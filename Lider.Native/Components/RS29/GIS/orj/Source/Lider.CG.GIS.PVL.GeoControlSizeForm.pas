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
  Visual control for selecting fields.
}

{$IFDEF DCC}
  unit PVL.GisControlSizeForm ;
  {$HPPEMIT '#pragma link "PVL.GisControlSizeForm"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

{$INCLUDE GisInclude.inc}


interface

uses
  {$IFDEF CLR}
    TatukGIS.NDK,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    GisClasses,
    GisTypes,
    GisCSBase,
    PVL.GisPvl,
    PVL.GisValueValidatorHelper,
    PVL.GisPvlForms;
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
  ///   Visual form for managing custom size values.
  /// </summary>
  TGIS_ControlSizeForm = class( TGIS_PvlModalForm )
  public
    {#gendoc:hide}
    lblValue    : TGIS_PvlLabel       ;
    {#gendoc:hide}
    lblUnits    : TGIS_PvlLabel       ;
    {#gendoc:hide}
    spnFactor   : TGIS_PvlEdit        ;
    {#gendoc:hide}
    cmbUnits    : TGIS_PvlComboBox    ;
    {#gendoc:hide}
    isRotation  : Boolean             ;
    {#gendoc:hide}
    vvedtValue  : IGIS_ValueValidator ;

  private
    procedure fset_Precision( _value : Integer ) ;

  public
    /// <inheritdoc/>
    procedure DoInitForm     ; override;

    /// <inheritdoc/>
    procedure DoInitControls ; override;

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
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute   : TGIS_PvlModalResult; overload;

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute       ( const _proc   : TGIS_Proc
                           ) : TGIS_PvlModalResult ; overload;

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <param name="_onhelp">
    ///   help notification function; if assigned the help button will be
    ///   visible and help support will be enabled;
    /// </param>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function  Execute      ( const _onhelp : TGIS_HelpEvent ;
                             const _proc   : TGIS_Proc
                           ) : TGIS_PvlModalResult ; overload;
  public
    /// <summary>
    ///   Setting value creates a value validator
    ///   for Value edit box with given precision.
    /// </summary>
    property Precision : Integer write fset_Precision ;
  end;

//##############################################################################
implementation

{$IFDEF DCC}
uses
  GisRtl,
  GisParams,
  GisResource ;
{$ENDIF}

//==============================================================================
// Form events
//==============================================================================

  procedure TGIS_ControlSizeForm.DoInitForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_CUSTOMSIZE ) ;
    Self.ClientHeight := 104 ;
    Self.ClientWidth := 275 ;
    Self.Name := 'TGIS_ControlSizeForm' ;
  end ;

  procedure TGIS_ControlSizeForm.DoInitControls ;
  var
    ytop : Integer ;
  begin
    ytop := Context.VMargin ;

    lblValue  := TGIS_PvlLabel.Create( Context ) ;
    lblValue.Place ( 115, 0, nil, Context.HMargin, nil, ytop ) ;
    lblValue.Caption := _rsrc( GIS_RS_CUSTOMSIZE_VALUE ) ;

    lblUnits  := TGIS_PvlLabel.Create( Context ) ;
    lblUnits.Place ( -Context.VMargin, 0, lblValue, Context.HSpace, nil, ytop ) ;
    lblUnits.Caption := _rsrc( GIS_RS_CUSTOMSIZE_UNITS ) ;

    ytop := ytop + lblValue.Height + Context.LSpace;

    spnFactor := TGIS_PvlEdit.Create( Context ) ;
    spnFactor.Place( 115, 0, nil, Context.HMargin, nil, ytop ) ;
    spnFactor.TabOrder := 0 ;
    spnFactor.Text := '1' ;
    lblValue.FocusControl := spnFactor ;

    vvedtValue := spnFactor.CreateValueValidator ;
    vvedtValue.MinValue := -66535 ;
    vvedtValue.MaxValue := 65535 ;

    cmbUnits  := TGIS_PvlComboBox.Create( Context ) ;
    cmbUnits.Place ( -Context.HMargin, 0, spnFactor, Context.HSpace, nil, ytop ) ;
    cmbUnits.TabOrder := 1 ;

    BtnHelp.Visible := assigned( OnHelpEvent ) ;
    BtnHelp.TabOrder := 2 ;
    BtnCancel.TabOrder := 4 ;
    BtnOK.TabOrder := 3 ;

    Precision := 2 ;

    {$IFDEF GIS_MOBILE_DIALOGS}
      Self.ClientHeight := cmbUnits.Top + cmbUnits.Height
                           + Context.VMargin ;
    {$ELSE}
      Self.ClientHeight := cmbUnits.Top + cmbUnits.Height
                           + BtnOK.Height * 2 + Context.VMargin  ;
    {$ENDIF}

    {$IFNDEF GIS_MOBILE_DIALOGS}
      BtnHelp.Top   := Self.ClientHeight - BtnHelp.Height - Context.VMargin ;
      BtnCancel.Top := BtnHelp.Top ;
      BtnOK.Top     := BtnHelp.Top ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlSizeForm.fset_Precision(
    _value : Integer
  ) ;
  begin
    vvedtValue.Precision := _value ;
  end ;

  procedure TGIS_ControlSizeForm.FillUnits(
    const _rotation : Boolean
  ) ;
  begin
    isRotation := _rotation ;

    cmbUnits.BeginUpdate ;
    try
      cmbUnits.ItemsClear ;
      if _rotation then begin
        cmbUnits.ItemsAdd( GIS_PARAMTXT_ROTATION_DEG ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_ROTATION_RAD ) ;
        assert( cmbUnits.ItemsCount = GIS_PARAMTXT_ANGLE_CNT ) ;
      end
      else begin
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_PX    ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_DIP   ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_TWIPS ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_PT    ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_MM    ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_CM    ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_M     ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_KM    ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_IN    ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_FT    ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_YD    ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_MI    ) ;
        cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_NM    ) ;
        assert( cmbUnits.ItemsCount = GIS_PARAMTXT_SIZE_CNT ) ;
      end;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.EndUpdate ;
    end ;
  end ;

  procedure TGIS_ControlSizeForm.FillRealWorldUnits ;
  begin
    isRotation := False ;
    cmbUnits.BeginUpdate ;
    try
      cmbUnits.ItemsClear ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_IN ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_FT ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_YD ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_MI ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_MM ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_CM ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_M  ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_KM ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_NM ) ;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.EndUpdate ;
    end ;
  end ;

  procedure TGIS_ControlSizeForm.FillPrintWidthUnits ;
  begin
    isRotation := False ;

    cmbUnits.BeginUpdate ;
    try
      cmbUnits.ItemsClear ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_PX    ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_TWIPS ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_PT    ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_MM    ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_CM    ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_IN    ) ;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.EndUpdate ;
    end ;
  end ;

  procedure TGIS_ControlSizeForm.FillPrintFontSizeUnits ;
  begin
    isRotation := False ;

    cmbUnits.BeginUpdate ;
    try
      cmbUnits.ItemsClear ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_PT ) ;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.EndUpdate ;
    end ;

  end;

  procedure TGIS_ControlSizeForm.FillSnapUnits ;
  begin
    isRotation := False ;

    cmbUnits.BeginUpdate ;
    try
      cmbUnits.ItemsClear ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_CM    ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_MM    ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_PT    ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_IN    ) ;
      cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_PX    ) ;
      cmbUnits.ItemIndex := 0 ;
    finally
      cmbUnits.EndUpdate ;
    end ;
  end ;

  function TGIS_ControlSizeForm.Execute
   : TGIS_PvlModalResult;
  begin
    Result := Execute( nil ) ;
  end ;

  function TGIS_ControlSizeForm.Execute(
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( hlp, _proc ) ;
  end;

  function TGIS_ControlSizeForm.Execute(
    const _onhelp : TGIS_HelpEvent  ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  begin
    OnHelpEvent := _onhelp ;
    BtnHelp.Visible := assigned( OnHelpEvent ) ;

    Result := ShowModal( _proc, assigned( _proc ) ) ;
  end ;

//==================================== END =====================================
end.
