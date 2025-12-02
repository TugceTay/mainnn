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

{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoControlFieldFactor ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoControlFieldFactor"'}
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
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoParams,
    System.Generics.Collections,
    System.SysUtils,
    System.Classes,
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
    NoScale,

    /// <summary>
    ///   Color format.
    /// </summary>
    Color
  ) ;

  /// <summary>
  ///   Form used for defining visual parameters according to field values.
  /// </summary>
  TGIS_ControlFieldFactor = class( TGIS_PvlModalForm )
    cmbFields   : TGIS_PvlComboBox  ;
    lblFields   : TGIS_PvlLabel     ;
    lblFactor   : TGIS_PvlLabel     ;
    lblUnits    : TGIS_PvlLabel     ;
    spnFactor   : TGIS_PvlEdit      ;
    cmbUnits    : TGIS_PvlComboBox  ;
    lblFormat   : TGIS_PvlLabel     ;
    cmbFormat   : TGIS_PvlComboBox  ;
  public

    /// <inheritdoc/>
    procedure DoInitForm       ; override;

    /// <inheritdoc/>
    procedure DoInitControls   ; override;

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
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute    : TGIS_PvlModalResult; overload;

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute      ( const _proc   : TGIS_Proc
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
    function Execute      ( const _onhelp : TGIS_HelpEvent ;
                            const _proc   : TGIS_Proc
                          ) : TGIS_PvlModalResult ; overload;

  end;

//##############################################################################
implementation

//==============================================================================
// Form events
//==============================================================================

  procedure TGIS_ControlFieldFactor.DoInitForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_FIELDFACTOR ) ;
    Self.ClientHeight := 150 ;
    Self.ClientWidth := 280 ;
    Self.Name := 'TGIS_ControlFieldFactor' ;
  end ;

  procedure TGIS_ControlFieldFactor.DoInitControls ;
  var
    ytop : Integer ;
  begin
    ytop := Context.VMargin ;

    lblFields := TGIS_PvlLabel.Create( Context ) ;
    lblFields.Place( Context.ClientWidth - 2 * Context.HMargin, 0, nil, Context.HMargin, nil, ytop ) ;
    lblFields.Caption := _rsrc( GIS_RS_FIELDFACTOR_FIELD ) ;

    ytop := ytop + lblFields.Height + Context.LSpace;

    cmbFields := TGIS_PvlComboBox.Create( Context ) ;
    cmbFields.Place( Context.ClientWidth - Context.HMargin * 2 , 0, nil, Context.HMargin, nil, ytop ) ;
    cmbFields.TabOrder := 0 ;

    ytop := ytop + cmbFields.Height + 2 * Context.VSpace;

    lblFactor := TGIS_PvlLabel.Create( Context ) ;
    lblFactor.Place( RoundS( ( Context.ClientWidth - Context.HMargin * 2 ) / 2 ) - Context.HSpace, 0, nil, Context.HMargin, nil, ytop ) ;
    lblFactor.Caption := _rsrc( GIS_RS_FIELDFACTOR_FACTOR ) ;

    lblFormat := TGIS_PvlLabel.Create( Context ) ;
    lblFormat.Place( RoundS( ( Context.ClientWidth - Context.HMargin * 2 ) / 2 ) - Context.HSpace, 0, nil, Context.HMargin, nil, ytop ) ;
    lblFormat.Caption := _rsrc( GIS_RS_FIELDFACTOR_FORMAT ) ;

    lblUnits := TGIS_PvlLabel.Create( Context ) ;
    lblUnits.Place( -Context.VMargin, 0, lblFactor, Context.HSpace, nil, ytop ) ;
    lblUnits.Caption := _rsrc( GIS_RS_FIELDFACTOR_UNITS ) ;

    ytop := ytop + lblUnits.Height + Context.LSpace;

    spnFactor := TGIS_PvlEdit.Create( Context ) ;
    spnFactor.Place( RoundS( ( Context.ClientWidth - Context.HMargin * 2 ) / 2 ) - Context.HSpace, 0, nil, Context.HMargin, nil, ytop ) ;
    spnFactor.TabOrder := 1 ;
    spnFactor.Text := '1' ;

    cmbFormat := TGIS_PvlComboBox.Create( Context ) ;
    cmbFormat.Place( Context.ClientWidth - Context.HMargin * 2 , 0, nil, Context.HMargin, nil, ytop ) ;
    cmbFormat.TabOrder := 1 ;

    cmbUnits := TGIS_PvlComboBox.Create( Context ) ;
    cmbUnits.Place( -Context.HMargin, 0, spnFactor, Context.HSpace, nil, ytop ) ;
    cmbUnits.TabOrder := 2 ;

    lblUnits.FocusControl := cmbUnits ;
    lblFactor.FocusControl := spnFactor ;
    lblFields.FocusControl := cmbFields ;
    lblFormat.FocusControl := cmbFormat ;

    BtnHelp.Visible := assigned( OnHelpEvent ) ;
    BtnHelp.TabOrder := 3 ;
    BtnCancel.TabOrder := 5 ;
    BtnOK.TabOrder := 4 ;
  end ;

  procedure TGIS_ControlFieldFactor.FillFields(
    const _list : TStrings
  ) ;
  var
    i : Integer ;
  begin
    cmbFields.BeginUpdate ;
    try
      cmbFields.ItemsClear ;
      for i := 0 to _list.Count - 1 do
        cmbFields.ItemsAdd( _list[ i ] ) ;
    finally
      cmbFields.EndUpdate ;
    end;
    if cmbFields.ItemsCount > 0 then
      cmbFields.ItemIndex := 0 ;
  end;

  procedure TGIS_ControlFieldFactor.FillUnits(
    _unitsType : TGIS_FieldFactorUnitsType
  ) ;
  begin
    cmbUnits.BeginUpdate ;
    try
      cmbUnits.ItemsClear ;
      case _unitsType of
        TGIS_FieldFactorUnitsType.Size :
          begin
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
          end ;
        TGIS_FieldFactorUnitsType.Measure :
          begin
            cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_IN ) ;
            cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_FT ) ;
            cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_YD ) ;
            cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_MI ) ;
            cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_MM ) ;
            cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_CM ) ;
            cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_M  ) ;
            cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_KM ) ;
            cmbUnits.ItemsAdd( GIS_PARAMTXT_SIZE_NM ) ;
          end ;
        TGIS_FieldFactorUnitsType.Angular :
          begin
            cmbUnits.ItemsAdd( GIS_PARAMTXT_ROTATION_DEG ) ;
            cmbUnits.ItemsAdd( GIS_PARAMTXT_ROTATION_RAD ) ;
            assert( cmbUnits.ItemsCount = GIS_PARAMTXT_ANGLE_CNT ) ;
          end ;
      end;
    finally
      cmbUnits.EndUpdate ;
    end;

    if _unitsType = TGIS_FieldFactorUnitsType.NoScale then begin
      lblFactor.Visible := False ;
      spnFactor.Visible := False ;
      lblUnits.Visible  := False ;
      cmbUnits.Visible  := False ;
      lblFormat.Visible := False ;
      cmbFormat.Visible := False ;
      {$IFDEF GIS_MOBILE_DIALOGS}
        Self.ClientHeight := cmbFields.Top + cmbFields.Height
                             + Context.VMargin ;
      {$ELSE}
        Self.ClientHeight := cmbFields.Top + cmbFields.Height
                             + BtnOK.Height * 2 + Context.VMargin ;
      {$ENDIF}
    end
    else if _unitsType = TGIS_FieldFactorUnitsType.Color then begin
      cmbFormat.BeginUpdate ;
      try
        cmbFormat.ItemsClear ;
        cmbFormat.ItemsAdd( 'BGR' ) ;
        cmbFormat.ItemsAdd( 'ABGR' ) ;
        cmbFormat.ItemsAdd( 'RGB' ) ;
        cmbFormat.ItemsAdd( 'ARGB' ) ;
        cmbFormat.ItemsAdd( 'CSS' ) ;
      finally
        cmbFormat.EndUpdate ;
      end ;

      lblFactor.Visible := False ;
      spnFactor.Visible := False ;
      lblUnits.Visible  := False ;
      cmbUnits.Visible  := False ;
      lblFormat.Visible := True ;
      cmbFormat.Visible := True ;
      {$IFDEF GIS_MOBILE_DIALOGS}
        Self.ClientHeight := cmbFormat.Top + cmbFormat.Height
                             + Context.VMargin ;
      {$ELSE}
        Self.ClientHeight := cmbFormat.Top + cmbFormat.Height
                             + BtnOK.Height * 2 + Context.VMargin ;
      {$ENDIF}
      if cmbFormat.ItemsCount > 0 then
        cmbFormat.ItemIndex := 0 ;
    end
    else begin
      lblFactor.Visible := True ;
      spnFactor.Visible := True ;
      lblUnits.Visible  := True ;
      cmbUnits.Visible  := True ;
      lblFormat.Visible := False ;
      cmbFormat.Visible := False ;
      {$IFDEF GIS_MOBILE_DIALOGS}
        Self.ClientHeight := cmbUnits.Top + cmbUnits.Height
                             + Context.VMargin ;
      {$ELSE}
        Self.ClientHeight := cmbUnits.Top + cmbUnits.Height
                             + BtnOK.Height * 2 + Context.VMargin ;
      {$ENDIF}

      if cmbUnits.ItemsCount > 0 then
        cmbUnits.ItemIndex := 0 ;
    end;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      BtnHelp.Top   := Self.ClientHeight - BtnHelp.Height - Context.VMargin ;
      BtnCancel.Top := BtnHelp.Top ;
      BtnOK.Top     := BtnHelp.Top ;
    {$ENDIF}
  end;

  function TGIS_ControlFieldFactor.Execute
   : TGIS_PvlModalResult;
  begin
    Result := Execute( nil ) ;
  end ;

  function TGIS_ControlFieldFactor.Execute(
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( hlp, _proc ) ;
  end;

  function TGIS_ControlFieldFactor.Execute(
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  begin
    OnHelpEvent := _onhelp ;

    BtnHelp.Visible := assigned( OnHelpEvent ) ;

    Result := ShowModal( _proc, assigned( _proc ) ) ;
  end ;

//==================================== END =====================================
end.
