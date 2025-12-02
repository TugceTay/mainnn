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

unit Lider.CG.GIS.FMX.GeoControlFieldFactor ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlFieldFactor"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Controls,
  FMX.Dialogs,
  FMX.Edit,
  FMX.StdCtrls,
  FMX.Forms,
  FMX.Types,
  UITypes,
  FMX.ListBox,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoTypes,

  Lider.CG.GIS.GeoCsBase,
  FMX.Controls.Presentation,
  Lider.CG.GIS.FMX.GeoModalForm ;

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
    cmbUnits: TComboBox;
    spnFactor: TEdit;
    procedure FormCreate(Sender: TObject);

  private

    procedure initForm     ; override;
    procedure initControls ; override;

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
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute     ( const _onhelp : TGIS_HelpEvent ;
                            const _proc   : TProc<TModalResult>
                          ) ; overload;

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute     ( const _proc   : TProc<TModalResult>
                          ) ; overload;
  end;

//##############################################################################
implementation

uses
  System.Math,
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.FMX.GeoControlHelper ;

//==============================================================================
// Form events
//==============================================================================

  procedure TGIS_ControlFieldFactor.FormCreate(
    Sender: TObject
  ) ;
  begin
    spnFactor.Text   := '1' ;

    lblFields.FixSize ;
    lblFactor.FixSize ;
    lblUnits.FixSize ;
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
    end
    else begin
      lblFactor.Visible := True ;
      spnFactor.Visible := True ;
      lblUnits.Visible  := True ;
      cmbUnits.Visible  := True ;
      if cmbUnits.Items.Count > 0 then
        cmbUnits.ItemIndex := 0 ;
    end;
  end;

  procedure TGIS_ControlFieldFactor.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_FIELDFACTOR ) ;
    Self.ClientHeight := 150 ;
    Self.ClientWidth := 280 ;
    Self.Name := 'TGIS_ControlFieldFactor' ;
    Self.OnCreate := FormCreate ;
  end ;

  procedure TGIS_ControlFieldFactor.initControls ;
  var
    t : Single ;
  begin
    t := 8 ;

    lblFields := TLabel.Create( oMainForm ) ;
    lblFields.Parent := oMainForm ;
    lblFields.Position.Y := t ;
    PlaceControl( BiDiMode, nil, lblFields, 8, -1 ) ;
    lblFields.Size.PlatformDefault := False ;
    lblFields.Text := _rsrcna( GIS_RS_FIELDFACTOR_FIELD ) ;
    lblFields.FixSize ;

    t := t + lblFields.Height ;

    cmbFields := TComboBox.Create( oMainForm ) ;
    cmbFields.Parent := oMainForm ;
    cmbFields.Position.Y := t ;
    PlaceControl( BiDiMode, nil, cmbFields, 8, -1 ) ;
    cmbFields.Size.PlatformDefault := False ;
    cmbFields.TabOrder := 0 ;

    t := t + cmbFields.Height + 7 ;

    lblFactor := TLabel.Create( oMainForm ) ;
    lblFactor.Parent := oMainForm ;
    lblFactor.Position.Y := t ;
    PlaceControl( BiDiMode, nil, lblFactor, 8, 115 ) ;
    lblFactor.Size.PlatformDefault := False ;
    lblFactor.Text := _rsrcna( GIS_RS_FIELDFACTOR_FACTOR ) ;
    lblFactor.FixSize ;

    t := t + lblFactor.Height ;

    spnFactor := TEdit.Create( oMainForm ) ;
    spnFactor.Parent := oMainForm ;
    spnFactor.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                            TInteractiveGesture.DoubleTap] ;
    spnFactor.TabOrder := 1 ;
    spnFactor.Position.Y := t ;
    PlaceControl( BiDiMode, nil, spnFactor, 8, 115 ) ;
    spnFactor.Size.PlatformDefault := False ;
    spnFactor.FixSize ;

    lblUnits := TLabel.Create( oMainForm ) ;
    lblUnits.Parent := oMainForm ;
    lblUnits.Position.Y := lblFactor.Position.Y ;
    PlaceControl( BiDiMode, lblFactor, lblUnits, 6, 144 ) ;
    lblUnits.Size.PlatformDefault := False ;
    lblUnits.Text := _rsrcna( GIS_RS_FIELDFACTOR_UNITS ) ;
    lblUnits.FixSize ;

    cmbUnits := TComboBox.Create( oMainForm ) ;
    cmbUnits.Parent := oMainForm ;
    cmbUnits.Position.Y := spnFactor.Position.Y ;
    PlaceControl( BiDiMode, spnFactor, cmbUnits, 6, 144 ) ;
    cmbUnits.Size.PlatformDefault := False ;
    cmbUnits.TabOrder := 2 ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Position.Y := ClientHeight - btnHelp.Size.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;
      btnHelp.TabOrder := 3 ;

      btnCancel.Position.Y := ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnCancel, -8, 80 ) ;
      btnCancel.TabOrder := 5 ;

      btnOK.Position.Y := ClientHeight - btnOK.Height - 8 ;
      PlaceControl( BiDiMode, btnCancel, btnOK, -8, 80 ) ;
      btnOK.TabOrder := 4 ;
    {$ENDIF}

    {$IFDEF GIS_MOBILE_DIALOGS}
      ClientWidth := Round( cmbFields.Position.X + cmbFields.Width + 32 ) ;
    {$ENDIF}

    PlaceControl( BiDiMode, nil, lblFields, 8, -1 ) ;
    PlaceControl( BiDiMode, nil, cmbFields, 8, -1 ) ;
    PlaceControl( BiDiMode, nil, lblFactor, 8, 115 ) ;
    PlaceControl( BiDiMode, nil, spnFactor, 8, 115 ) ;
    PlaceControl( BiDiMode, lblFactor, lblUnits, 6, 144 ) ;
    PlaceControl( BiDiMode, spnFactor, cmbUnits, 6, 144 ) ;
  end ;

  procedure TGIS_ControlFieldFactor.Execute(
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TProc<TModalResult>
  ) ;
  begin
    pOnHelp := _onhelp ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

    ShowModalEx( _proc ) ;
  end ;

  procedure TGIS_ControlFieldFactor.Execute(
    const _proc : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( hlp, _proc ) ;
  end;

//==================================== END =====================================
end.

