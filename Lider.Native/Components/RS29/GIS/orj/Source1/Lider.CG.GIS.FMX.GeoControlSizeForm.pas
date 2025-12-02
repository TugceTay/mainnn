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

unit Lider.CG.GIS.FMX.GeoControlSizeForm ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlSizeForm"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  FMX.Controls,
  FMX.Dialogs,
  FMX.Edit,
  FMX.StdCtrls,
  FMX.Forms,
  FMX.Types,
  FMX.ListBox,

  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoCsBase,
  FMX.Controls.Presentation,
  Lider.CG.GIS.FMX.GeoModalForm ;

type
  /// <summary>
  ///   Visual form for managing custom size values.
  /// </summary>
  TGIS_ControlSizeForm = class( TGIS_ModalForm )
    lblValue: TLabel;
    lblUnits: TLabel;
    spnFactor: TEdit;
    cmbUnits: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FSize      : String ;
    isRotation : Boolean ;
    pModalProc : TProc<TModalResult> ;

    procedure doModalResult ( _modal_result : TModalResult
                            ) ;

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
    ///   Execute dialog.
    /// </summary>
    /// <param name="_onhelp">
    ///   help notification function; if assigned the help button will be
    ///   visible and help support will be enabled;
    /// </param>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute      ( const _onhelp : TGIS_HelpEvent ;
                             const _proc   : TProc<TModalResult>
                           ) ; overload;

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute      ( const _proc   : TProc<TModalResult>
                           ) ; overload;
  public
    /// <summary>
    ///   Result string defining custom size.
    /// </summary>
    property CustomSize : String read FSize ;
  end ;

//##############################################################################
implementation

uses
  System.Math,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.FMX.GeoControlHelper ;

//==============================================================================
// Form events
//==============================================================================

  procedure TGIS_ControlSizeForm.FormCreate(
    Sender: TObject
  ) ;
  begin
    lblValue.FixSize ;
    lblUnits.FixSize ;
  end;

  procedure TGIS_ControlSizeForm.FormShow(
    Sender: TObject
  ) ;
  begin
  end;

  procedure TGIS_ControlSizeForm.FillUnits(
    const _rotation : Boolean
  ) ;
  var
    i   : Integer ;
    unt : TGIS_CSUnits ;
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
  var
    i   : Integer ;
    unt : TGIS_CSUnits ;
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

  procedure TGIS_ControlSizeForm.Execute(
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TProc<TModalResult>
  ) ;
  var
    res : Integer ;
  begin
    pModalProc := _proc ;

    pOnHelp := _onhelp ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

    ShowModalEx( doModalResult ) ;
  end ;

  procedure TGIS_ControlSizeForm.Execute(
    const _proc   : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( hlp, _proc ) ;
  end ;

  procedure TGIS_ControlSizeForm.doModalResult(
    _modal_result : TModalResult
  ) ;
  begin
    if _modal_result = mrOk then begin
      if isRotation then
        FSize := GIS_PARAMTXT_TYPE_ANGLE + ':' +
                  spnFactor.Text + ' ' + cmbUnits.Items[cmbUnits.ItemIndex]
      else
        FSize := GIS_PARAMTXT_TYPE_SIZE + ':' +
                  spnFactor.Text + ' ' + cmbUnits.Items[cmbUnits.ItemIndex]
    end ;

    if Assigned( pModalProc ) then
      pModalProc( _modal_result ) ;
  end ;

  procedure TGIS_ControlSizeForm.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_CUSTOMSIZE ) ;
    Self.ClientHeight := 104 ;
    Self.ClientWidth := 275 ;
    Self.Name := 'TGIS_ControlSizeForm' ;
    Self.OnCreate := FormCreate ;
    Self.OnShow := FormShow ;
  end ;

  procedure TGIS_ControlSizeForm.initControls ;
  var
    t : Single ;
  begin
    t := 8 ;

    lblValue := TLabel.Create( oMainForm ) ;
    lblValue.Parent := oMainForm ;
    lblValue.Position.Y := t ;
    PlaceControl( BiDiMode, nil, lblValue, 8, 115 ) ;
    lblValue.Size.PlatformDefault := False ;
    lblValue.Text := _rsrcna( GIS_RS_CUSTOMSIZE_VALUE ) ;
    lblValue.FixSize ;

    t := t + lblValue.Height ;

    spnFactor := TEdit.Create( oMainForm ) ;
    spnFactor.Parent := oMainForm ;
    spnFactor.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                            TInteractiveGesture.DoubleTap] ;
    spnFactor.TabOrder := 0 ;
    spnFactor.Text := '1,00' ;
    spnFactor.Position.Y := t ;
    PlaceControl( BiDiMode, nil, spnFactor, 8, 115 ) ;
    spnFactor.Size.PlatformDefault := False ;

    lblUnits := TLabel.Create( oMainForm ) ;
    lblUnits.Parent := oMainForm ;
    lblUnits.Position.Y := lblValue.Position.Y ;
    PlaceControl( BiDiMode, nil, lblUnits, 129, Self.ClientWidth - 129 - 8 ) ;
    lblUnits.Size.PlatformDefault := False ;
    lblUnits.Text := _rsrcna( GIS_RS_CUSTOMSIZE_UNITS ) ;
    lblUnits.FixSize ;

    cmbUnits := TComboBox.Create( oMainForm ) ;
    cmbUnits.Parent := oMainForm ;
    cmbUnits.Position.Y := spnFactor.Position.Y ;
    PlaceControl( BiDiMode, nil, cmbUnits, 129, Self.ClientWidth - 129 - 8 ) ;
    cmbUnits.Size.PlatformDefault := False ;
    cmbUnits.TabOrder := 1 ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 75 ) ;
      btnHelp.TabOrder := 2 ;

      btnCancel.Position.Y := ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnCancel, -8, 75 ) ;
      btnCancel.TabOrder := 4 ;

      btnOK.Position.Y := ClientHeight - btnOK.Height - 8 ;
      PlaceControl( BiDiMode, btnCancel, btnOK, -8, 75 ) ;
      btnOK.TabOrder := 3 ;
    {$ENDIF}

    PlaceControl( BiDiMode, nil, lblValue, 8, 115 ) ;
    PlaceControl( BiDiMode, nil, spnFactor, 8, 115 ) ;
    PlaceControl( BiDiMode, nil, lblUnits, 129, Self.ClientWidth - 129 - 8 ) ;
    PlaceControl( BiDiMode, nil, cmbUnits, 129, Self.ClientWidth - 129 - 8 ) ;
  end ;

//==================================== END =====================================
end.

