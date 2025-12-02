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
  Modal form helper.
}

unit VCL.GisModalForm ;
{$HPPEMIT '#pragma link "VCL.GisModalForm"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  Winapi.Windows,
  Winapi.Messages,

  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.StdCtrls,

  GisTypes,
  GisTypesUI;

type
  /// <summary>
  ///   Utility class to simplify creation of DFM-free dialog boxes.
  /// </summary>
  TGIS_ModalForm = class( TCustomForm )
    private
      iLockLevel : Integer ;
      iPPI       : Integer ;
      bFirstShow : Boolean ;
    private
      {$IFDEF LEVEL_RX101_VCL}
        procedure WMDpiChanged           ( var _message: TWMDpi
                                         ); message WM_DPICHANGED;
      {$ENDIF}
      procedure doBeforeMonitorDpiChanged( _sender : TObject ;
                                           _oldDPI : Integer ;
                                           _newDPI : Integer
                                         );
      procedure doAfterMonitorDpiChanged ( _sender : TObject ;
                                           _oldDPI : Integer ;
                                           _newDPI : Integer
                                         );

    protected

      /// <summary>
      ///   Standard OK button.
      /// </summary>
      btnOK: TButton;

      /// <summary>
      ///   Standard Cancel button.
      /// </summary>
      btnCancel: TButton;

      /// <summary>
      ///   Standard Help button.
      /// </summary>
      btnHelp: TButton;

      /// <summary>
      ///   Handler for help service.
      /// </summary>
      pOnHelp : TGIS_HelpEvent ;

    protected

      /// <summary>
      ///   Block windows updates. Must be paired with unlockWindow.
      /// </summary>
      procedure lockWindow         ;

      /// <summary>
      ///   Release windows updates. Must be paired with lockWindow.
      /// </summary>
      procedure unlockWindow       ;

      /// <summary>
      ///   Convert value express in 96-ppi to active PPI.
      /// </summary>
      /// <param name="_value">
      ///   value in 96-ppi
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      function  ppiFix           ( const _value : Integer
                                 ) : Integer ;

      /// <summary>
      ///   Override this method upon to provide custom caption ad sizes.
      /// </summary>
      procedure initForm         ; virtual;

      /// <summary>
      ///   Override this method to add controls to this form.
      /// </summary>
      procedure initControls     ; virtual;

      /// <summary>
      ///   Override this method to provide any special functionality upon
      ///   form show.
      /// </summary>
      procedure showForm         ; virtual;

      /// <summary>
      ///   Create standard form buttons.
      /// </summary>
      procedure initButtons ;

      /// <summary>
      ///   Override this provide any special functionality before monitor
      ///   resolution change (including moving form on multi-monitor different
      ///   resolution scenario).
      /// </summary>
      /// <remarks>
      ///   Supported on RAD Studio 10.1 Berlin and up.
      /// <remarks>
      procedure beforePPIChanged ; virtual;

      /// <summary>
      ///   Override this provide any special functionality after monitor
      ///   resolution change (including moving form on multi-monitor different
      ///   resolution scenario).
      /// </summary>
      /// <remarks>
      ///   Supported on RAD Studio 10.1 Berlin and up.
      /// <remarks>
      procedure afterPPIChanged  ; virtual;

      /// <summary>
      ///   Standard action on OK button. ModalResult set to mrOK.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure btnOKClick       ( _sender : TObject
                                 ) ; virtual;

      /// <summary>
      ///   Standard action on Cancel button. ModalResult set to mrCancel.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure btnCancelClick   ( _sender : TObject
                                 ) ; virtual;

      /// <summary>
      ///   Standard action on Help button. Help event is raised.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure btnHelpClick     ( _sender : TObject
                                 ) ; virtual;

      /// <summary>
      ///   See documentation for TCustomForm in Delphi help.
      /// </summary>
      procedure DoShow           ; override;

      /// <summary>
      ///   See documentation for TCustomForm in Delphi help.
      /// </summary>
      /// <param name="_key">
      ///   See documentation for TCustomForm in Delphi help.
      /// </param>
      /// <param name="_shift">
      ///   See documentation for TCustomForm in Delphi help.
      /// </param>
      procedure KeyDown          ( var _key   : Word ;
                                       _shift : TShiftState
                                 ) ; override;

    public
      /// <summary>
      ///   Create a standard dialog box with fixed frames.
      /// </summary>
      /// <param name="_owner">
      ///   Form owner.
      /// </param>
      constructor Create    ( _owner    : TComponent
                            ) ; overload; override;

      /// <summary>
      ///   Create a standard dialog box.
      /// </summary>
      /// <param name="_owner">
      ///   Form owner.
      /// </param>
      /// <param name="_sizeable">
      ///   If true form is create as sizeable; otherwise is created with
      ///   fixed frame, dialog box style.
      /// </param>
      constructor Create    ( _owner    : TComponent ;
                              _sizeable : Boolean
                            ) ; overload; virtual;

    property
      /// <summary>
      ///   Provide a current PPI.
      /// </summary>
      CurrentPPI : Integer read iPPI ;

  end;


//##############################################################################
implementation

uses
  VCL.GisControlHelper,
  GisRTL,
  GisResource ;

constructor TGIS_ModalForm.Create(
  _owner    : TComponent
);
begin
  Create( _owner, false );
  PopupMode := pmAuto ;
end;

constructor TGIS_ModalForm.Create(
  _owner    : TComponent ;
  _sizeable : Boolean
);
var
  otmp : TComponent ;
begin
  // find owner form
  otmp := _owner ;

  while Assigned( otmp ) do begin
    if otmp is TCustomForm then
      break ;
    otmp := otmp.Owner ;
  end;

  if not Assigned( otmp ) then
    otmp := _owner ;

  inherited CreateNew( otmp ) ;

  iLockLevel := 0 ;

  {$IFDEF LEVEL_RX101_VCL}
    OnAfterMonitorDpiChanged := doAfterMonitorDpiChanged ;
    OnBeforeMonitorDpiChanged := doBeforeMonitorDpiChanged ;
  {$ENDIF}

  {$IFDEF LEVEL_RX101_VCL}
    Self.ControlState := [csReadingState] ;
  {$ENDIF}
  if _rsbidi then
    Self.BiDiMode := TBiDiMode.bdRightToLeft
  else
    Self.BiDiMode := TBiDiMode.bdLeftToRight ;

  Self.ClientWidth  := 320 ;
  Self.ClientHeight := 240 ;
  Self.Color := clBtnFace ;
  Self.DoubleBuffered := not IsWin11 ;
  Self.Font.Charset := DEFAULT_CHARSET ;
  Self.Font.Color := clWindowText ;
  Self.Font.Height := -11 ;
  Self.Font.Name := 'Tahoma' ;
  Self.Font.Style := [] ;
  Self.KeyPreview := True ;
  Self.Position := poOwnerFormCenter ;
  Self.PixelsPerInch := 96 ;
  if _sizeable then begin
    Self.BorderStyle := TFormBorderStyle.bsSizeable ;
    Self.BorderIcons := [] ;
  end
  else begin
    Self.BorderStyle := TFormBorderStyle.bsDialog ;
  end;

  initForm ;

  Self.ControlState := [];

  iPPI := 96 ;

  initButtons ;
  initControls ;

  {$IFDEF LEVEL_RX101_VCL}
    ScaleForCurrentDpi ;
    iPPI := Monitor.PixelsPerInch ;
  {$ELSE}
    Self.ScaleBy( Screen.PixelsPerInch, PixelsPerInch);
    iPPI := Screen.PixelsPerInch ;
  {$ENDIF}

  bFirstShow := True ;
end;

procedure TGIS_ModalForm.lockWindow ;
begin
  if not Visible then
    exit ;

  if iLockLevel = 0 then begin
    SendMessage(Handle, WM_SETREDRAW, Ord(False), 0);
  end;

  Inc( iLockLevel ) ;
end;

procedure TGIS_ModalForm.unlockWindow ;
begin
  if not Visible then
    exit ;

  Dec( iLockLevel ) ;

  if iLockLevel > 0 then
    exit ;

  Assert( iLockLevel >= 0 ) ;

  SendMessage(Handle, WM_SETREDRAW, Ord(True), 0);
  RedrawWindow( Handle, nil, 0,
                RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN
              );

  iLockLevel := 0 ;
end;

function TGIS_ModalForm.ppiFix(
  const _value : Integer
) : Integer ;
begin
  Result := MulDiv( _value, iPPI, 96 ) ;
end ;

procedure TGIS_ModalForm.initForm;
begin
  // for safe inheritane
end;

procedure TGIS_ModalForm.initButtons ;
var
  anchorsB  : TAnchors ;
  anchorsRB : TAnchors ;
begin

  if BiDiMode = bdRightToLeft then begin
    anchorsB  := [akRight, akBottom] ;
    anchorsRB := [akLeft, akBottom] ;
  end else begin
    anchorsB  := [akLeft, akBottom] ;
    anchorsRB := [akRight, akBottom]
  end ;

  btnHelp := TButton.Create( Self ) ;
  btnHelp.Parent := Self ;
  btnHelp.Anchors := anchorsB ;
  btnHelp.Height := 25 ;
  btnHelp.Top := Self.ClientHeight - btnHelp.Height - 8 ;
  PlaceControl( BiDiMode, nil, btnHelp, 8, 75 ) ;
  btnHelp.Caption := _rsrc( GIS_RS_BTN_HELP ) ;
  btnHelp.OnClick := btnHelpClick ;

  pOnHelp := nil ;

  btnCancel := TButton.Create( Self ) ;
  btnCancel.Parent := Self ;
  btnCancel.Anchors := anchorsRB ;
  btnCancel.Height := 25 ;
  btnCancel.Top := Self.ClientHeight - btnCancel.Height - 8 ;
  PlaceControl( BiDiMode, nil, btnCancel, -8, 75 ) ;
  btnCancel.Caption := _rsrc( GIS_RS_BTN_CANCEL ) ;
  btnCancel.OnClick := btnCancelClick ;

  btnOK := TButton.Create( Self ) ;
  btnOK.Parent := Self ;
  btnOK.Anchors := anchorsRB ;
  btnOK.Height := 25 ;
  btnOK.Top := Self.ClientHeight - btnOK.Height - 8 ;
  PlaceControl( BiDiMode, btnCancel, btnOK, -8, 75 ) ;
  btnOK.Caption := _rsrc( GIS_RS_BTN_OK ) ;
  btnOK.Default := True ;
  btnOK.OnClick := btnOKClick ;
end ;

procedure TGIS_ModalForm.initControls;
begin
  // for safe inheritane
end;

procedure TGIS_ModalForm.showForm;
begin
  if btnOK.Visible and btnOK.Enabled then
    btnOk.SetFocus
  else
  if btnCancel.Visible and btnCancel.Enabled then
    btnCancel.SetFocus ;
end;

procedure TGIS_ModalForm.afterPPIChanged;
begin
  // for safe inheritane
end;

procedure TGIS_ModalForm.beforePPIChanged;
begin
  // for safe inheritane
end;

{$IFDEF LEVEL_RX101_VCL}
  procedure TGIS_ModalForm.WMDpiChanged(
    var _message : TWMDpi
  );
  begin
    // make a fake PPI on monitor change
    if not bFirstShow then
      _message.YDpi := RoundS( _message.YDpi * GUIScale );
    inherited ;
  end;
{$ENDIF}

procedure TGIS_ModalForm.doBeforeMonitorDpiChanged(
  _sender : TObject ;
  _oldDPI : Integer ;
  _newDPI: Integer
);
begin
  iPPI := _newDPI ;
  beforePPIChanged ;
  lockWindow ;
end;

procedure TGIS_ModalForm.doAfterMonitorDpiChanged(
  _sender : TObject ;
  _oldDPI : Integer ;
  _newDPI: Integer
);
begin
  unlockWindow ;
  afterPPIChanged ;
end;


procedure TGIS_ModalForm.DoShow ;
begin
  inherited ;

  if bFirstShow then begin
    if GUIScale <> 1 then begin
      {$IFDEF LEVEL_RX101_VCL}
        iPPI := RoundS( IPPI * GUIScale )  ;
        ScaleForPPI( iPPI ) ;
      {$ENDIF}
    end ;
  end;

  showForm ;

  bFirstShow := False ;
end;

procedure TGIS_ModalForm.KeyDown(
  var _key   : Word ;
      _shift : TShiftState
) ;
begin
  inherited ;
  if      _key = VK_ESCAPE then begin
                                  if ( ActiveControl is TComboBox ) and
                                     TComboBox(ActiveControl).DroppedDown then exit ;
                                  btnCancelClick( Self )
                                end
  else if _key = VK_RETURN then begin
                                  if ActiveControl is TMemo then exit ;
                                  btnOKClick( Self ) ;
                                end
  else if _key = VK_F1     then btnHelpClick( Self ) ;
end;

procedure TGIS_ModalForm.btnOKClick ;
begin
  if btnOK.Visible then
    ModalResult := mrOK ;
end;

procedure TGIS_ModalForm.btnCancelClick ;
begin
  if btnCancel.Visible then
    ModalResult := mrCancel ;
end;

procedure TGIS_ModalForm.btnHelpClick ;
begin
  if Assigned( pOnHelp ) then pOnHelp( Self, Name ) ;
end;

{==================================== END =====================================}
end.




