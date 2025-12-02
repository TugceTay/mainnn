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
  Implementation of PVL forms for VCL
}

unit VCL.GisPvlForms;

interface

{$INCLUDE GisInclude.inc}


uses
  GisInterfaces,
  VCL.GisPvl,
  PVL.GisPvl,
  PVL.GisPvlForms;

//##############################################################################
implementation

{$IFDEF GIS_EDITOR}
  {$IFDEF GIS_PDK}
    {$DEFINE GIS_EDITOR_PDK_FIX}  // fix for for release form Python's thread
  {$ENDIF}
{$ENDIF}



uses
  Winapi.Messages,
  Winapi.Windows,
  System.Classes,
  System.Types,
  System.Math,
  System.UITypes,
  {$IFDEF GIS_EDITOR_PDK_FIX}
    System.SyncObjs,
  {$ENDIF}
  VCL.Controls,
  VCL.StdCtrls,
  VCL.Forms,
  VCL.Graphics,


  GisTypes,
  GisRtl,
  GisResource ;


const
  FORM_MARGIN = 5 ;

type
  T_PvlForm = class;
  T_Form = class( TCustomForm )
    private
      oMaster : TGIS_PvlBaseForm ;
      oParent : T_PvlForm ;
      bFreeOnClose : Boolean ;
    public
      {$IFDEF GIS_EDITOR_PDK_FIX}
        destructor Destroy; override;
      {$ENDIF}

      {$IFNDEF LEVEL_RX11_VCL}
        {$IFDEF LEVEL_RX101_VCL}
          // make it public!
          procedure ScaleForCurrentDpi; override ;
        {$ENDIF}
      {$ENDIF}
    private
      procedure doFormClose   (       _sender : TObject ;
                                var   _action : TCloseAction
                              );
      procedure doFormDestroy (       _sender : TObject
                              );
  end ;

  T_PvlForm = class( TGIS_PvlBaseVCL, IGIS_PvlForm )
    private
      iPPI        : Integer;
      dScale      : Single;
      oForm       : T_Form ;
      iLocklevel  : Integer ;
    public
      constructor Create          ( const _context    : TGIS_PvlContext;
                                    const _parent     : IGIS_PvlBase
                                  );
                                  override;
      destructor  Destroy;        override;
    public

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Name         : String;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_Name         ( const _value    : String
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_Caption      ( const _value    : String
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_StayOnTop    : Boolean;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_StayOnTop    ( const _value    : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Left         : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_Left         ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Top          : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_Top          ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Height       : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_Height       ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Width        : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_Width        ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_ClientHeight : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_ClientHeight ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_ClientWidth  : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_ClientWidth  ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_MinHeight    : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_MinHeight    ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_MinWidth     : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_MinWidth     ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_RightToLeft  : Boolean;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Visible      : Boolean;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_PPI          : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_PPIFix       : Single;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_CanvasScale  : Single;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Context      : TGIS_PvlContext;

       /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BorderStyle  : TGIS_PvlBorderStyle;

       /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BorderIcons  : TGIS_PvlBorderIcons;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_ModalResult  : TGIS_PvlModalResult ; //?

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_ModalResult  ( const _value    : TGIS_PvlModalResult
                                  ) ;

    protected
      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doUpdateGUI       ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doInitButtons     ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doPlaceButtons    ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doAfterPPIChanged ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doShowForm        ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure LockWindow        ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  IsLocked          : Boolean ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure UnlockWindow      ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure DoRedraw          ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure DoResize          ;
                                  virtual;
    private
      procedure doBeforeMonitorDpiChanged(       _sender : TObject ;
                                                 _oldDPI : Integer ;
                                                 _newDPI : Integer
                                         ); reintroduce;
      procedure doAfterMonitorDpiChanged (       _sender : TObject ;
                                                 _oldDPI : Integer ;
                                                _newDPI : Integer
                                         ); reintroduce;
      procedure doOnResize               (      _sender : TObject
                                         );
      procedure handleOnShow             (      _sender : TObject
                                         );
      function  doShowModal              : TGIS_PvlModalResult ;
      {$IFDEF GIS_EDITOR_PDK_FIX}
        procedure doIdle                 (      _sender : TObject ;
                                           var  _done   : Boolean
                                         ) ;
      {$ENDIF}

    public
      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure Show              ; overload;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure Show              ( const _free : Boolean
                                  ) ;
                                  overload;


      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure Hide              ;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure FreeForm          ;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  ShowModal         ( const  _proc    : TGIS_Proc ;
                                    const _free     : Boolean
                                  ) : TGIS_PvlModalResult ; overload;
      /// <inheritdoc from="IGIS_PvlForm"/>
      function  ShowModal         : TGIS_PvlModalResult ; overload;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure Close             ;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure ProcessMessages   ;
  end;

  T_PvlModalFormDesktop = class( T_PvlForm, IGIS_PvlModalForm )
    public
      constructor Create          ( const _context    : TGIS_PvlContext;
                                    const _parent     : IGIS_PvlBase
                                  );
                                  override;
    protected
      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnOK        : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnCancel    : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnHelp      : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_OnHelp       : TGIS_HelpEvent;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_OnHelp       ( const _value    : TGIS_HelpEvent
                                  );
    private
      procedure doKeyDown         (      _sender    : TObject ;
                                    var  _key       : Word ;
                                         _shift     : TShiftState
                                  ) ;
  end;

  T_PvlModalWizardDesktop = class( T_PvlForm, IGIS_PvlModalWizard )
    public
      constructor Create          ( const _context    : TGIS_PvlContext;
                                    const _parent     : IGIS_PvlBase
                                  );
                                  override;
    protected
      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnOK        : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnCancel    : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnHelp      : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlModalWizard"/>
      function  fget_BtnNext      : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlModalWizard"/>
      function  fget_BtnPrevious  : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlModalWizard"/>
      function  fget_Pages        : TGIS_PvlPages;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_OnHelp       : TGIS_HelpEvent;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_OnHelp       ( const _value    : TGIS_HelpEvent
                                  );
    private
      procedure doKeyDown         (      _sender    : TObject ;
                                    var  _key       : Word ;
                                         _shift     : TShiftState
                                  ) ;
  end;

{$REGION 'T_Form'}

{$IFDEF GIS_EDITOR_PDK_FIX}
  var
    editorFixCriticalSection : TCriticalSection ;
    editorFixCount           : Integer = 0 ;
    editorFixPrevious        : TIdleEvent ;

{$ENDIF}


{$IFDEF GIS_EDITOR_PDK_FIX}
  destructor T_Form.Destroy;
  begin
    inherited;

    editorFixCriticalSection.Enter ;
    try
      if editorFixCount > 0  then begin
        Dec( editorFixCount ) ;

        if editorFixCount = 0 then
          Application.OnIdle := editorFixPrevious ;
      end;
    finally
      editorFixCriticalSection.Leave ;
    end;
  end;
{$ENDIF}

{$IFNDEF LEVEL_RX11_VCL}
  {$IFDEF LEVEL_RX101_VCL}
    procedure T_Form.ScaleForCurrentDpi;
    begin
      inherited ScaleForCurrentDpi;
    end ;
  {$ENDIF}
{$ENDIF}

procedure T_Form.doFormClose(
      _sender : TObject ;
  var _action : TCloseAction
) ;
begin
  {$IFDEF GIS_EDITOR_PDK_FIX}
    editorFixCriticalSection.Enter ;
    try
      Inc( editorFixCount ) ;

      if editorFixCount = 1  then begin
        editorFixPrevious := Application.OnIdle ;
        Application.OnIdle := oParent.doIdle;
      end;
    finally
      editorFixCriticalSection.Leave ;
    end;
  {$ENDIF}
  if Assigned( oMaster.OnClose ) then
    oMaster.OnClose( _sender ) ;
   if bFreeOnClose then
    _action := TCloseAction.caFree;
end;

procedure T_Form.doFormDestroy(
  _sender: TObject
);
begin
  if bFreeOnClose then begin
    oParent.oForm := nil ;
    oMaster.Free  ;
  end;
end;

{$ENDREGION 'T_Form'}

{$REGION 'T_PvlForm'}

constructor T_PvlForm.Create(
  const _context : TGIS_PvlContext;
  const _parent  : IGIS_PvlBase
);
var
  border_style : TGIS_PvlBorderStyle ;
  border_icons : TGIS_PvlBorderIcons ;
begin
  inherited Create( _context, _parent ) ;

  iLocklevel := 0 ;

  border_style := fget_BorderStyle ;
  border_icons := fget_BorderIcons ;

  oForm := T_Form.CreateNew( TComponent( _context.NativeParent ), 0 ) ;
  oForm.oParent := self;
  oForm.oMaster := _parent as TGIS_PvlBaseForm;
  oForm.OnClose := oForm.doFormClose;
  oForm.OnDestroy := oForm.doFormDestroy;
  oForm.OnShow  := handleOnShow ;
  oForm.PopupMode := pmAuto ;


  {$IFDEF LEVEL_RX101_VCL}
    oForm.OnAfterMonitorDpiChanged := doAfterMonitorDpiChanged ;
    oForm.OnBeforeMonitorDpiChanged := doBeforeMonitorDpiChanged ;
  {$ENDIF}

  {$IFDEF LEVEL_RX101_VCL}
    oForm.ControlState := [csReadingState] ;
  {$ENDIF}
  if _rsbidi() then
    oForm.BiDiMode := TBiDiMode.bdRightToLeft
  else
    oForm.BiDiMode := TBiDiMode.bdLeftToRight ;

  if oForm.ClientWidth < 320  then
    oForm.ClientWidth  := 320 ;

  if oForm.ClientHeight < 240  then
    oForm.ClientHeight := 240 ;

  oForm.Color := clBtnFace ;
  oForm.Font.Charset := DEFAULT_CHARSET ;
  oForm.Font.Color := clWindowText ;
  oForm.Font.Height := -11 ;
  oForm.Font.Name := 'Tahoma' ;
  oForm.Font.Style := [] ;
  oForm.KeyPreview := True ;
  oForm.Position := poOwnerFormCenter ;
  oForm.PixelsPerInch := 96 ;

  case border_style of
    TGIS_PvlBorderStyle.None               : oForm.BorderStyle := TFormBorderStyle.bsNone ;
    TGIS_PvlBorderStyle.Fixed              : oForm.BorderStyle := TFormBorderStyle.bsDialog ;
    TGIS_PvlBorderStyle.Sizeable           : oForm.BorderStyle := TFormBorderStyle.bsSizeable ;
    TGIS_PvlBorderStyle.ToolWindow         : oForm.BorderStyle := TFormBorderStyle.bsToolWindow ;
    TGIS_PvlBorderStyle.ToolWindowSizeable : oForm.BorderStyle := TFormBorderStyle.bsSizeToolWin ;
  end;

  oForm.BorderIcons := [] ;
  if TGIS_PvlBorderIcon.SystemMenu in border_icons then begin
    oForm.BorderIcons := oForm.BorderIcons + [TBorderIcon.biSystemMenu] ;
    oForm.BorderIcons := oForm.BorderIcons + [TBorderIcon.biSystemMenu] ;
  end;
  if TGIS_PvlBorderIcon.Minimize in border_icons then begin
    oForm.BorderIcons := oForm.BorderIcons + [TBorderIcon.biSystemMenu] ;
    oForm.BorderIcons := oForm.BorderIcons + [TBorderIcon.biMinimize] ;
  end;
  if TGIS_PvlBorderIcon.Maximize in border_icons then begin
    oForm.BorderIcons := oForm.BorderIcons + [TBorderIcon.biSystemMenu] ;
    oForm.BorderIcons := oForm.BorderIcons + [TBorderIcon.biMaximize] ;
  end;

  {$IFDEF LEVEL_RX101_VCL}
    oForm.ScaleForPPI( 96 ) ;
  {$ENDIF}

  iPPI := 96 ;

  oForm.ControlState := [];

  {$IFDEF LEVEL_RX101_VCL}
    oForm.ScaleForCurrentDpi ;
    iPPI := oForm.Monitor.PixelsPerInch ;
  {$ELSE}
    oForm.ScaleBy( Screen.PixelsPerInch, oForm.PixelsPerInch);
    iPPI := Screen.PixelsPerInch ;
  {$ENDIF}

  oControl := oForm ;

  oForm.OnResize := doOnResize ;
end;

destructor T_PvlForm.Destroy;
begin
  if Assigned( oForm ) then begin
  {$IFDEF GIS_EDITOR_PDK_FIX}
    editorFixCriticalSection.Enter ;
    try
      Inc( editorFixCount ) ;

      if editorFixCount = 1  then begin
        editorFixPrevious := Application.OnIdle ;
        Application.OnIdle := doIdle;
      end;
    finally
      editorFixCriticalSection.Leave ;
    end;
  {$ENDIF}

    oForm.Release ;
  end;
  inherited ;
end;

function T_PvlForm.fget_Name
  : String;
begin
  Result := oForm.Name ;
end;

procedure T_PvlForm.fset_Name(
  const _value : String
);
begin
  oForm.Name := _value ;
end;

function T_PvlForm.fget_Caption
  : String;
begin
  Result := oForm.Caption ;
end;

procedure T_PvlForm.fset_Caption(
  const _value : String
);
begin
  oForm.Caption := _value ;
end;

function T_PvlForm.fget_StayOnTop
  : Boolean;
begin
  Result := fsStayOnTop = oForm.FormStyle ;
end;

procedure T_PvlForm.fset_StayOnTop(
  const _value : Boolean
);
begin
  if _value then
    oForm.FormStyle := fsStayOnTop
  else
    oForm.FormStyle := fsNormal ;
end;

function T_PvlForm.fget_Left
  : Integer ;
begin
  Result := oForm.Left ;
end;

procedure T_PvlForm.fset_Left(
  const _value: Integer
) ;
begin
  oForm.Position := poDesigned ;
  oForm.Left := _value ;
end;

function T_PvlForm.fget_Top
  : Integer ;
begin
  Result := oForm.Top ;
end;

procedure T_PvlForm.fset_Top(
  const _value: Integer
) ;
begin
  oForm.Position := poDesigned ;
  oForm.Top := _value ;
end;

function T_PvlForm.fget_Height
  : Integer;
begin
  Result := RoundS( oForm.Height / fget_PPIFix ) ;
end;

procedure T_PvlForm.fset_Height(
  const _value: Integer
);
begin
  oForm.Height := RoundS( _value * fget_PPIFix ) ;
end;

function T_PvlForm.fget_Width
  : Integer;
begin
  Result := RoundS( oForm.Width / fget_PPIFix ) ;
end;

procedure T_PvlForm.fset_Width(
  const _value : Integer
);
begin
  oForm.Width := RoundS( _value * fget_PPIFix ) ;
end;

function T_PvlForm.fget_ClientHeight
  : Integer;
begin
  Result := RoundS( oForm.ClientHeight / fget_PPIFix ) ;
end;

procedure T_PvlForm.fset_ClientHeight(
  const _value : Integer
);
begin
  oForm.Clientheight := RoundS( _value * fget_PPIFix ) ;
end;

function T_PvlForm.fget_ClientWidth
  : Integer;
begin
  Result := RoundS( oForm.ClientWidth / fget_PPIFix ) ;
end;

procedure T_PvlForm.fset_ClientWidth(
  const _value : Integer
);
begin
  oForm.ClientWidth := RoundS( _value * fget_PPIFix ) ;
end;

function T_PvlForm.fget_MinHeight
  : Integer;
begin
  Result := oForm.Constraints.MinHeight - oForm.Height + oForm.ClientHeight ;
end;

procedure T_PvlForm.fset_MinHeight(
  const _value : Integer
);
begin
  oForm.Constraints.MinHeight := _value + oForm.Height - oForm.ClientHeight ;
end;

function T_PvlForm.fget_MinWidth
  : Integer;
begin
  Result := oForm.Constraints.MinWidth - oForm.Width + oForm.ClientWidth ;
end;

procedure T_PvlForm.fset_MinWidth(
  const _value : Integer
);
begin
  oForm.Constraints.MinWidth := _value + oForm.Width - oForm.ClientWidth ;
end;

function T_PvlForm.fget_RightToLeft
  : Boolean;
begin
  Result := oForm.BiDiMode = TBiDiMode.bdRightToLeft ;
end;

function T_PvlForm.fget_Visible
  : Boolean ;
begin
  Result := oForm.Visible ;
end;

function T_PvlForm.fget_PPI
  : Integer;
begin
  Result := iPPI ;
end;

function T_PvlForm.fget_PPIFix
  : Single;
begin
  Result := iPPI / 96 ;
end;

function T_PvlForm.fget_CanvasScale
  : Single;
begin
  Result := 1 ;
end;

function T_PvlForm.fget_Context
 : TGIS_PvlContext;
begin
  // do nothing
  Result := nil ;
end;

function T_PvlForm.fget_BorderStyle
  : TGIS_PvlBorderStyle;
begin
  Result := ( oParent as IGIS_PvlForm ).fget_BorderStyle ;
end;

function T_PvlForm.fget_BorderIcons
  : TGIS_PvlBorderIcons;
begin
  Result := ( oParent as IGIS_PvlForm ).fget_BorderIcons ;
end;

function T_PvlForm.fget_ModalResult
  : TGIS_PvlModalResult;
begin
  case oForm.ModalResult of
    mrOk     : Result := TGIS_PvlModalResult.OK     ;
    mrCancel : Result := TGIS_PvlModalResult.Cancel ;
    mrAbort  : Result := TGIS_PvlModalResult.Abort  ;
    else       Result := TGIS_PvlModalResult.None   ;
  end;
end;

procedure T_PvlForm.fset_ModalResult(
  const _value: TGIS_PvlModalResult
);
begin
  case _value of
    TGIS_PvlModalResult.OK     : oForm.ModalResult := mrOk     ;
    TGIS_PvlModalResult.Cancel : oForm.ModalResult := mrCancel ;
    TGIS_PvlModalResult.Abort  : oForm.ModalResult := mrAbort  ;
    else                         oForm.ModalResult := mrNone   ;
  end;
end;

procedure T_PvlForm.doUpdateGUI ;
begin
  // do nothing
end;

procedure T_PvlForm.doInitButtons ;
begin
  // do nothing
end;

procedure T_PvlForm.doPlaceButtons ;
begin
  // do nothing
end;

procedure T_PvlForm.doAfterPPIChanged ;
begin
  // do nothing
end;

procedure T_PvlForm.doShowForm ;
begin
  // do nothing
end;

{$IFDEF GIS_EDITOR_PDK_FIX}
  procedure T_PvlForm.doIdle(
        _sender : TObject ;
    var _done   : Boolean
  );
  begin
    _done := False;
  end;
{$ENDIF}

procedure T_PvlForm.LockWindow ;
begin
  if not oForm.Visible then
    exit ;

  if iLockLevel = 0 then begin
    SendMessage(oForm.Handle, WM_SETREDRAW, Ord(False), 0);
  end;

  Inc( iLockLevel ) ;
end;

function T_PvlForm.IsLocked
  : Boolean ;
begin
  Result := iLocklevel > 0 ;
end;

procedure T_PvlForm.handleOnShow( _sender : TObject );
var
  o : TGIS_PvlBaseForm;
begin
  o := oParent as TGIS_PvlBaseForm ;
  if Assigned( o.OnShow ) then
    o.OnShow( self ) ;

  o.doShowForm ;
end;

function T_PvlForm.doShowModal
  : TGIS_PvlModalResult;
begin
  case oForm.ShowModal  of
    mrOk     : Result := TGIS_PvlModalResult.OK     ;
    mrCancel : Result := TGIS_PvlModalResult.Cancel ;
    mrAbort  : Result := TGIS_PvlModalResult.Abort  ;
    else       Result := TGIS_PvlModalResult.None   ;
  end;
end;

procedure T_PvlForm.UnlockWindow ;
begin
  if not oForm.Visible then
    exit ;

  Dec( iLockLevel ) ;

  if iLockLevel > 0 then
    exit ;

  Assert( iLockLevel >= 0 ) ;

  SendMessage( oForm.Handle, WM_SETREDRAW, Ord(True), 0);
  RedrawWindow( oForm.Handle, nil, 0,
                RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN
              );

  iLockLevel := 0 ;
end;


procedure T_PvlForm.doBeforeMonitorDpiChanged(
  _sender : TObject ;
  _oldDPI : Integer ;
  _newDPI: Integer
);
begin
  iPPI := _newDPI ;
  lockWindow ;
end;

procedure T_PvlForm.doAfterMonitorDpiChanged(
  _sender : TObject ;
  _oldDPI : Integer ;
  _newDPI: Integer
);
begin
  DoAfterPPIChanged ;
  iPPI := _newDPI ;
  DoRedraw ;

  unlockWindow ;
end;

procedure T_PvlForm.doOnResize(
  _sender: TObject
) ;
begin
  DoResize ;
end;

procedure T_PvlForm.DoRedraw ;
var
  frm : IGIS_PvlForm ;
begin
  frm := oParent as IGIS_PvlForm ;
  frm.fget_Context.UpdatePPI( iPPI, fget_PPIFix, dScale ) ;
  frm.DoRedraw ;
end;

procedure T_PvlForm.DoResize;
begin
  ( oParent as IGIS_PvlForm ).DoResize ;
end;

procedure T_PvlForm.Show;
begin
  oForm.bFreeOnClose := True ;
  oForm.Show ;
end;

procedure T_PvlForm.Show(
   const _free : Boolean
) ;
begin
  oForm.bFreeOnClose := _free ;
  oForm.Show ;
end;

procedure T_PvlForm.Hide;
begin
  oForm.Hide ;
end;

procedure T_PvlForm.FreeForm ;
begin
  // Do nothing
end;

function T_PvlForm.ShowModal(
  const _proc : TGIS_Proc;
  const _free : Boolean
): TGIS_PvlModalResult;
begin
  Result := TGIS_PvlModalResult.None ;

  oForm.bFreeOnClose := _free ;
  if assigned( _proc ) then begin
    _proc( doShowModal ) ;
  end
  else
    Result := doShowModal ;
end;

function T_PvlForm.ShowModal
  : TGIS_PvlModalResult;
begin
  oForm.bFreeOnClose := False ;

  Result := doShowModal ;

  fset_ModalResult( Result ) ;
end;

procedure T_PvlForm.Close;
begin
  oForm.bFreeOnClose := True ;
  oForm.Close ;
end;

procedure T_PvlForm.ProcessMessages ;
begin
  Application.ProcessMessages ;
end;

{$ENDREGION 'T_PvlForm'}

{$REGION 'T_PvlModalFormDesktop'}

constructor T_PvlModalFormDesktop.Create(
  const _context    : TGIS_PvlContext;
  const _parent     : IGIS_PvlBase
);
begin
  inherited Create( _context, _parent ) ;
  oForm.OnKeyDown := doKeyDown ;
end;

function T_PvlModalFormDesktop.fget_BtnOK
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_BtnOK ;
end;

function T_PvlModalFormDesktop.fget_BtnCancel
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_BtnCancel ;
end;

function T_PvlModalFormDesktop.fget_BtnHelp
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_BtnHelp ;
end;

function T_PvlModalFormDesktop.fget_OnHelp
  : TGIS_HelpEvent;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_OnHelp ;
end;

procedure T_PvlModalFormDesktop.fset_OnHelp(
  const _value    : TGIS_HelpEvent
);
begin
  ( oParent as IGIS_PvlModalForm ).fset_OnHelp( _value );
end;

procedure T_PvlModalFormDesktop.doKeyDown(
        _sender  : TObject;
    var _key     : Word ;
        _shift   : TShiftState
) ;
var
  par : TGIS_PvlModalForm ;//?
begin
  inherited ;

  par := oParent as TGIS_PvlModalForm ;

  if      _key = vkEscape then begin
                                 if ( oForm.ActiveControl is TComboBox ) and
                                    TComboBox(oForm.ActiveControl).DroppedDown then exit ;
                                 if oForm.ActiveControl is TListBox then exit ;
                                 par.BtnCancel.OnClick( Self )
                               end
  else if _key = vkReturn then begin
                                 if oForm.ActiveControl is TMemo then exit ;
                                 if oForm.ActiveControl is TListBox then exit ;
                                   par.BtnOk.OnClick( Self ) ;
                               end
  else if _key = vkF1     then begin
                                 par.BtnHelp.OnClick( Self ) ;
                               end ;
end;

{$ENDREGION 'T_PvlModalFormDesktop'}

{$REGION 'T_PvlModalWizardDesktop'}

constructor T_PvlModalWizardDesktop.Create(
  const _context    : TGIS_PvlContext;
  const _parent     : IGIS_PvlBase
);
begin
  inherited Create( _context, _parent ) ;
  oForm.OnKeyDown := doKeyDown ;
end;

function T_PvlModalWizardDesktop.fget_BtnOK
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_BtnOK ;
end;

function T_PvlModalWizardDesktop.fget_BtnCancel
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_BtnCancel ;
end;

function T_PvlModalWizardDesktop.fget_BtnNext
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalWizard ).fget_btnNext ;
end;

function T_PvlModalWizardDesktop.fget_BtnPrevious
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalWizard ).fget_btnPrevious ;
end;

function T_PvlModalWizardDesktop.fget_Pages
  : TGIS_PvlPages;
begin
  Result := ( oParent as IGIS_PvlModalWizard ).fget_Pages ;
end;

function T_PvlModalWizardDesktop.fget_BtnHelp
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_BtnHelp ;
end;

function T_PvlModalWizardDesktop.fget_OnHelp
  : TGIS_HelpEvent;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_OnHelp ;
end;

procedure T_PvlModalWizardDesktop.fset_OnHelp(
  const _value    : TGIS_HelpEvent
);
begin
  ( oParent as IGIS_PvlModalForm ).fset_OnHelp( _value );
end;

procedure T_PvlModalWizardDesktop.doKeyDown(
        _sender  : TObject;
    var _key     : Word ;
        _shift   : TShiftState
) ;
var
  par : TGIS_PvlModalWizard ;//?
begin
  inherited ;

  par := oParent as TGIS_PvlModalWizard ;

  if _sender is TGIS_PvlSearchBox then
    exit ;

  if      _key = vkEscape then begin
                                 if ( oForm.ActiveControl is TComboBox ) and
                                    TComboBox(oForm.ActiveControl).DroppedDown then exit ;
                                 if oForm.ActiveControl is TListBox then exit ;
                                 par.BtnCancel.OnClick( Self )
                               end
  else if _key = vkReturn then begin
                                 if oForm.ActiveControl is TMemo then exit ;
                                 if oForm.ActiveControl is TListBox then exit ;
                                 if par.BtnOK.Visible then
                                   par.BtnOk.OnClick( Self ) ;
                                 if par.BtnNext.Visible then
                                  par.BtnNext.OnClick( Self ) ;
                               end
  else if _key = vkF1     then begin
                                 par.BtnHelp.OnClick( Self ) ;
                               end ;
end;

{$ENDREGION 'T_PvlModalWizardDesktop'}

initialization
  RegisterPVLPlatformControl( 'Form', T_PvlForm ) ;

  RegisterPVLPlatformControl( 'ModalForm', T_PvlModalFormDesktop ) ;
  RegisterPVLPlatformControl( 'ModalWizard', T_PvlModalWizardDesktop ) ;

  {$IFDEF GIS_EDITOR_PDK_FIX}
    editorFixCriticalSection := TCriticalSection.Create ;
  {$ENDIF}

{$IFDEF GIS_EDITOR_PDK_FIX}
  finalization
    FreeObject( editorFixCriticalSection );
{$ENDIF}


//==================================== END =====================================
end.

