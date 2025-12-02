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
  Implementation of PVL forms for FMX
}

unit Lider.CG.GIS.FMX.GeoPvlForms;

interface

uses
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.FMX.GeoPvl,
  Lider.CG.GIS.PVL.GeoPvl,
  Lider.CG.GIS.PVL.GeoPvlForms;

//##############################################################################
implementation

uses
  System.Classes,
  System.Types,
  System.Math,
  System.UITypes,
  FMX.Ani,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.ComboEdit,
  FMX.Memo,
  FMX.Objects,
  FMX.Forms,
  FMX.Layouts,
  FMX.Platform,
  FMX.Graphics,
  FMX.BehaviorManager,
  {$IFNDEF LEVEL_RX10_FMX}
    FMX.Styles,
  {$ENDIF}

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoResource ;


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

  T_PvlForm = class( TGIS_PvlBaseFmx, IGIS_PvlForm )
    private
      iPPI       : Integer;
      iSystemPPI : Integer;
      dScale     : Single;
      oForm      : T_Form ;
      oldOnPaint : TOnPaintEvent ;
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
      function  fget_RightToLeft  : Boolean;

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
      procedure UnlockWindow      ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure DoRedraw          ;
                                  virtual;
    private
      procedure handleOnPaint     (       _sender : TObject ;
                                          _canvas : TCanvas ;
                                    const _arect  : TRectF
                                  );
      procedure handleOnShow      (       _sender : TObject
                                  );
      function  doShowModal       : TGIS_PvlModalResult ;
    public
      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure Show              ; overload;

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
                                    var  _keychar   : WideChar ;
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
                                    var  _keychar   : WideChar ;
                                         _shift     : TShiftState
                                  ) ;
  end;

  T_PvlModalFormMobile = class( TGIS_PvlBaseFmx, IGIS_PvlForm, IGIS_PvlModalForm )
    private
      pCallBack     : TGIS_Proc ;
      modal_result  : TGIS_PvlModalResult ;
      bidi          : TBiDiMode ;
      rct_back      : TRectangle   ;
      rct_main      : TRectangle   ;
      lay_form      : TLayout      ;
      tlb           : TRectangle   ;
      title         : TLabel       ;
      dKeyboardScroll          : Single ;
      oKeyboardScrollControl   : TControl ;
      oKeyboardScrollAnimation : TFloatAnimation ;
      oKeyboardScrollTimer     : TTimer ;

      iMaxWidth     : Integer      ;
      iMaxHeight    : Integer      ;
      iPPI          : Integer      ;
      iSystemPPI    : Integer      ;
      oParentForm   : TCustomForm  ;
      iClientWidth  : Integer ;
      iClientHeight : Integer ;
      sCaption      : String ;
      sName         : String ;
      prevKeyboardShown        : procedure(       _sender  : TObject;
                                                  _visible : Boolean;
                                            const _bounds  : TRect
                                          ) of object ;
      prevKeyboardHidden       : procedure(       _sender  : TObject;
                                                  _visible : Boolean;
                                            const _bounds  : TRect
                                          ) of object ;
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
      function  fget_RightToLeft  : Boolean;

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
      procedure UnlockWindow      ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure DoRedraw          ;
                                  virtual;
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
      procedure doApplyStyleLookup(      _sender   : TObject
                                  );

      procedure doKeyDown         (      _sender    : TObject ;
                                    var  _key       : Word ;
                                    var  _keychar   : WideChar ;
                                         _shift     : TShiftState
                                  ) ;
      procedure doKeyboardShown   (       _sender  : TObject;
                                          _visible : Boolean;
                                    const _bounds  : TRect
                                  ) ;
      procedure doKeyboardHidden  (       _sender  : TObject;
                                          _visible : Boolean;
                                    const _bounds  : TRect
                                  ) ;
      procedure doKeyboardScrollTimer(
                                          _sender  : TObject
                                  );
      procedure handleOnShow      (       _sender : TObject
                                  );
    public
      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure Show            ; overload;

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
  end;


  T_PvlModalWizardMobile = class( TGIS_PvlBaseFmx, IGIS_PvlForm, IGIS_PvlModalWizard )
    private
      pCallBack     : TGIS_Proc ;
      modal_result  : TGIS_PvlModalResult ;
      bidi          : TBiDiMode ;
      rct_back      : TRectangle   ;
      rct_main      : TRectangle   ;
      lay_form      : TLayout      ;
      tlb           : TRectangle   ;
      title         : TLabel       ;
      dKeyboardScroll          : Single ;
      oKeyboardScrollControl   : TControl ;
      oKeyboardScrollAnimation : TFloatAnimation ;
      oKeyboardScrollTimer     : TTimer ;

      iMaxWidth     : Integer      ;
      iMaxHeight    : Integer      ;
      iPPI          : Integer      ;
      iSystemPPI    : Integer      ;
      oParentForm   : TCustomForm  ;
      iClientWidth  : Integer ;
      iClientHeight : Integer ;
      sCaption      : String ;
      sName         : String ;
      prevKeyboardShown        : procedure(       _sender  : TObject;
                                                  _visible : Boolean;
                                            const _bounds  : TRect
                                          ) of object ;
      prevKeyboardHidden       : procedure(       _sender  : TObject;
                                                  _visible : Boolean;
                                            const _bounds  : TRect
                                          ) of object ;
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
      function  fget_RightToLeft  : Boolean;

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
      procedure UnlockWindow      ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure DoRedraw          ;
                                  virtual;
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
      procedure doApplyStyleLookup(      _sender   : TObject
                                  );

      procedure doKeyDown         (      _sender    : TObject ;
                                    var  _key       : Word ;
                                    var  _keychar   : WideChar ;
                                         _shift     : TShiftState
                                  ) ;
      procedure doKeyboardShown   (       _sender  : TObject;
                                          _visible : Boolean;
                                    const _bounds  : TRect
                                  ) ;
      procedure doKeyboardHidden  (       _sender  : TObject;
                                          _visible : Boolean;
                                    const _bounds  : TRect
                                  ) ;
      procedure doKeyboardScrollTimer(
                                          _sender  : TObject
                                  );
      procedure handleOnShow      (       _sender : TObject
                                  );
    public
      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure Show            ; overload;

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
  end;

{$REGION 'T_Form'}

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
  obehaviorsvc : IDeviceBehavior;
  met          : TDeviceDisplayMetrics;
  owidowscvs   : IFMXWindowService;
  border_style : TGIS_PvlBorderStyle ;
  border_icons : TGIS_PvlBorderIcons ;
begin

  inherited Create( _context, _parent ) ;

  border_style := fget_BorderStyle ;
  border_icons := fget_BorderIcons ;
  oForm := T_Form.CreateNew( TFmxObject( _context.NativeParent ) );
  oControl := oForm ;

  oForm.oParent := self;
  oForm.oMaster := _parent as TGIS_PvlBaseForm;
  oForm.OnClose := oForm.doFormClose;
  oForm.OnDestroy := oForm.doFormDestroy;

  oForm.OnShow := handleOnShow ;
  oldOnPaint := oForm.OnPaint ;
  oForm.OnPaint := handleOnPaint ;
  oControl := oForm ;

  case border_style of
    TGIS_PvlBorderStyle.Fixed              : oForm.BorderStyle := TFmxFormBorderStyle.Single ;
    TGIS_PvlBorderStyle.Sizeable           : oForm.BorderStyle := TFmxFormBorderStyle.Sizeable ;
    TGIS_PvlBorderStyle.ToolWindow         : oForm.BorderStyle := TFmxFormBorderStyle.ToolWindow ;
    TGIS_PvlBorderStyle.ToolWindowSizeable : oForm.BorderStyle := TFmxFormBorderStyle.SizeToolWin ;
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

  oForm.Position := TFormPosition.MainFormCenter ;

  if _rsbidi() then
    oForm.BiDiMode := TBiDiMode.bdRightToLeft
  else
    oForm.BiDiMode := TBiDiMode.bdLeftToRight ;

  if TBehaviorServices.Current.SupportsBehaviorService(
       IDeviceBehavior,
       obehaviorsvc, oForm
     )
  then begin
    met := obehaviorsvc.GetDisplayMetrics(oForm) ;
    iPPI       :=  met.PixelsPerInch ;
    iSystemPPI :=  RoundS( met.PixelsPerInch / met.ScreenScale ) ;
  end
  else begin
    iPPI       := 96 ;
    iSystemPPI := 96 ;
  end;

  if TPlatformServices.Current.SupportsPlatformService(
       IFMXWindowService,
       owidowscvs
     )
  then begin
//    iPPI := RoundS( owidowscvs.GetWindowScale(TCustomForm(oForm)) * iSystemPPI ) ; //Deprecated
    iPPI := RoundS( TCustomForm(oForm).Handle.Scale * iSystemPPI ) ;
  end ;

  dScale := oForm.Canvas.Scale ;

end;

destructor T_PvlForm.Destroy;
begin
  FreeObject( oForm ) ;
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

function T_PvlForm.fget_ClientHeight
  : Integer;
begin
  Result := oForm.ClientHeight ;
end;

procedure T_PvlForm.fset_ClientHeight(
  const _value : Integer
);
begin
  oForm.Clientheight := _value ;
end;

function T_PvlForm.fget_ClientWidth
  : Integer;
begin
  Result := oForm.ClientWidth ;
end;

procedure T_PvlForm.fset_ClientWidth(
  const _value : Integer
);
begin
  oForm.ClientWidth := _value ;
end;

function T_PvlForm.fget_RightToLeft
  : Boolean;
begin
  Result := oForm.BiDiMode = TBiDiMode.bdRightToLeft ;
end;

function T_PvlForm.fget_PPI
  : Integer;
begin
  Result := iPPI ;
end;

function T_PvlForm.fget_PPIFix
  : Single;
begin
  Result := 1 ; //on FMX always 1
end;

function T_PvlForm.fget_CanvasScale
  : Single;
begin
  Result := oForm.Canvas.Scale ;
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
  case TCustomForm( Self ).ModalResult of
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

procedure T_PvlForm.LockWindow ;
begin
  // do nothing
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
  {$IFNDEF GIS_MOBILE_DIALOGS}
    case oForm.ShowModal  of
      mrOk     : Result := TGIS_PvlModalResult.OK     ;
      mrCancel : Result := TGIS_PvlModalResult.Cancel ;
      mrAbort  : Result := TGIS_PvlModalResult.Abort  ;
      else       Result := TGIS_PvlModalResult.None   ;
    end;
  {$ELSE}
    oForm.Show ;
    Result := fget_ModalResult ;
  {$ENDIF}
end;

procedure T_PvlForm.handleOnPaint(
        _sender : TObject ;
        _canvas : TCanvas ;
  const _arect  : TRectF
);
begin
  if dScale <> _canvas.Scale then begin
    dScale := _canvas.Scale ;

    DoRedraw ;
  end;

  if Assigned( oldOnPaint ) then
     oldOnPaint( _sender, _canvas, _arect ) ;
end;

procedure T_PvlForm.UnlockWindow ;
begin
  // do nothing
end;

procedure T_PvlForm.DoRedraw ;
var
  frm : IGIS_PvlForm ;
begin
  frm := oParent as IGIS_PvlForm ;
  frm.fget_Context.UpdatePPI( iPPI, 1, dScale ) ;
  frm.DoRedraw ;
end;

procedure T_PvlForm.Show;
begin
  oForm.Show ;
  //? Free when???
end;

procedure T_PvlForm.Close;
begin
  oForm.bFreeOnClose := True ;
  oForm.Close ;
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
  Result := doShowModal ;

  fset_ModalResult( Result ) ;
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
    var _keychar : WideChar ;
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
                                 par.BtnCancel.OnClick( Self )
                               end
  else if _key = vkReturn then begin
                                 if oForm.ActiveControl is TMemo then exit ;
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
    var _keychar : WideChar ;
        _shift   : TShiftState
) ;
var
  par : TGIS_PvlModalWizard ;//?
begin
  inherited ;

  par := oParent as TGIS_PvlModalWizard ;

  if      _key = vkEscape then begin
                                 if ( oForm.ActiveControl is TComboBox ) and
                                    TComboBox(oForm.ActiveControl).DroppedDown then exit ;
                                 par.BtnCancel.OnClick( Self )
                               end
  else if _key = vkReturn then begin
                                 if oForm.ActiveControl is TMemo then exit ;
                                   par.BtnOk.OnClick( Self ) ;
                               end
  else if _key = vkF1     then begin
                                 par.BtnHelp.OnClick( Self ) ;
                               end ;
end;

{$ENDREGION 'T_PvlModalWizardDesktop'}

{$REGION 'T_PvlModalFormMobile'}

constructor T_PvlModalFormMobile.Create(
  const _context    : TGIS_PvlContext;
  const _parent     : IGIS_PvlBase
);
var
  oscreensvc   : IFMXScreenService;
  obehaviorsvc : IDeviceBehavior;
  met          : TDeviceDisplayMetrics;
  owidowscvs   : IFMXWindowService;

  lbl : TLabel ; // ??stramge; seems that having such dummy control resolve
                 // ??issue of aliging first label on the form
  btn : TSpeedButton ;
  h   : Integer      ;

  function parent_form(
    const _context : TGIS_PvlContext
  ) : TCustomForm ;
  var
    otmp : TFmxObject ;
  begin

    otmp := TFmxObject( _context.NativeParent ) ;
    while Assigned( otmp ) do begin
      if otmp is TCustomForm then
        break ;
      otmp := otmp.Parent ;
    end;

    Result := otmp as TCustomForm;
  end;
begin
  if _rsbidi() then
    bidi := TBiDiMode.bdRightToLeft
  else
    bidi := TBiDiMode.bdLeftToRight ;

  inherited Create( _context, _parent ) ;

  modal_result := TGIS_PvlModalResult.None ;

  oParentForm := parent_form( _context ) ;

  if TPlatformServices.Current.SupportsPlatformService(
      IFMXScreenService,
      IInterface(oscreensvc)
     )
  then begin
    iMaxWidth  := RoundS( oscreensvc.GetScreenSize.X * 0.8 ) ;
    iMaxHeight := RoundS( oscreensvc.GetScreenSize.Y * 0.8 ) ;
  end
  else begin
    iMaxWidth  := RoundS( Screen.Width  ) ;
    iMaxHeight := RoundS( Screen.Height ) ;
  end;

  if TBehaviorServices.Current.SupportsBehaviorService(
       IDeviceBehavior,
       obehaviorsvc, oParentForm
     )
  then begin
    met := obehaviorsvc.GetDisplayMetrics(oParentForm) ;
    iPPI       :=  met.PixelsPerInch ;
    iSystemPPI :=  RoundS( met.PixelsPerInch / met.ScreenScale ) ;
  end
  else begin
    iPPI       := 96 ;
    iSystemPPI := 96 ;
  end;

  if TPlatformServices.Current.SupportsPlatformService(
       IFMXWindowService,
       owidowscvs
     )
  then begin
//    iPPI := RoundS( owidowscvs.GetWindowScale( oParentForm ) * iSystemPPI ) ; //Deprecated
    iPPI := RoundS( oParentForm.Handle.Scale * iSystemPPI ) ;
  end ;

  iMaxWidth  := RoundS( oParentForm.ClientWidth  * 0.95 ) ;
  iMaxHeight := RoundS( oParentForm.ClientHeight * 0.95 ) ;

  btn := TSpeedButton.Create( oParentForm );
  btn.StyleLookup := 'donetoolbutton' ;
  h := RoundS( btn.Height ) ;
  btn.Free ;

  iMaxWidth  := iMaxWidth  -  2 * FORM_MARGIN ;
  iMaxHeight := iMaxHeight -  2 * FORM_MARGIN - h ;

  rct_back := TRectangle.Create( oParentForm ) ;
  oControl := rct_back ;

  rct_back.Parent := oParentForm ;
  rct_back.Opacity := 1.0 ;
  rct_back.Stroke.Thickness := 0 ;
  rct_back.Fill.Color := TAlphaColorRec.Null ;
  rct_back.Align := TAlignLayout.Contents ;
  rct_back.Visible := False ;

  oKeyboardScrollAnimation := TFloatAnimation.Create( rct_back ) ;
  oKeyboardScrollAnimation.Parent := rct_back ;
  oKeyboardScrollAnimation.Duration := 0.2;
  oKeyboardScrollAnimation.PropertyName := 'Position.Y' ;
  oKeyboardScrollAnimation.StartFromCurrent := True ;
  oKeyboardScrollAnimation.Enabled := True ;

  rct_main := TRectangle.Create( rct_back ) ;
  rct_main.Parent := TFmxObject( rct_back ) ;
  rct_main.XRadius := 5 ;
  rct_main.YRadius := 5 ;
  rct_main.Fill.Color := TAlphaColorRec.White ;
  rct_main.Fill.Kind := TBrushKind.Solid ;
  rct_main.Stroke.Color := TAlphaColorRec.Gray ;
  rct_main.Align := TAlignLayout.Center ;
  rct_main.Padding.Left := 1 ;
  rct_main.Padding.Top := 1 ;
  rct_main.Padding.Right := 1 ;
  rct_main.Padding.Bottom := 1 ;

  tlb := TRectangle.Create( rct_main ) ;
  tlb.Parent := TFmxObject( rct_main ) ;
  tlb.XRadius := 5 ;
  tlb.YRadius := 5 ;
  tlb.Fill.Color := TAlphaColorRec.Whitesmoke ;
  tlb.Fill.Kind := TBrushKind.Solid ;
  tlb.Stroke.Color := TAlphaColorRec.Null ;
  tlb.Corners := [TCorner.TopLeft, TCorner.TopRight];
  tlb.Align := TAlignLayout.Top ;
  tlb.Padding.Left   := FORM_MARGIN ;
  tlb.Padding.Right  := FORM_MARGIN ;
  tlb.Height := 30 ;

  title := TLabel.Create( tlb ) ;
  title.Parent := tlb ;
  title.Align := TAlignLayout.Client ;
  title.TextAlign := TTextAlign.Center ;
  title.TextSettings.WordWrap := False ;
  title.TextSettings.Trimming := TTextTrimming.Character ;
  title.StyleLookup := 'toollabel' ;
  title.Text := sCaption ;

  lay_form := TLayout.Create( rct_main );
  lay_form.Parent := rct_main ;
  lay_form.Align := TAlignLayout.Client ;
  lay_form.OnKeyDown := doKeyDown ; //? Doesnt work
  lay_form.ClipChildren := True ;

  oKeyboardScrollTimer := TTimer.Create( rct_main );
  oKeyboardScrollTimer.Enabled := False ;
  oKeyboardScrollTimer.Interval := 100 ;
  oKeyboardScrollTimer.OnTimer := doKeyboardScrollTimer ;

  oControl := lay_form ;
end;

destructor T_PvlModalFormMobile.Destroy;
begin
  FreeObject( rct_back ) ;
  inherited ;
end;

function T_PvlModalFormMobile.fget_Name
  : String;
begin
  Result := sName ;
end;

procedure T_PvlModalFormMobile.fset_Name(
  const _value : String
);
begin
  sName := _value ;
end;

function T_PvlModalFormMobile.fget_Caption
  : String;
begin
  Result := sCaption ;
end;

procedure T_PvlModalFormMobile.fset_Caption(
  const _value : String
);
begin
  sCaption := _value ;
  title.Text := _value ;
end;

function T_PvlModalFormMobile.fget_ClientHeight
  : Integer;
begin
  Result := iClientHeight ;
end;

procedure T_PvlModalFormMobile.fset_ClientHeight(
  const _value : Integer
);
begin
  iClientHeight := Min( iMaxHeight, RoundS( _value ) ) ;
  rct_main.Height := iClientHeight + tlb.Height;
end;

function T_PvlModalFormMobile.fget_ClientWidth
  : Integer;
begin
  Result := iClientWidth ;
end;

procedure T_PvlModalFormMobile.fset_ClientWidth(
  const _value : Integer
);
begin
  iClientWidth := Min( iMaxWidth, RoundS( _value ) ) ;
  rct_main.Width := iClientWidth ;
end;

function T_PvlModalFormMobile.fget_RightToLeft
  : Boolean;
begin
  Result := bidi = TBiDiMode.bdRightToLeft ;
end;

function T_PvlModalFormMobile.fget_PPI
  : Integer;
begin
  Result := iPPI ;
end;

function T_PvlModalFormMobile.fget_PPIFix
  : Single;
begin
  Result := 1 ; //on FMX always 1
end;

function T_PvlModalFormMobile.fget_CanvasScale
  : Single;
begin
  Result := rct_main.Canvas.Scale ;
end;

function T_PvlModalFormMobile.fget_Context
 : TGIS_PvlContext;
begin
  // do nothing
  Result := nil ;
end;

function T_PvlModalFormMobile.fget_BorderStyle
  : TGIS_PvlBorderStyle;
begin
  Result := ( oParent as IGIS_PvlForm ).fget_BorderStyle ;
end;

function T_PvlModalFormMobile.fget_BorderIcons
  : TGIS_PvlBorderIcons;
begin
  Result := ( oParent as IGIS_PvlForm ).fget_BorderIcons ;
end;

function T_PvlModalFormMobile.fget_ModalResult
  : TGIS_PvlModalResult;
begin
  Result := modal_result ;
end;

procedure T_PvlModalFormMobile.fset_ModalResult(
  const _value: TGIS_PvlModalResult
);
begin
  modal_result := _value ;

  close ;

  if Assigned( pCallback ) then
    pCallBack( modal_result ) ;

  oParentForm.OnVirtualKeyboardShown  := prevKeyboardShown ;
  oParentForm.OnVirtualKeyboardHidden := prevKeyboardHidden ;
end;

procedure T_PvlModalFormMobile.doUpdateGUI ;
begin
  // do nothing
end;

procedure T_PvlModalFormMobile.doInitButtons ;
begin
  // do nothing
end;

procedure T_PvlModalFormMobile.doAfterPPIChanged ;
begin
  // do nothing
end;

procedure T_PvlModalFormMobile.doShowForm ;
begin
  // do nothing
end;

procedure T_PvlModalFormMobile.LockWindow ;
begin
  // do nothing
end;

procedure T_PvlModalFormMobile.UnlockWindow;
begin
  // do nothing
end;

procedure T_PvlModalFormMobile.DoRedraw;
begin
  // do nothing
end;

procedure T_PvlModalFormMobile.handleOnShow( _sender : TObject );
var
  o : TGIS_PvlBaseForm;
begin
  o := oParent as TGIS_PvlBaseForm ;
  if Assigned( o.OnShow ) then
    o.OnShow( self ) ;

  o.doShowForm ;
end;

function T_PvlModalFormMobile.fget_BtnOK
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_BtnOK ;
end;

function T_PvlModalFormMobile.fget_BtnCancel
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_BtnCancel ;
end;

function T_PvlModalFormMobile.fget_BtnHelp
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_BtnHelp ;
end;

function T_PvlModalFormMobile.fget_OnHelp
  : TGIS_HelpEvent;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_OnHelp ;
end;

procedure T_PvlModalFormMobile.fset_OnHelp(
  const _value    : TGIS_HelpEvent
);
begin
  ( oParent as IGIS_PvlModalForm ).fset_OnHelp( _value );
end;

procedure T_PvlModalFormMobile.doPlaceButtons ;
begin
  TSpeedButton( fget_BtnHelp.NativeControl ).Parent := tlb ;
  TSpeedButton( fget_BtnHelp.NativeControl ).Align := TAlignLayout.Left ;
  {$IFDEF ANDROID}
    TSpeedButton( fget_BtnHelp.NativeControl ).StyleLookup := 'backtoolbutton' ;
  {$ELSE}
    TSpeedButton( fget_BtnHelp.NativeControl ).StyleLookup := 'toolbutton' ;
  {$ENDIF}
  fget_BtnHelp.Width := 73 ;

  TSpeedButton( fget_BtnCancel.NativeControl ).Parent := tlb ;
  TSpeedButton( fget_BtnCancel.NativeControl ).Align := TAlignLayout.Left ;
  {$IFDEF ANDROID}
    TSpeedButton( fget_BtnCancel.NativeControl ).StyleLookup := 'backtoolbutton' ;
  {$ELSE}
    TSpeedButton( fget_BtnCancel.NativeControl ).StyleLookup := 'toolbutton' ;
  {$ENDIF}
  TSpeedButton( fget_BtnCancel.NativeControl ).Width := 73 ;


  TSpeedButton( fget_btnOk.NativeControl ).Parent := tlb ;
  TSpeedButton( fget_btnOk.NativeControl ).Align := TAlignLayout.Right ;
  TSpeedButton( fget_btnOk.NativeControl ).StyleLookup := 'donetoolbutton' ;
  TSpeedButton( fget_btnOk.NativeControl ).Text := _rsrcna( GIS_RS_BTN_DONE ) ;
  TSpeedButton( fget_btnOk.NativeControl ).Width := 73 ;

  tlb.Height := Max( fget_btnOk.Height, fget_btnCancel.Height ) ;
end;


procedure T_PvlModalFormMobile.doApplyStyleLookup;
var
  bmp    : TBitmap    ;
  obj    : TFmxObject ;
  ctrl   : TControl   ;

  function doStyleObject( const _style_name : String ) : TControl ;
  var
    o : TFmxObject ;
  begin
    o := nil ;
    {$IFNDEF LEVEL_RX10_FMX}
      if Assigned(TStyleManager.ActiveStyleForScene( oParentForm ) ) then
        o := TStyleManager.ActiveStyleForScene(
               oParentForm
             ).FindStyleResource(_style_name);
    {$ELSE}
      o:= TStyledControl.LookupStyleObject(
            Self, Self, FParentForm, _style_name, '', '', True
          );
    {$ENDIF}

    if o is TControl then
      Result := TControl( o )
    else
     Result := nil ;

    DoRedraw ;
  end;

begin
  // apply styles
  // because we use TRect not a full styled control (to have rounded cornder)
  // we clone style by painting style to bitmap then applying style
  // to TRect brush.

  exit ;

  ctrl := doStyleObject( 'backgroundstyle' ) ;
  try
    if Assigned( ctrl ) then begin
      ctrl.Width  := rct_main.Width ;
      ctrl.Height := rct_main.Height ;
      bmp := TBitmap.Create( Round(ctrl.Width) , Round(ctrl.Height) ) ;
      try
        bmp.Canvas.BeginScene();
        ctrl.PaintTo( bmp.Canvas, RectF( 0, 0, bmp.Width, bmp.Height ) ) ;
        bmp.Canvas.EndScene();
        rct_main.Fill.Bitmap.Bitmap.Assign(bmp) ;
      finally
        bmp.Free ;
      end;
      rct_main.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
      rct_main.Fill.Kind := TBrushKind.Bitmap;
    end;
  finally
    FreeObject( ctrl ) ;
  end ;

  ctrl := doStyleObject( 'labelstyle' ) ;
  try
    if Assigned( ctrl ) then begin
      obj := ctrl.FindStyleResource('text') ;
      if obj is TText then begin
        rct_main.Stroke.Color := TText(obj).TextSettings.FontColor ;
      end;
    end;
  finally
    FreeObject( ctrl ) ;
  end ;

  ctrl := doStyleObject( 'toolbarstyle' ) ;
  try
    if Assigned( ctrl ) then begin
      ctrl.Width  := tlb.Width ;
      ctrl.Height := tlb.Height ;
      bmp := TBitmap.Create( Round(ctrl.Width) , Round(ctrl.Height) ) ;
      try
        bmp.Canvas.BeginScene();
        ctrl.PaintTo( bmp.Canvas, RectF( 0, 0, bmp.Width, bmp.Height ) ) ;
        bmp.Canvas.EndScene();
        tlb.Fill.Bitmap.Bitmap.Assign(bmp) ;
      finally
        bmp.Free ;
      end;
      tlb.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
      tlb.Fill.Kind := TBrushKind.Bitmap;
    end;
  finally
    FreeObject( ctrl ) ;
  end ;
end;

procedure T_PvlModalFormMobile.doKeyboardShown(
        _sender  : TObject;
        _visible : Boolean;
  const _bounds  : TRect
);
var
  clnt : TPointF ;
  delta : Single ;
begin
  delta := 0 ;

  clnt := oParentForm.ScreenToClient( _bounds.TopLeft )  ;

  if oParentForm.Focused is TControl then begin
    oKeyboardScrollControl :=  oParentForm.Focused as TControl ;

    if oKeyboardScrollControl.AbsoluteRect.Bottom > clnt.Y  then begin
      delta := -(clnt.Y - oKeyboardScrollControl.AbsoluteRect.Bottom  )  ;
    end;

    dKeyboardScroll := rct_back.Position.Y ;

    oKeyboardScrollAnimation.StopValue := rct_back.Position.Y - delta ;
    oKeyboardScrollAnimation.Start ;
  end ;
end ;

procedure T_PvlModalFormMobile.doKeyboardHidden(
        _sender  : TObject;
        _visible : Boolean;
  const _bounds  : TRect
);
begin
  oKeyboardScrollTimer.Enabled := True ;
end;

procedure T_PvlModalFormMobile.doKeyboardScrollTimer(
  _sender : TObject
) ;
begin
  // don't scroll if if combobox is opened
  if oKeyboardScrollControl is TComboEdit then
    if TComboEdit(oKeyboardScrollControl).DroppedDown then
      exit ;

  oKeyboardScrollTimer.Enabled := False ;

  oKeyboardScrollAnimation.StopValue := dKeyboardScroll ;
  oKeyboardScrollAnimation.Start ;
end ;

procedure T_PvlModalFormMobile.doKeyDown(
        _sender  : TObject;
    var _key     : Word ;
    var _keychar : WideChar ;
        _shift   : TShiftState
) ;
var
  par : TGIS_PvlModalForm ;//?
begin
  inherited ;

  par := oParent as TGIS_PvlModalForm ;
  if      _key = vkEscape then begin
                                 if ( TCustomForm( Self ).ActiveControl is TComboBox ) and
                                    TComboBox(TCustomForm( Self ).ActiveControl).DroppedDown then exit ;
                                 par.BtnCancel.OnClick( Self )
                               end
  else if _key = vkReturn then begin
                                 if TCustomForm( Self ).ActiveControl is TMemo then exit ;
                                   par.BtnOk.OnClick( Self ) ;
                               end
  else if _key = vkF1     then begin
                                 par.BtnHelp.OnClick( Self ) ;
                               end ;

end;

procedure T_PvlModalFormMobile.FreeForm ;
begin
  //DO NOTHING
end;

procedure T_PvlModalFormMobile.Show ;
begin
  pCallBack := nil ;

  prevKeyboardShown  := oParentForm.OnVirtualKeyboardShown ;
  prevKeyboardHidden := oParentForm.OnVirtualKeyboardHidden ;

  oParentForm.OnVirtualKeyboardShown  := doKeyboardShown ;
  oParentForm.OnVirtualKeyboardHidden := doKeyboardHidden ;

  TSpeedButton( fget_btnOk.NativeControl ).OnApplyStyleLookup
   := doApplyStyleLookup ;

  doApplyStyleLookup( self ) ;

  handleOnShow( Self ) ;
  rct_back.Visible := True ;
end;

function  T_PvlModalFormMobile.ShowModal(
  const  _proc    : TGIS_Proc ;
  const _free     : Boolean
) : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlModalResult.None ;

  pCallBack := _proc ;

  prevKeyboardShown  := oParentForm.OnVirtualKeyboardShown ;
  prevKeyboardHidden := oParentForm.OnVirtualKeyboardHidden ;

  oParentForm.OnVirtualKeyboardShown  := doKeyboardShown ;
  oParentForm.OnVirtualKeyboardHidden := doKeyboardHidden ;

  ///? _free
  TSpeedButton( fget_btnOk.NativeControl ).OnApplyStyleLookup
    := doApplyStyleLookup ;

  doApplyStyleLookup( self ) ;

  handleOnShow( Self ) ;
  rct_back.Visible := True ;
end;

function  T_PvlModalFormMobile.ShowModal
  : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlModalResult.None ;

  pCallBack := nil ;

  prevKeyboardShown  := oParentForm.OnVirtualKeyboardShown ;
  prevKeyboardHidden := oParentForm.OnVirtualKeyboardHidden ;

  oParentForm.OnVirtualKeyboardShown  := doKeyboardShown ;
  oParentForm.OnVirtualKeyboardHidden := doKeyboardHidden ;


  TSpeedButton( fget_btnOk.NativeControl ).OnApplyStyleLookup
    := doApplyStyleLookup ;

  doApplyStyleLookup( self ) ;

  handleOnShow( Self ) ;
  rct_back.Visible := True ;
end;

procedure T_PvlModalFormMobile.Close ;
var
  o : TGIS_PvlBaseForm;
begin
  o := oParent as TGIS_PvlBaseForm ;
  if Assigned( o.OnClose ) then
    o.OnClose( self ) ;

  rct_back.Visible := False ;
end;

{$ENDREGION 'T_PvlModalFormMobile'}

{$REGION 'T_PvlModalWizardMobile'}

constructor T_PvlModalWizardMobile.Create(
  const _context    : TGIS_PvlContext;
  const _parent     : IGIS_PvlBase
);
var
  oscreensvc   : IFMXScreenService;
  obehaviorsvc : IDeviceBehavior;
  met          : TDeviceDisplayMetrics;
  owidowscvs   : IFMXWindowService;

  lbl : TLabel ; // ??stramge; seems that having such dummy control resolve
                 // ??issue of aliging first label on the form
  btn : TSpeedButton ;
  h   : Integer      ;

  function parent_form(
    const _context : TGIS_PvlContext
  ) : TCustomForm ;
  var
    otmp : TFmxObject ;
  begin

    otmp := TFmxObject( _context.NativeParent ) ;
    while Assigned( otmp ) do begin
      if otmp is TCustomForm then
        break ;
      otmp := otmp.Parent ;
    end;

    Result := otmp as TCustomForm;
  end;
begin
  if _rsbidi() then
    bidi := TBiDiMode.bdRightToLeft
  else
    bidi := TBiDiMode.bdLeftToRight ;

  inherited Create( _context, _parent ) ;

  modal_result := TGIS_PvlModalResult.None ;

  oParentForm := parent_form( _context ) ;

  if TPlatformServices.Current.SupportsPlatformService(
      IFMXScreenService,
      IInterface(oscreensvc)
     )
  then begin
    iMaxWidth  := RoundS( oscreensvc.GetScreenSize.X * 0.8 ) ;
    iMaxHeight := RoundS( oscreensvc.GetScreenSize.Y * 0.8 ) ;
  end
  else begin
    iMaxWidth  := RoundS( Screen.Width  ) ;
    iMaxHeight := RoundS( Screen.Height ) ;
  end;

  if TBehaviorServices.Current.SupportsBehaviorService(
       IDeviceBehavior,
       obehaviorsvc, oParentForm
     )
  then begin
    met := obehaviorsvc.GetDisplayMetrics(oParentForm) ;
    iPPI       :=  met.PixelsPerInch ;
    iSystemPPI :=  RoundS( met.PixelsPerInch / met.ScreenScale ) ;
  end
  else begin
    iPPI       := 96 ;
    iSystemPPI := 96 ;
  end;

  if TPlatformServices.Current.SupportsPlatformService(
       IFMXWindowService,
       owidowscvs
     )
  then begin
//    iPPI := RoundS( owidowscvs.GetWindowScale( oParentForm ) * iSystemPPI ) ;  //Deprecated
    iPPI := RoundS( oParentForm.Handle.Scale * iSystemPPI ) ;
  end ;

  iMaxWidth  := RoundS( oParentForm.ClientWidth  * 0.95 ) ;
  iMaxHeight := RoundS( oParentForm.ClientHeight * 0.95 ) ;

  btn := TSpeedButton.Create( oParentForm );
  btn.StyleLookup := 'donetoolbutton' ;
  h := RoundS( btn.Height ) ;
  btn.Free ;

  iMaxWidth  := iMaxWidth  -  2 * FORM_MARGIN ;
  iMaxHeight := iMaxHeight -  2 * FORM_MARGIN - h ;

  rct_back := TRectangle.Create( oParentForm ) ;
  oControl := rct_back ;

  rct_back.Parent := oParentForm ;
  rct_back.Opacity := 1.0 ;
  rct_back.Stroke.Thickness := 0 ;
  rct_back.Fill.Color := TAlphaColorRec.Null ;
  rct_back.Align := TAlignLayout.Contents ;
  rct_back.Visible := False ;

  oKeyboardScrollAnimation := TFloatAnimation.Create( rct_back ) ;
  oKeyboardScrollAnimation.Parent := rct_back ;
  oKeyboardScrollAnimation.Duration := 0.2;
  oKeyboardScrollAnimation.PropertyName := 'Position.Y' ;
  oKeyboardScrollAnimation.StartFromCurrent := True ;
  oKeyboardScrollAnimation.Enabled := True ;

  rct_main := TRectangle.Create( rct_back ) ;
  rct_main.Parent := TFmxObject( rct_back ) ;
  rct_main.XRadius := 5 ;
  rct_main.YRadius := 5 ;
  rct_main.Fill.Color := TAlphaColorRec.White ;
  rct_main.Fill.Kind := TBrushKind.Solid ;
  rct_main.Stroke.Color := TAlphaColorRec.Gray ;
  rct_main.Align := TAlignLayout.Center ;
  rct_main.Padding.Left := 1 ;
  rct_main.Padding.Top := 1 ;
  rct_main.Padding.Right := 1 ;
  rct_main.Padding.Bottom := 1 ;

  tlb := TRectangle.Create( rct_main ) ;
  tlb.Parent := TFmxObject( rct_main ) ;
  tlb.XRadius := 5 ;
  tlb.YRadius := 5 ;
  tlb.Fill.Color := TAlphaColorRec.Whitesmoke ;
  tlb.Fill.Kind := TBrushKind.Solid ;
  tlb.Stroke.Color := TAlphaColorRec.Null ;
  tlb.Corners := [TCorner.TopLeft, TCorner.TopRight];
  tlb.Align := TAlignLayout.Top ;
  tlb.Padding.Left   := FORM_MARGIN ;
  tlb.Padding.Right  := FORM_MARGIN ;
  tlb.Height := 30 ;

  title := TLabel.Create( tlb ) ;
  title.Parent := tlb ;
  title.Align := TAlignLayout.Client ;
  title.TextAlign := TTextAlign.Center ;
  title.TextSettings.WordWrap := False ;
  title.TextSettings.Trimming := TTextTrimming.Character ;
  title.StyleLookup := 'toollabel' ;
  title.Text := sCaption ;

  lay_form := TLayout.Create( rct_main );
  lay_form.Parent := rct_main ;
  lay_form.Align := TAlignLayout.Client ;
  lay_form.OnKeyDown := doKeyDown ; //? Doesnt work
  lay_form.ClipChildren := True ;

  oKeyboardScrollTimer := TTimer.Create( rct_main );
  oKeyboardScrollTimer.Enabled := False ;
  oKeyboardScrollTimer.Interval := 100 ;
  oKeyboardScrollTimer.OnTimer := doKeyboardScrollTimer ;

  oControl := lay_form ;
end;

destructor T_PvlModalWizardMobile.Destroy;
begin
  FreeObject( rct_back ) ;
  inherited ;
end;

function T_PvlModalWizardMobile.fget_Name
  : String;
begin
  Result := sName ;
end;

procedure T_PvlModalWizardMobile.fset_Name(
  const _value : String
);
begin
  sName := _value ;
end;

function T_PvlModalWizardMobile.fget_Caption
  : String;
begin
  Result := sCaption ;
end;

procedure T_PvlModalWizardMobile.fset_Caption(
  const _value : String
);
begin
  sCaption := _value ;
  title.Text := _value ;
end;

function T_PvlModalWizardMobile.fget_ClientHeight
  : Integer;
begin
  Result := iClientHeight ;
end;

procedure T_PvlModalWizardMobile.fset_ClientHeight(
  const _value : Integer
);
begin
  iClientHeight := Min( iMaxHeight, RoundS( _value ) ) ;
  rct_main.Height := iClientHeight + tlb.Height;
end;

function T_PvlModalWizardMobile.fget_ClientWidth
  : Integer;
begin
  Result := iClientWidth ;
end;

procedure T_PvlModalWizardMobile.fset_ClientWidth(
  const _value : Integer
);
begin
  iClientWidth := Min( iMaxWidth, RoundS( _value ) ) ;
  rct_main.Width := iClientWidth ;
end;

function T_PvlModalWizardMobile.fget_RightToLeft
  : Boolean;
begin
  Result := bidi = TBiDiMode.bdRightToLeft ;
end;

function T_PvlModalWizardMobile.fget_PPI
  : Integer;
begin
  Result := iPPI ;
end;

function T_PvlModalWizardMobile.fget_PPIFix
  : Single;
begin
  Result := 1 ; //on FMX always 1
end;

function T_PvlModalWizardMobile.fget_CanvasScale
  : Single;
begin
  Result := rct_main.Canvas.Scale ;
end;

function T_PvlModalWizardMobile.fget_Context
 : TGIS_PvlContext;
begin
  // do nothing
  Result := nil ;
end;

function T_PvlModalWizardMobile.fget_BorderStyle
  : TGIS_PvlBorderStyle;
begin
  Result := ( oParent as IGIS_PvlForm ).fget_BorderStyle ;
end;

function T_PvlModalWizardMobile.fget_BorderIcons
  : TGIS_PvlBorderIcons;
begin
  Result := ( oParent as IGIS_PvlForm ).fget_BorderIcons ;
end;

function T_PvlModalWizardMobile.fget_ModalResult
  : TGIS_PvlModalResult;
begin
  Result := modal_result ;
end;

procedure T_PvlModalWizardMobile.fset_ModalResult(
  const _value: TGIS_PvlModalResult
);
begin
  modal_result := _value ;

  close ;

  if Assigned( pCallback ) then
    pCallBack( modal_result ) ;

  oParentForm.OnVirtualKeyboardShown  := prevKeyboardShown ;
  oParentForm.OnVirtualKeyboardHidden := prevKeyboardHidden ;
end;

procedure T_PvlModalWizardMobile.doUpdateGUI ;
begin
  // do nothing
end;

procedure T_PvlModalWizardMobile.doInitButtons ;
begin
  // do nothing
end;

procedure T_PvlModalWizardMobile.doAfterPPIChanged ;
begin
  // do nothing
end;

procedure T_PvlModalWizardMobile.doShowForm ;
begin
  // do nothing
end;

procedure T_PvlModalWizardMobile.LockWindow ;
begin
  // do nothing
end;

procedure T_PvlModalWizardMobile.UnlockWindow;
begin
  // do nothing
end;

procedure T_PvlModalWizardMobile.DoRedraw;
begin
  // do nothing
end;

procedure T_PvlModalWizardMobile.handleOnShow( _sender : TObject );
var
  o : TGIS_PvlBaseForm;
begin
  o := oParent as TGIS_PvlBaseForm ;
  if Assigned( o.OnShow ) then
    o.OnShow( self ) ;

  o.doShowForm ;
end;

function T_PvlModalWizardMobile.fget_BtnOK
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalWizard ).fget_BtnOK ;
end;

function T_PvlModalWizardMobile.fget_BtnCancel
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalWizard ).fget_BtnCancel ;
end;

function T_PvlModalWizardMobile.fget_BtnHelp
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalWizard ).fget_BtnHelp ;
end;

function T_PvlModalWizardMobile.fget_BtnNext
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalWizard ).fget_btnNext ;
end;

function T_PvlModalWizardMobile.fget_BtnPrevious
  : TGIS_PvlModalButton;
begin
  Result := ( oParent as IGIS_PvlModalWizard ).fget_btnPrevious ;
end;

function T_PvlModalWizardMobile.fget_Pages
  : TGIS_PvlPages;
begin
  Result := ( oParent as IGIS_PvlModalWizard ).fget_Pages ;
end;

function T_PvlModalWizardMobile.fget_OnHelp
  : TGIS_HelpEvent;
begin
  Result := ( oParent as IGIS_PvlModalForm ).fget_OnHelp ;
end;

procedure T_PvlModalWizardMobile.fset_OnHelp(
  const _value    : TGIS_HelpEvent
);
begin
  ( oParent as IGIS_PvlModalForm ).fset_OnHelp( _value );
end;

procedure T_PvlModalWizardMobile.doPlaceButtons ;
begin
  TSpeedButton( fget_BtnHelp.NativeControl ).Parent := tlb ;
  TSpeedButton( fget_BtnHelp.NativeControl ).Align := TAlignLayout.Left ;
  {$IFDEF ANDROID}
    TSpeedButton( fget_BtnHelp.NativeControl ).StyleLookup := 'backtoolbutton' ;
  {$ELSE}
    TSpeedButton( fget_BtnHelp.NativeControl ).StyleLookup := 'toolbutton' ;
  {$ENDIF}
  fget_BtnHelp.Width := 73 ;

  TSpeedButton( fget_BtnCancel.NativeControl ).Parent := tlb ;
  TSpeedButton( fget_BtnCancel.NativeControl ).Align := TAlignLayout.Left ;
  {$IFDEF ANDROID}
    TSpeedButton( fget_BtnCancel.NativeControl ).StyleLookup := 'backtoolbutton' ;
  {$ELSE}
    TSpeedButton( fget_BtnCancel.NativeControl ).StyleLookup := 'toolbutton' ;
  {$ENDIF}
  TSpeedButton( fget_BtnCancel.NativeControl ).Width := 73 ;


  TSpeedButton( fget_btnOk.NativeControl ).Parent := tlb ;
  TSpeedButton( fget_btnOk.NativeControl ).Align := TAlignLayout.Right ;
  TSpeedButton( fget_btnOk.NativeControl ).StyleLookup := 'donetoolbutton' ;
  TSpeedButton( fget_btnOk.NativeControl ).Text := _rsrcna( GIS_RS_BTN_DONE ) ;
  TSpeedButton( fget_btnOk.NativeControl ).Width := 73 ;

  //? Rethink positioning !!!
  fget_BtnNext.Place( 75, 0, nil, - oContext.VMargin, nil, RoundS( fget_ClientHeight - fget_BtnNext.Height - oContext.HMargin ) ) ;
  fget_BtnPrevious.Place( 75, 0, fget_BtnNext, -oContext.VSpace, nil, RoundS( fget_ClientHeight - fget_BtnNext.Height - oContext.HMargin ) ) ;

  tlb.Height := Max( fget_btnOk.Height, fget_btnCancel.Height ) ;
end;


procedure T_PvlModalWizardMobile.doApplyStyleLookup;
var
  bmp    : TBitmap    ;
  obj    : TFmxObject ;
  ctrl   : TControl   ;

  function doStyleObject( const _style_name : String ) : TControl ;
  var
    o : TFmxObject ;
  begin
    o := nil ;
    {$IFNDEF LEVEL_RX10_FMX}
      if Assigned(TStyleManager.ActiveStyleForScene( oParentForm ) ) then
        o := TStyleManager.ActiveStyleForScene(
               oParentForm
             ).FindStyleResource(_style_name);
    {$ELSE}
      o:= TStyledControl.LookupStyleObject(
            Self, Self, FParentForm, _style_name, '', '', True
          );
    {$ENDIF}

    if o is TControl then
      Result := TControl( o )
    else
     Result := nil ;

    DoRedraw ;
  end;

begin
  // apply styles
  // because we use TRect not a full styled control (to have rounded cornder)
  // we clone style by painting style to bitmap then applying style
  // to TRect brush.

  exit ;

  ctrl := doStyleObject( 'backgroundstyle' ) ;
  try
    if Assigned( ctrl ) then begin
      ctrl.Width  := rct_main.Width ;
      ctrl.Height := rct_main.Height ;
      bmp := TBitmap.Create( Round(ctrl.Width) , Round(ctrl.Height) ) ;
      try
        bmp.Canvas.BeginScene();
        ctrl.PaintTo( bmp.Canvas, RectF( 0, 0, bmp.Width, bmp.Height ) ) ;
        bmp.Canvas.EndScene();
        rct_main.Fill.Bitmap.Bitmap.Assign(bmp) ;
      finally
        bmp.Free ;
      end;
      rct_main.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
      rct_main.Fill.Kind := TBrushKind.Bitmap;
    end;
  finally
    FreeObject( ctrl ) ;
  end ;

  ctrl := doStyleObject( 'labelstyle' ) ;
  try
    if Assigned( ctrl ) then begin
      obj := ctrl.FindStyleResource('text') ;
      if obj is TText then begin
        rct_main.Stroke.Color := TText(obj).TextSettings.FontColor ;
      end;
    end;
  finally
    FreeObject( ctrl ) ;
  end ;

  ctrl := doStyleObject( 'toolbarstyle' ) ;
  try
    if Assigned( ctrl ) then begin
      ctrl.Width  := tlb.Width ;
      ctrl.Height := tlb.Height ;
      bmp := TBitmap.Create( Round(ctrl.Width) , Round(ctrl.Height) ) ;
      try
        bmp.Canvas.BeginScene();
        ctrl.PaintTo( bmp.Canvas, RectF( 0, 0, bmp.Width, bmp.Height ) ) ;
        bmp.Canvas.EndScene();
        tlb.Fill.Bitmap.Bitmap.Assign(bmp) ;
      finally
        bmp.Free ;
      end;
      tlb.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
      tlb.Fill.Kind := TBrushKind.Bitmap;
    end;
  finally
    FreeObject( ctrl ) ;
  end ;
end;

procedure T_PvlModalWizardMobile.doKeyboardShown(
        _sender  : TObject;
        _visible : Boolean;
  const _bounds  : TRect
);
var
  clnt : TPointF ;
  delta : Single ;
begin
  delta := 0 ;

  clnt := oParentForm.ScreenToClient( _bounds.TopLeft )  ;

  if oParentForm.Focused is TControl then begin
    oKeyboardScrollControl :=  oParentForm.Focused as TControl ;

    if oKeyboardScrollControl.AbsoluteRect.Bottom > clnt.Y  then begin
      delta := -(clnt.Y - oKeyboardScrollControl.AbsoluteRect.Bottom  )  ;
    end;

    dKeyboardScroll := rct_back.Position.Y ;

    oKeyboardScrollAnimation.StopValue := rct_back.Position.Y - delta ;
    oKeyboardScrollAnimation.Start ;
  end ;
end ;

procedure T_PvlModalWizardMobile.doKeyboardHidden(
        _sender  : TObject;
        _visible : Boolean;
  const _bounds  : TRect
);
begin
  oKeyboardScrollTimer.Enabled := True ;
end;

procedure T_PvlModalWizardMobile.doKeyboardScrollTimer(
  _sender : TObject
) ;
begin
  // don't scroll if if combobox is opened
  if oKeyboardScrollControl is TComboEdit then
    if TComboEdit(oKeyboardScrollControl).DroppedDown then
      exit ;

  oKeyboardScrollTimer.Enabled := False ;

  oKeyboardScrollAnimation.StopValue := dKeyboardScroll ;
  oKeyboardScrollAnimation.Start ;
end ;

procedure T_PvlModalWizardMobile.doKeyDown(
        _sender  : TObject;
    var _key     : Word ;
    var _keychar : WideChar ;
        _shift   : TShiftState
) ;
var
  par : TGIS_PvlModalWizard ;//?
begin
  inherited ;

  par := oParent as TGIS_PvlModalWizard ;
  if      _key = vkEscape then begin
                                 if ( TCustomForm( Self ).ActiveControl is TComboBox ) and
                                    TComboBox(TCustomForm( Self ).ActiveControl).DroppedDown then exit ;
                                 par.BtnCancel.OnClick( Self )
                               end
  else if _key = vkReturn then begin
                                 if TCustomForm( Self ).ActiveControl is TMemo then exit ;
                                   par.BtnOk.OnClick( Self ) ;
                               end
  else if _key = vkF1     then begin
                                 par.BtnHelp.OnClick( Self ) ;
                               end ;

end;

procedure T_PvlModalWizardMobile.FreeForm ;
begin
  //DO NOTHING
end;

procedure T_PvlModalWizardMobile.Show ;
begin
  pCallBack := nil ;

  prevKeyboardShown  := oParentForm.OnVirtualKeyboardShown ;
  prevKeyboardHidden := oParentForm.OnVirtualKeyboardHidden ;

  oParentForm.OnVirtualKeyboardShown  := doKeyboardShown ;
  oParentForm.OnVirtualKeyboardHidden := doKeyboardHidden ;

  TSpeedButton( fget_btnOk.NativeControl ).OnApplyStyleLookup
   := doApplyStyleLookup ;

  doApplyStyleLookup( self ) ;

  handleOnShow( Self ) ;
  rct_back.Visible := True ;
end;

function  T_PvlModalWizardMobile.ShowModal(
  const  _proc    : TGIS_Proc ;
  const _free     : Boolean
) : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlModalResult.None ;

  pCallBack := _proc ;

  prevKeyboardShown  := oParentForm.OnVirtualKeyboardShown ;
  prevKeyboardHidden := oParentForm.OnVirtualKeyboardHidden ;

  oParentForm.OnVirtualKeyboardShown  := doKeyboardShown ;
  oParentForm.OnVirtualKeyboardHidden := doKeyboardHidden ;

  ///? _free
  TSpeedButton( fget_btnOk.NativeControl ).OnApplyStyleLookup
    := doApplyStyleLookup ;

  doApplyStyleLookup( self ) ;

  handleOnShow( Self ) ;
  rct_back.Visible := True ;
end;

function  T_PvlModalWizardMobile.ShowModal
  : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlModalResult.None ;

  pCallBack := nil ;

  prevKeyboardShown  := oParentForm.OnVirtualKeyboardShown ;
  prevKeyboardHidden := oParentForm.OnVirtualKeyboardHidden ;

  oParentForm.OnVirtualKeyboardShown  := doKeyboardShown ;
  oParentForm.OnVirtualKeyboardHidden := doKeyboardHidden ;


  TSpeedButton( fget_btnOk.NativeControl ).OnApplyStyleLookup
    := doApplyStyleLookup ;

  doApplyStyleLookup( self ) ;

  handleOnShow( Self ) ;
  rct_back.Visible := True ;
end;

procedure T_PvlModalWizardMobile.Close;
var
  o : TGIS_PvlBaseForm;
begin
  o := oParent as TGIS_PvlBaseForm ;
  if Assigned( o.OnClose ) then
    o.OnClose( self ) ;

  rct_back.Visible := False ;
end;

{$ENDREGION 'T_PvlModalWizardMobile'}

initialization
  RegisterPVLPlatformControl( 'Form', T_PvlForm ) ;

  {$IFNDEF GIS_MOBILE_DIALOGS}
    RegisterPVLPlatformControl( 'ModalForm', T_PvlModalFormDesktop ) ;
    RegisterPVLPlatformControl( 'ModalWizard', T_PvlModalWizardDesktop ) ;
  {$ELSE}
    RegisterPVLPlatformControl( 'ModalForm', T_PvlModalFormMobile ) ;
    RegisterPVLPlatformControl( 'ModalWizard', T_PvlModalWizardMobile ) ;
  {$ENDIF}

//==================================== END =====================================
end.

