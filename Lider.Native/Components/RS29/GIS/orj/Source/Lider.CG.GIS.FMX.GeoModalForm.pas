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
  Modal form helper for mobile devices.
}

unit FMX.GisModalForm ;
{$HPPEMIT '#pragma link "FMX.GisModalForm"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,

  FMX.Objects,
  FMX.Types,
  FMX.BehaviorManager,
  FMX.Platform,
  FMX.Controls,
  FMX.ListBox,
  FMX.StdCtrls,
  FMX.Graphics,
  FMX.Layouts,
  {$IFNDEF GIS_MOBILES}
    FMX.Ani,
  {$ENDIF}
  {$IFNDEF LEVEL_RX10_FMX}
    FMX.Styles,
  {$ENDIF}
  FMX.Forms,

  GisTypes,
  GisTypesUI;

type
  {$IFDEF GIS_MOBILE_DIALOGS}
    /// <summary>
    ///   Emulation class which displays modal forms on a mobile devices
    ///   as a kind of dialog boxes rather then full screen forms.
    /// </summary>
    TGIS_ModalForm = class( TFrame )
      private // TForm like properties
        FBorderIcons  : TBorderIcons ;
        FBorderStyle  : TFmxFormBorderStyle ;
        FCaption      : String ;
        FClientHeight : Integer ;
        FClientWidth  : Integer ;
        FFormFactor   : TFormFactor ;
        FDesignerMasterStyle : Integer ;
        FPosition     : TFormPosition ;
        FModalResult  : TModalResult ;

        FOnCreate    : TNotifyEvent ;
        FOnDestroy   : TNotifyEvent ;
        FOnShow      : TNotifyEvent ;

        iMaxWidth    : Integer ;
        iMaxHeight   : Integer ;

        iPPI         : Integer ;
        iSystemPPI   : Integer ;

      protected
        oMainForm : TFrame ;

      private
        procedure fset_ClientHeight  ( const _value : Integer  ) ;
        procedure fset_ClientWidth   ( const _value : Integer  ) ;
        function  fget_ActiveControl : TControl ;
        procedure fset_ActiveControl ( const _value : TControl );
        function  fget_ModalResult   : TModalResult ;
        procedure fset_ModalResult   ( const _value : TModalResult );

      protected
        /// <summary>
        ///   Standard OK button.
        /// </summary>
        btnOk        : TSpeedButton ;

        /// <summary>
        ///   Standard Cancel button.
        /// </summary>
        btnCancel    : TSpeedButton ;

        /// <summary>
        ///   Standard Help button.
        /// </summary>
        btnHelp      : TSpeedButton ;

        /// <summary>
        ///   Help event.
        /// </summary>
        pOnHelp      : TGIS_HelpEvent ;

      protected

        /// <summary>
        ///   Override this method upon to provide custom caption ad sizes.
        /// </summary>
        procedure initForm     ; virtual;

        /// <summary>
        ///   Override this method to add controls to this form.
        /// </summary>
        procedure initControls ; virtual;

        /// <summary>
        ///   Override this method to provide any special functionality upon
        ///   form show.
        /// </summary>
        procedure showForm      ; virtual;

      protected
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

      published // TForm like properties
        /// <summary>
        ///   TForm emulattion property.
        /// </summary>
        property BorderIcons         : TBorderIcons
                                       read  FBorderIcons
                                       write FBorderIcons ;
        /// <summary>
        ///   TForm emulattion property.
        /// </summary>
        property BorderStyle         : TFmxFormBorderStyle
                                       read  FBorderStyle
                                       write FBorderStyle ;
        /// <summary>
        ///   TForm emulattion property.
        /// </summary>
        property Caption             : String
                                       read  FCaption
                                       write FCaption ;
        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property ClientHeight        : Integer
                                       read  FClientHeight
                                       write fset_ClientHeight ;
        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property ClientWidth         : Integer
                                       read  FClientWidth
                                       write fset_ClientWidth ;
        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property DesignerMasterStyle : Integer
                                       read  FDesignerMasterStyle
                                       write FDesignerMasterStyle ;
        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property Position            : TFormPosition
                                       read  FPosition
                                       write FPosition ;
        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property FormFactor          : TFormFactor
                                       read  FFormFactor
                                       write FFormFactor ;

        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property OnCreate            : TNotifyEvent
                                       read  FOnCreate
                                       write FOnCreate ;
        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property OnDestroy           : TNotifyEvent
                                       read  FOnDestroy
                                       write FOnDestroy ;
        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property OnShow              : TNotifyEvent
                                       read  FOnShow
                                       write FOnShow ;

        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property ModalResult         : TModalResult
                                       read  fget_ModalResult
                                       write fset_ModalResult ;

        /// <summary>
        ///   TForm emulation property.
        /// </summary>
        property ActiveControl       : TControl
                                       read  fget_ActiveControl
                                       write fset_ActiveControl ;

      private
        rct_back     : TRectangle ;
        rct_main     : TRectangle ;
        tlb          : TRectangle ;

        dKeyboardScroll          : Single ;
        oKeyboardScrollControl   : TControl ;
        oKeyboardScrollAnimation : TFloatAnimation ;
        oKeyboardScrollTimer     : TTimer ;
        prevKeyboardShown        : procedure(       _sender  : TObject;
                                                    _visible : Boolean;
                                              const _bounds  : TRect
                                            ) of object ;
        prevKeyboardHidden       : procedure(       _sender  : TObject;
                                                    _visible : Boolean;
                                              const _bounds  : TRect
                                            ) of object ;

      private
        FParentForm : TCustomForm ;
        FShowOK     : Boolean ;
        FShowCancel : Boolean ;
        FShowHelp   : Boolean ;
        FResultProc : TProc<TModalResult> ;

      private
        procedure fset_ShowCancel    ( const _value : Boolean  );
        procedure fset_ShowOK        ( const _value : Boolean  );

        procedure doApplyStyleLookup (       _sender  : TObject
                                     ) ;
        procedure doShow1 ;
        procedure doHide ;

        procedure doKeyboardShown    (       _sender  : TObject;
                                             _visible : Boolean;
                                       const _bounds  : TRect
                                     ) ;
        procedure doKeyboardHidden   (       _sender  : TObject;
                                             _visible : Boolean;
                                       const _bounds  : TRect
                                     ) ;
        procedure doKeyboardScrollTimer(
                                             _sender  : TObject
                                       ) ;
      protected
        BiDiMode     : TBiDiMode ;

      protected
        /// <summary>
        ///   See documentation for TCustomForm in Delphi help.
        /// </summary>
        /// <param name="_key">
        ///   See documentation for TCustomForm in Delphi help.
        /// </param>
        /// <param name="_keyChar">
        ///   See documentation for TCustomForm in Delphi help.
        /// </param>
        /// <param name="_shift">
        ///   See documentation for TCustomForm in Delphi help.
        /// </param>
        procedure KeyDown          ( var _key     : Word ;
                                     var _keyChar : Char ;
                                         _shift   : TShiftState
                                   ) ; override;

      public
        /// <summary>
        ///   Change tool bar OK button active/inactive (default is active).
        /// </summary>
        /// <remarks>
        ///   Has no meaning on desktop devices. <br />
        /// </remarks>
        property ShowOK     : Boolean  read FShowOK write fset_ShowOK ;

        /// <summary>
        ///   Change tool bar Cancel button/inactive (default is active).
        /// </summary>
        /// <remarks>
        ///   Has no meaning on desktop devices. <br />
        /// </remarks>
        property ShowCancel : Boolean  read FShowOK write fset_ShowCancel ;


        /// <summary>
        ///  Form PPI.
        /// </summary>
        property PPI : Integer read iPPI ;

        /// <summary>
        ///  Form System PPI (unscaled).
        /// </summary>
        property SystemPPI : Integer read iSystemPPI ;

      public

        /// <summary>
        ///   Standard constructor.
        /// </summary>
        /// <param name="_owner">
        ///   dialog box owner
        /// </param>
        {#ownership:_owner:ownif_empty}
        constructor Create           ( _owner : TComponent
                                     ) ; overload; override;

        /// <summary>
        ///   Standard constructor.
        /// </summary>
        /// <param name="_owner">
        ///   dialog box owner
        /// </param>
        /// <param name="_sizeable">
        ///   If true form is create as sizeable; otherwise is created with
        ///   fixed frame, dialog box style.
        /// </param>
        {#ownership:_owner:ownif_empty}
        constructor Create           ( _owner    : TComponent ;
                                       _sizeable : Boolean
                                     ) ; overload; virtual;

        /// <summary>
        ///   TForm emulation constructor for forms w/o FMX files.
        /// </summary>
        /// <param name="_owner">
        ///   dialog box owner
        /// </param>
        /// <param name="_dummy">
        ///   unused
        /// </param>
        {#ownership:_owner:ownif_empty}
        constructor CreateNew        ( _owner : TComponent  ;
                                       _dummy : NativeInt
                                     ) ; virtual;

        /// <summary>
        ///   Standard destructor.
        /// </summary>
        destructor  Destroy          ; override;

        /// <summary>
        ///   Show modal like method which emulates modal behavior on mobile
        ///   devices.
        /// </summary>
        /// <param name="_proc">
        ///   a call back method to be executed upon form closing.
        /// </param>
        /// <remarks>
        ///   <para>
        ///     On mobile platforms it displays a dialog box like form and
        ///     blocks interaction with a background form to provide modal like
        ///     behavior even on platform (like Android) which has no such
        ///     concept.
        ///   </para>
        ///   <para>
        ///     On a desktop platform it acts as a simple modal dialog box
        ///     always centered on a top of a parent form.
        ///   </para>
        /// </remarks>
        procedure   ShowModalEx      ( const _proc : TProc<TModalResult>
                                     ); overload;

      protected

        /// <summary>
        ///   Standard AfterSctructor handler.
        /// </summary>
        procedure AfterConstruction ; override;

    end;

    const
      // To discover that is a popup window. Do not change this name.
      MODALFORM_POPUP = 'T_popupModalForm' ;

  {$ELSE}

    /// <summary>
    ///   Emulation class which displays modal forms on a mobile devices
    ///   as a kind of dialog boxes rather then full screen forms.
    /// </summary>
    TGIS_ModalForm = class( TForm )
      private
        FParentForm   : TCustomForm ;
        iMaxWidth     : Integer ;
        iMaxHeight    : Integer ;
        dScreenScale  : Single  ;
        iPPI          : Integer ;
        iSystemPPI    : Integer ;
      protected
        /// <summary>
        ///   Parent object to place component on a form.
        /// </summary>
        oMainForm : TLayout ;

        /// <summary>
        ///   Standard OK button.
        /// </summary>
        btnOk        : TButton ;

        /// <summary>
        ///   Standard Cancel button.
        /// </summary>
        btnCancel    : TButton ;

        /// <summary>
        ///   Standard Help button.
        /// </summary>
        btnHelp      : TButton ;

        /// <summary>
        ///   Help event.
        /// </summary>
        pOnHelp      : TGIS_HelpEvent ;


      private
        function  fget_ClientHeight  : Integer;
        function  fget_ClientWidth   : Integer;
        procedure fset_ClientHeight  ( const _value : Integer ) ;
        procedure fset_ClientWidth   ( const _value : Integer ) ;

      protected

        /// <summary>
        ///   Override this method upon to provide custom caption ad sizes.
        /// </summary>
        procedure initForm     ; virtual;

        /// <summary>
        ///   Override this method to add controls to this form.
        /// </summary>
        procedure initControls ; virtual;

        /// <summary>
        ///   Override this method to provide any special functionality upon
        ///   form show.
        /// </summary>
        procedure showForm     ; virtual;

        /// <summary>
        ///   Create standard form buttons.
        /// </summary>
        procedure initButtons ;

      protected
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
      protected
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
        /// <param name="_keyChar">
        ///   See documentation for TCustomForm in Delphi help.
        /// </param>
        /// <param name="_shift">
        ///   See documentation for TCustomForm in Delphi help.
        /// </param>
        procedure KeyDown          ( var _key     : Word ;
                                     var _keyChar : Char ;
                                         _shift   : TShiftState
                                   ) ; override;

      public
        /// <summary>
        ///   Standard constructor.
        /// </summary>
        /// <param name="_owner">
        ///   dialog box owner
        /// </param>
        {#ownership:_owner:ownif_empty}
        constructor Create           ( _owner : TComponent
                                     ) ; overload; override;

        /// <summary>
        ///   Standard constructor.
        /// </summary>
        /// <param name="_owner">
        ///   dialog box owner
        /// </param>
        /// <param name="_sizeable">
        ///   If true form is create as sizeable; otherwise is created with
        ///   fixed frame, dialog box style.
        /// </param>
        {#ownership:_owner:ownif_empty}
        constructor Create           ( _owner    : TComponent ;
                                       _sizeable : Boolean
                                     ) ; overload; virtual;

        /// <summary>
        ///   Show modal like method which emulates modal behavior on mobile
        ///   devices.
        /// </summary>
        /// <param name="_proc">
        ///   a call back method to be executed upon form closing
        /// </param>
        /// <param name="_free">
        ///   True, if instance of the form must be destructed upn return
        /// </param>
        /// <remarks>
        ///   <para>
        ///     On mobile platforms it displays a dialog box like form and
        ///     blocks interaction with a background form to provide modal like
        ///     behavior even on platform (like Android) which has no such
        ///     concept.
        ///   </para>
        ///   <para>
        ///     On a desktop platform it acts as a simple modal dialog box
        ///     always centered on a top of a parent form.
        ///   </para>
        /// </remarks>
        procedure   ShowModalEx      ( const _proc : TProc<TModalResult> ;
                                       const _free : Boolean = True
                                     ); overload;

      public
        /// <summary>
        ///   See documentation for TCustomForm in Delphi help.
        /// </summary>
        property ClientHeight        : Integer
                                       read  fget_ClientHeight
                                       write fset_ClientHeight ;
        /// <summary>
        ///   See documentation for TCustomForm in Delphi help.
        /// </summary>
        property ClientWidth         : Integer
                                       read  fget_ClientWidth
                                       write fset_ClientWidth ;


        /// <summary>
        ///  Form PPI.
        /// </summary>
        property PPI : Integer read iPPI ;

        /// <summary>
        ///  Form System PPI (unscaled).
        /// </summary>
        property SystemPPI : Integer read iSystemPPI ;
    end;

    {$ENDIF}


//##############################################################################
implementation

uses
  System.Math,

  {$IFDEF MSWINDOWS}
    {$IFDEF LEVEL_RX104B_FMX}
      FMX.Platform.Win,
    {$ENDIF}
  {$ENDIF}

  FMX.Memo,
  FMX.ComboEdit,

  GisRtl,
  GisResource ;

const
  FORM_MARGIN = 5 ;


function parent_form(
  _owner : TComponent
) :  TCustomForm ;
var
  tmp : TFmxObject ;
begin
  tmp := TFmxObject( _owner ) ;
  while Assigned( tmp ) do begin
    if tmp is TCustomForm then
      break ;
    tmp := tmp.Parent ;
  end;
  Assert( ( not Assigned( tmp ) ) or (tmp is TCustomForm) ) ;
  Result := TCustomForm( tmp ) ;
end;

function rect2rect(
  const _rect : TRectF
) : TRect ; overload ;
begin
  Result := Rect( RoundS( _rect.Left ),
                  RoundS( _rect.Top  ),
                  RoundS( _rect.Left + _rect.Width  ),
                  RoundS( _rect.Top  + _rect.Height )
                ) ;
end;

function rect2rect(
  const _rect : TRect
) : TRect ; overload ;
begin
  Result := _rect ;
end;

{$IFDEF GIS_MOBILE_DIALOGS}

  constructor TGIS_ModalForm.Create(
    _owner: TComponent
  ) ;
  begin
    Create( _owner, False ) ;
  end;

  constructor TGIS_ModalForm.Create(
    _owner    : TComponent ;
    _sizeable : Boolean
  ) ;
  begin
    CreateNew( _owner, 0 ) ;

    pOnHelp := nil ;
    initForm ;
    initControls ;
  end;

  constructor TGIS_ModalForm.CreateNew(
    _owner : TComponent  ;
    _dummy : NativeInt
  ) ;
  var
    oscreensvc   : IFMXScreenService;
    obehaviorsvc : IDeviceBehavior;
    met          : TDeviceDisplayMetrics;
    owidowscvs   : IFMXWindowService;

    lbl : TLabel ; // stnage; seems that having such dummy filed resolve
                   // issue of aliging first label on the form
    btn : TSpeedButton ;
    h   : Integer      ;

  begin
    FParentForm := parent_form( _owner ) ;

    btnCancel := nil ;
    btnOk     := nil ;
    btnHelp   := nil ;

    FShowCancel := True ;
    FShowOk     := True ;

    FModalResult := 0 ;
    FFormFactor := TFormFactor.Create ;

    SetDesignInstance(True); // a trick to use FMX less forms
    inherited Create( _owner ) ;
    SetDesignInstance(False);

    Parent := TFmxObject( _owner ) ;

    lbl := TLabel.Create( Parent ) ;
    lbl.Text := '' ;
    lbl.Parent := Parent ;

    oMainForm := Self ;

    if Assigned( OnCreate ) then
      OnCreate( self ) ;

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
         obehaviorsvc, Self
       )
    then begin
      met := obehaviorsvc.GetDisplayMetrics(Self) ;
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
      iPPI := RoundS( owidowscvs.GetWindowScale( FParentForm ) * iSystemPPI ) ;
    end ;

    oKeyboardScrollTimer := TTimer.Create( self );
    oKeyboardScrollTimer.Enabled := False ;
    oKeyboardScrollTimer.Interval := 100 ;
    oKeyboardScrollTimer.OnTimer := doKeyboardScrollTimer ;

    iMaxWidth  := RoundS( FParentForm.ClientWidth  * 0.95 ) ;
    iMaxHeight := RoundS( FParentForm.ClientHeight * 0.95 ) ;

    btn := TSpeedButton.Create( self );
    btn.StyleLookup := 'donetoolbutton' ;
    h := RoundS( btn.Height ) ;
    self.Controls.Remove( btn ) ;

    iMaxWidth  := iMaxWidth  -  2 * FORM_MARGIN ;
    iMaxHeight := iMaxHeight -  2 * FORM_MARGIN - h ;

  end;

  destructor TGIS_ModalForm.Destroy ;
  begin
    FreeObject( FFormFactor ) ;

    if Assigned( OnDestroy ) then
      OnDestroy( self ) ;

    if ClassName <> MODALFORM_POPUP then begin
      FParentForm.OnVirtualKeyboardShown  := prevKeyboardShown  ;
      FParentForm.OnVirtualKeyboardHidden := prevKeyboardHidden ;
    end ;

    inherited ;
  end;

  procedure TGIS_ModalForm.AfterConstruction;
  begin
    inherited ;

    if Assigned( OnCreate ) then
      OnCreate( self ) ;
  end ;

  procedure TGIS_ModalForm.doApplyStyleLookup;
  var
    bmp    : TBitmap    ;
    obj    : TFmxObject ;
    ctrl   : TControl   ;

    function doStyleObject( const _style_name : String ) : TControl ;
    var
      o : TFmxObject ;
    begin
      {$IFNDEF LEVEL_RX10_FMX}
        if Assigned(TStyleManager.ActiveStyleForScene(FParentForm)) then
          o := TStyleManager.ActiveStyleForScene(
                 FParentForm
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
    end;

  begin
    // apply styles
    // because we use TRect not a full styled control (to have rounded cornder)
    // we clone style by painting style to bitmap then applying style
    // to TRect brush.

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

  procedure TGIS_ModalForm.doShow1 ;
  var
    lbl  : TLabel ;
    w, h : Single ;

    bmp  : TBitmap ;
    ctrl : TControl ;

  begin
    rct_back := TRectangle.Create( FParentForm ) ;
    rct_back.Parent := TFmxObject( FParentForm ) ;
    rct_back.Opacity := 1.0 ;
    rct_back.Stroke.Thickness := 0 ;
    rct_back.Fill.Color := TAlphaColorRec.Null ;
    rct_back.Align := TAlignLayout.Contents ;

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
    tlb.Stroke.Color := TAlphaColorRec.Null ;
    tlb.Corners := [TCorner.TopLeft, TCorner.TopRight];
    tlb.Align := TAlignLayout.Top ;
    tlb.Padding.Left  := FORM_MARGIN ;
    tlb.Padding.Right := FORM_MARGIN ;

    btnHelp := TSpeedButton.Create( tlb );
    btnHelp.Parent := TFmxObject( tlb ) ;
    btnHelp.Align := TAlignLayout.Left ;
    {$IFDEF ANDROID}
      btnHelp.StyleLookup := 'backtoolbutton' ;
    {$ELSE}
      btnHelp.StyleLookup := 'toolbutton' ;
    {$ENDIF}
    btnHelp.Text := _rsrcna( GIS_RS_BTN_HELP ) ;
    btnHelp.Width := 73 ;
    btnHelp.OnClick := btnHelpClick ;
    btnHelp.Enabled := False ;
    btnHelp.Visible := False ;

    btnCancel := TSpeedButton.Create( tlb );
    btnCancel.Parent := TFmxObject( tlb ) ;
    btnCancel.Align := TAlignLayout.Left ;
    {$IFDEF ANDROID}
      btnCancel.StyleLookup := 'backtoolbutton' ;
    {$ELSE}
      btnCancel.StyleLookup := 'toolbutton' ;
    {$ENDIF}
    btnCancel.Text := _rsrcna( GIS_RS_BTN_CANCEL ) ;
    btnCancel.Width := 73 ;
    btnCancel.OnClick := btnCancelClick ;
    btnCancel.Enabled := FShowCancel ;

    btnOk := TSpeedButton.Create( tlb );
    btnOk.Parent := TFmxObject( tlb ) ;
    btnOk.Align := TAlignLayout.Right ;
    btnOk.StyleLookup := 'donetoolbutton' ;
    {$IFDEF GIS_MOBILE}
      btnOk.Text := _rsrcna( GIS_RS_BTN_DONE ) ;
    {$ELSE}
      btnOk.Text := _rsrcna( GIS_RS_BTN_OK ) ;
    {$ENDIF}
    btnOk.Width := 73 ;
    btnOk.OnClick := btnOKClick ;
    btnOk.Enabled := FShowOk ;

    lbl := TLabel.Create( tlb ) ;
    lbl.Parent := tlb ;
    lbl.Align := TAlignLayout.Client ;
    lbl.TextAlign := TTextAlign.Center ;
    lbl.TextSettings.WordWrap := False ;
    lbl.TextSettings.Trimming := TTextTrimming.Character ;
    lbl.Text := Caption ;
    lbl.StyleLookup := 'toollabel' ;

    lbl.Height := Max( btnOK.Height, btnCancel.Height ) ;
    tlb.Height := lbl.Height ;

    if self.ClientWidth <> 0 then begin
      w := Round( self.ClientWidth  ) ;
      h := Round( self.ClientHeight ) ;
    end
    else begin
      w  := Round( self.Width  ) ;
      h := Round( self.Height ) ;
    end;

    self.Width  := w ;
    self.Height := h ;
    self.ClientWidth  := RoundS( w ) ;
    self.ClientHeight := RoundS( h ) ;

    rct_main.Width  := RoundS( FORM_MARGIN + w + FORM_MARGIN ) ;
    rct_main.Height := RoundS( tlb.Height + FORM_MARGIN + h + FORM_MARGIN ) ;

    self.Parent := rct_main ;
    self.Align  :=  TAlignLayout.Center ;

    if ClassName <> MODALFORM_POPUP then begin
      prevKeyboardShown  := FParentForm.OnVirtualKeyboardShown ;
      prevKeyboardHidden := FParentForm.OnVirtualKeyboardHidden ;

      FParentForm.OnVirtualKeyboardShown := doKeyboardShown ;
      FParentForm.onVirtualKeyboardHidden := doKeyboardHidden ;
    end ;

    FModalResult := mrNone ;

    btnOK.OnApplyStyleLookup := doApplyStyleLookup ;
    doApplyStyleLookup( self ) ;

    showForm ;
  end;

  procedure TGIS_ModalForm.ShowModalEx(
    const _proc: TProc<TModalResult>
  ) ;
  begin
    FResultProc := _proc ;
    if Assigned( OnShow ) then
      OnShow( self ) ;
    doShow1 ;
    Application.ProcessMessages ;
  end;

  function TGIS_ModalForm.fget_ActiveControl
    : TControl ;
  begin
    Result := TCustomForm( Owner ).ActiveControl ;
  end;

  function TGIS_ModalForm.fget_ModalResult
    : TModalResult ;
  begin
    Result := FModalResult ;
  end;

  procedure TGIS_ModalForm.fset_ActiveControl(
    const _value : TControl
  );
  begin
    TCustomForm( FParentForm ).ActiveControl := _value ;
  end;

  procedure TGIS_ModalForm.fset_ClientHeight(
    const _value : Integer
  );
  begin
    FClientHeight := Min( iMaxHeight, _value ) ;
    Height := FClientHeight ;
  end;

  procedure TGIS_ModalForm.fset_ClientWidth(
    const _value : Integer
  );
  begin
    FClientWidth := Min( iMaxWidth, _value ) ;
    Width := FClientWidth ;
  end;

  procedure TGIS_ModalForm.fset_ModalResult(
    const _value : TModalResult
  );
  begin
    if FModalResult <> mrNone then exit ;

    FModalResult := _value ;
    root.SetCaptured(nil); // restore mouse context on main form

    if Assigned( FResultProc ) then begin
      doHide ;
      FResultProc( FModalResult ) ;
    end;

    {$IFDEF NEXTGEN}
      DisposeOf ;
    {$ELSE}
      Free ;
    {$ENDIF}
  end;

  procedure TGIS_ModalForm.doHide ;
  begin
    TCustomForm( FParentForm ).RemoveObject( rct_back);
  end;

  procedure TGIS_ModalForm.doKeyboardShown(
          _sender  : TObject;
          _visible : Boolean;
    const _bounds  : TRect
  );
  var
    clnt : TPointF ;
    delta : Single ;
  begin
    delta := 0 ;

    clnt := FParentForm.ScreenToClient( _bounds.TopLeft )  ;

    if FParentForm.Focused is TControl then begin
      oKeyboardScrollControl :=  FParentForm.Focused as TControl ;

      if oKeyboardScrollControl.AbsoluteRect.Bottom > clnt.Y  then begin
        delta := -(clnt.Y - oKeyboardScrollControl.AbsoluteRect.Bottom  )  ;
      end;

      dKeyboardScroll := rct_back.Position.Y ;

      oKeyboardScrollAnimation.StopValue := rct_back.Position.Y - delta ;
      oKeyboardScrollAnimation.Start ;
    end ;
  end ;

  procedure TGIS_ModalForm.doKeyboardScrollTimer(
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

  procedure TGIS_ModalForm.fset_ShowCancel(
    const _value : Boolean
  );
  begin
    FShowCancel       := _value ;
    if Assigned( btnCancel ) then
      btnCancel.Enabled := _value ;
  end;

  procedure TGIS_ModalForm.fset_ShowOK(
    const _value : Boolean
  );
  begin
    FShowOk := _value ;
    if Assigned( btnOk ) then
      btnOk.Enabled := _value ;
  end;

  procedure TGIS_ModalForm.btnOKClick(
    _sender : TObject
  );
  begin
    ModalResult := mrOK ;
  end ;

  procedure TGIS_ModalForm.btnCancelClick(
    _sender : TObject
  );
  begin
    ModalResult := mrCancel ;
  end ;

  procedure TGIS_ModalForm.KeyDown(
    var _key     : Word ;
    var _keyChar : Char ;
        _shift   : TShiftState
  ) ;
  begin
    inherited ;
    if      _key = vkEscape then begin
                                   btnCancelClick( Self )
                                 end
    else if _key = vkReturn then begin
                                   btnOKClick( Self ) ;
                                 end ;
  end ;

  procedure TGIS_ModalForm.doKeyboardHidden(
          _sender  : TObject;
          _visible : Boolean;
    const _bounds  : TRect
  );
  begin
    oKeyboardScrollTimer.Enabled := True ;
  end;

{$ELSE}

  constructor TGIS_ModalForm.Create(
    _owner : TComponent
  ) ;
  begin
    Create( _owner, False ) ;
  end ;

  constructor TGIS_ModalForm.Create(
    _owner    : TComponent ;
    _sizeable : Boolean
  ) ;
  var
    oscreensvc   : IFMXScreenService;
    obehaviorsvc : IDeviceBehavior;
    met          : TDeviceDisplayMetrics;
    owidowscvs   : IFMXWindowService;
  begin
    FParentForm := parent_form( _owner ) ;
    inherited CreateNew( _owner, 0 ) ;

    if _rsbidi then
      Self.BiDiMode := TBiDiMode.bdRightToLeft
    else
      Self.BiDiMode := TBiDiMode.bdLeftToRight ;
    Self.Left := 0 ;
    Self.Top := 0 ;
    Self.BorderIcons := [TBorderIcon.biSystemMenu] ;
    inherited Position := TFormPosition.OwnerFormCenter ;
    if _sizeable then
      Self.BorderStyle := TFmxFormBorderStyle.Sizeable
    else
      Self.BorderStyle := TFmxFormBorderStyle.ToolWindow ;

    pOnHelp := nil ;

    if TPlatformServices.Current.SupportsPlatformService(
        IFMXScreenService,
        IInterface(oscreensvc)
       )
    then begin
      {$IFDEF MACOS}
        dScreenScale := 1 ;
      {$ELSE}
        dScreenScale := oscreensvc.GetScreenScale ;
        dScreenScale := 1 ;
      {$ENDIF}
      iMaxWidth  := RoundS( oscreensvc.GetScreenSize.X / dScreenScale * 0.95 ) ;
      iMaxHeight := RoundS( oscreensvc.GetScreenSize.Y / dScreenScale * 0.95 ) ;
    end
    else begin
      iMaxWidth    := RoundS( Screen.Width  ) ;
      iMaxHeight   := RoundS( Screen.Height ) ;
      dScreenScale := 1 ;
    end;

    if TBehaviorServices.Current.SupportsBehaviorService(
         IDeviceBehavior,
         obehaviorsvc, Self
       )
    then begin
      met := obehaviorsvc.GetDisplayMetrics(Self) ;
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
      iPPI := RoundS( owidowscvs.GetWindowScale( Self ) * iSystemPPI ) ;
    end ;

    ClientWidth  := 10 ;
    ClientHeight := 10 ;

    iMaxWidth  := iMaxWidth  - ( Width  - ClientWidth  ) ;
    iMaxHeight := iMaxHeight - ( Height - ClientHeight ) ;

    oMainForm := TLayout.Create( Self ) ;
    oMainForm.Parent := Self ;

    initForm ;

    oMainForm.Align := TAlignLayout.None ;
    oMainForm.Width := ClientWidth ;
    oMainForm.Height := ClientHeight ;

    initButtons ;
    initControls ;

    oMainForm.Align := TAlignLayout.Client ;

    oMainForm.Scale.X := GUIScale ;
    oMainForm.Scale.Y := GUIScale ;

    ClientWidth  := RoundS( ClientWidth  * GUIScale ) ;
    ClientHeight := RoundS( ClientHeight * GUIScale ) ;
  end ;

  function TGIS_ModalForm.fget_ClientHeight
    : Integer;
  begin
    Result := inherited ClientHeight ;
  end;

  procedure TGIS_ModalForm.fset_ClientHeight(
    const _value : Integer
  ) ;
  begin
    inherited ClientHeight := Min( iMaxHeight, _value ) ;
  end;

  function TGIS_ModalForm.fget_ClientWidth
    : Integer;
  begin
    Result := inherited ClientWidth ;
  end;

  procedure TGIS_ModalForm.fset_ClientWidth(
    const _value : Integer
  ) ;
  begin
    inherited ClientWidth := Min( iMaxWidth, _value ) ;
  end;

  procedure TGIS_ModalForm.initButtons ;
  begin
    btnHelp := TButton.Create( oMainForm ) ;
    btnHelp.Parent := oMainForm ;
    btnHelp.Size.Height := 24 ;
    btnHelp.Size.PlatformDefault := False ;
    btnHelp.Text := _rsrcna( GIS_RS_BTN_HELP ) ;
    btnHelp.OnClick := btnHelpClick ;

    btnCancel := TButton.Create( oMainForm ) ;
    btnCancel.Parent := oMainForm ;
    btnCancel.Size.Height := 24 ;
    btnCancel.Text := _rsrcna( GIS_RS_BTN_CANCEL ) ;
    btnCancel.Size.PlatformDefault := False ;
    btnCancel.OnClick := btnCancelClick ;

    btnOK := TButton.Create( oMainForm ) ;
    btnOK.Parent := oMainForm ;
    btnOK.Default := True ;
    btnOK.Size.Height := 24 ;
    btnOK.Size.PlatformDefault := False ;
    btnOK.Text := _rsrcna( GIS_RS_BTN_OK ) ;
    btnOK.OnClick := btnOKClick ;
  end ;

  procedure TGIS_ModalForm.ShowModalEx(
    const _proc: TProc<TModalResult>;
    const _free : Boolean
  );

    procedure center_form(
      const _baseform : TCustomForm ;
      const _currform : TCustomForm
    ) ;
    var
      bnd      : TRect ;
      newbnd   : TRect ;
      refarea  : TRect ;
      workarea : TRect ;
      l, t     : Integer ;
      bmod     : Boolean ;
    begin
      {$IFDEF MSWINDOWS}
        {$IFDEF LEVEL_RX104B_FMX}
          // force proper bounds
          bnd := TWinWindowHandle(_currform.Handle).WndBounds;
          bnd := Rect( RoundS( bnd.Left   / _currform.Canvas.Scale ),
                       RoundS( bnd.Top    / _currform.Canvas.Scale ),
                       RoundS( bnd.Right  / _currform.Canvas.Scale ),
                       RoundS( bnd.Bottom / _currform.Canvas.Scale )
                     ) ;
          TWinWindowHandle(_currform.Handle).Bounds := bnd ;
        {$ENDIF}
      {$ENDIF}

      bnd := Rect( _currform.Left,
                   _currform.Top,
                   _currform.Left + _currform.Width,
                   _currform.Top  + _currform.Height
                 ) ;

      if Assigned(_baseform) then begin
        refarea := Rect( _baseform.Left,
                         _baseform.Top,
                         _baseform.Left + _baseform.Width,
                         _baseform.Top  + _baseform.Height
                       ) ;
      end
      else begin
        refarea := rect2rect( Screen.DisplayFromForm(_currform).WorkArea );
      end;

      l := refarea.Left + ( refarea.Width  - bnd.Width  ) div 2 ;
      t := refarea.Top  + ( refarea.Height - bnd.Height ) div 2 ;

      newbnd := Rect(l, t, l + bnd.Width, t + bnd.Height);

      _currform.Left := newbnd.Left ;
      _currform.Top  := newbnd.Top ;

      if Assigned(_baseform) then begin
        workarea := rect2rect( Screen.DisplayFromForm(_baseform).WorkareaRect ) ;
      end
      else begin
        workarea := rect2rect( Screen.DisplayFromForm(_currform).WorkareaRect ) ;
      end;


      bmod := False;

      l := newbnd.Left;
      t := newbnd.Top;

      if newbnd.Left + newbnd.Width > workarea.Right then begin
        l := workarea.Right - newbnd.Width;
        bmod := True;
      end;
      if newbnd.Top + newbnd.Height > workarea.Bottom then begin
        t := workarea.Bottom - newbnd.Height;
        bmod := True;
      end;
      if newbnd.Left < workarea.Left then begin
        l := workarea.Left;
        bmod := True;
      end;
      if newbnd.Top < workarea.Top then begin
        t := workarea.Top;
        bmod := True;
      end;

      if bmod then begin
        newbnd := Rect(l, t, l + bnd.Width, t + bnd.Height);

        _currform.Left := newbnd.Left ;
        _currform.Top  := newbnd.Top ;
      end;
    end;

  begin
    case Position of
      TFormPosition.MainFormCenter:
        begin
          if Application.MainForm is TCustomForm then
            center_form(TCustomForm(Application.MainForm), Self)
          else
            center_form(nil, Self)
        end;
      TFormPosition.OwnerFormCenter:
        begin
          center_form(FParentForm, Self);
        end;
    else
      begin
        center_form(nil, Self);
      end
    end;
    Position := TFormPosition.MainFormCenter ; // avoid any FMX intermnal
                                               // repostions

    _proc(ShowModal);

    if _free then
      Free;
  end;

  procedure TGIS_ModalForm.btnOKClick(
    _sender : TObject
  ) ;
  begin
    ModalResult := mrOK ;
  end;

  procedure TGIS_ModalForm.btnCancelClick(
    _sender : TObject
  ) ;
  begin
    ModalResult := mrCancel ;
  end;

  procedure TGIS_ModalForm.DoShow ;
  begin
    inherited ;

    showForm ;
  end;

  procedure TGIS_ModalForm.KeyDown(
    var _key     : Word ;
    var _keyChar : Char ;
        _shift   : TShiftState
  ) ;
  var
    cbEsc : Boolean ;
  begin
    if ( _key = vkEscape ) and
       ( Focused is FMX.ComboEdit.TComboEdit ) and
       FMX.ComboEdit.TComboEdit(Focused).DroppedDown then
      cbEsc := True
    else
      cbEsc := False ;

    inherited ;
    if      _key = vkEscape then begin
                                   if cbEsc then exit ;
                                   btnCancelClick( Self )
                                 end
    else if _key = vkReturn then begin
                                   if Focused is TMemo then exit ;
                                   btnOKClick( Self ) ;
                                 end
    else if _key = vkF1     then btnHelpClick( Self ) ;
  end ;

{$ENDIF}

  procedure TGIS_ModalForm.initForm ;
  begin
    // do nothing
  end ;

  procedure TGIS_ModalForm.initControls ;
  begin
    // do nothing
  end ;

  procedure TGIS_ModalForm.showForm ;
  begin
    // do nothing
  end ;

  procedure TGIS_ModalForm.btnHelpClick(
    _sender : TObject
  ) ;
  begin
    if Assigned( pOnHelp ) then pOnHelp( Self, Name ) ;
  end;

{==================================== END =====================================}
end.




