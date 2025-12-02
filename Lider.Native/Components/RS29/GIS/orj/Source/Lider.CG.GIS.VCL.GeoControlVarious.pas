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
  Various visual controls.
}

unit VCL.GisControlVarious ;
{$HPPEMIT '#pragma link "VCL.GisControlVarious"'}

{$INCLUDE GisInclude.inc}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  System.Types,
  VCL.Graphics,
  VCL.Forms,
  VCL.Controls,
  VCL.StdCtrls,
  VCL.ComCtrls,
  VCL.Grids,
  VCL.Menus,
  VCL.ExtCtrls,
  VCL.Buttons,

  GisRtl,
  GisTypesUI,
  GisTypes,
  GisCsBase,
  GisParams,
  VCL.GisComboBoxHelper;

type

  /// <summary>
  ///   Type of supported units.
  /// </summary>
  TGIS_SpinEditUnits = (

      /// <summary>
      ///   Twips - 1/1440 of inch.
      /// </summary>
      TWIPS,

      /// <summary>
      ///   Pixels.
      /// </summary>
      PX,

      /// <summary>
      ///   Millimeters.
      /// </summary>
      MM,

      /// <summary>
      ///   Millimeters.
      /// </summary>
      MScales,

      /// <summary>
      ///   Points ( 1/72 of inch).
      /// </summary>
      PT
  ) ;

  /// <summary>
  ///   Type of supported modes.
  /// </summary>
  TGIS_SpinEditMode = (

      /// <summary>
      ///   Editing unnamed value (like "32.12" ).
      /// </summary>
      spinVALUE,

      /// <summary>
      ///   Editing in units (like "32pt" ).
      /// </summary>
      spinUNITS,

      /// <summary>
      ///   Editing in scale values (like "1:300").
      /// </summary>
      spinSCALE
  ) ;


{$IFNDEF GENDOC}
  /// <summary>
  ///   Spin Edit class with parameter validation. Mainly for internal
  ///   purposes.
  /// </summary>
  [ComponentPlatformsAttribute( pfidWindows )]
  TGIS_SpinEdit = class( TCustomEdit )

    private // property internal values
      FMode       : TGIS_SpinEditMode  ;
      FSizeUnits  : TGIS_SpinEditUnits ;
      FPrecision  : Integer    ;
      FIncrement  : Double     ;
      FMinVal     : Double     ;
      FMaxVal     : Double     ;
      FValue      : Double     ;

    protected // property access routines
      /// <summary>
      ///   Property Enabled access routine.
      /// </summary>
      function  GetEnabled            : Boolean ; override;

      /// <summary>
      ///   Property Enabled access routine.
      /// </summary>
      procedure SetEnabled            ( _value : Boolean
                                      ) ; override;

      procedure fset_MinVal           ( const _value : Double  ) ; virtual;
      procedure fset_MaxVal           ( const _value : Double  ) ; virtual;
      function  fget_Value            : Double ; virtual  ;
      procedure fset_Value            ( const _value : Double
                                      ) ; virtual  ;
      procedure fset_Precision        ( const _value : Integer
                                      ) ; virtual  ;
      procedure fset_Increment        ( const _value : Double
                                      ) ; virtual  ;

      /// <summary>
      ///   Property Value access routine.
      /// </summary>
      /// <param name="_value">
      ///   requested value in float
      /// </param>
      function  prepareValue          ( const _value : Double
                                      ) : String ; virtual  ;

    private
      oButton     : TUpDown ;
      tmpText     : String  ;
      tmpValue    : Double  ;
      iCreated    : Boolean ;
      oFont       : TFont   ;
      uponDestroy : Boolean ;

    protected
      procedure doDestroy      ;

      /// <summary>
      ///   Restrict edit area to keep spin area untouched.
      /// </summary>
      procedure setEditRect   ;

      /// <summary>
      ///   Increment/Decrement click handler.
      /// </summary>
      procedure doClick       ( _sender : TObject ;
                                _button : TUDBtnType
                              ) ;

      /// <summary>
      ///   Overrides standard procedure to support required window styles.
      /// </summary>
      procedure CreateParams  ( var _params : TCreateParams
                              ) ; override;

      /// <summary>
      ///   Overrides standard procedure to restrict edited area.
      /// </summary>
      procedure CreateWnd     ; override;

    public

      /// <summary>
      ///   Overrides standard procedure to turn off GetChildern behavior.
      /// </summary>
      procedure GetChildren  ( _proc : TGetChildProc ;
                               _root : TComponent
                             ) ; override;

      /// <summary>
      ///   Key event handler. For up/down handling.,
      /// </summary>
      procedure KeyDown      ( var _key : Word ;
                               _shift   : TShiftState
                             ) ; override;

      /// <summary>
      ///   Key event handler. For basic input verification - allow only
      ///   "known" characters
      /// </summary>
      procedure KeyPress     ( var _key : Char
                             ) ; override;

      /// <summary>
      ///   Change value handler. Do value validation etc.
      /// </summary>
      procedure Change       ; override;

      /// <summary>
      ///   Override standard size message for proper control childes alignment.
      /// </summary>
      procedure WMSize       ( var _msg : TWMSize
                             ) ; message WM_SIZE ;

      /// <summary>
      ///   Override standard size message for proper control childes
      ///   alignment.
      /// </summary>
      procedure CMFontChanged( var _msg : TMessage
                             ) ; message CM_FONTCHANGED ;
    public
      /// <summary>
      ///   Create the control.
      /// </summary>
      constructor Create     ( _aowner: TComponent
                             ) ; override;

      /// <summary>
      ///   Destroy the control.
      /// </summary>
      destructor Destroy     ; override;

    public

      /// <summary>
      ///   Check if value is valid (inside MinVal..MaxVal zone).
      /// </summary>
      function   Valid         : Boolean ;

    public
      property SizeUnits : TGIS_SpinEditUnits read  FSizeUnits   ;

    // properties
    published

        /// <summary>
        ///   Mode of operations (scale, unites, etc).
        /// </summary>
        property Mode      : TGIS_SpinEditMode  read  FMode
                                                write FMode        ;

        /// <summary>
        ///   Increment value on up/down.
        /// </summary>
        property Increment : Double             read  FIncrement
                                                write fset_Increment ;

        /// <summary>
        ///   Number of digits.
        /// </summary>
        property Precision : Integer            read  FPrecision
                                                write fset_Precision ;

        /// <summary>
        ///   Minimum expected value.
        /// </summary>
        property MinVal    : Double             read  FMinVal
                                                write fset_MinVal    ;

        /// <summary>
        ///   Maximum expected value.
        /// </summary>
        property MaxVal    : Double             read  FMaxVal
                                                write fset_MaxVal    ;

        /// <summary>
        ///   Value of spin control.
        /// </summary>
        property Value     : Double             read  fget_Value
                                                write fset_Value     ;

    published
      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property Anchors;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property AutoSize;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property BevelEdges;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property BevelInner;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property BevelKind default bkNone;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property BevelOuter;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property BiDiMode;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property BorderStyle;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property CharCase;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property Color;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property Constraints;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property Ctl3D;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property DragCursor;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property DragKind;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property DragMode;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property Enabled ;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property Font;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property HideSelection;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property ImeMode;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property ImeName;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property MaxLength;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OEMConvert;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property ParentBiDiMode;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property ParentColor;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property ParentCtl3D;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property ParentFont;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property ParentShowHint;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property PopupMenu;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property ReadOnly;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property ShowHint;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property TabOrder;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property TabStop;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property Text;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property Visible;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnChange;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnClick;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnContextPopup;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnDblClick;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnDragDrop;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnDragOver;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnEndDock;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnEndDrag;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnStartDrag ;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnEnter;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnExit;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnKeyDown;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnKeyPress;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnKeyUp;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnMouseDown;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnMouseMove;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnMouseUp;

      /// <summary>
      ///   See documentation for TCustomEdit in Delphi help.
      /// </summary>
      property OnStartDock;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheel ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheelUp ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property OnMouseWheelDown ;
  end ;
{$ENDIF}

  {#gendoc:hide}
  TGIS_Bevel = TBevel ;

  /// <summary>
  ///   String Grid with support to OnColumnResize.
  /// </summary>
  TGIS_StringGrid = class ( TStringGrid )
    private
      FOnColumnResize : TNotifyEvent ;

    protected
      /// <summary>
      ///   See documentation for TStringGrid in Delphi help.
      /// </summary>
      procedure ColWidthsChanged    ; override;
    protected

      /// <summary>
      ///   See documentation for TStringGrid in Delphi help.
      /// </summary>
      /// <param name="_shift">
      ///   See documentation for TStringGrid in Delphi help.
      /// </param>
      /// <param name="_mouse_pos">
      ///   See documentation for TStringGrid in Delphi help.
      /// </param>
      /// <returns>
      ///   See documentation for TStringGrid in Delphi help.
      /// </returns>
      function DoMouseWheelDown     ( _shift     : TShiftState ;
                                      _mouse_pos : TPoint
                                    ) : Boolean ; override;

      /// <summary>
      ///   See documentation for TStringGrid in Delphi help.
      /// </summary>
      /// <param name="_shift">
      ///   See documentation for TStringGrid in Delphi help.
      /// </param>
      /// <param name="_mouse_pos">
      ///   See documentation for TStringGrid in Delphi help.
      /// </param>
      /// <returns>
      ///   See documentation for TStringGrid in Delphi help.
      /// </returns>
      function DoMouseWheelUp       ( _shift     : TShiftState;
                                      _mouse_pos : TPoint
                                    ) : Boolean ; override;
    public

      /// <summary>
      ///   See documentation for TStringGrid in Delphi help.
      /// </summary>
      /// <param name="_aowner">
      ///   See documentation for TStringGrid in Delphi help.
      /// </param>
      constructor Create    ( _aowner : TComponent
                            ) ; override;

      /// <summary>
      ///   See documentation for TStringGrid.DeleteRow in Delphi help. Added
      ///   here because DeleteRow method is protected.
      /// </summary>
      /// <param name="_arow">
      ///   row to delete
      /// </param>
      procedure DeleteRowEx   ( _arow   : LongInt
                              ) ;

      /// <summary>
      ///   See documentation for TStringGrid in Delphi help.
      /// </summary>
      procedure HideEditor    ; reintroduce ;

    public    // events
      /// <event/>
      property OnColumnResize : TNotifyEvent read FOnColumnResize
                                             write FOnColumnResize ;
  end ;


  /// <summary>
  ///   Button with popup and panel for selecting field.
  /// </summary>
  TGIS_FieldButton = class ( TButton )
    private
      FPanel       : TPanel  ;
      FEvent       : TNotifyEvent ;
      FControl     : TWinControl ;
      FField       : String ;
      FFieldList   : TStrings ;
      FOnHelp      : TGIS_HelpEvent ;

    private
      frmUnitsType : TGIS_CSUnitsType ;
      frmNative    : Boolean ;
      uponDestroy  : Boolean ;

    protected

      /// <summary>
      ///   Perform low level destroy.
      /// </summary>
      procedure doDestroy           ;

      procedure fset_FieldControl   ( const _control : TWinControl
                                    ) ;

      /// <summary>
      ///   OnMouseUp button event. See documentation for OnMouseUp in Delphi
      ///   help.
      /// </summary>
      /// <param name="_sender">
      ///   See documentation for OnMouseUp in Delphi help.
      /// </param>
      /// <param name="_button">
      ///   See documentation for OnMouseUp in Delphi help.
      /// </param>
      /// <param name="_shift">
      ///   See documentation for OnMouseUp in Delphi help.
      /// </param>
      /// <param name="_x">
      ///   See documentation for OnMouseUp in Delphi help.
      /// </param>
      /// <param name="_y">
      ///   See documentation for OnMouseUp in Delphi help.
      /// </param>
      procedure fieldButtonMouseUp( _sender       : TObject ;
                                    _button       : TMouseButton ;
                                    _shift        : TShiftState ;
                                    _x            : Integer ;
                                    _y            : Integer
                                  ) ;

      /// <summary>
      ///   Notify of controlled component removal.
      /// </summary>
      /// <param name="_component">
      ///   a component that notified about changes
      /// </param>
      /// <param name="_operation">
      ///   operation of a change
      /// </param>
      procedure Notification      ( _component    : TComponent ;
                                    _operation    : TOperation
                                  ) ; override;

      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_value">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      procedure SetEnabled        ( _value        : Boolean
                                  ) ; override;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      destructor Destroy          ; override;

    public

      /// <summary>
      ///   Field name.
      /// </summary>
      property  Field : String read  FField
                               write FField ;

      /// <summary>
      ///   Field list.
      /// </summary>
      property  FieldsList : TStrings read FFieldList ;

    published
      /// <event/>
      property OnCommitClick : TNotifyEvent
                               read  FEvent
                               write FEvent ;

      /// <summary>
      ///   Field control.
      /// </summary>
      property FieldControl  : TWinControl
                               read  FControl
                               write fset_FieldControl ;

      /// <event/>
      property OnHelp        : TGIS_HelpEvent
                               read  FOnHelp
                               write FOnHelp ;
  end ;

  /// <summary>
  ///   Combobox with pen styles.
  /// </summary>
  TGIS_PenStyleComboBox = class ( TComboBox )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
    private
      function  doValuePenStyle ( _sender  : TObject ;
                                  _value   : String
                                ) : String ;
      function  doCustomPenStyle( _sender  : TObject ;
                                  _value   : String
                                ) : String ;
      procedure doRenderPenStyle( _control : TComboBox       ;
                                  _rect    : TRect           ;
                                  _state   : TOwnerDrawState ;
                                  _class   : Char            ;
                                  _caption : String          ;
                                  _value   : String
                                ) ;
      function  fget_PenValue   : String ;
      procedure fset_PenValue   ( const _value : String
                                ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      /// <param name="_hasSymbol">
      ///   if option to choose symbols is placed on the list
      /// </param>
      procedure Fill              ( const _hasSymbol : Boolean
                                  ) ;
    public //events

      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;

      /// <event/>
      property  GetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent
                                  read  FGetBitmapEvent
                                  write FGetBitmapEvent ;

    public

      /// <summary>
      ///   Value in text form.
      /// </summary>
      property  Value          : String
                                  read  fget_PenValue
                                  write fset_PenValue ;

  end;

  /// <summary>
  ///   Combobox with brush styles.
  /// </summary>
  TGIS_PatternComboBox = class ( TComboBox )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
    private
      function  doValueBrushStyle ( _sender  : TObject ;
                                    _value : String
                                  ) : String ;
      function  doCustomBrushStyle( _sender  : TObject ;
                                    _value : String
                                  ) : String ;
      procedure doRenderBrushStyle( _control : TComboBox       ;
                                    _rect    : TRect           ;
                                    _state   : TOwnerDrawState ;
                                    _class   : Char            ;
                                    _caption : String          ;
                                    _value   : String
                                  ) ;
      function  fget_BrushValue   : String ;
      procedure fset_BrushValue   ( const _value : String
                                  ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      /// <param name="_hasSymbol">
      ///   if option to choose symbols is placed on the list
      /// </param>
      procedure Fill              ( const _hasSymbol : Boolean
                                   ) ;

    public //events

      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;

      /// <event/>
      property  GetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent
                                  read  FGetBitmapEvent
                                  write FGetBitmapEvent ;

    public

      /// <summary>
      ///   Value in text form.
      /// </summary>
      property  Value          : String
                                  read  fget_BrushValue
                                  write fset_BrushValue ;

  end;


  /// <summary>
  ///   Combobox with font names.
  /// </summary>
  TGIS_FontNameComboBox = class ( TComboBox )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FOnChange       : TNotifyEvent ;
    private
      function  doValueFont    ( _sender : TObject ;
                                 _value  : String
                               ) : String ;
      function  fget_ValueFont : String ;
      procedure fset_ValueFont ( const _value : String
                               ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      procedure Fill              ;

    public //events
      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;

      /// <summary>
      ///   Value in text form.
      /// </summary>
      property  Value          : String
                                  read  fget_ValueFont
                                  write fset_ValueFont ;
  end ;

  /// <summary>
  ///   Combobox with sizes.
  /// </summary>
  TGIS_SizeComboBox = class ( TComboBox )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
    private
      function  doValueSize    ( _sender  : TObject ;
                                 _value   : String
                               ) : String ;
      function  doCustomSize   ( _sender  : TObject ;
                                 _value : String
                               ) : String ;
      procedure doRenderSize   ( _control : TComboBox       ;
                                 _rect    : TRect           ;
                                 _state   : TOwnerDrawState ;
                                 _class   : Char            ;
                                 _caption : String          ;
                                 _value   : String
                               ) ;
      function  fget_SizeValue : String ;
      procedure fset_SizeValue ( const _value : String
                               ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create           ( _owner        : TComponent
                                   ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      /// <param name="_forSymbol">
      ///   if units for symbol size
      /// </param>
      /// <param name="_forLine">
      ///   if units for symbol size
      /// </param>
      /// <param name="_field">
      ///   if values from fields
      /// </param>
      /// <param name="_renderer">
      ///   if renderer value
      /// </param>
      procedure Fill               ( const _forSymbol : Boolean ;
                                     const _forLine   : Boolean ;
                                     const _field     : Boolean ;
                                     const _renderer  : Boolean
                                   ) ;

      /// <summary>
      ///   Fill the list with measure units.
      /// </summary>
      /// <param name="_field">
      ///   if values from fields
      /// </param>
      procedure FillRealWorldUnits ( const _field     : Boolean
                                   ) ;

      /// <summary>
      ///   Fill the list with aggregation sizes.
      /// </summary>
      procedure FillAggregation    ;

      /// <summary>
      ///   Fill the list with snap values.
      /// </summary>
      procedure FillSnap           ;

    public //events
      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;

      /// <summary>
      ///   Value in text form.
      /// </summary>
      property  Value          : String
                                  read  fget_SizeValue
                                  write fset_SizeValue ;
  end ;

  /// <summary>
  ///   Combobox with rotations.
  /// </summary>
  TGIS_RotationComboBox = class ( TComboBox )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
    private
      function  doValueRotation    ( _sender  : TObject ;
                                     _value   : String
                                   ) : String ;
      function  doCustomRotation   ( _sender  : TObject ;
                                     _value   : String
                                   ) : String ;
      procedure doRenderRotation   ( _control : TComboBox       ;
                                     _rect    : TRect           ;
                                     _state   : TOwnerDrawState ;
                                     _class   : Char            ;
                                     _caption : String          ;
                                     _value   : String
                                   ) ;
      function  fget_RotationValue : String ;
      procedure fset_RotationValue ( const _value : String
                                   ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create          ( _owner        : TComponent
                                  ) ;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      /// <param name="_field">
      ///   if values from fields
      /// </param>
      procedure Fill              ( _field        : Boolean
                                  ) ;

    public //events
      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;

      /// <summary>
      ///   Value in text form.
      /// </summary>
      property  Value          : String
                                  read  fget_RotationValue
                                  write fset_RotationValue ;
  end ;

  /// <summary>
  ///   Combobox with marker styles.
  /// </summary>
  TGIS_SymbolComboBox = class ( TComboBox )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
    private
      function  doValueSymbol    ( _sender  : TObject ;
                                   _value : String
                                 ) : String ;
      function  doCustomSymbol   ( _sender  : TObject ;
                                   _value : String
                                 ) : String ;
      procedure doRenderSymbol   ( _control : TComboBox       ;
                                   _rect    : TRect           ;
                                   _state   : TOwnerDrawState ;
                                   _class   : Char            ;
                                   _caption : String          ;
                                   _value   : String
                                 ) ;
      function  fget_SymbolValue : String ;
      procedure fset_SymbolValue ( const _value : String
                                 ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      procedure Fill ;

    public //events
      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;
      /// <event/>
      property  GetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent
                                  read  FGetBitmapEvent
                                  write FGetBitmapEvent ;

      /// <summary>
      ///   Value in text form.
      /// </summary>
      property  Value          : String
                                  read  fget_SymbolValue
                                  write fset_SymbolValue ;
  end ;

  /// <summary>
  ///   Combobox with shield symbols.
  /// </summary>
  TGIS_ShieldComboBox = class ( TComboBox )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
    private
      function  doValueSymbol    ( _sender  : TObject ;
                                   _value : String
                                 ) : String ;
      function  doCustomSymbol   ( _sender  : TObject ;
                                   _value : String
                                 ) : String ;
      procedure doRenderSymbol   ( _control : TComboBox       ;
                                   _rect    : TRect           ;
                                   _state   : TOwnerDrawState ;
                                   _class   : Char            ;
                                   _caption : String          ;
                                   _value   : String
                                 ) ;
      function  fget_SymbolValue : String ;
      procedure fset_SymbolValue ( const _value : String
                                 ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      procedure Fill ;

    public //events
      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;
      /// <event/>
      property  GetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent
                                  read  FGetBitmapEvent
                                  write FGetBitmapEvent ;

      /// <summary>
      ///   Value in text form.
      /// </summary>
      property  Value          : String
                                  read  fget_SymbolValue
                                  write fset_SymbolValue ;
  end ;

  /// <summary>
  ///   Combobox with with fields
  /// </summary>
  TGIS_FieldValueComboBox = class( TComboBox )
    private
      edtBox          : TEdit ;
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      iPos            : Integer ;
      iSel            : Integer ;

      FOnChange       : TNotifyEvent ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
    private
      procedure doResize       (       _sender : TObject
                               );
      function  doValue        (       _sender : TObject ;
                                       _string : String
                               ) : String ;

      procedure doEdtKeyDown   (       _sender : TObject  ;
                                 var   _key    : Word     ;
                                       _shift  : TShiftState
                               ) ;
      procedure doEdtKeyUp     (       _sender : TObject  ;
                                 var   _key    : Word     ;
                                       _shift  : TShiftState
                               ) ;
      procedure doEdtKeyPress  (       _sender : TObject  ;
                                 var   _key    : Char
                               ) ;
      procedure doEdtMouseDown (       _sender : TObject  ;
                                       _button : TMouseButton ;
                                       _shift  : TShiftState  ;
                                       _x      : Integer  ;
                                       _y      : Integer
                               ) ;
      procedure doEdtChange    (       _sender : TObject );
      procedure doComboChange  (       _sender : TObject );
      procedure doComboKeyDown (       _sender : TObject  ;
                                 var   _key    : Word     ;
                                       _shift  : TShiftState
                               ) ;

      function  fget_EdtValue  : String ;
      procedure fset_EdtValue  ( const _value   : String
                               ) ;

    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create        (       _owner : TComponent
                                ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      /// <param name="_lst">
      ///   values to fill.
      /// </param>
      procedure   Fill          ( const _lst   : TStringList
                                ) ;

      /// <summary>
      ///   See documentation in Delphi help.
      /// </summary>
      /// <param name="_msg">
      ///   See documentation in Delphi help.
      /// </param>
      procedure WMPaint        ( var   _msg     : TWMPaint
                               ) ; message WM_PAINT ;

    public //events
      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write FOnChange ;
      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;

    public
      /// <summary>
      ///   Value in text form.
      /// </summary>
      property  Value          : String
                                  read  fget_EdtValue
                                  write fset_EdtValue ;

  end;
  /// <summary>
  ///   Button with bitmap.
  /// </summary>
  TGIS_BitmapButton = class ( TBitBtn )
    private
      FBitmap : TGIS_Bitmap ;
    private
      procedure displayBitmap ;
      procedure paintEmpty( const _bitmap : TBitmap ) ;
      procedure setBitmap( const _bmp : TGIS_Bitmap ) ;
      function  getBitmap : TGIS_Bitmap ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create          ( _owner        : TComponent
                                  ) ; override;
      /// <summary>
      ///   Destroy the control.
      /// </summary>
      destructor Destroy          ; override;

    public
      /// <summary>
      ///   Bitmap.
      /// </summary>
      property Bitmap : TGIS_Bitmap read getBitmap write setBitmap ;
  end;


  /// <summary>
  ///   Button with bitmap.
  /// </summary>
  TGIS_FontButton = class ( TButton )
    private
      FFontEx : TGIS_Font ;
    private
      procedure displayFont ;
      procedure setFontEx( const _font : TGIS_Font ) ;
      function  getFontEx : TGIS_Font ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      constructor Create          ( _owner        : TComponent
                                  ) ; override;
      /// <summary>
      ///   Destroy the control.
      /// </summary>
      destructor Destroy          ; override;

    public
      /// <summary>
      ///   Font.
      /// </summary>
      property FontEx : TGIS_Font read getFontEx write setFontEx ;
  end;


//##############################################################################
implementation

{$R GisControlVarious_16x16.RES}

uses
  System.Generics.Collections,
  PVL.GisPvl,
  PVL.GisControlFieldFactor,
  VCL.GisFramework,
  GisResource ;

const
  SIZE_M_SC = 'm*' ;                 // Text for millimeters
  SIZE_MM   = 'mm' ;                 // Text for millimeters
  SIZE_IN   = 'in' ;                 // Text for inches
  SIZE_PT   = 'pt' ;                 // Text for points
  SIZE_PX   = 'px' ;                 // Text for pixels

var
  oItemCache : TDictionary<String,TGIS_Bitmap> ; // global bitmap item cache

//==============================================================================
// Utilities
//==============================================================================

  // Convert size between different supported units.
  // _val     Value to be converted
  // _src     type of source units
  // _src     type of destination units
  // return   converted value
  function convert_size( const _val : Double ;
                         const _src, _dst : TGIS_SpinEditUnits
                       ) : Double ;
  var
    oval      : Double ;
    pxperinch : Integer ;
  begin
    if Screen.PixelsPerInch <> 0 then
      pxperinch := Screen.PixelsPerInch
    else
      pxperinch := 96 ;

    case _src of
      TGIS_SpinEditUnits.PX      : oval := _val * 1440 / pxperinch ;
      TGIS_SpinEditUnits.MM      : oval := _val * 1440 / 2.54 / 10 ;
      TGIS_SpinEditUnits.MScales : oval := _val * 1440 / 2.54 / 10 * 1000 ;
      TGIS_SpinEditUnits.PT      : oval := _val * 1440 / 72        ;
      else                         oval := _val                    ;
    end ;
    case _dst of
      TGIS_SpinEditUnits.PX      : Result := oval / 1440 * pxperinch ;
      TGIS_SpinEditUnits.MM      : Result := oval / 1440 * 2.54 * 10 ;
      TGIS_SpinEditUnits.MScales : Result := oval / 1440 * 2.54 * 10 / 1000 ;
      TGIS_SpinEditUnits.PT      : Result := oval / 1440 * 72        ;
      else                         Result := oval                    ;
    end ;
  end ;

function split( const _str : String ; const _separator : array of Char ) : TArray<String> ;
var
  res : TStringDynArray ;
  i   : Integer ;
begin
  {$IFDEF LEVEL_XE3_RTL}
    Result := _str.Split( _separator )
  {$ELSE}
    res := System.StrUtils.SplitString( _str, _separator ) ;
    SetLength( Result, length( res ) ) ;
    for i := 0 to length( res )-1 do
      Result[i] := res[i] ;
  {$ENDIF}
end;

{$IFNDEF GENDOC}
//==============================================================================
// TGIS_SpinEdit
//==============================================================================

  constructor TGIS_SpinEdit.Create( _aowner: TComponent ) ;
  begin
    inherited Create( _aowner ) ;

    FMode      := TGIS_SpinEditMode.spinVALUE ;
    FSizeUnits := TGIS_SpinEditUnits.PT    ;
    FMinVal    := 0   ;
    FMaxVal    := 100 ;
    FIncrement := 0   ;
    FPrecision := 0   ;
    FValue     := 0   ;

    iCreated   := False ;

    oButton := TUpDown.Create( Self );
    with oButton do begin
      Parent       := Self    ;
      Width        := 15      ;
      Height       := Height  ;
      Visible      := True    ;
      Min          := Low ( Smallint ) ;
      Max          := High( Smallint ) ;
      Increment    := 1       ;
      Wrap         := True    ;
      OnClick      := doClick ;
    end ;

    oFont := TFont.Create ;
    oFont.Assign( Font ) ;
    uponDestroy := False ;
  end ;


  destructor TGIS_SpinEdit.Destroy ;
  begin
    if not uponDestroy then
      doDestroy ;

    inherited ;
  end ;


  procedure TGIS_SpinEdit.doDestroy ;
  begin
    uponDestroy := True ;
    oButton.Free ;
    FreeObject( oFont ) ;
  end;

  function TGIS_SpinEdit.Valid : Boolean ;
  begin
    if not iCreated then begin
      Result := True ;
      exit ;
    end ;

    if IsStringEmpty( tmpText ) or
       ( tmpValue < FMinVal ) or
       ( tmpValue > FMaxVal ) then
    begin
      Result     := False        ;
      Font.Color := clRed      ;
    end
    else begin
      Result     := True         ;
      Font.Color := clWindowText ;
    end ;
  end ;


  function TGIS_SpinEdit.GetEnabled : Boolean  ;
  begin
    Result := inherited getEnabled ;
  end ;

  procedure TGIS_SpinEdit.SetEnabled( _value : Boolean ) ;
  begin
    inherited setEnabled( _value ) ;
    oButton.Enabled := _value ;
  end ;


  procedure TGIS_SpinEdit.fset_MinVal( const _value : Double ) ;
  begin
    FMinVal := _value ;

    Valid ;
  end ;

  procedure TGIS_SpinEdit.fset_MaxVal( const _value : Double ) ;
  begin
    FMaxVal := _value ;

    Valid ;
  end ;

  function  TGIS_SpinEdit.fget_Value : Double ;
  begin
    Result := FValue ;

   if FSizeUnits = TGIS_SpinEditUnits.MScales then begin
      Result := FValue + GIS_AUTOSIZE_SIZE ;
    end;

  end ;

  procedure TGIS_SpinEdit.fset_Value ( const _value : Double  ) ;
  begin
    Text := prepareValue( _value ) ;
  end ;

  procedure TGIS_SpinEdit.fset_Increment( const _value : Double ) ;
  begin
    if _value = FIncrement then exit ;
    FIncrement := _value ;
    RecreateWnd ;
  end ;

  procedure TGIS_SpinEdit.fset_Precision( const _value : Integer ) ;
  begin
    FPrecision := _value ;
    Invalidate ;
  end ;

  function TGIS_SpinEdit.prepareValue( const _value : Double ) : String ;
  var
    txt  : String  ;
    oval : Double  ;
  begin
    if Mode = TGIS_SpinEditMode.spinUNITS then begin
      if _value < 0 then FValue := convert_size( -_value, TGIS_SpinEditUnits.PX, TGIS_SpinEditUnits.TWIPS )
                    else FValue := _value ;

      if FValue > GIS_AUTOSIZE_SIZE then begin
        FValue := FValue - GIS_AUTOSIZE_SIZE ;
        FSizeUnits := TGIS_SpinEditUnits.MScales ;
      end;

      case FSizeUnits of
        TGIS_SpinEditUnits.MM :
          begin
            txt        := SIZE_MM ;
            oval       := convert_size( FValue, TGIS_SpinEditUnits.TWIPS, TGIS_SpinEditUnits.MM ) ;
            FIncrement := convert_size( 0.01, TGIS_SpinEditUnits.MM, TGIS_SpinEditUnits.TWIPS ) ;
            FPrecision := 2 ;
          end ;
        TGIS_SpinEditUnits.MScales :
          begin
            txt        := SIZE_M_SC ;
            oval       := convert_size( FValue, TGIS_SpinEditUnits.TWIPS, TGIS_SpinEditUnits.MScales ) ;
            FIncrement := convert_size( 0.01, TGIS_SpinEditUnits.MScales, TGIS_SpinEditUnits.TWIPS ) ;
            FPrecision := 2 ;
          end ;
        TGIS_SpinEditUnits.PT :
          begin
            txt        := SIZE_PT ;
            oval       := convert_size( FValue, TGIS_SpinEditUnits.TWIPS, TGIS_SpinEditUnits.PT ) ;
            FIncrement := convert_size( 0.1, TGIS_SpinEditUnits.PT, TGIS_SpinEditUnits.TWIPS ) ;
            FPrecision := 1 ;
          end ;
        TGIS_SpinEditUnits.PX :
          begin
            txt        := SIZE_PX ;
            oval       := convert_size( FValue, TGIS_SpinEditUnits.TWIPS, TGIS_SpinEditUnits.PX ) ;
            FIncrement := convert_size( 1, TGIS_SpinEditUnits.PX, TGIS_SpinEditUnits.TWIPS ) ;
            FPrecision := 0 ;
          end ;
        else
          begin
            txt  := ''     ;
            oval := _value ;
          end ;
      end ;
    end
    else begin
      FValue := _value ;
      oval   := _value ;
    end ;

    if Mode = TGIS_SpinEditMode.spinSCALE then begin
      if      oval =  0     then Result := ''
      else if oval >  1e300 then Result := ''
      else if oval <  1     then Result := Format( '1:%.0f%s'  , [ 1/oval, txt ] )
      else if oval >= 1     then Result := Format( '%.0f:1%s'  , [ oval/1, txt ] )
    end
    else begin
      if FPrecision > 14 then
        Result := Format( '%g%s'  , [ oval, txt ] )
      else
        Result := Format( '%.*f%s', [ FPrecision, oval, txt ] ) ;
    end ;
  end ;


  procedure TGIS_SpinEdit.setEditRect;
  var
    loc: TRect;
  begin
    loc.Bottom := ClientHeight + 1;  // +1 is workaround for windows paint bug

    if oButton.Visible then loc.Right := oButton.Left - 2
                       else loc.Right := ClientWidth ;
    loc.Top    := 0 ;
    loc.Left   := 0 ;
    SendMessage( Handle, EM_SETRECTNP, 0, NativeInt( @loc ) ) ;
    SendMessage( Handle, EM_GETRECT  , 0, NativeInt( @loc ) ) ;
  end ;


  procedure TGIS_SpinEdit.doClick( _sender: TObject; _button: TUDBtnType ) ;
  var
    fact : Double ;
    oval : Double ;
  begin
    if  FIncrement = 0 then exit ;

    fact := 1 / FIncrement ;

    if _button = btPrev then oval := RoundS( FValue * fact - 1 ) / fact
                        else oval := RoundS( FValue * fact + 1 ) / fact ;

    if oval < FMinVal then oval := FMinVal ;
    if oval > FMaxVal then oval := FMaxVal ;

    Value := oval ;
  end ;


  procedure TGIS_SpinEdit.CreateParams( var _params: TCreateParams ) ;
  begin
    inherited CreateParams( _params ) ;
    _params.Style := _params.Style or ES_MULTILINE or WS_CLIPCHILDREN ;
    oButton.Visible := FIncrement <> 0 ;
    iCreated := True ;
  end ;


  procedure TGIS_SpinEdit.CreateWnd ;
  begin
    inherited CreateWnd ;
    setEditRect ;
  end ;


  procedure TGIS_SpinEdit.GetChildren( _proc: TGetChildProc; _root: TComponent) ;
  begin
    // do nothing
  end ;

  procedure TGIS_SpinEdit.KeyDown( var _key: Word; _shift: TShiftState ) ;
  begin
    if      _key = VK_UP   then doClick( Self, btNext )
    else if _key = VK_DOWN then doClick( Self, btPrev ) ;
    inherited KeyDown( _key, _shift ) ;
  end ;

  procedure TGIS_SpinEdit.KeyPress( var _key : Char ) ;
  var
    i   : Integer ;
    tmp : String  ;
    key : Char    ;
  begin
    key := _key ;
    if Mode = TGIS_SpinEditMode.spinUNITS then begin
      tmp := SIZE_MM + SIZE_IN + SIZE_PT + SIZE_PX + SIZE_M_SC;
      for i := StringFirst to StringLast( tmp ) do
        if LowerCase( key ) = tmp[i] then begin
          inherited ;
          exit ;
        end ;
    end
    else if Mode = TGIS_SpinEditMode.spinVALUE then begin
      if LowerCase( key ) = 'e' then begin
        inherited ;
        exit ;
      end ;
    end ;

    if (FPrecision <=0) and CharInSet( key, [ '.', ',' ] ) then
      key := Char( 0 )
    else begin
      if Mode = TGIS_SpinEditMode.spinSCALE then begin
        if not CharInSet( key, [ Char(8), '0'..'9', ':', '/' ] ) then
          key := Char( 0 )
      end
      else begin
        if not CharInSet( key, [ Char(8), '0'..'9', '-', '.', ','] ) then
          key := Char( 0 ) ;
      end ;
    end ;

    _key := key ;
    inherited ;
  end ;

  procedure TGIS_SpinEdit.Change ;
  var
    i    : Integer ;
    c    : Char    ;
    tval : String  ;
    tunt : String  ;
  begin
    tval := '' ;
    tunt := '' ;
    for i := StringFirst to StringLast(Text) do begin
      c := Text[i] ;
      case Mode of
        TGIS_SpinEditMode.spinUNITS :
          if CharInSet( c, [ 'a'..'z', 'A', 'Z' ] ) and (c <> 'e') then begin
            tunt := LowerCase( Copy( Text, i, 4096 ) ) ;
            break ;
          end ;
        TGIS_SpinEditMode.spinSCALE :
          if CharInSet( c, [':','/'] ) then begin
            tunt := LowerCase( Copy( Text, i+1, 4096 ) ) ;
            break ;
          end ;
      end ;
      tval := tval + c ;
    end ;

    try
      tmpText := Trim( tval ) ;
      if IsStringEmpty( tmpText ) then
        abort ;

      tmpValue := DotStrToFloat( tmpText ) ;

      if Mode = TGIS_SpinEditMode.spinUNITS then begin
        if      tunt = SIZE_MM   then FSizeUnits := TGIS_SpinEditUnits.MM
        else if tunt = SIZE_M_SC then FSizeUnits := TGIS_SpinEditUnits.MScales
        else if tunt = SIZE_PX   then FSizeUnits := TGIS_SpinEditUnits.PX
        else if tunt = SIZE_PT   then FSizeUnits := TGIS_SpinEditUnits.PT
        else abort ;

        tmpValue := ( convert_size( tmpValue, FSizeUnits, TGIS_SpinEditUnits.TWIPS ) ) ;

        if FPrecision <=0  then tmpValue := RoundS( tmpValue ) ;

      end
      else if Mode = TGIS_SpinEditMode.spinSCALE then begin
        tmpValue := DotStrToFloat( tmpText ) / DotStrToFloat( tunt ) ;
      end ;

      if ( tmpValue < FMinVal ) or ( tmpValue > FMaxVal ) then abort ;
      prepareValue( tmpValue ) ;
      Font.Color := clWindowText ;
    except
      Font.Color := clRed ;
    end ;

    inherited ;
  end ;

  procedure TGIS_SpinEdit.WMSize( var _msg: TWMSize ) ;
  begin
    if oButton <> nil then begin
      if Ctl3D then
        oButton.SetBounds( ClientRect.Right - oButton.Width + 1 ,
                           ClientRect.Top                   - 1 ,
                           oButton.Width                        ,
                           ClientRect.Bottom                + 2
                         )
      else
        oButton.SetBounds( ClientRect.Right - oButton.Width - 1 ,
                           ClientRect.Top                   + 1 ,
                           oButton.Width                        ,
                           ClientRect.Bottom                - 2
                         ) ;
    end ;

    setEditRect ;
  end ;

  procedure TGIS_SpinEdit.CMFontChanged(var _msg : TMessage);
  begin
    if ( Font.Size = oFont.Size ) and ( Font.Name = oFont.Name ) then
      Invalidate
    else begin
      inherited ;
      oFont.Assign( Font ) ;
    end ;
  end ;
{$ENDIF}


//==============================================================================
// TGIS_StringGrid
//==============================================================================

  constructor TGIS_StringGrid.Create( _aowner : TComponent ) ;
  begin
    inherited ;
    DoubleBuffered := not IsWin11 ;
  end ;

  procedure TGIS_StringGrid.DeleteRowEx  ( _arow   : LongInt ) ;
  begin
    inherited DeleteRow( _arow )
  end ;

  procedure TGIS_StringGrid.HideEditor ;
  begin
    inherited HideEditor ;
  end ;

  procedure TGIS_StringGrid.ColWidthsChanged ;
  begin
    if Assigned( OnColumnResize ) then OnColumnResize( self ) ;
  end ;


  function TGIS_StringGrid.DoMouseWheelDown(
    _shift     : TShiftState ;
    _mouse_pos : TPoint
  ) : Boolean ;
  begin
    SendMessage( Handle, WM_VSCROLL, 1, 0 ) ;
    Result := True ;
  end ;

  function TGIS_StringGrid.DoMouseWheelUp(
    _shift     : TShiftState ;
    _mouse_pos : TPoint
  ) : Boolean ;
  begin
    SendMessage( Handle, WM_VSCROLL, 0, 0 ) ;
    Result := True ;
  end ;


//==============================================================================
// TGIS_FieldButton
//==============================================================================

  constructor TGIS_FieldButton.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;

    ControlStyle := ControlStyle - [ csSetCaption ] ;

    Width     := 21 ;
    Height    := 21 ;

    Caption := '...' ;
    OnMouseUp := fieldButtonMouseUp ;

    FPanel := TPanel.Create( self ) ;
    FPanel.Caption    := 'fld:' ;
    FPanel.BevelOuter := bvLowered ;
    FPanel.Parent     := Self ;
    FPanel.Visible    := False ;

    FField := '' ;
    FFieldList := TStringList.Create ;

    uponDestroy := False ;
  end ;

  destructor TGIS_FieldButton.Destroy ;
  begin
    if not uponDestroy then
      doDestroy ;
    inherited ;
  end ;

  procedure TGIS_FieldButton.doDestroy ;
  begin
    uponDestroy := True ;

    FreeObject( FFieldList ) ;
  end ;

  procedure TGIS_FieldButton.fieldButtonMouseUp(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Integer ;
    _y      : Integer
  ) ;
  var
    frm  : TGIS_ControlFieldFactor ;
    proc : TGIS_Proc ;
  begin
    frm := TGIS_ControlFieldFactor.Create( Self ) ;
    frm.FillFields( FFieldList ) ;
    frm.cmbFields.ItemIndex := frm.cmbFields.IndexOf( FField ) ;
    frm.FillUnits( TGIS_FieldFactorUnitsType.NoScale ) ;
    proc := procedure( _modal_result : TGIS_PvlModalResult )
    begin
      if _modal_result <> TGIS_PvlModalResult.OK then
        exit ;

      Field := frm.cmbFields.Item[frm.cmbFields.ItemIndex] ;
      if Assigned( FEvent ) then
        FEvent( self ) ;
    end;
    frm.Execute( FOnHelp, proc ) ;
  end ;

  procedure TGIS_FieldButton.Notification(
    _component : TComponent ;
    _operation : TOperation
  ) ;
  begin
    if ( _component is TGIS_FieldButton ) and ( _operation = opRemove ) then
      FControl := nil ;

    inherited Notification( _component, _operation ) ;
  end;

  procedure TGIS_FieldButton.SetEnabled(
    _value : Boolean
  ) ;
  begin
    inherited SetEnabled( _value ) ;

    if _value then
      FPanel.Font.Color := clBtnText
    else
      FPanel.Font.Color := clInactiveBorder ;
  end ;

  procedure TGIS_FieldButton.fset_FieldControl(
    const _control : TWinControl
  ) ;
  begin
    FControl := _control ;

    if Assigned( _control ) then begin
      FPanel.Left   := _control.Left   ;
      FPanel.Top    := _control.Top    ;
      FPanel.Width  := _control.Width  ;
      FPanel.Height := _control.Height ;
      FPanel.Parent := Self ;

      Left := _control.Left + _control.Width + 2 ;
      Top  := _control.Top ;
    end ;
    FPanel.Visible  := False ;
  end ;

{ TGIS_PenStyleComboBox }

  constructor TGIS_PenStyleComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;
    ItemHeight := 24 ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_PenStyle', 3 ) ;
    comboExt.ValueEvent  := doValuePenStyle ;
    comboExt.RenderEvent := doRenderPenStyle ;
    comboExt.CustomEvent := doCustomPenStyle ;
    IComboExt := comboExt ;

    ShowHint := True ;
  end ;


  procedure TGIS_PenStyleComboBox.doRenderPenStyle(
    _control : TComboBox       ;
    _rect    : TRect           ;
    _state   : TOwnerDrawState ;
    _class   : Char            ;
    _caption : String          ;
    _value   : String
  ) ;
  var
    r    : TRect ;
    sitm : String ;
    bmp  : TGIS_Bitmap ;
    s1, s2, s3 : String ;
    tf   : TTextFormat ;
  begin
    if BiDiMode = bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [] ;

    _control.Canvas.Pen.Width   := 1;
    _control.Canvas.Brush.Style := bsSolid ;
    _control.Canvas.Pen.Color   := _control.Canvas.Font.Color ;

    r := _rect;
    _control.Canvas.FillRect(r);
    InflateRect(r, -1, -1);

    SplitParamAsText( _value, s1, s2, s3 ) ;

    case _class of
      'f' : begin
              r.Left  := r.Left  + 4 ;
              r.Right := r.Right - 4 ;
              r.Top   := r.Top   + 4 ;
              _control.Canvas.TextRect( r, s2, tf ) ;
            end ;
      'C' : begin
              r.Left  := r.Left  + 4 ;
              r.Right := r.Right - 4 ;
              r.Top   := r.Top   + 4 ;
              _control.Canvas.TextRect( r, _caption, tf ) ;
            end ;
      'S','s' :
            begin
              if assigned( FGetBitmapEvent ) then begin
                sitm := ClassName + '_' +
                        _value + '_' +
                        IntToStr( r.Width  ) + '_' +
                        IntToStr( r.Height ) + '_' +
                        IntToStr( _control.Canvas.Font.Color ) ;

                if not oItemCache.TryGetValue( sitm, bmp ) then begin
                  bmp := FGetBitmapEvent( _control,
                                          _value,
                                          r.Width, r.Height
                                        ) ;
                  oItemCache.Add( sitm, bmp );
                end;

                if not TGIS_Bitmap.IsNilOrEmpty( bmp ) then begin
                  if odDisabled in _state then
                    _control.Canvas.Draw(
                      r.Left,
                      r.Top,
                      TBitmap(bmp.NativeBitmap),
                      128
                    )
                  else
                    _control.Canvas.Draw(
                      r.Left,
                      r.Top,
                      TBitmap(bmp.NativeBitmap),
                      255
                    ) ;
                end;
              end ;
            end;
    end ;
    Hint := _caption ;
  end;


  function TGIS_PenStyleComboBox.doCustomPenStyle(
     _sender : TObject ;
     _value  : String
  ) : String ;
  var
    res : String ;
    s1, s2, s3 : String ;
  begin
    if assigned( FCustomEvent ) then begin
      res := FCustomEvent( self, _value ) ;
      SplitParamAsText( res, s1, s2, s3 ) ;

      if (s1 = GIS_PARAMTXT_TYPE_SYMBOL) or (s1 = GIS_PARAMTXT_TYPE_CODE) then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'S',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else if s1 = GIS_PARAMTXT_TYPE_FIELD then
          Result := TGIS_ComboBoxHelper.PrepareItem(
                      False ,
                      'f',
                      TGIS_ComboBoxHelperPosition.Lru,
                      res,
                      res
                    )
      else
        Result := '' ;
    end ;
  end ;

  function TGIS_PenStyleComboBox.doValuePenStyle(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    s1, s2, s3 : String ;
  begin
    try
      SplitParamAsText( _value, s1, s2, s3 );

      if s1 = GIS_PARAMTXT_TYPE_STOCK then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    's',
                    TGIS_ComboBoxHelperPosition.Top,
                    _value,
                    _value
                  )
      else if s1 = GIS_PARAMTXT_TYPE_FIELD then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'f',
                    TGIS_ComboBoxHelperPosition.Lru,
                    _value,
                    _value
                  )

      else if not IsStringEmpty(_value) then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'S',
                    TGIS_ComboBoxHelperPosition.Lru,
                    _value,
                    _value
                  )
      else
        Result := '' ;
    except
      Result := '' ;
    end;
  end;

  function TGIS_PenStyleComboBox.fget_PenValue : String ;
  begin
    Result := comboExt.Value ;
  end;


  procedure TGIS_PenStyleComboBox.fset_PenValue(
    const _value : String
  ) ;
  begin
    comboExt.Value := _value ;
  end;

  procedure TGIS_PenStyleComboBox.Fill(
    const _hasSymbol : Boolean
  ) ;
  begin
    comboExt.BeginFill;
    try
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PEN_SOLID,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PEN_SOLID,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PEN_DASH,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PEN_DASH,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PEN_DOT,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PEN_DOT,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PEN_DASHDOT,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PEN_DASHDOT,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PEN_DASHDOTDOT,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PEN_DASHDOTDOT,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PEN_CLEAR,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PEN_CLEAR,
            ''
          )
        )
      ) ;
      if _hasSymbol then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
            'Editor...', GIS_PARAMTXT_TYPE_CODE
          )
        ) ;
      if _hasSymbol then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
            GIS_RS_LEGEND_PRM_SYMBOL + '...', GIS_PARAMTXT_TYPE_SYMBOL
          )
        ) ;

      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
        )
      ) ;

    finally
      comboExt.EndFill;
    end ;
  end ;

  constructor TGIS_PatternComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;
    ItemHeight := 24 ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_BrushStyle', 3 ) ;
    comboExt.ValueEvent  := doValueBrushStyle ;
    comboExt.RenderEvent := doRenderBrushStyle ;
    comboExt.CustomEvent := doCustomBrushStyle ;

    IComboExt := comboExt ;

    ShowHint := True ;
  end ;


  procedure TGIS_PatternComboBox.Fill(
    const _hasSymbol : Boolean
  ) ;
  begin
    comboExt.BeginFill ;
    try
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PATTERN_SOLID,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PATTERN_SOLID,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PATTERN_TRANSPARENT,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PATTERN_TRANSPARENT,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PATTERN_HORIZONTAL,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PATTERN_HORIZONTAL,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PATTERN_VERTICAL,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PATTERN_VERTICAL,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PATTERN_FDIAGONAL,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PATTERN_FDIAGONAL,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PATTERN_BDIAGONAL,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PATTERN_BDIAGONAL,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PATTERN_CROSS,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PATTERN_CROSS,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_PATTERN_DIAGCROSS,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_PATTERN_DIAGCROSS,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_LEGEND_PRM_BITMAP + '...', GIS_PARAMTXT_TYPE_TEXTURE
        )
      ) ;
      if _hasSymbol then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
            GIS_RS_LEGEND_PRM_SYMBOL + '...', GIS_PARAMTXT_TYPE_SYMBOL
          )
        ) ;
    finally
      comboExt.EndFill ;
    end ;
  end ;

  function TGIS_PatternComboBox.doValueBrushStyle(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    s1, s2, s3 : String ;
  begin
    try
      SplitParamAsText( _value, s1, s2, s3 ) ;

      if s1 = GIS_PARAMTXT_TYPE_STOCK then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    's',
                    TGIS_ComboBoxHelperPosition.Top,
                    _value,
                    _value
                  )
      else
      if not IsStringEmpty(_value) then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'B',
                    TGIS_ComboBoxHelperPosition.Lru,
                    _value,
                    _value
                  )
      else
        Result := '' ;
    except
      Result := '' ;
    end;
  end ;


  function TGIS_PatternComboBox.doCustomBrushStyle(
    _sender  : TObject ;
    _value : String
  ) : String ;
  var
    res : String ;
  begin
    if assigned( FCustomEvent ) then begin
      res := FCustomEvent( self, _value ) ;
      if not IsStringEmpty(res) then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'B',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else
        Result := '' ;
    end ;
  end ;

  procedure TGIS_PatternComboBox.doRenderBrushStyle(
    _control : TComboBox       ;
    _rect    : TRect           ;
    _state   : TOwnerDrawState ;
    _class   : Char            ;
    _caption : String          ;
    _value   : String
  ) ;
  var
    r    : TRect ;
    sitm : String ;
    bmp  : TGIS_Bitmap ;
    tf   : TTextFormat ;
    str  : String ;
  begin
    _control.Canvas.Pen.Width   := 1;
    _control.Canvas.Brush.Style := bsSolid ;
    _control.Canvas.Pen.Color   := _control.Canvas.Font.Color ;

    r := _rect;
    _control.Canvas.FillRect(r);
    InflateRect(r, -1, -1);

    case _class of
      'C' : begin
              if BiDiMode = bdRightToLeft then
                tf := [tfRtlReading,tfRight]
              else
                tf := [] ;
              r.Left  := r.Left  + 4 ;
              r.Right := r.Right - 4 ;
              r.Top   := r.Top   + 4 ;
              _control.Canvas.TextRect( r, _caption, tf ) ;
            end ;
      'B','s' :
            begin
              if assigned( FGetBitmapEvent ) then begin
                sitm := ClassName + '_' +
                        _value + '_' +
                        IntToStr( r.Width  ) + '_' +
                        IntToStr( r.Height ) + '_' +
                        IntToStr( _control.Canvas.Font.Color ) ;

                if not oItemCache.TryGetValue( sitm, bmp ) then begin
                  bmp := FGetBitmapEvent( _control,
                                          _value,
                                          r.Width, r.Height
                                        ) ;
                  oItemCache.Add( sitm, bmp );
                end;

                if not TGIS_Bitmap.IsNilOrEmpty( bmp ) then begin
                  if odDisabled in _state then
                    _control.Canvas.Draw(
                      r.Left,
                      r.Top,
                      TBitmap( bmp.NativeBitmap ),
                      128
                    )
                  else
                    _control.Canvas.Draw(
                      r.Left,
                      r.Top,
                      TBitmap( bmp.NativeBitmap ),
                      255
                    ) ;
                end;
              end ;
            end;
    end ;
    Hint := _caption ;
  end ;

  function TGIS_PatternComboBox.fget_BrushValue : String ;
  begin
    Result := comboExt.Value ;
  end ;

  procedure TGIS_PatternComboBox.fset_BrushValue(
    const _value : String
  ) ;
  begin
    comboExt.Value := _value ;
  end ;

  constructor TGIS_SymbolComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;
    ItemHeight := 24 ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Symbol', 3 ) ;
    comboExt.ValueEvent  := doValueSymbol ;
    comboExt.RenderEvent := doRenderSymbol ;
    comboExt.CustomEvent := doCustomSymbol ;

    IComboExt := comboExt ;

    ShowHint := True ;
  end ;


  procedure TGIS_SymbolComboBox.Fill ;
  begin
    comboExt.BeginFill ;
    try
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_MARKER_BOX,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_MARKER_BOX,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_MARKER_CIRCLE,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_MARKER_CIRCLE,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_MARKER_CROSS,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_MARKER_CROSS,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_MARKER_DIAGCROSS,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_MARKER_DIAGCROSS,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_MARKER_TRIANGLEUP,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_MARKER_TRIANGLEUP,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_MARKER_TRIANGLEDOWN,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_MARKER_TRIANGLEDOWN,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_MARKER_TRIANGLELEFT,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_MARKER_TRIANGLELEFT,
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top,
          GIS_INI_PARAM_MARKER_TRIANGLERIGHT,
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_STOCK,
            GIS_INI_PARAM_MARKER_TRIANGLERIGHT,
            ''
          )
        )
      ) ;

      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_LEGEND_PRM_SYMBOL + '...', GIS_PARAMTXT_TYPE_SYMBOL
        )
      ) ;

      ItemIndex := 0 ;
    finally
      comboExt.EndFill ;
    end ;
  end ;

  function TGIS_SymbolComboBox.doValueSymbol(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    s1, s2, s3 : String ;
  begin
    try
      SplitParamAsText( _value, s1, s2, s3 );

      if s1 = GIS_PARAMTXT_TYPE_STOCK then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    's',
                    TGIS_ComboBoxHelperPosition.Top,
                    _value,
                    _value
                  )
      else
      if not IsStringEmpty(_value) then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'B',
                    TGIS_ComboBoxHelperPosition.Lru,
                    _value,
                    _value
                  )
      else
        Result := '' ;
    except
      Result := '' ;
    end;
  end ;


  function TGIS_SymbolComboBox.doCustomSymbol(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    res : String ;
  begin
    if assigned( FCustomEvent ) then begin
      res := FCustomEvent( self, _value ) ;
      if not IsStringEmpty(res) then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'B',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else
        Result := '' ;
    end ;
  end ;

  procedure TGIS_SymbolComboBox.doRenderSymbol(
    _control : TComboBox       ;
    _rect    : TRect           ;
    _state   : TOwnerDrawState ;
    _class   : Char            ;
    _caption : String          ;
    _value   : String
  ) ;
  var
    r    : TRect;
    sitm : String ;
    bmp  : TGIS_Bitmap ;
    tf   : TTextFormat ;
  begin
    _control.Canvas.Pen.Width   := 1;
    _control.Canvas.Brush.Style := bsSolid ;
    _control.Canvas.Pen.Color   := _control.Canvas.Font.Color ;

    r := _rect;
    _control.Canvas.FillRect(r);
    InflateRect(r, -1, -1);

    case _class of
      'C' : begin
              if BiDiMode = bdRightToLeft then
                tf := [tfRtlReading,tfRight]
              else
                tf := [] ;
              r.Left  := r.Left  + 4 ;
              r.Right := r.Right - 4 ;
              r.Top   := r.Top   + 4 ;
              _control.Canvas.TextRect( r, _caption, tf ) ;
            end ;
      'B','s' :
            begin
              if assigned( FGetBitmapEvent ) then begin
                sitm := ClassName + '_' +
                        _value + '_' +
                        IntToStr( r.Width  ) + '_' +
                        IntToStr( r.Height ) + '_' +
                        IntToStr( _control.Canvas.Font.Color ) ;

                if not oItemCache.TryGetValue( sitm, bmp ) then begin
                  bmp := FGetBitmapEvent( _control,
                                          _value,
                                          r.Width, r.Height
                                        ) ;
                  oItemCache.Add( sitm, bmp );
                end;

                if not TGIS_Bitmap.IsNilOrEmpty( bmp ) then begin
                  if odDisabled in _state then
                    _control.Canvas.Draw(
                      r.Left,
                      r.Top,
                      TBitmap( bmp.NativeBitmap ),
                      128
                    )
                  else
                    _control.Canvas.Draw(
                      r.Left,
                      r.Top,
                      TBitmap( bmp.NativeBitmap ),
                      255
                    ) ;
                end;
              end ;
            end;
    end ;
    Hint := _caption ;
  end ;

  function TGIS_SymbolComboBox.fget_SymbolValue : String ;
  begin
    Result := comboExt.Value ;
  end ;

  procedure TGIS_SymbolComboBox.fset_SymbolValue(
    const _value : String
  ) ;
  begin
    comboExt.Value := _value ;
  end ;

  constructor TGIS_ShieldComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;
    ItemHeight := 24 ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Shield', 3 ) ;
    comboExt.ValueEvent  := doValueSymbol ;
    comboExt.RenderEvent := doRenderSymbol ;
    comboExt.CustomEvent := doCustomSymbol ;

    IComboExt := comboExt ;

    ShowHint := True ;
  end ;


  procedure TGIS_ShieldComboBox.Fill ;
  begin
    comboExt.BeginFill ;
    try
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'B', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_LEGEND_PRM_BRUSH_CLEAR, 'nil'
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_LEGEND_PRM_SYMBOL + '...', GIS_PARAMTXT_TYPE_SYMBOL
        )
      ) ;

      ItemIndex := 0 ;
    finally
      comboExt.EndFill ;
    end ;
  end ;

  function TGIS_ShieldComboBox.doValueSymbol(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    s1, s2, s3 : String ;
  begin
    try
      SplitParamAsText( _value, s1, s2, s3 );

      if not IsStringEmpty(_value) then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'B',
                    TGIS_ComboBoxHelperPosition.Lru,
                    _value,
                    _value
                  )
      else
        Result := '' ;
    except
      Result := '' ;
    end;
  end ;


  function TGIS_ShieldComboBox.doCustomSymbol(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    res : String ;
  begin
    if assigned( FCustomEvent ) then begin
      res := FCustomEvent( self, _value ) ;
      if not IsStringEmpty(res) then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'B',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else
        Result := '' ;
    end ;
  end ;

  procedure TGIS_ShieldComboBox.doRenderSymbol(
    _control : TComboBox       ;
    _rect    : TRect           ;
    _state   : TOwnerDrawState ;
    _class   : Char            ;
    _caption : String          ;
    _value   : String
  ) ;
  var
    r    : TRect;
    sitm : String ;
    bmp  : TGIS_Bitmap ;
    tf   : TTextFormat ;
  begin
    _control.Canvas.Pen.Width   := 1;
    _control.Canvas.Brush.Style := bsSolid ;
    _control.Canvas.Pen.Color   := _control.Canvas.Font.Color ;

    r := _rect;
    _control.Canvas.FillRect(r);
    InflateRect(r, -1, -1);

    case _class of
      'C' : begin
              if BiDiMode = bdRightToLeft then
                tf := [tfRtlReading,tfRight]
              else
                tf := [] ;
              r.Left  := r.Left  + 4 ;
              r.Right := r.Right - 4 ;
              r.Top   := r.Top   + 4 ;
              _control.Canvas.TextRect( r, _caption, tf ) ;
            end ;
      'B','s' :
            begin
              if assigned( FGetBitmapEvent ) then begin
                sitm := ClassName + '_' +
                        _value + '_' +
                        IntToStr( r.Width  ) + '_' +
                        IntToStr( r.Height ) + '_' +
                        IntToStr( _control.Canvas.Font.Color ) ;

                if not oItemCache.TryGetValue( sitm, bmp ) then begin
                  bmp := FGetBitmapEvent( _control,
                                          _value,
                                          r.Width, r.Height
                                        ) ;
                  oItemCache.Add( sitm, bmp );
                end;

                if not TGIS_Bitmap.IsNilOrEmpty( bmp ) then begin
                  if odDisabled in _state then
                    _control.Canvas.Draw(
                      r.Left,
                      r.Top,
                      TBitmap( bmp.NativeBitmap ),
                      128
                    )
                  else
                    _control.Canvas.Draw(
                      r.Left,
                      r.Top,
                      TBitmap( bmp.NativeBitmap ),
                      255
                    ) ;
                end;
              end ;
            end;
    end ;
    Hint := _caption ;
  end ;

  function TGIS_ShieldComboBox.fget_SymbolValue : String ;
  begin
    Result := comboExt.Value ;
  end ;

  procedure TGIS_ShieldComboBox.fset_SymbolValue(
    const _value : String
  ) ;
  begin
    comboExt.Value := _value ;
  end ;

  constructor TGIS_FieldValueComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;
    Visible := False ;

    self.Parent := TWinControl( _owner )  ;
    Style := csDropDown ;

    edtBox := TEdit.Create( _owner ) ;
    edtBox.Parent := TWinControl( _owner ) ;
    edtBox.BorderStyle := bsNone ;

    edtBox.OnKeyDown  := doEdtKeyDown;
    edtBox.OnKeyUp    := doEdtKeyUp;
    edtBox.OnKeyPress := doEdtKeyPress;
    edtBox.OnMouseUp  := doEdtMouseDown;
    edtBox.OnChange   := doEdtChange ;

    OnResize  := doResize ;

    OnKeyDown := doComboKeyDown ;
    inherited OnChange  := doComboChange ;

    comboExt := TGIS_ComboBoxHelper.Create(
                  Self, Self.Name + '_FieldValue', 0
                ) ;
    comboExt.ValueEvent  := doValue ;
    IComboExt := comboExt ;

    iPos := -1 ;

    ShowHint := True ;
    Visible := True ;
  end ;

  procedure TGIS_FieldValueComboBox.doResize(
    _sender  : TObject
  ) ;
  var
    r   : TRect         ;
    cmb : TComboBox     ;
    inf : TComboBoxInfo ;
  begin
    inherited;

    inf.cbSize := Sizeof( inf ) ;
    GetComboBoxInfo( Handle, inf ) ;

    edtBox.Left   := inf.rcItem.Left + Left ;
    edtBox.Top    := inf.rcItem.Top  + Top  ;
    edtBox.Width  := inf.rcItem.Width  ;
    edtBox.Height := inf.rcItem.Height ;
  end;


  function TGIS_FieldValueComboBox.doValue(
    _sender  : TObject ;
    _string : String
  ) : String;
  begin
    Result := TGIS_ComboBoxHelper.PrepareItem(
                False,
                'F',
                TGIS_ComboBoxHelperPosition.Top,
                _string,
                _string
              ) ;
  end;


  procedure TGIS_FieldValueComboBox.doEdtKeyDown(
       _sender : TObject ;
   var _key    : Word    ;
       _shift  : TShiftState
  ) ;
  begin
    if _key = VK_DOWN then begin
      PostMessage( Handle, WM_KEYDOWN, _key, 0 );
      _key := 0 ;
    end
    else if DroppedDown then begin
      PostMessage( Handle, WM_KEYDOWN, _key, 0 );
      _key := 0 ;
    end;
  end;

  procedure TGIS_FieldValueComboBox.doEdtKeyUp(
       _sender : TObject ;
   var _key    : Word    ;
       _shift  : TShiftState
  ) ;
  begin
    iPos := edtBox.SelStart;
    iSel := edtBox.SelLength;
  end;

  procedure TGIS_FieldValueComboBox.doEdtKeyPress(
        _sender : TObject ;
    var _key    : Char
  );
  begin
    if DroppedDown then
      _key := Char(0) ; // ignore key pressed upon dropdown
  end;

  procedure TGIS_FieldValueComboBox.doEdtMouseDown(
    _sender : TObject  ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Integer  ;
    _y      : Integer
  ) ;
  begin
    ipos := edtBox.SelStart;
    isel := edtBox.SelLength;
  end;

  procedure TGIS_FieldValueComboBox.doEdtChange(
    _sender : TObject
  ) ;
  begin
    if Assigned( FOnChange ) then
      FOnChange( _sender )
  end;

  procedure TGIS_FieldValueComboBox.doComboChange(
    _sender : TObject
  ) ;
  var
    i     : Integer ;
    s_tmp : String  ;
    s_org : String  ;
  begin
    s_tmp := edtBox.Text ;

    if iPos < 0 then
      iPos := Length( s_tmp ) ;

    Delete( s_tmp, iPos+1, isel ) ;

    Insert( comboExt.Value, s_tmp, ipos + 1 )  ;
    edtBox.SetFocus ;
    edtBox.Text := s_tmp ;
    edtBox.SelStart := iPos + length( comboExt.Value ) ;

    iPos := Length( s_tmp ) ;

    ItemIndex := -1 ;
  end;

  procedure TGIS_FieldValueComboBox.doComboKeyDown(
       _sender : TObject ;
   var _key    : Word    ;
       _shift  : TShiftState
  ) ;
  var
    i : Integer ;
    s : String ;
  begin
    if _key < 65 then exit ;

    // locate item based on first letter pressed
    for i := ItemIndex + 1 to Items.Count -1  do begin
      s := split( Items[i], ['|'] )[1] ;
      if UpperCase( s[1] ) = UpperCase( Char( _key ) ) then begin
        ItemIndex := i ;
        exit ;
      end;
    end;
    for I := 0 to ItemIndex  do begin
      s := split(Items[i], ['|'] )[1] ;
      if UpperCase( s[1] ) = UpperCase( Char( _key ) ) then begin
        ItemIndex := i ;
        exit ;
      end;
    end;
  end;

  function TGIS_FieldValueComboBox.fget_EdtValue : String ;
  begin
    Result := edtBox.Text ;
  end;

  procedure TGIS_FieldValueComboBox.fset_EdtValue(
    const _value : String
  ) ;
  begin
    edtBox.Text := _value ;
  end;

  procedure TGIS_FieldValueComboBox.Fill(
    const _lst   : TStringList
  )   ;
  var
    s : String ;
  begin
    comboExt.BeginFill ;
    try
      for s in _lst do begin
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
             False,
             'F',
             TGIS_ComboBoxHelperPosition.Top,
             s, '{' + s + '}'
          )
        ) ;
      end ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem( False, 'F', TGIS_ComboBoxHelperPosition.Top, GIS_RS_CONTROL_VARIOUS_CBH_BOLD, '<b></b>' )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem( False, 'F', TGIS_ComboBoxHelperPosition.Top, GIS_RS_CONTROL_VARIOUS_CBH_ITALIC, '<i></i>' )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem( False, 'F', TGIS_ComboBoxHelperPosition.Top, GIS_RS_CONTROL_VARIOUS_CBH_UNDERLINE, '<u></u>' )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem( False, 'F', TGIS_ComboBoxHelperPosition.Top, GIS_RS_CONTROL_VARIOUS_CBH_FONT, '<font color="FF0000"></font>' )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem( False, 'F', TGIS_ComboBoxHelperPosition.Top, GIS_RS_CONTROL_VARIOUS_CBH_NEW_LINE, '<br>' )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem( False, 'F', TGIS_ComboBoxHelperPosition.Top, GIS_RS_CONTROL_VARIOUS_CBH_HORIZONTAL_LINE, '<hr>' )
      ) ;
    finally
      comboExt.EndFill ;
    end ;
  end ;

  procedure TGIS_FieldValueComboBox.WMPaint(
    var _msg : TWMPaint
  );
  begin
    inherited;

    doResize( self ) ;
  end;

{ TGIS_BitmapButton }

  constructor TGIS_BitmapButton.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;

    FBitmap := TGIS_Bitmap.Create ;

    ShowHint := True ;
  end ;

  destructor TGIS_BitmapButton.Destroy ;
  begin
    FreeObject( FBitmap ) ;

    inherited ;
  end ;

  procedure TGIS_BitmapButton.paintEmpty(
    const _bitmap : TBitmap
  ) ;
  var
    i    : Integer ;
    imax : Integer ;
    k    : Integer ;
    kmax : Integer ;
    blst : Boolean ;
    bkgm : Double ;
    fggm : Double ;
    r    : Byte ;
    g    : Byte ;
    b    : byte ;
    rb   : Byte ;
    gb   : Byte ;
    bb   : Byte ;
    rw   : Byte ;
    gw   : Byte ;
    bw   : Byte ;
    bmix : Cardinal ;
    wmix : Cardinal ;
    clr  : Cardinal ;
  begin
    clr := $00000000 ;
    rb := $C0 ;
    gb := $C0 ;
    bb := $C0 ;
    rw := $88 ;
    gw := $88 ;
    bw := $88 ;

    imax := RoundS( _bitmap.Width  / 8 ) + 1 ;
    kmax := RoundS( _bitmap.Height / 8 ) + 1 ;

    fggm := (( clr shr 24 ) and $FF ) / 255 ;
    bkgm := 1.0 - fggm ;

    r := ( clr shr 16 ) and $FF ;
    g := ( clr shr  8 ) and $FF ;
    b := ( clr        ) and $FF ;

    bmix := ( RoundS( bkgm * rb + fggm * b ) shl 16 ) +
            ( RoundS( bkgm * gb + fggm * g ) shl 8  ) +
            ( RoundS( bkgm * bb + fggm * r )        ) ;
    wmix := ( RoundS( bkgm * rw + fggm * b ) shl 16 ) +
            ( RoundS( bkgm * gw + fggm * g ) shl 8  ) +
            ( RoundS( bkgm * bw + fggm * r )        ) ;

    _bitmap.Canvas.Brush.Style := TBrushStyle.bsSolid ;
    for k := 0 to kmax do begin
      blst := ( k mod 2 ) = 0 ;
      for i := 0 to imax do begin
        if blst then begin
          _bitmap.Canvas.Pen.Color   := TColor( bmix ) ;
          _bitmap.Canvas.Brush.Color := TColor( bmix ) ;
        end
        else begin
          _bitmap.Canvas.Pen.Color   := TColor( wmix ) ;
          _bitmap.Canvas.Brush.Color := TColor( wmix ) ;
        end ;
        _bitmap.Canvas.Rectangle( i*8+1, k*8+1, (i+1)*8+1, (k+1)*8+1 ) ;
        blst := not blst ;
      end ;
    end ;

    _bitmap.Canvas.Pen.Color   := TColor( $888888 ) ;
    _bitmap.Canvas.Brush.Style := TBrushStyle.bsClear ;
    _bitmap.Canvas.Rectangle( 0, 0, _bitmap.Width, _bitmap.Height ) ;
  end ;

  procedure TGIS_BitmapButton.displayBitmap ;
  begin
    if not TGIS_Bitmap.IsNilOrEmpty( FBitmap ) then begin
      Glyph.Assign( TPersistent( FBitmap.NativeBitmap ) ) ;
      Hint := FBitmap.Path ;
    end
    else begin
      Glyph.Assign( nil ) ;
      Glyph.Width  := ClientWidth  - 4 ;
      Glyph.Height := ClientHeight - 4 ;

      paintEmpty( Glyph ) ;

      Hint := '' ;
    end ;
  end ;

  function TGIS_BitmapButton.getBitmap : TGIS_Bitmap ;
  begin
    Result := FBitmap ;
  end ;

  procedure TGIS_BitmapButton.setBitmap(
    const _bmp : TGIS_Bitmap
  ) ;
  begin
    FBitmap.Assign( _bmp ) ;

    displayBitmap ;
  end ;


{ TGIS_FontButton }

  constructor TGIS_FontButton.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;

    FFontEx := TGIS_Font.Create ;
  end ;

  destructor TGIS_FontButton.Destroy ;
  begin
    FreeObject( FFontEx ) ;

    inherited ;
  end ;

  procedure TGIS_FontButton.displayFont ;
  begin
    Font.Name   := FFontEx.Name ;
    Font.Size   := FFontEx.Size ;
    Font.Color  := FFontEx.Color.ToBGR ;
    Font.Style  := VCLFontStyle( FFontEx.Style ) ;

    Caption := Font.Name ;
  end ;

  function TGIS_FontButton.getFontEx : TGIS_Font ;
  begin
    Result := FFontEx ;
  end ;

  procedure TGIS_FontButton.setFontEx(
    const _font : TGIS_Font
  ) ;
  begin
    FFontEx.Assign( _font ) ;

    displayFont ;
  end ;

  constructor TGIS_FontNameComboBox.Create(
    _owner: TComponent
  );
  begin
    inherited Create( _owner ) ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Size', 3 ) ;
    comboExt.ValueEvent  := doValueFont ;
    icomboExt := comboExt ;

    ShowHint := True ;
  end ;

  function TGIS_FontNameComboBox.fget_ValueFont : String ;
  begin
    Result := comboExt.Value ;
  end ;


  procedure TGIS_FontNameComboBox.fset_ValueFont(
    const _value : String
  ) ;
  begin
    comboExt.Value := _value ;
  end ;

  procedure TGIS_FontNameComboBox.Fill ;
  var
    lst : TStringList ;
    str : String ;
  begin
    comboExt.BeginFill ;
    lst := TStringList.Create ;
    try
      GetFontList( lst ) ;
      Items.Clear ;

      for str in lst do begin
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, str, str
          )
        ) ;
      end ;
      ItemIndex := 0 ;
    finally
      FreeObject( lst ) ;
      comboExt.EndFill ;
    end ;
  end ;

  function TGIS_FontNameComboBox.doValueFont(
    _sender  : TObject ;
    _value : String
  ) : String ;
  begin
    try
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  's',
                  TGIS_ComboBoxHelperPosition.Lru,
                  _value,
                  _value
                )
    except
      Result := '' ;
    end;
  end ;

  constructor TGIS_SizeComboBox.Create(
    _owner: TComponent
  );
  begin
    inherited Create( _owner ) ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Size', 3 ) ;
    comboExt.ValueEvent  := doValueSize ;
    comboExt.CustomEvent := doCustomSize ;
    comboExt.RenderEvent := doRenderSize ;
    icomboExt := comboExt ;

    ShowHint := True ;
  end ;


  function TGIS_SizeComboBox.doCustomSize(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    res : String ;
    s1, s2, s3 : String ;
  begin
    if assigned( FCustomEvent ) then begin
      res := FCustomEvent( self, _value ) ;
      SplitParamAsText( res, s1, s2, s3 ) ;

      if s1 =GIS_PARAMTXT_TYPE_SIZE then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    's',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else
      if s1 = GIS_PARAMTXT_TYPE_FIELD then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'f',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else
        Result := '' ;
    end ;
  end ;

  procedure TGIS_SizeComboBox.doRenderSize(
    _control : TComboBox       ;
    _rect    : TRect           ;
    _state   : TOwnerDrawState ;
    _class   : Char            ;
    _caption : String          ;
    _value   : String
  ) ;
  var
    s   : String ;
    s1, s2, s3 : String ;
    tf  : TTextFormat ;
  begin
    SplitParamAsText( _value, s1, s2, s3  );

    s := '' ;
    case _class of
      'f' : begin
              s := s2 + ' * ' + s3;
            end ;
      'r' : begin
              s := _caption ;
            end ;
      'C' : begin
              s := _caption ;
            end ;
      else begin
        if not IsStringEmpty( s2) then
          s := s2
        else
          s := s1
      end;
    end ;

    if BiDiMode = bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [] ;

    _control.Canvas.FillRect( _rect ) ;

    _rect.Left  := _rect.Left  + 2 ;
    _rect.Right := _rect.Right - 2 ;
    _rect.Top   := _rect.Top   + 1 ;

    _control.Canvas.TextRect( _rect, s, tf ) ;

    Hint := _caption ;
  end;

  function TGIS_SizeComboBox.doValueSize(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    s1, s2, s3 : String ;
  begin
    try
      SplitParamAsText( _value, s1, s2, s3 );
      if s1 = GIS_PARAMTXT_TYPE_SIZE then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    's',
                    TGIS_ComboBoxHelperPosition.Lru,
                    _value,
                    _value
                  )
      else
      if s1 = GIS_PARAMTXT_TYPE_RENDERER then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'r',
                    TGIS_ComboBoxHelperPosition.Top,
                    _value,
                    _value
                  )
      else
      if s1 = GIS_PARAMTXT_TYPE_FIELD then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'f',
                    TGIS_ComboBoxHelperPosition.Lru,
                    _value,
                    _value
                  )
      else
        Result := '' ;
    except
      Result := '' ;
    end;
  end ;

  function TGIS_SizeComboBox.fget_SizeValue : String ;
  begin
    Result := comboExt.Value ;
  end ;


  procedure TGIS_SizeComboBox.fset_SizeValue(
    const _value : String
  ) ;
  begin
    comboExt.Value := _value ;
  end ;


  procedure TGIS_SizeComboBox.Fill(
    const _forSymbol  : Boolean ;
    const _forLine    : Boolean ;
    const _field      : Boolean ;
    const _renderer   : Boolean
  ) ;
  begin
    comboExt.BeginFill ;
    try
      if _forSymbol then begin
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '8 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '8pt',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '10 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '10pt',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '12 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '12pt',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '16 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '16pt',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '24 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '24pt',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '32 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '32pt',
              ''
            )
          )
        ) ;
      end
      else if _forLine then begin
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, 'HAIR',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              'HAIR',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '1 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '1pt',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '2 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '2pt',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '4 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '4pt',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '6 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '6pt',
              ''
            )
          )
        ) ;
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '10 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '10pt',
              ''
            )
          )
        ) ;
      end
      else begin
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 's', TGIS_ComboBoxHelperPosition.Top, '0 pt',
            ConstructParamAsText(
              GIS_PARAMTXT_TYPE_SIZE,
              '0pt',
              ''
            )
          )
        ) ;
        comboExt.LoadState ;
        if Items.Count = 1 then begin
          comboExt.SetItem(
            TGIS_ComboBoxHelper.PrepareItem(
              False, 's', TGIS_ComboBoxHelperPosition.Top, '1 pt',
              ConstructParamAsText(
                GIS_PARAMTXT_TYPE_SIZE,
                '1pt',
                ''
              )
            )
          ) ;
          comboExt.SetItem(
            TGIS_ComboBoxHelper.PrepareItem(
              False, 's', TGIS_ComboBoxHelperPosition.Top, '2 pt',
              ConstructParamAsText(
                GIS_PARAMTXT_TYPE_SIZE,
                '2pt',
                ''
              )
            )
          ) ;
          comboExt.SetItem(
            TGIS_ComboBoxHelper.PrepareItem(
              False, 's', TGIS_ComboBoxHelperPosition.Top, '3 pt',
              ConstructParamAsText(
                GIS_PARAMTXT_TYPE_SIZE,
                '3pt',
                ''
              )
            )
          ) ;
          comboExt.SetItem(
            TGIS_ComboBoxHelper.PrepareItem(
              False, 's', TGIS_ComboBoxHelperPosition.Top, '4 pt',
              ConstructParamAsText(
                GIS_PARAMTXT_TYPE_SIZE,
                '4pt',
                ''
              )
            )
          ) ;
          comboExt.SetItem(
            TGIS_ComboBoxHelper.PrepareItem(
              False, 's', TGIS_ComboBoxHelperPosition.Top, '5 pt',
              ConstructParamAsText(
                GIS_PARAMTXT_TYPE_SIZE,
                '5pt',
                ''
              )
            )
          ) ;
        end ;

      end;

      if _renderer then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 'r', TGIS_ComboBoxHelperPosition.Bottom,
            GIS_RS_GENERAL_BYRENDERER, GIS_PARAMTXT_TYPE_RENDERER
          )
        ) ;

      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
        )
      ) ;

      if _field then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
            GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
          )
        ) ;

      ItemIndex := 0 ;
    finally
      comboExt.EndFill ;
    end ;
  end ;

  procedure TGIS_SizeComboBox.FillRealWorldUnits(
    const _field : Boolean
  ) ;
  begin
    comboExt.BeginFill ;
    try
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '0 m',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '0m',
            ''
          )
        )
      ) ;

      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
        )
      ) ;

      if _field then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
            GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
          )
        ) ;
    finally
      comboExt.EndFill ;
    end ;

  end ;

  procedure TGIS_SizeComboBox.FillAggregation ;
  begin
    comboExt.BeginFill ;
    try
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '20 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '20 pt',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '40 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '40 pt',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '60 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '60 pt',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '80 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '80 pt',
            ''
          )
        )
      ) ;

      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
        )
      ) ;
    finally
      comboExt.EndFill ;
    end ;

  end ;

  procedure TGIS_SizeComboBox.FillSnap ;
  begin
    comboExt.BeginFill ;
    try
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '0 cm',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '0cm',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '2 mm',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '2mm',
            ''
          )
        )
      ) ;

      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
        )
      ) ;
    finally
      comboExt.EndFill ;
    end ;

  end ;

  constructor TGIS_RotationComboBox.Create(
    _owner: TComponent
  );
  begin
    inherited Create( _owner ) ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Rotation', 3 ) ;
    comboExt.ValueEvent  := doValueRotation ;
    comboExt.CustomEvent := doCustomRotation ;
    comboExt.RenderEvent := doRenderRotation ;
    icomboExt := comboExt ;

    ShowHint := True ;
  end ;


  function TGIS_RotationComboBox.doCustomRotation(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    res : String ;
    s1, s2, s3 : String ;
  begin
    if assigned( FCustomEvent ) then begin
      res := FCustomEvent( self, _value ) ;

      SplitParamAsText( res, s1, s2, s3 );
      if s1 = GIS_PARAMTXT_TYPE_ANGLE then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    's',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else
      if s1 = GIS_PARAMTXT_TYPE_FIELD then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'f',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else
        Result := '' ;
    end ;
  end ;

  procedure TGIS_RotationComboBox.doRenderRotation(
    _control : TComboBox       ;
    _rect    : TRect           ;
    _state   : TOwnerDrawState ;
    _class   : Char            ;
    _caption : String          ;
    _value   : String
  ) ;
  var
    s : String ;
    s1, s2, s3 : String ;
    tf : TTextFormat ;
  begin
    SplitParamAsText( _value, s1, s2, s3 );

    s := '' ;
    case _class of
      'f' : begin
              s := s2 + ' * ' + s3;
            end ;
      'C' : begin
              s := _caption ;
            end ;
      else begin
        if not IsStringEmpty( s2) then
          s := s2
        else
          s := s1
      end;
    end ;

    if BiDiMode = bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [] ;

    _control.Canvas.FillRect( _rect ) ;
    _rect.Left  := _rect.Left  + 2 ;
    _rect.Right := _rect.Right - 2 ;
    _rect.Top   := _rect.Top   + 1 ;

    _control.Canvas.TextRect( _rect, s, tf ) ;

    Hint := _caption ;
  end ;

  function TGIS_RotationComboBox.doValueRotation(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    s1, s2, s3 : String ;
  begin
    try
      SplitParamAsText( _value, s1, s2, s3 );

      if s1 =GIS_PARAMTXT_TYPE_ANGLE then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    's',
                    TGIS_ComboBoxHelperPosition.Top,
                    _value,
                    _value
                  )
      else
      if not IsStringEmpty(_value) then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    's',
                    TGIS_ComboBoxHelperPosition.Lru,
                    _value,
                    _value
                  )
      else
        Result := '' ;
    except
      Result := '' ;
    end;
  end ;

  function TGIS_RotationComboBox.fget_RotationValue : String ;
  begin
    Result := comboExt.Value ;
  end ;


  procedure TGIS_RotationComboBox.fset_RotationValue(
    const _value : String
  ) ;
  begin
    comboExt.Value := _value ;
  end ;


  procedure TGIS_RotationComboBox.Fill(
    _field : Boolean
  ) ;
  begin
    comboExt.BeginFill ;
    try
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '-180 deg',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_ANGLE,
            '-180deg',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '-135 deg',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_ANGLE,
            '-135deg',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '-90 deg',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_ANGLE,
            '-90deg',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '-45 deg',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_ANGLE,
            '-45deg',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '0 deg',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_ANGLE,
            '0deg',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '45 deg',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_ANGLE,
            '45deg',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '90 deg',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_ANGLE,
            '90deg',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '135 deg',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_ANGLE,
            '135deg',
            ''
          )
        )
      ) ;
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 's', TGIS_ComboBoxHelperPosition.Top, '180 deg',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_ANGLE,
            '180deg',
            ''
          )
        )
      ) ;

      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
        )
      ) ;

      if _field then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
            GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
          )
        ) ;

      ItemIndex := 0 ;
    finally
      comboExt.EndFill ;
    end ;
  end ;

procedure freeItemCache ;
var
  pair : TPair< String, TGIS_Bitmap > ;
begin
  for pair in oItemCache do
    FreeObjectNotNil( pair.Value );

  FreeObject( oItemCache );
end;

initialization
  oItemCache := TDictionary<String,TGIS_Bitmap>.Create  ;

finalization
  freeItemCache ;

//==================================== END =====================================
end.


