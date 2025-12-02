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

unit FMX.GisControlVarious ;
{$HPPEMIT '#pragma link "FMX.GisControlVarious"'}

{$INCLUDE GisInclude.inc}

interface

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.Classes,
  System.Types,
  System.UITypes,
  System.SysUtils,
  FMX.Forms,
  FMX.Objects,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Graphics,
  FMX.ListBox,
  FMX.Grid,

  GisRtl,
  GisTypesUI,
  GisTypes,
  GisCsBase,
  GisParams,
  FMX.GisFramework,
  FMX.GisComboBox,
  FMX.GisComboBoxHelper ;

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
  [ComponentPlatformsAttribute( pfidAllDesktops )]
  TGIS_SpinEdit = class( TEdit )

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
      procedure SetEnabled            ( const _value : Boolean
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
      oButton     : TSpinEditButton ;
      tmpText     : String  ;
      tmpValue    : Double  ;
      iCreated    : Boolean ;
      oFont       : TFont   ;
      uponDestroy : Boolean ;

    protected
      procedure RecalcSize ; override ;

      function GetDefaultStyleLookupName: string; override;

      procedure doDestroy      ;

      /// <summary>
      ///   Increment/Decrement click handler.
      /// </summary>
      procedure doClickUp       ( _sender : TObject
                              ) ;

      /// <summary>
      ///   Increment/Decrement click handler.
      /// </summary>
      procedure doClickDown       ( _sender : TObject
                              ) ;
    public

      /// <summary>
      ///   Key event handler. For up/down handling.
      /// </summary>
      procedure KeyDown      ( var Key: Word;
                               var KeyChar: WideChar;
                               Shift: TShiftState
                             ) ; override;

      /// <summary>
      ///   Change value handler. Do value validation etc.
      /// </summary>
      procedure Change(  _sender : TObject )       ;

    public
      /// <summary>
      ///   Create the control.
      /// </summary>
      {#ownership:_owner:ownif_empty}
      constructor Create     ( _owner: TComponent
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

  end ;
{$ENDIF}

  {#gendoc:hide}
  TGIS_Bevel = TRectangle ;

  /// <summary>
  ///   String Grid with support to OnColumnResize.
  /// </summary>
  TGIS_StringGrid = class ( TStringGrid )
    private
      FOnColumnResize : TNotifyEvent ;

    public

      /// <summary>
      ///   See documentation for TStringGrid in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TStringGrid in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create    ( _owner : TComponent
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
      FControl     : TControl ;
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

      procedure fset_FieldControl   ( const _control : TControl
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
                                    _x            : Single ;
                                    _y            : Single
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
      procedure SetEnabled        ( const _value        : Boolean
                                  ) ; override;

    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Destroy the control.
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
      property FieldControl  : TControl
                               read  FControl
                               write fset_FieldControl ;

      /// <event/>
      property OnHelp        : TGIS_HelpEvent
                               read  FOnHelp
                               write FOnHelp ;
  end ;


  /// <summary>
  ///   Base class for all combo boxes which introduces a mechanism
  ///   for delayed update.
  /// </summary>
  TGIS_ComboBoxAbstract = class ( TGIS_ComboBox )
    protected
      /// <inheritdoc/>
      function GetDefaultStyleLookupName : String ; override;

      /// <summary>
      ///   See documentation for TGIS_ComboBox in Delphi help.
      /// </summary>
      procedure DoApplyStyleLookup ; override ;
    public
      /// <summary>
      ///   Delayed update routine.
      /// </summary>
      /// <param name="_val">
      ///   string value which defines the update action
      /// </param>
      procedure DelayedUpdate ( const _val : String
                              ) ; virtual; abstract;
  end ;


  /// <summary>
  ///   Combobox with pen styles.
  /// </summary>
  TGIS_PenStyleComboBox = class ( TGIS_ComboBoxAbstract )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
      FOnChange       : TNotifyEvent ;
    private
      function  doValuePenStyle ( _sender    : TObject      ;
                                  _value     : String
                                ) : String ;
      function  doCustomPenStyle( _sender    : TObject      ;
                                  _value     : String
                                ) : String ;
      procedure doRenderPenStyle( _item      : TListBoxItem ;
                                  _canvas    : TCanvas      ;
                                  _rect      : TRectF       ;
                                  _font      : TFont        ;
                                  _color     : TAlphaColor  ;
                                  _class     : Char         ;
                                  _caption   : String       ;
                                  _value     : String
                                ) ;
      function  fget_PenValue   : String ;
      procedure fset_PenValue   ( const _value : String
                                ) ;
      procedure fset_OnChange ( _event : TNotifyEvent
                               ) ;


    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
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

    public
      /// <inheritdoc/>
      procedure DelayedUpdate ( const _val : String
                              ) ; override;

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
      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write fset_OnChange ;

  end;

  /// <summary>
  ///   Combobox with brush styles.
  /// </summary>
  TGIS_PatternComboBox = class ( TGIS_ComboBoxAbstract )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
      FOnChange       : TNotifyEvent ;
    private
      function  doValueBrushStyle ( _sender    : TObject      ;
                                    _value     : String
                                  ) : String ;
      function  doCustomBrushStyle( _sender    : TObject      ;
                                    _value : String
                                  ) : String ;
      procedure doRenderBrushStyle( _item      : TListBoxItem ;
                                    _canvas    : TCanvas      ;
                                    _rect      : TRectF       ;
                                    _font      : TFont        ;
                                    _color     : TAlphaColor  ;
                                    _class     : Char         ;
                                    _caption   : String       ;
                                    _value     : String
                                  ) ;
      function  fget_BrushValue   : String ;
      procedure fset_BrushValue   ( const _value : String
                                  ) ;
      procedure fset_OnChange ( _event : TNotifyEvent
                               ) ;

    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
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

    public
      /// <inheritdoc/>
      procedure DelayedUpdate ( const _val : String
                              ) ; override;

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
                                  read  fget_BrushValue
                                  write fset_BrushValue ;

      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write fset_OnChange ;

  end;

  /// <summary>
  ///   Combobox with shields.
  /// </summary>
  TGIS_ShieldComboBox = class ( TGIS_ComboBoxAbstract )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
      FOnChange       : TNotifyEvent ;
    private
      function  doValueSymbol     ( _sender    : TObject      ;
                                    _value     : String
                                  ) : String ;
      function  doCustomSymbol    ( _sender    : TObject      ;
                                    _value : String
                                  ) : String ;
      procedure doRenderSymbol    ( _item      : TListBoxItem ;
                                    _canvas    : TCanvas      ;
                                    _rect      : TRectF       ;
                                    _font      : TFont        ;
                                    _color     : TAlphaColor  ;
                                    _class     : Char         ;
                                    _caption   : String       ;
                                    _value     : String
                                  ) ;
      function  fget_SymbolValue   : String ;
      procedure fset_SymbolValue   ( const _value : String
                                   ) ;
      procedure fset_OnChange      ( _event    : TNotifyEvent
                                   ) ;

    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      procedure Fill              ;

    public
      /// <inheritdoc/>
      procedure DelayedUpdate ( const _val : String
                              ) ; override;

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

      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write fset_OnChange ;

  end;

  /// <summary>
  ///   Combobox with font names.
  /// </summary>
  TGIS_FontNameComboBox = class ( TGIS_ComboBoxAbstract )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FOnChange       : TNotifyEvent ;
    private
      function  doValueFont    ( _sender : TObject      ;
                                  _value : String
                               ) : String ;
      function  fget_ValueFont : String ;
      procedure fset_ValueFont ( const _value : String
                               ) ;
      procedure fset_OnChange  ( _event : TNotifyEvent
                               ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      procedure Fill              ;

    public
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

      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write fset_OnChange ;
  end ;


  /// <summary>
  ///   Combobox with sizes.
  /// </summary>
  TGIS_SizeComboBox = class ( TGIS_ComboBoxAbstract )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FOnChange       : TNotifyEvent ;
    private
      function  doValueSize    ( _sender    : TObject      ;
                                  _value    : String
                               ) : String ;
      function  doCustomSize   ( _sender    : TObject      ;
                                  _value    : String
                               ) : String ;
      procedure doRenderSize   ( _item      : TListBoxItem ;
                                 _canvas    : TCanvas      ;
                                 _rect      : TRectF       ;
                                 _font      : TFont        ;
                                 _color     : TAlphaColor  ;
                                 _class     : Char         ;
                                 _caption   : String       ;
                                 _value     : String
                               ) ;
      function  fget_SizeValue : String ;
      procedure fset_SizeValue ( const _value : String
                               ) ;
      procedure fset_OnChange ( _event : TNotifyEvent
                               ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( _owner        : TComponent
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
      procedure Fill              ( const _forSymbol : Boolean ;
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

    public
      /// <inheritdoc/>
      procedure DelayedUpdate ( const _val : String
                              ) ; override;

    public
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

      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write fset_OnChange ;
  end ;

  /// <summary>
  ///   Combobox with rotations.
  /// </summary>
  TGIS_RotationComboBox = class ( TGIS_ComboBoxAbstract )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FOnChange       : TNotifyEvent ;
    private
      function  doValueRotation    ( _sender    : TObject      ;
                                     _value     : String
                                   ) : String ;
      function  doCustomRotation   ( _sender    : TObject      ;
                                     _value     : String
                                   ) : String ;
      procedure doRenderRotation   ( _item      : TListBoxItem ;
                                     _canvas    : TCanvas      ;
                                     _rect      : TRectF       ;
                                     _font      : TFont        ;
                                     _color     : TAlphaColor  ;
                                     _class     : Char         ;
                                     _caption   : String       ;
                                     _value     : String
                                   ) ;
      function  fget_RotationValue : String ;
      procedure fset_RotationValue ( const _value : String
                                   ) ;
      procedure fset_OnChange ( _event : TNotifyEvent
                               ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      /// <param name="_field">
      ///   if values from fields
      /// </param>
      procedure Fill              ( _field        : Boolean
                                  ) ;

    public
      /// <inheritdoc/>
      procedure DelayedUpdate ( const _val : String
                              ) ; override;

    public
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

      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write fset_OnChange ;
  end ;

  /// <summary>
  ///   Combobox with marker styles.
  /// </summary>
  TGIS_SymbolComboBox = class ( TGIS_ComboBoxAbstract )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
      FOnChange       : TNotifyEvent ;
    private
      function  doValueSymbol    ( _sender    : TObject      ;
                                   _value     : String
                                 ) : String ;
      function  doCustomSymbol   ( _sender    : TObject      ;
                                   _value     : String
                                 ) : String ;
      procedure doRenderSymbol   ( _item      : TListBoxItem ;
                                   _canvas    : TCanvas      ;
                                   _rect      : TRectF       ;
                                   _font      : TFont        ;
                                   _color     : TAlphaColor  ;
                                   _class     : Char         ;
                                   _caption   : String       ;
                                   _value     : String
                                 ) ;
      function  fget_SymbolValue : String ;
      procedure fset_SymbolValue ( const _value : String
                                 ) ;
      procedure fset_OnChange ( _event : TNotifyEvent
                               ) ;

    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the list.
      /// </summary>
      procedure Fill ;

    public
      /// <inheritdoc/>
      procedure DelayedUpdate ( const _val : String
                              ) ; override;

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

      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write fset_OnChange ;

  end ;

  /// <summary>
  ///   Combobox with with fields
  /// </summary>
  TGIS_FieldValueComboBox = class( TGIS_ComboBoxAbstract )
    private
      edtBox          : TEdit ;
      btnBox          : TSpeedButton ;
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      iPos,
      iSel            : Integer ;

      FOnChange       : TNotifyEvent ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FGetBitmapEvent : TGIS_ComboBoxHelperGetBitmapEvent ;
    private
      function  doValue        (        _sender : TObject ;
                                        _string : String
                               ) : String ;

      procedure doEdtKeyDown   (       _sender : TObject  ;
                                 var   _key    : Word     ;
                                 var   _keyChar: WideChar ;
                                       _shift  : TShiftState
                               ) ;
      procedure doEdtKeyUp     (       _sender : TObject  ;
                                 var   _key    : Word     ;
                                 var   _keyChar: WideChar ;
                                       _shift  : TShiftState
                               ) ;
      procedure doEdtKeyPress  (       _sender : TObject  ;
                                 var   _key    : Char
                               ) ;
      procedure doEdtMouseDown (       _sender : TObject  ;
                                       _button : TMouseButton ;
                                       _shift  : TShiftState  ;
                                       _x      : Single  ;
                                       _y      : Single
                               ) ;
      procedure doEdtChange    (       _sender : TObject);
      procedure doComboChange  (       _sender : TObject);
      procedure doComboKeyDown (       _sender : TObject  ;
                                 var   _key    : Word     ;
                                 var   _keyChar: WideChar ;
                                       _shift  : TShiftState
                               ) ;
      procedure doBtnClick     (       _sender : TObject
                               );

      function  fget_EdtValue  : String ;
      procedure fset_EdtValue  ( const _value   : String
                               ) ;
    protected

      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      procedure Resize         ; override;

    public

      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
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

    public
      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write FOnChange ;
      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;

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
      {#ownership:_owner:ownif_empty}
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
  FMX.BehaviorManager,
  PVL.GisControlFieldFactor,
  PVL.GisPvl,
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
  {$IFDEF MSWINDOWS}
      screen : THandle ;
      h,v    : Double  ;
  {$ELSE}
    {$IFDEF LEVEL_XE7_FMX}
        dev : IDeviceBehavior;
        met : TDeviceDisplayMetrics;
    {$ENDIF}
  {$ENDIF}
    oval      : Double ;
    pxperinch : Integer ;
  begin
    pxperinch := 96 ;
    {$IFDEF MSWINDOWS}
      SetProcessDPIAware(); //true
      screen := GetDC(0);
      h := GetDeviceCaps(screen,LOGPIXELSX);
      v := GetDeviceCaps(screen,LOGPIXELSY);
      ReleaseDC(0, screen);
      pxperinch := RoundS( ( h+v ) / 2 ) ;
    {$ELSE}

    {$ENDIF}

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

{$IFNDEF GENDOC}
//==============================================================================
// TGIS_SpinEdit
//==============================================================================

  constructor TGIS_SpinEdit.Create( _owner: TComponent ) ;
  begin
    inherited Create( _owner ) ;

    FMode      := TGIS_SpinEditMode.spinVALUE ;
    FSizeUnits := TGIS_SpinEditUnits.PT    ;
    FMinVal    := 0   ;
    FMaxVal    := 100 ;
    FIncrement := 0   ;
    FPrecision := 0   ;
    FValue     := 0   ;

    iCreated   := False ;

    oButton := TSpinEditButton.Create( Self );
    oButton.Parent := Self ;
    with oButton do begin
      Height       := Height  ;
      Visible      := True    ;
      Width        := 15 ;
      Increment    := 1  ;
      Stored       := False ;
      OnUpClick    := doClickUp ;
      OnDownClick  := doClickDown ;
    end ;

    oFont := TFont.Create ;
    oFont.Assign( Font ) ;
    uponDestroy := False ;

    OnChangeTracking := Change ;

    ClipChildren := True ;
    oButton.Visible := FIncrement <> 0 ;
    iCreated := True ;

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
      TextSettings.FontColor := TAlphaColorRec.Red      ;
    end
    else begin
      Result     := True         ;
      TextSettings.FontColor := TAlphaColorRec.White ;
    end ;
  end ;

  procedure TGIS_SpinEdit.RecalcSize ;
  begin
    // avoid AV
  end;


  function TGIS_SpinEdit.GetDefaultStyleLookupName: string;
  begin
    Result := 'editstyle';
  end ;

  procedure TGIS_SpinEdit.SetEnabled( const _value : Boolean ) ;
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
    oButton.Visible := FIncrement <> 0 ;
  end ;

  procedure TGIS_SpinEdit.fset_Precision( const _value : Integer ) ;
  begin
    FPrecision := _value ;
    Repaint ;
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

  procedure TGIS_SpinEdit.doClickUp( _sender: TObject ) ;
  var
    fact : Double ;
    oval : Double ;
  begin
    if  FIncrement = 0 then exit ;

    fact := 1 / FIncrement ;

    oval := RoundS( FValue * fact + 1 ) / fact ;

    if oval < FMinVal then oval := FMinVal ;
    if oval > FMaxVal then oval := FMaxVal ;

    Value := oval ;
  end ;

  procedure TGIS_SpinEdit.doClickDown( _sender: TObject ) ;
  var
    fact : Double ;
    oval : Double ;
  begin
    if  FIncrement = 0 then exit ;

    fact := 1 / FIncrement ;

    oval := RoundS( FValue * fact - 1 ) / fact ;

    if oval < FMinVal then oval := FMinVal ;
    if oval > FMaxVal then oval := FMaxVal ;

    Value := oval ;
  end ;


  procedure TGIS_SpinEdit.KeyDown( var Key: Word;
                               var KeyChar: WideChar;
                               Shift: TShiftState ) ;
  begin
    if      Key = vkUp   then doClickUp( Self  )
    else if Key = vkDown then doClickDown( Self ) ;
    inherited KeyDown( Key, KeyChar, Shift ) ;
  end ;


  procedure TGIS_SpinEdit.Change(  _sender : TObject ) ;
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
      TextSettings.FontColor := TAlphaColorRec.White ;
    except
      TextSettings.FontColor := TAlphaColorRec.Red ;
    end ;

    {$IFDEF LEVEL_XE7_FMX}
    inherited ;
    {$ENDIF}
  end ;
{$ENDIF}


//==============================================================================
// TGIS_StringGrid
//==============================================================================

  constructor TGIS_StringGrid.Create( _owner : TComponent ) ;
  begin
    inherited ;

  end ;

  procedure TGIS_StringGrid.DeleteRowEx  ( _arow   : LongInt ) ;
  begin

  end ;

  procedure TGIS_StringGrid.HideEditor ;
  begin
    {$IFNDEF LEVEL_RX101_FMX}
      inherited HideEditor ;
    {$ENDIF}
  end ;

//==============================================================================
// TGIS_FieldButton
//==============================================================================

  constructor TGIS_FieldButton.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;

    Width     := 21 ;
    Height    := 21 ;

    Text := '...' ;
    OnMouseUp := fieldButtonMouseUp ;

    FPanel := TPanel.Create( self ) ;
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
    _x      : Single ;
    _y      : Single
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
    const _value : Boolean
  ) ;
  begin
    inherited SetEnabled( _value ) ;
    { TODO : Verify if it's still needed }
    if _value then
      //FPanel.Font.Color := clBtnText
    else
      //FPanel.Font.Color := clInactiveBorder ;
  end ;

  procedure TGIS_FieldButton.fset_FieldControl(
    const _control : TControl
  ) ;
  begin
    FControl := _control ;

    if Assigned( _control ) then begin
      FPanel.Position.X   := _control.Position.X   ;
      FPanel.Position.Y    := _control.Position.Y    ;
      FPanel.Width  := _control.Width  ;
      FPanel.Height := _control.Height ;
      FPanel.Parent := Self ;

      Left := _control.Position.X + _control.Width + 2 ;
      Top  := _control.Position.Y ;
    end ;
    FPanel.Visible  := False ;
  end ;

{ TGIS_ComboBoxAbstract }

  function TGIS_ComboBoxAbstract.GetDefaultStyleLookupName : string ;
  begin
    Result := 'comboboxstyle' ;
  end ;

  procedure TGIS_ComboBoxAbstract.DoApplyStyleLookup ;
  var
    pair : TPair< String, TGIS_Bitmap > ;
  begin
    for pair in oItemCache do
      FreeObjectNotNil( pair.Value );
    oItemCache.Clear ;
    inherited ;
  end;

{ TGIS_PenStyleComboBox }

  constructor TGIS_PenStyleComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_PenStyle', 3 ) ;
    comboExt.ValueEvent  := doValuePenStyle ;
    comboExt.RenderEvent := doRenderPenStyle ;
    comboExt.CustomEvent := doCustomPenStyle ;
    IComboExt := comboExt ;

    Height := 28 ;
  end ;


  procedure TGIS_PenStyleComboBox.doRenderPenStyle(
    _item      : TListBoxItem ;
    _canvas    : TCanvas      ;
    _rect      : TRectF       ;
    _font      : TFont        ;
    _color     : TAlphaColor  ;
    _class     : Char         ;
    _caption   : String       ;
    _value     : String
  ) ;
  var
    sitm : String ;
    bmp  : TGIS_Bitmap ;
    str  : String ;
    s1, s2, s3 : String ;
  begin
    SplitParamAsText( _value, s1, s2, s3 ) ;

    str := '' ;

    case _class of
      'f' : begin
              str := s2 ;
            end ;
      'C' : begin
              str := _caption ;
            end ;
      'S','s' : begin
              if Assigned( _canvas ) then begin
                if assigned( FGetBitmapEvent ) then begin
                  sitm := ClassName + '_' +
                          _value + '_' +
                          IntToStr( TruncS(_rect.Width) ) + '_' +
                          IntToStr( TruncS(_rect.Height) ) ;

                  if not oItemCache.TryGetValue( sitm, bmp ) then begin
                    bmp := FGetBitmapEvent(
                             _canvas, _value,
                             _color, _font,
                             TruncS(_rect.Width), TruncS(_rect.Height)
                           ) ;
                    oItemCache.Add( sitm, bmp );
                  end;

                  _canvas.DrawBitmap(
                    TBitmap(bmp.NativeBitmap),
                    RectF(0,0,bmp.Width,bmp.Height),
                    _rect, _item.AbsoluteOpacity
                  )
                end ;
              end;
            end ;
    end ;

    Hint := _caption ;

    _canvas.Font.Assign( _font ) ;
    _canvas.Fill.Color := _color ;
    _canvas.FillText( _rect, str, False,
                      _item.AbsoluteOpacity, [], TTextAlign.Leading
                    );
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


  procedure TGIS_PenStyleComboBox.DelayedUpdate(
    const _val : String
  ) ;
  var
    val : String ;
    s1, s2, s3 : String ;
  begin
    if IsStringEmpty( _val ) then
      exit ;

    SplitParamAsText( _val, s1, s2, s3 ) ;
    if s1 = GIS_PARAMTXT_TYPE_FIELD then
      val := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  'f',
                  TGIS_ComboBoxHelperPosition.Lru,
                  _val,
                  _val
                )
    else
      val := TGIS_ComboBoxHelper.PrepareItem(
               False ,
               'S',
               TGIS_ComboBoxHelperPosition.Lru,
               _val,
               _val
             ) ;

    comboExt.SetItem( val, True ) ;
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
      else
      if not IsStringEmpty(_value) then
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

  procedure TGIS_PenStyleComboBox.fset_OnChange(
    _event : TNotifyEvent
  ) ;
  begin
    FOnChange := _event ;
    comboExt.SetOnChange( FOnChange ) ;
  end ;

  procedure TGIS_PenStyleComboBox.Fill(
    const _hasSymbol : Boolean
  ) ;
  begin
    comboExt.BeginFill ;
    try
      Items.Clear ;

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
            _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
          )
        ) ;


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

  constructor TGIS_PatternComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_BrushStyle', 3 ) ;
    comboExt.ValueEvent  := doValueBrushStyle ;
    comboExt.RenderEvent := doRenderBrushStyle ;
    comboExt.CustomEvent := doCustomBrushStyle ;

    IComboExt := comboExt ;

    Height := 28 ;
  end ;

  procedure TGIS_PatternComboBox.fset_OnChange(
    _event : TNotifyEvent
  ) ;
  begin
    FOnChange := _event ;
    comboExt.SetOnChange( FOnChange ) ;
  end ;

  procedure TGIS_PatternComboBox.Fill(
    const _hasSymbol : Boolean
  ) ;
  begin
    comboExt.BeginFill ;
    try
      Items.Clear ;

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
      {$IFNDEF GIS_MOBILE_DIALOGS}
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          _rsrcna( GIS_RS_LEGEND_PRM_BITMAP ) + '...', GIS_PARAMTXT_TYPE_TEXTURE
        )
      ) ;
      {$ENDIF}
      if _hasSymbol then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
            _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
          )
        ) ;

      ItemIndex := 0 ;
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


  procedure TGIS_PatternComboBox.DelayedUpdate(
    const _val : String
  ) ;
  var
    val : String ;
  begin
    if IsStringEmpty( _val ) then
      exit ;

    val := TGIS_ComboBoxHelper.PrepareItem(
             False ,
             'B',
             TGIS_ComboBoxHelperPosition.Lru,
             _val,
             _val
           ) ;

    comboExt.SetItem( val, True ) ;
  end ;


  procedure TGIS_PatternComboBox.doRenderBrushStyle(
    _item      : TListBoxItem ;
    _canvas    : TCanvas      ;
    _rect      : TRectF       ;
    _font      : TFont        ;
    _color     : TAlphaColor  ;
    _class     : Char         ;
    _caption   : String       ;
    _value     : String
  ) ;
  var
    sitm : STring ;
    bmp  : TGIS_Bitmap ;
    str  : String ;
    r    : TRectF ;
  begin
    str := '' ;

    case _class of
      'C' : begin
              str := _caption ;
            end ;
      'B','s' : begin
              if Assigned( _canvas ) then begin
                if assigned( FGetBitmapEvent ) then begin
                  r := _rect ;
                  r.Inflate(-2,-2);
                  sitm := ClassName + '_' +
                          _value + '_' +
                          IntToStr( TruncS(r.Width) ) + '_' +
                          IntToStr( TruncS(r.Height) ) ;

                  if not oItemCache.TryGetValue( sitm, bmp ) then begin
                    bmp := FGetBitmapEvent(
                             _canvas, _value,
                             _color, _font,
                             TruncS(r.Width), TruncS(r.Height)
                           ) ;
                    oItemCache.Add( sitm, bmp );
                  end;

                  _canvas.DrawBitmap(
                    TBitmap(bmp.NativeBitmap),
                    RectF(0,0,bmp.Width,bmp.Height),
                    r, _item.AbsoluteOpacity
                  ) ;
                end ;
              end
            end ;
    end ;

    if Assigned( _canvas ) then begin
      Hint := _caption ;
    end ;

    _canvas.Font.Assign( _font ) ;
    _canvas.Fill.Color := _color ;
    _canvas.FillText( _rect, str, False,
                      _item.AbsoluteOpacity, [], TTextAlign.Leading
                    );
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

  constructor TGIS_ShieldComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Shield', 3 ) ;
    comboExt.ValueEvent  := doValueSymbol  ;
    comboExt.RenderEvent := doRenderSymbol ;
    comboExt.CustomEvent := doCustomSymbol ;

    IComboExt := comboExt ;

    Height := 28 ;
  end ;

  procedure TGIS_ShieldComboBox.fset_OnChange(
    _event : TNotifyEvent
  ) ;
  begin
    FOnChange := _event ;
    comboExt.SetOnChange( FOnChange ) ;
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
  end;

  function TGIS_ShieldComboBox.doValueSymbol(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    s1, s2, s3 : String ;
  begin
    try
      SplitParamAsText( _value, s1, s2, s3 ) ;

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


  procedure TGIS_ShieldComboBox.DelayedUpdate(
    const _val : String
  ) ;
  var
    val : String ;
  begin
    if IsStringEmpty( _val ) then
      exit ;

    val := TGIS_ComboBoxHelper.PrepareItem(
             False ,
             'B',
             TGIS_ComboBoxHelperPosition.Lru,
             _val,
             _val
           ) ;

    comboExt.SetItem( val, True ) ;
  end ;


  procedure TGIS_ShieldComboBox.doRenderSymbol(
    _item      : TListBoxItem ;
    _canvas    : TCanvas      ;
    _rect      : TRectF       ;
    _font      : TFont        ;
    _color     : TAlphaColor  ;
    _class     : Char         ;
    _caption   : String       ;
    _value     : String
  ) ;
  var
    sitm : STring ;
    bmp  : TGIS_Bitmap ;
    str  : String ;
    r    : TRectF ;
  begin
    str := '' ;

    case _class of
      'C' : begin
              str := _caption ;
            end ;
      'B','s' : begin
              if Assigned( _canvas ) then begin
                if assigned( FGetBitmapEvent ) then begin
                  r := _rect ;
                  r.Inflate(-2,-2);
                  sitm := ClassName + '_' +
                          _value + '_' +
                          IntToStr( TruncS(r.Width) ) + '_' +
                          IntToStr( TruncS(r.Height) ) ;

                  if not oItemCache.TryGetValue( sitm, bmp ) then begin
                    bmp := FGetBitmapEvent(
                             _canvas, _value,
                             _color, _font,
                             TruncS(r.Width), TruncS(r.Height)
                           ) ;
                    oItemCache.Add( sitm, bmp );
                  end;

                  _canvas.DrawBitmap(
                    TBitmap(bmp.NativeBitmap),
                    RectF(0,0,bmp.Width,bmp.Height),
                    r, _item.AbsoluteOpacity
                  ) ;
                end ;
              end
            end ;
    end ;

    if Assigned( _canvas ) then begin
      Hint := _caption ;
    end ;

    _canvas.Font.Assign( _font ) ;
    _canvas.Fill.Color := _color ;
    _canvas.FillText( _rect, str, False,
                      _item.AbsoluteOpacity, [], TTextAlign.Leading
                    );
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

  constructor TGIS_SymbolComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Symbol', 3 ) ;
    comboExt.ValueEvent  := doValueSymbol ;
    comboExt.RenderEvent := doRenderSymbol ;
    comboExt.CustomEvent := doCustomSymbol ;

    IComboExt := comboExt ;

    Height := 28 ;
  end ;

  procedure TGIS_SymbolComboBox.fset_OnChange(
    _event : TNotifyEvent
  ) ;
  begin
    FOnChange := _event ;
    comboExt.SetOnChange( FOnChange ) ;
  end ;

  procedure TGIS_SymbolComboBox.Fill ;
  begin
    comboExt.BeginFill ;
    try
      Items.Clear ;

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
          _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
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


  procedure TGIS_SymbolComboBox.DelayedUpdate(
    const _val : String
  ) ;
  var
    val : String ;
  begin
    if IsStringEmpty( _val ) then
      exit ;

    val := TGIS_ComboBoxHelper.PrepareItem(
             False ,
             'B',
             TGIS_ComboBoxHelperPosition.Lru,
             _val,
             _val
           ) ;

    comboExt.SetItem( val, True ) ;
  end ;


  procedure TGIS_SymbolComboBox.doRenderSymbol(
    _item      : TListBoxItem ;
    _canvas    : TCanvas      ;
    _rect      : TRectF       ;
    _font      : TFont        ;
    _color     : TAlphaColor  ;
    _class     : Char         ;
    _caption   : String       ;
    _value     : String
  ) ;
  var
    sitm : String      ;
    bmp  : TGIS_Bitmap ;
    str  : String      ;
  begin

    str := '' ;

    case _class of
      'C' : begin
              str := _caption ;
            end ;
      'B','s' : begin
              if Assigned( _canvas ) then begin
                if assigned( FGetBitmapEvent ) then begin
                  sitm := ClassName + '_' +
                          _value + '_' +
                          IntToStr( TruncS(_rect.Width) ) + '_' +
                          IntToStr( TruncS(_rect.Height) ) ;

                  if not oItemCache.TryGetValue( sitm, bmp ) then begin
                    bmp := FGetBitmapEvent(
                             _canvas, _value,
                             _color, _font,
                             TruncS(_rect.Width), TruncS(_rect.Height)
                           ) ;
                    oItemCache.Add( sitm, bmp );
                  end;

                  _canvas.DrawBitmap(
                    TBitmap(bmp.NativeBitmap),
                    RectF(0,0,bmp.Width,bmp.Height),
                    _rect, _item.AbsoluteOpacity
                  ) ;
                end
              end;
            end ;
    end ;
    Hint := _caption ;

    _canvas.Font.Assign( _font ) ;
    _canvas.Fill.Color := _color ;
    _canvas.FillText( _rect, str, False,
                      _item.AbsoluteOpacity, [], TTextAlign.Leading
                    );
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

  constructor TGIS_FieldValueComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited Create( _owner ) ;
    self.Parent := TControl( _owner )  ;

    edtBox := TEdit.Create( _owner ) ;
    edtBox.Parent := TControl( _owner ) ;

    edtBox.OnKeyDown  := doEdtKeyDown;
    edtBox.OnKeyUp    := doEdtKeyUp;
    edtBox.OnMouseUp  := doEdtMouseDown;
    edtBox.OnChangeTracking := doEdtChange ;
    edtBox.KillFocusByReturn := True ;

    btnBox := TSpeedButton.Create( _owner ) ;
    btnBox.Parent := TControl( _owner ) ;
    btnBox.StyleLookup := 'spinbottombutton' ;
    btnBox.OnClick := doBtnClick ;


    OnKeyDown := doComboKeyDown ;
    inherited OnChange  := doComboChange ;

    comboExt := TGIS_ComboBoxHelper.Create(
                  Self, Self.Name + '_FieldValue', 0
                ) ;
    comboExt.ValueEvent  := doValue ;
    IComboExt := comboExt ;

    iPos := -1 ;

    StyleLookup := 'panelstyle' ;
  end ;

  function TGIS_FieldValueComboBox.doValue(
    _sender : TObject ;
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
   var _keyChar: WideChar ;
       _shift  : TShiftState
  ) ;
  begin
    if _key = vkDown then begin
      _key := 0 ;
    end
    else if DroppedDown then begin
      _key := 0 ;
    end;
  end;

  procedure TGIS_FieldValueComboBox.doEdtKeyUp(
       _sender : TObject ;
   var _key    : Word    ;
   var _keyChar: WideChar ;
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
      _key := Char(0) ; // ignore keys pressed upon dropdown
  end;

  procedure TGIS_FieldValueComboBox.doEdtMouseDown(
    _sender : TObject  ;
    _button : TMouseButton ;
    _shift  : TShiftState  ;
    _x      : Single  ;
    _y      : Single
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
   var _keyChar: WideChar ;
       _shift  : TShiftState
  ) ;
  var
    i : Integer ;
    s : String ;
  begin
    if _keyChar < #65 then exit ;

    // locate item based on first letter pressed
    for i := ItemIndex + 1 to Items.Count -1  do begin
      s := Items[i] ;
      if UpperCase( s[1] ) = UpperCase( _keyChar ) then begin
        ItemIndex := i ;
        exit ;
      end;
    end;
    for I := 0 to ItemIndex  do begin
      s := Items[i] ;
      if UpperCase( s[1] ) = UpperCase( _keyChar ) then begin
        ItemIndex := i ;
        exit ;
      end;
    end;
  end;

  procedure TGIS_FieldValueComboBox.doBtnClick(
     _sender : TObject
  ) ;
  begin
    Application.MainForm.Focused := nil ;
    DropDown ;
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

  procedure TGIS_FieldValueComboBox.Resize;
  begin
    inherited;

    btnBox.Width  := Height ;
    btnBox.Height := Height ;
    btnBox.Position := Position ;
    btnBox.Position.X := Position.X + Width - btnBox.Height ;

    edtBox.Position := Position ;
    edtBox.Width  := Width - btnBox.Width ;
    edtBox.Height := Height ;
  end;


  procedure TGIS_FieldValueComboBox.Fill(
    const _lst   : TStringList
  )   ;
  var
    s : String ;
  begin
    comboExt.BeginFill ;
    try
      Items.Clear ;

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

      ItemIndex := 0 ;
    finally
      comboExt.EndFill ;
    end ;
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
    TextSettings.Font.Family := FFontEx.Name ;
    TextSettings.Font.Size   := FFontEx.Size ;
    TextSettings.FontColor   := FFontEx.Color.ToBGR ;
    Font.Style := FMXFontStyle( FFontEx.Style ) ;

    Text := TextSettings.Font.Family ;
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

  procedure TGIS_FontNameComboBox.fset_OnChange(
    _event : TNotifyEvent
  ) ;
  begin
    FOnChange := _event ;
    comboExt.SetOnChange( FOnChange ) ;
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
    _sender : TObject ;
    _value  : String
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

      if s1 = GIS_PARAMTXT_TYPE_SIZE then
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


  procedure TGIS_SizeComboBox.DelayedUpdate(
    const _val : String
  ) ;
  var
    val : String ;
    s1, s2, s3 : String ;
  begin
    if IsStringEmpty( _val ) then
      exit ;

    SplitParamAsText( _val, s1, s2, s3 ) ;

    if s1 = GIS_PARAMTXT_TYPE_SIZE then
      val := TGIS_ComboBoxHelper.PrepareItem(
               False ,
               's',
               TGIS_ComboBoxHelperPosition.Lru,
               _val,
               _val
             )
    else
    if s1 = GIS_PARAMTXT_TYPE_FIELD then
      val := TGIS_ComboBoxHelper.PrepareItem(
               False ,
               'f',
               TGIS_ComboBoxHelperPosition.Lru,
               _val,
               _val
             ) ;

    comboExt.SetItem( val, True ) ;
  end ;


  procedure TGIS_SizeComboBox.doRenderSize(
    _item      : TListBoxItem ;
    _canvas    : TCanvas      ;
    _rect      : TRectF       ;
    _font      : TFont        ;
    _color     : TAlphaColor  ;
    _class     : Char         ;
    _caption   : String       ;
    _value     : String
  ) ;
  var
    s1, s2, s3 : String ;
    str : String ;
  begin
    str := '' ;

    SplitParamAsText( _value, s1, s2, s3  );

    case _class of
      'f' : begin
              str := s2 + ' * ' + s3;
            end ;
      'r' : begin
              str := _caption ;
            end ;
      'C' : begin
              str := _caption ;
            end ;
      else begin
        if not IsStringEmpty( s2) then
          str  := s2
        else
          str  := s1
      end;
    end;
    Hint := _caption ;

    _canvas.Font.Assign( _font ) ;
    _canvas.Fill.Color := _color ;
    _canvas.FillText( _rect, str, False,
                      _item.AbsoluteOpacity, [], TTextAlign.Leading
                    );
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

  procedure TGIS_SizeComboBox.fset_OnChange(
    _event : TNotifyEvent
  ) ;
  begin
    FOnChange := _event ;
    comboExt.SetOnChange( FOnChange ) ;
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
      Items.Clear ;

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


  procedure TGIS_RotationComboBox.DelayedUpdate(
    const _val : String
  ) ;
  var
    val : String ;
    s1, s2, s3 : String ;
  begin
    if IsStringEmpty( _val ) then
      exit ;

    SplitParamAsText( _val, s1, s2, s3 ) ;

    if s1 = GIS_PARAMTXT_TYPE_ANGLE then
      val := TGIS_ComboBoxHelper.PrepareItem(
               False ,
               's',
               TGIS_ComboBoxHelperPosition.Lru,
               _val,
               _val
             )
    else
    if s1 = GIS_PARAMTXT_TYPE_FIELD then
      val := TGIS_ComboBoxHelper.PrepareItem(
               False ,
               'f',
               TGIS_ComboBoxHelperPosition.Lru,
               _val,
               _val
             ) ;

    comboExt.SetItem( val, True ) ;
  end ;


  procedure TGIS_RotationComboBox.doRenderRotation(
    _item      : TListBoxItem ;
    _canvas    : TCanvas      ;
    _rect      : TRectF       ;
    _font      : TFont        ;
    _color     : TAlphaColor  ;
    _class     : Char         ;
    _caption   : String       ;
    _value     : String
  ) ;
  var
    str : String ;
    s1, s2, s3 : String ;

  begin
    str := '' ;

    SplitParamAsText( _value, s1, s2, s3 );

    case _class of
      'f' : begin
              str := s2 + ' * ' + s3;
            end ;
      'C' : begin
              str := _caption ;
            end ;
      else begin
        if not IsStringEmpty( s2) then
          str := s2
        else
          str := s1
      end;
    end ;
    Hint := _caption ;

    _canvas.Font.Assign( _font ) ;
    _canvas.Fill.Color := _color ;
    _canvas.FillText( _rect, str, False,
                      _item.AbsoluteOpacity, [], TTextAlign.Leading
                    );
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

  procedure TGIS_RotationComboBox.fset_OnChange(
    _event : TNotifyEvent
  ) ;
  begin
    FOnChange := _event ;
    comboExt.SetOnChange( FOnChange ) ;
  end ;

  procedure TGIS_RotationComboBox.Fill(
    _field : Boolean
  ) ;
  begin
    comboExt.BeginFill ;
    try
      Items.Clear ;

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

