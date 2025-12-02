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
  Helper to validate editable controls like TEdit or TComboBox.
}

unit VCL.GisValueValidatorHelper;
{$HPPEMIT '#pragma link "VCL.GisValueValidatorHelper"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls ;

type

  /// <summary>
  ///   Interface for validating editable control.
  /// </summary>
  IGIS_ValueValidator = interface
      procedure fset_MinVal           ( const _value : Double
                                      ) ;
      function  fget_MinVal           : Double ;
      procedure fset_MaxVal           ( const _value : Double
                                      ) ;
      function  fget_MaxVal           : Double ;
      function  fget_Value            : Double ;
      procedure fset_Value            ( const _value : Double
                                      ) ;
      procedure fset_Precision        ( const _value : Integer
                                      ) ;
      function  fget_Precision        : Integer ;

      /// <summary>
      ///   Check if value is valid (inside MinVal..MaxVal zone).
      /// </summary>
      /// <returns>
      ///   True if valid.
      /// </returns>
      function   Valid                : Boolean ;

      /// <summary>
      ///   Number of digits.
      /// </summary>
      property Precision : Integer            read  fget_Precision
                                              write fset_Precision ;

      /// <summary>
      ///   Minimum expected value.
      /// </summary>
      property MinVal    : Double             read  fget_MinVal
                                              write fset_MinVal    ;

      /// <summary>
      ///   Maximum expected value.
      /// </summary>
      property MaxVal    : Double             read  fget_MaxVal
                                              write fset_MaxVal    ;

      /// <summary>
      ///   Value of spin control.
      /// </summary>
      property Value     : Double             read  fget_Value
                                              write fset_Value     ;
  end ;

  /// <summary>
  ///   Implementation of interface for validating editable control.
  /// </summary>
  TGIS_ValueValidator = class( TInterfacedObject, IGIS_ValueValidator )
    private
      FMinVal         : Double  ;
      FMaxVal         : Double  ;
      FValue          : Double  ;
      FPrecision      : Integer ;

      tmpText         : String  ;
      tmpValue        : Double  ;
      isValueSet      : Boolean ;
    protected
      procedure fset_MinVal           ( const _value : Double
                                      ) ;
      function  fget_MinVal           : Double ;
      procedure fset_MaxVal           ( const _value : Double
                                      ) ;
      function  fget_MaxVal           : Double ;
      function  fget_Value            : Double ;
      procedure fset_Value            ( const _value : Double
                                      ) ;
      procedure fset_Precision        ( const _value : Integer
                                      ) ;
      function  fget_Precision        : Integer ;
      /// <summary>
      ///   Invalidates.
      /// </summary>
      procedure invalidate ;          virtual;
      /// <summary>
      ///   Set text of the validator.
      /// </summary>
      /// <param name="_text">
      ///   text to be set
      /// </param>
      procedure setText               ( const _text : String
                                      ) ; virtual;
      /// <summary>
      ///   Get text of the validator.
      /// </summary>
      /// <returns>
      ///   Text of validator.
      /// </returns>
      function  getText               : String ; virtual;
      /// <summary>
      ///   Set color of the font,
      /// </summary>
      /// <param name="_color">
      ///   color to be set
      /// </param>
      procedure setFontColor          ( const _color : TColor
                                      ) ; virtual;
      /// <summary>
      ///   Key to accept.
      /// </summary>
      /// <param name="_key">
      ///   key to be set
      /// </param>
      /// <returns>
      ///   True if accepted.
      /// </returns>
      function  acceptKey             ( const _key : Char
                                      ) : Boolean ;

      /// <summary>
      ///   Prepare value as text.
      /// </summary>
      /// <param name="_value">
      ///   requested value in float
      /// </param>
      /// <returns>
      ///   Prepared value as String text.
      /// </returns>
      function  prepareValue          ( const _value : Double
                                      ) : String ;
      /// <summary>
      ///   Validate on value change.
      /// </summary>
      procedure doChangeValue ;
    public
      /// <summary>
      ///   Constructor for the TGIS_ValueValidator object.
      /// </summary>
      constructor Create      ;

      /// <inheritdoc/>
      function   Valid         : Boolean ;
    public
      /// <inheritdoc/>
      property Precision : Integer            read  FPrecision
                                              write fset_Precision ;

      /// <inheritdoc/>
      property MinVal    : Double             read  FMinVal
                                              write fset_MinVal    ;

      
      /// <inheritdoc/>
      property MaxVal    : Double             read  FMaxVal
                                              write fset_MaxVal    ;

      /// <inheritdoc/>
      property Value     : Double             read  fget_Value
                                              write fset_Value     ;
  end ;

  /// <summary>
  ///   Helper for editable control to provide validation of numeric values.
  /// </summary>
  TGIS_ValueValidatorEditableControlHelper = class( TGIS_ValueValidator )
    private
      FParentControl  : TControl      ;

      FOrgOnChange    : TNotifyEvent   ;
      FOrgOnKeyPress  : TKeyPressEvent       ;
    protected
      /// <inheritdoc/>
      procedure invalidate ;          override;
      /// <inheritdoc/>
      procedure setText               ( const _text : String
                                      ) ; override;
      /// <inheritdoc/>
      function  getText               : String ; override;
      /// <inheritdoc/>
      procedure setFontColor          ( const _color : TColor
                                      ) ; override;

      /// <summary>
      ///   Key event handler. For basic input verification - allow only
      ///   "known" characters
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_key">
      ///   return pressed key as Char
      /// </param>
      procedure doKeyPress     ( _sender   : TObject ;
                                 var _key  : Char
                               ) ;

      /// <summary>
      ///   Change value handler. Do value validation etc.
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      procedure doChange       ( _sender : TObject ) ;
    public
      /// <summary>
      ///   Helper constructor
      /// </summary>
      /// <param name="_parent">
      ///   TEdit object to be linked with
      /// </param>
      constructor Create     ( const _parent   : TControl
                             ) ;
  end ;

  /// <summary>
  ///   Helper for TEdit control to provide validation of numeric values.
  /// </summary>
  TGIS_ValueValidatorEditHelper = class( TGIS_ValueValidatorEditableControlHelper )
    protected
      /// <inheritdoc/>
      procedure setText               ( const _text : String
                                      ) ; override;
      /// <inheritdoc/>
      function  getText               : String ; override;
      /// <inheritdoc/>
      procedure setFontColor          ( const _color : TColor
                                      ) ; override;
    public
      /// <summary>
      ///   Helper constructor
      /// </summary>
      /// <param name="_parent">
      ///   TEdit object to be linked with
      /// </param>
      constructor Create     ( const _parent : TEdit
                             ) ;
  end ;

  /// <summary>
  ///   Helper for TComboBox control to provide validation of numeric values.
  /// </summary>
  TGIS_ValueValidatorComboBoxHelper = class( TGIS_ValueValidatorEditableControlHelper )
    protected
      /// <inheritdoc/>
      procedure setText               ( const _text : String
                                      ) ; override;
      /// <inheritdoc/>
      function  getText               : String ; override;
      /// <inheritdoc/>
      procedure setFontColor          ( const _color : TColor
                                      ) ; override;
    public
      /// <summary>
      ///   Helper constructor
      /// </summary>
      /// <param name="_parent">
      ///   TComboBox object to be linked with
      /// </param>
      constructor Create     ( const _parent : TComboBox
                             ) ;
  end ;

implementation

uses
  GisRtl ;

{$REGION 'TGIS_ValueValidator'}

  constructor TGIS_ValueValidator.Create ;
  begin
    inherited Create ;

    FMinVal    := 0     ;
    FMaxVal    := 100   ;
    FValue     := 0     ;
    FPrecision := 0     ;

    tmpText    := ''    ;
    tmpValue   := 0     ;
    isValueSet := False ;
  end ;

  function TGIS_ValueValidator.fget_MinVal : Double ;
  begin
    Result := FMinVal ;
  end ;

  procedure TGIS_ValueValidator.fset_MinVal(
    const _value : Double
  ) ;
  begin
    FMinVal := _value ;

    Valid ;
  end ;

  procedure TGIS_ValueValidator.doChangeValue ;
  begin
    try
      tmpText := Trim( getText ) ;
      if IsStringEmpty( tmpText ) then begin
        setFontColor( clRed ) ;
        exit ;
      end ;

      tmpValue := DotStrToFloat( tmpText ) ;

      if ( tmpValue < FMinVal ) or ( tmpValue > FMaxVal ) then
        setFontColor( clRed )
      else begin
        prepareValue( tmpValue ) ;
        setFontColor( clWindowText ) ;
      end ;
    except
      setFontColor( clRed )
    end ;
  end ;

  function TGIS_ValueValidator.acceptKey(
    const _key : Char
  ) : Boolean ;
  begin
    Result := True ;

    if (FPrecision <= 0) and CharInSet( _key, [ '.', ',' ] ) then
      Result := False
    else if not CharInSet( _key, [ Char(8), '0'..'9', '-', '.', ','] ) then
      Result := False ;
  end ;

  function TGIS_ValueValidator.fget_MaxVal : Double ;
  begin
    Result := FMaxVal ;
  end ;

  procedure TGIS_ValueValidator.fset_MaxVal(
    const _value : Double
  ) ;
  begin
    FMaxVal := _value ;

    Valid ;
  end ;

  function TGIS_ValueValidator.fget_Precision : Integer ;
  begin
    Result := FPrecision ;
  end ;

  procedure TGIS_ValueValidator.fset_Precision(
    const _value : Integer
  ) ;
  begin
    FPrecision := _value ;
    invalidate ;
  end ;

  function  TGIS_ValueValidator.fget_Value : Double ;
  begin
    Result := FValue ;
  end ;

  procedure TGIS_ValueValidator.fset_Value(
    const _value : Double
  ) ;
  begin
    isValueSet := True ;
    setText( prepareValue( _value ) ) ;
  end ;

  procedure TGIS_ValueValidator.invalidate ;
  begin

  end ;

  function TGIS_ValueValidator.prepareValue(
      const _value : Double
    ) : String ;
  var
    txt  : String  ;
    oval : Double  ;
  begin
    FValue := _value ;
    oval   := _value ;

    if FPrecision > 14 then
      Result := Format( '%g%s'  , [ oval, txt ] )
    else
      Result := Format( '%.*f%s', [ FPrecision, oval, txt ] ) ;
  end ;

  procedure TGIS_ValueValidator.setFontColor(
    const _color : TColor
  ) ;
  begin

  end;

  procedure TGIS_ValueValidator.setText(
    const _text : String
  ) ;
  begin

  end ;

  function TGIS_ValueValidator.getText : String ;
  begin
    Result := '' ;
  end ;

  function TGIS_ValueValidator.Valid : Boolean ;
  begin
    Result := True ;
    if not isValueSet then exit ;

    if IsStringEmpty(tmpText) or (tmpValue < FMinVal) or (tmpValue > FMaxVal) then
    begin
      Result := False ;
      setFontColor( clRed ) ;
    end
    else begin
      Result := True ;
      setFontColor( clWindowText ) ;
    end ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_ValueValidatorEditableControlHelper'}

  constructor TGIS_ValueValidatorEditableControlHelper.Create(
    const _parent   : TControl
  );
  begin
    inherited Create ;

    FParentControl := _parent ;
    Assert( FParentControl <> nil ) ;
  end ;

  procedure TGIS_ValueValidatorEditableControlHelper.doKeyPress(
    _sender  : TObject ;
    var _key : Char
  ) ;
  var
    key : Char    ;
  begin
    key := _key ;

    if Key < Char(32) then exit;
    if LowerCase( key ) = 'e' then begin
      if assigned( FOrgOnKeyPress ) then
        FOrgOnKeyPress( FParentControl, _key ) ;
      exit ;
    end ;

    if not acceptKey( key ) then
      key := Char( 0 ) ;

    _key := key ;
    if assigned( FOrgOnKeyPress ) then
      FOrgOnKeyPress( FParentControl, _key ) ;
  end ;

  function TGIS_ValueValidatorEditableControlHelper.getText : String ;
  begin
    Result := '' ;
  end ;

  procedure TGIS_ValueValidatorEditableControlHelper.invalidate ;
  begin
    FParentControl.Invalidate ;
  end;

  procedure TGIS_ValueValidatorEditableControlHelper.setFontColor(
    const _color: TColor
  ) ;
  begin

  end ;

  procedure TGIS_ValueValidatorEditableControlHelper.setText(
    const _text : String
  ) ;
  begin

  end ;

  procedure TGIS_ValueValidatorEditableControlHelper.doChange ;
  begin
    doChangeValue ;

    if assigned( FOrgOnChange ) then
      FOrgOnChange( FParentControl ) ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_ValueValidatorEditHelper'}


  constructor TGIS_ValueValidatorEditHelper.Create(
    const _parent: TEdit
  ) ;
  begin
    inherited Create( _parent ) ;

    FOrgOnChange   := TEdit(FParentControl).OnChange    ;
    FOrgOnKeyPress := TEdit(FParentControl).OnKeyPress   ;

    TEdit(FParentControl).OnKeyPress := doKeyPress ;
    TEdit(FParentControl).OnChange   := doChange ;
  end ;

  function TGIS_ValueValidatorEditHelper.getText : String ;
  begin
    Result := TEdit(FParentControl).Text ;
  end ;

  procedure TGIS_ValueValidatorEditHelper.setFontColor(
    const _color : TColor
  ) ;
  begin
    TEdit(FParentControl).Font.Color := _color ;
  end ;

  procedure TGIS_ValueValidatorEditHelper.setText(
    const _text : String
  ) ;
  begin
    TEdit(FParentControl).Text := _text ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ValueValidatorComboBoxHelper'}

  constructor TGIS_ValueValidatorComboBoxHelper.Create(
    const _parent : TComboBox
  ) ;
  begin
    inherited Create( _parent ) ;

    FOrgOnChange   := TComboBox(FParentControl).OnChange    ;
    FOrgOnKeyPress := TComboBox(FParentControl).OnKeyPress   ;

    TComboBox(FParentControl).OnKeyPress := doKeyPress ;
    TComboBox(FParentControl).OnChange   := doChange ;
  end ;

  function TGIS_ValueValidatorComboBoxHelper.getText : String ;
  begin
    Result := TComboBox(FParentControl).Text ;
  end ;

  procedure TGIS_ValueValidatorComboBoxHelper.setFontColor(
    const _color : TColor
  ) ;
  begin
    TComboBox(FParentControl).Font.Color := _color ;
  end ;

  procedure TGIS_ValueValidatorComboBoxHelper.setText(
    const _text: String
  ) ;
  begin
    TComboBox(FParentControl).Text := _text ;
  end ;
{$ENDREGION}


end.

