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
  Helper to validate editable controls like TGIS_PvlEdit or TGIS_PvlComboBox.
}


{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoValueValidatorHelper;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoValueValidatorHelper"'}
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
    System.Classes,
    System.SysUtils,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.PVL.GeoPvl;
  {$ENDIF}

  {$IFDEF JAVA}
    java.util,
    java.awt.*,
    javax.swing.*,
    java.beans.*,
    tatukgis.rtl ;
  {$ENDIF}

type

  /// <summary>
  ///   Interface for validating editable control.
  /// </summary>
  IGIS_ValueValidator = interface

      /// <summary>
      ///   Setter for MinVal property
      /// </summary>
      /// <param name="_value">
      ///   New MinVal value
      /// </param>
      procedure fset_MinVal           ( const _value : Double
                                      ) ;

      /// <summary>
      ///   Getter for MinVal property
      /// </summary>
      /// <returns>
      ///   MinVal value
      /// </returns>
      function  fget_MinVal           : Double ;

      /// <summary>
      ///   Setter for MaxVal property
      /// </summary>
      /// <param name="_value">
      ///   New MaxVal value
      /// </param>
      procedure fset_MaxVal           ( const _value : Double
                                      ) ;

      /// <summary>
      ///  Getter for MaxVal property
      /// </summary>
      /// <returns>
      ///   MaxVal value
      /// </returns>
      function  fget_MaxVal           : Double ;

      /// <summary>
      ///   Getter for Value property
      /// </summary>
      /// <returns>
      ///   Value
      /// </returns>
      function  fget_Value            : Double ;

      /// <summary>
      ///   Setter for Value property
      /// </summary>
      /// <param name="_value">
      ///   New value value
      /// </param>
      procedure fset_Value            ( const _value : Double
                                      ) ;

      /// <summary>
      ///   Setter for Precision property
      /// </summary>
      /// <param name="_value">
      ///   New precision value
      /// </param>
      procedure fset_Precision        ( const _value : Integer
                                      ) ;

      /// <summary>
      ///   Getter for Precision property
      /// </summary>
      /// <returns>
      ///   Precision value
      /// </returns>
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
      /// <inheritdoc/>
      procedure fset_MinVal           ( const _value : Double
                                      ) ;

      /// <inheritdoc/>
      function  fget_MinVal           : Double ;

      /// <inheritdoc/>
      procedure fset_MaxVal           ( const _value : Double
                                      ) ;

      /// <inheritdoc/>
      function  fget_MaxVal           : Double ;

      /// <inheritdoc/>
      function  fget_Value            : Double ;

      /// <inheritdoc/>
      procedure fset_Value            ( const _value : Double
                                      ) ;

      /// <inheritdoc/>
      procedure fset_Precision        ( const _value : Integer
                                      ) ;

      /// <inheritdoc/>
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
      ///   Set color of the font to the alarm state,
      /// </summary>
      procedure setFontAlarm          ; virtual;

      /// <summary>
      ///   Set color of the font to the default state,
      /// </summary>
      procedure setFontDefault         ; virtual;

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
      FParentControl  : TGIS_PvlControl ;

      FOrgOnChange    : TGIS_PvlEvent   ;
      FOrgOnKeyPress  : TGIS_PvlKeyPressEvent ;
    protected
      /// <inheritdoc/>
      procedure invalidate ;          override;

      /// <inheritdoc/>
      procedure setText               ( const _text : String
                                      ) ; override;
      /// <inheritdoc/>
      function  getText               : String ; override;

      /// <inheritdoc/>
      procedure setFontAlarm          ; override;

      /// <inheritdoc/>
      procedure setFontDefault        ; override;

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
      ///   TGIS_PvlEditor object to be linked with
      /// </param>
      constructor Create     ( const _parent   : TGIS_PvlControl
                             ) ;
  end ;

  /// <summary>
  ///   Helper for TGIS_Pvl control to provide validation of numeric values.
  /// </summary>
  TGIS_ValueValidatorEditHelper = class( TGIS_ValueValidatorEditableControlHelper )
    protected
      /// <inheritdoc/>
      procedure setText               ( const _text : String
                                      ) ; override;
      /// <inheritdoc/>
      function  getText               : String ; override;

      /// <inheritdoc/>
      procedure setFontAlarm          ; override;

      /// <inheritdoc/>
      procedure setFontDefault        ; override;

    public
      /// <summary>
      ///   Helper constructor
      /// </summary>
      /// <param name="_parent">
      ///   TGIS_Pvl object to be linked with
      /// </param>
      constructor Create     ( const _parent : TGIS_PvlEdit
                             ) ;
  end ;

  /// <summary>
  ///   Helper for TGIS_PvlComboBox control to provide validation of numeric
  ///   values.
  /// </summary>
  TGIS_ValueValidatorComboBoxHelper = class( TGIS_ValueValidatorEditableControlHelper )
    protected
      /// <inheritdoc/>
      procedure setText               ( const _text : String
                                      ) ; override;
      /// <inheritdoc/>
      function  getText               : String ; override;

      /// <inheritdoc/>
      procedure setFontAlarm          ; override;

      /// <inheritdoc/>
      procedure setFontDefault        ; override;

    public
      /// <summary>
      ///   Helper constructor
      /// </summary>
      /// <param name="_parent">
      ///   TGIS_PvlComboBox object to be linked with
      /// </param>
      constructor Create     ( const _parent : TGIS_PvlComboBox
                             ) ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRtl ;
{$ENDIF}

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
  var
    k    : Integer ;
    sVal : String  ;
  begin
    try
      tmpText := Trim( getText ) ;
      if IsStringEmpty( tmpText ) then begin
        setFontAlarm ;
        exit ;
      end ;

      tmpValue := DotStrToFloat( tmpText ) ;
      sVal := DotFloatToStr( tmpValue ) ;

      if Precision > 0 then begin
         k := sVal.IndexOf('.') ;
         if k > 0 then
           if length( tmpValue.ToString ) - k - 1 > Precision then  begin
             setFontAlarm ;
             exit ;
           end;
      end ;

      if ( tmpValue < FMinVal ) or ( tmpValue > FMaxVal ) then
        setFontAlarm
      else begin
        prepareValue( tmpValue ) ;//
//        setText( prepareValue( tmpValue ) ) ;
        setFontDefault ;
      end ;
    except
      setFontAlarm ;
    end ;
  end ;

  function TGIS_ValueValidator.acceptKey(
    const _key : Char
  ) : Boolean ;
  {$IFDEF JAVA}
  const
    charSet : TSysCharSet = [ '0','1','2','3','4','5','6','7','8','9', '-', '.', ',' ] ;
  {$ENDIF}
  begin
    Result := True ;

    if (FPrecision <= 0) and CharInSet( _key, [ '.', ',' ] ) then
      Result := False
    {$IFDEF JAVA}
      else if not CharInSet( _key, charSet ) then
    {$ELSE}
      else if not CharInSet( _key, [ Char(8), '0'..'9', '-', '.', ','] ) then
    {$ENDIF}
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
    txt    := '' ; //????
    FValue := _value ;
    oval   := _value ;

    if FPrecision > 14 then
      Result := Format( '%g%s'  , [ oval, txt ] )
    else
      Result := Format( '%.*f%s', [ FPrecision, oval, txt ] ) ;
  end ;

  procedure TGIS_ValueValidator.setFontAlarm ;
  begin
    // safe inheritance only
  end;

  procedure TGIS_ValueValidator.setFontDefault ;
  begin
    // safe inheritance only
  end;

  procedure TGIS_ValueValidator.setText(
    const _text : String
  ) ;
  begin
    // safe inheritance only
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
      setFontAlarm ;
    end
    else begin
      Result := True ;
      setFontDefault ;
    end ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_ValueValidatorEditableControlHelper'}

  constructor TGIS_ValueValidatorEditableControlHelper.Create(
    const _parent   : TGIS_PvlControl
  );
  begin
    inherited Create ;

    FParentControl := _parent ;
    assert( FParentControl <> nil ) ;
  end ;

  procedure TGIS_ValueValidatorEditableControlHelper.doKeyPress(
    _sender  : TObject ;
    var _key : Char
  ) ;
  var
    key : Char    ;
  begin
    key := _key ;

    if key < Char(32) then exit;
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
//?    FParentControl.Invalidate ;
  end;

  procedure TGIS_ValueValidatorEditableControlHelper.setFontAlarm ;
  begin
    // safe inheritance only
  end ;

  procedure TGIS_ValueValidatorEditableControlHelper.setFontDefault ;
  begin
    // safe inheritance only
  end ;

  procedure TGIS_ValueValidatorEditableControlHelper.setText(
    const _text : String
  ) ;
  begin
    // safe inheritance only
  end ;

  procedure TGIS_ValueValidatorEditableControlHelper.doChange(_sender: TObject);
  begin
    doChangeValue ;

    if assigned( FOrgOnChange ) then
      FOrgOnChange( FParentControl ) ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_ValueValidatorEditHelper'}


  constructor TGIS_ValueValidatorEditHelper.Create(
    const _parent: TGIS_PvlEdit
  ) ;
  begin
    inherited Create( _parent ) ;

    FOrgOnChange   := TGIS_PvlEdit(FParentControl).OnChange    ;
    FOrgOnKeyPress := TGIS_PvlEdit(FParentControl).OnKeyPress   ;

    {$IFDEF DCC}
      TGIS_PvlEdit(FParentControl).OnKeyPress := doKeyPress ;
      TGIS_PvlEdit(FParentControl).OnChange   := doChange ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      TGIS_PvlEdit(FParentControl).OnKeyPress := @doKeyPress ;
      TGIS_PvlEdit(FParentControl).OnChange   := @doChange ;
    {$ENDIF}
  end ;

  function TGIS_ValueValidatorEditHelper.getText : String ;
  begin
    Result := TGIS_PvlEdit(FParentControl).Text ;
  end ;

  procedure TGIS_ValueValidatorEditHelper.setFontAlarm ;
  begin
    TGIS_PvlEdit(FParentControl).SetFontAlarm ;
  end ;

  procedure TGIS_ValueValidatorEditHelper.setFontDefault ;
  begin
    TGIS_PvlEdit(FParentControl).SetFontDefault ;
  end ;

  procedure TGIS_ValueValidatorEditHelper.setText(
    const _text : String
  ) ;
  begin
    TGIS_PvlEdit(FParentControl).Text := _text ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ValueValidatorComboBoxHelper'}

  constructor TGIS_ValueValidatorComboBoxHelper.Create(
    const _parent : TGIS_PvlComboBox
  ) ;
  begin
    inherited Create( _parent ) ;

//    FOrgOnChange   := TGIS_PvlComboBox(FParentControl).OnChange    ;
//    FOrgOnKeyPress := TGIS_PvlComboBox(FParentControl).OnKeyPress   ;

//?    TComboBox(FParentControl).OnKeyPress := doKeyPress ;
//?    TComboBox(FParentControl).OnChange   := doChange ;
  end ;

  function TGIS_ValueValidatorComboBoxHelper.getText : String ;
  begin
    Result := TGIS_PvlComboBox(FParentControl).Text ;
  end ;

  procedure TGIS_ValueValidatorComboBoxHelper.setFontAlarm ;
  begin
//?    TComboBox(FParentControl).Font.Color := _color ;
  end ;

  procedure TGIS_ValueValidatorComboBoxHelper.setFontDefault ;
  begin
//?    TComboBox(FParentControl).Font.Color := _color ;
  end ;

  procedure TGIS_ValueValidatorComboBoxHelper.setText(
    const _text: String
  ) ;
  begin
    TGIS_PvlComboBox(FParentControl).Text := _text ;
  end ;
{$ENDREGION}

//==================================== END =====================================
end.
