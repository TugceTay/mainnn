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
  Helper to validate editable controls like TGIS_PvlEdit, or TGIS_PvlComboBox.
}

{$IFDEF DCC}
  unit PVL.GisValueValidatorHelper;
  {$HPPEMIT '#pragma link "PVL.GisValueValidatorHelper"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

uses
  {$IFDEF CLR}
  TatukGIS.NDK,
  TatukGIS.RTL ;
  {$ENDIF}
  {$IFDEF DCC}
  System.Classes,
  System.SysUtils,
  GisTypesUI,
  PVL.GisPvl;
  {$ENDIF}
  {$IFDEF JAVA}
  java.util,
  java.awt.*,
  javax.swing.*,
  java.beans.*,
  tatukgis.rtl,
  tatukgis.jdk ;
  {$ENDIF}

type
  {$IFNDEF GENDOC}
  /// <summary>
  ///   Declares a reference to a generic validation method.
  /// </summary>
  /// <param name="_arg">
  ///   tested argument (text or value)
  /// </param>
  /// <returns>
  ///   True, if validation passes; otherwise False.
  /// </returns>
  TGIS_ValidationMethod<T> = function( const _arg : T ) : Boolean of object ;

  /// <summary>
  ///   Base interface for the validator.
  /// </summary>
  IGIS_Validator<T> = interface

    function  fget_IsValid : Boolean ;
    
    function  fget_ValidationMethod : TGIS_ValidationMethod<T> ;
    procedure fset_ValidationMethod ( const _value : TGIS_ValidationMethod<T> ) ;

    /// <summary>
    ///   Custom validation procedure.
    /// </summary>
    property ValidationMethod       : TGIS_ValidationMethod<T>   
                                      read fget_ValidationMethod
                                      write fset_ValidationMethod ;   

    /// <summary>
    ///   Runs the validator.
    /// </summary>
    procedure Validate ;

    /// <summary>
    ///   Indicates whether the value is valid.
    /// </summary>
    /// <returns>
    ///   True if valid, False otherwise.
    /// </returns>
    property IsValid                : Boolean
                                      read fget_IsValid ;
  end;

  /// <summary>
  ///   Interface for text validation part of the validator.
  /// </summary>
  IGIS_TextValidator = interface( IGIS_Validator<String> )
  
    function  fget_AllowEmpty      : Boolean ;
    procedure fset_AllowEmpty      ( const _value : Boolean ) ;

    function  fget_AllowWhiteSpace : Boolean ;
    procedure fset_AllowWhiteSpace ( const _value : Boolean ) ;

    function  fget_Text            : String ;
    procedure fset_Text            ( const _value : String ) ;

    /// <summary>
    ///   Indicates whether the validator allows an empty text.
    /// </summary>
    property AllowEmpty            : Boolean
                                     read fget_AllowEmpty
                                     write fset_AllowEmpty ;

    /// <summary>
    ///   Indicates whether the validator allows a text with white-space characters.
    /// </summary>
    property AllowWhiteSpace        : Boolean
                                      read fget_AllowWhiteSpace
                                      write fset_AllowWhiteSpace ;

    /// <summary>
    ///   Text of the parent control.
    /// </summary>
    property Text                   : String
                                      read fget_Text
                                      write fset_Text ;
  end;

  /// <summary>
  ///   Interface for value validation part of the validator.
  /// </summary>
  IGIS_ValueValidator = interface( IGIS_Validator<Double> )
    procedure fset_MinValue    ( const _value : Double ) ;
    function  fget_MinValue    : Double ;

    procedure fset_MaxValue    ( const _value : Double ) ;
    function  fget_MaxValue    : Double ;

    function  fget_Value     : Double ;
    procedure fset_Value     ( const _value : Double ) ;

    procedure fset_Precision ( const _value : Byte ) ;
    function  fget_Precision : Byte ;
    
    /// <summary>
    ///   Number of digits to the right of the decimal point in a number
    /// </summary>
    property Precision       : Byte 
                               read  fget_Precision
                               write fset_Precision ;

    /// <summary>
    ///   Minimum expected value.
    /// </summary>
    property MinValue        : Double
                               read  fget_MinValue
                               write fset_MinValue ;

    /// <summary>
    ///   Maximum expected value.
    /// </summary>
    property MaxValue        : Double
                               read  fget_MaxValue
                               write fset_MaxValue ;

    /// <summary>
    ///   Tries to get value from validator.
    /// </summary>
    /// <param name="_value">
    ///   if success, value is returned by the argument
    /// </param>
    /// <returns>
    ///   True on success, otherwise False.
    /// </returns>
    function TryGetValue     ( out _value : Double ) : Boolean ;
    
    /// <summary>
    ///   Gets or sets the value of the validator.
    /// </summary>
    property Value           : Double             
                               read  fget_Value
                               write fset_Value ;
  end ;
  {$ENDIF}
  
  /// <summary>
  ///   Class helper for TGIS_PvlEdit for creating validator directly from the control.
  /// </summary>
  TGIS_PvlEditHelper = class helper for TGIS_PvlEdit
    /// <summary>
    ///   Creates TextValidator for this control.
    /// </summary>
    /// <returns>
    ///   An instance of IGIS_TextValidator;
    /// </returns>
    function CreateTextValidator : IGIS_TextValidator ;

    /// <summary>
    ///   Creates TextValidator for this control.
    /// </summary>
    /// <returns>
    ///   An instance of IGIS_ValueValidator;
    /// </returns>
    function CreateValueValidator : IGIS_ValueValidator ;
  end;

  /// <summary>
  ///   Class helper for TGIS_PvlComboEdit for creating validator directly from the control.
  /// </summary>
  TGIS_PvlComboEditHelper = class helper for TGIS_PvlComboEdit
    /// <summary>
    ///   Creates TextValidator for this control.
    /// </summary>
    /// <returns>
    ///   An instance of IGIS_TextValidator;
    /// </returns>
    function CreateTextValidator : IGIS_TextValidator ;

    /// <summary>
    ///   Creates TextValidator for this control.
    /// </summary>
    /// <returns>
    ///   An instance of IGIS_ValueValidator;
    /// </returns>
    function CreateValueValidator : IGIS_ValueValidator ;
  end;


implementation

{$IFDEF DCC}
uses
  System.Character,
  System.Math,
  GisClasses,
  GisFunctions,
  GisResource,
  GisRtl,
  GisTypes;
{$ENDIF}

type
  /// <summary>
  ///   Interface for the control part of the validator.
  /// </summary>
  IGIS_ControlValidator = interface

    function fget_Control           : TGIS_PvlControl ;

    procedure fset_OnControlChange  ( const _value : TGIS_PvlEvent ) ;
    function fget_OnControlChange   : TGIS_PvlEvent ;

    procedure fset_OnControlKeyPress( const _value : TGIS_PvlKeyPressEvent ) ;
    function fget_OnControlKeyPress : TGIS_PvlKeyPressEvent ;

    procedure fset_OnChange         ( const _value : TGIS_PvlEvent ) ;
    function fget_OnChange          : TGIS_PvlEvent ;

    procedure fset_OnKeyPress       ( const _value : TGIS_PvlKeyPressEvent ) ;
    function fget_OnKeyPress        : TGIS_PvlKeyPressEvent ;

    /// <summary>
    ///   Executes OnChange original parent control event.
    /// </summary>
    procedure DoControlChange ;

    /// <summary>
    ///   Executes OnKeyPress original parent control event.
    /// </summary>
    procedure DoControlKeyPress( var _key : Char ) ;

    /// <summary>
    ///   Sets the text associated with PVL control.
    /// </summary>
    /// <param name="_value">
    ///   text to set
    /// </param>
    procedure setText          ( const _value : String ) ;

    /// <summary>
    ///   Gets the text associated with PVL control.
    /// </summary>
    /// <returns>
    ///   Text or caption.
    /// </returns>
    function  getText          : String ;

    /// <summary>
    ///   Sets the control font to the alarm state.
    /// </summary>
    procedure setFontAlarm ;

    /// <summary>
    ///   Sets the control font to the default state.
    /// </summary>
    procedure setFontDefault ;

    /// <summary>
    ///   Sets the control background to the alarm state.
    /// </summary>
    procedure setBackgroundAlarm ;

    /// <summary>
    ///   Sets the control background to the default state.
    /// </summary>
    procedure setBackgroundDefault ;

    /// <summary>
    ///   Parent control of the validator.
    /// </summary>
    property Control           : TGIS_PvlControl
                                 read fget_Control ;

    /// <summary>
    ///   OnChange property for the validator.
    /// </summary>
    property OnChange          : TGIS_PvlEvent
                                 read fget_OnChange
                                 write fset_OnChange ;

    /// <summary>
    ///   OnKeyPress property for the validator.
    /// </summary>
    property OnKeyPress        : TGIS_PvlKeyPressEvent
                                 read fget_OnKeyPress
                                 write fset_OnKeyPress ;

    /// <summary>
    ///   Original OnChange property of the parent control.
    /// </summary>
    property OnControlChange   : TGIS_PvlEvent
                                 read fget_OnControlChange
                                 write fset_OnControlChange ;

    /// <summary>
    ///   Original OnKeyPress property of the parent control.
    /// </summary>
    property OnControlKeyPress : TGIS_PvlKeyPressEvent
                                 read fget_OnControlKeyPress
                                 write fset_OnControlKeyPress ;
  end;

  {#gendoc:hide}
  /// <summary>
  ///   IGIS_Validator interface implementation.
  ///   Base class for text and value validator.
  /// </summary>
  TGIS_Validator<T> = {$IFDEF OXYGENE} abstract {$ENDIF}  class( TInterfacedObject, IGIS_Validator<T> )
  private
    FControlValidator : IGIS_ControlValidator ;
    FValidationMethod : TGIS_ValidationMethod<T> ;

  private
    /// <summary>
    ///   Invoked on parent conmtrol OnChange event.
    /// </summary>
    procedure doChange               ( _sender : TObject ) ;

  protected
    function  fget_IsValid : Boolean ; virtual ; abstract ;

    function  fget_Text              : String ;
    procedure fset_Text              ( const _value : String ) ;

    function  fget_ValidationMethod  : TGIS_ValidationMethod<T> ;
    procedure fset_ValidationMethod  ( const _value : TGIS_ValidationMethod<T> ) ;

  public
    /// <inheritdoc/>
    procedure Validate;

    /// <inheritdoc/>
    property IsValid           : Boolean
                                 read fget_IsValid ;

    /// <inheritdoc/>                                               
    property Text              : String
                                 read fget_Text
                                 write fset_Text ;

    /// <inheritdoc/>    
    property ValidationMethod  : TGIS_ValidationMethod<T>   
                                 read fget_ValidationMethod
                                 write fset_ValidationMethod ;  

    /// <summary>
    ///   Validator default constructor.
    /// </summary>
    /// <param name="_controlValidator">
    ///   instance of the control validator
    /// </param>
    constructor Create        ( const _controlValidator : IGIS_ControlValidator ) ;

    /// <summary>
    ///   Injected control validator.
    /// </summary>
    property ControlValidator : IGIS_ControlValidator read FControlValidator ;
  end;

  /// <summary>
  ///   IGIS_ControlValidator interface implementation.
  ///   Base class for PVL controls implementations
  /// </summary>
  TGIS_ControlValidator = {$IFDEF OXYGENE} abstract {$ENDIF} class( TInterfacedObject, IGIS_ControlValidator )
  private
    FControl           : TGIS_PvlControl ;
    FOnControlChange   : TGIS_PvlEvent ;
    FOnControlKeyPress : TGIS_PvlKeyPressEvent ;

  private
    function  fget_Control           : TGIS_PvlControl ;

    procedure fset_OnControlChange   ( const _value : TGIS_PvlEvent ) ;
    function  fget_OnControlChange   : TGIS_PvlEvent ;

    procedure fset_OnControlKeyPress ( const _value : TGIS_PvlKeyPressEvent ) ;
    function  fget_OnControlKeyPress : TGIS_PvlKeyPressEvent ;

  protected
    procedure fset_OnChange          ( const _value : TGIS_PvlEvent ) ; virtual ; abstract ;
    function  fget_OnChange          : TGIS_PvlEvent ; virtual ; abstract ;

    procedure fset_OnKeyPress        ( const _value : TGIS_PvlKeyPressEvent ) ; virtual ; abstract ;
    function  fget_OnKeyPress        : TGIS_PvlKeyPressEvent ; virtual ; abstract ;

    /// <inheritdoc/>
    procedure setText                ( const _text : String ) ; virtual ; abstract ;

    /// <inheritdoc/>
    function  getText                : String ; virtual ; abstract ;

    /// <inheritdoc/>
    procedure setFontAlarm           ; virtual ; abstract ;

    /// <inheritdoc/>
    procedure setFontDefault         ; virtual ; abstract ;

    /// <inheritdoc/>
    procedure setBackgroundAlarm     ; virtual ; abstract ;
                                     
    /// <inheritdoc/>
    procedure setBackgroundDefault   ; virtual ; abstract ;

  public
    /// <inheritdoc/>
    procedure DoControlChange ;

    /// <inheritdoc/>
    procedure DoControlKeyPress( var _key : Char ) ;

    /// <summary>
    ///   Default contructor.
    /// </summary>
    /// <param name="_control">
    ///   parent PVL control
    /// </param>
    constructor Create         ( const _control : TGIS_PvlControl ) ;

    /// <summary>
    ///   Parent PVL control.
    /// </summary>
    property Control           : TGIS_PvlControl
                                 read fget_Control ;

    /// <inheritdoc/>
    property OnControlChange   : TGIS_PvlEvent 
                                 read fget_OnControlChange
                                 write fset_OnControlChange ;

    /// <inheritdoc/>
    property OnControlKeyPress : TGIS_PvlKeyPressEvent 
                                 read fget_OnControlKeyPress
                                 write fset_OnControlKeyPress ;

    /// <inheritdoc/>
    property OnChange          : TGIS_PvlEvent 
                                 read fget_OnChange
                                 write fset_OnChange ; 
  end;

  /// <summary>
  ///   The control validator implementation for TGIS_PvlEdit.
  /// </summary>
  TGIS_ControlEditValidator = class( TGIS_ControlValidator{$IFDEF JAVA}, IGIS_ControlValidator{$ENDIF} )
  private
    function fget_ControlEdit : TGIS_PvlEdit ;

  protected
    procedure fset_OnChange   ( const _value : TGIS_PvlEvent ) ; override ;                               
    function fget_OnChange    : TGIS_PvlEvent ; override ;

    procedure fset_OnKeyPress ( const _value : TGIS_PvlKeyPressEvent ) ; override ;
    function fget_OnKeyPress  : TGIS_PvlKeyPressEvent ; override ;

    /// <inheritdoc/>
    procedure setText              ( const _text : String ) ; override ;
    
    /// <inheritdoc/>
    function  getText              : String ; override ;

    /// <inheritdoc/>
    procedure setFontAlarm         ; override ;

    /// <inheritdoc/>
    procedure setFontDefault       ; override ;

    /// <inheritdoc/>
    procedure setBackgroundAlarm   ; override ;

    /// <inheritdoc/>
    procedure setBackgroundDefault ; override ;

  public
    /// <summary>
    ///   Default constructor.
    /// </summary>
    /// <param name="_control">
    ///   parent TGIS_PvlEdit control
    /// </param>
    constructor Create         ( const _control: TGIS_PvlEdit ) ;

    /// <summary>
    ///   Parent TGIS_PvlEdit control.
    /// </summary>
    property ControlEdit       : TGIS_PvlEdit
                                 read fget_ControlEdit ;
  end;

  /// <summary>
  ///   The control validator implementation for TGIS_PvlComboBox.
  /// </summary>
  TGIS_ControlComboEditValidator = class( TGIS_ControlValidator{$IFDEF JAVA}, IGIS_ControlValidator{$ENDIF} )
  private
    function fget_ControlComboEdit : TGIS_PvlComboEdit ;

  protected
    procedure fset_OnChange        ( const _value : TGIS_PvlEvent ) ; override ;
    function fget_OnChange         : TGIS_PvlEvent ; override ;

    procedure fset_OnKeyPress      ( const _value : TGIS_PvlKeyPressEvent ) ; override ;
    function fget_OnKeyPress       : TGIS_PvlKeyPressEvent ; override ;

    /// <inheritdoc/>
    procedure setText              ( const _text : String ) ; override ;

    /// <inheritdoc/>
    function  getText              : String ; override ;

    /// <inheritdoc/>
    procedure setFontAlarm         ; override ;

    /// <inheritdoc/>
    procedure setFontDefault       ; override ;

    /// <inheritdoc/>
    procedure setBackgroundAlarm   ; override ;

    /// <inheritdoc/>
    procedure setBackgroundDefault ; override ;
    // implement in the same way as Edit
    constructor Create( const _control : TGIS_PvlComboEdit ) ;

    /// <summary>
    ///   Parent TGIS_PvlEdit control.
    /// </summary>
    property ControlComboEdit   : TGIS_PvlComboEdit
                                 read fget_ControlComboEdit ;
  end;

  /// <summary>
  ///   Implementation of the text validator.
  /// </summary>
  TGIS_TextValidator = class( TGIS_Validator<String>, IGIS_TextValidator )
  private
    FAllowEmpty      : Boolean ;
    FAllowWhiteSpace : Boolean ;
    
  protected
    function  fget_IsValid         : Boolean ; override ;
    
    function  fget_AllowEmpty      : Boolean ;
    procedure fset_AllowEmpty      ( const _value : Boolean ) ;

    function  fget_AllowWhiteSpace : Boolean ;
    procedure fset_AllowWhiteSpace ( const _value : Boolean ) ;
    
  public
    /// <inheritdoc/>
    constructor Create( const _controlValidator : IGIS_ControlValidator ) ;
  end;

  /// <summary>
  ///   Implementation of the text validator for TGIS_PvlEdit.
  /// </summary>
  TGIS_TextValidatorForEdit = class( TGIS_TextValidator )
  public
    /// <summary>
    ///   Default constructor.
    /// </summary>
    /// <param name="_control">
    ///   parent TGIS_PvlEdit control
    /// </param>
    constructor Create( const _control : TGIS_PvlEdit ) ;
  end;

  /// <summary>
  ///   Implementation of the text validator for TGIS_PvlEdit.
  /// </summary>
  TGIS_TextValidatorForComboEdit = class( TGIS_TextValidator )
  public
    /// <summary>
    ///   Default constructor.
    /// </summary>
    /// <param name="_control">
    ///   parent TGIS_PvlEdit control
    /// </param>
    constructor Create( const _control : TGIS_PvlComboEdit ) ;
  end;

  /// <summary>
  ///   Implementation of the value validator.
  /// </summary>
  TGIS_ValueValidator = class( TGIS_Validator<Double>, IGIS_ValueValidator )
  private
    FMinValue  : Double ;
    FMaxValue  : Double ;
    FPrecision : Byte ;
    
  private
    procedure fset_MinValue  ( const _value : Double ) ;
    function  fget_MinValue  : Double ;

    procedure fset_MaxValue  ( const _value : Double ) ;
    function  fget_MaxValue  : Double ;

    function  fget_Value     : Double ;
    procedure fset_Value     ( const _value : Double ) ;

    procedure fset_Precision ( const _value : Byte ) ;
    function  fget_Precision : Byte ;
    
    /// <summary>
    ///   Does basic input verification. Allows only known characters.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_key">
    ///   pressed key
    /// </param>
    procedure doKeyPress(     _sender : TObject ;
                          var _key    : Char
                        ) ;

  protected
    function  fget_IsValid   : Boolean ; override ;

  public
    /// <inheritdoc/>   
    constructor Create  ( const _controlValidator : IGIS_ControlValidator ) ;

    /// <inheritdoc/>
    function TryGetValue( out _value : Double ) : Boolean ;

     /// <inheritdoc/>
    property Value      : Double
                          read fget_Value
                          write fset_Value ;
  end;

  /// <summary>
  ///   Implementation of the value validator for TGIS_PvlEdit.
  /// </summary>
  TGIS_ValueValidatorForEdit = class( TGIS_ValueValidator )
  public
    /// <inheritdoc from="TGIS_ValueValidator"/>
    constructor Create( const _control : TGIS_PvlEdit ) ;
  end;

  /// <summary>
  ///   Implementation of the value validator for TGIS_PvlComboEdit.
  /// </summary>
  TGIS_ValueValidatorForComboEdit = class( TGIS_ValueValidator )
    public
    /// <inheritdoc from="TGIS_ValueValidator"/>
    constructor Create( const _control : TGIS_PvlComboEdit ) ;
  end;

{$REGION 'TGIS_TextValidator'}
constructor TGIS_TextValidator.Create( const _controlValidator : IGIS_ControlValidator ) ;
begin
  FAllowEmpty := True ;
  FAllowWhiteSpace := True ;

  inherited Create( _controlValidator ) ;
end;

// text validation takes place only here
function TGIS_TextValidator.fget_IsValid : Boolean ;
begin
  Result := True ;
  if not FAllowEmpty then begin
    Result := not IsStringEmpty( Text );
    if not Result then
      exit ;
  end;

  if not FAllowWhiteSpace then begin
    Result := not HasStringWhiteSpace( Text ) ;
    if not Result then
      exit ;
  end;

  if assigned( ValidationMethod ) then
    Result := ValidationMethod( Text ) ;
end;

function TGIS_TextValidator.fget_AllowEmpty : Boolean ;
begin
  Result := FAllowEmpty ;
end;

function TGIS_TextValidator.fget_AllowWhiteSpace : Boolean ;
begin
  Result := FAllowWhiteSpace ;
end;

procedure TGIS_TextValidator.fset_AllowEmpty( const _value : Boolean ) ;
begin
  if _value = FAllowEmpty then
    exit ;

  FAllowEmpty := _value ;
  Validate ;
end;

procedure TGIS_TextValidator.fset_AllowWhiteSpace( const _value : Boolean ) ;
begin
  if _value = FAllowWhiteSpace then
    exit ;

  FAllowWhiteSpace := _value ;
  Validate ;
end;
{$ENDREGION}

{$REGION 'TGIS_TextValidatorForEdit'}
constructor TGIS_TextValidatorForEdit.Create( const _control: TGIS_PvlEdit ) ;
var
  controlEditValidator : IGIS_ControlValidator ;
begin
  controlEditValidator := TGIS_ControlEditValidator.Create( _control ) ;

  inherited Create( controlEditValidator ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_TextValidatorForComboEdit'}
constructor TGIS_TextValidatorForComboEdit.Create( const _control: TGIS_PvlComboEdit ) ;
var
  controlEditValidator : IGIS_ControlValidator ;
begin
  controlEditValidator := TGIS_ControlComboEditValidator.Create( _control ) ;

  inherited Create( controlEditValidator ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_ControlValidator'}
constructor TGIS_ControlValidator.Create( const _control : TGIS_PvlControl ) ;
begin
  FControl := _control ;
end;

procedure TGIS_ControlValidator.DoControlChange;
begin
  if assigned( OnControlChange ) then
    OnControlChange( Control ) ;
end;

procedure TGIS_ControlValidator.DoControlKeyPress( var _key : Char ) ;
begin
  if assigned( OnControlKeyPress ) then
    OnControlKeyPress( Control, _key ) ;
end;

function TGIS_ControlValidator.fget_Control : TGIS_PvlControl ;
begin
  Result := FControl ;
end;
function TGIS_ControlValidator.fget_OnControlChange : TGIS_PvlEvent ;
begin
  Result := FOnControlChange ;
end;

function TGIS_ControlValidator.fget_OnControlKeyPress : TGIS_PvlKeyPressEvent ;
begin
  Result := FOnControlKeyPress ;
end;

procedure TGIS_ControlValidator.fset_OnControlChange( const _value : TGIS_PvlEvent ) ;
begin
  FOnControlChange := _value ;
end;

procedure TGIS_ControlValidator.fset_OnControlKeyPress( const _value : TGIS_PvlKeyPressEvent ) ;
begin
  FOnControlKeyPress := _value ;
end;
{$ENDREGION}

{$REGION 'TGIS_Validator'}
constructor TGIS_Validator<T>.Create( const _controlValidator : IGIS_ControlValidator ) ;
begin
  FControlValidator := _controlValidator ;        

  {$IFDEF DCC}
    ControlValidator.OnChange := doChange ;
  {$ENDIF}
  {$IFDEF OXYGENE}
    ControlValidator.OnChange := @doChange ;
  {$ENDIF}

  Validate ;
end;

procedure TGIS_Validator<T>.doChange( _sender : TObject ) ;
begin
  Validate ;

  ControlValidator.DoControlChange ;
end;

function TGIS_Validator<T>.fget_Text : String ;
begin
  Result := ControlValidator.getText ;
end;

function TGIS_Validator<T>.fget_ValidationMethod : TGIS_ValidationMethod<T> ;
begin
  Result := FValidationMethod ;
end;

procedure TGIS_Validator<T>.fset_Text( const _value : String ) ;
begin
  ControlValidator.setText( _value ) ;
end;

procedure TGIS_Validator<T>.fset_ValidationMethod( const _value : TGIS_ValidationMethod<T> ) ;
begin
  FValidationMethod := _value ;
  Validate ;
end;

procedure TGIS_Validator<T>.Validate ;
begin
  if not IsValid then begin
    if IsStringEmptyOrWhiteSpace( Text ) then begin 
      ControlValidator.setBackgroundAlarm ;
      ControlValidator.setFontDefault ;
    end
    else begin
      ControlValidator.setFontAlarm ;
      ControlValidator.setBackgroundDefault ;
    end ;
  end
  else begin
    ControlValidator.setFontDefault ;
    ControlValidator.setBackgroundDefault ;
  end;
end;
{$ENDREGION}

{$REGION 'TGIS_ControlEditValidator'}
constructor TGIS_ControlEditValidator.Create( const _control : TGIS_PvlEdit ) ;
begin
  inherited Create( _control ) ;

  if _control = nil then exit;

  OnControlChange := _control.OnChange ;
  OnControlKeyPress := _control.OnKeyPress ;
end;

function TGIS_ControlEditValidator.fget_ControlEdit : TGIS_PvlEdit ;
begin
  Result := TGIS_PvlEdit( Control ) ;
end;

function TGIS_ControlEditValidator.fget_OnChange : TGIS_PvlEvent ;
begin
  Result := ControlEdit.OnChange ;
end;

function TGIS_ControlEditValidator.fget_OnKeyPress : TGIS_PvlKeyPressEvent ;
begin
  Result := ControlEdit.OnKeyPress ;
end;

procedure TGIS_ControlEditValidator.fset_OnChange( const _value : TGIS_PvlEvent ) ;
begin
  ControlEdit.OnChange := _value ;
end;

procedure TGIS_ControlEditValidator.fset_OnKeyPress( const _value : TGIS_PvlKeyPressEvent ) ;
begin
  ControlEdit.OnKeyPress := _value ;
end;

function TGIS_ControlEditValidator.getText : String;
begin
  Result := ControlEdit.Text ;
end;

procedure TGIS_ControlEditValidator.setBackgroundAlarm ;
begin
  ControlEdit.SetBackgroundAlarm ;
end;

procedure TGIS_ControlEditValidator.setBackgroundDefault ;
begin
  ControlEdit.SetBackgroundDefault ;
end;

procedure TGIS_ControlEditValidator.setFontAlarm ;
begin
  ControlEdit.SetFontAlarm ;
end;

procedure TGIS_ControlEditValidator.setFontDefault ;
begin
  ControlEdit.SetFontDefault ;
end;

procedure TGIS_ControlEditValidator.setText( const _text : String ) ;
{$IFDEF JAVA}
var
  oldChange : TGIS_PvlEvent ;
{$ENDIF}
begin
  {$IFDEF JAVA}
    oldChange := ControlEdit.OnChange ;
    ControlEdit.OnChange := nil ;
    ControlEdit.Text := _text ;
    ControlEdit.OnChange := oldChange ;
  {$ELSE}
    ControlEdit.Text := _text ;
  {$ENDIF}
end;
{$ENDREGION}

{$REGION 'TGIS_ControlComboEditValidator'}
constructor TGIS_ControlComboEditValidator.Create( const _control : TGIS_PvlComboEdit ) ;
begin
  inherited Create( _control ) ;

  if _control = nil then exit;

  OnControlChange := _control.OnChange ;
  OnControlKeyPress := _control.OnKeyPress ;
end;

function TGIS_ControlComboEditValidator.fget_ControlComboEdit : TGIS_PvlComboEdit ;
begin
  Result := TGIS_PvlComboEdit( Control ) ;
end;

function TGIS_ControlComboEditValidator.fget_OnChange : TGIS_PvlEvent ;
begin
  Result := ControlComboEdit.OnChange ;
end;

function TGIS_ControlComboEditValidator.fget_OnKeyPress : TGIS_PvlKeyPressEvent ;
begin
  Result := ControlComboEdit.OnKeyPress ;
end;

procedure TGIS_ControlComboEditValidator.fset_OnChange( const _value : TGIS_PvlEvent ) ;
begin
  ControlComboEdit.OnChange := _value ;
end;

procedure TGIS_ControlComboEditValidator.fset_OnKeyPress( const _value : TGIS_PvlKeyPressEvent ) ;
begin
  ControlComboEdit.OnKeyPress := _value ;
end;

function TGIS_ControlComboEditValidator.getText : String;
begin
  Result := ControlComboEdit.Text ;
end;

procedure TGIS_ControlComboEditValidator.setBackgroundAlarm ;
begin
  ControlComboEdit.SetBackgroundAlarm ;
end;

procedure TGIS_ControlComboEditValidator.setBackgroundDefault ;
begin
  ControlComboEdit.SetBackgroundDefault ;
end;

procedure TGIS_ControlComboEditValidator.setFontAlarm ;
begin
  ControlComboEdit.SetFontAlarm ;
end;

procedure TGIS_ControlComboEditValidator.setFontDefault ;
begin
  ControlComboEdit.SetFontDefault ;
end;

procedure TGIS_ControlComboEditValidator.setText( const _text : String ) ;
{$IFDEF JAVA}
var
  oldChange : TGIS_PvlEvent ;
{$ENDIF}
begin
  {$IFDEF JAVA}
    oldChange := ControlComboEdit.OnChange ;
    ControlComboEdit.OnChange := nil ;
    ControlComboEdit.Text := _text ;
    ControlComboEdit.OnChange := oldChange ;
  {$ELSE}
    ControlComboEdit.Text := _text ;
  {$ENDIF}
end;
{$ENDREGION}

{$REGION 'TGIS_ValueValidator'}
constructor TGIS_ValueValidator.Create( const _controlValidator : IGIS_ControlValidator ) ;
begin
  FMinValue := -GIS_MAX_DOUBLE ;
  FMaxValue := GIS_MAX_DOUBLE ;
  FPrecision := 0 ;

  inherited Create( _controlValidator ) ;

  {$IFDEF DCC}
    ControlValidator.OnKeyPress := doKeyPress ;
  {$ENDIF}
  {$IFDEF OXYGENE}
    ControlValidator.fset_OnKeyPress(@doKeyPress)  ;
  {$ENDIF}
end;

procedure TGIS_ValueValidator.doKeyPress(
      _sender : TObject ;
  var _key    : Char
) ;

  function acceptKey( const _key : Char) : Boolean ;
  {$IFDEF JAVA}
  const
    CHAR_SET : TSysCharSet = [ '0','1','2','3','4','5','6','7','8','9', '-', '.', ',' ] ;
  {$ENDIF}
  begin
    Result := True ;

    if (FPrecision <= 0) and CharInSet( _key, [ '.', ',' ] ) then
      Result := False
    {$IFDEF JAVA}
    else if not CharInSet( _key, CHAR_SET ) then
    {$ELSE}
    else if not CharInSet( _key, [Char(8), '0'..'9', '-', '.', ',']) then
    {$ENDIF}
      Result := False ;
  end ;
begin
  try
    if _key < Char(32) then exit;

    if ( LowerCase( _key ) = 'e' ) then begin
      if Pos('e', LowerCase(Text) ) > 0 then
        _key := Char( 0 );

      exit ;
    end ;

    if not acceptKey( _key ) then
      _key := Char( 0 ) ;

  finally
    ControlValidator.DoControlKeyPress( _key ) ;
  end;
end ;

// value validation takes place only here
function TGIS_ValueValidator.fget_IsValid : Boolean ;
var
  tmp_value        : Double ;
  tmp_text         : String ;
  dot_pos, e_pos   : Integer ;
  val_str, exp_str : String ;
  exp              : Integer ;
  e_notation       : TArray<String> ;
begin
  Result := True ;

  {$IFNDEF JAVA}
    Text := Trim( Text ) ;
  {$ENDIF}

  tmp_text := Text ;
  Result := not IsStringEmptyOrWhiteSpace( tmp_text ) ;
  if Result = False then exit ;

  Result := TryGetValue( tmp_value ) ;
  if Result = False then exit ;

  tmp_text := DotFloatToStr( tmp_value) ;

  dot_pos := Max( Text.IndexOf( '.' ), Text.IndexOf( ',' ) ) ;
  if ( dot_pos <> -1 ) and ( FPrecision > 0 ) then begin
    e_pos := LowerCase( Text ).IndexOf( 'e' ) ;

    // non scientific notation (E)
    if e_pos = -1 then begin
      Result := length( Text ) - dot_pos - 1 <= FPrecision ;
    end
    else begin
      {$IFDEF JAVA OR ISLAND}
      e_notation := LowerCase( Text ).Split( 'e' ).ToArray ;
      {$ELSE}
      e_notation := LowerCase( Text ).Split( ['e'] ) ;
      {$ENDIF}

      val_str := e_notation[0] ;
      exp_str := e_notation[1] ;

      if TryStrToInt( exp_str, exp ) = False then exit ;

      Result := length( val_str ) - dot_pos - 1 <= FPrecision + exp ;
    end ;

    if Result = False then exit ;
  end;

  Result := ( tmp_value >= FMinValue ) and ( tmp_value <= FMaxValue ) ;
  if Result = False then exit ;

  if assigned( ValidationMethod ) then
    Result := ValidationMethod( tmp_value ) ;
end ;

function TGIS_ValueValidator.fget_MaxValue : Double ;
begin
  Result := FMaxValue ;
end;

function TGIS_ValueValidator.fget_MinValue : Double ;
begin
  Result := FMinValue ;
end;

function TGIS_ValueValidator.fget_Precision : Byte ;
begin
  Result := FPrecision ;
end;

function TGIS_ValueValidator.fget_Value : Double ;
begin
  if not IsStringEmptyOrWhiteSpace( Text ) then
    Result := DotStrToFloat( Text ) 
  else
    Result := 0 ;
end;

procedure TGIS_ValueValidator.fset_MaxValue( const _value : Double ) ;
begin
  if _value = FMaxValue then exit ;

  FMaxValue := _value ;
  Validate ;
end;

procedure TGIS_ValueValidator.fset_MinValue( const _value : Double ) ;
begin
  if _value = FMinValue then exit ;

  FMinValue := _value ;
  Validate ;
end;

procedure TGIS_ValueValidator.fset_Precision( const _value : Byte ) ;
begin
  if _value = FPrecision then exit ;

  FPrecision := _value ;
  Validate ;
end;

procedure TGIS_ValueValidator.fset_Value( const _value : Double ) ;
begin
  Text := DotFloatToStr( _value ) ;

  Validate ; 
end;

function TGIS_ValueValidator.TryGetValue( out _value : Double ) : Boolean;
begin
  Result := True ;
  try
    _value := Value ;
  except
    Result := False ;
  end;
end;
{$ENDREGION}

{$REGION 'TGIS_ValueValidatorForEdit'}
constructor TGIS_ValueValidatorForEdit.Create( const _control : TGIS_PvlEdit ) ;
var
  controlEditValidator : IGIS_ControlValidator ;
begin
  controlEditValidator := TGIS_ControlEditValidator.Create( _control ) ;

  inherited Create( controlEditValidator ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_ValueValidatorForComboEdit'}
constructor TGIS_ValueValidatorForComboEdit.Create( const _control : TGIS_PvlComboEdit ) ;
var
  controlEditValidator : IGIS_ControlValidator ;
begin
  controlEditValidator := TGIS_ControlComboEditValidator.Create( _control ) ;

  inherited Create( controlEditValidator ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_PvlEditHelper'}
function TGIS_PvlEditHelper.CreateTextValidator : IGIS_TextValidator ;
begin
  Result := TGIS_TextValidatorForEdit.Create( Self ) ;
end;

function TGIS_PvlEditHelper.CreateValueValidator : IGIS_ValueValidator ;
begin
  Result := TGIS_ValueValidatorForEdit.Create( Self ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_PvlComboEditHelper'}

function TGIS_PvlComboEditHelper.CreateTextValidator : IGIS_TextValidator ;
begin
  Result := TGIS_TextValidatorForComboEdit.Create( Self ) ;
end;

function TGIS_PvlComboEditHelper.CreateValueValidator : IGIS_ValueValidator ;
begin
  Result := TGIS_ValueValidatorForComboEdit.Create( Self ) ;
end;
{$ENDREGION}

end.
