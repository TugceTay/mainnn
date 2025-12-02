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
  This is a field rule unit.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFieldRules ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFieldRules"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Collections.Generic,
    System.Text.RegularExpressions,
    TatukGIS.RTL,
    TatukGIS.NDK,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Variants,
    System.SysUtils,
    System.Generics.Collections,
    System.RegularExpressions,

    Lider.CG.GIS.GeoTypes;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}

type

  /// <summary>
  ///   Alias value representation. For situations where data attributes value
  ///   should be presented on a screen differently. Aliases are always
  ///   presented as strings.
  /// </summary>
  TGIS_FieldValueAlias = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FAlias : String ;
      FValue : String ;

    public

      /// <summary>
      ///   Create a single alias.
      /// </summary>
      /// <param name="_alias">
      ///   value to be aliased
      /// </param>
      /// <param name="_value">
      ///   value of the alias
      /// </param>
      constructor Create( const _alias : String ;
                          const _value : String
                        ) ;
    public

        /// <summary>
        ///   Alias to attribute values.
        /// </summary>
        property Alias : String read  FAlias ;

        /// <summary>
        ///   Attribute value which should be aliased by Alias.
        /// </summary>
        property Value : String read  FValue ;
  end ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   List of TGIS_FieldValueAlias.
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_FieldValueAliasList = {$IFDEF OXYGENE} public {$ENDIF}
                               TObjectList< TGIS_FieldValueAlias > ;
  {$ELSE}
    TGIS_FieldValueAliasList = class(
                                 TObjectList< TGIS_FieldValueAlias >
                               ) ;
  {$ENDIF}

  /// <summary>
  ///   List of aliases. See also TGIS_FieldValueAlias.
  /// </summary>
  TGIS_FieldValueAliases = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FAliases : TGIS_FieldValueAliasList ;

    public

      /// <summary>
      ///   Create aliases list.
      /// </summary>
      constructor Create ;
      {$IFNDEF MANAGED}

        /// <summary>
        ///   Destroy aliases list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}
    public

      /// <summary>
      ///   Resolve value in a meaning of reading it from a layer. Value will
      ///   be substituted with alias
      /// </summary>
      /// <param name="_value">
      ///   value form a layer
      /// </param>
      /// <returns>
      ///   value to be presented to a user
      /// </returns>
      function ResolveGet( const _value : String  ) : String ;

      /// <summary>
      ///   Resolve value in a meaning of saving it to a layer. Alias will be
      ///   resolved to a corresponding field value.
      /// </summary>
      /// <param name="_value">
      ///   value to be check
      /// </param>
      /// <returns>
      ///   value to be saved to a layer
      /// </returns>
      function ResolveSet( const _value : Variant ) : Variant ;

    public
       /// <summary>
       ///   List of all aliases.
       /// </summary>
       property Aliases : TGIS_FieldValueAliasList read FAliases ;
  end ;

  /// <summary>
  ///   Behavior of value checks.
  /// </summary>
  TGIS_FieldValueCheckMode = {$IFDEF OXYGENE} public {$ENDIF} (

    /// <summary>
    ///   Value should be checked after editing.
    /// </summary>
    AfterEdit,

    /// <summary>
    ///   Value should be checked upon any changed (any key entered).
    /// </summary>
    OnEdit,

    /// <summary>
    ///   Value should be checked in both cases.
    /// </summary>
    Both
  ) ;

type

  /// <summary>
  ///   Type of formula checks.
  /// </summary>
  TGIS_FieldValueCheckFormula = {$IFDEF OXYGENE} public {$ENDIF} (

    /// <summary>
    ///   Value is required.
    /// </summary>
    Required,

    /// <summary>
    ///   Value is check against regular expression.
    /// </summary>
    Regex,

    /// <summary>
    ///   Value is checked against formula (like math expression)
    ///   or callback function.
    /// </summary>
    Expression,

    /// <summary>
    ///   Value is equal to string, case sensitive.
    /// </summary>
    StringEQ,

    /// <summary>
    ///   Value is not equal to string, case sensitive.
    /// </summary>
    StringNE,

    /// <summary>
    ///   Value is is less the string, case sensitive.
    /// </summary>
    StringLT,

    /// <summary>
    ///   Value is is less equal then string, case sensitive.
    /// </summary>
    StringLE,

    /// <summary>
    ///   Value is is great then string, case sensitive.
    /// </summary>
    StringGT,

    /// <summary>
    ///   Value is is great equal then string, case sensitive.
    /// </summary>
    StringGE,

    /// <summary>
    ///   Value is is one of provided strings, case sensitive; NOT SUPPORTED YET.
    /// </summary>
    StringIn,

    /// <summary>
    ///   Value is equal to text, case insensitive.
    /// </summary>
    TextEQ,

    /// <summary>
    ///   Value is not equal to text, case insensitive.
    /// </summary>
    TextNE,

    /// <summary>
    ///   Value is is less the text, case insensitive.
    /// </summary>
    TextLT,

    /// <summary>
    ///   Value is is less equal then text, case insensitive.
    /// </summary>
    TextLE,

    /// <summary>
    ///   Value is is great then text, case insensitive.
    /// </summary>
    TextGT,

    /// <summary>
    ///   Value is is great equal then text, case insensitive.
    /// </summary>
    TextGE,

    /// <summary>
    ///   Value is is one of provided text, case insensitive; NOT SUPPORTED YET.
    /// </summary>
    TextIn,

    /// <summary>
    ///   Proper number required.
    /// </summary>
    NumberRequired,

    /// <summary>
    ///   Value is equal to number.
    /// </summary>
    NumberEQ,

    /// <summary>
    ///   Value is not equal to number.
    /// </summary>
    NumberNE,

    /// <summary>
    ///   Value is is less then the number.
    /// </summary>
    NumberLT,

    /// <summary>
    ///   Value is is less equal then the number.
    /// </summary>
    NumberLE,

    /// <summary>
    ///   Value is is great then the number.
    /// </summary>
    NumberGT,

    /// <summary>
    ///   Value is is great equal then the number.
    /// </summary>
    NumberGE,

    /// <summary>
    ///   Proper date required; NOT SUPPORTED YET.
    /// </summary>
    DateRequired,

    /// <summary>
    ///   Value is equal to date; NOT SUPPORTED YET.
    /// </summary>
    DateEQ,

    /// <summary>
    ///   Value is not equal to date; NOT SUPPORTED YET.
    /// </summary>
    DateNE,

    /// <summary>
    ///   Value is not equal to date; NOT SUPPORTED YET.
    /// </summary>
    DateLT,

    /// <summary>
    ///   Value is is less equal then the date; NOT SUPPORTED YET.
    /// </summary>
    DateLE,

    /// <summary>
    ///   Value is is great then the date; NOT SUPPORTED YET.
    /// </summary>
    DateGT,

    /// <summary>
    ///   Value is is great equal then the date; NOT SUPPORTED YET.
    /// </summary>
    DateGE
  ) ;

type

  /// <summary>
  ///   Value check event used for custom validation.
  /// </summary>
  /// <param name="_value">
  ///   value to check
  /// </param>
  /// <returns>
  ///   True if value is valid
  /// </returns>
  TGIS_FieldValueCheckEvent = {$IFDEF OXYGENE} public {$ENDIF} function(
    _value  : String
  ) : Boolean of object ;

  /// <summary>
  ///   Value check representation. For situations where entered data must be
  ///   check against provided formula
  /// </summary>
  TGIS_FieldValueCheck = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FMode     : TGIS_FieldValueCheckMode ;
      FFormula  : TGIS_FieldValueCheckFormula ;
      FContent  : String ;
      FMessage  : String ;
      FCallback : TGIS_FieldValueCheckEvent ;
    public

      /// <summary>
      ///   Create a single value check.
      /// </summary>
      /// <param name="_mode">
      ///   mode of check
      /// </param>
      /// <param name="_formula">
      ///   formula of check
      /// </param>
      /// <param name="_content">
      ///   formula content
      /// </param>
      /// <param name="_message">
      ///   message to be provided upon error
      /// </param>
      constructor Create( const _mode    : TGIS_FieldValueCheckMode ;
                          const _formula : TGIS_FieldValueCheckFormula ;
                          const _content : String ;
                          const _message : String
                      ) ; overload ;

      /// <summary>
      ///   Create a single value check.
      /// </summary>
      /// <param name="_mode">
      ///   mode of check
      /// </param>
      /// <param name="_formula">
      ///   formula of check
      /// </param>
      /// <param name="_callback">
      ///   callback function to check value
      /// </param>
      /// <param name="_message">
      ///   message to be provided upon error
      /// </param>
      constructor Create( const _mode     : TGIS_FieldValueCheckMode ;
                          const _formula  : TGIS_FieldValueCheckFormula ;
                          const _callback : TGIS_FieldValueCheckEvent ;
                          const _message  : String
                      ) ; overload ;
    public

      /// <summary>
      ///   Check a value against formula.
      /// </summary>
      /// <param name="_value">
      ///   value to be check
      /// </param>
      /// <param name="_mode">
      ///   mode of check
      /// </param>
      /// <param name="_msg">
      ///   string with optional error message
      /// </param>
      /// <returns>
      ///   False if _value does not fulfill requirements
      /// </returns>
      function Check ( const _value : Variant ;
                       const _mode  : TGIS_FieldValueCheckMode ;
                       out   _msg   : String
                     ) : Boolean ;

        /// <summary>
        ///   Mode of check.
        /// </summary>
        property Mode    : TGIS_FieldValueCheckMode    read FMode    ;

        /// <summary>
        ///   Formula of check.
        /// </summary>
        property Formula : TGIS_FieldValueCheckFormula read FFormula ;

        /// <summary>
        ///   Formula content.
        /// </summary>
        property Content : String                      read FContent ;

        /// <summary>
        ///   Message to be provided upon error.
        /// </summary>
        property Message : String                      read FMessage ;
  end ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   List of TGIS_FieldValueCheck.
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_FieldValueCheckList = {$IFDEF OXYGENE} public {$ENDIF}
                               TObjectList< TGIS_FieldValueCheck > ;
  {$ELSE}
    TGIS_FieldValueCheckList = class(
                                 TObjectList< TGIS_FieldValueCheck >
                               ) ;
  {$ENDIF}

  /// <summary>
  ///   List of checks. See also TGIS_FieldValueCheck.
  /// </summary>
  TGIS_FieldValueChecks = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FChecks : TGIS_FieldValueCheckList ;
    public

      /// <summary>
      ///   Create value check list.
      /// </summary>
      constructor Create ;
      {$IFNDEF MANAGED}

        /// <summary>
        ///   Destroy value check list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}
    public

      /// <summary>
      ///   Check a value against whole list.
      /// </summary>
      /// <param name="_value">
      ///   value to be check
      /// </param>
      /// <param name="_mode">
      ///   mode of check
      /// </param>
      /// <param name="_msg">
      ///   string with optional error message
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    order of the list is important
      ///    </note>
      /// </remarks>
      /// <returns>
      ///   False if _value does not fulfill requirements
      /// </returns>
      function Check ( const _value : Variant ;
                       const _mode  : TGIS_FieldValueCheckMode ;
                       out   _msg   : String
                     ) : Boolean ;
    public
      /// <summary>
      ///   List of all checks. Order is important. Checks are always
      ///   verify  form first to last until first problem occurs.
      /// </summary>
      property Checks : TGIS_FieldValueCheckList read FChecks ;
  end ;

  /// <summary>
  ///   Hint of how filed should be represented upon editing.
  /// </summary>
  TGIS_FieldValuesMode = {$IFDEF OXYGENE} public {$ENDIF} (

    /// <summary>
    ///   Represent as drop-down list.
    /// </summary>
    SelectList ,

    /// <summary>
    ///   Represent as multi-line edit box.
    /// </summary>
    MultiLine  ,

    /// <summary>
    ///   Represent as standard edit box.
    /// </summary>
    Edit       ,

    /// <summary>
    ///   Hide an attribute.
    /// </summary>
    Hidden
  ) ;

type

  /// <summary>
  ///   List of allowed values for a list.
  /// </summary>
  TGIS_FieldValues = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FMode  : TGIS_FieldValuesMode ;
      FItems : TGIS_ListOfStrings ;
      FDefaultValue : Variant ;

    private
      procedure fset_Mode         ( const _value : TGIS_FieldValuesMode
                                  ) ;
      procedure fset_DefaultValue ( const _value : Variant
                                  ) ;

    public

      /// <summary>
      ///   Create values list.
      /// </summary>
      /// <param name="_mode">
      ///   mode of presentation
      /// </param>
      /// <param name="_defaultvalue">
      ///   default filed value
      /// </param>
      constructor Create( const _mode         : TGIS_FieldValuesMode ;
                          const _defaultvalue : String
                        ) ;
      {$IFNDEF MANAGED}

        /// <summary>
        ///   Destroy valued list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}
    public

        /// <summary>
        ///   Mode of presentation.
        /// </summary>
        property Mode         : TGIS_FieldValuesMode read  FMode
                                                     write fset_Mode ;

        /// <summary>
        ///   Allowed values.
        /// </summary>
        property Items        : TGIS_ListOfStrings   read  FItems ;

        /// <summary>
        ///   Default new value.
        /// </summary>
        property DefaultValue : Variant              read  FDefaultValue
                                                     write fset_DefaultValue ;
  end;

  /// <summary>
  ///   Class responsible for complete rule based editing of the attribute field.
  /// </summary>
  TGIS_FieldRule = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FCaption      : String ;
      FLongName     : String ;
      FValueChecks  : TGIS_FieldValueChecks  ;
      FValueAliases : TGIS_FieldValueAliases ;
      FValues       : TGIS_FieldValues       ;
      FValueFormat  : String ;
      FModified     : Boolean ;

    public
      /// <summary>
      ///   Create an object.
      /// </summary>
      constructor Create ;

      {$IFNDEF MANAGED}
        /// <summary>
        ///   Destroy an object.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}

    public

      /// <summary>
      ///   Resolve value in a meaning of reading it form a layer. Value will
      ///   be substituted with alias
      /// </summary>
      /// <param name="_value">
      ///   value form a layer
      /// </param>
      /// <param name="_fld">
      ///   Field definition
      /// </param>
      /// <returns>
      ///   value to be presented to a user
      /// </returns>
      function ResolveGet( const _value  : Variant ;
                           const _fld    : TObject
                         ) : Variant ;

      /// <summary>
      ///   Resolve value in a meaning of saving it to a layer. Alias will be
      ///   resolved to a corresponding field value and field will be verified
      ///   against formulas.
      /// </summary>
      /// <param name="_setvalue">
      ///   value to be check
      /// </param>
      /// <param name="_retvalue">
      ///   value to be saved to a layer
      /// </param>
      /// <param name="_msg">
      ///   optional error message
      /// </param>
      /// <returns>
      ///   False if _value does not fulfill requirements
      /// </returns>
      function ResolveSet( const _setvalue : Variant ;
                           var   _retvalue : Variant ;
                           var   _msg    : String
                         ) : Boolean ;

      /// <summary>
      ///   Check value against all formulas.
      /// </summary>
      /// <param name="_setvalue">
      ///   value to be check
      /// </param>
      /// <param name="_mode">
      ///   mode of check
      /// </param>
      /// <param name="_msg">
      ///   optional error message
      /// </param>
      /// <returns>
      ///   False if _value does not fulfill requirements
      /// </returns>
      function Check     ( const _setvalue : Variant ;
                           const _mode     : TGIS_FieldValueCheckMode ;
                           var   _msg      : String
                         ) : Boolean ;
    public

        /// <summary>
        ///   Field caption.
        /// </summary>
        property Caption      : String                 read  FCaption
                                                       write FCaption      ;

        /// <summary>
        ///   Field long name (w/o taking care of file format limits).
        /// </summary>
        property LongName     : String                 read  FLongName     ;

        /// <summary>
        ///   List of all checks associated the the filed.
        /// </summary>
        property ValueChecks  : TGIS_FieldValueChecks  read  FValueChecks  ;

        /// <summary>
        ///   List of all filed aliases.
        /// </summary>
        property ValueAliases : TGIS_FieldValueAliases read  FValueAliases ;

        /// <summary>
        ///   List of all allowed values.
        /// </summary>
        property Values       : TGIS_FieldValues       read  FValues       ;

        /// <summary>
        ///   Value format specifier like in printf.
        /// </summary>
        property ValueFormat  : String                 read  FValueFormat
                                                       write FValueFormat  ;

        /// <summary>
        ///   True if rules were modified.
        /// </summary>
        property Modified     : Boolean                read  FModified
                                                       write FModified     ;
  end ;

  /// <summary>
  ///   Contains methods for saving and loading fieldrules.
  /// </summary>
  TGIS_FieldRulesOperations = {$IFDEF OXYGENE} public {$ENDIF} class
      public
        /// <summary>
        ///   Parse .fldx file and read all information into _parent layer
        /// </summary>
        /// <param name="_path">
        ///   path to a .fldx file
        /// </param>
        /// <param name="_parent">
        ///   parent layer
        /// </param>
        /// <returns>
        ///   True if .fldx file properly interpreted
        /// </returns>
        class function ParseFldx( const _path : String; const _parent : TObject ) : Boolean ;

        /// <summary>
        ///   Save filed rules into .fldx file
        /// </summary>
        /// <param name="_path">
        ///   path to a .fldx file
        /// </param>
        /// <param name="_parent">
        ///   parent layer
        /// </param>
        class procedure SaveFldx( const _path : String; const _parent : TObject ) ;
  end;

//##############################################################################
implementation

{$IFNDEF OXYGENE}
  uses
    System.Math,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoXmlDoc,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

{$REGION 'TGIS_FieldValueAlias'}

constructor TGIS_FieldValueAlias.Create(
  const _alias : String ;
  const _value : String
) ;
begin
  FAlias := _alias ;
  FValue := _value ;
end;

{$ENDREGION}

{$REGION 'TGIS_FieldValueAliases'}

constructor TGIS_FieldValueAliases.Create;
begin
  inherited ;

  FAliases := TGIS_FieldValueAliasList.Create( True ) ;
end;

{$IFNDEF MANAGED}
  destructor TGIS_FieldValueAliases.Destroy;
  begin
    FreeObject( FAliases ) ;
    inherited ;
  end;
{$ENDIF}

function TGIS_FieldValueAliases.ResolveGet(
  const _value : String
) : String ;
{$IFNDEF OXYGENE}
  var
    o : TGIS_FieldValueAlias ;
{$ENDIF}
begin
  Result := _value ;
  {$IFDEF OXYGENE}
    for each o in FAliases do begin
    if o.Value = _value then begin
      Result := o.Alias ;
      break ;
    end;
  end;
  {$ELSE}
    for o in FAliases do begin
    if o.Value = _value then begin
      Result := o.Alias ;
      break ;
    end;
  end;
  {$ENDIF}
end;

function TGIS_FieldValueAliases.ResolveSet(
  const _value : Variant
) : Variant ;
{$IFNDEF OXYGENE}
  var
    o : TGIS_FieldValueAlias ;
{$ENDIF}
begin
  Result := _value ;
  {$IFDEF OXYGENE}
    for each o in FAliases do begin
      if o.Alias = _value then begin
        Result := o.Value ;
        break ;
      end;
    end;
  {$ELSE}
    for o in FAliases do begin
      if o.Alias = _value then begin
        Result := o.Value ;
        break ;
      end;
    end;
  {$ENDIF}
end;

{$ENDREGION}

{$REGION 'TGIS_FieldValueCheck'}

constructor TGIS_FieldValueCheck.Create(
  const _mode    : TGIS_FieldValueCheckMode ;
  const _formula : TGIS_FieldValueCheckFormula ;
  const _content : String ;
  const _message : String
) ;
begin
  inherited Create ;

  FMode     := _mode    ;
  FFormula  := _formula ;
  FContent  := _content ;
  FMessage  := _message ;
  FCallback := nil ;
end;

constructor TGIS_FieldValueCheck.Create(
  const _mode     : TGIS_FieldValueCheckMode ;
  const _formula  : TGIS_FieldValueCheckFormula ;
  const _callback : TGIS_FieldValueCheckEvent ;
  const _message  : String
) ;
begin
  inherited Create ;

  FMode     := _mode    ;
  FFormula  := _formula ;
  FCallback := _callback ;
  FMessage  := _message ;
  FContent  := '';
end ;

function TGIS_FieldValueCheck.Check(
  const _value : Variant ;
  const _mode  : TGIS_FieldValueCheckMode ;
  out   _msg   : String
) : Boolean ;
var
  {$IFDEF OXYGENE}
    regex : Regex ;
  {$ELSE}
    regex : TRegEx ;
  {$ENDIF}
  f1, f2 : Double ;
  value : String ;
begin
  Result := True ;

  if VarIsEmpty( _value ) or VarIsNull( _value ) then
    value := ''
  else
    {$IFDEF OXYGENE}
      value := VarToString( _value ) ;
    {$ELSE}
      value := _value ;
    {$ENDIF}

  if not ( ( _mode = FMode ) or
           ( _mode = TGIS_FieldValueCheckMode.Both ) ) then exit ;

  if FFormula = TGIS_FieldValueCheckFormula.Required then begin
    Result := value <> '' ;
  end
  else begin
    // if field is not required than can be empty regardless of the
    // further content

    Result := True ;
    if not IsStringEmpty( value ) then begin
      case FFormula of
        TGIS_FieldValueCheckFormula.Regex:
          begin
            { TODO -cReview : RegEx not implemented }
            {$IFDEF OXYGENE}
              regex := Regex.Create ( '^' + FContent + '$' ) ;
            {$ELSE}
              regex := TRegEx.Create( '^' + FContent + '$' ) ;
            {$ENDIF}
            Result := regex.IsMatch( value ) ;
          end;
        TGIS_FieldValueCheckFormula.Expression:
          if assigned( FCallback ) then
            Result := FCallback( value ) ;
        TGIS_FieldValueCheckFormula.StringEQ:
          Result := value = FContent ;
        TGIS_FieldValueCheckFormula.StringNE:
          Result := value <> FContent ;
        TGIS_FieldValueCheckFormula.StringLT:
          Result := value < FContent ;
        TGIS_FieldValueCheckFormula.StringLE:
          Result := value <= FContent ;
        TGIS_FieldValueCheckFormula.StringGT:
          Result := value > FContent ;
        TGIS_FieldValueCheckFormula.StringGE:
          Result := value >= FContent ;
        TGIS_FieldValueCheckFormula.StringIn:;
          { TODO -cReview : not supported yet }
        TGIS_FieldValueCheckFormula.TextEQ:
          Result := CompareText( value, FContent ) = 0 ;
        TGIS_FieldValueCheckFormula.TextNE:
          Result := CompareText( value, FContent ) <> 0 ;
        TGIS_FieldValueCheckFormula.TextLT:
          Result := CompareText( value, FContent ) < 0 ;
        TGIS_FieldValueCheckFormula.TextLE:
          Result := CompareText( value, FContent ) <= 0 ;
        TGIS_FieldValueCheckFormula.TextGT:
          Result := CompareText( value, FContent ) > 0 ;
        TGIS_FieldValueCheckFormula.TextGE:
          Result := CompareText( value, FContent ) >= 0 ;
        TGIS_FieldValueCheckFormula.TextIn: ;
          { TODO -cReview : not supported yet }
        TGIS_FieldValueCheckFormula.NumberRequired:
          try
            StrToFloat( value  ) ;
          except
            Result := False ;
          end;
        TGIS_FieldValueCheckFormula.NumberEQ:
          try
            f1 := StrToFloat( value    ) ;
            f2 := StrToFloat( FContent ) ;

            Result := f1 = f2 ;
          except
            Result := False ;
          end;
        TGIS_FieldValueCheckFormula.NumberNE:
          try
            f1 := StrToFloat( value    ) ;
            f2 := StrToFloat( FContent ) ;

            Result := f1 <> f2 ;
          except
            Result := False ;
          end;
        TGIS_FieldValueCheckFormula.NumberLT:
          try
            f1 := StrToFloat( value    ) ;
            f2 := StrToFloat( FContent ) ;

            Result := f1 < f2 ;
          except
            Result := False ;
          end;
        TGIS_FieldValueCheckFormula.NumberLE:
          try
            f1 := StrToFloat( value    ) ;
            f2 := StrToFloat( FContent ) ;

            Result := f1 <= f2 ;
          except
            Result := False ;
          end;
        TGIS_FieldValueCheckFormula.NumberGT:
          try
            f1 := StrToFloat( value    ) ;
            f2 := StrToFloat( FContent ) ;

            Result := f1 > f2 ;
          except
            Result := False ;
          end;
        TGIS_FieldValueCheckFormula.NumberGE:
          try
            f1 := StrToFloat( value    ) ;
            f2 := StrToFloat( FContent ) ;

            Result := f1 >= f2 ;
          except
            Result := False ;
          end;
        TGIS_FieldValueCheckFormula.DateRequired: ;
          { TODO -cReview : not supported yet }
        TGIS_FieldValueCheckFormula.DateEQ: ;
          { TODO -cReview : not supported yet }
        TGIS_FieldValueCheckFormula.DateNE: ;
          { TODO -cReview : not supported yet }
        TGIS_FieldValueCheckFormula.DateLT: ;
          { TODO -cReview : not supported yet }
        TGIS_FieldValueCheckFormula.DateLE: ;
          { TODO -cReview : not supported yet }
        TGIS_FieldValueCheckFormula.DateGT: ;
          { TODO -cReview : not supported yet }
        TGIS_FieldValueCheckFormula.DateGE: ;
          { TODO -cReview : not supported yet }
      end;
    end;
  end;

  if Result  then
    _msg := ''
  else
    _msg := FMessage ;
end;

{$ENDREGION}

{$REGION 'TGIS_FieldValueChecks'}

constructor TGIS_FieldValueChecks.Create;
begin
  inherited ;

  FChecks := TGIS_FieldValueCheckList.Create( True ) ;
end;

{$IFNDEF MANAGED}
  destructor TGIS_FieldValueChecks.Destroy;
  begin
    FreeObject( FChecks ) ;

    inherited ;
  end;
{$ENDIF}

function TGIS_FieldValueChecks.Check(
  const _value : Variant ;
  const _mode  : TGIS_FieldValueCheckMode ;
  out   _msg   : String
) : Boolean ;
{$IFNDEF OXYGENE}
  var
    o : TGIS_FieldValueCheck ;
{$ENDIF}
begin
  Result := True ;
  {$IFDEF OXYGENE}
    for each o in FChecks do begin
  {$ELSE}
    for o in FChecks do begin
  {$ENDIF}
    if not o.Check( _value, _mode, _msg ) then begin
      Result := False ;
      break ;
   end;
  end;
end;

{$ENDREGION}

{$REGION 'TGIS_FieldValues'}

constructor TGIS_FieldValues.Create(
  const _mode         : TGIS_FieldValuesMode ;
  const _defaultvalue : String
) ;
begin
  inherited Create;

  FItems := TGIS_ListOfStrings.Create ;

  FMode := _mode ;

  if not IsStringEmpty( _defaultvalue ) then
    FDefaultValue := _defaultvalue
  else
    FDefaultValue := Unassigned ;
end;

{$IFNDEF MANAGED}
  destructor  TGIS_FieldValues.Destroy ;
  begin
    FreeObject( FItems ) ;

    inherited ;
  end;
{$ENDIF}

procedure TGIS_FieldValues.fset_Mode(
  const _value : TGIS_FieldValuesMode
) ;
begin
  FMode := _value ;
end;

procedure TGIS_FieldValues.fset_DefaultValue(
  const _value : Variant
) ;
begin
  FDefaultValue := _value ;
end;

{$ENDREGION}

{$REGION 'TGIS_FieldRule'}

constructor TGIS_FieldRule.Create ;
begin
  inherited ;

  FCaption      := '' ;
  FLongName     := '' ;
  FValueChecks  := TGIS_FieldValueChecks.Create ;
  FValueAliases := TGIS_FieldValueAliases.Create ;
  FValues       := TGIS_FieldValues.Create(
                     TGIS_FieldValuesMode.Edit, ''
                   ) ;
  FValueFormat  := '' ;

  FModified     := False ;
end;

{$IFNDEF MANAGED}
  destructor  TGIS_FieldRule.Destroy ;
  begin
    FValueChecks.Free ;
    FValueAliases.Free ;
    FValues.Free ;

    inherited ;
  end;
{$ENDIF}

function TGIS_FieldRule.ResolveGet(
  const _value : Variant ;
  const _fld   : TObject
) : Variant ;
var
  stmp  : String ;
  stmpa : String ;
  dtmp  : Double ;
  fld   : TGIS_FieldInfo ;
begin
  if VarIsNull( _value ) or VarIsEmpty( _value ) then begin
    Result := _value ;
    exit ;
  end;

  {$IFDEF OXYGENE}
    stmp := VarToString( _value ) ;
  {$ELSE}
    stmp := String( _value ) ;
  {$ENDIF}

  stmpa := ValueAliases.ResolveGet( stmp ) ;
  if stmp <> stmpa then begin
    Result := stmpa ;
    exit ;
  end;

  fld := TGIS_FieldInfo( _fld ) ;
  try
    if assigned( fld ) then begin
      case fld.FieldType of
        TGIS_FieldType.String  :
          begin
            Result := stmp ;
          end;
        TGIS_FieldType.Number  :
          begin
            if IsStringEmpty( stmp ) then
              stmp := '0' ;
            dtmp := DotStrToFloat( stmp ) ;
            if fld.Decimal = 0  then
              Result := Int64( RoundS( dtmp ) )
            else
              Result := RoundTo( dtmp, -fld.Decimal ) ;
          end;
        TGIS_FieldType.Float   :
          begin
            if IsStringEmpty( stmp ) then
              stmp := '0' ;
            Result := DotStrToFloat( stmp ) ;
          end;
        TGIS_FieldType.Boolean :
          begin
            if IsStringEmpty( stmp ) then
              Result := False
            else begin
              case UpCase( stmp[StringFirst] ) of
                     'T', '1', 'Y' : Result := True  ;
                     'F', '0', 'N' : Result := False  ;
                     else            Abort ;
              end;
            end;
          end;
        TGIS_FieldType.Date    :
          begin
            if IsStringEmpty( stmp ) then
              Result := Now
            else
              Result := VarAsType( stmp, varDate ) ;
          end;
      end;
    end;
  except
    raise EGIS_Exception.Create( _rsrc(GIS_RS_ERR_WRONGVALUE), stmp, 0 )
  end;

end;

function TGIS_FieldRule.ResolveSet(
  const _setvalue : Variant ;
  var   _retvalue : Variant ;
  var   _msg    : String
) : Boolean ;
var
  val : Variant ;
begin
  _retvalue := _setvalue ;

  if VarIsEmpty( _setvalue ) or VarIsNull( _setvalue ) then
    val := ''
  else
    val := _setvalue ;

  Result := ValueChecks.Check(
              val,
              TGIS_FieldValueCheckMode.Both,
              _msg
            ) ;

  if not Result then exit ;

  _retvalue := ValueAliases.ResolveSet( _setvalue ) ;
end;

function TGIS_FieldRule.Check(
  const _setvalue : Variant ;
  const _mode     : TGIS_FieldValueCheckMode ;
  var   _msg      : String
) : Boolean ;
begin
  Result := ValueChecks.Check( _setvalue, _mode, _msg ) ;
end;

{$ENDREGION}

{$REGION 'Read and Save'}

class function TGIS_FieldRulesOperations.ParseFldx( const _path : String; const _parent : TObject ) : Boolean ;
var
  lv  : TGIS_LayerVector ;
  xml : IXMLDocument     ;
  nd  : IXMLNode         ;

  procedure parseValueCheck( _nd : IXMLNode; _obj : TGIS_FieldValueChecks ) ;
  var
    nd  : IXMLNode     ;
    lst : IXMLNodeList ;
    i   : Integer      ;

    sMode    : String ;
    tMode    : TGIS_FieldValueCheckMode ;
    sFormula : String ;
    tFormula : TGIS_FieldValueCheckFormula ;
    sContent : String ;
    sMessage : String ;
  begin
    sMode    := '' ;
    sFormula := '' ;
    sContent := '' ;
    sMessage := '' ;

    lst := _nd.ChildNodes ;

    for i:= 0 to lst.Count -1 do begin
      nd := lst[i] ;
      if      nd.NodeName = 'Mode'    then sMode    := String(nd.NodeValue)
      else if nd.NodeName = 'Formula' then sFormula := String(nd.NodeValue)
      else if nd.NodeName = 'Content' then sContent := String(nd.NodeValue)
      else if nd.NodeName = 'Message' then sMessage := String(nd.NodeValue) ;
    end ;

    if      sMode = 'AfterEdit' then
      tMode := TGIS_FieldValueCheckMode.AfterEdit
    else if sMode = 'OnEdit'    then
      tMode := TGIS_FieldValueCheckMode.OnEdit
    else if sMode = ''          then
      tMode := TGIS_FieldValueCheckMode.OnEdit
    else
      raise EGIS_Exception.Create( _rsrc(GIS_RS_ERR_UNTESTED), sMode, 0 ) ;

    if      sFormula = 'Required'       then
      tFormula := TGIS_FieldValueCheckFormula.Required
    else if sFormula = 'Regex'          then
      tFormula := TGIS_FieldValueCheckFormula.Regex
    else if sFormula = 'Expression'     then
      tFormula := TGIS_FieldValueCheckFormula.Expression
    else if sFormula = 'StringEQ'       then
      tFormula := TGIS_FieldValueCheckFormula.StringEQ
    else if sFormula = 'StringNE'       then
      tFormula := TGIS_FieldValueCheckFormula.StringNE
    else if sFormula = 'StringLT'       then
      tFormula := TGIS_FieldValueCheckFormula.StringLT
    else if sFormula = 'StringLE'       then
      tFormula := TGIS_FieldValueCheckFormula.StringLE
    else if sFormula = 'StringGT'       then
      tFormula := TGIS_FieldValueCheckFormula.StringGT
    else if sFormula = 'StringGE'       then
      tFormula := TGIS_FieldValueCheckFormula.StringGE
    else if sFormula = 'StringIn'       then
      tFormula := TGIS_FieldValueCheckFormula.StringIn
    else if sFormula = 'TextEQ'         then
      tFormula := TGIS_FieldValueCheckFormula.TextEQ
    else if sFormula = 'TextNE'         then
      tFormula := TGIS_FieldValueCheckFormula.TextNE
    else if sFormula = 'TextLT'         then
      tFormula := TGIS_FieldValueCheckFormula.TextLT
    else if sFormula = 'TextLE'         then
      tFormula := TGIS_FieldValueCheckFormula.TextLE
    else if sFormula = 'TextGT'         then
      tFormula := TGIS_FieldValueCheckFormula.TextGT
    else if sFormula = 'TextGE'         then
      tFormula := TGIS_FieldValueCheckFormula.TextGE
    else if sFormula = 'TextIn'         then
      tFormula := TGIS_FieldValueCheckFormula.TextIn
    else if sFormula = 'NumberRequired' then
      tFormula := TGIS_FieldValueCheckFormula.NumberRequired
    else if sFormula = 'NumberEQ'       then
      tFormula := TGIS_FieldValueCheckFormula.NumberEQ
    else if sFormula = 'NumberNE'       then
      tFormula := TGIS_FieldValueCheckFormula.NumberNE
    else if sFormula = 'NumberLT'       then
      tFormula := TGIS_FieldValueCheckFormula.NumberLT
    else if sFormula = 'NumberLE'       then
      tFormula := TGIS_FieldValueCheckFormula.NumberLE
    else if sFormula = 'NumberGT'       then
      tFormula := TGIS_FieldValueCheckFormula.NumberGT
    else if sFormula = 'NumberGE'       then
      tFormula := TGIS_FieldValueCheckFormula.NumberGE
    else if sFormula = 'DateRequired'   then
      tFormula := TGIS_FieldValueCheckFormula.DateRequired
    else if sFormula = 'DateEQ'         then
      tFormula := TGIS_FieldValueCheckFormula.DateEQ
    else if sFormula = 'DateNE'         then
      tFormula := TGIS_FieldValueCheckFormula.DateNE
    else if sFormula = 'DateLT'         then
      tFormula := TGIS_FieldValueCheckFormula.DateLT
    else if sFormula = 'DateLE'         then
      tFormula := TGIS_FieldValueCheckFormula.DateLE
    else if sFormula = 'DateGT'         then
      tFormula := TGIS_FieldValueCheckFormula.DateGT
    else if sFormula = 'DateGE'         then
      tFormula := TGIS_FieldValueCheckFormula.DateGE
    else
      raise EGIS_Exception.Create( _rsrc(GIS_RS_ERR_UNTESTED), sFormula, 0 ) ;

    _obj.Checks.Add(
      TGIS_FieldValueCheck.Create(
        tMode,
        tFormula,
        sContent,
        sMessage
      )
    )
  end ;

  procedure parseValueChecks( _nd : IXMLNode; _obj : TGIS_FieldValueChecks ) ;
  var
    nd  : IXMLNode     ;
    lst : IXMLNodeList ;
    i   : Integer      ;
  begin
    lst := _nd.ChildNodes ;

    for i:= 0 to lst.Count -1 do begin
      nd := lst[i] ;
      if nd.NodeName = 'ValueCheck' then parseValueCheck( nd, _obj ) ;
    end ;
  end ;

  procedure parseValueAlias( _nd : IXMLNode; _obj : TGIS_FieldValueAliases ) ;
  var
    nd     : IXMLNode     ;
    lst    : IXMLNodeList ;
    i      : Integer      ;
    salias : String       ;
    svalue : String       ;
  begin
    salias := '' ;
    svalue := '' ;

    lst := _nd.ChildNodes ;

    for i:= 0 to lst.Count -1 do begin
      nd := lst[i] ;
      if      nd.NodeName = 'Alias'  then salias := String(nd.NodeValue)
      else if nd.NodeName = 'Value'  then svalue := String(nd.NodeValue) ;
    end ;

    _obj.Aliases.Add(
      TGIS_FieldValueAlias.Create(
        salias,
        svalue
      )
    ) ;
  end ;

  procedure parseValueAliases( _nd : IXMLNode; _obj : TGIS_FieldValueAliases ) ;
  var
    nd  : IXMLNode     ;
    lst : IXMLNodeList ;
    i   : Integer      ;
  begin
    lst := _nd.ChildNodes ;

    for i:= 0 to lst.Count -1 do begin
      nd := lst[i] ;
      if nd.NodeName = 'ValueAlias' then parseValueAlias( nd, _obj ) ;
    end ;
  end ;

  procedure parseValue( _nd : IXMLNode; _obj : TGIS_FieldValues ) ;
  var
    nd  : IXMLNode     ;
    lst : IXMLNodeList ;
    i   : Integer      ;
  begin
    lst := _nd.ChildNodes ;

    for i:= 0 to lst.Count -1 do begin
      nd := lst[i] ;
      if nd.NodeName = 'Item' then _obj.Items.Add( String(nd.NodeValue) ) ;
    end ;
  end ;

  procedure parseValues( _nd : IXMLNode; _obj : TGIS_FieldValues ) ;
  var
    nd    : IXMLNode     ;
    lst   : IXMLNodeList ;
    i     : Integer      ;
    smode : String       ;
    tmode : TGIS_FieldValuesMode ;
    tmp   : String       ;

  begin
    smode := '' ;

    lst := _nd.ChildNodes ;

    _obj.Items.Clear ;

    for i:= 0 to lst.Count -1 do begin
      nd := lst[i] ;
      if      nd.NodeName = 'Mode'         then smode := String(nd.NodeValue)
      else if nd.NodeName = 'Items'        then parseValue( nd, _obj )
      else if nd.NodeName = 'DefaultValue' then begin
                                             tmp := String(nd.NodeValue) ;
                                             if not IsStringEmpty( tmp ) then
                                               _obj.FDefaultValue := tmp
                                             else
                                               _obj.FDefaultValue := Unassigned ;
                                           end;
    end ;

    if      smode = 'SelectList' then
      tmode := TGIS_FieldValuesMode.SelectList
    else if smode = 'MultiLine'  then
      tmode := TGIS_FieldValuesMode.MultiLine
    else if smode = 'Hidden'     then
      tmode := TGIS_FieldValuesMode.Hidden
    else if smode = ''           then
      tmode := TGIS_FieldValuesMode.Edit
    else
      raise EGIS_Exception.Create( _rsrc(GIS_RS_ERR_UNTESTED), smode, 0 ) ;

    _obj.Mode := tmode ;
  end ;

  procedure parseField( _nd : IXMLNode; _layer : TGIS_LayerVector  ) ;
  var
    nd     : IXMLNode     ;
    lst    : IXMLNodeList ;
    i      : Integer      ;
    obj    : TGIS_FieldRule ;
    sfield : String       ;
    k      : Integer      ;
  begin
    lst := _nd.ChildNodes ;

    obj := TGIS_FieldRule.Create ;

    for i:= 0 to lst.Count -1 do begin
      nd := lst[i] ;
      if      nd.NodeName = 'Name'         then sfield           := String(nd.NodeValue)
      else if nd.NodeName = 'Caption'      then obj.FCaption     := String(nd.NodeValue)
      else if nd.NodeName = 'LongName'     then obj.FLongName    := String(nd.NodeValue)
      else if nd.NodeName = 'ValueFormat'  then obj.FValueFormat := String(nd.NodeValue)
      else if nd.NodeName = 'ValueChecks'  then parseValueChecks ( nd, obj.ValueChecks  )
      else if nd.NodeName = 'ValueAliases' then parseValueAliases( nd, obj.ValueAliases )
      else if nd.NodeName = 'Values'       then parseValues      ( nd, obj.Values       )
    end ;
    if obj.Caption = '' then
      obj.FCaption := sfield ;

    k := _layer.FindField( sfield ) ;

    if k < 0 then begin
      FreeObject( obj ) ;
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FIELDNOEXIST ), sfield, 0 );
    end ;

    if assigned( _layer.FieldInfo( k ).Rules ) then
      FreeObjectNotNil( _layer.FieldInfo( k ).Rules ) ;

    _layer.FieldInfo( k ).Rules := obj ;
  end ;

  procedure parseFields( _nd : IXMLNode; const _layer : TGIS_LayerVector ) ;
  var
    nd  : IXMLNode     ;
    lst : IXMLNodeList ;
    i   : Integer      ;
  begin
    lst := _nd.ChildNodes ;

    for i:= 0 to lst.Count -1 do begin
      nd := lst[i] ;
      if nd.NodeName <> 'Field' then continue ;

      parseField( nd, _layer ) ;
    end ;
  end ;

  procedure parseTatukGIS( _nd : IXMLNode; const _layer : TGIS_LayerVector ) ;
  var
    nd  : IXMLNode     ;
    lst : IXMLNodeList ;
    i   : Integer      ;
  begin
  lst := _nd.ChildNodes ;

  for i:= 0 to lst.Count -1 do begin
    nd := lst[i] ;
    if nd.NodeName <> 'Fields' then continue ;

    parseFields( nd, _layer  ) ;
  end ;
end ;

begin
  Result := False ;
  lv := TGIS_LayerVector( _parent ) ;

  xml := TGIS_XMLDocument.Create ;
  try
    xml.LoadFromFile( _path );

    nd := xml.DocumentElement ;
    if nd.NodeName <> 'LicadGIS' then // ilker deðiþtirme
      exit ;

    parseTatukGIS( nd, lv ) ;
  finally
    FreeObject( xml ) ;
  end ;
  Result := True ;
end ;

class procedure TGIS_FieldRulesOperations.SaveFldx( const _path : String; const _parent : TObject ) ;
var
  lv  : TGIS_LayerVector ;
  xml : TGIS_XMLDocument ;
  nd  : IXMLNode ;

  procedure parseValueAlias( _nd : IXMLNode; const _alias : TGIS_FieldValueAlias ) ;
  var
    nd  : IXMLNode ;
    nd1 : IXMLNode ;
  begin
    nd := _nd.AddChild( 'ValueAlias' ) ;

    nd1 := nd.AddChild( 'Alias' ) ;
    nd1.NodeValue := _alias.Alias ;

    nd1 := nd.AddChild( 'Value' ) ;
    nd1.NodeValue := _alias.Value ;
  end;

  procedure parseValueAliases( _nd : IXMLNode; const _aliases : TGIS_FieldValueAliases ) ;
  var
    i    : Integer  ;
    nd  : IXMLNode ;
  begin
    nd := _nd.AddChild( 'ValueAliases' ) ;

    for i := 0 to _aliases.Aliases.Count - 1 do
      parseValueAlias( nd, _aliases.Aliases[i] ) ;
  end;

  procedure parseValueCheck( _nd : IXMLNode; const _check : TGIS_FieldValueCheck ) ;
  var
    nd  : IXMLNode ;
    nd1 : IXMLNode ;
    t   : String ;
  begin
    nd := _nd.AddChild( 'ValueCheck' ) ;

    case _check.Formula of
      TGIS_FieldValueCheckFormula.Required       :
        t := 'Required'        ;
      TGIS_FieldValueCheckFormula.Regex          :
        t := 'Regex'           ;
      TGIS_FieldValueCheckFormula.Expression     :
        t := 'Expression'      ;
      TGIS_FieldValueCheckFormula.StringEQ       :
        t := 'StringEQ'        ;
      TGIS_FieldValueCheckFormula.StringNE       :
        t := 'StringNE'        ;
      TGIS_FieldValueCheckFormula.StringLT       :
        t := 'StringLT'        ;
      TGIS_FieldValueCheckFormula.StringLE       :
        t := 'StringLE'        ;
      TGIS_FieldValueCheckFormula.StringGT       :
        t := 'StringGT'        ;
      TGIS_FieldValueCheckFormula.StringGE       :
        t := 'StringGE'        ;
      TGIS_FieldValueCheckFormula.StringIn       :
        t := 'StringIn'        ;
      TGIS_FieldValueCheckFormula.TextEQ         :
        t := 'TextEQ'          ;
      TGIS_FieldValueCheckFormula.TextNE         :
        t := 'TextNE'          ;
      TGIS_FieldValueCheckFormula.TextLT         :
        t := 'TextLT'          ;
      TGIS_FieldValueCheckFormula.TextLE         :
        t := 'TextLE'          ;
      TGIS_FieldValueCheckFormula.TextGT         :
        t := 'TextGT'          ;
      TGIS_FieldValueCheckFormula.TextGE         :
        t := 'TextGE'          ;
      TGIS_FieldValueCheckFormula.TextIn         :
        t := 'TextIn'          ;
      TGIS_FieldValueCheckFormula.NumberRequired :
        t := 'NumberRequired'  ;
      TGIS_FieldValueCheckFormula.NumberEQ       :
        t := 'NumberEQ'        ;
      TGIS_FieldValueCheckFormula.NumberNE       :
        t := 'NumberNE'        ;
      TGIS_FieldValueCheckFormula.NumberLT       :
        t := 'NumberLT'        ;
      TGIS_FieldValueCheckFormula.NumberLE       :
        t := 'NumberLE'        ;
      TGIS_FieldValueCheckFormula.NumberGT       :
        t := 'NumberGT'        ;
      TGIS_FieldValueCheckFormula.NumberGE       :
        t := 'NumberGE'        ;
      TGIS_FieldValueCheckFormula.DateRequired   :
        t := 'DateRequired'    ;
      TGIS_FieldValueCheckFormula.DateEQ         :
        t := 'DateEQ'          ;
      TGIS_FieldValueCheckFormula.DateNE         :
        t := 'DateNE'          ;
      TGIS_FieldValueCheckFormula.DateLT         :
        t := 'DateLT'          ;
      TGIS_FieldValueCheckFormula.DateLE         :
        t := 'DateLE'          ;
      TGIS_FieldValueCheckFormula.DateGT         :
        t := 'DateGT'          ;
      TGIS_FieldValueCheckFormula.DateGE         :
        t := 'DateGE'         ;
      else
        raise EGIS_Exception.Create( _rsrc(GIS_RS_ERR_UNTESTED), 'ValueCheck', 0 ) ;
    end;

    if not IsStringEmpty( t ) then begin
      nd1 := nd.AddChild( 'Formula' ) ;
      nd1.NodeValue := t ;
    end;

    if not IsStringEmpty( _check.Content ) then begin
      nd1 := nd.AddChild( 'Content' ) ;
      nd1.NodeValue := _check.Content ;
    end;

    if not IsStringEmpty( _check.Message ) then begin
      nd1 := nd.AddChild( 'Message' ) ;
      nd1.NodeValue := _check.Message ;
    end;
  end;

  procedure parseValueChecks( _nd : IXMLNode; const _checks : TGIS_FieldValueChecks ) ;
  var
    i    : Integer  ;
    nd   : IXMLNode ;
  begin
    nd := _nd.AddChild( 'ValueChecks' ) ;

    for i := 0 to _checks.Checks.Count - 1 do
      parseValueCheck( nd, _checks.Checks[i] ) ;
  end;

  procedure parseValues( _nd : IXMLNode; const _values : TGIS_FieldValues ) ;
  var
    i   : Integer  ;
    nd  : IXMLNode ;
    nd1 : IXMLNode ;
    nd2 : IXMLNode ;
    t   : String   ;
  begin
    nd := _nd.AddChild( 'Values' ) ;

    case _values.Mode of
      TGIS_FieldValuesMode.SelectList : t := 'SelectList' ;
      TGIS_FieldValuesMode.MultiLine  : t := 'MultiLine'  ;
      TGIS_FieldValuesMode.Hidden     : t := 'Hidden'     ;
      TGIS_FieldValuesMode.Edit       : t := ''           ;
      else
        raise EGIS_Exception.Create( _rsrc(GIS_RS_ERR_UNTESTED), 'Values', 0 ) ;
    end;

    if not IsStringEmpty( t ) then begin
      nd1 := nd.AddChild( 'Mode' ) ;
      nd1.NodeValue := t ;
    end;

    if _values.Items.Count > 0 then begin
      nd1 := nd.AddChild( 'Items' ) ;
      for i := 0 to _values.Items.Count - 1 do begin
        nd2 := nd1.AddChild( 'Item' ) ;
        nd2.NodeValue := _values.Items[i] ;
      end;
    end;

    if not IsStringEmpty( String( _values.DefaultValue ) ) then begin
      nd1 := nd.AddChild( 'DefaultValue' ) ;
      nd1.NodeValue := _values.DefaultValue ;
    end ;
  end;

  procedure parseField( _nd : IXMLNode; const _layer : TGIS_LayerVector; const _id : Integer ) ;
  var
    nd   : IXMLNode ;
    nd1  : IXMLNode ;
    fld  : TGIS_FieldInfo ;
    rule : TGIS_FieldRule ;
  begin
    fld  := _layer.FieldInfo( _id ) ;
    rule := TGIS_FieldRule( fld.Rules ) ;

    nd := _nd.AddChild( 'Field' ) ;

    if not IsStringEmpty( fld.NewName ) then begin
      nd1 := nd.AddChild( 'Name' ) ;
      nd1.NodeValue := fld.NewName ;
    end ;

    if not IsStringEmpty( rule.LongName ) then begin
      nd1 := nd.AddChild( 'LongName' ) ;
      nd1.NodeValue := rule.LongName ;
    end ;

    if not IsStringEmpty( rule.Caption ) then begin
      nd1 := nd.AddChild( 'Caption' ) ;
      nd1.NodeValue := rule.Caption ;
    end;

    if not IsStringEmpty( rule.ValueFormat ) then begin
      nd1 := nd.AddChild( 'ValueFormat' ) ;
      nd1.NodeValue := rule.ValueFormat ;
    end;

    if assigned( rule.ValueAliases ) then
      parseValueAliases( nd, rule.ValueAliases ) ;
    if assigned( rule.ValueChecks  ) then
      parseValueChecks( nd, rule.ValueChecks ) ;
    if assigned( rule.Values       ) then
      parseValues( nd, rule.Values ) ;
  end;

  function parseFields( _nd : IXMLNode; const _layer : TGIS_LayerVector ) : Boolean ;
  var
    i    : Integer  ;
    nd   : IXMLNode ;
    fld  : TGIS_FieldInfo ;
  begin
    Result := False ;

    nd := _nd.AddChild( 'Fields' ) ;

    for i := 0 to _layer.Fields.Count - 1 do begin
      fld := _layer.FieldInfo( i ) ;
      if not fld.Deleted then begin
        if assigned( fld.Rules ) then begin
          Result := True ;
          parseField( nd, _layer, i ) ;
        end;
      end;
    end;
  end;
begin
  lv := TGIS_LayerVector( _parent ) ;

  xml := TGIS_XMLDocument.Create ;
  try
    xml.Active := True;
    nd := xml.AddChild( 'LicadGIS' ) ; // ilker deðiþtirme
    if parseFields( nd, lv ) then begin
      if not IsEmbeddedSQLPath( _path ) then
        xml.SaveToFile( _path )
    end
    else begin
      if not IsEmbeddedSQLPath( _path ) and FileExists( _path ) then
        DeleteFile( _path ) ;
    end ;
  finally
    FreeObject( xml ) ;
  end;
end;

{$ENDREGION}

//==================================== END =====================================
end.


