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
  Address matching support.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoAddressMatching ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoAddressMatching"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Text.RegularExpressions,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Generics.Collections,
    System.IniFiles,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL,
  RemObjects.Elements.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Class supports address matching according to defined formulas.
  /// </summary>
  TGIS_AddressMatching = class( TGIS_BaseObjectDisposable )
    protected  // property access routines
      procedure fset_Formulas ( const _value : String
                              ) ;
      function  fget_Formulas : String ;

    private   // private variables
      iniFile       : TMemIniFile ;
      formulas      : TList<TObject> ;
      formats       : TList<TObject> ;
      {$IFNDEF  OXYGENE}
        wchars      : String ;
      {$ENDIF}

    private   // private routines

      /// <summary>
      ///   Deletes all address formulas, frees INI file object.
      /// </summary>
      procedure clearFormulas ;

      /// <summary>
      ///   Reads address formulas from INI file. Adds them to formula list.
      /// </summary>
      procedure loadFormulas ;

      /// <summary>
      ///   Clears all previously found matches.
      /// </summary>
      procedure clearValues ;

      /// <summary>
      ///   Creates internal representation of an address formula and adds the
      ///   formula to list.
      /// </summary>
      /// <param name="_formula">
      ///   address formula read from INI file
      /// </param>
      procedure setFormula             ( const _formula : String
                                       ) ;

      /// <summary>
      ///   Standardizes the processed address string. Removes leading and
      ///   trailing spaces. Replaces single white characters by space
      ///   characters. Replaces a group of white characters by one space
      ///   character.
      /// </summary>
      /// <param name="_str">
      ///   address string
      /// </param>
      function  standardizeInputString ( const _str     : String
                                       ) : String;

      /// <summary>
      ///   Matches address string and an address formula.
      /// </summary>
      /// <param name="_str">
      ///   address string
      /// </param>
      /// <param name="_index">
      ///   index in formula list
      /// </param>
      function  matchFormula           ( const _str     : String ;
                                         const _index   : Integer
                                       ) : Boolean ;
    protected
      procedure doDestroy ; override;

    public    // public methods

      /// <summary>
      ///   Constructs instance, initializes internal data.
      /// </summary>T_
      constructor Create ;

      /// <summary>
      ///  Matches given address string and address formulas.
      /// </summary>
      /// <param name="_str">
      ///   address string
      /// </param>
      /// <param name="_results">
      ///   list containing all found matches; every string is a single
      ///   match in form of 'fieldld1=value1#13#10field2=value2#13#10...';
      ///   if the reference equals nil, the object will be created ;
      /// </param>
      /// <returns>
      ///   Number of found matches (or 0 if nothing).
      /// </returns>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD
      /// </exception>
      function  Match                  ( const _str     : String ;
                                         var   _results : TGIS_ObjectList
                                       ) : Integer ;
    public // public properties

      /// <summary>
      ///   Field for formulas string.
      /// </summary>
      property FormulasString : String
                                read  fget_Formulas
                                write fset_Formulas ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.RegularExpressions,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

type
  // Class handles field formats.
  T_AddrFormat = class( TGIS_ObjectDisposable )
    public // public variables
      name       : String ;
      expression : String ;
      values     : TStringList ;
      synonyms   : TStringList ;
    protected

      // Destroys instance, frees synonym list.
      procedure doDestroy ; override;

    public // public routines

      // Constructs instance.
      constructor Create  ;

      // Initializes synonym lists.
      procedure   InitSynonyms ;

      // Frees synonym lists.
      procedure   FreeSynonyms ;
  end ;

  // Class handles formula fields.
  T_AddrField = class( TGIS_Object )
    public // public variables
      format : T_AddrFormat ;
      value  : String       ;
    protected

      // Constructs instance.
      procedure doDestroy ; override;
    public // public routines

      // Destroys instance, frees synonym list.
      constructor Create  ;
  end ;

  // Class handles address formulas.
  T_AddrFormula = class( TGIS_ObjectDisposable )
    public // public variables
      formula      : String ;
      fields       : TList<T_AddrField>  ;
      finalPattern : String ;
    protected

      // Destroys instance, frees field list .
      procedure doDestroy ; override;
    public // public routines

      // Constructs instance, initializes field list.
      constructor Create;
  end ;

//==============================================================================
// T_AddrFormat
//==============================================================================

  constructor T_AddrFormat.Create ;
  begin
    inherited ;
  end ;

  procedure T_AddrFormat.doDestroy ;
  begin
    FreeSynonyms ;
    inherited ;
  end ;
  procedure T_AddrFormat.InitSynonyms ;
  begin
    values := TStringList.Create ;
    values.Sorted := True ;
    values.Duplicates := TDuplicates.dupIgnore ;
    synonyms := TStringList.Create ;
    synonyms.Sorted := True ;
    synonyms.Duplicates := TDuplicates.dupIgnore ;
  end ;

  procedure T_AddrFormat.FreeSynonyms ;
  var
    i : Integer ;
  begin
    if assigned( values ) then
      FreeObject( values ) ;
    if assigned( synonyms ) then begin
      for i := 0 to synonyms.Count-1 do
        FreeObjectNotNil( TStringList( synonyms.Objects[i] ) ) ;
      FreeObject( synonyms ) ;
    end ;
  end ;

//==============================================================================
// T_AddrField
//==============================================================================

  constructor T_AddrField.Create ;
  begin
    inherited ;
  end ;

  procedure T_AddrField.doDestroy ;
  begin
    // does not free format description
    inherited ;
  end ;

//==============================================================================
// T_AddrFormula
//==============================================================================

  constructor T_AddrFormula.Create ;
  begin
    inherited ;
    fields := TList<T_AddrField>.Create ;
  end ;

  procedure T_AddrFormula.doDestroy ;
  var
    i : Integer ;
  begin
    if assigned ( fields ) then begin
      for i := 0 to fields.Count-1 do
        FreeObjectNotNil( T_AddrField(fields[i]) ) ;
      FreeObject( fields ) ;
    end ;
    inherited ;
  end ;

//==============================================================================
// TGIS_AddressMatching
//==============================================================================

  constructor TGIS_AddressMatching.Create ;
  begin
    inherited ;
    formulas := TList<TObject>.Create ;
    formats  := TList<TObject>.Create ;
    iniFile  := TMemIniFile.Create( '' ) ;
  end ;

  procedure TGIS_AddressMatching.doDestroy ;
  var
    i : Integer ;
  begin
    if assigned ( formulas ) then begin
      for i := 0 to formulas.Count-1 do
        FreeObjectNotNil( T_AddrFormula(formulas[i]) ) ;
      FreeObject( formulas ) ;
    end ;
    if assigned ( formats ) then begin
      for i := 0 to formats.Count-1 do
        FreeObjectNotNil( T_AddrFormat(formats[i]) ) ;
      FreeObject( formats ) ;
    end ;
    FreeObject ( iniFile ) ;
    inherited ;
  end ;

  procedure TGIS_AddressMatching.fset_Formulas ( const _value : String ) ;
  var
    iniContent : TStrings ;
  begin
    if IsStringEmpty( _value ) then exit ;

    try
      clearFormulas ;
      iniContent := TStringList.Create ;
      try
        iniContent.Text := _value ;
        iniFile.SetStrings ( iniContent ) ;
        loadFormulas ;
      finally
        FreeObject( iniContent ) ;
      end ;
    except
      on e : EGIS_Exception do
      begin
        clearFormulas ;
        raise ;
      end ;
    {$IFNDEF OXYGENE}
      else
        clearFormulas ;
        raise EGIS_Exception.Create ( GIS_RS_ERR_FILEREAD, 'address formulas', 0 ) ;
    {$ELSE}
      on e : Exception do begin
        clearFormulas ;
        raise EGIS_Exception.Create ( _rsrc( GIS_RS_ERR_FILEREAD ), 'address formulas', 0 ) ;
      end ;
    {$ENDIF}
    end ;
  end ;

  function TGIS_AddressMatching.fget_Formulas : String ;
  var
    iniContent : TStrings ;
  begin
    iniContent := TStringList.Create ;
    iniFile.GetStrings ( iniContent ) ;
    Result := iniContent.Text ;
  end ;

  procedure TGIS_AddressMatching.clearFormulas ;
  var
    i : Integer ;
  begin
    if assigned ( formulas ) then begin
      for i := 0 to formulas.Count-1 do
        FreeObjectNotNil( T_AddrFormula(formulas[i]) ) ;
      formulas.Clear ;
    end ;
    if assigned ( formats ) then begin
      for i := 0 to formats.Count-1 do
        FreeObjectNotNil( T_AddrFormat(formats[i]) ) ;
      formats.Clear ;
    end ;
    {$IFNDEF OXYGENE}
      wchars := '' ;
    {$ENDIF}
  end ;

  procedure TGIS_AddressMatching.clearValues ;
  var
    i, j    : Integer ;
    formula : T_AddrFormula ;
  begin
    if assigned ( formulas ) then
      for i := 0 to formulas.Count-1 do begin
        formula := T_AddrFormula( formulas[i] ) ;
        if assigned ( formula.fields ) then
          for j := 0 to formula.fields.Count-1 do
            T_AddrField(formula.fields[j]).value := '' ;
      end ;
  end ;

  procedure TGIS_AddressMatching.setFormula ( const _formula : String ) ;
  var
    name      : Integer ;
    i, j      : Integer ;
    tmpStr,
    tmpTmpStr : String  ;
    fmtName   : String  ;
    sectionStrings : TStrings  ;
    desc      : T_AddrFormula  ;
    field     : T_AddrField    ;
    format    : T_AddrFormat   ;

    procedure findSynonyms ;
    var
      found   : Boolean  ;
      {$IFDEF OXYGENE}
        r     : Regex    ;
      {$ENDIF}
      i1, j1    : Integer  ;
      {$IFDEF CLR}
        m     : System.Text.RegularExpressions.Match ;
      {$ENDIF}
      {$IFNDEF OXYGENE}
        num   : Integer  ;
      {$ENDIF}
        k     : Integer  ;
      {$IFDEF OXYGENE}
        astr  : array of String ;
      {$ELSE}
        astr  : TArray<String> ;
      {$ENDIF}
      strings : TStrings ;
      name1,
      value   : String   ;
    begin

      found := False ;
      try
        {$IFDEF OXYGENE}
          r := Regex.Create( '^(\w+)(\|\w+)*$', RegexOptions.IgnoreCase ) ;
          try
            {$IFDEF CLR}
            m := r.Match( field.format.expression ) ;
            if m.Success then
              found := True ;
            {$ENDIF}
            {$IFDEF JAVA}
            if r.IsMatch(field.format.expression) then
              found := True ;
            {$ENDIF}
          finally
            FreeObject( r ) ;
          end ;
        {$ELSE}
          with TRegEx.Create( '^(\w+)(\|\w+)*$', [TRegExOption.roIgnoreCase] ) do try
            { TODO : Verify if it's still needed }
            //case sensitive
            //? WordChars := wchars ;
            //? ModifierI := false ;
            //? Expression := '^(\w+)(\|\w+)*$' ;
            if IsMatch(field.format.expression) then
              found := True ;
          finally
            //? Free ;
          end ;
        {$ENDIF}
      except
        raise ;
      end ;

      if found = True then begin
        //split the format into a list of values
        {$IFDEF OXYGENE}
        {$ELSE}
          num := 0;
        {$ENDIF}
        try
          {$IFDEF OXYGENE}
            r := Regex.Create( '\|' ) ;
            try
          {$ELSE}
            with TRegEx.Create( '\|' ) do try
          {$ENDIF}
            if not assigned ( field.format.values ) then begin
              field.format.InitSynonyms ;
              {$IFDEF OXYGENE}
                {$IFDEF JAVA}
                  astr := r.split(field.format.expression) ;
                {$ELSE}
                  astr := r.Split( field.format.expression ) ;
                {$ENDIF}
              {$ELSE}
                astr := Split ( field.format.expression ) ;
              {$ENDIF}
                for k := 0 to length(astr)-1 do
                  field.format.values.Add( astr[k] ) ;
              {$IFDEF OXYGENE}
                astr := nil ;
              {$ENDIF}
            end ;
            for i1 := 0 to sectionStrings.Count-1 do begin
              {$IFDEF OXYGENE}
              {$ELSE}
                num := i1 ;
              {$ENDIF}
              name1 := sectionStrings.Names[i1] ;
              value := Copy ( sectionStrings.Strings[i1], StringLast(name1)+2,
                              length(sectionStrings.Strings[i1])-length(name1)-1 ) ;
              if field.format.values.Find ( value, j1 ) = True then begin
                //add a synonym
                field.format.expression := field.format.expression + '|' + name1 ;
                if field.format.synonyms.Find ( name1, j1 ) = True then
                  TStringList ( field.format.synonyms.Objects[j1] ).Add ( value )
                else begin
                  // a new synonym
                  strings := TStringList.Create ;
                  strings.Add ( value ) ;
                  field.format.synonyms.AddObject ( name1, strings )
                end ;
              end
            end
          finally
            {$IFDEF OXYGENE}
              FreeObject( r ) ;
            {$ELSE}
              { TODO : Verify if it's still needed }
              //? Free ;
            {$ENDIF}
          end ;
        except
          raise ;
        end ;
      end ;
    end ;

  begin

    if IsStringEmpty( _formula ) then exit ;

    desc := T_AddrFormula.Create ;
    desc.formula := _formula ;

    name := 0 ;
    i := StringFirst ;
    desc.finalPattern := '^' ;
    tmpStr := '';
    while i <= StringLast(desc.formula) do
    begin
      case desc.formula[i] of
        '<' : begin
                // characters between fields
                if desc.finalPattern = '^' then begin
                  if not IsStringEmpty( tmpStr ) then begin
                    tmpStr := TrimLeft(tmpStr) ;
                    tmpTmpStr := TrimRight(tmpStr) ;
                    desc.finalPattern := desc.finalPattern + tmpTmpStr;
                    if tmpTmpStr <> tmpStr then
                      desc.finalPattern := desc.finalPattern + '\s+' ;
                  end ;
                end else begin
                  if not IsStringEmpty( tmpStr ) then begin
                    tmpTmpStr := TrimLeft(tmpStr) ;
                    if tmpTmpStr <> tmpStr then
                      desc.finalPattern := desc.finalPattern + '\s+' ;
                    tmpStr := tmpTmpStr ;
                    tmpTmpStr := TrimRight(tmpStr) ;
                    desc.finalPattern := desc.finalPattern + tmpTmpStr ;
                    if tmpTmpStr <> tmpStr then
                      desc.finalPattern := desc.finalPattern + '\s+' ;
                  end else
                    desc.finalPattern := desc.finalPattern + '\s+' ;
                end ;
                // field begins
                field := T_AddrField.Create ;
                desc.fields.Add ( field ) ;
                fmtName := '' ;
                name := 2;
                inc(i) ;
              end;
          '>' : begin
                // check if the field already exists
                j := 0 ;
                while j < formats.Count do begin
                  if T_AddrFormat(formats[j]).name = fmtName then begin
                    // found
                    field.format := T_AddrFormat( formats[j] ) ;
                    break ;
                  end ;
                  inc(j) ;
                end ;
                if j = formats.Count then begin
                  // new format
                  format := T_AddrFormat.Create ;
                  format.name := fmtName ;
                  formats.Add ( format );
                  field.format := format ;
                  //read field format
                  //check synonyms if the format is an OR-expression
                  sectionStrings := TStringList.Create ;
                  iniFile.ReadSectionValues ( format.name, sectionStrings ) ;
                  if sectionStrings.Count > 0 then
                    try
                      format.expression := sectionStrings.Values['Format'] ;
                      sectionStrings.Delete ( sectionStrings.IndexOfName('Format') ) ;
                    except
                      format.expression := '\w+';
                    end;
                  if sectionStrings.Count > 0 then
                    findSynonyms ;
                  FreeObject( sectionStrings ) ;
                end ;
                desc.finalPattern := desc.finalPattern +
                                     '(' + field.format.expression + ')' ;
                tmpStr := '' ;
                name := 0 ;
                inc(i) ;
              end ;
        ':' : begin
              name := 2;
              inc(i) ;
              end ;
      else begin
        if name = 0 then
          tmpStr := tmpStr + desc.formula[i]
        else if name = 2 then
          fmtName := fmtName + desc.formula[i]
        end ;
        inc(i) ;
      end ;
    end ;
    if not IsStringEmpty( tmpStr ) then begin
      // characters on the end
      tmpTmpStr := TrimLeft(tmpStr) ;
      if tmpTmpStr <> tmpStr then
        desc.finalPattern := desc.finalPattern + '\s+' ;
      tmpStr := tmpTmpStr ;
      tmpTmpStr := TrimRight(tmpStr) ;
      desc.finalPattern := desc.finalPattern + tmpTmpStr ;
      if tmpTmpStr <> tmpStr then
        desc.finalPattern := desc.finalPattern + '\s+' ;
    end ;
    desc.finalPattern := desc.finalPattern + '$' ;
    formulas.Add ( desc ) ;
  end ;

  procedure TGIS_AddressMatching.loadFormulas ;
  var
    strings : TStrings ;
    i       : Integer  ;
  begin

    {$IFNDEF OXYGENE}
      wchars := iniFile.ReadString( 'WORDCHARS', 'Format',
        '1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-''' ) ;
    {$ENDIF}

    strings := TStringList.Create ;
    try
      //reads address formulas from given INI file
      iniFile.ReadSection ( 'FORMULA', strings ) ;

      for i := 0 to strings.Count-1 do
        setFormula ( iniFile.ReadString('FORMULA', strings.Strings[i], 'unknown') ) ;
    finally
      FreeObject( strings ) ;
    end ;

  end ;

  function TGIS_AddressMatching.standardizeInputString( const _str : String
                                                      ) : String ;
  var
    i, j      : Integer ;
    whiteChar : Integer ;
    c         : Char    ;
    str       : String  ;
  begin
    str := Trim( _str ) ;
    i := StringFirst ;
    j := StringFirst ;
    whiteChar := 0;
    while i <= StringLast( str ) do begin
      c := str[i] ;
      if (c = ' ') or (c = '\t') or (c = '\r') or (c = '\f') then
        inc(whiteChar)
      else begin
        if whiteChar > 0 then begin
          Delete ( str, j, whiteChar ) ;
          Insert ( ' ', str, j ) ;
          i := i - whiteChar + 1 ;
          inc(j);
          whiteChar := 0;
        end ;
        inc(j) ;
      end ;
      inc(i) ;
    end ;
    Result := str ;
  end ;

  function  TGIS_AddressMatching.matchFormula ( const _str    : String;
                                                const _index  : Integer
                                              ) : Boolean ;
  var
    i       : Integer ;
    j, k    : Integer ;
    {$IFDEF CLR}
      r     : Regex   ;
      m     : System.Text.RegularExpressions.Match ;
    {$ENDIF}
    {$IFDEF DCC}
      m     : TMatch ;
    {$ENDIF}
    {$IFDEF JAVA}
      r     : Regex   ;
      m     : java.util.regex.Matcher ;
    {$ENDIF}
    value   : String  ;
    formula : T_AddrFormula ;
    field   : T_AddrField   ;
  begin
    Result := False ;
    formula := T_AddrFormula( formulas[_index] ) ;
    {$IFNDEF OXYGENE}
      i := 0 ;
    {$ENDIF}
    try
      {$IFDEF OXYGENE}
        r := Regex.Create( formula.finalPattern,
                           RegexOptions.IgnoreCase ) ;
        try
          {$IFDEF CLR}
          m := r.Match( _str ) ;
          if m.Success then begin
              i := 0;
              while not IsStringEmpty( m.Groups[i+1].Value) do begin
                value := m.Groups[i+1].Value ;
                field := T_AddrField( formula.fields[i] ) ;
         {$ENDIF}
          {$IFDEF JAVA}
          m := r.FindFirstMatch( _str ) ;
          if assigned(m) then begin
              i := 0;
            if m.groupCount > 0 then
              while i <= m.groupCount-1 do begin
                value := m.group(i) ;
                field := formula.fields[i] ;
         {$ENDIF}
      {$ELSE}
        with TRegEx.Create( formula.finalPattern, [TRegexOption.roIgnoreCase] ) do try
          //case insensitive
          { TODO : Verify if it's still needed }
          //? WordChars := wchars ;
          //? ModifierI := true ;
          //? Expression := formula.finalPattern ;
          m := Match( _str ) ;
          if m.Success then begin
            i := 0;
            if m.Groups.Count > 0 then
              while i < m.Groups.Count-1 do begin
                value := m.Groups[i+1].Value ;
                field := formula.fields[i] ;
      {$ENDIF}
                if field.format.synonyms <> nil then begin
                  if field.format.values.Find ( value, j ) = True then begin
                    field.value := value ;
                    Result := True ;
                  end;
                  if field.format.synonyms.Find ( value, j ) = True then begin
                    for k := 0 to TStringList(field.format.synonyms.Objects[j]).Count-1 do begin
                      if not IsStringEmpty( field.value ) then
                        field.value := field.value + '|' ;
                      field.value := field.value +
                                   TStringList(field.format.synonyms.Objects[j]).Strings[k] ;
                      Result := True ;
                    end ;
                  end ;
                end else begin
                  field.value := value ;
                  Result := True ;
                end ;
                inc(i) ;
              end ;
          end
      finally
        {$IFDEF OXYGENE}
          FreeObject( r ) ;
        {$ELSE}
          //? Free ;
        {$ENDIF}
      end ;
    except
      raise ;
    end ;
  end ;

  function TGIS_AddressMatching.Match ( const _str     : String ;
                                        var   _results : TGIS_ObjectList
                                      ) : Integer ;
  var
    i, j  : Integer  ;
    text  : String   ;
    list  : TStrings ;
    field : T_AddrField ;
  begin

    Result := 0 ;
    if formulas.Count = 0 then exit ;

    clearValues ;

    if not assigned( _results ) then begin
      _results  := TGIS_ObjectList.Create ;
    end else begin
      _results.Clear ;
    end ;

    text := standardizeInputString ( _str ) ;

    //match
    for i := formulas.Count-1 downto 0 do begin
      if matchFormula ( text, i ) then begin
        //prepare output data
        with T_AddrFormula(formulas[i]) do begin
          list := TStringList.Create ;
          for j := 0 to fields.Count-1 do begin
            field := T_AddrField( fields[j] ) ;
            list.Add ( field.format.name + '=' + field.value ) ;
          end ;
          _results.Add ( list ) ;
          inc(Result) ;
        end ;
      end ;
    end ;

  end ;

//==================================== END =====================================
end.
