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
  Encapsulation of configuration file reader.
}

{$IFDEF DCC}
   unit GisIniFiles ;
   {$HPPEMIT '#pragma link "GisIniFiles"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.IniFiles,
    System.Classes,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisTypes ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF COCOA}
  uses
    TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Encapsulation of configuration file reader.
  /// </summary>
  TGIS_IniFile = {$IFDEF OXYGENE} public {$ENDIF} class( TCustomIniFile )

    private // other private variable
    {$IFNDEF GENDOC}
      lstSections      : TDictionary< String, TDictionary< String, String > > ;
    {$ENDIF}
      fileName         : String        ;
      lastSectionName  : String        ;
      lastSection      : TDictionary< String, String > ;
      lstSectionNames  : TStrings      ;

    private // other private functions

      /// <summary>
      ///   Add section to list.
      /// </summary>
      /// <param name="_section">
      ///   section name
      /// </param>
      /// <returns>
      ///   section list
      /// </returns>
      function  addSection        ( const _section  : String
                                  ) : TDictionary< String, String > ;

      /// <summary>
      ///   Load file values.
      /// </summary>
      procedure loadValues        ;

    {$IFDEF OXYGENE}
      protected
        procedure doDestroy       ; override;
    {$ELSE}
      public

        /// <inheritdoc/>
        destructor Destroy        ; override;
    {$ENDIF}

    public

      /// <summary>
      ///   Create an instance on provided file.
      /// </summary>
      /// <param name="_fileName">
      ///   file name
      /// </param>
      constructor Create          ( const _fileName : String
                                  ) ;

      /// <summary>
      ///   Clear list.
      /// </summary>
      procedure Clear             ;

      /// <inheritdoc/>
      procedure UpdateFile        ; override;

      /// <inheritdoc/>
      procedure DeleteKey         ( const _section : String ;
                                    const _ident   : String
                                  ) ; override;

      /// <inheritdoc/>
      procedure EraseSection      ( const _section : String
                                  ) ; override;

      /// <inheritdoc/>
      procedure ReadSection       ( const _section : String;
                                          _strings : TGIS_Strings
                                  ) ; override;

      /// <inheritdoc/>
      function  TestSection       ( const _section : String
                                  ) : Boolean ;

      /// <inheritdoc/>
      procedure ReadSections      ( _strings       : TGIS_Strings
                                  ) ; override;

      /// <inheritdoc/>
      procedure ReadSectionValues
                                  ( const _section : String;
                                          _strings : TGIS_Strings
                                  ) ; override;

      /// <inheritdoc/>
      function  ReadString        ( const _section : String ;
                                    const _ident   : String ;
                                    const _default : String
                                  ) : String; override;

      /// <inheritdoc/>
      procedure WriteString       ( const _section : String ;
                                    const _ident   : String ;
                                    const _value   : String
                                  ) ; override;

      /// <inheritdoc/>
      procedure SetStrings        ( _list          : TGIS_Strings
                                  ) ;

      /// <inheritdoc/>
      procedure GetStrings        ( _list          : TGIS_Strings
                                  ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Last section handle.
      /// </summary>
      property LastSec : TDictionary< String, String > read lastSection ;
    end;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Variants,
    System.SysUtils,

    GisRtl,
    GisInternals ;
{$ENDIF}

//==============================================================================
// TGIS_IniFile
//==============================================================================

  constructor TGIS_IniFile.Create(
    const _fileName : String
  );
  begin
    inherited Create( _fileName);

    fileName        := _fileName ;
    lstSections     := TDictionary< String, TDictionary< String, String > >.Create(
                       {$IFDEF OXYGENE}
                         {$IFDEF JAVA}
                           java.lang.String.CASE_INSENSITIVE_ORDER
                         {$ENDIF}
                         {$IFDEF CLR}
                           StringComparer.OrdinalIgnoreCase
                         {$ENDIF}
                       {$ELSE}
                         TIStringComparer.Ordinal
                       {$ENDIF}
                       );
    lstSectionNames := TStringList.Create ;

    loadValues ;
  end;

  {$IFDEF OXYGENE}
    procedure TGIS_IniFile.doDestroy;
  {$ELSE}

    destructor TGIS_IniFile.Destroy;
  {$ENDIF}
  begin
    if lstSections <> nil then
      Clear;

    FreeObject( lstSections     );
    FreeObject( lstSectionNames );

    inherited ;
  end ;

  function TGIS_IniFile.addSection(
    const _section : String
  ) : TDictionary< String, String >;
  var
    section : String ;
  begin
    section := _section ;
    Result  := TDictionary< String, String >.Create(
                   {$IFDEF OXYGENE}
                     {$IFDEF JAVA}
                       java.lang.String.CASE_INSENSITIVE_ORDER
                     {$ENDIF}
                     {$IFDEF CLR}
                       StringComparer.OrdinalIgnoreCase
                     {$ENDIF}
                   {$ELSE}
                     TIStringComparer.Ordinal
                   {$ENDIF}
               );
    try
      lstSections.Add( section, Result );
      lstSectionNames.Add( section );
    except
      FreeObject( Result ) ;
      raise ;
    end ;
  end ;

  procedure TGIS_IniFile.loadValues;
  var
    lst    : TGIS_StringList ;
  begin
    if ( not IsStringEmpty( fileName ) ) and SafeFileExists( fileName ) then
    begin
      lst := TStringList.Create;
      try
        lst.LoadFromFile( fileName );

        SetStrings( lst );
      finally
        FreeObject( lst );
      end;
    end
    else
      Clear;
  end;

  procedure TGIS_IniFile.Clear;
  {$IFNDEF OXYGENE}
    var
      itm : TPair< String, TDictionary< String, String > > ;
  {$ENDIF}
  begin
    {$IFNDEF NEXTGEN}
    for itm in lstSections do begin
      FreeObjectNotNil( itm.Value ) ;
    end;
    {$ENDIF}
    lstSections.Clear;
    lstSectionNames.Clear;
  end;

  procedure TGIS_IniFile.DeleteKey(
    const _section  : String ;
    const _ident    : String
   );
  var
    strings : TDictionary< String, String > ;
  begin
    if lstSections.TryGetValue( _section, strings ) then
      strings.Remove( _ident ) ;
  end;

  procedure TGIS_IniFile.EraseSection(
    const _section : String
  );
  var
    strings : TDictionary< String, String > ;
    idx     : Integer;
  begin
    if lstSections.TryGetValue( _section, strings ) then begin
      FreeObject( strings ) ;
      lstSections.Remove( _section );
      idx := lstSectionNames.IndexOf( _section ) ;
      if idx > 0 then
        lstSectionNames.Delete( idx );
    end;
  end;

  procedure TGIS_IniFile.GetStrings(
    _list : TGIS_Strings
  );
  var
    i     : Integer ;
    data  : TDictionary< String, String > ;
    {$IFNDEF OXYGENE}
      itm : TPair< String, String > ;
    {$ENDIF}
    items : TStringList ;
  begin
    _list.BeginUpdate;
    items := TGIS_StringList.Create;
    try
      for i := 0 to lstSectionNames.Count - 1 do begin
        data := lstSections[ lstSectionNames[ i ] ] ;

        _list.Add( '[' + lstSectionNames[ i ] + ']' ) ;

        if assigned( data ) then begin
          for itm in data do
            items.Add( itm.Key + '=' + itm.Value ) ;
        end;

        items.Sort;
        _list.AddStrings( items );
        _list.Add( '' ) ;
        items.Clear;
      end;
    finally
      FreeObject( items );
      _list.EndUpdate;
    end;
  end;

  procedure TGIS_IniFile.ReadSection(
    const _section : String ;
          _strings : TGIS_Strings
  ) ;
  var
    data  : TDictionary< String, String > ;
    {$IFNDEF OXYGENE}
      itm : TPair< String, String > ;
    {$ENDIF}
  begin
    _strings.BeginUpdate;
    try
      _strings.Clear;
      if lstSections.TryGetValue( _section, data ) then begin
        for itm in data do
          _strings.Add( itm.Key + '=' + itm.Value ) ;
      end;
    finally
      _strings.EndUpdate;
    end;
  end;

  function TGIS_IniFile.TestSection(
    const _section  : String
  ) : Boolean ;
  begin
    Result := lstSections.ContainsKey( _section ) ;
  end;

  procedure TGIS_IniFile.ReadSections(
    _strings : TGIS_Strings
  );
  begin
    _strings.AddStrings( lstSectionNames );
  end;

  procedure TGIS_IniFile.ReadSectionValues(
    const _section : String ;
          _strings : TGIS_Strings
  ) ;
  var
    data  : TDictionary< String, String > ;
    {$IFNDEF OXYGENE}
      itm : TPair< String, String > ;
    {$ENDIF}
  begin
    _strings.BeginUpdate;
    try
      _strings.Clear;
      if lstSections.TryGetValue( _section, data ) then begin
        for itm in data do begin
         _strings.Add( itm.Key + '=' + itm.Value ) ;
        end;
      end;
    finally
      _strings.EndUpdate;
    end;
  end;

  function TGIS_IniFile.ReadString(
    const _section  : String ;
    const _ident    : String ;
    const _default  : String
   ) : String;
  var
    strings : TDictionary< String, String > ;
    data    : String;
  begin
    if ( lastSectionName <> _section ) or ( lastSection = nil ) then begin
      if lstSections.TryGetValue( _section, strings ) then begin
        lastSection      := strings ;
        lastSectionName  := _section;
        if strings.TryGetValue( _ident, data ) then begin
          Result := data ;
          Exit;
        end;
      end;
    end
    else begin
      if lastSection.TryGetValue( _ident, data ) then begin
        Result := data ;
        Exit;
      end;
    end;

    Result := _default;
  end;

  procedure TGIS_IniFile.SetStrings(
    _list : TGIS_Strings
  );
  var
    i, j    : Integer ;
    s       : String  ;
    strings : TDictionary< String, String > ;
  begin
    lastSectionName  := '' ;
    lastSection      := nil ;
    strings          := nil;
    Clear;

    for i := 0 to _list.Count - 1 do begin
      s := Trim( _list[ i ] );
      if ( not IsStringEmpty( s ) ) and ( s[ StringFirst ] <> ';' ) then begin
        if ( s[ StringFirst ] = '[' ) and ( s[ StringLast( s ) ] = ']' ) then begin
          Delete( s, StringFirst, 1 );
          SetLengthStr( s, length( s ) - 1 );
          strings := addSection( Trim( s ) );
        end
        else if strings <> nil then begin
          j := Pos( String( '=' ), s );
          if j >= StringFirst then begin
            strings.Add( TrimRight( Copy( s, StringFirst, j-StringFirst ) ),
                         TrimLeft ( Copy( s, j+1        , MaxInt        ) )
                       ) ;
          end;
        end;
      end;
    end;
  end;

  procedure TGIS_IniFile.UpdateFile;
  var
    lst   : TGIS_StringList ;
  begin
    lst := TGIS_StringList.Create;
    try
      GetStrings( lst );

      if lst.Count > 0 then begin
        // empty line to make no UTF version workable
        if not IsStringEmpty( lst[0] ) then
          lst.Insert( 0, '' ) ;
      end ;
      lst.SaveToFile( fileName, TEncoding.UTF8 );
    finally
      FreeObject( lst );
    end;
  end;

  procedure TGIS_IniFile.WriteString(
    const _section  : String ;
    const _ident    : String ;
    const _value    : String
   ) ;
  var
    strings : TDictionary< String, String > ;
  begin
    if not lstSections.TryGetValue( _section, strings ) then
      strings := addSection( _section );

    if not strings.ContainsKey( _ident ) then
      strings.Add( _ident, _value )
    else
      strings[ _ident ] := _value ;
  end;

//==================================== END =====================================
end.
