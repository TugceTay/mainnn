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
  All resources.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoResourceEx ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoResourceEx"'}
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

{$IFDEF DCC}
uses
  System.SysUtils,
  System.IniFiles,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoTypes ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

  /// <summary>
  ///   Updates resource string variables with the localized content loaded
  ///   from string
  /// </summary>
  /// <param name="_localization">
  ///   string with localization KEY=TRANSLATION pairs
  /// </param>
  /// <param name="_errpath">
  ///   if not empty then missed symbols will be saved to the file named _errpath
  /// </param>
  procedure GisLocalizedStringsFromString( const _localization : String ;
                                           const _errpath      : String
                                         ) ;

  /// <summary>
  ///   Updates resource string variables with the localized content loaded
  ///   from file
  /// </summary>
  /// <param name="_path">
  ///   name of the file with localization strings
  /// </param>
  /// <param name="_language">
  ///   name of the language
  /// </param>
  /// <param name="_errpath">
  ///   if not empty then missed symbols will be saved to the file named _errpath
  /// </param>
  procedure GisLocalizedStringsFromFile  ( const _path         : String ;
                                           const _language     : String ;
                                           const _errpath      : String
                                         ) ;

implementation

type
  { Localization class.
  }
  T_LoadFromStringList = class( TGIS_ObjectDisposable )
    private
      lstStrings : TGIS_StringList ;
      errStrings : TGIS_StringList ;
      errPath    : String ;
    protected
      procedure   doDestroy         ; override;
    public

      // <summary>
      //   Create localization class based on string list with _key=_translations
      //   pairs.
      // </summary>
      // <param name="_lst">
      //   list with KEY=TRANSLATION pairs
      // </param>
      // <param name="_errpath">
      //   if not empty then missed symbols will be saved to the file
      //   named _errpath
      // </param>
      constructor Create            ( const _lst      : TGIS_StringList ;
                                      const _errpath  : String
                                    ) ;

      // <summary>
      //   Replace _orginal with value keyed by _key. If new value not exists en error
      //   will be recorded (and saved upon Destroy)
      // </summary>
      // <param name="_original">
      //   text to be replaced
      // </param>
      // <param name="_key">
      //   key name of the entry (like 'GIS_RS_BTN_OK')
      // </param>
      procedure   GetLocalizedString( var   _original : String ;
                                      const _key      : String
                                    ) ;
  end ;

//==============================================================================
// T_LoadFromStringList
//==============================================================================

  constructor T_LoadFromStringList.Create( const _lst     : TGIS_StringList;
                                           const _errpath : String
                                         ) ;
  begin
    inherited Create;

    lstStrings := _lst ;
    errStrings := nil  ;
    if not IsStringEmpty( _errpath ) then begin
      errPath := _errpath ;
      errStrings := TGIS_StringList.Create ;
    end ;
  end ;

  { Destroy localization. Save errors if _errpath was not empty (in Create)
  }
  procedure T_LoadFromStringList.doDestroy ;
  begin
    if assigned( errStrings ) then begin
      DeleteFile( errPath ) ;
      if errStrings.Count > 0 then
        errStrings.SaveToFile( errPath ) ;
      FreeObject( errStrings ) ;
    end ;

    inherited ;
  end ;

  procedure T_LoadFromStringList.GetLocalizedString( var   _original : String ;
                                                     const _key     : String
                                                   ) ;
  var
    res : String ;

    function slashn2cr( const _str : String ) : String ;
    var
      i     : Integer ;
      state : Integer ;
      c     : Char    ;
    begin
      Result := '' ;

      state := 0 ;
      for i := StringFirst to StringLast( _str ) do begin
        c := _str[i] ;
        case state of
          0 : begin
                if c = '\' then state  := 1
                           else Result := Result + c ;
              end ;
          1 : begin
                if c = 'n' then begin
                                  Result := Result + #13 ;
                                  state := 0 ;
                                end
                           else begin
                                  Result := Result + '\' + c ;
                                  state := 0 ;
                                end ;
              end ;
        end ;
      end ;

      if state = 1 then Result := Result + '\' ;
    end ;

    function cr2slashn( const _str : String ) : String ;
    var
      i : Integer ;
    begin
      Result := '' ;
      for i := StringFirst to StringLast( _str ) do begin
        case _str[i] of
          #13 : Result := Result + '\n' ;
          #10 : ;
          else  Result := Result + _str[i] ;
        end ;
      end ;
    end ;
  begin
    res := lstStrings.Values[ _key ] ;
    if not IsStringEmpty( res ) then
      _original := slashn2cr( res )
    else begin
      if assigned( errStrings ) then
        errStrings.Add( _key + '=' + cr2slashn( _original ) ) ;
    end ;
  end ;

  procedure GisLocalizedStringsFromString( const _localization : String ;
                                           const _errpath      : String
                                         ) ;
  var
    obj : T_LoadFromStringList ;
    lst : TGIS_StringList     ;
  begin
    if not assigned( LocalizedNotification ) then begin
      LocalizedNotification := TGIS_LocalizedNotification.Create ;
      GisLocalizedStrings( nil ) ;
    end ;

    lst := TGIS_StringList.Create ;
    try
      lst.Text := _localization ;
      obj := T_LoadFromStringList.Create( lst, _errpath ) ;
      try
        {$IFDEF OXYGENE}
          GisLocalizedStrings( @obj.GetLocalizedString ) ;
        {$ELSE}
          GisLocalizedStrings( obj.GetLocalizedString ) ;
        {$ENDIF}
      finally
        FreeObject( obj ) ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;

  end ;

  procedure GisLocalizedStringsFromFile( const _path     : String ;
                                         const _language : String ;
                                         const _errpath  : String
                                        ) ;
  var
    obj : T_LoadFromStringList ;
    ini : TMemIniFile          ;
    lst : TGIS_StringList      ;
    //i   : Integer              ;
    scp : String               ;
    icp : Integer              ;
  begin
    if not assigned( LocalizedNotification ) then begin
      LocalizedNotification := TGIS_LocalizedNotification.Create ;
      GisLocalizedStrings( nil ) ;
    end ;

    ini := TMemIniFile.Create( _path ) ;
    lst := TGIS_StringList.Create ;
    try
      ini.ReadSectionValues( _language, lst );

      try
        scp := lst.Values[ GIS_INI_CODEPAGE ] ;
        if not IsStringEmpty( scp ) then begin
          icp := StrToInt( scp ) ;

          { TODO : Verify if it's still needed }
          //for i:=0 to lst.Count -1 do begin
            {$IFDEF OXYGENE}
              //lst[i] := ConvertStr2WStrCP( lst[i], icp ) ;
            {$ELSE}
              {$IFDEF NEXTGEN}
                //??
              {$ELSE}
               // lst[i] := ConvertStr2WStrCP( AnsiString( lst[i] ), icp ) ;
              {$ENDIF}
            {$ENDIF}
          //end ;
        end;
      except
        // do nothing
      end;

      obj := T_LoadFromStringList.Create( lst, _errpath ) ;
      try
        {$IFDEF OXYGENE}
          GisLocalizedStrings( @obj.GetLocalizedString ) ;
        {$ELSE}
          GisLocalizedStrings(  obj.GetLocalizedString ) ;
        {$ENDIF}
      finally
        FreeObject( obj ) ;
      end ;
    finally
      FreeObject( lst ) ;
      FreeObject( ini ) ;
    end ;
  end ;

end.

