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
  TTKGP / INI configuration files.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoConfigIni ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoConfigIni"'}
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

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    {$IFDEF LEVEL_XE2_RTL}
      System.IOUtils,
    {$ENDIF}

    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoConfig,
    Lider.CG.GIS.GeoIniFiles;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Encapsulation of layer configuration file.
  /// </summary>
  TGIS_ConfigIni = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Config )

    // property internal values
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FIniObj          : TGIS_IniFile ;

    private // other private values
      lockSentinel    : String      ; // verification of a real ini changes
      sectionCount    : Integer     ; // list for cached ini values
      subSectionCount : Integer     ; // list for cached ini values

      sectionSelected : Integer     ; // selected section
      layerId         : Integer     ; // layer number
      subLayerId      : Integer     ; // layer number

    // other access functions
    protected

      /// <summary>
      ///   Read from project configuration file a stored number (#) of the
      ///   layer corresponding to a given name. This must be done because the
      ///   layer order can be rearranged without saving to ini, so the only
      ///   valid description of a layer is the name.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      /// <returns>
      ///   layer index
      /// </returns>
      function  getLayerNo       ( const _layer    : TObject
                                 ) : Integer ;

      /// <summary>
      ///   Read from project configuration file a stored number (#) of the
      ///   layer corresponding to a given name. This must be done because the
      ///   layer order can be rearranged without saving to ini, so the only
      ///   valid description of a layer is the name.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      /// <returns>
      ///   group index
      /// </returns>
      function  getGroupNo       ( const _layer    : TObject
                                 ) : Integer ;

      /// <summary>
      ///   Read from project configuration file a stored number (#) of the
      ///   layer corresponding to a given name. This must be done because the
      ///   layer order can be rearranged without saving to ini, so the only
      ///   valid description of a layer is the name.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      /// <returns>
      ///   sublayer index
      /// </returns>
      function  getSubLayerNo    ( const _layer    : TObject
                                 ) : Integer ;


      /// <summary>
      ///   Build parameters list. List will be built based on layer and
      ///   project configuration files.
      /// </summary>
      /// <param name="_uselayer">
      ///   if True, current layer section will be used
      /// </param>
      procedure buildParamList   ( const _uselayer : Boolean
                                 ) ;

      /// <summary>
      ///   Build parameters list. List will be built based on layer and
      ///   project configuration files.
      /// </summary>
      /// <param name="_uselayer">
      ///   if True, current group layer section will be used
      /// </param>
      procedure buildGroupParamList( const _uselayer : Boolean
                                 ) ;

      /// <summary>
      ///   Build parameters list. List will be built based on layer and
      ///   project configuration files.
      /// </summary>
      /// <param name="_uselayer">
      ///   if True, current sublayer section will be used
      /// </param>
      procedure buildSubParamList( const _uselayer : Boolean
                                 ) ;

      /// <summary>
      ///   Computes a section name based on the layer number and the number of
      ///   the zoom section. In the configuration file layers and sections are
      ///   numbered in the manner of 1..n (not 0..n).
      /// </summary>
      /// <param name="_layer_no">
      ///   id of layer; unused;
      /// </param>
      /// <param name="_section">
      ///   id of section
      /// </param>
      /// <returns>
      ///   section name
      /// </returns>
      function  getSectionName   ( const _layer_no : Integer ;
                                   const _section  : Integer
                                 ) : String ; virtual;

      /// <summary>
      ///   Computes a section name based on the layer number and the number of
      ///   the zoom section. In the configuration file layers and sections are
      ///   numbered in the manner of 1..n (not 0..n).
      /// </summary>
      /// <param name="_group_no">
      ///   id of layer; unused;
      /// </param>
      /// <param name="_section">
      ///   id of section
      /// </param>
      /// <returns>
      ///   group section name
      /// </returns>
      function  getGroupSectionName( const _group_no : Integer ;
                                     const _section  : Integer
                                   ) : String ; virtual;

      /// <summary>
      ///   Computes a section name based on the layer number and the number of
      ///   the zoom section. In the configuration file layers and sections are
      ///   numbered in the manner of 1..n (not 0..n).
      /// </summary>
      /// <param name="_layer_no">
      ///   id of layer; unused;
      /// </param>
      /// <param name="_sLayer_no">
      ///   id of sublayer
      /// </param>
      /// <param name="_section">
      ///   id of section
      /// </param>
      /// <returns>
      ///   subsection name
      /// </returns>
      function  getSubSectionName( const _layer_no : Integer ;
                                   const _sLayer_no: Integer ;
                                   const _section  : Integer
                                 ) : String ; virtual;

      /// <inheritdoc/>
      function  readParam        ( const _name    : String
                                 ) : String ; override;

      /// <inheritdoc/>
      procedure readCustomParam  ( const _name    : String ;
                                   const _list    : TGIS_Strings
                                 ) ; override;

      /// <inheritdoc/>
      procedure writeParam       ( const _name    : String ;
                                   const _value   : String ;
                                   const _default : String
                                 ) ; override;

      /// <inheritdoc/>
      procedure writeCustomParam ( const _name    : String ;
                                   const _list    : TGIS_Strings
                                 ) ; override;

      /// <inheritdoc/>
      procedure clearZones       ( const _name    : String
                                 ) ; override;

      /// <inheritdoc/>
      procedure readZones        ( const _name    : String ;
                                   const _list    : TGIS_StringList
                                 ) ; override;

      /// <inheritdoc/>
      procedure writeZones       ( const _name    : String ;
                                   const _list    : TGIS_StringList ;
                                   const _default : TGIS_StringList
                                 ) ; override;

    // other access functions
    protected

      function fget_FileName : String ; override;

    protected
      procedure doDestroy        ; override;

    public

      /// <inheritdoc/>
      constructor Create         ( const _layer : TObject ;
                                   const _path  : String
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Reread         ;  override;

      /// <inheritdoc/>
      procedure   Save           ; override;

      /// <inheritdoc/>
      procedure   Lock           ; override;

      /// <inheritdoc/>
      procedure   Unlock         ; override;

      /// <inheritdoc/>
      procedure SetLayer         ( const _layer   : TObject
                                 ) ; override;

      /// <inheritdoc/>
      procedure SetSubLayer      ( const _layer   : TObject
                                 ) ; override;

      /// <inheritdoc/>
      procedure AddSubLayer      ( const _layer   : TObject ;
                                   const _layerno : Integer ;
                                   const _sublayer: Integer
                                 ) ; override;

      /// <inheritdoc/>
      procedure SetGroup         ( const _layer   : TObject
                                 ) ; override;

      /// <inheritdoc/>
      function  SetSection       ( const _index   : Integer ;
                                   const _force   : Boolean
                                 ) : Boolean ; override;

      /// <inheritdoc/>
      function  SetSubSection    ( const _index   : Integer ;
                                   const _force   : Boolean
                                 ) : Boolean ; override;

      /// <inheritdoc/>
      function  SetGroupSection  ( const _index   : Integer
                                 ) : Boolean ; override;

      /// <inheritdoc/>
      procedure ClearSections    ; override;

      /// <inheritdoc/>
      procedure ClearSubSections ; override;

      /// <inheritdoc/>
      procedure ClearGroups      ; override;

      /// <inheritdoc/>
      procedure ClearActiveSection  ; override;

      /// <inheritdoc/>
      procedure ReadSectionValues( const _name    : String ;
                                   const _list    : TGIS_Strings
                                 ) ; override;

      /// <inheritdoc/>
      procedure SetStrings       ( _list          : TGIS_Strings
                                 ) ; override;

      /// <inheritdoc/>
      procedure GetStrings       ( _list          : TGIS_Strings
                                 ) ; override;
    public
      /// <summary>
      ///   Underlying ini file.
      /// </summary>
      property IniObj : TGIS_IniFile read FIniObj ;
  end ;

  /// <summary>
  ///   Encapsulation of project configuration file.
  /// </summary>
  TGIS_ConfigProjectIni = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ConfigIni )

    // property access functions
    protected

      function  fget_PrjLayersCount : Integer ; override;
      function  fget_PrjLayerName   ( const _index : Integer  ) : String ;
                                    override;
      function  fget_PrjLayerPath   ( const _index : Integer  ) : String ;
                                    override;
      function  fget_PrjLayerConfig ( const _index : Integer  ) : String ;
                                    override;
      function fget_IsProject       : Boolean ; override;

    // other access functions
    protected

      /// <inheritdoc/>
      function  getSectionName   ( const _layer_no : Integer ;
                                   const _section  : Integer
                                 ) : String ; override;

      /// <inheritdoc/>
      function  getSubSectionName( const _layer_no : Integer ;
                                   const _sLayer_no: Integer ;
                                   const _section  : Integer
                                 ) : String ; override;

    public

      /// <inheritdoc/>
      procedure BuildProject     ( const _viewer : IGIS_Viewer
                                 ) ; override;

      /// <inheritdoc/>
      procedure WriteHierarchyGroups( const _viewer : IGIS_Viewer
                                    ) ; override;

      /// <inheritdoc/>
      function ReadHierarchyGroups : TObject ; override;

  end ;

  /// <summary>
  ///   Encapsulation of shape level style.
  /// </summary>
  TGIS_ConfigShapeStyle = class ( TGIS_ConfigIni )
    // property access functions
    protected
      function  fget_IsShapeStyle    : Boolean ; override;
      function  fget_Text            : String ;
      procedure fset_Text            ( const _value : String
                                     ) ;

    public
      /// <inheritdoc/>
      constructor Create             ;
    public
      /// <summary>
      ///  Style content.
      /// </summary>
      property Text : String   read  fget_Text
                               write fset_Text ;
  end;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoResource;
{$ENDIF}


//==============================================================================
// TGIS_ConfigIni
//==============================================================================

  constructor TGIS_ConfigIni.Create(
    const _layer  : TObject  ;
    const _path   : String
  ) ;
  begin
    inherited Create( _layer, _path ) ;

    if FVersion = 0  then
      FVersion := GIS_CONFIG_VER_DK10_MAJOR * 1000 + GIS_CONFIG_VER_DK10_MINOR ;

    UseRelativePath := True ;
    WriteFull       := False ;
    FConfigFormat   := TGIS_ConfigFormat.Ini ;

    FIniObj := TGIS_IniFile.Create( GetPathAbsolute( '', _path ) ) ;

    FConfigPath := GetPathAbsolute( '', _path ) ;

    sectionCount := 0 ;

    Section := '' ;

    SetLayer( _layer ) ;
  end ;

  procedure TGIS_ConfigIni.doDestroy ;
  begin
    FreeObject( FIniObj ) ;
    inherited ;
  end ;

  function TGIS_ConfigIni.getLayerNo(
    const _layer : TObject
  ) : Integer ;
  var
    i     : Integer ;
    stmp  : String  ;
    sname : String  ;
  begin
    Result := 0 ;

    if not assigned( _layer ) then exit ;

    Result := GIS_MAX_INTEGER ;

    if assigned( _layer ) then sname := TGIS_Layer( _layer ).Name ;

    with FIniObj do begin
      for i:=1 to GIS_MAX_LAYERS_CNT do begin
        stmp :=  ParamString(
                   ReadString(
                     GIS_INI_LAYER_HEADER + IntToStr(i),
                     GIS_INI_NAME,
                     ''
                   ),
                   ''
                 ) ;;
        if IsStringEmpty( stmp ) then // no name so path must be used
          stmp := GetFileNameNoExt(
                    ParamString(
                      ReadString(
                        GIS_INI_LAYER_HEADER + IntToStr(i),
                        GIS_INI_PATH,
                        ''
                      ),
                      ''
                    )
                  ) ;

        if IsStringEmpty( stmp ) then
          break ;
        if stmp = sname then begin
          Result := i ;
          exit ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_ConfigIni.getGroupNo(
    const _layer : TObject
  ) : Integer ;
  var
    i     : Integer ;
    stmp  : String  ;
    sname : String  ;
  begin
    Result := 0 ;

    if not assigned( _layer ) then exit ;

    Result := GIS_MAX_INTEGER ;

    if assigned( _layer ) then sname := TGIS_Layer( _layer ).Name ;

    with FIniObj do begin
      for i:=1 to GIS_MAX_LAYERS_CNT do begin
        stmp := ParamString(
                  ReadString(
                    GIS_INI_GROUP_HEADER + IntToStr(i),
                    GIS_INI_NAME,
                    ''
                  ),
                  ''
                ) ;
        if IsStringEmpty( stmp ) then // no name so path must be used
          stmp := GetFileNameNoExt(
                    ParamString(
                      ReadString(
                        GIS_INI_GROUP_HEADER + IntToStr(i),
                        GIS_INI_PATH,
                        ''
                      ),
                      ''
                    )
                  ) ;

        if IsStringEmpty( stmp ) then
          break ;
        if stmp = sname then begin
          Result := i ;
          exit ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_ConfigIni.getSubLayerNo(
    const _layer : TObject
  ) : Integer ;
  var
    i     : Integer ;
    stmp  : String  ;
    sname : String  ;
  begin
    Result := 0 ;

    if not assigned( _layer ) then exit ;

    if assigned( _layer ) then sname := TGIS_Layer( _layer ).Name ;

    with FIniObj do begin
      for i:= 0 to GIS_MAX_LAYERS_CNT do begin
        stmp := ParamString(
                  ReadString(
                    getSubSectionName( layerId, i, 0 ),
                    GIS_INI_NAME,
                    ''
                  ),
                  ''
                ) ;
        if IsStringEmpty( stmp ) then // no name so path must be used
          stmp := GetFileNameNoExt(
                    ParamString(
                      ReadString(
                        GIS_INI_LAYER_HEADER +
                        IntToStr( layerId ) + '.' + IntToStr(i),
                        GIS_INI_PATH,
                        ''
                      ),
                      ''
                    )
                  ) ;

        if IsStringEmpty( stmp ) then
          break ;
        if stmp = sname then begin
          Result := i ;
          exit ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_ConfigIni.getSectionName(
    const _layer_no : Integer ;
    const _section  : Integer
  ) : String ;
  begin
    assert( _section  >= 0 ) ;
    assert( _section  <  GIS_MAX_SECTION_CNT  ) ;
    assert( _layer_no >= 0 ) ;

    if   _layer_no > GIS_MAX_LAYERS_CNT then
         Result := ''
    else begin
         Result := GIS_INI_LAYER_HEADER ;

         if _layer_no > 0 then
           Result := Result + IntToStr( _layer_no ) ;
         if _section > 0 then
           Result := Result + ' ' + IntToStr( _section ) ;
    end ;
  end ;


  function TGIS_ConfigIni.getGroupSectionName(
    const _group_no : Integer ;
     const _section  : Integer
 ) : String ;
  begin
    assert( _section  >= 0 ) ;
    assert( _section  <  GIS_MAX_SECTION_CNT  ) ;
    assert( _group_no >= 0 ) ;

    if   _group_no > GIS_MAX_LAYERS_CNT then
         Result := ''
    else begin
         Result := GIS_INI_GROUP_HEADER ;

         if _group_no > 0 then
           Result := Result + IntToStr( _group_no ) ;
         if _section > 0 then
           Result := Result + ' ' + IntToStr( _section ) ;
    end ;
  end ;

  function TGIS_ConfigIni.getSubSectionName(
    const _layer_no : Integer ;
    const _sLayer_no: Integer ;
    const _section  : Integer
  ) : String ;
  begin
    assert( _section  >= 0 ) ;
    assert( _section  <  GIS_MAX_SECTION_CNT  ) ;
    assert( _layer_no >= 0 ) ;

    if   _layer_no > GIS_MAX_LAYERS_CNT then
         Result := ''
    else begin
         Result := GIS_INI_LAYER_HEADER ;

         if _layer_no > 0 then
           Result := Result +
                     IntToStr( _layer_no ) + '.' + IntToStr( _sLayer_no )
         else
           Result := Result + '.' + IntToStr( _sLayer_no ) ;

         if _section > 0 then
           Result := Result + ' ' + IntToStr( _section ) ;
    end ;
  end ;


  procedure TGIS_ConfigIni.buildParamList(
    const _uselayer   : Boolean
  ) ;
  var
    sect_no   : Integer  ;
    sect_name : String   ;
    lst       : TGIS_StringList ;

  begin
    sectionCount := 0 ;

    if not assigned( FIniObj ) then exit ;

    if not _uselayer then
      inc( sectionCount )
    else begin
      lst := TGIS_StringList.Create ;
      try
         // read sections
         for sect_no := 0 to GIS_MAX_SECTION_CNT do begin
           sect_name := getSectionName( layerId, sect_no ) ;
           // exist - add it.
           if FIniObj.TestSection( sect_name ) then inc( sectionCount )
                                               else break ;

           if ( self is TGIS_ConfigProjectIni ) and
              ( layerId = 0 )
           then
              break ;
         end ;
      finally
        FreeObject( lst ) ;
      end ;
    end ;
  end;

  procedure TGIS_ConfigIni.buildGroupParamList(
    const _uselayer   : Boolean
  ) ;
  var
    sect_no   : Integer  ;
    sect_name : String   ;
    lst       : TGIS_StringList ;

  begin
    sectionCount := 0 ;

    if not assigned( FIniObj ) then exit ;

    if not _uselayer then
      inc( sectionCount )
    else begin
      lst := TGIS_StringList.Create ;
      try
         // read sections
         for sect_no := 0 to GIS_MAX_SECTION_CNT do begin
           sect_name := getGroupSectionName( layerId, sect_no ) ;
           // exist - add it.
           if FIniObj.TestSection( sect_name ) then inc( sectionCount )
                                               else break ;

           if ( self is TGIS_ConfigProjectIni ) and
              ( layerId = 0 )
           then
              break ;
         end ;
      finally
        FreeObject( lst ) ;
      end ;
    end ;
  end;

  procedure TGIS_ConfigIni.buildSubParamList(
    const _uselayer   : Boolean
  ) ;
  var
    sect_no   : Integer  ;
    sect_name : String   ;
    lst       : TGIS_StringList ;
  begin
    subSectionCount := 0 ;

    if not assigned( FIniObj ) then exit ;

    if not _uselayer then
      inc( subSectionCount )
    else begin
      lst := TGIS_StringList.Create ;
      try
         // read sections
         for sect_no := 0 to GIS_MAX_SECTION_CNT do begin
           sect_name := getSubSectionName( layerId, subLayerId, sect_no ) ;
           // exist - add it.
              FIniObj.ReadSection( sect_name, lst );
              if lst.Count > 0 then inc( subSectionCount )
                               else Break ;
           if ( self is TGIS_ConfigProjectIni ) and
              ( layerId = 0 )
           then
              break ;
         end ;
      finally
        FreeObject( lst ) ;
      end ;
    end ;

  end ;

  function TGIS_ConfigIni.readParam(
    const _name : String
  ) : String ;
  begin
    if not IsStringEmpty( Section ) then
      Result := ParamString(
                  FIniObj.ReadString( Section, _name, GIS_PARAM_NIL ),
                  GIS_PARAM_NIL,
                  False
                )
    else
      Result := GIS_PARAM_NIL ;
  end ;

  procedure TGIS_ConfigIni.readCustomParam(
    const _name    : String ;
    const _list    : TGIS_Strings
  ) ;
  var
    i : Integer;
    lst : TStringList ;
    nam : String ;
    val : String ;
  begin
    lst := TStringList.Create ;
    try
      FIniObj.ReadSectionValues( Section, lst );

      for i := 0 to lst.Count - 1 do begin
        if Pos( _name + '.', lst.Names[i] )  < StringFirst then
          continue ;

        nam := Copy( lst.Names[i], length( _name ) + 2, 1024 ) ;
        val := readParam( lst.Names[i] ) ;

        _list.Values[ nam ] := val ;
      end ;
    finally
      FreeObject( lst ) ;
    end;
  end;


  procedure TGIS_ConfigIni.writeParam(
    const _name    : String ;
    const _value   : String ;
    const _default : String
  ) ;
  var
    value   : String ;
    defval  : String ;
  begin
    assert( assigned( FIniObj ) ) ;

    value  := ConstructParamString( _value   ) ;
    defval := ConstructParamString( _default ) ;

    if not IsStringEmpty( Section ) then begin
      if WriteFull then
        FIniObj.WriteString( Section, _name, value )
      else begin
        if CompareText( _value, _default ) <> 0 then
          FIniObj.WriteString( Section, _name, value )
        else
          FIniObj.DeleteKey( Section, _name );
      end;

      FMustSave := True ;
    end ;
  end ;

  procedure TGIS_ConfigIni.writeCustomParam(
    const _name    : String ;
    const _list    : TGIS_Strings
  ) ;
  var
    i : Integer;
    nam : String ;
    val : String ;
  begin
    for i := 0 to _list.Count - 1 do begin
      nam := _list.Names[ i ] ;
      val := _list.ValueFromIndex[ i ] ;
      writeParam( _name + '.' + nam, val, '' ) ;
    end;
  end;

  procedure TGIS_ConfigIni.clearZones(
    const _name : String
  ) ;
  var
    i,j : Integer ;
    key : String  ;
    lst : TGIS_StringList ;
  begin
    // delete all zones
    lst := TGIS_StringList.Create ;
    try
      FIniObj.ReadSection( Section, lst ) ;

      for i := 0 to lst.Count - 1 do begin
        key := UpperCase( lst.Names[ i ] ) ;

        for j := 1 to 9 do begin
          if Pos( UpperCase( Format( '%s%d', [_name, j ] ) ), key ) = StringFirst then
            FIniObj.DeleteKey( Section, key ) ;
        end ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_ConfigIni.readZones(
    const _name : String ;
    const _list : TGIS_StringList
  ) ;
  var
    i    : Integer ;
    stmp : String ;
    tkn  : TGIS_Tokenizer ;
  begin
    stmp := readParam( Format( '%s%d', [_name, 1 ] ) ) ;
    if stmp = GIS_PARAM_NIL then exit ;

    _list.Clear ;
    for i:= 1 to GIS_MAX_ZONE_CNT do begin
      stmp := readParam( Format( '%s%d', [_name, i ] ) ) ;
      if stmp = GIS_PARAM_NIL then break ;

      if _name = GIS_INI_PIXEL_TRANSPARENTZONE then begin
        // fix improper BGR vs RGB in old projects

        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.ExecuteEx( stmp, ',' ) ;

          if ( tkn.Result.Count > 1 ) and
             ( CompareText( tkn.Result[tkn.Result.Count-1], 'BGR' ) <> 0 )
             and
             ( CompareText( tkn.Result[tkn.Result.Count-1], 'RGB' ) <> 0 )
          then begin
            stmp := stmp + ',BGR' ;
          end ;
        finally
          FreeObject( tkn ) ;
        end ;
      end ;

      _list.Add( stmp ) ;
    end ;
  end;

  procedure TGIS_ConfigIni.writeZones(
    const _name     : String ;
    const _list     : TGIS_StringList ;
    const _default  : TGIS_StringList
  ) ;
  var
    i    : Integer ;
    stmp : String ;
    tkn  : TGIS_Tokenizer ;
    j    : Integer        ;
    cl   : TGIS_Color     ;
  begin
    // and enter existing zones only
    for i:= 1 to _list.Count do begin
      if IsStringEmpty( _list[i-1] ) then break ;

      stmp := _list[i-1] ;

      if _name = GIS_INI_PIXEL_TRANSPARENTZONE then begin
        // fix improper BGR vs RGB in old projects

        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.ExecuteEx( stmp, ',' ) ;

          if ( tkn.Result.Count > 1 )
             and
             ( CompareText( tkn.Result[tkn.Result.Count-1], 'BGR' ) <> 0 )
          then begin
            stmp := '' ;

            for j := 0 to tkn.Result.Count-1 do begin

              cl := ParamColor( tkn.Result[j], TGIS_Color.Black ) ;
              stmp := stmp +
                      ConstructParamColor( cl )
                      + ',' ;

            end ;
            stmp := stmp + 'BGR' ;
          end ;
        finally
          FreeObject( tkn ) ;
        end ;
      end ;

      writeParam( Format( '%s%d', [_name, i ] ),
                  ConstructParamString( stmp ),
                  ConstructParamString( '' )
                ) ;
    end ;
  end;


  function TGIS_ConfigIni.fget_FileName : String ;
  begin
    Result := IniObj.fileName ;
  end ;

  procedure TGIS_ConfigIni.Reread ;
  var
    path : String ;
  begin
    if not assigned( FIniObj ) then exit ;

    path := FIniObj.fileName ;
    FreeObject( FIniObj ) ;
    FIniObj := TGIS_IniFile.Create( path ) ;
  end ;

  procedure TGIS_ConfigIni.Save ;
  var
    path : String ;
  begin
    if not FMustSave then exit ;

    path := FIniObj.fileName ;

    if IsStringEmpty( path ) then exit ;

    if SafeFileExists( GetBackupName( path ) ) then
      {$IFDEF OXYGENE}
        DeleteFile( GetBackupName( path ) ) ;
      {$ELSE}
        {$IFDEF LEVEL_XE2_RTL}
          TFile.Delete( GetBackupName( path ) ) ;
        {$ELSE}
          DeleteFile( PChar( GetBackupName( path ) ) ) ;
        {$ENDIF}
      {$ENDIF}
    if SafeFileExists( path ) then
      {$IFDEF OXYGENE}
        CopyFile( path, GetBackupName( path ), True ) ;
      {$ELSE}
        {$IFDEF LEVEL_XE2_RTL}
          TFile.Copy( path, GetBackupName( path ), True ) ;
        {$ELSE}
          CopyFile( PChar(path), PChar( GetBackupName( path )), True ) ;
        {$ENDIF}
      {$ENDIF}
    FIniObj.UpdateFile ;
    FMustSave := False ;
  end ;

  procedure TGIS_ConfigIni.Lock ;
  var
    lst : TGIS_StringList ;
  begin
    if not MustSave then begin
      lst := TGIS_StringList.Create ;
      try
        IniObj.GetStrings( lst ) ;
        lockSentinel := lst.Text ;
      finally
        FreeObject( lst ) ;
      end ;
    end
    else
      lockSentinel := '' ;
  end;

  procedure TGIS_ConfigIni.Unlock ;
  var
    lst : TGIS_StringList ;
  begin
    if not IsStringEmpty( lockSentinel ) then begin
      lst := TGIS_StringList.Create ;
      try
        IniObj.GetStrings( lst ) ;
        if lockSentinel = lst.Text then
          FMustSave := False ;
      finally
        FreeObject( lst ) ;
      end ;
    end ;

    lockSentinel := '' ;
  end;

  procedure TGIS_ConfigIni.SetLayer(
    const _layer : TObject
  ) ;
  begin
    if IsSameType( self, typeOf(TGIS_ConfigIni) )
    then begin
      // precisely TGIS_Ini instance

      layerId := getLayerNo( nil ) ;
      buildParamList( True ) ;
    end
    else begin
      // TGIS_Ini inherited instance

      layerId := getLayerNo( _layer ) ;
      buildParamList( assigned( _layer ) ) ;
    end ;

    SetSection( 0, False ) ;
  end ;

  procedure TGIS_ConfigIni.AddSubLayer(
    const _layer     : TObject ;
    const _layerno   : Integer ;
    const _sublayer  : Integer
   ) ;
  var
    sect_tmp : String ;
    stmp     : String ;
  begin
    sect_tmp := getSubSectionName( _layerno, _sublayer, 0 ) ;
    stmp     := ConstructParamString( TGIS_Layer( _layer ).Name ) ;

    IniObj.WriteString( sect_tmp, GIS_INI_NAME, ConstructParamString( stmp ) );
  end;

  procedure TGIS_ConfigIni.SetSubLayer(
    const _layer : TObject
  ) ;
  begin
    if IsSameType( self, typeOf(TGIS_ConfigIni) )
    then begin
      // precisely TGIS_Ini instance

      subLayerId := getSubLayerNo( _layer ) ;
      buildSubParamList( assigned( _layer ) ) ;
    end
    else begin
      // TGIS_Ini inherited instance

      subLayerId := getSubLayerNo( _layer ) ;
      buildSubParamList( assigned( _layer ) ) ;
    end ;

    SetSubSection( 0, False ) ;
  end ;

  procedure TGIS_ConfigIni.SetGroup(
    const _layer : TObject
  ) ;
  begin
    if IsSameType( self, typeOf(TGIS_ConfigIni) )
    then begin
      // precisely TGIS_Ini instance

      layerId := getGroupNo( nil ) ;
      buildGroupParamList( True ) ;
    end
    else begin
      // TGIS_Ini inherited instance

      layerId := getGroupNo( _layer ) ;
      buildGroupParamList( assigned( _layer ) ) ;
    end ;

    SetGroupSection( 0 ) ;
  end ;

  function TGIS_ConfigIni.SetSection(
    const _index  : Integer ;
    const _force  : Boolean
  ) : Boolean ;
  begin
    Result := False ;
    sectionSelected := _index ;
    Section := getSectionName( layerId, sectionSelected ) ;
    if sectionSelected < sectionCount then
      Result := True ;
  end ;

  procedure TGIS_ConfigIni.SetStrings(
    _list : TGIS_Strings
  ) ;
  begin
    IniObj.SetStrings( _list )  ;
  end;

  procedure TGIS_ConfigIni.GetStrings(
    _list : TGIS_Strings
  ) ;
  begin
    IniObj.GetStrings( _list )  ;
  end;

  function TGIS_ConfigIni.SetSubSection(
    const _index : Integer ;
    const _force : Boolean
  ) : Boolean ;
  begin
    Result := False ;
    sectionSelected := _index ;
    Section := getSubSectionName( layerId, subLayerId, sectionSelected ) ;
    if sectionSelected < subSectionCount then
      Result := True ;
  end ;

  function TGIS_ConfigIni.SetGroupSection(
    const _index : Integer
  ) : Boolean ;
  begin
    Result := False ;
    sectionSelected := _index ;
    Section := getGroupSectionName( layerId, sectionSelected ) ;
    if sectionSelected < sectionCount then
      Result := True ;
  end ;

  procedure TGIS_ConfigIni.ClearSections ;
  var
    lst       : TGIS_StringList ;
    sect_no   : Integer     ;
    sect_name : String      ;
  begin
    lst := TGIS_StringList.Create ;
    try
      for sect_no := 1 to GIS_MAX_SECTION_CNT do begin
        sect_name := getSectionName( layerId, sect_no ) ;
        FIniObj.ReadSection( sect_name, lst ) ;
        // section not exists ?
        if lst.Count = 0 then break ;

        FIniObj.EraseSection( sect_name );

        FMustSave := True ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_ConfigIni.ClearGroups ;
  var
    lst       : TGIS_StringList ;
    sect_no   : Integer     ;
    sect_name : String      ;
  begin
    lst := TGIS_StringList.Create ;
    try
      for sect_no := 1 to GIS_MAX_SECTION_CNT do begin
        sect_name := getGroupSectionName( layerId, sect_no ) ;
        FIniObj.ReadSection( sect_name, lst ) ;
        // section not exists ?
        if lst.Count = 0 then break ;

        FIniObj.EraseSection( sect_name );

        FMustSave := True ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_ConfigIni.ClearActiveSection ;
  begin
    FIniObj.EraseSection( Section );
    FMustSave := True ;
  end ;

  procedure TGIS_ConfigIni.ClearSubSections ;
  var
    lst       : TGIS_StringList ;
    sect_no   : Integer     ;
    sect_name : String      ;
  begin
    lst := TGIS_StringList.Create ;
    try
      for sect_no := 1 to GIS_MAX_SECTION_CNT do begin
        sect_name := getSubSectionName( layerId, subLayerId, sect_no ) ;
        FIniObj.ReadSection( sect_name, lst ) ;
        // section not exists ?
        if lst.Count = 0 then break ;

        FIniObj.EraseSection( sect_name );

        FMustSave := True ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_ConfigIni.ReadSectionValues(
    const _name : String ;
    const _list : TGIS_Strings
  ) ;
  begin
    FIniObj.ReadSectionValues( _name, _list ) ;
  end ;

//==============================================================================
// TGIS_IniProject
//==============================================================================

  function TGIS_ConfigProjectIni.fget_PrjLayersCount
    : Integer ;
  var
    i    : Integer ;
    stmp : String ;
  begin
    Result := 0 ;

    for i:=1 to GIS_MAX_LAYERS_CNT do begin
      stmp := FIniObj.ReadString( GIS_INI_LAYER_HEADER + IntToStr(i),
                                  GIS_INI_PATH, '' ) ;
      if IsStringEmpty( stmp ) then exit ;
      Result := i ;
    end ;
  end ;

  function TGIS_ConfigProjectIni.fget_PrjLayerName(
    const _index : Integer
  ) : String ;
  begin
    Result := ParamString(
                FIniObj.ReadString(
                  getSectionName(_index, 0 ),
                  GIS_INI_NAME,
                  ''
                ),
                ''
              ) ;
  end ;

  function TGIS_ConfigProjectIni.fget_PrjLayerPath(
    const _index : Integer
  ) : String ;
  begin
    Result := ParamString(
                FIniObj.ReadString(
                  getSectionName(_index, 0),
                  GIS_INI_PATH,
                  ''
                ),
                ''
              ) ;
  end ;


  function TGIS_ConfigProjectIni.fget_PrjLayerConfig(
    const _index : Integer
  ) : String ;
  begin
    Result := FIniObj.ReadString(
                getSectionName(_index, 0),
                GIS_INI_CONFIG,
                PrjLayerPath[_index]
              ) ;
  end ;

  function TGIS_ConfigProjectIni.fget_IsProject : Boolean ;
  begin
    Result := True ;
  end ;

  function TGIS_ConfigProjectIni.getSectionName(
    const _layer_no : Integer ;
    const _section  : Integer
  ) : String ;
  begin
    assert( _section  >= 0 ) ;
    assert( _section  <  GIS_MAX_SECTION_CNT  ) ;
    assert( _layer_no >= 0 ) ;

    if      _layer_no > GIS_MAX_LAYERS_CNT then
            Result := ''
    else if _layer_no = 0 then
            Result := GIS_INI_GENERAL_HEADER
    else begin
            Result := GIS_INI_LAYER_HEADER ;
            if _layer_no > 0 then
              Result := Result + IntToStr( _layer_no ) ;
            if _section > 0 then
              Result := Result + ' ' + IntToStr( _section ) ;
    end ;
  end ;

  function TGIS_ConfigProjectIni.getSubSectionName(
    const _layer_no : Integer ;
    const _sLayer_no: Integer ;
    const _section  : Integer
   ) : String ;
  begin
    assert( _section  >= 0 ) ;
    assert( _section  <  GIS_MAX_SECTION_CNT  ) ;
    assert( _layer_no >= 0 ) ;

    if      _layer_no > GIS_MAX_LAYERS_CNT then
            Result := ''
    else if _layer_no = 0 then
            Result := GIS_INI_GENERAL_HEADER
    else begin
            Result := GIS_INI_LAYER_HEADER ;
            if _layer_no > 0 then
              Result := Result +
                        IntToStr( _layer_no ) + '.' + IntToStr( _sLayer_no ) ;
            if _section > 0 then
              Result := Result + ' ' + IntToStr( _section ) ;
    end ;
  end ;

  procedure TGIS_ConfigProjectIni.BuildProject(
    const _viewer : IGIS_Viewer
  ) ;
  var
    i,j,k,no,s   : Integer            ;
    idx          : Integer            ;
    {$IFDEF DCC}
      [weak]
    {$ENDIF}
    vwr          : IGIS_Viewer        ;
    ll           : TGIS_Layer         ;
    prj_txt      : TGIS_StringList    ;
    sect_all     : TGIS_StringList    ;
    sect_txt     : TGIS_StringList    ;
    sect_name    : String             ;
    sect_name_sp : String             ;
    sect_name_sps: String             ;
    sect_tmp     : String             ;
    sect_curr    : String             ;
    sect_curr_sp : String             ;
    sect_sub     : String             ;
    stmp         : String             ;
    found        : Boolean            ;
    isHierarchy  : Boolean            ;
    isSublayer   : Boolean            ;
  begin
    assert( assigned( _viewer ) ) ;
    vwr := _viewer as IGIS_Viewer ;

    prj_txt  := TGIS_StringList.Create ;
    sect_all := TGIS_StringList.Create ;
    sect_txt := TGIS_StringList.Create ;
    try
      // read all section names
         FIniObj.ReadSections( sect_all ) ;
      // copy main section
         FIniObj.ReadSectionValues( GIS_INI_GENERAL_HEADER, sect_txt );
         prj_txt.Append( Format( '[%s]', [GIS_INI_GENERAL_HEADER] ) );
         prj_txt.AddStrings( sect_txt  );

      // build list of current parameters for each layer
         no := 0 ;
         for i:=1 to vwr.Items.Count do begin
           found := False ;

           ll := TGIS_Layer( vwr.Items[i-1] ) ;
           if not ll.IsPersistent then continue ;
           inc( no ) ;

           idx := getLayerNo( ll ) ;

           if idx <> 0 then begin
             sect_name := UpperCase( getSectionName( idx, 0 ) ) ;
             sect_name_sp  := sect_name + ' ' ;
             sect_name_sps := sect_name + '.' ;
             sect_sub      := GIS_INI_LAYER_HEADER + IntToStr( idx ) + '.' ;

             // if layer already in project
                for j:=0 to sect_all.Count -1 do begin
                  if ( CompareText( GIS_INI_GENERAL_HEADER,
                                    sect_all[j]
                                  ) = 0
                     ) or
                     ( CompareText( GIS_INI_HIERARCHY_HEADER,
                                    sect_all[j]
                                  ) = 0
                     ) or
                     ( CompareText( GIS_INI_GROUP_HEADER,
                                    sect_all[j]
                                  ) = 0
                     )
                  then
                    continue ;

                  sect_curr    := UpperCase( Trim( sect_all[j] ) ) ;
                  sect_curr_sp := sect_curr + ' ' ;
                  k := Pos( sect_name_sp,
                            sect_curr_sp
                          ) ;
                  if k = StringFirst-1 then begin
                    sect_curr_sp := sect_curr ;
                    k := Pos( sect_name_sps,
                              sect_curr_sp
                            ) ;
                  end;

                  if k = StringFirst then begin
                    found := True ;

                    if sect_name = sect_curr then begin
                      // refresh layer names
                         FIniObj.WriteString( getSectionName( idx, 0 ),
                                              GIS_INI_NAME,
                                              ConstructParamString(
                                                ll.Name
                                              )
                                             ) ;
                      // refresh layer path
                         if WriteFull or ( not IsStringEmpty( ll.Path ) ) then
                           FIniObj.WriteString( getSectionName( idx, 0 ),
                                                GIS_INI_PATH,
                                                ConstructParamString(
                                                  RelativePath( ll.PathWithDriver )
                                                )
                                              ) ;

                        if assigned( ll.SubLayers ) then begin
                          // refresh sublayers
                          for s := 0 to ll.SubLayers.Count - 1 do begin
                            sect_tmp := Format( '%s',
                                                [ getSubSectionName( idx, s, 0 ) ]
                                              ) ;
                            if sect_all.IndexOf( sect_tmp ) =-1 then
                              AddSubLayer( ll.SubLayers[ s ], idx, s ) ;
                          end ;
                        end ;
                    end ;

                    // get section content
                     FIniObj.ReadSectionValues( sect_all[j], sect_txt );
                    // prepare new section name
                     sect_tmp := Format( '[%s%s]',
                                         [ getSectionName( no, 0 ),
                                           Copy( sect_all[j],
                                                 length( sect_name ) + StringFirst,
                                                 2048
                                               )
                                         ]
                                       ) ;
                    // write section to a new project
                     prj_txt.Append( sect_tmp );
                     prj_txt.AddStrings( sect_txt  );

                     isSublayer := Pos( sect_sub, sect_all[j] ) >= StringFirst ;

                    if assigned( ll.SubLayers ) and not isSublayer then begin
                      for s := 0 to ll.SubLayers.Count-1 do begin
                        sect_tmp := Format( '%s',
                                            [ getSubSectionName( idx, s, 0 ) ]
                                          ) ;
                        if sect_all.IndexOf( sect_tmp ) =-1 then begin
                          sect_tmp := Format( '%s',
                                              [ getSubSectionName( no, s, 0 ) ]
                                            ) ;
                          // add missing sublayer sections
                          if sect_all.IndexOf( sect_tmp ) =-1 then begin
                            FIniObj.ReadSectionValues( sect_tmp, sect_txt );
                            sect_tmp := Format( '[%s]',
                                                [ getSubSectionName( no, s, 0 ) ]
                                              ) ;

                            prj_txt.Append( sect_tmp );
                            prj_txt.AddStrings( sect_txt  );
                          end ;
                        end ;
                      end ;
                    end ;
                  end
                end ;
           end ;

           // new layer - add it
              if not found then begin
                sect_tmp := Format( '[%s]', [ getSectionName( no, 0 ) ] ) ;

                prj_txt.Append( sect_tmp );

                prj_txt.Append( Format( '%s=%s',
                                  [ GIS_INI_NAME,
                                    ConstructParamString(
                                      ll.Name
                                    )
                                  ]
                                )
                              ) ;

                if WriteFull or ( not IsStringEmpty( ll.Path ) ) then
                  prj_txt.Append( Format( '%s=%s',
                                    [ GIS_INI_PATH,
                                      ConstructParamString(
                                        RelativePath( ll.PathWithDriver )
                                      )
                                    ]
                                  )
                                ) ;

                if assigned( ll.SubLayers ) then begin
                  for j := 0 to ll.SubLayers.Count-1 do begin

                    sect_tmp := Format( '[%s]',
                                        [ getSubSectionName( no, j, 0 ) ]
                                      ) ;

                    prj_txt.Append( sect_tmp );

                    stmp := ConstructParamString(
                              TGIS_Layer( ll.SubLayers[ j ] ).Name
                            ) ;
                    prj_txt.Append( Format( '%s=%s',
                                      [ GIS_INI_NAME,
                                        stmp
                                      ]
                                    )
                                  ) ;

                    stmp := ConstructParamString(
                              RelativePath(
                                 TGIS_Layer( ll.SubLayers[ j ] ).Path
                              )
                            ) ;
                    if WriteFull or ( not IsStringEmpty( stmp ) ) then
                      prj_txt.Append( Format( '%s=%s',
                                        [ GIS_INI_PATH,
                                          stmp
                                        ]
                                      )
                                    ) ;
                  end;
                end;

              end ;
         end ;

         isHierarchy := False;
      // add all third-party sections
         sect_name := UpperCase( GIS_INI_LAYER_HEADER ) ;
         for j:=0 to sect_all.Count -1 do begin
           if CompareText( GIS_INI_GENERAL_HEADER,
                           Trim( sect_all[j] )
                         ) = 0
           then
             continue
           else if CompareText( GIS_INI_HIERARCHY_HEADER,
                                Trim( sect_all[j] )
                              ) = 0
           then begin
             prj_txt.Append( Format( '[%s]', [ sect_all[j] ] ) ) ;
             sect_txt.Text := vwr.Hierarchy.GetHierarchy( TGIS_ConfigFormat.Ini ) ;
             prj_txt.AddStrings( sect_txt  );
             sect_txt.Text := vwr.Hierarchy.GetGroups ;
             prj_txt.AddStrings( sect_txt  );

             isHierarchy := True ;
             continue ;
           end
           else if Pos( UpperCase( GIS_INI_GROUP_HEADER ), UpperCase( Trim( sect_all[j] ) ) ) >= StringFirst
           then begin
             continue ;
           end ;

           if Pos( sect_name, UpperCase( Trim( sect_all[j] ) ) ) <> StringFirst then
           begin
             // get section content
                FIniObj.ReadSectionValues( sect_all[j], sect_txt );
             // prepare new section name
                 sect_tmp := Format( '[%s]', [ sect_all[j] ] ) ;
             // write section to a new project
                prj_txt.Append( sect_tmp );
                prj_txt.AddStrings( sect_txt  );
           end ;
         end ;

         if not isHierarchy then begin
           prj_txt.Append( Format( '[%s]',
                           [ GIS_INI_HIERARCHY_HEADER ] )
                         );
           sect_txt.Text := vwr.Hierarchy.GetHierarchy( TGIS_ConfigFormat.Ini ) ;
           prj_txt.AddStrings( sect_txt  );
           sect_txt.Text := vwr.Hierarchy.GetGroups ;
           prj_txt.AddStrings( sect_txt  );
         end;
      // set new ini
         FIniObj.SetStrings( prj_txt ) ;

    finally
      FreeObject( prj_txt  );
      FreeObject( sect_all );
      FreeObject( sect_txt );
    end ;

    FMustSave := True ;
  end ;

  procedure TGIS_ConfigProjectIni.WriteHierarchyGroups(
    const _viewer : IGIS_Viewer
  ) ;
  var
    {$IFDEF DCC}
      [weak]
    {$ENDIF}
    vwr     : IGIS_Viewer ;
    vwr_ref : TGIS_ViewerRef ;
    i       : Integer ;

    procedure walkHierarchy(
      const _group : IGIS_HierarchyGroup
    ) ;
    var
      j    : Integer ;
      lg   : TGIS_LayerGroup ;
    begin
      lg := TGIS_LayerGroup.Create ;
      try
        lg.Name       := _group.Name ;
        lg.Caption    := _group.Caption ;
        lg.Active     := _group.Active ;
        lg.Collapsed  := _group.Collapsed ;
        lg.Viewer     := vwr_ref ;
        lg.WriteConfig ;
      finally
        FreeObject( lg ) ;
      end ;

      for j := 0 to _group.GroupsCount - 1 do
        walkHierarchy( IGIS_HierarchyGroup( _group.Groups[j] ) ) ;
    end ;

  begin
    assert( assigned( _viewer ) ) ;
    vwr := _viewer ;

    vwr_ref := TGIS_ViewerRef.Create ;
    try
      vwr_ref.Ref := vwr ;

      for i := 0 to vwr.Hierarchy.GroupsCount - 1 do
        walkHierarchy( vwr.Hierarchy.Groups[ i ] ) ;
    finally
      FreeObject( vwr_ref ) ;
    end ;

    FMustSave := True ;
  end ;

  function TGIS_ConfigProjectIni.ReadHierarchyGroups : TObject ;
  var
    lst : TStringList ;
  begin
    lst := TGIS_StringList.Create ;
    ReadSectionValues( GIS_INI_HIERARCHY_HEADER, lst );
    Result := lst ;
  end ;


//==============================================================================
// TGIS_ConfigShapeStyle
//==============================================================================

  function TGIS_ConfigShapeStyle.fget_IsShapeStyle
    : Boolean ;
  begin
    Result := True ;
  end;

  function TGIS_ConfigShapeStyle.fget_Text: String;
  var
    lst : TGIS_StringList ;
  begin
    lst := TGIS_StringList.Create ;
    try
      lst.Delimiter := '|' ;
      lst.StrictDelimiter := True ;

      GetStrings( lst ) ;
      lst.Delete( 0 ) ;
      Result := lst.DelimitedText ;
    finally
      FreeObject( lst ) ;
    end;
  end;

  procedure TGIS_ConfigShapeStyle.fset_Text(
    const _value : String
  );
  var
    lst : TGIS_StringList ;
  begin
    lst := TGIS_StringList.Create ;
    try
      lst.Delimiter := '|' ;
      lst.StrictDelimiter := True ;

      lst.DelimitedText := _value ;
      lst.Insert( 0, '[' + GIS_INI_GENERAL_HEADER +']' ) ;
      SetStrings( lst ) ;
    finally
      FreeObject( lst ) ;
    end;
  end;

  constructor TGIS_ConfigShapeStyle.Create ;
  begin
    inherited Create( nil, '' ) ;

    FVersion := GIS_CONFIG_VER_DK11_MAJOR * 1000 + GIS_CONFIG_VER_DK11_MINOR ;
    Section := GIS_INI_GENERAL_HEADER ;
  end;

//==================================== END =====================================
end.
