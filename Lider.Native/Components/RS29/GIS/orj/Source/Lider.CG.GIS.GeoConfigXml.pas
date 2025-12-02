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

}

{$IFDEF DCC}
  unit GisConfigXml ;
  {$HPPEMIT '#pragma link "GisConfigXml"'}
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

{$INCLUDE GisInclude.inc}

{$IFDEF CLR}
  uses
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    {$IFDEF LEVEL_XE2_RTL}
      System.IOUtils,
    {$ENDIF}

    GisInterfaces,
    GisTypes,
    GisTypesUI,
    GisConfig,
    GisXmlFiles ;
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
  ///   Encapsulation of layer configuration file.
  /// </summary>
  TGIS_ConfigXml = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Config )

    // property internal values
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FXmlObj : TGIS_XmlFile ;

    private // other private values

    // other access functions
    protected

      /// <inheritdoc/>
      function  readParam              ( const _name    : String
                                       ) : String ; override;

      /// <inheritdoc/>
      procedure readCustomParam        ( const _name    : String ;
                                         const _list    : TGIS_Strings
                                       ) ; override;

      /// <inheritdoc/>
      procedure writeParam             ( const _name    : String ;
                                         const _value   : String ;
                                         const _default : String
                                       ) ; override;

      /// <inheritdoc/>
      procedure writeCustomParam       ( const _name    : String ;
                                         const _list    : TGIS_Strings
                                       ) ; override;

      /// <inheritdoc/>
      procedure clearZones             ( const _name    : String
                                       ) ; override;

      /// <inheritdoc/>
      procedure readZones             ( const _name     : String ;
                                        const _list     : TGIS_StringList
                                      ) ; override;

      /// <inheritdoc/>
      procedure writeZones            ( const _name     : String ;
                                        const _list     : TGIS_StringList ;
                                        const _default  : TGIS_StringList
                                      ) ; override;

      /// <inheritdoc/>
      procedure fset_Section          ( const _section : String
                                      ) ; override ;

    // other access functions
    protected

      function fget_FileName : String ; override;

    protected

      procedure doDestroy        ; override;

    public

      /// <inheritdoc/>
      constructor Create         ( const _layer      : TObject ;
                                   const _path       : String
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Reread         ; override;

      /// <inheritdoc/>
      procedure   Save           ; override;

      /// <inheritdoc/>
      procedure   Lock           ; override;

      /// <inheritdoc/>
      procedure   Unlock         ; override;

      /// <inheritdoc/>
      procedure SetLayer         ( const _layer      : TObject
                                 ) ; override;

      /// <inheritdoc/>
      procedure SetSubLayer      ( const _layer      : TObject
                                 ) ; override;

      /// <inheritdoc/>
      procedure AddSubLayer      ( const _layer      : TObject ;
                                   const _layerno    : Integer ;
                                   const _sublayer   : Integer
                                 ) ; override;

      /// <inheritdoc/>
      procedure SetGroup         ( const _layer      : TObject
                                 ) ; override;

      /// <inheritdoc/>
      function  SetSection       ( const _index      : Integer ;
                                   const _force      : Boolean
                                 ) : Boolean ; override;

      /// <inheritdoc/>
      function  SetSubSection    ( const _index      : Integer ;
                                   const _force      : Boolean
                                 ) : Boolean ; override;

      /// <inheritdoc/>
      function  SetGroupSection  ( const _index      : Integer
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
      procedure ReadSectionValues( const _name : String ;
                                   const _list : TGIS_Strings
                                 ) ; override;

      /// <inheritdoc/>
      procedure SetStrings       ( _list             : TGIS_Strings
                                 ) ; override;

      /// <inheritdoc/>
      procedure GetStrings       ( _list             : TGIS_Strings
                                 ) ; override;
    public
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Underlying ini file.
      /// </summary>
      property IniObj : TGIS_XmlFile read FXmlObj ;
  end ;

  /// <summary>
  ///   Encapsulation of project configuration file.
  /// </summary>
  TGIS_ConfigProjectXml = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ConfigXml )

    // property access functions
    protected

      function  fget_PrjLayersCount : Integer ; override;

      function  fget_PrjLayerName   ( const _index : Integer
                                    ) : String ; override;

      function  fget_PrjLayerPath   ( const _index : Integer
                                    ) : String ; override;

      function  fget_PrjLayerConfig ( const _index : Integer
                                    ) : String ; override;

      function fget_IsProject       : Boolean ; override;

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

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisClasses,
    GisInternals,
    GisLayer,
    GisParams,
    GisResource,
    GisRtl,
    GisXmlDoc;
{$ENDIF}

//==============================================================================
// TGIS_ConfigXml
//==============================================================================

  constructor TGIS_ConfigXml.Create(
    const _layer  : TObject  ;
    const _path   : String
  ) ;
  begin
    inherited Create( _layer, _path ) ;

    if FVersion = 0  then
      FVersion := GIS_CONFIG_VER_DK11_MAJOR * 1000 + GIS_CONFIG_VER_DK11_MINOR ;

    UseRelativePath := True ;
    WriteFull       := False ;
    FConfigFormat   := TGIS_ConfigFormat.Xml ;

    FXmlObj := TGIS_XmlFile.Create( GetPathAbsolute( '', _path ) ) ;

    Section := '' ;

    SetLayer( _layer ) ;
  end ;

  procedure TGIS_ConfigXml.doDestroy ;
  begin
    FreeObject( FXmlObj ) ;

    inherited ;
  end ;

  function TGIS_ConfigXml.fget_FileName : String ;
  begin
    Result := FXmlObj.FileName ;
  end ;

  function TGIS_ConfigXml.readParam(
    const _name : String
  ) : String ;
  begin
    assert( assigned( FXmlObj ) ) ;

    Result := ParamString(
                FXmlObj.ReadString( _name, GIS_PARAM_NIL ),
                GIS_PARAM_NIL,
                False
              ) ;
  end ;

  procedure TGIS_ConfigXml.readCustomParam(
    const _name : String ;
    const _list : TGIS_Strings
  ) ;
  var
    i   : Integer     ;
    lst : TStringList ;
    val : String      ;
  begin
    lst := TStringList.Create ;
    try
      FXmlObj.GetNodes( _name, lst ) ;

      for i := 0 to lst.Count - 1 do begin
        val := ReadString( _name + '.' + lst[i], '' ) ;

        _list.Values[ lst[i]  ] := val ;
      end;

    finally
      FreeObject( lst ) ;
    end;
  end;

  procedure TGIS_ConfigXml.writeParam(
    const _name    : String ;
    const _value   : String ;
    const _default : String
  ) ;
  var
    value   : String ;
    default : String ;
  begin
    assert( assigned( FXmlObj ) ) ;

    value   := ConstructParamString( _value   ) ;
    default := ConstructParamString( _default ) ;

    if not IsStringEmpty( Section ) then
      FXmlObj.AddSection( Section ) ;

    if WriteFull then
      FXmlObj.WriteString( _name, value )
    else begin
      if CompareText( value, default ) <> 0 then
        FXmlObj.WriteString( _name, value )
      else
        FXmlObj.DeleteKey( _name );
    end ;

    FMustSave := True ;
  end ;

  procedure TGIS_ConfigXml.writeCustomParam(
    const _name    : String ;
    const _list    : TGIS_Strings
  ) ;
  var
    i : Integer;
    nam : String ;
    val : String ;
  begin
    // clear section first
    writeParam( _name, '', '' ) ;
    for i := 0 to _list.Count - 1 do begin
      nam := _list.Names[ i ] ;
      val := _list.ValueFromIndex[ i ] ;
      writeParam( _name + '.' + nam, val, '' ) ;
    end;
  end ;

  procedure TGIS_ConfigXml.clearZones(
    const _name : String
  ) ;
  begin
    FXmlObj.ClearZones( _name ) ;
  end ;

  procedure TGIS_ConfigXml.readZones(
    const _name : String ;
    const _list : TGIS_StringList
  ) ;
  begin
    FXmlObj.ReadZoneValues( _name, _list ) ;
  end ;

  procedure TGIS_ConfigXml.writeZones(
    const _name     : String ;
    const _list     : TGIS_StringList ;
    const _default  : TGIS_StringList //? consider removal
  ) ;
  var
    i : Integer ;
    zone : String ;
  begin
    if _list.Count = 0 then
      exit ;

    FXmlObj.AddZone( _name ) ;

    // and enter existing zones only
    for i:= 0 to _list.Count - 1 do begin
      zone := _list[i] ;
      if IsStringEmpty( zone ) then
        break ;

      FXmlObj.AddZoneValue( i, ConstructParamString( zone ) ) ;
    end ;

    FXmlObj.EndZone ;
  end ;

  procedure TGIS_ConfigXml.fset_Section(
    const _section : String
  ) ;
  begin
    inherited fset_Section( _section ) ;

    FXmlObj.SetActiveNode( _section ) ;
  end;

  procedure TGIS_ConfigXml.Reread ;
  var
    path : String ;
  begin
    if not assigned( FXmlObj ) then exit ;

    path := FXmlObj.FileName ;
    FreeObject( FXmlObj ) ;
    FXmlObj := TGIS_XmlFile.Create( path ) ;
  end ;

  procedure TGIS_ConfigXml.Save ;
  var
    path : String ;
  begin
    if not FMustSave then exit ;

    path := FXmlObj.FileName ;

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
    FXmlObj.UpdateFile ;
    FMustSave := False ;
  end ;

  procedure TGIS_ConfigXml.Lock ;
  begin
    // intentionally empty
  end ;

  procedure TGIS_ConfigXml.Unlock ;
  begin
    // intentionally empty
  end ;

  procedure TGIS_ConfigXml.SetLayer(
    const _layer : TObject
  ) ;
  begin
    if IsSameType( self, typeOf(TGIS_ConfigXml) ) then begin
      // precisely TGIS_Xml instance

      FXmlObj.AddLayer( '', '' ) ;
    end
    else begin
      if not assigned( _layer ) then
        FXmlObj.SetViewer
      else
        FXmlObj.SetLayer( TGIS_Layer(_layer).Name ) ;
    end ;

    SetSection( 0, False ) ;
  end ;

  procedure TGIS_ConfigXml.AddSubLayer(
    const _layer     : TObject ;
    const _layerno   : Integer ;
    const _sublayer  : Integer
  ) ;
  begin
    // intentionally empty
  end ;

  procedure TGIS_ConfigXml.SetSubLayer(
    const _layer : TObject
  ) ;
  begin
    if assigned( _layer ) then
      FXmlObj.SetSubLayer( TGIS_Layer(_layer).Name ) ;

    SetSubSection( 0, False ) ;
  end ;

  procedure TGIS_ConfigXml.SetGroup(
    const _layer : TObject
  ) ;
  begin
    FXmlObj.SetGroup(TGIS_Layer(_layer).Name);
    //assert( False ) ;
  end ;

  function TGIS_ConfigXml.SetSection(
    const _index  : Integer ;
    const _force  : Boolean
  ) : Boolean ;
  begin
    Result := FXmlObj.SetSection( _index, _force ) ;
  end ;

  procedure TGIS_ConfigXml.SetStrings(
    _list : TGIS_Strings
  ) ;
  begin
    FXmlObj.SetStrings( _list )  ;
  end ;

  procedure TGIS_ConfigXml.GetStrings(
    _list : TGIS_Strings
  ) ;
  begin
    FXmlObj.GetStrings( _list )  ;
  end ;

  function TGIS_ConfigXml.SetSubSection(
    const _index : Integer ;
    const _force : Boolean
  ) : Boolean ;
  begin
    Result := FXmlObj.SetSubSection( _index, _force ) ;
  end ;

  function TGIS_ConfigXml.SetGroupSection(
    const _index : Integer
  ) : Boolean ;
  begin
    Result := FXmlObj.SetSubSection(_index, false) ;
    //assert( False ) ;
  end ;

  procedure TGIS_ConfigXml.ClearSections ;
  begin
    FXmlObj.ClearSections ;
    FMustSave := True ;
  end ;

  procedure TGIS_ConfigXml.ClearActiveSection ;
  begin
    FXmlObj.ClearActiveNode ;
    FMustSave := True ;
  end ;

  procedure TGIS_ConfigXml.ClearGroups ;
  begin
    assert( False ) ;
    FMustSave := True ;
  end ;

  procedure TGIS_ConfigXml.ClearSubSections ;
  begin
    FXmlObj.ClearSubSections ;
    FMustSave := True ;
  end ;

  procedure TGIS_ConfigXml.ReadSectionValues(
    const _name : String ;
    const _list : TGIS_Strings
  ) ;
  var
    i   : Integer     ;
    lst : TStringList ;
    val : String      ;
  begin
    lst := TStringList.Create ;
    try
      FXmlObj.SetActiveNode( _name ) ;
      FXmlObj.GetNodes( _name, lst ) ;
      FXmlObj.SetRoot ;

      for i := 0 to lst.Count - 1 do begin
        val := ReadString( _name + '.' + lst[i], '' ) ;

        _list.Values[ lst[i]  ] := val ;
      end;

    finally
      FreeObject( lst ) ;
    end;
  end ;

//==============================================================================
// TGIS_ConfigProjectXml
//==============================================================================

  function TGIS_ConfigProjectXml.fget_PrjLayersCount
    : Integer ;
  begin
    Result := FXmlObj.GetLayersCount ;
  end ;

  function TGIS_ConfigProjectXml.fget_PrjLayerName(
    const _index : Integer
  ) : String ;
  begin
    FXmlObj.SetLayer( _index-1 ) ;
    Result := ParamString( FXmlObj.ReadAttribute( GIS_INI_NAME, '' ), '' ) ;
  end ;

  function TGIS_ConfigProjectXml.fget_PrjLayerPath(
    const _index : Integer
  ) : String ;
  begin
    FXmlObj.SetLayer( _index-1 ) ;
    Result := ParamString( FXmlObj.ReadAttribute( GIS_INI_PATH, '' ), '' ) ;
  end ;

  function TGIS_ConfigProjectXml.fget_PrjLayerConfig(
    const _index : Integer
  ) : String ;
  begin
    FXmlObj.SetLayer( _index-1 ) ;
    Result := FXmlObj.ReadString( GIS_INI_CONFIG, PrjLayerPath[_index] ) ;
  end ;

  function TGIS_ConfigProjectXml.fget_IsProject : Boolean ;
  begin
    Result := True ;
  end ;

  procedure TGIS_ConfigProjectXml.BuildProject(
    const _viewer : IGIS_Viewer
  ) ;
  var
    i   : Integer     ;
    {$IFDEF DCC}
      [weak]
    {$ENDIF}
    vwr : IGIS_Viewer ;
    ll  : TGIS_Layer  ;

    procedure build_sublayers( const _ll  : TGIS_Layer ) ;
    var
      ls  : TGIS_Layer  ;
      j   : Integer     ;
    begin
      if assigned( _ll.SubLayers ) then
        for j := 0 to _ll.SubLayers.Count-1 do begin
          ls := TGIS_Layer( _ll.SubLayers[ j ] ) ;

          FXmlObj.AddSubLayer(
            ConstructParamString( ls.Name ),
            ConstructParamString( RelativePath( ls.Path ) )
          ) ;
          FXmlObj.SetSubLayer( ls.Name ) ;
          build_sublayers( ls ) ;
        end ;
    end ;

  begin
    assert( assigned( _viewer ) ) ;
    vwr := _viewer ;

    Section := '' ;
    FXmlObj.AddSection( GIS_XML_VIEWER_TAG    ) ;
    FXmlObj.AddSection( GIS_XML_LAYERS_TAG    ) ;
    FXmlObj.AddSection( GIS_XML_HIERARCHY_TAG ) ;

    FXmlObj.ClearLayers ;
    for i := 0 to vwr.Items.Count-1 do begin

      ll := TGIS_Layer( vwr.Items[i] ) ;
      if not ll.IsPersistent then continue ;

      FXmlObj.AddLayer(
        ConstructParamString( ll.Name ),
        ConstructParamString( RelativePath( ll.PathWithDriver ) )
      ) ;

      build_sublayers( ll ) ;
    end ;
    FMustSave := True ;
  end ;

  procedure TGIS_ConfigProjectXml.WriteHierarchyGroups(
    const _viewer : IGIS_Viewer
  ) ;
  var
    {$IFDEF DCC}
      [weak]
    {$ENDIF}
    vwr : IGIS_Viewer ;
    i   : Integer ;

    procedure walkHierarchy(
      const _node  : IXMLNode ;
      const _group : IGIS_HierarchyGroup
    ) ;
    var
      j, k : Integer ;
      nd   : IXMLNode ;
    begin
      nd := FXmlObj.AddGroup( _node, _group ) ;

      if _group.LayersCount > 0 then
        for k := 0 to _group.LayersCount - 1 do
          FXmlObj.AddGroupLayer( TGIS_Layer( _group.Layers[ k ] ).Name ) ;

      for j := 0 to _group.GroupsCount - 1 do
        walkHierarchy( nd, IGIS_HierarchyGroup( _group.Groups[j] ) ) ;
    end ;

  begin
    assert( assigned( _viewer ) ) ;
    vwr := _viewer ;

    FXmlObj.ClearGroups ;

    for i := 0 to vwr.Hierarchy.GroupsCount - 1 do
      walkHierarchy( nil, vwr.Hierarchy.Groups[ i ] ) ;

    FMustSave := True ;
  end ;


  function TGIS_ConfigProjectXml.ReadHierarchyGroups : TObject;
  begin
    FXmlObj.SetHierarchy ;
    Result := FXmlObj.ActiveNode ;
  end ;

//==================================== END =====================================
end.
