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
  Encapsulation of configuration file reader.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoXmlFiles ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoXmlFiles"'}
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
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoXmlDoc ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF COCOA}
  uses
    TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}

type
  /// <summary>
  ///   Encapsulation of configuration file reader.
  /// </summary>
  TGIS_XmlFile = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )

    private // other private variable
        FFileName       : String ;
        FDoc            : TGIS_XMLDocument ;
        FRoot           : IXMLNode ;
        FActiveNode     : IXMLNode ;
        FActiveLayer    : IXMLNode ;
        FActiveSubLayer : IXMLNode ;
        FActiveGroup    : IXMLNode ;

    private
      prevNode : IXMLNode ;

    private // other private functions
        function  findXmlNode     ( const _root : IXMLNode ;
                                    const _name : String
                                   ) : IXMLNode ;
        function  findXmlNodeOrAdd( const _root : IXMLNode ;
                                    const _name : String
                                   ) : IXMLNode ;
        function  readXmlString   ( const _name  : String ;
                                    const _default : String
                                   ) : String ;
        function  readXmlObject   ( const _name  : String ;
                                    const _default : String
                                   ) : String ;
        procedure writeXmlString  ( const _name  : String ;
                                    const _value : String
                                   ) ;
        procedure writeXmlObject  ( const _name  : String ;
                                    const _value : String
                                   ) ;
        procedure addXmlObject    ( const _root  : IXMLNode ;
                                    const _name  : String
                                   ) ;
        function  findXmlObject   ( const _root  : IXMLNode ;
                                    const _name  : String
                                   ) : IXMLNode;
        function  findLayer       ( const _name : String
                                   ) : IXMLNode ; overload;
        function  findLayer       ( const _index : Integer
                                   ) : IXMLNode ; overload;
        function  findGroup       ( const _name : String
                                   ) : IXMLNode ;
        function  findGroupLayer  ( const _name : String
                                   ) : IXMLNode ;

        function  findSubLayer    ( const _name : String
                                   ) : IXMLNode ;

    {$IFDEF OXYGENE}
      protected
        procedure doDestroy       ; override;
    {$ELSE}
      public

        /// <summary>
        ///   Destructor.
        /// </summary>
        destructor Destroy        ; override;
    {$ENDIF}

    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_fileName">
      ///   configuration file name
      /// </param>
      constructor Create          ( const _fileName : String
                                  ) ;

      /// <summary>
      ///   Update file with current values.
      /// </summary>
      procedure UpdateFile        ;

      /// <summary>
      ///   Parse and fill list with values.
      /// </summary>
      /// <param name="_list">
      ///   list to fill
      /// </param>
      procedure GetStrings        (  const _list    : TGIS_Strings
                                  ) ;

      /// <summary>
      ///   Read and parse values from list.
      /// </summary>
      /// <param name="_list">
      ///   String list
      /// </param>
      procedure SetStrings        (  const _list    : TGIS_Strings
                                  ) ;

      /// <summary>
      ///   Parse and fill list of all subnodes in a node.subnode.subnode form.
      /// </summary>
      /// <param name="_name">
      ///   node name
      /// </param>
      /// <param name="_list">
      ///   list to fill
      /// </param>
      procedure GetNodes          ( const _name    : String       ;
                                    const _list    : TGIS_Strings
                                  ) ;

      /// <summary>
      ///   Add new section.
      /// </summary>
      /// <param name="_name">
      ///   section name
      /// </param>
      procedure AddSection        ( const _name    : String
                                  ) ;
      /// <summary>
      ///   Add new subsection.
      /// </summary>
      /// <param name="_section">
      ///   section name
      /// </param>
      /// <param name="_name">
      ///   subsection name
      /// </param>
      procedure AddSubSection     ( const _section : String ;
                                    const _name    : String
                                  ) ;
      /// <summary>
      ///   Add new layer.
      /// </summary>
      /// <param name="_name">
      ///   layer name
      /// </param>
      /// <param name="_path">
      ///   layer path
      /// </param>
      procedure AddLayer          ( const _name    : String ;
                                    const _path    : String
                                  ) ;
      /// <summary>
      ///   Add new sublayer.
      /// </summary>
      /// <param name="_name">
      ///   sublayer name
      /// </param>
      /// <param name="_path">
      ///   sublayer path
      /// </param>
      procedure AddSubLayer       ( const _name    : String ;
                                    const _path    : String
                                  ) ;
      /// <summary>
      ///   Set active layer.
      /// </summary>
      /// <param name="_name">
      ///   layer name
      /// </param>
      procedure SetLayer          ( const _name    : String
                                  ) ; overload;
      /// <summary>
      ///   Set active layer.
      /// </summary>
      /// <param name="_index">
      ///   layer index
      /// </param>
      procedure SetLayer          ( const _index   : Integer
                                  ) ; overload;
      /// <summary>
      ///   Set active sublayer.
      /// </summary>
      /// <param name="_name">
      ///   sublayer name
      /// </param>
      procedure SetSubLayer       ( const _name    : String
                                  ) ; overload;

      /// <summary>
      ///   Add new group.
      /// </summary>
      /// <param name="_root">
      ///   root node
      /// </param>
      /// <param name="_group">
      ///   group object
      /// </param>
      /// <returns>
      ///   Node representing added group.
      /// </returns>
      function AddGroup           ( const _root    : IXMLNode ;
                                    const _group   : IGIS_HierarchyGroup
                                  ) : IXMLNode ;

      /// <summary>
      ///   Add new group layer.
      /// </summary>
      /// <param name="_name">
      ///   layer name
      /// </param>
      procedure AddGroupLayer     ( const _name    : String
                                  ) ;

      /// <summary>
      ///   Set active group.
      /// </summary>
      /// <param name="_name">
      ///   layer name
      /// </param>
      procedure SetGroup          ( const _name    : String
                                  ) ;

      /// <summary>
      ///   Clear layers section.
      /// </summary>
      procedure ClearLayers ;

      /// <summary>
      ///   Clear layers section.
      /// </summary>
      procedure ClearGroups ;

      /// <summary>
      ///   Set active section.
      /// </summary>
      /// <param name="_index">
      ///   section index
      /// </param>
      /// <param name="_force">
      ///   if True, new section will be created if not exists
      /// </param>
      /// <returns>
      ///   True if section exists
      /// </returns>
      function  SetSection        ( const _index   : Integer ;
                                    const _force   : Boolean
                                  ) : Boolean ;

      /// <summary>
      ///   Set active subsection.
      /// </summary>
      /// <param name="_index">
      ///   subsection index
      /// </param>
      /// <param name="_force">
      ///   if True, new subsection will be created if not exists
      /// </param>
      /// <returns>
      ///   True if subsection exists
      /// </returns>
      function  SetSubSection     ( const _index   : Integer ;
                                    const _force   : Boolean
                                  ) : Boolean ;
      /// <summary>
      ///   Set viewer section.
      /// </summary>
      procedure SetViewer ;

      /// <summary>
      ///   Set hierarchy section.
      /// </summary>
      procedure SetHierarchy ;

      /// <summary>
      ///   Set root level.
      /// </summary>
      procedure SetRoot ;

      /// <summary>
      ///   Set hierarchy section.
      /// </summary>
      /// <param name="_name">
      ///   node name
      /// </param>
      procedure SetActiveNode     ( const _name    : String
                                  ) ;

      /// <summary>
      ///   Write xml node as text.
      /// </summary>
      /// <param name="_name">
      ///   node name
      /// </param>
      /// <param name="_value">
      ///   node text
      /// </param>
      procedure WriteString       ( const _name    : String ;
                                    const _value   : String
                                  ) ;

      /// <summary>
      ///   Write xml node as text.
      /// </summary>
      /// <param name="_name">
      ///   node name
      /// </param>
      /// <param name="_xml">
      ///   node xml value
      /// </param>
      procedure WriteXML          ( const _name    : String ;
                                    const _xml     : String
                                  ) ;

      /// <summary>
      ///   Read xml node as text.
      /// </summary>
      /// <param name="_name">
      ///   node name
      /// </param>
      /// <param name="_value">
      ///   default value
      /// </param>
      /// <returns>
      ///   text value
      /// </returns>
      function  ReadString        ( const _name    : String ;
                                    const _value   : String
                                  ) : String ;
      /// <summary>
      ///   Delete xml node.
      /// </summary>
      /// <param name="_name">
      ///   node name
      /// </param>
      procedure DeleteKey         ( const _name    : String
                                  ) ;
      /// <summary>
      ///   Read node attribute.
      /// </summary>
      /// <param name="_name">
      ///   attribute name
      /// </param>
      /// <param name="_value">
      ///   default value
      /// </param>
      /// <returns>
      ///   Attribute value.
      /// </returns>
      function  ReadAttribute     ( const _name    : String ;
                                    const _value   : String
                                  ) : String ;

      /// <summary>
      ///   Write node attribute.
      /// </summary>
      /// <param name="_name">
      ///   attribute name
      /// </param>
      /// <param name="_value">
      ///   default value
      /// </param>
      procedure WriteAttribute    ( const _name    : String ;
                                    const _value   : String
                                  ) ;
      /// <summary>
      ///   Get number of layers.
      /// </summary>
      /// <returns>
      ///   Layers count.
      /// </returns>
      function  GetLayersCount    : Integer ;

      /// <summary>
      ///   Clear sections list.
      /// </summary>
      procedure ClearSections     ;

      /// <summary>
      ///   Clear subsections list.
      /// </summary>
      procedure ClearSubSections  ;

      /// <summary>
      ///   Clear active node.
      /// </summary>
      procedure ClearActiveNode ;

      /// <summary>
      ///   Add zone-type section. It might be an AltitudeZoneMap or ColorRamp.
      ///   Must be completed with EndZone method.
      /// </summary>
      /// <param name="_name">
      ///   zone name
      /// </param>
      procedure AddZone           ( const _name   : String
                                  ) ;


      /// <summary>
      ///   End zone-type section.
      /// </summary>
      procedure EndZone;

      /// <summary>
      ///   Add a value to the zone.
      /// </summary>
      /// <param name="_id">
      ///   zone id
      /// </param>
      /// <param name="_value">
      ///   value
      /// </param>
      procedure AddZoneValue      ( const _id     : Integer ;
                                    const _value  : String
                                  ) ;
      /// <summary>
      ///   Read zone values to list.
      /// </summary>
      /// <param name="_name">
      ///   zone name
      /// </param>
      /// <param name="_list">
      ///   list to fill
      /// </param>
      procedure ReadZoneValues    ( const _name   : String ;
                                    const _list   : TGIS_StringList
                                  ) ;
      /// <summary>
      ///   Clear a zone section.
      /// </summary>
      /// <param name="_name">
      ///   zone name
      /// </param>
      procedure ClearZones        ( const _name   : String
                                  ) ;


                                  //?
//      /// <summary>
//      ///   Adds color ramp for a
//      /// </summary>
//      procedure AddColorRamp      ( const _name   : String ;
//                                    const _list   : TGIS_StringList
//                                  ) ;
//
//      procedure ReadColorRamp     ( const _name   : String ;
//                                    const _list   : TGIS_StringList
//                                  ) ;


    public
      /// <summary>
      ///   Configuration file name.
      /// </summary>
      /// <value>
      ///   file name
      /// </value>
      property FileName : String read FFileName ;

      /// <summary>
      ///   Active xml node.
      /// </summary>
      /// <value>
      ///   node
      /// </value>
      property ActiveNode : IXMLNode read FActiveNode ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

//==============================================================================
// TGIS_XmlFile
//==============================================================================

  constructor TGIS_XmlFile.Create(
    const _fileName : String
  );
  begin
    inherited Create ;

    FFileName := _fileName ;
    FDoc := TGIS_XMLDocument.Create ;

    if SafeFileExists(_fileName) then
      FDoc.LoadFromFile( _fileName ) ;

    if FDoc.DocumentElement = nil then
      FDoc.DocumentElement := FDoc.CreateNode( GIS_XML_GENERAL_TAG ) ;

    FRoot := FDoc.DocumentElement ;

    FActiveNode     := nil ;
    FActiveLayer    := nil ;
    FActiveSubLayer := nil ;
  end ;

  {$IFDEF OXYGENE}
    procedure TGIS_XmlFile.doDestroy ;
    begin
      FreeObject( FDoc ) ;

      inherited ;
    end ;
  {$ELSE}
    destructor TGIS_XmlFile.Destroy ;
    begin
      FreeObject( FDoc ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_XmlFile.writeXmlObject(
    const _name  : String ;
    const _value : String
  ) ;
  var
    tkn    : TGIS_Tokenizer ;
    i      : Integer ;
    inode  : IXMLNode ;
    anode  : IXMLNode ;
  begin
    if not assigned( FActiveNode ) then
      exit ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( _name, '.' ) ;

      inode := FActiveNode ;
      for i := 0 to tkn.Result.Count-1 do begin
        if i < tkn.Result.Count-1 then
          inode := findXmlNodeOrAdd( inode, tkn.Result[i] )
        else begin
          anode := FActiveNode ;
          try
            FActiveNode := inode ;
            writeXmlString( tkn.Result[i], _value ) ;
          finally
            FActiveNode := anode ;
          end ;
        end ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_XmlFile.addXmlObject(
    const _root  : IXMLNode ;
    const _name  : String
  ) ;
  var
    tkn    : TGIS_Tokenizer ;
    i      : Integer ;
    inode  : IXMLNode ;
  begin
    inode := _root ;
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( _name, '.' ) ;
      for i := 0 to tkn.Result.Count-1 do
        inode := findXmlNodeOrAdd( inode, tkn.Result[i] )
    finally
      FreeObject( tkn ) ;
    end ;
    FActiveNode := inode ;
  end ;

  function TGIS_XmlFile.findXmlObject(
    const _root  : IXMLNode ;
    const _name  : String
  ) : IXMLNode ;
  var
    tkn    : TGIS_Tokenizer ;
    i      : Integer ;
    inode  : IXMLNode ;
  begin
    inode := _root ;
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( _name, '.' ) ;
      for i := 0 to tkn.Result.Count-1 do
        inode := findXmlNode( inode, tkn.Result[i] )
    finally
      FreeObject( tkn ) ;
    end ;
    Result := inode ;
  end ;

  procedure TGIS_XmlFile.writeXmlString(
    const _name  : String ;
    const _value : String
  ) ;
  var
    i         : Integer ;
    existing  : IXMLNode ;
    cnode     : IXMLNode ;
  begin
    if not assigned( FActiveNode ) then
      exit ;

    existing := nil ;

    for i := 0 to FActiveNode.ChildNodes.Count - 1 do begin
      cnode := FActiveNode.ChildNodes[i] ;
      if (cnode.NodeType = TNodeType.ntElement) and (cnode.NodeName = _name) then
      begin
        existing := cnode ;
        Break ;
      end ;
    end ;

    if not assigned(existing) then begin
      existing := FActiveNode.AddChild(_name) ;
      existing.Text := '';
    end ;
    if assigned( existing ) then
      existing.Text := _value ;
  end ;

  function TGIS_XmlFile.readXmlString(
    const _name     : String ;
    const _default  : String
  ) : String ;
  var
    i     : Integer ;
    cnode : IXMLNode ;
  begin
    if assigned( FActiveNode ) then begin
      for i := 0 to FActiveNode.ChildNodes.Count - 1 do begin
        cnode := FActiveNode.ChildNodes[i] ;
        if (cnode.NodeType = TNodeType.ntElement) and (cnode.NodeName = _name) then
        begin
          if (cnode.ChildNodes.Count > 0) and (cnode.ChildNodes[0].NodeType = TNodeType.ntCData) then
            Result := VarToString( cnode.ChildNodes[0].NodeValue )
          else
            Result := cnode.Text;
          Exit;
        end ;
      end ;
    end ;
    Result := _default;
  end ;

  function TGIS_XmlFile.readXmlObject(
    const _name     : String ;
    const _default  : String
  ) : String ;
  var
    tkn    : TGIS_Tokenizer ;
    i      : Integer ;
    inode  : IXMLNode ;
    anode  : IXMLNode ;
  begin
    if not assigned( FActiveNode ) then begin
      Result := _default ;
      exit ;
    end ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( _name, '.' ) ;

      inode := FActiveNode ;
      for i := 0 to tkn.Result.Count-1 do begin
        if i < tkn.Result.Count-1 then
          inode := findXmlNode( inode, tkn.Result[i] )
        else begin
          anode := FActiveNode ;
          try
            FActiveNode := inode ;
            Result := readXmlString( tkn.Result[i], _default ) ;
          finally
            FActiveNode := anode ;
          end ;
        end ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  function TGIS_XmlFile.findXmlNode(
    const _root : IXMLNode ;
    const _name : String
  ) : IXMLNode ;
  var
    i     : Integer ;
    cnode : IXMLNode;
    node  : IXMLNode ;
  begin
    Result := nil ;
    node := _root ;
    if not assigned( node ) then
      node := FRoot ;

    if assigned( node ) then begin
      for i := 0 to node.ChildNodes.Count - 1 do begin
        cnode := node.ChildNodes[i] ;
        if (cnode.NodeType = TNodeType.ntElement) and
           (cnode.NodeName = _name              ) then begin
          Result := cnode ;
          Exit ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_XmlFile.findXmlNodeOrAdd(
    const _root : IXMLNode ;
    const _name : String
  ) : IXMLNode ;
  var
    root : IXMLNode ;
  begin
    root := _root ;
    if not assigned( root ) then
      root := FRoot ;

    Result := findXmlNode( root, _name ) ;
    if not assigned( Result ) then
      Result := root.AddChild( _name ) ;
  end ;

  function TGIS_XmlFile.findLayer(
    const _name : String
  ) : IXMLNode ;
  var
    node  : IXMLNode ;
    lnode : IXMLNode ;
    i     : Integer ;
  begin
    Result := nil ;
    node := findXmlNodeOrAdd( nil, GIS_XML_LAYERS_TAG ) ;
    if assigned( node ) then begin
      for i := 0 to node.ChildNodes.Count - 1 do begin
        lnode := node.ChildNodes[i] ;
        if CompareText( VarToString(lnode.Attributes[GIS_INI_NAME]), _name ) = 0 then begin
          Result := lnode ;
          exit ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_XmlFile.findLayer(
    const _index : Integer
  ) : IXMLNode ;
  var
    node : IXMLNode ;
  begin
    node := findXmlNodeOrAdd( nil, GIS_XML_LAYERS_TAG ) ;
    if assigned( node ) and (_index < node.ChildNodes.Count) then
      Result := node.ChildNodes[_index]
    else
      Result := nil ;
  end ;

  function TGIS_XmlFile.findGroup(
    const _name : String
  ) : IXMLNode ;
  var
    node  : IXMLNode ;

      function lookForGroup( const _node : IXMLNode ) : IXMLNode ;
      var
        lnode : IXMLNode ;
        i     : Integer ;
      begin
        Result := nil ;
        if CompareText( VarToString(_node.Attributes[GIS_INI_NAME]), _name ) = 0 then begin
          Result := _node ;
          exit ;
        end ;

        for i := 0 to _node.ChildNodes.Count - 1 do begin
          lnode := _node.ChildNodes[i] ;
          Result := lookForGroup( lnode ) ;
          if assigned( Result ) then
            break ;
        end ;
      end ;

  begin
    Result := nil ;
    node := findXmlNodeOrAdd( nil, GIS_XML_HIERARCHY_TAG ) ;
    node := findXmlNodeOrAdd( node, GIS_XML_GROUPS_TAG ) ;

    if assigned( node ) then
      Result := lookForGroup( node ) ;
  end ;

  function TGIS_XmlFile.findSubLayer(
    const _name : String
  ) : IXMLNode ;
  var
    lnode : IXMLNode ;
    i     : Integer ;
    sublayers : IXMLNode ;
  begin
    Result := nil ;
    if assigned( FActiveLayer ) then begin
      sublayers := findXmlNodeOrAdd( FActiveLayer, GIS_XML_SUBLAYERS_TAG ) ;

      for i := 0 to sublayers.ChildNodes.Count - 1 do begin
        lnode := sublayers.ChildNodes[i] ;
        if CompareText( VarToString(lnode.Attributes[GIS_INI_NAME]), _name ) = 0 then begin
          Result := lnode ;
          exit ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_XmlFile.GetLayersCount
    : Integer ;
  var
    node : IXMLNode ;
  begin
    node := findXmlNodeOrAdd( nil, GIS_XML_LAYERS_TAG ) ;
    if assigned( node ) then
      Result := node.ChildNodes.Count
    else
      Result := 0 ;
  end ;

  procedure TGIS_XmlFile.GetStrings(
    const _list : TGIS_Strings
  );
  var
    str : String ;
  begin
    FDoc.SaveToXML( str ) ;
    _list.Text := str ;
  end ;

  procedure TGIS_XmlFile.SetStrings(
    const _list : TGIS_Strings
  ) ;
  var
    bload : Boolean ;
  begin
    { TODO : Add translation from INI to XML for user sections }
    bload := True ;
    if (_list.Count > 0) then begin
      if _list[0] = Format( '[%s]', [GIS_INI_GENERAL_HEADER] ) then
        bload := False ;
    end ;

    if bload then
      FDoc.LoadFromXML( _list.Text ) ;

    if FDoc.DocumentElement = nil then
      FDoc.DocumentElement := FDoc.CreateNode( GIS_XML_GENERAL_TAG ) ;

    FRoot := FDoc.DocumentElement ;

    FActiveNode     := nil ;
    FActiveLayer    := nil ;
    FActiveSubLayer := nil ;

  end ;

  procedure TGIS_XmlFile.GetNodes(
    const _name : String       ;
    const _list : TGIS_Strings
  ) ;

    procedure list_nodes( const _longname : String; const _node : IXMLNode ) ;
    var
      i     : Integer ;
      sname : String ;
      node  : IXMLNode ;
    begin
      if not assigned( _node ) then
        exit ;

      for i := 0 to _node.ChildNodes.Count-1 do begin
        node := _node.ChildNodes[i] ;
        if node.NodeType <> TNodeType.ntElement then
          continue ;

        if not IsStringEmpty( _longname ) then
          sname := _longname + '.' + node.NodeName
        else
          sname := node.NodeName ;
        _list.Add( sname ) ;
        list_nodes( sname, node ) ;
      end ;
    end ;
  begin
    _list.Clear ;

    if assigned( FActiveNode ) then begin
      if FActiveNode.NodeName = _name then
        list_nodes( '', FActiveNode )
      else
        list_nodes( '', findXmlNode( FActiveNode, _name ) )
    end
    else
      list_nodes( '', findXmlNode( FRoot, _name ) )
  end ;


  procedure TGIS_XmlFile.UpdateFile ;
  begin
    FDoc.SaveToFile( FFileName ) ;
  end ;

  procedure TGIS_XmlFile.AddSection(
    const _name : String
  ) ;
  begin
    FActiveNode := findXmlNodeOrAdd( nil, _name ) ;
  end ;

  procedure TGIS_XmlFile.AddSubSection(
    const _section : String ;
    const _name    : String
  ) ;
  var
    node : IXMLNode ;
  begin
    node := findXmlNodeOrAdd( nil, _name ) ;
    if assigned( node ) then
      FActiveNode := node.AddChild( _name ) ;
  end ;

  procedure TGIS_XmlFile.AddZone(
    const _name : String
  ) ;
  begin
    prevNode := FActiveNode ;

    addXmlObject( FActiveNode, _name ) ;
  end ;

  procedure TGIS_XmlFile.EndZone ;
  begin
    FActiveNode := prevNode ;
  end ;

//  procedure TGIS_XmlFile.AddColorRamp(
//    const _name : String ;
//    const _list : TGIS_StringList
//  ) ;
//  var
//    i         : Integer ;
//    value     : String ;
//    zone_node : IXMLNode ;
//    prev_node : IXMLNode ;
//  begin
//    prev_node := FActiveNode ;
//
//    addXmlObject( FActiveNode, _name ) ;
//
//    //? in future use ClearZone
//    ClearActiveNode ;
//
//    if assigned( FActiveNode ) then begin
//      for i := 0 to _list.Count - 1 do begin
//        value := _list[i] ;
//
//        zone_node := FActiveNode.AddChild( GIS_XML_ZONE_TAG ) ;
//        zone_node.Text := value ;
//        // legacy method
//        // node.Attributes[ GIS_XML_VALUE_TAG ] := value ;
//      end ;
//    end ;
//
//    // restore to previous active node
//    FActiveNode := prev_node ;
//  end ;

//  procedure TGIS_XmlFile.ReadColorRamp(
//    const _name : String ;
//    const _list : TGIS_StringList
//  ) ;
//  var
//    i_zone    : Integer ;
//    value     : String ;
//    ramp_node : IXMLNode ;
//    zone_node : IXMLNode ;
//  begin
//    ramp_node := findXmlObject( FActiveNode, _name ) ;
//    if assigned( ramp_node ) then begin
//
//      _list.Clear ;
//
//      for i_zone := 0 to ramp_node.ChildNodes.Count-1 do begin
//        zone_node := ramp_node.ChildNodes[i_zone] ;
//        if zone_node.HasAttribute( GIS_XML_VALUE_TAG ) then
//           // legacy method
//           value := VarToString( zone_node.Attributes[GIS_XML_VALUE_TAG] )
//        else
//          value := zone_node.Text ;
//
//        _list.Add( value ) ;
//      end ;
//    end ;
//  end ;

  procedure TGIS_XmlFile.AddZoneValue(
    const _id     : Integer ;
    const _value  : String
  ) ;
  var
    node : IXMLNode ;
  begin
    if assigned( FActiveNode ) then begin
      node := FActiveNode.AddChild( GIS_XML_ZONE_TAG ) ;
      node.Text := _value ;
//      node.Attributes[ GIS_XML_VALUE_TAG ] := _value ;
    end ;
  end ;

  procedure TGIS_XmlFile.ReadZoneValues(
    const _name : String ;
    const _list : TGIS_StringList
  ) ;
  var
    i         : Integer ;
    zone      : String ;
    node      : IXMLNode ;
    zone_node : IXMLNode ;
  begin
    _list.Clear ;
    node := findXmlObject( FActiveNode, _name ) ;
    if not assigned(node) then begin
      node := findXmlObject( FActiveLayer, _name ) ;
      if not assigned(node) then
        exit ;
    end ;

    for i := 0 to node.ChildNodes.Count-1 do begin
      zone_node := node.ChildNodes[i] ;
      if zone_node.HasAttribute( GIS_XML_VALUE_TAG ) then
         // legacy method
         zone := VarToString( zone_node.Attributes[GIS_XML_VALUE_TAG] )
      else
        zone := zone_node.Text ;

      if IsStringEmpty( zone ) then
        continue ;

      _list.Add( zone ) ;
    end ;
  end ;

  procedure TGIS_XmlFile.ClearZones(
    const _name : String
  ) ;
  var
    node  : IXMLNode ;
  begin
    node := findXmlObject( FActiveLayer, _name ) ;
    if not assigned( node ) then begin
      node := findXmlObject( FActiveNode, _name ) ;
      if not assigned( node ) then
        exit ;
    end ;

    node.ChildNodes.Clear ;
  end ;

  function TGIS_XmlFile.ReadString(
    const _name    : String ;
    const _value   : String
  ) : String ;
  begin
    if Pos( '.', _name ) > StringFirst then
      Result := readXmlObject( _name, _value )
    else
      Result := readXmlString( _name, _value ) ;
  end ;

  procedure TGIS_XmlFile.WriteString(
    const _name    : String ;
    const _value   : String
  ) ;
  begin
    if Pos( '.', _name ) > StringFirst then
      writeXmlObject( _name, _value )
    else
      writeXmlString( _name, _value ) ;
  end ;

  procedure TGIS_XmlFile.WriteXML(
    const _name    : String ;
    const _xml     : String
  ) ;
  var
    i         : Integer ;
    existing  : IXMLNode ;
    cnode     : IXMLNode ;
  begin
    if not assigned( FActiveNode ) then
      exit ;

    existing := nil ;

    for i := 0 to FActiveNode.ChildNodes.Count - 1 do begin
      cnode := FActiveNode.ChildNodes[i] ;
      if (cnode.NodeType = TNodeType.ntElement) and (cnode.NodeName = _name) then
      begin
        existing := cnode ;
        Break ;
      end ;
    end ;

    if not assigned(existing) then begin
      existing := FActiveNode.AddChild(_name) ;
      existing.Text := '';
    end ;
    if assigned( existing ) then
      existing.Text := _xml ;
  end ;

  procedure TGIS_XmlFile.SetLayer(
    const _name : String
  ) ;
  begin
    FActiveLayer := findLayer( _name ) ;
    FActiveNode := FActiveLayer ;
  end ;

  procedure TGIS_XmlFile.SetLayer(
    const _index   : Integer
  ) ;
  begin
    FActiveLayer := findLayer( _index ) ;
    FActiveNode := FActiveLayer ;
  end ;

  procedure TGIS_XmlFile.SetSubLayer(
    const _name : String
  ) ;
  begin
    FActiveSubLayer := findSubLayer( _name ) ;
    FActiveNode := FActiveSubLayer ;
  end ;

  function TGIS_XmlFile.SetSection(
    const _index   : Integer ;
    const _force   : Boolean
  ) : Boolean ;
  var
    sections : IXMLNode ;
  begin
    Result := False ;

    if assigned( FActiveLayer ) then begin
      sections := findXmlNodeOrAdd( FActiveLayer, GIS_XML_SECTIONS_TAG ) ;

      if _index < sections.ChildNodes.Count then begin
        FActiveNode := sections.ChildNodes[_index] ;
        Result := True ;
      end
      else begin
        if _force then
          FActiveNode := sections.AddChild( GIS_XML_SECTION_TAG ) ;
      end ;
    end ;
  end ;

  function TGIS_XmlFile.SetSubSection(
    const _index   : Integer ;
    const _force   : Boolean
  ) : Boolean ;
  var
    sections : IXMLNode ;
  begin
    Result := False ;

    if assigned( FActiveSubLayer ) then begin
      sections := findXmlNodeOrAdd( FActiveSubLayer, GIS_XML_SECTIONS_TAG ) ;

      if _index < sections.ChildNodes.Count then begin
        FActiveNode := sections.ChildNodes[_index] ;
        Result := True ;
      end
      else begin
        if _force then
          FActiveNode := sections.AddChild( GIS_XML_SECTION_TAG ) ;
      end ;
    end ;
  end ;

  procedure TGIS_XmlFile.ClearLayers ;
  var
    node : IXMLNode ;
  begin
    node := findXmlNodeOrAdd( nil, GIS_XML_LAYERS_TAG ) ;
    if assigned( node ) then
      node.ChildNodes.Clear ;
  end ;

  procedure TGIS_XmlFile.ClearGroups ;
  var
    node : IXMLNode ;
  begin
    node := findXmlNodeOrAdd( nil, GIS_XML_HIERARCHY_TAG ) ;
    if assigned( node ) then begin
      node := findXmlNodeOrAdd( node, GIS_XML_GROUPS_TAG ) ;
      if assigned( node ) then
        node.ChildNodes.Clear ;
    end ;
  end ;

  procedure TGIS_XmlFile.ClearSections ;
  var
    sections : IXMLNode ;
    i        : Integer ;
  begin
    if assigned( FActiveLayer ) then begin
      sections := findXmlNode( FActiveLayer, GIS_XML_SECTIONS_TAG ) ;
      if assigned( sections ) then
        for i := sections.ChildNodes.Count - 1 downto 1 do
          sections.ChildNodes.Delete( i ) ;
    end ;
  end ;

  procedure TGIS_XmlFile.ClearSubSections ;
  var
    sections : IXMLNode ;
    i        : Integer ;
  begin
    if assigned( FActiveSubLayer ) then begin
      sections := findXmlNode( FActiveSubLayer, GIS_XML_SECTIONS_TAG ) ;
      if assigned( sections ) then
        for i := sections.ChildNodes.Count - 1 downto 1 do
          sections.ChildNodes.Delete( i ) ;
    end ;
  end ;

  procedure TGIS_XmlFile.ClearActiveNode ;
  var
    i : Integer ;
  begin
    if assigned( FActiveNode ) then begin
      for i := FActiveNode.ChildNodes.Count - 1 downto 0 do
        FActiveNode.ChildNodes.Delete( i ) ;
    end ;
  end ;

  procedure TGIS_XmlFile.AddLayer(
    const _name    : String ;
    const _path    : String
  ) ;
  var
    node : IXMLNode ;
  begin
    if not IsStringEmpty( _name ) then begin
      // for project file
      SetLayer( _name ) ;

      if not assigned( FActiveLayer ) then begin
        node := findXmlNodeOrAdd( nil, GIS_XML_LAYERS_TAG ) ;
        FActiveLayer := node.AddChild( GIS_XML_LAYER_TAG ) ;
        FActiveLayer.Attributes[ GIS_INI_NAME ] := _name ;
        FActiveLayer.Attributes[ GIS_INI_PATH ] := _path ;
      end ;
    end
    else begin
      // for config file
      FActiveLayer := findXmlNodeOrAdd( nil, GIS_XML_LAYER_TAG ) ;
    end ;

    FActiveNode := FActiveLayer ;
  end ;

  procedure TGIS_XmlFile.SetGroup(
    const _name : String
  ) ;
  begin
    FActiveGroup := findGroup( _name ) ;
    FActiveNode := FActiveGroup ;
  end ;

  function TGIS_XmlFile.AddGroup(
    const _root  : IXMLNode ;
    const _group : IGIS_HierarchyGroup
  ) : IXMLNode ;
  var
    gnode : IXMLNode ;
    hnode : IXMLNode ;
  begin
    SetGroup( _group.Name ) ;

    if not assigned( FActiveGroup ) then begin
      hnode := findXmlNodeOrAdd( nil, GIS_XML_HIERARCHY_TAG ) ;

      if assigned( _root ) then
        FActiveGroup := _root.AddChild( GIS_XML_GROUP_TAG )
      else begin
        gnode := findXmlNodeOrAdd( hnode, GIS_XML_GROUPS_TAG ) ;
        FActiveGroup := gnode.AddChild( GIS_XML_GROUP_TAG ) ;
      end ;

      FActiveGroup.Attributes[ GIS_INI_NAME ] := _group.Name ;
    end ;

    FActiveGroup.Attributes[ GIS_INI_CAPTION   ] := _group.Caption ;
    FActiveGroup.Attributes[ GIS_INI_COLLAPSED ] := _group.Collapsed ;
    FActiveGroup.Attributes[ GIS_INI_ACTIVE    ] := _group.Active ;

    FActiveNode := FActiveGroup ;
    Result := FActiveNode ;
  end ;

  function TGIS_XmlFile.findGroupLayer(
    const _name : String
  ) : IXMLNode ;
  var
    node  : IXMLNode ;
    lnode : IXMLNode ;
    i     : Integer ;
  begin
    Result := nil ;
    node := findXmlNodeOrAdd( FActiveGroup, GIS_XML_LAYERS_TAG ) ;
    if assigned( node ) then begin
      for i := 0 to node.ChildNodes.Count - 1 do begin
        lnode := node.ChildNodes[i] ;
        if CompareText( VarToString(lnode.Attributes[GIS_INI_NAME]), _name ) = 0 then begin
          Result := lnode ;
          exit ;
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_XmlFile.AddGroupLayer(
    const _name    : String
  ) ;
  var
    node : IXMLNode ;
  begin
    node := findXmlNodeOrAdd( FActiveGroup, GIS_XML_LAYERS_TAG ) ;
    // find node
    FActiveNode := findGroupLayer( _name ) ;
    if not assigned( FActiveNode ) then begin
      FActiveNode := node.AddChild( GIS_XML_LAYER_TAG ) ;
      FActiveNode.Attributes[ GIS_INI_NAME ] := _name ;
    end ;
  end ;

  procedure TGIS_XmlFile.AddSubLayer(
    const _name    : String ;
    const _path    : String
  ) ;
  var
    node : IXMLNode ;
  begin
    SetSubLayer( _name ) ;

    if not assigned( FActiveSubLayer ) then begin
      node := findXmlNodeOrAdd( FActiveLayer, GIS_XML_SUBLAYERS_TAG ) ;
      FActiveSubLayer := node.AddChild( GIS_XML_SUBLAYER_TAG ) ;
      FActiveSubLayer.Attributes[ GIS_INI_NAME ] := _name ;
      FActiveSubLayer.Attributes[ GIS_INI_PATH ] := _path ;
    end ;

    FActiveNode := FActiveSubLayer ;
  end ;

  procedure TGIS_XmlFile.SetViewer ;
  begin
    FActiveNode   := findXmlNodeOrAdd( nil, GIS_XML_VIEWER_TAG ) ;
    FActiveLayer  := nil ;
  end ;

  procedure TGIS_XmlFile.SetHierarchy ;
  begin
    FActiveNode   := findXmlNode( nil, GIS_XML_HIERARCHY_TAG ) ;
    FActiveLayer  := nil ;
  end ;

  procedure TGIS_XmlFile.SetActiveNode(
    const _name : String
  ) ;
  begin
    if not IsStringEmpty( _name ) then
      FActiveNode := findXmlNodeOrAdd( nil, _name )
    else
      FActiveNode := nil
  end ;

  procedure TGIS_XmlFile.SetRoot ;
  begin
    FActiveNode := FRoot ;
  end ;

  procedure TGIS_XmlFile.DeleteKey(
    const _name : String
  ) ;
  var
    i     : Integer ;
    cnode : IXMLNode ;
    anode : IXMLNode ;
    tkn   : TGIS_Tokenizer ;
  begin
    if not assigned( FActiveNode ) then exit ;

    if Pos( '.', _name ) > StringFirst then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.ExecuteEx( _name, '.' ) ;
        cnode := FActiveNode ;

        for i := 0 to tkn.Result.Count-1 do begin
          if i < tkn.Result.Count-1 then
            cnode := findXmlNode( cnode, tkn.Result[i] )
          else begin
            anode := FActiveNode ;
            try
              FActiveNode := cnode ;
              DeleteKey( tkn.Result[i] ) ;
            finally
              FActiveNode := anode ;
            end ;
          end ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    end
    else begin
      for i := FActiveNode.ChildNodes.Count - 1 downto 0 do begin
        cnode := FActiveNode.ChildNodes[i] ;
        if (cnode.NodeType = TNodeType.ntElement) and
           (cnode.NodeName = _name              ) then begin
          FActiveNode.ChildNodes.Delete( i ) ;
          Exit ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_XmlFile.ReadAttribute(
    const _name    : String ;
    const _value   : String
  ) : String ;
  var
    val : OleVariant ;
  begin
    if assigned( FActiveNode ) then begin
      val := FActiveNode.Attributes[_name] ;
      if VarIsEmpty( val ) then
        Result := _value
      else
        Result := VarToString( val ) ;
    end
    else
      Result := _value ;
  end ;

  procedure TGIS_XmlFile.WriteAttribute(
    const _name    : String ;
    const _value   : String
  ) ;
  begin
    if assigned( FActiveNode ) then
      FActiveNode.Attributes[_name] := _value ;
  end ;


//==================================== END =====================================
end.

