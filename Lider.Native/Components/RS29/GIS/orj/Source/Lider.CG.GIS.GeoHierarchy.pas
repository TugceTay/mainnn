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
  Hierarchy management class used by viewer and legend.
}

{$IFDEF DCC}
  unit GisHierarchy ;
  {$HPPEMIT '#pragma link "GisHierarchy"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL,
    TatukGIS.RTL.XML   ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Generics.Collections,

    GisRtl,
    GisInterfaces,
    GisConfig,
    GisXmlDoc,
    GisLayer,
    GisClasses,
    GisTypes ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml    ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}

type

//  TGIS_Layer = {$IFDEF OXYGENE} public abstract {$ENDIF} class ;

  {$REGION 'TGIS_HierarchyGroupEvent*'}
  //----------------------------------------------------------------------------

  {$IFDEF OXYGENE}
    /// <summary>
    ///   Provides data for the hierarchy group event.
    /// </summary>
    TGIS_HierarchyGroupEventArgs = public class ( EventArgs )
      private
        FGroup : IGIS_HierarchyGroup ;

      public
        /// <summary>
        /// Create an object
        /// </summary>
        /// <param name="_group">
        ///   given layer
        /// </param>
        constructor Create  ( const _group : IGIS_HierarchyGroup
                            ) ;

      public
        /// <summary>
        ///   Group for hierarchy group event.
        /// </summary>
        property Group      : IGIS_HierarchyGroup read FGroup ;
    end ;

    /// <summary>
    ///   Standard event for hierarchy group.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_e">
    ///   TGIS_HierarchyGroupEvent arguments
    /// </param>
    TGIS_HierarchyGroupEvent = public procedure(
      _sender  : Object         ;
      _e       : TGIS_HierarchyGroupEventArgs
    ) of object ;
  {$ELSE}
    /// <summary>
    ///   Standard event for OnXXXX.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_group">
    ///   group to be passed
    /// </param>
    {$IFDEF GENXDK}
      TGIS_HierarchyGroupEvent = procedure(
        var _translated : Boolean ;
            _sender     : TObject   ;
            _group      : IGIS_HierarchyGroup
      ) of object ;
    {$ELSE}
      TGIS_HierarchyGroupEvent = procedure(
        _sender     : TObject   ;
        _group      : IGIS_HierarchyGroup
      ) of object ;
    {$ENDIF}
  {$ENDIF}
  {$ENDREGION}

  TGIS_HierarchyManager = class ;

  /// <summary>
  ///   Hierarchy group class.
  /// </summary>
  TGIS_HierarchyGroup = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TInterfacedObject, IGIS_HierarchyGroup
                                {$IFDEF CLR}
                                  , IDisposable
                                {$ENDIF}
                              )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

        /// <summary>
        ///   Hierarchy groups list.
        /// </summary>
        FGroups     : TList<IGIS_HierarchyGroup> ;

        /// <summary>
        ///   Hierarchy layer list.
        /// </summary>
        FLayers     : TGIS_LayerAbstractList ;

        /// <summary>
        ///   Parent group.
        /// </summary>
        FParentGroup : TGIS_HierarchyGroup ;

        /// <summary>
        ///   Is group modified.
        /// </summary>
        FIsModified : Boolean ;

        /// <summary>
        ///   Manager handle.
        /// </summary>
        FManager    : TGIS_HierarchyManager ;

        /// <summary>
        ///   Thread guard.
        /// </summary>
        FThreadGuard : TGIS_ThreadClass ;

    protected

        /// <summary>
        ///   Name.
        /// </summary>
        FName        : String ;

        /// <summary>
        ///   Caption.
        /// </summary>
        FCaption        : String ;

        /// <summary>
        ///   Collapsed.
        /// </summary>
        FCollapsed     : Boolean ;

        /// <summary>
        ///   Active.
        /// </summary>
        FActive     : Boolean ;

    private

      procedure lockThread ;
      procedure unlockThread ;

    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Find a group by name.
      /// </summary>
      /// <param name="_name">
      ///   group name
      /// </param>
      /// <returns>
      ///   group
      /// </returns>
      function  findGroup        ( const _name : String
                                 ) : IGIS_HierarchyGroup ;

    private

      function  fget_GroupsCount : Integer ;
      function  fget_LayersCount : Integer ;
      function  fget_Group       ( const _indexOrName : OleVariant
                                 ) : IGIS_HierarchyGroup ;
      function  fget_Layer       ( const _index : Integer
                                 ) : TGIS_LayerAbstract ;
      function  fget_Collapsed   : Boolean ;
      procedure fset_Collapsed   ( const _value : Boolean
                                 ) ;
      function  fget_Active      : Boolean ;
      procedure fset_Active      ( const _value : Boolean
                                 ) ;
      function  fget_Caption     : String ;
      procedure fset_Caption     ( const _value : String
                                 ) ;
      function  fget_Name        : String ;
      function  fget_IsModified  : Boolean ;
      function  fget_ParentGroup : IGIS_HierarchyGroup ;
      procedure fset_ParentGroup ( const _group : IGIS_HierarchyGroup
                                 ) ;
      function  fget_Manager     : IGIS_HierarchyManager ;
      procedure fset_Manager     ( const _manager : IGIS_HierarchyManager
                                 ) ;
    public
      {$IFDEF OXYGENE}
        /// <summary>
        ///   Destructor.
        /// </summary>
        procedure Dispose      ; virtual;
      {$ELSE}

        /// <summary>
        ///   Destructor.
        /// </summary>
        destructor Destroy     ; override;
      {$ENDIF}

    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_name">
      ///   group name
      /// </param>
      constructor Create       ( const _name : String
                               ) ;

      /// <inheritdoc/>
      function  CreateGroup    ( const _name : String
                               ) : IGIS_HierarchyGroup ;

      /// <inheritdoc/>
      procedure ClearGroups ;

      /// <inheritdoc/>
      procedure DeleteGroup    ( const _name : String
                               ) ;

      /// <inheritdoc/>
      procedure RemoveGroup    ( const _group : IGIS_HierarchyGroup
                               ) ;

      /// <inheritdoc/>
      procedure AddGroup       ( const _group : IGIS_HierarchyGroup
                               ) ;

      /// <inheritdoc/>
      procedure InsertGroup    ( const _group : IGIS_HierarchyGroup ;
                                 const _index : Integer
                               ) ;

      /// <inheritdoc/>
      procedure AddLayer       ( const _layer : TGIS_LayerAbstract

                               ) ;

      /// <inheritdoc/>
      procedure InsertLayer    ( const _layer : TGIS_LayerAbstract ;
                                 const _index : Integer
                               ) ;

      /// <inheritdoc/>
      procedure DeleteLayer    ( const _layer : TGIS_LayerAbstract
                               ) ;

    public
      /// <inheritdoc/>
      property Name         : String  read  fget_Name ;

      /// <inheritdoc/>
      property Caption      : String  read  fget_Caption
                                      write fset_Caption ;

      /// <inheritdoc/>
      property Collapsed    : Boolean read  fget_Collapsed
                                      write fset_Collapsed ;

      /// <inheritdoc/>
      property Active       : Boolean read  fget_Active
                                      write fset_Active ;

      /// <inheritdoc/>
      property IsModified   : Boolean read  fget_IsModified ;

      /// <inheritdoc/>
      property Groups[ const _indexOrName : OleVariant ]  : IGIS_HierarchyGroup
                                                             read fget_Group ;

      /// <inheritdoc/>
      property Layers[ const _index : Integer ] : TGIS_LayerAbstract
                                                 read fget_Layer ;

      /// <inheritdoc/>
      property GroupsCount  : Integer read  fget_GroupsCount ;

      /// <inheritdoc/>
      property LayersCount  : Integer read  fget_LayersCount ;

      /// <inheritdoc/>
      property ParentGroup                      : IGIS_HierarchyGroup
                                                  read  fget_ParentGroup
                                                  write fset_ParentGroup ;
      /// <inheritdoc/>
      property Manager                          : IGIS_HierarchyManager
                                                  read  fget_Manager
                                                  write fset_Manager ;
  end ;

  /// <summary>
  ///   <para>
  ///     Hierarchy manager class.
  ///   </para>
  ///   <para>
  ///     The hierarchy data contain a hierarchy list and additional hierarchy
  ///     groups that are saved in a project file under [TatukGIS Hierarchy]
  ///     and [TatukGIS GroupN] sections.
  ///   </para>
  ///   <para>
  ///     The hierarchy list section has a format like:
  ///   </para>
  /// </summary>
  TGIS_HierarchyManager = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TInterfacedObject, IGIS_HierarchyManager
                                 {$IFDEF CLR}
                                  , IDisposable
                                 {$ENDIF} )

    private
      // Hierarchy groups list.
      FGroups      : TList<IGIS_HierarchyGroup> ;
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      FViewer      : IGIS_Viewer  ;
      FIsModified  : Boolean ;
      FThreadGuard : TGIS_ThreadClass ;
    private
      /// <summary>
      ///   Add hierarchy element.
      /// </summary>
      procedure addHierarchy    ( const _groups  : String ;
                                  const _layers  : String
                                ) ;
      /// <summary>
      ///   Touch manager after changes.
      /// </summary>
      /// <param name="_valid">
      ///   if persistent touch
      /// </param>
      procedure markModified    ( const _valid   : Boolean
                                ) ;

      procedure parseXMLHierarchy( const _node : IXMLNode
                                 ) ;
      procedure parseXMLGroup   ( const _node   : IXMLNode ;
                                  const _group  : IGIS_HierarchyGroup
                                ) ;

      /// <summary>
      ///   Get hierarchy in xml format.
      /// </summary>
      function  getHierarchyINI : String ;

      procedure lockThread ;
      procedure unlockThread ;
    private
      function  fget_GroupsCount: Integer ;
      function  fget_Group      ( const _indexOrName : OleVariant
                                ) : IGIS_HierarchyGroup ;

      procedure fset_IsModified ( const _val     : Boolean
                                ) ;
      function  fget_IsModified : Boolean ;
    protected
      /// <summary>
      ///   Find a group by name.
      /// </summary>
      /// <param name="_name">
      ///   group name
      /// </param>
      /// <returns>
      ///   group
      /// </returns>
      function  findGroup       ( const _name    : String
                                ) : IGIS_HierarchyGroup ;

      /// <summary>
      ///   Add layers to a group.
      /// </summary>
      /// <param name="_group">
      ///   existing group handle
      /// </param>
      /// <param name="_layers">
      ///   list of separated layers
      /// </param>
      procedure addGroupLayers  ( const _group   : IGIS_HierarchyGroup ;
                                  const _layers  : String
                                ) ;
    protected
      /// <summary>
      ///   Get the viewer attached.
      /// </summary>
      /// <returns>
      ///   Viewer
      /// </returns>
      function getViewer : IGIS_Viewer ;

    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer handle
      /// </param>
      constructor Create( const _viewer : IGIS_Viewer ) ;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   Destructor.
        /// </summary>
        procedure Dispose      ;
      {$ELSE}
        /// <summary>
        ///   Destructor.
        /// </summary>
        destructor Destroy     ; override;
      {$ENDIF}

      // group management

      /// <inheritdoc/>
      function  CreateGroup     ( const _name    : String
                                ) : IGIS_HierarchyGroup ;

      /// <inheritdoc/>
      procedure ClearGroups     ;

      /// <inheritdoc/>
      procedure DeleteGroup     ( const _name    : String
                                ) ;

      /// <inheritdoc/>
      procedure MoveGroup       ( const _source  : String ;
                                  const _target  : String
                                ) ;

      /// <inheritdoc/>
      procedure MoveGroupEx     ( const _source  : String ;
                                  const _target  : String ;
                                  const _index   : Integer
                                ) ;

      /// <inheritdoc/>
      procedure AddGroup        ( const _group   : IGIS_HierarchyGroup
                                ) ;

      /// <inheritdoc/>
      procedure InsertGroup     ( const _group   : IGIS_HierarchyGroup ;
                                  const _index   : Integer
                                 ) ;
      // layer management

      /// <inheritdoc/>
      procedure AddLayer        ( const _layer   : TGIS_LayerAbstract
                                ) ;

      /// <inheritdoc/>
      procedure InsertLayer     ( const _layer   : TGIS_LayerAbstract ;
                                  const _index   : Integer
                                 ) ;

      /// <inheritdoc/>
      procedure DeleteLayer     ( const _layer   : TGIS_LayerAbstract
                                ) ;

      /// <inheritdoc/>
      procedure MoveLayer       ( const _from    : String ;
                                  const _to      : String ;
                                  const _layer   : TGIS_LayerAbstract
                                ) ;

      /// <inheritdoc/>
      procedure AddOtherLayers  ;
      // hierarchy management

      /// <inheritdoc/>
      procedure ParseHierarchy  ( const _hierarchy : String
                                ) ; overload;

     /// <inheritdoc/>
     procedure ParseHierarchy( const _hierarchy : {$IFDEF OXYGENE}
                                                     TGIS_Strings
                                                   {$ELSE}
                                                     TStrings
                                                   {$ENDIF} ;
                                const _format : TGIS_ConfigFormat
                              ) ; overload;

      /// <inheritdoc/>
      procedure LoadHierarchy   ( const _config : TGIS_ConfigAbstract
                                ) ;

      /// <inheritdoc/>
      function  GetHierarchy    ( const _format : TGIS_ConfigFormat
                                ) : String ;

      /// <inheritdoc/>
      function  GetGroups       : String ;

    public

      /// <inheritdoc/>
      property Groups[ const _indexOrName : OleVariant ] : IGIS_HierarchyGroup
                                                read fget_Group ;

      /// <inheritdoc/>
      property GroupsCount      : Integer        read fget_GroupsCount ;

      /// <inheritdoc/>
      property IsModified       : Boolean        read  fget_IsModified
                                                 write fset_IsModified ;
  end ;

const
  /// <summary>
  ///   Hierarchy separator char.
  /// </summary>
  GIS_HIERARCHY_SEPARATOR = '\' ;

  /// <summary>
  ///   Group separator char.
  /// </summary>
  GIS_GROUP_SEPARATOR     = ';' ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    {$IFDEF LEVEL_XE3_RTL}
      System.Types,
    {$ENDIF}
    System.SysUtils,
    System.SyncObjs,
    System.Variants,
    GisInternals,
    GisResource ;
{$ENDIF}

//=============================================================================
// TGIS_HierarchyGroupEventArgs
//=============================================================================

{$IFDEF OXYGENE}
  constructor TGIS_HierarchyGroupEventArgs.Create(
    const _group : IGIS_HierarchyGroup
  ) ;
  begin
    inherited Create ;
    FGroup := _group ;
  end ;
{$ENDIF}

//=============================================================================
// TGIS_HierarchyManager
//=============================================================================

  constructor TGIS_HierarchyManager.Create(
    const _viewer : IGIS_Viewer
  ) ;
  begin
    inherited Create;

    FGroups      := TList<IGIS_HierarchyGroup>.Create ;
    FIsModified  := False ;
    FViewer      := _viewer ;
    FThreadGuard := TGIS_ThreadClass.Create ;
  end ;

  {$IFDEF OXYGENE}
    procedure TGIS_HierarchyManager.Dispose      ;
  {$ELSE}
    destructor TGIS_HierarchyManager.Destroy     ;
  {$ENDIF}
  begin
    {$IFNDEF CLR}
      ClearGroups ;
    {$ENDIF}

    FreeObject( FGroups      ) ;
    FreeObject( FThreadGuard ) ;

    {$IFNDEF OXYGENE}
      inherited ;
    {$ENDIF}
  end ;

  function TGIS_HierarchyManager.findGroup(
    const _name : String
  ) : IGIS_HierarchyGroup ;
  var
    i : Integer ;
  begin
    Result := nil ;

    for i := 0 to FGroups.Count - 1 do begin
      if TGIS_HierarchyGroup( FGroups[ i ] ).Name = _name then
        Result := TGIS_HierarchyGroup( FGroups[ i ] )
      else
        Result := TGIS_HierarchyGroup( FGroups[ i ] ).findGroup( _name ) ;

      if assigned( Result ) then break ;
    end ;
  end ;

  function TGIS_HierarchyManager.fget_Group(
    const _indexOrName : OleVariant
  ) : IGIS_HierarchyGroup ;
  var
    idx : Integer ;
  begin
    if IsVariantString( _indexOrName ) then begin
      Result := findGroup( VarToString( _indexOrName ) ) ;
      exit ;
    end ;

    idx := VarToInt32( _indexOrName ) ;
    if ( idx >= 0 ) and ( idx < FGroups.Count ) then
      Result := TGIS_HierarchyGroup( FGroups[ idx ] )
    else
      Result := nil ;
  end ;

  function TGIS_HierarchyManager.fget_GroupsCount
    : Integer ;
  begin
    Result := FGroups.Count ;
  end ;

  function TGIS_HierarchyManager.fget_IsModified: Boolean;
  begin
    Result := FIsModified ;
  end;

  function TGIS_HierarchyManager.getViewer : IGIS_Viewer ;
  begin
    Result := FViewer ;
  end;


  procedure TGIS_HierarchyManager.addGroupLayers(
    const _group  : IGIS_HierarchyGroup ;
    const _layers : String
  ) ;
  var
    tknex : TGIS_Tokenizer ;
    j     : Integer ;
    la    : TGIS_Layer ;
  begin
    tknex := TGIS_Tokenizer.Create ;
    try
      tknex.ExecuteEx( _layers, GIS_GROUP_SEPARATOR ) ;

      for j := 0 to tknex.Result.Count - 1 do begin
        if IsStringEmpty( tknex.Result[ j ] ) then continue ;

        if assigned( FViewer ) then begin
          la := TGIS_Layer( FViewer.Get( tknex.Result[ j ] ) ) ;
          if assigned( la ) then
            _group.AddLayer( la ) ;
        end ;
      end ;
    finally
      FreeObject( tknex ) ;
    end ;
  end ;

  procedure TGIS_HierarchyManager.markModified(
    const _valid : Boolean
  ) ;
  begin
    FIsModified := True ;
    if assigned( FViewer ) and _valid then
      FViewer.MarkModified ;
  end ;

  procedure TGIS_HierarchyManager.addHierarchy(
    const _groups : String ;
    const _layers : String
  ) ;
  var
    tkn    : TGIS_Tokenizer ;
    i      : Integer ;
    igroup : IGIS_HierarchyGroup ;
    lg     : TGIS_LayerGroup ;
    vwr    : TGIS_ViewerRef ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( _groups, GIS_HIERARCHY_SEPARATOR ) ;

      igroup := nil ;
      for i := 0 to tkn.Result.Count - 1 do begin
        if IsStringEmpty( tkn.Result[ i ] ) then continue ;

        if assigned( igroup ) then
          igroup := igroup.CreateGroup( tkn.Result[ i ] )
        else
          igroup := CreateGroup( tkn.Result[ i ] ) ;

        if assigned( FViewer ) and assigned( FViewer.ProjectFile ) then begin
          lg := TGIS_LayerGroup.Create ;
          try
            lg.Caption    := igroup.Caption ;
            lg.Name       := igroup.Name ;

            vwr := TGIS_ViewerRef.Create ;
            try
              vwr.Ref := FViewer ;
              lg.Viewer := vwr ;
              lg.ReadConfig ;
              igroup.Caption   := lg.Caption ;
              igroup.Collapsed := lg.Collapsed ;
              igroup.Active    := lg.Active ;
            finally
              FreeObject( vwr ) ;
            end ;
          finally
            FreeObject( lg ) ;
          end ;
        end ;

        if i = ( tkn.Result.Count - 1 ) then
          addGroupLayers( igroup, _layers ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_HierarchyManager.fset_IsModified(
    const _val : Boolean
  ) ;
  var
    igroup : TGIS_HierarchyGroup ;
    i     : Integer ;

    procedure walkTouched( const _group : TGIS_HierarchyGroup ) ;
    var
      j : Integer ;
    begin
      for j := 0 to _group.FGroups.Count - 1 do begin
        TGIS_HierarchyGroup( _group.FGroups[ j ] ).FIsModified := _val ;
        walkTouched( TGIS_HierarchyGroup( _group.FGroups[ j ] ) ) ;
      end ;
    end ;

  begin
    lockThread ;
    try
      FIsModified := _val ;

      if not _val then begin
        for i := 0 to FGroups.Count - 1 do begin
          igroup := TGIS_HierarchyGroup( FGroups[ i ] ) ;
          walkTouched( igroup ) ;
        end ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;


  procedure TGIS_HierarchyManager.parseXMLGroup(
    const _node   : IXMLNode ;
    const _group  : IGIS_HierarchyGroup
  ) ;
  var
    ngroup  : IXMLNode ;
    nlayer  : IXMLNode ;
    i, j    : Integer ;
    igroup  : IGIS_HierarchyGroup ;
    la      : TGIS_Layer ;
  begin
    if not assigned( _node ) then exit ;

    if assigned( _group ) then
      igroup := _group.CreateGroup( VarToString( _node.Attributes[ GIS_INI_NAME ] ) )
    else
      igroup := CreateGroup( VarToString( _node.Attributes[ GIS_INI_NAME ] ) ) ;

    igroup.Caption   := VarToString ( _node.Attributes[ GIS_INI_CAPTION   ] ) ;
    igroup.Collapsed := VarToBoolean( _node.Attributes[ GIS_INI_COLLAPSED ] ) ;
    igroup.Active    := VarToBoolean( _node.Attributes[ GIS_INI_ACTIVE    ] ) ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      ngroup := _node.ChildNodes[i] ;
      if ngroup.NodeName = GIS_XML_GROUP_TAG then
        parseXMLGroup( ngroup, igroup )
      else if ngroup.NodeName = GIS_XML_LAYERS_TAG then
        for j := 0 to ngroup.ChildNodes.Count-1 do begin
          nlayer := ngroup.ChildNodes[j] ;
          if assigned( FViewer ) then begin
            la := TGIS_Layer( FViewer.Get( VarToString( nlayer.Attributes[ GIS_INI_NAME ] ) ) ) ;
            if assigned( la ) then
              igroup.AddLayer( la ) ;
          end ;
        end ;
    end ;
  end;


  procedure TGIS_HierarchyManager.parseXMLHierarchy(
    const _node : IXMLNode
  ) ;
  var
    ngroups : IXMLNode ;
    ngroup  : IXMLNode ;
    i       : Integer ;
  begin
    if not assigned( _node ) then exit ;

    ngroups := _node.ChildNodes.FindNode( GIS_XML_GROUPS_TAG ) ;

    if assigned( ngroups ) then begin
      for i := 0 to ngroups.ChildNodes.Count-1 do begin
        ngroup := ngroups.ChildNodes[i] ;

        parseXMLGroup( ngroup, nil ) ;
      end ;
    end ;
  end ;

  procedure TGIS_HierarchyManager.unlockThread;
  begin
    FThreadGuard.UnlockThread ;
  end;

  procedure TGIS_HierarchyManager.AddLayer(
    const _layer : TGIS_LayerAbstract
  ) ;
  var
    i : Integer ;
  begin
    if not assigned( _layer ) then exit ;

    for i := 0 to FGroups.Count - 1 do
      TGIS_HierarchyGroup( FGroups[ i ] ).AddLayer( _layer ) ;

    markModified( TGIS_Layer( _layer ).IsPersistent ) ;
  end ;

  procedure TGIS_HierarchyManager.InsertLayer(
    const _layer   : TGIS_LayerAbstract ;
    const _index   : Integer
  ) ;
  var
    i : Integer ;
  begin
    if not assigned( _layer ) then exit ;

    for i := 0 to FGroups.Count - 1 do
      TGIS_HierarchyGroup( FGroups[ i ] ).InsertLayer( _layer, _index ) ;

    markModified( TGIS_Layer( _layer ).IsPersistent ) ;
  end ;

  procedure TGIS_HierarchyManager.AddOtherLayers ;
  var
    other : IGIS_HierarchyGroup ;
    i     : Integer ;

    function findGroupLayer(
      const _group : IGIS_HierarchyGroup ;
      const _layer : TObject
    ) : Boolean ;
    var
      k : Integer ;
    begin
      Result := False ;

      for k := 0 to _group.LayersCount - 1 do
        if _group.Layers[ k ] = _layer then begin
          Result := True ;
          break ;
        end ;

      if not Result then
        for k := 0 to _group.GroupsCount - 1 do begin
          Result := findGroupLayer( _group.Groups[ k ], _layer ) ;
          if Result then break ;
        end ;
    end ;

    function findLayer(
      const _layer : TObject
    ) : Boolean ;
    var
      j : Integer ;
    begin
      Result := False ;

      for j := 0 to GroupsCount - 1 do
        if findGroupLayer( Groups[ j ], FViewer.Items[ i ] ) then
        begin
          Result := True ;
          break ;
        end ;
    end ;

  begin
    other := nil ;
    if assigned( FViewer ) then
      for i := 0 to FViewer.Items.Count - 1 do begin
        if not findLayer( FViewer.Items[ i ] ) and
           TGIS_Layer( FViewer.Items[ i ] ).IsPersistent then
        begin
          if not assigned( other ) then
            other := CreateGroup( _rsrc( GIS_RS_LEGEND_OTHER ) ) ;
          other.AddLayer( TGIS_LayerAbstract( FViewer.Items[ i ] ) ) ;
        end ;
      end ;
  end ;

  procedure TGIS_HierarchyManager.ClearGroups;
  begin
    lockThread ;
    try
      FGroups.Clear ;
      markModified( True ) ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_HierarchyManager.CreateGroup(
    const _name : String
  ) : IGIS_HierarchyGroup ;
  begin
    lockThread ;
    try
      Result := findGroup( _name ) ;

      if not assigned( Result ) then begin
        Result := TGIS_HierarchyGroup.Create( _name ) ;
        FGroups.Add( Result ) ;
        TGIS_HierarchyGroup(Result).Manager := Self ;

        markModified( True ) ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_HierarchyManager.AddGroup(
    const _group : IGIS_HierarchyGroup
  ) ;
  begin
    _group.ParentGroup := nil ;
    FGroups.Add( _group ) ;
    _group.Manager := Self   ;
    markModified( True ) ;
  end ;

  procedure TGIS_HierarchyManager.InsertGroup(
    const _group   : IGIS_HierarchyGroup ;
    const _index   : Integer
  ) ;
  begin
    if _group = nil then exit ;

    if findGroup( _group.Name ) <> nil then exit ;

    _group.ParentGroup := nil ;

    if ( _index < 0 ) or ( _index > FGroups.Count ) then
      AddGroup( _group )
    else begin
      FGroups.Insert( _index, _group ) ;
      _group.Manager := Self ;
    end ;

    markModified( True ) ;
  end ;

  procedure TGIS_HierarchyManager.DeleteGroup(
    const _name : String
  ) ;
  var
    i      : Integer ;
    igroup : TGIS_HierarchyGroup ;
  begin
    lockThread ;
    try
      for i := 0 to FGroups.Count - 1 do begin
        igroup := TGIS_HierarchyGroup( FGroups[ i ] ) ;
        if igroup.Name = _name then begin
          {$IFDEF OXYGENE}
            FGroups.RemoveAt( i ) ;
          {$ELSE}
            FGroups.Delete( i ) ;
          {$ENDIF}
          markModified( True ) ;
          break ;
        end
        else begin
          igroup.DeleteGroup( _name ) ;
          markModified( True ) ;
        end ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_HierarchyManager.DeleteLayer(
    const _layer : TGIS_LayerAbstract
  ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      for i := 0 to FGroups.Count - 1 do
        TGIS_HierarchyGroup( FGroups[ i ] ).DeleteLayer( _layer ) ;
      markModified( TGIS_Layer( _layer ).IsPersistent ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_HierarchyManager.MoveGroup(
    const _source : String ;
    const _target : String
  ) ;
  begin
    MoveGroupEx( _source, _target, -1 ) ;
  end ;

  procedure TGIS_HierarchyManager.MoveGroupEx(
    const _source : String ;
    const _target : String ;
    const _index  : Integer
  ) ;
  var
    sgroup  : IGIS_HierarchyGroup ;
    tgroup  : IGIS_HierarchyGroup ;
  begin
    lockThread ;
    try
      sgroup := findGroup( _source ) ;
      tgroup := findGroup( _target ) ;

      if assigned( sgroup ) and assigned( tgroup ) then begin
        if assigned( sgroup.ParentGroup ) then
          sgroup.ParentGroup.RemoveGroup( sgroup )
        else
          FGroups.Remove( sgroup ) ;

        if _index = -1 then
          tgroup.AddGroup( sgroup )
        else
          tgroup.InsertGroup( sgroup, _index ) ;

        markModified( True )
      end
      else if assigned( sgroup ) and not assigned( tgroup ) then begin
        if assigned( sgroup.ParentGroup ) then
          sgroup.ParentGroup.RemoveGroup( sgroup )
        else
          FGroups.Remove( sgroup ) ;

        if _index = -1 then
          AddGroup( sgroup )
        else
          InsertGroup( sgroup, _index ) ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_HierarchyManager.MoveLayer(
    const _from  : String ;
    const _to    : String ;
    const _layer : TGIS_LayerAbstract
  ) ;
  var
    fgroup : IGIS_HierarchyGroup ;
    tgroup : IGIS_HierarchyGroup ;
  begin
    if not assigned( _layer ) then exit ;

    lockThread ;
    try
      fgroup := findGroup( _from ) ;
      tgroup := findGroup( _to   ) ;

      if assigned( fgroup ) and assigned( tgroup ) then begin
        fgroup.DeleteLayer( _layer ) ;
        tgroup.AddLayer( _layer ) ;
        markModified( TGIS_Layer( _layer ).IsPersistent ) ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  { Custom sort by numbers.
  }
  {$IFDEF OXYGENE}
    function CompareIndexes(
      _item1 : Object ;
      _item2 : Object
   ) : Integer ;
  {$ELSE}
    function CompareIndexes(
      _list    : TGIS_StringList ;
      _index1  : Integer ;
      _index2  : Integer
    ) : Integer ;
  {$ENDIF}
  var
    d1, d2 : Integer ;
    r1, r2 : Boolean ;

    function getInt( const _str     : String ;
                       var _intVal  : Integer
                    ) : Boolean ;
    var
      c, l, r, max : Integer ;
      {$IFDEF JAVA}
      v : Variant ;
      {$ENDIF}
      {$IFDEF ISLAND}
      v : Variant ;
      {$ENDIF}
    begin
      if IsStringEmpty( _str ) then begin
        Result := False ;
        _intVal := 0 ;
        exit ;
      end ;
      l := StringFirst ;
      max := StringLast(_str);
      while (l<=max) and not InCharSet( _str[l], ['09'] ) do
        inc( l ) ;

      r := 0 ;
      while (l+r<=max) and InCharSet( _str[l+r], ['09'] ) do
        inc( r ) ;

      {$IFDEF ISLAND}
          v := _intVal ;
          Val( Copy( _str, l, r ), v, c ) ;
          _intVal := VarToInt32(v) ;
      {$ELSE}
        {$IFDEF JAVA}
          v := _intVal ;
          Val( Copy( _str, l, r ), v, c ) ;
          _intVal := VarToInt32(v) ;
        {$ELSE}
          Val( Copy( _str, l, r ), _intVal, c ) ;
        {$ENDIF}
      {$ENDIF}
      Result := (c = 0) ;
    end ;

  begin
    {$IFDEF OXYGENE}
      r1 := getInt( T_stringsItem( _item1 ).FString, d1 ) ;
      r2 := getInt( T_stringsItem( _item2 ).FString, d2 ) ;
    {$ELSE}
      r1 := getInt( _list[_index1], d1 ) ;
      r2 := getInt( _list[_index2], d2 ) ;
    {$ENDIF}

    Result := ord(r1 or r2) ;
    if Result <> 0 then begin
      if d1 < d2 then
        Result := -1
      else if d1 > d2 then
        Result := 1
      else
       Result := 0 ;
    end
    else
      {$IFDEF OXYGENE}
        Result := CompareText( T_stringsItem( _item1 ).FString, T_stringsItem( _item2 ).FString ) ;
      {$ELSE}
        Result := CompareText( _list[_index1], _list[_index2] ) ;
      {$ENDIF}
  end ;

  procedure TGIS_HierarchyManager.LoadHierarchy(
    const _config : TGIS_ConfigAbstract
  ) ;
  var
    hData : TGIS_StringList ;
    cfg   : TGIS_Config     ;
  begin
    if not assigned( _config ) then exit ;

    cfg := TGIS_Config( _config ) ;
    if cfg.ConfigFormat = TGIS_ConfigFormat.Ini then begin
      try
        hData := cfg.ReadHierarchyGroups as TGIS_StringList;
        {$IFDEF OXYGENE}
          hData.Sort( @CompareIndexes ) ;
        {$ELSE}
          hData.CustomSort( CompareIndexes ) ;
        {$ENDIF}
        ParseHierarchy( hData, TGIS_ConfigFormat.Ini ) ;
      finally
        FreeObject( hData ) ;
      end ;
    end
    else if cfg.ConfigFormat = TGIS_ConfigFormat.Xml then begin
      parseXMLHierarchy( cfg.ReadHierarchyGroups as IXMLNode ) ;
    end;
  end ;

  procedure TGIS_HierarchyManager.lockThread;
  begin
    FThreadGuard.LockThread ;
  end;

  procedure TGIS_HierarchyManager.ParseHierarchy(
    const _hierarchy : String
  ) ;
  var
    lst : TStrings ;
  begin
    lst := TGIS_StringList.Create ;
    try
      lst.Text := _hierarchy ;
      ParseHierarchy( lst, TGIS_ConfigFormat.Ini ) ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_HierarchyManager.ParseHierarchy(
    const _hierarchy : {$IFDEF OXYGENE}
                         TGIS_Strings
                       {$ELSE}
                         TStrings
                       {$ENDIF} ;
    const _format : TGIS_ConfigFormat
  ) ;
  var
    i,p     : Integer ;
    sgroups,
    slayers : String ;
    str     : String ;
    substr  : String ;
    xdoc    : TGIS_XMLDocument ;

      function splitHierarchy(
        const _data : String
      ) : Boolean ;
      var
        j : Integer ;
      begin
        j := Pos( '=', _data );
        if j >= StringFirst then begin
          sgroups := TrimRight( Copy( _data, StringFirst, j-StringFirst ) ) ;
          slayers := TrimLeft ( Copy( _data, j+1,         MaxInt        ) ) ;
        end ;
        Result := j >= StringFirst ;
      end ;

  begin
    if _format = TGIS_ConfigFormat.Ini then begin
      lockThread ;
      try
        for i := 0 to _hierarchy.Count - 1 do begin
          // new version
          substr := GIS_INI_HIERARCHY_INDEX + IntToStr(i+1) ;
          p := Pos( UpperCase( substr ), UpperCase( _hierarchy[ i ] ) ) ;
          if p >= StringFirst then
            str := Copy( _hierarchy[ i ], StringLast( substr )+2, MaxInt )
          else
            str := _hierarchy[ i ] ;

          if splitHierarchy( str ) then
            addHierarchy( sgroups, slayers ) ;
        end ;
      finally
        unlockThread ;
      end ;
    end
    else if _format = TGIS_ConfigFormat.Xml then begin
      xdoc := TGIS_XMLDocument.Create ;
      try
        xdoc.LoadFromXML( _hierarchy.Text ) ;
        parseXMLHierarchy( xdoc.DocumentElement ) ;
      finally
        FreeObject( xdoc ) ;
      end ;
    end ;
  end ;

  function TGIS_HierarchyManager.GetHierarchy(
    const _format : TGIS_ConfigFormat
  ) : String ;
  begin
    if _format = TGIS_ConfigFormat.Ini then
      Result := getHierarchyINI
    else
      Result := '' ;
  end ;

  function TGIS_HierarchyManager.getHierarchyINI : String ;
  var
    i         : Integer ;
    idx       : Integer ;
    igroup    : TGIS_HierarchyGroup ;
    hierarchy : String ;
    added     : Boolean ;
    res       : String ;

    function getLayers(
      const _group : TGIS_HierarchyGroup
    ) : String ;
    var
      k : Integer ;
    begin
      Result := '' ;

      for k := 0 to _group.FLayers.Count - 1 do begin
        Result := Result + TGIS_Layer( _group.FLayers[ k ] ).Name  ;
        if k < _group.FLayers.Count - 1 then
          Result := Result + GIS_GROUP_SEPARATOR ;
      end ;

    end ;

    procedure walk(
      const _group    : TGIS_HierarchyGroup ;
      var _index      : Integer ;
      var _hierarchy  : String ;
      var _added      : Boolean
    ) ;
    var
      j       : Integer ;
      groupex : TGIS_HierarchyGroup ;
      tmp     : String ;
    begin
      if not _added and ( _group.FLayers.Count > 0 ) then begin
        _added := True ;
        res := res +
               Format( '%s=%s=%s', [ GIS_INI_HIERARCHY_INDEX + IntToStr(_index),
                                     _hierarchy, getLayers( _group ) ]
                      ) + #13#10 ;
        inc( idx ) ;
      end ;

      for j := 0 to _group.FGroups.Count - 1 do begin
        _added := False ;
        groupex := TGIS_HierarchyGroup( _group.FGroups[ j ] ) ;

        tmp  := _hierarchy + GIS_HIERARCHY_SEPARATOR + groupex.Name ;

        walk( groupex, _index, tmp, _added ) ;
        if not _added then begin
          _added := True ;
          res := res +
                 Format( '%s=%s=%s', [ GIS_INI_HIERARCHY_INDEX + IntToStr(_index),
                                       tmp, getLayers( groupex ) ] ) + #13#10 ;
          inc( idx ) ;
        end ;
      end ;
    end ;

  begin
      lockThread ;
      try
        res := '' ;
        idx := 1 ;
        for i := 0 to FGroups.Count - 1 do begin
          added := False ;
          igroup := TGIS_HierarchyGroup( FGroups[ i ] ) ;
          hierarchy := igroup.Name ;
          walk( igroup, idx, hierarchy, added ) ;

          if not added then begin
            added := True ;
            res := res +
                   Format( '%s=%s=%s', [ GIS_INI_HIERARCHY_INDEX + IntToStr(idx),
                                         hierarchy, getLayers( igroup )
                                        ]
                          ) + #13#10 ;
            inc( idx ) ;
          end ;
        end ;
        Result := res ;
      finally
        unlockThread ;
      end ;
  end ;


  function TGIS_HierarchyManager.GetGroups : String ;
  var
    igroup : TGIS_HierarchyGroup ;
    i, p   : Integer ;
    sb     : TStringBuilder ;

    procedure walklg( const _group : TGIS_HierarchyGroup ) ;
    var
      j   : Integer ;
    begin
      sb.Append( Format( '[%s%d]', [ GIS_INI_GROUP_HEADER, p ] ) + #13#10 ) ;
      sb.Append( Format( '%s=%s' , [ GIS_INI_NAME, _group.Name ] ) + #13#10 ) ;
      inc( p ) ;

      for j := 0 to _group.FGroups.Count - 1 do
        walklg( TGIS_HierarchyGroup( _group.FGroups[ j ] ) ) ;
    end ;

  begin
    lockThread ;
    try
      sb := TStringBuilder.Create ;
      try
        p := 1 ;
        for i := 0 to FGroups.Count - 1 do begin
          igroup := TGIS_HierarchyGroup( FGroups[ i ] ) ;
          walklg( igroup ) ;
        end ;
        Result := sb.ToString ;
      finally
        FreeObject( sb ) ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

//=============================================================================
// TGIS_HierarchyGroup
//=============================================================================

  constructor TGIS_HierarchyGroup.Create(
    const _name : String
  ) ;
  begin
    inherited Create ;

    FName        := _name ;
    FCaption     := _name ;
    FActive      := True ;
    FCollapsed   := False ;

    FGroups      := TList<IGIS_HierarchyGroup>.Create ;
    FLayers      := TGIS_LayerAbstractList.Create( False ) ;
    FManager     := nil ;
    FThreadGuard := TGIS_ThreadClass.Create ;
  end ;

  {$IFDEF OXYGENE}
    {@@ Destructor.
    }
    procedure TGIS_HierarchyGroup.Dispose ;
  {$ELSE}

    destructor TGIS_HierarchyGroup.Destroy ;
  {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      ClearGroups ;
    {$ENDIF}

    FreeObject( FGroups      ) ;
    FreeObject( FLayers      ) ;
    FreeObject( FThreadGuard ) ;

    {$IFNDEF OXYGENE}
      inherited;
    {$ENDIF}
  end ;

  function TGIS_HierarchyGroup.findGroup(
    const _name : String
  ) : IGIS_HierarchyGroup ;
  var
    i : Integer ;
  begin
    Result := nil ;

    for i := 0 to FGroups.Count - 1 do begin
      if TGIS_HierarchyGroup( FGroups[ i ] ).FName = _name then
        Result := TGIS_HierarchyGroup( FGroups[ i ] )
      else
        Result := TGIS_HierarchyGroup( FGroups[ i ] ).findGroup( _name ) ;

      if assigned( Result ) then break ;
    end ;
  end ;

  function TGIS_HierarchyGroup.fget_Collapsed : Boolean ;
  begin
    Result := FCollapsed ;
  end;

  function TGIS_HierarchyGroup.fget_Active : Boolean ;
  begin
    Result := FActive ;
  end;

  function TGIS_HierarchyGroup.fget_Caption : String ;
  begin
    Result := FCaption ;
  end;

  function TGIS_HierarchyGroup.fget_Group(
    const _indexOrName : OleVariant
  ) : IGIS_HierarchyGroup ;
  var
    idx : Integer ;
  begin
    if IsVariantString( _indexOrName ) then begin
      Result := findGroup( VarToString( _indexOrName ) ) ;
      exit ;
    end ;

    idx := VarToInt32( _indexOrName ) ;
    if ( idx >= 0 ) and ( idx < FGroups.Count ) then
      Result := TGIS_HierarchyGroup( FGroups[ idx ] )
    else
      Result := nil ;
  end ;

  function TGIS_HierarchyGroup.fget_GroupsCount
    : Integer;
  begin
    Result := FGroups.Count ;
  end ;

  function TGIS_HierarchyGroup.fget_IsModified : Boolean ;
  begin
    Result := FIsModified ;
  end;

  function TGIS_HierarchyGroup.fget_Layer(
    const _index : Integer
  ) : TGIS_LayerAbstract ;
  begin
    if ( _index >= 0 ) and ( _index < FLayers.Count ) then
      Result := TGIS_LayerAbstract( FLayers[ _index ] )
    else
      Result := nil ;
  end ;

  function TGIS_HierarchyGroup.fget_LayersCount
    : Integer ;
  begin
    Result := FLayers.Count ;
  end ;

  function TGIS_HierarchyGroup.fget_Manager: IGIS_HierarchyManager;
  begin
    Result := FManager ;
  end;

  function TGIS_HierarchyGroup.fget_Name: String;
  begin
    Result := FName ;
  end;

  function TGIS_HierarchyGroup.fget_ParentGroup : IGIS_HierarchyGroup;
  begin
    Result := FParentGroup ;
  end;

  procedure TGIS_HierarchyGroup.fset_Collapsed(
    const _value : Boolean
  ) ;
  begin
    if FCollapsed = _value then exit ;

    FCollapsed := _value ;
    FIsModified := True ;
  end ;

  procedure TGIS_HierarchyGroup.fset_Active(
    const _value : Boolean
  ) ;
  begin
    if FActive = _value then exit ;

    FActive := _value ;
    FIsModified := True ;
  end ;

  procedure TGIS_HierarchyGroup.fset_Caption(
    const _value : String
  ) ;
  begin
    if FCaption = _value then exit ;

    FCaption := _value ;
    FIsModified := True ;
  end ;

  procedure TGIS_HierarchyGroup.fset_Manager(
    const _manager : IGIS_HierarchyManager
  ) ;
  begin
    FManager := _manager as TGIS_HierarchyManager ;
  end;

  procedure TGIS_HierarchyGroup.fset_ParentGroup(
    const _group : IGIS_HierarchyGroup
  ) ;
  begin
    FParentGroup := _group as TGIS_HierarchyGroup;
  end;

  procedure TGIS_HierarchyGroup.AddGroup(
    const _group : IGIS_HierarchyGroup
  ) ;
  begin
    if _group = nil then exit ;

    _group.ParentGroup := self ;
    FGroups.Add( _group ) ;
    _group.Manager := FManager ;
    FIsModified := True ;
  end ;

  procedure TGIS_HierarchyGroup.InsertGroup(
    const _group : IGIS_HierarchyGroup ;
    const _index : Integer
  ) ;
  begin
    if _group = nil then exit ;

    if findGroup( _group.Name ) <> nil then exit ;

    _group.ParentGroup := self ;

    if ( _index < 0 ) or ( _index > FGroups.Count ) then
      AddGroup( _group )
    else begin
      FGroups.Insert( _index, _group ) ;
      _group.Manager := FManager ;
    end ;

    FIsModified := True ;
  end ;

  procedure TGIS_HierarchyGroup.AddLayer(
    const _layer : TGIS_LayerAbstract
  ) ;
  begin
    if not assigned( _layer ) then exit ;

    FLayers.Add( _layer ) ;
    FIsModified := True ;
  end ;

  procedure TGIS_HierarchyGroup.InsertLayer(
    const _layer : TGIS_LayerAbstract ;
    const _index : Integer
  ) ;
  begin
    if not assigned( _layer ) then exit ;

    if ( _index < 0 ) or ( _index > FLayers.Count ) then
      AddLayer( _layer )
    else
      FLayers.Insert( _index, _layer ) ;

    FIsModified := True ;
  end ;

  procedure TGIS_HierarchyGroup.lockThread;
  begin
    FThreadGuard.LockThread ;
  end;

  procedure TGIS_HierarchyGroup.ClearGroups ;
  begin
    lockThread ;
    try
      FGroups.Clear ;
      FIsModified := True ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_HierarchyGroup.DeleteGroup(
    const _name : String
  ) ;
  var
    i      : Integer ;
    igroup : TGIS_HierarchyGroup ;
  begin
    lockThread ;
    try
      for i := 0 to FGroups.Count - 1 do begin
        igroup := TGIS_HierarchyGroup( FGroups[ i ] ) ;
        if igroup.Name = _name then begin
          {$IFDEF OXYGENE}
            FGroups.RemoveAt( i ) ;
          {$ELSE}
            FGroups.Delete( i ) ;
          {$ENDIF}

          FIsModified := True ;
          break ;
        end
        else
          igroup.DeleteGroup( _name ) ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_HierarchyGroup.RemoveGroup(
    const _group : IGIS_HierarchyGroup
  ) ;
  begin
    lockThread ;
    try
      FGroups.Remove( _group ) ;
      FIsModified := True ;
    finally
      unlockThread ;
    end ;
  end ;


  procedure TGIS_HierarchyGroup.unlockThread;
  begin
    FThreadGuard.UnlockThread ;
  end;

  procedure TGIS_HierarchyGroup.DeleteLayer(
    const _layer : TGIS_LayerAbstract
  ) ;
  var
    i : Integer ;
  begin
    if _layer = nil then exit ;

    lockThread ;
    try
      for i := FLayers.Count - 1 downto 0 do
        if TGIS_Layer( FLayers[ i ] ) = _layer then begin
          FLayers.Delete( i ) ;
          FIsModified := True ;
        end ;

      // traverse all sub groups for a layer
      for i := 0 to FGroups.Count - 1 do
        TGIS_HierarchyGroup( FGroups[ i ] ).DeleteLayer( _layer ) ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_HierarchyGroup.CreateGroup(
    const _name : String
  ) : IGIS_HierarchyGroup ;
  begin
    lockThread ;
    try
      Result := findGroup( _name ) ;

      if not assigned( Result ) then begin
        Result := TGIS_HierarchyGroup.Create( _name ) ;
        FGroups.Add( Result ) ;
        Result.Manager := FManager ;
        Result.ParentGroup := self ;

        FIsModified := True ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

{==================================== END =====================================}
end.

