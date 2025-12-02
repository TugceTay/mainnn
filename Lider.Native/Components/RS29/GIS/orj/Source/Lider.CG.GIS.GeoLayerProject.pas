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

{$IFDEF DCC}
  unit GisLayerProject ;
  {$HPPEMIT '#pragma link "GisLayerProject"'}
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
    System.SysUtils,
    System.Classes,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisInterfaces,
    GisTypes,
    GisConfig,
    GisLayer,
    GisLayerVector;
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
  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerProject = class
    public
      // Perform initialization section.
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   General project layer class. Used to represent a project file as a
  ///   single layer on the legend.
  /// </summary>
  TGIS_LayerProject = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_Layer )
    private

      /// <summary>
      ///   List of attached layers.
      /// </summary>
      FItems        : TGIS_LayerAbstractList ;

      /// <summary>
      ///   Hierarchy list.
      /// </summary>
      FHierarchy    : TStrings ;

      /// <summary>
      ///   Project file itself as opened in Open procedure.
      /// </summary>
      FProjectFile  : TGIS_Config ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Open the layer.
      /// </summary>
      /// <remarks>
      ///   Will open that layer, read all data. After this all not drawing
      ///   operations on layer will be available. But you can add such layer
      ///   later to the viewer anyway.
      /// </remarks>
      procedure openInternal ;

      /// <summary>
      ///   Add internal layer hierarchy to the viewer hierarchy.
      /// </summary>
      procedure addHierarchy ;

      /// <summary>
      ///   Verify parent layers to avoid circular references.
      /// </summary>
      /// <param name="_path">
      ///   project layer path
      /// </param>
      /// <returns>
      ///   False if subproject doesn't have circular references
      /// </returns>
      function  verifyParent( const _path : String
                              ) : Boolean ;

      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp ; override;

    public // various public routines
      /// <inheritdoc/>
      function  DormantGain       : Integer ; override;
      /// <inheritdoc/>
      procedure Dormant           ; override;

    protected

      procedure doDestroy ; override;

      function  fget_FileCopyrights  : String ; override;

    public // constructors

      /// <inheritdoc/>
      constructor Create ; override;

    public

      /// <inheritdoc/>
      function  DrawEx            ( const _extent : TGIS_Extent
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      procedure DrawFlash         ; override;

      /// <inheritdoc/>
      procedure RecalcExtent      ; override;

      /// <inheritdoc/>
      procedure RecalcProjectedExtent ; override;

      /// <inheritdoc/>
      procedure ReadConfig        ; override;

      /// <inheritdoc/>
      procedure RereadConfig      ; override;

    public

      /// <summary>
      ///   <para>
      ///     Add sub layer to list.
      ///   </para>
      ///   <para>
      ///     Add a layer to the Viewer.
      ///   </para>
      /// </summary>
      /// <param name="_layer">
      ///   layer to be added
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYEREXIST
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERBADEXTENT
      /// </exception>
      /// <remarks>
      ///   Use this method to add a new custom layer to the viewer. For some
      ///   layers (on-disk) it's recommended to assign a layer extent
      ///   earlier to avoid its recalculation.
      /// </remarks>
      procedure Add              ( const _layer : TGIS_Layer
                                 ) ;

      /// <summary>
      ///   <para>
      ///     Delete sub layer from list.
      ///   </para>
      ///   <para>
      ///     Delete the layer identified by a name.
      ///   </para>
      /// </summary>
      /// <param name="_name">
      ///   name of layer to be deleted
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERNOEXIST
      /// </exception>
      /// <remarks>
      ///   See Get method for example.
      /// </remarks>
      procedure Delete           ( const _name : String
                                  ) ;

      /// <summary>
      ///   <para>
      ///     Get sub layer from list by name.
      ///   </para>
      ///   <para>
      ///     Retrieve the layer identified by a name.
      ///   </para>
      /// </summary>
      /// <param name="_name">
      ///   name of layer found
      /// </param>
      /// <returns>
      ///   found layer or nil
      /// </returns>
      function  Get              ( const _name : String
                                 ) : TGIS_Layer ; overload;

      /// <summary>
      ///   <para>
      ///     Get sub layer from list by handle.
      ///   </para>
      ///   <para>
      ///     Retrieve the layer identified by a name.
      ///   </para>
      /// </summary>
      /// <param name="_layer">
      ///   name of layer found
      /// </param>
      /// <returns>
      ///   found layer or nil
      /// </returns>
      function  Get              ( const _layer : TGIS_Layer
                                 ) : TGIS_Layer ; overload;

      /// <summary>
      ///   Locate a shape (more precise).
      /// </summary>
      /// <param name="_ptg">
      ///   reference point /searching point/
      /// </param>
      /// <param name="_prec">
      ///   precision /not a longer distance than/; point inside a polygon
      ///   is always not greater than _dist; if _prec is less then 0, then
      ///   being outside/inside polygon means the same
      /// </param>
      /// <param name="_uid">
      ///   Uid of reference shape or -1; thanks to this, is possible to
      ///   find nearest shape, excluding shape given by _uid.
      /// </param>
      /// <param name="_dist">
      ///   reached distance between _pt and shape in Result; for points
      ///   inside the polygon (if _prec &gt;= 0) the distance will be
      ///   multiply by 0.95 (to prefer points inside the polygon) but will
      ///   not be bigger then _prec; for point distance will be multiplied
      ///   by 0.9 to prefer points over lines and polygons
      /// </param>
      /// <param name="_part">
      ///   number of the part closest to a given point
      /// </param>
      /// <param name="_proj">
      ///   point projected to the nearest element of a shape
      /// </param>
      /// <returns>
      ///   found shape or nil
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Use this method to locate a shape that is near _ptg but also
      ///     on _prec distance. It finds a shape using FindFirst method in
      ///     extent corrected by _prec and _ptg params.
      ///   </para>
      ///   <para>
      ///     When using this operation, never call any function which uses
      ///     FindFirst..FindNext (any function which iterates through the
      ///     shapes using it - so never use inside an operation like
      ///     Locate etc.).
      ///   </para>
      ///   <para>
      ///     See Locate for example.
      ///   </para>
      /// </remarks>
      function  LocateEx      ( const _ptg     : TGIS_Point ;
                                const _prec    : Double     ;
                                const _uid     : Integer    ;
                                var   _dist    : Double     ;
                                var   _part    : Integer    ;
                                var   _proj    : TGIS_Point
                              ) : TGIS_Shape ; overload; virtual;

      /// <summary>
      ///   Locate a shape (more precise).
      /// </summary>
      /// <param name="_ptg">
      ///   reference point /searching point/
      /// </param>
      /// <param name="_prec">
      ///   precision /not a longer distance than/; point inside a polygon is
      ///   always not greater than _dist; if _prec is less then 0, then
      ///   being outside/inside polygon means the same
      /// </param>
      /// <param name="_uid">
      ///   Uid of reference shape or -1; thanks to this, is possible to find
      ///   nearest shape, excluding shape given by _uid.
      /// </param>
      /// <param name="_dist">
      ///   reached distance between _pt and shape in Result; for points
      ///   inside the polygon (if _prec &gt;= 0) the distance will be
      ///   multiply by 0.95 (to prefer points inside the polygon) but will
      ///   not be bigger then _prec; for point distance will be multiplied
      ///   by 0.9 to prefer points over lines and polygons
      /// </param>
      /// <param name="_part">
      ///   number of the part closest to a given point
      /// </param>
      /// <param name="_proj">
      ///   point projected to the nearest element of a shape
      /// </param>
      /// <param name="_visible">
      ///   if true the only visible shapes will be evaluated; shapes turned
      ///   of by query of hidden will be ignored
      /// </param>
      /// <returns>
      ///   found shape or nil
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Use this method to locate a shape that is near _ptg but also on
      ///     _prec distance. It finds a shape using FindFirst method in
      ///     extent corrected by _prec and _ptg params.
      ///   </para>
      ///   <para>
      ///     When using this operation, never call any function which uses
      ///     FindFirst..FindNext (any function which iterates through the
      ///     shapes using it - so never use inside an operation like Locate etc.).
      ///   </para>
      ///   <para>
      ///     See Locate for example.
      ///   </para>
      /// </remarks>
      function  LocateEx      ( const _ptg     : TGIS_Point ;
                                const _prec    : Double     ;
                                const _uid     : Integer    ;
                                var   _dist    : Double     ;
                                var   _part    : Integer    ;
                                var   _proj    : TGIS_Point ;
                                const _visible : Boolean
                              ) : TGIS_Shape ; overload; virtual;

      /// <summary>
      ///   Locate a shape (more precise).
      /// </summary>
      /// <param name="_ptg">
      ///   reference point /searching point/
      /// </param>
      /// <param name="_prec">
      ///   precision /not a longer distance than/; point inside a polygon is
      ///   always not greater than _dist; if _prec is less then 0, then
      ///   being outside/inside polygon means the same
      /// </param>
      /// <param name="_uid">
      ///   Uid of reference shape or -1; thanks to this, is possible to find
      ///   nearest shape, excluding shape given by _uid.
      /// </param>
      /// <param name="_dist">
      ///   reached distance between _pt and shape in Result; for points
      ///   inside the polygon (if _prec &gt;= 0) the distance will be
      ///   multiply by 0.95 (to prefer points inside the polygon) but will
      ///   not be bigger then _prec; for point distance will be multiplied
      ///   by 0.9 to prefer points over lines and polygons
      /// </param>
      /// <param name="_part">
      ///   number of the part closest to a given point
      /// </param>
      /// <param name="_proj">
      ///   point projected to the nearest element of a shape
      /// </param>
      /// <param name="_dist_prefer">
      ///   preferred distance (including shape type priority :
      ///   points over lines and polygons)
      /// </param>
      /// <param name="_visible">
      ///   if true the only visible shapes will be evaluated; shapes turned
      ///   of by query of hidden will be ignored
      /// </param>
      /// <returns>
      ///   found shape or nil
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Use this method to locate a shape that is near _ptg but also on
      ///     _prec distance. It finds a shape using FindFirst method in
      ///     extent corrected by _prec and _ptg params.
      ///   </para>
      ///   <para>
      ///     When using this operation, never call any function which uses
      ///     FindFirst..FindNext (any function which iterates through the
      ///     shapes using it - so never use inside an operation like Locate etc.).
      ///   </para>
      ///   <para>
      ///     See Locate for example.
      ///   </para>
      /// </remarks>
      function  LocateEx      ( const _ptg        : TGIS_Point ;
                                const _prec       : Double     ;
                                const _uid        : Integer    ;
                                var   _dist       : Double     ;
                                var   _part       : Integer    ;
                                var   _proj       : TGIS_Point ;
                                var   _dist_prefer: Double     ;
                                const _visible    : Boolean
                              ) : TGIS_Shape ; overload; virtual;
    public

      /// <summary>
      ///   All sub layers.
      /// </summary>
      property Items     : TGIS_LayerAbstractList read FItems ;

      /// <summary>
      ///   Layer project hierarchy.
      /// </summary>
      property Hierarchy : TGIS_Strings  read FHierarchy;
  end ;

implementation

{$IFDEF DCC}
  uses
    GisRtl,
    GisClasses,
    GisCsSystems,
    GisFunctions,
    GisInternals,
    GisLayerSublayer,
    GisParams,
    GisRegistredLayers,
    GisResource;
{$ENDIF}

//=============================================================================
// TGIS_LayerProject
//=============================================================================

  constructor TGIS_LayerProject.Create;

  begin
    inherited ;

    ParamsList.SetUp( TGIS_ParamsSectionPixel.Create ) ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;

    FItems         := TGIS_LayerAbstractList.Create( True ) ;
    FHierarchy     := TGIS_StringList.Create ;
    Path           := '' ;
    Name           := '' ;

    FIsModified    := False ;
  end ;

  procedure TGIS_LayerProject.doDestroy ;
  begin
    FreeObject( FItems        ) ;
    FreeObject( FHierarchy    ) ;
    FreeObject( FProjectFile  ) ;

    inherited;
  end ;

  function TGIS_LayerProject.fget_FileCopyrights : String ;
  var
    i  : Integer ;
    ll : TGIS_Layer ;
  begin
    Result := '' ;

    for i := 0 to Items.Count -1 do begin
      ll := TGIS_Layer( Items[i] ) ;

      if not ll.Active then
        continue ;

      if IsStringEmpty( ll.FileCopyrights ) then
        continue ;

      if not IsStringEmpty( Result ) then
        Result := Result + #13#10 ;

      Result := Result + ll.Caption + ': ' + ll.FileCopyrights ;
    end ;

  end ;

  procedure TGIS_LayerProject.setUp;
  begin
    inherited ;

    FFileInfo := 'Generic Project Layer' ;

    openInternal ;
  end ;


  function TGIS_LayerProject.DormantGain
    : Integer ;
  var
    i   : Integer ;
  begin
    Result := 0 ;

    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    if assigned( SubLayers ) then
      for i := 0 to SubLayers.Count - 1 do
        Result := Result + TGIS_Layer( SubLayers[i] ).DormantGain ;
  end ;

  procedure TGIS_LayerProject.Dormant ;
  var
    i   : Integer ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    if assigned( SubLayers ) then
      for i := 0 to SubLayers.Count - 1 do
        TGIS_Layer( SubLayers[i] ).Dormant ;
  end ;

  function TGIS_LayerProject.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    i           : Integer    ;
    ll          : TGIS_Layer ;
    start       : Integer    ;
    stop        : Integer    ;
    cnt         : Integer    ;
    is_visible  : Boolean    ;
    oldvwr      : IGIS_Viewer ;
  begin
    Result := False ;
    if Viewer.Ref.Zoom <= 0 then exit ;
    if Viewer.Ref.IsEmpty then exit  ;

    is_visible := Self.Active                                         and
              ( not GisIsNoWorld( Self.ProjectedExtent ) )            and
              GisIsCommonExtent( Self.ProjectedExtent, Viewer.Ref.VisibleExtent ) ;
    if not is_visible then exit ;

    prepareParamsCache( '' ) ;
    cnt := paramsCache.Count ;

    if cnt <= 0 then exit ;

    is_visible := False ;
    for i:= cnt -1 downto 0 do begin
      is_visible := is_visible or
                    TGIS_ParamsSection( paramsCache[i] ).Visible ;
      if is_visible then break ;
    end ;

    if not is_visible then exit ;

    Result  := True ;
    start   := 0 ;
    stop    := Items.Count -1 ;

    for i:= start to stop do begin // all layers
      ll := TGIS_Layer( Items[i] ) ;
      oldvwr := Viewer.Ref.AttachLayer( ll ) ;
      ll.Renderer := Self.Renderer ;
      ll.DrawEx( _extent ) ;
      if assigned( oldvwr ) then
        oldvwr.AttachLayer(ll );
      if HourglassShake then exit ;
    end ;
  end ;

  procedure TGIS_LayerProject.DrawFlash ;
  begin

  end ;

  function TGIS_LayerProject.verifyParent(
    const _path : String
  ) : Boolean ;
  var
    la : TGIS_Layer ;
  begin
    Result := True ;

    if assigned( Viewer ) and ( _path = Viewer.Ref.ProjectName ) then begin
      Result := False ;
      exit;
    end ;

    la := self.ParentLayer ;
    while assigned( la ) do begin
      if _path = la.Path then begin
        Result := False ;
        break ;
      end ;

      la := la.ParentLayer ;
    end ;
  end ;

  procedure TGIS_LayerProject.addHierarchy ;
  begin

  end ;

  procedure TGIS_LayerProject.openInternal ;
  var
    i        : Integer      ;
    ll       : TGIS_Layer   ;
    dir      : String       ;
    ex       : TGIS_Extent  ;
    err      : String       ;
  begin
    if IsStringEmpty( Path ) then exit ;

    err := '' ;
    try
      FProjectFile := TGIS_ConfigFactory.CreateConfig( nil, Path ) ;
      ReadConfig ;
      FIsModified := False ;
      dir := GetFileDir( Path ) ;

      if assigned( FProjectFile ) then begin

        try
          for i := 1 to FProjectFile.PrjLayersCount do begin
            ll := GisCreateLayer( FProjectFile.PrjLayerName[i],
                                  GetPathAbsolute( dir,
                                                   FProjectFile.PrjLayerPath[i]
                                                 )
                                ) ;

            if assigned( ll ) then begin

              if IsFileOrServerPath( GetPathAbsolute( dir, ll.Path ) )
              then begin
                try
                  Add( ll ) ;
                  with FProjectFile do begin
                    SetLayer( ll ) ;
                    SetSection( 0, False ) ;
                    applyConfigProjection(FProjectFile, ll ) ;
                  end ;
                except
                  on e : Exception do begin
                    FreeObject( ll ) ;
                    err := err + #13#10 + e.Message ;
                    continue ;
                  end ;
                end ;
                ll.ConfigName := GetPathAbsolute( dir,
                                                  FProjectFile.PrjLayerConfig[i]
                                                 ) ;
              end ;
            end ;
          end ;

          FHierarchy.Clear ;
          FProjectFile.ReadSectionValues( GIS_INI_HIERARCHY_HEADER, FHierarchy );
          addHierarchy ;

          with FProjectFile do begin
            SetLayer( nil ) ;
            ex.XMin := ReadFloat( GIS_INI_VISIBLEEXTENT_XMIN, Extent.XMin ) ;
            ex.XMax := ReadFloat( GIS_INI_VISIBLEEXTENT_XMAX, Extent.XMax ) ;
            ex.YMin := ReadFloat( GIS_INI_VISIBLEEXTENT_YMIN, Extent.YMin ) ;
            ex.YMax := ReadFloat( GIS_INI_VISIBLEEXTENT_YMAX, Extent.YMax ) ;

            applyConfigProjection(FProjectFile ) ;
          end ;
          Extent := ex ;

          if CS.EPSG <> 0 then begin
            for i := 0 to Items.Count -1 do begin
              ll := TGIS_Layer( Self.Items[i] ) ;
              if ll.CS is TGIS_CSUnknownCoordinateSystem then begin
                Self.CS := nil ;
                break ;
              end;
            end ;
          end ;

        finally
          FIsModified := False ;
        end ;
      end ;

    finally
      FIsModified := False ;
      if not IsStringEmpty( err ) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERMISSED ), err, 0 ) ;
    end ;

  end ;

  procedure TGIS_LayerProject.Add(
    const _layer : TGIS_Layer
  ) ;
  var
    lst     : TGIS_StringList ;
    tmp     : String          ;
    oldprj  : TGIS_Config     ;

    function getUniqueLayerName( const _name : String ) : String ;
    var
      i    : Integer ;
      tmp1 : String  ;
    begin
      i := 0 ;
      Result := _name ;
      while Get( Result ) <> nil do begin
        if i > 0 then
          tmp1 := Copy( Result, StringFirst,
                        length( Result ) - length( Format( ' [%d]', [i] ) )
                      )
        else
          tmp1 := Result ;

        Result := Format( '%s [%d]', [tmp1, i+1 ] ) ;

        inc( i ) ;
      end ;
    end ;

  begin
    _layer.Viewer := Viewer ;
    _layer.ParentLayer := self ;

    if not verifyParent( _layer.Path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYEREXIST ), _layer.Path, 0 ) ;
    end ;

    if IsStringEmpty( _layer.Name ) then begin
       if SafeFileExists( _layer.Path ) then begin
         // for file based layers setup Name=Path if no other name exist
         tmp := GetFileNameNoExt( _layer.Path ) ;
       end
       else begin
         // for SQL Layers use table names
         lst := TGIS_StringList.Create ;
         try
           ReadSQLParamsFromPath( _layer.Path, lst ) ;
           tmp := lst.Values[ GIS_INI_LAYERSQL_LAYER ] ;
         finally
           FreeObject( lst ) ;
         end ;

         // for protocol based layers like ECWP:// - use file name
         if IsStringEmpty( tmp ) then begin
           tmp := GetFileNameNoExt( _layer.Path ) ;
           if tmp = _layer.Path then begin
             // not this case - it is not protocol based format
             tmp := ''
           end ;
         end ;

         // All other cases - use generic name
         if IsStringEmpty( tmp ) then
           tmp := _rsrc( GIS_RS_GENERAL_DEFAULT_LAYERNAME ) ;
       end ;
       _layer.Name := Name + '.' + getUniqueLayerName( tmp ) ;
    end ;

    if not _layer.IsOpened then
      _layer.Open ;

    if IsStringEmpty( _layer.Caption ) then
      // setup ConfigName=Path if no other name exist
      _layer.Caption := _layer.Name ;

    if Get( _layer.Name ) <> nil then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYEREXIST ), _layer.Name, 0 ) ;

    Items.Add( _layer ) ;           // add to list

    RecalcExtent ;

    if assigned( Viewer ) then begin
      oldprj := TGIS_Config( Viewer.Ref.ProjectFile ) ;
      try
        Viewer.Ref.ProjectFile := FProjectFile ;
        _layer.ReadConfig ;
      finally
        Viewer.Ref.ProjectFile := oldprj ;
      end ;
    end ;

    if _layer.IsPersistent then
      FIsModified := True ;
  end ;

  procedure TGIS_LayerProject.Delete(
    const _name : String
  ) ;
  var
    i  : Integer ;
    la : TGIS_Layer ;
  begin
    for i := 0 to (Items.Count - 1) do begin // all layers
      if TGIS_Layer( Items[ i ] ).Name = _name then
      begin
        // found! delete it!
        la := TGIS_Layer( Items[ i ] ) ;
        if la.IsPersistent then
          FIsModified := True ;

        FreeObject( la ) ;
        exit ;
      end ;
    end ;
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ), _name, 0 ) ;
  end ;

  function TGIS_LayerProject.Get(
    const _name : String
  ) : TGIS_Layer ;
  var
    i : Integer ;
  begin
    for i := 0 to (Items.Count - 1) do begin  // all layers
      if CompareText( TGIS_Layer( Items[ i ] ).Name, _name ) = 0 then begin
        // found!
        Result := TGIS_Layer( Items[ i ] ) ;
        exit ;
      end ;
    end ;
    Result := nil ; // not found
  end ;

  function TGIS_LayerProject.Get(
    const _layer : TGIS_Layer
  ) : TGIS_Layer ;
  var
    i : Integer ;
  begin
    for i := 0 to (Items.Count - 1) do begin  // all layers
      if TGIS_Layer( Items[ i ] ) = _layer then begin
        // found!
        Result := TGIS_Layer( Items[ i ] ) ;
        exit ;
      end ;

      if TGIS_Layer( Items[ i ] ) is TGIS_LayerProject then begin
        Result := TGIS_LayerProject( Items[ i ] ).Get( _layer ) ;
        if assigned( Result ) then
          exit;
      end ;
    end ;
    Result := nil ; // not found
  end ;

  procedure TGIS_LayerProject.RecalcProjectedExtent ;
  var
    i   : Integer ;
    ext : TGIS_Extent ;
  begin
    ext := GisNoWorld ;

    for i := 0 to Items.Count -1 do begin
      TGIS_Layer( Items[i] ).RecalcProjectedExtent ;

      if GisIsNoWorld( TGIS_Layer( Items[i] ).ProjectedExtent ) then
        continue ;

      ext := GisMaxExtent( ext,
                           TGIS_Layer( Items[i] ).ProjectedExtent
                         ) ;
    end ;

    FProjectedExtent := ext ;
  end ;

  procedure TGIS_LayerProject.RecalcExtent ;
  var
    i   : Integer ;
    ext : TGIS_Extent ;
  begin
    ext := GisNoWorld ;

    for i := 0 to Items.Count -1 do begin
      TGIS_Layer( Items[i] ).RecalcProjectedExtent ;

      if GisIsNoWorld( TGIS_Layer( Items[i] ).ProjectedExtent ) then
        continue ;

      ext := GisMaxExtent( ext,
                           TGIS_Layer( Items[i] ).ProjectedExtent
                         ) ;
    end ;

    FExtent := ext ;
  end ;

  function TGIS_LayerProject.LocateEx(
    const _ptg     : TGIS_Point ;
    const _prec    : Double     ;
    const _uid     : Integer    ;
    var   _dist    : Double     ;
    var   _part    : Integer    ;
    var   _proj    : TGIS_Point
  ) : TGIS_Shape ;
  begin
    Result := LocateEx( _ptg, _prec, _uid, _dist, _part, _proj, True ) ;
  end ;

  function TGIS_LayerProject.LocateEx(
    const _ptg     : TGIS_Point ;
    const _prec    : Double     ;
    const _uid     : Integer    ;
    var   _dist    : Double     ;
    var   _part    : Integer    ;
    var   _proj    : TGIS_Point ;
    const _visible : Boolean
  ) : TGIS_Shape ;
  var
    distp : Double ;
  begin
    Result := LocateEx( _ptg, _prec, _uid, _dist, _part, _proj, distp, True ) ;
  end ;

  function TGIS_LayerProject.LocateEx(
    const _ptg         : TGIS_Point ;
    const _prec        : Double     ;
    const _uid         : Integer    ;
    var   _dist        : Double     ;
    var   _part        : Integer    ;
    var   _proj        : TGIS_Point ;
    var   _dist_prefer : Double     ;
    const _visible     : Boolean
  ) : TGIS_Shape ;
  var
    i         : Integer ;
    shape     : TGIS_Shape ;  // found values
    tmp_dist  : Double ;              // temporary values
    tmp_distp : Double     ;
    tmp_part  : Integer ;             // temporary values
    tmp_shape : TGIS_Shape ;
    dist      : Double ;
    proj      : TGIS_Point ;
    ll        : TGIS_LayerVector ;
    lp        : TGIS_LayerProject ;
  begin
    Result     := nil ;

    if assigned( Viewer ) and
       Viewer.Ref.InPaint
    then
      exit ;

    if Basemap then
      exit ;

    if not IsOpened then
      Open ;

    shape := nil ;            // assume we find nothing
    dist  := GIS_MAX_DOUBLE ; // assume really huge starting distance

    for i:= Items.Count -1 downto 0 do begin // all layers
      if TGIS_Layer( Items[i] ).Active then begin // but only visible
        try
          if ( Items[i] is TGIS_LayerVector ) then begin

            ll := Items[i] as TGIS_LayerVector ;
            tmp_shape := ll.LocateEx( _ptg, _prec, -1, tmp_dist, tmp_part, proj,
                                      tmp_distp, _visible
                                    ) ;
          end
          else if ( Items[i] is TGIS_LayerProject ) then begin
            lp := Items[i] as TGIS_LayerProject ;
            tmp_shape := lp.LocateEx( _ptg, _prec, -1, tmp_dist, tmp_part, proj,
                                      tmp_distp, _visible
                                    ) ;
          end
          else
            tmp_shape := nil ;

          if tmp_shape <> nil then begin
            if tmp_distp <= dist then begin
              // is closer then previous (or lay out on top)
              shape := tmp_shape ;
              dist  := tmp_distp ;
            end ;
          end ;

        except
        end ;
      end ;
    end ;
    Result := shape;
  end ;

  procedure TGIS_LayerProject.ReadConfig ;
  begin
    inherited ;

    if assigned( FProjectFile ) then
      applyConfigProjection( FProjectFile ) ;
  end ;

  procedure TGIS_LayerProject.RereadConfig ;
  var
    i  : Integer ;
  begin
    if assigned( FProjectFile ) then
      ReadConfig ;

    for i := (Items.Count-1) downto 0 do
      TGIS_Layer( Items[i] ).RereadConfig ;
  end ;

//==============================================================================
// initialization / finalization
//==============================================================================

  class procedure Unit_GisLayerProject.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKGP', 'Generic Project Layer', TGIS_LayerProject,
                   GIS_TTKPROJECT_FILTER,
                   TGIS_RegisteredLayerType.Project,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                    True
                  ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerProject.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

