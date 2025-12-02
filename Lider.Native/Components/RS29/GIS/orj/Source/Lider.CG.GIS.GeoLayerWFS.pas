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
  Encapsulation of a OpenGIS WFS Layer.
}

{$IFDEF DCC}
  unit GisLayerWFS ;
  {$HPPEMIT '#pragma link "GisLayerWFS"'}
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

    GisTypes,
    GisLayerCompound,
    GisFileWFS ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerWFS = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  /// Encapsulation of an OpenGIS WFS layer.
  /// </summary>
  TGIS_LayerWFS = {$IFDEF OXYGENE} public {$ENDIF}
                  class( TGIS_LayerCompoundVector )
    private // properties internal values
      FUserAgent        : String      ;
      FProxyUrl         : String      ;
      FUseLayers        : TStringList ;
      FAllLayers        : TStringList ;
      FWFS              : TGIS_FileWFS ;
      FTimeOut          : Integer ;
      FPagingEnabled    : Boolean ;
      FPageSize         : Integer ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp    ; override;
    private
      /// <summary>
      ///   Query structure from data.
      /// </summary>
      procedure queryStructureFromData ;

      /// <summary>
      ///   Query structure from schema.
      /// </summary>
      procedure queryStructureFromSchema ;
    protected
      // destructor

      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function    Draw            : Boolean ; override;

      /// <inheritdoc/>
      function    DrawEx          ( const _extent  : TGIS_Extent
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      function    PreRecognize    ( const _path     : String ;
                                    var   _new_path : String
                                  ) : Boolean ; override;
    public // properties

      /// <summary>
      ///   User Agent string.
      /// </summary>
      property UserAgent   : String read FUserAgent write FUserAgent ;

      /// <summary>
      ///   Proxy URL as for ESRI proxy.ashx.
      /// </summary>
      property ProxyUrl    : String read FProxyUrl write FProxyUrl ;

      /// <summary>
      ///   List of layers to be used for render. If empty then
      /// </summary>
      property UseLayers   : TGIS_StringList read FUseLayers ;

      /// <summary>
      ///   List of all found layers names.
      /// </summary>
      property FoundLayers : TGIS_StringList read FAllLayers ;
  end ;

const
  {#gendoc:hide}
  METADATA_WFS_IGNORE504 = 'TGIS_LayerWFS.Ignore.504' ;

implementation
//##############################################################################

{$IFDEF OXYGENE}
{$ELSE}
  uses
    GisRegistredLayers,
    GisClasses,
    GisRtl,
    GisFunctions,
    GisInternals,
    GisInterfaces,
    GisLayer,
    GisLayerGML,
    GisLayerVector,
    GisResource ;
{$ENDIF}

{$IFDEF JAVA}
  const
    aSetUpShapeTypes : array of TGIS_ShapeType =
      ( TGIS_ShapeType.Point ,
        TGIS_ShapeType.MultiPoint,
        TGIS_ShapeType.Arc,
        TGIS_ShapeType.Polygon
      ) ;
{$ENDIF}

//==============================================================================
// TGIS_LayerWFS
//==============================================================================

  constructor TGIS_LayerWFS.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;

    FUseLayers   := TStringList.Create ;
    FAllLayers   := TStringList.Create ;

    FUserAgent   := GetDefaultUserAgent( 'ttkWP' ) ;
    FProxyUrl    := '' ;
    FWFS         := nil ;
  end ;

  procedure TGIS_LayerWFS.doDestroy ;
  begin
    FreeObject( FUseLayers ) ;
    FreeObject( FAllLayers ) ;
    FreeObject( FWFS       ) ;

    inherited ;
  end ;

  procedure TGIS_LayerWFS.setUp ;
  var
    i,k   : Integer ;
    lv    : TGIS_LayerGML ;
    lst   : TStringList    ;
    sPath : String ;
    fea   : TGIS_WFSFeature ;
  begin
    FreeObject( FWFS ) ; // for potential re-open scenario
    if assigned( Viewer ) then
      FWFS := TGIS_FileWFS.Create( Viewer.Ref, self )
    else
      FWFS := TGIS_FileWFS.Create( nil, self ) ;

    FWFS.PasswordEvent := FOnPassword ;

    if not IsServerPath( Path ) or IsEmbeddedSQLPath( Path ) then begin
      lst := TStringList.Create ;
      try
        ReadParamsFromPath( Path, lst ) ;

        sPath      := LstReadString ( lst,
                                      'Url',
                                      ''
                                    ) ;
        Name       := LstReadString ( lst,
                                      'Name',
                                      Name
                                    ) ;
        Caption    := LstReadString ( lst,
                                      'Caption',
                                      Caption ) ;
        FUserAgent := LstReadString ( lst,
                                      'UserAgent',
                                      FUserAgent
                                    ) ;
        FProxyUrl  := LstReadString ( lst,
                                      'ProxyUrl',
                                      FProxyUrl
                                    ) ;
        if not IsStringEmpty( FProxyUrl ) then begin
          if FProxyUrl[ StringLast( FProxyUrl ) ] <> '?' then
            FProxyUrl := FProxyUrl + '?' ;
        end ;
        FTimeOut        := LstReadInteger( lst,
                                          'TimeOut',
                                          90000
                                         ) ;
        FPagingEnabled  := LstReadBoolean( lst,
                                           'PagingEnabled',
                                           False
                                         ) ;
        FPageSize       := LstReadInteger( lst,
                                           'PageSize',
                                           1000
                                         ) ;
        if IsStringEmpty( sPath )  then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                       'Url=', 0
                                    ) ;
        FWFS.Metadata.Assign( lst ) ;
      finally
        FreeObject( lst ) ;
      end ;
      FWFS.TimeOut   := FTimeOut ;
    end
    else begin
      sPath := Path ;

      FWFS.TimeOut    := GisMetadataAsInteger(
                            'TGIS_LayerWFS.TimeOut',
                            90000
                         ) ;
      FWFS.TimeOut    := StrToIntDef(
                            URLGetAndDeleteParameterValue(
                              sPath, GIS_INI_TIMEOUT
                            ),
                            FWFS.TimeOut
                         ) ;
      FPagingEnabled  := GisMetadataAsBoolean(
                            'TGIS_LayerWFS.PagingEnabled',
                            False
                         ) ;
      FPageSize       := GisMetadataAsInteger(
                            'TGIS_LayerWFS.PageSize',
                            1000
                         ) ;
    end ;

    k := Pos( '?http://', sPath ) ;
    if k <= StringFirst then
      k := Pos( '?https://', sPath ) ;

    if k > StringFirst then begin
      FProxyUrl := Copy( sPath, StringFirst, k ) ;
      sPath     := Copy( sPath, k+1, 4096 ) ;
    end ;

    FWFS.UserAgent := FUserAgent ;
    FWFS.ProxyUrl  := FProxyUrl ;

    FWFS.PagingEnabled := FPagingEnabled ;
    FWFS.PageSize      := FPageSize ;

    sPath := URLFixed( sPath ) ;

    try
      FWFS.Load( sPath ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), FWFS.Error, 0 ) ;
    end ;

    if not IsStringEmpty( FWFS.Error ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), FWFS.Error, 0 ) ;

    if FWFS.PredefinedLayers.Count > 0 then
      UseLayers.Assign( FWFS.PredefinedLayers ) ;

    // setup layer if paging found in url
    FPagingEnabled := FWFS.PagingEnabled ;
    FPageSize      := FWFS.PageSize ;

    try
      inherited setUp ;
    except
      on e : Exception do
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), e.Message, 0 ) ;
    end ;

    if not IsStringEmpty( FWFS.PredefinedSRS ) then
      SetCSByWKT( FWFS.PredefinedSRS )
    else begin
      if FWFS.PredefinedLayers.Count > 0 then begin
        fea := FWFS.Feature[FWFS.PredefinedLayers[0]] ;
        if assigned( fea ) then
          SetCSByWKT( fea.DefaultSRS )
      end
      else
        SetCSByWKT( FWFS.GetCommonSRS ) ;
    end ;

    if FUseLayers.Count > 0 then begin
      for i := 0 to FUseLayers.Count - 1 do begin
        fea := FWFS.Feature[ FUseLayers[i] ] ;
        if assigned( fea ) then begin
          lv := TGIS_LayerGML.Create ;
          lv.BlockRTree  := True ;
          lv.Name        := fea.Name ;
          lv.Caption     := fea.Title ;
          lv.CS          := CS ;
          lv.Extent      := FWFS.GetLayerExtent( lv.Name, lv.CS.EPSG ) ;
          if GisIsNoWorld( lv.Extent ) then
            lv.Extent := GisExtent( -0, -0,
                                    0, 0
                                  ) ;
          lv.SupportedShapes := GisGetEmptyShapeType ;
          lv.SupportedShapes := GisAddShapeType(
                                  lv.SupportedShapes,
                                  TGIS_ShapeType.Point
                                ) ;
          lv.SupportedShapes := GisAddShapeType(
                                  lv.SupportedShapes,
                                  TGIS_ShapeType.MultiPoint
                                ) ;
          lv.SupportedShapes := GisAddShapeType(
                                  lv.SupportedShapes,
                                  TGIS_ShapeType.Arc
                                ) ;
          lv.SupportedShapes := GisAddShapeType(
                                  lv.SupportedShapes,
                                  TGIS_ShapeType.Polygon
                                ) ;

          lv.Active            := i = 0 ;
          lv.AxisOrderReversed := FWFS.IsAxisReversed ;
          Add( lv ) ;
          FAllLayers.Add( fea.Name ) ;
        end ;
      end ;
    end
    else begin
      for i := 0 to FWFS.FeaturesCount - 1 do begin
        fea := FWFS.Feature[ i ] ;
        if assigned( fea ) then begin
          lv := TGIS_LayerGML.Create ;
          lv.BlockRTree  := True ;
          lv.Name        := fea.Name ;
          lv.Caption     := fea.Title ;
          lv.CS          := CS ;
          lv.Extent      := FWFS.GetLayerExtent( lv.Name, lv.CS.EPSG ) ;

          lv.SupportedShapes := GisGetEmptyShapeType ;
          lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Point ) ;
          lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.MultiPoint ) ;
          lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Arc ) ;
          lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Polygon ) ;

          lv.Active            := i = 0 ;
          lv.AxisOrderReversed := FWFS.IsAxisReversed ;
          Add( lv ) ;
          FAllLayers.Add( fea.Name ) ;
        end ;
      end ;
    end ;

    ReadConfig ;
    RecalcProjectedExtent ;

    case GisMetadataAsInteger( 'TGIS_LayerWFS.QueryStructureMode', 0 ) of
      0 : ;                          // no query, call Draw() instead to read fields
      1 : queryStructureFromSchema ; // use DescribeFeatureType request
      2 : begin                      // fetch a feature to read fields
            FWFS.QueryStructure := True ;
            try
              queryStructureFromData ;
            finally
              FWFS.QueryStructure := False ;
            end ;
          end ;
    end ;

    FFileInfo := 'OpenGIS Web Feature Service (WFS)' + #13#10 + FWFS.Version ;
  end ;

  procedure TGIS_LayerWFS.queryStructureFromData ;
  var
    i     : Integer       ;
    lg    : TGIS_LayerGML ;
    cfg   : TStringList   ;
    fea   : TGIS_WFSFeature ;
    ready : Boolean ;
    cext  : TGIS_Extent ;
    uext  : TGIS_Extent ;
    ext   : TGIS_Extent ;
    oext  : TGIS_Extent ;
    ecalc : Boolean ;
  begin
    cext := ProjectedExtent ;
    uext := self.UnprojectExtent( cext ) ;

    ready := False ;
    while (not ready) do begin
      ready := True ;
      for i := 0 to SubLayers.Count - 1 do begin
        lg  := TGIS_LayerGML( SubLayers[ i ] ) ;
        ext := GisCommonExtent( uext, lg.Extent ) ;
        fea := FWFS.Feature[ lg.Name ] ;
        ready := fea.IsDataReady ;
        if not ready then
          FWFS.GetFeatureData( fea, ext ) ;
        if HourglassShake then break ;
      end ;
      if HourglassShake then break ;
    end ;

    for i := 0 to SubLayers.Count - 1 do begin
      lg    := TGIS_LayerGML( SubLayers[ i ] ) ;
      fea   := FWFS.Feature[ lg.Name ] ;
      oext  := lg.Extent ;
      ecalc := GisIsSameExtent( oext, GisExtent( 0, 0, 0, 0 ) ) ;
      if fea.IsDataReady then
        lg.Stream := fea.Data ;
      lg.Renderer := Renderer ;
      lg.Path     := '' ;
      try
        if not fea.IsDataValid then Abort ;
        cfg := TStringList.Create   ;
        try
          lg.ParamsList.SaveToStrings( cfg ) ;
          lg.ReOpen ;
          lg.ParamsList.LoadFromStrings( cfg ) ;
        finally
          FreeObject( cfg ) ;
        end ;
        lg.Path := fea.LastUrl ;
        lg.CS := CS ;
        lg.RecalcProjectedExtent ;
      except
        // ignore errors
      end ;
      // restore original state
      lg.RevertShapes ;
      fea.ResetState ;
      if ecalc then
        lg.RecalcProjectedExtent
      else
        lg.Extent := oext ;
      if HourglassShake then break ;
    end ;
  end ;

  procedure TGIS_LayerWFS.queryStructureFromSchema ;
  var
    i     : Integer       ;
    lg    : TGIS_LayerGML ;
    stm   : TStream ;
  begin
    for i := 0 to SubLayers.Count - 1 do begin
      lg := TGIS_LayerGML( SubLayers[ i ] ) ;

      stm := FWFS.DescribeFeatureType( lg.Name ) ;
      try
        lg.ParseSchemaStream( stm ) ;
      finally
        FreeObject( stm ) ;
      end ;
      if HourglassShake then break ;
    end ;
  end ;

  function TGIS_LayerWFS.Draw : Boolean ;
  begin
    resetMustReproject ;

    if assigned( Viewer ) then
      Result := DrawEx( drawExtentEx )
    else
      Result := DrawEx( ProjectedExtent ) ;
  end ;

  function TGIS_LayerWFS.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    i      : Integer       ;
    lg     : TGIS_LayerGML ;
    lst    : TStringList   ;
    cfg    : TStringList   ;
    msg    : String        ;
    fea    : TGIS_WFSFeature ;
    ready  : Boolean ;
    cext   : TGIS_Extent ;
    uext   : TGIS_Extent ;
    ext    : TGIS_Extent ;
    fcnt   : Integer ;
    fread  : Integer ;
    abrt   : Boolean ;
    lv     : TGIS_LayerVector ;
    oext   : TGIS_Extent ;
    ecalc  : Boolean ;
    iscext : Boolean ;
    ignore : Boolean ;
  begin
    if assigned( Viewer ) then begin
      Result := IsVisible( _extent ) ;
      if not Result then exit ;
      ext  := Viewer.Ref.UnrotatedExtent( Viewer.Ref.VisibleExtent ) ;
      cext := GisCommonExtent( ext, ProjectedExtent ) ;
    end
    else
      cext := _extent ;

    uext := self.UnprojectExtent( cext ) ;

    ready := False ;

    if not FPagingEnabled then begin
      {$REGION 'No paging'}
      while (not ready) do begin
        ready := True ;
        for i := 0 to SubLayers.Count - 1 do begin
          lg := TGIS_LayerGML( SubLayers[ i ] ) ;
          if not lg.Active then continue ;

          oext := lg.Extent ;
          ext  := GisCommonExtent( uext, lg.Extent ) ;
          fea  := FWFS.Feature[ lg.Name ] ;
          if not GisIsContainExtent( ext, fea.LastExtent ) then begin
            lg.RevertShapes ;
            ready := fea.IsDataReady ;
            if not ready then
              FWFS.GetFeatureData( fea, ext ) ;
          end;

          if HourglassShake then break ;
        end ;
        if HourglassShake then break ;
      end;

      for i := 0 to SubLayers.Count - 1 do begin
        lg := TGIS_LayerGML( SubLayers[ i ] ) ;
        if not lg.Active then continue ;

        if GisIsNoWorld( lg.Extent ) or (lg.Items.Count=0) then begin
          fea := FWFS.Feature[ lg.Name ] ;
          if fea.IsDataReady then
            lg.Stream := fea.Data ;
          lg.Renderer := Renderer ;
          lg.Path     := '' ;
          try
            if not fea.IsDataValid then Abort ;
            // prevent resetting a layer config
            cfg := TStringList.Create   ;
            try
              lg.ParamsList.SaveToStrings( cfg ) ;
              lg.ParamsList.ResetSerial ;
              lg.RevertAll ;
              lg.AddFromStream ;
              lg.ParamsList.LoadFromStrings( cfg ) ;
            finally
              FreeObject( cfg ) ;
            end ;
            lg.Path := fea.LastUrl ;
            lg.CS := CS ;
            lg.RecalcProjectedExtent ;
          except
            lst := TStringList.Create ;
            try
              lst.LoadFromStream( lg.Stream );
              msg := Format( _rsrc( GIS_RS_ERR_SERVER_ERROR ), [ lst.Text ] ) ;
              if not IsStringEmpty( msg ) then begin
                ignore := False ;

                fea := FWFS.Feature[ lg.Name ] ;
                if fea.HTTPStatus = 504 then
                  if GisMetadataAsBoolean( METADATA_WFS_IGNORE504, True ) then
                    ignore := True ;
                if not ignore then
                  raise EGIS_Exception.Create( msg, Path, 0 ) ;
              end;
            finally
              FreeObject( lst ) ;
            end ;
          end ;
          fea.ResetState ;
          fea.LastExtent := ext ;
          lg.Extent := oext ; // restore original extent
        end ;
        if assigned( Viewer ) then
          lg.Draw ;

        if HourglassShake then break ;
      end ;
      {$ENDREGION}
    end
    else begin
      {$REGION 'Paging'}
      abrt := False ;
      while (not ready) do begin
        ready := True ;
        for i := 0 to SubLayers.Count - 1 do begin
          lg := TGIS_LayerGML( SubLayers[ i ] ) ;
          if not lg.Active then continue ;
          ext := GisCommonExtent( uext, lg.Extent ) ;
          fea := FWFS.Feature[ lg.Name ] ;

          try
            iscext := GisIsContainExtent( ext, fea.LastExtent ) ;
            if not iscext then begin
              lg.RevertShapes ;
              fea.ResetDataPage ;
              fread := 0 ;
              oext := lg.Extent ;
              ecalc := GisIsSameExtent( oext, GisExtent( 0, 0, 0, 0 ) ) ;
              while True do begin
                ready := fea.IsDataReady ;
                if not ready then
                  abrt := not FWFS.GetFeatureData( fea, ext )
                else begin
                  if fea.IsDataValid then begin
                    lg.Stream   := fea.Data ;
                    lg.Renderer := Renderer ;
                    lg.Path     := '' ;
                    fcnt := lg.AddFromStream ;
                    inc( fread, fcnt ) ;
                    fea.IncreaseDataCount( fcnt ) ;
                    lg.Path := fea.LastUrl ;
                    fea.Data.Position := 0 ;

                    if assigned( Viewer ) then begin
                      // emulate progressive rendering
                      lv := TGIS_LayerVector.Create ;
                      try
                        lv.Renderer := Renderer ;
                        lv.Viewer   := Viewer ;
                        lv.Path     := '' ;
                        lv.Params.Assign( lg.Params ) ;
                        lv.CS := CS ;
                        lv.MergeLayer( lg, lg.Extent, TGIS_ShapeType.Unknown,
                                       'GIS_UID>='+IntToStr(fcnt), False, False
                                     ) ;
                        lv.RecalcProjectedExtent ;
                        lv.Draw ;
                      finally
                        FreeObject( lv ) ;
                      end ;
                    end ;

                    fea.ResetState ;
                    // server can return less than requested
                    if (fcnt < FPageSize) then
                      fea.NextDataPage( fcnt )
                    else
                      fea.NextDataPage( FPageSize ) ;

                    if fcnt = 0 then break ;
                  end
                  else begin
                    lst := TStringList.Create ;
                    try
                      lst.LoadFromStream( fea.Data );
                      msg := Format( _rsrc( GIS_RS_ERR_SERVER_ERROR ), [ lst.Text ] ) ;
                      if not IsStringEmpty( msg ) then begin
                        ignore := False ;

                        if fea.HTTPStatus = 504 then
                          if GisMetadataAsBoolean( METADATA_WFS_IGNORE504, True ) then
                            ignore := True ;
                        if not ignore then
                          raise EGIS_Exception.Create( msg, Path, 0 ) ;
                      end;
                    finally
                      FreeObject( lst ) ;
                    end ;
                    break ;
                  end;
                end ;
                if abrt or HourglassShake then begin
                  abrt := True ;
                  break ;
                end;
              end;
              if abrt or HourglassShake then break ;
              fea.LastExtent := ext ;
            end ;
          finally
            fea.ResetState ;

            if not iscext then begin
              if ecalc then
                lg.RecalcProjectedExtent
              else
                lg.Extent := oext ; // restore original extent
            end ;

            if assigned( Viewer ) then
              lg.Draw ;
          end;
        end;
        if abrt or HourglassShake then break ;
      end;
      {$ENDREGION}
    end ;

    Result := True ;
  end ;

  function TGIS_LayerWFS.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  begin
    Result := inherited PreRecognize( _path, _new_path ) and
              (
               ( GetParamFromPath( _path, GIS_INI_LAYERSQL_STORAGE ) = 'WFS' ) or
               ( UpperCase( URLGetParameterValue( _path, 'SERVICE' ) ) = 'WFS' ) or
               ( Pos( '/WFS', UpperCase( _path) ) > 0 )
              ) ;
  end ;


  { Perform initialization section.
  }
  class procedure Unit_GisLayerWFS.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKWP', GIS_PROTOCOL_LAYER_CONNECTOR,
                   TGIS_LayerWFS, GIS_TTKLAYER_WEB_FILTER,
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Protocol,
                    [ TGIS_RegisteredOperationType.Read
                    ],
                   False
                 ) ;
  end ;

  {$IFNDEF OXYGENE}
    initialization
      Unit_GisLayerWFS.SelfRegisterLayer() ;
  {$ENDIF}

{==================================== END =====================================}
end.


