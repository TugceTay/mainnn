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
  Encapsulation of a OpenGIS WCS Layer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerWCS ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerWCS"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.SysUtils,
    System.Classes,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoFileWCS,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoCsFactory,
    Lider.CG.GIS.GeoCsSystems ;
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
  GisLayerWCS = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  /// Encapsulation of an OpenGIS WCS layer.
  /// </summary>
  TGIS_LayerWCS = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private // properties internal values
      FAllCoverages     : TGIS_StringList ;
      FAllCRS           : TGIS_StringList ;
      FAllImageFormats  : TGIS_StringList ;
      FForcedCRS        : Integer ;
      FPPI              : Integer ;
      FIgnoreInternalURI: Boolean ;
      FUserAgent        : String ;
      FProxyUrl         : String ;
    private // properties access routine
      function  fget_LastUrl        : String ;
      function  fget_ServiceVersion : String ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      procedure fset_CS     ( const _value  : TGIS_CSCoordinateSystem
                            ) ; override ;
      /// <inheritdoc/>
      procedure setUp       ; override ;
    private
      procedure parseInternal ;
      procedure setLayerInternal ;
    private // other private values
      oWCS               : TGIS_FileWCS ;
      oResponse          : TGIS_HttpResponse ;
      oImage             : TGIS_LayerPixel ;
      sImageFormat       : String  ;
      cInfo              : String  ;
      cCopyright         : String  ;
      sPath              : String  ;
      iTimeOut           : Integer ;
      bAxisOrderIgnored  : Boolean ;
      bAxisOrderReversed : Boolean ;
    protected
      procedure doDestroy  ; override ;
    protected
      function fget_TransmittedBytes : Int64 ;
    public // various public routines
      /// <inheritdoc/>
      constructor Create   ; override ;

      /// <inheritdoc/>
      function  DrawEx            ( const _extent   : TGIS_Extent
                                  ) : Boolean ; override ;
      /// <inheritdoc/>
      procedure Alive             ; override ;

      /// <inheritdoc/>
      function  GetBitmap         ( const _extent   : TGIS_Extent ;
                                    const _bitmap   : TGIS_Pixels ;
                                    const _width    : Integer ;
                                    const _height   : Integer
                                  ) : Boolean; override ;
      /// <inheritdoc/>
      function  GetGrid           ( const _extent       : TGIS_Extent      ;
                                    const _grid         : TGIS_GridArray
                                  ) : Boolean ; override ;
      /// <inheritdoc/>
      function LocateEx           ( const _ptg          : TGIS_Point       ;
                                    var   _rgbMapped    : TGIS_Color       ;
                                    var   _nativesVals  : TGIS_DoubleArray ;
                                    var   _transparency : Boolean          ;
                                    const _pixelsize    : Double
                                  ) : Boolean ; override;
      /// <inheritdoc/>
      function  PreRecognize      ( const _path     : String ;
                                      var _new_path : String
                                  ) : Boolean ; override ;
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
      ///   Last Url fetched by client.
      /// </summary>
      property LastUrl    : String
                            read fget_LastUrl ;

      /// <summary>
      ///   EPSG code that will be forced to use with WCS request. If 0 then
      ///   first supported SRS will be used.
      /// </summary>
      property ForcedCRS : Integer  read  FForcedCRS
                                    write FForcedCRS ;

      /// <summary>
      ///   List of all found coverages names.
      /// </summary>
      property FoundCoverages : TGIS_StringList
                                read FAllCoverages ;

      /// <summary>
      ///   List of all found SRS.
      /// </summary>
      property FoundCRS : TGIS_StringList
                          read FAllCRS ;

      /// <summary>
      ///   List of all found image formats.
      /// </summary>
      property FoundImageFormats : TGIS_StringList
                                   read FAllImageFormats ;

      /// <summary>
      ///   Number of transmitted bytes.
      /// </summary>
      property TransmittedBytes : Int64 read fget_TransmittedBytes;

      /// <summary>
      ///   Resolution of requested image .
      /// </summary>
      /// <value>
      ///   Default 96; 0 means highest possible resolution.
      /// </value>
      property PPI  : Integer read FPPI write FPPI ;

      /// <summary>
      ///   Ignore internal URI for requests from Capabilities.
      /// </summary>
      property IgnoreInternalURI : Boolean read  FIgnoreInternalURI
                                           write FIgnoreInternalURI ;

      /// <summary>
      ///   Service version.
      /// </summary>
      property ServiceVerion : String read fget_ServiceVersion ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Math,

    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoRendererAbstract,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoLayerPNG,
    Lider.CG.GIS.GeoLayerGIF,
    Lider.CG.GIS.GeoLayerJPG,
    Lider.CG.GIS.GeoLayerTIFF,
    Lider.CG.GIS.GeoLayerGRD,
    Lider.CG.GIS.GeoLayerSublayer ;
{$ENDIF}

const
  WCS_FILE_INFO = 'OpenGIS Web Coverage Service (WCS)' ;

//==============================================================================
// TGIS_LayerWCS
//==============================================================================

  constructor TGIS_LayerWCS.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;

    oWCS         := nil ;
    FAllCoverages := TGIS_StringList.Create ;
    FAllCRS      := TGIS_StringList.Create ;
    FAllImageFormats := TGIS_StringList.Create ;
    FForcedCRS   := 0 ;
    FPPI         := 96 ;

    FUserAgent   := GetDefaultUserAgent( 'ttkWP' ) ;
    FProxyUrl    := '' ;
    FIgnoreInternalURI := False ;

    bAxisOrderIgnored  := False ;
    bAxisOrderReversed := False ;
  end ;

  procedure TGIS_LayerWCS.doDestroy ;
  begin
    FreeObject( oImage           ) ;
    FreeObject( oResponse.Stream ) ;

    FreeObject( FAllCoverages    ) ;
    FreeObject( FAllImageFormats ) ;
    FreeObject( FAllCRS          ) ;
    FreeObject( oWCS             ) ;

    inherited ;
  end ;

  procedure TGIS_LayerWCS.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  var
    scs : String ;
  begin
    if assigned( _value ) then begin
      scs := 'EPSG:' + IntToStr( _value.EPSG ) ;
      if assigned( oWCS ) and oWCS.VerifyCRS( scs ) then begin
        oWCS.SelectedCRS := scs ;
        inherited fset_CS( _value ) ;
        Extent := oWCS.GetExtent( oWCS.SelectedCRS ) ;
      end;
    end;
  end ;

  function TGIS_LayerWCS.fget_LastUrl
    : String ;
  begin
    Result := oWCS.LastUrl ;
  end ;

  function TGIS_LayerWCS.fget_ServiceVersion
    : String ;
  begin
    Result := oWCS.ServiceVersion ;
  end ;

  function TGIS_LayerWCS.fget_TransmittedBytes : Int64 ;
  begin
    Result := oWCS.TransmittedBytes ;
  end ;

  procedure TGIS_LayerWCS.parseInternal ;
  var
    lst    : TGIS_StringList;
    stmp   : String         ;
    vstr   : String         ;
    k      : Integer ;
  begin
    if not IsServerPath( Path ) or IsEmbeddedSQLPath( Path ) then
    begin
      lst := TGIS_StringList.Create ;
      try
        ReadParamsFromPath( Path, lst ) ;

        sPath              := LstReadString ( lst,
                                              'Url',
                                              ''
                                            ) ;
        Name               := LstReadString ( lst,
                                              'Caption',
                                              Name
                                            ) ;
        Caption            := LstReadString ( lst,
                                              'Caption',
                                              Caption ) ;
        cInfo              := LstReadString ( lst,
                                              'Info',
                                              ''
                                            ) ;
        cCopyright         := LstReadString ( lst,
                                              'Copyright',
                                              ''
                                            ) ;
        FUserAgent         := LstReadString ( lst,
                                              'UserAgent',
                                              FUserAgent
                                            ) ;
        FProxyUrl          := LstReadString ( lst,
                                              'ProxyUrl',
                                              FProxyUrl
                                            ) ;
        if not IsStringEmpty( FProxyUrl ) then begin
          if FProxyUrl[ StringLast( FProxyUrl ) ] <> '?' then
            FProxyUrl := FProxyUrl + '?' ;
        end ;

        iTimeOut           := LstReadInteger( lst,
                                              'TimeOut',
                                              40000
                                             ) ;
        // obsolate parameter
        stmp               := LstReadString  ( lst,
                                               GIS_INI_AXIS_ORDER,
                                               ''
                                             ) ;
        if not IsStringEmpty( stmp ) then
          bAxisOrderReversed := stmp = GIS_INI_AXIS_ORDER_NE ;

        // new parameter
        bAxisOrderReversed := LstReadBoolean( lst,
                                              GIS_INI_AXIS_ORDER_REVERSED,
                                              bAxisOrderReversed
                                            ) ;

        bAxisOrderIgnored  := LstReadBoolean( lst,
                                              GIS_INI_AXIS_ORDER_IGNORED,
                                              bAxisOrderIgnored
                                            ) ;

        FIgnoreInternalURI := LstReadBoolean( lst,
                                              'IgnoreInternalURI',
                                              False
                                            ) ;
        if IsStringEmpty( sPath )  then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                       'Url=', 0
                                    ) ;
      finally
        FreeObject( lst ) ;
      end ;

      oWCS.TimeOut   := iTimeOut ;
    end
    else begin
      sPath := Path ;
      oWCS.TimeOut := 40000 ;
      bAxisOrderReversed := StrToBoolean(
                              URLGetAndDeleteParameterValue(
                                sPath,
                                GIS_INI_AXIS_ORDER_REVERSED
                              ),
                              bAxisOrderReversed
                            ) ;

      bAxisOrderIgnored := StrToBoolean(
                             URLGetAndDeleteParameterValue(
                                sPath,
                                GIS_INI_AXIS_ORDER_IGNORED
                             ),
                             bAxisOrderIgnored
                           ) ;
    end ;

    k := Pos( '?http://', sPath ) ;
    if k <= StringFirst then
      k := Pos( '?https://', sPath ) ;

    if k > StringFirst then begin
      FProxyUrl := Copy( sPath, StringFirst, k ) ;
      sPath     := Copy( sPath, k+1, 4096 ) ;
    end ;

    oWCS.UserAgent := FUserAgent ;
    oWCS.ProxyUrl  := FProxyUrl ;

    sPath := URLFixed( sPath ) ;

    // old parameter
    vstr := GisMetadataAsString( 'TGIS_LayerWCS.AxisOrder', '' ) ;
    if IsStringEmpty( vstr ) then
      vstr := ReadConfigParam( GIS_INI_AXIS_ORDER ) ;
    if not IsStringEmpty( vstr ) then
      bAxisOrderReversed := vstr = GIS_INI_AXIS_ORDER_NE ;

    // new parameter
    bAxisOrderReversed := GisMetadataAsBoolean(
                            'TGIS_LayerWCS.AxisOrderReversed',
                            bAxisOrderReversed
                          ) ;
    bAxisOrderReversed := StrToBoolean(
                            ReadConfigParam( GIS_INI_AXIS_ORDER_REVERSED ),
                            bAxisOrderReversed
                          ) ;

    bAxisOrderIgnored  := GisMetadataAsBoolean(
                            'TGIS_LayerWCS.AxisOrderIgnored',
                            bAxisOrderIgnored
                          ) ;
    bAxisOrderIgnored  := StrToBoolean(
                            ReadConfigParam( GIS_INI_AXIS_ORDER_IGNORED ),
                            bAxisOrderIgnored
                          ) ;

    oWCS.AxisOrderReversed := bAxisOrderReversed ;
    oWCS.AxisOrderIgnored  := bAxisOrderIgnored  ;
    oWCS.IgnoreInternalURI := FIgnoreInternalURI ;
  end ;

  procedure TGIS_LayerWCS.setLayerInternal ;
  var
    i       : Integer ;
    lw      : TGIS_WCSCoverage ;
    tkn     : TGIS_Tokenizer ;
    lst     : TStringList ;
    itmpcs  : Integer        ;
    stmpcs  : String         ;
    btmpcs  : Boolean        ;
  begin
    if assigned( oWCS.ImageFormats ) then begin
      for i:= 0 to oWCS.ImageFormats.Count - 1 do begin
        case DecodeContentType( oWCS.ImageFormats[i], False ) of
          TGIS_ContentType.Gif   : begin
                                  sImageFormat := GIS_CONTENTTYPE_GIF ;
                                  break ;
                                end ;
          TGIS_ContentType.Jpg   : begin
                                  sImageFormat := GIS_CONTENTTYPE_JPEG ;
                                  break ;
                                end ;
          TGIS_ContentType.Png   : begin
                                  sImageFormat := GIS_CONTENTTYPE_PNG ;
                                  break ;
                                end ;
          TGIS_ContentType.Png24 : begin
                                  sImageFormat := GIS_CONTENTTYPE_PNG24 ;
                                  break ;
                                end ;
          else                  begin
                                  sImageFormat := oWCS.ImageFormats[i] ;
                                end ;

        end ;
      end ;
      FAllImageFormats.Assign( oWCS.ImageFormats ) ;
    end ;

    if not IsStringEmpty( oWCS.PredefinedFormat ) then begin
      case DecodeContentType( oWCS.PredefinedFormat, False ) of
        TGIS_ContentType.Gif   : begin
                                sImageFormat := GIS_CONTENTTYPE_GIF ;
                              end ;
        TGIS_ContentType.Jpg   : begin
                                sImageFormat := GIS_CONTENTTYPE_JPEG ;
                              end ;
        TGIS_ContentType.Png   : begin
                                sImageFormat := GIS_CONTENTTYPE_PNG ;
                              end ;
        TGIS_ContentType.Png24 : begin
                                sImageFormat := GIS_CONTENTTYPE_PNG24 ;
                              end ;
        else                  begin
                                sImageFormat := oWCS.PredefinedFormat ;
                              end ;
      end ;
    end ;

    if not IsStringEmpty( oWCS.Error ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), oWCS.Error, 0 ) ;

    if (oWCS.DescribedSize.X > 0) and (oWCS.DescribedSize.Y > 0) then begin
      FBitWidth  := oWCS.DescribedSize.X ;
      FBitHeight := oWCS.DescribedSize.Y ;
    end
    else if assigned( Viewer ) then begin
      FBitWidth  := Viewer.Ref.ViewerParent.ControlCanvasWidth  ;
      FBitHeight := Viewer.Ref.ViewerParent.ControlCanvasHeight ;
    end ;

    try
      inherited setUp ;
    except
      on e : Exception do
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), e.Message, 0 ) ;
    end ;

    if FForcedCRS > 0 then begin
      oWCS.ForcedCRS := 'EPSG:' + IntToStr( FForcedCRS )  ;
    end ;

    FCS := CSUnknownCoordinateSystem ;

    if IsStringEmpty( oWCS.ForcedCRS ) then begin
      if assigned( Viewer ) then
        lw := oWCS.FindCoverage( oWCS.DefaultCoverage, Viewer.Ref.CS )
      else
        lw := oWCS.FindCoverage( oWCS.DefaultCoverage, nil ) ;

      if assigned( lw ) then begin
        tkn := TGIS_Tokenizer.Create ;
        try
          lst := TStringList.Create ;
          try
            lst.Text := lw.CRS[0] ;
            if lst.Count > 0 then begin
              tkn.Execute( lst[0], [':'] ) ;
            end ;
          finally
            FreeObject( lst ) ;
          end ;
          if tkn.Result.Count > 1 then
            try
              FCS := TGIS_CSFactory.ByEPSG( StrToInt( tkn.Result[ 1 ] ) ) ;
              if lw.CRSBBox.Count > 0 then
                Extent := lw.CRSBBox[0].BoundingBox  ;
              oWCS.SelectedCRS := 'EPSG:' + tkn.Result[ 1 ] ;
            except
              FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
            end;
        finally
          FreeObject( tkn ) ;
        end ;
      end
    end ;

    if FCS is TGIS_CSUnknownCoordinateSystem then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        btmpcs := False ;

        if IsStringEmpty( oWCS.ForcedCRS ) then begin
          for itmpcs := 0 to oWCS.AllCoverages.Count -1 do begin
            stmpcs := oWCS.AllCoverages[ itmpcs ];
            if stmpcs = 'CRS:84' then begin
              FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
              oWCS.SelectedCRS := stmpcs ;
              oWCS.AxisOrderIgnored := True ; // CRS:84 is x,y - don't invert
              btmpcs := True ;
            end
            else begin
              tkn.Execute( stmpcs, [':'] ) ;
              if tkn.Result.Count > 1 then begin
                try
                  FCS := TGIS_CSFactory.ByEPSG( StrToInt( tkn.Result[ 1 ] ) ) ;
                  if FCS.EPSG = Viewer.Ref.CS.EPSG then begin
                    btmpcs := True ;
                    oWCS.SelectedCRS := stmpcs ;
                    break ;
                  end;
                except
                end;
              end ;
            end ;
          end ;
        end
        else begin
          if (Pos( 'CRS:84', oWCS.ForcedCRS ) >= StringFirst) or
             (Pos( 'CRS84' , oWCS.ForcedCRS ) >= StringFirst)  then begin
              FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
              oWCS.SelectedCRS := oWCS.ForcedCRS ;
              btmpcs := True ;
              oWCS.AxisOrderIgnored := True ; // CRS:84 is x,y - don't invert
            end
          else begin
            tkn.Execute( oWCS.ForcedCRS, [':'] ) ;
            if tkn.Result.Count > 1 then begin
              try
                FCS := TGIS_CSFactory.ByEPSG( StrToInt( tkn.Result[ 1 ] ) ) ;
                btmpcs := True ;
                oWCS.SelectedCRS := oWCS.ForcedCRS ;
              except
              end;
            end ;
          end;
        end ;

        if not btmpcs then begin
          tkn.Execute( oWCS.DefaultCRS, [':'] ) ;
          if tkn.Result.Count > 1 then begin
            FCS := TGIS_CSFactory.ByEPSG( StrToInt( tkn.Result[ 1 ] ) ) ;
            oWCS.SelectedCRS := oWCS.DefaultCRS ;
          end
          else
            Abort ;
        end;

      finally
        FreeObject( tkn ) ;
      end ;

    end ;

    if not IsStringEmpty( oWCS.SelectedCRS ) then begin
      Extent := oWCS.GetExtent( oWCS.SelectedCRS ) ;
      if RoundS(scaleX) <> RoundS(-scaleY)  then begin
        if RoundS(scaleX) < RoundS(-scaleY) then begin
          scaleY := -scaleX ;
          FBitHeight :=RoundS( (FExtent.YMax -FExtent.YMin)/scaleX) ;
        end
        else begin
          scaleX := -scaleY ;
          FBitWidth :=RoundS( (FExtent.XMax -FExtent.XMin)/scaleX) ;
        end;
      end;
    end;

    if GisIsNoWorld( Extent ) or ( FCS is TGIS_CSUnknownCoordinateSystem ) then begin
      oWCS.SelectedCRS := 'EPSG:' + IntToStr( GIS_EPSG_WGS84 ) ;
      Extent := oWCS.GetExtent( oWCS.SelectedCRS ) ;
      if GisIsNoWorld( Extent ) then
        Extent := GisExtent( -180,-90,180,90 ) ;
      FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
    end ;

    FAllCRS.Assign( oWCS.AllCRS ) ;
  end ;

  procedure TGIS_LayerWCS.setUp ;
  var
    stmp   : String         ;
  begin
    FreeObject( oWCS ) ; // for potential re-open scenario

    if assigned( Viewer ) then
      oWCS := TGIS_FileWCS.Create( Viewer.Ref, self )
    else
      oWCS := TGIS_FileWCS.Create( nil, self ) ;

    oWCS.PasswordEvent := FOnPassword ;
    try
      parseInternal ;

      oWCS.Load( sPath, nil ) ;
    except
      on ex : Exception do begin
        if FileExists( Path ) then
          stmp := Path
        else if not IsStringEmpty( Name ) then
          stmp := Name
        else if not IsStringEmpty( Caption ) then
          stmp := Name
        else
          stmp := GetClassName( Self ) ;

        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_LAYERBADFORMAT ),
          stmp,
          0,
          ex
        ) ;
      end;
    end ;

    setLayerInternal ;

    FAllCoverages.Assign( oWCS.AllCoverages ) ;

    ReadConfig ;

    FMaxTileSize := Point( 4000, 4000 ) ;

    FFileInfo := WCS_FILE_INFO  + #13#10 + oWCS.ServiceVersion + #13#10 +
                 oWCS.ServiceInfo + #13#10 ;

    if not IsStringEmpty( cInfo ) then
      FFileInfo := #10#13 + cInfo ;
    if not IsStringEmpty( cInfo ) then
      FFileInfo := #10#13 + cCopyright ;
    FFileCopyright := cCopyright ;
  end ;

  function TGIS_LayerWCS.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    w        : Integer         ;
    h        : Integer         ;
    ext      : TGIS_Extent     ;
    vext     : TGIS_Extent     ;
    rpvext   : TGIS_Extent     ;
    uvext    : TGIS_Extent     ;
    upvext   : TGIS_Extent     ;
    ext0     : TGIS_Extent     ;
    ext2     : TGIS_Extent     ;

    lst      : TGIS_StringList ;
    sc       : Double          ;
    img      : TGIS_LayerPixel ;
    msg      : String          ;
    scov     : String          ;
    vscx     : Double ;
  begin
    Result := IsVisible( _extent ) ;
    if not Result then exit ;

    img := nil ;

    scov := oWCS.DefaultCoverage ;

    if IsStringEmpty( scov ) then begin
      Active := False ;
      Result := True ;
      exit ;
    end ;

    Result := False ;

    if (FBitWidth = 0) and (FBitHeight = 0) then begin
      FBitWidth  := Viewer.Ref.ViewerParent.ControlCanvasWidth ;
      FBitHeight := Viewer.Ref.ViewerParent.ControlCanvasHeight ;
    end;

    w := Viewer.Ref.ViewerParent.ControlCanvasWidth  ;

    vext   := Viewer.Ref.VisibleExtent ;
    uvext  := self.UnprojectExtent( vext ) ;
    rpvext := self.ProjectExtent( uvext ) ;
    w := RoundS( w * (( rpvext.XMax - rpvext.XMin )/( vext.XMax -vext.XMin )) ) ;
    if w = 0 then
      w := 1 ;
    vscx  := (uvext.XMax -uvext.XMin)/w ;

    ext0     := GisCommonExtent( Viewer.Ref.VisibleExtent, ProjectedExtent ) ;
    upvext   := self.UnprojectExtent( vext ) ;
    ext2     := self.UnprojectExtent(  ext0 ) ;
    ext      := GisCommonExtent( ext2, Extent ) ;

    if ((ext.XMax - ext.XMin) > 0 ) and ((ext.YMax - ext.YMin) > 0)  then begin
      w := RoundS((ext.XMax - ext.XMin)/vscx) ;
      h := RoundS((ext.YMax - ext.YMin)/vscx) ;

      if FPPI > 0 then begin
        w := w * FPPI div TGIS_RendererAbstract(Renderer).PPI ;
        h := h * FPPI div TGIS_RendererAbstract(Renderer).PPI ;
      end ;
    end
    else begin
      w := 0 ;
      h := 0 ;
    end ;

    if ( w > FMaxTileSize.X ) or ( h > FMaxTileSize.Y ) then begin
      sc := Min( FMaxTileSize.X/w, FMaxTileSize.Y/h ) ;
      w := RoundS( sc * w ) ;
      h := RoundS( sc * h ) ;
    end ;

    if ( w > 10 ) and ( h > 10 ) then begin
      oResponse := oWCS.GetMap( ext, sImageFormat, w, h, scov ) ;

      if assigned( oResponse.Stream ) then begin
        try
          if ( oResponse.Status = GIS_HTTP_FETCH_TIMEOUT      ) or
             ( oResponse.Status = GIS_HTTP_FETCH_NOTCOMPLETED ) then
          begin
            msg := _rsrc( GIS_RS_ERR_SERVER_TIMEOUT ) ;
          end
          else if ( oResponse.Status = GIS_HTTP_NOTFOUND      ) then
            msg := _rsrc( GIS_RS_ERR_SERVER_WRONGURL )
          else if ( oResponse.Status = GIS_HTTP_SERVICEUNAVAILABLE ) then
            msg := _rsrc( GIS_RS_ERR_SERVER_ERROR )
          else if ( oResponse.Status = GIS_HTTP_AUTHORIZATIONREQUIRED ) then
            msg := _rsrc( GIS_RS_ERR_SERVER_AUTHORIZATIONREQ )
          else begin
            try
              case DecodeContentType( oResponse.ContentType, True ) of
                TGIS_ContentType.Gif   : img := TGIS_LayerGIF.Create ;
                TGIS_ContentType.Jpg   : img := TGIS_LayerJPG.Create ;
                TGIS_ContentType.Png   : img := TGIS_LayerPNG.Create ;
                else begin
                  if Pos( 'TIFF', UpperCase( sImageFormat ) ) >= StringFirst then
                    img := TGIS_LayerTIFF.Create
                  else if Pos( 'AAIGrid', sImageFormat ) >= StringFirst then
                    img := TGIS_LayerGRD.Create
                  else
                    Abort ;
                end ;
              end ;

              if assigned( img ) then begin
                img.Renderer := Renderer ;
                img.Stream := oResponse.Stream ;
                img.Open ;
                img.Viewer := Viewer ;
                img.Extent := ext ;
                img.CS := CS ;
                img.RecalcProjectedExtent ;
              end ;
              FreeObject( oImage ) ;
              oImage := img ;
              oImage.AssignedParentLayerInternal( self ) ;
              msg := '' ;
            except
              FreeObject( img ) ;
              oResponse.Stream.Position := 0 ;
              lst := TGIS_StringList.Create ;
              try
                lst.LoadFromStream( oResponse.Stream );
                msg := Format( _rsrc( GIS_RS_ERR_SERVER_ERROR ), [ lst.Text ] ) ;
              finally
                FreeObject( lst ) ;
              end ;
            end ;
          end;
        finally
          FreeObject( oResponse.Stream ) ;
        end ;
      end;

    end ;

    if Active and assigned( oImage ) then begin
      oImage.Renderer := Renderer ;
      FBandsCount := oImage.BandsCount ;
      oImage.Params.Assign( Params );
      oImage.Draw ;
      FMinZ := oImage.MinHeight ;
      FMaxZ := oImage.MaxHeight ;
      Params.Pixel.GridNoValue := oImage.NoDataValue ;
      Result := True ;
      IsGridImage := oImage.IsGridImage ;
    end ;

    FFileInfo := WCS_FILE_INFO  + #13#10 +
                oWCS.ServiceVersion + #13#10 + oWCS.ServiceInfo + #13#10 ;
    if not IsStringEmpty( cInfo ) then
      FFileInfo := WCS_FILE_INFO + cInfo + #13#10#13#10 ;
    FFileInfo := FFileInfo + 'LastUrl:' + #13#10 + LastUrl ;

    if not IsStringEmpty( msg ) then
      raise EGIS_Exception.Create( msg, Path, 0 ) ;
  end ;

  function TGIS_LayerWCS.GetBitmap(
    const _extent   : TGIS_Extent ;
    const _bitmap   : TGIS_Pixels ;
    const _width    : Integer ;
    const _height   : Integer
  ) : Boolean;
  begin
    if assigned( oImage ) then begin
      FBandsCount := oImage.BandsCount ;
      Result := oImage.GetBitmap( _extent, _bitmap, _width, _height ) ;
    end
    else
      Result := False ;
  end ;

  function TGIS_LayerWCS.GetGrid(
    const _extent       : TGIS_Extent      ;
    const _grid         : TGIS_GridArray
  ) : Boolean ;
  begin
    if assigned( oImage ) then begin
      FBandsCount := oImage.BandsCount ;
      Result := oImage.GetGrid( _extent, _grid ) ;
    end
    else
      Result := False ;
  end ;

  function TGIS_LayerWCS.LocateEx(
    const _ptg          : TGIS_Point       ;
    var   _rgbMapped    : TGIS_Color       ;
    var   _nativesVals  : TGIS_DoubleArray ;
    var   _transparency : Boolean          ;
    const _pixelsize    : Double
  ) : Boolean ;
  begin
    if assigned( oImage ) then begin
      Result := oImage.LocateEx( _ptg, _rgbMapped, _nativesVals,
                                 _transparency, _pixelsize ) ;
      if Result then begin
        if assigned(_nativesVals) then begin
          if _nativesVals[high(_nativesVals)] = 0 then
            _transparency := True ;
        end;
      end;
    end
    else
      Result := False ;
  end ;

  procedure TGIS_LayerWCS.Alive ;
  begin
    if assigned( oImage ) then
      oImage.Alive ;
  end ;

  function TGIS_LayerWCS.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  begin
    Result := inherited PreRecognize( _path, _new_path ) and
              (
               ( GetParamFromPath( _path, GIS_INI_LAYERSQL_STORAGE ) = 'WCS' ) or
               ( UpperCase( URLGetParameterValue( _path, 'SERVICE' ) ) = 'WCS' )
              ) ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerWCS.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKWP', GIS_PROTOCOL_LAYER_CONNECTOR,
                   TGIS_LayerWCS, GIS_TTKLAYER_WEB_FILTER,
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Protocol,
                    [ TGIS_RegisteredOperationType.Read
                    ],
                   False
                 ) ;
  end ;

  {$IFNDEF OXYGENE}
    initialization
      GisLayerWCS.SelfRegisterLayer() ;
  {$ENDIF}

{==================================== END =====================================}
end.


