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
  Encapsulation of a OpenGIS WMS Layer.
}

{$IFDEF DCC}
  unit GisLayerWMS ;
  {$HPPEMIT '#pragma link "GisLayerWMS"'}
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
    System.Types,
    System.SysUtils,
    System.Classes,

    GisTypes,
    GisLayerPixel,
    GisFileWMS,
    GisTypesUI,
    GisCsFactory,
    GisCsSystems ;
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
  Unit_GisLayerWMS = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  /// Encapsulation of an OpenGIS WMS layer.
  /// </summary>
  TGIS_LayerWMS = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private // properties internal values
      FUseLayers        : TGIS_StringList ;
      FAllLayers        : TGIS_StringList ;
      FAllSRS           : TGIS_StringList ;
      FAllImageFormats  : TGIS_StringList ;
      FForcedSRS        : Integer ;
      FPPI              : Integer ;
      FIgnoreInternalURI: Boolean ;
      FUserAgent        : String ;
      FProxyUrl         : String ;
    private // properties access routine

      function  fget_LastUrl       : String ;
      function  fget_ServiceVersion : String ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      procedure fset_CS     ( const _value  : TGIS_CSCoordinateSystem
                            ) ; override;

      /// <inheritdoc/>
      procedure setUp       ; override;


    private // other private values
      oWMS               : TGIS_FileWMS ;
      oResponse          : TGIS_HttpResponse ;
      oImage             : TGIS_LayerPixel ;
      sImageFormat       : String  ;
      cInfo              : String  ;
      cCopyright         : String  ;
      sPath              : String  ;
      sFullInfo          : String  ;
      iTimeOut           : Integer ;

      iMaxWidth          : Integer ;
      iMaxHeight         : Integer ;
      bAxisOrderIgnored  : Boolean ;
      bAxisOrderReversed : Boolean ;

    protected

      procedure doDestroy  ; override;

      function fget_TransmittedBytes : Int64;

    public // various public routines

      /// <inheritdoc/>
      constructor Create   ; override;

      /// <inheritdoc/>
      function  DrawEx             ( const _extent  : TGIS_Extent
                                     ) : Boolean ; override;

      /// <inheritdoc/>
      procedure   Alive      ; override;

      /// <inheritdoc/>
      function    GetBitmap      (  const _extent   : TGIS_Extent ;
                                    const _bitmap   : TGIS_Pixels ;
                                    const _width    : Integer ;
                                    const _height    : Integer
                                 ) : Boolean; override;

      /// <inheritdoc/>
      function LocateEx           ( const _ptg          : TGIS_Point       ;
                                    var   _rgbMapped    : TGIS_Color       ;
                                    var   _nativesVals  : TGIS_DoubleArray ;
                                    var   _transparency : Boolean          ;
                                    const _pixelsize    : Double
                                  ) : Boolean ; override;

      /// <summary>
      ///   Get feature info.
      /// </summary>
      /// <param name="_point">
      ///   coordinate
      /// </param>
      /// <returns>
      ///   info text
      /// </returns>
      function    GetFeatureInfo   ( const _point : TGIS_Point
                                   ) : String ;

      /// <inheritdoc/>
      function    PreRecognize     ( const _path     : String ;
                                       var _new_path : String
                                   ) : Boolean ; override;

      /// <summary>
      ///   Get layer legend image.
      /// </summary>
      /// <param name="_layer">
      ///   layer name
      /// </param>
      /// <returns>
      ///   bitmap
      /// </returns>
      function    GetLegendImage  ( const _layer : String
                                   ) : TGIS_Bitmap ;
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
        property LastUrl  : String
                            read fget_LastUrl ;

        /// <summary>
        ///   List of layers to be used for render. If empty then default.
        /// </summary>
        property UseLayers  : TGIS_StringList
                              read FUseLayers ;

        /// <summary>
        ///   EPSG code that will be forced to use with wms request. If 0 then
        ///   first supported SRS will be used.
        /// </summary>
        property ForcedSRS : Integer  read  FForcedSRS
                                      write FForcedSRS ;

        /// <summary>
        ///   List of all found layers names.
        /// </summary>
        property FoundLayers : TGIS_StringList
                               read FAllLayers ;

        /// <summary>
        ///   List of all found SRS.
        /// </summary>
        property FoundSRS : TGIS_StringList
                            read FAllSRS ;

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
        ///   Default 0, means highest possible resolution.
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

    GisResource,
    GisInterfaces,
    GisRtl,
    GisClasses,
    GisFunctions,
    GisInternals,
    GisLayer,
    GisRendererAbstract,
    GisRegistredLayers,
    GisLayerPNG,
    GisLayerGIF,
    GisLayerJPG,
    GisLayerSublayer ;
{$ENDIF}

const
  FILE_INFO = 'OpenGIS Web Map Service (WMS)' ;
  WMS_METADATA_MAXWIDTH  = 'TGIS_LayerWMS.MaxWidth' ;
  WMS_METADATA_MAXHEIGHT = 'TGIS_LayerWMS.MaxHeight' ;

//==============================================================================
// TGIS_LayerWMS
//==============================================================================

  constructor TGIS_LayerWMS.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;

    oWMS         := nil ;
    FUseLayers   := TGIS_StringList.Create ;
    FAllLayers   := TGIS_StringList.Create ;
    FAllSRS      := TGIS_StringList.Create ;
    FAllImageFormats := TGIS_StringList.Create ;
    FForcedSRS   := 0 ;
    FPPI         := 0 ;

    FUserAgent   := GetDefaultUserAgent( 'ttkWP' ) ;
    FProxyUrl    := '' ;
    FIgnoreInternalURI := False ;

    iMaxWidth    := -1 ;
    iMaxHeight   := -1 ;

    bAxisOrderIgnored  := False ;
    bAxisOrderReversed := False ;
  end ;

  procedure TGIS_LayerWMS.doDestroy ;
  begin
    FreeObject( oImage           ) ;
    FreeObject( oResponse.Stream ) ;

    FreeObject( FUseLayers       ) ;
    FreeObject( FAllLayers       ) ;
    FreeObject( FAllImageFormats ) ;
    FreeObject( FAllSRS          ) ;
    FreeObject( oWMS             ) ;

    inherited ;
  end ;

  procedure TGIS_LayerWMS.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  var
    scs : String ;
  begin
    if assigned( _value ) then begin
      if _value.EPSG = CS.EPSG then exit ;
      
      if (oWMS.ServiceVersion >= '1.3.0') and (_value.EPSG = 4326) then
        scs := 'CRS:84'
      else
        scs := 'EPSG:' + IntToStr( _value.EPSG ) ;

      if assigned( oWMS ) and oWMS.VerifySRS( scs ) then begin
        oWMS.SelectedSRS := scs ;
        inherited fset_CS( _value ) ;
        Extent := oWMS.GetExtent( oWMS.SelectedSRS ) ;
      end;
    end;
  end ;

  function TGIS_LayerWMS.fget_LastUrl
    : String ;
  begin
    Result := oWMS.LastUrl ;
  end ;

  function TGIS_LayerWMS.fget_ServiceVersion
    : String ;
  begin
    Result := oWMS.ServiceVersion ;
  end ;

  function TGIS_LayerWMS.fget_TransmittedBytes : Int64 ;
  begin
    Result := oWMS.TransmittedBytes ;
  end ;

  procedure TGIS_LayerWMS.setUp ;
  var
    i,k    : Integer        ;
    tkn    : TGIS_Tokenizer ;
    stmp   : String         ;
    lst    : TGIS_StringList;
    vstr   : String         ;
    lw     : TGIS_WMSLayer  ;
    itmpcs : Integer        ;
    stmpcs : String         ;
    btmpcs : Boolean        ;
    bext   : Boolean        ;
    lv     : TGIS_LayerSublayerPixel  ;

    function fnpos( const _subst : String ; const _string : String ) : Integer ;
    begin
      Result := Pos( UpperCase( _subst ), UpperCase( _string ) ) ;
    end;

    function getLegendImage( const _name : String ) : TGIS_Bitmap ;
    var
      hres   : TGIS_HttpResponse ;
      res    : TGIS_Bitmap ;
    begin
      res := nil ;

      hres := oWMS.GetLegendImage( _name ) ;
      if hres.Status = GIS_HTTP_OK then begin
        try
          res := TGIS_Bitmap.Create ;
          try
            res.LoadFromStream( hres.Stream ) ;
          except
            FreeObject( res ) ;
          end ;
        finally
          FreeObject( hres.Stream ) ;
        end ;
      end
      else
        FreeObject( hres.Stream ) ;

      Result := res ;
    end ;

    procedure parseSublayers( const _root : TGIS_Layer ; const _lw : TGIS_WMSLayer ) ;
    var
      lp : TGIS_LayerSublayerPixel  ;
      s  : Integer ;
      la : TGIS_WMSLayer ;
    begin
      if assigned( _lw.SubLayers ) then begin
        for s := 0 to _lw.SubLayers.Count-1 do begin
          la := TGIS_WMSLayer(_lw.SubLayers[s]) ;

          if FAllLayers.IndexOf( la.Name ) > -1 then continue ;

          lp := TGIS_LayerSublayerPixel.Create ;
          lp.Name             := la.Name ;
          lp.Caption          := la.Title ;
          lp.Viewer           := Viewer ;
          lp.UseParentParams  := False ;
          lp.ParentLayer      := _root ;
          lp.Active           := la.Name = oWMS.DefaultLayer ;
          lp.Params.Style     := la.Style ;

          if not assigned( _root.SubLayers ) then
            _root.SubLayers := TGIS_LayerAbstractList.Create( False ) ;

          _root.SubLayers.Add( lp ) ;
          FAllLayers.Add( la.Name ) ;

          parseSublayers( lp, la ) ;
        end ;
      end ;
    end ;

  begin
    FreeObject( oWMS ) ; // for potential re-open scenario
    if assigned( Viewer ) then
      oWMS := TGIS_FileWMS.Create( Viewer.Ref, self )
    else
      oWMS := TGIS_FileWMS.Create( nil, self ) ;

    oWMS.PasswordEvent := FOnPassword ;
    bext := True ;
    try
      if not IsServerPath( Path ) or IsEmbeddedSQLPath( Path ) then begin
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
          iMaxWidth          := LstReadInteger( lst,
                                                'MaxWidth',
                                                -1
                                               ) ;
          iMaxHeight         := LstReadInteger( lst,
                                                'MaxHeight',
                                                -1
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
          FExtentPixelAdjustment
                             := LstReadBoolean( lst,
                                                GIS_INI_EXTENT_PIXEL_ADJUSTMENT,
                                                False
                                              ) ;
          if IsStringEmpty( sPath )  then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                         'Url=', 0
                                      ) ;
        finally
          FreeObject( lst ) ;
        end ;

        oWMS.TimeOut   := iTimeOut ;
      end
      else begin
        sPath := Path ;
        oWMS.TimeOut := 40000 ;
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
        FExtentPixelAdjustment
                          := StrToBoolean(
                               URLGetAndDeleteParameterValue(
                                  sPath,
                                  GIS_INI_EXTENT_PIXEL_ADJUSTMENT
                               ),
                               False
                             ) ;
      end ;

      k := Pos( '?http://', sPath ) ;
      if k <= StringFirst then
        k := Pos( '?https://', sPath ) ;

      if k > StringFirst then begin
        FProxyUrl := Copy( sPath, StringFirst, k ) ;
        sPath     := Copy( sPath, k+1, 4096 ) ;
      end ;

      oWMS.UserAgent := FUserAgent ;
      oWMS.ProxyUrl  := FProxyUrl ;

      sPath := URLFixed( sPath ) ;

      // old parameter
      vstr := GisMetadataAsString( 'TGIS_LayerWMS.AxisOrder', '' ) ;
      if IsStringEmpty( vstr ) then
        vstr := ReadConfigParam( GIS_INI_AXIS_ORDER ) ;
      if not IsStringEmpty( vstr ) then
        bAxisOrderReversed := vstr = GIS_INI_AXIS_ORDER_NE ;

      // new parameter
      bAxisOrderReversed := GisMetadataAsBoolean(
                              'TGIS_LayerWMS.AxisOrderReversed',
                              bAxisOrderReversed
                            ) ;
      bAxisOrderReversed := StrToBoolean(
                              ReadConfigParam( GIS_INI_AXIS_ORDER_REVERSED ),
                              bAxisOrderReversed
                            ) ;

      bAxisOrderIgnored  := GisMetadataAsBoolean(
                              'TGIS_LayerWMS.AxisOrderIgnored',
                              bAxisOrderIgnored
                            ) ;
      bAxisOrderIgnored  := StrToBoolean(
                              ReadConfigParam( GIS_INI_AXIS_ORDER_IGNORED ),
                              bAxisOrderIgnored
                            ) ;

      oWMS.AxisOrderReversed := bAxisOrderReversed ;
      oWMS.AxisOrderIgnored  := bAxisOrderIgnored  ;
      oWMS.IgnoreInternalURI := FIgnoreInternalURI ;

      oWMS.Load( sPath, nil ) ;
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

    if assigned( oWMS.ImageFormats ) then begin
      for i:= 0 to oWMS.ImageFormats.Count - 1 do begin
        case DecodeContentType( oWMS.ImageFormats[i], False ) of
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
                                  sImageFormat := oWMS.ImageFormats[i] ;
                                end ;

        end ;
      end ;
      FAllImageFormats.Assign( oWMS.ImageFormats ) ;
    end ;

    if not IsStringEmpty( oWMS.PredefinedFormat ) then begin
      case DecodeContentType( oWMS.PredefinedFormat, False ) of
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
                                sImageFormat := oWMS.PredefinedFormat ;
                              end ;
      end ;
    end ;

    if oWMS.PredefinedLayers.Count > 0 then
      self.UseLayers.Assign( oWMS.PredefinedLayers ) ;

    if not IsStringEmpty( oWMS.Error ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), oWMS.Error, 0 ) ;

    if assigned( Viewer ) then begin
      FBitWidth  := Viewer.Ref.ViewerParent.ControlCanvasWidth  ;
      FBitHeight := Viewer.Ref.ViewerParent.ControlCanvasHeight ;
    end ;

    try
      inherited setUp ;
    except
      on e : Exception do
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), e.Message, 0 ) ;
    end ;

    if FForcedSRS > 0 then begin
      oWMS.ForcedSRS := 'EPSG:' + IntToStr( FForcedSRS )  ;
    end ;

    FCS := CSUnknownCoordinateSystem ;

    if IsStringEmpty( oWMS.ForcedSRS ) and ( self.UseLayers.Count > 0 ) then begin
      if assigned( Viewer ) then
        lw := oWMS.FindLayer( self.UseLayers[0], Viewer.Ref.CS )
      else
        lw := oWMS.FindLayer( self.UseLayers[0], nil ) ;

      if assigned( lw ) then begin
        try
          tkn := TGIS_Tokenizer.Create ;
          try
            lst := TStringList.Create ;
            try
              lst.Text := lw.CRS ;
              if lst.Count > 0 then begin
                tkn.Execute( lst[0], [':'] ) ;
              end ;
            finally
              FreeObject( lst ) ;
            end ;
            if tkn.Result.Count > 1 then
              try
                FCS := TGIS_CSFactory.ByEPSG( StrToInt( tkn.Result[ 1 ] ) ) ;
                Extent := lw.CRSBBox  ;
                bext   := False ;
                oWMS.SelectedSRS := 'EPSG:' + tkn.Result[ 1 ] ;
              except
                FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
              end;
          finally
            FreeObject( tkn ) ;
          end ;
        finally
          FreeObject( lw ) ;
        end ;
      end
    end ;

    if FCS is TGIS_CSUnknownCoordinateSystem then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        btmpcs := False ;

        if IsStringEmpty( oWMS.ForcedSRS ) then begin
          for itmpcs := 0 to oWMS.AllSRS.Count -1 do begin
            stmpcs := oWMS.AllSRS[ itmpcs ];
            if stmpcs = 'CRS:84' then begin
              FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
              oWMS.SelectedSRS := stmpcs ;
              oWMS.AxisOrderIgnored := True ; // CRS:84 is x,y - don't invert
              btmpcs := True ;
            end
            else begin
              tkn.Execute( stmpcs, [':'] ) ;
              if tkn.Result.Count > 1 then begin
                try
                  FCS := TGIS_CSFactory.ByEPSG( StrToInt( tkn.Result[ 1 ] ) ) ;
                  if assigned( Viewer ) and (FCS.EPSG = Viewer.Ref.CS.EPSG) then begin
                    btmpcs := True ;
                    oWMS.SelectedSRS := stmpcs ;
                    break ;
                  end;
                except
                end;
              end ;
            end ;
          end ;
        end
        else begin
          if oWMS.ForcedSRS = 'CRS:84' then begin
              FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
              oWMS.SelectedSRS := oWMS.ForcedSRS ;
              btmpcs := True ;
              oWMS.AxisOrderIgnored := True ; // CRS:84 is x,y - don't invert
            end
          else begin
            tkn.Execute( oWMS.ForcedSRS, [':'] ) ;
            if tkn.Result.Count > 1 then begin
              try
                FCS := TGIS_CSFactory.ByEPSG( StrToInt( tkn.Result[ 1 ] ) ) ;
                btmpcs := True ;
                oWMS.SelectedSRS := oWMS.ForcedSRS ;
              except
              end;
            end ;
          end;
        end ;

        if not btmpcs then begin
          tkn.Execute( oWMS.DefaultSRS, [':'] ) ;
          if tkn.Result.Count > 1 then begin
            FCS := TGIS_CSFactory.ByEPSG( StrToInt( tkn.Result[ 1 ] ) ) ;
            oWMS.SelectedSRS := oWMS.DefaultSRS ;
          end
          else
            Abort ;
        end;

      finally
        FreeObject( tkn ) ;
      end ;

    end ;

    if not IsStringEmpty( oWMS.SelectedSRS ) and bext then
      Extent := oWMS.GetExtent( oWMS.SelectedSRS ) ;

    if GisIsNoWorld( Extent ) or ( FCS is TGIS_CSUnknownCoordinateSystem ) then begin
      if oWMS.ServiceVersion >= '1.3.0' then
        oWMS.SelectedSRS := oWMS.DefaultSRS
      else
        oWMS.SelectedSRS := 'EPSG:' + IntToStr( GIS_EPSG_WGS84 ) ;
      Extent := oWMS.GetExtent( oWMS.SelectedSRS ) ;
      if GisIsNoWorld( Extent ) then
        Extent := GisExtent( -180,-90,180,90 ) ;
      FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
    end ;

    FAllSRS.Assign( oWMS.AllSRS ) ;

    if not assigned( SubLayers ) then
      SubLayers := TGIS_LayerAbstractList.Create( False ) ;

    if FUseLayers.Count > 0 then begin
      for i := 0 to oWMS.AllLayers.Count - 1 do begin
        if FUseLayers.IndexOf( oWMS.AllLayers[ i ] ) > -1 then begin
          lv := TGIS_LayerSublayerPixel.Create ;

          lv.Name               := oWMS.AllLayers[ i ]         ;
          lv.Caption            := oWMS.AllLayersCaptions[ i ] ;
          lv.Viewer             := Viewer                      ;
          lv.UseParentParams    := False                       ;
          lv.ParentLayer        := Self                        ;

          lw := oWMS.FindLayer( lv.Name, nil ) ;
          try
            if assigned( lw ) then
              lv.Params.Style := lw.Style ;
          finally
            FreeObject( lw ) ;
          end ;

          SubLayers.Add( lv ) ;
        end ;
        FAllLayers.Add( oWMS.AllLayers[ i ] ) ;
      end ;
    end
    else begin
      lw := oWMS.GetLayers ;
      try
        parseSublayers( Self, lw ) ;
      finally
        FreeObject( lw ) ;
      end ;
    end ;

    ReadConfig ;

    // fetch a legend image only if required
    for i := 0 to SubLayers.Count - 1 do begin
      lv := TGIS_LayerSublayerPixel( SubLayers[i] ) ;

      if lv.Active and lv.Params.Pixel.ShowLegend then
        lv.Params.Pixel.LegendImage := getLegendImage( lv.Name )   ;
    end ;

    iMaxWidth  := GisMetadataAsInteger( WMS_METADATA_MAXWIDTH , -1 );
    iMaxHeight := GisMetadataAsInteger( WMS_METADATA_MAXHEIGHT, -1 );

    if ( iMaxWidth <> -1 ) and ( iMaxHeight <> -1 ) then
      FMaxTileSize := Point( iMaxWidth, iMaxHeight )
    else
      FMaxTileSize := Point( oWMS.MaxWidth, oWMS.MaxHeight ) ;

    sFullInfo := FILE_INFO  + #13#10 + oWMS.ServiceVersion ;
    if not IsStringEmpty( oWMS.ServiceInfo ) then
      sFullInfo := sFullInfo + #13#10 + oWMS.ServiceInfo ;
    if not IsStringEmpty( cInfo ) then
      sFullInfo := sFullInfo + #10#13 + cInfo ;
    if not IsStringEmpty( cInfo ) then
      sFullInfo := sFullInfo + #10#13 + cCopyright ;

    FFileInfo      := sFullInfo ;
    FFileCopyright := cCopyright ;
  end ;


  function TGIS_LayerWMS.DrawEx(
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
    slays    : TGIS_StringList ;
    sstyles  : TGIS_StringList ;
    vscx     : Double          ;
    ippi     : Integer         ;

    procedure findActiveSublayers( const _layer : TGIS_Layer ) ;
    var
      s, i : Integer ;
      la   : TGIS_Layer ;
      badd : Boolean ;
    begin
      if assigned( _layer.SubLayers ) then begin
        badd := False ;
        for s := 0 to _layer.SubLayers.Count-1 do begin
          la := TGIS_Layer(_layer.SubLayers[s]) ;
          if la.Active and ( la.Name <> '' ) then begin
            slays.Add( la.Name ) ;
            sstyles.Add( la.Params.Style ) ;
            if _layer.Active then begin
              i := slays.IndexOf( _layer.Name ) ;
              if i > -1 then begin
                slays.Delete( i ) ;
                badd := True ;
              end ;
              i := sstyles.IndexOf( _layer.Params.Style ) ;
              if i > -1 then
                sstyles.Delete( i ) ;
            end ;
          end ;
          findActiveSublayers( la ) ;
        end;
        if badd and ( slays.Count = 0 ) then
          slays.Add( _layer.Name ) ;
      end ;
    end ;

  begin
    Result := IsVisible( _extent ) ;
    if not Result then exit ;

    slays := TGIS_StringList.Create ;
    sstyles := TGIS_StringList.Create ;
    try
      img := nil ;

      slays.Assign( FAllLayers );
      if assigned( SubLayers ) then begin
        slays.Clear ;
        findActiveSublayers( self ) ;
      end ;

      if slays.Count <= 0 then begin
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
      w := RoundS( w * ( ( rpvext.XMax - rpvext.XMin )/
                         ( vext.XMax -vext.XMin )
                       )
                 ) ;
      if w = 0 then
        w := 1 ;
      vscx  := (uvext.XMax -uvext.XMin)/w ;

      ext0  := GisCommonExtent( Viewer.Ref.VisibleExtent, ProjectedExtent ) ;
      upvext := self.UnprojectExtent( vext ) ;
      ext2 := self.UnprojectExtent(  ext0 ) ;
      ext  := GisCommonExtent( ext2, Extent ) ;

      if ((ext.XMax - ext.XMin) > 0 ) and ((ext.YMax - ext.YMin) > 0)  then begin
        w := RoundS((ext.XMax - ext.XMin)/vscx) ;
        h := RoundS((ext.YMax - ext.YMin)/vscx) ;

        ippi := FPPI ;
        if ippi = 0 then
          ippi := TGIS_RendererAbstract(Renderer).PPI ;

        if ippi > 0 then begin
          w := w * ippi div TGIS_RendererAbstract(Renderer).PPI ;
          h := h * ippi div TGIS_RendererAbstract(Renderer).PPI ;
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
        oResponse := oWMS.GetMap( ext, sImageFormat, w, h, slays, sstyles ) ;

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
                  TGIS_ContentType.Png   : img := TGIS_LayerPNG.Create
                  else                     Abort ;
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
//?        oImage.workLayerList := oLayerList ;
        oImage.ExtentPixelAdjustment := FExtentPixelAdjustment ;
        oImage.Draw ;
        Result := True ;
      end ;

      FFileInfo := sFullInfo + #13#10 + 'LastUrl:' + #13#10 + LastUrl ;

      if not IsStringEmpty( msg ) then
        raise EGIS_Exception.Create( msg, Path, 0 ) ;
    finally
      FreeObject( slays   ) ;
      FreeObject( sstyles ) ;
    end;
  end ;

  function TGIS_LayerWMS.GetBitmap(
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

  function TGIS_LayerWMS.LocateEx(
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

  function TGIS_LayerWMS.GetFeatureInfo(
    const _point : TGIS_Point
  ) : String ;
  var
    w     : Integer      ;
    h     : Integer      ;
    ext   : TGIS_Extent  ;
    ext2  : TGIS_Extent  ;

    sc    : Double       ;
    pt    : TPoint   ;
    buf   : TBytes       ;
    r     : TGIS_HttpResponse ;
    ptoff : TPoint   ;

    procedure findActiveSublayers( const _layer : TGIS_Layer ) ;
    var
      s, i : Integer ;
      la   : TGIS_Layer ;
      badd : Boolean ;
    begin
      if assigned( _layer.SubLayers ) then begin
        badd := False ;
        for s := 0 to _layer.SubLayers.Count-1 do begin
          la := TGIS_Layer(_layer.SubLayers[s]) ;
          if la.Active and ( la.Name <> '' ) then begin
            FUseLayers.Add( la.Name ) ;
            if _layer.Active then begin
              i := FUseLayers.IndexOf( _layer.Name ) ;
              if i > -1 then begin
                FUseLayers.Delete( i ) ;
                badd := True ;
              end ;
            end ;
          end ;
          findActiveSublayers( la ) ;
        end ;
        if badd and ( FUseLayers.Count = 0 ) then
          FUseLayers.Add( _layer.Name ) ;
      end;
    end ;

  begin
    w := Viewer.Ref.ViewerParent.ControlCanvasWidth  ;
    h := Viewer.Ref.ViewerParent.ControlCanvasHeight ;

    pt   := Viewer.Ref.MapToScreen( _point ) ;

    // correct pixel position based for sitution when layer extent is not
    // fully encomapssed by visible extent
    ext := self.ProjectedExtent ;
    ext := GisCommonExtent( ext, Viewer.Ref.VisibleExtent ) ;

    ptoff := Viewer.Ref.MapToScreen( GisPoint( ext.XMin, ext.YMax ) ) ;

    pt.X := pt.X - ptoff.X ;
    pt.Y := pt.Y - ptoff.Y ;

    // calculate width/height for situation when layer extent is not
    // fully encomapsseed by visible extent
    ext2 := self.UnprojectExtent(  Viewer.Ref.VisibleExtent ) ;
    ext  := GisCommonExtent( ext2, Extent ) ;

    if ((ext.XMax - ext.XMin) > 0 ) and ((ext.YMax - ext.YMin) > 0)  then begin
      w := RoundS( w * (ext.XMax - ext.XMin ) / (ext2.XMax - ext2.XMin) ) ;
      h := RoundS( h * (ext.YMax - ext.YMin ) / (ext2.YMax - ext2.YMin) ) ;
    end
    else begin
      w := 0 ;
      h := 0 ;
    end ;

    if (w > FMaxTileSize.X) or (h > FMaxTileSize.Y) then begin
      sc := Min( FMaxTileSize.X/w, FMaxTileSize.Y/h ) ;
      w := RoundS( sc * w ) ;
      h := RoundS( sc * h ) ;
    end ;

    if ( w > 10 ) and ( h > 10 ) then begin
      if assigned( SubLayers ) then begin
        FUseLayers.Clear ;
        findActiveSublayers( Self ) ;
      end ;

      r.Stream := nil ;
      try
        r := oWMS.GetFeatureInfo( ext, sImageFormat,
                                  w, h, FUseLayers,
                                 '', 1, pt.X, pt.Y
                                ) ;
        if r.Status = GIS_HTTP_OK then begin
            SetLength( buf, r.Stream.Size ) ;
            if r.Stream.Size <= 0  then
              Result := ''
            else begin
              {$IFDEF OXYGENE}
                r.Stream.Read( buf, r.Stream.Size ) ;
              {$ELSE}
                r.Stream.Read( buf[0], r.Stream.Size ) ;
              {$ENDIF}
              try
                Result := TEncoding.UTF8.GetString( buf ) ;
              except
                Result := TEncoding.ASCII.GetString( buf ) ;
              end;
            end ;
        end ;
      finally
        FreeObject( r.Stream ) ;
      end ;
    end ;
  end ;

  function TGIS_LayerWMS.GetLegendImage(
    const _layer : String
  ) : TGIS_Bitmap ;
  var
    res  : TGIS_HttpResponse ;
  begin
    Result := nil ;
    res := oWMS.GetLegendImage( _layer ) ;

    if res.Status = GIS_HTTP_OK then begin
      Result := TGIS_Bitmap.Create ;
      try
        Result.LoadFromStream( res.Stream ) ;
      finally
        FreeObject( res.Stream ) ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerWMS.Alive ;
  begin
    if assigned( oImage ) then
      oImage.Alive ;
  end ;

  function TGIS_LayerWMS.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  begin
    Result := inherited PreRecognize( _path, _new_path ) and
              (
               ( GetParamFromPath( _path, GIS_INI_LAYERSQL_STORAGE ) = 'WMS' ) or
               ( UpperCase( URLGetParameterValue( _path, 'SERVICE' ) ) = 'WMS' )
              ) ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerWMS.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKWP', GIS_PROTOCOL_LAYER_CONNECTOR,
                   TGIS_LayerWMS, GIS_TTKLAYER_WEB_FILTER,
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Protocol,
                    [ TGIS_RegisteredOperationType.Read
                    ],
                   False
                 ) ;
  end ;

  {$IFNDEF OXYGENE}
    initialization
      Unit_GisLayerWMS.SelfRegisterLayer() ;
  {$ENDIF}

{==================================== END =====================================}
end.


