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
  Support for importing Qgis project files (version 2.x).
}

{$IFDEF DCC}
  unit GisProjectQgis ;
  {$HPPEMIT '#pragma link "GisProjectQgis"'}
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
    System.Collections.Generic,

    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisViewer ;
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
  ///   Class for importing Qgis projects (version 2.x).
  /// </summary>
  TGIS_ProjectQgis = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      oViewer : TGIS_Viewer ;
    private

      /// <summary>
      ///   Import project form Qgis project.
      /// </summary>
      /// <param name="_path">
      ///   project file to be opened
      /// </param>
      procedure   importQgis        ( const _path   : String
                                    ) ;
    public

      /// <summary>
      ///   Construct an instance based on provided viewer object.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer which will be associated with this object
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BAD_CALL  if the _viewer is not assigned.
      /// </exception>
      constructor Create            ( const _viewer : TGIS_Viewer
                                    ) ; overload;

      /// <summary>
      ///   Import project form Qgis projects.
      /// </summary>
      /// <param name="_path">
      ///   project file to be opened
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD not existing file.
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEEXTENSION unknown file type.
      /// </exception>
      procedure   Import            ( const _path : String
                                    ) ;
  end ;

//#############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.SysUtils,
    System.Generics.Collections,
    System.Variants,
    System.Classes,
    GisRtl,
    GisFunctions,
    GisInternals,
    GisTypes,
    GisTypesUI,
    GisClasses,
    GisResource,
    GisRegistredLayers,
    GisLayer,
    GisLayerVector,
    GisCompression,
    GisLayerPixel,
    GisParams,
    GisXmlDoc ;
{$ENDIF}

type

  T_srs = class
    private
      fsrid : Integer ;
    protected

      /// <summary>
      ///   Parse xml structure of srs.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure parseSrs( const _node : IXMLNode ) ;
    public
      property Srid : Integer read fsrid ;
  end ;

  T_mapcanvas = class( T_srs )
    private
      funits   : String ;
      fextent  : TGIS_Extent ;
    private

      /// <summary>
      ///   Parse xml structure of extent.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure parseExtent( const _node : IXMLNode ) ;
    public

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property Units  : String      read funits ;
      property Extent : TGIS_Extent read fextent ;
  end ;

  T_legendlayer = class
    private
      fdrawingorder : Integer ;
      fopen         : Boolean ;
      fchecked      : Boolean ;
      fname         : String ;
      flayerid      : String ;
    public

      // Read xml into structure.
      // _node xml node with data
      procedure readXML( const _node : IXMLNode ) ;
    public
      property DrawingOrder : Integer read fdrawingorder ;
      property Open         : Boolean read fopen ;
      property Checked      : Boolean read fchecked ;
      property Name         : String  read fname ;
      property LayerId      : String  read flayerid ;
  end ;

  T_legend = class
    private
      {$IFDEF DCC}
        flegendlayers : TObjectList<T_legendlayer> ;
      {$ELSE}
        flegendlayers : TList<T_legendlayer> ;
      {$ENDIF}
    public
      constructor Create ;
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destroy aliases list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Parse xml structure of extent.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      {$IFDEF DCC}
        property LegendLayers : TObjectList<T_legendlayer>
                                read flegendlayers ;
      {$ELSE}
        property LegendLayers : TList<T_legendlayer>
                                read flegendlayers ;
      {$ENDIF}
  end ;

  T_symbol = class
    private
      ftype             : String ;
      fname             : String ;
      ffillcolor        : TGIS_Color ;
      ffillpattern      : TGIS_BrushStyle ;
      foutlinecolor     : TGIS_Color ;
      fstyle            : TGIS_PenStyle ;
      foutlinestyle     : TGIS_PenStyle ;
      foutlinewidth     : Double ;
      foutlinewidthunit : String ;
      fpointsymbol      : String ;
      fpointsize        : Double ;
      fpointsizeunit    : String ;
      fangle            : Double ;
      fwidth            : Double ;
      fwidthunit        : String ;
      falpha            : Double ;
    private

      /// <summary>
      ///   Convert style.
      /// </summary>
      /// <param name="_style">
      ///   style name
      /// </param>
      /// <returns>
      ///   style type
      /// </returns>
      function convertStyle  ( const _style   : String ) : TGIS_PenStyle ;

      /// <summary>
      ///   Convert pattern.
      /// </summary>
      /// <param name="_pattern">
      ///   pattern name
      /// </param>
      /// <returns>
      ///   pattern type
      /// </returns>
      function convertPattern( const _pattern : String ) : TGIS_BrushStyle ;

      /// <summary>
      ///   Parse color definition.
      /// </summary>
      /// <param name="_color">
      ///   color definition
      /// </param>
      /// <returns>
      ///   color
      /// </returns>
      function parseColor    ( const _color   : String ) : TGIS_Color ;
    public

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property Name             : String read fname ;
      property SymbolType       : String read ftype ;
      property FillColor        : TGIS_Color read ffillcolor ;
      property FillPattern      : TGIS_BrushStyle read ffillpattern ;
      property OutlineColor     : TGIS_Color read foutlinecolor ;
      property OutlineStyle     : TGIS_PenStyle read foutlinestyle ;
      property OutlineWidth     : Double read foutlinewidth ;
      property OutlineWidthUnit : String read foutlinewidthunit ;
      property Style            : TGIS_PenStyle read fstyle ;
      property PointSymbol      : String read fpointsymbol ;
      property PointSize        : Double read fpointsize ;
      property PointSizeUnit    : String read fpointsizeunit ;
      property Angle            : Double read fangle ;
      property Width            : Double read fwidth ;
      property WidthUnit        : String read fwidthunit ;
  end ;

  T_category = class
    private
      fsymbol : String ;
      fvalue  : String ;
      flabel  : String ;
    public

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property Symbol : String read fsymbol ;
      property Value  : String read fvalue ;
      property &Label : String read flabel ;
  end ;

  T_range = class
    private
      fsymbol : String ;
      flower  : String ;
      fupper  : String ;
      flabel  : String ;
    public

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property Symbol : String read fsymbol ;
      property Lower  : String read flower ;
      property Upper  : String read fupper ;
      property &Label : String read flabel ;
  end ;

  T_rule = class
    private
      fscalemin   : Integer ;
      fscalemax   : Integer ;
      ffilter     : String ;
      fsymbol     : String ;
      flabel      : String ;
      {$IFDEF DCC}
        frules      : TObjectList<T_rule> ;
      {$ELSE}
        frules : TList<T_rule> ;
      {$ENDIF}      
    public
      constructor Create ; overload ;
      constructor Create( const _scaleMin : Integer ;
                          const _scaleMax : Integer ;
                          const _filter   : String ;
                          const _symbol   : String ;
                          const _label    : String
                         ) ; overload ;
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destroy rules list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
      function  ParseRules : TObjectList<T_rule> ;
    public
      property Symbol   : String  read fsymbol ;
      property Filter   : String  read ffilter ;
      property &Label   : String  read flabel ;
      property ScaleMin : Integer read fscalemin ;
      property ScaleMax : Integer read fscalemax ;
  end ;

  T_QStyle = class
    private
      ftype       : String ;
      fattr       : String ;
      fsymbols    : TDictionary<String, T_symbol> ;
      fcategories : TObjectList<T_category> ;
      franges     : TObjectList<T_range> ;
      frules      : TObjectList<T_rule> ;
    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_type">
      ///   style type
      /// </param>
      constructor Create( const _type : String ) ;
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destroy aliases list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property StyleType  : String read ftype ;
      property Attribute  : String read fattr ;
      property Symbols    : TDictionary<String, T_symbol> read fsymbols ;
      property Categories : TObjectList<T_category> read fcategories ;
      property Ranges     : TObjectList<T_range> read franges ;
      property Rules      : TObjectList<T_rule> read frules ;
  end ;

  T_labelattrs = class
    private
      ffieldname  : String ;
      ffamily     : String ;
      fsize       : Double ;
      fsizeunit   : String ;
      fbold       : Boolean ;
      fitalic     : Boolean ;
      funderline  : Boolean ;
      fstrikeout  : Boolean ;
      fcolor      : TGIS_Color ;
      fangle      : Double ;
      falignment  : String ;
      fenabled    : Boolean ;
      fdisplayall : Boolean ;
      fplacement  : Integer ;
      fplacementf : Integer ;
      fplacementq : Integer ;
      fdrawback   : Boolean ;
      fbackcolor  : TGIS_Color ;
      fbackocolor : TGIS_Color ;
      fbackborder : Double ;
      fdrawshadow : Boolean ;
      fshadowcolor: TGIS_Color ;
      fscalemin   : Integer ;
      fscalemax   : Integer ;
      fscalevis   : Boolean ;
      frules      : Boolean ;
    public

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property FieldName              : String  read ffieldname ;
      property Family                 : String  read ffamily ;
      property Size                   : Double  read fsize ;
      property SizeUnit               : String  read fsizeunit ;
      property Bold                   : Boolean read fbold ;
      property Italic                 : Boolean read fitalic ;
      property Underline              : Boolean read funderline ;
      property Strikeout              : Boolean read fstrikeout ;
      property Color                  : TGIS_Color  read fcolor ;
      property Angle                  : Double  read fangle ;
      property Alignment              : String  read falignment ;
      property Enabled                : Boolean read fenabled ;
      property DisplayAll             : Boolean read fdisplayall ;
      property Placement              : Integer read fplacement ;
      property PlacementFlags         : Integer read fplacementf ;
      property PlacementQuad          : Integer read fplacementq ;
      property DrawBackground         : Boolean read fdrawback ;
      property BackgroundColor        : TGIS_Color  read fbackcolor ;
      property BackgroundBorder       : Double  read fbackborder ;
      property BackgroundOutlineColor : TGIS_Color  read fbackocolor ;
      property DrawShadow             : Boolean read fdrawshadow ;
      property ShadowColor            : TGIS_Color  read fshadowcolor ;
      property ScaleMin               : Integer read fscalemin ;
      property ScaleMax               : Integer read fscalemax ;
      property ScaleVisibility        : Boolean read fscalevis ;
  end ;

  T_labelRuleAttrs = class
    private
      fdesc       : String ;
      ffilter     : String ;
      flabelattrs : T_labelattrs ;
    public
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destroy aliases list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}
      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property Description      : String read fdesc ;
      property Filter           : String read ffilter ;
      property LabelAttributes  : T_labelattrs read flabelattrs ;
  end ;

  T_colorramp = class
    private
      fvalue : Double ;
      flabel : String ;
      fcolor : TGIS_Color ;
    public

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property Value  : Double read fvalue ;
      property &Label : String read flabel ;
      property Color  : TGIS_Color read fcolor ;
  end ;

  T_shader = class
    private
      fcolorRampType : String ;
      fcolorramps  : TObjectList<T_colorramp> ;
    public
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destroy aliases list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property ColorRampType : String read fcolorRampType ;
      property ColorRamps  : TObjectList<T_colorramp>
                             read fcolorramps ;
  end ;

  T_raster = class
    private
      ftype       : String ;
      fopacity    : Double ;
      falphaband  : Integer ;
      fblueband   : Integer ;
      fgreenband  : Integer ;
      fredband    : Integer ;
      fbrightness : Integer ;
      fcontrast   : Integer ;
      fgrayscale  : Boolean ;
      fhistogram  : Boolean ;
      fshader     : T_shader ;
      fclassmin   : Double ;
      fclassmax   : Double ;
    public
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destroy aliases list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property RasterType : String    read ftype ;
      property Opacity    : Double    read fopacity ;
      property AlphaBand  : Integer   read falphaband ;
      property BlueBand   : Integer   read fblueband ;
      property GreenBand  : Integer   read fgreenband ;
      property RedBand    : Integer   read fredband ;
      property Brightness : Integer   read fbrightness ;
      property Contrast   : Integer   read fcontrast ;
      property GrayScale  : Boolean   read fgrayscale ;
      property Histogram  : Boolean   read fhistogram ;
      property Shader     : T_shader  read fshader ;
      property ClassMin   : Double    read fclassmin ;
      property ClassMax   : Double    read fclassmax ;
  end ;

  T_maplayer = class( T_srs )
    private
      ftype         : String ;
      fid           : String ;
      fdatasource   : String ;
      fpath         : String ;
      fscope        : String ;
      flayername    : String ;
      fstyle        : T_QStyle ;
      fminscale     : Double ;
      fmaxscale     : Double ;
      fgeometry     : String ;
      flabelfield   : String ;
      fhaslabel     : Boolean ;
      flabelattrs   : T_labelattrs ;
      ftransparency : Integer ;
      fraster       : T_raster ;
      fhaslabelrules: Boolean ;
      {$IFDEF DCC}
        flabelrules : TObjectList<T_labelRuleAttrs> ;
      {$ELSE}
        flabelrules : TList<T_labelRuleAttrs> ;
      {$ENDIF}
    private

      // Parse datasource into path and scope.
      procedure parseDatasource ;
    public
      {$IFNDEF OXYGENE}
        destructor  Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property LayerType    : String        read ftype ;
      property Id           : String        read fid ;
      property DataSource   : String        read fdatasource ;
      property LayerName    : String        read flayername ;
      property Style        : T_QStyle      read fstyle ;
      property Scope        : String        read fscope ;
      property Path         : String        read fpath ;
      property MinScale     : Double        read fminscale ;
      property MaxScale     : Double        read fmaxscale ;
      property Geometry     : String        read fgeometry ;
      property LabelField   : String        read flabelfield ;
      property HasLabel     : Boolean       read fhaslabel ;
      property LabelAttrs   : T_labelattrs  read flabelattrs ;
      property Transparency : Integer       read ftransparency ;
      property Raster       : T_raster      read fraster ;
      property HasLabelRules: Boolean       read fhaslabelrules ;
      {$IFDEF DCC}
        property LabelRules   : TObjectList<T_labelRuleAttrs> read flabelrules ;
      {$ELSE}
        property LabelRules   : TList<T_labelRuleAttrs> read flabelrules ;
      {$ENDIF}
  end ;

  T_projectlayers = class
    private
      flayers : TDictionary<String, T_maplayer> ;
    public
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destroy aliases list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property Layers : TDictionary<String, T_maplayer> read flayers ;
  end ;

  T_Qgis = class
    private
      fprojectlayers : T_projectlayers ;
      fmapcanvas     : T_mapcanvas ;
      flegend        : T_legend ;
    public
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destroy aliases list.
        /// </summary>
        destructor  Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Read xml into structure.
      /// </summary>
      /// <param name="_node">
      ///   xml node with data
      /// </param>
      procedure readXML( const _node : IXMLNode ) ;
    public
      property MapCanvas     : T_mapcanvas     read fmapcanvas ;
      property ProjectLayers : T_projectlayers read fprojectlayers ;
      property Legend        : T_legend        read flegend ;
  end ;

//==============================================================================
// T_srs
//==============================================================================

  procedure T_srs.parseSrs(
    const _node : IXMLNode
  ) ;
  var
    i     : Integer ;
    nd    : IXMLNode ;
    srs   : IXMLNode ;
  begin
    assert( _node <> nil ) ;

    fsrid := -1 ;
    srs := _node.ChildNodes.FindNode( 'spatialrefsys' ) ;
    if srs = nil then exit ;

    for i := 0 to srs.ChildNodes.Count-1 do begin
      nd := srs.ChildNodes[i] ;
      if nd.NodeName = 'srid' then
        fsrid := StrToInt( nd.Text ) ;
    end ;

  end ;

//==============================================================================
// T_mapcanvas
//==============================================================================

  procedure T_mapcanvas.parseExtent(
    const _node : IXMLNode
  ) ;
  var
    i     : Integer ;
    nd    : IXMLNode ;
    xmin  : Double ;
    ymin  : Double ;
    xmax  : Double ;
    ymax  : Double ;
  begin
    assert( _node <> nil ) ;

    xmin := 1 ;
    ymin := 1 ;
    xmax := -1 ;
    ymax := -1 ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'xmin' then
        xmin := DotStrToFloat( nd.Text )
      else if nd.NodeName = 'ymin' then
        ymin := DotStrToFloat( nd.Text )
      else if nd.NodeName = 'xmax' then
        xmax := DotStrToFloat( nd.Text )
      else if nd.NodeName = 'ymax' then
        ymax := DotStrToFloat( nd.Text )
    end ;

    fextent := GisExtent( xmin, ymin, xmax, ymax ) ;
  end ;

  procedure T_mapcanvas.readXML(
    const _node : IXMLNode
  ) ;
  begin
    assert( _node <> nil ) ;

    funits  := _node.ChildNodes.FindNode( 'units' ).Text ;
    parseExtent( _node.ChildNodes.FindNode( 'extent' ) ) ;
    parseSrs( _node.ChildNodes.FindNode( 'destinationsrs' ) ) ;
  end ;

//==============================================================================
// T_legend
//==============================================================================

  constructor T_legend.Create ;
  begin
    inherited Create ;

    {$IFDEF DCC}
      flegendlayers := TObjectList<T_legendlayer>.Create( True ) ;
    {$ELSE}
      flegendlayers := TList<T_legendlayer>.Create ;
    {$ENDIF}
  end ;

  {$IFNDEF OXYGENE}
    destructor T_legend.Destroy ;
    begin
      FreeObject( flegendlayers ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_legend.readXML(
    const _node : IXMLNode
  ) ;
  var
    i  : Integer ;
    nd : IXMLNode ;
    la : T_legendlayer ;
  begin
    assert( _node <> nil ) ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'legendlayer' then begin
        la := T_legendlayer.Create ;
        try
          la.readXML( nd ) ;
          flegendlayers.Add( la ) ;
        except
          FreeObject( la ) ;
        end ;
      end
      else if nd.NodeName = 'legendgroup' then begin
        readXML( nd ) ;
      end ;
    end ;
  end ;

//==============================================================================
// T_legendlayer
//==============================================================================

  procedure T_legendlayer.readXML(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    assert( _node <> nil ) ;

    fdrawingorder := VarToInt32(_node.Attributes['drawingOrder']) ;
    fopen         := VarToBoolean(_node.Attributes['open']) ;
    fchecked      := VarToString(_node.Attributes['checked']) = 'Qt::Checked' ;
    fname         := VarToString(_node.Attributes['name']) ;

    nd := _node.ChildNodes.FindNode('filegroup') ;
    if nd = nil then exit ;
    nd := nd.ChildNodes.FindNode('legendlayerfile') ;
    if nd = nil then exit ;
    flayerid := VarToString(nd.Attributes['layerid']) ;
  end ;

//==============================================================================
// T_symbol
//==============================================================================

  function T_symbol.convertStyle(
    const _style   : String
  ) : TGIS_PenStyle ;
  begin
    if      (_style = 'NoPen' ) or (_style = 'no' ) then
      Result := TGIS_PenStyle.Clear
    else if (_style = 'SolidLine') or (_style = 'solid' ) then
      Result := TGIS_PenStyle.Solid
    else if (_style = 'DashLine') or (_style = 'dash' ) then
      Result := TGIS_PenStyle.Dash
    else if (_style = 'DotLine') or (_style = 'dot' )  then
      Result := TGIS_PenStyle.Dot
    else if (_style = 'DashDotLine') or (_style = 'dash dot' ) then
      Result := TGIS_PenStyle.DashDot
    else if (_style = 'DashDotDotLine') or (_style = 'dash dot dot' ) then
      Result := TGIS_PenStyle.DashDotDot
    else if (_style = 'MPenStyle') then
      Result := TGIS_PenStyle.Clear
    else
      Result := TGIS_PenStyle.Solid ;
  end ;

  function T_symbol.convertPattern(
    const _pattern : String
  ) : TGIS_BrushStyle ;
  begin
    if      (_pattern = 'NoBrush') or (_pattern = 'no') then
      Result := TGIS_BrushStyle.Clear
    else if (_pattern = 'SolidPattern') or (_pattern = 'solid') then
      Result := TGIS_BrushStyle.Solid
    else if (_pattern = 'HorPattern') or (_pattern = 'horizontal') then
      Result := TGIS_BrushStyle.Horizontal
    else if (_pattern = 'VerPattern') or (_pattern = 'vertical') then
      Result := TGIS_BrushStyle.Vertical
    else if (_pattern = 'FDiagPattern') or (_pattern = 'f_diagonal') then
      Result := TGIS_BrushStyle.FDiagonal
    else if (_pattern = 'BDiagPattern') or (_pattern = 'b_diagonal') then
      Result := TGIS_BrushStyle.BDiagonal
    else if (_pattern = 'CrossPattern') or (_pattern = 'cross') then
      Result := TGIS_BrushStyle.Cross
    else if (_pattern = 'DiagCrossPattern') or (_pattern = 'diagonal_x') then
      Result := TGIS_BrushStyle.DiagCross
    else
      Result := TGIS_BrushStyle.Solid ;
  end ;

  function T_symbol.parseColor(
    const _color : String
  ) : TGIS_Color ;
  var
    tkn : TGIS_Tokenizer ;
    a   : Integer ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( _color, ',' ) ;
      if tkn.Result.Count = 4 then
        a := StrToInt(tkn.Result[3])
      else
        a := 255 ;
      if tkn.Result.Count >= 3 then
        Result := TGIS_Color.FromARGB(
                    Byte(TruncS(falpha*a)),
                    StrToInt(tkn.Result[0]),
                    StrToInt(tkn.Result[1]),
                    StrToInt(tkn.Result[2])
                  )
      else
        Result := TGIS_Color.Black ;
    finally
      FreeObject( tkn ) ;
    end ;

  end ;

  procedure T_symbol.readXML(
    const _node : IXMLNode
  ) ;
  var
    nd   : IXMLNode ;
    prop : String ;
    val  : String ;
    i,j  : Integer ;
  begin
    assert( _node <> nil ) ;

    fname  := VarToString( _node.Attributes['name'] ) ;
    ftype  := VarToString( _node.Attributes['type'] ) ;
    falpha := DotStrToFloat( VarToString( _node.Attributes['alpha'] ) ) ;
    {$IFDEF GIS_NORECORDS}
      ffillcolor    := TGIS_Color.create;
      foutlinecolor := TGIS_Color.create;
    {$ENDIF}

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'layer' then begin
        for j := 0 to nd.ChildNodes.Count-1 do begin
          prop := VarToString( nd.ChildNodes[j].Attributes['k'] ) ;
          val  := VarToString( nd.ChildNodes[j].Attributes['v'] ) ;
          if prop = 'angle' then
            fangle := DotStrToFloat( val )
          else if prop = 'color' then
            ffillcolor := parseColor( val )
          else if prop = 'line_color' then
            ffillcolor := parseColor( val )
          else if prop = 'color_border' then
            foutlinecolor := parseColor( val )
          else if prop = 'name' then
            fpointsymbol := val
          else if prop = 'outline_color' then
            foutlinecolor := parseColor( val )
          else if prop = 'outline_style' then
            foutlinestyle := convertStyle( val )
          else if prop = 'outline_width' then
            foutlinewidth := DotStrToFloat( val )
          else if prop = 'outline_width_unit' then
            foutlinewidthunit := val
          else if prop = 'penstyle' then
            fstyle := convertStyle( val )
          else if prop = 'size' then
            fpointsize := DotStrToFloat( val )
          else if prop = 'size_unit' then
            fpointsizeunit := val
          else if prop = 'style' then
            ffillpattern := convertPattern( val )
          else if prop = 'line_style' then
            ffillpattern := convertPattern( val )
          else if prop = 'style_border' then
            foutlinestyle := convertStyle( val )
          else if prop = 'width' then
            fwidth := DotStrToFloat( val )
          else if prop = 'line_width' then
            fwidth := DotStrToFloat( val )
          else if prop = 'width_unit' then
            fwidthunit := val
          else if prop = 'line_width_unit' then
            fwidthunit := val
          else if prop = 'width_border' then
            foutlinewidth := DotStrToFloat( val )
          else if prop = 'border_width_unit' then
            foutlinewidthunit := val
        end ;
      end
      else begin
        if nd.NodeName = 'fillcolor' then
          ffillcolor := TGIS_Color.FromRGB(
                          VarToByte(nd.Attributes['red']),
                          VarToByte(nd.Attributes['green']),
                          VarToByte(nd.Attributes['blue'])
                        )
        else if nd.NodeName = 'fillpattern' then
          ffillpattern := convertPattern( nd.Text )
        else if nd.NodeName = 'outlinecolor' then
          foutlinecolor := TGIS_Color.FromRGB(
                             VarToByte(nd.Attributes['red']),
                             VarToByte(nd.Attributes['green']),
                             VarToByte(nd.Attributes['blue'])
                           )
        else if nd.NodeName = 'outlinestyle' then
          foutlinestyle := convertStyle( nd.Text )
        else if nd.NodeName = 'outlinewidth' then
          foutlinewidth := DotStrToFloat( nd.Text )
        else if nd.NodeName = 'pointsize' then
          fpointsize := DotStrToFloat( nd.Text )
        else if nd.NodeName = 'pointsymbol' then
          fpointsymbol := nd.Text
      end ;
    end ;
  end ;

//==============================================================================
// T_category
//==============================================================================

  procedure T_category.readXML(
    const _node : IXMLNode
  ) ;
  begin
    assert( _node <> nil ) ;

    fsymbol := VarToString( _node.Attributes['symbol'] ) ;
    fvalue  := VarToString( _node.Attributes['value']  ) ;
    flabel  := VarToString( _node.Attributes['label']  ) ;
  end ;

//==============================================================================
// T_range
//==============================================================================

  procedure T_range.readXML(
    const _node : IXMLNode
  ) ;
  begin
    assert( _node <> nil ) ;

    fsymbol := VarToString( _node.Attributes['symbol'] ) ;
    flower  := VarToString( _node.Attributes['lower']  ) ;
    fupper  := VarToString( _node.Attributes['upper']  ) ;
    flabel  := VarToString( _node.Attributes['label']  ) ;
  end ;

//==============================================================================
// T_rule
//==============================================================================

  constructor T_rule.Create ;
  begin
    inherited Create ;

    {$IFDEF DCC}
      frules := TObjectList<T_rule>.Create( True ) ;
    {$ELSE}
      frules := TList<T_rule>.Create ;
    {$ENDIF}
  end ;

  constructor T_rule.Create(
    const _scaleMin : Integer ;
    const _scaleMax : Integer ;
    const _filter   : String ;
    const _symbol   : String ;
    const _label    : String
  ) ;
  begin
    inherited Create ;

    fscalemin := _scaleMin ;
    fscalemax := _scaleMax ;
    ffilter   := _filter ;
    fsymbol   := _symbol ;
    flabel    := _label ;
    frules    := nil ;
  end ;

  {$IFNDEF OXYGENE}
    destructor T_rule.Destroy ;
    begin
      FreeObject( frules ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_rule.readXML(
    const _node : IXMLNode
  ) ;
  var
    i     : Integer ;
    sym   : IXMLNode ;
    rule  : T_rule ;
  begin
    assert( _node <> nil ) ;

    fscalemax := VarToInt32( _node.Attributes['scalemaxdenom']  ) ;
    ffilter   := VarToString( _node.Attributes['filter']  ) ;
    ffilter   := StringReplaceAll( ffilter, '$area', 'GIS_AREA' ) ;

    fsymbol   := VarToString( _node.Attributes['symbol'] ) ;
    fscalemin := VarToInt32( _node.Attributes['scalemindenom']  ) ;
    flabel    := VarToString( _node.Attributes['label']  ) ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      sym := _node.ChildNodes[i] ;
      if sym.NodeName = 'rule' then begin
        rule := T_rule.Create ;
        try
          rule.readXML( sym ) ;
          frules.Add( rule ) ;
        except
          FreeObject( rule ) ;
        end ;
      end ;
    end ;
  end ;

  function T_rule.ParseRules : TObjectList<T_rule> ;
  var
    res : TObjectList<T_rule> ; 

    procedure flattenRules( const _rule : T_rule; const _filter : String ) ;
    var
      i       : Integer ;
      rule    : T_rule ;
      ifilter : String ;
    begin
      if IsStringEmpty( _rule.Filter ) then
        ifilter := _filter
      else if not IsStringEmpty( _filter ) then
        ifilter := Format( '(%s) AND (%s)', [_filter, _rule.Filter] )
      else
        ifilter := _rule.Filter ;

      if _rule.frules.Count = 0 then
        res.Add( T_rule.Create( _rule.ScaleMin, _rule.ScaleMax, ifilter,
                                   _rule.Symbol, _rule.&Label )
                  )
      else begin
        for i := 0 to _rule.frules.Count-1 do begin
          rule := _rule.frules[i] ;
          flattenRules( rule, ifilter ) ;
        end ;

      end ;
    end ;

  begin
    res := TObjectList<T_rule>.Create ;
    flattenRules( Self, '' ) ;
    Result := res ;
  end;

//==============================================================================
// T_QStyle
//==============================================================================

  constructor T_QStyle.Create(
    const _type : String
  ) ;
  begin
    inherited Create ;

    ftype       := _type ;
    fsymbols    := TDictionary<String, T_symbol>.Create ;
    fcategories := TObjectList<T_category>.Create( True ) ;
    franges     := TObjectList<T_range>.Create( True ) ;
    frules      := TObjectList<T_rule>.Create( True ) ;
  end ;

  {$IFNDEF OXYGENE}

    destructor T_QStyle.Destroy ;
    var
      o : TPair<String, T_symbol> ;
    begin
      for o in fsymbols do
        o.Value.Free ;

      FreeObject( fsymbols    ) ;
      FreeObject( fcategories ) ;
      FreeObject( franges     ) ;
      FreeObject( frules      ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_QStyle.readXML(
    const _node : IXMLNode
  ) ;
  var
    nd      : IXMLNode ;
    sym     : IXMLNode ;
    i       : Integer ;
    symbol  : T_symbol ;
    cat     : T_category ;
    range   : T_range ;
    rule    : T_rule ;
  begin
    assert( _node <> nil ) ;

    if _node.NodeName = 'renderer-v2' then begin
      if ftype = 'singleSymbol' then begin
        nd := _node.ChildNodes.FindNode( 'symbols' ) ;
        if nd <> nil then begin
          for i := 0 to nd.ChildNodes.Count-1 do begin
            sym := nd.ChildNodes[i] ;
            if sym.NodeName = 'symbol' then begin
              symbol := T_symbol.Create ;
              try
                symbol.readXML( sym ) ;
                fsymbols.Add( symbol.Name, symbol ) ;
              except
                FreeObject( symbol ) ;
              end ;
            end ;
          end ;
        end ;
      end
      else if ftype = 'categorizedSymbol' then begin
        fattr := VarToString( _node.Attributes['attr'] ) ;
        nd := _node.ChildNodes.FindNode( 'categories' ) ;
        if nd <> nil then begin
          for i := 0 to nd.ChildNodes.Count-1 do begin
            sym := nd.ChildNodes[i] ;
            if sym.NodeName = 'category' then begin
              cat := T_category.Create ;
              try
                cat.readXML( sym ) ;
                fcategories.Add( cat ) ;
              except
                FreeObject( cat ) ;
              end ;
            end ;
          end ;
        end ;

        nd := _node.ChildNodes.FindNode( 'symbols' ) ;
        if nd <> nil then begin
          for i := 0 to nd.ChildNodes.Count-1 do begin
            sym := nd.ChildNodes[i] ;
            if sym.NodeName = 'symbol' then begin
              symbol := T_symbol.Create ;
              try
                symbol.readXML( sym ) ;
                fsymbols.Add( symbol.Name, symbol ) ;
              except
                FreeObject( symbol ) ;
              end ;
            end ;
          end ;
        end ;

      end
      else if ftype = 'graduatedSymbol' then begin
        fattr := VarToString( _node.Attributes['attr'] ) ;
        nd := _node.ChildNodes.FindNode( 'ranges' ) ;
        if nd <> nil then begin
          for i := 0 to nd.ChildNodes.Count-1 do begin
            sym := nd.ChildNodes[i] ;
            if sym.NodeName = 'range' then begin
              range := T_range.Create ;
              try
                range.readXML( sym ) ;
                franges.Add( range ) ;
              except
                FreeObject( cat ) ;
              end ;
            end ;
          end ;
        end ;

        nd := _node.ChildNodes.FindNode( 'symbols' ) ;
        if nd <> nil then begin
          for i := 0 to nd.ChildNodes.Count-1 do begin
            sym := nd.ChildNodes[i] ;
            if sym.NodeName = 'symbol' then begin
              symbol := T_symbol.Create ;
              try
                symbol.readXML( sym ) ;
                fsymbols.Add( symbol.Name, symbol ) ;
              except
                FreeObject( symbol ) ;
              end ;
            end ;
          end ;
        end ;
      end
      else if ftype = 'RuleRenderer' then begin
        nd := _node.ChildNodes.FindNode( 'rules' ) ;
        if nd <> nil then begin
          for i := 0 to nd.ChildNodes.Count-1 do begin
            sym := nd.ChildNodes[i] ;
            if sym.NodeName = 'rule' then begin
              rule := T_rule.Create ;
              try
                rule.readXML( sym ) ;
                frules.Add( rule ) ;
              except
                FreeObject( cat ) ;
              end ;
            end ;
          end ;
        end ;

        nd := _node.ChildNodes.FindNode( 'symbols' ) ;
        if nd <> nil then begin
          for i := 0 to nd.ChildNodes.Count-1 do begin
            sym := nd.ChildNodes[i] ;
            if sym.NodeName = 'symbol' then begin
              symbol := T_symbol.Create ;
              try
                symbol.readXML( sym ) ;
                fsymbols.Add( symbol.Name, symbol ) ;
              except
                FreeObject( symbol ) ;
              end ;
            end ;
          end ;
        end ;
      end

    end
    else if _node.NodeName = 'singlesymbol' then begin
      for i := 0 to _node.ChildNodes.Count-1 do begin
        sym := _node.ChildNodes[i] ;
        if sym.NodeName = 'symbol' then begin
          symbol := T_symbol.Create ;
          try
            symbol.readXML( sym ) ;
            fsymbols.Add( symbol.Name, symbol ) ;
          except
            FreeObject( symbol ) ;
          end ;
        end ;
      end ;
    end
    else if _node.NodeName = 'uniquevalue' then begin
      for i := 0 to 0 do begin
        nd := _node.ChildNodes[i] ;
        if nd.NodeName = 'symbol' then begin
          symbol := T_symbol.Create ;
          try
            symbol.readXML( nd ) ;
            fsymbols.Add( symbol.Name, symbol ) ;
          except
            FreeObject( symbol ) ;
          end ;
        end
        else if nd.NodeName = 'classificationfield' then begin
          fattr := nd.Text
        end ;
      end ;

    end ;

  end ;

//==============================================================================
// T_labelattrs
//==============================================================================

  procedure T_labelattrs.readXML(
    const _node : IXMLNode
  ) ;
  var
    nd    : IXMLNode ;
    i     : Integer ;
    key   : String ;
    r,g,b : Byte ;
  begin
    assert( _node <> nil ) ;
    g := 0 ;
    b := 0 ;
    {$IFDEF GIS_NORECORDS}
      fcolor      := TGIS_Color.create;
      fbackcolor  := TGIS_Color.create;
      fbackocolor := TGIS_Color.create;
      fshadowcolor:= TGIS_Color.create;
    {$ENDIF}

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;

      if nd.NodeName = 'property' then begin
        key := VarToString( nd.Attributes['key'] ) ;

        if key = 'labeling/fieldName' then
          ffieldname := VarToString( nd.Attributes['value'] )
        else if key = 'labeling/fontFamily' then
          ffamily := VarToString( nd.Attributes['value'] )
        else if key = 'labeling/enabled' then
          fenabled := VarToBoolean( nd.Attributes['value'] )
        else if key = 'labeling/fontBold' then
          fbold := VarToBoolean( nd.Attributes['value'] )
        else if key = 'labeling/fontItalic' then
          fitalic := VarToBoolean( nd.Attributes['value'] )
        else if key = 'labeling/fontSize' then
          fsize := DotStrToFloat( VarToString( nd.Attributes['value'] ) )
        else if key = 'labeling/fontStrikeout' then
          fstrikeout := VarToBoolean( nd.Attributes['value'] )
        else if key = 'labeling/fontUnderline' then
          funderline := VarToBoolean( nd.Attributes['value'] )
        else if key = 'labeling/textColorB' then
          b := VarToByte( nd.Attributes['value'] )
        else if key = 'labeling/textColorG' then
          g := VarToByte( nd.Attributes['value'] )
        else if key = 'labeling/textColorR' then begin
          r := VarToByte( nd.Attributes['value'] ) ;
          fcolor := TGIS_Color.FromRGB( r, g, b ) ;
        end
        else if key = 'labeling/displayAll' then
          fdisplayall := VarToBoolean( nd.Attributes['value'] )
        else if key = 'labeling/placement' then
          fplacement := VarToInt32( nd.Attributes['value'] )
        else if key = 'labeling/placementFlags' then
          fplacementf := VarToInt32( nd.Attributes['value'] )
        else if key = 'labeling/quadOffset' then
          fplacementq := VarToInt32( nd.Attributes['value'] )
        else if key = 'labeling/bufferDraw' then
          fdrawshadow := VarToBoolean( nd.Attributes['value'] )
        else if key = 'labeling/scaleVisibility' then
          fscalevis := VarToBoolean( nd.Attributes['value'] )
        else if key = 'labeling/scaleMax' then
          fscalemax := VarToInt32( nd.Attributes['value'] )
        else if key = 'labeling/scaleMin' then
          fscalemin := VarToInt32( nd.Attributes['value'] )
        else if key = 'labeling/bufferColorB' then
          b := VarToByte( nd.Attributes['value'] )
        else if key = 'labeling/bufferColorG' then
          g := VarToByte( nd.Attributes['value'] )
        else if key = 'labeling/bufferColorR' then begin
          r := VarToByte( nd.Attributes['value'] ) ;
          fshadowcolor := TGIS_Color.FromRGB( r, g, b ) ;
        end
        else if key = 'labeling/shapeDraw' then
          fdrawback := VarToBoolean( nd.Attributes['value'] )
        else if key = 'labeling/shapeFillColorB' then
          b := VarToByte( nd.Attributes['value'] )
        else if key = 'labeling/shapeFillColorG' then
          g := VarToByte( nd.Attributes['value'] )
        else if key = 'labeling/shapeFillColorR' then begin
          r := VarToByte( nd.Attributes['value'] ) ;
          fbackcolor := TGIS_Color.FromRGB( r, g, b ) ;
        end
        else if key = 'labeling/shapeBorderColorB' then
          b := VarToByte( nd.Attributes['value'] )
        else if key = 'labeling/shapeBorderColorG' then
          g := VarToByte( nd.Attributes['value'] )
        else if key = 'labeling/shapeBorderColorR' then begin
          r := VarToByte( nd.Attributes['value'] ) ;
          fbackocolor := TGIS_Color.FromRGB( r, g, b ) ;
        end
        else if key = 'labeling/shapeBorderWidth' then
          fbackborder := DotStrToFloat( VarToString( nd.Attributes['value'] ) )
      end
      else if nd.NodeName ='text-style' then begin
        ffieldname := VarToString( nd.Attributes['fieldName'] ) ;
        if not IsStringEmpty( ffieldname ) then
          fenabled := True ;
        ffamily := VarToString( nd.Attributes['fontFamily'] ) ;
        fitalic := VarToBoolean( nd.Attributes['fontItalic'] ) ;
        fsize := DotStrToFloat( VarToString( nd.Attributes['fontSize'] ) ) ;
        fsizeunit := VarToString( nd.Attributes['fontSizeUnit'] ) ;
        fplacementf := VarToInt32( nd.Attributes['placementFlags'] ) ;
      end
      else if nd.NodeName ='placement' then begin
        fplacement := VarToInt32( nd.Attributes['placement'] ) ;
        fplacementq := VarToInt32( nd.Attributes['quadOffset'] ) ;
      end
      else if nd.NodeName = 'label' then begin
        ffieldname := VarToString( nd.Attributes['fieldname'] ) ;
        if not IsStringEmpty( ffieldname ) then
          fenabled := True ;
      end
      else if nd.NodeName = 'family' then
        ffamily := VarToString( nd.Attributes['name'] )
      else if nd.NodeName = 'size' then
        fsize := VarToInt32( nd.Attributes['value'] )
      else if nd.NodeName = 'fontSizeUnit' then
        fsizeunit := VarToString( nd.Attributes['value'] )
      else if nd.NodeName = 'bold' then
        fbold := VarToBoolean( nd.Attributes['on'] )
      else if nd.NodeName = 'italic' then
        fitalic := VarToBoolean( nd.Attributes['on'] )
      else if nd.NodeName = 'underline' then
        funderline := VarToBoolean( nd.Attributes['on'] )
      else if nd.NodeName = 'strikeout' then
        fstrikeout := VarToBoolean( nd.Attributes['on'] )
      else if nd.NodeName = 'color' then
        fcolor := TGIS_Color.FromRGB(
                    VarToByte(nd.Attributes['red']),
                    VarToByte(nd.Attributes['green']),
                    VarToByte(nd.Attributes['blue'])
                  )
      else if nd.NodeName = 'angle' then
        fangle := DotStrToFloat( VarToString( nd.Attributes['value'] ) )
      else if nd.NodeName = 'alignment' then
        falignment := VarToString( nd.Attributes['value'] )
    end ;
  end ;

//==============================================================================
// T_labelRuleAttrs
//==============================================================================

  {$IFNDEF OXYGENE}
    destructor T_labelRuleAttrs.Destroy ;
    begin
      FreeObject( flabelattrs ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_labelRuleAttrs.readXML(
    const _node : IXMLNode
  ) ;
  begin
    assert( _node <> nil ) ;
    if _node.NodeName = 'rule' then begin
      fdesc := VarToString( _node.Attributes['description'] ) ;
      ffilter := VarToString( _node.Attributes['filter'] ) ;
      ffilter := StringReplaceAll( ffilter, '$area', 'GIS_AREA' ) ;
      ffilter := StringReplaceAll( ffilter, #$D#$A, '' ) ;
      flabelattrs := T_labelattrs.Create ;
      flabelattrs.readXML( _node.ChildNodes['settings'] ) ;
    end ;
  end ;

//==============================================================================
// T_colorramp
//==============================================================================

  procedure T_colorramp.readXML(
    const _node : IXMLNode
  ) ;
  var
    scolor : String ;
    sr,
    sg,
    sb    : String ;
    r,
    g,
    b     : Byte ;
  begin
    assert( _node <> nil ) ;

    fvalue := DotStrToFloat( VarToString( _node.Attributes['value'] ) ) ;
    flabel := VarToString( _node.Attributes['label'] ) ;
    scolor := VarToString( _node.Attributes['color'] ) ;

    if Pos( '#', scolor ) = StringFirst then begin
      sr := Copy( scolor, StringFirst+1, 2 ) ;
      sg := Copy( scolor, StringFirst+3, 2 ) ;
      sb := Copy( scolor, StringFirst+5, 2 ) ;
      try
        r := StrToInt( '$' + sr ) ;
      except
        r := 0 ;
      end ;
      try
        g := StrToInt( '$' + sg ) ;
      except
        g := 0 ;
      end ;
      try
        b := StrToInt( '$' + sb ) ;
      except
        b := 0 ;
      end ;
      fcolor := TGIS_Color.FromRGB( r, g, b ) ;
    end ;
  end ;

//==============================================================================
// T_shader
//==============================================================================

  {$IFNDEF OXYGENE}

    destructor T_shader.Destroy ;
    begin
      FreeObject( fcolorramps ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_shader.readXML(
    const _node : IXMLNode
  ) ;
  var
    nd  : IXMLNode ;
    nd2 : IXMLNode ;
    i   : Integer ;
    cr  : T_colorramp ;
  begin
    assert( _node <> nil ) ;

    nd := _node.ChildNodes.FindNode('colorrampshader') ;
    if nd <> nil then begin
      fcolorramps := TObjectList<T_colorramp>.Create( True ) ;

      fcolorRampType := VarToString( nd.Attributes['colorRampType'] ) ;
      for i := 0 to nd.ChildNodes.Count-1 do begin
        nd2 := nd.ChildNodes[i] ;
        if nd2.NodeName = 'item' then begin
          cr := T_colorramp.Create ;
          try
            cr.readXML( nd2 ) ;
            fcolorramps.Add( cr ) ;
          except
            FreeObject( cr ) ;
          end ;
        end ;
      end ;
    end ;

  end ;

//==============================================================================
// T_raster
//==============================================================================

  {$IFNDEF OXYGENE}

    destructor T_raster.Destroy ;
    begin
      FreeObject( fshader ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_raster.readXML(
    const _node : IXMLNode
  ) ;
  var
    nd  : IXMLNode ;
    nd2 : IXMLNode ;
    i   : Integer ;
  begin
    assert( _node <> nil ) ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;

      if nd.NodeName = 'rasterrenderer' then begin
        fopacity    := DotStrToFloat( VarToString( nd.Attributes['opacity'] ) ) ;
        falphaband  := VarToInt32( nd.Attributes['alphaBand'] ) ;
        fblueband   := VarToInt32( nd.Attributes['blueBand'] ) ;
        fgreenband  := VarToInt32( nd.Attributes['greenBand'] ) ;
        fredband    := VarToInt32( nd.Attributes['redBand'] ) ;
        ftype       := VarToString( nd.Attributes['type'] ) ;

        if (nd.ChildNodes.FindNode('redContrastEnhancement')   <> nil) and
           (nd.ChildNodes.FindNode('greenContrastEnhancement') <> nil) and
           (nd.ChildNodes.FindNode('blueContrastEnhancement')  <> nil) then
           fhistogram := True ;

        nd2 := nd.ChildNodes.FindNode('rastershader') ;
        if (nd2 <> nil) then begin
          fclassmin := DotStrToFloat( VarToString( nd.Attributes['classificationMin'] ) ) ;
          fclassmax := DotStrToFloat( VarToString( nd.Attributes['classificationMax'] ) ) ;

          fshader := T_shader.Create ;
          fshader.readXML( nd2 ) ;
        end ;
      end
      else if nd.NodeName = 'brightnesscontrast' then begin
        fbrightness  := VarToInt32( nd.Attributes['brightness'] ) ;
        fcontrast    := VarToInt32( nd.Attributes['contrast'] ) ;
      end
      else if nd.NodeName = 'huesaturation' then begin
        fgrayscale := VarToInt32( nd.Attributes['grayscaleMode'] ) > 0 ;
      end
    end ;

  end ;

//==============================================================================
// T_maplayer
//==============================================================================

  {$IFNDEF OXYGENE}
    destructor T_maplayer.Destroy ;
    begin
      FreeObject( fstyle      ) ;
      FreeObject( flabelattrs ) ;
      FreeObject( fraster     ) ;
      FreeObject( flabelrules ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_maplayer.parseDatasource ;
  var
    tkn : TGIS_Tokenizer ;
  begin
    if Pos( '|', fdatasource ) >= StringFirst then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.ExecuteEx( fdatasource, '|', '''' ) ;
        fpath := tkn.Result[0] ;
        if tkn.Result.Count = 3 then begin
          fscope := tkn.Result[2] ;
          fscope := Copy( fscope, Pos('=', fscope ) + 1, length(fscope) ) ;
          {$IFDEF OXYGENE}
            fscope := StringReplace( fscope, '!=', '<>', [TReplaceFlag.rfReplaceAll] ) ;
          {$ELSE}
            fscope := StringReplace( fscope, '!=', '<>', [rfReplaceAll] ) ;
          {$ENDIF}
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    end
    else begin
      fpath  := fdatasource ;
      fscope := '' ;
    end ;
  end ;

  procedure T_maplayer.readXML(
    const _node : IXMLNode
  ) ;
  var
    nd    : IXMLNode ;
    ndex  : IXMLNode ;
    i     : Integer ;
    lrule : T_labelRuleAttrs ;
  begin
    assert( _node <> nil ) ;

    if VarToString(_node.Attributes['hasScaleBasedVisibilityFlag']) = '1' then begin
      if _node.HasAttribute('minimumScale') then
        fminscale := DotStrToFloat( VarToString(_node.Attributes['minimumScale']) )
      else if _node.HasAttribute('minScale') then
        fminscale := DotStrToFloat( VarToString(_node.Attributes['minScale']) ) ;

      if _node.HasAttribute('maximumScale') then
        fmaxscale := DotStrToFloat( VarToString(_node.Attributes['maximumScale']) )
      else if _node.HasAttribute('maxScale') then
        fmaxscale := DotStrToFloat( VarToString(_node.Attributes['maxScale']) ) ;
    end
    else begin
      fminscale := 0 ;
      fmaxscale := 0 ;
    end ;
    fgeometry := VarToString( _node.Attributes['geometry'] ) ;

    ftype       := VarToString( _node.Attributes['geometry'] ) ;
    fid         := _node.ChildNodes.FindNode( 'id' ).Text ;
    fdatasource := _node.ChildNodes.FindNode( 'datasource' ).Text ;
    parseDatasource ;
    flayername  := _node.ChildNodes.FindNode( 'layername' ).Text ;
    parseSrs( _node.ChildNodes.FindNode( 'srs' ) ) ;

    nd := _node.ChildNodes.FindNode( 'renderer-v2' ) ;
    if nd <> nil then begin
      fstyle := T_QStyle.Create( VarToString( nd.Attributes['type'] ) ) ;
      fstyle.readXML( nd ) ;
    end ;

    nd := _node.ChildNodes.FindNode( 'singlesymbol' ) ;
    if nd <> nil then begin
      fstyle := T_QStyle.Create( 'singlesymbol' ) ;
      fstyle.readXML( nd ) ;
    end ;

    nd := _node.ChildNodes.FindNode( 'uniquevalue' ) ;
    if nd <> nil then begin
      fstyle := T_QStyle.Create( 'uniquevalue' ) ;
      fstyle.readXML( nd ) ;
    end ;

    nd := _node.ChildNodes.FindNode( 'graduatedsymbol' ) ;
    if nd <> nil then begin
      fstyle := T_QStyle.Create( 'graduatedsymbol' ) ;
      fstyle.readXML( nd ) ;
    end ;

    nd := _node.ChildNodes.FindNode( 'layerTransparency' ) ;
    if nd <> nil then
      ftransparency := 100-StrToIntDef( nd.Text, 0 )
    else
      ftransparency := 100 ;

    nd := _node.ChildNodes.FindNode( 'label' ) ;
    if nd <> nil then
      fhaslabel := nd.Text = '1'
    else begin
      fhaslabel := False ;
      nd := _node.ChildNodes.FindNode( 'labeling' ) ;
      if nd <> nil then
        fhaslabel := True
    end ;

    if fhaslabel then begin
      nd := _node.ChildNodes.FindNode( 'labelfield' ) ;
      if nd <> nil then
        flabelfield := nd.Text ;

      nd := _node.ChildNodes.FindNode( 'labelattributes' ) ;
      if nd <> nil then begin
        flabelattrs := T_labelattrs.Create ;
        flabelattrs.readXML( nd ) ;
      end ;
      nd := _node.ChildNodes.FindNode( 'labeling' ) ;
      if nd <> nil then begin
        ndex := nd.ChildNodes.FindNode( 'settings' ) ;
        if assigned( ndex ) then begin
          flabelattrs := T_labelattrs.Create ;
          flabelattrs.readXML( nd.ChildNodes['settings'] ) ;
          flabelfield := flabelattrs.FieldName ;
          fhaslabelrules := False ;
        end
        else begin
          ndex := nd.ChildNodes.FindNode( 'rules' ) ;
          if assigned( ndex ) then begin
            fhaslabelrules := True ;
            {$IFDEF DCC}
              flabelrules := TObjectList<T_labelRuleAttrs>.Create( True ) ;
            {$ELSE}
              flabelrules   := TList<T_labelRuleAttrs>.Create ;
            {$ENDIF}

            for i := 0 to ndex.ChildNodes.Count-1 do begin
              lrule := T_labelRuleAttrs.Create ;
              lrule.readXML( ndex.ChildNodes[i] ) ;
              flabelrules.Add( lrule ) ;
            end ;
          end ;
        end ;
      end ;
    end
    else begin
      nd := _node.ChildNodes.FindNode( 'customproperties' ) ;
      if nd <> nil then begin
        if not assigned( flabelattrs ) then
          flabelattrs := T_labelattrs.Create ;
        flabelattrs.readXML( nd ) ;
        fhaslabel := flabelattrs.Enabled ;
      end ;
    end ;

    nd := _node.ChildNodes.FindNode( 'pipe' ) ;
    if nd <> nil then begin
      fraster := T_raster.Create ;
      fraster.readXML( nd ) ;
    end ;

  end ;

//==============================================================================
// T_projectlayers
//==============================================================================

  {$IFNDEF OXYGENE}

    destructor T_projectlayers.Destroy ;
    var
      o : TPair<String, T_maplayer> ;
    begin
      for o in flayers do
        o.Value.Free ;

      FreeObject( flayers ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_projectlayers.readXML(
    const _node : IXMLNode
  ) ;
  var
    layer : T_maplayer ;
    nd    : IXMLNode ;
    i     : Integer ;
  begin
    assert( _node <> nil ) ;

    flayers := TDictionary<String, T_maplayer>.Create ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'maplayer' then begin
        layer := T_maplayer.Create ;
        try
          layer.readXML( nd ) ;
          flayers.Add( layer.Id, layer ) ;
        except
          FreeObject( layer ) ;
        end ;
      end ;
    end ;
  end ;

//==============================================================================
// T_Qgis
//==============================================================================

  {$IFNDEF OXYGENE}

    destructor T_Qgis.Destroy ;
    begin
      FreeObject( fmapcanvas     ) ;
      FreeObject( flegend        ) ;
      FreeObject( fprojectlayers ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_Qgis.readXML(
    const _node : IXMLNode
  ) ;
  begin
    assert( _node <> nil ) ;

    fmapcanvas := T_mapcanvas.Create ;
    fmapcanvas.readXML( _node.ChildNodes.FindNode( 'mapcanvas' ) ) ;

    flegend := T_legend.Create ;
    flegend.readXML( _node.ChildNodes.FindNode( 'legend' ) ) ;

    fprojectlayers := T_projectlayers.Create ;
    fprojectlayers.readXML( _node.ChildNodes.FindNode( 'projectlayers' ) ) ;
  end ;

//==============================================================================
// TGIS_ProjectQgis
//==============================================================================

  constructor TGIS_ProjectQgis.Create(
    const _viewer : TGIS_Viewer
  ) ;
  begin
    inherited Create ;

    if not assigned( _viewer ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ), '', 0 ) ;

    oViewer := _viewer ;
  end ;

  procedure TGIS_ProjectQgis.importQgis(
    const _path : String
  ) ;
  var
    xml        : IXMLDocument ;
    nd         : IXMLNode ;
    qgis       : T_Qgis ;
    err        : String ;
    dir        :  String ;
    i,j        : Integer ;
    la         : TGIS_Layer ;
    lv         : TGIS_LayerVector ;
    lp         : TGIS_LayerPixel ;
    layer      : T_maplayer ;
    {$IFNDEF OXYGENE}
      symbol   : TPair<String, T_symbol> ;
      range    : T_Range ;
      category : T_category ;
      rule     : T_rule ;
      irule    : T_rule ;
    {$ENDIF}
    cr1        : T_colorramp ;
    cr2        : T_colorramp ;
    sym        : T_symbol ;
    legend     : T_legendlayer ;
    prms       : TGIS_ParamsSectionPixel ;
    colormap   : TGIS_ColorMapArray ;
    ext        : String ;
    mstm       : TStream ;
    {$IFDEF DCC}
      lrule    : T_labelRuleAttrs ;
    {$ENDIF}
    lst : TObjectList<T_rule> ;

      function convert_size( const _unit : String ; const _val : Double ) : Integer ;
      begin
        if _unit = 'MM' then
          Result := RoundS( _val * 1440 / 2.54 / 10 )
        else if _unit = 'MapUnit' then
          Result := RoundS( _val * 1440 / 2.54 / 10 * 1000 ) + GIS_AUTOSIZE_SIZE
        else
          Result := RoundS( _val ) ;
      end ;

      function convert_pointsymbol( const _val : String ) : TGIS_MarkerStyle ;
      begin
        if (_val = 'circle') or (_val = 'hard:circle') then
          Result := TGIS_MarkerStyle.Circle
        else if (_val = 'rectangle') or (_val = 'hard:rectangle') then
          Result := TGIS_MarkerStyle.Box
        else if (_val = 'cross') or (_val = 'hard:cross') then
          Result := TGIS_MarkerStyle.Cross
        else if (_val = 'cross2') or (_val = 'hard:cross2') then
          Result := TGIS_MarkerStyle.DiagCross
        else if (_val = 'triangle') or (_val = 'hard:triangle') then
          Result := TGIS_MarkerStyle.TriangleUp
        else if (_val = 'filled_arrowhead') or (_val = 'hard:filled_arrowhead') then
          Result := TGIS_MarkerStyle.TriangleRight
        else
          Result := TGIS_MarkerStyle.Circle ;
      end ;

      procedure set_params( const _type : String ; const _sym : T_symbol ) ;
      begin
        if _type = 'Point' then begin
          lv.Params.Marker.Color        := _sym.FillColor ;
          lv.Params.Marker.Style        := convert_pointsymbol( _sym.PointSymbol ) ;
          lv.Params.Marker.Size         := convert_size( _sym.PointSizeUnit, _sym.PointSize ) ;
          lv.Params.Marker.OutlineColor := _sym.OutlineColor ;
          lv.Params.Marker.OutlineStyle := _sym.OutlineStyle ;
          lv.Params.Marker.OutlineWidth := convert_size( _sym.OutlineWidthUnit, _sym.OutlineWidth ) ;
        end
        else if _type = 'Line' then begin
          lv.Params.Line.Color        := _sym.FillColor ;
          lv.Params.Line.Width        := convert_size( _sym.WidthUnit, _sym.Width ) ;
          lv.Params.Line.OutlineColor := _sym.OutlineColor ;
          lv.Params.Line.OutlineWidth := convert_size( _sym.OutlineWidthUnit, _sym.OutlineWidth ) ;
          lv.Params.Line.Style        := _sym.Style ;
          lv.Params.Line.OutlineStyle := _sym.OutlineStyle ;
        end
        else if _type = 'Polygon' then begin
          lv.Params.Area.Pattern      := _sym.FillPattern ;
          lv.Params.Area.Color        := _sym.FillColor ;
          lv.Params.Area.OutlineColor := _sym.OutlineColor ;
          lv.Params.Area.OutlineStyle := _sym.OutlineStyle ;
          lv.Params.Area.OutlineWidth := convert_size( _sym.OutlineWidthUnit,_sym.OutlineWidth ) ;
        end
        else if _type = 'null' then begin
          lv.Params.Marker.Size         := 0 ;
          lv.Params.Marker.OutlineWidth := 0 ;
          lv.Params.Line.Width          := 0 ;
          lv.Params.Line.OutlineWidth   := 0 ;
          lv.Params.Area.Pattern        := TGIS_BrushStyle.Clear ;
          lv.Params.Area.OutlineStyle   := TGIS_PenStyle.Clear ;
          lv.Params.Area.OutlineWidth   := 0 ;
        end;
      end ;

      procedure set_labels( const _field : String ; const _lb : T_labelattrs ) ;
      var
        fs : TGIS_FontStyles ;
        {$IFDEF JAVA}
          //lps : TGIS_LabelPositions ;
          //pq  : Integer ;
        {$ENDIF}
      begin
        if not assigned( _lb ) then exit ;

        if _lb.ScaleVisibility then begin
          lv.ParamsList.Add ;
          if _lb.ScaleMin > 0 then
            lv.Params.MaxScale := 1/_lb.ScaleMin ;
          if _lb.ScaleMax > 0 then
            lv.Params.MinScale := 1/_lb.ScaleMax ;
        end ;

        if not IsStringEmpty(_field) then
          lv.Params.Labels.Field := _field
        else
          lv.Params.Labels.Field := _lb.FieldName ;
        lv.Params.Labels.Rotate := _lb.Angle ;

        lv.Params.Labels.Allocator := not _lb.DisplayAll ;
        if _lb.Placement = 2 then
          lv.Params.Labels.Alignment := TGIS_LabelAlignment.Follow
        else if _lb.Placement = 4 then
          lv.Params.Labels.Alignment := TGIS_LabelAlignment.Single ;

        if _lb.DrawBackground then begin
          lv.Params.Labels.Pattern        := TGIS_BrushStyle.Solid ;
          lv.Params.Labels.Color          := _lb.BackgroundColor ;
          lv.Params.Labels.OutlineColor   := _lb.BackgroundOutlineColor ;
          lv.Params.Labels.OutlineWidth   := convert_size( 'MM',_lb.BackgroundBorder ) ;
          lv.Params.Labels.OutlinePattern := TGIS_BrushStyle.Solid ;
          lv.Params.Labels.OutlineStyle   := TGIS_PenStyle.Solid ;
        end ;

        if _lb.DrawShadow then
          lv.Params.Labels.Color  := _lb.ShadowColor
        else
          lv.Params.Labels.Color  := _lb.Color ;
        { TODO -cReview : To review }
        (* //?? check
          pq := _lb.PlacementQuad ;
          if pq = 0 then
            lps.add( TGIS_LabelPosition.Undefined )
          else begin
            while True do begin
              if pq >= 512 then begin
                lps.add( TGIS_LabelPosition.Flow ) ;
                pq := pq - 512 ;
                continue ;
              end ;
              if pq >= 256 then begin
                lps.add( TGIS_LabelPosition.DownRight ) ;
                pq := pq - 256 ;
                continue ;
              end ;
              if pq >= 128 then begin
                lps.add( TGIS_LabelPosition.DownCenter ) ;
                pq := pq - 128 ;
                continue ;
              end ;
              if pq >= 64 then begin
                lps.add( TGIS_LabelPosition.DownLeft ) ;
                pq := pq - 64 ;
                continue ;
              end ;
              if pq >= 32 then begin
                lps.add( TGIS_LabelPosition.MiddleRight ) ;
                pq := pq - 32 ;
                continue ;
              end ;
              if pq >= 16 then begin
                lps.add( TGIS_LabelPosition.MiddleCenter ) ;
                pq := pq - 16 ;
                continue ;
              end ;
              if pq >= 8 then begin
                lps.add( TGIS_LabelPosition.MiddleLeft ) ;
                pq := pq - 8 ;
                continue ;
              end ;
              if pq >= 4 then begin
                lps.add( TGIS_LabelPosition.UpRight ) ;
                pq := pq - 4 ;
                continue ;
              end ;
              if pq >= 2 then begin
                lps.add( TGIS_LabelPosition.UpCenter ) ;
                pq := pq - 2 ;
                continue ;
              end ;
              if pq >= 1 then begin
                lps.add( TGIS_LabelPosition.UpLeft ) ;
                pq := pq - 1 ;
                continue ;
              end ;
              if pq = 0 then
                break ;
            end ;
          end ;
        *)
        lv.Params.Labels.Position   := GisGetLabelPosition(TGIS_LabelPosition(_lb.PlacementQuad)) ;
        lv.Params.Labels.Font.Color := _lb.Color ;
        lv.Params.Labels.Font.Name  := _lb.Family ;
        lv.Params.Labels.Font.Size  := RoundS(_lb.Size / 1440 * 2.54 * 10 ) ;

        lv.Params.Marker.Size := 0 ;
        fs := GisGetEmptyFontStyle ;
        if _lb.Bold then
          fs := GisAddFontStyle( fs, TGIS_FontStyle.Bold ) ;
        if _lb.Italic then
          fs := GisAddFontStyle( fs, TGIS_FontStyle.Italic ) ;
        if _lb.Underline then
          fs := GisAddFontStyle( fs, TGIS_FontStyle.Underline ) ;
        if _lb.Strikeout then
          fs := GisAddFontStyle( fs, TGIS_FontStyle.StrikeOut ) ;

        lv.Params.Labels.Font.Style := fs ;
      end ;

      procedure set_labels_rules( const _lrule : T_labelRuleAttrs ) ;
      begin
        lv.ParamsList.Add ;
        lv.Params.Visible := True ;
        lv.Params.Query := _lrule.Filter ;
        lv.Params.Legend := _lrule.Description ;
        set_labels( _lrule.LabelAttributes.FieldName, _lrule.LabelAttributes ) ;
      end ;


  begin
    xml := TGIS_XMLDocument.Create ;
    try
      ext := UpperCase( GetFileExt( _path ) ) ;
      if ext = '.QGZ' then begin
        mstm := DecompressZipFile( _path, GetFileNameNoExt( _path ) + '.qgs' ) ;
        if assigned( mstm ) then begin
          try
            mstm.Position := 0 ;
            xml.LoadFromStream( mstm ) ;
          finally
            FreeObject( mstm ) ;
          end ;
        end
      end
      else
        xml.LoadFromFile( _path ) ;

      nd := xml.DocumentElement ;
      assert( nd.NodeName = 'qgis' ) ;

      qgis := T_Qgis.Create ;
      try
        qgis.readXML( nd ) ;

        err := '' ;
        dir := GetFileDir( _path ) ;

        oViewer.Close;
        try
          for i := qgis.Legend.LegendLayers.Count - 1 downto 0 do begin
            legend := qgis.Legend.LegendLayers[i] ;
            if not qgis.ProjectLayers.Layers.TryGetValue( legend.LayerId,
                                                      layer
                                                     ) then continue ;
            la := GisCreateLayer( layer.Id,
                                  GetPathAbsolute( dir, layer.Path )
                                ) ;
            if not assigned( la ) then continue ;
            try
              oViewer.Add( la ) ;
            except
              on e : Exception do begin
                if oViewer.Items.Remove( la ) < 0 then
                  FreeObject( la ) ;
                la := nil ;

                err := err + #13 + e.Message ;
                continue ;
              end ;
            end ;

            la.Active := legend.Checked ;
            la.Transparency := layer.Transparency ;
            la.Collapsed := not legend.Open ;

            if not IsStringEmpty( layer.LayerName ) then
              la.Caption := layer.LayerName ;

            if not IsStringEmpty( layer.Scope ) then
              if la is TGIS_LayerVector then
                TGIS_LayerVector(la).Params.Query := layer.Scope ;

            if la.CS.EPSG = 0 then
              la.SetCSByEPSG( layer.Srid ) ;

            if assigned( layer.Style ) and ( la is TGIS_LayerVector ) then begin
              lv := la as TGIS_LayerVector ;
              if layer.MinScale > 0 then
                lv.Params.MaxScale := 1/layer.MinScale ;
              if layer.MaxScale > 0 then
                lv.Params.MinScale := 1/layer.MaxScale ;

              if layer.HasLabel then begin
                if assigned( layer.LabelAttrs ) then
                  set_labels( layer.LabelField, layer.LabelAttrs )
                else if layer.HasLabelRules then begin
                  for lrule in layer.LabelRules do
                    set_labels_rules( lrule )
                end ;
              end ;

               if layer.Geometry = 'Point' then
                 lv.Params.Marker.ShowLegend := True
               else if layer.Geometry = 'Line' then
                 lv.Params.Line.ShowLegend := True
               else if layer.Geometry = 'Polygon' then
                 lv.Params.Area.ShowLegend := True ;

              if (layer.Style.StyleType = 'singleSymbol') or
                 (layer.Style.StyleType = 'singlesymbol') then begin
                for symbol in layer.Style.Symbols do begin
                  sym := symbol.Value ;
                  set_params( layer.Geometry, sym ) ;
                end ;
              end
              else if (layer.Style.StyleType = 'nullSymbol') then begin
                set_params( 'null', nil ) ;
              end
              else if (layer.Style.StyleType = 'graduatedSymbol') then begin
                lv.Params.Visible := False ;
                 for range in layer.Style.Ranges do begin
                   lv.ParamsList.Add ;
                   lv.Params.Visible := True ;
                   lv.Params.Query := Format( '%s>=%s AND %s<=%s',
                                              [layer.Style.Attribute,
                                               range.Lower,
                                               layer.Style.Attribute,
                                               range.Upper ]
                                             ) ;
                   lv.Params.Legend := range.&Label ;
                   if layer.Style.Symbols.TryGetValue( range.Symbol, sym ) then
                     set_params( layer.Geometry, sym ) ;
                 end ;
              end
              else if (layer.Style.StyleType = 'categorizedSymbol') then begin
                lv.Params.Visible := False ;
                 for category in layer.Style.Categories do begin
                   lv.ParamsList.Add ;
                   lv.Params.Visible := True ;
                   lv.Params.Query := Format( '%s=''%s''',
                                              [layer.Style.Attribute,
                                               category.Value ]
                                             ) ;

                   lv.Params.Legend := category.&Label ;
                   if layer.Geometry = 'Point' then
                     lv.Params.Marker.ShowLegend := True
                   else if layer.Geometry = 'Line' then
                     lv.Params.Line.ShowLegend := True
                   else if layer.Geometry = 'Polygon' then
                     lv.Params.Area.ShowLegend := True ;

                   if layer.Style.Symbols.TryGetValue( category.Symbol, sym ) then
                     set_params( layer.Geometry, sym ) ;
                 end ;
              end
              else if (layer.Style.StyleType = 'RuleRenderer') then begin
                lv.Params.Visible := False ;
                 for rule in layer.Style.Rules do begin
                   lst := rule.ParseRules ;
                   try
                     for irule in lst do begin
                       lv.ParamsList.Add ;
                       lv.Params.Visible := True ;

                       if irule.ScaleMin > 0 then
                         lv.Params.MaxScale := 1/irule.ScaleMin ;
                       if irule.ScaleMax > 0 then
                         lv.Params.MinScale := 1/irule.ScaleMax ;

                       if not IsStringEmpty( irule.Filter ) then
                         lv.Params.Query := irule.Filter ;

                       lv.Params.Legend := irule.&Label ;

                       if layer.Geometry = 'Point' then
                         lv.Params.Marker.ShowLegend := True
                       else if layer.Geometry = 'Line' then
                         lv.Params.Line.ShowLegend := True
                       else if layer.Geometry = 'Polygon' then
                         lv.Params.Area.ShowLegend := True ;

                       if layer.Style.Symbols.TryGetValue( irule.Symbol, sym ) then
                         set_params( layer.Geometry, sym ) ;
                     end ;
                   finally
                     FreeObject( lst ) ;
                   end ;
                 end ;
              end
            end
            else if assigned( layer.Raster ) and
                    ( la is TGIS_LayerPixel ) then begin
              lp := TGIS_LayerPixel( la ) ;
              prms := TGIS_ParamsSectionPixel( lp.Params ) ;

              lp.Transparency := RoundS( layer.Raster.Opacity * 100 ) ;
              prms.Pixel.AlphaBand  := layer.Raster.AlphaBand ;
              prms.Pixel.RedBand    := layer.Raster.RedBand ;
              prms.Pixel.GreenBand  := layer.Raster.GreenBand ;
              prms.Pixel.BlueBand   := layer.Raster.BlueBand ;
              prms.Pixel.Contrast   := layer.Raster.Contrast ;
              prms.Pixel.Brightness := RoundS(layer.Raster.Brightness/255*100) ;
              prms.Pixel.GrayScale  := layer.Raster.GrayScale ;
              prms.Pixel.Histogram  := layer.Raster.Histogram ;

              if assigned( layer.Raster.Shader ) then begin
                if layer.Raster.Shader.ColorRampType = 'INTERPOLATED' then begin
                  SetLength( colormap, layer.Raster.Shader.ColorRamps.Count ) ;
                  for j := 0 to layer.Raster.Shader.ColorRamps.Count - 1 do begin
                    cr1 := layer.Raster.Shader.ColorRamps[j] ;
                    colormap[j].Index := cr1.Value/(layer.Raster.ClassMax-layer.Raster.ClassMin)*100 ;
                    colormap[j].RGB   := cr1.Color ;
                  end ;
                  lp.GenerateRampEx( layer.Raster.ClassMin, layer.Raster.ClassMax,
                                     colormap, nil
                                    ) ;
                  prms.Pixel.GridShadow := False ;
                end
                else if layer.Raster.Shader.ColorRampType = 'DISCRETE' then begin
                  prms.Pixel.GridShadow := False ;
                  for j := 0 to layer.Raster.Shader.ColorRamps.Count-2 do begin
                    cr1 := layer.Raster.Shader.ColorRamps[j] ;
                    cr2 := layer.Raster.Shader.ColorRamps[j+1] ;
                    prms.Pixel.AltitudeMapZones.Add(
                      ConstructParamAltitudeMapZone( cr1.Value, cr2.Value, cr1.Color, cr1.&Label )
                    ) ;

                    if j = layer.Raster.Shader.ColorRamps.Count-2 then begin
                      prms.Pixel.AltitudeMapZones.Add(
                        ConstructParamAltitudeMapZone( cr2.Value, layer.Raster.ClassMax, cr2.Color, cr2.&Label )
                      ) ;
                    end;
                  end ;
                end
              end ;
            end ;
          end ;

          oViewer.Lock ;
          try
            oViewer.SetCSByEPSG( qgis.MapCanvas.Srid ) ;
            oViewer.FullExtent ;
            oViewer.VisibleExtent := qgis.MapCanvas.Extent ;
          finally
            oViewer.Unlock ;
          end ;
        finally
          if not IsStringEmpty( err ) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERMISSED ), err, 0 ) ;
        end ;
      finally
        FreeObject( qgis ) ;
      end ;
    finally
      FreeObject( xml ) ;
    end ;
  end ;

  procedure TGIS_ProjectQgis.Import(
    const _path : String
  ) ;
  var
    ext : String  ;
  begin
    ext := UpperCase( GetFileExt( _path ) ) ;
    if not SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), _path, 0 ) ;
    end ;

    if (ext = '.QGS') or (ext = '.QGZ') then
      importQgis( _path )
    else
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXTENSION ), ext, 0 ) ;
  end ;

{==================================== END =====================================}
end.

