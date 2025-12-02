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
  Encapsulation of LandXML Layer
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerLandXML ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerLandXML"'}
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
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}

{$REGION 'TGIS_LayerLandXML'}
type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerLandXML = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Encapsulation of a LandXML Layer.
  /// </summary>
  TGIS_LayerLandXML = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_LayerVector )
    private
      FExportType : String ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      procedure fset_UseRTree ( const _value : Boolean
                              ) ; override;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp ; override;
    protected
      // destructor

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create ; override;
    public

      /// <inheritdoc/>
      function  DrawEx       ( const _extent   : TGIS_Extent
                             ) : Boolean ; override;

      /// <inheritdoc/>
      function  PreRecognize ( const _path     : String ;
                                 var _new_path : String
                             ) : Boolean ; override;

      /// <inheritdoc/>
      procedure Build         ( const _path      : String ;
                                const _extent    : TGIS_Extent      ;
                                const _type      : TGIS_ShapeType   ;
                                const _dim       : TGIS_DimensionType
                              ) ; override;

      /// <inheritdoc/>
      procedure ImportLayerEx ( const _layer       : TGIS_LayerVector ;
                                const _extent      : TGIS_Extent      ;
                                const _type        : TGIS_ShapeType   ;
                                const _scope       : String           ;
                                const _shape       : TGIS_Shape       ;
                                const _de9im       : String           ;
                                const _truncated   : Boolean
                              ) ; override;

      /// <inheritdoc/>
      procedure SaveData      ; override;
    public
      /// <summary>
      ///   Name of export type for a shape geometry.
      ///   Use to force conversion to other types than default (D) :
      ///   <para>
      ///     Point - CgPoints (D)
      ///   </para>
      ///   <para>
      ///     Arc - Alignments (D), Contours or PlanFeatures
      ///   </para>
      ///   <para>
      ///     Polygon - Parcels (D)
      ///   </para>
      ///   <para>
      ///     MultiPatch - TIN Surface (D) or Parcels
      ///   </para>
      /// </summary>
      property ExportType : String  read  FExportType
                                    write FExportType ;
   end ;
{$ENDREGION}

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.SysUtils,
    System.Variants,
    System.Math,
    System.Generics.Collections,

    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoLayerSublayer,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoTessellation,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoXmlDoc,
    Lider.CG.GIS.GeoXmlSax ;
{$ENDIF}

{$REGION 'T_LandXML'}
const
  GIS_LANDXML_NAME  = 'LANDXML_NAME' ;
  GIS_LANDXML_TYPE  = 'LANDXML_TYPE' ;

type
  T_CgPoint = class
    ptg : TGIS_Point3D ;
  end ;

  T_LandXML = class( TGIS_Object )
    private
      xDoc        : IXMLDocument ;
      oShp        : TGIS_Shape ;
      oLayer      : TGIS_LayerVector ;
      oSubLayer   : TGIS_LayerVector ;
      oSubLayerEx : TGIS_LayerVector ;
      oSubLayerFx : TGIS_LayerVector ;
      oSubLayerWx : TGIS_LayerVector ;
      cgPoints    : TDictionary<String,T_CgPoint> ;
      iPtn        : Integer ;
      hasName     : Boolean ;
      hasLandName : Boolean ;
      hasFields   : Boolean ;
    private
      FExportType : String ;
    private
      saxWriter   : TGIS_SAXWriter ;
    private
      function  parsePoint        ( const _txt      : String
                                  ) : TGIS_Point3D ;
      procedure addPoints         ( const _txt      : String ;
                                    const _dim      : Integer ;
                                    const _elev     : Double
                                  ) ;
      function  getPoint          ( const _name     : String ;
                                      var _ptg      : TGIS_Point3D
                                  ) : Boolean ;
      procedure parseFace         ( const _txt      : String ;
                                      var _p1       : Integer ;
                                      var _p2       : Integer ;
                                      var _p3       : Integer
                                  ) ;
      procedure loadCoordGeometry ( const _coordGeom : IXMLNode
                                  ) ;
      procedure loadFeaProperties ( const _feature   : IXMLNode
                                  ) ;
      procedure loadFeature       ( const _xmlNode   : IXMLNode
                                  ) ;
      procedure loadLineNode      ( const _geom     : IXMLNode
                                  ) ;
      procedure loadILineNode     ( const _geom     : IXMLNode
                                  ) ;
      procedure loadChainNode     ( const _geom     : IXMLNode
                                  ) ;
      procedure loadArcNode       ( const _geom     : IXMLNode
                                  ) ;
      procedure loadSpiralNode    ( const _geom     : IXMLNode
                                  ) ;

      procedure prepareLayer      ( const _layer    : TGIS_LayerVector
                                  ) ;
      function  getAttributeValue ( const _node     : IXMLNode ;
                                    const _key      : String
                                  ) : OleVariant ;
    protected
      procedure loadHeader        ( const _xmlNode  : IXMLNode
                                  ) ;
      procedure loadAllSurfaces   ( const _xele     : IXMLNode
                                  ) ;
      procedure loadSurfaceGroup  ( const _xmlGroup : IXMLNode
                                  ) ;
      procedure loadSurface       ( const _xmlNode  : IXMLNode
                                  ) ;
      procedure loadSurfaceFaces  ( const _xmlNode  : IXMLNode ;
                                    const _xmlPnts  : IXMLNode
                                  ) ;
      procedure loadContour       ( const _xmlNode  : IXMLNode
                                  ) ;
      procedure loadBreakline     ( const _xmlNode  : IXMLNode
                                  ) ;
      procedure loadWatershed     ( const _xmlNode  : IXMLNode
                                  ) ;

      procedure loadAllCgPoints   ( const _xele     : IXMLNode
                                  ) ;
      procedure loadAllMonuments  ( const _xele     : IXMLNode
                                  ) ;
      procedure loadCgPointGroup  ( const _xmlGroup : IXMLNode
                                  ) ;
      procedure loadMonumentGroup ( const _xmlGroup : IXMLNode
                                  ) ;
      function  loadCgPoint       ( const _xmlNode  : IXMLNode
                                  ) : TGIS_Point3D ;

      procedure loadAllParcels    ( const _xele     : IXMLNode
                                  ) ;
      procedure loadParcelGroup   ( const _xmlGroup : IXMLNode
                                  ) ;
      procedure loadParcel        ( const _xmlNode  : IXMLNode
                                  ) ;

      procedure loadAllAlignments ( const _xele     : IXMLNode
                                  ) ;
      procedure loadAlignmentGroup( const _xmlGroup : IXMLNode
                                  ) ;
      procedure loadAlignment     ( const _xmlNode  : IXMLNode
                                  ) ;
      procedure loadAllPlanFeatures ( const _xele     : IXMLNode ) ;
      procedure loadPlanFeatureGroup( const _xmlGroup : IXMLNode ) ;
      procedure loadPlanFeature     ( const _xmlNode  : IXMLNode ) ;

      procedure loadAllPipeNetworks ( const _xele     : IXMLNode ) ;
      procedure loadPipeNetwork     ( const _xmlNode  : IXMLNode ) ;
    private
      procedure startTag              ( const _qname        : String         ;
                                        const _attributes   : SAXAttributes
                                      ) ;
      procedure endTag                ( const _qname        : String
                                      ) ;
      procedure addAttribute          ( const _attributes   : SAXAttributes ;
                                        const _name         : String ;
                                        const _value        : String
                                      ) ;
      procedure setAttribute          ( const _attributes   : SAXAttributes ;
                                        const _idx          : Integer ;
                                        const _value        : String
                                      ) ;
      procedure writeGeometry         ( const _self         : TGIS_LayerVector ;
                                        const _shp          : TGIS_Shape
                                      ) ;
      procedure writeProperties       ( const _self         : TGIS_LayerVector ;
                                        const _shp          : TGIS_Shape
                                      ) ;
      procedure writeHeader           ( const _self         : TGIS_LayerVector ;
                                        const _layer        : TGIS_LayerVector
                                      ) ;
      function writeFeatures          ( const _self         : TGIS_LayerVector ;
                                        const _layer        : TGIS_LayerVector ;
                                        const _extent       : TGIS_Extent      ;
                                        const _type         : TGIS_ShapeType   ;
                                        const _scope        : String           ;
                                        const _shape        : TGIS_Shape       ;
                                        const _de9im        : String           ;
                                        const _truncated    : Boolean
                                      ) : Boolean ;
      function nameValue              ( const _shp          : TGIS_Shape
                                      ) : String ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      procedure LoadLandXMLFile( const _path        : String ;
                                 const _layer       : TGIS_LayerVector
                                ) ;
      procedure SaveLandXMLFile( const _path        : String ;
                                 const _self        : TGIS_LayerVector ;
                                 const _layer       : TGIS_LayerVector ;
                                 const _extent      : TGIS_Extent      ;
                                 const _type        : TGIS_ShapeType   ;
                                 const _scope       : String           ;
                                 const _shape       : TGIS_Shape       ;
                                 const _de9im       : String           ;
                                 const _truncated   : Boolean
                                ) ;
    public
      property ExportType : String  read  FExportType
                                    write FExportType ;
    end ;

//==============================================================================
// T_LandXML
//==============================================================================

  constructor T_LandXML.Create ;
  begin
    inherited;

    cgPoints := TDictionary<String,T_CgPoint>.Create ;

    xDoc := TGIS_XMLDocument.Create ;

    if not assigned( xDoc ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_FILEREAD ), 'MSXML parser', 0
      ) ;

    xDoc.Active    := True ;
    xDoc.Version   := '1.0' ;
    xDoc.Encoding  := 'utf-8' ;
    FExportType    := '' ;
  end ;

  procedure T_LandXML.doDestroy ;
  {$IFNDEF OXYGENE}
    var
      itm : TPair<String,T_CgPoint> ;
  {$ENDIF}
  begin
    {$IFNDEF NEXTGEN}
      for itm in cgPoints do begin
        FreeObjectNotNil( itm.Value ) ;
      end ;
    {$ENDIF}

    cgPoints.Clear ;
    FreeObject( cgPoints ) ;
    FreeObject( xDoc ) ;

    inherited ;
  end ;

  function T_LandXML.getAttributeValue(
    const _node : IXMLNode ;
    const _key  : String
  ) : OleVariant ;
  var
    attr : IXMLNode ;
  begin
    Result := Unassigned ;

    if not assigned( _node ) then exit ;

    attr := _node.AttributeNodes.FindNode( _key ) ;
    if assigned( attr ) then
      Result := attr.Text
  end ;

  function T_LandXML.getPoint(
    const _name     : String ;
      var _ptg      : TGIS_Point3D
  ) : Boolean ;
  var
    cgPtg : T_CgPoint ;
  begin
    Result := cgPoints.TryGetValue( _name, cgPtg ) ;
    if Result then
      _ptg := T_CgPoint( cgPtg ).ptg ;
  end ;

  procedure T_LandXML.loadHeader(
    const _xmlNode : IXMLNode
  ) ;
  var
    cs : IXMLNode ;
  begin
    cs := _xmlNode.ChildNodes.FindNode('CoordinateSystem') ;
    if not assigned( cs ) then exit ;

    oLayer.SetCSByWKT( VarToString( cs.Attributes['ogcWktCode'] ) ) ;
  end ;

  procedure T_LandXML.loadAllSurfaces(
    const _xele : IXMLNode
  ) ;
  var
    surList   : IXMLNodeList ;
    xmlGroup  : IXMLNode ;
    len, i    : Integer ;
    lv        : TGIS_LayerVector ;
  begin
    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'Surfaces' ;
    lv.Caption              := 'Surfaces' ;
    lv.Viewer               := oLayer.Viewer ;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Polygon ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params ) ;
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayer := lv ;

    oSubLayer.Params.Area.Pattern      := TGIS_BrushStyle.Clear ;
    oSubLayer.Params.Area.OutlineColor := TGIS_Color.Green ;
    oSubLayer.Params.Labels.Field      := GIS_LANDXML_NAME ;

    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'Contours' ;
    lv.Caption              := 'Contours' ;
    lv.Viewer               := oLayer.Viewer;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Arc ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params );
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayerEx := lv ;
    oSubLayerEx.Params.Line.Color   := TGIS_Color.FromBGR( $33405C ) ;
    oSubLayerEx.Params.Labels.Field := GIS_LANDXML_NAME ;
    oSubLayerEx.Params.Labels.Alignment := TGIS_LabelAlignment.Follow ;
    oSubLayerEx.Params.Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.MiddleCenter ) ;

    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'Breaklines' ;
    lv.Caption              := 'Breaklines' ;
    lv.Viewer               := oLayer.Viewer;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Arc ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params );
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayerFx := lv ;
    oSubLayerFx.Params.Labels.Field := GIS_LANDXML_NAME ;

    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'Watersheds' ;
    lv.Caption              := 'Watersheds' ;
    lv.Viewer               := oLayer.Viewer ;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Polygon ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params ) ;
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayerWx := lv ;
    oSubLayerWx := lv ;
    oSubLayerWx.Params.Labels.Field := GIS_LANDXML_NAME ;
    oSubLayerWx.Params.Area.Pattern      := TGIS_BrushStyle.Clear ;
    oSubLayerWx.Params.Area.OutlineColor := TGIS_Color.Blue ;

    surList   := _xele.ChildNodes ;
    len       := surList.Count ;

    for i := 0 to len - 1 do begin
      xmlGroup := surList[ i ] ;

      if xmlGroup.NodeName = 'Surfaces' then
        loadSurfaceGroup( xmlGroup ) ;
    end ;
  end ;

  procedure T_LandXML.loadSurfaceGroup(
    const _xmlGroup : IXMLNode
  ) ;
  var
    surList     : IXMLNodeList ;
    xmlSurface  : IXMLNode ;
    len, i      : Integer ;
  begin
    surList := _xmlGroup.ChildNodes ;
    len     := surList.Count ;

    for i := 0 to len - 1 do begin
      xmlSurface := surList[ i ] ;

      if xmlSurface.NodeName = 'Surface' then
        loadSurface( xmlSurface )
    end ;
  end ;

  procedure T_LandXML.loadSurface(
    const _xmlNode : IXMLNode
  ) ;
  var
    xmlFacesNode  : IXMLNode ;
    xmlPntsNode   : IXMLNode ;
    xmlDefNode    : IXMLNode ;
    xmlCntsNode   : IXMLNode ;
    xmlCntNode    : IXMLNode ;
    i             : Integer ;
  begin
    xmlDefNode := _xmlNode.ChildNodes.FindNode('Definition') ;
    if assigned( xmlDefNode ) then begin

      oShp := oLayer.CreateShape(
        TGIS_ShapeType.MultiPatch,
        TGIS_DimensionType.XYZ
      ) ;
      oShp.Lock( TGIS_Lock.Projection ) ;
      try
        xmlPntsNode  := xmlDefNode.ChildNodes.FindNode( 'Pnts' ) ;

        for i := 0 to xmlDefNode.ChildNodes.Count-1 do begin
          xmlFacesNode := xmlDefNode.ChildNodes[i] ;
          if xmlFacesNode.NodeName = 'Faces' then
            loadSurfaceFaces( xmlFacesNode, xmlPntsNode ) ;
        end ;

        loadFeature( _xmlNode ) ;

        oShp.SetField( GIS_LANDXML_NAME, getAttributeValue( _xmlNode, 'name' ) ) ;
        oShp.SetField( GIS_LANDXML_TYPE, _xmlNode.NodeName ) ;
      finally
        oShp.Layer := oSubLayer ;
        oShp.Unlock ;
      end ;
    end ;

    xmlDefNode := _xmlNode.ChildNodes.FindNode('SourceData') ;
    if assigned( xmlDefNode ) then begin
      xmlCntsNode := xmlDefNode.ChildNodes.FindNode('Contours') ;
      if assigned( xmlCntsNode ) then begin
        for i := 0 to xmlCntsNode.ChildNodes.Count-1 do begin
          xmlCntNode := xmlCntsNode.ChildNodes[i] ;
          if xmlCntNode.NodeName = 'Contour' then
            loadContour( xmlCntNode ) ;
        end ;
      end ;

      xmlCntsNode := xmlDefNode.ChildNodes.FindNode('Breaklines') ;
      if assigned( xmlCntsNode ) then begin
        for i := 0 to xmlCntsNode.ChildNodes.Count-1 do begin
          xmlCntNode := xmlCntsNode.ChildNodes[i] ;
          if xmlCntNode.NodeName = 'Breakline' then
            loadBreakline( xmlCntNode ) ;
        end ;
      end ;
    end ;

    xmlCntsNode := _xmlNode.ChildNodes.FindNode('Watersheds') ;
    if assigned( xmlCntsNode ) then begin
      for i := 0 to xmlCntsNode.ChildNodes.Count-1 do begin
        xmlCntNode := xmlCntsNode.ChildNodes[i] ;
        if xmlCntNode.NodeName = 'Watershed' then
          loadWatershed( xmlCntNode ) ;
      end ;
    end ;

  end ;

  procedure T_LandXML.loadContour(
    const _xmlNode  : IXMLNode
  ) ;
  var
    pntList    : IXMLNode ;
    xmlContour : IXMLNode ;
    elev       : Double ;
  begin
    xmlContour  := _xmlNode ;

    oShp := oLayer.CreateShape(
      TGIS_ShapeType.Arc,
      TGIS_DimensionType.XYZ
    ) ;
    oShp.Lock( TGIS_Lock.Projection ) ;
    try
      oShp.AddPart ;

      pntList := xmlContour.ChildNodes.FindNode( 'PntList2D' ) ;
      elev    := DotStrToFloat( VarToString( getAttributeValue( xmlContour, 'elev' ) ) ) ;

      if assigned( pntList ) then
        addPoints( pntList.Text, 2, elev ) ;

      loadFeature( _xmlNode ) ;

      oShp.SetField( GIS_LANDXML_NAME, elev ) ;
      oShp.SetField( GIS_LANDXML_TYPE, xmlContour.NodeName ) ;
    finally
      oShp.Layer := oSubLayerEx ;
      oShp.Unlock ;
    end ;
  end ;

  procedure T_LandXML.loadBreakline(
    const _xmlNode  : IXMLNode
  ) ;
  var
    pntList      : IXMLNode ;
    xmlBreakline : IXMLNode ;
  begin
    xmlBreakline  := _xmlNode ;

    oShp := oLayer.CreateShape(
      TGIS_ShapeType.Arc,
      TGIS_DimensionType.XYZ
    ) ;
    oShp.Lock( TGIS_Lock.Projection ) ;
    try
      oShp.AddPart ;

      pntList := xmlBreakline.ChildNodes.FindNode( 'PntList3D' ) ;
      if assigned( pntList ) then
        addPoints( pntList.Text, 3, 0 ) ;

      loadFeature( _xmlNode ) ;

      oShp.SetField( GIS_LANDXML_NAME, getAttributeValue( xmlBreakline, 'name' ) ) ;
      oShp.SetField( GIS_LANDXML_TYPE, xmlBreakline.NodeName ) ;
    finally
      oShp.Layer := oSubLayerFx ;
      oShp.Unlock ;
    end ;
  end ;

  procedure T_LandXML.loadWatershed(
    const _xmlNode  : IXMLNode
  ) ;
  var
    pntList      : IXMLNode ;
    xmlWatershed : IXMLNode ;
  begin
    xmlWatershed  := _xmlNode ;

    oShp := oLayer.CreateShape(
      TGIS_ShapeType.Polygon,
      TGIS_DimensionType.XYZ
    ) ;
    oShp.Lock( TGIS_Lock.Projection ) ;
    try
      oShp.AddPart ;

      pntList := xmlWatershed.ChildNodes.FindNode( 'PntList2D' ) ;

      if assigned( pntList ) then
        addPoints( pntList.Text, 2, 0 ) ;

      loadFeature( _xmlNode ) ;

      oShp.SetField( GIS_LANDXML_NAME, getAttributeValue( xmlWatershed, 'name' ) ) ;
      oShp.SetField( GIS_LANDXML_TYPE, xmlWatershed.NodeName ) ;
    finally
      oShp.Layer := oSubLayerWx ;
      oShp.Unlock ;
    end ;
  end ;

  procedure T_LandXML.loadSurfaceFaces(
    const _xmlNode  : IXMLNode ;
    const _xmlPnts  : IXMLNode
  ) ;
  var
    cgList    : IXMLNodeList ;
    cgNode    : IXMLNode ;
    len, i    : Integer ;
    p1,
    p2,
    p3        : Integer ;
    pnts      : TDictionary<Integer,T_CgPoint> ;
    ptg       : T_CgPoint ;
    {$IFNDEF OXYGENE}
      itm     : TPair<Integer,T_CgPoint> ;
    {$ENDIF}
  begin
    if not assigned( _xmlNode ) then exit ;

    cgList := _xmlNode.ChildNodes ;
    len    := cgList.Count ;

    pnts := TDictionary<Integer,T_CgPoint>.Create ;
    try
      for i := 0 to _xmlPnts.ChildNodes.Count - 1 do begin
        ptg := T_CgPoint.Create ;
        T_CgPoint( ptg ).ptg := parsePoint( _xmlPnts.ChildNodes[ i ].Text ) ;
        pnts.Add( VarToInt32( _xmlPnts.ChildNodes[ i ].Attributes[ 'id' ] ), ptg ) ;
      end ;

      for i := 0 to len - 1 do begin
        cgNode := cgList[ i ] ;
        if cgNode.NodeName = 'F' then begin
          if cgNode.HasAttribute('i') then continue ;

          oShp.AddPart ;

          parseFace( cgNode.Text, p1, p2, p3 ) ;

          if pnts.TryGetValue( p1, ptg ) then
            oShp.AddPoint3D( T_CgPoint( ptg ).ptg ) ;

          if pnts.TryGetValue( p2, ptg ) then
            oShp.AddPoint3D( T_CgPoint( ptg ).ptg ) ;

          if pnts.TryGetValue( p3, ptg ) then
            oShp.AddPoint3D( T_CgPoint( ptg ).ptg ) ;
        end ;
      end ;
    finally
      {$IFNDEF NEXTGEN}
        for itm in pnts do begin
          FreeObjectNotNil( itm.Value ) ;
        end ;
      {$ENDIF}
      pnts.Clear ;
      FreeObject( pnts ) ;
    end ;
  end ;

  procedure T_LandXML.loadAllAlignments(
    const _xele: IXMLNode
  );
  var
    aliList   : IXMLNodeList ;
    xmlGroup  : IXMLNode ;
    len, i    : Integer ;
    lv        : TGIS_LayerVector ;
  begin
    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'Alignments' ;
    lv.Caption              := 'Alignments' ;
    lv.Viewer               := oLayer.Viewer;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Arc ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params );
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayer := lv ;
    oSubLayer.Params.Line.Color   := TGIS_Color.Red ;
    oSubLayer.Params.Labels.Field := GIS_LANDXML_NAME ;

    aliList := _xele.ChildNodes ;
    len     := aliList.Count ;

    for i := 0 to len - 1 do begin
      xmlGroup := aliList[ i ] ;

      if xmlGroup.NodeName = 'Alignments' then
        loadAlignmentGroup( xmlGroup ) ;
    end ;
  end ;

  procedure T_LandXML.loadAlignmentGroup(
    const _xmlGroup: IXMLNode
  ) ;
  var
    aliList      : IXMLNodeList ;
    xmlAlignment : IXMLNode ;
    len, i       : Integer ;
  begin
    aliList := _xmlGroup.ChildNodes ;
    len     := aliList.Count ;

    for i := 0 to len - 1 do begin
      xmlAlignment  := aliList[ i ] ;

      oShp := oLayer.CreateShape(
        TGIS_ShapeType.Arc,
        TGIS_DimensionType.XYZ
      ) ;
      oShp.Lock( TGIS_Lock.Projection ) ;
      try
        oShp.AddPart ;

        loadAlignment( xmlAlignment ) ;

        oShp.SetField( GIS_LANDXML_NAME, getAttributeValue( xmlAlignment, 'name' ) ) ;
        oShp.SetField( GIS_LANDXML_TYPE, xmlAlignment.NodeName ) ;
      finally
        oShp.Layer := oSubLayer ;
        oShp.Unlock ;
      end ;
    end ;
  end ;

  procedure T_LandXML.loadAlignment(
    const _xmlNode : IXMLNode
  );
  var
    coordGeom : IXMLNode ;
  begin
    coordGeom := _xmlNode.ChildNodes.FindNode( 'CoordGeom' ) ;

    loadCoordGeometry( coordGeom ) ;
    loadFeature( _xmlNode ) ;
  end ;

  procedure T_LandXML.loadAllPlanFeatures(
    const _xele : IXMLNode
  ) ;
  var
    aliList   : IXMLNodeList ;
    xmlGroup  : IXMLNode ;
    len, i    : Integer ;
    lv        : TGIS_LayerVector ;
  begin
    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'PlanFeatures' ;
    lv.Caption              := 'PlanFeatures' ;
    lv.Viewer               := oLayer.Viewer;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Arc ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params );
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayer := lv ;
    oSubLayer.Params.Line.Color   := TGIS_Color.Blue ;
    oSubLayer.Params.Labels.Field := GIS_LANDXML_NAME ;

    aliList := _xele.ChildNodes ;
    len     := aliList.Count ;

    for i := 0 to len - 1 do begin
      xmlGroup := aliList[ i ] ;

      if xmlGroup.NodeName = 'PlanFeatures' then
        loadPlanFeatureGroup( xmlGroup ) ;
    end ;
  end ;

  procedure T_LandXML.loadPlanFeatureGroup(
    const _xmlGroup : IXMLNode
  );
  var
    aliList      : IXMLNodeList ;
    xmlPlanFeature : IXMLNode ;
    len, i       : Integer ;
  begin
    aliList := _xmlGroup.ChildNodes ;
    len     := aliList.Count ;

    for i := 0 to len - 1 do begin
      xmlPlanFeature  := aliList[ i ] ;

      oShp := oLayer.CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
      oShp.Lock( TGIS_Lock.Projection ) ;
      try
        oShp.AddPart ;

        loadPlanFeature( xmlPlanFeature ) ;

        oShp.SetField( GIS_LANDXML_NAME, getAttributeValue( xmlPlanFeature, 'name' ) ) ;
        oShp.SetField( GIS_LANDXML_TYPE, xmlPlanFeature.NodeName ) ;
      finally
        oShp.Layer := oSubLayer ;
        oShp.Unlock ;
      end ;
    end ;
  end ;

  procedure T_LandXML.loadPlanFeature(
    const _xmlNode : IXMLNode
  ) ;
  var
    coordGeom : IXMLNode ;
  begin
    coordGeom := _xmlNode.ChildNodes.FindNode( 'CoordGeom' ) ;

    loadCoordGeometry( coordGeom ) ;
    loadFeature( _xmlNode ) ;
  end ;

  procedure T_LandXML.loadAllPipeNetworks(
    const _xele : IXMLNode
  ) ;
  var
    aliList   : IXMLNodeList ;
    xmlGroup  : IXMLNode ;
    len, i    : Integer ;
    lv        : TGIS_LayerVector ;
  begin
    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'PipeNetworks' ;
    lv.Caption              := 'PipeNetworks' ;
    lv.Viewer               := oLayer.Viewer;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Arc ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params );
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayer := lv ;
    oSubLayer.Params.Line.Color   := TGIS_Color.Blue ;
    oSubLayer.Params.Labels.Field := GIS_LANDXML_NAME ;

    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'Structures' ;
    lv.Caption              := 'Structures' ;
    lv.Viewer               := oLayer.Viewer;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Point ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params );
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    lv.Params.Marker.Size   := 40 ;
    lv.Params.Marker.Style  := TGIS_MarkerStyle.Circle ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayerEx := lv ;

    oSubLayerEx.Params.Labels.Field := GIS_LANDXML_NAME ;

    aliList := _xele.ChildNodes ;
    len     := aliList.Count ;

    for i := 0 to len - 1 do begin
      xmlGroup := aliList[ i ] ;

      if xmlGroup.NodeName = 'PipeNetworks' then
        loadPipeNetwork( xmlGroup ) ;
    end ;
  end ;

  procedure T_LandXML.loadPipeNetwork(
    const _xmlNode : IXMLNode
  );
  var
    aliList        : IXMLNodeList ;
    xmlPipeNetwork : IXMLNode ;
    xmlStructs     : IXMLNode ;
    xmlStruct      : IXMLNode ;
    xmlCenter      : IXMLNode ;
    xmlPipes       : IXMLNode ;
    xmlPipe        : IXMLNode ;
    len, i, j      : Integer ;
    pnts           : TDictionary<String,T_CgPoint> ;
    ptg            : T_CgPoint ;
    {$IFNDEF OXYGENE}
      itm          : TPair<String,T_CgPoint> ;
    {$ENDIF}
    elev           : Double ;
  begin
    aliList := _xmlNode.ChildNodes ;
    len     := aliList.Count ;

    for i := 0 to len - 1 do begin
      xmlPipeNetwork  := aliList[ i ] ;

      xmlStructs := xmlPipeNetwork.ChildNodes.FindNode( 'Structs' ) ;

      if not assigned( xmlStructs ) then continue ;

      pnts := TDictionary<String,T_CgPoint>.Create ;
      try
        for j := 0 to xmlStructs.ChildNodes.Count-1 do begin
          xmlStruct := xmlStructs.ChildNodes[j] ;
          if xmlStruct.NodeName = 'Struct' then begin
            xmlCenter := xmlStruct.ChildNodes.FindNode( 'Center' ) ;
            elev      := DotStrToFloat( VarToString( getAttributeValue( xmlStruct, 'elevRim' ) ) ) ;

            ptg := T_CgPoint.Create ;
            T_CgPoint( ptg ).ptg := parsePoint( xmlCenter.Text ) ;
            T_CgPoint( ptg ).ptg.Z := elev ;
            pnts.Add( VarToString( xmlStruct.Attributes[ 'name' ] ), ptg ) ;

            oShp := oLayer.CreateShape(
              TGIS_ShapeType.Point,
              TGIS_DimensionType.XYZ
            ) ;
            oShp.Lock( TGIS_Lock.Projection ) ;
            try
              oShp.AddPart ;
              oShp.AddPoint3D( T_CgPoint( ptg ).ptg ) ;

              loadFeature( xmlStruct ) ;
              oShp.SetField( GIS_LANDXML_NAME, getAttributeValue( xmlStruct, 'name' ) ) ;
              oShp.SetField( GIS_LANDXML_TYPE, xmlStruct.NodeName ) ;
            finally
              oShp.Layer := oSubLayerEx ;
              oShp.Unlock ;
            end ;
          end ;
        end ;

        xmlPipes := xmlPipeNetwork.ChildNodes.FindNode( 'Pipes' ) ;
        if not assigned( xmlPipes ) then continue ;

        for j := 0 to xmlPipes.ChildNodes.Count-1 do begin
          xmlPipe := xmlPipes.ChildNodes[j] ;
          if xmlPipe.NodeName = 'Pipe' then begin
            oShp := oLayer.CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
            oShp.Lock( TGIS_Lock.Projection ) ;
            try
              oShp.AddPart ;

              if pnts.TryGetValue( VarToString(getAttributeValue( xmlPipe, 'refStart' )), ptg ) then
                oShp.AddPoint3D( T_CgPoint( ptg ).ptg ) ;

              if pnts.TryGetValue( VarToString(getAttributeValue( xmlPipe, 'refEnd' )), ptg ) then
                oShp.AddPoint3D( T_CgPoint( ptg ).ptg ) ;

              loadFeature( xmlPipe ) ;
              oShp.SetField( GIS_LANDXML_NAME, getAttributeValue( xmlPipe, 'name' ) ) ;
              oShp.SetField( GIS_LANDXML_TYPE, xmlPipe.NodeName ) ;
            finally
              oShp.Layer := oSubLayer ;
              oShp.Unlock ;
            end ;
          end ;
        end ;
      finally
        {$IFNDEF NEXTGEN}
          for itm in pnts do begin
            FreeObjectNotNil( itm.Value ) ;
          end ;
        {$ENDIF}
        pnts.Clear ;
        FreeObject( pnts ) ;
      end ;
    end ;
  end ;

  procedure T_LandXML.loadAllCgPoints(
    const _xele : IXMLNode
  ) ;
  var
    ptgList   : IXMLNodeList ;
    xmlGroup  : IXMLNode ;
    len, i    : Integer ;
    lv        : TGIS_LayerVector ;
  begin
    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'CgPoints' ;
    lv.Caption              := 'CgPoints' ;
    lv.Viewer               := oLayer.Viewer;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Point ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params );
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    lv.Params.Marker.Size   := 40 ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayer := lv ;

    oSubLayer.Params.Labels.Field := GIS_LANDXML_NAME ;

    ptgList := _xele.ChildNodes ;
    len     := ptgList.Count ;

    for i := 0 to len - 1 do begin
      xmlGroup := ptgList[ i ] ;

      if xmlGroup.NodeName = 'CgPoints' then
        loadCgPointGroup( xmlGroup ) ;
    end ;
  end ;

  procedure T_LandXML.loadAllMonuments(
    const _xele : IXMLNode
  ) ;
  var
    ptgList   : IXMLNodeList ;
    xmlGroup  : IXMLNode ;
    len, i    : Integer ;
    lv        : TGIS_LayerVector ;
  begin
    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'Monuments' ;
    lv.Caption              := 'Monuments' ;
    lv.Viewer               := oLayer.Viewer;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Point ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params );
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    lv.Params.Marker.Size   := 40 ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayer := lv ;

    oSubLayer.Params.Labels.Field := GIS_LANDXML_NAME ;

    ptgList := _xele.ChildNodes ;
    len     := ptgList.Count ;

    for i := 0 to len - 1 do begin
      xmlGroup := ptgList[ i ] ;

      if xmlGroup.NodeName = 'Monuments' then
        loadMonumentGroup( xmlGroup ) ;
    end ;
  end ;

  procedure T_LandXML.loadCgPointGroup(
    const _xmlGroup : IXMLNode
  ) ;
  var
    ptgList      : IXMLNodeList ;
    xmlPtg       : IXMLNode ;
    len, i       : Integer ;
    ptg          : TGIS_Point3D ;
    name         : Variant ;
    sname        : String ;
    cgPtg        : T_CgPoint ;
  begin
    ptgList := _xmlGroup.ChildNodes ;
    len     := ptgList.Count ;

    for i := 0 to len - 1 do begin
      xmlPtg  := ptgList[ i ] ;

      if xmlPtg.NodeName = 'CgPoint' then begin
        if xmlPtg.HasAttribute( 'pntRef' ) then continue ;

        oShp := oLayer.CreateShape(
          TGIS_ShapeType.Point,
          TGIS_DimensionType.XYZ
        ) ;
        oShp.Lock( TGIS_Lock.Projection ) ;
        try
          oShp.AddPart ;

          ptg  := loadCgPoint( xmlPtg ) ;
          name := getAttributeValue( xmlPtg, 'name' ) ;

          oShp.SetField( GIS_LANDXML_NAME, name ) ;
          oShp.SetField( GIS_LANDXML_TYPE, xmlPtg.NodeName ) ;
        finally
          oShp.Layer := oSubLayer ;
          oShp.Unlock ;
        end ;

        cgPtg := T_CgPoint.Create ;
        cgPtg.ptg := ptg ;
        sname := VarToString( name ) ;
        if not IsStringEmpty( sname ) then
          cgPoints.Add( sname, cgPtg )
        else
          FreeObject( cgPtg ) ;
      end
      else if xmlPtg.NodeName = 'CgPoints' then
        loadCgPointGroup( xmlPtg ) ;
    end ;

    loadFeature( _xmlGroup ) ;

  end ;

  procedure T_LandXML.loadMonumentGroup(
    const _xmlGroup : IXMLNode
  ) ;
  var
    monuments    : IXMLNodeList ;
    len, i       : Integer ;
    monument     : IXMLNode ;
    pntRef       : IXMLNode ;
    ptg          : TGIS_Point3D ;
  begin
    monuments := _xmlGroup.ChildNodes ;
    len       := monuments.Count ;

    for i := 0 to len - 1 do begin
      monument  := monuments[ i ] ;

      if monument.NodeName = 'Monument' then begin

        oShp := oLayer.CreateShape(
          TGIS_ShapeType.Point,
          TGIS_DimensionType.XYZ
        ) ;
        oShp.Lock( TGIS_Lock.Projection ) ;
        try
          oShp.AddPart ;

          pntRef := monument.AttributeNodes.FindNode( 'pntRef' ) ;

          if pntRef <> nil then
            getPoint( VarToString( pntRef.NodeValue ), ptg ) ;

          oShp.AddPoint3D( ptg ) ;
          oShp.SetField( GIS_LANDXML_NAME, VarToString( getAttributeValue( monument, 'name' ) ) ) ;
          oShp.SetField( GIS_LANDXML_TYPE, monument.NodeName ) ;
        finally
          oShp.Layer := oSubLayer ;
          oShp.Unlock ;
        end ;
      end;
    end ;

  end ;

  procedure T_LandXML.parseFace(
    const _txt : String  ;
      var _p1  : Integer ;
      var _p2  : Integer ;
      var _p3  : Integer
  ) ;
  var
    tkn : TGIS_Tokenizer ;
    i   : Integer ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      i := 0 ;
      tkn.Execute( _txt, [ ' ' ] ) ;

      _p1 := StrToInt( tkn.Result[ i ] ) ;
      inc( i );
      _p2 := StrToInt( tkn.Result[ i ] ) ;
      inc( i );
      _p3 := StrToInt( tkn.Result[ i ] ) ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  function T_LandXML.parsePoint(
    const _txt : String
  ) : TGIS_Point3D ;
  var
    tkn : TGIS_Tokenizer ;
    i   : Integer ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D;
    {$ENDIF}
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( _txt, [ ' ', ',' ] ) ;
      i := 0 ;

      while i < tkn.Result.Count - 1 do begin
        Result.Y := DotStrToFloat( tkn.Result[ i ] ) ;
        inc( i );
        Result.X := DotStrToFloat( tkn.Result[ i ] ) ;
        inc( i );
        if tkn.Result.Count > 2 then begin
          Result.Z := DotStrToFloat( tkn.Result[ i ] ) ;
          inc( i ) ;
        end ;
        Result.M := 0 ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure T_LandXML.addPoints(
    const _txt      : String ;
    const _dim      : Integer ;
    const _elev     : Double
  ) ;
  var
    tkn : TGIS_Tokenizer ;
    i   : Integer ;
    ptg : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      ptg := new TGIS_Point3D;
    {$ENDIF}
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( _txt, [ ' ' ] ) ;
      i := 0 ;

      while i < tkn.Result.Count - 1 do begin
        ptg.Y := DotStrToFloat( tkn.Result[ i ] ) ;
        inc( i );
        ptg.X := DotStrToFloat( tkn.Result[ i ] ) ;
        inc( i );
        if _dim > 2 then begin
          ptg.Z := DotStrToFloat( tkn.Result[ i ] ) ;
          inc( i ) ;
        end
        else
          ptg.Z := _elev ;

        ptg.M := 0 ;
        oShp.AddPoint3D( _TGIS_Point3D(ptg) ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  function T_LandXML.loadCgPoint(
    const _xmlNode : IXMLNode
  ) : TGIS_Point3D ;
  begin
    Result := parsePoint( _xmlNode.Text ) ;

    oShp.AddPoint3D( Result ) ;
  end ;

  procedure T_LandXML.loadAllParcels(
    const _xele : IXMLNode
  ) ;
  var
    parList   : IXMLNodeList ;
    xmlGroup  : IXMLNode ;
    len, i    : Integer ;
    lv        : TGIS_LayerVector ;
  begin
    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name                 := 'Parcels' ;
    lv.Caption              := 'Parcels' ;
    lv.Viewer               := oLayer.Viewer;
    lv.SupportedShapes      := GisGetShapeType( TGIS_ShapeType.Polygon ) ;
    lv.SupportedDimensions  := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;
    lv.ParentLayer          := oLayer ;
    lv.Params.Assign( oLayer.Params );
    lv.IgnoreShapeParams    := oLayer.IgnoreShapeParams ;
    oLayer.SubLayers.Add( lv ) ;

    oSubLayer := lv ;

    oSubLayer.Params.Area.Pattern      := TGIS_BrushStyle.Clear ;
    oSubLayer.Params.Area.OutlineColor := TGIS_Color.Green ;
    oSubLayer.Params.Labels.Field      := GIS_LANDXML_NAME ;

    parList := _xele.ChildNodes ;
    len     := parList.Count ;

    for i := 0 to len - 1 do begin
      xmlGroup := parList[ i ] ;

      if xmlGroup.NodeName = 'Parcels' then
        loadParcelGroup( xmlGroup ) ;
    end ;
  end ;

  procedure T_LandXML.loadParcelGroup(
    const _xmlGroup : IXMLNode
  ) ;
  var
    parList   : IXMLNodeList ;
    xmlParcel : IXMLNode ;
    len, i    : Integer ;
    ptype     : String ;
  begin
    parList := _xmlGroup.ChildNodes ;
    len     := parList.Count ;

    for i := 0 to len - 1 do begin
      xmlParcel := parList[ i ] ;

      ptype := VarToString( getAttributeValue( xmlParcel, 'parcelFormat' ) ) ;
      if ptype = 'Volumertric' then
        oShp := oLayer.CreateShape(
          TGIS_ShapeType.MultiPatch,
          TGIS_DimensionType.XYZ
        )
      else
        oShp := oLayer.CreateShape(
          TGIS_ShapeType.Polygon,
          TGIS_DimensionType.XYZ
        ) ;

      oShp.Lock( TGIS_Lock.Projection ) ;
      try
        loadParcel( xmlParcel ) ;

        oShp.SetField( GIS_LANDXML_NAME, getAttributeValue( xmlParcel, 'name' ) ) ;
        oShp.SetField( GIS_LANDXML_TYPE, xmlParcel.NodeName ) ;
      finally
        oShp.Layer := oSubLayer ;
        oShp.Unlock ;
      end ;
    end ;
  end ;

  procedure T_LandXML.loadParcel(
    const _xmlNode : IXMLNode
  ) ;
  var
    coordGeom  : IXMLNode ;
    volumeGeom : IXMLNode ;
    i, j       : Integer ;
  begin
    coordGeom := _xmlNode.ChildNodes.FindNode( 'CoordGeom' ) ;

    if assigned( coordGeom ) then begin
      oShp.AddPart ;
      loadCoordGeometry( coordGeom ) ;
      loadFeature( _xmlNode ) ;
    end
    else begin
      for i := 0 to _xmlNode.ChildNodes.Count-1 do begin
        volumeGeom := _xmlNode.ChildNodes[i] ;
        if volumeGeom.NodeName = 'VolumeGeom' then begin
          for j := 0 to volumeGeom.ChildNodes.Count-1 do begin
            coordGeom := volumeGeom.ChildNodes[j] ;
            if coordGeom.NodeName = 'CoordGeom' then begin
              oShp.AddPart ;
              loadCoordGeometry( coordGeom ) ;
            end ;
          end ;
        end ;
      end ;
      loadFeature( _xmlNode ) ;
    end ;
  end ;

  procedure T_LandXML.loadFeature(
    const _xmlNode   : IXMLNode
  ) ;
  var
    feature : IXMLNode ;
  begin
    feature := _xmlNode.ChildNodes.FindNode( 'Feature' ) ;
    loadFeaProperties( feature ) ;
  end;

  procedure T_LandXML.loadFeaProperties(
    const _feature : IXMLNode
  ) ;
  var
    propList  : IXMLNodeList ;
    prop      : IXMLNode ;
    pname     : String ;
    pvalue    : String ;
    len, i    : Integer ;
    res       : Integer ;
  begin
    if not assigned( _feature ) then exit ;

    propList := _feature.ChildNodes ;
    len      := propList.Count ;

    for i := 0 to len - 1 do begin
      prop  := propList[ i ] ;

      pname  := VarToString( getAttributeValue( prop, 'label' ) );
      pvalue := VarToString( getAttributeValue( prop, 'value' ) );

      res := oLayer.FindField( pname ) ;
      if res = -1 then
        oLayer.AddFieldInternal( pname, TGIS_FieldType.String, 1, 0);

      if assigned( oShp ) and not IsStringEmpty( pvalue ) then
        oShp.SetField( pname, pvalue ) ;

    end ;
  end ;

  procedure T_LandXML.loadCoordGeometry(
    const _coordGeom : IXMLNode
  ) ;
  var
    geomList  : IXMLNodeList ;
    geom      : IXMLNode ;
    geomName  : String ;
    len, i    : Integer ;
  begin
    if not assigned( _coordGeom ) then exit ;

    geomList := _coordGeom.ChildNodes ;
    len      := geomList.Count ;

    for i := 0 to len - 1 do begin
      geom     := geomList[ i ] ;
      geomName := geom.NodeName ;

      if      geomName = 'Curve'         then
        loadArcNode( geom )
      else if geomName = 'Spiral'        then
        loadSpiralNode( geom )
      else if geomName = 'Line'          then
        loadLineNode( geom )
      else if geomName = 'IrregularLine' then
        loadILineNode( geom )
      else if geomName = 'Chain'         then
        loadChainNode( geom )
    end ;
  end ;

  procedure T_LandXML.loadLineNode(
    const _geom : IXMLNode
  ) ;
  var
    startNode : IXMLNode ;
    endNode   : IXMLNode ;
    attrStart : IXMLNode ;
    attrEnd   : IXMLNode ;
    startPtg  : TGIS_Point3D ;
    endPtg    : TGIS_Point3D ;
  begin
    startNode := _geom.ChildNodes.FindNode( 'Start' ) ;
    endNode   := _geom.ChildNodes.FindNode( 'End' ) ;

    attrStart := startNode.AttributeNodes.FindNode( 'pntRef' ) ;
    attrEnd   := endNode.AttributeNodes.FindNode( 'pntRef' ) ;

    if attrStart <> nil then begin
      getPoint( VarToString( attrStart.NodeValue ), startPtg ) ;
    end
    else
      startPtg := parsePoint( startNode.Text ) ;

    if attrEnd <> nil then begin
      getPoint( VarToString( attrEnd.NodeValue ), endPtg ) ;
    end
    else
      endPtg := parsePoint( endNode.Text ) ;

    oShp.AddPoint3D( startPtg ) ;
    oShp.AddPoint3D( endPtg ) ;
  end ;

  procedure T_LandXML.loadILineNode(
    const _geom : IXMLNode
  ) ;
  var
    startNode : IXMLNode ;
    endNode   : IXMLNode ;
    attrStart : IXMLNode ;
    attrEnd   : IXMLNode ;
    pntList   : IXMLNode ;
    startPtg  : TGIS_Point3D ;
    endPtg    : TGIS_Point3D ;
    dim       : Integer ;
  begin
    startNode := _geom.ChildNodes.FindNode( 'Start' ) ;
    endNode   := _geom.ChildNodes.FindNode( 'End' ) ;
    pntList   := _geom.ChildNodes.FindNode( 'PntList2D' ) ;

    dim  := 2 ;
    if pntList = nil then begin
      pntList := _geom.ChildNodes.FindNode( 'PntList3D' ) ;
      dim := 3 ;
    end ;
    attrStart := startNode.AttributeNodes.FindNode( 'pntRef' ) ;
    attrEnd   := endNode.AttributeNodes.FindNode( 'pntRef' ) ;

    if attrStart <> nil then begin
      getPoint( VarToString( attrStart.NodeValue ), startPtg ) ;
    end
    else
      startPtg := parsePoint( startNode.Text ) ;

    if attrEnd <> nil then begin
      getPoint( VarToString( attrEnd.NodeValue ), endPtg ) ;
    end
    else
      endPtg := parsePoint( endNode.Text ) ;

    oShp.AddPoint3D( startPtg ) ;
    if pntList <> nil then
      addPoints( pntList.Text, dim, 0 ) ;
    oShp.AddPoint3D( endPtg ) ;
  end ;

  procedure T_LandXML.loadChainNode(
    const _geom : IXMLNode
  ) ;
  var
    tkn : TGIS_Tokenizer ;
    i   : Integer ;
    ptg : TGIS_Point3D ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( _geom.Text, [ ' ' ] ) ;
      for i := 0 to tkn.Result.Count - 1 do begin
        if getPoint( tkn.Result[ i ], ptg ) then
          oShp.AddPoint3D( ptg ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure T_LandXML.loadArcNode(
    const _geom : IXMLNode
  ) ;
  var
    startNode   : IXMLNode ;
    centerNode  : IXMLNode ;
    endNode     : IXMLNode ;
    attrStart   : IXMLNode ;
    attrCenter  : IXMLNode ;
    attrEnd     : IXMLNode ;
    startPtg    : TGIS_Point3D ;
    centerPtg   : TGIS_Point3D ;
    endPtg      : TGIS_Point3D ;
    isCW        : Boolean ;
    radius      : Double ;
    sa, ea      : Double ;
  begin
    startNode   := _geom.ChildNodes.FindNode( 'Start' ) ;
    centerNode  := _geom.ChildNodes.FindNode( 'Center' ) ;
    endNode     := _geom.ChildNodes.FindNode( 'End' ) ;

    attrStart   := startNode.AttributeNodes.FindNode( 'pntRef' ) ;
    attrCenter  := centerNode.AttributeNodes.FindNode( 'pntRef' ) ;
    attrEnd     := endNode.AttributeNodes.FindNode( 'pntRef' ) ;

    if attrStart <> nil then begin
      getPoint( VarToString( attrStart.NodeValue ), startPtg ) ;
    end
    else
      startPtg := parsePoint( startNode.Text ) ;

    if attrCenter <> nil then begin
      getPoint( VarToString( attrCenter.NodeValue ), centerPtg ) ;
    end
    else
      centerPtg := parsePoint( centerNode.Text ) ;

    if attrEnd <> nil then begin
      getPoint( VarToString( attrEnd.NodeValue ), endPtg ) ;
    end
    else
      endPtg := parsePoint( endNode.Text ) ;

    isCW   := LowerCase( VarToString( _geom.Attributes['rot'] ) ) = 'cw' ;
    radius := Sqrt( (centerPtg.X - startPtg.X) * (centerPtg.X - startPtg.X) +
                    (centerPtg.Y - startPtg.Y) * (centerPtg.Y - startPtg.Y)
                   ) ;

    oShp.AddPoint3D( startPtg ) ;

      if not isCW then begin
        sa := -1*ArcTan2( startPtg.Y - centerPtg.Y, startPtg.X - centerPtg.X ) * 180/Pi ;
        ea := -1*ArcTan2( endPtg.Y   - centerPtg.Y, endPtg.X   - centerPtg.X ) * 180/Pi ;

        if sa < 0 then
          sa := sa + 360 ;

        oShp.StrokeArc( centerPtg, radius, radius, DegToRad(sa), DegToRad(ea), 0 );
      end
      else begin
        sa := -1*ArcTan2( startPtg.Y - centerPtg.Y, startPtg.X - centerPtg.X ) * 180/Pi ;
        ea := -1*ArcTan2( endPtg.Y   - centerPtg.Y, endPtg.X   - centerPtg.X ) * 180/Pi ;

        if ea < 0 then
          ea := ea + 360 ;

        oShp.StrokeArc( centerPtg, radius, radius, DegToRad(sa), DegToRad(ea), 0 ) ;
      end ;

    oShp.AddPoint3D( endPtg ) ;
 end ;

  procedure T_LandXML.loadSpiralNode(
    const _geom : IXMLNode
  ) ;
  var
    startNode   : IXMLNode ;
    piNode      : IXMLNode ;
    endNode     : IXMLNode ;
    attrStart   : IXMLNode ;
    attrpi      : IXMLNode ;
    attrEnd     : IXMLNode ;
    startPtg    : TGIS_Point3D ;
    piPtg       : TGIS_Point3D ;
    endPtg      : TGIS_Point3D ;
    points      : array of TGIS_Point3D ;
    a           : Double ;

    function factoral( const _value : Integer ) : Double ;
    var
      t : Integer ;
    begin
      if _value = 0 then begin
        Result := 1 ;
        exit ;
      end ;

      Result := _value ;
      t := _value-1 ;
      while (t>0) do begin
        Result := Result * t ;
        t := t-1 ;
      end ;
    end ;

    function bezierPoint( const _pos    : Double ;
                          const _points : array of TGIS_Point3D
                         ) : TGIS_Point3D ;
    var
      xx,yy,zz  : Double ;
      factn     : Double ;
      n,ll      : Integer ;
      b, w      : Double ;
    begin
      xx := 0 ;
      yy := 0 ;
      zz := 0 ;
      n  := length( _points )-1 ;
      factn := factoral(n) ;

      for ll := 0 to n do begin
        b := factn/(factoral(ll)*factoral(n-ll));
        w := Power(1-_pos, n-ll)*Power(_pos, ll);
        xx := xx + b*w*_points[ll].X ;
        yy := yy + b*w*_points[ll].Y ;
        zz := zz + b*w*_points[ll].Z ;
      end ;
      Result := GisPoint3D(xx, yy, zz) ;
    end ;

  begin
    startNode := _geom.ChildNodes.FindNode( 'Start' ) ;
    piNode    := _geom.ChildNodes.FindNode( 'PI' ) ;
    endNode   := _geom.ChildNodes.FindNode( 'End' ) ;

    attrStart := startNode.AttributeNodes.FindNode( 'pntRef' ) ;
    attrpi    := piNode.AttributeNodes.FindNode( 'pntRef' ) ;
    attrEnd   := endNode.AttributeNodes.FindNode( 'pntRef' ) ;

    if attrStart <> nil then begin
      getPoint( VarToString( attrStart.NodeValue ), startPtg ) ;
    end
    else
      startPtg := parsePoint( startNode.Text ) ;

    if attrpi <> nil then begin
      getPoint( VarToString( attrpi.NodeValue ), piPtg ) ;
    end
    else
      piPtg := parsePoint( piNode.Text ) ;

    if attrEnd <> nil then begin
      getPoint( VarToString( attrEnd.NodeValue ), endPtg ) ;
    end
    else
      endPtg := parsePoint( endNode.Text ) ;

    SetLength( points, 3 ) ;
    points[0] := startPtg ;
    points[1] := piPtg ;
    points[2] := endPtg ;

    oShp.AddPoint3D( startPtg ) ;
      a := 0.1 ;
      while a <= 1 do begin
        oShp.AddPoint3D( bezierPoint(a, points) ) ;
        a := a + 0.1 ;
      end ;
    oShp.AddPoint3D( endPtg ) ;
  end ;

  procedure T_LandXML.prepareLayer(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    oLayer := _layer ;

    oLayer.AddFieldInternal( GIS_LANDXML_NAME, TGIS_FieldType.String, 1, 0 ) ;
    oLayer.AddFieldInternal( GIS_LANDXML_TYPE, TGIS_FieldType.String, 1, 0 ) ;

    if not assigned( oLayer.SubLayers ) then
      oLayer.SubLayers := TGIS_LayerAbstractList.Create( False ) ;
  end ;

  procedure T_LandXML.startTag(
    const _qname        : String ;
    const _attributes   : SAXAttributes
  ) ;
  begin
    saxWriter.StartElement( '', '', _qname, _attributes ) ;
  end ;

  procedure T_LandXML.endTag(
    const _qname : String
  ) ;
  begin
    saxWriter.EndElement( '', '', _qname ) ;
  end ;

  procedure T_LandXML.addAttribute(
    const _attributes   : SAXAttributes ;
    const _name         : String ;
    const _value        : String
  ) ;
  begin
    _attributes.AddAttribute( '', '', _name, '', _value ) ;
  end ;

  procedure T_LandXML.setAttribute(
    const _attributes   : SAXAttributes ;
    const _idx          : Integer ;
    const _value        : String
  ) ;
  begin
    _attributes.SetValue( _idx, _value ) ;
  end ;

  function T_LandXML.nameValue(
    const _shp  : TGIS_Shape
  ) : String ;
  begin
    if hasName then
      Result := VarToString( _shp.GetField( 'name' ) )
    else if hasLandName then
      Result := VarToString( _shp.GetField( GIS_LANDXML_NAME ) )
    else
      Result := IntToStr( iPtn ) ;
  end ;

  function T_LandXML.writeFeatures(
    const _self         : TGIS_LayerVector ;
    const _layer        : TGIS_LayerVector ;
    const _extent       : TGIS_Extent      ;
    const _type         : TGIS_ShapeType   ;
    const _scope        : String           ;
    const _shape        : TGIS_Shape       ;
    const _de9im        : String           ;
    const _truncated    : Boolean
  ) : Boolean ;
  var
    {$IFNDEF OXYGENE}
      shp       : TGIS_Shape ;
    {$ENDIF}
    shp_tmp     : TGIS_Shape ;
    shape_no    : Integer    ;
    end_uid     : TGIS_Uid   ;
    abort       : Boolean    ;
    shp_found   : Boolean    ;
    i           : Integer    ;

    procedure writeStartTag ;
    var
      sax_attrs   : SAXAttributes ;
    begin
      case _type of
        TGIS_ShapeType.Point      :
          if not hasFields then
            startTag( 'CgPoints', nil ) ;
        TGIS_ShapeType.MultiPoint :
          if not hasFields then
            startTag( 'CgPoints', nil ) ;
        TGIS_ShapeType.Arc        :
          begin
            if FExportType = 'Contours' then begin
              startTag( 'Surfaces', nil ) ;
              sax_attrs := CoSAXAttributes.Create ;
              try
                addAttribute(
                  sax_attrs,
                  'name',
                  _self.Name
                ) ;
                inc( iPtn ) ;
                startTag( 'Surface', sax_attrs ) ;
              finally
                FreeObject( sax_attrs ) ;
              end;
              startTag( 'SourceData', nil ) ;
                startTag( 'Contours', nil ) ;
            end
            else if FExportType = 'PlanFeatures' then
              startTag( 'PlanFeatures', nil )
            else
              startTag( 'Alignments', nil ) ;
          end;
        TGIS_ShapeType.Polygon    :
          startTag( 'Parcels', nil ) ;
        TGIS_ShapeType.MultiPatch :
          if FExportType = 'Parcels' then
            startTag( 'Parcels', nil )
          else
            startTag( 'Surfaces', nil ) ;
      end ;
    end ;

    procedure writeCloseTag ;
    begin
      case _type of
        TGIS_ShapeType.Point      :
          if not hasFields then
            endTag( 'CgPoints' ) ;
        TGIS_ShapeType.MultiPoint :
          if not hasFields then
            endTag( 'CgPoints' ) ;
        TGIS_ShapeType.Arc        :
          if FExportType = 'Contours' then begin
                  endTag( 'Contours' ) ;
                endTag( 'SourceData' ) ;
              endTag( 'Surface' ) ;
            endTag( 'Surfaces' ) ;
          end
          else if FExportType = 'PlanFeatures' then
            endTag( 'PlanFeatures' )
          else
            endTag( 'Alignments' ) ;
        TGIS_ShapeType.Polygon    :
          endTag( 'Parcels' ) ;
        TGIS_ShapeType.MultiPatch :
          if FExportType = 'Parcels' then
            endTag( 'Parcels' )
          else
            endTag( 'Surfaces' ) ;
      end ;
    end;

  begin
    shape_no  := 0 ;
    end_uid   := _layer.GetLastUid ;
    abort     := False ;
    hasFields := False ;

    for i := 0 to _self.Fields.Count -1 do begin
      if not (TGIS_FieldFlags.Exportable in _self.FieldInfo(i).Flags) then continue ;
      if _self.FieldInfo(i).ExportName = GIS_LANDXML_NAME then continue ;
      if _self.FieldInfo(i).ExportName = GIS_LANDXML_TYPE then continue ;
      hasFields := True ;
    end ;

    shp_found := False ;
    try
      for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
          _layer.Loop( _extent, _scope, _shape, _de9im ) do
      begin
        shp_tmp := shp.PrepareExportShape(
                         _self.CS, _extent, _truncated, True
                       ) ;
        try
          if assigned( shp_tmp ) and
             ( not shp_tmp.IsDeleted ) and
             ( ( _type = shp_tmp.ShapeType   ) or
               ( _type = TGIS_ShapeType.Unknown )
             ) then
          begin
            if not shp_found then
              writeStartTag ;
            shp_found := True ;

            shp_tmp.Lock( TGIS_Lock.Projection );
            writeGeometry( _self, shp_tmp ) ;
            shp_tmp.Unlock;
          end ;
        finally
          if shp <> shp_tmp then
            FreeObject( shp_tmp ) ;
        end ;

        if shape_no mod 100 = 1 then begin
          abort := _layer.RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
          if abort then break ;
        end ;
        inc( shape_no ) ;
      end ;
    finally
      if shp_found then
        writeCloseTag ;
    end;
    Result := abort ;
  end ;

  procedure T_LandXML.writeProperties(
    const _self   : TGIS_LayerVector ;
    const _shp    : TGIS_Shape
  ) ;
  var
    sax_attrs  : SAXAttributes ;
    i          : Integer ;
    val        : String ;
    fld        : TGIS_FieldInfo ;
  begin
    if not hasFields then exit ;

    sax_attrs := CoSAXAttributes.Create ;
    try
      startTag( 'Feature', nil ) ;
        addAttribute(
          sax_attrs,
          'label',
          ''
        ) ;
        addAttribute(
          sax_attrs,
          'value',
          ''
        ) ;

        for i := 0 to _self.Fields.Count-1 do begin
          fld := _self.FieldInfo( i ) ;
          if fld.Deleted   then continue ;
          if fld.Temporary then continue ;
          if VarIsNull( _shp.GetFieldEx( fld.NewName ) ) then continue;

          if fld.FieldType = TGIS_FieldType.Date then
            val := DateTimeToXMLString(
                                  VarToDateTime( _shp.GetField( fld.NewName ) ),
                                  2, True
                                )
          else
            val := VarToString( _shp.GetField( fld.NewName ) ) ;

          setAttribute( sax_attrs, 0, fld.ExportName ) ;
          setAttribute( sax_attrs, 1, val ) ;
          startTag( 'Property', sax_attrs ) ;
          endTag( 'Property' ) ;
        end ;
      endTag( 'Feature' ) ;
    finally
      FreeObject( sax_attrs ) ;
    end ;
  end ;

  procedure T_LandXML.writeGeometry(
    const _self   : TGIS_LayerVector ;
    const _shp    : TGIS_Shape
  ) ;
  var
    sax_attrs  : SAXAttributes ;
    numparts   : Integer ;
    partsize   : Integer ;
    i, k       : Integer ;
    is3D       : Boolean ;
    mb         : TGIS_MultiPatchBuilder ;
    v          : TGIS_SingleVector ;
    f1, f2, f3 : Integer ;
    vv         : Integer ;

      function pointToString(
        const _ptg : TGIS_Point
      ) : String ;
      begin
        Result := Format( '%s %s',
                          [ DotFloatToStr( _ptg.Y ),
                            DotFloatToStr( _ptg.X ) ]
                        ) ;
      end ;

      function point3DToString(
        const _ptg : TGIS_Point3D
      ) : String ;
      begin
        Result := Format( '%s %s %s',
                          [ DotFloatToStr( _ptg.Y ),
                            DotFloatToStr( _ptg.X ),
                            DotFloatToStr( _ptg.Z ) ]
                        ) ;
      end ;

      procedure addChars(
        const _shp          : TGIS_Shape     ;
        const _partno       : Integer        ;
        const _partsize     : Integer
      ) ;
      var
        j     : Integer ;
        chars : String ;
      begin
        for j := 0 to _partsize - 1 do begin
          if is3D then
            chars := point3DToString( _shp.GetPoint3D( _partno, j ) )
          else
            chars := pointToString( _shp.GetPoint( _partno, j ) ) ;

          if j < ( _partsize -1 ) then
            chars := chars + ' ' ;
          saxWriter.Characters( chars );
        end ;
      end ;

      procedure addPoint(
        const _shp          : TGIS_Shape     ;
        const _partno       : Integer        ;
        const _pointno      : Integer
      ) ;
      var
        chars : String ;
      begin
        if is3D then
          chars := point3DToString( _shp.GetPoint3D( _partno, _pointno ) )
        else
          chars := pointToString( _shp.GetPoint( _partno, _pointno ) ) ;

        saxWriter.Characters( chars );
      end ;

      procedure addVertex(
        const _v : TGIS_SingleVector
      ) ;
      var
        chars : String ;
      begin
        if is3D then
          chars := Format( '%s %s %s',
                          [ DotFloatToStr( _v.Y ),
                            DotFloatToStr( _v.X ),
                            DotFloatToStr( _v.Z ) ]
                         )
        else
          chars := Format( '%s %s',
                          [ DotFloatToStr( _v.Y ),
                            DotFloatToStr( _v.X ) ]
                         ) ;


        saxWriter.Characters( chars ) ;
      end ;

      procedure addFace(
        const _v1 : Integer ;
        const _v2 : Integer ;
        const _v3 : Integer
      ) ;
      var
        chars : String ;
      begin
        chars := Format( '%d %d %d', [ _v1, _v2, _v3 ] ) ;
        saxWriter.Characters( chars ) ;
      end ;

  begin
    sax_attrs := CoSAXAttributes.Create ;
    try
      numparts := _shp.GetNumParts ;
      if numparts <= 0 then exit ;

      is3D := _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] ;

      case _shp.ShapeType of
        TGIS_ShapeType.Point :
          begin
            addAttribute(
              sax_attrs,
              'name',
              nameValue( _shp )
            ) ;
            inc( iPtn ) ;

            if hasFields then
              startTag( 'CgPoints', nil ) ;

            startTag( 'CgPoint', sax_attrs ) ;
              addChars( _shp, 0, _shp.GetPartSize(0) ) ;
            endTag( 'CgPoint' ) ;

            if hasFields then begin
              writeProperties( _self, _shp ) ;
              endTag( 'CgPoints' ) ;
            end ;
          end ;
        TGIS_ShapeType.MultiPoint :
          begin
            addAttribute(
              sax_attrs,
              'name',
              nameValue( _shp )
            ) ;
            for i := 0 to numparts - 1 do begin
              partsize := _shp.GetPartSize( i ) ;
              for k := 0 to partsize-1 do begin
                setAttribute(
                  sax_attrs,
                  0,
                  IntToStr(iPtn)
                ) ;
                inc( iPtn ) ;

                if hasFields then
                  startTag( 'CgPoints', nil ) ;

                startTag( 'CgPoint', sax_attrs ) ;
                  addPoint( _shp, i, k ) ;
                endTag( 'CgPoint' ) ;

                if hasFields then begin
                  writeProperties( _self, _shp ) ;
                  endTag( 'CgPoints' ) ;
                end ;
              end ;
            end ;
          end ;
        TGIS_ShapeType.Arc :
          begin
            for i := 0 to numparts - 1 do begin
              if FExportType = 'Contours' then begin
                addAttribute(
                  sax_attrs,
                  'elev',
                  DotFloatToStr( _shp.GetFirstPoint3D.Z )
                ) ;
                startTag( 'Contour', sax_attrs ) ;
                  sax_attrs.Clear ;

                  is3D := False ;
                  startTag( 'PntList2D', nil ) ;
                  partsize := _shp.GetPartSize( i ) ;
                  addChars( _shp, i, partsize ) ;

                  endTag( 'PntList2D' ) ;

                endTag( 'Contour' ) ;
              end
              else if FExportType = 'PlanFeatures' then begin
                addAttribute(
                  sax_attrs,
                  'name',
                  nameValue( _shp )
                ) ;
                inc( iPtn ) ;
                startTag( 'PlanFeature', sax_attrs ) ;
                  sax_attrs.Clear ;
                  startTag( 'CoordGeom', nil ) ;
                    partsize := _shp.GetPartSize( i ) ;
                    for k := 0 to partsize-2 do begin
                      startTag( 'Line', nil ) ;
                        startTag( 'Start', nil ) ;
                          addPoint( _shp, i, k ) ;
                        endTag( 'Start' ) ;
                        startTag( 'End', nil ) ;
                          addPoint( _shp, i, k+1 ) ;
                        endTag( 'End' ) ;
                      endTag( 'Line' ) ;
                    end;
                  endTag( 'CoordGeom' ) ;
                  writeProperties( _self, _shp ) ;
                endTag( 'PlanFeature' ) ;
              end
              else begin
                addAttribute(
                  sax_attrs,
                  'name',
                  nameValue( _shp )
                ) ;
                addAttribute(
                  sax_attrs,
                  'length',
                  DotFloatToStr( _shp.PartLength(i) )
                ) ;
                addAttribute(
                  sax_attrs,
                  'staStart',
                  DotFloatToStr(0.0)
                ) ;
                inc( iPtn ) ;
                startTag( 'Alignment', sax_attrs ) ;
                  sax_attrs.Clear ;
                  startTag( 'CoordGeom', nil ) ;
                    startTag( 'IrregularLine', nil ) ;
                      startTag( 'Start', nil ) ;
                        addPoint( _shp, 0, 0 ) ;
                      endTag( 'Start' ) ;

                      startTag( 'End', nil ) ;
                        addPoint( _shp, 0, _shp.GetPartSize( 0 )-1 ) ;
                      endTag( 'End' ) ;

                      if is3D then
                        startTag( 'PntList3D', nil )
                      else
                        startTag( 'PntList2D', nil ) ;

                        partsize := _shp.GetPartSize( i ) ;
                        addChars( _shp, i, partsize ) ;

                      if is3D then
                        endTag( 'PntList3D' )
                      else
                        endTag( 'PntList2D' ) ;

                    endTag( 'IrregularLine' ) ;
                  endTag( 'CoordGeom' ) ;
                  writeProperties( _self, _shp ) ;
                endTag( 'Alignment' ) ;
              end;
            end ;
          end ;
        TGIS_ShapeType.Polygon :
          begin
            for i := 0 to numparts - 1 do begin
              addAttribute(
                sax_attrs,
                'name',
                nameValue( _shp )
              ) ;
              inc( iPtn ) ;
              addAttribute(
                sax_attrs,
                'area',
                DotFloatToStr( _shp.Area )
              ) ;

              startTag( 'Parcel', sax_attrs ) ;
                sax_attrs.Clear ;
                startTag( 'CoordGeom', nil ) ;
                  startTag( 'IrregularLine', nil ) ;
                    startTag( 'Start', nil ) ;
                      addPoint( _shp, i, 0 ) ;
                    endTag( 'Start' ) ;

                    startTag( 'End', nil ) ;
                      addPoint( _shp, i, _shp.GetPartSize( i )-1 ) ;
                    endTag( 'End' ) ;

                    if is3D then
                      startTag( 'PntList3D', nil )
                    else
                      startTag( 'PntList2D', nil ) ;

                      partsize := _shp.GetPartSize( i ) ;
                      addChars( _shp, i, partsize ) ;

                    if is3D then
                      endTag( 'PntList3D' )
                    else
                      endTag( 'PntList2D' ) ;

                  endTag( 'IrregularLine' ) ;
                endTag( 'CoordGeom' ) ;
                writeProperties( _self, _shp ) ;
              endTag( 'Parcel' ) ;
            end ;
          end ;
        TGIS_ShapeType.MultiPatch :
          begin
            if FExportType = 'Parcels' then begin
              addAttribute(
                sax_attrs,
                'name',
                nameValue( _shp )
              ) ;
              inc( iPtn ) ;
              addAttribute(
                sax_attrs,
                'area',
                DotFloatToStr( _shp.Area )
              ) ;
              addAttribute(
                sax_attrs,
                'parcelFormat',
                'Volumertric'
              ) ;
              startTag( 'Parcel', sax_attrs ) ;
                sax_attrs.Clear ;
                addAttribute(
                  sax_attrs,
                  'name',
                  '3D'
                ) ;
                startTag( 'VolumeGeom', sax_attrs ) ;
                  for i := 0 to numparts - 1 do begin
                    setAttribute(
                      sax_attrs,
                      0,
                      'p' + IntToStr(i)
                    ) ;
                    startTag( 'CoordGeom', sax_attrs ) ;

                      partsize := _shp.GetPartSize( i ) ;
                      for k := 0 to partsize-2 do begin
                        startTag( 'Line', nil ) ;
                          startTag( 'Start', nil ) ;
                            addPoint( _shp, i, k ) ;
                          endTag( 'Start' ) ;
                          startTag( 'End', nil ) ;
                            addPoint( _shp, i, k+1 ) ;
                          endTag( 'End' ) ;
                        endTag( 'Line' ) ;
                      end ;
                      if partsize > 0 then begin
                        startTag( 'Line', nil ) ;
                          startTag( 'Start', nil ) ;
                            addPoint( _shp, i, partsize-1 ) ;
                          endTag( 'Start' ) ;
                          startTag( 'End', nil ) ;
                            addPoint( _shp, i, 0 ) ;
                          endTag( 'End' ) ;
                        endTag( 'Line' ) ;
                      end ;

                    endTag( 'CoordGeom' ) ;
                  end ;
                endTag( 'VolumeGeom' ) ;
              writeProperties( _self, _shp ) ;
              endTag( 'Parcel' ) ;
            end
            else begin
              addAttribute(
                sax_attrs,
                'name',
                nameValue( _shp )
              ) ;
              inc( iPtn ) ;

              startTag( 'Surface', sax_attrs ) ;
                sax_attrs.Clear ;
                addAttribute(
                  sax_attrs,
                  'surfType',
                  'TIN'
                ) ;
                startTag( 'Definition', sax_attrs ) ;
                sax_attrs.Clear ;

                  mb := TGIS_MultiPatchBuilder.Create ;
                  try
                    mb.BuildMesh( _shp ) ;
                    vv := 0 ;
                    startTag( 'Pnts', nil ) ;
                      sax_attrs.Clear ;
                      addAttribute(
                        sax_attrs,
                        'id',
                        IntToStr(vv)
                      ) ;
                      for vv := 0 to mb.VertexesCount-1 do begin
                        mb.GetVertex( vv, v ) ;

                        setAttribute(
                          sax_attrs,
                          0,
                          IntToStr(vv+1)
                        ) ;

                        startTag( 'P', sax_attrs ) ;
                          addVertex( v ) ;
                        endTag( 'P' ) ;
                      end ;
                    endTag( 'Pnts' ) ;

                    startTag( 'Faces', nil ) ;
                      for vv := 0 to mb.FacesCount-1 do begin
                        mb.GetFace( vv, f1, f2, f3 ) ;

                        startTag( 'F', nil ) ;
                          addFace( f1, f2, f3 ) ;
                        endTag( 'F' ) ;
                      end ;
                    endTag( 'Faces' ) ;
                  finally
                    FreeObject( mb ) ;
                  end ;

                endTag( 'Definition' ) ;
                writeProperties( _self, _shp ) ;
              endTag( 'Surface' ) ;
            end ;
          end
      end ;

    finally
      FreeObject( sax_attrs ) ;
    end ;
  end ;

  procedure T_LandXML.writeHeader(
    const _self  : TGIS_LayerVector ;
    const _layer : TGIS_LayerVector
  ) ;
  var
    sax_attrs   : SAXAttributes   ;
    epsg        : Integer ;
    blinear     : Boolean ;
    bimperial   : Boolean ;
  begin
      sax_attrs := CoSAXAttributes.Create ;
      try
        addAttribute(
          sax_attrs,
          'xmlns',
          'http://www.landxml.org/schema/LandXML-1.2'
        ) ;
        addAttribute(
          sax_attrs,
          'xmlns:xsi',
          'http://www.w3.org/2001/XMLSchema-instance'
        ) ;
        addAttribute(
          sax_attrs,
          'xsi:schemaLocation',
          'http://www.landxml.org/schema/LandXML-1.2 ' +
          'http://www.landxml.org/schema/LandXML-1.2/LandXML-1.2.xsd'
        ) ;
        addAttribute(
          sax_attrs,
          'date',
          FormatDateTime( 'yyyy-mm-dd', Now )
        ) ;
        addAttribute(
          sax_attrs,
          'time',
          FormatDateTime( 'hh:nn:ss', Now )
        ) ;
        addAttribute(
          sax_attrs,
          'version',
          '1.2'
        ) ;

        startTag( 'LandXML', sax_attrs ) ;
      finally
        FreeObject( sax_attrs ) ;
      end ;

      blinear   := False ;
      bimperial := False ;
      if _self.CS is TGIS_CSProjectedCoordinateSystem then begin
        epsg := TGIS_CSProjectedCoordinateSystem(_self.CS).Units.EPSG ;
        if (epsg=904001) or (epsg=904002) or (epsg=9001) or (epsg=1025) or
           (epsg=1033) or (epsg=904101) or (epsg=9036) then
          blinear := True
        else if (epsg=904003) or (epsg=9002) or (epsg=9003) or (epsg=9093) or
          (epsg=9096) or (epsg=904103) or (epsg=9035) then
          bimperial := True
      end
      else begin
        blinear   := False ;
        bimperial := False ;
      end;

      startTag( 'Units', nil ) ;
      sax_attrs := CoSAXAttributes.Create ;
      try
        if blinear then begin
          addAttribute(
            sax_attrs,
            'areaUnit',
            'squareMeter'
          ) ;
          addAttribute(
            sax_attrs,
            'linearUnit',
            'meter'
          ) ;
          addAttribute(
            sax_attrs,
            'elevationUnit',
            'meter'
          ) ;
          addAttribute(
            sax_attrs,
            'volumeUnit',
            'cubicMeter'
          ) ;
          addAttribute(
            sax_attrs,
            'temperatureUnit',
            'celsius'
          ) ;
          addAttribute(
            sax_attrs,
            'pressureUnit',
            'HPA'
          ) ;
          addAttribute(
            sax_attrs,
            'diameterUnit',
            'meter'
          ) ;
          addAttribute(
            sax_attrs,
            'heightUnit',
            'meter'
          ) ;
          addAttribute(
            sax_attrs,
            'angularUnit',
            'radians'
          ) ;
          addAttribute(
            sax_attrs,
            'directionUnit',
            'radians'
          ) ;

          startTag( 'Metric', sax_attrs ) ;
          endTag( 'Metric' ) ;
        end
        else if bimperial then begin
          addAttribute(
            sax_attrs,
            'areaUnit',
            'squareFoot'
          ) ;
          addAttribute(
            sax_attrs,
            'linearUnit',
            'foot'
          ) ;
          addAttribute(
            sax_attrs,
            'elevationUnit',
            'meter'
          ) ;
          addAttribute(
            sax_attrs,
            'volumeUnit',
            'cubicYard'
          ) ;
          addAttribute(
            sax_attrs,
            'temperatureUnit',
            'fahrenheit'
          ) ;
          addAttribute(
            sax_attrs,
            'pressureUnit',
            'inchHG'
          ) ;
          addAttribute(
            sax_attrs,
            'diameterUnit',
            'inch'
          ) ;
          addAttribute(
            sax_attrs,
            'heightUnit',
            'foot'
          ) ;
          addAttribute(
            sax_attrs,
            'angularUnit',
            'decimal degrees'
          ) ;
          addAttribute(
            sax_attrs,
            'directionUnit',
            'decimal degrees'
          ) ;

          startTag( 'Imperial', sax_attrs ) ;
          endTag( 'Imperial' ) ;
        end
        else begin
          addAttribute(
            sax_attrs,
            'areaUnit',
            'squareMeter'
          ) ;
          addAttribute(
            sax_attrs,
            'linearUnit',
            'meter'
          ) ;
          addAttribute(
            sax_attrs,
            'elevationUnit',
            'meter'
          ) ;
          addAttribute(
            sax_attrs,
            'volumeUnit',
            'cubicMeter'
          ) ;
          addAttribute(
            sax_attrs,
            'temperatureUnit',
            'celsius'
          ) ;
          addAttribute(
            sax_attrs,
            'pressureUnit',
            'milliBars'
          ) ;
          addAttribute(
            sax_attrs,
            'diameterUnit',
            'meter'
          ) ;
          addAttribute(
            sax_attrs,
            'heightUnit',
            'meter'
          ) ;
          addAttribute(
            sax_attrs,
            'angularUnit',
            'decimal degrees'
          ) ;
          addAttribute(
            sax_attrs,
            'directionUnit',
            'decimal degrees'
          ) ;

          startTag( 'Metric', sax_attrs ) ;
          endTag( 'Metric' ) ;
        end;
      finally
        FreeObject( sax_attrs ) ;
      end ;
      endTag( 'Units' ) ;

      sax_attrs := CoSAXAttributes.Create ;
      try
        addAttribute(
          sax_attrs,
          'name',
          _layer.Path
        ) ;
        addAttribute(
          sax_attrs,
          'desc',
          _layer.FileInfo
        ) ;

        startTag( 'Project', sax_attrs ) ;
        endTag( 'Project' ) ;
      finally
        FreeObject( sax_attrs ) ;
      end ;

      sax_attrs := CoSAXAttributes.Create ;
      try
        addAttribute(
          sax_attrs,
          'name',
          'LicadGIS Developer Kernel'
        ) ; // ilker deðiþtirme
        addAttribute(
          sax_attrs,
          'desc',
          'LandXML Export'
        ) ;
        addAttribute(
          sax_attrs,
          'manufacturer',
          'LicadGIS'
        ) ; // ilker deðiþtirme
        addAttribute(
          sax_attrs,
          'version',
          '11'
        ) ;
        addAttribute(
          sax_attrs,
          'manufacturerURL',
          'www.lideryazilim.com'
        ) ; // ilker deðiþtirme

        startTag( 'Application', sax_attrs ) ;
        endTag( 'Application' ) ;
      finally
        FreeObject( sax_attrs ) ;
      end ;

      sax_attrs := CoSAXAttributes.Create ;
      try
        addAttribute(
          sax_attrs,
          'name',
          StringReplaceAll( _self.CS.WKT, '_', ' ' )
        ) ;
        addAttribute(
          sax_attrs,
          'epsgCode',
          IntToStr( _self.CS.EPSG )
        ) ;
        addAttribute(
          sax_attrs,
          'ogcWktCode',
          _self.CS.FullWKT
        ) ;

        startTag( 'CoordinateSystem', sax_attrs ) ;
        endTag( 'CoordinateSystem' ) ;
      finally
        FreeObject( sax_attrs ) ;
      end ;
  end ;

  procedure T_LandXML.LoadLandXMLFile(
    const _path  : String ;
    const _layer : TGIS_LayerVector
  ) ;
  var
    abort : Boolean ;
  begin
    xDoc.LoadFromFile( _path ) ;
    prepareLayer( _layer ) ;

    loadHeader( xDoc.DocumentElement ) ;

    abort := _layer.RaiseBusyShake( _layer, 20, 100 ) ;
    loadAllSurfaces( xDoc.DocumentElement ) ;

    abort := _layer.RaiseBusyShake( _layer, 30, 100 ) ;
    loadAllCgPoints( xDoc.DocumentElement ) ;

    abort := _layer.RaiseBusyShake( _layer, 40, 100 ) ;
    loadAllMonuments( xDoc.DocumentElement ) ;

    abort := _layer.RaiseBusyShake( _layer, 50, 100 ) ;
    loadAllParcels( xDoc.DocumentElement ) ;

    abort := _layer.RaiseBusyShake( _layer, 60, 100 ) ;
    loadAllAlignments( xDoc.DocumentElement ) ;

    abort := _layer.RaiseBusyShake( _layer, 70, 100 ) ;
    loadAllPlanFeatures( xDoc.DocumentElement ) ;

    abort := _layer.RaiseBusyShake( _layer, 80, 100 ) ;
    loadAllPipeNetworks( xDoc.DocumentElement ) ;

    oLayer.RecalcExtent ;
  end ;

  procedure T_LandXML.SaveLandXMLFile(
    const _path       : String ;
    const _self       : TGIS_LayerVector ;
    const _layer      : TGIS_LayerVector ;
    const _extent     : TGIS_Extent      ;
    const _type       : TGIS_ShapeType   ;
    const _scope      : String           ;
    const _shape      : TGIS_Shape       ;
    const _de9im      : String           ;
    const _truncated  : Boolean
  ) ;
  var
    same_name   : Boolean         ;
    abrt        : Boolean         ;
    ex          : TGIS_Extent     ;
    old_scope   : String          ;
    {$IFDEF JAVA}
      stream    : TXMLOutputStream ;
    {$ELSE}
      stream    : TGIS_FileStream ;
    {$ENDIF}
  begin
    abrt := False ;
    _layer.RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [_layer.Name] ) ) ;
    try
      _self.ImportStructure( _layer ) ;
      _self.PrepareExportFieldNames( 32 ) ;
      _self.ExportStructureToFLD ;

      same_name := CompareText( GetPathAbsolute( '', _self.Path  ),
                                GetPathAbsolute( '', _layer.Path )
                              ) = 0  ;
      // prepare output stream
      try
        {$IFDEF JAVA}
          stream := java.io.FileOutputStream.Create( GetTemporaryName( _self.Path ) ) ;
        {$ELSE}
          stream := TGIS_FileStream.Create( GetTemporaryName( _self.Path ), fmCreate ) ;
        {$ENDIF}
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                     GetTemporaryName( _self.Path ), GetLastError
                                   ) ;
      end ;
      // prepare and save GML file
      saxWriter := TGIS_SAXWriter.Create( stream ) ;
      saxWriter.SaveCompact := False ;
      saxWriter.StartDocument ;
      saxWriter._XMLDecl( '1.0', 'UTF-8', '' ) ;

      writeHeader( _self, _layer ) ;

      ex := GisCommonExtent( _layer.Extent, _extent ) ;
      try
        old_scope := _layer.Scope ;
        if same_name then
          _layer.Scope := '' ;

        hasName     := _self.FindField( 'name' ) > -1 ;
        hasLandName := _self.FindField( GIS_LANDXML_NAME ) > -1 ;
        iPtn := 1 ;
        if _type = TGIS_ShapeType.Unknown then begin
          abrt := writeFeatures(
            _self, _layer, ex, TGIS_ShapeType.Point, _scope, _shape, _de9im, _truncated
          ) ;
          iPtn := 1 ;
          abrt := writeFeatures(
            _self, _layer, ex, TGIS_ShapeType.MultiPoint, _scope, _shape, _de9im, _truncated
          ) ;
          iPtn := 1 ;
          abrt := writeFeatures(
            _self, _layer, ex, TGIS_ShapeType.Arc, _scope, _shape, _de9im, _truncated
          ) ;
          iPtn := 1 ;
          abrt := writeFeatures(
            _self, _layer, ex, TGIS_ShapeType.Polygon, _scope, _shape, _de9im, _truncated
          ) ;
          iPtn := 1 ;
          abrt := writeFeatures(
            _self, _layer, ex, TGIS_ShapeType.MultiPatch, _scope, _shape, _de9im, _truncated
          ) ;
        end
        else
          abrt := writeFeatures(
                    _self, _layer, ex, _type, _scope, _shape, _de9im, _truncated
                  ) ;
      finally
        _layer.Scope := old_scope ;

        endTag( 'LandXML' ) ;

        saxWriter.EndDocument ;
        FreeObject( saxWriter ) ;
        FreeObject( stream ) ;

        if abrt then begin
          DeleteFile( GetTemporaryName( _self.Path ) ) ;
        end
        else begin
          DeleteFile( GetBackupName( _self.Path ) ) ;
          RenameFile( _self.Path, GetBackupName( _self.Path ) ) ;
          try
            if not RenameFile( GetTemporaryName( _self.Path ), _self.Path ) then
              raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                           _self.Path, GetLastError
                                         ) ;
          except
            RenameFile( GetBackupName( _self.Path ), _self.Path ) ;
            raise ;
          end ;
        end ;
      end ;
    finally
      _layer.RaiseBusyRelease( _layer ) ;
    end ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_LayerLandXML'}
//==============================================================================
// TGIS_LayerLandXML
//==============================================================================

  constructor TGIS_LayerLandXML.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                           ] ;
    FExportType := '' ;
  end ;

  procedure TGIS_LayerLandXML.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_LayerLandXML.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_LayerLandXML.setUp ;
  var
    oLandXML : T_LandXML ;
  begin
    inherited;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
    Lock ;
    try
      oLandXML := T_LandXML.Create ;
      try
        oLandXML.LoadLandXMLFile( Path, self ) ;
      finally
        FreeObject( oLandXML ) ;
      end ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := 'LandXML' ;
    finally
      Unlock ;
      RaiseBusyRelease( Self ) ;

      FIsModified := False ;
    end ;
  end ;

  function TGIS_LayerLandXML.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    lx : TGIS_LayerSublayerVector ;
    i  : Integer ;
  begin
    Result := True ;
    if Items.Count = 0 then
      exit ;

    for i := 0 to SubLayers.Count - 1 do begin
      lx := TGIS_LayerSublayerVector( SubLayers[ i ] ) ;

      if not lx.Active then continue ;

      lx.Renderer := Renderer ;
    end ;

    Result := inherited DrawEx( _extent ) ;
  end ;

  procedure TGIS_LayerLandXML.SaveData  ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;

  procedure TGIS_LayerLandXML.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    inherited ;

    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;
  end ;

  procedure TGIS_LayerLandXML.ImportLayerEx(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent      ;
    const _type        : TGIS_ShapeType   ;
    const _scope       : String           ;
    const _shape       : TGIS_Shape       ;
    const _de9im       : String           ;
    const _truncated   : Boolean
  ) ;
  var
    oLandXML : T_LandXML ;
  begin
    if not assigned( _layer ) then exit ;

    if not CheckFileWriteAccessEx( Path, True, True, True ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    try
      oLandXML := T_LandXML.Create ;
      try
        oLandXML.ExportType := FExportType ;
        oLandXML.SaveLandXMLFile(
          Path,
          Self,
          _layer,
          _extent,
          _type,
          _scope,
          _shape,
          _de9im,
          _truncated
        ) ;
      finally
        FreeObject( oLandXML ) ;
      end ;
    finally
      if not IsOpened then begin
        Items.Clear ;
        Fields.Clear ;
        Open ;
      end;
    end ;
  end ;

  function TGIS_LayerLandXML.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  var
    oStm : TGIS_BufferedFileStream ;
    oRdr : TGIS_TextStreamReader ;
    buf  : String ;
    i    : Integer ;
    len  : Integer ;
  begin
    if SafeFileExists( _path ) then begin
      Result := False ;
      oStm := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
      try
        oRdr := TGIS_TextStreamReader.Create( oStm ) ;
        try
          buf := oRdr.ReadBuffer ;
          i := StringFirst ;
          len := length( buf ) ;
          if len < 200 then exit ;

          while i < len do begin
            if ( ( buf[ i ] )   = 'L' ) and
               ( ( buf[ i+1 ] ) = 'a' ) and
               ( ( buf[ i+2 ] ) = 'n' ) and
               ( ( buf[ i+3 ] ) = 'd' ) and
               ( ( buf[ i+4 ] ) = 'X' ) and
               ( ( buf[ i+5 ] ) = 'M' ) and
               ( ( buf[ i+6 ] ) = 'L' ) then begin
                 Result := True ;
                 break ;
               end ;
            inc( i ) ;
          end ;
        finally
          FreeObject( oRdr ) ;
        end ;
      finally
        FreeObject( oStm ) ;
      end ;
    end
    else
      Result := True ;

    Result := inherited PreRecognize( _path, _new_path ) and Result ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerLandXML.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-LandXML', 'LandXML',
                   TGIS_LayerLandXML, '.xml',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                    [ TGIS_RegisteredOperationType.Read,
                      TGIS_RegisteredOperationType.Write,
                      TGIS_RegisteredOperationType.&Create
                    ],
                   True
                 ) ;
  end ;
{$ENDREGION}

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerLandXML.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

