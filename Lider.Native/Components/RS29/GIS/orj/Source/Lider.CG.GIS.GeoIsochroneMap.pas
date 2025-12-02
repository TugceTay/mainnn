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
  Isochrone map implementation.
}

{$IFDEF DCC}
  unit GisIsochroneMap ;
  {$HPPEMIT '#pragma link "GisIsochroneMap"'}
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
    TatukGIS.RTL,
    TatukGIS.NDK ;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisTypes,
    GisInterfaces,
    GisLayerVector,
    GisLayerPixel,
    GisNetwork,
    GisRtl,
    GisShortestPath,
    GisInterpolation ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

type
  /// <summary>
  ///   Network analysis tool for creating isochrone maps - for a given point
  ///   finds the network area reachable within a given maximum network
  ///   traversing cost.
  /// </summary>
  TGIS_IsochroneMap = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_BaseObjectDisposable )
    private
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      oViewer : IGIS_Viewer ;
    private
      FOutCostField : String ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ; overload;
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer object (for updating progress)
      /// </param>
      constructor Create ( const _viewer : IGIS_Viewer
                         ) ; overload;

    public
      /// <summary>
      ///   Finds the network area reachable within a given cost and outputs
      ///   the result to the destination layer as shape(s).
      /// </summary>
      /// <param name="_src">
      ///   source layer which contains the network
      /// </param>
      /// <param name="_spath">
      ///   shortest path object for the source layer
      /// </param>
      /// <param name="_dst">
      ///   destination layer
      /// </param>
      /// <param name="_type">
      ///   output shape type
      /// </param>
      /// <param name="_start">
      ///   starting point; if not a network node then the nearest network node
      ///   will be used as the starting point
      /// </param>
      /// <param name="_cost">
      ///   maximum cost of traversing the network
      /// </param>
      /// <param name="_intrvl">
      ///   cost interval; only the first node which exceeds each interval will
      ///   be added to the output
      /// </param>
      /// <returns>
      ///   True if the operation went successful
      /// </returns>
      /// <remarks>
      ///   Choosing each output shape type will yield different result:
      ///   <list type="bullet">
      ///     <item>
      ///       point - reachable network nodes as separate shapes,
      ///     </item>
      ///     <item>
      ///       multipoint - reachable network nodes as one shape,
      ///     </item>
      ///     <item>
      ///       arc - reachable network links as separate shapes,
      ///     </item>
      ///     <item>
      ///       polygon - concave hull of the reachable network.
      ///     </item>
      ///   </list>
      /// </remarks>
      function  Generate ( const _src    : TGIS_LayerVector ;
                           const _spath  : TGIS_ShortestPath ;
                           const _dst    : TGIS_LayerVector ;
                           const _type   : TGIS_ShapeType ;
                           const _start  : TGIS_Point ;
                           const _cost   : Double ;
                           const _intrvl : Double
                         ) : Boolean ;

    public
      /// <summary>
      ///   Specifies the name of the attribute field which will be added to
      ///   the destination layer.
      /// </summary>
      /// <remarks>
      ///   If the attribute field name is set to an empty string then the
      ///   default name will be used (GIS_COST). Depending on the output
      ///   shape type the attribute field will be populated with:
      ///   <list type="bullet">
      ///     <item>
      ///       point - the cost of reaching the node,
      ///     </item>
      ///     <item>
      ///       arc - the cost of reaching the end of the link,
      ///     </item>
      ///     <item>
      ///       other - maximum cost defined by the _cost parameter.
      ///     </item>
      ///   </list>
      /// </remarks>
      property OutputCostFieldName : String
                           read  FOutCostField
                           write FOutCostField ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisFunctions,
    GisTopology,
    GisCsSystems,
    GisResource ;
{$ENDIF}

const
  LOCAL_NETWORK_COST_FIELD_NAME : String = 'GIS_COST' ;


//==============================================================================
// TGIS_IsochroneMap
//==============================================================================

  constructor TGIS_IsochroneMap.Create ;
  begin
    inherited ;

    oViewer := nil ;
    FOutCostField := LOCAL_NETWORK_COST_FIELD_NAME ;
  end ;


  constructor TGIS_IsochroneMap.Create(
    const _viewer : IGIS_Viewer
  ) ;
  begin
    inherited Create ;

    oViewer := _viewer ;
    FOutCostField := LOCAL_NETWORK_COST_FIELD_NAME ;
  end ;


  procedure TGIS_IsochroneMap.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_IsochroneMap.Generate(
    const _src    : TGIS_LayerVector ;
    const _spath  : TGIS_ShortestPath ;
    const _dst    : TGIS_LayerVector ;
    const _type   : TGIS_ShapeType ;
    const _start  : TGIS_Point ;
    const _cost   : Double ;
    const _intrvl : Double
  ) : Boolean ;
  var
    shrtp : TGIS_ShortestPath ;
    mxdst : Double ;
    lt    : TGIS_LayerVector ;
    ntwrk : TGIS_Network ;
    node  : TGIS_NetworkNode ;
    prec  : Double ;
    luid  : TDictionary<Integer,Integer> ;
    {$IFDEF DCC}
    pair  : TPair<Integer,Integer> ;
    {$ENDIF}
    lnod  : TList<TGIS_NetworkNode> ;
    barc  : Boolean ;
    bivl  : Boolean ;
    shp   : TGIS_Shape ;
    tmp   : TGIS_Shape ;
    tpl   : TGIS_Topology ;
    abort : Boolean ;
    bstop : Boolean ;
    i     : Integer ;
    ibusy : Integer ;

    procedure go_node(
      const _n : TGIS_NetworkNode ;
      const _l : TGIS_NetworkLink ;
      const _c : Double ;
      const _i : Integer
    ) ;
    var
      ii : Integer ;
      cc : Double ;
      aa : Boolean ;
      dd : Double ;
      iv : Integer ;
      cost_direct  : Double ;
      cost_reverse : Double ;
      {$IFDEF OXYGENE}
        evnt     : TGIS_LinkDynamicEventArgs ;
      {$ENDIF}
      lnk : TGIS_NetworkLink ;
      nd : TGIS_NetworkNode ;
    begin
      if assigned( oViewer ) and ( ( ibusy mod 10000 ) = 0 ) then begin
        oViewer.BusyShake( Self, 1, 2, abort ) ;
        if abort then begin
          bstop := True ;
          exit ;
        end ;
      end ;

      if ( not assigned( _l ) ) and ( not barc ) then
        lnod.Add( _n ) ;

      iv := _i ;
      if barc then begin
        if assigned( _l ) and ( not luid.ContainsKey( _l.ShapeUid ) ) then begin
          luid.Add( _l.ShapeUid, lnod.Count ) ;
          lnod.Add( _n ) ;
        end ;
      end
      else
      if ( _n.CurrentCost > 1e+308 ) then begin
        if not bivl then
          lnod.Add( _n )
        else
        if _c > ( _i + 1 )*_intrvl then begin
          lnod.Add( _n ) ;
          iv := FloorS( _c/_intrvl ) ;
        end ;
      end
      else
      if bivl then begin
        if _c > ( _i + 1 )*_intrvl then
          iv := FloorS( _c/_intrvl )
        else
          lnod.Remove( _n ) ;
      end ;

      if _c >= _n.CurrentCost then
        exit ;

      _n.CurrentCost := _c ;

      aa := True ;
      for ii := 0 to _n.LinkCount - 1 do begin

        lnk := _n.Link[ii] ;
        if assigned( _l ) and ( lnk.ObjectId = _l.ObjectId ) then
          continue ;

        cost_direct  := lnk.Cost ;
        cost_reverse := lnk.ReverseCost ;

        {$IFDEF OXYGENE}
          if assigned( shrtp.LinkDynamicEvent ) then begin
            evnt := TGIS_LinkDynamicEventArgs.Create(
                      lnk.ShapeUid,
                      cost_direct,
                      cost_reverse
                    ) ;
            try
              shrtp.LinkDynamicEvent( self, evnt ) ;
            finally
              cost_direct  := evnt.Cost    ;
              cost_reverse := evnt.RevCost ;
              FreeObject( evnt ) ;
            end;
          end ;
        {$ELSE}
          if Assigned( shrtp.LinkDynamicEvent ) then
            shrtp.LinkDynamicEvent(
              self,
              lnk.ShapeUid,
              cost_direct,
              cost_reverse
            ) ;
        {$ENDIF}

        if _n.ObjectId = lnk.NodeA then begin
          if cost_direct < 0 then
            continue ;

          cc := _c + cost_direct * shrtp.CostModifiers[ lnk.LinkType ] ;
          nd := ntwrk.GetNode( lnk.NodeB ) ;
        end
        else begin
          if cost_reverse < 0 then
            continue ;

          cc := _c + cost_reverse * shrtp.CostModifiers[ lnk.LinkType ] ;
          nd := ntwrk.GetNode( lnk.NodeA ) ;
        end ;

        if cc < _cost then begin
          aa := False ;

          dd := GisPoint2Point( _n.Pos, nd.Pos ) ;
          mxdst := Max( mxdst, dd ) ;

          go_node( nd, _n.Link[ii], cc, iv ) ;
          if bstop then
            exit ;
        end ;
      end ;

      if bivl then begin
        if aa and ( iv = _i ) then
          lnod.Add( _n ) ;
      end ;

    end ;

    function extent_diagonal( const _e : TGIS_Extent ) : Double ;
    begin
      Result := Sqrt( ( _e.XMax - _e.XMin )*( _e.XMax - _e.XMin ) +
                      ( _e.YMax - _e.YMin )*( _e.YMax - _e.YMin ) ) ;
    end ;

  begin
    Result := False ;

    if not assigned( _spath ) then
      exit ;

    if not assigned( _src ) then
      exit ;

    if not _src.IsOpened then
      exit ;

    if not assigned( _dst ) then
      exit ;

    if not _dst.IsOpened then
      exit ;

    if ( _type <> TGIS_ShapeType.Point      ) and
       ( _type <> TGIS_ShapeType.MultiPoint ) and
       ( _type <> TGIS_ShapeType.Arc        ) and
       ( _type <> TGIS_ShapeType.Polygon    ) then
      exit ;

    barc := ( _type = TGIS_ShapeType.Arc     ) or
            ( _type = TGIS_ShapeType.Polygon ) ;
    bivl := _intrvl > 0 ;

    if _type = TGIS_ShapeType.Arc then
      _dst.ImportStructure( _src ) ;
    if _dst.FindField( LOCAL_NETWORK_COST_FIELD_NAME ) < 0 then
      _dst.AddField( LOCAL_NETWORK_COST_FIELD_NAME,
                     TGIS_FieldType.Float, 0, 0 ) ;

    prec := extent_diagonal( _src.Extent )/100.0 ;

    shrtp := _spath ;
    try
      shrtp.LoadTheData( _src ) ;
      ntwrk := shrtp.Network ;

      while True do begin
        node := ntwrk.FindNode( _start, prec ) ;
        if assigned( node ) then
          break ;
        prec := 2*prec ;
      end ;

      if not assigned( node ) then
        exit ;

      if assigned( oViewer ) then
        oViewer.BusyPrepare( Self, _rsrc( GIS_RS_BUSY_DEFAULT ) ) ;
      bstop := False ;

      luid := TDictionary<Integer,Integer>.Create ;
      lnod := TList<TGIS_NetworkNode>.Create ;
      try
        ibusy := 0 ;
        mxdst := 0.0 ;
        go_node( node, nil, 0, 0 ) ;

        if _type = TGIS_ShapeType.Arc then begin
          for pair in luid do begin
            tmp := _src.GetShape( pair.Key ) ;
            shp := _dst.CreateShape(
              TGIS_ShapeType.Arc, _dst.DefaultDimension ) ;
            shp.CopyGeometry( tmp ) ;
            shp.CopyFields( tmp ) ;
            shp.SetField( LOCAL_NETWORK_COST_FIELD_NAME,
                          lnod[pair.Value].CurrentCost ) ;
          end ;
        end
        else
        if _type = TGIS_ShapeType.Point then begin
          for i := 0 to lnod.Count - 1 do begin
            shp := _dst.CreateShape(
              TGIS_ShapeType.Point, _dst.DefaultDimension ) ;
            shp.AddPart ;
            shp.AddPoint( lnod[i].Pos ) ;
            shp.SetField( LOCAL_NETWORK_COST_FIELD_NAME,
                          lnod[i].CurrentCost ) ;
          end ;
        end
        else
        if _type = TGIS_ShapeType.MultiPoint then begin
          shp := _dst.CreateShape(
            TGIS_ShapeType.MultiPoint, _dst.DefaultDimension ) ;
          shp.AddPart ;
          for i := 0 to lnod.Count - 1 do
            shp.AddPoint( lnod[i].Pos ) ;
          shp.SetField( LOCAL_NETWORK_COST_FIELD_NAME, _cost ) ;
        end
        else
        if _type = TGIS_ShapeType.Polygon then begin

          lt := TGIS_LayerVector.Create ;
          try

            lt.Name := 'TEMP' ;
            lt.CS := _src.CS ;

            for pair in luid do begin
              tmp := _src.GetShape( pair.Key ) ;
              lt.AddShape( tmp ) ;
            end ;

            tpl := TGIS_Topology.Create ;
            try
              shp := tpl.ConcaveHull( lt, mxdst ) ;
            finally
              FreeObject( tpl ) ;
            end ;

          finally
            FreeObject( lt ) ;
          end ;

          if assigned( shp ) then begin
            tmp := _dst.AddShape( shp ) ;
            FreeObject( shp ) ;
            tmp.SetField( LOCAL_NETWORK_COST_FIELD_NAME, _cost ) ;
          end ;
        end ;

      finally
        FreeObject( luid ) ;
        FreeObject( lnod ) ;

        if assigned( oViewer ) then
          oViewer.BusyRelease( Self ) ;
      end ;

    finally
      // do nothing
    end ;

    Result := True ;
  end ;


//==================================== END =====================================
end.
