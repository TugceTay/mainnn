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
  Encapsulation of a Voronoi/Delaunay Layer and triangulation methods.

  This unit was partially based on SWEEP2:

  The author of this software is Steven Fortune.  Copyright (c) 1994 by AT&T
  Bell Laboratories.
  Permission to use, copy, modify, and distribute this software for any
  purpose without fee is hereby granted, provided that this entire notice
  is included in all copies of any software which is or includes a copy
  or modification of this software and in all copies of the supporting
  documentation for such software.
  THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
  WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
  REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
  OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.

  Sublicensing of this unit is a subject of TatukGIS Developer
  Kernel License
}

{$IFDEF DCC}
  unit GisTriangulation ;
  {$HPPEMIT '#pragma link "GisTriangulation"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Variants,
    GisRtl,
    GisTypes,
    GisLayerVector ;
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
  TGIS_LayerVoronoi   = class  ;  // encapsulation of Voronoi Diagrams
  TGIS_LayerDelaunay  = class  ;  // encapsulation of Delaunay Triangulation

  /// <summary>
  ///   Encapsulation of Voronoi layer. It can keep polygons as Voronoi cells
  ///   or Delaunay triangles (see Triangulate property)
  /// </summary>
  TGIS_LayerVoronoi = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      trObj         : TObject ;
      edSentinel    : Integer ;

    private

      /// <summary>
      ///   Generates polygons (Voronoi cells) form list of edges.
      /// </summary>
      procedure completteVoronoiCells ;

    protected
      procedure doDestroy ; override;

    public

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor Create  ; override;

    public

      /// <inheritdoc/>
      procedure ImportLayerEx( const _layer       : TGIS_LayerVector ;
                               const _extent      : TGIS_Extent      ;
                               const _type        : TGIS_ShapeType   ;
                               const _scope       : String           ;
                               const _shape       : TGIS_Shape       ;
                               const _de9im       : String           ;
                               const _truncated   : Boolean
                             ) ; override;
  end ;


  /// <summary>
  ///   Encapsulation of Delaunay layer. It can keep polygons as Voronoi
  ///   cells or Delaunay triangles (see Triangulate property)
  /// </summary>
  TGIS_LayerDelaunay = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      trObj  : TObject ;

    protected
      procedure doDestroy ; override;

    public

      /// <summary>
      ///   Create an object.
      /// </summary>
      constructor Create  ; override;

    public

      /// <inheritdoc/>
      procedure ImportLayerEx ( const _layer       : TGIS_LayerVector ;
                                const _extent      : TGIS_Extent;
                                const _type        : TGIS_ShapeType ;
                                const _scope       : String ;
                                const _shape       : TGIS_Shape ;
                                const _de9im       : String ;
                                const _truncated   : Boolean
                              ) ; override;

      /// <summary>
      ///   Prepares Delaunay triangles from an existing layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be imported
      /// </param>
      /// <param name="_extent">
      ///   start extent of layer - can't be zero sized
      /// </param>
      /// <param name="_type">
      ///   shape type supported by the layer; if Unknown is used then
      ///   Export will try to recognize the shape type on its own;
      /// </param>
      /// <param name="_scope">
      ///   SQL query for which the shape will be exported
      /// </param>
      /// <param name="_truncated">
      ///   if True, then all shapes will be truncated in _extent; if False,
      ///   only shape visible in _extent will be exported ;
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEWRITE
      /// </exception>
      /// <remarks>
      ///   See TGIS_LayerVector.ImportLayer for details and example.
      /// </remarks>
      procedure PrepareAltitudeData
                              ( const _layer       : TGIS_LayerVector ;
                                const _extent      : TGIS_Extent;
                                const _type        : TGIS_ShapeType ;
                                const _scope       : String ;
                                const _truncated   : Boolean
                              ) ; overload;

      /// <summary>
      ///   Prepares Delaunay triangles from an existing layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be imported
      /// </param>
      /// <param name="_extent">
      ///   start extent of layer - can't be zero sized
      /// </param>
      /// <param name="_type">
      ///   shape type supported by the layer; if Unknown is used then
      ///   Export will try to recognize the shape type on its own;
      /// </param>
      /// <param name="_scope">
      ///   SQL query for which the shape will be exported
      /// </param>
      /// <param name="_shape">
      ///   shape use to check relationship using DE9IM model
      /// </param>
      /// <param name="_de9im">
      ///   DE9IM model to check relationship to provided shape
      /// </param>
      /// <param name="_truncated">
      ///   if True, then all shapes will be truncated in _extent; if False,
      ///   only shape visible in _extent will be exported ;
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEWRITE
      /// </exception>
      /// <remarks>
      ///   See TGIS_LayerVector.ImportLayer for details and example.
      /// </remarks>
      procedure PrepareAltitudeData
                              ( const _layer       : TGIS_LayerVector ;
                                const _extent      : TGIS_Extent;
                                const _type        : TGIS_ShapeType ;
                                const _scope       : String ;
                                const _shape       : TGIS_Shape ;
                                const _de9im       : String ;
                                const _truncated   : Boolean
                              ) ; overload;

      /// <summary>
      ///   Approximation point elevation from Delaunay triangulation net
      /// </summary>
      /// <param name="_pt">
      ///   given point
      /// </param>
      /// <returns>
      ///   returns point elevation
      /// </returns>
      function  PointAltitude ( const _pt : TGIS_Point) : Single ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Generics.Defaults,
    System.Generics.Collections,

    GisFunctions ;
{$ENDIF}

type
  //----------------------------------------------------------------------------
  // forwards
  //----------------------------------------------------------------------------
    T_Triangulation     = class  ;  // base triangulation class
    T_VoroSite          = class  ;
    T_VoroEdge          = class  ;
    T_VoroHalfEdge      = class  ;
    T_Triple            = class  ;

  //----------------------------------------------------------------------------

   // Encapsulation of base Triangulation layer.
   T_Triangulation = class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}  // internal values
      sitesLst        : TGIS_ObjectList ;
      fedgesLst       : TGIS_ObjectList ;
      edgesLst        : TGIS_ObjectList ;
      hedgesLst       : TGIS_ObjectList ;
      triplesLst      : TGIS_ObjectList ;
      sitesArray      : Array of T_VoroSite ;
      hashSize        : Integer ;
      leftEnd         : T_VoroHalfEdge ;
      rightEnd        : T_VoroHalfEdge ;
      hashTable       : Array of T_VoroHalfEdge ;
      deltax, deltay  : Double ;
      prec_factor     : Double ;
      pqHashSize      : Integer ;
      pqHash          : Array of T_VoroHalfEdge ;
      pqCount         : Integer ;
      pqMin           : Integer ;
      sqrt_nsites     : Integer ;
      bottomsite      : T_VoroSite ;
      baseLayer       : TGIS_LayerVector ;
      numPoints       : Integer ;
      voronoiExtent   : TGIS_Extent ;
      dimension       : TGIS_DimensionType ;
    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}  // property internal values
      triangulate : Boolean ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      // <summary>
      //   Gets nearest left half edge for given point.
      // </summary>
      // <param name="_p">
      //   given point
      // </param>
      // <returns>
      //   needed half edge
      // </returns>
      function  elLeftbnd ( const _p : TGIS_Point3D
                           ) : T_VoroHalfEdge ;

      // <summary>
      //   Get entry from hash table, pruning any deleted nodes.
      // </summary>
      // <param name="_b">
      //   index to hash table
      // </param>
      // <returns>
      //   half edge corresponding to given index or nil
      //   if half edge not exist or deleted
      // </returns>
      function  elGethash ( const _b : Integer
                           ) : T_VoroHalfEdge ;

      // <summary>
      //   Marks half edge as deleted and removes it from related list.
      // </summary>
      // <param name="_he">
      //   given half edge
      // </param>
      procedure elDelete  ( const _he : T_VoroHalfEdge
                           ) ;

      // <summary>
      //   Gets left site of given half edge
      // </summary>
      // <param name="_he">
      //   given half edge
      // </param>
      function  leftreg   ( const _he : T_VoroHalfEdge
                           ) : T_VoroSite ;


      // <summary>
      //   Gets right site of given half edge
      // </summary>
      // <param name="_she">
      //   given half edge
      // </param>
      function  rightreg  ( const _he : T_VoroHalfEdge
                           ) : T_VoroSite ;

      // <summary>
      //   Gets left site of given half edge
      // </summary>
      // <param name="_he">
      //   given half edge
      // </param>
      // <param name="_v">
      //  site corresponding for half edge
      // </param>
      // <param name="_offset">
      //   distance between _v and next site
      // </param>
      procedure pqInsert  ( const _he     : T_VoroHalfEdge;
                            const _v      : T_VoroSite;
                            const _offset : Double
                           ) ;

      // <summary>
      //   Gets nearest index in hash table for given half edge (for fast searching)
      // </summary>
      // <param name="_he">
      //   given half edge
      // </param>
      function  pqHashidx ( const _he : T_VoroHalfEdge
                           ) : Integer ;

      // <summary>
      //   Deletes half edge from queue.
      // </summary>
      // <param name="_he">
      //   given half edge
      // </param>
      procedure pqDelete  ( const _he : T_VoroHalfEdge
                           ) ;

      // <summary>
      //   Cuts given edge in Voronoi extent
      // </summary>
      // <param name="_e">
      //   given edge
      // </param>
      procedure cutEdge   ( const _e : T_VoroEdge
                           ) ;

      // <summary>
      //   Gets coordinates of actual nearest point.
      // </summary>
      // <returns>
      //   needed coordinates
      // </returns>
      function  pq_min       : TGIS_Point3D;

      // <summary>
      //   Gets next half edge from queue.
      // </summary>
      // <returns>
      //   needed half edge
      // </returns>
      function  pqExtractmin : T_VoroHalfEdge ;

      // <summary>
      //   Initializing of queue
      // </summary>
      procedure pqInitialize ;

      // <summary>
      //   Initializing hash table with left and right half edges.
      // </summary>
      procedure elInitialize ;

      // <summary>
      //   Procedure collecting edges for Voronoi cells or Delaunay triangles.
      // </summary>
      procedure mvoronoi ;

      // <summary>
      //   Frees allocated memory
      // </summary>
      procedure freeData ;

      // <summary>
      //   Preparing for import layer from an existing layer.
      // </summary>
      // <remarks>
      //   See TGIS_LayerVector.ImportLayer for details and example.
      // </remarks>
      // <param name="_layer">
      //   layer to be imported
      // </param>
      // <param name="_extent">
      //   start extent of layer - can't be zero sized
      // </param>
      // <param name="_type">
      //   shape type supported by the layer; if Unknown is
      //   used then Export will try to recognize the shape
      //   type on its own;
      // </param>
      // <param name="_scope">
      //   SQL query for which the shape will be exported
      // </param>
      // <param name="_shape">
      //
      // </param>
      // <param name="_de9im">
      //
      // </param>
      // <param name="_truncated">
      //   if True, then all shapes will be truncated in
      //   _extent; if False, only shape visible in _extent
      //   will be exported ;
      // </param>
      // <exception cref="EGIS_Exception">
      //   GIS_RS_ERR_FILEWRITE
      // </exception>
      procedure prepareDataForImport( const _layer       : TGIS_LayerVector ;
                                      const _extent      : TGIS_Extent;
                                      const _type        : TGIS_ShapeType ;
                                      const _scope       : String ;
                                      const _shape       : TGIS_Shape ;
                                      const _de9im       : String ;
                                      const _truncated   : Boolean
                                    ) ;
  end ;

  // Encapsulation of Voronoi diagram site.
  T_VoroSite = class
    public
      coord     : TGIS_Point3D ;
      src_id    : TGIS_Uid ;
      neighbors : Array [0..3] of T_VoroEdge ;
  end ;

  // Encapsulation of vertices of triangle.
  T_Triple = class
    public
      b        : TGIS_Point3D ;
      t        : TGIS_Point3D ;
      r        : TGIS_Point3D ;
      b_src_id : TGIS_Uid      ;
      t_src_id : TGIS_Uid        ;
      r_src_id : TGIS_Uid      ;
  end ;

  // Encapsulation of Voronoi diagram edge.
  T_VoroEdge = class
    public
      pts      : Array [0..1] of TGIS_Point3D ;
      endcut   : Array [0..1] of Boolean ;
      a        : Double ;
      b        : Double ;
      c        : Double ;
      ep       : Array [0..1] of T_VoroSite ;
      reg      : Array [0..1] of T_VoroSite ;
      rused    : Boolean ;
      runs     : Byte ;
      outofext : Boolean ;
      redefnbr : Boolean ;
    public

      /// <summary>
      ///   Create an object.
      /// </summary>
      constructor Create( const _s1, _s2 : T_VoroSite) ;

      /// <summary>
      ///   Create left or right T_VoroHalfEdge object corresponding for T_VoroEdge object.
      /// </summary>
      /// <param name="_lr">
      ///   left (0) or right (1) indicator
      /// </param>
      /// <returns>
      ///   created T_VoroHalfEdge object
      /// </returns>
      function  HEcreate( const _lr : Integer
                         ) : T_VoroHalfEdge ;

      /// <summary>
      ///   Sets edge end point (left or right).
      /// </summary>
      /// <param name="_lr">
      ///   left (0) or right (1) indicator
      /// </param>
      /// <param name="_s">
      ///   site corresponding to end point
      /// </param>
      /// <param name="_v">
      ///   Voronoi layer using edges
      /// </param>
      procedure Endpoint( const _lr : Integer;
                          const _s  : T_VoroSite ;
                          const _v  : T_Triangulation
                         ) ;

      /// <summary>
      ///   Calculates doubled signed area of triangle given by three points.
      ///   First two are p1 and p2 from edge 3th is given as function param.
      /// </summary>
      /// <remarks>
      ///   This method is for internal use only.
      /// </remarks>
      /// <param name="_p">
      ///   given point
      /// </param>
      /// <returns>
      ///   True  - given point is on left from edge
      ///   False - on right or in line.
      /// </returns>
      function  IsOnLeft( const _p  : TGIS_Point3D
                         ) : Boolean ;
  end ;

  // Encapsulation of Voronoi diagram half edge.
  T_VoroHalfEdge = class
    public
      ELleft      : T_VoroHalfEdge ;
      ELright     : T_VoroHalfEdge ;
      ELedge      : T_VoroEdge ;
      ELpm        : Integer ;
      vertex      : T_VoroSite ;
      ystar       : Double ;
      PQnext      : T_VoroHalfEdge ;
      edgedeleted : Boolean ;
    public

      /// <summary>
      ///   Calculates intersection point for given half edge end creates
      ///   corresponding site if such point exists
      /// </summary>
      /// <param name="_el2">
      ///   sender object from a layer
      /// </param>
      /// <returns>
      ///   T_VoroSite with calculated intersection point
      /// </returns>
      function Intersect  ( const _el2 : T_VoroHalfEdge
                          ) : T_VoroSite ;

      /// <summary>
      ///   Checks half edge location relatively to given point
      /// </summary>
      /// <param name="_p">
      ///   given point
      /// </param>
      /// <returns>
      ///   True if half edge is on right
      /// </returns>
      function RightOf    ( const _p : TGIS_Point3D
                          ) : Boolean ;

      /// <summary>
      ///   Inserts self as left half edge for given one
      /// </summary>
      /// <param name="_new">
      ///   given half edge
      /// </param>
      procedure ELinsert  ( const _new : T_VoroHalfEdge
                          ) ;
  end ;

const
  TRI_MIN_DIST   = 1e-16 ;
  TRI_VERY_CLOSE = 1e-32 ;
  TRI_LEFT       = 0 ;
  TRI_RIGHT      = 1 ;


//==============================================================================
// T_VoroEdge
//==============================================================================

  constructor T_VoroEdge.Create( const _s1, _s2 : T_VoroSite ) ;
  var
    dx, dy, adx, ady : Double ;
  begin
  {$IFDEF CLR}
    inherited Create ;
  {$ENDIF}

   reg[0] := _s1 ;
   reg[1] := _s2 ;
   ep[0]  := nil ;
   ep[1]  := nil ;
   redefnbr := False ;
   {$IFDEF JAVA}
    pts := new TGIS_Point3D[2];
    for i : Integer := 0 to length(pts)-1 do begin
      pts[i] := new TGIS_Point3D;
    end;
   {$ENDIF}
   dx  := _s2.coord.X -_s1.coord.X ;
   dy  := _s2.coord.Y -_s1.coord.Y ;
   adx := Abs(dx) ;
   ady := Abs(dy) ;

   c := _s1.coord.X*dx +_s1.coord.Y*dy +(Sqr(dx) +Sqr(dy))*0.5 ;
   if adx > ady then begin
    a := 1 ;
    b := dy/dx ;
    c := c / dx ;
   end
   else begin
    a := dx/dy ;
    b := 1 ;
    c := c / dy ;
   end ;
  end ;

  function T_VoroEdge.HEcreate( const _lr : Integer ) : T_VoroHalfEdge ;
  begin
    Result := T_VoroHalfEdge.Create ;
    Result.ELedge := self ;
    Result.ELpm   := _lr ;
    Result.PQnext := nil ;
    Result.vertex := nil ;
  end ;

  procedure T_VoroEdge.Endpoint( const _lr : Integer;
                             const _s : T_VoroSite ;
                             const _v : T_Triangulation
                            ) ;
  begin
    ep[_lr] := _s ;

    if _s.neighbors[TRI_RIGHT-_lr] = nil then
       _s.neighbors[TRI_RIGHT-_lr] := self
    else begin
      if _s.neighbors[TRI_RIGHT-_lr] <> self then begin
        _s.neighbors[TRI_RIGHT-_lr +2] := self ;
      end
      else
        _s.neighbors[TRI_RIGHT-_lr +2] := self ;//surprise
    end ;

    if ep[TRI_RIGHT-_lr] = nil then
      exit ;

    _v.cutEdge(self) ;
    _v.edgesLst.Add(self) ;
    self.rused := True ;
  end ;

  function T_VoroEdge.IsOnLeft( const _p : TGIS_Point3D ) : Boolean ;
  begin
    if (pts[1].X -pts[0].X) * (_p.Y -pts[0].Y) - (_p.X -pts[0].X) * (pts[1].Y -pts[0].Y) > 0 then
      Result := True
    else
      Result := False ;
  end ;


//==============================================================================
// T_VoroHalfEdge
//==============================================================================

  function T_VoroHalfEdge.Intersect( const _el2 : T_VoroHalfEdge ) : T_VoroSite ;
  var
    e1, e2, e : T_VoroEdge ;
    el : T_VoroHalfEdge ;
    d, xint, yint : Double ;
    right_of_site : Boolean ;
  const
    DMIN = TRI_VERY_CLOSE ;
  begin
    if _el2 = nil then begin
      Result := nil ;
      exit ;
    end ;
    if (ELedge = nil) or (_el2.ELedge = nil) then begin
      Result := nil ;
      exit ;
    end ;

    e1 := ELedge ;
    e2 := _el2.ELedge ;

    if e1.reg[1] = e2.reg[1] then begin
      Result := nil ;
      exit ;
    end ;

    d := e1.a * e2.b - e1.b * e2.a ;
    if (d > -DMIN) and (d < DMIN) then begin
      Result := nil ;
      exit ;
    end ;
    xint := (e1.c*e2.b - e2.c*e1.b)/d ;
    yint := (e2.c*e1.a - e1.c*e2.a)/d ;

    if(e1.reg[1].coord.Y < e2.reg[1].coord.Y) or
      ((e1.reg[1].coord.Y = e2.reg[1].coord.Y) and
       (e1.reg[1].coord.X < e2.reg[1].coord.X))
    then begin
      el := self ;
      e := e1 ;
    end
    else begin
      el := _el2 ;
      e := e2 ;
    end ;

    right_of_site := xint >= e.reg[1].coord.X ;

    if (right_of_site and (el.ELpm = TRI_LEFT)) or
       ((not right_of_site) and (el.ELpm = TRI_RIGHT))
    then begin
      Result := nil ;
      exit ;
    end ;
    Result := T_VoroSite.Create ;
    {$IFDEF GIS_NORECORDS}
      Result.coord := new TGIS_Point3D;
    {$ENDIF}
    Result.coord.X := xint ;
    Result.coord.Y := yint ;
  end ;

  procedure T_VoroHalfEdge.ELinsert( const _new : T_VoroHalfEdge ) ;
  begin
    _new.ELleft := self ;
    _new.ELright := ELright ;
    if ELright <> nil then
      ELright.ELleft := _new ;
    ELright := _new ;
  end ;

  function T_VoroHalfEdge.RightOf( const _p : TGIS_Point3D ) : Boolean ;
  var
    e : T_VoroEdge ;
    topsite : T_VoroSite ;
    right_of_site : Boolean ;
    above : Boolean ;
    fast : Boolean ;
    dxp, dyp, dxs, t1, t2, t3, yl : Double ;
  begin
    e := ELedge ;
    if edgedeleted then begin
      Result := True ;
      exit ;
    end ;
    topsite := e.reg[1] ;
    right_of_site := (_p.X > topsite.coord.X) ;
    if right_of_site and (ELpm = TRI_LEFT) then begin
      Result := True ;
      exit ;
    end ;
    if (not right_of_site) and (ELpm = TRI_RIGHT) then begin
      Result := False ;
      exit ;
    end ;

    if e.a = 1.0 then begin
      dyp := _p.Y -topsite.coord.Y ;
      dxp := _p.X -topsite.coord.X ;
      fast := False ;
      if ((not right_of_site) and (e.b < 0)) or
         ( right_of_site and (e.b >= 0))
      then begin
        above := dyp >= e.b * dxp ;
        fast := above ;
      end
      else begin
        above := (_p.X +_p.Y*e.b) > e.c ;
        if e.b < 0 then
          above := not above ;
        if not above then
          fast := True ;
      end ;
      if not fast then begin
        dxs := topsite.coord.X - e.reg[0].coord.X ;
        above := (e.b * (Sqr(dxp) -Sqr(dyp))) <
                 (dxs*dyp*(1 + 2*dxp/dxs +Sqr(e.b))) ;
        if e.b < 0 then
          above := not above ;
      end ;
    end
    else begin //e.b = 1.0
      yl := e.c -e.a * _p.X ;
      t1 := _p.Y - yl ;
      t2 := _p.X -topsite.coord.X ;
      t3 := yl - topsite.coord.Y ;
      above := Sqr(t1) > (Sqr(t2) +Sqr(t3)) ;
    end ;
    if ELpm = TRI_LEFT then
      Result := above
    else
      Result := not above ;
  end ;


//==============================================================================
// T_Triangulation
//==============================================================================

  procedure T_Triangulation.mvoronoi ;
  var
    newsite, bot, top, temp, p : T_VoroSite ;
    v, s, bb, tt, vv : T_VoroSite ;
    newintstar : TGIS_Point3D ;
    pm : Integer ;
    lbnd, rbnd, llbnd, rrbnd, bisector : T_VoroHalfEdge ;
    e : T_VoroEdge ;
    is_pqempty : Boolean ;
    t : T_Triple ;
    idx : Integer ;

    function nextsite : T_VoroSite ;
    begin
      if idx < numPoints then begin
        Result := sitesArray[idx] ;
        inc(idx) ;
      end
      else
        Result := nil ;
    end ;

    function dist(s, t : T_VoroSite) : Double ;
    begin
      Result := Sqrt(Sqr(s.coord.X - t.coord.X) +Sqr(s.coord.Y - t.coord.Y)) ;
    end ;

  begin
    idx := 0 ;

    sqrt_nsites := TruncS(Sqrt(numPoints +4)) ;
    deltay := voronoiExtent.YMax -voronoiExtent.YMin ;
    deltax := voronoiExtent.XMax -voronoiExtent.XMin ;
    if voronoiExtent.YMax > voronoiExtent.XMax then
      prec_factor := voronoiExtent.YMax*10
    else
      prec_factor := voronoiExtent.XMax*10 ;

    pqInitialize ;
    bottomsite := nextsite() ;
    elInitialize() ;
    newsite := nextsite() ;

    while True do begin
      if not (pqCount = 0)  then
        newintstar := pq_min() ;

      is_pqempty := pqCount = 0  ;
      if (newsite <> nil) and
         ( is_pqempty or (newsite.coord.Y < newintstar.Y) or
           ((newsite.coord.Y = newintstar.Y) and (newsite.coord.X < newintstar.X))
         )
      then begin
        lbnd := elLeftbnd(newsite.coord) ;
        rbnd := lbnd.ELright ;
        bot := rightreg(lbnd) ;
        e := T_VoroEdge.Create(bot, newsite) ;
        fedgesLst.Add(e) ;

        bisector := e.HEcreate(TRI_LEFT) ;
        hedgesLst.Add(bisector) ;
        lbnd.ELinsert(bisector) ;
        if lbnd <> nil then begin
          p := lbnd.Intersect(bisector) ;
          if p <> nil then begin
            sitesLst.Add(p) ;
            pqDelete(lbnd) ;
            pqInsert(lbnd, p, dist(p, newsite)) ;
          end ;
        end ;
        lbnd := bisector ;
        bisector := e.HEcreate(TRI_RIGHT) ;
        hedgesLst.Add(bisector) ;
        lbnd.ELinsert(bisector) ;
        if bisector <> nil then begin
          p := bisector.Intersect(rbnd) ;
          if p <> nil then
            pqInsert(bisector, p, dist(p, newsite)) ;
            sitesLst.Add(p) ;
        end ;
        newsite := nextsite() ;
      end
      else
      if not is_pqempty then begin
        lbnd := pqExtractmin() ;
        llbnd := lbnd.ELleft ;
        rbnd := lbnd.ELright ;
        rrbnd := rbnd.ELright ;
        bot := leftreg(lbnd) ;
        top := rightreg(rbnd) ;
        //out_triple   bot, top, rightreg(lbnd)
        if triangulate then begin
          t := T_Triple.Create ;
          v := rightreg(lbnd) ;

          vv := v ;
          tt := top ;
          bb := bot ;

          if tt.coord.Y < bb.coord.Y then begin
            s := tt ;
            tt := bb ;
            bb := s ;
          end
          else
          if tt.coord.Y = bb.coord.Y then begin
            if tt.coord.X > bb.coord.X then begin
              s := tt ;
              tt := bb ;
              bb := s ;
            end ;
          end ;

          if tt.coord.Y < vv.coord.Y then begin
            s := tt ;
            tt := vv ;
            vv := s ;
          end
          else
          if tt.coord.Y = vv.coord.Y then begin
            if top.coord.X > v.coord.X then begin
              s := tt ;
              tt := vv ;
              vv := s ;
            end ;
          end ;

          if bb.coord.Y > vv.coord.Y then begin
            s := bb ;
            bb := vv ;
            vv := s ;
          end
          else
          if bb.coord.Y = vv.coord.Y then begin
            if bb.coord.X > vv.coord.X then begin
              s := bb ;
              bb := vv ;
              vv := s ;
            end ;
          end ;
          //if t.r on left
          if ((tt.coord.X -bb.coord.X) * (vv.coord.Y -bb.coord.Y) -
              (vv.coord.X -bb.coord.X) * (tt.coord.Y -bb.coord.Y)) > 0 then
          begin
            s := bb ;
            bb := vv ;
            vv := s ;
          end ;

          t.t := tt.coord ;
          t.t_src_id := tt.src_id ;
          t.b := bb.coord ;
          t.b_src_id := bb.src_id ;

          t.r := vv.coord ;
          t.r_src_id := vv.src_id ;
          triplesLst.Add(t) ;
        end ;

        rightreg(lbnd) ;
        v := lbnd.vertex ;
        lbnd.ELedge.Endpoint(lbnd.ELpm, v, self) ;
        rbnd.ELedge.Endpoint(rbnd.ELpm, v, self) ;
        elDelete(lbnd) ;
        pqDelete(rbnd) ;
        elDelete(rbnd) ;
        pm := TRI_LEFT ;
        if bot.coord.Y > top.coord.Y then begin
          temp := bot ;
          bot := top ;
          top := temp ;
          pm := TRI_RIGHT ;
        end ;
        e := T_VoroEdge.Create(bot, top) ;
        fedgesLst.Add(e) ;

        bisector := e.HEcreate(pm) ;
        hedgesLst.Add(bisector) ;
        llbnd.ELinsert(bisector) ;
        e.Endpoint(TRI_RIGHT -pm, v, self) ;
        if llbnd <> nil then begin
          p := llbnd.Intersect(bisector) ;
          if p <> nil then begin
            sitesLst.Add(p) ;
            pqDelete(llbnd) ;
            pqInsert(llbnd, p, dist(p, bot)) ;
          end ;
        end ;
        if bisector <> nil then begin
          p := bisector.Intersect(rrbnd) ;
          if p <> nil then
            pqInsert(bisector, p, dist(p, bot)) ;
            sitesLst.Add(p) ;
        end ;
      end
      else
        break ;
    end ;
    hashTable := nil ;
    pqHash := nil ;
  end ;

  procedure T_Triangulation.pqInitialize ;
  var
    i : Integer ;
  begin
    pqCount := 0 ;
    pqMin := 0 ;
    pqHashSize := 4*sqrt_nsites ;
    SetLength(pqHash, pqHashSize) ;
    for i := 0 to pqHashSize -1 do begin
      pqHash[i] := T_VoroHalfEdge.Create ;
      hedgesLst.Add(pqHash[i]) ;
      pqHash[i].PQnext := nil ;
    end ;
  end ;

  function T_Triangulation.pqExtractmin : T_VoroHalfEdge ;
  begin
    Result := pqHash[pqMin].PQnext ;
    pqHash[pqMin].PQnext := Result.PQnext ;
    dec(pqCount) ;
  end ;

  function T_Triangulation.pq_min : TGIS_Point3D ;
  begin
    while pqHash[pqMin].PQnext = nil do
      inc(pqMin) ;
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D;
    {$ENDIF}
    Result.X := pqHash[pqMin].PQnext.vertex.coord.X ;
    Result.Y := pqHash[pqMin].PQnext.ystar ;
  end ;

  procedure T_Triangulation.pqDelete( const _he : T_VoroHalfEdge ) ;
  var
    last : T_VoroHalfEdge ;
  begin
    if _he.vertex <> nil then begin
      last := pqHash[pqHashidx(_he)] ;
      while last.PQnext <> _he do last := last.PQnext ;

      last.PQnext := _he.PQnext ;
      dec(pqCount) ;
      _he.vertex := nil ;
    end ;
  end ;

  function T_Triangulation.pqHashidx( const _he : T_VoroHalfEdge ) : Integer ;
  var
    wv : Double ;
  begin

    wv := (_he.ystar -voronoiExtent.YMin)/deltay*pqHashSize ;
    if wv < 0 then
      Result := 0
    else
    if wv >= pqHashSize then
      Result := pqHashSize -1
    else
      Result := TruncS(wv) ;

    if Result < pqMin then
      pqMin := Result ;

  end ;

  procedure T_Triangulation.pqInsert( const _he       : T_VoroHalfEdge;
                                        const _v      : T_VoroSite;
                                        const _offset : Double
                                       ) ;
  var
    last, next : T_VoroHalfEdge ;
  begin
    _he.vertex := _v ;
    _he.ystar := _v.coord.Y +_offset ;
    last := pqHash[pqHashidx(_he)] ;

    next := last.PQnext ;
    while (next <> nil) and
          ((_he.ystar > next.ystar) or ((_he.ystar = next.ystar) and
                                       (_v.coord.X > next.vertex.coord.X)
                                      )
          )
    do begin
      last := next ;
      next := last.PQnext ;
    end ;
    _he.PQnext := last.PQnext ;
    last.PQnext := _he ;
    inc(pqCount) ;
  end ;

  function T_Triangulation.leftreg( const _he : T_VoroHalfEdge ) : T_VoroSite ;
  begin
    if _he.ELedge = nil then
      Result := bottomsite
    else
    if _he.ELpm = TRI_LEFT then
      Result := _he.ELedge.reg[TRI_LEFT]
    else
      Result := _he.ELedge.reg[TRI_RIGHT] ;
  end ;

  function T_Triangulation.rightreg( const _he : T_VoroHalfEdge ) : T_VoroSite ;
  begin
    if _he.ELedge = nil then
      Result := bottomsite
    else
    if _he.ELpm = TRI_LEFT then
      Result := _he.ELedge.reg[TRI_RIGHT]
    else
      Result := _he.ELedge.reg[TRI_LEFT] ;
  end ;

  procedure T_Triangulation.elDelete( const _he : T_VoroHalfEdge ) ;
  begin
    _he.ELleft.ELright := _he.ELright ;
    _he.ELright.ELleft := _he.ELleft ;
    _he.edgedeleted    := True ;
  end ;

  function T_Triangulation.elLeftbnd( const _p : TGIS_Point3D ) : T_VoroHalfEdge ;
  var
    i, hashidx : Integer ;
  begin
    //Use hash table to get close to desired half edge
    if deltax = 0 then
      hashidx := hashSize -1
    else begin
      hashidx := TruncS((_p.X -voronoiExtent.XMin)/deltax*hashSize) ;
      if hashidx < 0 then
        hashidx := 0 ;
      if hashidx >= hashSize then
        hashidx := hashSize -1 ;
    end ;
    Result := elGethash(hashidx) ;
    if Result = nil then begin
      i := 1 ;
      while True do begin
        Result := elGethash(hashidx -i) ;
        if Result <> nil then
          break ;
        Result := elGethash(hashidx +i) ;
        if Result <> nil then
          break ;
        inc(i) ;
      end ;
    end ;
    //Now search linear list of half edges for the correct one
    if (Result = leftEnd) or
       ((Result <> rightEnd) and Result.RightOf(_p))
    then begin
      repeat
        Result := Result.ELright
      until not ((Result <> rightEnd) and (Result.RightOf(_p)));
      Result := Result.ELleft
    end
    else begin
      repeat
        Result := Result.ELleft
      until not ((Result <> leftEnd) and (not Result.RightOf(_p)));
    end ;
    //Update hash table and reference counts
    if (hashidx > 0) and (hashidx < hashSize -1) then begin
      hashTable[hashidx] := Result ;
    end ;
  end ;

  function T_Triangulation.elGethash( const _b : Integer ) : T_VoroHalfEdge ;
  begin
    if (_b < 0) or (_b >= hashSize) then begin
      Result := nil ;
      exit ;
    end ;
    Result := hashTable[_b] ;
    if (Result = nil) then
      exit ;
    if not Result.edgedeleted then
      exit ;
    hashTable[_b] := nil ;
    Result := nil ;
  end ;

  procedure T_Triangulation.elInitialize ;
  begin
    hashSize := 2*sqrt_nsites ;
    SetLength(hashTable, hashSize) ;
    leftEnd := T_VoroHalfEdge.Create ;
    hedgesLst.Add(leftEnd) ;
    rightEnd := T_VoroHalfEdge.Create ;
    hedgesLst.Add(rightEnd) ;

    leftEnd.ELleft := nil ;

    leftEnd.ELright := rightEnd ;
    rightEnd.ELleft := leftEnd ;

    rightEnd.ELright := nil ;
    hashTable[0] := leftEnd ;
    hashTable[hashSize -1] := rightEnd ;
  end ;

  procedure T_Triangulation.cutEdge( const _e : T_VoroEdge ) ;
  var
    s0 , s1 : T_VoroSite ;
    rev : Boolean ;
    pt : TGIS_Point3D ;
    cut0, cut1 : Boolean ;

  begin
    cut0 := False ;
    cut1 := False ;

    if (_e.a = 1) and (_e.b >= 0) then begin
      s0 := _e.ep[1] ;
      s1 := _e.ep[0] ;
      rev := True ;
    end
    else begin
      s0 := _e.ep[0] ;
      s1 := _e.ep[1] ;
      rev := False ;
    end ;

    if s0 = nil then
      cut0 := true ;

    if s1 = nil then
      cut1 := true ;
    if _e.a = 1 then begin
      _e.pts[0].Y := voronoiExtent.YMin ;
      if s0 <> nil then
        if s0.coord.Y > voronoiExtent.YMin then
          _e.pts[0].Y := s0.coord.Y
        else
          cut0 := True ;

      if _e.pts[0].Y > voronoiExtent.YMax then begin
        _e.outofext := True ;
        exit ;
      end ;
      _e.pts[0].X := _e.c -_e.b*_e.pts[0].Y ;

      _e.pts[1].Y := voronoiExtent.YMax;
      if s1 <> nil then
        if s1.coord.Y < voronoiExtent.YMax then
          _e.pts[1].Y := s1.coord.Y
        else
          cut1 := True ;

      if _e.pts[1].Y < voronoiExtent.YMin then begin
        _e.outofext := True ;
        exit ;
      end ;
      _e.pts[1].X := _e.c -_e.b*_e.pts[1].Y ;
      if ((_e.pts[0].X > voronoiExtent.XMax) and (_e.pts[1].X > voronoiExtent.XMax))
         or
         ((_e.pts[0].X < voronoiExtent.XMin) and (_e.pts[1].X < voronoiExtent.XMin))
      then begin
        _e.outofext := True ;
        exit ;
      end ;

      if _e.pts[0].X > voronoiExtent.XMax then begin
        _e.pts[0].X := voronoiExtent.XMax ;
        _e.pts[0].Y := (_e.c - _e.pts[0].X)/_e.b ;
        cut0 := True ;
      end ;
      if _e.pts[0].X < voronoiExtent.XMin then begin
        _e.pts[0].X := voronoiExtent.XMin ;
        _e.pts[0].Y := (_e.c - _e.pts[0].X)/_e.b ;
        cut0 := True ;
      end ;

      if _e.pts[1].X > voronoiExtent.XMax then begin
        _e.pts[1].X := voronoiExtent.XMax ;
        _e.pts[1].Y := (_e.c - _e.pts[1].X)/_e.b ;
        cut1 := True ;
      end ;
      if _e.pts[1].X < voronoiExtent.XMin then begin
        _e.pts[1].X := voronoiExtent.XMin ;
        _e.pts[1].Y := (_e.c - _e.pts[1].X)/_e.b ;
        cut1 := True ;
      end ;

    end
    else begin
      _e.pts[0].X := voronoiExtent.XMin ;
      if s0 <> nil then
        if s0.coord.X > voronoiExtent.XMin then
          _e.pts[0].X := s0.coord.X
        else
          cut0 := True ;

      if _e.pts[0].X > voronoiExtent.XMax then begin
        _e.outofext := True ;
        exit ;
      end ;

      _e.pts[0].Y := _e.c -_e.a*_e.pts[0].X ;

      _e.pts[1].X := voronoiExtent.XMax;
      if s1 <> nil then
        if s1.coord.X < voronoiExtent.XMax then
          _e.pts[1].X := s1.coord.X
        else
          cut1 := True ;
      if _e.pts[1].X < voronoiExtent.XMin then begin
        _e.outofext := True ;
        exit ;
      end ;

      _e.pts[1].Y := _e.c -_e.a*_e.pts[1].X ;

      if ((_e.pts[0].Y > voronoiExtent.YMax) and (_e.pts[1].Y > voronoiExtent.YMax))
          or
         ((_e.pts[0].Y < voronoiExtent.YMin) and (_e.pts[1].Y < voronoiExtent.YMin))
      then begin
        _e.outofext := True ;
        exit ;
      end ;

      if _e.pts[0].Y > voronoiExtent.YMax then begin
        _e.pts[0].Y := voronoiExtent.YMax ;
        _e.pts[0].X := (_e.c - _e.pts[0].Y)/_e.a ;
        cut0 := True ;
      end ;
      if _e.pts[0].Y < voronoiExtent.YMin then begin
        _e.pts[0].Y := voronoiExtent.YMin ;
        _e.pts[0].X := (_e.c - _e.pts[0].Y)/_e.a ;
        cut0 := True ;
      end ;

      if _e.pts[1].Y > voronoiExtent.YMax then begin
        _e.pts[1].Y := voronoiExtent.YMax ;
        _e.pts[1].X := (_e.c - _e.pts[1].Y)/_e.a ;
        cut1 := True ;
      end ;
      if _e.pts[1].Y < voronoiExtent.YMin then begin
        _e.pts[1].Y := voronoiExtent.YMin ;
        _e.pts[1].X := (_e.c - _e.pts[1].Y)/_e.a ;
        cut1 := True ;
      end ;
    end ;

    if rev then begin
      pt := _TGIS_Point3D(_e.pts[0]) ;
      _e.pts[0] := _TGIS_Point3D(_e.pts[1]) ;
      _e.pts[1] := _TGIS_Point3D(pt) ;
      if cut0 then
        _e.endcut[1] := True ;
      if cut1 then
        _e.endcut[0] := True ;
    end
    else begin
      if cut0 then
        _e.endcut[0] := True ;
      if cut1 then
        _e.endcut[1] := True ;
    end ;
  end ;

  // Points coordinate comparing function using in quick sorting
  function PointsCompare( const _p1, _p2 : TObject ) : Integer ;
  var
    p1, p2 : TGIS_Point3D ;
  begin
    p1 := _TGIS_Point3D(T_VoroSite(_p1).coord) ;
    p2 := _TGIS_Point3D(T_VoroSite(_p2).coord) ;
    if p1.Y < p2.Y then
      Result := -1
    else
    if p1.Y > p2.Y then
      Result := 1
    else begin
      if p1.X < p2.X then
        Result := -1
      else
      if p1.X > p2.X then
        Result := 1
      else
        Result := 0 ;
    end ;
  end ;

  // Triples top points coordinate comparing function using in quick sorting
  function TriplesCompare( const _t1, _t2 : TObject ) : Integer ;
  var
    p1, p2 : TGIS_Point3D ;
  begin
    p1 := _TGIS_Point3D(T_Triple(_t1).t) ;
    p2 := _TGIS_Point3D(T_Triple(_t2).t) ;
    if p1.Y < p2.Y then
      Result := -1
    else
    if p1.Y > p2.Y then
      Result := 1
    else begin
      if p1.X < p2.X then
        Result := -1
      else
      if p1.X > p2.X then
        Result := 1
      else
        Result := 0 ;
    end ;
  end ;

  procedure T_Triangulation.freeData ;
  var
    i : Integer ;
  begin

    {$IFNDEF NEXTGEN}
      for i := hedgesLst.Count -1 downto 0 do
        FreeObjectNotNil( T_VoroHalfEdge(hedgesLst.Items[i]) ) ;
    {$ENDIF}
    FreeObject( hedgesLst ) ;

    FreeObject( edgesLst ) ;

    {$IFNDEF NEXTGEN}
      for i := fedgesLst.Count -1 downto 0 do
        FreeObjectNotNil( T_VoroEdge(fedgesLst.Items[i]) ) ;
    {$ENDIF}
    FreeObject( fedgesLst ) ;

    {$IFNDEF NEXTGEN}
      for i := sitesLst.Count -1 downto 0 do
        FreeObjectNotNil( T_VoroSite(sitesLst.Items[i]) ) ;
    {$ENDIF}
    FreeObject( sitesLst ) ;

    {$IFNDEF NEXTGEN}
      for i := 0 to numPoints -1 do
        FreeObjectNotNil( T_VoroSite(sitesArray[i]) ) ;
    {$ENDIF}
    sitesArray := nil ;

  end ;

  procedure T_Triangulation.prepareDataForImport(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape ;
    const _de9im     : String ;
    const _truncated : Boolean
  ) ;
  var
    j, k  : Integer ;
    site  : T_VoroSite ;
    pl    : TGIS_ObjectList ;
    ext   : TGIS_Extent ;
    {$IFDEF DCC}
    shp   : TGIS_Shape ;
    {$ENDIF}
    dist2 : Double ;
    first : Boolean ;
    extent : TGIS_Extent ;
    ny, fy : Double ;
    maxval : Double ;
  begin
    if not assigned(_layer) then
      exit ;
    baseLayer := _layer ;
    extent :=_extent ;
    if FileExists(baseLayer.Path) then
      baseLayer.Open ;
    if extent.XMin >= extent.XMax then
      extent := baseLayer.Extent ;

    if _truncated or (  _layer.Viewer = nil ) then begin
      voronoiExtent := extent ;
    end
    else begin
      ext :=  _layer.Viewer.Ref.VisibleExtent ;
      voronoiExtent := GisCommonExtent( ext, extent ) ;
    end ;

    if Abs(voronoiExtent.YMin) > Abs(voronoiExtent.YMax) then
      maxval := Abs(voronoiExtent.YMin)
    else
      maxval := Abs(voronoiExtent.YMax) ;

    fy := 1e14 ;
    while maxval >1 do begin
      fy := fy/10 ;
      maxval := maxval/10 ;
    end;

    pl := TGIS_ObjectList.Create( False ) ;

    first := True ;
    for shp in baseLayer.Loop(
      voronoiExtent, _scope, _shape, _de9im
    ) do begin
      if first then begin
        dimension := TGIS_Shape(shp).Dimension ;
        first := False ;
      end;
      if TGIS_Shape(shp).ShapeType = TGIS_ShapeType.MultiPoint then begin
        for j := 0 to TGIS_Shape(shp).GetNumParts -1 do begin
          for k := 0 to TGIS_Shape(shp).GetPartSize(j) -1 do begin
            site := T_VoroSite.Create ;
            site.coord :=  TGIS_Shape(shp).GetPoint3D(j, k) ;
            site.src_id := TGIS_Shape(shp).Uid ;
            pl.Add(site) ;
          end ;
        end ;
      end
      else
      if TGIS_Shape(shp).ShapeType = TGIS_ShapeType.Point then begin
        site := T_VoroSite.Create ;
        site.coord := TGIS_Shape(shp).GetPoint3D(0, 0) ;
        ny := (TruncS(site.coord.Y*fy))/fy ;
        site.coord.Y := ny ;
        pl.Add(site) ;
        site.src_id := TGIS_Shape(shp).Uid ;
      end
      else begin
        site := T_VoroSite.Create ;
        site.coord := GisPoint3DFrom2D( TGIS_Shape(shp).Centroid ) ;
        pl.Add(site) ;
        site.src_id := TGIS_Shape(shp).Uid ;
      end ;
    end ;

    if pl.Count = 0 then begin
      FreeObject( pl ) ;
      exit ;
    end ;

    {$IFNDEF OXYGENE}
      pl.Sort(  PointsCompare ) ;
    {$ELSE}
      pl.Sort( @PointsCompare ) ;
    {$ENDIF}
    site := T_VoroSite(pl.Items[0]) ;
    SetLength(sitesArray, pl.Count) ;
    sitesArray[0] := site ;

    numPoints := 1 ;

    for k := 1 to pl.Count -1 do begin
      dist2 := Sqr(T_VoroSite(pl.Items[k]).coord.X - site.coord.X) +
               Sqr(T_VoroSite(pl.Items[k]).coord.Y - site.coord.Y) ;
      if dist2 <  TRI_VERY_CLOSE then
      begin
        FreeObjectNotNil( T_VoroSite(pl.Items[k]) ) ;
        continue ;
      end ;
      site := T_VoroSite(pl.Items[k]) ;
      sitesArray[numPoints] := site ;
      inc(numPoints) ;
    end ;

    FreeObject( pl ) ;

    sitesLst  := TGIS_ObjectList.Create( False ) ;
    edgesLst  := TGIS_ObjectList.Create( False ) ;
    fedgesLst := TGIS_ObjectList.Create( False ) ;
    hedgesLst := TGIS_ObjectList.Create( False ) ;

    mvoronoi ;
//
  end ;


//==============================================================================
// TGIS_LayerVoronoi
//==============================================================================

  constructor TGIS_LayerVoronoi.Create ;
  begin
    inherited ;
    trObj := T_Triangulation.Create ;
    T_Triangulation(trObj).dimension := TGIS_DimensionType.Unknown ;
  end ;

  procedure TGIS_LayerVoronoi.doDestroy ;
  begin
    FreeObject( trObj ) ;
    inherited ;
  end ;

  procedure TGIS_LayerVoronoi.ImportLayerEx(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent;
    const _type        : TGIS_ShapeType ;
    const _scope       : String ;
    const _shape       : TGIS_Shape ;
    const _de9im       : String ;
    const _truncated   : Boolean
  ) ;
  var
    lbnd : T_VoroHalfEdge ;
    lne  : T_VoroEdge ;
    e    : T_VoroEdge ;
    ls   : T_VoroSite ;
  begin
    T_Triangulation(trObj).triangulate := False ;
    T_Triangulation(trObj).prepareDataForImport(
                            _layer,
                            _extent,
                            _type,
                            _scope,
                            _shape,
                            _de9im,
                            _truncated
                          ) ;

    if not assigned(T_Triangulation(trObj).leftEnd) then
      exit ;

    lbnd := T_Triangulation(trObj).leftEnd.ELright ;

    lne := nil ;
    while true do begin
      e := lbnd.ELedge ;
      if e = nil then
        break ;
      if not e.rused then begin
        e.rused := True ;
        if (e.reg[0] <> nil) or (e.reg[1] <> nil) or
           (T_Triangulation(trObj).edgesLst.Count = 0) then begin
          T_Triangulation(trObj).cutEdge(e) ;
          if not e.outofext then begin
            if (e.ep[0] = nil) and (e.ep[1] = nil) then begin //parallel edges
              if lne <> nil then begin
                ls := T_VoroSite.Create ;
                T_Triangulation(trObj).sitesLst.Add(ls) ;
                ls.neighbors[0]  := lne ;
                ls.neighbors[1] := e ;
                if lne.ep[0] = nil then begin
                   e.ep[0] := ls ;
                  lne.ep[0] := ls ;
                end
                else begin
                  lne.ep[1] := ls ;
                  e.ep[1] := ls ;
                end ;
              end ;
              lne := e ;
            end ;
          end ;
          T_Triangulation(trObj).edgesLst.Add(e)
        end ;
      end ;
      if lbnd = T_Triangulation(trObj).rightEnd then
        break ;
      lbnd := lbnd.ELright ;
    end ;

    completteVoronoiCells ;

    T_Triangulation(trObj).freeData ;
    RecalcExtent ;
  end;

  procedure TGIS_LayerVoronoi.completteVoronoiCells ;
  const
    GIS_SRCID = 'GIS_SRCID' ;
    GIS_SRC_X = 'GIS_SRC_X' ;
    GIS_SRC_Y = 'GIS_SRC_Y' ;
  var
    e, ef : T_VoroEdge ;
    v, vf, vl : T_VoroSite ;
    idx : TGIS_Uid ;
    k : Integer ;
    ptb, pte : TGIS_Point3D ;
    backward : Boolean ;
    fbackward : Boolean ;
    lbackward : Boolean ;
    vList  : TGIS_ObjectList ;
    svList : TGIS_ObjectList ;
    fList : TGIS_ObjectList ;
    polygon : TGIS_ShapePolygon ;
    cs : T_VoroSite ;
    endcut : Boolean ;
    fromext : Boolean ;
     x_min, x_max, y_min, y_max : Double ;
    df : Int64 ;
    dfmax : Double ;

    procedure redef_neighbors(const _e : T_VoroEdge) ;
    var
      i  : Integer ;
      prec : Double ;
      pt   : TGIS_Point3D ;
      vve, ed : T_VoroEdge ;
      rv  : T_VoroEdge ;
      absx, absy : Double ;
      count : Integer ;
    begin

      pt := _TGIS_Point3D(_e.pts[0]) ;
      pt.X := _e.pts[0].X +((_e.pts[1].X -_e.pts[0].X)/2) ;
      pt.Y := _e.pts[0].Y +((_e.pts[1].Y -_e.pts[0].Y)/2) ;

      _e.redefnbr := True ;
      _e.pts[0] := _TGIS_Point3D(pt) ;
      _e.pts[1] := _TGIS_Point3D(pt) ;

      prec :=  T_Triangulation(trObj).prec_factor*TRI_MIN_DIST ;

      rv := _e ;
      count := 0 ;

      // edSentinel
      for i := 0 to T_Triangulation(trObj).edgesLst.Count -1 do begin
        ed := T_VoroEdge(T_Triangulation(trObj).edgesLst.Items[i]) ;
        if ed = _e then
          continue ;

        vve := ed ;
        absx := Abs(vve.pts[0].X - pt.X) ;
        absy := Abs(vve.pts[0].Y - pt.Y) ;
        if (absx <= prec) and
           (absy <= prec) and
           ((absy + absx) > 0 )then begin
           vve.pts[0] := pt ;
           inc(count) ;
        end
        else begin
          absx := Abs(vve.pts[1].X - pt.X) ;
          absy := Abs(vve.pts[1].Y - pt.Y) ;
          if (absx <= prec) and
             (absy <= prec) and
             ((absy + absx) > 0 )then begin
             vve.pts[1] := _TGIS_Point3D(pt) ;
             inc(count) ;
          end;
        end ;
        if count = 4 then
          break ;
      end ;
    end ;

    function find_next_edge : T_VoroEdge ;
    var
      i  : Integer ;
      vv  : T_VoroSite ;
    begin
      Result := nil ;

      if vl = nil then
        exit
      else
        vv := vl ;

      for i := 0 to 3 do begin
        if vv.neighbors[i] <> nil then begin
          if vv.neighbors[i] <> e then begin
            if vv.neighbors[i].runs < 2 then begin
              if vv.neighbors[i].reg[TRI_LEFT] = cs then begin
                Result := vv.neighbors[i] ;
                inc(Result.runs) ;
                Result.rused := False ;
                if Result.ep[0] = vv then
                  backward := False
                else
                  backward := True ;
                exit ;
              end else
              if vv.neighbors[i].reg[TRI_RIGHT] = cs then begin
                Result := vv.neighbors[i] ;
                inc(Result.runs) ;
                Result.rused := True ;
                if Result.ep[0] = vv then
                  backward := False
                else
                  backward := True ;
                exit ;
              end ;
            end ;
          end ;
        end ;
      end ;
      if Result = nil then begin
        if backward then begin
          if e.endcut[0] then
            vl := nil ;
        end
        else begin
          if e.endcut[1] then
            vl := nil ;
        end ;
      end ;
    end ;

    procedure set_vl ;
    begin
      if backward = True then begin
        vl  :=  e.ep[0] ;
      end
      else begin
        vl  :=  e.ep[1] ;
      end ;

    end ;

    procedure move_to_polygon(const _l : TGIS_ObjectList ;const _bw : Boolean) ;
    var
      i : Integer ;
      lv : T_VoroSite ;

      expt : TGIS_Point3D ;
      lvpt : TGIS_Point3D ;
      maxid : Integer ;

      started : Boolean ;

      procedure add_corner_if_needed ;
      var
        cor : TGIS_Point3D ;
      begin
        {$IFDEF GIS_NORECORDS}
          cor := new TGIS_Point3D;
        {$ENDIF}
        cor.Z := cs.coord.Z ;
        cor.M := cs.coord.M ;
        if (lvpt.X <> expt.X) and (lvpt.Y <> expt.Y) then begin
          if lvpt.X = T_Triangulation(trObj).voronoiExtent.XMin then
            cor.X := T_Triangulation(trObj).voronoiExtent.XMin
          else
          if lvpt.X = T_Triangulation(trObj).voronoiExtent.XMax then
            cor.X := T_Triangulation(trObj).voronoiExtent.XMax
          else
          if expt.X = T_Triangulation(trObj).voronoiExtent.XMin then
            cor.X := T_Triangulation(trObj).voronoiExtent.XMin
          else
            cor.X := T_Triangulation(trObj).voronoiExtent.XMax ;

          if lvpt.Y = T_Triangulation(trObj).voronoiExtent.YMin then
            cor.Y := T_Triangulation(trObj).voronoiExtent.YMin
          else
          if lvpt.Y = T_Triangulation(trObj).voronoiExtent.YMax then
            cor.Y := T_Triangulation(trObj).voronoiExtent.YMax
          else
          if expt.Y = T_Triangulation(trObj).voronoiExtent.YMin then
            cor.Y := T_Triangulation(trObj).voronoiExtent.YMin
          else
            cor.Y := T_Triangulation(trObj).voronoiExtent.YMax ;
          polygon.AddPoint3D(cor) ;
        end ;
      end ;

      function std_pt(const _ptns : TGIS_Point3D) : TGIS_Point3D ;
      var
        ix, iy : Int64 ;
      begin
        {$IFDEF GIS_NORECORDS}
          Result := new TGIS_Point3D;
        {$ENDIF}
        Result.Z := _ptns.Z ;
        Result.M := _ptns.M ;
        ix := RoundS(_ptns.X * df) ;
        Result.X := ix / df ;
        iy := RoundS(_ptns.Y * df) ;
        Result.Y := iy / df ;
      end;

    begin

      if _bw then begin
        lv := T_VoroEdge(_l.Items[0]).ep[0] ;
        ptb := _TGIS_Point3D(T_VoroEdge(_l.Items[0]).pts[1]) ;
        pte := _TGIS_Point3D(T_VoroEdge(_l.Items[0]).pts[0]) ;
      end
      else begin
        lv := T_VoroEdge(_l.Items[0]).ep[1] ;
        ptb := _TGIS_Point3D(T_VoroEdge(_l.Items[0]).pts[0]) ;
        pte := _TGIS_Point3D(T_VoroEdge(_l.Items[0]).pts[1]) ;
      end ;

      ptb.Z := cs.coord.Z ;
      ptb.M := cs.coord.M ;
      pte.Z := cs.coord.Z ;
      pte.M := cs.coord.M ;

      if not T_VoroEdge(_l.Items[0]).outofext then begin
        polygon.AddPoint3D(std_pt(ptb)) ;
        polygon.AddPoint3D(std_pt(pte)) ;
        lvpt := _TGIS_Point3D(pte) ;
        started := True ;
      end
      else
        started := False ;

      if _l.Count = 1 then
        exit ;

      for maxid := _l.Count -1 downto 1 do
        if not T_VoroEdge(_l.Items[maxid]).outofext then
          break ;

      for i := 1 to maxid do begin
        if lv = T_VoroEdge(_l.Items[i]).ep[1] then begin
          if not T_VoroEdge(_l.Items[i]).outofext then begin
            endcut := T_VoroEdge(_l.Items[i]).endcut[1] ;
            if endcut then begin
              expt := _TGIS_Point3D(T_VoroEdge(_l.Items[i]).pts[1]) ;
              expt.Z := cs.coord.Z ;
              expt.M := cs.coord.M ;
              if started then
                add_corner_if_needed ;
            end ;
          end ;
          pte := _TGIS_Point3D(T_VoroEdge(_l.Items[i]).pts[0]) ;
          pte.Z := cs.coord.Z ;
          pte.M := cs.coord.M ;
          lv := T_VoroEdge(_l.Items[i]).ep[0] ;
        end
        else begin
          if not T_VoroEdge(_l.Items[i]).outofext then begin
            endcut := T_VoroEdge(_l.Items[i]).endcut[0] ;
            if endcut then begin
              expt := _TGIS_Point3D(T_VoroEdge(_l.Items[i]).pts[0]) ;
              expt.Z := cs.coord.Z ;
              expt.M := cs.coord.M ;
              if started then
                add_corner_if_needed ;
            end ;
          end ;
          pte := _TGIS_Point3D(T_VoroEdge(_l.Items[i]).pts[1]) ;
          pte.Z := cs.coord.Z ;
          pte.M := cs.coord.M ;
          lv := T_VoroEdge(_l.Items[i]).ep[1] ;
        end ;
        if not T_VoroEdge(_l.Items[i]).outofext then begin
          if endcut then begin
            if not started then
              ptb := _TGIS_Point3D(expt) ;
            polygon.AddPoint3D(std_pt(expt)) ;
          end ;

          lvpt := _TGIS_Point3D(pte) ;
          polygon.AddPoint3D(std_pt(pte)) ;
          started := True ;
        end ;
      end ;
    end ;

    function find_first_edge : T_VoroEdge ;
    var
      i  : Integer ;
      ed : T_VoroEdge ;
      prec : Double ;
    begin
      Result := nil ;
      prec :=  T_Triangulation(trObj).prec_factor*TRI_MIN_DIST ;
      for i := edSentinel to T_Triangulation(trObj).edgesLst.Count -1 do begin
        ed := T_VoroEdge(T_Triangulation(trObj).edgesLst.Items[i]) ;
        if ed.runs < 2 then begin
          if not ed.outofext then begin
            if (Abs(ed.pts[0].X - ed.pts[1].X) > prec) or
               (Abs(ed.pts[0].Y - ed.pts[1].Y) > prec)
            then begin
              inc(ed.runs) ;
              Result := ed ;
              edSentinel := i ;
              break ;
            end ;
          end ;
        end ;
      end ;
    end ;

    procedure check_edges ;
    var
      i  : Integer ;
      ed : T_VoroEdge ;
      prec : Double ;
    begin
      prec :=  T_Triangulation(trObj).prec_factor*TRI_MIN_DIST ;
      for i := 0 to T_Triangulation(trObj).edgesLst.Count -1 do begin
        ed := T_VoroEdge(T_Triangulation(trObj).edgesLst.Items[i]) ;
        if not ed.outofext then begin
          if ( Abs(ed.pts[0].X - ed.pts[1].X) > prec) or
             ( Abs(ed.pts[0].Y - ed.pts[1].Y) > prec) then begin
            continue ;
          end
          else begin
            if not ed.redefnbr then
              redef_neighbors(ed) ;
          end;
        end ;
      end ;
    end ;

  begin

    self.ImportStructure(T_Triangulation(trObj).baseLayer);
    if FieldInfo( FindField( GIS_SRCID ) ) = nil then
      self.AddField(GIS_SRCID, TGIS_FieldType.Number, 10, 0);
    if FieldInfo( FindField( GIS_SRC_X ) ) = nil then
      self.AddField(GIS_SRC_X, TGIS_FieldType.Float, 0, 0);
    if FieldInfo( FindField( GIS_SRC_Y ) ) = nil then
      self.AddField(GIS_SRC_Y, TGIS_FieldType.Float, 0, 0);

    vList := TGIS_ObjectList.Create( False ) ;
    svList := TGIS_ObjectList.Create( False ) ;
    fList := TGIS_ObjectList.Create( False ) ;
    polygon := nil ;
    edSentinel := 0 ;

    dfmax :=  Abs(T_Triangulation(trObj).voronoiExtent.XMax) ;
    if dfmax <  Abs(T_Triangulation(trObj).voronoiExtent.YMax) then
      dfmax := Abs(T_Triangulation(trObj).voronoiExtent.YMax) ;

    df := RoundS(1/TRI_MIN_DIST) ;
    while dfmax > 0.1 do begin
      dfmax := dfmax / 10 ;
      df := df div 10 ;
    end ;

    check_edges ;

    while(True) do begin
      if not assigned(polygon) then begin
        polygon := TGIS_ShapePolygon.Create(
             nil, nil, False, -1, self,  T_Triangulation(trObj).dimension
                                           ) ;
        polygon.Lock(TGIS_Lock.Extent);
        polygon.AddPart ;

        if T_Triangulation(trObj).edgesLst.Count = 0 then begin
          idx := T_VoroSite(T_Triangulation(trObj).sitesArray[0]).src_id ;
          ptb := T_Triangulation(trObj).baseLayer.GetShape(idx).GetPoint3D(0,0) ;
          polygon.CopyFields(T_Triangulation(trObj).baseLayer.GetShape( idx ));
          polygon.SetField( GIS_SRCID, Variant(idx) );
          polygon.SetField( GIS_SRC_X, ptb.X );
          polygon.SetField( GIS_SRC_Y, ptb.Y );

          ptb.X := T_Triangulation(trObj).voronoiExtent.XMax ;
          ptb.Y := T_Triangulation(trObj).voronoiExtent.YMax ;
          polygon.AddPoint3D(ptb);

          ptb.Y := T_Triangulation(trObj).voronoiExtent.YMin ;
          polygon.AddPoint3D(ptb);

          ptb.X := T_Triangulation(trObj).voronoiExtent.XMin ;
          polygon.AddPoint3D(ptb);

          ptb.Y := T_Triangulation(trObj).voronoiExtent.YMax ;
          polygon.AddPoint3D(ptb);

          polygon.Unlock ;
          polygon.AddToLayer ;
          break ;
        end ;
      end ;

      e := find_first_edge ;
      if e = nil then
        break ;
      ef := e ;

      if e.runs = 1 then begin
        backward := False ;

        if e.IsOnLeft(e.reg[TRI_LEFT].coord) then begin
          cs := e.reg[TRI_RIGHT] ;
          e.rused := True ;
        end
        else begin
          cs := e.reg[TRI_LEFT] ;
          e.rused := False ;
        end ;
      end
      else begin
        if e.rused then begin
          cs := e.reg[TRI_LEFT] ;
        end
        else begin
          cs := e.reg[TRI_RIGHT]
        end ;
        if e.IsOnLeft(cs.coord) then
          backward := True
        else
          backward := False ;

      end ;

      if backward = True then begin
        vf :=  e.ep[1] ;
        fromext := e.endcut[1] ;
        v  :=  e.ep[0] ;
      end
      else begin
        vf :=  e.ep[0] ;
        fromext := e.endcut[0] ;
        v  :=  e.ep[1] ;
      end ;
      vList.Add(e) ;
      fbackward := backward ;
      lbackward := backward ;

      vl := v ;
      if vl <> nil then begin
        while True do begin
          e := find_next_edge ;
          if e = nil then
            break ;
          vList.Add(e) ;
          set_vl ;
          if vl = nil then begin
            fromext := True ;
            break ;
          end ;
          if vl = vf then begin
            break ;
          end ;
        end ;
      end ;

      if vl = vf then begin
        move_to_polygon(vList, fbackward) ;
      end
      else begin
        vl := vf ;
        e := ef ;

        backward := not fbackward ;

        while True do begin
          e := find_next_edge ;
          if e = nil then
            break ;
          lbackward := backward ;
          svList.Add(e) ;
          set_vl ;
          if vl = nil then
            break ;
        end ;

        for k := svList.Count -1 downto 0 do
          fList.Add(svList.Items[k]) ;
        for k := 0 to vList.Count -1 do
          fList.Add(vList.Items[k]) ;

        if svList.Count > 0 then
          backward := not lbackward
        else
          backward := fbackward ;
        move_to_polygon(fList, backward) ;

        fromext := True ;

        svList.Clear ;
        fList.Clear ;
      end ;

      if fromext then begin
        x_min := T_Triangulation(trObj).voronoiExtent.XMin ;
        x_max := T_Triangulation(trObj).voronoiExtent.XMax ;
        y_min := T_Triangulation(trObj).voronoiExtent.YMin ;
        y_max := T_Triangulation(trObj).voronoiExtent.YMax ;
        // Adding points from extent - if necessary
        while ((ptb.X <> pte.X) or ((ptb.X <> x_min) and (ptb.X <> x_max)))
                                 and
              ((ptb.Y <> pte.Y)  or ((ptb.Y <> y_min) and (ptb.Y <> y_max)))do
        begin
          if pte.X = x_min then begin
            if pte.Y <> y_max then
              pte.Y := y_max
            else
              pte.X := x_max ;
          end
          else
          if pte.X = x_max then begin
            if pte.Y <> y_min then
              pte.Y := y_min
            else
              pte.X := x_min ;
          end
          else
          if pte.Y = y_min then begin
            if pte.X <> x_min then
              pte.X := x_min
            else
              pte.Y := y_max ;
          end
          else
          if pte.Y = y_max then begin
            if pte.X <> x_max then
              pte.X := x_max
            else
              pte.Y := y_min ;
          end ;
            polygon.AddPoint3D(pte) ;
        end ;
      end ;
      vList.Clear ;

      polygon.CopyFields(T_Triangulation(trObj).baseLayer.GetShape( cs.src_id ));
      polygon.SetField( GIS_SRCID, cs.src_id  );
      polygon.SetField( GIS_SRC_X, cs.coord.X );
      polygon.SetField( GIS_SRC_Y, cs.coord.Y );
      polygon.Unlock;
      polygon.AddToLayer ;

      polygon.Reset ;
      polygon.Lock(TGIS_Lock.Extent);
      polygon.AddPart ;
    end ;
    FreeObject( polygon ) ;
    FreeObject( fList   ) ;
    FreeObject( svList  ) ;
    FreeObject( vList   ) ;

  end ;


//==============================================================================
// TGIS_LayerDelaunay
//==============================================================================

  constructor TGIS_LayerDelaunay.Create ;
  begin
    inherited ;
    trObj := T_Triangulation.Create ;
    T_Triangulation(trObj).dimension := TGIS_DimensionType.Unknown ;
  end ;

  procedure TGIS_LayerDelaunay.doDestroy ;
  var
    i : Integer ;
    tobj : T_Triangulation ;
  begin
    tobj := T_Triangulation(trObj) ;
    if assigned(tobj.triplesLst) then begin
      {$IFNDEF NEXTGEN}
        for i := tobj.triplesLst.Count -1 downto 0 do
          FreeObjectNotNil( T_Triple(tobj.triplesLst.Items[i]) ) ;
      {$ENDIF}
      FreeObject( tobj.triplesLst ) ;
    end;
    FreeObject( trObj ) ;
    inherited ;
  end ;

  procedure TGIS_LayerDelaunay.ImportLayerEx(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent;
    const _type        : TGIS_ShapeType ;
    const _scope       : String ;
    const _shape       : TGIS_Shape ;
    const _de9im       : String ;
    const _truncated   : Boolean
  ) ;
  var
    p : TGIS_ShapePolygon ;
    i : Integer ;
    tobj : T_Triangulation ;
  const
    GIS_SRCID_A = 'GIS_SRCID_A' ;
    GIS_SRCID_B = 'GIS_SRCID_B' ;
    GIS_SRCID_C = 'GIS_SRCID_C' ;

  begin
    tobj := T_Triangulation(trObj) ;
    tobj.triangulate := True ;

    tobj.triplesLst := TGIS_ObjectList.Create( False ) ;

    tobj.prepareDataForImport(
      _layer, _extent, _type, _scope, _shape, _de9im, _truncated
    ) ;

    if tobj.triplesLst.Count = 0 then begin
      FreeObject( tobj.triplesLst ) ;
      tobj.triplesLst := nil ;
      exit ;
    end;

    tobj.freeData ;

    self.AddField(GIS_SRCID_A, TGIS_FieldType.Number, 10, 0);
    self.AddField(GIS_SRCID_B, TGIS_FieldType.Number, 10, 0);
    self.AddField(GIS_SRCID_C, TGIS_FieldType.Number, 10, 0);
    p := TGIS_ShapePolygon.Create(
          nil, nil, False, 0, self, T_Triangulation(trObj).dimension
                                 ) ;
    for i := tobj.triplesLst.Count -1 downto 0 do begin

      p.Reset ;
      p.Lock(TGIS_Lock.Extent);
      p.AddPart ;
      p.AddPoint3D(T_Triple(tobj.triplesLst.Items[i]).t);
      p.AddPoint3D(T_Triple(tobj.triplesLst.Items[i]).r);
      p.AddPoint3D(T_Triple(tobj.triplesLst.Items[i]).b);

      p.SetField(GIS_SRCID_A, T_Triple(tobj.triplesLst.Items[i]).t_src_id);
      p.SetField(GIS_SRCID_B, T_Triple(tobj.triplesLst.Items[i]).r_src_id);
      p.SetField(GIS_SRCID_C, T_Triple(tobj.triplesLst.Items[i]).b_src_id);

      {$IFNDEF NEXTGEN}
        FreeObjectNotNil( T_Triple(tobj.triplesLst.Items[i]) ) ;
      {$ENDIF}
      p.Unlock ;
      p.AddToLayer ;

    end ;
    FreeObject( p ) ;
    FreeObject( tobj.triplesLst ) ;
    tobj.triplesLst := nil ;
    RecalcExtent ;
  end;

  procedure TGIS_LayerDelaunay.PrepareAltitudeData(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent;
    const _type        : TGIS_ShapeType ;
    const _scope       : String ;
    const _truncated   : Boolean
  ) ;
  begin
    PrepareAltitudeData( _layer, _extent, _type, _scope, nil, '', _truncated ) ;
  end;

  procedure TGIS_LayerDelaunay.PrepareAltitudeData(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent;
    const _type        : TGIS_ShapeType ;
    const _scope       : String ;
    const _shape       : TGIS_Shape ;
    const _de9im       : String ;
    const _truncated   : Boolean
  ) ;
  var
    i : Integer ;
    tobj : T_Triangulation ;
  begin
    if not (_layer is TGIS_LayerVector)  then
      exit ;

    tobj := T_Triangulation(trObj) ;
    tobj.triangulate := True ;

    tobj.triplesLst := TGIS_ObjectList.Create( False ) ;
    tobj.prepareDataForImport(
      _layer, _extent, _type, _scope, _shape, _de9im, _truncated
    ) ;

    if not assigned( tobj.triplesLst ) then
      exit ;
    tobj.freeData ;
    {$IFNDEF OXYGENE}
      tobj.triplesLst.Sort(  TriplesCompare ) ;
    {$ELSE}
      tobj.triplesLst.Sort( @TriplesCompare ) ;
    {$ENDIF}

    FExtent.XMin :=  GIS_MAX_SINGLE ;
    FExtent.XMax := -GIS_MAX_SINGLE ;
    FExtent.YMin :=  GIS_MAX_SINGLE ;
    FExtent.YMax := -GIS_MAX_SINGLE ;

    for i := 0 to tobj.triplesLst.Count - 1 do begin
      if T_Triple(tobj.triplesLst[i]).t.Y > FExtent.YMax then
        FExtent.YMax := T_Triple(tobj.triplesLst[i]).t.Y ;

      if T_Triple(tobj.triplesLst[i]).t.X > FExtent.XMax then
        FExtent.XMax := T_Triple(tobj.triplesLst[i]).t.X
      else
      if T_Triple(tobj.triplesLst[i]).t.X < FExtent.XMin then
        FExtent.XMin := T_Triple(tobj.triplesLst[i]).t.X ;

      if T_Triple(tobj.triplesLst[i]).b.Y < FExtent.YMin then
        FExtent.YMin := T_Triple(tobj.triplesLst[i]).b.Y ;

      if T_Triple(tobj.triplesLst[i]).b.X > FExtent.XMax then
        FExtent.XMax := T_Triple(tobj.triplesLst[i]).b.X
      else
      if T_Triple(tobj.triplesLst[i]).b.X < FExtent.XMin then
        FExtent.XMin := T_Triple(tobj.triplesLst[i]).b.X ;

      if T_Triple(tobj.triplesLst[i]).r.Y < FExtent.YMin then
        FExtent.YMin := T_Triple(tobj.triplesLst[i]).r.Y ;

      if T_Triple(tobj.triplesLst[i]).r.X > FExtent.XMax then
        FExtent.XMax := T_Triple(tobj.triplesLst[i]).r.X
      else
      if T_Triple(tobj.triplesLst[i]).r.X < FExtent.XMin then
        FExtent.XMin := T_Triple(tobj.triplesLst[i]).r.X ;

    end;
  end;

  function TGIS_LayerDelaunay.PointAltitude( const _pt : TGIS_Point) : Single ;
  var
    ma, a, mb, b, c : Double ;
    pt : Array [0..3] of TGIS_Point3D ;
    shp : TGIS_Shape ;
    a2 : Double ;
    k : Integer ;
    tobj : T_Triangulation ;
    ls, start, up, down  : Integer ;
    t : T_Triple ;
    minx, maxx : Double ;
    tstext : TGIS_Extent ;
    w, h : Double ;
    function calc_altitude( var _r : Single ) : Boolean ;
    var
      n : Integer ;
    begin
      Result := True ;
      for n := 0 to 2 do begin //point must by on right from every edge
        a2 := (pt[n +1].X -pt[n].X) * (_pt.Y -pt[n].Y) -
              (_pt.X -pt[n].X) * (pt[n +1].Y -pt[n].Y) ;

        if a2 = 0 then
          break ;
        if a2 > 0 then begin
          Result := False ;
          break ;
        end;
      end;
      if Result then begin //Approximation

        ma := (pt[1].X -pt[0].X) ;
        if ma <> 0 then begin

          mb := ((pt[2].Y -pt[1].Y)*(pt[1].X -pt[0].X) -
                (pt[1].Y -pt[0].Y)*(pt[2].X -pt[1].X)) ;
          b := ((pt[2].Z -pt[1].Z)*(pt[1].X -pt[0].X) -
                (pt[1].Z -pt[0].Z)*(pt[2].X -pt[1].X)) / mb ;
          a := ((pt[1].Z -pt[0].Z) -b*(pt[1].Y -pt[0].Y)) / ma ;
          c := pt[0].Z -a*pt[0].X -b*pt[0].Y ;
        end
        else begin
          ma := (pt[2].X -pt[1].X) ;

          mb := ((pt[3].Y -pt[2].Y)*(pt[2].X -pt[1].X) -
                (pt[2].Y -pt[1].Y)*(pt[3].X -pt[2].X)) ;
          b := ((pt[3].Z -pt[2].Z)*(pt[2].X -pt[1].X) -
                (pt[2].Z -pt[1].Z)*(pt[3].X -pt[2].X)) / mb ;
          a := ((pt[2].Z -pt[1].Z) -b*(pt[2].Y -pt[1].Y)) / ma ;
          c := pt[1].Z -a*pt[1].X -b*pt[1].Y ;
        end;

        _r := a*_pt.X +b*_pt.Y +c ;
      end;
    end;
  begin
    Result := GIS_GRID_NOVALUE ;
    if (T_Triangulation(trObj).dimension <> TGIS_DimensionType.XYZ) and
       (T_Triangulation(trObj).dimension <> TGIS_DimensionType.XYZM)
    then
      exit ;

    tobj := T_Triangulation(trObj) ;
    if assigned(tobj.triplesLst) then begin

      if ( _pt.X < Extent.XMin ) or
         ( _pt.X > Extent.XMax ) or
         ( _pt.Y < Extent.YMin ) or
         ( _pt.Y > Extent.YMax )
      then
        exit ;

      up := tobj.triplesLst.Count ;
      start := up div 2 ;
      down := 0 ;
      ls := 0 ;

      while True do begin
        t := T_Triple(tobj.triplesLst.Items[start]) ;
        if _pt.Y > t.t.Y   then begin
          down := start ;
          if ls >= start then
            break ;
          ls := start ;
          start := down +((up -down) div 2) ;
          continue ;
        end ;
        up := start ;
        start := down +((up -down) div 2) ;
        if ls >= start then
          break ;
      end;

      for k := start to tobj.triplesLst.Count - 1 do begin
        t := T_Triple(tobj.triplesLst.Items[k]) ;

        if (_pt.Y > t.t.Y ) then
          continue ;

        if (_pt.Y < t.b.Y ) and (_pt.Y < t.r.Y ) then
          continue ;
        if t.b.X < t.t.X then begin
          minx := t.b.X ;
          maxx := t.t.X ;
        end
        else begin
          minx := t.t.X ;
          maxx := t.b.X ;
        end;
        if minx > t.r.X then begin
          minx := t.r.X ;
        end
        else
        if maxx < t.r.X then begin
          maxx := t.r.X ;
        end;
        if (_pt.X < minx ) or (_pt.X > maxx ) then
          continue ;

        pt[0] := _TGIS_Point3D(t.t) ;
        pt[1] := _TGIS_Point3D(t.r) ;
        pt[2] := _TGIS_Point3D(t.b) ;
        pt[3] := _TGIS_Point3D(pt[0]) ;

        if calc_altitude(Result) then
          exit ;
      end;
    end;

    if not assigned( Items ) then
      exit ;
    if Items.Count = 0 then
      exit ;
    if ( _pt.X < Extent.XMin ) or
       ( _pt.X > Extent.XMax ) or
       ( _pt.Y < Extent.YMin ) or
       ( _pt.Y > Extent.YMax )
    then
      exit ;

    w := (Extent.XMax -Extent.XMin)/Items.Count ;
    h := (Extent.YMax -Extent.YMin)/Items.Count ;
    tstext.XMin := _pt.X -w ;
    tstext.XMax := _pt.X +w ;
    tstext.YMin := _pt.Y -h ;
    tstext.YMax := _pt.Y +h ;
    shp := FindFirst(tstext) ;
    while shp <> nil do begin
      if (_pt.X <= shp.Extent.XMax) and
         (_pt.X >= shp.Extent.XMin) and
         (_pt.Y <= shp.Extent.YMax) and
         (_pt.Y >= shp.Extent.YMin) then
      begin
        pt[0] := shp.GetPoint3D(0, 0) ;
        pt[1] := shp.GetPoint3D(0, 1) ;
        pt[2] := shp.GetPoint3D(0, 2) ;
        pt[3] := _TGIS_Point3D(pt[0]) ;

        if calc_altitude(Result) then
          exit ;
      end;
      shp := FindNext ;
    end;
  end;

//==================================== END =====================================
end.
