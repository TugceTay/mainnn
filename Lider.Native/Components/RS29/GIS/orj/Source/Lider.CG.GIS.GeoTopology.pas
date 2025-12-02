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
  Encapsulation of basic topological operations.
}
{$IFDEF DCC}
  unit GisTopology ;
  {$HPPEMIT '#pragma link "GisTopology"'}
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
  {$DEFINE GIS_NOASM}
  uses
    System.Text,
    System.Collections,
    System.Runtime.InteropServices,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  {$IFDEF CPUARM}
    {$DEFINE GIS_NOASM}
  {$ENDIF}
  uses
    System.Classes,
    System.Types,
    System.Math,
    System.SysUtils,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisRtl,
    GisTypes,
    GisClasses,
    GisLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  {$DEFINE GIS_NOASM}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

const

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_EQUALITY       = 'T*F**FFF'  ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_DISJOINT       = 'FF*FF'     ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_INTERSECT      = 'T'         ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_INTERSECT1     = '*T'        ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_INTERSECT2     = '***T'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_INTERSECT3     = '****T'     ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_TOUCH_INTERIOR = 'F**T'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_TOUCH          = 'F***T'     ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_CROSS          = 'T*T'       ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_CROSS_LINE     = '0'         ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_WITHIN                         = 'T*F**F'    ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_WITHIN_COMPLETELY              = 'TFF*FF' ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_CONTAINS                       = 'T*****FF' ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_CONTAINS_COMPLETELY            = 'T**FF*FF'  ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_OVERLAP                        = 'T*T***T'   ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_OVERLAP_LINE                   = '1*T***T'   ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_LINE_CROSS_POLYGON             = 'T*T'     ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_POLYGON_CROSSED_BY_LINE        = 'T*****T' ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_LINE_CROSS_LINE                = '0'       ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_LINE_TRAVERS_POLYGON           = 'T**F'    ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_POLYGON_TRAVERSED_BY_LINE      = 'TF'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_LINE_CROSSTRAVERS_POLYGON      = 'T*TFF'   ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_POLYGON_CROSSTRAVERSED_BY_LINE = 'TF**F*T' ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_INTERSECT_INTERIOR_INTERIOR    = 'T'       ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_INTERSECT_INTERIOR_BOUNDARY    = '*T'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_INTERSECT_BOUNDARY_INTERIOR    = '***T'    ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_INTERSECT_BOUNDARY_BOUNDARY    = '****T'   ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_TOUCH_BOUNDARY_INTERIOR        = 'F**T'    ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_TOUCH_INTERIOR_BOUNDARY        = 'FT'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    RELATE_TOUCH_BOUNDARY_BOUNDARY        = 'F***T'   ;
    ///<summary>
    ///   predefined contour open setting
    ///</summary>
    CONTOUR_OPENED = True  ;
    ///<summary>
    ///   predefined contour closed setting
    ///</summary>
    CONTOUR_CLOSED = False ;

type
  /// <summary>
  ///   Types of section conections for offset line.
  /// </summary>
  TGIS_JoinType = {$IFDEF OXYGENE} public {$ENDIF}(
    /// <summary>
    ///   Connection is part of a circle.
    /// </summary>
    Round,
    /// <summary>
    ///   Connection maintains a shape based on a trapezoid.
    /// </summary>
    Trapeze,
    /// <summary>
    ///   Connection maintains ntersecting sections.
    /// </summary>
    Miter,
    /// <summary>
    ///   Connection like a rectangle with a cut corner.
    /// </summary>
    Bevel
  );

  /// <summary>
  ///   Encapsulation of basic topological operations.
  /// </summary>
  TGIS_Topology = {$IFDEF OXYGENE} public {$ENDIF} class  ( TGIS_ObjectDisposable )

      private // properties event
        FOnBusy : TGIS_BusyEvent ;

    // properties internal value
    {$IFNDEF OXYGENE} private {$ELSE} private {$ENDIF}
      FTolerance  : Double ;
      FForceShapeFixing : Boolean ;

    protected // properties access routine

      procedure fset_Tolerance ( const _tolerance : Double ) ;

    // other internal values
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      contour1           : TObject    ;
      contour2           : TObject    ;
      pshape1            : TGIS_Shape ;
      pshape2            : TGIS_Shape ;
      pshapePrepared     : TGIS_Shape ;
      smatrixPrepared    : String     ;
      smatrixPreparedInv : String     ;
      pcontourPrepared   : TObject    ;
      commonContour      : TObject    ;
      combineType        : TGIS_TopologyCombineType ;
      vdescriptors       : TList<TObject>      ;
      listOfVdescriptors : TList<TObject>      ;
      listOfEdges        : TList<TObject>      ;
      interTolerance     : Double     ;
      gridStep           : Double     ;
      gridStep09         : Double     ;
      crossingsNo        : Integer    ;
      fullProgress       : Integer    ;
      inProgress         : Integer    ;
      busyStep           : Integer    ;
      busyUsed           : Boolean    ;
      doBreak            : Boolean    ;
      lockedProjection   : Boolean    ;
      forceFix           : Boolean    ;
      checkCombine       : Boolean    ;

    // other internal routines
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Add a shape to an existing one. Shape will be added as a next part
      ///   to a destination shape. Destination shape cannot be TGIS_ShapePoint.
      /// </summary>
      /// <param name="_dest">
      ///   destination shape (cannot be TGIS_ShapePoint)
      /// </param>
      /// <param name="_source">
      ///   source shape
      /// </param>
      procedure addShape             ( const _dest       : TGIS_Shape          ;
                                       const _source     : TGIS_Shape
                                     ) ; overload;

      /// <summary>
      ///   Add a shape to an existing one. Shape will be added as a next part
      ///   to a destination shape. Destination shape cannot be TGIS_ShapePoint.
      /// </summary>
      /// <param name="_dest">
      ///   destination shape (cannot be TGIS_ShapePoint)
      /// </param>
      /// <param name="_source">
      ///   source shape
      /// </param>
      /// <param name="_reverse">
      ///   If true, then the source shape will be added with a changed, reversed winding
      /// </param>
      procedure addShape             ( const _dest       : TGIS_Shape          ;
                                       const _source     : TGIS_Shape          ;
                                       const _reverse    : Boolean
                                     ) ; overload;

      /// <summary>
      ///   Generate very small polygon (from first point given shape) or tight
      ///   (from first and second point). Used in relate functions.
      /// </summary>
      /// <param name="_s">
      ///   given shape
      /// </param>
      /// <returns>
      ///   TGIS_ShapePolygon
      /// </returns>
      function  makeDPoly            ( const _s : TGIS_Shape
                                     ) : TGIS_ShapePolygon ;

      /// <summary>
      ///   Generate very small arc (from first point given shape). Used in
      ///   relate functions.
      /// </summary>
      /// <param name="_s">
      ///   given shape
      /// </param>
      /// <returns>
      ///   TGIS_ShapePolygon
      /// </returns>
      function  makeDArc             ( const _s : TGIS_Shape
                                     ) : TGIS_ShapeArc ;

      /// <summary>
      ///   Check intersection matrix. Used in relate functions.
      /// </summary>
      /// <param name="_de9im">
      ///   intersection matrix
      /// </param>
      /// <returns>
      ///   returns true if first interior was not used
      /// </returns>
      function  firstInteriorNotUsed ( var   _de9im      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Check intersection matrix. Used in relate functions.
      /// </summary>
      /// <param name="_de9im">
      ///   intersection matrix
      /// </param>
      /// <returns>
      ///   returns true if second interior was not used
      /// </returns>
      function  secondInteriorNotUsed( var   _de9im      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Compute a processor tolerance based on the Extent of operations.
      ///   Tolerance is the maximum extent divided by 10e13 ;
      /// </summary>
      /// <returns>
      ///   returns computed tolerance
      /// </returns>
      function  computeTolerance     : Double ;

      /// <summary>
      ///   Return a reference to a list of descriptors of crossing for a given
      ///   vertex. If such a list doesn't exist then, a new one will be
      ///   created.
      /// </summary>
      /// <param name="_vertex">
      ///   vertex to be checked
      /// </param>
      /// <returns>
      ///   TList of TObjects with descriptors of crossing for given vertex.
      /// </returns>
      function  getVdescriptors      ( const _vertex     : TObject
                                     ) : TList<TObject> ;

      /// <summary>
      ///   Find crossings. Insert in such places a new vertex or snapping
      ///   vertices within tolerance.
      /// </summary>
      procedure fixCrossVertices     ;

      /// <summary>
      ///   Test if the edge belongs to the region and defines a direction of
      ///   movement on the list of edges.
      /// </summary>
      /// <param name="_edge">
      ///   edge to be tested
      /// </param>
      /// <param name="_direction">
      ///   computed direction
      /// </param>
      /// <returns>
      ///   true if edge belongs to the region
      /// </returns>
      function  edgeRule             ( const _edge       : TObject             ;
                                       var   _direction  : Integer
                                     ) : Boolean ;

      /// <summary>
      ///   Add to all common points descriptors.
      /// </summary>
      /// <param name="_contour">
      ///   scope of operations
      /// </param>
      procedure addCrossVrtxDesc     ( const _contour    : TObject
                                     ) ;

      /// <summary>
      ///   Try to create polygon contour part starting from a given vertex in
      ///   a given direction
      /// </summary>
      /// <param name="_vertex">
      ///   start position
      /// </param>
      /// <param name="_direction">
      ///   direction of traversing start
      /// </param>
      /// <param name="_cpart">
      ///   working contour part
      /// </param>
      procedure collectPolygonCPart  ( const _vertex     : TObject             ;
                                       const _direction  : Integer             ;
                                       const _cpart      : TObject
                                     ) ;

      /// <summary>
      ///   Prepare a traverse starting from a given vertex in a given direction
      /// </summary>
      /// <param name="_vertex">
      ///   start position; as a result, an updated position
      /// </param>
      /// <param name="_direction">
      ///   start direction of traversing; as a result, an updated direction
      /// </param>
      /// <returns>
      ///   returns true if done preparing
      /// </returns>
      function  jumpAccordingToDesc  ( var   _vertex     : TObject             ;
                                       var   _direction  : Integer
                                     ) : Boolean ;

      /// <summary>
      ///   Compute angles of edges at crossing (or common) points
      /// </summary>
      /// <param name="_vdescriptors">
      ///   list of descriptors for a selected point
      /// </param>
      procedure computeEdgesAngles   ( const _vdescriptors : TList<TObject>
                                     ) ;

      /// <summary>
      ///   Create an edge list and compute the location of each related to the
      ///   _nextcontour.
      /// </summary>
      /// <param name="_contour">
      ///   contour to be tested
      /// </param>
      /// <param name="_nextcontour">
      ///   related contour
      /// </param>
      procedure createEdgesAndDfnSite( const _contour    : TObject             ;
                                       const _nextcontour: TObject
                                     ) ;

      /// <summary>
      ///   Free descriptors list.
      /// </summary>
      /// <param name="_vdescriptors">
      ///   descriptors list
      /// </param>
      procedure freeDescriptors      ( const _vdescriptors : TList<TObject>
                                     ) ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a given
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   first polygon
      /// </param>
      /// <param name="_shpB">
      ///   second polygon
      /// </param>
      /// <param name="_operation">
      ///   operation code; what operation should be perform between (_shpA)
      ///   and (_shpB)
      /// </param>
      /// <returns>
      ///   returns combined shape
      /// </returns>
      function  combinePolygons      ( const _shpA       : TGIS_ShapePolygon   ;
                                       const _shpB       : TGIS_ShapePolygon   ;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TGIS_Shape ;

      /// <summary>
      ///   Compute a new polygon contour from two provided polygon contours
      ///   based on a given operation.
      /// </summary>
      /// <param name="_contourA">
      ///   first polygon contour
      /// </param>
      /// <param name="_contourB">
      ///   second polygon contour
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <returns>
      ///   returns combined contour out of two given polygon contours
      /// </returns>
      function  combineCPolygons     ( const _contourA   : TObject ;
                                       const _contourB   : TObject ;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TObject ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a given
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   arc
      /// </param>
      /// <param name="_shpB">
      ///   polygon
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <returns>
      ///   returns combined shape from arc and polygon
      /// </returns>
      function  combineArcPolygon    ( const _shpA       : TGIS_ShapeArc       ;
                                       const _shpB       : TGIS_ShapePolygon   ;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TGIS_Shape ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a given
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   first arc
      /// </param>
      /// <param name="_shpB">
      ///   second arc
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <returns>
      ///   returns combined shape out of two arc shapes
      /// </returns>
      function  combineArcs          ( const _shpA       : TGIS_ShapeArc       ;
                                       const _shpB       : TGIS_ShapeArc       ;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TGIS_Shape ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a given
      ///   operation.
      /// </summary>
      /// <param name="_contourA">
      ///   first arc
      /// </param>
      /// <param name="_contourB">
      ///   second arc
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <returns>
      ///   returns new shape created out of two provided based on given operation
      /// </returns>
      function  combineCArcs         ( const _contourA   : TObject ;
                                       const _contourB   : TObject ;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TObject ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a given
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   point or multipoint
      /// </param>
      /// <param name="_shpB">
      ///   polygon
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <returns>
      ///   returns new shape combined out of point and polygon based on given operation
      /// </returns>
      function  combinePointPolygon  ( const _shpA       : TGIS_Shape          ;
                                       const _shpB       : TGIS_ShapePolygon   ;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TGIS_Shape ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a given
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   point or multipoint
      /// </param>
      /// <param name="_shpB">
      ///   arc
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <returns>
      ///   returns combined shape out of point and arc based on given operation
      /// </returns>
      function  combinePointArc      ( const _shpA       : TGIS_ShapePoint     ;
                                       const _shpB       : TGIS_ShapeArc       ;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TGIS_Shape ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a given
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   multipoint
      /// </param>
      /// <param name="_shpB">
      ///   multipoint
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <returns>
      ///   returns combined shape out of two multipoint shapes based on given operation
      /// </returns>
      function  combineMultiPoints   ( const _shpA       : TGIS_ShapeMultiPoint;
                                       const _shpB       : TGIS_ShapeMultiPoint;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TGIS_Shape ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a given
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   point
      /// </param>
      /// <param name="_shpB">
      ///   multipoint
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <returns>
      ///   returns combined shape out of point and multipoint shape based on given operation
      /// </returns>
      function  combinePointMPoint   ( const _shpA       : TGIS_ShapePoint     ;
                                       const _shpB       : TGIS_ShapeMultiPoint;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TGIS_Shape ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a difference
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   multipoint
      /// </param>
      /// <param name="_shpB">
      ///   point
      /// </param>
      /// <returns>
      ///   returns computed difference shape out of point and multipoint
      /// </returns>
      function differenceMPointPoint ( const _shpA       : TGIS_ShapeMultiPoint ;
                                       const _shpB       : TGIS_ShapePoint
                                     ) : TGIS_Shape ;

      /// <summary>
      ///   Compute a new shape from two provided shapes based on a given
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   point
      /// </param>
      /// <param name="_shpB">
      ///   point
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <returns>
      ///   returns combined shape out of two points and given operation
      /// </returns>
      function  combinePoints        ( const _shpA       : TGIS_ShapePoint ;
                                       const _shpB       : TGIS_ShapePoint ;
                                       const _operation  : TGIS_TopologyCombineType
                                     ) : TGIS_Shape ;

      /// <summary>
      ///   Checks nine-intersection matrix for given polygons
      /// </summary>
      /// <param name="_shpA">
      ///   first polygon
      /// </param>
      /// <param name="_shpB">
      ///   second polygon
      /// </param>
      /// <param name="_de9im">
      ///   intersection matrix
      /// </param>
      /// <returns>
      ///   returns true if related
      /// </returns>
      function  relatePolygons       ( const _shpA       : TGIS_ShapePolygon   ;
                                       const _shpB       : TGIS_ShapePolygon   ;
                                       const _de9im      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Checks nine-intersection matrix for given arc and polygon.
      /// </summary>
      /// <param name="_shpA">
      ///   given arc
      /// </param>
      /// <param name="_shpB">
      ///   given polygon
      /// </param>
      /// <param name="_de9im">
      ///   intersection matrix
      /// </param>
      /// <returns>
      ///    returns true if related
      /// </returns>
      function  relateArcPolygon     ( const _shpA       : TGIS_ShapeArc       ;
                                       const _shpB       : TGIS_ShapePolygon   ;
                                       const _de9im      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Checks nine-intersection matrix for given arc and polygon
      /// </summary>
      /// <param name="_shpA">
      ///   given arc
      /// </param>
      /// <param name="_shpB">
      ///   second given arc
      /// </param>
      /// <param name="_de9im">
      ///   intersection matrix
      /// </param>
      /// <returns>
      ///    returns true if related
      /// </returns>
      function  relateArcs           ( const _shpA       : TGIS_ShapeArc       ;
                                       const _shpB       : TGIS_ShapeArc       ;
                                       const _de9im      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Checks nine-intersection matrix for given point or multipoint and
      ///   polygon
      /// </summary>
      /// <param name="_shpA">
      ///   given point or multipoint
      /// </param>
      /// <param name="_shpB">
      ///   given polygon
      /// </param>
      /// <param name="_de9im">
      ///   intersection matrix
      /// </param>
      /// <returns>
      ///    returns true if related
      /// </returns>
      function  relatePointPolygon   ( const _shpA       : TGIS_Shape          ;
                                       const _shpB       : TGIS_ShapePolygon   ;
                                       const _de9im      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Checks nine-intersection matrix for given point or multipoint and
      ///   arc
      /// </summary>
      /// <param name="_shpA">
      ///   given point or multipoint
      /// </param>
      /// <param name="_shpB">
      ///   given arc
      /// </param>
      /// <param name="_de9im">
      ///   intersection matrix
      /// </param>
      /// <returns>
      ///    returns true if related
      /// </returns>
      function  relatePointArc       ( const _shpA       : TGIS_Shape          ;
                                       const _shpB       : TGIS_ShapeArc       ;
                                       const _de9im      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Checks nine-intersection matrix for given point or multipoint and
      ///   second point or multipoint
      /// </summary>
      /// <param name="_shpA">
      ///   given point or multipoint
      /// </param>
      /// <param name="_shpB">
      ///   second given point or multipoint
      /// </param>
      /// <param name="_de9im">
      ///   intersection matrix
      /// </param>
      /// <returns>
      ///    returns true if related
      /// </returns>
      function  relatePoints         ( const _shpA       : TGIS_Shape          ;
                                       const _shpB       : TGIS_Shape          ;
                                       const _de9im      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Gets independent copy of given shape
      /// </summary>
      /// <param name="_shp">
      ///   given shape
      /// </param>
      /// <returns>
      ///   returns copy of the given shape
      /// </returns>
      function make_cpy              ( const _shp        : TGIS_Shape
                                     ) : TGIS_Shape ;

    protected

      /// <summary>
      ///   Destroy instance.
      /// </summary>
      procedure doDestroy            ; override;

    public // API

      /// <summary>
      ///   Create instance.
      /// </summary>
      constructor  Create            ;

      /// <summary>
      ///   Find on the list arc with duplicated geometry.
      /// </summary>
      /// <param name="_arc">
      ///   arc to be found
      /// </param>
      /// <param name="_shplist">
      ///   list of arcs
      /// </param>
      /// <param name="_idx">
      ///   found position
      /// </param>
      /// <returns>
      ///   returns true if finds arc with duplicated geometry
      /// </returns>
      function  ArcFind              ( const _arc          : TGIS_ShapeArc     ;
                                       const _shplist      : {$IFDEF CLR}
                                                               IList           ;
                                                             {$ELSE}
                                                               TGIS_ObjectList ;
                                                             {$ENDIF}
                                       var   _idx          : Integer
                                     ) : Boolean ;

      /// <summary>
      ///   Connect the current unconnected shape.
      /// </summary>
      /// <param name="_arc">
      ///   given shape
      /// </param>
      /// <param name="_shplist">
      ///   arc list to which the shape must be connected
      /// </param>
      /// <returns>
      ///   connected shape or nil
      /// </returns>
      /// <remarks>
      ///   Shape will be connected to the "best fit" shape from a given
      ///   list. As a result, a new shape will be created.
      /// </remarks>
      /// <returns>
      ///   returns connected shape
      /// </returns>
      function  ArcMakeConnected     ( const _arc          : TGIS_ShapeArc     ;
                                       const _shplist      : {$IFDEF CLR}
                                                               IList
                                                             {$ELSE}
                                                               TGIS_ObjectList
                                                             {$ENDIF}
                                     ) : TGIS_ShapeArc ;

      /// <summary>
      ///   Return a copy of _shp divided into parts in crossing points with
      ///   shapes from the list. Ensure proper order and orientation of
      ///   sections (last point of previous is first of next one).
      /// </summary>
      /// <param name="_shp">
      ///   shape to divide
      /// </param>
      /// <param name="_shplist">
      ///   a list of Arcs or Polygons
      /// </param>
      /// <returns>
      ///   returns copy of _shp divided into parts in crossing points with
      ///   shapes from the list
      /// </returns>
      function  ArcSplitOnCross      ( const _shp          : TGIS_ShapeArc     ;
                                       const _shplist      : {$IFDEF CLR}
                                                               IList
                                                             {$ELSE}
                                                               TGIS_ObjectList
                                                             {$ENDIF}
                                     ) : TGIS_ShapeArc ;

      /// <summary>
      ///   Check, if the current shape has an exactly similar counter
      ///   partner on a given list.
      /// </summary>
      /// <param name="_list">
      ///   list to be checked
      /// </param>
      /// <param name="_shp">
      ///   shape to be compared
      /// </param>
      /// <returns>
      ///   number of shapes with exactly similar counter partner on given list
      /// </returns>
      function  FindSameOnList       ( const _list         : {$IFDEF CLR}
                                                               IList           ;
                                                             {$ELSE}
                                                               TGIS_ObjectList ;
                                                             {$ENDIF}
                                       const _shp          : TGIS_Shape
                                     ) : Integer ;

      /// <summary>
      ///   Divide an input shape (polygon, arc or multipoint) into a row
      ///   of shapes along a given line.
      /// </summary>
      /// <param name="_shape">
      ///   a shape to divide
      /// </param>
      /// <param name="_arc">
      ///   dividing line
      /// </param>
      /// <returns>
      ///   List of shapes divided by line. List will contain original shape
      ///   if dividing was not possible.
      /// </returns>
      function  SplitByArc           ( const _shape        : TGIS_Shape        ;
                                       const _arc          : TGIS_ShapeArc
                                     ) : {$IFDEF CLR}
                                           IList           ;
                                         {$ELSE}
                                           TGIS_ObjectList ;
                                         {$ENDIF}
                                     overload;

      /// <summary>
      ///   Divide an input shape (polygon, arc or multipoint) into a row of
      ///   shapes along a given line, otherwise return a copy of the shape
      ///   when the line doesn't divide it.
      /// </summary>
      /// <param name="_shape">
      ///   a shape to divide
      /// </param>
      /// <param name="_arc">
      ///   dividing line
      /// </param>
      /// <param name="_fixshape">
      ///   if True, then checks and eventually fixes the input shape for
      ///   common topological problems, like self-crossings, overlapped parts
      ///   etc.
      /// </param>
      /// <returns>
      ///   returns list of shapes divided by arc
      /// </returns>
      function  SplitByArc         ( const _shape        : TGIS_Shape        ;
                                     const _arc          : TGIS_ShapeArc     ;
                                     const _fixshape     : Boolean
                                   ) : {$IFDEF CLR}
                                         IList           ;
                                       {$ELSE}
                                         TGIS_ObjectList ;
                                       {$ENDIF}
                                   overload;

      /// <summary>
      ///   Checks whether a shape is valid.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be checked
      /// </param>
      /// <returns>
      ///   True if a _shp is valid
      /// </returns>
      /// <remarks>
      ///   Function calls FixShapeEx method internally to validate.
      ///   To fix a shape use FixShape or FixShapeEx methods directly.
      /// </remarks>
      function  CheckShape           ( const _shp          : TGIS_Shape
                                     ) : Boolean ;

      /// <summary>
      ///   Verifies if provided part is an island/hole
      /// </summary>
      /// <param name="_shp">
      ///   source polygon,
      /// </param>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_parentPart">
      ///   return parent part number for given part, for main part will be
      ///   -1, for islands/holes &gt;=0
      /// </param>
      /// <returns>
      ///   1 if island (on hole), -1 if hole; 0 if main part (also in multipolygon)
      /// </returns>
      function  PartStatus           ( const _shp          : TGIS_ShapePolygon ;
                                       const _part         : Integer           ;
                                       var   _parentPart   : Integer
                                     ) : Integer ;


      /// <summary>
      ///   Computes a shape without self-crossings (loops) for arcs
      ///   and polygons, and with a proper winding for polygon
      ///   (clockwise, and opposite for holes).
      /// </summary>
      /// <param name="_shp">
      ///   polygon, or arc to be fixed
      /// </param>
      /// <returns>
      ///   returns the computed shape (self)
      /// </returns>
      /// <remarks>
      ///   The result depends on the ForceShapeFixing property.
      ///   For more control, see FixShapeEx method.
      ///   To just validate a shape, use CheckShape method.
      /// </remarks>
      function  FixShape             ( const _shp          : TGIS_Shape
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Computes a shape without self-crossings (loops) for arcs
      ///   and polygons, and with a proper winding for polygon
      ///   (clockwise, and opposite for holes).
      /// </summary>
      /// <param name="_shp">
      ///   polygon, or arc to be fixed
      /// </param>
      /// <param name="_returnnewobj">
      ///   if True, the result is a newly created shape;
      ///   if False, the self object is returned
      /// </param>
      /// <returns>
      ///   returns the computed shape
      /// </returns>
      /// <remarks>
      ///   The result depends on the ForceShapeFixing property.
      ///   For more control, see FixShapeEx method.
      ///   To just validate a shape, use CheckShape method.
      /// </remarks>
      {@ownership:release:result_returnnewobj}
      function  FixShape             ( const _shp          : TGIS_Shape        ;
                                       const _returnnewobj : Boolean
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Computes a shape without self-crossings (loops) for arcs
      ///   and polygons, and with a proper winding for polygon
      ///   (clockwise, and opposite for holes).
      /// </summary>
      /// <param name="_shp">
      ///   polygon, or arc to be fixed
      /// </param>
      /// <param name="_returnnewobj">
      ///   if True, the result is a newly created shape;
      ///   if False, the self object is returned
      /// </param>
      /// <param name="_wasfixed">
      ///   indicates whether the shape was modified to fix errors
      /// </param>
      /// <returns>
      ///   returns the computed shape
      /// </returns>
      /// <remarks>
      ///   The result depends on the ForceShapeFixing property.
      ///   See also FixShape method.
      ///   To just validate a shape, use CheckShape method.
      /// </remarks>
      {#ownership:result:ownif_returnewobject}
      function  FixShapeEx           ( const _shp          : TGIS_Shape        ;
                                       const _returnnewobj : Boolean           ;
                                       var   _wasfixed     : Boolean
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Computes a shape without self-crossings (loops) for arcs
      ///   and polygons, and with a proper winding for polygon
      ///   (clockwise, and opposite for holes).
      /// </summary>
      /// <param name="_shp">
      ///   polygon, or arc to be fixed
      /// </param>
      /// <param name="_returnnewobj">
      ///   if True, the result is a newly created shape;
      ///   if False, the self object is returned
      /// </param>
      /// <param name="_forceFix">
      ///   fix invalid geometry that does not have enough distinct points
      ///   by adding missing points
      /// </param>
      /// <param name="_wasfixed">
      ///   indicates whether the shape was modified to fix errors
      /// </param>
      /// <returns>
      ///   returns the computed shape
      /// </returns>
      /// <remarks>
      ///   See also FixShape method.
      ///   To just validate a shape, use CheckShape method.
      /// </remarks>
      {#ownership:result:ownif_returnewobject}
      function  FixShapeEx             ( const _shp          : TGIS_Shape      ;
                                         const _returnnewobj : Boolean         ;
                                         const _forceFix     : Boolean         ;
                                         var   _wasfixed     : Boolean
                                       ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Make a copy of the shape with removal of redundant (overlapped)
      ///   points.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be copied
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TOPOLOGY_EMPTYAFTERFIX
      /// </exception>
      /// <returns>
      ///   returns copy of shape without redundant points
      /// </returns>
      function  ClearShape             ( const _shp          : TGIS_Shape
                                       ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Make a copy of the shape with removal of redundant (overlapped)
      ///   points.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be copied
      /// </param>
      /// <param name="_returnnewobj">
      ///   if True, then result will be returned in a newly created object; if
      ///   False, then self object will be returned
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TOPOLOGY_EMPTYAFTERFIX
      /// </exception>
      /// <returns>
      ///   returns copy of shape without redundant points
      /// </returns>
      {#ownership:result:ownif_returnewobject}
      function  ClearShape             ( const _shp          : TGIS_Shape        ;
                                         const _returnnewobj : Boolean
                                       ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Delete any loop which is smaller than the tolerance.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be cleared
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Valid only for Arc and Polygon.
      ///    </note>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///   returns shape without loops which are smaller than tolerance
      /// </returns>
      function  FindAndDeleteLoops     ( const _shp          : TGIS_Shape
                                       ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Delete any loop which is smaller than the tolerance.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be cleared
      /// </param>
      /// <param name="_returnnewobj">
      ///   if True, then result will be returned in a newly created object; if
      ///   False, then self object will be returned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Valid only for Arc and Polygon.
      ///    </note>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///   returns shape without loops which are smaller than tolerance
      /// </returns>
      {#ownership:result:ownif_returnewobject}
      function  FindAndDeleteLoops     ( const _shp          : TGIS_Shape ;
                                         const _returnnewobj : Boolean
                                       ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape from the two shapes provided, based on a
      ///   given operation.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///   returns new shape combined out of two others based on given type of operation
      /// </returns>
      function  Combine                ( const _shpA         : TGIS_Shape        ;
                                         const _shpB         : TGIS_Shape        ;
                                         const _operation    : TGIS_TopologyCombineType
                                       ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape from the two shapes provided, based on a given
      ///   operation.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <param name="_operation">
      ///   operation code; what kind of operation should be perform between
      ///   (_shpA) and (_shpB)
      /// </param>
      /// <param name="_fixshape">
      ///   if True, then checks and eventually fixes the input shapes for
      ///   common topological problems, like self-crossings, overlapped parts
      ///   etc.
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///   returns new shape combined out of two others based on given type of operation
      /// </returns>
      function  Combine              ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape        ;
                                       const _operation    : TGIS_TopologyCombineType ;
                                       const _fixshape     : Boolean
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape as an union of two shapes provided.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as union of two others
      /// </returns>
      function  Union                ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape as an union of two shapes provided.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <param name="_fixshape">
      ///   if True, then checks and eventually fixes the input shapes for
      ///   common topological problems, like self-crossings, overlapped parts
      ///   etc.
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as union of two others
      /// </returns>
      function  Union                ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape        ;
                                       const _fixshape     : Boolean
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a union from the list of shapes.
      /// </summary>
      /// <param name="_shpList">
      ///   list of shapes
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as union of shapes from list
      /// </returns>
      function  UnionOnList          ( const _shpList      : TGIS_ShapeList
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a union form the list of shapes.
      /// </summary>
      /// <param name="_shpList">
      ///   list of shapes
      /// </param>
      /// <param name="_fixshape">
      ///   if True, then checks and eventually fixes the input shapes for
      ///   common topological problems, like self-crossings, overlapped parts
      ///   etc.
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as union of shapes from list
      /// </returns>
      function  UnionOnList        ( const _shpList      : TGIS_ShapeList ;
                                     const _fixshape     : Boolean
                                   ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape as an intersection of two shapes provided.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as intersection of two shapes
      /// </returns>
      function  Intersection       ( const _shpA         : TGIS_Shape        ;
                                     const _shpB         : TGIS_Shape
                                   ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape as an intersection of two shapes provided.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <param name="_fixshape">
      ///   if True, then checks and eventually fixes the input shapes for
      ///   common topological problems, like self-crossings, overlapped parts
      ///   etc.
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as intersection of two shapes
      /// </returns>
      function  Intersection         ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape        ;
                                       const _fixshape     : Boolean
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape as a difference of two shapes provided.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as difference of two shapes
      /// </returns>
      function  Difference           ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape as a difference of two shapes provided.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <param name="_fixshape">
      ///   if True, then checks and eventually fixes the input shapes for
      ///   common topological problems, like self-crossings, overlapped parts
      ///   etc.
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as difference of two shapes
      /// </returns>
      function  Difference           ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape        ;
                                       const _fixshape     : Boolean
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape as a symmetrical difference of two shapes
      ///   provided.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as symmetrical difference of two shapes
      /// </returns>
      function  SymmetricalDifference( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a new shape as a symmetrical difference of two shapes
      ///   provided.
      /// </summary>
      /// <param name="_shpA">
      ///   first shape
      /// </param>
      /// <param name="_shpB">
      ///   second shape
      /// </param>
      /// <param name="_fixshape">
      ///   if True, then checks and eventually fixes the input shapes for
      ///   common topological problems, like self-crossings, overlapped parts
      ///   etc.
      /// </param>
      /// <remarks>
      ///   See TGIS_Shape.Combine for example.
      /// </remarks>
      /// <returns>
      ///  returns new shape as symmetrical difference of two shapes
      /// </returns>
      function  SymmetricalDifference( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape        ;
                                       const _fixshape     : Boolean
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Creates a convex hull based on the provided layer.
      /// </summary>
      /// <param name="_layer">
      ///   source layer
      /// </param>
      /// <returns>
      ///   convex hull as a polygon
      /// </returns>
      function  ConvexHull   ( const _layer  : TGIS_LayerVector
                             ) : TGIS_ShapePolygon ; overload;

      /// <summary>
      ///   Creates a convex hull based on the provided shape.
      /// </summary>
      /// <param name="_shape">
      ///   source shape
      /// </param>
      /// <returns>
      ///   convex hull as a polygon
      /// </returns>
      function  ConvexHull   ( const _shape  : TGIS_Shape
                             ) : TGIS_ShapePolygon ; overload;

      /// <summary>
      ///   Creates a convex hull based on the two provided shapes.
      /// </summary>
      /// <param name="_shape1">
      ///   first source shape
      /// </param>
      /// <param name="_shape2">
      ///   second source shape; if nil only _shape1 will be used
      /// </param>
      /// <returns>
      ///   convex hull as a polygon
      /// </returns>
      function  ConvexHull   ( const _shape1 : TGIS_Shape ;
                               const _shape2 : TGIS_Shape
                             ) : TGIS_ShapePolygon ; overload;

      /// <summary>
      ///   Creates a convex hull based on the two provided shapes.
      /// </summary>
      /// <param name="_shape1">
      ///   first source shape
      /// </param>
      /// <param name="_shape2">
      ///   second source shape; if nil only _shape1 will be used
      /// </param>
      /// <param name="_fix">
      ///   if True, then checks and, if necessary, fixes the input shapes for
      ///   common topological errors, like self-crossings, overlapping parts
      ///   etc.
      /// </param>
      /// <returns>
      ///   convex hull as a polygon
      /// </returns>
      function  ConvexHull   ( const _shape1 : TGIS_Shape ;
                               const _shape2 : TGIS_Shape ;
                               const _fix    : Boolean
                             ) : TGIS_ShapePolygon ; overload;

      /// <summary>
      ///   Creates a concave hull based on the provided layer
      ///   and edge length.
      /// </summary>
      /// <param name="_layer">
      ///   source layer
      /// </param>
      /// <param name="_alpha">
      ///   maximum edge length
      /// </param>
      /// <returns>
      ///   concave hull as a polygon
      /// </returns>
      function  ConcaveHull  ( const _layer  : TGIS_LayerVector ;
                               const _alpha  : Double
                             ) : TGIS_ShapePolygon ; overload;

      /// <summary>
      ///   Creates a concave hull based on the provided shape
      ///   and edge length.
      /// </summary>
      /// <param name="_shape">
      ///   source shape
      /// </param>
      /// <param name="_alpha">
      ///   maximum edge length
      /// </param>
      /// <returns>
      ///   concave hull as a polygon
      /// </returns>
      function  ConcaveHull  ( const _shape  : TGIS_Shape ;
                               const _alpha  : Double
                             ) : TGIS_ShapePolygon ; overload;

      /// <summary>
      ///   Creates a concave hull based on the two provided shapes
      ///   and edge length.
      /// </summary>
      /// <param name="_shape1">
      ///   first source shape
      /// </param>
      /// <param name="_shape2">
      ///   second source shape; if nil only _shape1 will be used
      /// </param>
      /// <param name="_alpha">
      ///   maximum edge length
      /// </param>
      /// <returns>
      ///   concave hull as a polygon
      /// </returns>
      function  ConcaveHull  ( const _shape1 : TGIS_Shape ;
                               const _shape2 : TGIS_Shape ;
                               const _alpha  : Double
                             ) : TGIS_ShapePolygon ; overload;

      /// <summary>
      ///   Creates a concave hull based on the two provided shapes
      ///   and edge length.
      /// </summary>
      /// <param name="_shape1">
      ///   first source shape
      /// </param>
      /// <param name="_shape2">
      ///   second source shape; if nil only _shape1 will be used
      /// </param>
      /// <param name="_alpha">
      ///   maximum edge length
      /// </param>
      /// <param name="_fix">
      ///   if True, then checks and, if necessary, fixes the input shapes for
      ///   common topological errors, like self-crossings, overlapping parts
      ///   etc.
      /// </param>
      /// <returns>
      ///   concave hull as a polygon
      /// </returns>
      function  ConcaveHull  ( const _shape1 : TGIS_Shape ;
                               const _shape2 : TGIS_Shape ;
                               const _alpha  : Double     ;
                               const _fix    : Boolean
                             ) : TGIS_ShapePolygon ; overload;

      /// <summary>
      ///   Compute a buffer around a shape to a given distance. Uses 9
      ///   vertices to create a curve.
      /// </summary>
      /// <param name="_shp">
      ///   shape to which a buffer must be constructed
      /// </param>
      /// <param name="_dist">
      ///   distance
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.RevertAll for example.
      /// </remarks>
      /// <returns>
      ///   returns new shape with buffer around given shape within given distance
      /// </returns>
      function  MakeBuffer         ( const _shp          : TGIS_Shape        ;
                                     const _dist         : Double
                                   ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Compute a shifted line for a shape to a given distance.
      /// </summary>
      /// <param name="_shp">
      ///   shape to which a shifted line must be constructed
      /// </param>
      /// <param name="_dist">
      ///   distance  ( &lt; 0 for line on right, &gt; 0 on left)
      /// </param>
      /// <returns>
      ///   returns new shifted shape with given distance
      /// </returns>
      function  MakeOffsetLine    ( const _shp          : TGIS_Shape        ;
                                    const _dist         : Double
                                  ) : TGIS_Shape ; overload;


      /// <summary>
      ///   Compute a shifted line for a shape to a given distance.
      /// </summary>
      /// <param name="_shp">
      ///   shape to which a shifted line must be constructed
      /// </param>
      /// <param name="_dist">
      ///   distance  ( &lt; 0 for line on right, &gt; 0 on left)
      /// </param>
      /// <param name="_jointype">
      ///   way of connecting sections
      /// </param>
      /// <returns>
      ///   returns new shifted shape with given distance
      /// </returns>
      function MakeOffsetLine    ( const _shp          : TGIS_Shape        ;
                                   const _dist         : Double            ;
                                   const _jointype     : TGIS_JoinType
                                 ) : TGIS_Shape ; overload;


      /// <summary>
      ///   Compute a buffer around a shape to a given distance.
      /// </summary>
      /// <param name="_shp">
      ///   shape to which a buffer must be constructed
      /// </param>
      /// <param name="_dist">
      ///   distance
      /// </param>
      /// <param name="_pi2_points">
      ///   number of vertices used to create a curve;
      /// </param>
      /// <param name="_fixshape">
      ///   if True, then checks and eventually fixes the input shapes for
      ///   common topological problems, like self-crossings, overlapped parts
      ///   etc.
      /// </param>
      /// <remarks>
      ///   See TGIS_LayerVector.RevertAll for example.
      /// </remarks>
      /// <returns>
      ///   returns new shape with buffer around given shape within given distance
      /// </returns>
      function  MakeBuffer           ( const _shp          : TGIS_Shape        ;
                                       const _dist         : Double            ;
                                       const _pi2_points   : Integer           ;
                                       const _fixshape     : Boolean
                                     ) : TGIS_Shape ; overload;

      /// <summary>
      ///   Mark given shape as a main shape for subsequent Relate operations.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be optimized
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Used to optimize TGIS_LayerVector.MoveFirst TGIS_LayerVector.MoveNext,
      ///    TGIS_LayerVector.FindFirst and TGIS_LayerVector.FindNext
      ///    </note>
      /// </remarks>
      procedure RelatePrepare        ( const _shp          : TGIS_Shape
                                     ) ;

      /// <summary>
      ///   Checks nine-intersection matrix for given shapes.
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <param name="_de9im">
      ///   intersection matrix
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Relate can be optimized for subsequent call (if _shpA is the same)
      ///    by calling RelatePrepare
      ///    </note>
      ///   See TGIS_Topology for details and TGIS_Shape.Relate for example.
      /// </remarks>
      /// <returns>
      ///   returns true if shapes are related
      /// </returns>
      function  Relate               ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape        ;
                                       const _de9im        : String
                                     ) : Boolean ;

      /// <summary>
      ///   Tests equality relationship for given shapes
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <returns>
      ///   returns true if shapes are equal
      /// </returns>
      function  Equality             ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : Boolean ;

      /// <summary>
      ///   Tests disjoint relationship for given shapes
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <returns>
      ///   returns true if shapes are disjoining
      /// </returns>
      function  Disjoint             ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : Boolean ;

      /// <summary>
      ///   Test intersects relationship for given shapes
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <returns>
      ///   returns true if shapes are intersecting
      /// </returns>
      function  Intersect            ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : Boolean ;

      /// <summary>
      ///   Tests touch relationship for given shapes
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <returns>
      ///   returns true if shapes are touching
      /// </returns>
      function  Touch                ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : Boolean ;

      /// <summary>
      ///   Tests cross relationship for given shapes
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <returns>
      ///   returns true if shapes are crossing
      /// </returns>
      function  Cross                ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : Boolean ;

      /// <summary>
      ///   Tests within relationship for given shapes
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <returns>
      ///   returns true if shape is within
      /// </returns>
      function  Within               ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : Boolean ;

      /// <summary>
      ///   Tests contains relationship for given shapes
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <returns>
      ///   returns true if shapes contains each other
      /// </returns>
      function  Contains             ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : Boolean ;

      /// <summary>
      ///   Tests overlap relationship for given shapes
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <returns>
      ///   returns true if shapes are overlapping
      /// </returns>
      function  Overlap              ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : Boolean ;

      /// <summary>
      ///   Get all crossing points between given shapes.
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Remember to free this list.
      ///    </note>
      ///   See TGIS_ShapePolygon.GetIntersect for example.
      /// </remarks>
      /// <returns>
      ///   returns list of crossing points
      /// </returns>
      function  GetCrossings         ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : TGIS_PointList ;

      /// <summary>
      ///   Get all crossing points between given shapes.
      /// </summary>
      /// <param name="_shpA">
      ///   given shape
      /// </param>
      /// <param name="_shpB">
      ///   second given shape
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Remember to free this list.
      ///    </note>
      ///   See TGIS_ShapePolygon.GetIntersect for example.
      /// </remarks>
      /// <returns>
      ///   returns list of crossing points
      /// </returns>
      function  GetCrossings3D       ( const _shpA         : TGIS_Shape        ;
                                       const _shpB         : TGIS_Shape
                                     ) : TGIS_Point3DList ;

    public // properties

      /// <summary>
      ///   Tolerance of operations. Things below this level of tolerance
      ///   will be snapped, removed etc.
      /// </summary>
      property Tolerance : Double read  FTolerance
                                  write fset_Tolerance ;

      /// <summary>
      ///   If True then topological operations will force shape fixing. True
      ///   will slow down any polygonal operations due to the fact that
      ///   shape fixing will ensure proper winding and overlapping issues.
      ///   Global default (which is False) can be set using
      ///   GisTopologyForceShapeFixing().
      /// </summary>
      property ForceShapeFixing : Boolean read  FForceShapeFixing
                                          write FForceShapeFixing ;

   published //events
      {$IFDEF CLR}
        /// <summary>
        ///   Busy event. Will be fired regularly during long-drawn operations.
        /// </summary>
        event    BusyEvent : TGIS_BusyEvent delegate FOnBusy ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Busy event. Will be fired regularly during long-drawn operations.
        /// </summary>
        property   BusyEvent : TGIS_BusyEvent read FOnBusy write FOnBusy ;
      {$ENDIF}

  end ;

  /// <summary>
  ///   Set default topological operation mode.
  /// </summary>
  /// <param name="_mode">
  ///   If True then topological operations will force shape fixing. See
  ///   TGIS_Topology.ForceShapeFixing for details.
  /// </param>
  procedure GisTopologyForceShapeFixing( const _mode : Boolean ) ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisTypesUI,
    GisFunctions,
    GisInternals,
    GisResource ;
{$ENDIF}

const
  BMP_SIZE     = 512 ;                // size of bmp contour draw
  BMP_OUTSIDE  = Integer( $ffffff ) ; // outside color same as TGIS_Color.None
  BMP_INSIDE   = Integer( $d3d3d3 ) ; // inside color same as TGIS_Color.LightGray
  BMP_EDGE     = Integer( $696969 ) ; // edge color same as TGIS_Color.DimGray ;

var
  {$IFDEF OXYGENE}
    bForceShapeFixing : Boolean := False ;
  {$ELSE}
    bForceShapeFixing : Boolean  = False ;
  {$ENDIF}

type
  T_CrossingPassType = ( gcpUndefined, gcpInsideOutside, gcpOutsideInside
                      ) ;

  T_EdgeSite       = ( gesUndefined, gesInside, gesOutside, gesSharedSD,
                       gesSharedCD ) ;

  T_DescriptorType = ( gdtPrevious, gdtNext ) ;

  T_EdgeDirection  = ( gedForward, gedBackward ) ;

  T_VerticesOrder  = ( gvoNotShared, gvoEqualSD, gvoEqualCD );

  T_Contour               = class ;
  T_ContourPart           = class ;
  T_CrossVertexDescriptor = class ;
  T_Edge                  = class ;
  T_Vertex                = class ;

  // Encapsulation of contour. For all topological operations.
  T_Contour = class ( TGIS_ObjectDisposable )
    // properties
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FPersistent     : Boolean      ;
      FPartsCount     : Integer      ;
      FPart           : Integer      ;
      FExtent         : TGIS_Extent  ;
      FPartVertices   : T_ContourPart;
      FIsEdgeShared   : Boolean      ;
      FIsEdgeInside   : Boolean      ;
      FIsEdgeOutside  : Boolean      ;
      FEdgesNo        : Integer      ;
      FSharedEdgesNo  : Integer      ;
      FInsideEdgesNo  : Integer      ;
      FOutsideEdgesNo : Integer      ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      bmpOrigin       : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF}  ;
      isClosed        : Boolean      ;
      isChanged       : Boolean      ;
      isSharedSD      : Boolean      ;
      isSharedCD      : Boolean      ;
      gridStep        : Double       ;
      gridStep005     : Double       ;
      listOfParts     : TList<T_ContourPart> ;
      bmpGrid         : TGIS_Pixels  ;
      bmpGridScale    : Double       ;
      bmpGridPos      : TPoint     {$IFDEF GIS_NORECORDS} := new TPoint(0,0) {$ENDIF}  ;

    // property access routines
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      function  fget_VertexNo       : Integer;
      procedure fset_Part           ( const _part        : Integer
                                    ) ;
    private //  various private routines
      procedure setHoles            ;
      procedure redefPartVertices  (const _p : T_ContourPart) ;
      procedure checkFixPartsEx     (const _topology     : TGIS_Topology ;
                                     const _fixselfcross : Boolean ) ;

      // Get any common edges point.
      // _vEf    first vertex of first edge
      // _vEn    first vertex of second edge
      // _cp     crossing point
      // return  True if crossing occurs
      function  getEdgeCrossing     ( const _vE1         : T_Vertex ;
                                      const _vE2         : T_Vertex ;
                                      var   _cp          : TGIS_Point
                                    ) : Boolean ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function  checkAllVertices    : Boolean ;
    protected
      procedure doDestroy           ; override;

    public //constructors
      constructor Create            ; overload;
      constructor Create            ( const _open         : Boolean       ;
                                      const _shape        : TGIS_Shape    ;
                                      const _topology     : TGIS_Topology
                                    ) ; {$IFNDEF OXYGENE} overload; {$ENDIF}
      constructor Create            ( const _open         : Boolean       ;
                                      const _shape        : TGIS_Shape    ;
                                      const _topology     : TGIS_Topology ;
                                      const _prepcontour  : Boolean
                                    ) ; {$IFNDEF OXYGENE} overload; {$ENDIF}
      constructor Create            ( const _open         : Boolean       ;
                                      const _shape        : TGIS_Shape    ;
                                      const _topology     : TGIS_Topology ;
                                      const _prepcontour  : Boolean       ;
                                      const _fixselfcross : Boolean
                                    ) ; {$IFNDEF OXYGENE} overload; {$ENDIF}
      procedure doCreateClosed      ( const _shape        : TGIS_Shape    ;
                                      const _topology     : TGIS_Topology ;
                                      const _prepcontour  : Boolean       ;
                                      const _fixselfcross : Boolean
                                    ) ;
      procedure doCreateOpened      ( const _shape        : TGIS_Shape    ;
                                      const _topology     : TGIS_Topology ;
                                      const _prepcontour  : Boolean       ;
                                      const _fixselfcross : Boolean
                                    ) ;
      procedure   setHolesEx          (const _topology     : TGIS_Topology ) ;
      procedure   Clear             ;
      procedure   FreeExtentsArrays ;

      // Make shaWrite2Shapepe from the contour.
      // shp     shape  which must filled form the contour (must exists)
      procedure   Write2Shape       ( var   _shp         : TGIS_Shape
                                    ) ;
    public // API
      procedure Reinitialize        ;
      procedure AddPart             ( const _part        : T_ContourPart
                                    ) ;
      procedure DeletePart          ( const _partNo      : Integer
                                    ) ;
      function  SnapToContour       ( const _vertex      : T_Vertex   ;
                                      const _tolerance   : Double
                                    ) : Boolean;
      function  JoinContour         ( const _contour     : T_Contour
                                    ) : T_Contour ;
      function  IsPointInsideContour( const _ptg         : TGIS_Point
                                    ) : Boolean ; overload;
      function  IsPointInsideContour( const _ptg         : TGIS_Point ;
                                      const _part        : Integer
                                    ) : Boolean ; overload;
      function  IsPointOnContour    ( const _ptg         : TGIS_Point
                                    ) : Boolean ; overload;
      function  IsPointOnContour    ( const _ptg         : TGIS_Point ;
                                      const _part        : Integer
                                    ) : Boolean ; overload;
      procedure DefineEdgesSite     ( const _nextcontour : TObject
                                    ) ;
      function  GetEdgesNumber          : Integer ;

    public
        // If true the contour will be created as persistent for optimized
        // TGIS_Topology.Relate operations.
        property Persistent : Boolean read FPersistent write FPersistent ;

        // Number of parts in a contour.
        property PartsCount : Integer read FPartsCount ;

        // Number of vertices in a contour.
        property Count : Integer read  fget_VertexNo ;

        // List of vertices in the current part.
        property PartVertices : T_ContourPart read  FPartVertices ;

        // Current part.
        property Part : Integer read  FPart write fset_Part ;

        // Contour extent.
        property Extent : TGIS_Extent read FExtent ;

        // Shared edge with other contour exist.
        property IsEdgeShared : Boolean read FIsEdgeShared ;

        property IsEdgeInside : Boolean read FIsEdgeInside ;

        // Edges  outside other contour exist.
        property IsEdgeOutside : Boolean read FIsEdgeOutside ;

        // Count of all edges in contour.
        property EdgesNo : Integer read FEdgesNo ;

        // Count of shared edges with other contour.
        property SharedEdgesNo : Integer read FSharedEdgesNo ;

        // Count of edges inside other contour.
        property InsideEdgesNo : Integer read FInsideEdgesNo ;

        // Count of edges  outside other contour.
        property OutsideEdgesNo : Integer read FOutsideEdgesNo ;
  end ;

  // Encapsulation of contour part.
  T_ContourPart = class( TGIS_Object )
    public
      Extent       : TGIS_Extent   ;
      SignedArea2  : Double        ;
      ParentPart   : T_ContourPart ;
      VerticesList : TList<T_Vertex> ;
      ExtentsArray : Array of TGIS_Extent ;
      PartNumber   : Integer       ;
      PartChanged  : Boolean       ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function  checkCrossingPassType
                                ( const _partvertex : T_Vertex ;
                                  const _linevertex : T_Vertex
                                ) : T_CrossingPassType ;
    protected
      procedure doDestroy       ; override;
    public //constructors
      constructor  Create       ; overload;
      procedure   Clear         ;
      procedure   SetOwner      ( const _contour  : T_Contour) ;
      procedure   SetExtents    ( const _gridstep : Double
                                ) ; overload;
      procedure   SetExtents    ( const _gridstep : Double  ;
                                  const _idx      : Integer
                                ) ; overload;
      procedure   SetExtents    ( const _gridstep : Double  ;
                                  const _idx      : Integer ;
                                  const _add      : Boolean
                                ) ; overload;
      procedure   SetSignedArea2 ;
      procedure   CheckVreticesDist
                                ( const _gridstep : Double ) ;
  end ;

  // Encapsulation of common and crossing points.
  T_CrossVertexDescriptor = class
    public
      // Type of descriptor.
      DescriptorType : T_DescriptorType {$IFDEF JAVA} := T_DescriptorType.gdtPrevious {$ENDIF} ;

      // Vertex associated to the descriptor.
      CorrespondingVertex : T_Vertex ;

      // Angle of edge.
      EdgeAngle : Double ;
  end ;

  // Encapsulation of edge.
  T_Edge = class
    public
      // Previous vertex - start of the edge.
      PrevVertex : T_Vertex ;

      // Next vertex - end of the edge.
      NextVertex : T_Vertex ;

      // If not nil then a shared edge.
      CommonEdge : T_Edge ;

      // Site of edge related to second contour.
      Site : T_EdgeSite {$IFDEF JAVA} := T_EdgeSite.gesUndefined {$ENDIF} ;

      // For internal use during traversing a contour.
        Marked : Boolean ;
  end ;

  // Encapsulation of common and crossing points.
  T_ExPoint = class
    public
      // Point.
      Point : TGIS_Point ;

      // Is point crossed?
      IsCross : Boolean ;

      // Is point the first point in a part.
      IsPartStart : Boolean ;
  end ;

  // Encapsulation of vertex.
  T_Vertex = class
    public
      // Is the vertex crossed?
      IsCrossVertex : Boolean ;

      // Site for previous edge.
      PrevEdgeSite : T_EdgeSite {$IFDEF JAVA} := T_EdgeSite.gesUndefined {$ENDIF};

      // Site for next edge.
      NextEdgeSite : T_EdgeSite {$IFDEF JAVA} := T_EdgeSite.gesUndefined {$ENDIF};

      // If not nil, then a shared vertex.
      CommonVertex : T_Vertex ;

      // Used temporarily during operations.
      WorkPoint2 : TGIS_Point ;

      //Original position.
      NativePoint2 : TGIS_Point ;

      // Used temporarily during operations.
      WorkPoint3 : TGIS_Point3D ;

      // Original position.
      NativePoint3 : TGIS_Point3D ;

      // Contour which is on the vertex.
      Owner : T_Contour ;

      // List of descriptors associated to the vertex
      // (only if IsCrossvertex)
      DescriptorsList : TList<TObject> ;

      // Descriptor corresponding to the previous edge.
      PrevDescriptor : T_CrossVertexDescriptor ;

      // Descriptor corresponding to the next edge.
      NextDescriptor : T_CrossVertexDescriptor ;

      // Previous edge.
      PrevEdge : T_Edge ;

      // Next edge.
      NextEdge : T_Edge ;

      // Previous vertex.
      PrevVertex : T_Vertex ;

      // Next vertex.
      NextVertex : T_Vertex ;

    public
      // Checks if vertices (self and given) are spliced.
      // _v given vertex
      // return True if are spliced
      function are_spliced( const _v : T_Vertex) : Boolean ;

      // Finds spliced vertex when sheredSD edges exist.
      // return spliced vertex
      function find_sharedSD_vertex : T_Vertex ;

      // Finds spliced vertex when sheredCD edges exist.
      // return spliced vertex
      function find_sharedCD_vertex : T_Vertex ;
  end ;


  T_activePoint = class
    public
      P  : TGIS_Point ;
      AU : Boolean ;
      AD : Boolean ;
  end ;

  T_hullEdge = class
    public
      SI : Integer ;
      EI : Integer ;
      L  : Double ;
    public
      function  Clone : T_hullEdge ;
  end ;


  T_hullBuilder = class
    private
      procedure doConvexHull   ( {$IFDEF OXYGENE}
                                   const _nodes : TList<T_activePoint>
                                 {$ELSE}
                                   const _nodes : TObjectList<T_activePoint>
                                 {$ENDIF}
                               ) ;
      procedure doConcaveHull  ( {$IFDEF OXYGENE}
                                   const _nodes : TList<T_activePoint> ;
                                   const _edges : TList<T_hullEdge> ;
                                 {$ELSE}
                                   const _nodes : TObjectList<T_activePoint> ;
                                   const _edges : TObjectList<T_hullEdge> ;
                                 {$ENDIF}
                                 const _alpha : Double
                               ) ;
      function  shapeFromNodes ( {$IFDEF OXYGENE}
                                   const _nodes : TList<T_activePoint>
                                 {$ELSE}
                                   const _nodes : TObjectList<T_activePoint>
                                 {$ENDIF}
                               ) : TGIS_ShapePolygon ;
      function  shapeFromEdges ( {$IFDEF OXYGENE}
                                   const _nodes : TList<T_activePoint> ;
                                   const _edges : TList<T_hullEdge>
                                 {$ELSE}
                                   const _nodes : TObjectList<T_activePoint> ;
                                   const _edges : TObjectList<T_hullEdge>
                                 {$ENDIF}
                               ) : TGIS_ShapePolygon ;
    public
      /// <summary>
      ///   Creates a convex hull based on the provided layer.
      /// </summary>
      /// <param name="_layer">
      ///   source layer
      /// </param>
      /// <returns>
      ///  convex hull as a polygon
      /// </returns>
      function  ConvexHull   ( const _layer  : TGIS_LayerVector
                             ) : TGIS_ShapePolygon ;
                             {$IFNDEF OXYGENE} overload; {$ENDIF}
      /// <summary>
      ///   Creates a convex hull based on the provided shape.
      /// </summary>
      /// <param name="_shape">
      ///   source shape
      /// </param>
      /// <returns>
      ///  convex hull as a polygon
      /// </returns>
      function  ConvexHull   ( const _shape  : TGIS_Shape
                             ) : TGIS_ShapePolygon ;
                             {$IFNDEF OXYGENE} overload; {$ENDIF}
      /// <summary>
      ///   Creates a convex hull based on the two provided shapes.
      /// </summary>
      /// <param name="_shape1">
      ///   first source shape
      /// </param>
      /// <param name="_shape2">
      ///   second source shape; if nil only _shape1 will be used
      /// </param>
      /// <returns>
      ///  convex hull as a polygon
      /// </returns>
      function  ConvexHull   ( const _shape1 : TGIS_Shape ;
                               const _shape2 : TGIS_Shape
                             ) : TGIS_ShapePolygon ;
                             {$IFNDEF OXYGENE} overload; {$ENDIF}
      /// <summary>
      ///   Creates a convex hull based on the two provided shapes.
      /// </summary>
      /// <param name="_shape1">
      ///   first source shape
      /// </param>
      /// <param name="_shape2">
      ///   second source shape; if nil only _shape1 will be used
      /// </param>
      /// <param name="_fix">
      ///   if True, then checks and, if necessary, fixes the input shapes for
      ///   common topological errors, like self-crossings, overlapping parts
      ///   etc.
      /// </param>
      /// <returns>
      ///  convex hull as a polygon
      /// </returns>
      function  ConvexHull   ( const _shape1 : TGIS_Shape ;
                               const _shape2 : TGIS_Shape ;
                               const _fix    : Boolean
                             ) : TGIS_ShapePolygon ;
                             {$IFNDEF OXYGENE} overload; {$ENDIF}
      /// <summary>
      ///   Creates a concave hull based on the provided layer
      ///   and edge length.
      /// </summary>
      /// <param name="_layer">
      ///   source layer
      /// </param>
      /// <param name="_alpha">
      ///   maximum edge length
      /// </param>
      /// <returns>
      ///  concave hull as a polygon
      /// </returns>
      function  ConcaveHull  ( const _layer  : TGIS_LayerVector ;
                               const _alpha  : Double
                             ) : TGIS_ShapePolygon ;
                             {$IFNDEF OXYGENE} overload; {$ENDIF}
      /// <summary>
      ///   Creates a concave hull based on the provided shape
      ///   and edge length.
      /// </summary>
      /// <param name="_shape">
      ///   source shape
      /// </param>
      /// <param name="_alpha">
      ///   maximum edge length
      /// </param>
      /// <returns>
      ///  concave hull as a polygon
      /// </returns>
      function  ConcaveHull  ( const _shape  : TGIS_Shape ;
                               const _alpha  : Double
                             ) : TGIS_ShapePolygon ;
                             {$IFNDEF OXYGENE} overload; {$ENDIF}
      /// <summary>
      ///   Creates a concave hull based on the two provided shapes
      ///   and edge length.
      /// </summary>
      /// <param name="_shape1">
      ///   first source shape
      /// </param>
      /// <param name="_shape2">
      ///   second source shape; if nil only _shape1 will be used
      /// </param>
      /// <param name="_alpha">
      ///   maximum edge length
      /// </param>
      /// <returns>
      ///  concave hull as a polygon
      /// </returns>
      function  ConcaveHull  ( const _shape1 : TGIS_Shape ;
                               const _shape2 : TGIS_Shape ;
                               const _alpha  : Double
                             ) : TGIS_ShapePolygon ;
                             {$IFNDEF OXYGENE} overload; {$ENDIF}
      /// <summary>
      ///   Creates a concave hull based on the two provided shapes
      ///   and edge length.
      /// </summary>
      /// <param name="_shape1">
      ///   first source shape
      /// </param>
      /// <param name="_shape2">
      ///   second source shape; if nil only _shape1 will be used
      /// </param>
      /// <param name="_alpha">
      ///   maximum edge length
      /// </param>
      /// <param name="_fix">
      ///   if True, then checks and, if necessary, fixes the input shapes for
      ///   common topological errors, like self-crossings, overlapping parts
      ///   etc.
      /// </param>
      /// <returns>
      ///  concave hull as a polygon
      /// </returns>
      function  ConcaveHull  ( const _shape1 : TGIS_Shape ;
                               const _shape2 : TGIS_Shape ;
                               const _alpha  : Double     ;
                               const _fix    : Boolean
                             ) : TGIS_ShapePolygon ;
                             {$IFNDEF OXYGENE} overload; {$ENDIF}
    public
      /// <summary>
      ///   Attach the TGIS_Topology object that is using an instance of
      ///   this class .
      /// </summary>
      GIS_Topology : TGIS_Topology ;
  end ;

//==============================================================================
// Utilities
//==============================================================================

  function ActivePointsCompare(
    const _left  : T_activePoint ;
    const _right : T_activePoint
  ) : Integer ;
  begin
    if _left.P.X < _right.P.X then
      Result := -1
    else
    if _left.P.X > _right.P.X then
      Result := 1
    else
      Result := 0 ;
  end ;

  procedure GisTopologyForceShapeFixing(
    const _mode : Boolean
  ) ;
  begin
    bForceShapeFixing := _mode ;
  end ;

  // Compare T_CrossVertexDescriptor based on an edge angle as required for
  // TList.Sort
  // _p1 first T_CrossVertexDescriptor
  // _p2 second T_CrossVertexDescriptor
  // return    0 if equal, 1 if _p1 > _p2; -1 if _p1 < _p2
  function AnglesVerticesCompare( const _p1, _p2 : TObject ) : Integer ;
  begin
    if T_CrossVertexDescriptor( _p1 ).EdgeAngle >
       T_CrossVertexDescriptor( _p2 ).EdgeAngle
    then
      Result := -1
    else begin
      if T_CrossVertexDescriptor( _p1 ).EdgeAngle <
         T_CrossVertexDescriptor( _p2 ).EdgeAngle
      then
        Result := 1
      else
         Result := 0 ;
    end ;
  end ;

  // Compare shapes Etents as required for TList.Sort
  // _p1 first TGIS_Shape
  // _p2 second TGIS_Shape
  // return    0 if equal, 1 if _p1 top left _p2; -1 if _p1 down right _p2
  function ExtentsCompare( const _p1, _p2 : TGIS_Shape ) : Integer ;
  begin
    if _p1.Extent.YMax >
       _p2.Extent.YMax
    then
      Result := 1
    else begin
      if _p1.Extent.YMax <
         _p2.Extent.YMax
      then
        Result := -1
      else begin
        if _p1.Extent.XMin <
           _p2.Extent.XMin
        then
          Result := 1
        else
        if _p1.Extent.XMin >
           _p2.Extent.XMin
        then
          Result := -1
        else
         Result := 0 ;
      end ;
    end ;
  end ;


  // Compare contour parts areas required for
  // TList.Sort
  // _p1     first contour part
  // _p2     second contour part
  // return  0 if equal, 1 if _p1 < _p2; -1 if _p1 < _p2
  function PartsAreasCompare( const _p1, _p2 : T_ContourPart ) : Integer ;
  begin
    if Abs(T_ContourPart( _p1 ).SignedArea2) <
       Abs(T_ContourPart( _p2 ).SignedArea2)
    then
      Result := 1
    else begin
      if Abs(T_ContourPart( _p1 ).SignedArea2) >
         Abs(T_ContourPart( _p2 ).SignedArea2)
      then
        Result := -1
      else
        Result := 0 ;
    end ;
  end ;

  // Compare contour parts areas required for
  // TList.Sort
  // _p1 first contour part
  // _p2 second contour part
  // return    0 if equal, 1 if _p1 < _p2; -1 if _p1 < _p2
  function PartsNumbersCompare( const _p1, _p2 : T_ContourPart ) : Integer ;
  begin
    if T_ContourPart( _p1 ).PartNumber >
       T_ContourPart( _p2 ).PartNumber
    then
      Result := 1
    else begin
      if T_ContourPart( _p1 ).PartNumber <
         T_ContourPart( _p2 ).PartNumber
      then
        Result := -1
      else
        Result := 0 ;
    end ;
  end ;


//==============================================================================
// T_Contour
//==============================================================================

  // Create an instance.
  constructor T_Contour.Create ;
  begin
    inherited Create ;

    listOfParts := TList<T_ContourPart>.Create ;
    FPartVertices := nil ;
    FPartsCount := 0 ;
    FPart := 0 ;
  end ;

  // Create an instance based on a provided shape. Contour will be closed,
  //  redundant points will be removed. Multiparts are stored on separate lists.
  // _shape        base shape
  // _gridstep     grid tolerance of operations
  constructor T_Contour.Create(
    const _open         : Boolean       ;
    const _shape        : TGIS_Shape    ;
    const _topology     : TGIS_Topology
  ) ;
  begin
    Create( _open, _shape, _topology, False, False )
  end ;

  // Create an instance based on a provided shape. Contour will be closed,
  // redundant points will be removed. Multiparts are stored on separate lists.
  // _shape        base shape
  // _gridstep     grid tolerance of operations
  // _prepcontout  if True, then bitmap representation of contour will
  //               be created for fast point-in-contour check
  constructor T_Contour.Create(
    const _open         : Boolean       ;
    const _shape        : TGIS_Shape    ;
    const _topology     : TGIS_Topology ;
    const _prepcontour  : Boolean
  ) ;
  begin
    Create( _open, _shape, _topology, _prepcontour, False )
  end ;

  // Create an instance based on a provided shape. Contour will be closed,
  // redundant points will be removed. Multiparts are stored on separate lists.
  //_shape        base shape
  //_gridstep     grid tolerance of operations
  //_prepcontout  if True, then bitmap representation of contour will
  //              be created for fast point-in-contour check
  //_fixselfcross if True, then created contour will create subparts
  //              for each loop defined by self-crossing; this can slow
  //              down operations
  constructor T_Contour.Create(
    const _open         : Boolean       ;
    const _shape        : TGIS_Shape    ;
    const _topology     : TGIS_Topology ;
    const _prepcontour  : Boolean       ;
    const _fixselfcross : Boolean
  ) ;
  begin
    Create ;

    if _open then
      doCreateOpened( _shape, _topology, _prepcontour, _fixselfcross )
    else
      doCreateClosed( _shape, _topology, _prepcontour, _fixselfcross ) ;
  end ;

  procedure T_Contour.doCreateClosed(
    const _shape        : TGIS_Shape      ;
    const _topology     : TGIS_Topology   ;
    const _prepcontour  : Boolean         ;
    const _fixselfcross : Boolean
  ) ;
  var
    i, k,
    part_size   : Integer    ;
    vertex      : T_Vertex   ;
    firstvertex : T_Vertex   ;
    prevvertex  : T_Vertex   ;
    partcount   : Integer    ;

    lastpoint2  : TGIS_Point ;
    curpoint2   : TGIS_Point   {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
    firstpoint2 : TGIS_Point ;

    curpoint3   : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;
    dist        : Double     ;
    grid        : Double     ;
    area2       : Double     ;
    nshp        : TGIS_Shape ;

    procedure add_curpoint3_to_polygon ;
    begin

      vertex := T_Vertex.Create ;

      vertex.NativePoint2 := _TGIS_Point( curpoint2 ) ;
      vertex.NativePoint3 := _TGIS_Point3D( curpoint3 ) ;

      vertex.WorkPoint2 := _TGIS_Point( curpoint2 ) ;
      vertex.WorkPoint3 := _TGIS_Point3D( curpoint3 ) ;

      vertex.Owner       := self     ;

      if FPartVertices.Extent.XMin > curpoint2.X then
        FPartVertices.Extent.XMin := curpoint2.X
      else
      if FPartVertices.Extent.XMax < curpoint2.X then
        FPartVertices.Extent.XMax := curpoint2.X ;

      if FPartVertices.Extent.YMin > curpoint2.Y then
        FPartVertices.Extent.YMin := curpoint2.Y
      else
      if FPartVertices.Extent.YMax < curpoint2.Y then
        FPartVertices.Extent.YMax := curpoint2.Y ;

      FPartVertices.VerticesList.Add( vertex ) ;
      lastpoint2 := _TGIS_Point( curpoint2 ) ;

      prevvertex.NextVertex := vertex ;
      vertex.PrevVertex     := prevvertex ;
      prevvertex := vertex ;
    end ;

    procedure close_polygon ;
    var
      v, nv : T_Vertex ;
      nx, ny : Double ;
    begin
      vertex := T_Vertex(FPartVertices.VerticesList[FPartVertices.VerticesList.Count-1]) ;
      firstvertex.PrevVertex := vertex ;
      vertex.NextVertex := firstvertex ;
      v := firstvertex ;
      nx := v.NativePoint2.X ;
      ny := v.NativePoint2.Y ;
      v := v.NextVertex ;
      nv := v.NextVertex ;
      area2 := 0 ;
      while True do begin
        area2 := area2 +(v.NativePoint2.X -nx)*(nv.NativePoint2.Y -ny) -
                        (v.NativePoint2.Y -ny)*(nv.NativePoint2.X -nx) ;

        if nv = firstvertex then
          break ;
        v := nv ;
        nv := v.NextVertex ;
      end ;

      if Abs(area2) < _topology.interTolerance then begin
        FreeObject(FPartVertices) ;
        exit ;
      end ;

      FPartVertices.SignedArea2 := area2 ;
      FPartVertices.ParentPart  := nil ;
      listOfParts.Add(FPartVertices) ;
      FPartVertices.PartNumber := FPartsCount ;
      inc( FPartsCount ) ;
    end ;
  begin

    if GisLockGreaterThanEqual( _shape.LockLevel, TGIS_Lock.Projection ) then begin
      bmpOrigin.X := _shape.Extent.XMin ;
      bmpOrigin.Y := _shape.Extent.YMax ;
    end
    else begin
      bmpOrigin.X := _shape.ProjectedExtent.XMin ;
      bmpOrigin.Y := _shape.ProjectedExtent.YMax ;
    end ;

    isClosed := True ;
    isSharedCD := False ;
    isSharedSD := False ;
    gridStep005 := _topology.gridStep * 0.05 ;
    gridStep    := _topology.gridStep ;

    FIsEdgeInside  := False ;
    FIsEdgeOutside := False ;
    FIsEdgeShared  := False ;

    FPartsCount := 0 ;
    partcount := _shape.GetNumParts ;
    {$IFNDEF JAVA OR ISLAND}
      listOfParts.Capacity := partcount;
    {$ENDIF}

    for k := 0 to partcount -1 do begin
      part_size := _shape.GetPartSize(k) ;
      if (part_size <= 3) and (not _topology.forceFix) then
        continue ;

      FPartVertices := T_ContourPart.Create ;
      {$IFNDEF JAVA OR ISLAND}
        FPartVertices.VerticesList.Capacity := part_size ;
      {$ENDIF}

      vertex := T_Vertex.Create ;
      vertex.NativePoint3 := _shape.GetPoint3D(k, 0) ;
      {$IFDEF GIS_NORECORDS}
        vertex.NativePoint2 := new TGIS_Point ;
      {$ENDIF}
      vertex.NativePoint2.X := vertex.NativePoint3.X ;
      vertex.NativePoint2.Y := vertex.NativePoint3.Y ;
      vertex.WorkPoint2 := _TGIS_Point( vertex.NativePoint2 ) ;
      vertex.WorkPoint3 := _TGIS_Point3D( vertex.NativePoint3 ) ;
      vertex.Owner       := self                  ;

      lastpoint2  := _TGIS_Point( vertex.WorkPoint2 ) ;
      firstpoint2 := _TGIS_Point( vertex.WorkPoint2 ) ;

      firstvertex := vertex ;
      FPartVertices.VerticesList.Add(vertex) ;
      prevvertex   := vertex ;

      FPartVertices.Extent.XMin := firstpoint2.X ;
      FPartVertices.Extent.XMax := firstpoint2.X ;
      FPartVertices.Extent.YMin := firstpoint2.Y ;
      FPartVertices.Extent.YMax := firstpoint2.Y ;

      grid := _topology.Tolerance ;
      for i := 1 to part_size -2 do begin

        curpoint3 := _shape.GetPoint3D(k, i) ;
        curpoint2.X := curpoint3.X ;
        curpoint2.Y := curpoint3.Y ;

        dist := GisPoint2Point(curpoint2, lastpoint2) ;
        if dist < grid  then
          continue ;

        add_curpoint3_to_polygon ;
      end ;

      vertex := T_Vertex(FPartVertices.VerticesList[FPartVertices.VerticesList.Count-1]) ;
      curpoint2 := _TGIS_Point( vertex.WorkPoint2 ) ;

      dist := GisPoint2Point(firstpoint2, curpoint2 ) ;
      while ( dist < grid) and (FPartVertices.VerticesList.Count > 1) do
      begin
        {$IFDEF OXYGENE}
          FPartVertices.VerticesList.RemoveAt(FPartVertices.VerticesList.Count-1) ;
        {$ELSE}
          FPartVertices.VerticesList.Delete(FPartVertices.VerticesList.Count-1) ;
        {$ENDIF}
        prevvertex := vertex.PrevVertex ;
        FreeObject(vertex) ;
        vertex := prevvertex ;

        curpoint2 := T_Vertex(FPartVertices.VerticesList[FPartVertices.VerticesList.Count-1]).WorkPoint2 ;
        dist := GisPoint2Point(firstpoint2, curpoint2 ) ;
      end ;

      if FPartVertices.VerticesList.Count >= 3 then begin
        close_polygon ;
      end
      else begin
        if _topology.forceFix and ( FPartVertices.VerticesList.Count >= 1)then
        begin
          curpoint3.X := curpoint3.X + _topology.Tolerance ;
          curpoint3.Y := curpoint3.Y + _topology.Tolerance ;
          add_curpoint3_to_polygon ;
          if FPartVertices.VerticesList.Count = 2 then begin
            curpoint3.Y := curpoint3.Y -_topology.Tolerance ;
            add_curpoint3_to_polygon ;
          end ;
          close_polygon ;
          isChanged := True ;
        end
        else
          FreeObject( FPartVertices ) ;
      end ;
    end ;

    if listOfParts.Count > 0 then begin
      FPartVertices := listOfParts[0] ;
      FPart := 0 ;
      FExtent := _TGIS_Extent( T_ContourPart(listOfParts[0]).Extent ) ;
      for k := 1 to listOfParts.Count -1 do begin

        if FExtent.XMin > T_ContourPart(listOfParts[k]).Extent.XMin then
          FExtent.XMin := T_ContourPart(listOfParts[k]).Extent.XMin ;

        if FExtent.XMax < T_ContourPart(listOfParts[k]).Extent.XMax then
          FExtent.XMax := T_ContourPart(listOfParts[k]).Extent.XMax ;

        if FExtent.YMin > T_ContourPart(listOfParts[k]).Extent.YMin then
          FExtent.YMin := T_ContourPart(listOfParts[k]).Extent.YMin ;

        if FExtent.YMax < T_ContourPart(listOfParts[k]).Extent.YMax then
          FExtent.YMax := T_ContourPart(listOfParts[k]).Extent.YMax ;
      end ;

      if ( not ( _shape.IsFixed ) and _shape.IsModified )
         or
         ( _fixselfcross or _topology.ForceShapeFixing or _topology.checkCombine)
      then begin
        if _fixselfcross or _topology.ForceShapeFixing then begin
          checkAllVertices ;
        end ;

        checkFixPartsEx(_topology, _fixselfcross ) ;
        if _fixselfcross then
          _shape.IsFixed := True ;
      end
      else begin
        if (FPartVertices.SignedArea2 > 0) and (listOfParts.Count = 1) then
          redefPartVertices(FPartVertices) ;
      end ;

      // create bitmap grid
      if _prepcontour then begin
        if isChanged then begin
          nshp := TGIS_ShapePolygon.Create( nil, nil, False, 0, _shape.Layer,
                     _shape.Dimension ) ;
          Write2Shape(nshp) ;
          nshp.PrepareContourInternal( BMP_SIZE, True, bmpGrid, bmpGridScale, bmpGridPos ) ;
          FreeObject(nshp) ;
        end
        else
          _shape.PrepareContourInternal( BMP_SIZE, True, bmpGrid, bmpGridScale, bmpGridPos ) ;
      end
      else begin
        bmpGridScale := 0 ;
      end ;

      if  _topology.ForceShapeFixing or _fixselfcross or _topology.checkCombine then begin
        listOfParts.Sort( {$IFDEF OXYGENE}
                            @PartsNumbersCompare
                          {$ELSE}
                            TComparer<T_ContourPart>.Construct( PartsNumbersCompare )
                          {$ENDIF} ) ;
        if partcount = FPartsCount then
          setHoles ;
      end ;
    end ;
  end ;

  // Create an instance based on a provided shape. Redundant points will be
  // removed. Multiparts are stored on separate lists. Last element points to
  // itself.
  // _shape        base shape
  // _prepcontout  if True, then bitmap representation of contour will
  //               be created for fast point-in-contour check
  // _fixselfcross if True, then created contour will create subparts
  //               for each loop defined by self-crossing; this can slow
  //               down operations
  procedure T_Contour.doCreateOpened(
    const _shape        : TGIS_Shape    ;
    const _topology     : TGIS_Topology ;
    const _prepcontour  : Boolean       ;
    const _fixselfcross : Boolean
  ) ;
  var
    i, k,
    part_size   : Integer    ;
    vertex      : T_Vertex   ;
    prevvertex  : T_Vertex   ;
    partcount   : Integer    ;

    lastpoint2   : TGIS_Point ;
    blastpoint2  : TGIS_Point ;
    curpoint2    : TGIS_Point ;
    ncurpoint2   : TGIS_Point ;
    firstpoint2  : TGIS_Point ;

    lastpoint3   : TGIS_Point3D ;
    blastpoint3  : TGIS_Point3D ;
    curpoint3    : TGIS_Point3D ;
    ncurpoint3   : TGIS_Point3D ;
    dist         : Double     ;
    grid09       : Double     ;

    procedure add_ncurpoint3_to_arc ;
    begin
      curpoint3 := _TGIS_Point3D( ncurpoint3 ) ;
      {$IFDEF GIS_NORECORDS}
        ncurpoint2 := new TGIS_Point ;
      {$ENDIF}
      ncurpoint2.X := ncurpoint3.X ;
      ncurpoint2.Y := ncurpoint3.Y ;
      curpoint2 := _TGIS_Point( ncurpoint2 ) ;

      if FPartVertices.VerticesList.Count >1 then begin
        dist := GisPoint2Point(curpoint2, blastpoint2) ;
        if dist < grid09  then begin
          exit ;
        end ;
      end ;
      dist := GisPoint2Point(curpoint2, lastpoint2) ;
      if dist >= grid09  then begin
        vertex := T_Vertex.Create ;

        vertex.NativePoint2 := _TGIS_Point( ncurpoint2 ) ;
        vertex.NativePoint3 := _TGIS_Point3D( ncurpoint3 ) ;

        vertex.WorkPoint2 := _TGIS_Point( curpoint2 ) ;
        vertex.WorkPoint3 := _TGIS_Point3D( curpoint3 ) ;

        vertex.Owner       := self     ;

        if FPartVertices.Extent.XMin > curpoint2.X then
          FPartVertices.Extent.XMin := curpoint2.X
        else
        if FPartVertices.Extent.XMax < curpoint2.X then
          FPartVertices.Extent.XMax := curpoint2.X ;

        if FPartVertices.Extent.YMin > curpoint2.Y then
          FPartVertices.Extent.YMin := curpoint2.Y
        else
        if FPartVertices.Extent.YMax < curpoint2.Y then
          FPartVertices.Extent.YMax := curpoint2.Y ;

        FPartVertices.VerticesList.Add( vertex ) ;
        blastpoint2 := _TGIS_Point( lastpoint2 ) ;
        lastpoint2  := _TGIS_Point( curpoint2  ) ;
        blastpoint3 := _TGIS_Point3D( lastpoint3 ) ;
        lastpoint3  := _TGIS_Point3D( curpoint3  ) ;
        prevvertex.NextVertex := vertex ;
        vertex.PrevVertex     := prevvertex ;
        prevvertex := vertex ;
      end ;
    end ;
  begin

    bmpGrid := nil ;

    // create bitmap grid
    if _prepcontour then begin
      _shape.PrepareContourInternal( BMP_SIZE, True, bmpGrid, bmpGridScale, bmpGridPos ) ;
    end
    else begin
      bmpGridScale := 0 ;
    end ;

    if GisLockGreaterThanEqual( _shape.LockLevel, TGIS_Lock.Projection ) then
    begin
      bmpOrigin.X := _shape.Extent.XMin ;
      bmpOrigin.Y := _shape.Extent.YMax ;
    end
    else begin
      bmpOrigin.X := _shape.ProjectedExtent.XMin ;
      bmpOrigin.Y := _shape.ProjectedExtent.YMax ;
    end ;

    isClosed := False ;
    isSharedCD := False ;
    isSharedSD := False ;
    gridStep005 := _topology.gridStep * 0.05 ;

    FIsEdgeInside  := False ;
    FIsEdgeOutside := False ;
    FIsEdgeShared  := False ;

    FPartsCount := 0 ;
    partcount := _shape.GetNumParts ;
    {$IFNDEF JAVA OR ISLAND}
      listOfParts.Capacity := partcount;
    {$ENDIF}

    for k := 0 to partcount -1 do begin
      part_size := _shape.GetPartSize(k) ;
      if part_size <= 1 then
        continue ;

      FPartVertices := T_ContourPart.Create ;
      {$IFNDEF JAVA OR ISLAND}
        FPartVertices.VerticesList.Capacity := part_size ;
      {$ENDIF}

      vertex := T_Vertex.Create ;
      vertex.NativePoint3 := _shape.GetPoint3D(k, 0) ;
      {$IFDEF GIS_NORECORDS}
        vertex.NativePoint2 := new TGIS_Point ;
      {$ENDIF}
      vertex.NativePoint2.X := vertex.NativePoint3.X ;
      vertex.NativePoint2.Y := vertex.NativePoint3.Y ;
      vertex.WorkPoint2 := _TGIS_Point( vertex.NativePoint2 ) ;
      vertex.WorkPoint3 := _TGIS_Point3D( vertex.NativePoint3 ) ;
      vertex.Owner       := self ;

      vertex.PrevVertex := vertex ;

      lastpoint2  := _TGIS_Point( vertex.WorkPoint2 ) ;
      firstpoint2 := _TGIS_Point( vertex.WorkPoint2 ) ;
      lastpoint3  := _TGIS_Point3D( vertex.WorkPoint3 ) ;

      FPartVertices.VerticesList.Add(vertex) ;
      prevvertex   := vertex ;

      FPartVertices.Extent.XMin := firstpoint2.X ;
      FPartVertices.Extent.XMax := firstpoint2.X ;
      FPartVertices.Extent.YMin := firstpoint2.Y ;
      FPartVertices.Extent.YMax := firstpoint2.Y ;

      gridStep := _topology.gridStep ;
      grid09   := _topology.gridStep09 ;

      for i := 1 to part_size -1 do begin
        ncurpoint3 := _shape.GetPoint3D(k, i) ;
        add_ncurpoint3_to_arc ;
      end ;

      vertex.NextVertex := vertex ;

      if FPartVertices.VerticesList.Count >= 2 then begin
        listOfParts.Add(FPartVertices) ;
        inc( FPartsCount ) ;
      end
      else begin
        if _topology.forceFix and ( FPartVertices.VerticesList.Count = 1)then
        begin
          ncurpoint3.X := ncurpoint3.X + _topology.Tolerance ;
          ncurpoint3.Y := ncurpoint3.Y + _topology.Tolerance ;
          add_ncurpoint3_to_arc ;
          vertex.NextVertex := vertex ;
          listOfParts.Add(FPartVertices) ;
          inc( FPartsCount ) ;
          isChanged := True ;
        end
        else
          FreeObject( FPartVertices ) ;
      end ;
    end ;

    if listOfParts.Count > 0 then begin
      FPartVertices := T_ContourPart( listOfParts[0] ) ;
      FPart := 0 ;
      FExtent := _TGIS_Extent( T_ContourPart(listOfParts[0]).Extent ) ;
      for k := 1 to listOfParts.Count -1 do begin

        if FExtent.XMin > T_ContourPart(listOfParts[k]).Extent.XMin then
          FExtent.XMin := T_ContourPart(listOfParts[k]).Extent.XMin ;

        if FExtent.XMax < T_ContourPart(listOfParts[k]).Extent.XMax then
          FExtent.XMax := T_ContourPart(listOfParts[k]).Extent.XMax ;

        if FExtent.YMin > T_ContourPart(listOfParts[k]).Extent.YMin then
          FExtent.YMin := T_ContourPart(listOfParts[k]).Extent.YMin ;

        if FExtent.YMax < T_ContourPart(listOfParts[k]).Extent.YMax then
          FExtent.YMax := T_ContourPart(listOfParts[k]).Extent.YMax ;
      end ;

      if _fixselfcross then begin
        checkAllVertices ;
        _shape.IsFixed := True ;
      end ;
    end ;
  end ;

  // Destroy an instance.
  procedure T_Contour.doDestroy ;
  var
    i : Integer ;
  begin
    if listOfParts <> nil then begin
      for i := FPartsCount -1 downto 0 do begin
        {$IFNDEF NEXTGEN}
          if assigned( T_ContourPart( listOfParts[i] ) ) then
            FreeObjectNotNil( T_ContourPart( listOfParts[i] ) ) ;
        {$ENDIF}
      end ;
      FreeObject( listOfParts );
    end ;
    inherited ;
  end ;

  // Clears parts list.
  procedure T_Contour.Clear ;
  begin
    if listOfParts <> nil then
      listOfParts.Clear;

    FPartsCount := 0 ;
  end ;

  // Clears parts ExtentsArray.
  procedure T_Contour.FreeExtentsArrays ;
  var
    i : Integer ;
  begin
     for i := FPartsCount -1 downto 0 do
        T_ContourPart(listOfParts[i]).ExtentsArray := nil ;
  end ;

  procedure T_Contour.Write2Shape(
    var _shp : TGIS_Shape
  ) ;
  var
    i, k : Integer       ;
    p    : T_ContourPart ;
  begin
    if (_shp = nil) or (Count = 0) then
      exit
    else begin
      _shp.Reset ;
      for i := 0 to FPartsCount -1 do begin
        p := listOfParts[i] ;
        _shp.AddPart ;
        if _shp.ShapeType = TGIS_ShapeType.Polygon then begin
          for k := 0 to p.VerticesList.Count -1 do begin
            TGIS_ShapePolygon(_shp).AddPoint3D(
                    T_Vertex(p.VerticesList[k]).NativePoint3) ;
          end ;
          TGIS_ShapePolygon(_shp).AddPoint3D(
                    T_Vertex(p.VerticesList[0]).NativePoint3) ;
        end
        else
          for k := 0 to p.VerticesList.Count -1 do begin
            _shp.AddPoint3D(T_Vertex(p.VerticesList[k]).NativePoint3) ;
          end ;
      end ;
      _shp.IsFixed := True ;
    end ;
  end ;

  // Property Part access routine.
  procedure T_Contour.fset_Part(
    const _part : Integer
  ) ;
  begin
    if _part < FPartsCount then begin
      FPartVertices := listOfParts[_part] ;
      FPart := _part ;
    end ;
  end ;

  procedure T_Contour.redefPartVertices(
    const _p : T_ContourPart
  ) ;
  var
    i1     : Integer ;
    cc    : Integer ;
    vertex      : T_Vertex   ;
    firstvertex : T_Vertex   ;
    prevvertex  : T_Vertex   ;

  begin

     if assigned( _p.ExtentsArray ) then
        _p.ExtentsArray := nil ;

    _p.SignedArea2 := -_p.SignedArea2 ;
    cc := _p.VerticesList.Count -1 ;
    i1 := 0 ;
    repeat
      vertex := T_Vertex( _p.VerticesList[i1] ) ;
      prevvertex := vertex.PrevVertex ;
      vertex.PrevVertex := vertex.NextVertex ;
      vertex.NextVertex := prevvertex ;

      firstvertex := T_Vertex( _p.VerticesList[cc] ) ;
      prevvertex := firstvertex.PrevVertex ;
      firstvertex.PrevVertex := firstvertex.NextVertex ;
      firstvertex.NextVertex := prevvertex ;

      _p.VerticesList[i1] := firstvertex ;

      _p.VerticesList[cc] := vertex ;
      inc( i1 ) ;
      dec( cc ) ;
    until i1 >= cc ;

    if i1 = cc then begin
      vertex := T_Vertex( _p.VerticesList[i1] ) ;
      prevvertex := vertex.PrevVertex ;
      vertex.PrevVertex := vertex.NextVertex ;
      vertex.NextVertex := prevvertex ;
    end ;

  end ;

  procedure T_Contour.checkFixPartsEx(
    const _topology     : TGIS_Topology ;
    const _fixselfcross : Boolean
  ) ;
  var
    i1, k1, j   : Integer ;
    i_part      : T_ContourPart ;
    k_part      : T_ContourPart ;
    li_parent   : T_ContourPart ;
    lk_parent   : T_ContourPart ;

    k_ext,
    i_ext       : TGIS_Extent ;
    start_i,
    start_k     : Integer ;
    topo        : TGIS_Topology ;
    c1p, c2     : T_Contour  ;
    cand        : T_Contour  ;
    c2_cand     : T_Contour  ;
    c1_cand     : T_Contour  ;
    redef       : Boolean    ;
    break_label : Boolean    ;
    ccw_p       : Array of Boolean ;
  const
    SIN45 = 0.70710678118654752440084436210485 ;
  function is_hole ( const _tpart : T_ContourPart ) : Boolean ;
  var
    tpart : T_ContourPart ;
  begin
    Result := False ;
    tpart := _tpart ;
    while tpart.ParentPart <> nil do begin
      Result := not Result ;
      tpart := tpart.ParentPart ;
    end ;
  end ;
  begin

    if listOfParts.Count = 0 then exit ;

    {$IFDEF OXYGENE}
      listOfParts.Sort( @PartsAreasCompare ) ;
    {$ELSE}
      listOfParts.Sort( TComparer<T_ContourPart>.Construct( PartsAreasCompare ) ) ;
    {$ENDIF}

    SetLength(ccw_p, listOfParts.Count) ;
    for i1 := 0 to listOfParts.Count -1 do begin
      T_ContourPart( listOfParts[i1] ).ParentPart := nil ;
      if T_ContourPart( listOfParts[i1] ).SignedArea2 > 0 then
        ccw_p[i1] := True
      else
        ccw_p[i1] := False ;
    end ;

    start_i := 1 ;
    start_k := 0 ;
    break_label := False ;
    topo := TGIS_Topology.Create ;
    topo.Tolerance := _topology.interTolerance ;

    repeat
      if break_label then begin
        {$IFDEF OXYGENE}
          listOfParts.Sort( @PartsAreasCompare ) ;
        {$ELSE}
          listOfParts.Sort( TComparer<T_ContourPart>.Construct( PartsAreasCompare ) ) ;
        {$ENDIF}
        for i1 := 0 to listOfParts.Count -1 do
          T_ContourPart( listOfParts[i1] ).ParentPart := nil ;
        break_label := False ;
        start_i := 1 ;
        start_k := 0 ;
      end;

      FPartVertices := T_ContourPart( listOfParts[0] ) ;

      if FPartVertices.SignedArea2 > 0 then begin
        redefPartVertices(FPartVertices) ;
        redef := True ;
        isChanged := True ;
      end
      else
        redef := False ;
      if listOfParts.Count <= 1 then begin
        FreeObject( topo ) ;
        exit ;
      end;

      if not (_topology.ForceShapeFixing or _fixselfcross or _topology.checkCombine) then begin
        if redef then begin
          for i1 := 1 to listOfParts.Count -1 do
            redefPartVertices( T_ContourPart(listOfParts[i1]) ) ;
        end ;
        FreeObject( topo ) ;
        exit ;
      end ;

      for i1 := listOfParts.Count -1 downto start_k +1 do
      begin
        start_i := i1 ;
        if i1 >= listOfParts.Count then begin
          dec(start_i) ;
          break ;
        end;
        i_part := T_ContourPart( listOfParts[i1] ) ;

        i_ext := i_part.Extent ;

        for k1 := 0 to i1 - 1do begin
          if k1 >= listOfParts.Count then begin
            break ;
          end;

          start_k := k1 ;
          k_part := T_ContourPart( listOfParts[k1] ) ;
          k_ext := k_part.Extent ;

          if ( i_ext.XMin > k_ext.XMax ) or
             ( i_ext.YMin > k_ext.YMax ) or
             ( i_ext.XMax < k_ext.XMin ) or
             ( i_ext.YMax < k_ext.YMin ) then continue ;

          li_parent := i_part.ParentPart ;
          i_part.ParentPart := nil ;
          lk_parent := k_part.ParentPart ;
          k_part.ParentPart := nil ;

          c1p := T_Contour.Create ;
          c1p.gridStep := _topology.interTolerance / 2 * SIN45 ; ;
          c1p.gridStep005 := c1p.gridStep*0.05 ;

          c2 := T_Contour.Create ;
          c1p.isClosed := True ;
          c2.isClosed := True ;
          c2.gridStep := c1p.gridStep ;
          c2.gridStep005 := c1p.gridStep005 ;

          if k_part.SignedArea2 > 0 then begin
            redefPartVertices(k_part) ;
          end ;
          c1p.AddPart(k_part) ;
          k_part.SetOwner(c1p);
          c1p.FExtent := k_part.Extent ;

          if i_part.SignedArea2 > 0 then begin
            redefPartVertices(i_part) ;
          end ;
          c2.AddPart(i_part);
          i_part.SetOwner(c2);
          c2.FExtent := i_part.Extent ;

          cand := T_Contour(topo.combineCPolygons(c1p, c2,
                            TGIS_TopologyCombineType.Intersection)) ;

          c1p.Reinitialize ;
          c2.Reinitialize ;

          if cand = nil then begin
            i_part.SetOwner(self);
            i_part.ParentPart := li_parent ;
            if assigned(li_parent) then
              i_part.ParentPart.SetOwner(self);
            k_part.SetOwner(self);
            k_part.ParentPart := lk_parent ;
            if assigned(lk_parent) then
              k_part.ParentPart.SetOwner(self);

            c1p.DeletePart(0) ;
            FreeObject(c1p) ;
            c2.DeletePart(0);
            FreeObject( c2 ) ;
            continue ;
          end
          else
          if cand.FPartsCount = 1 then begin
            i_part.SetSignedArea2 ;
            if Abs(i_part.SignedArea2 - cand.FPartVertices.SignedArea2) <
                gridStep005 then
            begin
              if Abs(k_part.SignedArea2 - cand.FPartVertices.SignedArea2) <
                gridStep005 then
              begin // two parts are identical
                {$IFDEF OXYGENE}
                  listOfParts.RemoveAt( i1 );
                  c1p.listOfParts.RemoveAt(0);
                {$ELSE}
                  listOfParts.Delete( i1 );
                  c1p.listOfParts.Delete(0);
               {$ENDIF}
                FreeObject( cand ) ;
                FreeObject(i_part) ;
                dec(start_i) ;
                dec(start_k) ;
                dec(FPartsCount) ;
                FreeObject(c1p.listOfParts) ;
                FreeObject( c1p ) ;
                FreeObject(c2.listOfParts) ;
                FreeObject( c2 ) ;
                break_label := True ;
                break ;
              end;
              FreeObject( cand ) ;

              if assigned(i_part) then begin
                i_part.SetOwner(self);
                i_part.ParentPart := k_part ;
                i_part.ParentPart.SetOwner(self);
              end;
              FreeObject(c1p.listOfParts) ;
              FreeObject( c1p ) ;

              FreeObject(c2.listOfParts) ;
              FreeObject( c2 ) ;
              continue ;
            end ;
          end ;

          c2_cand := T_Contour(topo.combineCPolygons(c2, cand,
                            TGIS_TopologyCombineType.Difference)) ;

          if c2_cand = nil then begin
            c2.Reinitialize ;
            i_part.SetOwner(self);

            FreeObject( cand ) ;
            FreeObject(c1p.listOfParts) ;
            FreeObject( c1p ) ;

            FreeObject(c2.listOfParts) ;
            FreeObject( c2 ) ;
            continue ;
          end ;

          cand.Reinitialize ;

          c1_cand := T_Contour(topo.combineCPolygons(c1p, cand,
                            TGIS_TopologyCombineType.Difference)) ;

          cand.Reinitialize ;

          FreeObject( i_part ) ;
          FreeObject( k_part ) ;
          {$IFDEF OXYGENE}
            listOfParts.RemoveAt( i1 );
          {$ELSE}
            listOfParts.Delete( i1 );
          {$ENDIF}

          {$IFDEF OXYGENE}
            listOfParts.RemoveAt( k1 );
          {$ELSE}
            listOfParts.Delete( k1 );
          {$ENDIF}


          start_i := i1 ;

          if start_i = 0 then
            inc( start_i ) ;

          dec( FPartsCount ) ;

          for j := 0 to c2_cand.listOfParts.Count - 1 do begin
            T_ContourPart(c2_cand.listOfParts[j]).SetOwner(self);
            listOfParts.Add(c2_cand.listOfParts[j]) ;
          end ;
          inc( FPartsCount, c2_cand.listOfParts.Count ) ;

          FreeObject( c2_cand.listOfParts ) ;
          FreeObject( c2_cand ) ;

          dec( FPartsCount ) ;
          if assigned(c1_cand) then begin
            for j := 0 to c1_cand.listOfParts.Count - 1 do begin
              T_ContourPart(c1_cand.listOfParts[j]).SetOwner(self);
              listOfParts.Add(c1_cand.listOfParts[j]) ;
            end ;
            inc( FPartsCount, c1_cand.listOfParts.Count ) ;

            FreeObject( c1_cand.listOfParts ) ;
            FreeObject( c1_cand ) ;
          end;

          for j := 0 to cand.listOfParts.Count - 1 do begin
            T_ContourPart(cand.listOfParts[j]).SetOwner(self);
            redefPartVertices(T_ContourPart(cand.listOfParts[j])) ;
            listOfParts.Add(cand.listOfParts[j]) ;
            T_ContourPart(cand.listOfParts[j]).ParentPart := k_part ;
          end ;
          inc( FPartsCount, cand.listOfParts.Count ) ;

          FreeObject( cand.listOfParts ) ;
          FreeObject( cand ) ;

          FreeObject( c1p.listOfParts ) ;
          FreeObject( c1p ) ;

          FreeObject( c2.listOfParts ) ;
          FreeObject( c2 ) ;
          break_label := True ;
          break ;
        end ;
        if break_label then
          break ;
      end ;
    until not break_label ;

    FreeObject( topo ) ;

    for i1 := 0 to listOfParts.Count -1 do begin
      if  listOfParts[i1].SignedArea2 < 0 then begin
        if is_hole( listOfParts[i1])  then
          redefPartVertices(listOfParts[i1]) ;
      end
      else
      begin
        if not is_hole( listOfParts[i1])  then
          redefPartVertices(listOfParts[i1]) ;
      end ;
    end ;

    if listOfParts.Count = length(ccw_p) then begin
      for i1 := 0 to listOfParts.Count -1 do begin
        if  listOfParts[i1].SignedArea2 > 0 <> ccw_p[i1]then begin
          isChanged := True ;
          T_ContourPart( listOfParts[i1] ).PartChanged := True ;
        end ;
      end ;
    end
    else
      isChanged := True ;

  end ;

  // Set holes after proper parts
  // _list - list of polygon parts
  procedure T_Contour.setHoles ;
  var
    lands   : array of TObject ;
    holes   : array of TObject ;
    al      : TObject ;
    lno,
    hno, i  : Integer ;
    hdone   : Integer ;
    ldone   : Integer ;
    listidx : Integer ;
  begin
    if listOfParts.Count <= 1 then exit ;

    SetLength( lands, listOfParts.Count ) ;
    SetLength( holes, listOfParts.Count ) ;

    lno := 0 ;
    hno := 0 ;

    for i := 0 to listOfParts.Count -1 do begin
      if T_ContourPart(listOfParts[i]).SignedArea2 < 0 then begin
        lands[lno] := listOfParts[i] ;
        inc( lno ) ;
      end
      else begin
        holes[hno] := listOfParts[i] ;
        inc( hno ) ;
      end ;
    end ;

    ldone   := 0 ;
    hdone   := 0 ;
    listidx := 0 ;

    repeat
      listOfParts[listidx] := T_ContourPart(lands[ldone]) ;
      inc( listidx ) ;
      if hdone < hno then begin
        al := lands[ldone] ;
        for i := 0 to hno -1 do begin
          if assigned( holes[i]) and (al = T_ContourPart(holes[i]).ParentPart) then begin
            listOfParts[listidx] := T_ContourPart(holes[i]) ;
            inc( listidx ) ;
            inc( hdone ) ;
          end ;
        end ;
      end ;
      inc( ldone ) ;
    until ( ldone >= lno ) ;

    // copy the rest of not used holes
    for i := 0 to hno -1 do begin
      if listidx = listOfParts.Count then break ;
      if assigned( holes[i]) then begin
        listOfParts[listidx] := T_ContourPart(holes[i]) ;
        inc( listidx ) ;
      end ;
    end ;

    holes := nil ;
    lands := nil ;
  end ;

  // Find islands and thirs holes and set holes after proper parts
  // _list - list of polygon parts
  procedure T_Contour.setHolesEx(
    const _topology     : TGIS_Topology
  ) ;
  var
    ff : Boolean ;
  begin
    ff := _topology.ForceShapeFixing ;
    _topology.ForceShapeFixing := True ;
    checkFixPartsEx(_topology, False) ;
    setHoles ;
    _topology.ForceShapeFixing := ff ;
  end ;

  // Property VertexNo access routine.
  function T_Contour.fget_VertexNo : Integer ;
  var
    i : Integer ;
    ncount : Integer ;
  begin
    ncount := FPartsCount ;
    Result := 0 ;
    for i := ncount -1 downto 0 do begin
      if not assigned( T_ContourPart(listOfParts[i]).VerticesList ) then begin
        {$IFDEF OXYGENE}
          listOfParts.RemoveAt( i );
        {$ELSE}
          listOfParts.Delete( i );
        {$ENDIF}
        dec( FPartsCount ) ;
        continue ;
      end ;
      Result := Result +T_ContourPart(listOfParts[i]).VerticesList.Count ;
    end ;
  end ;

  // Test all points in contour.
  // return: if True, then all points are located properly
  function T_Contour.checkAllVertices : Boolean ;
  var
    i           : Integer    ;
    vertex      : T_Vertex   ;
    npart       : T_ContourPart ;

    function splitPart(
      const _part   : Integer
    ) : T_ContourPart ;
    var
      fv, v, cv : T_Vertex      ;
      pv, nv    : T_Vertex      ;
      lpart     : T_ContourPart ;
      idx, i1   : Integer ;
      area2     : Double  ;
      nx, ny    : Double ;
    begin
      isChanged := True ;
      Result := T_ContourPart.Create ;
      lpart := listOfParts[_part] ;
      fv := lpart.VerticesList[0] ;

      idx := 0 ;
      v := fv ;
      while v.CommonVertex = nil do begin
        v := v.NextVertex ;
        inc( idx ) ;
      end ;
      cv  := v ;

      Result.Extent.XMin := v.WorkPoint2.X ;
      Result.Extent.YMin := v.WorkPoint2.Y ;
      Result.Extent.XMax := v.WorkPoint2.X ;
      Result.Extent.YMax := v.WorkPoint2.Y ;
      area2 := 0 ;
      nx := v.WorkPoint2.X ;
      ny := v.WorkPoint2.Y ;
      repeat
        Result.VerticesList.Add(v) ;

        if Result.Extent.XMin > v.WorkPoint2.X then
          Result.Extent.XMin := v.WorkPoint2.X
        else
        if Result.Extent.XMax < v.WorkPoint2.X then
          Result.Extent.XMax := v.WorkPoint2.X ;

        if Result.Extent.YMin > v.WorkPoint2.Y then
          Result.Extent.YMin := v.WorkPoint2.Y
        else
        if Result.Extent.YMax < v.WorkPoint2.Y then
          Result.Extent.YMax := v.WorkPoint2.Y ;

        {$IFDEF OXYGENE}
          lpart.VerticesList.RemoveAt(idx);
        {$ELSE}
          lpart.VerticesList.Delete(idx);
        {$ENDIF}
        v := v.NextVertex ;
        area2 := area2 +(v.PrevVertex.WorkPoint2.X -nx)*
                        (v.WorkPoint2.Y -ny)-
                        (v.PrevVertex.WorkPoint2.Y -ny)*
                        (v.WorkPoint2.X - nx);

      until v  = cv.CommonVertex ;

      cv.CommonVertex := nil ;
      cv.IsCrossVertex := False ;

      v.CommonVertex := nil ;
      v.IsCrossVertex := False ;

      pv := v.PrevVertex ;

      v.PrevVertex := cv.PrevVertex ;
      cv.PrevVertex.NextVertex := v ;

      if isClosed then begin
        Result.SignedArea2 := area2 ;
        lpart.SignedArea2 := lpart.SignedArea2 -area2 ;

        cv.PrevVertex := pv ;
        pv.NextVertex := cv ;
      end
      else begin
        nv := T_Vertex.Create ;
        nv.NativePoint2 := _TGIS_Point  ( cv.NativePoint2 ) ;
        nv.NativePoint3 := _TGIS_Point3D( cv.NativePoint3 ) ;
        nv.WorkPoint2   := _TGIS_Point  ( cv.WorkPoint2   ) ;
        nv.WorkPoint3   := _TGIS_Point3D( cv.WorkPoint3   ) ;
        nv.Owner := cv.Owner ;
        nv.NextVertex := nv ;
        nv.PrevVertex := pv ;
        pv.NextVertex := nv ;

        Result.VerticesList.Add(nv) ;
      end ;

      fv := lpart.VerticesList[0] ;

      lpart.Extent.XMin := fv.WorkPoint2.X ;
      lpart.Extent.YMin := fv.WorkPoint2.Y ;
      lpart.Extent.XMax := fv.WorkPoint2.X ;
      lpart.Extent.YMax := fv.WorkPoint2.Y ;

      for i1 := 1 to lpart.VerticesList.Count -1 do begin
        fv := fv.NextVertex ;

        if lpart.Extent.XMin > fv.WorkPoint2.X then
          lpart.Extent.XMin := fv.WorkPoint2.X
        else
        if lpart.Extent.XMax < fv.WorkPoint2.X then
          lpart.Extent.XMax := fv.WorkPoint2.X ;

        if lpart.Extent.YMin > fv.WorkPoint2.Y then
          lpart.Extent.YMin := fv.WorkPoint2.Y
        else
        if lpart.Extent.YMax < fv.WorkPoint2.Y then
          lpart.Extent.YMax := fv.WorkPoint2.Y ;
      end ;

    end ;

    function findSelfCrossings(
      const _part : Integer
    ) : Boolean ;
    var
      vE1, vE2  : T_Vertex      ;
      cv1, cv2  : T_Vertex      ;
      pt        : TGIS_Point    ;
      fv, nv    : T_Vertex      ;
      fvf       : T_Vertex      ;
      lpart     : T_ContourPart ;
      idx1,idx2 : Integer       ;
      idxa      : Integer       ;
      d1, d2    : Double ;
    begin

      Result := False ;
      lpart := T_ContourPart( listOfParts[_part] ) ;
      if lpart.VerticesList.Count <= 3 then
        exit ;

      fv := T_Vertex( lpart.VerticesList[0] ) ;

      vE1 := fv ;
      nv := fv.NextVertex ;
      vE2 := nv ;

      if fv.PrevVertex = fv then //not isClosed
        fvf := T_Vertex( lpart.VerticesList[lpart.VerticesList.Count -1] )
      else
        fvf := fv ;

      idx1 := 1 ;

      while True do begin
        vE2 := vE2.NextVertex ;
        idx2 := idx1 + 2 ;
        idxa := idx1 + 1 ;
        repeat

          if not assigned( lpart.ExtentsArray ) then
            lpart.SetExtents(gridStep) ;

          if (vE1.WorkPoint2.X < lpart.ExtentsArray[idxa].XMin) and
             (vE1.NextVertex.WorkPoint2.X < lpart.ExtentsArray[idxa].XMin) then
          begin
            break ;
          end ;

          if (vE1.WorkPoint2.X > lpart.ExtentsArray[idxa].XMax) and
             (vE1.NextVertex.WorkPoint2.X > lpart.ExtentsArray[idxa].XMax) then
          begin
            break ;
          end ;

          if (vE1.WorkPoint2.Y < lpart.ExtentsArray[idxa].YMin) and
             (vE1.NextVertex.WorkPoint2.Y < lpart.ExtentsArray[idxa].YMin) then
          begin
            break ;
          end ;

          if (vE1.WorkPoint2.Y > lpart.ExtentsArray[idxa].YMax) and
             (vE1.NextVertex.WorkPoint2.Y > lpart.ExtentsArray[idxa].YMax) then
          begin
            break ;
          end ;

          if (vE1.WorkPoint2.X <> vE2.NextVertex.WorkPoint2.X) or
             (vE1.WorkPoint2.Y <> vE2.NextVertex.WorkPoint2.Y) then
          begin
            if getEdgeCrossing(vE1, vE2, pt) then begin
              Result := True ;
              d1 := GisPoint2Point(pt, vE1.WorkPoint2) ;
              d2 := GisPoint2Point(pt, vE1.NextVertex.WorkPoint2) ;
              if d1 <= d2 then begin
                if d1 < gridStep then
                  pt := vE1.WorkPoint2 ;
              end
              else begin
                if d2 < gridStep then
                  pt := vE1.NextVertex.WorkPoint2 ;
              end ;
              if (vE1.WorkPoint2.X = pt.X) and
                 (vE1.WorkPoint2.Y = pt.Y)
              then begin
                vE1.IsCrossVertex := True ;
                cv1 := vE1 ;
              end
              else
              if (vE1.NextVertex.WorkPoint2.X = pt.X) and
                 (vE1.NextVertex.WorkPoint2.Y = pt.Y) then begin
                vE1.NextVertex.IsCrossVertex := True ;
                cv1 := vE1.NextVertex ;
              end
              else begin
                vertex := T_Vertex.Create ;
                vertex.IsCrossVertex := True ;
                cv1 := vertex ;
                vertex.NativePoint2 := _TGIS_Point( pt ) ;
                vertex.WorkPoint2   := _TGIS_Point( pt ) ;

                vertex.NativePoint3 := GisPoint3DFrom2D(pt) ;
                vertex.WorkPoint3   := _TGIS_Point3D( vertex.NativePoint3 ) ;

                vertex.Owner       := self ;
                vertex.PrevVertex  := vE1 ;
                vertex.NextVertex  := vE1.NextVertex ;
                vE1.NextVertex.PrevVertex := vertex ;
                vE1.NextVertex := vertex ;
                lpart.VerticesList.Insert(idx1, vertex) ;
                inc( idx2 ) ;
              end ;

              if (vE2.WorkPoint2.X = pt.X) and
                 (vE2.WorkPoint2.Y = pt.Y)
              then begin
                vE2.IsCrossVertex := True ;
                cv2 := vE2 ;
              end
              else
              if (vE2.NextVertex.WorkPoint2.X = pt.X) and
                 (vE2.NextVertex.WorkPoint2.Y = pt.Y) then begin
                vE2.NextVertex.IsCrossVertex := True ;
                cv2 := vE2.NextVertex ;
              end
              else begin
                vertex := T_Vertex.Create ;
                vertex.IsCrossVertex := True ;
                cv2 := vertex ;
                vertex.NativePoint2 := _TGIS_Point( pt ) ;
                vertex.WorkPoint2   := _TGIS_Point( pt ) ;

                vertex.NativePoint3 := GisPoint3DFrom2D(pt) ;
                vertex.WorkPoint3   := _TGIS_Point3D( vertex.NativePoint3 ) ;

                vertex.Owner       := self ;
                vertex.PrevVertex  := vE2 ;
                vertex.NextVertex  := vE2.NextVertex ;
                vE2.NextVertex.PrevVertex := vertex ;
                vE2.NextVertex := vertex ;
                if vertex.NextVertex = fv then
                  lpart.VerticesList.Add(vertex)
                else begin
                  lpart.VerticesList.Insert(idx2, vertex) ;
                end ;
              end ;
              cv1.CommonVertex := cv2 ;
              cv2.CommonVertex := cv1 ;
              exit ;
            end
            else
            if GisLine2Point(vE2.WorkPoint2, vE2.NextVertex.WorkPoint2,
                    vE1.WorkPoint2)
                    <= gridStep
            then begin
              Result := True ;
              vertex := T_Vertex.Create ;
              vertex.IsCrossVertex := True ;
              vertex.NativePoint2 := _TGIS_Point( vE1.WorkPoint2 ) ;
              vertex.WorkPoint2   := _TGIS_Point( vE1.WorkPoint2 ) ;

              vertex.NativePoint3 := GisPoint3DFrom2D(vE1.WorkPoint2) ;
              vertex.WorkPoint3   := _TGIS_Point3D( vertex.NativePoint3 ) ;

              vertex.Owner       := self ;
              vertex.PrevVertex  := vE2 ;
              vertex.NextVertex  := vE2.NextVertex ;
              vE2.NextVertex.PrevVertex := vertex ;
              vE2.NextVertex := vertex ;
              vertex.CommonVertex := vE1 ;
              vE1.CommonVertex := vertex ;
              lpart.VerticesList.Insert(idx2, vertex) ;
              exit ;
            end;
          end ;
          vE2 := vE2.NextVertex ;
          inc( idx2 ) ;
          inc( idxa ) ;
        until idx2 > lpart.VerticesList.Count ;
        inc( idx1 ) ;
        vE1 := vE1.NextVertex ;
        vE2 := vE1.NextVertex ;

        if vE2.NextVertex = fvf then
          break ;
        if idx1 > lpart.VerticesList.Count - 1 then
          break ;
      end ;
    end ;
  begin ;
    Result := False ;

    i := 0 ;

    while True do begin
      if T_ContourPart(listOfParts[i]).VerticesList.Count > 3 then begin
        while findSelfCrossings(i) do begin
          npart := splitPart(i) ;

          if isClosed then begin
            T_ContourPart(listOfParts[i]).CheckVreticesDist(gridStep) ;
            T_ContourPart(listOfParts[i]).SetSignedArea2 ;
            if Abs(T_ContourPart(listOfParts[i]).SignedArea2) <= gridStep
            then begin
              {$IFNDEF NEXTGEN}
                FreeObjectNotNil( listOfParts[i] ) ;
              {$ENDIF}
              {$IFDEF OXYGENE}
                listOfParts.RemoveAt( i );
              {$ELSE}
                listOfParts.Delete( i );
              {$ENDIF}
              dec( FPartsCount ) ;
            end
            else
              T_ContourPart(listOfParts[i]).SetExtents(gridStep) ;

          end ;
          if isClosed then begin
            npart.CheckVreticesDist(gridStep) ;
            npart.SetSignedArea2 ;
          end ;

          if (not isClosed) or
             (isClosed and (Abs(npart.SignedArea2) > gridStep))
          then begin
            listOfParts.Add(npart);
            inc( FPartsCount ) ;
          end
          else
            FreeObject( npart ) ;

          Result := True ;
          if i >= FPartsCount then
            break ;
        end ;
      end ;
      inc( i ) ;
      if i >= FPartsCount then
        break ;
    end ;
  end ;

  function  T_Contour.getEdgeCrossing(
    const _vE1         : T_Vertex ;
    const _vE2         : T_Vertex ;
    var   _cp          : TGIS_Point
  ) : Boolean ;
  var
    r, s, m   : Double;
    svE1, svE2 : T_Vertex ;
    at : Double ;
    b1, b2, t1, t2 : TGIS_Point ;
    xmax, xmin, ymax, ymin : Double ;
  begin
    Result := False ;

    svE1 := _vE1.NextVertex ;
    svE2 := _vE2.NextVertex ;

    b1 := _vE1.NativePoint2 ;
    b2 := svE1.NativePoint2 ;

    if (Abs(b1.X -b2.X) < gridStep) and  (Abs(b1.Y -b2.Y) < gridStep) then
      exit ;

    t1 := _vE2.NativePoint2 ;
    t2 := svE2.NativePoint2 ;

    if (Abs(t1.X -t2.X) < gridStep) and  (Abs(t1.Y -t2.Y) < gridStep) then
      exit ;

    m := (b2.X-b1.X)*(t2.Y-t1.Y)
       - (b2.Y-b1.Y)*(t2.X-t1.X);
    if m <> 0 then
    begin
      r := ((b1.Y-t1.Y)*(t2.X-t1.X)
           - (b1.X-t1.X)*(t2.Y-t1.Y))/m;

      if (r>=0) and (r<=1) then begin

        s := ((b1.Y-t1.Y)*(b2.X-b1.X)
             - (b1.X-t1.X)*(b2.Y-b1.Y))/m;
        if (s>=0) and (s<=1) then begin
          {$IFDEF GIS_NORECORDS}
            _cp := new TGIS_Point;
          {$ENDIF}
          _cp.X := b1.X + r *(b2.X - b1.X);
          _cp.Y := b1.Y + r *(b2.Y - b1.Y);
          Result := True ;
          exit ;
        end ;
      end ;
    end ;

    if isClosed then begin
    //overlaid edges
      at := Abs((b2.X -b1.X)*(t1.Y -b1.Y) -(b2.Y -b1.Y)*(t1.X -b1.X))*4 ;
      if at < gridStep then begin
        at := Abs((b2.X -b1.X)*(t2.Y -b1.Y) -(b2.Y -b1.Y)*(t2.X -b1.X))*4 ;

        if at < gridStep  then begin
          if t1.X <= t2.X then begin
            xmin := t1.X +gridStep ;
            xmax := t2.X -gridStep
          end
          else begin
            xmin := t2.X +gridStep ;
            xmax := t1.X -gridStep
          end ;

          if t1.Y <= t2.Y then begin
            ymin := t1.Y +gridStep ;
            ymax := t2.Y -gridStep
          end
          else begin
            ymin := t2.Y +gridStep ;
            ymax := t1.Y -gridStep
          end ;

          if (b1.X  > xmin) and (b1.X  < xmax) then begin
            _cp := b1 ;
            Result := True ;
            exit ;
          end ;

          if (b1.Y  > ymin) and (b1.Y  < ymax) then begin
            _cp := b1 ;
            Result := True ;
            exit ;
          end ;

          if (b2.X  > xmin) and (b2.X  < xmax) then begin
            _cp := b2 ;
            Result := True ;
            exit ;
          end ;

          if (b2.Y  > ymin) and (b2.Y  < ymax) then begin
            _cp := b2 ;
            Result := True ;
            exit ;
          end ;
        end ;
      end ;
    end ;
  end ;

  // Reinitialize contour by deleting any site information.
  procedure T_Contour.Reinitialize ;
  var
    p, i   : Integer ;
  begin
    if listOfParts <> nil then begin
      for p := 0 to FPartsCount -1 do begin
        FPartVertices := T_ContourPart( listOfParts[p] ) ;
        if (FPartVertices <> nil) and (FPartVertices.VerticesList<> nil) then begin
          for i := 0 to FPartVertices.VerticesList.Count -1 do begin
            T_Vertex( FPartVertices.VerticesList[i] ).IsCrossVertex := False ;
            T_Vertex( FPartVertices.VerticesList[i] ).PrevEdgeSite := T_EdgeSite.gesUndefined ;
            T_Vertex( FPartVertices.VerticesList[i] ).NextEdgeSite := T_EdgeSite.gesUndefined ;
            T_Vertex( FPartVertices.VerticesList[i] ).CommonVertex := nil ;

            T_Vertex( FPartVertices.VerticesList[i] ).DescriptorsList := nil ;
            T_Vertex( FPartVertices.VerticesList[i] ).PrevDescriptor := nil ;
            T_Vertex( FPartVertices.VerticesList[i] ).NextDescriptor := nil ;
            T_Vertex( FPartVertices.VerticesList[i] ).PrevEdge := nil ;
            T_Vertex( FPartVertices.VerticesList[i] ).NextEdge := nil ;
          end ;
        end ;
      end ;
    end ;

  end ;

  // Add a new part to the contour.
  procedure T_Contour.AddPart(
    const _part : T_ContourPart
  ) ;
  begin
    listOfParts.Add(_part) ;
    inc( FPartsCount ) ;
    if listOfParts.Count = 1 then
      FPartVertices := _part ;
  end ;

  // Deletes part from the contour.
  procedure T_Contour.DeletePart(
    const _partNo : Integer
  ) ;
  begin
    {$IFDEF OXYGENE}
      listOfParts.RemoveAt(_partNo) ;
    {$ELSE}
      listOfParts.Delete(_partNo) ;
    {$ENDIF}
    dec( FPartsCount ) ;
  end ;


  // Snap the NativePoint of vertex to the contour based on a level of
  // tolerance.
  // _vertex    vertex to be snapped
  // _tolerance tolerance within which point can be snapped
  // return     True if  the point was snapped
  function T_Contour.SnapToContour(
    const _vertex    : T_Vertex ;
    const _tolerance : Double
  ) : Boolean ;
  var
    i          : Integer       ;
    ii         : Integer       ;
    line       : TGIS_Line     ;
    line3D     : TGIS_Line3D   ;
    vi,
    vni        : T_Vertex      ;
    testextent : TGIS_Extent   ;
    a_dist,
    b_dist     : Double        ;
    l_dist     : Double        ;
    corr       : Double        ;
    ab_dist    : Double        ;
    ilist      : T_ContourPart ;
    dist       : Double        ;
  begin

    Result := False ;

    testextent.XMin := _vertex.NativePoint2.X - _tolerance ;
    testextent.YMin := _vertex.NativePoint2.Y - _tolerance ;
    testextent.XMax := _vertex.NativePoint2.X + _tolerance ;
    testextent.YMax := _vertex.NativePoint2.Y + _tolerance ;

    for ii := 0 to listOfParts.Count -1 do begin

      ilist := listOfParts[ii] ;

      vi := T_Vertex( ilist.VerticesList[0] ) ;
      vni := vi.NextVertex ;

      for i := 0 to ilist.VerticesList.Count -1 do begin

        if i > 0 then
        begin
          vi := vni ;
          vni := vi.NextVertex ;
          if vi = vni then break ;
        end ;

        line.A   := _TGIS_Point(vi.NativePoint2) ;
        line.B   := _TGIS_Point(vni.NativePoint2) ;
        line3D.A := _TGIS_Point3D(vi.NativePoint3) ;
        line3D.B := _TGIS_Point3D(vni.NativePoint3) ;

        if (line.A.X < testextent.XMin) and
           (line.B.X < testextent.XMin) then
          continue ;

        if (line.A.X > testextent.XMax) and
           (line.B.X > testextent.XMax) then
          continue ;

        if (line.A.Y < testextent.YMin) and
           (line.B.Y < testextent.YMin) then
          continue ;

        if (line.A.Y > testextent.YMax) and
           (line.B.Y > testextent.YMax) then
          continue ;

        a_dist := GisPoint2Point(line.A, _vertex.NativePoint2) ;
        if a_dist = 0 then begin
          _vertex.WorkPoint2 := _TGIS_Point(_vertex.NativePoint2) ;
          _vertex.WorkPoint3 := _TGIS_Point3D(_vertex.NativePoint3) ;
          Result := True ;
          exit ;
        end ;

        b_dist := GisPoint2Point(line.B, _vertex.NativePoint2) ;
        if b_dist = 0 then begin
          _vertex.WorkPoint2 := _TGIS_Point(_vertex.NativePoint2) ;
          _vertex.WorkPoint3 := _TGIS_Point3D(_vertex.NativePoint3) ;
          Result := True ;
          exit ;
        end ;

        l_dist := GisLine2Point(line.A, line.B, _vertex.NativePoint2) ;
        if (l_dist <= _tolerance) then begin
          Result := True ;
          dist := GisPoint2Point(_vertex.WorkPoint2, _vertex.NativePoint2) ;

          if (dist = 0) or (l_dist < dist) then begin
            if (l_dist < a_dist) and (l_dist < b_dist) then begin
              corr := Sqrt( Sqr(a_dist) -Sqr(l_dist) ) ;
              ab_dist := GisPoint2Point(line.A, line.B) ;
              _vertex.WorkPoint2.X := line.A.X +
                                     ( line.B.X -line.A.X ) * corr / ab_dist ;
              _vertex.WorkPoint2.Y := line.A.Y +
                                     ( line.B.Y -line.A.Y ) * corr / ab_dist ;
              _vertex.WorkPoint3   := GisPoint3DFrom2D(_vertex.WorkPoint2) ;
              _vertex.WorkPoint3.Z := _vertex.NativePoint3.Z ;
              _vertex.WorkPoint3.M := _vertex.NativePoint3.M ;
            end
            else
            begin
              if Abs(l_dist - a_dist) < Abs(l_dist - b_dist) then begin
                _vertex.WorkPoint2   := _TGIS_Point(line.A) ;
                _vertex.WorkPoint3   := GisPoint3DFrom2D(_vertex.WorkPoint2) ;
                _vertex.WorkPoint3.Z := _vertex.NativePoint3.Z ;
                _vertex.WorkPoint3.M := _vertex.NativePoint3.M ;
              end
              else begin
                _vertex.WorkPoint2   := _TGIS_Point(line.B) ;
                _vertex.WorkPoint3   := GisPoint3DFrom2D(_vertex.WorkPoint2) ;
                _vertex.WorkPoint3.Z := _vertex.NativePoint3.Z ;
                _vertex.WorkPoint3.M := _vertex.NativePoint3.M ;
              end
            end ;
          end ;
        end ;

      end ; //end of for i loop
    end ;  //end of for ii loop

  end ;

  // Join current and provide contour
  // _contour contour to be joined
  // return   common (joined) contour
  function T_Contour.JoinContour(
    const _contour : T_Contour
  ) : T_Contour ;
  var
    i : Integer ;
  begin
    Result := T_Contour.Create ;
    for i := 0 to FPartsCount -1 do begin
      Result.AddPart(listOfParts[i]);
    end ;

    for i := 0 to _contour.PartsCount -1 do begin
      _contour.Part := i ;
      Result.AddPart(_contour.PartVertices);
    end ;
  end ;

  // Test if a point is inside a contour.
  // _ptg    point to be checked
  // return  if True, then id point is inside
  function T_Contour.IsPointInsideContour(
    const _ptg  : TGIS_Point
  ) : Boolean ;
  begin
    Result := IsPointInsideContour( _ptg, -1 ) ;
  end ;

{$IFDEF GIS_NOASM}
  // Test if a point is inside a contour.
  // _ptg    point to be checked
  // return  if True, then id point is inside
  function T_Contour.IsPointInsideContour(
    const _ptg  : TGIS_Point ;
    const _part : Integer
  ) : Boolean ;
  var
    point_no     : Integer  ;
    ncount       : Integer  ;
    vertices_cnt : Integer  ;
    npar         : Integer  ;
    vertex,
    nvertex      : T_Vertex ;
    line_Ax      : Double   ;
    line_Ay      : Double   ;
    line_Bx      : Double   ;
    line_By      : Double   ;
    start, stop  : Integer  ;
    pt           : TPoint   ;

    function map_to_screen( const _ptg : TGIS_Point ) : TPoint ;
    begin
      {$IFDEF GIS_NORECORDS}
        Result := TPoint.Create(0,0);
      {$ENDIF}
      Result.X := RoundS( ( _ptg.X - bmpOrigin.X ) * bmpGridScale ) + bmpGridPos.X ;
      Result.Y := RoundS( ( bmpOrigin.Y - _ptg.Y ) * bmpGridScale ) + bmpGridPos.Y ;
    end ;
  begin
    Result := False ;

    if (_ptg.Y > FExtent.YMax) or (_ptg.X > FExtent.XMax) or
       (_ptg.Y < FExtent.YMin) or (_ptg.X < FExtent.XMin) then
      exit ;

    // do bitmap grid guessing
    if bmpGridScale <> 0 then begin

      pt := map_to_screen( _ptg ) ;

      if ( pt.Y >= BMP_SIZE ) or ( pt.X >= BMP_SIZE ) or
         ( pt.Y <  0        ) or ( pt.X <  0        )
      then
        exit ;

      case bmpGrid[ pt.Y * BMP_SIZE + pt.X ] and $00ffffff of
        BMP_OUTSIDE : begin
                        exit ;
                      end ;
        BMP_INSIDE  : begin
                        Result := True ;
                        exit ;
                      end
        else          begin
                        // unsure position - continue with deep computation
                      end ;
      end ;
    end ;

    if _part = -1 then begin
      start := 0 ;
      stop  := FPartsCount -1 ;
    end
    else begin
      start := _part ;
      stop  := _part ;
    end ;

    for ncount := start to stop do begin

      if T_ContourPart(listOfParts[ncount]).Extent.XMin >
         _ptg.X + gridStep005
      then
        continue ;

      if T_ContourPart(listOfParts[ncount]).Extent.XMax <
         _ptg.X - gridStep005
      then
        continue ;

      if T_ContourPart(listOfParts[ncount]).Extent.YMin >
         _ptg.Y + gridStep005
      then
        continue ;

      vertices_cnt := T_ContourPart(
                        listOfParts[ncount]
                      ).VerticesList.Count ;
      npar := 0 ;
      vertex := T_ContourPart(
                  listOfParts[ncount]
                ).VerticesList[0] ;
      nvertex := vertex.NextVertex ;

      for point_no := 0 to vertices_cnt - 1 do  begin // all points
        // point will be tested on every line of polygon
        // test is based on odd/even algorithm
        if point_no > 0 then begin
          vertex := nvertex ;
          nvertex := vertex.NextVertex ;
        end ;

        line_Ax := vertex.WorkPoint2.X ;
        line_Ay := vertex.WorkPoint2.Y ;
        line_Bx := nvertex.WorkPoint2.X ;
        line_By := nvertex.WorkPoint2.Y ;
          try
            if ( ( ( ( line_Ay <= _ptg.Y ) and ( _ptg.Y < line_By ) ) or
                   ( ( line_By <= _ptg.Y ) and ( _ptg.Y < line_Ay ) )
                 ) and
                 ( _ptg.X < ( line_Bx - line_Ax )*( _ptg.Y - line_Ay ) /
                            ( line_By - line_Ay ) + line_Ax
                 )
               ) then
              npar := npar +1;
          except
        end ;
      end ;

      if ( npar mod 2) = 1 then
        Result := not Result ; // even - point is inside part
    end ;
  end ;
{$ELSE}
  // Test if a point is inside a contour.
  // _ptg   point to be checked
  // return if True, then id point is inside
  function T_Contour.IsPointInsideContour(
    const _ptg  : TGIS_Point ;
    const _part : Integer
  ) : Boolean ;
  label
    l0, l1, l2, leof, lend, lend1, lpoint, lloop;
  var
    ncount       : Integer  ;
    vertices_cnt : Integer  ;
    vertex,
    nvertex      : T_Vertex ;
    pt           : TPoint   ;
    start, stop  : Integer  ;

    function map_to_screen( const _ptg : TGIS_Point ) : TPoint ;
    begin
      Result.X := RoundS( ( _ptg.X - bmpOrigin.X ) * bmpGridScale ) + bmpGridPos.X ;
      Result.Y := RoundS( ( bmpOrigin.Y - _ptg.Y ) * bmpGridScale ) + bmpGridPos.Y ;
    end ;

  begin
    Result := False ;

    if (_ptg.Y > FExtent.YMax) or (_ptg.X > FExtent.XMax) or
       (_ptg.Y < FExtent.YMin) or (_ptg.X < FExtent.XMin) then
      exit ;

    // do bitmap grid guessing
    if   bmpGridScale <> 0  then begin

      pt := map_to_screen( _ptg ) ;

      if ( pt.Y >= BMP_SIZE ) or ( pt.X >= BMP_SIZE ) or
         ( pt.Y <  0        ) or ( pt.X <  0        )
      then
        exit ;

      case bmpGrid[ pt.Y * BMP_SIZE + pt.X ] and $00ffffff of
        BMP_OUTSIDE : begin
                        exit ;
                      end ;
        BMP_INSIDE  : begin
                        Result := True ;
                        exit ;
                      end ;
        else          begin
                        // unsure position - continue with deep computation
                      end ;
      end ;
    end ;

    if _part = -1 then begin
      start := 0 ;
      stop  := FPartsCount -1 ;
    end
    else begin
      start := _part ;
      stop  := _part ;
    end ;

    for ncount := start to stop do begin
      vertices_cnt := T_ContourPart(listOfParts[ncount]).VerticesList.Count ;
      vertex := T_ContourPart(listOfParts[ncount]).VerticesList[0] ;
      nvertex := vertex.NextVertex ;

      (*
      Author: Humberto Aicardi
      Contact: humberto@freenet.com.br
      Date: 2003/06/17 (yyyy/mm/dd)
      Description: Convert the code to assembler to gain performance.
      Results: A 80% time reduction could be seen, with 50% to 60% a average.
        The result is also dependent on the number of vertices and the way they
        are placed on the polygon.
      *)
      asm
        push    edi
        push    esi
        push    ebx

        mov     esi,[vertex]
        mov     edx,[nvertex]
        mov     ebx,[_ptg]

        xor     edi,edi
        xor     ecx,ecx

        fld     qword [ebx+offset TGIS_Point.Y];

      lloop:
        cmp     ecx,0
        jz      lpoint

        mov     esi,edx
        mov     edx,[esi + offset T_Vertex.NextVertex ]

      lpoint:
        fld     qword [esi+offset T_Vertex.Workpoint2.Y];

        fcom    st(1)
        fnstsw  ax
        test    ah,$41
        je      l0

        fld     st(1)
        fcomp   qword [edx+offset T_Vertex.Workpoint2.Y]
        fnstsw  ax
        test    ah,1
        jne     l1
        jmp     l2
      l0:
        fld     qword [edx+offset T_Vertex.Workpoint2.Y]
        fcomp   st(2)
        fnstsw  ax
        test    ah,$41
        je      leof
      l2:
        fld     st(1)
        fcomp   st(1)
        fnstsw  ax
        test    ah,1
        je      leof
      l1:
        fld     qword [edx+offset T_Vertex.Workpoint2.X]
        fsub    qword [esi+offset T_Vertex.Workpoint2.X]
        fld     st(2)
        fsub    st,st(2)
        fmulp   st(1),st
        fld     qword [edx+offset T_Vertex.Workpoint2.Y]
        fsub    st,st(2)
        fdivp   st(1),st
        fadd    qword [esi+offset T_Vertex.Workpoint2.X]
        fld     qword [ebx+offset TGIS_Point.X]
        fcompp
        fnstsw  ax
        test    ah,1
        fstp    st(0)
        je      lend
        inc     edi
        jmp     lend
      leof:
        fstp    st(0);
      lend:
        inc     ecx
        cmp     ecx,[vertices_cnt]
        jb      lloop
        and     edi,$01
        jz      lend1
        xor     byte ptr [result],1
      lend1:
        fstp    st(0);

        pop     ebx
        pop     esi
        pop     edi
      end ;
    end ;
  end ;
{$ENDIF}

  // Test if a point is inside a contour.
  // _ptg    point to be checked
  // return  if True, then id point is inside
  function T_Contour.IsPointOnContour(
    const _ptg  : TGIS_Point
  ) : Boolean ;
  begin
    Result := IsPointOnContour( _ptg, -1 ) ;
  end ;

  // Test if a point is inside a contour.
  //_ptg    point to be checked
  // return if True, then id point is inside
  function T_Contour.IsPointOnContour(
    const _ptg  : TGIS_Point ;
    const _part : Integer
  ) : Boolean ;
  var
    point_no     : Integer  ;
    ncount       : Integer  ;
    vertices_cnt : Integer  ;
    vertex,
    nvertex      : T_Vertex ;
    start, stop  : Integer  ;
    pt           : TPoint   ;

    function map_to_screen( const _ptg : TGIS_Point ) : TPoint ;
    begin
      {$IFDEF GIS_NORECORDS}
        Result := TPoint.Create(0,0);
      {$ENDIF}
      Result.X := RoundS( ( _ptg.X - bmpOrigin.X ) * bmpGridScale ) + bmpGridPos.X ;
      Result.Y := RoundS( ( bmpOrigin.Y - _ptg.Y ) * bmpGridScale ) + bmpGridPos.Y ;
    end ;

  begin
    Result := False ;

    if (_ptg.Y > FExtent.YMax) or (_ptg.X > FExtent.XMax) or
       (_ptg.Y < FExtent.YMin) or (_ptg.X < FExtent.XMin) then
      exit ;
    // do bitmap grid guessing
    if bmpGridScale <> 0 then begin
      pt := map_to_screen(_ptg) ;
      if ( pt.Y >= BMP_SIZE ) or ( pt.X >= BMP_SIZE ) or
         ( pt.Y <  0        ) or ( pt.X <  0        )
      then
        exit ;

      case bmpGrid[ pt.Y * BMP_SIZE + pt.X ] and $00ffffff of
        BMP_EDGE    : begin
                        // unsure position - continue with deep computation
                      end ;
        else          begin
                        // for sure not on edge
                        exit ;
                      end ;
      end ;
    end ;

    if _part = -1 then begin
      start := 0 ;
      stop  := FPartsCount -1 ;
    end
    else begin
      start := _part ;
      stop  := _part ;
    end ;

    for ncount := start to stop do begin

      if T_ContourPart(listOfParts[ncount]).Extent.XMin >
         _ptg.X + gridStep005
      then
        continue ;

      if T_ContourPart(listOfParts[ncount]).Extent.XMax <
         _ptg.X - gridStep005
      then
        continue ;

      if T_ContourPart(listOfParts[ncount]).Extent.YMin >
         _ptg.Y + gridStep005
      then
        continue ;

      if T_ContourPart(listOfParts[ncount]).Extent.YMax <
         _ptg.Y - gridStep005
      then
        continue ;

      vertices_cnt := T_ContourPart(listOfParts[ncount]).VerticesList.Count ;
      vertex := T_ContourPart(listOfParts[ncount]).VerticesList[0] ;
      nvertex := vertex.NextVertex ;
      for point_no := 0 to vertices_cnt - 1 do  begin // all points
        if point_no > 0 then begin
          vertex := nvertex ;
          nvertex := vertex.NextVertex ;
          if vertex = nvertex then
            break ;
        end ;

        if GisLine2Point(vertex.WorkPoint2, nvertex.WorkPoint2, _ptg)
          <= gridStep005
        then begin
          Result := True ;
          exit ;
        end ;
      end ;
    end ;
  end ;

  // Compute the location of each edge related to the _nextcotour.
  // _nextcontour related contour
  procedure T_Contour.DefineEdgesSite(
    const _nextcontour : TObject
  ) ;
  var
    i, k                : Integer    ;
    last_edge_site      : T_EdgeSite {$IFDEF JAVA} := T_EdgeSite.gesUndefined {$ENDIF};
    vertex              : T_Vertex   ;
    nvertex             : T_Vertex   ;
    nnvertex            : T_Vertex   ;
    cvertex             : T_Vertex   ;
    testpoint           : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
    short_path          : Boolean    ;
  begin

    FEdgesNo        := 0 ;
    FSharedEdgesNo  := 0 ;
    FInsideEdgesNo  := 0 ;
    FOutsideEdgesNo := 0 ;

    isSharedCD := False ;
    isSharedSD := False ;

    FIsEdgeInside  := False ;
    FIsEdgeOutside := False ;
    FIsEdgeShared  := False ;

    for k := 0 to self.PartsCount -1 do begin
      self.Part := k ;

      vertex := self.PartVertices.VerticesList[0] ;
      nvertex := vertex.NextVertex ;

      for i := 0 to self.PartVertices.VerticesList.Count -1 do begin

        if i > 0 then begin
          vertex := nvertex ;
          nvertex := vertex.NextVertex ;
          if nvertex = vertex then break ;
        end ;

        if vertex.NextEdgeSite = T_EdgeSite.gesUndefined then begin
          short_path := False ;

          if vertex.IsCrossVertex and nvertex.IsCrossVertex then begin
            cvertex := vertex.find_sharedSD_vertex ;
            if assigned( cvertex ) then begin
              vertex.NextEdgeSite := T_EdgeSite.gesSharedSD ;
              nvertex.PrevEdgeSite := T_EdgeSite.gesSharedSD ;
              cvertex.NextEdgeSite := T_EdgeSite.gesSharedSD ;
              cvertex.NextVertex.PrevEdgeSite := T_EdgeSite.gesSharedSD ;
              short_path := True ;
            end
            else begin
              cvertex := vertex.find_sharedCD_vertex ;
              if assigned( cvertex ) then begin
                vertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
                nvertex.PrevEdgeSite := T_EdgeSite.gesSharedCD ;
                cvertex.PrevEdgeSite := T_EdgeSite.gesSharedCD ;
                cvertex.PrevVertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
                short_path := True ;
              end ;
            end ;
          end ;

          if _nextcontour = nil then
            short_path := True ;

          if not short_path then begin

            if (not vertex.IsCrossVertex) and
               (vertex.PrevEdgeSite <> T_EdgeSite.gesUndefined)
            then
                last_edge_site := vertex.PrevEdgeSite
            else
            begin
              if nvertex.CommonVertex = nil then begin

                nnvertex := nvertex.NextVertex ;

                if nnvertex.CommonVertex = nil then begin
                  testpoint.X := nnvertex.WorkPoint2.X ;
                  testpoint.Y := nnvertex.WorkPoint2.Y ;
                end
                else begin
                  testpoint.X := nvertex.WorkPoint2.X ;
                  testpoint.Y := nvertex.WorkPoint2.Y ;
                end ;
              end
              else begin

                testpoint.X := ( vertex.WorkPoint2.X + nvertex.WorkPoint2.X ) / 2 ;
                testpoint.Y := ( vertex.WorkPoint2.Y + nvertex.WorkPoint2.Y ) / 2 ;
              end ;

              if T_Contour( _nextcontour ).IsPointInsideContour( testpoint )
              then
                last_edge_site := T_EdgeSite.gesInside
              else
                last_edge_site := T_EdgeSite.gesOutside ;

            end ;

            vertex.NextEdgeSite := last_edge_site ;
            nvertex.PrevEdgeSite := last_edge_site ;
          end ;
        end ;

        case vertex.NextEdgeSite of
          T_EdgeSite.gesInside :
            begin ;
              self.FIsEdgeInside := True  ;
              inc( self.FInsideEdgesNo ) ;
            end ;
          T_EdgeSite.gesOutside :
            begin
              self.FIsEdgeOutside := True ;
              inc( self.FOutsideEdgesNo ) ;
            end ;
          T_EdgeSite.gesSharedCD :
            begin
              isSharedCD := True ;
              self.FIsEdgeShared := True  ;
              inc( self.FSharedEdgesNo ) ;
            end ;
          T_EdgeSite.gesSharedSD :
            begin
              isSharedSD := True ;
              self.FIsEdgeShared := True  ;
              inc( self.FSharedEdgesNo ) ;
            end ;
        end ;

        inc( self.FEdgesNo ) ;

      end ;
    end ;

  end ;

  // Compute the number of all edge in the contour.
  function T_Contour.GetEdgesNumber : Integer  ;
  var
    i : Integer ;
    cor : Integer ;
  begin

    if FEdgesNo > 0 then begin
      Result := FEdgesNo ;
      exit ;
    end ;

    Result := 0 ;
    if isClosed then
      cor := 0
    else
      cor := 1 ;

    for i := 0 to FPartsCount -1 do begin
      Result := Result +
                T_ContourPart(listOfParts[i]).VerticesList.Count -cor;
    end ;
    FEdgesNo := Result ;
  end ;

//==============================================================================
// T_ContourPart
//==============================================================================

  // Create an instance.
  constructor T_ContourPart.Create ;
  begin
    inherited Create ;

    SignedArea2 := 0 ;
    ParentPart := nil ;
    VerticesList := TList<T_Vertex>.Create ;
    {$IFDEF GIS_NORECORDS}
      Extent := new TGIS_Extent ;
    {$ENDIF}
  end ;

  // Destroy an instance.
  procedure T_ContourPart.doDestroy ;
  var
    i   : Integer  ;
  begin
    if assigned( VerticesList ) then begin
      for i := VerticesList.Count -1 downto 0 do begin
        {$IFNDEF NEXTGEN}
          FreeObjectNotNil(VerticesList[i]) ;
        {$ENDIF}
      end ;
      FreeObject( VerticesList );
    end ;
    inherited ;
  end ;

  // Checking crossing pass type.
  // _partvertex  first vertex from tested contour part
  // _crossvertex second vertex which describes pass
  // return       type of crossing pass
  function  T_ContourPart.checkCrossingPassType(
    const _partvertex : T_Vertex ;
    const _linevertex : T_Vertex
  ) : T_CrossingPassType ;
  var
    a_p_pp_cp, a_p_pp_cn, a_p_pn_cp, a_p_pn_cn : Double ;
    p, pp, pn : TGIS_Point ;
    cp, cn : TGIS_Point ;
    case_key : Integer ;
    test_next, test_prev : Boolean ;

    function _area2( const _a, _b, _c : TGIS_Point) : Double ;
    begin
      Result := (_b.X -_a.X) * (_c.Y -_a.Y) - (_c.X -_a.X) * (_b.Y -_a.Y) ;
    end ;

    function _normalize_point(const _p : TGIS_Point) : TGIS_Point ;
    var
      fact : Double ;
      pt : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    begin
      pt.X := _p.X - p.X ;
      pt.Y := _p.Y - p.Y ;
      fact := 1/Sqrt((Sqr(pt.X) + Sqr(pt.Y))) ;
      Result.X := pt.X * fact ;
      Result.Y := pt.Y * fact ;
    end ;

  begin
    Result := T_CrossingPassType.gcpUndefined ;
    p  := _TGIS_Point (_partvertex.WorkPoint2) ;
    pp := _normalize_point(_partvertex.PrevVertex.WorkPoint2) ;
    pn := _normalize_point(_partvertex.NextVertex.WorkPoint2) ;
    if _linevertex.PrevVertex <> _linevertex then begin
      cp := _normalize_point(_linevertex.PrevVertex.WorkPoint2) ;
      test_prev := True ;
    end
    else
      test_prev := False ;

    if _linevertex.NextVertex <> _linevertex then begin
      cn := _normalize_point(_linevertex.NextVertex.WorkPoint2) ;
      test_next := True ;
    end
    else
      test_next := False ;

    p.X := 0 ;
    p.Y := 0 ;

    if test_prev then begin
      a_p_pp_cp := _area2(p, pp, cp) ;
      a_p_pn_cp := _area2(p, pn, cp) ;
    end
    else begin
      a_p_pp_cp := 0 ;
      a_p_pn_cp := 0 ;
    end ;

    if test_next then begin
      a_p_pp_cn := _area2(p, pp, cn) ;
      a_p_pn_cn := _area2(p, pn, cn) ;
    end
    else begin
      a_p_pp_cn := 0 ;
      a_p_pn_cn := 0 ;
    end ;

    //a_p_pp_cp
    if a_p_pp_cp > 0 then begin
      case_key := 3 ;
    end
    else
    if a_p_pp_cp < 0 then begin
      case_key := 2 ;
    end
    else begin // = 0
      case_key := 1 ;
    end ;

    //a_p_pp_cn
    if a_p_pp_cn > 0 then begin
      case_key := case_key + 10*3 ;
    end
    else
    if a_p_pp_cn < 0 then begin
      case_key := case_key + 10*2 ;
    end
    else begin // = 0
      case_key := case_key + 10*1 ;
    end ;

    //a_p_pn_cp
    if a_p_pn_cp > 0 then begin
      case_key := case_key + 100*3 ;
    end
    else
    if a_p_pn_cp < 0 then begin
      case_key := case_key + 100*2 ;
    end
    else begin // = 0
      case_key := case_key + 100*1 ;
    end ;

    if a_p_pn_cn > 0 then begin
      case_key := case_key + 1000*3 ;
    end
    else
    if a_p_pn_cn < 0 then begin
      case_key := case_key + 1000*2 ;
    end
    else begin // = 0
      case_key := case_key + 1000*1 ;
    end ;

    //a_p_pn_cn   a_p_pn_cp   a_p_pp_cn   a_p_pp_cp
    case case_key of
      2332 :
        begin
          if SignedArea2 > 0 then //ccw part (hole)
            Result := T_CrossingPassType.gcpOutsideInside
          else
            Result := T_CrossingPassType.gcpOutsideInside ;
        end ;
      3223 :
        begin
          if SignedArea2 > 0 then //ccw part (hole)
            Result := T_CrossingPassType.gcpOutsideInside
          else
            Result := T_CrossingPassType.gcpInsideOutside ;
        end ;
    end ;

  end ;

  // Clear an instance.
  procedure T_ContourPart.Clear ;
  var
    i : Integer ;
  begin
    SignedArea2 := 0 ;
    ParentPart := nil ;
    {$IFNDEF NEXTGEN}
      for i := VerticesList.Count -1 downto 0 do
        FreeObjectNotNil( T_Vertex(VerticesList[i]) ) ;
    {$ENDIF}
    VerticesList.Clear ;
    if assigned( ExtentsArray ) then begin
      ExtentsArray := nil ;
    end ;
  end ;

  // Set new vertices owner.
  procedure T_ContourPart.SetOwner(
    const _contour : T_Contour
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to VerticesList.Count -1 do
      T_Vertex(VerticesList[i]).Owner := _contour ;
  end ;

  // Set array of extents for all vericy.
  procedure T_ContourPart.SetExtents(
    const _gridstep : Double
  ) ;
  begin
    SetExtents( _gridstep, 0, True ) ;
  end ;

  // Set array of extents for all vericy.
  procedure T_ContourPart.SetExtents(
    const _gridstep : Double ;
    const _idx      : Integer
  ) ;
  begin
    SetExtents( _gridstep, _idx, True ) ;
  end ;

  // Set array of extents for all vericy.
  procedure T_ContourPart.SetExtents(
    const _gridstep : Double  ;
    const _idx      : Integer ;
    const _add      : Boolean
  ) ;
  var
    idx, lidx : Integer ;
    ext   : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    gext  : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    fv ,v : T_Vertex ;
    sidx  : Integer ;

    procedure reconstruct_table ;
    var
      n : Integer ;
    begin
      if _add then begin
        for n := idx downto sidx do
          ExtentsArray[n] := ExtentsArray[n -1] ;
      end
      else begin
        for n := sidx to idx do
          ExtentsArray[n ] := ExtentsArray[n +1] ;
      end ;
    end ;
  begin
    fv := T_Vertex(VerticesList[0]) ;
    v := T_Vertex(VerticesList[VerticesList.Count -1]) ;

    lidx := VerticesList.Count ;

    ext.XMin := v.WorkPoint2.X ;
    ext.XMax := v.WorkPoint2.X ;
    ext.YMin := v.WorkPoint2.Y ;
    ext.YMax := v.WorkPoint2.Y ;

    gext.XMin := ext.XMin - _gridstep ;
    gext.XMax := ext.XMax + _gridstep ;
    gext.YMin := ext.YMin - _gridstep ;
    gext.YMax := ext.YMax + _gridstep ;

    if v.NextVertex = fv then begin// closed contour
      if ext.XMin > fv.WorkPoint2.X then begin
        ext.XMin  := fv.WorkPoint2.X ;
        gext.XMin := ext.XMin -_gridstep;
      end
      else
      if ext.XMax < fv.WorkPoint2.X then begin
        ext.XMax := fv.WorkPoint2.X ;
        gext.XMax := ext.XMax +_gridstep ;
      end ;

      if ext.YMin > fv.WorkPoint2.Y then begin
        ext.YMin  := fv.WorkPoint2.Y ;
        gext.YMin := ext.YMin -_gridstep;
      end
      else
      if ext.YMax < fv.WorkPoint2.Y then begin
        ext.YMax := fv.WorkPoint2.Y ;
        gext.YMax := ext.YMax +_gridstep ;
      end ;

    end ;

    if length( ExtentsArray ) < lidx then begin
      SetLength(ExtentsArray, lidx +lidx div 2) ;
      sidx := 0 ;
    end
    else
      sidx := _idx ;

    idx := lidx -1 ;

    if sidx > 0 then begin
      reconstruct_table ;
      exit ;
    end ;

    ExtentsArray[idx] := _TGIS_Extent( gext ) ;
    dec( idx ) ;

    assert( idx >= 0 ) ;

    while idx >= 0 do begin
      v := v.PrevVertex ;

      if ext.XMin > v.WorkPoint2.X then begin
        ext.XMin  := v.WorkPoint2.X ;
        gext.XMin := ext.XMin -_gridstep;
      end
      else
      if ext.XMax < v.WorkPoint2.X then begin
        ext.XMax := v.WorkPoint2.X ;
        gext.XMax := ext.XMax +_gridstep ;
      end ;

      if ext.YMin > v.WorkPoint2.Y then begin
        ext.YMin  := v.WorkPoint2.Y ;
        gext.YMin := ext.YMin -_gridstep;
      end
      else
      if ext.YMax < v.WorkPoint2.Y then begin
        ext.YMax := v.WorkPoint2.Y ;
        gext.YMax := ext.YMax +_gridstep ;
      end ;
      ExtentsArray[idx] := _TGIS_Extent( gext ) ;
      dec( idx ) ;
    end ;
  end ;

  // Calculate doubled area of part (with sign - winding indicator).
  procedure T_ContourPart.SetSignedArea2 ;
  var
    i : Integer ;
    vertex, nvertex : T_Vertex ;
    xn, yn : Double ;
  begin
    SignedArea2 := 0 ;
    if VerticesList.Count < 3 then
      exit ;
    vertex := T_Vertex(VerticesList[0]) ;
    xn := vertex.NativePoint2.X ;
    yn := vertex.NativePoint2.Y ;
    nvertex := vertex.NextVertex ;
    for i := 0 to VerticesList.Count -1 do begin
      SignedArea2 := SignedArea2  +(vertex.NativePoint2.X -xn)*(nvertex.NativePoint2.Y -yn)
                -(vertex.NativePoint2.Y -yn)*(nvertex.NativePoint2.X -xn) ;
      vertex := nvertex ;
      nvertex := vertex.NextVertex ;

    end ;
  end ;

  // Deletes vertics wchich is to close to next.
  procedure T_ContourPart.CheckVreticesDist(
    const _gridstep : Double
  ) ;
  var
    i : Integer ;
    d : Double ;
    vertex, nvertex : T_Vertex ;
  begin
    i := 0 ;

    while i < VerticesList.Count do begin
      vertex := T_Vertex(VerticesList[i]) ;
      nvertex := vertex.NextVertex ;
      d := GisPoint2Point(vertex.NativePoint2, nvertex.NativePoint2) ;
      if d < _gridstep then begin
        vertex.PrevVertex.NextVertex := nvertex ;
        nvertex.PrevVertex := vertex.PrevVertex ;
        {$IFDEF OXYGENE}
          VerticesList.RemoveAt(i) ;
        {$ELSE}
          VerticesList.Delete(i) ;
        {$ENDIF}
        FreeObject(vertex) ;
        continue ;
      end ;
      inc(i) ;
    end ;
  end ;

//==============================================================================
// T_Vertex
//==============================================================================

  function T_Vertex.are_spliced(
    const _v : T_Vertex
  ) : Boolean ;
  var
    nv : T_Vertex ;
  begin
    Result := False ;
    if (CommonVertex = nil) or (_v.CommonVertex = nil) then
      exit ;
    nv := CommonVertex ;
    while True do begin
      if nv = _v then begin
        Result := True ;
        break ;
      end ;
      if nv = self then
        break ;
      nv := nv.CommonVertex ;
    end ;
  end ;

  function T_Vertex.find_sharedSD_vertex : T_Vertex ;
  var
    sv : T_Vertex ;
  begin
    Result := nil ;
    if CommonVertex = nil then
      exit ;
    if self.NextVertex = self then
      exit ;
    sv := CommonVertex ;
    while sv <> self do begin
      if (sv.Owner <> self.Owner) then begin
        if sv <> sv.NextVertex then begin
          if sv.NextVertex.are_spliced( self.NextVertex) then begin
            Result := sv ;
            exit ;
          end ;
        end ;
      end ;
      sv := sv.CommonVertex
    end ;
  end ;

  function T_Vertex.find_sharedCD_vertex : T_Vertex ;
  var
    sv : T_Vertex ;
  begin
    Result := nil ;
    if CommonVertex = nil then
      exit ;
    if self.NextVertex = self then
      exit ;
    sv := CommonVertex ;
    while sv <> self do begin
      if (sv.Owner <> self.Owner) then begin
        if sv.PrevVertex <> sv then begin
          if sv.PrevVertex.are_spliced( self.NextVertex) then begin
            Result := sv ;
            exit ;
          end ;
        end ;
      end ;
      sv := sv.CommonVertex
    end ;
  end ;


//==============================================================================
// T_hullEdge
//==============================================================================

  function T_hullEdge.Clone : T_hullEdge ;
  var
    he : T_hullEdge ;
  begin
    he := T_hullEdge.Create ;
    he.SI := SI ;
    he.EI := EI ;
    he.L := L ;

    Result := he ;
  end ;


//==============================================================================
// T_hullBuilder
//==============================================================================

  procedure T_hullBuilder.doConvexHull(
    {$IFDEF OXYGENE}
      const _nodes : TList<T_activePoint>
    {$ELSE}
      const _nodes : TObjectList<T_activePoint>
    {$ENDIF}
  ) ;
  var
  {$IFDEF OXYGENE}
    lap : TList<T_activePoint> ;
  {$ELSE}
    lap : TObjectList<T_activePoint> ;
  {$ENDIF}
    us  : Integer ;
    ue  : Integer ;
    ds  : Integer ;
    de  : Integer ;
    cnt : Integer ;
    jp  : Integer ;
    jc  : Integer ;
    jn  : Integer ;
    pp  : TGIS_Point ;
    pc  : TGIS_Point ;
    pn  : TGIS_Point ;
    tc  : Double ;
    tn  : Double ;
    i   : Integer ;
    j   : Integer ;
  begin
    lap := _nodes ;

    {$IFDEF OXYGENE}
      lap.Sort( @ActivePointsCompare ) ;
    {$ELSE}
      lap.Sort( TComparer<T_activePoint>.Construct( ActivePointsCompare ) ) ;
    {$ENDIF}

    for i := lap.Count - 2 downto 0 do begin
      if GisIsSamePoint( lap[i].P, lap[i+1].P ) then
        {$IFDEF OXYGENE}
          lap.RemoveAt( i+1 ) ;
        {$ELSE}
          lap.Delete( i+1 ) ;
        {$ENDIF}
    end ;

    // "trimstart" lup
    j := 0 ;
    cnt := 1 ;
    for i := 1 to lap.Count - 1 do begin
      if lap[i].P.X = lap[j].P.X then begin
        if lap[i].P.Y > lap[j].P.Y then begin
          lap[j].AU := False ;
          j := cnt ;
        end
        else
          lap[i].AU := False ;
        inc( cnt ) ;
      end
      else
        break ;
    end ;
    us := j ;

    // "trimend" lup
    j := lap.Count - 1 ;
    cnt := 1 ;
    for i := lap.Count - 2 downto 0 do begin
      if lap[i].P.X = lap[j].P.X then begin
        if lap[i].P.Y > lap[j].P.Y then begin
          lap[j].AU := False ;
          j := lap.Count - 1 - cnt ;
        end
        else
          lap[i].AU := False ;
        inc( cnt ) ;
      end
      else
        break ;
    end ;
    ue := j ;

    // "trimstart" ldn
    j := 0 ;
    cnt := 1 ;
    for i := 1 to lap.Count - 1 do begin
      if lap[i].P.X = lap[j].P.X then begin
        if lap[i].P.Y < lap[j].P.Y then begin
          lap[j].AD := False ;
          j := cnt ;
        end
        else
          lap[i].AD := False ;
        inc( cnt ) ;
      end
      else
        break ;
    end ;
    ds := j ;

    // "trimend" ldn
    j := lap.Count - 1 ;
    cnt := 1 ;
    for i := lap.Count - 2 downto 0 do begin
      if lap[i].P.X = lap[j].P.X then begin
        if lap[i].P.Y < lap[j].P.Y then begin
          lap[j].AD := False ;
          j := lap.Count - 1 - cnt ;
        end
        else
          lap[i].AD := False ;
        inc( cnt ) ;
      end
      else
        break ;
    end ;
    de := j ;

    cnt := 1 ;
    while cnt > 0 do begin

      cnt := 0 ;
      jn  := 0 ;
      jc := us ;
      for i := jc+1 to ue do begin
        if lap[i].AU then begin
          jn := i ;
          break ;
        end ;
      end ;

      while jn < ue do begin

        jp := jc ;
        jc := jn ;
        for i := jc+1 to ue do begin
          if lap[i].AU then begin
            jn := i ;
            break ;
          end ;
        end ;

        pp := lap[jp].P ;
        pc := lap[jc].P ;
        pn := lap[jn].P ;

        if pc.X = pp.X then begin
          if pc.Y <= pp.Y then
            lap[jc].AU := False
          else
            lap[jp].AU := False ;
          continue ;
        end ;

        tc := ( pc.Y - pp.Y )/( pc.X - pp.X ) ;
        tn := ( pn.Y - pp.Y )/( pn.X - pp.X ) ;

        if tn >= tc then begin
          lap[jc].AU := False ;
          inc( cnt ) ;
        end ;

      end ;

    end ;

    cnt := 1 ;
    while cnt > 0 do begin

      cnt := 0 ;
      jn  := 0 ;
      jc := ds ;
      for i := jc+1 to de do begin
        if lap[i].AD then begin
          jn := i ;
          break ;
        end ;
      end ;

      while jn < de do begin

        jp := jc ;
        jc := jn ;
        for i := jc+1 to de do begin
          if lap[i].AD then begin
            jn := i ;
            break ;
          end ;
        end ;

        pp := lap[jp].P ;
        pc := lap[jc].P ;
        pn := lap[jn].P ;

        if pc.X = pp.X then begin
          if pc.Y >= pp.Y then
            lap[jc].AD := False
          else
            lap[jp].AD := False ;
          continue ;
        end ;

        tc := ( pc.Y - pp.Y )/( pc.X - pp.X ) ;
        tn := ( pn.Y - pp.Y )/( pn.X - pp.X ) ;

        if tn <= tc then begin
          lap[jc].AD := False ;
          inc( cnt ) ;
        end ;

      end ;

    end ;

    if ( lap[us].P.X = lap[ds].P.X ) and
       ( lap[us].P.Y = lap[ds].P.Y ) then
      lap[ds].AD := False ;

    if ( lap[ue].P.X = lap[de].P.X ) and
       ( lap[ue].P.Y = lap[de].P.Y ) then
      lap[de].AD := False ;
  end ;


  procedure T_hullBuilder.doConcaveHull(
    {$IFDEF OXYGENE}
      const _nodes : TList<T_activePoint> ;
      const _edges : TList<T_hullEdge> ;
    {$ELSE}
      const _nodes : TObjectList<T_activePoint> ;
      const _edges : TObjectList<T_hullEdge> ;
    {$ENDIF}
    const _alpha : Double
  ) ;
  var
  {$IFDEF OXYGENE}
    lap : TList<T_activePoint> ;
    lhe : TList<T_hullEdge> ;
  {$ELSE}
    lap : TObjectList<T_activePoint> ;
    lhe : TObjectList<T_hullEdge> ;
  {$ENDIF}
    lcd : TList<Integer> ;
    he  : T_hullEdge ;
    he1 : T_hullEdge ;
    he2 : T_hullEdge ;
    ps  : TGIS_Point ;
    pe  : TGIS_Point ;
    pt  : TGIS_Point ;
    p0  : TGIS_Point ;
    pp  : TGIS_Point ;
    m11 : Double ;
    m12 : Double ;
    m21 : Double ;
    m22 : Double ;
    den : Double ;
    d   : Double ;
    db  : Double ;
    kb  : Integer ;
    i   : Integer ;
    i0  : Integer ;
    ip  : Integer ;
    k   : Integer ;
    cnt : Integer ;

    procedure add_edge ;
    begin
      he := T_hullEdge.Create ;
      he.SI := ip ;
      he.EI := i ;
      he.L := GisPoint2Point( lap[ip].P, lap[i].P ) ;
      lhe.Add( he ) ;
      ip := i ;
      inc( cnt ) ;
    end ;

    function check_edge( const _e : T_hullEdge ) : Boolean ;
    var
      pis : TGIS_Point ;
      pie : TGIS_Point ;
      pks : TGIS_Point ;
      pke : TGIS_Point ;
      pd  : TGIS_Point ;
      det : Double ;
      ti  : Double ;
      tk  : Double ;
      ii : Integer ;
    begin
      Result := False ;

      pis := _nodes[_e.SI].P ;
      pie := _nodes[_e.EI].P ;
      pie := GisPoint( pie.X - pis.X, pie.Y - pis.Y ) ;
      for ii := 0 to _edges.Count - 1 do begin
        pks := _nodes[_edges[ii].SI].P ;
        pke := _nodes[_edges[ii].EI].P ;
        pke := GisPoint( pke.X - pks.X, pke.Y - pks.Y ) ;

        det := pie.X*pke.Y - pke.X*pie.Y ;
        if Abs( det ) < 1e-12 then
          continue ;

        pd := GisPoint( pis.X - pks.X, pis.Y - pks.Y ) ;

        ti := ( pd.Y*pke.X - pd.X*pke.Y )/det ;
        tk := ( pd.Y*pie.X - pd.X*pie.Y )/det ;
        if ( ( ti > 0.001 ) and ( ti < 0.999 ) ) and
           ( ( tk > 0.001 ) and ( tk < 0.999 ) ) then
          exit ;
      end ;

      Result := True ;
    end ;

  begin
    lap := _nodes ;
    lhe := _edges ;

    for i := 0 to lap.Count - 1 do begin
      if lap[i].AU then begin
        i0 := i ;
        break ;
      end ;
    end ;

    ip := i0 ;
    cnt := 0 ;
    for i := ip + 1 to lap.Count - 1 do begin
      if lap[i].AU then
        add_edge ;
    end ;

    for i := lap.Count - 1 downto 0 do begin
      if lap[i].AD then
        add_edge ;
    end ;

    i := i0 ;
    add_edge ;

    lcd := TList<Integer>.Create ;
    try

      i := 0 ;
      while True do begin

        if i >= lhe.Count then
          break ;

        lcd.Clear ;
        if lhe[i].L > _alpha then begin

          ps := lap[lhe[i].SI].P ;
          pe := lap[lhe[i].EI].P ;

          for k := 0 to lap.Count - 1 do begin
            if ( not ( lap[k].AU or lap[k].AD ) ) and
               ( ( GisPoint2Point( lap[k].P, ps ) < lhe[i].L ) or
                 ( GisPoint2Point( lap[k].P, pe ) < lhe[i].L )
               ) then
              lcd.Add( k ) ;
          end ;

          if lcd.Count = 0 then begin
            inc( i ) ;
            continue ;
          end ;

          p0 := GisPoint( pe.X - ps.X, pe.Y - ps.Y ) ;
          den := p0.X*p0.X + p0.Y*p0.Y ;
          m11 := p0.X*p0.X/den ;
          m12 := p0.X*p0.Y/den ;
          m21 := m12 ;
          m22 := p0.Y*p0.Y/den ;

          db := GIS_MAX_DOUBLE ;
          kb := -1 ;
          for k := 0 to lcd.Count - 1 do begin
            pt := lap[lcd[k]].P ;
            pt := GisPoint( pt.X - ps.X, pt.Y - ps.Y ) ;
            pp := GisPoint( m11*pt.X + m12*pt.Y, m21*pt.X + m22*pt.Y ) ;
            d := GisPoint2Point( pt, pp ) ;
            if ( pp.X < Min( 0.0, p0.X ) ) or
               ( pp.X > Max( 0.0, p0.X ) ) then
              continue ;
            if ( pp.Y < Min( 0.0, p0.Y ) ) or
               ( pp.Y > Max( 0.0, p0.Y ) ) then
              continue ;
            if d < db then begin
              kb := k ;
              db := d ;
            end ;
          end ;

          if kb < 0 then begin
            inc( i ) ;
            continue ;
          end ;

          he1 := T_hullEdge.Create ;
          he1.SI := lhe[i].SI ;
          he1.EI := lcd[kb] ;
          he1.L := GisPoint2Point( lap[lhe[i].SI].P, lap[lcd[kb]].P ) ;

          he2 := T_hullEdge.Create ;
          he2.SI := lcd[kb] ;
          he2.EI := lhe[i].EI ;
          he2.L := GisPoint2Point( lap[lcd[kb]].P, lap[lhe[i].EI].P ) ;

          if not ( check_edge( he1 ) and check_edge( he2 ) ) then begin
            FreeObject( he1 ) ;
            FreeObject( he2 ) ;
            inc( i ) ;
            continue ;
          end ;

          lap[lcd[kb]].AU := True ;
          lhe.Insert( i+1, he1 ) ;
          lhe.Insert( i+2, he2 ) ;

          {$IFDEF OXYGENE}
            lhe.RemoveAt( i ) ;
          {$ELSE}
            lhe.Delete( i ) ;
          {$ENDIF}
        end
        else
          inc( i ) ;

      end ;

    finally
      FreeObject( lcd ) ;
    end ;
  end ;


  function T_hullBuilder.shapeFromNodes(
    {$IFDEF OXYGENE}
      const _nodes : TList<T_activePoint>
    {$ELSE}
      const _nodes : TObjectList<T_activePoint>
    {$ENDIF}
  ) : TGIS_ShapePolygon ;
  var
  {$IFDEF OXYGENE}
    lap : TList<T_activePoint> ;
  {$ELSE}
    lap : TObjectList<T_activePoint> ;
  {$ENDIF}
    shp : TGIS_ShapePolygon ;
    i   : Integer ;
  begin
    lap := _nodes ;

    shp := TGIS_ShapePolygon.Create ;
    shp.AddPart ;
    for i := 0 to lap.Count - 1 do begin
      if lap[i].AU then
        shp.AddPoint( lap[i].P ) ;
    end ;
    for i := lap.Count - 1 downto 0 do begin
      if lap[i].AD then
        shp.AddPoint( lap[i].P ) ;
    end ;

    Result := shp ;
  end ;


  function T_hullBuilder.shapeFromEdges(
    {$IFDEF OXYGENE}
      const _nodes : TList<T_activePoint> ;
      const _edges : TList<T_hullEdge>
    {$ELSE}
      const _nodes : TObjectList<T_activePoint> ;
      const _edges : TObjectList<T_hullEdge>
    {$ENDIF}
  ) : TGIS_ShapePolygon ;
  var
  {$IFDEF OXYGENE}
    lap : TList<T_activePoint> ;
    lhe : TList<T_hullEdge> ;
  {$ELSE}
    lap : TObjectList<T_activePoint> ;
    lhe : TObjectList<T_hullEdge> ;
  {$ENDIF}
    shp : TGIS_ShapePolygon ;
    i   : Integer ;
  begin
    lap := _nodes ;
    lhe := _edges ;

    shp := TGIS_ShapePolygon.Create ;
    shp.AddPart ;
    for i := 0 to lhe.Count - 1 do
      shp.AddPoint( lap[lhe[i].SI].P ) ;

    Result := shp ;
  end ;


  function T_hullBuilder.ConvexHull(
    const _layer : TGIS_LayerVector
  ) : TGIS_ShapePolygon ;
  var
    en  : TGIS_LayerVectorEnumerator ;
    shp : TGIS_ShapePolygon ;
  {$IFDEF OXYGENE}
    lap : TList<T_activePoint> ;
  {$ELSE}
    lap : TObjectList<T_activePoint> ;
  {$ENDIF}
    apt : T_activePoint ;
    sc  : TGIS_Shape ;
    pt  : TGIS_Point ;
    i   : Integer ;
    k   : Integer ;
  begin
    assert( assigned( _layer ) ) ;

    shp := nil ;
    Result := nil ;
    {$IFDEF OXYGENE}
      lap := TList<T_activePoint>.Create ;
    {$ELSE}
      lap := TObjectList<T_activePoint>.Create ;
    {$ENDIF}
    try
      en := _layer.Loop.GetEnumerator ;
      try
        while en.MoveNext do begin
          sc := en.GetCurrent ;
          for i := 0 to sc.GetNumParts - 1 do begin
            for k := 0 to sc.GetPartSize( i ) - 1 do begin
              pt := sc.GetPoint( i, k ) ;
              apt := T_activePoint.Create ;
              apt.P  := pt ;
              apt.AU := True ;
              apt.AD := True ;
              lap.Add( apt ) ;
            end ;
          end ;
        end ;
      finally
        FreeObject( en ) ;
      end ;

      if lap.Count = 0 then
        exit ;

      doConvexHull( lap ) ;

      shp := shapeFromNodes( lap ) ;

    finally
      FreeObject( lap ) ;
    end ;

    Result := shp ;
  end ;


  function T_hullBuilder.ConvexHull(
    const _shape : TGIS_Shape
  ) : TGIS_ShapePolygon ;
  begin
    Result := ConvexHull( _shape, nil, False ) ;
  end ;


  function T_hullBuilder.ConvexHull(
    const _shape1 : TGIS_Shape ;
    const _shape2 : TGIS_Shape
  ) : TGIS_ShapePolygon ;
  begin
    Result := ConvexHull( _shape1, _shape2, False ) ;
  end ;


  function T_hullBuilder.ConvexHull(
    const _shape1 : TGIS_Shape ;
    const _shape2 : TGIS_Shape ;
    const _fix    : Boolean
  ) : TGIS_ShapePolygon ;
  var
    shp : TGIS_ShapePolygon ;
  {$IFDEF OXYGENE}
    lap : TList<T_activePoint> ;
  {$ELSE}
    lap : TObjectList<T_activePoint> ;
  {$ENDIF}
    apt : T_activePoint ;
    pt  : TGIS_Point ;
    i   : Integer ;
    k   : Integer ;
  begin
    assert( assigned( _shape1 ) ) ;

    Result := nil ;
    shp    := nil ;

    {$IFDEF OXYGENE}
      lap := TList<T_activePoint>.Create ;
    {$ELSE}
      lap := TObjectList<T_activePoint>.Create ;
    {$ENDIF}
    try

      if _shape1.ShapeType = TGIS_ShapeType.Polygon then begin
        if _fix and assigned( GIS_Topology ) then
          GIS_Topology.FixShape( _shape1 ) ;
      end ;

      for i := 0 to _shape1.GetNumParts - 1 do begin
        for k := 0 to _shape1.GetPartSize( i ) - 1 do begin
          pt := _shape1.GetPoint( i, k ) ;
          apt := T_activePoint.Create ;
          apt.P  := pt ;
          apt.AU := True ;
          apt.AD := True ;
          lap.Add( apt ) ;
        end ;
      end ;

      if assigned( _shape2 ) then begin

        if _shape2.ShapeType = TGIS_ShapeType.Polygon then begin
          if _fix and assigned( GIS_Topology ) then
            GIS_Topology.FixShape( _shape2 ) ;
        end ;

        for i := 0 to _shape2.GetNumParts - 1 do begin
          for k := 0 to _shape2.GetPartSize( i ) - 1 do begin
            pt := _shape2.GetPoint( i, k ) ;
            apt := T_activePoint.Create ;
            apt.P  := pt ;
            apt.AU := True ;
            apt.AD := True ;
            lap.Add( apt ) ;
          end ;
        end ;

      end ;

      if lap.Count = 0 then
        exit ;

      doConvexHull( lap ) ;

      shp := shapeFromNodes( lap ) ;

    finally
      FreeObject( lap ) ;
    end ;

    Result := shp ;
  end ;


  function T_hullBuilder.ConcaveHull(
    const _layer : TGIS_LayerVector ;
    const _alpha : Double
  ) : TGIS_ShapePolygon ;
  var
    en  : TGIS_LayerVectorEnumerator ;
    shp : TGIS_ShapePolygon ;
    apt : T_activePoint ;
  {$IFDEF OXYGENE}
    lap : TList<T_activePoint> ;
    lhe : TList<T_hullEdge> ;
  {$ELSE}
    lap : TObjectList<T_activePoint> ;
    lhe : TObjectList<T_hullEdge> ;
  {$ENDIF}
    sc  : TGIS_Shape ;
    pt  : TGIS_Point ;
    i   : Integer ;
    k   : Integer ;

    function is_dup : Boolean ;
    var
      ii : Integer ;
    begin
      Result := True ;
      for ii := 0 to lap.Count - 1 do begin
        if GisIsSamePoint( pt, lap[ii].P ) then
          exit ;
      end ;
      Result := False ;
    end ;

  begin
    assert( assigned( _layer ) ) ;

    Result := nil ;
    shp    := nil ;

    {$IFDEF OXYGENE}
      lap := TList<T_activePoint>.Create ;
    {$ELSE}
      lap := TObjectList<T_activePoint>.Create ;
    {$ENDIF}
    try
      en := _layer.Loop.GetEnumerator ;
      try
        while en.MoveNext do begin
          sc := en.GetCurrent ;
          for i := 0 to sc.GetNumParts - 1 do begin
            for k := 0 to sc.GetPartSize( i ) - 1 do begin
              pt := sc.GetPoint( i, k ) ;
              if is_dup then
                continue ;
              apt := T_activePoint.Create ;
              apt.P  := pt ;
              apt.AU := True ;
              apt.AD := True ;
              lap.Add( apt ) ;
            end ;
          end ;
        end ;
      finally
        FreeObject( en ) ;
      end ;

      if lap.Count = 0 then
        exit ;

      doConvexHull( lap ) ;

      {$IFDEF OXYGENE}
        lhe := TList<T_hullEdge>.Create ;
      {$ELSE}
        lhe := TObjectList<T_hullEdge>.Create ;
      {$ENDIF}
      try
        doConcaveHull( lap, lhe, _alpha ) ;
        shp := shapeFromEdges( lap, lhe ) ;
      finally
        FreeObject( lhe ) ;
      end ;

    finally
      FreeObject( lap ) ;
    end ;

    Result := shp ;
  end ;


  function T_hullBuilder.ConcaveHull(
    const _shape  : TGIS_Shape ;
    const _alpha  : Double
  ) : TGIS_ShapePolygon ;
  begin
    Result := ConcaveHull( _shape, nil, _alpha, False ) ;
  end ;


  function T_hullBuilder.ConcaveHull(
    const _shape1 : TGIS_Shape ;
    const _shape2 : TGIS_Shape ;
    const _alpha  : Double
  ) : TGIS_ShapePolygon ;
  begin
    Result := ConcaveHull( _shape1, _shape2, _alpha, False ) ;
  end ;


  function T_hullBuilder.ConcaveHull(
    const _shape1 : TGIS_Shape ;
    const _shape2 : TGIS_Shape ;
    const _alpha  : Double     ;
    const _fix    : Boolean
  ) : TGIS_ShapePolygon ;
  var
    shp : TGIS_ShapePolygon ;
    apt : T_activePoint ;
  {$IFDEF OXYGENE}
    lap : TList<T_activePoint> ;
    lhe : TList<T_hullEdge> ;
  {$ELSE}
    lap : TObjectList<T_activePoint> ;
    lhe : TObjectList<T_hullEdge> ;
  {$ENDIF}
    pt  : TGIS_Point ;
    i   : Integer ;
    k   : Integer ;
  begin
    assert( assigned( _shape1 ) ) ;

    shp := nil ;
    Result := nil ;
    {$IFDEF OXYGENE}
      lap := TList<T_activePoint>.Create ;
    {$ELSE}
      lap := TObjectList<T_activePoint>.Create ;
    {$ENDIF}
    try

      if _shape1.ShapeType = TGIS_ShapeType.Polygon then begin
        if _fix and assigned( GIS_Topology ) then
          GIS_Topology.FixShape( _shape1 ) ;
      end ;

      for i := 0 to _shape1.GetNumParts - 1 do begin
        for k := 0 to _shape1.GetPartSize( i ) - 1 do begin
          pt := _shape1.GetPoint( i, k ) ;
          apt := T_activePoint.Create ;
          apt.P  := pt ;
          apt.AU := True ;
          apt.AD := True ;
          lap.Add( apt ) ;
        end ;
      end ;

      if assigned( _shape2 ) then begin

        if _shape2.ShapeType = TGIS_ShapeType.Polygon then begin
          if _fix and assigned( GIS_Topology ) then
            GIS_Topology.FixShape( _shape2 ) ;
        end ;

        for i := 0 to _shape2.GetNumParts - 1 do begin
          for k := 0 to _shape2.GetPartSize( i ) - 1 do begin
            pt := _shape2.GetPoint( i, k ) ;
            apt := T_activePoint.Create ;
            apt.P  := pt ;
            apt.AU := True ;
            apt.AD := True ;
            lap.Add( apt ) ;
          end ;
        end ;

      end ;

      if lap.Count = 0 then
        exit ;

      doConvexHull( lap ) ;

      {$IFDEF OXYGENE}
        lhe := TList<T_hullEdge>.Create ;
      {$ELSE}
        lhe := TObjectList<T_hullEdge>.Create ;
      {$ENDIF}
      try
        doConcaveHull( lap, lhe, _alpha ) ;
        shp := shapeFromEdges( lap, lhe ) ;
      finally
        FreeObject( lhe ) ;
      end ;

    finally
      FreeObject( lap ) ;
    end ;

    Result := shp ;
  end ;


//==============================================================================
// TGIS_Topology
//==============================================================================

  constructor TGIS_Topology.Create ;
  begin
    inherited Create ;
    FForceShapeFixing := bForceShapeFixing ;
    lockedProjection := False ;
    forceFix := False ;

    contour1 := nil ;
    contour2 := nil ;
  end ;

  procedure TGIS_Topology.doDestroy ;
  begin
    if contour1 = contour2 then
      contour2 := nil ;
    {$IFDEF MANAGED}
      {$IFDEF JAVA}
        FreeObjectNotNilEx( contour1 ) ;
        contour1 := nil ;
        FreeObjectNotNilEx( contour2 ) ;
        contour2:= nil ;
      {$ELSE}
        FreeObject( contour1 ) ;
        FreeObject( contour2 ) ;
      {$ENDIF}
    {$ELSE}
      FreeObject( T_Contour( contour1 ) ) ;
      FreeObject( T_Contour( contour2 ) ) ;
    {$ENDIF}
    inherited ;
  end ;

  procedure TGIS_Topology.fset_Tolerance(
    const _tolerance : Double
  ) ;
  const
    SIN45 = 0.70710678118654752440084436210485 ;
  begin
    if _tolerance > 0 then FTolerance := _tolerance
                      else FTolerance := 0 ;
    gridStep   := FTolerance / 2 * SIN45 ;
    gridStep09 := gridStep * 0.9 ;
  end ;

  procedure TGIS_Topology.addShape(
    const _dest    : TGIS_Shape ;
    const _source  : TGIS_Shape
  ) ;
  begin
    addShape( _dest, _source, False ) ;
  end ;

  procedure TGIS_Topology.addShape(
    const _dest    : TGIS_Shape ;
    const _source  : TGIS_Shape ;
    const _reverse : Boolean
  ) ;
  var
    point_no : Integer ;
    part_no  : Integer ;
  begin
    assert( assigned( _source ) ) ;
    assert( assigned( _dest   ) ) ;

    if _dest is TGIS_ShapePoint then exit ;

    if _source.IsEmpty then
      exit ;
    _dest.Lock( TGIS_Lock.Projection );
    for part_no := 0 to _source.GetNumParts -1 do begin
      _dest.AddPart ;
      if _reverse then begin
        for point_no := _source.GetPartSize( part_no ) -1 downto 0 do
          _dest.AddPoint( _source.GetPoint( part_no, point_no ) ) ;
      end
      else begin
        for point_no := 0 to _source.GetPartSize( part_no ) -1 do
          _dest.AddPoint( _source.GetPoint( part_no, point_no ) ) ;
      end ;
    end ;
    _dest.Unlock ;
  end ;

  function TGIS_Topology.makeDPoly(
    const _s : TGIS_Shape
  ) : TGIS_ShapePolygon ;
  var
    deg_pt0  : TGIS_Point ;
    deg_pt1  : TGIS_Point ;
    deg_pt2  : TGIS_Point ;
    deg_pt3  : TGIS_Point ;
  begin
    deg_pt0 := _s.GetPoint(0, 0) ;
    deg_pt1 := _s.GetPoint(0, 1) ;
    if GisPoint2Point(deg_pt0, deg_pt1) <= 1.5*FTolerance then
    begin
      deg_pt0.X := deg_pt0.X -FTolerance*0.75 ;
      deg_pt0.Y := deg_pt0.Y -FTolerance*0.75 ;
      deg_pt1.X := deg_pt0.X -FTolerance*0.75 ;
      deg_pt1.Y := deg_pt0.Y +FTolerance*0.75 ;
    end ;

    deg_pt2.X := deg_pt1.X +FTolerance*1.5 ;
    deg_pt2.Y := deg_pt1.Y ;

    deg_pt3.X := deg_pt0.X +FTolerance*1.5 ;
    deg_pt3.Y := deg_pt0.Y ;

    Result := TGIS_ShapePolygon.Create ;
    Result.AddPart ;
    Result.AddPoint(deg_pt0) ;
    Result.AddPoint(deg_pt1) ;
    Result.AddPoint(deg_pt2) ;
    Result.AddPoint(deg_pt3) ;
  end ;

  function TGIS_Topology.makeDArc(
    const _s : TGIS_Shape
  ) : TGIS_ShapeArc ;
  var
    deg_pt0  : TGIS_Point ;
    deg_pt1  : TGIS_Point ;
  begin
    deg_pt0 := _s.GetPoint(0, 0) ;
    deg_pt1.X := deg_pt0.X +FTolerance*1.5 ;
    deg_pt1.Y := deg_pt0.Y +FTolerance*1.5 ;

    Result := TGIS_ShapeArc.Create ;
    Result.AddPart ;
    Result.AddPoint(deg_pt0) ;
    Result.AddPoint(deg_pt1) ;
  end ;

  function  TGIS_Topology.firstInteriorNotUsed(
    var _de9im : String
  ) : Boolean ;
  var
    ld       : Integer        ;
    i        : Integer        ;
    de9im_sb : TStringBuilder ;
  begin
    ld := length( _de9im ) ;

    de9im_sb := TStringBuilder.Create( _de9im ) ;
    try
      Result := True ;
      i := StringFirst ;
      if (_de9im[i] <> '*') and (_de9im[i] <> 'F') and
         (_de9im[i] <> 'f') then
      begin
        Result := False ;
      end
      else begin
        de9im_sb[i-StringFirst] := '*' ;
        if ld > 1 then begin
          i := StringFirst + 1 ;
          if (_de9im[i] <> '*') and (_de9im[i] <> 'F') and
             (_de9im[i] <> 'f') then
          begin
            Result := False ;
          end
          else begin
            de9im_sb[i-StringFirst] := '*' ;
            if ld > 2 then begin
              i := StringFirst + 2 ;
              if (_de9im[i] <> '*') and (_de9im[i] <> 'F') and
                 (_de9im[i] <> 'f') then
              begin
                Result := False ;
              end
              else
                de9im_sb[i-StringFirst] := '*' ;
            end ;
          end ;
        end ;
      end ;
      _de9im := de9im_sb.ToString ;
    finally
      FreeObject( de9im_sb ) ;
    end ;

  end ;

  function  TGIS_Topology.secondInteriorNotUsed(
    var _de9im : String
  ) : Boolean ;
  var
    ld       : Integer        ;
    i        : Integer        ;
    de9im_sb : TStringBuilder ;
  begin
    ld := length( _de9im ) ;

    de9im_sb := TStringBuilder.Create( _de9im ) ;
    try
      Result := True ;
      i := StringFirst ;
      if (_de9im[i] <> '*') and (_de9im[i] <> 'F') and
         (_de9im[i] <> 'f') then
      begin
        Result := False ;
      end
      else begin
        de9im_sb[i-StringFirst] := '*' ;
        if ld > 3 then begin
          i := StringFirst + 3 ;
          if (_de9im[i] <> '*') and (_de9im[i] <> 'F') and
             (_de9im[i] <> 'f') then
          begin
            Result := False ;
          end
          else begin
            de9im_sb[i-StringFirst] := '*' ;
            if ld > 6 then begin
              i := StringFirst + 6 ;
              if (_de9im[i] <> '*') and (_de9im[i] <> 'F') and
                 (_de9im[i] <> 'f') then
              begin
                Result := False ;
              end
              else
                de9im_sb[i-StringFirst] := '*' ;
            end ;
          end ;
        end ;
      end ;
      _de9im := de9im_sb.ToString ;
    finally
      FreeObject( de9im_sb ) ;
    end ;

  end ;

  function TGIS_Topology.computeTolerance : Double ;
  var
    w1, w2, h1, h2  : Double ;
    min1, min2      : Double ;
    max1, max2      : Double ;
    ext1, ext2      : TGIS_Extent ;
  const
    SIN45 = 0.70710678118654752440084436210485 ;
    PREC_FACTOR = 1e15 ;
  begin

    if assigned( pshape1 ) then begin
      if GisLockGreaterThanEqual( pshape1.LockLevel,  TGIS_Lock.Projection ) then
        ext1 := _TGIS_Extent (pshape1.Extent)
      else
        ext1 := _TGIS_Extent (pshape1.ProjectedExtent) ;

      with ext1 do begin
        w1 := XMax - XMin ;
        h1 := YMax - YMin ;
        min1 := Min(Abs(XMin), Abs(YMin)) ;
        max1 := Max(Abs(XMax), Abs(YMax)) ;
      end ;
    end
    else begin
      w1 := 0 ;
      h1 := 0 ;
      min1 := GIS_MAX_SINGLE ;
      max1:= -GIS_MAX_SINGLE ;
    end ;

    if assigned( pshape2 ) then begin
      if GisLockGreaterThanEqual( pshape2.LockLevel, TGIS_Lock.Projection ) then
        ext2 := _TGIS_Extent (pshape2.Extent)
      else
        ext2 := _TGIS_Extent (pshape2.ProjectedExtent) ;
      with ext2 do begin
        w2 := XMax - XMin ;
        h2 := YMax - YMin ;
        min2 := Min(Abs(XMin), Abs(YMin)) ;
        max2 := Max(Abs(XMax), Abs(YMax)) ;
      end ;
    end
    else begin
      w2 := 0 ;
      h2 := 0 ;
      min2 := GIS_MAX_SINGLE ;
      max2:= -GIS_MAX_SINGLE ;
    end ;

    w1 := Max( w1, w2);
    h1 := Max( h1, h2 );
    min1 := Min(min1, min2) ;
    max1 := Max(max1, max2) ;

    if w2 > w1 then w1 := w2 ; // w1 := Max( w1, w2 )
    if h2 > h1 then h1 := h2 ; // h1 := Max( h1, h2 )
    if h1 > w1 then w1 := h1 ; // w1 := Max( w1, h1 )

    Result := w1 / 1e14 / SIN45 * 2 ;

    while (min1 > 1000) and (Result < 1e-8) do begin
      min1 := min1/10 ;
      Result := Result*10 ;
    end ;

    interTolerance := Result ;

    if FTolerance < Result then
      FTolerance := Result
    else begin
      w1 := Min(w1, h1) ;
      if w1 > 0 then begin
        if FTolerance > w1*0.49 then
          FTolerance := w1*0.49 ;
      end ;
    end ;

    if FTolerance = 0 then
      FTolerance := 1e-15 ;

    while FTolerance*PREC_FACTOR < max1 do
      FTolerance := 10*FTolerance ;

    Result := FTolerance ;
    gridStep   := FTolerance / 2 * SIN45 ;
    gridStep09 := gridStep * 0.9 ;

  end ;

  function TGIS_Topology.getVdescriptors(
    const _vertex : TObject
  ) : TList<TObject> ;
  var
    m   : Integer  ;
    lst : TList<TObject>    ;
    vrt : T_Vertex ;
    dsc : T_CrossVertexDescriptor ;
    obj : TObject ;
  begin

    Result := nil ;
    if listOfVdescriptors.Count > 0 then begin
      for m := 0 to listOfVdescriptors.Count -1 do begin
        lst := TList<TObject>(listOfVdescriptors[m]);
        dsc := T_CrossVertexDescriptor(lst[0]) ;
        vrt := T_Vertex(dsc.CorrespondingVertex);
        if GisPoint2Point(vrt.WorkPoint2, T_Vertex( _vertex ).WorkPoint2)
              < gridStep09
        then begin
          Result := lst ;
          break ;
        end ;
      end ;
    end ;

    if Result = nil then begin
      Result := TList<TObject>.Create ;
      // force to add a list instance to list, not its items
      // difference in RTL and sugar (that worked fine)
      obj := Result ;
      listOfVdescriptors.Add(obj) ;
      assert( listOfVdescriptors.Count > 0 ) ;
    end ;

  end ;

  function TGIS_Topology.edgeRule(
    const _edge      : TObject ;
    var   _direction : Integer
  ) : Boolean ;
  begin
    Result := False ;

    case combineType of
      TGIS_TopologyCombineType.Union :
        if ( T_Edge( _edge ).Site = T_EdgeSite.gesOutside ) or
           ( T_Edge( _edge ).Site = T_EdgeSite.gesSharedSD) then
        begin
          _direction := ord( T_EdgeDirection.gedForward ) ;
          Result := True ;
        end ;
      TGIS_TopologyCombineType.Intersection :
        if ( T_Edge( _edge ).Site = T_EdgeSite.gesInside  ) or
           ( T_Edge( _edge ).Site = T_EdgeSite.gesSharedSD) then
        begin
          _direction := ord( T_EdgeDirection.gedForward ) ;
          Result := True ;
        end ;
      TGIS_TopologyCombineType.Difference :
        if ( ( T_Edge( _edge ).Site = T_EdgeSite.gesOutside ) or
             ( T_Edge( _edge ).Site = T_EdgeSite.gesSharedCD)
           ) and
           ( T_Edge( _edge ).PrevVertex.Owner = contour1) then
        begin
          _direction := ord( T_EdgeDirection.gedForward ) ;
          Result := True ;
        end
        else
        if ( ( T_Edge( _edge ).Site = T_EdgeSite.gesInside  ) or
             ( T_Edge( _edge ).Site = T_EdgeSite.gesSharedCD)
           ) and
           ( T_Edge( _edge ).PrevVertex.Owner = contour2) then
        begin
          _direction := ord( T_EdgeDirection.gedBackward ) ;
          Result := True ;
        end ;
      TGIS_TopologyCombineType.SymmetricalDifference :
        if ( T_Edge( _edge ).Site = T_EdgeSite.gesOutside) then
        begin
          _direction := ord( T_EdgeDirection.gedForward ) ;
          Result := True ;
        end
        else
        if ( T_Edge( _edge ).Site = T_EdgeSite.gesInside) then
        begin
          _direction := ord( T_EdgeDirection.gedBackward ) ;
          Result := True ;
        end
      else
         exit ; // none meet our criteria
    end ;

  end ;

  procedure TGIS_Topology.fixCrossVertices ;
  var
    cross_ptg2    : TGIS_Point     {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    cross_ptg3i   : TGIS_Point3D   {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    cross_ptg3k   : TGIS_Point3D   {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    is_cross_ptg  : Boolean        ;
    testvertex    : Boolean        ;
    testline      : Boolean        ;
    line1         : TGIS_Line      ;
    line2         : TGIS_Line      ;
    i, k          : Integer        ;
    ii, kk        : Integer        ;
    ni, nk        : Integer        ;
    di, dk,
    dni, dnk      : Double         ;
    dmin          : Double         ;
    dik, dink,
    dnik, dnink   : Double         ;
    dci, dck,
    dcni, dcnk    : Double         ;
    mincase       : Word        ;
    vi, vni      : T_Vertex        ;
    vk, vnk      : T_Vertex        ;
    vertex       : T_Vertex        ;
    crossvertex  : T_Vertex        ;
    testextent1  : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF}    ;
    ended        : Boolean         ;
    current_i,
    current_ii,
    start_ii,
    current_k    : Integer         ;
    start_i,
    start_k      : Integer         ;
    k_list,
    i_list       : T_ContourPart   ;
    svertex      : T_Vertex        ;
    cc         : TObject         ;
    cs_changed   : Boolean         ;
    inprogressd  : Integer         ;
    oldk         : Int64           ;
    allk         : Int64           ;
    {$IFDEF OXYGENE}
      bargs      : TGIS_BusyEventArgs ;
    {$ENDIF}

    function areSpliced( const _v1, _v2 : T_Vertex) : Boolean ;
    var
      nv : T_Vertex ;
    begin
      Result := False ;
      if (_v1.CommonVertex = nil) or (_v2.CommonVertex = nil) then
        exit ;
      nv := _v1.CommonVertex ;
      while True do begin
        if nv = _v2 then begin
          Result := True ;
          break ;
        end ;
        if nv = _v1 then
          break ;
        nv := nv.CommonVertex ;
      end ;
    end ;

    procedure separateVertex( const _v : T_Vertex ) ;
    var
      nv : T_Vertex ;
    begin
      if _v.CommonVertex = nil then
        exit ;
      if _v.CommonVertex.CommonVertex = _v then begin
        _v.CommonVertex.CommonVertex := nil ;
        _v.CommonVertex.IsCrossVertex := False ;

        _v.CommonVertex := nil ;
        _v.IsCrossVertex := False ;

        exit ;
      end ;

      nv := _v.CommonVertex.CommonVertex ;
      while True do begin
        if nv.CommonVertex = _v then begin
          nv.CommonVertex := _v.CommonVertex ;
          break ;
        end ;
        if nv = _v then
          break ;
        nv := nv.CommonVertex ;
      end ;
    end ;

    procedure spliceVertices( const _baseVertex, _nextVertex : T_Vertex) ;
    var
      cvertex : T_Vertex ;
      nvertex : T_Vertex ;
      dvertex : T_Vertex ;
      rvertex : T_Vertex ;
    begin
      if (_baseVertex.CommonVertex = nil) and
         (_nextVertex.CommonVertex = nil) then
      begin
        _baseVertex.IsCrossVertex := True ;
        _baseVertex.CommonVertex  := _nextVertex ;
        _nextVertex.IsCrossVertex := True ;
        _nextVertex.CommonVertex  := _baseVertex ;
        inc( crossingsNo ) ;
        exit ;
      end ;

      if _baseVertex.CommonVertex <> nil then
        if _baseVertex.CommonVertex = _nextVertex then
          exit ;

      if _baseVertex.CommonVertex = nil then
      begin
        dvertex := _baseVertex ;
        dvertex.IsCrossVertex := True ;
        _baseVertex.CommonVertex := _nextVertex ;
        cvertex := _nextVertex ;
        nvertex := _nextVertex ;
      end
      else
      if _nextVertex.CommonVertex = nil then
      begin
        dvertex := _nextVertex ;
        dvertex.IsCrossVertex := True ;
        _nextVertex.CommonVertex := _baseVertex ;
        cvertex := _baseVertex ;
        nvertex := _baseVertex ;
      end
      else
      begin

        if areSpliced(_baseVertex, _nextVertex) then begin
          exit ;
        end ;
        dvertex := _baseVertex.CommonVertex ;
        _baseVertex.CommonVertex := _nextVertex ;
        cvertex := _nextVertex ;
        nvertex := _nextVertex ;
      end ;
      rvertex := cvertex ;
      while cvertex.CommonVertex <> nvertex do begin
        cvertex := cvertex.CommonVertex ;
        if rvertex = cvertex then begin
          break ;
        end ;
      end ;
      checkCombine := FForceShapeFixing ;
      cvertex.CommonVertex := dvertex ;
    end ;
  begin

    if (T_Contour(contour1).Extent.YMin > T_Contour(contour2).Extent.YMin) and
       (T_Contour(contour1).Extent.YMax < T_Contour(contour2).Extent.YMax) and
       (T_Contour(contour1).Extent.XMin > T_Contour(contour2).Extent.XMin) and
       (T_Contour(contour1).Extent.XMax < T_Contour(contour2).Extent.XMax)
    then
      cs_changed := True
    else
    if (T_Contour( contour1 ).PartsCount > T_Contour( contour2 ).PartsCount) then
      cs_changed := True
    else
    if (T_Contour(contour1).Count < T_Contour(contour2).Count ) then
      cs_changed := True
    else
      cs_changed := False ;

    if cs_changed then begin
      cc := contour1 ;
      contour1 := contour2 ;
      contour2 := cc ;
    end ;

    crossingsNo := 0 ;
    checkCombine := False ;

    oldk := 0 ;
    inprogressd := 0 ;
    allk := T_Contour( contour1 ).Count ;

    for kk := 0 to T_Contour( contour1 ).PartsCount -1 do begin
      T_Contour( contour1 ).Part := kk ;

      k_list := T_ContourPart(T_Contour( contour1 ).listOfParts[kk]) ;

      if k_list.Extent.YMin > (T_Contour(contour2).Extent.YMax +gridStep) then
        continue ;

      if k_list.Extent.YMax < (T_Contour(contour2).Extent.YMin -gridStep) then
        continue ;

      if k_list.Extent.XMin > (T_Contour(contour2).Extent.XMax +gridStep) then
        continue ;

      if k_list.Extent.XMax < (T_Contour(contour2).Extent.XMin -gridStep) then
        continue ;

      if not assigned( k_list.ExtentsArray ) then
        k_list.SetExtents(gridStep);

      current_k := 0 ;
      current_i := 0 ;
      current_ii := 0 ;

      repeat
        ended := True ;
        if current_k >= k_list.VerticesList.Count then
          break ;

        start_k := current_k ;

        vk := T_Vertex(k_list.VerticesList[start_k]) ;
        vnk := T_Vertex(vk).NextVertex ;
        for k := start_k to k_list.VerticesList.Count -1 do begin
          current_k := k ;

          if k < k_list.VerticesList.Count -1 then
            nk := k +1
          else
            nk := 0 ;

          if k > start_k then begin
            vk := vnk ;

            vnk := T_Vertex(vk).NextVertex ;

          end ;

          if k_list.ExtentsArray[k].YMin > T_Contour(contour2).Extent.YMax then
          begin
            break ;
          end ;

          if k_list.ExtentsArray[k].YMax < T_Contour(contour2).Extent.YMin then
          begin
            break ;
          end ;

          if k_list.ExtentsArray[k].XMin > T_Contour(contour2).Extent.XMax then
          begin
            break ;
          end ;

          if k_list.ExtentsArray[k].XMax < T_Contour(contour2).Extent.XMin then
          begin
            break ;
          end ;

          testextent1.YMin := Min(vk.WorkPoint2.Y, vnk.WorkPoint2.Y) -gridStep;
          if T_Contour(contour2).Extent.YMax < testextent1.YMin then
            continue ;

          testextent1.YMax := Max(vk.WorkPoint2.Y, vnk.WorkPoint2.Y) +gridStep;
          if T_Contour(contour2).Extent.YMin > testextent1.YMax then
            continue ;

          testextent1.XMin := Min(vk.WorkPoint2.X, vnk.WorkPoint2.X) -gridStep;
          if T_Contour(contour2).Extent.XMax < testextent1.XMin then
            continue ;

          testextent1.XMax := Max(vk.WorkPoint2.X, vnk.WorkPoint2.X) +gridStep;
          if T_Contour(contour2).Extent.XMin > testextent1.XMax then
            continue ;

          if busyUsed then begin
            if k mod GIS_PROGRESS_TRESHOLD = 0 then begin
              if busyStep <> 0 then
                inprogressd := (busyStep*(k +oldk)) div allk ;

              {$IFDEF OXYGENE}
                bargs := TGIS_BusyEventArgs.Create( inProgress +inprogressd,
                                                    fullProgress,
                                                    doBreak
                                                  ) ;
                try
                  FOnBusy( self, bargs ) ;
                finally
                  doBreak := bargs.Abort ;
                  FreeObject( bargs ) ;
                end ;
              {$ELSE}
                FOnBusy( self, inProgress +inprogressd, fullProgress, doBreak ) ;
              {$ENDIF}
              if doBreak then
                break ;
            end ;
          end ;

          start_ii := current_ii ;
          for ii := start_ii to T_Contour( contour2 ).PartsCount -1 do begin
            current_ii := ii ;
            T_Contour( contour2 ).Part := ii ;

            i_list := T_ContourPart(T_Contour( contour2 ).listOfParts[ii]) ;

            if T_Contour( contour2 ).PartsCount > 1 then begin

              if k_list.Extent.YMin > (i_list.Extent.YMax +gridStep) then begin
                continue ;
              end ;

              if k_list.Extent.YMax < (i_list.Extent.YMin -gridStep) then begin
                continue ;
              end ;

              if k_list.Extent.XMin > (i_list.Extent.XMax +gridStep) then begin
                continue ;
              end ;

              if k_list.Extent.XMax < (i_list.Extent.XMin -gridStep) then begin
                continue ;
              end ;

            end ;

            if current_i >= i_list.VerticesList.Count then begin
              current_i := 0 ;
              continue ;
            end ;

            if not assigned( i_list.ExtentsArray ) then
              i_list.SetExtents(gridStep);

            start_i := current_i ;

            vi := T_Vertex(i_list.VerticesList[start_i]) ;
            vni := T_Vertex(vi).NextVertex ;
            for i := start_i to i_list.VerticesList.Count -1 do begin
              current_i := i ;

              if i < i_list.VerticesList.Count -1 then
                ni := i +1
              else
                ni := 0 ;

              if i > start_i then
              begin
                vi := vni ;
                vni := T_Vertex(vi).NextVertex ;
              end ;

              if i_list.ExtentsArray[i].YMin > testextent1.YMax then
              begin
                break ;
              end ;

              if i_list.ExtentsArray[i].YMax < testextent1.YMin then
              begin
                break ;
              end ;

              if i_list.ExtentsArray[i].XMin > testextent1.XMax then
              begin
                break ;
              end ;

              if i_list.ExtentsArray[i].XMax < testextent1.XMin then
              begin
                break ;
              end ;


              if (vi.WorkPoint2.X < testextent1.XMin) and
                 (vni.WorkPoint2.X < testextent1.XMin) then begin
                inc( current_i ) ;
                continue ;
              end ;

              if (vi.WorkPoint2.X > testextent1.XMax) and
                 (vni.WorkPoint2.X > testextent1.XMax) then begin
                inc( current_i ) ;
                continue ;
              end ;

              if (vi.WorkPoint2.Y < testextent1.YMin) and
                 (vni.WorkPoint2.Y < testextent1.YMin) then begin
                inc( current_i ) ;
                continue ;
              end ;

              if (vi.WorkPoint2.Y > testextent1.YMax) and
                 (vni.WorkPoint2.Y > testextent1.YMax) then begin
                inc( current_i ) ;
                continue ;
              end ;

              if vi.NextEdgeSite <> T_EdgeSite.gesUndefined then begin
                if vk.NextEdgeSite <> T_EdgeSite.gesUndefined then begin
                  inc( current_i ) ;
                  continue ;
                end;
              end ;

              dnik := GisPoint2Point(vk.WorkPoint2, vni.WorkPoint2) ;
              dink := GisPoint2Point(vnk.WorkPoint2, vi.WorkPoint2) ;
              if (dnik < gridStep09) and (dink < gridStep09) then begin
                if T_Vertex(vk).NextEdgeSite = T_EdgeSite.gesSharedCD then
                  continue ;
                spliceVertices(vk, vni) ;
                spliceVertices(vnk, vi) ;
                vk.WorkPoint2  := _TGIS_Point( vni.WorkPoint2 ) ;
                vnk.WorkPoint2 := _TGIS_Point( vi.WorkPoint2  ) ;
                vk.WorkPoint3  := _TGIS_Point3D( vni.WorkPoint3 ) ;
                vnk.WorkPoint3 := _TGIS_Point3D( vi.WorkPoint3  ) ;

                T_Vertex(vk).NextEdgeSite  := T_EdgeSite.gesSharedCD ;
                T_Vertex(vnk).PrevEdgeSite := T_EdgeSite.gesSharedCD ;
                T_Vertex(vi).NextEdgeSite  := T_EdgeSite.gesSharedCD ;
                T_Vertex(vni).PrevEdgeSite := T_EdgeSite.gesSharedCD ;
                break ;
              end ;

              dik   := GisPoint2Point(vk.WorkPoint2, vi.WorkPoint2) ;
              dnink := GisPoint2Point(vnk.WorkPoint2, vni.WorkPoint2) ;
              if (dik < gridStep09) and (dnink < gridStep09)
              then begin

                if T_Vertex(vk).NextEdgeSite = T_EdgeSite.gesSharedSD then
                  continue ;
                spliceVertices(vk, vi) ;
                spliceVertices(vnk, vni) ;
                vk.WorkPoint2  := _TGIS_Point  ( vi.WorkPoint2  ) ;
                vk.WorkPoint3  := _TGIS_Point3D( vi.WorkPoint3  ) ;
                vnk.WorkPoint2 := _TGIS_Point  ( vni.WorkPoint2 ) ;
                vnk.WorkPoint3 := _TGIS_Point3D( vni.WorkPoint3 ) ;
                T_Vertex(vk).NextEdgeSite  := T_EdgeSite.gesSharedSD ;
                T_Vertex(vnk).PrevEdgeSite := T_EdgeSite.gesSharedSD ;
                T_Vertex(vi).NextEdgeSite  := T_EdgeSite.gesSharedSD ;
                T_Vertex(vni).PrevEdgeSite := T_EdgeSite.gesSharedSD ;
                break ;
              end ;

              is_cross_ptg  := False ;
              crossvertex   := nil   ;
              mincase       := 0     ;
              testline      := True  ;
              dnk           := FTolerance ;
              dni           := FTolerance ;

              if vi.CommonVertex = nil then
                testvertex := True
              else
              if (not areSpliced(vi, vk)) and (not areSpliced(vi, vnk))
              then
              begin
                testvertex := True;
              end

              else begin
                testvertex := False ;
                testline := False ;
              end ;
              if testvertex then begin
                di := GisLine2Point(vk.WorkPoint2, vnk.WorkPoint2, vi.WorkPoint2) ;
                if di < gridStep09 then
                  if (di = 0) or ((di <= dik) and (di <= dink)) then
                    mincase := 1 ;
              end
              else
                di := FTolerance ;

              if vk.CommonVertex = nil then
                testvertex := True
              else
              if (not areSpliced(vk, vi)) and (not areSpliced(vk, vni))
              then
                testvertex := True
              else begin
                testvertex := False ;
                testline := False ;
              end ;
              if testvertex then begin
                dk := GisLine2Point(vi.WorkPoint2, vni.WorkPoint2, vk.WorkPoint2) ;
                if dk < gridStep09 then
                  if (dk = 0) or ((dk <= dik) and (dk <= dnik)) then
                        mincase := mincase or 4 ;
              end
              else
                dk := FTolerance ;
              if mincase = 0 then begin
                if vni.CommonVertex = nil then
                  testvertex := True
                else
                if (not areSpliced(vni, vk)) and (not areSpliced(vni, vnk))
                then
                  testvertex := True
                else begin
                  testvertex := False ;
                  testline := False ;
                end ;

                if testvertex then begin
                  dni := GisLine2Point(vk.WorkPoint2, vnk.WorkPoint2, vni.WorkPoint2) ;
                  if dni < gridStep09  then begin
                    if (dni = 0) or ((dni <= dnik) and (dni <= dnink)) then begin
                      mincase := mincase or 2 ;
                    end ;
                  end ;
                end ;

                if vnk.NextVertex <> vnk then begin // last from open contour
                  if vnk.CommonVertex = nil then
                    testvertex := True
                  else
                  if (not areSpliced(vnk, vi)) and (not areSpliced(vnk, vni))
                  then
                    testvertex := True
                  else begin
                    testvertex := False ;
                    testline := False ;
                  end ;

                  if testvertex then begin
                    dnk := GisLine2Point(vi.WorkPoint2, vni.WorkPoint2, vnk.WorkPoint2) ;
                    if dnk < gridStep09 then begin
                      if (dnk = 0) or ((dnk <= dink) and (dni <= dnink)) then begin
                        mincase := mincase or 8 ;
                      end ;
                    end ;
                  end ;
                end ;
              end ;
              if mincase <> 0 then begin
                if (mincase and 1) <> 0 then begin
                  cross_ptg2  := _TGIS_Point  ( vi.WorkPoint2 ) ;
                  cross_ptg3i := _TGIS_Point3D( vi.WorkPoint3 ) ;
                  cross_ptg3k := _TGIS_Point3D( cross_ptg3i   ) ;
                  cross_ptg3k.Z := vk.WorkPoint3.Z ;
                  cross_ptg3k.M := vk.WorkPoint3.M ;
                  crossvertex := vi ;
                  dmin := di ;
                end
                else
                  dmin := FTolerance ;

                if (mincase and 2) <> 0 then begin

                  if dni < dmin then begin
                    cross_ptg2  := _TGIS_Point( vni.WorkPoint2 ) ;
                    cross_ptg3i := _TGIS_Point3D( vni.WorkPoint3 ) ;
                    cross_ptg3k := _TGIS_Point3D( cross_ptg3i ) ;
                    cross_ptg3k.Z := vk.WorkPoint3.Z ;
                    cross_ptg3k.M := vk.WorkPoint3.M ;
                    crossvertex := vni ;
                    mincase := mincase and $FFFE ;
                    dmin := dni ;
                  end
                  else
                    mincase := mincase and $FFFD ;
                end ;
                if (mincase and 4) <> 0 then begin
                  if dk <= dmin then begin
                    cross_ptg2  := _TGIS_Point( vk.WorkPoint2 ) ;
                    cross_ptg3k := _TGIS_Point3D( vk.WorkPoint3 ) ;
                    cross_ptg3i := _TGIS_Point3D( cross_ptg3k ) ;
                    cross_ptg3i.Z := vi.WorkPoint3.Z ;
                    cross_ptg3i.M := vi.WorkPoint3.M ;
                    crossvertex := vk ;
                    mincase := mincase and $FFFC ;
                    dmin := dk
                  end
                  else
                    mincase := mincase and $FFFB ;
                end ;

                if (mincase and 8) <> 0 then begin
                  if dnk < dmin then begin
                    cross_ptg2  := _TGIS_Point( vnk.WorkPoint2 ) ;
                    cross_ptg3k := _TGIS_Point3D( vnk.WorkPoint3 ) ;
                    cross_ptg3i := _TGIS_Point3D( cross_ptg3k ) ;
                    cross_ptg3i.Z := vi.WorkPoint3.Z ;
                    cross_ptg3i.M := vi.WorkPoint3.M ;
                    crossvertex := vnk ;
                  end ;
                end ;
                is_cross_ptg := True ;
              end ;

              if (not is_cross_ptg) and testline then begin
                line1.A := _TGIS_Point( T_Vertex(vk).WorkPoint2 ) ;
                line1.B := _TGIS_Point( T_Vertex(vnk).WorkPoint2 ) ;

                line2.A := _TGIS_Point( T_Vertex(vi).WorkPoint2 ) ;
                line2.B := _TGIS_Point( T_Vertex(vni).WorkPoint2 ) ;
                if GisGetLinesCrossing(line1, line2, cross_ptg2) then begin
                  if vnk.NextVertex <> vnk then begin // not last from open contour
                    dcnk := GisPoint2Point(cross_ptg2, T_Vertex(vnk).WorkPoint2) ;
                    if dcnk < gridStep09 then
                    begin
                      crossvertex := vnk ;
                      T_Vertex(vnk).WorkPoint2 := _TGIS_Point( cross_ptg2 ) ;
                      T_Vertex(vnk).WorkPoint3.X := cross_ptg2.X ;
                      T_Vertex(vnk).WorkPoint3.Y := cross_ptg2.Y ;
                      T_Vertex(vnk).WorkPoint3.Z := vi.WorkPoint3.Z ;
                      T_Vertex(vnk).WorkPoint3.M := vi.WorkPoint3.M ;

                      cross_ptg3k := _TGIS_Point3D( T_Vertex(vnk).WorkPoint3 ) ;
                    end
                    else begin
                      cross_ptg3k   := GisPoint3DFrom2D(cross_ptg2) ;
                      cross_ptg3k.Z := vk.WorkPoint3.Z ;
                      cross_ptg3k.M := vk.WorkPoint3.M ;
                    end ;
                  end
                  else begin
                    cross_ptg3k   := GisPoint3DFrom2D(cross_ptg2) ;
                    cross_ptg3k.Z := vk.WorkPoint3.Z ;
                    cross_ptg3k.M := vk.WorkPoint3.M ;
                  end ;
                  cross_ptg3i := _TGIS_Point3D( cross_ptg3k ) ;
                  cross_ptg3i.Z := vi.WorkPoint3.Z ;
                  cross_ptg3i.M := vi.WorkPoint3.M ;

                  is_cross_ptg  := True ;
                end ;
              end ;

              if is_cross_ptg then begin
                if (crossvertex <> vk ) and (crossvertex <> vnk ) then
                begin
                  svertex := nil ;
                  if crossvertex <> nil then begin
                    if crossvertex.CommonVertex <> nil then begin
                      if areSpliced(crossvertex, vk) or
                         areSpliced(crossvertex, vnk)
                      then
                        continue ;

                      if (vk.PrevVertex <> vk) and (vnk <> vnk.NextVertex) then
                      if areSpliced(crossvertex, vk.PrevVertex) then begin
                        if vk.PrevEdgeSite = T_EdgeSite.gesUndefined then begin
                          if vk.CommonVertex <> nil then
                            separateVertex(vk) ;
                          vk := vk.PrevVertex ;
                          FreeObject(vnk.PrevVertex) ;
                          vnk.PrevVertex := vk ;
                          vk.NextVertex := vnk ;
                          ended := False ;
                          {$IFDEF OXYGENE}
                            k_list.VerticesList.RemoveAt(k) ;
                          {$ELSE}
                            k_list.VerticesList.Delete(k) ;
                          {$ENDIF}
                          k_list.SetExtents(gridStep, k, False );
                          if GisPoint2Point(vnk.WorkPoint2,
                                            vk.WorkPoint2) < gridStep09 then
                          begin
                            if vnk.CommonVertex <> nil then
                              separateVertex(vnk) ;
                            vnk := vnk.NextVertex ;
                            FreeObject(vnk.PrevVertex) ;
                            vnk.PrevVertex := vk ;
                            vk.NextVertex := vnk ;
                            if nk <> 0 then begin
                              {$IFDEF OXYGENE}
                                k_list.VerticesList.RemoveAt(k) ;
                              {$ELSE}
                                k_list.VerticesList.Delete(k) ;
                              {$ENDIF}
                              k_list.SetExtents(gridStep, k, False );
                            end
                            else begin
                              {$IFDEF OXYGENE}
                                k_list.VerticesList.RemoveAt(nk) ;
                              {$ELSE}
                                k_list.VerticesList.Delete(nk) ;
                              {$ENDIF}
                              k_list.SetExtents(gridStep, nk, False );
                            end ;
                          end ;
                          dec( current_k ) ;
                          if current_k < 0 then
                            current_k := 0 ;
                        end ;
                          break ;
                      end ;

                      if (vnk.NextVertex <> vnk) and (vk <> vk.PrevVertex) then
                      if areSpliced(crossvertex, vnk.NextVertex) then begin
                        if vnk.CommonVertex = nil then begin
                          vnk := vnk.NextVertex ;
                          FreeObject(vnk.PrevVertex) ;
                          vnk.PrevVertex := vk ;
                          vk.NextVertex := vnk ;
                          ended := False ;
                          {$IFDEF OXYGENE}
                            k_list.VerticesList.RemoveAt(nk) ;
                          {$ELSE}
                            k_list.VerticesList.Delete(nk) ;
                          {$ENDIF}
                          k_list.SetExtents(gridStep, nk, False);
                        end
                        else
                        if vk.CommonVertex = nil then begin
                          vk := vk.PrevVertex ;
                          FreeObject(vk.NextVertex) ;
                          vk.NextVertex := vnk ;
                          vnk.PrevVertex := vk ;
                          ended := False ;
                          {$IFDEF OXYGENE}
                            k_list.VerticesList.RemoveAt(k) ;
                          {$ELSE}
                            k_list.VerticesList.Delete(k) ;
                          {$ENDIF}
                          k_list.SetExtents(gridStep, k, False);
                        end
                        else
                          ended := True ;

                      end ;
                      if not ended then
                        break ;

                    end ;
                  end ;
                  if crossvertex = vi then begin
                    dck  := dik ;
                    dcnk := dink ;
                  end
                  else
                  if crossvertex = vni then begin
                    dck  := dnik ;
                    dcnk := dnink ;
                  end
                  else begin //new
                    dck  := GisPoint2Point(cross_ptg2, T_Vertex(vk).WorkPoint2) ;
                    dcnk := GisPoint2Point(cross_ptg2, T_Vertex(vnk).WorkPoint2) ;
                  end ;

                  if dck < dcnk then begin
                    if dck < gridStep09 then begin
                      if crossvertex <> nil then begin
                        crossvertex.WorkPoint2 := _TGIS_Point  ( vk.WorkPoint2 ) ;
                        crossvertex.WorkPoint3 := _TGIS_Point3D( vk.WorkPoint3 ) ;
                      end
                      else begin
                        cross_ptg2  := _TGIS_Point  ( vk.WorkPoint2 ) ;
                        cross_ptg3k := _TGIS_Point3D( vk.WorkPoint3 ) ;
                        cross_ptg3i := _TGIS_Point3D( cross_ptg3k ) ;
                        cross_ptg3i.Z := vi.WorkPoint3.Z ;
                        cross_ptg3i.M := vi.WorkPoint3.M ;
                        crossvertex := vk ;
                      end ;
                      svertex := vk ;
                    end ;
                  end
                  else begin
                    if dcnk < gridStep09 then begin
                      if crossvertex <> nil then begin
                        crossvertex.WorkPoint2 := _TGIS_Point  ( vnk.WorkPoint2 ) ;
                        crossvertex.WorkPoint3 := _TGIS_Point3D( vnk.WorkPoint3 ) ;
                      end
                      else begin
                        cross_ptg2  := _TGIS_Point  ( vnk.WorkPoint2 ) ;
                        cross_ptg3k := _TGIS_Point3D( vnk.WorkPoint3 ) ;
                        cross_ptg3i := _TGIS_Point3D( cross_ptg3k ) ;
                        cross_ptg3i.Z :=  vi.WorkPoint3.Z ;
                        cross_ptg3i.M :=  vi.WorkPoint3.M ;
                        crossvertex := vnk ;
                      end ;
                      svertex := vnk ;
                    end ;
                  end ;
                  if svertex = nil then begin
                    svertex := T_Vertex.Create ;
                    svertex.NativePoint2 := _TGIS_Point( cross_ptg2 ) ;
                    cross_ptg3k.Z := vk.NativePoint3.Z ;
                    cross_ptg3k.M := vk.NativePoint3.M ;
                    cross_ptg3i.Z := vi.NativePoint3.Z ;
                    cross_ptg3i.M := vi.NativePoint3.M ;
                    if GisPoint2Point(cross_ptg2, vk.NativePoint2 ) <
                       GisPoint2Point(cross_ptg2, vi.NativePoint2 ) then begin
                      svertex.NativePoint3 := _TGIS_Point3D( cross_ptg3k ) ;
                      if svertex.NativePoint3.Z = 0 then
                        svertex.NativePoint3.Z := cross_ptg3i.Z ;
                    end
                    else begin
                      svertex.NativePoint3 := _TGIS_Point3D( cross_ptg3i ) ;
                      if svertex.NativePoint3.Z = 0 then
                        svertex.NativePoint3.Z := cross_ptg3k.Z ;
                    end;
                    svertex.WorkPoint2 := _TGIS_Point( svertex.NativePoint2 ) ;
                    svertex.WorkPoint3 := _TGIS_Point3D( svertex.NativePoint3 ) ;
                    svertex.Owner := T_Contour( contour1 );
                    svertex.PrevVertex := vk ;
                    svertex.NextVertex := vnk ;
                    T_Vertex(vk).NextVertex := svertex ;
                    T_Vertex(vnk).PrevVertex := svertex ;
                    if nk <> 0 then
                      T_Contour( contour1 ).PartVertices.VerticesList.Insert(
                                                                  nk, svertex
                                                                  )
                    else
                      T_Contour( contour1 ).PartVertices.VerticesList.Add(svertex) ;
                    T_Contour( contour1 ).PartVertices.SetExtents(gridStep, nk );

                    ended := False ;
                  end ;
                end
                else
                  svertex := crossvertex ;

                if (crossvertex <> vi ) and (crossvertex <> vni ) then
                begin
                  vertex := nil ;
                  if crossvertex <> nil then begin
                    if crossvertex.CommonVertex <> nil then begin
                      if areSpliced(crossvertex, vi) or
                         areSpliced(crossvertex, vni)
                      then
                        continue ;

                      if (vi.PrevVertex <> vi) and (vni <> vni.NextVertex) then
                      if areSpliced(crossvertex, vi.PrevVertex) then begin
                        if vi.PrevEdgeSite = T_EdgeSite.gesUndefined then begin
                          if vi.CommonVertex <> nil then
                            separateVertex(vi) ;
                          vi := vi.PrevVertex ;
                          FreeObject(vni.PrevVertex) ;
                          vni.PrevVertex := vi ;
                          vi.NextVertex := vni ;
                          ended := False ;
                          {$IFDEF OXYGENE}
                            i_list.VerticesList.RemoveAt(i) ;
                          {$ELSE}
                            i_list.VerticesList.Delete(i) ;
                          {$ENDIF}
                          i_list.SetExtents(gridStep, i, False);

                          if vni <> vni.NextVertex then
                          if GisPoint2Point(vni.WorkPoint2,
                                            vi.WorkPoint2) < gridStep09 then
                          begin
                            if vni.CommonVertex <> nil then
                              separateVertex(vni) ;
                            vni := vni.NextVertex ;
                            FreeObject(vni.PrevVertex) ;
                            vni.PrevVertex := vi ;
                            vi.NextVertex := vni ;
                            if ni <> 0 then begin
                              {$IFDEF OXYGENE}
                                i_list.VerticesList.RemoveAt(i) ;
                              {$ELSE}
                                i_list.VerticesList.Delete(i) ;
                              {$ENDIF}
                              i_list.SetExtents(gridStep, i, False);
                            end
                            else begin
                              {$IFDEF OXYGENE}
                                i_list.VerticesList.RemoveAt(ni) ;
                              {$ELSE}
                                i_list.VerticesList.Delete(ni) ;
                              {$ENDIF}
                              i_list.SetExtents(gridStep, ni, False);
                            end ;
                          end ;

                          dec( current_i ) ;
                          if current_i < 0 then
                            current_i := 0 ;
                        end ;
                        break ;
                      end ;

                      if (vni.NextVertex <> vni) and  (vi <> vi.PrevVertex) then
                      if areSpliced(crossvertex, vni.NextVertex) then begin
                        if vni.CommonVertex = nil then begin
                          vni := vni.NextVertex ;
                          FreeObject(vni.PrevVertex) ;
                          vni.PrevVertex := vi ;
                          vi.NextVertex := vni ;
                          ended := False ;
                          {$IFDEF OXYGENE}
                            i_list.VerticesList.RemoveAt(ni) ;
                          {$ELSE}
                            i_list.VerticesList.Delete(ni) ;
                          {$ENDIF}
                          i_list.SetExtents(gridStep, ni, False);
                        end
                        else
                        if vi.CommonVertex = nil then begin
                          vi := vi.PrevVertex ;
                          FreeObject(vi.NextVertex) ;
                          vi.NextVertex := vni ;
                          vni.PrevVertex := vi ;
                          ended := False ;
                          {$IFDEF OXYGENE}
                            i_list.VerticesList.RemoveAt(i) ;
                          {$ELSE}
                            i_list.VerticesList.Delete(i) ;
                          {$ENDIF}
                          i_list.SetExtents(gridStep, i, False);
                        end
                        else
                          ended := True ;

                      end ;
                      if not ended then
                        break ;

                    end ;
                  end ;

                  if crossvertex = vk then begin
                    dci  := dik ;
                    dcni := dnik ;
                  end
                  else
                  if crossvertex = vnk then begin
                    dci  := dink ;
                    dcni := dnink ;
                  end
                  else begin //new
                    dci  := GisPoint2Point(cross_ptg2, T_Vertex(vi).WorkPoint2) ;
                    dcni := GisPoint2Point(cross_ptg2, T_Vertex(vni).WorkPoint2) ;
                  end ;

                  if dci < dcni then begin
                    if dci < gridStep09 then begin
                      if crossvertex <> nil then begin
                        vi.WorkPoint2 := _TGIS_Point( crossvertex.WorkPoint2 ) ;
                        vi.WorkPoint3.X := crossvertex.WorkPoint3.X ;
                        vi.WorkPoint3.Y := crossvertex.WorkPoint3.Y ;
                      end
                      else begin
                        vi.WorkPoint2 := _TGIS_Point( cross_ptg2 ) ;
                        cross_ptg3i.Z := vi.NativePoint3.Z ;
                        cross_ptg3i.M := vi.NativePoint3.M ;
                        cross_ptg3k.Z := vk.NativePoint3.Z ;
                        cross_ptg3k.M := vk.NativePoint3.M ;
                        vi.WorkPoint3.X := cross_ptg3i.X ;
                        vi.WorkPoint3.Y := cross_ptg3i.Y ;
                      end ;
                      vertex := vi ;
                    end ;
                  end
                  else begin
                    if dcni < gridStep09 then begin
                      if crossvertex <> nil then begin

                        vni.WorkPoint2 := _TGIS_Point  ( crossvertex.WorkPoint2 ) ;
                        vni.WorkPoint3 := _TGIS_Point3D( crossvertex.WorkPoint3 ) ;
                      end
                      else begin
                        vni.WorkPoint2 := _TGIS_Point( cross_ptg2 ) ;
                        cross_ptg3i.Z := vni.NativePoint3.Z ;
                        cross_ptg3i.M := vni.NativePoint3.M ;
                        vni.WorkPoint3 := cross_ptg3i ;
                      end ;
                      vertex := vni ;
                    end ;
                  end ;

                  if vertex = nil then begin
                   //adding cross vertex to the contour2
                    vertex := T_Vertex.Create ;
                    vertex.NativePoint2 := _TGIS_Point  ( svertex.NativePoint2 ) ;
                    vertex.NativePoint3 := _TGIS_Point3D( svertex.NativePoint3 ) ;
                    vertex.NativePoint3.Z := svertex.WorkPoint3.Z ;
                    vertex.NativePoint3.M := svertex.WorkPoint3.M ;
                    vertex.WorkPoint2 := _TGIS_Point  ( svertex.WorkPoint2 ) ;
                    vertex.WorkPoint3 := _TGIS_Point3D( svertex.WorkPoint3 ) ;
                    vertex.Owner := T_Contour( contour2 ) ;
                    vertex.PrevVertex := vi ;
                    vertex.NextVertex := vni ;
                    T_Vertex(vi).NextVertex := vertex ;
                    T_Vertex(vni).PrevVertex := vertex ;
                    if ni <> 0 then
                      T_Contour( contour2 ).PartVertices.VerticesList.Insert(
                      ni, vertex
                      )
                    else
                      T_Contour( contour2 ).PartVertices.VerticesList.Add(vertex) ;
                    i_list.SetExtents(gridStep, ni );
                    ended := False ;
                  end ;
                end
                else
                  vertex := crossvertex ;

                spliceVertices(vertex, svertex) ;
              end ;
              if not ended then
                break ;
            end ; //end of for i loop

            if ended then
              current_i  := 0
            else
              break ;

          end ;  //end of for ii loop

          if not ended then
            break ;
          current_ii := 0 ;
        end ; //end of for k loop
        if doBreak then
          break ;
      until ended ;
      if doBreak then
        break ;
      oldk := oldk + k_list.VerticesList.Count ;
    end ;   //end of for kk loop

    if busyStep <> 0 then begin
      inProgress := inProgress +busyStep ;
      busyStep := 0 ;
    end ;

    T_Contour(contour2).FreeExtentsArrays ;
    T_Contour(contour1).FreeExtentsArrays ;

    if cs_changed then begin
      cc := contour1 ;
      contour1 := contour2 ;
      contour2 := cc ;
    end ;
  end ;

  procedure TGIS_Topology.addCrossVrtxDesc(
    const _contour : TObject
  ) ;
  var
    dprev  : T_CrossVertexDescriptor ;
    dnext  : T_CrossVertexDescriptor ;
    k, kk  : Integer  ;
    vertex : T_Vertex ;
  begin

     for kk := 0 to T_Contour( _contour ).PartsCount -1 do begin
      T_Contour( _contour ).Part := kk ;
      for k := 0 to T_Contour( _contour ).PartVertices.VerticesList.Count -1 do
      begin
        vertex := T_Vertex(
                    T_Contour( _contour ).PartVertices.VerticesList[k]
                  ) ;
        if vertex.IsCrossVertex then begin
          // connectivity list (cross-vertex descriptors list)
          // generating or updating
          vdescriptors := getVdescriptors(vertex);

          dprev := T_CrossVertexDescriptor.Create ;
          dnext := T_CrossVertexDescriptor.Create ;
          dprev.DescriptorType := T_DescriptorType.gdtPrevious ;
          dnext.DescriptorType := T_DescriptorType.gdtNext ;
          dprev.CorrespondingVertex := vertex;
          dnext.CorrespondingVertex := vertex;
          vertex.DescriptorsList := vdescriptors ;
          vertex.PrevDescriptor := dprev ;
          vertex.NextDescriptor := dnext ;

          vdescriptors.Add(dprev);
          vdescriptors.Add(dnext);
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_Topology.collectPolygonCPart(
    const _vertex    : TObject ;
    const _direction : Integer ;
    const _cpart     : TObject
  ) ;
  var
    vertex    : T_Vertex ;
    lvertex   : T_Vertex ;
    pvertex   : T_Vertex ;
    fvertex   : T_Vertex ;
    {$IFDEF MANAGED}
      obj     : TObject  ;
      ret     : BOOL     ;
      cp      : T_ContourPart ;
    {$ENDIF}
    direction : Integer  ;
    edge      : T_Edge   ;
    idx       : Integer  ;
    xm, ym    : Double ;
  begin
    vertex := T_Vertex( _vertex ) ;
    pvertex := nil ;
    fvertex := nil ;
    direction := _direction ;

    {$IFDEF MANAGED}
      cp := T_ContourPart(_cpart) ;
      cp.Extent.XMin := T_Vertex(_vertex).WorkPoint2.X ;
      cp.Extent.YMin := T_Vertex(_vertex).WorkPoint2.Y ;
      cp.Extent.XMax := T_Vertex(_vertex).WorkPoint2.X ;
      cp.Extent.YMax := T_Vertex(_vertex).WorkPoint2.Y ;
    {$ELSE}
      T_ContourPart(_cpart).Extent.XMin := T_Vertex(_vertex).WorkPoint2.X ;
      T_ContourPart(_cpart).Extent.YMin := T_Vertex(_vertex).WorkPoint2.Y ;
      T_ContourPart(_cpart).Extent.XMax := T_Vertex(_vertex).WorkPoint2.X ;
      T_ContourPart(_cpart).Extent.YMax := T_Vertex(_vertex).WorkPoint2.Y ;
    {$ENDIF}
    xm := T_Vertex(_vertex).WorkPoint2.X ;
    ym := T_Vertex(_vertex).WorkPoint2.Y ;

    repeat
      lvertex := T_Vertex.Create ;
      lvertex.WorkPoint2 := _TGIS_Point (vertex.WorkPoint2) ;
      lvertex.WorkPoint3 := _TGIS_Point3D (vertex.WorkPoint3) ;
      lvertex.NativePoint2 := _TGIS_Point (vertex.NativePoint2) ;
      lvertex.NativePoint3 := _TGIS_Point3D (vertex.NativePoint3) ;
      lvertex.NativePoint3.Z := lvertex.WorkPoint3.Z ;
      lvertex.NativePoint3.M := lvertex.WorkPoint3.M ;

      T_ContourPart(_cpart).VerticesList.Add(lvertex) ;
      if fvertex = nil then
        fvertex := lvertex ;

      if (pvertex <> nil) then begin

        {$IFDEF MANAGED}
          if cp.Extent.XMin > lvertex.NativePoint2.X then
            cp.Extent.XMin := lvertex.NativePoint2.X
          else
          if cp.Extent.XMax < lvertex.NativePoint2.X then
            cp.Extent.XMax := lvertex.NativePoint2.X ;

          if cp.Extent.YMin > lvertex.NativePoint2.Y then
            cp.Extent.YMin := lvertex.NativePoint2.Y
          else
          if cp.Extent.YMax < lvertex.NativePoint2.Y then
            cp.Extent.YMax := lvertex.NativePoint2.Y ;
        {$ELSE}
          if T_ContourPart(_cpart).Extent.XMin > lvertex.NativePoint2.X then
            T_ContourPart(_cpart).Extent.XMin := lvertex.NativePoint2.X
          else
          if T_ContourPart(_cpart).Extent.XMax < lvertex.NativePoint2.X then
            T_ContourPart(_cpart).Extent.XMax := lvertex.NativePoint2.X ;

          if T_ContourPart(_cpart).Extent.YMin > lvertex.NativePoint2.Y then
            T_ContourPart(_cpart).Extent.YMin := lvertex.NativePoint2.Y
          else
          if T_ContourPart(_cpart).Extent.YMax < lvertex.NativePoint2.Y then
            T_ContourPart(_cpart).Extent.YMax := lvertex.NativePoint2.Y ;
        {$ENDIF}

        pvertex.NextVertex := lvertex ;
        lvertex.PrevVertex := pvertex ;
        T_ContourPart(_cpart).SignedArea2 :=
              T_ContourPart(_cpart).SignedArea2
                +(pvertex.NativePoint2.X -xm)*(lvertex.NativePoint2.Y -ym)
                -(pvertex.NativePoint2.Y -ym)*(lvertex.NativePoint2.X -xm) ;

      end ;

      pvertex := lvertex ;
      if direction = ord( T_EdgeDirection.gedForward ) then begin
        edge := vertex.NextEdge ;
        vertex := vertex.NextVertex ;
      end
      else begin
        edge := vertex.PrevEdge ;
        vertex := vertex.PrevVertex ;
      end ;

      if not assigned( edge ) then begin
        break ;
      end ;
      if edge.Marked then
        break
      else
        edge.Marked := True ;

      if (edge.Site = T_EdgeSite.gesSharedSD) or (edge.Site = T_EdgeSite.gesSharedCD) then
        if assigned( edge.CommonEdge ) then
          edge.CommonEdge.Marked := True ;

      if vertex.IsCrossVertex then begin
        {$IFDEF MANAGED}
          obj := vertex ;
          ret := jumpAccordingToDesc( obj , direction );
          if vertex <> T_Vertex(obj) then
            vertex := T_Vertex(obj) ;
          if not ret then
        {$ELSE}
          if not jumpAccordingToDesc( TObject( vertex ), direction) then
        {$ENDIF}
          break ;
      end ;

    until(False) ;

    if (lvertex.NativePoint2.X = fvertex.NativePoint2.X) and
       (lvertex.NativePoint2.Y = fvertex.NativePoint2.Y) then
    begin
      if lvertex.PrevVertex <> nil then begin
        lvertex.PrevVertex.NextVertex := fvertex ;
        fvertex.PrevVertex := lvertex.PrevVertex ;
        idx := T_ContourPart(_cpart).VerticesList.Count -1 ;
        {$IFDEF OXYGENE}
          T_ContourPart(_cpart).VerticesList.RemoveAt(idx);
        {$ELSE}
          T_ContourPart(_cpart).VerticesList.Delete(idx);
        {$ENDIF}
        FreeObject( lvertex ) ;
      end ;
    end
    else
    begin

      lvertex.NextVertex := fvertex ;
      fvertex.PrevVertex := lvertex ;

      T_ContourPart(_cpart).SignedArea2 :=    T_ContourPart(_cpart).SignedArea2
                +(lvertex.NativePoint2.X -xm)*(fvertex.NativePoint2.Y -ym)
                -(lvertex.NativePoint2.Y -ym)*(fvertex.NativePoint2.X -xm);

    end ;
  end ;

  function TGIS_Topology.jumpAccordingToDesc(
    var _vertex    : TObject ;
    var _direction : Integer
  ) : Boolean;
  var
    desc : T_CrossVertexDescriptor ;
    nextdesc : T_CrossVertexDescriptor ;
    vertex : T_Vertex ;
    nextvertex : T_Vertex ;
    nextdirection : Integer ;
    olddirection : Integer ;
    didx : Integer ;
    dlist : TList<TObject> ;
    dcount : Integer ;
    loops : Integer ;
    edge : T_Edge ;
  begin
    vertex := T_Vertex( _vertex ) ;
    dlist := vertex.DescriptorsList ;
    dcount := dlist.Count ;

    if _direction = ord( T_EdgeDirection.gedForward ) then begin
      desc := vertex.PrevDescriptor ;
      didx := dlist.IndexOf(desc);
      dec( didx ) ;
      if didx < 0 then
        didx := dcount -1 ;
      nextdesc := T_CrossVertexDescriptor(dlist[didx]) ;
    end
    else
    begin
      desc := vertex.NextDescriptor ;
      didx := dlist.IndexOf(desc);
      dec( didx ) ;
      if didx < 0 then
        didx := dcount -1 ;
      nextdesc := T_CrossVertexDescriptor(dlist[didx]) ;
    end ;

    Result := False ;
    loops := 0 ;
    repeat
      nextvertex := nextdesc.CorrespondingVertex ;
      if nextdesc.DescriptorType = T_DescriptorType.gdtNext then begin
        nextdirection := ord( T_EdgeDirection.gedForward ) ;
        edge := nextvertex.NextEdge ;
      end
      else begin
        nextdirection := ord( T_EdgeDirection.gedBackward ) ;
        edge := nextvertex.PrevEdge ;
      end ;
      olddirection := nextdirection ;

      if edge <> nil then begin
        if ( not edge.Marked ) and
           edgeRule(edge, nextdirection ) then
        begin
          if olddirection = nextdirection then
           begin
            _direction := nextdirection ;
            _vertex := nextvertex ;
            Result := True ;
            exit ;
          end ;
        end ;
      end ;

      inc( didx ) ;
      if didx >= dcount then
        didx := 0 ;
      nextdesc := T_CrossVertexDescriptor(dlist[didx]) ;
      inc( loops ) ;
      if loops > dlist.Count then
        break ;
    until(Result) ;
  end ;

  procedure TGIS_Topology.computeEdgesAngles(
    const _vdescriptors : TList<TObject>
  ) ;
  var
    i          : Integer  ;
    descriptor : T_CrossVertexDescriptor ;
    fvertex,
    nvertex    : T_Vertex ;
    vx, vy     : Double   ;
    angle      : Double   ;
  begin
    for i := 0 to _vdescriptors.Count -1 do begin
      descriptor := T_CrossVertexDescriptor(_vdescriptors[i]);
      fvertex := descriptor.CorrespondingVertex ;
      if descriptor.DescriptorType = T_DescriptorType.gdtPrevious then begin
        nvertex := fvertex.PrevVertex ;
      end
      else begin
        nvertex := fvertex.NextVertex ;
      end ;

      if nvertex.WorkPoint2.X = fvertex.WorkPoint2.X then begin
        if fvertex.WorkPoint2.Y > nvertex.WorkPoint2.Y then
          angle := 0.5 * Pi
        else
        if fvertex.WorkPoint2.Y < nvertex.WorkPoint2.Y then
          angle := 1.5 * Pi
        else
          angle := 0 ;
      end
      else begin
        vx := nvertex.WorkPoint2.X -fvertex.WorkPoint2.X ;
        vy := nvertex.WorkPoint2.Y -fvertex.WorkPoint2.Y ;

        angle := ArcTan2(vy, vx);
        if angle < 0 then
          angle := angle + 2 * Pi ;
      end ;
      (T_CrossVertexDescriptor(_vdescriptors[i])).EdgeAngle := angle ;
    end ;

  end ;

  procedure TGIS_Topology.createEdgesAndDfnSite(
    const _contour     : TObject ;
    const _nextcontour : TObject
  ) ;
  var
    i, k              : Integer    ;
    last_edge_site    : T_EdgeSite ;
    vertex            : T_Vertex   ;
    nvertex           : T_Vertex   ;
    nnvertex          : T_Vertex   ;
    cvertex           : T_Vertex   ;
    testpoint         : TGIS_Point ;
    edge              : T_Edge     ;
    short_path        : Boolean    ;
    {$IFDEF OXYGENE}
      bargs           : TGIS_BusyEventArgs ;
    {$ENDIF}

    function are_spliced( const _v1, _v2 : T_Vertex) : Boolean ;
    var
      nv : T_Vertex ;
    begin
      Result := False ;
      if (_v1.CommonVertex = nil) or (_v2.CommonVertex = nil) then
        exit ;
      nv := _v1.CommonVertex ;
      while True do begin
        if nv = _v2 then begin
          Result := True ;
          break ;
        end ;
        if nv = _v1 then
          break ;
        nv := nv.CommonVertex ;
      end ;
    end ;

    function find_sharedSD_vertex(const _vertex : T_Vertex) : T_Vertex ;
    var
      sv : T_Vertex ;
    begin
      Result := nil ;
      sv := _vertex.CommonVertex ;
      while sv <> _vertex do begin
        if (sv.Owner <> _vertex.Owner) then begin
          if assigned( sv.NextEdge )  then begin
            if sv.NextEdgeSite = T_EdgeSite.gesSharedSD then begin
              if are_spliced(sv.NextVertex, _vertex.NextVertex) then begin
                Result := sv ;
                exit ;
              end ;
            end ;
          end ;
        end ;
        sv := sv.CommonVertex
      end ;
    end ;

    function find_sharedCD_vertex(const _vertex : T_Vertex) : T_Vertex ;
    var
      sv : T_Vertex ;
    begin
      Result := nil ;
      sv := _vertex.CommonVertex ;
      while sv <> _vertex do begin
        if (sv.Owner <> _vertex.Owner) then begin
          if assigned( sv.PrevEdge )  then begin
            if sv.PrevEdgeSite = T_EdgeSite.gesSharedCD then begin
              if are_spliced(sv.PrevVertex, _vertex.NextVertex) then begin
                Result := sv ;
                exit ;
              end ;
            end ;
          end ;
        end ;
        sv := sv.CommonVertex
      end ;
    end ;

  begin

    if not assigned( listOfEdges ) then begin
      listOfEdges := TList<TObject>.Create ;
      {$IFNDEF JAVA OR ISLAND}
      listOfEdges.Capacity := T_Contour( contour1 ).Count +
                              T_Contour( contour2 ).Count;
      {$ENDIF}
    end ;

    for k := 0 to T_Contour( _contour ).PartsCount -1 do begin
      T_Contour( _contour ).Part := k ;
      vertex := T_Contour( _contour ).PartVertices.VerticesList[0] ;
      nvertex := vertex.NextVertex ;

      last_edge_site := T_EdgeSite.gesUndefined ;

      for i := 0 to T_Contour( _contour ).PartVertices.VerticesList.Count -1 do
      begin

        if busyUsed then begin
          if i mod GIS_PROGRESS_TRESHOLD = 0 then begin
            {$IFDEF OXYGENE}
              bargs := TGIS_BusyEventArgs.Create( inProgress,
                                                  fullProgress,
                                                  doBreak
                                                ) ;
              try
                FOnBusy( self, bargs ) ;
              finally
                doBreak := bargs.Abort ;
                FreeObject( bargs ) ;
              end ;
            {$ELSE}
              FOnBusy( self, inProgress, fullProgress, doBreak ) ;
            {$ENDIF}
            if doBreak then begin
              break ;
            end ;
          end ;
        end ;

        if i > 0 then begin
          vertex := nvertex ;
          nvertex := vertex.NextVertex ;
          if nvertex = vertex then break ;
        end ;

        edge := T_Edge.Create ;
        listOfEdges.Add(edge) ;

        edge.PrevVertex := vertex ;
        edge.NextVertex := nvertex ;
        vertex.NextEdge := edge ;
        nvertex.PrevEdge := edge ;

        if vertex.NextEdgeSite = T_EdgeSite.gesUndefined then begin
          short_path := False ;
          if vertex.IsCrossVertex and nvertex.IsCrossVertex then begin
            if are_spliced(nvertex.CommonVertex.PrevVertex, vertex)  then
            begin
              vertex.NextEdgeSite := T_EdgeSite.gesSharedSD ;
              nvertex.PrevEdgeSite := T_EdgeSite.gesSharedSD ;
              cvertex := vertex.CommonVertex ;
              while (cvertex.Owner = vertex.Owner) do
                cvertex := cvertex.CommonVertex ;
              cvertex.NextEdgeSite := T_EdgeSite.gesSharedSD ;
              cvertex.NextVertex.PrevEdgeSite := T_EdgeSite.gesSharedSD ;
              short_path := True ;
            end
            else
            if are_spliced(nvertex.CommonVertex.NextVertex, vertex)  then
            begin
              vertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
              nvertex.PrevEdgeSite := T_EdgeSite.gesSharedCD ;
              cvertex := vertex.CommonVertex ;
              while (cvertex.Owner = vertex.Owner) do
                cvertex := cvertex.CommonVertex ;
              cvertex.PrevEdgeSite := T_EdgeSite.gesSharedCD ;
              cvertex.PrevVertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
              short_path := True ;
            end
            else
            if are_spliced(nvertex, vertex.CommonVertex.PrevVertex)  then
            begin
              vertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
              nvertex.PrevEdgeSite := T_EdgeSite.gesSharedCD ;
              cvertex := vertex.CommonVertex ;
              while (cvertex.Owner = vertex.Owner) do
                cvertex := cvertex.CommonVertex ;
              cvertex.PrevEdgeSite := T_EdgeSite.gesSharedCD ;
              cvertex.PrevVertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
              short_path := True ;
            end

          end ;

          if not short_path then begin

            if (not vertex.IsCrossVertex) and
               (vertex.PrevEdgeSite <> T_EdgeSite.gesUndefined)
            then
                last_edge_site := vertex.PrevEdgeSite
            else
            begin
              if nvertex.CommonVertex = nil  then begin

                nnvertex := nvertex.NextVertex ;

                if nnvertex.CommonVertex = nil then begin
                  testpoint := _TGIS_Point (nnvertex.WorkPoint2) ;
                end
                else begin
                  testpoint := _TGIS_Point (nvertex.WorkPoint2) ;
                end ;
              end
              else begin
                if not vertex.IsCrossVertex then begin
                  testpoint := _TGIS_Point (vertex.WorkPoint2) ;
                end
                else
                if not nvertex.IsCrossVertex then begin
                  testpoint := _TGIS_Point (nvertex.WorkPoint2) ;
                end
                else begin
                  {$IFDEF GIS_NORECORDS}
                    testpoint := new TGIS_Point ;
                  {$ENDIF}
                  testpoint.X :=
                      ( vertex.WorkPoint2.X + nvertex.WorkPoint2.X ) / 2 ;
                  testpoint.Y :=
                      ( vertex.WorkPoint2.Y + nvertex.WorkPoint2.Y ) / 2 ;
                end ;

              end ;

              if T_Contour( _nextcontour ).IsPointInsideContour( testpoint )
              then begin
                last_edge_site := T_EdgeSite.gesInside ;
                T_Contour( _contour ).FIsEdgeInside := True ;
              end
              else begin
                last_edge_site := T_EdgeSite.gesOutside ;
                T_Contour( _contour ).FIsEdgeOutside := True ;
              end ;
            end ;

            vertex.NextEdgeSite := last_edge_site ;
            nvertex.PrevEdgeSite := last_edge_site ;
          end ;
        end ;

        edge.Site := vertex.NextEdgeSite ;
        if assigned( vertex.CommonVertex ) then begin
          if edge.Site = T_EdgeSite.gesSharedSD then
          begin
            cvertex := find_sharedSD_vertex(vertex) ;
            if assigned( cvertex ) then begin
              cvertex.NextEdge.CommonEdge := edge ;
              edge.CommonEdge := cvertex.NextEdge ;
            end ;
          end
          else
          if edge.Site = T_EdgeSite.gesSharedCD then
          begin
            cvertex := find_sharedCD_vertex(vertex) ;
            if assigned( cvertex ) then begin
              cvertex.PrevEdge.CommonEdge := edge ;
              edge.CommonEdge := cvertex.PrevEdge ;
            end ;
          end ;
        end ;
      end ;

      if doBreak then
         break ;
    end ;
  end ;

  procedure TGIS_Topology.freeDescriptors(
    const _vdescriptors : TList<TObject>
  ) ;
  var
    i : Integer ;
  begin
    {$IFNDEF NEXTGEN}
      for i := _vdescriptors.Count -1 downto 0 do begin
        FreeObjectNotNil(T_CrossVertexDescriptor(_vdescriptors[i])) ;
      end ;
      FreeObjectNotNil( _vdescriptors ) ;
    {$ENDIF}
  end ;

  function TGIS_Topology.combinePolygons(
    const _shpA      : TGIS_ShapePolygon ;
    const _shpB      : TGIS_ShapePolygon ;
    const _operation : TGIS_TopologyCombineType
  ) : TGIS_Shape ;
  var
    outContour : T_Contour ;
  begin
    Result := nil ;

    if      _shpA.GetPartSize(0) < 3 then begin
              if (_operation = TGIS_TopologyCombineType.Intersection) or
                 (_operation = TGIS_TopologyCombineType.Difference)
              then
                exit ;
              if _shpB.GetPartSize(0) >= 3 then
                Result := TGIS_ShapePolygon(_shpB.CreateCopy) ;
              exit ;
    end
    else if _shpB.GetPartSize(0) < 3 then begin
              if (_operation = TGIS_TopologyCombineType.Intersection) then
                exit ;
              Result := TGIS_ShapePolygon(_shpA.CreateCopy) ;
              exit ;
            end ;

    pshape1 := _shpA ;
    pshape2 := _shpB ;
    computeTolerance ;

    contour1 := T_Contour.Create( CONTOUR_CLOSED, _shpA, self) ;
    if T_Contour( contour1 ).Count = 0 then begin
      FreeObject( contour1 ) ;
      if (_operation = TGIS_TopologyCombineType.Intersection) or
         (_operation = TGIS_TopologyCombineType.Difference)
      then
        exit ;

      Result := TGIS_ShapePolygon(_shpB.CreateCopy) ;
      exit ;
    end ;
    contour2 := T_Contour.Create( CONTOUR_CLOSED, _shpB, self) ;

    if T_Contour( contour2 ).Count = 0 then begin
      FreeObject( contour1 ) ;
      FreeObject( contour2 ) ;
      if _operation <> TGIS_TopologyCombineType.Intersection then
        Result := TGIS_ShapePolygon(_shpA.CreateCopy) ;
      exit ;
    end ;

    outContour := T_Contour(combineCPolygons(contour1, contour2,
                                                _operation )) ;
    FreeObject( contour2 ) ;
    FreeObject( contour1 ) ;

    if outContour <> nil then begin
      if outContour.Count > 0 then begin
        outContour.setHolesEx(self) ; ;
        Result :=  TGIS_ShapePolygon.Create( nil, nil, False, 0, _shpA.Layer,
                     _shpA.Dimension ) ;
        if GisLockGreaterThanEqual( _shpA.LockLevel, TGIS_Lock.Projection ) then
          Result.Lock(_shpA.LockLevel)
        else
          Result.Lock(TGIS_Lock.Extent) ;
        outContour.Write2Shape(Result) ;
        Result.IsFixed := True ;
        Result.Unlock ;
      end ;
      FreeObject( outContour ) ;
    end ;

  end ;

  function TGIS_Topology.combineCPolygons(
    const _contourA   : TObject ;
    const _contourB   : TObject ;
    const _operation  : TGIS_TopologyCombineType
  ) : TObject ;
  var
    cpart         : T_ContourPart     ;
    i, k          : Integer           ;
    edge          : T_Edge            ;
    direction     : Integer           ;
    sc1, sc2      : TObject           ;
    {$IFDEF MANAGED}
      res         : T_Contour ;
    {$ENDIF}

    procedure free_edges ;
    var
      k1 : Integer ;
    begin
      for k1 := listOfEdges.Count -1 downto 0 do begin
        {$IFNDEF NEXTGEN}
          FreeObjectNotNil( T_Edge(listOfEdges[k1]) ) ;
        {$ENDIF}
      end ;
      FreeObject(listOfEdges) ;
    end ;

  begin
    sc1 := contour1 ;
    sc2 := contour2 ;

    contour1 := _contourA ;
    contour2 := _contourB ;

    Result := nil ;


    // Computing contour1 and contour2 cross-vertex with adding them to the
    // contours. Simultaneously edges and cross-vertex descriptors
    // will be created
    fixCrossVertices ;
    if doBreak then begin
      contour1 := sc1 ;
      contour2 := sc2 ;
      exit ;
    end ;

    // edges site defining
    createEdgesAndDfnSite( contour1, contour2 ) ;
    if doBreak then begin
      free_edges ;
      contour1 := sc1 ;
      contour2 := sc2 ;
      exit ;
    end ;

    createEdgesAndDfnSite( contour2, contour1 ) ;
    if doBreak then begin
      free_edges ;
      contour1 := sc1 ;
      contour2 := sc2 ;
      exit ;
    end ;

    Result :=  T_Contour.Create ;

    combineType := _operation ;

    listOfVdescriptors := TList<TObject>.Create ;

    // adding descriptors
    addCrossVrtxDesc( contour1 ) ;

    // adding descriptors
    addCrossVrtxDesc( contour2 ) ;


    // edges angles computing
    for i := 0 to listOfVdescriptors.Count -1 do
      computeEdgesAngles( TList<TObject>(listOfVdescriptors[i]) ) ;

    // connectivity lists sorting
    for i := 0 to listOfVdescriptors.Count -1 do
      {$IFNDEF OXYGENE}
        TList<TObject>( listOfVdescriptors.Items[i] ).Sort( TComparer<TObject>.Construct( AnglesVerticesCompare ) ) ;
      {$ELSE}
        TList<TObject>( listOfVdescriptors[i] ).Sort( @AnglesVerticesCompare ) ;
      {$ENDIF}

    // making common contour
    commonContour := T_Contour( contour1 ).JoinContour( T_Contour( contour2 ) ) ;


    // work polygon
    cpart := T_ContourPart.Create ;

    for i := 0 to listOfEdges.Count -1 do begin
      edge := T_Edge(listOfEdges[i]) ;
      if edge.Marked then
        continue ;

      if edgeRule(edge, direction) then begin
        if direction = ord( T_EdgeDirection.gedForward ) then
          collectPolygonCPart(edge.PrevVertex, direction, cpart)
        else
          collectPolygonCPart(edge.NextVertex, direction, cpart) ;
      end ;

      if cpart.VerticesList.Count >= 3 then begin
        if Abs(cpart.SignedArea2) > FTolerance then begin
          if T_Contour(Result).PartsCount = 0 then
            T_Contour(Result).FExtent := _TGIS_Extent( cpart.Extent )
          else begin
            {$IFDEF MANAGED}
              res := T_Contour( Result ) ;
              if res.FExtent.XMin > cpart.Extent.XMin then
                res.FExtent.XMin := cpart.Extent.XMin ;
              if res.FExtent.XMax < cpart.Extent.XMax then
                res.FExtent.XMax := cpart.Extent.XMax ;
              if res.FExtent.YMin > cpart.Extent.YMin then
                res.FExtent.YMin := cpart.Extent.YMin ;
              if res.FExtent.YMax < cpart.Extent.YMax then
                res.FExtent.YMax := cpart.Extent.YMax ;
            {$ELSE}
              if T_Contour(Result).FExtent.XMin > cpart.Extent.XMin then
                T_Contour(Result).FExtent.XMin := cpart.Extent.XMin ;
              if T_Contour(Result).FExtent.XMax < cpart.Extent.XMax then
                T_Contour(Result).FExtent.XMax := cpart.Extent.XMax ;
              if T_Contour(Result).FExtent.YMin > cpart.Extent.YMin then
                T_Contour(Result).FExtent.YMin := cpart.Extent.YMin ;
              if T_Contour(Result).FExtent.YMax < cpart.Extent.YMax then
                T_Contour(Result).FExtent.YMax := cpart.Extent.YMax ;
            {$ENDIF}
          end ;
          cpart.SetOwner(T_Contour(Result)) ;
          T_Contour(Result).AddPart(cpart) ;
          cpart := T_ContourPart.Create ;
        end
        else
          cpart.Clear ;
      end
      else
        cpart.Clear ;
    end ;


    // on end objects destroying
    for k := listOfVdescriptors.Count -1 downto 0 do begin
      vdescriptors := TList<TObject>( listOfVdescriptors[k] ) ;
      freeDescriptors( vdescriptors ) ;
    end  ;

    FreeObject( listOfVdescriptors ) ;

    T_Contour(commonContour).Clear ;

    FreeObject( commonContour ) ;

    if cpart.VerticesList.Count = 0 then
      FreeObject( cpart ) ;

    free_edges ;


    if T_Contour(Result).FPartsCount = 0 then begin
      FreeObject( Result ) ;
    end
    else begin

      T_Contour(Result).isClosed := True ;
      T_Contour(Result).gridStep := T_Contour(contour1).gridStep ;
      T_Contour(Result).gridStep005 := T_Contour(contour1).gridStep005 ;
      T_Contour(Result).listOfParts.Sort( {$IFDEF OXYGENE}
                                            @PartsAreasCompare
                                          {$ELSE}
                                            TComparer<T_ContourPart>.Construct(PartsAreasCompare)
                                          {$ENDIF}
                                        ) ;
      if checkCombine then
        T_Contour(Result).checkAllVertices ;

    end ;

    contour1 := sc1 ;
    contour2 := sc2 ;
  end ;

  function TGIS_Topology.combineArcPolygon(
     const _shpA      : TGIS_ShapeArc ;
     const _shpB      : TGIS_ShapePolygon ;
     const _operation : TGIS_TopologyCombineType
   ) : TGIS_Shape ;
  var
    i, k       : Integer ;
    edge       : T_Edge  ;
    collect    : Boolean ;
    part_size  : Integer ;
    res        : TGIS_Shape ;

    procedure add_to_result( const _shp : TGIS_Shape ) ;
    var
      p, ps, m : Integer ;
    begin
      for p := 0 to _shp.GetNumParts -1 do begin
        ps := _shp.GetPartSize( p ) ;
        res.AddPart ;
        for m := 0 to ps -1 do
          res.AddPoint( _shp.GetPoint( p, m ) );
      end ;
    end ;

  begin
    if _shpB.GetPartSize( 0 ) < 3 then begin
      if _operation = TGIS_TopologyCombineType.Difference then begin
        res := TGIS_ShapeArc.Create(nil, nil, False, 0, nil, _shpA.Dimension) ;
        res.Lock(TGIS_Lock.Extent) ;
        add_to_result(_shpA) ;
        res.Unlock ;
      end
      else
        res := nil ;
      Result := res ;
      exit ;
    end ;

    pshape1 := _shpA ;
    pshape2 := _shpB  ;
    computeTolerance ;

    contour1 := T_Contour.Create( CONTOUR_OPENED, pshape1, self) ;
    if T_Contour( contour1 ).Count = 0 then begin
      FreeObject( contour1 ) ;
      Result := nil ;
      exit ;
    end ;

    contour2 := T_Contour.Create( CONTOUR_CLOSED, _shpB, self) ;
    if T_Contour( contour2 ).Count = 0 then begin
      FreeObject( contour1 ) ;
      FreeObject( contour2 ) ;
      if _operation = TGIS_TopologyCombineType.Difference then
        Result := TGIS_ShapeArc(_shpA.CreateCopy)
      else
        Result := nil ;
      exit ;
    end ;

    Result := TGIS_ShapeArc.Create( nil, nil, False, 0, _shpA.Layer,
                                    _shpA.Dimension ) ;

    if GisLockGreaterThanEqual( _shpA.LockLevel, TGIS_Lock.Projection ) then
      Result.Lock(_shpA.LockLevel)
    else
      Result.Lock(TGIS_Lock.Extent) ;

    combineType := _operation ;

    // Computing contour1 and contour2 cross-vertex with adding them to the
    // contours. Simultaneously edges and cross-vertex descriptors are creating.
    fixCrossVertices ;

    // edges site defining
    createEdgesAndDfnSite( contour1, contour2 ) ;

    // work polygon
    part_size := 0 ;

    for i := 0 to listOfEdges.Count -1 do begin
      edge := T_Edge(listOfEdges[i]) ;

      case _operation of
        TGIS_TopologyCombineType.Intersection :
          collect := edge.Site <> T_EdgeSite.gesOutside ;
        TGIS_TopologyCombineType.Difference :
          collect := edge.Site = T_EdgeSite.gesOutside ;
        else
          collect := False ;
      end ;

      if collect then begin
        if part_size = 0 then begin
          Result.AddPart ;
          Result.AddPoint3D(edge.PrevVertex.NativePoint3) ;
          part_size := 1 ;
        end ;
        Result.AddPoint3D(edge.NextVertex.NativePoint3) ;
        if edge.NextVertex.NextVertex = edge.NextVertex then
          part_size := 0 ;
      end
      else
        part_size := 0 ;
    end ;

    //on end objects destroying
    FreeObject( contour1 ) ;
    FreeObject( contour2 ) ;

    for k := listOfEdges.Count -1 downto 0 do begin
      edge := T_Edge(listOfEdges[k]) ;
      {$IFNDEF NEXTGEN}
        FreeObjectNotNil(T_Edge(listOfEdges[k])) ;
      {$ENDIF}
    end ;

    FreeObject(listOfEdges) ;

    if Result.IsEmpty then begin
      FreeObject(Result) ;
    end
    else
      Result.Unlock ;
  end ;

  function TGIS_Topology.combineArcs(
    const _shpA      : TGIS_ShapeArc ;
    const _shpB      : TGIS_ShapeArc ;
    const _operation : TGIS_TopologyCombineType
  ) : TGIS_Shape ;
  var
    rcontour  : T_Contour ;
    lcontour1 : T_Contour ;
    lcontour2 : T_Contour ;
    res       : TGIS_Shape ;

    procedure define_edges_seite (_c : T_Contour) ;
    var
      svertex   : T_Vertex ;
      lk, li    : Integer  ;
      k_list    : TList<T_Vertex>    ;
      vertex    : T_Vertex ;
    begin
      for lk := 0 to _c.PartsCount -1 do begin

        _c.Part := lk ;
        k_list := _c.PartVertices.VerticesList ;

        vertex := T_Vertex(k_list[0]) ;

        for li := 0 to k_list.Count -1 do begin

          if li > 0 then begin
            if vertex.NextVertex = vertex then
              break;
            vertex := vertex.NextVertex ;
          end ;

          svertex := vertex.CommonVertex ;

          if svertex <> nil then begin
            if vertex.PrevVertex.CommonVertex = svertex.PrevVertex then
            begin
              if ( vertex.PrevVertex  <> vertex  ) and
                 ( svertex.PrevVertex <> svertex ) then
              begin
                vertex.PrevEdgeSite := T_EdgeSite.gesSharedSD ;
                vertex.PrevVertex.NextEdgeSite := T_EdgeSite.gesSharedSD ;
                svertex.PrevEdgeSite := T_EdgeSite.gesSharedSD ;
                svertex.PrevVertex.NextEdgeSite := T_EdgeSite.gesSharedSD ;
              end ;
            end
            else
            if vertex.PrevVertex.CommonVertex = svertex.NextVertex then
            begin
              if ( vertex.PrevVertex  <> vertex  ) and
                 ( svertex.NextVertex <> svertex ) then
              begin
                vertex.PrevEdgeSite := T_EdgeSite.gesSharedCD ;
                vertex.PrevVertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
                svertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
                svertex.NextVertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
              end ;
            end ;
          end ;
        end ;
      end ;
    end ;

    procedure add_to_result( const _shp : TGIS_Shape ) ;
    var
      p, ps, m : Integer ;
    begin
      for p := 0 to _shp.GetNumParts -1 do begin
        ps := _shp.GetPartSize(p);
        res.AddPart ;
        for m := 0 to ps -1 do
          res.AddPoint(_shp.GetPoint(p, m));
      end ;
    end ;

  begin

    res := nil ;

    if _shpB.GetPartSize(0) < 2 then begin
      if _operation <> TGIS_TopologyCombineType.Intersection then begin
        res := TGIS_ShapeArc.Create(nil, nil, False, 0, nil, _shpA.Dimension) ;
        res.Lock(TGIS_Lock.Extent) ;
        add_to_result(_shpA) ;
        res.Unlock ;
      end ;
      Result := res ;
      exit ;
    end ;

    pshape1 := _shpA ;
    pshape2 := _shpB  ;
    computeTolerance ;

    lcontour1 := T_Contour.Create( CONTOUR_OPENED, _shpA, self) ;
    if T_Contour( lcontour1 ).Count = 0 then begin
      FreeObject( lcontour1 ) ;
      if (_operation = TGIS_TopologyCombineType.SymmetricalDifference) or
         (_operation = TGIS_TopologyCombineType.Union                ) then begin
        res := TGIS_ShapeArc.Create(nil, nil, False, 0, nil, _shpB.Dimension) ;
        res.Lock(TGIS_Lock.Extent) ;
        add_to_result(_shpB) ;
        res.Unlock ;
      end ;
      Result := res ;
      exit ;
    end ;
    lcontour2 := T_Contour.Create( CONTOUR_OPENED ,_shpB, self) ;
    if T_Contour( lcontour2 ).Count = 0 then begin
      FreeObject( lcontour1 ) ;
      FreeObject( lcontour2 ) ;
      if _operation <> TGIS_TopologyCombineType.Intersection then begin
        res := TGIS_ShapeArc.Create(nil, nil, False, 0, nil, _shpA.Dimension) ;
        res.Lock(TGIS_Lock.Extent) ;
        add_to_result(_shpA) ;
        res.Unlock ;
      end ;
      Result := res ;
      exit ;
    end ;

    rcontour := T_Contour(combineCArcs( lcontour1, lcontour2, _operation )) ;

    if rcontour <> nil then begin

      Result :=  TGIS_ShapeArc.Create( nil, nil, False, 0, _shpA.Layer,
                                       _shpA.Dimension ) ;

      if GisLockGreaterThanEqual( _shpA.LockLevel, TGIS_Lock.Projection ) then
        Result.Lock(_shpA.LockLevel)
      else
        Result.Lock(TGIS_Lock.Extent) ;
      rcontour.Write2Shape(Result);
      FreeObject( rcontour ) ;
      Result.Unlock ;
    end
    else
      Result := nil ;
    // on end objects destroying
    FreeObject( contour1 ) ;
    FreeObject( contour2 ) ;
  end ;

  function TGIS_Topology.combineCArcs(
    const _contourA   : TObject ;
    const _contourB   : TObject ;
    const _operation  : TGIS_TopologyCombineType
  ) : TObject ;
  var
    i, k        : Integer  ;
    vertex      : T_Vertex ;
    lvertex     : T_Vertex ;
    cvertex     : T_Vertex ;
    k_list      : TList<T_Vertex>    ;

    contour     : T_Contour;
    cpart       : T_ContourPart ;
    direction   : T_EdgeDirection ;
    edges_list  : TList<T_Edge> ;
    edge        : T_Edge ;
    owner       : T_Contour ;
    numcross    : Integer ;

    procedure define_edges_seite (_c : T_Contour) ;
    var
      ccvertex : T_Vertex ;
      lk, li   : Integer ;
    begin
      for lk := 0 to _c.PartsCount -1 do begin

        _c.Part := lk ;
        k_list := _c.PartVertices.VerticesList ;

        for li := 0 to k_list.Count -2 do begin

          vertex := T_Vertex(k_list[li]) ;

          if assigned( vertex.CommonVertex ) then begin
            ccvertex := vertex.find_sharedSD_vertex ;
            if assigned( ccvertex ) then begin
              vertex.NextEdgeSite := T_EdgeSite.gesSharedSD ;
              ccvertex.NextEdgeSite := T_EdgeSite.gesSharedSD ;
              vertex.NextVertex.PrevEdgeSite := T_EdgeSite.gesSharedSD ;
              ccvertex.NextVertex.PrevEdgeSite := T_EdgeSite.gesSharedSD ;
            end
            else begin
              ccvertex := vertex.find_sharedCD_vertex ;
              if assigned( ccvertex ) then begin
                vertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
                ccvertex.PrevEdgeSite := T_EdgeSite.gesSharedCD ;
                vertex.NextVertex.PrevEdgeSite := T_EdgeSite.gesSharedCD ;
                ccvertex.PrevVertex.NextEdgeSite := T_EdgeSite.gesSharedCD ;
              end ;
            end ;
          end ;
        end ;
      end ;
    end ;

    procedure make_contour_edges( const _c : T_Contour ) ;
    var
      i1, k1    : Integer ;
      v       : T_Vertex   ;
      nv      : T_Vertex   ;
      sc_v    : T_Vertex   ;
      ss_v    : T_Vertex   ;
      e, ce   : T_Edge     ;
      idx_max : Integer    ;
    begin

      for k1 := 0 to T_Contour( _c ).PartsCount -1 do begin
        T_Contour( _c ).Part := k1 ;

        v := T_Vertex( T_Contour( _c ).PartVertices.VerticesList[0] ) ;
        nv := v.NextVertex ;

        idx_max := T_Contour( _c ).PartVertices.VerticesList.Count -1 ;

        for i1 := 0 to idx_max do begin
          if i1 > 0 then begin
            v := nv ;
            nv := v.NextVertex ;
            if nv = v then break ;
          end ;

          e := T_Edge.Create ;
          e.PrevVertex := v ;
          e.NextVertex := nv ;
          v.NextEdge := e ;
          nv.PrevEdge := e ;

          if v.NextEdgeSite <> T_EdgeSite.gesUndefined then begin

            e.Site := v.NextEdgeSite ;
            ce := nil ;
            if v.NextEdgeSite = T_EdgeSite.gesSharedSD then begin
              ss_v := v.find_sharedSD_vertex ;
              if assigned( ss_v ) then
                ce := ss_v.NextEdge
              else
                ce := nil ;
            end
            else
            if v.NextEdgeSite = T_EdgeSite.gesSharedCD then begin
              sc_v := v.find_sharedCD_vertex ;
              if assigned( sc_v ) then
                ce := sc_v.PrevEdge
              else
                ce := nil ;
            end ;
            if ce <> nil then begin
              ce.CommonEdge := e ;
              e.CommonEdge := ce ;
            end ;
          end ;

          edges_list.Add(e) ;
        end ;
      end ;
    end ;

    function get_last_forward( const _v : T_Vertex;
                               const _o : T_Contour ) : T_Vertex ;
    var
      lv : T_Vertex ;
    begin
      lv := _v ;
      while lv <> lv.NextVertex do begin
        if lv.NextEdge.Marked then
          break ;
        lv := lv.NextVertex ;
        if lv.IsCrossVertex then begin
          if lv.CommonVertex.Owner = _o then
            break ;
        end ;
      end ;
      Result := lv ;
    end ;

    function get_last_backward( const _v : T_Vertex;
                                const _o : T_Contour  ) : T_Vertex ;
    var
      lv : T_Vertex ;
    begin
      lv := _v ;
      while lv <> lv.PrevVertex do begin
        if lv.PrevEdge.Marked then
          break ;
        lv := lv.PrevVertex ;
        if lv.IsCrossVertex then begin
          if lv.CommonVertex.Owner = _o then
            break ;
        end ;
      end ;
      Result := lv ;
    end ;

    procedure collect_arc_cpart( const _v  : T_Vertex ;
                                 const _d  : T_EdgeDirection ;
                                 const _cp : T_ContourPart ) ;
    var
      wv, sv      : T_Vertex ;
      d           : T_EdgeDirection ;
      in_owner    : T_Contour ;
      is_fork     : Boolean ;
      function test_fork( const _fv  : T_Vertex ;
                          var   _fd  : T_EdgeDirection
                         ) : Boolean ;
      begin

        if _fv.NextEdge <> nil then begin
          if not _fv.NextEdge.Marked then begin
            _fd := T_EdgeDirection.gedForward ;
            Result := True ;
            exit ;
          end ;
        end ;
        if _fv.PrevEdge <> nil then begin
          if not _fv.PrevEdge.Marked then begin
            _fd := T_EdgeDirection.gedBackward ;
            Result := True ;
            exit ;
          end ;
        end ;
        Result := False ;
      end ;
    begin
      wv := _v ;
      d := _d ;
      in_owner := wv.Owner ;
      is_fork := False ;
      while True do begin
        if d = T_EdgeDirection.gedBackward then begin
          while True do begin
            sv := T_Vertex.Create ;
            sv.NativePoint2 := wv.NativePoint2 ;
            sv.NativePoint3 := wv.NativePoint3 ;
            _cp.VerticesList.Add(sv) ;
            if wv.PrevVertex = wv then
              break ;

            if ( wv.Owner <> in_owner ) or
               ( wv.Owner =  contour2 ) or
               ( wv.PrevEdge.Marked ) then
            begin
              if wv.IsCrossVertex then begin
                is_fork := test_fork(wv.CommonVertex, d) ;
                if is_fork then
                  break
                else
                  exit ;
              end ;
            end ;

            wv.PrevEdge.Marked := True ;
            if wv.PrevEdge.CommonEdge <> nil then
              wv.PrevEdge.CommonEdge.Marked := True ;
            wv := wv.PrevVertex ;
          end ;
        end
        else
        if d = T_EdgeDirection.gedForward then begin
          while True do begin
            sv := T_Vertex.Create ;
            sv.NativePoint2 := _TGIS_Point (wv.NativePoint2) ;
            sv.NativePoint3 := _TGIS_Point3D (wv.NativePoint3) ;
            _cp.VerticesList.Add(sv) ;
            if wv.NextVertex = wv then
              break ;
            if( wv.Owner <> in_owner ) or
              ( wv.Owner =  contour2 ) or
              ( wv.NextEdge.Marked   )  then
            begin
              if wv.IsCrossVertex then begin
                is_fork := test_fork(wv.CommonVertex, d) ;
                if is_fork then
                  break
                else begin
                  if _cp.VerticesList.Count > 1 then
                    exit ;
                end ;
              end ;
            end ;
            wv.NextEdge.Marked := True ;
            if wv.NextEdge.CommonEdge <> nil then
              wv.NextEdge.CommonEdge.Marked := True ;
            wv := wv.NextVertex ;
          end ;
        end ;

        if  not wv.IsCrossVertex then
          exit ;
        wv := wv.CommonVertex ;

        if is_fork then begin
          if d = T_EdgeDirection.gedForward then begin
            wv.NextEdge.Marked := True ;
            if wv.NextEdge.CommonEdge <> nil then
              wv.NextEdge.CommonEdge.Marked := True ;
            wv := wv.NextVertex ; // skip common point
          end
          else begin
            wv.PrevEdge.Marked := True ;
            if wv.PrevEdge.CommonEdge <> nil then
              wv.PrevEdge.CommonEdge.Marked := True ;
            wv := wv.PrevVertex ; // skip common point
          end ;
          is_fork := False ;
          continue ;
        end ;

        if wv = wv.PrevVertex then begin
          d := T_EdgeDirection.gedForward ;
          if wv.NextEdge.Marked then
            exit ;
          wv.NextEdge.Marked := True ;
          if wv.NextEdge.CommonEdge <> nil then
            wv.NextEdge.CommonEdge.Marked := True ;
          wv := wv.NextVertex ; // skip common point
        end
        else
        if wv = wv.NextVertex then begin
          d := T_EdgeDirection.gedBackward ;
          if wv.PrevEdge.Marked then
            exit ;
          wv.PrevEdge.Marked := True ;
          if wv.PrevEdge.CommonEdge <> nil then
            wv.PrevEdge.CommonEdge.Marked := True ;
          wv := wv.PrevVertex ; // skip common point
        end
        else begin
          if not test_fork(wv, d) then
            exit ;
          if d = T_EdgeDirection.gedForward then begin
            wv.NextEdge.Marked := True ;
            if wv.NextEdge.CommonEdge <> nil then
              wv.NextEdge.CommonEdge.Marked := True ;
            wv := wv.NextVertex ; // skip common point
          end
          else begin
            wv.PrevEdge.Marked := True ;
            if wv.PrevEdge.CommonEdge <> nil then
              wv.PrevEdge.CommonEdge.Marked := True ;
            wv := wv.PrevVertex ; // skip common point
          end ;
          is_fork := False ;
        end ;
      end ;
    end ;

  begin

    Result := nil ;

    contour1 := _contourA ;
    contour2 := _contourB ;
    combineType := _operation ;

    // Computing contour1 and contour2 cross-vertex with adding them to the
    // contours. Simultaneously edges and cross-vertex descriptors are creating.
    fixCrossVertices ;

    edges_list := TList<T_Edge>.Create ;
    {$IFNDEF JAVA OR ISLAND}
    edges_list.Capacity := T_Contour( contour1 ).Count +
                           T_Contour( contour2 ).Count ;
    {$ENDIF}
    define_edges_seite(T_Contour( contour1 )) ;

    make_contour_edges(T_Contour(contour1)) ;
    make_contour_edges(T_Contour(contour2)) ;

    case _operation of
      TGIS_TopologyCombineType.Intersection :
        for i := 0 to edges_list.Count -1 do begin
          edge := T_Edge(edges_list[i]) ;
          if edge.PrevVertex.Owner = contour2 then
            edge.Marked := True
          else
          if edge.CommonEdge = nil then
            edge.Marked := True ;
        end ;
      TGIS_TopologyCombineType.Difference :
        for i := 0 to edges_list.Count -1 do begin
          edge := T_Edge(edges_list[i]) ;
          if edge.PrevVertex.Owner = contour2 then
            edge.Marked := True
          else
          if edge.CommonEdge <> nil then
            edge.Marked := True ;
        end ;
      TGIS_TopologyCombineType.SymmetricalDifference :
        for i := 0 to edges_list.Count -1 do begin
          edge := T_Edge(edges_list[i]) ;
          if edge.CommonEdge <> nil then
            edge.Marked := True ;
        end ;
    end ;

    contour := T_Contour.Create ;
    cpart := nil ;

    for i := 0 to edges_list.Count -1 do begin

      edge := T_Edge(edges_list[i]) ;
      if edge.Marked then
        continue ;

      direction := T_EdgeDirection.gedForward ;

      vertex := edge.PrevVertex ;
      owner := vertex.Owner ;

      if  cpart = nil then
        cpart := T_ContourPart.Create ;

      lvertex := vertex ;
      numcross := 0;
      while vertex.IsCrossVertex do begin
        cvertex := vertex.CommonVertex ;
        inc( numcross ) ;
        if numcross > 2 then begin
          break ;
        end ;

        if ( cvertex.NextEdge <> nil ) and
           ( cvertex.PrevEdge =  nil ) then begin
          if cvertex.NextEdge.CommonEdge = nil then begin
            if not cvertex.NextEdge.Marked then begin
              vertex := get_last_forward(cvertex, owner) ;
              if vertex = cvertex.CommonVertex then
                break ;
              if vertex = lvertex then begin
                break ;
              end ;
              direction := T_EdgeDirection.gedBackward ;
              continue ;
            end ;
          end ;
        end ;

        if ( cvertex.PrevEdge <> nil ) and
           ( cvertex.NextEdge =  nil ) then begin
          if cvertex.PrevEdge.CommonEdge = nil then begin
            if cvertex.PrevEdge.Marked then
              break ;
            vertex := get_last_backward(cvertex, owner) ;
            if vertex = cvertex.CommonVertex  then
              break ;
            if vertex = lvertex then
              break ;
            direction := T_EdgeDirection.gedForward ;
            continue ;
          end ;
        end ;
        break ;
      end ;

      collect_arc_cpart(vertex, direction, cpart) ;

      if cpart.VerticesList.Count > 1 then begin
        contour.AddPart(cpart) ;
        cpart := nil ;
      end

    end ;

    if cpart <> nil then
      FreeObject( cpart ) ;

    {$IFNDEF NEXTGEN}
      for k := edges_list.Count -1 downto 0 do
        FreeObjectNotNil( T_Edge(edges_list[k]) ) ;
    {$ENDIF}

    FreeObject( edges_list ) ;

    if contour.FPartsCount > 0 then begin

      Result :=  contour ;
    end
    else
      FreeObject( contour ) ;

    // on end objects destroying
    FreeObject( contour1 ) ;
    FreeObject( contour2 ) ;
  end ;

  function TGIS_Topology.combinePointPolygon(
    const _shpA      : TGIS_Shape        ;
    const _shpB      : TGIS_ShapePolygon ;
    const _operation : TGIS_TopologyCombineType
  ) : TGIS_Shape ;
  var
    i         : Integer  ;
    ii        : Integer  ;
    vertex    : T_Vertex ;
    part_size : Integer  ;
    add_part  : Boolean  ;
    inside    : Boolean  ;
    res       : TGIS_Shape ;

    procedure add_point2result ;
    begin
      if add_part then
        res.AddPart ;
      res.AddPoint3D(vertex.WorkPoint3) ;
      add_part := False ;
    end ;

  begin

    Result := nil ;

    if (_operation <> TGIS_TopologyCombineType.Intersection) and
       (_operation <> TGIS_TopologyCombineType.Difference  )
    then
      exit ;

    if _shpA.ShapeType = TGIS_ShapeType.MultiPoint then
      res :=  TGIS_ShapeMultiPoint.Create( nil, nil, False, 0, _shpA.Layer, _shpA.Dimension )
    else
      res :=  TGIS_ShapePoint.Create( nil, nil, False, 0, _shpA.Layer, _shpA.Dimension ) ;

    res.Lock( TGIS_Lock.Extent ) ;

    contour1 := T_Contour.Create( CONTOUR_CLOSED, _shpB, self) ;
    vertex := T_Vertex.Create ;

    for ii := 0 to _shpA.GetNumParts -1 do begin

      part_size := _shpA.GetPartSize(ii) ;
      add_part := True ;

      for i := 0 to part_size -1 do begin
        vertex.NativePoint3 := _shpA.GetPoint3D(ii, i) ;
        vertex.WorkPoint3   := _TGIS_Point3D(vertex.NativePoint3) ;
        vertex.NativePoint2 := GisPoint2DFrom3D( vertex.NativePoint3 ) ;
        vertex.WorkPoint2   := _TGIS_Point(vertex.NativePoint2) ;
        inside := T_Contour( contour1 ).IsPointInsideContour(
                                          vertex.NativePoint2
                                        ) ;
        if inside then begin
          if (_operation = TGIS_TopologyCombineType.Intersection) then
            add_point2result ;
        end
        else begin
          if (_operation = TGIS_TopologyCombineType.Intersection) then begin
            if T_Contour( contour1 ).SnapToContour(vertex, FTolerance) then
              add_point2result ;
          end
          else
            if not T_Contour( contour1 ).SnapToContour(vertex, FTolerance) then
              add_point2result ;
        end ;
      end ; //end of for i loop
    end ;  //end of for ii loop

    FreeObject(vertex) ;
    FreeObject( contour1 ) ;

    Result := res ;

    if Result.IsEmpty then begin
      FreeObject(Result) ;
    end
    else
      Result.Unlock ;

  end ;

  function TGIS_Topology.combinePointArc(
    const _shpA      : TGIS_ShapePoint ;
    const _shpB      : TGIS_ShapeArc   ;
    const _operation : TGIS_TopologyCombineType
  ) : TGIS_Shape ;
  var
    i         : Integer  ;
    ii        : Integer  ;
    vertex    : T_Vertex ;
    part_size : Integer  ;
    add_part  : Boolean  ;
    res       : TGIS_Shape ;

    procedure add_point2result ;
    begin
      if add_part then
        res.AddPart ;
      res.AddPoint3D( vertex.WorkPoint3 ) ;
      add_part := False ;
    end ;

  begin

    Result := nil ;

    if (_operation <> TGIS_TopologyCombineType.Intersection) and
       (_operation <> TGIS_TopologyCombineType.Difference  )
    then
      exit ;

    if _shpA.ShapeType = TGIS_ShapeType.MultiPoint then
      res :=  TGIS_ShapeMultiPoint.Create( nil, nil, False, 0, _shpA.Layer, _shpA.Dimension )
    else
      res :=  TGIS_ShapePoint.Create( nil, nil, False, 0, _shpA.Layer, _shpA.Dimension ) ;

    res.Lock( TGIS_Lock.Extent ) ;

    contour1 := T_Contour.Create( CONTOUR_OPENED, _shpB, self) ;
    vertex := T_Vertex.Create ;

    for ii := 0 to _shpA.GetNumParts -1 do begin

      part_size := _shpA.GetPartSize(ii) ;
      add_part := True ;

      for i := 0 to part_size -1 do begin
        vertex.NativePoint3 := _shpA.GetPoint3D(ii, i) ;
        vertex.WorkPoint3 := _TGIS_Point3D(vertex.NativePoint3) ;
        vertex.NativePoint2 := _shpA.GetPoint(ii, i) ;
        vertex.WorkPoint2 := _TGIS_Point (vertex.NativePoint2) ;

        if (_operation = TGIS_TopologyCombineType.Intersection) then begin
          if T_Contour( contour1 ).SnapToContour(vertex, FTolerance) then
            add_point2result
        end
        else
          if not T_Contour( contour1 ).SnapToContour(vertex, FTolerance) then
             add_point2result ;
      end ; //end of for i loop
    end ;  //end of for ii loop

    FreeObject( vertex   ) ;
    FreeObject( contour1 ) ;

    Result := res ;

    if Result.IsEmpty then begin
      FreeObject( Result ) ;
    end
    else
      Result.Unlock ;

  end ;

  function  TGIS_Topology.combineMultiPoints(
    const _shpA      : TGIS_ShapeMultiPoint ;
    const _shpB      : TGIS_ShapeMultiPoint ;
    const _operation : TGIS_TopologyCombineType
  ) : TGIS_Shape ;
  var
    shp1, shp2 : TGIS_Shape ;
    i, k       : Integer    ;
    ii, kk     : Integer    ;
    add_part   : Boolean    ;
    times      : Integer    ;
    runno      : Integer    ;
    dist,
    mindist    : Double     ;
    ptg, tptg  : TGIS_Point ;
    mptg       : TGIS_Point ;
    bdobreak   : Boolean    ;
    res        : TGIS_Shape ;

    procedure add_point2result ;
    begin
      if add_part then
        res.AddPart ;
      res.AddPoint(ptg) ;
      add_part := False ;
    end ;

  begin

    res :=  TGIS_ShapeMultiPoint.Create( nil, nil, False, 0, _shpA.Layer ) ;
    res.Lock( TGIS_Lock.Extent ) ;

    if (_operation = TGIS_TopologyCombineType.SymmetricalDifference) or
       (_operation = TGIS_TopologyCombineType.Union)
    then
      times := 2
    else
      times := 1 ;

    shp1 := _shpA;
    shp2 := _shpB;

    for runno := 1 to times do begin
      for ii := 0 to shp1.GetNumParts -1 do begin
        add_part := True ;
        for i := 0 to shp1.GetPartSize(ii) -1 do begin
          ptg := shp1.GetPoint(ii, i) ;
          mindist := -1;

          for kk := 0 to shp2.GetNumParts -1 do begin
            bdobreak := False ;
            for k := 0 to shp2.GetPartSize(kk) -1 do begin
              tptg := shp2.GetPoint(kk, k) ;
              dist := GisPoint2Point(ptg, tptg) ;
              if dist <= FTolerance then begin
                if (mindist < 0) or (dist < mindist) then begin
                  mindist := dist ;
                  mptg := tptg ;
                  if dist = 0 then begin
                    bdobreak := True ;
                    break ;
                  end ;
                end ;
              end ;
            end ;
            if bdobreak then
              break ;
          end ;
          case _operation of
            TGIS_TopologyCombineType.Union :
               if runno = 0 then
                 add_point2result
               else
               if mindist < 0 then
                 add_point2result
                else
                if runno = 1 then
                 add_point2result ;
            TGIS_TopologyCombineType.Intersection :
               if mindist >= 0 then begin
                 ptg := _TGIS_Point (mptg) ;
                 add_point2result ;
               end ;
            TGIS_TopologyCombineType.Difference :
               if mindist < 0 then
                 add_point2result ;
            TGIS_TopologyCombineType.SymmetricalDifference :
               if mindist < 0 then
                 add_point2result ;
          end ;
        end ;
      end ;
      shp1 := _shpB;
      shp2 := _shpA;
    end ;

    Result := res ;

    if Result.IsEmpty then begin
      FreeObject(Result) ;
    end
    else
      Result.Unlock ;

  end ;

  function TGIS_Topology.combinePointMPoint(
     const _shpA : TGIS_ShapePoint ;
     const _shpB : TGIS_ShapeMultiPoint ;
     const _operation : TGIS_TopologyCombineType
  ) : TGIS_Shape ;
  var
    dist    : Double     ;
    ptg,
    tptg    : TGIS_Point ;
    k, i    : Integer ;
    addpt   : Boolean ;
    isbreak : Boolean ;
  begin

    Result :=  TGIS_ShapeMultiPoint.Create( nil, nil, False, 0, _shpA.Layer ) ;

    Result.Lock( TGIS_Lock.Extent ) ;

    ptg  := _shpA.GetPoint(0, 0) ;

    case _operation of
      TGIS_TopologyCombineType.Union :
        begin
          addpt := True ;
          Result.AddPart ;
          for k := 0 to _shpB.GetNumParts -1 do begin
            for i := 0 to _shpB.GetPartSize(k) -1 do begin
              tptg := _shpB.GetPoint(k, i) ;
              Result.AddPoint(tptg) ;
              dist := GisPoint2Point(ptg, tptg) ;
              if dist <= FTolerance then begin
                addpt := False ;
              end ;
            end ;
          end ;
          if addpt then begin
            Result.AddPoint(ptg) ;
          end ;
        end ;
      TGIS_TopologyCombineType.Intersection :
        begin
          isbreak := False ;
          Result.AddPart ;
          for k := 0 to _shpB.GetNumParts -1 do begin
            for i := 0 to _shpB.GetPartSize(k) -1 do begin
              tptg := _shpB.GetPoint(k, i) ;
              dist := GisPoint2Point(ptg, tptg) ;
              if dist <= FTolerance then begin
                Result.AddPoint(tptg) ;
                isbreak := True ;
                break ;
              end ;
            end ;
            if isbreak then
              break ;
          end ;
        end ;
      TGIS_TopologyCombineType.Difference :
        begin
          isbreak := False ;
          for k := 0 to _shpB.GetNumParts -1 do begin
            for i := 0 to _shpB.GetPartSize(k) -1 do begin
              tptg := _shpB.GetPoint(k, i) ;
              dist := GisPoint2Point(ptg, tptg) ;
              if dist <= FTolerance then begin
                isbreak := True ;
                break ;
              end ;
            end ;
            if isbreak then
              break ;
          end ;
          if not isbreak then begin
            Result.AddPart ;
            Result.AddPoint(ptg) ;
          end ;
        end ;
      else //TGIS_TopologyCombineType.SymmeticalDifference
      begin
        addpt := True ;
        Result.AddPart ;

        for k := 0 to _shpB.GetNumParts -1 do begin
          for i := 0 to _shpB.GetPartSize(k) -1 do begin
            tptg := _shpB.GetPoint(k, i) ;
            dist := GisPoint2Point(ptg, tptg) ;
            if dist > FTolerance then begin
              Result.AddPoint(tptg) ;
            end
            else
              addpt := False ;
          end ;
        end ;
        if addpt then begin
          Result.AddPoint(ptg) ;
        end ;
      end ;
    end ;

    if Result.IsEmpty then begin
      FreeObject(Result) ;
    end
    else begin
      if Result.ShapeType = TGIS_ShapeType.MultiPoint then begin
        if Result.GetNumPoints = 1 then begin
          FreeObject(Result) ;
          Result :=  TGIS_ShapePoint.Create( nil, nil, False, 0, _shpA.Layer ) ;
          Result.Lock( TGIS_Lock.Extent ) ;
          Result.AddPart ;
          Result.AddPoint(ptg) ;
        end ;
      end ;
      Result.Unlock ;
    end ;

  end ;

  function TGIS_Topology.differenceMPointPoint (
    const _shpA       : TGIS_ShapeMultiPoint ;
    const _shpB       : TGIS_ShapePoint
  ) : TGIS_Shape ;
  var
    ptg,
    tptg    : TGIS_Point ;
    k, i    : Integer ;
    addpart : Boolean ;
  begin

    Result :=  TGIS_ShapeMultiPoint.Create( nil, nil, False, 0, _shpA.Layer ) ;

    Result.Lock( TGIS_Lock.Extent ) ;

    ptg  := _shpB.GetPoint(0, 0) ;

    for k := 0 to _shpA.GetNumParts -1 do begin
      addpart := True ;
      for i := 0 to _shpA.GetPartSize(k) -1 do begin
        tptg := _shpA.GetPoint(k, i) ;
        if GisPoint2Point(ptg, tptg) <= FTolerance then
          continue ;
        if addpart then begin
          Result.AddPart ;
          addpart := False ;
        end ;
        Result.AddPoint(tptg) ;
      end ;
    end ;

    if Result.IsEmpty then begin
      FreeObject(Result) ;
    end
    else begin
      if Result.ShapeType = TGIS_ShapeType.MultiPoint then begin
        if Result.GetNumPoints = 1 then begin
          ptg := Result.GetPoint(0, 0) ;
          FreeObject(Result) ;
          Result :=  TGIS_ShapePoint.Create( nil, nil, False, 0, _shpA.Layer ) ;
          Result.Lock( TGIS_Lock.Extent ) ;
          Result.AddPart ;
          Result.AddPoint(ptg) ;
        end ;
      end ;
      Result.Unlock ;
    end ;

  end ;

  function TGIS_Topology.combinePoints(
     const _shpA : TGIS_ShapePoint ;
     const _shpB : TGIS_ShapePoint ;
     const _operation : TGIS_TopologyCombineType
   ) : TGIS_Shape ;
  var
    ptgA,
    ptgB    : TGIS_Point ;
    same    : Boolean ;
  begin

    ptgA  := _shpA.GetPoint(0, 0) ;
    ptgB  := _shpB.GetPoint(0, 0) ;
    if GisPoint2Point(ptgA, ptgB) <= FTolerance then
      same := True
    else
      same := False ;

    Result := nil ;

    case _operation of
      TGIS_TopologyCombineType.Union :
        begin
          if same then begin
            Result := TGIS_ShapePoint.Create( nil, nil, False, 0, _shpA.Layer ) ;
            Result.AddPart ;
            Result.AddPoint(ptgA);
          end
          else begin
            Result := TGIS_ShapeMultiPoint.Create( nil, nil, False, 0, _shpA.Layer ) ;
            Result.AddPart ;
            Result.AddPoint(ptgA);
            Result.AddPoint(ptgB);
          end ;
        end ;
      TGIS_TopologyCombineType.Intersection :
        begin
          if not same then
            exit ;
          Result :=  TGIS_ShapePoint.Create( nil, nil, False, 0, _shpA.Layer ) ;
          Result.AddPart ;
          Result.AddPoint(ptgA);
        end ;
      TGIS_TopologyCombineType.Difference :
        begin
          if same then
            exit ;
          Result :=  TGIS_ShapePoint.Create( nil, nil, False, 0, _shpA.Layer ) ;
          Result.AddPart ;
          Result.AddPoint(ptgA);
        end ;
      else //TGIS_TopologyCombineType.SymmeticalDifference
      begin
        if same then
          exit ;
        Result :=  TGIS_ShapeMultiPoint.Create( nil, nil, False, 0, _shpA.Layer ) ;
        Result.AddPart ;
        Result.AddPoint(ptgA);
        Result.AddPoint(ptgB);
      end ;
    end ;

  end ;

  function TGIS_Topology.ArcFind(
    const _arc      : TGIS_ShapeArc     ;
    const _shplist  : {$IFDEF CLR}
                        IList           ;
                      {$ELSE}
                        TGIS_ObjectList ;
                      {$ENDIF}
    var   _idx      : Integer
  ) : Boolean ;
  var
    localShpList : TList<TGIS_ShapeArc> ;
    sptg         : TGIS_Point    ;
    ptg          : TGIS_Point    ;
    k, kk , c    : Integer       ;
    arcpartsize,
    spartsize    : Integer       ;
    arcpartsno,
    spartsno     : Integer       ;
    sextent      : TGIS_Extent   ;
    rarc,
    arc          : TGIS_ShapeArc ;
  begin
    Result := False ;
    _idx := -1  ;

    if GisLockGreaterThanEqual( _arc.LockLevel, TGIS_Lock.Projection ) then
      sextent := _TGIS_Extent (_arc.Extent)
    else
      sextent := _TGIS_Extent (_arc.ProjectedExtent) ;

    localShpList := TList<TGIS_ShapeArc>.Create;
    for c := 0 to _shplist.Count -1 do begin
      arc := TGIS_ShapeArc(_shplist[c]);
      if arc = nil then continue ;
      if arc.IsDeleted then continue ;
      if (arc.Uid <> _arc.Uid) or (arc.Layer <> _arc.Layer)  then begin
        if arc.IsInsideExtent(sextent, TGIS_InsideType.Partial) then
          localShpList.Add(arc) ;
      end ;
    end ;

    spartsno := _arc.GetNumParts ;

    for c := 0 to localShpList.Count -1 do begin
      arc := TGIS_ShapeArc(localShpList[c]);
      arcpartsno := arc.GetNumParts ;
      Result := True ;
      _idx := c;
      if arcpartsno <> spartsno then begin
        Result := False ;
        continue ;
      end ;
      for k := 0 to arcpartsno -1 do begin

        arcpartsize := arc.GetPartSize(k) ;
        spartsize := _arc.GetPartSize(k) ;
        if arcpartsize <> spartsize then begin
          Result := False ;
          break ;
        end ;
        for kk := 0 to arcpartsize -1 do begin
          ptg:= arc.GetPoint(k, kk);
          sptg:= _arc.GetPoint(k, kk);
          if (ptg.X <> sptg.X) or (ptg.Y <> sptg.Y) then begin
            Result := False ;
            break ;
          end ;
        end ;
        if Result = False  then
          break ;
      end ;
      if Result then
        break ;
    end ;
    if Result then begin
      rarc := TGIS_ShapeArc(localShpList[_idx]);
      for k := 0 to _shplist.Count -1 do begin
        arc := TGIS_ShapeArc(_shplist[k]);
        if (arc.Layer = rarc.Layer) and (rarc.Uid = arc.Uid) then begin
          _idx := k ;
          break ;
        end ;
      end ;
    end ;

    FreeObject( localShpList ) ;
  end ;

  type
    T_connection_point = {$IFNDEF OXYGENE} packed {$ENDIF} record
      cno  : Integer    ;
      dist : Double     ;
      pt   : TGIS_Point ;
    end ;

  function TGIS_Topology.ArcMakeConnected(
    const _arc     : TGIS_ShapeArc     ;
    const _shplist : {$IFDEF CLR}
                      IList
                    {$ELSE}
                      TGIS_ObjectList
                    {$ENDIF}
  ) : TGIS_ShapeArc ;
  var
    cross_ptg        : TGIS_Point  ;
    localShpList     : TList<TGIS_ShapeArc>       ;
    be_to_line_ptg   : array [0..1] of TGIS_Point;
    be_ptg           : array [0..1] of TGIS_Point;
    min_to_line_dist : array [0..1] of Double;
    sline, line      : TGIS_Line   ;
    wline            : TGIS_Line   ;
    i, k, ii, kk , c : Integer     ;
    arcpartsize,
    spartsize        : Integer     ;
    arcpartsno,
    spartsno         : Integer     ;
    arcextent        : TGIS_Extent ;
    sextent          : TGIS_Extent ;
    is_connected     : Boolean     ;
    is_to_line_connected : array [0..1] of Boolean;
    arc, warc        : TGIS_ShapeArc ;
    dist             : Double      ;
    c2, dx, dy       : Double      ;
    ratunku          : Integer     ;
    clist            : array [0..1] of TList<T_connection_point> ;
    cnp              : T_connection_point ;
    scno             : array [0..1] of Integer ;

    function inc_cno_if_on_list( const _list : TList<T_connection_point>;
                                 const _pt   : TGIS_Point
                               ) : Boolean ;
    var
      count : Integer ;
      cpt : T_connection_point ;
    begin
      Result := False ;
      for count := 0 to _list.Count -1 do begin
          cpt := T_connection_point( _list[count] ) ;
          if (cpt.pt.X = _pt.X) and (cpt.pt.Y = _pt.Y) then begin
            inc( cpt.cno ) ;
          Result := True ;
          break ;
        end ;
      end ;
    end ;

    function find_best_on_list(_list : TList<T_connection_point>) : Integer ;
    var
      count   : Integer ;
        cpt   : T_connection_point ;
      mindist : Double  ;
      cno     : Integer ;
    begin
      Result := -1 ;
      mindist := GIS_MAX_DOUBLE ;
      cno := 0;
      for count := 0 to _list.Count -1 do begin
        cpt := T_connection_point( _list[count] ) ;
        if (cpt.cno > 1) and (cpt.dist = 0)then begin
          Result := count ;
          break ;
        end
        else
          if cpt.cno > cno then begin
            Result := count ;
            cno := cpt.cno ;
            mindist := cpt.dist ;
          end
          else
            if cpt.cno = cno then begin
              if cpt.dist < mindist then begin
                Result := count ;
                cno := cpt.cno ;
                mindist := cpt.dist ;
              end ;
            end ;

      end ;
    end ;

  begin

    Result := nil ;

    if GisLockGreaterThanEqual( _arc.LockLevel, TGIS_Lock.Projection ) then
      sextent := _TGIS_Extent (_arc.Extent)
    else
      sextent := _TGIS_Extent (_arc.ProjectedExtent) ;

    warc := TGIS_ShapeArc(_arc.CreateCopy) ;
    warc.Reset;

    is_connected := False ;

    spartsno := _arc.GetNumParts ;

    localShpList := TList<TGIS_ShapeArc>.Create;
    clist[0] := TList<T_connection_point>.Create ;
    clist[1] := TList<T_connection_point>.Create ;

    for c := 0 to _shplist.Count -1 do begin
      arc := TGIS_ShapeArc(_shplist[c]);
      if arc = nil then continue ;
      if arc.IsDeleted then continue ;

      if (arc.Uid <> _arc.Uid) or (arc.Layer <> _arc.Layer) then begin
        if GisLockGreaterThanEqual( arc.LockLevel,  TGIS_Lock.Projection ) then
          arcextent := arc.Extent
        else
          arcextent := arc.ProjectedExtent ;
        if ( arcextent.XMax < sextent.XMin) or
           ( arcextent.XMin > sextent.XMax) or
           ( arcextent.YMax < sextent.YMin) or
           ( arcextent.YMin > sextent.YMax)
        then
          continue
        else
          localShpList.Add(arc);
        end ;
    end ;

    for i := 0 to spartsno -1 do begin

      spartsize := _arc.GetPartSize(i) ;
      for ii := 0 to 1 do begin

        min_to_line_dist[ii] := FTolerance +1;
        is_to_line_connected[ii] := False;
        scno[ii] := 0;

        if ii = 0 then begin
          line.A := _arc.GetPoint(i, 0);
          line.B := _arc.GetPoint(i, 1);
        end
        else begin
          line.A := _arc.GetPoint(i, spartsize -1);
          line.B := _arc.GetPoint(i, spartsize -2);
        end ;

        be_ptg[ii] := line.A ;

        for c := 0 to localShpList.Count -1 do begin
          arc := TGIS_ShapeArc(localShpList[c]);
          arcpartsno := arc.GetNumParts ;
          for k := 0 to arcpartsno -1 do begin

            arcpartsize := arc.GetPartSize(k) ;
            for kk := 0 to arcpartsize -2 do begin
              if kk > 0 then
                wline.A := wline.B
              else begin  // for first point only
                wline.A  := arc.GetPoint(k, 0);
                dist := GisPoint2Point(line.A, wline.A);
                if dist <= FTolerance then begin
                  if not inc_cno_if_on_list(clist[ii], wline.A) then begin
                      cnp.cno  := 1;
                      cnp.cno  := 1;
                      cnp.pt   := _TGIS_Point (wline.A) ;
                      cnp.dist := dist ;
                      clist[ii].Add(cnp);
                    if dist <= Tolerance then
                      continue ;
                  end ;
                end ;
              end ;

              wline.B := arc.GetPoint(k, kk +1);

              dist := GisPoint2Point(wline.B, line.A) ;
              if dist <= FTolerance then begin
                if not inc_cno_if_on_list(clist[ii], wline.B) then begin
                    cnp.cno := 1;
                    cnp.pt := _TGIS_Point (wline.B) ;
                    cnp.dist := dist ;
                    clist[ii].Add(cnp);
                  if dist <= Tolerance then
                    continue ;
                end ;
              end ;

              dist := GisLine2Point(wline.A, wline.B, line.A);
              if (dist <= FTolerance) and (dist > Tolerance) then begin
                if dist < min_to_line_dist[ii] then begin
                  if GisGetLinesCrossing(line, wline, cross_ptg ) then begin
                    dist := GisPoint2Point(cross_ptg, line.A);
                    if dist < min_to_line_dist[ii] then begin
                      if (cross_ptg.X <> be_ptg[ii].X) or
                         (cross_ptg.Y <> be_ptg[ii].Y) then begin
                        if dist <= Tolerance then
                          continue ; // point is practically on line
                        min_to_line_dist[ii] := dist ;
                        is_to_line_connected[ii] := True ;
                        be_to_line_ptg[ii] := _TGIS_Point (cross_ptg) ;
                        continue ;
                      end ;
                    end ;
                  end ;

                  line.A.X := line.A.X +line.A.X -line.B.X ;
                  line.A.Y := line.A.Y +line.A.Y -line.B.Y ;

                  if GisGetLinesCrossing(line, wline, cross_ptg ) then begin
                    dist := GisPoint2Point(cross_ptg,  be_ptg[ii]);  //line.A changed
                    if dist < min_to_line_dist[ii] then begin
                      sline.B := _TGIS_Point (line.B) ;
                      sline.A := _TGIS_Point (cross_ptg) ;
                      min_to_line_dist[ii] := dist ;
                      is_to_line_connected[ii] := True ;
                      dx := cross_ptg.X -line.B.X ;
                      dy := cross_ptg.Y -line.B.Y ;
                      if (dx > 0) or (dy > 0) then begin
                        c2 := Sqrt(Sqr(dx) +Sqr
                        (dy)) ;
                        c2 := (c2/Tolerance)*4 ;
                        if c2 > 0 then begin
                          dx := (dx/c2);
                          dy := (dy/c2) ;
                          ratunku := 0;
                          while( not GisGetLinesCrossing(sline, wline, cross_ptg )) do
                          begin
                               sline.A.X :=  sline.A.X +dx ;
                               sline.A.Y :=  sline.A.Y +dy ;
                               inc( ratunku ) ;
                               if ratunku = 10 then begin
                                 break ;
                               end ;
                          end ;
                        end ;
                      end ;
                      be_to_line_ptg[ii] := _TGIS_Point (sline.A) ;
                    end ;
                  end ;
                  line.A := _TGIS_Point (be_ptg[ii]) ;
                end ;
              end ;
            end ;
          end ;
        end ; //end of the shape
      end ;

      for ii := 0 to 1 do begin
        if clist[ii].Count > 0 then begin
            cnp := T_connection_point( clist[ii][find_best_on_list(clist[ii])] ) ;
            be_ptg[ii] := _TGIS_Point (cnp.pt);
            if cnp.dist > 0 then
              is_connected := True
            else
            if is_to_line_connected[ii] and (cnp.cno = 1)  then begin
              if min_to_line_dist[ii] < cnp.dist then
            if GisPoint2Point(be_ptg[ii], be_to_line_ptg[ii]) > FTolerance
            then begin
              be_ptg[ii] := be_to_line_ptg[ii] ;
              is_connected := True
            end ;
          end ;
        end
        else
        if is_to_line_connected[ii]  then begin
          be_ptg[ii] := be_to_line_ptg[ii] ;
          is_connected := True ;
        end ;

        clist[ii].Clear ;
      end ;

      warc.AddPart ;
      warc.AddPoint(be_ptg[0]);

      for ii := 1 to spartsize -2 do
        warc.AddPoint(_arc.GetPoint(i, ii));

      warc.AddPoint(be_ptg[1]) ;

    end ;

    FreeObject( localShpList ) ;
    FreeObject( clist[0] ) ;
    FreeObject( clist[1] ) ;

    if is_connected then
      Result := TGIS_ShapeArc(ClearShape(warc)) ;

    FreeObject( warc ) ;
  end ;

  function TGIS_Topology.SplitByArc(
    const _shape      : TGIS_Shape    ;
    const _arc        : TGIS_ShapeArc
  ) : {$IFDEF CLR}
       IList           ;
     {$ELSE}
       TGIS_ObjectList ;
     {$ENDIF}
  begin
    Result := SplitByArc( _shape, _arc, False ) ;
  end ;

  function TGIS_Topology.SplitByArc(
    const _shape      : TGIS_Shape        ;
    const _arc        : TGIS_ShapeArc     ;
    const _fixshape   : Boolean
  ) : {$IFDEF CLR}
       IList           ;
     {$ELSE}
       TGIS_ObjectList ;
     {$ENDIF}
  var
    cpypolygon    : TGIS_ShapePolygon   ;
    {$IFDEF MANAGED}
      plg         : TGIS_Shape          ;
    {$ENDIF}
    i, k, n       : Integer             ;
    edge          : T_Edge              ;
    direction     : Integer             ;
    wpolygon      : TGIS_ShapePolygon   ;
    cwparts       : TGIS_ObjectList     ;
    ccwparts      : TGIS_ObjectList     ;
    cpart         : T_ContourPart       ;
    contour       : T_Contour           ;
    check_contour : T_Contour           ;
    pt            : TGIS_Point  {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF}          ;
    spt           : TGIS_ShapePoint     ;
    wmpoint       : TGIS_ShapeMultiPoint;
    topolist      : TList<TGIS_Topology>;
    topo          : TGIS_Topology       ;
    ctr_finded    : Boolean             ;
    resultList    : TGIS_ObjectList     ;
    shp           : TGIS_Shape          ;
    shplist       : TGIS_ShapeList     ;
    warc          : TGIS_ShapeArc       ;
    ext           : TGIS_Extent         ;

    dist          : Double  ;
    ptf, ptl      : TGIS_Point          ;

    procedure split_arc ;
    var
      warc1   : TGIS_ShapeArc ;
      k1, i1  : Integer ;
      ptno    : Integer ;
      vertex  : T_Vertex ;
      idx_max : Integer ;
    begin

      contour1 := T_Contour.Create( CONTOUR_OPENED, _shape, self) ;
      if T_Contour( contour1 ).Count <= 1 then begin
        FreeObject( contour1 ) ;
        exit ;
      end ;
      contour2 := T_Contour.Create( CONTOUR_OPENED, _arc, self) ;
      if T_Contour( contour2 ).Count <= 1 then begin
        FreeObject( contour2 ) ;
        FreeObject( contour1 ) ;
        exit ;
      end ;

      resultList := TGIS_ObjectList.Create( True ) ;

    // Computing contour1 and contour2 cross-vertex with adding them to the
    // contours.
      fixCrossVertices ;
      ptno := 0 ;
      warc1 := TGIS_ShapeArc(_shape.CreateCopy) ;
      warc1.Reset ;
      warc1.AddPart ;
      for k1 := 0 to T_Contour( contour1 ).PartsCount -1 do begin
        T_Contour( contour1 ).Part := k1 ;

        {$IFDEF MANAGED}
          vertex := T_Vertex( T_Contour( contour1 ).PartVertices.VerticesList[0] ) ;
        {$ELSE}
          vertex := T_Contour( contour1 ).PartVertices.VerticesList[0] ;
        {$ENDIF}

        idx_max := T_Contour( contour1 ).PartVertices.VerticesList.Count -1 ;
        for i1 := 0 to idx_max do begin

          if i1 > 0 then
            vertex := vertex.NextVertex ;
          warc1.AddPoint3D(vertex.NativePoint3);
          inc( ptno ) ;
          if vertex.IsCrossVertex and (ptno > 1) then begin
            resultList.Add(warc1) ;
            warc1 := TGIS_ShapeArc(_shape.CreateCopy) ;
            warc1.Reset ;
            warc1.AddPart ;
            warc1.AddPoint3D(vertex.NativePoint3);
            ptno := 1 ;
          end ;
        end ;
        if ptno > 1 then begin
           resultList.Add(warc1) ;
           warc1 := TGIS_ShapeArc(_shape.CreateCopy) ;
           warc1.Reset ;
           warc1.AddPart ;
           ptno := 0 ;
        end ;
      end ;
      FreeObject( contour2 ) ;
      FreeObject( contour1 ) ;
      FreeObject( warc1 ) ;
      if resultList.Count = 0 then
        FreeObject( resultList) ;
    end ;

    function cut_to_cross : Boolean ;
    var
      vk : T_Vertex ;
      k1  : Integer ;
      k_list : T_ContourPart ;
      eidx   : Integer ;
      c_part : Integer ;
      parts  : Integer ;
      part_ok : Array of Boolean ;
      del_part : Boolean ;
    begin
      Result := False ;
      del_part := False ;
      parts := T_Contour( contour2 ).PartsCount ;

      SetLength(part_ok, parts) ;
      for c_part := 0 to parts - 1 do
        part_ok[c_part] := True ;

      for c_part := 0 to parts - 1 do begin
        T_Contour( contour2 ).Part := c_part ;
        {$IFDEF MANAGED}
          k_list := T_ContourPart( T_Contour( contour2 ).listOfParts[c_part] ) ;
        {$ELSE}
          k_list := T_Contour( contour2 ).listOfParts[c_part] ;
        {$ENDIF}

        repeat
          if k_list.VerticesList.Count <= 1 then begin
            if (not Result) and (c_part = parts -1) then begin
              part_ok := nil ;
              exit ;
            end ;
            {$IFNDEF NEXTGEN}
              FreeObjectNotNil( T_Vertex(k_list.VerticesList[0] ) ) ;
            {$ENDIF}
            {$IFDEF OXYGENE}
            k_list.VerticesList.RemoveAt(0) ;
            {$ELSE}
            k_list.VerticesList.Delete(0) ;
            {$ENDIF}
            part_ok[c_part] := False ;
            del_part := True ;
            break ;
          end ;
          vk := T_Vertex(k_list.VerticesList[0]) ;
          vk.PrevVertex := vk ;
          if vk.IsCrossVertex then begin
            if (vk.NextEdgeSite <> T_EdgeSite.gesOutside) or
               (vk.CommonVertex.CommonVertex <> vk)  then //more then one
              break ;                                     //edge is crossed
            vk.CommonVertex.IsCrossVertex := False ;
            vk.CommonVertex.CommonVertex := nil ;
          end ;
          eidx := listOfEdges.IndexOf(vk.NextEdge) ;
          FreeObject( vk.NextEdge ) ;
          {$IFDEF OXYGENE}
            listOfEdges.RemoveAt(eidx) ;
          {$ELSE}
            listOfEdges.Delete(eidx) ;
          {$ENDIF}
          FreeObject( vk ) ;
          {$IFDEF OXYGENE}
            k_list.VerticesList.RemoveAt(0) ;
          {$ELSE}
            k_list.VerticesList.Delete(0) ;
          {$ENDIF}
        until False ;

        if k_list.VerticesList.Count = 0 then
          continue ;

        repeat
          if k_list.VerticesList.Count <= 1 then begin
            if (not Result) and (c_part = parts -1) then begin
              part_ok := nil ;
              exit ;
            end ;

            {$IFNDEF NEXTGEN}
              FreeObjectNotNil( T_Vertex(k_list.VerticesList[0])  ) ;
            {$ENDIF}
            {$IFDEF OXYGENE}
              k_list.VerticesList.RemoveAt(0) ;
            {$ELSE}
              k_list.VerticesList.Delete(0) ;
            {$ENDIF}
            part_ok[c_part] := False ;
            del_part := True ;
            break ;
          end
          else
            Result := True ;

          k1 := k_list.VerticesList.Count -1 ;
          vk := T_Vertex(k_list.VerticesList[k1]) ;
          vk.NextVertex := vk ;
          if vk.IsCrossVertex then begin
            if (vk.PrevEdgeSite = T_EdgeSite.gesInside)  or
               (vk.CommonVertex.CommonVertex <> vk)  then //more then one
              break ;                                     //edge is crossed
            vk.CommonVertex.IsCrossVertex := False ;
            vk.CommonVertex.CommonVertex := nil ;
          end ;
          eidx := listOfEdges.IndexOf(vk.PrevEdge) ;
          FreeObject( vk.PrevEdge ) ;
          {$IFDEF OXYGENE}
            listOfEdges.RemoveAt(eidx) ;
          {$ELSE}
            listOfEdges.Delete(eidx) ;
          {$ENDIF}
          FreeObject( vk ) ;
          {$IFDEF OXYGENE}
            k_list.VerticesList.RemoveAt(k1)
          {$ELSE}
            k_list.VerticesList.Delete(k1)
          {$ENDIF}
        until False ;
      end ;

      if del_part then begin
        for c_part := parts - 1 downto 0 do begin
          if not part_ok[c_part] then begin
            if T_Contour( contour2 ).FPartsCount = 1 then begin
              Result := False ;
              exit ;
            end ;
            {$IFDEF OXYGENE}
              T_Contour( contour2 ).listOfParts.RemoveAt(c_part);
            {$ELSE}
              T_Contour( contour2 ).listOfParts.Delete(c_part);
            {$ENDIF}
            dec( T_Contour( contour2 ).FPartsCount ) ;
          end ;
        end ;
      end ;

      part_ok := nil ;
      Result := True ;
    end ;

    function areSpliced( const _v1, _v2 : T_Vertex) : Boolean ;
    var
      nv : T_Vertex ;
    begin
      Result := False ;
      if (_v1.CommonVertex = nil) or (_v2.CommonVertex = nil) then
        exit ;
      nv := _v1.CommonVertex ;
      while True do begin
        if nv = _v2 then begin
          Result := True ;
          break ;
        end ;
        if nv = _v1 then
          break ;
        nv := nv.CommonVertex ;
      end ;
    end ;

    procedure spliceVertices( const _baseVertex, _nextVertex : T_Vertex) ;
    var
      cvertex : T_Vertex ;
      nvertex : T_Vertex ;
      dvertex : T_Vertex ;
      rvertex : T_Vertex ;
    begin
      if (_baseVertex.CommonVertex = nil) and
         (_nextVertex.CommonVertex = nil) then
      begin
        _baseVertex.IsCrossVertex := True ;
        _baseVertex.CommonVertex  := _nextVertex ;
        _nextVertex.IsCrossVertex := True ;
        _nextVertex.CommonVertex  := _baseVertex ;
        inc( crossingsNo ) ;
        exit ;
      end ;

      if _baseVertex.CommonVertex <> nil then
        if _baseVertex.CommonVertex = _nextVertex then
          exit ;

      if _baseVertex.CommonVertex = nil then
      begin
        dvertex := _baseVertex ;
        dvertex.IsCrossVertex := True ;
        _baseVertex.CommonVertex := _nextVertex ;
        cvertex := _nextVertex ;
        nvertex := _nextVertex ;
      end
      else
      if _nextVertex.CommonVertex = nil then
      begin
        dvertex := _nextVertex ;
        dvertex.IsCrossVertex := True ;
        _nextVertex.CommonVertex := _baseVertex ;
        cvertex := _baseVertex ;
        nvertex := _baseVertex ;
      end
      else
      begin

        if areSpliced(_baseVertex, _nextVertex) then begin
          exit ;
        end ;
        dvertex := _baseVertex.CommonVertex ;
        _baseVertex.CommonVertex := _nextVertex ;
        cvertex := _nextVertex ;
        nvertex := _nextVertex ;
      end ;
      rvertex := cvertex ;
      while cvertex.CommonVertex <> nvertex do begin
        cvertex := cvertex.CommonVertex ;
        if rvertex = cvertex then begin
          break ;
        end ;
      end ;
      cvertex.CommonVertex := dvertex ;
    end ;

    procedure make_contour1_edges ;
    var
      i1, k1  : Integer  ;
      vertex  : T_Vertex ;
      nvertex : T_Vertex ;
      edge1   : T_Edge   ;
      idx_max : Integer  ;
    begin

      for k1 := 0 to T_Contour( contour1 ).PartsCount -1 do begin
        T_Contour( contour1 ).Part := k1 ;

        {$IFDEF MANAGED}
          vertex := T_Vertex( T_Contour( contour1 ).PartVertices.VerticesList[0] ) ;
        {$ELSE}
          vertex := T_Contour( contour1 ).PartVertices.VerticesList[0] ;
        {$ENDIF}
        nvertex := vertex.NextVertex ;

        idx_max := T_Contour( contour1 ).PartVertices.VerticesList.Count -1 ;
        for i1 := 0 to idx_max do begin

          if i1 > 0 then begin
            vertex := nvertex ;
            nvertex := vertex.NextVertex ;
            if nvertex = vertex then break ;
          end ;

          edge1 := T_Edge.Create ;
          edge1.PrevVertex := vertex ;
          edge1.NextVertex := nvertex ;
          vertex.NextEdge := edge1 ;
          nvertex.PrevEdge := edge1 ;

          if vertex.NextEdgeSite = T_EdgeSite.gesInside   then
            edge1.Site := T_EdgeSite.gesInside
          else
          if vertex.NextEdgeSite = T_EdgeSite.gesSharedCD then
          begin
            edge1.Site := T_EdgeSite.gesInside ;
            vertex.NextEdgeSite := T_EdgeSite.gesInside ;
            nvertex.PrevEdgeSite := T_EdgeSite.gesInside ;
          end else
          if vertex.NextEdgeSite = T_EdgeSite.gesSharedSD then
          begin
            edge1.Site := T_EdgeSite.gesOutside ;
            vertex.NextEdgeSite := T_EdgeSite.gesOutside ;
            nvertex.PrevEdgeSite := T_EdgeSite.gesOutside ;
          end else
          begin
            edge1.Site := T_EdgeSite.gesOutside ;
            vertex.NextEdgeSite := T_EdgeSite.gesOutside ;
            nvertex.PrevEdgeSite := T_EdgeSite.gesOutside ;
          end ;

          listOfEdges.Add( edge1 ) ;
        end ;
      end ;
    end ;

    procedure close_contour2 ;
    var
      v, first_v, last_v : T_Vertex ;
      prev_v, sym_v : T_Vertex ;
      comm_v : T_Vertex ;
      first_comm_v : T_Vertex ;
      i1, idx : Integer ;
      vno : Integer ;
      c1idx : Integer ;
      c1v : T_Vertex ;
      c_part : Integer ;
      parts : Integer ;
      have_cont : Boolean ;
      cross_pass : T_CrossingPassType {$IFDEF JAVA} := T_CrossingPassType.gcpUndefined {$ENDIF} ;
      sdx, sdy : Double ;
      sdxr, sdyr : Double ;

      procedure add_cross_vertex_to_c1 ;
      begin
        c1v := T_Vertex.Create ;
        c1v.Owner := comm_v.Owner ;
        c1v.NativePoint2 := _TGIS_Point(sym_v.NativePoint2) ;
        c1v.NativePoint3 := _TGIS_Point3D(sym_v.NativePoint3) ;
        c1v.WorkPoint2 := _TGIS_Point(sym_v.WorkPoint2) ;
        c1v.WorkPoint3 := _TGIS_Point3D(sym_v.WorkPoint3) ;
        c1v.IsCrossVertex := True ;
        spliceVertices(sym_v, c1v) ;

      end ;

    begin

      parts := T_Contour( contour2 ).PartsCount ;
      sdxr := gridStep*10000 ;
      sdx := sdxr ;
      sdyr := gridStep*9876 ;
      sdy := sdyr ;

      for c_part := 0 to parts - 1 do begin

        T_Contour(contour2).Part := c_part ;
        vno := T_Contour(contour2).PartVertices.VerticesList.Count ;

        first_v := T_Vertex(T_Contour(contour2).PartVertices.VerticesList[0]) ;
        first_v.PrevEdgeSite := T_EdgeSite.gesOutside ;
        last_v := T_Vertex(T_Contour(contour2).PartVertices.VerticesList[vno -1]) ;
        last_v.NextEdgeSite := T_EdgeSite.gesOutside ;
        if last_v.WorkPoint2.X >= last_v.PrevVertex.WorkPoint2.X then begin
          if last_v.WorkPoint2.Y >= last_v.PrevVertex.WorkPoint2.Y then begin
            sdx := sdxr ;
            sdy := -sdyr ;
          end
          else begin
            sdx := -sdxr ;
            sdy := -sdyr ;
          end ;
        end
        else begin
          if last_v.WorkPoint2.Y >= last_v.PrevVertex.WorkPoint2.Y then begin
            sdx := sdxr ;
            sdy := sdyr ;
          end
          else begin
            sdx := -sdxr ;
            sdy := sdyr ;
          end ;
        end ;
        edge   := T_Edge.Create ;
        listOfEdges.Add(edge) ;

        edge.Site := T_EdgeSite.gesOutside ;
        edge.CommonEdge := nil ;
        edge.PrevVertex := last_v ;
        last_v.NextEdge := edge ;

        prev_v := last_v ;
        for idx := vno -1 downto 0 do begin
          v := T_Vertex(T_Contour(contour2).PartVertices.VerticesList[idx]) ;
          sym_v := T_Vertex.Create ;
          sym_v.PrevEdge := edge ;
          sym_v.PrevEdgeSite := edge.Site ;
          edge.NextVertex := sym_v ;
          prev_v.NextVertex := sym_v ;

          edge   := T_Edge.Create ;
          listOfEdges.Add(edge) ;
          if idx = 0 then
            edge.Site := T_EdgeSite.gesOutside
          else
            edge.Site := v.PrevEdgeSite ;

          edge.CommonEdge := nil ;
          edge.PrevVertex := sym_v ;
          sym_v.NextEdge := edge ;
          sym_v.NextEdgeSite := edge.Site ;

          T_Contour(contour2).PartVertices.VerticesList.Add(sym_v) ;
          sym_v.Owner := v.Owner ;
          sym_v.PrevVertex := prev_v ;
          sym_v.IsCrossVertex := v.IsCrossVertex ;
          sym_v.NativePoint2 := _TGIS_Point(v.NativePoint2) ;
          sym_v.NativePoint3 := _TGIS_Point3D(v.NativePoint3) ;
          sym_v.WorkPoint2 := _TGIS_Point(v.WorkPoint2) ;
          sym_v.WorkPoint3 := _TGIS_Point3D(v.WorkPoint3) ;

          sym_v.WorkPoint2.X := sym_v.WorkPoint2.X +sdx ;
          sym_v.WorkPoint2.Y := sym_v.WorkPoint2.Y +sdy ;

          sym_v.WorkPoint3.X := sym_v.WorkPoint2.X ;
          sym_v.WorkPoint3.Y := sym_v.WorkPoint2.Y ;

          if v.IsCrossVertex then begin
            if v = v.CommonVertex.CommonVertex then begin
            // normal way
              c1idx := 0 ;
              comm_v := v.CommonVertex ;
              for i1 := 0 to T_Contour(contour1).PartsCount -1 do begin
                T_Contour(contour1).Part := i1 ;
                c1idx := T_Contour(contour1)
                        .PartVertices.VerticesList.IndexOf(comm_v) ;
                if c1idx >= 0 then break ;
              end ;

              if c1idx < 0 then begin
                prev_v := sym_v ;
                v.IsCrossVertex := False ;
                v.CommonVertex := nil ;
                continue ;
              end ;

              if (v.NextEdgeSite = T_EdgeSite.gesInside) and
                 ( (v.PrevEdgeSite = T_EdgeSite.gesOutside ) or
                   (v.PrevEdgeSite = T_EdgeSite.gesSharedSD) or
                   (v.PrevEdgeSite = T_EdgeSite.gesSharedCD) )
              then begin
                add_cross_vertex_to_c1 ;

                comm_v.PrevVertex.NextVertex := c1v ;
                c1v.PrevVertex := comm_v.PrevVertex ;
                c1v.NextVertex := comm_v ;
                comm_v.PrevVertex := c1v;
                comm_v.PrevEdgeSite := T_EdgeSite.gesInside ;
                c1v.NextEdgeSite := T_EdgeSite.gesInside ;
                T_Contour(contour1).PartVertices.VerticesList.Insert(c1idx, c1v) ;
              end else
              if (v.PrevEdgeSite = T_EdgeSite.gesInside) and
                 ( (v.NextEdgeSite = T_EdgeSite.gesOutside ) or
                   (v.NextEdgeSite = T_EdgeSite.gesSharedSD) or
                   (v.NextEdgeSite = T_EdgeSite.gesSharedCD) )
              then begin
                add_cross_vertex_to_c1 ;

                c1v.PrevVertex := comm_v ;
                c1v.NextVertex := comm_v.NextVertex ;
                comm_v.NextVertex.PrevVertex := c1v;
                comm_v.NextVertex := c1v;
                comm_v.NextEdgeSite := T_EdgeSite.gesInside ;
                c1v.PrevEdgeSite := T_EdgeSite.gesInside ;

                if c1idx = T_Contour(contour1).PartVertices.VerticesList.Count -1 then
                  T_Contour(contour1).PartVertices.VerticesList.Add(c1v)
                else
                  T_Contour(contour1).PartVertices.VerticesList.Insert(c1idx +1, c1v) ;
              end
              else
              if (v.PrevEdgeSite = T_EdgeSite.gesInside) and
                 (v.NextEdgeSite = T_EdgeSite.gesInside)
              then begin
                sym_v.IsCrossVertex := False ;
                v.IsCrossVertex := False ;
                v.CommonVertex.IsCrossVertex := False ;
              end
              else //(v.PrevEdgeSite = T_EdgeSite.gesOutside) and (v.NextEdgeSite = T_EdgeSite.gesOutside)
              begin
                sym_v.IsCrossVertex := False ;
              end ;

            end
            else begin

              first_comm_v := v ;

              have_cont := False ;

              while True do begin

                c1idx := 0 ;
                comm_v := v.CommonVertex ;
                for i1 := 0 to T_Contour(contour1).PartsCount -1 do begin
                  T_Contour(contour1).Part := i1 ;
                  c1idx := T_Contour(contour1)
                      .PartVertices.VerticesList.IndexOf(comm_v) ;
                  if c1idx >= 0 then break ;
                end ;

                if c1idx < 0 then begin
                  prev_v := sym_v ;
                  v.IsCrossVertex := False ;
                  v.CommonVertex := nil ;
                  have_cont := True ;
                  break ;
                end ;

                cross_pass := T_Contour(contour1).PartVertices.
                                  checkCrossingPassType(comm_v, first_comm_v) ;

                if cross_pass = T_CrossingPassType.gcpOutsideInside then
                begin
                  add_cross_vertex_to_c1 ;

                  comm_v.PrevVertex.NextVertex := c1v ;
                  c1v.PrevVertex := comm_v.PrevVertex ;
                  c1v.NextVertex := comm_v ;
                  comm_v.PrevVertex := c1v;
                  comm_v.PrevEdgeSite := T_EdgeSite.gesInside ;
                  c1v.NextEdgeSite := T_EdgeSite.gesInside ;
                  T_Contour(contour1).PartVertices.VerticesList.Insert(c1idx, c1v) ;
                end else
                if cross_pass = T_CrossingPassType.gcpInsideOutside then
                begin
                  add_cross_vertex_to_c1 ;

                  c1v.PrevVertex := comm_v ;
                  c1v.NextVertex := comm_v.NextVertex ;
                  comm_v.NextVertex.PrevVertex := c1v;
                  comm_v.NextVertex := c1v;
                  comm_v.NextEdgeSite := T_EdgeSite.gesInside ;
                  c1v.PrevEdgeSite := T_EdgeSite.gesInside ;

                  if c1idx = T_Contour(contour1).PartVertices.VerticesList.Count -1 then
                    T_Contour(contour1).PartVertices.VerticesList.Add(c1v)
                  else
                    T_Contour(contour1).PartVertices.VerticesList.Insert(c1idx +1, c1v) ;
                end
                else
                if (first_comm_v.PrevEdgeSite = T_EdgeSite.gesInside) and
                   (first_comm_v.NextEdgeSite = T_EdgeSite.gesInside)
                then begin
                  sym_v.IsCrossVertex := False ;
                  v.IsCrossVertex := False ;
                  v.CommonVertex.IsCrossVertex := False ;
                  break ;
                end
                else //(v.PrevEdgeSite = T_EdgeSite.gesOutside) and (v.NextEdgeSite = T_EdgeSite.gesOutside)
                begin
                  sym_v.IsCrossVertex := False ;
                  break ;
                end ;
                if comm_v.CommonVertex = first_comm_v then
                  break ;

                v := v.CommonVertex ;
              end ;

              if have_cont then
                continue ;

            end ;

          end ;

          prev_v := sym_v ;
        end ;
        edge.NextVertex := first_v ;
        prev_v.NextVertex := first_v ;
        first_v.PrevVertex := prev_v ;
        first_v.PrevEdge := edge ;
      end ;
    end ;

    procedure close_contour2_closed ;
    var
      v, first_v : T_Vertex ;
      prev_sym_v, first_sym_v, sym_v : T_Vertex ;
      comm_v : T_Vertex ;
      i1 : Integer ;
      vno : Integer ;
      c1idx : Integer ;
      c1v : T_Vertex ;
      added_part : T_ContourPart ;
      prev_edge : T_Edge ;

      procedure add_cross_vertex_to_c1 ;
      begin
        c1v := T_Vertex.Create ;
        c1v.Owner := comm_v.Owner ;
        c1v.NativePoint2 := _TGIS_Point(sym_v.NativePoint2) ;
        c1v.NativePoint3 := _TGIS_Point3D(sym_v.NativePoint3) ;
        c1v.WorkPoint2 := _TGIS_Point(sym_v.WorkPoint2) ;
        c1v.WorkPoint3 := _TGIS_Point3D(sym_v.WorkPoint3) ;
        c1v.IsCrossVertex := True ;
        spliceVertices(sym_v, c1v) ;
      end ;

      procedure redef_site_contour1 ;
      var
        fv, v2 : T_Vertex ;
        i2 : Integer ;
      begin
        for i2 := 0 to T_Contour(contour1).FPartsCount - 1 do begin
          T_Contour(contour1).Part := i2 ;
          v2 := T_Vertex(T_Contour(contour1).PartVertices.VerticesList[0]) ;
          fv := v2 ;
          while True do begin
            if ( v2.PrevEdgeSite <> T_EdgeSite.gesSharedSD ) and
               ( v2.PrevEdgeSite <> T_EdgeSite.gesSharedCD ) then
            begin
              if v2.NextEdge <> nil then begin
                v2.NextEdgeSite := T_EdgeSite.gesOutside ;
              end ;
              if v2.PrevEdge <> nil then begin
                v2.PrevEdgeSite := T_EdgeSite.gesOutside ;
                v2.PrevEdge.Site := T_EdgeSite.gesOutside ;
              end ;
            end ;
            v2 := v2.PrevVertex ;
            if v2 = fv then
              break ;
          end ;
        end ;
      end ;

    begin
      //Used only for one-part contour

      T_Contour(contour2).Part := 0 ;

      vno := T_Contour(contour2).PartVertices.VerticesList.Count ;
      added_part := T_ContourPart.Create ;
      {$IFNDEF JAVA OR ISLAND}
        added_part.VerticesList.Capacity := vno ;
      {$ENDIF}

      first_v := T_Vertex(T_Contour(contour2).PartVertices.VerticesList[0]) ;
      v := first_v ;
      prev_sym_v := nil ;
      first_sym_v := nil ;
      prev_edge := nil ;

      repeat
        edge   := T_Edge.Create ;
        listOfEdges.Add(edge) ;
        if v.PrevEdgeSite = T_EdgeSite.gesSharedSD then begin
          edge.Site := T_EdgeSite.gesSharedCD ;
        end
        else
        if v.PrevEdgeSite = T_EdgeSite.gesSharedCD then begin
          v.PrevEdgeSite := T_EdgeSite.gesOutside ;
          edge.Site := T_EdgeSite.gesOutside ;
        end
        else
          edge.Site := v.PrevEdgeSite ;

        sym_v := T_Vertex.Create ;
        sym_v.NextEdge := edge ;
        sym_v.NextEdgeSite := edge.Site ;
        edge.PrevVertex := sym_v ;
        if prev_sym_v <> nil then begin
          prev_sym_v.NextVertex := sym_v ;
          prev_edge.NextVertex := sym_v ;
          sym_v.PrevEdgeSite := prev_edge.Site ;
          sym_v.PrevVertex := prev_sym_v ;
          sym_v.PrevEdge := prev_edge ;
        end
        else begin
          first_sym_v := sym_v ;
        end ;

        edge.CommonEdge := nil ;

        added_part.VerticesList.Add(sym_v) ;
        sym_v.Owner := v.Owner ;
        sym_v.IsCrossVertex := v.IsCrossVertex ;
        sym_v.NativePoint2 := _TGIS_Point(v.NativePoint2) ;
        sym_v.NativePoint3 := _TGIS_Point3D(v.NativePoint3) ;
        sym_v.WorkPoint2 := _TGIS_Point(v.WorkPoint2) ;
        sym_v.WorkPoint3 := _TGIS_Point3D(v.WorkPoint3) ;
        sym_v.WorkPoint2.X := sym_v.WorkPoint2.X +gridStep*10000 ;
        sym_v.WorkPoint3.X := sym_v.WorkPoint2.X ;

        if v.IsCrossVertex then begin
          c1idx := 0 ;
          comm_v := v.CommonVertex ;
          for i1 := 0 to T_Contour(contour1).PartsCount -1 do begin
            T_Contour(contour1).Part := i1 ;
            c1idx := T_Contour(contour1)
                    .PartVertices.VerticesList.IndexOf(comm_v) ;
            if c1idx >= 0 then break ;
          end ;

          if c1idx < 0 then begin
            prev_sym_v := sym_v ;
            v.IsCrossVertex := False ;
            v.CommonVertex := nil ;
            continue ;
          end ;

          if ((v.NextEdgeSite = T_EdgeSite.gesInside) and (v.PrevEdgeSite <> T_EdgeSite.gesInside ))
            or
             ((v.NextEdgeSite = T_EdgeSite.gesSharedSD) and (v.PrevEdgeSite = T_EdgeSite.gesOutside ))
          then begin
            add_cross_vertex_to_c1 ;

            c1v.PrevVertex := comm_v.PrevVertex ;
            c1v.NextVertex := comm_v ;
            comm_v.PrevVertex.NextVertex := c1v ;
            comm_v.PrevVertex := c1v;

            comm_v.PrevEdgeSite := T_EdgeSite.gesInside ;
            c1v.NextEdgeSite := T_EdgeSite.gesInside ;

            c1v.PrevEdge := comm_v.PrevEdge ;
            c1v.PrevEdge.NextVertex := c1v ;
            comm_v.PrevEdge := nil ;

            T_Contour(contour1).PartVertices.VerticesList.Insert(c1idx, c1v) ;
          end else
          if ((v.PrevEdgeSite = T_EdgeSite.gesInside) and (v.NextEdgeSite <> T_EdgeSite.gesInside))
            or
             ((v.PrevEdgeSite = T_EdgeSite.gesSharedSD) and (v.NextEdgeSite = T_EdgeSite.gesOutside ))
          then begin
            add_cross_vertex_to_c1 ;

            c1v.PrevVertex := comm_v ;
            c1v.NextVertex := comm_v.NextVertex ;
            comm_v.NextVertex.PrevVertex := c1v;
            comm_v.NextVertex := c1v;

            c1v.NextEdge := comm_v.NextEdge ;
            c1v.NextEdge.PrevVertex := c1v ;

            comm_v.NextEdge := nil ;
            comm_v.NextEdgeSite := T_EdgeSite.gesInside ;
            c1v.PrevEdgeSite := T_EdgeSite.gesInside ;
            if c1idx = T_Contour(contour1).PartVertices.VerticesList.Count -1 then
              T_Contour(contour1).PartVertices.VerticesList.Add(c1v)
            else
              T_Contour(contour1).PartVertices.VerticesList.Insert(c1idx +1, c1v) ;
          end
          else
          if (v.PrevEdgeSite = T_EdgeSite.gesInside) and
             (v.NextEdgeSite = T_EdgeSite.gesInside)
          then
          begin
            sym_v.IsCrossVertex := False ;
            v.IsCrossVertex := False ;
            v.CommonVertex.IsCrossVertex := False ;
          end ;

        end ;

        prev_sym_v := sym_v ;
        prev_edge := edge ;
        v := v.PrevVertex ;
        dec( vno ) ;
        if vno <= 1 then
          vno := 0 ;
        if v = T_Vertex(T_Contour(contour2).PartVertices.VerticesList[0])
        then begin
          break ;
        end ;

      until v = first_v ;

      edge.NextVertex := first_sym_v ;
      sym_v.NextVertex := first_sym_v ;
      first_sym_v.PrevVertex := sym_v ;
      first_sym_v.PrevEdge := edge ;
      first_sym_v.PrevEdgeSite := edge.Site ;
      redef_site_contour1 ;
      T_Contour(contour2).AddPart(added_part) ;
      added_part.SignedArea2 := -T_Contour(contour2).PartVertices.SignedArea2 ;
      added_part.ParentPart := T_Contour(contour2).PartVertices ;

    end ;

    procedure free_descriptors ;
    var
      k1 : Integer ;
    begin
      for k1 := listOfVdescriptors.Count -1 downto 0 do begin
        vdescriptors := TList<TObject>( listOfVdescriptors[k1] ) ;
        freeDescriptors( vdescriptors ) ;
      end  ;
      FreeObject( listOfVdescriptors ) ;
    end ;

    procedure free_edges ;
    var
      k1 : Integer ;
    begin
      for k1 := listOfEdges.Count -1 downto 0 do begin
        {$IFNDEF NEXTGEN}
          FreeObjectNotNil( T_Edge( listOfEdges[k1] ) ) ;
        {$ENDIF}
      end ;
      FreeObject( listOfEdges ) ;
    end ;

  begin

    Result := nil ;
    resultList := nil ;
    try
      if ((_shape.ShapeType <> TGIS_ShapeType.Arc        ) and
          (_shape.ShapeType <> TGIS_ShapeType.Polygon    ) and
          (_shape.ShapeType <> TGIS_ShapeType.MultiPoint )    )
         or
         ( _arc.ShapeType <> TGIS_ShapeType.Arc)
      then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGSHAPE ), '', 0 ) ;

      if ( _arc.GetPartSize(0) < 2 ) or
         (_arc.Extent.XMin > _shape.Extent.XMax) or
         (_arc.Extent.XMax < _shape.Extent.XMin) or
         (_arc.Extent.YMin > _shape.Extent.YMax) or
         (_arc.Extent.YMax < _shape.Extent.YMin)
      then begin
        resultList := TGIS_ObjectList.Create( True ) ;
        resultList.Add( _shape.CreateCopy ) ;
        exit ;
      end ;

      pshape1 := _shape ;
      pshape2 := _arc ;

      computeTolerance ;

      wpolygon := nil ;

      case _shape.ShapeType of
        TGIS_ShapeType.Arc :
          begin
            if _shape.GetPartSize(0) < 2 then exit ;
            split_arc ;
            exit ;
          end ;
        TGIS_ShapeType.Polygon :
          begin
            if _shape.GetPartSize(0) < 3 then exit ;
            if _fixshape then
              FixShape(_shape) ;
            wpolygon := TGIS_ShapePolygon(_shape) ;
          end ;
        TGIS_ShapeType.MultiPoint :
          begin
            if _shape.GetPartSize(0) < 1 then exit ;
            wpolygon := TGIS_ShapePolygon.Create(nil, nil, False, 0, _shape.Layer,
                        _shape.Dimension);
            wpolygon.AddPart ;
            if GisLockGreaterThanEqual( _shape.LockLevel, TGIS_Lock.Projection ) then begin
              ext := _shape.Extent ;
              wpolygon.Lock(_shape.LockLevel)
            end
            else begin
              ext := _shape.ProjectedExtent ;
              wpolygon.Lock(TGIS_Lock.Extent)
            end ;
            pt.X := ext.XMin ;//-gridStep;
            pt.Y := ext.YMin ;//-gridStep;
            wpolygon.AddPoint(pt) ;
            pt.Y := ext.YMax ;//+gridStep;
            wpolygon.AddPoint(pt) ;
            pt.X := ext.XMax ;//+gridStep;
            wpolygon.AddPoint(pt) ;
            pt.Y := ext.YMin ;//-gridStep;
            wpolygon.AddPoint(pt) ;
            wpolygon.Unlock ;
          end ;
      end ;

      pshape1 := wpolygon ;

      contour1 := T_Contour.Create( CONTOUR_CLOSED, wpolygon, self) ;
      if T_Contour( contour1 ).Count = 0 then begin
        FreeObject( contour1 ) ;
        exit ;
      end ;

      n := _arc.GetNumParts ;
      if n > 1 then begin
        topo := TGIS_Topology.Create ;
        shplist := TGIS_ShapeList.Create( False ) ;
        for k := 0 to n -1 do begin
          shp := TGIS_ShapeArc.Create(nil, nil, False, 0, nil);
          shp.Lock(TGIS_Lock.Extent);
          shp.AddPart ;
          for i := 0 to _arc.GetPartSize(k) -1 do
            shp.AddPoint(_arc.GetPoint(k, i)) ;
          shp.Unlock();
          shplist.Add(shp) ;
        end ;

        warc := TGIS_ShapeArc(topo.UnionOnList(shplist, False)) ;

        for k := n -1 downto 0 do
          {$IFDEF MANAGED}
            FreeObjectNotNil( TGIS_Shape(shplist[k]) ) ;
          {$ELSE}
            TGIS_Shape(shplist.Items[k]).Free ;  //?
          {$ENDIF}
        FreeObject( shplist ) ;
        FreeObject( topo    ) ;
      end
      else
        warc := _arc ;

      pshape2 := warc ;

      _arc.IsFixed := False ;
      ptf :=  warc.GetFirstPoint ;
      ptl :=  warc.GetLastPoint ;
      dist := GisPoint2Point(ptf, ptl) ;
      if ( warc.GetNumParts = 1 ) and ( dist <= FTolerance ) then  begin
        contour2 := T_Contour.Create( CONTOUR_CLOSED, warc, self) ;
        if T_Contour(contour2).PartVertices = nil then begin
          FreeObject( contour2 ) ;
          contour2 := T_Contour.Create( CONTOUR_OPENED, warc, self) ;
        end ;
      end
      else
        contour2 := T_Contour.Create( CONTOUR_OPENED, warc, self) ;
      if warc <> _arc then
        FreeObject( warc ) ;

      combineType := TGIS_TopologyCombineType.Difference ;

      listOfVdescriptors := TList<TObject>.Create ;

      // Computing contour1 and contour2 cross-vertex with adding them to the
      // contours. Simultaneously edges and cross-vertex descriptors
      // will be created
      fixCrossVertices ;

      createEdgesAndDfnSite( contour2, contour1 ) ;
      if T_Contour(contour2).isClosed then begin
        createEdgesAndDfnSite( contour1, contour2 ) ;
      end ;

      if ( not T_Contour(contour2).isClosed ) then begin
        //deleting not needed parts
        if (crossingsNo = 0) or (not T_Contour(contour2).IsEdgeInside) then begin
          resultList := TGIS_ObjectList.Create( True ) ;
          resultList.Add( _shape.CreateCopy ) ;
          free_descriptors ;
          free_edges ;
          FreeObject( contour1 ) ;
          FreeObject( contour2 ) ;
          exit ;
        end ;
      end ;

      cwparts  := TGIS_ObjectList.Create( True ) ;
      ccwparts := TGIS_ObjectList.Create( False ) ; ;

      // close contour2
      if ( T_Contour(contour2).isClosed ) then
        close_contour2_closed
      else begin
        close_contour2 ;
       // polygons edges making and site defining
        make_contour1_edges ;
      end ;

      // adding descriptors
      addCrossVrtxDesc( contour1 ) ;

      // adding descriptors
      addCrossVrtxDesc( contour2 ) ;

      // edges angles computing
      for i := 0 to listOfVdescriptors.Count -1 do
        computeEdgesAngles( TList<TObject>( listOfVdescriptors[i] ) ) ;

      // connectivity lists sorting
      for i := 0 to listOfVdescriptors.Count -1 do
        {$IFNDEF OXYGENE}
          TList<TObject>( listOfVdescriptors.Items[i] ).Sort(  TComparer<TObject>.Construct( AnglesVerticesCompare ) ) ;
        {$ELSE}
          TList<TObject>( listOfVdescriptors[i] ).Sort( @AnglesVerticesCompare ) ;
        {$ENDIF}

      // making common contour
      commonContour := T_Contour( contour1 ).JoinContour( T_Contour( contour2 ) ) ;

     // work contour part
      cpart := T_ContourPart.Create ;

      for i := 0 to listOfEdges.Count -1 do begin
        edge := T_Edge( listOfEdges[i] ) ;

        if edge.Marked then
          continue ;

        if edgeRule(edge, direction) then begin
          if direction = ord( T_EdgeDirection.gedForward ) then
            collectPolygonCPart(edge.PrevVertex, direction, cpart)
          else
            collectPolygonCPart(edge.NextVertex, direction, cpart) ;
        end ;
        if cpart.VerticesList.Count >= 3 then begin
          if checkCombine then begin
            check_contour := T_Contour.Create ;
            check_contour.isClosed := True ;
            check_contour.FExtent := cpart.Extent ;
            check_contour.AddPart(cpart) ;
            check_contour.checkAllVertices ;
            if check_contour.FPartsCount > 1 then begin
              for k := check_contour.FPartsCount - 1 downto 1 do begin
                cpart := T_ContourPart(check_contour.listOfParts[k]) ;
                if Abs(cpart.SignedArea2) > FTolerance then begin
                  if cpart.SignedArea2 < 0 then begin
                    contour := T_Contour.Create ;
                    contour.FExtent := cpart.Extent ;
                    contour.AddPart(cpart) ;
                    cwparts.Add(contour) ;
                  end
                  else
                    ccwparts.Add(cpart) ;
                end
                else
                  FreeObject(cpart) ;
              end ;
            end ;
            cpart := T_ContourPart(check_contour.listOfParts[0]) ;
            check_contour.Clear ;
            FreeObject(check_contour) ;
          end ;

          if Abs(cpart.SignedArea2) > FTolerance then begin
            if cpart.SignedArea2 < 0 then begin
              contour := T_Contour.Create ;
              contour.isClosed := True ;
              contour.FExtent := cpart.Extent ;
              contour.AddPart(cpart) ;
              cwparts.Add(contour) ;
            end
            else
              ccwparts.Add(cpart) ;
            cpart := T_ContourPart.Create ;
          end
          else
            cpart.Clear ;
        end
        else
          cpart.Clear ;
      end ;

      FreeObject(cpart) ;

      for k := 0 to ccwparts.Count -1 do begin
        cpart := T_ContourPart( ccwparts.Items[k] ) ;
        for i := 0 to cwparts.Count -1 do begin
          contour := T_Contour( cwparts.Items[i] ) ;
          ctr_finded := True ;
          for n := 0 to cpart.VerticesList.Count -1 do begin
            if not contour.IsPointInsideContour
                           (T_Vertex(cpart.VerticesList[n]).WorkPoint2)
            then begin
              if contour.IsPointOnContour
                           (T_Vertex(cpart.VerticesList[n]).WorkPoint2)
              then
                continue ;
              ctr_finded := False ;
              break ;
            end ;
          end ;
          if ctr_finded then begin
            contour.AddPart(cpart);
          end ;
        end ;
      end ;

      FreeObject( ccwparts ) ;
      // contours -> shapes
      for i := 0 to cwparts.Count -1 do begin
        contour := T_Contour( cwparts.Items[i] ) ;
        cpypolygon := TGIS_ShapePolygon(wpolygon.CreateCopy) ;

        if GisLockGreaterThanEqual( wpolygon.LockLevel, TGIS_Lock.Projection ) then
          cpypolygon.Lock(wpolygon.LockLevel)
        else
          cpypolygon.Lock(TGIS_Lock.Extent) ;
        {$IFDEF MANAGED}
          plg := cpypolygon ;
          contour.Write2Shape(plg) ;
        {$ELSE}
          contour.Write2Shape(TGIS_Shape(cpypolygon)) ;
        {$ENDIF}
        cpypolygon.Unlock ;
        cwparts.Items[i] := cpypolygon ;
      end ;

      // on end objects destroying
      for k := listOfVdescriptors.Count -1 downto 0 do begin
        vdescriptors := TList<TObject>( listOfVdescriptors[k] ) ;
        freeDescriptors( vdescriptors ) ;
      end  ;

      FreeObject( listOfVdescriptors ) ;

      T_Contour(commonContour).Clear ;
      FreeObject( commonContour ) ;
      FreeObject( contour1 ) ;
      FreeObject( contour2 ) ;

      free_edges ;

      if _shape.ShapeType = TGIS_ShapeType.MultiPoint then begin
        resultList := TGIS_ObjectList.Create( True ) ;
        topolist := TList<TGIS_Topology>.Create ;
        for i := 0 to cwparts.Count -1 do begin
          wmpoint := TGIS_ShapeMultiPoint.Create(nil, nil, False, 0, _shape.Layer);
          topo := TGIS_Topology.Create ;
          topo.RelatePrepare( TGIS_Shape( cwparts.Items[i] ) );
          topolist.Add(topo) ;
          wmpoint.AddPart ;
          resultList.Add(wmpoint) ;
        end ;

        spt := TGIS_ShapePoint.Create(nil, nil, False, 0, _shape.Layer);
        for i := 0 to _shape.GetNumParts -1 do begin
          for k := 0 to _shape.GetPartSize(i) -1 do begin
            pt := _shape.GetPoint(i, k) ;
            spt.Reset ;
            spt.AddPart ;
            spt.AddPoint(pt) ;
            for n := 0 to cwparts.Count -1 do begin
              topo := TGIS_Topology( topolist[n] ) ;
              if topo.Relate(spt, TGIS_Shape( cwparts.Items[n] ), '**F') then
              begin
                TGIS_ShapeMultiPoint(resultList.Items[n]).AddPoint(pt) ;
                break ;
              end ;
            end ;
          end ;
        end ;
        FreeObject(spt) ;
        i := 0 ;
        repeat
          if i = resultList.Count then
            break ;
          wmpoint := TGIS_ShapeMultiPoint( resultList.Items[i] ) ;
          if wmpoint.GetNumPoints = 0 then begin
            FreeObject(wmpoint) ;
            resultList.Delete(i);
          end else
            inc( i ) ;
        until False ;
        if resultList.Count = 0 then
          FreeObject(resultList) ;

        for n := cwparts.Count -1 downto 0 do begin
          cpypolygon := TGIS_ShapePolygon( cwparts.Items[n] ) ;
          topo := TGIS_Topology( topolist[n] ) ;
          FreeObject(topo) ;
        end ;
        FreeObject( cwparts  ) ;
        FreeObject( topolist ) ;
        FreeObject( wpolygon ) ;

      end
      else
        resultList := cwparts ;

    finally
      if assigned( resultList ) then begin
        {$IFDEF CLR}
          Result := System.Collections.ArrayList.Create ;
          try
            for n := 0 to resultList.Count - 1 do begin
              Result.Add( resultList[n] ) ;
            end ;
          finally
            resultList.OwnsObjects := False ;
            FreeObject( resultList ) ;
          end ;
        {$ELSE}
          Result := resultList ;
          Result.OwnsObjects := True ;
        {$ENDIF}
        if Result.Count > 0 then begin
          for n := 0 to Result.Count - 1 do begin
            TGIS_Shape( Result[n] ).Unlock ;
          end ;
        end
        else
          Result.Add( _shape.CreateCopy ) ;
      end ;
    end

  end ;

  function TGIS_Topology.CheckShape(
    const _shp : TGIS_Shape
  ) : Boolean ;
  var
    shp : TGIS_Shape ;
  begin
    shp := FixShapeEx( _shp, True, True, Result ) ;
    Result := not Result ; //if _wasfixed is True shape is not valid
    FreeObject( shp ) ;
  end ;

  function  TGIS_Topology.PartStatus(
    const _shp          : TGIS_ShapePolygon ;
    const _part         : Integer           ;
    var   _parentPart   : Integer
  ) : Integer ;
  var
    c         : T_Contour ;
    p         : T_ContourPart ;
    pp        : T_ContourPart ;
    i         : Integer ;
    llevel    : TGIS_Lock ;
    ff        : Boolean ;
  begin
    Result      := 0 ;
    if _part < 0 then exit ;
    _parentPart := -1 ;

    if not assigned( _shp ) then exit ;
    if _shp.GetNumParts = 1 then
      exit ;

    if _shp.ShapeType <> TGIS_ShapeType.Polygon then exit ;

    ff := FForceShapeFixing ;
    llevel := _shp.LockLevel ;
    _shp.Lock( TGIS_Lock.Projection );
    try
      if not assigned( contour1 ) or ( pshape1 <> _shp ) then begin
        RelatePrepare( _shp ) ;

        pshape1 := _shp ;
        pshape2 := nil ;

        computeTolerance ;

        FForceShapeFixing := True ;
        contour1 := T_Contour.Create( CONTOUR_CLOSED, _shp, self ) ;
      end ;

      c := T_Contour( contour1 ) ;

      if c.FPartsCount <= _part then
        exit ;

      for i := 0 to  c.FPartsCount -1 do begin
        p := T_ContourPart( c.listOfParts[i] ) ;
        if p.PartNumber = _part then begin
          if p.ParentPart <> nil then begin
            pp := p.ParentPart ;
            _parentPart := pp.PartNumber ;
            if p.SignedArea2 < 0 then
              Result := 1
            else
              Result := -1 ;
          end
          else begin
            _parentPart := -1 ;
            Result := 0
          end;
          break ;
        end;
      end;
    finally
     _shp.Unlock() ;
      if llevel <> TGIS_Lock.None then
        _shp.Lock(llevel) ;
      FForceShapeFixing := ff ;
    end ;
  end ;

  function TGIS_Topology.FixShape(
    const _shp : TGIS_Shape
  ) : TGIS_Shape ;
  begin
    Result := FixShape( _shp, False ) ;
  end ;

  function TGIS_Topology.FixShape(
    const _shp          : TGIS_Shape ;
    const _returnnewobj : Boolean
  ) : TGIS_Shape ;
  var
    wasfixed : Boolean ;
  begin
    checkCombine := True ;
    Result := FixShapeEx(_shp, _returnnewobj, wasfixed ) ;
    checkCombine := False ;
  end ;

  function TGIS_Topology.FixShapeEx(
    const _shp          : TGIS_Shape ;
    const _returnnewobj : Boolean    ;
    var   _wasfixed     : Boolean
  ) : TGIS_Shape ;
  var
    sh1, sh2      : TGIS_Shape ;
    ctr           : T_Contour  ;
    i             : Integer    ;
    cor           : Integer    ;
    npoints       : Integer    ;
    llevel        : TGIS_Lock  ;
  begin
    llevel := _shp.LockLevel ;
    _shp.Lock( TGIS_Lock.Projection );
    try
      Result := nil ;
      sh1 := pshape1 ;
      sh2 := pshape2 ;

      pshape1 := _shp ;
      pshape2 := nil ;

      computeTolerance ;

      if _shp.ShapeType = TGIS_ShapeType.Polygon then begin
        ctr := T_Contour.Create( CONTOUR_CLOSED, _shp, self, False, forceFix ) ;
        cor := 1 ;
      end
      else
      if _shp.ShapeType = TGIS_ShapeType.Arc then begin
        ctr := T_Contour.Create( CONTOUR_OPENED, _shp, self, False, forceFix ) ;
        cor := 0 ;
      end
      else begin
        if _returnnewobj then
          Result := _shp.CreateCopy
        else
          Result := _shp ;
        exit ;
      end ;

      npoints := 0 ;
      if not ctr.isChanged then begin
        for i := 0 to ctr.listOfParts.Count - 1 do begin
          if T_ContourPart( ctr.listOfParts[i] ).PartChanged then begin
            ctr.isChanged := True ;
            break ;
          end ;
          npoints := npoints +
                     T_ContourPart( ctr.listOfParts[i] ).VerticesList.Count +cor ;
        end ;
      end ;
      if not ctr.isChanged then
        if (npoints <> _shp.GetNumPoints) or
           (ctr.FPartsCount <> _shp.GetNumParts) then
          ctr.isChanged := True ;

      _wasfixed := ctr.isChanged ;

      if _returnnewobj then begin
        if _shp.ShapeType = TGIS_ShapeType.Polygon then
          Result :=  TGIS_ShapePolygon.Create( nil, nil, False, 0, _shp.Layer,
                                              _shp.Dimension )
        else
          Result :=  TGIS_ShapeArc.Create( nil, nil, False, 0, _shp.Layer,
                                              _shp.Dimension ) ;
        if GisLockGreaterThanEqual( _shp.LockLevel, TGIS_Lock.Projection ) then
          Result.Lock(_shp.LockLevel)
        else
          Result.Lock(TGIS_Lock.Extent) ;
        ctr.Write2Shape(Result) ;
        Result.Unlock ;
      end
      else begin
        Result := _shp ;
        if _shp.IsMapped then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_NOEDITABLE ), '', 0 )
        else begin

          if GisLockLessThan( _shp.LockLevel, TGIS_Lock.Projection ) then
            pshape1.Lock(TGIS_Lock.Extent) ;
          ctr.Write2Shape(pshape1) ;
          pshape1.Unlock ;
        end ;
      end ;

      pshape1 := sh1 ;
      pshape2 := sh2 ;

      FreeObject( ctr ) ;
    finally
      _shp.Unlock() ;
      if llevel <> TGIS_Lock.None then
        _shp.Lock(llevel) ;
    end ;
  end ;

  function TGIS_Topology.FixShapeEx(
    const _shp          : TGIS_Shape      ;
    const _returnnewobj : Boolean         ;
    const _forceFix     : Boolean         ;
    var   _wasfixed     : Boolean
  ) : TGIS_Shape ;
  var
    ff : Boolean ;
  begin
    ff := forceFix ;
    forceFix := _forceFix ;
    Result := FixShapeEx(_shp, _returnnewobj, _wasfixed) ;
    forceFix := ff ;
  end ;

  function TGIS_Topology.ClearShape(
    const _shp : TGIS_Shape
  ) : TGIS_Shape ;
  begin
    Result := ClearShape( _shp, False ) ;
  end ;

  function TGIS_Topology.ClearShape(
    const _shp          : TGIS_Shape ;
    const _returnnewobj : Boolean
  ) : TGIS_Shape ;
  var
    k,
    kk          : Integer    ;
    shppartsize,
    shppartsno  : Integer    ;
    sshp        : TGIS_Shape ;
    rshp        : TGIS_Shape ;
    ptg,
    prev_ptg    : TGIS_Point ;
    next_ptg    : TGIS_Point ;
    last_ptg    : TGIS_Point ;
    ladd_ptg    : TGIS_Point ;
    points      : Integer ;
    ldist       : Double ;
    firstptidx  : Integer ;
    bad_part    : Boolean ;
  const
    SIN45 = 0.70710678118654752440084436210485 ;
  begin
    if (_shp.ShapeType <> TGIS_ShapeType.Arc    ) and
       (_shp.ShapeType <> TGIS_ShapeType.Polygon)
    then
    begin
      Result := nil ;
      exit ;
    end ;

    sshp := _shp.CreateCopy ;

    if _returnnewobj then begin
      if _shp.ShapeType = TGIS_ShapeType.Polygon then
        Result :=  TGIS_ShapePolygon.Create( nil, nil, False, 0, _shp.Layer,
                                            _shp.Dimension )
      else
        Result :=  TGIS_ShapeArc.Create( nil, nil, False, 0, _shp.Layer,
                                        _shp.Dimension ) ;

      rshp := Result ;
    end
    else begin
      Result := _shp ;
      rshp := _shp ;
      _shp.Reset ;
    end ;

    ldist := FTolerance*SIN45 ;

    // deleting not needed points
    shppartsno := sshp.GetNumParts ;
    for k := 0 to shppartsno -1 do begin
      shppartsize := sshp.GetPartSize(k);
      points := 0 ;
      if shppartsize > 1 then begin
        prev_ptg := sshp.GetPoint(k, 0);
        next_ptg := sshp.GetPoint(k, 1);

        firstptidx := 1 ;

        if _shp.ShapeType = TGIS_ShapeType.Polygon then begin
          ladd_ptg := sshp.GetPoint(k, shppartsize -2);
          bad_part := False ;
          while True do begin
            if GisLine2Point(ladd_ptg, next_ptg, prev_ptg) > ldist then begin
              break ;
            end ;

            if firstptidx >= shppartsize -2 then begin
              bad_part := True ;
              break ;
            end ;
            inc( firstptidx ) ;
            prev_ptg := _TGIS_Point (next_ptg);
            next_ptg := sshp.GetPoint(k, firstptidx);
          end ;
          if bad_part then continue ;
        end ;

        last_ptg := sshp.GetPoint(k, shppartsize -1);
        for kk := firstptidx to  shppartsize -2 do begin
          ptg := _TGIS_Point (next_ptg);
          next_ptg := sshp.GetPoint(k, kk +1);
          if GisLine2Point(prev_ptg, next_ptg, ptg) > ldist then begin
            if (GisPoint2Point(prev_ptg, ptg) > FTolerance) and
               (GisPoint2Point(last_ptg, ptg) > FTolerance) then begin
              if points = 0 then begin
                rshp.AddPart ;
                rshp.AddPoint(prev_ptg);
                inc( points ) ;
              end ;
              prev_ptg := _TGIS_Point (ptg);
              rshp.AddPoint(prev_ptg);
            end ;
          end ;
        end ;
        if _shp.ShapeType = TGIS_ShapeType.Arc then
          rshp.AddPoint(last_ptg) ;
      end ;
    end ;

    FreeObject( sshp ) ;
    if rshp.GetNumPoints = 0 then begin
      if _returnnewobj then
        FreeObject(Result) ;
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TOPOLOGY_EMPTYAFTERFIX ), '', 0 ) ;
    end ;
  end ;

  function TGIS_Topology.FindAndDeleteLoops(
    const _shp : TGIS_Shape
  ) : TGIS_Shape ;
  begin
    Result := FindAndDeleteLoops( _shp, False ) ;
  end ;

  function TGIS_Topology.FindAndDeleteLoops(
    const _shp          : TGIS_Shape ;
    const _returnnewobj : Boolean
  ) : TGIS_Shape ;
  var
    ptarray : array of TGIS_Point ;
    i ,k,
    ptno      : Integer ;
    n, nsub   : Integer ;
    nextk     : Integer ;
    no_empty  : Boolean ;
    crosspt   : TGIS_Point ;
    line1,
    line2     : TGIS_Line ;
    sshp      : TGIS_Shape ;
    dshp      : TGIS_Shape ;

    {$IFNDEF OXYGENE}
      procedure decptarray(_mb, _me : Integer; _addcross : Boolean  = False) ;
    {$ELSE}
      procedure decptarray(_mb, _me : Integer; _addcross : Boolean := False) ;
    {$ENDIF}
    var
      ii, kk : Integer ;
    begin
      ii := _mb +1;
      if _addcross then begin
        ptarray[ii] := crosspt ;
        inc( ii ) ;
      end ;

      for kk := _me to ptno -1 do begin
        ptarray[ii] := ptarray[kk] ;
        inc( ii ) ;
      end ;

      ptno := ii;
    end ;
  begin

    Result := nil ;

    if (_shp.ShapeType <> TGIS_ShapeType.Arc    ) and
       (_shp.ShapeType <> TGIS_ShapeType.Polygon)
    then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGSHAPE ), '', 0 ) ;

    if _returnnewobj then begin
      if _shp.ShapeType = TGIS_ShapeType.Polygon then begin
        Result :=  TGIS_ShapePolygon.Create( nil, nil, False, 0, _shp.Layer,
                                            _shp.Dimension ) ;
        nsub := 2 ;
      end
      else begin
        Result :=  TGIS_ShapeArc.Create( nil, nil, False, 0, _shp.Layer,
                                        _shp.Dimension ) ;
        nsub := 1 ;
      end ;
      sshp := _shp ;
      dshp := Result ;
    end
    else begin
      Result := _shp ;
      sshp := _shp.CreateCopy ;
      _shp.Reset ;
      dshp := _shp ;

      if _shp.ShapeType = TGIS_ShapeType.Polygon then
        nsub := 2
      else
        nsub := 1 ;
    end ;

    no_empty := False ;

    for i := 0 to sshp.GetNumParts -1 do begin
      ptno := sshp.GetPartSize(i) ;

      SetLength(ptarray, ptno) ;

      for k := 0 to ptno -1 do
        ptarray[k] := sshp.GetPoint(i, k) ;

      k := 0 ;
      repeat
        nextk := k +2 ;

        while nextk <= ptno -nsub do begin
          if GisPoint2Point(ptarray[k], ptarray[nextk]) <= FTolerance then begin
            decptarray(k, nextk +1) ;
            nextk := k +1;
          end ;
          inc( nextk ) ;
        end ;
        inc( k ) ;
      until k > ptno -nsub ;

      k := 0 ;
      repeat
        nextk := k +2 ;
        line1.A := _TGIS_Point (ptarray[k]) ;
        line1.B := _TGIS_Point (ptarray[k +1]) ;

        while nextk <= ptno -nsub do begin
          line2.A := _TGIS_Point (ptarray[nextk]) ;
          line2.B := _TGIS_Point (ptarray[nextk +1]) ;
          if (line1.A.X = line2.B.X) and (line1.A.Y = line2.B.Y) then
            break ;
          if GisGetLinesCrossing(line1, line2, crosspt) then begin
            decptarray(k, nextk +1, True) ;
            nextk := k +1;
            line1.B := _TGIS_Point (crosspt) ;
          end ;
          inc( nextk ) ;
        end ;
        inc( k ) ;
      until k > ptno -nsub;

      if ((_shp.ShapeType = TGIS_ShapeType.Polygon) and
          (ptno -nsub > 1))
         or
         ((_shp.ShapeType = TGIS_ShapeType.Arc) and
          (ptno -nsub > 0))
      then begin
        dshp.AddPart ;
        for n := 0 to ptno -nsub do
          dshp.AddPoint(ptarray[n]) ;
        no_empty := True ;
      end ;
    end ;
    ptarray := nil ;

    if not _returnnewobj then
      FreeObject(sshp) ;

    if not no_empty then begin
      if _returnnewobj then
        FreeObject(Result) ;
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TOPOLOGY_EMPTYAFTERFIX ), '', 0 ) ;
    end ;

  end ;

  function TGIS_Topology.Combine(
    const _shpA      : TGIS_Shape        ;
    const _shpB      : TGIS_Shape        ;
    const _operation : TGIS_TopologyCombineType
  ) : TGIS_Shape ;
  begin
    Result := Combine( _shpA, _shpB, _operation, False ) ;
  end ;

  function TGIS_Topology.Combine(
    const _shpA      : TGIS_Shape        ;
    const _shpB      : TGIS_Shape        ;
    const _operation : TGIS_TopologyCombineType ;
    const _fixshape  : Boolean
  ) : TGIS_Shape ;
  var
    llevel_a    : TGIS_Lock ;
    llevel_b    : TGIS_Lock ;

    procedure unlock_shapes ;
    begin
      if assigned( _shpA.Layer ) then begin
        _shpA.Unlock ;
        if llevel_a <> TGIS_Lock.None then
          _shpA.Lock(llevel_a) ;
      end ;
      if assigned( _shpB.Layer ) then begin
        _shpB.Unlock ;
        if llevel_b <> TGIS_Lock.None then
          _shpB.Lock(llevel_b) ;
      end ;
      lockedProjection := False ;
    end ;
  begin
    Result := nil ;
    if _shpA = nil then begin
      if _shpB <> nil then begin
        if ( _operation = TGIS_TopologyCombineType.Union ) or
           ( _operation = TGIS_TopologyCombineType.SymmetricalDifference )
        then
          Result := TGIS_ShapePolygon(_shpB.CreateCopy) ;
      end ;
      exit ;
    end ;
    if _shpB = nil then begin
      if _operation <> TGIS_TopologyCombineType.Intersection then
        Result := TGIS_ShapePolygon(_shpA.CreateCopy) ;
      exit ;
    end ;

    if assigned( _shpA.Layer ) and assigned( _shpB.Layer ) then begin
      if ( _shpA.Layer = _shpB.Layer ) or
         ( _shpA.Layer.CS.EPSG = _shpB.Layer.CS.EPSG ) then
      begin
        lockedProjection := True ;
        llevel_a := _shpA.LockLevel ;
        llevel_b := _shpB.LockLevel ;

        _shpA.Lock( TGIS_Lock.Projection );
        _shpB.Lock( TGIS_Lock.Projection );
      end ;
    end ;

    case _shpA.ShapeType of
      TGIS_ShapeType.Polygon :
        begin
          if _fixshape then
            FixShape(_shpA) ;

          case _shpB.ShapeType of
            TGIS_ShapeType.Polygon :
              begin
                if _fixshape then
                  FixShape(_shpB) ;
                Result := combinePolygons( TGIS_ShapePolygon( _shpA ),
                                           TGIS_ShapePolygon( _shpB ),
                                           _operation
                                         ) ;
              end ;
            TGIS_ShapeType.Arc :
              if _operation = TGIS_TopologyCombineType.Intersection then
                Result := combineArcPolygon( TGIS_ShapeArc    ( _shpB ),
                                             TGIS_ShapePolygon( _shpA ),
                                             _operation
                                           ) ;
            TGIS_ShapeType.MultiPoint :
              Result := combinePointPolygon( TGIS_ShapePoint  ( _shpB ),
                                             TGIS_ShapePolygon( _shpA ),
                                            _operation
                                           ) ;

            TGIS_ShapeType.Point :
              Result := combinePointPolygon( TGIS_ShapePoint  ( _shpB ),
                                             TGIS_ShapePolygon( _shpA ),
                                             _operation
                                           ) ;

            else begin
              if lockedProjection then
                unlock_shapes ;
              exit ;
            end ;
          end ;
        end ;
      TGIS_ShapeType.Arc :
        case _shpB.ShapeType of
          TGIS_ShapeType.Polygon :
            begin
              if _fixshape then
                FixShape( _shpB ) ;
              Result := combineArcPolygon( TGIS_ShapeArc    ( _shpA ),
                                           TGIS_ShapePolygon( _shpB ),
                                           _operation
                                         ) ;
            end ;
          TGIS_ShapeType.Arc :
            Result := combineArcs( TGIS_ShapeArc( _shpA ),
                                    TGIS_ShapeArc( _shpB ),
                                    _operation
                                  ) ;
          TGIS_ShapeType.MultiPoint :
            Result := combinePointArc( TGIS_ShapePoint( _shpB ),
                                       TGIS_ShapeArc  ( _shpA ),
                                       _operation
                                     ) ;

          TGIS_ShapeType.Point :
            Result := combinePointArc( TGIS_ShapePoint( _shpB) ,
                                       TGIS_ShapeArc  ( _shpA ),
                                       _operation
                                     ) ;
          else begin
            if lockedProjection then
              unlock_shapes ;
            exit ;
          end ;
        end ;
      TGIS_ShapeType.MultiPoint :
        case _shpB.ShapeType of
          TGIS_ShapeType.Polygon :
            begin
              if _fixshape then
                FixShape(_shpB) ;
              Result := combinePointPolygon( TGIS_ShapePoint  ( _shpA ),
                                             TGIS_ShapePolygon( _shpB ),
                                             _operation
                                           ) ;
            end ;
          TGIS_ShapeType.Arc :
            Result := combinePointArc( TGIS_ShapePoint( _shpA ),
                                       TGIS_ShapeArc  ( _shpB ),
                                       _operation
                                     ) ;

          TGIS_ShapeType.MultiPoint :
            Result := combineMultiPoints( TGIS_ShapeMultiPoint( _shpA ),
                                          TGIS_ShapeMultiPoint( _shpB ),
                                          _operation
                                        ) ;

          TGIS_ShapeType.Point :
            if _operation = TGIS_TopologyCombineType.Difference then
              Result := differenceMPointPoint( TGIS_ShapeMultiPoint( _shpA ),
                                               TGIS_ShapePoint     ( _shpB )
                                             )
            else
              Result := combinePointMPoint( TGIS_ShapePoint     ( _shpB ),
                                            TGIS_ShapeMultiPoint( _shpA ),
                                            _operation
                                          ) ;

          else begin
            if lockedProjection then
              unlock_shapes ;
            exit ;
          end ;
        end ;
      TGIS_ShapeType.Point :
        case _shpB.ShapeType of
          TGIS_ShapeType.Polygon :
            begin
              if _fixshape then
                FixShape(_shpB) ;
              Result := combinePointPolygon( TGIS_ShapePoint  ( _shpA ),
                                             TGIS_ShapePolygon( _shpB ),
                                             _operation
                                           ) ;
            end ;
          TGIS_ShapeType.Arc :
            Result := combinePointArc( TGIS_ShapePoint( _shpA ),
                                       TGIS_ShapeArc  ( _shpB ),
                                       _operation
                                     ) ;

          TGIS_ShapeType.MultiPoint :
            Result := combinePointMPoint( TGIS_ShapePoint     ( _shpA ),
                                          TGIS_ShapeMultiPoint( _shpB ),
                                          _operation
                                        ) ;

          TGIS_ShapeType.Point :
            Result := combinePoints( TGIS_ShapePoint( _shpA ),
                                     TGIS_ShapePoint( _shpB ),
                                     _operation
                                   ) ;

          else begin
            if lockedProjection then
              unlock_shapes ;
            exit ;
          end ;

        end ;
      else begin
        if lockedProjection then
          unlock_shapes ;
        exit ;
      end ;

    end ;
    if lockedProjection then
      unlock_shapes ;

  end ;

  function  TGIS_Topology.Union(
    const _shpA : TGIS_Shape ;
    const _shpB : TGIS_Shape
  ) : TGIS_Shape ;
  begin
    Result := Union( _shpA, _shpB, False ) ;
  end ;

  function  TGIS_Topology.Union(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape ;
    const _fixshape : Boolean
  ) : TGIS_Shape ;
  begin
    Result := Combine( _shpA, _shpB, TGIS_TopologyCombineType.Union, _fixshape ) ;
  end ;


  function  TGIS_Topology.UnionOnList(
    const _shpList  : TGIS_ShapeList
  ) : TGIS_Shape ;
  begin
    Result := UnionOnList( _shpList, False ) ;
  end ;

  function  TGIS_Topology.UnionOnList(
    const _shpList  : TGIS_ShapeList ;
    const _fixshape : Boolean
  ) : TGIS_Shape ;
  var
    tree_with2, c, i  : Integer ;
    cntr1             : T_Contour ;
    cntr2             : T_Contour ;
    ucntr             : T_Contour ;
    lcntr             : T_Contour ;
    shp1, shp2        : TGIS_Shape ;
    lshp              : TGIS_Shape ;
    cntrList, cl      : TList<TObject> ;
    cntr2List         : TList<TObject> ;
    rl                : TList<TObject> ;
    cshp              : TGIS_Shape ;
    lbu               : Boolean ;
    actual_level      : Integer ;
    localprogress     : Integer ;
    localprogressin   : Integer ;
    localfullprogress : Integer ;
    lastprogress      : Integer ;
    localbusystep     : Integer ;
    shpList           : TList<TGIS_Shape>  ;
    {$IFDEF OXYGENE}
      bargs           : TGIS_BusyEventArgs ;
    {$ENDIF}

    procedure find_big ;
    var
      tmax : Double ;
      wmax : Double ;
      shp : TGIS_Shape ;
      m : Integer ;
    begin
      pshape1 := TGIS_Shape( _shpList[0] ) ;
      tmax := Max((pshape1.Extent.XMax -pshape1.Extent.XMin),
                 (pshape1.Extent.YMax -pshape1.Extent.YMin)) ;
      for m := 1 to _shpList.Count -1 do begin

        shp  := TGIS_Shape( _shpList[m] ) ;
        wmax := Max((shp.Extent.XMax -shp.Extent.XMin),
                    (shp.Extent.YMax -shp.Extent.YMin)) ;
        if wmax > tmax then begin
          pshape1 := shp ;
          tmax := wmax ;
        end ;
      end ;
    end ;

    procedure free_list(_l : TList<TObject>) ;
    var
      i1 : Integer ;
      count2_2 : Integer ;
    begin
      count2_2 := _l.Count -(_l.Count and 1) ;
      for i1 := count2_2 -1 downto 0 do begin
        {$IFNDEF NEXTGEN}
          FreeObjectNotNil(_l[i1]) ;
        {$ENDIF}
      end ;
      _l.Clear ;
    end ;

    procedure busy_lists_free ; //for break
    var
      s   : Integer ;
    begin
      if assigned( cntrList ) then begin
        for s := cntrList.Count -1 downto 0 do begin
          {$IFNDEF NEXTGEN}
            FreeObjectNotNil(cntrList[s] ) ;
          {$ENDIF}
        end ;
        FreeObject(cntrList) ;
      end ;
      if assigned( cntr2List ) then begin
        for s := cntr2List.Count -1 downto 0 do begin
          {$IFNDEF NEXTGEN}
            FreeObjectNotNil( cntr2List[s] ) ;
          {$ENDIF}
        end ;
        FreeObject(cntr2List) ;
      end ;

      if assigned( rl ) then begin
        for s := rl.Count -1 downto 0 do begin
          {$IFNDEF NEXTGEN}
            FreeObjectNotNil(rl[s]) ;
          {$ENDIF}
        end ;
        FreeObject(rl) ;
      end ;
      busyUsed := False ;
      // doBreak will be ignored
      {$IFDEF OXYGENE}
        FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, doBreak ) ) ;
      {$ELSE}
        FOnBusy( self, -1, -1, doBreak ) ;
      {$ENDIF}
    end ;

  begin
    Result := nil ;

    if _shpList.Count > 0 then begin

      lbu := False ;
      actual_level := TruncS(Log2(_shpList.Count)) ;
      localprogress := 0 ;
      localprogressin := inProgress ;
      localbusystep := busyStep ;
      if busyStep = 0 then begin
        fullProgress :=  _shpList.Count ;
        localfullprogress := fullProgress ;
        inProgress   := 0 ;
      end
      else begin
        busyStep := 0 ;
        localfullprogress := localbusystep ;
      end ;

      if assigned( FOnBusy ) then begin
        lbu := True ;
        busyStep := 0 ;
        // doBreak will be ignored
        {$IFDEF OXYGENE}
          FOnBusy( self, TGIS_BusyEventArgs.Create( inProgress,
                                                    fullProgress,
                                                    doBreak
                                                  ) ) ;
        {$ELSE}
          FOnBusy( self, inProgress, fullProgress, doBreak ) ;
        {$ENDIF}
        doBreak := False ;
        cntrList := nil ;
        cntr2List := nil ;
        rl := nil ;
      end ;

      if TGIS_Shape(_shpList[0]).ShapeType = TGIS_ShapeType.Polygon then
      begin


        shpList := TList<TGIS_Shape>.Create ;
        for i := 0 to _shpList.Count - 1 do begin
          if TGIS_ShapePolygon(_shpList[i]).Area <> 0 then
            shpList.Add(TGIS_Shape(_shpList[i]))
          else
            continue ;
        end ;
        if shpList.Count = 0 then begin
          FreeObject( shpList ) ;
          exit ;
        end ;

      {$IFDEF OXYGENE}
        shpList.Sort( @ExtentsCompare )  ;
      {$ELSE}
        shpList.Sort( TComparer<TGIS_Shape>.Construct( ExtentsCompare ) ) ;
      {$ENDIF}

        busyUsed := lbu ;
        pshape2 := nil ;
        find_big ;
        computeTolerance ;

        cntrList := TList<TObject>.Create ;
        if shpList.Count >= 8 then
          cntr2List := TList<TObject>.Create
        else
          cntr2List := nil ;

        tree_with2 := shpList.Count div 2 ;

        for i := 0 to tree_with2 -1 do begin
          {$IFDEF MANAGED}
            cntr1 := T_Contour.Create( CONTOUR_CLOSED, TGIS_Shape(shpList[2*i]), self, False, _fixshape);
            cntr2 := T_Contour.Create( CONTOUR_CLOSED, TGIS_Shape(shpList[2*i +1]), self, False, _fixshape);
          {$ELSE}
            cntr1 := T_Contour.Create( CONTOUR_CLOSED, TGIS_Shape(shpList.Items[2*i]), self, False, _fixshape);
            cntr2 := T_Contour.Create( CONTOUR_CLOSED, TGIS_Shape(shpList.Items[2*i +1]), self, False, _fixshape);
          {$ENDIF}

          ucntr := T_Contour( combineCPolygons( cntr1 ,
                                                cntr2 ,
                                                TGIS_TopologyCombineType.Union
                                               )) ;
          FreeObject(cntr2) ;
          FreeObject(cntr1) ;

          if busyUsed then begin
            if doBreak then begin
              busy_lists_free ;
              exit ;
            end ;
            inc( localprogress ) ;
            inProgress := localprogressin +
                          localprogress div actual_level ;
          end ;

          if ucntr <> nil then begin
            cntrList.Add(ucntr) ;
            if assigned( cntr2List ) then begin
              if cntrList.Count = 2 then begin
                cntr1 := T_Contour(cntrList[0]) ;
                cntr2 := T_Contour(cntrList[1]) ;
                ucntr := T_Contour( combineCPolygons( cntr1 ,
                                                      cntr2 ,
                                                      TGIS_TopologyCombineType.Union
                                                     )) ;
                FreeObject(cntr2) ;
                FreeObject(cntr1) ;
                cntrList.Clear ;

                if busyUsed then begin
                  if doBreak then begin
                    busy_lists_free ;
                    exit ;
                  end ;
                  inc( localprogress ) ;
                  inProgress := localprogressin +
                                localprogress div actual_level ;
                end ;

                if ucntr <> nil then
                  cntr2List.Add(ucntr) ;
              end ;
            end ;
          end ;
        end ;
        if assigned( cntr2List ) then begin
          if cntr2List.Count > 0 then begin
            if cntrList.Count = 1 then
              cntr2List.Add(cntrList[0]) ;
            cntrList.Clear ;
            FreeObject( cntrList ) ;
            cntrList := cntr2List ;
            cntr2List := nil ;
          end ;
        end ;

        if (shpList.Count and 1) = 1 then begin
          cntr1 := T_Contour.Create( CONTOUR_CLOSED, TGIS_Shape(shpList[shpList.Count -1]), self, False, _fixshape);
          if cntr1 <> nil then
            cntrList.Add(cntr1) ;
        end ;

        rl := TList<TObject>.Create ;
        while True do begin
          if (cntrList.Count and 1) = 1 then begin
            lcntr := T_Contour( cntrList[cntrList.Count -1] ) ;
          end
          else
            lcntr := nil ;

          tree_with2 := cntrList.Count div 2 ;
          c := 0 ;
          if cntrList.Count > 0 then
            actual_level := TruncS(Log2(cntrList.Count)) ;
          lastprogress := inProgress ;

          for i := 0 to tree_with2 -1 do begin
            cntr1 := T_Contour(cntrList[c]) ;
            cntrList[c] := nil ;
            inc( c ) ;
            cntr2 := T_Contour(cntrList[c]) ;
            cntrList[c] := nil ;
            inc( c ) ;

            ucntr := T_Contour( combineCPolygons( cntr1 ,
                                                  cntr2 ,
                                                  TGIS_TopologyCombineType.Union
                                                )) ;
            FreeObject(cntr2) ;
            FreeObject(cntr1) ;
            if ucntr <> nil then
              rl.Add(ucntr) ;

            if busyUsed then begin
              if doBreak then begin
                busy_lists_free ;
                exit ;
              end ;
              inc( localprogress ) ;
              inProgress := localprogressin +
                            localprogress div actual_level +(i div 2);
              if inProgress < lastprogress then
                inProgress := lastprogress ;
              if (tree_with2 = 2) and (i = 1) then
                busyStep := localprogressin +localfullprogress - inProgress ;
            end ;

          end ;

          if lcntr <> nil then
            rl.Add(lcntr) ;

          cntrList.Clear ;
          if rl.Count <= 1 then
            break ;

          cl := cntrList ;
          cntrList := rl ;
          rl := cl ;
        end ;

        FreeObject( cntrList ) ;

        if busyUsed then begin
          if localbusystep = 0 then begin
            // doBreak will be ignored
            {$IFDEF OXYGENE}
              FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, doBreak ) ) ;
              busyUsed := False ;
            {$ELSE}
              FOnBusy( self, -1, -1, doBreak ) ;
              busyUsed := False ;
            {$ENDIF}
          end
          else begin
            inProgress := localprogressin +localbusystep ;
          end ;
        end ;

        if (rl.Count = 1) then begin
          Result := TGIS_Shape(shpList[0]).CreateCopy ;
            cntr1 := T_Contour( rl[0] ) ;
          if GisLockGreaterThanEqual( TGIS_Shape(shpList[0]).LockLevel, TGIS_Lock.Projection ) then
            Result.Lock(TGIS_Shape(shpList[0]).LockLevel)
          else
            Result.Lock(TGIS_Lock.Extent) ;

          cntr1.setHolesEx(self) ;
          cntr1.Write2Shape(Result) ;
          Result.Unlock ;

          Result.IsFixed := True ;
          FreeObject( cntr1 ) ;
        end ;
        FreeObject( rl      ) ;
        FreeObject( shpList ) ;
      end
      else begin
        cntrList := TList<TObject>.Create ;
        busyUsed := False ;

        for i := 0 to _shpList.Count -1 do begin
          cshp := TGIS_Shape(_shpList[i]).CreateCopy ;
          if cshp <> nil then
            cntrList.Add(cshp) ;
        end ;

        rl := TList<TObject>.Create ;

        while True do begin
          if (cntrList.Count and 1) = 1 then begin
            lshp := TGIS_Shape( cntrList[cntrList.Count -1] ) ;
          end
          else
            lshp := nil ;

          tree_with2 := cntrList.Count div 2 ;
          c := 0 ;

          for i := 0 to tree_with2 -1 do begin
            shp1 := TGIS_Shape(cntrList[c]) ;
            inc( c ) ;
            shp2 := TGIS_Shape(cntrList[c]) ;
            inc( c ) ;

            shp1 := Combine( shp1 , shp2 , TGIS_TopologyCombineType.Union) ;

            if shp1 <> nil then
              rl.Add(shp1) ;
            if lbu then begin
              {$IFDEF OXYGENE}
                bargs := TGIS_BusyEventArgs.Create( inProgress,
                                                    fullProgress,
                                                    doBreak
                                                  ) ;
                try
                  FOnBusy( self, bargs ) ;
                finally
                  doBreak := bargs.Abort ;
                  FreeObject( bargs ) ;
                end ;
              {$ELSE}
                FOnBusy( self, inProgress, fullProgress, doBreak ) ;
              {$ENDIF}
              if doBreak then begin
                busy_lists_free ;
                exit ;
              end ;
              inc( inProgress ) ;
            end ;
          end ;

          if lshp <> nil then
            rl.Add(lshp) ;

          free_list(cntrList) ;
          if rl.Count <= 1 then
            break ;

          cl := cntrList ;
          cntrList := rl ;
          rl := cl ;
        end ;

        FreeObject( cntrList ) ;
        if lbu then
          {$IFDEF OXYGENE}
            FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, doBreak ) ) ;
          {$ELSE}
            FOnBusy( self, -1, -1, doBreak ) ;
          {$ENDIF}

        if rl.Count = 1 then
          Result := TGIS_Shape(rl[0]) ;
        FreeObject( rl ) ;
      end ;
    end ;
  end ;

  function  TGIS_Topology.Intersection(
    const _shpA      : TGIS_Shape ;
    const _shpB      : TGIS_Shape
  ) : TGIS_Shape ;
  begin
    Result := Intersection( _shpA, _shpB, False ) ;
  end ;

  function  TGIS_Topology.Intersection(
    const _shpA      : TGIS_Shape ;
    const _shpB      : TGIS_Shape ;
    const _fixshape  : Boolean
  ) : TGIS_Shape ;
  begin
    Result := Combine( _shpA, _shpB,
                       TGIS_TopologyCombineType.Intersection, _fixshape ) ;
  end ;

  function  TGIS_Topology.Difference(
    const _shpA      : TGIS_Shape ;
    const _shpB      : TGIS_Shape
  ) : TGIS_Shape ;
  begin
    Result := Difference( _shpA, _shpB, False ) ;
  end ;

  function  TGIS_Topology.Difference(
    const _shpA      : TGIS_Shape ;
    const _shpB      : TGIS_Shape ;
    const _fixshape  : Boolean
  ) : TGIS_Shape ;
  begin
    Result := Combine( _shpA, _shpB,
                       TGIS_TopologyCombineType.Difference, _fixshape ) ;
  end ;

  function  TGIS_Topology.SymmetricalDifference(
    const _shpA : TGIS_Shape ;
    const _shpB : TGIS_Shape
  ) : TGIS_Shape ;
  begin
    Result := SymmetricalDifference( _shpA, _shpB, False ) ;
  end ;

  function  TGIS_Topology.SymmetricalDifference(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape ;
    const _fixshape : Boolean
  ) : TGIS_Shape ;
  begin
    Result := Combine(
      _shpA, _shpB,
      TGIS_TopologyCombineType.SymmetricalDifference, _fixshape
    ) ;
  end ;

  function TGIS_Topology.ConvexHull(
    const _layer : TGIS_LayerVector
  ) : TGIS_ShapePolygon ;
  var
    hb : T_hullBuilder ;
  begin
    hb := T_hullBuilder.Create ;
    try
      hb.GIS_Topology := Self ;
      Result := hb.ConvexHull( _layer ) ;
    finally
      FreeObject( hb ) ;
    end ;
  end ;

  function TGIS_Topology.ConvexHull(
    const _shape : TGIS_Shape
  ) : TGIS_ShapePolygon ;
  var
    hb : T_hullBuilder ;
  begin
    hb := T_hullBuilder.Create ;
    try
      hb.GIS_Topology := Self ;
      Result := hb.ConvexHull( _shape ) ;
    finally
      FreeObject( hb ) ;
    end ;
  end ;

  function TGIS_Topology.ConvexHull(
    const _shape1 : TGIS_Shape ;
    const _shape2 : TGIS_Shape
  ) : TGIS_ShapePolygon ;
  var
    hb : T_hullBuilder ;
  begin
    hb := T_hullBuilder.Create ;
    try
      hb.GIS_Topology := Self ;
      Result := hb.ConvexHull( _shape1, _shape2 ) ;
    finally
      FreeObject( hb ) ;
    end ;
  end ;

  function TGIS_Topology.ConvexHull(
    const _shape1 : TGIS_Shape ;
    const _shape2 : TGIS_Shape ;
    const _fix    : Boolean
  ) : TGIS_ShapePolygon ;
  var
    hb : T_hullBuilder ;
  begin
    hb := T_hullBuilder.Create ;
    try
      hb.GIS_Topology := Self ;
      Result := hb.ConvexHull( _shape1, _shape2, _fix ) ;
    finally
      FreeObject( hb ) ;
    end ;
  end ;

  function TGIS_Topology.ConcaveHull(
    const _layer : TGIS_LayerVector ;
    const _alpha : Double
  ) : TGIS_ShapePolygon ;
  var
    hb : T_hullBuilder ;
  begin
    hb := T_hullBuilder.Create ;
    try
      hb.GIS_Topology := Self ;
      Result := hb.ConcaveHull( _layer, _alpha ) ;
    finally
      FreeObject( hb ) ;
    end ;
  end ;

  function TGIS_Topology.ConcaveHull(
    const _shape  : TGIS_Shape ;
    const _alpha  : Double
  ) : TGIS_ShapePolygon ;
  var
    hb : T_hullBuilder ;
  begin
    hb := T_hullBuilder.Create ;
    try
      hb.GIS_Topology := Self ;
      Result := hb.ConcaveHull( _shape, _alpha ) ;
    finally
      FreeObject( hb ) ;
    end ;
  end ;

  function TGIS_Topology.ConcaveHull(
    const _shape1 : TGIS_Shape ;
    const _shape2 : TGIS_Shape ;
    const _alpha  : Double
  ) : TGIS_ShapePolygon ;
  var
    hb : T_hullBuilder ;
  begin
    hb := T_hullBuilder.Create ;
    try
      hb.GIS_Topology := Self ;
      Result := hb.ConcaveHull( _shape1, _shape2, _alpha ) ;
    finally
      FreeObject( hb ) ;
    end ;
  end ;

  function TGIS_Topology.ConcaveHull(
    const _shape1 : TGIS_Shape ;
    const _shape2 : TGIS_Shape ;
    const _alpha  : Double     ;
    const _fix    : Boolean
  ) : TGIS_ShapePolygon ;
    var
    hb : T_hullBuilder ;
  begin
    hb := T_hullBuilder.Create ;
    try
      hb.GIS_Topology := Self ;
      Result := hb.ConcaveHull( _shape1, _shape2, _alpha, _fix ) ;
    finally
      FreeObject( hb ) ;
    end ;
  end ;


  function TGIS_Topology.FindSameOnList(
    const _list : {$IFDEF CLR}
                     IList           ;
                   {$ELSE}
                     TGIS_ObjectList ;
                   {$ENDIF}
    const _shp  : TGIS_Shape
  ) : Integer ;
  var
    localShpList : TList<TGIS_Shape>       ;
    sptg         : TGIS_Point  ;
    ptg          : TGIS_Point  ;
    k, kk , c    : Integer     ;
    shppartsize,
    spartsize    : Integer     ;
    shppartsno,
    spartsno     : Integer     ;
    sextent      : TGIS_Extent ;
    rshape,
    shape        : TGIS_Shape  ;
  begin

    if GisLockGreaterThanEqual( _shp.LockLevel, TGIS_Lock.Projection ) then
      sextent := _shp.Extent
    else
      sextent := _shp.ProjectedExtent ;

    localShpList := TList<TGIS_Shape>.Create;
    for c := 0 to _list.Count -1 do begin
      shape := TGIS_Shape(_list[c]);
      if shape = nil then continue ;
      if shape.IsDeleted then continue ;
      if (shape.Uid <> _shp.Uid) or (shape.Layer <> _shp.Layer)  then begin
        if shape.IsInsideExtent(sextent, TGIS_InsideType.Partial) then
          localShpList.Add(shape) ;
      end ;
    end ;

    spartsno := _shp.GetNumParts ;
    Result := -1 ;

    for c := 0 to localShpList.Count -1 do begin
      shape := TGIS_Shape(localShpList[c]);
      shppartsno := shape.GetNumParts ;
      Result := c ;
      if shppartsno <> spartsno then begin
        Result := -1 ;
        continue ;
      end ;
      for k := 0 to shppartsno -1 do begin

        shppartsize := shape.GetPartSize(k) ;
        spartsize := _shp.GetPartSize(k) ;
        if shppartsize <> spartsize then begin
          Result := -1 ;
          break ;
        end ;
        for kk := 0 to shppartsize -1 do begin
          ptg:= shape.GetPoint(k, kk);
          sptg:= _shp.GetPoint(k, kk);
          if (ptg.X <> sptg.X) or (ptg.Y <> sptg.Y) then begin
            Result := -1 ;
            break ;
          end ;
        end ;
        if Result = -1  then
          break ;
      end ;
      if Result >= 0 then
        break ;
    end ;
    if Result >= 0 then begin
      rshape := TGIS_Shape(localShpList[Result]);
      for k := 0 to _list.Count -1 do begin
        shape := TGIS_Shape(_list[k]);
        if (shape.Layer = rshape.Layer) and (rshape.Uid = shape.Uid) then begin
          Result := k ;
          break ;
        end ;
      end ;
    end ;

    FreeObject( localShpList ) ;
  end ;

  function TGIS_Topology.MakeBuffer(
    const _shp          : TGIS_Shape        ;
    const _dist         : Double
  ) : TGIS_Shape ;
  begin
    Result := MakeBuffer( _shp, _dist, 9, False ) ;
  end ;

  function  TGIS_Topology.MakeOffsetLine(
    const _shp          : TGIS_Shape        ;
    const _dist         : Double
  ) : TGIS_Shape ;
  begin
    Result := MakeOffsetLine(_shp, _dist, TGIS_JoinType.Bevel) ;
  end;

  function TGIS_Topology.MakeOffsetLine(
    const _shp          : TGIS_Shape        ;
    const _dist         : Double            ;
    const _jointype     : TGIS_JoinType
  ) : TGIS_Shape ;
  var
    part_no,
    point_no         : Integer    ;
    num_parts        : Integer    ;
    max_points_idx   : Integer    ;
    max_idx          : Integer    ;
    unit_idx         : Integer    ;
    max_unit_idx     : Integer    ;
    points_in_unit   : Integer    ;
    start_idx        : Integer    ;
    dist             : Double     ;
    ushp,
    polygon          : TGIS_ShapePolygon   ;
    offshp           : TGIS_Shape   ;
    ptg, ptg_next    : TGIS_Point ;
    started          : Boolean    ;
    pipoints         : Integer    ;
    pi2_points       : Integer    ;
    jointype         : TGIS_JoinType  ;

    sin_angle,
    cos_angle        : Double     ;
    step_angle       : Double     ;
    angle_pi2        : Double     ;
    distsin, distcos : Double     ;
    slist            : TGIS_ShapeList      ;
    ulist            : TGIS_ShapeList      ;
    udlist           : TGIS_ShapeList      ;
    shape1           : TGIS_ShapePolygon ;
    shape2           : TGIS_ShapePolygon ;
    ushape           : TGIS_ShapePolygon ;

    shp_buf          : array of TGIS_Point ;
    full_circle      : array of TGIS_Point ;
    in_units         : Boolean ;
    max_units        : Integer ;
    max_shapes_idx   : Integer ;
    idx              : Integer ;
    shapes_in_unit   : Integer ;
    only_circle      : Boolean ;
    line_on_left     : Boolean ;

    first_offpoint,
    last_offpoint       : array of TGIS_Point ;

    allpoints        : Int64 ;

    ioshp            : TGIS_Shape ;
    poly_done        : Boolean ;
  const
    MAX_POINTS_IN_UNIT = 1000 ;
    MAX_JOIN_UNITS = 5 ;

    function set_sin_cosin(const _dx : Double; const _dy : Double) : Boolean ;
    begin
      if _dx = 0 then begin
        if _dy > 0 then
          step_angle := Pi / 2
        else
        if _dy < 0 then
          step_angle := 1.5 * Pi
        else
        begin
          Result := False ;
          exit ;
        end ;
      end
      else
       step_angle := Pi / 2 -ArcTan2(_dx, _dy) ;

      if step_angle < 0 then
        step_angle := step_angle +2 * Pi ;

      SinCos( step_angle, sin_angle, cos_angle ) ;
      Result := True ;
    end;


    function pt_is_left(const _pt : TGIS_Point) : Boolean ;
    begin
      if ((ptg_next.X -ptg.X)*(_pt.Y -ptg.Y) -
          (ptg_next.Y -ptg.Y)*(_pt.X -ptg.X) ) < 0 then
        Result := False
      else
        Result := True ;
    end;

    procedure make_basic_circle ;
    var
      p00 : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
      i   : Integer    ;
    begin
      p00.X := 0 ;
      p00.Y := -dist ;

      full_circle[0] := _TGIS_Point (p00);
      angle_pi2 := Pi / 2 ;

      for i := 1 to pi2_points do begin
        step_angle := (angle_pi2 * i)/pi2_points ;
        SinCos( step_angle, sin_angle, cos_angle ) ;

        full_circle[i].X := p00.X*cos_angle -p00.Y*sin_angle ;
        full_circle[i].Y := p00.X*sin_angle +p00.Y*cos_angle ;
      end ;

      for i := 1  to pi2_points do begin
        full_circle[pi2_points +i].X :=  full_circle[pi2_points -i].X ;
        full_circle[pi2_points +i].Y := -full_circle[pi2_points -i].Y;
      end ;

      for i := 1 to pipoints -1 do begin
        full_circle[pipoints +i].X := -full_circle[pipoints -i].X  ;
        full_circle[pipoints +i].Y :=  full_circle[pipoints -i].Y  ;
      end ;

    end ;

    procedure make_circle(_shp : TGIS_ShapePolygon) ;
    var
      pp : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
      k  : Integer    ;
    begin

      for k := 2*pipoints -1 downto 0 do
      begin
        pp.X := full_circle[k].X +ptg.X ;
        pp.Y := full_circle[k].Y +ptg.Y ;
        _shp.AddPoint(pp) ;
      end ;
      _shp.IsFixed := True ;
    end ;


    function make_rectangle : Boolean ;
    var
      k  : Integer ;
      dx,
      dy : Double  ;
    begin

      dx := ptg_next.X -ptg.X;
      dy := ptg_next.Y -ptg.Y;
      Result := set_sin_cosin(dx, dy) ;
      if not Result then
        exit ;

      if dx = 0 then begin
        if dy > 0 then
          step_angle := Pi / 2
        else
        if dy < 0 then
          step_angle := 1.5 * Pi
        else
        begin
          Result := False ;
          exit ;
        end ;
      end
      else
       step_angle := Pi / 2 -ArcTan2(dx, dy) ;

      if step_angle < 0 then
        step_angle := step_angle +2 * Pi ;

      SinCos( step_angle, sin_angle, cos_angle ) ;

      distsin := dist*sin_angle ;
      distcos := dist*cos_angle ;

      shp_buf[0].X := ptg.X -distsin ;
      shp_buf[0].Y := ptg.Y +distcos ;

      shp_buf[1].X :=  ptg_next.X -distsin ;
      shp_buf[1].Y :=  ptg_next.Y +distcos ;

      shp_buf[2].X := ptg_next.X +distsin;
      shp_buf[2].Y := ptg_next.Y -distcos;

      shp_buf[3].X := ptg.X +distsin;
      shp_buf[3].Y := ptg.Y -distcos;

      for k := 0 to 3 do begin
        polygon.AddPoint( shp_buf[k] ) ;
      end ;

      if point_no = max_points_idx then begin
        if line_on_left then
          last_offpoint[part_no] := shp_buf[1]
        else
          last_offpoint[part_no] := shp_buf[2] ;
      end;


      polygon.IsFixed := True ;
      Result := True ;

    end ;

    function make_bevel : Boolean ;
    var
      k, i   : Integer ;
      dx, dy : Double  ;
      ptgn  : TGIS_Point ;
      distsina, distcosa : Double     ;
    begin

      dx := ptg_next.X -ptg.X;
      dy := ptg_next.Y -ptg.Y;

      Result := set_sin_cosin(dx, dy) ;
      if not Result then
        exit ;

      distsin := dist*sin_angle ;
      distcos := dist*cos_angle ;

      shp_buf[0].X := ptg.X -distsin ;
      shp_buf[0].Y := ptg.Y +distcos ;

      shp_buf[1].X :=  ptg_next.X -distsin ;
      shp_buf[1].Y :=  ptg_next.Y +distcos ;


      k := 2 ;
      if point_no < (ioshp.GetPartSize(part_no) -2) then begin
        ptgn := ioshp.GetPoint( part_no, point_no +2) ;

        dx := ptgn.X -ptg_next.X;
        dy := ptgn.Y -ptg_next.Y;

        Result := set_sin_cosin(dx, dy) ;
        if Result then begin
          distsina := dist*sin_angle ;
          distcosa := dist*cos_angle ;

          if pt_is_left(ptgn) then begin
            shp_buf[k].X :=  ptg_next.X +distsina ;
            shp_buf[k].Y :=  ptg_next.Y -distcosa ;
          end
          else begin
            shp_buf[k].X := ptg_next.X -distsina;
            shp_buf[k].Y := ptg_next.Y +distcosa;
          end;
          inc(k) ;
        end;
      end;

      shp_buf[k].X := ptg_next.X +distsin;
      shp_buf[k].Y := ptg_next.Y -distcos;
      inc(k) ;

      shp_buf[k].X := ptg.X +distsin;
      shp_buf[k].Y := ptg.Y -distcos;

      if point_no = 0 then begin
        if part_no = 0  then begin
          SetLength(first_offpoint,num_parts) ;
          SetLength(last_offpoint,num_parts) ;
        end;
        if line_on_left then
          first_offpoint[part_no] := shp_buf[0]
        else
          first_offpoint[part_no] := shp_buf[k] ;
      end;


      if point_no = max_points_idx then begin
        if line_on_left then
          last_offpoint[part_no] := shp_buf[1]
        else
          last_offpoint[part_no] := shp_buf[2] ;
      end;


      for i := 0 to k do begin
        polygon.AddPoint( shp_buf[i] ) ;
      end ;

      polygon.IsFixed := True ;
      Result := True ;

    end ;

    function make_miter : Boolean ;
    var
      k, i   : Integer ;
      dx, dy : Double  ;
      dx1, dy1 : Double  ;
      dx2, dy2 : Double  ;
      ptgn  : TGIS_Point ;
      ptcross : TGIS_Point ;
      line1, line2 : TGIS_Line ;
      distsina, distcosa : Double ;
      is_lef : Boolean ;
    const
      M = 2;
    begin

      dx := ptg_next.X -ptg.X;
      dy := ptg_next.Y -ptg.Y;

      Result := set_sin_cosin(dx, dy) ;
      if not Result then
        exit ;


      distsin := dist*sin_angle ;
      distcos := dist*cos_angle ;

      dx1 := M*dist*cos_angle ;
      dy1 := M*dist*sin_angle ;

      shp_buf[0].X := ptg.X -distsin ;
      shp_buf[0].Y := ptg.Y +distcos ;

      shp_buf[1].X :=  ptg_next.X -distsin ;
      shp_buf[1].Y :=  ptg_next.Y +distcos ;

      is_lef := False ;
      k := 2 ;
      if point_no < (ioshp.GetPartSize(part_no) -2) then begin
        ptgn := ioshp.GetPoint( part_no, point_no +2) ;

        dx := ptgn.X -ptg_next.X;
        dy := ptgn.Y -ptg_next.Y;

        Result := set_sin_cosin(dx, dy) ;
        if Result then begin
          distsina := dist*sin_angle ;
          distcosa := dist*cos_angle ;

          dx2 := M*dist*cos_angle ;
          dy2 := M*dist*sin_angle ;

          if pt_is_left(ptgn) then begin
            is_lef := True ;
            shp_buf[k].X :=  ptg_next.X +distsina ;
            shp_buf[k].Y :=  ptg_next.Y -distcosa ;
            inc(k) ;
            shp_buf[k].X := ptg_next.X +distsina -dx2;
            shp_buf[k].Y := ptg_next.Y -distcosa -dy2;
            inc(k) ;
            shp_buf[k].X := ptg_next.X +distsin +dx1 ;
            shp_buf[k].Y := ptg_next.Y -distcos +dy1;

            line1.A.X := ptgn.X +distsina ;
            line1.A.Y := ptgn.Y -distcosa;
            line1.B := shp_buf[3] ;

            line2.A.X := ptg.X +distsin ;
            line2.A.Y := ptg.Y -distcos ;
            line2.B.X := ptg_next.X +distsin +dx1;
            line2.B.Y := ptg_next.Y -distcos +dy1;


            if GisGetLinesCrossing(line1, line2, ptcross) then begin
              shp_buf[3] := ptcross ;
              dec(k) ;

            end ;
          end
          else begin
            shp_buf[1].X :=  shp_buf[1].X +dx1;
            shp_buf[1].Y :=  shp_buf[1].Y +dy1;

            shp_buf[k].X := ptg_next.X -distsina -dx2;
            shp_buf[k].Y := ptg_next.Y +distcosa -dy2;
            inc(k) ;
            shp_buf[k].X := ptg_next.X -distsina ;
            shp_buf[k].Y := ptg_next.Y +distcosa ;
            inc(k) ;
            shp_buf[k].X := ptg_next.X +distsin ;
            shp_buf[k].Y := ptg_next.Y -distcos ;


            line1.A := shp_buf[0] ;
            line1.B := shp_buf[1] ;

            line2.A.X := ptgn.X -distsina ;
            line2.A.Y := ptgn.Y +distcosa ;
            line2.B.X := ptg_next.X -distsina -dx2;
            line2.B.Y := ptg_next.Y +distcosa -dy2;


            if GisGetLinesCrossing(line1, line2, ptcross) then begin
              shp_buf[1] := ptcross ;
              shp_buf[2] := shp_buf[3] ;
              shp_buf[3] := shp_buf[4] ;
            end ;
          end;
          inc(k) ;
        end;
      end
      else begin
        shp_buf[k].X := ptg_next.X +distsin ;
        shp_buf[k].Y := ptg_next.Y -distcos ;
        inc(k) ;
      end;


      shp_buf[k].X := ptg.X +distsin ;
      shp_buf[k].Y := ptg.Y -distcos ;

      if point_no = 0 then begin
        if part_no = 0  then begin
          SetLength(first_offpoint,num_parts) ;
          SetLength(last_offpoint,num_parts) ;
        end;
        if line_on_left then
          first_offpoint[part_no] := shp_buf[0]
        else
          first_offpoint[part_no] := shp_buf[k] ;
      end;


      if point_no = max_points_idx then begin
        if line_on_left then
          last_offpoint[part_no] := shp_buf[1]
        else
          last_offpoint[part_no] := shp_buf[2] ;
      end;


      for i := 0 to k do begin
        polygon.AddPoint( shp_buf[i] ) ;
      end ;

      polygon.IsFixed := True ;
      Result := True ;

    end ;


    function make_trapeze : Boolean ;
    var
      k, i   : Integer ;
      dx, dy : Double  ;
      sdx1, sdy1 : Double  ;
      sdx2, sdy2 : Double  ;
      ptgn : TGIS_Point ;
      ptcross : TGIS_Point ;
      line1, line2 : TGIS_Line ;
      distsina1, distcosa1 : Double ;
      distsina2, distcosa2 : Double ;
      sina1, cosa1 : Double ;
      sina2, cosa2 : Double ;
      is_lef : Boolean ;
      sdist  : Double ;
      ab2, bc2, ac2 : Double ;
      a1Point, c1Point : TGIS_Point ;
      a3Point, c3Point: TGIS_Point ;
      wanted_dist : Double ;

    begin

      dx := ptg_next.X -ptg.X;
      dy := ptg_next.Y -ptg.Y;

      Result := set_sin_cosin(dx, dy) ;
      if not Result then
        exit ;
      sina1 := sin_angle ;
      cosa1 := cos_angle ;


      distsina1 := dist*sina1 ;
      distcosa1 := dist*cosa1 ;

//      sdist := sqrt(dist*dist*2) ;
      sdist := dist ;
      sdx1 := sdist*cosa1 ;
      sdy1 := sdist*sina1 ;

      shp_buf[0].X := ptg.X -distsina1 ;
      shp_buf[0].Y := ptg.Y +distcosa1 ;

      shp_buf[1].X :=  ptg_next.X -distsina1 ;
      shp_buf[1].Y :=  ptg_next.Y +distcosa1 ;


      is_lef := False ;
      k := 2 ;
      if point_no < (ioshp.GetPartSize(part_no) -2) then begin
        ptgn := ioshp.GetPoint( part_no, point_no +2) ;

        dx := ptgn.X -ptg_next.X;
        dy := ptgn.Y -ptg_next.Y;

        Result := set_sin_cosin(dx, dy) ;
        if Result then begin
          sina2 := sin_angle ;
          cosa2 := cos_angle ;
          distsina2 := dist*sina2 ;
          distcosa2 := dist*cosa2 ;

          sdx2 := sdist*cosa2 ;
          sdy2 := sdist*sina2 ;

          if pt_is_left(ptgn) then begin
            is_lef := True ;
            shp_buf[k].X :=  ptg_next.X +distsina2 ;
            shp_buf[k].Y :=  ptg_next.Y -distcosa2 ;
            a3Point := shp_buf[k] ;
            inc(k) ;
            shp_buf[k].X := ptg_next.X +distsina2 -sdx2;
            shp_buf[k].Y := ptg_next.Y -distcosa2 -sdy2;
            c3Point := shp_buf[k] ;
            inc(k) ;
            shp_buf[k].X := ptg_next.X +distsina1 +sdx1 ;
            shp_buf[k].Y := ptg_next.Y -distcosa1 +sdy1;
            c1Point := shp_buf[k] ;

            line1.A.X := ptgn.X +distsina2 ;
            line1.A.Y := ptgn.Y -distcosa2;
            line1.B := shp_buf[3] ;

            line2.A.X := ptg.X +distsina1 ;
            line2.A.Y := ptg.Y -distcosa1 ;
            line2.B.X := ptg_next.X +distsina1 +sdx1;
            line2.B.Y := ptg_next.Y -distcosa1 +sdy1;


            if GisGetLinesCrossing(line1, line2, ptcross) then begin
              shp_buf[3] := ptcross ;
              dec(k) ;

            end
            else begin //moving to right distance
//              ac1 := GisPoint2Point(a1Point, c1Point) ;
              a1Point.X :=  ptg_next.X +distsina1 ;
              a1Point.Y :=  ptg_next.Y -distcosa1 ;

              ab2 := GisLine2Point(  shp_buf[2], a1Point, ptg_next) ;
              ac2 := GisLine2Point(  shp_buf[3], shp_buf[4], ptg_next) ;
              bc2 := ac2 -ab2 ;
              wanted_dist := ((dist -ab2)/(ac2 -ab2))*sdist ;

              sdx1 := wanted_dist*cosa1 ;
              sdy1 := wanted_dist*sina1 ;
              sdx2 := wanted_dist*cosa2 ;
              sdy2 := wanted_dist*sina2 ;


              shp_buf[3].X := ptg_next.X +distsina2 -sdx2;
              shp_buf[3].Y := ptg_next.Y -distcosa2 -sdy2;
              shp_buf[4].X := ptg_next.X +distsina1 +sdx1 ;
              shp_buf[4].Y := ptg_next.Y -distcosa1 +sdy1;

              shp_buf[5].X := ptg.X +distsina1 ;
              shp_buf[5].Y := ptg.Y -distcosa1 ;


              ac2 := GisLine2Point(  shp_buf[3], shp_buf[4], ptg_next) ;
              bc2 := GisLine2Point(  shp_buf[5], shp_buf[4], ptg_next) ;
            end;
          end
          else begin
            shp_buf[1].X :=  shp_buf[1].X +sdx1;
            shp_buf[1].Y :=  shp_buf[1].Y +sdy1;

            shp_buf[k].X := ptg_next.X -distsina2 -sdx2;
            shp_buf[k].Y := ptg_next.Y +distcosa2 -sdy2;
            inc(k) ;
            shp_buf[k].X := ptg_next.X -distsina2 ;
            shp_buf[k].Y := ptg_next.Y +distcosa2 ;
            inc(k) ;
            shp_buf[k].X := ptg_next.X +distsina1 ;
            shp_buf[k].Y := ptg_next.Y -distcosa1 ;


            line1.A := shp_buf[0] ;
            line1.B := shp_buf[1] ;

            line2.A.X := ptgn.X -distsina2 ;
            line2.A.Y := ptgn.Y +distcosa2 ;
            line2.B.X := ptg_next.X -distsina2 -sdx2;
            line2.B.Y := ptg_next.Y +distcosa2 -sdy2;


            if GisGetLinesCrossing(line1, line2, ptcross) then begin
              shp_buf[1] := ptcross ;
              shp_buf[2] := shp_buf[3] ;
              shp_buf[3] := shp_buf[4] ;
            end
            else begin //moving to right distance
              a1Point.X :=  ptg_next.X -distsina1 ;
              a1Point.Y :=  ptg_next.Y +distcosa1 ;

              ab2 := GisLine2Point(  shp_buf[3], a1Point, ptg_next) ;
              ac2 := GisLine2Point(  shp_buf[1], shp_buf[2], ptg_next) ;
              bc2 := ac2 -ab2 ;
              wanted_dist := ((dist -ab2)/(ac2 -ab2))*sdist ;

              sdx1 := wanted_dist*cosa1 ;
              sdy1 := wanted_dist*sina1 ;
              sdx2 := wanted_dist*cosa2 ;
              sdy2 := wanted_dist*sina2 ;


              shp_buf[1].X := ptg_next.X -distsina1 +sdx1;
              shp_buf[1].Y := ptg_next.Y +distcosa1 +sdy1;


              shp_buf[2].X := ptg_next.X -distsina2 -sdx2 ;
              shp_buf[2].Y := ptg_next.Y +distcosa2 -sdy2;

              shp_buf[4].X := ptg.X +distsina1 ;
              shp_buf[4].Y := ptg.Y -distcosa1 ;


              ac2 := GisLine2Point(  shp_buf[1], shp_buf[2], ptg_next) ;
            end;
          end;
          inc(k) ;
        end;
      end
      else begin
        shp_buf[k].X := ptg_next.X +distsina1 ;
        shp_buf[k].Y := ptg_next.Y -distcosa1 ;
        inc(k) ;
      end;


      shp_buf[k].X := ptg.X +distsina1 ;
      shp_buf[k].Y := ptg.Y -distcosa1 ;

      if point_no = 0 then begin
        if part_no = 0  then begin
          SetLength(first_offpoint,num_parts) ;
          SetLength(last_offpoint,num_parts) ;
        end;
        if line_on_left then
          first_offpoint[part_no] := shp_buf[0]
        else
          first_offpoint[part_no] := shp_buf[k] ;
      end;


      if point_no = max_points_idx then begin
        if line_on_left then
          last_offpoint[part_no] := shp_buf[1]
        else
          last_offpoint[part_no] := shp_buf[2] ;
      end;


      for i := 0 to k do begin
        polygon.AddPoint( shp_buf[i] ) ;
      end ;

      polygon.IsFixed := True ;
      Result := True ;

    end ;



    function make_polygon2off : Boolean ;
    var
      k             : Integer ;
      dx, dy        : Double  ;
      start_point   : Integer ;
      d_start_point : Double  ;
      point_no1     : Integer ;
      extp1         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
      extp2         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
      idx1, idx2    : Integer ;
    begin

      dx := ptg_next.X -ptg.X;
      dy := ptg_next.Y -ptg.Y;

      if dx = 0 then begin
        if dy > 0 then
          step_angle := Pi / 2
        else
        if dy < 0 then
          step_angle := 1.5 * Pi
        else
        begin
          Result := False ;
          exit ;
        end ;
      end
      else
       step_angle := Pi / 2 - ArcTan2( dx, dy ) ;

      if step_angle < 0 then
        step_angle := step_angle +2 * Pi ;

      SinCos( step_angle, sin_angle, cos_angle ) ;

      d_start_point := step_angle * ( pipoints / Pi ) ;
      start_point := TruncS( d_start_point ) ;

      if d_start_point <> start_point then
        point_no1 := pipoints +1
      else
        point_no1 := pipoints ;

      idx1 := start_point ;
      while idx1 >= 2*pipoints do
        idx1 := idx1 -2*pipoints ;
      idx2 := start_point + point_no1 ;
      while idx2 >= 2*pipoints do
        idx2 := idx2 - 2*pipoints ;

      extp1.X := full_circle[idx1].X +ptg.X ;
      extp1.Y := full_circle[idx1].Y +ptg.Y ;

      extp2.X := full_circle[idx2].X +ptg.X ;
      extp2.Y := full_circle[idx2].Y +ptg.Y ;

      inc( start_point ) ;
      for k := 1 to point_no1 -1 do begin

        if start_point >= 2*pipoints then
          start_point := 0 ;

        shp_buf[k].X := full_circle[start_point].X +ptg_next.X ;
        shp_buf[k].Y := full_circle[start_point].Y +ptg_next.Y ;
        inc( start_point ) ;
      end ;

      shp_buf[0].X :=  dist*sin_angle ;
      shp_buf[0].Y := -dist*cos_angle ;

      shp_buf[point_no1].X := -dist*sin_angle ;
      shp_buf[point_no1].Y :=  dist*cos_angle ;

      shp_buf[point_no1 +1].X := shp_buf[point_no1].X +ptg.X ;
      shp_buf[point_no1 +1].Y := shp_buf[point_no1].Y +ptg.Y ;
      //extra two points
      shp_buf[point_no1 +2] := _TGIS_Point (extp2) ;
      shp_buf[point_no1 +3] := _TGIS_Point (extp1) ;
      //
      shp_buf[point_no1 +2 +2].X := shp_buf[0].X +ptg.X ;
      shp_buf[point_no1 +2 +2].Y := shp_buf[0].Y +ptg.Y ;

      shp_buf[0].X := shp_buf[0].X  +ptg.X ;
      shp_buf[0].Y := shp_buf[0].Y  +ptg.Y ;

      shp_buf[point_no1].X := shp_buf[point_no1].X +ptg_next.X ;
      shp_buf[point_no1].Y := shp_buf[point_no1].Y +ptg_next.Y ;

      if point_no = 0 then begin
        if part_no = 0  then begin
          SetLength(first_offpoint,num_parts) ;
          SetLength(last_offpoint,num_parts) ;
        end;
        first_offpoint[part_no] := ptg ;
        if line_on_left then begin
          first_offpoint[part_no].X := first_offpoint[part_no].X -dist*sin_angle ;
          first_offpoint[part_no].Y := first_offpoint[part_no].Y +dist*cos_angle ;
        end
        else begin
          first_offpoint[part_no].X := first_offpoint[part_no].X +dist*sin_angle ;
          first_offpoint[part_no].Y := first_offpoint[part_no].Y -dist*cos_angle ;
        end;
      end;


      if point_no = max_points_idx then begin
        if line_on_left then
          last_offpoint[part_no] := shp_buf[1]
        else
          last_offpoint[part_no] := shp_buf[2] ;
      end;



      for k := point_no1 +2 +2downto 0 do
        polygon.AddPoint(shp_buf[k]) ;

      polygon.IsFixed := True ;

      Result := True ;

    end ;

    procedure completeOffsetLine ;
    var
      ii, kk : Integer ;
      firstidx, lastidx : Integer ;
      smfirstidx, smlastidx : Integer ;
      iipoint : TGIS_Point ;
      ppno : Integer ;
      ppsize : Integer ;
      dist, smdist : Double ;
      pi2_points : Integer ;
      parts_av : Array of Boolean ;
    begin
      pi2_points := 9 ;

    if pi2_points < 0 then begin
      offshp :=  TGIS_ShapeMultiPoint.Create( nil, nil, False, 0, ioshp.Layer,
                                       ioshp.Dimension ) ;
      for ppno := 0 to ushp.GetNumParts -1 do begin
         offshp.AddPart ;
         offshp.AddPoint(first_offpoint[ppno]) ;
         offshp.AddPoint(last_offpoint[ppno]) ;
      end;

      exit ;

    end;

    offshp :=  TGIS_ShapeArc.Create( nil, nil, False, 0, ioshp.Layer,

                                       ioshp.Dimension ) ;

    SetLength(parts_av, ushp.GetNumParts) ;


    for ppno := 0 to ushp.GetNumParts -1 do begin
      kk := PartStatus(ushp, ppno, ii) ;
      if ii < 0 then
        parts_av[ppno] := True
      else
        parts_av[ppno] := False
    end;



    for ppno := 0 to ushp.GetNumParts -1 do begin

      if not parts_av[ppno] then
        continue ;


      firstidx := -1 ;

      ppsize := ushp.GetPartSize(ppno) ;
      smdist := 1e100 ;

      for ii := 0 to ppsize -1 do begin
        iipoint := ushp.GetPoint(ppno, ii) ;
        dist := Sqr(iipoint.X - first_offpoint[ppno].X) +
                Sqr(iipoint.Y - first_offpoint[ppno].Y) ;
        if dist < interTolerance then begin
          firstidx := ii ;
          break ;
        end ;
        if dist < smdist then begin
          smdist := dist ;
          smfirstidx := ii ;
        end;
      end;
      if firstidx = -1 then
        firstidx := smfirstidx ;


      lastidx := -1 ;
      smdist := 1e100 ;
      for ii := ppsize -1 downto 0 do begin
        iipoint := ushp.GetPoint(ppno, ii) ;
        dist := Sqr(iipoint.X - last_offpoint[ppno].X) +
                Sqr(iipoint.Y - last_offpoint[ppno].Y) ;
        if dist < interTolerance then begin
          lastidx := ii ;
          break ;
        end ;
        if dist < smdist then begin
          smdist := dist ;
          smlastidx := ii ;
        end;
      end;

      if lastidx = -1 then
        lastidx := smlastidx ;

      offshp.AddPart ;
      if line_on_left then begin
        if firstidx <= lastidx then begin
          for ii := firstidx to lastidx do
            offshp.AddPoint(ushp.GetPoint(ppno, ii)) ;
        end
        else begin
          for ii := firstidx to ppsize -1 do
            offshp.AddPoint(ushp.GetPoint(ppno, ii)) ;
          for ii := 0 to lastidx do
            offshp.AddPoint(ushp.GetPoint(ppno, ii)) ;
        end;
      end
      else begin
        if firstidx <= lastidx then begin
          for ii := firstidx downto 0 do
            offshp.AddPoint(ushp.GetPoint(ppno, ii)) ;
          for ii := ppsize -1 downto lastidx do
            offshp.AddPoint(ushp.GetPoint(ppno, ii)) ;
        end
        else begin
          for ii := firstidx downto lastidx do
            offshp.AddPoint(ushp.GetPoint(ppno, ii)) ;
        end;


      end;

    end;
  end;



  begin
    pshape1 := _shp ;
    pshape2 := nil ;
    computeTolerance ;
    pi2_points := 9 ;

    jointype := _jointype ;

    Result := nil ;

    ioshp := make_cpy(_shp) ;
    if ioshp = nil then
      exit ;

    try
      if _dist = 0 then begin
        Result := ioshp.CreateCopy ;
        exit ;
      end ;

      allpoints := ioshp.GetNumPoints ;
      inProgress := 0 ;
      fullProgress := 3*allpoints ;

      slist := TGIS_ShapeList.Create( False ) ;

      dist := Abs(_dist) ;

      polygon:=TGIS_ShapePolygon.Create( nil, nil, False, 0, ioshp.Layer,
                                        ioshp.Dimension ) ;


      SetLength(shp_buf,      4*pi2_points +3 +2) ;
      SetLength(full_circle,  4*pi2_points +3) ;
      {$IFDEF GIS_NORECORDS}
        for idx := 0 to 4*pi2_points +3 +2 -1 do
           shp_buf[idx] := new TGIS_Point;

        for idx := 0 to 4*pi2_points + 3 -1 do
          full_circle[idx] := new TGIS_Point;
      {$ENDIF}

      pipoints := 2*pi2_points;
      make_basic_circle ;

      num_parts := ioshp.GetNumParts ;
      ulist := nil ;
      in_units := False ;
      if _dist < 0 then
        line_on_left := False
      else
        line_on_left := True ;


      if num_parts > 1 then begin
        ulist := TGIS_ShapeList.Create( False ) ;
        in_units := True ;
      end
      else begin
        if ioshp.GetNumPoints  > MAX_POINTS_IN_UNIT then begin
          ulist := TGIS_ShapeList.Create( False ) ;
          in_units := True ;
        end ;
      end ;

      for part_no := 0 to num_parts - 1 do
      begin // all parts
        max_points_idx := ioshp.GetPartSize( part_no ) -2;
        only_circle := False ;
        if max_points_idx < 0 then begin
          if (ioshp.ShapeType <> TGIS_ShapeType.Polygon) and ( max_points_idx = -1) then
          begin
            only_circle := True ;
            max_points_idx := 0 ;
          end
          else
            continue ;
        end ;

        max_unit_idx := (max_points_idx +MAX_POINTS_IN_UNIT ) div MAX_POINTS_IN_UNIT -1;
        if max_unit_idx = 0 then begin
          points_in_unit := max_points_idx +1 ;
        end
        else begin
          points_in_unit :=  (max_points_idx + max_unit_idx +1)
                              div (max_unit_idx +1) ;
        end ;

        started := False ;
        ptg_next := ioshp.GetPoint( part_no, 0 ) ;
        start_idx := 0 ;

        for unit_idx := 0 to max_unit_idx do begin

          if (max_points_idx -start_idx) > points_in_unit - 1 then
            max_idx := (points_in_unit +start_idx) -1
          else
            max_idx := max_points_idx ;

          for point_no := start_idx to max_idx do
          begin // all points

            if not started then begin

              started := True ;

              ptg := ioshp.GetPoint( part_no, point_no ) ;

              if ioshp.ShapeType <> TGIS_ShapeType.Polygon then begin
                polygon.Lock( TGIS_Lock.Extent ) ;
                polygon.Reset;
                polygon.AddPart;
              end ;
            end
            else
              ptg := _TGIS_Point (ptg_next) ;

            if point_no <= max_points_idx then begin
              ptg_next := ioshp.GetPoint( part_no, point_no +1) ;
              if (ptg_next.X = ptg.X) and (ptg_next.Y = ptg.Y) then
                continue ;


              polygon.Lock( TGIS_Lock.Extent ) ;
              polygon.Reset;
              polygon.AddPart;

              case jointype of
                TGIS_JoinType.Round:
                  begin
                    if (point_no = max_points_idx) and
                       (ioshp.ShapeType = TGIS_ShapeType.Arc) then
                      poly_done := make_rectangle
                    else
                      poly_done := make_polygon2off ;
                  end ;
                TGIS_JoinType.Trapeze :
                  poly_done := make_trapeze ;
                TGIS_JoinType.Miter :
                  poly_done := make_miter ;
                TGIS_JoinType.Bevel:
                  poly_done := make_bevel ;
              end;

              if not poly_done then begin
               continue ;
              end ;


              polygon.Unlock ;
              slist.Add(polygon) ;
              polygon := TGIS_ShapePolygon(polygon.CreateCopy) ;
              if only_circle then
                break ;
            end ;
          end ;
          busyStep := max_idx -start_idx ;
          ushp := TGIS_ShapePolygon(UnionOnList(slist, False)) ;
          if in_units and (ushp <> nil) then
            ulist.Add(ushp) ;

          {$IFNDEF NEXTGEN}
            for max_idx := slist.Count -1 downto 0 do
              FreeObjectNotNil( TGIS_Shape(slist[max_idx]) ) ;
          {$ENDIF}

          start_idx := start_idx +points_in_unit ;
          slist.Clear ;
          if doBreak then
            break ;
        end ;
        if doBreak then begin
          {$IFNDEF NEXTGEN}
            for max_idx := ulist.Count -1 downto 0 do
              FreeObjectNotNil( TGIS_Shape(ulist[max_idx]) ) ;
          {$ENDIF}
          FreeObject( ulist ) ;
          FreeObject( slist ) ;
          FreeObject( polygon ) ;
          exit ;
        end ;
      end ;

      FreeObject( polygon ) ;

      if in_units then begin
        max_units := MAX_JOIN_UNITS ;
        if max_units > 1 then begin
          if ulist.Count > max_units then begin

            max_shapes_idx := ulist.Count ;
            max_unit_idx := (max_shapes_idx +max_units -1)div max_units ;

            if max_unit_idx = 0 then begin
              shapes_in_unit := max_shapes_idx ;
            end
            else begin
              shapes_in_unit := max_units ;
            end ;

            start_idx := 0 ;
            udlist := TGIS_ShapeList.Create( False ) ;

            for unit_idx := 0 to max_unit_idx -1 do begin
              if (max_shapes_idx -start_idx) > shapes_in_unit then
                max_idx := (shapes_in_unit +start_idx)
              else
                max_idx := max_shapes_idx ;

              for idx := start_idx to max_idx -1 do
              begin // all points
                slist.Add(ulist[max_shapes_idx -idx -1]) ;
              end ;

              busyStep := allpoints div max_unit_idx ;
              if not doBreak then
                ushp := TGIS_ShapePolygon(UnionOnList(slist, False))
              else
                ushp := nil ;
              if ushp <> nil then
                udlist.Add(ushp) ;
              {$IFNDEF NEXTGEN}
                for max_idx := 0 to slist.Count -1 do
                  FreeObjectNotNil( TGIS_Shape(slist[max_idx]) ) ;
              {$ENDIF}
              slist.Clear ;

              start_idx := start_idx +shapes_in_unit ;
            end ;
            ulist.Clear ;
            FreeObject( ulist ) ;
            ulist := udlist ;
          end ;
        end ;
      end ;

      FreeObject( slist ) ;

      if ushp <> nil then begin
        if ioshp.ShapeType = TGIS_ShapeType.Polygon then begin

          if in_units then begin
            shape1 := TGIS_ShapePolygon(ioshp) ;

            for max_idx := 0 to ulist.Count -1 do begin
              if max_idx = ulist.Count -1 then
                busyStep := fullProgress - inProgress
              else
                busyStep := allpoints div ulist.Count ;
              shape2 := TGIS_ShapePolygon(ulist[max_idx]) ;
              if shape2 <> nil then begin
                if _dist > 0 then
                  ushape := TGIS_ShapePolygon(combinePolygons( shape1 ,
                                                               shape2 ,
                                                       TGIS_TopologyCombineType.Union
                                                             ))
                else
                  if shape1 <> nil then
                    ushape := TGIS_ShapePolygon(combinePolygons( shape1 ,
                                                                 shape2 ,
                                                     TGIS_TopologyCombineType.Difference
                                                ))
                else
                    ushape := nil ;
                FreeObject( shape2 ) ;
                if max_idx > 0 then
                  FreeObject( shape1 ) ;
                shape1 := ushape ;
              end ;
            end ;
            ulist.Clear ;
            FreeObject( ulist ) ;
            Result := shape1 ;
          end
          else begin
            busyStep := fullProgress - inProgress ;

            if _dist > 0 then
              Result := combinePolygons( TGIS_ShapePolygon(ioshp), ushp,
                                 TGIS_TopologyCombineType.Union )
            else
              Result := combinePolygons( TGIS_ShapePolygon(ioshp), ushp,
                                 TGIS_TopologyCombineType.Difference ) ;

            if Result <> ushp then begin
              ushp.Reset ;
              FreeObject(ushp) ;
            end ;
          end ;
        end
        else begin

          if in_units then begin
            ushp := TGIS_ShapePolygon(UnionOnList(ulist, False)) ;
            {$IFNDEF NEXTGEN}
              for max_idx := ulist.Count -1 downto 0 do
                FreeObjectNotNil( TGIS_Shape(ulist[max_idx]) ) ;
            {$ENDIF}
            FreeObject( ulist ) ;
          end ;

          completeOffsetLine ;
          Result := offshp ;
          ushp.Reset ;
          FreeObject(ushp) ;
        end ;
      end
      else
      begin
        if assigned( ulist ) then begin
          if ulist.Count > 0 then begin
            {$IFNDEF NEXTGEN}
              for max_idx := ulist.Count -1 downto 0 do
                FreeObjectNotNil( TGIS_Shape(ulist[max_idx]) ) ;
            {$ENDIF}
          end ;
          FreeObject( ulist ) ;
        end ;
      end ;

      if assigned( FOnBusy ) then
        {$IFDEF OXYGENE}
          FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, doBreak ) ) ;
        {$ELSE}
          FOnBusy( self, -1, -1, doBreak ) ;
        {$ENDIF}
    finally
      FreeObject( ioshp ) ;
    end ;
  end ;

  function TGIS_Topology.MakeBuffer(
    const _shp          : TGIS_Shape        ;
    const _dist         : Double            ;
    const _pi2_points   : Integer           ;
    const _fixshape     : Boolean
  ) : TGIS_Shape ;
  var
    part_no,
    point_no         : Integer    ;
    num_parts        : Integer    ;
    max_points_idx   : Integer    ;
    max_idx          : Integer    ;
    unit_idx         : Integer    ;
    max_unit_idx     : Integer    ;
    points_in_unit   : Integer    ;
    start_idx        : Integer    ;
    dist             : Double     ;
    ushp,
    polygon          : TGIS_ShapePolygon   ;
    ptg, ptg_next    : TGIS_Point ;
    started          : Boolean    ;
    pipoints         : Integer    ;
    pi2_points       : Integer    ;

    sin_angle,
    cos_angle        : Double     ;
    step_angle       : Double     ;
    angle_pi2        : Double     ;
    slist            : TGIS_ShapeList      ;
    ulist            : TGIS_ShapeList      ;
    udlist           : TGIS_ShapeList      ;
    shape1           : TGIS_ShapePolygon ;
    shape2           : TGIS_ShapePolygon ;
    ushape           : TGIS_ShapePolygon ;

    shp_buf          : array of TGIS_Point ;
    full_circle      : array of TGIS_Point ;
    in_units         : Boolean ;
    max_units        : Integer ;
    max_shapes_idx   : Integer ;
    idx              : Integer ;
    shapes_in_unit   : Integer ;
    only_circle      : Boolean ;

    allpoints        : Int64 ;

    ishp              : TGIS_Shape ;
  const
    MAX_POINTS_IN_UNIT = 1000 ;
    MAX_JOIN_UNITS = 5 ;

    procedure make_basic_circle ;
    var
      p00 : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
      i   : Integer    ;
    begin
      p00.X := 0 ;
      p00.Y := -dist ;

      full_circle[0] := _TGIS_Point (p00);
      angle_pi2 := Pi / 2 ;

      for i := 1 to pi2_points do begin
        step_angle := (angle_pi2 * i)/pi2_points ;
        SinCos( step_angle, sin_angle, cos_angle ) ;

        full_circle[i].X := p00.X*cos_angle -p00.Y*sin_angle ;
        full_circle[i].Y := p00.X*sin_angle +p00.Y*cos_angle ;
      end ;

      for i := 1  to pi2_points do begin
        full_circle[pi2_points +i].X :=  full_circle[pi2_points -i].X ;
        full_circle[pi2_points +i].Y := -full_circle[pi2_points -i].Y;
      end ;

      for i := 1 to pipoints -1 do begin
        full_circle[pipoints +i].X := -full_circle[pipoints -i].X  ;
        full_circle[pipoints +i].Y :=  full_circle[pipoints -i].Y  ;
      end ;

    end ;

    procedure make_circle(_shp : TGIS_ShapePolygon) ;
    var
      pp : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
      k  : Integer    ;
    begin

      for k := 2*pipoints -1 downto 0 do
      begin
        pp.X := full_circle[k].X +ptg.X ;
        pp.Y := full_circle[k].Y +ptg.Y ;
        _shp.AddPoint(pp) ;
      end ;
      _shp.IsFixed := True ;
    end ;

    function make_polygon : Boolean ;
    var
      k             : Integer ;
      dx, dy        : Double  ;
      start_point   : Integer ;
      d_start_point : Double  ;
      point_no1     : Integer ;
      extp1         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
      extp2         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
      idx1, idx2    : Integer ;
    begin

      dx := ptg_next.X -ptg.X;
      dy := ptg_next.Y -ptg.Y;

      if dx = 0 then begin
        if dy > 0 then
          step_angle := Pi / 2
        else
        if dy < 0 then
          step_angle := 1.5 * Pi
        else
        begin
          Result := False ;
          exit ;
        end ;
      end
      else
       step_angle := Pi / 2 - ArcTan2( dx, dy ) ;

      if step_angle < 0 then
        step_angle := step_angle +2 * Pi ;

      SinCos( step_angle, sin_angle, cos_angle ) ;

      d_start_point := step_angle * ( pipoints / Pi ) ;
      start_point := TruncS( d_start_point ) ;

      if d_start_point <> start_point then
        point_no1 := pipoints +1
      else
        point_no1 := pipoints ;

      idx1 := start_point ;
      while idx1 >= 2*pipoints do
        idx1 := idx1 -2*pipoints ;
      idx2 := start_point + point_no1 ;
      while idx2 >= 2*pipoints do
        idx2 := idx2 - 2*pipoints ;

      extp1.X := full_circle[idx1].X +ptg.X ;
      extp1.Y := full_circle[idx1].Y +ptg.Y ;

      extp2.X := full_circle[idx2].X +ptg.X ;
      extp2.Y := full_circle[idx2].Y +ptg.Y ;

      inc( start_point ) ;
      for k := 1 to point_no1 -1 do begin

        if start_point >= 2*pipoints then
          start_point := 0 ;

        shp_buf[k].X := full_circle[start_point].X +ptg_next.X ;
        shp_buf[k].Y := full_circle[start_point].Y +ptg_next.Y ;
        inc( start_point ) ;
      end ;

      shp_buf[0].X :=  dist*sin_angle ;
      shp_buf[0].Y := -dist*cos_angle ;

      shp_buf[point_no1].X := -dist*sin_angle ;
      shp_buf[point_no1].Y :=  dist*cos_angle ;

      shp_buf[point_no1 +1].X := shp_buf[point_no1].X +ptg.X ;
      shp_buf[point_no1 +1].Y := shp_buf[point_no1].Y +ptg.Y ;
      //extra two points
      shp_buf[point_no1 +2] := _TGIS_Point (extp2) ;
      shp_buf[point_no1 +3] := _TGIS_Point (extp1) ;
      //
      shp_buf[point_no1 +2 +2].X := shp_buf[0].X +ptg.X ;
      shp_buf[point_no1 +2 +2].Y := shp_buf[0].Y +ptg.Y ;

      shp_buf[0].X := shp_buf[0].X  +ptg_next.X ;
      shp_buf[0].Y := shp_buf[0].Y  +ptg_next.Y ;

      shp_buf[point_no1].X := shp_buf[point_no1].X +ptg_next.X ;
      shp_buf[point_no1].Y := shp_buf[point_no1].Y +ptg_next.Y ;

      for k := point_no1 +2 +2downto 0 do
        polygon.AddPoint(shp_buf[k]) ;

      polygon.IsFixed := True ;

      Result := True ;
    end ;

    function make_rectangle : Boolean ;
    var
      k  : Integer ;
      dx,
      dy : Double  ;
    begin

      dx := ptg_next.X -ptg.X;
      dy := ptg_next.Y -ptg.Y;

      if dx = 0 then begin
        if dy > 0 then
          step_angle := Pi / 2
        else
        if dy < 0 then
          step_angle := 1.5 * Pi
        else
        begin
          Result := False ;
          exit ;
        end ;
      end
      else
       step_angle := Pi / 2 -ArcTan2(dx, dy) ;

      if step_angle < 0 then
        step_angle := step_angle +2 * Pi ;

      SinCos( step_angle, sin_angle, cos_angle ) ;

      shp_buf[0].X :=  dist*sin_angle ;
      shp_buf[0].Y := -dist*cos_angle ;

      shp_buf[1].X := -dist*sin_angle ;
      shp_buf[1].Y :=  dist*cos_angle ;

      shp_buf[2].X := shp_buf[1].X +ptg.X;
      shp_buf[2].Y := shp_buf[1].Y +ptg.Y;

      shp_buf[3].X := shp_buf[0].X +ptg.X;
      shp_buf[3].Y := shp_buf[0].Y +ptg.Y;

      shp_buf[0].X := shp_buf[0].X +ptg_next.X;
      shp_buf[0].Y := shp_buf[0].Y +ptg_next.Y;

      shp_buf[1].X := shp_buf[1].X +ptg_next.X;
      shp_buf[1].Y := shp_buf[1].Y +ptg_next.Y;

      for k := 3 downto 0 do begin
        polygon.AddPoint( shp_buf[k] ) ;
      end ;

      polygon.IsFixed := True ;
      Result := True ;

    end ;

  begin
    Result := nil ;
    ishp := make_cpy(_shp) ;
    if ishp = nil then
      exit ;

    try
      if _dist = 0 then begin
        Result := ishp.CreateCopy ;
        exit ;
      end ;

      allpoints := ishp.GetNumPoints ;
      inProgress := 0 ;
      fullProgress := 3*allpoints ;

      slist := TGIS_ShapeList.Create( False ) ;

      if ishp.ShapeType = TGIS_ShapeType.Polygon then begin
        dist := Abs(_dist) ;
      end
      else begin
        if _dist < 0 then
          exit ;
        dist := _dist ;
      end ;

      if _fixshape and (ishp.ShapeType = TGIS_ShapeType.Polygon) then
        FixShape(ishp) ;

      polygon:=TGIS_ShapePolygon.Create( nil, nil, False, 0, ishp.Layer,
                                        ishp.Dimension ) ;

      if _pi2_points < 1 then
        pi2_points := 9
      else
        pi2_points := _pi2_points ;

      SetLength(shp_buf,      4*pi2_points +3 +2) ;
      SetLength(full_circle,  4*pi2_points +3) ;
      {$IFDEF GIS_NORECORDS}
        for idx := 0 to 4*pi2_points +3 +2 -1 do
           shp_buf[idx] := new TGIS_Point;

        for idx := 0 to 4*pi2_points + 3 -1 do
          full_circle[idx] := new TGIS_Point;
      {$ENDIF}

      pipoints := 2*pi2_points;
      make_basic_circle ;

      num_parts := ishp.GetNumParts ;
      ulist := nil ;
      in_units := False ;

      if num_parts > 1 then begin
        ulist := TGIS_ShapeList.Create( False ) ;
        in_units := True ;
      end
      else begin
        if ishp.GetNumPoints  > MAX_POINTS_IN_UNIT then begin
          ulist := TGIS_ShapeList.Create( False ) ;
          in_units := True ;
        end ;
      end ;

      for part_no := 0 to num_parts - 1 do
      begin // all parts
        max_points_idx := ishp.GetPartSize( part_no ) -2;
        only_circle := False ;
        if max_points_idx < 0 then begin
          if (ishp.ShapeType <> TGIS_ShapeType.Polygon) and ( max_points_idx = -1) then
          begin
            only_circle := True ;
            max_points_idx := 0 ;
          end
          else
            continue ;
        end ;

        max_unit_idx := (max_points_idx +MAX_POINTS_IN_UNIT ) div MAX_POINTS_IN_UNIT -1;
        if max_unit_idx = 0 then begin
          points_in_unit := max_points_idx +1 ;
        end
        else begin
          points_in_unit :=  (max_points_idx + max_unit_idx +1)
                              div (max_unit_idx +1) ;
        end ;

        started := False ;
        start_idx := 0 ;

        for unit_idx := 0 to max_unit_idx do begin

          if (max_points_idx -start_idx) > points_in_unit - 1 then
            max_idx := (points_in_unit +start_idx) -1
          else
            max_idx := max_points_idx ;

          for point_no := start_idx to max_idx do
          begin // all points

            if not started then begin

              started := True ;

              ptg := ishp.GetPoint( part_no, point_no ) ;

              if ishp.ShapeType <> TGIS_ShapeType.Polygon then begin
                polygon.Lock( TGIS_Lock.Extent ) ;
                polygon.Reset;
                polygon.AddPart;

                make_circle(polygon) ;
                polygon.Unlock ;
                slist.Add(polygon) ;
                polygon := TGIS_ShapePolygon(polygon.CreateCopy) ;
              end ;
            end
            else
              ptg := _TGIS_Point (ptg_next) ;

            if ishp.ShapeType <> TGIS_ShapeType.MultiPoint then begin
              if point_no <= max_points_idx then begin
                ptg_next := ishp.GetPoint( part_no, point_no +1) ;

                polygon.Lock( TGIS_Lock.Extent ) ;
                polygon.Reset;
                polygon.AddPart;

                if not make_polygon then
                  continue ;

                polygon.Unlock ;
                slist.Add(polygon) ;
                polygon := TGIS_ShapePolygon(polygon.CreateCopy) ;
                if only_circle then
                  break ;
              end ;
            end
            else
            begin
                ptg := ishp.GetPoint( part_no, point_no +1) ;

                polygon.Lock( TGIS_Lock.Extent ) ;
                polygon.Reset;
                polygon.AddPart;

                make_circle(polygon) ;

                polygon.Unlock ;

                slist.Add(polygon) ;
                polygon := TGIS_ShapePolygon(polygon.CreateCopy) ;
            end ;
          end ;
          busyStep := max_idx -start_idx ;
          ushp := TGIS_ShapePolygon(UnionOnList(slist, False)) ;
          if in_units and (ushp <> nil) then
            ulist.Add(ushp) ;

          {$IFNDEF NEXTGEN}
            for max_idx := slist.Count -1 downto 0 do
              FreeObjectNotNil( TGIS_Shape(slist[max_idx]) ) ;
          {$ENDIF}

          start_idx := start_idx +points_in_unit ;
          slist.Clear ;
          if doBreak then
            break ;
        end ;
        if doBreak then begin
          {$IFNDEF NEXTGEN}
            for max_idx := ulist.Count -1 downto 0 do
              FreeObjectNotNil( TGIS_Shape(ulist[max_idx]) ) ;
          {$ENDIF}
          FreeObject( ulist ) ;
          FreeObject( slist ) ;
          FreeObject( polygon ) ;
          exit ;
        end ;
      end ;

      FreeObject( polygon ) ;

      if in_units then begin
        max_units := MAX_JOIN_UNITS ;
        if max_units > 1 then begin
          if ulist.Count > max_units then begin

            max_shapes_idx := ulist.Count ;
            max_unit_idx := (max_shapes_idx +max_units -1)div max_units ;

            if max_unit_idx = 0 then begin
              shapes_in_unit := max_shapes_idx ;
            end
            else begin
              shapes_in_unit := max_units ;
            end ;

            start_idx := 0 ;
            udlist := TGIS_ShapeList.Create( False ) ;

            for unit_idx := 0 to max_unit_idx -1 do begin
              if (max_shapes_idx -start_idx) > shapes_in_unit then
                max_idx := (shapes_in_unit +start_idx)
              else
                max_idx := max_shapes_idx ;

              for idx := start_idx to max_idx -1 do
              begin // all points
                slist.Add(ulist[max_shapes_idx -idx -1]) ;
              end ;

              busyStep := allpoints div max_unit_idx ;
              if not doBreak then
                ushp := TGIS_ShapePolygon(UnionOnList(slist, False))
              else
                ushp := nil ;
              if ushp <> nil then
                udlist.Add(ushp) ;
              {$IFNDEF NEXTGEN}
                for max_idx := 0 to slist.Count -1 do
                  FreeObjectNotNil( TGIS_Shape(slist[max_idx]) ) ;
              {$ENDIF}
              slist.Clear ;

              start_idx := start_idx +shapes_in_unit ;
            end ;
            ulist.Clear ;
            FreeObject( ulist ) ;
            ulist := udlist ;
          end ;
        end ;
      end ;

      FreeObject( slist ) ;

      if ushp <> nil then begin
        if ishp.ShapeType = TGIS_ShapeType.Polygon then begin

          if in_units then begin
            shape1 := TGIS_ShapePolygon(ishp) ;

            for max_idx := 0 to ulist.Count -1 do begin
              if max_idx = ulist.Count -1 then
                busyStep := fullProgress - inProgress
              else
                busyStep := allpoints div ulist.Count ;
              shape2 := TGIS_ShapePolygon(ulist[max_idx]) ;
              if shape2 <> nil then begin
                if _dist > 0 then
                  ushape := TGIS_ShapePolygon(combinePolygons( shape1 ,
                                                               shape2 ,
                                                       TGIS_TopologyCombineType.Union
                                                             ))
                else
                  if shape1 <> nil then
                    ushape := TGIS_ShapePolygon(combinePolygons( shape1 ,
                                                                 shape2 ,
                                                     TGIS_TopologyCombineType.Difference
                                                ))
                else
                    ushape := nil ;
                FreeObject( shape2 ) ;
                if max_idx > 0 then
                  FreeObject( shape1 ) ;
                shape1 := ushape ;
              end ;
            end ;
            ulist.Clear ;
            FreeObject( ulist ) ;
            Result := shape1 ;
          end
          else begin
            busyStep := fullProgress - inProgress ;

            if _dist > 0 then
              Result := combinePolygons( TGIS_ShapePolygon(ishp), ushp,
                                 TGIS_TopologyCombineType.Union )
            else
              Result := combinePolygons( TGIS_ShapePolygon(ishp), ushp,
                                 TGIS_TopologyCombineType.Difference ) ;

            if Result <> ushp then begin
              ushp.Reset ;
              FreeObject(ushp) ;
            end ;
          end ;
        end
        else begin

          if in_units then begin
            ushp := TGIS_ShapePolygon(UnionOnList(ulist, False)) ;
            {$IFNDEF NEXTGEN}
              for max_idx := ulist.Count -1 downto 0 do
                FreeObjectNotNil( TGIS_Shape(ulist[max_idx]) ) ;
            {$ENDIF}
            FreeObject( ulist ) ;
          end ;

          Result :=  TGIS_ShapePolygon.Create( nil, nil, False, 0, ishp.Layer,
                                               ishp.Dimension ) ;

          addShape(Result, ushp) ;
          ushp.Reset ;
          FreeObject(ushp) ;
        end ;
      end
      else
      begin
        if assigned( ulist ) then begin
          if ulist.Count > 0 then begin
            {$IFNDEF NEXTGEN}
              for max_idx := ulist.Count -1 downto 0 do
                FreeObjectNotNil( TGIS_Shape(ulist[max_idx]) ) ;
            {$ENDIF}
          end ;
          FreeObject( ulist ) ;
        end ;
      end ;

      if assigned( FOnBusy ) then
        {$IFDEF OXYGENE}
          FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, doBreak ) ) ;
        {$ELSE}
          FOnBusy( self, -1, -1, doBreak ) ;
        {$ENDIF}
    finally
      FreeObject( ishp ) ;
    end ;
  end ;


  function TGIS_Topology.ArcSplitOnCross(
    const _shp     : TGIS_ShapeArc     ;
    const _shplist : {$IFDEF CLR}
                       IList
                     {$ELSE}
                       TGIS_ObjectList
                     {$ENDIF}
  ) : TGIS_ShapeArc ;
  var
    cross_ptg    : TGIS_Point    ;
    ptg          : TGIS_Point    ;
    cptg         : T_ExPoint ;
    localShpList : TList<TGIS_Shape>         ;
    line         : TGIS_Line     ;
    wline        : TGIS_Line     ;
    c,
    i,
    k,
    ii,
    kk           : Integer       ;
    arcpartsize,
    spartsize    : Integer       ;
    arcpartsno,
    spartsno     : Integer       ;
    sextent      : TGIS_Extent   ;
    wextent      : TGIS_Extent   ;
    is_changed   : Boolean       ;
    is_cross     : Boolean       ;
    is_added     : Boolean       ;
    shp          : TGIS_Shape    ;
    dist         : Double        ;
    ptlist       : TList<T_ExPoint>         ;

    function get_split_shp : TGIS_ShapeArc ;
    var
      li : Integer ;
    begin
      Result := TGIS_ShapeArc(_shp.CreateCopy) ;
      Result.Reset ;
      Result.AddPart ;
      Result.AddPoint(T_ExPoint(ptlist[0]).Point);
      for li := 1 to ptlist.Count -2 do begin
        if T_ExPoint(ptlist[li]).IsPartStart then
          Result.AddPart ;
        Result.AddPoint(T_ExPoint(ptlist[li]).Point);
        if T_ExPoint(ptlist[li]).IsCross then begin
          Result.AddPart ;
          Result.AddPoint(T_ExPoint(ptlist[li]).Point);
        end ;
      end ;
      Result.AddPoint(T_ExPoint(ptlist[ptlist.Count -1]).Point);
    end ;

  begin

    if (_shp.ShapeType <> TGIS_ShapeType.Arc    ) and
       (_shp.ShapeType <> TGIS_ShapeType.Polygon) then
    begin
      Result := nil ;
      exit ;
    end ;

    pshape1 := _shp ;
    pshape2 := nil  ;
    computeTolerance ;

    localShpList := TList<TGIS_Shape>.Create ;
    is_changed := False ;
    Result := nil ;

    if GisLockGreaterThanEqual( _shp.LockLevel, TGIS_Lock.Projection ) then
      sextent := _TGIS_Extent (_shp.Extent)
    else
      sextent := _TGIS_Extent (_shp.ProjectedExtent) ;

    for c := 0 to _shplist.Count -1 do begin
      shp := TGIS_Shape(_shplist[c]);

      if shp.IsDeleted then continue ;

      if shp = nil then continue ;

      if shp.Uid = _shp.Uid then
      if shp.Layer = _shp.Layer then
      if shp.TagInternal = _shp.TagInternal then
        continue ;

      if GisLockGreaterThanEqual( shp.LockLevel, TGIS_Lock.Projection ) then
        wextent := _TGIS_Extent (shp.Extent)
      else
        wextent := _TGIS_Extent (shp.ProjectedExtent) ;

      if ( wextent.XMax < sextent.XMin) or
         ( wextent.XMin > sextent.XMax) or
         ( wextent.YMax < sextent.YMin) or
         ( wextent.YMin > sextent.YMax)
      then
        continue
      else
        localShpList.Add(shp);
    end ;

    spartsno := _shp.GetNumParts ;
    ptlist := TList<T_ExPoint>.Create ;

    for k := 0 to spartsno -1 do begin
      spartsize := _shp.GetPartSize(k) ;
      cptg := T_ExPoint.Create ;
      cptg.Point := _shp.GetPoint(k, 0);
      cptg.IsCross := True ;
      cptg.IsPartStart := True ;
      ptlist.Add(cptg);
      for i := 1 to spartsize -1 do begin
        cptg := T_ExPoint.Create ;
        cptg.Point := _shp.GetPoint(k, i);
        if i = spartsize -1 then
          cptg.IsCross := True ;
        ptlist.Add(cptg);
      end ;
    end ;

    for c := 0 to localShpList.Count -1 do begin
      shp := TGIS_Shape(localShpList[c]);

      arcpartsno := shp.GetNumParts ;

      for kk := 0 to arcpartsno -1 do begin
        arcpartsize := shp.GetPartSize(kk) ;
        for ii := 0 to arcpartsize -2 do begin
          if ii > 0 then
            wline.A := _TGIS_Point (wline.B)
          else
            wline.A  := shp.GetPoint(kk, 0);
          wline.B := shp.GetPoint(kk, ii +1);
          if (wline.A.X = wline.B.X) and (wline.A.Y = wline.B.Y) then begin
            continue ;
          end ;

          i := 0 ;
          repeat begin
            is_cross := False ;
            is_added := False ;

            line.A := _TGIS_Point (T_ExPoint(ptlist[i]).Point) ;
            line.B := _TGIS_Point (T_ExPoint(ptlist[i +1]).Point) ;

            if T_ExPoint(ptlist[i +1]).IsPartStart then begin
              line.A := _TGIS_Point (line.B) ;
              inc( i ) ;
              line.B := _TGIS_Point (T_ExPoint(ptlist[i +1]).Point) ;
            end ;

            if (line.A.X <> line.B.X) or (line.A.Y <> line.B.Y) then begin
              dist := GisLine2Point(line.A, line.B, wline.A);
              if dist < FTolerance then begin
                cross_ptg := _TGIS_Point (wline.A) ;
                is_cross := True ;
              end
              else
              begin
                dist := GisLine2Point(line.A, line.B, wline.B);
                if dist < FTolerance then begin
                  cross_ptg := _TGIS_Point (wline.B) ;
                  is_cross := True ;
                end ;
              end ;

              if not is_cross then
                if GisGetLinesCrossing(line, wline, ptg ) then begin
                  is_cross := True ;
                  cross_ptg := _TGIS_Point (ptg) ;

                end ;

              if is_cross then begin
                if (GisPoint2Point(cross_ptg, line.A) <= FTolerance) then begin
                  if not T_ExPoint(ptlist[i]).IsCross then begin
                    if (cross_ptg.X <> line.B.X) or
                       (cross_ptg.Y <> line.B.Y) then begin
                      T_ExPoint(ptlist[i]).IsCross := True;
                      is_changed := True ;
                    end ;
                  end ;
                end
                else
                if (GisPoint2Point(cross_ptg, line.B) <= FTolerance) then begin
                  if not T_ExPoint(ptlist[i +1]).IsCross then begin
                    T_ExPoint(ptlist[i +1]).IsCross := True;
                    is_changed := True ;
                  end ;
                end
                else begin

                   cptg := T_ExPoint.Create ;
                   cptg.Point := _TGIS_Point (cross_ptg) ;
                   cptg.IsCross := True ;
                   is_changed := True ;
                   is_added := True ;
                   ptlist.Insert(i +1, cptg);
                end ;
              end ;
            end ;
            if not is_added then
              inc( i )
            else
              Result := nil ;
          end ;
          until i >= ptlist.Count -1 ;
        end ;
      end ;
    end ;

    FreeObject( localShpList ) ;

    if is_changed then
      Result := get_split_shp;

    for i := ptlist.Count -1 downto 0 do begin
        cptg := T_ExPoint( ptlist[i] );
      FreeObject(cptg);
    end ;
    FreeObject( ptlist ) ;
  end ;

  function TGIS_Topology.relatePolygons(
    const _shpA  : TGIS_ShapePolygon ;
    const _shpB  : TGIS_ShapePolygon ;
    const _de9im : String
  ) : Boolean ;
  var
    idx : Integer ;
    deg_pol : TGIS_ShapePolygon ;
    persistent : Boolean ;
    de9im : String ;

    function make_contours : Boolean ;
    begin
      Result := True ;
      try

        if ( _shpA = pshapePrepared ) and assigned( pcontourPrepared ) then begin
          T_Contour( pcontourPrepared ).Reinitialize ;
          contour1 := pcontourPrepared ;
          if T_Contour( pcontourPrepared ).Count = 0 then Abort ;
        end
        else begin
          if ( _shpB <> pshapePrepared ) or
             not assigned( pcontourPrepared ) then
            computeTolerance ;

          if _shpA = pshapePrepared then begin
            contour1 := T_Contour.Create( CONTOUR_CLOSED, _shpA, self, False ) ;
            T_Contour( contour1 ).Persistent := True ;
            pcontourPrepared := contour1 ;
          end
          else
            contour1 := T_Contour.Create( CONTOUR_CLOSED, _shpA, self, False ) ;

          if T_Contour( contour1 ).Count = 0 then begin
            persistent := T_Contour( contour1 ).Persistent ;
            FreeObject(contour1) ;

            Result := firstInteriorNotUsed(de9im) ;
            if not Result then begin
              if persistent then begin
                pshapePrepared := nil ;
                pcontourPrepared := nil ;
              end ;
              exit ;
            end ;

            deg_pol := makeDPoly(_shpA) ;
            contour1 := T_Contour.Create( CONTOUR_CLOSED, deg_pol, self, False ) ;
            FreeObject( deg_pol ) ;
            if persistent then begin
              pcontourPrepared := contour1 ;
              T_Contour( contour1 ).Persistent :=  persistent ;
            end ;

          end ;
        end ;

        if ( _shpB = pshapePrepared ) and assigned( pcontourPrepared ) then begin
          T_Contour( pcontourPrepared ).Reinitialize ;
          contour2 := pcontourPrepared ;
          if T_Contour( pcontourPrepared ).Count = 0 then Abort ;
        end
        else begin
          if _shpB = pshapePrepared then begin
            contour2 := T_Contour.Create( CONTOUR_CLOSED, _shpB, self, False ) ;
            T_Contour( contour2 ).Persistent := True ;
            pcontourPrepared := contour2 ;
          end
          else
            contour2 := T_Contour.Create( CONTOUR_CLOSED, _shpB, self, False ) ;

          if T_Contour( contour2 ).Count = 0 then begin
            persistent := T_Contour( contour2 ).Persistent ;
            FreeObject(contour2) ;

            Result := secondInteriorNotUsed(de9im) ;

            if not Result then begin
              if persistent then begin
                pshapePrepared := nil ;
                pcontourPrepared := nil ;
              end ;
              exit ;
            end ;

            deg_pol := makeDPoly(_shpB) ;
            contour2 := T_Contour.Create( CONTOUR_CLOSED, deg_pol, self, False ) ;
            FreeObject( deg_pol ) ;
            if persistent then begin
              pcontourPrepared := contour1 ;
              T_Contour( contour2 ).Persistent :=  persistent ;
            end ;
          end ;
        end ;

        fixCrossVertices ;

        T_Contour(contour1).DefineEdgesSite(contour2) ;
        T_Contour(contour2).DefineEdgesSite(contour1) ;

      except
        if assigned( contour1 ) and not T_Contour(contour1).Persistent then
          FreeObject( contour1 ) ;
        if assigned( contour2 ) and not T_Contour(contour2).Persistent then
          FreeObject( contour2 ) ;
        Result := False ;
      end ;

    end ;

  begin
    de9im := _de9im ;
    Result := make_contours ;
    if Result then begin

      for idx := StringFirst to StringLast( de9im ) do begin
        case de9im[idx] of
          'F', 'f' : // An intersection not exist (dim = -1)
            case idx + 1 - StringFirst of
              1 :  // Interior-Interior
                begin
                  if T_Contour(contour1).IsEdgeInside or
                     T_Contour(contour2).IsEdgeInside or
                     T_Contour(contour2).isSharedSD
                  then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              2 :  // Interior-Boundary
                begin
                  if T_Contour(contour2).IsEdgeInside then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              4 : // Boundary-Interior
                begin
                  if T_Contour(contour1).IsEdgeInside then
                  begin
                    Result := False ;
                    break ;
                  end ;

                end ;

              3 :  // Interior-Exterior
                begin
                  if T_Contour(contour1).IsEdgeOutside or
                     T_Contour(contour2).IsEdgeInside or
                     T_Contour(contour1).isSharedCD
                  then begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              6 : // Boundary-Exterior
                begin
                  if T_Contour(contour1).IsEdgeOutside then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              5 : // Boundary-Boundary
                begin
                  if (crossingsNo > 0) or
                     (T_Contour(contour1).IsEdgeShared) then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              7 :  // Exterior-Interior
                begin
                  if T_Contour(contour2).IsEdgeOutside or
                     T_Contour(contour1).IsEdgeInside or
                     T_Contour(contour2).isSharedCD
                  then begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              8 : // Exterior-Boundary
                begin
                  if T_Contour(contour2).IsEdgeOutside then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              9 : // Exterior-Exterior
                begin
                  Result := False ;
                  break ;
                end ;
            end ;

          'T', 't' ,  // An intersection exist (dim = 0, 1 or 2)
          '2'      : // An intersection exist,
                     // maximum dimension must be 2 (dim = 2)
            case idx + 1 - StringFirst of
              1 :  // Interior-Interior
                begin
                  if T_Contour(contour1).IsEdgeInside or
                     T_Contour(contour2).IsEdgeInside or
                     T_Contour(contour2).isSharedSD
                  then
                    continue ;

                  Result := False ;
                  break ;
                end ;

              2 :  // Interior-Boundary
                begin
                  if T_Contour(contour2).IsEdgeInside then
                    if ( _de9im[ idx ] <> '2' ) then
                      continue ;

                  Result := False ;
                  break ;
                end ;

              4 : // Boundary-Interior
                begin
                  if T_Contour(contour1).IsEdgeInside then
                    if ( _de9im[ idx ] <> '2' ) then
                      continue ;

                  Result := False ;
                  break ;
                end ;

              3 :  // Interior-Exterior
                begin
                  if T_Contour(contour1).IsEdgeOutside or
                     T_Contour(contour2).IsEdgeInside or
                     T_Contour(contour1).isSharedCD
                  then
                    continue ;

                  Result := False ;
                  break ;
                end ;

              6 : // Boundary-Exterior
                begin
                  if T_Contour(contour1).IsEdgeOutside then
                    if ( _de9im[ idx ] <> '2' ) then
                      continue ;

                  Result := False ;
                  break ;
                end ;

              5 : // Boundary-Boundary
                begin
                  if ( _de9im[ idx ] <> '2' ) then
                    if (crossingsNo > 0) or
                       (T_Contour(contour1).IsEdgeShared) then
                      continue ;

                  Result := False ;
                  break ;
                end ;

              7 :  // Exterior-Interior
                begin
                  if T_Contour(contour2).IsEdgeOutside or
                     T_Contour(contour1).IsEdgeInside or
                     T_Contour(contour2).isSharedCD
                   then
                    continue ;

                  Result := False ;
                  break ;
                end ;

              8 : // Exterior-Boundary
                begin
                  if ( _de9im[ idx ] <> '2' ) then
                    if T_Contour(contour2).IsEdgeOutside then
                      continue ;

                  Result := False ;
                  break ;
                end ;

              9 : // Exterior-Exterior
                begin
                  continue ;
                end ;
            end ;
          '0'      ,  // An intersection exist,
                     // maximum dimension must be 0 (dim = 0)
          '1'      : // An intersection exist,
                     // maximum dimension must be 1 (dim = 1)
            case idx + 1 - StringFirst of
              1,  // Interior-Interior
              2,  // Interior-Boundary
              3,  // Interior-Exterior
              4,  // Boundary-Interior
              6,  // Boundary-Exterior
              7,  // Exterior-Interior
              8,  // Exterior-Boundary
              9 : // Exterior-Exterior
                begin
                  Result := False ;
                  break ;
                end ;

              5 : // Boundary-Boundary
                begin
                  if crossingsNo > 0 then begin
                    if ( _de9im[ idx ] = '0' ) and
                       ( T_Contour(contour1).IsEdgeShared )
                    then begin
                      Result := False ;
                      break ;
                    end
                    else
                    if ( _de9im[ idx ] = '1' ) and
                       ( not T_Contour(contour1).IsEdgeShared )
                    then begin
                      Result := False ;
                      break ;
                    end ;
                    continue ;
                  end ;
                  Result := False ;
                  break ;
                end ;
            end ;
          else begin
            // other char
            continue ;
          end ;
        end ;
      end ;
    end ;

    if assigned( contour1 ) then begin
      if not T_Contour(contour1).Persistent then
        FreeObject( contour1 )
    end ;
    if assigned( contour2 ) then begin
      if not T_Contour(contour2).Persistent then
        FreeObject( contour2 )
    end ;
  end ;

  function  TGIS_Topology.relateArcPolygon(
    const _shpA  : TGIS_ShapeArc ;
    const _shpB  : TGIS_ShapePolygon ;
    const _de9im : String
  ) : Boolean ;
  var
    idx          : Integer  ;
    bcrossingsNo : Integer  ;
    k            : Integer  ;
    v            : T_Vertex ;
    vlist        : T_ContourPart ;
    finded       : Boolean ;
    deg_pol      : TGIS_ShapePolygon ;
    deg_arc      : TGIS_ShapeArc ;
    persistent   : Boolean ;
    de9im        : String ;

    function empty_boundary(const _part : Integer) : Boolean ;    //crossings on arc boundary
    var
      v1, v2 : T_Vertex ;
      vlist1 : T_ContourPart ;
    begin
      {$IFDEF MANAGED}
        vlist1 :=  T_ContourPart( T_Contour(contour1).listOfParts[_part] ) ;
        v1 := T_Vertex( vlist1.VerticesList[0] ) ;
      {$ELSE}
        vlist1 :=  T_Contour(contour1).listOfParts[k] ;
        v1 := vlist1.VerticesList[0] ;
      {$ENDIF}
      {$IFDEF MANAGED}
        v2 := T_Vertex( vlist1.VerticesList[vlist1.VerticesList.Count -1] ) ;
      {$ELSE}
        v2 := vlist1.VerticesList[vlist1.VerticesList.Count -1] ;
      {$ENDIF}

      if GisPoint2Point(v1.WorkPoint2, v2.WorkPoint2) <= gridStep09 then
        Result := True
      else
        Result := False ;
    end ;

    procedure check_bcrossings ;    //crossings on arc boundary
    var
      k1 : Integer ;
      v1 : T_Vertex ;
      vlist1 : T_ContourPart ;
    begin
      bcrossingsNo := 0 ;
      finded := False ;
      for k1 := 0 to T_Contour(contour1).PartsCount -1 do begin

        {$IFDEF MANAGED}
          vlist1 :=  T_ContourPart( T_Contour(contour1).listOfParts[k1] ) ;
          v1 := T_Vertex( vlist1.VerticesList[0] ) ;
        {$ELSE}
          vlist1 :=  T_Contour(contour1).listOfParts[k1] ;
          v1 := vlist1.VerticesList[0] ;
        {$ENDIF}
        if v1.IsCrossVertex then begin
          finded := True ;
        end ;
        {$IFDEF MANAGED}
          v1 := T_Vertex( vlist1.VerticesList[vlist1.VerticesList.Count -1] ) ;
        {$ELSE}
          v1 := vlist1.VerticesList[vlist1.VerticesList.Count -1] ;
        {$ENDIF}
        if v1.IsCrossVertex then begin
          if finded then
            finded := False
          else
            inc( bcrossingsNo ) ;
        end ;
        if finded then begin
          inc( bcrossingsNo ) ;
          finded := False ;
        end ;
      end ;
    end ;

    function make_contours : Boolean ;
    begin
      Result := True ;
      try

        if ( _shpA = pshapePrepared ) and assigned( pcontourPrepared ) then begin
          T_Contour( pcontourPrepared ).Reinitialize ;
          contour1 := pcontourPrepared ;
          if T_Contour( pcontourPrepared ).Count = 0 then Abort ;
        end
        else begin
          if ( _shpB <> pshapePrepared ) or
             not assigned( pcontourPrepared ) then
            computeTolerance ;
          if _shpA = pshapePrepared then begin
            contour1 := T_Contour.Create( CONTOUR_OPENED, _shpA, self, False) ;
            T_Contour( contour1 ).Persistent := True ;
            pcontourPrepared := contour1 ;
          end
          else
            contour1 := T_Contour.Create( CONTOUR_OPENED, _shpA, self, False) ;

          if T_Contour( contour1 ).Count = 0 then begin
            persistent := T_Contour( contour1 ).Persistent ;
            FreeObject(contour1) ;

            Result := firstInteriorNotUsed(de9im) ;
            if not Result then begin
              if persistent then begin
                pshapePrepared := nil ;
                pcontourPrepared := nil ;
              end ;
              exit ;
            end ;

            deg_arc := makeDArc(_shpA) ;
            contour1 := T_Contour.Create( CONTOUR_OPENED, deg_arc, self, False ) ;
            FreeObject( deg_arc ) ;
            if persistent then begin
              pcontourPrepared := contour1 ;
              T_Contour( contour1 ).Persistent :=  persistent ;
            end ;
          end ;
        end ;

        if ( _shpB = pshapePrepared ) and assigned( pcontourPrepared ) then begin
          T_Contour( pcontourPrepared ).Reinitialize ;
          contour2 := pcontourPrepared ;
          if T_Contour( pcontourPrepared ).Count = 0 then Abort ;
        end
        else begin

          if _shpB = pshapePrepared then begin
            contour2 := T_Contour.Create( CONTOUR_CLOSED, _shpB, self, False ) ;
            T_Contour( contour2 ).Persistent := True ;
            pcontourPrepared := contour2 ;
          end
          else
            contour2 := T_Contour.Create( CONTOUR_CLOSED, _shpB, self, False ) ;

          if T_Contour( contour2 ).Count = 0 then begin
            persistent := T_Contour( contour2 ).Persistent ;
            FreeObject(contour2) ;

            Result := secondInteriorNotUsed(de9im) ;

            if not Result then begin
              if persistent then begin
                pshapePrepared := nil ;
                pcontourPrepared := nil ;
              end ;
              exit ;
            end ;

            deg_pol := makeDPoly(_shpB) ;
            contour2 := T_Contour.Create( CONTOUR_CLOSED, deg_pol, self, False ) ;
            FreeObject( deg_pol ) ;
            if persistent then begin
              pcontourPrepared := contour1 ;
              T_Contour( contour2 ).Persistent :=  persistent ;
            end ;
          end ;
        end ;

        fixCrossVertices ;

        T_Contour(contour1).DefineEdgesSite(contour2) ;
        check_bcrossings ;

      except
        if assigned( contour1 ) and not T_Contour(contour1).Persistent then
          FreeObject( contour1 ) ;
        if assigned( contour2 ) and not T_Contour(contour2).Persistent then
          FreeObject( contour2 ) ;
        Result := False ;
      end ;

    end ;
  begin
    de9im := _de9im ;
    Result := make_contours ;
    if Result then begin

      for idx := StringFirst to StringLast( de9im ) do begin
        case de9im[idx] of
          'F', 'f' : // An intersection not exist (dim = -1)
            case idx + 1 - StringFirst of
              1 :  // Interior-Interior
                begin
                  if T_Contour(contour1).IsEdgeInside then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              2 :  // Interior-Boundary
                begin
                  if (T_Contour(contour1).IsEdgeShared) or
                     (crossingsNo > bcrossingsNo)
                  then begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              3 :  // Interior-Exterior
                begin
                  if T_Contour(contour1).IsEdgeOutside then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              4 : // Boundary-Interior
                begin

                  for k := 0 to T_Contour(contour1).PartsCount -1 do begin
                    if empty_boundary(k) then
                      continue ;
                    {$IFDEF MANAGED}
                      vlist :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;
                      v := T_Vertex( vlist.VerticesList[0] ) ;
                    {$ELSE}
                      vlist :=  T_Contour(contour1).listOfParts[k] ;
                      v := vlist.VerticesList[0] ;
                    {$ENDIF}
                    if (not v.IsCrossVertex) and
                       (v.NextEdgeSite = T_EdgeSite.gesInside) then
                    begin
                      Result := False ;
                      break ;
                    end ;

                    {$IFDEF MANAGED}
                      v := T_Vertex( vlist.VerticesList[vlist.VerticesList.Count -1] ) ;
                    {$ELSE}
                      v := vlist.VerticesList[vlist.VerticesList.Count -1] ;
                    {$ENDIF}
                    if (not v.IsCrossVertex) and
                       (v.PrevEdgeSite = T_EdgeSite.gesInside) then
                    begin
                      Result := False ;
                      break ;
                    end ;
                  end ;

                  if not Result then
                    break ;
                end ;

              5 : // Boundary-Boundary
                begin
                  if (bcrossingsNo > 0) then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              6 : // Boundary-Exterior
                begin
                  for k := 0 to T_Contour(contour1).PartsCount -1 do begin
                    if empty_boundary(k) then
                      continue ;

                    {$IFDEF MANAGED}
                      vlist :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;
                      v := T_Vertex( vlist.VerticesList[0] ) ;
                    {$ELSE}
                      vlist :=  T_Contour(contour1).listOfParts[k] ;
                      v := vlist.VerticesList[0] ;
                    {$ENDIF}
                    if (not v.IsCrossVertex) and
                       (v.NextEdgeSite = T_EdgeSite.gesOutside) then
                    begin
                      Result := False ;
                      break ;
                    end ;

                    {$IFDEF MANAGED}
                      v := T_Vertex( vlist.VerticesList[vlist.VerticesList.Count -1] ) ;
                    {$ELSE}
                      v := vlist.VerticesList[vlist.VerticesList.Count -1] ;
                    {$ENDIF}
                    if (not v.IsCrossVertex) and
                       (v.PrevEdgeSite = T_EdgeSite.gesOutside) then
                    begin
                      Result := False ;
                      break ;
                    end ;
                  end ;

                  if not Result then
                    break ;
                end ;

              7 :  // Exterior-Interior
                begin
                  Result := False ;
                  break ;
                end ;

              8 : // Exterior-Boundary
                begin
                  if T_Contour(contour2).SharedEdgesNo <
                     T_Contour(contour2).EdgesNo  then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;

              9 : // Exterior-Exterior
                begin
                  Result := False ;
                  break ;
                end ;
            end ;

          'T', 't',  // An intersection exist (dim = 0, 1 or 2)
          '1' :
            case idx + 1 - StringFirst of
              1 :  // Interior-Interior
                begin
                  if (T_Contour(contour1).IsEdgeInside) then
                    continue ;

                  Result := False ;
                  break ;
                end ;

              2 :  // Interior-Boundary
                begin
                  if (T_Contour(contour1).IsEdgeShared) or
                     (crossingsNo > bcrossingsNo)
                  then
                    continue ;

                  Result := False ;
                  break ;
                end ;

              3 :  // Interior-Exterior
                begin
                  if T_Contour(contour1).IsEdgeOutside then
                    continue ;

                  Result := False ;
                  break ;
                end ;

              4 : // Boundary-Interior
                begin

                  Result := False ;
                  for k := 0 to T_Contour(contour1).PartsCount -1 do begin
                    if empty_boundary(k) then
                      continue ;

                    {$IFDEF MANAGED}
                      vlist :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;
                      v := T_Vertex( vlist.VerticesList[0] ) ;
                    {$ELSE}
                      vlist :=  T_Contour(contour1).listOfParts[k] ;
                      v := vlist.VerticesList[0] ;
                    {$ENDIF}
                    if (not v.IsCrossVertex) and
                       (v.NextEdgeSite = T_EdgeSite.gesInside) then
                    begin
                      Result := True ;
                      break ;
                    end ;

                    {$IFDEF MANAGED}
                      v := T_Vertex( vlist.VerticesList[vlist.VerticesList.Count -1] ) ;
                    {$ELSE}
                      v := vlist.VerticesList[vlist.VerticesList.Count -1] ;
                    {$ENDIF}
                    if (not v.IsCrossVertex) and
                       (v.PrevEdgeSite = T_EdgeSite.gesInside) then
                    begin
                      Result := True ;
                      break ;
                    end ;
                  end ;

                  if not Result then
                    break ;
                end ;

              5 : // Boundary-Boundary
                begin
                  if (bcrossingsNo > 0) then
                    continue ;

                  Result := False ;
                  break ;
                end ;

              6 : // Boundary-Exterior
                begin
                  Result := False ;
                  for k := 0 to T_Contour(contour1).PartsCount -1 do begin
                    if empty_boundary(k) then
                      continue ;

                    {$IFDEF MANAGED}
                      vlist :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;
                      v := T_Vertex( vlist.VerticesList[0] ) ;
                    {$ELSE}
                      vlist :=  T_Contour(contour1).listOfParts[k] ;
                      v := vlist.VerticesList[0] ;
                    {$ENDIF}
                    if (not v.IsCrossVertex) and
                       (v.NextEdgeSite = T_EdgeSite.gesOutside) then
                    begin
                      Result := True ;
                      break ;
                    end ;

                    {$IFDEF MANAGED}
                      v := T_Vertex( vlist.VerticesList[vlist.VerticesList.Count -1] ) ;
                    {$ELSE}
                      v := vlist.VerticesList[vlist.VerticesList.Count -1] ;
                    {$ENDIF}
                    if (not v.IsCrossVertex) and
                       (v.PrevEdgeSite = T_EdgeSite.gesOutside) then
                    begin
                      Result := True ;
                      break ;
                    end ;
                  end ;

                  if not Result then
                    break ;
                end ;

              7,   // Exterior-Interior
              9 : // Exterior-Exterior
                begin
                  continue ;
                end ;

              8 : // Exterior-Boundary
                begin
                  if T_Contour(contour2).SharedEdgesNo <
                     T_Contour(contour2).EdgesNo  then
                    continue ;

                  Result := False ;
                  break ;
                end ;
            end ;

          '0' :      // An intersection exist,
                     // maximum dimension must be 0 (dim = 0)
            case idx + 1 - StringFirst of
              1,  // Interior-Interior
              2,  // Interior-Boundary
              3,  // Interior-Exterior
              4,  // Boundary-Interior
              6,  // Boundary-Exterior
              7,  // Exterior-Interior
              8,  // Exterior-Boundary
              9 : // Exterior-Exterior
                begin
                  Result := False ;
                  break ;
                end ;

              5 : // Boundary-Boundary
                begin
                  if (bcrossingsNo > 0) then
                    continue ;

                  Result := False ;
                  break ;
                end ;
            end ;

          '2': // An intersection exist,
                     // maximum dimension must be 2 (dim = 2)
            begin
              Result := False ;
              break ;
            end ;
          else begin
            // other char
            continue ;
          end ;
        end ;
      end ;
    end ;

    if assigned( contour1 ) then begin
      if not T_Contour(contour1).Persistent then
        FreeObject( contour1 )
    end ;
    if assigned( contour2 ) then begin
      if not T_Contour(contour2).Persistent then
        FreeObject( contour2 )
    end ;
  end ;

  function  TGIS_Topology.relateArcs(
    const _shpA  : TGIS_ShapeArc ;
    const _shpB  : TGIS_ShapeArc ;
    const _de9im : String
  ) : Boolean ;
  var
    idx           : Integer       ;
    b1NoCrossings : Integer       ;
    b2NoCrossings : Integer       ;
    v2            : T_Vertex      ;
    v1            : T_Vertex      ;
    vlist1        : T_ContourPart ;
    deg_arc       : TGIS_ShapeArc ;
    persistent    : Boolean ;
    de9im         : String ;

    function is_boundary(const _v : T_Vertex) : Boolean ;
    var
      wv : T_Vertex ;
    begin
      if _v.PrevVertex = _v then begin
        wv := _v.NextVertex ;
        while wv <> wv.NextVertex do
          wv := wv.NextVertex ;

        if GisPoint2Point(_v.WorkPoint2, wv.WorkPoint2) <= gridStep09 then
          Result := False
        else
          Result := True ;
      end
      else
      if _v.NextVertex = _v then begin
        wv := _v.PrevVertex ;
        while wv <> wv.PrevVertex do
          wv := wv.PrevVertex ;

        if GisPoint2Point(_v.WorkPoint2, wv.WorkPoint2) <= gridStep09 then
          Result := False
        else
          Result := True ;
      end
      else
        Result := False ;
    end ;

    function is_interior_cross_interior : Boolean ;
    var
      i, k : Integer ;
      c : Integer ;
    begin

      Result := False ;

      for k := 0 to T_Contour(contour1).PartsCount -1 do begin

        {$IFDEF MANAGED}
          vlist1 :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;
        {$ELSE}
          vlist1 :=  T_Contour(contour1).listOfParts[k] ;
        {$ENDIF}

        for i := 0 to vlist1.VerticesList.Count -1 do begin
          {$IFDEF MANAGED}
            v1 := T_Vertex( vlist1.VerticesList[i] ) ;
          {$ELSE}
            v1 := vlist1.VerticesList[i] ;
          {$ENDIF}
          if v1.IsCrossVertex then begin
            if not is_boundary(v1) then begin
              v2 := v1.CommonVertex ;
              c := 0 ;
              while v2.Owner = v1.Owner do begin
                v2 :=  v2.CommonVertex ;
                inc( c ) ;
                if c > 2 then
                  break ;
              end ;
              if not is_boundary(v2) then
              begin
                Result := True ;
                exit ;
              end ;
            end ;
          end ;
        end ;
      end ;
    end ;

    function is_boundary_interior(_contour1, _contour2 : TObject) : Boolean ;
    var
      k, c : Integer ;
    begin

      Result := False ;
      for k := 0 to T_Contour(_contour1).PartsCount -1 do begin

        {$IFDEF MANAGED}
          vlist1 :=  T_ContourPart( T_Contour(_contour1).listOfParts[k] ) ;

          v1 := T_Vertex( vlist1.VerticesList[0] ) ;
        {$ELSE}
          vlist1 :=  T_Contour(_contour1).listOfParts[k] ;

          v1 := vlist1.VerticesList[0] ;
        {$ENDIF}
        if v1.IsCrossVertex then begin
          if not is_boundary(v1) then
            continue ;

          v2 := v1.CommonVertex ;
          c := 0 ;
          while v2.Owner = v1.Owner do begin
            v2 :=  v2.CommonVertex ;
            inc( c ) ;
            if c > 2 then
              break ;
          end ;
          if not is_boundary(v2) then
          begin
            Result := True ;
            exit ;
          end ;
        end ;
        {$IFDEF MANAGED}
          v1 := T_Vertex( vlist1.VerticesList[vlist1.VerticesList.Count -1] ) ;
        {$ELSE}
          v1 := vlist1.VerticesList[vlist1.VerticesList.Count -1] ;
        {$ENDIF}

        if v1.IsCrossVertex then begin
          v2 := v1.CommonVertex ;
          c := 0 ;
          while v2.Owner = v1.Owner do begin
            v2 :=  v2.CommonVertex ;
            inc( c ) ;
            if c > 2 then
              break ;
          end ;
          if not is_boundary(v2) then
          begin
            Result := True ;
            exit ;
          end ;
        end ;
      end ;
    end ;

    function is_boundary_boundary : Boolean ;
    var
      k, c : Integer ;
    begin

      Result := False ;

      for k := 0 to T_Contour(contour1).PartsCount -1 do begin

        {$IFDEF MANAGED}
          vlist1 :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;

          v1 := T_Vertex( vlist1.VerticesList[0] ) ;
        {$ELSE}
          vlist1 :=  T_Contour(contour1).listOfParts[k] ;

          v1 := vlist1.VerticesList[0] ;
        {$ENDIF}

        if v1.IsCrossVertex then begin
          if not is_boundary(v1) then
            continue ;
          v2 := v1.CommonVertex ;
          c := 0 ;
          while v2.Owner = v1.Owner do begin
            v2 :=  v2.CommonVertex ;
            inc( c ) ;
            if c > 2 then
              break ;
          end ;

          if is_boundary(v2) then
          begin
            Result := True ;
            exit ;
          end ;
        end ;
        {$IFDEF MANAGED}
          v1 := T_Vertex( vlist1.VerticesList[vlist1.VerticesList.Count -1] ) ;
        {$ELSE}
          v1 := vlist1.VerticesList[vlist1.VerticesList.Count -1] ;
        {$ENDIF}

        if v1.IsCrossVertex then begin
          if not is_boundary(v1) then
            continue ;
          v2 := v1.CommonVertex ;
          c := 0 ;
          while v2.Owner = v1.Owner do begin
            v2 :=  v2.CommonVertex ;
            inc( c ) ;
            if c > 2 then
              break ;
          end ;
          if is_boundary(v2) then
          begin
            Result := True ;
            exit ;
          end ;
        end ;
      end ;
    end ;

    procedure check_bcrossings ;    //crossings on arc boundary
    var
      k : Integer ;
      v : T_Vertex ;
      vlist : T_ContourPart ;
    begin
      b1NoCrossings := 0 ;
      for k := 0 to T_Contour(contour1).PartsCount -1 do begin

        {$IFDEF MANAGED}
          vlist :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;
          v := T_Vertex( vlist.VerticesList[0] ) ;
        {$ELSE}
          vlist :=  T_Contour(contour1).listOfParts[k] ;
          v := vlist.VerticesList[0] ;
        {$ENDIF}
        if not is_boundary(v) then
          continue ;

        if not v.IsCrossVertex then
          inc( b1NoCrossings ) ;

        {$IFDEF MANAGED}
          v := T_Vertex( vlist.VerticesList[vlist.VerticesList.Count -1] ) ;
        {$ELSE}
          v := vlist.VerticesList[vlist.VerticesList.Count -1] ;
        {$ENDIF}
        if not v.IsCrossVertex then
          inc( b1NoCrossings ) ;
      end ;

      b2NoCrossings := 0 ;
      for k := 0 to T_Contour(contour2).PartsCount -1 do begin

        {$IFDEF MANAGED}
          vlist :=  T_ContourPart( T_Contour(contour2).listOfParts[k] ) ;
          v := T_Vertex( vlist.VerticesList[0] ) ;
        {$ELSE}
          vlist :=  T_Contour(contour2).listOfParts[k] ;
          v := vlist.VerticesList[0] ;
        {$ENDIF}
        if not is_boundary(v) then
          continue ;

        if not v.IsCrossVertex then
          inc( b2NoCrossings ) ;

        {$IFDEF MANAGED}
          v := T_Vertex( vlist.VerticesList[vlist.VerticesList.Count -1] ) ;
        {$ELSE}
          v := vlist.VerticesList[vlist.VerticesList.Count -1] ;
        {$ENDIF}
        if not v.IsCrossVertex then
          inc( b2NoCrossings ) ;
      end ;

    end ;

    function make_contours : Boolean ;
    begin
      Result := True ;
      try

        if ( _shpA = pshapePrepared ) and assigned( pcontourPrepared ) then begin
          T_Contour( pcontourPrepared ).Reinitialize ;
          contour1 := pcontourPrepared ;
          if T_Contour( pcontourPrepared ).Count = 0 then Abort ;
        end
        else begin
          if ( _shpB <> pshapePrepared ) or
             not assigned( pcontourPrepared ) then
            computeTolerance ;
          if _shpA = pshapePrepared then begin
            contour1 := T_Contour.Create( CONTOUR_OPENED, _shpA, self, False) ;
            T_Contour( contour1 ).Persistent := True ;
            pcontourPrepared := contour1 ;
          end
          else
            contour1 := T_Contour.Create( CONTOUR_OPENED, _shpA, self, False) ;

          if T_Contour( contour1 ).Count = 0 then begin
            persistent := T_Contour( contour1 ).Persistent ;
            FreeObject(contour1) ;

            Result := firstInteriorNotUsed(de9im) ;
            if not Result then begin
              if persistent then begin
                pshapePrepared := nil ;
                pcontourPrepared := nil ;
              end ;
              exit ;
            end ;

            deg_arc := makeDArc(_shpA) ;
            contour1 := T_Contour.Create( CONTOUR_OPENED, deg_arc, self, False ) ;
            FreeObject( deg_arc ) ;
            if persistent then begin
              pcontourPrepared := contour1 ;
              T_Contour( contour1 ).Persistent :=  persistent ;
            end ;
          end ;
        end ;

        if ( _shpB = pshapePrepared ) and assigned( pcontourPrepared ) then begin
          T_Contour( pcontourPrepared ).Reinitialize ;
          contour2 := pcontourPrepared ;
          if T_Contour( pcontourPrepared ).Count = 0 then Abort ;
        end
        else begin
          if _shpB = pshapePrepared then begin
            contour2 := T_Contour.Create( CONTOUR_OPENED, _shpB, self, False) ;
            T_Contour( contour2 ).Persistent := True ;
            pcontourPrepared := contour2 ;
          end
          else
            contour2 := T_Contour.Create( CONTOUR_OPENED, _shpB, self, False) ;

          if T_Contour( contour2 ).Count = 0 then begin
            persistent := T_Contour( contour2 ).Persistent ;
            FreeObject(contour2) ;

            Result := secondInteriorNotUsed(de9im) ;

            if not Result then begin
              if persistent then begin
                pshapePrepared := nil ;
                pcontourPrepared := nil ;
              end ;
              exit ;
            end ;

            deg_arc := makeDArc(_shpB) ;
            contour2 := T_Contour.Create( CONTOUR_OPENED, deg_arc, self, False ) ;
            FreeObject( deg_arc ) ;
            if persistent then begin
              pcontourPrepared := contour1 ;
              T_Contour( contour2 ).Persistent :=  persistent ;
            end ;
          end ;
        end ;

        fixCrossVertices ;

        T_Contour(contour1).DefineEdgesSite(nil) ;
        T_Contour(contour2).DefineEdgesSite(nil) ;

        check_bcrossings ;

      except
        if assigned( contour1 ) and not T_Contour(contour1).Persistent then
          FreeObject( contour1 ) ;
        if assigned( contour2 ) and not T_Contour(contour2).Persistent then
          FreeObject( contour2 ) ;
        Result := False ;
      end ;

    end ;
  begin
    de9im := _de9im ;
    Result := make_contours ;
    if Result then begin

      for idx := StringFirst to StringLast( de9im ) do begin
        case de9im[idx] of
          'F', 'f' : // An intersection not exist (dim = -1)
            case idx + 1 - StringFirst of
              1 :  // Interior-Interior
                begin
                  if T_Contour(contour1).IsEdgeShared or
                     is_interior_cross_interior then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              2 :  // Interior-Boundary
                begin
                  if is_boundary_interior( contour2, contour1 ) then begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              3 :  // Interior-Exterior
                begin
                  if T_Contour(contour1).EdgesNo  >
                     T_Contour(contour1).SharedEdgesNo then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              4 : // Boundary-Interior
                begin
                  if is_boundary_interior( contour1, contour2 ) then begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              6 : // Boundary-Exterior
                begin
                  if b1NoCrossings > 0 then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              5 : // Boundary-Boundary
                begin
                  if is_boundary_boundary then begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              7 :  // Exterior-Interior
                begin
                  if T_Contour(contour2).SharedEdgesNo <
                     T_Contour(contour2).EdgesNo then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              8 : // Exterior-Boundary
                begin
                   if b2NoCrossings > 0 then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;
              9 : // Exterior-Exterior
                begin
                  Result := False ;
                  break ;
                end ;
            end ;

          'T',
          't' :  // An intersection exist (dim = 0, 1 or 2)
            case idx + 1 - StringFirst of
              1 :  // Interior-Interior
                begin
                  if ( is_interior_cross_interior ) or
                     ( T_Contour(contour1).IsEdgeShared ) then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              2 :  // Interior-Boundary
                begin
                  if is_boundary_interior( contour2, contour1 ) then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              3 :  // Interior-Exterior
                begin
                  if T_Contour(contour1).EdgesNo  >
                     T_Contour(contour1).SharedEdgesNo then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              4 : // Boundary-Interior
                begin
                  if is_boundary_interior( contour1, contour2 ) then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              6 : // Boundary-Exterior
                begin
                  if b1NoCrossings > 0 then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              5 : // Boundary-Boundary
                begin
                  if is_boundary_boundary then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              7 :  // Exterior-Interior
                begin
                  if T_Contour(contour2).SharedEdgesNo <
                     T_Contour(contour2).EdgesNo then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              8 : // Exterior-Boundary
                begin
                  if b2NoCrossings > 0 then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              9 : // Exterior-Exterior
                begin
                  if _de9im[idx] = '1' then begin
                    Result := False ;
                    break ;
                  end ;
                  continue ;
                end ;
            end ;

          '1' :      // An intersection must be a segment (dim = 1)
            case idx + 1 - StringFirst of
              1 :  // Interior-Interior
                begin
                  if T_Contour(contour1).IsEdgeShared then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              2 ,  // Interior-Boundary
              4 , // Boundary-Interior
              5 , // Boundary-Boundary
              6 : // Boundary-Exterior
                begin
                  Result := False ;
                  break ;
                end ;
              3 :  // Interior-Exterior
                begin
                  if T_Contour(contour1).EdgesNo  >
                     T_Contour(contour1).SharedEdgesNo then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              7 :  // Exterior-Interior
                begin
                  if T_Contour(contour2).EdgesNo  >
                     T_Contour(contour2).SharedEdgesNo then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              8, 9 : // Exterior-Boundary
                     // Exterior-Exterior (can't be line segment)
                begin
                  Result := False ;
                  break ;
                end ;
              end ;

          '0'      : // An intersection exist,
                     // maximum dimension must be 0 (dim = 0)
            case idx + 1 - StringFirst of
              1 :  // Interior-Interior
                begin
                  if ( is_interior_cross_interior ) and
                     ( not T_Contour(contour1).IsEdgeShared ) then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              2 :  // Interior-Boundary
                begin
                  if is_boundary_interior( contour2, contour1 ) then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              3 :  // Interior-Exterior
                begin
                  Result := False ;
                  break ;
                end ;
              4 : // Boundary-Interior
                begin
                  if is_boundary_interior( contour1, contour2 ) then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              6 : // Boundary-Exterior
                begin
                  if b1NoCrossings > 0 then
                    continue ;

                  Result := False ;
                  break ;

                end ;
              5 : // Boundary-Boundary
                begin
                  if is_boundary_boundary then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              7 :  // Exterior-Interior
                begin
                  Result := False ;
                  break ;
                end ;
              8 : // Exterior-Boundary
                begin
                  if b2NoCrossings > 0 then
                    continue ;

                  Result := False ;
                  break ;
                end ;
              9 : // Exterior-Exterior
                begin
                  Result := False ;
                  break ;
                end ;
            end ;
          '2' :  // An intersection must be an are (dim = 2)
              begin
                if idx <> 9 then begin// Exterior-Exterior
                  Result := False ;
                  break ;
                end ;
              end ;
          else begin
            // other char
            continue ;
          end ;
        end ;
      end ;
    end ;

    if assigned( contour1 ) then begin
      if not T_Contour(contour1).Persistent then
        FreeObject( contour1 )
    end ;
    if assigned( contour2 ) then begin
      if not T_Contour(contour2).Persistent then
        FreeObject( contour2 )
    end ;
  end ;

  function  TGIS_Topology.relatePointPolygon(
    const _shpA  : TGIS_Shape        ;
    const _shpB  : TGIS_ShapePolygon ;
    const _de9im : String
  ) : Boolean ;
  var
    idx, i      : Integer    ;
    point       : TGIS_Point ;
    cnt         : Integer    ;
    deg_pol     : TGIS_ShapePolygon ;
    persistent  : Boolean ;
    de9im       : String ;
  begin
    Result := True ;
    de9im  := _de9im ;

    try
      if ( _shpB = pshapePrepared ) and assigned( pcontourPrepared ) then begin
        // reinitialization not important for relations point-polygon
        //T_Contour( pcontourPrepared ).Reinitialize ;
        contour1 := pcontourPrepared ;
        contour2 := nil ;
        if T_Contour( pcontourPrepared ).Count = 0 then Abort ;
      end
      else begin
        computeTolerance ;
        if _shpB = pshapePrepared then begin
          contour1 := T_Contour.Create( CONTOUR_CLOSED, _shpB, self, True) ;
          T_Contour( contour1 ).Persistent := True ;
          pcontourPrepared := contour1 ;
        end
        else
          contour1 := T_Contour.Create( CONTOUR_CLOSED, _shpB, self, False) ;

        if T_Contour( contour1 ).Count = 0 then begin
          persistent := T_Contour( contour1 ).Persistent ;
          FreeObject(contour1) ;

          Result := firstInteriorNotUsed(de9im) ;
          if not Result then begin
            if persistent then begin
              pshapePrepared := nil ;
              pcontourPrepared := nil ;
            end ;
            exit ;
          end ;

          deg_pol := makeDPoly(_shpA) ;
          contour1 := T_Contour.Create( CONTOUR_CLOSED, deg_pol, self, False ) ;
          FreeObject( deg_pol ) ;
          if persistent then begin
            pcontourPrepared := contour1 ;
            T_Contour( contour1 ).Persistent :=  persistent ;
          end ;
        end ;
      end ;
    except
      if assigned( contour1 ) and not T_Contour(contour1).Persistent then
        FreeObject( contour1 ) ;
      Result := False ;
    end ;

    if not Result then exit ;

    cnt := _shpA.GetPartSize( 0 ) - 1 ;

    for idx := StringFirst to StringLast( de9im ) do begin
      case de9im[idx] of
        'F', 'f' : // An intersection not exist (dim = -1)
          case idx + 1 - StringFirst of
            1 :  // Interior-Interior
              begin
                for i := 0 to cnt do begin
                  point := _shpA.GetPoint(0, i) ;
                  if not T_Contour(contour1).IsPointOnContour(point) then begin
                    if T_Contour(contour1).IsPointInsideContour( point ) then begin
                      Result := False ;
                      break ;
                    end ;
                  end ;
                end ;
                if not Result then
                  break ;
              end ;
            2 :  // Interior-Boundary
              begin
                for i := 0 to cnt do begin
                  point := _shpA.GetPoint(0, i) ;
                  if T_Contour(contour1).IsPointOnContour( point ) then begin
                    Result := False ;
                    break ;
                  end ;
                end ;
                if not Result then
                  break ;
              end ;
            3 :  // Interior-Exterior
              begin
                for i := 0 to cnt do begin
                  point := _shpA.GetPoint(0, i) ;
                  if not T_Contour(contour1).IsPointInsideContour(point) then
                  begin
                    if not T_Contour(contour1).IsPointOnContour( point ) then begin
                      Result := False ;
                      break ;
                   end ;
                  end ;
                end ;
                if not Result then
                  break ;
              end ;
            4,  // Boundary-Interior
            5,  // Boundary-Boundary
            6 : // Boundary-Exterior
              begin
                continue ;
              end ;
            7,   // Exterior-Interior
            8,  // Exterior-Boundary
            9 : // Exterior-Exterior
              begin
                Result := False ;
                break ;
              end ;
          end ;
        'T', 't',  // An intersection exist (dim = 0, 1 or 2)
        '0' :      // An intersection exist,
                   // maximum dimension must be 0 (dim = 0)
          case idx + 1 - StringFirst of
            1 :  // Interior-Interior
              begin
                Result := False ;
                for i := 0 to cnt do begin
                  point := _shpA.GetPoint(0, i) ;
                  if T_Contour(contour1).IsPointInsideContour( point ) then begin
                    if not T_Contour(contour1).IsPointOnContour( point ) then
                    begin
                      Result := True ;
                      break ;
                    end ;
                  end ;
                end ;
                if not Result then
                  break ;
              end ;
            2 :  // Interior-Boundary
              begin
                Result := False ;
                for i := 0 to cnt do begin
                  point := _shpA.GetPoint(0, i) ;
                  if T_Contour(contour1).IsPointOnContour(point) then begin
                    Result := True ;
                    break ;
                  end ;
                end ;

                if not Result then
                  break ;
              end ;

            3 :  // Interior-Exterior
              begin
                Result := False ;
                for i := 0 to cnt do begin
                  point := _shpA.GetPoint(0, i) ;
                  if not T_Contour(contour1).IsPointOnContour(point) then
                  begin
                    if not T_Contour(contour1).IsPointInsideContour( point ) then
                    begin
                      Result := True ;
                      break ;
                    end ;
                  end ;
                end ;

                if not Result then
                  break ;
              end ;
            4,  // Boundary-Interior
            5,  // Boundary-Boundary
            6 : // Boundary-Exterior
              begin
                Result := False ;
                break ;
              end ;
            7,   // Exterior-Interior
            8,  // Exterior-Boundary
            9 : // Exterior-Exterior
              begin
                if _de9im[idx] <> '0' then
                  continue ;
                Result := False ;
                break ;
              end ;
          end ;
        '1' :      // An intersection exist,
                   // maximum dimension must be 1 (dim = 1)
          begin
            if idx = 8 then
              continue ;
            Result := False ;
            break ;
          end ;
        '2' :      // An intersection exist,
                   // maximum dimension must be 2 (dim = 2)
          begin
            if (idx = 7) or  // Exterior-Interior
               (idx = 9)then // Exterior-Exterior
              continue ;
            Result := False ;
            break ;
          end ;
        else begin
          // other char
          continue ;
        end ;
      end ;
    end ;

    if not T_Contour(contour1).Persistent then
      FreeObject( contour1 ) ;
  end ;

  function TGIS_Topology.relatePointArc(
    const _shpA  : TGIS_Shape     ;
    const _shpB  : TGIS_ShapeArc  ;
    const _de9im : String
  ) : Boolean ;
  var
    idx, i : Integer ;
    point : TGIS_Point ;
    v : T_Vertex ;
    vlist : T_ContourPart ;
    bNumber : Integer ;
    boundary_checked : Boolean ;
    deg_arc      : TGIS_ShapeArc ;
    persistent   : Boolean ;
    de9im        : String ;

    procedure check_boundary ;
    var
      k      : Integer ;
      v1, v2 : T_Vertex ;
    begin

      bNumber := 0 ;
      boundary_checked := True ;
      for k := 0 to T_Contour(contour1).PartsCount -1 do begin
        {$IFDEF MANAGED}
          vlist :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;
          v1 := T_Vertex( vlist.VerticesList[0] ) ;
          v2 := T_Vertex( vlist.VerticesList[vlist.VerticesList.Count -1] ) ;
        {$ELSE}
          vlist :=  T_Contour(contour1).listOfParts[k] ;
          v1 := vlist.VerticesList[0] ;
          v2 := vlist.VerticesList.
                     Items[vlist.VerticesList.Count -1] ;
        {$ENDIF}

        if GisPoint2Point(v1.WorkPoint2, v2.WorkPoint2) > gridStep09 then
          inc( bNumber, 2 ) ;
      end ;
    end ;

    function point_on_boundary : Boolean ;
    var
      finded : Boolean ;
      k : Integer ;
    begin

      finded := False ;
      for k := 0 to T_Contour(contour1).PartsCount -1 do begin
        {$IFDEF MANAGED}
          vlist :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;
          v := T_Vertex( vlist.VerticesList[0] ) ;
        {$ELSE}
          vlist :=  T_Contour(contour1).listOfParts[k] ;
          v := vlist.VerticesList[0] ;
        {$ENDIF}
        if GisPoint2Point(point, v.WorkPoint2) <= gridStep09 then begin
          finded := True ;
        end ;

        {$IFDEF MANAGED}
          v := T_Vertex( vlist.VerticesList[vlist.VerticesList.Count -1] ) ;
        {$ELSE}
          v := vlist.VerticesList.
                     Items[vlist.VerticesList.Count -1] ;
        {$ENDIF}

        if GisPoint2Point(point, v.WorkPoint2) <= gridStep09 then begin
          if finded then begin
            finded := False ;
            continue ;
          end ;
          Result := True ;
          exit ;
        end ;
        if finded then begin
          Result := True ;
          exit ;
        end ;

      end ;
      Result := False ;
    end ;

    function point_on_all_boundarys : Boolean ;
    var
      k : Integer ;
    begin

      for k := 0 to T_Contour(contour1).PartsCount -1 do begin
        {$IFDEF MANAGED}
          vlist :=  T_ContourPart( T_Contour(contour1).listOfParts[k] ) ;
          v := T_Vertex( vlist.VerticesList[0] ) ;
        {$ELSE}
          vlist :=  T_Contour(contour1).listOfParts[k] ;
          v := vlist.VerticesList[0] ;
        {$ENDIF}
        if GisPoint2Point(point, v.WorkPoint2) > gridStep09 then begin
          Result := False ;
          exit ;
        end ;

        {$IFDEF MANAGED}
          v := T_Vertex( vlist.VerticesList[vlist.VerticesList.Count -1] ) ;
        {$ELSE}
          v := vlist.VerticesList[vlist.VerticesList.Count -1] ;
        {$ENDIF}

        if GisPoint2Point(point, v.WorkPoint2) > gridStep09 then begin
          Result := False ;
          exit ;
        end ;
      end ;
      Result := True ;
    end ;

  begin
    Result := True ;
    de9im := _de9im ;

    try
      if ( _shpB = pshapePrepared ) and assigned( pcontourPrepared ) then begin
        // reinitialization not important for Arcs
        // T_Contour( pcontourPrepared ).Reinitialize ;
        contour1 := pcontourPrepared ;
        if T_Contour( pcontourPrepared ).Count = 0 then Abort ;
      end
      else begin
        computeTolerance ;
        if _shpB = pshapePrepared then begin
          contour1 := T_Contour.Create( CONTOUR_OPENED, _shpB, self, True) ;
          T_Contour( contour1 ).Persistent := True ;
          pcontourPrepared := contour1 ;
        end
        else
          contour1 := T_Contour.Create( CONTOUR_OPENED, _shpB, self, False) ;

        if T_Contour( contour1 ).Count = 0 then begin
          persistent := T_Contour( contour1 ).Persistent ;
          FreeObject(contour1) ;

          Result := firstInteriorNotUsed(de9im) ;
          if not Result then begin
            if persistent then begin
              pshapePrepared := nil ;
              pcontourPrepared := nil ;
            end ;
            exit ;
          end ;

          deg_arc := makeDArc(_shpA) ;
          contour1 := T_Contour.Create( CONTOUR_OPENED, deg_arc, self, False ) ;
          FreeObject( deg_arc ) ;
          if persistent then begin
            pcontourPrepared := contour1 ;
            T_Contour( contour1 ).Persistent :=  persistent ;
          end ;
        end ;
      end ;
    except
      if assigned( contour1 ) and not T_Contour(contour1).Persistent then
        FreeObject( contour1 ) ;
      Result := False ;
    end ;

    if not Result then exit ;
    boundary_checked := False ;
    for idx := StringFirst to StringLast( de9im ) do begin
      case de9im[idx] of
        'F', 'f' : // An intersection not exist (dim = -1)
          case idx + 1 - StringFirst of
            1 :  // Interior-Interior
              begin
                for i := 0 to _shpA.GetNumParts -1 do begin
                  point := _shpA.GetPoint(i, 0) ;
                  if T_Contour(contour1).IsPointOnContour(point) then begin
                    if not point_on_boundary then begin
                      Result := False ;
                      break ;
                    end ;
                  end ;
                end ;
                if not Result then
                  break ;
              end ;
            2 :  // Interior-Boundary
              begin
                for i := 0 to _shpA.GetNumParts -1 do begin
                  point := _shpA.GetPoint(i, 0) ;
                  if point_on_boundary then begin
                    Result := False ;
                    break ;
                  end ;
                end ;
                if not Result then
                  break ;
              end ;

            3 :  // Interior-Exterior
              begin
                for i := 0 to _shpA.GetNumParts -1 do begin
                  point := _shpA.GetPoint(i, 0) ;
                  if not T_Contour(contour1).IsPointOnContour(point) then
                  begin
                    Result := False ;
                    break ;
                  end ;
                end ;
                if not Result then
                  break ;

              end ;
            4,  // Boundary-Interior
            5,  // Boundary-Boundary
            6 : // Boundary-Exterior
              begin
                continue ;
              end ;
            7,   // Exterior-Interior
            9 : // Exterior-Exterior
              begin
                Result := False ;
                break ;
              end ;
            8 :  // Exterior-Boundary
              begin
                if not boundary_checked then
                  check_boundary ;
                if bNumber > 0 then begin
                  Result := False ;
                  break ;
                end ;
              end ;
          end ;
        'T', 't',  // An intersection exist (dim = 0, 1 or 2)
        '0' :       // An intersection exist,
                   // maximum dimension must be 0 (dim = 0)
          case idx + 1 - StringFirst of
            1 :  // Interior-Interior
              begin
                Result := False ;
                for i := 0 to _shpA.GetNumParts -1 do begin
                  point := _shpA.GetPoint(i, 0) ;
                  if T_Contour(contour1).IsPointOnContour(point) then begin
                    if  not point_on_boundary then begin
                      Result := True ;
                      break ;
                    end ;
                  end ;
                end ;
                if not Result then
                  break ;
              end ;
            2 :  // Interior-Boundary
              begin
                Result := False ;
                for i := 0 to _shpA.GetNumParts -1 do begin
                  point := _shpA.GetPoint(i, 0) ;
                  if point_on_boundary then begin
                    Result := True ;
                    break ;
                  end ;
                end ;
                if not Result then
                  break ;
              end ;

            3 :  // Interior-Exterior
              begin
                Result := False ;
                for i := 0 to _shpA.GetNumParts -1 do begin
                  point := _shpA.GetPoint(i, 0) ;
                  if not T_Contour(contour1).IsPointOnContour(point) then
                  begin
                    Result := True ;
                    break ;
                  end ;
                end ;
                if not Result then
                  break ;

              end ;
            4,  // Boundary-Interior
            5,  // Boundary-Boundary
            6 :  // Boundary-Exterior
              begin
                Result := False ;
                break ;
              end ;
            7 : // Exterior-Interior
              begin
                if _de9im[idx] = '0' then
                  Result := False ;
                  break ;
              end ;
            8 :  // Exterior-Boundary
              begin
                if not boundary_checked then
                  check_boundary ;
                if bNumber = 0 then begin
                  Result := False ;
                  break ;
                end ;
                Result := False ;
                for i := 0 to _shpA.GetNumParts -1 do begin
                  point := _shpA.GetPoint(i, 0) ;
                  if not point_on_all_boundarys then begin
                    Result := True ;
                    break ;
                  end ;
                end ;
                if not Result then
                  break ;
              end ;
            9 : // Exterior-Exterior
              begin
                if _de9im[idx] = '0' then begin
                  Result := False ;
                  break ;
                end ;
                continue ;
              end ;
          end ;
        '1' :      // An intersection exist,
                   // maximum dimension must be 1 (dim = 1)
          begin
            if idx = 7 then //Exterior-Interior
              continue ;
            Result := False ;
            break ;
          end ;
        '2' :      // An intersection exist,
                   // maximum dimension must be 2 (dim = 2)
          begin
            if idx = 9 then //Exterior-Exterior
              continue ;
            Result := False ;
            break ;
          end ;
        else begin
          // other char
          continue ;
        end ;
      end ;
    end ;

    if not T_Contour(contour1).Persistent then
      FreeObject( contour1 ) ;
  end ;

  function  TGIS_Topology.relatePoints(
    const _shpA  : TGIS_Shape ;
    const _shpB  : TGIS_Shape ;
    const _de9im : String
  ) : Boolean ;
  var
    idx, i, k : Integer ;
    pointA : TGIS_Point ;
    pointB : TGIS_Point ;
  begin
    Result := True ;

    for idx := StringFirst to StringLast( _de9im ) do begin
      case _de9im[idx] of
        'F', 'f' : // An intersection not exist (dim = -1)
          case idx + 1 - StringFirst of
            1 :  // Interior-Interior
              begin
                for i := 0 to _shpA.GetPartSize(0) -1 do begin
                  pointA := _shpA.GetPoint(0, i) ;

                  for k := 0 to _shpB.GetPartSize(0) -1 do begin
                    pointB := _shpB.GetPoint(0, k) ;
                    if GisPoint2Point(pointA, pointB) <= gridStep09 then begin
                      Result := False ;
                      break
                    end ;
                    if not Result then break ;
                  end ;
                end ;
                if not Result then break ;
              end ;
            2,  // Interior-Boundary
            4,  // Boundary-Interior
            5,  // Boundary-Boundary
            6,  // Boundary-Exterior
            8 : // Exterior-Boundary
              begin
                continue ;
              end ;
            3 :   // Interior-Exterior
              begin
                for i := 0 to _shpA.GetPartSize(0) -1 do begin
                  pointA := _shpA.GetPoint(0, i) ;

                  Result := False ;
                  for k := 0 to _shpB.GetPartSize(0) -1 do begin
                    pointB := _shpB.GetPoint(0, k) ;
                    if GisPoint2Point(pointA, pointB) <= gridStep09 then begin
                      Result := True ;
                      break
                    end ;
                  end ;
                  if Result = False then break ;
                end ;
                if not Result then break ;
              end ;
            7 :  // Exterior-Interior
              begin
                for i := 0 to _shpB.GetPartSize(0) -1 do begin
                  pointB := _shpB.GetPoint(0, i) ;
                  Result := False ;
                  for k := 0 to _shpA.GetPartSize(0) -1 do begin
                    pointA := _shpA.GetPoint(0, k) ;
                    if GisPoint2Point(pointA, pointB) <= gridStep09 then begin
                      Result := True ;
                      break
                    end ;
                  end ;
                  if Result = False then break ;
                end ;
                if not Result then break ;
              end ;
            9 : // Exterior-Exterior
              begin
                Result := False ;
                break ;
              end ;
          end ;
        'T', 't',  // An intersection exist (dim = 0, 1 or 2)
        '0' :      // An intersection exist,
                   // maximum dimension must be 0 (dim = 0)
          case idx + 1 - StringFirst of
            1 :  // Interior-Interior
              begin
                Result := False ;
                for i := 0 to _shpA.GetPartSize(0) -1 do begin
                  pointA := _shpA.GetPoint(0, i) ;

                  for k := 0 to _shpB.GetPartSize(0) -1 do begin
                    pointB := _shpB.GetPoint(0, k) ;
                    if GisPoint2Point(pointA, pointB) <= gridStep09 then begin
                      Result := True ;
                      break
                    end ;
                  end ;
                  if Result then break ;
                end ;
                if not Result then break ;
              end ;
            2,  // Interior-Boundary
            4,  // Boundary-Interior
            5,  // Boundary-Boundary
            6,  // Boundary-Exterior
            8 : // Exterior-Boundary
              begin
                Result := False ;
                break ;
              end ;
            3 :   // Interior-Exterior
              begin
                for i := 0 to _shpA.GetPartSize(0) -1 do begin
                  pointA := _shpA.GetPoint(0, i) ;

                  Result := True ;
                  for k := 0 to _shpB.GetPartSize(0) -1 do begin
                    pointB := _shpB.GetPoint(0, k) ;
                    if GisPoint2Point(pointA, pointB) <= gridStep09 then begin
                      Result := False ;
                      break
                    end ;
                  end ;
                  if Result = True then break ;
                end ;
                if not Result then break ;
              end ;
            7 :  // Exterior-Interior
              begin
                for i := 0 to _shpB.GetPartSize(0) -1 do begin
                  pointB := _shpB.GetPoint(0, i) ;
                  Result := True ;
                  for k := 0 to _shpA.GetPartSize(0) -1 do begin
                    pointA := _shpA.GetPoint(0, k) ;
                    if GisPoint2Point(pointA, pointB) <= gridStep09 then begin
                      Result := False ;
                      break
                    end ;
                  end ;
                  if Result = True then break ;
                end ;
                if not Result then break ;
              end ;
            9 : // Exterior-Exterior
              begin
                if _de9im[idx] <> '0' then
                  continue ;
                Result := False ;
                break ;
              end ;
          end ;
        '1',       // An intersection exist,
                   // maximum dimension must be 1 (dim = 1)
        '2' :      // An intersection exist,
                   // maximum dimension must be 2 (dim = 2)
          begin
            if idx = 9 then begin
              if _de9im[idx] = '2' then
                  continue ;
            end ;
            Result := False ;
            break ;
          end ;
        else begin
          // other char
          continue ;
        end ;
      end ;
    end ;

  end ;

  function TGIS_Topology.make_cpy(
    const _shp        : TGIS_Shape
  ) : TGIS_Shape ;
  var
    i, k : Integer ;
  begin
    Result := nil ;
    if not assigned( _shp ) then
      exit ;

    case _shp.ShapeType of
      TGIS_ShapeType.Arc :
        Result := TGIS_ShapeArc.Create( nil, nil, False, 0, nil,
                                            _shp.Dimension ) ;
      TGIS_ShapeType.Polygon :
        Result := TGIS_ShapePolygon.Create( nil, nil, False, 0, nil,
                                            _shp.Dimension ) ;
      TGIS_ShapeType.MultiPoint :
        Result := TGIS_ShapeMultiPoint.Create( nil, nil, False, 0, nil,
                                            _shp.Dimension ) ;
      TGIS_ShapeType.Point :
        Result := TGIS_ShapePoint.Create( nil, nil, False, 0, nil,
                                            _shp.Dimension ) ;
      else
        exit ;
    end ;
    Result.Lock( TGIS_Lock.Extent ) ;
    for i := 0 to _shp.GetNumParts - 1 do begin
      Result.AddPart ;
      for k := 0 to _shp.GetPartSize(i) - 1 do
       Result.AddPoint3D(_shp.GetPoint3D(i, k));
    end ;
    Result.Unlock ;
  end ;

  procedure TGIS_Topology.RelatePrepare(
    const _shp : TGIS_Shape
  ) ;
  begin
    {$IFDEF JAVA}
      FreeObjectNotNilEx( contour1 ) ;
      contour1 := nil ;
      FreeObjectNotNilEx( contour2 ) ;
      contour2:= nil ;
    {$ELSE}
      FreeObject( contour1 ) ;
      FreeObject( contour2 ) ;
    {$ENDIF}

    pshapePrepared := _shp  ;
  end ;

  function TGIS_Topology.Relate(
    const _shpA  : TGIS_Shape ;
    const _shpB  : TGIS_Shape ;
    const _de9im : String
  ) : Boolean ;
  var
    shapehd,
    shapeld : TGIS_Shape ;
    isectionMatrix : String ;
    ul : Boolean ;
    llevel_ld    : TGIS_Lock ;
    llevel_hd    : TGIS_Lock ;

    function invert_matrix( const _de9im : String ) : String ;
    var
      idx   : Integer ;
      sidx  : Integer ;
      mm    : String  ;
      mm_sb : TStringBuilder ;
    begin
      mm := '*********';
      SetLengthStr( Result, 9 ) ;
      mm_sb := TStringBuilder.Create( mm ) ;
      try
        sidx := Min( StringLast( _de9im ), StringFirst + 8 ) ;
        for idx := StringFirst to sidx do
          mm_sb[idx-StringFirst] := _de9im[idx] ;
        mm := mm_sb.ToString ;
      finally
        FreeObject( mm_sb ) ;
      end ;

      mm_sb := TStringBuilder.Create( Result ) ;
      try
        for idx := StringFirst to StringFirst + 8 do begin
          sidx := 3*((idx - StringFirst) mod 3) +((idx - StringFirst)div 3) + StringFirst ;
          mm_sb[idx-StringFirst] := mm[sidx] ;
        end ;
        Result := mm_sb.ToString ;
      finally
        FreeObject( mm_sb ) ;
      end ;

      for idx := StringFirst + 8 downto StringFirst do begin
        if Result[idx] <> '*' then
          break ;
      end ;
      if idx < StringFirst + 8 then begin
        SetLengthStr( Result, idx + 1 - StringFirst ) ;
      end ;

    end ;

  begin
    if ( not assigned( _shpA ) ) or
       ( not assigned( _shpB ) ) then
    begin
      Result := False ;
      exit ;
    end ;

    if (_shpA.ShapeType = TGIS_ShapeType.Unknown) or
       (_shpA.ShapeType = TGIS_ShapeType.Deleted) or
       (_shpB.ShapeType = TGIS_ShapeType.Unknown) or
       (_shpB.ShapeType = TGIS_ShapeType.Deleted) then
    begin
      Result := False ;
      exit ;
    end ;

    Result := True ;

    if length( _de9im ) = 0 then
      exit ;

    if ord( _shpA.ShapeType ) <= ord( _shpB.ShapeType ) then begin
      shapeld := _shpA ;
      shapehd := _shpB ;
      isectionMatrix := _de9im ;
    end else begin
      shapeld := _shpB ;
      shapehd := _shpA ;

      if _de9im <> smatrixPrepared then begin
        smatrixPrepared    := _de9im ;
        smatrixPreparedInv := invert_matrix( _de9im ) ;
      end ;

      isectionMatrix := smatrixPreparedInv ;
    end ;

    pshape1 := shapeld ;
    pshape2 := shapehd ;

    llevel_ld := shapeld.LockLevel ;
    llevel_hd := shapehd.LockLevel ;

    if ( shapeld.Layer = shapehd.Layer ) or
       ( assigned( shapeld.Layer ) and
         assigned( shapehd.Layer ) and
         ( shapeld.Layer.CS.EPSG = shapehd.Layer.CS.EPSG )
        ) then begin
      ul := True ;
      shapeld.Lock(TGIS_Lock.Projection);
      shapehd.Lock(TGIS_Lock.Projection);
    end
    else
      ul := False ;

    case shapehd.ShapeType of
      TGIS_ShapeType.Polygon :
        case shapeld.ShapeType of
          TGIS_ShapeType.Polygon :
            Result := relatePolygons    ( TGIS_ShapePolygon(shapeld),
                                          TGIS_ShapePolygon(shapehd),
                                          isectionMatrix
                                        ) ;

          TGIS_ShapeType.Arc :
            Result := relateArcPolygon  ( TGIS_ShapeArc(shapeld),
                                          TGIS_ShapePolygon(shapehd),
                                          isectionMatrix
                                        ) ;

          TGIS_ShapeType.Point,
          TGIS_ShapeType.MultiPoint :
            Result := relatePointPolygon( shapeld,
                                          TGIS_ShapePolygon(shapehd),
                                          isectionMatrix
                                        ) ;
        end ;
      TGIS_ShapeType.Arc :
        case shapeld.ShapeType of
          TGIS_ShapeType.Arc :
            Result := relateArcs        ( TGIS_ShapeArc(shapeld),
                                          TGIS_ShapeArc(shapehd),
                                          isectionMatrix
                                        ) ;

          TGIS_ShapeType.Point,
          TGIS_ShapeType.MultiPoint :
            Result := relatePointArc    ( shapeld,
                                          TGIS_ShapeArc(shapehd),
                                          isectionMatrix
                                        ) ;
        end ;
      TGIS_ShapeType.Point,
      TGIS_ShapeType.MultiPoint :
         Result := relatePoints(shapeld, shapehd, isectionMatrix) ;
      else begin
        Result := False ;
      end ;
    end ;
    if ul then begin
      shapeld.Unlock ;
      shapehd.Unlock ;

      if llevel_ld <> TGIS_Lock.None then
        shapeld.Lock(llevel_ld) ;
      if llevel_hd <> TGIS_Lock.None then
        shapehd.Lock(llevel_hd) ;
    end ;
  end ;

  function  TGIS_Topology.Equality(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape
  ) : Boolean ;
  begin
    Result := Relate( _shpA, _shpB, RELATE_EQUALITY ) ;
  end ;

  function  TGIS_Topology.Disjoint(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape
  ) : Boolean ;
  begin
    Result := Relate( _shpA, _shpB, RELATE_DISJOINT ) ;
  end ;

  function  TGIS_Topology.Intersect(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape
  ) : Boolean ;
  begin
    Result := Relate(TGIS_Shape(_shpA), TGIS_Shape(_shpB), RELATE_INTERSECT)
              or
              Relate(TGIS_Shape(_shpA), TGIS_Shape(_shpB), RELATE_INTERSECT1)
              or
              Relate(TGIS_Shape(_shpA), TGIS_Shape(_shpB), RELATE_INTERSECT2)
              or
              Relate(TGIS_Shape(_shpA), TGIS_Shape(_shpB), RELATE_INTERSECT3) ;
  end ;

  function  TGIS_Topology.Touch(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape
  ) : Boolean ;
  begin
    Result := Relate(_shpA, _shpB, RELATE_TOUCH_BOUNDARY_BOUNDARY) or
              Relate(_shpA, _shpB, RELATE_TOUCH_INTERIOR_BOUNDARY) or
              Relate(_shpA, _shpB, RELATE_TOUCH_BOUNDARY_INTERIOR) ;
  end ;

  function  TGIS_Topology.Cross(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape
  ) : Boolean ;
  begin
    if (_shpA.ShapeType = TGIS_ShapeType.Point) or
       (_shpB.ShapeType = TGIS_ShapeType.Point) then
    begin
      Result := False ;
    end
    else begin
      if (_shpA.ShapeType <> _shpB.ShapeType) then
      begin

        if ord( _shpA.ShapeType ) < ord( _shpB.ShapeType ) then
          Result := Relate(_shpA, _shpB, RELATE_CROSS)
        else
          Result := Relate(_shpB, _shpA, RELATE_CROSS) ;
      end
      else begin
        if _shpA.ShapeType <> TGIS_ShapeType.Arc then
          Result := False
        else
          Result := Relate(_shpA, _shpB, RELATE_CROSS_LINE) ;
      end ;
    end ;
  end ;

  function  TGIS_Topology.Within(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape
  ) : Boolean ;
  begin
    Result := Relate(_shpA, _shpB, RELATE_WITHIN) ;
  end ;

  function  TGIS_Topology.Contains(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape
  ) : Boolean ;
  begin
    Result := Relate(_shpA, _shpB, RELATE_CONTAINS) ;
  end ;

  function  TGIS_Topology.Overlap(
    const _shpA     : TGIS_Shape ;
    const _shpB     : TGIS_Shape
  ) : Boolean ;

  begin
    if (_shpA.ShapeType <> _shpB.ShapeType) then begin
      if (ord( _shpA.ShapeType ) <= ord( TGIS_ShapeType.MultiPoint )) and
         (ord( _shpB.ShapeType ) <= ord( TGIS_ShapeType.MultiPoint ))
      then
        Result := Relate(_shpA, _shpB, RELATE_OVERLAP)
      else
        Result := False ;
    end
    else begin
      if (_shpA.ShapeType <> TGIS_ShapeType.Arc) then
        Result := Relate(_shpA, _shpB, RELATE_OVERLAP)
      else
        Result := Relate(_shpA, _shpB, RELATE_OVERLAP_LINE) ;
    end ;

  end ;

  function  TGIS_Topology.GetCrossings(
    const _shpA         : TGIS_Shape ;
    const _shpB         : TGIS_Shape
  ) : TGIS_PointList ;
  var
    hshp, lshp : TGIS_Shape ;
    chshp, clshp : TGIS_Shape ;
    is_real_cross : Boolean ;
    tshp : TGIS_Shape ;
    pt   : TGIS_Point ;
    i, k, parts, points : Integer ;
    vertex              : T_Vertex   ;
    nvertex             : T_Vertex   ;

  const
    MAX_PART_SIZE = 1000 ;
    ADD_SIZE = 100 ;

    procedure _divide_arc(_shp_in, _shp_out : TGIS_Shape) ;
    var
      ii, kk, ks, maxsize, actsize, idx : Integer ;
      fppt, ppt : TGIS_Point ;
    begin
      _shp_out.Reset ;
      _shp_out.Lock( TGIS_Lock.Projection );
      for ii := 0 to _shp_in.GetNumParts -1 do begin
        maxsize := _shp_in.GetPartSize(ii) ;
        fppt := _shp_in.GetPoint(ii, 0) ;
        idx := 1 ;
        ks := 2 ;
        while maxsize > 0 do begin
          if maxsize > (MAX_PART_SIZE + ADD_SIZE) then
            actsize := MAX_PART_SIZE
          else
            actsize := maxsize ;
          maxsize := maxsize - actsize ;
          _shp_out.AddPart ;
          _shp_out.AddPoint(fppt);
          for kk := ks to actsize do begin
            ppt := _shp_in.GetPoint(ii, idx) ;
            _shp_out.AddPoint(ppt) ;
            inc( idx ) ;
          end ;
          ks := 1 ;
          fppt := ppt ;
        end ;
      end ;
      _shp_out.Unlock ;
    end ;

    procedure _reset_crossings(_v : T_Vertex) ;
    var
      rv : T_Vertex ;
    begin
      rv := _v ;
      while rv.IsCrossVertex do begin
        rv.IsCrossVertex := False ;
        rv := rv.CommonVertex ;
      end ;
    end ;

  begin

    if (not assigned( _shpA )) or (not assigned( _shpB )) then begin
      Result := nil ;
      exit ;
    end ;

    Result := TGIS_PointList.Create ;

    if ord( _shpA.ShapeType ) >= ord( _shpB.ShapeType ) then begin
      hshp := _shpA ;
      lshp := _shpB ;
    end
    else begin
      hshp := _shpB ;
      lshp := _shpA ;
    end ;

    pshape1 := hshp ;
    pshape2 := lshp ;
    chshp := nil ;
    clshp := nil ;
    computeTolerance ;

    if (lshp.ShapeType = TGIS_ShapeType.Point) or
       (lshp.ShapeType = TGIS_ShapeType.MultiPoint) then
    begin

      if (hshp.ShapeType = TGIS_ShapeType.Polygon) or
         (hshp.ShapeType = TGIS_ShapeType.Arc)
      then
        RelatePrepare(hshp) ;

      tshp := TGIS_ShapePoint.Create ;
      tshp.Lock( TGIS_Lock(1)) ;
      parts := lshp.GetNumParts ;
      for k := 0 to parts -1 do begin
        points := lshp.GetPartSize(k) ;
        for i := 0 to points -1 do begin
          tshp.Reset ;
          tshp.AddPart ;
          pt := lshp.GetPoint(k, i) ;
          tshp.AddPoint(pt);
          if Relate(tshp, hshp, RELATE_INTERSECT) then
            Result.Add(pt) ;
        end ;
      end ;
      FreeObject( tshp ) ;
    end
    else begin
      if hshp.ShapeType = TGIS_ShapeType.Polygon then
        contour1 := T_Contour.Create( CONTOUR_CLOSED, hshp, self)
      else begin
        chshp := hshp.CreateCopy ;
        _divide_arc(hshp, chshp) ;
        contour1 := T_Contour.Create( CONTOUR_OPENED, chshp, self) ;
      end ;

      if T_Contour( contour1 ).Count = 0 then begin
        FreeObject( contour1 ) ;
        FreeObject( clshp ) ;
        FreeObject( chshp ) ;
        FreeObject( Result ) ;
        exit ;
      end ;

      if lshp.ShapeType = TGIS_ShapeType.Polygon then
        contour2 := T_Contour.Create( CONTOUR_CLOSED, lshp, self)
      else begin
        clshp := lshp.CreateCopy ;
        _divide_arc(lshp, clshp) ;
        contour2 := T_Contour.Create( CONTOUR_OPENED, clshp, self) ;
      end ;

      if T_Contour( contour2 ).Count = 0 then begin
        FreeObject( contour1 ) ;
        FreeObject( contour2 ) ;
        FreeObject( clshp ) ;
        FreeObject( chshp ) ;
        FreeObject( Result ) ;
        exit ;
      end ;

      fixCrossVertices ;

      for k := 0 to T_Contour( contour1 ).PartsCount -1 do begin
        T_Contour( contour1 ).Part := k ;
        vertex := T_Contour( contour1 ).PartVertices.VerticesList[0] ;
        nvertex := vertex.NextVertex ;
        for i := 0 to T_Contour( contour1 ).PartVertices.VerticesList.Count -1 do
        begin
          if i > 0 then begin
            vertex := nvertex ;
            nvertex := vertex.NextVertex ;
          end ;
          if vertex.IsCrossVertex then begin
            is_real_cross := True ;

            if (vertex.PrevEdgeSite = T_EdgeSite.gesSharedSD) and
               (vertex.NextEdgeSite = T_EdgeSite.gesSharedSD)
            then
              is_real_cross := False
            else
            if (vertex.PrevEdgeSite = T_EdgeSite.gesSharedCD) and
               (vertex.NextEdgeSite = T_EdgeSite.gesSharedCD)
            then
              is_real_cross := False ;

            if is_real_cross then
              Result.Add(vertex.WorkPoint2) ;
            _reset_crossings(vertex) ;
          end ;
        end ;
      end ;
      FreeObject( contour1 ) ;
      FreeObject( contour2 ) ;
      FreeObject( clshp ) ;
      FreeObject( chshp ) ;
    end ;

    if Result.Count = 0 then
      FreeObject( Result ) ;
  end ;

  function  TGIS_Topology.GetCrossings3D(
    const _shpA         : TGIS_Shape ;
    const _shpB         : TGIS_Shape
  ) : TGIS_Point3DList ;
  var
    hshp, lshp : TGIS_Shape ;
    chshp, clshp : TGIS_Shape ;
    is_real_cross : Boolean ;
    tshp : TGIS_Shape ;
    pt   : TGIS_Point3D ;
    i, k, parts, points : Integer ;
    vertex              : T_Vertex   ;
    nvertex             : T_Vertex   ;

  const
    MAX_PART_SIZE = 1000 ;
    ADD_SIZE = 100 ;

    procedure _divide_arc(_shp_in, _shp_out : TGIS_Shape) ;
    var
      ii, kk, ks, maxsize, actsize, idx : Integer ;
      fppt, ppt : TGIS_Point3D;
    begin
      _shp_out.Reset ;
      _shp_out.Lock( TGIS_Lock.Projection );
      for ii := 0 to _shp_in.GetNumParts -1 do begin
        maxsize := _shp_in.GetPartSize(ii) ;
        fppt := _shp_in.GetPoint3D(ii, 0) ;
        idx := 1 ;
        ks := 2 ;
        while maxsize > 0 do begin
          if maxsize > (MAX_PART_SIZE + ADD_SIZE) then
            actsize := MAX_PART_SIZE
          else
            actsize := maxsize ;
          maxsize := maxsize - actsize ;
          _shp_out.AddPart ;
          _shp_out.AddPoint3D(fppt);
          for kk := ks to actsize do begin
            ppt := _shp_in.GetPoint3D(ii, idx) ;
            _shp_out.AddPoint3D(ppt) ;
            inc( idx ) ;
          end ;
          ks := 1 ;
          fppt := ppt ;
        end ;
      end ;
      _shp_out.Unlock ;
    end ;

    procedure _reset_crossings(_v : T_Vertex) ;
    var
      rv : T_Vertex ;
    begin
      rv := _v ;
      while rv.IsCrossVertex do begin
        rv.IsCrossVertex := False ;
        rv := rv.CommonVertex ;
      end ;
    end ;

  begin

    if (not assigned( _shpA )) or (not assigned( _shpB )) then begin
      Result := nil ;
      exit ;
    end ;

    Result := TGIS_Point3DList.Create ;

    if ord( _shpA.ShapeType ) >= ord( _shpB.ShapeType ) then begin
      hshp := _shpA ;
      lshp := _shpB ;
    end
    else begin
      hshp := _shpB ;
      lshp := _shpA ;
    end ;

    pshape1 := hshp ;
    pshape2 := lshp ;
    chshp := nil ;
    clshp := nil ;
    computeTolerance ;

    if (lshp.ShapeType = TGIS_ShapeType.Point) or
       (lshp.ShapeType = TGIS_ShapeType.MultiPoint) then
    begin

      if (hshp.ShapeType = TGIS_ShapeType.Polygon) or
         (hshp.ShapeType = TGIS_ShapeType.Arc)
      then
        RelatePrepare(hshp) ;

      tshp := TGIS_ShapePoint.Create ;
      tshp.Lock( TGIS_Lock(1)) ;
      parts := lshp.GetNumParts ;
      for k := 0 to parts -1 do begin
        points := lshp.GetPartSize(k) ;
        for i := 0 to points -1 do begin
          tshp.Reset ;
          tshp.AddPart ;
          pt := lshp.GetPoint3D(k, i) ;
          tshp.AddPoint3D(pt);
          if Relate(tshp, hshp, RELATE_INTERSECT) then
            Result.Add(pt) ;
        end ;
      end ;
      FreeObject( tshp ) ;
    end
    else begin
      if hshp.ShapeType = TGIS_ShapeType.Polygon then
        contour1 := T_Contour.Create( CONTOUR_CLOSED, hshp, self)
      else begin
        chshp := hshp.CreateCopy ;
        _divide_arc(hshp, chshp) ;
        contour1 := T_Contour.Create( CONTOUR_OPENED, chshp, self) ;
      end ;

      if T_Contour( contour1 ).Count = 0 then begin
        FreeObject( contour1 ) ;
        FreeObject( clshp ) ;
        FreeObject( chshp ) ;
        FreeObject( Result ) ;
        exit ;
      end ;

      if lshp.ShapeType = TGIS_ShapeType.Polygon then
        contour2 := T_Contour.Create( CONTOUR_CLOSED, lshp, self)
      else begin
        clshp := lshp.CreateCopy ;
        _divide_arc(lshp, clshp) ;
        contour2 := T_Contour.Create( CONTOUR_OPENED, clshp, self) ;
      end ;

      if T_Contour( contour2 ).Count = 0 then begin
        FreeObject( contour1 ) ;
        FreeObject( contour2 ) ;
        FreeObject( clshp ) ;
        FreeObject( chshp ) ;
        FreeObject( Result ) ;
        exit ;
      end ;

      fixCrossVertices ;

      for k := 0 to T_Contour( contour1 ).PartsCount -1 do begin
        T_Contour( contour1 ).Part := k ;
        vertex := T_Contour( contour1 ).PartVertices.VerticesList[0] ;
        nvertex := vertex.NextVertex ;
        for i := 0 to T_Contour( contour1 ).PartVertices.VerticesList.Count -1 do
        begin
          if i > 0 then begin
            vertex := nvertex ;
            nvertex := vertex.NextVertex ;
          end ;
          if vertex.IsCrossVertex then begin
            is_real_cross := True ;

            if (vertex.PrevEdgeSite = T_EdgeSite.gesSharedSD) and
               (vertex.NextEdgeSite = T_EdgeSite.gesSharedSD)
            then
              is_real_cross := False
            else
            if (vertex.PrevEdgeSite = T_EdgeSite.gesSharedCD) and
               (vertex.NextEdgeSite = T_EdgeSite.gesSharedCD)
            then
              is_real_cross := False ;

            if is_real_cross then
              Result.Add(vertex.WorkPoint3) ;
            _reset_crossings(vertex) ;

            if is_real_cross then begin
              Result.Add(vertex.WorkPoint3) ;
              _reset_crossings(vertex) ;
            end ;

          end ;
        end ;
      end ;
      FreeObject( contour1 ) ;
      FreeObject( contour2 ) ;
      FreeObject( clshp ) ;
      FreeObject( chshp ) ;
    end ;

    if Result.Count = 0 then
      FreeObject( Result ) ;
  end ;

//==================================== END =====================================
end.

