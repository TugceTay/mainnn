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
  Tessellation code.
}

{$IFDEF DCC}
  unit GisTessellation ;
  {$HPPEMIT '#pragma link "GisTessellation"'}
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
  GisRtl,
  GisTypes ;
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

  {$REGION 'TGIS_Tessellation'}
  /// <summary>
  ///   Encapsulation of Tessellation class used to triangulate topologically
  ///   correct 3D polygon.
  /// </summary>
  TGIS_Tessellation = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_BaseObjectDisposable )
    private
      oTri          : TObject ;
      FTriangles    : Integer ;
      FExecuteCode  : Integer ;
    protected
      /// <summary>
      ///   class destructor.
      /// </summary>
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Class constructor.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Add vertex from a polygon outline.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_ptg">
      ///   coordinates
      /// </param>
      procedure AddVertex               ( const _part     : Integer ;
                                          const _ptg      : TGIS_Point3D
                                        ) ; overload ;

      /// <summary>
      ///   Add vertex from a polygon outline.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      /// <param name="_z">
      ///   Z coordinate
      /// </param>
      /// <param name="_m">
      ///   M coordinate
      /// </param>
      procedure AddVertex               ( const _part     : Integer ;
                                          const _x        : Double  ;
                                          const _y        : Double  ;
                                          const _z        : Double  ;
                                          const _m        : Double
                                        ) ; overload ;

      /// <summary>
      ///   Add vertex from a polygon outline.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      /// <param name="_z">
      ///   Z coordinate
      /// </param>
      /// <param name="_m">
      ///   M coordinate
      /// </param>
      ///  <param name="_i">
      ///   user defined vertex id
      /// </param>
      procedure AddVertex               ( const _part     : Integer ;
                                          const _x        : Double  ;
                                          const _y        : Double  ;
                                          const _z        : Double  ;
                                          const _m        : Double  ;
                                          const _i        : Integer
                                        ) ; overload ;

      /// <summary>
      ///   Add vertex but with normals and texture coordinates.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      /// <param name="_z">
      ///   Z coordinate
      /// </param>
      /// <param name="_m">
      ///   m coordinate
      /// </param>
      /// <param name="_nx">
      ///   normal X value
      /// </param>
      /// <param name="_ny">
      ///   normal Y value
      /// </param>
      /// <param name="_nz">
      ///   normal Z value
      /// </param>
      /// <param name="_u">
      ///   texture u coordinate
      /// </param>
      /// <param name="_v">
      ///   texture v coordinate
      /// </param>
      procedure AddVertexTuv            ( const _part     : Integer ;
                                          const _x        : Double  ;
                                          const _y        : Double  ;
                                          const _z        : Double  ;
                                          const _m        : Double  ;
                                          const _nx       : Single  ;
                                          const _ny       : Single  ;
                                          const _nz       : Single  ;
                                          const _u        : Single  ;
                                          const _v        : Single
                                        ) ; overload ;
      /// <summary>
      ///   Add vertex but with normals and texture coordinates.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_ptg">
      ///   coordinates
      /// </param>
      /// <param name="_normal">
      ///   normals
      /// </param>
      /// <param name="_u">
      ///   texture u coordinate
      /// </param>
      /// <param name="_v">
      ///   texture v coordinate
      /// </param>
      procedure AddVertexTuv            ( const _part     : Integer ;
                                          const _ptg      : TGIS_Point3D ;
                                          const _normal   : TGIS_SingleVector  ;
                                          const _u        : Single  ;
                                          const _v        : Single
                                        ) ; overload ;
      /// <summary>
      ///   Get triangle from a list of created triangles.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_x1">
      ///   X coordinates of 1 point of triangle
      /// </param>
      /// <param name="_y1">
      ///   Y coordinates of 1 point of triangle
      /// </param>
      /// <param name="_z1">
      ///   Z coordinates of 1 point of triangle
      /// </param>
      /// <param name="_x2">
      ///   X coordinates of 2 point of triangle
      /// </param>
      /// <param name="_y2">
      ///   Y coordinates of 2 point of triangle
      /// </param>
      /// <param name="_z2">
      ///   Z coordinates of 2 point of triangle
      /// </param>
      /// <param name="_x3">
      ///   X coordinates of 3 point of triangle
      /// </param>
      /// <param name="_y3">
      ///   Y coordinates of 3 point of triangle
      /// </param>
      /// <param name="_z3">
      ///   Z coordinates of 3 point of triangle
      /// </param>
      procedure GetTriangle             ( const _idx       : Integer ;
                                            var _x1       : Single  ;
                                            var _y1       : Single  ;
                                            var _z1       : Single  ;
                                            var _x2       : Single  ;
                                            var _y2       : Single  ;
                                            var _z2       : Single  ;
                                            var _x3       : Single  ;
                                            var _y3       : Single  ;
                                            var _z3       : Single
                                        ) ; overload ;
      /// <summary>
      ///   Get triangle from a list of created triangles.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_v1">
      ///   coordinates of 1 point of triangle
      /// </param>
      /// <param name="_v2">
      ///   coordinates of 1 point of triangle
      /// </param>
      /// <param name="_v3">
      ///   coordinates of 1 point of triangle
      /// </param>
      procedure GetTriangle             ( const _idx       : Integer ;
                                            var _v1       : TGIS_SingleVector  ;
                                            var _v2       : TGIS_SingleVector  ;
                                            var _v3       : TGIS_SingleVector
                                        ) ; overload ;
      /// <summary>
      ///   Get M coordinates of a triangle.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_m1">
      ///   M coordinate of 1 point of triangle
      /// </param>
      /// <param name="_m2">
      ///   M coordinate of 2 point of triangle
      /// </param>
      /// <param name="_m3">
      ///   M coordinate of 3 point of triangle
      /// </param>
      procedure GetTriangleM            ( const _idx       : Integer ;
                                            var _m1       : Single  ;
                                            var _m2       : Single  ;
                                            var _m3       : Single
                                        ) ;
      /// <summary>
      ///   Get normals and texture coordinates of a triangle.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_n1">
      ///   normal to 1 point of triangle
      /// </param>
      /// <param name="_n2">
      ///   normal to 2 point of triangle
      /// </param>
      /// <param name="_n3">
      ///   normal to 3 point of triangle
      /// </param>
      /// <param name="_t1x">
      ///   u coordinate of 1 point of triangle
      /// </param>
      /// <param name="_t1y">
      ///   v coordinate of 1 point of triangle
      /// </param>
      /// <param name="_t2x">
      ///   u coordinate of 2 point of triangle
      /// </param>
      /// <param name="_t2y">
      ///   v coordinate of 2 point of triangle
      /// </param>
      /// <param name="_t3x">
      ///   u coordinate of 3 point of triangle
      /// </param>
      /// <param name="_t3y">
      ///   v coordinate of 3 point of triangle
      /// </param>
      procedure GetTriangleTuv         ( const  _idx       : Integer  ;
                                           var  _n1       : TGIS_SingleVector ;
                                           var  _n2       : TGIS_SingleVector ;
                                           var  _n3       : TGIS_SingleVector ;
                                           var  _t1x      : Single       ;
                                           var  _t1y      : Single       ;
                                           var  _t2x      : Single       ;
                                           var  _t2y      : Single       ;
                                           var  _t3x      : Single       ;
                                           var  _t3y      : Single
                                        ) ;

      /// <summary>
      ///   Get user Ids of vertices forming a triangle.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_p1">
      ///   first point Id of triangle
      /// </param>
      /// <param name="_p2">
      ///   second point Id of triangle
      /// </param>
      /// <param name="_p3">
      ///   third point Id of triangle
      /// </param>
      procedure GetTriangleUIds         ( const   _idx      : Integer  ;
                                            var   _p1       : Integer  ;
                                            var   _p2       : Integer  ;
                                            var   _p3       : Integer
                                        ) ;
      /// <summary>
      ///   Reset settings.
      /// </summary>
      procedure Reset ;

      /// <summary>
      ///   Start a triangulation process. The result data can be accessed via
      ///   TrianglesCount and GetTriangle().
      /// </summary>
      procedure Execute ;

    public
      /// <summary>
      ///   Number of triangles created after triangulation.
      /// </summary>
      property TrianglesCount : Integer read FTriangles ;
      /// <summary>
      ///   Result code after triangulation. 0 if success, &gt; 0 found problems.
      /// </summary>
      property ExecuteCode    : Integer read FExecuteCode ;
  end ;
  {$ENDREGION 'TGIS_Tessellation'}

  {$REGION 'TGIS_MultiPatchBuilder'}
  /// <summary>
  ///   Encapsulation of multipatch builder class used to create a triangulated
  ///   mesh with vertexes and faces.
  /// </summary>
  TGIS_MultiPatchBuilder = {$IFDEF OXYGENE} public {$ENDIF}
                           class ( TGIS_BaseObjectDisposable )
  private
    oMesh : TObject ;
  private
    function fget_VertexCount : Integer ;
    function fget_FaceCount   : Integer ;
  protected
    /// <summary>
    ///   class destructor.
    /// </summary>
    procedure doDestroy ; override ;
  public
    /// <summary>
    ///   Class constructor.
    /// </summary>
    constructor Create ;
  public
    /// <summary>
    ///   Get vertex of a mesh. Vertex index counts from 0.
    /// </summary>
    /// <param name="_idx">
    ///   vertex number
    /// </param>
    /// <param name="_v">
    ///   coordinates of vertex
    /// </param>
    procedure GetVertex               ( const _idx      : Integer ;
                                          var _v        : TGIS_SingleVector
                                      ) ;
    /// <summary>
    ///   Get vertex normal of a mesh.
    /// </summary>
    /// <param name="_idx">
    ///   vertex number
    /// </param>
    /// <param name="_n">
    ///   normals
    /// </param>
    procedure GetVertexNormal         ( const _idx      : Integer ;
                                          var _n        : TGIS_SingleVector
                                      ) ;

    /// <summary>
    ///   Get face of a mesh. Face index counts from 1.
    /// </summary>
    /// <param name="_idx">
    ///   triangle number
    /// </param>
    /// <param name="_f1">
    ///   face first index
    /// </param>
    /// <param name="_f2">
    ///   face second index
    /// </param>
    /// <param name="_f3">
    ///   face third index
    /// </param>
    procedure GetFace                 ( const _idx      : Integer ;
                                          var _f1       : Integer ;
                                          var _f2       : Integer ;
                                          var _f3       : Integer
                                      ) ;

    /// <summary>
    ///   Get face normal of a mesh.
    /// </summary>
    /// <param name="_idx">
    ///   vertex number
    /// </param>
    /// <param name="_n">
    ///   normals
    /// </param>
    procedure GetFaceNormal           ( const _idx      : Integer ;
                                          var _n        : TGIS_SingleVector
                                      ) ;
    /// <summary>
    ///   Build a mesh with vertexes and faces. Must be called after Execute.
    ///   The result data can be accessed via VertexesCount, FacesCount,
    ///   GetVertex() and GetFace().
    /// </summary>
    /// <param name="_shape">
    ///   shape to parse
    /// </param>
    procedure BuildMesh              ( const _shape      : TObject
                                     ) ;
  public
    /// <summary>
    ///   Number of created vertexes after building a mesh.
    /// </summary>
    property VertexesCount  : Integer read fget_VertexCount ;
    /// <summary>
    ///   Number of created faces after building a mesh.
    /// </summary>
    property FacesCount     : Integer read fget_FaceCount ;
  end ;
  {$ENDREGION 'TGIS_MultiPatchBuilder'}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Generics.Defaults,
    System.Generics.Collections,

    GisClasses,
    GisFunctions,
    GisLayerVector ;
{$ENDIF}

{$REGION 'T_Tessellation'}
type

  /// <summary>
  ///   Structure storing detail info about vertex.
  /// </summary>
  T_3DNode = record
    /// <summary>
    ///   Point id.
    /// </summary>
    Id   : Integer ;

    /// <summary>
    ///   Position in sorted vertex array.
    /// </summary>
    Pos  : Integer ;

    /// <summary>
    ///   Part number.
    /// </summary>
    Part : Integer ;

    /// <summary>
    ///   X coordinate.
    /// </summary>
    X    : Double  ;

    /// <summary>
    ///   Y coordinate.
    /// </summary>
    Y    : Double  ;

    /// <summary>
    ///   Z coordinate.
    /// </summary>
    Z    : Double  ;

    /// <summary>
    ///   M coordinate.
    /// </summary>
    M    : Double  ;

    /// <summary>
    ///   Normal X value.
    /// </summary>
    Nx   : Single  ;

    /// <summary>
    ///   Normal Y value.
    /// </summary>
    Ny   : Single  ;

    /// <summary>
    ///   Normal Z value.
    /// </summary>
    Nz   : Single  ;

    /// <summary>
    ///   Texture X coordinate.
    /// </summary>
    Tu   : Single  ;

    /// <summary>
    ///   Texture Y coordinate.
    /// </summary>
    Tv   : Single  ;

    /// <summary>
    ///   Previous point.
    /// </summary>
    Prev : Integer ;

    /// <summary>
    ///   Next point.
    /// </summary>
    Next : Integer ;

    /// <summary>
    ///   Selection marker.
    /// </summary>
    Sel  : Integer ;

    /// <summary>
    ///   User defined vertex id.
    /// </summary>
    Uid  : Integer ;
  end ;

  /// <summary>
  ///   Array storing nodes for triangulation .
  /// </summary>
  T_3DNodeArray = array of T_3DNode ;

  /// <summary>
  ///   Structure storing coordinates of individual triangle.
  /// </summary>
  T_Triangle = record
    /// <summary>
    ///   First point X coordinate.
    /// </summary>
    X1   : Double;

    /// <summary>
    ///   First point Y coordinate.
    /// </summary>
    Y1   : Double;

    /// <summary>
    ///   First point Z coordinate.
    /// </summary>
    Z1   : Double;

    /// <summary>
    ///   Second point X coordinate.
    /// </summary>
    X2   : Double;

    /// <summary>
    ///   Second point Y coordinate.
    /// </summary>
    Y2   : Double;

    /// <summary>
    ///   Second point Z coordinate.
    /// </summary>
    Z2   : Double;

    /// <summary>
    ///   Third point X coordinate.
    /// </summary>
    X3   : Double;

    /// <summary>
    ///   Third point Y coordinate.
    /// </summary>
    Y3   : Double;

    /// <summary>
    ///   Third point Z coordinate.
    /// </summary>
    Z3   : Double;
  end ;

  /// <summary>
  ///   Array of triangles storing result of triangulation process.
  /// </summary>
  T_TriangleArray = array of T_Triangle ;

  /// <summary>
  ///   Structure storing basic info about vertex.
  /// </summary>
  T_Pnt = record
    /// <summary>
    ///   Point Id.
    /// </summary>
    Id   : Integer;

    /// <summary>
    ///   Point X coordinate.
    /// </summary>
    X    : Double;

    /// <summary>
    ///   Point Y coordinate.
    /// </summary>
    Y    : Double;
  end ;

  /// <summary>
  ///   Array vertices sorted by X,Y coordinates.
  /// </summary>
  T_PntArray = array of T_Pnt;

  /// <summary>
  ///   Array of M coordinate in returned triangles.
  /// </summary>
  T_TriangleMArray = array of TGIS_SingleVector ;

  /// <summary>
  ///   Output triangle vertices ids.
  /// </summary>
  T_Ids = record
    /// <summary>
    ///   First point Id.
    /// </summary>
    Id1 : Integer ;

    /// <summary>
    ///   Second point Id.
    /// </summary>
    Id2 : Integer ;

    /// <summary>
    ///   Third point Id.
    /// </summary>
    Id3 : Integer ;
  end ;

  /// <summary>
  ///   Array of output triangle vertices.
  /// </summary>
  T_IdsArray  = array of T_Ids ;

  /// <summary>
  ///   Part parameters.
  /// </summary>
  T_Part = record
    /// <summary>
    ///   Part number.
    /// </summary>
    Nr   : Integer ;

    /// <summary>
    ///   First point position.
    /// </summary>
    Fst  : Integer ;

    /// <summary>
    ///   Last point position.
    /// </summary>
    Lst  : Integer ;

    /// <summary>
    ///   Start point position.
    /// </summary>
    Spt  : Integer ;

    /// <summary>
    ///   Part type (True - CW, False - CCW).
    /// </summary>
    Tp   : Boolean ;
  end ;

  /// <summary>
  ///   Array of part parameters.
  /// </summary>
  T_PartArray = array of T_Part ;

  /// <summary>
  ///   Encapsulation of Triangulation class used to triangulate topologically
  ///   correct 3D polygon.
  /// </summary>
  T_Tessellation = class ( TGIS_ObjectDisposable )
    private
      firstNode       : Integer     ; // first node number in part
      curNode         : Integer     ; // current number of nodes to be processed
      extNode         : Integer     ; // current number of nodes of external contour
                                      // to be processed
      curPart         : Integer     ; // current part number
      nodNum          : Integer     ; // total number of nodes
      partNum         : Integer     ; // total number of parts
      id1,id2,id3     : Integer     ; // point position in Tnod array
      triNum          : Integer     ; // number of triangles created
      minX            : Double      ;
      startPnt        : Integer     ;
      baseNum         : Integer     ;
      ar3DNodes       : T_3DNodeArray ; // array of Vertices
      arPnts          : T_PntArray  ;   // sorted xy array
      arTriangles     : T_TriangleArray  ;   // array of returned triangles
      arTrianglesM    : T_TriangleMArray ;   // array M coordinate in returned triangles
      arTrianglesIds  : T_IdsArray  ;   // array of output triangle vertices ids
      TrianglesCount  : Integer     ;
      arParts         : T_PartArray ;   // part info array
    private
      /// <summary>
      ///   Set 'sorted position' value of node array (ar3DNodes[i].Pos).
      /// </summary>
      function  setSortedPosition       : Integer;

      /// <summary>
      ///   Get searching loop indices from sorted data array
      /// </summary>
      /// <param name="_i1">
      ///   start index
      /// </param>
      /// <param name="_i2">
      ///   end index
      /// </param>
      procedure getSortedPosition       ( var _i1         : Integer ;
                                          var _i2         : Integer
                                        ) ;

      /// <summary>
      ///   Get vertex position in sorted array.
      /// </summary>
      /// <param name="_i">
      ///   vertex position in not sorted array
      /// </param>
      function  getId                   ( const _i        : Integer
                                        ) : Integer ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Get starting point for triangulation of next contour.
      /// </summary>
      function  setNewExternalContour   : Integer ;

      /// <summary>
      ///   Check if vertex _i is equal to any of Id1 or Id2 or Id3.
      /// </summary>
      /// <param name="_i">
      ///   vertex position in ar3DNodes array
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    globals Id1, Id2, Id3 are points position in ar3DNodes array
      ///    </note>
      /// </remarks>
      function  checkCoordinates        ( const _i        : Integer
                                        ) : Boolean ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}

      /// <summary>
      ///   Distance between vertex _id and line from Id1 to Id2.
      /// </summary>
      /// <remarks>
      ///   Global Id1, Id2 are points position in T_nod array.
      /// </remarks>
      /// <param name ="_id">
      ///   vertex position in T_nod array
      /// </param>
      /// <param name ="_dis">
      ///   calculated distance
      /// </param>
      /// <returns>
      ///  -1 error, 1 point behind section, 0 point on section.
      /// </returns>
      function  distance                ( const _id       : Integer ;
                                          var   _dis      : Double
                                        ) : Integer ;

      /// <summary>
      ///   Link internal contour to the external one.
      /// </summary>
      /// <param name="_id1">
      ///   first point on contour
      /// </param>
      /// <param name="_id2">
      ///   second point on contour
      /// </param>
      /// <param name="_i">
      ///   third point on contour
      /// </param>
      /// <param name="_lnk_part">
      ///   part of shape to be linked
      /// </param>
      function  linkInternalContour     ( const _id1      : Integer ;
                                          const _id2      : Integer ;
                                          const _i        : Integer ;
                                          const _lnk_part : Integer
                                        ) : Integer ;

      /// <summary>
      ///   Find third point of triangle.
      /// </summary>
      /// <param name="_id1">
      ///   first point of triangle
      /// </param>
      /// <param name="_id2">
      ///   second point of triangle
      /// </param>
      function  getNextPoint            ( var _id1        : Integer ;
                                          var _id2        : Integer
                                        ) : Integer ;

      /// <summary>
      ///   Return winding of triangle.
      /// </summary>
      /// <param name="_id1">
      ///   first point of triangle
      /// </param>
      /// <param name="_id2">
      ///   second point of triangle
      /// </param>
      /// <param name="_id3">
      ///   third point of triangle
      /// </param>
      function  checkDirection          ( const _id1      : Integer ;
                                          const _id2      : Integer ;
                                          const _id3      : Integer
                                        ) : Integer ;

      /// <summary>
      ///   Add new triangle to array.
      /// </summary>
      /// <param name="_id1">
      ///   first point of triangle
      /// </param>
      /// <param name="_id2">
      ///   second point of triangle
      /// </param>
      /// <param name="_id3">
      ///   third point of triangle
      /// </param>
      procedure addTriangle             ( const _id1      : Integer ;
                                          const _id2      : Integer ;
                                          const _id3      : Integer
                                        ) ;

      /// <summary>
      ///   Triangulate current polygon.
      /// </summary>
      procedure search                  ;

      /// <summary>
      ///   Swap positions of two elements in array.
      /// </summary>
      /// <param name="_i1">
      ///   first element
      /// </param>
      /// <param name="_i2">
      ///   second element
      /// </param>
      function  swapNode                ( const _i1       : Integer ;
                                          const _i2       : Integer
                                        ) : Integer ;

      /// <summary>
      ///   Sort node array by Y coordinate (ascending order).
      /// </summary>
      /// <param name="_num">
      ///   array size
      /// </param>
      procedure sortNodeArray           ( const _num      : Integer
                                        ) ;

      /// <summary>
      ///   Sort node array by X coordinate (ascending order).
      /// </summary>
      /// <param name="_low">
      ///   lowest array index
      /// </param>
      /// <param name="_high">
      ///   highest array index
      /// </param>
      procedure quickSort               ( const _low      : Integer ;
                                          const _high     : Integer
                                        ) ;

      /// <summary>
      ///   Main routine controlling triangulation process.
      /// </summary>
      function  doTriangulation         : Integer ;

      /// <summary>
      ///   Check intersecting of two arcs.
      /// </summary>
      /// <param name="_x1">
      ///   X begin coordinate of given arc 1
      /// </param>
      /// <param name="_y1">
      ///   Y begin coordinate of given arc 1
      /// </param>
      /// <param name="_x2">
      ///   X end coordinate of given arc 1
      /// </param>
      /// <param name="_y2">
      ///   Y end coordinate of given arc 1
      /// </param>
      /// <param name="_x3">
      ///   X begin coordinate of given arc 2
      /// </param>
      /// <param name="_y3">
      ///   Y begin coordinate of given arc 2
      /// </param>
      /// <param name="_x4">
      ///   X end coordinate of given arc 2
      /// </param>
      /// <param name="_y4">
      ///   Y end coordinate of given arc 2
      /// </param>
      /// <param name="_xr">
      ///   X coordinate of intersection point
      /// </param>
      /// <param name="_yr">
      ///   Y coordinate of intersection point
      /// </param>
      function  intersect               ( const _x1 : Double ;
                                          const _y1 : Double ;
                                          const _x2 : Double ;
                                          const _y2 : Double ;
                                          const _x3 : Double ;
                                          const _y3 : Double ;
                                          const _x4 : Double ;
                                          const _y4 : Double ;
                                          var   _xr : Double ;
                                          var   _yr : Double
                                        ) : Integer ;
      /// <summary>
      ///   Check a part.
      /// </summary>
      function  checkPart               : Boolean ;

      /// <summary>
      ///   Add new part.
      /// </summary>
      procedure addPart                 ;

    protected
      /// <summary>
      ///   T_Tessellation class destructor.
      /// </summary>
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Value greater than zero means that shape is not completely triangulated.
      /// </summary>
      EmergencyCode : Integer;

    public
      /// <summary>
      ///   T_Tessellation class constructor.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Set initial values of necessary variables.
      /// </summary>
      procedure InitVariables           ;

      /// <summary>
      ///   Add vertex from polygon outline.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      /// <param name="_z">
      ///   Z coordinate
      /// </param>
      /// <param name="_m">
      ///   m coordinate
      /// </param>
      procedure AddVertex               ( const _part     : Integer ;
                                          const _x        : Double  ;
                                          const _y        : Double  ;
                                          const _z        : Double  ;
                                          const _m        : Double
                                        ) ; overload ;

      /// <summary>
      ///   Add vertex from polygon outline.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      /// <param name="_z">
      ///   Z coordinate
      /// </param>
      /// <param name="_m">
      ///   m coordinate
      /// </param>
      /// <param name="_i">
      ///  user defined vertex id
      /// </param>
      procedure AddVertex               ( const _part     : Integer ;
                                          const _x        : Double  ;
                                          const _y        : Double  ;
                                          const _z        : Double  ;
                                          const _m        : Double  ;
                                          const _i        : Integer
                                        ) ;  overload ;

      /// <summary>
      ///   As AddVertex but with normals and texture coordinates.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      /// <param name="_z">
      ///   Z coordinate
      /// </param>
      /// <param name="_m">
      ///   m coordinate
      /// </param>
      /// <param name="_nx">
      ///   normal X value
      /// </param>
      /// <param name="_ny">
      ///   normal Y value
      /// </param>
      /// <param name="_nz">
      ///   normal Z value
      /// </param>
      /// <param name="_u">
      ///   texture u coordinate
      /// </param>
      /// <param name="_v">
      ///   texture v coordinate
      /// </param>
      procedure AddVertexTuv            ( const _part     : Integer ;
                                          const _x        : Double  ;
                                          const _y        : Double  ;
                                          const _z        : Double  ;
                                          const _m        : Double  ;
                                          const _nx       : Single  ;
                                          const _ny       : Single  ;
                                          const _nz       : Single  ;
                                          const _u        : Single  ;
                                          const _v        : Single
                                        ) ;

      /// <summary>
      ///   Start with triangulation.
      /// </summary>
      /// <returns>
      ///   Number of triangles created.
      /// </returns>
      function  Execute                 : Integer ;

      /// <summary>
      ///   Get triangle from a list of created triangles.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_x1">
      ///   X coordinates of 1 point of triangle
      /// </param>
      /// <param name="_y1">
      ///   Y coordinates of 1 point of triangle
      /// </param>
      /// <param name="_z1">
      ///   Z coordinates of 1 point of triangle
      /// </param>
      /// <param name="_x2">
      ///   X coordinates of 2 point of triangle
      /// </param>
      /// <param name="_y2">
      ///   Y coordinates of 2 point of triangle
      /// </param>
      /// <param name="_z2">
      ///   Z coordinates of 2 point of triangle
      /// </param>
      /// <param name="_x3">
      ///   X coordinates of 3 point of triangle
      /// </param>
      /// <param name="_y3">
      ///   Y coordinates of 3 point of triangle
      /// </param>
      /// <param name="_z3">
      ///   Z coordinates of 3 point of triangle
      /// </param>
      procedure GetTriangle             ( const   _idx      : Integer ;
                                            var   _x1       : Single  ;
                                            var   _y1       : Single  ;
                                            var   _z1       : Single  ;
                                            var   _x2       : Single  ;
                                            var   _y2       : Single  ;
                                            var   _z2       : Single  ;
                                            var   _x3       : Single  ;
                                            var   _y3       : Single  ;
                                            var   _z3       : Single
                                        ) ;

      /// <summary>
      ///   Get M coordinates of a triangle.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_m1">
      ///   M coordinate of 1 point of triangle
      /// </param>
      /// <param name="_m2">
      ///   M coordinate of 2 point of triangle
      /// </param>
      /// <param name="_m3">
      ///   M coordinate of 3 point of triangle
      /// </param>
      procedure GetTriangleM            ( const   _idx      : Integer ;
                                            var   _m1       : Single  ;
                                            var   _m2       : Single  ;
                                            var   _m3       : Single
                                        ) ;

      /// <summary>
      ///   Get Ids of vertices forming a triangle.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_p1">
      ///   first point Id of triangle
      /// </param>
      /// <param name="_p2">
      ///   second point Id of triangle
      /// </param>
      /// <param name="_p3">
      ///   third point Id of triangle
      /// </param>
      procedure GetTriangleIds          ( const   _idx      : Integer  ;
                                            var   _p1       : Integer  ;
                                            var   _p2       : Integer  ;
                                            var   _p3       : Integer
                                        ) ;

      /// <summary>
      ///   Get user Ids of vertices forming a triangle.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_p1">
      ///   first point Id of triangle
      /// </param>
      /// <param name="_p2">
      ///   second point Id of triangle
      /// </param>
      /// <param name="_p3">
      ///   third point Id of triangle
      /// </param>
      procedure GetTriangleUIds         ( const   _idx      : Integer  ;
                                            var   _p1       : Integer  ;
                                            var   _p2       : Integer  ;
                                            var   _p3       : Integer
                                        ) ;


      /// <summary>
      ///   Get normals and texture coordinates of a triangle.
      /// </summary>
      /// <param name="_idx">
      ///   triangle number
      /// </param>
      /// <param name="_n1">
      ///   normal to 1 point of triangle
      /// </param>
      /// <param name="_n2">
      ///   normal to 2 point of triangle
      /// </param>
      /// <param name="_n3">
      ///   normal to 3 point of triangle
      /// </param>
      /// <param name="_t1x">
      ///   u coordinate of 1 point of triangle
      /// </param>
      /// <param name="_t1y">
      ///   v coordinate of 1 point of triangle
      /// </param>
      /// <param name="_t2x">
      ///   u coordinate of 2 point of triangle
      /// </param>
      /// <param name="_t2y">
      ///   v coordinate of 2 point of triangle
      /// </param>
      /// <param name="_t3x">
      ///   u coordinate of 3 point of triangle
      /// </param>
      /// <param name="_t3y">
      ///   v coordinate of 3 point of triangle
      /// </param>
      procedure GetTriangleTuv         ( const    _idx      : Integer  ;
                                           var    _n1       : TGIS_SingleVector ;
                                           var    _n2       : TGIS_SingleVector ;
                                           var    _n3       : TGIS_SingleVector ;
                                           var    _t1x      : Single       ;
                                           var    _t1y      : Single       ;
                                           var    _t2x      : Single       ;
                                           var    _t2y      : Single       ;
                                           var    _t3x      : Single       ;
                                           var    _t3y      : Single
                                        ) ;
  end ;

  T_FaceRecord = record
    I : array [0..2] of Integer ;
    N : TGIS_SingleVector ;
  end ;
  T_FaceRecordArray = array of T_FaceRecord ;
  T_FaceVertex = record
    P : TGIS_SingleVector ;
    N : TGIS_SingleVector ;
  end ;
  T_FaceVertexArray = array of T_FaceVertex ;

  T_MeshBuilder = class ( TGIS_ObjectDisposable )
    {$IFDEF OXYGENE} unit {$ELSE} private {$ENDIF}
      vertexLookup : TDictionary<TGIS_SingleVector, Integer> ;
      faceList     : T_FaceRecordArray ;
      numFaces     : Integer ;
      unqPointList : T_FaceVertexArray ;
      allPointList : T_FaceVertexArray ;
      numAllPoints : Integer ;
      numUnqPoints : Integer ;
    protected
      procedure doDestroy ; override ;
    public
      constructor Create ;

      procedure Reset     ;
      procedure Prepare   ( const _triangles : Integer
                          ) ;
      procedure AddVertex ( const _v   : TGIS_SingleVector ;
                            const _n   : TGIS_SingleVector
                          ) ;
      procedure AddFace   ( const _v1  : TGIS_SingleVector ;
                            const _v2  : TGIS_SingleVector ;
                            const _v3  : TGIS_SingleVector ;
                            const _n   : TGIS_SingleVector
                          ) ;
      procedure BuildFaces ;
 end ;

 T_Vector3Utils = class
  public
    class procedure v3Add(
        out _vOut : TGIS_SingleVector ;
      const _v1   : TGIS_SingleVector ;
      const _v2   : TGIS_SingleVector
    ) ;
    class procedure v3Subtract(
        out _vOut : TGIS_SingleVector ;
      const _v1   : TGIS_SingleVector ;
      const _v2   : TGIS_SingleVector
    ) ;
    class procedure v3Cross(
      out   _vOut : TGIS_SingleVector ;
      const _v1   : TGIS_SingleVector ;
      const _v2   : TGIS_SingleVector
    ) ;
    class procedure v3Normalize(
        out _vOut : TGIS_SingleVector ;
      const _v    : TGIS_SingleVector
    ) ;
    class function triangleNormal(
      const _v0 : TGIS_SingleVector ;
      const _v1 : TGIS_SingleVector ;
      const _v2 : TGIS_SingleVector
    ) : TGIS_SingleVector ;

 end ;

//==============================================================================
// T_Vector3Utils
//==============================================================================

  class procedure T_Vector3Utils.v3Add(
      out _vOut : TGIS_SingleVector ;
    const _v1   : TGIS_SingleVector ;
    const _v2   : TGIS_SingleVector
  ) ;
  begin
    _vOut.X := _v1.X + _v2.X ;
    _vOut.Y := _v1.Y + _v2.Y ;
    _vOut.Z := _v1.Z + _v2.Z ;
  end ;

  class procedure T_Vector3Utils.v3Subtract(
      out _vOut : TGIS_SingleVector ;
    const _v1   : TGIS_SingleVector ;
    const _v2   : TGIS_SingleVector
  ) ;
  begin
    _vOut.X:= _v1.X - _v2.X ;
    _vOut.Y:= _v1.Y - _v2.Y ;
    _vOut.Z:= _v1.Z - _v2.Z ;
  end ;

  class procedure T_Vector3Utils.v3Cross(
    out   _vOut : TGIS_SingleVector ;
    const _v1   : TGIS_SingleVector ;
    const _v2   : TGIS_SingleVector
  ) ;
  begin
    _vOut.X := _v1.Y * _v2.Z - _v1.Z * _v2.Y ;
    _vOut.Y := _v1.Z * _v2.X - _v1.X * _v2.Z ;
    _vOut.Z := _v1.X * _v2.Y - _v1.Y * _v2.X ;
  end ;

  class procedure T_Vector3Utils.v3Normalize(
      out _vOut : TGIS_SingleVector ;
    const _v    : TGIS_SingleVector
  ) ;
  var
    l : Double ;
  begin
    l := Sqrt(Sqr(_v.X) + Sqr(_v.Y) + Sqr(_v.Z)) ;
    if l = 0 then begin
      _vOut.Y := 1 ;
      exit;
    end ;
    _vOut.X := _vOut.X / l ;
    _vOut.Y := _vOut.Y / l ;
    _vOut.Z := _vOut.Z / l ;
  end ;

  class function T_Vector3Utils.triangleNormal(
    const _v0 : TGIS_SingleVector ;
    const _v1 : TGIS_SingleVector ;
    const _v2 : TGIS_SingleVector
  ) : TGIS_SingleVector ;
  var
    vout, vv1, vv2 : TGIS_SingleVector ;
  begin
    T_Vector3Utils.v3Subtract( vv1, _v1, _v0 ) ;
    T_Vector3Utils.v3Subtract( vv2, _v2, _v0 ) ;
    T_Vector3Utils.v3Cross( vout, vv1, vv2 ) ;
    T_Vector3Utils.v3Normalize( vout, vout ) ;
    Result := vout ;
  end;

//==============================================================================
// T_Tessellation
//==============================================================================

  constructor T_Tessellation.Create ;
  begin
    inherited ;
  end ;

  procedure T_Tessellation.doDestroy ;
  begin
    SetLength( ar3DNodes   , 0 ) ;
    SetLength( arPnts    , 0 ) ;
    SetLength( arTriangles    , 0 ) ;
    SetLength( arParts   , 0 ) ;
    SetLength( arTrianglesM   , 0 ) ;
    SetLength( arTrianglesIds , 0 ) ;

    inherited ;
  end ;

  procedure T_Tessellation.InitVariables ;
  begin
    firstNode      := 0 ;
    curNode        := 0 ;
    extNode        := 0 ;
    curPart        := 0 ;
    triNum         := 0 ;
    nodNum         := 0 ;
    partNum        := 0 ;
    minX           := 1.0e37 ;
    startPnt       := 0 ;
    EmergencyCode := 0 ;
    SetLength( ar3DNodes   , 0  ) ;
    SetLength( ar3DNodes   , 32 ) ;
    SetLength( arPnts    , 32 ) ;
    SetLength( arTriangles    , 32 ) ;
    SetLength( arTrianglesM   , 32 ) ;
    SetLength( arTrianglesIds , 32 ) ;
    SetLength( arParts   , 1  ) ;
    {$IFDEF GIS_NORECORDS}
      arParts[0] := new T_Part() ;
    {$ENDIF}
    arParts[0].Nr  := 0 ;
    arParts[0].Fst := 0 ;
  end ;

  function T_Tessellation.intersect(
    const _x1 : Double ;
    const _y1 : Double ;
    const _x2 : Double ;
    const _y2 : Double ;
    const _x3 : Double ;
    const _y3 : Double ;
    const _x4 : Double ;
    const _y4 : Double ;
    var   _xr : Double ;
    var   _yr : Double
  ) : Integer ;
  var
    la, mab, lb : Double ;
    ua, ub      : Double ;
  begin
    la  := (_x4 - _x3)*(_y1 - _y3) - (_y4 - _y3)*(_x1 - _x3) ;
    lb  := (_x2 - _x1)*(_y1 - _y3) - (_y2 - _y1)*(_x1 - _x3) ;
    mab := (_y4 - _y3)*(_x2 - _x1) - (_x4 - _x3)*(_y2 - _y1) ;

    if mab <> 0.0 then begin
      ua := la/mab ;
      ub := lb/mab ;
      if(ua >= 0) and (ua <= 1) and (ub >= 0) and (ub <= 1) then begin
        _xr := _x1 + ua*(_x2 - _x1) ;
        _yr := _y1 + ua*(_y2 - _y1) ;
        Result := 0 ;
      end
      else begin
        _xr := _x1 + ua*(_x2 - _x1) ;
        _yr := _y1 + ua*(_y2 - _y1) ;
        Result := 1 ;
      end
    end
    else begin
        _xr := 0 ;
        _yr := 0 ;
        Result := 1 ;
    end ;
  end ;

  procedure T_Tessellation.addPart ;
  begin
    SetLength( arParts   , high(arParts) + 2 ) ;
    arParts[high(arParts)].Nr  := high(arParts) ;
    arParts[high(arParts)].Fst := nodNum ;
    arParts[high(arParts)].Spt := startPnt ;
  end ;

  function T_Tessellation.checkPart : Boolean ;
  var
    p : Integer ;

    procedure reset_part ;
    var
      p : Integer ;
    begin
      p := high( arParts ) ;
      if p > 0 then begin
        firstNode := arParts[p-1].Fst ;
        nodNum    := arParts[p].Fst ;
        startPnt  := arParts[p].Spt ;
      end
      else begin
        firstNode := 0 ;
        nodNum    := 0 ;
        startPnt  := 0 ;
        extNode   := 0 ;
      end ;
    end ;

    procedure check_selfcrossing ;
    var
      i, j, k : Integer ;
      xr, yr  : Double ;
      counter : Integer ;
    begin
      k := 3 ;
      while True do begin
        counter := 0 ;
        if (arParts[p].Lst - arParts[p].Fst ) > k then begin
          i := arParts[p].Fst ;
          while True do begin
            if not ((ar3DNodes[i].X = ar3DNodes[i+1].X) and
                    (ar3DNodes[i].Y = ar3DNodes[i+1].Y)) then
            if intersect(
                   ar3DNodes[i].X    , ar3DNodes[i].Y    , ar3DNodes[i+1].X, ar3DNodes[i+1].Y,
                   ar3DNodes[i+k-1].X, ar3DNodes[i+k-1].Y, ar3DNodes[i+k].X, ar3DNodes[i+k].Y,
                   xr, yr ) = 0 then begin
              for j := 1 to k -1 do begin
                ar3DNodes[i+j].X := xr ;
                ar3DNodes[i+j].Y := yr ;
              end ;
              inc( counter ) ;
            end ;
            inc( i ) ;
            if i >= arParts[p].Lst-k then
              break ;
          end ;
        end ;

        case k of
          3 : if counter = 0 then
                break
              else
                inc( k ) ;
          8 : break ;
          else inc( k ) ;
        end ;
      end ;
    end ;

    function calc_area : Integer ;
    var
      pnt_no       : Integer ;
      n_pt         : Integer ;
      darea        : Double  ;
      line_ax      : Double ;
      line_bx      : Double ;
      originx      : Double ;
      line_ay      : Double ;
      line_by      : Double ;
      originy      : Double ;
      first        : Boolean ;
    begin
      originx := 0 ;
      originy := 0 ;
      darea := 0 ;
      first := True ;
      for pnt_no := arParts[p].Fst to arParts[p].Lst - 1 do begin // all points
        n_pt := pnt_no +1;
        line_ax := ar3DNodes[pnt_no].X ;
        line_ay := ar3DNodes[pnt_no].Y ;
        if (line_ax > 1e30) or (line_ay > 1e30) then
          continue ;
        // to avoid big numbers - make it relative to some point in the geometry
        if first then begin
          originx := line_ax ;
          originy := line_ay ;
          first  := False ;
        end ;

        line_ax := line_ax - originx;
        line_ay := line_ay - originy;

        line_bx := ar3DNodes[n_pt].X ;
        line_by := ar3DNodes[n_pt].Y ;
        if (line_bx > 1e30) or (line_by > 1e30) then
          continue ;

        line_bx := line_bx - originx;
        line_by := line_by - originy;
        darea := darea + (line_by*line_ax -line_ay*line_bx) / 2 ;
      end ;

      if darea < 0 then
        Result := -1
      else if darea > 0 then
        Result := 1
      else
        Result := 0 ;
    end ;

  begin
    Result := False ;
    p := high( arParts ) ;
    arParts[p].Lst := nodNum -1 ;
    if (nodNum - arParts[p].Fst < 3) then begin
      reset_part ;
      exit;
    end ;

    case calc_area of
      -1 : arParts[p].Tp := True ;
       1 : arParts[p].Tp := False ;
       else begin
         reset_part ;
         exit;
       end ;
    end ;

    check_selfcrossing ;

    Result := True ;
  end ;

  procedure T_Tessellation.AddVertex(
    const _part : Integer ;
    const _x    : Double ;
    const _y    : Double ;
    const _z    : Double ;
    const _m    : Double
  ) ;
  begin
    if partNum < _part then
       partNum := _part ;

    if curPart <> _part  then begin
      if checkPart then begin
        addPart ;
        ar3DNodes[firstNode].Prev := nodNum - 1 ;
        ar3DNodes[nodNum - 1].Next := firstNode ;
        firstNode := nodNum ;
        curPart   := _part ;
      end
      else begin
        firstNode := nodNum ;
        curPart   := _part ;
      end ;
    end ;

    if high(ar3DNodes) + 1 = nodNum then begin
      SetLength( ar3DNodes, 2*(high(ar3DNodes) + 1) ) ;
      SetLength( arPnts , 2*(high(arPnts ) + 1) ) ;
    end ;

    {$IFDEF GIS_NORECORDS}
      ar3DNodes[nodNum] := new T_3DNode() ;
    {$ENDIF}
    ar3DNodes[nodNum].Id   := nodNum     ;
    ar3DNodes[nodNum].Part := high( arParts ) ;
    ar3DNodes[nodNum].X    := _x         ;
    ar3DNodes[nodNum].Y    := _y         ;
    ar3DNodes[nodNum].Z    := _z         ;
    ar3DNodes[nodNum].M    := _m         ;
    ar3DNodes[nodNum].Tu   := 0          ;
    ar3DNodes[nodNum].Tv   := 0          ;
    ar3DNodes[nodNum].Prev := nodNum - 1 ;
    ar3DNodes[nodNum].Next := nodNum + 1 ;
    ar3DNodes[nodNum].Sel  := 0          ;
    ar3DNodes[nodNum].Uid  := -1         ;

    if minX > _x  then begin
      minX := _x ;
      startPnt := nodNum ;
    end ;

    if high( arParts ) = 0 then
      inc(extNode) ;

    {$IFDEF GIS_NORECORDS}
      arPnts[nodNum] := new T_Pnt() ;
    {$ENDIF}
    arPnts[nodNum].Id   := nodNum ;
    arPnts[nodNum].X    := _x     ;
    arPnts[nodNum].Y    := _y     ;

    inc(nodNum) ;
  end ;

  procedure T_Tessellation.AddVertex(
    const _part : Integer ;
    const _x    : Double ;
    const _y    : Double ;
    const _z    : Double ;
    const _m    : Double ;
    const _i    : Integer
  ) ;
  begin
    if partNum < _part then
       partNum := _part ;

    if curPart <> _part  then begin
      if checkPart then begin
        addPart ;
        ar3DNodes[firstNode].Prev := nodNum - 1 ;
        ar3DNodes[nodNum - 1].Next := firstNode ;
        firstNode := nodNum ;
        curPart   := _part ;
      end
      else begin
        firstNode := nodNum ;
        curPart   := _part ;
      end ;
    end ;

    if high(ar3DNodes) + 1 = nodNum then begin
      SetLength( ar3DNodes, 2*(high(ar3DNodes) + 1) ) ;
      SetLength( arPnts , 2*(high(arPnts ) + 1) ) ;
    end ;

    {$IFDEF GIS_NORECORDS}
      ar3DNodes[nodNum] := new T_3DNode() ;
    {$ENDIF}
    ar3DNodes[nodNum].Id   := nodNum     ;
    ar3DNodes[nodNum].Part := high( arParts ) ;
    ar3DNodes[nodNum].X    := _x         ;
    ar3DNodes[nodNum].Y    := _y         ;
    ar3DNodes[nodNum].Z    := _z         ;
    ar3DNodes[nodNum].M    := _m         ;
    ar3DNodes[nodNum].Tu   := 0          ;
    ar3DNodes[nodNum].Tv   := 0          ;
    ar3DNodes[nodNum].Prev := nodNum - 1 ;
    ar3DNodes[nodNum].Next := nodNum + 1 ;
    ar3DNodes[nodNum].Sel  := 0          ;
    ar3DNodes[nodNum].Uid  := _i         ;

    if minX > _x  then begin
      minX := _x ;
      startPnt := nodNum ;
    end ;

    if high( arParts ) = 0 then
      inc(extNode) ;

    {$IFDEF GIS_NORECORDS}
      arPnts[nodNum] := new T_Pnt() ;
    {$ENDIF}
    arPnts[nodNum].Id   := nodNum ;
    arPnts[nodNum].X    := _x     ;
    arPnts[nodNum].Y    := _y     ;

    inc(nodNum) ;
  end ;

  procedure T_Tessellation.AddVertexTuv(
    const _part     : Integer ;
    const _x        : Double  ;
    const _y        : Double  ;
    const _z        : Double  ;
    const _m        : Double  ;
    const _nx       : Single  ;
    const _ny       : Single  ;
    const _nz       : Single  ;
    const _u        : Single  ;
    const _v        : Single
  ) ;
  begin
    if partNum < _part then
       partNum := _part ;

    if curPart <> _part  then begin
      if checkPart then begin
        addPart ;
        ar3DNodes[firstNode].Prev := nodNum - 1 ;
        ar3DNodes[nodNum - 1].Next := firstNode ;
        firstNode := nodNum ;
        curPart   := _part ;
      end
      else begin
        firstNode := nodNum ;
        curPart   := _part ;
      end ;
    end ;

    if high(ar3DNodes) + 1 = nodNum then begin
      SetLength( ar3DNodes, 2*(high(ar3DNodes) + 1) ) ;
      SetLength( arPnts , 2*(high(arPnts ) + 1) ) ;
    end ;
    {$IFDEF GIS_NORECORDS}
      ar3DNodes[nodNum] := new T_3DNode() ;
    {$ENDIF}
    ar3DNodes[nodNum].Id   := nodNum     ;
    ar3DNodes[nodNum].Part := high( arParts ) ;
    ar3DNodes[nodNum].X    := _x         ;
    ar3DNodes[nodNum].Y    := _y         ;
    ar3DNodes[nodNum].Z    := _z         ;
    ar3DNodes[nodNum].M    := _m         ;
    ar3DNodes[nodNum].Nx   := _nx        ;
    ar3DNodes[nodNum].Ny   := _ny        ;
    ar3DNodes[nodNum].Nz   := _nz        ;
    ar3DNodes[nodNum].Tu   := _u         ;
    ar3DNodes[nodNum].Tv   := _v         ;
    ar3DNodes[nodNum].Prev := nodNum - 1 ;
    ar3DNodes[nodNum].Next := nodNum + 1 ;
    ar3DNodes[nodNum].Sel  := 0          ;

    if minX > _x  then begin
      minX := _x ;
      startPnt := nodNum ;
    end ;

    if high( arParts ) = 0 then
      inc(extNode) ;

    {$IFDEF GIS_NORECORDS}
      arPnts[nodNum] := new T_Pnt() ;
    {$ENDIF}
    arPnts[nodNum].Id   := nodNum ;
    arPnts[nodNum].X    := _x     ;
    arPnts[nodNum].Y    := _y     ;

    inc(nodNum) ;
  end ;

  function T_Tessellation.doTriangulation : Integer ;
  var
    p, aa : Integer ;
  begin
    id1 := startPnt ;
    p   := ar3DNodes[startPnt].Part ;
    curPart   := p ;

    if p <> 0  then begin
      extNode := 0 ;
      for aa := 0 to nodNum-1 do
        if ar3DNodes[aa].Part = 0 then
          ar3DNodes[aa].Part := p
        else if ar3DNodes[aa].Part = p then begin
          ar3DNodes[aa].Part := 0 ;
          inc( extNode ) ;
        end ;
    end ;
      id2 := ar3DNodes[startPnt].Next ;
      search ;
    while True do begin
      id1 := ar3DNodes[id1].Next ;
      if ar3DNodes[id1].Sel = 0 then
        begin
          if extNode <= 2  then
            break ;
          id2 := ar3DNodes[id1].Next ;
          search ;
          if triNum <> TrianglesCount  then begin
              TrianglesCount := triNum ;
              EmergencyCode := 0 ;
          end
          else begin
            if EmergencyCode > extNode then begin
              if extNode <= 3 then
                EmergencyCode := 0 ;
                break ;
            end ;
              inc(EmergencyCode) ;
          end ;
        end
      else
          id1 := ar3DNodes[id1].Next ;
    end ;

    // External island exists
    if extNode <> curNode then begin
      curNode := curNode - extNode ;
      if curNode >= 3  then begin
          startPnt := setNewExternalContour ;
          if startPnt <> -1 then
            doTriangulation ;
      end ;
    end ;
    Result := triNum ;
  end ;

  function T_Tessellation.Execute : Integer;
  begin
    if nodNum < 3 then begin
      Result := 0 ;
      exit ;
    end ;

    ar3DNodes[firstNode].Prev := nodNum - 1 ;
    ar3DNodes[nodNum - 1].Next := firstNode ;
    curNode := nodNum ;
    baseNum := nodNum ;

    // Sort arPnts array by X
    quickSort(0,nodNum-1) ;
    // Additionally sort arPnts array by Y
    sortNodeArray(nodNum) ;
    // Set sorted position parameter
    // and check one point touch
    setSortedPosition ;

    // Start with triangulation
    TrianglesCount := 0 ;
    doTriangulation ;

    EmergencyCode := 0 ;
    Result := triNum ;
  end ;

  function T_Tessellation.setSortedPosition : Integer;
  var
    i, no : Integer ;
    x1, y1, x2, y2 : Double ;
  begin
    no := 0 ;
    x1 := arPnts[0].X ;
    y1 := arPnts[0].Y ;
    ar3DNodes[arPnts[0].Id].Pos := 0 ;
    for i := 1 to nodNum-1 do begin
      ar3DNodes[arPnts[i].Id].Pos := i ;
      x2 := arPnts[i].X ;
      y2 := arPnts[i].Y ;
      if ( x1 = x2 ) and ( y1 = y2 ) then begin
        inc(no)  ;
        x1 := x2 ;
        y1 := y2 ;
      end
      else begin
        x1 := x2 ;
        y1 := y2 ;
      end ;
    end ;
    Result := no ;
  end ;

  procedure T_Tessellation.getSortedPosition(
    var _i1 : Integer ;
    var _i2 : Integer
  ) ;
  var
    p1, p2, p3 : Integer ;
  begin
    p1 := ar3DNodes[id1].Pos ;
    p2 := ar3DNodes[id2].Pos ;
    p3 := ar3DNodes[id3].Pos ;
    if p1 < p2 then begin
      if p1 < p3 then
        _i1 := p1
      else
        _i1 := p3 ;

      if p2 < p3 then
        _i2 := p3
      else
        _i2 := p2 ;
    end
    else begin
      if p1 > p3 then
        _i2 := p1
      else
        _i2 := p3 ;

      if p2 < p3 then
        _i1 := p2
      else
        _i1 := p3 ;
    end ;
  end ;

  function T_Tessellation.getId(
    const _i : Integer
  ) : Integer ;
  begin
    Result := arPnts[_i].Id ;
  end ;

  function T_Tessellation.setNewExternalContour  : Integer;
  var
    i,
    wrk : Integer ;
  begin
  for i := 0 to nodNum-1 do begin
    if ar3DNodes[i].Part <> 0 then
      break ;
  end ;

  if i = nodNum then begin
    Result := -1 ;
    exit ;
  end ;

    ar3DNodes[i].Part := 0 ;
    i := ar3DNodes[i].Next ;
    extNode := 1 ;
    wrk := ar3DNodes[i].Id ;

    while True do begin
      if ar3DNodes[i].Next = wrk then
        break ;
      ar3DNodes[i].Part := 0 ;
      inc(extNode) ;
      i := ar3DNodes[i].Next ;
    end ;
    Result := i ;
  end ;

  function T_Tessellation.checkCoordinates(
    const _i : Integer
  ) : Boolean ;
  var
    x1, y1 : Double ;
  begin
    x1 := ar3DNodes[_i].X ;
    y1 := ar3DNodes[_i].Y ;
    if (x1 = ar3DNodes[id1].X) and
       (y1 = ar3DNodes[id1].Y) then begin
      Result := True ;
      exit ;
    end ;
    if (x1 = ar3DNodes[id2].X) and
       (y1 = ar3DNodes[id2].Y) then begin
      Result := True ;
      exit ;
    end ;
    if (x1 = ar3DNodes[id3].X) and
       (y1 = ar3DNodes[id3].Y) then begin
      Result := True ;
      exit ;
    end ;
    Result := False ;
  end ;

  function T_Tessellation.distance(
    const _id  : Integer ;
    var   _dis : Double
  ) : Integer ;
  var
    a, b, aa, bb, X, Y, d  : Double ;
    x1, y1, x2, y2, x3, y3 : Double ;
    xr, yr, xof, yof       : Double ;
  begin
    x1 := ar3DNodes[id1].X ;
    y1 := ar3DNodes[id1].Y ;
    x2 := ar3DNodes[id2].X ;
    y2 := ar3DNodes[id2].Y ;
    x3 := ar3DNodes[_id].X ;
    y3 := ar3DNodes[_id].Y ;

    xof := x1 ;
    yof := y1 ;
    x1 := 0.0 ;
    y1 := 0.0 ;
    x2 := x2 - xof ;
    y2 := y2 - yof ;
    x3 := x3 - xof ;
    y3 := y3 - yof ;

    // -1 error, 1 point behind section, 0 point on section
    if x1 > x2 then begin
      d  := x2 ;
      x2 := x1 ;
      x1 := d  ;
      d  := y2 ;
      y2 := y1 ;
      y1 := d  ;
    end ;

    if x1 = x2 then begin
      if y1 = y2 then begin
        Result := -1 ;
        exit ;
      end ;
      yr := y3 ;
      _dis := Sqrt((x3-x1)*(x3-x1)) ;
      if ((yr>y1)and(yr<y2)) or ((yr>y2)and(yr<y1)) then begin
        Result := 0 ;
        exit ;
      end
      else begin
        Result := 1 ;
        exit ;
      end ;
    end ;
      if y1 = y2 then begin
      xr  :=  x3 ;
      _dis := Sqrt((y3-y1)*(y3-y1)) ;
      if ( xr > x1 ) and ( xr < x2 ) then begin
        Result := 0 ;
        exit ;
      end
      else begin
        Result := 1 ;
        exit ;
      end ;
    end ;
    a  := (y1-y2)/(x1-x2) ;
    b  := -1.0*a*x1+y1 ;
    aa := -1.0/a ;
    bb := y3-aa*x3 ;
    X  := (bb-b)/(a-aa) ;
    Y  := a*X+b ;
    xr := X ;
    _dis := Sqrt((y3-Y)*(y3-Y)+(x3-X)*(x3-X)) ;
    if (xr > x1) and (xr < x2) then begin
      Result := 0 ;
      exit ;
    end ;
    Result := 1 ;
  end ;

  function T_Tessellation.linkInternalContour(
    const _id1      : Integer ;
    const _id2      : Integer ;
    const _i        : Integer ;
    const _lnk_part : Integer
  ) : Integer ;
  var
    k, wrk, store_prev_i, store_next_id2: Integer ;
  begin
    store_next_id2 := ar3DNodes[_id2].Next ;
    store_prev_i := ar3DNodes[_i].Prev ;
    //update internal contour point attributes
    k := _i ;
    wrk := ar3DNodes[k].Id ;
    while True do begin
      ar3DNodes[k].Part := _lnk_part ;
      k := ar3DNodes[k].Next ;
      if _lnk_part = 0 then
       inc(extNode) ;
      if wrk = k then
        break ;
      if extNode > nodNum then begin
        Result := -1 ;  // improper data, terminate triangulation
        exit;
      end ;
    end ;

    // i.prev attribute should point to Id2
    ar3DNodes[_i].Prev := _id2 ;
    // Id2.next should point to 'i'
    ar3DNodes[_id2].Next := _i ;
    // point previously pointed to 'i' should point to just added node
    ar3DNodes[store_prev_i].Next := nodNum ;

    // add new internal contour point as a link point to external contour
    SetLength( ar3DNodes, nodNum + 1 ) ;
    ar3DNodes[nodNum] := ar3DNodes[_i] ;
    // set point attributes
    ar3DNodes[nodNum].Id   := nodNum ;
    ar3DNodes[nodNum].Part := _lnk_part ;
    ar3DNodes[nodNum].Prev := store_prev_i ;
    ar3DNodes[nodNum].Next := nodNum+1 ;
    inc(nodNum) ;
    inc(curNode) ;
    if _lnk_part = 0 then
      inc(extNode) ;

    // add new external contour point as a link point to internal contour
    SetLength( ar3DNodes, nodNum + 1 ) ;
    ar3DNodes[nodNum] := ar3DNodes[_id2] ;
    // set point attributes
    ar3DNodes[nodNum].Id   := nodNum ;
    ar3DNodes[nodNum].Part := _lnk_part ;
    ar3DNodes[nodNum].Prev := nodNum -1 ;
    ar3DNodes[nodNum].Next := store_next_id2 ;
    ar3DNodes[store_next_id2].Prev := nodNum ;
    inc(nodNum) ;
    inc(curNode) ;
    if _lnk_part = 0 then
      inc(extNode) ;

   Result := _i ;
  end ;

  function T_Tessellation.getNextPoint(
    var _id1 : Integer ;
    var _id2 : Integer
  ) : Integer ;
  var
    i, i1, i2, id, cd, internal_found, nearest: Integer;
    dst, dis: Double;
    x1, y1, x2, y2, x3, y3, _x, _y : Double ;

    function isPntInTriangle : Boolean ;
    var
      d, a, b, c : Double ;
    begin
      Result := False ;

      d := ((y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3)) ;
      if d = 0 then
        exit ;

      a := ((y2 - y3)*(_x - x3) + (x3 - x2)*(_y - y3)) / d ;
      if (a < 0) or (a > 1) then
        exit;

      b := ((y3 - y1)*(_x - x3) + (x1 - x3)*(_y - y3)) / d ;
      if (b < 0) or (b > 1) then
        exit;

      c := 1 - a - b ;
      if (c < 0) or (c > 1) then
        exit;

      Result := True
    end ;

  begin
    internal_found := 0 ;
    nearest := 0 ;
    dst := 1.0e37 ;

    while True do begin
      id3 := ar3DNodes[_id2].Next ;
      if ar3DNodes[id3].Sel = 0 then
         break ;
      _id2 := id3 ;
    end ;
    if (_id1 = id3) or (_id1 = _id2) then begin
      Result := -1 ;
      exit ;
    end ;

    x1 := ar3DNodes[_id1].X ;
    y1 := ar3DNodes[_id1].Y ;
    x2 := ar3DNodes[_id2].X ;
    y2 := ar3DNodes[_id2].Y ;
    x3 := ar3DNodes[ id3].X ;
    y3 := ar3DNodes[ id3].Y ;

    getSortedPosition(i1, i2) ;
    cd := checkDirection(_id1, _id2, id3) ;
    if cd = -1 then begin
      Result := -1 ;
      exit ;
    end
    else begin
      for i := i1 to i2-1 do begin
        id := getId(i) ;
        _x := ar3DNodes[id].X ;
        _y := ar3DNodes[id].Y ;
        if ar3DNodes[id].Sel = 0  then
          if not checkCoordinates(id) then
            if isPntInTriangle then begin
                if ar3DNodes[id].Part = 0 then begin// is external
                  Result := -1 ;
                  exit ;
                end
                else begin // is internal
                  if (ar3DNodes[id].Sel = 0) and
                    (not arParts[ar3DNodes[id].Part].Tp) then begin
                    internal_found := 1 ;
                    nearest := i ;
                  end ;
                end ;
            end ;
      end ;

      if baseNum < nodNum then begin
        i1 := baseNum;
        i2 := nodNum;
        for i := i1 to i2-1 do begin
          _x := ar3DNodes[i].X ;
          _y := ar3DNodes[i].Y ;
          if ar3DNodes[i].Sel = 0 then
            if not checkCoordinates(i) then
              if isPntInTriangle then begin
                  if ar3DNodes[i].Part = 0 then begin
                    Result := -1 ;
                    exit ;
                  end
                  else begin // is internal
                    id := getId(i) ;
                    if (ar3DNodes[id].Sel = 0) and
                     (not arParts[ar3DNodes[id].Part].Tp) then begin
                      internal_found := 1 ;
                      break ;
                    end ;
                  end ;
              end ;
        end ; // for
      end ;
    end ;

    // internal point found
    if internal_found = 1 then begin
      // Set basic distance
      distance(getId(nearest), dst) ;
      getSortedPosition(i1, i2) ;
      for i := i1 to i2-1 do begin
        id := getId(i) ;
        _x := ar3DNodes[id].X ;
        _y := ar3DNodes[id].Y ;
        if not (checkCoordinates(id) or (ar3DNodes[id].Sel=1)) then
          if isPntInTriangle then begin
            if ar3DNodes[id].Part <> 0 then begin
              distance(id, dis) ;
              if dis < dst then begin
                dst := dis ;
                nearest := i ;
              end ;
            end ;
          end ;
      end ;
        // link internal contour to the external one between Id2 & nearest
        id3 := linkInternalContour(_id1, _id2, getId(nearest), 0) ;
        Result := id3 ;
        exit ;
      end ;
    Result := id3 ;
  end ;

  function T_Tessellation.checkDirection(
    const _id1 : Integer ;
    const _id2 : Integer ;
    const _id3 : Integer
  ) : Integer ;
  var
    area : Double;
  begin
    area := ((ar3DNodes[_id2].X - ar3DNodes[_id1].X) *
             (ar3DNodes[_id3].Y - ar3DNodes[_id1].Y))
          - ((ar3DNodes[_id3].X - ar3DNodes[_id1].X) *
             (ar3DNodes[_id2].Y - ar3DNodes[_id1].Y)) ;
    if area > 0 then begin
      Result := -1 ;    // counterclockwise
      exit ;
    end
    else
    if area = 0 then begin
      Result := 0 ; // collinear
      exit ;
    end
    else
      Result := 1 ; // clockwise
  end ;

  procedure T_Tessellation.addTriangle(
    const _id1 : Integer ;
    const _id2 : Integer ;
    const _id3 : Integer
  ) ;
  begin
    ar3DNodes[_id2].Sel  := 1 ;
    ar3DNodes[_id1].Next := ar3DNodes[_id3].Id ;
    ar3DNodes[_id3].Prev := ar3DNodes[_id1].Id ;

    if high(arTriangles) + 1 = triNum then begin
      SetLength( arTriangles    , 2*(high(arTriangles)    + 1) ) ;
      SetLength( arTrianglesM   , 2*(high(arTrianglesM)   + 1) ) ;
      SetLength( arTrianglesIds , 2*(high(arTrianglesIds) + 1) ) ;
    end ;

    {$IFDEF GIS_NORECORDS}
      arTriangles[triNum] := new T_Triangle() ;
    {$ENDIF}
    arTriangles[triNum].X1  := ar3DNodes[_id1].X ;
    arTriangles[triNum].Y1  := ar3DNodes[_id1].Y ;
    arTriangles[triNum].Z1  := ar3DNodes[_id1].Z ;
    arTriangles[triNum].X2  := ar3DNodes[_id2].X ;
    arTriangles[triNum].Y2  := ar3DNodes[_id2].Y ;
    arTriangles[triNum].Z2  := ar3DNodes[_id2].Z ;
    arTriangles[triNum].X3  := ar3DNodes[_id3].X ;
    arTriangles[triNum].Y3  := ar3DNodes[_id3].Y ;
    arTriangles[triNum].Z3  := ar3DNodes[_id3].Z ;

    {$IFDEF GIS_NORECORDS}
      arTrianglesM[triNum] := new TGIS_SingleVector() ;
    {$ENDIF}
    arTrianglesM[triNum].X  := ar3DNodes[_id1].M ;
    arTrianglesM[triNum].Y  := ar3DNodes[_id2].M ;
    arTrianglesM[triNum].Z  := ar3DNodes[_id3].M ;

    {$IFDEF GIS_NORECORDS}
      arTrianglesIds[triNum] := new T_Ids() ;
    {$ENDIF}
    arTrianglesIds[triNum].Id1 := _id1 ;
    arTrianglesIds[triNum].Id2 := _id2 ;
    arTrianglesIds[triNum].Id3 := _id3 ;

    inc(triNum)  ;
    dec(curNode) ;
    dec(extNode) ;
  end ;

  procedure T_Tessellation.search ;
  begin
    while True do begin
      id3 := getNextPoint(id1, id2) ;
      if id3 = -1 then begin
        id2 := id1 ;
        id1 := ar3DNodes[id1].Prev ;
        id3 := getNextPoint(id1, id2) ;
        if id3 = -1 then begin
          id2 := id1 ;
          id1 := ar3DNodes[id1].Prev ;
          id3 := getNextPoint(id1, id2) ;
          if id3 = -1 then
            exit ;
        end ;
      end ;
      addTriangle(id1, id2, id3) ;
      if extNode = 2 then
        exit ;

      id2 := id1 ;
      while True do begin
        id1 := ar3DNodes[id2].Prev ;
        if ar3DNodes[id1].Sel = 0 then
          break ;
        id2 := id1 ;
      end ;

      id3 := getNextPoint(id1, id2) ;
      if id3 = -1 then begin
        id1 := id2 ;
        id2 := ar3DNodes[id2].Next ;
        id3 := getNextPoint(id1, id2) ;
        if id3 = -1 then begin
          id1 := id2 ;
          id2 := ar3DNodes[id2].Next ;
          id3 := getNextPoint(id1, id2) ;
          if id3 = -1 then
            exit ;
        end ;
      end ;
      addTriangle(id1, id2, id3) ;
      id2 := id3 ;
      if extNode = 2 then
        exit ;
    end ;
  end ;

  function T_Tessellation.swapNode(
    const _i1 : Integer ;
    const _i2 : Integer
  ) : Integer ;
  var
    i,
    irob : Integer ;
    yp,
    yp1,
    rob  : Double ;
  begin
    for i := _i1 to _i2-1 do begin
      yp  := arPnts[i].Y   ;
      yp1 := arPnts[i+1].Y ;
      if yp > yp1 then  begin
        rob           := yp1           ;
        arPnts[i+1].Y  := yp            ;
        arPnts[i].Y    := rob           ;
        irob          := arPnts[i+1].Id ;
        arPnts[i+1].Id := arPnts[i].Id   ;
        arPnts[i].Id   := irob          ;
        Result        := -1            ;
        exit  ;
      end ;
    end ;
    Result := 0 ;
  end ;

  procedure T_Tessellation.sortNodeArray(
    const _num : Integer
  ) ;
  var
    i,
    i1,
    i2 : Integer ;
    X,
    x1 : Double  ;
  begin
    i := 0 ;
    X := arPnts[i].X ;
    while True do begin
      inc(i) ;
      if i = _num then
        exit ;
      x1 := arPnts[i].X ;
      if X = x1 then begin
      // Determine the number of identical X values
        i1 := i-1 ;
        i2 := i1  ;
        while True do begin
          if i2+1 < _num then
            inc(i2)
          else
            break ;

          if X <> arPnts[i2].X then  begin
             dec(i2) ;
             break ;
          end ;
        end ;

        while True do begin
          if swapNode(i1,i2) = 0 then
            break ;
        end ;
        i := i2 ;
      end ;
      X := x1 ;
    end ;
  end ;

  procedure T_Tessellation.quickSort(
    const _low  : Integer ;
    const _high : Integer
  ) ;

   //Sort all elements by X
    procedure doQuickSort(_ilo, _ihi: Integer) ;
    var
      lo,
      hi  : Integer ;
      mid : Double  ;
      t   : T_Pnt     ;
    begin
      lo := _ilo ;
      hi := _ihi ;
      mid := arPnts[(lo + hi) div 2].X ;
      repeat
        while arPnts[lo].X < mid do inc(lo) ;
        while arPnts[hi].X > mid do dec(hi) ;
        if lo <= hi then begin
          t := arPnts[lo] ;
          arPnts[lo] := arPnts[hi] ;
          arPnts[hi] := t ;
          inc(lo) ;
          dec(hi) ;
        end ;
      until lo > hi ;
      if hi > _ilo then
        doQuickSort(_ilo, hi) ;
      if lo < _ihi then
        doQuickSort(lo, _ihi) ;
    end ;

  begin
    doQuickSort(_low, _high) ;
  end ;

  procedure T_Tessellation.GetTriangle(
    const _idx : Integer ;
    var   _x1 : Single  ;
    var   _y1 : Single  ;
    var   _z1 : Single  ;
    var   _x2 : Single  ;
    var   _y2 : Single  ;
    var   _z2 : Single  ;
    var   _x3 : Single  ;
    var   _y3 : Single  ;
    var   _z3 : Single
  ) ;
  begin
    _x1 := arTriangles[_idx].X1 ;
    _y1 := arTriangles[_idx].Y1 ;
    _z1 := arTriangles[_idx].Z1 ;

    _x2 := arTriangles[_idx].X2 ;
    _y2 := arTriangles[_idx].Y2 ;
    _z2 := arTriangles[_idx].Z2 ;

    _x3 := arTriangles[_idx].X3 ;
    _y3 := arTriangles[_idx].Y3 ;
    _z3 := arTriangles[_idx].Z3 ;
  end ;

  procedure T_Tessellation.GetTriangleM(
    const _idx : Integer ;
    var  _m1  : Single  ;
    var  _m2  : Single  ;
    var  _m3  : Single
  ) ;
  begin
    _m1 := arTrianglesM[_idx].X ;
    _m2 := arTrianglesM[_idx].Y ;
    _m3 := arTrianglesM[_idx].Z ;
  end ;

  procedure T_Tessellation.GetTriangleIds(
    const _idx : Integer ;
    var  _p1  : Integer ;
    var  _p2  : Integer ;
    var  _p3  : Integer
  ) ;
  begin
    _p1 := arTrianglesIds[_idx].Id1 ;
    _p2 := arTrianglesIds[_idx].Id2 ;
    _p3 := arTrianglesIds[_idx].Id3 ;
  end ;

  procedure T_Tessellation.GetTriangleUIds(
    const _idx : Integer ;
    var  _p1  : Integer ;
    var  _p2  : Integer ;
    var  _p3  : Integer
  ) ;
  begin
    _p1 := ar3DNodes[arTrianglesIds[_idx].Id1].Uid ;
    _p2 := ar3DNodes[arTrianglesIds[_idx].Id2].Uid ;
    _p3 := ar3DNodes[arTrianglesIds[_idx].Id3].Uid ;
  end ;

  procedure T_Tessellation.GetTriangleTuv(
    const  _idx       : Integer   ;
    var    _n1       : TGIS_SingleVector ;
    var    _n2       : TGIS_SingleVector ;
    var    _n3       : TGIS_SingleVector ;
    var    _t1x      : Single    ;
    var    _t1y      : Single    ;
    var    _t2x      : Single    ;
    var    _t2y      : Single    ;
    var    _t3x      : Single    ;
    var    _t3y      : Single
  ) ;
  begin
    _n1.X := ar3DNodes[arTrianglesIds[_idx].Id1].Nx ;
    _n1.Y := ar3DNodes[arTrianglesIds[_idx].Id1].Ny ;
    _n1.Z := ar3DNodes[arTrianglesIds[_idx].Id1].Nz ;
    _n2.X := ar3DNodes[arTrianglesIds[_idx].Id2].Nx ;
    _n2.Y := ar3DNodes[arTrianglesIds[_idx].Id2].Ny ;
    _n2.Z := ar3DNodes[arTrianglesIds[_idx].Id2].Nz ;
    _n3.X := ar3DNodes[arTrianglesIds[_idx].Id3].Nx ;
    _n3.Y := ar3DNodes[arTrianglesIds[_idx].Id3].Ny ;
    _n3.Z := ar3DNodes[arTrianglesIds[_idx].Id3].Nz ;
    _t1x  := ar3DNodes[arTrianglesIds[_idx].Id1].Tu ;
    _t1y  := ar3DNodes[arTrianglesIds[_idx].Id1].Tv ;
    _t2x  := ar3DNodes[arTrianglesIds[_idx].Id2].Tu ;
    _t2y  := ar3DNodes[arTrianglesIds[_idx].Id2].Tv ;
    _t3x  := ar3DNodes[arTrianglesIds[_idx].Id3].Tu ;
    _t3y  := ar3DNodes[arTrianglesIds[_idx].Id3].Tv ;
  end ;
{$ENDREGION 'T_Tessellation'}

{$REGION 'T_MeshBuilder'}
//==============================================================================
// T_MeshBuilder
//==============================================================================

{$IFDEF JAVA}
type
  T_SVComparator = class ( java.util.Comparator<TGIS_SingleVector> )
    public
      method compare( _v1 : TGIS_SingleVector;  _v2 : TGIS_SingleVector ) : Integer;
  end ;

  method T_SVComparator.compare(_v1: TGIS_SingleVector; _v2: TGIS_SingleVector) : Integer ;
  begin
    if (_v1.X = _v2.X) and (_v1.Y = _v2.Y) and (_v1.Z = _v2.Z) then
      Result := 0
    else if (_v1.X < _v2.X) then
      Result := 1
    else
      Result := -1 ;
  end ;
{$ENDIF}

  constructor T_MeshBuilder.Create ;
  begin
    inherited ;

    vertexLookup := TDictionary<TGIS_SingleVector, Integer>.Create( {$IFDEF JAVA} new T_SVComparator() {$ENDIF} ) ;
    Reset ;
  end ;

  procedure T_MeshBuilder.doDestroy ;
  begin
    FreeObject( vertexLookup ) ;
    faceList     := nil ;
    unqPointList := nil ;
    allPointList := nil ;

    inherited ;
  end ;

  procedure T_MeshBuilder.Reset ;
  begin
    vertexLookup.Clear ;

    faceList     := nil ;
    unqPointList := nil ;
    allPointList := nil ;
    numFaces     := 0 ;
    numUnqPoints := 0 ;
    numAllPoints := 0 ;
  end ;

  procedure T_MeshBuilder.Prepare(
    const _triangles : Integer
  )  ;
  begin
    SetLength( faceList, length( faceList ) + _triangles ) ;
    SetLength( unqPointList, length( unqPointList ) + _triangles * 3 ) ;
    SetLength( allPointList, length( allPointList ) + _triangles * 3 ) ;
  end ;

  procedure T_MeshBuilder.AddVertex(
    const _v : TGIS_SingleVector ;
    const _n : TGIS_SingleVector
  ) ;
  begin
    if not vertexLookup.ContainsKey( _v ) then begin
      vertexLookup.Add( _v, numUnqPoints + 1 ) ;
      {$IFDEF GIS_NORECORDS}
        unqPointList[numUnqPoints] := new T_FaceVertex() ;
      {$ENDIF}
      unqPointList[numUnqPoints].P := _v ;
      unqPointList[numUnqPoints].N := _n ;
      inc( numUnqPoints ) ;
    end ;
    {$IFDEF GIS_NORECORDS}
      allPointList[numAllPoints] := new T_FaceVertex() ;
    {$ENDIF}
    allPointList[numAllPoints].P := _v ;
    allPointList[numAllPoints].N := _n ;
    inc( numAllPoints ) ;
  end ;

  procedure T_MeshBuilder.AddFace(
    const _v1   : TGIS_SingleVector ;
    const _v2   : TGIS_SingleVector ;
    const _v3   : TGIS_SingleVector ;
    const _n    : TGIS_SingleVector
  ) ;
  var
    fr   : T_FaceRecord ;
    v    : {$IFDEF JAVA} nullable {$ENDIF} Integer ;
  begin
    {$IFDEF OXYGENE}
      {$IFNDEF ISLAND}
      fr.I := new Integer[3] ;
      {$ENDIF}
    {$ENDIF}
    fr.I[0] := 0 ;
    fr.I[1] := 0 ;
    fr.I[2] := 0 ;

    if vertexLookup.TryGetValue( _v1, v ) then
      fr.I[0] := v ;
    if vertexLookup.TryGetValue( _v2, v ) then
      fr.I[1] := v ;
    if vertexLookup.TryGetValue( _v3, v ) then
      fr.I[2] := v ;

    fr.N := _n ;
    faceList[numFaces] := fr ;
    inc( numFaces ) ;
  end ;

  procedure T_MeshBuilder.BuildFaces ;
  var
    i : Integer ;
  begin
    numFaces := 0 ;

    i := 0 ;
    while i < numAllPoints-2 do begin
      AddFace( allPointList[i].P, allPointList[i+1].P, allPointList[i+2].P,
               allPointList[i].N
             ) ;
      inc( i, 3 ) ;
    end ;
  end ;
{$ENDREGION 'T_MeshBuilder'}

{$REGION 'TGIS_Tessellation'}
//==============================================================================
// TGIS_Renderer3DTriang
//==============================================================================

  constructor TGIS_Tessellation.Create ;
  begin
    inherited ;

    oTri  := T_Tessellation.Create ;
    Reset ;
  end ;

  procedure TGIS_Tessellation.doDestroy ;
  begin
    FreeObject( oTri ) ;

    inherited ;
  end ;

  procedure TGIS_Tessellation.AddVertex(
    const _part     : Integer ;
    const _ptg      : TGIS_Point3D
  ) ;
  begin
    T_Tessellation(oTri).AddVertex( _part, _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  end ;

  procedure TGIS_Tessellation.AddVertex(
    const _part     : Integer ;
    const _x        : Double  ;
    const _y        : Double  ;
    const _z        : Double  ;
    const _m        : Double
  ) ;
  begin
    T_Tessellation(oTri).AddVertex( _part, _x, _y, _z, _m ) ;
  end ;

  procedure TGIS_Tessellation.AddVertex(
    const _part     : Integer ;
    const _x        : Double  ;
    const _y        : Double  ;
    const _z        : Double  ;
    const _m        : Double  ;
    const _i        : Integer
  ) ;
  begin
    T_Tessellation(oTri).AddVertex( _part, _x, _y, _z, _m, _i ) ;
  end ;

  procedure TGIS_Tessellation.AddVertexTuv(
    const _part     : Integer ;
    const _x        : Double  ;
    const _y        : Double  ;
    const _z        : Double  ;
    const _m        : Double  ;
    const _nx       : Single  ;
    const _ny       : Single  ;
    const _nz       : Single  ;
    const _u        : Single  ;
    const _v        : Single
  ) ;
  begin
    T_Tessellation(oTri).AddVertexTuv(
      _part, _x, _y, _z, _m, _nx, _ny, _nz, _u, _v
    ) ;
  end ;

  procedure TGIS_Tessellation.AddVertexTuv(
    const _part     : Integer ;
    const _ptg      : TGIS_Point3D ;
    const _normal   : TGIS_SingleVector  ;
    const _u        : Single  ;
    const _v        : Single
  ) ;
  begin
    T_Tessellation(oTri).AddVertexTuv(
      _part, _ptg.X, _ptg.Y, _ptg.Z, _ptg.M, _normal.X, _normal.Y, _normal.Z, _u, _v
    ) ;
  end ;

  procedure TGIS_Tessellation.GetTriangle(
    const _idx       : Integer ;
      var   _x1     : Single  ;
      var   _y1     : Single  ;
      var   _z1     : Single  ;
      var   _x2     : Single  ;
      var   _y2     : Single  ;
      var   _z2     : Single  ;
      var   _x3     : Single  ;
      var   _y3     : Single  ;
      var   _z3     : Single
  ) ;
  begin
    T_Tessellation(oTri).GetTriangle(
      _idx, _x1, _y1, _z1, _x2, _y2, _z2, _x3, _y3, _z3
    ) ;
  end ;

  procedure TGIS_Tessellation.GetTriangle(
    const _idx       : Integer ;
    var   _v1       : TGIS_SingleVector  ;
    var   _v2       : TGIS_SingleVector  ;
    var   _v3       : TGIS_SingleVector
  ) ;
  begin
    T_Tessellation(oTri).GetTriangle(
      _idx, _v1.X, _v1.Y, _v1.Z, _v2.X, _v2.Y, _v2.Z, _v3.X, _v3.Y, _v3.Z
    ) ;
  end ;

  procedure TGIS_Tessellation.GetTriangleM(
    const _idx       : Integer ;
    var   _m1       : Single  ;
    var   _m2       : Single  ;
    var   _m3       : Single
  ) ;
  begin
    T_Tessellation(oTri).GetTriangleM( _idx, _m1, _m2, _m3 ) ;
  end ;

  procedure TGIS_Tessellation.GetTriangleTuv(
    const  _idx       : Integer   ;
      var  _n1       : TGIS_SingleVector ;
      var  _n2       : TGIS_SingleVector ;
      var  _n3       : TGIS_SingleVector ;
      var  _t1x      : Single    ;
      var  _t1y      : Single    ;
      var  _t2x      : Single    ;
      var  _t2y      : Single    ;
      var  _t3x      : Single    ;
      var  _t3y      : Single
  ) ;
  begin
    T_Tessellation(oTri).GetTriangleTuv(
      _idx, _n1, _n2, _n3, _t1x, _t1y, _t2x, _t2y, _t3x, _t3y
    ) ;
  end ;


  procedure TGIS_Tessellation.GetTriangleUIds(
    const _idx : Integer ;
    var  _p1  : Integer ;
    var  _p2  : Integer ;
    var  _p3  : Integer
  ) ;
  begin
    T_Tessellation(oTri).GetTriangleUIds( _idx, _p1, _p2, _p3 ) ;
  end ;

  procedure TGIS_Tessellation.Reset ;
  begin
    T_Tessellation(oTri).InitVariables ;
    FTriangles   := 0 ;
    FExecuteCode := 0 ;
  end ;

  procedure TGIS_Tessellation.Execute ;
  begin
    FTriangles   := T_Tessellation(oTri).Execute ;
    FExecuteCode := T_Tessellation(oTri).EmergencyCode ;
  end ;

{$ENDREGION 'TGIS_Tessellation'}

{$REGION 'TGIS_MultiPatchBuilder'}

  constructor TGIS_MultiPatchBuilder.Create ;
  begin
    inherited ;

    oMesh := T_MeshBuilder.Create ;
  end ;

  procedure TGIS_MultiPatchBuilder.doDestroy ;
  begin
    FreeObject( oMesh ) ;

    inherited ;
  end ;

  function TGIS_MultiPatchBuilder.fget_FaceCount : Integer ;
  begin
    Result := T_MeshBuilder(oMesh).numFaces ;
  end ;

  function TGIS_MultiPatchBuilder.fget_VertexCount : Integer ;
  begin
    Result := T_MeshBuilder(oMesh).numUnqPoints ;
  end ;

  procedure TGIS_MultiPatchBuilder.BuildMesh(
    const _shape      : TObject
  ) ;
  var
    shp     : TGIS_Shape    ;
    triMode : Integer       ;
    tri     : TGIS_Tessellation ;
    pdata   : array of TGIS_Point3D ;
    num_pts : Integer       ;

    procedure loadTriBuffer(
      const _part : Integer ;
      const _num  : Integer ;
      const _area : Double
    ) ;
    var
      i : Integer ;
    begin
      if _area < 0 then begin
        for i := 0 to _num do
          case triMode of
            0 : tri.AddVertex( _part, pdata[i].X, pdata[i].Y, pdata[i].Z, 0 ) ;
            1 : tri.AddVertex( _part, -pdata[i].X, pdata[i].Z, pdata[i].Y, 0 ) ;
            2 : tri.AddVertex( _part, pdata[i].Y, pdata[i].Z, pdata[i].X, 0 ) ;
          end
      end
      else begin
        for i := _num downto 0 do
          case triMode of
            0 : tri.AddVertex( _part, pdata[i].X, pdata[i].Y, pdata[i].Z, 0 ) ;
            1 : tri.AddVertex( _part, -pdata[i].X, pdata[i].Z, pdata[i].Y, 0 ) ;
            2 : tri.AddVertex( _part, pdata[i].Y, pdata[i].Z, pdata[i].X, 0 ) ;
          end ;
      end ;
    end ;

    procedure normalVect(
      const _v0 : TGIS_SingleVector ;
      const _v1 : TGIS_SingleVector ;
      const _v2 : TGIS_SingleVector ;
        var _n1 : TGIS_SingleVector ;
        var _n2 : TGIS_SingleVector ;
        var _n3 : TGIS_SingleVector ;
      const _wind : Boolean
    ) ;
    begin
      _n1 := T_Vector3Utils.triangleNormal( _v0, _v1, _v2 ) ;
      _n2 := T_Vector3Utils.triangleNormal( _v1, _v2, _v0 ) ;
      _n3 := T_Vector3Utils.triangleNormal( _v2, _v0, _v1 ) ;

      if not _wind then begin
        _n1.X := -_n1.X ;
        _n1.Y := -_n1.Y ;
        _n1.Z := -_n1.Z ;
        _n2.X := -_n2.X ;
        _n2.Y := -_n2.Y ;
        _n2.Z := -_n2.Z ;
        _n3.X := -_n3.X ;
        _n3.Y := -_n3.Y ;
        _n3.Z := -_n3.Z ;
      end;
    end;

    procedure doTessellation( const _winding : Boolean ) ;
    var
      ptg1   : TGIS_SingleVector ;
      ptg2   : TGIS_SingleVector ;
      ptg3   : TGIS_SingleVector ;
      n1     : TGIS_SingleVector ;
      n2     : TGIS_SingleVector ;
      n3     : TGIS_SingleVector ;
      i      : Integer ;
    begin
      tri.Execute ;
      // assert( tri.ExecuteCode = 0 ) ;
      // assert( tri.TrianglesCount > 0 ) ;

      if tri.TrianglesCount = 0 then exit ;

      T_MeshBuilder(oMesh).Prepare( tri.TrianglesCount ) ;

      for i := 0 to tri.TrianglesCount-1 do begin
        case triMode of
          0 : tri.GetTriangle( i, ptg1.X, ptg1.Y, ptg1.Z,
                                  ptg2.X, ptg2.Y, ptg2.Z,
                                  ptg3.X, ptg3.Y, ptg3.Z
                             ) ;
          1 : tri.GetTriangle( i, ptg1.X, ptg1.Z, ptg1.Y,
                                  ptg2.X, ptg2.Z, ptg2.Y,
                                  ptg3.X, ptg3.Z, ptg3.Y
                             ) ;
          2 : tri.GetTriangle( i, ptg1.Y, ptg1.Z, ptg1.X,
                                  ptg2.Y, ptg2.Z, ptg2.X,
                                  ptg3.Y, ptg3.Z, ptg3.X
                             ) ;
        end ;

        if triMode = 1 then begin
          ptg1.X := -ptg1.X ;
          ptg2.X := -ptg2.X ;
          ptg3.X := -ptg3.X ;
        end ;

        normalVect( ptg1, ptg2, ptg3, n1, n2, n3, _winding ) ;

        T_MeshBuilder(oMesh).AddVertex( ptg1, n1 ) ;
        T_MeshBuilder(oMesh).AddVertex( ptg2, n2 ) ;
        T_MeshBuilder(oMesh).AddVertex( ptg3, n3 ) ;
      end ;
    end ;

    function closedRing( const _num : Integer ) : Boolean ;
    begin
      if (num_pts > 0) and (_num <= num_pts) then begin
        if (pdata[0].X = pdata[_num-1].X) and
           (pdata[0].Y = pdata[_num-1].Y) and
           (pdata[0].Z = pdata[_num-1].Z) then
          Result := True
        else
          Result := False ;
      end
      else
        Result := False ;
    end ;

    procedure analizeShape ;
    var
      part_no, k  : Integer       ;
      point_no    : Integer       ;
      num_parts   : Integer       ;
      first_pnt   : TGIS_Point3D  ;
      ptg, ptg0   : TGIS_Point3D  ;
      ptype       : TGIS_PartType ;
      area        : Double        ;
      arXY        : Double        ;
      arXZ        : Double        ;
      arYZ        : Double        ;
    begin
      num_parts := shp.GetNumParts ;

      for part_no := 0 to num_parts -1 do begin
        area := 0 ;
        arXY := 0 ;
        arXZ := 0 ;
        arYZ := 0 ;

        ptype   := shp.GetPartType( part_no ) ;
        num_pts := shp.GetPartSize( part_no ) ;
        SetLength( pdata, num_pts ) ;

        for point_no := 0 to num_pts - 1 do begin
          ptg := shp.GetPoint3D( part_no, point_no ) ;

          if point_no = 0 then
            first_pnt := ptg
          else begin
            arXY := arXY + ( ptg0.X*ptg.Y - ptg.X*ptg0.Y ) ;
            arXZ := arXZ + ( ptg0.X*ptg.Z - ptg.X*ptg0.Z ) ;
            arYZ := arYZ + ( ptg0.Y*ptg.Z - ptg.Y*ptg0.Z ) ;
          end ;
          ptg0 := ptg ;

          pdata[point_no] := ptg ;
        end ;

        if not closedRing( num_pts ) then begin
          arXY := arXY + ( ptg.X*first_pnt.Y - first_pnt.X*ptg.Y ) ;
          arXZ := arXZ + ( ptg.X*first_pnt.Z - first_pnt.X*ptg.Z ) ;
          arYZ := arYZ + ( ptg.Y*first_pnt.Z - first_pnt.Y*ptg.Z ) ;
        end ;

        // mode types - 0: XY, 1: -XZ, 2: YZ
        triMode := 0 ;
        area := arXY ;

        if Abs(area) < 0.5 * Abs(arXZ) then begin
          area    := -arXZ ;
          triMode := 1 ;
        end ;
        if Abs(area) < 0.5 * Abs(arYZ) then begin
          area    := arYZ ;
          triMode := 2 ;
        end ;

        if closedRing( num_pts ) then
          k := num_pts - 2
        else
          k := num_pts - 1 ;

        loadTriBuffer( 0, k, area ) ;
        doTessellation( area < 0 ) ;
        tri.Reset ;
      end ;
    end ;

  begin
    if not assigned( _shape ) then exit ;

    tri := TGIS_Tessellation.Create ;
    try
      shp := _shape as TGIS_Shape ;
      if shp.IsEmpty then exit ;

      // safe for all types including triangles
      analizeShape ;

      T_MeshBuilder(oMesh).BuildFaces ;
    finally
      FreeObject( tri ) ;
    end ;
  end ;

  procedure TGIS_MultiPatchBuilder.GetVertex(
    const _idx : Integer ;
      var _v   : TGIS_SingleVector
  ) ;
  begin
    _v := T_MeshBuilder(oMesh).unqPointList[_idx].P ;
  end ;

  procedure TGIS_MultiPatchBuilder.GetVertexNormal(
    const _idx : Integer ;
      var _n   : TGIS_SingleVector
  ) ;
  begin
    _n := T_MeshBuilder(oMesh).unqPointList[_idx].N ;
  end ;

  procedure TGIS_MultiPatchBuilder.GetFace(
    const _idx : Integer ;
      var _f1  : Integer ;
      var _f2  : Integer ;
      var _f3  : Integer
  ) ;
  begin
    _f1 := T_MeshBuilder(oMesh).faceList[_idx].I[0] ;
    _f2 := T_MeshBuilder(oMesh).faceList[_idx].I[1] ;
    _f3 := T_MeshBuilder(oMesh).faceList[_idx].I[2] ;
  end ;

  procedure TGIS_MultiPatchBuilder.GetFaceNormal(
    const _idx : Integer ;
      var _n   : TGIS_SingleVector
  ) ;
  begin
    _n := T_MeshBuilder(oMesh).faceList[_idx].N
  end ;

{$ENDREGION 'TGIS_MultiPatchBuilder'}

//==================================== END =====================================
end.
