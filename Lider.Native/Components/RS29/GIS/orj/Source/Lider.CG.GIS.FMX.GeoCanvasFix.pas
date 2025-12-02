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
  Canvas helper which fixes FMX GPU canvas rendering errors.
}

unit FMX.GisCanvasFix ;
{$HPPEMIT '#pragma link "FMX.GisCanvasFix"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.RTLConsts,
  System.Generics.Collections,
  System.Math,
  System.Math.Vectors,
  FMX.Types,
  FMX.Types3D,
  FMX.Graphics,
  FMX.Materials,
  FMX.Canvas.GPU,
  GisRtl ;

type

  /// <summary>
  ///   Helper class which overrides misbehaving methods
  ///   of the FMX GPU canvas.
  /// </summary>
  TGIS_CanvasHelperGpu = class helper for TCanvas
    private
      procedure gl2dStrokePath ( const _path    : TObject ;
                                 const _opacity : Single
                               ) ;
      procedure gl2dDrawStroke ( const _stroke  : TObject ;
                                 const _opacity : Single
                               ) ;
      procedure gl2dFillPath   ( const _path    : TObject ;
                                 const _opacity : Single
                               ) ;
      procedure gl2dDrawFill   ( const _fill    : TObject ;
                                 const _opacity : Single
                               ) ;
    private
      function  createMaterial ( const _color   : TAlphaColor
                               ) : TColorMaterial ; overload;
      function  createMaterial ( const _bitmap  : TBitmap
                               ) : TTextureMaterial ; overload;
      function  auxFillSimple  ( const _path    : TPathData ;
                                 const _opacity : Single
                               ) : Boolean ;
      procedure drawTexture    ( const _vbuf    : TVertexBuffer ;
                                 const _ibuf    : TIndexBuffer ;
                                 const _tris    : Boolean ;
                                 const _bitmap  : TBitmap ;
                                 const _rect    : TRectF ;
                                 const _opacity : Single
                               ) ;
      procedure drawColor      ( const _vbuf    : TVertexBuffer ;
                                 const _ibuf    : TIndexBuffer ;
                                 const _tris    : Boolean ;
                                 const _color   : TAlphaColor ;
                                 const _stencil : Boolean ;
                                 const _opacity : Single
                               ) ;
      procedure drawColor1     ( const _vbuf    : TVertexBuffer ;
                                 const _ibuf    : TIndexBuffer ;
                                 const _vbuf1   : TVertexBuffer ;
                                 const _ibuf1   : TIndexBuffer ;
                                 const _tris    : Boolean ;
                                 const _color   : TAlphaColor ;
                                 const _stencil : Boolean ;
                                 const _opacity : Single
                               ) ;
    public

      /// <summary>
      ///   Draws line segment between two points using
      ///   the current drawing style.
      /// </summary>
      /// <param name="_start">
      ///   start point
      /// </param>
      /// <param name="_end">
      ///   end point
      /// </param>
      /// <param name="_opacity">
      ///   opacity; varies between 0 (transparent) and 1 (opaque)
      /// </param>
      procedure DrawLine    ( const _start   : TPointF ;
                              const _end     : TPointF ;
                              const _opacity : Single
                            ) ;

      /// <summary>
      ///   Draws line along a path using
      ///   the current drawing style.
      /// </summary>
      /// <param name="_path">
      ///   path (list of points)
      /// </param>
      /// <param name="_opacity">
      ///   opacity; varies between 0 (transparent) and 1 (opaque)
      /// </param>
      procedure DrawPath    ( const _path    : TPathData ;
                              const _opacity : Single
                            ) ;

      /// <summary>
      ///   Fills an area bounded by a closed path using
      ///   the current filling style.
      /// </summary>
      /// <param name="_path">
      ///   path (list of points)
      /// </param>
      /// <param name="_opacity">
      ///   opacity; varies between 0 (transparent) and 1 (opaque)
      /// </param>
      procedure FillPath    ( const _path    : TPathData ;
                              const _opacity : Single
                            ) ;
  end ;


//##############################################################################
implementation

uses
  GisTypes ;

type
  T_pathCommand = ( JumpTo, ConnectTo, CurveTo, Close ) ;

  T_pathVertex = record
    V : TPointF ;
    C : T_pathCommand ;
  end ;

  T_strokeCommand = ( MoveTo, MoveToFan, LineTo ) ;

  T_strokeVertex = record
    V : TPointF ;
    C : T_strokeCommand ;
  end ;

  T_strokeJoin = ( Bevel, Miter, Round ) ;

  T_strokeCap = ( Butt, Square, Round ) ;

  T_triangle = record
    X1 : Single ;
    Y1 : Single ;
    X2 : Single ;
    Y2 : Single ;
    X3 : Single ;
    Y3 : Single ;
  end ;

  T_node = record
    Id   : Integer ; // point id
    Pos  : Integer ; // position in sorted vertex array
    Part : Integer ; // part number
    X    : Double  ; // x coordinate
    Y    : Double  ; // y coordinate
    Prev : Integer ; // previous point
    Next : Integer ; // next point
    Sel  : Integer ; // selection marker
  end ;

  T_nodeArray = array of T_node ;

  T_triangles = array of T_triangle ;

  T_part = record
    Nr  : Integer ; // part number
    Fst : Integer ; // first point position
    Lst : Integer ; // last point position
  end ;

  T_partArray = array of T_part ;

  T_Triangulate = class
    private
      firstNode   : Integer     ; // first node number in part
      curPart     : Integer     ; // current part number
      nodNum      : Integer     ; // total number of nodes
      partNum     : Integer     ; // total number of parts
      id1,id2,id3 : Integer     ; // point position in Tnod array
      triNum      : Integer     ; // number of triangles created
      arTnod      : T_nodeArray ; // array of Vertices
      arTri       : T_triangles ; // array of returned triangles
      arPart      : T_partArray ; // part info array

    private

      // Add new triangle to triangle array
      // _id1  first point of triangle
      // _id2  second point of triangle
      // _i3d  third point of triangle
      procedure addTriangle             ( const _id1      : Integer ;
                                          const _id2      : Integer ;
                                          const _id3      : Integer
                                        ) ;

      // Fan triangulation routine.
      procedure doTriangulation         ;

      // Check part.
      function  checkPart               : Boolean ;

      // Add part to part array
      procedure addPart                 ;

    public
      constructor Create                ;
    public
      destructor  Destroy ; override;
    public

      // Set initial values of necessary variables.
      //  _numpnts  number of vertices in shape
      procedure Reset                   ( const _numpnts  : Integer ) ;

      // Add point to array of points
      // _part  part number
      // _x     x coordinate
      // _y     y coordinate
      procedure AddVertex               ( const _part     : Integer ;
                                          const _x        : Double  ;
                                          const _y        : Double
                                        ) ;
      // Start with triangulation.
      // number of triangles created during triangulation.
      function  Go                      : Integer;

      // Get trinagle from the triangle array
      // _no  triangle position in triangle array
      // triangle coordinates
      function  GetTriangle             ( const _no       : Integer
                                        ) : T_triangle ;

      // Get number of triangles created
      function  GetCount                : Integer ;

  end ;

  T_pathLowLevel = class
    private
      aPath      : array of T_pathVertex ;
      oPartPoint : T_pathVertex ;
    private
      FIsClosed  : Boolean ;
      FCount     : Integer ;
      FNewPart   : Boolean ;
    public
      constructor Create ;
    public
      destructor Destroy ; override;
    public
      procedure NewPart      ;
      procedure AddVertex  ( const _vec : TPointF
                           ) ; overload;
      procedure MakeClosed   ;
    public
      function  GetIsClosed  : Boolean ;
      function  GetCount     : Integer ;
      function  GetVertex    ( const _i : Integer
                             ) : TPointF ;
      function  GetCommand   ( const _i : Integer
                             ) : T_pathCommand ;
      function  GetBoundRect : TRectF ;
  end ;

  T_path = class
    private
      oPathData : TPathData ;
      bMatrix   : Boolean ;
      oPolygon  : TPolygon ;
      bNative   : Boolean ;
      isNewPart : Boolean ;
      oPath     : TObject ;
    private
      function  fget_IsClosed : Boolean ;
      function  fget_Count    : Integer ;
      function  fget_Vertex   ( const _i : Integer
                              ) : TPointF ;
      function  fget_Command  ( const _i : Integer
                              ) : T_pathCommand ;
    private
      procedure calcBezierCoeff
                              ( const _bezier : TCubicBezier ;
                                  out _ax     : Single ;
                                  out _bx     : Single ;
                                  out _cx     : Single ;
                                  out _ay     : Single ;
                                  out _by     : Single ;
                                  out _cy     : Single
                              ) ;
      function  pointOnBezier ( const _point  : TPointF ;
                                const _ax     : Single ;
                                const _bx     : Single ;
                                const _cx     : Single ;
                                const _ay     : Single ;
                                const _by     : Single ;
                                const _cy     : Single ;
                                const _t      : Single
                              ) : TPointF ;
      function  createBezier  ( const _bezier : TCubicBezier ;
                                const _count  : Integer
                              ) : TPolygon ;
    public
      constructor Create ; overload;
      constructor Create ( const _path    : TPathData
                         ) ; overload;
    public
      destructor Destroy ; override;
    public
      procedure NewPart      ;
      procedure AddVertex    ( const _x : Integer ;
                               const _y : Integer
                             ) ; overload;
      procedure AddVertex    ( const _x : Single ;
                               const _y : Single
                             ) ; overload;
      procedure AddVertex    ( const _vec : TPointF
                             ) ; overload;
      procedure MakeClosed   ;
      function  ExpandCurve  ( const _idx : Integer ;
                                 var _arr : TPolygon
                             ) : Integer ;
      function  GetBoundRect : TRectF ;
      procedure ApplyMatrix  ( const _mtx : TMatrix
                             ) ;
    public
      property IsClosed : Boolean
                          read  fget_IsClosed ;
      property Count    : Integer
                          read  fget_Count ;
      property Vertex   [const _i : Integer]
                        : TPointF
                          read  fget_Vertex ;
      property Command  [const _i : Integer]
                        : T_pathCommand
                          read  fget_Command ;
  end ;

  T_strokeLowLevel = class
    private
      oPathBase   : T_path ;
      oPath       : T_path ;
      iJoin       : T_strokeJoin ;
      iCap        : T_strokeCap  ;
      sWidth      : Single ;
      aSequence   : array of Single ;
      iSequence   : Integer ;
      sOffset     : Single ;
      iOffset     : Integer ;
      bSolidStart : Boolean ;
      iSteps      : Integer ;
      sAngle      : Single ;
      iJoinIndexK : Integer ;
      iJoinIndexJ : Integer ;
      bExpandPath : Boolean ;
      bOverW1Join : Boolean ;
    private
      aSkeleton : TList<T_strokeVertex> ;
      aGeometry : TList<T_strokeVertex> ;
    private
      function  calcVecLen       ( const _v1   : TPointF
                                 ) : Single ;
      function  calcScalarProd   ( const _v1   : TPointF ;
                                   const _v2   : TPointF
                                 ) : Single ;
      function  calcVecSum       ( const _v1   : TPointF ;
                                   const _v2   : TPointF
                                 ) : TPointF ;
      function  calcVecDif       ( const _v1   : TPointF ;
                                   const _v2   : TPointF
                                 ) : TPointF ;
      function  calcVecNorm      ( const _v    : TPointF
                                 ) : TPointF ;
      function  calcAngle        ( const _v0   : TPointF;
                                   const _v1   : TPointF;
                                   const _v2   : TPointF
                                 ) : Single ;
      function  calcVecRot       ( const _v0   : TPointF ;
                                   const _v1   : TPointF ;
                                   const _phi  : Single
                                 ) : TPointF ;
      function  calcIntersection ( const _v11  : TPointF ;
                                   const _v12  : TPointF ;
                                   const _v21  : TPointF ;
                                   const _v22  : TPointF ;
                                     out _res  : TPointF
                                 ) : Integer ;
      function calc3PointCenter  ( const _v1   : TPointF ;
                                   const _v2   : TPointF ;
                                   const _v3   : TPointF ;
                                     out _vr   : TPointF
                                 ) : Boolean ;
    private
      procedure expandPath       ;
      procedure expandSequence ( const _seq   : array of Single ;
                                 const _off   : Single
                               ) ;
      procedure buildSkeleton    ;
      procedure buildGeometry    ;
    public
      constructor Create ( const _path  : T_path          ;
                           const _join  : T_strokeJoin    ;
                           const _cap   : T_strokeCap     ;
                           const _width : Single          ;
                           const _seq   : array of Single ;
                           const _off   : Single          ;
                           const _over1 : Boolean
                         ) ;
    public
      destructor Destroy ; override;
    public
      function  GetWidth     : Single ;
      function  GetVertex  ( const _i : Integer
                           ) : TPointF ;
      function  GetCommand ( const _i : Integer
                           ) : T_strokeCommand ;
      function  GetCount     : Integer ;
  end ;

  T_stroke = class
    private
      oStroke : TObject ;
    private
      FPath : T_path ;
    private
      function  fget_Width     : Single ;
      function  fget_Vertex  ( const _i : Integer
                            ) : TPointF ;
      function  fget_Command ( const _i : Integer
                            ) : T_strokeCommand ;
      function  fget_Count     : Integer ;
    public
      constructor Create ( const _path  : T_path          ;
                           const _join  : T_strokeJoin    ;
                           const _cap   : T_strokeCap     ;
                           const _width : Single          ;
                           const _over1 : Boolean
                         ) ; overload;
      constructor Create ( const _path  : T_path          ;
                           const _join  : T_strokeJoin    ;
                           const _cap   : T_strokeCap     ;
                           const _width : Single          ;
                           const _seq   : array of Single ;
                           const _off   : Single          ;
                           const _over1 : Boolean
                         ) ; overload;
    public
      destructor Destroy ; override;
    public
      property Path    : T_Path
                         read  FPath ;
      property Width   : Single
                         read  fget_Width ;
      property Vertex  [ const _i : Integer ]
                       : TPointF
                         read  fget_Vertex ;
      property Command [ const _i : Integer ]
                       : T_strokeCommand
                         read  fget_Command ;
      property Count   : Integer
                         read  fget_Count ;
  end ;

  T_fill = class
    private
      oTri  : TObject ;
      oRect : array of T_triangle ;
    private
      FPath : T_Path ;
    private
      function  fget_Count    : Integer ;
      function  fget_Triangle ( const _i : Integer
                              ) : T_triangle ;
      function  fget_Count1   : Integer ;
      function  fget_Triangle1( const _i : Integer
                              ) : T_triangle ;
    private
      procedure readPath      ( const _path : T_path
                              ) ;
    public
      constructor Create      ( const _path : T_path
                              ) ;
      destructor  Destroy     ; override;
    public
      property Path           : T_Path
                                read  FPath ;
      property Count          : Integer
                                read  fget_Count ;
      property Triangle [const _i : Integer]
                              : T_triangle
                                read  fget_Triangle ;
      property Count1         : Integer
                                read  fget_Count1 ;
      property Triangle1[const _i : Integer]
                              : T_triangle
                                read  fget_Triangle1 ;
  end ;


  function MatrixEqual(
    const _mtx1 : TMatrix ;
    const _mtx2 : TMatrix
  ) : Boolean ;
  const
    LOCAL_EPSILON : Double = 1E-5 ;
  begin
    Result :=
      SameValue( _mtx1.m11, _mtx2.m11, LOCAL_EPSILON ) and
      SameValue( _mtx1.m12, _mtx2.m12, LOCAL_EPSILON ) and
      SameValue( _mtx1.m13, _mtx2.m13, LOCAL_EPSILON ) and
      SameValue( _mtx1.m21, _mtx2.m21, LOCAL_EPSILON ) and
      SameValue( _mtx1.m22, _mtx2.m22, LOCAL_EPSILON ) and
      SameValue( _mtx1.m23, _mtx2.m23, LOCAL_EPSILON ) and
      SameValue( _mtx1.m31, _mtx2.m31, LOCAL_EPSILON ) and
      SameValue( _mtx1.m32, _mtx2.m32, LOCAL_EPSILON ) and
      SameValue( _mtx1.m33, _mtx2.m33, LOCAL_EPSILON ) ;
  end ;


{$REGION 'T_Triangualte'}
//==============================================================================
// T_Triangulate
//==============================================================================

  constructor T_Triangulate.Create ;
  begin
    inherited Create ;
  end ;

  destructor T_Triangulate.Destroy ;
  begin
    SetLength( arTnod   , 0 ) ;
    SetLength( arTri    , 0 ) ;
    SetLength( arPart   , 0 ) ;

    inherited ;
  end ;

  procedure T_Triangulate.Reset(
    const _numpnts  : Integer
  ) ;
  begin
    firstNode      := 0 ;
    curPart        := 0 ;
    triNum         := 0 ;
    nodNum         := 0 ;
    partNum        := 0 ;
    SetLength( arTnod   , 0 ) ;
    SetLength( arTnod   , _numpnts ) ;
    SetLength( arTri    , _numpnts ) ;
    SetLength( arPart   , 1 ) ;
    arPart[0].Nr  := 0 ;
    arPart[0].Fst := 0 ;
  end;

  procedure T_Triangulate.addPart;
  begin
    SetLength( arPart   , High(arPart) + 2 ) ;
    arPart[High(arPart)].Nr  := High(arPart) ;
    arPart[High(arPart)].Fst := nodNum ;
  end;

  function T_Triangulate.checkPart : Boolean ;
  var
    ip : Integer ;

    procedure reset_part ;
    var
      ip : Integer ;
    begin
      ip := High( arPart ) ;
      if ip > 0 then begin
        firstNode := arPart[ip-1].Fst ;
        nodNum    := arPart[ip].Fst ;
      end
      else begin
        firstNode := 0 ;
        nodNum    := 0 ;
      end;
    end;

  begin
    if (arTnod[arPart[High(arPart)].Fst].X = arTnod[nodNum-1].X) and
       (arTnod[arPart[High(arPart)].Fst].Y = arTnod[nodNum-1].Y) then
        Dec( nodNum ) ;

    Result := False ;
    ip := High( arPart ) ;
    arPart[ip].Lst := nodNum -1 ;
    if (nodNum - arPart[ip].Fst < 3) then begin
      reset_part ;
      exit;
    end ;

    Result := True ;
  end;

  procedure T_Triangulate.AddVertex(
    const _part : Integer ;
    const _x    : Double  ;
    const _y    : Double
  ) ;
  begin
    if nodNum > 0 then  begin
      if (arTnod[nodNum -1].X = _x) and
         (arTnod[nodNum -1].Y = _y)  then
        exit ;
    end ;

    if partNum < _part then
       partNum := _part ;

    if curPart <> _part  then begin
      if checkPart then begin
        addPart ;
        arTnod[firstNode].Prev := nodNum - 1 ;
        arTnod[nodNum - 1].Next := firstNode ;
        firstNode := nodNum ;
        curPart   := _part ;
      end
      else begin
        firstNode := nodNum ;
        curPart   := _part ;
      end;

    end;

    if High(arTnod) + 1 = nodNum then begin
      SetLength( arTnod, 2*(High(arTnod) + 1) ) ;
    end;

    arTnod[nodNum].Id   := nodNum     ;
    arTnod[nodNum].Part := High( arPart ) ;
    arTnod[nodNum].X    := _x         ;
    arTnod[nodNum].Y    := _y         ;
    arTnod[nodNum].Prev := nodNum - 1 ;
    arTnod[nodNum].Next := nodNum + 1 ;
    arTnod[nodNum].Sel  := 0          ;

    Inc(nodNum) ;
  end;

  procedure T_Triangulate.doTriangulation ;
  var
    i, j : Integer ;
    j1, j2 : Integer ;
  begin
    for i := 0 to High(arPart) do begin
      id1 := arPart[i].Fst ;
      j1 := arPart[i].Fst +1 ;
      j2 := arPart[i].Lst -1 ;
      for j := j1 to j2 do begin
        id2 := arTnod[j].Id ;
        id3 := arTnod[j+1].Id ;
        addTriangle( id1, id2, id3 ) ;
      end;
    end;
  end;

  function T_Triangulate.Go
    : Integer ;
  begin
    if not checkPart then begin
      SetLength( arTnod, nodNum ) ;
      if High( arPart ) > 0 then
        SetLength( arPart, High( arPart ) );
    end;

    if nodNum < 3 then begin
      Result := 0 ;
      exit ;
    end;

    arTnod[firstNode].Prev := nodNum - 1 ;
    arTnod[nodNum - 1].Next := firstNode ;
    // Start with triangulation
    doTriangulation ;

    Result := triNum ;
  end;

  procedure T_Triangulate.addTriangle(
    const _id1 : Integer ;
    const _id2 : Integer ;
    const _id3 : Integer
  ) ;
  begin
    arTnod[_id2].Sel  := 1 ;
    arTnod[_id1].Next := arTnod[_id3].Id ;
    arTnod[_id3].Prev := arTnod[_id1].Id ;

    if High(arTri) + 1 = triNum then
      SetLength( arTri , 2*(High(arTri) + 1) ) ;

    arTri[triNum].X1 := arTnod[_id1].X ;
    arTri[triNum].Y1 := arTnod[_id1].Y ;
    arTri[triNum].X2 := arTnod[_id2].X ;
    arTri[triNum].Y2 := arTnod[_id2].Y ;
    arTri[triNum].X3 := arTnod[_id3].X ;
    arTri[triNum].Y3 := arTnod[_id3].Y ;

    Inc(triNum)  ;
  end;

  function T_Triangulate.GetCount : Integer ;
  begin
    Result := triNum ;
  end ;

  function T_Triangulate.GetTriangle(
    const _no : Integer
  ) : T_triangle ;
  begin
    Result := arTri[_no] ;
  end ;
{$ENDREGION}

{$REGION 'T_pathLowLevel'}
//==============================================================================
// T_pathLowLevel
//==============================================================================

  constructor T_pathLowLevel.Create ;
  begin
    inherited ;

    FIsClosed := False ;
    FCount    := 0 ;
    FNewPart  := True ;
    SetLength( aPath, 10 ) ;
  end ;

  destructor T_pathLowLevel.Destroy ;
  begin
    SetLength( aPath, 0 ) ;

    inherited ;
  end ;

  procedure T_pathLowLevel.NewPart ;
  begin
    FNewPart := True ;
  end ;

  procedure T_pathLowLevel.AddVertex(
    const _vec : TPointF
  ) ;
  begin
    if Length( aPath ) = FCount then
      SetLength( aPath, Length( aPath ) + 10 ) ;

    aPath[FCount].V := _vec ;

    if FNewPart then begin
      aPath[FCount].C := T_pathCommand.JumpTo ;
      oPartPoint := aPath[FCount] ;
      oPartPoint.C := T_pathCommand.Close ;
    end
    else
      aPath[FCount].C := T_pathCommand.ConnectTo ;

    FNewPart := False ;

    Inc( FCount ) ;
  end ;

  procedure T_pathLowLevel.MakeClosed ;
  begin
    AddVertex( oPartPoint.V ) ;
    aPath[FCount-1].C := T_pathCommand.Close ;

    FIsClosed := True ;
  end ;

  function T_pathLowLevel.GetIsClosed : Boolean ;
  begin
    Result := FIsClosed ;
  end ;

  function T_pathLowLevel.GetCount : Integer ;
  begin
    Result := FCount ;
  end ;

  function T_pathLowLevel.GetVertex(
    const _i : Integer
  ) : TPointF ;
  begin
    if ( _i >= 0 ) and ( _i < FCount ) then
      Result := aPath[_i].V
    else begin
      Result.X := 0.0 ;
      Result.Y := 0.0 ;
    end ;
  end ;

  function T_pathLowLevel.GetCommand(
    const _i : Integer
  ) : T_pathCommand ;
  begin
    if ( _i >= 0 ) and ( _i < FCount ) then
      Result := aPath[_i].C
    else
      Result := T_pathCommand.JumpTo ;
  end ;

  function T_pathLowLevel.GetBoundRect : TRectF ;
  var
    xmin : Double ;
    xmax : Double ;
    ymin : Double ;
    ymax : Double ;
    i    : Integer ;
  begin
    xmin := aPath[0].V.X ;
    xmax := aPath[0].V.X ;
    ymin := aPath[0].V.Y ;
    ymax := aPath[0].V.Y ;
    for i := 1 to FCount - 1 do begin
      xmin := Min( xmin, aPath[i].V.X ) ;
      xmax := Max( xmax, aPath[i].V.X ) ;
      ymin := Min( ymin, aPath[i].V.Y ) ;
      ymax := Max( ymax, aPath[i].V.Y ) ;
    end ;

    Result := TRectF.Create( xmin, ymin, xmax, ymax ) ;
  end ;

{$ENDREGION}

{$REGION 'T_path'}
//==============================================================================
// T_path
//==============================================================================

  constructor T_path.Create ;
  begin
    inherited ;

    oPath := T_pathLowLevel.Create ;

    bNative := True ;
    bMatrix := False ;
  end ;

  constructor T_path.Create(
    const _path : TPathData
  ) ;
  begin
    inherited Create ;

    oPathData := _path ;

    bNative := False ;
    bMatrix := False ;

    isNewPart := True ;
  end ;


  destructor T_path.Destroy ;
  begin
    if bNative then
      FreeObject( oPath )
    else
    if bMatrix then
      FreeObject( oPathData ) ;

    inherited ;
  end ;


  function T_path.fget_IsClosed : Boolean ;
  var
    p1 : TPointF ;
    p2 : TPointF ;
    i  : Integer ;
  begin
    if bNative then
      Result := T_pathLowLevel( oPath ).GetIsClosed
    else begin
      Result := False ;
      for i := oPathData.Count - 1 downto 0 do begin
        if oPathData.Points[i].Kind = TPathPointKind.Close then begin
          Result := True ;
          break ;
        end ;
      end ;
      if not Result then begin
        p1 := oPathData.Points[0].Point ;
        p2 := oPathData.Points[oPathData.Count-1].Point ;
        if ( p1.X = p2.X ) and ( p1.Y = p2.Y ) then
          Result := True ;
      end ;
    end ;
  end ;

  function T_path.fget_Count : Integer ;
  var
    last : Integer ;
  begin
    if bNative then
      Result := T_pathLowLevel( oPath ).GetCount
    else begin
      last := oPathData.Count - 1 ;
      if IsClosed and ( last > 0 ) and
         ( oPathData.Points[last] = oPathData.Points[last-1] ) then
        Result := oPathData.Count - 1
      else
        Result := oPathData.Count ;
    end ;
  end ;

  function T_path.fget_Vertex(
    const _i : Integer
  ) : TPointF ;
  begin
    if bNative then
      Result := T_pathLowLevel( oPath ).GetVertex( _i )
    else
      Result := oPathData.Points[_i].Point ;
  end ;

  function T_path.fget_Command(
    const _i : Integer
  ) : T_pathCommand ;
  begin
    if bNative then
      Result := T_pathLowLevel( oPath ).GetCommand( _i )
    else begin
      case oPathData.Points[_i].Kind of
        TPathPointKind.MoveTo  :
          Result := T_pathCommand.JumpTo ;
        TPathPointKind.LineTo  :
          Result := T_pathCommand.ConnectTo ;
        TPathPointKind.CurveTo :
          Result := T_pathCommand.CurveTo ;
        TPathPointKind.Close   :
          Result := T_pathCommand.Close ;
        else begin
          Assert( False, 'Untested case' ) ;
        end;
      end;
    end ;
  end ;

  procedure T_path.calcBezierCoeff(
    const _bezier : TCubicBezier ;
      out _ax     : Single ;
      out _bx     : Single ;
      out _cx     : Single ;
      out _ay     : Single ;
      out _by     : Single ;
      out _cy     : Single
  ) ;
  begin
    _cx := 3 * ( _bezier[1].X - _bezier[0].X ) ;
    _cy := 3 * ( _bezier[1].Y - _bezier[0].Y ) ;
    _bx := 3 * ( _bezier[2].X - _bezier[1].X ) - _cx ;
    _by := 3 * ( _bezier[2].Y - _bezier[1].Y ) - _cy ;
    _ax := _bezier[3].X - _bezier[0].X - _cx - _bx ;
    _ay := _bezier[3].Y - _bezier[0].Y - _cy - _by ;
  end ;


  function T_path.pointOnBezier(
    const _point  : TPointF ;
    const _ax     : Single ;
    const _bx     : Single ;
    const _cx     : Single ;
    const _ay     : Single ;
    const _by     : Single ;
    const _cy     : Single ;
    const _t      : Single
  ) : TPointF ;
  var
    tsq : Single ;
    tcu : Single ;
  begin
    tsq := _t * _t ;
    tcu := tsq * _t;
    Result.X := ( _ax * tcu ) + ( _bx * tsq ) + ( _cx * _t ) + _point.X ;
    Result.Y := ( _ay * tcu ) + ( _by * tsq ) + ( _cy * _t ) + _point.Y ;
  end ;


  function T_path.createBezier(
    const _bezier : TCubicBezier ;
    const _count  : Integer
  ) : TPolygon ;
  var
    ax : Single ;
    bx : Single ;
    cx : Single ;
    ay : Single ;
    by : Single ;
    cy : Single ;
    dt : Single ;
    t  : Single ;
    i  : Integer ;
  begin
    if _count = 0 then
      exit ;

    dt := 1 / ( 1 * _count - 1 ) ;
    t := 0 ;

    SetLength( Result, _count ) ;

    calcBezierCoeff( _bezier, ax, bx, cx, ay, by, cy ) ;
    for i := 0 to _count - 1 do begin
      Result[I] := PointOnBezier( _bezier[0], ax, bx, cx, ay, by, cy, t ) ;
      t := t + dt ;
    end ;
  end ;

  procedure T_path.NewPart ;
  begin
    if bNative then
      T_pathLowLevel( oPath ).NewPart
    else
      isNewPart := True ;
  end ;

  procedure T_path.AddVertex(
    const _x : Integer ;
    const _y : Integer
  ) ;
  var
    vec : TPointF ;
  begin
    vec.X := 1.0 * _x ;
    vec.Y := 1.0 * _y ;

    if bNative then
      T_pathLowLevel( oPath ).AddVertex( vec )
    else begin
      if isNewPart then
        oPathData.MoveTo( vec )
      else
        oPathData.LineTo( vec ) ;

      isNewPart := False ;
    end ;
  end ;

  procedure T_path.AddVertex(
    const _x : Single ;
    const _y : Single
  ) ;
  var
    vec : TPointF ;
  begin
    vec.X := _x ;
    vec.Y := _y ;

    if bNative then
      T_pathLowLevel( oPath ).AddVertex( vec )
    else begin
      if isNewPart then
        oPathData.MoveTo( vec )
      else
        oPathData.LineTo( vec ) ;

      isNewPart := False ;
    end ;
  end ;

  procedure T_path.AddVertex(
    const _vec : TPointF
  ) ;
  begin
    if bNative then
      T_pathLowLevel( oPath ).AddVertex( _vec )
    else begin
      if isNewPart then
        oPathData.MoveTo( _vec )
      else
        oPathData.LineTo( _vec ) ;

      isNewPart := False ;
    end ;
  end ;

  procedure T_path.MakeClosed ;
  begin
    if bNative then
      T_pathLowLevel( oPath ).MakeClosed
    else
      oPathData.ClosePath ;
  end ;


  function T_path.ExpandCurve(
    const _idx : Integer ;
      var _arr : TPolygon
  ) : Integer ;
  var
    bez : TCubicBezier ;
    pts : TPolygon ;
    len : Single ;
    cnt : Integer ;

    SP, CurPoint: TPointF;
    CurBounds: TRectF;
    i : Integer ;
    j : Integer ;
  begin
    j := _idx ;
    bez[0] := oPathData[j-1].Point ;
    bez[1] := oPathData[j].Point ;
    Inc( j ) ;
    bez[2] := oPathData[j].Point ;
    Inc( j ) ;
    bez[3] := oPathData[j].Point ;

    pts := createBezier( bez, 6 ) ;

    len := 0 ;
    for i := 0 to High( pts ) - 1 do
      len := len + ( pts[i] - pts[i+1] ).Length ;

    cnt := Round( len / 0.25 ) ;
    if cnt < 2 then begin
      SetLength( _arr, 2 ) ;
      _arr[0] := bez[0] ;
      _arr[1] := bez[3] ;
    end
    else
      _arr := createBezier( bez, cnt ) ;

    Result := j ;
  end ;


  function T_path.GetBoundRect : TRectF ;
  var
    xmin : Double ;
    xmax : Double ;
    ymin : Double ;
    ymax : Double ;
    i    : Integer ;
  begin
    if bNative then
      Result := T_pathLowLevel( oPath ).GetBoundRect
    else begin
      xmin := oPathData[0].Point.X ;
      xmax := oPathData[0].Point.X ;
      ymin := oPathData[0].Point.Y ;
      ymax := oPathData[0].Point.Y ;
      for i := 1 to Count - 1 do begin
        xmin := Min( xmin, oPathData[i].Point.X ) ;
        xmax := Max( xmax, oPathData[i].Point.X ) ;
        ymin := Min( ymin, oPathData[i].Point.Y ) ;
        ymax := Max( ymax, oPathData[i].Point.Y ) ;
      end ;
    end ;

    Result := TRectF.Create( xmin, ymin, xmax, ymax ) ;
  end ;


  procedure T_path.ApplyMatrix(
    const _mtx : TMatrix
  ) ;
  var
    pd : TPathData ;
    i  : Integer ;
    k  : Integer ;
  begin
    if bNative then
      exit ;

    if MatrixEqual( _mtx, TMatrix.Identity ) then
      exit ;

    pd := TPathData.Create ;
    k := 0 ;
    for i := 0 to oPathData.Count - 1 do begin
      case oPathData.Points[i].Kind of
        TPathPointKind.MoveTo : pd.MoveTo( oPathData.Points[i].Point*_mtx ) ;
        TPathPointKind.LineTo : pd.LineTo( oPathData.Points[i].Point*_mtx ) ;
        TPathPointKind.CurveTo :
          begin
            case k of
              0 : Inc( k ) ;
              1 : Inc( k ) ;
              2 :
                begin
                  pd.CurveTo( oPathData.Points[i-2].Point*_mtx,
                              oPathData.Points[i-1].Point*_mtx,
                              oPathData.Points[i  ].Point*_mtx
                            ) ;
                  k := 0 ;
                end ;
            end ;
          end ;
        TPathPointKind.Close : pd.ClosePath ;
      end ;
    end ;

    oPathData := pd ;

    bMatrix := True ;
  end ;

{$ENDREGION}

{$REGION 'T_strokeLowLevel'}
//==============================================================================
// T_strokeLowLevel
//==============================================================================

  constructor T_strokeLowLevel.Create(
    const _path  : T_path          ;
    const _join  : T_strokeJoin    ;
    const _cap   : T_strokeCap     ;
    const _width : Single          ;
    const _seq   : array of Single ;
    const _off   : Single          ;
    const _over1 : Boolean
  ) ;
  begin
    inherited Create ;

    oPathBase := _path ;
    iJoin     := _join ;
    iCap      := _cap ;

    if _width > 0.0 then
      sWidth := _width
    else
      sWidth := 1.0 ;

    if _width > 1.0 then
      bOverW1Join := False
    else
      bOverW1Join := _over1 ;

    iSteps := 4 + FloorS( ( sWidth - 0.1 ) / 5 ) ;
    sAngle := Pi / iSteps ;

    expandPath ;

    if Length( _seq ) > 0 then
      expandSequence( _seq, _off ) ;

    aSkeleton := TList<T_strokeVertex>.Create ;
    try
      buildSkeleton ;
      aGeometry := TList<T_strokeVertex>.Create ;
      if aSkeleton.Count > 1 then
        buildGeometry ;
    finally
      FreeObject( aSkeleton ) ;
    end ;
  end ;

  destructor T_strokeLowLevel.Destroy ;
  begin
    FreeObject( aGeometry ) ;

    if bExpandPath then
      FreeObject( oPath ) ;

    inherited ;
  end ;

  function T_strokeLowLevel.calcVecLen(
    const _v1   : TPointF
  ) : Single ;
  begin
    Result := Sqrt( _v1.X * _v1.X + _v1.Y * _v1.Y ) ;
  end ;

  function T_strokeLowLevel.calcScalarProd(
    const _v1   : TPointF ;
    const _v2   : TPointF
  ) : Single ;
  begin
    Result := _v1.X * _v2.X + _v1.Y * _v2.Y ;
  end ;

  function T_strokeLowLevel.calcVecSum(
    const _v1  : TPointF ;
    const _v2  : TPointF
  ) : TPointF ;
  var
    v : TPointF ;
  begin
    v.X := _v1.X + _v2.X ;
    v.Y := _v1.Y + _v2.Y ;

    Result := v ;
  end ;

  function T_strokeLowLevel.calcVecDif(
    const _v1  : TPointF ;
    const _v2  : TPointF
  ) : TPointF ;
  var
    v : TPointF ;
  begin
    v.X := _v1.X - _v2.X ;
    v.Y := _v1.Y - _v2.Y ;

    Result := v ;
  end ;

  function T_strokeLowLevel.calcVecNorm(
    const _v    : TPointF
  ) : TPointF ;
  var
    v   : TPointF ;
    len : Single ;
  begin
    len := Sqrt( _v.X * _v.X + _v.Y * _v.Y ) ;

    if len = 0 then begin
      v.X := 0.0 ;
      v.Y := 0.0 ;
    end
    else begin
      v.X := _v.X / len ;
      v.Y := _v.Y / len ;
    end ;

    Result := v ;
  end ;

  function T_strokeLowLevel.calcAngle(
    const _v0   : TPointF ;
    const _v1   : TPointF ;
    const _v2   : TPointF
  ) : Single ;
  var
    v1 : TPointF ;
    v2 : TPointF ;
    e  : Single ;
    d  : Single ;
    q  : Single ;
  begin
    v1 := calcVecDif( _v1, _v0 ) ;
    v2 := calcVecDif( _v2, _v0 ) ;

    e := calcScalarProd( v1, v2 ) ;
    d := calcVecLen( v1 ) * calcVecLen( v2 ) ;

    q := e / d ;
    if q >  1.0 then
      Result := 0.0
    else
    if q < -1.0 then
      Result := 1.0
    else
      Result := ArcCos( e / d ) ;
  end ;

  function T_strokeLowLevel.calcVecRot(
    const _v0   : TPointF ;
    const _v1   : TPointF ;
    const _phi  : Single
  ) : TPointF ;
  var
    v1 : TPointF ;
    vr : TPointF ;
    s  : Single ;
    c  : Single ;
  begin
    v1 := calcVecDif( _v1, _v0 ) ;

    s := Sin( _phi ) ;
    c := Cos( _phi ) ;

    vr.X := v1.X * c - v1.Y * s ;
    vr.Y := v1.X * s + v1.Y * c ;

    Result := calcVecSum( vr, _v0 ) ;
  end ;

  function T_strokeLowLevel.calcIntersection(
    const _v11  : TPointF ;
    const _v12  : TPointF ;
    const _v21  : TPointF ;
    const _v22  : TPointF ;
      out _res  : TPointF
  ) : Integer ;
  var
    w1 : TPointF ;
    w2 : TPointF ;
    a1 : Single ;
    a2 : Single ;
    t  : TPointF ;
    xpw1w2 : Single ;

    function xprod(
      const _vv1 : TPointF ;
      const _vv2 : TPointF
    ) : Single ;
    begin
      Result := _vv1.X*_vv2.Y - _vv1.Y*_vv2.X ;
    end ;

  begin
    _res.X := 0.0 ;
    _res.Y := 0.0 ;

    w1 := calcVecDif( _v12, _v11 ) ;
    w2 := calcVecDif( _v22, _v21 ) ;

    xpw1w2 := xprod( w1, w2 ) ;

    if xpw1w2 = 0 then begin
      Result := 0 ;
      exit ;
    end ;

    t := calcVecDif( _v21, _v11 ) ;
    a1 := xprod( t, w2 ) / xpw1w2 ;

    t := calcVecDif( _v11, _v21 ) ;
    a2 := xprod( t, w1 ) / ( -1.0 * xpw1w2 ) ;

    _res.X := _v11.X + a1*w1.X ;
    _res.Y := _v11.Y + a1*w1.Y ;

    if ( 0.0 < a1 ) and ( a1 < 1.0 ) and
       ( 0.0 < a2 ) and ( a2 < 1.0 ) then
      Result := 1
    else
      Result := 0 ;
  end ;

  function T_strokeLowLevel.calc3PointCenter(
    const _v1   : TPointF ;
    const _v2   : TPointF ;
    const _v3   : TPointF ;
      out _vr   : TPointF
  ) : Boolean ;
  var
    v : TPointF ;
    d : Single ;
  begin
    Result := True ;

    d := 2 * ( _v1.X * ( _v2.Y - _v3.Y ) +
               _v2.X * ( _v3.Y - _v1.Y ) +
               _v3.X * ( _v1.Y - _v2.Y )
             ) ;

    if d = 0 then begin
      _vr.X := 0.0 ;
      _vr.Y := 0.0 ;
      Result := False ;
      exit ;
    end ;

    v.X := ( ( _v1.X * _v1.X + _v1.Y * _v1.Y ) * ( _v2.Y - _v3.Y ) +
             ( _v2.X * _v2.X + _v2.Y * _v2.Y ) * ( _v3.Y - _v1.Y ) +
             ( _v3.X * _v3.X + _v3.Y * _v3.Y ) * ( _v1.Y - _v2.Y )
           ) / d ;
    v.Y := ( ( _v1.X * _v1.X + _v1.Y * _v1.Y ) * ( _v3.X - _v2.X ) +
             ( _v2.X * _v2.X + _v2.Y * _v2.Y ) * ( _v1.X - _v3.X ) +
             ( _v3.X * _v3.X + _v3.Y * _v3.Y ) * ( _v2.X - _v1.X )
           ) / d ;

    _vr := v ;
  end ;

  procedure T_strokeLowLevel.expandPath ;
  var
    poly : TPolygon ;
    i    : Integer ;
    j    : Integer ;
  begin
    bExpandPath := False ;
    for i := 0 to oPathBase.Count - 1 do begin
      if oPathBase.Command[i] = T_pathCommand.CurveTo then begin
        bExpandPath := True ;
        break ;
      end ;
    end ;

    if not bExpandPath then
      oPath := oPathBase
    else begin

      oPath := T_path.Create ;
      i := 0 ;
      while i < oPathBase.Count do begin
        case oPathBase.Command[i] of
          T_pathCommand.JumpTo :
            begin
              oPath.NewPart ;
              oPath.AddVertex( oPathBase.Vertex[i] ) ;
            end ;
          T_pathCommand.ConnectTo :
            begin
              oPath.AddVertex( oPathBase.Vertex[i] ) ;
            end ;
          T_pathCommand.CurveTo :
            begin
              i := oPathBase.ExpandCurve( i, poly ) ;
              for j := 0 to High( poly ) do
                oPath.AddVertex( poly[j] ) ;
            end ;
          T_pathCommand.Close :
            begin
              oPath.AddVertex( oPathBase.Vertex[i] ) ;
            end ;
        end ;

        Inc( i ) ;
      end ;

    end ;
  end ;

  procedure T_strokeLowLevel.expandSequence(
    const _seq : array of Single ;
    const _off : Single
  ) ;
  var
    len : Integer ;
    cnt : Integer ;
    dst : Single ;
    mlt : Single ;
    i   : Integer ;
  begin
    len := Length( _seq ) ;

    bSolidStart := True ;
    iSequence := 1 ;

    if len < 2 then
      exit ;

    SetLength( aSequence, len ) ;
    iSequence := len ;

    if sWidth < 2.0 then
      mlt := 2.0
    else
      mlt := sWidth ;

    for i := 0 to len - 1 do begin
      if _seq[i] > 0 then
        aSequence[i] := mlt * _seq[i]
      else
        aSequence[i] := mlt * ( -1.0 * _seq[i] ) ;
    end ;

    dst := 0.0 ;
    for i := 0 to len - 1 do
      dst := dst + aSequence[i] ;

    if _off = 0.0 then begin
      sOffset := 0.0 ;
      iOffset := 0 ;
    end
    else begin
      cnt := FloorS( Abs( _off ) / dst ) ;
      sOffset := Abs( _off ) - cnt * dst ;
      if _off < 0.0 then
        sOffset := dst - sOffset ;
    end ;

    dst := 0.0 ;
    for i := 0 to len - 1 do begin
      dst := dst + aSequence[i] ;
      if dst > sOffset then
        break ;
      bSolidStart := not bSolidStart ;
    end ;

    dst := dst - aSequence[i] ;
    sOffset := sOffset - dst ;

    iOffset := i ;
  end ;

  procedure T_strokeLowLevel.buildSkeleton ;
  var
    interm  : TList<T_strokeVertex> ;
    lengths : array of Single ;
    vectors : array of TPointF ;
    apos    : Integer ;
    scmd    : T_strokeCommand ;
    v       : TPointF ;
    vt      : TPointF ;
    i       : Integer ;
    clen    : Single ;
    nlen    : Single ;
    llen    : Single ;
    spos    : Integer ;
    bspace  : Boolean ;
    vc      : T_strokeVertex ;
    ow      : Boolean ;

    procedure inc_spos ;
    begin
      Inc( spos ) ;

      if spos = iSequence then
        spos := 0 ;
    end ;

    procedure dec_spos ;
    begin
      Dec( spos ) ;

      if spos < 0 then
        spos := iSequence - 1 ;
    end ;

    function must_modify_path_weak : Boolean ;
    begin
      if ( not oPath.IsClosed ) and
         ( ( sWidth > 1.0 ) or bOverW1Join ) then
        Result := True
      else
        Result := False ;
    end ;

    function must_modify_path : Boolean ;
    begin
      if ( not oPath.IsClosed ) and
         ( ( sWidth > 1.0 ) or bOverW1Join ) and
         ( ( iCap = T_strokeCap.Square ) or
           ( iCap = T_strokeCap.Round  )
         ) then
        Result := True
      else
        Result := False ;
    end ;

    procedure calc_len_vec ;
    var
      vv1 : TPointF ;
      vv2 : TPointF ;
      vv3 : TPointF ;
    begin
      if interm[i].C = T_strokeCommand.MoveTo then
        exit ;

      vv1 := interm[i-1].V ;
      vv2 := interm[i].V ;
      vv3 := calcVecDif( vv2, vv1 ) ;

      lengths[apos] := Sqrt( vv3.X*vv3.X + vv3.Y*vv3.Y ) ;
      vectors[apos].X := vv3.X / lengths[apos] ;
      vectors[apos].Y := vv3.Y / lengths[apos] ;

      if ( i = 0 ) or ( i = interm.Count - 1 ) then begin
        if must_modify_path then
          lengths[apos] := lengths[apos] + sWidth / 2.0 ;
      end ;

      Inc( apos ) ;
    end ;

    procedure apply_style ;
    var
      slen : Single ;
      rtmp : TPointF ;
      vava : TPointF ;
    begin
      v := interm[i].V ;

      if scmd = T_strokeCommand.MoveTo then begin
        clen := sOffset ;
        nlen := lengths[apos] ;
        llen := sOffset ;
        spos := iOffset ;
        bspace := not bSolidStart ;

        vc.V := v ;
        vc.C := scmd ;
        aSkeleton.Add( vc ) ;
      end
      else begin
        if ( not bspace ) and ( clen - nlen < 0.51*sWidth ) then
          clen := nlen + 0.51*sWidth ;
        llen := clen - nlen - aSequence[spos] ;
        clen := clen - aSequence[spos] ;
        nlen := nlen + lengths[apos] ;
      end ;

      slen := llen ;

      while True do begin

        clen := clen + aSequence[spos] ;

        if clen > nlen then begin

          if bspace then
            break ;

          if nlen - clen + aSequence[spos] < 0.51*sWidth then begin
            aSkeleton.Delete( aSkeleton.Count - 1 ) ;
            vc.V := interm[i+1].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aSkeleton.Add( vc ) ;
            break ;
          end ;

          vc.V := interm[i+1].V ;
          vc.C := T_strokeCommand.LineTo ;
          aSkeleton.Add( vc ) ;

          break ;
        end ;

        slen := slen + aSequence[spos] ;

        rtmp := vc.V ;
        rtmp.X := v.X + slen * vectors[apos].X ;
        rtmp.Y := v.Y + slen * vectors[apos].Y ;
        vc.V := rtmp ;

        if bspace then
          vc.C := T_strokeCommand.MoveTo
        else
          vc.C := T_strokeCommand.LineTo ;
        aSkeleton.Add( vc ) ;

        inc_spos ;
        if spos = iSequence then
          spos := 0 ;

        bspace := not bspace ;
      end ;
    end ;

  begin

    if iSequence > 1 then
      interm := TList<T_strokeVertex>.Create
    else
      interm := aSkeleton ;

    for i := 0 to oPath.Count - 1 do begin

      if oPath.Command[i] = T_pathCommand.JumpTo then
        scmd := T_strokeCommand.MoveTo
      else
        scmd := T_strokeCommand.LineTo ;

      ow := False ;
      if i > 0 then begin
        if ( oPath.Vertex[i].X = oPath.Vertex[i-1].X ) and
           ( oPath.Vertex[i].Y = oPath.Vertex[i-1].Y ) and
           ( scmd = T_strokeCommand.LineTo   ) then
          continue
        else
        if ( scmd = T_strokeCommand.MoveTo ) and
           ( interm[interm.Count-1].C =
               T_strokeCommand.MoveTo ) then
          ow := True ;
      end ;

      if i = 0 then begin
        if must_modify_path then begin
          vt := calcVecDif( oPath.Vertex[0], oPath.Vertex[1] ) ;
          vt := calcVecNorm( vt ) ;
          v.X := oPath.Vertex[0].X + sWidth/2 * vt.X ;
          v.Y := oPath.Vertex[0].Y + sWidth/2 * vt.Y ;
        end
        else
          v := oPath.Vertex[i] ;
      end
      else
      if i = oPath.Count - 1 then begin
        if must_modify_path then begin
          vt := calcVecDif( oPath.Vertex[oPath.Count-2],
                            oPath.Vertex[oPath.Count-1]
                          ) ;
          vt := calcVecNorm( vt ) ;
          v.X := oPath.Vertex[oPath.Count-1].X -
                            sWidth/2 * vt.X ;
          v.Y := oPath.Vertex[oPath.Count-1].Y -
                            sWidth/2 * vt.Y ;
        end
        else
          v := oPath.Vertex[i] ;
      end
      else
        v := oPath.Vertex[i] ;

      vc.V := v ;
      vc.C := scmd ;
      if ow then
        interm[interm.Count-1] := vc
      else
        interm.Add( vc ) ;

    end ;

    for i := interm.Count - 1 downto 0 do begin
      if interm[i].C <> T_strokeCommand.MoveTo then
        break
      else
        interm.Delete( i ) ;
    end ;

    if interm.Count = 0 then begin
      if iSequence > 1 then
        interm.Free ;
      exit ;
    end ;

    if iSequence > 1 then begin

      SetLength( lengths, interm.Count - 1 ) ;
      SetLength( vectors, interm.Count - 1 ) ;

      apos := 0 ;
      for i := 1 to interm.Count - 1 do
        calc_len_vec ;

      apos := 0 ;
      for i := 0 to interm.Count - 2 do begin
        if interm[i+1].C = T_strokeCommand.MoveTo then begin
          Inc( apos ) ;
          continue ;
        end
        else
        if interm[i].C = T_strokeCommand.MoveTo then
          scmd := T_strokeCommand.MoveTo
        else begin
          scmd := T_strokeCommand.LineTo ;
          Inc( apos ) ;
        end ;

        apply_style ;
      end ;

      interm.Free ;

    end ;

    for i := aSkeleton.Count - 1 downto 0 do begin
      if aSkeleton[i].C <> T_strokeCommand.MoveTo then
        break
      else
        aSkeleton.Delete( i ) ;
    end ;

  end ;

  procedure T_strokeLowLevel.buildGeometry ;
  var
    scmd     : T_strokeCommand ;
    len      : Integer ;
    ageotemp : TList<T_strokeVertex> ;
    v        : TPointF ;
    vc       : T_strokeVertex ;
    i        : Integer ;
    j        : Integer ;
    k        : Integer ;
    l        : Integer ;

    procedure build_segment ;
    var
      x, y, nx, ny, w, dx, dy, phi : Single ;
      vsin, vcos : Single ;
      ort : Integer ;
    begin
      if ( i = len - 1 ) or
         ( aSkeleton[i+1].C = T_strokeCommand.MoveTo ) then
        exit ;

      w := sWidth/2 ;

      x := aSkeleton[i].V.X ;
      y := aSkeleton[i].V.Y ;

      nx := aSkeleton[i+1].V.X ;
      ny := aSkeleton[i+1].V.Y ;

      if ( nx = x ) and ( ny = y ) then
        exit ;

      if ( nx > x ) and ( ny > y ) then begin
        phi := ArcTan2( ny - y, nx - x ) ;
        vsin := Sin( phi ) ;
        vcos := Cos( phi ) ;
        ort := 1 ;
      end
      else
      if ( nx < x ) and ( ny < y ) then begin
        phi := ArcTan2( y - ny, x - nx ) ;
        vsin := Sin( phi ) ;
        vcos := Cos( phi ) ;
        ort := 2 ;
      end
      else
      if ( nx > x ) and ( ny < y ) then begin
        phi := ArcTan2( y - ny, nx - x ) ;
        vsin := Sin( phi ) ;
        vcos := Cos( phi ) ;
        ort := 3 ;
      end
      else
      if ( nx < x ) and ( ny > y ) then begin
        phi := ArcTan2( ny - y ,x - nx ) ;
        vsin := Sin( phi ) ;
        vcos := Cos( phi ) ;
        ort := 4 ;
      end
      else
      if ( nx > x ) and ( ny = y ) then begin
        vsin := 0 ;
        vcos := 1 ;
        ort := 5 ;
      end
      else
      if ( nx < x ) and ( ny = y ) then begin
        vsin := 0 ;
        vcos := 1 ;
        ort := 6 ;
      end
      else
      if ( nx = x ) and ( ny > y ) then begin
        vsin := 1 ;
        vcos := 0 ;
        ort := 7 ;
      end
      else
      if ( nx = x ) and ( ny < y ) then begin
        vsin := 1 ;
        vcos := 0 ;
        ort := 8 ;
      end ;

      dx := w * vsin ;
      dy := w * vcos ;

      vc.C := aSkeleton[i].C ;

      case ort of
        1 : begin
              vc.V.X := x - dx ;
              vc.V.Y := y + dy ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := x + dx ;
              vc.V.Y := y - dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx - dx ;
              vc.V.Y := ny + dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx + dx ;
              vc.V.Y := ny - dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
            end ;
        2 : begin
              vc.V.X := x + dx ;
              vc.V.Y := y - dy ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := x - dx ;
              vc.V.Y := y + dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx + dx ;
              vc.V.Y := ny - dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx - dx ;
              vc.V.Y := ny + dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
            end ;
        3 : begin
              vc.V.X := x + dx ;
              vc.V.Y := y + dy ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := x - dx ;
              vc.V.Y := y - dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx + dx ;
              vc.V.Y := ny + dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx - dx ;
              vc.V.Y := ny - dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
            end ;
        4 : begin
              vc.V.X := x - dx ;
              vc.V.Y := y - dy ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := x + dx ;
              vc.V.Y := y + dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx - dx ;
              vc.V.Y := ny - dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx + dx ;
              vc.V.Y := ny + dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
            end ;
        5 : begin
              vc.V.X := x ;
              vc.V.Y := y + dy ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := x ;
              vc.V.Y := y - dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx ;
              vc.V.Y := ny + dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx ;
              vc.V.Y := ny - dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
            end ;
        6 : begin
              vc.V.X := x ;
              vc.V.Y := y - dy ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := x ;
              vc.V.Y := y + dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx ;
              vc.V.Y := ny - dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx ;
              vc.V.Y := ny + dy ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
            end ;
        7 : begin
              vc.V.X := x - dx ;
              vc.V.Y := y ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := x + dx ;
              vc.V.Y := y ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx - dx ;
              vc.V.Y := ny ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx + dx ;
              vc.V.Y := ny ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
            end ;
        8 : begin
              vc.V.X := x + dx ;
              vc.V.Y := y ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := x - dx ;
              vc.V.Y := y ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx + dx ;
              vc.V.Y := ny ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
              Inc( j ) ;
              vc.V.X := nx - dx ;
              vc.V.Y := ny ;
              vc.C := T_strokeCommand.LineTo ;
              ageotemp.Add( vc ) ;
            end ;
      end ;

      Inc( j ) ;
    end ;

    procedure mod_copy ;
    begin
      if k > ageotemp.Count - 1 then begin
        Dec( k, 2 ) ;
        Dec( j, 2 ) ;
      end ;

      aGeometry.Add( ageotemp[k] ) ;
      Inc( j ) ;
      Inc( k ) ;
      aGeometry.Add( ageotemp[k] ) ;
      Inc( j ) ;
      Inc( k ) ;
    end ;

    procedure mod_cap_round_start ;
    var
      psi : Single ;
      vt  : TPointF ;
      vn  : TPointF ;
      vv  : TPointF ;
      v0  : TPointF ;
      v1  : TPointF ;
      v2  : TPointF ;
      ii  : Integer ;
    begin
      vt := calcVecDif( aSkeleton[i+1].V, aSkeleton[i].V ) ;
      vn := calcVecNorm( vt ) ;
      vn.X := sWidth/2 * vn.X ;
      vn.Y := sWidth/2 * vn.Y ;

      v0 := calcVecSum( aSkeleton[i].V, vn ) ;
      v1 := calcVecSum( ageotemp[k].V, vn ) ;
      v2 := calcVecSum( ageotemp[k+1].V, vn ) ;

      vc.V := v0 ;
      vc.C := T_strokeCommand.MoveToFan ;
      aGeometry.Add( vc ) ;
      Inc( j ) ;

      for ii := 0 to iSteps do begin
        vv := calcVecRot( v0,
                          v1,
                          ii*sAngle
                        ) ;

        if ii = 0 then
          vc.V := v1
        else
        if ii = iSteps then
          vc.V := v2
        else
          vc.V := vv ;
        vc.C := T_strokeCommand.LineTo ;
        aGeometry.Add( vc ) ;
        Inc( j ) ;
      end ;

      vc.V := v1 ;
      vc.C := T_strokeCommand.MoveTo ;
      aGeometry.Add( vc ) ;
      Inc( j ) ;
      vc.V := v2 ;
      vc.C := T_strokeCommand.LineTo ;
      aGeometry.Add( vc ) ;
      Inc( j ) ;

      Inc( k, 2 ) ;
    end ;

    procedure mod_cap_round_end ;
    var
      psi : Single ;
      vt  : TPointF ;
      vn  : TPointF ;
      vv  : TPointF ;
      v0  : TPointF ;
      v1  : TPointF ;
      v2  : TPointF ;
      ii  : Integer ;
    begin
      vt := calcVecDif( aSkeleton[i-1].V, aSkeleton[i].V ) ;
      vn := calcVecNorm( vt ) ;
      vn.X := sWidth/2 * vn.X ;
      vn.Y := sWidth/2 * vn.Y ;

      v0 := calcVecSum( aSkeleton[i].V, vn ) ;
      v1 := calcVecSum( ageotemp[k].V, vn ) ;
      v2 := calcVecSum( ageotemp[k+1].V, vn ) ;

      vc.V := v1 ;
      vc.C := T_strokeCommand.LineTo ;
      aGeometry.Add( vc ) ;
      Inc( j ) ;
      vc.V := v2 ;
      vc.C := T_strokeCommand.LineTo ;
      aGeometry.Add( vc ) ;
      Inc( j ) ;

      vc.V := v0 ;
      vc.C := T_strokeCommand.MoveToFan ;
      aGeometry.Add( vc ) ;
      Inc( j ) ;

      for ii := 0 to iSteps do begin
        vv := calcVecRot( v0,
                          v2,
                          ii*sAngle
                        ) ;

        if ii = 0 then
          vc.V := v2
        else
        if ii = iSteps then
          vc.V := v1
        else
          vc.V := vv ;
        vc.C := T_strokeCommand.LineTo ;
        aGeometry.Add( vc ) ;
        Inc( j ) ;
      end ;

      Inc( k, 2 ) ;
    end ;

    procedure mod_join_miter( const _closed : Boolean ) ;
    var
      vi0 : TPointF ;
      vi1 : TPointF ;
      vi  : TPointF ;
      vt  : TPointF ;
      len : Single ;
      c   : Integer ;
      e   : Integer ;
    begin
      if _closed then
        l := iJoinIndexK
      else
        l := k ;

      if l + 4 > ageotemp.Count - 1 then begin
        mod_copy ;
        exit ;
      end ;

      e := 0 ;
      c := calcIntersection(
             ageotemp[k-2].V,
             ageotemp[k  ].V,
             ageotemp[l+2].V,
             ageotemp[l+4].V,
             vi0
           ) ;
      vi := vi0 ;

      if c = 0 then begin
        e := 1 ;
        c := calcIntersection(
               ageotemp[k-1].V,
               ageotemp[k+1].V,
               ageotemp[l+3].V,
               ageotemp[l+5].V,
               vi1
             ) ;
        vi := vi1 ;
      end ;

      if c = 0 then begin
        e := 0 ;
        c := calcIntersection(
               ageotemp[k-2].V,
               ageotemp[k-1].V,
               ageotemp[l+2].V,
               ageotemp[l+4].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 1 ;
        c := calcIntersection(
               ageotemp[k-2].V,
               ageotemp[k-1].V,
               ageotemp[l+3].V,
               ageotemp[l+5].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 0 ;
        c := calcIntersection(
               ageotemp[k-2].V,
               ageotemp[k  ].V,
               ageotemp[l+4].V,
               ageotemp[l+5].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 1 ;
        c := calcIntersection(
               ageotemp[k-1].V,
               ageotemp[k+1].V,
               ageotemp[l+4].V,
               ageotemp[l+5].V,
               vi
             ) ;

        if c = 0 then
          e := 2 ;
      end ;

      case e of
        0 :
        begin
          vt := calcVecDif( aSkeleton[i].V, vi0 ) ;

          if calcVecLen( vt ) > 3.0 * sWidth then begin
            vc.V := vi ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[k+1].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := vi ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[k+1].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            if _closed then begin
              vc.V := vi ;
              vc.C := T_strokeCommand.MoveTo ;
              aGeometry[iJoinIndexJ  ] := vc ;
              vc.V := ageotemp[l+3].V ;
              vc.C := T_strokeCommand.LineTo ;
              aGeometry[iJoinIndexJ+1] := vc ;
              Dec( k, 2 ) ;
            end
            else begin
              vc.V := vi ;
              vc.C := T_strokeCommand.MoveTo ;
              aGeometry.Add( vc ) ;
              Inc( j ) ;
              vc.V := ageotemp[l+3].V ;
              vc.C := T_strokeCommand.LineTo ;
              aGeometry.Add( vc ) ;
              Inc( j ) ;
            end ;
          end
          else begin
            vc.V := vi ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := calcVecSum( aSkeleton[i].V, vt ) ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;

            if _closed then begin
              vc.V := vi ;
              vc.C := T_strokeCommand.MoveTo ;
              aGeometry[iJoinIndexJ  ] := vc ;
              vc.V := calcVecSum( aSkeleton[i].V, vt ) ;
              vc.C := T_strokeCommand.LineTo ;
              aGeometry[iJoinIndexJ+1] := vc ;
              Dec( k, 2 ) ;
            end ;
          end ;
        end ;
        1 : begin
          vt := calcVecDif( aSkeleton[i].V, vi1 ) ;

          if calcVecLen( vt ) > 3.0 * sWidth then begin
            vc.V := ageotemp[k].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := vi ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[k].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := vi ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            if _closed then begin
              vc.V := ageotemp[l+2].V ;
              vc.C := T_strokeCommand.MoveTo ;
              aGeometry[iJoinIndexJ  ] := vc ;
              vc.V := vi ;
              vc.C := T_strokeCommand.LineTo ;
              aGeometry[iJoinIndexJ+1] := vc ;
              Dec( k, 2 ) ;
            end
            else begin
              vc.V := ageotemp[l+2].V ;
              vc.C := T_strokeCommand.MoveTo ;
              aGeometry.Add( vc ) ;
              Inc( j ) ;
              vc.V := vi ;
              vc.C := T_strokeCommand.LineTo ;
              aGeometry.Add( vc ) ;
              Inc( j ) ;
            end ;
          end
          else begin
            vc.V := calcVecSum( aSkeleton[i].V, vt ) ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := vi ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;

            if _closed then begin
              vc.V := calcVecSum( aSkeleton[i].V, vt ) ;
              vc.C := T_strokeCommand.MoveTo ;
              aGeometry[iJoinIndexJ  ] := vc ;
              vc.V := vi ;
              vc.C := T_strokeCommand.LineTo ;
              aGeometry[iJoinIndexJ+1] := vc ;
              Dec( k, 2 ) ;
            end ;
          end ;
        end ;
        2 : begin
          vc.V := ageotemp[k].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := ageotemp[k+1].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          if _closed then begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry[iJoinIndexJ  ] := vc ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry[iJoinIndexJ+1] := vc ;
            Dec( k, 2 ) ;
          end
          else begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;
        end ;
      end ;

      Inc( k, 4 ) ;
    end ;

    procedure mod_join_bevel( const _closed : Boolean ) ;
    var
      vi : TPointF ;
      c  : Integer ;
      e  : Integer ;
    begin
      if _closed then
        l := iJoinIndexK
      else
        l := k ;

      if l + 4 > ageotemp.Count - 1 then begin
        mod_copy ;
        exit ;
      end ;

      e := 0 ;
      c := calcIntersection(
             ageotemp[k-2].V,
             ageotemp[k  ].V,
             ageotemp[l+2].V,
             ageotemp[l+4].V,
             vi
           ) ;

      if c = 0 then begin
        e := 1 ;
        c := calcIntersection(
               ageotemp[k-1].V,
               ageotemp[k+1].V,
               ageotemp[l+3].V,
               ageotemp[l+5].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 0 ;
        c := calcIntersection(
               ageotemp[k-2].V,
               ageotemp[k-1].V,
               ageotemp[l+2].V,
               ageotemp[l+4].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 1 ;
        c := calcIntersection(
               ageotemp[k-2].V,
               ageotemp[k-1].V,
               ageotemp[l+3].V,
               ageotemp[l+5].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 0 ;
        c := calcIntersection(
               ageotemp[k-2].V,
               ageotemp[k  ].V,
               ageotemp[l+4].V,
               ageotemp[l+5].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 1 ;
        c := calcIntersection(
               ageotemp[k-1].V,
               ageotemp[k+1].V,
               ageotemp[l+4].V,
               ageotemp[l+5].V,
               vi
             ) ;

        if c = 0 then
          e := 2 ;
      end ;

      case e of
        0 :
        begin
          vc.V := vi ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := ageotemp[k+1].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := vi ;
          vc.C := T_strokeCommand.MoveTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := ageotemp[k+1].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := ageotemp[l+3].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          if _closed then begin
            vc.V := vi ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry[iJoinIndexJ  ] := vc ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry[iJoinIndexJ+1] := vc ;
            Dec( k, 2 ) ;
          end
          else begin
            vc.V := vi ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;
        end ;
        1 : begin
          vc.V := ageotemp[k].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := vi ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := ageotemp[k].V ;
          vc.C := T_strokeCommand.MoveTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := vi ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := ageotemp[l+2].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          if _closed then begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry[iJoinIndexJ  ] := vc ;
            vc.V := vi ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry[iJoinIndexJ+1] := vc ;
            Dec( k, 2 ) ;
          end
          else begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := vi ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;
        end ;
        2 : begin
          vc.V := ageotemp[k].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := ageotemp[k+1].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          if _closed then begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry[iJoinIndexJ  ] := vc ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry[iJoinIndexJ+1] := vc ;
            Dec( k, 2 ) ;
          end
          else begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;
        end ;
      end ;

      Inc( k, 4 ) ;
    end ;

    procedure mod_join_round( const _closed : Boolean ) ;
    var
      vi : TPointF ;
      c  : Integer ;
      e  : Integer ;

      phi : Single ;
      psi : Single ;
      ii  : Integer ;
      kk  : Integer ;
      vv  : TPointF ;
    begin
      if _closed then
        l := iJoinIndexK
      else
        l := k ;

      if l + 4 > ageotemp.Count - 1 then begin
        mod_copy ;
        exit ;
      end ;

      e := 0 ;
      c := calcIntersection(
             ageotemp[k-2].V,
             ageotemp[k  ].V,
             ageotemp[l+2].V,
             ageotemp[l+4].V,
             vi
           ) ;

      if c = 0 then begin
        e := 1 ;
        c := calcIntersection(
               ageotemp[k-1].V,
               ageotemp[k+1].V,
               ageotemp[l+3].V,
               ageotemp[l+5].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 0 ;
        c := calcIntersection(
               ageotemp[k-2].V,
               ageotemp[k-1].V,
               ageotemp[l+2].V,
               ageotemp[l+4].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 1 ;
        c := calcIntersection(
               ageotemp[k-2].V,
               ageotemp[k-1].V,
               ageotemp[l+3].V,
               ageotemp[l+5].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 0 ;
        c := calcIntersection(
               ageotemp[k-2].V,
               ageotemp[k  ].V,
               ageotemp[l+4].V,
               ageotemp[l+5].V,
               vi
             ) ;
      end ;

      if c = 0 then begin
        e := 1 ;
        c := calcIntersection(
               ageotemp[k-1].V,
               ageotemp[k+1].V,
               ageotemp[l+4].V,
               ageotemp[l+5].V,
               vi
             ) ;

        if c = 0 then
          e := 2 ;
      end ;

      case e of
        0 :
        begin
          phi := calcAngle( aSkeleton[i].V,
                            ageotemp[k+1].V,
                            ageotemp[l+3].V
                          ) ;
          kk := FloorS( iSteps * phi / Pi ) + 1 ;
          psi := phi / kk ;

          vc.V := vi ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := ageotemp[k+1].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := vi ;
          vc.C := T_strokeCommand.MoveToFan ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;

          for ii := 0 to kk do begin
            vv := calcVecRot( aSkeleton[i].V,
                              ageotemp[k+1].V,
                              ii*psi
                            ) ;
            vc.V := vv ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;

          if _closed then begin
            vc.V := vi ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry[iJoinIndexJ  ] := vc ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry[iJoinIndexJ+1] := vc ;
            Dec( k, 2 ) ;
          end
          else begin
            vc.V := vi ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;

        end ;
        1 : begin
          phi := calcAngle( aSkeleton[i].V,
                            ageotemp[k  ].V,
                            ageotemp[l+2].V
                          ) ;
          kk := FloorS( iSteps * phi / Pi ) + 1 ;
          psi := phi / kk ;

          vc.V := ageotemp[k].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := vi ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := vi ;
          vc.C := T_strokeCommand.MoveToFan ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;

          for ii := kk downto 0 do begin
            vv := calcVecRot( aSkeleton[i].V,
                              ageotemp[l+2].V,
                              ii*psi
                            ) ;
            vc.V := vv ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;

          if _closed then begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry[iJoinIndexJ  ] := vc ;
            vc.V := vi ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry[iJoinIndexJ+1] := vc ;
            Dec( k, 2 ) ;
          end
          else begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := vi ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;

        end ;
        2 : begin
          vc.V := ageotemp[k].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;
          vc.V := ageotemp[k+1].V ;
          vc.C := T_strokeCommand.LineTo ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;

          vc.V := aSkeleton[i].V ;
          vc.C := T_strokeCommand.MoveToFan ;
          aGeometry.Add( vc ) ;
          Inc( j ) ;

          kk := 2 * iSteps + 1 ;
          psi := 2 * Pi / kk ;

          for ii := 0 to kk do begin
            vv := calcVecRot( aSkeleton[i].V,
                              ageotemp[k+1].V,
                              ii*psi
                            ) ;
            vc.V := vv ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;

          if _closed then begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry[iJoinIndexJ  ] := vc ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry[iJoinIndexJ+1] := vc ;
            Dec( k, 2 ) ;
          end
          else begin
            vc.V := ageotemp[l+2].V ;
            vc.C := T_strokeCommand.MoveTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
            vc.V := ageotemp[l+3].V ;
            vc.C := T_strokeCommand.LineTo ;
            aGeometry.Add( vc ) ;
            Inc( j ) ;
          end ;
        end ;
      end ;

      Inc( k, 4 ) ;
    end ;

    function must_close : Boolean ;
    begin
      if iSequence = 1 then
        Result := oPath.IsClosed and bSolidStart
      else
        Result := False ;
    end ;

  begin

    ageotemp := TList<T_strokeVertex>.Create ;
    try

      if ( sWidth <= 1.0 ) and ( not bOverW1Join ) then
        aGeometry.AddRange( aSkeleton.ToArray )
      else begin
        len := aSkeleton.Count ;
        j   := 0 ;
        for i := 0 to len - 1 do
          build_segment ;
      end ;

      if ( sWidth > 1.0 ) or bOverW1Join then begin

        len := aSkeleton.Count ;

        j := 0 ;
        k := 0 ;

        for i := 0 to len - 1 do begin
          if ( i = 0 ) and must_close then begin
            mod_copy ;
            iJoinIndexK := -2 ;
            iJoinIndexJ :=  0 ;
          end
          else
          if ( i = len - 1 ) or
             ( aSkeleton[i+1].C = T_strokeCommand.MoveTo ) then begin
            if must_close then begin
              case iJoin of
                T_strokeJoin.Miter : mod_join_miter( True ) ;
                T_strokeJoin.Bevel : mod_join_bevel( True ) ;
                T_strokeJoin.Round : mod_join_round( True ) ;
              end ;
            end
            else begin
              case iCap of
                T_strokeCap.Butt   : mod_copy ;
                T_strokeCap.Round  : mod_cap_round_end ;
                T_strokeCap.Square : mod_copy ;
              end ;
            end ;
          end
          else
          if aSkeleton[i].C = T_strokeCommand.MoveTo then begin
            if must_close then begin
              mod_copy ;
              iJoinIndexK := k - 4 ;
              iJoinIndexJ := j - 2 ;
            end
            else begin
              case iCap of
                T_strokeCap.Butt   : mod_copy ;
                T_strokeCap.Round  : mod_cap_round_start ;
                T_strokeCap.Square : mod_copy ;
              end ;
            end ;
          end
          else begin
            case iJoin of
              T_strokeJoin.Miter : mod_join_miter( False ) ;
              T_strokeJoin.Bevel : mod_join_bevel( False ) ;
              T_strokeJoin.Round : mod_join_round( False ) ;
            end ;
          end ;
        end ;

      end ;

    finally
      FreeObject( ageotemp ) ;
    end ;

  end ;

  function T_strokeLowLevel.GetWidth : Single ;
  begin
    Result := sWidth ;
  end ;

  function T_strokeLowLevel.GetVertex(
    const _i : Integer
  ) : TPointF ;
  var
    cnt : Integer ;
  begin
    cnt := aGeometry.Count ;

    if ( _i >= 0   ) and
       ( _i <  cnt ) then
      Result := aGeometry[_i].V
    else begin
      Result.X := 0.0 ;
      Result.Y := 0.0 ;
    end ;
  end ;

  function T_strokeLowLevel.GetCommand(
    const _i : Integer
  ) : T_strokeCommand ;
  var
    cnt : Integer ;
  begin
    cnt := aGeometry.Count ;

    if ( _i >= 0   ) and
       ( _i <  cnt ) then
      Result := aGeometry[_i].C
    else
      Result := T_strokeCommand.MoveTo ;
  end ;

  function T_strokeLowLevel.GetCount : Integer ;
  begin
    Result := aGeometry.Count ;
  end ;
{$ENDREGION}

{$REGION 'T_stroke'}
//==============================================================================
// T_stroke
//==============================================================================

  constructor T_stroke.Create(
    const _path  : T_path       ;
    const _join  : T_strokeJoin ;
    const _cap   : T_strokeCap  ;
    const _width : Single       ;
    const _over1 : Boolean
  ) ;
  var
    arr : array of Single ;
  begin
    inherited Create ;

    SetLength( arr, 1 ) ;
    arr[0] := 1.0 ;

    Create( _path, _join, _cap, _width, arr, 0.0, _over1 ) ;
  end ;

  constructor T_stroke.Create(
    const _path  : T_path          ;
    const _join  : T_strokeJoin    ;
    const _cap   : T_strokeCap     ;
    const _width : Single          ;
    const _seq   : array of Single ;
    const _off   : Single          ;
    const _over1 : Boolean
  ) ;
  begin
    inherited Create ;

    FPath := _path ;

    oStroke :=
      T_strokeLowLevel.Create(
        _path, _join, _cap, _width, _seq, _off, _over1
      ) ;
  end ;

  destructor T_stroke.Destroy ;
  begin
    FreeObject( oStroke ) ;

    inherited ;
  end ;

  function T_stroke.fget_Width : Single ;
  begin
    Result := T_strokeLowLevel( oStroke ).GetWidth ;
  end ;

  function T_stroke.fget_Vertex(
    const _i : Integer
  ) : TPointF ;
  begin
    Result := T_strokeLowLevel( oStroke ).GetVertex( _i ) ;
  end ;

  function T_stroke.fget_Command(
    const _i : Integer
  ) : T_strokeCommand ;
  begin
    Result := T_strokeLowLevel( oStroke ).GetCommand( _i ) ;
  end ;

  function T_stroke.fget_Count : Integer ;
  begin
    Result := T_strokeLowLevel( oStroke ).GetCount ;
  end ;
{$ENDREGION}

{$REGION 'T_fill'}
//==============================================================================
// T_fill
//==============================================================================

  constructor T_fill.Create(
    const _path   : T_path
  ) ;
  begin
    inherited Create ;

    FPath := _path ;

    oTri := T_Triangulate.Create ;
    T_Triangulate( oTri ).Reset( _path.Count ) ;

    readPath( _path ) ;
    T_Triangulate( oTri ).Go ;
  end ;

  destructor T_fill.Destroy ;
  begin
    FreeObject( oTri ) ;

    inherited ;
  end;

  procedure T_fill.readPath(
    const _path : T_path
  ) ;
  var
    poly : TPolygon ;
    v    : TPointF ;
    len  : Integer ;
    xmin : Single ;
    ymin : Single ;
    xmax : Single ;
    ymax : Single ;
    i    : Integer ;
    j    : Integer ;
    n    : Integer ;

    procedure min_max ;
    begin
      xmin := Min( xmin, v.X ) ;
      ymin := Min( ymin, v.Y ) ;
      xmax := Max( xmax, v.X ) ;
      ymax := Max( ymax, v.Y ) ;
    end ;

    procedure add_rect ;
    begin
      Inc( len, 2 ) ;
      SetLength( oRect, len ) ;
      oRect[len-2].X1 := xmin ;
      oRect[len-2].Y1 := ymin ;
      oRect[len-2].X2 := xmax ;
      oRect[len-2].Y2 := ymin ;
      oRect[len-2].X3 := xmax ;
      oRect[len-2].Y3 := ymax ;
      oRect[len-1].X1 := xmin ;
      oRect[len-1].Y1 := ymin ;
      oRect[len-1].X2 := xmax ;
      oRect[len-1].Y2 := ymax ;
      oRect[len-1].X3 := xmin ;
      oRect[len-1].Y3 := ymax ;
    end ;

  begin
    len := 0 ;
    xmin :=  GIS_MAX_SINGLE ;
    ymin :=  GIS_MAX_SINGLE ;
    xmax := -GIS_MAX_SINGLE ;
    ymax := -GIS_MAX_SINGLE ;

    n := -1 ;
    i := 0 ;
    while i < _path.Count do begin

      if _path.Command[i] = T_pathCommand.JumpTo then
        Inc( n ) ;

      if _path.Command[i] = T_pathCommand.CurveTo then begin
        i := _path.ExpandCurve( i, poly ) ;
        for j := 0 to High( poly ) do begin
          v := poly[j] ;
          v.Y := -v.Y ;
          T_Triangulate( oTri ).AddVertex( n, v.X, v.Y ) ;
          min_max ;
        end ;
      end
      else begin
        v := _path.Vertex[i] ;
        v.Y := -v.Y ;
        T_Triangulate( oTri ).AddVertex( n, v.X, v.Y ) ;
        min_max ;
      end ;

      Inc( i ) ;
    end ;

    add_rect ;
  end ;

  function T_fill.fget_Triangle(
    const _i : Integer
  ) : T_triangle ;
  begin
    Result := T_Triangulate( oTri ).GetTriangle( _i ) ;
  end ;

  function T_fill.fget_Count : Integer ;
  begin
    Result := T_Triangulate( oTri ).GetCount ;
  end ;

  function T_fill.fget_Triangle1(
    const _i : Integer
  ) : T_triangle ;
  begin
    Result := oRect[_i] ;
  end ;

  function T_fill.fget_Count1 : Integer ;
  begin
    Result := Length( oRect ) ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_CanvasHelperGpu'}
//==============================================================================
// TGIS_CanvasHelperGpu
//==============================================================================
var
  oDefaultIndexBuffer3 : TIndexBuffer = nil;
  oDefaultIndexBuffer4 : TIndexBuffer = nil;

  procedure TGIS_CanvasHelperGpu.gl2dStrokePath(
    const _path    : TObject ;
    const _opacity : Single
  ) ;
  var
    strk : T_stroke ;
    join : T_strokeJoin ;
    cap  : T_strokeCap  ;
    wdth : Single ;
    arr  : array of Single ;
    off  : Single ;

    procedure select_join ;
    begin
      case Stroke.Join of
        TStrokeJoin.Miter : join := T_strokeJoin.Miter ;
        TStrokeJoin.Round : join := T_strokeJoin.Round ;
        TStrokeJoin.Bevel : join := T_strokeJoin.Bevel ;
      end ;
    end ;

    procedure select_cap ;
    begin
      case Stroke.Cap of
        TStrokeCap.Flat  : cap := T_strokeCap.Square ;
        TStrokeCap.Round : cap := T_strokeCap.Round  ;
      end ;
    end ;

    procedure select_style ;
    begin
      case Stroke.Dash of
        TStrokeDash.Solid      :
          begin
            SetLength( arr, 1 ) ;
            arr[0] := 1.0 ;
          end ;
        TStrokeDash.Dash       :
          begin
            SetLength( arr, 2 ) ;
            arr[0] := 3.0 ;
            arr[1] := 1.0 ;
          end ;
        TStrokeDash.Dot        :
          begin
            SetLength( arr, 2 ) ;
            arr[0] := 1.0 ;
            arr[1] := 1.0 ;
          end ;
        TStrokeDash.DashDot    :
          begin
            SetLength( arr, 4 ) ;
            arr[0] := 3.0 ;
            arr[1] := 1.0 ;
            arr[2] := 1.0 ;
            arr[3] := 1.0 ;
          end ;
        TStrokeDash.DashDotDot :
          begin
            SetLength( arr, 6 ) ;
            arr[0] := 3.0 ;
            arr[1] := 1.0 ;
            arr[2] := 1.0 ;
            arr[3] := 1.0 ;
            arr[4] := 1.0 ;
            arr[5] := 1.0 ;
          end ;
        TStrokeDash.Custom     :
          begin
            SetLength( arr, Length( Stroke.DashArray ) ) ;
            Move( Stroke.DashArray[0], arr[0], 4*Length( arr ) ) ;
          end ;
      end;
    end ;

  begin
    select_join ;
    select_cap ;
    wdth := Stroke.Thickness ;
    select_style ;
    off := Stroke.DashOffset ;

    strk :=
      T_stroke.Create(
        T_path( _path ), join, cap, wdth, arr, off,
        Stroke.Join <> TStrokeJoin.Round
      ) ;
    try
      gl2dDrawStroke( strk, _opacity ) ;
    finally
      FreeObject( strk ) ;
    end ;
  end ;

  procedure TGIS_CanvasHelperGpu.gl2dDrawStroke(
    const _stroke  : TObject ;
    const _opacity : Single
  ) ;
  var
    vbuf : TVertexBuffer ;
    ibuf : TIndexBuffer ;
    fan  : Boolean ;
    v    : TPointF ;
    i    : Integer ;
    j    : Integer ;
    k    : Integer ;
    kk   : Integer ;
    rev  : Boolean ;
  begin
    if T_stroke( _stroke ).Count < 1 then
      exit ;
    vbuf := TVertexBuffer.Create(
      [TVertexFormat.Vertex], T_stroke( _stroke ).Count
    ) ;
    j := 0 ;
    k := 0 ;

    if ( T_stroke( _stroke ).Width <= 1.0 ) and
       ( Stroke.Join = TStrokeJoin.Round  ) then begin

      ibuf := TIndexBuffer.Create( 2 * vbuf.Length ) ;

      for i := 0 to T_stroke( _stroke ).Count - 1 do begin

        v := T_stroke( _stroke ).Vertex[i] ;

        vbuf.Vertices[i] := TPoint3D.Create( v.X, v.Y, 0.0 ) ;

        if T_stroke( _stroke ).Command[i] = T_strokeCommand.MoveTo then begin
          k := i ;
          continue ;
        end ;

        ibuf.Indices[2*j  ] := k   ;
        ibuf.Indices[2*j+1] := k+1 ;

        Inc( j ) ;
        Inc( k ) ;
      end ;

      ibuf.Length := 2*j ;

      if Stroke.Kind = TBrushKind.Bitmap then begin
        drawTexture(
          vbuf,
          ibuf,
          False,
          Stroke.Bitmap.Bitmap,
          T_stroke( _stroke ).Path.GetBoundRect,
          _opacity
        ) ;
      end
      else begin
        drawColor(
          vbuf,
          ibuf,
          False,
          Stroke.Color,
          False,
          _opacity
        ) ;
      end ;

    end
    else begin

      fan := False ;

      ibuf := TIndexBuffer.Create( 3 * vbuf.Length ) ;

      kk := 0 ;
      rev := False ;

      for i := 0 to T_stroke( _stroke ).Count - 1 do begin

        v := T_stroke( _stroke ).Vertex[i] ;

        vbuf.Vertices[i] := TPoint3D.Create( v.X, v.Y, 0.0 ) ;

        if T_stroke( _stroke ).Command[i  ] = T_strokeCommand.MoveTo then
          continue
        else
        if T_stroke( _stroke ).Command[i-1] = T_strokeCommand.MoveTo then
        begin
          fan := False ;
          rev := False ;
          k   := i + 1;
          continue ;
        end
        else
        if T_stroke( _stroke ).Command[i  ] = T_strokeCommand.MoveToFan then
          continue
        else
        if T_stroke( _stroke ).Command[i-1] = T_strokeCommand.MoveToFan then
        begin
          fan := True ;
          rev := False ;
          k   := i + 1 ;
          kk  := k - 2 ;
          continue ;
        end ;

        case fan of
          True  :
            begin
              ibuf.Indices[3*j  ] := kk  ;
              ibuf.Indices[3*j+1] := k   ;
              ibuf.Indices[3*j+2] := k-1 ;
            end ;
          False :
            begin
              if rev then begin
                ibuf.Indices[3*j  ] := k   ;
                ibuf.Indices[3*j+1] := k-1 ;
                ibuf.Indices[3*j+2] := k-2 ;
              end
              else begin
                ibuf.Indices[3*j  ] := k   ;
                ibuf.Indices[3*j+1] := k-1 ;
                ibuf.Indices[3*j+2] := k-2 ;
              end ;
            end ;
        end ;

        Inc( j ) ;
        Inc( k ) ;
        rev := not rev ;
      end ;

      ibuf.Length := 3*j ;

      if Stroke.Kind = TBrushKind.Bitmap then begin
        drawTexture(
          vbuf,
          ibuf,
          True,
          Stroke.Bitmap.Bitmap,
          T_stroke( _stroke ).Path.GetBoundRect,
          _opacity
        ) ;
      end
      else begin
        drawColor(
          vbuf,
          ibuf,
          True,
          Stroke.Color,
          False,
          _opacity
        ) ;
      end ;

    end ;

    FreeObject( ibuf ) ;
    FreeObject( vbuf ) ;
  end ;

  procedure TGIS_CanvasHelperGpu.gl2dFillPath(
    const _path    : TObject ;
    const _opacity : Single
  ) ;
  var
    fill : T_fill ;
  begin
    fill := T_fill.Create( T_path( _path ) ) ;
    try
      gl2dDrawFill( fill, _opacity ) ;
    finally
      FreeObject( fill ) ;
    end;
  end ;

  procedure TGIS_CanvasHelperGpu.gl2dDrawFill(
    const _fill    : TObject ;
    const _opacity : Single
  ) ;
  var
    vbuf : TVertexBuffer ;
    ibuf : TIndexBuffer ;
    vbuf1 : TVertexBuffer ;
    ibuf1 : TIndexBuffer ;
    t    : T_triangle ;
    i    : Integer ;
  begin
    vbuf := TVertexBuffer.Create(
      [TVertexFormat.Vertex], 3 * T_fill( _fill ).Count
    ) ;
    ibuf := TIndexBuffer.Create( vbuf.Length ) ;

    for i := 0 to T_fill( _fill ).Count - 1 do begin
      t := T_fill( _fill ).Triangle[i] ;

      vbuf.Vertices[3*i  ] := TPoint3D.Create( t.X1, -t.Y1, 0.0 ) ;
      vbuf.Vertices[3*i+1] := TPoint3D.Create( t.X2, -t.Y2, 0.0 ) ;
      vbuf.Vertices[3*i+2] := TPoint3D.Create( t.X3, -t.Y3, 0.0 ) ;

      ibuf.Indices[3*i  ] := 3*i   ;
      ibuf.Indices[3*i+1] := 3*i+1 ;
      ibuf.Indices[3*i+2] := 3*i+2 ;
    end ;

    vbuf1 := TVertexBuffer.Create(
      [TVertexFormat.Vertex], 3 * T_fill( _fill ).Count1
    ) ;
    ibuf1 := TIndexBuffer.Create( vbuf1.Length ) ;

    for i := 0 to T_fill( _fill ).Count1 - 1 do begin
      t := T_fill( _fill ).Triangle1[i] ;

      vbuf1.Vertices[3*i  ] := TPoint3D.Create( t.X1, -t.Y1, 0.0 ) ;
      vbuf1.Vertices[3*i+1] := TPoint3D.Create( t.X2, -t.Y2, 0.0 ) ;
      vbuf1.Vertices[3*i+2] := TPoint3D.Create( t.X3, -t.Y3, 0.0 ) ;

      ibuf1.Indices[3*i  ] := 3*i   ;
      ibuf1.Indices[3*i+1] := 3*i+1 ;
      ibuf1.Indices[3*i+2] := 3*i+2 ;
    end ;

    if Fill.Kind = TBrushKind.Bitmap then begin
      drawTexture(
        vbuf,
        ibuf,
        True,
        Fill.Bitmap.Bitmap,
        T_fill( _fill ).Path.GetBoundRect,
        _opacity
      ) ;
    end
    else begin
      drawColor1(
        vbuf,
        ibuf,
        vbuf1,
        ibuf1,
        True,
        Fill.Color,
        True,
        _opacity
      ) ;
    end ;

    FreeObject( ibuf ) ;
    FreeObject( vbuf ) ;
  end ;

  function TGIS_CanvasHelperGpu.createMaterial(
    const _color : TAlphaColor
  ) : TColorMaterial ;
  var
    mtr : TColorMaterial ;
  begin
    mtr := TColorMaterial.Create ;
    mtr.Color := _color ;

    Result := mtr ;
  end ;

  function TGIS_CanvasHelperGpu.createMaterial(
    const _bitmap : TBitmap
  ) : TTextureMaterial ;
  var
    [unsafe] ctx : TContext3D ;
    mtr : TTextureMaterial ;
  begin
    Result := nil ;
    ctx := TCustomCanvasGpu( Self ).Context ;
    mtr := TTextureMaterial.Create ;
    mtr.Texture := ctx.BitmapToTexture( _bitmap ) ;
    Result := mtr ;
  end ;

  function TGIS_CanvasHelperGpu.auxFillSimple(
    const _path    : TPathData ;
    const _opacity : Single
  ) : Boolean ;
  var
    path : TPathData ;
    bmtx : Boolean ;
    vbuf : TVertexBuffer ;
    ibuf : TIndexBuffer ;
    i    : Integer ;
    c    : Integer ;
    cnt  : Integer ;
  begin
    Result := False ;

    if ( _path.Count > 6 ) then
      exit ;

    for i := 1 to _path.Count - 1 do begin
      if _path.Points[i].Kind = TPathPointKind.MoveTo then begin
        Result := False ;
        exit ;
      end ;
    end ;

    c := 0 ;
    if _path.Points[_path.Count-1].Point =
       _path.Points[_path.Count-2].Point then
      Inc( c ) ;
    if _path.Points[_path.Count-1].Point =
       _path.Points[0].Point then
      Inc( c ) ;
    cnt := _path.Count - c ;

    if cnt < 3 then begin
      Result := True ;
      exit ;
    end
    else
    if cnt > 4 then begin
      Result := False ;
      exit ;
    end ;

    Result := True ;

    vbuf := TVertexBuffer.Create( [TVertexFormat.Vertex], _path.Count - c ) ;

    bmtx := not MatrixEqual( Matrix, TMatrix.Identity ) ;
    if bmtx then begin
      path := TPathData.Create ;
      path.AddPath( _path ) ;
      path.ApplyMatrix( Matrix ) ;
    end
    else
      path := _path ;

    for i := 0 to cnt - 1 do
      vbuf.Vertices[i] :=
        TPoint3D.Create(
          path.Points[i].Point.X, path.Points[i].Point.Y, 0.0
        ) ;

    case cnt of
      3 : ibuf := oDefaultIndexBuffer3 ;
      4 : ibuf := oDefaultIndexBuffer4 ;
    end ;

    if Fill.Kind = TBrushKind.Bitmap then begin
      drawTexture(
        vbuf,
        ibuf,
        True,
        Fill.Bitmap.Bitmap,
        path.GetBounds,
        _opacity
      ) ;
    end
    else begin
      drawColor(
        vbuf,
        ibuf,
        True,
        Fill.Color,
        True,
        _opacity
      ) ;
    end ;

    if bmtx then
      FreeObject( path ) ;

    FreeObject( vbuf ) ;
  end ;

  procedure TGIS_CanvasHelperGpu.drawTexture(
    const _vbuf    : TVertexBuffer ;
    const _ibuf    : TIndexBuffer ;
    const _tris    : Boolean ;
    const _bitmap  : TBitmap ;
    const _rect    : TRectF ;
    const _opacity : Single
  ) ;
  var
    [unsafe] ctx : TContext3D ;
    mtr   : TMaterial ;
    svbuf : TVertexBuffer ;
    w     : Integer ;
    h     : Integer ;
    wmax  : Integer ;
    hmax  : Integer ;
    bmpw  : Integer ;
    bmph  : Integer ;
  begin
    ctx := TCustomCanvasGpu( Self ).Context ;

    ctx.SetContextState( TContextState.csStencilOn ) ;
    ctx.Clear( [TClearTarget.Stencil], 0, 0, 0 ) ;
    ctx.SetContextState( TContextState.csColorWriteOff ) ;
    ctx.SetContextState( TContextState.csZWriteOff ) ;
    ctx.SetStencilFunc( TStencilFunc.Always, 0, $FF ) ;
    ctx.SetStencilOp( TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Invert ) ;

    mtr := createMaterial( $FF ) ;
    if _tris then
      ctx.DrawTriangles( _vbuf, _ibuf, mtr, 1.0 )
    else
      ctx.DrawLines( _vbuf, _ibuf , mtr, 1.0 ) ;
    FreeObject( mtr ) ;

    ctx.SetContextState( TContextState.csZWriteOn ) ;
    ctx.SetContextState( TContextState.csColorWriteOn ) ;
    ctx.SetStencilFunc( TStencilFunc.NotEqual, 0, $FF ) ;
    ctx.SetStencilOp( TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Keep ) ;

    hmax := RoundS( _rect.Height/_bitmap.Height ) + 1 ;
    wmax := RoundS( _rect.Width/_bitmap.Width  ) + 1 ;

    bmpw := _bitmap.Width ;
    bmph := _bitmap.Height ;

    svbuf := TVertexBuffer.Create(
      [TVertexFormat.Vertex, TVertexFormat.Normal, TVertexFormat.TexCoord0],
      4
    ) ;
    svbuf.Normals[0] := TPoint3D.Create( 0, 0, -1 ) ;
    svbuf.TexCoord0[0] := TPointF.Create( 0, 0 ) ;
    svbuf.Normals[1] := TPoint3D.Create( 0, 0, -1 ) ;
    svbuf.TexCoord0[1] := TPointF.Create( 1, 0 ) ;
    svbuf.Normals[2] := TPoint3D.Create( 0, 0, -1 ) ;
    svbuf.TexCoord0[2] := TPointF.Create( 1, 1 ) ;
    svbuf.Normals[3] := TPoint3D.Create( 0, 0, -1 ) ;
    svbuf.TexCoord0[3] := TPointF.Create( 0, 1 ) ;

    mtr := createMaterial( _bitmap ) ;
    for h := 0 to hmax do begin
      for w := 0 to wmax do begin
        svbuf.Vertices[0] := TPoint3D.Create(
          _rect.Left+w*bmpw, _rect.Top+h*bmph, 0 ) ;
        svbuf.Vertices[1] := TPoint3D.Create(
          _rect.Left+(w+1)*bmpw, _rect.Top+h*bmph, 0 ) ;
        svbuf.Vertices[2] := TPoint3D.Create(
          _rect.Left+(w+1)*bmpw, _rect.Top+(h+1)*bmph, 0 ) ;
        svbuf.Vertices[3] := TPoint3D.Create(
          _rect.Left+w*bmpw, _rect.Top+(h+1)*bmph, 0 ) ;
        ctx.DrawTriangles( svbuf, oDefaultIndexBuffer4, mtr, _opacity ) ;
      end ;
    end ;
    FreeObject( mtr ) ;
    FreeObject( svbuf ) ;

    ctx.SetContextState( TContextState.csStencilOff ) ;
  end ;

  procedure TGIS_CanvasHelperGpu.drawColor(
    const _vbuf    : TVertexBuffer ;
    const _ibuf    : TIndexBuffer ;
    const _tris    : Boolean ;
    const _color   : TAlphaColor ;
    const _stencil : Boolean ;
    const _opacity : Single
  ) ;
  var
    [unsafe] ctx : TContext3D ;
    mtr : TMaterial ;
  begin
    if _vbuf.Length = 0 then
      exit ;

    ctx := TCustomCanvasGpu( Self ).Context ;

    if not _stencil then begin
      mtr := createMaterial( _color ) ;
      if _tris then
        ctx.DrawTriangles( _vbuf, _ibuf, mtr, _opacity )
      else
        ctx.DrawLines( _vbuf, _ibuf , mtr, _opacity ) ;
      FreeObject( mtr ) ;
      exit ;
    end ;

    ctx.SetContextState( TContextState.csStencilOn ) ;
    ctx.Clear( [TClearTarget.Stencil], 0, 0, 0 ) ;
    ctx.SetContextState( TContextState.csColorWriteOff ) ;
    ctx.SetContextState( TContextState.csZWriteOff ) ;
    ctx.SetStencilFunc( TStencilFunc.Always, 0, $FF ) ;
    ctx.SetStencilOp( TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Invert ) ;

    mtr := createMaterial( $FF ) ;
    if _tris then
      ctx.DrawTriangles( _vbuf, _ibuf, mtr, 1.0 )
    else
      ctx.DrawLines( _vbuf, _ibuf , mtr, 1.0 ) ;
    FreeObject( mtr ) ;

    ctx.SetContextState( TContextState.csZWriteOn ) ;
    ctx.SetContextState( TContextState.csColorWriteOn ) ;
    ctx.SetStencilFunc( TStencilFunc.NotEqual, 0, $FF ) ;
    ctx.SetStencilOp( TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Keep ) ;

    mtr := createMaterial( _color ) ;
    if _tris then
      ctx.DrawTriangles( _vbuf, _ibuf, mtr, _opacity )
    else
      ctx.DrawLines( _vbuf, _ibuf , mtr, _opacity ) ;
    FreeObject( mtr ) ;

    ctx.SetContextState( TContextState.csStencilOff ) ;
  end ;

  procedure TGIS_CanvasHelperGpu.drawColor1(
    const _vbuf    : TVertexBuffer ;
    const _ibuf    : TIndexBuffer ;
    const _vbuf1   : TVertexBuffer ;
    const _ibuf1   : TIndexBuffer ;
    const _tris    : Boolean ;
    const _color   : TAlphaColor ;
    const _stencil : Boolean ;
    const _opacity : Single
  ) ;
  var
    [unsafe] ctx : TContext3D ;
    mtr : TMaterial ;
  begin
    ctx := TCustomCanvasGpu( Self ).Context ;

    if not _stencil then begin
      mtr := createMaterial( _color ) ;
      if _tris then
        ctx.DrawTriangles( _vbuf, _ibuf, mtr, _opacity )
      else
        ctx.DrawLines( _vbuf, _ibuf , mtr, _opacity ) ;
      FreeObject( mtr ) ;
      exit ;
    end ;

    ctx.SetContextState( TContextState.csStencilOn ) ;
    ctx.Clear( [TClearTarget.Stencil], 0, 0, 0 ) ;
    ctx.SetContextState( TContextState.csColorWriteOff ) ;
    ctx.SetContextState( TContextState.csZWriteOff ) ;
    ctx.SetStencilFunc( TStencilFunc.Always, 0, $FF ) ;
    ctx.SetStencilOp( TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Invert ) ;

    mtr := createMaterial( $FF ) ;
    if _tris then
      ctx.DrawTriangles( _vbuf, _ibuf, mtr, 1.0 )
    else
      ctx.DrawLines( _vbuf, _ibuf, mtr, 1.0 ) ;
    FreeObject( mtr ) ;

    ctx.SetContextState( TContextState.csZWriteOn ) ;
    ctx.SetContextState( TContextState.csColorWriteOn ) ;
    ctx.SetStencilFunc( TStencilFunc.NotEqual, 0, $FF ) ;
    ctx.SetStencilOp( TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Keep ) ;

    mtr := createMaterial( _color ) ;
    if _tris then
      ctx.DrawTriangles( _vbuf1, _ibuf1, mtr, _opacity )
    else
      ctx.DrawLines( _vbuf, _ibuf, mtr, _opacity ) ;
    FreeObject( mtr ) ;

    ctx.SetContextState( TContextState.csStencilOff ) ;
  end ;

  procedure TGIS_CanvasHelperGpu.FillPath(
    const _path    : TPathData ;
    const _opacity : Single
  ) ;
  var
    path : T_path ;
  begin
    if not ( Self is TCustomCanvasGpu ) then begin
      inherited FillPath( _path, _opacity );
      exit ;
    end ;

    if Fill.Kind = TBrushKind.None then
      exit ;

    if _path.Count < 3 then
      exit
    else
    if auxFillSimple( _path, _opacity ) then
      exit ;

    path := T_path.Create( _path ) ;
    path.ApplyMatrix( Matrix ) ;
    try
      gl2dFillPath( path, _opacity ) ;
    finally
      FreeObject( path ) ;
    end ;
  end ;

  procedure TGIS_CanvasHelperGpu.DrawLine(
    const _start   : TPointF ;
    const _end     : TPointF ;
    const _opacity : Single
  ) ;
  var
    path : T_path ;
  begin
    if not ( Self is TCustomCanvasGpu ) then begin
      inherited DrawLine( _start, _end, _opacity );
      exit ;
    end ;

    if Stroke.Kind = TBrushKind.None then
      exit ;

    path := T_path.Create ;
    try
      path.AddVertex( _start * Matrix ) ;
      path.AddVertex( _end * Matrix   ) ;

      gl2dStrokePath( path, _opacity ) ;
    finally
      FreeObject( path ) ;
    end ;
  end ;

  procedure TGIS_CanvasHelperGpu.DrawPath(
    const _path    : TPathData ;
    const _opacity : Single
  ) ;
  var
    i : Integer   ;
    p : TPathData ;
  var
    path : T_path ;
  begin
    if not ( Self is TCustomCanvasGpu ) then begin
      inherited DrawPath( _path, _opacity );
      exit ;
    end ;

    if Stroke.Kind = TBrushKind.None then
      exit ;

    path := T_path.Create( _path ) ;
    path.ApplyMatrix( Matrix ) ;
    try
      gl2dStrokePath( path, _opacity ) ;
    finally
      FreeObject( path ) ;
    end ;
  end ;
{$ENDREGION}


//==============================================================================
// initialization/finalization
//==============================================================================


initialization
  oDefaultIndexBuffer3 := TIndexBuffer.Create( 3 ) ;
  oDefaultIndexBuffer3.Indices[0] := 0 ;
  oDefaultIndexBuffer3.Indices[1] := 1 ;
  oDefaultIndexBuffer3.Indices[2] := 2 ;

  oDefaultIndexBuffer4 := TIndexBuffer.Create( 6 ) ;
  oDefaultIndexBuffer4.Indices[0] := 0 ;
  oDefaultIndexBuffer4.Indices[1] := 1 ;
  oDefaultIndexBuffer4.Indices[2] := 2 ;
  oDefaultIndexBuffer4.Indices[3] := 2 ;
  oDefaultIndexBuffer4.Indices[4] := 3 ;
  oDefaultIndexBuffer4.Indices[5] := 0 ;
finalization
  FreeObject( oDefaultIndexBuffer3 ) ;
  FreeObject( oDefaultIndexBuffer4 ) ;

//==================================== END =====================================
end.

