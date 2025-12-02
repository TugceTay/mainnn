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
  Encapsulation of AutoCAD DWG layer.
}

{$IFDEF DCC}
  unit GisLayerDWG ;
  {$HPPEMIT '#pragma link "GisLayerDWG"'}
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

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    GisLayerVector ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerDWG = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  //----------------------------------------------------------------------------
  // general GIS types and declarations
  //----------------------------------------------------------------------------
    {$IFDEF OXYGENE}
      /// <summary>
      ///   Provides data for the DWG extended entity definition event
      ///  ( code: 1001).
      /// </summary>
      TGIS_LayerDWGExtendedDataEventArgs = public class ( EventArgs )
        // properties internal value
        {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}
          FAppName : String          ;
          FShape   : TGIS_Shape      ;
          FData    : TGIS_StringList ;
        public
          /// <summary>
          ///   Create an event object.
          /// </summary>
          /// <param name="_appname">
          ///   application name
          /// </param>
          /// <param name="_shape">
          ///   shape to which event is related
          /// </param>
          /// <param name="_data">
          ///   strings with extended data
          /// </param>
          constructor Create  ( const _appname : String          ;
                                const _shape   : TGIS_Shape      ;
                                const _data    : TGIS_StringList
                              ) ;
        public
          /// <summary>
          ///   Application name (1001 entity creator).
          /// </summary>
          property AppName     : String
                                read FAppName ;
          /// <summary>
          ///   Shape object.
          /// </summary>
          property Shape      : TGIS_Shape
                                read FShape ;
          /// <summary>
          ///   Strings defining extended data.
          /// </summary>
          property Data       : TGIS_StringList
                                read FData ;
      end ;

      /// <summary>
      ///   Delegate for ExtendedDataEvent (code: 1001).
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_e">
      ///   event object
      /// </param>
      TGIS_LayerDWGExtendedDataEvent = public procedure(
        _sender : Object ;
        _e      : TGIS_LayerDWGExtendedDataEventArgs
      ) of object ;
    {$ELSE}
      /// <summary>
      ///   ExtendedDataEvent (code: 1001).
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_appname">
      ///   application name (1001 entity creator)
      /// </param>
      /// <param name="_shape">
      ///   shape to which event is related
      /// </param>
      /// <param name="_data">
      ///   strings with extended data
      /// </param>
      {$IFDEF GENXDK}
        TGIS_LayerDWGExtendedDataEvent = procedure(
          var _translated : Boolean     ;
              _sender     : TObject     ;
              _appname    : String      ;
              _shape      : TGIS_Shape  ;
              _data       : TStringList
        ) of object ;
      {$ELSE}
        TGIS_LayerDWGExtendedDataEvent = procedure(
          _sender  : TObject     ;
          _appname : String      ;
          _shape   : TGIS_Shape  ;
          _data    : TStringList
        ) of object ;
      {$ENDIF}
    {$ENDIF}

  /// <summary>
  ///   Layer which can read AutoCAD DWG file.
  /// </summary>
  TGIS_LayerDWG = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      dwgFile : TObject ;
    private // events
      /// <summary>
      ///   Extended data event (code: 1001)
      /// </summary>
      FExtendedDataEvent : TGIS_LayerDWGExtendedDataEvent ;
    private
      procedure buildShapes ;
      {$IFDEF JAVA}
        procedure add_ExtendedDataEvent   ( _e : TGIS_LayerDWGExtendedDataEvent
                                          ) ;
        procedure remove_ExtendedDataEvent( _e : TGIS_LayerDWGExtendedDataEvent
                                          ) ;
      {$ENDIF}
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <inheritdoc/>
      procedure setUp     ; override;
    protected
      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      /// <inheritdoc/>
      constructor Create ; override;

    published // events
       {$IFDEF CLR}
         /// <event/>
         /// <summary>
         ///   Extended entity definition data event (DXF code 1001).
         /// </summary>
         /// <remarks>
         ///   <para>
         ///     Event will context in extended entity definition data as a
         ///    list of strings ready for further parsing.
         ///   </para>
         ///   <para>
         ///     If not attached then ADE extended entity definition (if exists)
         ///     will be parsed internally.
         ///   </para>
         /// </remarks>
         event ExtendedDataEvent    : TGIS_LayerDWGExtendedDataEvent
                                      delegate FExtendedDataEvent ;
       {$ENDIF}
       {$IFDEF JAVA}
         /// <event/>
         /// <summary>
         ///   Extended entity definition data event (DXF code 1001).
         /// </summary>
         /// <remarks>
         ///   <para>
         ///     Event will context in extended entity definition data as a
         ///    list of strings ready for further parsing.
         ///   </para>
         ///   <para>
         ///     If not attached then ADE extended entity definition (if exists)
         ///     will be parsed internally.
         ///   </para>
         /// </remarks>
         event ExtendedDataEvent    : TGIS_LayerDWGExtendedDataEvent
                                      add    add_ExtendedDataEvent
                                      remove remove_ExtendedDataEvent ;
       {$ENDIF}
       {$IFDEF DCC}
         /// <event/>
         /// <summary>
         ///   Extended entity definition data event (DXF code 1001).
         /// </summary>
         /// <remarks>
         ///   <para>
         ///     Event will context in extended entity definition data as a
         ///    list of strings ready for further parsing.
         ///   </para>
         ///   <para>
         ///     If not attached then ADE extended entity definition (if exists)
         ///     will be parsed internally.
         ///   </para>
         /// </remarks>
         property ExtendedDataEvent : TGIS_LayerDWGExtendedDataEvent
                                      read  FExtendedDataEvent
                                      write FExtendedDataEvent ;
       {$ENDIF}

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
uses
  System.Math,
  System.Generics.Defaults,
  Generics.Collections,

  GisInterfaces,
  GisLayer,
  GisRtl,
  GisRegistredLayers,
  GisInternals,
  GisResource,
  GisParams,
  GisTypes,
  GisTypesUI,
  GisSymbol,
  GisFunctions,
  GisClasses,
  GisLayerSublayer,
  GisFileDWG ;
{$ENDIF}

const
  GIS_DWG_HANDLE  = 'DWG_HANDLE' ;
  GIS_DWG_LABEL   = 'DWG_LABEL'  ;
  GIS_DWG_LAYER   = 'DWG_LAYER'  ;

  GIS_DWG_DIMSTYLE = '&D(0 100S)D(0 -200S)D(0 100S)I(0 0 80S 20S 0 -40S -80S 20S)'+
                     'M(80S 0)L(40S)L(-120S)L(40S)I(0 20S 80S -20S -80S -20S 0 40S)'+
                     'G(100%)D(0 80S)D(0 -160S)D(0 80S)' ;
  GIS_DWG_LEADERSTYLE = '&L(-120S)L(40S)I(0 40S 80S -40S -80S -40S 0 80S)' ;
type

  {$IFDEF JAVA}
    TDWG_Matrix       = array[0..3] of array[0..3] of Double ;
  {$ELSE}
    TDWG_Matrix       = array[0..3,0..3] of Double ; 
  {$ENDIF}

  TDWG_Transformation = class
    private
      arTransform         : TDWG_Matrix ;
      arTransformInsert   : TDWG_Matrix ;
      inInsert            : Integer ;
    private
      function mtrx_fill            : TDWG_Matrix ;
      function mtrx_normalize       ( const _pt           : TGIS_Point3D
                                     ) : TGIS_Point3D ;
      function mtrx_cross           ( const _pt1, _pt2    : TGIS_Point3D
                                     ) : TGIS_Point3D ;
      function mtrx_rotate          ( const _dcos, _dsin  : Double
                                     ) : TDWG_Matrix ;
      function mtrx_multiply        ( const _mtrx1,_mtrx2 : TDWG_Matrix
                                     ) : TDWG_Matrix ;
      function mtrx_scale           ( const _scale        : TGIS_Point3D
                                     ) : TDWG_Matrix ;
      function mtrx_translate       ( const _translate    : TGIS_Point3D
                                     ) : TDWG_Matrix ;
      function mtrx_inv_translate   ( const _translate    : TGIS_Point3D
                                     ) : TDWG_Matrix ;
      function mtrx_transform       ( const _vx, _vy, _vz : TGIS_Point3D
                                     ) : TDWG_Matrix ;
    public
      constructor Create ;
      procedure PrepareMatrix       ( const _extrusion    : TGIS_Point3D
                                     ) ;
      procedure PrepareMatrixInsert ( const _extrusion    : TGIS_Point3D ;
                                      const _scale        : TGIS_Point3D ;
                                      const _rotation     : Double  ;
                                      const _insert       : TGIS_Point3D
                                     ) ;
      procedure TransformInsert     ( const _basePtg      : TGIS_Point3D ;
                                      const _oldTransform : TDWG_Matrix
                                     ) ;
      function  TransformPtg2D      ( const _ptg          : TGIS_Point
                                     ) : TGIS_Point3D ;
      function  TransformPtg3D      ( const _ptg          : TGIS_Point3D
                                     ) : TGIS_Point3D ;
      function  TransformPtg4D      ( const _ptg          : TGIS_Point3D
                                     ) : TGIS_Point3D ;
      function  GetTransformInsert  : TDWG_Matrix ;
      procedure SetTransformInsert  ( const _old          : TDWG_Matrix
                                     ) ;
  end ;

  // B-spline interpolation of control points using de Boor's algorithm.
  TDWG_BSplineInterpolator = class
    private
      degree  : Integer ;
      pts     : TList<TGIS_Point3D> ;
      weights : array of Double ;
      knots   : array of Double ;
      domain  : array of Integer ;
    public
      function Prepare(
        const _degree  : Integer;
        const _points  : TList<TGIS_Point3D>;
        const _knots   : TList<Double> ;
        const _weights : TList<Double>
      ) : Boolean ;

      function InterpolatePoint(
        const _t : Double
      ) : TGIS_Point3D ;
  end ;

  {$IFDEF JAVA}
  T_listSortByNameDwg = class( java.util.Comparator<TGIS_LayerAbstract> )
    public
      function    compare ( _item1    : TGIS_LayerAbstract ;
                            _item2    : TGIS_LayerAbstract
                          ) : Integer ;
  end ;

  function T_listSortByNameDwg.compare(
    _item1 : TGIS_LayerAbstract ;
    _item2 : TGIS_LayerAbstract
  ) : Integer ;
  begin
    Result := dwg_sort_by_name( _item1, _item2 ) ;
  end ;
  {$ENDIF}

//=============================================================================
// TDWG_BSplineInterpolator
//=============================================================================

  function TDWG_BSplineInterpolator.Prepare(
    const _degree  : Integer;
    const _points  : TList<TGIS_Point3D>;
    const _knots   : TList<Double> ;
    const _weights : TList<Double>
  ) : Boolean ;
  var
    n, i, j : Integer ;
  begin
    Result := False ;

    n := _points.Count ;

    if (_degree < 1)     then exit ;
    if (_degree > (n-1)) then exit ;

    pts    := _points ;
    degree := _degree ;

    SetLength( weights, n ) ;
    if not assigned( _weights ) or (_weights.Count = 0) then begin
      for i := 0 to n-1 do
        weights[i] := 1.0 ;
    end
    else begin
      for i := 0 to n-1 do
        weights[i] := _weights[i] ;
    end ;

    if not assigned( _knots ) or (_knots.Count = 0) then begin
      // build knot vector of length [n + degree + 1]
      SetLength( knots, n+degree+1 ) ;
      for i := 0 to n+degree do
        knots[i] := i ;
    end
    else begin
      SetLength( knots, n+degree+1 ) ;
      if (_knots.Count <> (n+degree+1)) then begin
        j := 1 ;
        for i := degree+1 to (n+degree) do begin
          knots[i] := j ;
          if (i mod degree) = 0 then
            inc(j) ;
        end ;
      end
      else begin
        for i := 0 to _knots.Count-1 do
          knots[i] := _knots[i] ;
      end ;
    end ;

    SetLength( domain, 2 ) ;
    domain[0] := degree ;
    domain[1] := length(knots)-1 - degree ;

    Result := True ;
  end;

  function TDWG_BSplineInterpolator.InterpolatePoint(
    const _t : Double
  ) : TGIS_Point3D ;
  var
    i, s, l, n : Integer ;
    lo, hi, alpha, t : Double ;
    v : array of TGIS_Point3D ;
  begin
    // remap t to the domain where the spline is defined
    lo := knots[domain[0]] ;
    hi := knots[domain[1]] ;
    t := _t * (hi - lo) + lo ;

    assert(not ((t < lo) or (t > hi)), 'out of bounds') ;

    // find s (the spline segment) for the [t] value provided
    s := domain[0] ;
    while (s < domain[1] ) do begin
      if (t >= knots[s]) and (t <= knots[s+1]) then
        break;
      s := s + 1 ;
    end ;

    n := pts.Count ;
    // convert points to homogeneous coordinates
    SetLength( v, n ) ;
    for i := 0 to n-1 do begin
      {$IFDEF GIS_NORECORDS}
      v[i] := new TGIS_Point3D() ;
      {$ENDIF}
      v[i].X := pts[i].X * weights[i] ;
      v[i].Y := pts[i].Y * weights[i] ;
      v[i].Z := pts[i].Z * weights[i] ;
      v[i].M := weights[i] ;
    end ;

    // l (level) goes from 1 to the curve degree + 1
    for l := 1 to degree+1 do begin
      // build level l of the pyramid
      i := s ;
      while (i > s-degree-1+l ) do begin
        alpha := (t - knots[i]) / (knots[i+degree+1-l] - knots[i]);
        // interpolate each component
        v[i].X := (1 - alpha) * v[i-1].X + alpha * v[i].X;
        v[i].Y := (1 - alpha) * v[i-1].Y + alpha * v[i].Y;
        v[i].Z := (1 - alpha) * v[i-1].Z + alpha * v[i].Z;
        v[i].M := (1 - alpha) * v[i-1].M + alpha * v[i].M;
        i := i - 1 ;
      end ;
    end ;

    // convert back to cartesian and return
    {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Point3D() ;
    {$ENDIF}
    Result.X := v[s].X / v[s].M ;
    Result.Y := v[s].Y / v[s].M ;
    Result.Z := v[s].Z / v[s].M ;
    Result.M := 0 ;
  end;

//=============================================================================
// TDWG_Transformation
//=============================================================================

  // Constructor.
  constructor TDWG_Transformation.Create ;
  begin
    inherited ;

    arTransform       := mtrx_fill ;
    arTransformInsert := mtrx_fill ;
    inInsert          := 0 ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare null transformation coordinate.
  // return  prepared matrix
  function TDWG_Transformation.mtrx_fill : TDWG_Matrix ;
  var
    i, j : Integer ;
  begin
    for i:=0 to 3 do
      for j:=0 to 3 do
        if i=j then Result[i,j] := 1
               else Result[i,j] := 0 ;
  end ;

  // Supporting routines for matrix operations.
  // Normalize coordinate.
  // _pt     coordinate to be normalized
  // return  normalized coordinate
  function TDWG_Transformation.mtrx_normalize(
    const _pt : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    dvd : Double ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := TGIS_Point3D.Create ; 
    {$ENDIF}
    dvd := Sqrt( Sqr( _pt.X ) + Sqr( _pt.Y ) + Sqr( _pt.Z ) ) ;
    if dvd <> 0 then begin
      Result.X := _pt.X / dvd ;
      Result.Y := _pt.Y / dvd ;
      Result.Z := _pt.Z / dvd ;
    end
    else begin
      Result.X := _pt.X ;
      Result.Y := _pt.Y ;
      Result.Z := _pt.Z ;
    end ;
  end ;

  // Supporting routines for matrix operations.
  // Cross two coordinates.
  // _pt1    first coordinate to be crossed
  // _pt2    second coordinate to be crossed
  // return  crosseed coordinate
  function TDWG_Transformation.mtrx_cross(
    const _pt1, _pt2 : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := TGIS_Point3D.Create ; 
    {$ENDIF}
    Result.X := _pt1.Y * _pt2.Z - _pt1.Z * _pt2.Y ;
    Result.Y := _pt1.Z * _pt2.X - _pt1.X * _pt2.Z ;
    Result.Z := _pt1.X * _pt2.Y - _pt1.Y * _pt2.X ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare rotation matrix based on provided angle.
  // _dcos   cosinus of rotation angle
  // _dsin   sinus rotation angle
  // return  rotation matrix
  function TDWG_Transformation.mtrx_rotate(
    const _dcos, _dsin : Double
  ) : TDWG_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 0, 0 ] :=   _dcos ;
    Result[ 0, 1 ] := - _dsin ;
    Result[ 1, 0 ] :=   _dsin ;
    Result[ 1, 1 ] :=   _dcos ;
  end ;

  // Supporting routines for matrix operations.
  // Multiply matrixes.
  // _mtrx1  first matrix to be multiplied
  // _mtrx1  second matrix to be multiplied
  // return  resulting matrix
  function TDWG_Transformation.mtrx_multiply(
    const _mtrx1 , _mtrx2 : TDWG_Matrix
  ) : TDWG_Matrix ;
  var
    i,j : Integer ;
  begin
    for i := 0 to 3 do begin
      for j := 0 to 3 do
        Result[ i, j ] := _mtrx1[ i, 0 ] * _mtrx2[ 0, j ] +
                          _mtrx1[ i, 1 ] * _mtrx2[ 1, j ] +
                          _mtrx1[ i, 2 ] * _mtrx2[ 2, j ] +
                          _mtrx1[ i, 3 ] * _mtrx2[ 3, j ] ;
    end ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare scaling matrix based on _scale.
  // _scale  scale
  // return  scaled matrix
  function TDWG_Transformation.mtrx_scale(
    const _scale : TGIS_Point3D
  ) : TDWG_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 0, 0 ] := _scale.X ;
    Result[ 1, 1 ] := _scale.Y ;
    Result[ 2, 2 ] := _scale.Z ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare translation matrix based on _translate vector.
  // _translate  translation vector
  // return      translation matrix
  function TDWG_Transformation.mtrx_translate(
    const _translate : TGIS_Point3D
  ) : TDWG_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 3, 0 ] := _translate.X ;
    Result[ 3, 1 ] := _translate.Y ;
    Result[ 3, 2 ] := _translate.Z ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare inverse translation matrix based on _translate vector.
  // _translate  translation vector
  // return      translation matrix
  function TDWG_Transformation.mtrx_inv_translate(
    const _translate : TGIS_Point3D
  ) : TDWG_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 3, 0 ] := -_translate.X ;
    Result[ 3, 1 ] := -_translate.Y ;
    Result[ 3, 2 ] := -_translate.Z ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare transformation matrix based on provided vectors.
  // _vx     transformation vector
  // _vy     transformation vector
  // _vz     transformation vector
  // return  transformation matrix
  function TDWG_Transformation.mtrx_transform(
    const _vx, _vy, _vz : TGIS_Point3D
  ) : TDWG_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 0, 0 ] := _vx.X ;
    Result[ 1, 0 ] := _vy.X ;
    Result[ 2, 0 ] := _vz.X ;
    Result[ 0, 1 ] := _vx.Y ;
    Result[ 1, 1 ] := _vy.Y ;
    Result[ 2, 1 ] := _vz.Y ;
    Result[ 0, 2 ] := _vx.Z ;
    Result[ 1, 2 ] := _vy.Z ;
    Result[ 2, 2 ] := _vz.Z ;
  end ;

  // Prepare matrix for DXF extrusion.
  // To prepare matrix for insert see prepareMatrixInsert to
  // _extrusion  extrusion X,Y,Z
  procedure TDWG_Transformation.PrepareMatrix(
    const _extrusion : TGIS_Point3D
  ) ;
  var
    vx  : TGIS_Point3D ;
    vy  : TGIS_Point3D ;
    ext : TGIS_Point3D ;
  begin
    arTransform := mtrx_fill ;

    ext := _extrusion ;

    if not ((ext.X=0) and (ext.Y=0) and (ext.Z=1)) then begin
      if ( Abs( ext.X ) < 1.0/64 ) and ( Abs( ext.Y) < 1.0/64 ) then
        vx := mtrx_normalize( mtrx_cross( GisPoint3D( 0, 1, 0 ), ext) )
      else
        vx := mtrx_normalize( mtrx_cross( GisPoint3D( 0, 0, 1 ), ext) ) ;

      vy  := mtrx_normalize( mtrx_cross( ext, vx ) ) ;

      arTransform := mtrx_transform( vx, vy, ext ) ;
    end ;

    if inInsert > 0 then
      arTransform := mtrx_multiply( arTransform, arTransformInsert ) ;
  end ;

  // Prepare matrix for DXF extrusion form BLOK.INSERT feature.
  // To prepare matrix for standard features see prepareMatrix.
  // _extrusion  extrusion X,Y,Z
  // _scale      scale for X,Y,Z
  // _rotate     rotation in readians
  // _insert     insertation point
  procedure TDWG_Transformation.PrepareMatrixInsert(
    const _extrusion : TGIS_Point3D ;
    const _scale     : TGIS_Point3D ;
    const _rotation  : Double       ;
    const _insert    : TGIS_Point3D
  ) ;
  var
    vx : TGIS_Point3D ;
    vy : TGIS_Point3D ;
  begin
    arTransformInsert := mtrx_fill ;

    if ( Abs( _extrusion.X ) < 1.0/64 ) and ( Abs( _extrusion.Y) < 1.0/64 ) then
      vx := mtrx_normalize( mtrx_cross( GisPoint3D( 0, 1, 0 ), _extrusion) )
    else
      vx := mtrx_normalize( mtrx_cross( GisPoint3D( 0, 0, 1 ), _extrusion) ) ;

    vy  := mtrx_normalize( mtrx_cross( _extrusion, vx ) ) ;

    arTransformInsert := mtrx_multiply( arTransformInsert,
                           mtrx_scale( _scale )
                         ) ;
    arTransformInsert := mtrx_multiply( arTransformInsert,
                           mtrx_rotate( Cos( -_rotation ), Sin( -_rotation ) )
                         ) ;
    arTransformInsert := mtrx_multiply( arTransformInsert,
                           mtrx_translate( _insert )
                         ) ;
    arTransformInsert := mtrx_multiply( arTransformInsert,
                           mtrx_transform( vx, vy, _extrusion )
                        ) ;
  end ;

  // Transform insert.
  procedure TDWG_Transformation.TransformInsert(
    const _basePtg      : TGIS_Point3D ;
    const _oldTransform : TDWG_Matrix
   ) ;
  var
    block_mtrx : TDWG_Matrix ;
  begin
    arTransformInsert := mtrx_multiply( arTransformInsert, _oldTransform ) ;
    inc( inInsert ) ;

    block_mtrx        := mtrx_fill ;
    block_mtrx        := mtrx_inv_translate( _basePtg ) ;
    arTransformInsert := mtrx_multiply( block_mtrx, arTransformInsert ) ;
  end ;

  // Transform single point based on extrusion matrix.
  // _ptg    point to be transformed
  // return  transformed point
  function TDWG_Transformation.TransformPtg3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
    {$ENDIF}
    Result.X := _ptg.X * arTransform[ 0, 0 ] +
                _ptg.Y * arTransform[ 1, 0 ] +
                _ptg.Z * arTransform[ 2, 0 ] +
                         arTransform[ 3, 0 ] ;
    Result.Y := _ptg.X * arTransform[ 0, 1 ] +
                _ptg.Y * arTransform[ 1, 1 ] +
                _ptg.Z * arTransform[ 2, 1 ] +
                         arTransform[ 3, 1 ] ;
    Result.Z := _ptg.X * arTransform[ 0, 2 ] +
                _ptg.Y * arTransform[ 1, 2 ] +
                _ptg.Z * arTransform[ 2, 2 ] +
                         arTransform[ 3, 2 ] ;
    Result.M := 0                            ;
  end ;

  // Transform single point based on extrusion matrix.
  // _ptg    point to be transformed
  // return  transformed point
  function TDWG_Transformation.TransformPtg4D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
    {$ENDIF}
    Result.X := _ptg.X * arTransform[ 0, 0 ] +
                _ptg.Y * arTransform[ 1, 0 ] +
                _ptg.Z * arTransform[ 2, 0 ] +
                         arTransform[ 3, 0 ] ;
    Result.Y := _ptg.X * arTransform[ 0, 1 ] +
                _ptg.Y * arTransform[ 1, 1 ] +
                _ptg.Z * arTransform[ 2, 1 ] +
                         arTransform[ 3, 1 ] ;
    Result.Z := _ptg.X * arTransform[ 0, 2 ] +
                _ptg.Y * arTransform[ 1, 2 ] +
                _ptg.Z * arTransform[ 2, 2 ] +
                         arTransform[ 3, 2 ] ;
    Result.M := _ptg.X * arTransform[ 0, 3 ] +
                _ptg.Y * arTransform[ 1, 3 ] +
                _ptg.Z * arTransform[ 2, 3 ] +
                         arTransform[ 3, 3 ] ;
  end ;

  // Transform single point based on extrusion matrix.
  // _ptg    point to be transformed
  // return  transformed point
  function TDWG_Transformation.TransformPtg2D(
    const _ptg : TGIS_Point
  ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
    {$ENDIF}
    Result.X := _ptg.X * arTransform[ 0, 0 ] +
                _ptg.Y * arTransform[ 1, 0 ] +
                         arTransform[ 3, 0 ] ;
    Result.Y := _ptg.X * arTransform[ 0, 1 ] +
                _ptg.Y * arTransform[ 1, 1 ] +
                         arTransform[ 3, 1 ] ;
    Result.Z := _ptg.X * arTransform[ 0, 2 ] +
                _ptg.Y * arTransform[ 1, 2 ] +
                         arTransform[ 3, 2 ] ;
    Result.M := 0                            ;
  end ;

  // Get transformation matrix for insert.
  function TDWG_Transformation.GetTransformInsert : TDWG_Matrix ;
  begin
    Result := arTransformInsert ;
  end ;

  // Set transformation matrix for insert.
  procedure TDWG_Transformation.SetTransformInsert(
    const _old : TDWG_Matrix
  ) ;
  begin
    arTransformInsert := _old ;
    if inInsert > 0 then
      dec( inInsert ) ;
  end ;

//=============================================================================
// TGIS_LayerDWGExtendedDataEvent
//=============================================================================

{$IFDEF OXYGENE}
  constructor TGIS_LayerDWGExtendedDataEventArgs.Create(
    const _appname : String          ;
    const _shape   : TGIS_Shape      ;
    const _data    : TGIS_StringList
  ) ;
  begin
    inherited Create ;

    FAppName := _appname ;
    FShape   := _shape   ;
    FData    := _data    ;
  end;
{$ENDIF}

//=============================================================================
// TGIS_LayerDWG
//=============================================================================

  constructor TGIS_LayerDWG.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                           ] ;
    dwgFile := nil ;

    SupportsAutoStyle := False ;
  end ;

  procedure TGIS_LayerDWG.doDestroy ;
  begin

    inherited ;
  end ;

  procedure TGIS_LayerDWG.setUp ;
  var
    dwgModel : T_FileDwg_Model;
  begin
    inherited ;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;

    if FindField( GIS_DWG_HANDLE ) < 0 then
      AddFieldInternal( GIS_DWG_HANDLE, TGIS_FieldType.Number, 10, 0 ) ;
    if FindField( GIS_DWG_LAYER )  < 0 then
      AddFieldInternal( GIS_DWG_LAYER, TGIS_FieldType.String,   1, 0 ) ;
    if FindField( GIS_DWG_LABEL )  < 0 then
      AddFieldInternal( GIS_DWG_LABEL, TGIS_FieldType.String,   1, 0 ) ;

    dwgFile := TGIS_FileDWG.Create ;
    Lock ;
    try
      if TGIS_FileDWG(dwgFile).Open( Path ) then begin
        buildShapes ;
        dwgModel := TGIS_FileDWG(dwgFile).GetModel ;
        if not GisIsNoWorld3D( dwgModel.Extent ) then
          Extent3D := GisCommonExtent3D( Extent3D, dwgModel.Extent ) ;
      end ;
    finally
      Unlock ;
      FIsModified := False ;
      FreeObjectNotNil( TGIS_FileDWG(dwgFile) ) ;
      dwgFile := nil ;
      RaiseBusyRelease( Self ) ;
    end ;

    SetCSByEPSG(0) ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'AutoCAD DWG' ;
  end ;

  {$IFDEF JAVA}
    procedure TGIS_LayerDWG.add_ExtendedDataEvent(
      _e : TGIS_LayerDWGExtendedDataEvent
    ) ;
    begin
      FExtendedDataEvent := _e ;
    end;

    procedure TGIS_LayerDWG.remove_ExtendedDataEvent(
      _e : TGIS_LayerDWGExtendedDataEvent
    ) ;
    begin
      FExtendedDataEvent := nil ;
    end;
  {$ENDIF}

  function dwg_sort_by_name( const _item1, _item2 : TGIS_LayerAbstract ) : Integer ;
  begin
    Result := CompareText( TGIS_Layer(_item1).Name, TGIS_Layer(_item2).Name ) ;
  end ;

  procedure TGIS_LayerDWG.buildShapes;
  var
    dwgModel      : T_FileDwg_Model;
    dwgModelBlock : T_FileDwg_Block;
    {$IFDEF DCC}
    dwgEntity     : T_FileDwg_Entity;
    {$ENDIF}
    dwgTransform  : TDWG_Transformation ;
    shp           : TGIS_Shape;
    bpos          : Integer ;

    procedure doExtendedData(
      const _shp : TGIS_Shape ;
      const _e   : T_FileDwg_Entity
    ) ;
    var
      sappname : String           ;
      odata    : TStringList      ;
      {$IFDEF OXYGENE}
        e : TGIS_LayerDWGExtendedDataEventArgs ;
      {$ENDIF}
    begin
      if assigned( FExtendedDataEvent ) then begin
        sappname := '' ;

        odata := TStringList.Create ;
        try
          if assigned( _e.ExtData ) then
            odata.AddStrings( _e.ExtData ) ;
          {$IFDEF OXYGENE}
            e := TGIS_LayerDWGExtendedDataEventArgs.Create(
                   sappname,
                   _shp,
                   odata
                 ) ;
            try
              FExtendedDataEvent( self, e ) ;
            finally
              FreeObject( e ) ;
            end ;
          {$ELSE}
            FExtendedDataEvent(
              self,
              sappname,
              _shp,
              odata
            ) ;
          {$ENDIF}
        finally
          FreeObject( odata ) ;
        end;
      end ;
    end ;


    procedure applyStyle( const _shp: TGIS_Shape;
                          const _e  : T_FileDwg_Entity
                         ) ;
    var
      dstyle   : T_FileDwg_DrawStyle;
      clvwr    : TGIS_Color ;
      r,g,b    : Byte    ;
      vr,vg,vb : Byte    ;
      cl       : TGIS_Color ;
      la       : T_FileDwg_Layer ;
      bylayer  : Boolean ;
    begin
      if not assigned( _shp ) then exit ;
      
      dstyle := _e.GetDrawStyle( dwgModel ) ;

      if assigned( _e.Layer ) then
        la := _e.Layer
      else if assigned( _e.Parent ) then
        la := _e.Parent.Layer
      else
        la := nil ;

      cl := dstyle.Color ;
      r  := cl.R ;
      g  := cl.G ;
      b  := cl.B ;

      bylayer := True ;
      if assigned( la ) then begin

        if ( cl <> la.Style.Color.Color ) then
          bylayer := False ;

        if ( dstyle.Pen <> la.Style.Pen.Pen ) then
          bylayer := False ;

        if ( dstyle.Width <> TruncS(la.Style.Width.Width) ) then
          bylayer := False ;
      end
      else
        bylayer := False ;

      if assigned( Viewer ) then
        clvwr :=  Viewer.Ref.Color
      else
        clvwr := TGIS_Color.Black ;

      vr := clvwr.R ;
      vg := clvwr.G ;
      vb := clvwr.B ;

      if ( Abs( vr - r ) < 16 ) and
         ( Abs( vg - g ) < 16 ) and
         ( Abs( vb - b ) < 16 )
      then // color very close to the window color
        cl := TGIS_Color.FromRGB( r xor vr, g xor vg, b xor vb ) ;

      if not bylayer then begin
        case _shp.ShapeType of
          TGIS_ShapeType.Point, TGIS_ShapeType.MultiPoint:
            begin
              _shp.Params.Marker.Color := cl;
              _shp.Params.Marker.Size  := 1;
            end ;
          TGIS_ShapeType.Arc:
            begin
              _shp.Params.Line.Color := cl;
              _shp.Params.Line.Style := dstyle.Pen;
              _shp.Params.Line.Width := dstyle.Width;
            end ;
          TGIS_ShapeType.Polygon,
          TGIS_ShapeType.MultiPatch:
            begin
              _shp.Params.Area.Color        := cl;
              _shp.Params.Area.OutlineColor := dstyle.Color;
              _shp.Params.Area.OutlineStyle := dstyle.Pen;
            end ;
        end ;
        _shp.Params.Ground := TGIS_3DGroundType.AboveZero ;
      end ;

      if assigned( la ) then
        _shp.Layer := la.NativeLayer as TGIS_LayerVector;

      doExtendedData( _shp, _e ) ;
    end ;

    procedure setTransform(
      const _v : TGIS_Point3D
    ) ;
    begin
      dwgTransform.PrepareMatrix( _v ) ;
    end ;

    function transformPtg2D(
      const _v : TGIS_Point
    ) : TGIS_Point3D ;
    begin
      Result := dwgTransform.TransformPtg2D( _v ) ;
    end ;

    function transformPtg3D(
      const _v : TGIS_Point3D
    ) : TGIS_Point3D ;
    begin
      Result := dwgTransform.TransformPtg3D( _v ) ;
    end ;

    function transformPtg4D(
      const _v : TGIS_Point3D
    ) : TGIS_Point3D ;
    begin
      Result := dwgTransform.TransformPtg4D( _v ) ;
    end ;

    procedure buildPoint( const _e : T_FileDwg_Entity );
    var
      pt  : T_FileDwg_Point;
    begin
      pt := _e as T_FileDwg_Point;
      setTransform( pt.Extrusion ) ;

      shp := CreateShape(TGIS_ShapeType.Point, TGIS_DimensionType.XYZ);
      shp.Lock(TGIS_Lock.Projection);
      shp.AddPart;
      shp.AddPoint3D( transformPtg3D( pt.Ptg ) );
      shp.Unlock;

      applyStyle(shp, _e);
    end ;

    procedure buildLine( const _e : T_FileDwg_Entity );
    var
      line: T_FileDwg_Line;

    begin
      line := _e as T_FileDwg_Line;
      setTransform( line.Extrusion ) ;

      if not assigned( shp ) then
        shp := CreateShape(TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ);
      shp.Lock(TGIS_Lock.Projection);
      shp.AddPart;
      shp.AddPoint3D( transformPtg3D( line.Ptg1 ) );
      shp.AddPoint3D( transformPtg3D( line.Ptg2 ) );
      shp.Unlock;

      applyStyle(shp, _e);
    end ;

    procedure prepareDimension( const _pt1 : TGIS_Point3D ;
                                const _pt2 : TGIS_Point3D ;
                                const _a1  : TGIS_Point3D ;
                                const _txt : TGIS_Point3D ;
                                const _shp : TGIS_Shape
                               ) ;
    var
      vec1    : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
      vec2    : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
      a2      : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
      l1m     : Double ;
      l1b     : Double ;
      l2m     : Double ;
      l2b     : Double ;
    begin
      // compute direction vector between Target and Arrow
      vec1.X := (_a1.X - _pt1.X) ;
      vec1.Y := (_a1.Y - _pt1.Y) ;
      vec2.X := vec1.Y ;
      vec2.Y := -vec1.X ;

      // compute intersection of lines
      if ( vec1.X = 0.0 ) then begin
        a2.X := _pt2.X ;
        a2.Y := _a1.Y ;
      end
      else if ( vec1.Y = 0.0 ) then begin
        a2.X := _a1.X ;
        a2.Y := _pt2.Y ;
      end
      else begin
        l1m := vec1.Y / vec1.X ;
        l1b := _pt2.Y - l1m * _pt2.X ;

        l2m := vec2.Y / vec2.X ;
        l2b := _a1.Y - l2m * _a1.X ;

        a2.X := (l2b - l1b) / (l1m-l2m) ;
        a2.Y := l2m * a2.X + l2b ;
      end ;

      shp.AddPoint3D( GisPoint3D( _a1.X, _a1.Y, _a1.Z ) ) ;
      shp.AddPoint3D( GisPoint3D( a2.X, a2.Y, _a1.Z ) ) ;
    end ;


    procedure buildCircle( const _e : T_FileDwg_Entity );
    var
      c     : T_FileDwg_Circle;
      a     : Double ;
      i     : Integer ;
      p     : TGIS_Point3D ;
      step  : Double       ;
      steps : Integer      ;
      rsin, rcos : Double       ;
    begin
      c := _e as T_FileDwg_Circle;
      setTransform( c.Extrusion ) ;

      if not assigned( shp ) then
        shp := CreateShape(TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ);
      shp.Lock(TGIS_Lock.Projection);
      shp.AddPart;

      steps := 90 - 1 ;
      step  := Abs( 2 * Pi ) / steps ;
      a := Pi/2 ;

      for i := 0 to steps do begin
        SinCos( a, rsin, rcos ) ;
        p := GisPoint3D( c.Center.X + c.Radius * rcos,
                         c.Center.Y + c.Radius * rsin,
                         c.Center.Z + c.Radius
                       ) ;
        shp.AddPoint3D( transformPtg3D(p) ) ;
        a := a + step ;
      end ;
      shp.Unlock;

      applyStyle(shp, _e);
    end ;

    procedure buildArc( const _e : T_FileDwg_Entity );
    var
      arc       : T_FileDwg_Arc;
      a,sa,ea   : Double ;
      i         : Integer ;
      p         : TGIS_Point3D ;
      isa, iea  : Integer ;
      rsin, rcos: Double ;
    begin
      arc := _e as T_FileDwg_Arc;
      setTransform( arc.Extrusion ) ;

      if not assigned( shp ) then
        shp := CreateShape(TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ);
      shp.Lock(TGIS_Lock.Projection);
      shp.AddPart;

      if ( arc.StartAngle < 0 ) then
        sa := arc.StartAngle + 2*Pi
      else if ( arc.StartAngle > (2*Pi) ) then
        sa := arc.StartAngle - (2*Pi)
      else
        sa := arc.StartAngle ;

      if ( arc.EndAngle < 0 ) then
        ea := arc.EndAngle + 2*Pi
      else if ( arc.EndAngle > (2*Pi) ) then
        ea := arc.EndAngle - (2*Pi)
      else
        ea := arc.EndAngle ;

      isa := RoundS( sa*180/Pi ) ;
      iea := RoundS( ea*180/Pi ) ;

      if ( sa <= ea ) then begin
        a := sa;
        p := GisPoint3D( arc.Center.X + arc.Radius * Cos(a),
                         arc.Center.Y + arc.Radius * Sin(a),
                         arc.Center.Z + arc.Radius
                       ) ;
        shp.AddPoint3D( transformPtg3D(p) ) ;

        for i := 1 to (iea-isa) do begin
          a := (isa + i)*Pi/180.0 ;
          SinCos( a, rsin, rcos ) ;
          p := GisPoint3D( arc.Center.X + arc.Radius * rcos,
                           arc.Center.Y + arc.Radius * rsin,
                           arc.Center.Z + arc.Radius
                         ) ;
          shp.AddPoint3D( transformPtg3D(p) ) ;
        end ;
        a := ea ;
        p := GisPoint3D( arc.Center.X + arc.Radius * Cos(a),
                         arc.Center.Y + arc.Radius * Sin(a),
                         arc.Center.Z + arc.Radius
                       ) ;
        shp.AddPoint3D( transformPtg3D(p) ) ;
      end
      else begin
        a := sa ;
        p := GisPoint3D( arc.Center.X + arc.Radius * Cos(a),
                         arc.Center.Y + arc.Radius * Sin(a),
                         arc.Center.Z + arc.Radius
                       ) ;
        shp.AddPoint3D( transformPtg3D(p) ) ;
        for i := 1 to (360-isa)-1 do begin
          a := (isa+i)*Pi/180.0 ;
          SinCos( a, rsin, rcos ) ;
          p := GisPoint3D( arc.Center.X + arc.Radius * rcos,
                           arc.Center.Y + arc.Radius * rsin,
                           arc.Center.Z + arc.Radius
                         ) ;
          shp.AddPoint3D( transformPtg3D(p) ) ;
        end ;

        for i := (360-isa)+1 to (360-isa)+iea do begin
          a := (i-(360-isa))*Pi/180.0 ;
          p := GisPoint3D( arc.Center.X + arc.Radius * Cos(a),
                           arc.Center.Y + arc.Radius * Sin(a),
                           arc.Center.Z + arc.Radius
                         ) ;
          shp.AddPoint3D( transformPtg3D(p) ) ;
        end ;
        a := ea;
        p := GisPoint3D( arc.Center.X + arc.Radius * Cos(a),
                         arc.Center.Y + arc.Radius * Sin(a),
                         arc.Center.Z + arc.Radius
                       ) ;
        shp.AddPoint3D( transformPtg3D(p) ) ;
      end ;
      shp.Unlock;

      applyStyle(shp, _e);
    end ;

    procedure buildEllipse( const _e : T_FileDwg_Entity );
    var
      elipse : T_FileDwg_Ellipse ;
      a,sa,ea   : Double ;
      rx,ry,ra  : Double ;
      i         : Integer ;
      p         : TGIS_Point3D ;
      isa, iea  : Integer ;

       procedure calculatePtg(
           var _p : TGIS_Point3D ;
         const _c : TGIS_Point3D ;
         const _a : Double
       ) ;
       var
         dx, dy : Double ;
         rsin, rcos : Double ;
       begin
         SinCos( _a, rsin, rcos ) ;
         dx := _p.X*rcos - _p.Y*rsin ;
         dy := _p.X*rsin + _p.Y*rcos ;
         _p.X := _c.X + dx ;
         _p.Y := _c.Y + dy ;
         _p.Z := _c.Z ;
       end ;

    begin
      elipse := _e as T_FileDwg_Ellipse;

      if not assigned( shp ) then
        shp := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
      setTransform( elipse.Extrusion ) ;

      if not GisIsSamePoint3D( elipse.Extrusion, GisPoint3D( 0, 0, 1 ) ) then begin
        elipse.Center:= transformPtg3D( elipse.Center ) ;
        elipse.MajorAxis:= transformPtg3D( elipse.MajorAxis ) ;
      end ;

      shp.Lock( TGIS_Lock.Projection ) ;
        shp.AddPart ;

        if ( elipse.InitAngle > elipse.EndAngle ) then
          elipse.EndAngle := elipse.EndAngle + 2*Pi ;

        sa  := elipse.InitAngle ;
        ea  := elipse.EndAngle ;
        isa := RoundS( sa*180/Pi ) ;
        iea := RoundS( ea*180/Pi ) ;

        rx := Sqrt( elipse.MajorAxis.X * elipse.MajorAxis.X +
                    elipse.MajorAxis.Y * elipse.MajorAxis.Y +
                    elipse.MajorAxis.Z * elipse.MajorAxis.Z
                   ) ;
        ry := rx * elipse.AxisRatio ;
        ra := ArcTan2( elipse.MajorAxis.Y, elipse.MajorAxis.X ) ;

        if ( sa <= ea ) then begin
          a := sa;
          p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
          calculatePtg( p, elipse.Center, ra ) ;
          shp.AddPoint3D( transformPtg3D(p) ) ;

          for i := 1 to (iea-isa) do begin
            a := (isa + i)*Pi/180.0 ;
            p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
            calculatePtg( p, elipse.Center, ra ) ;
            shp.AddPoint3D( transformPtg3D(p) ) ;
          end ;
          a := ea ;
          p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
          calculatePtg( p, elipse.Center, ra ) ;
          shp.AddPoint3D( transformPtg3D(p) ) ;
        end
        else begin
          a := sa ;
          p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
          calculatePtg( p, elipse.Center, ra ) ;
          shp.AddPoint3D( transformPtg3D(p) ) ;
          for i := 1 to (360-isa)-1 do begin
            a := (isa+i)*Pi/180.0 ;
            p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
            calculatePtg( p, elipse.Center, ra ) ;
            shp.AddPoint3D( transformPtg3D(p) ) ;
          end ;

          for i := (360-isa)+1 to (360-isa)+iea do begin
            a := (i-(360-isa))*Pi/180.0 ;
            p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
            calculatePtg( p, elipse.Center, ra ) ;
            shp.AddPoint3D( transformPtg3D(p) ) ;
          end ;
          a := ea;
          p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
          calculatePtg( p, elipse.Center, ra ) ;
          shp.AddPoint3D( transformPtg3D(p) ) ;
        end ;

      shp.Unlock ;

      applyStyle(shp, _e);
    end ;

    function calculateBulge(
      const _v1       : TGIS_Point3D ;
      const _v2       : TGIS_Point3D ;
        var _center   : TGIS_Point3D ;
        var _radiusA  : Double ;
        var _radiusB  : Double ;
        var _start    : Double ;
        var _stop     : Double ;
        var _rotation : Double
     ) : Boolean ;
    var
      len           : Double ;
      radius        : Double ;
      h             : Double ;
      bulge         : Double ;
      saggita       : Double ;
      apo           : Double ;
      clockwise     : Boolean ;
      v             : TGIS_Point ;
      midpoint      : TGIS_Point ;
      pperp         : TGIS_Point ;
      nl            : Double ;
      linedir       : Double ;
      a             : Double ;
      arccenter     : TGIS_Point3D ;
      arcradius     : Double ;
      arcstartangle : Double ;
      arcendangle   : Double ;
      arcrotation   : Double ;
    begin
      Result := False ;
      len := Sqrt( Sqr( _v2.X - _v1.X ) + Sqr( _v2.Y - _v1.Y) ) ;
      if ( len = 0 ) or ( _v1.M = 0 ) then exit ;

      {$IFDEF GIS_NORECORDS}
        v         := new TGIS_Point ;
        midpoint  := new TGIS_Point ;
        pperp     := new TGIS_Point ;
        arccenter := new TGIS_Point3D ;
      {$ENDIF}

      bulge := _v1.M ;
      h := (bulge * len) / 2 ;
      radius := (h/2) + (len*len / (8*h)) ;
      arcradius := Abs(radius) ;

      clockwise := (bulge < 0) ;
      if clockwise then
        bulge := bulge * -1 ;

      saggita := Abs( bulge * (len / 2.0) ) ;
      if clockwise then
        apo := -(arcradius - saggita)
      else
        apo := -(saggita - arcradius) ;

      v.X := _v1.X - _v2.X ;
      v.Y := _v1.Y - _v2.Y ;

      midpoint.X := _v2.X + 0.5 * v.X ;
      midpoint.Y := _v2.Y + 0.5 * v.Y ;
      pperp.X := v.Y ;
      pperp.Y := -v.X ;
      nl := Sqrt( pperp.X*pperp.X + pperp.Y*pperp.Y ) ;
      pperp.X := pperp.X / nl ;
      pperp.Y := pperp.Y / nl ;

      arccenter.X := midpoint.X + (pperp.X * apo) ;
      arccenter.Y := midpoint.Y + (pperp.Y * apo) ;
      arccenter.Z := _v1.Z ;
      if _v2.Y > _v1.Y then
        linedir := 1
      else
        linedir := -1 ;

      a := ArcTan2((arccenter.Y - _v1.Y), (arccenter.X - _v1.X)) * 180.0 / Pi ;
      if clockwise and (linedir = 1.0) then
        a := a + (linedir * 180.0) ;

      if a > 0.0 then
        arcstartangle := -(a - 180.0)
      else
        arcstartangle := -(a + 180.0) ;

      a := ArcTan2((arccenter.Y - _v2.Y), (arccenter.X - _v2.X)) * 180.0 / Pi ;
      if clockwise and (linedir = 1.0) then
        a := a + (linedir * 180.0) ;

      if a > 0.0 then
        arcendangle := -(a - 180.0)
      else
        arcendangle := -(a + 180.0) ;

      if not clockwise and (arcstartangle < arcendangle) then
        arcendangle := -180 + (linedir * a) ;

      arcrotation := 0 ;
      if clockwise and (linedir = 1.0) then
        arcrotation := (linedir * 180.0) ;

      _center   := arccenter ;
      _radiusA  := arcradius ;
      _radiusB  := arcradius ;
      _start    := DegToRad( arcstartangle ) ;
      _stop     := DegToRad( arcendangle ) ;
      _rotation := DegToRad( arcrotation ) ;
      Result := True ;
    end ;

    procedure strokeArc(
      const _center   : TGIS_Point3D ;
      const _radiusA  : Double       ;
      const _radiusB  : Double       ;
      const _start    : Double       ;
      const _stop     : Double       ;
      const _rotation : Double       ;
      const _segments : Integer      ;
      const _shp      : TGIS_Shape
    ) ;
    var
      cnt      : Integer ;
      angle    : Double  ;
      arcangle : Double  ;
      delta    : Double  ;
      step     : Double  ;
      steps    : Integer ;
      rsin     : Double  ;
      rcos     : Double  ;
      ptg      : TGIS_Point3D ;

      function fmod( a, b : Double ) : Double ;
      var
       f : Integer ;
      begin
        f := TruncS( a/b ) ;
        Result := a - (b*f) ;
      end ;
    begin
      {$IFDEF GIS_NORECORDS} 
        ptg  := new TGIS_Point3D ;
      {$ENDIF}

      // rotation
       SinCos( -_rotation, rsin, rcos ) ;

       delta := (_stop-_start) ;

       if delta > 0 then
         arcangle := fmod( (_stop-_start) + 4*Pi, 2*Pi)
       else
         arcangle := fmod( (_stop-_start) -4*Pi, 2*Pi) ;

       if arcangle = 0 then begin
         // full rotation
         if _stop <> _start then
           arcangle := 2*Pi ;
       end ;

      // calculate number of segments - and minimize it
       steps := RoundS( Abs( arcangle ) / (2*Pi) * _segments ) ;

       step := arcangle / Max( 4, steps ) ;

      // calculate elliptical arc
       angle := _start ;
       for cnt := 0 to steps do begin
         ptg.Y := -_radiusB * Sin(angle) * rcos +
                   _radiusA * Cos(angle) * rsin + _center.Y ;
         ptg.X :=  _radiusA * Cos(angle) * rcos +
                   _radiusB * Sin(angle) * rsin + _center.X ;
         ptg.Z := _center.Z ;
         _shp.AddPoint3D( transformPtg3D( ptg ) ) ;

         angle := angle + step ;
       end ;
    end ;

    procedure buildLWPolyline( const _e : T_FileDwg_Entity );
    var
      lw            : T_FileDwg_LWPolyline ;
      i,np          : Integer ;
      arccenter     : TGIS_Point3D ;
      arcradiusa    : Double ;
      arcradiusb    : Double ;
      arcstartangle : Double ;
      arcendangle   : Double ;
      arcrotation   : Double ;
    begin
      lw := _e as T_FileDwg_LWPolyline;

      if not assigned( shp ) then
        shp := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;

      if (lw.Extrusion.X=0) and (lw.Extrusion.Y=0) and (lw.Extrusion.Z=0) then
        lw.Extrusion.Z := 1 ;

      setTransform( lw.Extrusion ) ;

      shp.Lock( TGIS_Lock.Projection ) ;
        shp.AddPart ;

        if lw.NumBulges = 0 then begin
          if lw.NumVertex > 0 then begin
            for i := 0 to lw.NumVertex-1 do
              shp.AddPoint3D( transformPtg3D( lw.Vertices[i] ) ) ;

            if ( lw.Flag and 1 ) = 1 then
              shp.AddPoint3D( transformPtg3D( lw.Vertices[0] ) ) ;
          end ;
        end
        else begin
          if lw.NumVertex > 0 then begin
            i := 0 ;
            np := lw.NumVertex ;
            shp.AddPoint3D( transformPtg3D( lw.Vertices[i] ) ) ;
            while i < np-1 do begin
              if calculateBulge( lw.Vertices[i], lw.Vertices[i+1],
                                 arccenter, arcradiusa, arcradiusb,
                                 arcstartangle, arcendangle, arcrotation
                               ) then
              begin
                strokeArc( arccenter, arcradiusa, arcradiusb, arcstartangle,
                           arcendangle, arcrotation, 90, shp
                          )
              end
              else begin
                shp.AddPoint3D( transformPtg3D( lw.Vertices[i  ] ) ) ;
                shp.AddPoint3D( transformPtg3D( lw.Vertices[i+1] ) ) ;
              end ;
              inc( i ) ;
            end ;

            if ( lw.Flag and 1 ) = 1 then begin
              if calculateBulge( lw.Vertices[i], lw.Vertices[0],
                                 arccenter, arcradiusa, arcradiusb,
                                 arcstartangle, arcendangle, arcrotation
                               ) then
              begin
                strokeArc( arccenter, arcradiusa, arcradiusb, arcstartangle,
                           arcendangle, arcrotation, 90, shp
                          )
              end
              else begin
                shp.AddPoint3D( transformPtg3D( lw.Vertices[np-1] ) ) ;
              end ;
            end ;
            if ( lw.Flag and 1 ) = 1 then
              shp.AddPoint3D( transformPtg3D( lw.Vertices[0] ) ) ;
        end ;
      end ;

      shp.Unlock ;

      applyStyle(shp, _e);
    end ;

    procedure buildLeader( const _e : T_FileDwg_Entity );
    var
      ld  : T_FileDwg_Leader ;
      i   : Integer ;
    begin
      ld := _e as T_FileDwg_Leader;

      if not assigned( shp ) then
        shp := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;

      if (ld.Extrusion.X=0) and (ld.Extrusion.Y=0) and (ld.Extrusion.Z=0) then
        ld.Extrusion.Z := 1 ;

      setTransform( ld.Extrusion ) ;

      shp.Lock( TGIS_Lock.Projection ) ;
        shp.AddPart ;

        for i := 0 to ld.NumVertex-1 do
          shp.AddPoint3D( transformPtg3D( ld.Vertices[i] ) ) ;

      shp.Unlock ;

      applyStyle(shp, _e);
    end ;

    function parseText( const lineName : String ) : String ;
    var
      state     : Integer ;
      iPos      : Integer ;
      len       : Integer ;
      c         : Char    ;
      code      : Char ;
      text      : TStringBuilder ;
      codevalue : TStringBuilder ;
      tkn       : TGIS_Tokenizer ;
      revert    : TStringList ;
      cval      : Integer ;
      color     : TGIS_Color ;
      special   : String ;
      hex       : Integer ;


        procedure next_char ;
        begin
          inc( iPos ) ;
          if iPos < StringFirst + len then
            c := lineName[ iPos ]
          else
            c := #0 ;
        end ;

        procedure collect_code ;
        begin
          codevalue.Clear ;
          while (c <> ';') and (c <> #0) do begin
            next_char ;
            if c <> ';' then
              codevalue.Append( c ) ;
          end ;
        end ;

        function parse_format : String ;
        var
          ii : Integer ;
        begin
          Result := '' ;
          iPos  := StringFirst ;
          len   := length( lineName ) ;
          if len = 0 then exit ;

          c     := lineName[ iPos ] ;
          state := 0 ;

          text      := TStringBuilder.Create ;
          codevalue := TStringBuilder.Create ;
          tkn       := TGIS_Tokenizer.Create ;
          revert    := TStringList.Create ;
          try
            while iPos < StringFirst + len do begin
              case state of
                // start
                0 : state := 1 ;
                // start format
                1 : if c = '{' then begin
                      next_char ;
                    end
                    else if c = '}' then begin
                      for ii := 0 to revert.Count-1 do
                        text.Append( revert[ii] ) ;
                      revert.Clear ;
                      next_char
                    end
                    else if c = '\' then begin
                      state := 3 ;
                      next_char ;
                    end
                    else if c = '%' then begin
                      special := UpperCase( &Copy( lineName, iPos, 3) ) ;
                      if      special = '%%C' then begin
                        text.Append( #$2300 ) ;
                        next_char ;
                        next_char ;
                        next_char ;
                      end
                      else if special = '%%D' then begin
                        text.Append( #$B0 ) ;
                        next_char ;
                        next_char ;
                        next_char ;
                      end
                      else if special = '%%P' then begin
                        text.Append( #$B1 ) ;
                        next_char ;
                        next_char ;
                        next_char ;
                      end
                      else if special = '%%U' then begin
                        text.Append( '</U>' ) ;
                        next_char ;
                        next_char ;
                        next_char ;
                      end
                      else if special = '%%O' then begin
                        next_char ;
                        next_char ;
                        next_char ;
                      end
                      else
                        next_char ;
                      state := 1 ;
                    end
                    else
                      state := 4 ;
                // format code
                3 : begin
                      code := c ;

                      if (code = 'f') or (code = 'F') then begin
                        // read font
                        collect_code ;

                        for ii := 0 to revert.Count-1 do
                          text.Append( revert[ii] ) ;

                        tkn.Execute( codevalue.ToString, ['|'] ) ;
                        for ii := 0 to tkn.Result.Count-1 do begin
                          if ii = 0 then begin
                            text.Append( Format( '<FNT NAME="%s">', [tkn.Result[0]] ) ) ;
                            revert.Add( '</FNT>' ) ;
                          end
                          else if tkn.Result[ii] = 'b1' then begin
                            text.Append( '<B>' ) ;
                          end
                          else if tkn.Result[ii] = 'b0' then begin
                            text.Append( '</B>' ) ;
                          end
                          else if tkn.Result[ii] = 'i1' then begin
                            text.Append( '<I>' ) ;
                          end
                          else if tkn.Result[ii] = 'i0' then begin
                            text.Append( '</I>' ) ;
                          end ;
                        end ;
                      end
                      else if (code = 's') or (code = 'S') then begin
                        // read stacking
                        collect_code ;
                        text.Append( codevalue.ToString ) ;
                      end
                      else if (code = 'a') or (code = 'A') then begin
                        // read alignment
                        collect_code ;
                      end
                      else if (code = 'c') or (code = 'C') then begin
                        // read color
                        collect_code ;
                        if TryStrToInt( codevalue.ToString, cval ) then begin
                          if (cval >= 0) and (cval < 256) then begin
                            color := dwgModel.GetPaletteColor( cval ) ;
                            text.Append( Format( '<CLR red="%d" green="%d" blue="%d">',
                                                 [ color.R, color.G, color.B ]
                                               )
                                        ) ;
                            revert.Add( '</CLR>' ) ;
                          end ;
                        end
                      end
                      else if (code = 'L') then begin
                        // start underline
                        text.Append( '<U>' ) ;
                      end
                      else if (code = 'l') then begin
                        // stop underline
                        text.Append( '</U>' ) ;
                      end
                      else if (code = 'O') then begin
                        // start overstrike
                      end
                      else if (code = 'o') then begin
                        // stop overstrike
                      end
                      else if (code = 'K') then begin
                        // start strike-through
                      end
                      else if (code = 'k') then begin
                        // stop strike-through
                      end
                      else if (code = 'P') then begin
                        // numbered paragraph
                        text.Append( '<BR>' ) ;
                      end
                      else if (code = 'p') then begin
                        // control codes
                        collect_code ;
                      end
                      else if (code = 'X') then begin
                        // paragraph wrap
                      end
                      else if (code = 'Q') then begin
                        // slanting text
                        collect_code ;
                      end
                      else if (code = 'H') then begin
                        // text height
                        collect_code ;
                      end
                      else if (code = 'W') then begin
                        // text width
                        collect_code ;
                      end
                      else if (code = 'T') then begin
                        // tracking spacing
                        collect_code ;
                      end
                      else if (code = '~') then begin
                        // hard space
                        text.Append( ' ') ;
                      end
                      else if (code = '\') then begin
                        // escape char
                        text.Append( '\') ;
                      end
                      else if (code = 'U') then begin
                        // unicode
                        next_char ;
                        if c = '+' then begin
                          next_char ;
                          hex := StrToIntDef( '$'+&Copy( lineName, iPos, 4 ), 0 ) ;
                          text.Append( Char( hex ) ) ;
                          next_char ;
                          next_char ;
                          next_char ;
                        end
                        else begin
                          text.Append( '\') ;
                          text.Append( 'U') ;
                          text.Append( c ) ;
                        end;
                      end
                      else begin
                        text.Append( '\') ;
                        text.Append( c ) ;
                      end ;
                      next_char ;
                      state := 1 ;
                    end ;
                // collect text
                4 : begin
                      text.Append( c ) ;
                      next_char ;
                      state := 1 ;
                    end ;
              end ;
            end ;
          finally
            Result := text.ToString ;

            FreeObject( codevalue ) ;
            FreeObject( text      ) ;
            FreeObject( tkn       ) ;
            FreeObject( revert    ) ;
          end ;
        end ;
    begin
      Result := parse_format ;
    end ;

    procedure buildText( const _e : T_FileDwg_Entity );
    var
      txt : T_FileDwg_Text;
      pos : TGIS_LabelPositions ;
    begin
      txt := _e as T_FileDwg_Text;
      setTransform( txt.Extrusion ) ;

      shp := CreateShape(TGIS_ShapeType.Point, TGIS_DimensionType.XYZ);
      shp.Lock(TGIS_Lock.Projection);
      shp.AddPart;
      shp.AddPoint3D( transformPtg3D( txt.InsertionPoint ) );
      shp.Unlock;

      applyStyle(shp, _e);

      shp.Params.Labels.FontSizeAsText := 'SIZE:' + DotFloatToStr( txt.Height *7/10 ) + 'mu' ;
      shp.Params.Labels.Width := -999999;

      if txt.Angle <> 0 then
        shp.Params.Labels.Rotate := -txt.Angle ;
      if not IsStringEmpty( txt.Text ) then
        shp.SetField( GIS_DWG_LABEL, parseText( txt.Text ) ) ;

       pos := GisGetLabelPosition( TGIS_LabelPosition.MiddleCenter ) ;
       case txt.AlignH of
          TGIS_TextHAlign.HLeft,
          TGIS_TextHAlign.HAligned,
          TGIS_TextHAlign.HFit :
           case txt.AlignV of
             TGIS_TextVAlign.VBaseLine,
             TGIS_TextVAlign.VMiddle :
                pos := GisGetLabelPosition( TGIS_LabelPosition.MiddleRight  ) ;
             TGIS_TextVAlign.VBottom   :
                pos := GisGetLabelPosition( TGIS_LabelPosition.UpRight      ) ;
             TGIS_TextVAlign.VTop   :
                pos := GisGetLabelPosition( TGIS_LabelPosition.DownRight    ) ;
           end ;
         TGIS_TextHAlign.HMiddle,
         TGIS_TextHAlign.HCenter   :
           case txt.AlignV of
             TGIS_TextVAlign.VBaseLine,
             TGIS_TextVAlign.VMiddle :
                pos := GisGetLabelPosition( TGIS_LabelPosition.MiddleCenter ) ;
             TGIS_TextVAlign.VBottom   :
                pos := GisGetLabelPosition( TGIS_LabelPosition.UpCenter     ) ;
             TGIS_TextVAlign.VTop   :
                pos := GisGetLabelPosition( TGIS_LabelPosition.DownCenter   ) ;
           end ;
         TGIS_TextHAlign.HRight     :
           case txt.AlignV of
             TGIS_TextVAlign.VBaseLine,
             TGIS_TextVAlign.VMiddle :
                pos := GisGetLabelPosition( TGIS_LabelPosition.MiddleLeft   ) ;
             TGIS_TextVAlign.VBottom   :
                pos := GisGetLabelPosition( TGIS_LabelPosition.UpLeft       ) ;
             TGIS_TextVAlign.VTop   :
                pos := GisGetLabelPosition( TGIS_LabelPosition.DownLeft     ) ;
           end ;
       end ;

       if shp.Layer.Params.Labels.Position <> pos then
         shp.Layer.Params.Labels.Position := pos ;
    end ;

    procedure buildSolid( const _e : T_FileDwg_Entity );
    var
      solid : T_FileDwg_Solid;
    begin
      solid := _e as T_FileDwg_Solid;
      setTransform( solid.Extrusion ) ;

      shp := CreateShape(TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ);
      shp.Lock(TGIS_Lock.Projection);
      shp.AddPart;
      shp.AddPoint3D( transformPtg3D( solid.Ptg2 ) );
      shp.AddPoint3D( transformPtg3D( solid.Ptg1 ) );
      shp.AddPoint3D( transformPtg3D( solid.Ptg3 ) );
      shp.AddPoint3D( transformPtg3D( solid.Ptg4 ) );
      shp.Unlock;

      applyStyle(shp, _e);
    end ;

    procedure buildFace3D( const _e : T_FileDwg_Entity );
    var
      face  : T_FileDwg_3DFace;
    begin
      face := _e as T_FileDwg_3DFace;
      setTransform( face.Extrusion ) ;

      shp := CreateShape(TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ);
      shp.Lock(TGIS_Lock.Projection);
      shp.AddPart;

      shp.AddPoint3D( transformPtg3D( face.Ptg1 ) ) ;
      shp.AddPoint3D( transformPtg3D( face.Ptg2 ) ) ;
      shp.AddPoint3D( transformPtg3D( face.Ptg3 ) ) ;
      if not GisIsSamePoint3D( face.Ptg3, face.Ptg4 ) then
        shp.AddPoint3D( transformPtg3D( face.Ptg4 ) ) ;

      shp.Unlock;

      applyStyle(shp, _e);
    end ;

    procedure drawSpline(
      const _shp      : TGIS_Shape ;
      const _cp       : TList<TGIS_Point3D> ;
      const _cpCount  : Integer ;
      const _knot     : TList<Double> ;
      const _knotCount: Integer
     ) ;
    var
      i    : Integer ;
      step : Double ;
      t    : Double ;

      function N( _n, _i : Integer ; _t : Double ) : Double ;
      var
        v1, d1, v2, d2 : Double ;
      begin
        v1 := 0 ;
        v2 := 0 ;
        if _n = 0 then begin
          if ((_knot[_i] <= _t) and (_t < _knot[_i+1])) then
            Result := 1.0
          else
            Result := 0.0 ;
          exit ;
        end
        else begin
          if _i+_n < _knotCount then begin
            d1 := _knot[_i+_n] - _knot[_i] ;
            v1 := (_t - _knot[_i]) * N(_n - 1, _i, _t ) ;
          end
          else
            d1 := 0 ;

          if d1 = 0 then
            v1 := 0
          else
            v1 := (v1 / d1) ;

          if _i+_n+1 < _knotCount then begin
            d2 := _knot[_i+_n+1] - _knot[_i+1] ;
            v2 := (_knot[_i + _n + 1] - _t) * N(_n - 1, _i + 1, _t ) ;
          end
          else
            d2 := 0 ;

          if d2 = 0 then
            v2 := 0
          else
            v2 := (v2 / d2) ;

          Result := v1 + v2 ;
        end ;
      end ;

      function NURBS_3( _j : Integer ; _t : Double ) : TGIS_Point3D ;
      var
        ni : Double ;
        k  : Integer ;
      begin
        {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Point3D() ;
        {$ENDIF}
        Result.X := 0 ;
        Result.Y := 0 ;
        Result.Z := 0 ;

        for k := _j-3 to _j do begin
          ni := N(3, k, _t) ;
          Result.X := Result.X + _cp[k].X * ni ;
          Result.Y := Result.Y + _cp[k].Y * ni ;
          Result.Z := Result.Z + _cp[k].Z * ni ;
        end ;
      end ;

    begin
      // draw nurbs
      _shp.AddPoint3D( transformPtg3D( _cp[0] ) ) ;

      for i := 3 to _cpCount-1 do begin

        if i+1 < _knotCount then
          step := (_knot[i+1]-_knot[i]) / 25
        else
          step := 0 ;
        t := _knot[i] ;
        while ( step > 0 ) and ( t < _knot[i+1] ) do begin
          _shp.AddPoint3D( transformPtg3D( NURBS_3( i, t ) ) ) ;
          t := t + step ;
        end ;
      end ;

      _shp.AddPoint3D( transformPtg3D( _cp[_cpCount-1] ) ) ;
    end ;

    procedure buildSpline( const _e : T_FileDwg_Entity );
    var
      spline : T_FileDwg_Spline;
      i      : Integer ;
      t      : Double ;
      ptg    : TGIS_Point3D ;
      splint : TDWG_BSplineInterpolator ;
    begin
      spline := _e as T_FileDwg_Spline;
      setTransform( spline.Extrusion ) ;

      if not assigned( shp ) then
        shp := CreateShape(TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ);
      shp.Lock(TGIS_Lock.Projection);
      shp.AddPart;

      if spline.NumControlPoints = 0 then begin
        // draw polyline
        for i := 0 to spline.NumFitPoints-1 do
          shp.AddPoint3D( transformPtg3D( spline.FitPoints[i] ) ) ;

        shp.Smooth( 25, False ) ;
      end
      else begin
        //drawSpline( shp,
        //            spline.ControlPoints, spline.NumControlPoints,
        //            spline.Knots, spline.NumKnots
        //           ) ;
        // new interpolator
        splint := TDWG_BSplineInterpolator.Create ;
        try
          if splint.Prepare( spline.Degree, spline.ControlPoints,
                             spline.Knots, spline.Weights ) then
          begin
            shp.AddPoint3D( transformPtg3D( spline.ControlPoints[0] ) ) ;
            t := 0 ;
            while t < 1 do begin
              ptg := splint.InterpolatePoint( t ) ;
              shp.AddPoint3D( transformPtg3D( ptg ) ) ;
              t := t + 1/25 ;
            end ;
            shp.AddPoint3D( transformPtg3D( spline.ControlPoints[spline.NumControlPoints-1] ) ) ;
          end
          else begin
            // draw polyline
            for i := 0 to spline.NumControlPoints-1 do
              shp.AddPoint3D( transformPtg3D( spline.ControlPoints[i] ) ) ;
          end;
        finally
          FreeObject( splint ) ;
        end ;
      end ;
      shp.Unlock;

      applyStyle(shp, _e);
    end ;


    procedure buildPolyline( const _e : T_FileDwg_Entity );
    var
      pl  : T_FileDwg_Polyline ;
      i   : Integer ;
      attrm     : Integer ;
      attrn     : Integer ;
      n, m      : Integer ;
    begin
      pl := _e as T_FileDwg_Polyline;

      if pl.PType = T_FileDwg_PolylineType.PFace then
        shp := CreateShape( TGIS_ShapeType.MultiPatch, TGIS_DimensionType.XYZ )
      else
        shp := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;

      if (pl.Extrusion.X=0) and (pl.Extrusion.Y=0) and (pl.Extrusion.Z=0) then
        pl.Extrusion.Z := 1 ;

      setTransform( pl.Extrusion ) ;

      shp.Lock( TGIS_Lock.Projection ) ;
        shp.AddPart ;

        if pl.PType = T_FileDwg_PolylineType.P2D then begin
          for i := 0 to pl.NumVertex-1 do
            shp.AddPoint3D( transformPtg3D( pl.Vertices[i].Ptg ) ) ;

          if ( pl.NumVertex > 0 ) and (( pl.Flag and 1 ) = 1) then
            shp.AddPoint3D( transformPtg3D( pl.Vertices[0].Ptg ) ) ;
        end
        else if pl.PType = T_FileDwg_PolylineType.P3D then begin
          for i := 0 to pl.NumVertex-1 do
            shp.AddPoint3D( transformPtg3D( pl.Vertices[i].Ptg ) ) ;

          if ( pl.NumVertex > 0 ) and (( pl.Flag and 1 ) = 1) then
            shp.AddPoint3D( transformPtg3D( pl.Vertices[0].Ptg ) ) ;
        end
        else if pl.PType = T_FileDwg_PolylineType.PFace then begin
          for i := 0 to pl.NumFaces - 1 do begin
            if (pl.Faces[i].VIndex[0] = 0) or (pl.Faces[i].VIndex[1] = 0) then continue ;

              shp.AddPart ;
              if (pl.Faces[i].VIndex[0] <> 0) then
                shp.AddPoint3D( transformPtg3D(pl.Vertices[Abs(pl.Faces[i].VIndex[0])-1].Ptg)) ;
              if (pl.Faces[i].VIndex[1] <> 0) then
                shp.AddPoint3D( transformPtg3D(pl.Vertices[Abs(pl.Faces[i].VIndex[1])-1].Ptg)) ;
              if (pl.Faces[i].VIndex[2] <> 0) then
                shp.AddPoint3D( transformPtg3D(pl.Vertices[Abs(pl.Faces[i].VIndex[2])-1].Ptg)) ;
              if (pl.Faces[i].VIndex[3] <> 0) then
                shp.AddPoint3D( transformPtg3D(pl.Vertices[Abs(pl.Faces[i].VIndex[3])-1].Ptg)) ;
          end
        end
        else if pl.PType = T_FileDwg_PolylineType.PMesh then begin
          if ( pl.Flag and 16 ) = 16 then begin
           if pl.MDensity <> 0 then
             attrm := pl.MDensity
           else
             attrm := pl.MVertexCount ;

            if pl.NDensity <> 0 then
              attrn := pl.NDensity
            else
              attrn := pl.NVertexCount ;

            for m := 0 to attrm-1 do begin
              for n := 0 to attrn-1 do begin
                if (m < (attrm - 1)) then begin
                  shp.AddPoint3D(
                    transformPtg3D( pl.Vertices[ m*attrn + n ].Ptg )
                  ) ;
                  shp.AddPoint3D(
                    transformPtg3D( pl.Vertices[ (m+1)*attrn + n ].Ptg )
                  ) ;
                end
                else begin
                  if ((pl.Flag and 1) = 1) then begin
                    shp.AddPoint3D(
                      transformPtg3D( pl.Vertices[ m*attrn + n ].Ptg )
                    ) ;
                    shp.AddPoint3D(
                      transformPtg3D( pl.Vertices[ n ].Ptg )
                    ) ;
                  end
                end ;

                if (n < (attrn - 1)) then begin
                  shp.AddPoint3D(
                    transformPtg3D( pl.Vertices[ m*attrn + n ].Ptg )
                  ) ;
                  shp.AddPoint3D(
                    transformPtg3D( pl.Vertices[ m*attrn + n+1 ].Ptg )
                  ) ;
                end
                else begin
                  if ((pl.Flag and 32) = 32) then begin
                    shp.AddPoint3D(
                      transformPtg3D( pl.Vertices[ m*attrn + n ].Ptg )
                    ) ;
                    shp.AddPoint3D(
                      transformPtg3D( pl.Vertices[ m * attrn].Ptg )
                    ) ;
                  end
                end ;

              end ;
            end
          end
          else begin
            for i := 0 to pl.NumVertex-1 do
              shp.AddPoint3D( transformPtg3D( pl.Vertices[i].Ptg ) ) ;
          end ;
        end ;

      shp.Unlock ;

      applyStyle(shp, _e);
    end ;

    procedure buildDimension( const _e : T_FileDwg_Entity );
    var
      dim : T_FileDwg_Dimension;
    begin
      dim := _e as T_FileDwg_Dimension;
      setTransform( dim.Extrusion ) ;

      if not assigned( shp ) then
        shp := CreateShape(TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ);

      shp.Lock(TGIS_Lock.Projection);
      shp.AddPart;
        if ( dim.eType = T_FileDwg_EntityType.DimensionOrdinate ) or
           ( dim.eType = T_FileDwg_EntityType.DimensionDiametric ) then begin
          shp.AddPoint3D( transformPtg3D( dim.Ptg2 ) ) ;
          shp.AddPoint3D( transformPtg3D( dim.Ptg1 ) ) ;
        end
        else
          prepareDimension( transformPtg3D( dim.Ptg2 ),
                            transformPtg3D( dim.Ptg1 ),
                            transformPtg3D( dim.Ptg3 ),
                            transformPtg3D( dim.PtgText ),
                            shp
                           ) ;
      shp.Unlock;

      applyStyle(shp, _e);

      if dim.eType = T_FileDwg_EntityType.DimensionDiametric then begin
        shp.Params.Line.Symbol      := SymbolList.Prepare( GIS_DWG_LEADERSTYLE ) ;
        shp.SetField( GIS_DWG_LABEL, parseText( dim.Text ) ) ;
      end
      else if dim.eType <> T_FileDwg_EntityType.DimensionOrdinate then begin
        shp.Params.Line.Symbol      := SymbolList.Prepare( GIS_DWG_DIMSTYLE ) ;
        shp.Params.Labels.Field     := GIS_FIELD_LENGTH ;
        shp.Params.Labels.Alignment := TGIS_LabelAlignment.Follow ;
        shp.Params.Labels.Color     := shp.Params.Line.Color ;
        shp.Params.Labels.FontColor := shp.Params.Line.Color ;
      end ;
    end ;


    procedure buildEntity( const _e : T_FileDwg_Entity ); forward ;

    procedure buildHatch( const _e : T_FileDwg_Entity );
    var
      hatch  : T_FileDwg_Hatch ;
      {$IFDEF DCC}
      hloop  : T_FileDwg_HatchLoop ;
      he     : T_FileDwg_Entity ;
      {$ENDIF}
    begin
      hatch := _e as T_FileDwg_Hatch;

      setTransform( hatch.Extrusion ) ;

      shp := CreateShape(TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ);
      shp.Lock(TGIS_Lock.Projection);

      for hloop in hatch.Loops do begin
        for he in hloop.Entities do begin
          buildEntity( he ) ;
        end;
      end;

      shp.Unlock;

      applyStyle(shp, _e);
    end ;


    procedure buildInsert( const _e : T_FileDwg_Entity );
    var
      ins   : T_FileDwg_Insert ;
      blk   : T_FileDwg_Block ;
      {$IFDEF DCC}
      e     : T_FileDwg_Entity ;
      {$ENDIF}
      oldti : TDWG_Matrix ;
    begin
      oldti := dwgTransform.GetTransformInsert ;
      try
        ins := _e as T_FileDwg_Insert;

        dwgTransform.PrepareMatrixInsert(
          ins.Extrusion,
          ins.Scale,
          ins.Rotation,
          ins.InsertionPoint
        ) ;

        blk := dwgModel.GetBlock( ins.BlockHandle ) ;
        if ( blk <> nil ) then begin
          dwgTransform.TransformInsert( blk.BasePoint, oldti ) ;

          for e in blk.Entities do begin
            buildEntity( e );
            if (e.Style.Color.CType = T_FileDwg_Flag.ByLayer) then begin
              if assigned( e.Layer ) and ( e.Layer.Name = '0' ) then
                applyStyle( shp, ins ) ; // override if e has no style but ins has
            end
            else if (e.Style.Color.CType = T_FileDwg_Flag.ByBlock)  then
              applyStyle( shp, ins ) ;
            shp := nil ;
          end ;
        end
        else
          shp := nil ;

      finally
        dwgTransform.SetTransformInsert( oldti ) ;
      end ;
    end ;

    procedure buildEntity( const _e : T_FileDwg_Entity );
    begin
      case _e.eType of
        T_FileDwg_EntityType.Point:
          buildPoint(_e);
        T_FileDwg_EntityType.Line,
        T_FileDwg_EntityType.XLine,
        T_FileDwg_EntityType.Ray:
          buildLine(_e);
        T_FileDwg_EntityType.Insert:
          buildInsert(_e);
        T_FileDwg_EntityType.Circle:
          buildCircle(_e);
        T_FileDwg_EntityType.Arc:
          buildArc(_e);
        T_FileDwg_EntityType.Ellipse:
          buildEllipse(_e);
        T_FileDwg_EntityType.LWPolyline:
          buildLWPolyline(_e);
        T_FileDwg_EntityType.Text,
        T_FileDwg_EntityType.MText,
        T_FileDwg_EntityType.Attrib,
        T_FileDwg_EntityType.AttDef:
          buildText(_e);
        T_FileDwg_EntityType.Solid,
        T_FileDwg_EntityType.Trace:
          buildSolid(_e);
        T_FileDwg_EntityType.Face3D:
          buildFace3D(_e);
        T_FileDwg_EntityType.Spline:
          buildSpline(_e);
        T_FileDwg_EntityType.Polyline:
          buildPolyline(_e);
        T_FileDwg_EntityType.Hatch:
          buildHatch(_e);
        T_FileDwg_EntityType.Leader:
          buildLeader(_e);
        T_FileDwg_EntityType.DimensionLinear,
        T_FileDwg_EntityType.DimensionAligned,
        T_FileDwg_EntityType.DimensionDiametric,
        T_FileDwg_EntityType.DimensionOrdinate :
          buildDimension(_e);
      end ;

      if assigned( shp ) then begin
        shp.SetField( GIS_DWG_HANDLE, _e.Handle      ) ;
        shp.SetField( GIS_DWG_LAYER , shp.Layer.Name ) ;
      end ;
    end ;

    procedure prepareLayers ;
    var
      {$IFDEF DCC}
      la  : TPair<String, T_FileDwg_Layer> ;
      {$ENDIF}
      lv  : TGIS_LayerSublayerVector ;
      clvwr, cl : TGIS_Color ;
      r,g,b : Byte ;
      vr,vg,vb : Byte ;
    begin
      if not assigned( SubLayers ) then
        SubLayers := TGIS_LayerAbstractList.Create( False ) ;

      for la in dwgModel.Layers do begin
        lv := TGIS_LayerSublayerVector.Create ;

        lv.Name    := la.Value.Name ;
        lv.Caption := la.Value.Name ;
        lv.Active  := la.Value.Visible ;
        lv.Viewer  := Viewer ;

        lv.SupportedShapes := GisGetEmptyShapeType ;
        lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Point ) ;
        lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.MultiPoint ) ;
        lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Arc ) ;
        lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Polygon ) ;

        lv.Params.Assign( self.Params );
        lv.IgnoreShapeParams  := IgnoreShapeParams ;
        lv.UseParentParams    := False ;
        lv.ParentLayer        := self ;
        SubLayers.Add( lv ) ;

        if assigned( Viewer ) then
          clvwr :=  Viewer.Ref.Color
        else
          clvwr := TGIS_Color.Black ;

        cl := la.Value.Style.Color.Color ;
        r := cl.R ;
        g := cl.G ;
        b := cl.B ;

        vr := clvwr.R ;
        vg := clvwr.G ;
        vb := clvwr.B ;

        if ( Abs( vr - r ) < 16 ) and
           ( Abs( vg - g ) < 16 ) and
           ( Abs( vb - b ) < 16 )
        then // color very close to the window color
          cl := TGIS_Color.FromRGB( r xor vr, g xor vg, b xor vb ) ;

        lv.Params.Marker.SmartSize := 0 ;
        lv.Params.Marker.Color      := cl ;
        lv.Params.Marker.Size       := 1;
        lv.Params.Line.Color        := cl;
        lv.Params.Line.Style        := la.Value.Style.Pen.Pen;
        lv.Params.Line.Width        := TruncS(la.Value.Style.Width.Width);
        lv.Params.Area.Color        := cl;
        lv.Params.Area.OutlineColor := cl;
        lv.Params.Area.OutlineStyle := la.Value.Style.Pen.Pen;
        lv.Params.Ground            := TGIS_3DGroundType.AboveZero ;
        lv.Params.Labels.Alignment  := TGIS_LabelAlignment.LeftJustify ;
        lv.Params.Labels.Allocator  := False ;
        lv.Params.Labels.Duplicates := True ;
        lv.Params.Labels.Color      := lv.Params.Marker.Color ;
        lv.Params.Labels.FontColor  := lv.Params.Marker.Color ;
        lv.Params.Labels.Position   := GisGetLabelPosition( TGIS_LabelPosition.UpRight ) ;
        lv.Params.Labels.SmartSize  := 0 ;
        lv.Params.Labels.Field      := GIS_DWG_LABEL ;

        la.Value.NativeLayer := lv ;
      end ;

    end;

    procedure sortSubLayers ;
    begin
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          java.util.Collections.sort( SubLayers, new T_listSortByNameDwg ) ;
        {$ELSE}
          SubLayers.Sort( @dwg_sort_by_name );
        {$ENDIF}
      {$ELSE}
        SubLayers.Sort( TComparer<TGIS_LayerAbstract>.Construct( dwg_sort_by_name ) );
      {$ENDIF}
    end ;

  begin
    dwgModel := TGIS_FileDWG(dwgFile).GetModel;

    prepareLayers ;
    sortSubLayers ;

    // take main model block
    dwgTransform := TDWG_Transformation.Create ;
    try
      dwgModelBlock := dwgModel.GetModelBlock;
      assert( dwgModelBlock <> nil ) ;
      if assigned( dwgModelBlock ) then begin
        shp := nil ;
        bpos := 0 ;
        for dwgEntity in dwgModelBlock.Entities do begin
          if (dwgEntity.Handle >= dwgModel.FirstEntity) and
             (dwgEntity.Handle <= dwgModel.LastEntity) then
          buildEntity( dwgEntity );
          shp := nil ;
          inc( bpos ) ;
          if bpos mod GIS_PROGRESS_TRESHOLD = 0 then
            if assigned( Viewer ) then
              if Viewer.Ref.HourglassShake then
                break ;
        end ;
      end ;
    finally
      FreeObject( dwgTransform ) ;
    end ;

  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerDWG.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-DWG', 'AutoCAD DWG',
                   TGIS_LayerDWG, '.dwg',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                    False
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerDWG.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

