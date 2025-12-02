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
  Matrix operations.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoMatrix ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoMatrix"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
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

  /// <summary>
  ///   Vector of double precision real numbers.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_Vector = array of Double ;
  {$ELSE}
    {#typehint:array:Double}
    TGIS_Vector = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer read flength ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : Double read fvalue write fvalue;
     end ;
  {$ENDIF}


  /// <summary>
  ///   Matrix of double precision real numbers.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_Matrix = array of TGIS_Vector ;
  {$ELSE}
    {#typehint:array:TGIS_Vector}
    TGIS_Matrix = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_rows">
        ///   number of rows
        /// </param>
        /// <param name="_columns">
        ///   number of columns
        /// </param>
        procedure SetSize( const _rows    : Integer ;
                           const _columns : Integer
                         ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Rows : Integer read flength ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Columns : Integer read flength ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_col">
        ///   column index
        /// </param>
        /// <param name="_col">
        ///   raw index
        /// </param>
        property Value[ const _row : Integer;
                        const _col : Integer
                      ] : Double read fvalue write fvalue;
    end ;
  {$ENDIF}


  /// <summary>
  ///   Square matrix of double precision real numbers.
  /// </summary>
  TGIS_SquareMatrix = class
    private
      elements  : TGIS_Matrix ;
      pivotList : TObject ;
    private
      FDimension : Integer ;
    private
      function  fget_Element    ( const _row : Integer ;
                                  const _col : Integer
                                ) : Double ;
      procedure fset_Element    ( const _row : Integer ;
                                  const _col : Integer ;
                                  const _val : Double
                                ) ;
      function  fget_Row        ( const _row : Integer
                                ) : TGIS_Vector ;
      procedure fset_Row        ( const _row : Integer ;
                                  const _vec : TGIS_Vector
                                ) ;
    private
      /// <summary>
      ///   Prepares the pivoting sequence used to make the LU decomposition
      ///   stable.
      /// </summary>
      procedure preparePivoting ;
      /// <summary>
      ///   Applies the pivoting to a specified matrix.
      /// </summary>
      /// <param name="_mtr">
      ///   the matrix to perform pivoting on
      /// </param>
      procedure applyPivoting   ( const _mtr : TGIS_SquareMatrix
                                ) ; overload;
      /// <summary>
      ///   Applies the pivoting to a specified vector.
      /// </summary>
      /// <param name="_vec">
      ///   the vector to perform pivoting on
      /// </param>
      procedure applyPivoting   ( const _vec : TGIS_Vector
                                ) ; overload;
      /// <summary>
      ///   Solves a system of linear equations for the "lower" matrix obtained
      ///   by LU decomposition.
      /// </summary>
      /// <param name="_lwr">
      ///   the "lower" LU decomposition matrix
      /// </param>
      /// <param name="_vec">
      ///   the known vector
      /// </param>
      function  solveLower      ( const _lwr : TGIS_SquareMatrix ;
                                  const _vec : TGIS_Vector
                                ) : TGIS_Vector ;
      /// <summary>
      ///   Solves a system of linear equations for the "upper" matrix obtained
      ///   by LU decomposition.
      /// </summary>
      /// <param name="_upr">
      ///   the "upper" LU decomposition matrix
      /// </param>
      /// <param name="_vec">
      ///   the known vector
      /// </param>
      function  solveUpper      ( const _upr : TGIS_SquareMatrix ;
                                  const _vec : TGIS_Vector
                                ) : TGIS_Vector ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create        ; overload;
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_dim">
      ///   the dimension of the matrix
      /// </param>
      constructor Create        ( const _dim : Integer
                                ) ; overload;
      /// <summary>
      ///   Creates an instance. If the source matrix is not square then it
      ///   will be truncated by the smaller dimension.
      /// </summary>
      /// <param name="_mtr">
      ///   the source data for matrix
      /// </param>
      constructor Create        ( const _mtr : TGIS_Matrix
                                ) ; overload;

    {$IFNDEF MANAGED}
      public
        /// <summary>
        ///   Destroys the instance.
        /// </summary>
        destructor Destroy      ; override;
    {$ENDIF}

    public
      /// <summary>
      ///   Clones the matrix.
      /// </summary>
      /// <returns>
      ///   The exact copy of the object.
      /// </returns>
      function  Clone           : TGIS_SquareMatrix ;
      /// <summary>
      ///   Checks if the supplied matrix is equal to the current matrix.
      /// </summary>
      /// <param name="_mtr">
      ///   the matrix to be checked
      /// </param>
      /// <returns>
      ///   True if the matrices are equal.
      /// </returns>
      function  Equal           ( const _mtr : TGIS_SquareMatrix
                                ) : Boolean ;
      /// <summary>
      ///   Makes the identity matrix.
      /// </summary>
      procedure MakeIdentity    ;
      /// <summary>
      ///   Makes the 2D rotation matrix.
      /// </summary>
      /// <param name="_angle">
      ///   the angle of rotation
      /// </param>
      /// <param name="_cwise">
      ///   True for clockwise (Y to X); False for counterclockwise
      /// </param>
      procedure Make2DRotation  ( const _angle : Double  ;
                                  const _cwise : Boolean
                                ) ;
      /// <summary>
      ///   Makes the 3D rotation matrix around the X axis; increases dimension
      ///   to 3 if smaller, does not change dimension if larger than 3.
      /// </summary>
      /// <param name="_angle">
      ///   the angle of rotation
      /// </param>
      /// <param name="_cwise">
      ///   True for clockwise (Z to Y); False for counterclockwise
      /// </param>
      procedure Make3DRotationX ( const _angle : Double  ;
                                  const _cwise : Boolean
                                ) ;
      /// <summary>
      ///   Makes the 3D rotation matrix around the Y axis; increases dimension
      ///   to 3 if smaller, does not change dimension if larger than 3.
      /// </summary>
      /// <param name="_angle">
      ///   the angle of rotation
      /// </param>
      /// <param name="_cwise">
      ///   True for clockwise (X to Z); False for counterclockwise
      /// </param>
      procedure Make3DRotationY ( const _angle : Double  ;
                                  const _cwise : Boolean
                                ) ;
      /// <summary>
      ///   Makes a 3D rotation matrix around the Z axis; increases dimension
      ///   to 3 if smaller, does not change dimension if larger than 3.
      /// </summary>
      /// <param name="_angle">
      ///   the angle of rotation
      /// </param>
      /// <param name="_cwise">
      ///   True for clockwise (Y to X); False for counterclockwise
      /// </param>
      procedure Make3DRotationZ ( const _angle : Double  ;
                                  const _cwise : Boolean
                                ) ;
      /// <summary>
      ///   Adds translation to a 4D rotation-translation matrix; increases
      ///   dimension to 4 if smaller, does not change dimension if larger
      ///   than 4.
      /// </summary>
      /// <param name="_trans">
      ///   translation vector; must be 3D or larger
      /// </param>
      procedure Add3DTranslation( const _trans : TGIS_Vector
                                ) ; overload;
      /// <summary>
      ///   Adds translation to a 4D rotation-translation matrix; increases
      ///   dimension to 4 if smaller, does not change dimension if larger
      ///   than 4.
      /// </summary>
      /// <param name="_dx">
      ///   translation along the X axis
      /// </param>
      /// <param name="_dy">
      ///   translation along the Y axis
      /// </param>
      /// <param name="_dz">
      ///   translation along the Z axis
      /// </param>
      procedure Add3DTranslation( const _dx : Double ;
                                  const _dy : Double ;
                                  const _dz : Double
                                ) ; overload;
      /// <summary>
      ///   Computes the cofactor (minor) of the specified matrix element.
      /// </summary>
      /// <param name="_row">
      ///   the row index
      /// </param>
      /// <param name="_col">
      ///   the column index
      /// </param>
      /// <returns>
      ///   The cofactor of the matrix element.
      /// </returns>
      function  Cofactor        ( const _row : Integer ;
                                  const _col : Integer
                                ) : Double ;
      /// <summary>
      ///   Computes the determinant of the matrix.
      /// </summary>
      /// <returns>
      ///   The determinant of the matrix.
      /// </returns>
      function  Determinant     : Double ;
      /// <summary>
      ///   Transposes the matrix.
      /// </summary>
      procedure Transpose       ;
      /// <summary>
      ///   Computes the LU (Crout) decomposition of the matrix. Does not
      ///   perform pivoting.
      /// </summary>
      /// <param name="_lower">
      ///   returns the "lower" matrix
      /// </param>
      /// <param name="_upper">
      ///   returns the "upper" matrix
      /// </param>
      /// <returns>
      ///   True if the LU decomposition can be performed.
      /// </returns>
      function  LUDecomposition (   out _lower : TGIS_SquareMatrix ;
                                    out _upper : TGIS_SquareMatrix
                                ) : Boolean ;
      /// <summary>
      ///   Inverts the matrix.
      /// </summary>
      /// <returns>
      ///   True if the inversion can be performed.
      /// </returns>
      function  Invert          : Boolean ;
      /// <summary>
      ///   Multiplies the supplied matrix by the current matrix.
      /// </summary>
      /// <param name="_mtr">
      ///   the matrix to be multiplied
      /// </param>
      /// <returns>
      ///   The result of multiplication.
      /// </returns>
      function  Multiply        ( const _mtr : TGIS_SquareMatrix
                                ) : TGIS_SquareMatrix ; overload;
      /// <summary>
      ///   Multiplies the supplied vector by the current matrix.
      /// </summary>
      /// <param name="_vec">
      ///   the vector to be multiplied
      /// </param>
      /// <returns>
      ///   The result of multiplication.
      /// </returns>
      function  Multiply        ( const _vec : TGIS_Vector
                                ) : TGIS_Vector ; overload;
      /// <summary>
      ///   Multiplies the matrix by a double precision real number.
      /// </summary>
      /// <param name="_val">
      ///   the value to multiply by
      /// </param>
      procedure MultiplyBy      ( const _val : Double
                                ) ;
      /// <summary>
      ///   Solves a system of linear equations of the form Matrix * unknown
      ///   vector = known vector.
      /// </summary>
      /// <param name="_vec">
      ///   the known vector
      /// </param>
      /// <returns>
      ///   The solution of the linear problem.
      /// </returns>
      function  Solve           ( const _vec : TGIS_Vector
                                ) : TGIS_Vector ;
    public
      /// <summary>
      ///   The dimension (number of rows/columns) of the matrix.
      /// </summary>
      property Dimension : Integer
                           read  FDimension ;
      /// <summary>
      ///   The value of the matrix element.
      /// </summary>
      /// <param name="_row">
      ///   the row index
      /// </param>
      /// <param name="_col">
      ///   the column index
      /// </param>
      property Element[const _row : Integer ; const _col : Integer] : Double
                           read  fget_Element
                           write fset_Element ; default ;
      /// <summary>
      ///   Gives access to the whole matrix row.
      /// </summary>
      /// <param name="_row">
      ///   the row index
      /// </param>
      property Row[const _row : Integer] : TGIS_Vector
                           read  fget_Row
                           write fset_Row ;
  end ;


  /// <summary>
  ///   Dot (inner, scalar) product of two vectors.
  /// </summary>
  /// <param name="_v1">
  ///   the first vector
  /// </param>
  /// <param name="_v2">
  ///   the second vector
  /// </param>
  /// <returns>
  ///   The result of dot product.
  /// </returns>
  function GisDotProduct( const _v1 : TGIS_Vector ;
                          const _v2 : TGIS_Vector
                        ) : Double ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.Generics.Collections,
    Lider.CG.GIS.GeoRtl ;
{$ENDIF}

type

  T_rowExchange = record
    Row1 : Integer ;
    Row2 : Integer ;
  end ;


  T_rowExchangeList = TList<T_rowExchange> ;


//==============================================================================
// global methods
//==============================================================================

  function GisDotProduct(
    const _v1 : TGIS_Vector ;
    const _v2 : TGIS_Vector
  ) : Double ;
  var
    dim : Integer ;
    res : Double ;
    i   : Integer ;
  begin
    dim := length( _v1 ) ;

    if dim <> length( _v2 ) then begin
      Result := 0 ;
      exit ;
    end ;

    res := 0 ;
    for i := 0 to dim - 1 do
      res := res + _v1[i]*_v2[i] ;

    Result := res ;
  end ;


//==============================================================================
// TGIS_SquareMatrix
//==============================================================================

  constructor TGIS_SquareMatrix.Create ;
  begin
    inherited ;

    FDimension := 2 ;

    SetLength( elements, Dimension, Dimension ) ;
  end ;


  constructor TGIS_SquareMatrix.Create(
    const _dim : Integer
  ) ;
  begin
    inherited Create ;

    if _dim < 2 then
      FDimension := 2
    else
      FDimension := _dim ;

    SetLength( elements, Dimension, Dimension ) ;
  end ;


  constructor TGIS_SquareMatrix.Create(
    const _mtr : TGIS_Matrix
  ) ;
  var
    i : Integer ;
  begin
    inherited Create ;

    FDimension := Min( length( _mtr ), length( _mtr[0] ) ) ;
    SetLength( elements, Dimension, Dimension ) ;
    for i := 0 to Dimension - 1 do
      elements[i] := TGIS_Vector( Copy( _mtr[i], 0, Dimension ) ) ;
  end ;


  {$IFNDEF MANAGED}
    destructor TGIS_SquareMatrix.Destroy ;
    begin

      inherited Destroy ;
    end ;
  {$ENDIF}


  function TGIS_SquareMatrix.fget_Element(
    const _row : Integer ;
    const _col : Integer
  ) : Double ;
  begin
    Result := elements[_row,_col] ;
  end ;


  procedure TGIS_SquareMatrix.fset_Element(
    const _row : Integer ;
    const _col : Integer ;
    const _val : Double
  ) ;
  begin
    elements[_row,_col] := _val ;
  end ;


  function TGIS_SquareMatrix.fget_Row(
    const _row : Integer
  ) : TGIS_Vector ;
  begin
    Result := elements[_row] ;
  end ;


  procedure TGIS_SquareMatrix.fset_Row(
    const _row : Integer ;
    const _vec : TGIS_Vector
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to Dimension - 1 do
      elements[_row,i] := _vec[i] ;
  end ;


  procedure TGIS_SquareMatrix.preparePivoting ;
  var
    lst : T_rowExchangeList ;
    val : Double ;
    r   : Integer ;
    ex  : T_rowExchange ;
    i   : Integer ;
    k   : Integer ;
  begin
    lst := T_rowExchangeList.Create ;

    for i := 0 to Dimension - 1 do begin
      val := 0.0 ;
      r   := i ;
      if elements[i,i] = 0.0 then begin
        for k := 0 to Dimension - 1 do begin
          if Abs( Element[k,i] ) > val then begin
            val := Abs( Element[k,i] ) ;
            r := k ;
          end ;
        end ;
        ex.Row1 := i ;
        ex.Row2 := r ;
        lst.Add( ex ) ;
      end ;
    end ;

    pivotList := lst ;
  end ;


  procedure TGIS_SquareMatrix.applyPivoting(
    const _mtr : TGIS_SquareMatrix
  ) ;
  var
    lst : T_rowExchangeList ;
    r   : TGIS_Vector ;
    i   : Integer ;
  begin
    lst := T_rowExchangeList( pivotList ) ;

    for i := 0 to lst.Count - 1 do begin
      r := TGIS_Vector( Copy( _mtr.Row[lst[i].Row1], 0 , Dimension ) ) ;
      _mtr.Row[lst[i].Row1] := _mtr.Row[lst[i].Row2] ;
      _mtr.Row[lst[i].Row2] := r ;
    end ;
  end ;


  procedure TGIS_SquareMatrix.applyPivoting(
    const _vec : TGIS_Vector
  ) ;
  var
    lst : T_rowExchangeList ;
    r   : Double ;
    i   : Integer ;
  begin
    lst := T_rowExchangeList( pivotList ) ;

    for i := 0 to lst.Count - 1 do begin
      r := _vec[lst[i].Row1] ;
      _vec[lst[i].Row1] := _vec[lst[i].Row2] ;
      _vec[lst[i].Row2] := r ;
    end ;
  end ;


  function TGIS_SquareMatrix.solveLower(
    const _lwr : TGIS_SquareMatrix ;
    const _vec : TGIS_Vector
  ) : TGIS_Vector ;
  var
    res : TGIS_Vector ;
    sum : Double ;
    i   : Integer ;
    k   : Integer ;
  begin
    SetLength( res, Dimension ) ;

    for i := 0 to Dimension - 1 do begin
      sum := 0 ;
      for k := 0 to i - 1 do
        sum := sum + _lwr[i,k]*res[k] ;
      res[i] := ( _vec[i] - sum )/_lwr[i,i] ;
    end ;

    Result := res ;
  end ;


  function TGIS_SquareMatrix.solveUpper(
    const _upr : TGIS_SquareMatrix ;
    const _vec : TGIS_Vector
  ) : TGIS_Vector ;
  var
    res : TGIS_Vector ;
    sum : Double ;
    i   : Integer ;
    k   : Integer ;
  begin
    SetLength( res, Dimension ) ;

    for i := Dimension - 1 downto 0 do begin
      sum := 0 ;
      for k := Dimension - 1 downto i + 1 do
        sum := sum + _upr[i,k]*res[k] ;
      res[i] := ( _vec[i] - sum )/_upr[i,i] ;
    end ;

    Result := res ;
  end ;


  function TGIS_SquareMatrix.Clone : TGIS_SquareMatrix ;
  begin
    Result := TGIS_SquareMatrix.Create( elements ) ;
  end ;


  function TGIS_SquareMatrix.Equal(
    const _mtr : TGIS_SquareMatrix
  ) : Boolean ;
  var
    d : Double ;
    i : Integer ;
    k : Integer ;
  begin
    Result := False ;

    if Dimension <> _mtr.Dimension then
      exit ;

    for i := 0 to Dimension - 1 do begin
      for k := 0 to Dimension - 1 do begin
        d := elements[i,k] - _mtr[i,k] ;
        if d > 1e-15 then
          exit ;
      end ;
    end ;

    Result := True ;
  end ;


  procedure TGIS_SquareMatrix.MakeIdentity ;
  var
    i : Integer ;
    k : Integer ;
  begin
    for i := 0 to Dimension - 1 do begin
      for k := 0 to Dimension - 1 do begin
        if i = k then
          elements[i,k] := 1.0
        else
          elements[i,k] := 0.0 ;
      end ;
    end ;
  end ;


  procedure TGIS_SquareMatrix.Make2DRotation(
    const _angle : Double  ;
    const _cwise : Boolean
  ) ;
  var
    vsin : Double ;
    vcos : Double ;
    rot  : Double ;
  begin
    if Dimension < 2 then begin
      FDimension := 2 ;
      SetLength( elements, Dimension, Dimension ) ;
    end ;

    if _cwise then
      rot :=  1.0
    else
      rot := -1.0 ;

    SinCos( _angle, vsin, vcos ) ;
    if vsin < 1e-14 then
      vsin := 0.0 ;
    if vcos < 1e-14 then
      vcos := 0.0 ;

    elements[0,0] := vcos ;
    elements[0,1] := rot*vsin ;
    elements[1,0] := -rot*vsin ;
    elements[1,1] := vcos ;
  end ;


  procedure TGIS_SquareMatrix.Make3DRotationX(
    const _angle : Double  ;
    const _cwise : Boolean
  ) ;
  var
    vsin : Double ;
    vcos : Double ;
    rot  : Double ;
  begin
    if Dimension < 3 then begin
      FDimension := 3 ;
      SetLength( elements, Dimension, Dimension ) ;
    end ;

    if _cwise then
      rot :=  1.0
    else
      rot := -1.0 ;

    SinCos( _angle, vsin, vcos ) ;
    if vsin < 1e-14 then
      vsin := 0.0 ;
    if vcos < 1e-14 then
      vcos := 0.0 ;

    elements[0,0] := 1.0 ;
    elements[0,1] := 0.0 ;
    elements[0,2] := 0.0 ;
    elements[1,0] := 0.0 ;
    elements[1,1] := vcos ;
    elements[1,2] := rot*vsin ;
    elements[2,0] := 0.0 ;
    elements[2,1] := -rot*vsin ;
    elements[2,2] := vcos ;
  end ;


  procedure TGIS_SquareMatrix.Make3DRotationY(
    const _angle : Double  ;
    const _cwise : Boolean
  ) ;
  var
    vsin : Double ;
    vcos : Double ;
    rot  : Double ;
  begin
    if Dimension < 3 then begin
      FDimension := 3 ;
      SetLength( elements, Dimension, Dimension ) ;
    end ;

    if _cwise then
      rot :=  1.0
    else
      rot := -1.0 ;

    SinCos( _angle, vsin, vcos ) ;
    if vsin < 1e-14 then
      vsin := 0.0 ;
    if vcos < 1e-14 then
      vcos := 0.0 ;

    elements[0,0] := vcos ;
    elements[0,1] := 0.0 ;
    elements[0,2] := -rot*vsin ;
    elements[1,0] := 0.0 ;
    elements[1,1] := 1.0 ;
    elements[1,2] := 0.0 ;
    elements[2,0] := rot*vsin ;
    elements[2,1] := 0.0 ;
    elements[2,2] := vcos ;
  end ;


  procedure TGIS_SquareMatrix.Make3DRotationZ(
    const _angle : Double  ;
    const _cwise : Boolean
  ) ;
  var
    vsin : Double ;
    vcos : Double ;
    rot  : Double ;
  begin
    if Dimension < 3 then begin
      FDimension := 3 ;
      SetLength( elements, Dimension, Dimension ) ;
    end ;

    if _cwise then
      rot :=  1.0
    else
      rot := -1.0 ;

    SinCos( _angle, vsin, vcos ) ;
    if vsin < 1e-14 then
      vsin := 0.0 ;
    if vcos < 1e-14 then
      vcos := 0.0 ;

    elements[0,0] := vcos ;
    elements[0,1] := rot*vsin ;
    elements[0,2] := 0.0 ;
    elements[1,0] := -rot*vsin ;
    elements[1,1] := vcos ;
    elements[1,2] := 0.0 ;
    elements[2,0] := 0.0 ;
    elements[2,1] := 0.0 ;
    elements[2,2] := 1.0 ;
  end ;


  procedure TGIS_SquareMatrix.Add3DTranslation(
    const _trans : TGIS_Vector
  ) ;
  begin
    if length( _trans ) < 3 then
      Add3DTranslation( 0.0, 0.0, 0.0 )
    else
      Add3DTranslation( _trans[0], _trans[1], _trans[2] ) ;
  end ;


  procedure TGIS_SquareMatrix.Add3DTranslation(
    const _dx : Double ;
    const _dy : Double ;
    const _dz : Double
  ) ;
  begin
    if Dimension < 4 then begin
      FDimension := 4 ;
      SetLength( elements, Dimension, Dimension ) ;
    end ;

    elements[0,3] := _dx ;
    elements[1,3] := _dy ;
    elements[2,3] := _dz ;
    elements[3,0] := 0.0 ;
    elements[3,1] := 0.0 ;
    elements[3,2] := 0.0 ;
    elements[3,3] := 1.0 ;
  end ;


  function TGIS_SquareMatrix.Cofactor(
    const _row : Integer ;
    const _col : Integer
  ) : Double ;
  var
    mtr : TGIS_SquareMatrix ;
    dim : Integer ;
    det : Double ;
    r   : Integer ;
    c   : Integer ;
    rs  : Integer ;
    cs  : Integer ;
  begin
    dim := Dimension - 1 ;
    mtr := TGIS_SquareMatrix.Create( dim ) ;
    try
      rs := 0 ;
      for r := 0 to dim - 1 do begin
        if r = _row then
          rs := 1 ;
        cs := 0 ;
        for c := 0 to dim - 1 do begin
          if c = _col then
            cs := 1 ;
          mtr[r,c] := elements[r+rs,c+cs] ;
        end ;
      end ;
      det := mtr.Determinant ;
    finally
      FreeObject( mtr ) ;
    end ;

    Result := Power( -1, ( _row + _col ) mod 2 )*det ;
  end ;


  function TGIS_SquareMatrix.Determinant : Double ;
  var
    lwr  : TGIS_SquareMatrix ;
    upr  : TGIS_SquareMatrix ;
    dlwr : Double ;
    dupr : Double ;
    pexp : Integer ;
    dim  : Integer ;
    i    : Integer ;
  begin
    Result := 0.0 ;

    preparePivoting ;
    applyPivoting( Self ) ;

    if not LUDecomposition( lwr, upr ) then begin
      applyPivoting( Self ) ;

      FreeObject( pivotList ) ;
      FreeObject( lwr ) ;
      FreeObject( upr ) ;

      exit ;
    end ;

    pexp := T_rowExchangeList( pivotList ).Count ;

    dim := Dimension - 1 ;
    dlwr := lwr[0,0] ;
    dupr := upr[0,0] ;
    for i := 1 to dim do begin
      dlwr := dlwr*lwr[i,i] ;
      dupr := dupr*upr[i,i] ;
    end ;

    applyPivoting( Self ) ;
    FreeObject( pivotList ) ;
    FreeObject( lwr ) ;
    FreeObject( upr ) ;

    Result := Power( -1, pexp )*dlwr*dupr ;
  end ;


  procedure TGIS_SquareMatrix.Transpose ;
  var
    tmp : Double ;
    dim : Integer ;
    i   : Integer ;
    k   : Integer ;
  begin
    dim := Dimension - 1 ;

    for i := 0 to dim do begin
      for k := i + 1 to dim do begin
        tmp := elements[i,k] ;
        elements[i,k] := elements[k,i] ;
        elements[k,i] := tmp ;
      end ;
    end ;
  end ;


  function TGIS_SquareMatrix.LUDecomposition(
    out _lower : TGIS_SquareMatrix ;
    out _upper : TGIS_SquareMatrix
  ) : Boolean ;
  var
    dim : Integer ;
    sum : Double ;
    i   : Integer ;
    j   : Integer ;
    k   : Integer ;
  begin
    Result := False ;

    _lower := TGIS_SquareMatrix.Create( Dimension ) ;
    _upper := TGIS_SquareMatrix.Create( Dimension ) ;

    dim := Dimension - 1 ;

    for i := 0 to dim do
      _upper[i,i] := 1.0 ;

    for j := 0 to dim do begin
      for i := j to dim do begin
        sum := 0.0 ;
        for k := 0 to j - 1 do
          sum := sum + _lower[i,k]*_upper[k,j] ;
        _lower[i,j] := elements[i,j] - sum ;

        if ( i = j ) and ( _lower[i,j] = 0 ) then begin
          FreeObject( _lower ) ;
          FreeObject( _upper ) ;
          exit ;
        end ;
      end ;

      for i := j to dim do begin
        sum := 0.0 ;
        for k := 0 to j - 1 do
          sum := sum + _lower[j,k]*_upper[k,i] ;
        _upper[j,i] := ( elements[j,i] - sum )/_lower[j,j] ;
      end ;
    end ;

    Result := True ;
  end ;


  function TGIS_SquareMatrix.Invert : Boolean ;
  var
    lwr : TGIS_SquareMatrix ;
    upr : TGIS_SquareMatrix ;
    idt : TGIS_SquareMatrix ;
    tmp : TGIS_SquareMatrix ;
    dim : Integer ;
    i   : Integer ;
  begin
    Result := False ;

    preparePivoting ;
    applyPivoting( Self ) ;

    if not LUDecomposition( lwr, upr ) then begin
      applyPivoting( Self ) ;

      FreeObject( pivotList ) ;
      FreeObject( lwr ) ;
      FreeObject( upr ) ;

      exit ;
    end ;


    dim := Dimension - 1 ;

    idt := TGIS_SquareMatrix.Create( Dimension ) ;
    idt.MakeIdentity ;
    applyPivoting( idt ) ;
    idt.Transpose ;

    tmp := TGIS_SquareMatrix.Create( Dimension ) ;
    try
      for i := 0 to dim do
        tmp.Row[i] := solveLower( lwr, idt.Row[i] ) ;
      for i := 0 to dim do
        Row[i] := solveUpper( upr, tmp.Row[i] ) ;
    finally
      FreeObject( pivotList ) ;
      FreeObject( idt ) ;
      FreeObject( lwr ) ;
      FreeObject( upr ) ;
      FreeObject( tmp ) ;
    end ;

    Transpose ;

    Result := True ;
  end ;


  function TGIS_SquareMatrix.Multiply(
    const _mtr : TGIS_SquareMatrix
  ) : TGIS_SquareMatrix ;
  var
    res : TGIS_SquareMatrix ;
    dim : Integer ;
    i   : Integer ;
    k   : Integer ;
    l   : Integer ;
  begin
    if Dimension <> _mtr.Dimension then begin
      Result := nil ;
      exit ;
    end ;

    dim := Dimension - 1 ;

    res := TGIS_SquareMatrix.Create( Dimension ) ;
    for i := 0 to dim do begin
      for k := 0 to dim do begin
        res[i,k] := 0.0 ;
        for l := 0 to dim do
          res[i,k] := res[i,k] + elements[i,l]*_mtr[l,k] ;
        if Abs( res[i,k] ) < 1e-15 then
          res[i,k] := 0.0 ;
      end ;
    end ;

    Result := res ;
  end ;


  function TGIS_SquareMatrix.Multiply(
    const _vec : TGIS_Vector
  ) : TGIS_Vector ;
  var
    res : TGIS_Vector ;
    dim : Integer ;
    i   : Integer ;
    k   : Integer ;
  begin
    if Dimension <> length( _vec ) then begin
      Result := nil ;
      exit ;
    end ;

    dim := Dimension - 1 ;

    SetLength( res, Dimension ) ;
    for i := 0 to dim do begin
      res[i] := 0.0 ;
      for k := 0 to dim do
        res[i] := res[i] + elements[i,k]*_vec[k] ;
      if Abs( res[i] ) < 1e-15 then
        res[i] := 0.0 ;
    end ;

    Result := res ;
  end ;


  procedure TGIS_SquareMatrix.MultiplyBy(
    const _val : Double
  ) ;
  var
    i : Integer ;
    k : Integer ;
  begin
    for i := 0 to Dimension - 1 do begin
      for k := 0 to Dimension - 1 do begin
        elements[i,k] := _val*elements[i,k] ;
      end ;
    end ;
  end ;


  function TGIS_SquareMatrix.Solve(
    const _vec : TGIS_Vector
  ) : TGIS_Vector ;
  var
    lwr : TGIS_SquareMatrix ;
    upr : TGIS_SquareMatrix ;
    vec : TGIS_Vector ;
    tmp : TGIS_Vector ;
    res : TGIS_Vector ;
  begin
    Result := nil ;

    if Dimension <> length( _vec ) then
      exit ;

    preparePivoting ;
    applyPivoting( Self ) ;

    if not LUDecomposition( lwr, upr ) then begin
      applyPivoting( Self ) ;

      FreeObject( pivotList ) ;
      FreeObject( lwr ) ;
      FreeObject( upr ) ;

      exit ;
    end ;

    SetLength( vec, Dimension ) ;
    vec := TGIS_Vector( Copy( _vec, 0, Dimension ) ) ;
    applyPivoting( vec ) ;

    SetLength( tmp, Dimension ) ;
    SetLength( res, Dimension ) ;
    try
      tmp := solveLower( lwr, vec ) ;
      res := solveUpper( upr, tmp ) ;
    finally
      FreeObject( pivotList ) ;
      FreeObject( lwr ) ;
      FreeObject( upr ) ;
    end ;

    Result := res ;
  end ;


//==================================== END =====================================
end.
