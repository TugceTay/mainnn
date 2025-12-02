unit Lider.CG.Com.StMatrix;

{*********************************************************}
{*                  STMATRIX.PAS 2.00                    *}
{*     Copyright (c) TurboPower Software Co., 1996-98    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFNDEF VER120}
   // Delphi 4.0 only: requires dynamic arrays
{$ENDIF}

{$Q-} {Overflow check}
{$R-} {Range check}
{$S-} {Stack check}
{$T-} {Typed @ check}
{$V-} {Var strings}
{$A-} {Packed records}
{$B-} {Incomplete boolean evaluation}
{$X+} {Extended syntax}

{Notes:
   This unit provides simple implementations of basic
   matrix algebra functions. Algorithms for inversion and
   eigensystems were lifted from Numerical Recipes in C,
   by Press, Teukolsky, Vetterling, and Flannery.

   Generally tradeoffs are made towards safety and usability at
   the expense of memory usage and efficiency. The unit is intended
   for small to medium size matrices instead of huge ones where
   thrashing, access order, and in-place operations would be
   important issues.

   This unit requires Delphi 4.0, whose new dynamic array feature
   makes a general-purpose matrix unit convenient.

   The parameter-passing conventions are:
     const a : TStMatrix
       the matrix is input only and not modified.
     a : TStMatrix
       the matrix must be allocated by the caller but its elements
       are modified by the routine.
     function : TStMatrix or var a : TStMatrix
       the matrix is allocated and initialized by the routine and
       returned to the caller.
}

  {-General-purpose matrix manipulation}

interface

uses
  grids;

const
  {these could eventually be merged into SysTools string resource}
  stscMatrixBadDims = 200;     {invalid combination of matrix dimensions}
  stscMatrixSingular = 201;    {singular matrix}
  stscMatrixBadData = 202;     {data inconsistent with requirements}
  stscMatrixNoConverge = 203;  {no convergence in numerical routine}
  stscMatrixBadInput = 204;    {input line in bad format}

type
  StmFloat = double;

  TStVector = array of StmFloat;

  TStMatrix = array of array of StmFloat;

  TStColSet = set of byte;  {for selecting columns from a matrix}

procedure RaiseMatrixError(Code: longint);
  {-Generate a matrix or vector exception}

{----------------------------------------------------------------------}

function vmake(nrows: integer): TStVector;
  {-Allocate space for a vector of nrows}

function vdim(const v: TStVector): integer;
  {-Return the number of elements in a}

procedure vassign(const v1: TStVector; v2: TStVector);
  {-Assign elements of v1 to v2}

procedure vextract(const a: TStMatrix; col: integer; v: TStVector);
  {-Extract column col from a and store it in v}

procedure vnormalize(const v1: TStVector; v2: TStVector);
  {-Return vector in normal form: centered at mean and scaled by
    the sample standard deviation}

procedure vunitscale(const v1: TStVector; v2: TStVector);
  {-Return vector in unit-scaled form: centered at mean and scaled
    by the corrected sum of squares}

function vdot(const v1, v2: TStVector): StmFloat;
  {-Return the dot product of two vectors}

function vmag(const v: TStVector): StmFloat;
  {-Return the squared magnitude of a vector}

function vsum(const v: TStVector): StmFloat;
  {-Return the sum of the elements of a vector}

{----------------------------------------------------------------------}

procedure mdim(const a: TStMatrix; var nrows, ncols: integer);
  {-Return the number of rows and columns in matrix a}

function mmake(nrows, ncols: integer): TStMatrix;
  {-Allocate space for a matrix of nrows x ncols}

procedure minit(a: TStMatrix; value: double);
  {-fill matrix with a value}

function mcopy(a: TStMatrix): TStMatrix;
  {-Return a full copy of matrix a}

function mfromv(const v: TStVector): TStMatrix;
  {-Return a one-column matrix with contents of vector v}

procedure mcompose(const a: TStMatrix; c: TStMatrix; cols: TStVector);
  {-Compose matrix c from the specified columns of matrix a. Real
    numbers in cols are truncated.}

procedure mselect(const a: TStMatrix; c: TStMatrix; selected: array of boolean);
  {-Compose matrix c from the selected columns of matrix a}

procedure mselectset(const a: TStMatrix; c: TStMatrix; colset: TStColSet);
  {-Compose matrix c from the specified columns of matrix a}

procedure midentity(a: TStMatrix);
  {-Initialize square matrix a as an identity matrix}

procedure massign(const a: TStMatrix; c: TStMatrix);
  {-Assign elements of a to c}

procedure massigncol(v: TStVector; a: TStMatrix; col: integer);
  {-Assign vector v to specified column of matrix a}

procedure massignrow(v: TStVector; a: TStMatrix; row: integer);
  {-Assign vector v to specified row of matrix a}

procedure mmult(const a, b: TStMatrix; c: TStMatrix);
  {-Multiply two matrices}

procedure madd(const a, b: TStMatrix; c: TStMatrix);
  {-Add two matrices}

procedure msub(const a, b: TStMatrix; c: TStMatrix);
  {-Subtract two matrices}

procedure mscale(const a: TStMatrix; k: StmFloat; c: TStMatrix);
  {-Scale a matrix by a scalar}

procedure mtranspose(const a: TStMatrix; c: TStMatrix);
  {-Return the transpose of matrix a in c}

procedure minverse(const a: TStMatrix; c: TStMatrix); overload;

function minverse(const a: TStMatrix): TStMatrix; overload;
  {-Return the inverse of square matrix a in c}

function mtrace(const a: TStMatrix): StmFloat;
  {-Return the trace of square matrix a}

function mdeterm(const a: TStMatrix): StmFloat;
  {-Return the determinant of a square matrix}

function msymmetrical(const a: TStMatrix): boolean;
  {-Return true if a is square and symmetrical}

procedure meigen(const a: TStMatrix; val: TStVector; vec: TStMatrix);
  {-For a square symmetric matrix a, return the eigenvalues and
    eigenvectors. The eigenvalues are sorted in decreasing order in val.
    The vectors are the corresponding columns of vec.}

procedure vwrite(var f: textfile; const v: TStVector; showsize: boolean);
  {-Write v to a text file. First line of file contains nrows if
    showsize is true. Next line contains the entire vector using ASCII
    comma-delimited notation.}

procedure vread(var f: textfile; var v: TStVector);
  {-Allocate and read v from a text file. First line of file
    contains nrows. Next line contains the entire vector. Skips
    blank lines and lines beginning with '#'.}

procedure mwrite(var f: textfile; const a: TStMatrix; showsize: boolean);
  {-Write a to a text file. First line of file contains nrows, ncols if
    showsize is true. Remaining lines contain each row using ASCII
    comma-delimited notation, with one row per line.}

procedure mdisplay(const a: TStMatrix; grid: TStringGrid);
  {-Write a to a StringGrid. ATE 1999}

function mmean(const a: TStMatrix): StmFloat;
  {-Mean value of elements of a. ATE 23.06.1999}

type
  TStMatrixTester = function(const a: TStMatrix; row: integer): boolean;

procedure mwritetest(var f: textfile; const a: TStMatrix; tester: TStMatrixTester);
  {-Write a to a text file. Each row is written only if the given
    testing function returns True. The first line does not include
    the number of rows and columns since the number of rows isn't
    known in advance and it is not considered worth making two passes.}

procedure mread(var f: textfile; var a: TStMatrix);
  {-Allocate and read a from a text file. First line of file
    contains nrows, ncols. Remaining lines contain each row
    of the matrix. Skips blank lines and lines beginning with '#'.}

{==========================================================================}

implementation

uses
  SysUtils;

type
  TStIntVector = array of integer;

procedure RaiseMatrixError(Code: longint);
  {-Generate a matrix exception}
var
  E: Exception;
begin
  case Code of
    stscMatrixBadDims:
      E := Exception.Create('Invalid matrix dimensions');
    stscMatrixSingular:
      E := Exception.Create('Singular matrix');
    stscMatrixBadData:
      E := Exception.Create('Data inconsistent with requirements');
    stscMatrixNoConverge:
      E := Exception.Create('No convergence in numerical routine');
    stscMatrixBadInput:
      E := Exception.Create('Input line in bad format');
  else
    E := Exception.Create('Unknown matrix error');
  end;
  raise E;
end;

{----------------------------------------------------------------------}

function vmake(nrows: integer): TStVector;
begin
  setlength(result, nrows);
end;

function vdim(const v: TStVector): integer;
begin
  result := high(v) + 1;
end;

procedure vassign(const v1: TStVector; v2: TStVector);
var
  i: integer;
begin
  if (high(v1) <= 0) or (high(v1) <> high(v2)) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to high(v1) do
    v2[i] := v1[i];
end;

procedure vextract(const a: TStMatrix; col: integer; v: TStVector);
var
  ra, ca, i: integer;
begin
  mdim(a, ra, ca);
  if (col < 0) or (col >= ca) or (ra > high(v) + 1) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to ra - 1 do
    v[i] := a[i, col];
end;

procedure vnormalize(const v1: TStVector; v2: TStVector);
var
  i: integer;
  sx, mean, stddev: StmFloat;
begin
  if (high(v1) <= 0) or (high(v1) <> high(v2)) then
    RaiseMatrixError(stscMatrixBadDims);

  {compute mean}
  sx := 0;
  for i := 0 to high(v1) do
    sx := sx + v1[i];
  mean := sx / (high(v1) + 1);

  {compute standard deviation}
  sx := 0;
  for i := 0 to high(v1) do
    sx := sx + sqr(v1[i] - mean);
  stddev := sqrt(sx / high(v1));

  {return normalized vector}
  for i := 0 to high(v1) do
    v2[i] := (v1[i] - mean) / stddev;
end;

procedure vunitscale(const v1: TStVector; v2: TStVector);
var
  i: integer;
  x, sx, sxx, mean: StmFloat;
begin
  if (high(v1) <= 0) or (high(v1) <> high(v2)) then
    RaiseMatrixError(stscMatrixBadDims);

  {compute basic sums}
  sx := 0.0;
  sxx := 0.0;
  for i := 0 to high(v1) do
  begin
    x := v1[i];
    sx := sx + x;
    sxx := sxx + sqr(x);
  end;
  mean := sx / (high(v1) + 1);
  sxx := sqrt(sxx - (high(v1) + 1) * sqr(mean));

  {return normalized vector}
  for i := 0 to high(v1) do
    v2[i] := (v1[i] - mean) / sxx;
end;

function vdot(const v1, v2: TStVector): StmFloat;
var
  i: integer;
begin
  if (high(v1) <> high(v2)) then
    RaiseMatrixError(stscMatrixBadDims);
  result := 0.0;
  for i := 0 to high(v1) do
    result := result + v1[i] * v2[i];
end;

function vmag(const v: TStVector): StmFloat;
var
  i: integer;
begin
  result := 0.0;
  for i := 0 to high(v) do
    result := result + sqr(v[i]);
end;

function vsum(const v: TStVector): StmFloat;
var
  i: integer;
begin
  result := 0.0;
  for i := 0 to high(v) do
    result := result + v[i];
end;

{--------------------------------------------------------------------}

procedure mdim(const a: TStMatrix; var nrows, ncols: integer);
begin
  if (a = nil) then
  begin
    nrows := 0;
    ncols := 0;
  end
  else
  begin
    nrows := high(a) + 1;
    if nrows = 0 then
      ncols := 0
    else
      ncols := high(a[0]) + 1;
  end;
end;

function mmake(nrows, ncols: integer): TStMatrix;
begin
  setlength(result, nrows, ncols);
end;

function mcopy(a: TStMatrix): TStMatrix;
var
  ra, ca, i, j: integer;
begin
  mdim(a, ra, ca);
  if (ra = 0) or (ca = 0) then
    result := nil
  else
  begin
    setlength(result, ra, ca);
    for i := 0 to ra - 1 do
      for j := 0 to ca - 1 do
        result[i, j] := a[i, j];
  end;
end;

function mfromv(const v: TStVector): TStMatrix;
begin
  if (high(v) < 0) then
    mfromv := nil
  else
  begin
    result := mmake(high(v) + 1, 1);
    massigncol(v, result, 0);
  end;
end;

procedure mcompose(const a: TStMatrix; c: TStMatrix; cols: TStVector);
var
  ra, rc, ca, cc, i, j, count: integer;
begin
  mdim(a, ra, ca);
  mdim(c, rc, cc);
  if (ra <> rc) then
    RaiseMatrixError(stscMatrixBadDims);

  count := 0;
  for j := 0 to high(cols) do
  begin
    if (trunc(cols[j]) >= ca) or (count >= cc) then
      RaiseMatrixError(stscMatrixBadDims);
    for i := 0 to ra - 1 do
      c[i, count] := a[i, trunc(cols[j])];
    inc(count);
  end;
end;

procedure mselect(const a: TStMatrix; c: TStMatrix; selected: array of boolean);
var
  ra, rc, ca, cc, i, j, count: integer;
begin
  mdim(a, ra, ca);
  mdim(c, rc, cc);
  if (ra <> rc) or (high(selected) + 1 <> ca) then
    RaiseMatrixError(stscMatrixBadDims);

  count := 0;
  for j := 0 to ca - 1 do
    if (selected[j]) then
    begin
      if (count >= cc) then
        RaiseMatrixError(stscMatrixBadDims);
      for i := 0 to ra - 1 do
        c[i, count] := a[i, j];
      inc(count);
    end;
end;

procedure mselectset(const a: TStMatrix; c: TStMatrix; colset: TStColSet);
var
  ra, rc, ca, cc, i, j, count: integer;
begin
  mdim(a, ra, ca);
  mdim(c, rc, cc);
  if (ra <> rc) or (cc > ca) then
    RaiseMatrixError(stscMatrixBadDims);

  count := 0;
  for j := 0 to ca - 1 do
    if (j in colset) then
    begin
      if (count >= cc) then
        RaiseMatrixError(stscMatrixBadDims);
      for i := 0 to ra - 1 do
        c[i, count] := a[i, j];
      inc(count);
    end;
end;

procedure mmult(const a, b: TStMatrix; c: TStMatrix);
var
  ra, rb, rc, ca, cb, cc, i, j, k: integer;
  sum: extended;
begin
  mdim(a, ra, ca);
  mdim(b, rb, cb);
  mdim(c, rc, cc);
  if (ca <> rb) or (ra <> rc) or (cb <> cc) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to rc - 1 do
    for j := 0 to cc - 1 do
    begin
      sum := 0.0;
      for k := 0 to ca - 1 do
        sum := sum + (a[i, k] * b[k, j]);
      c[i, j] := sum;
    end;
end;

procedure madd(const a, b: TStMatrix; c: TStMatrix);
var
  ra, rb, rc, ca, cb, cc, i, j: integer;
begin
  mdim(a, ra, ca);
  mdim(b, rb, cb);
  mdim(c, rc, cc);
  if (ra <> rb) or (ra <> rc) or (ca <> cb) or (ca <> cc) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to rc - 1 do
    for j := 0 to cc - 1 do
      c[i, j] := a[i, j] + b[i, j];
end;

procedure msub(const a, b: TStMatrix; c: TStMatrix);
var
  ra, rb, rc, ca, cb, cc, i, j: integer;
begin
  mdim(a, ra, ca);
  mdim(b, rb, cb);
  mdim(c, rc, cc);
  if (ra <> rb) or (ra <> rc) or (ca <> cb) or (ca <> cc) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to rc - 1 do
    for j := 0 to cc - 1 do
      c[i, j] := a[i, j] - b[i, j];
end;

procedure mscale(const a: TStMatrix; k: StmFloat; c: TStMatrix);
var
  ra, rc, ca, cc, i, j: integer;
begin
  mdim(a, ra, ca);
  mdim(c, rc, cc);
  if (ra <> rc) or (ca <> cc) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to rc - 1 do
    for j := 0 to cc - 1 do
      c[i, j] := k * a[i, j];
end;

procedure mtranspose(const a: TStMatrix; c: TStMatrix);
var
  ra, rc, ca, cc, i, j: integer;
begin
  mdim(a, ra, ca);
  mdim(c, rc, cc);
  if (ra <> cc) or (ca <> rc) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to rc - 1 do
    for j := 0 to cc - 1 do
      c[i, j] := a[j, i];
end;

procedure ludcmp(a: TStMatrix; var indx: TStIntVector; var d: StmFloat);
  {-Replaces a with the LU decomposition of a rowwise permutation of itself.
    indx is an output vector that records the row permutation effected by
    the partial pivoting. d is +1 if the number of row interchanges was even;
    otherwise -1. a should have been previously confirmed as square.}
const
  TINY = 1.0e-100; {reciprocals easily fit in StmFloat range of 1e324}
var
  i, imax, j, k, n: integer;
  big, dum, sum, temp: StmFloat;
  vv: TStVector;
begin
  {top index of a}
  n := high(a);

  {get space for indx}
  setlength(indx, n + 1);

  {allocate a vector to hold the implicit scaling of each row}
  vv := vmake(n + 1);

  {no row interchanges yet}
  d := 1.0;

  {loop over rows to get the implicit scaling information}
  for i := 0 to n do
  begin
    big := 0.0;
    for j := 0 to n do
    begin
      temp := abs(a[i, j]);
      if (temp > big) then
        big := temp;
    end;
    if (big = 0.0) then
      RaiseMatrixError(stscMatrixSingular);
    vv[i] := 1.0 / big;
  end;

  {loop over columns of Crout's method}
  for j := 0 to n do
  begin
    for i := 0 to j - 1 do
    begin
      sum := a[i, j];
      for k := 0 to i - 1 do
        sum := sum - a[i, k] * a[k, j];
      a[i, j] := sum;
    end;

    {initialize search for largest pivot element}
    imax := 0;
    big := 0.0;
    for i := j to n do
    begin
      sum := a[i, j];
      for k := 0 to j - 1 do
        sum := sum - a[i, k] * a[k, j];
      a[i, j] := sum;
      dum := vv[i] * abs(sum);
      if (dum >= big) then
      begin
        big := dum;
        imax := i;
      end;
    end;

    {do we need to interchange rows?}
    if (j <> imax) then
    begin
      {yes, do so}
      for k := 0 to n do
      begin
        dum := a[imax, k];
        a[imax, k] := a[j, k];
        a[j, k] := dum;
      end;
      {change parity of d}
      d := -d;
      {interchange scale factor}
      vv[imax] := vv[j];
    end;

    indx[j] := imax;
    {if the pivot element is zero the matrix is singular or nearly so}
    if (a[j, j] = 0.0) then
      a[j, j] := TINY;

    {finally divide by the pivot element}
    if (j <> n) then
    begin
      dum := 1.0 / a[j, j];
      for i := j + 1 to n do
        a[i, j] := a[i, j] * dum;
    end;
  end; {for j}

  vv := nil;
end;

procedure lubksb(const a: TStMatrix; const indx: TStIntVector; b: TStVector);
  {-Solves the set of linear equations ax=b.
    a is the input matrix already LU-decomposed by ludcmp.
    indx is the permutation vector returned by ludcmp.
    b is the right-hand-side vector b and returns the solution vector x.}
var
  i, ii, ip, j, n: integer;
  sum: StmFloat;
begin
  {top index of a}
  n := high(a);

  ii := -1;

  {perform forward substitution, unscrambling the permutation of rows}
  for i := 0 to n do
  begin
    ip := indx[i];
    sum := b[ip];
    b[ip] := b[i];
    if (ii >= 0) then
    begin
      for j := ii to i - 1 do
        sum := sum - a[i, j] * b[j];
    end
    else if (sum <> 0.0) then
      {non-zero element encountered, start doing sums above}
      ii := i;
    b[i] := sum;
  end;

  {perform backsubstitution}
  for i := n downto 0 do
  begin
    sum := b[i];
    for j := i + 1 to n do
      sum := sum - a[i, j] * b[j];
    {store component of solution}
    b[i] := sum / a[i, i];
  end;
end;

procedure mprove(const a, alud: TStMatrix; indx: TStIntVector; const b:
  TStVector; x: TStVector);
  {-Improve solution x by iterative method}
var
  i, j, n: integer;
  r: TStVector;
  sdp: extended;
begin
  n := high(a);
  r := vmake(n + 1);

  {improve solution to full StmFloat precision}
  for i := 0 to n do
  begin
    sdp := -b[i];
    {accumulate residual in extended precision}
    for j := 0 to n do
      sdp := sdp + a[i, j] * x[j];
    r[i] := sdp;
  end;

  {solve for the error term}
  lubksb(alud, indx, r);

  {correct old solution}
  for i := 0 to n do
    x[i] := x[i] - r[i];

  r := nil;
end;

procedure minverse(const a: TStMatrix; c: TStMatrix);
var
  ra, rc, ca, cc, i, j: integer;
  indx: TStIntVector;
  col, b: TStVector;
  alud: TStMatrix;
  d: StmFloat;
begin
  mdim(a, ra, ca);
  mdim(c, rc, cc);
  if (ra <> ca) or (ra <> rc) or (ra <> cc) or (ra <= 0) then
    RaiseMatrixError(stscMatrixBadDims);

  {copy a}
  alud := mcopy(a);

  {LU decompose the matrix}
  ludcmp(alud, indx, d);

  {get temporary storage}
  col := vmake(ra);
  b := vmake(ra);

  {find inverse by columns}
  for j := 0 to ca - 1 do
  begin
    {initialize result of linear equations; identity matrix in this case}
    for i := 0 to ra - 1 do
    begin
      col[i] := 0.0;
      b[i] := 0.0;
    end;
    col[j] := 1.0;
    b[j] := 1.0;

    {solve linear equations}
    lubksb(alud, indx, col);

    {improve solution using residuals}
    mprove(a, alud, indx, b, col);

    {and store corrected solution}
    for i := 0 to ra - 1 do
      c[i, j] := col[i];
  end;

  b := nil;
  alud := nil;
  indx := nil;
  col := nil;
end;

function minverse(const a: TStMatrix): TStMatrix;
var
  ra, rc, ca, cc, i, j: integer;
  indx: TStIntVector;
  col, b: TStVector;
  alud: TStMatrix;
  d: StmFloat;
begin
  mdim(a, ra, ca);

  result := mmake(ra, ca);

  mdim(result, rc, cc);
  if (ra <> ca) or (ra <> rc) or (ra <> cc) or (ra <= 0) then
    RaiseMatrixError(stscMatrixBadDims);

  {copy a}
  alud := mcopy(a);

  {LU decompose the matrix}
  ludcmp(alud, indx, d);

  {get temporary storage}
  col := vmake(ra);
  b := vmake(ra);

  {find inverse by columns}
  for j := 0 to ca - 1 do
  begin
    {initialize result of linear equations; identity matrix in this case}
    for i := 0 to ra - 1 do
    begin
      col[i] := 0.0;
      b[i] := 0.0;
    end;
    col[j] := 1.0;
    b[j] := 1.0;

    {solve linear equations}
    lubksb(alud, indx, col);

    {improve solution using residuals}
    mprove(a, alud, indx, b, col);

    {and store corrected solution}
    for i := 0 to ra - 1 do
      result[i, j] := col[i];
  end;

  b := nil;
  alud := nil;
  indx := nil;
  col := nil;
end;

procedure minit(a: TStMatrix; value: double);
var
  ra, ca, i, j: integer;
begin
  mdim(a, ra, ca);
  for i := 0 to ra - 1 do
  begin
    for j := 0 to ca - 1 do
      a[i, j] := value;
  end;
end;

procedure midentity(a: TStMatrix);
var
  ra, ca, i, j: integer;
begin
  mdim(a, ra, ca);
  if (ra <> ca) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to ra - 1 do
  begin
    for j := 0 to ca - 1 do
      a[i, j] := 0.0;
    a[i, i] := 1.0;
  end;
end;

procedure massign(const a: TStMatrix; c: TStMatrix);
var
  ra, rc, ca, cc, i, j: integer;
begin
  mdim(a, ra, ca);
  mdim(c, rc, cc);
  if (ra <> rc) or (ca <> cc) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to rc - 1 do
    for j := 0 to cc - 1 do
      c[i, j] := a[i, j];
end;

procedure massigncol(v: TStVector; a: TStMatrix; col: integer);
var
  ra, ca, i: integer;
begin
  mdim(a, ra, ca);
  if (high(v) + 1 <> ra) or (col < 0) or (col >= ca) then
    RaiseMatrixError(stscMatrixBadDims);

  for i := 0 to ra - 1 do
    a[i, col] := v[i];
end;

procedure massignrow(v: TStVector; a: TStMatrix; row: integer);
var
  ra, ca, j: integer;
begin
  mdim(a, ra, ca);
  if (high(v) + 1 <> ca) or (row < 0) or (row >= ra) then
    RaiseMatrixError(stscMatrixBadDims);

  for j := 0 to ca - 1 do
    a[row, j] := v[j];
end;

function mtrace(const a: TStMatrix): StmFloat;
var
  ra, ca, i: integer;
begin
  mdim(a, ra, ca);
  if (ra <> ca) then
    RaiseMatrixError(stscMatrixBadDims);

  result := 0.0;
  for i := 0 to ra - 1 do
    result := result + a[i, i];
end;

function mdeterm(const a: TStMatrix): StmFloat;
var
  ra, ca, i: integer;
  alud: TStMatrix;
  indx: TStIntVector;
begin
  result := 0.0;
  mdim(a, ra, ca);
  if (ra <> ca) or (ra <= 0) then
    RaiseMatrixError(stscMatrixBadDims);

  alud := mcopy(a);
  ludcmp(alud, indx, result);
  for i := 0 to ra - 1 do
    result := result * alud[i, i];
  indx := nil;
  alud := nil;
end;

function msymmetrical(const a: TStMatrix): boolean;
var
  ra, ca, i, j: integer;
begin
  result := false;
  mdim(a, ra, ca);
  if (ra <> ca) or (ra <= 0) then
    exit;

  for i := 0 to ra - 1 do
    for j := i + 1 to ca - 1 do
      if (a[i, j] <> a[j, i]) then
        exit;
  result := true;
end;

procedure meigen(const a: TStMatrix; val: TStVector; vec: TStMatrix);
const
  MAXIT = 200;
  EPS = 1.0e-15;
var
  ra, rv, ca, cv, i, ii, j, k, iq, ip: integer;
  tresh, theta, tau, t, sm, s, h, g, c: StmFloat;
  b, z: TStVector;
  acopy: TStMatrix;
begin
  {confirm a is square and symmetric}
  if (not msymmetrical(a)) then
    RaiseMatrixError(stscMatrixBadData);

  mdim(a, ra, ca);
  mdim(vec, rv, cv);
  if (ra <> rv) or (ra <> cv) or (ra <> high(val) + 1) then
    RaiseMatrixError(stscMatrixBadDims);

  acopy := mcopy(a);
  b := vmake(ra);
  z := vmake(ra);

  {initialize to identity matrix}
  for i := 0 to ra - 1 do
  begin
    for j := 0 to ca - 1 do
      vec[i, j] := 0.0;
    vec[i, i] := 1.0;
  end;

  {initialize b and d to the diagonal of a}
  {z will accumulate terms of form t*a[pq]}
  for i := 0 to ra - 1 do
  begin
    b[i] := acopy[i, i];
    val[i] := b[i];
    z[i] := 0.0;
  end;

  {Jacobi rotations}
  for i := 1 to MAXIT do
  begin
    {sum off-diagonal elements}
    sm := 0.0;
    for ip := 0 to ra - 2 do
    begin
      for iq := ip + 1 to ra - 1 do
        sm := sm + abs(acopy[ip, iq]);
    end;

    if (sm < EPS) then
    begin
      {the normal return, which relies on quadratic convergence}
      b := nil;
      z := nil;

      {sort the eigenvalues in descending order and rearrange vec}
      for ii := 0 to ra - 2 do
      begin
        s := val[ii];
        k := ii;
        for j := ii + 1 to ra - 1 do
          if (val[j] >= s) then
          begin
            s := val[j];
            k := j;
          end;
        if (k <> ii) then
        begin
          val[k] := val[ii];
          val[ii] := s;
          for j := 0 to ra - 1 do
          begin
            s := vec[j, ii];
            vec[j, ii] := vec[j, k];
            vec[j, k] := s;
          end;
        end;
      end;

      exit;
    end;

    if (i < 4) then
      tresh := 0.2 * sm / (ra * ra)
    else
      tresh := 0.0;

    for ip := 0 to ra - 2 do
    begin
      for iq := ip + 1 to ra - 1 do
      begin
        g := 100.0 * abs(acopy[ip, iq]);
        {after 4 sweeps skip the rotation if the off-diagonal element is small}
        if (i > 4) and (g < EPS * abs(val[ip])) and (g < EPS * abs(val[iq])) then
          acopy[ip, iq] := 0.0
        else if (abs(acopy[ip, iq]) > tresh) then
        begin
          h := val[iq] - val[ip];
          if (g < EPS * abs(h)) then
            t := acopy[ip, iq] / h
          else
          begin
            theta := 0.5 * h / acopy[ip, iq];
            t := 1.0 / (abs(theta) + sqrt(1.0 + theta * theta));
            if (theta < 0.0) then
              t := -t;
          end;
          c := 1.0 / sqrt(1.0 + t * t);
          s := t * c;
          tau := s / (1.0 + c);
          h := t * acopy[ip, iq];
          z[ip] := z[ip] - h;
          z[iq] := z[iq] + h;
          val[ip] := val[ip] - h;
          val[iq] := val[iq] + h;

          acopy[ip, iq] := 0.0;

          {rotations 0 <= j < p}
          for j := 0 to ip - 1 do
          begin
            g := acopy[j, ip];
            h := acopy[j, iq];
            acopy[j, ip] := g - s * (h + g * tau);
            acopy[j, iq] := h + s * (g - h * tau);
          end;
          {rotations p < j < q}
          for j := ip + 1 to iq - 1 do
          begin
            g := acopy[ip, j];
            h := acopy[j, iq];
            acopy[ip, j] := g - s * (h + g * tau);
            acopy[j, iq] := h + s * (g - h * tau);
          end;
          {rotations q < j <= n}
          for j := iq + 1 to ra - 1 do
          begin
            g := acopy[ip, j];
            h := acopy[iq, j];
            acopy[ip, j] := g - s * (h + g * tau);
            acopy[iq, j] := h + s * (g - h * tau);
          end;

          {update eigenvectors}
          for j := 0 to ra - 1 do
          begin
            g := vec[j, ip];
            h := vec[j, iq];
            vec[j, ip] := g - s * (h + g * tau);
            vec[j, iq] := h + s * (g - h * tau);
          end;
        end;
      end;
    end;

    {update d with the sum of t*a[pq] and reinitialize z}
    for ip := 0 to ra - 1 do
    begin
      b[ip] := b[ip] + z[ip];
      val[ip] := b[ip];
      z[ip] := 0.0;
    end;
  end;

  acopy := nil;
  b := nil;
  z := nil;
  RaiseMatrixError(stscMatrixNoConverge);
end;

procedure vwrite(var f: textfile; const v: TStVector; showsize: boolean);
var
  i: integer;
begin
  if (showsize) then
    {write the number of rows in the vector}
    writeln(f, high(v) + 1);
  for i := 0 to high(v) - 1 do
    write(f, FloatToStr(v[i]), ',');
  writeln(f, FloatToStr(v[high(v)]));
end;

procedure readskip(var f: textfile; var s: string);
  {-Read lines until non-blank starting with non '#' is found}
begin
  repeat
    readln(f, s);
  until (length(s) > 0) and (s[1] <> '#');
end;

procedure vread(var f: textfile; var v: TStVector);
var
  nrows, code, n, p, r, t: integer;
  inst: string;
begin
  readskip(f, inst);
  val(inst, nrows, code);
  if (code <> 0) then
    RaiseMatrixError(stscMatrixBadInput);

  v := vmake(nrows);

  readskip(f, inst);
  p := 1;
  for r := 0 to nrows - 1 do
  begin
    {skip leading space}
    while (p <= length(inst)) and (inst[p] <= ' ') do
      inc(p);

    {skip to next comma or end of line}
    n := p;
    while (n <= length(inst)) and (inst[n] <> ',') do
      inc(n);

    {skip trailing space}
    t := n;
    while (t > 1) and (inst[t - 1] <= ' ') do
      dec(t);

    {convert substring to number}
    if (t = p) then
      {treat empty elements as zeros}
      v[r] := 0.0
    else
    begin
      val(copy(inst, p, t - p), v[r], code);
      if (code <> 0) then
        RaiseMatrixError(stscMatrixBadInput);
    end;

    {on to next element}
    p := n + 1;

    if (p > length(inst)) and (r < nrows - 1) then
    begin
      {get another line}
      readskip(f, inst);
      p := 1;
    end;
  end;
end;

procedure mwrite(var f: textfile; const a: TStMatrix; showsize: boolean);
var
  ra, ca, i, j: integer;
begin
  mdim(a, ra, ca);
  if (showsize) then
    {write the number of rows and cols in the matrix}
    writeln(f, ra, ',', ca);

  for i := 0 to ra - 1 do
  begin
    for j := 0 to ca - 2 do
      write(f, FloatToStr(a[i, j]), ',');
    writeln(f, FloatToStr(a[i, ca - 1]));
  end;
end;

procedure mdisplay(const a: TStMatrix; grid: TStringGrid); //ATE 1999
var
  ra, ca, i, j: integer;
begin
  mdim(a, ra, ca);

  grid.RowCount := ra;
  grid.ColCount := ca;

  for i := 0 to ca - 1 do
    for j := 0 to ra - 1 do
    begin
      grid.cells[i, j] := ' '; //clear the cell
      grid.Cells[i, j] := format('%10.5f', [a[j, i]]);
    end;
end;

function mmean(const a: TStMatrix): StmFloat; //ATE 23.06.1999
var
  nElement, x, y, i, j: integer;
  total: StmFloat;
begin
  total := 0;
  mdim(a, x, y);
  for i := 0 to x - 1 do
    for j := 0 to y - 1 do
      total := total + a[i, j];

  mmean := total / (x * y);
end;

procedure mwritetest(var f: textfile; const a: TStMatrix; tester: TStMatrixTester);
var
  ra, ca, i, j: integer;
begin
  mdim(a, ra, ca);

  for i := 0 to ra - 1 do
    if tester(a, i) then
    begin
      for j := 0 to ca - 2 do
        write(f, FloatToStr(a[i, j]), ',');
      writeln(f, FloatToStr(a[i, ca - 1]));
    end;
end;

procedure mread(var f: textfile; var a: TStMatrix);
var
  nrows, ncols, code, n, p, r, t, c: integer;
  inst: string;
begin
  readskip(f, inst);
  p := pos(',', inst);
  if (p = 0) then
    RaiseMatrixError(stscMatrixBadInput);
  val(trim(copy(inst, 1, p - 1)), nrows, code);
  if (code <> 0) then
    RaiseMatrixError(stscMatrixBadInput);
  val(trim(copy(inst, p + 1, length(inst))), ncols, code);
  if (code <> 0) then
    RaiseMatrixError(stscMatrixBadInput);

  a := mmake(nrows, ncols);

  for r := 0 to nrows - 1 do
  begin
    {get next row of matrix}
    readskip(f, inst);

    p := 1;
    for c := 0 to ncols - 1 do
    begin
      {skip leading space}
      while (p <= length(inst)) and (inst[p] <= ' ') do
        inc(p);

      {skip to next comma or end of line}
      n := p;
      while (n <= length(inst)) and (inst[n] <> ',') do
        inc(n);

      {skip trailing space}
      t := n;
      while (t > 1) and (inst[t - 1] <= ' ') do
        dec(t);

      {convert substring to number}
      if (t = p) then
        {treat empty elements as zeros}
        a[r, c] := 0.0
      else
      begin
        val(copy(inst, p, t - p), a[r, c], code);
        if (code <> 0) then
          RaiseMatrixError(stscMatrixBadInput);
      end;

      {make sure all elements specified}
      if (n > length(inst)) and (c < ncols - 1) then
        RaiseMatrixError(stscMatrixBadInput);

      {on to next element}
      p := n + 1;
    end;
  end;
end;

end.


