unit Lider.CG.Com.Math;

interface

uses
  Classes,
  SysUtils,
  Math,
  IniFiles,
  Lider.CG.Com.Lib,
  Lider.CG.Com.StringLi,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

const
  MaxTRANS_PNT_COUNT = 40;

type
  PointArray = array[0..PRED(MaxTRANS_PNT_COUNT)] of TlicgCoor3D;

  PPointArray = ^PointArray;

  ErrorArray = array[0..PRED(MaxTRANS_PNT_COUNT)] of Double;

  PErrorArray = ^ErrorArray;

function CalcMatrix(var AMatrix: TlicgMatrix; const pr1, pr2, pr1N, pr2N: TlicgCoor): Boolean;

function CalcAffineMatrix(N: longint; var pr, prN: pointarray; var Ea:
  ErrorArray; var AMatrix: TlicgMatrix; var m: Double): boolean; overload;

procedure CalcHelmertMatrix(N: integer; var pr, prN: pointarray; var Ea: ErrorArray; var AMatrix: TlicgMatrix; var m: Double);

function AffineTransPnt(const AMatrix: TlicgMatrix; const p: TlicgCoor3D): TlicgCoor3D; overload;

function AffineTransPnt(const AMatrix: TlicgMatrix; const p: TlicgCoor): TlicgCoor; overload;

function AsFloat(const opt: string): Double;

function AsFloatDef(const AValue: string; const ADefaultValue: Double): Double; // ilker ekleme

function SamePoint(const c1, c2: TlicgCoor4D): Boolean; overload; // nc
//function IsSamePoint(const p1, p2: TlicgCoor4D): Boolean; ilker silme gerek yok

function IsSamePoint2D(const p1, p2: TlicgCoor; APERTURE: Double = 0.0): Boolean; overload;

function GetCoor(const yatay, dusey: double; const a, d: Double; const aType: TlicgAngleType): TlicgCoor; overload;

function GetCoor(const yatay, dusey: double; const Delta_y, Delta_d: Double): TlicgCoor; overload;

procedure GetCoor(const yatay, dusey: double; const a, d: Double; const aType: TlicgAngleType; var outX, outY: double); overload;

/// <summary>
///   Semt Açýsýný RADYAN olarak hesaplar.
/// </summary>
function Angle2DS(const P1, P2: TlicgCoor): Double; overload;

/// <summary>
///   Semt Açýsýný RADYAN olarak hesaplar.
/// </summary>
function Angle2DS(const P1X, P1Y, P2X, P2Y: Double): Double; overload;

function IsMidPointInsideEntity(const Ent: IlicgEntity; p1, p2: TlicgCoor): Boolean;

procedure SortIntersectPoints(_P1: TlicgCoor; __intV: IlicgVector);

function MeterToDeg(m: double): double;

function DegToMeter(d: double): double;

function GetAffineMatrixFromDRE(DreFn: string): TlicgMatrix; overload;

function GetAffineMatrixFromDRE(DreFn: string; var pikselW, pikselH: integer): TlicgMatrix; overload;

function IsPointOnLine(const C, A, B: TlicgCoor; const ASegmentEpsilon: Double = 0.05): Boolean;
function CalcLineSide(PFirstC: TlicgCoor; PSecondC: TlicgCoor; PCurrCoor: TlicgCoor): Double;
procedure Rotate_Around(Origin: TlicgCoor; var C: TLicgcoor; Angle:Double);
function Polar(Origin: TlicgCoor; Angle, Dist: Double): TLicgCoor;
procedure INVside_shoot(const cside, ca, cb: TlicgCoor; var side_length,
  side_tail: double; const AEpsilon: Double =_EPSILON_LOW);
function Side_Shoot(c1: TlicgCoor; c2: TlicgCoor; sidelength: Double; sidetail: Double): TlicgCoor;
procedure FindPointLine(sPoints: IlicgVector; P: TlicgCoor; var FI, SI: Integer);
function Intersect(Vect1, Vect2, ResultIntersectVector: IlicgVector): Boolean;
function VectorAngle(PVec1, PVec2: IlicgVector): Double;
/// <summary>
///   Referans olarak verilen <b><u>RefP</u></b> noktasýnýn <b><u>P1</u></b>
///   ve <b><u>P2</u></b> noktasý arasýndaki hatta olan dikey uzaklýðýný
///   hesaplayýp geri döndürür.
/// </summary>
function CalcVerticalDistance(P1, P2, RefP: TLicgCoor): Double;

implementation

function CalcMatrix(var AMatrix: TlicgMatrix; const pr1, pr2, pr1N, pr2N: TlicgCoor): boolean;
var
  det: Double;
  x12h, y12h, x12, y12: double;
begin
  calcMatrix := false;

  AMatrix := IDENTITY_MATRIX2D;

  x12h := pr1N.y - pr2N.y;
  y12h := pr1N.x - pr2N.x;
  x12 := pr1.y - pr2.y;
  y12 := pr1.x - pr2.x;
  det := sqr(x12) + sqr(y12);
  if not _NotZero(det) then
    exit;

  AMatrix.Matrix[1, 1] := (x12 * x12h + y12 * y12h) / det;
  AMatrix.Matrix[1, 0] := -(y12 * x12h - x12 * y12h) / det;
  AMatrix.Matrix[0, 1] := -AMatrix.Matrix[1, 0];
  AMatrix.Matrix[0, 0] := AMatrix.Matrix[1, 1];
  AMatrix.Matrix[1, 2] := pr1N.y - (AMatrix.Matrix[1, 1] * pr1.y + AMatrix.Matrix[1, 0] * pr1.x);
  AMatrix.Matrix[0, 2] := pr1N.x - (-AMatrix.Matrix[1, 0] * pr1.y + AMatrix.Matrix[1, 1] * pr1.x);

  calcMatrix := true;
end;

function AffineTransPnt(const AMatrix: TlicgMatrix; const p: TlicgCoor3D): TlicgCoor3D;
begin
  result := _NULL_COOR_3D;
            // b2*x + b1*y + cy  -> b2=a b1=b a2=d a1=e
            // a2*x + a1*y + cx
  Result.x := AMatrix.Matrix[0, 0] * p.x + AMatrix.Matrix[0, 1] * p.y + AMatrix.Matrix[0, 2];
  Result.y := AMatrix.Matrix[1, 0] * p.x + AMatrix.Matrix[1, 1] * p.y + AMatrix.Matrix[1, 2];
  Result.Z := p.Z;
end;

function AffineTransPnt(const AMatrix: TlicgMatrix; const p: TlicgCoor): TlicgCoor;
begin
  result := _NULL_COOR;
  Result.x := AMatrix.Matrix[0, 0] * p.x + AMatrix.Matrix[0, 1] * p.y + AMatrix.Matrix[0, 2];
  Result.y := AMatrix.Matrix[1, 0] * p.x + AMatrix.Matrix[1, 1] * p.y + AMatrix.Matrix[1, 2];
end;

procedure CalcHelmertMatrix(N: integer; var pr, prN: pointarray; var Ea:
  ErrorArray; var AMatrix: TlicgMatrix; var m: DOUBLE);
var
  rp: integer;
  vx, vy, xX, yY, xY, yX, _xx, _yy: DOUBLE;
  cen, cenN: TlicgCoor;
  c1, c2: TlicgCoor3D;
begin

  AMatrix := IDENTITY_MATRIX2D;

  cen := _NULL_COOR;
  cenN := _NULL_COOR;

  if n = 0 then
    exit;

  for rp := 0 to N - 1 do
  begin
    cen.x := cen.x + pr[rp].x;
    cen.y := cen.y + pr[rp].y;
    cenN.x := cenN.x + prN[rp].x;
    cenN.y := cenN.y + prN[rp].y;
  end;
  cen.x := cen.x / N;
  cen.y := cen.y / N;
  cenN.x := cenN.x / N;
  cenN.y := cenN.y / N;
  for rp := 0 to N - 1 do
  begin
    pr[rp].x := pr[rp].x - cen.x;
    pr[rp].y := pr[rp].y - cen.y;
    prN[rp].x := prN[rp].x - cenN.x;
    prN[rp].y := prN[rp].y - cenN.y;
  end;
  xX := 0.0;
  yY := 0.0;
  xY := 0.0;
  yX := 0.0;
  _xx := 0.0;
  _yy := 0.0;
  for rp := 0 to N - 1 do
  begin
    _xx := _xx + pr[rp].y * pr[rp].y;
    _yy := _yy + pr[rp].x * pr[rp].x;
    xX := xX + pr[rp].y * prN[rp].y;
    xY := xY + pr[rp].y * prN[rp].x;
    yY := yY + pr[rp].x * prN[rp].x;
    yX := yX + pr[rp].x * prN[rp].y;
  end;
  for rp := 0 to N - 1 do
  begin
    pr[rp].x := pr[rp].x + cen.x;
    pr[rp].y := pr[rp].y + cen.y;
    prN[rp].x := prN[rp].x + cenN.x;
    prN[rp].y := prN[rp].y + cenN.y;
  end;

  AMatrix.Matrix[1, 1] := (xX + yY) / (_xx + _yy);
  AMatrix.Matrix[1, 0] := -(xY - yX) / (_xx + _yy);
  AMatrix.Matrix[0, 2] := 0.0;
  AMatrix.Matrix[1, 2] := 0.0;
  for rp := 0 to N - 1 do
  begin
    AMatrix.Matrix[1, 2] := AMatrix.Matrix[1, 2] + prN[rp].y - AMatrix.Matrix[1, 1] * pr[rp].y
      - AMatrix.Matrix[1, 0] * pr[rp].x;
    AMatrix.Matrix[0, 2] := AMatrix.Matrix[0, 2] + prN[rp].x - AMatrix.Matrix[1, 1] * pr[rp].x
      + AMatrix.Matrix[1, 0] * pr[rp].y;
  end;
  AMatrix.Matrix[0, 1] := -AMatrix.Matrix[1, 0];
  AMatrix.Matrix[0, 0] := AMatrix.Matrix[1, 1];
  AMatrix.Matrix[1, 2] := AMatrix.Matrix[1, 2] / N;
  AMatrix.Matrix[0, 2] := AMatrix.Matrix[0, 2] / N;

  m := 0;
  for rp := 0 to N - 1 do
  begin
    c1 := prN[rp];
    c2 := pr[rp];
    c2 := AffineTransPnt(AMatrix, c2);
    Ea[rp] := _distance(point2D(c1.x, c1.y), Point2D(c2.x, c2.Y));
    vx := (c1.y - c2.y);
    vy := (c1.x - c2.x);
    m := m + (sqr(vx) + sqr(vy));
  end;
  if N = 2 then
    m := 0
  else
    m := sqrt(m / (2 * N - 4));
end;

function CalcAffineMatrix(N: longint; var pr, prN: pointarray; var Ea:
  ErrorArray; var AMatrix: TlicgMatrix; var m: DOUBLE): boolean;
var
  rp: longint;
  vx, vy, xxyy_xyxy, _xy, xX, yY, xY, yX, _xx, _yy: DOUBLE;
  cen, cenN: TlicgCoor;
  c1, c2: TlicgCoor3D;
begin

  AMatrix := IDENTITY_MATRIX2D;

  if n >= 3 then
  begin
    cen := _NULL_COOR;
    cenN := _NULL_COOR;
    for rp := 0 to N - 1 do
    begin
      cen.x := cen.x + pr[rp].x;
      cen.y := cen.y + pr[rp].y;
      cenN.x := cenN.x + prN[rp].x;
      cenN.y := cenN.y + prN[rp].y;
    end;
    cen.x := cen.x / N;
    cen.y := cen.y / N;
    cenN.x := cenN.x / N;
    cenN.y := cenN.y / N;
    for rp := 0 to N - 1 do
    begin
      pr[rp].x := pr[rp].x - cen.x;
      pr[rp].y := pr[rp].y - cen.y;
      prN[rp].x := prN[rp].x - cenN.x;
      prN[rp].y := prN[rp].y - cenN.y;
    end;
    _xy := 0.0;
    xX := 0.0;
    yY := 0.0;
    xY := 0.0;
    yX := 0.0;
    _xx := 0.0;
    _yy := 0.0;
    for rp := 0 to N - 1 do
    begin
      _xy := _xy + pr[rp].y * pr[rp].x;
      _xx := _xx + pr[rp].y * pr[rp].y;
      _yy := _yy + pr[rp].x * pr[rp].x;
      xX := xX + pr[rp].y * prN[rp].y;
      xY := xY + pr[rp].y * prN[rp].x;
      yY := yY + pr[rp].x * prN[rp].x;
      yX := yX + pr[rp].x * PrN[rp].y;
    end;
    for rp := 0 to N - 1 do
    begin
      pr[rp].x := pr[rp].x + cen.x;
      pr[rp].y := pr[rp].y + cen.y;
      prN[rp].x := prN[rp].x + cenN.x;
      prN[rp].y := prN[rp].y + cenN.y;
    end;

    xxyy_xyxy := (_xx * _yy - sqr(_xy));
    AMatrix.Matrix[0, 2] := 0.0;
    AMatrix.Matrix[1, 2] := 0.0;
    if _NotZero(xxyy_xyxy) then
    begin
      AMatrix.Matrix[1, 1] := (_yy * xX - _xy * yX) / xxyy_xyxy;
      AMatrix.Matrix[0, 1] := (_yy * xY - _xy * yY) / xxyy_xyxy;
      AMatrix.Matrix[1, 0] := (_xx * yX - _xy * xX) / xxyy_xyxy;
      AMatrix.Matrix[0, 0] := (_xx * yY - _xy * xY) / xxyy_xyxy;
      for rp := 0 to N - 1 do
      begin
        AMatrix.Matrix[1, 2] := AMatrix.Matrix[1, 2] + prN[rp].y - AMatrix.Matrix[1, 1] * pr
          [rp].y - AMatrix.Matrix[1, 0] * pr[rp].x;
        AMatrix.Matrix[0, 2] := AMatrix.Matrix[0, 2] + prN[rp].x - AMatrix.Matrix[0, 1] * pr
          [rp].y - AMatrix.Matrix[0, 0] * pr[rp].x;
      end;
      AMatrix.Matrix[1, 2] := AMatrix.Matrix[1, 2] / N;
      AMatrix.Matrix[0, 2] := AMatrix.Matrix[0, 2] / N;
    end
    else
    begin
      AMatrix.Matrix[1, 1] := 1;
      AMatrix.Matrix[1, 0] := 0;
      AMatrix.Matrix[0, 1] := 0;
      AMatrix.Matrix[0, 0] := 1;
    end;

    m := 0;
    for rp := 0 to N - 1 do
    begin
      c1.x := prN[rp].x;
      c1.y := prN[rp].y;
      c2.x := pr[rp].x;
      c2.y := pr[rp].y;
      c2 := AffineTransPnt(AMatrix, c2);

      Ea[rp] := _distance(point2d(c1.x, c1.y), point2D(c2.x, c2.Y));
      vx := (c1.y - c2.y);
      vy := (c1.x - c2.x);
      m := m + (sqr(vx) + sqr(vy));
    end;
    if N = 3 then
      m := 0
    else
      m := sqrt(m / (2 * N - 6));
    result := true;
  end
  else if n > 1 then
  begin
    CalcHelmertMatrix(N, pr, prN, Ea, AMatrix, m);
    result := true;
  end
  else
    result := false;

end;

function AsFloat(const opt: string): double;
var
  Value: string;
  i: integer;
begin
  if opt <> '' then
    Value := opt
  else
    Value := '0';
  Value := StringReplace(Value, FormatSettings.ThousandSeparator, FormatSettings.DecimalSeparator,
    [rfReplaceAll]);
  for i := length(Value) downto 1 do
  begin
    if FormatSettings.DecimalSeparator = value[i] then
    begin
      value[i] := ';';
      BREAK;
    end;
  end;

  Value := StringReplace(Value, FormatSettings.DecimalSeparator, '', [rfReplaceAll]);
  value := StringReplace(Value, ';', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  result := StrToFloat(value);
end;

function AsFloatDef(const AValue: string; const ADefaultValue: Double): Double;
var
  Value: string;
  I: integer;
begin
  Value :=  AValue;
  Value := StringReplace(Value, FormatSettings.ThousandSeparator, FormatSettings.DecimalSeparator, [rfReplaceAll]);
  for I := length(Value) downto 1 do
  begin
    if FormatSettings.DecimalSeparator = value[I] then
    begin
      Value[I] := ';';
      BREAK;
    end;
  end;

  Value := StringReplace(Value, FormatSettings.DecimalSeparator, '', [rfReplaceAll]);
  value := StringReplace(Value, ';', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  Result := StrToFloatDef(Value, ADefaultValue);
end;

(*
function IsSamePoint(const p1, p2: TlicgCoor4D): Boolean;
begin
  Result := (roundTo(p1.x, -10) = roundto(p2.x, -10)) and (roundto(p1.y, -10) = RoundTo(p2.y, -10));
end; *)

function GetCoor(const yatay, dusey: double; const a, d: Double; const aType:
  TlicgAngleType): TlicgCoor;
begin
  Result.x := 0;
  Result.y := 0;

  case aType of
    atRadian:
      begin
        Result.X := yatay + (sin(a) * d);
        Result.Y := dusey + (cos(a) * d);
      end;
    atDegree:
      begin
        Result.X := yatay + (sin(DegToRad(a)) * d);
        Result.Y := dusey + (cos(DegToRad(a)) * d);
      end;
    atGrad:
      begin
        Result.X := yatay + (sin(GradToRad(a)) * d);
        Result.Y := dusey + (cos(GradToRad(a)) * d);
      end;
  end;
end;

procedure GetCoor(const yatay, dusey: double; const a, d: Double; const aType:
  TlicgAngleType; var outX, outY: double);
begin
  outx := 0;
  outy := 0;

  case aType of
    atRadian:
      begin
        outX := yatay + (sin(a) * d);
        outY := dusey + (cos(a) * d);
      end;
    atDegree:
      begin
        outX := yatay + (sin(DegToRad(a)) * d);
        outY := dusey + (cos(DegToRad(a)) * d);
      end;
    atGrad:
      begin
        outX := yatay + (sin(GradToRad(a)) * d);
        outY := dusey + (cos(GradToRad(a)) * d);
      end;
  end;
end;

function GetCoor(const yatay, dusey: double; const Delta_y, Delta_d: Double): TlicgCoor;
begin
  result.x := yatay + Delta_y;
  result.y := dusey + Delta_d;
end;

function Angle2DS(const P1, P2: TlicgCoor): Double;
var
  Dx, Dy: Double;
begin
  Dx := P2.x - P1.x;
  Dy := P2.y - P1.y;
  Result := 0;
  if ((Dx) > 0) and (Dy = 0) then
    Result := Pi / 2
  else if (Dx = 0) and (Dy > 0) then
    Result := 0
  else if (Dx = 0) and (Dy < 0) then
    Result := pi
  else if (Dx < 0) and (Dy = 0) then
    Result := pi + (pi / 2)
  else if ((Dx) > 0) and ((Dy) > 0) then
    Result := (arctan((Dx) / (Dy)))
  else if ((Dx) > 0) and ((Dy) < 0) then
    Result := (arctan((Dx) / (Dy))) + (pi)
  else if ((Dx) < 0) and ((Dy) < 0) then
    Result := (arctan((Dx) / (Dy))) + pi
  else if ((Dx) < 0) and ((Dy) > 0) then
    Result := (arctan((Dx) / (Dy))) + (pi * 2);
end;

function Angle2DS(const P1X, P1Y, P2X, P2Y: Double): Double;
var
  Dx, Dy: Double;
begin
  Dx := P2x - P1x;
  Dy := P2y - P1y;
  Result := 0;
  if ((Dx) > 0) and (Dy = 0) then
    Result := Pi / 2
  else if (Dx = 0) and (Dy > 0) then
    Result := 0
  else if (Dx = 0) and (Dy < 0) then
    Result := pi
  else if (Dx < 0) and (Dy = 0) then
    Result := pi + (pi / 2)
  else if ((Dx) > 0) and ((Dy) > 0) then
    Result := (arctan((Dx) / (Dy)))
  else if ((Dx) > 0) and ((Dy) < 0) then
    Result := (arctan((Dx) / (Dy))) + (pi)
  else if ((Dx) < 0) and ((Dy) < 0) then
    Result := (arctan((Dx) / (Dy))) + pi
  else if ((Dx) < 0) and ((Dy) > 0) then
    Result := (arctan((Dx) / (Dy))) + (pi * 2);
end;

function SamePoint(const c1, c2: TlicgCoor4D): Boolean; overload; // nc
//const
 //_EPSILONx : Double = 1.0E-04;

  function zr(r: Double):boolean;
  begin
    zr := Abs(r) < _EPSILON_LOW;
  end;
begin
  Result := zr(c1.Y - c2.Y) and zr(c1.X - c2.X);
end;

function IsSamePoint2D(const p1, p2: TlicgCoor; APERTURE: Double = 0.0): Boolean; overload;
begin
  Result := (_Distance(p1, p2) <= APERTURE)
end;

type
  pDistRec = ^tDistRec;

  tDistRec = record
    dist: Double;
    ptIndis: Integer;
  end;

function CompareDist(Item1, Item2: Pointer): Integer;
begin
  if tDistRec(Item1^).dist < tDistRec(Item2^).dist then
    result := 1
  else if tDistRec(Item1^).dist > tDistRec(Item2^).dist then
    result := -1
  else
    result := 0;
end;

procedure SortIntersectPoints(_P1: TlicgCoor; __intV: IlicgVector);

  function sortVector(pt: TlicgCoor; vect: IlicgVector): TlicgCoor;
  var
    I: Integer;
    dist: Double;
    p: pDistRec;
    pList: TList;
  begin
    pList := TList.Create;
    for I := 0 to __intV.count - 1 do
    begin
      dist := _Distance(pt, __intV[i]);
      New(P);
      p^.dist := dist;
      p^.ptIndis := I;
      pList.Add(p);
    end;

    pList.Sort(@CompareDist);

    for I := pList.Count - 1 downto 0 do
    begin
      p := pList[I];
      vect.Add(__intV[p^.ptIndis].x, __intV[p^.ptIndis].y);
    end;

    for I := pList.Count - 1 downto 0 do
    begin
      Dispose(pList[I]);
    end;
    pList.Free;
  end;

var
  vect: IlicgVector;
begin
  vect := Licad.CreateEntityFactory.MakeVector(2, 0);
  sortVector(_p1, vect);
  __intV.Clear;
  __intV.assign(vect);
  vect := nil;
end;

function isMidPointInsideEntity(const Ent: IlicgEntity; p1, p2: TlicgCoor): Boolean;
var
  cx, cy: Double;
begin
  cx := (p1.x + p2.x) / 2;
  cy := (p1.y + p2.y) / 2;
  Result := Ent.InPoly(AsCoor(cx, cy));
end;

function MeterToDeg(m: double): double;
begin
  // 1 degree 110574.61 meter
  Result := m / 110574.61;
end;

function DegToMeter(d: double): double;
begin
  result := d * 110574.61;
end;

function GetAffineMatrixFromDRE(DreFn: string; var pikselW, pikselH: integer):
  TlicgMatrix; overload;
var
  n, i: integer;
  pr, prN: pointarray;
  Ea: ErrorArray;
  AMatrix: TlicgMatrix;
  m: DOUBLE;
  ini: TIniFile;
  ini1: TMemIniFile;         //tuna ekleme
  encoding: TEncoding;      //tuna ekleme
begin

  Result := IDENTITY_MATRIX2D;
  encoding := TEncoding.ANSI;
  pikselW := 0;
  pikselH := 0;

  if FileExists(DreFn) then
  begin
    ini := TIniFile.Create(DreFn);
    try
      n := ini.ReadInteger('Tr-Noktalari', 'Sayisi', 0);
      if n > 3 then
      begin

        if ini.SectionExists('ImageFileInfoEx') then
        begin
          pikselW := ini.ReadInteger('ImageFileInfoEx', 'Width', 0);
          pikselH := ini.ReadInteger('ImageFileInfoEx', 'Height', 0);
        end
        else if ini.SectionExists('ImageFileInfo') then
        begin
          pikselW := ini.ReadInteger('ImageFileInfo', 'Width', 0);
          pikselH := ini.ReadInteger('ImageFileInfo', 'Height', 0);
        end;

        if not ((pikselW = 0) or (pikselH = 0)) then
        begin

          for i := 1 to n do
          begin

            pr[i - 1].X := AsFloat(ini.ReadString('Tr-Noktalari', 'RasterPY' +
              inttostr(i), '0'));

            pr[i - 1].Y := pikselH - AsFloat(ini.ReadString('Tr-Noktalari',
              'RasterPX' + inttostr(i), '0'));

            prN[i - 1].X := AsFloat(ini.ReadString('Tr-Noktalari', 'HaritaPY' +
              inttostr(i), '0'));
            prN[i - 1].Y := AsFloat(ini.ReadString('Tr-Noktalari', 'HaritaPX' +
              inttostr(i), '0'));

          end;

          AMatrix := result;
          if CalcAffineMatrix(n, pr, prN, Ea, AMatrix, m) then
          begin
            Result := AMatrix;
          end;
        end;
      end;
    finally
      ini.Free;
    end;
    if n < 3 then
    begin
      ini1 := TMemIniFile.Create(DreFn, encoding); //tuna ekleme
      try
        n := ini1.ReadInteger('Tr-Noktalari', 'Sayisi', 0);
        if n > 3 then
        begin

         if ini1.SectionExists('ImageFileInfoEx') then
         begin
           pikselW := ini1.ReadInteger('ImageFileInfoEx', 'Width', 0);
            pikselH := ini1.ReadInteger('ImageFileInfoEx', 'Height', 0);
          end
         else if ini.SectionExists('ImageFileInfo') then
          begin
           pikselW := ini1.ReadInteger('ImageFileInfo', 'Width', 0);
           pikselH := ini1.ReadInteger('ImageFileInfo', 'Height', 0);
          end;

          if not ((pikselW = 0) or (pikselH = 0)) then
          begin

            for i := 1 to n do
           begin

              pr[i - 1].X := AsFloat(ini1.ReadString('Tr-Noktalari', 'RasterPY' +
              inttostr(i), '0'));

              pr[i - 1].Y := pikselH - AsFloat(ini1.ReadString('Tr-Noktalari',
              'RasterPX' + inttostr(i), '0'));

              prN[i - 1].X := AsFloat(ini1.ReadString('Tr-Noktalari', 'HaritaPY' +
              inttostr(i), '0'));
              prN[i - 1].Y := AsFloat(ini1.ReadString('Tr-Noktalari', 'HaritaPX' +
              inttostr(i), '0'));

            end;

            AMatrix := result;
            if CalcAffineMatrix(n, pr, prN, Ea, AMatrix, m) then
            begin
             Result := AMatrix;
             end;
          end;
        end;
      finally
        ini1.Free;    // tuna ekleme bitiþ
      end;
    end;
  end;
end;

function getAffineMatrixFromDRE(DreFn: string): TlicgMatrix; overload;
var
  n, i: integer;
  pr, prN: pointarray;
  Ea: ErrorArray;
  AMatrix: TlicgMatrix;
  m: DOUBLE;
  ini: TIniFile;
begin
  Result := IDENTITY_MATRIX2D;
  if FileExists(DreFn) then
  begin

    ini := TIniFile.Create(DreFn);

    try

      n := ini.ReadInteger('Tr-Noktalari', 'Sayisi', 0);

      if n > 3 then
      begin

        for i := 1 to n do
        begin

          pr[i - 1].X := AsFloat(ini.ReadString('Tr-Noktalari', 'RasterPY' +
            inttostr(i), '0'));

          pr[i - 1].Y := AsFloat(ini.ReadString('Tr-Noktalari', 'RasterPX' +
            inttostr(i), '0'));

          prN[i - 1].X := AsFloat(ini.ReadString('Tr-Noktalari', 'HaritaPY' +
            inttostr(i), '0'));
          prN[i - 1].Y := AsFloat(ini.ReadString('Tr-Noktalari', 'HaritaPX' +
            inttostr(i), '0'));

        end;

        AMatrix := result;
        if CalcAffineMatrix(n, pr, prN, Ea, AMatrix, m) then
        begin
          Result := AMatrix;
        end;
      end;
    finally
      ini.Free;
    end;
  end;
end;

function IsPointOnLine(const C, A, B: TlicgCoor; const ASegmentEpsilon: Double = 0.05): Boolean;
//const
  //_SEGEPS  : double = 0.05; // nc
var
  st, sl: double;
begin
  INVside_shoot(C, A, B, sl, st);
  Result := (abs(st) < ASegmentEpsilon) and
                 (sl < (_Distance(A, B) + ASegmentEpsilon)) and
                 (sl > -ASegmentEpsilon);
end;

function CalcLineSide(PFirstC: TlicgCoor; PSecondC: TlicgCoor; PCurrCoor: TlicgCoor): Double;
begin
  Result := ((PCurrCoor.X-PFirstC.X)*(PSecondC.Y-PFirstC.Y))-((PCurrCoor.Y-PFirstC.Y)*(PSecondC.X-PFirstC.X));
end;

procedure Rotate_Around(Origin: TlicgCoor; var C: TLicgcoor; Angle:Double);
var
  XX, YY, CosT, Sint: Double;
Begin
  YY := C.Y - Origin.Y;
  XX := C.X - Origin.X; (* first shift origin to Origin *)
  SinCos(Angle, Sint, Cost); { cost:=cos(Angle); sint:=sin(Angle);}
  C.X := YY * Sint + XX * Cost;(* rotates counterclockwise *)
  C.Y := YY * Cost - XX * Sint;
  C.X := C.X + Origin.X; (* take origin back to (0,0) *)
  C.Y := C.Y + Origin.Y;
end;

function Polar(Origin: TlicgCoor; Angle, Dist: Double): TLicgCoor;
var
  C: TLicgCoor;
begin
  C := Origin;
  C.Y := C.Y + Dist;
  Rotate_Around(Origin, C , Angle);
  Result := C;
end;

procedure INVside_shoot(const cside, ca, cb: TlicgCoor; var side_length,
  side_tail: double; const AEpsilon: Double =_EPSILON_LOW);
var
  s, dx, dy, dxn, dyn: double;
begin
  s := _Distance(ca, cb);
  side_length := 0.0;
  side_tail := _Distance(cside, ca);
  if abs(s) < (AEpsilon * AEpsilon) then
    exit;
  dy := cb.X - ca.X;
  dx := cb.y - ca.y;
  dyn := cside.x - ca.x;
  dxn := cside.y - ca.y;
  side_length := (dy * dyn + dx * dxn) / s;
  side_tail := (dx * dyn - dy * dxn) / s;
end;

function Side_Shoot(c1: TlicgCoor; c2: TlicgCoor; sidelength: Double; sidetail: Double): TlicgCoor;
var
  resultC: TlicgCoor;
  _angle: Double;
begin
  _angle := Angle2DS(c1,c2);
  resultC := c1;
  if sidelength <> 0 then
    resultC := Polar(c1,_angle,sidelength);
  c1 := ResultC;
  if sidetail <> 0 then
    resultC := Polar(c1,(_angle+(pi/2)),sidetail);
  Result := resultC;
end;

procedure FindPointLine(sPoints: IlicgVector; P: TlicgCoor; var FI, SI: Integer);
var
  I: Integer;
  Dist1, Dist2, Dist3, Fark, EnKFark: Double;
begin
  FI := -1;
  SI := -1;
  EnKFark := 9999999999999.9999;
  for I := 0 to sPoints.Count - 2 do
  begin
    Dist1 := _Distance(sPoints[I], sPoints[I + 1]);
    Dist2 := _Distance(p, sPoints[I]);
    Dist3 := _Distance(p, sPoints[I + 1]);
    Fark := (Dist2 + Dist3) - Dist1;
    if Fark < EnKFark then
    begin
      FI := I;
      SI := I + 1;
      EnKFark := Fark;
    end;
  end; //For I
end; //FindPointLine

function Intersect(Vect1, Vect2, ResultIntersectVector: IlicgVector): Boolean;
var
  X1, Y1, X2, Y2, X11, Y11, X22, Y22: Double;
  Alfa1, alfa2, SinAlfa1, SinAlfa2, CosAlfa1, CosAlfa2: Double;
  IPoint: TlicgCoor;
  ParalelUcUca: Boolean;
  UstUste: Boolean;
begin
  Result := True;
//  ParalelUcUca := False;
  ResultIntersectVector.Clear;
  X1 := Vect1[0].y;
  Y1 := Vect1[0].x;
  X11 := Vect1[1].y;
  Y11 := Vect1[1].x;
  X2 := Vect2[0].y;
  Y2 := Vect2[0].x;
  X22 := Vect2[1].y;
  Y22 := Vect2[1].x;

  Alfa1 := Angle2DS(AsCoor(Y1, X1), AsCoor(Y11, X11));
  Alfa2 := Angle2DS(AsCoor(Y2, X2), AsCoor(Y22, X22));
  Alfa1 := Roundto(Alfa1, -8);
  Alfa2 := Roundto(Alfa2, -8);
  ParalelUcUca := (((X11 = X2) and (Y11 = Y2)) or ((X1 = X2) and (Y1 = Y2)) or ((X1
    = X22) and (Y1 = Y22)) or ((X11 = X22) and (Y11 = Y22)));
  UstUste := (((X1 = X2) and (Y1 = Y2)) and ((X11 = X22) and (Y11 = Y22))) or (((X1
    = X22) and (Y1 = Y22)) and ((X11 = X2) and (Y11 = Y2)));
  if UstUste then
  begin
    Result := False;
    Exit; //Hatlar Üst üste
  end;
  if ((Alfa1 = Alfa2) or (Alfa1 = Alfa2 + (2 * pi)) or (Alfa1 = Alfa2 - (2 * pi)))
    and (not ParalelucUca) then
  begin

    Result := False;
    Exit; //Paralel Hatlar
  end;

  if ParalelUcUca and ((Alfa1 = Alfa2) or (Alfa1 = Alfa2 + (2 * pi)) or (Alfa1 =
    Alfa2 - (2 * pi))) then
  begin
    if ((X11 = X2) and (Y11 = Y2)) then
      ResultIntersectVector.Add(y11, X11)
    else if ((X1 = X2) and (Y1 = Y2)) then
      ResultIntersectVector.Add(Y1, X1)
    else if ((X1 = X22) and (Y1 = Y22)) then
      ResultIntersectVector.Add(Y1, X1)
    else if ((X11 = X22) and (Y11 = Y22)) then
      ResultIntersectVector.Add(Y11, X11);
    Result := True;
    Exit;
  end;

  SinAlfa1 := Sin(Alfa1);
  CosAlfa1 := Cos(Alfa1);
  SinAlfa2 := Sin(Alfa2);
  CosAlfa2 := Cos(Alfa2);

  IPoint.x := (((SinAlfa1 * SinAlfa2) * (X2 - X1)) + (Y1 * SinAlfa2 * CosAlfa1)
    - (Y2 * SinAlfa1 * CosAlfa2)) / ((CosAlfa1 * SinAlfa2) - (SinAlfa1 * CosAlfa2));
  if (Roundto(SinAlfa1, -10) <> 0) and (Roundto(SinAlfa2, -10) <> 0) then
    IPoint.y := ((IPoint.x * CosAlfa1) - (Y1 * CosAlfa1) + (X1 * SinAlfa1)) / SinAlfa1
  else
  begin
    if Roundto(SinAlfa2, -10) = 0 then
    begin
      IPoint.y := ((IPoint.x * CosAlfa1) - (Y1 * CosAlfa1) + (X1 * SinAlfa1)) / SinAlfa1;
    end;
    if Roundto(SinAlfa1, -10) = 0 then
    begin
      IPoint.y := ((IPoint.x * CosAlfa2) - (Y2 * CosAlfa2) + (X2 * SinAlfa2)) / SinAlfa2;
    end;
  end;
  ResultIntersectVector.Add(IPoint.x, IPoint.y);
end;

function VectorAngle(PVec1, PVec2: IlicgVector): Double;
const
  EPSILON = 0.0001;
var
  Dist1, Dist2, Dist3: Double;
  TempTotal1, TempTotal2, TempTotal3: Double;
begin
  try
    Result := 0;
    dist1 := _Distance(PVec1[0],PVec1[1]);
    dist2 := _Distance(PVec2[0],PVec2[1]);
    dist3 := _Distance(PVec1[1],PVec2[1]);
    if ((dist1) = 0) or ((dist2) = 0) or ((dist1 * dist2) = 0) then
      Exit;
    TempTotal1 := (sqr(dist1) + sqr(dist2) - sqr(dist3));
    TempTotal2 := (2 * dist1 * dist2);
    TempTotal3 := TempTotal1 / TempTotal2;
    if CompareValue(TempTotal3,-1,EPSILON) = 0 then
      Result := pi
    else if CompareValue(TempTotal3,1,EPSILON) = 0 then
      Result := 0
    else
      Result := ArcCos(TempTotal3);
  Except // Sýfýra Bölme Hatasý.
  end;
end;

function CalcVerticalDistance(P1, P2, RefP: TLicgCoor): Double;
var
  O, A, Dist: Double;
begin
  Dist := _Distance(P1, P2);
  O := (P2.X - P1.X) / Dist;
  A := (P2.Y - P1.Y) / Dist;
  Result := ((RefP.X * A) - (RefP.Y * O) + (P1.Y * O) - (P1.X * A)) / ((A * A) + (O * O));
end;

end.


