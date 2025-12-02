unit Lider.CG.Com.GeoLibrary;

interface

{$I Lider.CG.Com.Component.inc}

uses
  Graphics,
  Math,
  Lider.CG.Com.Lib,
  Lider.CG.Com.System,
  Lider.CG.Com.GIS,
  Lider.CG.Com.FastGeo,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

const
   {TlicgPolyline nesnesinde, fVector dinamik dizisinin bellekte kapladigi yerin genisletilmesi gerektiginde,
    VECTOR_EXTENTION_LENGTH kadar genisletilir.}
  VECTOR_EXTENTION_LENGTH = 100;

   { Polygon + geometry iliski sabitleri. PointWhere ve SegmentWhere fonksiyonunun sonuclari. }
  undefined = -2;
  insideIt = -1;
  onIt = 0;
  outsideIt = 1;
  crossesIt = 2; // for the fonc. Segment_Is_Where_Of_Polygon.
  InsideTouch = 3;
  OutsideTouch = 4;
  NoTouch = 5; // polygon_touch_polygon isleminde kullaniliyor.

{ LINERELATION sabitleri }
  _cross = 100;
  _equal = 101;
  _overlap = 102;
  _touch = 103;
  _contains = 104;
  _within = 105;
  _district = 106;

const
   { Color masks }
  _Red_Mask = $00FF0000;
  _Green_Mask = $0000FF00;
  _Blue_Mask = $000000FF;

(* ilker silme gerek yok

function _Min(const Point1, Point2: TlicgCoor): TlicgCoor; overload;

function _Min(const Point1, Point2: TlicgCoor3D): TlicgCoor3D; overload;

function _Max(const Point1, Point2: TlicgCoor): TlicgCoor; overload;

function _Max(const Point1, Point2: TlicgCoor3D): TlicgCoor3D; overload;  *)

function DegToDMS(angle: extended; decimals: integer = 2): string; // ilker ekleme
procedure MidPoint(const X1, Y1, X2, Y2: Double; out MidX, MidY: Double); overload; // ilker ekleme
function MidPoint(const P1, P2: TlicgCoor):TlicgCoor; overload; // ilker ekleme


function _DegreeToRadian(x: double): double;

function _RadianToDegree(x: double): double;

function _AddExtensions(const a, b: TlicgExtent4D): TlicgExtent4D;

function InflateExtent(const R: TlicgExtent; apperture: double): TlicgExtent;

function _IsClosed(const Polygon: IlicgVector; const Length: integer): boolean; overload; // 4D

function _Distance(const Point1, Point2: TlicgCoor): double; overload;

function _Distance(const x1, y1, x2, y2: Double): Double; overload; // ilker OK

function _Distance(const Point: TlicgCoor; const CoorArray: IlicgVector; const
  Length: integer): double; overload;

function _Distance(const CoorArray1, CoorArray2: IlicgVector; const Length1,
  Length2: integer): double; overload;

function _Distance(const Point: TlicgCoor; const LineA, LineB: TlicgCoor): double; overload;

function _Distance(const x1, y1, z1, x2, y2, z2: double): double; overload;

function _Distance(const LineA1, LineA2, LineB1, LineB2: TlicgCoor): double; overload;

function _Distance(const LineA1, LineA2: TlicgCoor; const CoorArray: IlicgVector;
  const Length: integer): double; overload;

function _IsEqual_RightDirection(const CoorArray1, CoorArray2: IlicgVector;
  const start1, start2, length1, length2: integer; const aEpsilon: double =
  _EPSILON): boolean; overload;

function _IsEqual_RightDirection(const CoorArray1, CoorArray2: IlicgVector;
  const length1, length2: integer; const aEpsilon: double = _EPSILON):
  boolean; overload;

function _IsEqual_ReverseDirection(const CoorArray1, CoorArray2: IlicgVector;
  const start1, start2, length1, length2: integer; const aEpsilon: double =
  _EPSILON): boolean; overload;

function _IsEqual_ReverseDirection(const CoorArray1, CoorArray2: IlicgVector;
  const length1, length2: integer; const aEpsilon: double = _EPSILON):
  boolean; overload;

function _IsPolygonConvex(const Polygon: IlicgVector): boolean;

{ iki yonde de }
function _IsEqual(const AValue1, AValue2: Double; const AEpsilon: Double = _EPSILON): Boolean; overload;
function _IsEqual(const Coor1, Coor2: TlicgCoor; const aEpsilon: Double = _EPSILON): Boolean; overload;
function _IsEqual(const Coor1, Coor2: TlicgCoor3D; const aEpsilon: Double = _EPSILON): Boolean; overload;
function _IsEqual(const CoorArray1, CoorArray2: IlicgVector; const Length1,
  Length2: integer; const aEpsilon: double = _EPSILON): boolean; overload;
function _IsEqual(const CoorArray1, CoorArray2: IlicgVector; const start1,
  start2, length1, length2: integer; const aEpsilon: double = _EPSILON):
  boolean; overload;
function _IsEqual(const Coor1, Coor2: TlicgCoor4D; const aEpsilon: double = _EPSILON): boolean; overload;
function _IsEqual(const MBB1, MBB2: TlicgExtent4D): boolean; overload;

function _NotEqual(const AValue1, AValue2: Double; const AEpsilon: double = _EPSILON): Boolean; overload;                                                                    overload;

function _NotZero(const r: double; aEpsilon: Double = _EPSILON): boolean;

function _Parallel(const Point1, Point2, Point3, Point4: TlicgCoor; const Epsilon:
  double = _EPSILON): Boolean;


{ TRIANGULATION FONKSIYONLARI }
{ Ucgeni, T[1] = minZ, T[3] = maxZ degerlerine gore siralar, dondurur. }
procedure _SortTriangle(var T: TlicgTriangle3D);
{ Fonksiyona gonderilen 3 nokta, x ve y yi iceren bir ucgenin kenarlari ise, nokta icinden bir Z degeri dondurulur.
  Eger ucgen icinde degilse, en yakin olan deger donduruluyor. }

function _GetAvarageZValue(const x, y: double; const HowMany: integer; const P1,
  P2, P3, P4: TlicgCoor3D): double;
{ DIKKAT!! fonksiyona gelen Ucgenin, siralanmis olmasi gerekiyor. }

function _GetZValueFromTriangle(const x, y: double; T: TlicgTriangle3D): double; overload;

function _GetZValueFromTriangle(const x, y: double; const x1, y1, z1, x2, y2, z2,
  x3, y3, z3: double): double; overload;

function _FormatReal(r: double; sdigits: byte): double;

function _RGBToColor(const _r, _g, _b: Byte): TColor;

procedure _ColorToRGB(const Color: TColor; out _r, _g, _b: byte);

{ if Assigned(Ent) and (Ent.Geometry.Points.Count = 4) then begin.... den sonra cagirilmali. }
procedure _EntityToTriangle(const Ent: IlicgEntity; var T: TlicgTriangle3D);
{ END - TRIANGULATION FONKSIYONLARI }

procedure _NormalizePolygon(Entity: IlicgEntity);

{ Bir plane denkleminin Ax + By + Cz + D = 0 oldugunu dusundugumuzde, fonksiyon, verilen ucgenin duzlemini A,B,C,D degerleri ile dondurur. }
procedure _GetPlaneEquation(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double;
  out A, B, C, D: double); overload;

procedure _GetPlaneEquation(const T: TlicgTriangle3D; out A, B, C, D: double); overload;
{ Ucgenin Normali dogrultusunda, Geometrik ortayindan gecen, Z farki 1 olan, yukariya dogru kisa bir dogru dondurur.
  Yukariya dogru olmasi, ucgenin sonunu de belirtir. Ilk nokta, ucgenin agirlik merkezidir. }

procedure _GetNormalLine(const T: TlicgTriangle3D; out P1, P2: TlicgCoor3D); overload;
{ C, ucgen/polygonun orta noktasi, A,B,C ise duzlem denkleminin parametreleri. }

procedure _GetNormalLine(const Cx, Cy, Cz: double; const A, B, C: double; out x,
  y, z: double); overload;
{ verilen ucgenin egimini bulur. Donen deger, Egimin tanjantidir. }

function _GetSlopeAngle(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double):
  double; overload;
{ Ax + By + Cz + D = 0 denklemi ile tanimlanan duzlemin egimini getirir. }

function _GetSlopeAngle(const A, B, C, D: double): double; overload;
{ baki analizi icin, ucgenin x eksenine gore derece cinsinde yonunu dondurur.
  Aci degeri, 360 derece uzerinden doner. }

function _GetAngleOfAspect(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double):
  double; overload;
{ Ax + By + Cz + D = 0 denklemi ile tanimlanan duzlemin, x ekseni ile arasindaki aciyi dondurur.
  Aci degeri, 360 derece uzerinden doner. }

function _GetAngleOfAspect(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double;
  const A, B, C, D: double): double; overload;
{ ucgenin centroidinin yuksekligini, yani ucgenin yuksekligini dondurur. }

function _GetMeanHeight(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double): double;

procedure _Centroid(const x1,y1,x2,y2: double; out x,y: double); overload;
procedure _Centroid(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double; out x, y, z: double); overload;
function _Centroid(const P1, P2: TlicgCoor): TlicgCoor; overload;
function _Centroid(const CoorArray: IlicgVector; start, finish: integer): TlicgCoor; overload;

{ Akis analizinde, verilen ucgenin maksimum egimini... Line_1 noktasi ust, Line_2 noktasi alttaki nokta }
procedure _GetMaxSlopeLine(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double; out
  line_x1, line_y1, line_z1, line_x2, line_y2, line_z2: double); overload;

procedure _GetMaxSlopeLine(const T: TlicgTriangle3D; out Line: TlicgLine3D); overload;

{ Iki CoorArray kesisiyorlar mi? Ama bu intersect isleminde CoorArray larin ic kisimlarini hesaba katmiyor. }
function _Intersect(const CoorArray1, CoorArray2: IlicgVector; start1, length1,
  start2, length2: integer): Boolean; overload;

function _Intersect(const CoorArray1, CoorArray2: IlicgVector; length1,
  length2: integer): Boolean; overload;
{ bu intersection fonksiyonlari, iki line in, cross noktalarini dondurur. Touch noktalarindaki kesisim noktalarini almak icin,
  _IntersectionPoints kullanilmali. }

function _Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: double; const
  ConsiderInfinity: boolean = false; const aEpsilon: double = _EPSILON):
  boolean; overload;

function _Intersect(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  ConsiderInfinity: boolean = false): Boolean; overload;

function _Intersect(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; out P:
  TlicgCoor; const ConsiderInfinity: boolean = false): Boolean; overload;
{ X=0, Y=0, z=Height duzleminin, x1, y1, z1, x2, y2, z2 dogrusunu kestigi noktayi ariyoruz.}

function _Intersect(const Height, x1, y1, z1, x2, y2, z2: double; out P:
  TlicgCoor3D): boolean; overload;

function _Intersect(const A, B: TlicgCoor; const T2D: TlicgTriangle): boolean; overload;

function _Intersect(const Line1A, Line1B, Line2A, Line2B: TlicgCoor3D): boolean; overload;

function _CrossesTriangle(const A, B: TlicgCoor; const T2D: TlicgTriangle; out P1,
  P2: TlicgCoor): boolean; overload;

{ 1->2 segmenti uzerinde, 1. noktadan "newdistance" uzakligina dusen nokta dondurulur. }
function _CoorAtDistance(const newdistance, x1, y1, x2, y2: double; out P:
  TlicgCoor): boolean;

function _SelfIntersecting(const CoorArray: IlicgVector; const startIndex,
  endIndex: integer): Boolean; overload; // 3D

function _SegmentsOnSameVector(const Line: IlicgVector; const start, length:
  integer; const aEpsilon: double = _EPSILON): boolean; overload;

function _SegmentsOnSameVector(const Line: IlicgVector; const length: integer;
  const aEpsilon: double = _EPSILON): boolean; overload;

{ Su durumlarin hepsinde true doner :- Overlap + Equal + Within + contains }
function _LinesAdjacent(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  aEpsilon: double = _EPSILON): boolean;

function _Line1OnLine2(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  aEpsilon: double = _EPSILON): boolean;

function _LinesOverlap(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  aEpsilon: double = _EPSILON): boolean;

{ iki segment ve Polyline'i kesistirir, kesisim noktalarini dondurur.
  ordered = true ise, Noktalar, A->B ye sirali sekilde doner.
  herhangi bir kesisim yoksa, false doner.
  Sonucunda donen "IntersectionPoints"in nillenmesi kullananin sorumlulugundadir...
  Free ederken... finalize(IntersectionPoints) kullanilmali. }
function _IntersectionPoints(const A, B: TlicgCoor; const Polyline: IlicgVector;
  const length: integer; var IntersectionPoints: IlicgVector; IntType:
  TIntersectionType; const ordered: boolean = true; const ConsiderInfinity:
  boolean = false; const aEpsilon: double = _EPSILON): boolean; overload;

function _IntersectionPoints(const Polyline1, Polyline2: IlicgVector; const
  Length1, Length2: integer; var IntersectionPoints: IlicgVector; IntType:
  TIntersectionType; const ordered: boolean = true; const ConsiderInfinity:
  boolean = false; const aEpsilon: double = _EPSILON): boolean; overload;

function VectIntersect(Vector1, Vector2, IntersectVector: IlicgVector;
  StrictlyBetween: Boolean = true; IntersectionType: TIntersectionType =
  itCrossTouch): Boolean;

function _WhereIsPoint(const P: TlicgCoor; const Polygon: IlicgVector; const
  length: integer; const aEpsilon: double = _EPSILON): integer;

function _WhereIsSegment(const A, B: TlicgCoor; const Polygon: IlicgVector; const
  length: integer; const aEpsilon: double = _EPSILON): integer; overload;

function _WhereIsSegment(const AX, AY, BX, BY: double; const Polygon:
  IlicgVector; const length: integer; const aEpsilon: double = _EPSILON):
  integer; overload;

{ sonuc olarak, eger nokta cizginin ustundeyse noktanin bulundugu cizgiyi dondurur. Eger nokta bulunmadi ise, -1 doner. }
function _PointOnLine(const Px, Py: double; const CoorArray: IlicgVector; const
  Length: integer; const aEpsilon: double = _EPSILON): integer; overload;

function _PointOnLine(const P: TlicgCoor; const CoorArray: IlicgVector; const
  Length: integer; const aEpsilon: double = _EPSILON): integer; overload;

function _ClosestSegmentInPart(const X, Y: double; const Coors: IlicgVector;
  start_Index, end_Index: integer; var P1, P2: integer; var MinDist: double): boolean;

function _ClosestSegmentInCoorArray(const X, Y: double; const Coors:
  IlicgVector; var P1, P2: integer; var MinDist: double): boolean;

{ P noktasi, A->B vektoru icinde mi?}
function _IsPointCollinear(const Ax, Ay, Bx, By: double; const Px, Py: double;
  const aEpsilon: double = _EPSILON): boolean; overload;

function _IsPointCollinear(const A, B: TlicgCoor; const Px, Py: double; const
  aEpsilon: double = _EPSILON): boolean; overload;

function _IsPointCollinear(const A, B, P: TlicgCoor; const aEpsilon: double =
  _EPSILON): boolean; overload;

{ sonucta donen,  integer deger, tanimlanmis LINERELATION sabitlerinden birisi
   _cross    = 100;
   _equal    = 101;
   _overlap  = 102;
   _touch    = 103;
   _contains = 104;
   _within   = 105;
   _district = 106; }
function _LineRelate(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  aEpsilon: double = _EPSILON): integer;

function _Polyline1OnPolyline2(const Line1, Line2: IlicgVector; const
  Length1, Length2: integer; const aEpsilon: double = _EPSILON): boolean;

function _PolylinesCross(const Line1, Line2: IlicgVector; const Length1,
  Length2: integer; const aEpsilon: double = _EPSILON): boolean;

function _PolylinesOverlap(const Polyline1, Polyline2: IlicgVector; const
  Length1, Length2: integer; const aEpsilon: double = _EPSILON): boolean;

function _Polyline_Touch_Polyline(const Polyline1, Polyline2:
  IlicgVector; const Length1, Length2: integer; const aEpsilon: double =
  _EPSILON): boolean;

function _MBBIntersect(const MBB1, MBB2: TlicgExtent4D): boolean; // iki MBB birbiriyle kesisiyor mu?

function _MBB1WithinMBB2(const MBB1, MBB2: TlicgExtent4D): boolean;    // MBB1, MBB2 in icinde mi?

function _PointInMBB(const P: TlicgCoor; const MBB: TlicgExtent4D): boolean;

{ saat yonunun tersi poligonlarda(Exterior) pozitif, saat yonundeki poligonlarda(Interior) negatif sonuc cikarir. }
function _Area(const Polygon: IlicgVector; const Length: integer): double; overload;

function _Area(const x1, y1, x2, y2, x3, y3: double): double; overload;

function _Area(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double): double; overload;

function _SimplePolygonOrientation(const CoorArray: IlicgVector; const Length:
  integer): Integer;
{ Dizinin yonunu degistirir. Clockwise -> CounterClockwise, CounterClockwise -> Clockwise  }
{ StartIndex -> dizinin hangi indeksinden itibaren orientation un deðiþtirileceðini belirler.
  Length -> StartIndex'ten sonra ne kadar uzunluk içinde iþlem yapýlacaðýný belirtir. }

procedure _ChangeOrientation(CoorArray: IlicgVector; const Length: integer); overload;

procedure _ChangeOrientation(CoorArray: IlicgVector; startIndex, endIndex:
  integer); overload;

function _IsCounterClockWise(CoorArray: IlicgVector; startIndex, endIndex:
  integer): Boolean; overload;

function _IsCounterClockWise(CoorArray: IlicgVector): Boolean; overload;

{ Semt açýsý }
function Angle2DS(const P1, P2: TlicgCoor): Double; overload;
function Angle2DS(const P1X, P1Y, P2X, P2Y: Double): Double; overload;


(* TatukGIS kullanýlmaya baþlandýðýnda açýlmak üzere kapatýlmýþtýr.

   { TatukGIS için.... Daha sonra TlicgLine ye çevrilmelidir. }
   {@@ Line between two Geometry.Points.
     @param A begin
     @param B end }
   T_Line = packed record
     A : TlicgCoor;
     B : TlicgCoor;
   end ;

{ TatukGIS icin }
function GisLine2Point(  const _lineA, _lineB, _ptg: TlicgCoor ): Double;
function GisGetLinesCrossing( const _line1, _line2: T_Line; var _ptg: TlicgCoor ): Boolean;
{ Geometry de bulunan tum nokta sayisini dondurur...}
function GetSizeOf( const geom: IlicgGeometry ): integer;*)

implementation

uses
  SysUtils,
  Lider.CG.Com.LicadInt;

function DegToDMS(angle: extended; decimals: integer = 2): string; // ilker ekleme
var
  D:integer;
  F, M,S:extended;
  sign:integer;
  SStr: string;
begin
  d:=Trunc(angle);
  If angle<0 then sign:=-1 else sign:=+1;
  F:=abs(frac(angle));
  If F > 0.9997222 then
  begin
    m:=0.0;
    d:=d+sign;
  end
  else
    m:=F*60;

  s:=frac(M)*60;
  m:=int(M);
  if s>=59.99 then
  begin
    s:=0;
    m:=m+1;
  end;
  if (angle<0) and (d=0) then
  begin
    SStr := format('%5.*f',[decimals,s]);
    SStr := FormatFloat('00.00', StrToFloatDef(SStr, 0)) + '''''';
    result:=format('-%3d°%2.2d''',[d,trunc(m)]) + SStr;
    //orj result:=format('-%3d°%2.2d''%5.*f''''',[d,trunc(m),decimals,s])
  end
  else
  begin
    SStr := format('%5.*f',[decimals,s]);
    SStr := FormatFloat('00.00', StrToFloatDef(SStr, 0)) + '''''';
    result:=format('%3d°%2.2d''',[d,trunc(m)]) + SStr;
    // orj result:=format('%3d°%2.2d''%5.*f''''',[d,trunc(m),decimals,s]);
  end;
end;

procedure MidPoint(const X1, Y1, X2, Y2: Double; out MidX, MidY: Double);
begin
  MidX := (x1 + x2) * 0.5;
  MidY := (y1 + y2) * 0.5;
end;

function MidPoint(const P1, P2: TlicgCoor):TlicgCoor;
begin
  MidPoint(P1.X, P1.Y, P2.X, P2.Y, Result.X, Result.Y);
end;

function _DegreeToRadian(x: double): double;
begin
  result := x * PIDiv180;
end;

function _RadianToDegree(x: double): double;
begin
  result := x * _180DivPI;
end;

function _AddExtensions(const a, b: TlicgExtent4D): TlicgExtent4D;
begin
  result.LowerLeft.X := Min(a.LowerLeft.X, b.LowerLeft.X);
  result.LowerLeft.Y := Min(a.LowerLeft.Y, b.LowerLeft.Y);
  result.LowerLeft.Z := Min(a.LowerLeft.Z, b.LowerLeft.Z);
  result.LowerLeft.N := Min(a.LowerLeft.N, b.LowerLeft.N);

  result.UpperRight.X := Max(a.UpperRight.X, b.UpperRight.X);
  result.UpperRight.Y := Max(a.UpperRight.Y, b.UpperRight.Y);
  result.UpperRight.Z := Max(a.UpperRight.Z, b.UpperRight.Z);
  result.UpperRight.N := Max(a.UpperRight.N, b.UpperRight.N);
end;

function InflateExtent(const R: TlicgExtent; apperture: double): TlicgExtent;
begin
  result.LowerLeft.X := R.LowerLeft.X - apperture;
  result.LowerLeft.Y := R.LowerLeft.Y - apperture;
  result.UpperRight.X := R.UpperRight.X + apperture;
  result.UpperRight.Y := R.UpperRight.Y + apperture;
end;

function _IsClosed(const Polygon: IlicgVector; const Length: integer): boolean;
begin
  result := (Length > 3) and _IsEqual(Polygon.Coors4D[0], Polygon.Coors4D[Length - 1]);
end;

function _Distance(const Point1, Point2: TlicgCoor): double;
begin
  result := _Distance(Point1, Point2);
end;

function _Distance(const x1, y1, x2, y2: Double): Double; overload; // ilker OK
var
  dx : Double;
  dy : Double;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  Result := Sqrt(dx * dx + dy * dy);
end;

// parts are counted in.
function _Distance(const Point: TlicgCoor; const CoorArray: IlicgVector; const
  Length: integer): double; overload;
var
  _start, _end, i, j: Integer;
  TempDist: double;
begin
  Result := -1;
  if (Length > 0) then
  begin
    if Length = 1 then
    begin
      result := _Distance(Point, CoorArray[0]);
      exit;
    end;

    i := 0;
    Result := MinimumDistanceFromPointToSegment(Point.x, Point.y, CoorArray[i].x,
      CoorArray[i].y, CoorArray[i + 1].x, CoorArray[i + 1].y);

    if result = 0 then
      exit;

    for i := 0 to CoorArray.PartCount - 1 do
    begin
      _start := CoorArray.Parts[i];
      _end := CoorArray.PartEnd(i);

      for j := _start to _end - 1 do
      begin
        TempDist := MinimumDistanceFromPointToSegment(Point.x, Point.y,
          CoorArray[j].x, CoorArray[j].y, CoorArray[j + 1].x, CoorArray[j + 1].y);
        if TempDist < Result then
        begin
          Result := TempDist;
          if result = 0 then
            exit; // 0, minimum distance olmali cunku.
        end;
      end;
    end;
  end;
end;

function _Distance(const CoorArray1, CoorArray2: IlicgVector; const Length1,
  Length2: integer): double; overload;
var
  _start, _end, i, j: integer;
  TempDist: double;
begin
  Result := -1;

  if (Length1 > 0) and (Length2 > 0) then
  begin
    result := _Distance(CoorArray1[0], CoorArray2, Length2);

    if Length1 = 1 then
      exit;

    for i := 0 to CoorArray1.PartCount - 1 do
    begin
      _start := CoorArray1.Parts[i];
      _end := CoorArray1.PartEnd(i);

      for j := _start to _end - 1 do
      begin
        TempDist := _distance(CoorArray1[j], CoorArray2, Length2);
        if TempDist < Result then
        begin
          Result := TempDist;
          if result = 0 then
            exit; // 0, minimum distance olmali cunku.
        end;
      end;
    end;
  end;
end;

function _Distance(const Point: TlicgCoor; const LineA, LineB: TlicgCoor): double; overload;
begin
  result := MinimumDistanceFromPointToSegment(Point.X, Point.Y, LineA.X, LineA.Y,
    LineB.X, LineB.Y);
end;

function _Distance(const x1, y1, z1, x2, y2, z2: double): double;
var
  dx: double;
  dy: double;
  dz: double;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  dz := z2 - z1;
  Result := Sqrt(dx * dx + dy * dy + dz * dz);
end;

function _Distance(const LineA1, LineA2, LineB1, LineB2: TlicgCoor): double; overload;
begin
  Result := DistanceSegmentToSegment(LineA1.X, LineA1.Y, LineA2.X, LineA2.Y, LineB1.X, LineB1.Y, LineB2.X, LineB2.Y);
end;

function _Distance(const LineA1, LineA2: TlicgCoor; const CoorArray: IlicgVector;
  const Length: integer): double;
var
  dist: double;
  i: integer;
begin
  result := -1;
  if length > 1 then
  begin
    result := _Distance(LineA1, LineA2, CoorArray[0], CoorArray[1]);
    for i := 1 to Length - 2 do
    begin
      dist := _Distance(LineA1, LineA2, CoorArray[i], CoorArray[i + 1]);

      if result > dist then
        result := dist;

      if result = 0 then
        exit;
    end;
  end;
end;

function _IsEqual(const AValue1, AValue2: double; const AEpsilon: double = _EPSILON): Boolean; overload;
var
  Diff : double;
begin
  Diff := AValue1 - AValue2;
  Assert(((-AEpsilon <= Diff) and (Diff <= AEpsilon)) = (Abs(Diff) <= AEpsilon),'Error - Illogical error in equality check. (IsEqual)');
  Result := ((-AEpsilon <= Diff) and (Diff <= AEpsilon));
end;

function _IsEqual(const Coor1, Coor2: TlicgCoor; const aEpsilon: double = _EPSILON): boolean; overload;
begin
  Result := _IsEqual(Coor1, Coor2, aEpsilon);
end;

function _IsEqual(const Coor1, Coor2: TlicgCoor3D; const aEpsilon: double = _EPSILON): boolean; overload;
begin
  Result := _IsEqual(Coor1, Coor2, aEpsilon);
end;

function _IsEqual(const CoorArray1, CoorArray2: IlicgVector; const Length1,
  Length2: integer; const aEpsilon: double = _EPSILON): boolean;
begin
  Result := _IsEqual(CoorArray1, CoorArray2, 0, 0, length1, length2, aEpsilon);
end;

function _IsEqual(const CoorArray1, CoorArray2: IlicgVector; const start1,
  start2, length1, length2: integer; const aEpsilon: double = _EPSILON): boolean;
begin
  Result := _IsEqual_RightDirection(CoorArray1, CoorArray2, start1, start2,
    length1, length2, aEpsilon) or _IsEqual_ReverseDirection(CoorArray1,
    CoorArray2, start1, start2, length1, length2, aEpsilon);
end;

function _IsEqual(const Coor1, Coor2: TlicgCoor4D; const aEpsilon: double = _EPSILON): boolean;
begin
  Result := _IsEqual(Coor1.X, Coor2.X, aEpsilon) and _IsEqual(Coor1.Y, Coor2.Y, aEpsilon) and
    _IsEqual(Coor1.Z, Coor2.Z, aEpsilon) and _IsEqual(Coor1.N, Coor2.N, aEpsilon);
end;

function _IsEqual(const MBB1, MBB2: TlicgExtent4D): boolean;
begin
  Result := _IsEqual(MBB1.LowerLeft, MBB2.LowerLeft) and _IsEqual(MBB1.UpperRight, MBB2.UpperRight);
end;

function _NotEqual(const AValue1, AValue2: Double; const AEpsilon: Double = _EPSILON): Boolean;
var
  Diff : Double;
begin
  Diff := AValue1 - AValue2;
  Assert(((-AEpsilon > Diff) or (Diff > AEpsilon)) = (Abs(AValue1 - AValue2) > AEpsilon),'Error - Illogical error in equality check. (NotEqual)');
  Result := ((-AEpsilon > Diff) or (Diff > AEpsilon));
end;

function _NotZero(const r: double; aEpsilon: double = _EPSILON): boolean;
begin
  result := abs(r) > aEpsilon;
end;

function _Parallel(const Point1, Point2, Point3, Point4: TlicgCoor; const Epsilon: double = _EPSILON): Boolean;
begin
  Result := Parallel(Point1.x, Point1.y, Point2.x, Point2.y, Point3.x, Point3.y, Point4.x, Point4.y, Epsilon);
end;

function _IsEqual_RightDirection(const CoorArray1, CoorArray2: IlicgVector;
  const start1, start2, length1, length2: integer; const aEpsilon: double =
  _EPSILON): boolean; overload;
var
  i: integer;
begin
  result := false;
  if (length1 = length2) and (length1 > 0) then
  begin
    result := true;
    for i := 0 to Length1 - 1 do
    begin
      if not _IsEqual(CoorArray1[start1 + i], CoorArray2[start2 + i], aEpsilon) then
      begin
        result := false;
        break;
      end;
    end;
  end;
end;

function _IsEqual_RightDirection(const CoorArray1, CoorArray2: IlicgVector;
  const length1, length2: integer; const aEpsilon: double = _EPSILON):
  boolean; overload;
begin
  result := _IsEqual_RightDirection(CoorArray1, CoorArray2, 0, 0, length1,
    length2, aEpsilon);
end;

function _IsEqual_ReverseDirection(const CoorArray1, CoorArray2: IlicgVector;
  const start1, start2, length1, length2: integer; const aEpsilon: double =
  _EPSILON): boolean; overload;
var
  i: integer;
begin
  result := false;
  if (length1 = length2) and (length1 > 0) then
  begin
    result := true;
    for i := 0 to Length1 - 1 do
    begin  // soyle
      if not _IsEqual(CoorArray1[start1 + i], CoorArray2[start2 + Length1 - 1
        - i], aEpsilon) then
      begin
        result := false;
        break;
      end;
    end;
  end;
end;

function _GetZValueFromTriangle(const x, y: double; T: TlicgTriangle3D): double;
var
  point: TlicgCoor3D;
  T2D: TlicgTriangle;
  Line: TlicgLine3D;
begin

  T2D[1].X := T[1].X;
  T2D[1].Y := T[1].Y;
  T2D[2].X := T[2].X;
  T2D[2].Y := T[2].Y;
  T2D[3].X := T[3].X;
  T2D[3].Y := T[3].Y;

   { T[1] min, T[3] max degerini tutar. }
  Line[1].X := x;
  Line[1].Y := y;
  Line[1].Z := T[1].Z - 1; // 1, tampon icin.

  Line[2].X := x;
  Line[2].Y := y;
  Line[2].Z := T[3].Z + 1;

  point.X := 0;
  point.Y := 0;
  point.Z := 0;

  IntersectionPoint(Line, T, point);
  Result := point.Z;
end;

function _GetZValueFromTriangle(const x, y: double; const x1, y1, z1, x2, y2, z2,
  x3, y3, z3: double): double;
var
  T: TlicgTriangle3D;
begin
  T[1].X := x1;
  T[1].Y := y1;
  T[1].Z := z1;

  T[2].X := x2;
  T[2].Y := y2;
  T[2].Z := z2;

  T[3].X := x3;
  T[3].Y := y3;
  T[3].Z := z3;

  _SortTriangle(T);
  result := _GetZValueFromTriangle(x, y, T);
end;

procedure _SortTriangle(var T: TlicgTriangle3D);
var
  I: Integer;
  APoint: TlicgCoor3D;
  AChanged: Boolean;
begin
  AChanged := True;
  while AChanged do
  begin
    AChanged := False;
    for I := 1 to 2 do
    begin
      if (T[I].Z > T[I + 1].Z) then
      begin
        AChanged := True;
        APoint := T[I];
        T[I] := T[I + 1];
        T[I + 1] := APoint;
      end;
    end;
  end;
end;

{ Bir plane denkleminin Ax + By + Cz + D = 0 oldugunu dusundugumuzde, fonksiyon, verilen ucgenin duzlemini A,B,C,D degerleri ile dondurur. }
procedure _GetPlaneEquation(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double;
  out A, B, C, D: double);
begin
  A := y1 * (z2 - z3) + y2 * (z3 - z1) + y3 * (z1 - z2);
  B := z1 * (x2 - x3) + z2 * (x3 - x1) + z3 * (x1 - x2);
  C := x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2);
  D := -(x1 * (y2 * z3 - y3 * z2)) - (x2 * (y3 * z1 - y1 * z3)) - (x3 * (y1 * z2 - y2 * z1));
end;

procedure _GetPlaneEquation(const T: TlicgTriangle3D; out A, B, C, D: double); overload;
begin
  _GetPlaneEquation(T[1].x, T[1].y, T[1].z, T[2].x, T[2].y, T[2].z, T[3].x, T[3].y, T[3].z, A, B, C, D);
end;

procedure _GetNormalLine(const T: TlicgTriangle3D; out P1, P2: TlicgCoor3D);
var
  A, B, C, D: double;
begin
  Centroid(T[1].x, T[1].y, T[2].x, T[2].y, T[3].x, T[3].y, P1.x, P1.y);
  P1.Z := _GetZValueFromTriangle(P1.x, P1.y, T);

  _GetPlaneEquation(T, A, B, C, D);

  _GetNormalLine(P1.X, P1.Y, P1.Z, A, B, C, P2.X, P2.Y, P2.Z);
end;

procedure _GetNormalLine(const Cx, Cy, Cz: double; const A, B, C: double; out x, y, z: double); overload;
var
  prop: double;
begin
  prop := 0;
  if not ((A = 0) and (B = 0) and (C = 0)) then
  begin
      // P2 noktasi hesaplaniyor...

    if c <> 0 then
    begin  // Normalin yonu yukari baksin diye, Z yi, bir birim artiriyorum.
      z := Cz + 1;
      prop := 1 / C;

      X := Cx + prop * A;

      Y := Cy + prop * B;
    end
      { c=0 burada ucgenin altini ustunu ayirt edemiyorum. Normal, Z duzlemine paralel gidiyor.
        O yuzden, bu donguye girecek olan verinin, gercek hayatta hic karsimiza cikmayacagini da dusunmekle birlikte, burada, Yuzeyin normali ya da ters normalini
        aliyor olma riskini sakliyorum.
        Prop degerini, degeri 0 olmayan ilk duzlem degiskeninden aliyorum. }
    else
    begin
      z := Cz;

      if A = 0 then
        X := Cx
      else
      begin
        X := Cx + 1;
        prop := 1 / A;
      end;

      Y := Cy + prop * B;
    end;
  end;
end;

function _GetSlopeAngle(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double): double;
var
  A, B, C, D: double;
begin
  _GetPlaneEquation(x3, y3, z3, x2, y2, z2, x1, y1, z1, A, B, C, D);
  result := _GetSlopeAngle(A, B, C, D);
end;

function _GetSlopeAngle(const A, B, C, D: double): double;
var
  cosG, temp: double;
begin
  result := 0;
  temp := sqrt((A * A) + (B * B) + (C * C));
  if temp <> 0 then
  begin
    cosG := C / temp;
    result := _RadianToDegree(arcCos(cosG));
  end;
end;

function _GetMeanHeight(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double): double;
var
  Cx, Cy: double;
begin
  Centroid(x1, y1, x2, y2, x3, y3, Cx, Cy);
  result := _GetZValueFromTriangle(Cx, Cy, x1, y1, z1, x2, y2, z2, x3, y3, z3);
end;

procedure _Centroid(const x1,y1,x2,y2:double; out x,y: double);
begin
  x := (x1 + x2) * 0.5;
  y := (y1 + y2) * 0.5;
end;
function _Centroid(const P1,P2:TlicgCoor):TlicgCoor;
begin
  _Centroid(P1.x,P1.y,P2.x,P2.y,Result.x,Result.y);
end;

procedure _Centroid(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double; out x, y,z: double);
begin
  Centroid(x1, y1, x2, y2, x3, y3, x, y);
  z := _GetZValueFromTriangle(x, y, x1, y1, z1, x2, y2, z2, x3, y3, z3);
end;

function _Centroid(const CoorArray: IlicgVector; start, finish: integer): TlicgCoor;
var
  i: Integer;
  j: Integer;
  asum: double;
  term: double;
begin
  Result.X := 0.0;
  Result.Y := 0.0;

  if (finish - start) < 2 then
    Exit;

  asum := 0.0;
  j := (finish - start);

  for i := 0 to (finish - start) do
  begin
    term := ((CoorArray[j].x * CoorArray[i].y) - (CoorArray[j].y * CoorArray[i].x));
    asum := asum + term;
    Result.X := Result.X + (CoorArray[j].x + CoorArray[i].x) * term;
    Result.Y := Result.Y + (CoorArray[j].y + CoorArray[i].y) * term;
    j := i;
  end;

  if _NotEqual(asum, 0.0) then
  begin
    Result.x := Result.x / (3.0 * asum);
    Result.y := Result.y / (3.0 * asum);
  end;
end;

procedure _GetMaxSlopeLine(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double; out
  line_x1, line_y1, line_z1, line_x2, line_y2, line_z2: double); overload;
var
  A, B, C, D: double;
  Cntrd: TlicgCoor3D;
  dist, t: double;
  cosG, tanG: double;
  Line: TlicgLine3D;
  payda: double;
begin
  _Centroid(x1, y1, z1, x2, y2, z2, x3, y3, z3, Cntrd.x, Cntrd.y, Cntrd.z);
  line_x1 := Cntrd.X;
  line_y1 := Cntrd.Y;
  line_z1 := Cntrd.Z;
  line_x2 := Cntrd.X;
  line_y2 := Cntrd.Y;
  line_z2 := Cntrd.Z;

   { Orta noktanin, kenarlardan birine olan en kisa dik uzakligi, bu ic-teget cemberin de yaricapi. }
  dist := Lider.CG.Com.FastGeo.MinimumDistanceFromPointToSegment(Cntrd.x, Cntrd.y,
    Cntrd.z, x1, y1, z1, x2, y2, z2);
  dist := Min(dist, Lider.CG.Com.FastGeo.MinimumDistanceFromPointToSegment(Cntrd.x,
    Cntrd.y, Cntrd.z, x2, y2, z2, x3, y3, z3));
  dist := Min(dist, Lider.CG.Com.FastGeo.MinimumDistanceFromPointToSegment(Cntrd.x,
    Cntrd.y, Cntrd.z, x3, y3, z3, x1, y1, z1));
  dist := 2 * dist / 3;

  _GetPlaneEquation(x1, y1, z1, x2, y2, z2, x3, y3, z3, A, B, C, D);
   { yuzeyin normali uzerinde, C noktasindan +/-t kadar uzaklikta iki nokta aliniyor.
     Bu t uzakligindaki iki noktanin x ve y sinin, ucgen uzerindeki izdusumleri, Bizim akis cizgisinin iki noktasina tekabul ediyor. }

  payda := sqrt(A * A + B * B + C * C);
  cosG := C / payda;
  tanG := Tan(arcCos(cosG));

  if tanG <> 0 then
  begin
    t := abs(dist / tanG);
      { x ve y yi bulmak icin, su iki denklemi kullaniyorum.
        x = x1 + t*cosA
        y = y1 + t*cosB }
    Line[1].X := Cntrd.X - t * (A / payda);
    Line[1].Y := Cntrd.Y - t * (B / payda);
    Line[1].Z := _GetZValueFromTriangle(Line[1].X, Line[1].Y, x1, y1, z1, x2, y2,
      z2, x3, y3, z3);

    Line[2].X := Cntrd.X + t * (A / payda);
    Line[2].Y := Cntrd.Y + t * (B / payda);
    Line[2].Z := _GetZValueFromTriangle(Line[2].X, Line[2].Y, x1, y1, z1, x2, y2,
      z2, x3, y3, z3);

    if Line[2].Z > Line[1].Z then
    begin
      line_x1 := Line[2].X;
      line_y1 := Line[2].Y;
      line_z1 := Line[2].Z;

      line_x2 := Line[1].X;
      line_y2 := Line[1].Y;
      line_z2 := Line[1].Z;
    end
    else
    begin
      line_x1 := Line[1].X;
      line_y1 := Line[1].Y;
      line_z1 := Line[1].Z;

      line_x2 := Line[2].X;
      line_y2 := Line[2].Y;
      line_z2 := Line[2].Z;
    end;
  end;
end;

procedure _GetMaxSlopeLine(const T: TlicgTriangle3D; out Line: TlicgLine3D); overload;
begin
  _GetMaxSlopeLine(T[1].X, T[1].Y, T[1].Z, T[2].X, T[2].Y, T[2].Z, T[3].X, T[3].Y,
    T[3].Z, Line[1].X, Line[1].Y, Line[1].Z, Line[2].X, Line[2].Y, Line[2].Z);
end;

function _GetAngleOfAspect(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double): double;
var
  A, B, C, D: double;
begin
  _GetPlaneEquation(x1, y1, z1, x2, y2, z2, x3, y3, z3, A, B, C, D);
  result := _GetAngleOfAspect(x1, y1, z1, x2, y2, z2, x3, y3, z3, A, B, C, D);
end;

function _GetAngleOfAspect(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double;
  const A, B, C, D: double): double; overload;
var
  NLine: TlicgLine3D; // NLine[1] ucgenin merkez moktasi, NLine[2], yukari bakan taraftaki bir nokta.
  dX, dY: double; // distance X and distance Y.
begin
  _Centroid(x1, y1, z1, x2, y2, z2, x3, y3, z3, NLine[1].X, NLine[1].Y, NLine[1].Z);
  _GetNormalLine(NLine[1].X, NLine[1].Y, NLine[1].Z, A, B, C, NLine[2].X, NLine[2].Y,
    NLine[2].Z);

  dX := NLine[2].X - NLine[1].X;
  dY := NLine[2].Y - NLine[1].Y;

  result := _RadianToDegree(ArcTan2(dY, dX));
  if result < 0 then
    result := 360 + result;
end;


{ HowManyPoints ile, noktalardan 3 unun mu, 4 unun mu dolu oldugu bilgisi geliyor. }
function _GetAvarageZValue(const x, y: double; const HowMany: integer; const P1,
  P2, P3, P4: TlicgCoor3D): double;
var
  T2D: TlicgTriangle;
  T3D: TlicgTriangle3D;
  MinZ, MaxZ: double;
  Line: TlicgLine3D;
  point: TlicgCoor3D;
  dist1, dist2, dist3: double;

  procedure FillTriangles(aP1, aP2, aP3: TlicgCoor3D);
  begin
    T3D[1] := aP1;
    T2D[1].X := aP1.X;
    T2D[1].Y := aP1.Y;
    MinZ := T3D[1].Z;
    MaxZ := T3D[1].Z;

    T3D[2] := aP2;
    T2D[2].X := aP2.X;
    T2D[2].Y := aP2.Y;
    MinZ := Min(MinZ, T3D[2].Z);
    MaxZ := Max(MaxZ, T3D[2].Z);

    T3D[3] := aP3;
    T2D[3].X := aP3.X;
    T2D[3].Y := aP3.Y;
    MinZ := Min(MinZ, T3D[3].Z);
    MaxZ := Max(MaxZ, T3D[3].Z);

    Line[1].Z := MinZ;
    Line[2].Z := MaxZ;
  end;

  function GetZFromTriangle(aP1, aP2, aP3: TlicgCoor3D): double;
  begin
    result := 0;
      { P1,P2,P3 ucgeni }
    FillTriangles(aP1, aP2, aP3);
    if PointInTriangle(x, y, T2D) then
    begin
      IntersectionPoint(Line, T3D, point);
      result := point.Z;
    end;
  end;

begin
  result := 0;

   { x,y kordinati, bu 4 noktadan olusmasi muhtemel 4 ucgenden birisinin sinirlari icerisine girerse, o ucgen uzerinden hesaplanan
   koordinat degerine atanir. }
  Line[1].X := x;
  Line[1].Y := y;
  Line[1].Z := 0;  // ucgenlerle birlikte dolacak.

  Line[2].X := x;
  Line[2].Y := y;
  Line[2].Z := 0;  // ucgenlerle birlikte dolacak.

  point := _NULL_COOR_3D;

  case HowMany of
    1:
      result := P1.Z; // o noktanin Z si doner.
    2:
      begin              { yine en yakindakininki doner. }
        if (_distance(x, y, P1.X, P1.Y) > _distance(x, y, P2.X, P2.Y)) then
          result := P2.Z
        else
          result := P1.Z;
      end;
    3:
      begin   // ucgenin icine dusuyor mu dusmuyor mu?
        result := GetZFromTriangle(P1, P2, P3);
        if result = 0 then
        begin
          dist1 := _distance(x, y, P1.X, P1.Y); // en yakin Z donsun.
          dist2 := _distance(x, y, P2.X, P2.Y); // en yakin Z donsun.
          dist3 := _distance(x, y, P3.X, P3.Y); // en yakin Z donsun.

          result := P1.Z;
          if (dist2 < dist1) and (dist2 < dist3) then
            result := P2.Z;
          if (dist3 < dist2) and (dist3 < dist1) then
            result := P3.Z;
        end;
      end;
    4:
      begin { iki ucgene ayrilir alan. Noktanin, ucgenlerin sadece birisinin icinde olacagi garantidir. }
        result := GetZFromTriangle(P1, P2, P3);
        if result = 0 then
          result := GetZFromTriangle(P1, P3, P4);
      end;
  end;
end;

function _RGBToColor(const _r, _g, _b: Byte): TColor;
begin
  Result := (_b shl 16) + (_g shl 8) + _r;
end;

procedure _ColorToRGB(const Color: TColor; out _r, _g, _b: byte);
begin
  _r := Color;
  _g := Color shr 8;
  _b := Color shr 16;
end;

function _FormatReal(r: double; sdigits: byte): double;
begin
  Result := RoundTo(r, 0 - sdigits);
end;

{ if Assigned(Ent) and (Ent.Geometry.Points.Count = 4) then begin.... den sonra cagirilmali }
procedure _EntityToTriangle(const Ent: IlicgEntity; var T: TlicgTriangle3D);
begin
  T[1].X := Ent.Geometry.Points.X[0];
  T[1].Y := Ent.Geometry.Points.Y[0];
  T[1].Z := Ent.Geometry.Points.Z[0];

  T[2].X := Ent.Geometry.Points.X[1];
  T[2].Y := Ent.Geometry.Points.Y[1];
  T[2].Z := Ent.Geometry.Points.Z[1];

  T[3].X := Ent.Geometry.Points.X[2];
  T[3].Y := Ent.Geometry.Points.Y[2];
  T[3].Z := Ent.Geometry.Points.Z[2];
end;

{ if first point is not equal to last point, then add last point }

procedure _NormalizePolygon(Entity: IlicgEntity);
var
  n, Idx1, Idx2: integer;
  Found: boolean;
begin
  with (Entity) do
  begin
    if EntityID = idPolygon then
    begin
      if Geometry.Points.PartCount < 2 then
      begin
        if not Lider.CG.Com.Lib.FuzzEqualPoint2D(Geometry.Points[0], Geometry.Points[Geometry.Points.Count - 1]) then
          Geometry.Points.Add(Geometry.Points[0]);
      end
      else
      begin
        n := 0;
        repeat
          Found := False;
          Idx1 := Geometry.Points.Parts[n];
          if n < Geometry.Points.PartCount - 1 then
            Idx2 := Geometry.Points.Parts[n + 1] - 1
          else
            Idx2 := Geometry.Points.Count - 1;
          repeat
            if not Lider.CG.Com.Lib.FuzzEqualPoint2D(Geometry.Points[Idx1],
              Geometry.Points[Idx2]) then
            begin
              if Idx2 < Geometry.Points.Count - 1 then
                Geometry.Points.Insert(Idx2 + 1, Geometry.Points[Idx1].x,
                  Geometry.Points[Idx1].y)
              else
                Geometry.Points.Add(Geometry.Points[Idx1].x, Geometry.Points[Idx1].y);
              Found := True;
              Inc(n);
              Break;
            end;
            Inc(n);
            if n >= Geometry.Points.PartCount then
              Break;
            Idx1 := Geometry.Points.Parts[n];
            if n < Geometry.Points.PartCount - 1 then
              Idx2 := Geometry.Points.Parts[n + 1] - 1
            else
              Idx2 := Geometry.Points.Count - 1;
          until False;
        until (not Found) or (n >= Geometry.Points.PartCount);
      end;
    end;
  end;
end;

function _IsEqual_ReverseDirection(const CoorArray1, CoorArray2: IlicgVector;
  const length1, length2: integer; const aEpsilon: double = _EPSILON):
  boolean; overload;
begin
  result := _IsEqual_ReverseDirection(CoorArray1, CoorArray2, 0, 0, length1,
    length2, aEpsilon);
end;

function _IsPolygonConvex(const Polygon: IlicgVector): boolean;
var
  i: Integer;
  j: Integer;
  k: Integer;
  InitialOrientation: Integer;
  CurrentOrientation: Integer;
  FirstTime: Boolean;
begin
  Result := False;
  if Polygon.Count < 3 then
    Exit;
  FirstTime := True;
  InitialOrientation := Orientation(Polygon[Polygon.Count - 2], Polygon[Polygon.Count
    - 1], Polygon[0].x, Polygon[0].y);
  j := 0;
  k := Polygon.Count - 1;
  for i := 1 to Polygon.Count - 1 do
  begin
    CurrentOrientation := Orientation(Polygon[k], Polygon[j], Polygon[i].x, Polygon[i].y);
    if (InitialOrientation = CollinearOrientation) and (InitialOrientation <>
      CurrentOrientation) and FirstTime then
    begin
      InitialOrientation := CurrentOrientation;
      FirstTime := False;
    end
    else if (InitialOrientation <> CurrentOrientation) and (CurrentOrientation
      <> CollinearOrientation) then
      Exit;
    k := j;
    j := i;
  end;
  Result := True;
end;

function _Intersect(const CoorArray1, CoorArray2: IlicgVector; start1, length1,
  start2, length2: integer): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  if (Length1 > 1) and (Length2 > 1) then
  begin
    for i := start1 to Length1 - 2 do
    begin
      for j := start2 to Length2 - 2 do
      begin
        if Intersect(CoorArray1[i], CoorArray1[i + 1], CoorArray2[j],
          CoorArray2[j + 1]) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

{ iki kordinat dizisi, uzayin herhangibir yerinde herhangi ayni noktadan geciyorlarsa :- 'true'.}
function _Intersect(const CoorArray1, CoorArray2: IlicgVector; length1,
  length2: integer): Boolean;
begin
  result := _Intersect(CoorArray1, CoorArray2, 0, length1, 0, length2);
end;

function _Intersect(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  ConsiderInfinity: boolean = false): Boolean; overload;
begin
  result := _Intersect(Line1A.X, Line1A.Y, Line1B.X, Line1B.Y, Line2A.X, Line2A.Y,
    Line2B.X, Line2B.Y, ConsiderInfinity);
end;

function _Intersect(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; out P:
  TlicgCoor; const ConsiderInfinity: boolean = false): Boolean;
var
  Line1, Line2: TlicgLine;
begin
  result := false;
  if ConsiderInfinity then
  begin
    if _Intersect(Line1A, Line1B, Line2A, Line2B, true) then
    begin
      Line1[1] := Line1A;
      Line1[2] := Line1B;
      Line2[1] := Line2A;
      Line2[2] := Line2B;

      P := IntersectionPoint(Line1, Line2);
      result := true;
    end
  end
  else
  begin
    result := Intersect(Line1A.X, Line1A.Y, Line1B.X, Line1B.Y, Line2A.X, Line2A.Y,
      Line2B.X, Line2B.Y, P.X, P.Y);
  end;
end;

{ X=0, Y=0, z=Height duzleminin, x1, y1, z1, x2, y2, z2 dogrusunu kestigi noktayi ariyoruz.}
function _Intersect(const Height, x1, y1, z1, x2, y2, z2: double; out P:
  TlicgCoor3D): boolean;
var
  adjacent: double; // hipotenus uzunlugu
  newAdjacent: double;
  opposite: double; // komsu kenar uzunlugu
  newOpposite: double;
  p2: TlicgCoor;
begin
  result := false;

  if Height = z1 then
  begin
    result := true;
    P.x := x1;
    P.y := y1;
    P.z := z1;
    exit;
  end;
  if Height = z2 then
  begin
    result := true;
    P.x := x2;
    P.y := y2;
    P.z := z2;
    exit;
  end;

  if (Max(z1, z2) > Height) and (Height > Min(z1, z2)) then
  begin
    opposite := abs(z2 - z1);
    newOpposite := abs(Height - z1);
    adjacent := distance(x1, y1, x2, y2);

    if opposite <> 0 then
    begin
         { AA, sorunu ucgenlerin benzerliginden cozdum. }
      newAdjacent := (Adjacent * NewOpposite) / opposite;
         { 1. noktadan, p.x ve p.y ye olan uzunlugu bulduk. Simdi o uzunluktaki kordinatlari getirmek gerekiyor. }

      if _CoorAtDistance(newAdjacent, x1, y1, x2, y2, p2) then
      begin
        P.X := p2.X;
        P.Y := p2.Y;
        P.Z := Height;
        result := true;
      end;
    end;
  end;
end;

function _Intersect(const A, B: TlicgCoor; const T2D: TlicgTriangle): boolean; overload;
begin
  Result := _Intersect(A, B, T2D[1], T2D[2]) or _Intersect(A, B, T2D[2], T2D[3])
    or _Intersect(A, B, T2D[3], T2D[1]) or PointInTriangle(A, T2D) or
    PointInTriangle(B, T2D);
end;

function _Intersect(const Line1A, Line1B, Line2A, Line2B: TlicgCoor3D): boolean; overload;
begin
  result := Intersect(Line1A, Line1B, Line2A, Line2B);
end;

{ A->B vektoru uzerinde, A ya en yakindan en uzak olanina dogru siralar. }
function _CrossesTriangle(const A, B: TlicgCoor; const T2D: TlicgTriangle; out P1,
  P2: TlicgCoor): boolean; overload;
var
  Triangle, IntPoints: IlicgVector;
begin
  Triangle := Licad.CreateEntityFactory.MakeVector(_2D, 4, false);
  IntPoints := Licad.CreateEntityFactory.MakeVector(_2D, 0);

  Triangle.Add(T2D[1].X, T2D[1].Y);
  Triangle.Add(T2D[2].X, T2D[2].Y);
  Triangle.Add(T2D[3].X, T2D[3].Y);
  Triangle.Add(T2D[1].X, T2D[1].Y);

  _IntersectionPoints(A, B, Triangle, 4, IntPoints, itCrossTouch);

  case IntPoints.Count of
    0:
      begin
             { kesismiyorsa, ama ikisi de ucgenin icindeyse, yine crosses. }
        if PointInTriangle(A, T2D) and PointInTriangle(B, T2D) then
        begin
          result := true;
          P1 := A;
          P2 := B;
        end;
      end;
    1:
      begin  // Ya teðettir ya da line üçgenin içinde sonlanmýþtýr.
        if PointInTriangle(B, T2D) then
        begin
          P1 := IntPoints.Coors2D[0];
          P2 := B;
          result := true;
        end
        else if PointInTriangle(A, T2D) then
        begin // A içerdedir.
          P1 := A;
          P2 := IntPoints.Coors2D[0];
          result := true;
        end
        else
        begin // teðettir...
          P1 := IntPoints.Coors2D[0];
          P2 := IntPoints.Coors2D[0];
          result := true;
        end;
      end;
    2:
      begin  // sýraya sokacaðýz.
        P1 := IntPoints.Coors2D[0];
        P2 := IntPoints.Coors2D[1];
        result := true;
      end
  else
    result := false;
  end;
end;


{ A->B vektoru uzerinde, A ya en yakindan en uzak olanina dogru siralar. }
(*function  _CrossesTriangle(const A, B: TlicgCoor; const T2D: TlicgTriangle; out P1, P2: TlicgCoor ): boolean; overload;
var
   P    : TlicgCoor;
   Line : TlicgLine;
   cnt  : integer;

begin
   result := false;
   cnt    := 0;

   if _Intersect( A, B, T2D[1], T2D[2], P ) then begin
      inc(cnt);
      Line[cnt] := P;
   end;

   if _Intersect( A, B, T2D[2], T2D[3], P ) then begin
      inc(cnt);
      Line[cnt] := P;
   end;

   if _Intersect( A, B, T2D[3], T2D[1], P ) then begin
      if (cnt<2) then begin
        inc(cnt);
        Line[cnt] := P;
      end
      else begin
         if _IsEqual(Line[1], Line[2]) then
            Line[cnt] := P;
      end;
   end;

   case cnt of
      0 : begin
             { kesismiyorsa, ama ikisi de ucgenin icindeyse, yine crosses. }
             if PointInTriangle( A, T2D ) and PointInTriangle( B, T2D ) then begin
                result := true;
                P1 := A;
                P2 := B;
             end;
          end;
      1 : begin  // son noktayý da ekleyeceðiz.
             if PointInTriangle( B, T2D ) then begin
                P1 := Line[1];
                P2 := B;
                result := true;
             end
             else begin // A içerdedir.
                P1 := A;
                P2 := Line[1];
                result := true;
             end;
          end;
      2 : begin  // sýraya sokacaðýz.
            result := true;
            if _Distance(A, Line[1]) > _Distance(A, Line[2]) then begin
               P1 := Line[2];
               P2 := Line[1];
            end
            else begin
               P1 := Line[1];
               P2 := Line[2];
            end;
          end
       else
          result := false;
   end;
end; *)

function _Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: double; const
  ConsiderInfinity: boolean = false; const aEpsilon: double = _EPSILON):
  boolean; overload;
var
  C1, C2, C3, C4: TlicgCoor;
begin
  if ConsiderInfinity then
  begin
    C1.X := x1;
    C1.Y := y1;
    C2.X := x2;
    C2.Y := y2;
    C3.X := x3;
    C3.Y := y3;
    C4.X := x4;
    C4.Y := y4;

    if not _Line1OnLine2(C1, C2, C3, C4) then
    begin
      result := not _Parallel(C1, C2, C3, C4, aEpsilon);
    end
    else
    begin  // ustustelerse...
      result := true;
    end;
  end
  else
  begin
    result := Intersect(x1, y1, x2, y2, x3, y3, x4, y4);
  end;
end;

function _CoorAtDistance(const newdistance, x1, y1, x2, y2: double; out P:
  TlicgCoor): boolean;
var
  distance: double;
  proportion: double;
  dif: double;
begin
  result := false;
  if (newdistance > 0) then
  begin
    distance := _Distance(x1, y1, x2, y2);
    if distance > 0 then
    begin
      if distance > newdistance then
      begin
        proportion := distance / newdistance;
        dif := x2 - x1;
        if dif = 0 then
          p.x := x1
        else
        begin
          P.X := x1 + (dif / proportion);
        end;
        dif := y2 - y1;
        if dif = 0 then
          p.y := y1
        else
        begin
          P.Y := y1 + (dif / proportion)
        end;
        result := true;
      end
      else if distance = newdistance then
      begin
        result := true;
        P.X := x2;
        P.Y := y2;
      end;
    end;
  end;
end;

function _SelfIntersecting(const CoorArray: IlicgVector; const startIndex,
  endIndex: integer): Boolean; overload;
var
  I, J, LastPoint: Integer;
  length: integer;
begin
  Result := False;

  length := endIndex - startIndex + 1;

  if (Length > 2) then
  begin
      {Shortcut - kendi uzerinden geri donen Polyline.}
    if not ((Length = 3) and _IsEqual(CoorArray[startIndex], CoorArray[endIndex])) then
    begin
      LastPoint := endIndex - 1;
      for i := startIndex to endIndex do
      begin
        for j := i + 2 to LastPoint do
        begin
          if (i <> j) and (not ((i = startIndex) and (j = LastPoint))) then
          begin
            if Intersect(CoorArray.Coors3D[i].X, CoorArray.Coors3D[i].Y,
              CoorArray.Coors3D[i].Z, CoorArray.Coors3D[i + 1].X, CoorArray.Coors3D
              [i + 1].Y, CoorArray.Coors3D[i + 1].Z, CoorArray.Coors3D[j].X,
              CoorArray.Coors3D[j].Y, CoorArray.Coors3D[j].Z, CoorArray.Coors3D
              [j + 1].X, CoorArray.Coors3D[j + 1].Y, CoorArray.Coors3D[j + 1].Z) then
            begin
              Result := True;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function _SegmentsOnSameVector(const Line: IlicgVector; const start, length:
  integer; const aEpsilon: double = _EPSILON): boolean;
var
  i: integer;
begin
  result := true;
  if length = 2 then
    Exit;

  for i := start + 1 to Length - 2 do
  begin
    if RobustCollinear(Line[i], Line[i - 1], Line[i + 1], aEpsilon) then
    begin
      result := false;
      break;
    end;
  end;
end;

function _SegmentsOnSameVector(const Line: IlicgVector; const length: integer;
  const aEpsilon: double = _EPSILON): boolean;
begin
  result := _SegmentsOnSameVector(Line, 0, length, aEpsilon);
end;

// FastGeodakinin kopyasi... FastGeodaki epsilon katsayisini gecirmemize izin vermedigi icin kopyaladim.
function _IsPointCollinear(const A, B, P: TlicgCoor; const aEpsilon: double =
  _EPSILON): boolean;
begin
  result := _IsPointCollinear(A, B, P.X, P.Y, aEpsilon);
end;

function _IsPointCollinear(const Ax, Ay, Bx, By: double; const Px, Py: double;
  const aEpsilon: double = _EPSILON): boolean; overload;
begin
  Result := False;

  if (((Ax <= Px + aEpsilon) and (Px <= Bx + aEpsilon)) or ((Bx <= Px + aEpsilon)
    and (Px <= Ax + aEpsilon))) and (((Ay <= Py + aEpsilon) and (Py <= By +
    aEpsilon)) or ((By <= Py + aEpsilon) and (Py <= Ay + aEpsilon))) then
  begin
    Result := RobustCollinear(Ax, Ay, Bx, By, Px, Py, aEpsilon);
  end;
end;

function _IsPointCollinear(const A, B: TlicgCoor; const Px, Py: double; const
  aEpsilon: double = _EPSILON): boolean; overload;
begin
  Result := _IsPointCollinear(A.X, A.Y, B.X, B.Y, Px, Py, aEpsilon);
end;

function _WhereIsPoint(const P: TlicgCoor; const Polygon: IlicgVector; const
  length: integer; const aEpsilon: double = _EPSILON): integer;
var
  i, intersectionCounter: integer;
begin
   { noktadan _NULL_POINT e bir cizgi ciziyoruz. Cizgi,
      - Eger Polygonu kesme sayisi cift olursa outside(0 dahil)
      - Eger tek olursa icindedir. }

  result := outsideIt;
  intersectionCounter := 0;

  for i := 0 to length - 2 do
  begin
    if _IsPointCollinear(Polygon[i], Polygon[i + 1], P, aEpsilon) then
    begin
      result := OnIt;
      exit;
    end
    else if Intersect(P, _NULL_COOR, Polygon[i], Polygon[i + 1]) then
    begin
      inc(intersectionCounter);
    end;
  end;

  if ((intersectionCounter mod 2) = 0) then
  begin // even
    result := outsideIt;
  end
  else
  begin // odd
    result := insideIt;
  end;
end;

function _WhereIsSegment(const A, B: TlicgCoor; const Polygon: IlicgVector; const
  length: integer; const aEpsilon: double = _EPSILON): integer;
var
  i: integer;
  intersectionCounterA, intersectionCounterB: integer;
  ICollinear, I_1Collinear: boolean;
  WhereIsA, WhereIsB: integer;
  AIsOn, BIsOn: integer;
  TouchOrCross: boolean;

   { buradan donecek deger, cross, insideTouch, outsideTouch }

  function Determine_If_TouchOrCross: integer;
  var
    intersectionPoints: IlicgVector;
    intersectionCounter: integer;
    tempCondition, Condition: integer; { insideIt, onIt, outsideIt degerleri alabilir. }
    Pnt: TlicgCoor;
    i: integer;
  begin
      // IntersectionCounter := 0;
    Condition := onIt;
      { IntersectionPoints i yaratmam gerekmiyor ama finalize etmem gerekiyor. }
    if _IntersectionPoints(A, B, Polygon, length, IntersectionPoints,
      itCrossTouch, true, false, aEpsilon) then
    begin
      for i := 0 to intersectionPoints.Count - 2 do
      begin
        Pnt := MidPoint(IntersectionPoints[i], IntersectionPoints[i + 1]);
        tempCondition := _WhereIsPoint(Pnt, Polygon, length, aEpsilon);

        if Condition = OnIt then
        begin
          Condition := tempCondition;
        end
        else if (tempCondition <> OnIt) and (Condition <> tempCondition) then
        begin
               { biri outside, biri inside demektir, bu da = crossesIt. }
          result := CrossesIt;
          exit;
        end;
      end;
         { Buraya kadar geldi ise, Cross yok demektir. Inside ya da outside touch var. }

      case Condition of
        OnIt:
          result := OnIt;
        InsideIt:
          result := InsideTouch;
        OutsideIt:
          result := OutsideTouch;
      end;
    end;
    IntersectionPoints := nil;
  end;

begin
  intersectionCounterA := 0;
  intersectionCounterB := 0;
  TouchOrCross := false;

  WhereIsA := undefined;
  WhereIsB := undefined;

  for i := 0 to length - 2 do
  begin
    if _IsPointCollinear(Polygon[i], Polygon[i + 1], A, aEpsilon) then
    begin
      WhereIsA := OnIt;
      AIsOn := i;
    end
    else if Intersect(A, _NULL_COOR, Polygon[i], Polygon[i + 1]) then
    begin
      inc(intersectionCounterA);
    end;
    if _IsPointCollinear(Polygon[i], Polygon[i + 1], B, aEpsilon) then
    begin
      WhereIsB := OnIt;
      BIsOn := i;
      if (WhereIsA = OnIt) and (AIsOn = BIsOn) then
      begin
        result := OnIt;
        exit;  // Polygonun uzerinde
      end;
    end
    else if Intersect(B, _NULL_COOR, Polygon[i], Polygon[i + 1]) then
    begin
      inc(intersectionCounterB);
    end;

    if ((WhereIsA <> OnIt) or not ((WhereIsA = OnIt) and (AIsOn = i))) and ((WhereIsB
      <> OnIt) or not ((WhereIsB = OnIt) and (BIsOn = i))) and Intersect(A, B,
      Polygon[i], Polygon[i + 1]) then
    begin
      ICollinear := _IsPointCollinear(A, B, Polygon[i], aEpsilon);
      I_1Collinear := _IsPointCollinear(A, B, Polygon[i + 1], aEpsilon);
      if not (ICollinear or I_1Collinear) then
      begin
        result := crossesIt;
        exit; // gercekten kesiyor.
      end
      else
      begin
        TouchOrCross := true; // polygonun eklemi uzerinden kesisme var. Bu, Cross ta da touch olabilir. Asagida karar verilecek.
      end;
    end;
  end;

  if (WhereIsA <> OnIt) then
  begin
    if ((intersectionCounterA mod 2) = 0) then
      WhereIsA := outsideIt
    else
      WhereIsA := insideIt;
  end;

  if (WhereIsB <> OnIt) then
  begin
    if ((intersectionCounterB mod 2) = 0) then
      WhereIsB := outsideIt
    else
      WhereIsB := insideIt;
  end;

  if TouchOrCross then
  begin
    { buraya kadar geldiginde, segmentin, ortasindan cross olma sansi kalmamistir.
      Ancak, polygonun KIRILMA NOKTALARINDAN 'cross' ya da 'touch' olma sansi hala vardir. }
    result := Determine_If_TouchOrCross;
  end
  else
  begin
    if (WhereIsA = OnIt) then
    begin
      if (WhereIsB = insideIt) then  {iceridedir... Icerden touch...}
        result := insideTouch
      else if (WhereIsB = OutsideIt) then
        result := OutsideTouch
      else if (WhereIsB = OnIt) then
        result := Determine_If_TouchOrCross;
    end;

    if (WhereIsB = OnIt) then
    begin
      if (WhereIsA = insideIt) then  {iceridedir... Icerden touch...}
        result := insideTouch
      else if (WhereIsA = OutsideIt) then
        result := OutsideTouch
      else if (WhereIsA = OnIt) then
        result := Determine_If_TouchOrCross;
    end;

    if (WhereIsA <> OnIt) and (WhereIsB <> OnIt) then
    begin
      if (WhereIsA <> WhereIsB) then
        result := crossesIt
      else  // esitlerse, o zaman sonuc, o degerdir.
        result := WhereIsA;
    end;
  end;
end;

function _WhereIsSegment(const AX, AY, BX, BY: double; const Polygon:
  IlicgVector; const length: integer; const aEpsilon: double = _EPSILON):
  integer; overload;
var
  A, B: TlicgCoor;
begin
  A.X := AX;
  A.Y := AY;
  B.X := BX;
  B.Y := BY;

  result := _WhereIsSegment(A, B, Polygon, length, aEpsilon);
end;

function _PointOnLine(const Px, Py: double; const CoorArray: IlicgVector; const
  Length: integer; const aEpsilon: double = _EPSILON): integer;
var
  i: Integer;
begin
  Result := -1;
  if (Length > 0) then
  begin
    for i := 0 to Length - 2 do
    begin
      if ((CoorArray[i].y <= Py) and (Py <= CoorArray[i + 1].y)) or ((CoorArray
        [i + 1].y <= Py) and (Py <= CoorArray[i].y)) then
      begin
        if _IsPointCollinear(CoorArray[i], CoorArray[i + 1], Px, Py, aEpsilon) then
        begin
          Result := i;
          Exit;
        end;
      end;
    end;
  end;
end;

function _PointOnLine(const P: TlicgCoor; const CoorArray: IlicgVector; const
  Length: integer; const aEpsilon: double = _EPSILON): integer; overload;
begin
  result := _PointOnLine(P.X, P.Y, CoorArray, Length, aEpsilon);
end;

function _ClosestSegmentInPart(const X, Y: double; const Coors: IlicgVector;
  start_Index, end_Index: integer; var P1, P2: integer; var minDist: double): boolean;
var
  i: integer;
  D: Double;
begin
  P1 := -1;
  P2 := -1;

  result := false;
  if end_Index - start_Index < 1 then
    exit;

  try
    P1 := start_Index;
    P2 := start_Index + 1;

    MinDist := MinimumDistanceFromPointToSegment(X, Y, Coors.Coors2D[start_Index].X,
      Coors.Coors2D[P1].Y, Coors.Coors2D[P2].X, Coors.Coors2D[start_Index + 1].Y);

    for i := start_Index + 1 to end_Index - 1 do
    begin
      D := MinimumDistanceFromPointToSegment(X, Y, Coors.Coors2D[i].X, Coors.Coors2D
        [i].Y, Coors.Coors2D[i + 1].X, Coors.Coors2D[i + 1].Y);
      if D < MinDist then
      begin
        MinDist := D;
        P1 := i;
        P2 := i + 1;
      end;
    end;
  finally
    Result := (P1 <> -1) and (P2 <> -1);
  end;
end;

function _ClosestSegmentInCoorArray(const X, Y: double; const Coors:
  IlicgVector; var P1, P2: integer; var MinDist: double): boolean;
var
  a1, a2: integer;
  i, start_Index, end_Index: integer;
  d: double;
begin
  P1 := -1;
  P2 := -1;
  if Coors.PartCount > 0 then
  begin
    start_Index := Coors.Parts[0];
    if Coors.PartCount = 1 then
      end_Index := Coors.Count - 1
    else
      end_Index := Coors.Parts[1] - 1;

    if _ClosestSegmentInPart(X, Y, Coors, start_Index, end_Index, P1, P2, MinDist) then
    begin

      for i := 1 to Coors.PartCount - 1 do
      begin
        start_Index := Coors.Parts[i];
        if i = Coors.PartCount - 1 then
          end_Index := Coors.Count - 1
        else
          end_Index := Coors.Parts[i + 1] - 1;

        if _ClosestSegmentInPart(X, Y, Coors, start_Index, end_Index, a1, a2, d) then
        begin
          if d < MinDist then
          begin
            MinDist := d;
            P1 := a1;
            P2 := a2;
          end;
        end;
      end;

    end;
  end;
  result := (P1 <> -1) and (P2 <> -1);
end;


{  _cross, _equal, _overlap, _touch, _contains, _within, _district }
function _LineRelate(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  aEpsilon: double = _EPSILON): integer;
var
  MBB1, MBB2: TlicgExtent4D;
begin
  result := _district;

  MBB1.LowerLeft.X := Min(Line1A.X, Line1B.X);
  MBB1.LowerLeft.Y := Min(Line1A.Y, Line1B.Y);
  MBB1.UpperRight.X := Max(Line1A.X, Line1B.X);
  MBB1.UpperRight.Y := Max(Line1A.Y, Line1B.Y);

  MBB2.LowerLeft.X := Min(Line2A.X, Line2B.X);
  MBB2.LowerLeft.Y := Min(Line2A.Y, Line2B.Y);
  MBB2.UpperRight.X := Max(Line2A.X, Line2B.X);
  MBB2.UpperRight.Y := Max(Line2A.Y, Line2B.Y);

  if _MBBIntersect(MBB1, MBB2) then
  begin
      {Equal...}
    if (_IsEqual(Line1A, Line2A, aEpsilon) and _IsEqual(Line1B, Line2B, aEpsilon))
      or (_IsEqual(Line1A, Line2B, aEpsilon) and _IsEqual(Line1B, Line2A, aEpsilon)) then
    begin
      result := _Equal;
      exit;
    end;
      {Within...}
    if _MBB1WithinMBB2(MBB1, MBB2) then
    begin
      if _Line1OnLine2(Line1A, Line1B, Line2A, Line2B, aEpsilon) then
      begin
        result := _within;
        exit;
      end;
    end;
      {Contains...}
    if _MBB1WithinMBB2(MBB2, MBB1) then
    begin
      if _Line1OnLine2(Line2A, Line2B, Line1A, Line1B, aEpsilon) then
      begin
        result := _contains;
        exit;
      end;
    end;
      {Overlaps...}
    if _LinesOverlap(Line1A, Line1B, Line2A, Line2B, aEpsilon) then
    begin
      result := _overlap;
      exit;
    end;
      {Touch...}
    if (_IsPointCollinear(Line1A, Line1B, Line2A, aEpsilon) or _IsPointCollinear
      (Line1A, Line1B, Line2B, aEpsilon) or _IsPointCollinear(Line2A, Line2B,
      Line1B, aEpsilon) or _IsPointCollinear(Line2A, Line2B, Line1B, aEpsilon)) then
    begin
      result := _touch;
      exit;
    end;
      {Cross...}
    if _Intersect(Line1A, Line1B, Line2A, Line2B) then
    begin
      result := _cross;
      exit;
    end;
  end;
end;

function _Line1OnLine2(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  aEpsilon: double = _EPSILON): boolean;
begin
  result := _IsPointCollinear(Line2A, Line2B, Line1A, aEpsilon) and
    _IsPointCollinear(Line2A, Line2B, Line1B, aEpsilon);
end;

function _IntersectionPoints(const A, B: TlicgCoor; const Polyline: IlicgVector;
  const length: integer; var IntersectionPoints: IlicgVector; IntType:
  TIntersectionType; const ordered: boolean = true; const ConsiderInfinity:
  boolean = false; const aEpsilon: double = _EPSILON): boolean;
var
  i, initial, PartIndex: integer; // initial index of IntersectionPoints... Is used in ordering.
  pass: boolean; { eger bir ya da iki nokta bir digerinin uzerinde ise, bu iki cizgi intersect edemezler -
                     ya cakisik ya da dokunuyorlardir.
                     bu durumda tekrar intersect metoduna girmemesi icin kullanilan boolean degisken...}
  Pnt: TlicgCoor;
  tmpbl: boolean;


   { _IntersectionPoints icin yardimci fonksiyon... }

  procedure AddToList(const IntPnt: TlicgCoor);

    function AlreadyInList: Boolean;
    var
      j: Integer;
    begin
      Result := False;
      for j := 0 to IntersectionPoints.Count - 1 do
      begin
        if _IsEqual(IntersectionPoints[j], IntPnt, aEpsilon) then
        begin
          result := true;
          exit;
        end;
      end;
    end;

  begin
    if not AlreadyInList then
    begin
      IntersectionPoints.Add(IntPnt.x, IntPnt.y);
    end;
  end;

  procedure Order;
  var
    i: integer;
    changed: boolean;
    distances: array of double;
    tempF: double;
    tempPnt: TlicgCoor;
  begin
    SetLength(distances, IntersectionPoints.Count);

    for i := Initial to IntersectionPoints.Count - 1 do
    begin
      distances[i] := Distance(A, IntersectionPoints[i]);
    end;

    changed := true;
    while changed do
    begin
      changed := false;
      for i := initial to IntersectionPoints.Count - 2 do
      begin
        if distances[i] > distances[i + 1] then
        begin
          changed := true;
          tempF := distances[i];
          tempPnt := IntersectionPoints[i];

          distances[i] := distances[i + 1];
          IntersectionPoints[i] := IntersectionPoints[i + 1];

          distances[i + 1] := tempF;
          IntersectionPoints[i + 1] := tempPnt;
        end;
      end;
    end;
    finalize(distances);
  end;

begin
  if not Assigned(IntersectionPoints) then
    IntersectionPoints := Licad.CreateEntityFactory.MakeVector(_3D, 0);

  tmpbl := IntersectionPoints.DisableEvents;
  IntersectionPoints.DisableEvents := true;

  try

    initial := IntersectionPoints.Count;
    PartIndex := 0;                             //(* Oguz Ekleme *)
    for i := 0 to Length - 2 do
    begin
      if Polyline.PartEnd(PartIndex) = i then
      begin
        inc(PartIndex);
        Continue;
      end;                                    //(* Oguz Ekleme Bitis *)

      pass := false;

      if _IsPointCollinear(Polyline[i], Polyline[i + 1], A, aEpsilon) then
      begin
        pass := true;
        if (IntType = itTouch) or (IntType = itCrossTouch) then
          AddToList(A);
      end;
      if _IsPointCollinear(Polyline[i], Polyline[i + 1], B, aEpsilon) then
      begin
        pass := true;
        if (IntType = itTouch) or (IntType = itCrossTouch) then
          AddToList(B);
      end;
      if _IsPointCollinear(A, B, Polyline[i], aEpsilon) then
      begin
        pass := true;
        if (IntType = itTouch) or (IntType = itCrossTouch) then
          AddToList(Polyline[i]);
      end;
      if _IsPointCollinear(A, B, Polyline[i + 1], aEpsilon) then
      begin
        pass := true;
        if (IntType = itTouch) or (IntType = itCrossTouch) then
          AddToList(Polyline[i + 1]);
      end;

        { eger uc noktalarin cakisma durumu yoksa ve kesisme varsa... }
      if (not pass) and (not _Parallel(A, B, Polyline[i], Polyline[i + 1],
        aEpsilon)) and _Intersect(A, B, Polyline[i], Polyline[i + 1], Pnt,
        ConsiderInfinity) and ((IntType = itCross) or (IntType = itCrossTouch)) then
      begin
           { kesisme noktasini ekle. }
        AddToList(Pnt);
      end;
    end;

    if ordered and (IntersectionPoints.Count > 1) then
    begin
      Order;
    end;
    result := IntersectionPoints.Count > initial;
  except
  end;
  IntersectionPoints.DisableEvents := tmpbl;
end;

function _IntersectionPoints(const Polyline1, Polyline2: IlicgVector; const
  Length1, Length2: integer; var IntersectionPoints: IlicgVector; IntType:
  TIntersectionType; const ordered: boolean = true; const ConsiderInfinity:
  boolean = false; const aEpsilon: double = _EPSILON): boolean;
var
  i, IntersectionCount, PartIndex: integer;
  tmpbl: boolean;
begin
  tmpbl := IntersectionPoints.DisableEvents;
  IntersectionPoints.DisableEvents := true;

  try
    if not Assigned(IntersectionPoints) then
      IntersectionPoints := Licad.CreateEntityFactory.MakeVector(_3D, 0);

    IntersectionCount := IntersectionPoints.Count;
    PartIndex := 0;
    for i := 0 to Length1 - 2 do                      //(* Oguz Ekleme *)
    begin
      if Polyline1.PartEnd(PartIndex) = i then
      begin
        inc(PartIndex);
        Continue;
      end;                                            //(* Oguz Ekleme Bitis *)
      _IntersectionPoints(Polyline1[i], Polyline1[i + 1], Polyline2,
        Length2, IntersectionPoints, IntType, ordered, ConsiderInfinity, aEpsilon);
    end;
    result := IntersectionPoints.Count > IntersectionCount;
  except
  end;
  IntersectionPoints.DisableEvents := tmpbl;
end;

function VectIntersect(Vector1, Vector2, IntersectVector: IlicgVector;
  StrictlyBetween: Boolean = true; IntersectionType: TIntersectionType =
  itCrossTouch): Boolean;
begin
  IntersectVector.Clear;
  result := _IntersectionPoints(Vector1, Vector2, Vector1.Count, Vector2.Count,
    IntersectVector, IntersectionType, false, not StrictlyBetween);
end;

function _Polyline1OnPolyline2(const Line1, Line2: IlicgVector; const
  Length1, Length2: integer; const aEpsilon: double = _EPSILON): boolean;
var
  i, j: integer;
  onit, equal: boolean;
begin
  for i := 0 to length1 - 2 do
  begin
    OnIt := false;
    for j := 0 to Length2 - 2 do
    begin
      OnIt := _Line1OnLine2(Line1[i], Line1[i + 1], Line2[j], Line2[j + 1], aEpsilon);
      if OnIt then
        break;
    end;
    if not OnIt then
    begin
      break;
    end;
  end;
  if OnIt then
  begin // eger uzerinde ise, esit olup olmadigina bakilmali... sadece son noktalara.
    Equal := (_IsEqual(Line1[0], Line2[0], aEpsilon) and _IsEqual(Line1[length1
      - 1], Line2[length2 - 1], aEpsilon)) or (_IsEqual(Line1[length1 - 1],
      Line2[0], aEpsilon) and _IsEqual(Line1[0], Line2[length2 - 1], aEpsilon))
  end;

  result := OnIt and not Equal;
end;

function _LinesOverlap(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  aEpsilon: double = _EPSILON): boolean;
begin
  result := false;

   { eger hepsi coolinear ise.... }
  if RobustCollinear(Line1A, Line1B, Line2A, aEpsilon) and RobustCollinear(Line1A,
    Line1B, Line2B, aEpsilon) then
  begin
      { Iki line icin de, iki uctan birinin digerinin icinde, birinin de disinda olmasi gerekiyor... }
      { ilk line icin... Bir nokta digerinin icinde ve digeri disinda...}

    result := ((_IsPointCollinear(Line1A, Line1B, Line2A, aEpsilon) and not (_IsPointCollinear
      (Line1A, Line1B, Line2B, aEpsilon))) or (_IsPointCollinear(Line1A, Line1B,
      Line2B, aEpsilon) and not (_IsPointCollinear(Line1A, Line1B, Line2A,
      aEpsilon)))) and ((_IsPointCollinear(Line2A, Line2B, Line1A, aEpsilon) and
      not (_IsPointCollinear(Line2A, Line2B, Line1B, aEpsilon))) or (_IsPointCollinear
      (Line2A, Line2B, Line1B, aEpsilon) and not (_IsPointCollinear(Line2A,
      Line2B, Line1A, aEpsilon))));
  end;
end;

{ cakisiklar mi? }
function _LinesAdjacent(const Line1A, Line1B, Line2A, Line2B: TlicgCoor; const
  aEpsilon: double = _EPSILON): boolean;
begin
  result := false;

   { eger hepsi coolinear ise.... }
  if RobustCollinear(Line1A, Line1B, Line2A, aEpsilon) and RobustCollinear(Line1A,
    Line1B, Line2B, aEpsilon) then
  begin
      { en azindan birisi bir digerinin uzerinde ise o zaman adjacent. }
    if (not (_IsPointCollinear(Line1A, Line1B, Line2A, aEpsilon) and
      _IsPointCollinear(Line1A, Line1B, Line2B, aEpsilon))) or (not (_IsPointCollinear
      (Line2A, Line2B, Line1A, aEpsilon) and _IsPointCollinear(Line2A, Line2B,
      Line1B, aEpsilon))) or (_IsEqual(Line1A, Line2A) and _IsEqual(Line1B,
      Line2B)) or // ya da esitlerse..
      (_IsEqual(Line1B, Line2A) and _IsEqual(Line1A, Line2B)) then
    begin
      result := true;
    end;
  end;
end;

{ Overlap compares two geometries of the same dimension and returns t (TRUE)
  if their intersection set results in a geometry different from both but of the same dimension.}
function _PolylinesOverlap(const Polyline1, Polyline2: IlicgVector; const
  Length1, Length2: integer; const aEpsilon: double = _EPSILON): boolean;
var
  OneAdjacent, FirstIsDifferent, secondIsDifferent: boolean;
  i, j: integer;
begin
   { - En az bir segment (not _SegmentsOnSameVec
   or and not equal) olmamali
     - eger bir kordinatlari esitse, bir segmentin bir kordinati, digerinin kordinatlari arasinda olmali. ) }
  OneAdjacent := false;
  FirstIsDifferent := false;
  secondIsDifferent := false;

  if (Length1 > 1) and (Length2 > 1) then
  begin
    for i := 0 to Length1 - 2 do
    begin
      for j := 0 to Length2 - 2 do
      begin
        OneAdjacent := _LinesAdjacent(Polyline1[i], Polyline1[i + 1],
          Polyline2[j], Polyline2[j + 1], aEpsilon);
        if OneAdjacent then
          break;
      end;
      if OneAdjacent then
        break;
    end;

    for i := 0 to length1 - 1 do
    begin
      FirstIsDifferent := _PointOnLine(Polyline1[i], Polyline2, Length2,
        aEpsilon) = -1;
      if FirstIsDifferent then
        break;
    end;

    for i := 0 to length2 - 1 do
    begin
      SecondIsDifferent := _PointOnLine(Polyline2[i], Polyline1, Length1,
        aEpsilon) = -1;
      if SecondIsDifferent then
        break;
    end;
  end;
  result := OneAdjacent and FirstIsDifferent and SecondIsDifferent and (not
    _IsEqual(Polyline1, Polyline2, length1, length2, aEpsilon));
end;


{ en az bir segment kesismeli, ve o iki segment, ustuste olmamali. Iki line in kesismesinden, sadece 'bir nokta' cikabilir...
  kesisim noktasinin boundaryler olmasi kabul edilmiyor. o nedenle her kesisen nokta icin, 'NotTheBoundaries' fonksiyonu isleniyor. }
function _PolylinesCross(const Line1, Line2: IlicgVector; const Length1,
  Length2: integer; const aEpsilon: double = _EPSILON): boolean;
var
  i, j: integer;
  Line1Closed, Line2Closed: boolean;

  function DoTheyTouch: boolean; // kesisen noktanin boundary olup olmadigini dondurur.
  begin
    result := true;
      { ikisi de kapali ise boundaryleri olmadigindan := true...
        ama herhangi birisi acik ise, boundary de kesisim soz konusu oluyor. }
    if (not Line1Closed) then
    begin
      result := ((i = 0) and _IsPointCollinear(Line2[j], Line2[j + 1], Line1[0],
        aEpsilon) and not RobustCollinear(Line2[j], Line2[j + 1], Line1[1],
        aEpsilon)) or ((i = length1 - 2) and _IsPointCollinear(Line2[j], Line2[j
        + 1], Line1[length1 - 1], aEpsilon) and not RobustCollinear(Line2[j],
        Line2[j + 1], Line1[length1 - 2], aEpsilon));
      if result then
        exit;
    end;

    if (not Line2Closed) then
    begin
      result := ((j = 0) and _IsPointCollinear(Line1[i], Line1[i + 1], Line2[0],
        aEpsilon) and not RobustCollinear(Line1[i], Line1[i + 1], Line2[1],
        aEpsilon)) or ((j = length2 - 2) and _IsPointCollinear(Line1[i], Line1[i
        + 1], Line2[length2 - 1], aEpsilon) and not RobustCollinear(Line1[i],
        Line1[i + 1], Line2[length2 - 2], aEpsilon));
      if result then
        exit;
    end;
  end;

begin
  result := false;
  Line1Closed := _IsClosed(Line1, Length1);
  Line2Closed := _IsClosed(Line2, Length2);

  for i := 0 to Length1 - 2 do
  begin
    for j := 0 to Length2 - 2 do
    begin
      if Intersect(Line1[i], Line1[i + 1], Line2[j], Line2[j + 1]) and (not
        DoTheyTouch) then
      begin
        if _LinesAdjacent(Line1[i], Line1[i + 1], Line2[j], Line2[j + 1], aEpsilon) then
        begin
               { herhangi bir yerde segment overlap ya da equality var ise, patlar... }
          result := false;
          exit;
        end;
        result := true;
      end;
    end;
  end;
end;

function _Polyline_Touch_Polyline(const Polyline1, Polyline2:
  IlicgVector; const Length1, Length2: integer; const aEpsilon: double =
  _EPSILON): boolean;
var
  OnWhichLine: integer;

  function DoesSegmentTouch(const Line1A, Line1B: TlicgCoor; const Polyline2:
    IlicgVector; const length2: integer): integer;
  var
    i, rel: integer;
  begin
    result := -1;
    for i := 0 to length2 - 1 do
    begin
      rel := _LineRelate(Line1A, Line1B, Polyline2[i], Polyline2[i + 1], aEpsilon);
      case rel of
        _touch:
          result := _touch;
        _district:
          result := _district;
        _cross, _equal, _overlap, _contains, _within:
          begin
            result := -1;
            exit;
          end;
      end;
    end;
  end;

begin
  if (length1 > 1) and (length2 > 1) and (not _IsClosed(Polyline1, Length1))
    and (not _IsClosed(Polyline2, Length2)) then
  begin
    result := (_PointOnLine(Polyline1[0], Polyline2, Length2, aEpsilon) > -1)
      or (_PointOnLine(Polyline1[Length1 - 1], Polyline2, Length2, aEpsilon)
      > -1) or (_PointOnLine(Polyline2[0], Polyline1, Length1, aEpsilon) > -
      1) or (_PointOnLine(Polyline2[Length2 - 1], Polyline1, Length1, aEpsilon) > -1);
    if result then
    begin // eger dokunuyorsa bir sekilde.

      if (Length1 > 3) and (length2 > 3) then
      begin
        if _Intersect(Polyline1, Polyline2, 1, Length1 - 2, 1, length2 - 2) then
        begin
               { herhangi bir kesisme var ise, touch degil, cross ediyordur. }
          result := false;
          exit;
        end;
      end;

         { iki dogrunun da ilk ve son segmentlerini, diger Line ile _LineRelate fonksiyonuna sokacagiz. }
         // line1 ilk segment
      if (DoesSegmentTouch(Polyline1[0], Polyline1[1], Polyline2, length2)
        = -1) then
      begin
        result := false;
        exit;
      end;

         // line2 ilk segment
      if (DoesSegmentTouch(Polyline2[0], Polyline2[1], Polyline1, length1)
        = -1) then
      begin
        result := false;
        exit;
      end;

         // line1 son segment
      if (length1 > 2) and (DoesSegmentTouch(Polyline1[length1 - 2],
        Polyline1[length1 - 1], Polyline2, length2) = -1) then
      begin
        result := false;
        exit;
      end;

         // line2 son segment
      if (length2 > 2) and (DoesSegmentTouch(Polyline2[length2 - 2],
        Polyline2[length2 - 1], Polyline1, length1) = -1) then
      begin
        result := false;
        exit;
      end;
    end;
  end
  else if (_IsClosed(Polyline1, Length1)) and (not _IsClosed(Polyline2, Length2)) then
  begin
      { Polyline1 acik Polyline2 kapali ise, Polyline2 nin uc noktalarindan en azindan birisi Polyline1 uzerinde ise
        ve Polyline2 yi kesmiyorsa, o zaman Touch = true  }
    result := ((_PointOnLine(Polyline2[0], Polyline1, Length1, aEpsilon) > -
      1) or (_PointOnLine(Polyline2[length2 - 1], Polyline1, Length1,
      aEpsilon) > -1)) and (not _PolylinesCross(Polyline1, Polyline2,
      Length1, Length2, aEpsilon));
  end
  else if (not _IsClosed(Polyline1, Length1)) and _IsClosed(Polyline2, Length2) then
  begin
      { Polyline1 acik Polyline2 kapali ise, Polyline2 nin uc noktalarindan en azindan birisi Polyline1 uzerinde ise
        ve Polyline2 yi kesmiyorsa, o zaman Touch = true  }
    result := ((_PointOnLine(Polyline1[0], Polyline1, Length1, aEpsilon) > -
      1) or (_PointOnLine(Polyline2[length2 - 1], Polyline1, Length1,
      aEpsilon) > -1)) and (not _PolylinesCross(Polyline1, Polyline2,
      Length1, Length2, aEpsilon));
  end;
end;

function _MBBIntersect(const MBB1, MBB2: TlicgExtent4D): boolean; // iki MBB birbiriyle kesisiyor mu?
begin
  result := RectangleToRectangleIntersect(MBB1.LowerLeft.X, MBB1.LowerLeft.Y,
    MBB1.UpperRight.X, MBB1.UpperRight.Y, MBB2.LowerLeft.X, MBB2.LowerLeft.Y,
    MBB2.UpperRight.X, MBB2.UpperRight.Y);
end;

function _MBB1WithinMBB2(const MBB1, MBB2: TlicgExtent4D): boolean;    // MBB1, MBB2 in icinde mi?
begin
  result := RectangleWithinRectangle(MBB1.LowerLeft.X, MBB1.LowerLeft.Y, MBB1.UpperRight.X,
    MBB1.UpperRight.Y, MBB2.LowerLeft.X, MBB2.LowerLeft.Y, MBB2.UpperRight.X,
    MBB2.UpperRight.Y);
end;

function _PointInMBB(const P: TlicgCoor; const MBB: TlicgExtent4D): boolean;
begin
  result := (P.X >= MBB.LowerLeft.X) and (P.X <= MBB.UpperRight.X) and (P.Y >=
    MBB.LowerLeft.Y) and (P.Y <= MBB.UpperRight.Y);
end;

function _Area(const Polygon: IlicgVector; const Length: integer): double; overload;
var
  i, j: Integer;
begin
  Result := 0.0;
  if _IsClosed(Polygon, Length) then
  begin
    j := Length - 2;
    for i := 0 to Length - 2 do
    begin
      Result := Result + ((Polygon[j].y * Polygon[i].x) - (Polygon[j].x * Polygon[i].y));
      j := i;
    end;
    Result := Result * 0.5;
  end;
end;

function _Area(const x1, y1, x2, y2, x3, y3: double): double; overload;
begin

  result := Area(x1, y1, x2, y2, x3, y3);

end;

function _Area(const x1, y1, z1, x2, y2, z2, x3, y3, z3: double): double; overload;
begin

  result := Area(x1, y1, z1, x2, y2, z2, x3, y3, z3);

end;

function _SimplePolygonOrientation(const CoorArray: IlicgVector; const Length:
  integer): Integer;
var
  i, anchor, prevpos, postpos, OpenLength: integer;

  function Orientation(const x1, y1, x2, y2, Px, Py: double): Integer;
  var
    Orin: double;
  begin
      (* Determinant of the 3 Geometry.Points *)
    Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

    if Orin < 0.0 then
      Result := LeftHandSide          (* Orientaion is to the left-hand side  *)
    else if Orin > 0.0 then
      Result := RightHandSide         (* Orientaion is to the right-hand side *)
    else
      Result := CollinearOrientation; (* Orientaion is neutral aka collinear  *)
  end;

begin
  Result := 0;
  if not _IsClosed(CoorArray, Length) then
    Exit;

  OpenLength := Length - 1;
  anchor := 0;
  for i := 1 to OpenLength - 1 do
  begin
    if CoorArray[i].x > CoorArray[anchor].x then
      anchor := i
    else if (CoorArray[i].x = CoorArray[anchor].x) and (CoorArray[i].y <
      CoorArray[anchor].y) then
      anchor := i;
  end;
  postpos := (anchor + 1) mod OpenLength;
  prevpos := anchor - 1;
  if prevpos < 0 then
      // PrevPos negatif olduðu zaman OpenLength ile toplanmalý;
      // eðer çýkarýlýrsa (kodun fastGeo'daki halindeki gibi)  index sýnýrý aþýldýðýndan hatalý sonuç üretiliyor.
    prevpos := OpenLength + prevpos;

  Result := Orientation(CoorArray[prevpos].x, CoorArray[prevpos].Y, CoorArray
    [postpos].X, CoorArray[postpos].Y, CoorArray[anchor].x, CoorArray[anchor].y);
end;

procedure _ChangeOrientation(CoorArray: IlicgVector; const Length: integer); overload;
var
  temp: TlicgCoor4D;
  i, k: integer;
begin
  if Length > 1 then
  begin
    k := Floor((Length - 1) / 2);
    for i := 0 to k do
    begin
      temp := CoorArray.Coors4D[i];
      CoorArray.Coors4D[i] := CoorArray.Coors4D[Length - i - 1];
      CoorArray.Coors4D[Length - i - 1] := temp;
    end;
  end;
end;

procedure _ChangeOrientation(CoorArray: IlicgVector; startIndex, endIndex:
  integer); overload;
var
  temp: TlicgCoor4D;
  i, k: integer;
begin
  if (startIndex < endIndex) then
  begin
    k := Floor((endIndex - startIndex) / 2);

    for i := 0 to k do
    begin
      temp := CoorArray.Coors4D[startIndex + i];
      CoorArray.Coors4D[startIndex + i] := CoorArray.Coors4D[endIndex - i];
      CoorArray.Coors4D[endIndex - i] := temp;
    end;
  end;
end;

function _IsCounterClockWise(CoorArray: IlicgVector; startIndex, endIndex:
  integer): Boolean;
// FindLR finds the lowest, rightmost vertex of poly.

  function FindLR: Integer;
  var
    cnt: Integer;
    min, tmp: TlicgCoor;
  begin
    min.X := CoorArray[startIndex].X;
    min.Y := CoorArray[startIndex].Y;

    Result := 0;
    for cnt := startIndex to endIndex do
    begin
      tmp := CoorArray[cnt];
      if (tmp.Y < min.Y) or ((tmp.Y = min.Y) and (tmp.X > min.X)) then
      begin
        Result := cnt;
        min.X := tmp.X;
        min.Y := tmp.Y;
      end;
    end;
  end;

  function Ccw(m: Integer): Integer;
  var
    area2: Double;
    m1, n: Integer;
    a, b, c: TlicgCoor;
  begin
    n := endIndex + 1;
    m1 := (m + (n - 1)) mod n;
    // assign adjacents Geometry.Points
    a := CoorArray[m1];
    b := CoorArray[m];
    c := CoorArray[(m + 1) mod n];
    area2 := a.X * b.Y - a.Y * b.X + a.Y * c.X - a.X * c.Y + b.X * c.Y - c.X * b.Y;
    if area2 > 0 then
      result := 1
    else if area2 < 0 then
      result := -1
    else
      result := 0;
  end;

begin
  Result := (Ccw(FindLR) = 1);
end;


(* Determines wheter the vertices are in cw or ccw order. It finds
   the lowest and rightmost vertex, and computes the cross-product
   of the vectors along its incident edges *)

function _IsCounterClockWise(CoorArray: IlicgVector): Boolean;

// FindLR finds the lowest, rightmost vertex of poly.
  function FindLR: Integer;
  var
    cnt: Integer;
    min, tmp: TlicgCoor;
  begin
    min.X := CoorArray[0].X;
    min.Y := CoorArray[0].Y;
    Result := 0;
    for cnt := 0 to CoorArray.Count - 1 do
    begin
      tmp := CoorArray[cnt];
      if (tmp.Y < min.Y) or ((tmp.Y = min.Y) and (tmp.X > min.X)) then
      begin
        Result := cnt;
        min.X := tmp.X;
        min.Y := tmp.Y;
      end;
    end;
  end;

  function Ccw(m: Integer): Integer;
  var
    area2: Double;
    m1, n: Integer;
    a, b, c: TlicgCoor;
  begin
    n := CoorArray.Count;
    m1 := (m + (n - 1)) mod n;
    // assign adjacents Geometry.Points
    a := CoorArray[m1];
    b := CoorArray[m];
    c := CoorArray[(m + 1) mod n];
    area2 := a.X * b.Y - a.Y * b.X + a.Y * c.X - a.X * c.Y + b.X * c.Y - c.X * b.Y;
    if area2 > 0 then
      result := 1
    else if area2 < 0 then
      result := -1
    else
      result := 0;
  end;

begin
  Result := (Ccw(FindLR) = 1);
end;

// Semt açý hesabý
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
end; //Angle2DS

// Semt açý hesabý
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

end.


