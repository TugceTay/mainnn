unit Lider.CG.Com.GeometryInt;

{ <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
  << Çoðu Unit tarafýndan kullanýlan promitif veri tipleri bulunduran taným + interface dosyasý.    >>
  <<                                                                                                >>
  <<                                                                                                >>
  << Interface'de, nesnelerin boyutlarý (dimension) ne olursa olsun 2D olarak çalýþan fonksiyonlar  >>
  << bulunuyor. Bu fonksiyonlarýn açýklamalarýnda, "SADECE_2D" ibaresi bulunuyor.                   >>
  <<                                                                                                >>
  << !!!!!!!!!!!!!!!                                                                                >>
  << IlicgGeometry, IOGCGeometry'den devþirebilinirdi. Ama program içerisinde kullanýlan hep        >>
  << IlicgGeometry olduðundan, ve geometrik iþlemler sonunda IBGeometry dönmesinin iyi olacaðýndan, >>
  << buna þimdilik gerek görmedim.                                                                  >>
  << !!!!!!!!!!!!!!!                                                                                >>
  <<                                                                                                >>
  << OGC SFO hakkýnda :-                                                                            >>
  << OGC fonksiyonlarý birebir tanýmlanmamýþtýr, ama gerekli fonksiyonalite nesnelerde vardýr.      >>
  << OGC nin kitabýna göre tanýmý için bakýnýz : IntOGCGeometry.pas                                 >>
  <<                                                                                                >>
  << Bu dosya þu anda kullanýlmýyor, ama eðer objeler OGC nin ayný interface'lerde kullanýlmak      >>
  << istenirse, o durumda yine 'QueryInterface' kullanarak destek saðlanabilir.                     >>
  <<                                                                                                >>
  <<                                                                                                >>
  << ........Multi objeler Fenomeni.......                                                          >>
  << _Multi objeler, yaratýlýrken, Multi diye yaratýlmazlar. Herhangi bir tekil objeye part         >>
  << eklenmesi, ve objenin birden fazla obje taþýr hale gelmesi, mimari açýdan mümkün.              >>
  << Bu da, beraberinde þöyle bir pratiði getiriyor :- Objeler her daim tekil olarak yaratýlýyor.   >>
  << Ama kendisine eklenen ilk parttan sonra multi hale gelebilirler.                               >>
  << _Multi bir objenin, tekil olandan tek farký, GeometryType fonksiyonunda ortaya çýkýyor,        >>
  << Eðer objenin partlarý, birden büyükse GeometryType := GeometryType + 4 olarak döndürülür.      >>
  << Ki bu da, _Point2D yi örneðin, _MultiPoint2D olarak döndürmek demektir.                        >>
  <<                                                                                                >>
  <<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>}

interface

{$I Lider.CG.Com.Component.inc}

uses
  Types,
  Classes,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

const
   // GeometryGroup Sabitleri geometrinin boyutundan baðýmsýz olarak
  _None = 0;
  _Point = 1;
  _Polyline = 2;
  _Polygon = 3;
  _GeomCollection = 4;
  //_MultiPoint = 5;
  //_MultiPolyline = 6;
  //_MultiPolygon = 7;
  _Line = 8;
  _Triangle = 9;
  _Pafta = 10;
  _BlockInsert = 11;
   (*
  _Place = 12;
  _Spline = 13;
  _Arc = 14;

  _Ellipse = 15;
  _Rectangle = 16;
  _Circle = 17;    *)

   // dimension olmadan, nesnelerin base deðerleri... yüzler, onlar, birler basamaklarý
  _Place = 101;
  _Spline = 102;
  _Arc = 202;

  _Ellipse = 302;
  _Rectangle = 402;
  _Circle = 502;

  _PolyArc = 203;
  _PolyEllipse = 303;
  _PolyRectangle = 403;
  _MultiPlace = 105;
  _MultiSpline = 106;
  _MultiArc = 206;
  _MultiEllipse = 306;
  _MultiRectangle = 406;

  _MultiPolyArc = 207;
  _MultiPolyEllipse = 307;
  _MultiPolyRectangle = 407;

  (* GeometryType Sabitleri
     sabit degerlerin
      + birler basamagindaki deger seklin tipini, - point, Polyline, polygon
      + yuzler basamagindaki deger ise ozel obje ise, onun turunu - ellipse, spline
      + binler hanesindeki sayi seklin geometrik boyutunu - 2D, 3D, 4D... belirtiyor *)

  _Point2D = 2001;
  _Point3D = 3001;
  _Point4D = 4001;

  _Place2D = 2101;
  _Place3D = 3101;
  _Place4D = 4101;

  _Line2D = 2008;
  _Line3D = 3008;

  _Triangle2D = 2009;
  _Triangle3D = 3009;

  _Pafta2D = 2010;
  _Pafta3D = 3010;
  _Pafta4D = 4010;

  _BlockInsert2D = 2011;
  _BlockInsert3D = 3011;

  _Polyline2D = 2002;
  _Polyline3D = 3002;
  _Polyline4D = 4002;

   // x002 - Polyline'den gelenler
  _Spline2D = 2102;
  _Spline3D = 3102;
  _Spline4D = 4102;

  _Arc2D = 2202;
  _Arc3D = 3202;
  _Arc4D = 4202;

  _Ellipse2D = 2302;
  _Ellipse3D = 3302;
  _Ellipse4D = 4302;

  _Circle2D = 2502;
  _Circle3D = 3502;
  _Circle4D = 4502;

  _Rectangle2D = 2402;
  _Rectangle3D = 3402;
  _Rectangle4D = 4402;

  _Preview = 2412;

   // Text 2502, Text nesnelerinin cenerik kodu
  _VectorialFittedText = 3512;
  _VectorialText = 3522;
  _Text = 3532;
  _TextM = 3542;
  _TextFX = 3553;

   // Dimension 2104, dimension nesnelerinin cenerik kodu
  _HorizontalMeasurement = 2114;
  _VerticalMeasurement = 2124;
  _ParallelMeasurement = 2134;

  _Polygon2D = 2003;
  _Polygon3D = 3003;
  _Polygon4D = 4003;

  _PolyArc2D = 2203;
  _PolyArc3D = 3203;
  _PolyArc4D = 4203;

  _PolyEllipse2D = 2303;
  _PolyEllipse3D = 3303;
  _PolyEllipse4D = 4303;

  _PolyRectangle2D = 2403;
  _PolyRectangle3D = 3403;
  _PolyRectangle4D = 4403;

  _GeomCollection2D = 2004;
  _GeomCollection3D = 3004;
  _GeomCollection4D = 4004;

  //_MultiPoint2D = 2005;
  //_MultiPoint3D = 3005;
  //_MultiPoint4D = 4005;

  _MultiPlace2D = 2105;
  _MultiPlace3D = 3105;
  _MultiPlace4D = 4105;

  //_MultiPolyline2D = 2006;
  //_MultiPolyline3D = 3006;
  //_MultiPolyline4D = 4006;

   // x006 - MultiPolyline'den gelenler
  _MultiSpline2D = 2106;
  _MultiSpline3D = 3106;
  _MultiSpline4D = 4106;

  _MultiArc2D = 2206;
  _MultiArc3D = 3206;
  _MultiArc4D = 4206;

  _MultiEllipse2D = 2306;
  _MultiEllipse3D = 3306;
  _MultiEllipse4D = 4306;

  _MultiRectangle2D = 2406;
  _MultiRectangle3D = 3406;
  _MultiRectangle4D = 4406;

  //_MultiPolygon2D = 2007;
  //_MultiPolygon3D = 3007;
  //_MultiPolygon4D = 4007;

   // Implemente edilecek
  _MultiPolyArc2D = 2207;
  _MultiPolyArc3D = 3207;
  _MultiPolyArc4D = 4207;

  _MultiPolyEllipse2D = 2307;
  _MultiPolyEllipse3D = 3307;
  _MultiPolyEllipse4D = 4307;

  _MultiPolyRectangle2D = 2407;
  _MultiPolyRectangle3D = 3407;
  _MultiPolyRectangle4D = 4407;

type
  IlicgGeometry = interface
    ['{878F37CB-B040-4BB7-9EC6-B040C16DDFD9}']
    function Assign(const Source: IlicgGeometry): Boolean; stdcall;
    function GetID: Integer; stdcall;
    procedure SetID(Value: Integer); stdcall;
    function GetInfoID: Integer; stdcall;
    procedure SetInfoID(Value: Integer); stdcall;
    function GetDrawPoints: IlicgVector; stdcall;
    procedure SetDrawPoints(const Value: IlicgVector); stdcall;
    function GetDisableEvents: Boolean; stdcall;
    procedure SetDisableEvents(Value: Boolean); stdcall;

    procedure BeginUpdate; stdcall;
    procedure EndUpdate; stdcall;
    function Points: IlicgVector; stdcall;
    function GeometryType: Integer; stdcall;
    function GeometryGroup: Integer; stdcall;
    function IsValid: Boolean; stdcall;
    function GISDimension: Integer; stdcall;
    procedure UpdateExtension(Grapher: TObject = nil); stdcall;
    procedure SetExtension2D(Value: TlicgExtent); stdcall;

    // Edit Komutunda Kullanýlanlar Baþlama
    function GetControlPoints(Grapher: TObject = nil): IlicgVector; stdcall;
    procedure UpdateControlPoint(Index: Integer; const Value: TlicgCoor; Grapher: TObject = nil); stdcall;
    function GetControlPointType(Index: Integer): TlicgControlPointType; stdcall;
    function PointCode(const Pt: TlicgCoor; const Aperture: Double;
      var Distance: Double; SelectPickingInside: Boolean; UseDrawPoints: Boolean = True): Integer; stdcall;
    procedure MoveAndRotateControlPts(var MovePt, RotatePt: TlicgCoor; Grapher: TObject); stdcall;
    procedure UpdateExtensionFromControlPts(Grapher: TObject = nil); stdcall;
    // Edit Komutunda Kullanýlanlar Bitiþ

    // Transform Fonksiyonlarý Baþlama
    procedure ApplyTransform; stdcall;
    procedure ResetTransform; stdcall;
    procedure TranslateTransform(const Tx, Ty: Double); stdcall;
    procedure RotateTransform(const Angle: Double; const ReferencePt: TlicgCoor); stdcall;
    procedure ScaleTransform(const Sx, Sy: Double; const ReferencePt: TlicgCoor); stdcall;
    procedure MatrixTransform(const M: TlicgMatrix); stdcall;
    function GetTransformMatrix: TlicgMatrix; stdcall;
    function HasTransform: Boolean; stdcall;
    procedure SetTransformMatrix(const Matrix: TlicgMatrix); stdcall;
    // Transform Fonksiyonlarý Bitiþ

    // OGC IlicgGeometry Fonksiyonlarý Baþlama
    function Dimension: Integer; stdcall;
    function IsEmpty: Boolean; stdcall;
    function IsSimple: Boolean; stdcall;
    function Clone: IlicgGeometry; stdcall;
    function Envelope: IlicgGeometry; stdcall;
    function Extent: TlicgExtent; stdcall;
    function Extent4D: TlicgExtent4D; stdcall;
    procedure SetEmpty; stdcall;

    // Ýliþkiler, Baðlantýlar Relate Baþlama
    function InPoly(const ACoor: TlicgCoor): Boolean; stdcall; //Nokta Nesnenin içinde mi? (Kenarlar Hariç).
    function OnPoly(const ACoor: TlicgCoor): Boolean; stdcall; //Nokta Nesene Üstünde mi? (Kenarlarýn üstündemi, Kapalý alanýn içi Hariç)

    function Equality(const BGeometry: IlicgGeometry): Boolean; stdcall;
    function Disjoint(const BGeometry: IlicgGeometry): Boolean; stdcall;
    function Intersect(const BGeometry: IlicgGeometry): Boolean; stdcall;
    function Touch(const BGeometry: IlicgGeometry): Boolean; stdcall;
    function Cross(const BGeometry: IlicgGeometry): Boolean; stdcall;
    function Within(const BGeometry: IlicgGeometry): Boolean; stdcall;
    function Contains(const BGeometry: IlicgGeometry): Boolean; stdcall;
    function Overlap(const BGeometry: IlicgGeometry): Boolean; stdcall;
    function Relate(const BGeometry: IlicgGeometry; const A_de9im: string): Boolean; stdcall;
    // Ýliþkiler, Baðlantýlar Relate Bitiþ

    function MakeBuffer(const ADist: Double; const Api2_Points: Integer = 9): IlicgGeometry; stdcall;
    function MakeOffset(const ADist: Double; AJoinType: TlicgJoinType; AEndType: TlicgEndType; AMiterLimit: Double = -1; AArcTolerance: Double = -1): IlicgGeometry; stdcall;

    function ConvexHull(const BGeometry: IlicgGeometry = nil; const AFix: Boolean = True): IlicgGeometry; stdcall;
    function ConcaveHull(const BGeometry: IlicgGeometry = nil; const AAlpha: Double = 0; const AFix: Boolean = True): IlicgGeometry; stdcall;

    function Intersection(const BGeometry: IlicgGeometry): IlicgGeometry; stdcall;
    function Difference(const BGeometry: IlicgGeometry): IlicgGeometry; stdcall;
    function Distance(const BGeometry: IlicgGeometry): Double; stdcall;
    function SymmetricalDifference(const BGeometry: IlicgGeometry): IlicgGeometry; stdcall;
    function Union(const BGeometry: IlicgGeometry): IlicgGeometry; stdcall;
    function Centroid: TlicgCoor; stdcall;
    function CenterOfMass: TlicgCoor; stdcall;
    // OGC IlicgGeometry Fonksiyonlarý Bitiþ

    /// <summary>
    ///   ID deðerinde tabloya eklenen kaçýncý nesne olduðunun sayýsýný tutar.
    ///   Çoðu zaman RecNo ile ayný deðere sahip olabilir ama RecNo için
    ///   kullanmak hataya sebep olabilir. Çünkü yapýlan iþlemlere göre sürekli
    ///   RecNo deðiþkenlik gösterir. <u><b>ID = RecNo OLMAZ.</b></u> Bundan
    ///   dolayý <b>Entity güncellemek için</b> kullanmayýnýz. <b>Entityleri
    ///   bozabilir.</b>
    /// </summary>
    property ID: Integer read GetID write SetID;
    property InfoID: Integer read GetInfoID write SetInfoID;
    property DrawPoints: IlicgVector read GetDrawPoints write SetDrawPoints;
    property DisableEvents: Boolean read GetDisableEvents write SetDisableEvents;
  end;

  IlicgPoint = interface(IlicgGeometry)
    ['{D027AC24-9139-4746-8861-1815E1AE7EA9}']
    function GetPointCode: string; stdcall;
    procedure SetPointCode(value: string); stdcall;
    property PointCode: string read GetPointCode write SetPointCode;
  end;

  IlicgPolyline = interface(IlicgGeometry)
    ['{DADA55BF-967B-4B93-9AD3-E5D270BE1B88}']
   { Geometric Dimension : 1D
     Line, Polyline, LinearRing
     LinearRing : simple ve closed bir Polyline.}

    function ContainsArcs: Boolean; stdcall;
    function ContainsBeziers: Boolean; stdcall;
    function GetArcsStartIndex: IlicgIntegerList; stdcall;
    procedure SetArcsStartIndex(const Value: IlicgIntegerList); stdcall;
    property ArcsStartIndex: IlicgIntegerList read GetArcsStartIndex write
      SetArcsStartIndex;
    function GetBezierList: IlicgBezierPointList; stdcall;
    procedure SetBezierList(const Value: IlicgBezierPointList); stdcall;
    property BezierList: IlicgBezierPointList read GetBezierList write SetBezierList;
    procedure AddArcStart(Index: Integer); stdcall;
    procedure AddCurve(Index: Integer; DoUpdate: Boolean = True); stdcall;
    procedure AllToCurve(DoUpdate: Boolean = True); stdcall;
    procedure AllToLinear(DoUpdate: Boolean = True); stdcall;
    procedure LoadBezierAndArcs(Stream: TStream); stdcall;
    procedure SaveBezierAndArcs(Stream: TStream); stdcall;

    { OGC ISURFACE Fonksiyonlarý }
    function GetAreaParcel: Double; stdcall;
    procedure SetAreaParcel(Value: Double); stdcall;
    { OGC ISURFACE Fonksiyonlarý }

    {....<OGC ICURVE Fonksiyonlarý>......}
     {<OGC IMultiCurve Fonksiyonlarý>}
    function Length: Double; stdcall;
    function IsClosed: boolean; stdcall;
     {</OGC IMultiCurve Fonksiyonlarý>}
    function StartPoint: IlicgPoint;                       stdcall;
    function EndPoint: IlicgPoint;                         stdcall;
      // egrinin t. uzunlugunda bulunan nokta.
    function Value(const t: Double): IlicgPoint;         stdcall;
{.....</OGC ICURVE Fonksiyonlarý>......}

{....<OGC IPolyline Fonksiyonlarý>......}
    function NumPoints: integer; stdcall;
    function Point(index: integer): IlicgPoint;             stdcall;
{....</OGC IPolyline Fonksiyonlarý>......}

    function GetSmoothType: TlicgCurveType; stdcall;
    procedure SetSmoothType(const Value: TlicgCurveType); stdcall;
    property SmoothType: TlicgCurveType read GetSmoothType write SetSmoothType;
    property AreaParcel: Double read GetAreaParcel write SetAreaParcel;
  end;

  IlicgPolygon = interface(IlicgPolyline)
    ['{FE5CF8D8-0270-478B-9006-27CAFA4A906D}']
    {....<OGC ISURFACE Fonksiyonlarý>......}
    function SignedArea: Double; stdcall;
    function Area: Double; stdcall;

    function Centroid: TlicgCoor; stdcall;

      // yuzey uzerinde random bir nokta döndürür.
    function PointOnSurface: IlicgPoint; stdcall;


    {....<OGC IPOLYGON Fonksiyonlarý>......}
    function ExteriorRing: IlicgPolyline; stdcall;

    function NumRings: integer; stdcall;
    function Ring(index: integer): IlicgPolyline; stdcall;
    {....</OGC IPOLYGON Fonksiyonlarý>......}
    procedure AddInteriorRing(const ring: IlicgGeometry); stdcall;
    procedure AddExteriorRing(const ring: IlicgGeometry); stdcall;
  end;

  IlicgMultiCurve = interface(IlicgGeometry)
    ['{D1EB5B60-9AFA-4A74-BD9F-A05E14E1F5A1}']
   { 1D non-instantiable
      Her elemani simple ise = simle
      Her elemani closed ise = closed
      Topologically closed...}
    function IsClosed: boolean; stdcall;
    function Length: Double; stdcall;
  end;

  IlicgSpline = interface(IlicgMultiCurve)
    ['{731DDA13-CADF-40A2-81C9-7E933BCA6411}']
    procedure MakeCurvePoints; stdcall;
    function getCurvePoints: IlicgVector; stdcall;
    procedure setCurvePoints(const value: IlicgVector); stdcall;
    property CurvePoints: IlicgVector read getCurvePoints write setCurvePoints;
    procedure SetPointsInCurve(const Value: Word); stdcall;
    function GetPointsInCurve: Word; stdcall;
    property PointsInCurve: word read getPointsInCurve write SetPointsInCurve;
    function GetCurveType: TlicgCurveType; stdcall;
    procedure SetCurveType(const Value: TlicgCurveType); stdcall;
    property CurveType: TlicgCurveType read GetCurveType write SetCurveType;
      {
      function   InvisibleVertexCount  : integer; stdcall;
      function   InvisibleVertex(index : integer) : integer; stdcall;
      procedure  AddInvisibleVertex (vertex : integer); stdcall;
      procedure  DeleteInvisibleVertex (vertex : integer); stdcall;
      procedure  ClearInvisibleVertex; stdcall;
      }
  end;

   { IlicgPolyline ve IlicgPolygonla birlikte implemente edilen iki türü vardýr. Birisi
   içi dolu daireleri, diðeri ise içi boþ kutularý çemberleri temsil eder.
   IlicgPolygon olduðunda, çizilebilecek nesneler arc tipleri :- asPie, asChord
   IlicgPolyline ile birlikte implemente edilen arc tipleri :- asOpened, asPie, asChord}
  IlicgArc = interface(IlicgSpline)
    ['{73070016-69FD-482A-B98F-3C7EBEBCC756}']
    function GetCenter: TlicgCoor; stdcall;
    procedure SetCenter(const Value: TlicgCoor); stdcall;
    function GetRadius: Double; stdcall;
    procedure SetRadius(const Value: Double); stdcall;
    function GetArcStyle: TlicgArcStyle; stdcall;
    procedure SetArcStyle(const Value: TlicgArcStyle); stdcall;
    function Centroid: TlicgCoor; stdcall;
    function Area: Double; stdcall;
    function Length: Double; stdcall;
    function NormalizeAngle(const Angle: Double): Double; stdcall;
    procedure SetArc(const CX, CY, R, StartAng, NumRadians: Double; IsCounterClockWise: Boolean); stdcall;
    procedure CalcCenterRadius; stdcall;
    function IsColinear: Boolean; stdcall;
    function ArcStartAngle: Double; stdcall;
    function ArcEndAngle: Double; stdcall;
    function SweepAngle: Double; stdcall;
    property Center: TlicgCoor read GetCenter write SetCenter;
    property Radius: Double read GetRadius write SetRadius;
    property ArcStyle: TlicgArcStyle read GetArcStyle write SetArcStyle;
  end;

  IlicgCircle = interface(IlicgSpline)
    ['{84EBFC29-1C1E-4E4B-8054-C4D4639678BA}']
    function GetCenter: TlicgCoor; stdcall;
    procedure SetCenter(const Value: TlicgCoor); stdcall;
    function GetRadius: Double; stdcall;
    procedure SetRadius(const Value: Double); stdcall;
    function Centroid: TlicgCoor; stdcall;
    function Area: Double; stdcall;
    function Length: Double; stdcall;
    property Center: TlicgCoor read GetCenter write SetCenter;
    property Radius: Double read GetRadius write SetRadius;
  end;

   { IlicgPolyline ve IlicgPolygonla birlikte implemente edilen iki türü vardýr. Birisi
   içi dolu daireleri, diðeri ise içi boþ kutularý çemberleri temsil eder. }
  IlicgEllipse = interface
    ['{B0118124-132E-475A-A092-7EC4542D9EEA}']
    procedure MakeCurvePoints; stdcall;
    function GetRotangle: Double; stdcall;
    procedure SetRotangle(const Value: Double); stdcall;
    property Rotangle: Double read GetRotangle write SetRotangle;
    procedure SetPointsInCurve(const Value: Word); stdcall;
    function GetPointsInCurve: Word; stdcall;
    property PointsInCurve: word read getPointsInCurve write SetPointsInCurve;
    function getCurvePoints: IlicgVector; stdcall;
    procedure setCurvePoints(const value: IlicgVector); stdcall;
    property CurvePoints: IlicgVector read getCurvePoints write setCurvePoints;
  end;

   { IlicgPolyline ve IlicgPolygonla birlikte implemente edilen iki türü vardýr. Birisi
   içi dolu kutularý, diðeri ise içi boþ kutularý temsil eder. }
  IlicgRectangle = interface
    ['{48CCE405-556B-45E3-8EA3-3E3881A8766F}']
    function GetPolyPoints: IlicgVector; stdcall;
    procedure SetPolyPoints(value: IlicgVector); stdcall;
    function GetRotangle: Double; stdcall;
    procedure SetRotangle(Value: Double); stdcall;
    procedure SetRoundness(const Value: Double); stdcall;
    function GetRoundness: Double; stdcall;
    property PolyPoints: IlicgVector read getPolyPoints write setPolyPoints;
    property Rotangle: Double read GetRotangle write SetRotangle;
    property Roundness: Double read GetRoundness write SetRoundness;
  end;

  IlicgPlace = interface(IlicgGeometry)
    ['{352C29AA-45CB-4D9E-828A-6325E7E8CFE6}']
      { Bazý fonksiyonlarda (extension hesaplanmasý gibi.) SembolTool'a ait deðerler (height..) kullanýlýyor. }
    procedure SetSymbolTool(const Value: IInterface); stdcall;
    function GetText: string; stdcall;
    procedure SetText(const Value: string); stdcall;
      // Symboltool.Height - bir deðerde ise her ölçekte sýnýr deðerlerin yeniden hesaplanmasý gerekiyor.
    function CalcRealSizeExtension(Grapher: TObject): TlicgExtent; stdcall;
    property Text: string read GetText write SetText;
  end;

  IlicgBlockInsert = interface(IlicgGeometry)
    ['{EBBAC8A1-E2DC-4D46-984C-CD30C22C1F95}']
      { Bazý fonksiyonlarda (extension hesaplanmasý gibi.) SembolTool'a ait deðerler (height..) kullanýlýyor. }
    procedure SetBlockTool(const Value: IInterface); stdcall;
    function GetText: string; stdcall;
    procedure SetText(const Value: string); stdcall;
      // Blocktool.Height - bir deðerde ise her ölçekte sýnýr deðerlerin yeniden hesaplanmasý gerekiyor.
    function CalcRealSizeExtension(Grapher: TObject): TlicgExtent; stdcall;
    property Text: string read GetText write SetText;
  end;

  { The type of grouping actually done }
  TlicgGroupType = (grtNone, gtFitToPath, gtHatch, gtCotourLine);

  IlicgGroupGeometry = interface(IlicgGeometry)
    ['{D519BE9E-C9E2-4C27-90CA-AAE8EF7CF90F}']
    procedure ClearEntities; stdcall;
    procedure AddEntity(const Ent: IInterface); stdcall;
    procedure DeleteEntity(Index: Integer); stdcall;
    function GetGroupType: TlicgGroupType; stdcall;
    procedure SetGroupType(const Value: TlicgGroupType); stdcall;
    property GroupType: TlicgGroupType read GetGroupType write SetGroupType;
    function NumEntities: integer; stdcall;

     // IlicgEntity cinsinde interface döndürür.
    function Entity(index: integer): IInterface; stdcall;
  end;

  IlicgLine = interface(IlicgGeometry)
    ['{178D61FE-815D-42DE-9A3E-D97B663E5CF3}']
   { Geometric Dimension : 1D
     Line, Polyline, LinearRing
     LinearRing : simple ve closed bir Polyline.}
{....<OGC ICURVE Fonksiyonlarý>......}
     {<OGC IMultiCurve Fonksiyonlarý>}
    function Length: Double; stdcall;
    function IsClosed: boolean; stdcall;
     {</OGC IMultiCurve Fonksiyonlarý>}
    function StartPoint: IlicgPoint;                       stdcall;
    function EndPoint: IlicgPoint;                         stdcall;
      // egrinin t. uzunlugunda bulunan nokta.
    //function Value(const t: Double): IlicgPoint;         stdcall;
{.....</OGC ICURVE Fonksiyonlarý>......}

{....<OGC IPolyline Fonksiyonlarý>......}
    function NumPoints: integer; stdcall;
    function Point(index: integer): IlicgPoint;             stdcall;
  {....</OGC IPolyline Fonksiyonlarý>......}
  end;

  IlicgTriangle = interface(IlicgGeometry)
    ['{088836D7-2F1E-4802-9BFD-3671789A37CD}']
    { OGC IMultiCurve Fonksiyonlarý }
    function Length: Double; stdcall;
    function IsClosed: boolean; stdcall;

    { OGC IMultiCurve Fonksiyonlarý }
    function StartPoint: IlicgPoint; stdcall;
    function EndPoint: IlicgPoint; stdcall;

    { OGC IPolyline Fonksiyonlarý }
    function NumPoints: integer; stdcall;
    function Point(index: integer): IlicgPoint; stdcall;
    { OGC IPolyline Fonksiyonlarý }

    { OGC ISURFACE Fonksiyonlarý }
    function Area: Double; stdcall;
    function Centroid: TlicgCoor; stdcall;
    { OGC ISURFACE Fonksiyonlarý }
  end;

  IlicgPafta = interface(IlicgGeometry)
    ['{957EE35F-8A1D-4083-9D51-BC56F36D7A9A}']
    { OGC IMultiCurve Fonksiyonlarý }
    function Length: Double; stdcall;
    function IsClosed: boolean; stdcall;

    { OGC IMultiCurve Fonksiyonlarý }
    function StartPoint: IlicgPoint; stdcall;
    function EndPoint: IlicgPoint; stdcall;

    { OGC IPolyline Fonksiyonlarý }
    function NumPoints: integer; stdcall;
    function Point(index: integer): IlicgPoint; stdcall;
    { OGC IPolyline Fonksiyonlarý }

    { OGC ISURFACE Fonksiyonlarý }
    function Area: Double; stdcall;
    function Centroid: TlicgCoor; stdcall;
    { OGC ISURFACE Fonksiyonlarý }
    (*uður ekleme baþlama *)
    function GetScale: Integer; stdcall;
    procedure SetScale(Scale: Integer); stdcall;
    property Scale: Integer read GetScale write SetScale;
    function GetDOB: Integer; stdcall;
    procedure SetDOB(DOB: Integer); stdcall;
    property DOB: Integer read GetDOB write SetDOB;
    function GetLowerLeft: IlicgPoint; stdcall;
    procedure SetLowerLeft(LowerLeft: IlicgPoint); stdcall;
    property LowerLeft: IlicgPoint read GetLowerLeft write SetLowerLeft;
    function GetUpperRigth: IlicgPoint; stdcall;
    procedure SetUpperRigth(UpperRigth: IlicgPoint); stdcall;
    property UpperRigth: IlicgPoint read GetUpperRigth write SetUpperRigth;
    (*uður ekleme bitme*)
  end;

  TlicgTextBorderStyle = (tbsNone, tbsBanner, tbsCallout, tbsBulletLeader);

  PEzTextBorderStyle = ^TlicgTextBorderStyle;

const
  True_Type_Text_Cofficient = 8 / 5;

type
  IlicgTextValue = interface(IlicgGeometry)
    ['{2FE7A411-F7A0-4008-AFF8-38A8F017DE87}']
    function GetText: string; stdcall;
    procedure SetText(const Value: string); stdcall;
    property Text: string read GetText write SetText;
    (* uður ekleme
    function GetTextPos: TlicgTextPos; stdcall;
    procedure SetTextPos( value: TlicgTextPos); stdcall;
    property TextPos: TlicgTextPos read GetTextPos write SetTextPos;
    uður ekleme Bitiþ *)
  end;


  { Vectorial fonts }
  { one single character
    Every single character is enclosed in a (0,0) to (1,1)unit
    Coordinates in order to support transformation }
  IlicgVectorChar = interface
    ['{8A6A5E76-0CAF-45EF-A872-F25DA0322B2E}']
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure UpdateExtension; stdcall;
    function GetVector: IlicgVector; stdcall;
    property Vector: IlicgVector read GetVector;
    function GetExtension: TlicgExtent; stdcall;
    procedure SetExtension(Extent: TlicgExtent); stdcall;
    property Extension: TlicgExtent read GetExtension write SetExtension;
  end;

  { This class contains the list of chars that builds up the font typeface }
  IlicgVectorFont = interface
    ['{894119B4-66A7-402A-9F87-460899AB8619}']
    function FileName: string; stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure LoadFromFile(const FileName: string); stdcall;
    procedure SaveToFile(const FileName: string); stdcall;
    function GetTextExtension(const Value: AnsiString; const Height: Double; const ACharWidthFactor: Double = 1): TlicgExtent; stdcall;
    function GetFittedTextExtension(const Value: AnsiString; ACharSpacing: Double): TlicgExtent; stdcall;
    function GetChar(Index: Integer): IlicgVectorChar; stdcall;
    procedure SetChar(Index: Integer; const VectorChar: IlicgVectorChar); stdcall;
    property Chars[Index: Integer]: IlicgVectorChar read GetChar write SetChar;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
    property Name: string read GetName write SetName;
    procedure SetMaxChar(Value: Integer); stdcall;
    function GetMaxChar: Integer; stdcall;
    property MaxChar: Integer read GetMaxChar write SetMaxChar;
  end;

  IlicgVectorText = interface(IlicgTextValue)
    ['{237B08EE-FA4F-44DA-9008-4EA0D4C996D0}']
    procedure SetFontTool(Value: IInterface); stdcall;
    function GetTextExtension: TlicgExtent; stdcall;
    procedure PopulatePoints; stdcall;
    function getVectorFont: IlicgVectorFont; stdcall;
    procedure setVectorFont(const value: IlicgVectorFont); stdcall;
    property VectorFont: IlicgVectorFont read getVectorFont write setVectorFont;
    function GetCharSpacing: Double; stdcall;
    procedure SetCharSpacing(const Value: Double); stdcall;
    property CharSpacing: Double read GetCharSpacing write SetCharSpacing;
    function GetLineSpacing: Double; stdcall;
    procedure SetLineSpacing(const Value: Double); stdcall;
     { the interline spacing: 0.3 means 30% of text height }
    property LineSpacing: Double read GetLineSpacing write SetLineSpacing;

    // uður Ekleme baþlama
    function GetCharWidthFactor: Double; stdcall;
    procedure SetCharWidthFactor(const Value: Double); stdcall;
    property CharWidthFactor: Double read GetCharWidthFactor write SetCharWidthFactor;
    // uður Ekleme bitme
  end;

  IlicgVectorialFittedText = interface(IlicgVectorText)
    ['{47A7FDE3-8DBE-43CE-90D1-784FDFE3321D}']
    procedure DoNormalizedVector(const BasePt: TlicgCoor; V: IlicgVector); stdcall;
    function GetBasePoint: TlicgCoor; stdcall;
    procedure SetBasePoint(const Value: TlicgCoor); stdcall;
    property BasePoint: TlicgCoor read GetBasePoint write SetBasePoint;
    function GetPivot: TlicgCoor; stdcall;
    procedure SetPivot(const Value: TlicgCoor); stdcall;
    property Pivot: TlicgCoor read GetPivot write SetPivot;
    function GetTextWidth: Double; stdcall;
    procedure SetTextWidth(const Value: Double); stdcall;
    property TextWidth: Double read GetTextWidth write SetTextWidth;
    function GetTextBorderStyle: TlicgTextBorderStyle; stdcall;
    procedure setTextBorderStyle(value: TlicgTextBorderStyle); stdcall;
    property TextBorderStyle: TlicgTextBorderStyle read GetTextBorderStyle write setTextBorderStyle;
  end;

  TlicgTextHorzAlignment = (thaLeft, thaCenter, thaRight);

  TlicgTextVertAlignment = (tvaTop, tvaCenter, tvaBottom);

  IlicgVectorialText = interface(IlicgVectorText)
    ['{A85CD104-0E7B-4A29-B3C1-B95A8095956B}']
    procedure DoRotation; stdcall;
    procedure DoNormalizedVector(const LowerLeft, UpperRight: TlicgCoor); stdcall;
    procedure SetTextBox(const Value: TlicgExtent); stdcall;
    function GetTextBox: TlicgExtent; stdcall;
    property TextBox: TlicgExtent read GetTextBox write SetTextBox;
    function GetHorzAlignment: TlicgTextHorzAlignment; stdcall;
    procedure SetHorzAlignment(const Value: TlicgTextHorzAlignment); stdcall;
    property HorzAlignment: TlicgTextHorzAlignment read GetHorzAlignment write SetHorzAlignment;
    function GetVertAlignment: TlicgTextVertAlignment; stdcall;
    procedure SetVertAlignment(const Value: TlicgTextVertAlignment); stdcall;
    property VertAlignment: TlicgTextVertAlignment read GetVertAlignment write SetVertAlignment;
  end;

  IlicgText = interface(IlicgTextValue)
    ['{B6A6DC3A-BA49-483F-9A26-9182973E1FB4}']
    procedure SetFontTool(const Value: IInterface); stdcall;
    procedure PopulatePoints; stdcall;
    procedure MakePolyPoints(Grapher: TObject = nil); stdcall;
    procedure DoPolyPoints(const XY: TlicgCoor; Grapher: TObject = nil); stdcall;
    function GetBasePoint: TlicgCoor; stdcall;
    procedure SetBasePoint(const Value: TlicgCoor); stdcall;
    property BasePoint: TlicgCoor read GetBasePoint write SetBasePoint;
  end;

  IlicgTextM = interface(IlicgText)
    ['{4A5AABA8-3DA6-4CAB-AF2F-50D6C9987D79}']
    procedure DoNormalizedVector(const BasePt: TlicgCoor; V: IlicgVector); stdcall;
    procedure SetHeight(const Value: Double); stdcall;
    function GetHeight: Double; stdcall;
    property Height: Double read GetHeight write SetHeight;
    procedure SetWidth(const Value: Double); stdcall;
    function GetWidth: Double; stdcall;
    property Width: Double read GetWidth write SetWidth;
    function GetHorzAlignment: TlicgTextHorzAlignment; stdcall;
    procedure SetHorzAlignment(const Value: TlicgTextHorzAlignment); stdcall;
    property HorzAlignment: TlicgTextHorzAlignment read GetHorzAlignment write
      SetHorzAlignment;
    function GetVertAlignment: TlicgTextVertAlignment; stdcall;
    procedure SetVertAlignment(const Value: TlicgTextVertAlignment); stdcall;
    property VertAlignment: TlicgTextVertAlignment read GetVertAlignment write
      SetVertAlignment;
  end;

  IlicgTextFX = interface(IlicgText)
    ['{0BE9EC59-536A-42A0-9BC2-031F32BCDB2F}']
    function GetInflateFactor: Double; stdcall;
    procedure SetInflateFactor(const Value: Double); stdcall;
    property InflateFactor: Double read GetInflateFactor write SetInflateFactor;
    function GetPivot: TlicgCoor; stdcall;
    procedure SetPivot(const Value: TlicgCoor); stdcall;
    property Pivot: TlicgCoor read GetPivot write SetPivot;
    function GetRounded: Boolean; stdcall;
    procedure SetRounded(const Value: Boolean); stdcall;
    property Rounded: Boolean read GetRounded write SetRounded;
    function GetTextBorderStyle: TlicgTextBorderStyle; stdcall;
    procedure SetTextBorderStyle(const Value: TlicgTextBorderStyle); stdcall;
    property TextBorderStyle: TlicgTextBorderStyle read GetTextBorderStyle write
      SetTextBorderStyle;
  end;

  IlicgMeasurement = interface(IlicgGeometry)
    ['{30C5FC3C-19BA-4B1A-9BF8-F2D21BCDA369}']
    procedure SetDrawTools(aDrawTools: IInterface); stdcall;
    procedure Calculate; stdcall;
    function TextEntity: IInterface;  { 6. nesne text nesnesi, onu döndürür. }
      stdcall;
    function GetEntities: IInterfaceList; stdcall;
    property Entities: IInterfaceList read GetEntities;
    function GetNumDecimals: Integer; stdcall;
    procedure SetNumDecimals(Value: Integer); stdcall;
    property NumDecimals: Integer read GetNumDecimals write SetNumDecimals;
    function GetTextBasePoint: TlicgCoor; stdcall;
    procedure SetTextBasePoint(const Value: TlicgCoor); stdcall;
    property TextBasePoint: TlicgCoor read GetTextBasePoint write SetTextBasePoint;
    function GetBaseLineTo: TlicgCoor; stdcall;
    procedure SetBaseLineTo(const Value: TlicgCoor); stdcall;
    property BaseLineTo: TlicgCoor read GetBaseLineTo write SetBaseLineTo;
    function GetBaseLineFrom: TlicgCoor; stdcall;
    procedure SetBaseLineFrom(const Value: TlicgCoor); stdcall;
    property BaseLineFrom: TlicgCoor read GetBaseLineFrom write SetBaseLineFrom;

       // TextWidth sadece Vertical nesnede kullanýlýyor.
    function IsTextOnLine(const TextWidth: Double = 0): Boolean; stdcall;
       // Sets to Y in Horizontal, to X in Vertical
    procedure SetTextLine(const Value: Double); stdcall;
    function GetTextLine: Double; stdcall;
    property TextLine: Double read GetTextLine write SetTextLine;
  end;

{Interfaceler arasindaki cevrim fonksiyonlari}
function AslicgGeometry(const Int: IInterface): IlicgGeometry;

function AslicgPoint(const Int: IInterface): IlicgPoint;
function AslicgPolyline(const Int: IInterface): IlicgPolyline;
function AslicgSpline(const Int: IInterface): IlicgSpline;
function AslicgArc(const Int: IInterface): IlicgArc;
function AslicgCircle(const Int: IInterface): IlicgCircle;
function AslicgEllipse(const Int: IInterface): IlicgEllipse;
function AslicgRectangle(const Int: IInterface): IlicgRectangle;
function AslicgPlace(const Int: IInterface): IlicgPlace;
function AslicgTextValue(const Int: IInterface): IlicgTextValue;
function AslicgVectorialFittedText(const Int: IInterface): IlicgVectorialFittedText;
function AslicgVectorialText(const Int: IInterface): IlicgVectorialText;
function AslicgText(const Int: IInterface): IlicgText;
function AslicgTextM(const Int: IInterface): IlicgTextM;
function AslicgTextFX(const Int: IInterface): IlicgTextFX;
function AslicgMeasurement(const Int: IInterface): IlicgMeasurement;
function AslicgPolygon(const Int: IInterface): IlicgPolygon;
function AslicgGroupGeometry(const Int: IInterface): IlicgGroupGeometry;
function AslicgVectorFont(const Int: IInterface): IlicgVectorFont;
function AslicgVectorChar(const int: IInterface): IlicgVectorChar;
function AslicgLine(const Int: IInterface): IlicgLine;
function AslicgTriangle(const Int: IInterface): IlicgTriangle;
function AslicgPafta(const Int: IInterface): IlicgPafta;
function AslicgBlockInsert(const Int: IInterface ): IlicgBlockInsert;

{ tekil bir geometry nin cogul GeometryType kodunu uretir. }
function CalcMultiGeometryTypeCode(const GeomCode: integer): integer;
function CalcGeometryType(const dimension, GeomGroup: integer): integer;
function IsACollection(const GeomCode: integer): boolean;

implementation

function CalcMultiGeometryTypeCode(const GeomCode: integer): integer;
begin
  Result := 0;
  if (GeomCode mod 10) in [1, 2, 3] then
  begin
    Result := GeomCode + 4;
  end;
end;

function CalcGeometryType(const dimension, GeomGroup: integer): integer;
begin
  if (dimension in [_2D, _3D, _4D]) then
    Result := dimension * 1000 + GeomGroup
  else
    Result := 3000 + GeomGroup;
end;

function IsACollection(const GeomCode: integer): boolean;
begin
  Result := (GeomCode mod 10) > 4;
end;

function AslicgGeometry(const Int: IInterface): IlicgGeometry;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgGeometry, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgPoint(const Int: IInterface): IlicgPoint;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgPoint, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgPolyline(const Int: IInterface): IlicgPolyline;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgPolyline, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgPolygon(const Int: IInterface): IlicgPolygon;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgPolygon, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgSpline(const Int: IInterface): IlicgSpline;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgSpline, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgArc(const Int: IInterface): IlicgArc;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgArc, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgCircle(const Int: IInterface): IlicgCircle;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgCircle, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgEllipse(const Int: IInterface): IlicgEllipse;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgEllipse, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgRectangle(const Int: IInterface): IlicgRectangle;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgRectangle, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgPlace(const Int: IInterface): IlicgPlace;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgPlace, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgBlockInsert(const Int: IInterface ): IlicgBlockInsert;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgBlockInsert, Result ) <> 0) then
      Result := nil;
  end;
end;

function AslicgVectorialFittedText(const Int: IInterface): IlicgVectorialFittedText;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgVectorialFittedText, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgTextValue(const Int: IInterface): IlicgTextValue;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgTextValue, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgVectorialText(const Int: IInterface): IlicgVectorialText;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgVectorialText, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgText(const Int: IInterface): IlicgText;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgText, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgTextM(const Int: IInterface): IlicgTextM;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgTextM, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgTextFX(const Int: IInterface): IlicgTextFX;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgTextFX, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgMeasurement(const Int: IInterface): IlicgMeasurement;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgMeasurement, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgGroupGeometry(const Int: IInterface): IlicgGroupGeometry;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgGroupGeometry, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgVectorChar(const int: IInterface): IlicgVectorChar;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgVectorChar, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgVectorFont(const Int: IInterface): IlicgVectorFont;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgVectorFont, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgLine(const Int: IInterface): IlicgLine;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgLine, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgTriangle(const Int: IInterface): IlicgTriangle;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgTriangle, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgPafta(const Int: IInterface): IlicgPafta;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgPafta, Result) <> 0) then
      Result := nil;
  end;
end;

end.


