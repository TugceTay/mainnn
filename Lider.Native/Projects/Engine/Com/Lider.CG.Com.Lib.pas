unit Lider.CG.Com.Lib;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  Controls,
  SysUtils,
  Graphics,
  Printers,
  Dialogs,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.STMatrix,
  Lider.CG.Com.CSInt;

type
  TOnlineMapPreference = (omrOSM, omrGoogleHybrid, omrGoogleSatellite,
    omrGoogleRoad, omrGoogleElev, omrBingHybrid, omrBingSatellite, omrBingRoad,
    omrBingElev, omrYandex);

  //IE types ilker
  TVectorIEType = (ieMIF, ieTAB, ieSHP, ieNCZ, ieDXF, ieDGN, ieDGNV8, ieTXT, ieKML);

  TlicgBaseGrapher = class;
  TlicgBasePointList = class;
  TlicgRubberList = class;

  { This enumerated list the possible inputs from the user on the command line }
  TlicgInputType = (itNone, { user typed nothing valid }
    itFloatValue, { user typed a single float value, like when positioning guidelines}
    itString); { user typed a string, ex: "Rob Brown" (double quotes included) }

  TlicgLineRelation = (lrDivToRight, lrDivToLeft, lrAtDivStart, lrAtDivEnd,
    lrBetweenDiv, lrOffDivStart, lrOffDivEnd, lrParToRight, lrParToLeft,
    lrAtParStart, lrAtParEnd, lrBetweenPar, lrOffParStart, lrOffParEnd,
    lrParallel, lrIntOffDiv, lrIntOffPar);

  TlicgLineRelations = set of TlicgLineRelation;
  TlicgDrawMode = (dmNormal, dmRubber, dmRubberLineType, dmSelection, dmHilite, dmSymbol);
  { Graphic Transformer class - used for transforming from
    world Coordinate to device Coors and viceversa - abreviated TlicgGrapher }
  TlicgActiveDevice = (adScreen, adPrinter, adPreview);
  TlicgClipCodes = set of (ccFirst, ccSecond, ccNotVisible, ccVisible);
  TDoubleArray = array of Double;
  TPointArray = array of TPoint;
  TlicgPointArray = array of TlicgCoor3D;
  TlicgPointArray2 = array[0..0] of TlicgCoor;
  PlicgPointArray2 = ^TlicgPointArray2;

  TlicgTransformParams = packed record
    VisualWindow: TlicgExtent;
    MidPoint: TlicgCoor;
    Scale: Double;
  end;

  TlicgBaseTransformParamsList = class
  protected
    procedure Grow; virtual; abstract;
    function Get(Index: Integer): TlicgTransformParams; virtual; abstract;
    procedure Put(Index: Integer; const Item: TlicgTransformParams); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual; abstract;
    procedure SetCount(NewCount: Integer); virtual; abstract;
    function GetCapacity: Integer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
  public
    destructor Destroy; override;
    function Add(const Item: TlicgTransformParams): Integer; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TlicgTransformParams read Get write Put; default;
  end;

  TNotifyEventStdcall = procedure(Sender: TObject) of object;

  TlicgBaseGrapher = class(TObject)
  public
    FViewPortExtent: TlicgExtent;
    FCurrentScale: Double;
    OriginalParams: TlicgTransformParams;
    CurrentParams: TlicgTransformParams;
    { DPI of devices }
    ScreenDpiX, ScreenDpiY: Integer;
    PrinterDpiX, PrinterDpiY: Integer;
  protected
    procedure Push; virtual; abstract;
    function GetDevice: TlicgActiveDevice; virtual; abstract;
    function GetInUpdate: Boolean; virtual; abstract;
    function GetViewPortExtent: TlicgExtent; virtual; abstract;
    function GetViewsHistory: TlicgBaseTransformParamsList; virtual; abstract;
    procedure SetDevice(const Value: TlicgActiveDevice); virtual; abstract;
    procedure SetInUpdate(const Value: Boolean); virtual; abstract;
    function GetOnChange: TNotifyEventStdcall; virtual; abstract;
    procedure SetOnChange(value: TNotifyEventStdcall); virtual; abstract;
    //function GetDrawBoxColor: TColor; virtual; abstract;
    //procedure  SetDrawBoxColor(value: TColor); virtual; abstract;
    function GetDrawBox: TObject; virtual; abstract;
    procedure SetDrawBox(value: TObject); virtual; abstract;
  public
    { methods }
    constructor Create(ADevice: TlicgActiveDevice; DrawBox: TObject = nil);
    destructor Destroy; override;
    procedure Assign(Source: TlicgBaseGrapher); virtual; abstract;
    procedure Pop; virtual; abstract;
    function CanPop: Boolean; virtual; abstract;
    procedure Zoom(const Factor: Double); virtual; abstract;
    procedure SetViewPort(const ALeft, ATop, ARight, ABottom: Double); virtual; abstract;
    procedure Window(const Xi, Xf, Yi, Yf: Double); virtual; abstract;
    procedure SetWindow(const Xi, Xf, Yi, Yf: Double); virtual; abstract;
    procedure SetViewTo(const ViewWin: TlicgExtent); virtual; abstract;
    procedure ReCentre(const MX, MY: Double); virtual; abstract;
    {conversion}
    function PointToReal(const P: TPoint): TlicgCoor; virtual; abstract;
    function RealToPoint(const P: TlicgCoor): TPoint; virtual; abstract;
    function ExtentToReal(const ARect: TRect): TlicgExtent; virtual; abstract;
    function RealToExtent(const AExtent: TlicgExtent): TRect; virtual; abstract;
    function DistToRealX(DistX: Integer): Double; virtual; abstract;
    function DistToRealY(DistY: Integer): Double; virtual; abstract;
    function RealToDistX(const DistX: Double): Integer; virtual; abstract;
    function RealToDistY(const DistY: Double): Integer; virtual; abstract;
    function RDistToRealX(const DistX: Double): Double; virtual; abstract;
    function RDistToRealY(const DistY: Double): Double; virtual; abstract;
    function RRealToDistX(const DistX: Double): Double; virtual; abstract;
    function RRealToDistY(const DistY: Double): Double; virtual; abstract;
    function PointsToDistX(const Size: Double): Double; virtual; abstract;
    function PointsToDistY(const Size: Double): Double; virtual; abstract;
    function DistToPointsX(const Dist: Double): Double; virtual; abstract;
    function DistToPointsY(const Dist: Double): Double; virtual; abstract;
    function DpiX: Integer; virtual; abstract;
    function DpiY: Integer; virtual; abstract;
    procedure SaveCanvas(ACanvas: TCanvas); virtual; abstract;
    procedure RestoreCanvas(ACanvas: TCanvas); virtual; abstract;
    { for internal use }
    function GetSizeInPoints(const Value: Double): Double; virtual; abstract;
    function GetRealSize(const value: Double): Double; virtual; abstract;
    function ZoomToExtentScale(AZoomExtent: TlicgExtent): Double; virtual; abstract;
    procedure SetScale(S: double); virtual; abstract;
    function GetAffineMatrix: TlicgMatrix; virtual; abstract;
    procedure SetAffineMatrix(value: TlicgMatrix); virtual; abstract;
    function GetUseAffineMatrix: Boolean; virtual; abstract;
    procedure SetUseAffineMatrix(value: Boolean); virtual; abstract;
    function IsGoogleGrapher: boolean; virtual; abstract;
    procedure ApplyProjection(const E: IlicgEntity; var _Clip: TlicgExtent); virtual; abstract;
    procedure CancelProjection(const E: IlicgEntity); virtual; abstract;

    {properties}
    property Device: TlicgActiveDevice read GetDevice write SetDevice;
    property ViewPortExtent: TlicgExtent read GetViewPortExtent;
    property InUpdate: Boolean read GetInUpdate write SetInUpdate;
    property ViewsHistory: TlicgBaseTransformParamsList read GetViewsHistory;

    property UseAffineMatrix: Boolean read GetUseAffineMatrix write SetUseAffineMatrix;
    property AffineMatrix: TlicgMatrix read GetAffineMatrix write SetAffineMatrix;

    { events }
    property OnChange: TNotifyEventStdcall read GetOnChange write SetOnChange;
    property CurrentScale: Double read FCurrentScale write FCurrentScale;
    property Drawbox: TObject read GetDrawBox write SetDrawBox;
  end;

  {-----------------------------------------------------------------------------}
  //                   TlicgDoubleList
  {-----------------------------------------------------------------------------}
{
  IlicgDoubleList = class
  protected
    function Get(Index: Integer): Double; virtual; abstract;
    procedure Grow; virtual; abstract;
    procedure Put(Index: Integer; const Item: Double); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual; abstract;
    procedure SetCount(NewCount: Integer); virtual; abstract;
    function GetCapacity: Integer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
  public
    destructor Destroy; override;
    function Add(const Item: Double): Integer;virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Exchange(Index1, Index2: Integer); virtual; abstract;
    procedure Insert(Index: Integer; const Item: Double); virtual; abstract;
    procedure Sort; virtual; abstract;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: Double read Get write Put; default;
  end;
 }
  { TlicgPointList - used for optimizing the redrawing of entities }
  TlicgBasePointList = class
  protected
    function Get(Index: Integer): TPoint; virtual; abstract;
    procedure Put(Index: Integer; const Item: TPoint); virtual; abstract;
    function GetX(Index: Integer): Integer; virtual; abstract;
    function GetY(Index: Integer): Integer; virtual; abstract;
    procedure PutX(Index: Integer; Value: Integer); virtual; abstract;
    procedure PutY(Index: Integer; Value: Integer); virtual; abstract;
    procedure SetCapacity(Value: Integer); virtual; abstract;
    function GetCanGrow: Boolean; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetLast: Integer; virtual; abstract;
    procedure SetCanGrow(const Value: Boolean); virtual; abstract;
  public
    constructor Create(Size: Integer);
    destructor Destroy; override;
    procedure Clear; virtual; abstract;
    procedure Add(const Item: TPoint); virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Insert(Index: Integer; const Item: TPoint); virtual; abstract;
    procedure AddPoint(AX, AY: Integer); virtual; abstract;
    function IsEqual(Index1, Index2: Integer): Boolean; virtual; abstract;
    property X[Index: Integer]: Integer read GetX write PutX;
    property Y[Index: Integer]: Integer read GetY write PutY;
    property Count: Integer read GetCount;
    property Size: Integer read GetLast;
    property CanGrow: Boolean read GetCanGrow write SetCanGrow;
    property Points[Index: Integer]: TPoint read Get write Put; default;
    property Capacity: Integer read GetLast write SetCapacity;
  end;

  { TlicgGradient }

  TlicgBaseGradient = class
  protected
    procedure GradientFill(Canvas: TCanvas; const Rect: TRect; StartColor:
      TColor; EndColor: TColor; Direction: TlicgFillDirection); virtual; abstract;
    function GetDirection: TlicgFillDirection; virtual; abstract;
    function GetEndColor: TColor; virtual; abstract;
    function GetMidColor: TColor; virtual; abstract;
    function GetStartColor: TColor; virtual; abstract;
    function GetVisible: Boolean; virtual; abstract;
    procedure SetDirection(const Value: TlicgFillDirection); virtual; abstract;
    procedure SetEndColor(const Value: TColor); virtual; abstract;
    procedure SetMidColor(const Value: TColor); virtual; abstract;
    procedure SetStartColor(const Value: TColor); virtual; abstract;
    procedure SetVisible(const Value: Boolean); virtual; abstract;
  public
    constructor Create;
    procedure Assign(Source: TlicgBaseGradient); virtual; abstract;
    procedure Draw(Canvas: TCanvas; const Rect: TRect); virtual; abstract;
    property Direction: TlicgFillDirection read GetDirection write SetDirection;
    property StartColor: TColor read GetStartColor write SetStartColor;
    property MidColor: TColor read GetMidColor write SetMidColor;
    property EndColor: TColor read GetEndColor write SetEndColor;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  //****** start of section for diagramming

  { type of existing connectivity for element }
  TlicgConnectType = (ctNodeMaster, ctNodeDetail, ctLink);

  { travel restriction. used when TlicgConnectType = ctLink }
  TlicgTravelRestriction = (trBothWays, trFromStartToEndOnly, trFromEndToStartOnly, trCannotTravel);

  TlicgNodeAssocRec = packed record
    ConnectType: TlicgConnectType; // el tipo de conexion con la otra entidad
    ID: Integer; // ID of the other node/link associated
    Recno: Integer; // Recno of the other node/link for redundant check purposes
    VertexNo: Integer; // number of vertex asociated of other node if VertexNo >= 0
    Restriction: TlicgTravelRestriction; // only used when ConnectType=ctLink
  end;

  TlicgBaseNodeAssocList = class
  protected
    function Get(Index: Integer): TlicgNodeAssocRec; virtual; abstract;
    procedure Grow; virtual; abstract;
    procedure Put(Index: Integer; const Item: TlicgNodeAssocRec); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual; abstract;
    procedure SetCount(NewCount: Integer); virtual; abstract;
    function GetCapacity: Integer; virtual; abstract;
    function GetConnectType: TlicgConnectType; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    procedure SetConnectType(const Value: TlicgConnectType); virtual; abstract;
  public
    destructor Destroy; override;
    function Add(const Item: TlicgNodeAssocRec): Integer; virtual; abstract;
    function AddNode(ID, Recno, VertexNo: Integer; ConnectType: TlicgConnectType): Integer; virtual; abstract;
    procedure AddLink(FromID, FromRecno, FromVertexNo, ToID, ToRecno, ToVertexNo:
      Integer; Restriction: TlicgTravelRestriction); virtual; abstract;
    function IsLink: Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TlicgNodeAssocRec read Get write Put; default;
    property ConnectType: TlicgConnectType read GetConnectType write SetConnectType;
  end;

  TlicgRubberItem = class
  public
    Pts: array of TPoint;
    PenColor: TColor;
    FillColor: TColor;
    PenWidth: Byte;
    PenStyle: TPenStyle;
    constructor Create(const Pts: array of TPoint; PenColor: TColor; PenWidth:
      Byte; PenStyle: TPenStyle; FillColor: TColor);
    destructor Destroy; override;
    function IsClosed: Boolean;
  end;

  TlicgRubberList = class
  private
    FRubberList: TList;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(ACanvas: TCanvas; Pts: array of TPoint; PenColor: TColor;
      PenWidth: Byte; PenStyle: TPenStyle; FillColor: TColor);
    procedure Draw(ACanvas: IlicgCanvas);
    property Count: Integer read GetCount;
  end;
  //****** end of section for diagramming

  { TlicgPointList - used for optimizing the redrawing of entities }
  { class implementation a list of Windows.TPoint }

  TlicgPointList = class(TlicgBasePointList)
  private
    FLast, FCount: Integer;
    FCanGrow: Boolean;
  protected
    function Get(Index: Integer): TPoint; override;
    procedure Put(Index: Integer; const Item: TPoint); override;
    function GetX(Index: Integer): Integer; override;
    function GetY(Index: Integer): Integer; override;
    procedure PutX(Index: Integer; Value: Integer); override;
    procedure PutY(Index: Integer; Value: Integer); override;
    procedure SetCapacity(Value: Integer); override;
    function GetCanGrow: Boolean; override;
    function GetCount: Integer; override;
    function GetLast: Integer; override;
    procedure SetCanGrow(const Value: Boolean); override;
  public
    FPoints: TPointArray;
    constructor Create(Size: Integer);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(const Item: TPoint); override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const Item: TPoint); override;
    procedure AddPoint(AX, AY: Integer); override;
    function IsEqual(Index1, Index2: Integer): Boolean; override;
    property X[Index: Integer]: Integer read GetX write PutX;
    property Y[Index: Integer]: Integer read GetY write PutY;
    property Count: Integer read GetCount;
    property Size: Integer read GetLast;
    property CanGrow: Boolean read GetCanGrow write SetCanGrow;
    property Points[Index: Integer]: TPoint read Get write Put; default;
    property Capacity: Integer read GetLast write SetCapacity;
  end;

  TlicgTransformParamsList = class(TlicgBaseTransformParamsList)
  private
    FList: array of TlicgTransformParams;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; override;
    function Get(Index: Integer): TlicgTransformParams; override;
    procedure Put(Index: Integer; const Item: TlicgTransformParams); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetCount(NewCount: Integer); override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
  public
    destructor Destroy; override;
    function Add(const Item: TlicgTransformParams): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

  { TlicgGradient }

  TlicgGradient = class(TlicgBaseGradient)
  private
    FDirection: TlicgFillDirection;
    FEndColor: TColor;
    FMidColor: TColor;
    FStartColor: TColor;
    FVisible: Boolean;
  protected
    procedure GradientFill(Canvas: TCanvas; const Rect: TRect; StartColor:
      TColor; EndColor: TColor; Direction: TlicgFillDirection); override;
    function GetDirection: TlicgFillDirection; override;
    function GetEndColor: TColor; override;
    function GetMidColor: TColor; override;
    function GetStartColor: TColor; override;
    function GetVisible: Boolean; override;
    procedure SetDirection(const Value: TlicgFillDirection); override;
    procedure SetEndColor(const Value: TColor); override;
    procedure SetMidColor(const Value: TColor); override;
    procedure SetStartColor(const Value: TColor); override;
    procedure SetVisible(const Value: Boolean); override;
  public
    constructor Create;
    procedure Assign(Source: TlicgBaseGradient); override;
    procedure Draw(Canvas: TCanvas; const Rect: TRect); override;
    property Direction: TlicgFillDirection read GetDirection write SetDirection;
    property StartColor: TColor read GetStartColor write SetStartColor;
    property MidColor: TColor read GetMidColor write SetMidColor;
    property EndColor: TColor read GetEndColor write SetEndColor;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  //****** start of section for diagramming

  TlicgNodeAssocList = class(TlicgBaseNodeAssocList)
  private
    FList: array of TlicgNodeAssocRec;
    FConnectType: TlicgConnectType; // type of connectivity (for links, it is ctLink, for nodes it is ctNodeMaster or ctNodeDetail)
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): TlicgNodeAssocRec; override;
    procedure Grow; override;
    procedure Put(Index: Integer; const Item: TlicgNodeAssocRec); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetCount(NewCount: Integer); override;
    function GetCapacity: Integer; override;
    function GetConnectType: TlicgConnectType; override;
    function GetCount: Integer; override;
    procedure SetConnectType(const Value: TlicgConnectType); override;
  public
    destructor Destroy; override;
    function Add(const Item: TlicgNodeAssocRec): Integer; override;
    function AddNode(ID, Recno, VertexNo: Integer; ConnectType: TlicgConnectType):
      Integer; override;
    procedure AddLink(FromID, FromRecno, FromVertexNo, ToID, ToRecno, ToVertexNo:
      Integer; Restriction: TlicgTravelRestriction); override;
    function IsLink: Boolean; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TlicgNodeAssocRec read Get write Put; default;
    property ConnectType: TlicgConnectType read GetConnectType write SetConnectType;
  end;

  // used in trim, break, Tlicgnetwork.CleanUpLayer etc.
  PDistRec = ^TDistRec;
  TDistRec = record
    D: Double;
    Index: Integer;
  end;

  TlicgDistPoint = class
    P: TlicgCoor;
    D: Double;
    constructor create(const _P: TlicgCoor; const _D: double);
  end;

const
  IDENTITY_MATRIX2D: TlicgMatrix = (Matrix: ((1, 0, 0), (0, 1, 0), (0, 0, 1)));

{ Useful functions }
function InPoly(const AEntity: IlicgEntity; const ACoor: TlicgCoor): Boolean; overload; //Nokta Nesnenin iÁinde mi? (Kenarlar HariÁ).
function OnPoly(const AEntity: IlicgEntity; const ACoor: TlicgCoor): Boolean; overload; //Nokta Nesene ‹st¸nde mi? (Kenarlar˝n ¸st¸ndemi, Kapal˝ alan˝n iÁi HariÁ)
function InPoly(const AGeometry: IlicgGeometry; const ACoor: TlicgCoor): Boolean; overload; //Nokta Nesnenin iÁinde mi? (Kenarlar HariÁ).
function OnPoly(const AGeometry: IlicgGeometry; const ACoor: TlicgCoor): Boolean; overload; //Nokta Nesene ‹st¸nde mi? (Kenarlar˝n ¸st¸ndemi, Kapal˝ alan˝n iÁi HariÁ)
function InPoly(const AVector: IlicgVector; const ACoor: TlicgCoor): Boolean; overload; //Nokta Nesnenin iÁinde mi? (Kenarlar HariÁ).
function OnPoly(const AVector: IlicgVector; const ACoor: TlicgCoor): Boolean; overload; //Nokta Nesene ‹st¸nde mi? (Kenarlar˝n ¸st¸ndemi, Kapal˝ alan˝n iÁi HariÁ)

function PointLineDistance2D(const P, P1, P2: TlicgCoor; var Dist: Double): Boolean;
function PointOnPolyLine2D_Vector(const Vector: IlicgVector; Idx1: Integer; const P: TlicgCoor;
  var Dist: Double; const Aperture: Double; const T: TlicgMatrix; const MustClose: Boolean): Integer;
function PointInPolygon2D_Vector(const Vector: IlicgVector; const P: TlicgCoor;
  var Dist: Double; const Aperture: Double; const T: TlicgMatrix): Integer;
function BuildTransformationMatrix(const sx, sy, phi, tx, ty: Double; const
  refPt: TlicgCoor): TlicgMatrix;
procedure Matrix3x3PreMultiply(const m: TlicgMatrix; var t: TlicgMatrix);
function EqualMatrix2D(const M1, M2: TlicgMatrix): Boolean;
function EqualExtension(const E1, E2: TlicgExtent): Boolean;
function IsInvalidExtent(const E: TlicgExtent): Boolean;
function EqualPoint2D(const P1, P2: TlicgCoor; DecimalCount: integer = -12): Boolean;
function EqualPoint3D(const P1, P2: TlicgCoor3D): Boolean;
function FuzzEqualPoint2D(const P1, P2: TlicgCoor): Boolean;
function Point3D(const AX, AY, AZ: Double): TlicgCoor3D;
function Point2D(const AX, AY: Double): TlicgCoor;
function licgExtent(const AXmin, AYmin, AXmax, AYmax: Double): TlicgExtent;
function AddExtent(const AExtent, BExtent: TlicgExtent): TlicgExtent; overload;
function AddExtent(const AExtent, BExtent: TlicgExtent4D): TlicgExtent4D; overload;
function ReorderExtent(const E: TlicgExtent): TlicgExtent;
function ReorderRect(const R: TRect): TRect;
function SetRealToPoint(const P: TlicgCoor): TPoint;
function SetPointToReal(const P: TPoint): TlicgCoor;
function SetExtentToReal(const R: TRect): TlicgExtent;
function SetRealToExtent(const E: TlicgExtent): TRect;
function IsExtensionEmpty(const E: TlicgExtent): Boolean;
procedure OffsetExtent(var E: TlicgExtent; const dx, dy: Double);
function InflateExtent(var E: TlicgExtent; const dx, dy: Double): TlicgExtent; overload;
function InflateExtent(const E: TlicgExtent; Apperture: Double): TlicgExtent; overload;
//function InflateExtent(var E: TlicgExtent; Apperture: Double): TlicgExtent; overload;
function InflateExtentEx(const E: TlicgExtent; const dx, dy: Double; IsLatLon: Boolean = False): TlicgExtent;
procedure MaxBound(var bounds: TlicgCoor; const pt: TlicgCoor);
procedure MinBound(var bounds: TlicgCoor; const pt: TlicgCoor);
procedure CalcMaxMinBounds(var bounds: TlicgExtent; const Value: TlicgExtent); overload;
procedure CalcMaxMinBounds(var bounds: TlicgExtent; const MinValue, MaxValue: TlicgCoor); overload;

{ Yeni yap˝ya geÁi˛ fonksiyonlar˝... }
function TransformCoor2D(const P: TlicgCoor; const T: TlicgMatrix): TlicgCoor;
function Angle2D(const P1, P2: TlicgCoor): Double; overload;

//Matrix functions
function MultiplyMatrix2D(const m, t: TlicgMatrix): TlicgMatrix;
function TransformPoint2D(const P: TlicgCoor; const T: TlicgMatrix): TlicgCoor;
function TransformExtent(const R: TlicgExtent; const T: TlicgMatrix): TlicgExtent;
function Translate2D(const Tx, Ty: Double): TlicgMatrix;
function Rotate2D(const phi: Double; const refPt: TlicgCoor): TlicgMatrix;
function Scale2D(const Sx, Sy: Double; const refPt: TlicgCoor): TlicgMatrix;
function MirrorAroundX: TlicgMatrix;

{ Clipping functions }
function IntersectExtent(const r1, r2: TlicgExtent): TlicgExtent;
function ClipLine2D(const Clip: TlicgExtent; var X1, Y1, X2, Y2: Double): TlicgClipCodes;
function ClipLineLeftRight2D(const Clip: TlicgExtent; var X1, Y1, X2, Y2: Double): TlicgClipCodes;
function ClipLineUpBottom2D(const Clip: TlicgExtent; var X1, Y1, X2, Y2: Double): TlicgClipCodes;
function IsNearPoint2D(const RP, P: TlicgCoor; const Aperture: Double; var Dist: Double): Boolean;
function Dist2DPerimeter(const Pt1, Pt2: TlicgCoor): Double;
function VectDist2D(const V: IlicgVector; const Index: integer; From0Index: Boolean): Double;

function SortDistPoints1(Item1, Item2: Pointer): Integer; //K¸Á¸kten B¸y¸e
function SortDistPoints2(Item1, Item2: Pointer): Integer;
function IsExtentVisible(const ARect, AClip: TlicgExtent): Boolean;
function IsExtentVisibleForPlace(const ARect, AClip: TlicgExtent): Boolean;
function IsDistVisibleByScale(const D: double; const Scale: double): Boolean;
function IsPointInExtent(const APoint: TlicgCoor; const AExtent: TlicgExtent): Boolean;
function IsPointOnExtent(const APoint: TlicgCoor; const AExtent: TlicgExtent): Boolean;
function IsExtentInExtent(const AExtent, BExtent: TlicgExtent): Boolean;
function IsExtentFullInExtent(const AExtent, BExtent: TlicgExtent): Boolean;
function ExtentOutExtent(AExtent, BExtent: TlicgExtent): TlicgExtent;
function ExtentFilling(const AExtent, BExtent: TlicgExtent): Integer;
function TransformBoundingExtent(const AExtent: TlicgExtent; const Matrix: TlicgMatrix): TlicgExtent;
function ChangeToOrtogonal(const Pt1, Pt2: TlicgCoor): TlicgCoor;
procedure FindCG(Vector: IlicgVector; var CG: TlicgCoor; ComputeCG: Boolean);
function PointIsOnSegment2D(const C, A, B: TlicgCoor): Boolean;
function PrintersInstalled: Boolean;
function LineRel(const P1, P2, D1, D2: TlicgCoor; var P: TlicgCoor): TlicgLineRelations;
procedure ClipPolygonToArea(Vector, ClippedVector: IlicgVector; const ClipArea: TlicgExtent);
// this code was generousely donated by Jens Gruschel
procedure GetMinimumDistance2D(Poly1, Poly2: IlicgVector; var Distance: Double; var Min1, Min2: TlicgCoor);
procedure GetMinimumDistance2D_2(const Pivot: TlicgCoor; const Poly: IlicgVector; var Distance: Double; var Min1, Min2: TlicgCoor);
{ this function returns the perpendicular projection of point C on
  the lines that goes from A to B }
function Perpend(const C, A, B: TlicgCoor): TlicgCoor;
function GDICheck(Value: THandle): THandle;
// ilker dei˛tirme Ez orjden yeni s¸r¸me g¸ncellendi function GDICheck(Value: Integer): Integer;
function ExtentWidth(const AExtent: TlicgExtent): Double;
function ExtentHeight(const AExtent: TlicgExtent): Double;
function StrISNumeric(Str: string): Boolean;//is String numeric ?

 //String numeric bir deer mi?  Gˆnderilen string iÁeriinin
 //geÁerli bir say˝sal deer olup olmad˝˝n˝ dˆnd¸r¸r.
function StrContainAnyNumber(tmpStr: string): Boolean;
procedure SetArc(FPoints: IlicgVector; const X, Y, StartAng, EndAng, Rad: Double); overload;

//TlicgArc nesnesinin SetArc metodu (Import iÁin d˝˛ar˝ al˝nd˝)
procedure SetArc(FPoints: IlicgVector; const CX, CY, R, StartAng, NumRadians: Double; IsCounterClockWise: Boolean); overload;

procedure VectorElevationChange(var AVector: IlicgVector; AElevation: Double = 0); overload;
procedure VectorElevationChange(var AEntity: IlicgEntity; AElevation: Double = 0); overload;

procedure ExtentToVector(AExtent: TlicgExtent; var AVector: IlicgVector; var P1, P2, P3, P4: TlicgCoor); overload;
procedure ExtentToVector(AExtent: TlicgExtent; var AVector: IlicgVector); overload;

//This procedure provides that between Which Geometry.Points is the current point and
//returns first and second index of Geometry.Points.
//Vect: Entity.Geometry.Points
//Grapher: For aperture calculate
procedure BetweenWhichPoints(Vect: IlicgVector; CurrPoint: TlicgCoor; Grapher:
  TlicgBaseGrapher; Aperture: Integer; var FirstI, SecondI: Integer; var
  Distance: Double);

//Verilen kutu Koordinatlardan Extent'e dˆnd¸r¸r. ilker ekleme
function Extent(x1, y1, x2, y2: Double): TlicgExtent;
//Noktay˝ Extent'e dˆn¸˛t¸r¸r. ilker ekleme
function PointToExtent(APoint: TlicgCoor): TlicgExtent;
// Extend Orta Noktas˝n˝ bulur. ilker ekleme
function ExtentMidPoint(AExtent: TlicgExtent): TlicgCoor;

//DXF import s˝ras˝nda t¸rkÁe karakterler bozuk geliyordu.  ConvertToTurkish okunan shortString karakterlerin T¸rkÁeye Áevririr
function ConvertToTurkish(Value: string): string;
function StMatrixToEzMatrix(const _stMAtrix: TStMatrix): TlicgMatrix;
function EzMatrixToStMatrix(const ezMatrix: TlicgMatrix): TStMatrix;
procedure SortOrderList(Layer: TObject; Solver: TObject; ol: IInterface);
function ApplyMatrixToExtent(R: TlicgExtent; M: TlicgMatrix): TlicgExtent;
function RoundToDouble(const D: Double; const Decimal: integer): Double;
function DoubleToStr(const D: double; const DecimalCount: integer): string;
function PointString(const X, Y: double; DecimalCount: integer): string;
function PointFromString(const s: string; var X, Y: double): Boolean;
function ClosestLineInVector(const P: TlicgCoor; V: IlicgVector; var P1, P2:
  TlicgCoor; var MinDistance: Double): Boolean; overload;
function ClosestLineInVector(const P: TlicgCoor; V: IlicgVector; var P1, P2:
  integer; var MinDistance: Double): Boolean; overload;
procedure ClosestPointOnLineFromPoint(const x1, y1, x2, y2, Px, Py: double; out Nx, Ny: double);
function MinimumDistanceFromPointToLine(const Px, Py, x1, y1, x2, y2: Double): Double;
function MinimumDistanceFromPointToSegment(const Px, Py, x1, y1, x2, y2: Double): Double;
function NearestCoor(const P: TlicgCoor; V: IlicgVector; w_otele: double = 0; h_otele: double = 0): TlicgCoor;
procedure QuickSortDistList(SortList: TList; L, R: Integer);
function ScaleMatrixText(const Pt: TlicgCoor): TlicgMatrix;
//ocedure SaveClippedAreaTo(DrawBox: TObject; NewGis: TObject);
function PolygonExtentIntersectClip(APolygon: IlicgEntity; AExtent: TlicgExtent): IlicgEntity;
function CenterExtentIntersectClip(APolygon: IlicgEntity; AExtent: TlicgExtent): TlicgCoor;
function IsFullInsideEntityInExtent(APolygon: IlicgEntity; AExtent: TlicgExtent): boolean;
function Geo_DistanceToKm(const lat1, lon1, lat2, lon2: double): double;
function Geo_DistanceToM(const lat1, lon1, lat2, lon2: double): double;
function GeoLengthToM(const Points: IlicgVector): double;
//function MeterToDeg_H (P: TlicgCoor; M : Double) : double;
//function MeterToDeg_V (P: TlicgCoor; M : Double) : double;
function TransformRectToProjection(const aGis: TObject; inPrj, OutPrj: IlicgCS; R: TlicgExtent): TlicgExtent;

/// <summary>
///   <para>
///     B Polygonu A Polygonun iÁinde olup olmad˝˝na bakar.
///   </para>
///   <para>
///     21.07.2024 ilker ekleme
///   </para>
/// </summary>
/// <param name="AVector">
///   Kapal˝ alan olmas˝ gerekir. <br />
/// </param>
/// <param name="BVector">
///   Kapal˝ alan olmas˝ gerekir.
/// </param>
/// <param name="AFullInside">
///   Tamam˝ iÁinde olmas˝ isteniyorsa ve True ayarlan˝rsa sonuÁ ona gˆre
///   dˆner.
/// </param>
function IsInsidePolygon(const AVector: IlicgVector; const BVector: IlicgVector; const AFullInside: Boolean): Boolean; // ilker ekleme 21.07.2024

implementation

uses
  Consts,
  WinSpool,
  Math,
  Lider.CG.Com.Geolibrary,
  Lider.CG.Com.Consts,
  Lider.CG.Com.System,
  Lider.CG.Com.Graphics,
  Lider.CG.Com.Base,
  Lider.CG.Com.Expr,
  Lider.CG.Com.LexLib,
  Lider.CG.Com.YaccLib,
  Lider.CG.Com.Expressions,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Math,
  Lider.CG.Com.GIS,
  Lider.CG.Com.ReferenceInt;

type
  TlicgOutCode = set of (left, bottom, right, top, infront, behind);

  PDouble = ^Double;

function TransformRectToProjection(const aGis: TObject; inPrj, OutPrj:
  IlicgCS; R: TlicgExtent): TlicgExtent;
begin
  Result := R;
  try
    TlicgBaseGis(AGis).CSCoorToCSCoor(Result.LowerLeft.X, Result.LowerLeft.Y,
      Result.LowerLeft.X, Result.LowerLeft.Y, inPrj, OutPrj, nil);
    TlicgBaseGis(AGis).CSCoorToCSCoor(Result.UpperRight.X, Result.UpperRight.Y,
      Result.UpperRight.X, Result.UpperRight.Y, inPrj, OutPrj, nil);
  except
  end;
end;

function ApplyMatrixToExtent(R: TlicgExtent; M: TlicgMatrix): TlicgExtent;
var
  i: integer;
  V: IlicgVector;
begin

  V := Licad.CreateEntityFactory.MakeVector(3, 0);
  try

    for i := 1 to 5 do
      V.Add(_NULL_COOR.x, _NULL_COOR.y);

    V[0] := AsCoor(R.LowerLeft.X, R.LowerLeft.Y);
    V[1] := AsCoor(R.LowerLeft.X, R.UpperRight.Y);
    V[2] := AsCoor(R.UpperRight.X, R.UpperRight.Y);
    V[3] := AsCoor(R.UpperRight.X, R.LowerLeft.Y);
    V[4] := AsCoor(R.LowerLeft.X, R.LowerLeft.Y);

    V[0] := TransformPoint2D(V[0], M);
    V[1] := TransformPoint2D(V[1], M);
    V[2] := TransformPoint2D(V[2], M);
    V[3] := TransformPoint2D(V[3], M);
    V[4] := TransformPoint2D(V[4], M);

  finally
    result := V.Extent;
    V := nil;
  end;

end;

function Defuzz(const x: Double): Double;
begin
  if Abs(x) < _EPSILON_FUZZ then
    Result := 0.0
  else
    Result := x;
end;


/// <summary>
 /// Hatta noktan˝n en k˝sa uzankl˝˝
 /// <Uyari> MinimumDistanceFromPointToLine ve MinimumDistanceFromPointToSegment
 /// Perpend, PointLineDistance2D
 /// fonksiyonlar˝ da kullan˝labilir</Uyari>
 /// </summary>
function PointLineDistance2D(const P, P1, P2: TlicgCoor; var Dist: Double): Boolean;
var
  r, L, LQ, DX, DY: Double;
begin
  Result := False;
  DX := P2.X - P1.X;
  DY := P2.Y - P1.Y;
  L := Sqrt(DX * DX + DY * DY);
  if L = 0 then
    Exit;
  LQ := L * L;
  r := ((P1.Y - P.Y) * (-DY) - (P1.X - P.X) * DX) / LQ;
  Result := (r >= 0) and (r <= 1);
  if not Result then
    Exit;
  Dist := Abs(((P1.Y - P.Y) * DX - (P1.X - P.X) * DY) / L);
end;



(* If point P is on a line of Vector.Geometry.Points a distance <= Aperture return:
   PICKED_NONE if point P is not on the line
   PICKED_SEGMENT if point P is on the line
   Dist is the distance found and must be <= Aperture
   Taken from Graphics programming book *)

function PointOnPolyLine2D_Vector(const Vector: IlicgVector; Idx1: Integer;
  const P: TlicgCoor; var Dist: Double; const Aperture: Double; const T:
  TlicgMatrix; const MustClose: Boolean): Integer;
var
  TmpDist: Double;
  TmpPt1, TmpPt2: TlicgCoor;
  I, Max: Integer;
begin
  Result := PICKED_NONE;
  Licad.Settings.GNumPoint := PICKED_NONE;
  TmpPt1 := TransformPoint2D(Vector[0], T);
  Dist := Aperture { * 2};
  if MustClose then
    Max := Vector.Count
  else
    Max := Vector.Count - 1;
  for I := 1 to Max do
  begin
    if I = Vector.Count then
      TmpPt2 := TransformPoint2D(Vector[0], T)
    else
      TmpPt2 := TransformPoint2D(Vector[I], T);
    if PointLineDistance2D(P, TmpPt1, TmpPt2, TmpDist) and (TmpDist <= Dist) then
    begin
      Result := PICKED_SEGMENT;
      Dist := TmpDist;
      Licad.Settings.GNumPoint := Idx1 + Pred(I);
    end;
    TmpPt1 := TmpPt2;
  end;
end;

function Point3D(const AX, AY, AZ: Double): TlicgCoor3D;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
    Z := AZ;
  end;
end;

function Point2D(const AX, AY: Double): TlicgCoor;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

function ReorderExtent(const E: TlicgExtent): TlicgExtent;
begin
  if E.LowerLeft.x < E.UpperRight.x then
  begin
    Result.LowerLeft.x := E.LowerLeft.x;
    Result.UpperRight.x := E.UpperRight.x;
  end
  else
  begin
    Result.LowerLeft.x := E.UpperRight.x;
    Result.UpperRight.x := E.LowerLeft.x;
  end;
  if E.LowerLeft.y < E.UpperRight.y then
  begin
    Result.LowerLeft.y := E.LowerLeft.y;
    Result.UpperRight.y := E.UpperRight.y;
  end
  else
  begin
    Result.LowerLeft.y := E.UpperRight.y;
    Result.UpperRight.y := E.LowerLeft.y;
  end;
end;

function ReorderRect(const R: TRect): TRect;
begin
  if R.Left < R.Right then
  begin
    Result.Left := R.Left;
    Result.Right := R.Right;
  end
  else
  begin
    Result.Left := R.Right;
    Result.Right := R.Left;
  end;
  if R.Bottom > R.Top then
  begin
    Result.Bottom := R.Bottom;
    Result.Top := R.Top;
  end
  else
  begin
    Result.Bottom := R.Top;
    Result.Top := R.Bottom;
  end;
end;

procedure OffsetExtent(var E: TlicgExtent; const dx, dy: Double);
begin
  E.LowerLeft.x := E.LowerLeft.x + dx;
  E.UpperRight.x := E.UpperRight.x + dx;
  E.LowerLeft.y := E.LowerLeft.y + dy;
  E.UpperRight.y := E.UpperRight.y + dy;
end;

function InflateExtent(var E: TlicgExtent; const dx, dy: Double): TlicgExtent;
begin
  E.LowerLeft.x := E.LowerLeft.x - dx;
  E.UpperRight.x := E.UpperRight.x + dx;
  E.LowerLeft.y := E.LowerLeft.y - dy;
  E.UpperRight.y := E.UpperRight.y + dy;
  Result := E;
end;
(*
function InflateExtent(const E: TlicgExtent; Apperture: Double): TlicgExtent;
begin
  E.LowerLeft.x := E.LowerLeft.x - Apperture;
  E.UpperRight.x := E.UpperRight.x + Apperture;
  E.LowerLeft.y := E.LowerLeft.y - Apperture;
  E.UpperRight.y := E.UpperRight.y + Apperture;
  Result := E;
end;  *)

function InflateExtent(const E: TlicgExtent; Apperture: Double): TlicgExtent;
begin
  Result.LowerLeft.X := E.LowerLeft.X - Apperture;
  Result.LowerLeft.Y := E.LowerLeft.Y - Apperture;
  Result.UpperRight.X := E.UpperRight.X + Apperture;
  Result.UpperRight.Y := E.UpperRight.Y + Apperture;
end;

function InflateExtentEx(const E: TlicgExtent; const dx, dy: Double; IsLatLon: Boolean = False): TlicgExtent;
var
  _dx, _dy: double;
begin
  if IsLatLon then
    _dx := MeterToDeg(dx)
  else
    _dx := dx;

  if IsLatLon then
    _dy := MeterToDeg(dy)
  else
    _dy := dy;

  Result.LowerLeft.x := E.LowerLeft.x - _dx;
  Result.UpperRight.x := E.UpperRight.x + _dx;
  Result.LowerLeft.y := E.LowerLeft.y - _dy;
  Result.UpperRight.y := E.UpperRight.y + _dy;
end;

function licgExtent(const AXmin, AYmin, AXmax, AYmax: Double): TlicgExtent;
begin
  with Result do
  begin
    LowerLeft.x := AXmin;
    LowerLeft.y := AYmin;
    UpperRight.x := AXmax;
    UpperRight.y := AYmax;
  end;
end;

function AddExtent(const AExtent, BExtent: TlicgExtent): TlicgExtent;
begin
  Result.LowerLeft.X := Min(AExtent.LowerLeft.X, BExtent.LowerLeft.X);
  Result.LowerLeft.Y := Min(AExtent.LowerLeft.Y, BExtent.LowerLeft.Y);

  Result.UpperRight.X := Max(AExtent.UpperRight.X, BExtent.UpperRight.X);
  Result.UpperRight.Y := Max(AExtent.UpperRight.Y, BExtent.UpperRight.Y);
end;

function AddExtent(const AExtent, BExtent: TlicgExtent4D): TlicgExtent4D;
begin
  Result.LowerLeft.X := Min(AExtent.LowerLeft.X, BExtent.LowerLeft.X);
  Result.LowerLeft.Y := Min(AExtent.LowerLeft.Y, BExtent.LowerLeft.Y);
  Result.LowerLeft.Z := Min(AExtent.LowerLeft.Z, BExtent.LowerLeft.Z);
  Result.LowerLeft.N := Min(AExtent.LowerLeft.N, BExtent.LowerLeft.N);

  Result.UpperRight.X := Max(AExtent.UpperRight.X, BExtent.UpperRight.X);
  Result.UpperRight.Y := Max(AExtent.UpperRight.Y, BExtent.UpperRight.Y);
  Result.UpperRight.Z := Max(AExtent.UpperRight.Z, BExtent.UpperRight.Z);
  Result.UpperRight.N := Max(AExtent.UpperRight.N, BExtent.UpperRight.N);
end;

function SetRealToPoint(const P: TlicgCoor): TPoint;
begin
  Result.X := Round(P.X);
  Result.Y := Round(P.Y);
end;

function SetPointToReal(const P: TPoint): TlicgCoor;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function SetExtentToReal(const R: TRect): TlicgExtent;
begin
  Result := licgExtent(R.Left, R.Top, R.Right, R.Bottom);
end;

function SetRealToExtent(const E: TlicgExtent): TRect;
begin
  Result.Left := Round(E.LowerLeft.x);
  Result.Right := Round(E.UpperRight.x);
  if E.LowerLeft.y < E.UpperRight.y then
  begin
    Result.Top := Round(E.LowerLeft.y);
    Result.Bottom := Round(E.UpperRight.y);
  end
  else
  begin
    Result.Bottom := Round(E.LowerLeft.y);
    Result.Top := Round(E.UpperRight.y);
  end;
end;

function EqualMatrix2D(const M1, M2: TlicgMatrix): Boolean;
begin
  Result := CompareMem(@M1, @M2, Sizeof(TlicgMatrix));
end;

function MultiplyMatrix2D(const m, t: TlicgMatrix): TlicgMatrix;
var
  r, c: integer;
begin
  if CompareMem(@m, @IDENTITY_MATRIX2D, Sizeof(TlicgMatrix)) then
  begin
    Result := t;
    exit;
  end;
  if CompareMem(@t, @IDENTITY_MATRIX2D, Sizeof(TlicgMatrix)) then
  begin
    Result := m;
    exit;
  end;
  for r := 0 to 2 do
  begin
    for c := 0 to 2 do
    begin
      Result.Matrix[r, c] := m.Matrix[r, 0] * t.Matrix[0, c] + m.Matrix[r, 1] *
        t.Matrix[1, c] + m.Matrix[r, 2] * t.Matrix[2, c];
    end;
  end;
end;

function TransformCoor2D(const P: TlicgCoor; const T: TlicgMatrix): TlicgCoor;
begin
  Result := P;

  if not CompareMem(@T, @IDENTITY_MATRIX2D, Sizeof(TlicgMatrix)) then
  begin
    Result.X := T.Matrix[0, 0] * P.X + T.Matrix[0, 1] * P.Y + T.Matrix[0, 2];
    Result.Y := T.Matrix[1, 0] * P.X + T.Matrix[1, 1] * P.Y + T.Matrix[1, 2];
  end;

end;

function Angle2D(const P1, P2: TlicgCoor): Double; overload;
var
  Dx, Dy: Double;
begin
  Dx := P2.x - P1.x;
  Dy := P2.y - P1.y;
  Result := 0;
  if (Dx = 0) and (Dy = 0) then
    Exit;
  if (Dx = 0) and (Dy > 0) then
    Result := Pi / 2
  else if (Dx = 0) and (Dy < 0) then
    Result := -Pi / 2
  else
    Result := Math.ArcTan2(Dy, Dx);
end;

function TransformPoint2D(const P: TlicgCoor; const T: TlicgMatrix): TlicgCoor;
begin
  if CompareMem(@T, @IDENTITY_MATRIX2D, Sizeof(TlicgMatrix)) then
  begin
    { don't waste precious time }
    Result := P;
    Exit;
  end;
  Result.X := T.Matrix[0, 0] * P.X + T.Matrix[0, 1] * P.Y + T.Matrix[0, 2];
  Result.Y := T.Matrix[1, 0] * P.X + T.Matrix[1, 1] * P.Y + T.Matrix[1, 2];
end;

function MirrorAroundX: TlicgMatrix;
begin
  Result := IDENTITY_MATRIX2D;
  Result.Matrix[0, 0] := 1;
  Result.Matrix[1, 1] := -1;
  Result.Matrix[2, 2] := 1;
end;

function TransformExtent(const R: TlicgExtent; const T: TlicgMatrix): TlicgExtent;
begin
  Result.LowerLeft := TransformPoint2D(R.LowerLeft, T);
  Result.UpperRight := TransformPoint2D(R.UpperRight, T);
end;

function Translate2D(const Tx, Ty: Double): TlicgMatrix;
begin
  Result := IDENTITY_MATRIX2D;
  Result.Matrix[0, 2] := Tx;
  Result.Matrix[1, 2] := Ty;
end;

function Rotate2D(const phi: Double; const refPt: TlicgCoor): TlicgMatrix;
var
  aCos, aSin: Double;//Uur Ekleme
begin
  aCos := Cos(phi);//Uur Ekleme
  aSin := Sin(phi);//Uur Ekleme
  Result := IDENTITY_MATRIX2D;
  Result.Matrix[0, 0] := aCos;
  Result.Matrix[0, 1] := -aSin;
  Result.Matrix[0, 2] := refPt.X * (1 - aCos) + refPt.Y * aSin;
  Result.Matrix[1, 0] := aSin;
  Result.Matrix[1, 1] := aCos;
  Result.Matrix[1, 2] := refPt.Y * (1 - aCos) - refPt.X * aSin;
end;

function Scale2D(const Sx, Sy: Double; const refPt: TlicgCoor): TlicgMatrix;
begin
  Result := IDENTITY_MATRIX2D;
  Result.Matrix[0, 0] := Sx;
  Result.Matrix[0, 2] := (1 - Sx) * refPt.X;
  Result.Matrix[1, 1] := Sy;
  Result.Matrix[1, 2] := (1 - Sy) * refPt.Y;
end;

procedure Matrix3x3PreMultiply(const m: TlicgMatrix; var t: TlicgMatrix);
var
  r, c: integer;
  tmp: TlicgMatrix;
begin
  for r := 0 to 2 do
    for c := 0 to 2 do
      tmp.Matrix[r, c] := m.Matrix[r, 0] * t.Matrix[0, c] + m.Matrix[r, 1] * t.Matrix
        [1, c] + m.Matrix[r, 2] * t.Matrix[2, c];
  t := tmp;
end;

function BuildTransformationMatrix(const sx, sy, phi, tx, ty: Double; const
  refPt: TlicgCoor): TlicgMatrix;
begin
  Result := IDENTITY_MATRIX2D;
  if not ((sx = 1) and (sy = 1)) then
    matrix3x3PreMultiply(Scale2D(sx, sy, refPt), Result);
  if phi <> 0 then
    matrix3x3PreMultiply(Rotate2D(phi, refPt), Result);
  if not ((tx = 0) and (ty = 0)) then
    matrix3x3PreMultiply(Translate2D(tx, ty), Result);
end;

function DoubleToStr(const D: double; const DecimalCount: integer): string;
begin
  result := floattostr(RoundToDouble(D, DecimalCount));
end;

function RoundToDouble(const D: Double; const Decimal: integer): Double;
begin
  result := RoundTo(D, -abs(Decimal));
end;

function EqualPoint3D(const P1, P2: TlicgCoor3D): Boolean;
begin
  Result := CompareMem(@P1, @P2, Sizeof(TlicgCoor3D));
end;

function EqualExtension(const E1, E2: TlicgExtent): Boolean;
begin
  Result := CompareMem(@E1, @E2, Sizeof(TlicgExtent));
end;

function IsInvalidExtent(const E: TlicgExtent): Boolean;
begin
  Result := ((E.LowerLeft.X <= _MINCOOR) or (E.LowerLeft.X >= _MAXCOOR)) or
            ((E.LowerLeft.Y <= _MINCOOR) or (E.LowerLeft.Y >= _MAXCOOR)) or
            ((E.UpperRight.X <= _MINCOOR) or (E.UpperRight.X >= _MAXCOOR)) or
            ((E.UpperRight.Y <= _MINCOOR) or (E.UpperRight.Y >= _MAXCOOR));
end;

function FuzzEqualPoint2D(const P1, P2: TlicgCoor): Boolean;
begin
  Result := Defuzz(_Distance(P1, P2)) = 0.0;
end;

function IsDistVisibleByScale(const D: double; const Scale: double): Boolean;
var
  m: Double;
begin
  m := (1000 / Scale) * D;
  Result := (m >= 0.2);
end;

function ExtentFilling(const AExtent, BExtent: TlicgExtent): Integer;
var
  Tmp1, Tmp2: Integer;
begin
  try
    if CompareMem(@BExtent.UpperRight.x, @BExtent.LowerLeft.x, Sizeof(Double)) = false then
      Tmp1 := Round((AExtent.UpperRight.x - AExtent.LowerLeft.x) / (BExtent.UpperRight.x - BExtent.LowerLeft.x) * 1000)
    else
      Tmp1 := 0;
    if CompareMem(@BExtent.UpperRight.y, @BExtent.LowerLeft.y, Sizeof(Double)) = false then
      Tmp2 := Round((AExtent.UpperRight.y - AExtent.LowerLeft.y) / (BExtent.UpperRight.y - BExtent.LowerLeft.y) * 1000)
    else
      Tmp2 := 0;
    if Tmp1 > Tmp2 then
      Result := Tmp1
    else
      Result := Tmp2;
  except
    //on EInvalidOp do
    Result := 1000;
  end;
end;

function PositionCode2D(const Clip: TlicgExtent; const P: TlicgCoor): TlicgOutCode;
begin
  Result := [];
  if Clip.LowerLeft.x < Clip.UpperRight.x then
  begin
    if P.X < Clip.LowerLeft.x then
      Result := [left]
    else if P.X > Clip.UpperRight.x then
      Result := [right];
  end
  else
  begin
    if P.X > Clip.LowerLeft.x then
      Result := [left]
    else if P.X < Clip.UpperRight.x then
      Result := [right];
  end;

  if Clip.LowerLeft.y < Clip.UpperRight.y then
  begin
    if P.Y < Clip.LowerLeft.y then
      Result := Result + [bottom]
    else if P.Y > Clip.UpperRight.y then
      Result := Result + [top];
  end
  else
  begin
    if P.Y > Clip.LowerLeft.y then
      Result := Result + [bottom]
    else if P.Y < Clip.UpperRight.y then
      Result := Result + [top];
  end;

end;

function IsPointInExtent(const APoint: TlicgCoor; const AExtent: TlicgExtent): Boolean;
begin
  Result := PositionCode2D(AExtent, APoint) = [];
end;

function IsExtentVisible(const ARect, AClip: TlicgExtent): Boolean;
var
  dx: double;
begin
  Result := False;
  with ARect do
  begin
    if LowerLeft.x > AClip.UpperRight.x then
      Exit
    else if UpperRight.x < AClip.LowerLeft.x then
      Exit;
    if LowerLeft.y > AClip.UpperRight.y then
      Exit
    else if UpperRight.y < AClip.LowerLeft.y then
      Exit;
  end;

  if CompareMem(@ARect.LowerLeft, @ARect.UpperRight, Sizeof(TlicgCoor)) and
    IsPointInExtent(ARect.LowerLeft, AClip) then
  begin
    Result := true;
    Exit;
  end;

  dx := ExtentFilling(ARect, AClip);
  if Licad.Settings.MinDrawLimit <> 0 then
    Result := dx >= Licad.Settings.MinDrawLimit
  else
    Result := true // dx >= 1e-12;

end;

function IsExtentVisibleForPlace(const ARect, AClip: TlicgExtent): Boolean;
begin
  Result := False;
  with ARect do
  begin
    if LowerLeft.x > AClip.UpperRight.x then
      Exit
    else if UpperRight.x < AClip.LowerLeft.x then
      Exit;
    if LowerLeft.y > AClip.UpperRight.y then
      Exit
    else if UpperRight.y < AClip.LowerLeft.y then
      Exit;
  end;
  if CompareMem(@ARect.LowerLeft, @ARect.UpperRight, Sizeof(TlicgCoor)) and
    IsPointInExtent(ARect.LowerLeft, AClip) then
  begin
    Result := true;
    Exit;
  end;
  Result := True;
end;

function ClipPt(const Denom, Num: Double; var tE, tL: Double): Boolean;
var
  T: Double;
begin
  Result := False;
  if Denom > 0 then
  begin
    T := Num / Denom;
    if T > tL then
      Exit
    else if T > tE then
      tE := T;
  end
  else if Denom < 0 then
  begin
    T := Num / Denom;
    if T < tE then
      Exit
    else if T < tL then
      tL := T;
  end
  else if Num > 0 then
    Exit;
  Result := True;
end;

function ClipLine2D(const Clip: TlicgExtent; var X1, Y1, X2, Y2: Double): TlicgClipCodes;
var
  DX, DY, tE, tL: Double;
begin
  DX := X2 - X1;
  DY := Y2 - Y1;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInExtent(AsCoor(X1, Y1), Clip) then
  begin
    Result := [ccVisible];
    Exit;
  end;
  tE := 0.0;
  tL := 1.0;
  if ClipPt(DX, Clip.LowerLeft.x - X1, tE, tL) then
    if ClipPt(-DX, X1 - Clip.UpperRight.x, tE, tL) then
      if ClipPt(DY, Clip.LowerLeft.y - Y1, tE, tL) then
        if ClipPt(-DY, Y1 - Clip.UpperRight.y, tE, tL) then
        begin
          Result := [];
          if tL < 1 then
          begin
            X2 := X1 + tL * DX;
            Y2 := Y1 + tL * DY;
            Result := [ccSecond];
          end;
          if tE > 0 then
          begin
            X1 := X1 + tE * DX;
            Y1 := Y1 + tE * DY;
            Result := Result + [ccFirst];
          end;
          if Result = [] then
            Result := [ccVisible];
        end;
end;


(* The famous Liang-Barsky algoritm *)

function ClipLineLeftRight2D(const Clip: TlicgExtent; var X1, Y1, X2, Y2: Double):
  TlicgClipCodes;
var
  DX, DY, tE, tL: Double;
begin
  DX := X2 - X1;
  DY := Y2 - Y1;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInExtent(AsCoor(X1, Y1), Clip) then
  begin
    Result := [ccVisible];
    Exit;
  end;
  tE := 0.0;
  tL := 1.0;
  if ClipPt(DX, Clip.LowerLeft.x - X1, tE, tL) then
    if ClipPt(-DX, X1 - Clip.UpperRight.x, tE, tL) then
    begin
      Result := [];
      if tL < 1 then
      begin
        X2 := X1 + tL * DX;
        Y2 := Y1 + tL * DY;
        Result := [ccSecond];
      end;
      if tE > 0 then
      begin
        X1 := X1 + tE * DX;
        Y1 := Y1 + tE * DY;
        Result := Result + [ccFirst];
      end;
      if Result = [] then
        Result := [ccVisible];
    end;
end;

function ClipLineUpBottom2D(const Clip: TlicgExtent; var X1, Y1, X2, Y2: Double):
  TlicgClipCodes;
var
  DX, DY, tE, tL: Double;
begin
  DX := X2 - X1;
  DY := Y2 - Y1;
  Result := [ccNotVisible];
  if (DX = 0) and (DY = 0) and IsPointInExtent(AsCoor(X1, Y1), Clip) then
  begin
    Result := [ccVisible];
    Exit;
  end;
  tE := 0.0;
  tL := 1.0;
  if ClipPt(DY, Clip.LowerLeft.y - Y1, tE, tL) then
    if ClipPt(-DY, Y1 - Clip.UpperRight.y, tE, tL) then
    begin
      Result := [];
      if tL < 1 then
      begin
        X2 := X1 + tL * DX;
        Y2 := Y1 + tL * DY;
        Result := [ccSecond];
      end;
      if tE > 0 then
      begin
        X1 := X1 + tE * DX;
        Y1 := Y1 + tE * DY;
        Result := Result + [ccFirst];
      end;
      if Result = [] then
        Result := [ccVisible];
    end;
end;

function IsExtentInExtent(const AExtent, BExtent: TlicgExtent): Boolean;
var
  FCode, SCode: TlicgOutCode;
begin
  FCode := PositionCode2D(BExtent, AExtent.LowerLeft);
  SCode := PositionCode2D(BExtent, AExtent.UpperRight);
  Result := (FCode * SCode) = [];
end;

function IsPointOnExtent(const APoint: TlicgCoor; const AExtent: TlicgExtent): Boolean;
begin
  Result := False;
  if not IsPointOnLine(APoint, AExtent.LowerLeft, AsCoor(AExtent.LowerLeft.X, AExtent.UpperRight.Y)) then
  begin
    if not IsPointOnLine(APoint, AsCoor(AExtent.LowerLeft.X, AExtent.UpperRight.Y), AExtent.UpperRight) then
    begin
      if not IsPointOnLine(APoint, AExtent.UpperRight, AsCoor(AExtent.UpperRight.X, AExtent.LowerLeft.Y)) then
      begin
        if IsPointOnLine(APoint, AsCoor(AExtent.UpperRight.X, AExtent.LowerLeft.Y), AExtent.LowerLeft) then
          Result := True;
      end
      else
        Result := True;
    end
    else
      Result := True;
  end
  else
    Result := True;
end;

function IsExtentFullInExtent(const AExtent, BExtent: TlicgExtent): Boolean;
var
  FCode, SCode: TlicgOutCode;
begin
  FCode := PositionCode2D(BExtent, AExtent.LowerLeft);
  SCode := PositionCode2D(BExtent, AExtent.UpperRight);
  Result := (FCode = []) and (SCode = []);
end;

function ExtentOutExtent(AExtent, BExtent: TlicgExtent): TlicgExtent;
begin
  AExtent := ReorderExtent(AExtent);
  BExtent := ReorderExtent(BExtent);
  Result := AExtent;
  if BExtent.LowerLeft.x < AExtent.LowerLeft.x then
    Result.LowerLeft.x := BExtent.LowerLeft.x;
  if BExtent.UpperRight.x > AExtent.UpperRight.x then
    Result.UpperRight.x := BExtent.UpperRight.x;
  if BExtent.LowerLeft.y < AExtent.LowerLeft.y then
    Result.LowerLeft.y := BExtent.LowerLeft.y;
  if BExtent.UpperRight.y > AExtent.UpperRight.y then
    Result.UpperRight.y := BExtent.UpperRight.y;
end;

function IsNearPoint2D(const RP, P: TlicgCoor; const Aperture: Double; var Dist: Double): Boolean;
var
  TmpExtent: TlicgExtent;
begin
  TmpExtent.LowerLeft := AsCoor(RP.X - Aperture, RP.Y - Aperture);
  TmpExtent.UpperRight := AsCoor(RP.X + Aperture, RP.Y + Aperture);
  Result := PositionCode2D(TmpExtent, P) = [];
  if Result then
    Dist := Sqrt(Sqr(P.X - RP.X) + Sqr(P.Y - RP.Y));
end;

constructor TlicgDistPoint.Create(const _P: TlicgCoor; const _D: double);
begin
  inherited create;
  P := _P;
  D := _D;
end;

function SortDistPoints1(Item1, Item2: Pointer): Integer; // K¸Á¸kten B¸y¸e
begin
  if TlicgDistPoint(item1).D > TlicgDistPoint(item2).D then
    result := 1
  else if TlicgDistPoint(item1).D < TlicgDistPoint(item2).D then
    result := -1
  else
    result := 0;
end;

function SortDistPoints2(Item1, Item2: Pointer): Integer; // B¸y¸kten K¸Á¸e
begin
  if TlicgDistPoint(item1).D > TlicgDistPoint(item2).D then
    result := -1
  else if TlicgDistPoint(item1).D < TlicgDistPoint(item2).D then
    result := 1
  else
    result := 0;
end;

function VectDist2D(const V: IlicgVector; const Index: integer; From0Index:
  Boolean): Double;
var
  i: integer;
begin
  result := 0;
  if (Index >= 0) and (Index < V.Count) then
  begin
    if from0Index then
    begin
      for i := 0 to Index - 1 do
        Result := Result + _Distance(V[i], V[i + 1]);
    end
    else
    begin
      for i := V.Count - 1 to Index + 1 do
        Result := Result + _Distance(V[i], V[i - 1]);
    end;
  end;
end;

function Dist2DPerimeter(const Pt1, Pt2: TlicgCoor): Double;
begin
  if CurrCmdLine = nil then
    Result := Sqrt(Sqr(Pt1.X - Pt2.X) + Sqr(Pt1.Y - Pt2.Y))
  {else
  begin
    if CurrCmdLine.ActiveDrawBox.isLatlon then begin

        if  Assigned(CurrCmdLine.ActiveDrawBox.GIS.raster) then begin
          with CurrCmdLine.ActiveDrawBox.GIS.raster Do
            CalcDistance(Pt1.X,Pt1.Y, Pt2.X, Pt2.Y ,
                            true,
                            Result)
        end;
    end
    }
  else

    Result := Sqrt(Sqr(Pt1.X - Pt2.X) + Sqr(Pt1.Y - Pt2.Y));

  //end;

end;

function RightIntersection2D(const P, P1, P2: TlicgCoor): Boolean;
var
  R: Double;
begin
  Result := ((P.Y >= P1.Y) and (P.Y < P2.Y)) or ((P.Y < P1.Y) and (P.Y >= P2.Y));
  if not Result then
    Exit;
  R := (P.Y - P1.Y) * (P2.X - P1.X) / (P2.Y - P1.Y) + P1.X;
  Result := P.X <= R;
end;

function TransformBoundingExtent(const AExtent: TlicgExtent; const Matrix: TlicgMatrix): TlicgExtent;
var
  Extent1, Extent2: TlicgExtent;
begin
  Extent1 := TransformExtent(AExtent, Matrix);
  Extent2 := TransformExtent(licgExtent(AExtent.LowerLeft.x, AExtent.UpperRight.y,
    AExtent.UpperRight.x, AExtent.LowerLeft.y), Matrix);
  Result := ExtentOutExtent(Extent1, Extent2);
end;

function ChangeToOrtogonal(const Pt1, Pt2: TlicgCoor): TlicgCoor;
begin
  Result := Pt2;
  if Abs(Pt2.X - Pt1.X) > Abs(Pt2.Y - Pt1.Y) then
    Result.Y := Pt1.Y
  else
    Result.X := Pt1.X;
end;

function EQ(const a, b: Double): Boolean;
begin
  Result := (abs(a - b) <= _EPSILON_ULTRA);
end;

{line intersections}

function LineRel(const P1, P2, D1, D2: TlicgCoor; var P: TlicgCoor): TlicgLineRelations;
(* Returnes a code which indicates the relationship between the
   partition line (going from P1 to P2) and the division line (going
   from D1 to D2).  If the lines are not parallel the point where the
   lines will intersect is returned.  This point is not necessarilly
   within the boundaries of either line, but is the point at which the
   lines will intersect if they were infinitally extended in both
   directions.                                                         *)
var
  Denominator: Double;
  Numerator: Double;
  Fraction: Double;
  XInt, YInt: Double;
  XPar, YPar: Double;
  XDiv, YDiv: Double;
  dXPar, dYPar: Double;
  dXDiv, dYDiv: Double;
begin
  (* Compute the delta X and delta Y for both lines, and move all the
   parameters into real types to avoid arithmetic overflow error. *)
  XPar := P1.X;
  YPar := P1.Y;
  XDiv := D1.X;
  YDiv := D1.Y;
  dXPar := P2.X - P1.X;
  dYPar := P2.Y - P1.Y;
  dXDiv := D2.X - D1.X;
  dYDiv := D2.Y - D1.Y;

  Result := [];

  { First we compute the denominator and numerator to find
   the relation of the division line to the partition line.}
  Denominator := (dYPar * dXDiv) - (dXPar * dYDiv);
  Numerator := ((XPar - XDiv) * dYPar) + ((YDiv - YPar) * dXPar);
  { Find the intersection in relation to the division line. }
  if EQ(Denominator, 0.0) then
  begin { The two lines are parallel }
    Include(Result, lrParallel);
    { Find which side of the partition line the division line is on. }
    if EQ(Numerator, 0.0) then

    else if Numerator > 0.0 then
      Include(Result, lrDivToRight)
    else if Numerator < 0.0 then
      Include(Result, lrDivToLeft);
  end
  else
  begin { The lines are NOT parallel }
    Fraction := Numerator / Denominator;
    { Compute the intersection point }
    XInt := XDiv + dXDiv * Fraction;
    YInt := YDiv + dYDiv * Fraction;
    P.X := XInt;
    P.Y := YInt;
    (* Check if the intersection point actually falls on the
       start or end of the division line, and reassign fraction
       and numerator to reflect this.                           *)
    if EQ(P.X, D1.X) and EQ(P.Y, D1.Y) then
    begin
      if Fraction > 0.0 then
        Numerator := -Numerator;
      Fraction := 0.0;
    end
    else if EQ(P.X, D2.X) and EQ(P.Y, D2.Y) then
      Fraction := 1.0;

    { Find which side of the partition line the division line is on. }
    if Numerator > 0.0 then
      Include(Result, lrDivToRight)
    else if Numerator < 0.0 then
      Include(Result, lrDivToLeft)
    else
    begin
      { If numerator=0 we use denominator to find the side. }
      if Denominator < 0.0 then
        Include(Result, lrDivToRight)
      else if Denominator > 0.0 then
        Include(Result, lrDivToLeft);
    end;

    if Fraction < 0.0 then
      Include(Result, lrOffDivStart)
        { Intersection is beyond the start of the division line. }
    else if Fraction = 0.0 then
      Include(Result, lrAtDivStart)
        { Intersection at the start of the division line. }
    else if Fraction > 1.0 then
      Include(Result, lrOffDivEnd)
        { Intersection is beyond the end of the division line. }
    else if Fraction = 1.0 then
      Include(Result, lrAtDivEnd)
        { Intersection at the end of the division line. }
    else
      Include(Result, lrBetweenDiv);
    { Intersection between the start and the end of the division line. }
  end;

  (* Now we compute the denominator and numerator to find
     the relation of the partiton line to the division line. *)
  Denominator := (dYDiv * dXPar) - (dXDiv * dYPar);
  Numerator := ((XDiv - XPar) * dYDiv) + ((YPar - YDiv) * dXDiv);
  { Find the intersection in relation to the partition line. }
  if EQ(Denominator, 0.0) then
  begin { The two lines are parallel }
    { Find which side of the division line the partition line is on. }
    if EQ(Numerator, 0.0) then

    else if Numerator > 0.0 then
      Include(Result, lrParToRight)
    else if Numerator < 0.0 then
      Include(Result, lrParToLeft);
  end
  else
  begin { The lines are NOT parallel }
    Fraction := Numerator / Denominator;
    (* Check if the intersection point actually falls on the
       start or end of the partition line, and reassign fraction
       and numerator to reflect this.                            *)
    if EQ(P.X, P1.X) and EQ(P.Y, P1.Y) then
    begin
      if Fraction > 0.0 then
        Numerator := -Numerator;
      Fraction := 0.0;
    end
    else if EQ(P.X, P2.X) and EQ(P.Y, P2.Y) then
      Fraction := 1.0;

    { Find which side of the division line the partition line is on. }
    if Numerator > 0.0 then
      Include(Result, lrParToRight)
    else if Numerator < 0.0 then
      Include(Result, lrParToLeft)
    else
    begin
      { If numerator=0 we use denominator to find the side. }
      if Denominator < 0.0 then
        Include(Result, lrParToRight)
      else if Denominator > 0.0 then
        Include(Result, lrParToLeft);
    end;

    if Fraction < 0.0 then
      Include(Result, lrOffParStart)
        { Intersection is beyond the start of the partition line. }
    else if Fraction = 0.0 then
      Include(Result, lrAtParStart)
        { Intersection at the start of the partition line. }
    else if Fraction > 1.0 then
      Include(Result, lrOffParEnd)
        { Intersection is beyond the end of the partition line. }
    else if Fraction = 1.0 then
      Include(Result, lrAtParEnd)
        { Intersection at the end of the partition line. }
    else
      Include(Result, lrBetweenPar);
    { Intersection between the start and the end of the partition line. }
  end;
end;


(* Computes the centroid (center of gravity) of a polygon
 via weighted sum of signed triangle areas. *)

procedure FindCG(Vector: IlicgVector; var CG: TlicgCoor; ComputeCG: Boolean);
var
  cnt, N: Integer;
  A2, Areasum2: Double;
  Cent3: TlicgCoor;

  function Area2(const a, b, c: TlicgCoor): Double;
  begin
    Result := (b.X - a.X) * (c.Y - a.Y) - (c.X - a.X) * (b.Y - a.Y);
  end;

  procedure Centroid3(const p1, p2, p3: TlicgCoor; var c: TlicgCoor);
  begin
    c.X := p1.X + p2.X + p3.X;
    c.Y := p1.Y + p2.Y + p3.Y;
  end;

  procedure NormalCentroid;
  var
    cnt, n: Integer;
  begin
    n := Vector.Count;
    if n = 0 then
      Exit;
    for cnt := 0 to n - 1 do
    begin
      CG.X := CG.X + Vector[cnt].X;
      CG.Y := CG.Y + Vector[cnt].Y;
    end;
    CG.X := CG.X / n;
    CG.Y := CG.Y / n;
  end;

begin
  Areasum2 := 0;
  CG.X := 0;
  CG.Y := 0;
  if not ComputeCG or (Vector.Count < 3) then
  begin
    NormalCentroid;
    Exit;
  end;
  if EqualPoint2d(Vector[0], Vector[Vector.Count - 1]) then
    N := Vector.Count - 3
  else
    N := Vector.Count - 2;
  for cnt := 1 to N do
  begin
    Centroid3(Vector[0], Vector[cnt], Vector[cnt + 1], Cent3);
    A2 := Area2(Vector[0], Vector[cnt], Vector[cnt + 1]);
    CG.X := CG.X + A2 * Cent3.X;
    CG.Y := CG.Y + A2 * Cent3.Y;
    Areasum2 := Areasum2 + A2;
  end;
  if Areasum2 <> 0 then
  begin
    CG.X := CG.X / (3 * Areasum2);
    CG.Y := CG.Y / (3 * Areasum2);
  end
  else
    NormalCentroid;
end;

(* Determines if point (CX,CY) is on line that goes from (AX,AY) to
   (BX,BY)
   P is the point of perpendicular projection of C on AB*)

function PointIsOnSegment2D(const C, A, B: TlicgCoor): Boolean;
var
  r, s, L: Double;
  {P: TlicgCoor;}
begin
  Result := False;
  L := _Distance(A, B);
  if L = 0 then
    Exit;

  r := ((C.X - A.X) * (B.X - A.X) + (C.Y - A.Y) * (B.Y - A.Y)) / (L * L);
  (* P.X := A.X + r * (B.X - A.X);
  P.Y := A.Y + r * (B.Y - A.Y); *)
  s := ((A.Y - C.Y) * (B.X - A.X) - (A.X - C.X) * (B.Y - A.Y)) / (L * L);
  Result := (r >= 0) and (r <= 1) and (Abs(s) <= _EPSILON_ULTRA);
end;

function PrintersInstalled: Boolean;
var
  Flags: Integer;
  Level: Byte;
  Count, NumInfo: DWORD;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
    Level := 4;
  end
  else
  begin
    Flags := PRINTER_ENUM_LOCAL;
    Level := 5;
  end;
  Count := 0;
  EnumPrinters(Flags, nil, Level, nil, 0, Count, NumInfo);
  Result := Count > 0;
end;

procedure MaxBound(var bounds: TlicgCoor; const pt: TlicgCoor);
begin
  if pt.x > bounds.x then
    bounds.x := pt.x;
  if pt.y > bounds.y then
    bounds.y := pt.y;
end;

procedure MinBound(var bounds: TlicgCoor; const pt: TlicgCoor);
begin
  if pt.x < bounds.x then
    bounds.x := pt.x;
  if pt.y < bounds.y then
    bounds.y := pt.y;
end;

procedure CalcMaxMinBounds(var bounds: TlicgExtent; const Value: TlicgExtent);
begin
  MaxBound(bounds.UpperRight, Value.UpperRight);
  MinBound(bounds.LowerLeft, Value.LowerLeft);
end;

procedure CalcMaxMinBounds(var bounds: TlicgExtent; const MinValue, MaxValue: TlicgCoor);
begin
  MaxBound(bounds.UpperRight, MaxValue);
  MinBound(bounds.LowerLeft, MinValue);
end;

{ This routine will clip the vector of Geometry.Points to an area
  and will return the Resulting polygon }

procedure ClipPolygonToArea(Vector, ClippedVector: IlicgVector; const ClipArea: TlicgExtent);
var
  VisPoints, VisPoints1, cnt: Integer;
  TmpPt1, TmpPt2: TlicgCoor;
  TmpPts, FirstClipPts: array of TlicgCoor;
  ClipRes: TlicgClipCodes;
  TmpSize: Integer;
begin
  if Vector.Count < 3 then
    Exit;
  TmpSize := Vector.Count + 4;
  SetLength(TmpPts, TmpSize);
  SetLength(FirstClipPts, TmpSize);
  try
    VisPoints := 0;
    VisPoints1 := 0;
    if IsExtentFullInExtent(Vector.Extent, ClipArea) then
      ClippedVector.Assign(Vector)
    else
    begin
      for cnt := 0 to Vector.Count - 1 do
      begin
        TmpPt1 := Vector[cnt];
        if cnt < Vector.Count - 1 then
          TmpPt2 := Vector[cnt + 1]
        else
          TmpPt2 := Vector[0];
        ClipRes := ClipLineLeftRight2D(ClipArea, TmpPt1.X, TmpPt1.Y, TmpPt2.X, TmpPt2.Y);
        if not (ccNotVisible in ClipRes) then
        begin
          FirstClipPts[VisPoints1] := TmpPt1;
          Inc(VisPoints1);
        end;
        if ccSecond in ClipRes then
        begin
          FirstClipPts[VisPoints1] := TmpPt2;
          Inc(VisPoints1);
        end;
      end;
      FirstClipPts[VisPoints1] := FirstClipPts[0];
      Inc(VisPoints1);
      VisPoints := 0;
      for cnt := 0 to VisPoints1 - 2 do
      begin
        TmpPt1 := FirstClipPts[cnt];
        TmpPt2 := FirstClipPts[cnt + 1];
        ClipRes := ClipLineUpBottom2D(ClipArea, TmpPt1.X, TmpPt1.Y, TmpPt2.X, TmpPt2.Y);
        if not (ccNotVisible in ClipRes) then
        begin
          TmpPts[VisPoints] := TmpPt1;
          Inc(VisPoints);
        end;
        if ccSecond in ClipRes then
        begin
          TmpPts[VisPoints] := TmpPt2;
          Inc(VisPoints);
        end;
      end;
    end;
    if VisPoints > 1 then
      for cnt := 0 to VisPoints - 1 do
        ClippedVector.Add(TmpPts[cnt].x, TmpPts[cnt].y);
  finally
    Finalize(TmpPts);
    Finalize(FirstClipPts);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// this code was generousely donated by Jens Gruschel
// 2001 by Jens Gruschel (www.pegtop.de)
// vector algebra 2D
////////////////////////////////////////////////////////////////////////////////

function Add2D(const A, B: TlicgCoor): TlicgCoor; // A+B
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function Sub2D(const A, B: TlicgCoor): TlicgCoor; // A-B
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

function Mul2D(const A, B: TlicgCoor): Double; // A*B
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

function Ort2D(const A: TlicgCoor): TlicgCoor;
begin
  Result.X := -A.Y;
  Result.Y := A.X;
end;

function Len2D(const A: TlicgCoor): Double; // length(A)
begin
  Result := Sqrt(Sqr(A.X) + Sqr(A.Y));
end;

function DistLinePoint2D(const P, V, X: TlicgCoor): Double;
// distance between X and a line P+rV
// V may not be (0,0) !!!
var
  N: TlicgCoor;
begin
  N := Ort2D(V);
  Result := Mul2D(Sub2D(X, P), N) / Len2D(N);
end;

function NearestCoor(const P: TlicgCoor; V: IlicgVector; w_otele: double = 0;
  h_otele: double = 0): TlicgCoor;
var
  i, n: integer;
  D, T: double;
begin
  Result := _INVALID_COOR;
  T := 0;
  n := -1;
  for i := 0 to V.Count - 1 do
  begin
    D := _Distance(V[i], P);
    if (T > D) or (T = 0) then
    begin
      T := D;
      n := i;
    end;
  end;
  if n <> -1 then
  begin
    Result.X := V[n].X + w_otele;
    Result.Y := V[n].Y + h_otele;

  end;
end;

function NearestLinePoint2D(const P, V, X: TlicgCoor): Double;
// nearest point of the line P+rV to X in Units of V
// V may not be (0,0) !!!
// to gain the corresponding point:
// Result.X := P.X+V.X*Result;
// Result.Y := P.Y+V.Y*Result;
var
  T: Double;
begin
  if V.Y = 0 then
  begin // V.X may not be 0
    T := V.X + Sqr(V.Y) / V.X; // T cannot be 0 if V.X is not 0
    Result := (X.X - P.X - V.Y / V.X * (P.Y - X.Y)) / T;
  end
  else
  begin // V.Y may not be 0
    T := V.Y + Sqr(V.X) / V.Y; // T cannot be 0 if V.Y is not 0
    Result := (X.Y - P.Y - V.X / V.Y * (P.X - X.X)) / T;
  end;
end;

{ calculates minimum distance }

procedure GetMinimumDistance2D(Poly1, Poly2: IlicgVector; var Distance: Double;
  var Min1, Min2: TlicgCoor);
var
  N1, N2, I1, I2: Integer;
  D, R: Double;
  P1, P2, V: TlicgCoor;
  Nearest: TlicgCoor;
begin
  N1 := Poly1.Count;
  N2 := Poly2.Count;
  Distance := _MAXCOOR; // very large distance
  // check corners of second polygon with lines of first one:
  for I1 := 0 to N1 - 2 do
  begin
    P1 := Poly1[I1]; // first corner
    P2 := Poly1[(I1 + 1) {Mod N1}]; // second corner
    V := Sub2D(P2, P1); // line vector
    if (V.X = 0) and (V.Y = 0) then
      Continue;
    for I2 := 0 to N2 - 1 do
    begin
      R := NearestLinePoint2D(P1, V, Poly2[I2]);
      if R <= 0 then
      begin
        Nearest := P1;
      end
      else if R >= 1 then
      begin
        Nearest := P2;
      end
      else
      begin
        Nearest.X := P1.X + R * V.X;
        Nearest.Y := P1.Y + R * V.Y;
      end;
      D := Len2D(Sub2D(Poly2[I2], Nearest));
      if D < Distance then
      begin // smaller distance found
        Distance := D;
        Min1 := Poly2[I2];
        Min2 := Nearest;
      end;
    end;
  end;
  // check corners of first polygon with lines of second one:
  for I2 := 0 to N2 - 2 do
  begin
    P1 := Poly2[I2]; // first corner
    P2 := Poly2[(I2 + 1) {Mod N2}]; // second corner
    V := Sub2D(P2, P1); // line vector
    if (V.X = 0) and (V.Y = 0) then
      Continue;
    for I1 := 0 to N1 - 1 do
    begin
      R := NearestLinePoint2D(P1, V, Poly1[I1]);
      if R <= 0 then
      begin
        Nearest := P1;
      end
      else if R >= 1 then
      begin
        Nearest := P2;
      end
      else
      begin
        Nearest.X := P1.X + R * V.X;
        Nearest.Y := P1.Y + R * V.Y;
      end;
      D := Len2D(Sub2D(Poly1[I1], Nearest));
      if D < Distance then
      begin // smaller distance found
        Distance := D;
        Min1 := Poly1[I1];
        Min2 := Nearest;
      end;
    end;
  end;
end;

procedure GetMinimumDistance2D_2(const Pivot: TlicgCoor; const Poly: IlicgVector;
  var Distance: Double; var Min1, Min2: TlicgCoor);
begin
  ClosestLineInVector(Pivot, Poly, Min1, Min2, Distance);
end;


// Return the intersection rectangle (if any) of this and r

function IntersectExtent(const r1, r2: TlicgExtent): TlicgExtent;
begin
  Result.LowerLeft.x := Max(r1.LowerLeft.x, r2.LowerLeft.x);
  Result.LowerLeft.y := Max(r1.LowerLeft.y, r2.LowerLeft.y);
  Result.UpperRight.x := Min(r1.UpperRight.x, r2.UpperRight.x);
  Result.UpperRight.y := Min(r1.UpperRight.y, r2.UpperRight.y);

  if (Result.LowerLeft.x > Result.UpperRight.x) or (Result.LowerLeft.y > Result.UpperRight.y) then
    Result := _NULL_EXTENT; // no intersection
end;

function IsExtensionEmpty(const E: TlicgExtent): Boolean;
begin
  Result := CompareMem(@_NULL_EXTENT, @E, Sizeof(TlicgExtent));
end;

{ this function returns the perpendicular projection of point C on
  the lines that goes from A to B }

/// <summary>
 /// Hatta noktan˝n en k˝sa uzankl˝˝
 /// <Uyari> MinimumDistanceFromPointToLine ve MinimumDistanceFromPointToSegment
 /// Perpend, PointLineDistance2D
 /// fonksiyonlar˝ da kullan˝labilir</Uyari>
 /// </summary>
function Perpend(const C, A, B: TlicgCoor): TlicgCoor;
var
  r, L: Double;
begin
  L := _Distance(A, B);
  if L = 0 then
  begin
    Result := A;
    Exit;
  end;
  r := ((C.X - A.X) * (B.X - A.X) + (C.Y - A.Y) * (B.Y - A.Y)) / (L * L);
  Result.X := A.X + r * (B.X - A.X);
  Result.Y := A.Y + r * (B.Y - A.Y);
end;

{ TlicgRubberItem }

constructor TlicgRubberItem.Create(const Pts: array of TPoint; PenColor: TColor;
  PenWidth: Byte; PenStyle: TPenStyle; FillColor: TColor);
var
  I: Integer;
begin
  inherited Create;
  SetLength(Self.Pts, Length(Pts));
  for I := 0 to Length(Pts) - 1 do
    Self.Pts[I] := Pts[I];
  Self.PenColor := PenColor;
  Self.PenWidth := PenWidth;
  Self.PenStyle := PenStyle;
  Self.FillColor := FillColor;
end;

destructor TlicgRubberItem.Destroy;
begin
  Finalize(Pts);
  inherited;
end;

function TlicgRubberItem.IsClosed: Boolean;
var
  LenPts: Integer;
begin
  LenPts := Length(Pts);
  Result := (LenPts > 1) and (Pts[0].X = Pts[LenPts - 1].X) and (Pts[0].Y = Pts[LenPts - 1].Y);
end;

{ TlicgRubberList }

constructor TlicgRubberList.Create;
begin
  inherited Create;
  FRubberList := TList.Create;
end;

destructor TlicgRubberList.Destroy;
begin
  Clear;
  FRubberList.Free;
  inherited;
end;

procedure TlicgRubberList.Draw(ACanvas: IlicgCanvas);
var
  RubberItem: TlicgRubberItem;
  I: integer;
  //parts: array[0..0] of integer;
begin
  if FRubberList.Count = 0 then
    exit;
  for I := 0 to FRubberList.Count - 1 do
  begin
    RubberItem := TlicgRubberItem(FRubberList[I]);
    ACanvas.PenStyle := RubberItem.PenStyle;
    ACanvas.PenColor := RubberItem.PenColor;
    ACanvas.PenWidth := RubberItem.PenWidth;
    if (RubberItem.FillColor <> clNone) and RubberItem.IsClosed then
    begin
      ACanvas.BrushStyle := bsSolid;
      ACanvas.BrushColor := RubberItem.FillColor;
      Windows.Polygon(ACanvas.Handle, RubberItem.Pts[0], Length(RubberItem.Pts));
    end
    else
      Windows.Polyline(ACanvas.Handle, RubberItem.Pts[0], Length(RubberItem.Pts));
  end;
end;

procedure TlicgRubberList.Add(ACanvas: TCanvas; Pts: array of TPoint; PenColor:
  TColor; PenWidth: Byte; PenStyle: TPenStyle; FillColor: TColor);
var
  Item: TlicgRubberItem;
  LenPts: integer;
  IsClosed: boolean;
begin
  if ACanvas = nil then
  begin
    Item := TlicgRubberItem.Create(Pts, PenColor, PenWidth, PenStyle, FillColor);
    FRubberList.Add(Item);
  end
  else
  begin
    LenPts := Length(Pts);
    IsClosed := (LenPts > 1) and (Pts[0].X = Pts[LenPts - 1].X) and (Pts[0].Y =
      Pts[LenPts - 1].Y);
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := PenColor;
    ACanvas.Pen.Width := PenWidth;
    if IsClosed and (FillColor <> clNone) then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := FillColor;
      ACanvas.Polygon(Pts);
    end
    else
      ACanvas.Polyline(Pts);
  end;
end;

procedure TlicgRubberList.Clear;
var
  I: Integer;
begin
  for I := 0 to FRubberList.Count - 1 do
    TlicgRubberItem(FRubberList[I]).Free;
  FRubberList.Clear;
end;

function TlicgRubberList.GetCount: Integer;
begin
  Result := FRubberList.Count;
end;

{ TlicgBaseTransformParamsList }

destructor TlicgBaseTransformParamsList.Destroy;
begin
  inherited;
end;

{ TlicgBaseGrapher }

constructor TlicgBaseGrapher.Create(ADevice: TlicgActiveDevice; DrawBox: TObject = nil);
begin
  inherited Create;
end;

destructor TlicgBaseGrapher.Destroy;
begin
  inherited;
end;

{ TlicgBasePointList }

constructor TlicgBasePointList.Create(Size: Integer);
begin
  inherited Create;
end;

destructor TlicgBasePointList.Destroy;
begin
  inherited;
end;

{ TlicgBaseGradient }

constructor TlicgBaseGradient.Create;
begin
  inherited Create;
end;

{ TlicgBaseNodeAssocList }

destructor TlicgBaseNodeAssocList.Destroy;
begin

  inherited;
end;

procedure OutOfResources;
begin
  raise EOutOfResources.Create(SOutOfResources);
end;

procedure GDIError;
const
  BufSize = 256;
var
  ErrorCode: Integer;
{$IF DEFINED(CLR)}
  Buf: StringBuilder;
{$ELSE}
  Buf: array [Byte] of Char;
{$ENDIF}
begin
{$IF DEFINED(CLR)}
  Buf := StringBuilder.Create(BufSize);
{$ENDIF}
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, BufSize, nil) <> 0) then
{$IF DEFINED(CLR)}
    raise EOutOfResources.Create(Buf.ToString)
{$ELSE}
    raise EOutOfResources.Create(Buf)
{$ENDIF}
  else
    OutOfResources;
end;

function GDICheck(Value: THandle): THandle;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

(*  ilker dei˛tirme Ez orj den Delphi yeni s¸r¸me g¸ncellendi.
procedure GDIError;
var
  ErrorCode: Integer;
  Buf: array[Byte] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, Sizeof(Buf), nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
    OutOfResources;
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then
    GDIError;
  Result := Value;
end; *)

function ExtentWidth(const AExtent: TlicgExtent): Double;
begin
  Result := Abs(AExtent.UpperRight.X - AExtent.LowerLeft.X);
end;

function ExtentHeight(const AExtent: TlicgExtent): Double;
begin
  Result := Abs(AExtent.UpperRight.Y - AExtent.LowerLeft.Y);
end;

 //String numeric bir deer mi?  Gˆnderilen string iÁeriinin
 //geÁerli bir say˝sal deer olup olmad˝˝n˝ dˆnd¸r¸r.
function StrContainAnyNumber(tmpStr: string): Boolean;
var
  cC, lengthStr: Integer;
begin
  tmpStr := SysUtils.Trim(tmpStr);
  lengthStr := Length(tmpStr);
  for cC := 1 to lengthStr do
  begin
    case ord(tmpStr[cC]) of
      48, 49, 50, 51, 52, 53, 54, 55, 56, 57:
        begin
          Result := true;
        end;
    else
      Result := False;
    end; //case
  end;
end;

//This procedure provides that between Which Geometry.Points is the current point and
//returns first and second index of Geometry.Points.
//Vect: Entity.Geometry.Points
//Grapher: For aperture calculate
procedure BetweenWhichPoints(Vect: IlicgVector; CurrPoint: TlicgCoor; Grapher:
  TlicgBaseGrapher; Aperture: Integer; var FirstI, SecondI: Integer; var
  Distance: Double);
var
  MinDist: Double;
  cnt, partcount: integer;
  RealAperture: TlicgCoor;
  TmpVect: IlicgVector;
begin
  Aperture := Aperture div 2;
  RealAperture := AsCoor(Grapher.DistToRealX(Aperture), Grapher.DistToRealY(Aperture));
  if RealAperture.X > RealAperture.Y then
    MinDist := RealAperture.X
  else
    MinDist := RealAperture.Y;
  MinDist := Sqrt(2) * MinDist;
  TmpVect := Licad.CreateEntityFactory.MakeVector(3, 2);
  FirstI := -1;
  SecondI := -1;
  partcount := 0;
  for cnt := 0 to Vect.Count - 2 do
  begin
    TmpVect[0] := Vect[cnt];
    TmpVect[1] := Vect[cnt + 1];
    if Vect.PartEnd(partcount) = cnt then
    begin
      inc(partcount);
      Continue;
    end;
    if (EqualPoint2D(TmpVect[0], CurrPoint, -6)) or (EqualPoint2D(TmpVect[1],
      CurrPoint, -6)) then
    begin
      FirstI := cnt;
      SecondI := cnt + 1;
      break;
    end;

    if PointOnPolyLine2D_Vector(TmpVect, 0, CurrPoint, Distance, MinDist,
      IDENTITY_MATRIX2D, False) >= PICKED_INTERIOR then
    begin
      FirstI := cnt;
      SecondI := cnt + 1;
      Break;
    end;
  end;
  TmpVect := nil;
end;

function Extent(x1, y1, x2, y2: Double): TlicgExtent;
begin
  Result.LowerLeft.X := x1;
  Result.LowerLeft.Y := y1;
  Result.UpperRight.X := x2;
  Result.UpperRight.Y := y2;
end;

function PointToExtent(APoint: TlicgCoor): TlicgExtent;
begin
  Result := Extent(APoint.X, APoint.Y, APoint.X, APoint.Y)
end;

function ExtentMidPoint(AExtent: TlicgExtent): TlicgCoor;
begin
  with AExtent do
  begin
    Result.X := (LowerLeft.X + UpperRight.X) / 2;
    Result.Y := (LowerLeft.Y + UpperRight.Y) / 2;
  end;
end;

procedure SetArc(FPoints: IlicgVector; const X, Y, StartAng, EndAng, Rad: Double);
var
  _ang, fark: double;
  p: TlicgCoor;
begin

  FPoints.Clear;

  fark := abs(StartAng - EndAng);

  if fark > pi then
    fark := 2 * pi - fark;
  _ang := StartAng + fark / 2;

  p := Lider.CG.Com.Math.GetCoor(x, y, (pi / 2) - StartAng, rad, atRadian);
  FPoints.Add(p.x, p.y);
  p := Lider.CG.Com.Math.GetCoor(x, y, (pi / 2) - _ang, rad, atRadian);
  FPoints.Add(p.x, p.y);
  p := Lider.CG.Com.Math.GetCoor(x, y, (pi / 2) - EndAng, rad, atRadian);
  FPoints.Add(p.x, p.y);
end;

//TlicgArc nesnesinin SetArc metodu (Import iÁin d˝˛ar˝ al˝nd˝)
procedure SetArc(FPoints: IlicgVector; const CX, CY, R, StartAng, NumRadians:
  Double; IsCounterClockWise: Boolean);

  procedure ArcAddPoint(const ang: Double);
  begin
    FPoints.Add((CX + R * Cos(ang)), (CY + R * Sin(ang)));
  end;

begin
  (* StartAng and EndAng (in radians) always CounterClockWise *)
  FPoints.DisableEvents := True;
  FPoints.Clear;
  if IsCounterClockWise then
  begin
    ArcAddPoint(StartAng);
    ArcAddPoint(StartAng + NumRadians / 2);
    ArcAddPoint(StartAng + NumRadians);
  end
  else
  begin
    ArcAddPoint(StartAng);
    ArcAddPoint(StartAng - NumRadians / 2);
    ArcAddPoint(StartAng - NumRadians);
  end;
  FPoints.DisableEvents := False;
end;

function StrIsNumeric(Str: string): Boolean;
var
  I: Integer;
begin
  Result := (Length(Str) > 0);

  for I := Length(Str) downto 1 do
  begin
    if not (Str[I] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '/']) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure VectorElevationChange(var AVector: IlicgVector; AElevation: Double);
begin
  for var I := 0 to AVector.Count - 1 do
  begin
    AVector.Z[I] := AElevation;
  end;
end;

procedure VectorElevationChange(var AEntity: IlicgEntity; AElevation: Double);
begin
  for var I := 0 to AEntity.Geometry.Points.Count - 1 do
  begin
    AEntity.Geometry.Points.Z[I] := AElevation;
  end;
end;

procedure ExtentToVector(AExtent: TlicgExtent; var AVector: IlicgVector);
var
  p1, p2, p3, p4: TlicgCoor;
  a, b, alfa1, alfa2: Double;
begin
  a := AExtent.UpperRight.Y - AExtent.LowerLeft.Y;
  b := AExtent.UpperRight.X - AExtent.LowerLeft.X;
  Alfa1 := ArcCos(a / (_Distance(AsCoor(AExtent.LowerLeft.X, AExtent.LowerLeft.Y),
    AsCoor(AExtent.UpperRight.X, AExtent.UpperRight.Y))));
  p2.y := AExtent.LowerLeft.Y + (a * Cos(Angle2DS(AExtent.LowerLeft, AExtent.UpperRight) - Alfa1));
  p2.x := AExtent.LowerLeft.X + (a * Sin(Angle2DS(AExtent.LowerLeft, AExtent.UpperRight) - Alfa1));

  Alfa2 := (Pi / 2) - Alfa1;
  p4.y := AExtent.LowerLeft.Y + (b * Cos(Angle2DS(AExtent.LowerLeft, AExtent.UpperRight) + Alfa2));
  p4.x := AExtent.LowerLeft.X + (b * Sin(Angle2DS(AExtent.LowerLeft, AExtent.UpperRight) + Alfa2));

  p1.x := AExtent.LowerLeft.X;
  p1.y := AExtent.LowerLeft.Y;
  p3.x := AExtent.UpperRight.X;
  p3.y := AExtent.UpperRight.Y;

  if AVector <> nil then
  begin
    AVector.Clear;
    AVector.Add(p1.x, p1.y);
    AVector.Add(p2.x, p2.y);
    AVector.Add(p3.x, p3.y);
    AVector.Add(p4.x, p4.y);
  end;
end;

procedure ExtentToVector(AExtent: TlicgExtent; var AVector: IlicgVector; var P1, P2, P3, P4: TlicgCoor);
var
  a, b, alfa1, alfa2: Double;
begin
  a := AExtent.UpperRight.Y - AExtent.LowerLeft.Y;
  b := AExtent.UpperRight.X - AExtent.LowerLeft.X;
  Alfa1 := ArcCos(a / (_Distance(AsCoor(AExtent.LowerLeft.X, AExtent.LowerLeft.Y),
    AsCoor(AExtent.UpperRight.X, AExtent.UpperRight.Y))));
  p2.y := AExtent.LowerLeft.Y + (a * Cos(Angle2DS(AExtent.LowerLeft, AExtent.UpperRight) - Alfa1));
  p2.x := AExtent.LowerLeft.X + (a * Sin(Angle2DS(AExtent.LowerLeft, AExtent.UpperRight) - Alfa1));

  Alfa2 := (Pi / 2) - Alfa1;
  p4.y := AExtent.LowerLeft.Y + (b * Cos(Angle2DS(AExtent.LowerLeft, AExtent.UpperRight) + Alfa2));
  p4.x := AExtent.LowerLeft.X + (b * Sin(Angle2DS(AExtent.LowerLeft, AExtent.UpperRight) + Alfa2));

  p1.x := AExtent.LowerLeft.X;
  p1.y := AExtent.LowerLeft.Y;
  p3.x := AExtent.UpperRight.X;
  p3.y := AExtent.UpperRight.Y;

  if AVector <> nil then
  begin
    AVector.Clear;
    AVector.Add(p1.x, p1.y);
    AVector.Add(p2.x, p2.y);
    AVector.Add(p3.x, p3.y);
    AVector.Add(p4.x, p4.y);
  end;
end;

{ SpatialReference kullan˝ld˝˝ndan fonksiyonlar bo˛a Á˝km˝˛t˝r.


function ProjectionPoint1 (const Ent: IUnknown; const aXIn, aYIn: Double; var aXOut, aYOut: Double): Boolean;
Var GIS:TlicgBaseGIS;
  _onTheFlyProjection,
  _projection:GM_Projection_tExt;
  np, P : TlicgCoor;
begin

 result := false;
 P := AsCoor(aXIn,aYIn);
 np:= _INVALID_COOR;
 if CompareMem(@P,@nP, SIZEOF(TlicgCoor)) then exit;
 if Ent=Nil  then Exit;
 GIS := getProjectionGISFromEnt(Ent as IlicgEntity);
 if GIS= nil then exit;
 if not Gis.SpatialReference.IsProjectionOntheFly then exit;
 with GIS do begin
  _onTheFlyProjection := SpatialReference.OntheflyProjection;
  _projection         := Projection;

   if Raster<>nil then
   begin
     result :=  Raster.GM_ProjectPoint(axIn, aYIn,
       aXOut,aYOut, @_Projection, @_onTheFlyProjection);
   end;
 end;
end;

function ProjectionPoint1L (const layer: TObject; const aXIn, aYIn: Double; var aXOut, aYOut: Double): Boolean;
Var GIS:TlicgBaseGIS;
  _onTheFlyProjection,
  _projection:GM_Projection_tExt;
  np, P : TlicgCoor;
begin
 result := false;
 P := AsCoor(aXIn,aYIn);
 np:= _INVALID_COOR;

 if CompareMem(@P,@nP, SIZEOF(TlicgCoor)) then exit;
 if layer=Nil  then Exit;
 GIS := TlicgBaseLayer(layer)._GIS;
 if GIS= nil then exit;

 if not Gis.SpatialReference.IsProjectionOntheFly then exit;

 with GIS do begin
  _onTheFlyProjection := SpatialReference.OntheflyProjection;
  _projection         := Projection;
   if Raster<>nil then
     result :=  Raster.GM_ProjectPoint(axIn, aYIn, aXOut,aYOut, @_Projection, @_onTheFlyProjection);
 end;
end;

function ProjectionPoint2 (const Entity: IUnknown;
   const aXIn, aYIn: Double; var aXOut, aYOut: Double): Boolean;
var
  GIS: TlicgBaseGIS;
  _onTheFlyProjection,
  _projection:GM_Projection_tExt;
  np, P : TlicgCoor;
begin

 result := false;
 P := AsCoor(aXIn,aYIn);
 np:= _INVALID_COOR;

 if CompareMem(@P,@nP, SIZEOF(TlicgCoor)) then exit;

 GIS := getProjectionGISFromEnt(Entity as IlicgEntity);

 if GIS= nil then exit;
 if not Gis.SpatialReference.IsProjectionOntheFly then exit;

 with GIS do begin
  _onTheFlyProjection :=SpatialReference.OntheflyProjection;
  _projection         := Projection;
   if Raster<>nil then
   begin
     result :=  Raster.GM_ProjectPoint(axIn, aYIn,
       aXOut,aYOut, @_onTheFlyProjection, @_Projection);
   end;
 end;
end;

function ProjectionPoint2L (const layer: TObject;
   const aXIn, aYIn: Double; var aXOut, aYOut: Double): Boolean;
var
  GIS: TlicgBaseGIS;
  _onTheFlyProjection,
  _projection:GM_Projection_tExt;
  np, P : TlicgCoor;
begin

 result := false;
 P := AsCoor(aXIn,aYIn);
 np:= _INVALID_COOR;

 if CompareMem(@P,@nP, SIZEOF(TlicgCoor)) then exit;


 if layer=Nil  then Exit;
 GIS := TlicgBaseLayer(layer)._gis;
 if not Gis.SpatialReference.IsProjectionOntheFly then exit;

 if GIS= nil then exit;

 with GIS do begin
  _onTheFlyProjection := SpatialReference.OntheflyProjection;
  _projection         := Projection;
   if Raster<>nil then
   begin
     result :=  Raster.GM_ProjectPoint(axIn, aYIn,
       aXOut,aYOut, @_onTheFlyProjection, @_Projection);
   end;
 end;
end;}

function EzMatrixToStMatrix(const ezMatrix: TlicgMatrix): TStMatrix;
var
  I, C: Integer;
begin
  result := mmake(3, 3);
  for I := 0 to 2 do
  begin
    for C := 0 to 2 do
    begin
      Result[I, C] := ezMAtrix.matrix[I, C];
    end;
  end;
end;

function StMatrixToEzMatrix(const _stMAtrix: TStMatrix): TlicgMatrix;
var
  I, C: Integer;
begin
  for I := 0 to 2 do
  begin
    for C := 0 to 2 do
    begin
      Result.matrix[I, C] := _stMAtrix[I, C];
    end;
  end;
end;

procedure SortOrderList(Layer: TObject; Solver: TObject; ol: IInterface);
var
  SortList: TlicgSortList;
  templ: IlicgIntegerList;
  I, J, Len: Integer;
begin
  SortList := TlicgMemSortList.Create;
  try
    { generate the sort fields }
    for I := 0 to TlicgBaseMainExpr(Solver).OrderByCount - 1 do
    begin
      Len := 0;
      if TlicgBaseMainExpr(Solver).OrderByList[I].ExprType = ttString then
      begin
        Len := TlicgBaseMainExpr(Solver).OrderByList[I].MaxLen;
      end;
      SortList.AddField(TlicgBaseMainExpr(Solver).OrderByList[I].ExprType, Len,
        TlicgBaseMainExpr(Solver).OrderDescending[I]);
    end;
    for I := 0 to IlicgIntegerList(ol).Count - 1 do
    begin
      TlicgBaseLayer(Layer).Recno := IlicgIntegerList(ol)[I];
      TlicgBaseLayer(Layer).Synchronize;
      SortList.Insert;
      SortList.SourceRecno := IlicgIntegerList(ol)[I];
      for J := 0 to TlicgBaseMainExpr(Solver).OrderByCount - 1 do
      begin
        case TlicgBaseMainExpr(Solver).OrderByList[J].ExprType of
          ttString:
            SortList.Fields[J].Asstring := TlicgBaseMainExpr(Solver).OrderByList
              [J].Asstring;
          ttFloat:
            SortList.Fields[J].AsFloat := TlicgBaseMainExpr(Solver).OrderByList[J].AsFloat;
          ttInteger:
            SortList.Fields[J].AsInteger := TlicgBaseMainExpr(Solver).OrderByList
              [J].AsInteger;
          ttBoolean:
            SortList.Fields[J].AsBoolean := TlicgBaseMainExpr(Solver).OrderByList
              [J].AsBoolean;
        end;
      end;
    end;
    SortList.Sort; // do sort
    { now create a new order list }
    templ := CreateIntegerList; // TlicgIntegerList.Create;
    for I := 1 to SortList.Count do
    begin
      SortList.Recno := I;
      templ.Add(SortList.SourceRecno);
    end;
    ol := nil; // free old list
    ol := templ; // assign new order list
  finally
    SortList.Free;
  end;
end;

function ConvertToTurkish(Value: string): string;
begin
  Result := Value;
  Result := StringReplace(Result, '¶', '–', [rfReplaceall]);
  Result := StringReplace(Result, 'ö', '‹', [rfReplaceall]);
  Result := StringReplace(Result, 'û', 'ﬁ', [rfReplaceall]);
  Result := StringReplace(Result, 'ò', '›', [rfReplaceall]);
  Result := StringReplace(Result, 'ô', '÷', [rfReplaceall]);
  Result := StringReplace(Result, 'Ä', '«', [rfReplaceall]);
  Result := StringReplace(Result, 'á', 'Á', [rfReplaceall]);
  Result := StringReplace(Result, 'î', 'ˆ', [rfReplaceall]);
  Result := StringReplace(Result, 'i', 'i', [rfReplaceall]);
  Result := StringReplace(Result, 'ü', '˛', [rfReplaceall]);
  Result := StringReplace(Result, 'Å', '¸', [rfReplaceall]);
  Result := StringReplace(Result, 'ß', '', [rfReplaceall]);
  Result := StringReplace(Result, 'ç', '˝', [rfReplaceall]);
  //¶öûòôÄáîiüÅßç
  //–‹ﬁ›÷«Áˆi˛¸˝
end;

{
procedure SetReadOnlyProjects(GIS: TObject);
var
 i: integer;
 p1,p2: GM_Projection_tExt;
begin
  with CurrCmdLine.ActiveDrawBox.GisiCover do begin
    if IsOnTheFlyProjection then
    begin
      p1 :=  onTheFlyProjection;

      for i :=0 to GISList.Count-1 do begin
         p2 := GISList[i].GIS.Projection;
        if uppercase(GISList[i].GIS.filename)=uppercase(TlicgBaseGis(GIS).filename) then
          GISList[i].GIS.ReadOnly := not CompareMem(@p1,@p2,sizeof(GM_Projection_tExt))
        else
          GISList[i].GIS.ReadOnly := true;
      end;
    end
    else begin
      for i :=0 to GISList.Count-1 do begin
        if uppercase(GISList[i].GIS.filename)=uppercase(TlicgBaseGis(GIS).filename) then
          GISList[i].GIS.ReadOnly := false
        else
          GISList[i].GIS.ReadOnly := true;

      end;
    end;
  end;
end;
}

procedure ClosestPointOnSegmentFromPoint(const x1, y1, x2, y2, Px, Py: Double;
  out Nx, Ny: Double);
var
  Vx: Double;
  Vy: Double;
  Wx: Double;
  Wy: Double;
  c1: Double;
  c2: Double;
  Ratio: Double;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Wx := Px - x1;
  Wy := Py - y1;

  c1 := Vx * Wx + Vy * Wy;

  if c1 <= 0 then
  begin
    Nx := x1;
    Ny := y1;
    Exit;
  end;

  c2 := Vx * Vx + Vy * Vy;

  if c2 <= c1 then
  begin
    Nx := x2;
    Ny := y2;
    Exit;
  end;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;

procedure ClosestPointOnLineFromPoint(const x1, y1, x2, y2, Px, Py: double; out Nx, Ny: double);
var
  Vx: double;
  Vy: double;
  Wx: double;
  Wy: double;
  c1: double;
  c2: double;
  Ratio: double;
begin

  Vx := x2 - x1;
  Vy := y2 - y1;
  if (Vx = 0) and (Vy = 0) then
  begin
    Nx := x1;
    Ny := y1;
    Exit;
  end;
  Wx := Px - x1;
  Wy := Py - y1;

  c1 := Vx * Wx + Vy * Wy;
  c2 := Vx * Vx + Vy * Vy;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;

/// <summary>
 /// Hatta noktan˝n en k˝sa uzankl˝˝
 /// MinimumDistanceFromPointToLine ve MinimumDistanceFromPointToSegment
 /// Perpend, PointLineDistance2D
 /// fonksiyonlar˝ da kullan˝labilir
 /// </summary>
function MinimumDistanceFromPointToSegment(const Px, Py, x1, y1, x2, y2: Double): Double;
var
  Nx: Double;
  Ny: Double;
begin
  ClosestPointOnSegmentFromPoint(x1, y1, x2, y2, Px, Py, Nx, Ny);
  Result := _Distance(Px, Py, Nx, Ny);
end;

/// <summary>
 /// Hatta noktan˝n en k˝sa uzankl˝˝
 /// <Uyari> MinimumDistanceFromPointToLine ve MinimumDistanceFromPointToSegment
 /// Perpend, PointLineDistance2D
 /// fonksiyonlar˝ da kullan˝labilir</Uyari>
 /// </summary>
function MinimumDistanceFromPointToLine(const Px, Py, x1, y1, x2, y2: Double): double;
var
  Nx: Double;
  Ny: Double;
begin
  ClosestPointOnLineFromPoint(x1, y1, x2, y2, Px, Py, Nx, Ny);
  Result := _Distance(Px, Py, Nx, Ny);
end;

function ClosestLineInVector(const P: TlicgCoor; V: IlicgVector; var P1, P2:
  TlicgCoor; var MinDistance: Double): Boolean;
var
  i: integer;
  D: Double;
begin
  result := false;
  if V.Count < 2 then
    exit;

  P1 := _INVALID_COOR;
  P2 := _INVALID_COOR;

  try

    MinDistance := MinimumDistanceFromPointToSegment(P.x, P.y, V.X[0], V.Y[0], V.X
      [1], V.Y[1]);

    P1 := V[0];
    P2 := V[1];

    for i := 1 to V.Count - 2 do
    begin
      D := MinimumDistanceFromPointToSegment(P.x, P.y, V.X[i], V.Y[i], V.X[i + 1],
        V.Y[i + 1]);
      if D < MinDistance then
      begin
        MinDistance := D;
        P1 := V[i];
        P2 := V[i + 1];
      end;
    end;
  finally
    Result := (not CompareMem(@P1, @_INVALID_COOR, Sizeof(TlicgCoor))) and (not
      CompareMem(@P2, @_INVALID_COOR, Sizeof(TlicgCoor)));
  end;
end;

function ClosestLineInVector(const P: TlicgCoor; V: IlicgVector; var P1, P2:
  integer; var MinDistance: Double): Boolean;
var
  i: integer;
  D: Double;
begin
  result := false;
  if V.Count < 2 then
    exit;

  P1 := -1;
  P2 := -1;

  try
    MinDistance := MinimumDistanceFromPointToSegment(P.x, P.y, V.X[0], V.Y[0], V.X
      [1], V.Y[1]);

    P1 := 0;
    P2 := 1;

    for i := 1 to V.Count - 2 do
    begin
      D := MinimumDistanceFromPointToSegment(P.x, P.y, V.X[i], V.Y[i], V.X[i + 1],
        V.Y[i + 1]);
      if D < MinDistance then
      begin
        MinDistance := D;
        P1 := i;
        P2 := i + 1;
      end;
    end;
  finally
    Result := (P1 <> -1) and (P2 <> -1);
  end;
end;

function PointString(const X, Y: double; DecimalCount: integer): string;
begin
  result := DoubleToStr(x, DecimalCount) + '_' + DoubleToStr(y, DecimalCount)
end;

function PointFromString(const s: string; var X, Y: double): Boolean;
var
  posi: integer;
  sx, sy: string;
begin
  X := _INVALID_COOR.x;
  Y := _INVALID_COOR.y;
  posi := pos('_', s);
  if posi > 0 then
  begin
    sX := copy(s, 1, posi - 1);
    sY := copy(s, posi + 1, length(s) - posi);
    X := Lider.CG.Com.Math.AsFloat(Sx);
    Y := Lider.CG.Com.Math.AsFloat(Sy);
  end;

  result := (not CompareMem(@_INVALID_COOR.x, @X, SizeOf(Double))) and (not
    CompareMem(@_INVALID_COOR.y, @Y, SizeOf(Double)))
end;

function EqualPoint2D(const P1, P2: TlicgCoor; DecimalCount: integer = -12): Boolean;
begin
  if DecimalCount <> 0 then
    Result := _IsEqual(P1, P2, 1 / (Power(10, Abs(DecimalCount))))
  else
  begin
    Result := CompareMem(@P1, @P2, Sizeof(TlicgCoor));
  end;
end;

function PointInPolygon2D_Vector(const Vector: IlicgVector; const P: TlicgCoor;
  var Dist: Double; const Aperture: Double; const T: TlicgMatrix): Integer;
var
  I, J, Idx1, Idx2, n, np: Integer;
  p1_i, p1_j: TlicgCoor;
  IsInside: Boolean;
  IsInsedeCount: Integer;
begin
  Result := PICKED_NONE;

  (* ilker ekleme ba˛lama *)
  if PointOnPolyLine2D_Vector(Vector, 0, P, dist, Aperture, T, True) = PICKED_SEGMENT then
  begin
    Dist := Aperture * 2;
    Exit;
  end;
  (* ilker ekleme biti˛ *)

  Dist := Aperture * 2;
  np := Vector.PartCount;
  n := 0;
  IsInsedeCount := 0;
  if np < 2 then
  begin
    Idx1 := 0;
    Idx2 := Vector.Count - 1;
  end
  else
  begin
    Idx1 := Vector.Parts[n];
    Idx2 := Vector.Parts[n + 1] - 1;
  end;
  repeat
    IsInside := false;
    J := Idx2;
    for I := Idx1 to Idx2 do
    begin
      p1_i := TransformPoint2D(Vector[I], T);
      p1_j := TransformPoint2D(Vector[J], T);
      if ((((p1_i.y <= p.y) and (p.y < p1_j.y)) or
        ((p1_j.y <= p.y) and (p.y < p1_i.y))) and
        (p.x < (p1_j.x - p1_i.x) * (p.y - p1_i.y) / (p1_j.y - p1_i.y) + p1_i.x)) then
        IsInside := not IsInside;
      J := I;
    end;

    if IsInside then // ilker ekleme
      Inc(IsInsedeCount);

    // is this part inside ?
    if (np < 2) and IsInside then Break; // ilker dei˛tirme if (np < 2) or IsInside then Break;

    Inc(n);
    if n >= np then Break;

    Idx1 := Vector.Parts[n];
    if n < np - 1 then
      Idx2 := Vector.Parts[n + 1] - 1
    else
      Idx2 := Vector.Count - 1;
  until false;
  (* ilker ekleme ba˛lama *)
  if (IsInsedeCount  mod 2) = 0 then
    IsInside := False
  else
    IsInside := True;
  (* ilker ekleme biti˛ *)

  if IsInside then
  begin
    Dist := Aperture;
    Result := PICKED_INTERIOR;
  end;
end;

{ TlicgPointList }

constructor TlicgPointList.Create(Size: Integer);
begin
  inherited Create(Size);
  FCount := 0;
  FLast := Size;
  SetLength(FPoints, Size);
  FCanGrow := True;
end;

destructor TlicgPointList.Destroy;
begin
  Finalize(FPoints {, FLast});
  inherited Destroy;
end;

function TlicgPointList.GetCanGrow: Boolean;
begin
  Result := FCanGrow
end;

function TlicgPointList.GetCount: Integer;
begin
  Result := FCount
end;

function TlicgPointList.GetLast: Integer;
begin
  Result := FLast
end;

procedure TlicgPointList.SetCanGrow(const Value: Boolean);
begin
  FCanGrow := Value
end;

procedure TlicgPointList.SetCapacity(Value: Integer);
begin
  if FLast >= Value then
    Exit;
  FLast := Value;
  SetLength(FPoints, FLast);
end;

function TlicgPointList.Get(Index: Integer): TPoint;
begin
  if Index < FCount then
    Result := FPoints[Index]
  else
    LicadGISError(SVectorOutOfBound);
end;

procedure TlicgPointList.Put(Index: Integer; const Item: TPoint);
begin
  if Index < FLast then
  begin
    FPoints[Index] := Item;
    if Index >= FCount then
      FCount := Index + 1;
  end
  else if FCanGrow then
  begin
    { Resize the vector }
    SetLength(FPoints, Index + 1);
    FPoints[Index] := Item;
    FCount := Index + 1;
    FLast := FCount;
  end
  else
    LicadGISError(SVectorOutOfBound);
end;

procedure TlicgPointList.Add(const Item: TPoint);
begin
  Put(FCount, Item);
end;

procedure TlicgPointList.AddPoint(AX, AY: Integer);
begin
  Put(FCount, Point(AX, AY));
end;

function TlicgPointList.GetX(Index: Integer): Integer;
begin
  Result := 0;
  if Index < FCount then
    Result := FPoints[Index].X
  else
    LicadGISError(SVectorOutOfBound);
end;

function TlicgPointList.IsEqual(Index1, Index2: Integer): Boolean;
begin
  Result := CompareMem(@FPoints[Index1], @FPoints[Index2], Sizeof(TPoint));
end;

function TlicgPointList.GetY(Index: Integer): Integer;
begin
  Result := 0;
  if Index < FCount then
    Result := FPoints[Index].Y
  else
    LicadGISError(SVectorOutOfBound);
end;

procedure TlicgPointList.PutX(Index: Integer; Value: Integer);
var
  Item: TPoint;
begin
  Item := Get(Index);
  Item.X := Value;
  Put(Index, Item);
end;

procedure TlicgPointList.PutY(Index: Integer; Value: Integer);
var
  Item: TPoint;
begin
  Item := Get(Index);
  Item.Y := Value;
  Put(Index, Item);
end;

procedure TlicgPointList.Clear;
begin
  FCount := 0;
end;

procedure TlicgPointList.Delete(Index: Integer);
var
  cnt: Integer;
begin
  if Index >= FCount then
    LicadGISError(SVectorOutOfBound);
  if FCount >= 2 then
    for cnt := Index to FCount - 2 do
      FPoints[cnt] := FPoints[cnt + 1];
  Dec(FCount);
end;

procedure TlicgPointList.Insert(Index: Integer; const Item: TPoint);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SVectorOutOfBound);
  { Now, make the space for the new item }
  Add(Item);
  { Insert the item }
  for I := FCount - 1 downto Index + 1 do
    FPoints[I] := FPoints[I - 1];
  FPoints[Index] := Item;
end;

procedure QuickSortDistList(SortList: TList; L, R: Integer);
var
  I, J: Integer;
  P, T: TDistRec;
begin
  if SortList.Count < 2 then
    exit;
  repeat
    I := L;
    J := R;
    P := PDistRec(SortList[(L + R) shr 1])^;
    repeat
      while PDistRec(SortList[I])^.D < P.D do
        Inc(I);
      while PDistRec(SortList[J])^.D > P.D do
        Dec(J);
      if I <= J then
      begin
        T := PDistRec(SortList[I])^;
        PDistRec(SortList[I])^ := PDistRec(SortList[J])^;
        PDistRec(SortList[J])^ := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortDistList(SortList, L, J);
    L := I;
  until I >= R;
end;

function ScaleMatrixText(const Pt: TlicgCoor): TlicgMatrix;
begin
  Result := Scale2D(True_Type_Text_Cofficient, True_Type_Text_Cofficient, pt);
end;

{ TlicgTransformParamsList }

destructor TlicgTransformParamsList.Destroy;
begin
  Clear;
  Finalize(FList);
  inherited Destroy;
end;

function TlicgTransformParamsList.Get(Index: Integer): TlicgTransformParams;
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Result := FList[Index];
end;

procedure TlicgTransformParamsList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TlicgTransformParamsList.Put(Index: Integer; const Item: TlicgTransformParams);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  FList[Index] := Item;
end;

procedure TlicgTransformParamsList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    LicadGISError(SWordListError);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList[FCount], (NewCount - FCount) * SizeOf(TlicgTransformParams), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure TlicgTransformParamsList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    LicadGISError(SWordListError);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

function TlicgTransformParamsList.Add(const Item: TlicgTransformParams): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

procedure TlicgTransformParamsList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TlicgTransformParamsList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TlicgTransformParams));
end;

function TlicgTransformParamsList.GetCapacity: Integer;
begin
  Result := FCapacity
end;

function TlicgTransformParamsList.GetCount: Integer;
begin
  Result := FCount
end;

{ TlicgGradient }

constructor TlicgGradient.Create;
begin
  inherited Create;
  FDirection := fdTopBottom;
  FStartColor := clWhite;
  FEndColor := clYellow;
  FMidColor := clNone;
end;

procedure TlicgGradient.Assign(Source: TlicgBaseGradient);
begin
  with Source do
  begin
    Self.FDirection := FDirection;
    Self.FEndColor := FEndColor;
    Self.FMidColor := FMidColor;
    Self.FStartColor := FStartColor;
    Self.FVisible := FVisible;
  end;
end;

procedure TlicgGradient.Draw(Canvas: TCanvas; const Rect: TRect);
var
  R: TRect;

  procedure DoVert(C0, C1, C2, C3: TColor);
  begin
    R.Bottom := (Rect.Bottom + Rect.Top) div 2;
    GradientFill(Canvas, R, C0, C1, Direction);
    R.Top := R.Bottom;
    R.Bottom := Rect.Bottom;
    GradientFill(Canvas, R, C2, C3, Direction);
  end;

  procedure DoHoriz(C0, C1, C2, C3: TColor);
  begin
    R.Right := (Rect.Left + Rect.Right) div 2;
    GradientFill(Canvas, R, C0, C1, Direction);
    R.Left := R.Right;
    R.Right := Rect.Right;
    GradientFill(Canvas, R, C2, C3, Direction);
  end;

begin
  if MidColor = clNone then
    GradientFill(Canvas, Rect, StartColor, EndColor, Direction)
  else
  begin
    R := Rect;
    case Direction of
      fdTopBottom:
        DoVert(MidColor, EndColor, StartColor, MidColor);
      fdBottomTop:
        DoVert(StartColor, MidColor, MidColor, EndColor);
      fdLeftRight:
        DoHoriz(MidColor, EndColor, StartColor, MidColor);
      fdRightLeft:
        DoHoriz(StartColor, MidColor, MidColor, EndColor);
    else
      GradientFill(Canvas, Rect, StartColor, EndColor, Direction)
    end;
  end;
end;

procedure RectSize(const R: TRect; var RectWidth, RectHeight: Integer);
begin
  with R do
  begin
    RectWidth := Right - Left;
    RectHeight := Bottom - Top;
  end;
end;

procedure SwapInteger(var a, b: Integer);
var
  tmp: Integer;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

procedure TlicgGradient.GradientFill(Canvas: TCanvas; const Rect: TRect;
  StartColor: TColor; EndColor: TColor; Direction: TlicgFillDirection);
var
  T0, T1, T2: Integer;
  D0, D1, D2: Integer;
  tmpBrush: HBRUSH;
  FDC: THandle;
  OldColor: TColor;

  procedure CalcBrushColor(Index, Range: Integer);
  var
    tmp: TColor;
  begin
    tmp := RGB(T0 + MulDiv(Index, D0, Range), T1 + MulDiv(Index, D1, Range), T2
      + MulDiv(Index, D2, Range));
    if tmp <> OldColor then
    begin
      if tmpBrush <> 0 then
        DeleteObject(SelectObject(FDC, tmpBrush));
      tmpBrush := SelectObject(FDC, CreateSolidBrush(tmp));
      OldColor := tmp;
    end;
  end;

var
  tmpRect: TRect;

  {procedure RectGradient(Horizontal: Boolean);
  var
    t, P1, P2, P3: Integer;
    Size: Integer;
    Steps: Integer;
  begin
    FDC := Canvas.Handle;
    with tmpRect do
    begin
      if Horizontal then
      begin
        P3 := Bottom - Top;
        Size := Right - Left;
      end
      else
      begin
        P3 := Right - Left;
        Size := Bottom - Top;
      end;
      Steps := Size;
      if Steps > 256 then
        Steps := 256;
      P1 := 0;
      OldColor := -1;
      for t := 0 to Steps - 1 do
      begin
        CalcBrushColor(t, Pred(Steps));
        P2 := MulDiv(t + 1, Size, Steps);
        if Horizontal then
          PatBlt(FDC, Right - P1, Top, P1 - P2, P3, PATCOPY)
        else
          PatBlt(FDC, Left, Bottom - P1, P3, P1 - P2, PATCOPY);
        P1 := P2;
      end;
    end;
  end;}

  procedure RectGradient(Horizontal: Boolean);
  var
    t, P1, P2, P3: Integer;
    Size: Integer;
    Steps: Integer;
  begin
    FDC := Canvas.Handle;
    //with tmpRect do //li2016 dei˛tirme
    begin
      if Horizontal then
      begin
        P3 := tmpRect.Bottom - tmpRect.Top;
        Size := tmpRect.Right - tmpRect.Left;
      end
      else
      begin
        P3 := tmpRect.Right - tmpRect.Left;
        Size := tmpRect.Bottom - tmpRect.Top;
      end;
      Steps := Size;
      if Steps > 256 then
        Steps := 256;
      P1 := 0;
      OldColor := -1;
      for t := 0 to Steps - 1 do
      begin
        CalcBrushColor(t, Pred(Steps));
        P2 := MulDiv(t + 1, Size, Steps);
        if Horizontal then
          PatBlt(FDC, tmpRect.Right - P1, tmpRect.Top, P1 - P2, P3, PATCOPY)
        else
          PatBlt(FDC, tmpRect.Left, tmpRect.Bottom - P1, P3, P1 - P2, PATCOPY);
        P1 := P2;
      end;
    end;
  end;

  procedure FromCenter;
  const
    GradientPrecision: Integer = 1;
  var
    tmpXCenter, tmpYCenter, SizeX, SizeY, P0, P1, tmpLeft, tmpTop: Integer;
    tmp1, tmp2, tmp3: Integer;
  begin
    RectSize(tmpRect, SizeX, SizeY);
    tmpXCenter := SizeX shr 1;
    tmpYCenter := SizeY shr 1;
    tmp1 := 0;
    tmp2 := 0;
    tmp3 := tmpXCenter + tmpYCenter;
    FDC := Canvas.Handle;
    repeat
      CalcBrushColor(tmp1 + tmp2, tmp3);
      P0 := SizeY - (2 * tmp1);
      P1 := SizeX - (2 * tmp2);
      tmpLeft := tmpRect.Left + tmp2;
      tmpTop := tmpRect.Top + tmp1;
      PatBlt(FDC, tmpLeft, tmpTop, GradientPrecision, P0, PATCOPY);
      PatBlt(FDC, tmpRect.Right - tmp2 - 1, tmpTop, GradientPrecision, P0, PATCOPY);
      PatBlt(FDC, tmpLeft, tmpTop, P1, GradientPrecision, PATCOPY);
      PatBlt(FDC, tmpLeft, tmpRect.Bottom - tmp1 - GradientPrecision, P1,
        GradientPrecision, PATCOPY);
      if tmp1 < tmpYCenter then
        Inc(tmp1, GradientPrecision);
      if tmp2 < tmpXCenter then
        Inc(tmp2, GradientPrecision);
    until (tmp1 >= tmpYCenter) and (tmp2 >= tmpXCenter);
  end;

begin
  if (Direction = fdTopBottom) or (Direction = fdLeftRight) then

  else
    SwapInteger(Integer(StartColor), Integer(EndColor));

  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);

  T0 := GetRValue(StartColor);
  T1 := GetGValue(StartColor);
  T2 := GetBValue(StartColor);
  D0 := GetRValue(EndColor) - T0;
  D1 := GetGValue(EndColor) - T1;
  D2 := GetBValue(EndColor) - T2;

  tmpRect := Rect;
  with tmpRect do
  begin
    if Right < Left then
      SwapInteger(Left, Right);
    if Bottom < Top then
      SwapInteger(Top, Bottom);
  end;

  tmpBrush := 0;
  OldColor := -1;

  case Direction of
    fdLeftRight, fdRightLeft:
      RectGradient(True);
    fdTopBottom, fdBottomTop:
      RectGradient(False);
    fdFromCenter:
      FromCenter;
  end;
  if tmpBrush <> 0 then
    DeleteObject(SelectObject(FDC, tmpBrush));
end;

function TlicgGradient.GetDirection: TlicgFillDirection;
begin
  Result := FDirection
end;

function TlicgGradient.GetEndColor: TColor;
begin
  Result := FEndColor
end;

function TlicgGradient.GetMidColor: TColor;
begin
  Result := FMidColor
end;

function TlicgGradient.GetStartColor: TColor;
begin
  Result := FStartColor
end;

function TlicgGradient.GetVisible: Boolean;
begin
  Result := FVisible
end;

procedure TlicgGradient.SetDirection(const Value: TlicgFillDirection);
begin
  FDirection := Value
end;

procedure TlicgGradient.SetEndColor(const Value: TColor);
begin
  FEndColor := Value
end;

procedure TlicgGradient.SetMidColor(const Value: TColor);
begin
  FMidColor := Value
end;

procedure TlicgGradient.SetStartColor(const Value: TColor);
begin
  FStartColor := Value
end;

procedure TlicgGradient.SetVisible(const Value: Boolean);
begin
  FVisible := Value
end;

{ TlicgNodeAssocList }

destructor TlicgNodeAssocList.Destroy;
begin
  Clear;
  Finalize(FList);
  inherited;
end;

function TlicgNodeAssocList.Add(const Item: TlicgNodeAssocRec): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

function TlicgNodeAssocList.AddNode(ID, Recno, VertexNo: Integer; ConnectType:
  TlicgConnectType): Integer;
var
  Item: TlicgNodeAssocRec;
begin
  Item.ID := ID;
  Item.Recno := Recno;
  Item.VertexNo := VertexNo;
  Result := Add(Item);

  FConnectType := ConnectType;
end;

procedure TlicgNodeAssocList.AddLink(FromID, FromRecno, FromVertexNo, ToID,
  ToRecno, ToVertexNo: Integer; Restriction: TlicgTravelRestriction);
var
  Item: TlicgNodeAssocRec;
begin
  Clear;
  SetCapacity(2);

  Item.ID := FromID;
  Item.Recno := FromRecno;
  Item.VertexNo := FromVertexNo;
  Item.Restriction := Restriction;
  Add(Item);

  Item.ID := ToID;
  Item.Recno := ToRecno;
  Item.VertexNo := ToVertexNo;
  Item.Restriction := Restriction;
  Add(Item);

  FConnectType := ctLink;

end;

function TlicgNodeAssocList.IsLink: Boolean;
begin
  Result := (FCount = 2) and (FConnectType = ctLink);
end;

procedure TlicgNodeAssocList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TlicgNodeAssocList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TlicgNodeAssocRec));
end;

function TlicgNodeAssocList.Get(Index: Integer): TlicgNodeAssocRec;
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Result := FList[Index];
end;

procedure TlicgNodeAssocList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TlicgNodeAssocList.Put(Index: Integer; const Item: TlicgNodeAssocRec);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  FList[Index] := Item;
end;

procedure TlicgNodeAssocList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    LicadGISError(SWordListError);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TlicgNodeAssocList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    LicadGISError(SWordListError);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList[FCount], (NewCount - FCount) * SizeOf(TlicgNodeAssocRec), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure TlicgNodeAssocList.LoadFromStream(Stream: TStream);
var
  I, N: Integer;
  Value: TlicgNodeAssocRec;
begin
  Clear;
  with Stream do
  begin
    Read(FConnectType, SizeOf(FConnectType));
    Read(N, Sizeof(N));
    for I := 1 to N do
    begin
      Read(Value, Sizeof(Value));
      Add(Value);
    end;
  end;
end;

procedure TlicgNodeAssocList.SaveToStream(Stream: TStream);
var
  I, N: Integer;
  Value: TlicgNodeAssocRec;
begin
  N := FCount;
  with Stream do
  begin
    Write(FConnectType, SizeOf(FConnectType));
    Write(N, Sizeof(N));
    for I := 0 to FCount - 1 do
    begin
      Value := FList[I];
      Write(Value, Sizeof(Value));
    end;
  end;
end;

function TlicgNodeAssocList.GetCapacity: Integer;
begin
  Result := FCapacity
end;

function TlicgNodeAssocList.GetConnectType: TlicgConnectType;
begin
  Result := FConnectType
end;

function TlicgNodeAssocList.GetCount: Integer;
begin
  Result := FCount
end;

procedure TlicgNodeAssocList.SetConnectType(const Value: TlicgConnectType);
begin
  FConnectType := Value
end;

function PolygonExtentIntersectClip(APolygon: IlicgEntity; AExtent: TlicgExtent): IlicgEntity;
var
  ANewExtentPoly: IlicgEntity;
begin
  Result := nil;
  ANewExtentPoly := Licad.CreateEntityFactory.MakePolygon([]);
  ANewExtentPoly.Geometry.Points.Add(AExtent.LowerLeft.X, AExtent.LowerLeft.Y);
  ANewExtentPoly.Geometry.Points.Add(AExtent.LowerLeft.X, AExtent.UpperRight.Y);
  ANewExtentPoly.Geometry.Points.Add(AExtent.UpperRight.X, AExtent.UpperRight.Y);
  ANewExtentPoly.Geometry.Points.Add(AExtent.UpperRight.X, AExtent.LowerLeft.Y);
  ANewExtentPoly.Geometry.Points.Add(AExtent.LowerLeft.X, AExtent.LowerLeft.Y);
  try
    Result := APolygon.Intersection(ANewExtentPoly);
  finally
    ANewExtentPoly := nil;
  end;
end;

function CenterExtentIntersectClip(APolygon: IlicgEntity; AExtent: TlicgExtent): TlicgCoor;
var
  ANewExtentPoly, AResultPoly: IlicgEntity;
begin
  Result := _INVALID_COOR;
  ANewExtentPoly := Licad.CreateEntityFactory.MakePolygon([]);
  ANewExtentPoly.Geometry.Points.Add(AExtent.LowerLeft.X, AExtent.LowerLeft.Y);
  ANewExtentPoly.Geometry.Points.Add(AExtent.LowerLeft.X, AExtent.UpperRight.Y);
  ANewExtentPoly.Geometry.Points.Add(AExtent.UpperRight.X, AExtent.UpperRight.Y);
  ANewExtentPoly.Geometry.Points.Add(AExtent.UpperRight.X, AExtent.LowerLeft.Y);
  ANewExtentPoly.Geometry.Points.Add(AExtent.LowerLeft.X, AExtent.LowerLeft.Y);
  try
    AResultPoly := APolygon.Intersection(ANewExtentPoly);
    Result := AResultPoly.Geometry.Centroid;
  finally
    ANewExtentPoly := nil;
    AResultPoly := nil;
  end;
end;

function IsFullInsideEntityInExtent(APolygon: IlicgEntity; AExtent: TlicgExtent): boolean;
begin
  Result := APolygon.Geometry.Points.Count > 0;
  try
    if Result then
    begin
      for var I := 0 to APolygon.Geometry.Points.Count - 1 do
      begin
        if not IsPointInExtent(APolygon.Geometry.Points[I], AExtent) then
          Result := false;
        if not Result then
          Break;
      end;
    end;
  except
  end;
end;

function Geo_DistanceTokm(const lat1, lon1, lat2, lon2: double): double;
const
  R = 6371; // km
var
  a, c, dLat, dLon, _lat1, _lat2: double;
begin

  dLat := DegToRad(lat2 - lat1);
  dLon := DegToRad(lon2 - lon1);
  _lat1 := DegToRad(lat1);
  _lat2 := DegToRad(lat2);

  a := Sin(dLat / 2) * Sin(dLat / 2) + Sin(dLon / 2) * Sin(dLon / 2) * Cos(lat1)
    * Cos(lat2);
  c := 2 * arctan2(sqrt(a), sqrt(1 - a));

  Result := R * c;

end;

function Geo_DistanceToM(const lat1, lon1, lat2, lon2: double): double;
begin
  Result := Geo_DistanceTokm(lat1, lon1, lat2, lon2) * 1000;
end;

function GeoLengthToM(const Points: IlicgVector): double;
var
  start_Index, end_Index, i, j: integer;
begin
  Result := 0.0;

   { partlar ayr˝ ayr˝ d¸˛¸n¸lmeli. }
  for i := 0 to Points.PartCount - 1 do
  begin
    Points.GetIndexes(i, start_Index, end_Index);
    for j := start_Index to end_Index - 1 do
    begin
      result := result + Geo_DistanceToM(Points[j].Y, Points[j].X, Points[j + 1].Y,
        Points[j + 1].X);
    end;
  end;
end;

function MeterToDeg_H(P: TlicgCoor; M: Double): double;
var
  V: IlicgVector;
begin
  V := Licad.CreateEntityFactory.MakeVector(_2D, 2);
  try
    V.Add(P.X, P.Y);
    V.Add(P.X + 1, P.Y);
    Result := M * V.Length / GeoLengthToM(V);

  except
  end;
  V := nil;
end;

function MeterToDeg_V(P: TlicgCoor; M: Double): double;
var
  V: IlicgVector;
begin
  V := Licad.CreateEntityFactory.MakeVector(_2D, 2);
  try
    V.Add(P.X, P.Y);
    V.Add(P.X, P.Y + 1);
    Result := M * V.Length / GeoLengthToM(V);
  except
  end;
  V := nil;
end;

(* ilker ekleme ba˛lama *)
function InPoly(const AEntity: IlicgEntity; const ACoor: TlicgCoor): Boolean;
var
  Dist: Double;
begin
  Result := PointInPolygon2D_Vector(AEntity.Geometry.DrawPoints, ACoor, Dist,
    0, AEntity.Geometry.GetTransformMatrix) >= PICKED_INTERIOR;
end;

function OnPoly(const AEntity: IlicgEntity; const ACoor: TlicgCoor): Boolean;
//var
  //Dist: Double;
begin
  Result := OnPoly(AEntity.Geometry.DrawPoints, ACoor);
  (* ilker eskisi silme
  if AEntity.IsPolyline then
    Result := PointOnPolyLine2D_Vector(AEntity.Geometry.DrawPoints, 0, ACoor, Dist,
      _EPSILON, AEntity.Geometry.GetTransformMatrix, False) = PICKED_SEGMENT
  else
    Result := PointOnPolyLine2D_Vector(AEntity.Geometry.DrawPoints, 0, ACoor, Dist,
      _EPSILON, AEntity.Geometry.GetTransformMatrix, True) = PICKED_SEGMENT;*)
end;

function InPoly(const AGeometry: IlicgGeometry; const ACoor: TlicgCoor): Boolean;
var
  Dist: Double;
begin
  Result := PointInPolygon2D_Vector(AGeometry.DrawPoints, ACoor, Dist,
    0, AGeometry.GetTransformMatrix) >= PICKED_INTERIOR;
end;

function OnPoly(const AGeometry: IlicgGeometry; const ACoor: TlicgCoor): Boolean;
//var
  //Dist: Double;
begin
  Result := OnPoly(AGeometry.DrawPoints, ACoor);
  (* ilker eskisi silme
  if AGeometry.GeometryGroup in [_Line, _Polyline, _Spline, _Arc] then
    Result := PointOnPolyLine2D_Vector(AGeometry.DrawPoints, 0, ACoor, Dist,
      _EPSILON, AGeometry.GetTransformMatrix, False) = PICKED_SEGMENT
  else
    Result := PointOnPolyLine2D_Vector(AGeometry.DrawPoints, 0, ACoor, Dist,
      _EPSILON, AGeometry.GetTransformMatrix, True) = PICKED_SEGMENT;  *)
end;

function InPoly(const AVector: IlicgVector; const ACoor: TlicgCoor): Boolean;
var
  Dist: Double;
begin
  Result := PointInPolygon2D_Vector(AVector, ACoor, Dist, 0, IDENTITY_MATRIX2D) >= PICKED_INTERIOR;
end;

function OnPoly(const AVector: IlicgVector; const ACoor: TlicgCoor): Boolean;
var
  //Dist: Double; ilker silme eskisi
  X: Integer;
  C: TlicgCoor;
begin
  Result := False;
  for X := 0 to AVector.Count - 2 do
  begin
    C.X := ACoor.X;
    C.Y := ACoor.Y;
    if IsPointOnLine(c, AVector.Coors2D[X], AVector.Coors2D[X + 1]) then
    begin
      Result := True;
      Break;
    end;
  end;
  // ilker silme eskisi Result := PointOnPolyLine2D_Vector(AVector, 0, ACoor, Dist, _EPSILON, IDENTITY_MATRIX2D, AVector.IsClosed) = PICKED_SEGMENT;
end;

function IsInsidePolygon(const AVector: IlicgVector; const BVector: IlicgVector; const AFullInside: Boolean): Boolean;

  function OnPoly(const AVector: IlicgVector; const ACoor: TlicgCoor): Boolean;
  var
    ASegmentEpsilon: Double;
    X: Integer;
    C: TlicgCoor;
  begin
    Result := False;
    ASegmentEpsilon := 0.001;
    for X := 0 to AVector.Count - 2 do
    begin
      C.X := ACoor.X;
      C.Y := ACoor.Y;
      if IsPointOnLine(c, AVector.Coors2D[X], AVector.Coors2D[X + 1], ASegmentEpsilon) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

var
  ACount: Integer;
begin
  ACount := 0;
  Result := False;
  for var I := 0 to BVector.Count - 2 do // B - A nin iÁindeki
  begin
    if OnPoly(AVector, BVector.Coors2D[I]) or
       InPoly(AVector, BVector.Coors2D[I]) then
    begin
      ACount := ACount + 1;
    end
    else
    begin
      if ACount > 0 then
        Break;
    end;
  end;
  if ACount = 0 then // Full D˝˛˝ndaysa
  begin
    //ShowMessage('D˝˛˝nda')
    Result := False;
  end
  else if ACount = (BVector.Count - 1) then // Full iÁindeyse
  begin
    //ShowMessage('Full iÁindeyse ');
    Result := True;
  end
  else // Bir k˝sm˝ iÁindeyse
  begin
    //ShowMessage('Bir k˝sm˝ iÁindeyse');
    if AFullInside then
      Result := False
    else
      Result := True;
  end;
end;

end.


