unit Lider.CG.Com.GIS;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
  Printers,
  WinSpool,
  Messages,
  Dialogs,
  Variants,
  IniFiles,
  IOUtils,
  DB,
  ADODB,
  System.Generics.Collections,
  Lider.CG.Lib.Referens,
  Lider.CG.Com.TransformInt,
  Lider.CG.Com.StringLi,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.Rtree,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Base,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Types,
  Lider.CG.Com.CSInt,
  Lider.CG.Com.Lists,
  Lider.CG.Com.ProjectInt,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.TextureFill;

const
  {$J+}CURRENT_ENTITY_VERSION: Byte = 37; {$J-} // 02.09.2025 37 oldu.

  THREAD_MESSAGE = WM_USER + 100; // Thread message
  THREAD_FINISHED = 1;
  THREAD_REFRESH = 2;

  // Licad Dosya uzantıları
  EXT_LCG        = '.LCG';
  EXT_BACKUP     = '.$LCG';

  EXT_MAP        = '.LMP'; // Lider Map Project (*.LMP)
  EXT_THEMATICS  = '.LTM';
  EXT_BLOCK      = '.BLOCK';
  EXT_LABEL      = '.LABEL';

  // Refeans
  EXT_TRANSFORM  = '.LTRM';
  EXT_REFERANS   = '.LRF';

  // Desktop layer Dosya Uzantıları
  EXT_LAYERDB    = '.DBF';
  EXT_LAYERDRAW  = '.LDW';
  EXT_LAYERINDEX = '.LIX';
  EXT_LAYERRTREE = '.LRX';

  // Memory layer Dosya Uzantıları
  EXT_LAYERCAD     = '.LCD';

  EXT_LONGFIELD  = '.LFD';
  EXT_FIELD      = '.LFLDS';

  EXT_SYMBOL     = '.LSM';
  EXT_LINE       = '.LLN';
  EXT_FONT       = '.FNT';

  EXT_LITEXTEDIT = '.LRF'; // Licad Text Editor File
  EXT_LCN        = '.LCN'; // Licad Nokta

  // Diğer Dosya Uzantıları
  EXT_NCZ        = '.NCZ'; // NC
  EXT_CKS        = '.CKS'; // NC text editor
  EXT_NCN        = '.NCN'; // NC Nokta

  EXT_DWG        = '.DWG';
  EXT_DGN        = '.DGN';
  EXT_DXF        = '.DXF';

  EXT_GEOJSON    = '.GEOJSON';
  EXT_GML        = '.GML';
  EXT_GPX        = '.GPX';
  EXT_JSON       = '.JSON';
  EXT_KML        = '.KML';
  EXT_KMZ        = '.KMZ';
  EXT_MIF        = '.MIF';
  EXT_SHP        = '.SHP';
  EXT_TAB        = '.TAB';
  EXT_LANDXML    = '.XML';

  EXT_METIN      = '.TXT';
  EXT_CSV        = '.CSV';

  // Nokta Dosyaları
  EXT_NIR        = '.NIR';
  EXT_NRG        = '.NRG';
  EXT_POL        = '.POL';
  EXT_XYZ        = '.XYZ';

  EXT_LAS        = '.LAS'; // Nokta Bulutu
  EXT_LAZ        = '.LAZ'; // Nokta Bulutu

  //Geodatabase Dosyaları
  EXT_GDB        = '.GDB';
  EXT_GDBTABLE   = '.GDBTABLE';


  LAYER_VERSION_NUMBER = 201;
  MAP_ID = 8206;
  MAP_VERSION_NUMBER = 200;
  RandomColor: array[0..14] of TColor = (clMaroon, clGreen, clOlive, clNavy,
    clPurple, clTeal, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua,
    clMoneyGreen, clSkyBlue, clCream);

type
  { forwards }
  TlicgBaseLayer = class;

  TlicgBaseLayers = class;

  TlicgBaseGIS = class;

  TlicgBaseDrawBox = class;

  TlicgDrawBoxList = class;

  TlicgBaseSelection = class;

  TlicgBaseTable = class;

  TlicgGISItem = class;

  TlicgEntityList = class;

  IlicgPainterObject = interface;

  IlicgDesktopLayer = interface;

  TLicad_DetectCancelPaintProc = function(): boolean; stdcall;

  TLicad_RegenDrawingProc = procedure(AGIS: TObject; DrawParam: integer); stdcall;

  EGISException = class(Exception)
  end;

  IlicgReferenceGIS = interface
    ['{651E1C8D-2B15-4D8C-BC77-E021DBFFDC00}']
    function FindLayer(path: wideString): TObject; stdcall;
    function ItemCount: integer; stdcall;
    function ItemNativeExt(index: integer): TReferenceGISExtent; stdcall;
    function ItemProjectedExt(index: integer): TReferenceGISExtent; stdcall;
    function ItemPath(index: integer): wideString; stdcall;
    function isItemRaster(index: integer): boolean; stdcall;
    function ItemTransformGCP(index: integer): IlicgTransformGCP; stdcall;
    procedure Open(fn: WideString); stdcall;
    procedure Save(fn: WideString); stdcall;
    procedure Close; stdcall;
    procedure LayerReOpen(path: widestring); stdcall;
    procedure LayerTransformClear(path: widestring); stdcall;
    function Extent: TReferenceGISExtent; stdcall;
    procedure FullExtent; stdcall;
    procedure LayerReCalcExtent(path: widestring); stdcall;
    procedure LayerReCalcProjecrtedExtent(path: widestring); stdcall;

    (*
      {@@ All supported files. }
      0 =  gisFileTypeAll,
      {@@ Project files (.TTKGP etc.) }
      1 =  gisFileTypeProject,
      {@@ Vector files (.SHP etc.) }
      2 =  gisFileTypeVector,
      {@@ Pixel files (.BMP etc.) }
      3 =  gisFileTypePixel,
      {@@ Grid files (.GRD etc.) }
      4 =  gisFileTypeGrid
    *)
    function LayerExchange(GroupIndex, LayerIndex1, LayerIndex2: integer):
      boolean; stdcall;
    function AddDRE(var ARasterFileName: widestring; const Group: wideString; const Dre_Ext:
      TReferenceGISExtent; A, B, C, D, E, F: double; var Native_Ext, Projected_Ext:
      TReferenceGISExtent): Boolean; stdcall;

    function AddLayer(const fn, Group: WideString; var Native_Ext, Projected_Ext:
      TReferenceGISExtent): boolean; stdcall;
    function RemoveLayer(const fn: WideString): boolean; stdcall;
    procedure Draw(DBox: pointer; CanUseCache: boolean; const W, H: integer;
      RealRect: TRect; PrintPreview: boolean; const Xmin, Ymin, XMax, YMax:
      double; const MidPntX, MidPntY: double; const HDC: THandle; const
      Backcolor: integer; const Scale: double; var CancelDraw: boolean;
      ShowOnlineMap: Boolean; OnlineMapPreference: TOnlineMapPreference =
      omrBingHybrid); stdcall;
    procedure AddDrawBox(DBox: Pointer); stdcall;
    procedure DeleteDrawBox(DBox: Pointer); stdcall;
    procedure ClearCaches; stdcall;
    function LayerActive(GroupIndex, LayerIndex: integer): boolean; stdcall;
    procedure SetLayerActive(path: WideString; Value: boolean); stdcall;
    function LayerFullPath(GroupIndex, LayerIndex: integer): WideString; stdcall;
    function GetLayerZOrder(path: WideString): integer; stdcall;
    procedure SetLayerZOrder(path: WideString; Value: integer); stdcall;
    function GetLayerNativeExtent(path: WideString): TReferenceGISExtent; stdcall;
    function GetLayerProjectedExtent(path: WideString): TReferenceGISExtent; stdcall;

    // function ShowLayerProperties (GroupIndex, LayerIndex : integer) : integer; stdcall;
    function ShowLayerProperties(path: WideString): integer; stdcall;

    // projection

    function GetRasterTransformInfo(path: widestring; var msg, TempPath:
      widestring; var PixelW, PixelH: integer): IlicgTransformGCP; stdcall;
    function LayerExport(path: widestring; var msg: widestring): boolean; stdcall;
    function GetNewTransform: IlicgTransformGCP; stdcall;
    function GetTransform(path: widestring): IlicgTransformGCP; stdcall;
    function GetDrawBoxCanvasHDC: THandle; stdcall;
    procedure Update; stdcall;

    //function GetlicgPainterObject: TObject; stdcall;

    // ilker Yeniler 03/07/2022
    procedure AddLayerGIS(const AFileName: string); stdcall;
    function ReferenceAllFilesDialogFilter: WideString; stdcall;
    function ReferenceRasterFilesDialogFilter: WideString; stdcall;

    //function GetCS: IlicgCS; stdcall;
    //procedure SetCS(Value: IlicgCS); stdcall;
    procedure Interrupt; stdcall;
    function GetReferenceManager: IXMLReferenceManagerType; stdcall;

    //property CS: IlicgCS read GetCS write SetCS;
    property ReferenceManager: IXMLReferenceManagerType read GetReferenceManager;
    //property licgPainterObject: TObject read GetlicgPainterObject;

  end;

  IlicgTopology = interface
    ['{BD6DA432-AB37-4E50-8222-8C099A64CFFC}']

    /// <summary>
    ///   Cross (kesme) noktalrını çift döndürür. Çünkü bitiş noktası diğerinin
    ///   başlama noktası olduğunda.
    /// </summary>
    //function ClearShape(const AEntity: IlicgEntity): IlicgVector; stdcall;

    function FixEntity(const AEntity: IlicgEntity): IlicgVector; stdcall;
    function FixEntityEx(const AEntity: IlicgEntity; var AWasFixed: Boolean): IlicgVector; overload; stdcall;
    function FixEntityEx(const AEntity: IlicgEntity; const AForceFix: Boolean; var AWasFixed: Boolean): IlicgVector; overload; stdcall;

    function ConvexHull(const AEntity: IlicgEntity; const BEntity: IlicgEntity = nil; const AFix: Boolean = True): IlicgEntity; overload; stdcall;
    function ConvexHull(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry = nil; const AFix: Boolean = True): IlicgGeometry; overload; stdcall;
    function ConcaveHull(const AEntity: IlicgEntity; const BEntity: IlicgEntity = nil; const AAlpha: Double = 0; const AFix: Boolean = True): IlicgEntity; overload; stdcall;
    function ConcaveHull(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry = nil; const AAlpha: Double = 0; const AFix: Boolean = True): IlicgGeometry; overload; stdcall;

    function Intersection(const AEntity: IlicgEntity; const BEntity: IlicgEntity; const AFix: Boolean = True): IlicgEntity; overload; stdcall;
    function Intersection(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry; const AFix: Boolean = True): IlicgGeometry; overload; stdcall;

    function Difference(const AEntity: IlicgEntity; const BEntity: IlicgEntity; const AFix: Boolean = True): IlicgEntity; overload; stdcall;
    function Difference(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry; const AFix: Boolean = True): IlicgGeometry;  overload; stdcall;
    function SymmetricalDifference(const AEntity: IlicgEntity; const BEntity: IlicgEntity; const AFix: Boolean = True): IlicgEntity; overload; stdcall;
    function SymmetricalDifference(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry; const AFix: Boolean = True): IlicgGeometry; overload; stdcall;

    function Union(const AEntity: IlicgEntity; const BEntity: IlicgEntity; const AFix: Boolean = True): IlicgEntity; overload; stdcall;
    function Union(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry; const AFix: Boolean = True): IlicgGeometry; overload; stdcall;
    function UnionOnList(const AEntityList: IlicgEntityList; const AFixEntity: Boolean = True): IlicgEntity; overload; stdcall;
    function UnionOnList(const ASelection: TlicgBaseSelection; const AFixEntity: Boolean = True): IlicgEntity; overload; stdcall;

    function MakeBuffer(const AEntity: IlicgEntity; const ADist: Double; const Api2_Points: Integer = 9; const AFix: Boolean = True): IlicgEntity; overload; stdcall;
    function MakeBuffer(const AGeometry: IlicgGeometry; const ADist: Double; const Api2_Points: Integer = 9; const AFix: Boolean = True): IlicgGeometry; overload; stdcall;
    function MakeOffset(const AEntity: IlicgEntity; const ADist: Double; AJoinType: TlicgJoinType; AEndType: TlicgEndType; AMiterLimit: Double = -1; AArcTolerance: Double = -1): IlicgEntity; overload; stdcall;
    function MakeOffset(const AGeometry: IlicgGeometry; const ADist: Double; AJoinType: TlicgJoinType; AEndType: TlicgEndType; AMiterLimit: Double = -1; AArcTolerance: Double = -1): IlicgGeometry; overload; stdcall;

    // İlişkiler, Bağlantılar Relate
    function Equality(const AEntity: IlicgEntity; const BEntity: IlicgEntity): Boolean; overload; stdcall;
    function Equality(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry): Boolean; overload; stdcall;
    function Disjoint(const AEntity: IlicgEntity; const BEntity: IlicgEntity): Boolean; overload; stdcall;
    function Disjoint(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry): Boolean; overload; stdcall;
    function Intersect(const AEntity: IlicgEntity; const BEntity: IlicgEntity): Boolean; overload; stdcall;
    function Intersect(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry): Boolean; overload; stdcall;
    function Touch(const AEntity: IlicgEntity; const BEntity: IlicgEntity): Boolean; overload; stdcall;
    function Touch(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry): Boolean; overload; stdcall;
    function Cross(const AEntity: IlicgEntity; const BEntity: IlicgEntity): Boolean; overload; stdcall;
    function Cross(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry): Boolean; overload; stdcall;
    function Within(const AEntity: IlicgEntity; const BEntity: IlicgEntity): Boolean; overload; stdcall;
    function Within(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry): Boolean; overload; stdcall;
    function Contains(const AEntity: IlicgEntity; const BEntity: IlicgEntity): Boolean; overload; stdcall;
    function Contains(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry): Boolean; overload; stdcall;
    function Overlap(const AEntity: IlicgEntity; const BEntity: IlicgEntity): Boolean; overload; stdcall;
    function Overlap(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry): Boolean; overload; stdcall;
    function Relate(const AEntity: IlicgEntity; const BEntity: IlicgEntity; const A_de9im: string): Boolean; overload; stdcall;
    function Relate(const AGeometry: IlicgGeometry; const BGeometry: IlicgGeometry; const A_de9im: string): Boolean; overload; stdcall;

    // Island or Hole
    function Smooth(const AEntity: IlicgEntity; AFactor : Integer): IlicgEntity; stdcall;
    function Split(const AEntity: IlicgEntity; const AKeepHoles: Boolean = False; ASplitOnlyHole: Boolean = False; AFix: Boolean = True): IlicgEntityList; stdcall;
    function SplitByLine(const AEntity: IlicgEntity; const BLineEntity: IlicgEntity; const AFix: Boolean = True): IlicgEntityList; stdcall;
    function LineSplitOnCross(const ALineEntity: IlicgEntity; const AEntityList: IlicgEntityList): IlicgEntity; stdcall;
    function GetCrossings(const AEntity: IlicgEntity; const BEntity: IlicgEntity): IlicgVector; stdcall;

    function InPoly(const AEntity: IlicgEntity; const ACoor: TlicgCoor): Boolean; stdcall; //Nokta Nesnenin içinde mi? (Kenarlar Hariç).
    function OnPoly(const AEntity: IlicgEntity; const ACoor: TlicgCoor): Boolean; stdcall; //Nokta Nesene Üstünde mi? (Kenarların üstündemi, Kapalı alanın içi Hariç)
    function IsPointInsidePolygon(const APoint: TlicgCoor; const AGeometry: IlicgGeometry): Boolean; stdcall;

    function GetTolerance: Double; stdcall;
    procedure SetTolerance(const Value: Double); stdcall;
    function GetForceEntityFixing: Boolean; stdcall;
    procedure SetForceEntityFixing(const Value: Boolean); stdcall;

    property Tolerance: Double read GetTolerance write SetTolerance;
    property ForceEntityFixing: Boolean read GetForceEntityFixing write SetForceEntityFixing;
  end;

  TlicgADODataset = class(TCustomADODataset)
  published
    property CommandText;
    property CommandTimeout;
    property CommandType;
    property Command;
    property DataSetField;
    property DataSource;
    property EnableBCD;
    property FieldDefs;
    property IndexName;
    property IndexFieldNames;
    property MasterFields;
    property ParamCheck;
    property Parameters;
    property Prepared;
    property StoreDefs;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
  end;

  IPickedClass = interface
    ['{2D3F23F7-9F52-4A1C-B760-FD3900078EA2}']
    function GetGis: TlicgBaseGis; stdcall;
    procedure SetGis(Value: TlicgBaseGis); stdcall;
    function GetLayerName: string; stdcall;
    procedure SetLayerName(Value: string); stdcall;
    function GetRecNo: Integer; stdcall;
    procedure SetRecNo(Value: Integer); stdcall;
    function GetPickedPnt: Integer; stdcall;
    procedure SetPickedPnt(Value: Integer); stdcall;
    function Pick: IPickedClass; stdcall;
    function Layer: TlicgBaseLayer; stdcall;
    function Entity: IlicgEntity; stdcall;
    property GIS: TlicgBaseGis read GetGis write SetGis;
    property LayerName: string read GetLayerName write SetLayerName;
    property RecNo: Integer read GetRecNo write SetRecNo;
    property PickedPnt: Integer read GetPickedPnt write SetPickedPnt;
  end;

  IPickedList = interface(IInterfaceList)
    ['{1535252A-4129-4BF0-87B7-BAD0B0B884A6}']
    function GetItem(Index: Integer): IPickedClass; stdcall;
    procedure SetItem(Index: Integer; Value: IPickedClass); stdcall;
    function AddPickedItem: IPickedClass; stdcall;
    property PickedItems[Index: Integer]: IPickedClass read GetItem write SetItem;
  end;

  TPickedList = class(TInterfaceList, IPickedList)
    function GetItem(Index: Integer): IPickedClass; stdcall;
    procedure SetItem(Index: Integer; Value: IPickedClass); stdcall;
    function AddPickedItem: IPickedClass; stdcall;
  end;

  TTriangleObjectParams = record
    DrawBox: TlicgBaseDrawBox;
    AllLayers: Boolean;
    SomeLayers: Boolean;
    LayerList: TList; // SomeLayersTrue ise, o zaman anlamli bir degisken.
    SelectionList: Boolean;
  end;

  TlicgUndoAction = (uaUndelete, uaDelete, uaUnTransform);

  IRegionWork = interface
    ['{EA971541-A784-4BBC-BBC0-C32F8652EB27}']
    function GetDescription: string; stdcall;
    procedure SetDescription(Value: string); stdcall;
    function GetDefined: Boolean; stdcall;
    procedure SetDefined(Value: Boolean); stdcall;
    function GetExtent: TlicgExtent; stdcall;
    procedure SetExtent(Value: TlicgExtent); stdcall;
    function GetClipHRgn(const Grapher: TlicgBaseGrapher; Canvas: TCanvas): HRgn; stdcall;
    procedure DeleteClipHRgn(Rgn: HRgn; Canvas: TCanvas); stdcall;
    function InPointRegion(const P: TlicgCoor): Boolean; stdcall;
    function IntersectPolyRealRegion(const PVec: IlicgVector): Boolean; stdcall;
    function GetGeomPoints: IlicgVector; stdcall;
    procedure SetworkScale(Value: double); stdcall;
    function GetworkScale: double; stdcall;
    property Defined: Boolean read GetDefined write SetDefined;
    property Extent: TlicgExtent read GetExtent write SetExtent;
    property Description: string read GetDescription write SetDescription;
    property GeomPoints: IlicgVector read GetGeomPoints;
    property workScale: double read GetworkScale write SetworkScale;
  end;

  { TlicgBaseTable }

  TlicgIndexUnique = (iuUnique, iuDuplicates);

  TlicgSortStatus = (ssAscending, ssDescending);

{$IFDEF NATIVEDLL}
  TlicgDBFTypes = (dtClipper, dtDBaseIII, dtDBaseIV, dtFoxPro2);
{$ENDIF}

  TlicgBaseTable = class(TObject)
  private
    FGis: TlicgBaseGIS;
    function GetFieldAlias(FieldNo: Integer): string;
    function GetFieldAliasByName(const FieldName: string): string;
    procedure SetFieldAlias(FieldNo: Integer; const Value: string);
    procedure SetFieldAliasByName(const FieldName: string; const Value: string);
    function GetGis: TlicgBaseGIS;
    function GetLongFieldNames: TStrings;
    procedure SetGis(const Value: TlicgBaseGIS);
  protected
    function GetActive: Boolean; virtual; abstract;
    function GetRecNo: Integer; virtual; abstract;
    procedure SetActive(Value: Boolean); virtual; abstract;
    procedure SetRecNo(Value: Integer); virtual; abstract;
  public
    constructor Create(Gis: TlicgBaseGIS; const FName: string; ReadWrite, Shared:
      Boolean); virtual;
    constructor CreateAndOpen(Gis: TlicgBaseGis; const fname: string; ReadWrite,
      Shared: Boolean); virtual; abstract;
    constructor CreateNoOpen(Gis: TlicgBaseGIS); overload; virtual;
    constructor CreateNoOpen(Layer: TlicgBaseLayer); overload; virtual;
    destructor Destroy; override;
    procedure Append(NewRecNo: Integer); virtual; abstract;
    procedure AssignFrom(Dataset: TlicgBaseTable; SrcFieldNo, DstFieldNo: Integer); virtual; abstract;
    procedure BeginTrans; virtual; abstract;
    function Bof: Boolean; virtual; abstract;
    function DateGet(const FieldName: string): TDateTime; virtual; abstract;
    function DateGetN(FieldNo: Integer): TDateTime; virtual; abstract;
    procedure DatePut(const FieldName: string; Value: TDateTime); virtual; abstract;
    procedure DatePutN(FieldNo: Integer; Value: TDateTime); virtual; abstract;
    function DBCreateTable(const fname: string; AFieldList: TStringList): Boolean; virtual;
    function DBDropIndex(const TableName: string): Boolean; virtual;
    function DBDropTable(const TableName: string): Boolean; virtual;
    function DBRenameTable(const Source, Target: string): Boolean; virtual;
    function DBTableExists(const TableName: string): Boolean; virtual;
    procedure Delete; virtual; abstract;
    function Deleted: Boolean; virtual; abstract;
    procedure Edit; virtual; abstract;
    procedure EndTrans; virtual; abstract;
    function Eof: Boolean; virtual; abstract;
    function Field(FieldNo: Integer): string; virtual; abstract;
    function FieldCount: Integer; virtual; abstract;
    function FieldDec(FieldNo: Integer): Integer; virtual; abstract;
    function FieldGet(const FieldName: string): string; virtual; abstract;
    function FieldGetN(FieldNo: Integer): string; virtual; abstract;
    function FieldLen(FieldNo: Integer): Integer; virtual; abstract;
    function FieldNo(const FieldName: string): Integer; virtual; abstract;
    function FieldNofromAlias(const AliasName: string): Integer;
    procedure FieldPut(const FieldName, Value: string); virtual; abstract;
    procedure FieldPutN(FieldNo: Integer; const Value: string); virtual; abstract;
    function FieldType(FieldNo: Integer): AnsiChar; virtual; abstract;
    function Find(const ss: string; IsExact, IsNear: Boolean): Boolean; virtual; abstract;
    procedure First; virtual; abstract;
    function FloatGet(const Fieldname: string): Double; virtual; abstract;
    function FloatGetN(FieldNo: Integer): Double; virtual; abstract;
    procedure FloatPut(const FieldName: string; const Value: Double); virtual; abstract;
    procedure FloatPutN(FieldNo: Integer; const Value: Double); virtual; abstract;
    procedure FlushDB; virtual; abstract;
    procedure Go(n: Integer); virtual; abstract;
    function Index(const INames, Tag: string): Integer; virtual; abstract;
    function IndexAscending(Value: Integer): Boolean; virtual; abstract;
    function IndexCount: Integer; virtual; abstract;
    function IndexExpression(Value: Integer): string; virtual; abstract;
    function IndexFilter(Value: Integer): string; virtual; abstract;
    function IndexCurrent: string; virtual; abstract;
    procedure IndexOn(const IName, tag, keyexp, forexp: string; uniq:
      TlicgIndexUnique; ascnd: TlicgSortStatus); virtual; abstract;
    function IndexTagName(Value: Integer): string; virtual; abstract;
    function IndexUnique(Value: Integer): Boolean; virtual; abstract;
    function IntegerGet(const FieldName: string): Integer; virtual; abstract;
    function IntegerGetN(FieldNo: Integer): Integer; virtual; abstract;
    procedure IntegerPut(const Fieldname: string; Value: Integer); virtual; abstract;
    procedure IntegerPutN(FieldNo: Integer; Value: Integer); virtual; abstract;
    procedure Last; virtual; abstract;
    function LogicGet(const FieldName: string): Boolean; virtual; abstract;
    function LogicGetN(FieldNo: Integer): Boolean; virtual; abstract;
    procedure LogicPut(const fieldname: string; Value: Boolean); virtual; abstract;
    procedure LogicPutN(fieldno: Integer; Value: Boolean); virtual; abstract;
    procedure MemoLoad(const fieldname: string; Stream: TStream); virtual; abstract;
    procedure MemoLoadN(fieldno: Integer; Stream: TStream); virtual; abstract;
    procedure MemoSave(const FieldName: string; Stream: TStream); virtual; abstract;
    procedure MemoSaveN(FieldNo: Integer; Stream: TStream); virtual; abstract;
    function MemoSize(const FieldName: string): Integer; virtual; abstract;
    function MemoSizeN(FieldNo: Integer): Integer; virtual; abstract;
    procedure Next; virtual; abstract;
    procedure Pack; virtual; abstract;
    procedure Post; virtual; abstract;
    procedure Prior; virtual; abstract;
    procedure Recall; virtual; abstract;
    function RecordCount: Integer; virtual; abstract;
    procedure Refresh; virtual; abstract;
    procedure Reindex; virtual; abstract;
    procedure RollBackTrans; virtual;
    procedure SetTagTo(const TName: string); virtual; abstract;
    procedure SetUseDeleted(tf: Boolean); virtual; abstract;
    function StringGet(const FieldName: string): string; virtual; abstract;
    function StringGetN(FieldNo: Integer): string; virtual; abstract;
    procedure StringPut(const fieldname, Value: string); virtual; abstract;
    procedure StringPutN(fieldno: Integer; const Value: string); virtual; abstract;
    procedure Zap; virtual; abstract;
    function GetCapsulTable: TObject; virtual; abstract;
    property Active: Boolean read GetActive write SetActive;
    property FieldAlias[FieldNo: Integer]: string read GetFieldAlias write SetFieldAlias;
    property FieldAliasByName[const FieldName: string]: string read
      GetFieldAliasByName write SetFieldAliasByName;
    property Gis: TlicgBaseGIS read GetGis write SetGis;
    property LongFieldNames: TStrings read GetLongFieldNames;
    property RecNo: Integer read GetRecNo write SetRecNo;
    property CapsulTable: TObject read GetCapsulTable;
  end;

  TlicgBaseTableClass = class of TlicgBaseTable;

  TDesktopLayerType = (dtLiderGISFile, dtDB);

//Referans Eventler
  TReferenceGisEvent = procedure(const GIS: TlicgBaseGis); stdcall;

  TReferenceGisEvent_OpenProject = TReferenceGisEvent;

  TReferenceGisEvent_SaveProject = TReferenceGisEvent;

  TReferenceGisEvent_CloseProject = TReferenceGisEvent;

  TReferenceGisEvent_UpdateExtension = TReferenceGisEvent;

  TReferenceGisEvent_QuickUpdateExtension = TReferenceGisEvent;

  TReferenceGisEvent_RebuildTree = TReferenceGisEvent;

  TReferenceGisEvent_VectorDraw = procedure(GIS: TlicgBaseGis; WCRect: TlicgExtent;
    Canvas: IlicgCanvas; Grapher: TlicgBaseGrapher; Selection:
    TlicgBaseSelection; IsAerial: Boolean; ShowAnimationLayers: Boolean;
    PrintMode: TlicgPrintMode; BufferBitmap: TBitmap; ClipRgn: HRgn; PreviewDraw:
    Boolean; CurrentScale: double = 0; PMatrix: PlicgMatrix = nil); stdcall;

  TReferenceGisEvent_RasterDraw = procedure(GIS: TlicgBaseGis; WCRect: TlicgExtent;
    Canvas: IlicgCanvas; Grapher: TlicgBaseGrapher; ClipRgn: HRgn; PreviewDraw:
    Boolean; var CancelDraw: Boolean; CurrentScale: double = 0; PMatrix:
    PlicgMatrix = nil); stdcall;

  TFarOnchangeCurrProject = procedure(const PrevCmdline, FocusedCmdline: TObject);
    stdcall;

  IDrawSymbol = interface
    ['{C8C96D6A-A7B3-4C6B-A9C3-26CFAFB925C8}']
    procedure Draw(Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; const Clip:
      TlicgExtent; const Matrix: TlicgMatrix; DrawMode: TlicgDrawMode; ClipRgn: HRgn;
      PenTool: IlicgPenTool = nil); stdcall;
    procedure Add2RubberBandList(ACanvas: IlicgCanvas; RubberBandList:
      TlicgRubberList; Grapher: TlicgBaseGrapher; const Clip: TlicgExtent; const M:
      TlicgMatrix; PenColor: TColor; PenWidth: Byte; PenStyle: TPenStyle); stdcall;
  end;

  IDrawBlock = interface
    ['{E616CE8E-4B6D-4F12-8CCD-1EB732A332EF}']
    procedure Draw(Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; const Clip:
      TlicgExtent; const Matrix: TlicgMatrix; DrawMode: TlicgDrawMode; ClipRgn: HRgn;
      _PenTool: IlicgPenTool = nil); stdcall;
    procedure Add2RubberBandList(ACanvas: IlicgCanvas; RubberBandList:
      TlicgRubberList; Grapher: TlicgBaseGrapher; const Clip: TlicgExtent; const M:
      TlicgMatrix; PenColor: TColor; PenWidth: Byte; PenStyle: TPenStyle); stdcall;
  end;


  IDrawLineType = interface(IDrawSymbol)
    ['{8FB6CD9A-79A5-4DF0-96CA-D0382871AAF2}']
    procedure DrawVector(const V: IlicgVector; const ADrawTools: IlicgDrawTools;
      Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; const Clip: TlicgExtent; const
      Matrix: TlicgMatrix; DrawMode: TlicgDrawMode; ClipRgn: HRgn;
      aLineWidthDirection: TlicgLineWidthDirection; aProjectScale: double); stdcall;
  end;

  IDrawEntity = interface
    ['{D8AF6073-584E-460C-9AD3-2A08F405805B}']
    function isPreviewDraw: Boolean; stdcall;
    procedure Draw(const _Entity: IlicgEntity; Grapher: TlicgBaseGrapher; Canvas:
      IlicgCanvas; const Clip: TlicgExtent; DrawMode: TlicgDrawMode; ClipRgn: HRgn;
      AGIS: TlicgBaseGIS = nil; ALayer: TlicgBaseLayer = nil; isPreview: Boolean = false;
      isSymbolEntity: boolean = False; Flo: TObject = nil; isContrastingColor: boolean = true); stdcall;
    procedure DrawControlPoints(const _Entity: IlicgEntity; Grapher:
      TlicgBaseGrapher; Canvas: IlicgCanvas; const VisualWindow: TlicgExtent;
      TransfPts: Boolean; DefaultPaint: Boolean = True; HideVertexNumber:
      Boolean = False; UseLayerProps: Boolean = False); stdcall;
    procedure Add2RubberBandList(const _Entity: IlicgEntity; ACanvas:
      IlicgCanvas; RubberBandList: TlicgRubberList; Grapher: TlicgBaseGrapher;
      const Clip: TlicgExtent; PenColor: TColor; PenWidth: Byte; PenStyle: TPenStyle);
      stdcall;
    procedure ControlPointsAdd2RubberBandList(const _Entity: IlicgEntity;
      IsDisabled: Boolean; ACanvas: IlicgCanvas; RubberBandList: TlicgRubberList;
      Grapher: TlicgBaseGrapher; const VisualWindow: TlicgExtent; TransfPts,
      HideVertexNumber: Boolean; PenColor: TColor; FillColor: TColor); stdcall;

    // FittedText
    procedure DrawToPath(const _Entity: IlicgEntity; Path: IlicgVector;
      FittedToLen: Boolean; Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas;
      const Clip: TlicgExtent; DrawMode: TlicgDrawMode; ClipRgn: HRgn); stdcall;
  end;



  {-------------------------------------------------------------------------------}
  {                  Layers                                                       }
  {-------------------------------------------------------------------------------}

  { TlicgBaseLayerInfo }
  TlicgBaseLayerInfo = class
  private
    FLayer: TlicgBaseLayer;
  protected
    FDefSymbolTool: IlicgSymbolTool;
    FDefBlockTool: IlicgBlockTool;
    FDefPenTool: IlicgPenTool;
    FDefBrushTool: IlicgBrushTool;
    FDefFontToolTT: IlicgFontTool;
    FDefFontTool: IlicgFontTool;

    (* ilker layeroptions dan aktarma başlama *)
    { zoom range for layer }
    FZoomRangeActive: Boolean;
    FMinZoomScale: Double;
    FMaxZoomScale: Double;
    { zoom range for text }
    FTextZoomRangeActive: Boolean;
    FMinZoomScaleForText: Double;
    FMaxZoomScaleForText: Double;
    FBufferWidthForPolyline: Double;

    FThickness: Boolean;
    FThicknessWidth: Integer;

    FTextFixed: Boolean;
    FTextFixedSize: Integer;

    FSymbolFixed: Boolean;
    FSymbolFixedSize: Integer;

    FGuideIsoline: Boolean;
    FGuideIsolineInterval: Double;
    FDrawDirectionArrow: Boolean;

    FPointSize: Double;
    FPointAngle: Double;
    FPointSymbolIndex: Byte;
    FPointSymbolFileIndex: Byte;
    FApplyPointLayerValues: Boolean;
    (* ilker layeroptions dan aktarma bitiş *)

    function GetLayerHeader: TlicgLayerHeader; virtual; abstract;
    procedure SetLayerHeader(Value: TlicgLayerHeader); virtual; abstract;
    function GetCS: IlicgCS; virtual; abstract;
    function GetExtension: TlicgExtent; virtual; abstract;
    function GetIDCounter: Integer; virtual; abstract;
    function GetIsAnimationLayer: Boolean; virtual;
    function GetIsCosmethic: Boolean; virtual;
    function GetWithConnectors: Boolean; virtual; abstract;
    procedure SetCS(Value: IlicgCS); virtual; abstract;
    procedure SetExtension(const Value: TlicgExtent); virtual; abstract;
    procedure SetIDCounter(Value: Integer); virtual; abstract;
    procedure SetIsAnimationLayer(Value: Boolean); virtual; abstract;
    procedure SetIsCosmethic(Value: Boolean); virtual; abstract;
    procedure SetWithConnectors(Value: Boolean); virtual; abstract;
    function GetParselLayer: Boolean; virtual; abstract;
    procedure SetParselLayer(Value: Boolean); virtual; abstract;
    function GetLayerBrush: Boolean; virtual; abstract;
    procedure SetLayerBrush(Value: Boolean); virtual; abstract;
    function GetModified: Boolean; virtual; abstract;
    procedure SetModified(Value: Boolean); virtual; abstract;
    function GetIsIndexed: Boolean; virtual; abstract;
    function GetLocked: Boolean; virtual; abstract;
    function GetSelectable: Boolean; virtual; abstract;
    function GetUseAttachedDB: Boolean; virtual; abstract;
    function GetVisible: Boolean; virtual; abstract;
    function GetApplyDefSymbol: Boolean; virtual; abstract;
    function GetApplyDefPen: Boolean; virtual; abstract;
    function GetApplyDefBrush: Boolean; virtual; abstract;
    function GetApplyDefFontTT: Boolean; virtual; abstract;
    function GetApplyDefFont: Boolean; virtual; abstract;
    procedure SetApplyDefSymbol(Value: Boolean); virtual; abstract;
    procedure SetApplyDefPen(Value: Boolean); virtual; abstract;
    procedure SetApplyDefBrush(Value: Boolean); virtual; abstract;
    procedure SetApplyDefFontTT(Value: Boolean); virtual; abstract;
    procedure SetApplyDefFont(Value: Boolean); virtual; abstract;
    procedure SetIsIndexed(Value: Boolean); virtual; abstract;
    procedure SetLocked(Value: Boolean); virtual; abstract;
    procedure SetSelectable(Value: Boolean); virtual; abstract;
    procedure SetUseAttachedDB(Value: Boolean); virtual; abstract;
    procedure SetVisible(Value: Boolean); virtual; abstract;
    function GetTolerance: Double; virtual; abstract;
    procedure SetTolerance(const Value: Double); virtual; abstract;
    function GetBalastro: Boolean; virtual; abstract;
    procedure SetBalastro(Value: Boolean); virtual; abstract;
    function GetPointNameONOFF: Boolean; virtual; abstract;
    procedure SetPointNameONOFF(Value: Boolean); virtual; abstract;
    function GetPointKodONOFF: Boolean; virtual; abstract;
    procedure SetPointKodONOFF(Value: Boolean); virtual; abstract;
    function GetPolygonNameONOFF: Boolean; virtual; abstract;
    procedure SetPolygonNameONOFF(Value: Boolean); virtual; abstract;
    function GetPrintONOFF: Boolean; virtual; abstract;
    procedure SetPrintONOFF(Value: Boolean); virtual; abstract;
    function GetPenNo: Byte; virtual; abstract;
    procedure SetPenNo(Value: Byte); virtual; abstract;
    function GetShowArrow: Boolean; virtual; abstract;
    procedure SetShowArrow(Value: Boolean); virtual; abstract;
    function GetZDisplayOnOff: Boolean; virtual; abstract;
    procedure SetZDisplayOnOff(Value: Boolean); virtual; abstract;
    function GetShowDBInfoOnCreateEnt: Boolean; virtual; abstract;
    procedure SetShowDBInfoOnCreateEnt(Value: Boolean); virtual; abstract;
    function GetIsNetworkLayer: Boolean; virtual; abstract;
    procedure SetIsNetworkLayer(Value: Boolean); virtual; abstract;
    procedure SetRecordCount(Value: Integer); virtual; abstract;
    function GetRecordCount: Integer; virtual; abstract;
    function GetVersionNumber: smallInt; virtual; abstract;
    procedure SetVersionNumber(Value: smallInt); virtual; abstract;
    function GetHeaderID: smallInt; virtual; abstract;
    procedure SetHeaderID(Value: smallInt); virtual; abstract;
    function GetIsMemoryLayer: Boolean; virtual; abstract;
    procedure SetIsMemoryLayer(Value: Boolean); virtual; abstract;
    function GetReserved(index: Integer): Byte; virtual; abstract;
    procedure SetReserved(index: Integer; Value: Byte); virtual; abstract;
    function GetDefBrushTool: IlicgBrushTool; virtual;
    function GetDefFontTool: IlicgFontTool; virtual;
    function GetDefFontToolTT: IlicgFontTool; virtual;
    function GetDefPenTool: IlicgPenTool; virtual;
    function GetDefSymbolTool: IlicgSymbolTool; virtual;
    function GetDefBlockTool: IlicgBlockTool; virtual;
    function GetLabeling: Boolean; virtual;
    procedure SetLabeling(Value: Boolean); virtual;
    function GetNodeVisible: Boolean; virtual;
    procedure SetNodeVisible(Value: Boolean); virtual;
    function GetisApplyBufferForPolyline: Boolean; virtual;
    procedure SetisApplyBufferForPolyline(Value: Boolean); virtual;

    (* ilker layeroptions dan aktarma başlama *)
    function GetZoomRangeActive: Boolean; virtual; abstract;
    procedure SetZoomRangeActive(Value: Boolean); virtual; abstract;
    function GetMinZoomScale: Double; virtual; abstract;
    procedure SetMinZoomScale(Value: Double); virtual; abstract;
    function GetMaxZoomScale: Double; virtual; abstract;
    procedure SetMaxZoomScale(Value: Double); virtual; abstract;
    function GetMinZoomScaleForText: Double; virtual; abstract;
    procedure SetMinZoomScaleForText(Value: Double); virtual; abstract;
    function GetMaxZoomScaleForText: Double; virtual; abstract;
    procedure SetMaxZoomScaleForText(Value: Double); virtual; abstract;
    function GetTextZoomRangeActive: Boolean; virtual; abstract;
    procedure SetTextZoomRangeActive(Value: Boolean); virtual; abstract;
    function GetBufferWidthForPolyline: Double; virtual; abstract;
    procedure SetBufferWidthForPolyline(Value: Double); virtual; abstract;
    function GetColor: TColor; virtual; abstract;
    procedure SetColor(Value: TColor); virtual; abstract;

    function GetThickness: Boolean; virtual; abstract;
    function GetThicknessWidth: Integer; virtual; abstract;
    procedure SetThickness(const Value: Boolean); virtual; abstract;
    procedure SetThicknessWidth(const Value: Integer); virtual; abstract;

    function GetTextFixed: Boolean; virtual; abstract;
    function GetTextFixedSize: Integer; virtual; abstract;
    procedure SetTextFixed(const Value: Boolean); virtual; abstract;
    procedure SetTextFixedSize(const Value: Integer); virtual; abstract;

    function GetSymbolFixed: Boolean; virtual; abstract;
    function GetSymbolFixedSize: Integer; virtual; abstract;
    procedure SetSymbolFixed(const Value: Boolean); virtual; abstract;
    procedure SetSymbolFixedSize(const Value: Integer); virtual; abstract;

    function GetGuideIsoline: Boolean; virtual; abstract;
    function GetGuideIsolineInterval: Double; virtual; abstract;
    procedure SetGuideIsoline(const Value: Boolean); virtual; abstract;
    procedure SetGuideIsolineInterval(const Value: Double); virtual; abstract;

    function GetDrawDirectionArrow: Boolean; virtual; abstract;
    procedure SetDrawDirectionArrow(const Value: Boolean); virtual; abstract;

    function GetPointSize: Double; virtual; abstract;
    function GetPointAngle: Double; virtual; abstract;
    function GetPointSymbolIndex: Byte; virtual; abstract;
    function GetPointSymbolFileIndex: Byte;virtual; abstract;
    function GetApplyPointLayerValues: Boolean; virtual; abstract;

    procedure SetPointSize(const Value: Double); virtual; abstract;
    procedure SetPointAngle(const Value: Double); virtual; abstract;
    procedure SetPointSymbolIndex(const Value: Byte); virtual; abstract;
    procedure SetPointSymbolFileIndex(const Value: Byte); virtual; abstract;
    procedure SetApplyPointLayerValues(const Value: Boolean); virtual; abstract;

    function GetTransparent: Boolean; virtual; abstract;
    function GetTransparency: Byte; virtual; abstract;
    procedure SetTransparent(const Value: Boolean); virtual; abstract;
    procedure SetTransparency(const Value: Byte); virtual; abstract;

    (* ilker layeroptions dan aktarma bitiş *)
  public
    FHeader: TlicgLayerHeader;
    constructor Create(Layer: TlicgBaseLayer); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TlicgBaseLayerInfo);

    // Tabakanın Kullacıya gösterilip gösterilmeyeceğini karar verir
    function DisplayUser: Boolean;
    property VersionNumber: SmallInt read GetVersionNumber write SetVersionNumber;
    property HeaderID: SmallInt read GetHeaderID write SetHeaderID;
    property IsMemoryLayer: Boolean read GetIsMemoryLayer write SetIsMemoryLayer;
    property RecordCount: Integer read GetRecordCount write SetRecordCount;
    property CS: IlicgCS read GetCS write SetCS;
    property DefBrushTool: IlicgBrushTool read GetDefBrushTool;
    property DefFontTool: IlicgFontTool read GetDefFontTool;
    property DefFontToolTT: IlicgFontTool read GetDefFontToolTT;
    property DefPenTool: IlicgPenTool read GetDefPenTool;
    property DefSymbolTool: IlicgSymbolTool read GetDefSymbolTool;
    property DefBlockTool: IlicgBlockTool read GetDefBlockTool;
    property Extension: TlicgExtent read GetExtension write SetExtension;
    property IDCounter: Integer read GetIDCounter write SetIDCounter;
    property IsAnimationLayer: Boolean read GetIsAnimationLayer write SetIsAnimationLayer;
    property IsCosmethic: Boolean read GetIsCosmethic write SetIsCosmethic;
    property IsIndexed: Boolean read GetIsIndexed write SetIsIndexed;
    property Locked: Boolean read GetLocked write SetLocked;
    property Selectable: Boolean read GetSelectable write SetSelectable;
    property UseAttachedDB: Boolean read GetUseAttachedDB write SetUseAttachedDB;
    property WithConnectors: Boolean read GetWithConnectors write SetWithConnectors;
    property ApplyDefSymbol: Boolean read GetApplyDefSymbol write SetApplyDefSymbol;
    property ApplyDefPen: Boolean read GetApplyDefPen write SetApplyDefPen default false;
    property ApplyDefBrush: Boolean read GetApplyDefBrush write SetApplyDefBrush default false;
    property ApplyDefFontTT: Boolean read GetApplyDefFontTT write SetApplyDefFontTT default false;
    property ApplyDefFont: Boolean read GetApplyDefFont write SetApplyDefFont default false;
    property Visible: Boolean read GetVisible write SetVisible;
    property Tolerance: Double read GetTolerance write SetTolerance;
    property IsLayerBrush: Boolean read GetLayerBrush write SetLayerBrush;
    property Balastro: Boolean read GetBalastro write SetBalastro;
    property PointNameOnOFF: Boolean read GetPointNameONOFF write SetPointNameONOFF default True; //ilker
    property PointKodOnOFF: Boolean read GetPointKodONOFF write SetPointKodONOFF;
    property PolygonNameOnOFF: Boolean read GetPolygonNameONOFF write SetPolygonNameONOFF;
    property PrintOnOFF: Boolean read GetPrintONOFF write SetPrintONOFF;
    property PenNo: Byte read GetPenNo write SetPenNo;
    property ShowArrow: Boolean read GetShowArrow write SetShowArrow;
    property ZDisplayOnOff: Boolean read GetZDisplayOnOff write SetZDisplayOnOff;
    property Modified: Boolean read GetModified write SetModified;
    property ShowDBInfoOnCreateEnt: Boolean read GetShowDBInfoOnCreateEnt write SetShowDBInfoOnCreateEnt;
    property IsNetworkLayer: Boolean read GetIsNetworkLayer write SetIsNetworkLayer;
    property LayerHeader: TlicgLayerHeader read GetLayerHeader write SetLayerHeader;
    property Labeling: Boolean read GetLabeling write SetLabeling;
    property Reserved[Index: Integer]: byte read GetReserved write SetReserved;
    property NodeVisible: Boolean read GetNodeVisible write SetNodeVisible;
    property isApplyBufferForPolyline: Boolean read GetisApplyBufferForPolyline write SetisApplyBufferForPolyline;

    (* ilker layeroptions dan aktarma başlama *)
    property ZoomRangeActive: Boolean read GetZoomRangeActive write SetZoomRangeActive;
    property MinZoomScale: Double read GetMinZoomScale write SetMinZoomScale;
    property MaxZoomScale: Double read GetMaxZoomScale write SetMaxZoomScale;
    property MinZoomScaleForText: Double read GetMinZoomScaleForText write SetMinZoomScaleForText;
    property MaxZoomScaleForText: Double read GetMaxZoomScaleForText write SetMaxZoomScaleForText;
    property TextZoomRangeActive: Boolean read GetTextZoomRangeActive write SetTextZoomRangeActive;
    property BufferWidthForPolyline: double read GetBufferWidthForPolyline write SetBufferWidthForPolyline;

    property Thickness: Boolean read GetThickness write SetThickness;
    property ThicknessWidth: Integer read GetThicknessWidth write SetThicknessWidth;

    property TextFixed: Boolean read GetTextFixed write SetTextFixed;
    property TextFixedSize: Integer read GetTextFixedSize write SetTextFixedSize;
    property SymbolFixed: Boolean read GetSymbolFixed write SetSymbolFixed;
    property SymbolFixedSize: Integer read GetSymbolFixedSize write SetSymbolFixedSize;

    property GuideIsoline: Boolean read GetGuideIsoline write SetGuideIsoline;
    property GuideIsolineInterval: Double read GetGuideIsolineInterval write SetGuideIsolineInterval;
    property DrawDirectionArrow: Boolean read GetDrawDirectionArrow write SetDrawDirectionArrow;

    property PointSize: Double read GetPointSize write SetPointSize;
    property PointAngle: Double read GetPointAngle write SetPointAngle;
    property PointSymbolIndex: Byte read GetPointSymbolIndex write SetPointSymbolIndex;
    property PointSymbolFileIndex: Byte read GetPointSymbolFileIndex write SetPointSymbolFileIndex;
    property ApplyPointLayerValues: Boolean read GetApplyPointLayerValues write SetApplyPointLayerValues;

    property Color: TColor read GetColor write SetColor;

    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Transparency: Byte read GetTransparency write SetTransparency;
    (* ilker layeroptions dan aktarma bitiş *)
  end;

  // the default Tools for layers that uses a header defined in EzBase.Pas unit
  TlicgLayerSymbolTool = class(TlicgBaseSymbolTool)
  private
    FLayerInfo: TlicgBaseLayerInfo;
  protected
    function GetSymbolStyle: TlicgSymbolStyle; override; stdcall;
    procedure SetSymbolStyle(Value: TlicgSymbolStyle); override; stdcall;
    function GetIndex: Integer; override; stdcall;
    procedure SetIndex(Value: Integer); override; stdcall;
    function GetSymbolFileIndex: Integer; override; stdcall;
    procedure SetSymbolFileIndex(Value: Integer); override; stdcall;
    function GetRotangle: Single; override; stdcall;
    procedure SetRotangle(const Value: Single); override; stdcall;
    function GetHeight: Double; override; stdcall;
    procedure SetHeight(const Value: Double); override; stdcall;
  public
    constructor Create(LayerInfo: TlicgBaseLayerInfo);
  end;

  TlicgLayerPenTool = class(TlicgBasePenTool)
  private
    FLayerInfo: TlicgBaseLayerInfo;
  protected
    function GetPenStyle: TlicgPenStyle; override; stdcall;
    procedure SetPenStyle(Value: TlicgPenStyle); override; stdcall;
    function GetStyle: Integer; override; stdcall;
    procedure SetStyle(Value: Integer); override; stdcall;
    function GetColor: TColor; override; stdcall;
    procedure SetColor(Value: TColor); override; stdcall;
    function GetWidth: Double; override; stdcall;
    procedure SetWidth(const Value: Double); override; stdcall;
    function GetFlow: Boolean; override; stdcall;
    procedure SetFlow(const Value: Boolean); override; stdcall;
    function GetLineWidthDirection: TlicgLineWidthDirection; override; stdcall;
    procedure SetLineWidthDirection(Value: TlicgLineWidthDirection); override; stdcall;
  public
    constructor Create(LayerInfo: TlicgBaseLayerInfo);
  end;

  TlicgLayerBrushTool = class(TlicgBaseBrushTool)
  private
    FLayerInfo: TlicgBaseLayerInfo;
  protected
    function GetPattern: Integer; override; stdcall;
    procedure SetPattern(Value: Integer); override; stdcall;
    function GetBackColor: TColor; override; stdcall;
    function GetForeColor: TColor; override; stdcall;
    procedure SetBackColor(const Value: TColor); override; stdcall;
    procedure SetForeColor(const Value: TColor); override; stdcall;
    function GetFillFactor: Single; override; stdcall;
    procedure SetFillFactor(const Value: Single); override; stdcall;
    function GetFillDirection: TlicgFillDirection; override; stdcall;
    procedure SetFillDirection(const Value: TlicgFillDirection); override; stdcall;
    function GetGradientFill: Boolean; override; stdcall;
    procedure SetGradientFill(const Value: Boolean); override; stdcall;
    function GetGradientDirection: TlicgFillDirection; override; stdcall;
    procedure SetGradientDirection(const Value: TlicgFillDirection); override; stdcall;
    function GetBrushStyle: TlicgBrushStyle; override; stdcall;
    procedure SetBrushStyle(Value: TlicgBrushStyle); override; stdcall;
  public
    constructor Create(LayerInfo: TlicgBaseLayerInfo);
  end;

  TlicgLayerFontTool = class(TlicgBaseFontTool)
  private
    FLayerInfo: TlicgBaseLayerInfo;
  protected
    function GetFontStyle: TlicgFontStyle; override; stdcall;
    procedure SetFontStyle(Value: TlicgFontStyle); override; stdcall;
    function GetName: string; override;
    procedure SetName(const Value: string); override; stdcall;
    function GetColor: TColor; override; stdcall;
    procedure SetColor(Value: TColor); override; stdcall;
    function GetAngle: single; override; stdcall;
    procedure SetAngle(const Value: single); override; stdcall;
    function GetHeight: Double; override; stdcall;
    procedure SetHeight(const Value: Double); override; stdcall;
    function GetStyle: TFontStyles; override; stdcall;
    procedure SetStyle(const Value: TFontStyles); override; stdcall;
    function GetCharSet: TFontCharSet; override; stdcall;
    procedure SetCharSet(const Value: TFontCharSet); override; stdcall;
    (*uğur ekleme başlama*)
    function GetCharWidthFactor: Single; override; stdcall;
    procedure SetCharWidthFactor(AValue: Single); override; stdcall;

    function GetCharSpacing: Single; override; stdcall;
    procedure SetCharSpacing(AValue: Single); override; stdcall;

    function GetTextPos: TlicgTextPos; override; stdcall;
    procedure SetTextPos(AValue: TlicgTextPos); override; stdcall;
    (*uğur ekleme bitme*)
  public
    constructor Create(LayerInfo: TlicgBaseLayerInfo);
  end;

  TlicgLayerFontToolTT = class(TlicgBaseFontTool)
  private
    FLayerInfo: TlicgBaseLayerInfo;
  protected
    function GetFontStyle: TlicgFontStyle; override; stdcall;
    procedure SetFontStyle(Value: TlicgFontStyle); override; stdcall;
    function GetName: string; override; stdcall;
    procedure SetName(const Value: string); override; stdcall;
    function GetColor: TColor; override; stdcall;
    procedure SetColor(Value: TColor); override; stdcall;
    function GetAngle: single; override; stdcall;
    procedure SetAngle(const Value: single); override; stdcall;
    function GetHeight: Double; override; stdcall;
    procedure SetHeight(const Value: Double); override; stdcall;
    function GetStyle: TFontStyles; override; stdcall;
    procedure SetStyle(const Value: TFontStyles); override; stdcall;
    function GetCharSet: TFontCharSet; override; stdcall;
    procedure SetCharSet(const Value: TFontCharSet); override; stdcall;
    (*uğur ekleme başlama*)
    function GetCharWidthFactor: Single; override; stdcall;
    procedure SetCharWidthFactor(AValue: Single); override; stdcall;

    function GetCharSpacing: Single; override; stdcall;
    procedure SetCharSpacing(AValue: Single); override; stdcall;

    function GetTextPos: TlicgTextPos; override; stdcall;
    procedure SetTextPos(AValue: TlicgTextPos); override; stdcall;
    (*uğur ekleme bitme*)
  public
    constructor Create(LayerInfo: TlicgBaseLayerInfo);
  end;

  TlicgLayerInfo = class(TlicgBaseLayerInfo)
  private
    FModified: Boolean;
  protected
    function GetLayerHeader: TlicgLayerHeader; override;
    procedure SetLayerHeader(Value: TlicgLayerHeader); override;
    function GetIsCosmethic: Boolean; override;
    procedure SetIsCosmethic(Value: Boolean); override;
    function GetExtension: TlicgExtent; override;
    procedure SetExtension(const Value: TlicgExtent); override;
    function GetIDCounter: Integer; override;
    procedure SetIDCounter(Value: Integer); override;
    function GetIsAnimationLayer: Boolean; override;
    procedure SetIsAnimationLayer(Value: Boolean); override;
    function GetCS: IlicgCS; override;
    procedure SetCS(Value: IlicgCS); override;
    function GetUseAttachedDB: Boolean; override;
    procedure SetUseAttachedDB(Value: Boolean); override;
    procedure SetLocked(Value: Boolean); override;
    procedure SetWithConnectors(Value: Boolean); override;
    function GetWithConnectors: Boolean; override;
    function GetLayerBrush: Boolean; override;
    procedure SetLayerBrush(Value: Boolean); override;
    function GetModified: Boolean; override;
    procedure SetModified(Value: Boolean); override;
    function GetIsIndexed: Boolean; override;
    function GetLocked: Boolean; override;
    function GetSelectable: Boolean; override;
    function GetVisible: Boolean; override;
    function GetApplyDefSymbol: Boolean; override;
    function GetApplyDefPen: Boolean; override;
    function GetApplyDefBrush: Boolean; override;
    function GetApplyDefFontTT: Boolean; override;
    function GetApplyDefFont: Boolean; override;
    procedure SetApplyDefSymbol(Value: Boolean); override;
    procedure SetApplyDefPen(Value: Boolean); override;
    procedure SetApplyDefBrush(Value: Boolean); override;
    procedure SetApplyDefFontTT(Value: Boolean); override;
    procedure SetApplyDefFont(Value: Boolean); override;
    procedure SetIsIndexed(Value: Boolean); override;
    procedure SetSelectable(Value: Boolean); override;
    procedure SetVisible(Value: Boolean); override;
    function GetTolerance: Double; override;
    procedure SetTolerance(const Value: Double); override;
    function GetBalastro: Boolean; override;
    procedure SetBalastro(Value: Boolean); override;
    function GetPointNameONOFF: Boolean; override;
    procedure SetPointNameONOFF(Value: Boolean); override;
    function GetPointKodONOFF: Boolean; override;
    procedure SetPointKodONOFF(Value: Boolean); override;
    function GetPolygonNameONOFF: Boolean; override;
    procedure SetPolygonNameONOFF(Value: Boolean); override;
    function GetPrintONOFF: Boolean; override;
    procedure SetPrintONOFF(Value: Boolean); override;
    function GetPenNo: Byte; override;
    procedure SetPenNo(Value: Byte); override;
    function GetShowArrow: Boolean; override;
    procedure SetShowArrow(Value: Boolean); override;
    function GetZDisplayOnOff: Boolean; override;
    procedure SetZDisplayOnOff(Value: Boolean); override;
    function GetShowDBInfoOnCreateEnt: Boolean; override;
    procedure SetShowDBInfoOnCreateEnt(Value: Boolean); override;
    function GetIsNetworkLayer: Boolean; override;
    procedure SetIsNetworkLayer(Value: Boolean); override;
    procedure SetRecordCount(Value: Integer); override;
    function GetRecordCount: Integer; override;
    function GetVersionNumber: smallInt; override;
    procedure SetVersionNumber(Value: smallInt); override;
    function GetHeaderID: smallInt; override;
    procedure SetHeaderID(Value: smallInt); override;
    function GetIsMemoryLayer: Boolean; override;
    procedure SetIsMemoryLayer(Value: Boolean); override;
    function GetReserved(index: Integer): Byte; override;
    procedure SetReserved(index: Integer; Value: Byte); override;

    (* ilker layeroptions dan aktarma başlama *)
    function GetZoomRangeActive: Boolean; override;
    procedure SetZoomRangeActive(Value: Boolean); override;
    function GetMinZoomScale: Double; override;
    procedure SetMinZoomScale(Value: Double); override;
    function GetMaxZoomScale: Double; override;
    procedure SetMaxZoomScale(Value: Double); override;
    function GetMinZoomScaleForText: Double; override;
    procedure SetMinZoomScaleForText(Value: Double); override;
    function GetMaxZoomScaleForText: Double; override;
    procedure SetMaxZoomScaleForText(Value: Double); override;
    function GetTextZoomRangeActive: Boolean; override;
    procedure SetTextZoomRangeActive(Value: Boolean); override;
    function GetBufferWidthForPolyline: Double; override;
    procedure SetBufferWidthForPolyline(Value: Double); override;
    function GetColor: TColor; override;
    procedure SetColor(Value: TColor); override;

    function GetThickness: Boolean; override;
    function GetThicknessWidth: Integer; override;
    procedure SetThickness(const Value: Boolean); override;
    procedure SetThicknessWidth(const Value: Integer); override;

    function GetTextFixed: Boolean; override;
    function GetTextFixedSize: Integer; override;
    procedure SetTextFixed(const Value: Boolean); override;
    procedure SetTextFixedSize(const Value: Integer); override;

    function GetSymbolFixed: Boolean; override;
    function GetSymbolFixedSize: Integer; override;
    procedure SetSymbolFixed(const Value: Boolean); override;
    procedure SetSymbolFixedSize(const Value: Integer); override;

    function GetGuideIsoline: Boolean; override;
    function GetGuideIsolineInterval: Double; override;
    procedure SetGuideIsoline(const Value: Boolean); override;
    procedure SetGuideIsolineInterval(const Value: Double); override;

    function GetDrawDirectionArrow: Boolean; override;
    procedure SetDrawDirectionArrow(const Value: Boolean); override;

    function GetPointSize: Double; override;
    function GetPointAngle: Double; override;
    function GetPointSymbolIndex: Byte; override;
    function GetPointSymbolFileIndex: Byte; override;
    function GetApplyPointLayerValues: Boolean; override;

    procedure SetPointSize(const Value: Double); override;
    procedure SetPointAngle(const Value: Double); override;
    procedure SetPointSymbolIndex(const Value: Byte); override;
    procedure SetPointSymbolFileIndex(const Value: Byte); override;
    procedure SetApplyPointLayerValues(const Value: Boolean); override;

    function GetTransparent: Boolean; override;
    function GetTransparency: Byte; override;
    procedure SetTransparent(const Value: Boolean); override;
    procedure SetTransparency(const Value: Byte); override;

    (* ilker layeroptions dan aktarma bitiş *)
  public
    constructor Create(Layer: TlicgBaseLayer); override;
  end;

  TlicgLayerLabelInfo = record
    ShowViewingOrder: Boolean;
    LabelFieldNames: string;
    DelimitedChar: string[1];
  end;

  PlicgLayerLabelInfo = ^TlicgLayerLabelInfo;

  { layer events }
  TlicgBeforePaintEntityEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; Entity: IlicgEntity; Grapher: TlicgBaseGrapher; Canvas:
    IlicgCanvas; const Clip: TlicgExtent; DrawMode: TlicgDrawMode; var CanShow:
    Boolean; var EntList: IlicgEntityList; var Autofree: Boolean) of object;

  TlicgAfterPaintEntityEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; Entity: IlicgEntity; Grapher: TlicgBaseGrapher; Canvas:
    IlicgCanvas; const Clip: TlicgExtent; DrawMode: TlicgDrawMode) of object;

  TlicgLayerType = (ltMemory, ltDesktop, ltPostgres);

  TlicgLayerClassType = (lctLayer, lctMemoryLayer, lctSHPLayer, lctDGNLayer,
    lctVirtualLayer, lctUserLayer, lctPostgresUserTable,
    lctPostgresStandartTable, lctNCLayer);

  TlicgLayerClassTypes = set of TlicgLayerClassType;

  TProgCallBackUpdateMessage = procedure(const ProgCnt: Integer) of object;

  IDatabaseModulProperties = IInterfaceList;

  IDatabaseModulLayerProperites = IInterfaceList;

  ILayerConnectorAndSqlInfo = interface
    ['{F74907AE-6175-4E33-912F-23DFD32C2ABE}']
    function Get_ConnParams: TConnectionParams; stdcall;
    procedure Set_ConnParams(Value: TConnectionParams); stdcall;
    function GetSpatialType: Integer; stdcall;
    function GetHostName: string; stdcall;
    function GetDatabase: string; stdcall;
    function GetUser: string; stdcall;
    function GetPassword: string; stdcall;
    function GetPort: Integer; stdcall;
    function GetEncoding: string; stdcall;
    procedure SetSpatialType(Value: Integer); stdcall;
    procedure SetHostName(Value: string); stdcall;
    procedure SetDatabase(Value: string); stdcall;
    procedure SetUser(Value: string); stdcall;
    procedure SetPassword(Value: string); stdcall;
    procedure SetPort(Value: Integer); stdcall;
    procedure SetEncoding(Value: string); stdcall;
    function GetTableName: string; stdcall;
    procedure SetTableName(Value: string); stdcall;
    property SpatialType: Integer read GetSpatialType write SetSpatialType;
    property HostName: string read GetHostName write SetHostName;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Port: Integer read GetPort write SetPort;
    property Encoding: string read GetEncoding write SetEncoding;
    property TableName: string read GetTableName write SetTableName;
    property ConnParams: TConnectionParams read Get_ConnParams write Set_ConnParams;
    function Get_TableSQLRecord: TTableSQLRecord; stdcall;
    procedure Set_TableSQLRecord(Value: TTableSQLRecord); stdcall;
    function GetLayerFullPathName: string; stdcall;
    function GetDisplayName: string; stdcall;
    function GetGroupCap: string; stdcall;
    function GetPrimaryIdField: string; stdcall;
    function GetInfoIdField: string; stdcall;
    function GetGeometryField: string; stdcall;
    function GetSQLOpenFileName: string; stdcall;
    function GetSQLGetFeatureFileName: string; stdcall;
    function GetDBFFields: string; stdcall;
    procedure SetLayerFullPathName(Value: string); stdcall;
    procedure SetDisplayName(Value: string); stdcall;
    procedure SetGroupCap(Value: string); stdcall;
    procedure SetPrimaryIdField(Value: string); stdcall;
    procedure SetInfoIdField(Value: string); stdcall;
    procedure SetGeometryField(Value: string); stdcall;
    procedure SetSQLOpenFileName(Value: string); stdcall;
    procedure SetSQLGetFeatureFileName(Value: string); stdcall;
    procedure SetDBFFields(Value: string); stdcall;
    procedure Assigned(LC: ILayerConnectorAndSqlInfo); stdcall;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property GroupCap: string read GetGroupCap write SetGroupCap;
    property PrimaryIdField: string read GetPrimaryIdField write SetPrimaryIdField;
    property InfoIdField: string read GetInfoIdField write SetInfoIdField;
    property GeometryField: string read GetGeometryField write SetGeometryField;
    property SQLOpenFileName: string read GetSQLOpenFileName write SetSQLOpenFileName;
    property SQLGetFeatureFileName: string read GetSQLGetFeatureFileName write
      SetSQLGetFeatureFileName;
    property LayerFullPathName: string read GetLayerFullPathName write
      SetLayerFullPathName;
    property DBFFields: string read GetDBFFields write SetDBFFields;
    property TableSQLRecord: TTableSQLRecord read Get_TableSQLRecord write
      Set_TableSQLRecord;
  end;

  TLayerConnectorAndSqlInfo = class(TInterfacedObject, ILayerConnectorAndSqlInfo)
  private
    F_TableSQLRecord: TTableSQLRecord;
    F_ConnParams: TConnectionParams;
  protected
    function Get_ConnParams: TConnectionParams; stdcall;
    procedure Set_ConnParams(Value: TConnectionParams); stdcall;
    function GetSpatialType: Integer; stdcall;
    function GetHostName: string; stdcall;
    function GetDatabase: string; stdcall;
    function GetUser: string; stdcall;
    function GetPassword: string; stdcall;
    function GetPort: Integer; stdcall;
    function GetEncoding: string; stdcall;
    procedure SetSpatialType(Value: Integer); stdcall;
    procedure SetHostName(Value: string); stdcall;
    procedure SetDatabase(Value: string); stdcall;
    procedure SetUser(Value: string); stdcall;
    procedure SetPassword(Value: string); stdcall;
    procedure SetPort(Value: Integer); stdcall;
    procedure SetEncoding(Value: string); stdcall;
    function GetTableName: string; stdcall;
    procedure SetTableName(Value: string); stdcall;
    function Get_TableSQLRecord: TTableSQLRecord; stdcall;
    procedure Set_TableSQLRecord(Value: TTableSQLRecord); stdcall;
    function GetLayerFullPathName: string; stdcall;
    function GetDisplayName: string; stdcall;
    function GetGroupCap: string; stdcall;
    function GetPrimaryIdField: string; stdcall;
    function GetInfoIdField: string; stdcall;
    function GetGeometryField: string; stdcall;
    function GetSQLOpenFileName: string; stdcall;
    function GetSQLGetFeatureFileName: string; stdcall;
    function GetDBFFields: string; stdcall;
    procedure SetLayerFullPathName(Value: string); stdcall;
    procedure SetDisplayName(Value: string); stdcall;
    procedure SetGroupCap(Value: string); stdcall;
    procedure SetPrimaryIdField(Value: string); stdcall;
    procedure SetInfoIdField(Value: string); stdcall;
    procedure SetGeometryField(Value: string); stdcall;
    procedure SetSQLOpenFileName(Value: string); stdcall;
    procedure SetSQLGetFeatureFileName(Value: string); stdcall;
    procedure SetDBFFields(Value: string); stdcall;
    procedure Assigned(LC: ILayerConnectorAndSqlInfo); stdcall;
  public
    constructor Create;
    property ConnParams: TConnectionParams read Get_ConnParams write Set_ConnParams;
    property TableSQLRecord: TTableSQLRecord read Get_TableSQLRecord write
      Set_TableSQLRecord;
  end;

  TPropertyValueType = (pvtUnknown, pvtString, pvtFloat, pvtInteger, pvtBoolean, pvtDate);

  TPropertyValue = class
    _Property: string;
    _Value: variant;
    _Type: TPropertyValueType;
  end;

  IPropertyValueList = interface
    ['{E6BC2193-AFDA-448C-8FD4-23CAE1E1A62C}']
    function GetPropValue(_Prop: string; var _val: variant): Boolean; stdcall;
    function Count: Integer; stdcall;
    function propName(i: Integer): string; stdcall;
    function propValue(i: Integer): variant; stdcall;
    function propType(i: Integer): TPropertyValueType; stdcall;
    function Add(_Prop: string; _val: variant; _type: TPropertyValueType =
      pvtUnknown): Integer; stdcall;
  end;

  TPropertyValueList = class(TInterfacedObject, IPropertyValueList)
  private
    FList: TList;
  public
    function GetPropValue(_Prop: string; var _val: variant): Boolean; stdcall;
    function Count: Integer; stdcall;
    function propName(i: Integer): string; stdcall;
    function propValue(i: Integer): variant; stdcall;
    function propType(i: Integer): TPropertyValueType; stdcall;
    function Add(_Prop: string; _val: variant; _type: TPropertyValueType =
      pvtUnknown): Integer; stdcall;
    constructor create;
    destructor destroy; override;
  end;

  IlicgDesktopLayer = interface
    ['{0B9532E1-2E7E-494F-A8BD-7E3410291537}']
   {----- Property functions ------}

    function GetActiveSearchCanceledEvent: Boolean; stdcall;
    procedure SetActiveSearchCanceledEvent(Value: Boolean); stdcall;
    function GetAutoflush: Boolean; stdcall;
    procedure SetAutoflush(Value: Boolean); stdcall;
    procedure SetLayerInfo(Value: TlicgBaseLayerInfo); stdcall;
    function GetGIS: TlicgBaseGIS; stdcall;
    procedure SetGIS(Value: TlicgBaseGIS); stdcall;
    function GetActive: Boolean; stdcall;
    procedure SetActive(Value: Boolean); stdcall;
    function GetBlinkers: IlicgIntegerList; stdcall;
    procedure SetBlinkers(Value: IlicgIntegerList); stdcall;
    function GetDBTable: TlicgBaseTable; stdcall;
    procedure SetDBTable(Value: TlicgBaseTable); stdcall;
    function GetFileName: string; stdcall;
    procedure SetFileName(const Value: string); stdcall;
    function GetIndex: Integer; stdcall;
    function GetLayerInfo: TlicgBaseLayerInfo; stdcall;
    function GetModified: Boolean; stdcall;
    procedure SetModified(const Value: Boolean); stdcall;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
    function GetRecNo: Integer; stdcall;
    procedure SetRecNo(Value: Integer); stdcall;
    function GetRecordCount: LongInt; stdcall;
    function Getrt: TRTree; stdcall;
    function GetOnBeforePaintEntity: TlicgBeforePaintEntityEvent; stdcall;
    procedure SetOnBeforePaintEntity(Value: TlicgBeforePaintEntityEvent); stdcall;
    procedure SetSpaceKeySelected(Value: Boolean); stdcall;
    function GetSpaceKeySelected: Boolean; stdcall;
    function GetLayerClassType: TlicgLayerClassType; stdcall;
    procedure SetLayerClassType(const Value: TlicgLayerClassType); stdcall;
    function GetDisplayName: string; stdcall;
    procedure SetDisplayName(Value: string); stdcall;
    function GetSaveHeaderVersion: smallint; stdcall;
    procedure SetSaveHeaderVersion(Value: smallint); stdcall;
   {----- END Property functions ------}

    function CanUndo(_UndoAction: TlicgUndoAction): Boolean; stdcall;
    function SupportEntitiyIDs: TlicgEntityIDs; stdcall;
    procedure ReadHeaders(HeaderVersion: smallint; var Stream: TStream); stdcall;
    function InitValuesBeforeAddEntity(const Entity: IlicgEntity; const Gis:
      TObject; var isUndo: Boolean; _PropVList: IUnknown = nil): Boolean; stdcall;
    function InfoTableName: string; stdcall;
    function GetProperties(EntityUid, InfoId: Integer): IDatabaseModulProperties; stdcall;
    function SetupGeometryRelation(EntityUid: Integer; var infoid: Integer; var
      NewLayer: TlicgBaseLayer; var NewRecNo: Integer): Boolean; stdcall;
    function GetLabelFields(var LabelFieldNames: string): Boolean; stdcall;

      ////
    function AddEntity(Entity: IlicgEntity; ApplyLayerProps: Boolean = true;
      GetfirstPropFromLayer: Boolean = true; _PropVList: IUnknown = nil):
      Integer; overload; stdcall;
    function AddEntity(Entity: IlicgEntity; infoid: Integer; ApplyLayerProps:
      Boolean = true; GetfirstPropFromLayer: Boolean = true; _Param: string = ''):
      Integer; overload; stdcall;
    function DeleteEntity(RecNo: Integer; var IgnoreAskDelete, CancelDelete:
      Boolean; LookRelation: Boolean = true): Boolean; overload; stdcall;
    function DeleteEntity(RecNo: Integer): Boolean; overload; stdcall;
    function UnDeleteEntity(RecNo: Integer): Boolean; stdcall;
    function UpdateEntity(RecNo: Integer; Entity: IlicgEntity; Grapher:
      TlicgBaseGrapher = nil): Boolean; stdcall;
    procedure UpdateApplyLayerPropsEntity(RecNo: Integer; Entity: IlicgEntity;
      _ApplyLayerProps: Boolean; Grapher: TlicgBaseGrapher = nil); stdcall;
    function GetInfoId(RecNo: Integer): Integer; stdcall;

      ///

    function BringEntityTofront(ARecNo: Integer): Integer; stdcall;
    function SendEntityToBack(ARecNo: Integer): Integer; stdcall;
    function MoveEntityForward(ARecNo: Integer): Integer; stdcall;
    function MoveEntityBackwards(ARecNo: Integer): Integer; stdcall;
    procedure CancelScope; stdcall;
    procedure ClearBlinkers; stdcall;
    procedure Close; stdcall;
    function ContainsDeleted: Boolean; stdcall;
    procedure CopyRecord(SourceRecNo, DestRecNo: Integer); stdcall;
    function DefinePolygonScope(Polygon: IlicgEntity; const Scope: string;
      operator: TlicgGraphicOperator; _oList: IlicgIntegerList = nil;
      NoPickFilter: TlicgEntityIDs = []): Boolean; overload; stdcall;
    function DefinePolygonScope(Polygon: IlicgEntity; const Scope: string;
      operator: TlicgGraphicOperator; _Sel: TlicgBaseSelection): Boolean; overload; stdcall;
    function DefineScope(const Scope: string): Boolean; stdcall;

    function DeleteLayerFiles: Boolean; stdcall;
    procedure EndBuffering; stdcall;
    function EntityType(RecNo: Integer): TlicgEntityID; stdcall;
    function EntityWithRecNo(RecNo: Integer): IlicgEntity; stdcall;
    function Eof: Boolean; stdcall;
    function Bof: Boolean; stdcall;
    procedure StartBatchInsert; stdcall;
    procedure CancelBatchInsert; stdcall;
    procedure FinishBatchInsert; stdcall;
    function InBatchInsert: Boolean; stdcall;
    procedure First; stdcall;
    procedure ForceOpened; stdcall;
    function GetExtensionForRecords(List: IlicgIntegerList): TlicgExtent; stdcall;
    procedure GetFieldList(Strings: TStrings); stdcall;
    function HasBlinkers: Boolean; stdcall;
    procedure InitializeOnCreate(const FileName: string; AttachedDB,
      IsAnimation: Boolean; ACS: IlicgCS; FieldList: TObject); stdcall;
    function IsCurrentLayer: Boolean; stdcall;
    procedure Last; stdcall;
    function LoadEntityWithRecNo(RecNo: Longint): IlicgEntity; stdcall;
    procedure MaxMinExtents(var AMinX, AMinY, AMaxX, AMaxY: Double); stdcall;
    procedure Next; stdcall;
    procedure Prior; stdcall;
    procedure Open; stdcall;
    procedure Pack(ShowMessages, ShowProgress: Boolean; CallBackUpdateMes:
      TProgCallBackUpdateMessage = nil); stdcall;
    procedure PopulateFieldList(Strings: TStrings; WithAlias: Boolean = False); stdcall;
    function QuickUpdateExtension: TlicgExtent; stdcall;
    procedure RebuildTree; stdcall;
    procedure Recall; stdcall;
    function RecEntity: IlicgEntity; stdcall;
    function RecEntityID: TlicgEntityID; stdcall;
    function RecExtension: TlicgExtent; stdcall;
    function RecIsDeleted: Boolean; overload; stdcall;
    function RecIsDeleted(RecNo: Integer): Boolean; overload; stdcall;
    function RecLoadEntity: IlicgEntity; stdcall;
    procedure RecLoadEntity2(Entity: IlicgEntity); stdcall;
    function SetGraphicFilter(s: TSearchType; const visualWindow: TlicgExtent;
      LabelInfo: PlicgLayerLabelInfo = nil): Boolean; stdcall;
    procedure StartBuffering; stdcall;
    procedure Synchronize; stdcall;
    function UpdateExtension(Grapher: TlicgBaseGrapher = nil): TlicgExtent; stdcall;
    function UpdateExtensionNotLoadEntity: TlicgExtent; stdcall;
    procedure WriteHeaders(FlushFiles: Boolean; ForceWrite: Boolean = false); stdcall;
    procedure Zap; stdcall;
    procedure SaveIfEntityVersionNumberChange; stdcall;
    procedure SetPasifEntity(RecNo: Integer); stdcall; // Parsel durumu virtual layer
    procedure SetAktifEntity(RecNo: Integer); stdcall; // Parsel durumu virtual layer

    procedure SaveToFile; stdcall;
    procedure LoadFromFile; stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    function GetLayerConnector: ILayerConnectorAndSqlInfo; stdcall;
    function ReferansLayer: TlicgBaseLayer; stdcall;
    procedure SaveInfoTableByPropList(RecNo: Integer; PropertyValueList:IPropertyValueList); stdcall;
    function GetSymBitmap: TSymBitmap; stdcall;
    procedure SetSymBitmap(Value: TSymBitmap); stdcall;
    property LayerConnector: ILayerConnectorAndSqlInfo read GetLayerConnector;
    property Active: Boolean read GetActive write SetActive;
    property Blinkers: IlicgIntegerList read GetBlinkers write SetBlinkers;
    property DBTable: TlicgBaseTable read GetDBTable write SetDBTable;
    property FileName: string read GetFileName write SetFileName;
    property _Index: Integer read GetIndex;
    property LayerInfo: TlicgBaseLayerInfo read GetLayerInfo;
    property Modified: Boolean read GetModified write SetModified;
    property Name: string read GetName write SetName;
    property RecNo: Integer read GetRecNo write SetRecNo;
    property RecordCount: LongInt read GetRecordCount;
    property RT: TRTree read GetRT;
    property OnBeforePaintEntity: TlicgBeforePaintEntityEvent read
      GetOnBeforePaintEntity write SetOnBeforePaintEntity;
    property SpaceKeySelected: Boolean read GetSpaceKeySelected write SetSpaceKeySelected;
    property LayerClassType: TlicgLayerClassType read GetLayerClassType write
      SetLayerClassType;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property SaveHeaderVersion: smallint read GetSaveHeaderVersion write
      SetSaveHeaderVersion;
  end;

  TlicgBaseLayer = class(TInterfacedObject, IlicgDesktopLayer)
  private
  protected
    FActiveSearchCanceledEvent: Boolean;
    FAutoflush,
    FSpaceKeySelected,
    FModified: Boolean;
    FFileName: string;
    FName: string;
    FGroupName: string;
    FDisplayName: string;
    FLayerInfo: TlicgBaseLayerInfo;
    FBlinkers: IlicgIntegerList;
    FOnBeforePaintEntity: TlicgBeforePaintEntityEvent;
    FLayerClassType: TlicgLayerClassType;
    FSaveHeaderVersion: Smallint;
    FGIS: TlicgBaseGIS;
    function GetActiveSearchCanceledEvent: Boolean; virtual; stdcall;
    procedure SetActiveSearchCanceledEvent(Value: Boolean); virtual; stdcall;
    function GetAutoflush: Boolean; virtual; stdcall;
    procedure SetAutoflush(Value: Boolean); virtual; stdcall;
    procedure SetLayerInfo(Value: TlicgBaseLayerInfo); virtual; stdcall;
    function GetGIS: TlicgBaseGIS; virtual; stdcall;
    procedure SetGIS(Value: TlicgBaseGIS); virtual; stdcall;
    function GetActive: Boolean; virtual; stdcall;
    procedure SetActive(Value: Boolean); virtual; stdcall;
    function GetBlinkers: IlicgIntegerList; virtual; stdcall;
    procedure SetBlinkers(Value: IlicgIntegerList); virtual; stdcall;
    function GetDBTable: TlicgBaseTable; virtual; stdcall;
    procedure SetDBTable(Value: TlicgBaseTable); virtual; stdcall;
    function GetFileName: string; virtual; stdcall;
    procedure SetFileName(const Value: string); virtual; stdcall;
    function GetIndex: Integer; virtual; stdcall;
    function GetLayerInfo: TlicgBaseLayerInfo; virtual; stdcall;
    function GetModified: Boolean; virtual; stdcall;
    procedure SetModified(const Value: Boolean); virtual; stdcall;
    function GetName: string; virtual; stdcall;
    procedure SetName(const Value: string); virtual; stdcall;
    function GetRecNo: Integer; virtual; stdcall;
    procedure SetRecNo(Value: Integer); virtual; stdcall;
    function GetRecordCount: LongInt; virtual; stdcall;
    function Getrt: TRTree; virtual; stdcall;
    function GetOnBeforePaintEntity: TlicgBeforePaintEntityEvent; virtual; stdcall;
    procedure SetOnBeforePaintEntity(Value: TlicgBeforePaintEntityEvent); virtual; stdcall;
    procedure SetSpaceKeySelected(Value: Boolean); virtual; stdcall;
    function GetSpaceKeySelected: Boolean; virtual; stdcall;
    function GetGroupName: string; virtual; stdcall;
    function GetLayerClassType: TlicgLayerClassType; virtual; stdcall;
    procedure SetLayerClassType(const Value: TlicgLayerClassType); virtual; stdcall;
    function GetDisplayName: string; virtual; stdcall;
    procedure SetDisplayName(Value: string); virtual; stdcall;
    function GetSaveHeaderVersion: smallint; virtual; stdcall;
    procedure SetSaveHeaderVersion(Value: smallint); virtual; stdcall;

   {----- END Property functions ------}

  public
    procedure ReadHeaders(HeaderVersion: SmallInt; var Stream: TStream); virtual; stdcall;
    constructor Create(AGIS: TlicgBaseGIS; const _FileName: string; IsAddToGis: Boolean = True);
    destructor Destroy; override;
    function CanUndo(_UndoAction: TlicgUndoAction): Boolean; virtual; stdcall;
    function SupportEntitiyIDs: TlicgEntityIDs; virtual; stdcall;
    function InitValuesBeforeAddEntity(const Entity: IlicgEntity; const Gis:
      TObject; var isUndo: Boolean; _PropVList: IUnknown): Boolean; virtual; stdcall;
    function InfoTableName: string; virtual; stdcall;
    function GetProperties(EntityUid, InfoId: Integer): IDatabaseModulProperties;
      virtual; stdcall; abstract;
    function SetupGeometryRelation(EntityUid: Integer; var infoid: Integer; var
      NewLayer: TlicgBaseLayer; var NewRecNo: Integer): Boolean; virtual;
      stdcall; abstract;
    function GetLabelFields(var LabelFieldNames: string): Boolean; virtual;
      stdcall; abstract;
    function AddEntity(Entity: IlicgEntity; ApplyLayerProps: Boolean = true;
      GetfirstPropFromLayer: Boolean = true; _PropVList: IUnknown = nil):
      Integer; overload; virtual; stdcall;
    function AddEntity(Entity: IlicgEntity; infoid: Integer; ApplyLayerProps:
      Boolean = true; GetfirstPropFromLayer: Boolean = true; _Param: string = ''):
      Integer; overload; virtual; stdcall;
    function UnDeleteEntity(RecNo: Integer): Boolean; virtual; stdcall; abstract;
    function UpdateEntity(RecNo: Integer; Entity: IlicgEntity; Grapher:
      TlicgBaseGrapher = nil): Boolean; virtual; stdcall; abstract;
    function DeleteEntity(RecNo: Integer; var IgnoreAskDelete, CancelDelete:
      Boolean; LookRelation: Boolean = true): Boolean; overload; virtual;
      stdcall; abstract;
    function DeleteEntity(RecNo: Integer): Boolean; overload; virtual; stdcall; abstract;
    function GetInfoId(RecNo: Integer): Integer; virtual; stdcall;
    procedure UpdateApplyLayerPropsEntity(RecNo: Integer; Entity: IlicgEntity;
      _ApplyLayerProps: Boolean; Grapher: TlicgBaseGrapher = nil); virtual; stdcall;
    function BringEntityTofront(ARecNo: Integer): Integer; dynamic; stdcall;
    function SendEntityToBack(ARecNo: Integer): Integer; dynamic; stdcall;
    function MoveEntityForward(ARecNo: Integer): Integer; dynamic; stdcall;
    function MoveEntityBackwards(ARecNo: Integer): Integer; dynamic; stdcall;
    procedure CancelScope; virtual; stdcall; abstract;
    procedure ClearBlinkers; stdcall;
    procedure Close; dynamic; stdcall;
    function ContainsDeleted: Boolean; dynamic; stdcall;
    procedure CopyRecord(SourceRecNo, DestRecNo: Integer); virtual; stdcall; abstract;
    function DefinePolygonScope(Polygon: IlicgEntity; const Scope: string;
      operator: TlicgGraphicOperator; _oList: IlicgIntegerList = nil;
      NoPickFilter: TlicgEntityIDs = []): Boolean; overload; dynamic; stdcall;
    function DefinePolygonScope(Polygon: IlicgEntity; const Scope: string;
      operator: TlicgGraphicOperator; _Sel: TlicgBaseSelection): Boolean;
      overload; dynamic; stdcall;
    function DefineScope(const Scope: string): Boolean; dynamic; stdcall;
    function DeleteLayerFiles: Boolean; dynamic; stdcall;
    procedure EndBuffering; virtual; stdcall; abstract;
    function EntityType(RecNo: Integer): TlicgEntityID; stdcall;
    function EntityWithRecNo(RecNo: Integer): IlicgEntity; virtual; stdcall;
    function Eof: Boolean; virtual; stdcall;
    function Bof: Boolean; virtual; stdcall;
    procedure StartBatchInsert; dynamic; stdcall; abstract;
    procedure CancelBatchInsert; dynamic; stdcall; abstract;
    procedure FinishBatchInsert; dynamic; stdcall; abstract;
    function InBatchInsert: Boolean; dynamic; stdcall; abstract;
    procedure First; virtual; stdcall; abstract;
    procedure ForceOpened; virtual; stdcall; abstract;
    function GetExtensionForRecords(List: IlicgIntegerList): TlicgExtent; dynamic;
      stdcall; abstract;
    procedure GetFieldList(Strings: TStrings); dynamic; stdcall; abstract;
    function HasBlinkers: Boolean; virtual; stdcall;
    procedure InitializeOnCreate(const FileName: string; AttachedDB,
      IsAnimation: Boolean; ACS: IlicgCS; FieldList: TObject); dynamic; stdcall; abstract;
    function IsCurrentLayer: Boolean; virtual; stdcall;
    procedure Last; virtual; stdcall; abstract;
    function LoadEntityWithRecNo(RecNo: Longint): IlicgEntity; virtual; stdcall;
    procedure MaxMinExtents(var AMinX, AMinY, AMaxX, AMaxY: Double); virtual; stdcall;
    procedure Next; virtual; stdcall; abstract;
    procedure Prior; virtual; stdcall; abstract;
    procedure Open; dynamic; stdcall;
    procedure Pack(ShowMessages, ShowProgress: Boolean; CallBackUpdateMes:
      TProgCallBackUpdateMessage = nil); virtual; stdcall; abstract;
    procedure PopulateFieldList(Strings: TStrings; WithAlias: Boolean = False);
      virtual; stdcall;
    procedure PopulateFieldListNoFuncFields(Strings: TStrings); virtual; stdcall;
    function QuickUpdateExtension: TlicgExtent; dynamic; stdcall; abstract;
    procedure RebuildTree; dynamic; stdcall; abstract;
    procedure Recall; dynamic; stdcall; abstract;
    function RecEntity: IlicgEntity; virtual; stdcall;
    function RecEntityID: TlicgEntityID; virtual; stdcall;
    function RecExtension: TlicgExtent; virtual; stdcall; abstract;
    function RecIsDeleted: Boolean; overload; virtual; stdcall;
    function RecIsDeleted(RecNo: Integer): Boolean; overload; virtual; stdcall;
    function RecLoadEntity: IlicgEntity; virtual; stdcall;
    procedure RecLoadEntity2(Entity: IlicgEntity); virtual; stdcall; abstract;
    function SetGraphicFilter(s: TSearchType; const visualWindow: TlicgExtent;
      LabelInfo: PlicgLayerLabelInfo = nil): Boolean; virtual; stdcall; abstract;
    procedure StartBuffering; virtual; stdcall; abstract;
    procedure Synchronize; virtual; stdcall; abstract;
    function UpdateExtension(Grapher: TlicgBaseGrapher = nil): TlicgExtent; dynamic; stdcall; abstract;
    function UpdateExtensionNotLoadEntity: TlicgExtent; virtual; stdcall; abstract;
    procedure WriteHeaders(FlushFiles: Boolean; ForceWrite: Boolean = false); virtual; stdcall; abstract;
    procedure Zap; virtual; stdcall; abstract;
    procedure SaveIfEntityVersionNumberChange; virtual; stdcall; abstract;
    procedure SetPasifEntity(RecNo: Integer); virtual; stdcall; // Parsel durumu virtual layer
    procedure SetAktifEntity(RecNo: Integer); virtual; stdcall; // Parsel durumu virtual layer

    procedure SaveToFile; virtual; stdcall; abstract;
    procedure LoadFromFile; virtual; stdcall; abstract;
    procedure SaveToStream(Stream: TStream); virtual; stdcall; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; stdcall; abstract;
    function GetLayerConnector: ILayerConnectorAndSqlInfo; virtual; stdcall; abstract;
    function ReferansLayer: TlicgBaseLayer; virtual; stdcall;
    procedure SaveInfoTableByPropList(RecNo: Integer; PropertyValueList: IPropertyValueList); virtual; stdcall; abstract;
    function GetSymBitmap: TSymBitmap; virtual; stdcall; abstract;
    procedure SetSymBitmap(Value: TSymBitmap); virtual; stdcall; abstract;
    property LayerConnector: ILayerConnectorAndSqlInfo read GetLayerConnector;
    property Active: Boolean read GetActive write SetActive;
    property Blinkers: IlicgIntegerList read GetBlinkers write SetBlinkers;
    property DBTable: TlicgBaseTable read GetDBTable write SetDBTable;
    property FileName: string read GetFileName write SetFileName;
    property Index: Integer read GetIndex;
    property LayerInfo: TlicgBaseLayerInfo read GetLayerInfo;
    property Modified: Boolean read GetModified write SetModified;
    property Name: string read GetName write SetName;
    property RecNo: Integer read GetRecNo write SetRecNo;
    property RecordCount: LongInt read GetRecordCount;
    property rt: TRTree read Getrt;
    property OnBeforePaintEntity: TlicgBeforePaintEntityEvent read GetOnBeforePaintEntity write SetOnBeforePaintEntity;
    property SpaceKeySelected: Boolean read GetSpaceKeySelected write SetSpaceKeySelected;
    property GroupName: string read GetGroupName;
    property LayerClassType: TlicgLayerClassType read GetLayerClassType write SetLayerClassType;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property SaveHeaderVersion: smallint read GetSaveHeaderVersion write SetSaveHeaderVersion;
    property GIS: TlicgBaseGIS read GetGIS write SetGIS;
    property ActiveSearchCanceledEvent: Boolean read GetActiveSearchCanceledEvent write SetActiveSearchCanceledEvent;
  end;

  TlicgLayerSortType = (lstAZ, lstZA, lstGroupAZ, lstGroupZA, lstRecordCountAZ, lstRecordCountZA, lstColorAZ, lstColorZA);

  { TlicgBaseLayers }
  TlicgBaseLayers = class
  private
    FItems: TList;
    function Get(Index: Integer): TlicgBaseLayer;
    function GetCount: Integer;
    function GetEntityCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddLayer(Layer: TlicgBaseLayer): Integer; virtual;
    function RemoveLayer(Layer: TlicgBaseLayer): Integer; virtual;
    function BringToTop(Index: Integer): Integer;
    procedure Clear; virtual;
    procedure Exchange(Index1, Index2: Integer);
    procedure ChangePosition(fromIndex, toIndex: Integer);
    function IndexOf(Layer: TlicgBaseLayer): Integer;
    function IndexOfName(const LayerName: string): Integer;
    function LayerByName(const LayerName: string): TlicgBaseLayer;
    function LayerDown(Index: Integer): Integer;
    function LayerUp(Index: Integer): Integer;
    procedure OpenLayers;
    procedure PopulateList(Strings: TStrings);
    function SendToBack(Index: Integer): Integer;
    property Count: Integer read GetCount;
    property EntityCount: Integer read GetEntityCount;
    property Items[Index: Integer]: TlicgBaseLayer read Get; default;
  end;

  { TlicgBaseMapInfo }

  TlicgBaseMapInfo = class
  private
    FGIS: TlicgBaseGis;
  protected
    function GetAerialViewLayer: string; virtual; abstract;
    function GetAreaClipped: TlicgExtent; virtual; abstract;
    function GetClipAreaKind: TlicgClipAreaKind; virtual; abstract;

    function GetCurrentLayer: string; virtual; abstract;
    function GetDrawBoxColor: TColor; virtual; abstract;
    function GetExtension: TlicgExtent; virtual; abstract;
    function GetIsAreaClipped: Boolean; virtual; abstract;
    function GetLastView: TlicgExtent; virtual; abstract;
    function GetNumLayers: Integer; virtual; abstract;
    procedure SetAerialViewLayer(const Value: string); virtual; abstract;
    procedure SetAreaClipped(const Value: TlicgExtent); virtual; abstract;
    procedure SetClipAreaKind(Value: TlicgClipAreaKind); virtual; abstract;
    procedure SetCurrentLayer(const Value: string); virtual;
    procedure SetDrawBoxColor(Value: TColor); virtual; abstract;
    procedure SetExtension(const Value: TlicgExtent); virtual; abstract;
    procedure SetIsAreaClipped(Value: Boolean); virtual; abstract;
    procedure SetLastView(const Value: TlicgExtent); virtual; abstract;
    procedure SetNumLayers(Value: Integer); virtual; abstract;
    function GetModified: Boolean; virtual; abstract;
    procedure SetModified(Value: Boolean); virtual; abstract;

    // ilker ekleme yeni
    function GetCS: IlicgCS; virtual; abstract;
    procedure SetCS(Value: IlicgCS); virtual; abstract;

    function GetOnTheFlyCS: IlicgCS; virtual; abstract;
    procedure SetOnTheFlyCS(Value: IlicgCS); virtual; abstract;
    function GetIsOnTheFly: Boolean; virtual; abstract;
    procedure SetIsOnTheFly(Value: Boolean); virtual; abstract;

    function GetImportCS: IlicgCS; virtual; abstract;
    procedure SetImportCS(Value: IlicgCS); virtual; abstract;

    function GetPenTool: IlicgPenTool; virtual; abstract;
    procedure SetPenTool(Value: IlicgPenTool); virtual; abstract;

    function GetFontTool: IlicgFontTool; virtual; abstract;
    procedure SetFontTool(Value: IlicgFontTool); virtual; abstract;
  public
    constructor Create(const AGIS: TlicgBaseGis); virtual;
    procedure Assign(Source: TlicgBaseMapInfo);
    procedure Initialize; dynamic; abstract;
    property AerialViewLayer: string read GetAerialViewLayer write SetAerialViewLayer;
    property AreaClipped:TlicgExtent read GetAreaClipped write SetAreaClipped;
    property ClipAreaKind:TlicgClipAreaKind read GetClipAreaKind write SetClipAreaKind;
    property CurrentLayer: string read GetCurrentLayer write SetCurrentLayer;
    property DrawBoxColor:TColor read GetDrawBoxColor write SetDrawBoxColor;
    property Extension: TlicgExtent read GetExtension write SetExtension;
    property IsAreaClipped:Boolean read GetIsAreaClipped write SetIsAreaClipped;
    property LastView:TlicgExtent read GetLastView write SetLastView;
    property NumLayers: Integer read GetNumLayers write SetNumLayers;
    property Modified: Boolean read GetModified write SetModified;
    property GIS: TlicgBaseGIS read FGIS;

    property CS: IlicgCS read GetCS write SetCS;
    property OnTheFlyCS: IlicgCS read GetOnTheFlyCS write SetOnTheFlyCS;
    property IsOnTheFly: Boolean read GetIsOnTheFly write SetIsOnTheFly;
    // ilker ekleme yeni
    property ImportCS: IlicgCS read GetImportCS write SetImportCS;

    property PenTool: IlicgPenTool read GetPenTool write SetPenTool;
    property FontTool: IlicgFontTool read GetFontTool write SetFontTool;
  end;

  { TlicgDrawBoxList }

  TlicgDrawBoxList = class
  private
  protected
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TlicgBaseDrawBox;
  public
    constructor Create;
    destructor destroy; override;
    procedure Add(Item: TlicgBaseDrawBox);
    procedure Delete(Index: Integer);
    function IndexOf(Item: TlicgBaseDrawBox): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TlicgBaseDrawBox read GetItem; default;
  end;

  { events definition for TlicgGIS}

  TlicgProgressStage = (epsStarting, epsMessage, epsEnding, epsUpdating);

  { the Result of a query method :
    Data : any additional info for IDentifying the caller
    Layername : the name of the layer of the entity detected
    RecNo : the record number
    Hiçbiryerde kullanılmamış ileride lazım olabilir ilker }
  {TlicgQueryResultsEvent = procedure(Sender: TObject; Data: Longint; const
    Layername: string; RecNo: Integer) of object;}

  TlicgEntityEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer; RecNo:
    Integer) of object;

  TlicgEntityAcceptEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; var Accept: Boolean) of object;

  TlicgBeforeTransformEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; Entity: IlicgEntity; TransformType: TlicgTransformType; var
    Accept: Boolean) of object;

  { Before insert an entity (can be used to filter the inserted entities) }
  TlicgBeforeInsertEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    Entity: IlicgEntity; var Accept: Boolean) of object;

  TlicgUDFCheckEvent = procedure(Sender: TObject; const LayerName, Identifier:
    string; Params: TParameterList; var DataType: TExprType; var MaxLen: Integer;
    var Accept: Boolean) of object;

  TlicgUDFSolveEvent = procedure(Sender: TObject; const Identifier: string;
    Params: TParameterList; Layer: TlicgBaseLayer; RecNo: Integer; var Value:
    string) of object;

  TlicgStartExternalPopulateEvent = procedure(Sender: TObject; const LayerName:
    string; var Accept: Boolean) of object;

  TlicgExternalPopulateEvent = procedure(Sender: TObject; const LayerName:
    string; var Identifier: string) of object;

  TlicgEndExternalPopulateEvent = procedure(Sender: TObject; const LayerName:
    string) of object;

  TlicgIsControlPointSelectableEvent = procedure(Sender: TObject; Entity:
    IlicgEntity; Layer: TlicgBaseLayer; RecNo: Integer; Index: Integer;
    ControlPointType: TlicgControlPointType; var Accept: Boolean) of object;

  TlicgControlPointChangedEvent = procedure(Sender: TObject; Entity: IlicgEntity;
    Layer: TlicgBaseLayer; RecNo: Integer; Index: Integer; ControlPointType:
    TlicgControlPointType; const NewPosition: TlicgCoor) of object;

  TlicgBeforePaintLayerEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    Grapher: TlicgBaseGrapher; var CanShow: Boolean; var WasFiltered: Boolean;
    var EntList: IlicgEntityList; var Autofree: Boolean) of object;

  TlicgAfterPaintLayerEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    Grapher: TlicgBaseGrapher; var EntList: IlicgEntityList; var Autofree:
    Boolean) of object;

  TlicgPaintFilteredEntities = procedure(AGIS: TlicgBaseGis; aLayer:
    TlicgBaseLayer; Grapher: TlicgBaseGrapher) of object;

  TlicgBeforePaintLayerThematicEvent = procedure(Sender: TObject; Layer:
    TlicgBaseLayer) of object;

  TlicgAfterPaintLayerThematicEvent = procedure(Sender: TObject; Layer:
    TlicgBaseLayer) of object;

  TlicgBeforeSymbolPaintEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; const Clip:
    TlicgExtent; Style: IlicgSymbolTool) of object;

  TlicgBeforeBrushPaintEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; const Clip:
    TlicgExtent; Style: IlicgBrushTool) of object;

  TlicgBeforePenPaintEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; const Clip:
    TlicgExtent; Style: IlicgPenTool) of object;

  TlicgBeforeFontPaintEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; const Clip:
    TlicgExtent; Style: IlicgFontTool) of object;

  { show progress status for internal map actions }
  TlicgProgressEvent = procedure(Sender: TObject; Stage: TlicgProgressStage;
    const Caption: string; Min, Max, Position: Integer; _ProgressHandle: THandle)
    of object; stdcall;

  TlicgBeforeLayerEvent = procedure(Sender: TObject; const LayerName: string;
    var Proceed: Boolean) of object;

  TlicgLayerEvent = procedure(Sender: TObject; const LayerName: string) of object;

  TlicgPrintProgressEvent = procedure(Sender: TObject; Percent: Integer; var
    Cancel: Boolean) of object;

  TlicgErrorSource = (esUndefined, esImporting, esLayerNotFound);

  TlicgErrorEvent = procedure(Sender: TObject; const Msg: string; ErrorSource:
    TlicgErrorSource; var CanContinue: Boolean) of object;

  TlicgGisTimerEvent = procedure(Sender: TObject; var CancelPainting: Boolean)
    of object; stdcall;

  { TlicgBaseGISItem }
  TApppyRefrenceMapInfo = (rmMain, rmSelf, rmRef);

  // aşağıdaki yapının, GIS bir referans ise bir anlamı var.
  TReferenceInfoSettings = record
    FDisplayName: string;
    FVisible: Boolean;
    FSnapable: Boolean;
    FCategory: string;
    FDate: string;
    FDescription: string;
  end;

  TlicgGISItem = class(TCollectionItem)
  private
    FGIS: TlicgBaseGIS;
    FReferenceInfo: TReferenceInfoSettings;
    function GetGIS: TlicgBaseGIS;
    procedure SetGIS(const Value: TlicgBaseGIS);
    function GetCaption: string;
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    function GetSnapable: Boolean;
    procedure SetSnapable(Value: Boolean);
    function GetCategory: string;
    procedure SetCategory(Value: string);
    function GetDate: string;
    procedure SetDate(Value: string);
    function GetDescription: string;
    procedure SetDescription(Value: string);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(Value: string);
  public
    ApppyRefrenceMapInfo: TApppyRefrenceMapInfo;
    procedure Assign(Source: TPersistent); override;
  published
    property GIS: TlicgBaseGIS read GetGIS write SetGIS;
    property Visible: Boolean read GetVisible write SetVisible;
    property Snapable: Boolean read GetSnapable write SetSnapable;
    property Category: string read GetCategory write SetCategory;
    property Displayname: string read GetDisplayName write SetDisplayname;
    property Date: string read GetDate write SetDate;
    property Description: string read GetDescription write SetDescription;
  end;

  { TlicgGISList }
  TlicgGISList = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TlicgGISItem;
    procedure SetItem(Index: Integer; Value: TlicgGISItem);
  public
    ShowReferences: Boolean;
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function Add: TlicgGISItem;
    property Items[Index: Integer]: TlicgGISItem read GetItem write SetItem; default;
  end;

  (*
  TPickClass = class
    GIS       : TlicgBaseGis;
    LayerName : string;
    RecNo     : Integer;
    PickedPnt : Integer;
    Function Layer  : TlicgBaseLayer;
    Function Entity : IlicgEntity;
  end;

  TPickItem = class(TCollectionItem)
  private
    FPick : TPickClass;
    function  GetPick: TPickClass;
    procedure SetPick (const Value: TPickClass);
  public
    Destructor Destroy; override;
    property Pick : TPickClass read GetPick write SetPick;
  end;

  { TPickList }
  TPickList = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPickItem;
    procedure SetItem(Index: Integer; Value: TPickItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TPickItem;
    property Items[Index: Integer]: TPickItem read GetItem write SetItem; default;
  end;  *)

  TPickedClass = class(TInterfacedObject, IPickedClass)
  private
    FGIS: TlicgBaseGis;
    fLayerName: string;
    fRecNo: Integer;
    fPickedPnt: Integer;
  public
    function GetGis: TlicgBaseGis; stdcall;
    procedure SetGis(Value: TlicgBaseGis); stdcall;
    function GetLayerName: string; stdcall;
    procedure SetLayerName(Value: string); stdcall;
    function GetRecNo: Integer; stdcall;
    procedure SetRecNo(Value: Integer); stdcall;
    function GetPickedPnt: Integer; stdcall;
    procedure SetPickedPnt(Value: Integer); stdcall;
    function Pick: IPickedClass; stdcall;
    function Layer: TlicgBaseLayer; stdcall;
    function Entity: IlicgEntity; stdcall;
  end;

  (*
  TlicgaseGdalRaster = class

      function  GetGroupCation  : string; virtual; abstract;
      procedure SetGroupCation  (Value: string); virtual; abstract;


      function  GetFileName  : string; virtual; abstract;
      procedure SetFileName  (Value: string); virtual; abstract;

      function  GetRasterHandle : GDAL_RasterHandle;virtual; abstract;
      procedure SetRasterHandle (Value: GDAL_RasterHandle);virtual; abstract;

      function  GetRasterInfo : Gdal_RasterInfo; virtual; abstract;
      function  GetVisible : Boolean; virtual; abstract;
      procedure SetVisible (Value: Boolean); virtual; abstract;
      function  GetFlag : Integer; virtual; abstract;
      procedure SetFlag (Value: Integer); virtual; abstract;

      function  GetContrast : Integer; virtual; abstract;
      procedure SetContrast (Value: Integer); virtual; abstract;
      function  GetBright : Integer; virtual; abstract;
      procedure SetBright (Value : Integer); virtual; abstract;
      function  GetColorfulness : Integer; virtual; abstract;
      procedure SetColorfulness (Value: Integer); virtual; abstract;
      function  GetRedGreenTint : Integer; virtual; abstract;
      procedure SetRedGreenTint (Value: Integer); virtual; abstract;
      function  GetGammaRed : Integer; virtual; abstract;
      procedure SetGammaRed (Value: Integer); virtual; abstract;
      function  GetGammaGreen : Integer; virtual; abstract;
      procedure SetGammaGreen (Value: Integer); virtual; abstract;
      function  GetGammaBlue : Integer; virtual; abstract;
      procedure SetGammaBlue (Value : Integer); virtual; abstract;
      function  GetRefBlack : Integer; virtual; abstract;
      procedure SetRefBlack (Value: Integer); virtual; abstract;
      function  GetRefWhite : Integer; virtual; abstract;
      procedure SetRefWhite (Value: Integer); virtual; abstract;
      function  GetConvertedGray  : Boolean; virtual; abstract;// renkli imajlar için
      procedure SetConvertedGray  (Value: Boolean); virtual; abstract;// renkli imajlar için
      function  GetClipped        : Boolean;virtual; abstract;
      procedure SetClipped(Value: Boolean);virtual; abstract;
      Function  GetClipArea       : Gdal_GroundControlRectangle;virtual; abstract;
      procedure SetClipArea (Value: Gdal_GroundControlRectangle);virtual; abstract;
      function  GetClipScale      : double; virtual; abstract;
      procedure SetClipScale(Value: double); virtual; abstract;
      function  Extension     : TlicgExtent; virtual; abstract;

      property  FileName      : string read GetFileName write SetFileName;

      property  RasterHandle : GDAL_RasterHandle read GetRasterHandle write SetRasterHandle;

      property  Visible       : Boolean read GetVisible write SetVisible;
      property  Flag : Integer  read GetFlag write SetFlag ;

      property  Contrast : Integer  read GetContrast write SetContrast ;
      property  Bright : Integer read GetBright write SetBright;
      property  Colorfulness : Integer read GetColorfulness write SetColorfulness ;
      property  RedGreenTint : Integer read GetRedGreenTint write SetRedGreenTint;
      property  GammaRed : Integer read GetGammaRed write SetGammaRed;
      property  GammaGreen : Integer read GetGammaGreen write SetGammaGreen;
      property  GammaBlue : Integer read GetGammaBlue write SetGammaBlue;
      property  RefBlack : Integer read GetRefBlack write SetRefBlack;
      property  RefWhite : Integer read GetRefWhite write SetRefWhite;
      property  ConvertedGray  : Boolean read GetConvertedGray write SetConvertedGray;
      property  Clipped        : Boolean read GetClipped write SetClipped ;
      property  ClipArea       : Gdal_GroundControlRectangle read GetClipArea write SetClipArea;
      property  ClipScale      : double read GetClipScale write SetClipScale;

      property GroupName   : string read GetGroupCation write SetGroupCation;

   end;

  TRasterGroup = class
     GroupName : string;
     RasterList : TList;
     constructor Create;
     destructor destroy; override;
  end;

  TRasterGroupList = class(TList)
     function GetIndex(gc: string) : Integer;
     function Get(gc: string) : TRasterGroup;
  end;

  TlicgaseRasterList = class
     function DeleteGroup(cap : string): Boolean ; virtual; abstract;
     function  AddRaster   (fn: string; isMemoryLoad : Boolean) : TlicgaseGdalRaster; virtual; abstract;
     procedure RemoveRaster(fn: string);virtual; abstract;
     function isExistsRaster(fn: string) : Boolean; virtual; abstract;
     procedure ClipRaster  (fn: string;const aWorldBoundsRect: TlicgExtent; scaleFactor: Double); virtual; abstract;
     procedure ClipCancel  (fn: string); virtual; abstract;
     function  GetRaster   (fn: string): TlicgaseGdalRaster; virtual; abstract;
     function  GetIndex    (fn: string): Integer; virtual; abstract;
     function  GetRasterFromIndex   (index : Integer): TlicgaseGdalRaster; virtual; abstract;
     procedure Exchange(index1, index2 : Integer); virtual; abstract;
     procedure Clear; virtual; abstract;
     function  GetCancelTag : Integer ; virtual; abstract;
     procedure SetCancelTag (Value : Integer); virtual; abstract;
     procedure ShowRasterEditor; virtual; abstract;
     procedure ShowRasterList; virtual; abstract;

     procedure SaveToFile  (Inifn: string); virtual; abstract;
     procedure OpenFromFile(Inifn: string); virtual; abstract;

     procedure DrawRasters (VisualExtent: TlicgExtent; ClipRgn: HRgn; Canvas: IlicgCanvas;
        Grapher: TObject; GIS: TObject; CurrentScale: double; var CancelDraw : Boolean;
        PMatrix : PBMatrix = nil);  virtual; abstract;

     procedure UpdateExtension(var R: TlicgExtent); virtual; abstract;

     function  CSCoorToCSCoor( aXIn: Double;  aYIn: Double; var aXOut: Double; var aYOut: Double;
                              const aProjIn:  Pointer; const aProjOut: Pointer;
                              ProjectApplyMatrix : PAffineMatrixElements; isAfter : Boolean = true): Boolean;virtual; abstract;

     function  ExecuteSelect (const inPrj: Pointer;  outPj: Pointer;Params : Pointer): Boolean;virtual; abstract;
     procedure GetPrjDescription (const inPrj: Pointer; params : Pointer); virtual; abstract;
     function  RasterCount : Integer; virtual; abstract;
     function  RasterName (index: Integer): string; virtual; abstract;

     function  GetExtension (const RasterName : string): TlicgExtent;virtual; abstract;
     function  GetVisible   (const RasterName : string):Boolean;virtual; abstract;
     procedure SetVisible   (const RasterName : string; _Visible: Boolean);virtual; abstract;

     procedure SetProjection(Prj: Pointer);virtual; abstract;

     function  GetProjEPSGCode (Prj: Pointer) : Integer;virtual; abstract;
     function  GetProjCRSCode  (Prj: Pointer) : Integer;virtual; abstract;
     function  GetProjAutoCode (Prj: Pointer) : Integer;virtual; abstract;

     function  Gdal_GetFromESRI (PrjText : PChar ) : PIlicgCoordinatSystem; virtual; abstract;
     function  RasterProjection: Pointer;virtual; abstract;

     function  RasterGroupList : TRasterGroupList;   virtual; abstract;

     function  CompareMemPrjX(Proj1,Proj2: IlicgCoordinatSystem) : Boolean; virtual; abstract;

     property  CancelTag : Integer read GetCancelTag write SetCancelTag;
     property  Rasters[index : Integer] : TlicgaseGdalRaster read GetRasterFromIndex;

  end;
  *)

  TSPClass1 = class
    Name: string;
    DirL: TStrings;
    EL: TlicgEntitylist;
    TotalCost: double;
    StartPoint: TlicgCoor;
    constructor create(_startPoint: TlicgCoor; _Name: string; _DirL: TStrings;
      _EL: TlicgEntitylist; _TotalCosts: IlicgDoubleList);
    destructor Destroy; override;
  end;

  TShortPathInterface = class
  protected
    function GetDirections: TStrings; virtual; abstract;
  public
    function FindShortPath(PickedPoints: IlicgVector; StopPoint: TlicgCoor):
      TlicgEntitylist; overload; virtual; abstract;
    function FindShortPath(SearchLayerName: string; StopPoint: TlicgCoor):
      TSPClass1; overload; virtual; abstract;
    property Directions: TStrings read GetDirections;
  end;

  IQuerySelectTool = interface
    ['{1556B933-FE9C-4931-9A95-13C38FE4272D}']
    procedure QueryExpression(const LayerName, Expression: string; List:
      IlicgIntegerList; ClearBefore: Boolean; NoPickFilter: TlicgEntityIDs = []);
      overload; stdcall;
    procedure QueryBuffer(Polyline: IlicgEntity; const Layername,
      QueryExpression: string; operator: TlicgGraphicOperator; CurvePoints:
      Integer; const Distance: Double; List: IlicgIntegerList; ClearBefore:
      Boolean; NoPickFilter: TlicgEntityIDs = []); overload; stdcall;
    procedure QueryPolygon(Polygon: IlicgEntity; const Layername,
      QueryExpression: string; operator: TlicgGraphicOperator; List:
      IlicgIntegerList; ClearBefore: Boolean; NoPickFilter: TlicgEntityIDs = []);
      overload; stdcall;
    procedure QueryPolygon(Polygon: IlicgEntity; const Layername,
      QueryExpression: string; operator: TlicgGraphicOperator; sel:
      TlicgBaseSelection; ClearBefore: Boolean; NoPickFilter: TlicgEntityIDs = []);
      overload; stdcall;
    procedure QueryPolygon(Polygon: IlicgEntity; const Layer: TlicgBaseLayer;
      QueryExpression: string; operator: TlicgGraphicOperator; List:
      IlicgIntegerList; ClearBefore: Boolean; NoPickFilter: TlicgEntityIDs = []);
      overload; stdcall;
    procedure QueryRectangle(const AxMin, AyMin, AxMax, AyMax: double; const
      Layername, QueryExpression: string; operator: TlicgGraphicOperator; List:
      IlicgIntegerList; ClearBefore: Boolean; NoPickFilter: TlicgEntityIDs = []);
      overload; stdcall;
    procedure QueryRectangle(const AxMin, AyMin, AxMax, AyMax: double; const
      Layer: TlicgBaseLayer; QueryExpression: string; operator:
      TlicgGraphicOperator; List: IlicgIntegerList; ClearBefore: Boolean;
      NoPickFilter: TlicgEntityIDs = []); overload; stdcall;
    procedure QueryExpression(const LayerName, Expression: string; sel:
      TlicgBaseSelection; ClearBefore: Boolean; NoPickFilter: TlicgEntityIDs = []);
      overload; stdcall;
    procedure QueryBuffer(Polyline: IlicgEntity; const Layername,
      QueryExpression: string; operator: TlicgGraphicOperator; CurvePoints:
      Integer; const Distance: Double; sel: TlicgBaseSelection; ClearBefore:
      Boolean; NoPickFilter: TlicgEntityIDs = []); overload; stdcall;
    procedure QueryRectangle(const AxMin, AyMin, AxMax, AyMax: double; const
      Layername, QueryExpression: string; operator: TlicgGraphicOperator; sel:
      TlicgBaseSelection; ClearBefore: Boolean; NoPickFilter: TlicgEntityIDs = []);
      overload; stdcall;
    function IsQueryPolygon(Polygon: IlicgEntity; const Layername,
      QueryExpression: string; operator: TlicgGraphicOperator; EntityFilter:
      TlicgEntityIDs): Boolean; stdcall;
    function IsQueryRectangle(const AxMin, AyMin, AxMax, AyMax: double; const
      Layername, QueryExpression: string; operator: TlicgGraphicOperator;
      EntityFilter: TlicgEntityIDs): Boolean; stdcall;
    procedure PolygonSelect(Polygon: IlicgEntity; const Layername,
      QueryExpression: string; operator: TlicgGraphicOperator; _Selection:
      TlicgBaseSelection); stdcall;
    procedure PolylineSelect(Polyline: IlicgEntity; const Layername,
      QueryExpression: string; _Selection: TlicgBaseSelection); stdcall;
    procedure RectangleSelect(const AxMin, AyMin, AxMax, AyMax: double; const
      Layername, QueryExpression: string; operator: TlicgGraphicOperator;
      _Selection: TlicgBaseSelection); stdcall;
    function DoPolygonSelect(Polygon: IlicgEntity; ClearBefore: Boolean;
      _Selection: TlicgBaseSelection; selectQueryExp: string; InverseSelection:
      Boolean; NoPickFilter: TlicgEntityIDs; GraphicOp: TlicgGraphicOperator =
      goWithin): Boolean; overload; stdcall;
    function DoPolygonSelect(const LayerList: TList; Polygon: IlicgEntity;
      ClearBefore: Boolean; _Selection: TlicgBaseSelection; selectQueryExp:
      string; InverseSelection: Boolean; NoPickFilter: TlicgEntityIDs; GraphicOp:
      TlicgGraphicOperator = goWithin): Boolean; overload; stdcall;
    function DoPolygonSelect(const LayerName: string; Polygon: IlicgEntity;
      ClearBefore: Boolean; _Selection: TlicgBaseSelection; selectQueryExp:
      string; InverseSelection: Boolean; NoPickFilter: TlicgEntityIDs; GraphicOp:
      TlicgGraphicOperator = goWithin): Boolean; overload; stdcall;
    function SelectInFrameOp(Frame: TlicgExtent; ClearBefore: Boolean; var SelRect:
      TlicgExtent; _Selection: TlicgBaseSelection; selectQueryExp: string;
      InverseSelection: Boolean; NoPickFilter: TlicgEntityIDs; GraphicOp:
      TlicgGraphicOperator = goWithin): Boolean; overload; stdcall;
    function SelectInFrameOp(const LayerList: TList; Frame: TlicgExtent; ClearBefore:
      Boolean; var SelRect: TlicgExtent; _Selection: TlicgBaseSelection;
      selectQueryExp: string; InverseSelection: Boolean; NoPickFilter:
      TlicgEntityIDs; GraphicOp: TlicgGraphicOperator = goWithin): Boolean;
      overload; stdcall;
    function SelectInFrameOp(const LayerName: string; Frame: TlicgExtent;
      ClearBefore: Boolean; var SelRect: TlicgExtent; _Selection: TlicgBaseSelection;
      selectQueryExp: string; InverseSelection: Boolean; NoPickFilter:
      TlicgEntityIDs; GraphicOp: TlicgGraphicOperator = goWithin): Boolean;
      overload; stdcall;
  end;

  THintCommandMessageEvent = function(CmdHint: string): string of object;

  { TlicgBaseGIS }

  TlicgBaseGIS = class(TObject)
  private
    FTempParamString: string;
    FOnFillLayersToListbox: TNotifyEvent;
    FHintCommandMessageEvent: THintCommandMessageEvent;
    FOnFarFillLayers: TNotifyEvent;
    FOnCurrentLayerChange: TlicgLayerEvent;
    FOnlySelectionExport: Boolean;
    FThematicList: TList;
    FTextureFillLibs: TlicgTextureFillLibraries;
    FReserved: array[0..20] of Byte;
    FUseReservedData: Boolean;
    FThicknessWidth: Integer;
    FGuideIsolineInterval: Double;
  private
    function Get_Reserved_GuideIsoline: Boolean; // 19;
    function Get_Reserved_Thickness: Boolean; // 18;
    function Get_Reserved_PointKod: Boolean;  // 17
    function Get_Reserved_LineWidthDirection: Boolean; // 16;
    function Get_Reserved_ApplyBufferForPolyline: Boolean; // 15;
    function Get_Reserved_NodeVisible: Boolean; // 14;
    function Get_Reserved_Labeling: Boolean; // 13;
    function Get_Reserved_ZDisplay: Boolean; // 10;
    function Get_Reserved_ShowArrow: Boolean; // 8;
    function Get_Reserved_LayerBrush: Boolean; // 7;
    function Get_Reserved_Print: Boolean; // 3;
    function Get_Reserved_PolygonName: Boolean; // 2;
    function Get_Reserved_PointName: Boolean; // 1;
    function Get_Reserved_Balastro: Boolean; //0;

    procedure Set_Reserved_GuideIsoline(Value: Boolean); // 19;
    procedure Set_Reserved_Thickness(Value: Boolean); // 18;
    procedure Set_Reserved_PointKod(Value: Boolean); // 17
    procedure Set_Reserved_LineWidthDirection(Value: Boolean); // 16;
    procedure Set_Reserved_ApplyBufferForPolyline(Value: Boolean); // 15;
    procedure Set_Reserved_NodeVisible(Value: Boolean); // 14;
    procedure Set_Reserved_Labeling(Value: Boolean); // 13;
    procedure Set_Reserved_ZDisplay(Value: Boolean); // 10;
    procedure Set_Reserved_ShowArrow(Value: Boolean); // 8;
    procedure Set_Reserved_LayerBrush(Value: Boolean); // 7;
    procedure Set_Reserved_Print(Value: Boolean); // 3;
    procedure Set_Reserved_PolygonName(Value: Boolean); // 2;
    procedure Set_Reserved_PointName(Value: Boolean); // 1;
    procedure Set_Reserved_Balastro(Value: Boolean);

    function GetThicknessWidth: Integer;
    procedure SetThicknessWidth(const Value: Integer);

    function GetGuideIsolineInterval: Double;
    procedure SetGuideIsolineInterval(const Value: Double);

  protected

    //function GetDatabaseModulType: TDatabaseModulType; virtual;

    function GetTempEntities: IlicgEntityList; virtual; abstract;
    procedure SetBeforePaintLayerThematicEvent(Value:
      TlicgBeforePaintLayerThematicEvent); virtual; abstract;
    function GetBeforePaintLayerThematicEvent:
      TlicgBeforePaintLayerThematicEvent; virtual; abstract;
    procedure SetAfterPaintLayerThematicEvent(Value:
      TlicgAfterPaintLayerThematicEvent); virtual; abstract;
    function GetAfterPaintLayerThematicEvent: TlicgAfterPaintLayerThematicEvent; virtual; abstract;
    function GetOnSelectableLayerChange: TlicgLayerEvent; virtual; abstract;
    procedure SetOnSelectableLayerChange(Value: TlicgLayerEvent); virtual; abstract;
    function GetOnVisibleLayerChange: TlicgLayerEvent; virtual; abstract;
    procedure SetOnVisibleLayerChange(Value: TlicgLayerEvent); virtual; abstract;
    function GetOnBeforeInsertLayer: TlicgBeforeLayerEvent; virtual; abstract;
    procedure SetOnBeforeInsertLayer(Value: TlicgBeforeLayerEvent); virtual; abstract;
    function GetOnAfterInsertLayer: TlicgLayerEvent; virtual; abstract;
    procedure SetOnAfterInsertLayer(Value: TlicgLayerEvent); virtual; abstract;
    function GetOnAfterDeleteLayer: TlicgLayerEvent; virtual; abstract;
    procedure SetOnAfterDeleteLayer(Value: TlicgLayerEvent); virtual; abstract;
    function GetOnBeforeDeleteLayer: TlicgBeforeLayerEvent; virtual; abstract;
    procedure SetOnBeforeDeleteLayer(Value: TlicgBeforeLayerEvent); virtual; abstract;
    function GetOnCurrentLayerChange: TlicgLayerEvent; virtual;
    procedure SetOnCurrentLayerChange(Value: TlicgLayerEvent); virtual;
    function GetCurrentLayer: TlicgBaseLayer; virtual; abstract;
    function GetCurrentLayerName: string; virtual; abstract;
    procedure SetActive(Value: Boolean); virtual; abstract;
    procedure SetCurrentLayerName(const Value: string); virtual; abstract;
    procedure SetLayers(Value: TlicgBaseLayers); virtual; abstract;
    procedure SetModified(Value: Boolean); virtual; abstract;
    function GetActive: Boolean; virtual; abstract;
    function GetAutoSetLastView: Boolean; virtual; abstract;
    function GetClippedEntities: TlicgBaseSelection; virtual; abstract;
    function GetClipPolygonalArea: IlicgVector; virtual; abstract;
    function GetDrawBoxList: TlicgDrawBoxList; virtual; abstract;
    function GetFileName: string; virtual; abstract;
    function GetHGuidelines: IlicgDoubleList; virtual; abstract;
    function GetLayers: TlicgBaseLayers; virtual; abstract;
    function GetMapInfo: TlicgBaseMapInfo; virtual; abstract;
    function GetRefrenceMapInfo: TlicgBaseMapInfo; virtual; abstract;
    function GetApppyRefrenceMapInfo: Boolean; virtual; abstract;
    procedure SetApppyRefrenceMapInfo(Value: Boolean); virtual; abstract;
    function GetModified: Boolean; virtual; abstract;
    function GetPrintTimerFrequency: Integer; virtual; abstract;
    function GetReadOnly: Boolean; virtual; abstract;
    function GetShowGuidelines: Boolean; virtual; abstract;
    function GetShowHourglass: Boolean; virtual; abstract;
    function GetTimerFrequency: Integer; virtual; abstract;
    function GetVGuidelines: IlicgDoubleList; virtual; abstract;
    procedure SetAutoSetLastView(const Value: Boolean); virtual; abstract;
    procedure SetPrintTimerFrequency(const Value: Integer); virtual; abstract;
    procedure SetReadOnly(const Value: Boolean); virtual; abstract;
    procedure SetShowGuidelines(const Value: Boolean); virtual; abstract;
    procedure SetShowHourglass(const Value: Boolean); virtual; abstract;
    procedure SetTimerFrequency(const Value: Integer); virtual; abstract;
    procedure SetFileName(const Value: string); virtual; abstract;
    function GetOnEntityChanged: TlicgEntityEvent; virtual; abstract;
    procedure SetOnEntityChanged(Value: TlicgEntityEvent); virtual; abstract;
    function GetOnBeforeDeleteEntity: TlicgEntityAcceptEvent; virtual; abstract;
    procedure SetOnBeforeDeleteEntity(Value: TlicgEntityAcceptEvent); virtual; abstract;
    function GetOnAfterDeleteEntity: TlicgEntityEvent; virtual; abstract;
    procedure SetOnAfterDeleteEntity(Value: TlicgEntityEvent); virtual; abstract;
    function GetOnBeforeInsertEntity: TlicgBeforeInsertEvent; virtual; abstract;
    procedure SetOnBeforeInsertEntity(Value: TlicgBeforeInsertEvent); virtual; abstract;
    function GetOnAfterInsertEntity: TlicgEntityEvent; virtual; abstract;
    procedure SetOnAfterInsertEntity(Value: TlicgEntityEvent); virtual; abstract;
    function GetOnAfterDragDrop: TlicgEntityEvent; virtual; abstract;
    procedure SetOnAfterDragDrop(Value: TlicgEntityEvent); virtual; abstract;
    function GetOnAfterPaintEntity: TlicgAfterPaintEntityEvent; virtual; abstract;
    procedure SetOnAfterPaintEntity(Value: TlicgAfterPaintEntityEvent); virtual; abstract;
    function GetOnAfterPaintLayer: TlicgAfterPaintLayerEvent; virtual; abstract;
    procedure SetOnAfterPaintLayer(Value: TlicgAfterPaintLayerEvent); virtual; abstract;
    function GetOnAfterPaintLayers: TNotifyEvent; virtual; abstract;
    procedure SetOnAfterPaintLayers(Value: TNotifyEvent); virtual; abstract;
    function GetOnAfterTransform: TlicgEntityEvent; virtual; abstract;
    procedure SetOnAfterTransform(Value: TlicgEntityEvent); virtual; abstract;
    function GetOnBeforeBrushPaint: TlicgBeforeBrushPaintEvent; virtual; abstract;
    procedure SetOnBeforeBrushPaint(Value: TlicgBeforeBrushPaintEvent); virtual; abstract;
    function GetOnBeforeClose: TNotifyEvent; virtual; abstract;
    procedure SetOnBeforeClose(Value: TNotifyEvent); virtual; abstract;
    function GetOnBeforeDragDrop: TlicgEntityAcceptEvent; virtual; abstract;
    procedure SetOnBeforeDragDrop(Value: TlicgEntityAcceptEvent); virtual; abstract;
    function GetOnBeforeFontPaint: TlicgBeforeFontPaintEvent; virtual; abstract;
    procedure SetOnBeforeFontPaint(Value: TlicgBeforeFontPaintEvent); virtual; abstract;
    function GetOnBeforeOpen: TNotifyEvent; virtual; abstract;
    procedure SetOnBeforeOpen(Value: TNotifyEvent); virtual; abstract;
    function GetOnBeforePaintEntity: TlicgBeforePaintEntityEvent; virtual; abstract;
    procedure SetOnBeforePaintEntity(Value: TlicgBeforePaintEntityEvent); virtual; abstract;
    function GetOnBeforePaintLayer: TlicgBeforePaintLayerEvent; virtual; abstract;
    procedure SetOnBeforePaintLayer(Value: TlicgBeforePaintLayerEvent); virtual; abstract;
    function GetOnBeforePaintLayers: TNotifyEvent; virtual; abstract;
    procedure SetOnBeforePaintLayers(Value: TNotifyEvent); virtual; abstract;
    function GetOnBeforePenPaint: TlicgBeforePenPaintEvent; virtual; abstract;
    procedure SetOnBeforePenPaint(Value: TlicgBeforePenPaintEvent); virtual; abstract;
    function GetOnBeforeSymbolPaint: TlicgBeforeSymbolPaintEvent; virtual; abstract;
    procedure SetOnBeforeSymbolPaint(Value: TlicgBeforeSymbolPaintEvent); virtual; abstract;
    function GetOnBeforeTransform: TlicgBeforeTransformEvent; virtual; abstract;
    procedure SetOnBeforeTransform(Value: TlicgBeforeTransformEvent); virtual; abstract;
    function GetOnClippedAreaChanged: TNotifyEvent; virtual; abstract;
    procedure SetOnClippedAreaChanged(Value: TNotifyEvent); virtual; abstract;
    function GetOnEndExternalPopulate: TlicgEndExternalPopulateEvent; virtual; abstract;
    procedure SetOnEndExternalPopulate(Value: TlicgEndExternalPopulateEvent); virtual; abstract;
    function GetOnError: TlicgErrorEvent; virtual; abstract;
    procedure SetOnError(Value: TlicgErrorEvent); virtual; abstract;
    function GetOnExternalPopulate: TlicgExternalPopulateEvent; virtual; abstract;
    procedure SetOnExternalPopulate(Value: TlicgExternalPopulateEvent); virtual; abstract;
    function GetOnFileNameChange: TNotifyEvent; virtual; abstract;
    procedure SetOnFileNameChange(Value: TNotifyEvent); virtual; abstract;
    function GetOnGisTimer: TlicgGisTimerEvent; virtual; abstract;
    procedure SetOnGisTimer(Value: TlicgGisTimerEvent); virtual; abstract;
    function GetOnIsControlPointSelectable: TlicgIsControlPointSelectableEvent; virtual; abstract;
    procedure SetOnIsControlPointSelectable(Value: TlicgIsControlPointSelectableEvent); virtual; abstract;
    function GetOnControlPointChanged: TlicgControlPointChangedEvent; virtual; abstract;
    procedure SetOnControlPointChanged(Value: TlicgControlPointChangedEvent); virtual; abstract;
    function GetOnModified: TNotifyEvent; virtual; abstract;
    procedure SetOnModified(Value: TNotifyEvent); virtual; abstract;
    function GetOnPrintBegin: TNotifyEvent; virtual; abstract;
    procedure SetOnPrintBegin(Value: TNotifyEvent); virtual; abstract;
    function GetOnPrintEnd: TNotifyEvent; virtual; abstract;
    procedure SetOnPrintEnd(Value: TNotifyEvent); virtual; abstract;
    function GetOnPrintProgress: TlicgPrintProgressEvent; virtual; abstract;
    procedure SetOnPrintProgress(Value: TlicgPrintProgressEvent); virtual; abstract;
    function GetOnProgress: TlicgProgressEvent; virtual; abstract;
    procedure SetOnProgress(Value: TlicgProgressEvent); virtual; abstract;
    function GetOnStartExternalPopulate: TlicgStartExternalPopulateEvent; virtual; abstract;
    procedure SetOnStartExternalPopulate(Value: TlicgStartExternalPopulateEvent); virtual; abstract;
    function GetOnUDFCheck: TlicgUDFCheckEvent; virtual; abstract;
    procedure SetOnUDFCheck(Value: TlicgUDFCheckEvent); virtual; abstract;
    function GetOnUDFSolve: TlicgUDFSolveEvent; virtual; abstract;
    procedure SetOnUDFSolve(Value: TlicgUDFSolveEvent); virtual; abstract;
    function GetOnBeforeOpenLayer: TlicgLayerEvent; virtual; abstract;
    procedure SetOnBeforeOpenLayer(Value: TlicgLayerEvent); virtual; abstract;
    function GetOnBeforeCloseLayer: TlicgLayerEvent; virtual; abstract;
    procedure SetOnBeforeCloseLayer(Value: TlicgLayerEvent); virtual; abstract;
    function GetLastPointName: string; virtual; abstract;
    procedure SetLastPointName(Value: string); virtual; abstract;
    function GetLastPolygonName: string; virtual; abstract;
    procedure SetLastPolygonName(Value: string); virtual; abstract;
    function GetPointNameDisplay: Boolean; virtual; abstract;
    procedure SetPointNameDisplay(Value: Boolean); virtual; abstract;
    //function  GetLastPointColor : TColor ;virtual; abstract;
    //procedure SetLastPointColor(Value:TColor );virtual; abstract;

    //function GetTopology: IlicgTopology; virtual; abstract;
    //procedure SetTopology(Value: IlicgTopology); virtual; abstract;

    function GetProjectScale: Double; virtual; abstract;
    procedure SetProjectScale(Value: Double); virtual; abstract;
    function GetOnTheFlyMatrixElements: TAffineMatrixElements; virtual; abstract;
    procedure SetOnTheFlyMatrixElements(Value: TAffineMatrixElements); virtual; abstract;
    function GetInProcess: Boolean; virtual; abstract;
    procedure SetInProcess(Value: Boolean); virtual; abstract;
    function GetBaseTableClass: TlicgBaseTableClass; virtual; abstract;
    procedure SetBaseTableClass(Value: TlicgBaseTableClass); virtual; abstract;
    //function  GetRasterList  : TlicgaseRasterList ;virtual; abstract;
    //procedure SetRasterList  (Value: TlicgaseRasterList);virtual; abstract;
    procedure SetMapInfo(Value: TlicgBaseMapInfo); virtual; abstract;
    function GetDesktopLayerType: TDesktopLayerType; virtual; abstract;
    procedure SetDesktopLayerType(Value: TDesktopLayerType); virtual; abstract;
    function GetCurrentScale: double; virtual; abstract;
    procedure SetCurrentScale(Value: double); virtual; abstract;
    //function  GetDBConnectionName : String; virtual; abstract;
    //procedure SetDBConnectionName (Value : String) ; virtual; abstract;
    function GetOnRefOpenProjEvent: TReferenceGisEvent_OpenProject; virtual; abstract;
    procedure SetOnRefOpenProjEvent(Value: TReferenceGisEvent_OpenProject); virtual; abstract;
    function GetOnRefCloseProjEvent: TReferenceGisEvent_CloseProject; virtual; abstract;
    procedure SetOnRefCloseProjEvent(Value: TReferenceGisEvent_CloseProject); virtual; abstract;
    function GetOnRefSaveProjEvent: TReferenceGisEvent_SaveProject; virtual; abstract;
    procedure SetOnRefSaveProjEvent(Value: TReferenceGisEvent_SaveProject); virtual; abstract;
    function GetOnRefUpdateExtProjEvent: TReferenceGisEvent_UpdateExtension; virtual; abstract;
    procedure SetOnRefUpdateExtProjEvent(Value: TReferenceGisEvent_UpdateExtension); virtual; abstract;
    function GetOnRefQUpdateExtProjEvent: TReferenceGisEvent_QuickUpdateExtension; virtual; abstract;
    procedure SetOnRefQUpdateExtProjEvent(Value: TReferenceGisEvent_QuickUpdateExtension); virtual; abstract;
    function GetOnRefRebuildProjEvent: TReferenceGisEvent_RebuildTree; virtual; abstract;
    procedure SetOnRefRebuildProjEvent(Value: TReferenceGisEvent_RebuildTree); virtual; abstract;
    function GetOnRefDrawProjEvent_VectorDraw: TReferenceGisEvent_VectorDraw; virtual; abstract;
    function GetOnRefDrawProjEvent_RasterDraw: TReferenceGisEvent_RasterDraw; virtual; abstract;
    procedure SetOnRefDrawProjEvent_VectorDraw(Value: TReferenceGisEvent_VectorDraw); virtual; abstract;
    procedure SetOnRefDrawProjEvent_RasterDraw(Value: TReferenceGisEvent_RasterDraw); virtual; abstract;
    //function GetSpatialReference: IlicgSpatialReference; virtual; abstract;
    //procedure SetSpatialReference(Value: IlicgSpatialReference); virtual; abstract;
    function AddLayer(const FileName: string; IsAnimation, WithDB: Boolean; LayerType: TlicgLayerType;
       ACS: IlicgCS; AColor: TColor; FieldList: TObject; ALayerListRefresh: Boolean = True): TlicgBaseLayer; virtual;
    function GetShortPathLayers: TStrings; virtual; abstract;
    procedure SetShortPathLayers(Value: TStrings); virtual; abstract;
    function GetQuerySelectTool: IQuerySelectTool; virtual; abstract;
    function GetRegionWork: IRegionWork; virtual; abstract;
    procedure SetRegionWork(Value: IRegionWork); virtual; abstract;
    //
    function GetReferenceGIS: IlicgReferenceGIS; virtual; abstract;
    procedure SetReferenceGIS(Value: IlicgReferenceGIS); virtual; abstract;
    function GetShowOnlineMap: Boolean; virtual; abstract;
    procedure SetShowOnlineMap(Value: Boolean); virtual; abstract;
    function GetOnlineMapPreference: TOnlineMapPreference; virtual; abstract;
    procedure SetOnlineMapPreference(Value: TOnlineMapPreference); virtual; abstract;
    function GetOnReadOnlyChange: TNotifyEvent; virtual; abstract;
    procedure SetOnReadOnlyChange(Value: TNotifyEvent); virtual; abstract;
    function GetTurkeyPaftaGis: TlicgBaseGis; virtual; abstract;
    function GetisTurkeyPaftaDrawing: Boolean; virtual; abstract;
    procedure SetisTurkeyPaftaDrawing(Value: Boolean); virtual; abstract;
    function GetBeginPaintFilteredEntities: TlicgPaintFilteredEntities; virtual; abstract;
    procedure SetBeginPaintFilteredEntities(Value: TlicgPaintFilteredEntities); virtual; abstract;
    function GetEndPaintFilteredEntities: TlicgPaintFilteredEntities; virtual; abstract;
    procedure SetEndPaintFilteredEntities(Value: TlicgPaintFilteredEntities); virtual; abstract;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function IsCAD: Boolean; virtual; abstract;

    procedure ClearClippedArea; virtual; abstract;
    function Internal_CreateLayer(const LayerName: string; LayerType: TlicgLayerType;
      AColor: TColor = clBlue; ALayerListRefresh: Boolean = True): TlicgBaseLayer; dynamic; abstract;
    procedure CreateNew(const FileName: string; const ATempGISFile: Boolean = False); dynamic; abstract;
    procedure OpenProject(AProjectFileName: string; AShowProgress: Boolean = True); dynamic; abstract;
    procedure SaveProject(AProjectFileName: string; AShowProgress: Boolean = True); dynamic; abstract;
    function IsDesigning: Boolean; virtual; abstract;
    procedure LoadFontFile(const FileName: string); virtual; abstract;
    procedure LoadLineTypesFile(const FileName: string; AGIS: TlicgBaseGIS); virtual; abstract;
    procedure LoadSymbolsFile(const FileName: string); virtual; abstract;
    procedure MaxMinExtents(var AXMin, AYMin, AXMax, AYMax: Double); virtual; abstract;
    function OpenMode: Word; virtual; abstract;
    procedure QuickUpdateExtension; virtual; abstract;
    procedure RebuildTree; virtual; abstract;
    procedure UpdateExtension(Grapher: TlicgBaseGrapher = nil); virtual; abstract;
    function UpdateExtensionNotLoadEntity: TlicgExtent; virtual; abstract;
    procedure RefreshViewports; virtual; abstract;
    procedure RefreshLayerManager; virtual; abstract;
    procedure RepaintViewports(UseThread: Boolean = False); virtual; abstract;
    procedure Save; virtual; abstract;
    procedure Open(IsReferans: Boolean = False); dynamic; abstract;
    procedure Close; dynamic; abstract;
    procedure WriteMapHeader(AFileName: string = ''); dynamic; abstract;
    procedure SaveAs(const AFileName: string); dynamic; abstract;
    procedure SaveIfModified; virtual; abstract;
    procedure UpdateExtensionWithRtree; virtual; abstract;
    procedure CreateDBFTable(const dbfFileName: string); virtual; abstract;
    function isReferansProject: Boolean; virtual; abstract;
    procedure SaveIfEntityVersionNumberChange; virtual; abstract;
    function PickEntity(Grapher: TlicgBaseGrapher; NoPickFilter: TlicgEntityIDs;
      const X, Y: Double; Aperture: Integer; const LayerName: string; var NLayer:
      TlicgBaseLayer; var NRecNo, NPoint: Integer; List: IPickedList;
      IncludeCSLayers: Boolean = True; CanDoOSNAP: Boolean = False): Boolean; virtual; abstract;
    function GetProject: IlicgProject; virtual; abstract;
    procedure SetProject(Value: IlicgProject); virtual; abstract;

    { Layers ile ilgili fonksiyonlar... }
    function AddToLayers(const FileName: string; LayerType: TlicgLayerType;
      AColor: TColor = clBlue; ALayerListRefresh: Boolean = True): Integer;
    function CreateSablonLayers(const Filenames: TStrings): Boolean;
    function CreateLayer(const ALayerName: string; ALayerType: TlicgLayerType; AColor: TColor = clBlue;
      AFieldList: TObject = nil; ALayerListRefresh: Boolean = True): TlicgBaseLayer;
    function CreateLayerEx(const LayerName: string; ACS: IlicgCS; AColor: TColor = clBlue;
      FieldList: TObject = nil; ALayerListRefresh: Boolean = True): TlicgBaseLayer; stdcall;
    function CreateCosmethicLayer(const AFileName: string; AColor: TColor = clBlue; ALayerListRefresh: Boolean = True): TlicgBaseLayer;
    function CreateAnimationLayer(const AFileName: string; AColor: TColor = clBlue; ALayerListRefresh: Boolean = True): TlicgBaseLayer;
    procedure SortLayers(SortType: TlicgLayerSortType);
    function DeleteLayer(const LayerName: string; DeleteFiles: Boolean = True): Boolean; virtual;
    function GetShortPath(SP_LayerName: string): TShortPathInterface; virtual; abstract;
    procedure DelShortPath(SP_LayerName: string); virtual; abstract;
    procedure PrepareShortPathLayers; virtual; abstract;

    function GetBlocks: IlicgBlocks; virtual; abstract;
    procedure SetBlocks(Value: IlicgBlocks); virtual; abstract;

    function GetSymbolsList: IlicgSymbolsList; virtual; abstract;
    procedure SetSymbolsList(Value: IlicgSymbolsList); virtual; abstract;
    function GetLineTypes: IlicgLineTypes; virtual; abstract;
    procedure SetLineTypes(Value: IlicgLineTypes); virtual; abstract;

    function SetFilter(const AExtent: TlicgExtent; const ALayerNames: TArray<string>; const AEntitys: TlicgEntityIDs): TlicgBaseSelection; virtual; abstract;
    function SetFilterEntityList(const AExtent: TlicgExtent; const ALayerNames: TArray<string>; const AEntitys: TlicgEntityIDs): IlicgEntityList; virtual; abstract;

    procedure ClearThematicList(_ThematicList: TList);


    // TlicgReferenceGIS Api
    function CSCoorToCSCoor(aXIn: Double; aYIn: Double; var aXOut: Double; var
      aYOut: Double; aProjIn, aProjOut: IlicgCS; ProjectApplyMatrix:
      PAffineMatrixElements; isAfter: Boolean = true): Boolean;

    {
    function  ZConnection : TZConnection; virtual; abstract;
    function  CreateZTable : TZTable; virtual; abstract;
    procedure DestroyZTable ( ztbl: TZTable ); virtual; abstract;
    function  CreateZQuery : TZQuery; virtual; abstract;
    procedure DestroyZQuery ( zsql: TZQuery ); virtual; abstract;
    function  ZTableExists (aTableName : string) : Boolean; virtual; abstract;
    function  MapLayerDataset : TObject; virtual; abstract;

    procedure BeginTransaction; virtual;
    procedure CommitTransaction; virtual;
    procedure RollbackTransaction; virtual;
    }
    property TempEntities: IlicgEntityList read GetTempEntities;
    property OnSelectableLayerChange: TlicgLayerEvent read
      GetOnSelectableLayerChange write SetOnSelectableLayerChange;
    property OnVisibleLayerChange: TlicgLayerEvent read GetOnVisibleLayerChange
      write SetOnVisibleLayerChange;
    property OnAfterInsertLayer: TlicgLayerEvent read GetOnAfterInsertLayer
      write SetOnAfterInsertLayer;
    property OnBeforeInsertLayer: TlicgBeforeLayerEvent read
      GetOnBeforeInsertLayer write SetOnBeforeInsertLayer;
    property OnAfterDeleteLayer: TlicgLayerEvent read GetOnAfterDeleteLayer
      write SetOnAfterDeleteLayer;
    property OnBeforeDeleteLayer: TlicgBeforeLayerEvent read
      GetOnBeforeDeleteLayer write SetOnBeforeDeleteLayer;
    property OnCurrentLayerChange: TlicgLayerEvent read GetOnCurrentLayerChange write SetOnCurrentLayerChange;
    property OnFarFillLayers: TNotifyEvent read FOnFarFillLayers write FOnFarFillLayers;
    property ClippedEntities: TlicgBaseSelection read GetClippedEntities;
    property ClipPolygonalArea: IlicgVector read GetClipPolygonalArea;
    property CurrentLayer: TlicgBaseLayer read GetCurrentLayer;
    property CurrentLayerName: string read GetCurrentLayerName write SetCurrentLayerName;
    property DrawBoxList: TlicgDrawBoxList read GetDrawBoxList;
    property HGuidelines: IlicgDoubleList read GetHGuidelines;
    property MapInfo: TlicgBaseMapInfo read GetMapInfo write SetMapInfo;
    property Modified: Boolean read GetModified write SetModified;
    //property ProjectionParams: TStrings read GetProjectionParams write SetProjectionParams ;
    property VGuidelines: IlicgDoubleList read GetVGuidelines;
    property ShowGuidelines: Boolean read GetShowGuidelines write SetShowGuidelines default True;
    property Active: Boolean read GetActive write SetActive;
    property AutoSetLastView: Boolean read GetAutoSetLastView write SetAutoSetLastView default True;
    property FileName: string read GetFileName write SetFileName;
    property PrintTimerFrequency: Integer read GetPrintTimerFrequency write SetPrintTimerFrequency default 800;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHourglass: Boolean read GetShowHourglass write SetShowHourglass default True;
    property TimerFrequency: Integer read GetTimerFrequency write SetTimerFrequency default 200;
    property OnEntityChanged: TlicgEntityEvent read GetOnEntityChanged write SetOnEntityChanged;
    property OnBeforeDeleteEntity: TlicgEntityAcceptEvent read GetOnBeforeDeleteEntity write SetOnBeforeDeleteEntity;
    property OnAfterDeleteEntity: TlicgEntityEvent read GetOnAfterDeleteEntity write SetOnAfterDeleteEntity;
    property OnBeforeInsertEntity: TlicgBeforeInsertEvent read GetOnBeforeInsertEntity write SetOnBeforeInsertEntity;
    property OnAfterInsertEntity: TlicgEntityEvent read GetOnAfterInsertEntity write SetOnAfterInsertEntity;
    property OnAfterDragDrop: TlicgEntityEvent read GetOnAfterDragDrop write SetOnAfterDragDrop;
    property OnAfterPaintEntity: TlicgAfterPaintEntityEvent read GetOnAfterPaintEntity write SetOnAfterPaintEntity;
    property OnAfterPaintLayer: TlicgAfterPaintLayerEvent read GetOnAfterPaintLayer write SetOnAfterPaintLayer;
    property OnAfterPaintLayers: TNotifyEvent read GetOnAfterPaintLayers write SetOnAfterPaintLayers;
    property OnAfterTransform: TlicgEntityEvent read GetOnAfterTransform write SetOnAfterTransform;
    property OnBeforeBrushPaint: TlicgBeforeBrushPaintEvent read GetOnBeforeBrushPaint write SetOnBeforeBrushPaint;
    property OnBeforeClose: TNotifyEvent read GetOnBeforeClose write SetOnBeforeClose;
    property OnBeforeDragDrop: TlicgEntityAcceptEvent read GetOnBeforeDragDrop write SetOnBeforeDragDrop;
    property OnBeforeFontPaint: TlicgBeforeFontPaintEvent read GetOnBeforeFontPaint write SetOnBeforeFontPaint;
    property OnBeforeOpen: TNotifyEvent read GetOnBeforeOpen write SetOnBeforeOpen;
    property OnBeforePaintEntity: TlicgBeforePaintEntityEvent read GetOnBeforePaintEntity write SetOnBeforePaintEntity;
    property OnBeforePaintLayer: TlicgBeforePaintLayerEvent read GetOnBeforePaintLayer write SetOnBeforePaintLayer;
    property OnBeforePaintLayers: TNotifyEvent read GetOnBeforePaintLayers write SetOnBeforePaintLayers;
    property OnBeforePenPaint: TlicgBeforePenPaintEvent read GetOnBeforePenPaint write SetOnBeforePenPaint;
    property OnBeforeSymbolPaint: TlicgBeforeSymbolPaintEvent read GetOnBeforeSymbolPaint write SetOnBeforeSymbolPaint;
    property OnBeforeTransform: TlicgBeforeTransformEvent read GetOnBeforeTransform write SetOnBeforeTransform;
    property OnClippedAreaChanged: TNotifyEvent read GetOnClippedAreaChanged write SetOnClippedAreaChanged;
    property OnEndExternalPopulate: TlicgEndExternalPopulateEvent read GetOnEndExternalPopulate write SetOnEndExternalPopulate;
    property OnError: TlicgErrorEvent read GetOnError write SetOnError;
    property OnExternalPopulate: TlicgExternalPopulateEvent read GetOnExternalPopulate write SetOnExternalPopulate;
    property OnFileNameChange: TNotifyEvent read GetOnFileNameChange write SetOnFileNameChange;
    property OnGisTimer: TlicgGisTimerEvent read GetOnGisTimer write SetOnGisTimer;
    property OnIsControlPointSelectable: TlicgIsControlPointSelectableEvent read GetOnIsControlPointSelectable write SetOnIsControlPointSelectable;
    property OnControlPointChanged: TlicgControlPointChangedEvent read GetOnControlPointChanged write SetOnControlPointChanged;
    property OnModified: TNotifyEvent read GetOnModified write SetOnModified;
    property OnPrintBegin: TNotifyEvent read GetOnPrintBegin write SetOnPrintBegin;
    property OnPrintEnd: TNotifyEvent read GetOnPrintEnd write SetOnPrintEnd;
    property OnPrintProgress: TlicgPrintProgressEvent read GetOnPrintProgress write SetOnPrintProgress;
    property OnStartExternalPopulate: TlicgStartExternalPopulateEvent read GetOnStartExternalPopulate write SetOnStartExternalPopulate;
    property OnUDFCheck: TlicgUDFCheckEvent read GetOnUDFCheck write SetOnUDFCheck;
    property OnUDFSolve: TlicgUDFSolveEvent read GetOnUDFSolve write SetOnUDFSolve;
    property OnBeforeOpenLayer: TlicgLayerEvent read GetOnBeforeOpenLayer write SetOnBeforeOpenLayer;
    property OnBeforeCloseLayer: TlicgLayerEvent read GetOnBeforeCloseLayer write SetOnBeforeCloseLayer;
    property LastPointName: string read GetLastPointName write SetLastPointName;
      // if True then Draw point name in TlicgPaintingThread.DrawEntities
    property LastPolygonName: string read GetLastPolygonName write SetLastPolygonName;
    property PointNameDisplay: Boolean read GetPointNameDisplay write SetPointNameDisplay;
    //property LastPointColor:TColor read  GetLastPointColor write SetLastPointColor;
    property ProjectScale: Double read GetProjectScale write SetProjectScale;

    // İmport işlemi yaparken set edilir, AddEntity işleminde uygulanır, import işlemi bittiğinde nillenir.

    //property Topology: IlicgTopology read GetTopology write SetTopology;

    property OntheFlyMatrixElements: TAffineMatrixElements read GetOnTheFlyMatrixElements write SetOnTheFlyMatrixElements;
    property InProcess: Boolean read GetInProcess write SetInProcess default False;     //Nopaint in some action (Pack etc.)
    property BaseTableClass: TlicgBaseTableClass read GetBaseTableClass write SetBaseTableClass;
    //property RasterList: TlicgaseRasterList read GetRasterList write SetRasterList;
    property Layers: TlicgBaseLayers read GetLayers;
    property CurrentScale: Double read GetCurrentScale write SetCurrentScale;
    //property SpatialReference: IlicgSpatialReference read GetSpatialReference write SetSpatialReference;
    property OnRefOpenProjEvent: TReferenceGisEvent_OpenProject read
      GetOnRefOpenProjEvent write SetOnRefOpenProjEvent;
    property OnRefCloseProjEvent: TReferenceGisEvent_CloseProject read
      GetOnRefCloseProjEvent write SetOnRefCloseProjEvent;
    property OnRefSaveProjEvent: TReferenceGisEvent_SaveProject read
      GetOnRefSaveProjEvent write SetOnRefSaveProjEvent;
    property OnRefUpdateExtProjEvent: TReferenceGisEvent_UpdateExtension read
      GetOnRefUpdateExtProjEvent write SetOnRefUpdateExtProjEvent;
    property OnRefQUpdateExtProjEvent: TReferenceGisEvent_QuickUpdateExtension
      read GetOnRefQUpdateExtProjEvent write SetOnRefQUpdateExtProjEvent;
    property OnRefRebuildProjEvent: TReferenceGisEvent_RebuildTree read
      GetOnRefRebuildProjEvent write SetOnRefRebuildProjEvent;
    property OnRefDrawProjEvent_VectorDraw: TReferenceGisEvent_VectorDraw read
      GetOnRefDrawProjEvent_VectorDraw write SetOnRefDrawProjEvent_VectorDraw;
    property OnRefDrawProjEvent_RasterDraw: TReferenceGisEvent_RasterDraw read
      GetOnRefDrawProjEvent_RasterDraw write SetOnRefDrawProjEvent_RasterDraw;
    property ShortPathLayers: TStrings read GetShortPathLayers write SetShortPathLayers;
    property QuerySelectTool: IQuerySelectTool read GetQuerySelectTool;
    property DesktopLayerType: TDesktopLayerType read GetDesktopLayerType write
      SetDesktopLayerType;
    property OnFillLayersToListBox: TNotifyEvent read FOnFillLayersToListBox
      write FOnFillLayersToListBox;
    property OnHintCommandMessageEvent: THintCommandMessageEvent read
      FHintCommandMessageEvent write FHintCommandMessageEvent;
    property Project: IlicgProject read GetProject write SetProject;
    property RegionWork: IRegionWork read GetRegionWork write SetRegionWork;
    property TempParamString: string read FTempParamString write FTempParamString;
    property OnlySelectionExport: Boolean read FOnlySelectionExport write
      FOnlySelectionExport;
    property ThematicList: TList read FThematicList write FThematicList;
    property TextureFillLibs: TlicgTextureFillLibraries read FTextureFillLibs write FTextureFillLibs;
    property OnBeforePaintLayerThematic: TlicgBeforePaintLayerThematicEvent read
      GetBeforePaintLayerThematicEvent write SetBeforePaintLayerThematicEvent;
    property OnAfterPaintLayerThematic: TlicgAfterPaintLayerThematicEvent read
      GetAfterPaintLayerThematicEvent write SetAfterPaintLayerThematicEvent;
    property ReferenceGIS: IlicgReferenceGIS read GetReferenceGIS write SetReferenceGIS;
    property OnlineMapPreference: TOnlineMapPreference read
      GetOnlineMapPreference write SetOnlineMapPreference;
    property OnReadOnlyChange: TNotifyEvent read GetOnReadOnlyChange write
      SetOnReadOnlyChange;
    property TurkeyPaftaGis: TlicgBaseGis read GetTurkeyPaftaGis;
    property isTurkeyPaftaDrawing: Boolean read GetisTurkeyPaftaDrawing write
      SetisTurkeyPaftaDrawing;
    property OnBeginPaintFilteredEntities: TlicgPaintFilteredEntities read
      GetBeginPaintFilteredEntities write SetBeginPaintFilteredEntities;
    property OnEndPaintFilteredEntities: TlicgPaintFilteredEntities read
      GetEndPaintFilteredEntities write SetEndPaintFilteredEntities;
    property ShowOnlineMap: Boolean read GetShowOnlineMap write SetShowOnlineMap;

    property ThicknessWidth: Integer read GetThicknessWidth write SetThicknessWidth;

    property GuideIsolineInterval: Double read GetGuideIsolineInterval write SetGuideIsolineInterval;

    property GuideIsoline: Boolean read Get_Reserved_GuideIsoline write Set_Reserved_GuideIsoline;// 19;
    property Thickness: Boolean read Get_Reserved_Thickness write Set_Reserved_Thickness;// 18;
    property PointKod: Boolean read Get_Reserved_PointKod write Set_Reserved_PointKod;  // 17
    property LineWidthDirection: Boolean read Get_Reserved_LineWidthDirection write Set_Reserved_LineWidthDirection;  // 16;
    property ApplyBufferForPolyline: Boolean read Get_Reserved_ApplyBufferForPolyline write Set_Reserved_ApplyBufferForPolyline;  // 15;
    property NodeVisible: Boolean read Get_Reserved_NodeVisible write Set_Reserved_NodeVisible;  // 14;
    property Labeling: Boolean read Get_Reserved_Labeling write Set_Reserved_Labeling;  // 13;
    property ZDisplay: Boolean read Get_Reserved_ZDisplay write Set_Reserved_ZDisplay;  // 10;
    property ShowArrow: Boolean read Get_Reserved_ShowArrow write Set_Reserved_ShowArrow;  // 8;
    property LayerBrush: Boolean read Get_Reserved_LayerBrush write Set_Reserved_LayerBrush;  // 7;
    property Print: Boolean read Get_Reserved_Print write Set_Reserved_Print;  // 3;
    property PolygonName: Boolean read Get_Reserved_PolygonName write Set_Reserved_PolygonName;  // 2;
    property PointName: Boolean read Get_Reserved_PointName write Set_Reserved_PointName;  // 1;
    property Balastro: Boolean read Get_Reserved_Balastro write Set_Reserved_Balastro;  //0;
    property UseReservedData: Boolean read FUseReservedData write FUseReservedData;
    property Blocks: IlicgBlocks read GetBlocks write SetBlocks;
    property SymbolsList: IlicgSymbolsList read GetSymbolsList write SetSymbolsList;
    property LineTypes: IlicgLineTypes read GetLineTypes write SetLineTypes;
  end;

  {-------------------------------------------------------------------------------}
  {                  Selection                                                    }
  {-------------------------------------------------------------------------------}

  TlicgSelectProc = procedure(Selection: TlicgBaseSelection); stdcall;
    { TlicgSelectionLayer
      This class is for maintaining the selected entities in a layer}

  TlicgBaseSelectionLayer = class
  protected
    function GetLayer: TlicgBaseLayer; virtual; abstract;
    function GetSelList: IlicgIntegerList; virtual; abstract;
    procedure SetLayer(const Value: TlicgBaseLayer); virtual; abstract;
  public
    constructor Create(Selection: TlicgBaseSelection; Layer: TlicgBaseLayer);
    destructor Destroy; override;
    procedure Add(RecNo: Integer); virtual; abstract;
    procedure Delete(RecNo: Integer); virtual; abstract;
    function IsSelected(RecNo: Integer): Boolean; virtual; abstract;
    function GetRecExtension(RecNo: Integer; MarginSize: Integer = 4): TlicgExtent;
      virtual; abstract;
    procedure SelectAll(NoPickFilter: TlicgEntityIDs = []); virtual; abstract;
    procedure SelectIList(ol: IlicgIntegerList; NoPickFilter: TlicgEntityIDs = [];
      ClearBefore: Boolean = True); virtual; abstract; // clearBefore eklendi
    procedure InverseSelect(NoPickFilter: TlicgEntityIDs = []); virtual; abstract;
    property Layer: TlicgBaseLayer read GetLayer write SetLayer;
    property SelList: IlicgIntegerList read GetSelList;
  end;

  { TlicgBaseSelection - Main Selection object. Contains the list of selected layers }

  TlicgBaseSelection = class
  protected
    function Get(Index: Integer): TlicgBaseSelectionLayer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetNumSelected: Integer; virtual; abstract;
    function GetDrawBox: TlicgBaseDrawBox; virtual; abstract;
    function GetEnabled: Boolean; virtual; abstract;
    function GetInUpdate: Boolean; virtual; abstract;
    procedure SetDrawBox(const Value: TlicgBaseDrawBox); virtual; abstract;
    procedure SetEnabled(const Value: Boolean); virtual; abstract;
    procedure SetInUpdate(const Value: Boolean); virtual; abstract;
  public
    constructor Create(DrawBox: TlicgBaseDrawBox);
    destructor Destroy; override;
    procedure Add(Layer: TlicgBaseLayer; RecNo: Integer); virtual; abstract;
    procedure AddLayer(Layer: TlicgBaseLayer); virtual; abstract;
    procedure SelectAllLayer(Layer: TlicgBaselayer; NoPickFilter: TlicgEntityIDs = []); virtual; abstract;
    procedure SelectIListLayer(Layer: TlicgBaselayer; ol: IlicgIntegerList;
      NoPickFilter: TlicgEntityIDs = []; ClearBefore: Boolean = True); virtual; abstract;
    procedure InverseSelect(Layer: TlicgBaselayer; NoPickFilter: TlicgEntityIDs = []); virtual; abstract;
    function SelectEntityFromScreen(Cmdline: TObject; ClearBefore: Boolean; LayerList: TStrings;
      PickFilter: TlicgEntityIDs; SelectCount: Integer = -1): Boolean; virtual; abstract;
    procedure ApplyBrushStyle(Style: IlicgBrushTool; const Apply: TlicgBrushApply); virtual; abstract;
    procedure ApplyFontStyle(Style: IlicgFontTool; const Apply: TlicgFontApply); virtual; abstract;
    procedure ApplyPenStyle(Style: IlicgPenTool; const Apply: TlicgPenApply); virtual; abstract;
    procedure ApplySymbolStyle(Style: IlicgSymbolTool; const Apply: TlicgSymbolApply); virtual; abstract;
    procedure ApplyOpenedEnts(Style: IlicgBaseTool; StyleX: IlicgBaseTool; const Apply: TlicgPenApply); virtual; abstract;
    procedure ApplyClosedEnts(Style: IlicgBaseTool; PenStyle: IlicgBaseTool; PenStyleX: IlicgBaseTool;
      const ApplyPen: TlicgPenApply; const ApplyBrush: TlicgBrushApply); virtual; abstract;
    procedure ApplySymbolEnts(Style: IlicgBaseTool; const Apply: TlicgSymbolApply); virtual; abstract;

    procedure ApplyVTextEnts(Style: IlicgBaseTool; PenStyle: IlicgBaseTool;
      BrushStyle: IlicgBaseTool; ApplyPen: TlicgPenApply; ApplyBrush:
      TlicgBrushApply; vfontApply: TlicgVFontApply; textWidth: Double;
      heightKats: double = 1); virtual; abstract;
    procedure ApplyTTextEnts(Style: IlicgBaseTool; PenStyle: IlicgBaseTool;
      BrushStyle: IlicgBaseTool; ApplyPen: TlicgPenApply; ApplyBrush:
      TlicgBrushApply; tfontApply: TlicgFontApply; heightKats: double = 1);
      virtual; abstract;
    procedure ApplyPointEnts(color: TColor); virtual; abstract;
    procedure ApplyAllEnts(PenTool: IlicgPenTool; ArrowTool: IlicgArrowTool;
      ApplyPen: TlicgPenApply; // pen parameters
      BrushTool: IlicgBrushTool; ApplyBrush: TlicgBrushApply;              // brush parameters
      SymbolTool: IlicgSymbolTool; ApplySymbol: TlicgSymbolApply;                 // symbol
      VFontTool: IlicgFontTool; vfontApply: TlicgVFontApply; textWidth: Double;
      vheightKats: double;                   // Vec.Font
      ttFontTool: IlicgFontTool; tfontApply: TlicgFontApply; ttheightKats:
      double;  // tt Font
      PointColor: TColor; ApplyPoint: Boolean; // point
      LayerProps, ApplyLayerProps: Boolean // Özelliklerini tabakadan al
    ); virtual; abstract;
    procedure Assign(Source: TlicgBaseSelection); virtual; abstract;
    procedure BeginUpdate; virtual; abstract;
    procedure Clear(UseThread: Boolean = False); virtual; abstract;
    procedure Delete(Layer: TlicgBaseLayer; RecNo: Integer); virtual; abstract;
    procedure DeleteLayer(Layer: TlicgBaseLayer); virtual; abstract;
    procedure DeleteSelection; virtual; abstract;
    procedure EndUpdate; virtual; abstract;
    function GetExtension(MarginSize: Integer = 4; ExactArea: Boolean = False): TlicgExtent; virtual; abstract;
    function GetRecExtension(Layer: TlicgBaseLayer; RecNo: Integer; MarginSize: Integer = 4): TlicgExtent; virtual; abstract;
    function IndexOf(Layer: TlicgBaseLayer): Integer; virtual; abstract;
    function IsSelected(Layer: TlicgBaseLayer; RecNo: Integer): Boolean; virtual; abstract;
    function NumSelectedInLayer(Layer: TlicgBaseLayer): Integer; virtual; abstract;
    procedure RepaintSelectionArea; virtual; abstract;
    procedure Toggle(Layer: TlicgBaseLayer; RecNo: Integer); virtual; abstract;
    procedure UnSelectAndRepaint; virtual; abstract;
    property Count: Integer read GetCount;
    property DrawBox: TlicgBaseDrawBox read GetDrawBox write SetDrawBox;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property InUpdate: Boolean read GetInUpdate write SetInUpdate;
    property Items[Index: Integer]: TlicgBaseSelectionLayer read Get; default;
    property NumSelected: Integer read GetNumSelected;
  end;

  { TlicgEntityList }
  TlicgEntityList = class(TInterfacedObject, IlicgEntityList)
  private
    FList: IInterfaceList;
    FRecNoList: TList<Integer>;
    function Get(Index: Integer): IlicgEntity;
    procedure _Set(Index: Integer; Value: IlicgEntity);
    function GetList: IInterfaceList;
    function GetRecNoItems(Index: Integer): Integer;
    procedure SetRecNoItems(Index: Integer; Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Ent: IlicgEntity): Integer; stdcall;
    function AddIfNotFound(Ent: IlicgEntity): Integer; stdcall;
    function AddEntity(AEntity: IlicgEntity; ARecNo: Integer): Integer; stdcall;
    procedure Clear; stdcall;
    function Count: Integer; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    procedure Extract(Index: Integer); stdcall;
    procedure ExtractAll; stdcall;
    procedure Insert(Index: Integer; Ent: IlicgEntity); stdcall;
    procedure Replace(Index: Integer; Ent: IlicgEntity); stdcall;
    function IndexOf(Item: IlicgEntity): Integer; stdcall;
    property Items[Index: Integer]: IlicgEntity read Get write _Set; default;
    property List: IInterfaceList read GetList;
    property RecNoItems[Index: Integer]: Integer read GetRecNoItems write SetRecNoItems;
  end;

  {-------------------------------------------------------------------------------}
  {                  TEntityStringList                                                }
  {-------------------------------------------------------------------------------}

  TEntityIntStringList = class(TInterfaceStringList)
  private
    function GetEntity(Index: Integer): IlicgEntity;
  public
    property Entity[Index: Integer]: IlicgEntity read GetEntity;
  end;

  {-------------------------------------------------------------------------------}
  {                  TlicgBaseDrawBox                                               }
  {-------------------------------------------------------------------------------}

  TlicgBeforeSelectEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; var CanSelect: Boolean) of object;
  { This event is used for both : when the entity is selected and unselected }

  TlicgEntitySelectEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer) of object;
  { When an entity is DOUBLE clicked in a TlicgBaseDrawBox }

  TlicgEntityDblClickEvent = procedure(Sender: TObject; Layer: TlicgBaseLayer;
    RecNo: Integer; var Processed: Boolean) of object;
  { When an entity is clicked in a TlicgBaseDrawBox }

  TlicgEntityClickEvent = procedure(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer; const WX, WY: Double; Layer: TlicgBaseLayer;
    RecNo: Integer; var Processed: Boolean) of object;

  TlicgClickEvent = procedure(Sender: TObject; X, Y: Integer; const XWorld,
    YWorld: Double) of object;

  TlicgSelectWhenReshaping = procedure(Sender: TObject; X, Y: Integer; const WX, WY: Double;
    Layer: TlicgBaseLayer; RecNo: Integer; var CanSelect: Boolean) of object;

  (* ilker silme kullanılmıyorç
  TlicgThemedCustomControl = class(TCustomControl)
  private
    FUseXPThemes: Boolean;
    FBorderStyle: forms.TBorderStyle;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMCtl3DChanged(var m: TMessage); message CM_CTL3DCHANGED;
    procedure CreateThemeHandle; virtual;
    procedure FreeThemeHandle; virtual;
    procedure SetUseXPThemes(const Value: Boolean);
    procedure SetBorderStyle(const Value: forms.TBorderStyle);
    function GetBorderStyle: forms.TBorderStyle;
    function GetUseXPThemes: Boolean;
  protected
    FTheme: HTheme;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property UseXPThemes: Boolean read GetUseXPThemes write SetUseXPThemes default True;
    property BorderStyle: forms.TBorderStyle read GetBorderStyle write
      SetBorderStyle default bsSingle;
    property Align;
    property Color;
    property Ctl3D;
    property Enabled;
    property DragCursor;
  end;      *)

  {-------------------------------------------------------------------------------}
  //             TlicgBaseDrawBox - abstract control
  {-------------------------------------------------------------------------------}

  PThreadData = ^TlicgThreadData;

  TlicgThreadData = record
    ThreadHandle: THandle;
    ThreadID: Cardinal;
    DrawBox: TlicgBaseDrawBox;
    WCRect: TlicgExtent;
    Region: HRgn;
    PainterObject: IlicgPainterObject;
    Canvas: TCanvas;
  end;


  {-------------------------------------------------------------------------------}
  {                  Undo object                                                  }
  {-------------------------------------------------------------------------------}

  TlicgBaseUndoPool = class;
  { TlicgUndoSingle }

  TlicgBaseUndoSingle = class
  protected
    function GetUndoStream: TStream; virtual; abstract;
  public
    constructor Create(UndoPool: TlicgBaseUndoPool);
    destructor Destroy; override;
    procedure AddUnDeleteFromSelection(ABeginEndUndo: Boolean = True); virtual; abstract;
    procedure AddUndo(Layer: TlicgBaseLayer; RecNo: Integer; UndoAction:
      TlicgUndoAction; E: IlicgEntity = nil); virtual; abstract;
    procedure AddUnTransform(Layer: TlicgBaseLayer; RecNo: Integer); virtual; abstract;
    procedure AddUnTransformFromSelection; virtual; abstract;
    procedure BeginUndo(GlobalUndoAction: TlicgUndoAction); virtual; abstract;
    procedure EndUndo; virtual; abstract;
    function GetVerb: string; virtual; abstract;
    function Undo: TlicgExtent; virtual; abstract;
  end;

  TlicgBaseUndoPool = class
  protected
    procedure AdjustStack; virtual; abstract;
    procedure SetUndoLimit(Value: Integer); virtual; abstract;
    function GetUndoLimit: Integer; virtual; abstract;
  public
    constructor Create(DrawBox: TlicgBaseDrawBox; isCreateUndoSave: Boolean);
    destructor Destroy; override;
    procedure AddUnDeleteFromSelection(ABeginEndUndo: Boolean = True); virtual; abstract;
    procedure AddUndo(Layer: TlicgBaseLayer; RecNo: Integer; Action:
      TlicgUndoAction; E: IlicgEntity = nil); virtual; abstract;
    procedure AddUnTransform(Layer: TlicgBaseLayer; RecNo: Integer); virtual; abstract;
    procedure AddUnTransformFromSelection; virtual; abstract;
    procedure BeginUndo(GlobalUndoAction: TlicgUndoAction); virtual; abstract;
    function CanPaste: Boolean; virtual; abstract;
    function CanUndo: Boolean; virtual; abstract;
    procedure Clear(Clean: Boolean = False); virtual; abstract;
    procedure EndUndo; virtual; abstract;
    function GetVerb: string; virtual; abstract;
    procedure CopyToClipboardFromSelection(const ReferanceCoor: TlicgCoor); virtual; abstract;
    procedure PasteFromClipboard; virtual; abstract;
    function PasteFromClipboardToLocation(var AGIS: TlicgBaseGIS; out AReferanceCoor: TlicgCoor; var AClipboardExtent: TlicgExtent): Boolean; virtual; abstract;
    procedure Pop; virtual; abstract;
    function Undo: TlicgExtent; virtual; abstract;
    function Count: Integer; virtual; abstract;
    property UndoLimit: Integer read GetUndoLimit write SetUndoLimit;
  end;

  TlicgBaseRedoSingle = class
  protected
    function GetRedoStream: TStream; virtual; abstract;
  public
    constructor Create(DrawBox: TlicgBaseDrawBox);
    destructor Destroy; override;
    procedure BeginRedo; virtual; abstract;
    procedure AddRedo(Layer: TlicgBaseLayer; RecNo: Integer; Action:
      TlicgUndoAction); virtual; abstract;
    procedure EndRedo; virtual; abstract;
    function Redo: TlicgExtent; virtual; abstract;
  end;

  TlicgBaseRedoPool = class
  protected
    procedure AdjustStack; virtual; abstract;
    procedure SetRedoLimit(Value: Integer); virtual; abstract;
    function GetRedoLimit: Integer; virtual; abstract;
  public
    constructor Create(DrawBox: TlicgBaseDrawBox);
    destructor Destroy; override;
    procedure BeginRedo; virtual; abstract;
    procedure AddRedo(Layer: TlicgBaseLayer; RecNo: Integer; Action:
      TlicgUndoAction); virtual; abstract;
    procedure EndRedo; virtual; abstract;
    function CanRedo: Boolean; virtual; abstract;
    procedure Clear(Clean: Boolean = False); virtual; abstract;
    function GetVerb: string; virtual; abstract;
    function Redo: TlicgExtent; virtual; abstract;
    property RedoLimit: Integer read GetRedoLimit write SetRedoLimit;
  end;

  TSystemKeyDownEvent = procedure(Sender: TObject; KeyCode: Word) of object;

  TDrawBoxType = (dbtNormal, dbtSymbol, dbtPreview, dbtAerial, dbtMosaic, dbtActualPreview);

  TlicgCustomPaperItem = class(TCollectionItem)
  private
    FName: string;
    FWidthInches: Single;
    FHeightInches: Single;
    function GetHeightInches: Single;
    function GetName: string;
    function GetWidthInches: Single;
    procedure SetHeightInches(const Value: Single);
    procedure SetName(const Value: string);
    procedure SetWidthInches(const Value: Single);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read GetName write SetName;
    property WidthInches: Single read GetWidthInches write SetWidthInches;
    property HeightInches: Single read GetHeightInches write SetHeightInches;
  end;

  TlicgCustomPaperList = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TlicgCustomPaperItem;
    procedure SetItem(Index: Integer; Value: TlicgCustomPaperItem);
    function GetListPrintPapers: TStrings;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TlicgCustomPaperItem;
    property Items[Index: Integer]: TlicgCustomPaperItem read GetItem write
      SetItem; default;
  end;

  TlicgPageAdvance = (padvNone, padvLeft, padvRight, padvUp, padvDown);

  TlicgBaseDrawBox = class(TCustomControl) //(TlicgThemedCustomControl)
  private
    FMovePoint: TPoint;
    FOnAfterScroll: TNotifyEvent;
    FOnAfterSelect: TlicgEntitySelectEvent;
    FOnAfterUnSelect: TlicgEntitySelectEvent;
    FOnBeforeScroll: TNotifyEvent;
    FOnBeforeSelect: TlicgBeforeSelectEvent;
    FOnBeginRepaint: TNotifyEvent;
    FOnCustomClick: TlicgClickEvent;
    FOnEndRepaint: TNotifyEvent;
    FOnEntityDblClick: TlicgEntityDblClickEvent;
    FOnGisChanged: TNotifyEvent;
    FOnGridError: TNotifyEvent;
    FOnHChange: TNotifyEvent;
    FOnHScroll: TScrollEvent;
    FOnInternalUseMouseMove: TlicgMouseMoveEvent;
    FOnMouseDown2D: TlicgMouseEvent;
    FOnMouseMove2D: TlicgMouseMoveEvent;
    FOnMouseUp2D: TlicgMouseEvent;
    FOnPaint: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnShowHint: TlicgShowHintEvent;
    FOnVChange: TNotifyEvent;
    FOnVScroll: TScrollEvent;
    FOnZoomChange: TlicgZoomChangeEvent;
    FOnRepaintThreadFinished: TNotifyEvent;
    FOnSelectWhenReshaping: TlicgSelectWhenReshaping;
    FUpdateOnScroll: Boolean;
    FSystemKeyDownEvent: TSystemKeyDownEvent;
    FInsertPointWhenLineDraw: Boolean;
    FOnRegenDrawingForSomeActions: TNotifyEvent;
    FCmdline: TObject;
  protected
    procedure CreateWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Repaint; override;
    procedure SystemKeydown(var message: TMessage); message WM_SYSKEYDOWN;
    // FarEvents procedure
    procedure SetFarOnchangeCurrProject(Value: TFarOnchangeCurrProject); virtual; abstract;
    function GetFarOnchangeCurrProject: TFarOnchangeCurrProject; virtual; abstract;
    procedure CalcScrollBarDeltas(var DeltaX, DeltaY: Integer); virtual; abstract;
    procedure SetBorderStyle(const Value: TBorderStyle); virtual; abstract;
    function GetBorderStyle: TBorderStyle; virtual; abstract;
    procedure SetFlatScrollBar(Value: Boolean); virtual; abstract;
    procedure SetGridInfo(Value: TlicgGridInfo); virtual; abstract;
    procedure SetHParams(APosition, AMin, AMax: Integer); virtual; abstract;
    procedure SetScreenGrid(Value: TlicgScreenGrid); virtual; abstract;
    procedure SetScrollBars(Value: TScrollStyle); virtual; abstract;
    procedure SetShowLayerExtents(Value: Boolean); virtual; abstract;
    procedure SetShowMapExtents(Value: Boolean); virtual; abstract;
    procedure SetVParams(APosition, AMin, AMax: Integer); virtual; abstract;
    procedure DoRepaintExtent(const WCRect: TlicgExtent; UseThread: Boolean); virtual; abstract;
    function GetUseThread: Boolean; virtual; abstract;
    procedure SetUseThreads(const Value: Boolean); virtual; abstract;
    function GetAlwaysDisplayListDrawMode: TlicgDrawMode; virtual; abstract;
    function GetBlinkCount: Integer; virtual; abstract;
    function GetBlinkRate: Integer; virtual; abstract;
    function GetDefaultScaleUnits: TlicgScaleUnits; virtual; abstract;
    function GetDelayShowHint: Integer; virtual; abstract;
    function GetDropRepeat: Integer; virtual; abstract;
    function GetFlatScrollBar: Boolean; virtual; abstract;
    function GetGrapher: TlicgBaseGrapher; virtual; abstract;
    function GetGridInfo: TlicgGridInfo; virtual; abstract;
    function GetHideVertexNumber: Boolean; virtual; abstract;
    function GetInRepaint: Boolean; virtual; abstract;
    function GetInUpdate: Boolean; virtual; abstract;
    function GetIsAerial: Boolean; virtual; abstract;
    function GetIsThreadRunning: Boolean; virtual; abstract;
    procedure SetIsThreadRunning(Value: Boolean); virtual; abstract;
    function GetNoPickFilter: TlicgEntityIDs; virtual; abstract;
    function GetOutputDevices: TlicgOutputDevices; virtual; abstract;
    function GetPartialSelect: Boolean; virtual; abstract;
    function GetRedo: TlicgBaseRedoPool; virtual; abstract;
    //function  GetRubberPen: TPen;virtual; abstract;
    function GetRubberPenColor: TColor; virtual; abstract;
    function GetRubberPenMode: TPenMode; virtual; abstract;
    function GetRubberPenStyle: TPenStyle; virtual; abstract;
    function GetRubberPenWidth: Integer; virtual; abstract;
    procedure SetRubberPenStyle(Value: TPenStyle); virtual; abstract;
    procedure SetRubberPenWidth(Value: Integer); virtual; abstract;
    procedure SetRubberPenMode(Value: TPenMode); virtual; abstract;
    procedure SetRubberPenColor(Value: TColor); virtual; abstract;
    function GetScreenBitmap: TBitmap; virtual; abstract;
    function GetScreenGrid: TlicgScreenGrid; virtual; abstract;
    function GetScrollBars: TScrollStyle; virtual; abstract;
    function GetSelection: TlicgBaseSelection; virtual; abstract;
    function GetShowControlPoints: Boolean; virtual; abstract;
    function GetShowLayerExtents: Boolean; virtual; abstract;
    function GetShowMapExtents: Boolean; virtual; abstract;
    function GetShowTransfPoints: Boolean; virtual; abstract;
    function GetSnapToGuidelines: Boolean; virtual; abstract;
    function GetSnapToGuidelinesDist: Double; virtual; abstract;
    function GetStackedSelect: Boolean; virtual; abstract;
    function GetSymbolMarker: Integer; virtual; abstract;
    function GetTempEntities: IlicgEntityList; virtual; abstract;
    function GetThematicSeriesColors: TlicgThematicSeriesColor; virtual; abstract;
  //  function  GetTileBitmap: Tlicgitmap;virtual; abstract;
    function GetUndo: TlicgBaseUndoPool; virtual; abstract;
    function GetUseThreadOnResize: Boolean; virtual; abstract;
    function GetUseThreadOnScroll: Boolean; virtual; abstract;
    function GetZoomWithMargins: Boolean; virtual; abstract;
    procedure SetAlwaysDisplayListDrawMode(Value: TlicgDrawMode); virtual; abstract;
    procedure SetBlinkCount(const Value: Integer); virtual; abstract;
    procedure SetBlinkRate(const Value: Integer); virtual; abstract;
    procedure SetDefaultScaleUnits(const Value: TlicgScaleUnits); virtual; abstract;
    procedure SetDelayShowHint(const Value: Integer); virtual; abstract;
    procedure SetDropRepeat(const Value: Integer); virtual; abstract;
    procedure SetHideVertexNumber(const Value: Boolean); virtual; abstract;
    procedure SetInRepaint(const Value: Boolean); virtual; abstract;
    procedure SetInUpdate(const Value: Boolean); virtual; abstract;
    procedure SetIsAerial(const Value: Boolean); virtual; abstract;
    procedure SetNoPickFilter(const Value: TlicgEntityIDs); virtual; abstract;
    procedure SetOutputDevices(const Value: TlicgOutputDevices); virtual; abstract;
    procedure SetPartialSelect(const Value: Boolean); virtual; abstract;
    //procedure SetRubberPen(const Value: TPen);virtual; abstract;
    procedure SetShowControlPoints(const Value: Boolean); virtual; abstract;
    procedure SetShowTransfPoints(const Value: Boolean); virtual; abstract;
    procedure SetSnapToGuidelines(const Value: Boolean); virtual; abstract;
    procedure SetSnapToGuidelinesDist(const Value: Double); virtual; abstract;
    procedure SetStackedSelect(const Value: Boolean); virtual; abstract;
    procedure SetSymbolMarker(const Value: Integer); virtual; abstract;
  //  procedure SetTileBitmap(const Value: Tlicgitmap);virtual; abstract;
    procedure SetUseThreadOnResize(const Value: Boolean); virtual; abstract;
    procedure SetUseThreadOnScroll(const Value: Boolean); virtual; abstract;
    procedure SetZoomWithMargins(const Value: Boolean); virtual; abstract;
    procedure DrawGrid(const WCRect: TlicgExtent; ACanvas: IlicgCanvas = nil);
      virtual; abstract;
    function GetLayerByName(const LayerName: string): TlicgBaseLayer; virtual; abstract;
    procedure HChange; virtual; abstract;
    procedure HScroll(ScrollCode: TScrollCode; var ScrollPos: Integer); virtual; abstract;
    function IsDesigning: Boolean; virtual; abstract;
    procedure UpdateViewport(WCRect: TlicgExtent; UseThread: Boolean = False);
      virtual; abstract;
    procedure VChange; virtual; abstract;
    procedure ViewChanged(Sender: TObject); virtual;
    procedure VScroll(ScrollCode: TScrollCode; var ScrollPos: Integer); virtual; abstract;
    procedure RepaintThread(WCRect: TlicgExtent; Region: HRgn; aCanvas: TCanvas);
      virtual; abstract;
    function GetDrawBoxType: TDrawBoxType; virtual; abstract;
    function GetUndoSave: TlicgBaseUndoPool; virtual; abstract;
    procedure SetUndoSave(Value: TlicgBaseUndoPool); virtual; abstract;
    function GetGIS: TlicgBaseGIS; virtual; abstract;
    procedure SetGIS(Value: TlicgBaseGIS); virtual; abstract;
    function GetRubberBandList: TlicgRubberList; virtual; abstract;
    function GetRegionWork: IRegionWork; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateBarThematic(const LayerName, FieldNames: string; Fill:
      Boolean; Width, Height: Double; StartColor: Integer);
    procedure CreateDotThematic(const Layername, FieldName: string; Fill:
      Boolean; DotRange: Double; Dotsize, DotColor: Integer);
    procedure CreatePieThematic(const LayerName, FieldNames: string; Fill:
      Boolean; Radian: Double; StartColor: Integer);
    procedure AddMarker(const X, Y: Double; SetInView: Boolean; UseThread:
      Boolean = False); virtual; abstract;
    procedure BeginRepaint; virtual; abstract;
    procedure BeginUpdate; virtual; abstract;
    procedure CancelUpdate; virtual; abstract;
    procedure ForceEndUpdate; virtual; abstract;
    procedure EndRepaint; virtual; abstract;
    procedure EndUpdate(UseThread: Boolean = False); virtual; abstract;
    procedure BlinkEntities; virtual; abstract;
    procedure BlinkEntity(Entity: IlicgEntity); virtual; abstract;
    procedure BlinkEntityEx(const LayerName: string; RecNo: Integer); virtual; abstract;
    procedure BlinkThis(const X, Y: Double); virtual; abstract;
    procedure BringTofront(UseThread: Boolean = False); virtual; abstract;
    procedure MoveForward(UseThread: Boolean = False); virtual; abstract;
    procedure SendBackwards(UseThread: Boolean = False); virtual; abstract;
    procedure SendToBack(UseThread: Boolean = False); virtual; abstract;
    procedure ClearViewsHistory; virtual; abstract;
    procedure ClipPolylineAgainstPolygon; virtual; abstract;
    function CoorsToDisplaySuffix(const AX, AY: Double): string; virtual; abstract;
    function CoorsToDisplayText(const AX, AY: Double): string; virtual; abstract;
    procedure CopyToClipboardAsBMP; virtual; abstract;
    procedure CopyToClipboardAsEMF; virtual; abstract;
    function CreateEntity(EntType: TlicgEntityID): IlicgEntity; virtual; abstract;
    function CreateEntityFromText(const EntityText: Ansistring): IlicgEntity; virtual; abstract;
    procedure SaveAsImageToStream(const ALeft, ATop, ARight, ABottom: Double; AImageType: TlicgImageType; AJpegQuality: Cardinal; out AMemoryStream: TMemoryStream); virtual; abstract;

    procedure CreateThumbNail(W: Integer = 200); overload; virtual; abstract;
    procedure CreateThumbNail(W, H: Integer; fn: string; _Transparent: Boolean =
      false; _TransparentColor: TColor = clNone); overload; virtual; abstract;
    procedure CreateGoogleThumbNailByPolyWithOrderLabels(W, H: Integer; fn:
      string; _Entities: IlicgEntityList); virtual; abstract;
    procedure CurrentExtents(var AXMin, AYMin, AXMax, AYMax: Double); virtual; abstract;
    function GetLastSelection: TlicgBaseSelection; virtual; abstract;
    procedure DeleteSelection(ACanUndo: Boolean = True; ABeginEndUndo: Boolean = True); virtual; abstract;
    procedure ClearCanvas(Canvas: IlicgCanvas; ARect: TRect; BackCol: TColor); virtual; abstract;
    procedure DoCopyCanvas(ACanvas: IlicgCanvas); virtual; abstract;
    procedure DoFullPainting(const WCRect: TlicgExtent; Canvas: IlicgCanvas; Grapher: TlicgBaseGrapher); virtual; abstract;
    procedure DoSelectLayer(Layer: TlicgBaseLayer; var Canceled: Boolean); virtual; abstract;
    procedure DrawBoxToWorld(X, Y: Integer; var WX, WY: Double); virtual; abstract;
    procedure DrawControlPointsWithRubber(Ent: IlicgEntity); virtual; abstract;
    procedure DrawCross(ACanvas: IlicgCanvas; pt: TPoint; CrossSize: Integer = 0); virtual; abstract;
    procedure DrawEntity(Ent: IlicgEntity; DrawMode: TlicgDrawMode = dmNormal;
      ACanvas: IlicgCanvas = nil); virtual; abstract;
    procedure DrawEntity2D(Ent: IlicgEntity; CtrlPts: Boolean; DrawMode:
      TlicgDrawMode = dmNormal); virtual; abstract;
    procedure DrawEntity2DRubberBand(Entity: IlicgEntity; CtrlPts: Boolean = False; TransfPts: Boolean = False;
      rubberColor: TColor = clNone; ADrawMode: TlicgDrawMode = dmRubber); virtual; abstract;
    procedure DrawEntityRubberBand(Ent: IlicgEntity; rubberColor: TColor = clNone;
      ADrawMode: TlicgDrawMode = dmRubber); virtual; abstract;
    procedure DrawPie(const Drawlayer, FieldName: string); virtual; abstract;
    procedure DropSelectionAt(const Wx, Wy: Double); virtual; abstract;
    procedure ExpressionSelect(const Layername, Expression: string); virtual; abstract;
    function FindClosestEntity(const WX, WY: Double; const Scope: string; const
      ShapeType: TlicgEntityIDs; var LayerName: string; var RecNo: Integer):
      Boolean; overload; virtual; abstract;
    function FindClosestEntity(const WX, WY: Double; const Scope: string; const
      ShapeType: TlicgEntityIDs; Layer: TlicgBaseLayer; var RecNo: Integer):
      Boolean; overload; virtual; abstract;
    function CalculateElevation(const X, Y: Double): Double; virtual; abstract;
    function CalculateElevationFromTriangle(const X, Y: Double): Double; virtual; abstract;

    //Group type eklendi. Hatch için gtHatch geçilecek.
    procedure GroupSelection(groupType: TlicgGroupType = grtNone); virtual; abstract;
    procedure JoinSelection(DeleteOriginals: Boolean); virtual; abstract;
    procedure MovePlace(const LayerName: string; RecNo: Integer; const NewX,
      NewY: Double); virtual; abstract;
    procedure MoveWindow(const NewStartX, NewStartY: Double; UseThread: Boolean
      = False); virtual; abstract;
    procedure Panning(const DeltaX, DeltaY: Double; UseThread: Boolean = False);
      virtual; abstract;
    function PickEntity(const X, Y: Double; Aperture: Integer; const LayerName:
      string; var NLayer: TlicgBaseLayer; var NRecNo, NPoint: Integer; List:
      IPickedList; IncludeCSLayers: Boolean = True; CanDoOSNAP: Boolean = False): Boolean; virtual; abstract;
    procedure Refresh; virtual; abstract;
    procedure RefreshExtent(const WCRect: TlicgExtent); virtual; abstract;
    procedure RegenDrawing(UseThread: Boolean = False); virtual; abstract;
    procedure RepaintExtent(const WCRect: TlicgExtent; UseThread: Boolean = False); virtual; abstract;
    procedure RepaintExtentEx(const WCRect: TlicgExtent; UseThread: Boolean = False); virtual; abstract;
    procedure ReSync; virtual; abstract;
    procedure SaveAsBMP(const FileName: string); virtual; abstract;
    procedure SaveAsEMF(const FileName: string; AWidth: Integer = 0; AHeight: Integer = 0); virtual; abstract;
    procedure SaveAsJPG(const FileName: string); virtual; abstract;
    procedure SaveClippedAreaTo(NewGis: TlicgBaseGIS); virtual; abstract;
    procedure SelectAll(AClear: Boolean = True); virtual; abstract;
    procedure SelectionChangeDirection; virtual; abstract;
    procedure SelectionToPolygon; virtual; abstract;
    procedure SelectionToPolyline; virtual; abstract;
    procedure SetClipBoundaryFromSelection; virtual; abstract;
    procedure SetEntityInView(Entity: IlicgEntity; UseExtents: Boolean;
      DoRepaint: Boolean = True); virtual; abstract;
    procedure SetEntityInViewEx(const LayerName: string; RecNo: Integer;
      UseExtents: Boolean; DoRepaint: Boolean = True); virtual; abstract;
    procedure SetViewTo(const AXMin, AYMin, AXMax, AYMax: Double; UseThread:
      Boolean = False); virtual; abstract;
    procedure UnGroupSelection; virtual; abstract;
    procedure UnSelectAll(UseThread: Boolean = False); virtual; abstract;
    procedure WorldToDrawBox(const WX, WY: Double; var X, Y: Integer); virtual; abstract;
    procedure ZoomIn(var Handled: Boolean; percent: byte; X: Integer = MaxInt; Y:
      Integer = MaxInt; UseThread: Boolean = False); virtual; abstract;
    procedure ZoomOut(var Handled: Boolean; percent: byte; X: Integer = MaxInt;
      Y: Integer = MaxInt; UseThread: Boolean = False); virtual; abstract;
    procedure ZoomPrevious(UseThread: Boolean = False); virtual; abstract;
    procedure ZoomToExtension(UseThread: Boolean = False); virtual; abstract;
    procedure ZoomToHorzDistance(const Distance: Double; UseThread: Boolean = False); virtual; abstract;
    procedure ZoomToLayer(const ALayer: TlicgBaseLayer; AUseThread: Boolean = False); virtual; abstract;
    procedure ZoomToLayerByName(const ALayerName: string; AUseThread: Boolean = False); virtual; abstract;
    procedure ZoomToScale(const Scale: Double; ScaleUnits: TlicgScaleUnits; UseThread: Boolean = False); virtual; abstract;
    procedure ZoomToSelection(UseThread: Boolean = False); virtual; abstract;
    procedure ZoomWindow(const NewWindow: TlicgExtent; DecZoomLevel: Integer = 0; UseThread: Boolean = False); virtual; abstract;
    procedure ZoomToEntity(const E: IlicgEntity); virtual; abstract;
    procedure RepaintInThread; virtual; abstract;
    procedure StopRepaintingThread; virtual; abstract;
    procedure doAdd2RubberBandList(ACanvas: IlicgCanvas; IsDisabled: Boolean;
      Entity: IlicgEntity; ShowControlPts, ShowTransformPts: Boolean; PenColor:
      TColor; PenWidth: Byte; PenStyle: TPenStyle); virtual; abstract;
    procedure InvalidateIfRubberList; virtual; abstract;
    procedure RefreshIfRubberList; virtual; abstract;
    procedure Add2RubberBandList(ACanvas: IlicgCanvas; IsDisabled: Boolean;
      Entity: IlicgEntity; ShowControlPts, ShowTransformPts: Boolean; PenColor:
      TColor; PenWidth: Byte; PenStyle: TPenStyle); overload; virtual; abstract;
    procedure Add2RubberBandList(ACanvas: IlicgCanvas; Entity: IlicgEntity); overload; virtual; abstract;
    procedure Add2AlwaysDisplayList(Entity: IlicgEntity); virtual; abstract;
    procedure Add2RefOnceDisplayList(Layer: TlicgBaseLayer; RecNo: Integer); virtual; abstract;
    procedure Add2EntityOnceDisplayList(Entity: IlicgEntity); virtual; abstract;
    procedure ClearOnceDisplayList; virtual; abstract;
    procedure ClearRefOnceDisplayList; virtual; abstract;
    procedure ClearAlwaysDisplayList; virtual; abstract;
    procedure ClearOnceList; virtual; abstract;
    //Seçilen nesnelerden hat üstünde olanı geri döndürür.
    function GetClosestPickedEntity(Aperture: Double; const WX, WY: Double;
      const ShapeType: TlicgEntityIDs; var LayerName: string; var RecNo: Integer):
      Boolean; virtual; abstract;
    function CurrentScale: Double; virtual; abstract;
    function IsGeographicCS: Boolean; virtual;
    procedure SaveGIS(isUndoClear: Boolean = True; AAddRecentProjects: Boolean = True); virtual; abstract;
    function isViewChanged: Boolean; virtual;
    function ChangeGrapher(GrapherType: Integer): TlicgTransformParams; virtual; abstract;
    function GetShowGoogleMapRaster: Boolean; virtual; abstract;
    procedure SetShowGoogleMapRaster(Value: Boolean); virtual; abstract;
    property ShowGoogleMapRaster: Boolean read GetShowGoogleMapRaster write SetShowGoogleMapRaster;
    function GetMapTileSource: Integer; virtual; abstract;
    procedure SetMapTileSource(Value: Integer); virtual; abstract;
    function GetEntityAlwaysDisplayList: IlicgEntityList; virtual; abstract;

    //if box is Symbolbox
    procedure CreateNewEditor(UseCadObject: Boolean = true); virtual; abstract;
    procedure PopulateFrom(Symbol: IUnknown); virtual; abstract;
    procedure PopulateTo(Symbol: IUnknown); virtual; abstract;
    function GetInsertionPoint: TlicgCoor; virtual; abstract;
    procedure SetInsertionPoint(Value: TlicgCoor); virtual; abstract;
    property OwnerCmdline: TObject read FCmdline write FCmdline;
    property LastSelection: TlicgBaseSelection read GetLastSelection;
    property MapTileSource: Integer read GetMapTileSource write SetMapTileSource;
    property GIS: TlicgBaseGIS read GetGIS write SetGIS;
    property RegionWork: IRegionWork read GetRegionWork;
    property AlwaysDisplayListDrawMode: TlicgDrawMode read
      GetAlwaysDisplayListDrawMode write SetAlwaysDisplayListDrawMode default dmSelection;
    property BlinkCount: Integer read GetBlinkCount write SetBlinkCount default 3;
    property BlinkRate: Integer read GetBlinkRate write SetBlinkRate default 100;
    property DefaultScaleUnits: TlicgScaleUnits read GetDefaultScaleUnits write SetDefaultScaleUnits;
    property DelayShowHint: Integer read GetDelayShowHint write SetDelayShowHint default 200;
    property DropRepeat: Integer read GetDropRepeat write SetDropRepeat default 1;
    property FlatScrollBar: Boolean read GetFlatScrollBar write SetFlatScrollBar;
    property Grapher: TlicgBaseGrapher read GetGrapher;
    property GridInfo: TlicgGridInfo read GetGridInfo write SetGridInfo;
    property InRepaint: Boolean read GetInRepaint write SetInRepaint;
    property InUpdate: Boolean read GetInUpdate write SetInUpdate;
    property IsAerial: Boolean read GetIsAerial write SetIsAerial;
    property NoPickFilter: TlicgEntityIDs read GetNoPickFilter write SetNoPickFilter;
    property OutputDevices: TlicgOutputDevices read GetOutputDevices write SetOutputDevices default[odBitmap];
    property PartialSelect: Boolean read GetPartialSelect write SetPartialSelect default False;
    property RubberPenColor: TColor read GetRubberPenColor write SetRubberPenColor default clBlack;
    property RubberPenMode: TPenMode read GetRubberPenMode write SetRubberPenMode default pmCopy;
    property RubberPenStyle: TPenStyle read GetRubberPenStyle write SetRubberPenStyle default psSolid;
    property RubberPenWidth: Integer read GetRubberPenWidth write SetRubberPenWidth default 1;
    property ScreenBitmap: TBitmap read GetScreenBitmap;
    property ScreenGrid: TlicgScreenGrid read GetScreenGrid write SetScreenGrid;
    property ScrollBars: TScrollStyle read GetScrollBars write SetScrollBars default ssBoth;
    property Selection: TlicgBaseSelection read GetSelection;
    property ShowControlPoints: Boolean read GetShowControlPoints write SetShowControlPoints default False;
    property ShowLayerExtents: Boolean read GetShowLayerExtents write SetShowLayerExtents;
    property ShowMapExtents: Boolean read GetShowMapExtents write SetShowMapExtents;
    property ShowTransfPoints: Boolean read GetShowTransfPoints write SetShowTransfPoints default False;
    property SnapToGuidelines: Boolean read GetSnapToGuidelines write SetSnapToGuidelines default False;
    property SnapToGuidelinesDist: Double read GetSnapToGuidelinesDist write SetSnapToGuidelinesDist;
    property StackedSelect: Boolean read GetStackedSelect write SetStackedSelect default True;
    property SymbolMarker: Integer read GetSymbolMarker write SetSymbolMarker default 0;
    property TempEntities: IlicgEntityList read GetTempEntities;
    property ThematicSeriesColors: TlicgThematicSeriesColor read GetThematicSeriesColors;
 //   property TileBitmap: Tlicgitmap read GetTileBitmap write SetTileBitmap;
    property Undo: TlicgBaseUndoPool read GetUndo;
    property Redo: TlicgBaseRedoPool read GetRedo;
    property ZoomWithMargins: Boolean read GetZoomWithMargins write SetZoomWithMargins;
    property IsThreadRunning: Boolean read GetIsThreadRunning write SetIsThreadRunning;
    property Canvas;
    property OnAfterScroll: TNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnAfterSelect: TlicgEntitySelectEvent read FOnAfterSelect write FOnAfterSelect;
    property OnAfterUnSelect: TlicgEntitySelectEvent read FOnAfterUnSelect write FOnAfterUnSelect;
    property OnBeforeScroll: TNotifyEvent read FOnBeforeScroll write FOnBeforeScroll;
    property OnBeforeSelect: TlicgBeforeSelectEvent read FOnBeforeSelect write FOnBeforeSelect;
    property OnBeginRepaint: TNotifyEvent read FOnBeginRepaint write FOnBeginRepaint;
    property OnCustomClick: TlicgClickEvent read FOnCustomClick write FOnCustomClick;
    property OnEndRepaint: TNotifyEvent read FOnEndRepaint write FOnEndRepaint;
    property OnEntityDblClick: TlicgEntityDblClickEvent read FOnEntityDblClick write FOnEntityDblClick;
    property OnGisChanged: TNotifyEvent read FOnGisChanged write FOnGisChanged;
    property OnGridError: TNotifyEvent read FOnGridError write FOnGridError;
    property OnHChange: TNotifyEvent read FOnHChange write FOnHChange;
    property OnHScroll: TScrollEvent read FOnHScroll write FOnHScroll;
    property OnInternalUseMouseMove: TlicgMouseMoveEvent read FOnInternalUseMouseMove write FOnInternalUseMouseMove;
    property OnMouseDown2D: TlicgMouseEvent read FOnMouseDown2D write FOnMouseDown2D;
    property OnMouseMove2D: TlicgMouseMoveEvent read FOnMouseMove2D write FOnMouseMove2D;
    property OnMouseUp2D: TlicgMouseEvent read FOnMouseUp2D write FOnMouseUp2D;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnShowHint: TlicgShowHintEvent read FOnShowHint write FOnShowHint;
    property OnVChange: TNotifyEvent read FOnVChange write FOnVChange;
    property OnVScroll: TScrollEvent read FOnVScroll write FOnVScroll;
    property OnZoomChange: TlicgZoomChangeEvent read FOnZoomChange write FOnZoomChange;
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle default bsSingle;
    property Height default 100;
    property HideVertexNumber: Boolean read GetHideVertexNumber write SetHideVertexNumber default False;
    property UseThreadOnResize: Boolean read GetUseThreadOnResize write SetUseThreadOnResize default False;
    property UseThreadOnScroll: Boolean read GetUseThreadOnScroll write SetUseThreadOnScroll default False;
    property UseThreads: Boolean read GetUseThread write SetUseThreads default True;
    property ParentFont;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop default True;
    property UpdateOnScroll: Boolean read FUpdateOnScroll write FUpdateOnScroll default False;
    property Visible;
    property Width default 100;
    property OnRepaintThreadFinished: TNotifyEvent read FOnRepaintThreadFinished write FOnRepaintThreadFinished;
    property OnSelectWhenReshaping: TlicgSelectWhenReshaping read FOnSelectWhenReshaping write FOnSelectWhenReshaping;
    property OnRegenDrawingForSomeActions: TNotifyEvent read FOnRepaintThreadFinished write FOnRepaintThreadFinished;
    property Align;
    property Color;
    property Ctl3D;
    property Enabled;
    property DragCursor;
    property OnSystemKeyDown: TSystemKeyDownEvent read FSystemKeyDownEvent write FSystemKeyDownEvent;
    property InsertPointWhenLineDraw: Boolean read FInsertPointWhenLineDraw write FInsertPointWhenLineDraw default False;
    property DrawBoxType: TDrawBoxType read GetDrawBoxType;
    property FarOnchangeCurrProject: TFarOnchangeCurrProject read GetFarOnchangeCurrProject write SetFarOnchangeCurrProject;
    property UndoSave: TlicgBaseUndoPool read GetUndoSave write SetUndoSave;
    property RubberBandList: TlicgRubberList read GetRubberBandList;
    property MovePoint: TPoint read FMovePoint write FMovePoint;
    property EntityAlwaysDisplayList: IlicgEntityList read GetEntityAlwaysDisplayList;
  end;

  IlicgPainterObject = interface
    ['{AAA541FC-AE5E-4D8A-ABF2-210ABD8C27C7}']
    function GetInThread: Boolean; stdcall;
    property InThread: Boolean read GetInThread;
    function GetSourceGis: TlicgBaseGIS; stdcall;
    property SourceGis: TlicgBaseGIS read GetSourceGis;
    function GetWasUpdated: Boolean; stdcall;
    property WasUpdated: Boolean read GetWasUpdated;
    function GetTerminated: Boolean; stdcall;
    procedure SetTerminated(const Value: Boolean); stdcall;
    property Terminated: Boolean read GetTerminated write SetTerminated;
    function GetTickStart: DWord; stdcall;
    procedure SetTickStart(const Value: DWord); stdcall; stdcall;
    property TickStart: DWord read GetTickStart write SetTickStart;
    function isCancelDraw: Boolean; stdcall;
    procedure DrawEntities(WCRect: TlicgExtent; AGIS: TlicgBaseGIS; Canvas:
      IlicgCanvas; dGrapher: TlicgBaseGrapher; Selection: TlicgBaseSelection;
      IsAerial: Boolean; ShowAnimationLayers: Boolean; PrintMode: TlicgPrintMode;
      BufferBitmap: TBitmap; ClipRgn: HRgn; PreviewDraw: Boolean; CurrentScale:
      double = 0; PMatrix: PlicgMatrix = nil); stdcall;
    procedure DrawRasters(WCRect: TlicgExtent; AGIS: TlicgBaseGIS; _Canvas:
      IlicgCanvas; Grapher: TlicgBaseGrapher; ClipRgn: HRgn; PreviewDraw:
      Boolean; var CancelDraw: Boolean; CurrentScale: double = 0; PMatrix:
      PlicgMatrix = nil); stdcall;
  end;

function CheckComctlVersion: Boolean;

const
  LICADGISDATA = 'CF_LICADGIS'; // constant to register the format to the clipboard

var
  CF_LICADGISDATA: Word;

function convertPointsProjection(Gis: TlicgBaseGis; InPrj, OutPrj: IlicgCS;
  OntheFlyMatrixElems: PAffineMatrixElements): Boolean;

const
  CtrlStyle =[csAcceptsControls, csClickEvents, csCaptureMouse, csPaintBlackOpaqueOnGlass, csDoubleClicks];

function FindClosestEntity(const GIS: TlicgBaseGis; const WX, WY: Double; const
  Scope: string; const ShapeType: TlicgEntityIDs; Layer: TlicgBaseLayer; var
  RecNo: Integer): Boolean;

function AslicgPainterObject(const Int: IInterface): IlicgPainterObject;

function AslicgDrawTools(const Int: IInterface): IlicgDrawTools;

procedure _PopulateFieldList(Layer: TlicgBaseLayer; Strings: TStrings);

procedure CBRegenDrawing(AGIS: TObject; DrawParam: Integer); stdcall;

implementation

uses
  Math,
  Clipbrd,
  Consts,
  CommCtrl,
  Types,
  TypInfo,
  Lider.CG.Com.Math,
  Lider.CG.Com.UtilityInt,
  Lider.CG.Com.Geolibrary,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Graphics,
  Lider.CG.Com.Thematics;

procedure CBRegenDrawing(AGIS: TObject; DrawParam: Integer); stdcall;
var
  i: Integer;
begin
  if Assigned(AGIS) then
  begin
    (TlicgBaseGIS(AGIS).ReferenceGIS as IlicgReferenceGIS).ClearCaches;
    for i := 0 to TlicgBaseGIS(AGIS).DrawBoxList.Count - 1 do
    begin
      case DrawParam of
        0:
          TlicgBaseGIS(AGIS).DrawBoxList.Items[i].RegenDrawing;
        1:
          TlicgBaseGIS(AGIS).DrawBoxList.Items[i].RefreshExtent(TlicgBaseGIS(AGIS).DrawBoxList.Items
            [i].Grapher.CurrentParams.VisualWindow);
        2:
          TlicgBaseGIS(AGIS).DrawBoxList.Items[i].RepaintExtent(TlicgBaseGIS(AGIS).DrawBoxList.Items
            [i].Grapher.CurrentParams.VisualWindow);
      end;
    end;

  end;
end;

function AslicgPainterObject(const Int: IInterface): IlicgPainterObject;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgPainterObject, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgDrawTools(const Int: IInterface): IlicgDrawTools;
begin
  result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgDrawTools, result) <> 0) then
      result := nil;
  end;
end;

function CheckComctlVersion: Boolean;
var
  hInst: THandle;
begin
  hInst := LoadLibrary('comctl32.dll');
  if hInst < 32 then
    Result := False
  else
    //only the new version of comctl32.dll has this function
    Result := GetProcAddress(hInst, 'DllGetVersion') <> nil;
  FreeLibrary(hInst);
end;

{ TlicgEntityList }

constructor TlicgEntityList.Create;
begin
  inherited Create;
  FList := TInterfaceList.Create;
  FRecNoList := TList<Integer>.Create;
end;

destructor TlicgEntityList.Destroy;
begin
  Clear;
  FList := nil;
  FRecNoList.Free;
  inherited Destroy;
end;

function TlicgEntityList.GetList: IInterfaceList;
begin
  Result := FList;
end;

function TlicgEntityList.GetRecNoItems(Index: Integer): Integer;
begin
  Result := -1;
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  Result := FRecNoList[Index];
end;

procedure TlicgEntityList.SetRecNoItems(Index, Value: Integer);
begin
  FRecNoList[index] := Value;
end;

function TlicgEntityList.Add(Ent: IlicgEntity): Integer;
begin
  Result := FList.Add(Ent);
end;

function TlicgEntityList.AddEntity(AEntity: IlicgEntity; ARecNo: Integer): Integer;
begin
  Result := FList.Add(AEntity);
  FRecNoList.Add(ARecNo);
end;

function TlicgEntityList.AddIfNotFound(Ent: IlicgEntity): Integer;
var
  Index: Integer;
begin
  Result := -1;
  Index := FList.IndexOf(Ent);
  if Index < 0 then
    Result := FList.Add(Ent);
end;

procedure TlicgEntityList.Clear;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
  begin
    // Use Delete to properly decrease reference counts and free owned interfaces
    FList.Delete(I);
  end;
  FRecNoList.Clear;

//  for I := FList.Count - 1 downto 0 do
//    FList[I] := nil;
//  FList.Clear;
//  FRecNoList.Clear;
end;

function TlicgEntityList.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TlicgEntityList.Delete(Index: Integer);
begin
  FList[Index] := nil;
  FList.Delete(Index);
  if FRecNoList.Count > 0 then
    FRecNoList.Delete(Index);
end;

procedure TlicgEntityList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
  if FRecNoList.Count > 0 then
    FRecNoList.Exchange(Index1, Index2);
end;

procedure TlicgEntityList.Extract(Index: Integer);
begin
  FList.Delete(Index);
  if FRecNoList.Count > 0 then
    FRecNoList.Delete(Index);
end;

procedure TlicgEntityList.ExtractAll;
begin
  FList.Clear;
  FRecNoList.Clear;
end;

function TlicgEntityList.Get(Index: Integer): IlicgEntity;
begin
  Result := nil;
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  Result := FList[Index] as IlicgEntity;
end;

procedure TlicgEntityList.Insert(Index: Integer; Ent: IlicgEntity);
begin
  FList.Insert(Index, Ent);
end;

procedure TlicgEntityList.Replace(Index: Integer; Ent: IlicgEntity);
begin
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  FList[Index] := nil;
  FList[Index] := Ent;
end;

function TlicgEntityList.IndexOf(Item: IlicgEntity): Integer;
begin
  Result := FList.IndexOf(Item);
end;

{ TlicgBaseLayerInfo }

constructor TlicgBaseLayerInfo.Create(Layer: TlicgBaseLayer);
begin
  inherited Create;
  FLayer := Layer;
end;

destructor TlicgBaseLayerInfo.Destroy;
begin
  // these objects are destroyed here
  FDefSymbolTool := nil;
  FDefPenTool := nil;
  FDefBrushTool := nil;
  FDefFontToolTT := nil;
  FDefFontTool := nil;
  inherited;
end;

procedure TlicgBaseLayerInfo.Assign(Source: TlicgBaseLayerInfo);
begin
  Extension := Source.Extension;
  UseAttachedDB := Source.UseAttachedDB;
  IsCosmethic := Source.IsCosmethic;
  IsAnimationLayer := Source.IsAnimationLayer;
  IsIndexed := Source.IsIndexed;
  CS := Source.CS;
  Visible := Source.Visible;
  Selectable := Source.Selectable;
  TextFixed := Source.TextFixed;
  TextFixedSize := Source.TextFixedSize;
  Locked := Source.Locked;

  IsLayerBrush := Source.IsLayerBrush;
  Balastro := Source.Balastro;
  PointNameOnOFF := Source.PointNameOnOFF;
  PolygonNameOnOFF := Source.PolygonNameOnOFF;
  PrintOnOFF := Source.PrintOnOFF;
  PenNo := Source.PenNo;
  ShowArrow := Source.ShowArrow;
  ZDisplayOnOff := Source.ZDisplayOnOff;
  ShowDBInfoOnCreateEnt := Source.ShowDBInfoOnCreateEnt;

  DefSymbolTool.Assign(Source.DefSymbolTool);
  DefPenTool.Assign(Source.DefPenTool);
  DefBrushTool.Assign(Source.DefBrushTool);
  DefFontToolTT.Assign(Source.DefFontToolTT);
  DefFontTool.Assign(Source.DefFontTool);

  ApplyDefSymbol := Source.ApplyDefSymbol;
  ApplyDefPen := Source.ApplyDefPen;
  ApplyDefBrush := Source.ApplyDefBrush;
  ApplyDefFontTT := Source.ApplyDefFontTT;
  ApplyDefFont := Source.ApplyDefFont;
end;

{ TlicgBaseLayer }

function TlicgBaseLayer.AddEntity(Entity: IlicgEntity; ApplyLayerProps,
  GetfirstPropFromLayer: Boolean; _PropVList: IUnknown): Integer;
begin
  Result := -1;
end;

function TlicgBaseLayer.Bof: Boolean;
begin
  Result := false;
end;

function TlicgBaseLayer.BringEntityTofront(ARecNo: Integer): Integer;
begin
  Result := -1;
end;

procedure TlicgBaseLayer.ClearBlinkers;
begin
  Blinkers := nil;
end;

procedure TlicgBaseLayer.Close;
begin
  if Assigned(GIS.OnBeforeCloseLayer) then
    GIS.OnBeforeCloseLayer(GIS, Self.Name);
end;

function TlicgBaseLayer.ContainsDeleted: Boolean;
begin
  Result := false;
end;

constructor TlicgBaseLayer.Create(AGIS: TlicgBaseGIS; const _FileName: string; IsAddToGis: Boolean = True);
begin
  inherited Create;
  if Assigned(AGIS) then
  begin
    GIS := AGIS;
    if (Length(_FileName) > 0) and (AGIS.Layers.IndexofName(ExtractFileName(_FileName)) < 0) and isAddToGis then
    begin
      AGIS.Layers.AddLayer(Self);
    end;

    FileName := ChangeFileExt(_FileName, '');
    Name := FileName;
    SpaceKeySelected := True;
    DisplayName := ExtractFileName(ChangeFileExt(FileName, ''));
  end;
end;

function TlicgBaseLayer.DefinePolygonScope(Polygon: IlicgEntity; const Scope:
  string; operator: TlicgGraphicOperator; _oList: IlicgIntegerList = nil;
  NoPickFilter: TlicgEntityIDs = []): Boolean;
var
  cs: double;
begin
  Result := True;

  (*
  if (_GIS <> nil) then
  begin
    if (Self.LayerInfo.ZoomRangeActive) and (Self.LayerInfo.MaxZoomScale > Self.LayerInfo.MinZoomScale) then
    begin
      cs := _GIS.CurrentScale;
      Result := (cs >= Self.LayerInfo.MinZoomScale) and (cs <= Self.LayerInfo.MaxZoomScale);
    end;
  end;  *)
end;


function TlicgBaseLayer.DefinePolygonScope(Polygon: IlicgEntity; const Scope:
  string; operator: TlicgGraphicOperator; _Sel: TlicgBaseSelection): Boolean;
var
  cs: double;
begin
  Result := True;

  (*
  if (_GIS <> nil) then
  begin
    if (Self.LayerInfo.ZoomRangeActive) and (Self.LayerInfo.MaxZoomScale > Self.LayerInfo.MinZoomScale) then
    begin
      cs := _GIS.CurrentScale;
      Result := (cs >= Self.LayerInfo.MinZoomScale) and (cs <= Self.LayerInfo.MaxZoomScale);
    end;
  end;  *)
end;

function TlicgBaseLayer.DefineScope(const Scope: string): Boolean;
begin
  Result := false;
end;

function TlicgBaseLayer.DeleteLayerFiles: Boolean;
begin
  Result := False;
  if not LayerInfo.Locked then
    Result := Lider.CG.Com.System.DeleteLayerFiles(Self.FileName);
end;

destructor TlicgBaseLayer.Destroy;
begin
  if (GIS <> nil) then
  begin
    if Assigned(GIS.Layers) then
    begin
      if GIS.Layers.IndexOf(self) > -1 then
        GIS.Layers.RemoveLayer(Self);
    end;
  end;

  if (Ord(Self.LayerClassType)) in [0..5] then
  begin
    if Active then
      Close;

    if Assigned(FLayerInfo) then
    begin
      FLayerInfo.Free;
    end;
    Blinkers := nil;
    GIS := nil;
  end;
  inherited Destroy;
end;

{ TlicgBaseLayers }

function TlicgBaseLayers.AddLayer(Layer: TlicgBaseLayer): Integer;
begin
  Result := FItems.Add(Layer);
end;

function TlicgBaseLayers.BringToTop(Index: Integer): Integer;
var
  I: Integer;
  Temp: Pointer;
begin
  Result := 0;
  if (Index < 0) or (Index >= Count - 1) then
    Exit;
  Temp := FItems[Index];
  for I := Index to Count - 2 do
    FItems[I] := FItems[I + 1];
  FItems[Count - 1] := Temp;
  Result := Count - 1;
end;

procedure TlicgBaseLayers.ChangePosition(fromIndex, toIndex: Integer);
var
  temp: TlicgBaseLayer;
begin
  if (fromIndex = toIndex) or (fromIndex < 0) or (fromIndex > FItems.Count - 1)
    or (toIndex < 0) or (toIndex > FItems.Count - 1) then
    Exit;
  temp := TlicgBaseLayer(FItems[fromIndex]);
  if (toIndex > fromIndex) then
  begin
    FItems.Insert(toIndex, temp);
    FItems.Delete(fromIndex);
  end
  else
  begin
    FItems.Delete(fromIndex);
    FItems.Insert(toIndex, temp);
  end;
end;

procedure TlicgBaseLayers.Clear;
var
  I, n: Integer;
begin
  n := FItems.Count - 1;
  for I := 0 to n do
    TlicgBaseLayer(FItems[0]).Free;
  FItems.Clear;
end;

constructor TlicgBaseLayers.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TlicgBaseLayers.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

{ TlicgBaseTable }

constructor TlicgBaseTable.Create(Gis: TlicgBaseGIS; const FName: string;
  ReadWrite, Shared: Boolean);
var
  AFileName: string;
begin
  inherited Create;
  FGIS := Gis;
  if Length(ExtractFilePath(FName)) = 0 then
    Exit;
  { now read virtual fields }
  AFileName := ChangeFileExt(FName, EXT_LONGFIELD);
  if not FileExists(AFileName) then
    Exit;
end;

constructor TlicgBaseTable.CreateNoOpen(Gis: TlicgBaseGIS);
begin
  inherited create;
  FGIS := Gis;
 // FLongFieldNames := TStringList.Create;
end;

destructor TlicgBaseTable.Destroy;
begin
  //if FLongFieldNames<>nil then  FLongFieldNames.Free;
  inherited Destroy;
end;

function TlicgBaseTable.GetGis: TlicgBaseGIS;
begin
  Result := FGIS;
end;

function TlicgBaseTable.GetLongFieldNames: TStrings;
begin
  Result := nil; //FLongFieldNames;
end;

procedure TlicgBaseTable.SetGis(const Value: TlicgBaseGIS);
begin
  FGIS := Value;
end;

(*
procedure TlicgBaseTable.AssignFrom(Dataset: TlicgBaseTable; SrcFieldNo,
  DstFieldNo: Integer);
var
  SrcTyp, DstTyp: Char;
  BlobLen: Integer;
  stream: TStream;
  E: Integer;
  L: Double;
  _fieldName:String;
begin
  if (SrcFieldNo = 0) or (DstFieldNo = 0) then
    Exit;
  SrcTyp := Dataset.FieldType(SrcFieldno);
  DstTyp := Self.FieldType(DstFieldno);
  _fieldName:= Self.Field(DstFieldno) ;
  case DstTyp of
    'M', 'B', 'G':
      begin
        if not (SrcTyp in ['M', 'B', 'G']) then
          exit;
        BlobLen := Dataset.MemoSizeN(SrcFieldNo);
        if BlobLen > 0 then
        begin
          stream := TMemoryStream.Create;
          try
            Dataset.MemoLoadN(SrcFieldNo, stream);
            stream.Position := 0;
            Self.MemoSaveN(DstFieldNo, stream);
          finally
            stream.free;
          end;
        end;
      end;
  else
    begin
      if (SrcTyp = 'C') and (DstTyp <> 'C') then
      begin
        { possible conflict here with wrong assign }
        Val(Trim(Dataset.StringGetN(SrcFieldNo)), L, E);
        if E <> 0 then
          L := 0;
          //C:
//        FieldPutN(DstFieldNo, FloatToStr(L));
        FieldPut(_fieldName,  FloatToStr(L))
      end
      else
       //C:
        //FieldPutN(DstFieldNo, Dataset.FieldGetN(SrcFieldNo));
         FieldPut(_fieldName,  Dataset.FieldGetN(SrcFieldNo));
    end;
  end;
end;
     *)
{
procedure TlicgBaseTable.BeginTrans;
begin
end;
}
function TlicgBaseTable.DBCreateTable(const fname: string; AFieldList: TStringList): Boolean;
begin
  Result := False;
end;

function TlicgBaseTable.DBDropIndex(const TableName: string): Boolean;
begin
  Result := False;
end;

function TlicgBaseTable.DBDropTable(const TableName: string): Boolean;
begin
  Result := False;
end;

function TlicgBaseTable.DBRenameTable(const Source, Target: string): Boolean;
begin
  Result := False;
end;

function TlicgBaseTable.DBTableExists(const TableName: string): Boolean;
begin
  Result := False;
end;

function TlicgBaseTable.FieldNofromAlias(const AliasName: string): Integer;
begin
  Result := -1;
end;

function TlicgBaseTable.GetFieldAlias(FieldNo: Integer): string;
begin
  Result := Field(FieldNo);
end;

function TlicgBaseTable.GetFieldAliasByName(const FieldName: string): string;
begin
  Result := GetFieldAlias(FieldNo(FieldName));
end;

procedure TlicgBaseTable.RollBackTrans;
begin
end;

procedure TlicgBaseTable.SetFieldAlias(FieldNo: Integer; const Value: string);
begin

end;

procedure TlicgBaseTable.SetFieldAliasByName(const FieldName: string; const
  Value: string);
begin
  SetFieldAlias(FieldNo(FieldName), Value);
end;

{ TlicgBaseMapInfo }

constructor TlicgBaseMapInfo.Create(const AGIS: TlicgBaseGis);
begin
  inherited Create;
  FGIS := AGIS;
end;

procedure TlicgBaseMapInfo.Assign(Source: TlicgBaseMapInfo);
begin
//  if _GIS.Active then begin
  NumLayers := Source.NumLayers;
  Extension := Source.Extension;
  CurrentLayer := Source.CurrentLayer;
  AerialViewLayer := Source.AerialViewLayer;
  LastView := Source.LastView;
  IsAreaClipped := Source.IsAreaClipped;
  AreaClipped := Source.AreaClipped;
  ClipAreaKind := Source.ClipAreaKind;

  // ilker ekleme yeni
  CS := Source.CS;
  OnTheFlyCS := Source.OnTheFlyCS;
  IsOnTheFly := Source.IsOnTheFly;
  ImportCS := Source.ImportCS;
//  end;
end;

procedure TlicgBaseMapInfo.SetCurrentLayer(const Value: string);
begin
  if Assigned(FGIS) and Assigned(FGIS.OnCurrentLayerChange) then
  begin
    FGIS.OnCurrentLayerChange(FGIS, Value);
  end;
end;

{ TlicgBaseGIS }

function TlicgBaseGIS.AddLayer(const FileName: string; IsAnimation, WithDB: Boolean; LayerType: TlicgLayerType;
  ACS: IlicgCS; AColor: TColor; FieldList: TObject; ALayerListRefresh: Boolean = True): TlicgBaseLayer;
var
  FilePath, news, layname: string;
begin
  result := nil;

  if ReadOnly then
    Exit;

  if Length(Trim(FileName)) = 0 then
    LicadGISError(Format(SWronglayerName, [FileName]));

  if (LayerType = ltDesktop) and LayerExists(FileName, LayerType) then
    LicadGISError(SDuplicatelayer);

  layname := FileName;
  FilePath := ExtractFilePath(layname);
  if Length(FilePath) = 0 then
    FilePath := ExtractFilePath(self.FileName);
  FilePath := AddSlash(FilePath);
  news := Lider.CG.Com.System.GetValidLayerName(ExtractFileName(layname));
  while Layers.IndexOFName(ExtractFilename(news)) >= 0 do
    news := news + '_';
  layname := FilePath + news;

  Result := Internal_CreateLayer(layname, LayerType, AColor, ALayerListRefresh);
  try
    if Assigned(Result) then
    begin
      Result.InitializeOnCreate(layname, WithDb and (LayerType = ltDesktop) and
        (Result.LayerClassType = lctLayer), IsAnimation, ACS, FieldList);
      Result.LayerInfo.PrintOnOFF := True;
      Result.LayerInfo.isLayerBrush := False;

      if Self.Layers.IndexOfName(Result.Name) < 0 then
      begin
        Self.Layers.AddLayer(Result);
      end;
    end;
  except
    Result.Free;
    Result := nil;
    raise;
  end;
  Result.Open;
end;

function TlicgBaseGIS.AddToLayers(const FileName: string; LayerType: TlicgLayerType;
   AColor: TColor = clBlue; ALayerListRefresh: Boolean = True): Integer;
var
  Layer: TlicgBaseLayer;
begin
  Result := -1;
  if not ReadOnly then
  begin
    Layer := Internal_CreateLayer(FileName, LayerType, AColor, ALayerListRefresh);
    Result := Layer.Index;
  end;
end;

constructor TlicgBaseGIS.Create;
var
  I: Integer;
begin
  inherited Create;

  FUseReservedData := False;

  for I := Low(FReserved) to High(FReserved) do
    FReserved[I] := 1;

  FThematicList := TList.Create;
  FTextureFillLibs := TlicgTextureFillLibraries.Create;
  FOnlySelectionExport := False;
  TempParamString := '';
  FOnFillLayersToListbox := nil;
  fHintCommandMessageEvent := nil;
  FOnCurrentLayerChange := nil;
  FThicknessWidth := 1;
  FGuideIsolineInterval := 1;
end;

function TlicgBaseGIS.CreateAnimationLayer(const AFileName: string; AColor: TColor = clBlue; ALayerListRefresh: Boolean = True): TlicgBaseLayer;
begin
  Result := nil;
  if (not ReadOnly) then
  begin
    Result := AddLayer(AFileName, True, False, ltDesktop, MapInfo.CS, AColor, nil, ALayerListRefresh);
  end;
end;

function TlicgBaseGIS.CreateCosmethicLayer(const AFileName: string; AColor: TColor = clBlue; ALayerListRefresh: Boolean = True): TlicgBaseLayer;
begin
  Result := nil;
  if (not ReadOnly) then
  begin
    if FileExists(AFileName) then
      SysUtils.DeleteFile(AFileName);
    Result := AddLayer(AFileName, False, False, ltMemory, MapInfo.CS, AColor, nil, ALayerListRefresh);
  end;
end;

function TlicgBaseGIS.CreateLayerEx(const LayerName: string; ACS: IlicgCS; AColor: TColor = clBlue;
  FieldList: TObject = nil; ALayerListRefresh: Boolean = True): TlicgBaseLayer; stdcall;
var
  LayerType: TlicgLayerType;
begin
  Layertype := ltDesktop;
  if IsCAD then
    Layertype := ltMemory;

  if Layers.IndexOfName(ExtractFilePath(FileName) + LayerName) = -1 then
  begin
    if Lider.CG.Com.System.LayerExists(ExtractFilePath(FileName) + LayerName, ltDesktop) then
      Lider.CG.Com.System.DeleteLayerFiles(ExtractFilePath(FileName) + LayerName);
    if not ReadOnly then
    begin
      LayerType := ltDesktop;
      if IsCAD then
        LayerType := ltMemory;

      Result := AddLayer(ExtractFilePath(FileName) + LayerName, False, True, LayerType, ACS, AColor, FieldList, ALayerListRefresh);
    end;
  end;
  if Result = nil then
    Result := Layers.Get(Layers.IndexOfName(LayerName));
end;

function TlicgBaseGIS.CreateLayer(const ALayerName: string; ALayerType: TlicgLayerType; AColor: TColor = clBlue;
  AFieldList: TObject = nil; ALayerListRefresh: Boolean = True): TlicgBaseLayer;
var
  LayerType: TlicgLayerType;
  _LayerName: string;
begin
  if (ALayerType = ltMemory) or (ALayerType = ltDesktop) then
  begin
    Layertype := ltDesktop;
    if IsCAD then
      Layertype := ltMemory;

    _LayerName := ChangeFileExt(ExtractFileName(ALayerName), '');
    if Layers.IndexOfName(ExtractFilePath(FileName) + _LayerName) = -1 then
    begin

      if Lider.CG.Com.System.LayerExists(ExtractFilePath(FileName) + _LayerName, ltDesktop) then
        Lider.CG.Com.System.DeleteLayerFiles(ExtractFilePath(FileName) + _LayerName);

      if not ReadOnly then
      begin
        LayerType := ltDesktop;
        if IsCAD then
          LayerType := ltMemory;

        Result := AddLayer(ExtractFilePath(FileName) + _LayerName, False, True, LayerType, MapInfo.CS, AColor, AFieldList, ALayerListRefresh);
        _LayerName := Result.DisplayName;
      end;
    end;
  end;

  Result := Layers.Get(Layers.IndexOfName(_LayerName));
end;

function TlicgBaseGIS.DeleteLayer(const LayerName: string; DeleteFiles: Boolean = True): Boolean;
var
  Layer: TlicgBaseLayer;
  i, Index: Integer;
  LType: TlicgLayerClassType;
begin
  Result := False;
  if Active and not ReadOnly then
  begin
    { delete layer Index }
    Index := Layers.IndexOfName(LayerName);
    if Index >= 0 then
    begin

      Layer := Layers.Items[Index];

      for i := 0 to self.ThematicList.Count - 1 do
      begin
        if (Trim(TlicgThematicBuilder(self.ThematicList[I]).LayerName) = '') or (AnsiCompareText
          (ExtractFileName(TlicgThematicBuilder(self.ThematicList[I]).LayerName),
          ExtractFileName(Layer.Name)) = 0) then
        begin
          TlicgThematicBuilder(self.ThematicList[I]).Free;
          self.ThematicList.Delete(i);
          BREAK;
        end;
      end;


      { notify to all DrawBox about deletion of layer }
      for I := 0 to DrawBoxList.Count - 1 do
        DrawBoxList[I].Selection.DeleteLayer(Layer);

      Layer.Close;
      try
        if DeleteFiles and not Layer.DeleteLayerFiles then
        begin
          Layer.Open;
          LicadGISError(SCannotDeleteLayer);
        end;

        lType := Layer.LayerClassType;
        Layer.Free; // take it as deleted


        if Assigned(self.OnFillLayersToListbox) then
          self.OnFillLayersToListbox(nil);

        if Assigned(OnAfterDeleteLayer) then
        begin
            //if (LType in  [lctLayer,lctPostgres]) then
          TlicgLayerEvent(OnAfterDeleteLayer)(nil, '');
        end;

      except
        Layer.Open;
        raise;
      end;
      Result := True;
      Modified := true;
    end;
  end;
end;

procedure TlicgBaseGIS.ClearThematicList(_ThematicList: TList);
var
  I: Integer;
begin
  if _ThematicList = nil then
  begin
    for I := 0 to FThematicList.Count - 1 do
      TlicgThematicBuilder(FThematicList[I]).Free;
    FThematicList.Clear;
  end
  else
  begin
    for I := 0 to _ThematicList.Count - 1 do
      TlicgThematicBuilder(_ThematicList[I]).Free;
    _ThematicList.Clear;
  end;
end;

destructor TlicgBaseGIS.Destroy;
begin
  ClearThematicList(FThematicList);
  FThematicList.Free;
  FTextureFillLibs.Free;
  inherited Destroy;
end;

(* ilker silme kullanılmıyor
{ TlicgThemedCustomControl }
constructor TlicgThemedCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseXPThemes := True;
  FBorderStyle := bsSingle;
end;

function TlicgThemedCustomControl.GetBorderStyle: forms.TBorderStyle;
begin
  Result := FBorderStyle;
end;

function TlicgThemedCustomControl.GetUseXPThemes: Boolean;
begin
  Result := FUseXPThemes;
end;

procedure TlicgThemedCustomControl.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TlicgThemedCustomControl.CMCtl3DChanged(var m: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

procedure TlicgThemedCustomControl.CreateWnd;
begin
  inherited;
  CreateThemeHandle;
end;

procedure TlicgThemedCustomControl.DestroyWnd;
begin
  inherited;
  FreeThemeHandle;
end;

procedure TlicgThemedCustomControl.CreateThemeHandle;
begin
  if FUseXPThemes and Assigned(licg_IsAppThemed) and licg_IsAppThemed and
    licg_IsThemeActive then
    FTheme := licg_OpenThemeData(Handle, Pointer(PChar(LICGEDIT)))
  else
    FTheme := 0;
end;

procedure TlicgThemedCustomControl.FreeThemeHandle;
begin
  if FTheme <> 0 then
    licg_CloseThemeData(FTheme);
end;

procedure TlicgThemedCustomControl.WMThemeChanged(var Message: TMessage);
begin
  inherited;
  FreeThemeHandle;
  CreateThemeHandle;
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or
    SWP_NOSIZE or SWP_NOZORDER);
  RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  Message.Result := 1;
end;

procedure TlicgThemedCustomControl.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  RC, RW: TRect;
begin
  if FTheme = 0 then
  begin
    inherited;
    exit;
  end;
  {if BorderStyle = bsSingle then
  begin}
  DefaultHandler(Message);
  DC := GetWindowDC(Handle);
  try
    Windows.GetClientRect(Handle, RC);
    if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
    begin
      if (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LEFTSCROLLBAR) <> 0 then
        dec(RC.Left, GetSystemMetrics(SM_CXVSCROLL))
      else
        inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
    end;
    if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
      inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);
    licg_DrawThemeBackground(FTheme, DC, 0, 0, RW, nil);
    Message.Result := 0;
  finally
    ReleaseDC(Handle, DC);
  end;
  {end
  else
    inherited; }
end;

procedure TlicgThemedCustomControl.SetUseXPThemes(const Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    if HandleAllocated then
    begin
      FreeThemeHandle;
      CreateThemeHandle;
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or
        SWP_NOSIZE or SWP_NOZORDER);
      RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
    end;
  end;
end;

destructor TlicgThemedCustomControl.Destroy;
begin
  inherited;

end; *)

{ TlicgBaseDrawBox }

constructor TlicgBaseDrawBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMovePoint := Point(0, 0);
end;

destructor TlicgBaseDrawBox.Destroy;
begin
  inherited Destroy;
end;

function TlicgBaseDrawBox.IsGeographicCS: Boolean;
begin
  Result := GIS.MapInfo.CS.IsGeographicCS;
end;

function TlicgBaseDrawBox.isViewChanged: Boolean;
begin
//
end;

procedure TlicgBaseDrawBox.CreateBarThematic(const LayerName, FieldNames: string;
  Fill: Boolean; Width, Height: Double; StartColor: Integer);
(*
var
  layer: TlicgBaseLayer;
  Pos: TlicgCoor;
  TmpEnt: IlicgEntity;
  fl, vl: TStringList;
  i: Integer;
  Group: IlicgEntity;
  FileName: string;
  CosmethicLayer: TlicgBaseLayer;
  FMax, Value: Double;
  cnt: Integer;

  procedure MakeBar(const Pos: TlicgCoor; ValueList: TStringList);
  var
    pgent: IlicgEntity;
    txent: IlicgEntity;
    xy: array[0..4] of TlicgCoor;
    th, x0, Sp1, sum: Double;
    xpos, ypos, subwidth: double;
    j, n, nclr: Integer;
    ratio: double;

  begin
    xpos := pos.X - width / 2;
    X0 := Xpos;
    ypos := Pos.Y;
    subwidth := width / ValueList.count; //width of each bar

    n := ValueList.Count - 1;
    sum := 0;
    for j := 0 to n do
      Sum := Sum + StrToFloat(ValueList[j]); //sum of whole Value

    {width := width * sum / FMax;
    height := height * sum / Fmax;}
    ratio := sum / Fmax;
    subwidth := subwidth * ratio;

    for j := 0 to n do
    begin
      if j > n then
        break;
      if (Sum <> 0) and (Ratio <> 0) then
        sp1 := (StrToFloat(ValueList[j])) / sum * height * Ratio
      else
        sp1 := 0;

      Xpos := x0 + j * subwidth;

      //Starting Point
      xy[0].x := xpos;
      xy[0].y := Ypos;
      xy[1].x := xpos - subwidth;
      xy[1].y := Ypos;
      xy[2].x := xpos - subwidth;
      xy[2].y := Ypos + sp1;
      xy[3].x := xpos;
      xy[3].y := Ypos + sp1;
      xy[4].x := xpos;
      xy[4].y := Ypos;


      pgent := Licad.CreateEntityFactory.MakePolygon(xy);
      nclr := ThematicSeriesColors.Count;
      if Fill then
      begin
        PgEnt.DrawTools.Brushtool.ForeColor := ThematicSeriesColors[(j + StartColor) mod nclr];
        pgent.DrawTools.Brushtool.Pattern := 1;
      end
      else
      begin
        pgent.DrawTools.Brushtool.Pattern := 0;
        pgent.DrawTools.Pentool.Style := 0;
        PgEnt.DrawTools.Pentool.Color := ThematicSeriesColors[(j + StartColor) mod nclr];
      end;
      AslicgGroupGeometry(Group).AddEntity(pgent);

      th := subwidth / length(inttostr(trunc(sum)));

      TxEnt := Licad.CreateEntityFactory.MakeVectorialFittedText(xy[1], ValueList[j], th, -1, 0);
      AslicgVectorialFittedText(TxEnt).MakePolyPointsByCenter(point2d(xy[0].x - subwidth / 2, xy[2].y - sp1 / 2));
      TxEnt.Pentool.Style := 0;
      TxEnt.Brushtool.Pattern := 0;
      AslicgGroupGeometry(Group).Add(TxEnt);
    end;
  end;

begin
  VL := TStringlist.create;

  layer := GIS.Layers.LayerByName(LayerName);
  if (Layer = nil) or (Layer.DBTable = nil) then
    exit;

  //initialize
  CosmethicLayer := FGIS.FoundFirstCosmethicLayer;
  if CosmethicLayer = nil then
  begin
    // create a cosmethc layer
    FileName := GetTemporaryLayerName(AddSlash(ExtractFilePath(GIS.FileName)), 'CSM');
    CosmethicLayer := GIS.Layers[GIS.Layers.Add(FileName, ltMemory)];
    CosmethicLayer.Open;
  end
  else
  begin
    // clear the cosmethic layer
    CosmethicLayer.Open;
    CosmethicLayer.Zap;
  end;

  fl := SplitbySemiColon(FieldNames);
  if fl.count = 0 then
    exit;

  Layer.First;
  if Layer.Eof then
    Exit;

  //Calc Min,Max for bar's sizeing.
  FMax := -1E20;
  Layer.first;
  while not Layer.Eof do
  begin
    if Layer.RecIsDeleted then
    begin
      Layer.Next;
      Continue;
    end;
    Layer.DBTable.RecNo := Layer.RecNo;
    Value := 0;
    for cnt := 0 to fl.count - 1 do
      Value := Value + layer.DBTable.FloatGet(fl[cnt]);
    FMax := Max(Value, FMax);

    Layer.Next;
  end;

  width := Grapher.DistToRealX(trunc(width));
  height := Grapher.DistToRealY(trunc(height));

  Layer.First;
  if not Layer.Eof then
    repeat
      if layer.RecIsDeleted = False then
      begin
        Layer.DBTable.RecNo := Layer.RecNo;
        VL.Clear;
        for i := 0 to fl.count - 1 do
          vl.add(layer.DBTable.StringGet(fl[i]));

        TmpEnt := layer.RecLoadEntity;
        Tmpent.Centroid(pos.x, pos.y);
        Tmpent.free;

        Group := TlicgGroupEntity.CreateEntity;
        MakeBar(Pos, VL);
        CosmethicLayer.AddEntity(Group);
        Group.Free;
      end;
      Layer.Next;
    until Layer.Eof;

  vl.free;
  fl.free;
  Repaint;
  *)
begin
end;

procedure TlicgBaseDrawBox.CreateDotThematic(const Layername, FieldName: string;
  Fill: Boolean; DotRange: Double; Dotsize, DotColor: Integer);
(*
var
  layer: TlicgBaseLayer;
  TmpEnt: IlicgEntity;
  fl, vl: TStringList;
  i: Integer;
  hWid: Double;
  Group: TlicgGroupEntity;
  FileName: string;
  CosmethicLayer: TlicgBaseLayer;

  procedure MakeDot(Ent: IlicgEntity; ValueList: TStringList);
  var
    pgent: TEzEllipse;
    xpos, ypos, xwid, ywid: double;
    i, j, k, n: Integer;
    EBox: TlicgExtend;
  begin
    n := ValueList.Count - 1;

    xwid := Ent.FBox.Emax.X - Ent.FBox.Emin.X;
    ywid := Ent.FBox.Emax.Y - Ent.FBox.Emin.Y;

    randomize;
    for i := 0 to n do
    begin
      k := round(StrToFloat(ValueList[i]) / DotRange);
      for j := 0 to k do
      begin
        repeat
          xpos := Ent.FBox.Emin.X + random * Xwid;
          ypos := Ent.FBox.Emin.Y + random * Ywid;
          if Ent.IsPointInsideMe(Xpos, YPos) then
            break;
        until False;

        EBox.Emin.X := XPos - hwid;
        EBox.Emax.x := Xpos + hwid;
        EBox.Emin.Y := YPos - hwid;
        EBox.Emax.Y := Ypos + hwid;

        pgent := TEzEllipse.CreateEntity(ebox.emin, ebox.emax);
        if Fill then
        begin
          PgEnt.Brushtool.ForeColor := DotColor;
          pgent.Brushtool.Pattern := 1;
          pgent.Pentool.Style := 0;
        end
        else
        begin
          pgent.Brushtool.Pattern := 0;
          pgent.Pentool.Style := 0;
          pgent.Pentool.Color := DotColor;
        end;
        Group.Add(PgEnt);
      end;
    end;
  end;

begin
  if FGIS = nil then
    exit;
  VL := TStringlist.create;

  layer := GIS.Layers.LayerByName(LayerName);
  if (Layer = nil) or (Layer.DBTable = nil) then
    exit;

  //initialize
  CosmethicLayer := FGIS.FoundFirstCosmethicLayer;
  if CosmethicLayer = nil then
  begin
    // create a cosmethc layer
    FileName := GetTemporaryLayerName(AddSlash(ExtractFilePath(GIS.FileName)), 'CSM');
    CosmethicLayer := GIS.Layers[GIS.Layers.Add(FileName, ltMemory)];
    CosmethicLayer.Open;
  end
  else
  begin
    // clear the cosmethic layer
    CosmethicLayer.Open;
    CosmethicLayer.Zap;
  end;

  fl := SplitbySemiColon(FieldName);
  if fl.Count = 0 then
    exit;

  //calc dot size
  hwid := Grapher.DistToRealX(Dotsize);

  Layer.First;
  if not Layer.Eof then
    repeat
      if layer.RecIsDeleted = False then
      begin
        Layer.DBTable.RecNo := Layer.RecNo;
        VL.Clear;
        for i := 0 to fl.count - 1 do
          vl.add(Layer.DBTable.StringGet(fl[i]));

        Group := TlicgGroupEntity.CreateEntity;
        TmpEnt := layer.RecLoadEntity;
        MakeDot(TmPent, vl);
        Tmpent.free;
        CosmethicLayer.AddEntity(Group);
        Group.Free;
      end;

      Layer.Next;
    until Layer.Eof;

  vl.free;
  fl.free;
  Repaint;
  *)
begin
end;

procedure TlicgBaseDrawBox.CreatePieThematic(const LayerName, FieldNames: string;
  Fill: Boolean; Radian: Double; StartColor: Integer);
(*
var
  layer: TlicgBaseLayer;
  Pos: TlicgCoor;
  TmpEnt: IlicgEntity;
  fl, vl: TStringList;
  i: Integer;
  FMax, Value: Double;
  cnt: Integer;
  Group: IlicgEntity;
  FileName: string;
  CosmethicLayer: TlicgBaseLayer;
  PtInclude: array[0..100] of Boolean;
  TxPt: array[0..100] of TlicgCoor;
  xy: array[0..180] of TlicgCoor;

  procedure MakehalfCircle(const Pos: TlicgCoor; Rad: Double; ValueList: TStringList);
  var
    PgEnt: TEzPolygon;
    TxEnt: TEzFittedVectorText;
    Sp1, sum: Double;
    m1, n, nclr, j: Integer;
    i1, k1, k2, n1: double;
    X0, y0, X, Y: double;

  begin
    X0 := Pos.x;
    y0 := Pos.y;
    n := Min(100, ValueList.Count) - 1;

    //Sum of all Value
    sum := 0;
    for j := 0 to n do
      Sum := Sum + StrToFloat(ValueList[j]);

    Rad := Rad * sum / FMax;

    FillChar(PtInclude, SizeOf(PtInclude), 0);

    k2 := 0;
    for j := 0 to n do
    begin
      //if j>n then break;  // why is delphi doing this ???????????????????????????
      Sp1 := (StrToFloat(ValueList[j])) / (sum);
      if sp1 = 0 then
        Continue;
      k1 := sp1 * (2 * pi); //Angle of Pie by Value percent
      i1 := k2; //Pie's Start angle
      k2 := k2 + k1; //Next Pie's Start Angle
      n1 := k1 / 179; //Angle Splits.

      m1 := 0;
      xy[0].X := x0;
      xy[0].y := y0;

      while m1 <= 180 do
      begin
        X := Rad * sin(i1);
        y := Rad * cos(i1);
        X := X0 + X;
        y := Y0 + y;
        inc(m1);
        xy[m1].X := x;
        xy[m1].y := y;
        i1 := i1 + n1;
      end;

      PgEnt := TEzPolygon.CreateEntity(xy);
      nclr := ThematicSeriesColors.Count;
      if Fill then
      begin
        TEzPolygon(PgEnt).Brushtool.ForeColor := ThematicSeriesColors[(j + StartColor) mod nclr];
        TEzPolygon(PgEnt).Brushtool.Pattern := 1;
      end
      else
      begin
        with TEzPolygon(PgEnt) do
        begin
          Brushtool.Pattern := 0;
          Pentool.Style := 0;
          Pentool.Color := ThematicSeriesColors[(j + StartColor) mod nclr];
        end;
      end;
      PgEnt.Centroid(Txpt[j].x, Txpt[j].y);
      Group.Add(PgEnt);
      PtInclude[j] := True;
    end;

    //avoiding text hideing from another pie polygon,
    //Text will draw final drawing time.
    for j := 0 to n do
    begin
      if not PtInclude[j] then
        Continue;
      //create text and Calc Text location and height here.
      Y := Rad / 4;
      TxEnt := TEzFittedVectorText.CreateEntity(TxPt[j],
        ValueList[j],
        Y,
        -1,
        0);
      TxEnt.MakePolyPointsByCenter(TxPt[j]);
      with TEzFittedVectorText(txent) do
      begin
        Pentool.Style := 0;
        Brushtool.Pattern := 0;
      end;
      Group.Add(TxEnt);
    end;
  end;

begin
  vl := TStringlist.create;

  layer := GIS.Layers.LayerByName(LayerName);
  if (Layer = nil) or (Layer.DBTable = nil) then
    exit;

  //initialize
  CosmethicLayer := FGIS.FoundFirstCosmethicLayer;
  if CosmethicLayer = nil then
  begin
    // create a cosmethc layer
    FileName := GetTemporaryLayerName(AddSlash(ExtractFilePath(GIS.FileName)), 'CSM');
    CosmethicLayer := GIS.Layers[GIS.Layers.Add(FileName, ltMemory)];
    CosmethicLayer.Open;
  end
  else
  begin
    // clear the cosmethic layer
    CosmethicLayer.Open;
    CosmethicLayer.Zap;
  end;
  fl := SplitbySemiColon(FieldNames);
  if fl.count = 0 then
    exit;

  FMax := -1E20;
  Layer.First;
  while not Layer.Eof do
  begin
    if Layer.RecIsDeleted then
    begin
      Layer.Next;
      Continue;
    end;
    Layer.DBTable.RecNo := Layer.RecNo;
    Value := 0;
    for cnt := 0 to fl.count - 1 do
      Value := Value + layer.DBTable.FloatGet(fl[cnt]);
    FMax := Max(Value, FMax);

    Layer.Next;
  end;

  Radian := Grapher.DistToRealY(trunc(Radian));

  Layer.First;
  if not Layer.Eof then
    repeat
      if layer.RecIsDeleted = False then
      begin
        Layer.DBTable.RecNo := Layer.RecNo;
        VL.Clear;
        for i := 0 to fl.count - 1 do
          vl.add(layer.DBTable.StringGet(fl[i]));

        TmpEnt := layer.RecLoadEntity;
        Tmpent.Centroid(pos.x, pos.y);
        Tmpent.free;

        Group := TlicgGroupEntity.CreateEntity;
        MakehalfCircle(Pos, Radian, VL);
        CosmethicLayer.AddEntity(Group);
        Group.Free;
      end;

      Layer.Next;
    until Layer.Eof;

  vL.free;
  fl.free;
  Repaint;
  *)
begin
end;

{ TlicgBaseSelectionLayer }

constructor TlicgBaseSelectionLayer.Create(Selection: TlicgBaseSelection; Layer:
  TlicgBaseLayer);
begin
  inherited create;
end;

destructor TlicgBaseSelectionLayer.Destroy;
begin
  inherited;
end;

{ TlicgBaseSelection }

constructor TlicgBaseSelection.Create(DrawBox: TlicgBaseDrawBox);
begin
  inherited Create;
end;

destructor TlicgBaseSelection.Destroy;
begin
  inherited;
end;

{ TlicgBaseUndoSingle }

constructor TlicgBaseUndoSingle.Create(UndoPool: TlicgBaseUndoPool);
begin
  inherited Create;
end;

destructor TlicgBaseUndoSingle.Destroy;
begin
  inherited;
end;

{ TlicgBaseUndoPool }

constructor TlicgBaseUndoPool.Create(DrawBox: TlicgBaseDrawBox; isCreateUndoSave:
  Boolean);
begin
  inherited Create;
end;

destructor TlicgBaseUndoPool.Destroy;
begin
  inherited;
end;

{ TlicgBaseRedoSingle }

constructor TlicgBaseRedoSingle.Create(DrawBox: TlicgBaseDrawBox);
begin
  inherited Create;
end;

destructor TlicgBaseRedoSingle.Destroy;
begin
  inherited;
end;

{ TlicgBaseRedoPool }

constructor TlicgBaseRedoPool.Create(DrawBox: TlicgBaseDrawBox);
begin
  inherited Create;
end;

destructor TlicgBaseRedoPool.Destroy;
begin
  inherited;
end;

function TlicgBaseLayer.EntityType(RecNo: Integer): TlicgEntityID;
var
  Entity: IlicgEntity;
begin
  Result := Low(TlicgEntityID);
  if LayerInfo.IsCosmethic then
  begin
    Entity := EntityWithRecNo(RecNo);
    if Entity <> nil then
      Result := Entity.EntityID;
  end
  else
  begin
    Entity := LoadEntityWithRecNo(RecNo);
    try
      Result := Entity.EntityID;
    finally
      Entity := nil;
    end;
  end;
end;

function TlicgBaseLayer.EntityWithRecNo(RecNo: Integer): IlicgEntity;
begin
  Result := LoadEntityWithRecNo(RecNo);
end;

function TlicgBaseLayer.Eof: Boolean;
begin
  Result := true;
end;

function TlicgBaseLayer.GetActive: Boolean;
begin
//  REsult := false;
end;

function TlicgBaseLayer.GetActiveSearchCanceledEvent: Boolean;
begin
  result := false;
end;

function TlicgBaseLayer.GetAutoflush: Boolean;
begin

end;

function TlicgBaseLayer.GetBlinkers: IlicgIntegerList;
begin
{
  if Blinkers = nil then
    Blinkers := CreateIntegerList;
  Result := Blinkers;
  }
end;

function TlicgBaseLayer.GetDBTable: TlicgBaseTable;
begin
  REsult := nil;
end;

function TlicgBaseLayer.GetDisplayName: string;
begin

end;

function TlicgBaseLayer.GetFileName: string;
begin
//  Result := ExtractFilePath(_GIS.FileName) + ExtractFileName(FFileName);
end;

function TlicgBaseLayer.GetGIS: TlicgBaseGIS;
begin

end;

function TlicgBaseLayer.GetGroupName: string;

  function GetGroup(ALayerDisplayName: string): string;
  var
    G, L: Integer;
    AGroupName: string;
  begin
    for G := 0 to FGIS.Layers.Count - 1 do
    begin
      if Copy(FGIS.Layers[G].DisplayName, 1, 1) <> '#' then
        Continue;

      if FGIS.Layers[G].DisplayName = ALayerDisplayName then
      begin
        Result := ALayerDisplayName;
        Exit;
      end;

      AGroupName := FGIS.Layers[G].DisplayName;

      for L := 0 to FGIS.Layers.Count - 1 do
      begin
        if FGIS.Layers[L].DisplayName = ALayerDisplayName then
        begin
          AGroupName := Copy(AGroupName, Pos('#', AGroupName) + 1, Length(AGroupName) - 1) + '_';

          if AGroupName = Copy(ALayerDisplayName, 1, Length(AGroupName)) then
          begin
            Result := FGIS.Layers[G].DisplayName;
            Exit;
          end;
        end;
      end;

    end;
    Result := '';
  end;

begin
  Result := GetGroup(Self.DisplayName);
end;

function TlicgBaseLayer.GetIndex: Integer;
begin
//  Result := GIS.Layers.IndexofName(Self.FName);
end;

function TlicgBaseLayer.GetLayerClassType: TlicgLayerClassType;
begin
//   result := FLayerClassType;
end;

function TlicgBaseLayer.GetLayerInfo: TlicgBaseLayerInfo;
begin
//  Result := FLayerInfo;
end;

function TlicgBaseLayer.GetModified: Boolean;
begin
//  Result := FModified or FLayerInfo.Modified;
end;

function TlicgBaseLayer.GetName: string;
begin
//  Result := FName;
end;

function TlicgBaseLayer.GetOnBeforePaintEntity: TlicgBeforePaintEntityEvent;
begin
//   result := FOnBeforePaintEntity;
end;

function TlicgBaseLayer.GetRecNo: Integer;
begin
 //  Result := -1;
end;

function TlicgBaseLayer.GetRecordCount: LongInt;
begin
//   Result := 0;
end;

function TlicgBaseLayer.Getrt: TRTree;
begin
//  Result:=Frt;
end;

function TlicgBaseLayer.GetSaveHeaderVersion: smallint;
begin
//   result := FSaveHeaderVersion;
end;

{
function TlicgBaseLayer.GetSelect: ILayerSelect;
begin
 result := nil;
end;
}

function TlicgBaseLayer.GetSpaceKeySelected: Boolean;
begin
//  Result := FSpaceKeySelected;
end;

function TlicgBaseLayer.InitValuesBeforeAddEntity(const Entity: IlicgEntity;
  const Gis: TObject; var isUndo: Boolean; _PropVList: IUnknown): Boolean;
begin
  Result := True;
  case Entity.EntityID of
    idPlace, idBlockInsert:
      begin
        if TlicgBaseGIS(GIS).MapInfo.CS.IsGeographicCS then
        begin
          Entity.DrawTools.SymbolTool.Height := -4;
          Entity.DrawTools.SymbolTool.Index := 35;
        end;
      end;
  end;
end;

function TlicgBaseLayer.HasBlinkers: Boolean;
begin
  Result := (Blinkers <> nil) and (Blinkers.Count > 0);
end;

function TlicgBaseLayer.InfoTableName: string;
begin
  Result := '';
end;

function TlicgBaseLayer.IsCurrentLayer: Boolean;
begin
  Result := AnsiCompareText(GIS.MapInfo.CurrentLayer, Name) = 0;
end;

function TlicgBaseLayer.LoadEntityWithRecNo(RecNo: Integer): IlicgEntity;
begin
  Result := nil;
end;

procedure TlicgBaseLayer.MaxMinExtents(var AMinX, AMinY, AMaxX, AMaxY: Double);
begin
  with LayerInfo.Extension do
  begin
    AMinX := LowerLeft.x;
    AMinY := LowerLeft.y;
    AMaxX := UpperRight.x;
    AMaxY := UpperRight.y;
  end;
end;

function TlicgBaseLayer.MoveEntityBackwards(ARecNo: Integer): Integer;
begin
  Result := 0;
end;

function TlicgBaseLayer.MoveEntityForward(ARecNo: Integer): Integer;
begin
  Result := 0;
end;

procedure TlicgBaseLayer.Open;
begin
  if Assigned(GIS.OnBeforeOpenLayer) then
    GIS.OnBeforeOpenLayer(GIS, Self.Name);
end;

procedure _PopulateFieldList(Layer: TlicgBaseLayer; Strings: TStrings);
var
  I: Integer;
  FieldName: string;
begin
  Strings.Clear;
  if Layer.DBTable <> nil then
  begin
    with Layer.DBTable do
    begin
      for I := 1 to FieldCount do
      begin
        FieldName := Field(I);
        Strings.AddObject(FieldName, nil);
      end;
    end;
  end;
end;

procedure TlicgBaseLayer.PopulateFieldList(Strings: TStrings; WithAlias: Boolean);
var
  I: Integer;
  FieldName: string;
begin
  { populate lbColumns with fields of native database }
  Strings.Clear;
  Strings.AddObject(AddBrackets(DisplayName) + SEntityField, nil);
  Strings.AddObject(AddBrackets(DisplayName) + SRecNofield, nil);

  if DBTable <> nil then
    with DBTable do
      for I := 1 to FieldCount do
      begin
        if WithAlias then
          FieldName := FieldAlias[I]
        else
          FieldName := Field(I);
        Strings.AddObject(AddBrackets(DisplayName) + '.' + AddBrackets(FieldName), nil);
      end;
end;

procedure TlicgBaseLayer.PopulateFieldListNoFuncFields(Strings: TStrings);
var
  I: Integer;
  FieldName: string;
begin
  { populate lbColumns with fields of native database }
  Strings.Clear;

  if DBTable <> nil then
  begin
    with DBTable do
      for I := 1 to FieldCount do
      begin
        FieldName := Field(I);
        Strings.AddObject(FieldName, nil);
      end;
  end;
end;

procedure TlicgBaseLayer.ReadHeaders(HeaderVersion: SmallInt; var Stream: TStream);
var
  FHeader: TlicgLayerHeader;
begin
  Stream.Read(FHeader, SizeOf(TlicgLayerHeader));
  LayerInfo.SetLayerHeader(FHeader);

  if (FHeader.CS = '') or (FHeader.CS = 'Tanımsız') then
  begin
    if GIS.MapInfo.CS = nil then
      LayerInfo.CS := Licad.CreateCSFactory.ByEPSG(0)
    else
    begin
      if LayerInfo.CS = nil then
        LayerInfo.CS := Licad.CreateCSFactory.ByEPSG(0);
    end;
  end
  else
  begin
    if LayerInfo.CS = nil then
      LayerInfo.CS := Licad.CreateCSFactory.ByEPSG(0);
  end;
  LayerInfo.CS.CSCoordinateSystem := GIS.MapInfo.CS.CSCoordinateSystem;

//  if (FHeader.CS = '') or (FHeader.CS = 'Tanımsız') then
//  begin
//    if GIS.MapInfo.CS = nil then
//      LayerInfo.CS := Licad.CreateCSFactory.ByEPSG(0)
//    else
//      LayerInfo.CS.CSCoordinateSystem := GIS.MapInfo.CS.CSCoordinateSystem;
//  end
//  else
//    LayerInfo.CS := Licad.CreateCSFactory.ByWKT(FHeader.CS);
end;

function TlicgBaseLayer.RecEntity: IlicgEntity;
begin
  Result := nil;
end;

function TlicgBaseLayer.RecEntityID: TlicgEntityID;
begin
  Result := idNone;
end;

function TlicgBaseLayer.RecIsDeleted: Boolean;
begin
  Result := False;
end;

function TlicgBaseLayer.RecIsDeleted(RecNo: Integer): Boolean;
begin
  Result := False;
end;

function TlicgBaseLayer.RecLoadEntity: IlicgEntity;
begin
  Result := nil;
end;

function TlicgBaseLayer.SendEntityToBack(ARecNo: Integer): Integer;
begin
  Result := -1;
end;

procedure TlicgBaseLayer.SetActive(Value: Boolean);
begin

end;

procedure TlicgBaseLayer.SetActiveSearchCanceledEvent(Value: Boolean);
begin
//
end;

procedure TlicgBaseLayer.SetAutoflush(Value: Boolean);
begin
//
end;

procedure TlicgBaseLayer.SetBlinkers(Value: IlicgIntegerList);
begin
  // FBlinkers := Value;
end;

procedure TlicgBaseLayer.SetDBTable(Value: TlicgBaseTable);
begin

end;

procedure TlicgBaseLayer.SetDisplayName(Value: string);
begin

end;

procedure TlicgBaseLayer.SetFileName(const Value: string);
begin
  {
  FFileName   := ExtractFilePath(_GIS.FileName) + ExtractFileName(Value);
  FFileName   := ChangeFileExt(FFileName, '');
  Name        := FFileName;
  DisplayName := ExtractFileName(ChangeFileExt(FFileName,''));
  }
end;

procedure TlicgBaseLayer.SetGIS(Value: TlicgBaseGIS);
begin

end;

procedure TlicgBaseLayer.SetLayerClassType(const Value: TlicgLayerClassType);
begin
   //FLayerClassType := Value;
end;

procedure TlicgBaseLayer.SetLayerInfo(Value: TlicgBaseLayerInfo);
begin

end;

procedure TlicgBaseLayer.SetModified(const Value: Boolean);
begin
{   FModified := Value;
   if not Value then
     LayerInfo.Modified := Value;
     }
end;

procedure TlicgBaseLayer.SetName(const Value: string);
begin
//  FName := Value;
end;

procedure TlicgBaseLayer.SetOnBeforePaintEntity(Value: TlicgBeforePaintEntityEvent);
begin
  // FOnBeforePaintEntity := Value;
end;

procedure TlicgBaseLayer.SetRecNo(Value: Integer);
begin

end;

procedure TlicgBaseLayer.SetSaveHeaderVersion(Value: smallint);
begin
//   FSaveHeaderVersion := Value;
end;

procedure TlicgBaseLayer.SetSpaceKeySelected(Value: Boolean);
begin
//  FSpaceKeySelected := Value;
end;

{ TlicgGISItem }

procedure TlicgGISItem.Assign(Source: TPersistent);
begin
  if Source is TlicgGISItem then
  begin
    FGIS := TlicgGISItem(Source).FGIS;
  end
  else
    inherited Assign(Source);
end;

function TlicgGISItem.GetCaption: string;
begin
  if FGIS = nil then
    result := SGISUnassigned
  else
    result := ExtractFileName(FGIS.FileName);
end;

function TlicgGISItem.GetCategory: string;
begin
  Result := FReferenceInfo.FCategory;
end;

function TlicgGISItem.GetDisplayName: string;
begin
 { result := GetCaption;
  if Result = '' then
    Result := inherited GetDisplayName;}
  Result := FReferenceInfo.FDisplayName;
  if Result = '' then
    Result := GetCaption;
end;

procedure TlicgGISItem.SetDisplayName(Value: string);
begin
  FReferenceInfo.FDisplayName := Value;
end;

function TlicgGISItem.GetGIS: TlicgBaseGIS;
begin
  Result := FGIS
end;

function TlicgGISItem.GetSnapable: Boolean;
begin
  Result := FReferenceInfo.FSnapable;
end;

function TlicgGISItem.GetVisible: Boolean;
begin
  Result := FReferenceInfo.FVisible;
end;

procedure TlicgGISItem.SetCategory(Value: string);
begin
  FReferenceInfo.FCategory := Value;
end;

procedure TlicgGISItem.SetGIS(const Value: TlicgBaseGIS);
begin
  FGIS := Value
end;

procedure TlicgGISItem.SetSnapable(Value: Boolean);
begin
  FReferenceInfo.FSnapable := Value;
end;

procedure TlicgGISItem.SetVisible(Value: Boolean);
begin
  FReferenceInfo.FVisible := Value;
end;

function TlicgGISItem.GetDate: string;
begin
  Result := FReferenceInfo.FDate;
end;

procedure TlicgGISItem.SetDate(Value: string);
begin
  FReferenceInfo.FDate := Value;
end;

function TlicgGISItem.GetDescription: string;
begin
  Result := FReferenceInfo.FDescription;
end;

procedure TlicgGISItem.SetDescription(Value: string);
begin
  FReferenceInfo.FDescription := Value;
end;

{ TlicgGISList }

constructor TlicgGISList.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TlicgGISItem);
end;

function TlicgGISList.Add: TlicgGISItem;
begin
  Result := TlicgGISItem(inherited Add);
end;

function TlicgGISList.GetItem(Index: Integer): TlicgGISItem;
begin
  Result := TlicgGISItem(inherited GetItem(Index));
end;

procedure TlicgGISList.SetItem(Index: Integer; Value: TlicgGISItem);
begin
  inherited SetItem(Index, Value);
end;

{
function convertPointsProjection ( Gis: TlicgBaseGis ;
  InPrj,OutPrj: GM_Projection_tExt ): Boolean;
var
  i,j: Integer;
  L : TlicgBaseLayer;
  E : IlicgEntity;
  P : TlicgCoor;
begin
  result := true;
  CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaUnTransform);

  try
    for i:=0 to Gis.Layers.Count-1 do begin
       L := Gis.Layers.Items[i];
       L.First;
       L.StartBuffering;

       try
         while not L.Eof do begin
           E := L.LoadEntityWithRecNo(L.RecNo);
           if E<>nil then begin
             E.Geometry.DisableEvents := true;
             try
               with Gis.Raster do begin
                  for j:=0 to E.Geometry.Points.Count-1 do begin
                    P := E.Geometry.Points[j];
                    if CSCoorToCSCoor (P.x, P.y, P.x, P.y, @inPrj, @OutPrj) then
                    begin
                      E.Geometry.Points.X[j] := P.x;
                      E.Geometry.Points.Y[j] := P.y;
                    end
                    else begin
                      result := false;
                      exit;
                    end;
                  end;
               end;
             finally
               E.Geometry.DisableEvents := false;

               CurrCmdLine.ActiveDrawBox.Undo.AddUndo(L,L.RecNo,uaUnTransform);
               L.UpdateEntity(L.RecNo, E);

               E := nil;

             end;
           end;
           L.Next;
         end;
       finally
         L.EndBuffering;
       end;
    end;
  finally
    CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
    Gis.UpdateExtension;
    Gis.Save;
  end;

end;
}

function convertPointsProjection(Gis: TlicgBaseGis; InPrj, OutPrj: IlicgCS;
  OntheFlyMatrixElems: PAffineMatrixElements): Boolean;
var
  i, j: Integer;
  L: TlicgBaseLayer;
  E: IlicgEntity;
  P: TlicgCoor;
begin
  result := true;

  CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaUnTransform);

  try
    for i := 0 to Gis.Layers.Count - 1 do
    begin
      L := Gis.Layers.Items[i];
      L.First;
      L.StartBuffering;

      try
        while not L.Eof do
        begin
          E := L.LoadEntityWithRecNo(L.RecNo);
          if E <> nil then
          begin
            try
              E.ApplyProject(Gis, InPrj, OutPrj, OntheFlyMatrixElems);
              E.Geometry.UpdateExtension;
              CurrCmdLine.ActiveDrawBox.Undo.AddUndo(L, L.RecNo, uaUnTransform);
              L.UpdateEntity(L.RecNo, E);
            finally
              E := nil;
            end;
          end;
          L.Next;
        end;
      finally
        L.EndBuffering;
      end;
    end;
  finally
    CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
    Gis.UpdateExtension;
    Gis.Save;
  end;

end;

destructor TlicgGISList.Destroy;
begin

  inherited;
end;


{ TPickedClass }

function TPickedClass.Entity: IlicgEntity;
var
  L: TlicgBaseLayer;
begin
  L := Layer;
  if L <> nil then
  begin
    Result := L.LoadEntityWithRecNo(fRecNo);
  end
end;

function TPickedClass.GetGis: TlicgBaseGis;
begin
  Result := FGIS;
end;

function TPickedClass.GetLayerName: string;
begin
  Result := FLayerName;
end;

function TPickedClass.GetPickedPnt: Integer;
begin
  Result := FPickedPnt;
end;

function TPickedClass.GetRecNo: Integer;
begin
  Result := FrecNo;
end;

function TPickedClass.Layer: TlicgBaseLayer;
begin
  result := nil;
  if FGIS <> nil then
    Result := FGIS.Layers.LayerByName(fLayerName);
end;

function TlicgBaseLayerInfo.GetDefBlockTool: IlicgBlockTool;
begin
  Result := FDefBlockTool;
end;

function TlicgBaseLayerInfo.GetDefBrushTool: IlicgBrushTool;
begin
  Result := FDefBrushTool;
end;

function TlicgBaseLayerInfo.GetDefFontTool: IlicgFontTool;
begin
  Result := FDefFontTool;
end;

function TlicgBaseLayerInfo.GetDefFontToolTT: IlicgFontTool;
begin
  Result := FDefFontToolTT;
end;

function TlicgBaseLayerInfo.GetDefPenTool: IlicgPenTool;
begin
  Result := FDefPenTool;
end;

function TlicgBaseLayerInfo.GetDefSymbolTool: IlicgSymbolTool;
begin
  Result := FDefSymbolTool;
end;

procedure TlicgBaseLayers.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

function TlicgBaseLayers.Get(Index: Integer): TlicgBaseLayer;
begin
  if (Index < 0) or (Index > FItems.Count - 1) then
  begin
    // ilker silme raise Exception.Create(SLayerIndexOutofRange);
    Index := 0; // ilker eklme test et bakalım başka sorun çıkaracak mı
  end;
  Result := FItems[Index];
end;

function TlicgBaseLayers.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TlicgBaseLayers.GetEntityCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do
    Result := Result + TlicgBaseLayer(FItems[I]).RecordCount;
end;

function TlicgBaseLayers.IndexOf(Layer: TlicgBaseLayer): Integer;
begin
  Result := FItems.Indexof(Layer);
end;

function TlicgBaseLayers.IndexOfName(const LayerName: string): Integer;
var
  I: Integer;
  l1: string;
begin
  Result := -1;
  l1 := ExtractFileName(ChangeFileExt(LayerName, ''));
  for I := 0 to FItems.Count - 1 do
  begin
    if (AnsiCompareText(TlicgBaseLayer(FItems[I]).Name, ChangeFileExt(LayerName,
      '')) = 0) or (AnsiCompareText(TlicgBaseLayer(FItems[I]).Name, LayerName) = 0) then
    begin
      Result := I;
      Exit;
    end
    else if (AnsiCompareText(TlicgBaseLayer(FItems[I]).DisplayName, l1) = 0) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TlicgBaseLayers.LayerByName(const LayerName: string): TlicgBaseLayer;
var
  Index: Integer;
begin
  Result := nil;

  Index := IndexofName(LayerName);

  if index < 0 then
    index := IndexofName(ExtractFileName(LayerName));

  if Index < 0 then
    Exit;

  Result := TlicgBaseLayer(FItems[Index]);
end;

function TlicgBaseLayers.LayerDown(Index: Integer): Integer;
begin
  Result := -1;
  if (Index < 0) or (Index >= Count - 1) then
    Exit;
  Result := Index + 1;
  Exchange(Index, Result);
end;

function TlicgBaseLayers.LayerUp(Index: Integer): Integer;
begin
  Result := -1;
  if (Index <= 0) or (Index > Count - 1) then
  begin
    Exit;
  end;
  Result := Index - 1;
  Exchange(Index, Result);
end;

procedure TlicgBaseLayers.OpenLayers;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TlicgBaseLayer(FItems[I]).Open;
end;

procedure TlicgBaseLayers.PopulateList(Strings: TStrings);
var
  I: Integer;
begin
  with Strings do
  begin
    BeginUpdate;
    Clear;
    for I := 0 to FItems.Count - 1 do
      AddObject(TlicgBaseLayer(FItems[I]).DisplayName, TlicgBaseLayer(FItems[I]));
    EndUpdate;
  end;
end;

function TlicgBaseLayers.RemoveLayer(Layer: TlicgBaseLayer): Integer;
begin
  Result := FItems.Remove(Layer);
end;

function TlicgBaseLayers.SendToBack(Index: Integer): Integer;
var
  I: Integer;
  Temp: Pointer;
begin
  Result := 0;
  if (Index < 0) or (Index >= Count - 2) then
    Exit;
  Temp := FItems[Index];
  for I := Index downto 1 do
    FItems[I] := FItems[I - 1];
  FItems[0] := Temp;
  Result := 0;
end;

procedure TlicgBaseGIS.SortLayers(SortType: TlicgLayerSortType);

  function AZSort(V1, V2: Pointer): Integer;
  var
    L, N: Integer;
    AV1, AV2: string;
  begin
    AV1 := Trim(TlicgBaseLayer(V1).DisplayName);
    AV2 := Trim(TlicgBaseLayer(V2).DisplayName);

    if Pos('#', AV1) > 0 then
      AV1 := Copy(AV1, Pos('#', AV1)+1, Length(AV1)-1);

    if Pos('#', AV2) > 0 then
      AV2 := COPY(AV2, Pos('#', AV2) + 1, Length(AV2) - 1);

    //Result := StrCmpLogicalW(AV1, AV2);
    (* ilker değiştirme eskisi
    if AV1 = AV2 then
      Result := 0
    else if AV1 > AV2 then
      Result := -1
    else
      Result := 1;       *)

    Result := AnsiCompareStr(AV1, AV2);
  end;

  function ZASort(V1, V2: Pointer): Integer;
  var
    L, N: Integer;
    AV1, AV2: string;
  begin
    Result :=  AZSort(V1, V2) * -1;  // ilker değiştirdim

    Exit;
    if TlicgBaseLayer(V1).DisplayName = TlicgBaseLayer(V2).DisplayName then
      Result := 0
    else if TlicgBaseLayer(V1).DisplayName > TlicgBaseLayer(V2).DisplayName then
      Result := 1
    else
      Result := -1;
  end;

  function RecordCountSortAZ(V1, V2: Pointer): Integer;
  begin
    if TlicgBaseLayer(V1).RecordCount = TlicgBaseLayer(V2).RecordCount then
      Result := 0
    else if TlicgBaseLayer(V1).RecordCount > TlicgBaseLayer(V2).RecordCount then
      Result := -1
    else
      Result := 1;
  end;

  function RecordCountSortZA(V1, V2: Pointer): Integer;
  begin
    if TlicgBaseLayer(V1).RecordCount = TlicgBaseLayer(V2).RecordCount then
      Result := 0
    else if TlicgBaseLayer(V1).RecordCount > TlicgBaseLayer(V2).RecordCount then
      Result := 1
    else
      Result := -1;
  end;

  function ColorSortAZ(V1, V2: Pointer): Integer;
  begin
    if TlicgBaseLayer(V1).LayerInfo.Color = TlicgBaseLayer(V2).LayerInfo.Color then
      Result := 0
    else if TlicgBaseLayer(V1).LayerInfo.Color > TlicgBaseLayer(V2).LayerInfo.Color then
      Result := -1
    else
      Result := 1;
  end;

  function ColorSortZA(V1, V2: Pointer): Integer;
  begin
    if TlicgBaseLayer(V1).LayerInfo.Color = TlicgBaseLayer(V2).LayerInfo.Color then
      Result := 0
    else if TlicgBaseLayer(V1).LayerInfo.Color > TlicgBaseLayer(V2).LayerInfo.Color then
      Result := 1
    else
      Result := -1;
  end;

var
  I: Integer;
begin
  try
    case SortType of
      lstAZ:
        begin
          Layers.FItems.Sort(@AZSort);
        end;
      lstZA:
        begin
          Layers.FItems.Sort(@ZASort);
        end;
      lstRecordCountAZ:
        begin
          Layers.FItems.Sort(@RecordCountSortAZ);
        end;
      lstRecordCountZA:
        begin
          Layers.FItems.Sort(@RecordCountSortZA);
        end;
      lstColorAZ:
        begin
          Layers.FItems.Sort(@ColorSortAZ);
        end;
      lstColorZA:
        begin
          Layers.FItems.Sort(@ColorSortZA);
        end;
    end;
  finally
    Modified := True;
  end;
end;

procedure TlicgEntityList._Set(Index: Integer; Value: IlicgEntity);
begin
  FList[index] := Value;
end;

function TPickedClass.Pick: IPickedClass;
begin
  Result := Self;
end;

procedure TPickedClass.SetGis(Value: TlicgBaseGis);
begin
  FGIS := Value;
end;

procedure TPickedClass.SetLayerName(Value: string);
begin
  FlayerName := Value;
end;

procedure TPickedClass.SetPickedPnt(Value: Integer);
begin
  FPickedPnt := Value;
end;

procedure TPickedClass.SetRecNo(Value: Integer);
begin
  FRecNo := Value;
end;

{ TlicgDrawBoxList }

procedure TlicgDrawBoxList.Add(Item: TlicgBaseDrawBox);
begin
  FItems.Add(Item);
  if Assigned(item) and Assigned(item.GIS) and Assigned(item.GIS.ReferenceGIS) then
  begin
    (item.GIS.ReferenceGIS as IlicgReferenceGIS).AddDrawBox(item);
  end;
end;

constructor TlicgDrawBoxList.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

procedure TlicgDrawBoxList.Delete(Index: Integer);
var
  item: TlicgBaseDrawBox;
begin
  if (Index < 0) or (Index > FItems.Count - 1) then
    exit;

  item := TlicgBaseDrawBox(Self.FItems[Index]);
  if Assigned(item) and Assigned(item.GIS) and Assigned(item.GIS.ReferenceGIS) then
  begin
    (item.GIS.ReferenceGIS as IlicgReferenceGIS).DeleteDrawBox(item);
  end;

  FItems.Delete(Index);

end;

destructor TlicgDrawBoxList.destroy;
begin
  FItems.clear;
  FItems.Free;
  inherited;
end;

function TlicgDrawBoxList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TlicgDrawBoxList.GetItem(Index: Integer): TlicgBaseDrawBox;
begin
  Result := FItems[Index];
end;

function TlicgDrawBoxList.IndexOf(Item: TlicgBaseDrawBox): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if FItems[I] = Item then
    begin
      Result := I;
      exit;
    end;
end;

{ TlicgCustomPaperList }

constructor TlicgCustomPaperList.Create(AOwner: TPersistent);
begin

  inherited Create(AOwner, TlicgCustomPaperItem);

  with Add do
  begin
    Name := SpsPrinter;
  end;
  with Add do
  begin
    Name := SpsLetter;
    WidthInches := 8.5;
    HeightInches := 11.0;
  end;
  with Add do
  begin
    Name := SpsLegal;
    WidthInches := 8.5;
    HeightInches := 14.0;
  end;
  with Add do
  begin
    Name := SpsLedger;
    WidthInches := 11;
    HeightInches := 17;
  end;
  with Add do
  begin
    Name := SpsStatement;
    WidthInches := 5.5;
    HeightInches := 8.5;
  end;
  with Add do
  begin
    Name := SpsExecutive;
    WidthInches := 7.25;
    HeightInches := 10.5;
  end;
  with Add do
  begin
    Name := SpsA3;
    WidthInches := 11.69;
    HeightInches := 16.54;
  end;
  with Add do
  begin
    Name := SpsA4;
    WidthInches := 8.27;
    HeightInches := 11.69;
  end;
  with Add do
  begin
    Name := SpsA5;
    WidthInches := 5.83;
    HeightInches := 8.27;
  end;
  with Add do
  begin
    Name := SpsB3;
    WidthInches := 14.33;
    HeightInches := 20.28;
  end;
  with Add do
  begin
    Name := SpsB4;
    WidthInches := 10.12;
    HeightInches := 14.33;
  end;
  with Add do
  begin
    Name := SpsB5;
    WidthInches := 7.17;
    HeightInches := 10.12;
  end;
  with Add do
  begin
    Name := SpsFolio;
    WidthInches := 8.5;
    HeightInches := 13;
  end;
  with Add do
  begin
    Name := SpsQuarto;
    WidthInches := 8.47;
    HeightInches := 10.83;
  end;
  with Add do
  begin
    Name := Sps10x14;
    WidthInches := 10;
    HeightInches := 14;
  end;
  with Add do
  begin
    Name := Sps11x17;
    WidthInches := 11;
    HeightInches := 17;
  end;
  with Add do
  begin
    Name := SpsCsize;
    WidthInches := 17;
    HeightInches := 22;
  end;
  with Add do
  begin
    Name := SpsUSStdFanfold;
    WidthInches := 11;
    HeightInches := 14.88;
  end;
  with Add do
  begin
    Name := SpsGermanStdFanfold;
    WidthInches := 8.5;
    HeightInches := 12;
  end;
  with Add do
  begin
    Name := SpsGermanLegalFanfold;
    WidthInches := 8.5;
    HeightInches := 13;
  end;
  with Add do
  begin
    Name := Sps6x8;
    WidthInches := 6;
    HeightInches := 8;
  end;
  with Add do
  begin
    Name := SpsFoolscap;
    WidthInches := 13.5;
    HeightInches := 17;
  end;
  with Add do
  begin
    Name := SpsLetterPlus;
    WidthInches := 9;
    HeightInches := 13.3;
  end;
  with Add do
  begin
    Name := SpsA4Plus;
    WidthInches := 8.77;
    HeightInches := 14;
  end;
end;

function TlicgCustomPaperList.Add: TlicgCustomPaperItem;
begin
  Result := TlicgCustomPaperItem(inherited Add);
end;

function TlicgCustomPaperList.GetItem(Index: Integer): TlicgCustomPaperItem;
begin
  Result := TlicgCustomPaperItem(inherited GetItem(Index));
end;

procedure TlicgCustomPaperList.SetItem(Index: Integer; Value: TlicgCustomPaperItem);
begin
  inherited SetItem(Index, Value);
end;

function TlicgCustomPaperList.GetListPrintPapers: TStrings;
type
  TPaperName = array[0..63] of AnsiChar; // ilker değiştirme
const
  PaperMax = (4294967295 div 2) div Sizeof(TPaperName);
type
  TPaperNameArray = array[1..PaperMax] of TPaperName;

  PPapernameArray = ^TPaperNameArray;
var
  Device, Driver, Port: array[0..255] of Char;
  hDevMode: THandle;
  i, numPaperformats: Integer;
  pPaperFormats: PPapernameArray;
begin

  Result := TStringList.Create;

  Printer.PrinterIndex := -1;

  Printer.GetPrinter(Device, Driver, Port, hDevmode);
  numPaperformats := WinSpool.DeviceCapabilities(Device, Port, DC_PAPERNAMES, Nil, Nil);
  if numPaperformats > 0 then
  begin
    GetMem(pPaperformats, numPaperformats * Sizeof(TPapername));
    try
      WinSpool.DeviceCapabilities(Device, Port, DC_PAPERNAMES, Pchar(pPaperFormats), Nil);

      for i := 1 to numPaperformats do
        Result.Add(pPaperformats^[i]);
    finally
      FreeMem(pPaperformats);
    end;
  end;

end;

{ TlicgCustomPaperItem }

procedure TlicgCustomPaperItem.Assign(Source: TPersistent);
begin
  if Source is TlicgCustomPaperItem then
  begin
    FName := TlicgCustomPaperItem(Source).FName;
    FWidthInches := TlicgCustomPaperItem(Source).FWidthInches;
    FHeightInches := TlicgCustomPaperItem(Source).FHeightInches;
  end
  else
    inherited Assign(Source);
end;

function TlicgCustomPaperItem.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := Format('%s - %f X %f', [FName, FWidthInches, FHeightInches]);
end;

function TlicgCustomPaperItem.GetHeightInches: Single;
begin
  Result := FHeightInches
end;

function TlicgCustomPaperItem.GetName: string;
begin
  Result := FName
end;

function TlicgCustomPaperItem.GetWidthInches: Single;
begin
  Result := FWidthInches
end;

procedure TlicgCustomPaperItem.SetHeightInches(const Value: Single);
begin
  FHeightInches := Value
end;

procedure TlicgCustomPaperItem.SetName(const Value: string);
begin
  FName := Value
end;

procedure TlicgCustomPaperItem.SetWidthInches(const Value: Single);
begin
  FWidthInches := Value
end;

{ TlicgBaseDrawBox }

procedure TlicgBaseDrawBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TlicgBaseDrawBox.CreateWnd;
begin
  inherited CreateWnd;
  //DoubleBuffered := True; // ilker ekleme fare ile tutup kaydırıken siyah ekran yapıyor
end;

procedure TlicgBaseDrawBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TlicgBaseDrawBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TlicgBaseDrawBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TlicgBaseDrawBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TlicgBaseDrawBox.Paint;
begin
  inherited;
end;

procedure TlicgBaseDrawBox.SystemKeydown(var message: Tmessage);
begin
  if Assigned(FSystemKeyDownEvent) then
    FSystemKeyDownEvent(Self, message.WParam);
end;

procedure TlicgBaseDrawBox.Repaint;
begin
  inherited;
end;

procedure TlicgBaseDrawBox.ViewChanged(Sender: TObject);
begin

end;

function TlicgBaseLayerInfo.DisplayUser: Boolean;
begin
  result := true; // not IsNetworkLayer;
end;

function FindClosestEntity(const GIS: TlicgBaseGis; const WX, WY: Double; const
  Scope: string; const ShapeType: TlicgEntityIDs; Layer: TlicgBaseLayer; var
  RecNo: Integer): Boolean;
const
  FIND_MAX_ITER = 50;
var
  Circle: IlicgEntity;
  Pivot: IlicgVector;
  Radius: Double;
  NumIter: Integer;

  function QueryFindEnt(var qRecNo: Integer): Boolean;
  var
    I: Integer;
    TestDist, MinDist: Double;
    MinP1, Minp2: TlicgCoor;
    List: IlicgIntegerList;
    TmpEnt: IlicgEntity;
    inside: Boolean;
  begin
    qRecNo := -1;
    List := CreateIntegerList;
    TestDist := _MAXCOOR;
    inside := false;
    try

      GIS.QuerySelectTool.QueryPolygon(Circle, Layer, Scope, goWithin, List, True);

      inside := false;
      for I := 0 to List.Count - 1 do
      begin
        TmpEnt := Layer.LoadEntityWithRecNo(List[I]);
        try
          if (ShapeType = []) or (TmpEnt.EntityID in ShapeType) then
          begin

            if (TmpEnt.InPoly(Pivot[0])) or (TmpEnt.Geometry.Points.PointOnMe
              (Pivot[0].x, Pivot[0].y) > -1) then
            begin
              qRecNo := List[I];
              if not Inside then
              begin
                if TmpEnt.Geometry.Points.Count > 1 then
                  Lider.CG.Com.Lib.GetMinimumDistance2D(TmpEnt.Geometry.Points,
                    Pivot, MinDist, MinP1, MinP2)
                else if TmpEnt.Geometry.Points.Count = 1 then
                  MinDist := _Distance(TmpEnt.Geometry.Points[0], Pivot[0])
                else
                  MinDist := _MAXCOOR;

                TestDist := MinDist;
                inside := true;
                Continue;
              end;

              if TmpEnt.Geometry.Points.Count > 1 then
                Lider.CG.Com.Lib.GetMinimumDistance2D(TmpEnt.Geometry.Points,
                  Pivot, MinDist, MinP1, MinP2)
              else if TmpEnt.Geometry.Points.Count = 1 then
                MinDist := _Distance(TmpEnt.Geometry.Points[0], Pivot[0])
              else
                MinDist := _MAXCOOR;

              if MinDist < TestDist then
              begin
                TestDist := MinDist;
                qRecNo := List[I];
              end;

            end;

            if not Inside then
            begin
              if TmpEnt.Geometry.Points.Count > 1 then
                Lider.CG.Com.Lib.GetMinimumDistance2D(TmpEnt.Geometry.Points,
                  Pivot, MinDist, MinP1, MinP2)
              else if TmpEnt.Geometry.Points.Count = 1 then
                MinDist := _Distance(TmpEnt.Geometry.Points[0], Pivot[0])
              else
                MinDist := _MAXCOOR;

              if MinDist < TestDist then
              begin
                TestDist := MinDist;
                qRecNo := List[I];
              end;
            end;

          end;

        finally
          TmpEnt := nil;
        end;
      end;

    finally
      result := qRecNo <> -1;
      List := nil;
    end;
  end;

  function Dofind: Boolean;
  var
    I, Rc: Integer;
    qRecNo: Integer;
    ResSel: TlicgBaseSelection;
    Found: Boolean;
    Layer: TlicgBaseLayer;
  begin

    Result := False;

    NumIter := 0;
    Radius := 0.5; //FGrapher.PointsToDistY(1);
    Circle.Geometry.DisableEvents := true;
    Circle.Geometry.Points[0] := AsCoor(WX - Radius, WY - Radius);
    Circle.Geometry.Points[1] := AsCoor(WX + Radius, WY + Radius);
    Circle.Geometry.DisableEvents := false;

    repeat

      result := QueryFindEnt(qRecNo);

      if not Result then
      begin

        Inc(NumIter);
        if NumIter > FIND_MAX_ITER then
          Break;

        Radius := Radius * 2;

        Circle.Geometry.DisableEvents := true;
        Circle.Geometry.Points[0] := AsCoor(WX - Radius, WY - Radius);
        Circle.Geometry.Points[1] := AsCoor(WX + Radius, WY + Radius);
        Circle.Geometry.DisableEvents := false;
      end
      else
      begin
        RecNo := qRecNo;
      end;

    until result;

  end;

begin

  if (GIS = nil) then
    exit;

  Assert(GIS <> nil);

  Result := False;

  if not Assigned(Layer) then
    exit;

  RecNo := -1;
  Radius := 0.5; //FGrapher.PointsToDistY(1);

  Circle := Licad.CreateEntityFactory.MakeEllipse(AsCoor(WX - Radius, WY -
    Radius), AsCoor(WX + Radius, WY + Radius));
  Pivot := Licad.CreateEntityFactory.MakeVector(3, 1);

  try
    Pivot.Add(WX, WY);

    Result := Dofind;

  finally
    Circle := nil;
    Pivot := nil;
  end;

end;

constructor TSPClass1.create(_startPoint: TlicgCoor; _Name: string; _DirL:
  TStrings; _EL: TlicgEntitylist; _TotalCosts: IlicgDoubleList);
var
  i: Integer;
begin
  inherited create;
  Name := _Name;
  startPoint := _startPoint;
  DirL := TStringList.Create;
  DirL.Assign(_DirL);

  TotalCost := 0;
  for i := 0 to _TotalCosts.Count - 1 do
    TotalCost := TotalCost + _TotalCosts.Items[i];

  EL := _EL;

end;

destructor TSPClass1.Destroy;
begin
  DirL.Free;
  if EL <> nil then
    EL.free;
  inherited;
end;

{ TEntityIntStringList }

function TEntityIntStringList.GetEntity(Index: Integer): IlicgEntity;
begin
  Result := nil;
  if (index >= 0) and (index < count) then
    Result := _INT[index] as IlicgEntity;
end;

function TlicgBaseLayerInfo.GetIsCosmethic: Boolean;
begin
  Result := false;
end;

function TlicgBaseLayerInfo.GetIsAnimationLayer: Boolean;
begin
  Result := false;
end;

function TlicgBaseLayer.AddEntity(Entity: IlicgEntity; infoid: Integer;
  ApplyLayerProps: Boolean = true; GetfirstPropFromLayer: Boolean = true; _Param:
  string = ''): Integer;
begin

end;

procedure TlicgBaseLayer.UpdateApplyLayerPropsEntity(RecNo: Integer; Entity:
  IlicgEntity; _ApplyLayerProps: Boolean; Grapher: TlicgBaseGrapher);
begin
  if Assigned(Entity.Layer) then
  begin
    UpdateEntity(RecNo, Entity, Grapher);
  end;
end;

function TlicgBaseLayerInfo.GetLabeling: Boolean;
begin
  Result := Boolean(Reserved[Reserved_Labeling]);
end;

procedure TlicgBaseLayerInfo.SetLabeling(Value: Boolean);
begin
  Reserved[Reserved_Labeling] := Integer(Value);
  Modified := true;
end;

procedure TlicgBaseLayer.SetAktifEntity(RecNo: Integer);
begin

end;

procedure TlicgBaseLayer.SetPasifEntity(RecNo: Integer);
begin

end;

{ TlicgLayerInfo implementation }

constructor TlicgLayerInfo.Create(Layer: TlicgBaseLayer);
begin
  inherited Create(Layer);
  FDefSymbolTool := TlicgLayerSymbolTool.Create(Self);
  FDefPenTool := TlicgLayerPenTool.Create(Self);
  FDefBrushTool := TlicgLayerBrushTool.Create(Self);
  FDefFontToolTT := TlicgLayerFontToolTT.Create(Self);
  FDefFontTool := TlicgLayerFontTool.Create(Self);
  FModified := false;

end;

function TlicgLayerInfo.GetIsCosmethic: Boolean;
begin
  Result := FHeader.IsMemoryLayer;
end;

procedure TlicgLayerInfo.SetIsCosmethic(Value: Boolean);
begin
  if FHeader.IsMemoryLayer <> Value then
  begin
    FHeader.IsMemoryLayer := Value;
    Modified := True;
  end;
end;

procedure TlicgLayerInfo.SetWithConnectors(Value: Boolean);
begin
  FHeader.WithConnectors := Value;
end;

function TlicgLayerInfo.GetWithConnectors: Boolean;
begin
  Result := FHeader.WithConnectors;
end;

function TlicgLayerInfo.GetLayerBrush: Boolean;
begin
  if not (Assigned(FLayer.GIS) and FLayer.GIS.UseReservedData) then
    Result := Boolean(FHeader.Reserved[Reserved_LayerBrush])
  else
    Result := FLayer.GIS.Get_Reserved_LayerBrush and Boolean(FHeader.Reserved[Reserved_LayerBrush]);
end;

function TlicgLayerInfo.GetColor: TColor;
begin
  Result := Self.DefPenTool.Color;
end;

procedure TlicgLayerInfo.SetLayerBrush(Value: Boolean);
var
  _brush: byte;
begin
  if (isLayerBrush <> Value) then
  begin
    if Value then
      _brush := 1
    else
      _brush := 0;
    if FHeader.Reserved[Reserved_LayerBrush] <> _brush then
    begin
      FHeader.Reserved[Reserved_LayerBrush] := _brush;
      Modified := True;
    end;
  end;
end;

procedure TlicgLayerInfo.SetColor(Value: TColor);
var
  AColor: TColor;
begin
  AColor := Self.DefPenTool.Color;

  if Self.DefBrushTool.ForeColor = AColor then
    Self.DefBrushTool.ForeColor := Value;
  if Self.DefBrushTool.BackColor = AColor then
    Self.DefBrushTool.BackColor := Value;

  if Self.DefFontTool.Color = AColor then
    Self.DefFontTool.Color := Value;

  if Self.DefFontToolTT.Color = AColor then
    Self.DefFontToolTT.Color := Value;

  Self.DefPenTool.Color := Value;
  Modified := True;
end;

function TlicgLayerInfo.GetLocked: Boolean;
begin
  Result := FHeader.Locked;
end;

procedure TlicgLayerInfo.SetLocked(Value: Boolean);
begin
  FHeader.Locked := Value;
  Modified := True;
end;

function TlicgLayerInfo.GetMaxZoomScale: Double;
begin
  Result := FHeader.MaxZoomScale;
end;

function TlicgLayerInfo.GetMaxZoomScaleForText: Double;
begin
  Result := FHeader.MaxZoomScaleForText;
end;

function TlicgLayerInfo.GetMinZoomScale: Double;
begin
  Result := FHeader.MinZoomScale;
end;

function TlicgLayerInfo.GetMinZoomScaleForText: Double;
begin
  Result := FHeader.MinZoomScaleForText;
end;

function TlicgLayerInfo.GetModified: Boolean;
begin
  result := FModified or DefSymbolTool.Modified or DefPenTool.Modified or
    DefBrushTool.Modified or DefFontToolTT.Modified or DefFontTool.Modified;
end;

procedure TlicgLayerInfo.SetMaxZoomScale(Value: Double);
begin
  FHeader.MaxZoomScale := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetMaxZoomScaleForText(Value: Double);
begin
  FHeader.MaxZoomScaleForText := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetMinZoomScale(Value: Double);
begin
  FHeader.MinZoomScale := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetMinZoomScaleForText(Value: Double);
begin
  FHeader.MinZoomScaleForText := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetModified(Value: Boolean);
begin
  FModified := Value;
  if not Value then
  begin
    DefSymbolTool.Modified := Value;
    DefPenTool.Modified := Value;
    DefBrushTool.Modified := Value;
    DefFontToolTT.Modified := Value;
    DefFontTool.Modified := Value;
  end;
end;

function TlicgLayerInfo.GetUseAttachedDB: Boolean;
begin
  Result := FHeader.UseAttachedDB;
end;

procedure TlicgLayerInfo.SetUseAttachedDB(Value: Boolean);
begin
  if FHeader.UseAttachedDB <> Value then
  begin
    FHeader.UseAttachedDB := Value;
    Modified := True;
  end;
end;

procedure TlicgLayerInfo.SetTextFixed(const Value: Boolean);
begin
  if FHeader.TextFixed <> Value then
  begin
    FHeader.TextFixed := Value;
    Modified := True;
  end;
end;

procedure TlicgLayerInfo.SetTextZoomRangeActive(Value: Boolean);
begin
  FHeader.TextZoomRangeActive := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetThickness(const Value: Boolean);
begin
  FHeader.Thickness := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetThicknessWidth(const Value: Integer);
begin
  FHeader.ThicknessWidth := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetGuideIsoline(const Value: Boolean);
begin
  FHeader.GuideIsoline := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetGuideIsolineInterval(const Value: Double);
begin
  FHeader.GuideIsolineInterval := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetDrawDirectionArrow(const Value: Boolean);
begin
  FHeader.DrawDirectionArrow := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetTextFixedSize(const Value: Integer);
begin
  if FHeader.TextFixedSize <> Value then
  begin
    FHeader.TextFixedSize := Value;
    Modified := True;
  end;
end;

function TlicgLayerInfo.GetExtension: TlicgExtent;
begin
  if Assigned(FLayer) and (FLayer.GIS <> nil) and (FLayer.GIS.MapInfo.IsOntheFly) then
  begin
    Result := FHeader.Extent;
    if not EqualExtension(Result, _INVALID_EXTENT) then
    begin
      FLayer.GIS.CSCoorToCSCoor(Result.LowerLeft.X, Result.LowerLeft.Y, Result.LowerLeft.X,
        Result.LowerLeft.Y, fLayer.GIS.MapInfo.CS, FLayer.GIS.MapInfo.OntheFlyCS, nil);

      FLayer.GIS.CSCoorToCSCoor(Result.UpperRight.X, Result.UpperRight.Y, Result.UpperRight.X,
        Result.UpperRight.Y, flayer.GIS.MapInfo.CS, FLayer.GIS.MapInfo.OntheFlyCS, nil);
    end;
  end
  else
    Result := FHeader.Extent;
end;

procedure TlicgLayerInfo.SetExtension(const Value: TlicgExtent);
begin
  if Assigned(FLayer) and (FLayer.GIS <> nil) and (FLayer.GIS.MapInfo.IsOntheFly) then
    Exit;

  if not EqualExtension(Value, FHeader.Extent) then
  begin
    FHeader.Extent := Value;
    Modified := True;
  end;
end;

function TlicgLayerInfo.GetIDCounter: Integer;
begin
  Result := FHeader.IDCounter;
end;

procedure TlicgLayerInfo.SetIDCounter(Value: Integer);
begin
  if FHeader.IDCounter <> Value then
  begin
    FHeader.IDCounter := Value;
    Modified := True;
  end;
end;

function TlicgLayerInfo.GetIsAnimationLayer: Boolean;
begin
  Result := FHeader.IsAnimationLayer;
end;

procedure TlicgLayerInfo.SetIsAnimationLayer(Value: Boolean);
begin
  if FHeader.IsAnimationLayer <> Value then
  begin
    FHeader.IsAnimationLayer := Value;
    Modified := True;
  end;
end;

function TlicgLayerInfo.GetCS: IlicgCS;
begin
  Result := Licad.CreateCSFactory.ByWKT(FHeader.CS);
end;

procedure TlicgLayerInfo.SetCS(Value: IlicgCS);
begin
  if Value = nil then
  begin
    FHeader.CS := '';
    Exit;
  end;

  if FHeader.CS <> Value.FullWKT then
  begin
    FHeader.CS := Value.FullWKT;
    Modified := True;
  end;
end;

procedure TlicgLayerInfo.SetSelectable(Value: Boolean);
begin

{burayi sildimmm, buranın amaci, birkaç drawbox olması durumunda,
      bir layera set edilen özelliği, diğer drawboxlara da dağıtmak imiş.
      Özelliğin tekrar gelmesi gerekiyor.

 if FLayer = nil then
    Exit;
  with FLayer.Layers.GIS do
  begin
    for I := 0 to DrawBoxList.Count - 1 do
      DrawBoxList[I].Selection.DeleteLayer(FLayer);
  end;}
  if FHeader.Selectable <> Value then
  begin
    FHeader.Selectable := Value;
    Modified := True;
  end;
end;

procedure TlicgLayerInfo.SetVisible(Value: Boolean);
begin
{burayi sildimmm, buranın amaci, birkaç drawbox olması durumunda,
      bir layera set edilen özelliği, diğer drawboxlara da dağıtmak imiş.
      Özelliğin tekrar gelmesi gerekiyor.

  if FLayer = nil then
    exit;
  with FLayer.Layers.GIS do
  begin
    for I := 0 to DrawBoxList.Count - 1 do
      DrawBoxList[I].Selection.DeleteLayer(FLayer);
  end;}

  if FHeader.Visible <> Value then
  begin
    FHeader.Visible := Value;
    Modified := True;
  end;
end;

procedure TlicgLayerInfo.SetBalastro(Value: Boolean);
var
  balastroONOFF: byte;
begin
  if (Balastro <> Value) then
  begin
    if Value then
      balastroONOFF := 1
    else
      balastroONOFF := 0;
    if FHeader.Reserved[Reserved_Balastro] <> balastroONOFF then
    begin
      FHeader.Reserved[Reserved_Balastro] := balastroONOFF;
      Modified := True;
    end;
  end;
end;

procedure TlicgLayerInfo.SetBufferWidthForPolyline(Value: Double);
begin
  FHeader.BufferWidthForPolyline := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetPenNo(Value: Byte);
begin
  if (PenNo <> Value) then
  begin
    FHeader.Reserved[Reserved_PenNo] := Value;
    Modified := True;
  end;
end;

procedure TlicgLayerInfo.SetPointNameONOFF(Value: Boolean);
var
  ValueONOFF: byte;
begin
  if (PointNameOnOFF <> Value) then
  begin
    if Value then
      ValueONOFF := 1
    else
      ValueONOFF := 0;
    if FHeader.Reserved[Reserved_PointName] <> ValueONOFF then
    begin
      FHeader.Reserved[Reserved_PointName] := ValueONOFF;
      Modified := True;
    end;
  end;
end;

procedure TlicgLayerInfo.SetPointSize(const Value: Double);
begin
  FHeader.PointSize := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetPointSymbolFileIndex(const Value: Byte);
begin
  FHeader.PointSymbolFileIndex := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetPointSymbolIndex(const Value: Byte);
begin
  FHeader.PointSymbolIndex := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetPolygonNameONOFF(Value: Boolean);
var
  ValueONOFF: byte;
begin

  if (PolygonNameOnOFF <> Value) then
  begin
    if Value then
      ValueONOFF := 1
    else
      ValueONOFF := 0;
    if FHeader.Reserved[Reserved_PolygonName] <> ValueONOFF then
    begin
      FHeader.Reserved[Reserved_PolygonName] := ValueONOFF;
      Modified := True;
    end;
  end;
end;

procedure TlicgLayerInfo.SetPrintONOFF(Value: Boolean);
var
  ValueONOFF: byte;
begin

  if (PrintOnOFF <> Value) then
  begin
    if Value then
      ValueONOFF := 1
    else
      ValueONOFF := 0;
    if FHeader.Reserved[Reserved_Print] <> ValueONOFF then
    begin
      FHeader.Reserved[Reserved_Print] := ValueONOFF;
      Modified := True;
    end;
  end;
end;

function TlicgLayerInfo.GetTextFixed: Boolean;
begin
  Result := FHeader.TextFixed;
end;

function TlicgLayerInfo.GetTextZoomRangeActive: Boolean;
begin
  Result := FHeader.TextZoomRangeActive;
end;

function TlicgLayerInfo.GetThickness: Boolean;
begin
  Result := FHeader.Thickness;
end;

function TlicgLayerInfo.GetThicknessWidth: Integer;
begin
  Result := FHeader.ThicknessWidth;
end;

function TlicgLayerInfo.GetGuideIsoline: Boolean;
begin
  Result := FHeader.GuideIsoline;
end;

function TlicgLayerInfo.GetGuideIsolineInterval: Double;
begin
  Result := FHeader.GuideIsolineInterval;
end;

function TlicgLayerInfo.GetDrawDirectionArrow: Boolean;
begin
  Result := FHeader.DrawDirectionArrow;
end;

function TlicgLayerInfo.GetTextFixedSize: Integer;
begin
  Result := FHeader.TextFixedSize;
end;

function TlicgLayerInfo.GetVisible: Boolean;
begin
  Result := FHeader.Visible;
end;

function TlicgLayerInfo.GetSelectable: Boolean;
begin
  Result := FHeader.Selectable;
end;

function TlicgLayerInfo.GetApplyDefSymbol: Boolean;
begin
  Result := FHeader.ApplyDefSymbol;
end;

function TlicgLayerInfo.GetApplyPointLayerValues: Boolean;
begin
  Result := FHeader.ApplyPointLayerValues;
end;

function TlicgLayerInfo.GetApplyDefPen: Boolean;
begin
  Result := FHeader.ApplyDefPen;
end;

function TlicgLayerInfo.GetApplyDefBrush: Boolean;
begin
  Result := FHeader.ApplyDefBrush;
end;

function TlicgLayerInfo.GetApplyDefFontTT: Boolean;
begin
  Result := FHeader.ApplyDefFontTT;
end;

function TlicgLayerInfo.GetApplyDefFont: Boolean;
begin
  Result := FHeader.ApplyDefFont;
end;

procedure TlicgLayerInfo.SetApplyDefSymbol(Value: Boolean);
begin
  FHeader.ApplyDefSymbol := Value;
end;

procedure TlicgLayerInfo.SetApplyPointLayerValues(const Value: Boolean);
begin
  FHeader.ApplyPointLayerValues := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetApplyDefPen(Value: Boolean);
begin
  FHeader.ApplyDefPen := Value;
end;

procedure TlicgLayerInfo.SetApplyDefBrush(Value: Boolean);
begin
  FHeader.ApplyDefBrush := Value;
end;

procedure TlicgLayerInfo.SetApplyDefFontTT(Value: Boolean);
begin
  FHeader.ApplyDefFontTT := Value;
end;

procedure TlicgLayerInfo.SetApplyDefFont(Value: Boolean);
begin
  FHeader.ApplyDefFont := Value;
end;

function TlicgLayerInfo.GetTolerance: Double;
begin
  Result := FHeader.Tolerance;
end;

function TlicgLayerInfo.GetTransparent: Boolean;
begin
  Result := Boolean(FHeader.Reserved[Reserved_LayerTransparent]);
end;

procedure TlicgLayerInfo.SetTransparent(const Value: Boolean);
var
  AValue: Byte;
begin
  if (Transparent <> Value) then
  begin
    if Value then
      AValue := 1
    else
      AValue := 0;

    if FHeader.Reserved[Reserved_LayerTransparent] <> AValue then
    begin
      FHeader.Reserved[Reserved_LayerTransparent] := AValue;
      Modified := True;
    end;
  end;
end;

function TlicgLayerInfo.GetTransparency: Byte;
begin
  Result := FHeader.Reserved[Reserved_LayerTransparency];
end;

procedure TlicgLayerInfo.SetTransparency(const Value: Byte);
begin
  if (Transparency <> Value) then
  begin
    if FHeader.Reserved[Reserved_LayerTransparency] <> Value then
    begin
      FHeader.Reserved[Reserved_LayerTransparency] := Value;
      Modified := True;
    end;
  end;
end;

procedure TlicgLayerInfo.SetTolerance(const Value: Double);
begin
  FHeader.Tolerance := Value;
end;

function TlicgLayerInfo.GetBalastro: Boolean;
begin
  if not (Assigned(FLayer.GIS) and FLayer.GIS.UseReservedData) then
    Result := Boolean(FHeader.Reserved[Reserved_Balastro])
  else
    Result := FLayer.GIS.Get_Reserved_Balastro and Boolean(FHeader.Reserved[Reserved_Balastro]);
end;

function TlicgLayerInfo.GetBufferWidthForPolyline: Double;
begin
  Result := FHeader.BufferWidthForPolyline;
end;

function TlicgLayerInfo.GetPointNameONOFF: Boolean;
begin
  if not (Assigned(FLayer.GIS) and FLayer.GIS.UseReservedData) then
    Result := Boolean(FHeader.Reserved[Reserved_PointName])
  else
    Result := FLayer.GIS.Get_Reserved_PointName and Boolean(FHeader.Reserved[Reserved_PointName]);
end;

function TlicgLayerInfo.GetPointSize: Double;
begin
  Result := FHeader.PointSize;
end;

function TlicgLayerInfo.GetPointSymbolFileIndex: Byte;
begin
  Result := FHeader.PointSymbolFileIndex;
end;

function TlicgLayerInfo.GetPointSymbolIndex: Byte;
begin
  Result := FHeader.PointSymbolIndex;
end;

function TlicgLayerInfo.GetPolygonNameONOFF: Boolean;
begin
  if not (Assigned(FLayer.GIS) and FLayer.GIS.UseReservedData) then
    Result := Boolean(FHeader.Reserved[Reserved_PolygonName])
  else
    Result := FLayer.GIS.Get_Reserved_PolygonName and Boolean(FHeader.Reserved[Reserved_PolygonName]);
end;

function TlicgLayerInfo.GetPrintONOFF: Boolean;
begin
  if not (Assigned(FLayer.GIS) and FLayer.GIS.UseReservedData) then
    Result := Boolean(FHeader.Reserved[Reserved_Print])
  else
    Result := FLayer.GIS.Get_Reserved_Print and Boolean(FHeader.Reserved[Reserved_Print]);
end;

function TlicgLayerInfo.GetPenNo: Byte;
begin
  Result := FHeader.Reserved[Reserved_PenNo];
end;

function TlicgLayerInfo.GetShowArrow: Boolean;
begin
  if not (Assigned(FLayer.GIS) and FLayer.GIS.UseReservedData) then
    Result := Boolean(FHeader.Reserved[Reserved_ShowArrow])
  else
    Result := FLayer.GIS.Get_Reserved_ShowArrow and Boolean(FHeader.Reserved[Reserved_ShowArrow]);
end;

procedure TlicgLayerInfo.SetShowArrow(Value: Boolean);
var
  SA: Byte;
begin
  if Value then
    SA := 1
  else
    SA := 0;

  if FHeader.Reserved[Reserved_ShowArrow] <> SA then
  begin
    FHeader.Reserved[Reserved_ShowArrow] := SA;
    Modified := True;
  end;
end;

function TlicgLayerInfo.GetZDisplayOnOff: Boolean;
begin
  if not (Assigned(FLayer.GIS) and FLayer.GIS.UseReservedData) then
    Result := Boolean(FHeader.Reserved[Reserved_ZDisplay])
  else
    Result := FLayer.GIS.Get_Reserved_ZDisplay and Boolean(FHeader.Reserved[Reserved_ZDisplay]);
end;

function TlicgLayerInfo.GetZoomRangeActive: Boolean;
begin
  Result := FHeader.ZoomRangeActive;
end;

procedure TlicgLayerInfo.SetZDisplayOnOff(Value: Boolean);
var
  _t: byte;
begin
  if Value then
    _t := 1
  else
    _t := 0;
  if FHeader.Reserved[Reserved_ZDisplay] <> _T then
  begin
    FHeader.Reserved[Reserved_ZDisplay] := _t;
    Modified := True;
  end;
end;


procedure TlicgLayerInfo.SetZoomRangeActive(Value: Boolean);
begin
  FHeader.ZoomRangeActive := Value;
  Modified := True;
end;

//tabakaya yeni bir nesne eklendiğinde öznitelik bilgileri giriş formunun açılması  (FHeader.Reserved[11]=0 false 1 true)

function TlicgLayerInfo.GetShowDBInfoOnCreateEnt: Boolean;
begin
  result := FHeader.Reserved[Reserved_ShowDBInfoOnCreateEnt] = 1;
end;

procedure TlicgLayerInfo.SetShowDBInfoOnCreateEnt(Value: Boolean);
var
  _t: byte;
begin
  if Value then
    _t := 1
  else
    _t := 0;
  if FHeader.Reserved[Reserved_ShowDBInfoOnCreateEnt] <> _T then
  begin
    FHeader.Reserved[Reserved_ShowDBInfoOnCreateEnt] := _t;
    Modified := True;
  end;

end;

function TlicgLayerInfo.GetIsNetworkLayer: Boolean;
begin
  result := FHeader.Reserved[Reserved_NetworkLayer] = 1;
end;

procedure TlicgLayerInfo.SetIsNetworkLayer(Value: Boolean);
var
  _t: byte;
begin
  if Value then
    _t := 1
  else
    _t := 0;
  if FHeader.Reserved[Reserved_NetworkLayer] <> _T then
  begin
    FHeader.Reserved[Reserved_NetworkLayer] := _t;
    Modified := True;
  end;
end;

function TlicgLayerInfo.GetIsIndexed: Boolean;
begin
 { Buradaki orjinal satır şöyle idi :- Result := FHeader.IsIndexed; and Assigned(FLayer.rt);
   LayerIfo'yu layer den kurtarma pahasına bu koddan vazgeçtim - ama eksik çalışıyor olmalı. O nedenle, sonra düzeltilmeli. }
  Result := True; //Assigned(FLayer.rt);//FHeader.IsIndexed;
end;

procedure TlicgLayerInfo.SetIsIndexed(Value: Boolean);
begin
  if FHeader.IsIndexed <> Value then
  begin
    FHeader.IsIndexed := Value;
    Modified := True;
  end;
end;

{ TlicgLayerSymbolTool }

constructor TlicgLayerSymbolTool.Create(LayerInfo: TlicgBaseLayerInfo);
begin
  inherited Create;
  Modified := false;
  FLayerInfo := LayerInfo;
  Index := 1;
  Rotangle := 0;
  Height := 10;
end;

function TlicgLayerSymbolTool.GetHeight: Double;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle.Height;
end;

function TlicgLayerSymbolTool.GetIndex: Integer;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle.Index mod 256;
end;

function TlicgLayerSymbolTool.GetRotangle: Single;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle.Rotangle;
end;

function TlicgLayerSymbolTool.GetSymbolFileIndex: Integer;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle.Index div 256;
end;

function TlicgLayerSymbolTool.GetSymbolStyle: TlicgSymbolStyle;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle;
end;

procedure TlicgLayerSymbolTool.SetHeight(const Value: Double);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle.Height := Value;
  Modified := true;
end;

procedure TlicgLayerSymbolTool.SetIndex(Value: Integer);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle.Index := 256 * GetSymbolFileIndex + Value mod 256;
  Modified := true;
end;

procedure TlicgLayerSymbolTool.SetRotangle(const Value: Single);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle.Rotangle := Value;
  Modified := true;
end;

procedure TlicgLayerSymbolTool.SetSymbolFileIndex(Value: Integer);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle.Index := 256 * Value + TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle.Index mod 256;
  Modified := true;
end;

procedure TlicgLayerSymbolTool.SetSymbolStyle(Value: TlicgSymbolStyle);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefSymbolStyle := Value;
end;

{ TlicgLayerPenTool }

constructor TlicgLayerPenTool.Create(LayerInfo: TlicgBaseLayerInfo);
begin
  inherited Create;
  FLayerInfo := LayerInfo;
  Modified := false;
  Style := -1;
  Width := 0;
  Color := clBlue;
end;

function TlicgLayerPenTool.GetFlow: Boolean;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle.Flow;
end;

procedure TlicgLayerPenTool.SetFlow(const Value: Boolean);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle.Flow := Value;
end;

function TlicgLayerPenTool.GetColor: TColor;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle.Color;
end;

function TlicgLayerPenTool.GetStyle: Integer;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle.Style;
end;

function TlicgLayerPenTool.GetWidth: Double;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle.Width;
end;

procedure TlicgLayerPenTool.SetColor(Value: TColor);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle.Color := Value;
  Modified := true;
end;

procedure TlicgLayerPenTool.SetStyle(Value: Integer);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle.Style := Value;
  Modified := true;
end;

procedure TlicgLayerPenTool.SetWidth(const Value: Double);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle.Width := Value;
  Modified := true;
end;

function TlicgLayerPenTool.GetLineWidthDirection: TlicgLineWidthDirection;
begin
  if (ord(FLayerInfo.Reserved[Reserved_LineWidthDirection]) in [0..2]) then
    Result := TlicgLineWidthDirection(FLayerInfo.Reserved[Reserved_LineWidthDirection]);
end;

function TlicgLayerPenTool.GetPenStyle: TlicgPenStyle;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle;
end;

procedure TlicgLayerPenTool.SetLineWidthDirection(Value: TlicgLineWidthDirection);
begin
  if (ord(Value) in [0..2]) then
    FLayerInfo.Reserved[Reserved_LineWidthDirection] := ord(Value)
  else
    FLayerInfo.Reserved[Reserved_LineWidthDirection] := ord(lwdCenter);

  //if Assigned (FLayerInfo.FDefPenTool) then
    //FLayerInfo.FDefPenTool.LineWidthDirection := Value;
end;

procedure TlicgLayerPenTool.SetPenStyle(Value: TlicgPenStyle);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefPenStyle := Value;

end;

{ TlicgLayerBrushTool }
constructor TlicgLayerBrushTool.Create(LayerInfo: TlicgBaseLayerInfo);
begin
  inherited Create;
  FLayerInfo := LayerInfo;
  Modified := False;
  Pattern := 1;  // ilker değiştirme default dolu taramalı gelmesi için
  ForeColor := clBlue;
  BackColor := clNone; // ilker ekleme değiştirme
  FillFactor := 1.0;
  FillDirection := fdBottomTop;
end;

function TlicgLayerBrushTool.GetBackColor: TColor;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.BackColor;
end;

function TlicgLayerBrushTool.GetForeColor: TColor;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.ForeColor;
end;

function TlicgLayerBrushTool.GetPattern: Integer;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.Pattern;
end;

procedure TlicgLayerBrushTool.SetBackColor(const Value: TColor);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.BackColor := Value;
  Modified := true;
end;

procedure TlicgLayerBrushTool.SetForeColor(const Value: TColor);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.ForeColor := Value;
  Modified := true;
end;

procedure TlicgLayerBrushTool.SetPattern(Value: Integer);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.Pattern := Value;
  Modified := true;
end;

function TlicgLayerBrushTool.GetFillFactor: Single;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.FillFactor;
end;

procedure TlicgLayerBrushTool.SetFillFactor(const Value: Single);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.FillFactor := Value;
  Modified := true;
end;

function TlicgLayerBrushTool.GetFillDirection: TlicgFillDirection;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.FillDirection;
end;

procedure TlicgLayerBrushTool.SetFillDirection(const Value: TlicgFillDirection);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.FillDirection := Value;
  Modified := true;
end;

function TlicgLayerBrushTool.GetGradientFill: Boolean;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.GradientFill;
end;

procedure TlicgLayerBrushTool.SetGradientFill(const Value: Boolean);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.GradientFill := Value;
  Modified := true;
end;

function TlicgLayerBrushTool.GetGradientDirection: TlicgFillDirection;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.GradientDirection;
end;

procedure TlicgLayerBrushTool.SetGradientDirection(const Value: TlicgFillDirection);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle.GradientDirection := Value;
end;

function TlicgLayerBrushTool.GetBrushStyle: TlicgBrushStyle;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle;
end;

procedure TlicgLayerBrushTool.SetBrushStyle(Value: TlicgBrushStyle);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefBrushStyle := Value;
end;

{ TlicgLayerFontTool }

constructor TlicgLayerFontTool.Create(LayerInfo: TlicgBaseLayerInfo);
begin
  inherited Create;
  FLayerInfo := LayerInfo;
  Modified := false;
  Name := 'Arial';
  Angle := 0;
  Height := 2;
  CharWidthFactor := 1;
  CharSpacing := 0;
  Color := clRed;
  TextPos := tpLowerLeft;
end;

function TlicgLayerFontTool.GetAngle: single;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Angle;
end;

function TlicgLayerFontTool.GetCharSet: TFontCharSet;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.CharSet;
end;

function TlicgLayerFontTool.GetColor: TColor;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Color;
end;

function TlicgLayerFontTool.GetHeight: Double;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Height;
end;

function TlicgLayerFontTool.GetName: string;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Name;
end;

function TlicgLayerFontTool.GetStyle: TFontStyles;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Style;
end;

procedure TlicgLayerFontTool.SetAngle(const Value: single);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Angle := Value;
  Modified := true;
end;

procedure TlicgLayerFontTool.SetCharSet(const Value: TFontCharSet);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.CharSet := Value;
  Modified := true;
end;

procedure TlicgLayerFontTool.SetColor(Value: TColor);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Color := Value;
  Modified := true;
end;

procedure TlicgLayerFontTool.SetHeight(const Value: Double);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Height := Value;
  Modified := true;
end;

procedure TlicgLayerFontTool.SetName(const Value: string);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Name := Value;
  Modified := true;
end;

procedure TlicgLayerFontTool.SetStyle(const Value: TFontStyles);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.Style := Value;
  Modified := true;
end;

function TlicgLayerFontTool.GetFontStyle: TlicgFontStyle;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle;
end;

procedure TlicgLayerFontTool.SetFontStyle(Value: TlicgFontStyle);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle := Value;
end;

(*uğur ekleme başlama*)

function TlicgLayerFontTool.GetCharWidthFactor: Single;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.CharWidthFactor;
end;

procedure TlicgLayerFontTool.SetCharWidthFactor(AValue: Single);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.CharWidthFactor := AValue;
  Modified := true;
end;

function TlicgLayerFontTool.GetCharSpacing: Single;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.CharSpacing;
end;

procedure TlicgLayerFontTool.SetCharSpacing(AValue: Single);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.CharSpacing := AValue;
  Modified := true;
end;

function TlicgLayerFontTool.GetTextPos: TlicgTextPos;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.TextPos;
end;

procedure TlicgLayerFontTool.SetTextPos(AValue: TlicgTextPos);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle.TextPos := AValue;
  Modified := true;
end;
(*uğur ekleme bitme*)

{ TlicgLayerFontToolTT }

constructor TlicgLayerFontToolTT.Create(LayerInfo: TlicgBaseLayerInfo);
begin
  inherited Create;
  FLayerInfo := LayerInfo;
  Modified := false;

  Name := 'Arial';
  Angle := 0;
  Height := 2;
  CharWidthFactor := 1;
  CharSpacing := 0;
  Color := clRed;
  TextPos := tpLowerLeft;
end;

function TlicgLayerFontToolTT.GetAngle: single;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Angle;
end;

function TlicgLayerFontToolTT.GetCharSet: TFontCharSet;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.CharSet;
end;

function TlicgLayerFontToolTT.GetColor: TColor;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Color;
end;

function TlicgLayerFontToolTT.GetHeight: Double;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Height;
end;

function TlicgLayerFontToolTT.GetName: string;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Name;
end;

function TlicgLayerFontToolTT.GetStyle: TFontStyles;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Style;
end;

procedure TlicgLayerFontToolTT.SetAngle(const Value: single);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Angle := Value;
  Modified := true;
end;

procedure TlicgLayerFontToolTT.SetCharSet(const Value: TFontCharSet);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.CharSet := Value;
  Modified := true;
end;

procedure TlicgLayerFontToolTT.SetColor(Value: TColor);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Color := Value;
  Modified := true;
end;

procedure TlicgLayerFontToolTT.SetHeight(const Value: Double);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Height := Value;
  Modified := true;
end;

procedure TlicgLayerFontToolTT.SetName(const Value: string);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Name := Value;
  Modified := true;
end;

(*uğur ekleme başlama*)
function TlicgLayerFontToolTT.GetCharWidthFactor: Single;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.CharWidthFactor;
end;

procedure TlicgLayerFontToolTT.SetCharWidthFactor(AValue: Single);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.CharWidthFactor := AValue;
  Modified := true;
end;

function TlicgLayerFontToolTT.GetCharSpacing: Single;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.CharSpacing;
end;

procedure TlicgLayerFontToolTT.SetCharSpacing(AValue: Single);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.CharSpacing := AValue;
  Modified := true;
end;

function TlicgLayerFontToolTT.GetTextPos: TlicgTextPos;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.TextPos;
end;

procedure TlicgLayerFontToolTT.SetTextPos(AValue: TlicgTextPos);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.TextPos := AValue;
  Modified := true;
end;
(*uğur ekleme bitme*)

procedure TlicgLayerFontToolTT.SetStyle(const Value: TFontStyles);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyleTT.Style := Value;
  Modified := true;
end;

function TlicgLayerInfo.GetLayerHeader: TlicgLayerHeader;
begin
  result := FHeader;
end;

procedure TlicgLayerInfo.SetLayerHeader(Value: TlicgLayerHeader);
begin
  FHeader := Value;
end;

function TlicgLayerInfo.GetRecordCount: Integer;
begin
  Result := FHeader.RecordCount;
end;

function TlicgLayerInfo.GetVersionNumber: smallInt;
begin
  result := FHeader.VersionNumber;
end;

procedure TlicgLayerInfo.SetRecordCount(Value: Integer);
begin
  FHeader.RecordCount := Value;
end;

procedure TlicgLayerInfo.SetVersionNumber(Value: smallInt);
begin
  FHeader.VersionNumber := Value;

end;

function TlicgLayerInfo.GetHeaderID: smallInt;
begin
  result := FHeader.HeaderID;
end;

procedure TlicgLayerInfo.SetHeaderID(Value: smallInt);
begin
  FHeader.HeaderID := Value;
end;

function TlicgLayerInfo.GetIsMemoryLayer: Boolean;
begin
  Result := FHeader.IsMemoryLayer;
end;

procedure TlicgLayerInfo.SetIsMemoryLayer(Value: Boolean);
begin
  FHeader.IsMemoryLayer := Value;
end;

function TlicgLayerInfo.GetSymbolFixed: Boolean;
begin
  Result := FHeader.SymbolFixed;
end;

function TlicgLayerInfo.GetSymbolFixedSize: Integer;
begin
  Result := FHeader.SymbolFixedSize;
end;

procedure TlicgLayerInfo.SetSymbolFixed(const Value: Boolean);
begin
  FHeader.SymbolFixed := Value;
end;

procedure TlicgLayerInfo.SetSymbolFixedSize(const Value: Integer);
begin
  if FHeader.SymbolFixedSize <> Value then
  begin
    FHeader.SymbolFixedSize := Value;
    Modified := True;
  end;
end;

function TlicgLayerInfo.GetReserved(index: Integer): Byte;
begin
  Result := FHeader.Reserved[index];
end;

procedure TlicgLayerInfo.SetReserved(index: Integer; Value: Byte);
begin
  FHeader.Reserved[index] := Value;
end;

function TlicgLayerFontToolTT.GetFontStyle: TlicgFontStyle;
begin
  Result := TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle;
end;

procedure TlicgLayerFontToolTT.SetFontStyle(Value: TlicgFontStyle);
begin
  TlicgLayerInfo(FLayerInfo).FHeader.DefFontStyle := Value;
end;

{ TLayerConnectorAndSqlInfo }

constructor TLayerConnectorAndSqlInfo.Create;
begin
  inherited;
  FillChar(F_ConnParams, sizeof(F_ConnParams), #0);
  F_ConnParams.SpatialType := 1;
  F_ConnParams.Port := 5432;
  FillChar(F_TableSQLRecord, sizeof(F_TableSQLRecord), #0);
end;

function TLayerConnectorAndSqlInfo.GetDisplayName: string;
begin
  Result := F_TableSQLRecord.DisplayName;
end;

function TLayerConnectorAndSqlInfo.GetGeometryField: string;
begin
  Result := F_TableSQLRecord.GeometryField;
end;

function TLayerConnectorAndSqlInfo.GetGroupCap: string;
begin
  Result := F_TableSQLRecord.GroupCap;
end;

function TLayerConnectorAndSqlInfo.GetDBFFields: string;
begin
  Result := F_TableSQLRecord.DBFFields;

end;

function TLayerConnectorAndSqlInfo.GetLayerFullPathName: string;
begin
  Result := F_TableSQLRecord.LayerFullPathName;
end;

function TLayerConnectorAndSqlInfo.GetPrimaryIdField: string;
begin
  Result := F_TableSQLRecord.PrimaryIdField;
end;

function TLayerConnectorAndSqlInfo.GetSQLGetFeatureFileName: string;
begin
  Result := F_TableSQLRecord.SQLGetFeatureFileName;
end;

function TLayerConnectorAndSqlInfo.GetSQLOpenFileName: string;
begin
  Result := F_TableSQLRecord.SQLOpenFileName;
end;

function TLayerConnectorAndSqlInfo.GetTableName: string;
begin
  Result := F_TableSQLRecord.TableName;
end;

function TLayerConnectorAndSqlInfo.Get_TableSQLRecord: TTableSQLRecord;
begin
  Result := F_TableSQLRecord;
end;

procedure TLayerConnectorAndSqlInfo.SetDisplayName(Value: string);
begin
  F_TableSQLRecord.DisplayName := Value;
end;

procedure TLayerConnectorAndSqlInfo.SetGeometryField(Value: string);
begin
  F_TableSQLRecord.GeometryField := Value;
end;

procedure TLayerConnectorAndSqlInfo.SetGroupCap(Value: string);
begin
  F_TableSQLRecord.GroupCap := Value;
end;

procedure TLayerConnectorAndSqlInfo.SetDBFFields(Value: string);
begin
  F_TableSQLRecord.DBFFields := Value;
end;

procedure TLayerConnectorAndSqlInfo.SetLayerFullPathName(Value: string);
begin
  F_TableSQLRecord.LayerFullPathName := Value;
end;

procedure TLayerConnectorAndSqlInfo.SetPrimaryIdField(Value: string);
begin
  F_TableSQLRecord.PrimaryIdField := Value;
end;

procedure TLayerConnectorAndSqlInfo.SetSQLGetFeatureFileName(Value: string);
begin
  F_TableSQLRecord.SQLGetFeatureFileName := Value;
end;

procedure TLayerConnectorAndSqlInfo.SetSQLOpenFileName(Value: string);
begin
  F_TableSQLRecord.SQLOpenFileName := Value;
end;

procedure TLayerConnectorAndSqlInfo.SetTableName(Value: string);
begin
  inherited;
  F_TableSQLRecord.TableName := Value;
end;

procedure TLayerConnectorAndSqlInfo.Set_TableSQLRecord(Value: TTableSQLRecord);
begin
  F_TableSQLRecord := Value;
end;

function TLayerConnectorAndSqlInfo.GetInfoIdField: string;
begin
  Result := F_TableSQLRecord.InfoFiedId;
end;

procedure TLayerConnectorAndSqlInfo.SetInfoIdField(Value: string);
begin
  F_TableSQLRecord.InfoFiedId := Value;
end;

function TlicgBaseLayer.SupportEntitiyIDs: TlicgEntityIDs;
begin
  Result := AllEntityIDs;
end;

function TlicgBaseGIS.GetOnCurrentLayerChange: TlicgLayerEvent;
begin
  Result := FOnCurrentLayerChange;
end;

procedure TlicgBaseGIS.SetOnCurrentLayerChange(Value: TlicgLayerEvent);
begin
  FOnCurrentLayerChange := Value;
end;

function TlicgBaseLayer.GetInfoId(RecNo: Integer): Integer;
begin
  Result := -1;
end;

function TlicgBaseLayer.CanUndo(_UndoAction: TlicgUndoAction): Boolean;
begin
  Result := True;
end;

function TlicgBaseLayer.ReferansLayer: TlicgBaseLayer;
begin
  Result := nil;
end;

function TLayerConnectorAndSQLInfo.Get_ConnParams: TConnectionParams;
begin
  Result := F_ConnParams;
end;

function TLayerConnectorAndSQLInfo.GetDatabase: string;
begin
  Result := F_ConnParams.Database;
end;

function TLayerConnectorAndSQLInfo.GetEncoding: string;
begin
  Result := F_ConnParams.Encoding;
end;

function TLayerConnectorAndSQLInfo.GetHostName: string;
begin
  Result := F_ConnParams.HostName;
end;

function TLayerConnectorAndSQLInfo.GetPassword: string;
begin
  Result := F_ConnParams.Password;
end;

function TLayerConnectorAndSQLInfo.GetPort: Integer;
begin
  Result := F_ConnParams.Port;
end;

function TLayerConnectorAndSQLInfo.GetSpatialType: Integer;
begin
  Result := F_ConnParams.SpatialType;
end;

function TLayerConnectorAndSQLInfo.GetUser: string;
begin
  Result := F_ConnParams.User;
end;

procedure TLayerConnectorAndSQLInfo.Set_ConnParams(Value: TConnectionParams);
begin
  F_ConnParams := Value;
end;

procedure TLayerConnectorAndSQLInfo.SetDatabase(Value: string);
begin
  F_ConnParams.Database := Value;
end;

procedure TLayerConnectorAndSQLInfo.SetEncoding(Value: string);
begin
  F_ConnParams.Encoding := Value;
end;

procedure TLayerConnectorAndSQLInfo.SetHostName(Value: string);
begin
  F_ConnParams.HostName := Value;
end;

procedure TLayerConnectorAndSQLInfo.SetPassword(Value: string);
begin
  F_ConnParams.Password := Value;
end;

procedure TLayerConnectorAndSQLInfo.SetPort(Value: Integer);
begin
  F_ConnParams.Port := Value;
end;

procedure TLayerConnectorAndSQLInfo.SetSpatialType(Value: Integer);
begin
  F_ConnParams.SpatialType := Value;
end;

procedure TLayerConnectorAndSQLInfo.SetUser(Value: string);
begin
  F_ConnParams.User := Value;
end;

procedure TLayerConnectorAndSqlInfo.Assigned(LC: ILayerConnectorAndSqlInfo);
begin
  self.F_TableSQLRecord := ILayerConnectorAndSqlInfo(LC).TableSQLRecord;
  self.F_ConnParams := ILayerConnectorAndSqlInfo(LC).ConnParams;
end;

function TlicgBaseLayerInfo.GetNodeVisible: Boolean;
begin
  if not (Assigned(FLayer.GIS) and FLayer.GIS.UseReservedData) then
    Result := Boolean(FHeader.Reserved[Reserved_NodeVisible])
  else
    Result := FLayer.GIS.Get_Reserved_NodeVisible and Boolean(FHeader.Reserved[Reserved_NodeVisible]);
end;

procedure TlicgBaseLayerInfo.SetNodeVisible(Value: Boolean);
begin
  Reserved[Reserved_NodeVisible] := Integer(Value);
  Modified := true;
end;

{ TPropertyValueList }

function TPropertyValueList.Add(_Prop: string; _val: variant; _type:
  TPropertyValueType = pvtUnknown): Integer;
var
  _P: TPropertyValue;
begin
  _P := TPropertyValue.Create;
  _P._Property := _Prop;
  _P._Value := _val;
  _P._Type := _type;
  Result := FList.Add(_P);
end;

function TPropertyValueList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TPropertyValueList.create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TPropertyValueList.destroy;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    TPropertyValue(FList.Items[i]).Free;
  FList.Free;
  inherited;
end;

function TPropertyValueList.GetPropValue(_Prop: string; var _val: variant):
  Boolean; stdcall;
var
  i: Integer;
begin
  Result := false;
  _val := NULL;
  for i := 0 to FList.Count - 1 do
  begin
    if TPropertyValue(FList.Items[i])._Property = _Prop then
    begin
      Result := True;
      _val := TPropertyValue(FList.Items[i])._Value;
      BREAK;
    end;
  end;
end;

function TPropertyValueList.propName(i: Integer): string;
begin
  Result := TPropertyValue(FList.Items[i])._Property;
end;

function TPropertyValueList.propType(i: Integer): TPropertyValueType;
begin
  Result := TPropertyValue(FList.Items[i])._Type;
end;

function TPropertyValueList.propValue(i: Integer): variant;
begin
  Result := TPropertyValue(FList.Items[i])._Value;
end;

function TlicgBaseLayerInfo.GetisApplyBufferForPolyline: Boolean;
begin
  Result := Boolean(Reserved[Reserved_ApplyBufferForPolyline]);
end;

procedure TlicgBaseLayerInfo.SetisApplyBufferForPolyline(Value: Boolean);
begin
  Reserved[Reserved_ApplyBufferForPolyline] := Integer(Value);
  Modified := true;
end;

function TlicgBaseGIS.GetThicknessWidth: Integer;
begin
  Result := FThicknessWidth;
end;

function TlicgBaseGIS.GetGuideIsolineInterval: Double;
begin
  Result := FGuideIsolineInterval;
end;

procedure TlicgBaseGIS.SetThicknessWidth(const Value: Integer);
begin
  FThicknessWidth := Value;
end;

procedure TlicgBaseGIS.SetGuideIsolineInterval(const Value: Double);
begin
  FGuideIsolineInterval := Value;
end;

constructor TlicgBaseTable.CreateNoOpen(Layer: TlicgBaseLayer);
begin
  CreateNoOpen(Layer.GIS);
end;


(*
function TlicgBaseGIS.gcdFilename: string;
begin
  result := ExtractFilePath(ExtractFileDir(self.FileName)) + ChangeFileExt(ExtractFileName(self.FileName), '') + NEWEXT;
end;
*)

function TlicgBaseGIS.CreateSablonLayers(const Filenames: TStrings): Boolean;
var
  i, index, dindex: Integer;
  path, fn: string;
  found: Boolean;
  Kaynak, Hedef: string;
begin
  result := false;
  Path := ExtractFilePath(ParamStr(0)) + 'Data\';

  try
    result := FileExists(path + 'SablonLayer.LDW') and FileExists(path +
      'SablonLayer.LIX') and FileExists(path + 'SablonLayer.DBF') and FileExists
      (path + 'SablonLayer.LRX');

    if Result then
    begin
      for i := 0 to Filenames.Count - 1 do
      begin
        if not TFile.Exists(Filenames[i] + '.LDW') then
        begin
          TFile.Copy(path + 'SablonLayer.LDW', ChangeFileExt(Filenames[i], '.LDW'), False);
          TFile.Copy(path + 'SablonLayer.LIX', ChangeFileExt(Filenames[i], '.LIX'), false);
          TFile.Copy(path + 'SablonLayer.LRX', ChangeFileExt(Filenames[i], '.LRX'), false);
          TFile.Copy(path + 'SablonLayer.DBF', ChangeFileExt(Filenames[i], '.DBF'), false);
        end;

        if TFile.Exists(Filenames[i] + '.LDW') then
        begin
          Index := AddToLayers(Filenames[i], ltDesktop, clBlue,False);
          Found := Index >= 0;
          if Found then
          begin
            Layers[Index].LayerInfo.UseAttachedDB := TRUE;
            Layers[Index].Open;
            if Layers[Index].DBTable = nil then
            begin
              Layers[Index].LayerInfo.UseAttachedDB := TRUE;
              fn := Layers[Index].FileName;

              if Layers[Index].LayerInfo.UseAttachedDB then
              begin
                dindex := index;
                with self.BaseTableClass.CreateNoOpen(self) do
                try
                  if not DBTableExists(fn) then
                  begin
                    self.Layers[dIndex].LayerInfo.UseAttachedDB := False;
                    Modified := True;
                  end;
                finally
                  Free;
                end;
                if Layers[Index].LayerInfo.UseAttachedDB then
                begin
                  Layers[Index].DBTable := self.BaseTableClass.Create(self, fn,
                    not self.ReadOnly, True);
                  Layers[Index].DBTable.SetUseDeleted(True);
                end;
              end;

            end;

            //ilker silme if Layers[Index].DBTable = nil then
              //ShowMessage(Layers[Index].DisplayName);
          end;
        end;
      end;

    end;
  except
  end;
end;

function TlicgLayerInfo.GetPointAngle: Double;
begin
  Result := FHeader.PointAngle;
end;

function TlicgLayerInfo.GetPointKodONOFF: Boolean;
begin
  if not (Assigned(FLayer.GIS) and FLayer.GIS.UseReservedData) then
    Result := Boolean(FHeader.Reserved[Reserved_PointKod])
  else
    Result := FLayer.GIS.Get_Reserved_PointKod and Boolean(FHeader.Reserved[Reserved_PointKod]);
end;

procedure TlicgLayerInfo.SetPointAngle(const Value: Double);
begin
  FHeader.PointAngle := Value;
  Modified := True;
end;

procedure TlicgLayerInfo.SetPointKodONOFF(Value: Boolean);
var
  ValueONOFF: byte;
begin
  if (PointKodOnOFF <> Value) then
  begin
    if Value then
      ValueONOFF := 1
    else
      ValueONOFF := 0;
    if FHeader.Reserved[Reserved_PointKod] <> ValueONOFF then
    begin
      FHeader.Reserved[Reserved_PointKod] := ValueONOFF;
      Modified := True;
    end;
  end;
end;

{ TPickedList }

function TPickedList.AddPickedItem: IPickedClass;
begin
  Result := TPickedClass.Create;
  inherited Add(Result);
end;

function TPickedList.GetItem(Index: Integer): IPickedClass;
begin
  Result := IPickedClass(Self.Items[index]);
end;

procedure TPickedList.SetItem(Index: Integer; Value: IPickedClass);
begin
  Self.Items[index] := nil;
  Self.Items[index] := Value;

end;

(*
{ TRasterGroupList }

function TRasterGroupList.Get(gc: string): TRasterGroup;
var
  i : Integer;
begin
  Result := nil;

  for i:=0 to Count-1 do
  begin
    if TRasterGroup(Items[i]).GroupName = gc then
    begin
       Result := TRasterGroup(Items[i]);
       BREAK;
    end;
  end;

end;

function TRasterGroupList.GetIndex(gc: string): Integer;
var
  i : Integer;
begin
  Result := -1;

  for i:=0 to Count-1 do
  begin
    if TRasterGroup(Items[i]).GroupName = gc then
    begin
       Result := i;
       BREAK;
    end;
  end;

end;

{ TRasterGroup }

constructor TRasterGroup.Create;
begin
  inherited;
  RasterList := TList.Create;
end;

destructor TRasterGroup.destroy;
begin
   RasterList.Free;
  inherited;
end;

*)

function TlicgBaseGIS.CSCoorToCSCoor(aXIn, aYIn: Double; var aXOut, aYOut: Double;
  aProjIn, aProjOut: IlicgCS; ProjectApplyMatrix: PAffineMatrixElements;
  isAfter: Boolean): Boolean;
var
  _aXIn, _aYIn: Double;
  M: TlicgMatrix;
  P: TlicgCoor;
begin
  Result := false;
  aXOut := aXIn;
  aYOut := aYIn;
  try
    if (aYIn <> 0) and (aXIn <> 0) then
    begin
      _aXIn := aXIn;
      _aYIn := aYIn;
      if not isAfter then
      begin
        if (ProjectApplyMatrix <> nil) then
        begin
          M := Translate2D(-ProjectApplyMatrix^.HorizantalTraslate, -
            ProjectApplyMatrix^.VerticalTranslante);
          if not EqualMatrix2D(M, IDENTITY_MATRIX2D) then
          begin
            P := AffineTransPnt(M, AsCoor(_aXIn, _aYIn));
            _aXIn := P.X;
            _aYIn := P.Y;
            aXOut := _aXIn;
            aYOut := _aYIn;
          end;
        end;
      end;
      if Assigned(MapInfo.CS) then
      begin
        if not (aProjIn.IsUnknownCS or aProjOut.IsUnknownCS) then
          Result := MapInfo.CS.CSCoorToCSCoor(_aXIn, _aYIn, aXOut, aYOut, aProjIn, aProjOut);
      end;

      if isAfter then
      begin
        if (ProjectApplyMatrix <> nil) and Result then
        begin
          M := Translate2D(ProjectApplyMatrix^.HorizantalTraslate,
            ProjectApplyMatrix^.VerticalTranslante);
          if not EqualMatrix2D(M, IDENTITY_MATRIX2D) then
          begin
            P := AffineTransPnt(M, AsCoor(aXOut, aYOut));
            aXOut := P.X;
            aYOut := P.Y;
          end;
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function TlicgBaseGIS.Get_Reserved_ApplyBufferForPolyline: Boolean;
begin
  Result := Boolean(FReserved[Reserved_ApplyBufferForPolyline]);
end;

function TlicgBaseGIS.Get_Reserved_Balastro: Boolean;
begin
  Result := Boolean(FReserved[Reserved_Balastro]);
end;

function TlicgBaseGIS.Get_Reserved_Labeling: Boolean;
begin
  Result := Boolean(FReserved[Reserved_Labeling]);
end;

function TlicgBaseGIS.Get_Reserved_Thickness: Boolean;
begin
  Result := Boolean(FReserved[Reserved_Thickness]);
end;

function TlicgBaseGIS.Get_Reserved_GuideIsoline: Boolean;
begin
  Result := Boolean(FReserved[Reserved_GuideIsoline]);
end;

function TlicgBaseGIS.Get_Reserved_LayerBrush: Boolean;
begin
  Result := Boolean(FReserved[Reserved_LayerBrush]);
end;

function TlicgBaseGIS.Get_Reserved_LineWidthDirection: Boolean;
begin
  Result := Boolean(FReserved[Reserved_LineWidthDirection]);
end;

function TlicgBaseGIS.Get_Reserved_NodeVisible: Boolean;
begin
  Result := Boolean(FReserved[Reserved_NodeVisible]);
end;

function TlicgBaseGIS.Get_Reserved_PointKod: Boolean;
begin
  Result := Boolean(FReserved[Reserved_PointKod]);
end;

function TlicgBaseGIS.Get_Reserved_PointName: Boolean;
begin
  Result := Boolean(FReserved[Reserved_PointName]);
end;

function TlicgBaseGIS.Get_Reserved_PolygonName: Boolean;
begin
  Result := Boolean(FReserved[Reserved_PolygonName]);
end;

function TlicgBaseGIS.Get_Reserved_Print: Boolean;
begin
  Result := Boolean(FReserved[Reserved_Print]);
end;

function TlicgBaseGIS.Get_Reserved_ShowArrow: Boolean;
begin
  Result := Boolean(FReserved[Reserved_ShowArrow]);
end;

function TlicgBaseGIS.Get_Reserved_ZDisplay: Boolean;
begin
  Result := Boolean(FReserved[Reserved_ZDisplay]);
end;

procedure TlicgBaseGIS.Set_Reserved_ApplyBufferForPolyline(Value: Boolean);
begin
  FReserved[Reserved_ApplyBufferForPolyline] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_Balastro(Value: Boolean);
begin
  FReserved[Reserved_Balastro] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_Labeling(Value: Boolean);
begin
  FReserved[Reserved_Labeling] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_Thickness(Value: Boolean);
begin
  FReserved[Reserved_Thickness] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_GuideIsoline(Value: Boolean);
begin
  FReserved[Reserved_GuideIsoline] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_LayerBrush(Value: Boolean);
begin
  FReserved[Reserved_LayerBrush] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_LineWidthDirection(Value: Boolean);
begin
  FReserved[Reserved_LineWidthDirection] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_NodeVisible(Value: Boolean);
begin
  FReserved[Reserved_NodeVisible] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_PointKod(Value: Boolean);
begin
  FReserved[Reserved_PointKod] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_PointName(Value: Boolean);
begin
  FReserved[Reserved_PointName] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_PolygonName(Value: Boolean);
begin
  FReserved[Reserved_PolygonName] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_Print(Value: Boolean);
begin
  FReserved[Reserved_Print] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_ShowArrow(Value: Boolean);
begin
  FReserved[Reserved_ShowArrow] := Integer(Value);
  Self.Modified := True;
end;

procedure TlicgBaseGIS.Set_Reserved_ZDisplay(Value: Boolean);
begin
  FReserved[Reserved_ZDisplay] := Integer(Value);
  Self.Modified := True;
end;

initialization
  CF_LICADGISDATA := RegisterClipboardFormat(LICADGISDATA);

end.