unit Lider.CG.Com.Base;

{$I Lider.CG.Com.Component.inc}

interface

uses
  SysUtils,
  Windows,
  Classes,
  Graphics,
  Controls,
  Db,
  OleCtnrs,
  Forms,
  ExtCtrls,
  PngImage,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.CSInt,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Rtree;

const
  TwoPi = 2 * System.Pi;
  RTYPE = ttRTree;
  PenThicknessArray: array[1..16] of Double =  // nc
   (0.07, 0.14, 0.21, 0.28, 0.35, 0.42, 0.49, 0.56, 0.63, 0.70, 0.77, 0.84, 0.91, 0.98, 1.05, 1.12);

type
  TlicgStoringKind = (skIni, skReg);
  TlicgFrameStyle = (fsRectangular, fsPolar);

  TlicgAlignVerticalText = (alvtTop, alvtCenter, alvtBottom);

  TlicgAlignHorizantalText = (alhtLeft, alhtCenter, alhtRight);

  TParselDegOrders = (poInsert, poDelete, poUndelete);

  TlicgPrintAreaSelectModes = (smCurrentPage, smFreeStyle);

  TlicgPrint = record
    MarginTop: Double;
    MarginBottom: Double;
    MarginLeft: Double;
    MarginRight: Double;

    Scale: Double;
    FitToPage: Boolean;
    PageAngle: Double; // internal use uður
    PrintColorMode: Byte;
    DrawBorder: Boolean;
    DefaultPenThickness: Double;
    UsePenThicknesses: Boolean;
    PenThickness: array[1..16] of Double;

    KoordinatEdit: Boolean;
    EditY: Double;
    EditX: Double;

    FilterSymbolActive: Boolean;
    FilterPointNameActive: Boolean;
    FilterElevationActive: Boolean;

    FilterSymbol: AnsiString;
    FilterPointName: AnsiString;
    FilterElevation: AnsiString;

    FilterSymbolFontHeight: Double;
    FilterPointNameFontHeight: Double;
    FilterElevationFontHeight: Double;
  end;

  TlicgMouseDrawElements = set of (mdCursor, mdCursorFrame, mdFullViewCursor);

  TlicgSelectionMode = (smPicking, smWindow, smReshaping);

  { Group mode specifies how the object are picked in a box. }
  TlicgGroupMode = (gmAllInside, gmCrossFrame);

  TlicgShadowStyle = (fssLowerRight, fssUpperRight, fssLowerLeft, fssUpperLeft);

  TlicgTransformType = (ttNone, ttTranslate, ttRotate, ttScale, ttScaleHorizontal,  ttScaleVertical, ttMirror,
    ttOffset, ttBufferOffset, ttTwoPointsTransformation);

  TlicgTransformTypes = set of TlicgTransformType;

  TEPSG = integer;

  TlicgCoorSystem = TEPSG; //Gdal_ProjSystem;  // (csNull, csCartesian, csLatLon, csProjection);

  TlicgClipAreaKind = (cpkRectangular, cpkPolygonal);

  TlicgDirection = (diForward, diBackward);

  TlicgOrigin = (orEntire, orCurrRow);

  TDegMinSec = packed record
    Degrees: integer;
    Minutes: byte;
    Seconds: Double;
  end;

  TlicgOutputDevice = (odCanvas, odBitmap);

  TlicgOutputDevices = set of TlicgOutputDevice;

  { Object Snap Settings }
  TlicgOSNAPSetting = (osPoint, osEndPoint, osMidPoint, osCenter, osIntersect, osPerpend,
    osTangent, osNearest, osOrigin, osParallel, osKeyPoint, osBisector);

  TlicgOSNAPSettings = set of TlicgOSNAPSetting;
  { how to handle the overlapped text}
  TlicgOverlappedTextAction = (otaDoNothing, otaHideOverlapped, otaShowOverlappedOnColor);

  //penStyle deðiþikliði nedeniyle layer versiyon bilgisi okunacak
  //versiyona göre TlicgLayerHeader veya TlicgLayerHeader200 desenine göre okuma yapýlacak.
  TlicgLayerHeaderVer = record
    HeaderID: SmallInt;
    VersionNumber: SmallInt
  end;

  { Layer main file header (in desktop is used in .ED2 file) }
  TlicgLayerHeader = packed record
    HeaderID: SmallInt;
    VersionNumber: SmallInt; // 100, 110, etc.
    RecordCount: Integer;  // number of records in file
    Extent: TlicgExtent;  // max, min extensions of layer
    IDCounter: Integer;  // four bytes for next entity identifier
    Visible: Boolean;
    Selectable: Boolean;
    IsMemoryLayer: Boolean;  // the layer is a memory/cosmethic layer ?
    IsAnimationLayer: Boolean;
    IsIndexed: Boolean;
    CS: widestring;
    UseAttachedDB: Boolean;
    SymbolFixed: Boolean; 
    TextFixed: Boolean;
    TextFixedSize: Integer;  // if > 0 the font is drawn with same size
    Locked: Boolean;  // layer is locked: no copy,paste,insert,delete,etc.
    SymbolFixedSize: Integer;
    NodeLinkCount: Integer;
    DefSymbolStyle: TlicgSymbolStyle;
    DefPenStyle: TlicgPenStyle;
    DefBrushStyle: TlicgBrushStyle;
    DefFontStyleTT: TlicgFontStyle;
    DefFontStyle: TlicgFontStyle;
    WithConnectors: Boolean;  // = true when layer have connectivity entities

    (* ilker layeroptions dan aktarma baþlama *)
    Color: TColor;
    { zoom range for layer }
    ZoomRangeActive: Boolean;
    MinZoomScale: Double;
    MaxZoomScale: Double;
    { zoom range for text }
    TextZoomRangeActive: Boolean;
    MinZoomScaleForText: Double;
    MaxZoomScaleForText: Double;
    BufferWidthForPolyline: Double;

    Thickness: Boolean;
    ThicknessWidth: Integer;

    GuideIsoline: Boolean;
    GuideIsolineInterval: Double;
    DrawDirectionArrow: Boolean;

    PointSize: Double;
    PointAngle: Double;
    PointSymbolIndex: Byte;
    PointSymbolFileIndex: Byte;
    ApplyPointLayerValues: Boolean;
    (* ilker layeroptions dan aktarma bitiþ *)

    Tolerance: Double;
    ApplyDefSymbol: Boolean;
    ApplyDefPen: Boolean;
    ApplyDefBrush: Boolean;
    ApplyDefFontTT: Boolean;
    ApplyDefFont: Boolean;
    Reserved: array[0..99] of Byte; // for future use
  end;

  PlicgLayerHeader = ^TlicgLayerHeader;

  TlicgGraphicLink = class
  private
    FFBitmap: TBitmap;
    function GetBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
    procedure ReadEMF(const FileName: string);
    procedure ReadICO(const FileName: string);
    procedure ReadWMF(const FileName: string);
    procedure ReadBMP(const FileName: string);
    procedure ReadJPG(const FileName: string);
    procedure ReadPNG(const FileName: string); overload;
    procedure ReadPNG(const Stream: TStream); overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadGeneric(const FileName: string); overload;
    procedure ReadGeneric(const Stream: TStream; imgFormat: integer); overload;
    procedure putEMF(Bitmap: TBitmap; const FileName: string);
    procedure putBMP(Bitmap: TBitmap; const FileName: string);
    procedure putWMF(Bitmap: TBitmap; const FileName: string);
    procedure putPNG(Bitmap: TBitmap; const FileName: string; _TransParentColor: TColor);
    procedure putJPG(Bitmap: TBitmap; const FileName: string);
    procedure LoadBitmapFromStream(stream: TStream);
    { properties }
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
  end;


  { TlicgPointTool }
  TlicgPointTool = class(TlicgBaseTool)
  private
    function GetX: double;
    procedure SetX(const value: double);
    function GetY: double;
    procedure SetY(const value: double);
  public
    FPoint: TlicgCoor;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure Assign(Source: TObject); override;
  published
    property X: Double read GetX write SetX;
    property Y: Double read GetY write SetY;
  end;

  { IlicgSymbolTool }
  TlicgSymbolApply = set of (saIndex, saRotangle, saHeight);

  TlicgPenApply = set of (paStyle, paColor, paWidth, paShowArrow,
    paLineWidthDirection, paClockWise); // Nesne özellikleri toplu deðiþtir'e hat kalýnlýk yönü eklendi

  { IlicgBrushTool }

  TlicgBrushApply = set of (baPattern, baForeColor, baBackColor, baGradientFill,
    baGradientDirection, baFillFactor, baFillDirection);

  { IlicgFontTool }
  TlicgFontApply = set of (faName, faAngle, faHeight, faColor, faStyle, faHeightKats);

  TlicgVFontApply = set of (vfaName, vfaAngle, vfaHeight, vfaColor, vfaWidth, vfaHeightKats);

  { TlicgScreenGrid }
  TlicgScreenGrid = class(TPersistent)
  private
    FColor: TColor;
    FShow: Boolean;
    FStep: TlicgPointTool;
    procedure SetStep(value: TlicgPointTool);
    procedure SetColor(value: TColor);
    procedure SetShow(value: Boolean);
    function GetColor: TColor;
    function GetShow: Boolean;
    function GetStep: TlicgPointTool;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read GetColor write SetColor default clSilver;
    property Show: Boolean read GetShow write SetShow default False;
    property Step: TlicgPointTool read GetStep write SetStep;
  end;

  { TlicgThematicSeriesColor }

  TlicgThematicSeriesColor = class
  private
    FList: TList;
    function GetItem(Index: Integer): TColor;
    procedure SetItem(Index: Integer; Value: TColor);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Value: TColor);
    function Count: Integer;
    property Items[Index: Integer]: TColor read GetItem write SetItem; default;
  end;

  // OLE CONTAINER list
  TlicgOleContainerList = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteExisting(const Value: string);
    function PickExistingOrAdd(const FileName: string): TOleContainer;
    function GetFileNameWithIndex(index: Integer): string;
    function GetCount: Integer;
  end;

  TlicgOleContainerItem = class
  private
    FFileName: string;
    FOleContainer: TOleContainer;
    FOleContainerForm: TForm;
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetOleContainer: TOleContainer;
    function GetOleContainerForm: TForm;
  public
    constructor Create;
    destructor Destroy; override;
    property FileName: string read GetFileName write SetFileName;
    property OleContainer: TOleContainer read GetOleContainer;
    property OleContainerForm: TForm read GetOleContainerForm;
  end;

 { TlicgGridInfo }
  TlicgGridInfo = class(TPersistent)
  private
    FShowGrid: Boolean;
    FGrid: TlicgPointTool;
    FGridColor: TColor;
    FGridOffset: TlicgPointTool;
    FDrawAsCross: Boolean;
    FSnapToGrid: Boolean;
    //FGridSnap: TlicgPointTool;
    //Grid Snap  için GridLayer Eklendi.
    FGridLayer: TObject;
    procedure SetShowGrid(value: boolean);
    procedure SetGridColor(value: TColor);
    procedure SetGridOffset(value: TlicgPointTool);
    procedure SetGrid(value: TlicgPointTool);
    procedure SetDrawAsCross(value: boolean);
    procedure SetSnapToGrid(value: boolean);
    //procedure SetGridSnap(value: TlicgPointTool);
    function GetDrawAsCross: Boolean;
    function GetGrid: TlicgPointTool;
    function GetGridColor: TColor;
    function GetGridOffset: TlicgPointTool;
    //function  GetGridSnap: TlicgPointTool;
    function GetShowGrid: Boolean;
    function GetSnapToGrid: Boolean;
    function GetGridSnapAperture: double;
  public
    { this is here for fast access}
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property ShowGrid: Boolean read GetShowGrid write SetShowGrid default false;
    property Grid: TlicgPointTool read GetGrid write SetGrid;
    property GridColor: TColor read GetGridColor write SetGridColor default clBlack;
    property GridOffset: TlicgPointTool read GetGridOffset write SetGridOffset;
    property DrawAsCross: Boolean read GetDrawAsCross write SetDrawAsCross default False;
    property SnapToGrid: Boolean read GetSnapToGrid write SetSnapToGrid default False;
//    property GridSnap: TlicgPointTool read GetGridSnap;
    property GridLayer: TObject read FGridLayer;
    property GridSnapAperture: double read GetGridSnapAperture;
  end;

  {-------------------------------------------------------------------------------}
  {                  TlicgSortField to sort with variable type columns                 }
  {-------------------------------------------------------------------------------}
  TlicgSortFields = class;

  TlicgSortField = class(TObject)
  private
    FFields: TlicgSortFields;
    FDataType: TExprType;
    FDataSize: Integer;
    FDesc: Boolean;
    FBufferOffset: Integer;
    function GetData(Buffer: Pointer): Boolean;
    procedure SetData(Buffer: Pointer);
    function GetBufferOffset: Integer;
    function GetDataSize: Integer;
    function GetDataType: TExprType;
    function GetDesc: Boolean;
    procedure SetBufferOffset(const Value: Integer);
    procedure SetDataSize(const Value: Integer);
    procedure SetDesc(const Value: Boolean);
  protected
    function GetAsString: string; virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
    function GetAsFloat: double; virtual; abstract;
    procedure SetAsFloat(Value: double); virtual; abstract;
    function GetAsInteger: Longint; virtual; abstract;
    procedure SetAsInteger(Value: Longint); virtual; abstract;
    function GetAsBoolean: Boolean; virtual; abstract;
    procedure SetAsBoolean(Value: Boolean); virtual; abstract;
    procedure SetDataType(Value: TExprType);
  public
    constructor Create(Fields: TlicgSortFields); virtual;
    property DataType: TExprType read GetDataType write SetDataType;
    property DataSize: Integer read GetDataSize write SetDataSize;
    property Desc: Boolean read GetDesc write SetDesc;
    property BufferOffset: Integer read GetBufferOffset write SetBufferOffset;
    property AsString: string read GetAsString write SetAsString;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
  end;

  {-------------------------------------------------------------------------------}
  {                  TlicgSortStringField                                              }
  {-------------------------------------------------------------------------------}

  TlicgSortStringField = class(TlicgSortField)
  private
    function GetValue(var Value: string): Boolean;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetAsFloat: double; override;
    procedure SetAsFloat(Value: double); override;
    function GetAsInteger: Longint; override;
    procedure SetAsInteger(Value: Longint); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(Value: Boolean); override;
  public
    constructor Create(Fields: TlicgSortFields); override;
  end;

  {-------------------------------------------------------------------------------}
  {                  Define TlicgSortFloatField                                        }
  {-------------------------------------------------------------------------------}

  TlicgSortFloatField = class(TlicgSortField)
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetAsFloat: double; override;
    procedure SetAsFloat(Value: double); override;
    function GetAsInteger: Longint; override;
    procedure SetAsInteger(Value: Longint); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(Value: Boolean); override;
  public
    constructor Create(Fields: TlicgSortFields); override;
  end;

  {-------------------------------------------------------------------------------}
  {                  Define TlicgSortIntegerField                                      }
  {-------------------------------------------------------------------------------}

  TlicgSortIntegerField = class(TlicgSortField)
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetAsInteger: Longint; override;
    procedure SetAsInteger(Value: Longint); override;
    function GetAsFloat: double; override;
    procedure SetAsFloat(Value: double); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(Value: Boolean); override;
  public
    constructor Create(Fields: TlicgSortFields); override;
  end;

  {-------------------------------------------------------------------------------}
  {                  Define TlicgSortBooleanField                                      }
  {-------------------------------------------------------------------------------}

  TlicgSortBooleanField = class(TlicgSortField)
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(Value: Boolean); override;
    function GetAsInteger: Longint; override;
    procedure SetAsInteger(Value: Longint); override;
    function GetAsFloat: double; override;
    procedure SetAsFloat(Value: double); override;
  public
    constructor Create(Fields: TlicgSortFields); override;
  end;

  {-------------------------------------------------------------------------------}
  {                  Define TlicgSortFields                                            }
  {-------------------------------------------------------------------------------}

  TlicgSortList = class;

  TlicgSortFields = class
    fSortList: TlicgSortList;
    fItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TlicgSortField;
  private
    function GetSortList: TlicgSortList;
  public
    constructor Create(SortList: TlicgSortList);
    destructor Destroy; override;
    function Add(DataType: TExprType): TlicgSortField;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TlicgSortField read GetItem; default;
    property SortList: TlicgSortList read GetSortList;
  end;

  {-------------------------------------------------------------------------------}
  {                  Define TlicgSortList                                           }
  {-------------------------------------------------------------------------------}
  TlicgSortList = class(TObject)
  private
    fFields: TlicgSortFields;
    fRecNo: Integer;
    fRecordBufferSize: Integer;
    function ActiveBuffer: PByte; virtual; abstract; // ilker deðiþtirme
    function GetFields: TlicgSortFields;
  protected
    function GetFieldData(Field: TlicgSortField; Buffer: Pointer): Boolean;
      virtual; abstract;
    procedure SetFieldData(Field: TlicgSortField; Buffer: Pointer); virtual; abstract;
    procedure SetRecNo(Value: Integer);
    function GetRecNo: Integer;
    procedure SetSourceRecNo(Value: Integer); virtual; abstract;
    function GetSourceRecNo: Integer; virtual; abstract;
    function GetRecordCount: Integer; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddField(pDataType: TExprType; pDataSize: Integer; pDescending: Boolean);
    procedure Insert; virtual; abstract;
    procedure Sort;
    procedure Exchange(RecNo1, RecNo2: Integer); virtual; abstract;
    procedure Clear; virtual; abstract;
    property Count: Integer read GetRecordCount;
    property RecNo: Integer read GetRecNo write SetRecNo;
    property SourceRecNo: Integer read GetSourceRecNo write SetSourceRecNo;
    property Fields: TlicgSortFields read GetFields;
  end;

  TlicgMemSortList = class(TlicgSortList)
  private
    fBufferList: TList;
    function ActiveBuffer: PByte; override; // ilker deðiþtirme
  protected
    function GetFieldData(Field: TlicgSortField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData(Field: TlicgSortField; Buffer: Pointer); override;
    function GetRecordCount: Integer; override;
    procedure SetSourceRecNo(Value: Integer); override;
    function GetSourceRecNo: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insert; override;
    procedure Exchange(RecNo1, RecNo2: Integer); override;
    procedure Clear; override;
  end;
  { picking constants }
const
  PICKED_NONE = -3; { no point or interior picked }
  PICKED_INTERIOR = -2; { picked inside entity (only closed entities) }
  PICKED_SEGMENT = -1; { picked on a line segment }

implementation

uses
  TypInfo,
  Vcl.Imaging.jpeg,
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.GIS,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Math
  {$IFDEF USE_GRAPHICEX}
  , GraphicEx
  {$ENDIF};

{ Utilities }

procedure BitmapToWMF(Bitmap: TBitmap; MetaFile: TMetaFile);
var
  MetafileCanvas: TMetafileCanvas;
begin
  MetaFile.Width := Bitmap.Width;
  MetaFile.Height := Bitmap.Height;
  MetafileCanvas := TMetafileCanvas.CreateWithComment(MetaFile, 0, 'Licad GIS', 'Licad GIS');
  try
    BitBlt(MetafileCanvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    MetafileCanvas.Free;
  end;
end;

procedure WMFToBitmap(Bitmap: TBitmap; MetaFile: TMetaFile);
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  B.Monochrome := False;
  B.Width := MetaFile.Width;
  B.Height := MetaFile.Height;
  B.Canvas.Draw(0, 0, MetaFile);
  Bitmap.Assign(B);
  B.Free;
end;

function GetDeviceRes(DC: THandle): Integer;
var
  Noc: Integer;
begin
  Result := 8;
  Noc := GetDeviceCaps(DC, BITSPIXEL);
  if Noc <= 4 then
    Result := 4
  else if Noc = 8 then
    Result := 8
  else if Noc >= 16 then
    Result := 24;
end;

{ TlicgGraphicLink }

constructor TlicgGraphicLink.Create;
begin
  inherited Create;
  FFBitmap := nil;
end;

destructor TlicgGraphicLink.Destroy;
begin
  if FFBitmap <> nil then
    FFBitmap.Free;
  inherited Destroy;
end;

function TlicgGraphicLink.GetBitmap: TBitmap;
begin
  Result := FFBitmap;
end;

procedure TlicgGraphicLink.SetBitmap(const Value: TBitmap);
begin
  FFBitmap := Value
end;

procedure TlicgGraphicLink.readWMF(const FileName: string);
var
  Metafile: TMetafile;
begin
  Metafile := TMetafile.Create;
  try
    Metafile.LoadFromFile(FileName);
    WMFToBitmap(Bitmap, Metafile);
  finally
    Metafile.free;
  end;
end;

procedure TlicgGraphicLink.ReadEMF(const FileName: string);
begin
  ReadWMF(FileName);
end;

procedure TlicgGraphicLink.ReadICO(const FileName: string);
var
  TmpBitmap: TBitmap;
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  try
    Icon.LoadFromFile(FileName);
    TmpBitmap := CreateBitmapFromIcon(Icon, clOlive);
    Bitmap.Assign(TmpBitmap);
    TmpBitmap.Free;
  finally
    Icon.Free;
  end;
end;

procedure TlicgGraphicLink.ReadBMP(const FileName: string);
begin
  FFBitmap.LoadFromFile(FileName);
end;

procedure TlicgGraphicLink.ReadJPG(const FileName: string);
var
  Jpg: TJpegImage;
  JpegStream: TStream;
begin
  Jpg := TJpegImage.Create;
  try
    JpegStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    if Jpg.PixelFormat = jf24bit then
      Bitmap.PixelFormat := pf24bit
    else
      Bitmap.PixelFormat := pf8bit;
    Jpg.LoadFromStream(JpegStream);
    Jpg.PixelFormat := jf8bit;

    Bitmap.Width := Jpg.Width;
    Bitmap.Height := Jpg.Height;
    Bitmap.Canvas.Draw(0, 0, Jpg);
  finally
    Jpg.Free;
    JpegStream.Free;
  end;
end;

procedure TlicgGraphicLink.ReadPNG(const FileName: string);
var
  _png: TPNGObject;
begin
  _png := TPNGObject.Create;
  _png.LoadFromFile(FileName);
  _png.AssignTo(Bitmap);
  _png.Free;
end;

procedure TlicgGraphicLink.ReadPNG(const Stream: TStream);
var
  _png: TPNGObject;
begin
  _png := TPNGObject.Create;
  _png.LoadFromStream(Stream);
  _png.AssignTo(Bitmap);
  _png.Free;
end;

procedure TlicgGraphicLink.putJPG(Bitmap: TBitmap; const FileName: string);
var
  Jpg: TJpegImage;
begin
  Jpg := TJpegImage.Create;
  try
    Jpg.Assign(Bitmap);
    Jpg.PixelFormat := jf24Bit;
    //Jpg.Scale:= jsQuarter;
    Jpg.Smoothing := true;
    Jpg.CompressionQuality := 50;
    Jpg.Compress;
    Jpg.SaveToFile(FileName);
  finally
    Jpg.Free;
  end;
end;

procedure TlicgGraphicLink.putBMP(Bitmap: TBitmap; const FileName: string);
begin
  Bitmap.SaveToFile(FileName);
end;

procedure TlicgGraphicLink.putWMF(Bitmap: TBitmap; const FileName: string);
var
  Metafile: TMetafile;
begin
  Metafile := TMetafile.Create;
  try
    BitmapToWMF(Bitmap, Metafile);
    Metafile.Enhanced := false;
    Metafile.SaveToFile(FileName);
  finally
    Metafile.free;
  end;
end;

procedure TlicgGraphicLink.putEMF(Bitmap: TBitmap; const FileName: string);
var
  Metafile: TMetafile;
begin
  Metafile := TMetafile.Create;
  try

    BitmapToWMF(Bitmap, Metafile);
    Metafile.Enhanced := True;
    Metafile.SaveToFile(FileName);
  finally
    Metafile.free;
  end;
end;

procedure TlicgGraphicLink.putPNG(Bitmap: TBitmap; const FileName: string;
  _TransParentColor: TColor);
var
  _Png: TPngObject;
  c, i, j: integer;
begin
  _Png := TPngObject.Create;
  _Png.Assign(Bitmap);
  { PNG nin OGC tarafýndan transparan sayýlmasý için 4 kanallý bir formata (RGBA) sahip olmasý, ve piksele ait transparanlýðý da
    alpha'da tutmasý gerekiyor. Bu nedenle CreateAlpha ile yeni bir kanal açýp da _TransParentColor'a eþit olan piksellerin alfa
    transparanlýðýný, 0 deðeri atayarak, %100 yaptým. }
  _Png.CreateAlpha;

  c := 0;
  for j := 0 to _Png.Height - 1 do
  begin
    for i := 0 to _Png.Width - 1 do
    begin
      if _Png.Pixels[i, j] = _TransParentColor then
        _Png.AlphaScanline[0]^[c] := 0;
      inc(c);
    end;
  end;

//  if _TransParentColor <> clNone then
 //   _Png.TransparentColor := _TransParentColor;
  _Png.SaveToFile(FileName);
  _Png.Free;

end;

procedure TlicgGraphicLink.ReadGeneric(const FileName: string);
var
  Ext: string;
  Handled: Boolean;
{$IFDEF USE_GRAPHICEX}
  GraphicClass: TGraphicExGraphicClass;
{$ENDIF}
begin
  { release memory occupied by previous bitmap}
  if FFBitmap <> nil then
  begin
    FFBitmap.Free;
    FFBitmap := nil;
  end;

  { and create again for avoid memory leaks }
  FFBitmap := TBitmap.Create;
  Ext := AnsiLowerCase(ExtractFileExt(FileName));
  Handled := False;
  if AnsiCompareText(Ext, '.jpg') = 0 then
  begin
    ReadJPG(FileName);
    Handled := True;
  end
  else

if AnsiCompareText(Ext, '.png') = 0 then
  begin
    ReadPNG(FileName);
    Handled := True;
  end
  else
if AnsiCompareText(Ext, '.bmp') = 0 then
  begin
    ReadBMP(FileName);
    Handled := True;
  end
  else if AnsiCompareText(Ext, '.wmf') = 0 then
  begin
    ReadWMF(FileName);
    Handled := True;
  end
  else if AnsiCompareText(Ext, '.emf') = 0 then
  begin
    ReadEMF(FileName);
    Handled := True;
  end
  else if AnsiCompareText(Ext, '.ico') = 0 then
  begin
    ReadICO(FileName);
    Handled := True;
  end
      // graphicex is at last
{$IFDEF USE_GRAPHICEX}
  else
  begin
    GraphicClass := FileFormatList.GraphicFromContent(FileName);
    if GraphicClass <> nil then
    begin
      try
        FFBitmap := GraphicClass.Create;
        FFBitmap.LoadFromFile(FileName);
        Handled := True;
      except
        Handled := False;
      end;
    end;
  end;
{$ENDIF}
  ;
  if not Handled then
    LicadGISError(SWrongImageFormat);
end;

{ TlicgPointTool }

procedure TlicgPointTool.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FPoint, sizeof(FPoint));
end;

procedure TlicgPointTool.SaveToStream(Stream: TStream);
begin
  Stream.Write(FPoint, sizeof(FPoint));
end;

function TlicgPointTool.GetX: double;
begin
  Result := FPoint.X;
end;

procedure TlicgPointTool.SetX(const value: double);
begin
  FPoint.X := value;
end;

function TlicgPointTool.GetY: double;
begin
  Result := FPoint.Y;
end;

procedure TlicgPointTool.SetY(const value: double);
begin
  FPoint.Y := value;
end;

procedure TlicgPointTool.Assign(Source: TObject);
begin
  if Source is TlicgPointTool then
    FPoint := (Source as TlicgPointTool).FPoint
  else
    inherited;
end;

{ TlicgScreenGrid }

procedure TlicgScreenGrid.Assign(Source: TPersistent);
begin
  if Source is TlicgScreenGrid then
  begin
    FColor := (Source as TlicgScreenGrid).FColor;
    FShow := (Source as TlicgScreenGrid).FShow;
    FStep.Assign((Source as TlicgScreenGrid).FStep);
  end
  else
    inherited;
end;

constructor TlicgScreenGrid.Create;
begin
  inherited Create;
  FStep := TlicgPointTool.Create;
end;

destructor TlicgScreenGrid.Destroy;
begin
  FStep.Free;
  inherited Destroy;
end;

function TlicgScreenGrid.GetColor: TColor;
begin
  Result := FColor
end;

function TlicgScreenGrid.GetShow: Boolean;
begin
  Result := FShow
end;

function TlicgScreenGrid.GetStep: TlicgPointTool;
begin
  Result := FStep
end;

procedure TlicgScreenGrid.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FColor, sizeof(TColor));
  Stream.Read(FShow, sizeof(boolean));
  FStep.LoadFromStream(Stream);
end;

procedure TlicgScreenGrid.SaveToStream(Stream: TStream);
begin
  Stream.Write(FColor, sizeof(TColor));
  Stream.Write(FShow, sizeof(boolean));
  FStep.SaveToStream(Stream);
end;

procedure TlicgScreenGrid.SetColor(value: TColor);
begin
  FColor := value;
end;

procedure TlicgScreenGrid.SetShow(value: Boolean);
begin
  FShow := value;
end;

procedure TlicgScreenGrid.SetStep(value: TlicgPointTool);
begin
  FStep.Assign(value);
end;

{ TlicgGridInfo }

constructor TlicgGridInfo.Create;
begin
  inherited Create;
  FGridLayer := Licad.CreateLayer(nil, '', ltMemory);

  FGrid := TlicgPointTool.Create;
  FGridOffset := TlicgPointTool.Create;
  { set default values }
  FGrid.FPoint := AsCoor(100.0, 100.0); // ilker deðiþtirme orj AsCoor(1.0, 1.0);
  FDrawAsCross := True;
  FGridColor := clGray; //ilker deðiþtirme clMaroon;
  //FGridSnap := TlicgPointTool.Create;
  //FGridSnap.FPoint := AsCoor(FGrid.X/10, FGrid.Y/10);
  TlicgBaseLayer(FGridLayer).InitializeOnCreate('', False, False, nil, nil);
end;

destructor TlicgGridInfo.Destroy;
begin
  TlicgBaseLayer(FGridLayer).Close;
  TlicgBaseLayer(FGridLayer).Free;

  FGrid.Free;
  FGridOffset.Free;
  //FGridSnap.Free;
  inherited Destroy;
end;

procedure TlicgGridInfo.SetShowGrid(value: boolean);
begin
  FShowGrid := value
end;

procedure TlicgGridInfo.SetGridColor(value: TColor);
begin
  FGridColor := value
end;

procedure TlicgGridInfo.SetGridOffset(value: TlicgPointTool);
begin
  FGridOffset.Assign(value);
end;

procedure TlicgGridInfo.SetGrid(value: TlicgPointTool);
begin
  FGrid.Assign(value);
end;

procedure TlicgGridInfo.SetDrawAsCross(value: boolean);
begin
  FDrawAsCross := value
end;

procedure TlicgGridInfo.SetSnapToGrid(value: boolean);
begin
  FSnapToGrid := value;
end;

{procedure TlicgGridInfo.SetGridSnap(value: TlicgPointTool);
begin
  FGridSnap.Assign(value);
end;
}

procedure TlicgGridInfo.LoadFromStream(Stream: TStream);
begin
  { first save the objects }
  FGrid.LoadFromStream(Stream);
  FGridOffset.LoadFromStream(Stream);
  { and then ordinal types }
  with Stream do
  begin
    Read(FShowGrid, sizeof(FShowGrid));
    Read(FGridColor, sizeof(FGridColor));
    Read(FDrawAsCross, sizeof(FDrawAsCross));
  end;
end;

procedure TlicgGridInfo.SaveToStream(Stream: TStream);
begin
  { first save the objects }
  FGrid.SaveToStream(Stream);
  FGridOffset.SaveToStream(Stream);
  { and then ordinal types }
  with Stream do
  begin
    Write(FShowGrid, sizeof(FShowGrid));
    Write(FGridColor, sizeof(FGridColor));
    Write(FDrawAsCross, sizeof(FDrawAsCross));
  end;
end;

procedure TlicgGridInfo.Assign(Source: TPersistent);
begin
  if Source is TlicgGridInfo then
  begin
    FGrid.FPoint := (Source as TlicgGridInfo).FGrid.FPoint;
    FGridOffset.FPoint := (Source as TlicgGridInfo).FGridOffset.FPoint;
    FShowGrid := (Source as TlicgGridInfo).FShowGrid;
    FDrawAsCross := (Source as TlicgGridInfo).FDrawAsCross;
    FGridColor := (Source as TlicgGridInfo).FGridColor;
  end
  else
    inherited;
end;

function TlicgGridInfo.GetDrawAsCross: Boolean;
begin
  Result := FDrawAsCross
end;

function TlicgGridInfo.GetGrid: TlicgPointTool;
begin
  Result := FGrid
end;

function TlicgGridInfo.GetGridColor: TColor;
begin
  Result := FGridColor
end;

function TlicgGridInfo.GetGridOffset: TlicgPointTool;
begin
  Result := FGridOffset
end;

{
function TlicgGridInfo.GetGridSnap: TlicgPointTool;
begin
  Result:=FGridSnap
end;
}

function TlicgGridInfo.GetShowGrid: Boolean;
begin
  Result := FShowGrid
end;

function TlicgGridInfo.GetSnapToGrid: Boolean;
begin
  Result := FSnapToGrid
end;

function TlicgGridInfo.GetGridSnapAperture: double;
var
  d: double;
begin
  if FGrid.X < FGrid.Y then
    d := FGrid.X
  else
    d := FGrid.Y;
  Result := d / 2;
end;

{ TlicgThematicSeriesColor }

constructor TlicgThematicSeriesColor.Create;
begin
  inherited Create;
  FList := TList.Create;
  Add(clRed);
  Add(clYellow);
  Add(clAqua);
  Add($00FF0080);
  Add($008000FF);
  Add(clFuchsia);
  Add(clInfoBk);
  Add($00C080FF);
  Add(clTeal);
  Add($000080FF);
  Add($0080FF80);
end;

destructor TlicgThematicSeriesColor.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TlicgThematicSeriesColor.Add(Value: TColor);
begin
  FList.Add(Pointer(Value));
end;

function TlicgThematicSeriesColor.GetItem(Index: Integer): TColor;
begin
  Result := clBlack;
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  Result := TColor(FList[Index]);
end;

procedure TlicgThematicSeriesColor.SetItem(Index: Integer; Value: TColor);
begin
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  FList[Index] := Pointer(Value);
end;

function TlicgThematicSeriesColor.Count: Integer;
begin
  result := FList.Count;
end;

constructor TlicgSortField.Create(Fields: TlicgSortFields);
begin
  inherited Create;
  fFields := Fields;
end;

procedure TlicgSortField.SetDesc(const Value: Boolean);
begin
  FDesc := Value;
end;

function TlicgSortField.GetBufferOffset: Integer;
begin
  Result := FBufferOffset
end;

function TlicgSortField.GetData(Buffer: Pointer): Boolean;
begin
  Result := fFields.fSortList.GetFieldData(Self, Buffer);
end;

function TlicgSortField.GetDataSize: Integer;
begin
  Result := FDataSize
end;

function TlicgSortField.GetDataType: TExprType;
begin
  Result := FDataType
end;

function TlicgSortField.GetDesc: Boolean;
begin
  Result := FDesc
end;

procedure TlicgSortField.SetBufferOffset(const Value: Integer);
begin
  FBufferOffset := Value
end;

procedure TlicgSortField.SetData(Buffer: Pointer);
begin
  fFields.fSortList.SetFieldData(Self, Buffer);
end;

procedure TlicgSortField.SetDataSize(const Value: Integer);
begin
  FDataSize := Value
end;

procedure TlicgSortField.SetDataType(Value: TExprType);
begin
  fDataType := Value;
end;

{ TlicgSortStringField }

constructor TlicgSortStringField.Create(Fields: TlicgSortFields);
begin
  inherited Create(Fields);
  SetDataType(ttString);
end;

function TlicgSortStringField.GetValue(var Value: string): Boolean;
var
  Buffer: array[0..dsMaxStringSize] of AnsiChar; // ilker deðiþtirme
begin
  Result := GetData(@Buffer);
  if Result then
    Value := Buffer;
end;

function TlicgSortStringField.GetAsString: string;
begin
  if not GetValue(Result) then
    Result := '';
end;

procedure TlicgSortStringField.SetAsString(const Value: string);
var
  Buffer: array[0..dsMaxStringSize] of AnsiChar; // ilker deðiþtirme
  L: Integer;
begin
  FillChar(Buffer, fDataSize, 0);
  L := Length(Value);
  StrLCopy(Buffer, PAnsiChar(Value), L); // ilker deðiþtirme
  SetData(@Buffer);
end;

function TlicgSortStringField.GetAsFloat: double;
begin
  Result := 0;
end;

procedure TlicgSortStringField.SetAsFloat(Value: double);
begin
end;

function TlicgSortStringField.GetAsInteger: Longint;
begin
  Result := 0;
end;

procedure TlicgSortStringField.SetAsInteger(Value: Longint);
begin
end;

function TlicgSortStringField.GetAsBoolean: Boolean;
begin
  Result := False;
end;

procedure TlicgSortStringField.SetAsBoolean(Value: Boolean);
begin
end;

{ TlicgSortFloatField }

constructor TlicgSortFloatField.Create(Fields: TlicgSortFields);
begin
  inherited Create(Fields);
  SetDataType(ttFloat);
end;

function TlicgSortFloatField.GetAsFloat: double;
begin
  if not GetData(@Result) then
    Result := 0;
end;

procedure TlicgSortFloatField.SetAsFloat(Value: double);
begin
  SetData(@Value);
end;

function TlicgSortFloatField.GetAsString: string;
var
  F: Double;
begin
  if GetData(@F) then
    Result := FloatToStr(F)
  else
    Result := '';
end;

procedure TlicgSortFloatField.SetAsString(const Value: string);
var
  F: Extended;
begin
  if Value = '' then
    SetAsFloat(0)
  else
  begin
    if not TextToFloat(PChar(Value), F, fvExtended) then
      LicadGISError(Format(SIsInvalidFloatValue, [Value]));
    SetAsFloat(F);
  end;
end;

function TlicgSortFloatField.GetAsInteger: Longint;
begin
  Result := 0;
end;

procedure TlicgSortFloatField.SetAsInteger(Value: Longint);
begin
end;

function TlicgSortFloatField.GetAsBoolean: Boolean;
begin
  Result := False;
end;

procedure TlicgSortFloatField.SetAsBoolean(Value: Boolean);
begin
end;

{-------------------------------------------------------------------------------}
{                  implements TlicgSortIntegerField                                      }
{-------------------------------------------------------------------------------}

constructor TlicgSortIntegerField.Create(Fields: TlicgSortFields);
begin
  inherited Create(Fields);
  SetDataType(ttInteger);
end;

function TlicgSortIntegerField.GetAsInteger: Longint;
begin
  if not GetData(@Result) then
    Result := 0;
end;

procedure TlicgSortIntegerField.SetAsInteger(Value: Longint);
begin
  SetData(@Value);
end;

function TlicgSortIntegerField.GetAsString: string;
var
  L: Longint;
begin
  if GetData(@L) then
    Str(L, Result)
  else
    Result := '';
end;

procedure TlicgSortIntegerField.SetAsString(const Value: string);
var
  E: Integer;
  L: Longint;
begin
  Val(Value, L, E);
  if E <> 0 then
    LicadGISError(Format(SIsInvalidIntegerValue, [Value]));
  SetAsInteger(L);
end;

function TlicgSortIntegerField.GetAsFloat: double;
begin
  Result := 0;
end;

procedure TlicgSortIntegerField.SetAsFloat(Value: double);
begin
end;

function TlicgSortIntegerField.GetAsBoolean: Boolean;
begin
  Result := False;
end;

procedure TlicgSortIntegerField.SetAsBoolean(Value: Boolean);
begin
end;

{ TlicgSortBooleanField }

constructor TlicgSortBooleanField.Create(Fields: TlicgSortFields);
begin
  inherited Create(Fields);
  SetDataType(ttBoolean);
end;

function TlicgSortBooleanField.GetAsBoolean: Boolean;
var
  B: WordBool;
begin
  if GetData(@B) then
    Result := B
  else
    Result := False;
end;

procedure TlicgSortBooleanField.SetAsBoolean(Value: Boolean);
var
  B: WordBool;
begin
  if Value then
    Word(B) := 1
  else
    Word(B) := 0;
  SetData(@B);
end;

function TlicgSortBooleanField.GetAsString: string;
var
  B: WordBool;
begin
  if GetData(@B) then
    Result := Copy(Lider.CG.Com.Expr.NBoolean[B], 1, 1)
  else
    Result := '';
end;

procedure TlicgSortBooleanField.SetAsString(const Value: string);
var
  L: Integer;
begin
  L := Length(Value);
  if L = 0 then
  begin
    SetAsBoolean(False);
  end
  else
  begin
    if AnsiCompareText(Value, Copy(Lider.CG.Com.Expr.NBoolean[False], 1, L)) = 0 then
      SetAsBoolean(False)
    else if AnsiCompareText(Value, Copy(Lider.CG.Com.Expr.NBoolean[True], 1,
      L)) = 0 then
      SetAsBoolean(True)
    else
      LicadGISError(Format(SIsInvalidBoolValue, [Value]));
  end;
end;

function TlicgSortBooleanField.GetAsInteger: Longint;
begin
  Result := 0;
end;

procedure TlicgSortBooleanField.SetAsInteger(Value: Longint);
begin
end;

function TlicgSortBooleanField.GetAsFloat: double;
begin
  Result := 0;
end;

procedure TlicgSortBooleanField.SetAsFloat(Value: double);
begin
end;

{-------------------------------------------------------------------------------}
{                  implements TlicgSortFields                                            }
{-------------------------------------------------------------------------------}

constructor TlicgSortFields.Create(SortList: TlicgSortList);
begin
  inherited Create;
  fSortList := SortList;
  fItems := TList.Create;
end;

destructor TlicgSortFields.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

function TlicgSortFields.GetSortList: TlicgSortList;
begin
  Result := FSortList;
end;

function TlicgSortFields.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TlicgSortFields.GetItem(Index: Integer): TlicgSortField;
begin
  Result := fItems[Index];
end;

function TlicgSortFields.Add(DataType: TExprType): TlicgSortField;
begin
  Result := nil;
  case DataType of
    ttString:
      Result := TlicgSortStringField.Create(Self);
    ttFloat:
      Result := TlicgSortFloatField.Create(Self);
    ttInteger:
      Result := TlicgSortIntegerField.Create(Self);
    ttBoolean:
      Result := TlicgSortBooleanField.Create(Self);
  end;
  fItems.Add(Result);
end;

procedure TlicgSortFields.Clear;
var
  I: Integer;
begin
  for I := 0 to fItems.Count - 1 do
    TlicgSortField(fItems[I]).Free;
  fItems.Clear;
end;

{ TlicgSortList }

constructor TlicgSortList.Create;
begin
  inherited Create;
  fFields := TlicgSortFields.Create(Self);
  fRecNo := -1;
  fRecordBufferSize := SizeOf(Integer); { first data is the SourceRecNo property }
end;

destructor TlicgSortList.Destroy;
begin
  fFields.Free;
  inherited Destroy;
end;

function TlicgSortList.GetFields: TlicgSortFields;
begin
  Result := FFields;
end;

procedure TlicgSortList.SetRecNo(Value: Integer);
begin
  if (Value < 1) or (Value > GetRecordCount) then
    LicadGISError(SRecNoInvalid);
  fRecNo := Value;
end;

function TlicgSortList.GetRecNo: Integer;
begin
  Result := fRecNo;
end;

procedure TlicgSortList.AddField(pDataType: TExprType; pDataSize: Integer;
  pDescending: Boolean);
begin
  with fFields.Add(pDataType) do
  begin
    BufferOffset := fRecordBufferSize;
    DataType := pDataType;
    case DataType of
      ttString:
        DataSize := pDataSize + 1;
      ttFloat:
        DataSize := SizeOf(Double);
      ttInteger:
        DataSize := SizeOf(Integer);
      ttBoolean:
        DataSize := SizeOf(WordBool);
    end;
    Desc := pDescending;
    Inc(fRecordBufferSize, DataSize);
  end;
end;

procedure TlicgSortList.Sort;
var
  I, Idx: Integer;
  Index: Integer;
  Pivot: Integer;
  DataType: TExprType;
  IsDesc: Boolean;
  TempL, TempR: string;

  function SortCompare_S(RecNo: Integer; const Value: string): Integer;
  var
    s: string;
  begin
    SetRecNo(RecNo);
    s := fFields[Idx].AsString;
    if s = Value then
    begin
      Result := 0;
      Exit;
    end;
    if IsDesc then
    begin
      if s < Value then
        Result := 1
      else
        Result := -1;
    end
    else
    begin
      if s < Value then
        Result := -1
      else
        Result := 1;
    end;
  end;

  function SortCompare_F(RecNo: Integer; const Value: Double): Integer;
  var
    f: Double;
  begin
    SetRecNo(RecNo);
    f := fFields[Idx].AsFloat;
    if f = Value then
    begin
      Result := 0;
      Exit;
    end;
    if IsDesc then
    begin
      if f < Value then
        Result := 1
      else
        Result := -1;
    end
    else
    begin
      if f < Value then
        Result := -1
      else
        Result := 1;
    end;
  end;

  function SortCompare_I(RecNo: Integer; Value: Integer): Integer;
  var
    i: Integer;
  begin
    SetRecNo(RecNo);
    i := fFields[Idx].AsInteger;
    if i = Value then
    begin
      Result := 0;
      Exit;
    end;
    if IsDesc then
    begin
      if i < Value then
        Result := 1
      else
        Result := -1;
    end
    else
    begin
      if i < Value then
        Result := -1
      else
        Result := 1;
    end;
  end;

  function SortCompare_B(RecNo: Integer; Value: Boolean): Integer;
  var
    b: Boolean;
  begin
    SetRecNo(RecNo);
    b := fFields[Idx].AsBoolean;
    if Ord(b) = Ord(Value) then
    begin
      Result := 0;
      Exit;
    end;
    if IsDesc then
    begin
      if Ord(b) < Ord(Value) then
        Result := 1
      else
        Result := -1;
    end
    else
    begin
      if Ord(b) < Ord(Value) then
        Result := -1
      else
        Result := 1;
    end;
  end;

  procedure QuickSort(L, R: Integer);
  var
    I, J, P: Integer;
    s1: string;
    f1: Double;
    i1: Integer;
    b1: Boolean;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      SetRecNo(P);
      f1 := 0;
      i1 := 0;
      b1 := False;
      case DataType of
        ttString:
          s1 := fFields[Idx].AsString;
        ttFloat:
          f1 := fFields[Idx].AsFloat;
        ttInteger:
          i1 := fFields[Idx].AsInteger;
        ttBoolean:
          b1 := fFields[Idx].AsBoolean;
      end;
      repeat
        case DataType of
          ttString:
            begin
              while SortCompare_S(I, s1) < 0 do
                Inc(I);
            end;
          ttFloat:
            begin
              while SortCompare_F(I, f1) < 0 do
                Inc(I);
            end;
          ttInteger:
            begin
              while SortCompare_I(I, i1) < 0 do
                Inc(I);
            end;
          ttBoolean:
            begin
              while SortCompare_B(I, b1) < 0 do
                Inc(I);
            end;
        end;

        case DataType of
          ttString:
            begin
              while SortCompare_S(J, s1) > 0 do
                Dec(J);
            end;
          ttFloat:
            begin
              while SortCompare_F(J, f1) > 0 do
                Dec(J);
            end;
          ttInteger:
            begin
              while SortCompare_I(J, i1) > 0 do
                Dec(J);
            end;
          ttBoolean:
            begin
              while SortCompare_B(J, b1) > 0 do
                Dec(J);
            end;
        end;
        if I <= J then
        begin
          Exchange(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if (fFields.Count = 0) or (GetRecordCount = 0) then
    Exit;
  Idx := 0;
  DataType := fFields[0].DataType;
  IsDesc := fFields[0].Desc;
  QuickSort(1, GetRecordCount);
  for Idx := 1 to fFields.Count - 1 do
  begin
    SetRecNo(1);
    DataType := fFields[Idx].DataType;
    IsDesc := fFields[Idx].Desc;
    Index := 1;
    Pivot := 1;
    TempL := '';
    for I := 0 to Idx - 1 do
      TempL := TempL + fFields[I].AsString;
    while Index <= GetRecordCount do
    begin
      SetRecNo(Index);
      TempR := '';
      for I := 0 to Idx - 1 do
        TempR := TempR + fFields[I].AsString;
      if TempL <> TempR then
      begin
        if Index - 1 > Pivot then
          QuickSort(Pivot, Index - 1);

        Pivot := Index;
        SetRecNo(Pivot);
        TempL := TempR;
        Index := Pivot - 1;
      end;
      Inc(Index);
    end;
    if ((Index - 1) <= GetRecordCount) and ((Index - 1) > Pivot) then
      QuickSort(Pivot, Index - 1);
  end;
end;

{ TlicgMemSortList }

constructor TlicgMemSortList.Create;
begin
  inherited Create;
  fBufferList := TList.Create;
end;

destructor TlicgMemSortList.Destroy;
begin
  Clear;
  fBufferList.Free;
  inherited Destroy;
end;

procedure TlicgMemSortList.Clear;
var
  I: Integer;
  Buffer: PByte; // ilker deðiþtirme
begin
  for I := 0 to fBufferList.Count - 1 do
  begin
    Buffer := fBufferList[I];
    FreeMem(Buffer, fRecordBufferSize);
  end;
  fBufferList.Clear;
  fFields.Clear;
  fRecordBufferSize := SizeOf(Integer);
  fRecNo := -1;
end;

function TlicgMemSortList.ActiveBuffer: PByte; // ilker deðiþtirme
begin
  Result := nil;
  if (fRecNo < 1) or (fRecNo > fBufferList.Count) then
    Exit;
  Result := fBufferList[fRecNo - 1];
end;

function TlicgMemSortList.GetFieldData(Field: TlicgSortField; Buffer: Pointer): Boolean;
var
  RecBuf: PByte; // ilker deðiþtirme
begin
  Result := False;
  RecBuf := ActiveBuffer;
  if RecBuf = nil then
    Exit;
  Move((RecBuf + Field.BufferOffset)^, Buffer^, Field.DataSize);
  Result := True;
end;

procedure TlicgMemSortList.SetFieldData(Field: TlicgSortField; Buffer: Pointer);
var
  RecBuf: PByte; // ilker deðiþtirme
begin
  RecBuf := ActiveBuffer;
  if (RecBuf = nil) or (Buffer = nil) then
    Exit;
  Move(Buffer^, (RecBuf + Field.BufferOffset)^, Field.DataSize);
end;

procedure TlicgMemSortList.Insert;
var
  Buffer: PByte; // ilker deðiþtirme
begin
  GetMem(Buffer, fRecordBufferSize);
  FillChar(Buffer^, fRecordBufferSize, 0);
  fBufferList.Add(Buffer);
  fRecNo := fBufferList.Count;
end;

function TlicgMemSortList.GetRecordCount: Integer;
begin
  Result := fBufferList.Count;
end;

procedure TlicgMemSortList.Exchange(RecNo1, RecNo2: Integer);
begin
  fBufferList.Exchange(RecNo1 - 1, RecNo2 - 1);
end;

function TlicgMemSortList.GetSourceRecNo: Integer;
var
  Buffer: PByte; // ilker deðiþtirme
begin
  Result := 0;
  if (fRecNo < 1) or (fRecNo > GetRecordCount) then
    Exit;
  Buffer := PByte(fBufferList[fRecNo - 1]); // ilker deðiþtirme
  Move((Buffer + 0)^, Result, SizeOf(Integer));
end;

procedure TlicgMemSortList.SetSourceRecNo(Value: Integer);
var
  Buffer: PByte; // ilker deðiþtirme
begin
  if (fRecNo < 1) or (fRecNo > GetRecordCount) then
    Exit;
  Buffer := PByte(fBufferList[fRecNo - 1]); // ilker deðiþtirme
  Move(Value, (Buffer + 0)^, SizeOf(Integer));
end;

{ TlicgOleContainerItem }

constructor TlicgOleContainerItem.Create;
begin
  inherited Create;
  FOleContainer := TOleContainer.Create(OleContainerForm {Licad.Settings.OleContainerForm});
  FOleContainer.Parent := OleContainerForm; {Licad.Settings.OleContainerForm}
  ;
  FOleContainer.SizeMode := smStretch;
  //FOleContainer.Align:= alClient;
  FOleContainer.Color := clBlue;
end;

destructor TlicgOleContainerItem.Destroy;
begin
  FOleContainer.Parent := nil;
  if FOleContainer <> nil then
    FOleContainer.Free;
  if FOleContainerForm <> nil then
  begin
    FOleContainerForm.Free;
    FOleContainerForm := nil;
  end;

  inherited;
end;

function TlicgOleContainerItem.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TlicgOleContainerItem.SetFileName(const Value: string);
var
  temp: string;
begin
//  FFileName:= ExtractFileName(Value);
  FFileName := Value;
  //temp:= AddSlash(Licad.Path.GetLicad) + FFileName;
  temp := FFileName;
  if not FileExists(temp) then
  begin
    if FOleContainer.OleObjectInterface <> nil then
      FOleContainer.DestroyObject;
    Exit;
  end;
  with FOleContainer do
  begin
    if OleObjectInterface <> nil then
      DestroyObject;
    CreateObjectFromFile(temp, False);
    //DoVerb(ovShow);
  end;
end;

function TlicgOleContainerItem.GetOleContainerForm: TForm;
begin
  if FOleContainerForm = nil then
  begin
    FOleContainerForm := TForm.Create(nil);
    FOleContainerForm.Visible := False;
  end;
  Result := FOleContainerForm
end;

function TlicgOleContainerItem.GetOleContainer: TOleContainer;
begin
  Result := FOleContainer;
end;

{ TlicgOleContainerList }

constructor TlicgOleContainerList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TlicgOleContainerList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TlicgOleContainerList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    TlicgOleContainerItem(FList[i]).Free;
    FList[i] := nil;
  end;
  FList.Clear;
end;

procedure TlicgOleContainerList.DeleteExisting(const Value: string);
var
  I: Integer;
  item: TlicgOleContainerItem;
begin
  for I := FList.Count - 1 downto 0 do
  begin
    item := TlicgOleContainerItem(FList[i]);
    if AnsiCompareText(Value, item.FileName) = 0 then
    begin
      item.Free;
      FList.Delete(i);
      Break;
    end;
  end;
end;

function TlicgOleContainerList.PickExistingOrAdd(const FileName: string): TOleContainer;
var
  temp: string;
  I, n: Integer;
  item: TlicgOleContainerItem;
begin
//  temp:= ExtractFileName(FileName);
  temp := FileName;
  n := -1;
  for I := 0 to FList.Count - 1 do
  begin
    item := TlicgOleContainerItem(FList[i]);
    if AnsiCompareText(temp, item.FileName) = 0 then
    begin
      n := i;
      Break;
    end;
  end;

  if n < 0 then
  begin
    // add the item
    item := TlicgOleContainerItem.Create;
    item.FileName := temp;
    if item.OleContainer.OleObjectInterface <> nil then
    begin
      FList.Add(item);
      Result := item.OleContainer;
    end
    else
    begin
      item.Free;
      Result := nil;
    end;
  end
  else
  begin
    item := TlicgOleContainerItem(FList[i]);
    if item.OleContainer.OleObjectInterface <> nil then
    begin
      Result := item.OleContainer;
      //Result.UpdateObject;
      //Result.DoVerb(ovShow);
    end
    else
      Result := nil;
  end;
end;

function TlicgOleContainerList.GetCount: Integer;
begin
  result := FList.Count;
end;

function TlicgOleContainerList.GetFileNameWithIndex(index: Integer): string;
begin
  if (index > -1) and (index < FList.Count) then
  begin
    result := TlicgOleContainerItem(FList[index]).FileName;
  end;
end;

procedure TlicgGraphicLink.LoadBitmapFromStream(stream: TStream);
begin
  if FFBitmap = nil then
    FFBitmap := TBitmap.Create;

  FFBitmap.LoadFromStream(stream);
end;

procedure TlicgGraphicLink.ReadGeneric(const Stream: TStream; imgFormat: integer);
var
  Handled: Boolean;
{$IFDEF USE_GRAPHICEX}
  GraphicClass: TGraphicExGraphicClass;
{$ENDIF}
begin
  { release memory occupied by previous bitmap}
  if FFBitmap <> nil then
  begin
    FFBitmap.Free;
    FFBitmap := nil;
  end;

  { and create again for avoid memory leaks }
  FFBitmap := TBitmap.Create;

  Handled := False;

  if imgFormat = 0 then
  begin
    ReadPNG(Stream);
    Handled := True;
  end;

 {
  if AnsiCompareText(Ext, '.jpg') = 0 then
  begin
    ReadJPG(FileName);
    Handled := True;
  end
  else
  }
 {
 else

 if AnsiCompareText(Ext, '.bmp') = 0 then
    begin
      ReadBMP(FileName);
      Handled := True;
    end
    else if AnsiCompareText(Ext, '.wmf') = 0 then
    begin
      ReadWMF(FileName);
      Handled := True;
    end
    else if AnsiCompareText(Ext, '.emf') = 0 then
    begin
      ReadEMF(FileName);
      Handled := True;
    end
    else if AnsiCompareText(Ext, '.ico') = 0 then
    begin
      ReadICO(FileName);
      Handled := True;
    end
 }     // graphicex is at last
{$IFDEF USE_GRAPHICEX}
    {else
    begin
      GraphicClass := FileFormatList.GraphicFromContent(FileName);
      if GraphicClass <> nil then
      begin
        try
          FFBitmap := GraphicClass.Create;
          FFBitmap.LoadFromFile(FileName);
          Handled := True;
        except
          Handled := False;
        end;
      end;
    end;}
{$ENDIF}
  ;

  if not Handled then
    LicadGISError(SWrongImageFormat);

end;

initialization
  RegisterClasses([
    {TlicgPointTool,
      TlicgBaseSymbolTool,
      TlicgBasePenTool,
      TlicgBaseBrushTool,
      TlicgBaseFontTool,
      TlicgPolyClipTool,}
        TlicgScreenGrid, TlicgGridInfo]);

end.