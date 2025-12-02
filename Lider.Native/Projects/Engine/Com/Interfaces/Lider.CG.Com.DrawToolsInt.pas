unit Lider.CG.Com.DrawToolsInt;

interface

uses
  Classes, Graphics, Windows,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TLabelPositon = (lpdCenter, lpdLowerLeft, lpdLowerUp, lpdUpperUp, lpdUpperLeft,
    lpdCenterLeft, lpdCenterUp, lpdCenterRight, lpdCenterBottom);

  IlicgFontTool = interface;

  TDynamicEntityNameLabelingInfo = record
    BrushColor: integer;
    FontHeight: integer;
    FontColor: integer;
    FontStyle: TFontStyles;
    FontName: string;
    LabelPos: TLabelPositon;
    LabelOffset: double;
  end;

  TSymBitmap = record
    Bmp: Graphics.TBitmap;
    ppm: double;
  end;

  { DrawTools }

  IlicgBaseTool = interface
    ['{11B03016-5E01-4897-96A1-3FCC9F3BBAF9}']
    procedure SetModified(value: boolean); stdcall;
    function GetModified: boolean; stdcall;
    property Modified: boolean read GetModified write SetModified;
  end;

  {fill style record}
  TlicgFillDirection = (fdTopBottom, fdBottomTop, fdLeftRight, fdRightLeft, fdFromCenter);

  TlicgBrushStyle = packed record
    Pattern: ShortInt; // fill pattern 0= no fill, 1=solid, etc. < 0 = hatch ala AutoCAD
    ForeColor: TColor; // will be the Color in a hatch
    BackColor: TColor; // will be the Scale in a hatch
    // *** 2.0 *** Added
    GradientFill: Boolean; // ForeColor is the start color and BackColor is the EndColor
    GradientDirection: TlicgFillDirection; // the direction of flow of the gradient
    FillDirection: TlicgFillDirection; // how the element of the fill is considered
    FillFactor: Single; // percent of fill (0,0.5,1.0,etc.)
  end;

  IlicgBrushTool = interface(IlicgBaseTool)
    ['{C8C87723-02AF-4D9C-B154-74BDCFE5735C}']
    function isEqual(Source: IlicgBrushTool): boolean; stdcall;
    procedure Copy(Source: TlicgBrushStyle); stdcall;
    procedure Assign(Source: IlicgBrushTool); stdcall;
    procedure FillStyle(var Source: TlicgBrushStyle); stdcall;
    function GetPattern: Integer; stdcall;
    procedure SetPattern(Value: Integer); stdcall;
    property Pattern: Integer read GetPattern write SetPattern;
    function GetForeColor: TColor; stdcall;
    procedure SetForeColor(const Value: TColor); stdcall;
    property ForeColor: TColor read GetForeColor write SetForeColor;
    function GetBackColor: TColor; stdcall;
    procedure SetBackColor(const Value: TColor); stdcall;
    property BackColor: TColor read GetBackColor write SetBackColor;
    function GetFillDirection: TlicgFillDirection; stdcall;
    procedure SetFillDirection(const Value: TlicgFillDirection); stdcall;
    property FillDirection: TlicgFillDirection read GetFillDirection write
      SetFillDirection;
    function GetFillFactor: Single; stdcall;
    procedure SetFillFactor(const Value: Single); stdcall;
    property FillFactor: Single read GetFillFactor write SetFillFactor;
    function GetGradientFill: Boolean; stdcall;
    procedure SetGradientFill(const Value: Boolean); stdcall;
    property GradientFill: Boolean read GetGradientFill write SetGradientFill;
    function GetGradientDirection: TlicgFillDirection; stdcall;
    procedure SetGradientDirection(const Value: TlicgFillDirection); stdcall;
    property GradientDirection: TlicgFillDirection read GetGradientDirection
      write SetGradientDirection;
//    function getBrushStyle: TlicgBrushStyle; stdcall;
//    procedure setBrushStyle(Value: TlicgBrushStyle); stdcall;
//    property BrushStyle: TlicgBrushStyle read getBrushStyle write setBrushStyle;
  end;

  { the font style }
  TlicgFontStyle = packed record
    Name: string[(LF_FACESIZE - 1) * 4]; // ilker deðiþtirme Name: string[LF_FACESIZE - 1];
    CharSet: TFontCharSet;
    Style: TFontStyles;
    Color: TColor;
    Angle: Single;
    Height: Double; // in real map Units or < 0 in fixed pixel Units
    Bossssss1: Boolean; // ilker uður hiçbir yerde kullanýlmadý layer header okurken kullanýlýyor
    Bossssss2: TColor; // ilker uður hiçbir yerde kullanýlmadý layer header okurken kullanýlýyor
    CharWidthFactor: Single;
    CharSpacing: Single;
    (* uður ekleme baþlama *)
    TextPos: TlicgTextPos;
    (* uður ekleme bitme *)
  end;

  IlicgFontTool = interface(IlicgBaseTool)
    ['{0F3A9563-1DCD-43DB-9D12-86CFCD07ABBE}']
    function isEqual(Source: IlicgFontTool): boolean; stdcall;
    procedure Copy(Source: TlicgFontStyle); stdcall;
    procedure Assign(Source: IlicgFontTool); stdcall;
    procedure FillStyle(var Source: TlicgFontStyle); stdcall;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
    property Name: string read GetName write SetName;
    function GetColor: TColor; stdcall;
    procedure SetColor(Value: TColor); stdcall;
    property Color: TColor read GetColor write SetColor;
    function GetAngle: single; stdcall;
    procedure SetAngle(const Value: single); stdcall;
    property Angle: single read GetAngle write SetAngle;
    function GetHeight: Double; stdcall;
    procedure SetHeight(const Value: Double); stdcall;
    property Height: Double read GetHeight write SetHeight;
    function GetStyle: TFontStyles; stdcall;
    procedure SetStyle(const Value: TFontStyles); stdcall;
    property Style: TFontStyles read GetStyle write SetStyle;
    function GetCharSet: TFontCharSet; stdcall;
    procedure SetCharSet(const Value: TFontCharSet); stdcall;
    property CharSet: TFontCharSet read GetCharSet write SetCharSet;
    (*uður ekleme*)
    function GetCharWidthFactor: Single; stdcall;
    procedure SetCharWidthFactor(AValue: Single); stdcall;
    property CharWidthFactor: Single read GetCharWidthFactor write SetCharWidthFactor;

    function GetCharSpacing: Single; stdcall;
    procedure SetCharSpacing(AValue: Single); stdcall;
    property CharSpacing: Single read GetCharSpacing write SetCharSpacing;

    function GetTextPos: TlicgTextPos; stdcall;
    procedure SetTextPos(Value: TlicgTextPos); stdcall;
    property TextPos: TlicgTextPos read GetTextPos write SetTextPos;
    (*uður ekleme Bitiþ*)
  end;

  // Hat kalýnlýðýnýn hangi yöne verileceðini gösterir. TlicgOpenedEntity ye eklendi.
  TlicgLineWidthDirection = (lwdCenter, lwdRight, lwdLeft);

  { Line style record }
  TlicgPenStyle = packed record
    //Style: ShortInt; Entity Vers. 27 // *** 2.0 *** line Style, 0= no draw, 1=solid, etc. :: < 0 is a vectorial line type
    Style: Smallint; // *** 2.0 *** line Style, 0= no draw, 1=solid, etc. :: < 0 is a vectorial line type
    Color: TColor;   // color of line
    Width: Double;   // <0 is in Geometry.Points and fixed size, >0 is in real Units
    Flow: Boolean;  // flow along the segments of lines or start drawing on every segment
  end;

  IlicgPenTool = interface(IlicgBaseTool)
    ['{494E076C-4DC3-4169-8B6A-BBA8D0AF5148}']
    function isEqual(Source: IlicgPenTool): boolean; stdcall;
    procedure Copy(Source: TlicgPenStyle); stdcall;
    procedure Assign(Source: IlicgPenTool); stdcall;
    procedure FillStyle(var Source: TlicgPenStyle); stdcall;
//    function getPenStyle: TlicgPenStyle; stdcall;
//    procedure setPenStyle(Value: TlicgPenStyle); stdcall;
//    property PenStyle: TlicgPenStyle read getPenStyle write setPenStyle;
    function GetFlow: Boolean; stdcall;
    procedure SetFlow(const Value: Boolean); stdcall;
    property Flow: Boolean read GetFlow write SetFlow;
    function GetStyle: Integer; stdcall;
    procedure SetStyle(Value: Integer); stdcall;
    property Style: Integer read GetStyle write SetStyle;
    function GetColor: TColor; stdcall;
    procedure SetColor(Value: TColor); stdcall;
    property Color: TColor read GetColor write SetColor;
    function GetWidth: Double; stdcall;
    procedure SetWidth(const Value: Double); stdcall;
    property Width: Double read GetWidth write SetWidth;
    function GetLineWidthDirection: TlicgLineWidthDirection; stdcall;
    procedure SetLineWidthDirection(value: TlicgLineWidthDirection); stdcall;
    property LineWidthDirection: TlicgLineWidthDirection read
      GetLineWidthDirection write SetLineWidthDirection;
  end;

  TlicgArrowShape = (asSolid, asClear, asRecessedSolid, asRecessedClear, asChevron);

  TlicgArrowPosition = (apStart, apMiddle, apEnd);

  TlicgArrowPositions = set of TlicgArrowPosition;

  { Line style record }
  TlicgArrowStyle = packed record
    ShowArrow: Boolean;
    ArrowShape: TlicgArrowShape;
    ArrowPositions: TlicgArrowPositions;
  end;

  IlicgArrowTool = interface(IlicgBaseTool)
    ['{033D77DC-7B50-4474-ABB2-F6EB4162A641}']
    function getShowArrow: Boolean; stdcall;
    procedure setShowArrow(value: Boolean); stdcall;
    property ShowArrow: Boolean read getShowArrow write SetShowArrow;
    procedure setArrowShape(value: TlicgArrowShape); stdcall;
    function getArrowShape: TlicgArrowShape; stdcall;
    property ArrowShape: TlicgArrowShape read getArrowShape write SetArrowShape;
    procedure setArrowPositions(value: TlicgArrowPositions); stdcall;
    function getArrowPositions: TlicgArrowPositions; stdcall;
    property ArrowPositions: TlicgArrowPositions read getArrowPositions write
      SetArrowPositions;
  end;

  { Symbol style }
  TlicgSymbolStyle = packed record
    Index: Word;
    Rotangle: Single; // rotation angle in radians
    Height: Double; // <0 is in Geometry.Points and fixed size, >0 is in real Units
  end;

  { Block style }
  TlicgBlockStyle = packed record
    Index: Word;
    Rotangle: Single; // rotation angle in radians
    Height: Double; // <0 is in Geometry.Points and fixed size, >0 is in real Units
  end;

  IlicgSymbolTool = interface(IlicgBaseTool)
    ['{4597FFAD-6E3C-435D-ADBF-03F35DA4C385}']
    function isEqual(Source: IlicgSymbolTool): boolean; stdcall;
    procedure Copy(Source: TlicgSymbolStyle); stdcall;
    procedure Assign(Source: IlicgSymbolTool); stdcall;
    procedure FillStyle(var Source: TlicgSymbolStyle); stdcall;
//    function getSymbolStyle: TlicgSymbolStyle; stdcall;
//    procedure setSymbolStyle(value: TlicgSymbolStyle); stdcall;
//    property SymbolStyle: TlicgSymbolStyle read getSymbolStyle write setSymbolStyle;
    function GetIndex: Integer; stdcall;
    procedure SetIndex(Value: Integer); stdcall;
    property Index: Integer read GetIndex write SetIndex;
    function GetSymbolFileIndex: Integer; stdcall;
    procedure SetSymbolFileIndex(Value: Integer); stdcall;
    function GetRotangle: Single; stdcall;
    procedure SetRotangle(const Value: Single); stdcall;
    property Rotangle: Single read GetRotangle write SetRotangle;
    function GetHeight: Double; stdcall;
    procedure SetHeight(const Value: Double); stdcall;
    property Height: Double read GetHeight write SetHeight;
    property SymbolFileIndex: Integer read GetSymbolFileIndex write SetSymbolFileIndex;
  end;

  IlicgBlockTool = interface(IlicgBaseTool)
    ['{C06B5E0E-CFF0-4C63-8349-A3E8319A9F2D}']
    function isEqual(Source: IlicgBlockTool): boolean; stdcall;
    procedure Copy(Source: TlicgBlockStyle); stdcall;
    procedure Assign(Source: IlicgBlockTool); stdcall;
    procedure FillStyle(var Source: TlicgBlockStyle); stdcall;
//    function getBlockStyle: TlicgBlockStyle; stdcall;
//    procedure setBlockStyle(value: TlicgBlockStyle); stdcall;
//    property BlockStyle: TlicgBlockStyle read getBlockStyle write setBlockStyle;
    function GetIndex: Integer; stdcall;
    procedure SetIndex(Value: Integer); stdcall;
    property Index: Integer read GetIndex write SetIndex;
    function GetRotangle: Single; stdcall;
    procedure SetRotangle(const Value: Single); stdcall;
    property Rotangle: Single read GetRotangle write SetRotangle;
    function GetHeight: Double; stdcall;
    procedure SetHeight(const Value: Double); stdcall;
    property Height: Double read GetHeight write SetHeight;
  end;

  IlicgDrawTools = interface
    ['{EBFF94A7-AE20-41EA-89CA-77E2AB5F5EDF}']
    procedure Assign(const Source: IlicgDrawTools); stdcall;
    function BrushTool: IlicgBrushTool; stdcall;
    function FontTool: IlicgFontTool; stdcall;
    function PenTool: IlicgPenTool; stdcall;
    function ArrowTool: IlicgArrowTool; stdcall;
    function SymbolTool: IlicgSymbolTool; stdcall;
    function BlockTool: IlicgBlockTool; stdcall;
    function GetBitmap: Graphics.TBitmap; stdcall;
    procedure SetBitmap(const Value: Graphics.TBitmap); stdcall;
    property Bitmap: Graphics.TBitmap read GetBitmap write SetBitmap;
    function GetSymBitmap: TSymBitmap; stdcall;
    procedure SetSymBitmap(const Value: TSymBitmap); stdcall;
    property SymBitmap: TSymBitmap read GetSymBitmap write SetSymBitmap;
  end;

type
  TlicgBaseTool = class(TInterfacedObject, IlicgBaseTool)
  protected
    FModified: boolean;
  public
    procedure Assign(Source: TObject); virtual;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure SetModified(value: boolean); stdcall;
    function GetModified: boolean; stdcall;
    property Modified: boolean read GetModified write SetModified;
  end;

  TlicgBaseBrushTool = class(TlicgBaseTool, IlicgBrushTool)
  protected
    //FlicgrushStyle: TlicgBrushStyle;
  public
    function isEqual(Source: IlicgBrushTool): boolean; stdcall;
    procedure Copy(Source: TlicgBrushStyle); stdcall;
    procedure Assign(Source: IlicgBrushTool); stdcall;
    procedure FillStyle(var AValue: TlicgBrushStyle); stdcall;

    {IlicgBaseBrushTool}
    function GetPattern: Integer; virtual; stdcall; abstract;
    procedure SetPattern(Value: Integer); virtual; stdcall; abstract;
    property Pattern: Integer read GetPattern write SetPattern;
    function GetForeColor: TColor; virtual; stdcall; abstract;
    procedure SetForeColor(const Value: TColor); virtual; stdcall; abstract;
    property ForeColor: TColor read GetForeColor write SetForeColor;
    function GetBackColor: TColor; virtual; stdcall; abstract;
    procedure SetBackColor(const Value: TColor); virtual; stdcall; abstract;
    property BackColor: TColor read GetBackColor write SetBackColor;
    function GetFillDirection: TlicgFillDirection; virtual; stdcall; abstract;
    procedure SetFillDirection(const Value: TlicgFillDirection); virtual;
      stdcall; abstract;
    property FillDirection: TlicgFillDirection read GetFillDirection write
      SetFillDirection;
    function GetFillFactor: Single; virtual; stdcall; abstract;
    procedure SetFillFactor(const Value: Single); virtual; stdcall; abstract;
    property FillFactor: Single read GetFillFactor write SetFillFactor;
    function GetGradientFill: Boolean; virtual; stdcall; abstract;
    procedure SetGradientFill(const Value: Boolean); virtual; stdcall; abstract;
    property GradientFill: Boolean read GetGradientFill write SetGradientFill;
    function GetGradientDirection: TlicgFillDirection; virtual; stdcall; abstract;
    procedure SetGradientDirection(const Value: TlicgFillDirection); virtual;
      stdcall; abstract;
    property GradientDirection: TlicgFillDirection read GetGradientDirection
      write SetGradientDirection;
    function GetBrushStyle: TlicgBrushStyle; virtual; stdcall; abstract;
    procedure SetBrushStyle(Value: TlicgBrushStyle); virtual; stdcall; abstract;
    property BrushStyle: TlicgBrushStyle read GetBrushStyle write SetBrushStyle;
  end;

  TlicgBaseFontTool  = class(TlicgBaseTool, IlicgFontTool)
  private
  protected
  public
    function IsEqual(Source: IlicgFontTool): boolean; stdcall;
    procedure Copy(Source: TlicgFontStyle); stdcall;
    procedure Assign(Source: IlicgFontTool); stdcall;
    procedure FillStyle(var AValue: TlicgFontStyle); stdcall;
    function getFontStyle: TlicgFontStyle; virtual; stdcall; abstract;
    procedure setFontStyle(value: TlicgFontStyle); virtual; stdcall; abstract;
    property FontStyle: TlicgFontStyle read getFontStyle write setFontStyle;
    function GetName: string; virtual; stdcall; abstract;
    procedure SetName(const Value: string); virtual; stdcall; abstract;
    property Name: string read GetName write SetName;
    function GetColor: TColor; virtual; stdcall; abstract;
    procedure SetColor(Value: TColor); virtual; stdcall; abstract;
    property Color: TColor read GetColor write SetColor;
    function GetAngle: single; virtual; stdcall; abstract;
    procedure SetAngle(const Value: single); virtual; stdcall; abstract;
    property Angle: single read GetAngle write SetAngle;
    function GetHeight: Double; virtual; stdcall; abstract;
    procedure SetHeight(const Value: Double); virtual; stdcall; abstract;
    property Height: Double read GetHeight write SetHeight;
    function GetStyle: TFontStyles; virtual; stdcall; abstract;
    procedure SetStyle(const Value: TFontStyles); virtual; stdcall; abstract;
    property Style: TFontStyles read GetStyle write SetStyle;
    function GetCharSet: TFontCharSet; virtual; stdcall; abstract;
    procedure SetCharSet(const Value: TFontCharSet); virtual; stdcall; abstract;
    property CharSet: TFontCharSet read GetCharSet write SetCharSet default DEFAULT_CHARSET;
    (*uður ekleme*)
    function GetCharWidthFactor: Single; virtual; stdcall;  abstract;
    procedure SetCharWidthFactor(AValue: Single); virtual; stdcall;  abstract;
    property CharWidthFactor: Single read GetCharWidthFactor write SetCharWidthFactor;

    function GetCharSpacing: Single; virtual; stdcall;  abstract;
    procedure SetCharSpacing(AValue: Single); virtual; stdcall;  abstract;
    property CharSpacing: Single read GetCharSpacing write SetCharSpacing;

    function GetTextPos: TlicgTextPos; virtual; stdcall;  abstract;
    procedure SetTextPos(Value: TlicgTextPos); virtual; stdcall;  abstract;
    property TextPos: TlicgTextPos read GetTextPos write SetTextPos;
    (*uður ekleme Bitiþ*)
  end;

  TlicgBasePenTool = class(TlicgBaseTool, IlicgPenTool)
  protected
    //FPenStyle: TlicgPenStyle;
    //FLineWidthDirection : TlicgLineWidthDirection;
  public
    function isEqual(Source: IlicgPenTool): boolean; stdcall;
    procedure Copy(Source: TlicgPenStyle); stdcall;
    procedure Assign(Source: IlicgPenTool); stdcall;
    procedure FillStyle(var AValue: TlicgPenStyle); stdcall;
    function getPenStyle: TlicgPenStyle; virtual; stdcall; abstract;
    procedure setPenStyle(Value: TlicgPenStyle); virtual; stdcall; abstract;
    property PenStyle: TlicgPenStyle read getPenStyle write setPenStyle;
    function GetFlow: Boolean; virtual; stdcall; abstract;
    procedure SetFlow(const Value: Boolean); virtual; stdcall; abstract;
    property Flow: Boolean read GetFlow write SetFlow;
    function GetStyle: Integer; virtual; stdcall; abstract;
    procedure SetStyle(Value: Integer); virtual; stdcall; abstract;
    property Style: Integer read GetStyle write SetStyle;
    function GetColor: TColor; virtual; stdcall; abstract;
    procedure SetColor(Value: TColor); virtual; stdcall; abstract;
    property Color: TColor read GetColor write SetColor;
    function GetWidth: Double; virtual; stdcall; abstract;
    procedure SetWidth(const Value: Double); virtual; stdcall; abstract;
    property Width: Double read GetWidth write SetWidth;
    function GetLineWidthDirection: TlicgLineWidthDirection; virtual; stdcall; abstract;
    procedure SetLineWidthDirection(value: TlicgLineWidthDirection); virtual;
      stdcall; abstract;
    property LineWidthDirection: TlicgLineWidthDirection read
      GetLineWidthDirection write SetLineWidthDirection;
  end;

  TlicgBaseArrowTool = class(TlicgBaseTool, IlicgArrowTool)
  protected
    //FArrowStyle : TlicgArrowStyle;
  public
    function getShowArrow: Boolean; virtual; stdcall; abstract;
    procedure setShowArrow(value: Boolean); virtual; stdcall; abstract;
    property ShowArrow: Boolean read getShowArrow write SetShowArrow;
    procedure setArrowShape(value: TlicgArrowShape); virtual; stdcall; abstract;
    function getArrowShape: TlicgArrowShape; virtual; stdcall; abstract;
    property ArrowShape: TlicgArrowShape read getArrowShape write SetArrowShape;
    procedure setArrowPositions(value: TlicgArrowPositions); virtual; stdcall; abstract;
    function getArrowPositions: TlicgArrowPositions; virtual; stdcall; abstract;
    property ArrowPositions: TlicgArrowPositions read getArrowPositions write
      SetArrowPositions;
  end;

  TlicgBaseSymbolTool = class(TlicgBaseTool, IlicgSymbolTool)
  public
    function isEqual(Source: IlicgSymbolTool): boolean; stdcall;
    procedure Copy(Source: TlicgSymbolStyle); stdcall;
    procedure Assign(Source: IlicgSymbolTool); stdcall;
    procedure FillStyle(var AValue: TlicgSymbolStyle); stdcall;
    function getSymbolStyle: TlicgSymbolStyle; virtual; stdcall; abstract;
    procedure setSymbolStyle(value: TlicgSymbolStyle); virtual; stdcall; abstract;
    property SymbolStyle: TlicgSymbolStyle read getSymbolStyle write setSymbolStyle;
    function GetIndex: Integer; virtual; stdcall; abstract;
    procedure SetIndex(Value: Integer); virtual; stdcall; abstract;
    property Index: Integer read GetIndex write SetIndex;
    function GetSymbolFileIndex: Integer; virtual; stdcall; abstract;
    procedure SetSymbolFileIndex(Value: Integer); virtual; stdcall; abstract;
    property SymbolFileIndex: Integer read GetSymbolFileIndex write SetSymbolFileIndex;
    function GetRotangle: Single; virtual; stdcall; abstract;
    procedure SetRotangle(const Value: Single); virtual; stdcall; abstract;
    property Rotangle: Single read GetRotangle write SetRotangle;
    function GetHeight: Double; virtual; stdcall; abstract;
    procedure SetHeight(const Value: Double); virtual; stdcall; abstract;
    property Height: Double read GetHeight write SetHeight;
  end;

  TlicgBaseBlockTool = class(TlicgBaseTool, IlicgBlockTool)
  public
    function isEqual(Source: IlicgBlockTool): boolean; stdcall;
    procedure Copy(Source: TlicgBlockStyle); stdcall;
    procedure Assign(Source: IlicgBlockTool); stdcall;
    procedure FillStyle(var AValue: TlicgBlockStyle); stdcall;
//    function getBlockStyle: TlicgBlockStyle; virtual; stdcall; abstract;
//    procedure setBlockStyle(value: TlicgBlockStyle); virtual; stdcall; abstract;
//    property BlockStyle: TlicgBlockStyle read getBlockStyle write setBlockStyle;
    function GetIndex: Integer; virtual; stdcall; abstract;
    procedure SetIndex(Value: Integer); virtual; stdcall; abstract;
    property Index: Integer read GetIndex write SetIndex;
    function GetRotangle: Single; virtual; stdcall; abstract;
    procedure SetRotangle(const Value: Single); virtual; stdcall; abstract;
    property Rotangle: Single read GetRotangle write SetRotangle;
    function GetHeight: Double; virtual; stdcall; abstract;
    procedure SetHeight(const Value: Double); virtual; stdcall; abstract;
    property Height: Double read GetHeight write SetHeight;
  end;

function AslicgBrushTool(const int: IInterface): IlicgBrushTool;

function AslicgFontTool(const int: IInterface): IlicgFontTool;

function AslicgPenTool(const int: IInterface): IlicgPenTool;

function AslicgArrowTool(const int: IInterface): IlicgArrowTool;

function AslicgSymbolTool(const int: IInterface): IlicgSymbolTool;

function AslicgBlockTool(const int: IInterface): IlicgBlockTool;

var
  DynamicEntityNameLabelInfo: TDynamicEntityNameLabelingInfo;

implementation

uses
  Lider.CG.Com.System;

function AslicgBrushTool(const int: IInterface): IlicgBrushTool;
begin
  result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgBrushTool, result) <> 0) then
      result := nil;
  end;
end;

function AslicgFontTool(const int: IInterface): IlicgFontTool;
begin
  result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgFontTool, result) <> 0) then
      result := nil;
  end;
end;

function AslicgPenTool(const int: IInterface): IlicgPenTool;
begin
  result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgPenTool, result) <> 0) then
      result := nil;
  end;
end;

function AslicgArrowTool(const int: IInterface): IlicgArrowTool;
begin
  result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgArrowTool, result) <> 0) then
      result := nil;
  end;
end;

function AslicgSymbolTool(const int: IInterface): IlicgSymbolTool;
begin
  result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgSymbolTool, result) <> 0) then
      result := nil;
  end;
end;

function AslicgBlockTool(const int: IInterface): IlicgBlockTool;
begin
  result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgBlockTool, result) <> 0) then
      result := nil;
  end;
end;

{ TlicgBaseTool }
procedure TlicgBaseTool.Assign(Source: TObject);
begin

end;

function TlicgBaseTool.GetModified: boolean;
begin
  result := FModified;
end;

procedure TlicgBaseTool.SetModified(value: boolean);
begin
  FModified := value;
end;

{ TlicgBaseBrushTool }
procedure TlicgBaseBrushTool.Copy(Source: TlicgBrushStyle);
begin
  Pattern := Source.Pattern;
  ForeColor := Source.ForeColor;
  BackColor := Source.BackColor;
  FillFactor := Source.FillFactor;
  FillDirection := Source.FillDirection;
  GradientFill := Source.GradientFill;
  GradientDirection := Source.GradientDirection;
end;

procedure TlicgBaseBrushTool.Assign(Source: IlicgBrushTool);
begin
  Pattern := Source.Pattern;
  ForeColor := Source.ForeColor;
  BackColor := Source.BackColor;
  FillFactor := Source.FillFactor;
  FillDirection := Source.FillDirection;
  GradientFill := Source.GradientFill;
  GradientDirection := Source.GradientDirection;
end;

procedure TlicgBaseBrushTool.FillStyle(var AValue: TlicgBrushStyle);
begin
  AValue.Pattern := Pattern;
  AValue.ForeColor := ForeColor;
  AValue.BackColor := BackColor;
  AValue.FillFactor := FillFactor;
  AValue.FillDirection := FillDirection;
  AValue.GradientFill := GradientFill;
  AValue.GradientDirection := GradientDirection;
end;

{
function TlicgBaseBrushTool.GetBrushStyle: TlicgBrushStyle;
begin
  Result:=FBrushStyle;
end;

procedure TlicgBaseBrushTool.SetBrushStyle(Value: TlicgBrushStyle);
begin
  FBrushStyle:=Value;
end;
}

function TlicgBaseBrushTool.isEqual(Source: IlicgBrushTool): boolean;
begin
  Result := (self.Pattern = Source.Pattern) and (self.ForeColor
    = Source.ForeColor) and (self.BackColor = Source.BackColor)
    and (self.GradientFill = Source.GradientFill) and (self.GradientDirection
    = Source.GradientDirection) and (self.FillDirection =
    Source.FillDirection) and (self.FillFactor = Source.FillFactor);

end;

{ TlicgBaseFontTool }
procedure TlicgBaseFontTool.Copy(Source: TlicgFontStyle);
begin
  Name := Source.Name;
  Angle := Source.Angle;
  Height := Source.Height;
  Color := Source.Color;
  Style := Source.Style;
  CharSet := Source.CharSet;
  (* uður ekleme baþlama*)
  CharWidthFactor := Source.CharWidthFactor;
  CharSpacing := Source.CharSpacing;
  TextPos := Source.TextPos;
  (* uður ekleme bitme*)
end;

procedure TlicgBaseFontTool.Assign(Source: IlicgFontTool);
begin
  Name := Source.Name;
  Angle := Source.Angle;
  Height := Source.Height;
  Color := Source.Color;
  Style := Source.Style;
  CharSet := Source.CharSet;
  (* uður ekleme baþlama*)
  CharWidthFactor := Source.CharWidthFactor;
  CharSpacing := Source.CharSpacing;
  TextPos := Source.TextPos;
  (* uður ekleme bitme*)
end;

procedure TlicgBaseFontTool.FillStyle(var AValue: TlicgFontStyle); stdcall;
begin
  AValue.Name := Name;
  AValue.Angle := Angle;
  AValue.Height := Height;
  AValue.Color := Color;
  AValue.Style := Style;
  AValue.CharSet := CharSet;
  (* uður ekleme baþlama*)
  AValue.CharWidthFactor := CharWidthFactor;
  AValue.CharSpacing := CharSpacing;
  AValue.TextPos := TextPos;
  (* uður ekleme bitme*)
end;


{
function TlicgBaseFontTool.getFontStyle: TlicgFontStyle;
begin
   result := FFontStyle;
end;

procedure TlicgBaseFontTool.setFontStyle(value: TlicgFontStyle);
begin
   FFontStyle := value;
end;
}

function TlicgBaseFontTool.IsEqual(Source: IlicgFontTool): boolean;
begin
  Result := (Self.Name = Source.Name) and
    (Self.CharSet = Source.CharSet) and
    (Self.Style = Source.Style) and
    (Self.Color = Source.Color) and
    (Self.Angle = Source.Angle) and
    (Self.Height = Source.Height) and
    (Self.CharWidthFactor = Source.CharWidthFactor) and
    (Self.CharSpacing = Source.CharSpacing) and
    (Self.TextPos = Source.TextPos)
end;

{ TlicgBasePenTool }
procedure TlicgBasePenTool.Copy(Source: TlicgPenStyle);
begin
  Style := Source.Style;
  Color := Source.Color;
  Width := Source.Width;
  Flow := Source.Flow;
end;

procedure TlicgBasePenTool.Assign(Source: IlicgPenTool);
begin
  Style := Source.Style;
  Color := Source.Color;
  Width := Source.Width;
  Flow := Source.Flow;
  LineWidthDirection := Source.LineWidthDirection;
end;

procedure TlicgBasePenTool.FillStyle(var AValue: TlicgPenStyle); stdcall;
begin
  AValue.Style := Style;
  AValue.Color := Color;
  AValue.Width := Width;
  AValue.Flow := Flow;
end;

{
function TlicgBasePenTool.GetLineWidthDirection: TlicgLineWidthDirection;
begin
   result := FLineWidthDirection;
end;


function TlicgBasePenTool.getPenStyle: TlicgPenStyle;
begin
   result := FPenStyle;
end;

procedure TlicgBasePenTool.SetLineWidthDirection( value: TlicgLineWidthDirection);
begin
   FLineWidthDirection := value;
end;

procedure TlicgBasePenTool.setPenStyle(Value: TlicgPenStyle);
begin
   FPenStyle := value;
end;
}

function TlicgBasePenTool.isEqual(Source: IlicgPenTool): boolean;
begin
  Result := (self.Style = Source.Style) and (self.Color
    = Source.Color) and (self.Width = Source.Width)
    and (self.Flow = Source.Flow);
end;

{ TlicgBaseSymbolTool }
procedure TlicgBaseSymbolTool.Copy(Source: TlicgSymbolStyle);
begin
  SetSymbolFileIndex(Source.Index div 256);
  Index := Source.Index;
  Rotangle := Source.Rotangle;
  Height := Source.Height;
end;

procedure TlicgBaseSymbolTool.Assign(Source: IlicgSymbolTool);
begin
  SetSymbolFileIndex(Source.Index div 256);
  Index := Source.Index;
  Rotangle := Source.Rotangle;
  Height := Source.Height;
end;


procedure TlicgBaseSymbolTool.FillStyle(var AValue: TlicgSymbolStyle); stdcall;
begin
  AValue.Index := GetSymbolFileIndex * 256 + Index;
  AValue.Rotangle := Rotangle;
  AValue.Height := Height;
end;

{
function TlicgBaseSymbolTool.getSymbolStyle: TlicgSymbolStyle;
begin
   result := FSymbolStyle;
end;

procedure TlicgBaseSymbolTool.setSymbolStyle(value: TlicgSymbolStyle);
begin
   FSymbolStyle := value;
end;

}

function TlicgBaseSymbolTool.isEqual(Source: IlicgSymbolTool): boolean;
begin
  Result := (self.Index = source.Index) and (self.Rotangle
    = source.Rotangle) and (self.Height = source.Height);
end;

{ TlicgBaseBlockTool }

procedure TlicgBaseBlockTool.Copy(Source: TlicgBlockStyle);
begin
  Index := Source.Index;
  Rotangle := Source.Rotangle;
  Height := Source.Height;
end;

procedure TlicgBaseBlockTool.Assign(Source: IlicgBlockTool);
begin
  Index := Source.Index;
  Rotangle := Source.Rotangle;
  Height := Source.Height;
end;

procedure TlicgBaseBlockTool.FillStyle(var AValue: TlicgBlockStyle); stdcall;
begin
  AValue.Index := Index;
  AValue.Rotangle := Rotangle;
  AValue.Height := Height;
end;

function TlicgBaseBlockTool.isEqual(Source: IlicgBlockTool): boolean;
begin
  Result := (Self.Index = Source.Index) and (Self.Rotangle
    = Source.Rotangle) and (Self.Height = Source.Height);
end;

initialization
  DynamicEntityNameLabelInfo.BrushColor := clNone;
  DynamicEntityNameLabelInfo.FontHeight := 12;
  DynamicEntityNameLabelInfo.FontColor := clRed;
  DynamicEntityNameLabelInfo.LabelPos := lpdUpperUp;
  DynamicEntityNameLabelInfo.LabelOffset := 0;
  DynamicEntityNameLabelInfo.FontStyle := [];

end.


