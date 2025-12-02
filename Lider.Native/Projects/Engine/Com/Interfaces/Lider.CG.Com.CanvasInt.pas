unit Lider.CG.Com.CanvasInt;

{$P+,S-,W-,R-,T-,X+,B-}

interface

uses
  Windows,
  Graphics,
  Classes;

type
  IlicgCanvas = interface
    ['{B1047710-DF77-4A9C-AE00-7C5E5F167E4F}']
    function GetCanvas: TCanvas; stdcall;
    procedure SetCanvas(value: TCanvas); stdcall;
    function GetHandle: HDC; stdcall;
    procedure SetHandle(value: HDC); stdcall;
    function GetCanvasOrientation: TCanvasOrientation; stdcall;
    function GetPenPos: TPoint; stdcall;
    procedure SetPenPos(value: TPoint); stdcall;
    function GetPixel(X, Y: integer): Graphics.TColor; stdcall;
    procedure SetPixel(X, Y: integer; value: Graphics.TColor); stdcall;
    function GetClipRect: TRect; stdcall;
    function GetCopyMode: TCopyMode; stdcall;
    procedure SetCopyMode(value: TCopyMode); stdcall;
    procedure SetPenWidth(value: integer); stdcall;
    function GetPenWidth: integer; stdcall;
    procedure SetPenColor(value: TColor); stdcall;
    function GetPenColor: TColor; stdcall;
    procedure SetPenStyle(value: TPenStyle); stdcall;
    function GetPenStyle: TPenStyle; stdcall;
    procedure SetPenMode(value: TPenMode); stdcall;
    function GetPenMode: TPenMode; stdcall;
    function GetPenHandle: HPen; stdcall;
    procedure SetPenHandle(value: HPen); stdcall;
    procedure PenAssign(Source: TPersistent); stdcall;
    procedure SetBrushColor(value: TColor); stdcall;
    function GetBrushColor: TColor; stdcall;
    procedure SetBrushStyle(value: TBrushStyle); stdcall;
    function GetBrushStyle: TBrushStyle; stdcall;
    function GetBrushBitmap: TBitmap; stdcall;
    procedure SetBrushBitmap(value: TBitmap); stdcall;
    function GetBrushHandle: HBrush; stdcall;
    procedure SetBrushHandle(value: HBrush); stdcall;
    procedure BrushAssign(Source: TPersistent); stdcall;
    function getFontAdapter: IChangeNotifier; stdcall;
    procedure setFontAdapter(value: IChangeNotifier); stdcall;
    function GetFontHandle: HFont; stdcall;
    procedure SetFontHandle(value: HFont); stdcall;
    function getFontPixelsPerInch: Integer; stdcall;
    procedure SetFontPixelsPerInch(value: Integer); stdcall;
    function GetFontCharset: TFontCharset; stdcall;
    procedure SetFontCharset(value: TFontCharset); stdcall;
    function GetFontColor: TColor; stdcall;
    procedure SetFontColor(value: TColor); stdcall;
    function GetFontHeight: Integer; stdcall;
    procedure SetFontHeight(value: Integer); stdcall;
    function GetFontName: TFontName; stdcall;
    procedure SetFontName(value: TFontName); stdcall;
    function GetFontPitch: TFontPitch; stdcall;
    procedure SetFontPitch(value: TFontPitch); stdcall;
    function GetFontSize: Integer; stdcall;
    procedure SetFontSize(value: Integer); stdcall;
    function GetFontStyle: TFontStyles; stdcall;
    procedure SetFontStyle(value: TFontStyles); stdcall;
    procedure FontAssing(Source: TPersistent); stdcall;
    function GetLockCount: Integer; stdcall;
    function GetTextFlags: Longint; stdcall;
    procedure SetTextFlags(value: Longint); stdcall;
    function GetOnChange: TNotifyEvent; stdcall;
    procedure SetOnChange(value: TNotifyEvent); stdcall;
    function GetOnChanging: TNotifyEvent; stdcall;
    procedure SetOnChanging(value: TNotifyEvent); stdcall;
    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); stdcall;
    procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap; const Source: TRect;
      Color: TColor); stdcall;
    procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); stdcall;
    procedure CopyRect(const Dest: TRect; Canvas: TCanvas; const Source: TRect); stdcall;
    procedure Draw(X, Y: Integer; Graphic: TGraphic); stdcall;
    procedure DrawFocusRect(const Rect: TRect); stdcall;
    procedure Ellipse(X1, Y1, X2, Y2: Integer); overload; stdcall;
    procedure Ellipse(const Rect: TRect); overload; stdcall;
    procedure FillRect(const Rect: TRect); stdcall;
    procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle); stdcall;
    procedure FrameRect(const Rect: TRect); stdcall;
    function HandleAllocated: Boolean; stdcall;
    procedure LineTo(X, Y: Integer); stdcall;
    procedure Lock; stdcall;
    procedure MoveTo(X, Y: Integer); stdcall;
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); stdcall;
    procedure Polygon(const Points: array of TPoint); stdcall;
    procedure Polyline(const Points: array of TPoint); stdcall;
    procedure PolyBezier(const Points: array of TPoint); stdcall;
    procedure PolyBezierTo(const Points: array of TPoint); stdcall;
    procedure Rectangle(X1, Y1, X2, Y2: Integer); overload; stdcall;
    procedure Rectangle(const Rect: TRect); overload; stdcall;
    procedure Refresh; stdcall;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); stdcall;
    procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); stdcall;
    function TextExtent(const Text: string): TSize; stdcall;
    function TextHeight(const Text: string): Integer; stdcall;
    procedure TextOut(X, Y: Integer; const Text: string); stdcall;
    procedure TextRect(Rect: TRect; X, Y: Integer; const Text: string); stdcall;
    function TextWidth(const Text: string): Integer; stdcall;
    function TryLock: Boolean; stdcall;
    procedure Unlock; stdcall;
    property ClipRect: TRect read GetClipRect;
    property Handle: HDC read GetHandle write SetHandle;
    property LockCount: Integer read getLockCount;
    property CanvasOrientation: TCanvasOrientation read GetCanvasOrientation;
    property PenPos: TPoint read GetPenPos write SetPenPos;
    property Pixels[X, Y: Integer]: Graphics.TColor read GetPixel write SetPixel;
    property TextFlags: Longint read getTextFlags write setTextFlags;
    property OnChange: TNotifyEvent read getOnChange write setOnChange;
    property OnChanging: TNotifyEvent read getOnChanging write setOnChanging;
    property CopyMode: TCopyMode read getCopyMode write setCopyMode;
    property Canvas: TCanvas read GetCanvas write SetCanvas;
    property PenWidth: integer read GetPenWidth write SetPenWidth;
    property PenColor: TColor read GetPenColor write SetPenColor;
    property PenStyle: TPenStyle read GetPenStyle write SetPenStyle;
    property PenMode: TPenMode read GetPenMode write SetPenMode;
    property PenHandle: HPen read GetPenHandle write SetPenHandle;
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property BrushStyle: TBrushStyle read GetBrushStyle write SetBrushStyle;
    property BrushHandle: HBrush read GetBrushHandle write SetBrushHandle;
    property BrushBitmap: TBitmap read GetBrushBitmap write SetBrushBitmap;
    property FontAdapter: IChangeNotifier read getFontAdapter write setFontAdapter;
    property FontHandle: HFont read GetFontHandle write SetFontHandle;
    property FontCharset: TFontCharset read GetFontCharset write SetFontCharset;
    property FontColor: TColor read getFontColor write SetFontColor;
    property FontHeight: Integer read GetFontHeight write SetFontHeight;
    property FontName: TFontName read GetFontName write SetFontName;
    property FontPitch: TFontPitch read GetFontPitch write SetFontPitch;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property FontPixelsPerInch: Integer read getFontPixelsPerInch write setFontPixelsPerInch;
  end;

  TlicgCanvas = class(TInterfacedObject, IlicgCanvas)
  private
    FCanvas: TCanvas; // ref
  protected
    function GetCanvas: TCanvas; stdcall;
    procedure SetCanvas(value: TCanvas); stdcall;
    function GetHandle: HDC; stdcall;
    procedure SetHandle(value: HDC); stdcall;
    function GetCanvasOrientation: TCanvasOrientation; stdcall;
    function GetPenPos: TPoint; stdcall;
    procedure SetPenPos(value: TPoint); stdcall;
    function GetPixel(X, Y: integer): Graphics.TColor; stdcall;
    procedure SetPixel(X, Y: integer; value: Graphics.TColor); stdcall;
    function GetClipRect: TRect; stdcall;
    function GetCopyMode: TCopyMode; stdcall;
    procedure SetCopyMode(value: TCopyMode); stdcall;
    procedure SetPenWidth(value: integer); stdcall;
    function GetPenWidth: integer; stdcall;
    procedure SetPenColor(value: TColor); stdcall;
    function GetPenColor: TColor; stdcall;
    procedure SetPenStyle(value: TPenStyle); stdcall;
    function GetPenStyle: TPenStyle; stdcall;
    procedure SetPenMode(value: TPenMode); stdcall;
    function GetPenMode: TPenMode; stdcall;
    function GetPenHandle: HPen; stdcall;
    procedure SetPenHandle(value: HPen); stdcall;
    procedure PenAssign(Source: TPersistent); stdcall;
    procedure SetBrushColor(value: TColor); stdcall;
    function GetBrushColor: TColor; stdcall;
    procedure SetBrushStyle(value: TBrushStyle); stdcall;
    function GetBrushStyle: TBrushStyle; stdcall;
    function GetBrushBitmap: TBitmap; stdcall;
    procedure SetBrushBitmap(value: TBitmap); stdcall;
    function GetBrushHandle: HBrush; stdcall;
    procedure SetBrushHandle(value: HBrush); stdcall;
    procedure BrushAssign(Source: TPersistent); stdcall;
    function getFontAdapter: IChangeNotifier; stdcall;
    procedure setFontAdapter(value: IChangeNotifier); stdcall;
    function GetFontHandle: HFont; stdcall;
    procedure SetFontHandle(value: HFont); stdcall;
    function getFontPixelsPerInch: Integer; stdcall;
    procedure SetFontPixelsPerInch(value: Integer); stdcall;
    function GetFontCharset: TFontCharset; stdcall;
    procedure SetFontCharset(value: TFontCharset); stdcall;
    function GetFontColor: TColor; stdcall;
    procedure SetFontColor(value: TColor); stdcall;
    function GetFontHeight: Integer; stdcall;
    procedure SetFontHeight(value: Integer); stdcall;
    function GetFontName: TFontName; stdcall;
    procedure SetFontName(value: TFontName); stdcall;
    function GetFontPitch: TFontPitch; stdcall;
    procedure SetFontPitch(value: TFontPitch); stdcall;
    function GetFontSize: Integer; stdcall;
    procedure SetFontSize(value: Integer); stdcall;
    function GetFontStyle: TFontStyles; stdcall;
    procedure SetFontStyle(value: TFontStyles); stdcall;
    procedure FontAssing(Source: TPersistent); stdcall;
    function GetLockCount: Integer; stdcall;
    function GetTextFlags: Longint; stdcall;
    procedure SetTextFlags(value: Longint); stdcall;
    function GetOnChange: TNotifyEvent; stdcall;
    procedure SetOnChange(value: TNotifyEvent); stdcall;
    function GetOnChanging: TNotifyEvent; stdcall;
    procedure SetOnChanging(value: TNotifyEvent); stdcall;
  public
    constructor Create(_Canvas: TCanvas);
    destructor Destroy; override;
    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); stdcall;
    procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap; const Source: TRect;
      Color: TColor); stdcall;
    procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); stdcall;
    procedure CopyRect(const Dest: TRect; Canvas: TCanvas; const Source: TRect); stdcall;
    procedure Draw(X, Y: Integer; Graphic: TGraphic); stdcall;
    procedure DrawFocusRect(const Rect: TRect); stdcall;
    procedure Ellipse(X1, Y1, X2, Y2: Integer); overload; stdcall;
    procedure Ellipse(const Rect: TRect); overload; stdcall;
    procedure FillRect(const Rect: TRect); stdcall;
    procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle); stdcall;
    procedure FrameRect(const Rect: TRect); stdcall;
    function HandleAllocated: Boolean; stdcall;
    procedure LineTo(X, Y: Integer); stdcall;
    procedure Lock; stdcall;
    procedure MoveTo(X, Y: Integer); stdcall;
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); stdcall;
    procedure Polygon(const Points: array of TPoint); stdcall;
    procedure Polyline(const Points: array of TPoint); stdcall;
    procedure PolyBezier(const Points: array of TPoint); stdcall;
    procedure PolyBezierTo(const Points: array of TPoint); stdcall;
    procedure Rectangle(X1, Y1, X2, Y2: Integer); overload; stdcall;
    procedure Rectangle(const Rect: TRect); overload; stdcall;
    procedure Refresh; stdcall;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); stdcall;
    procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); stdcall;
    function TextExtent(const Text: string): TSize; stdcall;
    function TextHeight(const Text: string): Integer; stdcall;
    procedure TextOut(X, Y: Integer; const Text: string); stdcall;
    procedure TextRect(Rect: TRect; X, Y: Integer; const Text: string); stdcall;
    function TextWidth(const Text: string): Integer; stdcall;
    function TryLock: Boolean; stdcall;
    procedure Unlock; stdcall;
    property Canvas: TCanvas read GetCanvas write SetCanvas;
  end;

function GetIlicgCanvas(_Canvas: TCanvas): IlicgCanvas; stdcall;

implementation

uses
  VarCmplx;

function GetIlicgCanvas(_Canvas: TCanvas): IlicgCanvas; stdcall;
begin
  Result := TlicgCanvas.Create(_Canvas);
end;

{ TlicgCanvas }

procedure TlicgCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  FCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TlicgCanvas.BrushAssign(Source: TPersistent);
begin
  Fcanvas.Brush.Assign(Source);
end;

procedure TlicgCanvas.BrushCopy(const Dest: TRect; Bitmap: TBitmap; const Source:
  TRect; Color: TColor);
begin
  FCanvas.BrushCopy(Dest, Bitmap, Source, Color);
end;

procedure TlicgCanvas.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  FCanvas.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TlicgCanvas.CopyRect(const Dest: TRect; Canvas: TCanvas; const Source: TRect);
begin
  FCanvas.CopyRect(Dest, Canvas, Source);
end;

constructor TlicgCanvas.Create(_Canvas: TCanvas);
begin
  inherited create;
  FCanvas := _Canvas;
end;

destructor TlicgCanvas.Destroy;
begin
  inherited;
end;

procedure TlicgCanvas.Draw(X, Y: Integer; Graphic: TGraphic);
begin
  FCanvas.Draw(X, Y, Graphic);
end;

procedure TlicgCanvas.DrawFocusRect(const Rect: TRect);
begin
  FCanvas.DrawFocusRect(Rect);
end;

procedure TlicgCanvas.Ellipse(const Rect: TRect);
begin
  FCanvas.Ellipse(Rect);
end;

procedure TlicgCanvas.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  FCanvas.Ellipse(X1, Y1, X2, Y2);
end;

procedure TlicgCanvas.FillRect(const Rect: TRect);
begin
  FCanvas.FillRect(Rect);
end;

procedure TlicgCanvas.FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle);
begin
  FCanvas.FloodFill(X, Y, Color, FillStyle);
end;

procedure TlicgCanvas.FontAssing(Source: TPersistent);
begin
  FCanvas.Font.assign(Source);
end;

procedure TlicgCanvas.FrameRect(const Rect: TRect);
begin
  FCanvas.FrameRect(Rect);
end;

function TlicgCanvas.GetBrushBitmap: TBitmap;
begin
  result := FCanvas.Brush.Bitmap;

end;

function TlicgCanvas.GetBrushColor: TColor;
begin
  Result := FCanvas.Brush.Color;
end;

function TlicgCanvas.GetBrushHandle: HBrush;
begin
  Result := FCanvas.Brush.Handle;
end;

function TlicgCanvas.GetBrushStyle: TBrushStyle;
begin
  Result := FCanvas.Brush.Style;
end;

function TlicgCanvas.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TlicgCanvas.GetCanvasOrientation: TCanvasOrientation;
begin
  Result := FCanvas.CanvasOrientation;
end;

function TlicgCanvas.GetClipRect: TRect;
begin
  Result := FCanvas.ClipRect;
end;

function TlicgCanvas.GetCopyMode: TCopyMode;
begin
  Result := FCanvas.CopyMode;
end;

function TlicgCanvas.getFontAdapter: IChangeNotifier;
begin
  Result := FCanvas.Font.FontAdapter;

end;

function TlicgCanvas.GetFontCharset: TFontCharset;
begin
  Result := FCanvas.Font.Charset;
end;

function TlicgCanvas.GetFontColor: TColor;
begin
  Result := FCanvas.Font.Color;
end;

function TlicgCanvas.GetFontHandle: HFont;
begin
  Result := FCanvas.Font.Handle;
end;

function TlicgCanvas.GetFontHeight: Integer;
begin
  Result := FCanvas.Font.Height;
end;

function TlicgCanvas.GetFontName: TFontName;
begin
  Result := FCanvas.Font.Name;
end;

function TlicgCanvas.GetFontPitch: TFontPitch;
begin
  Result := FCanvas.Font.Pitch;
end;

function TlicgCanvas.getFontPixelsPerInch: Integer;
begin
  Result := FCanvas.Font.PixelsPerInch;
end;

function TlicgCanvas.GetFontSize: Integer;
begin
  Result := FCanvas.Font.Size;
end;

function TlicgCanvas.GetFontStyle: TFontStyles;
begin
  Result := FCanvas.Font.Style;
end;

function TlicgCanvas.GetHandle: HDC;
begin
  Result := FCanvas.Handle;
end;

function TlicgCanvas.GetLockCount: Integer;
begin
  Result := FCanvas.LockCount;
end;

function TlicgCanvas.GetOnChange: TNotifyEvent;
begin
  Result := FCanvas.OnChange;
end;

function TlicgCanvas.GetOnChanging: TNotifyEvent;
begin
  Result := FCanvas.OnChanging;
end;

function TlicgCanvas.GetPenColor: TColor;
begin
  Result := FCanvas.Pen.Color;
end;

function TlicgCanvas.GetPenHandle: HPen;
begin
  Result := FCanvas.Pen.Handle;
end;

function TlicgCanvas.GetPenMode: TPenMode;
begin
  Result := FCanvas.Pen.Mode;
end;

function TlicgCanvas.GetPenPos: TPoint;
begin
  Result := FCanvas.PenPos;
end;

function TlicgCanvas.GetPenStyle: TPenStyle;
begin
  Result := FCanvas.Pen.Style;
end;

function TlicgCanvas.GetPenWidth: integer;
begin
  Result := FCanvas.Pen.Width;
end;

function TlicgCanvas.GetPixel(X, Y: integer): Graphics.TColor;
begin
  Result := FCanvas.Pixels[X, Y];
end;

function TlicgCanvas.GetTextFlags: Longint;
begin
  Result := FCanvas.TextFlags;
end;

function TlicgCanvas.HandleAllocated: Boolean;
begin
  Result := FCanvas.HandleAllocated;
end;

procedure TlicgCanvas.LineTo(X, Y: Integer);
begin
  FCanvas.LineTo(X, Y);
end;

procedure TlicgCanvas.Lock;
begin
  FCanvas.Lock;
end;

procedure TlicgCanvas.MoveTo(X, Y: Integer);
begin
  FCanvas.MoveTo(X, Y);
end;

procedure TlicgCanvas.PenAssign(Source: TPersistent);
begin
  FCanvas.Pen.Assign(Source);
end;

procedure TlicgCanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  FCanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

procedure TlicgCanvas.PolyBezier(const Points: array of TPoint);
begin
  FCanvas.PolyBezier(Points);
end;

procedure TlicgCanvas.PolyBezierTo(const Points: array of TPoint);
begin
  FCanvas.PolyBezierTo(Points);
end;

procedure TlicgCanvas.Polygon(const Points: array of TPoint);
begin
  FCanvas.Polygon(Points);
end;

procedure TlicgCanvas.Polyline(const Points: array of TPoint);
begin
  FCanvas.Polyline(Points);
end;

procedure TlicgCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  FCanvas.Rectangle(X1, Y1, X2, Y2);
end;

procedure TlicgCanvas.Rectangle(const Rect: TRect);
begin
  FCanvas.Rectangle(Rect);
end;

procedure TlicgCanvas.Refresh;
begin
  FCanvas.Refresh;
end;

procedure TlicgCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
begin
  FCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3);
end;

procedure TlicgCanvas.SetBrushBitmap(value: TBitmap);
begin
  FCanvas.Brush.Bitmap := value;
end;

procedure TlicgCanvas.SetBrushColor(value: TColor);
begin
  FCanvas.Brush.Color := value;
end;

procedure TlicgCanvas.SetBrushHandle(value: HBrush);
begin
  FCanvas.Brush.Handle := value;
end;

procedure TlicgCanvas.SetBrushStyle(value: TBrushStyle);
begin
  FCanvas.Brush.Style := value;
end;

procedure TlicgCanvas.SetCanvas(value: TCanvas);
begin
  FCanvas := value;
end;

procedure TlicgCanvas.SetCopyMode(value: TCopyMode);
begin
  FCanvas.CopyMode := value;
end;

procedure TlicgCanvas.setFontAdapter(value: IChangeNotifier);
begin
  FCanvas.Font.FontAdapter := value;
end;

procedure TlicgCanvas.SetFontCharset(value: TFontCharset);
begin
  FCanvas.Font.Charset := value;
end;

procedure TlicgCanvas.SetFontColor(value: TColor);
begin
  FCanvas.Font.Color := value;
end;

procedure TlicgCanvas.SetFontHandle(value: HFont);
begin
  FCanvas.Font.Handle := value;
end;

procedure TlicgCanvas.SetFontHeight(value: Integer);
begin
  FCanvas.Font.Height := value;
end;

procedure TlicgCanvas.SetFontName(value: TFontName);
begin
  FCanvas.Font.Name := value;
end;

procedure TlicgCanvas.SetFontPitch(value: TFontPitch);
begin
  FCanvas.Font.Pitch := value;
end;

procedure TlicgCanvas.SetFontPixelsPerInch(value: Integer);
begin
  FCanvas.Font.PixelsPerInch := value;
end;

procedure TlicgCanvas.SetFontSize(value: Integer);
begin
  FCanvas.Font.Size := value;
end;

procedure TlicgCanvas.SetFontStyle(value: TFontStyles);
begin
  FCanvas.Font.Style := value;
end;

procedure TlicgCanvas.SetHandle(value: HDC);
begin
  FCanvas.Handle := value;
end;

procedure TlicgCanvas.SetOnChange(value: TNotifyEvent);
begin
  FCanvas.OnChange := value;
end;

procedure TlicgCanvas.SetOnChanging(value: TNotifyEvent);
begin
  FCanvas.OnChanging := value;
end;

procedure TlicgCanvas.SetPenColor(value: TColor);
begin
  FCanvas.Pen.Color := value;
end;

procedure TlicgCanvas.SetPenHandle(value: HPen);
begin
  FCanvas.Pen.Handle := value;
end;

procedure TlicgCanvas.SetPenMode(value: TPenMode);
begin
  FCanvas.Pen.Mode := value;
end;

procedure TlicgCanvas.SetPenPos(value: TPoint);
begin
  FCanvas.PenPos := value;
end;

procedure TlicgCanvas.SetPenStyle(value: TPenStyle);
begin
  FCanvas.Pen.Style := value;
end;

procedure TlicgCanvas.SetPenWidth(value: integer);
begin
  FCanvas.Pen.Width := value;
end;

procedure TlicgCanvas.SetPixel(X, Y: integer; value: TColor);
begin
  FCanvas.Pixels[X, Y] := value;
end;

procedure TlicgCanvas.SetTextFlags(value: Integer);
begin
  FCanvas.TextFlags := value;
end;

procedure TlicgCanvas.StretchDraw(const Rect: TRect; Graphic: TGraphic);
begin
  FCanvas.StretchDraw(Rect, Graphic);
end;

function TlicgCanvas.TextExtent(const Text: string): TSize;
begin
  Result := FCanvas.TextExtent(Text);
end;

function TlicgCanvas.TextHeight(const Text: string): Integer;
begin
  Result := FCanvas.TextHeight(Text);
end;

procedure TlicgCanvas.TextOut(X, Y: Integer; const Text: string);
begin
  FCanvas.TextOut(X, Y, Text);
end;

procedure TlicgCanvas.TextRect(Rect: TRect; X, Y: Integer; const Text: string);
begin
  FCanvas.TextRect(Rect, X, Y, Text);
end;

function TlicgCanvas.TextWidth(const Text: string): Integer;
begin
  Result := FCanvas.TextWidth(Text);
end;

function TlicgCanvas.TryLock: Boolean;
begin
  Result := FCanvas.TryLock;
end;

procedure TlicgCanvas.Unlock;
begin
  FCanvas.Unlock;
end;

end.


