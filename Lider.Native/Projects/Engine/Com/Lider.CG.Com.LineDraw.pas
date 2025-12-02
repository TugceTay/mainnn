unit Lider.CG.Com.LineDraw;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Lider.CG.Com.Lib,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.CanvasInt;

type

  // A list of word numbers
  TlicgBaseWordList = class
  protected
    function Get(Index: Integer): Word; virtual; abstract;
    procedure Grow; virtual; abstract;
    procedure Put(Index: Integer; Item: Word); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual; abstract;
    procedure SetCount(NewCount: Integer); virtual; abstract;
    function GetCapacity: Integer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
  public
    destructor Destroy; override;
    function Add(Item: Word): Integer; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Exchange(Index1, Index2: Integer); virtual; abstract;
    procedure Insert(Index: Integer; Item: Word); virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: Word read Get write Put; default;
  end;

  TlicgBaseDashPattern = class
  protected
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    function GetDashes: TlicgBaseWordList; virtual; abstract;
    function GetIsSolid: Boolean; virtual; abstract;
    function GetOffset: integer; virtual; abstract;
    procedure SetIsSolid(const Value: Boolean); virtual; abstract;
    procedure SetOffset(const Value: integer); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    property Dashes: TlicgBaseWordList read GetDashes;
    property Offset: integer read GetOffset write SetOffset;
    property IsSolid: Boolean read GetIsSolid write SetIsSolid;
  end;

  TlicgBaseRasterLineType = class
    function Get(Index: integer): TlicgBaseDashPattern; virtual; abstract;
    procedure CalcMaxLen; virtual; abstract;
    function ScaleBy(Factor: Integer): TlicgBaseRasterLineType; virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    function GetName: string; virtual; abstract;
    procedure SetName(const Value: string); virtual; abstract;
    constructor Create(const Name: string = '');
    destructor Destroy; override;
    function Add(Offset: integer = 0): TlicgBaseDashPattern; virtual; abstract;
    procedure Delete(Index: integer); virtual; abstract;
    procedure Clear; virtual; abstract;
    function Count: integer; virtual; abstract;
    procedure BuildFromBitmap(Bitmap: TBitmap); virtual; abstract;
    property Name: string read GetName write SetName;
    property Items[Index: integer]: TlicgBaseDashPattern read Get;
  end;

  TlicgBaseRasterLineTypes = class
  protected
    function Get(Index: integer): TlicgBaseRasterLineType; virtual; abstract;
    function GetFileName: string; virtual; abstract;
    procedure SetFileName(const Value: string); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName: string = ''): TlicgBaseRasterLineType; virtual; abstract;
    procedure Delete(Index: integer); virtual; abstract;
    procedure Clear; virtual; abstract;
    function Count: integer; virtual; abstract;
    procedure Open; virtual; abstract;
    procedure Save; virtual; abstract;
    procedure AddFromBitmap(Bitmap: TBitmap; const AName: string = ''); virtual; abstract;
    property FileName: string read GetFileName write SetFileName;
    property Items[Index: integer]: TlicgBaseRasterLineType read Get; default;
  end;


  // A list of word numbers
  TlicgWordList = class(TlicgBaseWordList)
  private
    FList: array of Word;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Word; override;
    procedure Grow; override;
    procedure Put(Index: Integer; Item: Word); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetCount(NewCount: Integer); override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
  public
    destructor Destroy; override;
    function Add(Item: Word): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; Item: Word); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TlicgDashPattern = class(TlicgBaseDashPattern)
  private
    FDashes: TlicgWordList;
    FOffset: integer;
    FIsSolid: Boolean;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetDashes: TlicgBaseWordList; override;
    function GetIsSolid: Boolean; override;
    function GetOffset: integer; override;
    procedure SetIsSolid(const Value: Boolean); override;
    procedure SetOffset(const Value: integer); override;
    constructor Create;
    destructor Destroy; override;
  end;

  TlicgRasterLineType = class(TlicgBaseRasterLineType)
  private
    FList: TList;
    FName: string;
    { non-data}
    FMaxLen: Integer; // calculated
  public
    function Get(Index: integer): TlicgBaseDashPattern; override;
    function ScaleBy(Factor: Integer): TlicgBaseRasterLineType; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetName: string; override;
    procedure SetName(const Value: string); override;
    constructor Create(const Name: string = '');
    destructor Destroy; override;
    procedure CalcMaxLen; override;
    function Add(Offset: integer = 0): TlicgBaseDashPattern; override;
    procedure Delete(Index: integer); override;
    procedure Clear; override;
    function Count: integer; override;
    procedure BuildFromBitmap(Bitmap: TBitmap); override;
    property MaxLen: Integer read FMaxLen write FMaxLen;
    property List: TList read FList;
  end;

  TlicgRasterLineTypes = class(TlicgBaseRasterLineTypes)
  private
    FList: TList;
    FFileName: string;
  protected
    function Get(Index: integer): TlicgBaseRasterLineType; override;
    function GetFileName: string; override;
    procedure SetFileName(const Value: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName: string = ''): TlicgBaseRasterLineType; override;
    procedure Delete(Index: integer); override;
    procedure Clear; override;
    function Count: integer; override;
    procedure Open; override;
    procedure Save; override;
    procedure AddFromBitmap(Bitmap: TBitmap; const AName: string = ''); override;
    property List: TList read FList;
  end;

procedure Draw_RasterLineType(RasterLineType: TlicgBaseRasterLineType; var
  PointArray: array of TPoint; const Parts: array of integer; PartCount: integer;
  Canvas: IlicgCanvas; Grapher: TlicgBaseGrapher; LineColor: TColor; LineWidth:
  integer; Flow: Boolean);

procedure Draw_RasterLineTypes(RasterLineTypes: TlicgBaseRasterLineTypes; Index:
  Integer; var PointArray: array of TPoint; const Parts: array of integer;
  PartCount: integer; Canvas: IlicgCanvas; Grapher: TlicgBaseGrapher; LineColor:
  TColor; LineWidth: integer; Flow: Boolean);

implementation

uses
  Math,
  Lider.CG.Com.Consts,
  Lider.CG.Com.System;

{TlicgDashPattern}

constructor TlicgDashPattern.Create;
begin
  inherited Create;
  FDashes := TlicgWordList.Create;
end;

destructor TlicgDashPattern.Destroy;
begin
  FDashes.Free;
  inherited;
end;

function TlicgDashPattern.GetDashes: TlicgBaseWordList;
begin
  Result := FDashes
end;

function TlicgDashPattern.GetIsSolid: Boolean;
begin
  Result := FIsSolid
end;

function TlicgDashPattern.GetOffset: integer;
begin
  Result := FOffset
end;

procedure TlicgDashPattern.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FIsSolid, Sizeof(FIsSolid));
  Stream.Read(FOffset, Sizeof(FOffset));
  FDashes.LoadFromStream(Stream);
end;

procedure TlicgDashPattern.SaveToStream(Stream: TStream);
begin
  Stream.Write(FIsSolid, Sizeof(FIsSolid));
  Stream.Write(FOffset, Sizeof(FOffset));
  FDashes.SaveToStream(Stream);
end;

procedure TlicgDashPattern.SetIsSolid(const Value: Boolean);
begin
  FIsSolid := Value
end;

procedure TlicgDashPattern.SetOffset(const Value: integer);
begin
  FOffset := Value
end;

{ TlicgRasterLineType }

constructor TlicgRasterLineType.Create(const Name: string = '');
begin
  inherited Create;
  FName := Name;
  FList := TList.Create;
end;

destructor TlicgRasterLineType.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TlicgRasterLineType.Delete(Index: integer);
begin
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  TlicgDashPattern(FList[Index]).Free;
  FList.Delete(Index);
  FMaxLen := 0;
end;

function TlicgRasterLineType.Add(Offset: integer = 0): TlicgBaseDashPattern;
begin
  Result := TlicgDashPattern.Create;
  Result.Offset := Offset;
  FList.Add(Result);
  FMaxLen := 0;
end;

procedure TlicgRasterLineType.Clear;
var
  I: integer;
begin
  for I := 0 to FList.Count - 1 do
    TlicgDashPattern(FList[I]).Free;
  FList.Clear;
  FMaxLen := 0;
end;

function TlicgRasterLineType.Count: integer;
begin
  Result := FList.Count - 1;
end;

function TlicgRasterLineType.Get(Index: integer): TlicgBaseDashPattern;
begin
  Result := nil;
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  Result := FList[Index];
end;

procedure TlicgRasterLineType.LoadFromStream(Stream: TStream);
var
  I, n: integer;
  temp: TlicgDashPattern;
begin
  Clear;
  with Stream do
  begin
    FName := licgReadStrFromStream(Stream);
    Read(n, Sizeof(n));
    for I := 1 to n do
    begin
      temp := TlicgDashPattern.Create;
      temp.LoadFromStream(Stream);
      FList.Add(temp);
    end;
  end;
  FMaxLen := 0;
end;

procedure TlicgRasterLineType.SaveToStream(Stream: TStream);
var
  I, n: integer;
  temp: TlicgDashPattern;
begin
  with Stream do
  begin
    licgWriteStrToStream(FName, Stream);
    n := FList.Count;
    Write(n, Sizeof(n));
    for I := 0 to n - 1 do
    begin
      temp := TlicgDashPattern(FList[I]);
      temp.SaveToStream(Stream);
    end;
  end;
end;

procedure TlicgRasterLineType.CalcMaxLen;
var
  I, p, NumSolids, PixelCountX: Integer;
begin
  FMaxLen := 0;
  NumSolids := 0;
  for I := 0 to FList.Count - 1 do
    with TlicgDashPattern(FList[I]) do
    begin
      if FIsSolid or (FDashes.Count = 1) then
        Inc(NumSolids)
      else
      begin
        PixelCountX := 0;
        for p := 0 to FDashes.Count - 1 do
          Inc(PixelCountX, FDashes[p]);
        FMaxLen := Max(PixelCountX, FMaxLen);
      end;
    end;
  if NumSolids = FList.Count then
    FMaxLen := 1; // solo para evitar que no se dibuje ya que la longitud no se usa
end;

function TlicgRasterLineType.ScaleBy(Factor: Integer): TlicgBaseRasterLineType;
var
  I, J, P, MaxOffset, MinOffset, PixelCountX, Start: Integer;
  Bitmap, ScaledBitmap: TBitmap;
  AValue: Boolean;
  DashPattern: TlicgDashPattern;
begin
  Result := nil;
  if FMaxLen = 0 then
  begin
    CalcMaxLen;
    if FMaxLen = 0 then
      Exit;
  end;
  MinOffset := MaxInt;
  MaxOffset := -MaxInt;
  for I := 0 to FList.Count - 1 do
    with TlicgDashPattern(FList[I]) do
    begin
      MaxOffset := Max(MaxOffset, FOffset);
      MinOffset := Min(MinOffset, FOffset);
    end;
  Bitmap := TBitmap.Create;
  ScaledBitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf1bit;
    Bitmap.Width := FMaxLen;
    Bitmap.Height := Succ(MaxOffset - MinOffset);
    Start := Bitmap.Height div 2;
    if (Bitmap.Height mod 2) = 0 then
      Dec(Start);
    // now create the pixels
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1));
    for I := 0 to FList.Count - 1 do
    begin
      DashPattern := TlicgDashPattern(FList[I]);
      with DashPattern do
      begin
        AValue := True;
        PixelCountX := 0;
        if Bitmap.Width > 1 then
        begin
          for P := 0 to FDashes.Count - 1 do
          begin
            if AValue then
              for J := PixelCountX to (PixelCountX + FDashes[P] - 1) do
                Bitmap.Canvas.Pixels[J, Start - FOffset] := clBlack;

            Inc(PixelCountX, FDashes[P]);
            AValue := not AValue;
          end;
        end
        else
          Bitmap.Canvas.Pixels[0, Start - FOffset] := clBlack;
      end;
    end;
    ScaledBitmap.PixelFormat := pf1bit;
    ScaledBitmap.Width := Bitmap.Width * Factor;
    ScaledBitmap.Height := Bitmap.Height * Factor;
    ScaledBitmap.Canvas.StretchDraw(Rect(0, 0, ScaledBitmap.Width - 1,
      ScaledBitmap.Height - 1), Bitmap);

    Result := TlicgRasterLineType.Create;
    Result.BuildFromBitmap(ScaledBitmap);
  finally
    Bitmap.Free;
    ScaledBitmap.Free;
  end;
end;

procedure TlicgRasterLineType.BuildFromBitmap(Bitmap: TBitmap);
var
  Start: integer;
  Dash: TlicgBaseDashPattern;
  n, K, Y, X: Integer;
  AValue, CurrValue: Boolean;
begin
  Clear;
  Start := Bitmap.Height div 2;
  if (Bitmap.Height mod 2) = 0 then
    Dec(Start);
  for Y := 0 to Bitmap.Height - 1 do
  begin
    // verifica si este tiene elementos
    n := 0;
    for X := 0 to Bitmap.Width - 1 do
      if (Bitmap.Canvas.Pixels[X, Y] <> clWhite) then
        Inc(n);

    if n > 0 then
    begin
      Dash := Self.Add(Start - Y);
      if n = Bitmap.Width then
      begin
        // a solid line
        Dash.IsSolid := True;
      end
      else
      begin
        AValue := True; // expected value
        K := 0;
        for X := 0 to Bitmap.Width - 1 do
        begin
          CurrValue := (Bitmap.Canvas.Pixels[X, Y] <> clWhite);
          if CurrValue = AValue then
            Inc(K)
          else
          begin
            Dash.Dashes.Add(K);
            K := 1;
            AValue := not AValue;
          end;
        end;
        if K > 0 then
          Dash.Dashes.Add(K);
      end;
    end;
  end;
end;

function TlicgRasterLineType.GetName: string;
begin
  Result := FName
end;

procedure TlicgRasterLineType.SetName(const Value: string);
begin
  FName := Value
end;

{ TlicgRasterLineTypes }

constructor TlicgRasterLineTypes.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TlicgRasterLineTypes.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TlicgRasterLineTypes.Add(const AName: string = ''): TlicgBaseRasterLineType;
begin
  if Length(AName) = 0 then
    Result := TlicgRasterLineType.Create(SDashPatt + IntToStr(FList.Count))
  else
    Result := TlicgRasterLineType.Create(AName);
  FList.Add(Result);
end;

procedure TlicgRasterLineTypes.Clear;
var
  I: integer;
begin
  for I := 0 to FList.Count - 1 do
    TlicgRasterLineType(FList[I]).Free;
  FList.Clear;
end;

function TlicgRasterLineTypes.Count: integer;
begin
  Result := FList.Count;
end;

procedure TlicgRasterLineTypes.Delete(Index: integer);
begin
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  TlicgRasterLineType(FList[Index]).Free;
  FList.Delete(Index);
end;

function TlicgRasterLineTypes.Get(Index: integer): TlicgBaseRasterLineType;
begin
  Result := nil;
  if (Index < 0) or (Index > FList.Count - 1) then
    Exit;
  Result := FList[Index];
end;

const
  RLT_ID = 8258;
  RLT_VERSIONNUMBER = 200;

type
  TlicgRLTHeader = packed record
    HeaderID: smallint;
    VersionNumber: SmallInt;
    RecordCount: integer;
  end;

procedure TlicgRasterLineTypes.Open;
var
  Stream: TStream;
  Header: TlicgRLTHeader;
  LineType: TlicgRasterLineType;
  I: integer;
begin
  if not FileExists(FFIleName) then
    Exit;
  Clear;
  Stream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    with Stream do
    begin
      Read(Header, SizeOf(TlicgRLTHeader));
      if Header.HeaderID <> RLT_ID then
        LicadGISError(SInvalidSymbolFile);
      if Header.VersionNumber <> RLT_VERSIONNUMBER then
        LicadGISError(SInvalidSymbolVersion);
      for I := 0 to Header.RecordCount - 1 do
      begin
        LineType := TlicgRasterLineType.Create;
        LineType.LoadFromStream(Stream);
        FList.Add(LineType);
      end;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TlicgRasterLineTypes.Save;
var
  Stream: TStream;
  Header: TlicgRLTHeader;
  I: integer;
begin
  if Length(FFileName) = 0 then
    Exit;
  Stream := TFileStream.Create(FFileName, fmCreate);
  try
    with Stream do
    begin
      Header.RecordCount := FList.Count;
      Header.VersionNumber := RLT_VERSIONNUMBER;
      Header.HeaderID := RLT_ID;
      Write(Header, Sizeof(Header));
      for I := 0 to FList.Count - 1 do
        TlicgRasterLineType(FList[I]).SaveToStream(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TlicgRasterLineTypes.AddFromBitmap(Bitmap: TBitmap; const AName: string = '');
begin
  with Self.Add(AName) do
    BuildFromBitmap(Bitmap);
end;




{ TlicgWordList class implementation }

destructor TlicgWordList.Destroy;
begin
  Clear;
  Finalize(FList);
  inherited Destroy;
end;

function TlicgWordList.GetCapacity: Integer;
begin
  Result := FCapacity
end;

function TlicgWordList.GetCount: Integer;
begin
  Result := FCount
end;

function TlicgWordList.Get(Index: Integer): Word;
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Result := FList[Index];
end;

procedure TlicgWordList.Grow;
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

procedure TlicgWordList.Put(Index: Integer; Item: Word);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  FList[Index] := Item;
end;

procedure TlicgWordList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    LicadGISError(SWordListError);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Word), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure TlicgWordList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    LicadGISError(SWordListError);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

function TlicgWordList.Add(Item: Word): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

procedure TlicgWordList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TlicgWordList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(Word));
end;

procedure TlicgWordList.Exchange(Index1, Index2: Integer);
var
  Item: Word;
begin
  if (Index1 < 0) or (Index1 >= FCount) or (Index2 < 0) or (Index2 >= FCount) then
    LicadGISError(SWordListError);
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

procedure TlicgWordList.Insert(Index: Integer; Item: Word);
begin
  if (Index < 0) or (Index > FCount) then
    LicadGISError(SWordListError);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(Word));
  FList[Index] := Item;
  Inc(FCount);
end;

procedure TlicgWordList.LoadFromStream(Stream: TStream);
var
  Value: Word;
  I, N: Integer;
begin
  Clear;
  with Stream do
  begin
    Read(N, Sizeof(N));
    SetCapacity(N);
    for I := 1 to N do
    begin
      Read(Value, Sizeof(Value));
      Add(Value);
    end;
  end;
end;

procedure TlicgWordList.SaveToStream(Stream: TStream);
var
  I, N: Integer;
  Value: Word;
begin
  N := FCount;
  with Stream do
  begin
    Write(N, Sizeof(N));
    for I := 0 to FCount - 1 do
    begin
      Value := FList[I];
      Write(Value, Sizeof(Value));
    end;
  end;
end;

{ ellipse use bresenham algorithm }

{ Remark : Only one quarter of the ellipse is computated, the rest
         is set by symmetry.

         a  and  b  are the half-axes (I hope this is the right
         word for it) of the ellipse in  x- and y-direction.
         If you want a circle set  a=b=radius  . }

procedure bresenham_ellipse(ACanvas: IlicgCanvas; Clr: TColor; cx, cy, a, b: integer);

  procedure symmetry(x, y: integer);
  begin
    ACanvas.Pixels[cx + x, cy + y] := Clr;
    ACanvas.Pixels[cx - x, cy + y];
    ACanvas.Pixels[cx - x, cy - y];
    ACanvas.Pixels[cx + x, cy - y];
  end;

var
  x, y, a2, b2, S, T: integer;
begin
  a2 := a * a;
  b2 := b * b;
  x := 0;
  y := b;
  S := a2 * (1 - 2 * b) + 2 * b2;
  T := b2 - 2 * a2 * (2 * b - 1);
  symmetry(x, y);
  repeat
    if S < 0 then
    begin
      S := S + 2 * b2 * (2 * x + 3);
      T := T + 4 * b2 * (x + 1);
      Inc(x);
    end
    else if T < 0 then
    begin
      S := S + 2 * b2 * (2 * x + 3) - 4 * a2 * (y - 1);
      T := T + 4 * b2 * (x + 1) - 2 * a2 * (2 * y - 3);
      Inc(x);
      Dec(y);
    end
    else
    begin
      S := S - 4 * a2 * (y - 1);
      T := T - 2 * a2 * (2 * y - 3);
      Dec(y);
    end;
    symmetry(x, y);
  until y <= 0;
end;

function TlicgRasterLineTypes.GetFileName: string;
begin
  Result := FFileName
end;

procedure TlicgRasterLineTypes.SetFileName(const Value: string);
begin
  FFileName := Value
end;

procedure Draw_RasterLineType(RasterLineType: TlicgBaseRasterLineType; var
  PointArray: array of TPoint; const Parts: array of integer; PartCount: integer;
  Canvas: IlicgCanvas; Grapher: TlicgBaseGrapher; LineColor: TColor; LineWidth:
  integer; Flow: Boolean);
var
  PixelOffsetX, PixelCountX: Integer;
  PixelSequenceX: array of Boolean;

  procedure PixelDraw(X, Y: Integer);
  begin
    if PixelSequenceX[PixelOffsetX] then
    begin
      if LineWidth = 1 then
        SetPixelV(Canvas.Handle, X, Y, LineColor)
      else
      begin
        Canvas.MoveTo(X, Y);
        Canvas.LineTo(X, Y);
      end;
    end;
    Inc(PixelOffsetX);
    if PixelOffsetX > PixelCountX - 1 then
      PixelOffsetX := 0;
  end;

  // Bresenham's Line Algorithm.  Byte, March 1988, pp. 249-253.
  procedure DrawLine(xStart, yStart, xEnd, yEnd: integer);
  var
    a, b: integer; {displacements in x and y}
    d: integer; {decision variable}
    diag_inc: integer; {d's increment for diagonal steps}
    dx_diag: integer; {diagonal x step for next pixel}
    dx_nondiag: integer; {nondiagonal x step for next pixel}
    dy_diag: integer; {diagonal y step for next pixel}
    dy_nondiag: integer; {nondiagonal y step for next pixel}
    i: integer; {loop index}
    nondiag_inc: integer; {d's increment for nondiagonal steps}
    swap: integer; {temporary variable for swap}
    x, y: integer; {current x and y Coordinates}
  begin {DrawLine}
    x := xStart; {line starting point}
    y := yStart;
    {Determine drawing direction and step to the next pixel.}
    a := xEnd - xStart; {difference in x dimension}
    b := yEnd - yStart; {difference in y dimension}
    {Determine whether end point lies to right or left of start point.}
    if a < 0 then {drawing towards smaller x values?}
    begin
      a := -a; {make 'a' positive}
      dx_diag := -1
    end
    else
      dx_diag := 1;
    {Determine whether end point lies above or below start point.}
    if b < 0 then {drawing towards smaller x values?}
    begin
      b := -b; {make 'a' positive}
      dy_diag := -1
    end
    else
      dy_diag := 1;
    {Identify octant containing end point.}
    if a < b then
    begin
      swap := a;
      a := b;
      b := swap;
      dx_nondiag := 0;
      dy_nondiag := dy_diag
    end
    else
    begin
      dx_nondiag := dx_diag;
      dy_nondiag := 0
    end;
    d := b + b - a; {initial value for d is 2*b - a}
    nondiag_inc := b + b; {set initial d increment values}
    diag_inc := b + b - a - a;
    for i := 0 to a do
    begin {draw the a+1 pixels}
      PixelDraw(x, y);
      if d < 0 then {is midpoint above the line?}
      begin {step nondiagonally}
        x := x + dx_nondiag;
        y := y + dy_nondiag;
        d := d + nondiag_inc {update decision variable}
      end
      else
      begin {midpoint is above the line; step diagonally}
        x := x + dx_diag;
        y := y + dy_diag;
        d := d + diag_inc
      end
    end;
  end;

  procedure DrawPolylineOffsetY(pOffset: Integer; IsSolid: Boolean);
  var
    I, p, pStart, pEnd: Integer;
    X1, Y1, X2, Y2, Dx, Dy, OffsX, OffsY: Integer;
    Angle: Double;
    p1, p2, pivot: TlicgCoor;
  begin
    pStart := 0;
    for p := 0 to PartCount - 1 do
    begin
      pEnd := pStart + Parts[p] - 1;
      PixelOffsetX := 0;
      for I := pStart to pEnd - 1 do
      begin
        X1 := PointArray[I].X;
        Y1 := PointArray[I].Y;
        X2 := PointArray[I + 1].X;
        Y2 := PointArray[I + 1].Y;
        Dx := X2 - X1;
        Dy := Y2 - Y1;
        if (Dx = 0) and (Dy = 0) then
          Continue;
        if Dx = 0 then
        begin
          // vertical line
          if Dy < 0 then
            OffsX := -pOffset
          else
            OffsX := pOffset;
          X1 := X1 + OffsX;
          X2 := X2 + OffsX;
        end
        else if Dy = 0 then
        begin
          // horizontal line
          if Dx > 0 then
            OffsY := -pOffset
          else
            OffsY := pOffset;
          Y1 := Y1 + OffsY;
          Y2 := Y2 + OffsY;
        end
        else
        begin
          // line with angle
          pivot := AsCoor(X1, Y1);
          p1 := pivot;
          p2 := AsCoor(X2, Y2);
          Angle := Angle2D(p1, p2);
          p2 := TransformPoint2D(p2, Rotate2D(-Angle, pivot));
          p1.Y := p1.Y - pOffset;
          p2.Y := p2.Y - pOffset;
          { now unrotate}
          p1 := TransformPoint2D(p1, Rotate2D(Angle, pivot));
          p2 := TransformPoint2D(p2, Rotate2D(Angle, pivot));
          X1 := trunc(p1.X);
          Y1 := trunc(p1.Y);
          X2 := trunc(p2.X);
          Y2 := trunc(p2.Y);

          {Angle := Math.ArcTan2(Abs(Dy), Abs(Dx));
          OffsX := Trunc(pOffset * Sin(Angle));
          OffsY := Trunc(pOffset * Cos(Angle));
          if Dy < 0 then
            OffsX := -OffsX;
          if Dx > 0 then
            OffsY := -OffsY;
          X1 := X1 + OffsX;
          X2 := X2 + OffsX;
          Y1 := Y1 + OffsY;
          Y2 := Y2 + OffsY;}
        end;
        if IsSolid then
        begin
          Canvas.MoveTo(X1, Y1);
          Canvas.LineTo(X2, Y2);
        end
        else
        begin
          if not Flow then
            PixelOffsetX := 0;
          DrawLine(X1, Y1, X2, Y2);
        end;
      end;
      pStart := pEnd + 1;
    end;
  end;

  procedure DrawPolylineNoOffsetY;
  var
    I, p, pStart, pEnd: integer;
  begin
    pStart := 0;
    for p := 0 to PartCount - 1 do
    begin
      pEnd := pStart + Parts[p] - 1;
      PixelOffsetX := 0;
      for I := pStart to pEnd - 1 do
      begin
        if not Flow then
          PixelOffsetX := 0;
        DrawLine(PointArray[I].X, PointArray[I].Y, PointArray[Succ(I)].X,
          PointArray[Succ(I)].Y);
      end;
      pStart := pEnd + 1;
    end;
  end;

var
  I, J, p, pStart, pEnd, iFactor: Integer;
  AValue: Boolean;
  Rlt: TlicgBaseRasterLineType;
begin

  with TlicgRasterLineType(RasterLineType) do
  begin
    if PartCount = 0 then
      Exit;

    if (Grapher <> nil) and (Grapher.Device = adPrinter) then
      iFactor := Trunc(Grapher.PrinterDpiY / Grapher.ScreenDpiY)
    else
      iFactor := 1;

    if MaxLen = 0 then
    begin
      CalcMaxLen;
      if MaxLen = 0 then
        Exit;
    end;

    if (iFactor > 1) and (MaxLen >= 1) then
      Rlt := ScaleBy(iFactor)
    else
      Rlt := RasterLineType;

    SetLength(PixelSequenceX, MaxLen * iFactor);
    try
      Canvas.PenWidth := LineWidth;
      Canvas.PenColor := LineColor;
      for I := 0 to TlicgRasterLineType(Rlt).List.Count - 1 do
        with TlicgDashPattern(TlicgRasterLineType(Rlt).List[I]) do
        begin
          if (getOffset = 0) and (getIsSolid or (getDashes.Count = 1)) then
          begin
            // una linea solida sin offset
            if PartCount = 1 then
              Canvas.Polyline(Slice(PointArray, Parts[0]))
            else
            begin
              if Win32Platform = VER_PLATFORM_WIN32_NT then
                { WinNT or Win2000 }
                PolyPolyline(Canvas.Handle, PointArray, Parts, PartCount)
              else
              begin
                { Win95 or Win98 }
                pStart := 0;
                for p := 0 to PartCount - 1 do
                begin
                  pEnd := pStart + Parts[p] - 1;
                  Windows.Polyline(Canvas.Handle, PointArray[pStart], Succ(pEnd
                    - pStart));
                  pStart := pEnd + 1;
                end;
              end;
            end;
          end
          else if getIsSolid or (getDashes.Count > 1) then
          begin
            // build the pixel sequence
            PixelCountX := 0;
            { Debido a lo siguiente, se asume que la secuencia empieza con
             dash-space asi que si no se quiere un dash al principio, tenemos que
             definirlo a longitud 0 el primer dash }
            AValue := True;
            for p := 0 to getDashes.Count - 1 do
            begin
              for J := PixelCountX to (PixelCountX + getDashes[p] - 1) do
                PixelSequenceX[J] := AValue;
              Inc(PixelCountX, getDashes[p]);
              AValue := not AValue;
            end;
            if getOffset = 0 then
              DrawPolylineNoOffsetY
            else
              DrawPolylineOffsetY(getOffset, getIsSolid or (getDashes.Count = 1));
          end;
        end;
    finally
      Finalize(PixelSequenceX);
      if Rlt <> RasterLineType then
        Rlt.Free; // created in ScaleBy
    end;
  end;
end;

procedure Draw_RasterLineTypes(RasterLineTypes: TlicgBaseRasterLineTypes; Index:
  Integer; var PointArray: array of TPoint; const Parts: array of integer;
  PartCount: integer; Canvas: IlicgCanvas; Grapher: TlicgBaseGrapher; LineColor:
  TColor; LineWidth: integer; Flow: Boolean);
begin
  if (Index < 0) or (Index > TlicgRasterLineTypes(RasterLineTypes).List.Count - 1) then
    Exit;
  Draw_RasterLineType(TlicgRasterLineTypes(RasterLineTypes).List[Index],
    PointArray, Parts, PartCount, Canvas, Grapher, LineColor, LineWidth, Flow);
end;


{ TlicgBaseWordList }

destructor TlicgBaseWordList.Destroy;
begin

  inherited;
end;

{ TlicgBaseDashPattern }

constructor TlicgBaseDashPattern.Create;
begin
  inherited;
end;

destructor TlicgBaseDashPattern.Destroy;
begin

  inherited;
end;

{ TlicgBaseRasterLineType }

constructor TlicgBaseRasterLineType.Create(const Name: string);
begin
  inherited Create;
end;

destructor TlicgBaseRasterLineType.Destroy;
begin

  inherited;
end;

{ TlicgBaseRasterLineTypes }

constructor TlicgBaseRasterLineTypes.Create;
begin
  inherited Create;
end;

destructor TlicgBaseRasterLineTypes.Destroy;
begin
  inherited;
end;

end.


