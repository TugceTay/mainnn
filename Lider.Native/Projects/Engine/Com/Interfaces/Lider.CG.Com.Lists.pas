unit Lider.CG.Com.Lists;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  Sysutils,
  Lider.CG.Com.Base,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Consts,
  Lider.CG.Com.ListsInt;

type
  { TlicgIntegerList }

  TlicgIntegerList = class(TInterfacedObject, IlicgIntegerList)
  private
    FList: TList;
  public
    function Get(Index: Integer): Integer; stdcall;
    procedure Put(Index: Integer; Value: Integer); stdcall;
    function GetCapacity: Integer; stdcall;
    procedure SetCapacity(Value: Integer); stdcall;
    function GetCount: Integer; stdcall;
    procedure SetCount(Value: Integer); stdcall;
    procedure SetList(value: TList); stdcall;
    function GetList: TList; stdcall;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AList: IlicgIntegerList); stdcall;
    function Add(Item: Integer): Integer; stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Insert(Index: Integer; Value: Integer); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    function IndexofValue(Item: Integer): Integer; stdcall;
    procedure DeleteValue(Item: Integer); stdcall;
    procedure Sort; stdcall;
    function Find(Value: Integer; var Index: Integer): Boolean; stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure LoadFromFile(const FileName: string); stdcall;
    procedure SaveToFile(const FileName: string); stdcall;
    procedure Reindex; stdcall;
    property Items[Index: Integer]: Integer read Get write Put; default;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property List: TList read getList write setList;
  end;

  { TlicgDoubleList }

  TlicgDoubleList = class(TInterfacedObject, IlicgDoubleList)
  private
    FList: array of Double;
    FCount: Integer;
    FCapacity: Integer;
  public
    function Get(Index: Integer): Double; stdcall;
    procedure Grow; stdcall;
    procedure Put(Index: Integer; const Item: Double); stdcall;
    procedure SetCapacity(NewCapacity: Integer); stdcall;
    procedure SetCount(NewCount: Integer); stdcall;
    function GetCapacity: Integer; stdcall;
    function GetCount: Integer; stdcall;
    constructor Create;
    destructor Destroy; override;
    function Add(const Item: Double): Integer; stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    procedure Insert(Index: Integer; const Item: Double); stdcall;
    procedure Sort; stdcall;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: Double read Get write Put; default;
  end;

  { TlicgBezierPointList } { build a list of Bezier Geometry.Points}

  TlicgBezierPointList = class(TInterfacedObject, IlicgBezierPointList)
  private
    FList: array of TBezierPoint;
    FCount: Integer;
    FCapacity: Integer;
  public
    procedure Grow; stdcall;
    function Get(Index: Integer): TBezierPoint; stdcall;
    procedure Put(Index: Integer; const Item: TBezierPoint); stdcall;
    procedure SetCapacity(NewCapacity: Integer); stdcall;
    procedure SetCount(NewCount: Integer); stdcall;
    function GetCapacity: Integer; stdcall;
    function GetCount: Integer; stdcall;
    constructor Create;
    destructor Destroy; override;
    function Add(const Item: TBezierPoint): Integer; stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Insert(Index: Integer; const Value: TBezierPoint); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    function IndexOfIndex(AIndex: Integer): Integer; stdcall;
    procedure Sort; stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TBezierPoint read Get write Put; default;
  end;

  TIntStringType = class
    _Int: IUnknown;
    _Obj: TObject;
    _Data: Pointer;
  end;

  TInterfaceStringList = class
  private
    FStrList: TStringList;
    FIsDestroyINT, FisDestroyOBJ, FisDestroyData: boolean;
    function GetINT(Index: Integer): IUnknown;
    function GetStr(Index: Integer): string;
    function GetOBJ(Index: Integer): TObject;
    function GetData(Index: Integer): Pointer;
  public
    constructor Create(isDestroyINT, isDestroyOBJ, isDestroyData: Boolean);
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function Add(const _StrParam: string; _INT: IInterface = nil; _Obj: TObject
      = nil; _Data: Pointer = nil): Integer;
    function IndexOf(str: string): Integer;
    procedure Delete(Index: Integer);
    procedure Sort;
    procedure CustomSort(Compare: TStringListSortCompare); // ilker ekleme
    procedure NaturalSort;
    property _INT[Index: integer]: IUnknown read GetINT;
    property _STR[Index: integer]: string read GetStr;
    property _OBJ[Index: integer]: TObject read GetOBJ;
    property _Data[Index: integer]: Pointer read GetData;
  end;

  TStringsPassIntegerData = class(TInterfacedObject, IStringsPassIntegerData)
  private
    FStrings: TStrings;
    FiList: array of integer;
    FisDestroy: boolean;
  public
    constructor Create(const AStrings: TStrings);
    destructor destroy; override;
    procedure Add(const StringData: string; const IntData: integer); stdcall;
    procedure Clear; stdcall;
    function Count: Integer; stdcall;
    function IntegerData(const Index: integer): integer; stdcall;
    function StringData(const Index: integer): string; stdcall;
    function GetStrings: TStrings; stdcall;
    property Strings: TStrings read GetStrings;
  end;

implementation

uses
  Lider.CG.Com.GIS,
  Lider.CG.Com.System;

{ TlicgIntegerList }

constructor TlicgIntegerList.Create;
begin
  inherited Create;
  FList := TList.Create
end;

destructor TlicgIntegerList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TlicgIntegerList.Assign(AList: IlicgIntegerList);
{$IFNDEF LEVEL6}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF LEVEL6}
  FList.Assign(AList.FList);
{$ELSE}
  FList.Clear;
  for I := 0 to AList.Count - 1 do
    FList.Add(AList.List[I]);
{$ENDIF}
end;

function TlicgIntegerList.Add(Item: Integer): Integer;
begin
  Result := FList.Add(Pointer(Item));
end;

procedure TlicgIntegerList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TlicgIntegerList.Clear;
begin
  FList.Clear;
end;

procedure TlicgIntegerList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

function TlicgIntegerList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TlicgIntegerList.SetCount(Value: Integer);
begin
  FList.Count := Value;
end;

function TlicgIntegerList.Get(Index: Integer): Integer;
begin
  Result := Longint(FList[Index]);
end;

procedure TlicgIntegerList.Insert(Index, Value: Integer);
begin
  FList.Insert(Index, Pointer(Value));
end;

procedure TlicgIntegerList.Put(Index, Value: Integer);
begin
  FList[Index] := Pointer(Value);
end;

function TlicgIntegerList.IndexofValue(Item: Integer): Integer;
begin
  Result := FList.IndexOf(Pointer(item));
end;

procedure TlicgIntegerList.DeleteValue(Item: Integer);
var
  Index: Integer;
begin
  Index := IndexOfValue(item);
  if Index >= 0 then
    FList.Delete(Index);
end;

function TlicgIntegerList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

procedure TlicgIntegerList.SetCapacity(Value: Integer);
begin
  FList.Capacity := Value;
end;

function IntegerListCompare(Item1, Item2: Pointer): Integer;
begin
  if Longint(Item1) > Longint(Item2) then
    Result := 1
  else if Longint(Item1) = Longint(Item2) then
    Result := 0
  else
    Result := -1;
end;

procedure TlicgIntegerList.Sort;
begin
  FList.Sort(IntegerListCompare);
end;

procedure TlicgIntegerList.LoadFromStream(Stream: TStream);
var
  I, N, Value: Integer;
begin
  FList.Clear;
  with Stream do
  begin
    Read(N, Sizeof(N));
    if N > 0 then
      SetCapacity(N);
    for I := 1 to N do
    begin
      Read(Value, Sizeof(Value));
      FList.Add(Pointer(Value));
    end;
  end;
end;

procedure TlicgIntegerList.SaveToStream(Stream: TStream);
var
  I, N, Value: Integer;
begin
  N := FList.Count;
  with Stream do
  begin
    Write(N, Sizeof(N));
    for I := 0 to FList.Count - 1 do
    begin
      Value := Longint(FList[I]);
      Write(Value, Sizeof(Value));
    end;
  end;
end;

procedure TlicgIntegerList.LoadFromFile(const FileName: string);
var
  s: TStream;
begin
  if not FileExists(FileName) then
    Exit;
  s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(s);
  finally
    s.Free;
  end;
end;

procedure TlicgIntegerList.SaveToFile(const FileName: string);
var
  s: TStream;
begin
  s := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(s);
  finally
    s.Free;
  end;
end;

function TlicgIntegerList.Find(Value: Integer; var Index: Integer): Boolean;
var
  nLow: Integer;
  nHigh: Integer;
  nCheckPos: Integer;
begin
  nLow := 0;
  nHigh := FList.Count - 1;
  Index := -1;
  Result := False;
  // keep searching until found or
  // no more items to search
  while nLow <= nHigh do
  begin
    nCheckPos := (nLow + nHigh) div 2;
    if Longint(FList[nCheckPos]) < Value then // less than
      nHigh := nCheckPos - 1
    else if Longint(FList[nCheckPos]) > Value then // greater than
      nLow := nCheckPos + 1
    else // equal to
    begin
      Index := nCheckPos;
      Result := true;
      Exit;
    end;
  end;
end;

procedure TlicgIntegerList.Reindex;
var
  I, n, Last: Integer;
begin
  { this method is only used in conjunction with a TlicgVector Parts property }
  if FList.Count = 0 then
    Exit;
  { it is needed to reindex with repeated values }
  n := 0;
  Last := Longint(FList.Items[0]);
  I := 0;
  while I <= FList.Count - 1 do
  begin
    if Last <> Longint(FList.Items[I]) then
    begin
      Inc(n);
      Last := Longint(FList.Items[I]);
    end;
    FList.Items[I] := Pointer(n);
    Inc(I);
  end;
end;

{ TlicgDoubleList }

destructor TlicgDoubleList.Destroy;
begin
  Clear;
  Finalize(FList);
  inherited Destroy;
end;

function TlicgDoubleList.GetCapacity: Integer;
begin
  Result := FCapacity
end;

function TlicgDoubleList.GetCount: Integer;
begin
  Result := FCount
end;

function TlicgDoubleList.Get(Index: Integer): Double;
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Result := FList[Index];
end;

procedure TlicgDoubleList.Grow;
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

procedure TlicgDoubleList.Put(Index: Integer; const Item: Double);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  FList[Index] := Item;
end;

procedure TlicgDoubleList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    LicadGISError(SWordListError);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Double), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure TlicgDoubleList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    LicadGISError(SWordListError);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

function TlicgDoubleList.Add(const Item: Double): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

procedure TlicgDoubleList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TlicgDoubleList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(Double));
end;

procedure TlicgDoubleList.Exchange(Index1, Index2: Integer);
var
  Item: Double;
begin
  if (Index1 < 0) or (Index1 >= FCount) or (Index2 < 0) or (Index2 >= FCount) then
    LicadGISError(SWordListError);
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

procedure TlicgDoubleList.Insert(Index: Integer; const Item: Double);
begin
  if (Index < 0) or (Index > FCount) then
    LicadGISError(SWordListError);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(Double));
  FList[Index] := Item;
  Inc(FCount);
end;

procedure TlicgDoubleList.Sort;

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: Double;
  begin
    repeat
      I := L;
      J := R;
      P := FList[(L + R) shr 1];
      repeat
        while FList[I] < P do
          Inc(I);
        while FList[J] > P do
          Dec(J);
        if I <= J then
        begin
          T := FList[I];
          FList[I] := FList[J];
          FList[J] := T;
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
  if FCount > 1 then
    QuickSort(0, FCount - 1);
end;

function TlicgIntegerList.GetList: TList;
begin
  Result := FList;
end;

procedure TlicgIntegerList.SetList(value: TList);
begin
  FList := value;

end;

{ TlicgBezierPointList }

constructor TlicgBezierPointList.Create;
begin
  inherited;
end;

destructor TlicgBezierPointList.Destroy;
begin
  Clear;
  Finalize(FList);
  inherited;
end;

function TlicgBezierPointList.GetCapacity: Integer;
begin
  Result := FCapacity
end;

function TlicgBezierPointList.GetCount: Integer;
begin
  Result := FCount
end;

function TlicgBezierPointList.Add(const Item: TBezierPoint): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

procedure TlicgBezierPointList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TlicgBezierPointList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TBezierPoint));
end;

function TlicgBezierPointList.Get(Index: Integer): TBezierPoint;
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  Result := FList[Index];
end;

procedure TlicgBezierPointList.Grow;
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

procedure TlicgBezierPointList.Put(Index: Integer; const Item: TBezierPoint);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError(SWordListError);
  FList[Index] := Item;
end;

procedure TlicgBezierPointList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    LicadGISError(SWordListError);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TlicgBezierPointList.SetCount(NewCount: Integer);
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

procedure TlicgBezierPointList.Exchange(Index1, Index2: Integer);
var
  temp: TBezierPoint;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    LicadGISError(SWordListError);
  if (Index1 < 0) or (Index1 >= FCount) then
    LicadGISError(SWordListError);
  temp := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := temp;
end;

function TlicgBezierPointList.IndexofIndex(AIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if FList[I].Index = AIndex then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TlicgBezierPointList.Insert(Index: Integer; const Value: TBezierPoint);
begin
  if (Index < 0) or (Index > FCount) then
    LicadGISError(SWordListError);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TBezierPoint));
  FList[Index] := Value;
  Inc(FCount);
end;

procedure TlicgBezierPointList.LoadFromStream(Stream: TStream);
var
  I, n: Integer;
  temp: TBezierPoint;
begin
  Clear;
  with Stream do
  begin
    Read(n, Sizeof(n));
    if n > 0 then
      SetCapacity(n);
    for I := 1 to n do
    begin
      Read(temp, SizeOf(TBezierPoint));
      Add(temp);
    end;
  end;
end;

procedure TlicgBezierPointList.SaveToStream(Stream: TStream);
var
  I, n: Integer;
  temp: TBezierPoint;
begin
  with Stream do
  begin
    n := FCount;
    Write(n, Sizeof(Integer));
    for I := 0 to n - 1 do
    begin
      temp := FList[I];
      Write(temp, Sizeof(TBezierPoint));
    end;
  end;
end;

procedure TlicgBezierPointList.Sort;

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: TBezierPoint;
  begin
    repeat
      I := L;
      J := R;
      P := FList[(L + R) shr 1];
      repeat
        while FList[I].Index < P.Index do
          Inc(I);
        while FList[J].Index > P.Index do
          Dec(J);
        if I <= J then
        begin
          T := FList[I];
          FList[I] := FList[J];
          FList[J] := T;
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
  if FCount > 1 then
    QuickSort(0, FCount - 1);
end;

constructor TlicgDoubleList.Create;
begin
  inherited;
end;

{ TInterfaceStringList }

function TInterfaceStringList.Add(const _StrParam: string; _INT: IInterface =
  nil; _Obj: TObject = nil; _Data: Pointer = nil): integer;
var
  item: TIntStringType;
begin
  item := TIntStringType.Create;
  item._Int := _INT;
  item._Obj := _Obj;
  item._Data := _Data;
  Result := FStrList.AddObject(_StrParam, item);
end;

procedure TInterfaceStringList.Clear;
var
  i: integer;
begin
  for i := FStrList.Count - 1 downto 0 do
  begin
    if FisDestroyINT then
      if Assigned(TIntStringType(FStrList.Objects[i])._Int) then
        TIntStringType(FStrList.Objects[i])._Int := nil;
    if FisDestroyOBJ then
      if Assigned(TIntStringType(FStrList.Objects[i])._Obj) then
        TIntStringType(FStrList.Objects[i])._Obj.Free;
    if FisDestroyData then
      if Assigned(TIntStringType(FStrList.Objects[i])._Data) then
        TObject(TIntStringType(FStrList.Objects[i])._Data).Free;

    FStrList.Objects[i].Free;
  end;
  FStrList.Clear;
end;

function TInterfaceStringList.Count: integer;
begin
  Result := FStrList.Count;
end;

constructor TInterfaceStringList.create(isDestroyINT, isDestroyOBJ,
  isDestroyData: Boolean);
begin
  inherited Create;
  FisDestroyINT := isDestroyINT;
  FisDestroyOBJ := isDestroyOBJ;
  FisDestroyData := isDestroyData;
  FStrList := TStringList.Create;
end;

procedure TInterfaceStringList.Delete(Index: integer);
begin

  if FisDestroyINT then
    if Assigned(TIntStringType(FStrList.Objects[Index])._Int) then
      TIntStringType(FStrList.Objects[Index])._Int := nil;
  if FisDestroyOBJ then
    if Assigned(TIntStringType(FStrList.Objects[Index])._Obj) then
      TIntStringType(FStrList.Objects[Index])._Obj.Free;
  if FisDestroyData then
    if Assigned(TIntStringType(FStrList.Objects[Index])._Data) then
      TObject(TIntStringType(FStrList.Objects[Index])._Data).Free;

  FStrList.Delete(Index);

end;

destructor TInterfaceStringList.destroy;
begin
  Clear;
  FStrList.Free;
  inherited;
end;

function TInterfaceStringList.GetData(index: integer): Pointer;
begin
  result := nil;
  if (index >= 0) and (index < count) then
    result := TIntStringType(FStrList.Objects[index])._Data;

end;

function TInterfaceStringList.GetINT(index: integer): IUnknown;
begin
  result := nil;
  if (index >= 0) and (index < count) then
    result := TIntStringType(FStrList.Objects[index])._Int;
end;

function TInterfaceStringList.GetOBJ(index: integer): TObject;
begin
  result := nil;
  if (index >= 0) and (index < count) then
    result := TIntStringType(FStrList.Objects[index])._Obj;

end;

function TInterfaceStringList.GetStr(index: integer): string;
begin
  result := '';
  if (index >= 0) and (index < count) then
    result := FStrList.Strings[index];
end;

function TInterfaceStringList.IndexOf(str: string): integer;
begin
  Result := FStrList.IndexOf(str);
end;

procedure TInterfaceStringList.Sort;
begin
  FStrList.Sort; // ilker ekleme önceden boþtu.
end;

procedure TInterfaceStringList.CustomSort(Compare: TStringListSortCompare); // ilker ekleme
begin
  FStrList.CustomSort(Compare);
end;

procedure TInterfaceStringList.NaturalSort;
begin
  FStrList.CustomSort(CustomNaturalSort);
end;

{ TStringsPassIntegerData }
procedure TStringsPassIntegerData.Add(const StringData: string; const IntData: integer);
var
  s: string;
begin
  s := StringData;
  FStrings.Add(s);
  if FStrings.count - 1 > high(FiList) then
    SetLength(FiList, FStrings.count + 100);
  FiList[FStrings.count - 1] := IntData;
end;

procedure TStringsPassIntegerData.Clear;
begin
  FStrings.Clear;
end;

function TStringsPassIntegerData.Count: integer;
begin
  Result := FStrings.Count;
end;

constructor TStringsPassIntegerData.Create(const AStrings: TStrings);
begin
  inherited Create;

  SetLength(FiList, 1000);
  FisDestroy := AStrings = nil;
  if FisDestroy then
    FStrings := TStringList.Create

  else
  begin
    AStrings.Clear;
    FStrings := AStrings;
  end;
end;

destructor TStringsPassIntegerData.destroy;
begin
  Clear;
  if FisDestroy then
    FStrings.Free;
  FiList := nil;
  inherited;
end;

function TStringsPassIntegerData.GetStrings: TStrings;
begin
  Result := FStrings;
end;

function TStringsPassIntegerData.IntegerData(const index: integer): integer;
begin
  Result := FiList[index];
end;

function TStringsPassIntegerData.StringData(const index: integer): string;
begin
  Result := FStrings.Strings[index];
end;

end.


