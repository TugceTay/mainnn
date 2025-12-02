unit Lider.CG.Com.StringArray;

interface

uses
  SysUtils,
  Classes,
  XQSparseArray;

type
  TStringArray = class;

  TStringArrayStrings = class(TStrings)
  private
    FIndex: Integer;
    FGrid: TStringArray;
    procedure CalcXY(Index: Integer; var X, Y: Integer);
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
    constructor Create(AGrid: TStringArray; AIndex: Longint);
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function Add(const S: string): Integer; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

  TStringArray = class
  private
    FData: Pointer;
    FRows: Pointer;
    FCols: Pointer;
    FRowCount: longint;
    FColCount: longint;
    procedure Initialize;
    function GetCells(ACol, ARow: Integer): string;
    function GetCols(Index: Integer): TStrings;
    function GetObjects(ACol, ARow: Integer): TObject;
    function GetRows(Index: Integer): TStrings;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    procedure SetCols(Index: Integer; Value: TStrings);
    procedure SetObjects(ACol, ARow: Integer; Value: TObject);
    procedure SetRows(Index: Integer; Value: TStrings);
    function EnsureColRow(Index: Integer; IsCol: Boolean): TStringArrayStrings;
    function EnsureDataRow(ARow: Integer): Pointer;
    procedure SetColCount(Value: longint);
    procedure SetRowCount(Value: longint);
  public
    constructor Create(ARowCount, AColCount: longint);
    destructor Destroy; override;
    procedure ColumnMove(FromIndex, ToIndex: Longint);
    procedure RowMove(FromIndex, ToIndex: Longint);
    procedure Clear;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property Cols[Index: Integer]: TStrings read GetCols write SetCols;
    property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
    property Rows[Index: Integer]: TStrings read GetRows write SetRows;
    property RowCount: longint read FRowCount write SetRowCount;
    property ColCount: longint read FColCount write SetColCount;
  end;

  { TStringSparseList class }

  TStringSparseList = class(TStrings)
  private
    FList: TSparseList; { of StrItems }
    FOnChange: TNotifyEvent;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure Changed; virtual;
    procedure Error; virtual;
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor Destroy; override;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Clear; override;
    property List: TSparseList read FList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  Lider.CG.Com.System;

type
  EStringSparseListError = class(Exception);

  PStrItem = ^TStrItem;

  TStrItem = record
    FObject: TObject;
    FString: string;
  end;

function NewStrItem(const AString: string; AObject: TObject): PStrItem;
begin
  New(Result);
  Result^.FObject := AObject;
  Result^.FString := AString;
end;

procedure DisposeStrItem(P: PStrItem);
begin
  Dispose(P);
end;

{ TStringArrayStrings }

{ AIndex < 0 is a column (for column -AIndex - 1)
  AIndex > 0 is a row (for row AIndex - 1)
  AIndex = 0 denotes an empty row or column }

constructor TStringArrayStrings.Create(AGrid: TStringArray; AIndex: Longint);
begin
  inherited Create;
  FGrid := AGrid;
  FIndex := AIndex;
end;

procedure TStringArrayStrings.Assign(Source: TPersistent);
var
  I, Max: Integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    Max := TStrings(Source).Count - 1;
    if Max >= Count then
      Max := Count - 1;
    try
      for I := 0 to Max do
      begin
        Put(I, TStrings(Source).Strings[I]);
        PutObject(I, TStrings(Source).Objects[I]);
      end;
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TStringArrayStrings.CalcXY(Index: Integer; var X, Y: Integer);
begin
  if FIndex = 0 then
  begin
    X := -1;
    Y := -1;
  end
  else if FIndex > 0 then
  begin
    X := Index;
    Y := FIndex - 1;
  end
  else
  begin
    X := -FIndex - 1;
    Y := Index;
  end;
end;

{ Changes the meaning of Add to mean copy to the first empty string }

function TStringArrayStrings.Add(const S: string): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Strings[I] = '' then
    begin
      Strings[I] := S;
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TStringArrayStrings.Clear;
var
  SSList: TStringSparseList;
  I: Integer;

  function BlankStr(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    Objects[TheIndex] := nil;
    Strings[TheIndex] := '';
    Result := 0;
  end;

begin
  if FIndex > 0 then
  begin
    SSList := TStringSparseList(TSparseList(FGrid.FData)[FIndex - 1]);
    if SSList <> nil then
      SSList.List.ForAll(@BlankStr);
  end
  else if FIndex < 0 then
    for I := Count - 1 downto 0 do
    begin
      Objects[I] := nil;
      Strings[I] := '';
    end;
end;

function TStringArrayStrings.Get(Index: Integer): string;
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  if X < 0 then
    Result := ''
  else
    Result := FGrid.Cells[X, Y];
end;

function TStringArrayStrings.GetCount: Integer;
begin
  { Count of a row is the column count, and vice versa }
  if FIndex = 0 then
    Result := 0
  else if FIndex > 0 then
    Result := Integer(FGrid.ColCount)
  else
    Result := Integer(FGrid.RowCount);
end;

function TStringArrayStrings.GetObject(Index: Integer): TObject;
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  if X < 0 then
    Result := nil
  else
    Result := FGrid.Objects[X, Y];
end;

procedure TStringArrayStrings.Put(Index: Integer; const S: string);
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  FGrid.Cells[X, Y] := S;
end;

procedure TStringArrayStrings.PutObject(Index: Integer; AObject: TObject);
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  FGrid.Objects[X, Y] := AObject;
end;

procedure TStringArrayStrings.Delete(Index: Integer);
begin
end;

procedure TStringArrayStrings.Insert(Index: Integer; const S: string);
begin
end;

{ TStringArray }

constructor TStringArray.Create(ARowCount, AColCount: longint);
begin
  inherited Create;
  FRowCount := ARowCount;
  FColCount := AColCount;
  Initialize;
end;

procedure TStringArray.Clear;

  function FreeItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    TObject(TheItem).Free;
    Result := 0;
  end;

begin
  if FRows <> nil then
  begin
    TSparseList(FRows).ForAll(@FreeItem);
    TSparseList(FRows).Free;
  end;
  if FCols <> nil then
  begin
    TSparseList(FCols).ForAll(@FreeItem);
    TSparseList(FCols).Free;
  end;
  if FData <> nil then
  begin
    TSparseList(FData).ForAll(@FreeItem);
    TSparseList(FData).Free;
  end;
  FRows := nil;
  FCols := nil;
  FData := nil;
  Initialize;
end;

destructor TStringArray.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStringArray.ColumnMove(FromIndex, ToIndex: Longint);

  function MoveColData(Index: Integer; ARow: TStringSparseList): integer; far;
  begin
    ARow.Move(FromIndex, ToIndex);
    Result := 0;
  end;

begin
  TSparseList(FData).ForAll(@MoveColData);
end;

procedure TStringArray.RowMove(FromIndex, ToIndex: Longint);
begin
  TSparseList(FData).Move(FromIndex, ToIndex);
  {Invalidate;
  inherited RowMove(FromIndex, ToIndex);}
end;

procedure TStringArray.Initialize;
var
  quantum: TSPAQuantum;
begin
  if FCols = nil then
  begin
    if ColCount > 512 then
      quantum := SPALarge
    else
      quantum := SPASmall;
    FCols := TSparseList.Create(quantum);
  end;
  if RowCount > 256 then
    quantum := SPALarge
  else
    quantum := SPASmall;
  if FRows = nil then
    FRows := TSparseList.Create(quantum);
  if FData = nil then
    FData := TSparseList.Create(quantum);
end;

function TStringArray.EnsureColRow(Index: Integer; IsCol: Boolean): TStringArrayStrings;
var
  RCIndex: Integer;
  PList: ^TSparseList;
begin
  if IsCol then
    PList := @FCols
  else
    PList := @FRows;
  Result := TStringArrayStrings(PList^[Index]);
  if Result = nil then
  begin
    if IsCol then
      RCIndex := -Index - 1
    else
      RCIndex := Index + 1;
    Result := TStringArrayStrings.Create(Self, RCIndex);
    PList^[Index] := Result;
  end;
end;

function TStringArray.EnsureDataRow(ARow: Integer): Pointer;
var
  quantum: TSPAQuantum;
begin
  Result := TStringSparseList(TSparseList(FData)[ARow]);
  if Result = nil then
  begin
    if ColCount > 512 then
      quantum := SPALarge
    else
      quantum := SPASmall;
    Result := TStringSparseList.Create(quantum);
    TSparseList(FData)[ARow] := Result;
  end;
end;

function TStringArray.GetCells(ACol, ARow: Integer): string;
var
  ssl: TStringSparseList;
begin
  ssl := TStringSparseList(TSparseList(FData)[ARow]);
  if ssl = nil then
    Result := ''
  else
    Result := ssl[ACol];
end;

function TStringArray.GetCols(Index: Integer): TStrings;
begin
  Result := EnsureColRow(Index, True);
end;

function TStringArray.GetObjects(ACol, ARow: Integer): TObject;
var
  ssl: TStringSparseList;
begin
  ssl := TStringSparseList(TSparseList(FData)[ARow]);
  if ssl = nil then
    Result := nil
  else
    Result := ssl.Objects[ACol];
end;

function TStringArray.GetRows(Index: Integer): TStrings;
begin
  Result := EnsureColRow(Index, False);
end;

procedure TStringArray.SetCells(ACol, ARow: Integer; const Value: string);
begin
  TStringArrayStrings(EnsureDataRow(ARow))[ACol] := Value;
  EnsureColRow(ACol, True);
  EnsureColRow(ARow, False);
end;

procedure TStringArray.SetCols(Index: Integer; Value: TStrings);
begin
  EnsureColRow(Index, True).Assign(Value);
end;

procedure TStringArray.SetObjects(ACol, ARow: Integer; Value: TObject);
begin
  TStringArrayStrings(EnsureDataRow(ARow)).Objects[ACol] := Value;
  EnsureColRow(ACol, True);
  EnsureColRow(ARow, False);
end;

procedure TStringArray.SetRows(Index: Integer; Value: TStrings);
begin
  EnsureColRow(Index, False).Assign(Value);
end;

procedure TStringArray.SetColCount(Value: longint);
begin
  if Value <> FColCount then
  begin
    FColCount := Value;
    Initialize;
  end;
end;

procedure TStringArray.SetRowCount(Value: longint);
begin
  if Value <> FRowCount then
  begin
    FRowCount := Value;
    Initialize;
  end;
end;

{ TStringSparseList }

constructor TStringSparseList.Create(Quantum: TSPAQuantum);
begin
  inherited Create;
  FList := TSparseList.Create(Quantum)
end;

destructor TStringSparseList.Destroy;
begin
  if FList <> nil then
  begin
    FList.Clear;
    FList.Free;
    FList := nil;
  end;
  inherited Destroy;
end;

procedure TStringSparseList.ReadData(Reader: TReader);
var
  i: Integer;
begin
  with Reader do
  begin
    i := Integer(ReadInteger);
    while i > 0 do
    begin
      InsertObject(Integer(ReadInteger), ReadString, nil);
      Dec(i)
    end
  end
end;

procedure TStringSparseList.WriteData(Writer: TWriter);
var
  itemCount: Integer;

  function CountItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    Inc(itemCount);
    Result := 0
  end;

  function StoreItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    with Writer do
    begin
      WriteInteger(TheIndex); { Item index }
      WriteString(PStrItem(TheItem)^.FString);
    end;
    Result := 0
  end;

begin
  with Writer do
  begin
    itemCount := 0;
    FList.ForAll(@CountItem);
    WriteInteger(itemCount);
    FList.ForAll(@StoreItem);
  end
end;

procedure TStringSparseList.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('List', ReadData, WriteData, True);
end;

function TStringSparseList.Get(Index: Integer): string;
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p = nil then
    Result := ''
  else
    Result := p^.FString
end;

function TStringSparseList.GetCount: Integer;
begin
  Result := FList.Count
end;

function TStringSparseList.GetObject(Index: Integer): TObject;
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p = nil then
    Result := nil
  else
    Result := p^.FObject
end;

procedure TStringSparseList.Put(Index: Integer; const S: string);
var
  p: PStrItem;
  obj: TObject;
begin
  p := PStrItem(FList[Index]);
  if p = nil then
    obj := nil
  else
    obj := p^.FObject;
  if (S = '') and (obj = nil) then { Nothing left to store }
    FList[Index] := nil
  else
    FList[Index] := NewStrItem(S, obj);
  if p <> nil then
    DisposeStrItem(p);
  Changed
end;

procedure TStringSparseList.PutObject(Index: Integer; AObject: TObject);
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p <> nil then
    p^.FObject := AObject
  else if AObject <> nil then
    Error;
  Changed
end;

procedure TStringSparseList.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self)
end;

procedure TStringSparseList.Error;
begin
  raise EStringSparseListError.Create('Put Object Error!')
end;

procedure TStringSparseList.Delete(Index: Integer);
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p <> nil then
    DisposeStrItem(p);
  FList.Delete(Index);
  Changed
end;

procedure TStringSparseList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TStringSparseList.Insert(Index: Integer; const S: string);
begin
  FList.Insert(Index, NewStrItem(S, nil));
  Changed
end;

procedure TStringSparseList.Clear;

  function ClearItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    DisposeStrItem(PStrItem(TheItem)); { Item guaranteed non-nil }
    Result := 0
  end;

begin
  FList.ForAll(@ClearItem);
  FList.Clear;
  Changed
end;

end.


