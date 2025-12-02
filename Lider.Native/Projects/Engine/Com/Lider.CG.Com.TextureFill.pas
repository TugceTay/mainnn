unit Lider.CG.Com.TextureFill;

interface

uses
  Classes, Graphics;

type
  TlicgTextureFill = class
    BitmapName: string;
    FileName: string;
    ppm: Double;
    guid: string;
    IsAlfa: Integer;
    AlfaMask: Integer;
    constructor Create(_sn, _fn: string; _ppm: Double; _guid: string = '';
      _isAlfa: Integer = 1; _AlfaMask: Integer = 1);
  end;

  TlicgTextureFills = class
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Exists(_sn: string): boolean;
    function GetBitmap(_sn: string): TlicgTextureFill;
    function AddBitmap(_sn, _fn: string; _ppm: double; _guid: string = ''; _isAlfa:
      Integer = 1; _AlfaMask: Integer = 1): TlicgTextureFill;
  end;

  TlicgTextureFillGroup = class
    GroupName: string;
    FTextureFills: TlicgTextureFills;
  public
    constructor Create(_gn: string);
    destructor Destroy; override;
  end;

  TlicgTextureFillGroups = class
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Exists(_gn: string): boolean;
    function GetGroup(_gn: string): TlicgTextureFillGroup;
    function AddGroup(_gn: string): TlicgTextureFillGroup;
  end;

  TlicgTextureFillLibrary = class
    FTextureFillGroups: TlicgTextureFillGroups;
    LibraryName: string;
  public
    constructor Create(_ln: string);
    destructor Destroy; override;
  end;

  TlicgTextureFillLibraries = class
    FList: TList;
  public
    DirName: string;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Exists(_ln: string): boolean;
    function GetLib(_ln: string): TlicgTextureFillLibrary;
    function AddLib(_ln: string): TlicgTextureFillLibrary;
    procedure AddBitmap(_ln, _gn, _sn, _fn: string; _ppm: double; _guid: string =
      ''; _isAlfa: integer = 1; _AlfaMask: integer = 1);
    function GetBitmap(_ln, _gn, _sn: string): TlicgTextureFill;
  end;

implementation

{ TlicgTextureFills }

function TlicgTextureFills.AddBitmap(_sn, _fn: string; _ppm: double; _guid:
  string = ''; _isAlfa: integer = 1; _AlfaMask: integer = 1): TlicgTextureFill;
begin
  Result := GetBitmap(_sn);
  if Result = nil then
  begin
    Result := TlicgTextureFill.Create(_sn, _fn, _ppm, _guid, _isAlfa, _AlfaMask);
    Flist.Add(Result);
  end;
end;

procedure TlicgTextureFills.Clear;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    TlicgTextureFill(FList.Items[I]).Free;
  FList.Clear;
end;

constructor TlicgTextureFills.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TlicgTextureFills.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TlicgTextureFills.Exists(_sn: string): boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FList.Count - 1 do
  begin
    if TlicgTextureFill(FList.Items[I]).BitmapName = _sn then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TlicgTextureFills.GetBitmap(_sn: string): TlicgTextureFill;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FList.Count - 1 do
  begin
    if TlicgTextureFill(FList.Items[I]).BitmapName = _sn then
    begin
      Result := TlicgTextureFill(FList.Items[I]);
      Break;
    end;
  end;
end;

{ TlicgTextureFill }

constructor TlicgTextureFill.Create(_sn, _fn: string; _ppm: double; _guid:
  string = ''; _isAlfa: integer = 1; _AlfaMask: integer = 1);
begin
  inherited Create;
  BitmapName := _sn;
  FileName := _fn;
  ppm := _ppm;
  guid := _guid;
  IsAlfa := _isAlfa;
  AlfaMask := _AlfaMask;
end;

{ TlicgTextureFillLibrary }

constructor TlicgTextureFillLibrary.Create(_ln: string);
begin
  inherited Create;
  LibraryName := _ln;
  FTextureFillGroups := TlicgTextureFillGroups.Create;
end;

destructor TlicgTextureFillLibrary.Destroy;
begin
  FTextureFillGroups.Free;
  inherited;
end;

{ TlicgTextureFillGroups }

function TlicgTextureFillGroups.AddGroup(_gn: string): TlicgTextureFillGroup;
begin
  Result := GetGroup(_gn);
  if Result = nil then
  begin
    Result := TlicgTextureFillGroup.Create(_gn);
    Flist.Add(Result);
  end;
end;

procedure TlicgTextureFillGroups.clear;
var
  I: Integer;
begin
  for I := FList.count - 1 downto 0 do
    TlicgTextureFillGroup(FList.items[I]).Free;
  FList.Clear;
end;

constructor TlicgTextureFillGroups.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TlicgTextureFillGroups.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TlicgTextureFillGroups.Exists(_gn: string): boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FList.Count - 1 do
  begin
    if TlicgTextureFillGroup(FList.Items[I]).GroupName = _gn then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TlicgTextureFillGroups.GetGroup(_gn: string): TlicgTextureFillGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FList.Count - 1 do
  begin
    if TlicgTextureFillGroup(FList.Items[I]).GroupName = _gn then
    begin
      Result := TlicgTextureFillGroup(FList.Items[I]);
      Break;
    end;
  end;
end;

{ TlicgTextureFillGroup }

constructor TlicgTextureFillGroup.Create(_gn: string);
begin
  inherited Create;
  GroupName := _gn;
  FTextureFills := TlicgTextureFills.create;
end;

destructor TlicgTextureFillGroup.Destroy;
begin
  FTextureFills.Free;
  inherited;
end;

{ TlicgTextureFillLibraries }

function TlicgTextureFillLibraries.AddLib(_ln: string): TlicgTextureFillLibrary;
begin
  Result := GetLib(_ln);
  if Result = nil then
  begin
    Result := TlicgTextureFillLibrary.create(_ln);
    Flist.Add(Result);
  end;
end;

procedure TlicgTextureFillLibraries.AddBitmap(_ln, _gn, _sn, _fn: string; _ppm: double;
  _guid: string; _isAlfa, _AlfaMask: integer);
var
  l: TlicgTextureFillLibrary;
  g: TlicgTextureFillGroup;
  s: TlicgTextureFill;
begin
  l := AddLib(_ln);
  if l <> nil then
  begin
    g := l.FTextureFillGroups.AddGroup(_gn);
    if g <> nil then
    begin
      s := g.FTextureFills.AddBitmap(_sn, _fn, _ppm, _guid, _isAlfa, _AlfaMask);
      if s <> nil then
      begin
      end;
    end;
  end;
end;

procedure TlicgTextureFillLibraries.Clear;
var
  I: Integer;
begin
  for I := FList.count - 1 downto 0 do
    TlicgTextureFillLibrary(FList.items[I]).Free;
  FList.Clear;
end;

constructor TlicgTextureFillLibraries.Create;
begin
  inherited create;
  FList := TList.Create;
  DirName := 'Symbols\Bitmap';
end;

destructor TlicgTextureFillLibraries.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TlicgTextureFillLibraries.Exists(_ln: string): boolean;
var
  I: Integer;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do
  begin
    if TlicgTextureFillLibrary(FList.Items[i]).LibraryName = _ln then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TlicgTextureFillLibraries.GetLib(_ln: string): TlicgTextureFillLibrary;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    if TlicgTextureFillLibrary(FList.Items[i]).LibraryName = _ln then
    begin
      Result := TlicgTextureFillLibrary(FList.Items[i]);
      Break;
    end;
  end;
end;

function TlicgTextureFillLibraries.GetBitmap(_ln, _gn, _sn: string): TlicgTextureFill;
var
  l: TlicgTextureFillLibrary;
  g: TlicgTextureFillGroup;
begin
  Result := nil;
  l := GetLib(_ln);
  if l <> nil then
  begin
    g := l.FTextureFillGroups.GetGroup(_gn);
    if g <> nil then
    begin
      Result := g.FTextureFills.GetBitmap(_sn);
    end;
  end;
end;

end.


