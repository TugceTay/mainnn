unit Lider.CG.Com.BTDBISAM;

{$I Lider.CG.Com.Component.inc}

interface

uses
  SysUtils,
  Windows,
  Classes,
  Graphics,
  DBISAMTb,
  Lider.CG.Com.GIS;

type

  TlicgDBISAMTable = class(TlicgBaseTable)
  private
    FDBISAMTable: TDBISAMTable;
  protected
    function GetActive: boolean; override;
    procedure SetActive(Value: boolean); override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(Gis: TlicgBaseGIS; const FName: string;
      ReadWrite, Shared: boolean); override;
    destructor Destroy; override;
    procedure Append(NewRecno: Integer); override;
    function BOF: Boolean; override;
    function EOF: Boolean; override;
    function DateGet(const FieldName: string): TDateTime; override;
    function DateGetN(FieldNo: integer): TDateTime; override;
    function Deleted: Boolean; override;
    function Field(FieldNo: integer): string; override;
    function FieldCount: integer; override;
    function FieldDec(FieldNo: integer): integer; override;
    function FieldGet(const FieldName: string): string; override;
    function FieldGetN(FieldNo: integer): string; override;
    function FieldLen(FieldNo: integer): integer; override;
    function FieldNo(const FieldName: string): integer; override;
    function FieldType(FieldNo: integer): char; override;
    function Find(const ss: string; IsExact, IsNear: boolean): boolean; override;
    function FloatGet(const Fieldname: string): Double; override;
    function FloatGetN(FieldNo: Integer): Double; override;
    function IndexCount: integer; override;
    function IndexAscending(Value: integer): boolean; override;
    function Index(const INames, Tag: string): integer; override;
    function IndexUnique(Value: integer): boolean; override;
    function IndexExpression(Value: integer): string; override;
    function IndexTagName(Value: integer): string; override;
    function IndexFilter(Value: integer): string; override;
    function IntegerGet(const FieldName: string): Integer; override;
    function IntegerGetN(FieldNo: integer): Integer; override;
    function LogicGet(const FieldName: string): Boolean; override;
    function LogicGetN(FieldNo: integer): Boolean; override;
    procedure MemoSave(const FieldName: string; Stream: TStream); override;
    procedure MemoSaveN(FieldNo: integer; Stream: TStream); override;
    function MemoSize(const FieldName: string): Integer; override;
    function MemoSizeN(FieldNo: integer): Integer; override;
    function RecordCount: Integer; override;
    function StringGet(const FieldName: string): string; override;
    function StringGetN(FieldNo: integer): string; override;
    //Procedure CopyStructure( Const FileName, APassword: String ); Override;
    //Procedure CopyTo( Const FileName, APassword: String ); Override;
    procedure DatePut(const FieldName: string; value: TDateTime); override;
    procedure DatePutN(FieldNo: integer; value: TDateTime); override;
    procedure Delete; override;
    procedure Edit; override;
    procedure FieldPut(const FieldName, Value: string); override;
    procedure FieldPutN(FieldNo: integer; const Value: string); override;
    procedure First; override;
    procedure FloatPut(const FieldName: string; const Value: Double); override;
    procedure FloatPutN(FieldNo: integer; const Value: Double); override;
    procedure FlushDB; override;
    procedure Go(n: Integer); override;
    procedure IndexOn(const IName, tag, keyexp, forexp: string;
      uniq: TlicgIndexUnique; ascnd: TlicgSortStatus); override;
    procedure IntegerPut(const Fieldname: string; Value: Integer); override;
    procedure IntegerPutN(FieldNo: integer; Value: Integer); override;
    procedure Last; override;
    procedure LogicPut(const fieldname: string; value: boolean); override;
    procedure LogicPutN(fieldno: integer; value: boolean); override;
    procedure MemoLoad(const fieldname: string; Stream: TStream); override;
    procedure MemoLoadN(fieldno: integer; Stream: TStream); override;
    procedure Next; override;
    procedure Pack; override;
    procedure Post; override;
    procedure Prior; override;
    procedure Recall; override;
    procedure Refresh; override;
    procedure Reindex; override;
    procedure SetTagTo(const TName: string); override;
    procedure SetUseDeleted(tf: boolean); override;
    procedure StringPut(const fieldname, value: string); override;
    procedure StringPutN(fieldno: integer; const value: string); override;
    procedure Zap; override;
    function DBCreateTable(const fname: string;
      AFieldList: TStringList): boolean; override;
    function DBTableExists(const TableName: string): Boolean; override;
    function DBDropTable(const TableName: string): Boolean; override;
    function DBDropIndex(const TableName: string): Boolean; override;
    function DBRenameTable(const Source, Target: string): Boolean; override;
  end;

implementation

uses
  Lider.CG.Com.System,
  Forms,
  DB; // EzSystem, EzConsts, EzBasicEnts;

function TlicgDBISAMTable.DBRenameTable(const Source, Target: string): Boolean;
begin
  if FileExists(Source + '.dat') then
    SysUtils.RenameFile(Source + '.dat', Target + '.dat');
  if FileExists(Source + '.idx') then
    SysUtils.RenameFile(Source + '.idx', Target + '.idx');
  if FileExists(Source + '.blb') then
    SysUtils.RenameFile(Source + '.blb', Target + '.blb');
end;

function TlicgDBISAMTable.DBDropIndex(const TableName: string): Boolean;
begin
  // not used
end;

function TlicgDBISAMTable.DBDropTable(const TableName: string): Boolean;
begin
  SysUtils.DeleteFile(Tablename + '.dat');
  SysUtils.DeleteFile(Tablename + '.idx');
  SysUtils.DeleteFile(Tablename + '.blb');
end;

function TlicgDBISAMTable.DBTableExists(const TableName: string): Boolean;
begin
  Result := FileExists(ChangeFileExt(TableName, '.dat'));
end;

function TlicgDBISAMTable.DBCreateTable(const fname: string; AFieldList: TStringList): boolean;
var
  s: Ansistring;
  v: boolean;
  i: integer;
  p: integer;
  fs: Ansistring;
  ft: string[1];
  fl: integer;
  fd: integer;

  procedure LoadField;
  begin
    v := true;
    p := Ansipos(';', s);
    fs := '';
    if p > 0 then
    begin
      fs := System.Copy(s, 1, pred(p));
      System.Delete(s, 1, p);
    end
    else
      v := false;

    p := Ansipos(';', s);
    ft := ' ';
    if p = 2 then
    begin
      ft := System.Copy(s, 1, 1);
      System.Delete(s, 1, p);
    end
    else
      v := false;

    p := Ansipos(';', s);
    fl := 0;
    if p > 0 then
    begin
      try
        fl := StrToInt(System.Copy(s, 1, pred(p)));
        System.Delete(s, 1, p);
      except
        on Exception do
          v := false;
      end;
    end
    else
      v := false;

    fd := 0;
    try
      fd := StrToInt(System.Copy(s, 1, 3));
    except
      on Exception do
        v := false;
    end;
  end;

begin
  { create a DBISAM table (.DAT and .IDX files }
  with TDBISAMTable.Create(nil) do
  try
    Databasename := ExtractFilePath(fname);
    TableName := ChangeFileExt(ExtractFileName(fname), '');
    with FieldDefs do
    begin
      Clear;
      Add('UID', ftInteger, 0, False);
      Add('DELETED', ftBoolean, 0, False);
      for I := 0 to AFieldList.count - 1 do
      begin
        s := AFieldList[I];
        LoadField;
        if not v then
          LicadGISError('Hatalý Alan Tanýmlanmýþ');
        case ft[1] of
          'C':
            Add(fs, ftString, fl, False);
          'F', 'N':
            if fd = 0 then
              Add(fs, ftInteger, 0, False)
            else
              Add(fs, ftFloat, 0, False);
          'M':
            Add(fs, ftMemo, 0, False);
          'G':
            Add(fs, ftGraphic, 0, False);
          'B':
            Add(fs, ftTypedBinary, 0, False);
          'L':
            Add(fs, ftBoolean, 0, False);
          'D':
            Add(fs, ftDate, 0, False);
          'T':
            Add(fs, ftTime, 0, False);
          'I':
            Add(fs, ftInteger, 0, False);
        end;
      end;
    end;
    with IndexDefs do
    begin
      Clear;
      Add('', 'UID', [ixPrimary]);
    end;
    if not Exists then
      CreateTable;
  finally
    Free;
  end;
end;

constructor TlicgDBISAMTable.Create(Gis: TlicgBaseGIS; const FName: string;
  ReadWrite, Shared: boolean);
begin
  inherited Create(Gis, FName, ReadWrite, Shared);
  if Length(FName) > 0 then
  begin
    FDBISAMTable := TDBISAMTable.Create(nil);
    with FDBISAMTable do
    begin
      DatabaseName := ExtractFilePath(FName);
      TableName := ChangeFileExt(ExtractFileName(FName), '');
      ReadOnly := not ReadWrite;
      Exclusive := not Shared;
      Open;
    end;
  end;
end;

destructor TlicgDBISAMTable.Destroy;
begin
  FDBISAMTable.Free;
  inherited Destroy;
end;

function TlicgDBISAMTable.GetActive: boolean;
begin
  result := FDBISAMTable.Active;
end;

procedure TlicgDBISAMTable.SetActive(Value: boolean);
begin
  FDBISAMTable.Active := value;
end;

function TlicgDBISAMTable.GetRecNo: Integer;
begin
  result := FDBISAMTable.FieldByName('UID').AsInteger;
end;

procedure TlicgDBISAMTable.SetRecNo(Value: Integer);
begin
  if FDBISAMTable.IndexName <> '' then
  begin
    FDBISAMTable.IndexName := ''; // primary index
    if not FDBISAMTable.FindKey([Value]) then
      LicadGisError('Record not found !');
  end
  else
  begin
    if FDBISAMTable.FieldByName('UID').AsInteger <> Value then
    begin
      if not FDBISAMTable.FindKey([Value]) then
        LicadGisError('Record not found !');
    end;
  end;
end;

procedure TlicgDBISAMTable.Append(NewRecno: Integer);
begin
  FDBISAMTable.Insert;
  FDBISAMTable.FieldByName('UID').AsInteger := NewRecno;
  FDBISAMTable.Post;
end;

function TlicgDBISAMTable.BOF: Boolean;
begin
  result := FDBISAMTable.BOF;
end;

function TlicgDBISAMTable.EOF: Boolean;
begin
  result := FDBISAMTable.EOF;
end;

function TlicgDBISAMTable.DateGet(const FieldName: string): TDateTime;
begin
  result := FDBISAMTable.FieldByName(FieldName).AsDateTime;
end;

function TlicgDBISAMTable.DateGetN(FieldNo: integer): TDateTime;
begin
  result := FDBISAMTable.Fields[FieldNo - 1].AsdateTime;
end;

function TlicgDBISAMTable.Deleted: Boolean;
begin
  result := False;
end;

function TlicgDBISAMTable.Field(FieldNo: integer): string;
begin
  result := FDBISAMTable.Fields[FieldNo - 1].FieldName;
end;

function TlicgDBISAMTable.FieldCount: integer;
begin
  result := FDBISAMTable.Fields.Count;
end;

function TlicgDBISAMTable.FieldDec(FieldNo: integer): integer;
var
  Datatype: TFieldType;
begin
  Datatype := FDBISAMTable.Fields[FieldNo - 1].Datatype;
  if Datatype in ftNonTexttypes then
    Result := 0
  else
    case Datatype of
      ftstring{$IFDEF LEVEL4}, ftFixedChar,
      ftWidestring{$ENDIF}
{$IFDEF LEVEL5}, ftGUID{$ENDIF}:
        Result := 0;
      ftBCD:
        Result := FDBISAMTable.Fields[FieldNo - 1].Size;
      ftFloat, ftCurrency,
        ftAutoInc, ftSmallInt, ftInteger, ftWord
{$IFNDEF LEVEL3}, ftLargeInt{$ENDIF}:
        Result := 0;
      ftDate, ftTime, ftDateTime:
        Result := 0;
      ftBoolean:
        Result := 0;
    end;
end;

function TlicgDBISAMTable.FieldGet(const FieldName: string): string;
begin
  result := FDBISAMTable.FieldByName(FieldName).AsString;
end;

function TlicgDBISAMTable.FieldGetN(FieldNo: integer): string;
begin
  result := FDBISAMTable.Fields[FieldNo - 1].AsString;
end;

function TlicgDBISAMTable.FieldLen(FieldNo: integer): integer;
var
  Datatype: TFieldType;
begin
  Datatype := FDBISAMTable.Fields[FieldNo - 1].Datatype;
  if Datatype in ftNonTexttypes then
    Result := 0
  else
    case Datatype of
      ftstring, ftFixedChar, ftWidestring, ftGUID:
        Result := FDBISAMTable.Fields[FieldNo - 1].Size;
      ftFloat, ftCurrency, ftBCD,
        ftAutoInc, ftSmallInt, ftInteger, ftWord, ftLargeInt:
        Result := 20;
      ftDate, ftTime, ftDateTime:
        Result := 0;
      ftBoolean:
        Result := 0;
    end;
end;

function TlicgDBISAMTable.FieldNo(const FieldName: string): integer;
var
  Field: TField;
begin
  Field := FDBISAMTable.FindField(FieldName);
  if Field = nil then
    Result := 0
  else
    Result := Field.Index + 1;
end;

function TlicgDBISAMTable.FieldType(FieldNo: integer): char;
var
  Datatype: TFieldType;
begin
  Datatype := FDBISAMTable.Fields[FieldNo - 1].Datatype;
  if Datatype in ftNonTexttypes then
  begin
    case DataType of
      ftMemo, ftFmtMemo: Result := 'M';
      ftGraphic: Result := 'G';
      ftTypedBinary: Result := 'B';
    end;
  end
  else
    case Datatype of
      ftstring, ftFixedChar,
        ftWidestring, ftGUID:
        Result := 'C';
      ftFloat, ftCurrency, ftBCD,
        ftAutoInc, ftSmallInt, ftInteger, ftWord, ftLargeInt:
        Result := 'N';
      ftDate, ftTime, ftDateTime:
        Result := 'D';
      ftBoolean:
        Result := 'L';
    end;
end;

function TlicgDBISAMTable.Find(const ss: string; IsExact, IsNear: boolean): boolean;
begin
  result := FDBISAMTable.FindKey([ss]); // findkey used
end;

function TlicgDBISAMTable.FloatGet(const Fieldname: string): Double;
begin
  result := FDBISAMTable.FieldByName(FieldName).Asfloat;
end;

function TlicgDBISAMTable.FloatGetN(FieldNo: Integer): Double;
begin
  result := FDBISAMTable.Fields[FieldNo - 1].Asfloat;
end;

function TlicgDBISAMTable.IndexCount: integer;
begin
  result := FDBISAMTable.IndexDefs.Count;
end;

function TlicgDBISAMTable.IndexAscending(Value: integer): boolean;
begin
  Result := not (ixDescending in FDBISAMTable.IndexDefs[Value].Options);
end;

function TlicgDBISAMTable.Index(const INames, Tag: string): integer;
begin
  // nothing to do here
end;

function TlicgDBISAMTable.IndexUnique(Value: integer): boolean;
begin
  Result := ixUnique in FDBISAMTable.IndexDefs[Value].Options;
end;

function TlicgDBISAMTable.IndexExpression(Value: integer): string;
begin
  result := FDBISAMTable.IndexDefs[Value].FieldExpression;
end;

function TlicgDBISAMTable.IndexTagName(Value: integer): string;
begin
  result := FDBISAMTable.IndexDefs[Value].Name;
end;

function TlicgDBISAMTable.IndexFilter(Value: integer): string;
begin
  result := '';
end;

function TlicgDBISAMTable.IntegerGet(const FieldName: string): Integer;
begin
  result := FDBISAMTable.FieldByName(Fieldname).AsInteger;
end;

function TlicgDBISAMTable.IntegerGetN(FieldNo: integer): Integer;
begin
  result := FDBISAMTable.Fields[FieldNo - 1].AsInteger;
end;

function TlicgDBISAMTable.LogicGet(const FieldName: string): Boolean;
begin
  result := FDBISAMTable.FieldByName(FieldName).AsBoolean;
end;

function TlicgDBISAMTable.LogicGetN(FieldNo: integer): Boolean;
begin
  result := FDBISAMTable.Fields[FieldNo - 1].AsBoolean;
end;

procedure TlicgDBISAMTable.MemoSave(const FieldName: string; Stream: TStream);
begin
  MemoSaveN(FDBISAMTable.FieldByname(FieldName).Index + 1, Stream);
end;

procedure TlicgDBISAMTable.MemoSaveN(FieldNo: integer; Stream: TStream);
begin
  stream.Position := 0;
  (FDBISAMTable.Fields[FieldNo - 1] as TBlobField).LoadFromStream(stream);
end;

function TlicgDBISAMTable.MemoSize(const FieldName: string): Integer;
begin
  result := (FDBISAMTable.FieldByName(FieldName) as TBlobField).BlobSize;
end;

function TlicgDBISAMTable.MemoSizeN(FieldNo: integer): Integer;
begin
  result := (FDBISAMTable.Fields[FieldNo - 1] as TBlobField).BlobSize;
end;

function TlicgDBISAMTable.RecordCount: Integer;
begin
  result := FDBISAMTable.PhysicalRecordCount;
end;

function TlicgDBISAMTable.StringGet(const FieldName: string): string;
begin
  result := FDBISAMTable.FieldByname(FieldName).AsString;
end;

function TlicgDBISAMTable.StringGetN(FieldNo: integer): string;
begin
  result := FDBISAMTable.Fields[FieldNo - 1].AsString;
end;

procedure TlicgDBISAMTable.DatePut(const FieldName: string; value: TDateTime);
begin
  FDBISAMTable.FieldByName(FieldName).AsDateTime := value;
end;

procedure TlicgDBISAMTable.DatePutN(FieldNo: integer; value: TDateTime);
begin
  FDBISAMTable.Fields[FieldNo - 1].AsDateTime := value;
end;

procedure TlicgDBISAMTable.Delete;
begin
  // only mark as deleted
  FDBISAMTable.Edit;
  FDBISAMTable.FieldByName('DELETED').AsBoolean := True;
  FDBISAMTable.Post;
end;

procedure TlicgDBISAMTable.Edit;
begin
  FDBISAMTable.Edit;
end;

procedure TlicgDBISAMTable.FieldPut(const FieldName, Value: string);
begin
  FDBISAMTable.FieldByName(FieldName).AsString := Value;
end;

procedure TlicgDBISAMTable.FieldPutN(FieldNo: integer; const Value: string);
begin
  FDBISAMTable.Fields[Fieldno - 1].Asstring := value;
end;

procedure TlicgDBISAMTable.First;
begin
  FDBISAMTable.First;
end;

procedure TlicgDBISAMTable.FloatPut(const FieldName: string; const Value: Double);
begin
  FDBISAMTable.Fieldbyname(Fieldname).AsFloat := value;
end;

procedure TlicgDBISAMTable.FloatPutN(FieldNo: integer; const Value: Double);
begin
  FDBISAMTable.Fields[FieldNo - 1].AsFloat := value;
end;

procedure TlicgDBISAMTable.FlushDB;
begin
  FDBISAMTable.FlushBuffers;
end;

procedure TlicgDBISAMTable.Go(n: Integer);
begin
  FDBISAMTable.IndexName := '';
  if not FDBISAMTable.FindKey([n]) then
    LicadGisError('Record not found !');
end;

procedure TlicgDBISAMTable.IndexOn(const IName, tag, keyexp, forexp: string;
  uniq: TlicgIndexUnique; ascnd: TlicgSortStatus);
var
  IndexOptions: TIndexOptions;
begin
  { tag receives the name of the new index
    keyexp is a semi-colon delimited list of fields to index on
    ex.:
    IndexOn('','FullName','LAST;FIRST','',iuUnique,ssDescending);}
  IndexOptions := [];
  if uniq = iuUnique then
    IndexOptions := IndexOptions + [ixUnique];
  if ascnd = ssDescending then
    IndexOptions := IndexOptions + [ixDescending];
  FDBISAMTable.AddIndex(tag, keyexp, IndexOptions);
end;

procedure TlicgDBISAMTable.IntegerPut(const Fieldname: string; Value: Integer);
begin
  FDBISAMTable.FieldByname(Fieldname).Asinteger := value;
end;

procedure TlicgDBISAMTable.IntegerPutN(FieldNo: integer; Value: Integer);
begin
  FDBISAMTable.Fields[Fieldno - 1].AsInteger := value;
end;

procedure TlicgDBISAMTable.Last;
begin
  FDBISAMTable.Last;
end;

procedure TlicgDBISAMTable.LogicPut(const fieldname: string; value: boolean);
begin
  FDBISAMTable.FieldByname(Fieldname).asboolean := value;
end;

procedure TlicgDBISAMTable.LogicPutN(fieldno: integer; value: boolean);
begin
  FDBISAMTable.fields[fieldno - 1].asboolean := value;
end;

procedure TlicgDBISAMTable.MemoLoad(const fieldname: string; Stream: TStream);
var
  field: TField;
begin
  field := FDBISAMTable.FindField(Fieldname);
  if field = nil then
    Exit;
  MemoLoadN(field.index + 1, Stream);
end;

procedure TlicgDBISAMTable.MemoLoadN(fieldno: integer; Stream: TStream);
begin
  (FDBISAMTable.Fields[fieldno - 1] as TBlobfield).SaveToStream(stream);
  stream.seek(0, 0);
end;

procedure TlicgDBISAMTable.Next;
begin
  FDBISAMTable.Next;
end;

procedure TlicgDBISAMTable.Pack;
begin
  FDBISAMTable.First;
  while not FDBISAMTable.Eof do
  begin
    if FDBISAMTable.FieldByName('DELETED').AsBoolean then
      FDBISAMTable.Delete
    else
      FDBISAMTable.Next;
  end;
end;

procedure TlicgDBISAMTable.Post;
begin
  FDBISAMTable.Post;
end;

procedure TlicgDBISAMTable.Prior;
begin
  FDBISAMTable.Prior;
end;

procedure TlicgDBISAMTable.Recall;
begin
  if FDBISAMTable.FieldByname('DELETED').AsBoolean then
  begin
    FDBISAMTable.Edit;
    FDBISAMTable.FieldByname('DELETED').AsBoolean := False;
    FDBISAMTable.Post;
  end;
end;

procedure TlicgDBISAMTable.Refresh;
begin
  FDBISAMTable.Refresh;
end;

procedure TlicgDBISAMTable.Reindex;
begin
  // nothing to do
end;

procedure TlicgDBISAMTable.SetTagTo(const TName: string);
begin
  FDBISAMTable.IndexName := TName;
end;

procedure TlicgDBISAMTable.SetUseDeleted(tf: boolean);
begin
  // nothing to do
end;

procedure TlicgDBISAMTable.StringPut(const fieldname, value: string);
begin
  FDBISAMTable.FieldByname(fieldname).Asstring := value;
end;

procedure TlicgDBISAMTable.StringPutN(fieldno: integer; const value: string);
begin
  FDBISAMTable.Fields[Fieldno - 1].Asstring := value;
end;

procedure TlicgDBISAMTable.Zap;
begin
  FDBISAMTable.EmptyTable;
end;

end.
