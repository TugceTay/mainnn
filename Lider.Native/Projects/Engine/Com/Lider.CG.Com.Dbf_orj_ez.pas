unit EZIMPL;
{$I EZ_FLAG.INC}

interface

uses
  SysUtils, Windows, Classes, Vcl.Graphics, EzBase, EzBaseGIS
{$IFNDEF NATIVEDLL}
  , dbf
{$ENDIF}
  ;

type

  // This class is used for reading dBASE .DBF files

  TEzNATIVEDbfTable = class(TEzBaseTable)
  private
{$IFDEF NATIVEDLL}
    FDBFHandle: Integer;
{$ELSE}
    FDbf: TDbf;
{$ENDIF}
  protected
    function GetActive: boolean; override;
    procedure SetActive(Value: boolean); override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(Gis: TEzBaseGIS; const FName: string;
      ReadWrite, Shared: boolean); override;
    destructor Destroy; override;
    procedure Append(NewRecno: Integer); override;
    function Bof: Boolean; override;
    function Eof: Boolean; override;
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
    function FloatGet(const FieldName: string): Double; override;
    function FloatGetN(FieldNo: Integer): Double; override;
    function IndexCount: integer; override;
    function IndexAscending(Value: integer): boolean; override;
    function Index(const INames, Tag: string): integer; override;
    function IndexUnique(Value: integer): boolean; override;
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
    function IndexExpression(Value: integer): string; override;
    procedure IndexOn(const IName, tag, keyexp, forexp: string;
      uniq: TEzIndexUnique; ascnd: TEzSortStatus); override;
    procedure IntegerPut(const FieldName: string; Value: Integer); override;
    procedure IntegerPutN(FieldNo: integer; Value: Integer); override;
    procedure Last; override;
    procedure LogicPut(const FieldName: string; value: boolean); override;
    procedure LogicPutN(fieldno: integer; value: boolean); override;
    procedure MemoLoad(const FieldName: string; Stream: TStream); override;
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
    procedure StringPut(const FieldName, value: string); override;
    procedure StringPutN(fieldno: integer; const value: string); override;
    procedure Zap; override;
    function DBCreateTable(const fname: string; AFieldList: TStringList): boolean; override;
    function DBTableExists(const TableName: string): Boolean; override;
    function DBDropTable(const TableName: string): Boolean; override;
    function DBDropIndex(const TableName: string): Boolean; override;
    function DBRenameTable(const Source, Target: string): Boolean; override;
  end;

{$IFDEF NATIVEDLL}
  TEzDBFTypes = (dtClipper, dtDBaseIII, dtDBaseIV, dtFoxPro2);
{$ENDIF}

function GetDesktopBaseTableClass: TlicgBaseTableClass;

function CreateAndOpenTable(GIS: TlicgBaseGIS; const FileName: string; ReadWrite, Shared: Boolean): TlicgBaseTable;
function CreateTable(GIS: TlicgBaseGIS): TlicgBaseTable;

implementation

uses
  Vcl.Forms, licgBasicEnts, Db, licgSystem, licgConsts
{$IFDEF FLASHFILER_DB}
  , licgBTff
{$ENDIF}
{$IFDEF DBISAM_DB}
  , licgBTDbisam
{$ENDIF}
{$IFDEF DATASET_PROVIDER}
  , licgBTDSProv
{$ENDIF}
{$IFDEF BORLAND_BDE}
  , licgBTBde
{$ENDIF}
{$IFDEF HALCYON_DB}
  , licgBTHalcyon
{$ENDIF}
{$IFDEF ACCESS_DB}
  , licgAccess2000
{$ENDIF}
  ;

function CreateAndOpenTable(GIS: TlicgBaseGIS; const FileName: string;
  ReadWrite, Shared: Boolean): TlicgBaseTable;
begin
  Result := licgBaseGIS.BaseTableClass.Create(GIS, FileName, ReadWrite, Shared);
end;

function CreateTable(GIS: TlicgBaseGIS): TlicgBaseTable;
begin
  Result := licgBaseGIS.BaseTableClass.CreateNoOpen(GIS);
end;

function TlicgNATIVEDbfTable.DBDropIndex(const TableName: string): Boolean;
begin
  Result := true;
  SysUtils.DeleteFile(ChangeFileExt(TableName, '.cdx'));
end;

function TlicgNATIVEDbfTable.DBDropTable(const TableName: string): Boolean;
begin
  SysUtils.DeleteFile(Tablename + '.dbf');
  SysUtils.DeleteFile(Tablename + '.mdx');
  SysUtils.DeleteFile(Tablename + '.dbt');
  Result := true;
end;

function TlicgNATIVEDbfTable.DBRenameTable(const Source, Target: string): Boolean;
begin
  if FileExists(Source + '.dbf') then
    SysUtils.RenameFile(Source + '.dbf', Target + '.dbf');
  if FileExists(Source + '.mdx') then
    SysUtils.RenameFile(Source + '.mdx', Target + '.mdx');
  if FileExists(Source + '.dbt') then
    SysUtils.RenameFile(Source + '.dbt', Target + '.dbt');
  Result := true;
end;

function TlicgNATIVEDbfTable.DBTableExists(const TableName: string): Boolean;
begin
  Result := FileExists(ChangeFileExt(TableName, '.dbf'));
end;

function TlicgNATIVEDbfTable.DBCreateTable(const fname: string;
  AFieldList: TStringList): boolean;
{$IFNDEF NATIVEDLL}
var
  s: string;
  v: boolean;
  i: integer;
  p: integer;
  fs: string;
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

{$ENDIF}
begin
  { create a DBF table }
  with TDbf.Create(nil) do
  try
    TableName := ChangeFileExt(fname, '.dbf');
    with FieldDefs do
    begin
      Clear;
      for I := 0 to AFieldList.count - 1 do
      begin
        s := AFieldList[I];
        LoadField;
        if not v then
          licgGisError(SErrWrongField);

        case ft[1] of
          'C':
            Add(fs, ftString, fl, False);
          'F', 'N':
            if fd = 0 then
              Add(fs, ftInteger, 0, False)
            else
              Add(fs, ftFloat, 0, False);
          'M', 'G', 'B':
            Add(fs, ftMemo, 0, False);
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
    CreateTable;
  finally
    Free;
  end;
  Result := True;
end;

constructor TlicgNATIVEDbfTable.Create(Gis: TlicgBaseGIS; const FName: string;
  ReadWrite, Shared: boolean);
begin
  inherited Create(Gis, FName, ReadWrite, Shared);
  FDbf := TDbf.Create(nil);
  with FDbf do
  begin
    TableName := ChangeFileExt(fname, '.dbf');
    ReadOnly := not ReadWrite;
    Open;
  end;
end;

destructor TlicgNATIVEDbfTable.Destroy;
begin
  FDbf.Free;
  inherited Destroy;
end;

function TlicgNATIVEDbfTable.GetActive: boolean;
begin
  result := FDbf.Active;
end;

procedure TlicgNATIVEDbfTable.SetActive(Value: boolean);
begin
  FDbf.Active := value;
end;

function TlicgNATIVEDbfTable.GetRecNo: Integer;
begin
  result := FDbf.RecNo;
end;

procedure TlicgNATIVEDbfTable.SetRecNo(Value: Integer);
begin
  FDbf.Recno := Value;
end;

procedure TlicgNATIVEDbfTable.Append(NewRecno: Integer);
var
  Field: TField;
begin
  FDbf.Insert;
  Field := FDbf.FindField('UID');
  if Field <> nil then
    Field.AsInteger := NewRecno;
  FDbf.Post;
end;

function TlicgNATIVEDbfTable.BOF: Boolean;
begin
  result := FDbf.Eof;
end;

function TlicgNATIVEDbfTable.EOF: Boolean;
begin
  result := FDbf.Eof;
end;

function TlicgNATIVEDbfTable.DateGet(const FieldName: string): TDateTime;
begin
  Result := FDbf.FieldByName(FieldName).AsDateTime;
end;

function TlicgNATIVEDbfTable.DateGetN(FieldNo: integer): TDateTime;
begin
  result := FDbf.Fields[FieldNo - 1].AsdateTime;
end;

function TlicgNATIVEDbfTable.Deleted: Boolean;
begin
  result := FDbf.IsDeleted;
end;

function TlicgNATIVEDbfTable.Field(FieldNo: integer): string;
begin
  result := FDbf.Fields[FieldNo - 1].FieldName;
end;

function TlicgNATIVEDbfTable.FieldCount: integer;
begin
  result := FDbf.Fields.Count;
end;

function TlicgNATIVEDbfTable.FieldDec(FieldNo: integer): integer;
var
  Datatype: TFieldType;
begin
  Datatype := FDbf.Fields[FieldNo - 1].Datatype;
  if Datatype in ftNonTexttypes then
    Result := 0
  else
    case Datatype of
      ftstring{$IFDEF LEVEL4}, ftFixedChar,
      ftWidestring{$ENDIF}
{$IFDEF LEVEL5}, ftGUID{$ENDIF}:
        Result := 0;
      ftBCD:
        Result := FDbf.Fields[FieldNo - 1].Size;
      ftFloat, ftCurrency,
        ftAutoInc, ftSmallInt, ftInteger, ftWord
{$IFNDEF LEVEL3}, ftLargeInt{$ENDIF}:
        Result := 0;
      ftDate, ftTime, ftDateTime:
        Result := 0;
      ftBoolean:
        Result := 0;
    else
      Result := 0;
    end;
end;

function TlicgNATIVEDbfTable.FieldGet(const FieldName: string): string;
begin
  result := FDbf.FieldByName(FieldName).AsString;
end;

function TlicgNATIVEDbfTable.FieldGetN(FieldNo: integer): string;
begin
  result := FDbf.Fields[FieldNo - 1].AsString;
end;

function TlicgNATIVEDbfTable.FieldLen(FieldNo: integer): integer;
var
  Datatype: TFieldType;
begin
  Datatype := FDbf.Fields[FieldNo - 1].Datatype;
  if Datatype in ftNonTexttypes then
    Result := 0
  else
    case Datatype of
      ftstring{$IFDEF LEVEL4}, ftFixedChar,
      ftWidestring{$ENDIF}
{$IFDEF LEVEL5}, ftGUID{$ENDIF}:
        Result := FDbf.Fields[FieldNo - 1].Size;
      ftFloat, ftCurrency, ftBCD,
        ftAutoInc, ftSmallInt, ftInteger, ftWord
{$IFNDEF LEVEL3}, ftLargeInt{$ENDIF}:
        Result := 20;
      ftDate, ftTime, ftDateTime:
        Result := 0;
      ftBoolean:
        Result := 0;
    else
      Result := 0;
    end;
end;

function TlicgNATIVEDbfTable.FieldNo(const FieldName: string): integer;
var
  Field: TField;
begin
  Field := FDbf.FindField(FieldName);
  if Field = nil then
    Result := 0
  else
    Result := Field.Index + 1;
end;

function TlicgNATIVEDbfTable.FieldType(FieldNo: integer): char;
var
  Datatype: TFieldType;
begin
  Datatype := FDbf.Fields[FieldNo - 1].Datatype;
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
      ftstring{$IFDEF LEVEL4}, ftFixedChar,
      ftWidestring{$ENDIF}
{$IFDEF LEVEL5}, ftGUID{$ENDIF}:
        Result := 'C';
      ftFloat, ftCurrency, ftBCD,
        ftAutoInc, ftSmallInt, ftInteger, ftWord
{$IFNDEF LEVEL3}, ftLargeInt{$ENDIF}:
        Result := 'N';
      ftDate, ftTime, ftDateTime:
        Result := 'D';
      ftBoolean:
        Result := 'L';
    else
      Result := 'C';
    end;
end;

function TlicgNATIVEDbfTable.Find(const ss: string; IsExact, IsNear: boolean): boolean;
begin
  result := false; // not yet implemented
end;

function TlicgNATIVEDbfTable.FloatGet(const FieldName: string): Double;
begin
  result := FDbf.FieldByName(FieldName).Asfloat;
end;

function TlicgNATIVEDbfTable.FloatGetN(FieldNo: Integer): Double;
begin
  result := FDbf.Fields[FieldNo - 1].Asfloat;
end;

function TlicgNATIVEDbfTable.IndexCount: integer;
begin
  result := 0;
end;

function TlicgNATIVEDbfTable.IndexAscending(Value: integer): boolean;
begin
  Result := true;
end;

function TlicgNATIVEDbfTable.Index(const INames, Tag: string): integer;
begin
  result := 0;
  if FileExists(INames) then
  begin
    FDbf.OpenIndexFile(INames);
    FDbf.IndexName := Tag;
  end;
end;

function TlicgNATIVEDbfTable.IndexUnique(Value: integer): boolean;
begin
  result := true;
end;

function TlicgNATIVEDbfTable.IndexTagName(Value: integer): string;
begin
  result := '';
end;

function TlicgNATIVEDbfTable.IndexFilter(Value: integer): string;
begin
  result := '';
end;

function TlicgNATIVEDbfTable.IntegerGet(const FieldName: string): Integer;
begin
  result := FDbf.FieldByName(Fieldname).AsInteger;
end;

function TlicgNATIVEDbfTable.IntegerGetN(FieldNo: integer): Integer;
begin
  result := FDbf.Fields[FieldNo - 1].AsInteger;
end;

function TlicgNATIVEDbfTable.LogicGet(const FieldName: string): Boolean;
begin
  result := FDbf.FieldByName(FieldName).AsBoolean;
end;

function TlicgNATIVEDbfTable.LogicGetN(FieldNo: integer): Boolean;
begin
  result := FDbf.Fields[FieldNo - 1].AsBoolean;
end;

procedure TlicgNATIVEDbfTable.MemoSave(const FieldName: string; Stream: TStream);
begin
  Stream.Position := 0;
  (FDbf.FieldByname(FieldName) as TBlobField).SaveToStream(Stream);
end;

procedure TlicgNATIVEDbfTable.MemoSaveN(FieldNo: integer; Stream: TStream);
begin
  Stream.Position := 0;
  (FDbf.Fields[FieldNo - 1] as TBlobField).SaveToStream(Stream);
end;

function TlicgNATIVEDbfTable.MemoSize(const FieldName: string): Integer;
begin
  result := (FDbf.FieldByName(FieldName) as TBlobField).BlobSize;
end;

function TlicgNATIVEDbfTable.MemoSizeN(FieldNo: integer): Integer;
begin
  result := (FDbf.Fields[FieldNo - 1] as TBlobField).BlobSize;
end;

function TlicgNATIVEDbfTable.RecordCount: Integer;
begin
  result := FDbf.RecordCount;
end;

function TlicgNATIVEDbfTable.StringGet(const FieldName: string): string;
begin
  result := FDbf.FieldByname(FieldName).AsString;
end;

function TlicgNATIVEDbfTable.StringGetN(FieldNo: integer): string;
begin
  result := FDbf.Fields[FieldNo - 1].AsString;
end;

procedure TlicgNATIVEDbfTable.DatePut(const FieldName: string; value: TDateTime);
begin
  FDbf.FieldByName(FieldName).AsDateTime := value;
end;

procedure TlicgNATIVEDbfTable.DatePutN(FieldNo: integer; value: TDateTime);
begin
  FDbf.Fields[FieldNo - 1].AsDateTime := value;
end;

procedure TlicgNATIVEDbfTable.Delete;
begin
  FDbf.Delete
end;

procedure TlicgNATIVEDbfTable.Edit;
begin
  FDbf.Edit;
end;

procedure TlicgNATIVEDbfTable.FieldPut(const FieldName, Value: string);
begin
  FDbf.FieldByName(FieldName).AsString := Value;
end;

procedure TlicgNATIVEDbfTable.FieldPutN(FieldNo: integer; const Value: string);
begin
  FDbf.Fields[Fieldno - 1].Asstring := value;
end;

procedure TlicgNATIVEDbfTable.First;
begin
  FDbf.First;
end;

procedure TlicgNATIVEDbfTable.FloatPut(const FieldName: string; const Value: Double);
begin
  FDbf.Fieldbyname(Fieldname).AsFloat := value;
end;

procedure TlicgNATIVEDbfTable.FloatPutN(FieldNo: integer; const Value: Double);
begin
  FDbf.Fields[FieldNo - 1].AsFloat := value;
end;

procedure TlicgNATIVEDbfTable.FlushDB;
begin
  //FDbf.FlushBuffers; ilker böyle idi.
end;

procedure TlicgNATIVEDbfTable.Go(n: Integer);
begin
  FDbf.Recno := n;
end;

function TlicgNATIVEDbfTable.IndexExpression(Value: integer): string;
begin
  result := '';
end;

procedure TlicgNATIVEDbfTable.IndexOn(const IName, tag, keyexp, forexp: string;
  uniq: TlicgIndexUnique; ascnd: TlicgSortStatus);
begin
  // how to do ?
end;

procedure TlicgNATIVEDbfTable.IntegerPut(const FieldName: string; Value: Integer);
begin
  FDbf.FieldByname(Fieldname).Asinteger := value;
end;

procedure TlicgNATIVEDbfTable.IntegerPutN(FieldNo: integer; Value: Integer);
begin
  FDbf.Fields[Fieldno - 1].AsInteger := value;
end;

procedure TlicgNATIVEDbfTable.Last;
begin
  FDbf.Last;
end;

procedure TlicgNATIVEDbfTable.LogicPut(const FieldName: string; value: boolean);
begin
  FDbf.FieldByname(Fieldname).asboolean := value;
end;

procedure TlicgNATIVEDbfTable.LogicPutN(fieldno: integer; value: boolean);
begin
  FDbf.fields[fieldno - 1].asboolean := value;
end;

procedure TlicgNATIVEDbfTable.MemoLoad(const FieldName: string; Stream: TStream);
var
  field: TField;
begin
  field := FDbf.FindField(Fieldname);
  if field = nil then
    Exit;
  MemoLoadN(field.index + 1, stream);
end;

procedure TlicgNATIVEDbfTable.MemoLoadN(fieldno: integer; Stream: TStream);
begin
  stream.seek(0, 0);
  (FDbf.Fields[fieldno - 1] as TBlobfield).SaveToStream(stream);
end;

procedure TlicgNATIVEDbfTable.Next;
begin
  FDbf.Next;
end;

procedure TlicgNATIVEDbfTable.Pack;
begin
  FDbf.PackTable;
end;

procedure TlicgNATIVEDbfTable.Post;
begin
  FDbf.Post;
end;

procedure TlicgNATIVEDbfTable.Prior;
begin
  FDbf.Prior;
end;

procedure TlicgNATIVEDbfTable.Recall;
begin
  FDbf.Undelete;
end;

procedure TlicgNATIVEDbfTable.Refresh;
begin
  FDbf.Refresh;
end;

procedure TlicgNATIVEDbfTable.Reindex;
begin
  // ilker yok birþey 
end;

procedure TlicgNATIVEDbfTable.SetTagTo(const TName: string);
begin
  FDbf.IndexName := TName;
end;

procedure TlicgNATIVEDbfTable.SetUseDeleted(tf: boolean);
begin
  FDbf.ShowDeleted := tf;
end;

procedure TlicgNATIVEDbfTable.StringPut(const FieldName, value: string);
begin
  FDbf.FieldByname(fieldname).Asstring := value;
end;

procedure TlicgNATIVEDbfTable.StringPutN(fieldno: integer; const value: string);
begin
  FDbf.Fields[Fieldno - 1].Asstring := value;
end;

procedure TlicgNATIVEDbfTable.Zap;
begin
  FDbf.First;
  while not FDbf.Eof do
    FDbf.Delete;
  FDbf.PackTable;
end;

function GetDesktopBaseTableClass: TlicgBaseTableClass;
begin
{$IFDEF DATASET_PROVIDER}

  Result := TlicgProviderTable;

{$ENDIF}

{$IFDEF NATIVE_DB}
  Result := TlicgNATIVEDbfTable;
{$ENDIF}

{$IFDEF HALCYON_DB}
  Result := TlicgHalcyonTable;
{$ENDIF}

{$IFDEF FLASHFILER_DB}
  Result := TlicgffTable;
{$ENDIF}

{$IFDEF DBISAM_DB}
  Result := TlicgDBISAMTable;
{$ENDIF}

{$IFDEF BORLAND_BDE}
  Result := TlicgBDETable;
{$ENDIF}

{$IFDEF ACCESS_DB}
  Result := TlicgADOXTable;
{$ENDIF}

end;

initialization

  licgBaseGIS.BaseTableClass := GetDesktopBaseTableClass;

{$IFDEF NATIVEDLL}
finalization
  if DLLLoaded then
    FreeLibrary(DLLHandle);
{$ENDIF}

end.
