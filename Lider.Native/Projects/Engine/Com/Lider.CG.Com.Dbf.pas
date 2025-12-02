unit Lider.CG.Com.Dbf;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  Graphics,
  SysUtils,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  dbf;
type
  // This class is used for reading dBASE .DBF files
  TlicgDbfTable = class(TlicgBaseTable)
  private
    FDbf: TDbf;
  protected
    function GetActive: Boolean; override;
    procedure SetActive(Value: Boolean); override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(Gis: TlicgBaseGIS; const FName: string;
      ReadWrite, Shared: Boolean); override;
    destructor Destroy; override;
    procedure Append(NewRecno: Integer); override;
    function Bof: Boolean; override;
    function Eof: Boolean; override;
    function DateGet(const FieldName: string): TDateTime; override;
    function DateGetN(FieldNo: Integer): TDateTime; override;
    function Deleted: Boolean; override;
    function Field(FieldNo: Integer): string; override;
    function FieldCount: Integer; override;
    function FieldDec(FieldNo: Integer): Integer; override;
    function FieldGet(const FieldName: string): string; override;
    function FieldGetN(FieldNo: Integer): string; override;
    function FieldLen(FieldNo: Integer): Integer; override;
    function FieldNo(const FieldName: string): Integer; override;
    function FieldType(FieldNo: Integer): AnsiChar; override;
    function Find(const ss: string; IsExact, IsNear: Boolean): Boolean; override;
    function FloatGet(const FieldName: string): Double; override;
    function FloatGetN(FieldNo: Integer): Double; override;
    function IndexCount: Integer; override;
    function IndexAscending(Value: Integer): Boolean; override;
    function Index(const INames, Tag: string): Integer; override;
    function IndexUnique(Value: Integer): Boolean; override;
    function IndexTagName(Value: Integer): string; override;
    function IndexFilter(Value: Integer): string; override;
    function IntegerGet(const FieldName: string): Integer; override;
    function IntegerGetN(FieldNo: Integer): Integer; override;
    function LogicGet(const FieldName: string): Boolean; override;
    function LogicGetN(FieldNo: Integer): Boolean; override;
    procedure MemoSave(const FieldName: string; Stream: TStream); override;
    procedure MemoSaveN(FieldNo: Integer; Stream: TStream); override;
    function MemoSize(const FieldName: string): Integer; override;
    function MemoSizeN(FieldNo: Integer): Integer; override;
    function RecordCount: Integer; override;
    function StringGet(const FieldName: string): string; override;
    function StringGetN(FieldNo: Integer): string; override;
    //Procedure CopyStructure( Const FileName, APassword: String ); Override;
    //Procedure CopyTo( Const FileName, APassword: String ); Override;
    procedure DatePut(const FieldName: string; value: TDateTime); override;
    procedure DatePutN(FieldNo: Integer; value: TDateTime); override;
    procedure Delete; override;
    procedure Edit; override;
    procedure FieldPut(const FieldName, Value: string); override;
    procedure FieldPutN(FieldNo: Integer; const Value: string); override;
    procedure First; override;
    procedure FloatPut(const FieldName: string; const Value: Double); override;
    procedure FloatPutN(FieldNo: Integer; const Value: Double); override;
    procedure FlushDB; override;
    procedure Go(n: Integer); override;
    function IndexExpression(Value: Integer): string; override;
    procedure IndexOn(const IName, tag, keyexp, forexp: string;
      uniq: TlicgIndexUnique; ascnd: TlicgSortStatus); override;
    procedure IntegerPut(const FieldName: string; Value: Integer); override;
    procedure IntegerPutN(FieldNo: Integer; Value: Integer); override;
    procedure Last; override;
    procedure LogicPut(const FieldName: string; value: Boolean); override;
    procedure LogicPutN(fieldno: Integer; value: Boolean); override;
    procedure MemoLoad(const FieldName: string; Stream: TStream); override;
    procedure MemoLoadN(fieldno: Integer; Stream: TStream); override;
    procedure Next; override;
    procedure Pack; override;
    procedure Post; override;
    procedure Prior; override;
    procedure Recall; override;
    procedure Refresh; override;
    procedure Reindex; override;
    procedure SetTagTo(const TName: string); override;
    procedure SetUseDeleted(tf: Boolean); override;
    procedure StringPut(const FieldName, value: string); override;
    procedure StringPutN(fieldno: Integer; const value: string); override;
    procedure Zap; override;
    function DBCreateTable(const fname: string; AFieldList: TStringList): Boolean; override;
    function DBTableExists(const TableName: string): Boolean; override;
    function DBDropTable(const TableName: string): Boolean; override;
    function DBDropIndex(const TableName: string): Boolean; override;
    function DBRenameTable(const Source, Target: string): Boolean; override;
    procedure AssignFrom(Dataset: TlicgBaseTable; SrcFieldNo, DstFieldNo: Integer); override;
  end;
  
function GetDesktopBaseTableClass: TlicgBaseTableClass;

function CreateAndOpenTable(GIS: TlicgBaseGIS; const FileName: string; ReadWrite, Shared: Boolean): TlicgBaseTable;

function CreateTable(GIS: TlicgBaseGIS): TlicgBaseTable;

implementation

uses
  Forms,
  Db,
  Lider.CG.Com.System,
  Lider.CG.Com.Consts;

function CreateAndOpenTable(GIS: TlicgBaseGIS; const FileName: string; ReadWrite,
  Shared: Boolean): TlicgBaseTable;
begin
  Result := GIS.BaseTableClass.Create(GIS, FileName, ReadWrite, Shared);
end;

function CreateTable(GIS: TlicgBaseGIS): TlicgBaseTable;
begin
  Result := GIS.BaseTableClass.CreateNoOpen(GIS);
end;

function TlicgDbfTable.DBDropIndex(const TableName: string): Boolean;
begin
  Result := True;
  SysUtils.DeleteFile(ChangeFileExt(TableName, '.cdx'));
end;

function TlicgDbfTable.DBDropTable(const TableName: string): Boolean;
begin
  SysUtils.DeleteFile(Tablename + '.dbf');
  SysUtils.DeleteFile(Tablename + '.mdx');
  SysUtils.DeleteFile(Tablename + '.dbt');
  Result := True;
end;

function TlicgDbfTable.DBRenameTable(const Source, Target: string): Boolean;
begin
  if FileExists(Source + '.dbf') then
    SysUtils.RenameFile(Source + '.dbf', Target + '.dbf');
  if FileExists(Source + '.mdx') then
    SysUtils.RenameFile(Source + '.mdx', Target + '.mdx');
  if FileExists(Source + '.dbt') then
    SysUtils.RenameFile(Source + '.dbt', Target + '.dbt');
  Result := True;
end;

function TlicgDbfTable.DBTableExists(const TableName: string): Boolean;
begin
  Result := FileExists(ChangeFileExt(TableName, '.dbf'));
end;

function TlicgDbfTable.DBCreateTable(const fname: string; AFieldList: TStringList): Boolean;
var
  s: string;
  v: Boolean;
  i: Integer;
  p: Integer;
  fs: string;
  ft: string[1];
  fl: Integer;
  fd: Integer;

  procedure LoadField;
  begin
    v := True;
    p := Ansipos(';', s);
    fs := '';
    if p > 0 then
    begin
      fs := System.Copy(s, 1, pred(p));
      System.Delete(s, 1, p);
    end
    else
      v := False;

    p := Ansipos(';', s);
    ft := ' ';
    if p = 2 then
    begin
      ft := System.Copy(s, 1, 1);
      System.Delete(s, 1, p);
    end
    else
      v := False;

    p := Ansipos(';', s);
    fl := 0;
    if p > 0 then
    begin
      try
        fl := StrToInt(System.Copy(s, 1, pred(p)));
        System.Delete(s, 1, p);
      except
        on Exception do
          v := False;
      end;
    end
    else
      v := False;

    fd := 0;
    try
      fd := StrToInt(System.Copy(s, 1, 3));
    except
      on Exception do
        v := False;
    end;
  end;

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
          LicadGisError(SErrWrongField);

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

constructor TlicgDbfTable.Create(Gis: TlicgBaseGIS; const FName: string;
  ReadWrite, Shared: Boolean);
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

destructor TlicgDbfTable.Destroy;
begin
  FDbf.Free;
  inherited Destroy;
end;

function TlicgDbfTable.GetActive: Boolean;
begin
  Result := FDbf.Active;
end;

procedure TlicgDbfTable.SetActive(Value: Boolean);
begin
  FDbf.Active := value;
end;

function TlicgDbfTable.GetRecNo: Integer;
begin
  Result := FDbf.RecNo;
end;

procedure TlicgDbfTable.SetRecNo(Value: Integer);
begin
  FDbf.Recno := Value;
end;

procedure TlicgDbfTable.Append(NewRecno: Integer);
var
  Field: TField;
begin
  FDbf.Insert;
  Field := FDbf.FindField('UID');
  if Field <> nil then
    Field.AsInteger := NewRecno;
  FDbf.Post;
end;

function TlicgDbfTable.BOF: Boolean;
begin
  Result := FDbf.Eof;
end;

function TlicgDbfTable.EOF: Boolean;
begin
  Result := FDbf.Eof;
end;

function TlicgDbfTable.DateGet(const FieldName: string): TDateTime;
begin
  Result := FDbf.FieldByName(FieldName).AsDateTime;
end;

function TlicgDbfTable.DateGetN(FieldNo: Integer): TDateTime;
begin
  Result := FDbf.Fields[FieldNo - 1].AsdateTime;
end;

function TlicgDbfTable.Deleted: Boolean;
begin
  Result := FDbf.IsDeleted;
end;

function TlicgDbfTable.Field(FieldNo: Integer): string;
begin
  Result := FDbf.Fields[FieldNo - 1].FieldName;
end;

function TlicgDbfTable.FieldCount: Integer;
begin
  Result := FDbf.Fields.Count;
end;

function TlicgDbfTable.FieldDec(FieldNo: Integer): Integer;
begin
  Result := FDbf.FieldDefs[FieldNo - 1].Precision;  // ilker deðiþtirme
end;

function TlicgDbfTable.FieldGet(const FieldName: string): string;
begin
  Result := FDbf.FieldByName(FieldName).AsString;
end;

function TlicgDbfTable.FieldGetN(FieldNo: Integer): string;
begin
  Result := FDbf.Fields[FieldNo - 1].AsString;
end;

function TlicgDbfTable.FieldLen(FieldNo: Integer): Integer;
var
  Datatype: TFieldType;
begin
  Datatype := FDbf.Fields[FieldNo - 1].Datatype;
  if Datatype in ftNonTexttypes then
    Result := 0
  else
    case Datatype of
      ftString, ftFixedChar, ftWideString, ftGUID:
        Result := FDbf.Fields[FieldNo - 1].Size;
      ftFloat, ftCurrency, ftBCD, ftAutoInc, ftSmallInt, ftInteger, ftWord, ftLargeInt:
        Result := 20;
      ftDate, ftTime, ftDateTime:
        Result := 0;
      ftBoolean:
        Result := 0;
    else
      Result := 0;
    end;
end;

function TlicgDbfTable.FieldNo(const FieldName: string): Integer;
var
  Field: TField;
begin
  Field := FDbf.FindField(FieldName);
  if Field = nil then
    Result := 0
  else
    Result := Field.Index + 1;
end;

function TlicgDbfTable.FieldType(FieldNo: Integer): AnsiChar;
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
      ftString, ftFixedChar, ftWideString, ftGUID:
        Result := 'C';
      ftAutoInc, ftSmallInt, ftInteger, ftWord, ftLargeInt: // ilker ekleme
        Result := 'I';
      ftFloat, ftCurrency, ftBCD:
        Result := 'N';
      ftDate, ftTime, ftDateTime:
        Result := 'D';
      ftBoolean:
        Result := 'L';
    else
      Result := 'C';
    end;
end;

function TlicgDbfTable.Find(const ss: string; IsExact, IsNear: Boolean): Boolean;
begin
  Result := False; // not yet implemented
end;

function TlicgDbfTable.FloatGet(const FieldName: string): Double;
begin
  Result := FDbf.FieldByName(FieldName).Asfloat;
end;

function TlicgDbfTable.FloatGetN(FieldNo: Integer): Double;
begin
  Result := FDbf.Fields[FieldNo - 1].Asfloat;
end;

function TlicgDbfTable.IndexCount: Integer;
begin
  Result := 0;
end;

function TlicgDbfTable.IndexAscending(Value: Integer): Boolean;
begin
  Result := True;
end;

function TlicgDbfTable.Index(const INames, Tag: string): Integer;
begin
  Result := 0;
  if FileExists(INames) then
  begin
    FDbf.OpenIndexFile(INames);
    FDbf.IndexName := Tag;
  end;
end;

function TlicgDbfTable.IndexUnique(Value: Integer): Boolean;
begin
  Result := True;
end;

function TlicgDbfTable.IndexTagName(Value: Integer): string;
begin
  Result := '';
end;

function TlicgDbfTable.IndexFilter(Value: Integer): string;
begin
  Result := '';
end;

function TlicgDbfTable.IntegerGet(const FieldName: string): Integer;
begin
  Result := FDbf.FieldByName(Fieldname).AsInteger;
end;

function TlicgDbfTable.IntegerGetN(FieldNo: Integer): Integer;
begin
  Result := FDbf.Fields[FieldNo - 1].AsInteger;
end;

function TlicgDbfTable.LogicGet(const FieldName: string): Boolean;
begin
  Result := FDbf.FieldByName(FieldName).AsBoolean;
end;

function TlicgDbfTable.LogicGetN(FieldNo: Integer): Boolean;
begin
  Result := FDbf.Fields[FieldNo - 1].AsBoolean;
end;

procedure TlicgDbfTable.MemoSave(const FieldName: string; Stream: TStream);
begin
  Stream.Position := 0;
  (FDbf.FieldByname(FieldName) as TBlobField).SaveToStream(Stream);
end;

procedure TlicgDbfTable.MemoSaveN(FieldNo: Integer; Stream: TStream);
begin
  Stream.Position := 0;
  (FDbf.Fields[FieldNo - 1] as TBlobField).SaveToStream(Stream);
end;

function TlicgDbfTable.MemoSize(const FieldName: string): Integer;
begin
  Result := (FDbf.FieldByName(FieldName) as TBlobField).BlobSize;
end;

function TlicgDbfTable.MemoSizeN(FieldNo: Integer): Integer;
begin
  Result := (FDbf.Fields[FieldNo - 1] as TBlobField).BlobSize;
end;

function TlicgDbfTable.RecordCount: Integer;
begin
  Result := FDbf.RecordCount;
end;

function TlicgDbfTable.StringGet(const FieldName: string): string;
begin
  //Result := UTF8ToString(FDbf.FieldByname(FieldName).AsString); //ilker ekleme
  Result := FDbf.FieldByName(FieldName).AsString; //ilker ekleme
end;

function TlicgDbfTable.StringGetN(FieldNo: Integer): string;
begin
  //Result := UTF8ToString(FDbf.Fields[FieldNo - 1].AsString); //ilker ekleme
  Result := FDbf.Fields[FieldNo - 1].AsString; //ilker ekleme
end;

procedure TlicgDbfTable.DatePut(const FieldName: string; value: TDateTime);
begin
  FDbf.FieldByName(FieldName).AsDateTime := value;
end;

procedure TlicgDbfTable.DatePutN(FieldNo: Integer; value: TDateTime);
begin
  FDbf.Fields[FieldNo - 1].AsDateTime := value;
end;

procedure TlicgDbfTable.Delete;
begin
  FDbf.Delete
end;

procedure TlicgDbfTable.Edit;
begin
  FDbf.Edit;
end;

procedure TlicgDbfTable.FieldPut(const FieldName, Value: string);
begin
  FDbf.FieldByName(FieldName).AsString := Value; // ilker ekleme
end;

procedure TlicgDbfTable.FieldPutN(FieldNo: Integer; const Value: string);
begin
  FDbf.Fields[Fieldno - 1].Asstring := Value; // ilker ekleme
end;

procedure TlicgDbfTable.First;
begin
  FDbf.First;
end;

procedure TlicgDbfTable.FloatPut(const FieldName: string; const Value: Double);
begin
  FDbf.Fieldbyname(Fieldname).AsFloat := value;
end;

procedure TlicgDbfTable.FloatPutN(FieldNo: Integer; const Value: Double);
begin
  FDbf.Fields[FieldNo - 1].AsFloat := value;
end;

procedure TlicgDbfTable.FlushDB;
begin
  //FDbf.FlushBuffers; ilker böyle idi.
end;

procedure TlicgDbfTable.Go(n: Integer);
begin
  FDbf.Recno := n;
end;

function TlicgDbfTable.IndexExpression(Value: Integer): string;
begin
  Result := '';
end;

procedure TlicgDbfTable.IndexOn(const IName, tag, keyexp, forexp: string;
  uniq: TlicgIndexUnique; ascnd: TlicgSortStatus);
begin
  // how to do ?
end;

procedure TlicgDbfTable.IntegerPut(const FieldName: string; Value: Integer);
begin
  FDbf.FieldByname(Fieldname).AsInteger := value;
end;

procedure TlicgDbfTable.IntegerPutN(FieldNo: Integer; Value: Integer);
begin
  FDbf.Fields[Fieldno - 1].AsInteger := value;
end;

procedure TlicgDbfTable.Last;
begin
  FDbf.Last;
end;

procedure TlicgDbfTable.LogicPut(const FieldName: string; value: Boolean);
begin
  FDbf.FieldByname(Fieldname).asBoolean := value;
end;

procedure TlicgDbfTable.LogicPutN(fieldno: Integer; value: Boolean);
begin
  FDbf.fields[fieldno - 1].asBoolean := value;
end;

procedure TlicgDbfTable.MemoLoad(const FieldName: string; Stream: TStream);
var
  field: TField;
begin
  field := FDbf.FindField(Fieldname);
  if field = nil then
    Exit;
  MemoLoadN(field.index + 1, stream);
end;

procedure TlicgDbfTable.MemoLoadN(fieldno: Integer; Stream: TStream);
begin
  stream.seek(0, 0);
  (FDbf.Fields[fieldno - 1] as TBlobfield).SaveToStream(stream);
end;

procedure TlicgDbfTable.Next;
begin
  FDbf.Next;
end;

procedure TlicgDbfTable.Pack;
begin
  FDbf.PackTable;
end;

procedure TlicgDbfTable.Post;
begin
  FDbf.Post;
end;

procedure TlicgDbfTable.Prior;
begin
  FDbf.Prior;
end;

procedure TlicgDbfTable.Recall;
begin
  FDbf.Undelete;
end;

procedure TlicgDbfTable.Refresh;
begin
  FDbf.Refresh;
end;

procedure TlicgDbfTable.Reindex;
begin
  // ilker yok birþey 
end;

procedure TlicgDbfTable.SetTagTo(const TName: string);
begin
  FDbf.IndexName := TName;
end;

procedure TlicgDbfTable.SetUseDeleted(tf: Boolean);
begin
  FDbf.ShowDeleted := tf;
end;

procedure TlicgDbfTable.StringPut(const FieldName, value: string);
begin
  FDbf.FieldByname(fieldname).Asstring := value;
end;

procedure TlicgDbfTable.StringPutN(fieldno: Integer; const value: string);
begin
  FDbf.Fields[Fieldno - 1].Asstring := value;
end;

procedure TlicgDbfTable.Zap;
begin
  FDbf.First;
  while not FDbf.Eof do
    FDbf.Delete;
  FDbf.PackTable;
end;

procedure TlicgDbfTable.AssignFrom(Dataset: TlicgBaseTable; SrcFieldNo, DstFieldNo: Integer);
var
  SrcTyp, DstTyp: AnsiChar;
  BlobLen: Integer;
  Stream: TStream;
  E: Integer;
  L: Double;
begin
  if (SrcFieldNo = 0) or (DstFieldNo = 0) then Exit;
  SrcTyp := Dataset.FieldType(SrcFieldno);
  DstTyp := Self.FieldType(DstFieldno);
  case DstTyp of
    'M', 'B', 'G':
      begin
        if not (SrcTyp in ['M', 'B', 'G']) then
          Exit;
        BlobLen := Dataset.MemoSizeN(SrcFieldNo);
        if BlobLen > 0 then
        begin
          Stream := TMemoryStream.Create;
          try
            Dataset.MemoLoadN(SrcFieldNo, Stream);
            Stream.Position := 0;
            Self.MemoSaveN(DstFieldNo, Stream);
          finally
            Stream.free;
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
        FieldPutN(DstFieldNo, FloatToStr(L));
      end
      else if (SrcTyp = 'N') and (DstTyp = 'N') then  // ilker ekleme
        FloatPutN(DstFieldNo, StrToFloatDef(Dataset.FieldGetN(SrcFieldNo), 0))
      else if (SrcTyp = 'I') and (DstTyp = 'I') then  // ilker ekleme
        IntegerPutN(DstFieldNo, StrToIntDef(Dataset.FieldGetN(SrcFieldNo), 0))
      else
        FieldPutN(DstFieldNo, Dataset.FieldGetN(SrcFieldNo));
    end;
  end;
end;

function GetDesktopBaseTableClass: TlicgBaseTableClass;
begin
//{$IFDEF NATIVE_DB}
  Result := TlicgDbfTable;
//{$ENDIF}
(*
{$IFDEF DATASET_PROVIDER}
  Result := TlicgProviderTable;
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
*)
end;

initialization
  //licgBaseGIS.BaseTableClass := GetDesktopBaseTableClass;

end.


