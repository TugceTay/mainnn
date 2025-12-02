unit Lider.CG.Com.BTHalcyon;

{$I Lider.CG.Com.Component.inc}

interface

uses
  SysUtils,
  Windows,
  Classes,
  Graphics,
  gs6_shel,
  Lider.CG.Com.GIS;

type
  TlicgHalcyonTable = class(TlicgBaseTable)
  private
    FgsDbfTable: TgsDBFTable;
  protected
    function GetActive: boolean; override;
    procedure SetActive(Value: boolean); override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(Gis: TlicgBaseGIS; const FName: string; ReadWrite, Shared:
      boolean); override;
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
    function FloatGet(const FieldName: string): Double; override;
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
    procedure IndexOn(const IName, tag, keyexp, forexp: string; uniq:
      TlicgIndexUnique; ascnd: TlicgSortStatus); override;
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
    function DBCreateTable(const fname: string; AFieldList: TStringList):
      boolean; override;
    function DBTableExists(const TableName: string): Boolean; override;
    function DBDropTable(const TableName: string): Boolean; override;
    function DBDropIndex(const TableName: string): Boolean; override;
    function DBRenameTable(const Source, Target: string): Boolean; override;
    procedure AssignFrom(Dataset: TlicgBaseTable; SrcFieldNo, DstFieldNo:
      Integer); override;
    procedure BeginTrans; override;
    procedure EndTrans; override;
  end;

implementation

uses
  Forms,
  DB;

function TlicgHalcyonTable.DBRenameTable(const Source, Target: string): Boolean;
begin
  if FileExists(Source + '.DBF') then
    SysUtils.RenameFile(Source + '.DBF', Target + '.DBF');
  if FileExists(Source + '.CDX') then
    SysUtils.RenameFile(Source + '.CDX', Target + '.CDX');
  if FileExists(Source + '.fpt') then
    SysUtils.RenameFile(Source + '.FPT', Target + '.FPT');
end;

function TlicgHalcyonTable.DBDropIndex(const TableName: string): Boolean;
begin
  SysUtils.DeleteFile(ChangeFileExt(TableName, '.cdx'));
end;

function TlicgHalcyonTable.DBDropTable(const TableName: string): Boolean;
begin
  SysUtils.DeleteFile(Tablename + '.DBF');
  SysUtils.DeleteFile(Tablename + '.CDX');
  SysUtils.DeleteFile(Tablename + '.FPT');
end;

function TlicgHalcyonTable.DBTableExists(const TableName: string): Boolean;
begin
  Result := FileExists(ChangeFileExt(TableName, '.DBF'));
end;

function TlicgHalcyonTable.DBCreateTable(const fname: string; AFieldList:
  TStringList): boolean;
begin

//  if (AFieldList is TStringList) Then
  result := gs6_shel.CreateDBF(ChangeFileExt(fname, '.DBF'), '', gs6_shel.FoxPro2,
    AFieldList)
//  Else
//     result := gs6_shel.CreateDBFFromTList(ChangeFileExt(fname, '.DBF'), '',
//       gs6_shel.FoxPro2, AFieldList);

end;

constructor TlicgHalcyonTable.Create(Gis: TlicgBaseGIS; const FName: string;
  ReadWrite, Shared: boolean);
begin
  inherited Create(Gis, fname, ReadWrite, Shared);

  FgsDbfTable := TgsDBFTable.Create(ChangeFileExt(fname, '.DBF'), '', ReadWrite, Shared);

end;

destructor TlicgHalcyonTable.Destroy;
begin
  FgsDbfTable.Free;
  inherited Destroy;
end;

function TlicgHalcyonTable.GetActive: boolean;
begin
  result := FgsDbfTable.Active;
end;

procedure TlicgHalcyonTable.SetActive(Value: boolean);
begin
  FgsDbfTable.Active := value;
end;

function TlicgHalcyonTable.GetRecNo: Integer;
begin
  result := FgsDbfTable.Recno;
end;

procedure TlicgHalcyonTable.SetRecNo(Value: Integer);
begin
  FgsDbfTable.Recno := Value;
end;

procedure TlicgHalcyonTable.Append(NewRecno: Integer);
var
  FieldNo: Integer;
begin

  FieldNo := FgsDbfTable.FieldNo('UID');
  FgsDbfTable.Append;
  if FieldNo > 0 then
    FgsDbfTable.IntegerPutN(FieldNo, NewRecno);
  FgsDbfTable.Post;
end;

function TlicgHalcyonTable.BOF: Boolean;
begin
  result := FgsDbfTable.BOF;
end;

function TlicgHalcyonTable.EOF: Boolean;
begin
  result := FgsDbfTable.EOF;
end;

function TlicgHalcyonTable.DateGet(const FieldName: string): TDateTime;
var
  Index: Integer;
begin
  Result := 0;
  Index := FieldNo(FieldName);
  if Index > 0 then
    Result := DateGetN(Index);
end;

function TlicgHalcyonTable.DateGetN(FieldNo: integer): TDateTime;
var
  s: string;
  yy, mm, dd: word;
begin
  s := TrimRight(FgsDbfTable.FieldGetN(FieldNo));
  Result := 0;
  if (length(s) = 8) and (s <> '00000000') then
  try
    yy := StrToInt(system.copy(s, 1, 4));
    mm := StrToInt(system.Copy(s, 5, 2));
    dd := StrToInt(system.Copy(s, 7, 2));
    Result := EncodeDate(yy, mm, dd);
  except
  end;
end;

function TlicgHalcyonTable.Deleted: Boolean;
begin
  result := FgsDbfTable.Deleted;
end;

function TlicgHalcyonTable.Field(FieldNo: integer): string;
begin
  result := FgsDbfTable.Field(FieldNo);
end;

function TlicgHalcyonTable.FieldCount: integer;
begin
  result := FgsDbfTable.FieldCount;
end;

function TlicgHalcyonTable.FieldDec(FieldNo: integer): integer;
begin
  result := FgsDbfTable.FieldDec(FieldNO);
end;

function TlicgHalcyonTable.FieldGet(const FieldName: string): string;
begin
  result := FgsDbfTable.FieldGet(FieldName);
end;

function TlicgHalcyonTable.FieldGetN(FieldNo: integer): string;
begin
  result := FgsDbfTable.FieldGetN(FieldNo);
end;

function TlicgHalcyonTable.FieldLen(FieldNo: integer): integer;
begin
  result := FgsDbfTable.FieldLen(FieldNO);
end;

function TlicgHalcyonTable.FieldNo(const FieldName: string): integer;
begin
  result := FgsDbfTable.FieldNo(FieldName);
end;

function TlicgHalcyonTable.FieldType(FieldNo: integer): char;
begin
  result := Char(FgsDbfTable.FieldType(FieldNo)); // li2016
end;

function TlicgHalcyonTable.Find(const ss: string; IsExact, IsNear: boolean): boolean;
begin
  result := FgsDbfTable.Find(ss, IsExact, Isnear);
end;

function TlicgHalcyonTable.FloatGet(const FieldName: string): Double;
begin
  result := FgsDbfTable.FloatGet(FieldName);
end;

function TlicgHalcyonTable.FloatGetN(FieldNo: Integer): Double;
begin
  result := FgsDbfTable.FloatGetN(FieldNo);
end;

function TlicgHalcyonTable.IndexCount: integer;
begin
  result := FgsDbfTable.IndexCount;
end;

function TlicgHalcyonTable.IndexAscending(Value: integer): boolean;
begin
  result := FgsDbfTable.IndexAscending(Value);
end;

function TlicgHalcyonTable.Index(const INames, Tag: string): integer;
begin
  if FileExists(INames) then
    result := FgsDbfTable.Index(INames, Tag);
end;

function TlicgHalcyonTable.IndexUnique(Value: integer): boolean;
begin
  Result := FgsDbfTable.IndexUnique(Value);
end;

function TlicgHalcyonTable.IndexExpression(Value: integer): string;
begin
  Result := FgsDbfTable.IndexExpression(Value);
end;

function TlicgHalcyonTable.IndexTagName(Value: integer): string;
begin
  Result := FgsDbfTable.IndexTagName(Value);
end;

function TlicgHalcyonTable.IndexFilter(Value: integer): string;
begin
  Result := FgsDbfTable.IndexFilter(Value);
end;

function TlicgHalcyonTable.IntegerGet(const FieldName: string): Integer;
var
  Temp: Double;
begin
  Temp := FgsDbfTable.IntegerGet(FieldName);
  Result := Trunc(Temp);
end;

function TlicgHalcyonTable.IntegerGetN(FieldNo: integer): Integer;
var
  Temp: Double;
begin
  Temp := FgsDbfTable.IntegerGetN(FieldNo);
  Result := Trunc(Temp);
end;

function TlicgHalcyonTable.LogicGet(const FieldName: string): Boolean;
begin
  result := FgsDbfTable.LogicGet(FieldName);
end;

function TlicgHalcyonTable.LogicGetN(FieldNo: integer): Boolean;
begin
  result := FgsDbfTable.LogicgetN(FieldNo);
end;

procedure TlicgHalcyonTable.MemoSave(const FieldName: string; Stream: TStream);
begin
  MemoSaveN(FgsDbfTable.FieldNo(FieldName), stream);
end;

procedure TlicgHalcyonTable.MemoSaveN(FieldNo: integer; Stream: TStream);
var
  BlobLen: Integer;
  Memory: PAnsiChar; // li2016
begin
  BlobLen := Stream.Size;
  GetMem(Memory, BlobLen + 1);
  try
    Memory[BlobLen] := #0;
    Stream.Read(Memory[0], BlobLen);
    FgsDbfTable.MemoSaveN(FieldNo, Memory, BlobLen);
  finally
    FreeMem(Memory, BlobLen + 1);
  end;
end;

function TlicgHalcyonTable.MemoSize(const FieldName: string): Integer;
begin
  result := FgsDbfTable.MemoSize(FieldName);
end;

function TlicgHalcyonTable.MemoSizeN(FieldNo: integer): Integer;
begin
  result := FgsDbfTable.MemoSizeN(FieldNo);
end;

function TlicgHalcyonTable.RecordCount: Integer;
begin
  result := FgsDbfTable.RecordCount;
end;

function TlicgHalcyonTable.StringGet(const FieldName: string): string;
begin
  result := FgsDbfTable.StringGet(FieldName);
end;

function TlicgHalcyonTable.StringGetN(FieldNo: integer): string;
begin
  result := FgsDbfTable.StringGetN(FieldNo);
end;

{Procedure TlicgHalcyonTable.CopyStructure( Const FileName, APassword: String );
begin
  FgsDbfTable.CopyStructure( FileName, APassword );
end;

Procedure TlicgHalcyonTable.CopyTo( Const FileName, APassword: String );
begin
  FgsDbfTable.CopyTo( FileName, APassword );
end;}

procedure TlicgHalcyonTable.DatePut(const FieldName: string; value: TDateTime);
begin
  FgsDbfTable.FieldPut(FieldName, FormatDateTime('yyyymmdd', value));
end;

procedure TlicgHalcyonTable.DatePutN(FieldNo: integer; value: TDateTime);
begin
  FgsDbfTable.FieldPutN(FieldNo, FormatDateTime('yyyymmdd', value));
end;

procedure TlicgHalcyonTable.Delete;
begin
  FgsDbfTable.Delete;
end;

procedure TlicgHalcyonTable.Edit;
begin
  FgsDbfTable.Edit;
end;

procedure TlicgHalcyonTable.FieldPut(const FieldName, Value: string);
begin
  FgsDbfTable.FieldPut(FieldName, Value);
end;

procedure TlicgHalcyonTable.FieldPutN(FieldNo: integer; const Value: string);
begin
  FgsDbfTable.FieldPutN(FieldNo, Value);
end;

procedure TlicgHalcyonTable.First;
begin
  FgsDbfTable.First;
end;

procedure TlicgHalcyonTable.FloatPut(const FieldName: string; const Value: Double);
begin
  FgsDbfTable.FloatPut(FieldName, Value);
end;

procedure TlicgHalcyonTable.FloatPutN(FieldNo: integer; const Value: Double);
begin
  FgsDbfTable.FloatPutN(FieldNo, Value);
end;

procedure TlicgHalcyonTable.FlushDB;
begin
  FgsDbfTable.FlushDBF;
end;

procedure TlicgHalcyonTable.Go(n: Integer);
begin
  FgsDbfTable.Go(n);
end;

procedure TlicgHalcyonTable.IndexOn(const IName, tag, keyexp, forexp: string;
  uniq: TlicgIndexUnique; ascnd: TlicgSortStatus);
begin
  SysUtils.DeleteFile(IName);
  FgsDbfTable.IndexOn(IName, tag, keyexp, forexp, gsIndexUnique(ord(uniq)),
    gsSortStatus(ord(ascnd)));
end;

procedure TlicgHalcyonTable.IntegerPut(const FieldName: string; Value: Integer);
begin
  FgsDbfTable.IntegerPut(FieldName, Value);
end;

procedure TlicgHalcyonTable.IntegerPutN(FieldNo: integer; Value: Integer);
begin
  FgsDbfTable.IntegerPutN(FieldNo, value);
end;

procedure TlicgHalcyonTable.Last;
begin
  FgsDbfTable.Last;
end;

procedure TlicgHalcyonTable.LogicPut(const FieldName: string; value: boolean);
begin
  FgsDbfTable.LogicPut(FieldName, value);
end;

procedure TlicgHalcyonTable.LogicPutN(fieldno: integer; value: boolean);
begin
  FgsDbfTable.LogicPutN(fieldno, value);
end;

procedure TlicgHalcyonTable.MemoLoad(const FieldName: string; Stream: TStream);
begin
  MemoLoadN(FgsDbfTable.FieldNo(FieldName), stream);
end;

procedure TlicgHalcyonTable.MemoLoadN(fieldno: integer; Stream: TStream);
var
  BlobLen: Integer;
  Memory: PAnsiChar; // li2016
begin
  if FieldNo < 1 then
    Exit;
  BlobLen := FgsDbfTable.MemoSizeN(FieldNo);
  GetMem(Memory, BlobLen + 1);
  try
    FgsDbfTable.MemoLoadN(FieldNo, Memory, BlobLen);
    Stream.Write(Memory[0], BlobLen * SizeOf(AnsiChar)); // ilker deðiþtirme
    Stream.Position := 0;
  finally
    FreeMem(Memory, BlobLen + 1);
  end;
end;

procedure TlicgHalcyonTable.Next;
begin
  FgsDbfTable.Next;
end;

procedure TlicgHalcyonTable.Pack;
begin
  FgsDbfTable.Pack;
end;

procedure TlicgHalcyonTable.Post;
begin
  FgsDbfTable.Post;
end;

procedure TlicgHalcyonTable.Prior;
begin
  FgsDbfTable.Prior;
end;

procedure TlicgHalcyonTable.Recall;
begin
  FgsDbfTable.Recall;
end;

procedure TlicgHalcyonTable.Refresh;
begin
  FgsDbfTable.Refresh;
end;

procedure TlicgHalcyonTable.Reindex;
begin
  FgsDbfTable.Reindex;
end;

procedure TlicgHalcyonTable.SetTagTo(const TName: string);
begin
  FgsDbfTable.SetTagTo(TName);
end;

procedure TlicgHalcyonTable.SetUseDeleted(tf: boolean);
begin
  FgsDbfTable.SetUseDeleted(tf);
end;

procedure TlicgHalcyonTable.StringPut(const FieldName, value: string);
begin
  FgsDbfTable.StringPut(FieldName, value);
end;

procedure TlicgHalcyonTable.StringPutN(fieldno: integer; const value: string);
begin
  FgsDbfTable.StringPutN(fieldno, value);
end;

procedure TlicgHalcyonTable.Zap;
begin
  FgsDbfTable.zap;
end;

procedure TlicgHalcyonTable.AssignFrom(Dataset: TlicgBaseTable; SrcFieldNo,
  DstFieldNo: Integer);
var
  SrcTyp, DstTyp: Char;
  BlobLen: Integer;
  stream: TStream;
  E: Integer;
  L: Double;
begin
  if (SrcFieldNo = 0) or (DstFieldNo = 0) then
    Exit;
  SrcTyp := Dataset.FieldType(SrcFieldno);
  DstTyp := Self.FieldType(DstFieldno);

  case DstTyp of
    'M', 'B', 'G':
      begin
        if not (SrcTyp in ['M', 'B', 'G']) then
          exit;
        BlobLen := Dataset.MemoSizeN(SrcFieldNo);
        if BlobLen > 0 then
        begin
          stream := TMemoryStream.Create;
          try
            Dataset.MemoLoadN(SrcFieldNo, stream);
            stream.Position := 0;
            Self.MemoSaveN(DstFieldNo, stream);
          finally
            stream.free;
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
      else

        FieldPutN(DstFieldNo, Dataset.FieldGetN(SrcFieldNo));

    end;
  end;

end;

procedure TlicgHalcyonTable.BeginTrans;
begin

end;

procedure TlicgHalcyonTable.EndTrans;
begin

end;

end.


