unit Lider.CG.Com.BTUNIDAC;

{$I Lider.CG.Com.Component.inc}

interface

uses
  SysUtils,
  Windows,
  Classes,
  Graphics,
  Data.DB, MemDS, DBAccess, Uni,
  Lider.CG.Com.GIS;

type
  TlicgUniDbfTable = class(TlicgBaseTable)
  private
    FUniDbfTable: TUniTable;
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

function TlicgUniDbfTable.DBRenameTable(const Source, Target: string): Boolean;
begin
  if FileExists(Source + '.DBF') then
    SysUtils.RenameFile(Source + '.DBF', Target + '.DBF');
  if FileExists(Source + '.CDX') then
    SysUtils.RenameFile(Source + '.CDX', Target + '.CDX');
  if FileExists(Source + '.fpt') then
    SysUtils.RenameFile(Source + '.FPT', Target + '.FPT');
end;

function TlicgUniDbfTable.DBDropIndex(const TableName: string): Boolean;
begin
  SysUtils.DeleteFile(ChangeFileExt(TableName, '.cdx'));
end;

function TlicgUniDbfTable.DBDropTable(const TableName: string): Boolean;
begin
  SysUtils.DeleteFile(Tablename + '.DBF');
  SysUtils.DeleteFile(Tablename + '.CDX');
  SysUtils.DeleteFile(Tablename + '.FPT');
end;

function TlicgUniDbfTable.DBTableExists(const TableName: string): Boolean;
begin
  Result := FileExists(ChangeFileExt(TableName, '.DBF'));
end;

function TlicgUniDbfTable.DBCreateTable(const fname: string; AFieldList:
  TStringList): boolean;
begin

//  if (AFieldList is TStringList) Then


//  result := gs6_shel.CreateDBF(ChangeFileExt(fname, '.DBF'), '', gs6_shel.FoxPro2,
  //  AFieldList)


  //  Else
//     result := gs6_shel.CreateDBFFromTList(ChangeFileExt(fname, '.DBF'), '',
//       gs6_shel.FoxPro2, AFieldList);

end;

constructor TlicgUniDbfTable.Create(Gis: TlicgBaseGIS; const FName: string;
  ReadWrite, Shared: boolean);
begin
  inherited Create(Gis, fname, ReadWrite, Shared);

  //FUniDbfTable := TgsDBFTable.Create(ChangeFileExt(fname, '.DBF'), '', ReadWrite, Shared);

end;

destructor TlicgUniDbfTable.Destroy;
begin
  FUniDbfTable.Free;
  inherited Destroy;
end;

function TlicgUniDbfTable.GetActive: boolean;
begin
  result := FUniDbfTable.Active;
end;

procedure TlicgUniDbfTable.SetActive(Value: boolean);
begin
  FUniDbfTable.Active := value;
end;

function TlicgUniDbfTable.GetRecNo: Integer;
begin
  result := FUniDbfTable.Recno;
end;

procedure TlicgUniDbfTable.SetRecNo(Value: Integer);
begin
  FUniDbfTable.Recno := Value;
end;

procedure TlicgUniDbfTable.Append(NewRecno: Integer);
begin
  FUniDbfTable.Insert;
  FUniDbfTable.FieldByName('UID').AsInteger := NewRecno;
  FUniDbfTable.Post;
end;

function TlicgUniDbfTable.BOF: Boolean;
begin
  result := FUniDbfTable.BOF;
end;

function TlicgUniDbfTable.EOF: Boolean;
begin
  result := FUniDbfTable.EOF;
end;

function TlicgUniDbfTable.DateGet(const FieldName: string): TDateTime;
var
  Index: Integer;
begin
  Result := 0;
  Index := FieldNo(FieldName);
  if Index > 0 then
    Result := DateGetN(Index);
end;

function TlicgUniDbfTable.DateGetN(FieldNo: integer): TDateTime;
var
  s: string;
  yy, mm, dd: word;
begin
  s := TrimRight(FUniDbfTable.FieldGetN(FieldNo));
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

function TlicgUniDbfTable.Deleted: Boolean;
begin
  result := FUniDbfTable.Deleted;
end;

function TlicgUniDbfTable.Field(FieldNo: integer): string;
begin
  result := FUniDbfTable.Field(FieldNo);
end;

function TlicgUniDbfTable.FieldCount: integer;
begin
  result := FUniDbfTable.FieldCount;
end;

function TlicgUniDbfTable.FieldDec(FieldNo: integer): integer;
begin
  result := FUniDbfTable.FieldDec(FieldNO);
end;

function TlicgUniDbfTable.FieldGet(const FieldName: string): string;
begin
  result := FUniDbfTable.FieldGet(FieldName);
end;

function TlicgUniDbfTable.FieldGetN(FieldNo: integer): string;
begin
  result := FUniDbfTable.FieldGetN(FieldNo);
end;

function TlicgUniDbfTable.FieldLen(FieldNo: integer): integer;
begin
  result := FUniDbfTable.FieldLen(FieldNO);
end;

function TlicgUniDbfTable.FieldNo(const FieldName: string): integer;
begin
  result := FUniDbfTable.FieldNo(FieldName);
end;

function TlicgUniDbfTable.FieldType(FieldNo: integer): char;
begin
  result := Char(FUniDbfTable.FieldType(FieldNo)); // li2016
end;

function TlicgUniDbfTable.Find(const ss: string; IsExact, IsNear: boolean): boolean;
begin
  result := FUniDbfTable.Find(ss, IsExact, Isnear);
end;

function TlicgUniDbfTable.FloatGet(const FieldName: string): Double;
begin
  result := FUniDbfTable.FloatGet(FieldName);
end;

function TlicgUniDbfTable.FloatGetN(FieldNo: Integer): Double;
begin
  result := FUniDbfTable.FloatGetN(FieldNo);
end;

function TlicgUniDbfTable.IndexCount: integer;
begin
  result := FUniDbfTable.IndexCount;
end;

function TlicgUniDbfTable.IndexAscending(Value: integer): boolean;
begin
  result := FUniDbfTable.IndexAscending(Value);
end;

function TlicgUniDbfTable.Index(const INames, Tag: string): integer;
begin
  if FileExists(INames) then
    result := FUniDbfTable.Index(INames, Tag);
end;

function TlicgUniDbfTable.IndexUnique(Value: integer): boolean;
begin
  Result := FUniDbfTable.IndexUnique(Value);
end;

function TlicgUniDbfTable.IndexExpression(Value: integer): string;
begin
  Result := FUniDbfTable.IndexExpression(Value);
end;

function TlicgUniDbfTable.IndexTagName(Value: integer): string;
begin
  Result := FUniDbfTable.IndexTagName(Value);
end;

function TlicgUniDbfTable.IndexFilter(Value: integer): string;
begin
  Result := FUniDbfTable.IndexFilter(Value);
end;

function TlicgUniDbfTable.IntegerGet(const FieldName: string): Integer;
var
  Temp: Double;
begin
  Temp := FUniDbfTable.IntegerGet(FieldName);
  Result := Trunc(Temp);
end;

function TlicgUniDbfTable.IntegerGetN(FieldNo: integer): Integer;
var
  Temp: Double;
begin
  Temp := FUniDbfTable.IntegerGetN(FieldNo);
  Result := Trunc(Temp);
end;

function TlicgUniDbfTable.LogicGet(const FieldName: string): Boolean;
begin
  result := FUniDbfTable.LogicGet(FieldName);
end;

function TlicgUniDbfTable.LogicGetN(FieldNo: integer): Boolean;
begin
  result := FUniDbfTable.LogicgetN(FieldNo);
end;

procedure TlicgUniDbfTable.MemoSave(const FieldName: string; Stream: TStream);
begin
  MemoSaveN(FUniDbfTable.FieldNo(FieldName), stream);
end;

procedure TlicgUniDbfTable.MemoSaveN(FieldNo: integer; Stream: TStream);
var
  BlobLen: Integer;
  Memory: PAnsiChar; // li2016
begin
  BlobLen := Stream.Size;
  GetMem(Memory, BlobLen + 1);
  try
    Memory[BlobLen] := #0;
    Stream.Read(Memory[0], BlobLen);
    FUniDbfTable.MemoSaveN(FieldNo, Memory, BlobLen);
  finally
    FreeMem(Memory, BlobLen + 1);
  end;
end;

function TlicgUniDbfTable.MemoSize(const FieldName: string): Integer;
begin
  result := FUniDbfTable.MemoSize(FieldName);
end;

function TlicgUniDbfTable.MemoSizeN(FieldNo: integer): Integer;
begin
  result := FUniDbfTable.MemoSizeN(FieldNo);
end;

function TlicgUniDbfTable.RecordCount: Integer;
begin
  result := FUniDbfTable.RecordCount;
end;

function TlicgUniDbfTable.StringGet(const FieldName: string): string;
begin
  result := FUniDbfTable.StringGet(FieldName);
end;

function TlicgUniDbfTable.StringGetN(FieldNo: integer): string;
begin
  result := FUniDbfTable.StringGetN(FieldNo);
end;

{Procedure TlicgHalcyonTable.CopyStructure( Const FileName, APassword: String );
begin
  FUniDbfTable.CopyStructure( FileName, APassword );
end;

Procedure TlicgHalcyonTable.CopyTo( Const FileName, APassword: String );
begin
  FUniDbfTable.CopyTo( FileName, APassword );
end;}

procedure TlicgUniDbfTable.DatePut(const FieldName: string; value: TDateTime);
begin
  FUniDbfTable.FieldPut(FieldName, FormatDateTime('yyyymmdd', value));
end;

procedure TlicgUniDbfTable.DatePutN(FieldNo: integer; value: TDateTime);
begin
  FUniDbfTable.FieldPutN(FieldNo, FormatDateTime('yyyymmdd', value));
end;

procedure TlicgUniDbfTable.Delete;
begin
  FUniDbfTable.Delete;
end;

procedure TlicgUniDbfTable.Edit;
begin
  FUniDbfTable.Edit;
end;

procedure TlicgUniDbfTable.FieldPut(const FieldName, Value: string);
begin
  FUniDbfTable.FieldPut(FieldName, Value);
end;

procedure TlicgUniDbfTable.FieldPutN(FieldNo: integer; const Value: string);
begin
  FUniDbfTable.FieldPutN(FieldNo, Value);
end;

procedure TlicgUniDbfTable.First;
begin
  FUniDbfTable.First;
end;

procedure TlicgUniDbfTable.FloatPut(const FieldName: string; const Value: Double);
begin
  FUniDbfTable.FloatPut(FieldName, Value);
end;

procedure TlicgUniDbfTable.FloatPutN(FieldNo: integer; const Value: Double);
begin
  FUniDbfTable.FloatPutN(FieldNo, Value);
end;

procedure TlicgUniDbfTable.FlushDB;
begin
  FUniDbfTable.FlushDBF;
end;

procedure TlicgUniDbfTable.Go(n: Integer);
begin
  FUniDbfTable.Go(n);
end;

procedure TlicgUniDbfTable.IndexOn(const IName, tag, keyexp, forexp: string;
  uniq: TlicgIndexUnique; ascnd: TlicgSortStatus);
begin
  SysUtils.DeleteFile(IName);
  FUniDbfTable.IndexOn(IName, tag, keyexp, forexp, gsIndexUnique(ord(uniq)),
    gsSortStatus(ord(ascnd)));
end;

procedure TlicgUniDbfTable.IntegerPut(const FieldName: string; Value: Integer);
begin
  FUniDbfTable.IntegerPut(FieldName, Value);
end;

procedure TlicgUniDbfTable.IntegerPutN(FieldNo: integer; Value: Integer);
begin
  FUniDbfTable.IntegerPutN(FieldNo, value);
end;

procedure TlicgUniDbfTable.Last;
begin
  FUniDbfTable.Last;
end;

procedure TlicgUniDbfTable.LogicPut(const FieldName: string; value: boolean);
begin
  FUniDbfTable.LogicPut(FieldName, value);
end;

procedure TlicgUniDbfTable.LogicPutN(fieldno: integer; value: boolean);
begin
  FUniDbfTable.LogicPutN(fieldno, value);
end;

procedure TlicgUniDbfTable.MemoLoad(const FieldName: string; Stream: TStream);
begin
  MemoLoadN(FUniDbfTable.FieldNo(FieldName), stream);
end;

procedure TlicgUniDbfTable.MemoLoadN(fieldno: integer; Stream: TStream);
var
  BlobLen: Integer;
  Memory: PAnsiChar; // li2016
begin
  if FieldNo < 1 then
    Exit;
  BlobLen := FUniDbfTable.MemoSizeN(FieldNo);
  GetMem(Memory, BlobLen + 1);
  try
    FUniDbfTable.MemoLoadN(FieldNo, Memory, BlobLen);
    Stream.Write(Memory[0], BlobLen * SizeOf(AnsiChar)); // ilker deðiþtirme
    Stream.Position := 0;
  finally
    FreeMem(Memory, BlobLen + 1);
  end;
end;

procedure TlicgUniDbfTable.Next;
begin
  FUniDbfTable.Next;
end;

procedure TlicgUniDbfTable.Pack;
begin
  FUniDbfTable.Pack;
end;

procedure TlicgUniDbfTable.Post;
begin
  FUniDbfTable.Post;
end;

procedure TlicgUniDbfTable.Prior;
begin
  FUniDbfTable.Prior;
end;

procedure TlicgUniDbfTable.Recall;
begin
  FUniDbfTable.Recall;
end;

procedure TlicgUniDbfTable.Refresh;
begin
  FUniDbfTable.Refresh;
end;

procedure TlicgUniDbfTable.Reindex;
begin
  FUniDbfTable.Reindex;
end;

procedure TlicgUniDbfTable.SetTagTo(const TName: string);
begin
  FUniDbfTable.SetTagTo(TName);
end;

procedure TlicgUniDbfTable.SetUseDeleted(tf: boolean);
begin
  FUniDbfTable.SetUseDeleted(tf);
end;

procedure TlicgUniDbfTable.StringPut(const FieldName, value: string);
begin
  FUniDbfTable.StringPut(FieldName, value);
end;

procedure TlicgUniDbfTable.StringPutN(fieldno: integer; const value: string);
begin
  FUniDbfTable.StringPutN(fieldno, value);
end;

procedure TlicgUniDbfTable.Zap;
begin
  FUniDbfTable.zap;
end;

procedure TlicgUniDbfTable.AssignFrom(Dataset: TlicgBaseTable; SrcFieldNo,
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

procedure TlicgUniDbfTable.BeginTrans;
begin

end;

procedure TlicgUniDbfTable.EndTrans;
begin

end;

end.


