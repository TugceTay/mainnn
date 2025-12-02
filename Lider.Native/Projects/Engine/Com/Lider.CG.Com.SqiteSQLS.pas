unit Lider.CG.Com.SqiteSQLS;

interface

uses
  SysUtils,
  Windows,
  Classes,
  Db,
  Variants;
  // li2016  ZAbstractConnection, ZConnection,ZAbstractTable, ZDataset, ZAbstractRODataset, ZAbstractDataset;

type
  TFieldStringListData = class
    dt: TFieldType;
    size: integer;
    dec: integer;
  end;

  TFieldStringListDataSL = class(TStringList)
    procedure AddField(f: string; dt: TFieldType; size: integer; dec: integer = 0);
    destructor Destroy; override;
    procedure ClearDatas;
  end;

{procedure EXEC_SQL_CREATE_TABLE (const conn : TZConnection; FieldSL : TFieldStringListDataSL; aTableName : string);
procedure EXEC_SQL_DROP_TABLE   (const conn : TZConnection;aTableName : string);
procedure EXEC_DELETE_RECORDS   (const conn : TZConnection;aTableName: string);
function  EXEC_TABLE_EXISTS     (const conn : TZConnection;aTableName: string): Boolean;
}

implementation

{
function EXEC_TABLE_EXISTS (const conn : TZConnection; aTableName: string): Boolean;
begin

   Result := False;

   with TZQuery.Create(nil) do
   begin

      try
        Connection := Conn;
        SQL.Add('SELECT count(*) as c FROM sqlite_master WHERE type='+QuotedStr('table')+' AND name='+ QuotedStr(aTableName)+';');
        Open;
        Result := FieldByName('c').AsInteger>0;
      finally
        Free;
      end;

   end;

end;


procedure EXEC_DELETE_RECORDS(const conn : TZConnection;aTableName: string);
begin
  with TZQuery.Create (nil) do
  begin
    try
      Connection := Conn;
      SQL.Add('DELETE FROM ' + aTableName + ';');
      ExecSQL;
    finally
      Free;
    end;
  end;

end;


procedure EXEC_SQL_CREATE_TABLE (const conn : TZConnection; FieldSL : TFieldStringListDataSL; aTableName : string);
var
  Q : TZQuery;
  i : integer;
  s : string;
begin
  Q := TZQuery.Create(nil);
  try
    Q.Connection := Conn;
    Q.SQL.Add('CREATE TABLE IF NOT EXISTS ' + aTableName);
    Q.SQL.Add(' ( ');
    for i:=0 to FieldSL.Count-1 do
    begin
       case TFieldStringListData(FieldSL.Objects[i]).dt of
         ftAutoInc    : Q.SQL.Add(FieldSL[i] + ' INTEGER PRIMARY KEY AUTOINCREMENT,');
         ftBoolean    : Q.SQL.Add(FieldSL[i] + ' BOOLEAN,');
         ftFloat      : Q.SQL.Add(FieldSL[i] + ' FLOAT,');
         ftSmallint   : Q.SQL.Add(FieldSL[i] + ' INT2,');
         ftInteger    : Q.SQL.Add(FieldSL[i] + ' INTEGER,');
         ftLargeint   : Q.SQL.Add(FieldSL[i] + ' BIGINT,');
         ftWord       : Q.SQL.Add(FieldSL[i] + ' INT,');
         ftString     : Q.SQL.Add(FieldSL[i] + ' VARCHAR('+ inttostr(TFieldStringListData(FieldSL.Objects[i]).size)  +'),');
         ftDate       : Q.SQL.Add(FieldSL[i] + ' DATE,');
         ftDateTime   : Q.SQL.Add(FieldSL[i] + ' DATETIME,');
         //ftTime       : Q.SQL.Add(FieldSL[i] + ' ;
         ftWideString : Q.SQL.Add(FieldSL[i] + ' TEXT,');
         ftBlob       : Q.SQL.Add(FieldSL[i] + ' BLOB,');
         ftMemo       : Q.SQL.Add(FieldSL[i] + ' BLOB,');
         ftTypedBinary: Q.SQL.Add(FieldSL[i] + ' BLOB,');
         ftGraphic    : Q.SQL.Add(FieldSL[i] + ' BLOB,');
         ftBytes      : Q.SQL.Add(FieldSL[i] + ' BLOB,');
        // ftCurrency   :;
        // ftTimeStamp  :;

       end;
    end;

    s := Q.SQL[Q.SQL.Count-1];
    delete(s, Length(s), 1);
    Q.SQL[Q.SQL.Count-1] := s;

    Q.SQL.Add(' ); ');
    Q.SQL.SaveToFile(ExtractFilePath(paramstr(0))  + 'Temp.sql');
    Q.ExecSQL;

  finally
    Q.Free;

  end;

end;

procedure EXEC_SQL_DROP_TABLE (const conn : TZConnection;aTableName : string);
var
  Q : TZQuery;
begin
  Q := TZQuery.Create(nil);
  try
    Q.Connection := Conn;
    Q.SQL.Add('DROP TABLE IF EXISTS ' + aTableName);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;
li2016
}
procedure TFieldStringListDataSL.AddField(f: string; dt: TFieldType; size, dec: integer);
var
  FData: TFieldStringListData;
begin
  FData := TFieldStringListData.Create;
  FData.dt := dt;
  FData.size := size;
  FData.dec := dec;
  AddObject(f, FData);
end;

procedure TFieldStringListDataSL.ClearDatas;
var
  i: integer;
begin
  for i := 0 to count - 1 do
  begin
    TFieldStringListData(Objects[i]).Free;

  end;

  Clear;

end;

destructor TFieldStringListDataSL.Destroy;
begin
  ClearDatas;
  inherited;
end;

end.


