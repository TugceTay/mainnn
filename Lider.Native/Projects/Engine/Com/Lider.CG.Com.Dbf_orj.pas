unit Lider.CG.Com.Impl;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  Graphics,
  SysUtils,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.BTHalcyon;

function GetDesktopBaseTableClass: TlicgBaseTableClass;

function CreateAndOpenTable(GIS: TlicgBaseGIS; const FileName: string; ReadWrite,
  Shared: Boolean): TlicgBaseTable;

function CreateTable(GIS: TlicgBaseGIS): TlicgBaseTable;

function GetRegularFieldName(const FSL: TStrings; const fn: string): string;

function PotgisTableExistsInProjectOrReferansProject(PrjGis, RefGis:
  TlicgBaseGis; Server, Database, TableName: string): boolean;

implementation

uses
  Forms,
  Db,
  Lider.CG.Com.System,
  Lider.CG.Com.Consts;
  {.$IFDEF HALCYON_DB}
     // , bBTHalcyon
  {.$ENDIF}

function PotgisTableExistsInProjectOrReferansProject(PrjGis, RefGis:
  TlicgBaseGis; Server, Database, TableName: string): boolean;

  function _exists(aGis: TlicgBaseGIS): boolean;
  var
    i: integer;
  begin
    result := False;

    for i := 0 to aGis.Layers.Count - 1 do
    begin
      if (aGis.Layers[i].LayerClassType in [lctPostgresUserTable,
        lctPostgresStandartTable]) then
      begin
        if (aGis.Layers[i].LayerConnector.HostName = Server) and (aGis.Layers[i].LayerConnector.Database
          = Database) and (aGis.Layers[i].LayerConnector.TableName = TableName) then
        begin
          result := true;
          BreaK;
        end;
      end;
    end;
  end;

begin
  result := _exists(PrjGis);
  if (not Result) and Assigned(RefGis) then
  begin
    Result := _exists(RefGis);
  end;
end;

function GetRegularFieldName(const FSL: TStrings; const fn: string): string;
var
  i, j: integer;
begin
  Result := fn;
  Delete(Result, 11, Length(fn) - 10);

  if (FSL.IndexOf(Result) >= 0) then
  begin
    j := 0;
    while (FSL.IndexOf(Result) >= 0) do
    begin
      inc(j);
      if j > 99 then
        BREAK;
      if j > 9 then
      begin

        if Length(Result) >= 10 then
        begin
          Result[9] := inttostr(j)[1];
          Result[10] := inttostr(j)[2];

        end
        else
        begin
          Result := Result + inttostr(j);
        end;

      end
      else
      begin

        if Length(Result) >= 10 then
        begin
          Result[10] := inttostr(j)[1];

        end
        else
        begin

          Result := Result + inttostr(j)[1];
        end;

      end;

    end;

  end
  else
  begin

  end;

  FSL.Add(Result);
end;

function CreateAndOpenTable(GIS: TlicgBaseGIS; const FileName: string; ReadWrite,
  Shared: Boolean): TlicgBaseTable;
begin
  Result := GIS.BaseTableClass.Create(GIS, FileName, ReadWrite, Shared);
end;

function CreateTable(GIS: TlicgBaseGIS): TlicgBaseTable;
begin
  Result := GIS.BaseTableClass.CreateNoOpen(GIS);
end;

function GetDesktopBaseTableClass: TlicgBaseTableClass;
begin
  Result := TlicgHalcyonTable;
   //ilker Result := TTable;
   //Result := TSQliteDataset;
end;

end.


