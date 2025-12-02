unit Lider.CG.ModulesCom.Table.BaseWorksheet;

interface

uses
  Lider.CG.ModulesCom.Table.WorksheetInt,
  Lider.CG.ModulesCom.Table.Styles,
  Generics.Defaults,
  Lider.CG.ModulesCom.Table,
  Lider.CG.ModulesCom.Table.TableInt,
  SysUtils;

type
  BaseWorksheet = class(TInterfacedObject, WorksheetInt)
  private
    _Name: String;
    _Styles: Styles;
    _Table: TableInt;

  Public
    function GetName: String;
    procedure SetName(Name: String);
    property Name: String read GetName write SetName;

    function GetTable: TableInt;
    procedure SetTable(Table: TableInt);
    property Table: TableInt read GetTable write SetTable;

    function GetStyle: Styles;
    procedure SetStyle(Stye: Styles);
    property Style: Styles read GetStyle write SetStyle;

    function GetElektronikTablo2003String: String; virtual;
    constructor Create;
  end;

implementation
{ BaseWorksheet }

constructor BaseWorksheet.Create;
begin
  inherited Create;
  _Styles := Styles.Create;
  _Table := Lider.CG.ModulesCom.Table.Table.Create(15, 48);
end;

function BaseWorksheet.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;
  try
    strExcelXml.Append(Self.Table.GetElektronikTablo2003StringTable);
  except
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

function BaseWorksheet.GetName: String;
begin
  result := Self._Name;
end;

function BaseWorksheet.GetStyle: Styles;
begin
  result := Self._Styles;
end;

function BaseWorksheet.GetTable: TableInt;
begin
  result := Self._Table;
end;

procedure BaseWorksheet.SetName(Name: String);
begin
  Self._Name := Name;
end;

procedure BaseWorksheet.SetStyle(Stye: Styles);
begin
  Self._Styles := Stye;
end;

procedure BaseWorksheet.SetTable(Table: TableInt);
begin
  Self._Table := Table;
end;

end.

