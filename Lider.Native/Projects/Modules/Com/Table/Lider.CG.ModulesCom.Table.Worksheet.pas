unit Lider.CG.ModulesCom.Table.Worksheet;

interface

uses
  Lider.CG.ModulesCom.Table.BaseWorksheet,
  Lider.CG.ModulesCom.Table.BaseNamedCell,
  Lider.CG.ModulesCom.Table.NamedCellStyleInt,
  Lider.CG.ModulesCom.Table.NamedCell,
  System.Generics.Collections,
  SysUtils;

type
  NamedRange = Class(BaseNamedCell)
    private
      _RefersTo: String;
      _NamedCel: NamedCell;
    public
      function GetNamedCell: NamedCellStyleInt; override;
      property NamedCell: NamedCellStyleInt read GetNamedCell;

      function GetRefersTo: String; override;
      property RefersTo: String read GetRefersTo;

      constructor Create(Refersto: String; NamedCel: String);
  End;

type
  Worksheet = class(BaseWorksheet)
    private
      _NamedRange: TList<NamedRange>;
    public
      function GetNamedRange: TList<NamedRange>;
      procedure SetNamedRange(NamedRange: TList<NamedRange>);
      property NamedRange: TList<NamedRange> read GetNamedRange write SetNamedRange;

      function GetElektronikTablo2003String: String; override;

      constructor Create; overload;
      constructor Create(SheetName: String); overload;
  end;

implementation
{ NamedRange }

constructor NamedRange.Create(Refersto, NamedCel: String);
begin
  inherited Create;
  Self._RefersTo := Refersto;
  Self._NamedCel := Lider.CG.ModulesCom.Table.NamedCell.NamedCell.Create(NamedCel);
end;

function NamedRange.GetNamedCell: NamedCellStyleInt;
begin
  result := Self._NamedCel.NamedCell;
end;

function NamedRange.GetRefersTo: String;
begin
  result := Self._RefersTo;
end;

{ Worksheet }

constructor Worksheet.Create;
begin
  inherited Create;
  inherited Name := 'Sayfa1';
end;

constructor Worksheet.Create(SheetName: String);
begin
  inherited Create;
  inherited Name := SheetName;
end;

function Worksheet.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;
  try
    strExcelXml.Append(inherited GetElektronikTablo2003String);
    //'strExcelXml.Append(vbLf)
  except
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

function Worksheet.GetNamedRange: TList<NamedRange>;
begin
  result := Self._NamedRange;
end;

procedure Worksheet.SetNamedRange(NamedRange: TList<NamedRange>);
begin
  Self._NamedRange := NamedRange;
end;

end.

