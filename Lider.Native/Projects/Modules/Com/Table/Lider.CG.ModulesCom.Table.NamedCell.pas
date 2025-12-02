unit Lider.CG.ModulesCom.Table.NamedCell;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  StrUtils,
  Lider.CG.ModulesCom.Table.BaseNamedCell,
  Lider.CG.ModulesCom.Table.NamedCellStyleInt,
  Lider.CG.ModulesCom.Table.PrintTitles,
  Lider.CG.ModulesCom.Table.PrintArea;
//  Windows;

type
  NamedCell = class(BaseNamedCell)
    private
      _NamedCell: NamedCellStyleInt;
    public

    function GetNamedCell: NamedCellStyleInt;
    property NamedCell: NamedCellStyleInt read GetNamedCell;
    constructor Create(Value: String);
  end;

implementation
{ NamedCell }

constructor NamedCell.Create(Value: String);
begin
  inherited Create;
  case ansiindexstr(Value, ['PrintTitles', 'PrintArea']) of
    0:  _NamedCell := PrintTitles.Create;
    1:  _NamedCell := PrintArea.Create;
  end;
end;

function NamedCell.GetNamedCell: NamedCellStyleInt;
begin
  result := Self._NamedCell;
end;

end.

