unit Lider.CG.ModulesCom.Table.CellData;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.BaseCellData;
//  Windows;

type
  CellData = class(BaseCellData)
  private
  public
    constructor Create; overload;
    constructor Create(Value: String); overload;
  end;

implementation
{ CellData }

constructor CellData.Create(Value: String);
begin
  Inherited Create;
  Inherited Value := Value;
end;

constructor CellData.Create;
begin
  Inherited Create;
end;

end.

