unit Lider.CG.ModulesCom.Table.Row;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.BaseRow,
  Lider.CG.ModulesCom.Table.Cells;

type
  Row = class(BaseRow)
    private
      _Index: Integer;
      _Cell: Cells;
    public
      function GetIndex: Integer; override;
      procedure SetIndex(Value: Integer); override;
      property Indexs: Integer read GetIndex write SetIndex;

      function GetCell: Cells; override;
      procedure SetCell(Cell: Cells); override;
      property Cell: Cells read GetCell write SetCell;

      constructor Create; overload;
      constructor Create(Height: Single); overload;
  end;

implementation
{ Row }

constructor Row.Create;
begin
  inherited Create;
  _Cell := Cells.Create;
end;

constructor Row.Create(Height: Single);
begin
  inherited Create;
  _Cell := Cells.Create;
  inherited Height := Height;
end;

function Row.GetCell: Cells;
begin
  result := _Cell;
end;

function Row.GetIndex: Integer;
begin
  result := _Index;
end;

procedure Row.SetCell(Cell: Cells);
begin
  _Cell := Cell;
end;

procedure Row.SetIndex(Value: Integer);
begin
  inherited;
  Self._Index := Value;
end;

end.

