unit Lider.CG.ModulesCom.Table.BaseTable;

interface

uses
  Lider.CG.ModulesCom.Table.TableInt,
  Lider.CG.ModulesCom.Table.Columns,
  Generics.Defaults,
  Lider.CG.ModulesCom.Table.Rows,
  Lider.CG.ModulesCom.Table.Row,
  Lider.CG.ModulesCom.Table.Cells,
  Lider.CG.ModulesCom.Table.Cell,
  Lider.CG.ModulesCom.Table.CellInt,
  Lider.CG.ModulesCom.Table.RowInt,
  Math;

type
  BaseTable = class(TInterfacedObject, TableInt)
  private
    _DefaultColumnWidth: Single;
    _DefaultRowHeight: Single;
    _ExpandedColumnCount: Integer;
    _ExpandedRowCount: Integer;
    _FullColumns: Boolean;
    _FullRows: Boolean;
    _StyleID: String;

  Public
    function GetExpandedColumnCount: Integer;
    property ExpandedColumnCount: Integer read GetExpandedColumnCount;

    function GetExpandedRowCount: Integer;
    property ExpandedRowCount: Integer read GetExpandedRowCount;

    function GetFullColumns: Boolean;
    procedure SetFullColumns(Value: Boolean);
    property FullColumns: Boolean read GetFullColumns write SetFullColumns;

    function GetFullRows: Boolean;
    procedure SetFullRows(Value: Boolean);
    property FullRows: Boolean read GetFullRows write SetFullRows;

    function GetStyleID: String;
    procedure SetStyleID(Value: String);
    property StyleID: String read GetStyleID write SetStyleID;

    function GetDefaultColumnWidth: Single;
    procedure SetDefaultColumnWidth(Value: Single);
    property DefaultColumnWidth: Single read GetDefaultColumnWidth write SetDefaultColumnWidth;

    function GetDefaultRowHeight: Single;
    procedure SetDefaultRowHeight(Value: Single);
    property DefaultRowHeight: Single read GetDefaultRowHeight write SetDefaultRowHeight;

    function GetColumn: Cols; virtual; abstract;
    procedure SetColumn(Column: Cols); virtual; abstract;
    property Column: Cols read GetColumn write SetColumn;

    function GetRow: Rows; virtual; abstract;
    procedure SetRow(Row: Rows); virtual; abstract;
    property Row: Rows read GetRow write SetRow;

    function GetElektronikTablo2003StringTable: String; virtual; abstract;

    constructor Create;
  end;

implementation
{ BaseTable }

constructor BaseTable.Create;
begin
  inherited Create;
end;

//function BaseTable.GetColumn: Cols;
//begin
//  result := Self.Column;
//end;

//function BaseTable.GetRow: Rows;
//begin
//  result := Self.Row;
//end;

function BaseTable.GetDefaultColumnWidth: Single;
begin
  result := Self._DefaultColumnWidth;
end;

function BaseTable.GetDefaultRowHeight: Single;
begin
  result := Self._DefaultRowHeight;
end;

function BaseTable.GetExpandedColumnCount: Integer;
var
  z: Integer;
//  Comparer: IComparer<RowInt>;
  item: RowInt;
begin
//  Comparer := TDelegatedComparer<RowInt>.Construct(
//    function (const L, R: RowInt): Integer
//    begin
//      result := (L.Cell.Count - R.Cell.Count); //FullCount
//    end
//  );
//  Self.Rowss.Sort(Comparer);
  z := 0;
  for item in Self.Row do
  begin
    if z < item.Cell.Count then
      z := item.Cell.Count;

    if z < item.Cell.FullCount then
      z := item.Cell.FullCount;
  end;
//  z := FoundItem.Cell.FullCount;
//  z := (Row.OrderByDescending(Function(c) c.Cell.FullCount).FirstOrDefault).Cell.FullCount;
//  z := Self.Rowss.First.Cell.Count; //FullCount
  _ExpandedColumnCount := IfThen(z > _ExpandedColumnCount, z, _ExpandedColumnCount);
//  'For Each R As Row.Row In Me.Row
//  '    z = Row.Where(Function(c) c.Cell.Count > 0).Select(Function(d) d.Cell.FullCount).Max
//  '    _ExpandedColumnCount = If(z > _ExpandedColumnCount, z, _ExpandedColumnCount)
//  'Next
  result := _ExpandedColumnCount;
end;

function BaseTable.GetExpandedRowCount: Integer;
var
  cel: CellInt;
  z: RowInt;
  down: Integer;
begin
//  z := Self.Row.GetCells[Self.Row.Count - 1];
  z := Self.Row.InnerList[Self.Row.Count - 1];
  down := 0;
  For cel In z.Cell do
  begin
    if (cel.MergeDown > down) then
      down := cel.MergeDown;
  end;

  if z.Indexs > 0 then
    down := down + z.Indexs
  Else
    down := down + Self.Row.Count;

  _ExpandedRowCount := down;

  result := _ExpandedRowCount;
end;

function BaseTable.GetFullColumns: Boolean;
begin
  result := Self._FullColumns;
end;

function BaseTable.GetFullRows: Boolean;
begin
  result := Self._FullRows;
end;

function BaseTable.GetStyleID: String;
begin
  result := Self._StyleID;
end;

//procedure BaseTable.SetColumn(Column: Cols);
//begin
//  Self.Column := Column;
//end;

procedure BaseTable.SetDefaultColumnWidth(Value: Single);
begin
  Self._DefaultColumnWidth := Value;
end;

procedure BaseTable.SetDefaultRowHeight(Value: Single);
begin
  Self._DefaultRowHeight := Value;
end;

procedure BaseTable.SetFullColumns(Value: Boolean);
begin
  Self._FullColumns := Value;
end;

procedure BaseTable.SetFullRows(Value: Boolean);
begin
  Self._FullRows := Value;
end;

//procedure BaseTable.SetRow(Row: Rows);
//begin
//  Self.Row := Row;
//end;

procedure BaseTable.SetStyleID(Value: String);
begin
  Self._StyleID := Value;
end;

end.

