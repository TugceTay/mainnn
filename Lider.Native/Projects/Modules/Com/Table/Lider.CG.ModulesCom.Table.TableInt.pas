unit Lider.CG.ModulesCom.Table.TableInt;

interface

uses
  Lider.CG.ModulesCom.Table.Columns,
  Lider.CG.ModulesCom.Table.Rows;

type
  TableInt = interface
    ['{FDC03051-303E-46C9-A478-BC75C646CE2B}']
    function GetExpandedColumnCount: Integer;
    property ExpandedColumnCount: Integer read GetExpandedColumnCount;

    function GetExpandedRowCount: Integer;
    property ExpandedRowCount: Integer read GetExpandedRowCount;

    function GetFullColumns: Boolean;
    procedure SetFullColumns(FullColumns: Boolean);
    property FullColumns: Boolean read GetFullColumns write SetFullColumns;

    function GetFullRows: Boolean;
    procedure SetFullRows(FullRows: Boolean);
    property FullRows: Boolean read GetFullRows write SetFullRows;

    function GetStyleID: String;
    procedure SetStyleID(StyleID: String);
    property StyleID: String read GetStyleID write SetStyleID;

    function GetDefaultColumnWidth: Single;
    procedure SetDefaultColumnWidth(DefaultColumnWidth: Single);
    property DefaultColumnWidth: Single read GetDefaultColumnWidth write SetDefaultColumnWidth;

    function GetDefaultRowHeight: Single;
    procedure SetDefaultRowHeight(DefaultRowHeight: Single);
    property DefaultRowHeight: Single read GetDefaultRowHeight write SetDefaultRowHeight;

    function GetColumn: Cols;
    procedure SetColumn(Column: Cols);
    property Column: Cols read GetColumn write SetColumn;

    function GetRow: Rows;
    procedure SetRow(Row: Rows);
    property Row: Rows read GetRow write SetRow;

    function GetElektronikTablo2003StringTable: String;
  end;

implementation
end.

