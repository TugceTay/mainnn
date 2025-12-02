unit Lider.CG.ModulesCom.Table.WorksheetInt;

interface

uses
  Lider.CG.ModulesCom.Table.Styles,
  Lider.CG.ModulesCom.Table.TableInt;

type
  WorksheetInt = interface
    ['{EE12A027-C317-4A8A-BC5D-24483FF022A4}']
    function GetName: String;
    procedure SetName(Name: String);
    property Name: String read GetName write SetName;

    function GetTable: TableInt;
    procedure SetTable(Table: TableInt);
    property Table: TableInt read GetTable write SetTable;

    function GetStyle: Styles;
    procedure SetStyle(Stye: Styles);
    property Style: Styles read GetStyle write SetStyle;

    function GetElektronikTablo2003String: String;
  end;

implementation
end.

