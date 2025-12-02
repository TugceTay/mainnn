unit Lider.CG.ModulesCom.Table.StyleInt;

interface

uses
  Lider.CG.ModulesCom.Table.Alignment,
  Lider.CG.ModulesCom.Table.Border,
  Lider.CG.ModulesCom.Table.Borders,
  Lider.CG.ModulesCom.Table.Fonts,
  Lider.CG.ModulesCom.Table.Interior,
  Lider.CG.ModulesCom.Table.NumberFormat,
  Lider.CG.ModulesCom.Table.Protection;

type
  StyleInt = interface
    ['{FC9B9A94-5E71-4474-B838-08A218BFCCAE}']
    function GetName: String;
    procedure SetName(Value: String);
    property Name: String read GetName write SetName;

    function GetId: String;
    procedure SetId(Value: String);
    property Id: String read GetId write SetId;

    function GetAligment: Alignment;
    procedure SetAligment(Aligment: Alignment);
    property Aligment: Alignment read GetAligment write SetAligment;

    function GetBorders: Borders;
    procedure SetBorders(Borders: Borders);
    property Borders: Borders read GetBorders write SetBorders;

    function GetFont: Fonts;
    procedure SetFont(Font: Fonts);
    property Font: Fonts read GetFont write SetFont;

    function GetInterior: Interior;
    procedure SetInterior(Interior: Interior);
    property Interior: Interior read GetInterior write SetInterior;

    function GetNumberFormat: NumberFormat;
    procedure SetNumberFormat(NumberFormat: NumberFormat);
    property NumberFormat: NumberFormat read GetNumberFormat write SetNumberFormat;

    function GetProtection: Protection;
    procedure SetProtection(Protection: Protection);
    property Protection: Protection read GetProtection write SetProtection;

    function GetElektronikTablo2003String: String;
  end;

implementation
end.

