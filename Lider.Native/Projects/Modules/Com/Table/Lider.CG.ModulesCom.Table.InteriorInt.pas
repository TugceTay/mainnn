unit Lider.CG.ModulesCom.Table.InteriorInt;

interface

uses
    Lider.CG.ModulesCom.Table.PatternInt;

type
  InteriorInt = interface
    ['{FD5693CF-C48C-4CF3-847E-6B69489AC6E8}']
    function GetPattern: PatternInt;
    property Pattern: PatternInt read GetPattern;

    function GetHexColor: String;
    procedure SetHexColor(HexColor: String);
    property HexColor: String read GetHexColor write SetHexColor;

    function GetElektronikTablo2003String: String;
  end;

implementation
end.

