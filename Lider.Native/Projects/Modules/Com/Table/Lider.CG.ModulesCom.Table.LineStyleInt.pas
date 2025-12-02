unit Lider.CG.ModulesCom.Table.LineStyleInt;

interface

type
  LineStyleInt = interface
    ['{B0B9333F-2C2F-4D1E-94E8-6D5A22494672}']
    function GetName: String;
    property Name: String read GetName;

    function GetWeight: Integer;
    procedure SetWeight(Weight: Integer);
    property Weight: Integer read GetWeight write SetWeight;

    function GetHexColor: String;
    procedure SetHexColor(HexColor: String);
    property HexColor: String read GetHexColor write SetHexColor;
  end;

implementation
end.

