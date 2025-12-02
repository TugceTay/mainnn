unit Lider.CG.ModulesCom.Table.PatternInt;

interface

type
  PatternInt = interface
    ['{DAE00D5A-C7F8-462C-B779-9E54FD319C07}']
    function GetName: String;
    property Name: String read GetName;

    function GetHexPatternColor: String;
    procedure SetHexPatternColor(HexPatternColor: String);
    property HexPatternColor: String read GetHexPatternColor write SetHexPatternColor;
  end;

implementation
end.

