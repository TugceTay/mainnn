unit Lider.CG.ModulesCom.Table.NumberFormatInt;

interface

type
  NumberFormatInt = interface
    ['{A82B6FB6-1179-4020-9AC3-4FB1F7930071}']
    function GetFormat: String;
    procedure SetFormat(Format: String);
    property Format: String read GetFormat write SetFormat;

    function GetElektronikTablo2003String: String;
  end;

implementation
end.

