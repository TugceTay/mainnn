unit Lider.CG.ModulesCom.Table.ProtectionInt;

interface

type
  ProtectionInt = interface
    ['{62ACFF17-0816-4550-BF38-E9F1158C204B}']
    function GetProtectet: Boolean;
    procedure SetProtectet(Protectet: Boolean);
    property Protectet: Boolean read GetProtectet write SetProtectet;

    function GetHideFormula: Boolean;
    procedure SetHideFormula(HideFormula: Boolean);
    property HideFormula: Boolean read GetHideFormula write SetHideFormula;

    function GetElektronikTablo2003String: String;
  end;

implementation
end.

