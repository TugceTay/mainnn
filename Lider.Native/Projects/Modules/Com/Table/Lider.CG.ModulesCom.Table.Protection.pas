unit Lider.CG.ModulesCom.Table.Protection;

interface

uses
	Lider.CG.ModulesCom.Table.BaseProtection;

type
  Protection = class(BaseProtection)
    constructor Create(Protectet: Boolean; HideFormula: Boolean);
  end;

implementation
{ Protection }

constructor Protection.Create(Protectet: Boolean; HideFormula: Boolean);
begin
  inherited Create;
  inherited HideFormula := HideFormula;
  inherited Protectet := Protectet;
end;

end.

