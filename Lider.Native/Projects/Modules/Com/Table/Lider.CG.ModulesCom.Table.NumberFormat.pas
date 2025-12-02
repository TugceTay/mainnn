unit Lider.CG.ModulesCom.Table.NumberFormat;

interface

uses
	Lider.CG.ModulesCom.Table.BaseNumberFormat;

type
  NumberFormat = class(BaseNumberFormat)
    constructor Create(NumberFormat: String);
  end;

implementation
{ NumberFormat }

constructor NumberFormat.Create(NumberFormat: String);
begin
  inherited Create;
  inherited Format := NumberFormat;
end;

end.

