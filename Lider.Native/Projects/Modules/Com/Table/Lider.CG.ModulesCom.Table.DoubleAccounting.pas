unit Lider.CG.ModulesCom.Table.DoubleAccounting;

interface

uses
	Lider.CG.ModulesCom.Table.BaseUnderlineStyle;

type
  DoubleAccounting = class(BaseUnderlineStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function DoubleAccounting.GetName: String;
	begin
	  result := 'DoubleAccounting';
	end;

end.

