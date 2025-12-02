unit Lider.CG.ModulesCom.Table.SingleAccounting;

interface

uses
	Lider.CG.ModulesCom.Table.BaseUnderlineStyle;

type
  SingleAccounting = class(BaseUnderlineStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function SingleAccounting.GetName: String;
	begin
	  result := 'SingleAccounting';
	end;

end.

