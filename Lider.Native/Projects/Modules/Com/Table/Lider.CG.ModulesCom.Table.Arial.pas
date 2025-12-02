unit Lider.CG.ModulesCom.Table.Arial;

interface

uses
	Lider.CG.ModulesCom.Table.BaseFontType;

type
  Arial = class(BaseFontType)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Arial.GetName: String;
	begin
	  result := 'Arial';
	end;

end.

