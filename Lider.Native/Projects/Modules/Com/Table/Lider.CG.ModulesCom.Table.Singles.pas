unit Lider.CG.ModulesCom.Table.Singles;

interface

uses
	Lider.CG.ModulesCom.Table.BaseUnderlineStyle;

type
  Singles = class(BaseUnderlineStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Singles.GetName: String;
	begin
	  result := 'Single';
	end;

end.

