unit Lider.CG.ModulesCom.Table.Swiss;

interface

uses
	Lider.CG.ModulesCom.Table.BaseFamilyType;

type
  Swiss = class(BaseFamilyType)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Swiss.GetName: String;
	begin
	  result := 'Swiss';
	end;

end.

