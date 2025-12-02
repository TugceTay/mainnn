unit Lider.CG.ModulesCom.Table.Roman;

interface

uses
	Lider.CG.ModulesCom.Table.BaseFamilyType;

type
  Roman = class(BaseFamilyType)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Roman.GetName: String;
	begin
	  result := 'Roman';
	end;

end.

