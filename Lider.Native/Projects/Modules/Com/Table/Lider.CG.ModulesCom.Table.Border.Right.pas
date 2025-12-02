unit Lider.CG.ModulesCom.Table.Border.Right;

interface

uses
	Lider.CG.ModulesCom.Table.BasePosition;

type
  Right = class(BasePosition)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Right.GetName: String;
	begin
	  result := 'Right';
	end;

end.

