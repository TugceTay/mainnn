unit Lider.CG.ModulesCom.Table.DiagonalLeft;

interface

uses
	Lider.CG.ModulesCom.Table.BasePosition;

type
  DiagonalLeft = class(BasePosition)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function DiagonalLeft.GetName: String;
	begin
	  result := 'DiagonalLeft';
	end;

end.

