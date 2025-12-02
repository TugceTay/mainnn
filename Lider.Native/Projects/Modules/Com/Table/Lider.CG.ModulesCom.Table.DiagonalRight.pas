unit Lider.CG.ModulesCom.Table.DiagonalRight;

interface

uses
	Lider.CG.ModulesCom.Table.BasePosition;

type
  DiagonalRight = class(BasePosition)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function DiagonalRight.GetName: String;
	begin
	  result := 'DiagonalRight';
	end;

end.

