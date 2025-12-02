unit Lider.CG.ModulesCom.Table.Border.Left;

interface

uses
	Lider.CG.ModulesCom.Table.BasePosition;

type
  Left = class(BasePosition)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Left.GetName: String;
	begin
	  result := 'Left';
	end;

end.

