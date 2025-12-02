unit Lider.CG.ModulesCom.Table.Border.Bottom;

interface

uses
	Lider.CG.ModulesCom.Table.BasePosition;

type
  Bottom = class(BasePosition)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Bottom.GetName: String;
	begin
	  result := 'Bottom';
	end;

end.

