unit Lider.CG.ModulesCom.Table.Bottom;

interface

uses
	Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

type
  Bottom = class(BaseAlignmentStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Bottom.GetName: String;
	begin
	  result := 'Bottom';
	end;

end.

