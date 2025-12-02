unit Lider.CG.ModulesCom.Table.Right;

interface

uses
	Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

type
  Right = class(BaseAlignmentStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Right.GetName: String;
	begin
	  result := 'Right';
	end;

end.

