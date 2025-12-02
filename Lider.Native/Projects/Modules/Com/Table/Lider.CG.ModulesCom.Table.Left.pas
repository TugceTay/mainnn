unit Lider.CG.ModulesCom.Table.Left;

interface

uses
	Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

type
  Left = class(BaseAlignmentStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Left.GetName: String;
	begin
	  result := 'Left';
	end;

end.

