unit Lider.CG.ModulesCom.Table.Justify;

interface

uses
	Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

type
  Justify = class(BaseAlignmentStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Justify.GetName: String;
	begin
	  result := 'Justify';
	end;

end.

