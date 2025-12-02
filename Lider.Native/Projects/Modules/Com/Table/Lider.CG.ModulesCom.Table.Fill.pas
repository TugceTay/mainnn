unit Lider.CG.ModulesCom.Table.Fill;

interface

uses
	Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

type
  Fill = class(BaseAlignmentStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Fill.GetName: String;
	begin
	  result := 'Fill';
	end;

end.

