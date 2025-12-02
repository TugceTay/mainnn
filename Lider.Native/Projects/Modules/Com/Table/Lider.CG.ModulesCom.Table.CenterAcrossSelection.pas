unit Lider.CG.ModulesCom.Table.CenterAcrossSelection;

interface

uses
	Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

type
  CenterAcrossSelection = class(BaseAlignmentStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function CenterAcrossSelection.GetName: String;
	begin
	  result := 'CenterAcrossSelection';
	end;

end.

