unit Lider.CG.ModulesCom.Table.Distributed;

interface

uses
	Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

type
  Distributed = class(BaseAlignmentStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Distributed.GetName: String;
	begin
	  result := 'Distributed';
	end;

end.

