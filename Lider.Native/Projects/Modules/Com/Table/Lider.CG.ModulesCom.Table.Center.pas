unit Lider.CG.ModulesCom.Table.Center;

interface

uses
	Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

type
  Center = class(BaseAlignmentStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Center.GetName: String;
	begin
	  result := 'Center';
	end;

end.

