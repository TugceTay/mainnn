unit Lider.CG.ModulesCom.Table.Top;

interface

uses
	Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

type
  Top = class(BaseAlignmentStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Top.GetName: String;
	begin
	  result := 'Top';
	end;

end.

