unit Lider.CG.ModulesCom.Table.Superscrip;

interface

uses
	Lider.CG.ModulesCom.Table.BaseVerticalAlignStyle;

type
  Superscrip = class(BaseVerticalAlignStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Superscrip.GetName: String;
	begin
	  result := 'Superscrip';
	end;

end.

