unit Lider.CG.ModulesCom.Table.Subscrip;

interface

uses
	Lider.CG.ModulesCom.Table.BaseVerticalAlignStyle;

type
  Subscrip = class(BaseVerticalAlignStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Subscrip.GetName: String;
	begin
	  result := 'Subscrip';
	end;

end.

