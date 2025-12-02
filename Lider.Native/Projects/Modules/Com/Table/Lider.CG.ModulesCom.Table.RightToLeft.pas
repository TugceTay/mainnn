unit Lider.CG.ModulesCom.Table.RightToLeft;

interface

uses
	Lider.CG.ModulesCom.Table.BaseReadingOrder;

type
  RightToLeft = class(BaseReadingOrder)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function RightToLeft.GetName: String;
	begin
	  result := 'RightToLeft';
	end;

end.

