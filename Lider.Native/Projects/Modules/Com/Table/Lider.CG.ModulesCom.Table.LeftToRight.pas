unit Lider.CG.ModulesCom.Table.LeftToRight;

interface

uses
	Lider.CG.ModulesCom.Table.BaseReadingOrder;

type
  LeftToRight = class(BaseReadingOrder)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function LeftToRight.GetName: String;
	begin
	  result := 'LeftToRight';
	end;

end.

