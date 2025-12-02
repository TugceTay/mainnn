unit Lider.CG.ModulesCom.Table.Font.Doubles;

interface

uses
	Lider.CG.ModulesCom.Table.BaseUnderlineStyle;

type
  Doubles = class(BaseUnderlineStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Doubles.GetName: String;
	begin
	  result := 'Double';
	end;

end.

