unit Lider.CG.ModulesCom.Table.ArialBlack;

interface

uses
	Lider.CG.ModulesCom.Table.BaseFontType;

type
  ArialBlack = class(BaseFontType)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function ArialBlack.GetName: String;
	begin
	  result := 'Arial Black';
	end;

end.

