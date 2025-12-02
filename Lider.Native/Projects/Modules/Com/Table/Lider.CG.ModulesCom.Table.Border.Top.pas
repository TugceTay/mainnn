unit Lider.CG.ModulesCom.Table.Border.Top;

interface

uses
	Lider.CG.ModulesCom.Table.BasePosition;

type
  Top = class(BasePosition)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Top.GetName: String;
	begin
	  result := 'Top';
	end;

end.

