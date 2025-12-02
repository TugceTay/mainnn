unit Lider.CG.ModulesCom.Table.Calibri;

interface

uses
	Lider.CG.ModulesCom.Table.BaseFontType;

type
  Calibri = class(BaseFontType)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function Calibri.GetName: String;
	begin
	  result := 'Calibri';
	end;

end.

