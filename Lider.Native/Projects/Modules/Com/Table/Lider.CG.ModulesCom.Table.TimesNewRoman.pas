unit Lider.CG.ModulesCom.Table.TimesNewRoman;

interface

uses
	Lider.CG.ModulesCom.Table.BaseFontType;

type
  TimesNewRoman = class(BaseFontType)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
	
	function TimesNewRoman.GetName: String;
	begin
	  result := 'Times New Roman';
	end;

end.

