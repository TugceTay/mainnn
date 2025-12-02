unit Lider.CG.ModulesCom.Table.Solid;

interface

uses
  Lider.CG.ModulesCom.Table.BasePattern;

type
  Solid = class(BasePattern)
    private
      _Color: String;

    public
      function GetName: String; override;
      property Name: String read GetName;

      function GetHexPatternColor: String; override;
      procedure SetHexPatternColor(HexPatternColor: String); override;
      property HexPatternColor: String read GetHexPatternColor write SetHexPatternColor;
  end;

implementation
{ Solid }

function Solid.GetHexPatternColor: String;
begin
  result := Self._Color;
end;

function Solid.GetName: String;
begin
  result := 'Solid';
end;

procedure Solid.SetHexPatternColor(HexPatternColor: String);
begin
  Self._Color := HexPatternColor
end;

end.

