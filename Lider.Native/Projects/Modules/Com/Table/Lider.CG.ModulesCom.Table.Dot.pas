unit Lider.CG.ModulesCom.Table.Dot;

interface

uses
  Lider.CG.ModulesCom.Table.BaseLineType;

type
  Dot = class(BaseLineType)

    private
      _Weight: Integer;
      _Color: String;
    public
      function GetName: String; override;
      property Name: String read GetName;

      function GetWeight: Integer; override;
      procedure SetWeight(Weight: Integer); override;
      property Weight: Integer read GetWeight write SetWeight;

      function GetHexColor: String; override;
      procedure SetHexColor(HexColor: String); override;
      property HexColor: String read GetHexColor write SetHexColor;
  end;

implementation
{ Dot }

function Dot.GetHexColor: String;
begin
  result := Self._Color;
end;

function Dot.GetName: String;
begin
  result := 'Dot';
end;

function Dot.GetWeight: Integer;
begin
  result := Self._Weight;
end;

procedure Dot.SetHexColor(HexColor: String);
begin
  Self._Color := HexColor;
end;

procedure Dot.SetWeight(Weight: Integer);
begin
  Self._Weight := Weight;
end;

end.

