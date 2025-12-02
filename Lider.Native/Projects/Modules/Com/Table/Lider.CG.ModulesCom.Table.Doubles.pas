unit Lider.CG.ModulesCom.Table.Doubles;

interface

uses
  Lider.CG.ModulesCom.Table.BaseLineType;

type
  Doubles = class(BaseLineType)

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
{ Doubles }

function Doubles.GetHexColor: String;
begin
  result := Self._Color;
end;

function Doubles.GetName: String;
begin
  result := 'Doubles';
end;

function Doubles.GetWeight: Integer;
begin
  result := Self._Weight;
end;

procedure Doubles.SetHexColor(HexColor: String);
begin
  Self._Color := HexColor;
end;

procedure Doubles.SetWeight(Weight: Integer);
begin
  Self._Weight := Weight;
end;

end.

