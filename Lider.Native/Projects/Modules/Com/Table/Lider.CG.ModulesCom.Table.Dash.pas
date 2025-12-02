unit Lider.CG.ModulesCom.Table.Dash;

interface

uses
  Lider.CG.ModulesCom.Table.BaseLineType;

type
  Dash = class(BaseLineType)

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
{ Dash }

function Dash.GetHexColor: String;
begin
  result := Self._Color;
end;

function Dash.GetName: String;
begin
  result := 'Dash';
end;

function Dash.GetWeight: Integer;
begin
  result := Self._Weight;
end;

procedure Dash.SetHexColor(HexColor: String);
begin
  Self._Color := HexColor;
end;

procedure Dash.SetWeight(Weight: Integer);
begin
  Self._Weight := Weight;
end;

end.

