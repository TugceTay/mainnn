unit Lider.CG.ModulesCom.Table.SlantDashDot;

interface

uses
  Lider.CG.ModulesCom.Table.BaseLineType;

type
  SlantDashDot = class(BaseLineType)

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
{ SlantDashDot }

function SlantDashDot.GetHexColor: String;
begin
  result := Self._Color;
end;

function SlantDashDot.GetName: String;
begin
  result := 'SlantDashDot';
end;

function SlantDashDot.GetWeight: Integer;
begin
  result := Self._Weight;
end;

procedure SlantDashDot.SetHexColor(HexColor: String);
begin
  Self._Color := HexColor;
end;

procedure SlantDashDot.SetWeight(Weight: Integer);
begin
  Self._Weight := Weight;
end;

end.

