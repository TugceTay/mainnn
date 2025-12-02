unit Lider.CG.ModulesCom.Table.BaseCol;

interface

uses
  Lider.CG.ModulesCom.Table.ColInt,
  Sysutils;

type
  BaseCol = Class(TInterfacedObject, ColInt)
  private
    _AutoFitWidth: Boolean;
    _Width: Single;
    _Hidden: Boolean;
    _Span: Integer;
    _StyleId: String;

  public
    function GetIndex: Integer; virtual; abstract;
    procedure SetIndex(Value: Integer); virtual; abstract;
    property Indexs: Integer read GetIndex write SetIndex;

    function GetAutoFitWidth: Boolean;
    procedure SetAutoFitWidth(AutoFitWidth: Boolean);
    property AutoFitWidth: Boolean read GetAutoFitWidth write SetAutoFitWidth;

    function GetWidth: Single;
    procedure SetWidth(Width: Single);
    property Width: Single read GetWidth write SetWidth;

    function GetHidden: Boolean;
    procedure SetHidden(Hidden: Boolean);
    property Hidden: Boolean read GetHidden write SetHidden;

    function GetSpan: Integer;
    procedure SetSpan(Span: Integer);
    property Span: Integer read GetSpan write SetSpan;

    function GetStyleID: String;
    procedure SetStyleID(StyleID: String);
    property StyleID: String read GetStyleID write SetStyleID;

    function ToString: String; override;
    function FulType: String;
    constructor Create;
  end;

implementation
{ BaseCol }

constructor BaseCol.Create;
begin
  inherited Create;
end;

function BaseCol.FulType: String;
begin
  result := Format('%d, %d',[Self.Indexs,Self.Width]);
end;

function BaseCol.GetAutoFitWidth: Boolean;
begin
  result := Self._AutoFitWidth;
end;

function BaseCol.GetHidden: Boolean;
begin
  result := Self._Hidden;
end;

//function BaseCol.GetIndex: Integer;
//begin
//  result := Self.Index;
//end;

function BaseCol.GetSpan: Integer;
begin
  result := Self._Span;
end;

function BaseCol.GetStyleID: String;
begin
  result := Self._StyleId;
end;

function BaseCol.GetWidth: Single;
begin
  result := Self._Width;
end;

procedure BaseCol.SetAutoFitWidth(AutoFitWidth: Boolean);
begin
  Self._AutoFitWidth := AutoFitWidth;
end;

procedure BaseCol.SetHidden(Hidden: Boolean);
begin
  _Hidden := Hidden;
end;

procedure BaseCol.SetSpan(Span: Integer);
begin
  _Span := Span;
end;

procedure BaseCol.SetStyleID(StyleID: String);
begin
  _StyleId := StyleID;
end;

procedure BaseCol.SetWidth(Width: Single);
begin
  _Width := Width;
end;

function BaseCol.ToString: String;
begin
  result := Format('%d, %d',[Self.Indexs,Self.Width]);
end;

end.

