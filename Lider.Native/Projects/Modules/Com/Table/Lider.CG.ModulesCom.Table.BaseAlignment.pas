unit Lider.CG.ModulesCom.Table.BaseAlignment;

interface

uses
  Lider.CG.ModulesCom.Table.AlignmentInt,
  Lider.CG.ModulesCom.Table.AlignmentStyleInt,
  Lider.CG.ModulesCom.Table.Fill,
  SysUtils;

type
  BaseAlignment = class(TInterfacedObject, AlignmentInt)
  private
    _Indent: Integer;
    _Rotate: Integer;
    _ShrinkToFit: Boolean;
    _WrapText: Boolean;

    function HasHorizontalAlign: Boolean;
    function HasVerticalAlign: Boolean;
    function HasReadingOrder: Boolean;
    function HasIndent: Boolean;
    function HasRotate: Boolean;
    function IsWrapText: Boolean;
    function IsShrinkToFit: Boolean;
    function IsHorizontalFill: Boolean;
  public
    function GetHorizontal: AlignmentStyleInt; virtual; abstract;
    property Horizontal: AlignmentStyleInt read GetHorizontal;

    function GetVertical: AlignmentStyleInt; virtual; abstract;
    property Vertical: AlignmentStyleInt read GetVertical;

    function GetReadingOrder: AlignmentStyleInt; virtual; abstract;
    property ReadingOrder: AlignmentStyleInt read GetReadingOrder;

    function GetIndent: Integer;
    procedure SetIndent(Indent: Integer);
    property Indent: Integer read GetIndent write SetIndent;

    function GetRotate: Integer;
    procedure SetRotate(Rotate: Integer);
    property Rotate: Integer read GetRotate write SetRotate;

    function GetShrinkToFit: Boolean;
    procedure SetShrinkToFit(ShrinkToFit: Boolean);
    property ShrinkToFit: Boolean read GetShrinkToFit write SetShrinkToFit;

    function GetWrapText: Boolean;
    procedure SetWrapText(WrapText: Boolean);
    property WrapText: Boolean read GetWrapText write SetWrapText;

    function GetElektronikTablo2003String: String;

    function ToString: String; override;
  end;

implementation
{ BaseAlignment }

//function BaseAlignment.GetHorizontal: AlignmentStyleInt;
//begin
//  result := Self.Horizontal;
//end;

function BaseAlignment.GetIndent: Integer;
begin
  result := Self._Indent;
end;

//function BaseAlignment.GetReadingOrder: AlignmentStyleInt;
//begin
//  result := Self.ReadingOrder;
//end;

function BaseAlignment.GetRotate: Integer;
begin
  result := Self._Rotate;
end;

function BaseAlignment.GetShrinkToFit: Boolean;
begin
  result := Self._ShrinkToFit;
end;

//function BaseAlignment.GetVertical: AlignmentStyleInt;
//begin
//  result := Self.Vertical;
//end;

function BaseAlignment.GetWrapText: Boolean;
begin
  result := Self._WrapText;
end;

function BaseAlignment.HasHorizontalAlign: Boolean;
begin
  result := False;
  if not (Self.Horizontal = nil) then
    if not (String.IsNullOrEmpty(Self.Horizontal.Name)) then
      result := True;
end;

function BaseAlignment.HasIndent: Boolean;
begin
  result := False;
  if Self.Indent <> 0 then
    result := True;
end;

function BaseAlignment.HasReadingOrder: Boolean;
begin
  result := False;
  if not (Self.ReadingOrder = nil) then
    if not (String.IsNullOrEmpty(Self.ReadingOrder.Name)) then
      result := True;
end;

function BaseAlignment.HasRotate: Boolean;
begin
  result := False;
  if Self.Rotate <> 0 then
    result := True;
end;

function BaseAlignment.HasVerticalAlign: Boolean;
begin
  result := False;
  if not (Self.Vertical = nil) then
    if not (String.IsNullOrEmpty(Self.Vertical.Name)) then
      result := True;
end;

function BaseAlignment.IsHorizontalFill: Boolean;
begin
  result := False;
  if (Horizontal.Name = Fill.Create.Name) then
    result := True;
end;

function BaseAlignment.IsShrinkToFit: Boolean;
begin
  result := Self.ShrinkToFit;
end;

function BaseAlignment.IsWrapText: Boolean;
begin
  result := Self.WrapText;
end;

procedure BaseAlignment.SetIndent(Indent: Integer);
begin
  Self._Indent := Indent;
end;

procedure BaseAlignment.SetRotate(Rotate: Integer);
begin
  Self._Rotate := Rotate;
end;

procedure BaseAlignment.SetShrinkToFit(ShrinkToFit: Boolean);
begin
  Self._ShrinkToFit := ShrinkToFit;
end;

procedure BaseAlignment.SetWrapText(WrapText: Boolean);
begin
  Self._WrapText := WrapText;
end;

function BaseAlignment.ToString: String;
begin
  result := format('%s, %s',[Self.Horizontal.Name,Self.Vertical.Name]);
end;

function BaseAlignment.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
  a: String;
begin
  strExcelXml := TStringBuilder.Create;
  a := '';
  try
      strExcelXml.Append('<Alignment').ToString;

      if HasHorizontalAlign then
          strExcelXml.Append(' ss:Horizontal="' + Horizontal.Name + '"');

      if HasVerticalAlign then
          strExcelXml.Append(' ss:Vertical="' + Vertical.Name + '"');

      if Not IsHorizontalFill then
      begin
          if HasIndent then
              strExcelXml.Append(' ss:Indent="' + Indent.ToString + '"');
      end;

      if HasReadingOrder then
          strExcelXml.Append(' ss:ReadingOrder="' + ReadingOrder.Name + '"');

      if Not HasIndent then
      begin
          if (Rotate <> 0) then
              strExcelXml.Append(' ss:Rotate="' + Rotate.ToString + '"');
      end;

      if Not IsHorizontalFill then
      begin
          if IsShrinkToFit then
              strExcelXml.Append(' ss:ShrinkToFit="' + '1' + '"');
      end;

      if IsWrapText then
          strExcelXml.Append(' ss:WrapText="' + '1' + '"');

      strExcelXml.Append('/>');
  except on e: Exception do
      strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

end.

