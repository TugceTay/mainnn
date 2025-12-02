unit Lider.CG.ModulesCom.Table.Alignment;

interface

uses
  Lider.CG.ModulesCom.Table.BaseAlignment,
  Lider.CG.ModulesCom.Table.AlignmentStyleInt,
  Lider.CG.ModulesCom.Table.EnumsAlignment,
  Lider.CG.ModulesCom.Table.Bottom,
  Lider.CG.ModulesCom.Table.Center,
  Lider.CG.ModulesCom.Table.CenterAcrossSelection,
  Lider.CG.ModulesCom.Table.Distributed,
  Lider.CG.ModulesCom.Table.Fill,
  Lider.CG.ModulesCom.Table.Justify,
  Lider.CG.ModulesCom.Table.Left,
  Lider.CG.ModulesCom.Table.Right,
  Lider.CG.ModulesCom.Table.Top,
  Lider.CG.ModulesCom.Table.RightToLeft,
  Lider.CG.ModulesCom.Table.LeftToRight,
  SysUtils,
  Vcl.Forms,
  Windows;

type
  Alignment = class(BaseAlignment)
  private
    _Horizontal: AlignmentStyleInt;
    _Vertical: AlignmentStyleInt;
    _ReadingOrder: AlignmentStyleInt;
  public
    function GetHorizontal: AlignmentStyleInt; override;
    property Horizontal: AlignmentStyleInt read GetHorizontal;

    function GetVertical: AlignmentStyleInt; override;
    property Vertical: AlignmentStyleInt read GetVertical;

    function GetReadingOrder: AlignmentStyleInt; override;
    property ReadingOrder: AlignmentStyleInt read GetReadingOrder;

    constructor Create(
    Horizontal: AligmentStyle; Vertical: AligmentStyle; Indent: Integer; ReadingOrder: ReadingOrderStyle;
    Rotate: Integer; ShrinkToFit: Boolean; WrapText: Boolean);
  end;

implementation
{ Alignment }

constructor Alignment.Create(Horizontal, Vertical: AligmentStyle;
  Indent: Integer; ReadingOrder: ReadingOrderStyle; Rotate: Integer;
  ShrinkToFit, WrapText: Boolean);
begin
  inherited Create;
  try
    case Horizontal of
      AligmentStyle.Bottom: _Horizontal := Left.create; //' Throw New Exception("Yatay Hizalama Bottom Verilemez.")
      AligmentStyle.Top: _Horizontal := Left.create; //' Throw New Exception("Yatay Hizalama Bottom Verilemez.")
      AligmentStyle.Left: _Horizontal := Left.create;
      AligmentStyle.Right: _Horizontal := Right.create;
      AligmentStyle.Center: _Horizontal := Center.create;
      AligmentStyle.Justify: _Horizontal := Justify.create;
      AligmentStyle.Distributed: _Horizontal := Distributed.create;
      AligmentStyle.Fill: _Horizontal := Fill.create;
      AligmentStyle.CenterAcrossSelection: _Horizontal := CenterAcrossSelection.create;
      else _Horizontal := Left.create;
    end;

    case Vertical of
      AligmentStyle.Bottom: _Vertical := Bottom.create;
      AligmentStyle.Top: _Vertical := Top.create;
      AligmentStyle.Left: _Vertical := Bottom.create; //' Throw New Exception("Dikey Hizalama Left Verilemez.")
      AligmentStyle.Right: _Vertical := Bottom.create; //' Throw New Exception("Dikey Hizalama Right Verilemez.")
      AligmentStyle.Center: _Vertical := Center.create;
      AligmentStyle.Justify: _Vertical := Justify.create;
      AligmentStyle.Distributed: _Vertical := Distributed.create;
      AligmentStyle.Fill: raise Exception.Create('Dikey Hizalama Fill Verilemez.');
      AligmentStyle.CenterAcrossSelection:
      begin
        _Vertical := Bottom.create;
        raise Exception.Create('Dikey Hizalama CenterAcrossSelection Verilemez.');
      end
      else _Vertical := Bottom.create;
    end;

    case ReadingOrder of
      ReadingOrderStyle.RightToLeft: _ReadingOrder := RightToLeft.create;
      ReadingOrderStyle.LeftToRight: _ReadingOrder := LeftToRight.create;
    end;

    Self.Indent := Indent;
    Self.Rotate := Rotate;
    Self.ShrinkToFit := ShrinkToFit;
    Self.WrapText := WrapText;

  except on e: Exception do
    Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
  end;
end;

function Alignment.GetHorizontal: AlignmentStyleInt;
begin
  result := Self._Horizontal;
end;

function Alignment.GetReadingOrder: AlignmentStyleInt;
begin
  result := Self._ReadingOrder;
end;

function Alignment.GetVertical: AlignmentStyleInt;
begin
  result := Self._Vertical;
end;

end.

