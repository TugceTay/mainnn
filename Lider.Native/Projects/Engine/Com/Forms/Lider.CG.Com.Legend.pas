unit Lider.CG.Com.Legend;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  Lider.CG.Com.Columns,
  Lider.CG.Com.Thematics,
  Lider.CG.Com.DrawToolsInt;

type
  TfmLegend = class(TForm)
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Legend1: TlicgLegend;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

{$R *.DFM}


{ TfmLegend }

procedure TfmLegend.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TfmLegend.FormResize(Sender: TObject);
begin
  Legend1.AdjustColWidths;
end;

procedure TfmLegend.FormCreate(Sender: TObject);
begin
  Legend1 := TlicgLegend.Create(self);
  Legend1.Parent := Self;

  with Legend1 do
  begin
    ParentFont := False;
    Left := 0;
    Top := 0;
    Width := 209;
    Height := 190;
    PenTool.Style := 0;
    PenTool.Color := clBlack;
    PenTool.Flow := False;
    BrushTool.Pattern := 0;
    BrushTool.ForeColor := clBlack;
    BrushTool.BackColor := clBlack;
    BrushTool.FillFactor := 1;
    BrushTool.GradientDirection := fdTopBottom;
    BorderWidth := 1;
    LoweredColor := clGray;
    Font.Charset := TURKISH_CHARSET;
    TitleFont.Charset := TURKISH_CHARSET;
    TitleFont.Color := clWindowText;
    TitleFont.Height := -13;
    TitleFont.Name := 'MS Sans Serif';
    TitleFont.Style := [];
    TitleColor := clBlack;
    TitleTransparent := False;
    TitleAlignment := taCenter;
    Transparent := False;
    //LegendStyle := ctLabel;

    with LegendRanges.Add do
    begin
      Legend := 'First legend';
      Frequency := 0;
      PenStyle.Style := 0;
      PenStyle.Color := clBlack;
      PenStyle.Flow := False;
      BrushStyle.Pattern := 1;
      BrushStyle.ForeColor := clBlack;
      BrushStyle.BackColor := clBlack;
      BrushStyle.FillFactor := 1;
      BrushStyle.GradientDirection := fdTopBottom;
      SymbolStyle.Index := 0;
      Color := clBlack;
      ImageIndex := 0;
    end;
    with LegendRanges.Add do
    begin
      Legend := 'Second legend';
      Frequency := 0;
      PenStyle.Style := 0;
      PenStyle.Color := clBlack;
      PenStyle.Flow := False;
      BrushStyle.Pattern := 1;
      BrushStyle.ForeColor := clBlack;
      BrushStyle.BackColor := clBlack;
      BrushStyle.FillFactor := 1;
      BrushStyle.GradientDirection := fdTopBottom;
      SymbolStyle.Index := 0;
      Color := clMaroon;
      ImageIndex := 1;
    end;
    with LegendRanges.Add do
    begin
      Legend := 'Third legend';
      Frequency := 0;
      PenStyle.Style := 0;
      PenStyle.Color := clBlack;
      PenStyle.Flow := False;
      BrushStyle.Pattern := 1;
      BrushStyle.ForeColor := clBlack;
      BrushStyle.BackColor := clBlack;
      BrushStyle.FillFactor := 1;
      BrushStyle.GradientDirection := fdTopBottom;
      SymbolStyle.Index := 0;
      Color := clGreen;
      ImageIndex := 2;
    end;
    with LegendRanges.Add do
    begin
      Legend := 'Fourth legend';
      Frequency := 0;
      PenStyle.Style := 0;
      PenStyle.Color := clBlack;
      PenStyle.Flow := False;
      BrushStyle.Pattern := 1;
      BrushStyle.ForeColor := clBlack;
      BrushStyle.BackColor := clBlack;
      BrushStyle.FillFactor := 1;
      BrushStyle.GradientDirection := fdTopBottom;
      SymbolStyle.Index := 0;
      Color := clOlive;
      ImageIndex := 3;
    end;
    with LegendRanges.Add do
    begin
      Legend := 'Fifth legend';
      Frequency := 0;
      PenStyle.Style := 0;
      PenStyle.Color := clBlack;
      PenStyle.Flow := False;
      BrushStyle.Pattern := 1;
      BrushStyle.ForeColor := clBlack;
      BrushStyle.BackColor := clBlack;
      BrushStyle.FillFactor := 1;
      BrushStyle.GradientDirection := fdTopBottom;
      SymbolStyle.Index := 0;
      Color := clNavy;
      ImageIndex := 4;
    end;
    with LegendRanges.Add do
    begin
      Legend := 'Sixth legend';
      Frequency := 0;
      PenStyle.Style := 0;
      PenStyle.Color := clBlack;
      PenStyle.Flow := False;
      BrushStyle.Pattern := 1;
      BrushStyle.ForeColor := clBlack;
      BrushStyle.BackColor := clBlack;
      BrushStyle.FillFactor := 1;
      BrushStyle.GradientDirection := fdTopBottom;
      SymbolStyle.Index := 0;
      Color := clPurple;
      ImageIndex := 5;
    end;

    ShowTitle := True;
    Title0 := 'Özellik';
    Title1 := 'Lejant';
    Stretch := False;
    Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing];
    Align := alClient;

    BorderStyle := bsNone;
    Font.Charset := TURKISH_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -9;
    Font.Name := 'Tahoma';
    Font.Style := [];
    ParentFont := False;
    TabOrder := 0;
    //ColWidths := (
      //46
      //78
      //67);
  end;

end;

end.


