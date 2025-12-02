unit Lider.CG.ModulesCom.Table.Style;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.BaseStyle,
  Lider.CG.ModulesCom.Table.Alignment,
  Lider.CG.ModulesCom.Table.Borders,
  Lider.CG.ModulesCom.Table.Border,
  Lider.CG.ModulesCom.Table.Fonts,
  Lider.CG.ModulesCom.Table.Interior,
  Lider.CG.ModulesCom.Table.NumberFormat,
  Lider.CG.ModulesCom.Table.Protection,
  Lider.CG.ModulesCom.Table.EnumsAlignment,
  Lider.CG.ModulesCom.Table.EnumsFont,
  Vcl.Graphics;

type
  Style = class(BaseStyle)
    private
      _Aligment: Alignment;
      _Borders: Borders;
      _Font: Fonts;
      _Interior: Interior;
      _NumberFormat: NumberFormat;
      _Protection: Protection;
    public
      function GetAligment: Alignment; override;
      procedure SetAligment(Value: Alignment); override;
      property Aligment: Alignment read GetAligment write SetAligment;

      function GetBorders: Borders; override;
      procedure SetBorders(Value: Borders); override;
      property Borders: Borders read GetBorders write SetBorders;

      function GetFont: Fonts; override;
      procedure SetFont(Value: Fonts); override;
      property Font: Fonts read GetFont write SetFont;

      function GetInterior: Interior; override;
      procedure SetInterior(Value: Interior); override;
      property Interior: Interior read GetInterior write SetInterior;

      function GetNumberFormat: NumberFormat; override;
      procedure SetNumberFormat(Value: NumberFormat); override;
      property NumberFormat: NumberFormat read GetNumberFormat write SetNumberFormat;

      function GetProtection: Protection; override;
      procedure SetProtection(Value: Protection); override;
      property Protection: Protection read GetProtection write SetProtection;

      function AddBorder(Border: Border): Integer;

      constructor Create; overload;
      constructor Create(PId: String; PName: String); overload;
      constructor Create(PId: String; PName: String; PAligment: Alignment; PBorders: Borders; PFont: Fonts;
                         PInterior: Interior; PNumberFormat: NumberFormat; PProtection: Protection); overload;
  end;

implementation
{ Style }

function Style.AddBorder(Border: Border): Integer;
begin
  Self.Borders.Add(Border);
  result := Self.Borders.Count;
end;

constructor Style.Create;
begin
  Self.Aligment := Alignment.Create(AligmentStyle.NoneAligmentStyle, AligmentStyle.Bottom, 0,
                                    ReadingOrderStyle.NoneReadingOrderStyle, 0, False, False);
  Self.Borders := Lider.CG.ModulesCom.Table.Borders.Borders.Create;
  Self.Font := Fonts.Create(Lider.CG.ModulesCom.Table.EnumsFont.Font.Calibri, 162,
                            FontFamily.Swiss, 11, clWhite, False, False, False,
                            FontVerticalAlign.NoneFontVerticalAlign, FontUnderline.NoneFontUnderline); //Drawing.Color.
  Self.Interior := Lider.CG.ModulesCom.Table.Interior.Interior.Create(clWhite, '', clWhite);
  Self.NumberFormat := Lider.CG.ModulesCom.Table.NumberFormat.NumberFormat.Create('');
  Self.Protection := Lider.CG.ModulesCom.Table.Protection.Protection.Create(True, False);
  inherited Id := 'Default';
  inherited Name := 'Normal';
end;

constructor Style.Create(PId, PName: String; PAligment: Alignment;
  PBorders: Borders; PFont: Fonts; PInterior: Interior; PNumberFormat: NumberFormat;
  PProtection: Protection);
begin
//  Inherited Create;
  inherited Id := PId;
  inherited Name := PName;
  Self.Aligment := PAligment;
  Self.Borders := PBorders;
  Self.Font := PFont;
  Self.Interior := PInterior;
  Self.NumberFormat := PNumberFormat;
  Self.Protection := PProtection;
end;

constructor Style.Create(PId, PName: String);
begin
  Self.Aligment := Alignment.Create(AligmentStyle.NoneAligmentStyle, AligmentStyle.Bottom, 0,
                                    ReadingOrderStyle.NoneReadingOrderStyle, 0, False, False);
  Self.Borders := Lider.CG.ModulesCom.Table.Borders.Borders.create;
  Self.Font := Fonts.Create(Lider.CG.ModulesCom.Table.EnumsFont.Font.Calibri, 162, FontFamily.Swiss, 11, clWhite, False, False, False,
                            FontVerticalAlign.NoneFontVerticalAlign, FontUnderline.NoneFontUnderline); //Drawing.Color.
  Self.Interior := Lider.CG.ModulesCom.Table.Interior.Interior.Create(clWhite, '', clWhite);
  Self.NumberFormat := Lider.CG.ModulesCom.Table.NumberFormat.NumberFormat.Create('');
  Self.Protection := Lider.CG.ModulesCom.Table.Protection.Protection.Create(True, False);
  inherited Id := PId;
  inherited Name := PName;
end;

function Style.GetAligment: Alignment;
begin
  result := Self._Aligment;
end;

function Style.GetBorders: Borders;
begin
  result := Self._Borders;
end;

function Style.GetFont: Fonts;
begin
  result := Self._Font;
end;

function Style.GetInterior: Interior;
begin
  result := Self._Interior;
end;

function Style.GetNumberFormat: NumberFormat;
begin
  result := Self._NumberFormat;
end;

function Style.GetProtection: Protection;
begin
  result := Self._Protection;
end;

procedure Style.SetAligment(Value: Alignment);
begin
  Self._Aligment := Value;
end;

procedure Style.SetBorders(Value: Borders);
begin
  Self._Borders := Value;
end;

procedure Style.SetFont(Value: Fonts);
begin
  Self._Font := Value;
end;

procedure Style.SetInterior(Value: Interior);
begin
  Self._Interior := Value;
end;

procedure Style.SetNumberFormat(Value: NumberFormat);
begin
  Self._NumberFormat := Value;
end;

procedure Style.SetProtection(Value: Protection);
begin
  Self._Protection := Value;
end;

end.

