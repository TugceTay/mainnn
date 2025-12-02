unit Lider.CG.Com.Columns;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Classes,
  Windows,
  Controls,
  SysUtils,
  Graphics,
  Printers,
  Forms,
  ActiveX,
  Math,
  OleCtnrs,
  XQSparseArray,
  Lider.CG.Com.Base,
  Lider.CG.Com.Lib,
  Lider.CG.Com.GIS,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.EntityInt;

type
  TlicgColumnItem = class;

  TlicgColumnList = class;

  TlicgTableBorderStyle = packed record
    Visible: Boolean;
    Style: Byte;
    Color: TColor;
    Width: Double;
  end;

  TlicgTableDrawCellEvent = procedure(Sender: TObject; ACol, ARow: Longint;
    Canvas: TCanvas; Grapher: TlicgBaseGrapher; Rect: TRect) of object;

  TlicgColumnType = (ctLabel, ctColor, ctLineStyle, ctBrushStyle, ctSymbolStyle,
    ctBitmap, ctVectBrush);

  TlicgColumnList = class
  private
    FItems: TList;
    FTable: TObject;
    function GetCount: Integer;
    function GetItem(Index: Integer): TlicgColumnItem;
  public
    constructor Create(Table: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TlicgColumnList);
    function Add: TlicgColumnItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Up(Index: Integer): Boolean;
    function Down(Index: Integer): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TlicgColumnItem read GetItem; default;
  end;

  TlicgGridOptions = set of (ezgoHorzLine, ezgoVertLine);

  TlicgColumnsType = (ctHeaderColumn, ctDataColumn, ctFooterColumn);

  TlicgTitleItem = class
  private
    FColumnItem: TlicgColumnItem;
    FCaption: string;
    FAlignment: TAlignment;
    FColor: TColor;
    FTransparent: Boolean;
    FFont: TlicgFontStyle;
    function GetAlignment: TAlignment;
    function GetCaption: string;
    function GetColor: TColor;
    function GetFont: TlicgFontStyle;
    function GetTransparent: Boolean;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TlicgFontStyle);
    procedure SetTransparent(const Value: Boolean);
  public
    constructor Create(ColumnItem: TlicgColumnItem);
    property Caption: string read GetCaption write SetCaption;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Color: TColor read GetColor write SetColor;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Font: TlicgFontStyle read GetFont write SetFont;
  end;

  TlicgColumnItem = class
  private
    FColumnList: TlicgColumnList;
    FColumnType: TlicgColumnType;
    FStrings: TStrings;
    FAlignment: TAlignment;
    FColor: TColor;
    FForeColor: TColor;
    FTransparent: Boolean;
    FFont: TlicgFontStyle;
    FTitle: TlicgTitleItem;
    FWidth: Double;
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetForeColor: TColor;
    function GetColumnType: TlicgColumnType;
    function GetFont: TlicgFontStyle;
    function GetStrings: TStrings;
    function GetTitle: TlicgTitleItem;
    function GetTransparent: Boolean;
    function GetWidth: Double;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetColumnType(const Value: TlicgColumnType);
    procedure SetFont(const Value: TlicgFontStyle);
    procedure SetTitle(const Value: TlicgTitleItem);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWidth(const Value: Double);
  public
    constructor Create(ColumnList: TlicgColumnList);
    destructor Destroy; override;
    property ColumnType: TlicgColumnType read GetColumnType write SetColumnType;
    property Strings: TStrings read GetStrings;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Color: TColor read GetColor write SetColor;
    property ForeColor: TColor read GetForeColor write SetForeColor;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Font: TlicgFontStyle read GetFont write SetFont;
    property Title: TlicgTitleItem read GetTitle write SetTitle;
    property Width: Double read GetWidth write SetWidth;
  end;

implementation

uses
  Lider.CG.Com.System,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Graphics,
  Lider.CG.Com.Consts,
  Lider.CG.Com.StringArray,
  Lider.CG.Com.Expr;

{ ----- TlicgTitleItem ----- }

constructor TlicgTitleItem.Create(ColumnItem: TlicgColumnItem);
begin
  inherited Create;
  FColumnItem := ColumnItem;
  FColor := clWhite;
  FFont.Name := 'Arial'; // ilker deðiþtirme Lider.CG.Com.System.DefaultFontName;
  FFont.Height := 1;
  FFont.Angle := 0;
end;

function TlicgTitleItem.GetAlignment: TAlignment;
begin
  Result := FAlignment
end;

function TlicgTitleItem.GetCaption: string;
begin
  Result := FCaption
end;

function TlicgTitleItem.GetColor: TColor;
begin
  Result := FColor
end;

function TlicgTitleItem.GetFont: TlicgFontStyle;
begin
  Result := FFont
end;

function TlicgTitleItem.GetTransparent: Boolean;
begin
  Result := FTransparent
end;

procedure TlicgTitleItem.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value
end;

procedure TlicgTitleItem.SetCaption(const Value: string);
begin
  FCaption := Value
end;

procedure TlicgTitleItem.SetColor(const Value: TColor);
begin
  FColor := Value
end;

procedure TlicgTitleItem.SetFont(const Value: TlicgFontStyle);
begin
  FFont := Value
end;

procedure TlicgTitleItem.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value
end;

{ ----- TlicgColumnItem ----- }

constructor TlicgColumnItem.Create(ColumnList: TlicgColumnList);
begin
  inherited Create;
  FColumnList := ColumnList;
  FStrings := TStringSparseList.Create(SPASmall);
  FTitle := TlicgTitleItem.Create(self);
  FTitle.Alignment := taCenter;
  FColor := clWhite;
  FFont.Name := 'Arial'; // ilker deðiþtirme Lider.CG.Com.System.DefaultFontName;
  FFont.Height := 1;
  Width := 1;
end;

destructor TlicgColumnItem.Destroy;
begin
  FTitle.Free;
  FStrings.Free;
  inherited Destroy;
end;

function TlicgColumnItem.GetAlignment: TAlignment;
begin
  Result := FAlignment
end;

function TlicgColumnItem.GetColor: TColor;
begin
  Result := FColor
end;

function TlicgColumnItem.GetColumnType: TlicgColumnType;
begin
  Result := FColumnType
end;

function TlicgColumnItem.GetFont: TlicgFontStyle;
begin
  Result := FFont
end;

function TlicgColumnItem.GetForeColor: TColor;
begin
  Result := FForeColor;
end;

function TlicgColumnItem.GetStrings: TStrings;
begin
  Result := FStrings
end;

function TlicgColumnItem.GetTitle: TlicgTitleItem;
begin
  Result := FTitle
end;

function TlicgColumnItem.GetTransparent: Boolean;
begin
  Result := FTransparent
end;

function TlicgColumnItem.GetWidth: Double;
begin
  Result := FWidth
end;

procedure TlicgColumnItem.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value
end;

procedure TlicgColumnItem.SetColor(const Value: TColor);
begin
  FColor := Value
end;

procedure TlicgColumnItem.SetColumnType(const Value: TlicgColumnType);
begin
  FColumnType := Value
end;

procedure TlicgColumnItem.SetFont(const Value: TlicgFontStyle);
begin
  FFont := Value
end;

procedure TlicgColumnItem.SetForeColor(const Value: TColor);
begin
  FForeColor := Value;
end;

procedure TlicgColumnItem.SetTitle(const Value: TlicgTitleItem);
begin
  FTitle := Value
end;

procedure TlicgColumnItem.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value
end;

procedure TlicgColumnItem.SetWidth(const Value: Double);
begin
  FWidth := Value
end;

{ ----- TlicgColumnList ----- }

constructor TlicgColumnList.Create(Table: TObject);
begin
  inherited Create;
  FTable := Table;
  FItems := TList.Create;
end;

destructor TlicgColumnList.Destroy;
begin
  Clear;
  FItems.free;
  inherited Destroy;
end;

function TlicgColumnList.Add: TlicgColumnItem;
begin
  Result := TlicgColumnItem.Create(Self);
  FItems.Add(Result);
end;

procedure TlicgColumnList.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TlicgColumnItem(FItems[I]).Free;
  FItems.Clear;
end;

procedure TlicgColumnList.Delete(Index: Integer);
begin
  TlicgColumnItem(FItems[Index]).Free;
  FItems.Delete(Index);
end;

function TlicgColumnList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TlicgColumnList.GetItem(Index: Integer): TlicgColumnItem;
begin
  Result := FItems[Index];
end;

procedure TlicgColumnList.Assign(Source: TlicgColumnList);
var
  I: Integer;
begin
  Clear;
  for I := 0 to Source.Count - 1 do
    with Add do
    begin
      ColumnType := Source[I].ColumnType;
      Strings.Assign(Source[I].Strings);
      Alignment := Source[I].Alignment;
      Color := Source[I].Color;
      ForeColor := Source[I].ForeColor;
      Transparent := Source[I].Transparent;
      FFont := Source[I].Font;
      Width := Source[I].Width;
      with Title do
      begin
        Caption := Source[I].Title.Caption;
        Alignment := Source[I].Title.Alignment;
        Color := Source[I].Title.Color;
        Transparent := Source[I].Title.Transparent;
        FFont := Source[I].Title.Font;
      end;
    end;
end;

procedure TlicgColumnList.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

function TlicgColumnList.Down(Index: Integer): Boolean;
var
  temp: Pointer;
begin
  Result := False;
  if (Index < 0) or (Index >= FItems.Count - 1) then
    Exit;
  temp := FItems[Index + 1];
  FItems[Index + 1] := FItems[Index];
  FItems[Index] := temp;
  Result := True;
end;

function TlicgColumnList.Up(Index: Integer): Boolean;
var
  temp: Pointer;
begin
  Result := False;
  if (Index <= 0) or (Index > FItems.Count - 1) then
    Exit;
  temp := FItems[Index - 1];
  FItems[Index - 1] := FItems[Index];
  FItems[Index] := temp;
  Result := True;
end;

end.


