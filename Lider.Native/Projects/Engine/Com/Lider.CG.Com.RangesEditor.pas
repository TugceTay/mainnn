unit Lider.CG.Com.RangesEditor;

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
  StdCtrls,
  Buttons,
  ExtCtrls,
  Lider.CG.Com.Thematics,
  Lider.CG.Com.GIS, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, cxContainer, cxEdit, cxCustomListBox,
  cxListBox;

type
  TfmRangesEditor = class(TForm)
    Panel1: TPanel;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    ListBox1: TListBox;
    Panel2: TPanel;
    Button2: TButton;
    BtnUP: TSpeedButton;
    BtnDOWN: TSpeedButton;
    Splitter1: TSplitter;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure BtnUPClick(Sender: TObject);
    procedure BtnDOWNClick(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure ListBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FBuilder: TlicgThematicBuilder;
    FLast: Integer;
    FGis: TlicgBaseGis;
    fTrs: TlicgThematicRangeSupport;
    procedure UpdateProperties;
  public
    { Public declarations }
    function Enter(trs: TlicgThematicRangeSupport; Builder: TlicgThematicBuilder;
      aGis: TlicgBaseGis): Word;
  end;

implementation

{$R *.DFM}

uses
  Lider.CG.Com.Lib,
  Lider.CG.Com.System;

{ TfmColumnsEditor }

function TfmRangesEditor.Enter(trs: TlicgThematicRangeSupport; Builder:
  TlicgThematicBuilder; aGis: TlicgBaseGis): Word;
var
  I: Integer;
  ColsStrings: TStrings;
begin
  FGis := aGis;
  FTrs := trs;

  FBuilder := Builder;
  for I := 0 to FBuilder.ThematicRanges.Count - 1 do
    ListBox1.Items.Add('');

  FLast := -1;
  if ListBox1.Items.Count > 0 then
  begin
    ListBox1.ItemIndex := 0;
    ListBox1.OnClick(nil);
  end;

  ColsStrings := TStringList.Create;
  try
    // ilker silme RestoreFormPlacement(ExtractFilePath(Application.ExeName) + 'formspos.ini', Self, true, ColsStrings);
    if ColsStrings.Count = 2 then
    begin
      //Inspector1.DivisorPos := StrToInt(ColsStrings.Values['Col0']);
    end;
  finally
    ColsStrings.free;
  end;

  Result := ShowModal;
end;

procedure TfmRangesEditor.btnDeleteClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBox1.ItemIndex;
  if Index < 0 then
    Exit;
  FBuilder.ThematicRanges.Delete(Index);
  ListBox1.Items.Delete(Index);
  if Index <= ListBox1.Items.Count - 1 then
    ListBox1.ItemIndex := Index
  else if ListBox1.Items.Count > 0 then
    ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  ListBox1.OnClick(nil);
end;

procedure TfmRangesEditor.btnAddClick(Sender: TObject);
begin
  FBuilder.ThematicRanges.Add;
  ListBox1.Items.Add('');
  ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  ListBox1.OnClick(nil);
end;

procedure TfmRangesEditor.BtnUPClick(Sender: TObject);
begin
  if ListBox1.ItemIndex < 0 then
    Exit;
  if FBuilder.ThematicRanges.Up(ListBox1.ItemIndex) then
  begin
    ListBox1.ItemIndex := ListBox1.ItemIndex - 1;
    ListBox1.OnClick(nil);
  end;
  ListBox1.Invalidate;
end;

procedure TfmRangesEditor.BtnDOWNClick(Sender: TObject);
begin
  if ListBox1.ItemIndex < 0 then
    Exit;
  if FBuilder.ThematicRanges.Down(ListBox1.ItemIndex) then
  begin
    ListBox1.ItemIndex := ListBox1.ItemIndex + 1;
    ListBox1.OnClick(nil);
  end;
  ListBox1.Invalidate;
end;

procedure TfmRangesEditor.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  AText: string;
begin
  with ListBox1.Canvas do
  begin
    FillRect(Rect);
    Font.Assign(Self.Font);
    if odSelected in State then
      Font.Color := clHighlightText;
    AText := Format('Aralýk %d', [Index]);
    TextOut(Rect.Left, Rect.Top, AText);
  end;
end;

procedure TfmRangesEditor.ListBox1Click(Sender: TObject);
(*var
  Range: TlicgThematicItem;
  bp: TlicgBaseProperty;
  tv: TlicgTreeViewProperty;*)
begin  (*
  if ListBox1.ItemIndex < 0 then
    Exit;
  UpdateProperties;

  Inspector1.ClearPropertyList;
  Inspector1.Visible := true;

  FLast := ListBox1.ItemIndex;
  { fill the property list }
  Range := Self.FBuilder.ThematicRanges[Flast];

  tv := TlicgTreeViewProperty.Create('Genel');
  tv.readonly := true;
  Inspector1.AddProperty(tv);
  tv.Modified := true; { causes to show on bold}

  bp := TlicgStringProperty.Create('Lejant');
  bp.ValString := Range.Legend;
  bp.Hint := ''; //Define the legend for this range';
  tv.AddProperty(bp);

  bp := TlicgExpressionProperty.Create('Ýfade');
  TlicgExpressionProperty(bp).Gis := FGis;
  TlicgExpressionProperty(bp).LayerName := Self.FBuilder.LayerName;
  bp.ValString := Range.Expression;
  bp.Hint := ''; //'Define the expression for this range';
  tv.AddProperty(bp);



  { pen style }

  if (FTrs = trsAll) or (FTrs = trsOpened) then
  begin
    tv := TlicgTreeViewProperty.Create('Hat');
    tv.readonly := true;
    Inspector1.AddProperty(tv);
    tv.Modified := true; { causes to show on bold}


    bp := TlicgLinetypeProperty.Create('Hat Tipi');
    bp.ValInteger := Range.PenStyle.Style;
    bp.Hint := ''; //'Select the pen style';
    bp.ItemHeight := Inspector1.ItemHeight * 2;
    tv.AddProperty(bp);

    bp := TlicgColorProperty.Create('Hat Rengi');
    bp.ValInteger := Range.PenStyle.Color;
    bp.Hint := 'Select the pen color';
    tv.AddProperty(bp);

    bp := TlicgFloatProperty.Create('Hat Kalýnlýk');
    bp.ValFloat := Range.PenStyle.Width;
    bp.Hint := ''; // 'Select the pen width or scale';
    tv.AddProperty(bp);
  end;

  { brush style }
  if (FTrs = trsAll) or (FTrs = trsClosed) then
  begin

    tv := TlicgTreeViewProperty.Create('Dolgu');
    tv.readonly := true;
    Inspector1.AddProperty(tv);
    tv.Modified := true; { causes to show on bold}

    bp := TlicgBrushstyleProperty.Create('Dolgu Tipi');
    bp.ValInteger := Range.BrushStyle.Pattern;
    bp.Hint := ''; // 'Select the brush pattern';
    bp.ItemHeight := Inspector1.ItemHeight * 2;
    tv.AddProperty(bp);

    bp := TlicgColorProperty.Create('Önalan Rengi');
    bp.ValInteger := Range.BrushStyle.ForeColor;
    bp.Hint := 'Select the brush foreground color';
    tv.AddProperty(bp);

    bp := TlicgColorProperty.Create('Arkaalan Rengi');
    TlicgColorProperty(bp).NoneColorText := '&Saydam';
    TlicgColorProperty(bp).ShowSystemColors := true;
    bp.ValInteger := Range.BrushStyle.BackColor;
    bp.Hint := ''; //'Select the brush background color';
    tv.AddProperty(bp);
  end;

   { symbol style }
  if (FTrs = trsAll) or (FTrs = trsSymbol) then
  begin

    tv := TlicgTreeViewProperty.Create('Sembol');
    tv.readonly := true;
    Inspector1.AddProperty(tv);
    tv.Modified := true; { causes to show on bold}

    bp := GetAngleProperty('Sembol Açý');
    bp.ValFloat := Range.SymbolStyle.Rotangle;
    bp.Hint := ''; // 'Define rotation angle';
    tv.AddProperty(bp);

    bp := TlicgFloatProperty.Create('Sembol Yükseklik');
    bp.ValFloat := Range.SymbolStyle.Height;
    bp.Hint := ''; // 'Define height of symbol';
    tv.AddProperty(bp);
  end;

  Inspector1.FullExpand;
   *)
end;

procedure TfmRangesEditor.UpdateProperties;
var
  Range: TlicgThematicItem;
  //bp: TlicgBaseProperty;
begin (*
  if (FLast < 0) or (FLast > ListBox1.Items.Count - 1) then
    Exit;

  { update the properties here }

  Range := FBuilder.ThematicRanges[FLast];

  bp := Inspector1.GetPropertyByName('Lejant');
  if bp.modified then
    Range.Legend := bp.ValString;

  bp := Inspector1.GetPropertyByName('Ýfade');
  if bp.modified then
    Range.Expression := bp.ValString;

  bp := Inspector1.GetPropertyByName('Hat Tipi');
  if Assigned(bp) and bp.modified then
    Range.Penstyle.Style := bp.ValInteger;

  bp := Inspector1.GetPropertyByName('Hat Rengi');
  if Assigned(bp) and bp.modified then
    Range.Penstyle.Color := bp.ValInteger;

  bp := Inspector1.GetPropertyByName('Hat Kalýnlýk');
  if Assigned(bp) and bp.modified then
    Range.Penstyle.Width := bp.ValFloat;

  bp := Inspector1.GetPropertyByName('Dolgu Tipi');
  if Assigned(bp) and bp.modified then
    Range.BrushStyle.Pattern := bp.ValInteger;

  bp := Inspector1.GetPropertyByName('Önalan Rengi');
  if Assigned(bp) and bp.modified then
    Range.BrushStyle.ForeColor := bp.ValInteger;

  bp := Inspector1.GetPropertyByName('Arkaalan Rengi');
  if Assigned(bp) and bp.modified then
    Range.BrushStyle.BackColor := bp.ValInteger;

  { symbol style }

  bp := Inspector1.GetPropertyByName('Sembol Tipi');
  if Assigned(bp) and bp.modified then
    Range.SymbolStyle.Index := bp.ValInteger;

  bp := Inspector1.GetPropertyByName('Sembol Açý');
  if Assigned(bp) and bp.modified then
    Range.SymbolStyle.Rotangle := bp.ValFloat;

  bp := Inspector1.GetPropertyByName('Sembol Yükseklik');
  if Assigned(bp) and bp.modified then
    Range.SymbolStyle.Height := bp.ValFloat;
    *)
end;

procedure TfmRangesEditor.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ColsString: TStrings;
begin
  ColsString := TStringList.Create;
  try
    ///ColsString.Add(Format('Col0=%d', [Inspector1.DivisorPos]));
    // ilker silme SaveFormPlacement(ExtractFilePath(Application.ExeName) + 'formspos.ini', Self, ColsString);
  finally
    ColsString.Free;
  end;
  //UpdateProperties;
end;

end.


