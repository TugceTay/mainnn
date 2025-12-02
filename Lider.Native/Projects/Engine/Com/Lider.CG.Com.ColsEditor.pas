unit Lider.CG.Com.ColsEditor;

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
  Math,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Grids,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Columns,
  Lider.CG.Com.DrawToolsInt, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, cxContainer, cxEdit, cxCustomListBox,
  cxListBox;

type
  TfmColumnsEditor = class(TForm)
    Panel1: TPanel;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    ListBox1: TListBox;
    Panel2: TPanel;
    Button1: TButton;
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
    procedure Inspector1PropertyChange(Sender: TObject; const PropertyName: string);
  private
    { Private declarations }
    FEditColumns: TlicgColumnList;
    FRowCount: Integer;
    FLast: Integer;
    procedure UpdateProperties;
  public
    { Public declarations }
    function Enter(RowCount: Integer; Columns: TlicgColumnList): Word;
    property EditColumns: TlicgColumnList read FEditColumns;
  end;

implementation

{$R *.dfm}

uses
  Lider.CG.Com.Lib,
  Lider.CG.Com.System;

{ TfmColumnsEditor }

function TfmColumnsEditor.Enter(RowCount: Integer; Columns: TlicgColumnList): Word;
var
  I: Integer;
  ColsStrings: TStrings;
begin
  FEditColumns := TlicgColumnList.Create(nil);
  FEditColumns.Assign(Columns);
  FRowCount := RowCount;

  for I := 0 to FEditColumns.Count - 1 do
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
      Inspector1.Divisorpos := StrToInt(ColsStrings.Values['Col0']);
    end;
  finally
    ColsStrings.free;
  end;

  Result := ShowModal;
end;

procedure TfmColumnsEditor.btnDeleteClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ListBox1.ItemIndex;
  if Index < 0 then
    Exit;
  FEditColumns.Delete(Index);
  ListBox1.Items.Delete(Index);
  if Index <= ListBox1.Items.Count - 1 then
    ListBox1.ItemIndex := Index
  else if ListBox1.Items.Count > 0 then
    ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  ListBox1.OnClick(nil);
end;

procedure TfmColumnsEditor.btnAddClick(Sender: TObject);
begin
  FEditColumns.Add;
  ListBox1.Items.Add('');
  ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  ListBox1.OnClick(nil);
end;

procedure TfmColumnsEditor.BtnUPClick(Sender: TObject);
begin
  if ListBox1.ItemIndex < 0 then
    Exit;
  if FEditColumns.Up(ListBox1.ItemIndex) then
  begin
    ListBox1.ItemIndex := ListBox1.ItemIndex - 1;
    ListBox1.OnClick(nil);
  end;
  ListBox1.Invalidate;
end;

procedure TfmColumnsEditor.BtnDOWNClick(Sender: TObject);
begin
  if ListBox1.ItemIndex < 0 then
    Exit;
  if FEditColumns.Down(ListBox1.ItemIndex) then
  begin
    ListBox1.ItemIndex := ListBox1.ItemIndex + 1;
    ListBox1.OnClick(nil);
  end;
  ListBox1.Invalidate;
end;

procedure TfmColumnsEditor.ListBox1DrawItem(Control: TWinControl; Index:
  Integer; Rect: TRect; State: TOwnerDrawState);
var
  AText: string;
begin
  with ListBox1.Canvas do
  begin
    FillRect(Rect);
    Font.Assign(Self.Font);
    if odSelected in State then
      Font.Color := clHighlightText;
    AText := FEditColumns[Index].Title.Caption;
    if Length(AText) = 0 then
      AText := Format('Kolon %d', [Index]);
    TextOut(Rect.Left, Rect.Top, AText);
  end;
end;

procedure TfmColumnsEditor.ListBox1Click(Sender: TObject);
var
  Column: TlicgColumnItem;
  bp: TlicgBaseProperty;
  J: Integer;
  pn: string;
  tv: TlicgTreeViewProperty;
begin
  if ListBox1.ItemIndex < 0 then
    Exit;
  UpdateProperties;

  Inspector1.ClearPropertyList;
  Inspector1.Visible := true;

  FLast := ListBox1.ItemIndex;
  { fill the property list }
  Column := FEditColumns[Flast];

  tv := TlicgTreeViewProperty.Create('Genel');
  tv.ReadOnly := true;
  Inspector1.AddProperty(tv);
  tv.Modified := true;

  bp := TlicgEnumerationProperty.Create('Kolon Türü');
  with TlicgEnumerationProperty(bp).Strings do
  begin
    Add('ctText');
    Add('ctColor');
    Add('ctLineStyle');
    Add('ctBrushStyle');
    Add('ctSymbolStyle');
    Add('ctBitmap');
    Add('ctVectBrush');
  end;
  bp.ValInteger := Ord(Column.ColumnType);
  tv.AddProperty(bp);

  bp := TlicgFloatProperty.Create('Geniþlik');
  bp.ValFloat := Column.Width;
  tv.AddProperty(bp);

  bp := TlicgEnumerationProperty.Create('Hizalama');
  with TlicgEnumerationProperty(bp).Strings do
  begin
    Add('taLeftJustify');
    Add('taRightJustify');
    Add('taCenter');
  end;
  bp.ValInteger := Ord(Column.Alignment);
  tv.AddProperty(bp);

  bp := TlicgColorProperty.Create('Zemin Rengi');
  bp.ValInteger := Column.Color;
  tv.AddProperty(bp);

  bp := TlicgColorProperty.Create('Nesne/Yazý Rengi');
  bp.ValInteger := Column.ForeColor;
  tv.AddProperty(bp);

  bp := TlicgBooleanProperty.Create('Transparan');
  bp.ValBoolean := Column.Transparent;
  tv.AddProperty(bp);

  tv := TlicgTreeViewProperty.Create('Font');
  tv.ReadOnly := true;
  Inspector1.AddProperty(tv);
  tv.Modified := true;

  bp := TlicgFontNameProperty.Create('Font Adý');
  TlicgFontNameProperty(bp).TrueType := true;
  bp.ValString := Column.Font.Name;
  tv.AddProperty(bp);

  bp := TlicgFloatProperty.Create('Font Ölçüsü');
  bp.ValFloat := Column.Font.Height;
  tv.AddProperty(bp);

  bp := TlicgColorProperty.Create('Font Rengi');
  bp.ValInteger := Column.Font.Color;
  tv.AddProperty(bp);

  bp := TlicgFontStylesProperty.Create('Font Stili');
  TlicgFontStylesProperty(bp).FontStyles := Column.Font.Style;
  tv.AddProperty(bp);

  tv := TlicgTreeViewProperty.Create('Baþlýk');
  tv.ReadOnly := true;
  Inspector1.AddProperty(tv);
  tv.Modified := true;

  bp := TlicgStringProperty.Create('Baþlýk Açýklama');
  bp.ValString := Column.Title.Caption;
  tv.AddProperty(bp);

  bp := TlicgEnumerationProperty.Create('Baþlýk Hiza');
  with TlicgEnumerationProperty(bp).Strings do
  begin
    Add('taLeftJustify');
    Add('taRightJustify');
    Add('taCenter');
  end;
  bp.ValInteger := Ord(Column.Title.Alignment);
  tv.AddProperty(bp);

  bp := TlicgColorProperty.Create('Baþlýk Rengi');
  bp.ValInteger := Column.Title.Color;
  tv.AddProperty(bp);

  bp := TlicgBooleanProperty.Create('Baþlýk Transparan');
  bp.ValBoolean := Column.Title.Transparent;
  tv.AddProperty(bp);

  bp := TlicgFontNameProperty.Create('Baþlýk Font Adý');
  TlicgFontNameProperty(bp).TrueType := true;
  bp.ValString := Column.Title.Font.Name;
  tv.AddProperty(bp);

  bp := TlicgColorProperty.Create('Baþlýk Font Rengi');
  bp.ValInteger := Column.Title.Font.Color;
  tv.AddProperty(bp);

  bp := TlicgFloatProperty.Create('Baþlýk Font Ölçüsü');
  bp.ValFloat := Column.Title.Font.Height;
  tv.AddProperty(bp);

  bp := TlicgFontStylesProperty.Create('Baþlýk Font Stili');
  TlicgFontStylesProperty(bp).FontStyles := Column.Title.Font.Style;
  tv.AddProperty(bp);

  tv := TlicgTreeViewProperty.Create('Satýrlar');
  tv.ReadOnly := true;
  Inspector1.AddProperty(tv);

  tv.Modified := true;

  for J := 0 to Self.FRowCount - 1 do
  begin
    pn := Format('Satýr(%d)', [J + 1]);
    case Column.ColumnType of
      ctLabel:
        begin
          bp := TlicgStringProperty.Create(pn);
          bp.ValString := Column.Strings[J];
          tv.AddProperty(bp);
        end;
      ctColor:
        begin
          bp := TlicgColorProperty.Create(pn);
          bp.ValInteger := StrToIntDef(Column.Strings[J], 0);
          tv.AddProperty(bp);
        end;
      ctLineStyle:
        begin
          bp := TlicgLinetypeProperty.Create(pn);
          bp.ValInteger := StrToIntDef(Column.Strings[J], 0);
          tv.AddProperty(bp);
        end;
      ctBrushStyle:
        begin
          bp := TlicgBrushstyleProperty.Create(pn);
          bp.ValInteger := StrToIntDef(Column.Strings[J], 0);
          tv.AddProperty(bp);
        end;
      ctSymbolStyle:
        begin
          //bp := TlicgSymbolProperty.Create(pn);
          bp.ValInteger := StrToIntDef(Column.Strings[J], 0);
          tv.AddProperty(bp);
        end;
      ctBitmap:
        begin
          bp := TlicgDefineAnyLocalImageProperty.Create(pn);
          bp.ValString := Column.Strings[J];
          tv.AddProperty(bp);
        end;
      ctVectBrush:
        begin
          bp := TlicgStringProperty.Create(pn);
          bp.ValString := Column.Strings[J];
          tv.AddProperty(bp);
        end;
    end;
  end;
end;

procedure TfmColumnsEditor.UpdateProperties;
var
  Column: TlicgColumnItem;
  bp: TlicgBaseProperty;
  FontStyle: TlicgFontStyle;
  temp: boolean;
  J: Integer;
  pn: string;
begin
  if (FLast < 0) or (FLast > ListBox1.Items.Count - 1) then
    Exit;

  { update the properties here }

  Column := FEditColumns[FLast];

  bp := Inspector1.GetPropertyByName('Kolon Türü');
  if bp.modified then
    Column.ColumnType := TlicgColumnType(Max(0, bp.ValInteger));

  bp := Inspector1.GetPropertyByName('Hizalama');
  if bp.modified then
    Column.Alignment := TAlignment(Max(0, bp.ValInteger));

  bp := Inspector1.GetPropertyByName('Zemin Rengi');
  if bp.modified then
    Column.Color := bp.ValInteger;

  bp := Inspector1.GetPropertyByName('Nesne/Yazý Rengi');
  if bp.modified then
    Column.ForeColor := bp.ValInteger;

  bp := Inspector1.GetPropertyByName('Transparan');
  if bp.modified then
    Column.Transparent := bp.ValBoolean;

  FontStyle := Column.Font;
  temp := false;

  bp := Inspector1.GetPropertyByName('Font Adý');
  if bp.modified then
  begin
    FontStyle.Name := bp.ValString;
    temp := true;
  end;

  bp := Inspector1.GetPropertyByName('Font Rengi');
  if bp.modified then
  begin
    FontStyle.Color := bp.ValInteger;
    temp := true;
  end;

  bp := Inspector1.GetPropertyByName('Font Ölçüsü');
  if bp.modified then
  begin
    FontStyle.Height := bp.ValFloat;
    temp := true;
  end;

  bp := Inspector1.GetPropertyByname('Font Stili');
  if bp.modified then
  begin
    FontStyle.Style := TlicgFontStylesProperty(bp).FontStyles;
    temp := true;
  end;

  if temp then
    Column.Font := FontStyle;

  bp := Inspector1.GetPropertyByName('Geniþlik');
  if bp.modified then
    Column.Width := bp.ValFloat;

  bp := Inspector1.GetPropertyByName('Baþlýk Açýklama');
  if bp.modified then
    Column.Title.Caption := bp.ValString;

  bp := Inspector1.GetPropertyByName('Baþlýk Hiza');
  if bp.modified then
    Column.Title.Alignment := TAlignment(Max(0, bp.ValInteger));

  bp := Inspector1.GetPropertyByName('Baþlýk Rengi');
  if bp.modified then
    Column.Title.Color := bp.ValInteger;

  bp := Inspector1.GetPropertyByName('Baþlýk Transparan');
  if bp.modified then
    Column.Title.Transparent := bp.ValBoolean;

  FontStyle := Column.Title.Font;
  temp := false;

  bp := Inspector1.GetPropertyByName('Baþlýk Font Adý');
  if bp.modified then
  begin
    FontStyle.Name := bp.ValString;
    temp := true;
  end;

  bp := Inspector1.GetPropertyByName('Baþlýk Font Rengi');
  if bp.modified then
  begin
    FontStyle.Color := bp.ValInteger;
    temp := true;
  end;

  bp := Inspector1.GetPropertyByName('Baþlýk Font Ölçüsü');
  if bp.modified then
  begin
    FontStyle.Height := bp.ValFloat;
    temp := true;
  end;

  bp := Inspector1.GetPropertyByname('Baþlýk Font Stili');
  if bp.modified then
  begin
    FontStyle.Style := TlicgFontStylesProperty(bp).FontStyles;
    temp := true;
  end;

  if temp then
    Column.Title.Font := FontStyle;

  for J := 0 to Self.FRowCount - 1 do
  begin
    pn := Format('Satýr(%d)', [J + 1]);
    case Column.ColumnType of
      ctLabel:
        begin
          bp := Inspector1.GetPropertyByName(pn);
          Column.Strings[J] := bp.ValString;
        end;
      ctColor:
        begin
          bp := Inspector1.GetPropertyByName(pn);
          Column.Strings[J] := IntToStr(bp.ValInteger);
        end;
      ctLineStyle:
        begin
          bp := Inspector1.GetPropertyByName(pn);
          Column.Strings[J] := Inttostr(bp.ValInteger);
        end;
      ctBrushStyle:
        begin
          bp := Inspector1.GetPropertyByName(pn);
          Column.Strings[J] := Inttostr(bp.ValInteger);
        end;
      ctSymbolStyle:
        begin
          bp := Inspector1.GetPropertyByName(pn);
          Column.Strings[J] := Inttostr(bp.ValInteger);
        end;
      ctBitmap:
        begin
          bp := Inspector1.GetPropertyByName(pn);
          Column.Strings[J] := bp.ValString;
        end;
      ctVectBrush:
        begin
          bp := Inspector1.GetPropertyByName(pn);
          Column.Strings[J] := bp.ValString;
        end;
    end;
  end;
end;

procedure TfmColumnsEditor.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ColsString: TStrings;
begin
  ColsString := TStringList.Create;
  try
    ColsString.Add(Format('Kolon=%d', [Inspector1.Divisorpos]));
    // ilker silme SaveFormPlacement(ExtractFilePath(Application.ExeName) + 'formspos.ini', Self, ColsString);
  finally
    ColsString.Free;
  end;
  UpdateProperties;
end;

procedure TfmColumnsEditor.Inspector1PropertyChange(Sender: TObject; const
  PropertyName: string);
var
  pn: string;
  J: Integer;
  Column: TlicgColumnItem;
  bp, bp1: TlicgBaseProperty;
begin
  if AnsiCompareText(PropertyName, 'Baþlýk Açýklama') = 0 then
    ListBox1.Invalidate
  else if AnsiCompareText(PropertyName, 'Kolon Türü') = 0 then
  begin
    Column := FEditColumns[FLast];
    bp1 := Inspector1.GetPropertyByName(PropertyName);
    for J := 0 to Self.FRowCount - 1 do
    begin
      pn := Format('Satýr(%d)', [J + 1]);
      case TlicgColumnType(bp1.ValInteger) of
        ctLabel:
          begin
            bp := TlicgStringProperty.Create(pn);
            bp.ValString := Column.Strings[J];
            Inspector1.ReplaceProperty(pn, bp);
          end;
        ctColor:
          begin
            bp := TlicgColorProperty.Create(pn);
            bp.ValInteger := Min(Low(TColor), Max(StrToIntDef(Column.Strings[J], 0), High(TColor)));
            Inspector1.ReplaceProperty(pn, bp);
          end;
        ctLineStyle:
          begin
            bp := TlicgLinetypeProperty.Create(pn);
            bp.ValInteger := Min(CurrCmdLine.ActiveDrawBox.GIS.LineTypes.Count - 1, Max(StrToIntDef(Column.Strings[J], 0), 0));
            Inspector1.ReplaceProperty(pn, bp);
          end;
        ctBrushStyle:
          begin
            bp := TlicgBrushstyleProperty.Create(pn);
            bp.ValInteger := Min(89, Max(StrToIntDef(Column.Strings[J], 0), 0));
            Inspector1.ReplaceProperty(pn, bp);
          end;
        ctSymbolStyle:
          begin
            //bp := TlicgSymbolProperty.Create(pn);
            bp.ValInteger := Min(CurrCmdLine.ActiveDrawBox.GIS.SymbolsList.Items[0].Count - 1, Max(StrToIntDef(Column.Strings[J], 0), 0));
            Inspector1.ReplaceProperty(pn, bp);
          end;
        ctBitmap:
          begin
            bp := TlicgDefineAnyLocalImageProperty.Create(pn);
            bp.ValString := Column.Strings[J];
            Inspector1.ReplaceProperty(pn, bp);
          end;
        ctVectBrush:
          begin
            bp := TlicgStringProperty.Create(pn);
            bp.ValString := Column.Strings[J];
            Inspector1.ReplaceProperty(pn, bp);
          end;
      end;
    end;
  end;
end;

end.


