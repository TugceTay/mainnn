unit Lider.CG.Com.ThemsEditor;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Dialogs,
  Buttons,
  ExtCtrls,
  Grids,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxCustomData,
  cxStyles,
  cxTL,
  cxButtonEdit,
  cxCheckBox,
  cxTLdxBarBuiltInMenu,
  dxSkinsCore,
  dxSkinsDefaultPainters,
  cxInplaceContainer,
  cxSplitter,
  cxContainer,
  cxEdit,
  cxTextEdit,
  cxLabel,
  cxGroupBox,
  Menus,
  cxButtons,
  Lider.CG.Com.Legend,
  Lider.CG.Com.Lib,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Thematics,
  Lider.CG.Com.Utilities,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TfmThematicsEditor = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    BtnRecalc: TSpeedButton;
    BtnSave: TSpeedButton;
    BtnOpen: TSpeedButton;
    BtnEasy: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    cxGroupBox1: TcxGroupBox;
    cxCheckBox_tematikGoster: TcxCheckBox;
    cxLabel1: TcxLabel;
    cxTextEdit_baslik: TcxTextEdit;
    cxCheckBox_hat: TcxCheckBox;
    cxCheckBox_dolgu: TcxCheckBox;
    cxCheckBox_symbol: TcxCheckBox;
    cxCheckBox_yazi: TcxCheckBox;
    cxButton1: TcxButton;
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnEasyClick(Sender: TObject);
    procedure BtnRecalcClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure cxCheckBox_tematikGosterPropertiesEditValueChanged(Sender: TObject);
    procedure cxTextEdit_baslikPropertiesEditValueChanged(Sender: TObject);
    procedure cxCheckBox_hatPropertiesEditValueChanged(Sender: TObject);
    procedure cxCheckBox_dolguPropertiesEditValueChanged(Sender: TObject);
    procedure cxCheckBox_symbolPropertiesEditValueChanged(Sender: TObject);
    procedure cxCheckBox_yaziPropertiesEditValueChanged(Sender: TObject);
    procedure cxButton1Click(Sender: TObject);
  private
    { Private declarations }
    FLayer: TlicgBaseLayer;
    FGis: TlicgBasegis;
    FfmLegend: TfmLegend;
    FThematicBuilder: TlicgThematicBuilder;
    FAfterLayerCreate: boolean;
    FFileName: string;
    function GetBuilder(Layer: TlicgBaselayer): TlicgThematicBuilder;
    procedure ShowLegend;
    procedure MyRangeChange(Sender: TObject);
  public
    { Public declarations }
    function Enter(aGis: TlicgBaseGis; aLayer: TlicgBaseLayer; isShowModal:
      boolean; AfterLayerCreate: boolean = false): WOrd;
  end;

implementation

{$R *.DFM}

uses
  Lider.CG.Com.Columns,
  Lider.CG.Com.Consts,
  Lider.CG.Com.System,
  Lider.CG.Com.EasyThematic,
  Lider.CG.Com.RangesEditor;
(*
type

  { for editing the columns in a table }
  TlicgThematicRangesProperty = class(TlicgBaseProperty)
  private
    FBuilder: TlicgThematicBuilder;
    FGis: TlicgBasegis;
    fTrs: TlicgThematicRangeSupport;
  public
    constructor Create(const PropName: string); override;
    procedure Edit(Inspector: TlicgInspector); override;
    function AsString: string; override;
    property Builder: TlicgThematicBuilder read FBuilder write FBuilder;
    property Gis: TlicgBaseGis read FGis write FGis;
  end;

  { TEzThematicRangesProperty }

constructor TlicgThematicRangesProperty.Create(const PropName: string);
begin
  inherited create(PropName);
  fTrs := trsAll;
  PropType := ptString;
  UseEditButton := true;
end;

function TlicgThematicRangesProperty.AsString: string;
begin
  Result := '(Aralýklar)';
end;

procedure TlicgThematicRangesProperty.Edit(Inspector: TlicgInspector);
begin
  if ReadOnly then
    exit;
  with TfmRangesEditor.create(Application) do
  try
    //if Enter(Self.Builder, Self.FGis) = mrOk then
    Enter(fTrs, Self.Builder, Self.FGis);
    begin
      Modified := true;
      if Assigned(OnChange) then
        OnChange(Self);
    end;
  finally
    free;
  end;
end;          *)

{ TfmThematicsEditor }

procedure TfmThematicsEditor.ShowLegend;
begin

  if Assigned(FfmLegend) then
    FreeAndNil(FfmLegend);

  if FThematicBuilder = nil then
    exit;

  if FThematicBuilder.ThematicRanges.Count < 1 then
    exit;

  FfmLegend := TfmLegend.Create(nil);

  //EmbeddedFormPanel1.FormLink := FfmLegend.EmbeddedFormLink1;

  FfmLegend.Legend1.Visible := True;
  FfmLegend.Legend1.PopulateFrom(FThematicBuilder);
  FfmLegend.Legend1.Invalidate;
  {
  if FThematicBuilder.ApplyPen then
    FfmLegend.Legend1.LegendStyle := ctLineStyle
  else if FThematicBuilder.ApplyBrush then
    FfmLegend.Legend1.LegendStyle := ctBrushStyle
  else if FThematicBuilder.ApplySymbol then
    FfmLegend.Legend1.LegendStyle := ctSymbolStyle;
  }

  FfmLegend.Show;

end;

function TfmThematicsEditor.GetBuilder(Layer: TlicgBaselayer): TlicgThematicBuilder;
var
  IsNew: Boolean;
  I, Index: Integer;
begin
  Result := nil;

  if Layer = nil then
    exit;

  IsNew := false;
  { search if this already exists for this layer }
  Index := -1;

  for I := 0 to Layer.GIS.ThematicList.Count - 1 do
    if AnsiCompareText(ExtractFileName(TlicgThematicBuilder(Layer.GIS.ThematicList
      [I]).LayerName), ExtractFileName(Layer.FileName)) = 0 then
    begin
      Index := I;
      break;
    end;

  if Index = -1 then
  begin
    Result := TlicgThematicBuilder.Create(nil);
    Result.LayerName := Layer.Name;
    IsNew := true;
  end
  else
    Result := TlicgThematicBuilder(Layer.GIS.ThematicList[Index]);

  FThematicBuilder := Result;
  if FGis <> nil then
  begin

    if not FAfterLayerCreate then
    begin

      cxCheckBox_tematikGoster.Checked := Result.ShowThematic;

      cxTextEdit_baslik.Text := Result.Title;

      cxCheckBox_hat.Visible := false;
      cxCheckBox_dolgu.Visible := false;
      cxCheckBox_symbol.Visible := false;
      cxCheckBox_yazi.Visible := false;

      if (Layer.SupportEntitiyIDs = [idPolyline]) then
      begin
        Result.ApplyPen := True;
        Result.ApplyBrush := False;
        Result.ApplySymbol := False;
        Result.ApplyFont := False;
      end
      else if Layer.SupportEntitiyIDs = [idPolygon] then
      begin
        Result.ApplyPen := false;
        Result.ApplyBrush := True;
        Result.ApplySymbol := False;
        Result.ApplyFont := False;

      end
      else if Layer.SupportEntitiyIDs = [idPlace, idBlockInsert] then
      begin

        Result.ApplyPen := false;
        Result.ApplyBrush := false;
        Result.ApplySymbol := true;
        Result.ApplyFont := False;

      end
      else
      begin
        cxCheckBox_hat.Visible := true;
        cxCheckBox_dolgu.Visible := true;
        cxCheckBox_symbol.Visible := true;
        cxCheckBox_yazi.Visible := true;

        cxCheckBox_hat.Checked := Result.ApplyPen;
        cxCheckBox_dolgu.Checked := Result.ApplyBrush;
        cxCheckBox_symbol.Checked := Result.ApplySymbol;
        cxCheckBox_yazi.Checked := Result.ApplyFont;
      end;

    end;

  end;

  ShowLegend;

end;

function TfmThematicsEditor.Enter(aGis: TlicgBaseGis; aLayer: TlicgBaseLayer;
  isShowModal: boolean; AfterLayerCreate: boolean = false): WOrd;
begin
  FLayer := aLayer;
  FGis := aGis;
  FAfterLayerCreate := isShowModal and AfterLayerCreate;

  FThematicBuilder := GetBuilder(FLayer);

  if FAfterLayerCreate then
    Panel2.Visible := False;

  if isShowModal then
    Result := ShowModal
  else
    Show;
end;

procedure TfmThematicsEditor.BtnOpenClick(Sender: TObject);
var
  I, Index: integer;
begin
  if not OpenDialog1.Execute then
    exit;

  Index := -1;
  FFileName := OpenDialog1.FileName;
  FThematicBuilder.LoadFromFile(FFileName, ExtractFileDir(FGis.FileName));

  ShowLegend;

  for I := 0 to FGis.ThematicList.Count - 1 do
    if AnsiCompareText(ExtractFileName(TlicgThematicBuilder(FGis.ThematicList[I]).LayerName),
      ExtractFileName(Self.FThematicBuilder.LayerName)) = 0 then
    begin
      Index := I;
      break;
    end;

  if Index = -1 then
  begin
    FGis.ThematicList.Add(Self.FThematicBuilder);
  end;

end;

procedure TfmThematicsEditor.BtnSaveClick(Sender: TObject);
begin
  SaveDialog1.FileName := FFileName;
  if not SaveDialog1.Execute then
    exit;
  FFileName := SaveDialog1.FileName;
  FThematicBuilder.SaveToFile(FFileName);
end;

procedure TfmThematicsEditor.BtnEasyClick(Sender: TObject);
var
  Index, i: integer;
begin
  Index := -1;
  if (FGis = nil) or (Length(FThematicBuilder.LayerName) = 0) then
    Exit;

  with TfmEasyThematic.Create(nil) do
  try
    if Enter(Self.FGis, Self.FThematicBuilder) = mrOK then
    begin

      ShowLegend;

      for I := 0 to FGis.ThematicList.Count - 1 do
        if AnsiCompareText(ExtractFileName(TlicgThematicBuilder(FGis.ThematicList[I]).LayerName),
          ExtractFileName(Self.FThematicBuilder.LayerName)) = 0 then
        begin
          Index := I;
          break;
        end;

      if Index = -1 then
      begin
        FGis.ThematicList.Add(Self.FThematicBuilder);
      end;

    end;

  finally
    free;
  end;
end;

procedure TfmThematicsEditor.BtnRecalcClick(Sender: TObject);
begin
  FThematicBuilder.Recalculate(FGIS);
end;

procedure TfmThematicsEditor.FormDestroy(Sender: TObject);
var
  Index, i: integer;
begin
  Index := -1;

  if Assigned(FThematicBuilder) then
  begin

    for I := 0 to FGis.ThematicList.Count - 1 do
      if AnsiCompareText(ExtractFileName(TlicgThematicBuilder(FGis.ThematicList[I]).LayerName),
        ExtractFileName(Self.FThematicBuilder.LayerName)) = 0 then
      begin
        Index := I;
        break;
      end;

    if Index = -1 then
    begin
      FGis.ThematicList.Add(Self.FThematicBuilder);
    end;

  end;

  if Assigned(FfmLegend) then
    FreeAndNil(FfmLegend);
end;

procedure TfmThematicsEditor.SpeedButton1Click(Sender: TObject);
begin
  if Assigned(FThematicBuilder) then
  begin
    FThematicBuilder.ThematicRanges.Clear;
    FThematicBuilder.Recalculate(FGIS);
    ShowLegend;
  end;
end;

procedure TfmThematicsEditor.MyRangeChange(Sender: TObject);
begin
  ShowLegend;
end;

procedure TfmThematicsEditor.cxCheckBox_tematikGosterPropertiesEditValueChanged
  (Sender: TObject);
begin
  if (not FAfterLayerCreate) and (FThematicBuilder <> nil) then
    FThematicBuilder.ShowThematic := TcxCheckBox(Sender).Checked;
  ShowLegend;
end;

procedure TfmThematicsEditor.cxTextEdit_baslikPropertiesEditValueChanged(Sender:
  TObject);
begin
  if (not FAfterLayerCreate) and (FThematicBuilder <> nil) then
    FThematicBuilder.Title := TcxTextEdit(Sender).Text;
  ShowLegend;

end;

procedure TfmThematicsEditor.cxCheckBox_hatPropertiesEditValueChanged(Sender: TObject);
begin
  if (not FAfterLayerCreate) and (FThematicBuilder <> nil) then
    FThematicBuilder.ApplyPen := TcxCheckBox(Sender).Checked;
  ShowLegend;
end;

procedure TfmThematicsEditor.cxCheckBox_dolguPropertiesEditValueChanged(Sender: TObject);
begin
  if (not FAfterLayerCreate) and (FThematicBuilder <> nil) then
    FThematicBuilder.ApplyBrush := TcxCheckBox(Sender).Checked;
  ShowLegend;

end;

procedure TfmThematicsEditor.cxCheckBox_symbolPropertiesEditValueChanged(Sender:
  TObject);
begin
  if (not FAfterLayerCreate) and (FThematicBuilder <> nil) then
    FThematicBuilder.ApplySymbol := TcxCheckBox(Sender).Checked;
  ShowLegend;

end;

procedure TfmThematicsEditor.cxCheckBox_yaziPropertiesEditValueChanged(Sender: TObject);
begin
  if (not FAfterLayerCreate) and (FThematicBuilder <> nil) then
    FThematicBuilder.ApplyFont := TcxCheckBox(Sender).Checked;
  ShowLegend;

end;

procedure TfmThematicsEditor.cxButton1Click(Sender: TObject);
var
  fTrs: TlicgThematicRangeSupport;
begin
  with TfmRangesEditor.create(Application) do
  try
    //if Enter(Self.Builder, Self.FGis) = mrOk then
    if (fLayer.SupportEntitiyIDs = [idPolyline]) then
      fTrs := trsOpened
    else if fLayer.SupportEntitiyIDs = [idPolygon] then
      fTrs := trsClosed
    else if fLayer.SupportEntitiyIDs = [idPlace, idBlockInsert] then
      fTrs := trsSymbol
    else
      fTrs := trsAll;

    Enter(fTrs, self.FThematicBuilder, Self.FGis);
    begin
      ShowLegend;
      //Modified := true;
      //if Assigned(OnChange) then
        //OnChange(Self);
    end;
  finally
    free;
  end;
end;

end.


