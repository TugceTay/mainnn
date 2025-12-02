unit Lider.CG.Com.KatmanOznitelikTablosu;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  DB,
  StdCtrls,
  ExtCtrls,
  Buttons,
  ComCtrls,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  cxLabel,
  cxButtons,
  cxGridLevel,
  cxClasses,
  cxGridCustomView,
  cxGridCustomTableView,
  cxGridTableView,
  cxGrid,
  dxSkinsCore,
  dxSkinsDefaultPainters,
  dxmdaset,
  cxStyles,
  cxCustomData,
  cxFilter,
  cxData,
  cxDataStorage,
  cxNavigator,
  dxDateRanges,
  dxScrollbarAnnotations,
  cxDBData,
  cxPC,
  cxSplitter,
  cxVGrid,
  cxDBVGrid,
   Vcl.Menus,
  Lider.CG.Com.GIS, dxUIAClasses, dxBarBuiltInMenu, cxInplaceContainer;

type
  TfmKatmanOznitelikTablosu = class(TForm)
    pnlRibbon: TPanel;
    btnParseGeometry: TcxButton;
    sbFilterArea: TSpeedButton;
    sbFilterLine: TSpeedButton;
    sbFilterPoint: TSpeedButton;
    btnValidateArea: TcxButton;
    btnValidateLine: TcxButton;
    btnValidatePoint: TcxButton;
    pnlSummary: TPanel;
    lblAreaSummary: TcxLabel;
    lblLineSummary: TcxLabel;
    lblPointSummary: TcxLabel;
    pnlMain: TPanel;
    pcGeometry: TcxPageControl;
    tsArea: TcxTabSheet;
    tsLine: TcxTabSheet;
    tsPoint: TcxTabSheet;
    grdArea: TcxGrid;
    tvArea: TcxGridTableView;
    lvArea: TcxGridLevel;
    grdLine: TcxGrid;
    tvLine: TcxGridTableView;
    lvLine: TcxGridLevel;
    grdPoint: TcxGrid;
    tvPoint: TcxGridTableView;
    lvPoint: TcxGridLevel;
    splDetail: TcxSplitter;
    pnlDetail: TPanel;
    lblDetail: TcxLabel;
    vgDetails: TcxDBVerticalGrid;
    StatusBar: TStatusBar;
    pmValidateArea: TPopupMenu;
    mniAreaMandatory: TMenuItem;
    mniAreaMetrics: TMenuItem;
    pmValidateLine: TPopupMenu;
    mniLineMandatory: TMenuItem;
    mniLineMetrics: TMenuItem;
    pmValidatePoint: TPopupMenu;
    mniPointMandatory: TMenuItem;
    mniPointMetrics: TMenuItem;
    dsArea: TDataSource;
    dsLine: TDataSource;
    dsPoint: TDataSource;
    dsDetail: TDataSource;
    mdArea: TdxMemData;
    mdLine: TdxMemData;
    mdPoint: TdxMemData;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnParseGeometryClick(Sender: TObject);
    procedure sbFilterClick(Sender: TObject);
    procedure pcGeometryChange(Sender: TObject);
    procedure btnValidateAreaClick(Sender: TObject);
    procedure btnValidateLineClick(Sender: TObject);
    procedure btnValidatePointClick(Sender: TObject);
    procedure mniAreaMandatoryClick(Sender: TObject);
    procedure mniAreaMetricsClick(Sender: TObject);
    procedure mniLineMandatoryClick(Sender: TObject);
    procedure mniLineMetricsClick(Sender: TObject);
    procedure mniPointMandatoryClick(Sender: TObject);
    procedure mniPointMetricsClick(Sender: TObject);
    procedure dsDataChange(Sender: TObject; Field: TField);
    procedure tvAreaStylesGetContentStyle(Sender: TcxCustomGridTableView;
      ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      var AStyle: TcxStyle);
    procedure tvLineStylesGetContentStyle(Sender: TcxCustomGridTableView;
      ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      var AStyle: TcxStyle);
    procedure tvPointStylesGetContentStyle(Sender: TcxCustomGridTableView;
      ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      var AStyle: TcxStyle);
  private
    FLayer: TlicgBaseLayer;
    FDrawBox: TlicgBaseDrawBox;
    FInvalidStyle: TcxStyle;
     FRefreshingSummaries: Boolean;
    FLoadingData: Boolean;
    procedure InitializeDatasets;
    procedure SetupMemData(AMemData: TdxMemData; ATag: Integer);
    function ExpectedGeometryType(ADataSet: TDataSet): string;
    function ActiveMemData: TdxMemData;
    procedure MemDataCalcFields(DataSet: TDataSet);
    procedure MemDataBeforePost(DataSet: TDataSet);
    procedure MemDataNewRecord(DataSet: TDataSet);
    function CountInvalidRecords(AMemData: TdxMemData): Integer;
    procedure ApplyValidation(AMemData: TdxMemData);
    procedure PopulateFromLayer;
    procedure RefreshSummaries;
    procedure UpdateDetailBinding;
    procedure UpdateStatus(const AMessage: string);
    procedure EnsureAtLeastOneFilter(Sender: TObject);
    procedure ApplyFilterVisibility;
    procedure SyncDetailRows;
  public
    procedure Enter(ADrawBox: TlicgBaseDrawBox; Layer: TlicgBaseLayer);
  end;

implementation

{$R *.dfm}

uses
  Math,
  StrUtils;

const
  GEOM_AREA = 0;
  GEOM_LINE = 1;
  GEOM_POINT = 2;

procedure TfmKatmanOznitelikTablosu.ApplyFilterVisibility;
begin
  tsArea.TabVisible := sbFilterArea.Down;
  tsLine.TabVisible := sbFilterLine.Down;
  tsPoint.TabVisible := sbFilterPoint.Down;

  if not pcGeometry.ActivePage.TabVisible then
  begin
    if tsArea.TabVisible then
      pcGeometry.ActivePage := tsArea
    else if tsLine.TabVisible then
      pcGeometry.ActivePage := tsLine
    else if tsPoint.TabVisible then
      pcGeometry.ActivePage := tsPoint;
  end;
end;

procedure TfmKatmanOznitelikTablosu.ApplyValidation(AMemData: TdxMemData);
begin
  if AMemData = nil then
    Exit;

  AMemData.DisableControls;
  try
    AMemData.First;
    while not AMemData.Eof do
    begin
      AMemData.FieldByName('ISGEOMETRYMATCH').AsBoolean;
      AMemData.Next;
    end;
  finally
    AMemData.EnableControls;
  end;

  RefreshSummaries;
  UpdateStatus(Format(' sekmesi ',
    [pcGeometry.ActivePage.Caption, CountInvalidRecords(AMemData)]));
end;

function TfmKatmanOznitelikTablosu.ExpectedGeometryType(ADataSet: TDataSet): string;
begin
  case TComponent(ADataSet).Tag of
    GEOM_AREA: Result := 'Area';
    GEOM_LINE: Result := 'Line';
    GEOM_POINT: Result := 'Point';
  else
    Result := '';
  end;
end;

function NormalizeGeometryType(const AValue: string): string;
begin
  if SameText(AValue, 'Polygon') or SameText(AValue, 'Area') then
    Result := 'Area'
  else if SameText(AValue, 'Polyline') or SameText(AValue, 'Line') then
    Result := 'Line'
  else if SameText(AValue, 'Point') then
    Result := 'Point'
  else
    Result := Trim(AValue);
end;

procedure TfmKatmanOznitelikTablosu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfmKatmanOznitelikTablosu.FormCreate(Sender: TObject);
begin
  Caption := 'Katman znitelik Tablosu';
  FInvalidStyle := TcxStyle.Create(Self);
  FInvalidStyle.Color := $00E8C0C0;
  FInvalidStyle.TextColor := clBlack;

  InitializeDatasets;
  dsDetail.DataSet := mdArea;
  vgDetails.DataController.DataSource := dsDetail;
  SyncDetailRows;
  UpdateStatus('Haz'#305'r');
end;

procedure TfmKatmanOznitelikTablosu.InitializeDatasets;
begin
  SetupMemData(mdArea, GEOM_AREA);
  SetupMemData(mdLine, GEOM_LINE);
  SetupMemData(mdPoint, GEOM_POINT);
end;

procedure TfmKatmanOznitelikTablosu.MemDataBeforePost(DataSet: TDataSet);
  var
    GeomType: string;
    Expected: string;
    GuidValue: TGUID;
begin
  Expected := ExpectedGeometryType(DataSet);
  GeomType := NormalizeGeometryType(DataSet.FieldByName('GEOMETRYTYPE').AsString);

  if GeomType = '' then
  begin
    DataSet.FieldByName('GEOMETRYTYPE').AsString := Expected;
    GeomType := Expected;
  end;

  if Trim(DataSet.FieldByName('LAYERUID').AsString) = '' then
  begin
    if CreateGUID(GuidValue) = S_OK then
      DataSet.FieldByName('LAYERUID').AsString := GUIDToString(GuidValue)
    else
      raise Exception.Create('LayerUid zorunlu.');
  end;

  if Trim(DataSet.FieldByName('REVISION').AsString) = '' then
    DataSet.FieldByName('REVISION').AsString := '1';

  if FLoadingData then
    Exit;

  if not SameText(GeomType, Expected) then
    raise Exception.Create('Geometri tipi bu sekmeyle uyumlu deil.');

  case TComponent(DataSet).Tag of
    GEOM_AREA:
      begin
        if not DataSet.FieldByName('ISCLOSED').AsBoolean then
          raise Exception.Create('Alanlar kapalı olmalıdır');
        if DataSet.FieldByName('AREA').AsFloat <= 0 then
          raise Exception.Create('Alan deeri girilmelidir.');
      end;
    GEOM_LINE:
      begin
        if DataSet.FieldByName('VERTEXCOUNT').AsInteger < 2 then
          raise Exception.Create('En az iki k');
        if DataSet.FieldByName('LENGTH').AsFloat <= 0 then
          raise Exception.Create('Uzunluk deeri girilmelidir.');
      end;
    GEOM_POINT:
      begin
        if DataSet.FieldByName('XCOORD').IsNull or DataSet.FieldByName('YCOORD').IsNull then
          raise Exception.Create('Nokta koordinatlarzorunludur.');
      end;
  end;
end;

procedure TfmKatmanOznitelikTablosu.MemDataCalcFields(DataSet: TDataSet);
var
  Expected, GeomType: string;
  Note: string;
  procedure AppendNote(const Msg: string);
  begin
    if Note <> '' then
      Note := Note + ' | ' + Msg
    else
      Note := Msg;
  end;
begin
  Expected := ExpectedGeometryType(DataSet);
  GeomType := NormalizeGeometryType(DataSet.FieldByName('GEOMETRYTYPE').AsString);
  if GeomType = '' then
    GeomType := Expected;
  Note := '';

  if not SameText(GeomType, Expected) then
    AppendNote('Geometri tipi uyu'#351'muyor');

  case TComponent(DataSet).Tag of
    GEOM_AREA:
      begin
        if not DataSet.FieldByName('ISCLOSED').AsBoolean then
          AppendNote('Kapal poligon zorunlu');
        if DataSet.FieldByName('AREA').AsFloat <= 0 then
          AppendNote('Alan deri bekleniyor');
      end;
    GEOM_LINE:
      begin
        if DataSet.FieldByName('VERTEXCOUNT').AsInteger < 2 then
          AppendNote('Minimum ke say 2');
        if DataSet.FieldByName('LENGTH').AsFloat <= 0 then
          AppendNote('Uzunluk deeri bekleniyor');
      end;
    GEOM_POINT:
      begin
        if DataSet.FieldByName('XCOORD').IsNull or DataSet.FieldByName('YCOORD').IsNull then
          AppendNote('Koordinatlar eksik');
      end;
  end;

  DataSet.FieldByName('ISGEOMETRYMATCH').AsBoolean := Note = '';
  if DataSet.FindField('VALIDATIONNOTE') <> nil then
    DataSet.FieldByName('VALIDATIONNOTE').AsString := Note;
end;

procedure TfmKatmanOznitelikTablosu.MemDataNewRecord(DataSet: TDataSet);
var
  GuidValue: TGUID;
begin
  if CreateGUID(GuidValue) = S_OK then
    DataSet.FieldByName('LAYERUID').AsString := GUIDToString(GuidValue);

  DataSet.FieldByName('REVISION').AsString := '1';
  DataSet.FieldByName('GEOMETRYTYPE').AsString := ExpectedGeometryType(DataSet);

  case TComponent(DataSet).Tag of
    GEOM_AREA:
      begin
        DataSet.FieldByName('ISCLOSED').AsBoolean := True;
        DataSet.FieldByName('VERTEXCOUNT').AsInteger := 4;
      end;
    GEOM_LINE:
      begin
        DataSet.FieldByName('VERTEXCOUNT').AsInteger := 2;
      end;
    GEOM_POINT:
      begin
        DataSet.FieldByName('ELEVATION').AsFloat := 0;
      end;
  end;
end;

procedure TfmKatmanOznitelikTablosu.SyncDetailRows;
var
  I: Integer;
  Row: TcxDBEditorRow;
begin
  vgDetails.BeginUpdate;
  try
    vgDetails.ClearRows;
    if (dsDetail.DataSet = nil) or (not dsDetail.DataSet.Active) then
      Exit;

    for I := 0 to dsDetail.DataSet.FieldCount - 1 do
    begin
      Row := vgDetails.Add(TcxDBEditorRow) as TcxDBEditorRow;
      Row.Properties.DataBinding.FieldName := dsDetail.DataSet.Fields[I].FieldName;
      Row.Properties.Caption := dsDetail.DataSet.Fields[I].DisplayName;
    end;
  finally
    vgDetails.EndUpdate;
  end;
end;

procedure TfmKatmanOznitelikTablosu.SetupMemData(AMemData: TdxMemData; ATag: Integer);
type
  TDataSetInitProc = procedure of object;
var
  LMethod: TMethod;

  procedure CreateField(const AName: string; ADataType: TFieldType; ASize: Integer = 0);
  var
    AFieldDef: TFieldDef;
  begin
    AFieldDef := AMemData.FieldDefs.AddFieldDef;
    AFieldDef.Name := AName;
    AFieldDef.DataType := ADataType;
    if ASize > 0 then
      AFieldDef.Size := ASize;
    // TdxMemData için alan nesnesini böyle yaratıyoruz
    AFieldDef.CreateField(AMemData);
  end;

begin
  AMemData.DisableControls;
  try
    if AMemData.Active then
      AMemData.Close;

    // Yapı her çağrıda sıfırlansın
    AMemData.FieldDefs.Clear;
    AMemData.Fields.Clear;

    CreateField('LAYERUID',      ftString, 64);
    CreateField('REVISION',      ftString, 16);
    CreateField('GEOMETRYTYPE',  ftString, 16);
    CreateField('NAME',          ftString, 80);
    CreateField('AREA',          ftFloat);
    CreateField('LENGTH',        ftFloat);
    CreateField('ELEVATION',     ftFloat);
    CreateField('VERTEXCOUNT',   ftInteger);
    CreateField('ISCLOSED',      ftBoolean);
    CreateField('XCOORD',        ftFloat);
    CreateField('YCOORD',        ftFloat);

    // Hesaplanan alanlar dataset kapalıyken eklensin
    with TBooleanField.Create(AMemData) do
    begin
      FieldName := 'ISGEOMETRYMATCH';
      FieldKind := fkCalculated;
      DataSet   := AMemData;
    end;
    with TStringField.Create(AMemData) do
    begin
      FieldName := 'VALIDATIONNOTE';
      FieldKind := fkCalculated;
      Size      := 255;
      DataSet   := AMemData;
    end;

    // Alanlar tanımlandıktan sonra dataset'i aç; CreateDataSet olan sürümler için
    // reflection ile dene, yoksa Open çağrısına düş.
    LMethod.Data := AMemData;
    LMethod.Code := AMemData.MethodAddress('CreateDataSet');

    if Assigned(LMethod.Code) then
      TDataSetInitProc(LMethod)()
    else
      AMemData.Open;
  finally
    AMemData.EnableControls;
  end;

  AMemData.Tag          := ATag;
  AMemData.OnCalcFields := MemDataCalcFields;
  AMemData.BeforePost   := MemDataBeforePost;  // BURAYA DİKKAT
  AMemData.OnNewRecord  := MemDataNewRecord;
end;


procedure TfmKatmanOznitelikTablosu.UpdateDetailBinding;
begin
  case pcGeometry.ActivePageIndex of
    0: dsDetail.DataSet := mdArea;
    1: dsDetail.DataSet := mdLine;
    2: dsDetail.DataSet := mdPoint;
  end;
  vgDetails.DataController.DataSource := dsDetail;
  SyncDetailRows;
end;

procedure TfmKatmanOznitelikTablosu.UpdateStatus(const AMessage: string);
begin
  StatusBar.SimpleText := AMessage;
end;

function TfmKatmanOznitelikTablosu.ActiveMemData: TdxMemData;
begin
  case pcGeometry.ActivePageIndex of
    0: Result := mdArea;
    1: Result := mdLine;
    2: Result := mdPoint;
  else
    Result := nil;
  end;
end;

procedure TfmKatmanOznitelikTablosu.btnParseGeometryClick(Sender: TObject);
begin
  PopulateFromLayer;
  ApplyFilterVisibility;
  RefreshSummaries;
  UpdateStatus('Geometri tiplerine gre ayrtrma tamamlandi');
end;

procedure TfmKatmanOznitelikTablosu.btnValidateAreaClick(Sender: TObject);
begin
  ApplyValidation(mdArea);
end;

procedure TfmKatmanOznitelikTablosu.btnValidateLineClick(Sender: TObject);
begin
  ApplyValidation(mdLine);
end;

procedure TfmKatmanOznitelikTablosu.btnValidatePointClick(Sender: TObject);
begin
  ApplyValidation(mdPoint);
end;

function TfmKatmanOznitelikTablosu.CountInvalidRecords(AMemData: TdxMemData): Integer;
begin
  Result := 0;
  if (AMemData = nil) or not AMemData.Active then
    Exit;

  AMemData.DisableControls;
  try
    AMemData.First;
    while not AMemData.Eof do
    begin
      AMemData.FieldByName('ISGEOMETRYMATCH').AsBoolean;
      if not AMemData.FieldByName('ISGEOMETRYMATCH').AsBoolean then
        Inc(Result);
      AMemData.Next;
    end;
  finally
    AMemData.EnableControls;
  end;
end;

procedure TfmKatmanOznitelikTablosu.dsDataChange(Sender: TObject; Field: TField);
begin
  RefreshSummaries;
end;

procedure TfmKatmanOznitelikTablosu.EnsureAtLeastOneFilter(Sender: TObject);
begin
  if not (sbFilterArea.Down or sbFilterLine.Down or sbFilterPoint.Down) then
    (Sender as TSpeedButton).Down := True;
end;

procedure TfmKatmanOznitelikTablosu.Enter(ADrawBox: TlicgBaseDrawBox; Layer: TlicgBaseLayer);
begin
  FDrawBox := ADrawBox;
  FLayer := Layer;

  PopulateFromLayer;
  ApplyFilterVisibility;
  UpdateDetailBinding;
  UpdateStatus('Kaynak katman yklendi');

  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfmKatmanOznitelikTablosu.pcGeometryChange(Sender: TObject);
begin
  UpdateDetailBinding;
  UpdateStatus(Format('%s sekmesi aktif', [pcGeometry.ActivePage.Caption]));
end;

procedure TfmKatmanOznitelikTablosu.PopulateFromLayer;
  procedure AddSampleRow(AMemData: TdxMemData; const AName: string; const AGeomType: string;
    const AArea, ALength, AX, AY: Double; AVtx: Integer; AClosed: Boolean);
  begin
    AMemData.Append;
    AMemData.FieldByName('NAME').AsString := AName;
    AMemData.FieldByName('GEOMETRYTYPE').AsString := AGeomType;
    AMemData.FieldByName('AREA').AsFloat := AArea;
    AMemData.FieldByName('LENGTH').AsFloat := ALength;
    AMemData.FieldByName('XCOORD').AsFloat := AX;
    AMemData.FieldByName('YCOORD').AsFloat := AY;
    AMemData.FieldByName('VERTEXCOUNT').AsInteger := AVtx;
    AMemData.FieldByName('ISCLOSED').AsBoolean := AClosed;
    AMemData.Post;
  end;
  var
    SourceName: string;
  begin
    FLoadingData := True;
    try
      InitializeDatasets;

      if (FLayer <> nil) then
        SourceName := FLayer.DisplayName
      else
        SourceName := 'CAD Katmani';

      AddSampleRow(mdArea, SourceName + ' Alan 1', 'Area', 1250.5, 0, 0, 0, 5, True);
      AddSampleRow(mdArea, SourceName + ' Alan 2', 'Area', 980.0, 0, 0, 0, 3, False);

      AddSampleRow(mdLine, SourceName + ' Cizgi 1', 'Line', 0, 120.4, 0, 0, 2, True);
      AddSampleRow(mdLine, SourceName + ' Cizgi 2', 'Line', 0, 35.2, 0, 0, 1, True);

      AddSampleRow(mdPoint, SourceName + ' Nokta 1', 'Point', 0, 0, 432100.123, 4543200.789, 0, True);
      AddSampleRow(mdPoint, SourceName + ' Nokta 2', 'Point', 0, 0, 0, 0, 0, True);
    finally
      FLoadingData := False;
    end;

    RefreshSummaries;
    UpdateDetailBinding;
    ApplyValidation(ActiveMemData);
  end;

procedure TfmKatmanOznitelikTablosu.RefreshSummaries;
  function SumField(AMemData: TdxMemData; const AFieldName: string): Double;
  begin
    Result := 0;
    if (AMemData = nil) or not AMemData.Active then
      Exit;

    AMemData.DisableControls;
    try
      AMemData.First;
      while not AMemData.Eof do
      begin
        Result := Result + AMemData.FieldByName(AFieldName).AsFloat;
        AMemData.Next;
      end;
    finally
      AMemData.EnableControls;
    end;
  end;

  function FormatCountAndMetric(const Title: string; ACount: Integer; const MetricName: string; MetricValue: Double): string;
  begin
    if MetricName <> '' then
      Result := Format('%s: %d kay | %s: %.2f', [Title, ACount, MetricName, MetricValue])
    else
      Result := Format('%s: %d kayit', [Title, ACount]);
  end;

  begin
    if FRefreshingSummaries then
      Exit;

    FRefreshingSummaries := True;
    try
      lblAreaSummary.Caption := FormatCountAndMetric('Alan', mdArea.RecordCount, 'Toplam Alan', SumField(mdArea, 'AREA'));
      lblLineSummary.Caption := FormatCountAndMetric('Cizgi', mdLine.RecordCount, 'Toplam Uzunluk', SumField(mdLine, 'LENGTH'));
      lblPointSummary.Caption := FormatCountAndMetric('Nokta', mdPoint.RecordCount, '', 0);
    finally
      FRefreshingSummaries := False;
    end;
  end;

procedure TfmKatmanOznitelikTablosu.sbFilterClick(Sender: TObject);
begin
  EnsureAtLeastOneFilter(Sender);
  ApplyFilterVisibility;
end;

procedure TfmKatmanOznitelikTablosu.tvAreaStylesGetContentStyle(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
var
  LDataSet: TDataSet;
begin
  LDataSet := dsArea.DataSet;
  if (LDataSet <> nil) and LDataSet.Active and (not LDataSet.IsEmpty) then
    if not LDataSet.FieldByName('ISGEOMETRYMATCH').AsBoolean then
      AStyle := FInvalidStyle;
end;

procedure TfmKatmanOznitelikTablosu.tvLineStylesGetContentStyle(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
var
  LDataSet: TDataSet;
begin
  LDataSet := dsLine.DataSet;
  if (LDataSet <> nil) and LDataSet.Active and (not LDataSet.IsEmpty) then
    if not LDataSet.FieldByName('ISGEOMETRYMATCH').AsBoolean then
      AStyle := FInvalidStyle;
end;

procedure TfmKatmanOznitelikTablosu.tvPointStylesGetContentStyle(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
var
  LDataSet: TDataSet;
begin
  LDataSet := dsPoint.DataSet;
  if (LDataSet <> nil) and LDataSet.Active and (not LDataSet.IsEmpty) then
    if not LDataSet.FieldByName('ISGEOMETRYMATCH').AsBoolean then
      AStyle := FInvalidStyle;
end;

procedure TfmKatmanOznitelikTablosu.mniAreaMandatoryClick(Sender: TObject);
begin
  ApplyValidation(mdArea);
end;

procedure TfmKatmanOznitelikTablosu.mniAreaMetricsClick(Sender: TObject);
begin
  ApplyValidation(mdArea);
  UpdateStatus('Alan metrik kontrolü tamamlandı');
end;

procedure TfmKatmanOznitelikTablosu.mniLineMandatoryClick(Sender: TObject);
begin
  ApplyValidation(mdLine);
end;

procedure TfmKatmanOznitelikTablosu.mniLineMetricsClick(Sender: TObject);
begin
  ApplyValidation(mdLine);
  UpdateStatus('Uzunluk ve kontroltamamland');
end;

procedure TfmKatmanOznitelikTablosu.mniPointMandatoryClick(Sender: TObject);
begin
  ApplyValidation(mdPoint);
end;

procedure TfmKatmanOznitelikTablosu.mniPointMetricsClick(Sender: TObject);
begin
  ApplyValidation(mdPoint);
  UpdateStatus('Koordinat kontroltamamland');
end;

end.
