unit Lider.CG.Com.Thematics;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Messages,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  Grids,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Base,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.Math,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Grapher,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Columns,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

const
  GLB_ThematicVersion: integer = 1;

type
  {-------------------------------------------------------------------------------}
  {                  Define TlicgThematicBuilder                                        }
  {-------------------------------------------------------------------------------}

  TlicgThematicRangeSupport = (trsAll, trsOpened, trsClosed, trsSymbol);

  TlicgThematicBuilder = class;

  // TlicgThematicItem class
  TlicgThematicItem = class(TCollectionItem)
  private
    FExpression: string;
    FLegend: string;
    FFrequency: Integer;
    FPenStyle: IlicgPenTool;
    FBrushStyle: IlicgBrushTool;
    FSymbolStyle: IlicgSymbolTool;
    FFontStyle: IlicgFontTool;
    procedure SetBrushstyle(Value: IlicgBrushTool);
    procedure SetPenstyle(const Value: IlicgPentool);
    procedure SetSymbolStyle(const Value: IlicgSymboltool);
    procedure SetFontStyle(Value: IlicgFonttool);
    function GetBrushStyle: IlicgBrushTool;
    function GetExpression: string;
    function GetFontStyle: IlicgFontTool;
    function GetFrequency: Integer;
    function GetLegend: string;
    function GetPenStyle: IlicgPenTool;
    function GetSymbolStyle: IlicgSymbolTool;
    procedure SetExpression(const Value: string);
    procedure SetFrequency(const Value: Integer);
    procedure SetLegend(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Legend: string read GetLegend write SetLegend;
    property Expression: string read GetExpression write SetExpression;
    property PenStyle: IlicgPenTool read GetPenStyle write SetPenStyle;
    property BrushStyle: IlicgBrushTool read GetBrushStyle write SetBrushStyle;
    property SymbolStyle: IlicgSymbolTool read GetSymbolStyle write SetSymbolStyle;
    property FontStyle: IlicgFontTool read GetFontStyle write SetFontStyle;
    property Frequency: Integer read GetFrequency write SetFrequency;
  end;

  TlicgThematicRanges = class(TOwnedCollection)
  private
    FThematicBuilder: TlicgThematicBuilder;
    function GetItem(Index: Integer): TlicgThematicItem;
    procedure SetItem(Index: Integer; Value: TlicgThematicItem);
  public
    constructor Create(AOwner: TPersistent);
    destructor destroy; override;
    function Add: TlicgThematicItem;
    function Up(Index: Integer): Boolean;
    function Down(Index: Integer): Boolean;
{$IFDEF DELPHI4}
    procedure Delete(Index: Integer);
{$ENDIF}

    property Items[Index: Integer]: TlicgThematicItem read GetItem write SetItem; default;
  end;

  TlicgThematicBuilder = class(TComponent)
  private
    FLayerName: string;
    FThematicRanges: TlicgThematicRanges;
    FTitle: string;
    FShowThematic: Boolean;
    FExprList: TList;
    FPenStyle: IlicgPenTool;
    FBrushStyle: IlicgBrushTool;
    FSymbolStyle: IlicgSymbolTool;
    FFontStyle: IlicgFontTool;
    FApplyPen: Boolean;
    FApplyBrush: Boolean;
    FApplySymbol: Boolean;
    FApplyFont: Boolean;
//    FApplyRandomColor: Boolean;
    FThematicOpened: Boolean;
    FThematicVersion: integer;
    procedure SetThematicRanges(Value: TlicgThematicRanges);
    procedure BeforePaintEntity(Sender: TObject; Layer: TlicgBaseLayer; Recno:
      Integer; Entity: IlicgEntity; Grapher: TlicgBaseGrapher; Canvas:
      IlicgCanvas; const Clip: TlicgExtent; DrawMode: TlicgDrawMode; var CanShow:
      Boolean; var EntList: IlicgEntityList; var AutoFree: Boolean);
    function StartThematic(Layer: TlicgBaseLayer): Boolean;
    function CalcThematicInfo(Layer: TlicgBaseLayer; Recno: Integer): Boolean;
    procedure EndThematic;
    function GetApplyBrush: Boolean;
//    function GetApplyRandomColor: Boolean;
    function GetApplyFont: Boolean;
    function GetApplyPen: Boolean;
    function GetApplySymbol: Boolean;
    function GetLayerName: string;
    function GetShowThematic: Boolean;
    function GetThematicRanges: TlicgThematicRanges;
    function GetTitle: string;
    procedure SetApplyBrush(const Value: Boolean);
//    procedure SetApplyRandomColor(const Value: Boolean);
    procedure SetApplyFont(const Value: Boolean);
    procedure SetApplyPen(const Value: Boolean);
    procedure SetApplySymbol(const Value: Boolean);
    procedure SetLayerName(const Value: string);
    procedure SetShowThematic(const Value: Boolean);
    procedure SetTitle(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream; path: string);
    procedure LoadFromStream1(Stream: TStream; path: string);
    procedure LoadFromFile(const FileName: string; path: string);
    procedure SaveToFile(const FileName: string);
    procedure Prepare(Layer: TlicgBaseLayer);
    procedure UnPrepare(Layer: TlicgBaseLayer);
    procedure CreateAutomaticThematicRangeInteger(Gis: TlicgBaseGis; const
      ThematicLayer, FieldName: string);
    procedure CreateAutomaticThematicRangeStrField(Gis: TlicgBaseGis; const
      ThematicLayer, FieldName: string);
    procedure Recalculate(Gis: TlicgBaseGis);
    procedure CreateAutomaticThematicRangeFloat(Gis: TlicgBaseGis; const
      ThematicLayer, FieldName: string);
  published
    property LayerName: string read GetLayerName write SetLayerName;
    property ThematicRanges: TlicgThematicRanges read GetThematicRanges write
      SetThematicRanges;
    property Title: string read GetTitle write SetTitle;
    property ShowThematic: Boolean read GetShowThematic write SetShowThematic
      default True;
    property ApplyPen: Boolean read GetApplyPen write SetApplyPen default false;
    property ApplyBrush: Boolean read GetApplyBrush write SetApplyBrush default false;
//    property ApplyRandomColor: Boolean read GetApplyRandomColor write SetApplyRandomColor default false;
    property ApplySymbol: Boolean read GetApplySymbol write SetApplySymbol default false;
    property ApplyFont: Boolean read GetApplyFont write SetApplyFont default false;
  end;

  { TlicgLegend component }

  TlicgLegendItem = class(TCollectionItem)
  private
    FLegend: string;
    FSubLegend: string;
    FFrequency: Integer;
    FPenStyle: IlicgPenTool;
    FBrushStyle: IlicgBrushTool;
    FSymbolStyle: IlicgSymbolTool;
    FFontStyle: IlicgFonttool;
    FColor: TColor;
    FImageIndex: Integer;
    FLegendStyle: TlicgColumnType;
    procedure SetBrushstyle(Value: IlicgBrushTool);
    procedure SetPenstyle(const Value: IlicgPentool);
    procedure SetSymbolStyle(const Value: IlicgSymboltool);
    procedure SetColor(const Value: TColor);
    procedure InvalidateLegend;
    procedure SetImageIndex(const Value: Integer);
    procedure SetFontStyle(const Value: IlicgFontTool);
    function GetBrushStyle: IlicgBrushTool;
    function GetColor: TColor;
    function GetFontStyle: IlicgFonttool;
    function GetFrequency: Integer;
    function GetImageIndex: Integer;
    function GetLegend: string;
    function GetPenStyle: IlicgPenTool;
    function GetSubLegend: string;
    function GetSymbolStyle: IlicgSymbolTool;
    procedure SetFrequency(const Value: Integer);
    procedure SetLegend(const Value: string);
    procedure SetSubLegend(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Legend: string read GetLegend write SetLegend;
    property SubLegend: string read GetSubLegend write SetSubLegend;
    property Frequency: Integer read GetFrequency write SetFrequency;
    property PenStyle: IlicgPenTool read GetPenStyle write SetPenStyle;
    property BrushStyle: IlicgBrushTool read GetBrushStyle write SetBrushStyle;
    property SymbolStyle: IlicgSymbolTool read GetSymbolStyle write SetSymbolStyle;
    property FontStyle: IlicgFonttool read GetFontStyle write SetFontStyle;
    property Color: TColor read GetColor write SetColor;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
  end;

  TlicgLegendRanges = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TlicgLegendItem;
    procedure SetItem(Index: Integer; Value: TlicgLegendItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TlicgLegendItem;
    function Up(Index: Integer): Boolean;
    function Down(Index: Integer): Boolean;
    function Owner: TPersistent;
    property Items[Index: Integer]: TlicgLegendItem read GetItem write SetItem; default;
  end;

  TlicgLegend = class(TCustomGrid)
  private
//    FLegendStyle: TlicgColumnType;
    FLegendRanges: TlicgLegendRanges;
    FShowTitle: Boolean;
    FTitle0: string;
    FTitle1: string;
    FTitle2: string;
    FImageList: TImageList;
    FStretch: Boolean;
    FPenTool: IlicgPenTool;
    FBrushTool: IlicgBrushTool;
    FBorderWidth: Integer;
    FLoweredColor: TColor;
    FTransparent: Boolean;
    FInColChange: Boolean;
    FTitleFont: TFont;
    FTitleColor: TColor;
    FTitleTransparent: Boolean;
    FTitleAlignment: TAlignment;
    //procedure SetLegendStyle(const Value: TlicgColumnType);
    procedure SetLegendRanges(const Value: TlicgLegendRanges);
    procedure SetShowTitle(const Value: Boolean);
    procedure SetTitle0(const Value: string);
    procedure SetTitle1(const Value: string);
    procedure SetTitle2(const Value: string);
    procedure SetImageList(const Value: TImageList);
    procedure InitializeRows;
    procedure SetStretch(const Value: Boolean);
    procedure SetBrushTool(const Value: IlicgBrushTool);
    procedure SetPenTool(const Value: IlicgPenTool);
    procedure SetTitleFont(const Value: TFont);
    function GetBorderWidth: Integer;
    function GetBrushTool: IlicgBrushTool;
    function GetImageList: TImageList;
    function GetLegendRanges: TlicgLegendRanges;
    //function GetLegendStyle: TlicgColumnType;
    function GetLoweredColor: TColor;
    function GetPenTool: IlicgPenTool;
    function GetShowTitle: Boolean;
    function GetStretch: Boolean;
    function GetTitle0: string;
    function GetTitle1: string;
    function GetTitle2: string;
    function GetTitleAlignment: TAlignment;
    function GetTitleColor: TColor;
    function GetTitleFont: TFont;
    function GetTitleTransparent: Boolean;
    function GetTransparent: Boolean;
    procedure SetBorderWidth(const Value: Integer);
    procedure SetLoweredColor(const Value: TColor);
    procedure SetTitleAlignment(const Value: TAlignment);
    procedure SetTitleColor(const Value: TColor);
    procedure SetTitleTransparent(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
      override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ColWidthsChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PopulateFrom(Source: TlicgThematicBuilder);
    procedure AdjustColWidths;

    { inherited properties }
    property ColWidths;
    property Row;
    property RowCount;
    property Col;
    property ColCount;
    property RowHeights;
  published
    property PenTool: IlicgPenTool read GetPenTool write SetPenTool;
    property BrushTool: IlicgBrushTool read GetBrushTool write SetBrushTool;
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth;
    property LoweredColor: TColor read GetLoweredColor write SetLoweredColor;
    property TitleFont: TFont read GetTitleFont write SetTitleFont;
    property TitleColor: TColor read GetTitleColor write SetTitleColor;
    property TitleTransparent: Boolean read GetTitleTransparent write SetTitleTransparent;
    property TitleAlignment: TAlignment read GetTitleAlignment write SetTitleAlignment;
    property Transparent: Boolean read GetTransparent write SetTransparent;
//    property LegendStyle: TlicgColumnType read GetLegendStyle write SetLegendStyle;
    property LegendRanges: TlicgLegendRanges read GetLegendRanges write SetLegendRanges;
    property ShowTitle: Boolean read GetShowTitle write SetShowTitle;
    property Title0: string read GetTitle0 write SetTitle0;
    property Title1: string read GetTitle1 write SetTitle1;
    property Title2: string read GetTitle2 write SetTitle2;
    property ImageList: TImageList read GetImageList write SetImageList;
    property Stretch: Boolean read GetStretch write SetStretch;
    { inherited properties and events }
    property Options;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    property GridLineWidth;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;
    property OnClick;
{$IFDEF LEVEL5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Forms,
  Math,
  Lider.CG.Com.Graphics,
  Lider.CG.Com.System,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Expressions,
  Lider.CG.Com.LicadInt;

{-------------------------------------------------------------------------------}
{                  Implements TlicgThematicItem                                     }
{-------------------------------------------------------------------------------}

constructor TlicgThematicItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FPenStyle := Licad.CreateEntityFactory.CreatePenTool;
  FPenStyle.Style := 0;
  FBrushStyle := Licad.CreateEntityFactory.CreateBrushTool;
  FBrushStyle.Pattern := 1;
  FFontStyle := Licad.CreateEntityFactory.CreateFontTool;
  FSymbolStyle := Licad.CreateEntityFactory.CreateSymbolTool;
end;

destructor TlicgThematicItem.Destroy;
begin
  FPenStyle := nil;
  FBrushStyle := nil;
  FFontStyle := nil;
  FSymbolStyle := nil;
  inherited Destroy;
end;

procedure TlicgThematicItem.Assign(Source: TPersistent);
begin
  if Source is TlicgThematicItem then
  begin
    FExpression := TlicgThematicItem(Source).Expression;
    FLegend := TlicgThematicItem(Source).Legend;
    FFrequency := TlicgThematicItem(Source).FFrequency;
    FPenStyle.Assign(TlicgThematicItem(Source).PenStyle);
    FBrushStyle.Assign(TlicgThematicItem(Source).BrushStyle);
    FFontStyle.Assign(TlicgThematicItem(Source).FontStyle);
    FSymbolStyle.Assign(TlicgThematicItem(Source).SymbolStyle);
  end
  else
    inherited Assign(Source);
end;

function TlicgThematicItem.GetDisplayName: string;
begin
  if FLegend = '' then
    Result := inherited GetDisplayName
  else
    result := FLegend;
end;

procedure TlicgThematicItem.SetBrushstyle(Value: IlicgBrushTool);
begin
  FBrushstyle.Assign(Value);
end;

procedure TlicgThematicItem.SetPenstyle(const Value: IlicgPentool);
begin
  FPenstyle.Assign(Value);
end;

procedure TlicgThematicItem.SetSymbolStyle(const Value: IlicgSymboltool);
begin
  FSymbolstyle.Assign(Value);
end;

procedure TlicgThematicItem.SetFontStyle(Value: IlicgFonttool);
begin
  FFontStyle.Assign(Value);
end;

{-------------------------------------------------------------------------------}
{                  Implements TlicgThematicRanges                                   }
{-------------------------------------------------------------------------------}

constructor TlicgThematicRanges.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TlicgThematicItem);
  FThematicBuilder := AOwner as TlicgThematicBuilder;
end;

destructor TlicgThematicRanges.destroy;
begin
  FThematicBuilder := nil;
  inherited Destroy;
end;

function TlicgThematicRanges.GetItem(Index: Integer): TlicgThematicItem;
begin
  Result := TlicgThematicItem(inherited GetItem(Index));
end;

procedure TlicgThematicRanges.SetItem(Index: Integer; Value: TlicgThematicItem);
begin
  inherited SetItem(Index, Value);
end;

function TlicgThematicRanges.Add: TlicgThematicItem;
begin
  Result := TlicgThematicItem(inherited Add);
end;

function TlicgThematicRanges.Down(Index: Integer): Boolean;
begin
  Result := False;
  if (Index < 0) or (Index >= Count - 1) then
    Exit;
  GetItem(Index).Index := Index + 1;
  Result := True;
end;

function TlicgThematicRanges.Up(Index: Integer): Boolean;
begin
  Result := False;
  if (Index <= 0) or (Index > Count - 1) then
    Exit;
  GetItem(Index).Index := Index - 1;
  Result := True;
end;

{$IFDEF DELPHI4}

procedure TlicgThematicRanges.Delete(Index: Integer);
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;
  GetItem(Index).Free;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
{                  Implements TlicgThematicBuilder                                    }
{-------------------------------------------------------------------------------}

constructor TlicgThematicBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThematicRanges := TlicgThematicRanges.Create(Self);

  FPenStyle := Licad.CreateEntityFactory.CreatePenTool;
  FBrushStyle := Licad.CreateEntityFactory.CreateBrushTool;
  FFontStyle := Licad.CreateEntityFactory.CreateFontTool;
  FSymbolStyle := Licad.CreateEntityFactory.CreateSymbolTool;
  FApplyPen := false;
  FApplyBrush := false;
//  FApplyRandomColor := false;
  FApplySymbol := false;
  FApplyFont := false;
  FShowThematic := false;
end;

destructor TlicgThematicBuilder.Destroy;
begin
  FThematicRanges.Free;
  FPenStyle := nil;
  ;
  FBrushStyle := nil;
  FFontStyle := nil;
  FSymbolStyle := nil;
  if FExprList <> nil then
    EndThematic;
  inherited Destroy;
end;

function TlicgThematicBuilder.GetApplyBrush: Boolean;
begin
  Result := FApplyBrush
end;

{
function TlicgThematicBuilder.GetApplyRandomColor: Boolean;
begin
  Result:=FApplyRandomColor
end;
}

function TlicgThematicBuilder.GetApplyFont: Boolean;
begin
  Result := FApplyFont
end;

function TlicgThematicBuilder.GetApplyPen: Boolean;
begin
  Result := FApplyPen
end;

function TlicgThematicBuilder.GetApplySymbol: Boolean;
begin
  Result := FApplySymbol
end;

function TlicgThematicBuilder.GetLayerName: string;
begin
  Result := FLayerName
end;

function TlicgThematicBuilder.GetShowThematic: Boolean;
begin
  Result := FShowThematic
end;

function TlicgThematicBuilder.GetThematicRanges: TlicgThematicRanges;
begin
  Result := FThematicRanges
end;

function TlicgThematicBuilder.GetTitle: string;
begin
  Result := FTitle
end;

procedure TlicgThematicBuilder.SetApplyBrush(const Value: Boolean);
begin
  FApplyBrush := Value
end;

{
procedure TlicgThematicBuilder.SetApplyRandomColor(const Value: Boolean);
begin
  FApplyRandomColor:= Value
end;
}

procedure TlicgThematicBuilder.SetApplyFont(const Value: Boolean);
begin
  FApplyFont := Value
end;

procedure TlicgThematicBuilder.SetApplyPen(const Value: Boolean);
begin
  FApplyPen := Value
end;

procedure TlicgThematicBuilder.SetApplySymbol(const Value: Boolean);
begin
  FApplySymbol := Value
end;

procedure TlicgThematicBuilder.SetLayerName(const Value: string);
begin
  FLayerName := Value
end;

procedure TlicgThematicBuilder.SetShowThematic(const Value: Boolean);
begin
  FShowThematic := Value
end;

procedure TlicgThematicBuilder.SetTitle(const Value: string);
begin
  FTitle := Value
end;

procedure TlicgThematicBuilder.Assign(Source: TPersistent);
begin
  if Source is TlicgThematicBuilder then
  begin
    LayerName := TlicgThematicBuilder(Source).LayerName;
    ThematicRanges.Assign(TlicgThematicBuilder(Source).ThematicRanges);
    Title := TlicgThematicBuilder(Source).Title;
    ShowThematic := TlicgThematicBuilder(Source).ShowThematic;
    ApplyPen := TlicgThematicBuilder(Source).ApplyPen;
    ApplyBrush := TlicgThematicBuilder(Source).ApplyBrush;
    ApplySymbol := TlicgThematicBuilder(Source).ApplySymbol;
    ApplyFont := TlicgThematicBuilder(Source).ApplyFont;
  end
  else
    inherited Assign(Source);
end;

const
  ThematicFileID = 890504;

procedure TlicgThematicBuilder.SaveToStream(Stream: TStream);
var
  I, n, IDFile: Integer;
  Ps: TlicgPenStyle;
  Bs: TlicgBrushStyle;
  Ss: TlicgSymbolStyle;
  Fs: TlicgFontStyle;
begin
  IDFile := ThematicFileID;
  with Stream do
  begin
    Write(GLB_ThematicVersion, sizeof(integer));
    Write(IDFile, sizeof(IDFile));
    licgWriteStrToStream(FTitle, stream);
    licgWriteStrToStream(FLayerName, stream);
    Write(FShowThematic, sizeof(FShowThematic));
    Write(FApplyPen, sizeof(FApplyPen));
    Write(FApplyBrush, sizeof(FApplyBrush));
    Write(FApplySymbol, sizeof(FApplySymbol));
    Write(FApplyFont, sizeof(FApplyFont));
//    Write(FApplyRandomColor, sizeof(FApplyRandomColor));

    n := ThematicRanges.Count;
    Write(n, sizeof(n));
    for I := 0 to n - 1 do
    begin
      with ThematicRanges[I] do
      begin
        licgWriteStrToStream(FLegend, stream);
        licgWriteStrToStream(FExpression, stream);
        Write(FFrequency, sizeof(FFrequency));

        FPenStyle.FillStyle(Ps);
        FBrushStyle.FillStyle(Bs);
        FSymbolStyle.FillStyle(Ss);
        FFontStyle.FillStyle(Fs);

        Stream.Write(Ps, sizeof(Ps));
        Stream.Write(Bs, sizeof(Bs));
        Stream.Write(Ss, sizeof(Ss));
        Stream.Write(Fs, sizeof(Fs));
        //FPenStyle.SaveToStream(Stream);
        //FBrushStyle.SaveToStream(Stream);
        //FSymbolStyle.SaveToStream(Stream);
        //FFontStyle.SaveToStream(Stream);

      end;
    end;
  end;
end;

procedure TlicgThematicBuilder.LoadFromStream1(Stream: TStream; path: string);
var
  I, n, IDFile: Integer;
  Ps: TlicgPenStyle;
  Bs: TlicgBrushStyle;
  Ss: TlicgSymbolStyle;
  Fs: TlicgFontStyle;
begin
  ThematicRanges.Clear;
  with Stream do
  begin
    Read(IDFile, sizeof(IDFile));
    if IDFile <> ThematicFileID then
      Exit;
    FTitle := licgReadStrFromStream(stream);
    FLayerName := AddSlash(path) + ExtractFileName(licgReadStrFromStream(stream));

    if FileExists(ChangeFileExt(FLayerName, EXT_LAYERDRAW)) then
    begin

      Read(FShowThematic, sizeof(FShowThematic));
      Read(FApplyPen, sizeof(FApplyPen));
      Read(FApplyBrush, sizeof(FApplyBrush));
      Read(FApplySymbol, sizeof(FApplySymbol));
      Read(FApplyFont, sizeof(FApplyFont));
  //    Read(FApplyRandomColor, sizeof(FApplyRandomColor));

      Read(n, sizeof(n));
      for I := 0 to n - 1 do
      begin
        with ThematicRanges.Add do
        begin
          FLegend := licgReadStrFromStream(stream);
          FExpression := licgReadStrFromStream(stream);
          Read(FFrequency, sizeof(FFrequency));

          Stream.Read(Ps, sizeof(Ps));
          Stream.Read(Bs, sizeof(Bs));
          Stream.Read(Ss, sizeof(Ss));
          Stream.Read(Fs, sizeof(Fs));

          FPenStyle.FillStyle(Ps);
          FBrushStyle.FillStyle(Bs);
          FSymbolStyle.FillStyle(Ss);
          FFontStyle.FillStyle(Fs);

          //FPenStyle.LoadFromStream(Stream);
          //FBrushStyle.LoadFromStream(Stream);
          //FSymbolStyle.LoadFromStream(Stream);
          //FFontStyle.LoadFromStream(Stream);
        end;
      end;
    end;
    //else
      //ShowMessage(FLayerName + ' Tematiði oluþturulmuþ tabakaya  ait dosyalar bulunamadý.');
  end;

end;

procedure TlicgThematicBuilder.LoadFromFile(const FileName: string; path: string);
var
  stream: TStream;
begin
  if not FileExists(FileName) then
    exit;
  stream := TFileStream.create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(stream, path);
  finally
    stream.free;
  end;
end;

procedure TlicgThematicBuilder.SaveToFile(const FileName: string);
var
  stream: TStream;
begin
  stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(stream);
  finally
    stream.free;
  end;
end;

procedure TlicgThematicBuilder.SetThematicRanges(Value: TlicgThematicRanges);
begin
  FThematicRanges.Assign(Value);
end;

function TlicgThematicBuilder.StartThematic(Layer: TlicgBaseLayer): Boolean;
var
  MainExpr: TlicgBaseMainExpr;
  I: Integer;
begin
  Result := False;
  if (FThematicRanges.Count = 0) or (Layer = nil) or not (Layer.Active) then
    Exit;
  EndThematic;
  FExprList := TList.Create;
  try
    for I := 0 to FThematicRanges.Count - 1 do
    begin
      if Length(FThematicRanges[I].Expression) = 0 then
        raise EExpression.Create(SExprFail)
      else
      begin
        // evaluate expression
        MainExpr := Licad.CreateMainExpr(Layer.GIS, Layer);
        MainExpr.ParseExpression(FThematicRanges[I].Expression);
        if (MainExpr.Expression <> nil) and (MainExpr.Expression.ExprType <>
          ttBoolean) then
        begin
          MainExpr.Free;
          raise EExpression.Create(SExprFail);
        end;
        if MainExpr.Expression <> nil then
          FExprList.Add(MainExpr)
        else
        begin
          MainExpr.Free;
          raise EExpression.Create(SExprFail);
        end;
      end;
    end;
  except
    Self.FShowThematic := False;
    EndThematic;
    raise;
  end;
  Result := True;
end;

function TlicgThematicBuilder.CalcThematicInfo(Layer: TlicgBaseLayer; Recno:
  Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Layer.Recno <> Recno then
    Layer.Recno := Recno;
  Layer.Synchronize;
  for I := 0 to FExprList.Count - 1 do
    if TlicgBaseMainExpr(FExprList[I]).Expression.AsBoolean = true then
    begin
      Self.FPenStyle.Assign(FThematicRanges[I].PenStyle);
      Self.FBrushStyle.Assign(FThematicRanges[I].BrushStyle);
      Self.FSymbolStyle.Assign(FThematicRanges[I].SymbolStyle);
      Self.FFontStyle.Assign(FThematicRanges[I].FontStyle);

      if Layer.GIS.MapInfo.CS.IsGeographicCS then
      begin
        Self.FSymbolstyle.Height := metertoDeg(FThematicRanges[I].SymbolStyle.Height);
        Self.FPenStyle.Width := metertoDeg(FThematicRanges[I].PenStyle.Width);
        Self.FFontStyle.Height := metertoDeg(FThematicRanges[I].FFontStyle.Height);
      end;

      Result := True;
      Exit;
    end;
end;

procedure TlicgThematicBuilder.EndThematic;
var
  I: Integer;
begin
  if FExprList = nil then
    Exit;
  for I := 0 to FExprList.Count - 1 do
    TlicgBaseMainExpr(FExprList[I]).Free;
  FExprList.Free;
  FExprList := nil;
end;

procedure TlicgThematicBuilder.Recalculate(Gis: TlicgBaseGis);
var
  Layer: TlicgBaseLayer;
  I: Integer;
begin
  Layer := Gis.layers.LayerByName(FLayerName);
  if Layer = nil then
    Exit;
  if not StartThematic(Layer) then
    Exit;
  for I := 0 to FThematicRanges.Count - 1 do
    FThematicRanges[I].FFrequency := 0;
  Screen.Cursor := crHourglass;
  try
    Layer.First;
    Layer.StartBuffering;
    try
      while not Layer.Eof do
      begin
        try
          if Layer.RecIsDeleted then
            Continue;
          Layer.Synchronize;
          for I := 0 to FExprList.Count - 1 do
            if TlicgBaseMainExpr(FExprList[I]).Expression.AsBoolean = true then
            begin
              Inc(FThematicRanges[I].FFrequency);
              Break;
            end;
        finally
          Layer.Next;
        end;
      end;
    finally
      Layer.EndBuffering;
    end;
  finally
    EndThematic;
    Screen.Cursor := crDefault;
  end;
end;

procedure TlicgThematicBuilder.Prepare(Layer: TlicgBaseLayer);
begin
  if FShowThematic and (AnsiCompareText(ExtractFileName(Layer.Name),
    ExtractFileName(Self.FLayerName)) = 0) then
  begin
    FThematicOpened := Self.StartThematic(Layer);

    if FThematicOpened then
    begin
      Layer.OnBeforePaintEntity := Self.BeforePaintEntity;

    end;
  end;
end;

procedure TlicgThematicBuilder.UnPrepare(Layer: TlicgBaseLayer);
begin
  if FThematicOpened and (AnsiCompareText(ExtractFileName(Layer.Name),
    ExtractFileName(Self.FLayerName)) = 0) then
  begin
    EndThematic;
    Layer.OnBeforePaintEntity := nil;
  end;
  FThematicOpened := False;
end;

procedure TlicgThematicBuilder.BeforePaintEntity(Sender: TObject; Layer:
  TlicgBaseLayer; Recno: Integer; Entity: IlicgEntity; Grapher: TlicgBaseGrapher;
  Canvas: IlicgCanvas; const Clip: TlicgExtent; DrawMode: TlicgDrawMode; var CanShow:
  Boolean; var EntList: IlicgEntityList; var AutoFree: Boolean);
begin
  { call the old event handler }
  if not CanShow then
    Exit;

  with Layer do
  begin
    if FThematicOpened and (Entity.EntityID in [idPoint, idPlace, idBlockInsert, idLine, idPolyline,
      idPolygon, idRectangle, idArc, idEllipse, idSpline, idPolyArc,
      idPolyEllipse, idPolyRectangle]) then
    begin
      if not Self.CalcThematicInfo(Layer, Recno) then
        Exit;

      if FApplyPen then
        Entity.DrawTools.PenTool.Assign(Self.FPenstyle);
      if FApplyBrush then
        Entity.DrawTools.BrushTool.Assign(Self.FBrushstyle);
      if FApplySymbol then
        Entity.DrawTools.SymbolTool.Assign(Self.FSymbolStyle);
      if FApplyFont then
        Entity.DrawTools.FontTool.Assign(Self.FFontStyle);

      if (Entity.EntityID = idVectorialText) and FApplyFont then
      begin
        with Entity.DrawTools do
        begin
          FontTool.Name := self.FFontStyle.Name;
          FontTool.Height := self.FFontStyle.Height;
          FontTool.Color := self.FFontStyle.Color;
        end;
      end;
      if (Entity.EntityID = idVectorialFittedText) and FApplyFont then
      begin
        with Entity.DrawTools do
        begin
          FontTool.Name := self.FFontStyle.Name;
          FontTool.Height := self.FFontStyle.Height;
          FontTool.Color := self.FFontStyle.Color;
        end;
      end;
    end;
  end;
end;

{ for string fields only }

procedure TlicgThematicBuilder.CreateAutomaticThematicRangeStrField(Gis:
  TlicgBaseGis; const ThematicLayer, FieldName: string);
var
  Layer: TlicgBaseLayer;
  DiscreteValues: TStringList;
  EdExpr, TmpStr, Pivot, Value: string;
  Cnt, NumRanges, Index: Integer;
  StepColor: TColor;
  MainExpr: TlicgBaseMainExpr;
  FThematicLayer: TlicgBaseLayer;
  ColorList: IlicgIntegerList;
  n, sn: integer;
begin
  if FieldName = '' then
  begin
    MessageToUser(SExpresionNull, smsgerror, MB_ICONERROR);
    Exit;
  end;
  EdExpr := ThematicLayer + '.' + Fieldname;
  FThematicLayer := Gis.layers.LayerByName(Thematiclayer);
  if FThematicLayer = nil then
    Exit;
  MainExpr := Licad.CreateMainExpr(Gis, FThematicLayer);
  try
    MainExpr.ParseExpression(EdExpr);
  except
    on E: Exception do
    begin
      MessageToUser(E.Message, smsgerror, MB_ICONERROR);
      MainExpr.Free;
      Exit;
    end;
  end;
  DiscreteValues := TStringList.create;
  DiscreteValues.Duplicates := dupIgnore;
  DiscreteValues.Sorted := true;
  Licad.ProgressStart(FThematicLayer.RecordCount, SCalculatingRanges);
  try
    cnt := 0;
    FThematicLayer.First;
    while not FThematicLayer.Eof do
    begin
      Inc(cnt);
      Licad.ProgressPosition(cnt);

      if FThematicLayer.RecIsDeleted then
      begin
        FThematicLayer.Next;
        Continue;
      end;
      FThematicLayer.Synchronize;
      Value := MainExpr.Expression.AsString;
      if Length(Trim(Value)) > 0 then
      begin
        DiscreteValues.Add(Value);
      end;
      FThematicLayer.Next;
    end;

    if DiscreteValues.Count = 0 then
      Exit;

    if DiscreteValues.Count > 12000 then
    begin
      Application.MessageBox('Tabakaya ait çok fazla farklý öznitelik kaydý var. 12000 den küçük olmalý.',
        'Bilgi', MB_OK + MB_ICONINFORMATION);
      Exit;
    end;

    //TStringList(DiscreteValues).Sort;
    DiscreteValues.Sorted := false;

    { now delete repeated records }
    Index := 0;
    Pivot := DiscreteValues[0];
    DiscreteValues.Objects[Index] := Pointer(-1);
    Inc(Index);
    while Index < DiscreteValues.Count do
    begin
      if DiscreteValues[Index] <> Pivot then
      begin
        DiscreteValues.Objects[Index] := Pointer(-1);
        Pivot := DiscreteValues[Index];
      end;
      Inc(Index);
    end;
    { now delete all non-marked records and that will be the ranges }
    for Index := DiscreteValues.Count - 1 downto 0 do
      if DiscreteValues.Objects[Index] = nil then
        DiscreteValues.Delete(Index);

    ColorList := CreateIntegerList;
    ColorList.Add(clMaroon);
    ColorList.Add(clGreen);
    ColorList.Add(clOlive);
    ColorList.Add(clNavy);
    ColorList.Add(clPurple);
    ColorList.Add(clTeal);
    ColorList.Add(clGray);
    ColorList.Add(clSilver);
    ColorList.Add(clRed);
    ColorList.Add(clLime);
    ColorList.Add(clYellow);
    ColorList.Add(clBlue);
    ColorList.Add(clFuchsia);
    ColorList.Add(clAqua);
    ColorList.Add(clLime);

    n := -1;

    sn := -1;
    ThematicRanges.Clear;

    NumRanges := DiscreteValues.Count;

    for cnt := 0 to NumRanges - 1 do
    begin

      inc(n);
      if n > ColorList.Count - 1 then
        n := 0;
      inc(sn);
      if sn > CurrCmdLine.ActiveDrawBox.GIS.SymbolsList.Items[0].Count - 1 then
        sn := 0;

      StepColor := ColorList.Items[n];

      DiscreteValues[cnt] := StringReplace(DiscreteValues[cnt], #34, #34#34, [rfReplaceAll]);
      TmpStr := Format('%s = "%s"', [EdExpr, DiscreteValues[cnt]]);

      with ThematicRanges.Add do
      begin
        Expression := TmpStr;
        Legend := DiscreteValues[cnt];
        PenStyle.Style := 0;
        PenStyle.color := RGB(Random(256), Random(256), Random(256)); //StepColor;
        if FALSE then
          PenStyle.Width := Gis.DrawBoxList[0].Grapher.PointsToDistY(cnt)
        else
          PenStyle.Width := 0;
        Brushstyle.Pattern := 1;
        Brushstyle.ForeColor := RGB(Random(256), Random(256), Random(256)); // StepColor;
        Brushstyle.BackColor := 536870911;
        SymbolStyle.Index := sn; // RandomRange(0, licgSymbols.Count - 1);
        Symbolstyle.Height := -10; //Gis.DrawBoxList[0].Grapher.PointsToDistY(20);
      end;
    end;

  finally
    DiscreteValues.free;
    MainExpr.Free;
    Licad.ProgressEnd;
    ColorList := nil;
  end;
end;

{ for numeric fields only }

(*
procedure TlicgThematicBuilder.CreateAutomaticThematicRange(Gis: TlicgBaseGis;
  NumRanges: Integer; const ThematicLayer, FieldName: string;
  BrushStartColor, BrushStopColor: TColor;
  BrushPattern: Integer; LineStartColor, LineStopColor: TColor;
  LineStyle: Integer; AutoLineWidth, CalcbyRange, IgnoreZero, ManualRange: boolean;
  ManualRangevalue: Double; DecimalPos: Integer);

var
  cnt, icnt, TotalAffected, Decimals, n: Integer;
  Value, MinValue, MaxValue, Delta, LowRange, HiRange: Double;
  BeginColor, EndColor, StepColor: TColor;
  LineBeginColor, LineEndColor, LineStepColor: TColor;
  tmpStr: string;
  R, G, B: Byte;
  BeginRGBValue, LineBeginRGBValue: array[0..2] of Byte;
  RGBDiff, LineRGBDiff: array[0..2] of Integer;
  MainExpr: TlicgBaseMainExpr;
  Values: IlicgDoubleList;
  EdExpr, temp1, temp2: string;
  FMax, FMin: double;
  FThematicLayer: TlicgBaseLayer;
  code: integer;
  _NumRanges : integer;
begin
  if FieldName = '' then
  begin
    MessageToUser(SExpresionNull, smsgerror, MB_ICONERROR);
    Exit;
  end;

  EdExpr := ThematicLayer + '.' + Fieldname;
  FThematicLayer := Gis.layers.LayerByName(Thematiclayer);
  if FThematicLayer = nil then
    Exit;

  MainExpr := Licad.CreateMainExpr(Gis, FThematicLayer);
  try
    MainExpr.ParseExpression(EdExpr);
  except
    on E: Exception do
    begin
      MessageToUser(E.Message, smsgerror, MB_ICONERROR);
      MainExpr.Free;
      Exit;
    end;
  end;

  ThematicRanges.Clear;

  Values := CreateDoubleList;
  FMax := -1E20;
  FMin := 1E20;
  try
    {Calculate min and max values}
    Licad.ProgressStart(SCalculatingRanges, 1, FThematicLayer.RecordCount);
    TotalAffected := 0;
    try
      FThematicLayer.First;
      cnt := 0;
      while not FThematicLayer.Eof do
      begin
        Inc(cnt);
        Licad.ProgressPosition(cnt);
        if FThematicLayer.RecIsDeleted then
        begin
          FThematicLayer.Next;
          Continue;
        end;
        FThematicLayer.Synchronize;
        Value := MainExpr.Expression.AsFloat;
        Values.Add(Value);
        if CalcbyRange then
        begin
          if IgnoreZero and (value = 0) then
          begin
            FThematicLayer.Next;
            Continue;
          end;
          FMin := Min(Value, FMin);
          FMax := Max(Value, FMax);
        end;
        Inc(TotalAffected);

        FThematicLayer.Next;
      end;
    finally
      Licad.ProgressEnd;
    end;
    Values.Sort;
    if not CalcbyRange then
    begin // equal number
      FMin := 1;
      FMax := TotalAffected;
      Decimals := 0;
    end
    else begin
      if DecimalPos= 0 then
        Decimals := 1
      else
        Decimals := DecimalPos;
    end;


    if NumRanges = 0 then
      _NumRanges := TotalAffected
    else
      _NumRanges := NumRanges;

    if FMax - FMin = 0 then
      Exit; // can't calculate
    if CalcbyRange and (DecimalPos<>0)   then
      Delta := (FMax - FMin) / _NumRanges
    else
      Delta := ((FMax - FMin + 1) / _NumRanges + 1);

    if (ManualRange) and (CalcbyRange) then
      Delta := ManualRangeValue;

    MinValue := FMin;
    if Decimals = 0  then
    begin
      MinValue := Int(MinValue);
      Delta := Int(Delta);
    end;
    // for fill color
    BeginColor := ColorToRGB(BrushStartColor;
    EndColor := ColorToRGB(BrushStopColor);
    BeginRGBValue[0] := GetRValue(BeginColor);
    BeginRGBValue[1] := GetGValue(BeginColor);
    BeginRGBValue[2] := GetBValue(BeginColor);
    RGBDiff[0] := GetRValue(EndColor) - BeginRGBValue[0];
    RGBDiff[1] := GetGValue(EndColor) - BeginRGBValue[1];
    RGBDiff[2] := GetBValue(EndColor) - BeginRGBValue[2];
    // for line color
    LineBeginColor := ColorToRGB(LineStartColor);
    LineEndColor := ColorToRGB(LineStopColor);
    LineBeginRGBValue[0] := GetRValue(LineBeginColor);
    LineBeginRGBValue[1] := GetGValue(LineBeginColor);
    LineBeginRGBValue[2] := GetBValue(LineBeginColor);
    LineRGBDiff[0] := GetRValue(LineEndColor) - LineBeginRGBValue[0];
    LineRGBDiff[1] := GetGValue(LineEndColor) - LineBeginRGBValue[1];
    LineRGBDiff[2] := GetBValue(LineEndColor) - LineBeginRGBValue[2];

    for cnt := 0 to _NumRanges - 1 do
    begin
      R := BeginRGBValue[0] + MulDiv(cnt, RGBDiff[0], Pred(_NumRanges));
      G := BeginRGBValue[1] + MulDiv(cnt, RGBDiff[1], Pred(_NumRanges));
      B := BeginRGBValue[2] + MulDiv(cnt, RGBDiff[2], Pred(_NumRanges));
      StepColor := RGB(R, G, B);
      // line color
      R := LineBeginRGBValue[0] + MulDiv(cnt, LineRGBDiff[0], Pred(_NumRanges));
      G := LineBeginRGBValue[1] + MulDiv(cnt, LineRGBDiff[1], Pred(_NumRanges));
      B := LineBeginRGBValue[2] + MulDiv(cnt, LineRGBDiff[2], Pred(_NumRanges));
      LineStepColor := RGB(R, G, B);

      if CalcbyRange then
      begin
        MaxValue := MinValue + Delta;
        if Decimals = 0 then
          MaxValue := MaxValue - 1;
      end
      else
      begin
        if cnt = _NumRanges - 1 then
          MaxValue := Values.Count
        else
        begin
          MaxValue := MinValue + Delta;
          if Decimals = 0 then
            MaxValue := MaxValue - 1;
        end;
      end;

      if cnt < Pred(_NumRanges) then
        TmpStr := '(%s >= %s) And (%s < %s)'
      else
        TmpStr := '(%s >= %s) And (%s <= %s)';
      if CalcbyRange then
      begin
        LowRange := MinValue;
        HiRange := MaxValue
      end
      else
      begin
        LowRange := Values[Trunc(MinValue) - 1];
        HiRange := Values[Trunc(MaxValue) - 1];
      end;

      with ThematicRanges.Add do
      begin
        System.Str(LowRange: 30: Decimals, temp1);
        System.Str(HiRange: 30: Decimals, temp2);
        Expression := Format(TmpStr, [EdExpr, trim(temp1), EdExpr, trim(temp2)]);
        Legend := Format('%.*n - %.*n', [Decimals, LowRange, Decimals, HiRange]);
        PenStyle.Style := 0;
        PenStyle.color := LineStepColor;
        if AutoLineWidth then
          PenStyle.Width := Gis.DrawBoxList[0].Grapher.PointsToDistY(cnt)
        else
          PenStyle.Width := 0;
        //PenStyle.Width := Gis.DrawBoxList[0].Grapher.PointsToDistY( cnt );
        Brushstyle.Pattern := Brushstyle.Pattern;
        Brushstyle.ForeColor := StepColor;
        Brushstyle.BackColor := clWhite;
        SymbolStyle.Index := Min(licgSymbols.Count - 1, cnt);
        Symbolstyle.Height := Gis.DrawBoxList[0].Grapher.PointsToDistY(20);
      end;
      MinValue := MaxValue;
    end;
  finally
    MainExpr.Free;
    Values := nil;
  end;
end;
*)

procedure TlicgThematicBuilder.CreateAutomaticThematicRangeInteger(Gis:
  TlicgBaseGis; const ThematicLayer, FieldName: string);
var
  cnt, icnt, TotalAffected, n, sn: Integer;
  tmpStr: string;
  MainExpr: TlicgBaseMainExpr;
  EdExpr: string;
  Values: IlicgIntegerList;
  ColorList: IlicgIntegerList;
  FThematicLayer: TlicgBaseLayer;
  Value: integer;
  StepColor: integer;
begin
  if FieldName = '' then
  begin
    MessageToUser(SExpresionNull, smsgerror, MB_ICONERROR);
    Exit;
  end;

  EdExpr := ThematicLayer + '.' + Fieldname;
  FThematicLayer := Gis.layers.LayerByName(Thematiclayer);
  if FThematicLayer = nil then
    Exit;

  MainExpr := Licad.CreateMainExpr(Gis, FThematicLayer);
  try
    MainExpr.ParseExpression(EdExpr);
  except
    on E: Exception do
    begin
      MessageToUser(E.Message, smsgerror, MB_ICONERROR);
      MainExpr.Free;
      Exit;
    end;
  end;

  ThematicRanges.Clear;

  Values := CreateIntegerList;

  try
    {Calculate min and max values}
    Licad.ProgressStart(FThematicLayer.RecordCount, SCalculatingRanges);
    TotalAffected := 0;
    try
      FThematicLayer.First;
      cnt := 0;
      while not FThematicLayer.Eof do
      begin
        Inc(cnt);
        Licad.ProgressPosition(cnt);
        if FThematicLayer.RecIsDeleted then
        begin
          FThematicLayer.Next;
          Continue;
        end;
        FThematicLayer.Synchronize;
        Value := MainExpr.Expression.AsInteger;
        if Values.IndexofValue(Value) < 0 then
        begin
          Values.Add(Value);
          Inc(TotalAffected);
        end;
        FThematicLayer.Next;
      end;
    finally
      Licad.ProgressEnd;
    end;

    if Values.Count > 12000 then
    begin
      Application.MessageBox('Tabakaya ait çok fazla farklý öznitelik kaydý var. 12000 den küçük olmalý.',
        'Bilgi', MB_OK + MB_ICONINFORMATION);
      Exit;
    end;

    Values.Sort;

    ColorList := CreateIntegerList;
    ColorList.Add(clMaroon);
    ColorList.Add(clGreen);
    ColorList.Add(clOlive);
    ColorList.Add(clNavy);
    ColorList.Add(clPurple);
    ColorList.Add(clTeal);
    ColorList.Add(clGray);
    ColorList.Add(clSilver);
    ColorList.Add(clRed);
    ColorList.Add(clLime);
    ColorList.Add(clYellow);
    ColorList.Add(clBlue);
    ColorList.Add(clFuchsia);
    ColorList.Add(clAqua);
    ColorList.Add(clLime);

    n := -1;
    sn := -1;

    for cnt := 0 to Values.Count - 1 do
    begin

      inc(n);
      inc(sn);

      if n > ColorList.Count - 1 then
        n := 0;

      if sn > CurrCmdLine.ActiveDrawBox.GIS.SymbolsList.Items[0].Count - 1 then
        sn := 0;

      StepColor := ColorList.Items[n];

      with ThematicRanges.Add do
      begin
        Expression := EdExpr + '=' + FloatToStr(Values.Items[cnt]);
        Legend := FloatToStr(Values.Items[cnt]);
        PenStyle.Style := 0;
        PenStyle.color := RGB(Random(256), Random(256), Random(256)); // StepColor;
        if FALSE then
          PenStyle.Width := Gis.DrawBoxList[0].Grapher.PointsToDistY(cnt)
        else
          PenStyle.Width := 0;
        //PenStyle.Width := Gis.DrawBoxList[0].Grapher.PointsToDistY( cnt );
        Brushstyle.Pattern := 1; // Brushstyle.Pattern;
        Brushstyle.ForeColor := RGB(Random(256), Random(256), Random(256)); // StepColor;
        Brushstyle.BackColor := 536870911;
        SymbolStyle.Index := sn; //RandomRange(0,licgSymbols.Count - 1); //Min(licgSymbols.Count - 1, cnt);
        Symbolstyle.Height := -10; // Gis.DrawBoxList[0].Grapher.PointsToDistY(20);
      end;
    end;
  finally
    MainExpr.Free;
    Values := nil;
    ColorList := nil;
  end;
end;

procedure TlicgThematicBuilder.CreateAutomaticThematicRangeFloat(Gis: TlicgBaseGis;
  const ThematicLayer, FieldName: string);
var
  cnt, icnt, TotalAffected, n, sn: Integer;
  tmpStr: string;
  MainExpr: TlicgBaseMainExpr;
  EdExpr: string;
  Values: IlicgDoubleList;
  ColorList: IlicgIntegerList;
  FThematicLayer: TlicgBaseLayer;
  Value: double;
  StepColor: integer;
begin
  if FieldName = '' then
  begin
    MessageToUser(SExpresionNull, smsgerror, MB_ICONERROR);
    Exit;
  end;

  EdExpr := ThematicLayer + '.' + Fieldname;
  FThematicLayer := Gis.layers.LayerByName(Thematiclayer);
  if FThematicLayer = nil then
    Exit;

  MainExpr := Licad.CreateMainExpr(Gis, FThematicLayer);
  try
    MainExpr.ParseExpression(EdExpr);
  except
    on E: Exception do
    begin
      MessageToUser(E.Message, smsgerror, MB_ICONERROR);
      MainExpr.Free;
      Exit;
    end;
  end;

  ThematicRanges.Clear;

  Values := CreateDoubleList;

  try
    {Calculate min and max values}
    Licad.ProgressStart(FThematicLayer.RecordCount, SCalculatingRanges);
    TotalAffected := 0;
    try
      FThematicLayer.First;
      cnt := 0;
      while not FThematicLayer.Eof do
      begin
        Inc(cnt);
        Licad.ProgressPosition(cnt);
        if FThematicLayer.RecIsDeleted then
        begin
          FThematicLayer.Next;
          Continue;
        end;
        FThematicLayer.Synchronize;
        Value := MainExpr.Expression.AsFloat;
        Values.Add(Value);
        Inc(TotalAffected);
        FThematicLayer.Next;
      end;
    finally
      Licad.ProgressEnd;
    end;

    if Values.Count > 12000 then
    begin
      Application.MessageBox('Tabakaya ait çok fazla farklý öznitelik kaydý var. 12000 den küçük olmalý.',
        'Bilgi', MB_OK + MB_ICONINFORMATION);
      Exit;
    end;

    Values.Sort;

    ColorList := CreateIntegerList;
    ColorList.Add(clMaroon);
    ColorList.Add(clGreen);
    ColorList.Add(clOlive);
    ColorList.Add(clNavy);
    ColorList.Add(clPurple);
    ColorList.Add(clTeal);
    ColorList.Add(clGray);
    ColorList.Add(clSilver);
    ColorList.Add(clRed);
    ColorList.Add(clLime);
    ColorList.Add(clYellow);
    ColorList.Add(clBlue);
    ColorList.Add(clFuchsia);
    ColorList.Add(clAqua);
    ColorList.Add(clLime);

    n := -1;
    sn := -1;

    for cnt := 0 to Values.Count - 1 do
    begin

      inc(n);
      inc(sn);

      if n > ColorList.Count - 1 then
        n := 0;

      if sn > CurrCmdLine.ActiveDrawBox.GIS.SymbolsList.Items[0].Count - 1 then
        sn := 0;

      StepColor := ColorList.Items[n];

      with ThematicRanges.Add do
      begin
        Expression := EdExpr + '=' + FloatToStr(Values.Items[cnt]);
        Legend := FloatToStr(Values.Items[cnt]);
        PenStyle.Style := 0;
        PenStyle.color := RGB(Random(256), Random(256), Random(256)); // StepColor;
        if FALSE then
          PenStyle.Width := Gis.DrawBoxList[0].Grapher.PointsToDistY(cnt)
        else
          PenStyle.Width := 0;
        //PenStyle.Width := Gis.DrawBoxList[0].Grapher.PointsToDistY( cnt );
        Brushstyle.Pattern := 1; // Brushstyle.Pattern;
        Brushstyle.ForeColor := RGB(Random(256), Random(256), Random(256)); // StepColor;
        Brushstyle.BackColor := 536870911;
        SymbolStyle.Index := sn; //RandomRange(0,licgSymbols.Count - 1); //Min(licgSymbols.Count - 1, cnt);
        Symbolstyle.Height := -10; // Gis.DrawBoxList[0].Grapher.PointsToDistY(20);
      end;
    end;
  finally
    MainExpr.Free;
    Values := nil;
    ColorList := nil;
  end;
end;

{ TlicgLegendItem }

constructor TlicgLegendItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FPenStyle := Licad.CreateEntityFactory.CreatePenTool;
  FPenStyle.Style := 0;
  FBrushStyle := Licad.CreateEntityFactory.CreateBrushTool;
  FBrushStyle.Pattern := 1;
  FSymbolStyle := Licad.CreateEntityFactory.CreateSymbolTool;
end;

destructor TlicgLegendItem.Destroy;
begin
  FPenStyle := nil;
  FBrushStyle := nil;
  FSymbolStyle := nil;
  inherited;
end;

procedure TlicgLegendItem.Assign(Source: TPersistent);
begin
  if Source is TlicgLegendItem then
  begin
    FLegend := TlicgLegendItem(Source).Legend;
    FPenStyle.Assign(TlicgLegendItem(Source).PenStyle);
    FBrushStyle.Assign(TlicgLegendItem(Source).BrushStyle);
    FSymbolStyle.Assign(TlicgLegendItem(Source).SymbolStyle);
    InvalidateLegend;
  end
  else
    inherited Assign(Source);
end;

function TlicgLegendItem.GetDisplayName: string;
begin
  if FLegend = '' then
    Result := inherited GetDisplayName
  else
    result := FLegend;
end;

procedure TlicgLegendItem.SetBrushstyle(Value: IlicgBrushTool);
begin
  FBrushstyle.Assign(Value);
  InvalidateLegend;
end;

procedure TlicgLegendItem.SetPenstyle(const Value: IlicgPentool);
begin
  FPenstyle.Assign(Value);
  InvalidateLegend;
end;

procedure TlicgLegendItem.SetSymbolStyle(const Value: IlicgSymboltool);
begin
  FSymbolstyle.Assign(Value);
  InvalidateLegend;
end;

procedure TlicgLegendItem.SetFontStyle(const Value: IlicgFonttool);
begin
  FFontStyle.Assign(Value);
  InvalidateLegend;
end;

procedure TlicgLegendItem.SetColor(const Value: TColor);
begin
  FColor := Value;
  InvalidateLegend;
end;

procedure TlicgLegendItem.InvalidateLegend;
begin
  if TlicgLegendRanges(Collection).Owner is TlicgLegend then
    TlicgLegend(TlicgLegendRanges(Collection).Owner).Invalidate;
end;

procedure TlicgLegendItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  InvalidateLegend;
end;

function TlicgLegendItem.GetBrushStyle: IlicgBrushTool;
begin
  Result := FBrushStyle
end;

function TlicgLegendItem.GetColor: TColor;
begin
  Result := FColor
end;

function TlicgLegendItem.GetFontStyle: IlicgFonttool;
begin
  Result := FFontStyle
end;

function TlicgLegendItem.GetFrequency: Integer;
begin
  Result := FFrequency
end;

function TlicgLegendItem.GetImageIndex: Integer;
begin
  Result := FImageIndex
end;

function TlicgLegendItem.GetLegend: string;
begin
  Result := FLegend
end;

function TlicgLegendItem.GetPenStyle: IlicgPenTool;
begin
  Result := FPenStyle
end;

function TlicgLegendItem.GetSubLegend: string;
begin
  Result := FSubLegend
end;

function TlicgLegendItem.GetSymbolStyle: IlicgSymbolTool;
begin
  Result := FSymbolStyle
end;

procedure TlicgLegendItem.SetFrequency(const Value: Integer);
begin
  FFrequency := Value
end;

procedure TlicgLegendItem.SetLegend(const Value: string);
begin
  FLegend := Value
end;

procedure TlicgLegendItem.SetSubLegend(const Value: string);
begin
  FSubLegend := Value
end;

{ TlicgLegendRanges }

constructor TlicgLegendRanges.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TlicgLegendItem);
end;

function TlicgLegendRanges.Add: TlicgLegendItem;
begin
  Result := TlicgLegendItem(inherited Add);
end;

function TlicgLegendRanges.Up(Index: Integer): Boolean;
begin
  Result := False;
  if (Index <= 0) or (Index > Count - 1) then
    Exit;
  GetItem(Index).Index := Index - 1;
  Result := True;
end;

function TlicgLegendRanges.Down(Index: Integer): Boolean;
begin
  Result := False;
  if (Index < 0) or (Index >= Count - 1) then
    Exit;
  GetItem(Index).Index := Index + 1;
  Result := True;
end;

function TlicgLegendRanges.GetItem(Index: Integer): TlicgLegendItem;
begin
  Result := TlicgLegendItem(inherited GetItem(Index));
end;

procedure TlicgLegendRanges.SetItem(Index: Integer; Value: TlicgLegendItem);
begin
  inherited SetItem(Index, Value);
end;

function TlicgLegendRanges.Owner: TPersistent;
begin
  Result := inherited GetOwner;
end;

{ TlicgLegend class implementation }

{ TlicgLegend }

constructor TlicgLegend.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLegendRanges := TlicgLegendRanges.Create(Self);
  FixedCols := 0;
  FixedRows := 1;
  ColCount := 3;

  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
  ShowTitle := true;
  FTitleFont := TFont.Create;
  FTitleColor := clBlack;
  FTitleTransparent := false;
  FTitleAlignment := taCenter;
  FPenTool := Licad.CreateEntityFactory.CreatePenTool;
  FBrushTool := Licad.CreateEntityFactory.CreateBrushTool;
  FBorderWidth := 1;
  FLoweredColor := clGray;
end;

destructor TlicgLegend.Destroy;
begin
  FLegendRanges.Free;
  FPentool := nil;
  FBrushtool := nil;
  FTitleFont.Free;
  inherited Destroy;
end;

function TlicgLegend.GetBorderWidth: Integer;
begin
  Result := FBorderWidth
end;

function TlicgLegend.GetBrushTool: IlicgBrushTool;
begin
  Result := FBrushTool
end;

function TlicgLegend.GetImageList: TImageList;
begin
  Result := FImageList
end;

function TlicgLegend.GetLegendRanges: TlicgLegendRanges;
begin
  Result := FLegendRanges
end;

{
function TlicgLegend.GetLegendStyle: TlicgColumnType;
begin
  Result:=FLegendStyle
end;
}

function TlicgLegend.GetLoweredColor: TColor;
begin
  Result := FLoweredColor
end;

function TlicgLegend.GetPenTool: IlicgPenTool;
begin
  Result := FPenTool
end;

function TlicgLegend.GetShowTitle: Boolean;
begin
  Result := FShowTitle
end;

function TlicgLegend.GetStretch: Boolean;
begin
  Result := FStretch
end;

function TlicgLegend.GetTitle0: string;
begin
  Result := FTitle0
end;

function TlicgLegend.GetTitle1: string;
begin
  Result := FTitle1
end;

function TlicgLegend.GetTitle2: string;
begin
  Result := FTitle2
end;

function TlicgLegend.GetTitleAlignment: TAlignment;
begin
  Result := FTitleAlignment
end;

function TlicgLegend.GetTitleColor: TColor;
begin
  Result := FTitleColor
end;

function TlicgLegend.GetTitleFont: TFont;
begin
  Result := FTitleFont
end;

function TlicgLegend.GetTitleTransparent: Boolean;
begin
  Result := FTitleTransparent
end;

function TlicgLegend.GetTransparent: Boolean;
begin
  Result := FTransparent
end;

procedure TlicgLegend.SetBorderWidth(const Value: Integer);
begin
  FBorderWidth := Value
end;

procedure TlicgLegend.SetLoweredColor(const Value: TColor);
begin
  FLoweredColor := Value
end;

procedure TlicgLegend.SetTitleAlignment(const Value: TAlignment);
begin
  FTitleAlignment := Value
end;

procedure TlicgLegend.SetTitleColor(const Value: TColor);
begin
  FTitleColor := Value
end;

procedure TlicgLegend.SetTitleTransparent(const Value: Boolean);
begin
  FTitleTransparent := Value
end;

procedure TlicgLegend.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value
end;

procedure TlicgLegend.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  uFormat: Word;
  AText: string;
  AIndex: Integer;
  AItem: TlicgLegendItem;
  R: TRect;
  Image: TBitmap;
  Ax, Ay: Integer;

  procedure DrawLineStyle;
  var
    FGrapher: TlicgGrapher;
  begin
    FGrapher := TlicgGrapher.Create(adScreen);
    try
      if AItem.FPenStyle.Style < 0 then
        Exit;
      Canvas.Font.Assign(Self.Font);

      if Licad <> nil then
        Licad.DrawLineType(FGrapher, GetIlicgCanvas(Canvas), AItem.FPenStyle.Style,
          ARect, False, AItem.FPenStyle.Color, Self.Color, 0, 2, True, False,
          True, True, False);
    finally
      FGrapher.free;
    end;
  end;

  procedure DrawBrushStyle;
  var
    FGrapher: TlicgGrapher;
  begin
    FGrapher := TlicgGrapher.Create(adScreen);
    try
      Canvas.Font.Assign(Self.Font);
      if Licad <> nil then
        Licad.DrawPattern(GetIlicgCanvas(Canvas), AItem.FBrushStyle.Pattern,
          AItem.FBrushStyle.ForeColor, AItem.FBrushStyle.BackColor, Self.Color,
          ARect, False, False, False, True, False);
    finally
      FGrapher.free;
    end;
  end;

  procedure DrawSymbolStyle;
  var
    FGrapher: TlicgGrapher;
    ValInteger: Integer;
  begin
    ValInteger := AItem.FSymbolStyle.Index;
    FGrapher := TlicgGrapher.Create(adScreen);
    try
      if ValInteger < 0 then
        Exit;
      if Licad <> nil then
        Licad.DrawSymbol(FGrapher, GetIlicgCanvas(Canvas), ValInteger, ARect,
          False, Self.Color, clNone, true, False, true, true, true, '');
    finally
      FGrapher.free;
    end;
  end;

begin
  InitializeRows;
  if csDesigning in ComponentState then
    Exit;
  with Canvas do
  begin
    Font.Assign(Self.Font);
    if FShowTitle and (ARow = 0) then
    begin
      case ACol of
        0:
          AText := FTitle0;
        1:
          AText := FTitle1;
        2:
          AText := FTitle2;
      end;
      Font.Style := Font.Style + [fsBold];
      uFormat := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
      DrawText(Handle, PChar(AText), -1, ARect, uFormat);
    end;
    if (FShowTitle and (ARow > 0)) or (not FShowTitle and (ARow >= 0)) then
    begin
      InflateRect(ARect, -1, -1);
      if FShowTitle then
        AIndex := ARow - 1
      else
        AIndex := ARow;
      if AIndex > FLegendRanges.Count - 1 then
        Exit;
      AItem := FLegendRanges[AIndex];
      if ACol = 0 then
      begin
        case AItem.FLegendStyle of
          ctLineStyle:
            begin
              DrawLineStyle;
            end;
          ctBrushStyle:
            begin
              DrawBrushStyle;
            end;
          ctColor:
            begin
              Pen.Width := 1;
              Pen.Color := clBlack;
              Brush.Style := bsSolid;
              Brush.Color := AItem.FColor;
              R := ARect;
              Windows.InflateRect(R, -2, -2);
              with R do
                Rectangle(left, top, right, bottom);
            end;
          ctSymbolStyle:
            begin
              DrawSymbolStyle;
            end;
          ctBitmap:
            begin
              if FImageList = nil then
                Exit;
              if AItem.FImageIndex > ImageList.Count - 1 then
                exit;
              Image := TBitmap.Create;
              try
                ImageList.GetBitmap(AIndex, Image);
                Image.Transparent := true;
                Image.TransparentMode := tmAuto;
                if FStretch then
                begin
                  R := ARect;
                  Windows.InflateRect(R, -2, -2);
                  StretchDraw(R, Image);
                end
                else
                begin
                  Ax := ARect.Left + Max(((ARect.Right - ARect.Left) - Image.Width) div 2, 0);
                  Ay := ARect.Top + Max(((ARect.Bottom - ARect.Top) - Image.Height) div 2, 0);
                  Draw(Ax, Ay, Image);
                end;
              finally
                Image.free;
              end;
            end;
        end;
      end
      else if ACol = 1 then
      begin
        AText := AItem.FLegend;
        uFormat := DT_LEFT or DT_VCENTER or DT_SINGLELINE;
        DrawText(Handle, PChar(AText), -1, ARect, uFormat);
      end
      else if ACol = 2 then
      begin
        AText := IntToStr(AItem.FFrequency);
        uFormat := DT_RIGHT or DT_VCENTER or DT_SINGLELINE;
        DrawText(Handle, PChar(AText), -1, ARect, uFormat);
      end;
    end;
  end;
end;

procedure TlicgLegend.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;
end;

procedure TlicgLegend.SetImageList(const Value: TImageList);
begin
  if FImageList = Value then
    Exit;
{$IFDEF LEVEL5}
  if Assigned(FImageList) then
    FImageList.RemoveFreeNotification(Self);
{$ENDIF}
  if Value <> nil then
    Value.FreeNotification(Self);
  FImageList := Value;
end;

procedure TlicgLegend.SetLegendRanges(const Value: TlicgLegendRanges);
begin
  FLegendRanges.Assign(Value);
end;

{
procedure TlicgLegend.SetLegendStyle(const Value: TlicgColumnType);
begin
  if FLegendStyle = Value then
    Exit;
  FLegendStyle := Value;
end;
}

procedure TlicgLegend.SetShowTitle(const Value: Boolean);
begin
  if FShowTitle = Value then
    exit;
  FShowTitle := Value;
  InitializeRows;
end;

procedure TlicgLegend.InitializeRows;
begin
  if FShowTitle then
  begin
    if FLegendRanges.Count = 0 then
    begin
      if RowCount <> 2 then
        RowCount := 2;
    end
    else
    begin
      if RowCount <> FLegendRanges.Count + 1 then
        RowCount := FLegendRanges.Count + 1;
    end;
    if FixedRows <> 1 then
      FixedRows := 1;
  end
  else
  begin
    if FLegendRanges.Count = 0 then
    begin
      if RowCount <> 0 then
        RowCount := 0;
    end
    else
    begin
      if RowCount <> FLegendRanges.Count then
        RowCount := FLegendRanges.Count;
    end;
    if FixedRows <> 0 then
      FixedRows := 0;
  end;
end;

procedure TlicgLegend.SetTitle0(const Value: string);
begin
  FTitle0 := Value;
  Invalidate;
end;

procedure TlicgLegend.SetTitle1(const Value: string);
begin
  FTitle1 := Value;
  Invalidate;
end;

procedure TlicgLegend.SetTitle2(const Value: string);
begin
  FTitle2 := Value;
  Invalidate;
end;

procedure TlicgLegend.PopulateFrom(Source: TlicgThematicBuilder);
var
  I: Integer;
  SourceItem: TlicgThematicItem;
begin
  FLegendRanges.Clear;
  (*
  for I := 0 to Source.FThematicRanges.Count - 1 do
  begin
    SourceItem := Source.FThematicRanges[I];
    with FLegendRanges.Add do
    begin
      FPenStyle.Copy(SourceItem.FPenStyle);
      FBrushStyle.Copy(SourceItem.FBrushStyle);
      FSymbolStyle.Copy(SourceItem.FSymbolStyle);
      FLegend := SourceItem.FLegend;
      FFrequency := SourceItem.Frequency;
    end;
  end;
  FTitle1 := Source.Title;
  if Source.ApplyPen then
    FLegendStyle := ctLineStyle
  else if Source.ApplyBrush then
    FLegendStyle := ctBrushStyle
//  else if Source.ApplyRandomColor then
  //  FLegendStyle := ctColor
  else if Source.ApplySymbol then
    FLegendStyle := ctSymbolStyle;

  *)

  for I := 0 to Source.FThematicRanges.Count - 1 do
  begin
    SourceItem := Source.FThematicRanges[I];

    if Source.ApplyPen then
    begin
      with FLegendRanges.Add do
      begin
        FPenStyle.Assign(SourceItem.FPenStyle);
        FLegend := SourceItem.FLegend;
        FFrequency := SourceItem.Frequency;
        FLegendStyle := ctLineStyle;
      end;
    end;

    if Source.ApplyBrush then
    begin
      with FLegendRanges.Add do
      begin
        FBrushStyle.Assign(SourceItem.FBrushStyle);
        FLegend := SourceItem.FLegend;
        FFrequency := SourceItem.Frequency;
        FLegendStyle := ctBrushStyle;
      end;
    end;

    if Source.ApplySymbol then
    begin
      with FLegendRanges.Add do
      begin
        FSymbolStyle.Assign(SourceItem.FSymbolStyle);
        FLegend := SourceItem.FLegend;
        FFrequency := SourceItem.Frequency;
        FLegendStyle := ctSymbolStyle;
      end;
    end;

  end;
  FTitle1 := Source.Title;

  InitializeRows;
  Invalidate;
end;

procedure TlicgLegend.SetStretch(const Value: Boolean);
begin
  if FStretch = Value then
    exit;
  FStretch := Value;
  Invalidate;
end;

procedure TlicgLegend.AdjustColWidths;
begin
  if FInColChange then
    Exit;
  FInColChange := true;
  try
    if ColWidths[0] >= MulDiv(ClientWidth, 1, 3) then
    begin
      ColWidths[0] := MulDiv(ClientWidth, 1, 3);
    end;
    ColWidths[2] := ClientWidth - ColWidths[0] - ColWidths[1] - GetSystemMetrics
      (SM_CXBORDER) * 1 - GridLineWidth * ColCount;
  finally
    FInColChange := false;
  end;
end;

procedure TlicgLegend.ColWidthsChanged;
begin
  AdjustColWidths;
  inherited ColWidthsChanged;
end;

procedure TlicgLegend.SetBrushTool(const Value: IlicgBrushTool);
begin
  FBrushTool.Assign(Value);
end;

procedure TlicgLegend.SetPenTool(const Value: IlicgPenTool);
begin
  FPenTool.Assign(Value);
end;

procedure TlicgLegend.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

function TlicgThematicItem.GetBrushStyle: IlicgBrushTool;
begin
  Result := FBrushStyle
end;

function TlicgThematicItem.GetExpression: string;
begin
  Result := FExpression
end;

function TlicgThematicItem.GetFontStyle: IlicgFontTool;
begin
  Result := FFontStyle
end;

function TlicgThematicItem.GetFrequency: Integer;
begin
  Result := FFrequency
end;

function TlicgThematicItem.GetLegend: string;
begin
  Result := FLegend
end;

function TlicgThematicItem.GetPenStyle: IlicgPenTool;
begin
  Result := FPenStyle
end;

function TlicgThematicItem.GetSymbolStyle: IlicgSymbolTool;
begin
  Result := FSymbolStyle
end;

procedure TlicgThematicItem.SetExpression(const Value: string);
begin
  FExpression := Value
end;

procedure TlicgThematicItem.SetFrequency(const Value: Integer);
begin
  FFrequency := Value
end;

procedure TlicgThematicItem.SetLegend(const Value: string);
begin
  FLegend := Value
end;

procedure TlicgThematicBuilder.LoadFromStream(Stream: TStream; path: string);
var
  v: integer;
begin
  v := 1;
  Stream.Read(v, sizeof(integer));
  FThematicVersion := v;
  case FThematicVersion of
    1:
      LoadFromStream1(Stream, path);
  end;
end;

end.


