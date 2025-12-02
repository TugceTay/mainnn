unit Lider.CG.Com.FonkSQL;

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
  Menus,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxEdit,
  cxTL,
  cxButtons,
  cxCustomData,
  cxStyles,
  cxButtonEdit,
  cxCheckBox,
  dxSkinsCore,
  cxTLdxBarBuiltInMenu,
  cxContainer,
  dxSkinsDefaultPainters,
  cxInplaceContainer,
  cxMemo,
  cxSplitter,
  cxGroupBox,
  cxGraphics,
  Lider.CG.Com.GIS,
  Lider.CG.Com.GISQSimple,
  Lider.CG.Com.Expressions,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Consts, dxScrollbarAnnotations;

type
  TfmFonkSQL = class(TForm)
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    layerList: TcxTreeList;
    colGroupLayerName: TcxTreeListColumn;
    cxSplitter1: TcxSplitter;
    Panel111: TPanel;
    cxGroupBox1: TcxGroupBox;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    cxButton4: TcxButton;
    cxButton5: TcxButton;
    procedure layerListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure layerListResize(Sender: TObject);
    procedure layerListCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas:
      TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure cxButton1Click(Sender: TObject);
    procedure cxButton2Click(Sender: TObject);
    procedure cxButton4Click(Sender: TObject);
    procedure cxButton5Click(Sender: TObject);
  private
    { Private declarations }
    FGis: TlicgBaseGIS;
    fLayer: TlicgBaseLayer;
    FFileName: string;
    FFormShowed: boolean;
    fmGISQSimple: TfmGISQSimple;
    FModalResult: TModalResult;
    procedure FillLayers(Sender: TObject);
    function VerifyExpression(ShwMsg: boolean): boolean;
  public
    { Public declarations }
    function Enter(aGis: TlicgBaseGIS; aLayer: TlicgBAselayer; ValString: string
      = ''; isModal: boolean = false): boolean;
    function SQLTEXT: string;
  end;

implementation

{$R *.DFM}

uses
  Lider.CG.Com.Lib,
  Lider.CG.Com.Columns,
  Lider.CG.Com.EasyThematic,
  Lider.CG.Com.RangesEditor,
  Lider.CG.Com.System;

{ TfmThematicsEditor }

function TfmFonkSQL.Enter(aGis: TlicgBaseGIS; aLayer: TlicgBAselayer; ValString:
  string = ''; isModal: boolean = false): boolean;
begin
  FGis := aGis;
  fLayer := aLayer;
  FModalResult := mrCancel;
  Result := False;
  if fLayer <> nil then
  begin
    cxSplitter1.Visible := False;
    layerList.Visible := False;
    fmGISQSimple := TfmGISQSimple.Create(nil);
    // ilker silme ileride bak EmbeddedFormPanel1.FormLink := fmGISQSimple.EmbeddedFormLink1;
    fmGISQSimple.Memo1.Text := ValString;
    fmGISQSimple.Enter(FGis, fLayer, false);
    Panel1.Visible := True;

  end
  else
  begin
    FillLayers(nil);
  end;
  if isModal then
  begin
    FormStyle := fsNormal;
    Result := ShowModal = mrOK
  end
  else
  begin
    FormStyle := fsStayOnTop;
    Show;
  end;

  Result := Result or (FModalResult = mrOK);

end;

procedure TfmFonkSQL.FillLayers(Sender: TObject);
var
  i, j: integer;
  L: TlicgBaselayer;
  GNode, LNode, FNode: TcxTreeListNode;
  AGroupName: string;
begin (*  ilker silme
  FNode := nil;

  LayerList.Clear;
  for i := FGis.LayerGroups.Count - 1 downto 0 do
  begin
    G := TlicgLayerGroup(FGis.LayerGroups.Items[i]);
    if G.Layers.Count > 0 then
    begin
      AGroupName := G.Name;
      GNode := LayerList.FindNodeByText(AGroupName, ColGroupLayerName);
      if not Assigned(GNode) then
      begin
        GNode := LayerList.AddChild(nil, G);
        GNode.Values[0] := AGroupName;
      end;

      for j := G.Layers.Count - 1 downto 0 do
      begin
        L := TlicgBaseLayer(G.Layers.Items[j]);
        if L = FGis.CurrentLayer then
          FNode := LNode;
        LNode := LayerList.AddChild(GNode, L);
        LNode.Values[0] := L.DisplayName;

      end;
    end;
  end;

  if FNode <> nil then
  begin
    //layerList.SetFocusedNode(FNode, []);
    //layerList.Select(FNode, []);
  end;
     *)
end;

procedure TfmFonkSQL.layerListClick(Sender: TObject);
var
  Layer: TlicgBaseLayer;
begin
  Panel1.Visible := False;
  if not FFormShowed then
    exit;

  if layerList.FocusedNode = nil then
    exit;

  if not layerList.FocusedNode.HasChildren then
  begin
    if Assigned(fmGISQSimple) then
      FreeAndNil(fmGISQSimple);

    Layer := TlicgBaseLayer(layerList.FocusedNode.Data);
    fmGISQSimple := TfmGISQSimple.Create(nil);
    // ilker silme ileride bak EmbeddedFormPanel1.FormLink := fmGISQSimple.EmbeddedFormLink1;
    fmGISQSimple.Enter(FGis, Layer, fLayer = nil);
    Panel1.Visible := True;
  end
  else
  begin

  end;
end;

procedure TfmFonkSQL.FormCreate(Sender: TObject);
begin
  FFormShowed := False;
end;

procedure TfmFonkSQL.FormShow(Sender: TObject);
begin
  FFormShowed := true;
end;

procedure TfmFonkSQL.layerListResize(Sender: TObject);
begin
  colGroupLayerName.Width := layerList.Width - 25;
end;

procedure TfmFonkSQL.layerListCustomDrawDataCell(Sender: TcxCustomTreeList;
  ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
  if AViewInfo.Node = TcxCustomTreeList(Sender).FocusedNode then
  begin
    ACanvas.Font.Color := ClRed;
  end;
end;

procedure TfmFonkSQL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := CaFree;
end;

function TfmFonkSQL.VerifyExpression(ShwMsg: boolean): boolean;
var
  Expr: string;
  MainExpr: TlicgBaseMainExpr;
begin
  Result := false;
  if Assigned(fmGISQSimple) then
  begin
    Expr := fmGISQSimple.Memo1.Text;
    if Length(Expr) = 0 then
    begin
      result := true;
      exit;
    end;
    MainExpr := Licad.CreateMainExpr(FGis, FLayer);
    try
      MainExpr.ParseExpression(Expr);
      Result := true;
      if ShwMsg then
        MessageToUser('ifade OK.', smsgwarning, MB_ICONINFORMATION);
    finally
      MainExpr.Free;
    end;

  end;
end;

function TfmFonkSQL.SQLTEXT: string;
begin
  Result := '';
  if Assigned(fmGISQSimple) and VerifyExpression(False) then
    Result := fmGISQSimple.Memo1.Text;
end;

procedure TfmFonkSQL.FormDestroy(Sender: TObject);
begin
  if Assigned(fmGISQSimple) then
    FreeAndNil(fmGISQSimple);

end;

procedure TfmFonkSQL.cxButton1Click(Sender: TObject);
var
  s: string;
  i: integer;
begin
  if Assigned(fmGISQSimple) and VerifyExpression(False) then
  begin
    //fmGISQSimple.Memo1.Lines.clear;
    //fmGISQSimple.GenerateQuery;
    s := '';
    for i := 0 to fmGISQSimple.Memo1.Lines.Count - 1 do
      s := s + fmGISQSimple.Memo1.Lines[i] + ' ';
    fmGISQSimple.DataSource1.DataSet.DisableControls;
    fmGISQSimple.cxGrid1DBTableView1.ClearItems;
    fmGISQSimple.ExecuteQuery(s);
    fmGISQSimple.cxGrid1DBTableView1.ClearItems;
    fmGISQSimple.cxGrid1DBTableView1.DataController.CreateAllItems;
    fmGISQSimple.DataSource1.DataSet.EnableControls;
  end;

end;

procedure TfmFonkSQL.cxButton2Click(Sender: TObject);
begin
  if Assigned(fmGISQSimple) then
  begin
    fmGISQSimple.Clear;
  end;
end;

procedure TfmFonkSQL.cxButton4Click(Sender: TObject);
begin
  fModalResult := mrOK;
  Close;
end;

procedure TfmFonkSQL.cxButton5Click(Sender: TObject);
begin
  fModalResult := mrCancel;
  Close;
end;

end.


