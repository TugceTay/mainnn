unit Lider.CG.Com.ShowEntityList;

interface

uses
  Windows,
  Classes,
  Controls,
  Forms,
  ComCtrls,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxCustomData,
  cxInplaceContainer,
  cxStyles,
  cxTL,
  cxTextEdit,
  cxTLdxBarBuiltInMenu,
  dxSkinsCore,
  dxSkinsDefaultPainters,
  Lider.CG.Com.GIS,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.EntityInt,
  dxScrollbarAnnotations;

type
  TfmShowEntityList = class(TForm)
    cxTreeList1: TcxTreeList;
    cxTreeList1Column1: TcxTreeListColumn;
    cxTreeList1Column2: TcxTreeListColumn;
    cxTreeList1Column3: TcxTreeListColumn;
    StatusBar1: TStatusBar;
    cxTreeList1Column4: TcxTreeListColumn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cxTreeList1DblClick(Sender: TObject);
  private
    { Private declarations }
    F_drawBox: TlicgBaseDrawBox;
    f_EL: IlicgEntityList;
  public
    { Public declarations }
  end;

procedure ShowEntityList(_drawBox: TlicgBaseDrawBox; cap: string; EL: IlicgEntityList);

implementation


{$R *.dfm}

procedure ShowEntityList(_drawBox: TlicgBaseDrawBox; cap: string; EL: IlicgEntityList);
var
  i: integer;
  N, C: TcxTreeListNode;
  L: TlicgBaseLayer;
begin

  with TfmShowEntityList.Create(nil) do
  begin

    f_drawBox := _drawBox;
    f_EL := EL;

    caption := cap;
    Screen.Cursor := crHourGlass;

    cxTreeList1.BeginUpdate;
    try

      for i := 0 to EL.Count - 1 do
      begin
        if Assigned(EL.Items[i].Layer) then
        begin
          L := TlicgBaseLayer(EL.Items[i].Layer);
          N := cxTreeList1.FindNodeByText(L.DisplayName, cxTreeList1Column1);
          if not Assigned(N) then
          begin
            N := cxTreeList1.AddChild(nil, L);
            N.Values[0] := L.DisplayName;
          end;

          C := cxTreeList1.AddChild(N, TObject(i));

          C.Values[1] := Licad.CreateEntityFactory.GetEntityDisplayText(EL.Items[i].EntityID);
          C.Values[2] := EL.Items[i].Geometry.ID;
          C.Values[3] := EL.Items[i].ParamString;

        end;
      end;

      cxTreeList1.FullExpand;

    except
    end;
    Screen.Cursor := crDefault;
    cxTreeList1.EndUpdate;
    Show;

  end;

end;

procedure TfmShowEntityList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := CaFree;
end;

procedure TfmShowEntityList.cxTreeList1DblClick(Sender: TObject);
begin

  if cxTreeList1.FocusedNode <> nil then
    if not cxTreeList1.FocusedNode.HasChildren then
      F_drawBox.ZoomToEntity(f_EL.Items[integer(cxTreeList1.FocusedNode.Data)]);
end;

end.


