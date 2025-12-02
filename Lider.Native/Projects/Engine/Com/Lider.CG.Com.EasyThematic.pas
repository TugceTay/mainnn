unit Lider.CG.Com.EasyThematic;

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
  Buttons,
  Grids,
  ComCtrls,
  StdCtrls,
  CheckLst,
  DBGrids,
  Db,
  Math,
  ExtCtrls,
  Mask,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  dxSkinsCore,
  cxTextEdit,
  cxMaskEdit,
  cxDropDownEdit,
  dxColorEdit,
  Lider.CG.Com.DB,
  Lider.CG.Com.System,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Base,
  Lider.CG.Com.Consts,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Thematics,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.LicadInt, dxCore;

type
  TfmEasyThematic = class(TForm)
    cmdSearchCompXY_0: TButton;
    Bevel2: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label10: TLabel;
    CLStart: TdxColorEdit;
    CLEnd: TdxColorEdit;
    MERange: TMaskEdit;
    UpDown5: TUpDown;
    CBZero: TCheckBox;
    Label11: TLabel;
    Label12: TLabel;
    Label3: TLabel;
    Label13: TLabel;
    cmdDraw: TButton;
    Label4: TLabel;
    CBfields: TListBox;
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmdDrawClick(Sender: TObject);
    procedure CBfieldsClick(Sender: TObject);
  private
    { Private declarations }
    FGis: TlicgBaseGis;
    FBuilder: TlicgThematicBuilder;
    FLayer: TlicgBaseLayer;
  public
    { Public declarations }
    function Enter(aGis: TlicgBaseGis; Builder: TlicgThematicBuilder): word;
  end;

implementation

{$R *.DFM}

resourcestring
  SSelectLayer = 'Layer to select';

procedure TfmEasyThematic.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmEasyThematic.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfmEasyThematic.cmdDrawClick(Sender: TObject);
var
  i, j: longint;
  sstr: string;

  function RGB(r, g, b: byte): Integer;
  begin
    Result := 65536 * (b) + 256 * (g) + r;
  end;

begin

  if CBFields.ItemIndex < 0 then
  begin
    ShowMessage('Kolon seçiniz.');
    exit;
  end;

  sstr := '';

  sstr := cbfields.items[cbfields.itemindex];
  {
  FBuilder.CreateAutomaticThematicRange(FLayer._Gis,
    UpDown5.Position,
    ExtractFileName(flayer.name),
    sstr,
    clstart.Selected,
    clend.Selected,
    1,
    clstart.Selected,
    clend.Selected,
    1,
    false,
    true,
    CBZero.checked,
    false,
    2000,
    2);   }

  if integer(CBFields.Items.Objects[CBFields.ItemIndex]) = 1 then
    FBuilder.CreateAutomaticThematicRangeInteger(FLayer.GIS, ExtractFileName(flayer.name),
      sstr)
  else if integer(CBFields.Items.Objects[CBFields.ItemIndex]) = 2 then
    FBuilder.CreateAutomaticThematicRangeStrField(FLayer.GIS, ExtractFileName(flayer.name),
      sstr)
  else if integer(CBFields.Items.Objects[CBFields.ItemIndex]) = 3 then
    FBuilder.CreateAutomaticThematicRangeFloat(FLayer.GIS, ExtractFileName(flayer.name),
      sstr)

end;

function TfmEasyThematic.Enter(aGis: TlicgBaseGis; Builder: TlicgThematicBuilder): word;
var
  k: integer;
begin
  Result := mrNone;
  FGis := aGis;
  FBuilder := Builder;
  if Builder.Layername = '' then
    Exit;

  Flayer := FGis.Layers.LayerByName(Builder.Layername);

  if (FLayer <> nil) and (FLayer.dbTable <> nil) then
  begin
    for k := 1 to FLayer.dbTable.FieldCount do
    begin
      if (FLayer.dbTable.FieldType(k) in ['N', 'I']) and (FLayer.dbTable.FieldDec
        (k) = 0) then
      begin
        CBFields.Items.AddObject(FLayer.dbTable.Field(k), TObject(1));
      end
      else if (FLayer.dbTable.FieldType(k) in ['C']) then
      begin
        CBFields.Items.AddObject(FLayer.dbTable.Field(k), TObject(2));
      end
      else if (FLayer.dbTable.FieldType(k) in ['F']) or ((FLayer.dbTable.FieldType
        (k) in ['N']) and (FLayer.dbTable.FieldDec(k) > 0)) then
      begin
        CBFields.Items.AddObject(FLayer.dbTable.Field(k), TObject(3));
      end

    end;
  end
  else
    exit;

  Showmodal;
  Result := ModalResult;
end;

procedure TfmEasyThematic.CBfieldsClick(Sender: TObject);
begin
  if CBfields.ItemIndex >= 0 then
    cmdDraw.Enabled := true;

end;

end.


