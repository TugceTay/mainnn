unit Lider.CG.Com.TipsUnit;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms,
  Dialogs,
  Lider.CG.Com.Tips,
  Lider.CG.Com.Consts;

type
  { TGlyphStyle }
  TGlyphStyle = (gsLightBulb, gsClown, gsHelp, gsArrow, gsTorch, gsCustom);

  { TTips class }
  TTips = class(TComponent)
  private
    FCurrentTip: Integer;
    FShowOnStartup: Boolean;
    FTips: TStringList;
    FDialog: TfmTips;
    procedure SetCurrentTip(Value: Integer);
    procedure SetTips(Value: TStringList);
    //procedure DialogCheckBoxClick(Sender: TObject);
    procedure DialogNextButtonClick(Sender: TObject);
  protected
    procedure DoShowTip; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowTip; dynamic;
    property ShowOnStartup: boolean read FShowOnStartup;
  published
    property CurrentTip: Integer read FCurrentTip write SetCurrentTip;
    property Tips: TStringList read FTips write SetTips;
  end;

// tips
resourcestring
  STip1 = '<CTRL+Z> Geri Al komutunu çalýþtýrýr. ' + CrLf +
    '<ESC> tuþu ile Çoklu Doðru gibi çizim iþlemlerinde bir önceki adýma gidebilirsiniz.' +
    CrLf + '<ESC> tuþu ile Seçilen nesneleri temizleyebilirsiniz.';
  STip2 = '<Alt+S> Çizim vb. iþlemlerde Yakalama Modunu Aç/Kapat yapar. ' + CrLf
    + 'Ayrýca <CTRL>+<SHIFT> tuþlarý basýlý iken geçici olarak snap noktalarý gösterilmez.';
  STip3 = '<Alt+X> Ekraný Kaydýrýr. ';
  STip4 = '<Alt+Z> Pencere zoom komutunu çalýþtýrýr. ';
  STip5 = '<Alt+C> Bir önceki ekrana döner.';
  STip6 = 'Mouse Orta Tuþ basýlý iken ekran kaydýrma iþlemini yapar.';
  STip7 =
    'Nesne yeniden þekillendirme iþleminde <SHIFT> tuþu basýlý iken köþe noktalarýný siler.';
//  + CrLf + 'Ayrýca <Ctrl> + <Right Arrow>/<Left Arrow> tuþlarýyla köþe noktalarý arasýnda dolaþabilirsiniz. ';
  STip8 = 'Ýþlem adýmlarýný komut satýrýndan takip edebilirsiniz.';
  STip9 =
    'Sistem boþdayken komut satýrýna komut yazarak iþlemleri baþlatabilirsiniz.' + CrLf
    + 'Örneðin pline yazarsanýz çoklu doðru iþlemi baþlatýrsýnýz.' + CrLf +
    'Diðer komutlar için yardým menüsüne bakabilirsiniz.';
  STip10 =
    'Seçim iþlemlerinden sonra mouse sað tuþ ile gelen popup penceresinde çeþitli komutlar çalýþtýrabilirsiniz.';
  STip11 =
    'Yakalama Modu Aktif olan komutlarda F5 tuþu Yakalama Modunu açýp/kapatýr.' + CrLf
    + 'F4 tuþu ekranýn aþaðýsýna doðru gezinerek yakalama modlarýný deðiþtirir.'
    + CrLf + 'F6 tuþu ekranýn yukarýsýna doðru gezinerek yakalama modlarýný deðiþtirir'
    + CrLf + 'F7 tuþu Referans Proje Yakalama seçeneðini açýp/kapatýr.';






{  STip1 =
    'You can press <Shift> or <Ctrl> key during Move, Rotate, Scale, Mirror entities in order to Copy entities.';
  STip2 =
    'When you are drawing a polygon / polyline entity, you can press <Alt> key and click a point in order to paste the list of points to the Clipboard.';
  STip3 =
    'You can hold down <Ctrl> <Alt> keys in order to disable temporary the AccuDraw.';
  STip4 =
    'You can hold down <Shift> <Ctrl> keys in order to disable temporary the AccuSnap.';
  STip5 =
    'You can hold down <Shift> <Ctrl> <Alt> keys in order to disable temporary the AccuDraw && AccuSnap.';
  STip6 =
    'You can select/unselect entities if you hold down <Shift> key when picking on entities.';
  STip7 =
    'When reshaping, you can select more than one node if you hold down the <Ctrl> key and then enclose nodes in a box. Then you can move more than one node at same time.';
  STip8 = 'When selecting entities with the rectangle select, polygon select, circle select, etc. you de-select entities instead of'
    + CrLf +
    'selecting if you hold down the <Alt> key.';
  STip9 =
    'When you use the Zoom-In, Zoom-Out, Zoom-Window, Hand-Scroll on the preview box, if you hold down the <Ctrl> key, the map zoom is modified instead of the preview zoom.';
  STip10 =
    'When reshaping an entity, you right click and a popup menu is displayed for adding, deleting, editing nodes or change rotation origin.';
  STip11 = 'A polyline/polygon can have mixed line segments and arc segments.';
  STip12 =
    'With the TlicgActionTracker component, you can add your own custom actions to the map.';
  STip13 =
    'You can define any bitmap to a closed entity in order to fill the entity with that bitmap.';
  STip14 = 'You can create multiple snap points by passing the mouse over a vertex of an entity while holding down the <Shift> key.'
    + CrLf +
    'The vertex snap point can be deleted by passing again the mouse over it when you hold down the <Shift> key.';
  STip15 =
    'If you have exactly two vertices snapped on the list, then you can press <Ctrl> key in order to force to perpendicular direction to both vertices';
  STip16 =
    'If you have exactly two vertices snapped on the list, then you can press <Alt> key in order to force to parallel direction to both vertices';
  STip17 =
    'When reshaping, you use <Ctrl> + <Right Arrow>/<Left Arrow> to select next or previous vertex';
  STip18 =
    'When reshaping, you use <Right Arrow>/<Left Arrow>/<Up Arrow>/<Down Arrow> to move the selected vertex';

  STip50 = '...no more tips';
}

implementation

uses
  Inifiles;

{ TTips }

constructor TTips.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with TiniFile.create(ExtractFilePath(paramstr(0)) + 'Settings\Tips.ini') do
  begin
    try
      FShowOnStartup := ReadBool('Tips', 'Start', True);

    finally
      Free;
    end;
  end;

  { Initialize }
  FCurrentTip := -1;
  FTips := TStringList.Create;
  FDialog := nil;
end;

destructor TTips.Destroy;
begin
  FTips.Free;
  if FDialog <> nil then
    FDialog.Free;
  inherited Destroy;
end;

procedure TTips.ShowTip;
begin
  { Show the tip dialog by calling DoShowTip }
  DoShowTip;
end;

procedure TTips.DoShowTip;
begin
  if Assigned(FDialog) then
    FDialog.ShowModal
  else
  begin
    { If no other dialog was found, create a new one and show it }
    FDialog := TfmTips.Create(nil);
    FDialog.chkShow.Checked := FShowOnStartup;
    FDialog.btnNextTip.OnClick := DialogNextButtonClick;
    SetCurrentTip(FCurrentTip);
    FDialog.ShowModal;

  end;

end;

procedure TTips.SetCurrentTip(Value: Integer);
begin
  //if FCurrentTip <> Value then begin
  FCurrentTip := Value;
  if FCurrentTip > Pred(FTips.Count) then
    FCurrentTip := 0;

  if Assigned(FDialog) then
    if FCurrentTip > -1 then
      FDialog.TipLabel.Caption := FTips[FCurrentTip]
    else
      FDialog.TipLabel.Caption := '';
  //end;
end;

procedure TTips.SetTips(Value: TStringList);
begin
  if Assigned(Value) then
  begin
    FTips.Assign(Value);
    if FCurrentTip > Pred(FTips.Count) then
      FCurrentTip := Pred(FTips.Count);
  end;
end;

procedure TTips.DialogNextButtonClick(Sender: TObject);
begin
  { Increase FCurrentTip }
  CurrentTip := Succ(CurrentTip);
end;

end.


