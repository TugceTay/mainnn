unit Lider.CG.Com.YeniKatmanOlustur;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Menus,
  StdCtrls,
  Controls,
  Forms,
  ExtCtrls,
  cxGraphics,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  dxSkinsCore,
  cxControls,
  cxContainer,
  cxEdit,
  cxTextEdit,
  cxLabel,
  cxButtons,
  Lider.CG.Com.GIS;

type
  TfmYeniKatmanOlustur = class(TForm)
    pnlAlt: TPanel;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    pnlOrta: TPanel;
    lblKatmanAdi: TcxLabel;
    teKatmanAdi: TcxTextEdit;
    pnlUst: TPanel;
    Image1: TImage;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    procedure btnTamamClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FGis: TlicgBaseGis;
  public
    { Public declarations }
    function Enter(Gis: TlicgBaseGis): Word;
  end;

implementation

{$R *.dfm}

procedure TfmYeniKatmanOlustur.btnTamamClick(Sender: TObject);
var
  fn: string;
  aa: string;
begin
  if Trim(teKatmanAdi.Text) = '' then
  begin
    ModalResult := mrNone;
    Application.MessageBox('Katman Adý Giriniz.', 'Bilgi', MB_OK + MB_ICONINFORMATION);
    teKatmanAdi.SetFocus;
    Exit;
  end;

  aa := teKatmanAdi.Text;
  if aa[1] in ['@'] then
  begin
    aa[1] := '#';
  end;
  teKatmanAdi.Text := aa;

  teKatmanAdi.Text := StringReplace(teKatmanAdi.Text, ' ', '_', [rfReplaceAll]);

  if Length(Trim(teKatmanAdi.Text)) = 0 then
  begin
    Application.MessageBox('Katman Adý Boþ. Katman Adý olmadan Katman Oluþturulamaz.',
      'Bilgi', MB_OK + MB_ICONINFORMATION);
    ModalResult := mrNone;
  end;
end;

function TfmYeniKatmanOlustur.Enter(Gis: TlicgBaseGis): Word;
begin
  FGis := Gis;
  Result := ShowModal;
end;

procedure TfmYeniKatmanOlustur.FormShow(Sender: TObject);
begin
  teKatmanAdi.SelStart := Length(teKatmanAdi.Text);
end;

end.


