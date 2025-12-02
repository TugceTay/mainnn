unit Lider.CG.ModulesCom.ProjeBilgileri;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Inifiles, Menus, StdCtrls, ExtCtrls, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  dxBarBuiltInMenu, dxmdaset, cxEditRepositoryItems, cxClasses, cxButtons,
  cxDropDownEdit, cxImageComboBox, cxDBEdit, cxTextEdit, cxMaskEdit, cxCalendar,
  cxPC, cxLabel;

type
  TfmProjeBilgileri = class(TForm)
    pnlUst: TPanel;
    Image1: TImage;
    cxLabel5: TcxLabel;
    cxLabel6: TcxLabel;
    pcOrta: TcxPageControl;
    tsGenel: TcxTabSheet;
    lblIl: TcxLabel;
    lblIlce: TcxLabel;
    lblMahalle: TcxLabel;
    lbMahalleKoy: TcxLabel;
    cxLabel3: TcxLabel;
    cxLabel26: TcxLabel;
    lbcxurumAdi: TcxLabel;
    lblMuhendis: TcxLabel;
    lblKontrolMemuru: TcxLabel;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxLabel4: TcxLabel;
    cxLabel11: TcxLabel;
    cxLabel12: TcxLabel;
    cxLabel14: TcxLabel;
    cxLabel15: TcxLabel;
    cxLabel16: TcxLabel;
    cxLabel17: TcxLabel;
    cxLabel18: TcxLabel;
    cxLabel21: TcxLabel;
    cxLabel22: TcxLabel;
    cxLabel23: TcxLabel;
    cxLabel10: TcxLabel;
    cxLabel13: TcxLabel;
    cxLabel27: TcxLabel;
    deKadastroMuduruTarih: TcxDBDateEdit;
    deImarMuduruTarih: TcxDBDateEdit;
    deKurumOnaylayanTarih: TcxDBDateEdit;
    deKurumKontrolEdenTarih: TcxDBDateEdit;
    deAplikasyonYapan2Tarih: TcxDBDateEdit;
    deAplikasyonYapan1Tarih: TcxDBDateEdit;
    deDuzenleyenTarih: TcxDBDateEdit;
    deBasvuranKisiTarih: TcxDBDateEdit;
    deBilirKisi3Tarih: TcxDBDateEdit;
    deBilirKisi2Tarih: TcxDBDateEdit;
    deBilirKisi1Tarih: TcxDBDateEdit;
    deMuhtarTarih: TcxDBDateEdit;
    deKadastroTek2Tarih: TcxDBDateEdit;
    deKadastroTek1Tarih: TcxDBDateEdit;
    deKontrolMemuruTarih: TcxDBDateEdit;
    deKontrolMuhendisiTarih: TcxDBDateEdit;
    teImarMuduru: TcxDBTextEdit;
    teKurumOnaylayan: TcxDBTextEdit;
    teKurumKontrolEden: TcxDBTextEdit;
    teAplikasyonYapan2: TcxDBTextEdit;
    teAplikasyonYapan1: TcxDBTextEdit;
    teDuzenleyen: TcxDBTextEdit;
    teBasvuranKisi: TcxDBTextEdit;
    teBilirKisi3: TcxDBTextEdit;
    teBilirKisi2: TcxDBTextEdit;
    teBilirKisi1: TcxDBTextEdit;
    teMuhtar: TcxDBTextEdit;
    teKadastroTek2: TcxDBTextEdit;
    teKadastroTek1: TcxDBTextEdit;
    teKontrolMemuru: TcxDBTextEdit;
    teKontrolMuhendisi: TcxDBTextEdit;
    teKadastroMuduru: TcxDBTextEdit;
    teKadastroMudurlugu: TcxDBTextEdit;
    teBelediye: TcxDBTextEdit;
    teFirma: TcxDBTextEdit;
    icbMAHALLEKOYTURU: TcxDBImageComboBox;
    teMAHALLEKOYADI: TcxDBTextEdit;
    teILCE: TcxDBTextEdit;
    teIL: TcxDBTextEdit;
    cxLabel35: TcxLabel;
    teProjeAdi: TcxDBTextEdit;
    tsLihkab: TcxTabSheet;
    cxLabel8: TcxLabel;
    cxLabel9: TcxLabel;
    cxLabel20: TcxLabel;
    cxLabel24: TcxLabel;
    cxLabel25: TcxLabel;
    cxLabel28: TcxLabel;
    cxLabel19: TcxLabel;
    teLihkabFirmaAdi: TcxDBTextEdit;
    deLihkabTek2Tarih: TcxDBDateEdit;
    deLihkabTek1Tarih: TcxDBDateEdit;
    deLihkabMuhendisTarih: TcxDBDateEdit;
    deLihkabLisansliMuhendisTarih: TcxDBDateEdit;
    teLihkabTek2: TcxDBTextEdit;
    teLihkabTek1: TcxDBTextEdit;
    teLihkabMuhendis: TcxDBTextEdit;
    teLihkabLisansliMuhendis: TcxDBTextEdit;
    tsDiger: TcxTabSheet;
    cxLabel7: TcxLabel;
    cxLabel29: TcxLabel;
    cxLabel30: TcxLabel;
    cxLabel31: TcxLabel;
    cxLabel32: TcxLabel;
    cxLabel33: TcxLabel;
    cxLabel34: TcxLabel;
    teFenKayitNo: TcxDBTextEdit;
    deFenKayitTarihi: TcxDBDateEdit;
    teMakbuzNo: TcxDBTextEdit;
    teMakbuzTarihi: TcxDBDateEdit;
    teEncumenNo: TcxDBTextEdit;
    deEncumenTarihi: TcxDBDateEdit;
    teEPOK: TcxDBTextEdit;
    pnlAlt: TPanel;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    EditRepository: TcxEditRepository;
    teAdi: TcxEditRepositoryTextItem;
    teAdiSoyadi: TcxEditRepositoryTextItem;
    teSoyadi: TcxEditRepositoryTextItem;
    dsProjeBilgileri: TDataSource;
    tbmProjeBilgileri: TdxMemData;
    tbmProjeBilgileriIl: TStringField;
    tbmProjeBilgileriIlce: TStringField;
    tbmProjeBilgileriMahalleKoy: TStringField;
    tbmProjeBilgileriMahalleKoyTipi: TIntegerField;
    tbmProjeBilgileriProjeAdi: TStringField;
    tbmProjeBilgileriFirma: TStringField;
    tbmProjeBilgileriBelediye: TStringField;
    tbmProjeBilgileriKadastroMudurlugu: TStringField;
    tbmProjeBilgileriEPOK: TIntegerField;
    tbmProjeBilgileriKadastroMuduru: TStringField;
    tbmProjeBilgileriKontrolMuhendisi: TStringField;
    tbmProjeBilgileriKontrolMemuru: TStringField;
    tbmProjeBilgileriKadastroTek1: TStringField;
    tbmProjeBilgileriKadastroTek2: TStringField;
    tbmProjeBilgileriMuhtar: TStringField;
    tbmProjeBilgileriBilirkisi1: TStringField;
    tbmProjeBilgileriBilirkisi2: TStringField;
    tbmProjeBilgileriBilirkisi3: TStringField;
    tbmProjeBilgileriBasvuranKisi: TStringField;
    tbmProjeBilgileriDuzenleyen: TStringField;
    tbmProjeBilgileriAplikasyonYapan1: TStringField;
    tbmProjeBilgileriAplikasyonYapan2: TStringField;
    tbmProjeBilgileriKurumKontrolEden: TStringField;
    tbmProjeBilgileriKurumOnaylayan: TStringField;
    tbmProjeBilgileriImarMuduru: TStringField;
    tbmProjeBilgileriKadastroMuduruTarih: TDateField;
    tbmProjeBilgileriKontrolMuhendisiTarih: TDateField;
    tbmProjeBilgileriKontrolMemuruTarih: TDateField;
    tbmProjeBilgileriKadastroTek1Tarih: TDateField;
    tbmProjeBilgileriKadastroTek2Tarih: TDateField;
    tbmProjeBilgileriMuhtarTarih: TDateField;
    tbmProjeBilgileriBilirkisi1Tarih: TDateField;
    tbmProjeBilgileriBilirkisi2Tarih: TDateField;
    tbmProjeBilgileriBilirkisi3Tarih: TDateField;
    tbmProjeBilgileriBasvuranKisiTarih: TDateField;
    tbmProjeBilgileriDuzenleyenTarih: TDateField;
    tbmProjeBilgileriAplikasyonYapan1Tarih: TDateField;
    tbmProjeBilgileriAplikasyonYapan2Tarih: TDateField;
    tbmProjeBilgileriKurumKontrolEdenTarih: TDateField;
    tbmProjeBilgileriKurumOnaylayanTarih: TDateField;
    tbmProjeBilgileriImarMuduruTarih: TDateField;
    tbmProjeBilgileriLihkabFirmaAdi: TStringField;
    tbmProjeBilgileriLihkabLisansliMuhendis: TStringField;
    tbmProjeBilgileriLihkabMuhendis: TStringField;
    tbmProjeBilgileriLihkabTek1: TStringField;
    tbmProjeBilgileriLihkabTek2: TStringField;
    tbmProjeBilgileriLihkabLisansliMuhendisTarih: TDateField;
    tbmProjeBilgileriLihkabMuhendisTarih: TDateField;
    tbmProjeBilgileriLihkabTek1Tarih: TStringField;
    tbmProjeBilgileriLihkabTek2Tarih: TStringField;
    tbmProjeBilgileriFenKayitNo: TStringField;
    tbmProjeBilgileriFenKayitTarihi: TDateField;
    tbmProjeBilgileriMakbuzNo: TStringField;
    tbmProjeBilgileriMakbuzTarihi: TDateField;
    tbmProjeBilgileriEncumenNo: TStringField;
    tbmProjeBilgileriEncumenTarihi: TDateField;
    procedure FormShow(Sender: TObject);
    procedure btnTamamClick(Sender: TObject);
    procedure btnIptalClick(Sender: TObject);
    procedure tbmProjeBilgileriAfterPost(DataSet: TDataSet);
  private
    procedure ProjeBilgileriniGetir;
    { Private declarations }
  public
    { Public declarations }
    constructor Execute;
  end;

var
  fmProjeBilgileri: TfmProjeBilgileri;

implementation

{$R *.dfm}

uses Lider.CG.Com.GIS, Lider.CG.Com.LicadInt;

constructor TfmProjeBilgileri.Execute;
begin
  fmProjeBilgileri := TfmProjeBilgileri.Create(nil);
  with fmProjeBilgileri do
  try
    ShowModal;
  finally
    FreeAndNil(fmProjeBilgileri);
  end;
end;

procedure TfmProjeBilgileri.ProjeBilgileriniGetir;
var
  IniFile: Tinifile;
begin
  tbmProjeBilgileri.Close;
  tbmProjeBilgileri.Open;
  tbmProjeBilgileri.Append;
  IniFile := TIniFile.Create(ChangeFileExt(CurrCmdLine.ActiveDrawBox.GIS.FileName, EXT_MAP));
  with IniFile do
  try
    tbmProjeBilgileriIl.AsString := ReadString('ProjectInformation', 'Il', '');
    tbmProjeBilgileriIlce.AsString := ReadString('ProjectInformation', 'Ilce', '');
    tbmProjeBilgileriMahalleKoy.AsString := ReadString('ProjectInformation', 'MahalleKoy', '');
    tbmProjeBilgileriMahalleKoyTipi.AsInteger := ReadInteger('ProjectInformation', 'MahalleKoyTipi', 1);
    tbmProjeBilgileriProjeAdi.AsString := ReadString('ProjectInformation', 'ProjeAdi', '');
    tbmProjeBilgileriFirma.AsString := ReadString('ProjectInformation', 'Firma', '');
    tbmProjeBilgileriBelediye.AsString := ReadString('ProjectInformation', 'Belediye', '');
    tbmProjeBilgileriKadastroMudurlugu.AsString := ReadString('ProjectInformation', 'KadastroMudurlugu', '');
    tbmProjeBilgileriKadastroMuduru.AsString := ReadString('ProjectInformation', 'KadastroMuduru', '');
    tbmProjeBilgileriKontrolMuhendisi.AsString := ReadString('ProjectInformation', 'KontrolMuhendisi', '');
    tbmProjeBilgileriKontrolMemuru.AsString := ReadString('ProjectInformation', 'KontrolMemuru', '');
    tbmProjeBilgileriKadastroTek1.AsString := ReadString('ProjectInformation', 'KadastroTek1', '');
    tbmProjeBilgileriKadastroTek2.AsString := ReadString('ProjectInformation', 'KadastroTek2', '');
    tbmProjeBilgileriMuhtar.AsString := ReadString('ProjectInformation', 'Muhtar', '');
    tbmProjeBilgileriBilirkisi1.AsString := ReadString('ProjectInformation', 'Bilirkisi1', '');
    tbmProjeBilgileriBilirkisi2.AsString := ReadString('ProjectInformation', 'Bilirkisi2', '');
    tbmProjeBilgileriBilirkisi3.AsString := ReadString('ProjectInformation', 'Bilirkisi3', '');
    tbmProjeBilgileriBilirkisi3.AsString := ReadString('ProjectInformation', 'Bilirkisi3', '');
    tbmProjeBilgileriBasvuranKisi.AsString := ReadString('ProjectInformation', 'BasvuranKisi', '');
    tbmProjeBilgileriDuzenleyen.AsString := ReadString('ProjectInformation', 'Duzenleyen', '');
    tbmProjeBilgileriAplikasyonYapan1.AsString := ReadString('ProjectInformation', 'AplikasyonYapan1', '');
    tbmProjeBilgileriAplikasyonYapan2.AsString := ReadString('ProjectInformation', 'AplikasyonYapan2', '');
    tbmProjeBilgileriKurumKontrolEden.AsString := ReadString('ProjectInformation', 'KurumKontrolEden', '');
    tbmProjeBilgileriKurumOnaylayan.AsString := ReadString('ProjectInformation', 'KurumOnaylayan', '');
    tbmProjeBilgileriImarMuduru.AsString := ReadString('ProjectInformation', 'ImarMuduru', '');

    tbmProjeBilgileriKadastroMuduruTarih.AsString := ReadString('ProjectInformation', 'KadastroMuduruTarih', '');
    tbmProjeBilgileriKontrolMuhendisiTarih.AsString := ReadString('ProjectInformation', 'KontrolMuhendisiTarih', '');
    tbmProjeBilgileriKontrolMemuruTarih.AsString := ReadString('ProjectInformation', 'KontrolMemuruTarih', '');
    tbmProjeBilgileriKadastroTek1Tarih.AsString := ReadString('ProjectInformation', 'KadastroTek1Tarih', '');
    tbmProjeBilgileriKadastroTek2Tarih.AsString := ReadString('ProjectInformation', 'KadastroTek2Tarih', '');
    tbmProjeBilgileriMuhtarTarih.AsString := ReadString('ProjectInformation', 'MuhtarTarih', '');
    tbmProjeBilgileriBilirkisi1Tarih.AsString := ReadString('ProjectInformation', 'Bilirkisi1Tarih', '');
    tbmProjeBilgileriBilirkisi2Tarih.AsString := ReadString('ProjectInformation', 'Bilirkisi2Tarih', '');
    tbmProjeBilgileriBilirkisi3Tarih.AsString := ReadString('ProjectInformation', 'Bilirkisi3Tarih', '');
    tbmProjeBilgileriBasvuranKisiTarih.AsString := ReadString('ProjectInformation', 'BasvuranKisiTarih', '');
    tbmProjeBilgileriDuzenleyenTarih.AsString := ReadString('ProjectInformation', 'DuzenleyenTarih', '');
    tbmProjeBilgileriAplikasyonYapan1Tarih.AsString := ReadString('ProjectInformation', 'AplikasyonYapan1Tarih', '');
    tbmProjeBilgileriAplikasyonYapan2Tarih.AsString := ReadString('ProjectInformation', 'AplikasyonYapan2Tarih', '');
    tbmProjeBilgileriKurumKontrolEdenTarih.AsString := ReadString('ProjectInformation', 'KurumKontrolEdenTarih', '');
    tbmProjeBilgileriKurumOnaylayanTarih.AsString := ReadString('ProjectInformation', 'KurumOnaylayanTarih', '');
    tbmProjeBilgileriImarMuduruTarih.AsString := ReadString('ProjectInformation', 'ImarMuduruTarih', '');

    tbmProjeBilgileriLihkabFirmaAdi.AsString := ReadString('ProjectInformation', 'LihkabFirmaAdi', '');
    tbmProjeBilgileriLihkabLisansliMuhendis.AsString := ReadString('ProjectInformation', 'LihkabLisansliMuhendis', '');
    tbmProjeBilgileriLihkabMuhendis.AsString := ReadString('ProjectInformation', 'LihkabMuhendis', '');
    tbmProjeBilgileriLihkabTek1.AsString := ReadString('ProjectInformation', 'LihkabTek1', '');
    tbmProjeBilgileriLihkabTek2.AsString := ReadString('ProjectInformation', 'LihkabTek2', '');
    tbmProjeBilgileriLihkabLisansliMuhendisTarih.AsString := ReadString('ProjectInformation', 'LihkabLisansliMuhendisTarih', '');
    tbmProjeBilgileriLihkabMuhendisTarih.AsString := ReadString('ProjectInformation', 'LihkabMuhendisTarih', '');
    tbmProjeBilgileriLihkabTek1Tarih.AsString := ReadString('ProjectInformation', 'LihkabTek1Tarih', '');
    tbmProjeBilgileriLihkabTek2Tarih.AsString := ReadString('ProjectInformation', 'LihkabTek2Tarih', '');

    tbmProjeBilgileriFenKayitNo.AsString := ReadString('ProjectInformation', 'FenKayitNo', '');
    tbmProjeBilgileriFenKayitTarihi.AsString := ReadString('ProjectInformation', 'FenKayitTarihi', '');
    tbmProjeBilgileriMakbuzNo.AsString := ReadString('ProjectInformation', 'MakbuzNo', '');
    tbmProjeBilgileriMakbuzTarihi.AsString := ReadString('ProjectInformation', 'MakbuzTarihi', '');
    tbmProjeBilgileriEncumenNo.AsString := ReadString('ProjectInformation', 'EncumenNo', '');
    tbmProjeBilgileriEncumenTarihi.AsString := ReadString('ProjectInformation', 'EncumenTarihi', '');

    tbmProjeBilgileriEPOK.AsInteger := ReadInteger('ProjectInformation', 'EPOK', 2005);
    tbmProjeBilgileri.Post;
    CurrCmdLine.ActiveDrawBox.GIS.Modified := True;
    CurrCmdLine.ActiveDrawBox.GIS.Layers[0].Modified := True;
  finally
    Free;
  end;
end;

procedure TfmProjeBilgileri.tbmProjeBilgileriAfterPost(DataSet: TDataSet);
var
  IniFile: Tinifile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(CurrCmdLine.ActiveDrawBox.GIS.FileName, EXT_MAP));
  with IniFile do
  try
    WriteString('ProjectInformation', 'Il', tbmProjeBilgileriIl.AsString);
    WriteString('ProjectInformation', 'Ilce', tbmProjeBilgileriIlce.AsString);
    WriteString('ProjectInformation', 'MahalleKoy', tbmProjeBilgileriMahalleKoy.AsString);
    WriteInteger('ProjectInformation', 'MahalleKoyTipi', tbmProjeBilgileriMahalleKoyTipi.AsInteger);
    WriteString('ProjectInformation', 'ProjeAdi', tbmProjeBilgileriProjeAdi.AsString);
    WriteString('ProjectInformation', 'Firma', tbmProjeBilgileriFirma.AsString);
    WriteString('ProjectInformation', 'Belediye', tbmProjeBilgileriBelediye.AsString);
    WriteString('ProjectInformation', 'KadastroMudurlugu', tbmProjeBilgileriKadastroMudurlugu.AsString);
    WriteString('ProjectInformation', 'KadastroMuduru', tbmProjeBilgileriKadastroMuduru.AsString);
    WriteString('ProjectInformation', 'KontrolMuhendisi', tbmProjeBilgileriKontrolMuhendisi.AsString);
    WriteString('ProjectInformation', 'KontrolMemuru', tbmProjeBilgileriKontrolMemuru.AsString);
    WriteString('ProjectInformation', 'KadastroTek1', tbmProjeBilgileriKadastroTek1.AsString);
    WriteString('ProjectInformation', 'KadastroTek2', tbmProjeBilgileriKadastroTek2.AsString);
    WriteString('ProjectInformation', 'Muhtar', tbmProjeBilgileriMuhtar.AsString);
    WriteString('ProjectInformation', 'Bilirkisi1', tbmProjeBilgileriBilirkisi1.AsString);
    WriteString('ProjectInformation', 'Bilirkisi2', tbmProjeBilgileriBilirkisi2.AsString);
    WriteString('ProjectInformation', 'Bilirkisi3', tbmProjeBilgileriBilirkisi3.AsString);
    WriteString('ProjectInformation', 'Bilirkisi3', tbmProjeBilgileriBilirkisi3.AsString);
    WriteString('ProjectInformation', 'BasvuranKisi', tbmProjeBilgileriBasvuranKisi.AsString);
    WriteString('ProjectInformation', 'Duzenleyen', tbmProjeBilgileriDuzenleyen.AsString);
    WriteString('ProjectInformation', 'AplikasyonYapan1', tbmProjeBilgileriAplikasyonYapan1.AsString);
    WriteString('ProjectInformation', 'AplikasyonYapan2', tbmProjeBilgileriAplikasyonYapan2.AsString);
    WriteString('ProjectInformation', 'KurumKontrolEden', tbmProjeBilgileriKurumKontrolEden.AsString);
    WriteString('ProjectInformation', 'KurumOnaylayan', tbmProjeBilgileriKurumOnaylayan.AsString);
    WriteString('ProjectInformation', 'ImarMuduru', tbmProjeBilgileriImarMuduru.AsString);

    WriteString('ProjectInformation', 'KadastroMuduruTarih', tbmProjeBilgileriKadastroMuduruTarih.AsString);
    WriteString('ProjectInformation', 'KontrolMuhendisiTarih', tbmProjeBilgileriKontrolMuhendisiTarih.AsString);
    WriteString('ProjectInformation', 'KontrolMemuruTarih', tbmProjeBilgileriKontrolMemuruTarih.AsString);
    WriteString('ProjectInformation', 'KadastroTek1Tarih', tbmProjeBilgileriKadastroTek1Tarih.AsString);
    WriteString('ProjectInformation', 'KadastroTek2Tarih', tbmProjeBilgileriKadastroTek2Tarih.AsString);
    WriteString('ProjectInformation', 'MuhtarTarih', tbmProjeBilgileriMuhtarTarih.AsString);
    WriteString('ProjectInformation', 'Bilirkisi1Tarih', tbmProjeBilgileriBilirkisi1Tarih.AsString);
    WriteString('ProjectInformation', 'Bilirkisi2Tarih', tbmProjeBilgileriBilirkisi2Tarih.AsString);
    WriteString('ProjectInformation', 'Bilirkisi3Tarih', tbmProjeBilgileriBilirkisi3Tarih.AsString);
    WriteString('ProjectInformation', 'BasvuranKisiTarih', tbmProjeBilgileriBasvuranKisiTarih.AsString);
    WriteString('ProjectInformation', 'DuzenleyenTarih', tbmProjeBilgileriDuzenleyenTarih.AsString);
    WriteString('ProjectInformation', 'AplikasyonYapan1Tarih', tbmProjeBilgileriAplikasyonYapan1Tarih.AsString);
    WriteString('ProjectInformation', 'AplikasyonYapan2Tarih', tbmProjeBilgileriAplikasyonYapan2Tarih.AsString);
    WriteString('ProjectInformation', 'KurumKontrolEdenTarih', tbmProjeBilgileriKurumKontrolEdenTarih.AsString);
    WriteString('ProjectInformation', 'KurumOnaylayanTarih', tbmProjeBilgileriKurumOnaylayanTarih.AsString);
    WriteString('ProjectInformation', 'ImarMuduruTarih', tbmProjeBilgileriImarMuduruTarih.AsString);

    WriteString('ProjectInformation', 'LihkabFirmaAdi', tbmProjeBilgileriLihkabFirmaAdi.AsString);
    WriteString('ProjectInformation', 'LihkabLisansliMuhendis', tbmProjeBilgileriLihkabLisansliMuhendis.AsString);
    WriteString('ProjectInformation', 'LihkabMuhendis', tbmProjeBilgileriLihkabMuhendis.AsString);
    WriteString('ProjectInformation', 'LihkabTek1', tbmProjeBilgileriLihkabTek1.AsString);
    WriteString('ProjectInformation', 'LihkabTek2', tbmProjeBilgileriLihkabTek2.AsString);
    WriteString('ProjectInformation', 'LihkabLisansliMuhendisTarih', tbmProjeBilgileriLihkabLisansliMuhendisTarih.AsString);
    WriteString('ProjectInformation', 'LihkabMuhendisTarih', tbmProjeBilgileriLihkabMuhendisTarih.AsString);
    WriteString('ProjectInformation', 'LihkabTek1Tarih', tbmProjeBilgileriLihkabTek1Tarih.AsString);
    WriteString('ProjectInformation', 'LihkabTek2Tarih', tbmProjeBilgileriLihkabTek2Tarih.AsString);

    WriteString('ProjectInformation', 'FenKayitNo', tbmProjeBilgileriFenKayitNo.AsString);
    WriteString('ProjectInformation', 'FenKayitTarihi', tbmProjeBilgileriFenKayitTarihi.AsString);
    WriteString('ProjectInformation', 'MakbuzNo', tbmProjeBilgileriMakbuzNo.AsString);
    WriteString('ProjectInformation', 'MakbuzTarihi', tbmProjeBilgileriMakbuzTarihi.AsString);
    WriteString('ProjectInformation', 'EncumenNo', tbmProjeBilgileriEncumenNo.AsString);
    WriteString('ProjectInformation', 'EncumenTarihi', tbmProjeBilgileriEncumenTarihi.AsString);

    WriteInteger('ProjectInformation', 'EPOK', tbmProjeBilgileriEPOK.AsInteger);
  finally
    Free;
  end;
end;

procedure TfmProjeBilgileri.FormShow(Sender: TObject);
begin
  tsGenel.Show;
  teIL.SetFocus;
  ProjeBilgileriniGetir;
end;

procedure TfmProjeBilgileri.btnTamamClick(Sender: TObject);
begin
  if tbmProjeBilgileri.State in [dsInsert, dsEdit] then
    tbmProjeBilgileri.Post;
  ModalResult := mrOK;
end;

procedure TfmProjeBilgileri.btnIptalClick(Sender: TObject);
begin
  if tbmProjeBilgileri.State in [dsInsert, dsEdit] then
    tbmProjeBilgileri.Cancel;
  ModalResult := mrCancel;
end;

end.

