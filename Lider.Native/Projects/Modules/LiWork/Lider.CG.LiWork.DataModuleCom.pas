unit Lider.CG.LiWork.DataModuleCom;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls,
  cxImageList, cxGraphics, frxClass, frxDBSet, Data.DB, dxmdaset,
  cxEditRepositoryItems, cxEdit, cxClasses;

type
  TdmCom = class(TDataModule)
    SmallImages: TcxImageList;
    LargeImages: TcxImageList;
    dxMemData1: TdxMemData;
    dxMemData1ADI: TStringField;
    dxMemData1SOYADI: TStringField;
    frxDBDataset1: TfrxDBDataset;
    EditRepository: TcxEditRepository;
    ceAci: TcxEditRepositoryCurrencyItem;
    ceAlan: TcxEditRepositoryCurrencyItem;
    ceKoordinat: TcxEditRepositoryCurrencyItem;
    ceKot: TcxEditRepositoryCurrencyItem;
    ceDebi: TcxEditRepositoryCurrencyItem;
    ceGenel: TcxEditRepositoryCurrencyItem;
    ceUzunluk: TcxEditRepositoryCurrencyItem;
    ceEgim: TcxEditRepositoryCurrencyItem;
    ceHiz: TcxEditRepositoryCurrencyItem;
    ceHacim: TcxEditRepositoryCurrencyItem;
    ceAgirlik: TcxEditRepositoryCurrencyItem;
    teAdi: TcxEditRepositoryTextItem;
    teAdiSoyadi: TcxEditRepositoryTextItem;
    teSoyadi: TcxEditRepositoryTextItem;
    ceButtonUzunluk: TcxEditRepositoryCurrencyItem;
  private
    { Private declarations }
  public
    procedure SettingPrecision;
    { Public declarations }
  end;

var
  dmCom: TdmCom = nil;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses lxStrUtil, Lider.CG.Com.LicadInt;

procedure TdmCom.SettingPrecision;
begin
  // Açý
  CurrencyItemFormat(dmCom.ceAci, Licad.Settings.Precision.Angle);
  // Aðýrlýk
  CurrencyItemFormat(dmCom.ceAgirlik, Licad.Settings.Precision.Weight);
  // Alan
  CurrencyItemFormat(dmCom.ceAlan, Licad.Settings.Precision.Area);
  // Debi
  CurrencyItemFormat(dmCom.ceDebi, Licad.Settings.Precision.Flow);
  // Eðim
  CurrencyItemFormat(dmCom.ceEgim, Licad.Settings.Precision.Slope);
  // Genel
  CurrencyItemFormat(dmCom.ceGenel, Licad.Settings.Precision.General);
  // Hacim
  CurrencyItemFormat(dmCom.ceHacim, Licad.Settings.Precision.Volume);
  // Hýz
  CurrencyItemFormat(dmCom.ceHiz, Licad.Settings.Precision.Velocity);
  // Koordinat
  CoorCurrencyItemFormat(dmCom.ceKoordinat, Licad.Settings.Precision.Coordinate, False); // Binlik Ayýraç Yok 19.02.2025
  // Kot
  CoorCurrencyItemFormat(dmCom.ceKot, Licad.Settings.Precision.Elevation, False); // Binlik Ayýraç Yok 19.02.2025
  // Uzunluk
  CurrencyItemFormat(dmCom.ceUzunluk, Licad.Settings.Precision.Length);
  // Uzunluk ölçülendirme
  CurrencyItemFormat(dmCom.ceButtonUzunluk, Licad.Settings.Precision.Length);
end;

initialization
  dmCom := TdmCom.Create(nil);

finalization
  dmCom.Free;

end.
