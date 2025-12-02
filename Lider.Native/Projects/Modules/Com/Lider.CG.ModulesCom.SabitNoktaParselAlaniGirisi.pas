unit Lider.CG.ModulesCom.SabitNoktaParselAlaniGirisi;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Math, Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, Menus, StdCtrls, cxButtons, cxTextEdit, cxCurrencyEdit,
  cxLabel, dxSkinsCore,
  Lider.CG.COM.EntityInt,
  Lider.CG.COM.GeoTypes,
  Lider.CG.COM.GeoLibrary,
  Lider.CG.Com.LicadInt;

type
  TfmSabitNoktaParselAlaniGirisi = class(TForm)
    cxLabel1: TcxLabel;
    ceParselAlani: TcxCurrencyEdit;
    cxLabel2: TcxLabel;
    ceCepheUzunlugu: TcxCurrencyEdit;
    cxLabel3: TcxLabel;
    btnTamam: TcxButton;
    cxButton1: TcxButton;
    ceIstenilenAlan: TcxCurrencyEdit;
    cxLabel4: TcxLabel;
    ceCephe: TcxCurrencyEdit;
  private
    { Private declarations }
  public
    class function Execute(ACaption: String; APoly: IlicgEntity; ANokta1: TlicgCoor; ANokta2: TlicgCoor; out AAci: Double; PAlan: Double): Double;
  end;

var
  fmSabitNoktaParselAlaniGirisi: TfmSabitNoktaParselAlaniGirisi;

implementation

{$R *.dfm}

class function TfmSabitNoktaParselAlaniGirisi.Execute(ACaption: String; APoly: IlicgEntity; ANokta1: TlicgCoor; ANokta2: TlicgCoor; out AAci: Double; PAlan: Double): Double;
var
  Alan, ParselAlani: Double;
begin
  Result := 0;
  with TfmSabitNoktaParselAlaniGirisi.Create(nil) do
  try
    Caption := ACaption;
    ceCepheUzunlugu.Value := _Distance(ANokta1, ANokta2);
    ParselAlani := Abs(APoly.Geometry.Points.Area);

    //PAlan -1 ise endekssizdir -1 den büyükse endekslidir
    if PAlan > -1 then
    begin
      cxLabel1.Caption := 'Parsel Deðer Sayýsý:';
      cxLabel3.Caption := 'Ýstenilen Deðer Sayýsý:';
      ParselAlani := PAlan;
    end;
//    if not ComplexEndeksHesapla(PolyAddObje(APoly), EndeksTabakasi, Alan, Endeks, DegerSayisi) then
    if (ParselAlani = 0) then
    begin
      Application.MessageBox('Deðer Sayýsý Hesaplanamadý. Lütfen Seçilen Alaný ve Endeks Alanlarýný kontrol ediniz.', 'Bilgi', MB_OK + MB_ICONINFORMATION);
      Exit;
    end;
    ceParselAlani.Value := RoundTo(ParselAlani,(-1*(Integer(Licad.Settings.Precision.Area))));//AlanYuvarla(DegerSayisi, -2);
    // ilker silme licad ceCepheUzunlugu.Value := NetcadMath.distance(ANokta1, ANokta2, False);
    ShowModal;
    if ModalResult = mrOK then
    begin
      AAci := ceCephe.Value;
      Result := ceIstenilenAlan.Value;
    end
    else
      Result := 0;
  finally
    Free;
  end;
end;

end.
