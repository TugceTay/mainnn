unit Lider.CG.ModulesCom.ParalelIfrazParselAlaniGirisi;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Math, Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, Menus, StdCtrls, cxButtons,
  cxTextEdit, cxCurrencyEdit, cxLabel, dxSkinsCore,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.LicadInt;

type
  TfmParalelIfrazParselAlaniGirisi = class(TForm)
    cxLabel1: TcxLabel;
    ceParselAlani: TcxCurrencyEdit;
    cxLabel2: TcxLabel;
    ceCepheUzunlugu: TcxCurrencyEdit;
    cxLabel3: TcxLabel;
    btnTamam: TcxButton;
    cxButton1: TcxButton;
    ceIstenilenAlan: TcxCurrencyEdit;
  private
    { Private declarations }
  public
    class function Execute(ACaption: String; APoly: IlicgEntity; ANokta1: TlicgCoor; ANokta2: TlicgCoor; PAlan: Double): Double;
  end;

var
  fmParalelIfrazParselAlaniGirisi: TfmParalelIfrazParselAlaniGirisi;

implementation

{$R *.dfm}

class function TfmParalelIfrazParselAlaniGirisi.Execute(ACaption: String; APoly: IlicgEntity; ANokta1: TlicgCoor; ANokta2: TlicgCoor; PAlan: Double): Double;
var
  Alan, ParselAlani: Double;
begin
  Result := 0;
  with TfmParalelIfrazParselAlaniGirisi.Create(nil) do
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
    if (ceCepheUzunlugu.Value <=0) or (ParselAlani = 0) then
    begin
      Application.MessageBox('Deðer Sayýsý Hesaplanamadý. Lütfen Seçilen Alaný ve Endeks Alanlarýný kontrol ediniz.', 'Bilgi', MB_OK + MB_ICONINFORMATION);
      Exit;
    end;
    ceParselAlani.Value := RoundTo(ParselAlani,(-1*(Integer(Licad.Settings.Precision.Area))));//AlanYuvarla(DegerSayisi, -2);
    // ilker licad ceCepheUzunlugu.Value := NetcadMath.distance(ANokta1, ANokta2, False);
    ShowModal;
    if ModalResult = mrOK then
      Result := ceIstenilenAlan.Value
    else
      Result := 0;
  finally
    Free;
  end;
end;

end.
