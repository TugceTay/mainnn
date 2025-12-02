unit Lider.CG.LiWork.HizliYaziDüzenle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, ImageList, ImgList, dxSkinsCore,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxControls, cxContainer,
  cxEdit, cxImageList, cxLabel, dxGDIPlusClasses, dxListView, cxCheckBox,
  cxButtons, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutControl,
  cxClasses, dxLayoutLookAndFeels;

type
  TfmHizliYaziDüzenle = class(TForm)
    ImageList: TcxImageList;
    lvcHizliYaziDuzenleAraclari: TdxListViewControl;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lcMain: TdxLayoutControl;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    Image1: TImage;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnTamam: TdxLayoutItem;
    libtnIptal: TdxLayoutItem;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    gbBlok: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    lblCaption: TdxLayoutItem;
    lblDes: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutItem1: TdxLayoutItem;
    chkCokluNesneSecim: TdxLayoutCheckBoxItem;
    procedure lvcHizliYaziDuzenleAraclariSelectItem(Sender: TdxCustomListView;
      AItem: TdxListItem; ASelected: Boolean);
    procedure lvcHizliYaziDuzenleAraclariDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  const FCaptions : array  [0..7] of string = (
    'Seçilen Metnin Tüm Harflerini Büyük Yapar.',
    'Seçilen Metnin Tüm Harflerini Küçük Yapar.',
    'Seçilen Metnin Ýlk Harflerini Büyük Yapar.',
    'Seçilen Metnin Harfleri Arasýna Boþluk Ekler.',
    'Seçilen Metni Girilen Sayý Kadar Parçalar.',
    'Hazýr Yazý Oluþturur ve Düzenleme Yapar.',
    'Seçilen Sayýyýnýn Ondalýklý Kýsmýný Yuvarlar.',
    'Seçilen Nesne Üzeinde Matematiksel Ýþlemler Yapar.');
  end;

var
  fmHizliYaziDüzenle: TfmHizliYaziDüzenle;

implementation

{$R *.dfm}

procedure TfmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclariDblClick(
  Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclariSelectItem(
  Sender: TdxCustomListView; AItem: TdxListItem; ASelected: Boolean);
var
  ABitmap: TIcon;
begin
  if ASelected then
  begin
    ABitmap := TIcon.Create;
    try
      lblCaption.Caption := '[B]' + AItem.Caption + '[/B]';
      lblDes.Caption := FCaptions[AItem.index];
      ImageList.GetIcon(AItem.ImageIndex, ABitmap);
      Image1.Picture.Assign(ABitmap);
    finally
      ABitmap.Free;
    end;
  end
  else
  begin
    lblCaption.Caption := '[B]Hýzlý Yazý Düzenle[/B]';
    lblDes.Caption := 'Hýzlý Yazý Düzenle Araçlarýný buradan seçebilirsiniz.';
  end;
end;

end.
