unit Lider.CG.LiWork.HazirYaziAyarlari;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, cxContainer, cxEdit, dxLayoutControl,
  dxLayoutcxEditAdapters, dxLayoutControlAdapters, cxClasses, cxTextEdit,
  dxLayoutLookAndFeels, dxLayoutContainer, cxButtons, cxDropDownEdit, cxMaskEdit,
  cxSpinEdit;

type
  TfmHazirYaziAyarlari = class(TForm)
    IcMain: TdxLayoutControl;
    IcMainGroup_Root: TdxLayoutGroup;
    dxLayoutImageItem1: TdxLayoutImageItem;
    dxLayoutLabeledItem1: TdxLayoutLabeledItem;
    dxLayoutLabeledItem2: TdxLayoutLabeledItem;
    gbxYazýAyar: TdxLayoutGroup;
    gbxYaziSecenek: TdxLayoutGroup;
    gbxHarfSecenek: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutCheckBoxItem1: TdxLayoutCheckBoxItem;
    dxLayoutCheckBoxItem2: TdxLayoutCheckBoxItem;
    dxLayoutCheckBoxItem3: TdxLayoutCheckBoxItem;
    dxLayoutCheckBoxItem4: TdxLayoutCheckBoxItem;
    dxLayoutCheckBoxItem5: TdxLayoutCheckBoxItem;
    dxLayoutCheckBoxItem6: TdxLayoutCheckBoxItem;
    dxLayoutCheckBoxItem7: TdxLayoutCheckBoxItem;
    rbHarflerAyni: TdxLayoutRadioButtonItem;
    rbIlkHarfBuyuk: TdxLayoutRadioButtonItem;
    rbHarflerBuyuk: TdxLayoutRadioButtonItem;
    rbHarflerKucuk: TdxLayoutRadioButtonItem;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    seBosluk: TcxSpinEdit;
    dxLayoutItem1: TdxLayoutItem;
    seParcaAdet: TcxSpinEdit;
    dxLayoutItem2: TdxLayoutItem;
    seSatirAraligi: TcxSpinEdit;
    dxLayoutItem3: TdxLayoutItem;
    seAci: TcxSpinEdit;
    dxLayoutItem4: TdxLayoutItem;
    seYBoyut: TcxSpinEdit;
    dxLayoutItem5: TdxLayoutItem;
    cbYaziFont: TcxComboBox;
    dxLayoutItem6: TdxLayoutItem;
    btnTamam: TcxButton;
    dxLayoutItem7: TdxLayoutItem;
    btnIptal: TcxButton;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmHazirYaziAyarlari: TfmHazirYaziAyarlari;

implementation

uses
  Lider.CG.Com.LicadInt;

{$R *.dfm}

procedure TfmHazirYaziAyarlari.FormShow(Sender: TObject);
begin
  cbYaziFont.Properties.Items.AddStrings(Licad.GetLicadFonts);
end;


end.
