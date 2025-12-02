unit Lider.CG.LiWork.MatematikIslemleri;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, cxContainer, cxEdit, dxLayoutControl,
  dxLayoutControlAdapters, dxLayoutContainer, dxLayoutcxEditAdapters, cxClasses,
  dxLayoutLookAndFeels, cxTextEdit, cxCurrencyEdit, cxButtons, cxGroupBox,
  cxRadioGroup;

type
  TfmMatematikIslemleriYeni = class(TForm)
    ceDeger: TcxCurrencyEdit;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel;
    lcMain: TdxLayoutControl;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    Image1: TImage;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnTamam: TdxLayoutItem;
    libtnIptal: TdxLayoutItem;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    gbBlok: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    lblCaption: TdxLayoutItem;
    lblDes: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutItem5: TdxLayoutItem;
    chkAlfasayisal: TdxLayoutCheckBoxItem;
    dxLayoutItem6: TdxLayoutItem;
    rgbIslemler: TcxRadioGroup;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMatematikIslemleriYeni: TfmMatematikIslemleriYeni;

implementation

{$R *.dfm}

end.
