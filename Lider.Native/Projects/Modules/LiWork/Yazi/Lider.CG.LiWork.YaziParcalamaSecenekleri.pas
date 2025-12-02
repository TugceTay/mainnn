unit Lider.CG.LiWork.YaziParcalamaSecenekleri;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, cxContainer, cxEdit, dxLayoutControl,
  dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, cxClasses,
  dxLayoutLookAndFeels, dxGDIPlusClasses, cxButtons, cxTextEdit, cxMaskEdit,
  cxSpinEdit;

type
  TfmYaziParcalamaSecenekleri = class(TForm)
    seParcaAdet: TcxSpinEdit;
    seSatirAraligi: TcxSpinEdit;
    Image1: TImage;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel;
    lcMain: TdxLayoutControl;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    Image2: TImage;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnTamam: TdxLayoutItem;
    libtnIptal: TdxLayoutItem;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    gbBlok: TdxLayoutGroup;
    dxLayoutItem4: TdxLayoutItem;
    lblCaption: TdxLayoutItem;
    lblDes: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutItem5: TdxLayoutItem;
    chkKopyaModu: TdxLayoutCheckBoxItem;
    dxLayoutItem1: TdxLayoutItem;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmYaziParcalamaSecenekleri: TfmYaziParcalamaSecenekleri;

implementation

{$R *.dfm}

end.
