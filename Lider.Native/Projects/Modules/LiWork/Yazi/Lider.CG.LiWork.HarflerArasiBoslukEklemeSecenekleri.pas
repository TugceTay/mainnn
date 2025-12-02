unit Lider.CG.LiWork.HarflerArasiBoslukEklemeSecenekleri;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, cxContainer, cxEdit, dxLayoutControl,
  dxLayoutControlAdapters, dxLayoutContainer, dxLayoutcxEditAdapters, cxClasses,
  dxLayoutLookAndFeels, cxTextEdit, cxMaskEdit, cxSpinEdit, dxGDIPlusClasses,
  cxButtons;

type
  TfmHarflerArasiBoslukEklemeSecenekleri = class(TForm)
    seBosluk: TcxSpinEdit;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lcMain: TdxLayoutControl;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    Image: TImage;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnTamam: TdxLayoutItem;
    libtnIptal: TdxLayoutItem;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    gbBlok: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    lblCaption: TdxLayoutItem;
    lblDes: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutItem2: TdxLayoutItem;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmHarflerArasiBoslukEklemeSecenekleri: TfmHarflerArasiBoslukEklemeSecenekleri;

implementation

{$R *.dfm}

end.
