unit Lider.CG.LiWork.YuvarlamaSecenekleri;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, cxContainer, cxEdit, dxLayoutControl,
  dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer,
  dxLayoutLookAndFeels, cxClasses, dxGDIPlusClasses, cxButtons, cxTextEdit,
  cxMaskEdit, cxSpinEdit;

type
  TfmYuvarlamaSecenekleri = class(TForm)
    seOndalikBasamakSayisi: TcxSpinEdit;
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
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    gbBlok: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    lblCaption: TdxLayoutItem;
    lblDes: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutItem3: TdxLayoutItem;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmYuvarlamaSecenekleri: TfmYuvarlamaSecenekleri;

implementation

{$R *.dfm}


end.
