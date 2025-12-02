unit Lider.CG.LiWork.HazirYaziSecenekleri;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, cxContainer, cxEdit, dxLayoutContainer,
  cxClasses, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxGDIPlusClasses,
  dxLayoutLookAndFeels, cxCheckBox, cxButtons, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, dxLayoutControl;

type
  TfmHazirYaziSecenekleri = class(TForm)
    IcMainGroup_Root: TdxLayoutGroup;
    IcMain: TdxLayoutControl;
    lblInfoHader: TdxLayoutLabeledItem;
    lblInfo: TdxLayoutLabeledItem;
    cbMetin1: TcxComboBox;
    dxLayoutItem1: TdxLayoutItem;
    cbMetin2: TcxComboBox;
    dxLayoutItem2: TdxLayoutItem;
    cbMetin3: TcxComboBox;
    dxLayoutItem3: TdxLayoutItem;
    cbMetin4: TcxComboBox;
    dxLayoutItem4: TdxLayoutItem;
    cbMetin5: TcxComboBox;
    dxLayoutItem5: TdxLayoutItem;
    cbMetin6: TcxComboBox;
    dxLayoutItem6: TdxLayoutItem;
    cbMetin7: TcxComboBox;
    dxLayoutItem7: TdxLayoutItem;
    cbMetin8: TcxComboBox;
    dxLayoutItem8: TdxLayoutItem;
    cbMetin9: TcxComboBox;
    dxLayoutItem9: TdxLayoutItem;
    cbMetin10: TcxComboBox;
    dxLayoutItem10: TdxLayoutItem;
    teTamMetin: TcxTextEdit;
    dxteTamMetin: TdxLayoutItem;
    btnAyarlar: TcxButton;
    btnOptions: TdxLayoutItem;
    btnTamam: TcxButton;
    btnOk: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    btnIptal: TcxButton;
    btnCancel: TdxLayoutItem;
    chkAyarlarKullan: TcxCheckBox;
    chkAyarlarKullanilsin: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    chkMetin2: TcxCheckBox;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    chkMetin3: TcxCheckBox;
    dxLayoutItem18: TdxLayoutItem;
    chkMetin4: TcxCheckBox;
    dxLayoutItem19: TdxLayoutItem;
    chkMetin5: TcxCheckBox;
    dxLayoutItem20: TdxLayoutItem;
    chkMetin6: TcxCheckBox;
    dxLayoutItem21: TdxLayoutItem;
    chkMetin7: TcxCheckBox;
    dxLayoutItem22: TdxLayoutItem;
    chkMetin8: TcxCheckBox;
    dxLayoutItem23: TdxLayoutItem;
    chkMetin9: TcxCheckBox;
    dxLayoutItem24: TdxLayoutItem;
    chkMetin10: TcxCheckBox;
    dxLayoutItem25: TdxLayoutItem;
    chkMetin1: TcxCheckBox;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    Image1: TImage;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    procedure IcMainDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmHazirYaziSecenekleri: TfmHazirYaziSecenekleri;

implementation

{$R *.dfm}

procedure TfmHazirYaziSecenekleri.IcMainDblClick(Sender: TObject);
begin
  IcMAin.Customization := True;
end;

end.
