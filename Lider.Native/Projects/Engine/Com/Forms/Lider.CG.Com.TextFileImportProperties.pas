unit Lider.CG.Com.TextFileImportProperties;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  lkGraphics,
  Menus,
  lkLookAndFeelPainters,
  StdCtrls,
  lkButtons,
  lkControls,
  lkContainer,
  lkEdit,
  lkTextEdit,
  lkMaskEdit,
  lkDropDownEdit,
  lkLookAndFeels, lkSkinsCore, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore, cxButtons,
  cxTextEdit, cxMaskEdit, cxDropDownEdit;

type
  TfmTextFileProperties = class(TForm)
    Label1: TLabel;
    cbDelimiterChar: TcxComboBox;
    btnOK: TcxButton;
    btnCancel: TcxButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmTextFileProperties: TfmTextFileProperties;

implementation

{$R *.dfm}

end.


