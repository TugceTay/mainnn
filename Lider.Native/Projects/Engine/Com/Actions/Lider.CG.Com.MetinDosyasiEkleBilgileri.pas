unit Lider.CG.Com.MetinDosyasiEkleBilgileri;

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
  StdCtrls,
  Menus,
  Buttons,
  ExtCtrls,
  cxPropertiesStore,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  dxSkinsCore,
  dxSkinsDefaultPainters,
  cxCheckBox,
  dxBevel,
  cxGroupBox,
  cxClasses,
  cxButtons,
  cxTextEdit,
  cxCurrencyEdit,
  cxLabel;

type
  TfmMetinDosyasiEkleBilgileri = class(TForm)
    pnlUst: TPanel;
    Image1: TImage;
    cxLabel1: TcxLabel;
    lblKoordinatSistemi: TcxLabel;
    pnlOrta: TPanel;
    lblY: TcxLabel;
    ceX: TcxCurrencyEdit;
    lblX: TcxLabel;
    ceY: TcxCurrencyEdit;
    lblZ: TcxLabel;
    ceZ: TcxCurrencyEdit;
    cxPropertiesStore: TcxPropertiesStore;
    OpenDialog: TOpenDialog;
    pnlAlt: TPanel;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
  private

  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.


