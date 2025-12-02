unit Lider.CG.Details.AlanBirlestirSecenekleri;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore, Vcl.Menus,
  lxFormHider, cxClasses, cxPropertiesStore, Vcl.StdCtrls, cxButtons, cxLabel,
  Vcl.ExtCtrls, cxCheckBox, cxTextEdit, cxMaskEdit, cxSpinEdit;

type
  TfmAlanBirlestirSecenekleri = class(TForm)
    PnlOrta: TPanel;
    chkBirlestirilenleriKoru: TcxCheckBox;
    pnlUst: TPanel;
    Image1: TImage;
    lblInfoHeader: TcxLabel;
    lblInfo: TcxLabel;
    pnlAlt: TPanel;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    cxLabel1: TcxLabel;
    spTolerance: TcxSpinEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmAlanBirlestirSecenekleri: TfmAlanBirlestirSecenekleri;

implementation

{$R *.dfm}


end.
