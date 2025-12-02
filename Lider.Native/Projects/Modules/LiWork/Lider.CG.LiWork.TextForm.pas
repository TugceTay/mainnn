unit Lider.CG.LiWork.TextForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  dxSkinOffice2019Black, dxSkinVisualStudio2013Dark, cxCheckBox, Vcl.Menus,
  cxTextEdit, cxLabel, Vcl.ExtCtrls, cxButtons, cxCurrencyEdit;

type
  TfmTextForm = class(TForm)
    chkAci: TcxCheckBox;
    pnlOrta: TPanel;
    pnlUst: TPanel;
    Image1: TImage;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxLabel1: TcxLabel;
    teMetin: TcxTextEdit;
    cxLabel4: TcxLabel;
    pnlAlt: TPanel;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    ceBoyut: TcxCurrencyEdit;
    procedure ceBoyutPropertiesChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmTextForm: TfmTextForm;

implementation

{$R *.dfm}

procedure TfmTextForm.ceBoyutPropertiesChange(Sender: TObject);
begin
//fmTextForm.ceBoyut.Value := abs(fmTextForm.ceBoyut.Value);
if fmTextForm.ceBoyut.Value < 1 then
  fmTextForm.ceBoyut.Value := 1;
end;

end.
