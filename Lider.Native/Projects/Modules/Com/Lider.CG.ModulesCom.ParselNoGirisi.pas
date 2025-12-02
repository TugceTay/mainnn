unit Lider.CG.ModulesCom.ParselNoGirisi;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Math, Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, Menus, StdCtrls, cxButtons,
  cxTextEdit, cxLabel, dxSkinsCore, Lider.CG.ModulesCom.Ifraz, lxStrUtil;

type
  TfmParselNoGirisi = class(TForm)
    cxLabel1: TcxLabel;
    teParselNo: TcxTextEdit;
    btnTamam: TcxButton;
    cxButton1: TcxButton;
  private
    { Private declarations }
  public
    class function Execute(ACaption: String; SonParselNo: String): String;
  end;

var
  fmParselNoGirisi: TfmParselNoGirisi;

implementation

{$R *.dfm}

uses LicadUtil;

class function TfmParselNoGirisi.Execute(ACaption: String; SonParselNo: String): String;
begin
  with TfmParselNoGirisi.Create(nil) do
  try
    Caption := ACaption;
    teParselNo.Text := KadastroAdaNoGetir(SonParselNo) + '/' + IntToStr((StrToIntDef(KadastroParselNoGetir(SonParselNo),0) + 1));
    ShowModal;
    if ModalResult = mrOK then
      Result := teParselNo.Text
    else
      Result := '';
  finally
    Free;
  end;
end;

end.
