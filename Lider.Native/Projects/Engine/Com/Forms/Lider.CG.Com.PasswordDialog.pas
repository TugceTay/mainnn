unit Lider.CG.Com.PasswordDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, Menus, ExtCtrls, StdCtrls, Actions, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, dxLayoutContainer, cxButtons,
  dxLayoutControl, cxClasses, dxLayoutLookAndFeels, cxTextEdit;

type
  TfmPasswordDialog = class(TForm)
    teParola: TcxTextEdit;
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
    gbParola: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    lblCaption: TdxLayoutItem;
    lblDes: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    ActionList: TActionList;
    actTamam: TAction;
    procedure actTamamUpdate(Sender: TObject);
    procedure actTamamExecute(Sender: TObject);
  private
    FPassword: string;
  public
    class function Execute(APassword: string): Boolean;
  end;

var
  fmPasswordDialog: TfmPasswordDialog;

implementation

{$R *.dfm}

class function TfmPasswordDialog.Execute(APassword: string): Boolean;
begin
  Result := False;
  fmPasswordDialog := TfmPasswordDialog.Create(nil);
  with fmPasswordDialog do
  try
    FPassword := APassword;
    Result := fmPasswordDialog.ShowModal = mrOk;
  finally
    FreeAndNil(fmPasswordDialog);
  end;
end;

procedure TfmPasswordDialog.actTamamExecute(Sender: TObject);
begin
  if FPassword = teParola.Text then
    ModalResult := mrOk
  else
    Application.MessageBox('Girilen parola yanlýþ. Lütfen tekrar giriniz.', 'Hata', MB_OK + MB_ICONERROR);
end;

procedure TfmPasswordDialog.actTamamUpdate(Sender: TObject);
begin
  actTamam.Enabled := teParola.Text <> '';
end;

end.
