unit Lider.CG.Com.GenSingleInput;

{
        Single Input form
        (Etiket, caption ve editBox maxlength çaðýran uygulama tarafýndan
         deðiþtirilip kullanýlan çok amaçlý bir giriþ formudur)
}

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
  Buttons,
  ExtCtrls, Vcl.Mask;

type
  TfmGenSingleInput = class(TForm)
    edtGeneral: TLabeledEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.


