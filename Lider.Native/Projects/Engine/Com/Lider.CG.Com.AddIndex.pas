unit Lider.CG.Com.AddIndex;

interface

uses
  Windows,
  Classes,
  Forms,
  Controls,
  StdCtrls,
  Dialogs;

type
  TfmAddIndex = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    List2: TListBox;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Check1: TCheckBox;
    Check2: TCheckBox;
    procedure List2Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TfmAddIndex.List2Click(Sender: TObject);
begin
  if List2.ItemIndex < 0 then
    Exit;
  if Length(Edit2.Text) = 0 then
    Edit2.Text := List2.Items[List2.ItemIndex]
  else
    Edit2.Text := Edit2.Text + '+' + List2.Items[List2.ItemIndex];
end;

procedure TfmAddIndex.OKBtnClick(Sender: TObject);
begin
  if Length(Edit2.Text) = 0 then
  begin
    ShowMessage('Kolon türü hatalý !');
    ModalResult := mrNone;
  end;
end;

end.


