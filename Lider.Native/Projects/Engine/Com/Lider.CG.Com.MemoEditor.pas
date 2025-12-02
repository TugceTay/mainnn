unit Lider.CG.Com.MemoEditor;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  Messages,
  ExtCtrls;

type
  TfmMemoEditor = class(TForm)
    Bevel1: TBevel;
    Memo1: TMemo;
    Panel1: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    procedure Memo1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TfmMemoEditor.Memo1Change(Sender: TObject);
//var
  //LineNumber, ColNumber: Integer;
begin
  //LineNumber  := SendMessage(Memo1.Handle, EM_LINEFROMCHAR, 0, Memo1.SelStart);
  //ColNumber := (Memo1.SelStart - SendMessage(Memo1.Handle, EM_LINEINDEX, LineNumber, 0));

  Label1.Caption := Format('%d Satýr', [Memo1.Lines.Count]);

  //Format('%d Lines; Row %d, Col %d', [Memo1.Lines.Count, LineNumber, ColNumber]);
end;

procedure TfmMemoEditor.FormCreate(Sender: TObject);
begin

  Caption := 'Metin Editörü';
  OKBtn.Caption := 'Tamam';
  CancelBtn.caption := 'Vazgeç';
  Label1.Caption := 'Metin';

end;

end.


