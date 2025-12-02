unit Lider.CG.Com.Tips;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls
{$IFDEF LEVEL6}
    ,
  Variants
{$ENDIF}
;

type
  TfmTips = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Bevel1: TBevel;
    Label1: TLabel;
    TipLabel: TLabel;
    Image1: TImage;
    chkShow: TCheckBox;
    BtnNextTip: TButton;
    BtnClose: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  IniFiles;

procedure TfmTips.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TiniFile.create(ExtractFilePath(paramstr(0)) + 'Settings\Tips.ini') do
  begin
    try
      writeBool('Tips', 'Start', chkShow.Checked);
    finally
      Free;
    end;
  end;

end;

end.


