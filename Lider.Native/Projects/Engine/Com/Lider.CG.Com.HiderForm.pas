unit Lider.CG.Com.HiderForm;

{$I Lider.CG.Com.Component.inc}

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
  lxFormHider,
  Lider.CG.Com.CmdLine;

type
  TfmHiderForm = class(TForm)
    FormHider1: TlxFormHider;
    procedure FormHide(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    FPushAction: TlicgAction;
    FCmdLine: TlicgBaseCmdLine;
    SaveOnAfterCommand: TlicgAfterCommandEvent;
    procedure MyAfterCommand(Sender: TObject; const Command, ActionID: string);
  public
    { Public declarations }
    constructor Create(cmdLine: TlicgBaseCmdline; PushAction: TlicgAction);
  end;

implementation

{$R *.dfm}

const
  HiderCommand: string = 'FHiderActCommand';

constructor TfmHiderForm.Create(cmdLine: TlicgBaseCmdline; PushAction: TlicgAction);
begin
  inherited Create(nil);
  FPushAction := PushAction;
  FCmdLine := CmdLine;
  SaveOnAfterCommand := CmdLine.OnAfterCommand;
  CmdLine.OnAfterCommand := MyAfterCommand;
end;

procedure TfmHiderForm.FormHide(Sender: TObject);
begin
  FCmdLine.Push(FPushAction, false, '', HiderCommand);
  if not FCmdLine.ActiveDrawBox.Focused then
    FCmdLine.ActiveDrawBox.SetFocus;
end;

procedure TfmHiderForm.MyAfterCommand(Sender: TObject; const Command, ActionID: string);
begin
  //if HiderCommand = ActionID then begin
  fCmdLine.OnAfterCommand := SaveOnAfterCommand;
  FormHider1.Close;
  //end;
end;

procedure TfmHiderForm.FormPaint(Sender: TObject);
begin
  FormHider1.Hide;

end;

end.


