unit Lider.CG.Com.ConnectionDialogExt;

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
  ComCtrls,
  cxPropertiesStore,
  Lider.CG.Com.GIS,
  cxClasses;

type
  TfmConnDialogExt = class(TForm)
    Editus: TEdit;
    Editpa: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Editdb: TEdit;
    Editse: TEdit;
    Editpo: TEdit;
    Button1: TButton;
    Button2: TButton;
    StatusBar1: TStatusBar;
    cxPropertiesStore1: TcxPropertiesStore;
    Label6: TLabel;
    ComboBox1: TComboBox;
  private
    { Private declarations }
    FConnected: boolean;
  public
    { Public declarations }
  end;

function ShowConnDialogExt(var _user, _pass, _dbName, _host: string; var _port:
  integer): TModalResult;

implementation

{$R *.dfm}

uses
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Types;

function ShowConnDialogExt(var _user, _pass, _dbName, _host: string; var _port:
  integer): TModalResult;
begin

  result := mrNone;

  with TfmConnDialogExt.Create(nil) do
  begin
    try
      FConnected := false;
      if _user <> '' then
        Editus.Text := _user;
      if _pass <> '' then
        Editpa.Text := _pass;
      if _dbName <> '' then
        Editdb.Text := _dbName;
      if _host <> '' then
        Editse.Text := _host;
      if _port <> 0 then
        Editpo.Text := inttostr(_port);

      Button1.caption := 'Baðlan';
      Button2.caption := 'Ýptal';

      Result := ShowModal;
      if Result = mrOK then
      begin
        _user := Editus.Text;
        _pass := Editpa.Text;
        _dbName := Editdb.Text;
        _host := Editse.Text;
        _port := strtoint(Editpo.Text);
      end;

    finally
      Free;
    end;
  end;
end;

end.


