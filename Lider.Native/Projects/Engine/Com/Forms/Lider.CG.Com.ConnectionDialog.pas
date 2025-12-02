unit Lider.CG.Com.ConnectionDialog;

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
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  dxSkinsCore,
  dxSkinsDefaultPainters,
  cxListBox,
  cxGroupBox,
  dxBevel,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Types, cxCustomListBox;

type
  TfmConnDialog = class(TForm)
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
    cxGroupBox1: TcxGroupBox;
    dxBevel1: TdxBevel;
    dxBevel2: TdxBevel;
    cxListBox1: TcxListBox;
    dxBevel3: TdxBevel;
    Button3: TButton;
    procedure cxListBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FConnected: boolean;
  public
    { Public declarations }
  end;

function ShowConnDialog(var _user, _pass, _dbName, _host: string; var _port:
  integer): TModalResult;

procedure saveConnListIni(const Value: TConnectionParams);

implementation

{$R *.dfm}

uses
  IniFiles,
  Lider.CG.Com.LicadInt;

function ShowConnDialog(var _user, _pass, _dbName, _host: string; var _port:
  integer): TModalResult;
var
  ini: TIniFile;
  i, n: integer;
begin

  result := mrNone;

  with TfmConnDialog.Create(nil) do
  begin
    try

      ini := TIniFile.Create(ExtractFilePath(paramstr(0)) + 'PgConn.List');
      try
        n := ini.ReadInteger('Conn', 'ConnCount', 0);
        for i := 1 to n do
        begin
          cxListBox1.AddItem(ini.ReadString('Conn', 'host' + inttostr(i), '') +
            ' ' + ini.ReadString('Conn', 'db' + inttostr(i), '') + ' ' + ini.ReadString
            ('Conn', 'user' + inttostr(i), '') + ' ' + ini.ReadString('Conn',
            'pass' + inttostr(i), '') + ' ' + ini.ReadString('Conn', 'port' +
            inttostr(i), ''), TObject(i));
        end;
      finally
        ini.Free;
      end;

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

procedure saveConnListIni(const Value: TConnectionParams);
var
  ini: TIniFile;
  i, n: integer;
  user, pass, host, db, port: string;
  ok: boolean;
begin
  ok := False;
  if (Value.HostName <> '') and (Value.Database <> '') and (Value.User <> '')
    and (Value.Password <> '') and (Value.Port <> 0) then
  begin

    ini := TIniFile.Create(ExtractFilePath(paramstr(0)) + 'PgConn.List');
    try
      n := ini.ReadInteger('Conn', 'ConnCount', 0);
      for i := 1 to n do
      begin
        host := ini.ReadString('Conn', 'host' + inttostr(i), '');
        db := ini.ReadString('Conn', 'db' + inttostr(i), '');
        user := ini.ReadString('Conn', 'user' + inttostr(i), '');
        pass := ini.ReadString('Conn', 'pass' + inttostr(i), '');
        port := ini.ReadString('Conn', 'port' + inttostr(i), '');
        if port = '' then
          port := '0';

        if (Value.HostName = host) and (Value.Database = db) and (Value.User =
          user) and (Value.Password = pass) and (Value.Port = strtoint(port)) then
        begin
          ok := true;
          BREAK;

        end;

      end;

      if not ok then
      begin
        n := n + 1;
        ini.WriteInteger('Conn', 'ConnCount', n);
        ini.WriteString('Conn', 'host' + inttostr(n), Value.HostName);
        ini.WriteString('Conn', 'db' + inttostr(n), Value.Database);
        ini.WriteString('Conn', 'user' + inttostr(n), Value.User);
        ini.WriteString('Conn', 'pass' + inttostr(n), Value.Password);
        ini.WriteString('Conn', 'port' + inttostr(n), inttostr(Value.Port));
      end;

    finally
      ini.Free;
    end;
  end;
end;

procedure TfmConnDialog.cxListBox1Click(Sender: TObject);
var
  ini: TIniFile;
  i, n: integer;
begin
  if (cxListBox1.ItemIndex >= 0) and (cxListBox1.ItemObject <> nil) then
  begin

    ini := TIniFile.Create(ExtractFilePath(paramstr(0)) + 'PgConn.List');
    try
      n := ini.ReadInteger('Conn', 'ConnCount', 0);
      for i := 1 to n do
      begin
        if i = integer(cxListBox1.ItemObject) then
        begin

          Editse.Text := ini.ReadString('Conn', 'host' + inttostr(i), '');
          Editdb.Text := ini.ReadString('Conn', 'db' + inttostr(i), '');
          Editus.Text := ini.ReadString('Conn', 'user' + inttostr(i), '');
          Editpa.Text := ini.ReadString('Conn', 'pass' + inttostr(i), '');
          Editpo.Text := ini.ReadString('Conn', 'port' + inttostr(i), '');

          BREAK;

        end;

      end;
    finally
      ini.Free;
    end;

  end;
end;

procedure TfmConnDialog.Button3Click(Sender: TObject);
begin
  cxListBox1.Clear;
  DeleteFile(ExtractFilePath(paramstr(0)) + 'PgConn.List')
end;

end.


