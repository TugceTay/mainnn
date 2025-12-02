unit Lider.CG.Com.OleFileDef;

//Temizle komutu Strem size sýfýrlanmadýðýndan çalýþmýyordu.

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Dialogs,
  SysUtils,
  ComCtrls,
  ExtCtrls,
  ExtDlgs,
  ShellApi,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS;

type
  TfmOleFileDef = class(TForm)
    Panel1: TPanel;
    btnLoadFile: TButton;
    Button2: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    btnShow: TButton;
    Image1: TImage;
    btnSaveChanges: TButton;
    procedure btnLoadFileClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure btnSaveChangesClick(Sender: TObject);
  private
    function fileAssIcon(filename: string; getSmallIcon: Boolean): HIcon;
    function FileIcon(Name: string): ticon;
    { Private declarations }
  public
    { Public declarations }
    FBlobStream: TStream;
    FShowFileName: string;
    function Enter: Word;
  end;

implementation

{$R *.DFM}

uses
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Graphics;

function TfmOleFileDef.Enter: Word;
var
  fIcon: TIcon;
  Bitmap: TBitmap;
  iconStream: TMemoryStream;
  fileExt, headerInfo: string;
  i: Integer;
begin

  FBlobStream.Position := 0;
  { save to a temporary file }

  headerInfo := licgReadStrFromStream(FBlobStream);
  if headerInfo = 'LicadGISOLE' then
  begin
    fileExt := licgReadStrFromStream(FBlobStream);
    fIcon := TIcon.Create;
    FBlobStream.Read(i, sizeof(integer));
    iconStream := TMemoryStream.Create;
    iconStream.Position := 0;
    iconStream.CopyFrom(FBlobStream, i);
    iconStream.Position := 0;
    fIcon.LoadFromStream(iconStream);
    Bitmap := TBitmap.Create;
    Bitmap.Width := fIcon.Width;
    Bitmap.Height := fIcon.Height;
    Bitmap.Canvas.Draw(0, 0, fIcon);
    image1.Picture.Bitmap.Assign(bitmap);
    bitmap.Free;
    ficon.Free;
  end;
  FBlobStream.Position := 0;
  Result := ShowModal;
end;

function TfmOleFileDef.FileIcon(Name: string): ticon;
var
  FileInfo: TSHFileInfo;
  ImageListHandle: THandle;
  aIcon: TIcon;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  ImageListHandle := SHGetFileInfo(PChar(Name), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_ICON or SHGFI_LARGEICON);
  try
    aIcon := TIcon.Create;
    try
      aIcon.Handle := FileInfo.hIcon;
      aIcon.Transparent := True;
    except
    end;
  except
  end;
  result := aicon;
end;

procedure TfmOleFileDef.btnLoadFileClick(Sender: TObject);
var
  ListImageKind: TListImageKind;
  fStream: TFileStream;
  mStream, iconStream: TMemoryStream;
  fIcon: TIcon;
  i: integer;
  Bitmap: TBitmap;
begin

  with TOpenDialog.Create(nil) do
  try
    Title := 'Dosya Ekle';
    if not Execute then
      Exit;
    iconStream := TMemoryStream.Create;
    iconStream.Position := 0;

    fIcon := FileIcon(filename);
    fIcon.SaveToStream(iconStream);
    fStream := TFileStream.Create(fileName, fmOpenRead);
    fStream.Position := 0;
    mStream := TMemoryStream.Create;
    mStream.Position := 0;
    licgWriteStrToStream('LicadGISOLE', mStream);
    licgWriteStrToStream(extractFileExt(filename), mStream);
    i := iconStream.Size;
    mStream.Write(i, sizeof(integer));
    iconStream.Position := 0;
    mStream.CopyFrom(iconStream, iconStream.Size);
    mStream.CopyFrom(fStream, fStream.Size);
    mStream.Position := 0;
    fStream.Free;
    FBlobStream.Free;
    FBlobStream := TMemoryStream.Create;
    FBlobStream.Position := 0;
    FBlobStream.CopyFrom(mStream, mStream.Size);
    Bitmap := TBitmap.Create;
    Bitmap.Width := fIcon.Width;
    Bitmap.Height := fIcon.Height;
    Bitmap.Canvas.Draw(0, 0, fIcon);

    image1.Picture.Bitmap.Assign(bitmap);
    bitmap.Free;

    mStream.free;
  finally
    Free;
  end;

end;

procedure TfmOleFileDef.Button2Click(Sender: TObject);
begin
  FBlobStream.Position := 0;
  FBlobStream.Size := 0;
end;

procedure TfmOleFileDef.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfmOleFileDef.FormDestroy(Sender: TObject);
begin
  FBlobStream.Free;
end;

procedure TfmOleFileDef.FormCreate(Sender: TObject);
begin
  FBlobStream := TMemoryStream.Create;
  FBlobStream.Position := 0;
end;

function TfmOleFileDef.fileAssIcon(filename: string; getSmallIcon: Boolean): HIcon;
var
  shfi: TShFileInfo;
begin
  if getSmallIcon then
  begin
    try
      FillChar(shfi, SizeOf(TShFileInfo), 0);
      ShGetFileInfo(PChar(Filename), 0, shfi, SizeOf(TShFileInfo), SHGFI_ICON or
        SHGFI_SMALLICON);
      result := shfi.hIcon;
    except
      result := 0;
    end;
  end
  else
  begin
    try
      FillChar(shfi, SizeOf(TShFileInfo), 0);
      ShGetFileInfo(PChar(Filename), 0, shfi, SizeOf(TShFileInfo), SHGFI_ICON);
      result := shfi.hIcon;
    except
      result := 0;
    end;
  end;
end;

procedure TfmOleFileDef.btnShowClick(Sender: TObject);
var
  fileStream: TFileStream;
  headerInfo, fileExt: string;
  TmpFileName: string;
  pos: Integer;
  byteAry: Pointer;
  fIcon: TIcon;
  i: Integer;
  iconStream: TMemoryStream;
  Bitmap: TBitmap;
begin
  FBlobStream.Position := 0;
  { save to a temporary file }

  headerInfo := licgReadStrFromStream(FBlobStream);
  if headerInfo = 'LicadGISOLE' then
  begin

    fileExt := licgReadStrFromStream(FBlobStream);
   // TmpBitmap.LoadFromStream(stream);
    TmpFileName := ChangeFileExt(GetTempFileNameLicadGISTemp('LicadGISOLEFile'), fileExt);
    FShowFileName := TmpFileName;
  //  TmpBitmap.SaveToFile(TmpFileName);
    fIcon := TIcon.Create;
    FBlobStream.Read(i, sizeof(integer));
    iconStream := TMemoryStream.Create;
    iconStream.Position := 0;
    iconStream.CopyFrom(FBlobStream, i);
    iconStream.Position := 0;
    fIcon.LoadFromStream(iconStream);
    Bitmap := TBitmap.Create;
    Bitmap.Width := fIcon.Width;
    Bitmap.Height := fIcon.Height;
    Bitmap.Canvas.Draw(0, 0, fIcon);

    image1.Picture.Bitmap.Assign(bitmap);
    bitmap.Free;

    fIcon.Free;
    iconStream.Free;
    pos := FBlobStream.Position;
    fileStream := TFileStream.Create(TmpFileName, fmCreate);
    fileStream.Position := 0;
    byteAry := GetMemEx(FBlobStream.Size - pos);
    FBlobStream.Readbuffer(byteAry^, FBlobStream.Size - pos);
    fileStream.Writebuffer(byteAry^, FBlobStream.Size - pos);
    fileStream.Free;
    shellapi.shellExecute(0, 'open', Pchar(TmpFileName), '', NIL, SW_SHOWNORMAL);

    FreeMemEx(byteAry);

  end;
end;

procedure TfmOleFileDef.btnSaveChangesClick(Sender: TObject);
var
  ListImageKind: TListImageKind;
  fStream: TFileStream;
  mStream, iconStream: TMemoryStream;
  fIcon: TIcon;
  i: integer;
  Bitmap: TBitmap;
  fileName: string;
begin

  fileName := FShowFileName;
  iconStream := TMemoryStream.Create;
  iconStream.Position := 0;

  fIcon := FileIcon(filename);
  fIcon.SaveToStream(iconStream);
  fStream := TFileStream.Create(fileName, fmOpenRead);
  fStream.Position := 0;
  mStream := TMemoryStream.Create;
  mStream.Position := 0;
  licgWriteStrToStream('LicadGISOLE', mStream);
  licgWriteStrToStream(extractFileExt(filename), mStream);
  i := iconStream.Size;
  mStream.Write(i, sizeof(integer));
  iconStream.Position := 0;
  mStream.CopyFrom(iconStream, iconStream.Size);
  mStream.CopyFrom(fStream, fStream.Size);
  mStream.Position := 0;
  fStream.Free;
  FBlobStream.Free;
  FBlobStream := TMemoryStream.Create;
  FBlobStream.Position := 0;
  FBlobStream.CopyFrom(mStream, mStream.Size);
  Bitmap := TBitmap.Create;
  Bitmap.Width := fIcon.Width;
  Bitmap.Height := fIcon.Height;
  Bitmap.Canvas.Draw(0, 0, fIcon);

  image1.Picture.Bitmap.Assign(bitmap);
  bitmap.Free;
  mStream.free;
end;

end.


