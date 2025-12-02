unit Lider.CG.Com.GooglePrefences;

interface

const
  ZOOM_LEVELS = 30;
  scale_img: integer = 1;
  {$J+}
  maxi_Ximg_size: integer = 640;
  maxi_Yimg_size: integer = 640;
  {$J-}
  forceScale1AndSize640640: boolean = true;

type
  TGooglePreferences = class
  private
    FMapType: string;
    procedure LoadFromFile;
    procedure SaveToFile;
  public
    constructor create;
    destructor destroy; override;
    property MapType: string read FMapType write FMapType;
  end;

var
  GLB_GooglePreferences: TGooglePreferences = nil;

implementation

uses
  Inifiles,
  SysUtils;

  { TGooglePreferences }

constructor TGooglePreferences.create;
begin
  inherited;
  LoadFromFile;
end;

destructor TGooglePreferences.destroy;
begin
  SaveToFile;
  inherited;
end;

procedure TGooglePreferences.LoadFromFile;
begin
  with TIniFile.Create(ExtractFilePath(paramstr(0)) + 'Settings\GooglePreferences.ini') do
  begin
    try
      FMapType := ReadString('General', 'MapType', 'hybrid');
    finally
      FRee;
    end;
  end;

end;

procedure TGooglePreferences.SaveToFile;
begin
  with TIniFile.Create(ExtractFilePath(paramstr(0)) + 'Settings\GooglePreferences.ini') do
  begin
    try
      WriteString('General', 'MapType', FMapType);
    finally
      FRee;
    end;
  end;

end;

initialization
  GLB_GooglePreferences := TGooglePreferences.Create;

finalization
  if Assigned(GLB_GooglePreferences) then
    FreeAndNil(GLB_GooglePreferences)

end.


