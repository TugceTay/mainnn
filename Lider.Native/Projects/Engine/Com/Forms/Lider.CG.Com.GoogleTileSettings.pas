unit Lider.CG.Com.GoogleTileSettings;

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
  Menus,
  StdCtrls,
  PngImage,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  dxSkinsCore,
  dxSkinsDefaultPainters,
  cxPropertiesStore,
  cxLabel,
  cxTextEdit,
  cxMaskEdit,
  cxButtonEdit,
  cxSpinEdit,
  cxButtons,
  cxClasses,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.System,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Grapher,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.Base,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.GIS,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.ReferenceInt,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.CSInt;

type
  TfmGoogleTileSettings = class(TForm)
    cxButtonEdit1: TcxButtonEdit;
    cxLabel1: TcxLabel;
    cxSpinEdit1: TcxSpinEdit;
    cxSpinEdit2: TcxSpinEdit;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    cxButton3: TcxButton;
    FileOpenDialog1: TFileOpenDialog;
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxButtonEdit1PropertiesEditValueChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cxSpinEdit1PropertiesChange(Sender: TObject);
    procedure cxSpinEdit2PropertiesChange(Sender: TObject);
    procedure cxButton3Click(Sender: TObject);
  private
    { Private declarations }
    fpath: string;
    fminL, fmaxL: integer;
    FFormShowed: boolean;
  public
    { Public declarations }

  end;

function GetTileSettings(var path: string; var minL, maxL: integer): boolean;

implementation

{$R *.dfm}

uses
  Lider.CG.Com.ProjectToGoogleTile;

function GetTileSettings(var path: string; var minL, maxL: integer): boolean;
begin

  with TfmGoogleTileSettings.Create(nil) do
  begin
    try
      fpath := path;
      fminL := minL;
      fmaxL := maxL;
      cxSpinEdit1.Properties.MinValue := minL;
      cxSpinEdit1.Properties.MaxValue := maxL;
      cxSpinEdit1.Value := minL;

      cxSpinEdit2.Properties.MinValue := minL;
      cxSpinEdit2.Properties.MaxValue := maxL;
      cxSpinEdit2.Value := maxL;

      Result := Showmodal = mrOK;

      if Result then
      begin
        path := trim(cxButtonEdit1.Text);
        minL := ROUND(cxSpinEdit1.Value);
        maxL := ROUND(cxSpinEdit2.Value);
      end;

    finally
      Free;
    end;

  end;

end;

procedure TfmGoogleTileSettings.cxButtonEdit1PropertiesButtonClick(Sender:
  TObject; AButtonIndex: Integer);
begin
  with FileOpenDialog1 do
  begin
    try
      Title := 'Tile Ýmajlerýn Oluþturulacaðý Yeri Seç';
      if Execute then
      begin
        fPath := FileOpenDialog1.FileName;
        cxButtonEdit1.Text := fpath;

      end;
    finally
      Free;
    end;
  end;
end;

procedure TfmGoogleTileSettings.cxButtonEdit1PropertiesEditValueChanged(Sender: TObject);
begin
  fPath := cxButtonEdit1.Text;
end;

procedure TfmGoogleTileSettings.FormShow(Sender: TObject);
begin
  FFormShowed := true;
end;

procedure TfmGoogleTileSettings.FormCreate(Sender: TObject);
begin
  FFormShowed := False;
end;

procedure TfmGoogleTileSettings.cxSpinEdit1PropertiesChange(Sender: TObject);
begin
  if not FFormShowed then
    exit;
  if cxSpinEdit1.Value > cxSpinEdit2.Value then
    cxSpinEdit1.Value := cxSpinEdit2.Value;

end;

procedure TfmGoogleTileSettings.cxSpinEdit2PropertiesChange(Sender: TObject);
begin
  if not FFormShowed then
    exit;

  if cxSpinEdit2.Value < cxSpinEdit1.Value then
    cxSpinEdit2.Value := cxSpinEdit1.Value;

end;

procedure TfmGoogleTileSettings.cxButton3Click(Sender: TObject);
var
  SL: TStrings;
  P1, P2: IlicgCS;
  R: TlicgExtent;
  minz, maxz: integer;
begin

  P1 := CurrCmdLine.AcTiveDrawBox.GIS.MapInfo.CS;
  if P1.IsUnknownCS then
  begin
    Application.MessageBox('Proje projeksiyonu tanýmlayýnýz...', 'Bilgi', MB_OK
      + MB_ICONINFORMATION);
    Exit;
  end;

  P2 := Licad.CreateCSFactory.ByEPSG(4326);
  {
  P2.ProjSystem := ord(Gdal_ProjSystem_GEO) ;
  P2.ProjDatum  := ord(Gdal_ProjDatum_World_Geodetic_System_1984) ;
  P2.ProjAttrbCnt := 0;
  P2.ProjUnit := Gdal__UnitARC_DEGREES;
  }

  R := CurrCmdLine.AcTiveDrawBox.Grapher.CurrentParams.VisualWindow;
  CurrCmdLine.AcTiveDrawBox.Gis.CSCoorToCSCoor(R.LowerLeft.X, R.LowerLeft.Y, R.LowerLeft.X,
    R.LowerLeft.Y, P1, P2, nil);
  CurrCmdLine.AcTiveDrawBox.Gis.CSCoorToCSCoor(R.UpperRight.X, R.UpperRight.Y, R.UpperRight.X,
    R.UpperRight.Y, P1, P2, nil);

  minz := Lider.CG.Com.ProjectToGoogleTile.CalcZoomFactor(R.LowerLeft.Y, R.LowerLeft.X,
    R.UpperRight.Y, R.UpperRight.X, abs(round(CurrCmdLine.AcTiveDrawBox.Grapher.ViewPortExtent.LowerLeft.X
    - CurrCmdLine.AcTiveDrawBox.Grapher.ViewPortExtent.UpperRight.X)), abs(round(CurrCmdLine.AcTiveDrawBox.Grapher.ViewPortExtent.LowerLeft.Y
    - CurrCmdLine.AcTiveDrawBox.Grapher.ViewPortExtent.UpperRight.Y)));

  maxz := 19;
  with TSaveDialog.Create(nil) do
  begin
    DefaultExt := 'txt';
    Filter := 'Text|*.txt';
    FileName := 'boundary.txt';
    try
      SL := TStringList.create;

      if Execute then
      begin
        SL.Add('MinZoom  MaxZoom  Minx (lng)    Miny(lat) Maxx (lng)    Maxy(lat) ');
        SL.Add(inttostr(minz) + '  ' + inttostr(maxz) + '  ' + Floattostr(R.lowerLeft.Y)
          + '  ' + Floattostr(R.lowerLeft.X) + '  ' + Floattostr(R.UpperRight.Y)
          + '  ' + Floattostr(R.UpperRight.X) + '  ');
        SL.SaveToFile(FileName);
      end;
    finally
      SL.Free;
      Free;
    end;
  end;

end;

end.


