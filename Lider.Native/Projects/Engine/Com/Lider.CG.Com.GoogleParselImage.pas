unit Lider.CG.Com.GoogleParselImage;

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
  ExtCtrls,
  ActnList,
  Menus,
  StdCtrls,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  dxSkinsCore,
  dxSkinsDefaultPainters,
  cxGroupBox,
  cxButtons,
  cxListBox,
  cxCheckListBox,
  dxSkinsdxStatusBarPainter,
  dxStatusBar,
  cxSplitter,
  Lider.CG.Com.Lists,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GIS,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TfmGoogleParselImg = class(TForm)
    cxGroupBox1: TcxGroupBox;
    Image1: TImage;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    cxButton3: TcxButton;
    procedure cxButton3Click(Sender: TObject);
  private
    { Private declarations }
    fFN: string;
  public
    { Public declarations }
  end;

function GenerateGoogleParselImg(FN: string): boolean;

implementation

{$R *.dfm}

uses
  Lider.CG.Com.StringLi,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.CmdLine;

function GenerateGoogleParselImg(FN: string): boolean;
var
  i: integer;
begin
  result := false;
  with TfmGoogleParselImg.Create(nil) do
  begin
    try
      fFN := fn;
      Image1.Picture.LoadFromFile(fn);
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
  end;
end;

procedure TfmGoogleParselImg.cxButton3Click(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  begin
    try
      DefaultExt := ExtractFileExt(fFN);
      Filter := 'Ýmaj Dosya|*' + ExtractFileExt(fFN);
      if Execute then
      begin
        Image1.Picture.SaveToFile(ChangeFileExt(FileName, ExtractFileExt(fFN)));
      end;
    finally
      Free;
    end;
  end;
end;

end.


