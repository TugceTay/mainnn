unit Lider.CG.Com.Resim;

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
  Menus,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  dxSkinsCore,
  cxGraphics,
  cxButtons,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Base,
  Lider.CG.Com.EntityInt, cxControls, cxContainer, cxEdit, System.ImageList,
  Vcl.ImgList, cxClasses, cxPropertiesStore, cxLabel, cxImage, cxImageList;

type
  TfmResim = class(TForm)
    pnlUst: TPanel;
    Image1: TImage;
    cxLabel1: TcxLabel;
    lblKoordinatSistemi: TcxLabel;
    pnlOrta: TPanel;
    pnlAlt: TPanel;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    cxPropertiesStore: TcxPropertiesStore;
    btnResimEkle: TcxButton;
    btnResimTemizle: TcxButton;
    ImageList: TcxImageList;
    imgResim: TcxImage;
    procedure FormDestroy(Sender: TObject);
    procedure btnResimTemizleClick(Sender: TObject);
    procedure btnResimEkleClick(Sender: TObject);
  private
    { Private declarations }
    FEditFileName: string;
    FGraphLink: TlicgGraphicLink;
    FEntityID: TlicgEntityID;
    FCommonSubDir: string;
    FImageWidth: integer;
    FImageHeight: integer;
    procedure ReadThePicture;
    procedure Ciz;
  public
    { Public declarations }
    function Enter(EntityID: TlicgEntityID; const FileName, CommonSubDir: string):
      boolean;
    property EditFileName: string read FEditFileName;
    property ImageWidth: Integer read FImageWidth;
    property ImageHeight: Integer read FImageHeight;
  end;

implementation

{$R *.DFM}

uses
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Graphics;

resourcestring
  SDefinePicTitle = 'Resim';
  SPictNotDefined = 'Resim Yüklenmedi !';

function TfmResim.Enter(EntityID: TlicgEntityID; const FileName, CommonSubDir:
  string): boolean;
begin
  FEntityID := EntityID;
  FCommonSubDir := AddSlash(CommonSubDir);

  FGraphLink := TlicgGraphicLink.Create;

  FEditFileName := FileName;

  if {not (FEntityID = idBandsBitmap) and } (Length(FEditFileName) > 0) and
    FileExists(FEditFileName) then
    readThePicture;

  if ShowModal = mrOK then
    Result := ((FEditFileName <> '') and (imgResim.Picture <> nil))
  else
    Result := False;
end;

procedure TfmResim.ReadThePicture;
var
  IsCompressed: Boolean;
  Exten: string;
begin
  if not FileExists(FEditFileName) then
    Exit;

  (*
  if FEntityID = idBandsBitmap then
  begin
    Exten := AnsiUpperCase(ExtractFileExt(FEditFileName));
    if Exten = '.BMP' then
      GetDIBDimensions(FEditFileName, nil, FImageWidth, FImageHeight, IsCompressed);
    { kullanýlmýyordu, silindi.
    else if Exten = '.TIF' then
      GetTiffDimensions(FEditFileName, nil, FImageWidth, FImageHeight, IsCompressed)
    else if Exten = '.BIL' then
      GetBILDimensions(FEditFileName, FImageWidth, FImageHeight);}
    PaintBox1.Invalidate;
    Exit;
  end;
  *)
  FGraphLink.ReadGeneric(FEditFileName);
  //imgResim.Invalidate;
  FImageWidth := FGraphLink.Bitmap.Width;
  FImageHeight := FGraphLink.Bitmap.Height;
  imgResim.Picture.LoadFromFile(FEditFileName);
//  Ciz;
end;

procedure TfmResim.btnResimEkleClick(Sender: TObject);
var
  ListImageKind: TListImageKind;
begin

  begin
    with TOpenPictureDialog.Create(nil) do
    try
      Title := 'Resim Seç';
      Filter :=
        'Tüm Resimler (*.jpg;*.png;*.bmp;*.wmf;*.emf;*.ico)|*.jpg;*.png;*.bmp;*.wmf;*.emf;*.ico' +
        '|' + SJPGFilter + '|' + SPNGFilter + '|' + SBitmapFilter + '|' +
        SMetafileFilter + '|' + SICOFilter;
      Options := [ofPathMustExist, ofFileMustExist];
      Title := SDefinePicTitle;
      FileName := Self.FEditFileName;
      if not Execute then
        Exit;
      Self.FEditFileName := FileName;
    finally
      Free;
    end;
  end;
  ReadThePicture;
end;

procedure TfmResim.btnResimTemizleClick(Sender: TObject);
begin
  if FEditFileName <> '' then
  begin
    FGraphLink.Bitmap.Assign(nil);
    imgResim.Picture.Assign(nil);
    //imgResim.Invalidate;
    FEditFileName := '';
  end;
end;

procedure TfmResim.FormDestroy(Sender: TObject);
begin
  FGraphLink.Free;
end;

procedure TfmResim.Ciz;
(*
begin
  ABitmap := TcxBitmap32.Create;
  try
    ABitmap.Clear;
    cxImageList1.GetBitmap(0, ABitmap);
    ABitmap.Canvas.Brush.Color := Random(100000);
    ABitmap.Canvas.FillRect(Rect(2, 10, 14, 14));
    ABitmap.MakeOpaque;
    cxImageList1.Replace(0, ABitmap, nil);
  finally
    ABitmap.Free;
  end;
end;
              *)
var
  ABitmap: TcxBitmap32;

  hPaintPal, hOldPal: HPalette; {Used for realizing the palette}
  BoundsR: TRect;
begin
    ABitmap := TcxBitmap32.Create;
    ABitmap.Clear;
    ABitmap.Canvas.Brush.Color := Random(100000);
    Canvas.FillRect(Rect(2, 10, 14, 14));
    ABitmap.MakeOpaque;


  with ABitmap.Canvas do
  begin
    Boundsr := imgResim.ClientRect;
    DrawEdge(Handle, Boundsr, EDGE_RAISED, BF_RECT {or BF_MIDDLE or BF_FLAT});
  end;

  {if FEntityID = idBandsBitmap then
  begin
    with pbResim.Canvas do
    begin
      BoundsR := pbResim.ClientRect;
      if Length(FEditFileName) > 0 then
      begin
        Font.Handle := EzSystem.DefaultFontHandle;
        InflateRect(BoundsR, -1, -1);
        SetBkMode(Handle, TRANSPARENT);
        Drawtext(Handle, PChar(FEditFileName), -1, BoundsR,
          DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_PATH_ELLIPSIS);
      end;
    end;
    Exit;
  end;
  }

  if (FGraphLink.Bitmap = nil) or (FGraphLink.Bitmap.Handle = 0) then
  begin
    with ABitmap.Canvas do
    begin
      BoundsR := imgResim.ClientRect;
      FEditFileName := '(Boþ)';
      Font.Handle := Lider.CG.Com.System.DefaultFontHandle;
      InflateRect(BoundsR, -1, -1);
      //SetBkMode(Handle, TRANSPARENT);
      Drawtext(Handle, PChar(FEditFileName), -1, BoundsR, DT_SINGLELINE or
        DT_VCENTER or DT_CENTER or DT_PATH_ELLIPSIS);
    end;
    exit;
  end;


  hPaintPal := FGraphLink.Bitmap.Palette;

  {Get the old palette and select the new palette}
  hOldPal := SelectPalette(ABitmap.Canvas.Handle, hPaintPal, False);

  {Realize palette}
  RealizePalette(ABitmap.Canvas.Handle);

  {Set the stretch blt mode}
  SetStretchBltMode(ABitmap.Canvas.Handle, STRETCH_DELETESCANS);

  with imgResim do
    StretchBlt(ABitmap.Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
      FGraphLink.Bitmap.Canvas.Handle, 0, 0, FGraphLink.Bitmap.Width, FGraphLink.Bitmap.Height,
      SRCCOPY);
  if hOldPal <> 0 then
    SelectPalette(ABitmap.Canvas.Handle, hOldPal, False);


  imgResim.Picture.Bitmap.Assign(ABitmap);

end;

end.

