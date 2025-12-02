unit Lider.CG.ModulesCom.Ifraz;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Types,
  Graphics,
  Controls,
  Forms,
  Math,cxGraphics,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  Vcl.Menus,
  dxSkinsCore,
  cxControls,
  cxContainer,
  cxEdit,
  System.Actions,
  System.ImageList,
  Vcl.ImgList,
  cxImageList,
  IniFiles,
  Vcl.ActnList,
  cxTextEdit,
  cxMaskEdit,
  cxSpinEdit,
  cxLabel,
  cxGroupBox,
  cxCheckBox,
  cxMCListBox,
  cxRadioGroup,
  cxImage,
  Vcl.StdCtrls,
  cxButtons,
  Vcl.ExtCtrls,
  cxCustomListBox,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.GIS,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Math,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Lib,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.System,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.DrawToolsInt,
  lxStrUtil, lxFormHider;

type
  TfmIfraz = class(TForm)
    Panel1: TPanel;
    imgIfraz: TcxImage;
    rgYeniAlanYonu: TcxRadioGroup;
    MClbIfrazYontemi: TcxMCListBox;
    cxImageList1: TcxImageList;
    chbGeriKalanAlaniTekrarBolunecek: TcxCheckBox;
    cxGroupBox1: TcxGroupBox;
    chkParselNoYaz: TcxCheckBox;
    chkAdaNoYaz: TcxCheckBox;
    chkParselinOrtaNoktasinaYaz: TcxCheckBox;
    cxLabel1: TcxLabel;
    seYaziBoyu: TcxSpinEdit;
    chbSonSecilenHattiKullan: TcxCheckBox;
    ActionList1: TActionList;
    actIptal: TAction;
    actTamam: TAction;
    pnlAlt: TPanel;
    btnTamam: TcxButton;
    btnIptal: TcxButton;
    procedure CreateParams(var Params: TCreateParams);
    procedure MClbIfrazYontemiClick(Sender: TObject);
    procedure rgYeniAlanYonuClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
//    procedure actIptalExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private

  public
    FAction: TLicgAction;
  end;

var
  fmIfraz: TfmIfraz;

implementation

{$R *.dfm}

procedure TfmIfraz.CreateParams(var Params: TCreateParams);
begin
  inherited Createparams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  //Params.WndParent := GetDesktopWindow; ilker bunu kullanma
end;

procedure TfmIfraz.FormClose(Sender: TObject;
  var Action: TCloseAction);
var Shift:  TShiftState;
begin
  CurrCmdLine.Tag := 0;
  CurrCmdLine.CurrentAction.Finished := True;

  Action := caFree;
  fmIfraz := nil;
  //CurrCmdLine.CurrentAction.
//  FormAyarlariniKaydet;
// Yapýlcak click den sonra kapanýyor action
//  if FAction <> nil then
//    FAction.Finished := True;
//
//  if CurrCmdLine.CurrentAction <> nil then
//  begin
//    CurrCmdLine.CurrentAction.Finished := True;
//    CurrCmdLine.LastCommand := SCmdExternal;
//  end;
end;

(*
procedure TfmIfraz.actIptalExecute(Sender: TObject);
begin
//  SendMessage(Self.Handle, WM_SYSCOMMAND, SC_CLOSE, 0 );
  //PostMessage(Self.Handle,wm_close,0,0);
  {FormAyarlariniKaydet;
  FormStyle := fsNormal;
  Hide;
  fmBYAna.Show;}
end;
*)

procedure TfmIfraz.FormShow(Sender: TObject);
begin
  MClbIfrazYontemi.SetFocus;
  FAction := CurrCmdLine.CurrentAction;
end;

procedure TfmIfraz.MClbIfrazYontemiClick(Sender: TObject);
begin
  case MClbIfrazYontemi.ItemIndex of
  0 : begin // Paralel
        rgYeniAlanYonu.Properties.Items.Items[0].Caption := 'Solda';
        rgYeniAlanYonu.Properties.Items.Items[1].Caption := 'Saðda';
        case rgYeniAlanYonu.ItemIndex of
        0 : begin
              cxImageList1.GetImage(0, imgIfraz.Picture.Bitmap);
            end;
        1 : begin
              cxImageList1.GetImage(1, imgIfraz.Picture.Bitmap);
            end;
        end;
      end;
  1 : begin // Dik
        rgYeniAlanYonu.Properties.Items.Items[0].Caption := 'Solda';
        rgYeniAlanYonu.Properties.Items.Items[1].Caption := 'Saðda';
      case rgYeniAlanYonu.ItemIndex of
        0 : begin
              cxImageList1.GetImage(2, imgIfraz.Picture.Bitmap);
            end;
        1 : begin
              cxImageList1.GetImage(3, imgIfraz.Picture.Bitmap);
            end;
        end;
      end;
  2 : begin // Sabit Nokta
        rgYeniAlanYonu.Properties.Items.Items[0].Caption := 'Saat Yönü';
        rgYeniAlanYonu.Properties.Items.Items[1].Caption := 'Saat Yönü Tersi';
      case rgYeniAlanYonu.ItemIndex of
        0 : begin
              cxImageList1.GetImage(4, imgIfraz.Picture.Bitmap);
            end;
        1 : begin
              cxImageList1.GetImage(5, imgIfraz.Picture.Bitmap);
            end;
        end;
      end;
  3 : begin // Serbest
        rgYeniAlanYonu.Properties.Items.Items[0].Caption := 'Solda';
        rgYeniAlanYonu.Properties.Items.Items[1].Caption := 'Saðda';
      case rgYeniAlanYonu.ItemIndex of
        0 : begin
              cxImageList1.GetImage(6, imgIfraz.Picture.Bitmap);
            end;
        1 : begin
              cxImageList1.GetImage(7, imgIfraz.Picture.Bitmap);
            end;
        end;
      end;
  4 : begin // Cephe Açý
        rgYeniAlanYonu.Properties.Items.Items[0].Caption := 'Solda';
        rgYeniAlanYonu.Properties.Items.Items[1].Caption := 'Saðda';
      case rgYeniAlanYonu.ItemIndex of
        0 : begin
              cxImageList1.GetImage(8, imgIfraz.Picture.Bitmap);
            end;
        1 : begin
              cxImageList1.GetImage(9, imgIfraz.Picture.Bitmap);
            end;
        end;
      end;
  5 : begin // Paralel Mesafe
        rgYeniAlanYonu.Properties.Items.Items[0].Caption := 'Solda';
        rgYeniAlanYonu.Properties.Items.Items[1].Caption := 'Saðda';
      case rgYeniAlanYonu.ItemIndex of
        0 : begin
              cxImageList1.GetImage(10, imgIfraz.Picture.Bitmap);
            end;
        1 : begin
              cxImageList1.GetImage(11, imgIfraz.Picture.Bitmap);
            end;
        end;
      end;
  6 : begin // Kalan
        rgYeniAlanYonu.Properties.Items.Items[0].Caption := 'Solda';
        rgYeniAlanYonu.Properties.Items.Items[1].Caption := 'Saðda';
        cxImageList1.GetImage(12, imgIfraz.Picture.Bitmap);
      end;
  end;
end;

procedure TfmIfraz.rgYeniAlanYonuClick(Sender: TObject);
begin
  MClbIfrazYontemi.OnClick(Sender As TObject);
end;

end.
