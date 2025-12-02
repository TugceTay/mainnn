unit Lider.CG.Com.Skin;

interface

uses
  dxCore,
  cxClasses,
  cxLookAndFeels,
  dxSkinsForm,
  dxSkinsCore;

var
  LicadSkinController: TdxSkinController = nil;

implementation

initialization
  LicadSkinController := TdxSkinController.Create(nil);
  LicadSkinController.RenderMode := rmDirectX;
  LicadSkinController.ScrollbarMode := sbmClassic;
  LicadSkinController.ShowFormShadow := bTrue;
  LicadSkinController.UseSkinsInPopupMenus := False;

finalization
  LicadSkinController.Free;

end.


