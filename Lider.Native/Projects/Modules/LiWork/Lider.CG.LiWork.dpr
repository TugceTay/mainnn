library Lider.CG.LiWork;

 //{$Include Lider.CG.Com.MM.pas}

uses
  {$Include Lider.CG.Com.MM.pas}
  Lider.CG.LiWork.DataModuleCom in 'Lider.CG.LiWork.DataModuleCom.pas' {dmCom: TDataModule},
  Lider.CG.LiWork.Module in 'Lider.CG.LiWork.Module.pas',
  Lider.CG.LiWork.Ribbon in 'Lider.CG.LiWork.Ribbon.pas' {Lider.CG.LiWork.ActionSetTextQuickText in 'Yazi\Lider.CG.LiWork.ActionSetTextQuickText.pas',
  Lider.CG.LiWork.ActionSetTextSeparate in 'Yazi\Lider.CG.LiWork.ActionSetTextSeparate.pas',
  Lider.CG.LiWork.HarflerArasiBosluk in 'Yazi\Lider.CG.LiWork.HarflerArasiBosluk.pas' {fmHarflerArasiBosluk},
  Lider.CG.LiWork.ActionSetTextQuickText in 'Yazi\Lider.CG.LiWork.ActionSetTextQuickText.pas',
  Lider.CG.LiWork.ActionSetTextSeparate in 'Yazi\Lider.CG.LiWork.ActionSetTextSeparate.pas',
  Lider.CG.LiWork.HarflerArasiBoslukEklemeSecenekleri in 'Yazi\Lider.CG.LiWork.HarflerArasiBoslukEklemeSecenekleri.pas' {fmHarflerArasiBoslukEklemeSecenekleri},
  Lider.CG.LiWork.HazirYaziSecenekleri in 'Yazi\Lider.CG.LiWork.HazirYaziSecenekleri.pas' {fmHazirYaziSecenekleri},
  Lider.CG.LiWork.HazirYaziAyarlari in 'Yazi\Lider.CG.LiWork.HazirYaziAyarlari.pas' {fmHazirYaziAyarlari},
  Lider.CG.LiWork.HizliYaziDüzenle in 'Yazi\Lider.CG.LiWork.HizliYaziDüzenle.pas' {fmHizliYaziDüzenle},
  Lider.CG.LiWork.LauncherSetTextFirstWordUpper in 'Yazi\Lider.CG.LiWork.LauncherSetTextFirstWordUpper.pas',
  Lider.CG.LiWork.LauncherSetTextMathOperations in 'Yazi\Lider.CG.LiWork.LauncherSetTextMathOperations.pas',
  Lider.CG.LiWork.LauncherSetTextRoundingToDecimal in 'Yazi\Lider.CG.LiWork.LauncherSetTextRoundingToDecimal.pas',
  Lider.CG.LiWork.LauncherSetTextSpacingBetweenWord in 'Yazi\Lider.CG.LiWork.LauncherSetTextSpacingBetweenWord.pas',
  Lider.CG.LiWork.LauncherSetTextUpperOrLower in 'Yazi\Lider.CG.LiWork.LauncherSetTextUpperOrLower.pas',
  Lider.CG.LiWork.MatematikIslemleri in 'Yazi\Lider.CG.LiWork.MatematikIslemleri.pas' {fmMatematikIslemleriYeni},
  Lider.CG.LiWork.StringOperations in 'Yazi\Lider.CG.LiWork.StringOperations.pas',
  Lider.CG.LiWork.YaziParcalamaSecenekleri in 'Yazi\Lider.CG.LiWork.YaziParcalamaSecenekleri.pas' {fmYaziParcalamaSecenekleri},
  Lider.CG.LiWork.YuvarlamaSecenekleri in 'Yazi\Lider.CG.LiWork.YuvarlamaSecenekleri.pas' {fmYuvarlamaSecenekleri},
  Lider.CG.LiWork.LauncherPointFromText in 'Lider.CG.LiWork.LauncherPointFromText.pas',
  Lider.CG.LiWork.TextForm in 'Lider.CG.LiWork.TextForm.pas' {fmTextForm},
  Lider.CG.LiWork.LauncherPoligonNameFromText in 'Lider.CG.LiWork.LauncherPoligonNameFromText.pas',
  Lider.CG.Details.AlanBirlestirSecenekleri in 'Lider.CG.Details.AlanBirlestirSecenekleri.pas' {fmAlanBirlestirSecenekleri},
  Lider.CG.LiWork.LauncherUnionSameNamedPolys in 'Lider.CG.LiWork.LauncherUnionSameNamedPolys.pas';

{$R *.res}

// ilker ekleme
{$SetPEFlags $0020}
{$SetPEOptFlags $140}

{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

begin

end.




