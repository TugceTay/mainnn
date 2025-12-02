unit Lider.CG.Com.ModulesConsts;

interface

uses
  Classes, Forms, ActiveX;

type
  TModulePassParams = array of Pointer;
  PModulePassParams = TModulePassParams;
  TModulePassSkins = array[1..30] of Pointer;

  TModuleSkinChangeEventProc = procedure(const ASkinStream: IStream; const ASkinName: widestring); stdcall;
  TSetSkinChangeEventToModulePassParamsProc = procedure(AParams: PModulePassParams); stdcall;

  // 0 : OnClose
  // 1 : PModulePassSkins

{$J+}
const
  ModulePassParams: PModulePassParams = nil;
{$J-}

var
  ModulePassSkins: TModulePassSkins;

const
  // Modül dosya tanýmlarý
  ModuleModulesDLLName = 'Lider.CG.Modules.dll' + #0;
  ModuleCoreDLLName = 'Lider.CG.Core.dll' + #0;
  ModuleCADDLLName = 'Lider.CG.CAD.dll' + #0;
  ModuleGISDLLName = 'Lider.CG.GIS.dll' + #0;
  ModuleExpressionDLLName = 'Lider.CG.Expression.dll' + #0;
  ModuleDetailsDLLName = 'Lider.CG.Details.dll' + #0;
  //ModuleReferenceDLLName = 'Lider.CG.Reference.dll' + #0;
  ModuleTopologyDLLName = 'Lider.CG.Topology.dll' + #0;
  ModuleIEDLLName = 'Lider.CG.IE.dll' + #0;
  ModuleIE2DLLName = 'Lider.CG.IE2.dll' + #0;
  ModuleDBGISDLLName = 'Lider.CG.DBGIS.dll' + #0;
  ModuleLiSurveyDLLName = 'Modules\LiSurvey\Lider.CG.LiSurvey.dll' + #0;
  ModuleLiMapDLLName = 'Modules\LiMap\Lider.CG.LiMap.dll' + #0;
  ModuleLiMarDLLName = 'Modules\LiMar\Lider.CG.LiMar.dll' + #0;
  ModuleLiSurfDLLName = 'Modules\LiSurf\Lider.CG.LiSurf.dll' + #0;
  ModuleLiPlanDLLName = 'Modules\LiPlan\Lider.CG.LiPlan.dll' + #0;
  ModuleLiTopDLLName = 'Modules\LiTop\Lider.CG.LiTop.dll' + #0;

  ModuleEditorDLLName = 'LicadEditor.dll' + #0;
  ModuleReportDLLName = 'LicadReport.dll' + #0;

  ModuleLiKasDLLName = 'Modules\LiKas\Lider.CG.LiKas.dll' + #0;
  ModuleLiTesDLLName = 'Modules\LiTes\Lider.CG.LiTes.dll' + #0;

  ModuleLiKrokiDLLName = 'Modules\LiKroki\Lider.CG.LiKroki.dll' + #0;
  ModuleLiWorkDLLName = 'Modules\LiWork\Lider.CG.LiWork.dll' + #0;
  ModuleMEGSISDLLName = 'Modules\MEGSIS\Lider.CG.MEGSIS.dll' + #0;
  ModuleLiCityDLLName = 'Modules\LiCity\Lider.CG.LiCity.dll' + #0;

  ModuleMainEXEName = 'LicadGIS.exe' + #0; // Kullanýlmýyor

  // Modül adý tanýmlarý
  ModuleModulesName = 'Modules';
  ModuleCoreName = 'ModulesCore';
  ModuleCADName = 'ModulesCAD';
  ModuleGISName = 'ModulesGIS';
  ModuleExpressionName = 'ModulesExpression';
  ModuleDetailsName = 'ModulesDetails';
  ModuleReferenceName = 'ModulesReference';
  ModuleTopologyName = 'ModulesTopology';
  ModuleIEName = 'ModulesIE';
  ModuleIE2Name = 'ModulesIE2';
  ModuleDBGISName = 'ModulesDBGIS';
  ModuleLiSurveyName = 'ModulesLiSurvey';
  ModuleLiMapName = 'ModulesLiMap';
  ModuleLiMarName = 'ModulesLiMar';
  ModuleLiSurfName = 'ModulesLiSurf';
  ModuleLiPlanName = 'ModulesLiPlan';
  ModuleLiTopName = 'ModulesLiTop';
  ModuleLiKasName = 'ModulesLiKas';
  ModuleLiTesName = 'ModulesLiTes';
  ModuleLiKrokiName = 'ModulesLiKroki';
  ModuleLiWorkName = 'ModulesLiWork';
  ModuleMEGSISName = 'ModulesMEGSIS';
  ModuleLiCityName = 'ModulesLiCity';
  ModuleMainName = 'ModulesMain';

  // Skin için tanýmlamalar
  SkinModuleDetailsID = 1;
  SkinModuleTopologyID = 2;
  SkinModuleIEID = 3;
  SkinModuleIE2ID = 4;
  SkinModuleDBGISID = 5; // Kullanýlmýyor
  SkinModuleLiSurveyID = 6;
  SkinModuleLiMapID = 7;
  SkinModuleLiSurfID = 8;
  SkinModuleLiMarID = 9;
  SkinModuleLiPlanID = 10;
  SkinModuleLiKasID = 11;
  SkinModuleLiTesID = 12;
  SkinModuleLiKrokiID = 13;
  SkinModuleLiTopID = 14;
  SkinModuleLiWorkID = 15;
  SkinModuleMEGSISID = 16;
  SkinModuleLiCityID = 17;

implementation

end.


