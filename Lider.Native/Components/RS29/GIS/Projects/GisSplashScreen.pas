//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
//    $USER:LICENSE$
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  TatukGIS Developer Kernel registration unit.
}
unit GisSplashScreen ;

interface

uses
  DesignIntf,
  ToolsAPI,
  Windows,
  Classes;


//##############################################################################
implementation

{$R GisSplashScreen_24x24.RES}

{$INCLUDE 'GisVersion.inc'}

var
  iAboutPluginIndex : Integer ;
initialization
  iAboutPluginIndex := (BorlandIDEServices As IOTAAboutBoxServices).AddPluginInfo(
    'TatukGIS Developer Kernel ' + GIS_VERSION,	
    'TatukGIS Developer Kernel for Delphi' +#13#10+
    GIS_VERSION +#13#10+
    GIS_COPYRIGHT +#13#10+
    'www.TatukGIS.com' +#13#10,
    LoadBitmap(HInstance, 'TGIS_LOGO32X32'),
    {$IFDEF GIS_TRIAL_DK}
      True, 'TRIAL' 
    {$ELSE}
      False
    {$ENDIF}
  ) ;  

  SplashScreenServices.AddPluginBitmap(
    'TatukGIS Developer Kernel ' + GIS_VERSION,
    LoadBitmap(HInstance, 'TGIS_LOGO'),
    {$IFDEF GIS_TRIAL_DK}
      True, 'TRIAL' 
    {$ELSE}
      False
    {$ENDIF}
  ) ;  
finalization
  // Remove Aboutbox Plugin Interface
  if iAboutPluginIndex > 0 then
    (BorlandIDEServices As IOTAAboutBoxServices).RemovePluginInfo(iAboutPluginIndex);

//==================================== END =====================================
end.

