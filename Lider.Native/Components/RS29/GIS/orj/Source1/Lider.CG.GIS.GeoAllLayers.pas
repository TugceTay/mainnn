//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoAllLayers ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoAllLayers"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ELSE}
  uses
    Lider.CG.GIS.GeoAllBasicLayers
    ,Lider.CG.GIS.GeoAllEnterpriseLayers

    ,Lider.CG.GIS.GeoLayerADF
    ,Lider.CG.GIS.GeoLayerCSV
    ,Lider.CG.GIS.GeoLayerDEM
    ,Lider.CG.GIS.GeoLayerGRD
    ,Lider.CG.GIS.GeoLayerPixelUDF
    ,Lider.CG.GIS.GeoLayerSGRD
    ,Lider.CG.GIS.GeoLayerVectorUDF
    ,Lider.CG.GIS.GeoLayerSqlGpkgSqlite

    {$IFNDEF GIS_MOBILE}
      ,Lider.CG.GIS.GeoLayerBIL
      ,Lider.CG.GIS.GeoLayerBT
      ,Lider.CG.GIS.GeoLayerCADRG
      ,Lider.CG.GIS.GeoLayerDGN
      ,Lider.CG.GIS.GeoLayerDLG
      ,Lider.CG.GIS.GeoLayerDTED
      ,Lider.CG.GIS.GeoLayerDWG
      ,Lider.CG.GIS.GeoLayerDXF
      ,Lider.CG.GIS.GeoLayerE00
      ,Lider.CG.GIS.GeoLayerGDF
      ,Lider.CG.GIS.GeoLayerGIF
      ,Lider.CG.GIS.GeoLayerGSHHS
      ,Lider.CG.GIS.GeoLayerIMG
      ,Lider.CG.GIS.GeoLayerLandXML
      ,Lider.CG.GIS.GeoLayerLAS
      ,Lider.CG.GIS.GeoLayerMVT
      ,Lider.CG.GIS.GeoLayerOSM
      ,Lider.CG.GIS.GeoLayerPLY
      ,Lider.CG.GIS.GeoLayerPNG
      ,Lider.CG.GIS.GeoLayerS57
      ,Lider.CG.GIS.GeoLayerSDTS
      ,Lider.CG.GIS.GeoLayerSRTM
      ,Lider.CG.GIS.GeoLayerSTL
      ,Lider.CG.GIS.GeoLayerTAB
      ,Lider.CG.GIS.GeoLayerTiger
      ,Lider.CG.GIS.GeoLayerVPF
      ,Lider.CG.GIS.GeoFilePixelStoreSqlite
  
      {$IFDEF MSWINDOWS}
        ,Lider.CG.GIS.GeoLayerAssimp
        ,Lider.CG.GIS.GeoLayerECW
        ,Lider.CG.GIS.GeoLayerFGDB
        ,Lider.CG.GIS.GeoLayerFME
        ,Lider.CG.GIS.GeoLayerGDAL
        ,Lider.CG.GIS.GeoLayerMrSID
        ,Lider.CG.GIS.GeoLayerOGR
      {$ENDIF}
      {$IFNDEF GIS_NOADO}
        ,Lider.CG.GIS.GeoFilePixelStoreAdo
        ,Lider.CG.GIS.GeoLayerPixelStoreAdo
        ,Lider.CG.GIS.GeoLayerSqlAdo
        ,Lider.CG.GIS.GeoLayerSqlGmAdo
        ,Lider.CG.GIS.GeoLayerSqlOgisAdo
        ,Lider.CG.GIS.GeoLayerSqlPgdbAdo
      {$ENDIF}
      {$IFNDEF GIS_NODB}
        ,Lider.CG.GIS.GeoFilePixelStoreDbx
        ,Lider.CG.GIS.GeoLayerPixelStoreDbx
        ,Lider.CG.GIS.GeoLayerSqlDbx
        ,Lider.CG.GIS.GeoLayerSqlGmDbx
        ,Lider.CG.GIS.GeoLayerSqloGisDbx
        ,Lider.CG.GIS.GeoLayerSqlPgdbDbx
  
        {$IFDEF GIS_FIREDAC}
          ,Lider.CG.GIS.GeoFilePixelStoreFireDac
          ,Lider.CG.GIS.GeoLayerPixelStoreFireDac
          ,Lider.CG.GIS.GeoLayerSqlFireDac
          ,Lider.CG.GIS.GeoLayerSqlGmFireDac
          ,Lider.CG.GIS.GeoLayerSqlOgisFireDac
          ,Lider.CG.GIS.GeoLayerSqlPgdbFireDac
        {$ENDIF}
      {$ENDIF}
  
      {$IFNDEF GIS_NOADONET}
        ,Lider.CG.GIS.GeoFilePixelStoreAdoNet
        ,Lider.CG.GIS.GeoLayerPixelStoreAdoNet
        ,Lider.CG.GIS.GeoLayerSqlAdoNet
        ,Lider.CG.GIS.GeoLayerSqlOgisAdoNet
      {$ENDIF}

    {$ENDIF}
    ;
{$ENDIF}

implementation

//==================================== END =====================================
end.


