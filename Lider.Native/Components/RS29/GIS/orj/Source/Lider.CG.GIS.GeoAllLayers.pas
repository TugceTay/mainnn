//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================

{$IFDEF DCC}
  unit GisAllLayers ;
  {$HPPEMIT '#pragma link "GisAllLayers"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ELSE}
  uses
    GisAllBasicLayers
    ,GisAllEnterpriseLayers

    ,GisLayerADF
    ,GisLayerCSV
    ,GisLayerDEM
    ,GisLayerGRD
    ,GisLayerPixelUDF
    ,GisLayerSGRD
    ,GisLayerVectorUDF
    ,GisLayerSqlGpkgSqlite

    {$IFNDEF GIS_MOBILE}
      ,GisLayerBIL
      ,GisLayerBT
      ,GisLayerCADRG
      ,GisLayerDGN
      ,GisLayerDLG
      ,GisLayerDTED
      ,GisLayerDWG
      ,GisLayerDXF
      ,GisLayerE00
      ,GisLayerGDF
      ,GisLayerGIF
      ,GisLayerGSHHS
      ,GisLayerIMG
      ,GisLayerLandXML
      ,GisLayerLAS
      ,GisLayerMVT
      ,GisLayerOSM
      ,GisLayerPLY
      ,GisLayerPNG
      ,GisLayerS57
      ,GisLayerSDTS
      ,GisLayerSRTM
      ,GisLayerSTL
      ,GisLayerTAB
      ,GisLayerTiger
      ,GisLayerVPF
      ,GisFilePixelStoreSqlite
  
      {$IFDEF MSWINDOWS}
        ,GisLayerAssimp
        ,GisLayerECW
        ,GisLayerFGDB
        ,GisLayerFME
        ,GisLayerGDAL
        ,GisLayerMrSID
        ,GisLayerOGR
      {$ENDIF}
      {$IFNDEF GIS_NOADO}
        ,GisFilePixelStoreAdo
        ,GisLayerPixelStoreAdo
        ,GisLayerSqlAdo
        ,GisLayerSqlGmAdo
        ,GisLayerSqlOgisAdo
        ,GisLayerSqlPgdbAdo
      {$ENDIF}
      {$IFNDEF GIS_NODB}
        ,GisFilePixelStoreDbx
        ,GisLayerPixelStoreDbx
        ,GisLayerSqlDbx
        ,GisLayerSqlGmDbx
        ,GisLayerSqlOgisDbx
        ,GisLayerSqlPgdbDbx
  
        {$IFDEF GIS_FIREDAC}
          ,GisFilePixelStoreFireDac
          ,GisLayerPixelStoreFireDac
          ,GisLayerSqlFireDac
          ,GisLayerSqlGmFireDac
          ,GisLayerSqlOgisFireDac
          ,GisLayerSqlPgdbFireDac
        {$ENDIF}
      {$ENDIF}
  
      {$IFNDEF GIS_NOADONET}
        ,GisFilePixelStoreAdoNet
        ,GisLayerPixelStoreAdoNet
        ,GisLayerSqlAdoNet
        ,GisLayerSqlOgisAdoNet
      {$ENDIF}

    {$ENDIF}
    ;
{$ENDIF}

implementation

//==================================== END =====================================
end.


