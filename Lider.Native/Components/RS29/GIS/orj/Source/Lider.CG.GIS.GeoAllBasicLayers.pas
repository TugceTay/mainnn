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
{
  Basic layers.
}

{$IFDEF DCC}
  unit GisAllBasicLayers ;
  {$HPPEMIT '#pragma link "GisAllBasicLayers"'}
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
    GisClasses
    ,GisResource
    
    ,GisAggregator
    ,GisLayerBMP
    ,GisLayerFLT
    ,GisLayerGML
    ,GisLayerGPX
    ,GisLayerJPG
    ,GisLayerJSON
    ,GisLayerKML
    ,GisLayerMIF
    ,GisLayerSHP
    ,GisLayerTIFF
    ,GisLayerWCS
    ,GisLayerWebTiles
    ,GisLayerWFS
    ,GisLayerWMS
    ,GisLayerWMTS
    ,GisLayerXYZ
    ,GisMVTStyler
    ,GisLayerPixelStoreSqlite
    ,GisLayerSqlOgisSqlite
    ,GisLayerSqlSqlite

    {$IFNDEF GIS_MOBILE}
    {$ENDIF}
    ; 
{$ENDIF}

implementation

//==================================== END =====================================
end.

