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
{
  Basic layers.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoAllBasicLayers ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoAllBasicLayers"'}
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
    Lider.CG.GIS.GeoClasses
    ,Lider.CG.GIS.GeoResource
    
    ,Lider.CG.GIS.GeoAggregator
    ,Lider.CG.GIS.GeoLayerBMP
    ,Lider.CG.GIS.GeoLayerFLT
    ,Lider.CG.GIS.GeoLayerGML
    ,Lider.CG.GIS.GeoLayerGPX
    ,Lider.CG.GIS.GeoLayerJPG
    ,Lider.CG.GIS.GeoLayerJSON
    ,Lider.CG.GIS.GeoLayerKML
    ,Lider.CG.GIS.GeoLayerMIF
    ,Lider.CG.GIS.GeoLayerSHP
    ,Lider.CG.GIS.GeoLayerTIFF
    ,Lider.CG.GIS.GeoLayerWCS
    ,Lider.CG.GIS.GeoLayerWebTiles
    ,Lider.CG.GIS.GeoLayerWFS
    ,Lider.CG.GIS.GeoLayerWMS
    ,Lider.CG.GIS.GeoLayerWMTS
    ,Lider.CG.GIS.GeoLayerXYZ
    ,Lider.CG.GIS.GeoMVTStyler
    ,Lider.CG.GIS.GeoLayerPixelStoreSqlite
    ,Lider.CG.GIS.GeoLayerSqlOgisSqlite
    ,Lider.CG.GIS.GeoLayerSqlSqlite

    {$IFNDEF GIS_MOBILE}
    {$ENDIF}
    ; 
{$ENDIF}

implementation

//==================================== END =====================================
end.

