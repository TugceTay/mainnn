//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
//    ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Skia based renderer. Stub file.
}

unit VCL.GisRendererSkia.Stub ;

{$INCLUDE GisInclude.inc}

interface

{$IFDEF SKIA}
  {$IFDEF LEVEL_RX12_VCL}
    {$DEFINE GIS_SKIA_SUPPORT}
    {$DEFINE GIS_SKIA_VERSION_6}
  {$ELSE}
  //  {$DEFINE GIS_SKIA_SUPPORT}
  //  {$DEFINE GIS_SKIA_VERSION_6}
  {$ENDIF}
{$ENDIF}

uses
  VCL.GisRendererSkia.Common

  {$IFDEF GIS_SKIA_SUPPORT}
    ,VCL.GisRendererSkia
 //   {$NOINCLUDE VCL.GisRendererSkia}
  {$ENDIF}
  ;

  {$HPPEMIT '#ifdef SKIA'}
    {$HPPEMIT ' #include <VCL.GisRendererSkia.hpp>'}
  {$HPPEMIT '#endif'}

implementation
end.


