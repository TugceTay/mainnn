//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  General include file for compiler specific support.
}
{$IFNDEF GIS_SHOW_WARNINGS}
  {$IFDEF DCC}
    {$WARNINGS OFF}
    {$HINTS OFF}
  {$ENDIF}
{$ENDIF}

// Delphi XE2 (RAD XE2)
{$IFDEF VER230}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_XE2}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}

  {$DEFINE GIS_3D}

  {$DEFINE GIS_INLINE}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE2.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE2_VCL.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE2" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE2_VCL" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}


// Delphi XE3 (RAD XE3)
{$IFDEF VER240}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_XE3}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}

  {$DEFINE GIS_3D}

  {$DEFINE GIS_INLINE}
  {$DEFINE GIS_INLINE_BUG}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE3.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE3_VCL.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE3" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE3_VCL" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi XE4 (RAD XE4)
{$IFDEF VER250}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_XE4}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}

  {$DEFINE GIS_3D}

  {$DEFINE GIS_INLINE}
  {$DEFINE GIS_INLINE_BUG}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '   #pragma link "TatukGIS_DK_XE4.bpi"'}
  {$HPPEMIT '   #pragma link "TatukGIS_DK_XE4_VCL.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '   #pragma comment( lib, "TatukGIS_DK_XE4" )'}
  {$HPPEMIT '   #pragma comment( lib, "TatukGIS_DK_XE4_VCL" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi XE5 (RAD XE5)
{$IFDEF VER260}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_XE5}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}

  {$DEFINE GIS_3D}

  {$DEFINE GIS_INLINE}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE5.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE5_VCL.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE5" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE5_VCL" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi XE6 (RAD XE6)
{$IFDEF VER270}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_XE6}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}

  {$DEFINE GIS_3D}

  {$DEFINE GIS_INLINE}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE6.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE6_VCL.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE6" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE6_VCL" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi XE7 (RAD XE7)
{$IFDEF VER280}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_XE7}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}
  {$DEFINE LEVEL_XE7_RTL}
  {$DEFINE LEVEL_XE7_VCL}
  {$DEFINE LEVEL_XE7_FMX}

  {$DEFINE GIS_3D}
  {$DEFINE GIS_FMX_SUPPORTED}

  {$DEFINE GIS_INLINE}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE7.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE7_VCL.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE7_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE7" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE7_VCL" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE7_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE7.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE7_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE7" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE7_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi XE8 (RAD XE8)
{$IFDEF VER290}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_XE8}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}
  {$DEFINE LEVEL_XE7_RTL}
  {$DEFINE LEVEL_XE7_VCL}
  {$DEFINE LEVEL_XE7_FMX}
  {$DEFINE LEVEL_XE8_RTL}
  {$DEFINE LEVEL_XE8_VCL}
  {$DEFINE LEVEL_XE8_FMX}

  {$DEFINE GIS_3D}
  {$DEFINE GIS_FMX_SUPPORTED}

  {$DEFINE GIS_INLINE}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE8.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE8_VCL.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE8_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE8" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE8_VCL" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE8_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE8.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_XE8_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE8" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_XE8_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi DX10 (RAD RX10 Seattle)
{$IFDEF VER300}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_RX10}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}
  {$DEFINE LEVEL_XE7_RTL}
  {$DEFINE LEVEL_XE7_VCL}
  {$DEFINE LEVEL_XE7_FMX}
  {$DEFINE LEVEL_XE8_RTL}
  {$DEFINE LEVEL_XE8_VCL}
  {$DEFINE LEVEL_XE8_FMX}
  {$DEFINE LEVEL_RX10_RTL}
  {$DEFINE LEVEL_RX10_VCL}
  {$DEFINE LEVEL_RX10_FMX}

  {$DEFINE GIS_3D}
  {$DEFINE GIS_FMX_SUPPORTED}

  {$DEFINE GIS_INLINE}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX10.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX10_VCL.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX10_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX10" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX10_VCL" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX10_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX10.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX10_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX10" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX10_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi DX10.1 (RAD RX10.1 Berlin)
{$IFDEF VER310}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_RX101}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}
  {$DEFINE LEVEL_XE7_RTL}
  {$DEFINE LEVEL_XE7_VCL}
  {$DEFINE LEVEL_XE7_FMX}
  {$DEFINE LEVEL_XE8_RTL}
  {$DEFINE LEVEL_XE8_VCL}
  {$DEFINE LEVEL_XE8_FMX}
  {$DEFINE LEVEL_RX10_RTL}
  {$DEFINE LEVEL_RX10_VCL}
  {$DEFINE LEVEL_RX10_FMX}
  {$DEFINE LEVEL_RX101_RTL}
  {$DEFINE LEVEL_RX101_VCL}
  {$DEFINE LEVEL_RX101_FMX}

  {$DEFINE GIS_3D}
  {$DEFINE GIS_FMX_SUPPORTED}

  {$DEFINE GIS_INLINE}
  {$DEFINE USE_NEW_HTTP_CLIENT}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX101.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX101_VCL.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX101_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX101" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX101_VCL" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX101_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX101.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX101_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX101" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX101_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi DX10.2 (RAD RX10.2 Tokyo)
{$IFDEF VER320}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_RX102}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}
  {$DEFINE LEVEL_XE7_RTL}
  {$DEFINE LEVEL_XE7_VCL}
  {$DEFINE LEVEL_XE7_FMX}
  {$DEFINE LEVEL_XE8_RTL}
  {$DEFINE LEVEL_XE8_VCL}
  {$DEFINE LEVEL_XE8_FMX}
  {$DEFINE LEVEL_RX10_RTL}
  {$DEFINE LEVEL_RX10_VCL}
  {$DEFINE LEVEL_RX10_FMX}
  {$DEFINE LEVEL_RX101_RTL}
  {$DEFINE LEVEL_RX101_VCL}
  {$DEFINE LEVEL_RX101_FMX}
  {$DEFINE LEVEL_RX102_RTL}
  {$DEFINE LEVEL_RX102_VCL}
  {$DEFINE LEVEL_RX102_FMX}

  {$DEFINE GIS_3D}
  {$DEFINE GIS_FMX_SUPPORTED}

  {$DEFINE GIS_INLINE}
  {$DEFINE USE_NEW_HTTP_CLIENT}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX102.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX102_VCL.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX102_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX102" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX102_VCL" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX102_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX102.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX102_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX102" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX102_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi DX10.3 (RAD RX10.3 Tokyo)
{$IFDEF VER330}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_RX103}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}
  {$DEFINE LEVEL_XE7_RTL}
  {$DEFINE LEVEL_XE7_VCL}
  {$DEFINE LEVEL_XE7_FMX}
  {$DEFINE LEVEL_XE8_RTL}
  {$DEFINE LEVEL_XE8_VCL}
  {$DEFINE LEVEL_XE8_FMX}
  {$DEFINE LEVEL_RX10_RTL}
  {$DEFINE LEVEL_RX10_VCL}
  {$DEFINE LEVEL_RX10_FMX}
  {$DEFINE LEVEL_RX101_RTL}
  {$DEFINE LEVEL_RX101_VCL}
  {$DEFINE LEVEL_RX101_FMX}
  {$DEFINE LEVEL_RX102_RTL}
  {$DEFINE LEVEL_RX102_VCL}
  {$DEFINE LEVEL_RX102_FMX}
  {$DEFINE LEVEL_RX103_RTL}
  {$DEFINE LEVEL_RX103_VCL}
  {$DEFINE LEVEL_RX103_FMX}

  {$DEFINE LEVEL_RX103_DIRECT2DFIX}

  {$DEFINE GIS_3D}
  {$DEFINE GIS_FMX_SUPPORTED}

  {$DEFINE GIS_INLINE}
  {$DEFINE USE_NEW_HTTP_CLIENT}


  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX103.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX103_VCL.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX103_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX103" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX103_VCL" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX103_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX103.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX103_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX103" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX103_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi DX10.4 (RAD RX10.4 Sydney)
{$IFDEF VER340}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_RX104}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}
  {$DEFINE LEVEL_XE7_RTL}
  {$DEFINE LEVEL_XE7_VCL}
  {$DEFINE LEVEL_XE7_FMX}
  {$DEFINE LEVEL_XE8_RTL}
  {$DEFINE LEVEL_XE8_VCL}
  {$DEFINE LEVEL_XE8_FMX}
  {$DEFINE LEVEL_RX10_RTL}
  {$DEFINE LEVEL_RX10_VCL}
  {$DEFINE LEVEL_RX10_FMX}
  {$DEFINE LEVEL_RX101_RTL}
  {$DEFINE LEVEL_RX101_VCL}
  {$DEFINE LEVEL_RX101_FMX}
  {$DEFINE LEVEL_RX102_RTL}
  {$DEFINE LEVEL_RX102_VCL}
  {$DEFINE LEVEL_RX102_FMX}
  {$DEFINE LEVEL_RX103_RTL}
  {$DEFINE LEVEL_RX103_VCL}
  {$DEFINE LEVEL_RX103_FMX}
  {$DEFINE LEVEL_RX104_RTL}
  {$DEFINE LEVEL_RX104_VCL}
  {$DEFINE LEVEL_RX104_FMX}

  {$DEFINE GIS_3D}
  {$DEFINE GIS_FMX_SUPPORTED}

  {$DEFINE GIS_INLINE}
  {$DEFINE USE_NEW_HTTP_CLIENT}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX104.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX104_VCL.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX104_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX104" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX104_VCL" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX104_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX104.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX104_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX104" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX104_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi DX11 (RAD RX11 Alexandria)
{$IFDEF VER350}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_RX11}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}
  {$DEFINE LEVEL_XE7_RTL}
  {$DEFINE LEVEL_XE7_VCL}
  {$DEFINE LEVEL_XE7_FMX}
  {$DEFINE LEVEL_XE8_RTL}
  {$DEFINE LEVEL_XE8_VCL}
  {$DEFINE LEVEL_XE8_FMX}
  {$DEFINE LEVEL_RX10_RTL}
  {$DEFINE LEVEL_RX10_VCL}
  {$DEFINE LEVEL_RX10_FMX}
  {$DEFINE LEVEL_RX101_RTL}
  {$DEFINE LEVEL_RX101_VCL}
  {$DEFINE LEVEL_RX101_FMX}
  {$DEFINE LEVEL_RX102_RTL}
  {$DEFINE LEVEL_RX102_VCL}
  {$DEFINE LEVEL_RX102_FMX}
  {$DEFINE LEVEL_RX103_RTL}
  {$DEFINE LEVEL_RX103_VCL}
  {$DEFINE LEVEL_RX103_FMX}
  {$DEFINE LEVEL_RX104_RTL}
  {$DEFINE LEVEL_RX104_VCL}
  {$DEFINE LEVEL_RX104_FMX}
  {$DEFINE LEVEL_RX11_RTL}
  {$DEFINE LEVEL_RX11_VCL}
  {$DEFINE LEVEL_RX11_FMX}

  {$DEFINE GIS_3D}
  {$DEFINE GIS_FMX_SUPPORTED}

  {$DEFINE GIS_INLINE}
  {$DEFINE USE_NEW_HTTP_CLIENT}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX11.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX11_VCL.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX11_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX11" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX11_VCL" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX11_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX11.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX11_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX11" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX11_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// Delphi DX12 (RAD RX12)
{$IFDEF VER360}
  {$DEFINE SUPPORTED}

  {$DEFINE RES16x16}
  {$DEFINE ADOINTUNIT}

  {$DEFINE VER_RX11}

  {$DEFINE LEVEL_XE2_RTL}
  {$DEFINE LEVEL_XE2_VCL}
  {$DEFINE LEVEL_XE2_FMX}
  {$DEFINE LEVEL_XE3_RTL}
  {$DEFINE LEVEL_XE3_VCL}
  {$DEFINE LEVEL_XE3_FMX}
  {$DEFINE LEVEL_XE4_RTL}
  {$DEFINE LEVEL_XE4_VCL}
  {$DEFINE LEVEL_XE4_FMX}
  {$DEFINE LEVEL_XE5_RTL}
  {$DEFINE LEVEL_XE5_VCL}
  {$DEFINE LEVEL_XE5_FMX}
  {$DEFINE LEVEL_XE6_RTL}
  {$DEFINE LEVEL_XE6_VCL}
  {$DEFINE LEVEL_XE6_FMX}
  {$DEFINE LEVEL_XE7_RTL}
  {$DEFINE LEVEL_XE7_VCL}
  {$DEFINE LEVEL_XE7_FMX}
  {$DEFINE LEVEL_XE8_RTL}
  {$DEFINE LEVEL_XE8_VCL}
  {$DEFINE LEVEL_XE8_FMX}
  {$DEFINE LEVEL_RX10_RTL}
  {$DEFINE LEVEL_RX10_VCL}
  {$DEFINE LEVEL_RX10_FMX}
  {$DEFINE LEVEL_RX101_RTL}
  {$DEFINE LEVEL_RX101_VCL}
  {$DEFINE LEVEL_RX101_FMX}
  {$DEFINE LEVEL_RX102_RTL}
  {$DEFINE LEVEL_RX102_VCL}
  {$DEFINE LEVEL_RX102_FMX}
  {$DEFINE LEVEL_RX103_RTL}
  {$DEFINE LEVEL_RX103_VCL}
  {$DEFINE LEVEL_RX103_FMX}
  {$DEFINE LEVEL_RX104_RTL}
  {$DEFINE LEVEL_RX104_VCL}
  {$DEFINE LEVEL_RX104_FMX}
  {$DEFINE LEVEL_RX11_RTL}
  {$DEFINE LEVEL_RX11_VCL}
  {$DEFINE LEVEL_RX11_FMX}
  {$DEFINE LEVEL_RX12_RTL}
  {$DEFINE LEVEL_RX12_VCL}
  {$DEFINE LEVEL_RX12_FMX}

  {$DEFINE GIS_3D}
  {$DEFINE GIS_FMX_SUPPORTED}

  {$DEFINE GIS_INLINE}
  {$DEFINE USE_NEW_HTTP_CLIENT}

  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}

  {$HPPEMIT '#if _Windows'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX12.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX12_VCL.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX12_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX12" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX12_VCL" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX12_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
  {$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX12.bpi"'}
  {$HPPEMIT '    #pragma link "TatukGIS_DK_RX12_FMX.bpi"'}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX12" )'}
  {$HPPEMIT '    #pragma comment( lib, "TatukGIS_DK_RX12_FMX" )'}
  {$HPPEMIT '  #endif'}
  {$HPPEMIT '#endif'}
{$ENDIF}

// for all Windows C++
{$HPPEMIT '#if _Windows'}
{$HPPEMIT '  #pragma comment( lib, "winhttp" )'}
{$HPPEMIT '  #pragma comment( lib, "crypt32" )'}
{$IFNDEF GIS_NODB}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "dbexpress.bpi"'}
  {$HPPEMIT '    #pragma link "DBXCommonDriver.bpi"'}
  {$IFDEF LEVEL_XE5_RTL}
    {$HPPEMIT '    #pragma link "FireDAC.bpi"'}
    {$HPPEMIT '    #pragma link "FireDACCommonDriver.bpi"'}
    {$HPPEMIT '    #pragma link "FireDACCommon.bpi"'}
    {$HPPEMIT '    #pragma link "xmlrtl.bpi"'}
  {$ENDIF}
  {$IFNDEF LEVEL_XE3_RTL}
    {$HPPEMIT '    #pragma link "dsnap.bpi"'}
  {$ENDIF}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "dbexpress" )'}
  {$HPPEMIT '    #pragma comment( lib, "DBXCommonDriver" )'}
  {$IFDEF LEVEL_XE5_RTL}
    {$HPPEMIT '    #pragma comment( lib, "FireDAC" )'}
    {$HPPEMIT '    #pragma comment( lib, "FireDACCommonDriver" )'}
    {$HPPEMIT '    #pragma comment( lib, "FireDACCommon" )'}
    {$HPPEMIT '    #pragma comment( lib, "xmlrtl" )'}
  {$ENDIF}
  {$IFNDEF LEVEL_XE3_RTL}
    {$HPPEMIT '    #pragma comment( lib, "dsnap" )'}
  {$ENDIF}
  {$HPPEMIT '  #endif'}
{$ENDIF}
{$HPPEMIT '#endif'}


// for OSX C++
{$HPPEMIT '#if __APPLE__ && !(__arm__ || __arm64__)'}
{$IFNDEF GIS_NODB}
  {$HPPEMIT '  #ifdef USEPACKAGES'}
  {$HPPEMIT '    #pragma link "dbexpress,bpi"'}
  {$HPPEMIT '    #pragma link "DBXCommonDriver.bpi"'}
  {$IFDEF LEVEL_XE5_RTL}
    {$HPPEMIT '    #pragma link "FireDAC,bpi"'}
    {$HPPEMIT '    #pragma link "FireDACCommonDriver.bpi"'}
    {$HPPEMIT '    #pragma link "FireDACCommon.bpi"'}
    {$HPPEMIT '    #pragma link "xmlrtl.bpi"'}
  {$ENDIF}
  {$HPPEMIT '  #else'}
  {$HPPEMIT '    #pragma comment( lib, "dbexpress" )'}
  {$HPPEMIT '    #pragma comment( lib, "DBXCommonDriver" )'}
  {$IFDEF LEVEL_XE5_RTL}
    {$HPPEMIT '    #pragma comment( lib, "FireDAC" )'}
    {$HPPEMIT '    #pragma comment( lib, "FireDACCommonDriver" )'}
    {$HPPEMIT '    #pragma comment( lib, "FireDACCommon" )'}
    {$HPPEMIT '    #pragma comment( lib, "xmlrtl" )'}
  {$ENDIF}
  {$HPPEMIT '  #endif'}
{$ENDIF}
{$HPPEMIT '#endif'}

{$IFNDEF LEVEL_XE6_RTL}
  {$HPPEMIT '#ifndef USEPACKAGES'}
  {$HPPEMIT '  #pragma alias "toupper"    = "_toupper"'     }
  {$HPPEMIT '  #pragma alias "tolower"    = "_tolower"'     }
  {$HPPEMIT '  #pragma alias "isupper"    = "_isupper"'     }
  {$HPPEMIT '  #pragma alias "isalnum"    = "_isalnum"'     }
  {$HPPEMIT '  #pragma alias "isalpha"    = "_isalpha"'     }
  {$HPPEMIT '  #pragma alias "iscntrl"    = "_iscntrl"'     }
  {$HPPEMIT '  #pragma alias "isdigit"    = "_isdigit"'     }
  {$HPPEMIT '  #pragma alias "isgraph"    = "_isgraph"'     }
  {$HPPEMIT '  #pragma alias "isleadbyte" = "_isleadbyte"'  }
  {$HPPEMIT '  #pragma alias "islower"    = "_islower"'     }
  {$HPPEMIT '  #pragma alias "isprint"    = "_isprint"'     }
  {$HPPEMIT '  #pragma alias "ispunct"    = "_ispunct"'     }
  {$HPPEMIT '  #pragma alias "isspace"    = "_isspace"'     }
  {$HPPEMIT '  #pragma alias "isxdigit"   = "_isxdigit"'    }
  {$HPPEMIT '  #pragma alias "strchr"     = "_strchr"'      }
  {$HPPEMIT '  #pragma alias "strncmp"    = "_strncmp"'     }
  {$HPPEMIT '  #pragma alias "memcmp"     = "_memcmp"'      }
  {$HPPEMIT '  #pragma alias "memmove"    = "_memmove"'     }
  {$HPPEMIT '#endif'}
{$ENDIF}

{$IFDEF ISLAND}
  {$DEFINE OXYGENE}
  {$DEFINE GIS_NODB}
  {$DEFINE GIS_NOJDBC}
  {$DEFINE GIS_NOADO_JOIN}
  {$DEFINE GIS_NOADO}
  {$DEFINE GIS_NOWITH}
{$ENDIF}

{$IFDEF OXYGENE}
  {$DEFINE SUPPORTED}
  {$DEFINE GIS_NOASM}
  {$DEFINE GIS_NOOVERLOAD}
  {$DEFINE GIS_NOINLINE}
  {$DEFINE GIS_NOPOINTERS}
  {$DEFINE MANAGED}
{$ENDIF}

// Test if supported version
{$IFDEF DCC}
  {$IFNDEF SUPPORTED}
    {$MESSAGE 'NOT SUPPORTED VERSION OF DELPHI / C++ BUILDER. AT LEAST VERSION 2009 IS REQUIRED.'}
  {$ENDIF}
{$ENDIF}

{$SCOPEDENUMS ON}

{$IFDEF CLR}
  {$IFNDEF SUPPORTED}
    {$MESSAGE 'NOT SUPPORTED VERSION OF PASCAL. OXYGENE for .NET IS REQUIRED.'}
  {$ENDIF}
  {$DEFINE GIS_NODB}
  {$DEFINE GIS_NOJDBC}
  {$DEFINE GIS_NOADO_JOIN}
  {$DEFINE UNKNOWN_OS}

  {$IFDEF MSWINDOWS}
    {$DEFINE MSWINDOWS_OS}
    {$UNDEF UNKNOWN_OS}
  {$ENDIF}

  {$IFDEF IOS}
    {$DEFINE IOS_OS}
    {$UNDEF UNKNOWN_OS}
    {$DEFINE GIS_NOADO}
  {$ENDIF}

  {$IFDEF LINUX}
    {$DEFINE LINUX_OS}
    {$UNDEF UNKNOWN_OS}
    {$DEFINE GIS_NOADO}
  {$ENDIF}

  {$IFDEF MACOSX}
    {$DEFINE MACOSX_OS}
    {$UNDEF UNKNOWN_OS}
    {$DEFINE GIS_NOADO}
  {$ENDIF}

  {$IFDEF ANDROID}
    {$DEFINE ANDROID_OS}
    {$UNDEF UNKNOWN_OS}
    {$DEFINE GIS_NOADO}
  {$ENDIF}

  {$IFDEF UNKNOWN_OS}
    {$DEFINE GIS_NOADO}
  {$ENDIF}

{$ENDIF}

{$IFDEF JAVA}
  {$IFNDEF SUPPORTED}
    {$MESSAGE 'NOT SUPPORTED VERSION OF PASCAL. OXYGENE for JAVA IS REQUIRED.'}
  {$ENDIF}
  {$DEFINE GIS_NOASM}
  {$DEFINE GIS_NODB}
  {$DEFINE GIS_NOADO}
  {$DEFINE GIS_NOWITH}
{$ENDIF}

{$IFDEF COCOA}
  {$IFNDEF SUPPORTED}
    {$MESSAGE 'NOT SUPPORTED VERSION OF PASCAL. OXYGENE for COCOA IS REQUIRED.'}
  {$ENDIF}
  {$DEFINE GIS_NODB}
  {$DEFINE GIS_NOADO}
  {$DEFINE GIS_NOJDBC}
  {$DEFINE GIS_NOWITH}
{$ENDIF}

{$IFDEF DCC}
  {$IFDEF IOS}
    {$DEFINE GIS_NOADO}
    {$DEFINE GIS_NOADONET}
    {$DEFINE GIS_NOPOINTERS}
    {$DEFINE GIS_NOASM}
  {$DEFINE GIS_NOJDBC}
    {$ZEROBASEDSTRINGS OFF}
  {$ENDIF}
  {$IFDEF ANDROID}
    {$DEFINE GIS_NOADONET}
    {$DEFINE GIS_NOADO}
    {$DEFINE GIS_NOPOINTERS}
    {$DEFINE GIS_NOASM}
  {$DEFINE GIS_NOJDBC}
    {$ZEROBASEDSTRINGS OFF}
  {$ENDIF}
{$ENDIF}

// Various setup
{$IFDEF GIS_NOLZW}
  {$DEFINE GIS_NOLZWREAD}
  {$DEFINE GIS_NOLZWWRITE}
{$ENDIF}

{$IFDEF CPUX64}
  {$DEFINE GIS_NOASM}
{$ENDIF}

{$IFDEF GIS_SCRIPT_PARSER}
  {$DEFINE GIS_NOOVERLOAD}
{$ENDIF}

{$IFDEF DCC}
  {$IFNDEF MSWINDOWS}
    {$DEFINE GIS_NOADO}
  {$ELSE}
    {$DEFINE MSWINDOWS_OS}
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFNDEF IOS}
      {$DEFINE MACOSX}
      {$DEFINE MACOSX_OS}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF IOS}
    {$DEFINE GIS_MOBILE}
    {$IFDEF CPUARM}
      {$IFDEF IOSSIMULATOR}
        {$DEFINE IOS_SIMULATOR}
      {$ELSE}
        {$DEFINE IOS_DEVICE}
      {$ENDIF}
    {$ELSE}
      {$DEFINE IOS_SIMULATOR}
    {$ENDIF}
    {$DEFINE IOS_OS}
  {$ENDIF}

  {$IFDEF LINUX}
    {$DEFINE GIS_NOADO}
    {$DEFINE GIS_NOADONET}
    {$DEFINE LINUX_OS}
  {$ENDIF}

  {$IFDEF MACOSX}
    {$DEFINE GIS_NOADO}
    {$DEFINE MACOSX_OS}
  {$ENDIF}

  {$IFDEF ANDROID}
    {$DEFINE GIS_MOBILE}
    {$DEFINE ANDROID_OS}
  {$ENDIF}
{$ENDIF}


{$IFDEF GIS_MOBILE}
  {$DEFINE GIS_MOBILE_DIALOGS}
{$ENDIF}

{$IFDEF JAVA}
  {$DEFINE GIS_NORECORDS}
{$ENDIF}

{$IFDEF DCC}
  {$DEFINE GIS_DK10VCL_COMPATIBILITY}
  {$DEFINE GIS_NOJDBC}
{$ENDIF}

{$IFDEF FMX}
  {$MESSAGE ERROR 'avoid conditional compilation flag FMX'}
{$ENDIF}

{$IFDEF LEVEL_XE5_RTL}
  {$DEFINE GIS_FIREDAC}
{$ENDIF}

{$IFDEF GIS_NOADONET}
  {$DEFINE GIS_NOADONET_JOIN}
{$ENDIF}

{$IFDEF GIS_NOADO}
  {$DEFINE GIS_NOADO_JOIN}
{$ENDIF}

{$IFDEF GIS_NODB}
  {$DEFINE GIS_NODB_JOIN}
{$ENDIF}

{$IFDEF GIS_EDITOR}
  {$DEFINE GIS_NOGENERICS}
{$ENDIF}

{$IFDEF GIS_PDK}
  {$DEFINE GIS_NOGENERICS}
{$ENDIF}

//==================================== END =====================================
