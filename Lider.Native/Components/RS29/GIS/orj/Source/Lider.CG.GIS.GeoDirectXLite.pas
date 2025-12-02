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
{$IFNDEF GENDOC}
unit GisDirectXLite ;

interface

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils ;

{$ALIGN ON}

//==============================================================================
// Constants
//==============================================================================

const
  D3D_SDK_VERSION                         = 32 or $80000000;

  D3DCREATE_FPU_PRESERVE                  = $00000002;
  D3DCREATE_SOFTWARE_VERTEXPROCESSING     = $00000020;
  D3DCREATE_HARDWARE_VERTEXPROCESSING     = $00000040;

  D3DX_DEFAULT                            = Cardinal(-1);

  D3DADAPTER_DEFAULT                      =   0;

  D3DFMT_UNKNOWN                          =   0;
  D3DFMT_A8R8G8B8                         =  21;
  D3DFMT_X8R8G8B8                         =  22;
  D3DFMT_D24S8                            =  75;
  D3DFMT_D16                              =  80;
  D3DFMT_INDEX16                          = 101;
  D3DFMT_INDEX32                          = 102;

  D3DX_FILTER_NONE                        = (1 shl 0);
  D3DX_FILTER_LINEAR                      = (3 shl 0);

  D3DFVF_XYZ                              = $002;
  D3DFVF_NORMAL                           = $010;
  D3DFVF_DIFFUSE                          = $040;
  D3DFVF_TEX1                             = $100;

  D3DPRESENT_INTERVAL_DEFAULT             = $00000000;
  D3DCLEAR_TARGET                         = $00000001;
  D3DCLEAR_ZBUFFER                        = $00000002;

  D3DTEXF_NONE                            =   0;
  D3DTEXF_LINEAR                          =   2;
  D3DTEXF_ANISOTROPIC                     =   3;

  D3DTADDRESS_WRAP                        =   1;

  D3DTS_VIEW                              =   2;
  D3DTS_PROJECTION                        =   3;
  D3DTS_WORLD                             =   0 + 256;

  D3DFILL_WIREFRAME                       =   2;
  D3DFILL_SOLID                           =   3;

  D3DCULL_NONE                            =   1;

  D3DBLEND_SRCALPHA                       =   5;
  D3DBLEND_INVSRCALPHA                    =   6;

  D3DBLENDOP_ADD                          =   1;

  D3DTOP_SELECTARG1                       =   2;

  D3DTA_DIFFUSE                           = $00000000;
  D3DTA_TEXTURE                           = $00000002;

  D3DMCS_COLOR1                           =   1;
  D3DMCS_COLOR2                           =   2;

  D3DSHADE_GOURAUD                        =   2;

  D3DDEVTYPE_HAL                          =   1;

  D3DMULTISAMPLE_NONE                     =   0;
  D3DMULTISAMPLE_2_SAMPLES                =   2;
  D3DMULTISAMPLE_4_SAMPLES                =   4;

  D3DSWAPEFFECT_DISCARD                   =   1;

  D3DBACKBUFFER_TYPE_MONO                 =   0;

  D3DPOOL_DEFAULT                         =   0;
  D3DPOOL_MANAGED                         =   1;
  D3DPOOL_SYSTEMMEM                       =   2;
  D3DPOOL_SCRATCH                         =   3;

  D3DLIGHT_DIRECTIONAL                    =   3;

  D3DRS_ZENABLE                           =   7;
  D3DRS_FILLMODE                          =   8;
  D3DRS_SHADEMODE                         =   9;
  D3DRS_ZWRITEENABLE                      =  14;
  D3DRS_SRCBLEND                          =  19;
  D3DRS_DESTBLEND                         =  20;
  D3DRS_CULLMODE                          =  22;
  D3DRS_ALPHABLENDENABLE                  =  27;
  D3DRS_SPECULARENABLE                    =  29;
  D3DRS_LIGHTING                          = 137;
  D3DRS_AMBIENT                           = 139;
  D3DRS_COLORVERTEX                       = 141;
  D3DRS_DIFFUSEMATERIALSOURCE             = 145;
  D3DRS_SPECULARMATERIALSOURCE            = 146;
  D3DRS_AMBIENTMATERIALSOURCE             = 147;
  D3DRS_BLENDOP                           = 171;

  D3DTSS_ALPHAOP                          =   4;
  D3DTSS_ALPHAARG1                        =   5;

  D3DSAMP_ADDRESSU                        =   1;
  D3DSAMP_ADDRESSV                        =   2;
  D3DSAMP_BORDERCOLOR                     =   4;
  D3DSAMP_MAGFILTER                       =   5;
  D3DSAMP_MINFILTER                       =   6;
  D3DSAMP_MIPFILTER                       =   7;
  D3DSAMP_MAXANISOTROPY                   =  10;

  D3DPT_POINTLIST                         =   1;
  D3DPT_LINELIST                          =   2;
  D3DPT_LINESTRIP                         =   3;
  D3DPT_TRIANGLELIST                      =   4;
  D3DPT_TRIANGLESTRIP                     =   5;
  D3DPT_TRIANGLEFAN                       =   6;

  D3DXIFF_BMP                             =   0;

  D3DUSAGE_RENDERTARGET                   = $00000001;
  D3DUSAGE_DEPTHSTENCIL                   = $00000002;
  D3DUSAGE_WRITEONLY                      = $00000008;
  D3DUSAGE_DYNAMIC                        = $00000200;
  D3DUSAGE_AUTOGENMIPMAP                  = $00000400;

  D3DLOCK_READONLY                        = $00000010;
  D3DLOCK_NOSYSLOCK                       = $00000800;
  D3DLOCK_DISCARD                         = $00002000;

  D3DRTYPE_SURFACE                        =  1;

  D3DXMESH_SYSTEMMEM                      = $110;

  MAX_DEVICE_IDENTIFIER_STRING            = 512;

//==============================================================================
// Types
//==============================================================================

type
  {$EXTERNALSYM HMONITOR}  
  HMONITOR                  = THandle;

  TD3DColor                 = DWORD ;
  TD3DFormat                = DWORD ;
  TD3DDevType               = DWORD ;
  TD3DResourceType          = DWORD ;
  TD3DMultisampleType       = DWORD ;
  TD3DSwapEffect            = DWORD ;
  TD3DBackBufferType        = DWORD ;
  TD3DPool                  = DWORD ;
  TD3DTransformStateType    = DWORD ;
  TD3DLightType             = DWORD ;
  TD3DRenderStateType       = DWORD ;
  TD3DTextureStageStateType = DWORD ;
  TD3DSamplerStateType      = DWORD ;
  TD3DPrimitiveType         = DWORD ;
  TD3DTextureFilterType     = DWORD ;


  TD3DVector4 = record
    x : Single ;
    y : Single ;
    z : Single ;
    w : SIngle ;
  end ;
  PD3DVector4  = ^TD3DVector4;
  TD3DXVector4 =  TD3DVector4;

  TD3DVector = record
    x : Single ;
    y : Single ;
    z : Single ;
  end ;
  PD3DVector   = ^TD3DVector;
  TD3DXVector3 =  TD3DVector;

  TD3DVector2 = record
    x : Single ;
    y : Single ;
  end;
  PD3DVector2  = ^TD3DVector2;
  TD3DXVector2 =  TD3DVector2;

  TD3DColorValue = record
    r : Single ;
    g : Single ;
    b : Single ;
    a : Single ;
  end ;

  TD3DDisplayMode = record
    Width        : LongWord   ;
    Height       : LongWord   ;
    RefreshRate  : LongWord   ;
    Format       : TD3DFormat ;
  end ;

  TD3DVShaderCaps2_0 = record
    Caps                     : DWORD   ;
    DynamicFlowControlDepth  : Integer ;
    NumTemps                 : Integer ;
    StaticFlowControlDepth   : Integer ;
  end;

  TD3DPShaderCaps2_0 = record
    Caps                     : DWORD   ;
    DynamicFlowControlDepth  : Integer ;
    NumTemps                 : Integer ;
    StaticFlowControlDepth   : Integer ;
    NumInstructionSlots      : Integer ;
  end;

  TD3DCaps9 = record
    DeviceType                         : TD3DDevType ;
    AdapterOrdinal                     : DWORD       ;
    Caps                               : DWORD       ;
    Caps2                              : DWORD       ;
    Caps3                              : DWORD       ;
    PresentationIntervals              : DWORD       ;
    CursorCaps                         : DWORD       ;
    DevCaps                            : DWORD       ;
    PrimitiveMiscCaps                  : DWORD       ;
    RasterCaps                         : DWORD       ;
    ZCmpCaps                           : DWORD       ;
    SrcBlendCaps                       : DWORD       ;
    DestBlendCaps                      : DWORD       ;
    AlphaCmpCaps                       : DWORD       ;
    ShadeCaps                          : DWORD       ;
    TextureCaps                        : DWORD       ;
    TextureFilterCaps                  : DWORD       ;
    CubeTextureFilterCaps              : DWORD       ;
    VolumeTextureFilterCaps            : DWORD       ;
    TextureAddressCaps                 : DWORD       ;
    VolumeTextureAddressCaps           : DWORD       ;
    LineCaps                           : DWORD       ;
    MaxTextureWidth                    : DWORD       ;
    MaxTextureHeight                   : DWORD       ;
    MaxVolumeExtent                    : DWORD       ;
    MaxTextureRepeat                   : DWORD       ;
    MaxTextureAspectRatio              : DWORD       ;
    MaxAnisotropy                      : DWORD       ;
    MaxVertexW                         : Single      ;
    GuardBandLeft                      : Single      ;
    GuardBandTop                       : Single      ;
    GuardBandRight                     : Single      ;
    GuardBandBottom                    : Single      ;
    ExtentsAdjust                      : Single      ;
    StencilCaps                        : DWORD       ;
    FVFCaps                            : DWORD       ;
    TextureOpCaps                      : DWORD       ;
    MaxTextureBlendStages              : DWORD       ;
    MaxSimultaneousTextures            : DWORD       ;
    VertexProcessingCaps               : DWORD       ;
    MaxActiveLights                    : DWORD       ;
    MaxUserClipPlanes                  : DWORD       ;
    MaxVertexBlendMatrices             : DWORD       ;
    MaxVertexBlendMatrixIndex          : DWORD       ;
    MaxPointSize                       : Single      ;
    MaxPrimitiveCount                  : DWORD       ;
    MaxVertexIndex                     : DWORD       ;
    MaxStreams                         : DWORD       ;
    MaxStreamStride                    : DWORD       ;
    VertexShaderVersion                : DWORD       ;
    MaxVertexShaderConst               : DWORD       ;
    PixelShaderVersion                 : DWORD       ;
    PixelShader1xMaxValue              : Single      ;
    DevCaps2                           : DWORD       ;
    MaxNpatchTessellationLevel         : Single      ;
    Reserved5                          : DWORD       ;
    MasterAdapterOrdinal               : LongWord    ;
    AdapterOrdinalInGroup              : LongWord    ;
    NumberOfAdaptersInGroup            : LongWord    ;
    DeclTypes                          : DWORD       ;
    NumSimultaneousRTs                 : DWORD       ;
    StretchRectFilterCaps              : DWORD       ;
    VS20Caps                           : TD3DVShaderCaps2_0 ;
    PS20Caps                           : TD3DPShaderCaps2_0 ;
    VertexTextureFilterCaps            : DWORD       ;
    MaxVShaderInstructionsExecuted     : DWORD       ;
    MaxPShaderInstructionsExecuted     : DWORD       ;
    MaxVertexShader30InstructionSlots  : DWORD       ;
    MaxPixelShader30InstructionSlots   : DWORD       ;
  end ;

  TD3DPresentParameters = record
    BackBufferWidth                    : LongWord    ;
    BackBufferHeight                   : LongWord    ;
    BackBufferFormat                   : TD3DFormat  ;
    BackBufferCount                    : LongWord    ;
    MultiSampleType                    : TD3DMultiSampleType ;
    MultiSampleQuality                 : DWORD       ;
    SwapEffect                         : TD3DSwapEffect      ;
    hDeviceWindow                      : HWND        ;
    Windowed                           : Bool        ;
    EnableAutoDepthStencil             : Bool        ;
    AutoDepthStencilFormat             : TD3DFormat  ;
    Flags                              : LongInt     ;
    FullScreen_RefreshRateInHz         : LongWord    ;
    PresentationInterval               : LongWord    ;
  end ;
  PD3DPresentParameters = ^TD3DPresentParameters ;

  TD3DDeviceCreationParameters = record
    AdapterOrdinal : LongWord    ;
    DeviceType     : TD3DDevType ;
    hFocusWindow   : HWND        ;
    BehaviorFlags  : LongInt     ;
  end ;
  TD3DRasterStatus = record
    InVBlank : Bool     ;
    ScanLine : LongWord ;
  end;

  TD3DGammaRamp = record
    red   : array [0..255] of Word;
    green : array [0..255] of Word;
    blue  : array [0..255] of Word;
  end;

  TD3DRect = record
    x1 : LongInt ;
    y1 : LongInt ;
    x2 : LongInt ;
    y2 : LongInt ;
  end ;
  PD3DRect = ^TD3DRect;

  { Structure for LockRect }
  _D3DLOCKED_RECT = record
    Pitch: Integer;
    pBits: Pointer; // void*
  end {_D3DLOCKED_RECT};
  D3DLOCKED_RECT = _D3DLOCKED_RECT;
  TD3DLockedRect = _D3DLOCKED_RECT;
  PD3DLockedRect = ^_D3DLOCKED_RECT;

  TD3DMatrix = record
    {$IFNDEF GENDOC}
    case integer of
      0 : (_11, _12, _13, _14: Single;
           _21, _22, _23, _24: Single;
           _31, _32, _33, _34: Single;
           _41, _42, _43, _44: Single);
      1 : (m : array [0..3, 0..3] of Single);
    {$ENDIF}
  end ;
  PD3DMatrix = ^TD3DMatrix ;
  TD3DXMatrix = TD3DMatrix ;

  TD3DViewport9 = record
    X      : DWORD  ;
    Y      : DWORD  ;
    Width  : DWORD  ;
    Height : DWORD  ;
    MinZ   : Single ;
    MaxZ   : Single ;
  end ;

  TD3DMaterial9 = record
    Diffuse  : TD3DColorValue ;
    Ambient  : TD3DColorValue ;
    Specular : TD3DColorValue ;
    Emissive : TD3DColorValue ;
    Power    : Single         ;
  end ;

  TD3DLight9 = record
    _Type        : TD3DLightType  ;
    Diffuse      : TD3DColorValue ;
    Specular     : TD3DColorValue ;
    Ambient      : TD3DColorValue ;
    Position     : TD3DVector     ;
    Direction    : TD3DVector     ;
    Range        : Single         ;
    Falloff      : Single         ;
    Attenuation0 : Single         ;
    Attenuation1 : Single         ;
    Attenuation2 : Single         ;
    Theta        : Single         ;
    Phi          : Single         ;
  end ;

  TD3DSurfaceDesc = record
    Format              : TD3DFormat          ;
    _Type               : TD3DResourceType    ;
    Usage               : DWORD               ;
    Pool                : TD3DPool            ;
    MultiSampleType     : TD3DMultiSampleType ;
    MultiSampleQuality  : DWORD               ;
    Width               : LongWord            ;
    Height              : LongWord            ;
  end ;



  TD3DXImageFileFormat = DWORD ;

  PD3DXImageInfo = ^TD3DXImageInfo;
  TD3DXImageInfo = record
    Width            : LongWord   ;
    Height           : LongWord   ;
    Depth            : LongWord   ;
    MipLevels        : LongWord   ;
    Format           : TD3DFormat ;
    ResourceType     : TD3DResourceType     ;
    ImageFileFormat  : TD3DXImageFileFormat ;
  end;

  TD3DXPlane = record
    a : Single ;
    b : Single ;
    c : Single ;
    d : Single ;
  end;
  PD3DXPlane = ^TD3DXPlane ;

//==============================================================================
// Interfaces
//==============================================================================

  IDirect3DDevice9       = interface ;
  IDirect3DSurface9      = interface ;
  IDirect3DVertexBuffer9 = interface ;
  IDirect3DIndexBuffer9  = interface ;
  IDirect3DBaseTexture9  = interface ;
  IDirect3DTexture9      = interface ;

  IDirect3D9 = interface(IUnknown)
    ['{81BDCBCA-64D4-426d-AE8D-AD0147F4275C}']
    {stub} function stubRegisterSoftwareDevice      : HResult; stdcall;
    {stub} function stubGetAdapterCount             : HResult; stdcall;
    {stub} function stubGetAdapterIdentifier        : HResult; stdcall;
    {stub} function stubGetAdapterModeCount         : HResult; stdcall;
    {stub} function stubEnumAdapterModes            : HResult; stdcall;
           function GetAdapterDisplayMode
                    (       Adapter                 : LongWord        ;
                      out   pMode                   : TD3DDisplayMode
                    )                               : HResult; stdcall;
    {stub} function stubCheckDeviceType             : HResult; stdcall;
           function CheckDeviceFormat(
                            Adapter                 : LongWord        ;
                            DeviceType              : TD3DDevType     ;
                            AdapterFormat           : TD3DFormat      ;
                            Usage                   : DWord           ;
                            RType                   : TD3DResourceType ;
                            CheckFormat             : TD3DFormat
                    )                               : HResult; stdcall;
           function CheckDeviceMultiSampleType(
                            Adapter                 : LongWord        ;
                            DeviceType              : TD3DDevType     ;
                            SurfaceFormat           : TD3DFormat      ;
                            Windowed                : BOOL            ;
                            MultiSampleType         : TD3DMultiSampleType ;
                            pQualityLevels          : PDWORD
                    )                               : HResult; stdcall;
    {stub} function stubCheckDepthStencilMatch      : HResult; stdcall;
    {stub} function stubCheckDeviceFormatConversion : HResult; stdcall;
    {stub} function stubGetDeviceCaps               : HResult; stdcall;
    {stub} function stubGetAdapterMonitor           : HResult; stdcall;
           function CreateDevice(
                            Adapter                 : LongWord        ;
                            DeviceType              : TD3DDevType     ;
                            hFocusWindow            : HWND            ;
                            BehaviorFlags           : DWORD           ;
                            pPresentationParameters : PD3DPresentParameters ;
                      out   ppReturnedDeviceInterface
                                                    : IDirect3DDevice9
                    )                               : HResult; stdcall;
  end;

  IDirect3DDevice9 = interface(IUnknown)
    ['{D0223B96-BF7A-43fd-92BD-A43B0D82B9EB}']
    {stub} function stubTestCooperativeLevel        : HResult; stdcall;
    {stub} function stubGetAvailableTextureMem      : LongWord; stdcall;
    {stub} function stubEvictManagedResources       : HResult; stdcall;
    {stub} function stubGetDirect3D: HResult        ; stdcall;
           function GetDeviceCaps(
                      out   pCaps                   : TD3DCaps9
                    )                               : HResult; stdcall;
    {stub} function stubGetDisplayMode              : HResult; stdcall;
    {stub} function stubGetCreationParameters       : HResult; stdcall;
    {stub} procedure stubSetCursorProperties                 ; stdcall;
    {stub} function stubSetCursorPosition           : HResult; stdcall;
    {stub} function stubShowCursor                  : HResult; stdcall;
    {stub} function stubCreateAdditionalSwapChain   : HResult; stdcall;
    {stub} function stubGetSwapChain                : HResult; stdcall;
    {stub} function stubGetNumberOfSwapChains       : HResult; stdcall;
           function Reset(
                      const pPresentationParameters : TD3DPresentParameters
                    )                               : HResult; stdcall;
           function Present(
                            pSourceRect,
                            pDestRect               : PRect           ;
                            hDestWindowOverride     : HWND            ;
                            pDirtyRegion            : PRgnData
                    )                               : HResult; stdcall;
           function GetBackBuffer(
                            iSwapChain              : LongWord        ;
                            iBackBuffer             : LongWord        ;
                            _Type                   : TD3DBackBufferType ;
                      out   ppBackBuffer            : IDirect3DSurface9
                    )                               : HResult; stdcall;
    {stub} function stubGetRasterStatus             : HResult; stdcall;
    {stub} function stubSetDialogBoxMode            : HResult; stdcall;
    {stub} function stubSetGammaRamp                : HResult; stdcall;
    {stub} function stubGetGammaRamp                : HResult; stdcall;
           function CreateTexture(
                            Width, Height, Levels   : LongWord        ;
                            Usage                   : DWord           ;
                            Format                  : TD3DFormat      ;
                            Pool                    : TD3DPool        ;
                      out   ppTexture               : IDirect3DTexture9 ;
                      pSharedHandle                 : PHandle
                    )                               : HResult; stdcall;
    {stub} function stubCreateVolumeTexture         : HResult; stdcall;
    {stub} function stubCreateCubeTexture           : HResult; stdcall;
           function CreateVertexBuffer(
                            Length                  : LongWord        ;
                            Usage,
                            FVF                     : DWORD           ;
                            Pool                    : TD3DPool        ;
                      out   ppVertexBuffer          : IDirect3DVertexBuffer9 ;
                            pSharedHandle           : PHandle
                    )                               : HResult; stdcall;
           function CreateIndexBuffer(
                            Length                  : LongWord        ;
                            Usage                   : DWord           ;
                            Format                  : TD3DFormat      ;
                            Pool                    : TD3DPool        ;
                      out   ppIndexBuffer           : IDirect3DIndexBuffer9  ;
                            pSharedHandle           : PHandle
                    )                               : HResult; stdcall;
           function CreateRenderTarget(
                            Width,
                            Height                  : LongWord        ;
                            Format                  : TD3DFormat      ;
                            MultiSample             : TD3DMultiSampleType;
                            MultisampleQuality      : DWORD           ;
                            Lockable                : BOOL            ;
                      out   ppSurface               : IDirect3DSurface9;
                            pSharedHandle           : PHandle
                   )                                : HResult; stdcall;
    {stub} function stubCreateDepthStencilSurface   : HResult; stdcall;
    {stub} function stubUpdateSurface               : HResult; stdcall;
    {stub} function stubUpdateTexture               : HResult; stdcall;
           function GetRenderTargetData(
                            pRenderTarget,
                            pDestSurface            : IDirect3DSurface9
                    )                               : HResult; stdcall;
    {stub} function stubGetFrontBufferData          : HResult; stdcall;
           function StretchRect(
                            pSourceSurface          : IDirect3DSurface9;
                            pSourceRect             : PRect            ;
                            pDestSurface            : IDirect3DSurface9;
                            pDestRect               : PRect            ;
                            Filter                  : TD3DTextureFilterType
                    )                               : HResult; stdcall;
    {stub} function stubColorFill                   : HResult; stdcall;
           function CreateOffscreenPlainSurface(
                            Width,
                            Height                  : LongWord        ;
                            Format                  : TD3DFormat      ;
                            Pool                    : TD3DPool        ;
                      out   ppSurface               : IDirect3DSurface9 ;
                            pSharedHandle           : PHandle
                    )                               : HResult; stdcall;
    {stub} function stubSetRenderTarget             : HResult; stdcall;
           function GetRenderTarget(
                            RenderTargetIndex       : DWORD           ;
                      out   ppRenderTarget          : IDirect3DSurface9
                    )                               : HResult; stdcall;
    {stub} function stubSetDepthStencilSurface      : HResult; stdcall;
    {stub} function stubGetDepthStencilSurface      : HResult; stdcall;
           function BeginScene                      : HResult; stdcall;
           function EndScene                        : HResult; stdcall;
           function Clear(
                            Count                   : DWORD           ;
                            pRects                  : PD3DRect        ;
                            Flags                   : DWORD           ;
                            Color                   : TD3DColor       ;
                            Z                       : Single          ;
                            Stencil                 : DWORD
                    )                               : HResult; stdcall;
           function SetTransform(
                            State                   : TD3DTransformStateType ;
                      const pMatrix                 : TD3DMatrix
                    )                               : HResult; stdcall;
           function GetTransform(
                            State                   : TD3DTransformStateType ;
                      out   pMatrix                 : TD3DMatrix
                    )                               : HResult; stdcall;
    {stub} function stubMultiplyTransform           : HResult; stdcall;
    {stub} function stubSetViewport                 : HResult; stdcall;
           function GetViewport(
                      out   pViewport               : TD3DViewport9
                    )                               : HResult; stdcall;
           function SetMaterial(
                      const pMaterial               : TD3DMaterial9
                    )                               : HResult; stdcall;
    {stub} function stubGetMaterial                 : HResult; stdcall;
           function SetLight(
                            Index                   : DWORD           ;
                      const pLight                  : TD3DLight9
                    )                               : HResult; stdcall;
           function GetLight(
                            Index                   : DWORD           ;
                      out   pLight                  : TD3DLight9
                    )                               : HResult; stdcall;
           function LightEnable(
                            Index                   : DWORD           ;
                            Enable                  : BOOL
                    )                               : HResult; stdcall;
    {stub} function stubGetLightEnable              : HResult; stdcall;
    {stub} function stubSetClipPlane                : HResult; stdcall;
    {stub} function stubGetClipPlane                : HResult; stdcall;
           function SetRenderState(
                            State                   : TD3DRenderStateType ;
                            Value                   : DWORD
                    )                               : HResult; stdcall;
    {stub} function stubstubGetRenderState          : HResult; stdcall;
    {stub} function stubCreateStateBlock            : HResult; stdcall;
    {stub} function stubBeginStateBlock             : HResult; stdcall;
    {stub} function stubEndStateBlock               : HResult; stdcall;
    {stub} function stubSetClipStatus               : HResult; stdcall;
    {stub} function stubGetClipStatus               : HResult; stdcall;
    {stub} function stubGetTexture                  : HResult; stdcall;
           function SetTexture(
                            Stage                   : DWORD           ;
                            pTexture                : IDirect3DBaseTexture9
                    )                               : HResult; stdcall;
    {stub} function stubGetTextureStageState        : HResult; stdcall;
           function SetTextureStageState(
                            Stage                   : DWORD           ;
                            _Type                   : TD3DTextureStageStateType ;
                            Value                   : DWORD
                    )                               : HResult; stdcall;
    {stub} function GetSamplerState                 : HResult; stdcall;
           function SetSamplerState(
                            Sampler                 : DWORD           ;
                            _Type                   : TD3DSamplerStateType ;
                            Value                   : DWORD
                    )                               : HResult; stdcall;
    {stub} function stubValidateDevice              : HResult; stdcall;
    {stub} function stubSetPaletteEntries           : HResult; stdcall;
    {stub} function stubGetPaletteEntries           : HResult; stdcall;
    {stub} function stubSetCurrentTexturePalette    : HResult; stdcall;
    {stub} function stubGetCurrentTexturePalette    : HResult; stdcall;
    {stub} function stubSetScissorRect              : HResult; stdcall;
    {stub} function stubGetScissorRect              : HResult; stdcall;
    {stub} function stubSetSoftwareVertexProcessing : HResult; stdcall;
    {stub} function stubGetSoftwareVertexProcessing : HResult; stdcall;
    {stub} function stubSetNPatchMode               : HResult; stdcall;
    {stub} function stubGetNPatchMode               : HResult; stdcall;
           function DrawPrimitive(
                            PrimitiveType           : TD3DPrimitiveType ;
                            StartVertex,
                            PrimitiveCount          : LongWord
                    )                               : HResult; stdcall;
           function DrawIndexedPrimitive(
                            _Type                   : TD3DPrimitiveType ;
                            BaseVertexIndex         : Integer ;
                            MinVertexIndex,
                            NumVertices,
                            startIndex,
                            primCount               : LongWord
                    )                               : HResult; stdcall;
    {stub} function stubDrawPrimitiveUP             : HResult; stdcall;
    {stub} function stubDrawIndexedPrimitiveUP      : HResult; stdcall;
    {stub} function stubProcessVertices             : HResult; stdcall;
    {stub} function stubCreateVertexDeclaration     : HResult; stdcall;
    {stub} function stubSetVertexDeclaration        : HResult; stdcall;
    {stub} function stubGetVertexDeclaration        : HResult; stdcall;
           function SetFVF(
                            FVF                     : DWORD
                    )                               : HResult; stdcall;
    {stub} function stubGetFVF                      : HResult; stdcall;
    {stub} function stubCreateVertexShader          : HResult; stdcall;
    {stub} function stubSetVertexShader             : HResult; stdcall;
    {stub} function stubGetVertexShader             : HResult; stdcall;
    {stub} function stubSetVertexShaderConstantF    : HResult; stdcall;
    {stub} function stubGetVertexShaderConstantF    : HResult; stdcall;
    {stub} function stubSetVertexShaderConstantI    : HResult; stdcall;
    {stub} function stubGetVertexShaderConstantI    : HResult; stdcall;
    {stub} function stubSetVertexShaderConstantB    : HResult; stdcall;
    {stub} function stubGetVertexShaderConstantB    : HResult; stdcall;
           function SetStreamSource(
                            StreamNumber            : LongWord        ;
                            pStreamData             : IDirect3DVertexBuffer9 ;
                            OffsetInBytes,
                            Stride                  : LongWord
                    )                               : HResult; stdcall;
    {stub} function stubGetStreamSource             : HResult; stdcall;
    {stub} function stubSetStreamSourceFreq         : HResult; stdcall;
    {stub} function stubGetStreamSourceFreq         : HResult; stdcall;
           function SetIndices(
                            pIndexData              : IDirect3DIndexBuffer9
                    )                               : HResult; stdcall;
    {stub} function stubGetIndices: HResult         ; stdcall;
    {stub} function stubCreatePixelShader           : HResult; stdcall;
    {stub} function stubSetPixelShader              : HResult; stdcall;
    {stub} function stubGetPixelShader              : HResult; stdcall;
    {stub} function stubSetPixelShaderConstantF     : HResult; stdcall;
    {stub} function stubGetPixelShaderConstantF     : HResult; stdcall;
    {stub} function stubSetPixelShaderConstantI     : HResult; stdcall;
    {stub} function stubGetPixelShaderConstantI     : HResult; stdcall;
    {stub} function stubSetPixelShaderConstantB     : HResult; stdcall;
    {stub} function stubGetPixelShaderConstantB     : HResult; stdcall;
    {stub} function stubDrawRectPatch               : HResult; stdcall;
    {stub} function stubDrawTriPatch                : HResult; stdcall;
    {stub} function stubDeletePatch                 : HResult; stdcall;
    {stub} function stubCreateQuery                 : HResult; stdcall;
  end;

  IDirect3DResource9 = interface(IUnknown)
    ['{05EEC05D-8F7D-4362-B999-D1BAF357C704}']
    {stub} function stubGetDevice                   : HResult; stdcall;
    {stub} function stubSetPrivateData              : HResult; stdcall;
    {stub} function stubGetPrivateData              : HResult; stdcall;
    {stub} function stubFreePrivateData             : HResult; stdcall;
    {stub} function stubSetPriority                 : HResult; stdcall;
    {stub} function stubGetPriority                 : HResult; stdcall;
    {stub} function stubPreLoad                     : HResult; stdcall;
    {stub} function stubGetType                     : HResult; stdcall;
  end ;

  IDirect3DSurface9 = interface(IDirect3DResource9)
    ['{0CFBAF3A-9FF6-429a-99B3-A2796AF8B89B}']
    {stub} function stubGetContainer                : HResult; stdcall;
           function GetDesc(
                      out   pDesc                   : TD3DSurfaceDesc
                    )                               : HResult; stdcall;
           function LockRect(
                      out   pLockedRect             : TD3DLockedRect  ;
                            pRect                   : PRect           ;
                            Flags                   : DWord
                    )                               : HResult; stdcall;
           function UnlockRect                      : HResult; stdcall;
    {stub} function stubGetDC                       : HResult; stdcall;
    {stub} function stubReleaseDC                   : HResult; stdcall;
  end ;

  IDirect3DBaseTexture9 = interface(IDirect3DResource9)
    ['{580CA87E-1D3C-4d54-991D-B7D3E3C298CE}']
    {stub} function stubSetLOD                      : DWord; stdcall;
    {stub} function stubGetLOD                      : DWord; stdcall;
    {stub} function stubGetLevelCount               : DWord; stdcall;
    {stub} function stubSetAutoGenFilterType        : HResult; stdcall;
    {stub} function stubGetAutoGenFilterType        : Tobject; stdcall;
    {stub} procedure stubGenerateMipSubLevels       ; stdcall;

  end ;

  IDirect3DTexture9 = interface(IDirect3DBaseTexture9)
    ['{85C31227-3DE5-4f00-9B3A-F11AC38C18B5}']
    (*** IDirect3DTexture9 methods ***)
    {stub} function stubGetLevelDesc                : HResult; stdcall;
           function GetSurfaceLevel(
                                  Level             : LongWord        ;
                              out ppSurfaceLevel    : IDirect3DSurface9
                            )                       : HResult; stdcall;
           function LockRect(
                                  Level             : LongWord        ;
                              out pLockedRect       : TD3DLockedRect  ;
                                  pRect             : PRect           ;
                                  Flags             : DWord
                            )                       : HResult; stdcall;
           function UnlockRect(
                                  Level             : LongWord
                              )                     : HResult; stdcall;
    {stub} function stubAddDirtyRect                : HResult; stdcall;
  end ;

  IDirect3DVertexBuffer9 = interface(IDirect3DResource9)
    ['{B64BB1B5-FD70-4df6-BF91-19D0A12455E3}']
           function Lock(
                            OffsetToLock,
                            SizeToLock              : LongWord        ;
                      out   ppbData                 : Pointer         ;
                            Flags                   : DWORD
                    )                               : HResult; stdcall;
           function Unlock                          : HResult; stdcall;
    {stub} function stubGetDesc                     : HResult; stdcall;
  end;

  IDirect3DIndexBuffer9 = interface(IDirect3DResource9)
    ['{7C9DD65E-D3F7-4529-ACEE-785830ACDE35}']
    (*** IDirect3DIndexBuffer9 methods ***)
           function Lock(
                            OffsetToLock,
                            SizeToLock              : DWord           ;
                      out   ppbData                 : Pointer         ;
                            Flags                   : DWord
                    )                               : HResult; stdcall;
           function Unlock                          : HResult; stdcall;
    {stub} function stubGetDesc                     : HResult; stdcall;
  end;

//==============================================================================
// Utility function prototypes
//==============================================================================

  function D3DCOLOR_ARGB(
          a           : DWORD              ;
          r           : DWORD              ;
          g           : DWORD              ;
          b           : DWORD
  ): TD3DColor;{$IFDEF SUPPORTS_INLINE} inline;{$ENDIF}

  function D3DCOLOR_XRGB(
          r           : DWORD              ;
          g           : DWORD              ;
          b           : DWORD
  ): TD3DColor;{$IFDEF SUPPORTS_INLINE} inline;{$ENDIF}

//==============================================================================
// Dynamic link function prototypes
//==============================================================================

type
  TDirect3DCreate9 = function (
          SDKVersion   : LongWord
  ): Pointer; stdcall;

  function Direct3DCreate9(
          SDKVersion  : LongWord
  ): IDirect3D9; stdcall;

//==============================================================================
// Dynamic procedure handle
//==============================================================================

var
  _Direct3DCreate9 : TDirect3DCreate9 ;

implementation

var
  hD3D9     : THandle ;

const
  D3D9_DLL  = 'd3d9.dll' ;

  function D3DCOLOR_ARGB(
    a : DWORD ;
    r : DWORD ;
    g : DWORD ;
    b : DWORD
  ): TD3DColor;
  begin
    Result := (a shl 24) or (r shl 16) or (g shl 8) or b;
  end;

  function D3DCOLOR_XRGB(
    r,g,b: DWORD
  ): TD3DColor;
  begin
    Result := DWORD($FF shl 24) or (r shl 16) or (g shl 8) or b;
  end;

  // initialize directX and load libraries
  function Direct3DCreate9(
    SDKVersion: LongWord
  ): IDirect3D9; stdcall;

  begin
    try
      if hD3D9 = 0 then hD3D9  := LoadLibrary( D3D9_DLL  ) ;

      if hD3D9 = 0 then Abort ;

      _Direct3DCreate9
        := GetProcAddress( hD3D9,  'Direct3DCreate9' ) ;
      if not Assigned( @_Direct3DCreate9 ) then Abort ;

      Result := IDirect3D9( _Direct3DCreate9( SDKVersion ) ) ;
    except
      if hD3D9 <> 0 then FreeLibrary( hD3D9  ) ;

      hD3D9 := 0 ;
    end;
  end;


//==============================================================================
// initialization / finalization
//==============================================================================

initialization
  hD3D9 := 0 ;

finalization
  if hD3D9 <> 0 then FreeLibrary( hD3D9 ) ;
  hD3D9 := 0 ;
  
{$ENDIF}
//==================================== END =====================================
end.


