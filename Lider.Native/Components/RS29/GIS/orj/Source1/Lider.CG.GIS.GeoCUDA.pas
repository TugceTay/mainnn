//==============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//==============================================================================
{
  Nvidia CUDA wrapper.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoCUDA ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoCUDA"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.CUDA ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.cuda ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.NDK.CUDA ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.IO,
    System.Security,
    System.Runtime.InteropServices,
    System.Collections.Generic,
    TatukGIS.RTL,
    TatukGIS.NDK,
    TatukGIS.NDK.OpenCL;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Generics.Collections,
    Lider.CG.GIS.GeoOpenCL,
    Lider.CG.GIS.GeoBaseObject;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}

{$IFDEF NEXTGEN}
//=============================== NEXTGEN ======================================
type

  /// <summary>
  ///   Provides the means to initialize and enable the CUDA computing
  ///   engine.
  /// </summary>
  TGIS_CUDAEngine = {$IFDEF OXYGENE} public {$ENDIF}
                      class ( TGIS_BaseObjectDisposable )
    private
      FInitialized : Boolean ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   True if CUDA is initialized.
      /// </summary>
      property Initialized   : Boolean
                               read  FInitialized ;
  end ;

  /// <summary>
  ///   Returns a global CUDA engine object.
  /// </summary>
  /// <returns>
  ///   CUDA engine object
  /// </returns>
  function  Lider.CG.GIS.GeoCUDAEngine : TGIS_CUDAEngine ;

//##############################################################################
implementation


var
  cudaEngine : TGIS_CUDAEngine ;

  constructor TGIS_CUDAEngine.Create ;
  begin
    inherited ;

    FInitialized := False ;
  end ;

  procedure TGIS_CUDAEngine.doDestroy ;
  begin

    inherited ;
  end ;

  function GisCUDAEngine : TGIS_CUDAEngine ;
  begin
    if not assigned( cudaEngine ) then
      cudaEngine := TGIS_CUDAEngine.Create ;

    Result := cudaEngine ;
  end ;

//==============================================================================
{$ELSE}

type

  /// <summary>
  ///   Provides information about a CUDA device.
  /// </summary>
  TGIS_CUDADeviceInfo = {$IFDEF OXYGENE} public {$ENDIF}
                          class ( TGIS_Object )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oDevice : TObject ;
    private
      function  fget_Name        : String ;
      function  fget_TotalMemory : NativeUInt ;
      function  fget_APIVersion  : Cardinal ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Device name.
      /// </summary>
      property Name        : String
                             read  fget_Name ;
      /// <summary>
      ///   Total memory available on the device.
      /// </summary>
      property TotalMemory : NativeUInt
                             read  fget_TotalMemory ;
      /// <summary>
      ///   API version of the context.
      /// </summary>
      property APIVersion  : Cardinal
                             read  fget_APIVersion ;
  end ;


  /// <summary>
  ///   Provides the means to initialize and enable the CUDA computing
  ///   engine.
  /// </summary>
  TGIS_CUDAEngine = {$IFDEF OXYGENE} public {$ENDIF}
                      class ( TGIS_ObjectDisposable )
    private
      {$IFDEF DCC}
        FDevices : TObjectList<TGIS_CUDADeviceInfo> ;
      {$ENDIF}
      {$IFDEF CLR}
        FDevices : TList<TGIS_CUDADeviceInfo> ;
      {$ENDIF}
    private
      function  fget_DriverVersion : Integer ;
      function  fget_Available     : Boolean ;
      function  fget_Initialized   : Boolean ;
      function  fget_Enabled       : Boolean ;
      procedure fset_Enabled       ( const _val : Boolean
                                   ) ;
      function  fget_DeviceCount    : Cardinal ;
      function  fget_Devices        ( const _idx : Integer
                                    ) : TGIS_CUDADeviceInfo ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   CUDA driver version.
      /// </summary>
      property DriverVersion : Integer
                               read  fget_DriverVersion ;
      /// <summary>
      ///   True if CUDA is available; calling this property attempts
      ///   to initialize CUDA.
      /// </summary>
      property Available     : Boolean
                               read  fget_Available ;
      /// <summary>
      ///   True if CUDA is initialized.
      /// </summary>
      property Initialized   : Boolean
                               read  fget_Initialized ;
      /// <summary>
      ///   Specifies if CUDA should be used whenever possible.
      /// </summary>
      property Enabled       : Boolean
                               read  fget_Enabled
                               write fset_Enabled ;
      /// <summary>
      ///   Number of CUDA devices.
      /// </summary>
      property DeviceCount   : Cardinal
                               read  fget_DeviceCount ;
      /// <summary>
      ///   List of CUDA devices.
      /// </summary>
      /// <param name="_idx">
      ///   index of the device
      /// </param>
      property Devices       [const _idx : Integer]
                             : TGIS_CUDADeviceInfo
                               read  fget_Devices ;
  end ;


  /// <summary>
  ///   Returns a global CUDA engine object.
  /// </summary>
  /// <returns>
  ///   CUDA engine object
  /// </returns>
  function GisCUDAEngine : TGIS_CUDAEngine ;


type

  /// <summary>
  ///   Provides the means to execute a CUDA program.
  /// </summary>
  TGIS_CUDAProgram = {$IFDEF OXYGENE} public {$ENDIF}
                       class ( TGIS_ObjectDisposable )
    private
      oDevice          : TObject ;
      oProgram         : TObject ;
      {$IFDEF DCC}
        oKernelList    : TObjectList<TObject> ;
      {$ENDIF}
      {$IFDEF CLR}
        oKernelList    : TList<TObject> ;
      {$ENDIF}
      sKernel          : String ;
    private
      FKernelIndex     : Integer ;
    private
      function  fget_KernelCount     : Integer ;
      function  fget_KernelIndex     : Integer ;
      procedure fset_KernelIndex     ( const _val : Integer
                                     ) ;
      function  fget_KernelName      : String ;
      function  fget_GridDimensions  ( const _idx : Integer
                                     ) : NativeUInt ;
      procedure fset_GridDimensions  ( const _idx : Integer ;
                                       const _val : NativeUInt
                                     ) ;
      function  fget_BlockDimensions ( const _idx : Integer
                                     ) : NativeUInt ;
      procedure fset_BlockDimensions ( const _idx : Integer ;
                                       const _val : NativeUInt
                                     ) ;
      function  fget_ErrorCode       : Integer ;
      function  fget_ErrorName       : String ;
      function  fget_ErrorString     : String ;
    private
      function  prepare ( const _source : String ;
                          const _kernel : String
                        ) : Boolean ;
      function  enqueue ( const _kernel : String
                        ) : Boolean ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Loads a CUDA program from a string (cubin, PTX or fatbin image) and
      ///   enqueues a kernel for execution.
      /// </summary>
      /// <param name="_source">
      ///   source of the program
      /// </param>
      /// <param name="_kernel">
      ///   name of the kernel function
      /// </param>
      /// <returns>
      ///   True if the program was successfully loaded
      /// </returns>
      function  LoadFromString ( const _source : String ;
                                 const _kernel : String
                               ) : Boolean ; overload ;
      /// <summary>
      ///   Loads a CUDA program (cubin, PTX or fatbin image) from a text file
      ///   and enqueues a kernel for execution.
      /// </summary>
      /// <param name="_path">
      ///   path to the file
      /// </param>
      /// <param name="_kernel">
      ///   name of the kernel function
      /// </param>
      /// <returns>
      ///   True if the program was successfully loaded
      /// </returns>
      function  LoadFromFile   ( const _path   : String ;
                                 const _kernel : String
                               ) : Boolean ; overload ;
      /// <summary>
      ///   Enqueues a kernel for execution and sets it as the active kernel.
      /// </summary>
      /// <param name="_kernel">
      ///   name of the kernel function
      /// </param>
      /// <returns>
      ///   True if the kernel was successfully enqueued
      /// </returns>
      function EnqueueKernel   ( const _kernel : String
                               ) : Boolean ;
      /// <summary>
      ///   Sets an argument of the active kernel.
      /// </summary>
      /// <param name="_idx">
      ///   index of the argument
      /// </param>
      /// <param name="_size">
      ///   size of the argument (in bytes)
      /// </param>
      /// <param name="_data">
      ///   data of the argument
      /// </param>
      /// <returns>
      ///   True if the argument has been properly set
      /// </returns>
      function  SetArgument    ( const _idx    : Integer ;
                                 const _size   : Integer ;
                                 {$IFDEF DCC}
                                   const _data : Pointer
                                 {$ENDIF}
                                 {$IFDEF CLR}
                                   const _data : Object
                                 {$ENDIF}
                               ) : Boolean ;
      /// <summary>
      ///   Sets a argument of the active kernel with a argument already set
      ///   for another kernel.
      /// </summary>
      /// <param name="_idx">
      ///   index of the argument
      /// </param>
      /// <param name="_keridx">
      ///   index of the owner kernel
      /// </param>
      /// <param name="_argidx">
      ///   index of the owner kernel argument
      /// </param>
      /// <returns>
      ///   True if the argument has been properly set
      /// </returns>
      function  ShareArgument  ( const _idx    : Integer ;
                                 const _keridx : Integer ;
                                 const _argidx : Integer
                               ) : Boolean ;
      /// <summary>
      ///   Checks if an argument of the active kernel has been propertly set.
      /// </summary>
      /// <param name="_idx">
      ///   index of the argument
      /// </param>
      /// <returns>
      ///   True if the argument has been properly set
      /// </returns>
      function  CheckArgument  ( const _idx    : Integer
                               ) : Boolean ;
      /// <summary>
      ///   Executes the CUDA program.
      /// </summary>
      /// <returns>
      ///   True if the program has finished successfully
      /// </returns>
      function  Execute        : Boolean ;
      /// <summary>
      ///   Reads an argument of the active kernel.
      /// </summary>
      /// <param name="_idx">
      ///   index of the argument
      /// </param>
      /// <param name="_size">
      ///   size of the argument (in bytes)
      /// </param>
      /// <param name="_data">
      ///   buffer to be filled
      /// </param>
      /// <returns>
      ///   True if the argument has been successfully read
      /// </returns>
      function  ReadArgument   ( const _idx    : Integer ;
                                 const _size   : Integer ;
                                 {$IFDEF DCC}
                                   const _data : Pointer
                                 {$ENDIF}
                                 {$IFDEF CLR}
                                   const _data : Object
                                 {$ENDIF}
                               ) : Boolean ;
    public
      /// <summary>
      ///   Number of enqueued kernels of the CUDA program.
      /// </summary>
      property KernelCount     : Integer
                                 read fget_KernelCount ;
      /// <summary>
      ///   Index of the active kernel.
      /// </summary>
      property KernelIndex     : Integer
                                 read  fget_KernelIndex
                                 write fset_KernelIndex ;
      /// <summary>
      ///   Name of the active kernel.
      /// </summary>
      property KernelName      : String
                                 read  fget_KernelName ;
      /// <summary>
      ///   Grid dimensions of the active kernel.
      /// </summary>
      /// <param name="_idx">
      ///   index of the grid dimension
      /// </param>
      property GridDimensions  [const _idx : Integer]
                               : NativeUInt
                                 read  fget_GridDimensions
                                 write fset_GridDimensions ;
      /// <summary>
      ///   Block dimensions of the active kernel.
      /// </summary>
      /// <param name="_idx">
      ///   index of the block dimension
      /// </param>
      property BlockDimensions [const _idx : Integer]
                               : NativeUInt
                                 read  fget_BlockDimensions
                                 write fset_BlockDimensions ;
      /// <summary>
      ///   Last error code returned by the CUDA engine.
      /// </summary>
      property ErrorCode       : Integer
                                 read  fget_ErrorCode ;
      /// <summary>
      ///   String representation of the last error returned by the CUDA engine.
      /// </summary>
      property ErrorName       : String
                                 read  fget_ErrorName ;
      /// <summary>
      ///   String description of the last error returned by the CUDA engine.
      /// </summary>
      property ErrorString     : String
                                 read  fget_ErrorString ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoResource;
{$ENDIF}

{$IFDEF DCC}
  type
    PCUAnsiChar   = PAnsiChar ;
    TCUAnsiString = TBytes ;
{$ENDIF}
{$IFDEF CLR}
  type
    PCUAnsiChar   = String ;
    TCUAnsiString = TBytes ;
{$ENDIF}

const
  {$IFDEF MSWINDOWS_OS}
    NVCUDA_LIB = 'nvcuda.dll' ;
  {$ENDIF}
  {$IFDEF MACOSX_OS}
    { TODO : NOT TESTED }
    NVCUDA_LIB = '?' ;
  {$ENDIF}
  {$IFDEF IOS_OS}
    { TODO : NOT TESTED }
    NVCUDA_LIB = '?' ;
  {$ENDIF}
  {$IFDEF LINUX_OS}
    { TODO : NOT TESTED }
    NVCUDA_LIB = '?' ;
  {$ENDIF}
  {$IFDEF ANDROID_OS}
    { TODO : NOT TESTED }
    NVCUDA_LIB = '?' ;
  {$ENDIF}
  {$IFDEF UNKNOWN_OS}
  NVCUDA_LIB = '?';
  {$ENDIF}

var
  {$IFDEF DCC}
    libNVCUDA : IntPtr = 0 ;
  {$ENDIF}
  cudaAvailable     : Boolean = False ;
  cudaInitialized   : Boolean = False ;
  cudaEnabled       : Boolean = False ;
  cudaError         : Integer = 0 ;
  cudaEngine        : TGIS_CUDAEngine ;
  cudaDefaultDevice : TGIS_CUDADeviceInfo ;
  cudaActiveDevice  : TGIS_CUDADeviceInfo ;

{$IFDEF DCC}
  function AnsiStringToBytes ( const _str : String ) : TBytes ;
  var
    buf : TBytes ;
    len : Integer ;
  begin
    buf := TEncoding.ANSI.GetBytes( _str ) ;
    len := length( buf ) ;
    SetLength( buf, len + 1 ) ;
    buf[len] := 0 ;
    Result := buf ;
  end ;
{$ENDIF}

type
//==============================================================================
// CUDA type definitions
//==============================================================================

  CUdevice = NativeInt ;

  CUuuid = NativeUInt ;

  CUdeviceptr = NativeUInt ;

  CUctx_st = record
  end ;
  {$IFDEF DCC}
    CUcontext = ^CUctx_st ;
  {$ENDIF}
  {$IFDEF CLR}
    CUcontext = IntPtr ;
  {$ENDIF}

  CUmod_st = record
  end ;
  {$IFDEF DCC}
    CUmodule = ^CUmod_st ;
  {$ENDIF}
  {$IFDEF CLR}
    CUmodule = IntPtr ;
  {$ENDIF}

  CUfunc_st = record
  end ;
  {$IFDEF DCC}
    CUfunction = ^CUfunc_st ;
  {$ENDIF}
  {$IFDEF CLR}
    CUfunction = IntPtr ;
  {$ENDIF}

  CUarray_st = record
  end ;
  {$IFDEF DCC}
    CUarray = ^CUarray_st ;
  {$ENDIF}
  {$IFDEF CLR}
    CUarray = IntPtr ;
  {$ENDIF}

  CUtexref_st = record
  end ;
  {$IFDEF DCC}
    CUtexref = ^CUtexref_st ;
  {$ENDIF}
  {$IFDEF CLR}
    CUtexref = IntPtr ;
  {$ENDIF}

  CUsurfref_st = record
  end ;
  {$IFDEF DCC}
    CUsurfref = ^CUsurfref_st ;
  {$ENDIF}
  {$IFDEF CLR}
    CUsurfref = IntPtr ;
  {$ENDIF}

  CUevent_st = record
  end ;
  {$IFDEF DCC}
    CUevent = ^CUevent_st ;
  {$ENDIF}
  {$IFDEF CLR}
    CUevent = IntPtr ;
  {$ENDIF}

  CUstream_st = record
  end ;
  {$IFDEF DCC}
    CUstream = ^CUstream_st ;
  {$ENDIF}
  {$IFDEF CLR}
    CUstream = IntPtr ;
  {$ENDIF}

  CUgraphicsResource_st = record
  end ;
  {$IFDEF DCC}
    CUgraphicsResource = ^CUgraphicsResource_st ;
  {$ENDIF}
  {$IFDEF CLR}
    CUgraphicsResource = IntPtr ;
  {$ENDIF}

  CUresult = Integer ;

  T_cuResult = class
  public
  const
    CUDA_SUCCESS                              =   0 ;
    CUDA_ERROR_INVALID_VALUE                  =   1 ;
    CUDA_ERROR_OUT_OF_MEMORY                  =   2 ;
    CUDA_ERROR_NOT_INITIALIZED                =   3 ;
    CUDA_ERROR_DEINITIALIZED                  =   4 ;
    CUDA_ERROR_PROFILER_DISABLED              =   5 ;
    CUDA_ERROR_PROFILER_NOT_INITIALIZED       =   6 ; // deprecated
    CUDA_ERROR_PROFILER_ALREADY_STARTED       =   7 ; // deprecated
    CUDA_ERROR_PROFILER_ALREADY_STOPPED       =   8 ; // deprecated
    CUDA_ERROR_NO_DEVICE                      = 100 ; // deprecated
    CUDA_ERROR_INVALID_DEVICE                 = 101 ;
    CUDA_ERROR_INVALID_IMAGE                  = 200 ;
    CUDA_ERROR_INVALID_CONTEXT                = 201 ;
    CUDA_ERROR_CONTEXT_ALREADY_CURRENT        = 202 ;
    CUDA_ERROR_MAP_FAILED                     = 205 ; // deprecated
    CUDA_ERROR_UNMAP_FAILED                   = 206 ;
    CUDA_ERROR_ARRAY_IS_MAPPED                = 207 ;
    CUDA_ERROR_ALREADY_MAPPED                 = 208 ;
    CUDA_ERROR_NO_BINARY_FOR_GPU              = 209 ;
    CUDA_ERROR_ALREADY_ACQUIRED               = 210 ;
    CUDA_ERROR_NOT_MAPPED                     = 211 ;
    CUDA_ERROR_NOT_MAPPED_AS_ARRAY            = 212 ;
    CUDA_ERROR_NOT_MAPPED_AS_POINTER          = 213 ;
    CUDA_ERROR_ECC_UNCORRECTABLE              = 214 ;
    CUDA_ERROR_UNSUPPORTED_LIMIT              = 215 ;
    CUDA_ERROR_CONTEXT_ALREADY_IN_USE         = 216 ;
    CUDA_ERROR_PEER_ACCESS_UNSUPPORTED        = 217 ;
    CUDA_ERROR_INVALID_PTX                    = 218 ;
    CUDA_ERROR_INVALID_GRAPHICS_CONTEXT       = 219 ;
    CUDA_ERROR_NVLINK_UNCORRECTABLE           = 220 ;
    CUDA_ERROR_JIT_COMPILER_NOT_FOUND         = 221 ;
    CUDA_ERROR_INVALID_SOURCE                 = 300 ;
    CUDA_ERROR_FILE_NOT_FOUND                 = 301 ;
    CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND = 302 ;
    CUDA_ERROR_SHARED_OBJECT_INIT_FAILED      = 303 ;
    CUDA_ERROR_OPERATING_SYSTEM               = 304 ;
    CUDA_ERROR_INVALID_HANDLE                 = 400 ;
    CUDA_ERROR_NOT_FOUND                      = 500 ;
    CUDA_ERROR_NOT_READY                      = 600 ;
    CUDA_ERROR_ILLEGAL_ADDRESS                = 700 ;
    CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES        = 701 ;
    CUDA_ERROR_LAUNCH_TIMEOUT                 = 702 ;
    CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING  = 703 ;
    CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED    = 704 ;
    CUDA_ERROR_PEER_ACCESS_NOT_ENABLED        = 705 ;
    CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE         = 708 ;
    CUDA_ERROR_CONTEXT_IS_DESTROYED           = 709 ;
    CUDA_ERROR_ASSERT                         = 710 ;
    CUDA_ERROR_TOO_MANY_PEERS                 = 711 ;
    CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED = 712 ;
    CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED     = 713 ;
    CUDA_ERROR_HARDWARE_STACK_ERROR           = 714 ;
    CUDA_ERROR_ILLEGAL_INSTRUCTION            = 715 ;
    CUDA_ERROR_MISALIGNED_ADDRESS             = 716 ;
    CUDA_ERROR_INVALID_ADDRESS_SPACE          = 717 ;
    CUDA_ERROR_INVALID_PC                     = 718 ;
    CUDA_ERROR_LAUNCH_FAILED                  = 719 ;
    CUDA_ERROR_COOPERATIVE_LAUNCH_TOO_LARGE   = 720 ;
    CUDA_ERROR_NOT_PERMITTED                  = 800 ;
    CUDA_ERROR_NOT_SUPPORTED                  = 801 ;
    CUDA_ERROR_UNKNOWN                        = 999 ;
  end ;

  CUjit_option = (
    CU_JIT_MAX_REGISTERS = 0,
    CU_JIT_THREADS_PER_BLOCK,
    CU_JIT_WALL_TIME,
    CU_JIT_INFO_LOG_BUFFER,
    CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES,
    CU_JIT_ERROR_LOG_BUFFER,
    CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES,
    CU_JIT_OPTIMIZATION_LEVEL,
    CU_JIT_TARGET_FROM_CUCONTEXT,
    CU_JIT_TARGET,
    CU_JIT_FALLBACK_STRATEGY
  ) ;
  pCUjit_option = ^CUjit_option ;

  CUfunc_cache = (
    CU_FUNC_CACHE_PREFER_NONE   = $00,
    CU_FUNC_CACHE_PREFER_SHARED = $01,
    CU_FUNC_CACHE_PREFER_L1     = $02,
    CU_FUNC_CACHE_PREFER_EQUAL  = $03
  ) ;

  CUdevice_attribute = (
    CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK                 = 1,
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X                       = 2,
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y                       = 3,
    CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z                       = 4,
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X                        = 5,
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y                        = 6,
    CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z                        = 7,
    CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK           = 8,
    CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK               = 8,
    CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY                 = 9,
    CU_DEVICE_ATTRIBUTE_WARP_SIZE                             = 10,
    CU_DEVICE_ATTRIBUTE_MAX_PITCH                             = 11,
    CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK               = 12,
    CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK                   = 12,
    CU_DEVICE_ATTRIBUTE_CLOCK_RATE                            = 13,
    CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT                     = 14,
    CU_DEVICE_ATTRIBUTE_GPU_OVERLAP                           = 15,
    CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT                  = 16,
    CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT                   = 17,
    CU_DEVICE_ATTRIBUTE_INTEGRATED                            = 18,
    CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY                   = 19,
    CU_DEVICE_ATTRIBUTE_COMPUTE_MODE                          = 20,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_WIDTH               = 21,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_WIDTH               = 22,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_HEIGHT              = 23,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_WIDTH               = 24,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_HEIGHT              = 25,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_DEPTH               = 26,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LAYERED_WIDTH       = 27,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LAYERED_HEIGHT      = 28,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LAYERED_LAYERS      = 29,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_WIDTH         = 27,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_HEIGHT        = 28,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_NUMSLICES     = 29,
    CU_DEVICE_ATTRIBUTE_SURFACE_ALIGNMENT                     = 30,
    CU_DEVICE_ATTRIBUTE_CONCURRENT_KERNELS                    = 31,
    CU_DEVICE_ATTRIBUTE_ECC_ENABLED                           = 32,
    CU_DEVICE_ATTRIBUTE_PCI_BUS_ID                            = 33,
    CU_DEVICE_ATTRIBUTE_PCI_DEVICE_ID                         = 34,
    CU_DEVICE_ATTRIBUTE_TCC_DRIVER                            = 35,
    CU_DEVICE_ATTRIBUTE_MEMORY_CLOCK_RATE                     = 36,
    CU_DEVICE_ATTRIBUTE_GLOBAL_MEMORY_BUS_WIDTH               = 37,
    CU_DEVICE_ATTRIBUTE_L2_CACHE_SIZE                         = 38,
    CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_MULTIPROCESSOR        = 39,
    CU_DEVICE_ATTRIBUTE_ASYNC_ENGINE_COUNT                    = 40,
    CU_DEVICE_ATTRIBUTE_UNIFIED_ADDRESSING                    = 41,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_LAYERED_WIDTH       = 42,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_LAYERED_LAYERS      = 43,
    CU_DEVICE_ATTRIBUTE_CAN_TEX2D_GATHER                      = 44,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_GATHER_WIDTH        = 45,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_GATHER_HEIGHT       = 46,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_WIDTH_ALTERNATE     = 47,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_HEIGHT_ALTERNATE    = 48,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_DEPTH_ALTERNATE     = 49,
    CU_DEVICE_ATTRIBUTE_PCI_DOMAIN_ID                         = 50,
    CU_DEVICE_ATTRIBUTE_TEXTURE_PITCH_ALIGNMENT               = 51,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURECUBEMAP_WIDTH          = 52,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURECUBEMAP_LAYERED_WIDTH  = 53,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURECUBEMAP_LAYERED_LAYERS = 54,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE1D_WIDTH               = 55,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_WIDTH               = 56,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_HEIGHT              = 57,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE3D_WIDTH               = 58,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE3D_HEIGHT              = 59,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE3D_DEPTH               = 60,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE1D_LAYERED_WIDTH       = 61,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE1D_LAYERED_LAYERS      = 62,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_LAYERED_WIDTH       = 63,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_LAYERED_HEIGHT      = 64,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACE2D_LAYERED_LAYERS      = 65,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACECUBEMAP_WIDTH          = 66,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACECUBEMAP_LAYERED_WIDTH  = 67,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_SURFACECUBEMAP_LAYERED_LAYERS = 68,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_LINEAR_WIDTH        = 69,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LINEAR_WIDTH        = 70,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LINEAR_HEIGHT       = 71,
    CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_LINEAR_PITCH        = 72
  ) ;

  CUlimit = (
    CU_LIMIT_STACK_SIZE                       = $00,
    CU_LIMIT_PRINTF_FIFO_SIZE                 = $01,
    CU_LIMIT_MALLOC_HEAP_SIZE                 = $02,
    CU_LIMIT_DEV_RUNTIME_SYNC_DEPTH           = $03,
    CU_LIMIT_DEV_RUNTIME_PENDING_LAUNCH_COUNT = $04,
    CU_LIMIT_MAX
  ) ;

  CUsharedconfig = (
    CU_SHARED_MEM_CONFIG_DEFAULT_BANK_SIZE    = $00,
    CU_SHARED_MEM_CONFIG_FOUR_BYTE_BANK_SIZE  = $01,
    CU_SHARED_MEM_CONFIG_EIGHT_BYTE_BANK_SIZE = $02
  ) ;

{$IFDEF MACOS}
  {$DEFINE CDECL}
{$ENDIF}

{$IFDEF DCC}
type
//==============================================================================
// CUDA method definitions
//==============================================================================

  // error handling

  TcuGetErrorName = function (
          _error : CUresult ;
      var _pStr  : PCUAnsiChar
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuGetErrorString = function (
          _error : CUresult ;
      var _pStr  : PCUAnsiChar
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  // initialization

  TcuInit = function (
          _flags : Cardinal
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  // driver management

  TcuDriverGetVersion = function (
      var _driverVersion : Integer
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  // device management

  TcuDeviceGet = function (
      var _device  : CUdevice ;
          _ordinal : Integer
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuDeviceGetAttribute = function (
      var _pi     : Integer ;
          _attrib : CUdevice_attribute ;
          _dev    : CUdevice
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuDeviceGetCount = function (
      var _count : Integer
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuDeviceGetName = function (
          _name : PCUAnsiChar ;
          _len  : Integer ;
          _dev  : CUdevice
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuDeviceGetUuid = function (
      var _uuid : CUuuid ;
          _dev  : CUdevice
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuDeviceTotalMem = function (
      var _bytes : NativeUInt ;
          _dev   : CUdevice
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  //context management

  TcuCtxCreate = function (
      var _pctx  : CUcontext ;
          _flags : Cardinal ;
          _dev   : CUdevice
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxDestroy = function (
          _pctx : CUcontext
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxGetApiVersion = function (
          _pctx    : CUcontext ;
      var _version : Cardinal
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxGetCacheConfig = function (
      var _config : CUfunc_cache
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxGetCurrent = function (
      var _pctx : CUcontext
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxGetDevice = function (
      var _device : CUdevice
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxGetFlags = function (
      var _flags : Cardinal
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxGetLimit = function (
      var _pvalue : NativeUInt ;
          _limit  : CUlimit
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxGetSharedMemConfig = function (
      var _pConfig : CUsharedconfig
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxGetStreamPriorityRange = function (
      var _leastPriority    : Integer ;
      var _greatestPriority : Integer
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxPopCurrent = function (
      var _pctx : CUcontext
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxPushCurrent = function (
          _ctx : CUcontext
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxSetCacheConfig = function (
          _config : CUfunc_cache
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxSetCurrent = function (
          _ctx : CUcontext
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxSetLimit  = function (
          _limit : CUlimit ;
          _value : NativeUInt
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxSetSharedMemConfig = function (
          _config : CUsharedconfig
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuCtxSynchronize = function (
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  // module management

  TcuModuleGetFunction = function (
      var _hfunc : CUfunction ;
          _hmod  : CUmodule ;
          _name  : PCUAnsiChar
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuModuleLoadData = function (
      var _module : CUmodule ;
          _image  : PCUAnsiChar
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuModuleLoadDataEx = function (
      var _module       : CUmodule ;
          _image        : PCUAnsiChar ;
          _numOptions   : Cardinal ;
          _options      : pCUjit_option ;
          _optionValues : Pointer
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuModuleUnload = function (
          _module : CUmodule
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  // memory management

  TcuMemAlloc = function (
      var _dptr     : CUdeviceptr ;
          _bytesize : NativeUInt
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuMemFree = function (
          _dptr : CUdeviceptr
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuMemcpyDtoH = function (
          _dstHost   : Pointer ;
          _srcDevice : CUdeviceptr ;
          _byteCount : NativeUInt
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuMemcpyHtoD = function (
          _dstDevice : CUdeviceptr ;
          _srcHost   : Pointer ;
          _byteCount : NativeUInt
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  // event management

  TcuEventCreate = function (
      var _phEvent : CUevent ;
          _flags   : Cardinal
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuEventDestroy = function (
          _hEvent : CUevent
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuEventElapsedTime = function (
      var _pMilliseconds : Single ;
          _hStart        : CUevent ;
          _hEnd          : CUevent
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuEventQuery = function (
          _hEvent : CUevent
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuEventRecord = function (
          _hEvent  : CUevent ;
          _hStream : CUstream
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TcuEventSynchronize = function (
          _hEvent : CUevent
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  // execution control

  TcuLaunchKernel = function (
          _f              : CUfunction ;
          _gridDimX       : Cardinal ;
          _gridDimY       : Cardinal ;
          _gridDimZ       : Cardinal ;
          _blockDimX      : Cardinal ;
          _blockDimY      : Cardinal ;
          _blockDimz      : Cardinal ;
          _sharedMemBytes : Cardinal ;
          _hStream        : CUstream ;
          _kernelParams   : Pointer ;
          _extra          : Pointer
  ) : CUresult ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

{$ENDIF}


{$IFDEF DCC}
var
//==============================================================================
// CUDA method declarations
//==============================================================================

  // error handling
  cuGetErrorName              : TcuGetErrorName ;
  cuGetErrorString            : TcuGetErrorString ;
  // initialization
  cuInit                      : TcuInit ;
  // driver management
  cuDriverGetVersion          : TcuDriverGetVersion ;
  // device management
  cuDeviceGet                 : TcuDeviceGet ;
  cuDeviceGetAttribute        : TcuDeviceGetAttribute ;
  cuDeviceGetCount            : TcuDeviceGetCount ;
  cuDeviceGetName             : TcuDeviceGetName ;
  cuDeviceGetUuid             : TcuDeviceGetUuid ;
  cuDeviceTotalMem            : TcuDeviceTotalMem ;
  //context management
  cuCtxCreate                 : TcuCtxCreate ;
  cuCtxDestroy                : TcuCtxDestroy ;
  cuCtxGetApiVersion          : TcuCtxGetApiVersion ;
  cuCtxGetCacheConfig         : TcuCtxGetCacheConfig ;
  cuCtxGetCurrent             : TcuCtxGetCurrent ;
  cuCtxGetDevice              : TcuCtxGetDevice ;
  cuCtxGetFlags               : TcuCtxGetFlags ;
  cuCtxGetLimit               : TcuCtxGetLimit ;
  cuCtxGetSharedMemConfig     : TcuCtxGetSharedMemConfig ;
  cuCtxGetStreamPriorityRange : TcuCtxGetStreamPriorityRange ;
  cuCtxPopCurrent             : TcuCtxPopCurrent ;
  cuCtxPushCurrent            : TcuCtxPushCurrent ;
  cuCtxSetCacheConfig         : TcuCtxSetCacheConfig ;
  cuCtxSetCurrent             : TcuCtxSetCurrent ;
  cuCtxSetLimit               : TcuCtxSetLimit ;
  cuCtxSetSharedMemConfig     : TcuCtxSetSharedMemConfig ;
  cuCtxSynchronize            : TcuCtxSynchronize ;
  // module management
  cuModuleGetFunction         : TcuModuleGetFunction ;
  cuModuleLoadData            : TcuModuleLoadData ;
  cuModuleLoadDataEx          : TcuModuleLoadDataEx ;
  cuModuleUnload              : TcuModuleUnload ;
  // memory management
  cuMemAlloc                  : TcuMemAlloc ;
  cuMemFree                   : TcuMemFree ;
  cuMemcpyDtoH                : TcuMemcpyDtoH ;
  cuMemcpyHtoD                : TcuMemcpyHtoD ;
  // event management
  cuEventCreate               : TcuEventCreate ;
  cuEventDestroy              : TcuEventDestroy ;
  cuEventElapsedTime          : TcuEventElapsedTime ;
  cuEventQuery                : TcuEventQuery ;
  cuEventRecord               : TcuEventRecord ;
  cuEventSynchronize          : TcuEventSynchronize ;
  // execution control
  cuLaunchKernel              : TcuLaunchKernel ;

{$ENDIF}


//==============================================================================
// CUDA library loading
//==============================================================================

  procedure loadNVCUDA ;
  {$IFDEF CLR}
    var
      path : String ;
  {$ENDIF}
  begin
    cudaAvailable := False ;
    {$IFDEF DCC}
      libNVCUDA := LoadLibrary( NVCUDA_LIB ) ;
      cudaAvailable := libNVCUDA <> 0 ;
    {$ENDIF}
    {$IFDEF CLR}
      path := System.Environment.SystemDirectory + '\' + NVCUDA_LIB ;
      cudaAvailable := FileExists( path ) ;
    {$ENDIF}
  end ;


  procedure freeNVCUDA ;
  begin
    {$IFDEF DCC}
      if cudaAvailable then
        FreeLibrary( HMODULE( libNVCUDA ) ) ;
    {$ENDIF}
  end ;


  procedure initNVCUDA ;
    {$IFDEF DCC}
      function attach(
        const _name : String
      ) : Pointer ;
      begin
        Result := GetProcAddress( libNVCUDA, PChar( _name ) ) ;
      end ;
   {$ENDIF}
  begin
    {$IFDEF DCC}
      // error handling
      cuGetErrorName :=
        attach( 'cuGetErrorName' ) ;
      cuGetErrorString :=
        attach( 'cuGetErrorString' ) ;
      // initialization
      cuInit :=
        attach( 'cuInit' ) ;
      // driver management
      cuDriverGetVersion :=
        attach( 'cuDriverGetVersion' ) ;
      // device management
      cuDeviceGet :=
        attach( 'cuDeviceGet' ) ;
      cuDeviceGetAttribute :=
        attach( 'cuDeviceGetAttribute' ) ; // unused
      cuDeviceGetCount :=
        attach( 'cuDeviceGetCount' ) ;
      cuDeviceGetName :=
        attach( 'cuDeviceGetName' ) ;
      cuDeviceGetUuid :=
        attach( 'cuDeviceGetUuid' ) ; // unused
      cuDeviceTotalMem :=
        attach( 'cuDeviceTotalMem' ) ;
      //context management
      cuCtxCreate :=
        attach( 'cuCtxCreate_v2' ) ; // v2
      cuCtxDestroy :=
        attach( 'cuCtxDestroy_v2' ) ; // v2
      cuCtxGetApiVersion :=
        attach( 'cuCtxGetApiVersion' ) ;
      cuCtxGetCacheConfig :=
        attach( 'cuCtxGetCacheConfig' ) ; // unused
      cuCtxGetCurrent :=
        attach( 'cuCtxGetCurrent' ) ; // unused
      cuCtxGetDevice :=
        attach( 'cuCtxGetDevice' ) ; // unused
      cuCtxGetFlags :=
        attach( 'cuCtxGetFlags' ) ; // unused
      cuCtxGetLimit :=
        attach( 'cuCtxGetLimit' ) ; // unused
      cuCtxGetSharedMemConfig :=
        attach( 'cuCtxGetSharedMemConfig' ) ; // unused
      cuCtxGetStreamPriorityRange :=
        attach( 'cuCtxGetStreamPriorityRange' ) ; // unused
      cuCtxPopCurrent :=
        attach( 'cuCtxPopCurrent' ) ; // unused
      cuCtxPushCurrent :=
        attach( 'cuCtxPushCurrent' ) ; // unused
      cuCtxSetCacheConfig :=
        attach( 'cuCtxSetCacheConfig' ) ; // unused
      cuCtxSetCurrent :=
        attach( 'cuCtxSetCurrent' ) ; // unused
      cuCtxSetLimit :=
        attach( 'cuCtxSetLimit' ) ; // unused
      cuCtxSetSharedMemConfig :=
        attach( 'cuCtxSetSharedMemConfig' ) ; // unused
      cuCtxSynchronize :=
        attach( 'cuCtxSynchronize' ) ;
      // module management
      cuModuleGetFunction :=
        attach( 'cuModuleGetFunction' ) ;
      cuModuleLoadData :=
        attach( 'cuModuleLoadData' ) ;
      cuModuleLoadDataEx :=
        attach( 'cuModuleLoadDataEx' ) ; // unused
      cuModuleUnload :=
        attach( 'cuModuleUnload' ) ; // unused
      // memory management
      cuMemAlloc :=
        attach( 'cuMemAlloc_v2' ) ; // v2
      cuMemFree :=
        attach( 'cuMemFree_v2' ) ; // v2
      cuMemcpyDtoH :=
        attach( 'cuMemcpyDtoH_v2' ) ; // v2
      cuMemcpyHtoD :=
        attach( 'cuMemcpyHtoD_v2' ) ; // v2
      // event management
      cuEventCreate :=
        attach( 'cuEventCreate' ) ; // unused
      cuEventDestroy :=
        attach( 'cuEventDestroy_v2' ) ; // v2, unused
      cuEventElapsedTime :=
        attach( 'cuEventElapsedTime' ) ; // unused
      cuEventQuery :=
        attach( 'cuEventQuery' ) ; // unused
      cuEventRecord :=
        attach( 'cuEventRecord' ) ; // unused
      cuEventSynchronize :=
        attach( 'cuEventSynchronize' ) ; // unused
      // execution control
      cuLaunchKernel :=
        attach( 'cuLaunchKernel' ) ;
    {$ENDIF}
  end ;


{$IFDEF CLR}
type
//==============================================================================
// CUDA method definitions
//==============================================================================

  // error handling

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuGetErrorName (
          _error : CUresult ;
      var _pStr  : PCUAnsiChar
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuGetErrorString (
          _error : CUresult ;
      var _pStr  : PCUAnsiChar
  ) : CUresult ; external ;

  // initialization

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuInit (
          _flags : Cardinal
  ) : CUresult ; external ;

  // driver management

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuDriverGetVersion (
      out _driverVersion : Integer
  ) : CUresult ; external ;

  // device management

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuDeviceGet (
      out _device  : CUdevice ;
          _ordinal : Integer
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuDeviceGetAttribute (
      out _pi     : Integer ;
          _attrib : CUdevice_attribute ;
          _dev    : CUdevice
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuDeviceGetCount (
      out _count : Integer
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuDeviceGetName (
          _name : IntPtr ;
          _len  : Integer ;
          _dev  : CUdevice
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuDeviceGetUuid (
      out _uuid : CUuuid ;
          _dev  : CUdevice
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuDeviceTotalMem (
      out _bytes : NativeUInt ;
          _dev   : CUdevice
  ) : CUresult ; external ;

  //context management

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi, EntryPoint = 'cuCtxCreate_v2'
    )
  ]
  function cuCtxCreate (
      out _pctx  : CUcontext ;
          _flags : Cardinal ;
          _dev   : CUdevice
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi, EntryPoint = 'cuCtxDestroy_v2'
    )
  ]
  function cuCtxDestroy (
          _pctx : CUcontext
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxGetApiVersion (
          _pctx    : CUcontext ;
      out _version : Cardinal
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxGetCacheConfig (
      out _config : CUfunc_cache
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxGetCurrent (
      out _pctx : CUcontext
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxGetDevice (
      out _device : CUdevice
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxGetFlags (
      out _flags : Cardinal
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxGetLimit (
      out _pvalue : NativeUInt ;
          _limit  : CUlimit
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxGetSharedMemConfig (
      out _pConfig : CUsharedconfig
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxGetStreamPriorityRange (
      out _leastPriority    : Integer ;
      out _greatestPriority : Integer
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxPopCurrent (
      out _pctx : CUcontext
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxPushCurrent (
          _ctx : CUcontext
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxSetCacheConfig (
          _config : CUfunc_cache
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxSetCurrent (
          _ctx : CUcontext
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxSetLimit (
          _limit : CUlimit ;
          _value : NativeUInt
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxSetSharedMemConfig (
          _config : CUsharedconfig
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuCtxSynchronize (
  ) : CUresult ; external ;

  // module management

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuModuleGetFunction (
      out _hfunc : CUfunction ;
          _hmod  : CUmodule ;
          _name  : PCUAnsiChar
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuModuleLoadData (
      out _module : CUmodule ;
          _image  : PCUAnsiChar
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuModuleLoadDataEx (
      out _module       : CUmodule ;
          _image        : PCUAnsiChar ;
          _numOptions   : Cardinal ;
          _options      : pCUjit_option ;
          _optionValues : Pointer
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuModuleUnload (
          _module : CUmodule
  ) : CUresult ; external ;

  // memory management

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi, EntryPoint = 'cuMemAlloc_v2'
    )
  ]
  function cuMemAlloc (
      out _dptr     : CUdeviceptr ;
          _bytesize : NativeUInt
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi, EntryPoint = 'cuMemFree_v2'
    )
  ]
  function cuMemFree (
          _dptr : CUdeviceptr
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi, EntryPoint = 'cuMemcpyDtoH_v2'
    )
  ]
  function cuMemcpyDtoH (
          _dstHost   : IntPtr ;
          _srcDevice : CUdeviceptr ;
          _byteCount : NativeUInt
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi, EntryPoint = 'cuMemcpyHtoD_v2'
    )
  ]
  function cuMemcpyHtoD (
          _dstDevice : CUdeviceptr ;
          _srcHost   : IntPtr ;
          _byteCount : NativeUInt
  ) : CUresult ; external ;

  // event management

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuEventCreate (
      var _phEvent : CUevent ;
          _flags   : Cardinal
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi, EntryPoint = 'cuEventDestroy_v2'
    )
  ]
  function cuEventDestroy (
          _hEvent : CUevent
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuEventElapsedTime (
      var _pMilliseconds : Single ;
          _hStart        : CUevent ;
          _hEnd          : CUevent
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuEventQuery (
          _hEvent : CUevent
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuEventRecord (
          _hEvent  : CUevent ;
          _hStream : CUstream
  ) : CUresult ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuEventSynchronize (
          _hEvent : CUevent
  ) : CUresult ; external ;

  // execution control

  [ SuppressUnmanagedCodeSecurity,
    DllImport( NVCUDA_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function cuLaunchKernel (
          _f              : CUfunction ;
          _gridDimX       : Cardinal ;
          _gridDimY       : Cardinal ;
          _gridDimZ       : Cardinal ;
          _blockDimX      : Cardinal ;
          _blockDimY      : Cardinal ;
          _blockDimz      : Cardinal ;
          _sharedMemBytes : Cardinal ;
          _hStream        : CUstream ;
          _kernelParams   : IntPtr ;
          _extra          : IntPtr
  ) : CUresult ; external ;

{$ENDIF}


type
//==============================================================================
// CUDA type wrappers
//==============================================================================

  T_cuBuffer = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      iBuffer      : CUdeviceptr ;
      __RESERVED__ : Integer ;
      bClone       : Boolean ;
    private
      FSize  : NativeUInt ;
      FIndex : Integer ;
    public
      constructor Create ( const _size : NativeUInt ;
                           {$IFDEF DCC}
                             const _buf : Pointer ;
                           {$ENDIF}
                           {$IFDEF CLR}
                             const _buf : IntPtr ;
                           {$ENDIF}
                           const _idx  : Integer
                         ) ; overload ;
      constructor Create ( const _idx  : Integer ;
                           const _buf  : T_cuBuffer
                         ) ; overload ;
    protected
      procedure doDestroy ; override ;
    public
      procedure ReadBuffer ( const _size  : NativeUInt ;
                             {$IFDEF DCC}
                               const _buf : Pointer
                             {$ENDIF}
                             {$IFDEF CLR}
                               const _buf : IntPtr
                             {$ENDIF}
                           ) ;
    public
      property Size       : NativeUInt
                            read  FSize ;
      property ParamIndex : Integer
                            read FIndex ;
  end ;


  T_cuKernel = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pKernel : CUfunction ;
    private
      {$IFDEF DCC}
        oParamList : TObjectList<T_cuBuffer> ;
      {$ENDIF}
      {$IFDEF CLR}
        oParamList : TList<T_cuBuffer> ;
      {$ENDIF}
    private
      FName : String ;
    private
      function  fget_Argument       ( const _idx : Integer
                                    ) : T_cuBuffer ;
    public
      constructor Create ( const _mod  : CUmodule ;
                           const _name : String
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      function  SetArgument  ( const _idx    : Integer ;
                               const _size   : Integer ;
                               {$IFDEF DCC}
                                 const _data : Pointer
                               {$ENDIF}
                               {$IFDEF CLR}
                                 const _data : IntPtr
                               {$ENDIF}
                             ) : Boolean ; overload ;
      function  SetArgument  ( const _idx    : Integer ;
                               const _buf    : T_cuBuffer
                             ) : Boolean ; overload ;
      function  Launch       : Boolean ;
    public
      property Name           : String
                                read  FName ;
      property Argument       [const _idx : Integer]
                              : T_cuBuffer
                                read  fget_Argument ;
    public
      GridDimensions  : array of NativeUInt ;
      BlockDimensions : array of NativeUInt ;
      IsArgumentSet  : array of Boolean ;
  end ;


  T_cuProgram = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pModule : CUmodule ;
      bLoaded : Boolean ;
    public
      constructor Create ( const _img : String
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      function CreateKernel ( const _name : String
                            ) : T_cuKernel ;
  end ;


  T_cuContext = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pContext : CUcontext ;
    private
      FAPIVersion : Cardinal ;
    public
      constructor Create ( const _dev : CUdevice
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      property APIVersion : Cardinal
                            read  FAPIVersion ;
  end ;


  T_cuDevice = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      iDevice     : CUdevice ;
      oDeviceInfo : TGIS_CUDADeviceInfo ;
    private
      FName     : String ;
      FTotalMem : NativeUInt ;
    private
      FContext : T_cuContext ;
    public
      constructor Create ( const _dev : CUdevice
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      property Name        : String
                             read  FName ;
      property TotalMemory : NativeUInt
                             read  FTotalMem ;
    public
      property Context     : T_cuContext
                             read  FContext ;
  end ;


var
  {$IFDEF DCC}
    cudaDevices : TObjectList<T_cuDevice> ;
  {$ENDIF}
  {$IFDEF CLR}
    cudaDevices : TList<T_cuDevice> ;
  {$ENDIF}


//==============================================================================
// T_cuBuffer
//==============================================================================

  constructor T_cuBuffer.Create(
    const _size : NativeUInt ;
    {$IFDEF DCC}
      const _buf : Pointer ;
    {$ENDIF}
    {$IFDEF CLR}
      const _buf : IntPtr ;
    {$ENDIF}
    const _idx  : Integer
  ) ;
  var
    {$IFDEF DCC}
      buf : Pointer ;
    {$ENDIF}
    {$IFDEF CLR}
      buf : IntPtr ;
      gch : GCHandle ;
    {$ENDIF}
    arr : TBytes ;
  begin
    inherited Create ;

    __RESERVED__ := $00000000 ;

    cudaError := cuMemAlloc( iBuffer, _size ) ;

    if cudaError <> T_cuResult.CUDA_SUCCESS then
      exit ;

    if _buf = nil then begin
      SetLength( arr, _size ) ;
      {$IFDEF DCC}
        buf := @arr[0] ;
        FillChar( buf^, _size, 0 ) ;
      {$ENDIF}
      {$IFDEF CLR}
        Array.Clear( arr, 0, _size ) ;
        gch := GCHandle.Alloc( arr, GCHandleType.Pinned ) ;
        buf := gch.AddrOfPinnedObject ;
      {$ENDIF}
    end else
      buf := _buf ;

    cudaError := cuMemcpyHtoD( iBuffer, buf, _size ) ;

    {$IFDEF CLR}
      if _buf = nil then
        gch.Free ;
    {$ENDIF}

    FSize  := _size ;
    FIndex := _idx ;

    bClone := False ;
  end ;


  constructor T_cuBuffer.Create(
    const _idx : Integer ;
    const _buf : T_cuBuffer
  ) ;
  begin
    inherited Create ;

    __RESERVED__ := $00000000 ;

    iBuffer := _buf.iBuffer ;

    FSize  := _buf.Size ;
    FIndex := _idx ;

    bClone := True ;
  end ;


  procedure T_cuBuffer.doDestroy ;
  begin
    if not bClone then
      cuMemFree( iBuffer ) ;

    inherited ;
  end ;


  procedure T_cuBuffer.ReadBuffer(
    const _size  : NativeUInt ;
    {$IFDEF DCC}
      const _buf : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _buf : IntPtr
    {$ENDIF}
  ) ;
  begin
    cudaError := cuMemcpyDtoH( _buf, iBuffer, _size ) ;
  end ;


//==============================================================================
// T_cuKernel
//==============================================================================

  constructor T_cuKernel.Create(
    const _mod  : CUmodule ;
    const _name : String
  ) ;
  var
    {$IFDEF DCC}
      str : TCUAnsiString ;
    {$ENDIF}
    i     : Integer ;
  begin
    inherited Create ;

    FName := _name ;

    {$IFDEF DCC}
      str := AnsiStringToBytes( _name ) ;
      cudaError := cuModuleGetFunction( pKernel, _mod, @str[0] ) ;
    {$ENDIF}
    {$IFDEF CLR}
      cudaError := cuModuleGetFunction( pKernel, _mod, _name ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oParamList := TObjectList<T_cuBuffer>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      oParamList := TList<T_cuBuffer>.Create ;
    {$ENDIF}

    SetLength(  GridDimensions, 3 ) ;
    SetLength( BlockDimensions, 3 ) ;
    for i := 0 to 2 do begin
      GridDimensions[i] := 1 ;
      BlockDimensions[i] := 1 ;
    end ;

    SetLength( IsArgumentSet, 32 ) ;
    for i := 0 to 32 - 1 do
      IsArgumentSet[i] := False ;
  end ;


  procedure T_cuKernel.doDestroy ;
  begin
    FreeObject( oParamList ) ;

    inherited ;
  end ;


  function T_cuKernel.fget_Argument(
    const _idx : Integer
  ) : T_cuBuffer ;
  var
    buf : T_cuBuffer ;
    i   : Integer ;
  begin
    buf := nil ;
    for i := 0 to oParamList.Count - 1 do begin
      buf := oParamList[i] ;
      if buf.ParamIndex = _idx then
        break ;
    end ;
    Result := buf ;
  end ;


  function T_cuKernel.SetArgument(
    const _idx    : Integer ;
    const _size   : Integer ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) : Boolean ;
  var
    buf : T_cuBuffer ;
    tmp : T_cuBuffer ;
    i   : Integer ;
  begin
    Result := False ;

    buf := T_cuBuffer.Create( _size, _data, _idx ) ;

    if cudaError <> T_cuResult.CUDA_SUCCESS then begin
      FreeObject( buf ) ;
      exit ;
    end ;

    for i := 0 to oParamList.Count - 1 do begin
      tmp := oParamList[i] ;
      if tmp.ParamIndex = _idx then begin
        {$IFDEF DCC}
          oParamList.Delete( i ) ;
        {$ENDIF}
        {$IFDEF CLR}
          oParamList.RemoveAt( i ) ;
        {$ENDIF}
        break ;
      end ;
    end ;
    oParamList.Add( buf ) ;

    IsArgumentSet[_idx] := True ;

    Result := True ;
  end ;


  function T_cuKernel.SetArgument(
    const _idx : Integer ;
    const _buf : T_cuBuffer
  ) : Boolean ;
  var
    buf : T_cuBuffer ;
    tmp : T_cuBuffer ;
    i   : Integer ;
  begin
    Result := False ;

    buf := T_cuBuffer.Create( _idx, _buf ) ;

    for i := 0 to oParamList.Count - 1 do begin
      tmp := oParamList[i] ;
      if tmp.ParamIndex = _idx then begin
        {$IFDEF DCC}
          oParamList.Delete( i ) ;
        {$ENDIF}
        {$IFDEF CLR}
          oParamList.RemoveAt( i ) ;
        {$ENDIF}
        break ;
      end ;
    end ;
    oParamList.Add( buf ) ;

    IsArgumentSet[_idx] := True ;

    Result := True ;
  end ;


  function T_cuKernel.Launch : Boolean ;
  var
    buf    : T_cuBuffer ;
    {$IFDEF DCC}
      arr  : array of Pointer ;
    {$ENDIF}
    {$IFDEF CLR}
      agch : array of GCHandle ;
      arr  : array of IntPtr ;
      gch  : GCHandle ;
    {$ENDIF}
    i      : Integer ;
    k      : Integer ;
  begin
    Result := False ;
    buf := nil ;
    {$IFDEF CLR}
      SetLength( agch, oParamList.Count ) ;
    {$ENDIF}

    SetLength( arr, oParamList.Count ) ;
    for i := 0 to oParamList.Count - 1 do begin
      for k := 0 to oParamList.Count - 1 do begin
        buf := oParamList[k] ;
        if buf.ParamIndex = i then
          break ;
      end ;
      {$IFDEF DCC}
        arr[i] := @(buf.iBuffer) ;
      {$ENDIF}
      {$IFDEF CLR}
        agch[i] := GCHandle.Alloc( buf.iBuffer, GCHandleType.Pinned  ) ;
        arr[i] := agch[i].AddrOfPinnedObject ;
      {$ENDIF}
    end ;

    {$IFDEF DCC}
      cudaError := cuLaunchKernel(
        pKernel,
        GridDimensions[0],
        GridDimensions[1],
        GridDimensions[2],
        BlockDimensions[0],
        BlockDimensions[1],
        BlockDimensions[2],
        0, nil, @arr[0], nil
      ) ;
    {$ENDIF}
    {$IFDEF CLR}
      gch := GCHandle.Alloc( arr, GCHandleType.Pinned ) ;
      cudaError := cuLaunchKernel(
        pKernel,
        GridDimensions[0],
        GridDimensions[1],
        GridDimensions[2],
        BlockDimensions[0],
        BlockDimensions[1],
        BlockDimensions[2],
        0, nil, gch.AddrOfPinnedObject, nil
      ) ;
    {$ENDIF}

    if cudaError <> T_cuResult.CUDA_SUCCESS then
      exit ;

    cudaError := cuCtxSynchronize ;

    {$IFDEF CLR}
      gch.Free ;
      for i := 0 to oParamList.Count - 1 do
        agch[i].Free ;
    {$ENDIF}

    if cudaError <> T_cuResult.CUDA_SUCCESS then
      exit ;

    Result := True ;
  end ;


//==============================================================================
// T_cuModule
//==============================================================================

  constructor T_cuProgram.Create(
    const _img : String
  ) ;
  {$IFDEF DCC}
    var
      str : TCUAnsiString ;
  {$ENDIF}
  begin
    inherited Create ;

    bLoaded := False ;

    {$IFDEF DCC}
      str := AnsiStringToBytes( _img ) ;
      cudaError := cuModuleLoadData( pModule, @str[0] ) ;
    {$ENDIF}
    {$IFDEF CLR}
      cudaError := cuModuleLoadData( pModule, _img ) ;
    {$ENDIF}

    if cudaError <> T_cuResult.CUDA_SUCCESS then
      exit ;

    bLoaded := True ;
  end ;

  procedure T_cuProgram.doDestroy ;
  begin

    inherited ;
  end ;


  function T_cuProgram.CreateKernel(
    const _name : String
  ) : T_cuKernel ;
  begin
    Result := T_cuKernel.Create( pModule, _name ) ;
  end ;


//==============================================================================
// T_cuContext
//==============================================================================

  constructor T_cuContext.Create(
    const _dev : CUdevice
  ) ;
  begin
    inherited Create ;

    cudaError := cuCtxCreate( pContext, 0, _dev ) ;

    cudaError := cuCtxGetApiVersion( pContext, FAPIVersion ) ;
  end ;


  procedure T_cuContext.doDestroy ;
  begin
    cuCtxDestroy( pContext ) ;

    inherited ;
  end ;


//==============================================================================
// T_cuDevice
//==============================================================================

  constructor T_cuDevice.Create(
    const _dev : CUdevice
  ) ;
  var
    val   : TCUAnsiString ;
    {$IFDEF CLR}
      gch : GCHandle ;
    {$ENDIF}
  begin
    inherited Create ;

    iDevice := _dev ;

    {$IFDEF DCC}
      SetLength( val, 256 ) ;
      cudaError := cuDeviceGetName( @val[0], 256, iDevice ) ;
      FName := ConvertAnsiString( val ) ;
    {$ENDIF}
    {$IFDEF CLR}
      SetLength( val, 256 ) ;
      gch := GCHandle.Alloc( val, GCHandleType.Pinned ) ;
      cudaError := cuDeviceGetName( gch.AddrOfPinnedObject, 256, iDevice ) ;
      gch.Free ;
      FName := ConvertAnsiString( val ) ;
    {$ENDIF}

    cudaError := cuDeviceTotalMem( FTotalMem, iDevice ) ;

    FContext := T_cuContext.Create( iDevice ) ;
  end ;


  procedure T_cuDevice.doDestroy ;
  begin
    FreeObject( FContext ) ;

    inherited ;
  end ;


//==============================================================================
// TGIS_CUDADeviceInfo
//==============================================================================

  constructor TGIS_CUDADeviceInfo.Create;
  begin
    inherited ;

  end ;


  procedure TGIS_CUDADeviceInfo.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_CUDADeviceInfo.fget_Name
    : String ;
  begin
    Result := T_cuDevice( oDevice ).Name ;
  end ;


  function TGIS_CUDADeviceInfo.fget_TotalMemory
    : NativeUInt ;
  begin
    Result := T_cuDevice( oDevice ).TotalMemory ;
  end ;


  function TGIS_CUDADeviceInfo.fget_APIVersion
    : Cardinal ;
  begin
    Result := T_cuDevice( oDevice ).Context.APIVersion ;
  end ;


//==============================================================================
// TGIS_CUDAEngine
//==============================================================================

  constructor TGIS_CUDAEngine.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_CUDAEngine.doDestroy ;
  begin
    FreeObject( FDevices ) ;

    inherited ;
  end ;


  function TGIS_CUDAEngine.fget_DriverVersion
    : Integer ;
  var
    ver : Integer ;
  begin
    cudaError := cuDriverGetVersion( ver ) ;
    Result := ver ;
  end ;


  function TGIS_CUDAEngine.fget_Available
    : Boolean ;
  var
    cnt : Integer ;
    dev : T_cuDevice ;
    inf : TGIS_CUDADeviceInfo ;
    id  : CUdevice ;
    i   : Integer ;
  begin
    Result := False ;

    if GisOpenCLEngine.Initialized then
      exit ;

    if cudaAvailable then begin
      Result := True ;
      exit ;
    end ;

    loadNVCUDA ;
    if not cudaAvailable then begin
      freeNVCUDA ;
      exit ;
    end ;

    try
      initNVCUDA ;
    except
      freeNVCUDA ;
      cudaAvailable := False ;
      exit ;
    end ;

    cudaError := cuInit( 0 ) ;
    if cudaError <> T_cuResult.CUDA_SUCCESS then begin
      freeNVCUDA ;
      cudaAvailable := False ;
      exit ;
    end ;

    cudaError := cuDeviceGetCount( cnt ) ;
    if ( cudaError <> T_cuResult.CUDA_SUCCESS ) or ( cnt = 0 ) then begin
      freeNVCUDA ;
      cudaAvailable := False ;
      exit ;
    end ;

    {$IFDEF DCC}
      cudaDevices := TObjectList<T_cuDevice>.Create ;
      FDevices := TObjectList<TGIS_CUDADeviceInfo>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      cudaDevices := TList<T_cuDevice>.Create ;
      FDevices := TList<TGIS_CUDADeviceInfo>.Create ;
    {$ENDIF}
    for i := 0 to cnt - 1 do begin
      cuDeviceGet( id, i ) ;
      dev := T_cuDevice.Create( id ) ;
      inf := TGIS_CUDADeviceInfo.Create ;
      inf.oDevice := dev ;
      dev.oDeviceInfo := inf ;

      cudaDevices.Add( dev ) ;
      FDevices.Add( inf ) ;
    end ;

    cudaInitialized := True ;

    Result := True ;
  end ;


  function TGIS_CUDAEngine.fget_Initialized
    : Boolean ;
  begin
    Result := cudaInitialized ;
  end ;


  function TGIS_CUDAEngine.fget_Enabled
    : Boolean ;
  begin
    if GisOpenCLEngine.Initialized then
      cudaEnabled := False ;

    Result := cudaEnabled ;
  end ;


  procedure TGIS_CUDAEngine.fset_Enabled(
    const _val : Boolean
  ) ;
  begin
    if Available then
      cudaEnabled := _val ;
  end ;


  function TGIS_CUDAEngine.fget_DeviceCount
    : Cardinal ;
  begin
    if assigned( cudaDevices ) then
      Result := cudaDevices.Count
    else
      Result := 0 ;
  end ;


  function TGIS_CUDAEngine.fget_Devices(
    const _idx : Integer
  ) : TGIS_CUDADeviceInfo ;
  begin
    if ( _idx < 0 ) or ( _idx >= DeviceCount ) then
      Result := nil
    else
      Result := FDevices[_idx] ;
  end ;


  function GisCUDAEngine
    : TGIS_CUDAEngine ;
  begin
    if not assigned( cudaEngine ) then
      cudaEngine := TGIS_CUDAEngine.Create ;

    Result := cudaEngine ;
  end ;


//==============================================================================
// TGIS_CUDAProgram
//==============================================================================

  constructor TGIS_CUDAProgram.Create ;
  begin
    inherited ;

    if not GisCUDAEngine.Available then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_NVCUDA_NOT_AVAILABLE ), '', 0
            ) ;

    {$IFDEF DCC}
      oKernelList := TObjectList<TObject>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      oKernelList := TList<TObject>.Create ;
    {$ENDIF}

    FKernelIndex := -1 ;
  end ;


  procedure TGIS_CUDAProgram.doDestroy ;
  begin
    FreeObject( oKernelList ) ;
    FreeObject( oProgram ) ;

    inherited ;
  end ;


  function TGIS_CUDAProgram.fget_KernelCount
    : Integer ;
  begin
    Result := oKernelList.Count ;
  end ;


  function TGIS_CUDAProgram.fget_KernelIndex
    : Integer ;
  begin
    Result := FKernelIndex ;
  end ;


  procedure TGIS_CUDAProgram.fset_KernelIndex(
    const _val : Integer
  ) ;
  begin
    if ( _val >= 0 ) and ( _val < oKernelList.Count ) then
      FKernelIndex := _val ;
  end ;


  function TGIS_CUDAProgram.fget_KernelName
    : String ;
  var
    ker : T_cuKernel ;
  begin
    ker := T_cuKernel( oKernelList[FKernelIndex] ) ;
    Result := ker.Name ;
  end ;


  function TGIS_CUDAProgram.fget_GridDimensions(
    const _idx : Integer
  ) : NativeUInt ;
  var
    ker : T_cuKernel ;
  begin
    Result := 0 ;
    ker := T_cuKernel( oKernelList[FKernelIndex] ) ;
    if ( _idx >= 0 ) and ( _idx < 3 ) then
      Result := ker.GridDimensions[_idx] ;
  end ;


  procedure TGIS_CUDAProgram.fset_GridDimensions(
    const _idx : Integer ;
    const _val : NativeUInt
  ) ;
  var
    ker : T_cuKernel ;
  begin
    ker := T_cuKernel( oKernelList[FKernelIndex] ) ;
    if ( _idx >= 0 ) and ( _idx < 3 ) then
      ker.GridDimensions[_idx] := _val ;
  end ;


  function TGIS_CUDAProgram.fget_BlockDimensions(
    const _idx : Integer
  ) : NativeUInt ;
  var
    ker : T_cuKernel ;
  begin
    Result := 0 ;
    ker := T_cuKernel( oKernelList[FKernelIndex] ) ;
    if ( _idx >= 0 ) and ( _idx < 3 ) then
      Result := ker.BlockDimensions[_idx] ;
  end ;


  procedure TGIS_CUDAProgram.fset_BlockDimensions(
    const _idx : Integer ;
    const _val : NativeUInt
  ) ;
  var
    ker : T_cuKernel ;
  begin
    ker := T_cuKernel( oKernelList[FKernelIndex] ) ;
    if ( _idx >= 0 ) and ( _idx < 3 ) then
      ker.BlockDimensions[_idx] := _val ;
  end ;


  function TGIS_CUDAProgram.fget_ErrorCode
    : Integer ;
  begin
    Result := cudaError ;
  end ;


  function TGIS_CUDAProgram.fget_ErrorName
    : String ;
  var
    p : PCUAnsiChar ;
  begin
    cuGetErrorName( cudaError, p ) ;
    Result := p ;
  end ;


  function TGIS_CUDAProgram.fget_ErrorString
    : String ;
  var
    p : PCUAnsiChar ;
  begin
    cuGetErrorString( cudaError, p ) ;
    Result := p ;
  end ;


  function TGIS_CUDAProgram.prepare(
    const _source : String ;
    const _kernel : String
  ) : Boolean ;
  var
    ker : T_cuKernel ;
  begin
    Result := False ;

    oProgram := T_cuProgram.Create( _source ) ;

    if cudaError <> T_cuResult.CUDA_SUCCESS then begin
      FreeObject( oProgram ) ;
      exit ;
    end ;

    ker := T_cuProgram( oProgram ).CreateKernel( _kernel ) ;

    if cudaError <> T_cuResult.CUDA_SUCCESS then begin
      FreeObject( oProgram ) ;
      FreeObject( ker ) ;
      exit ;
    end ;

    oKernelList.Add( ker ) ;
    FKernelIndex := 0 ;
    sKernel := _kernel ;

    Result := True ;
  end ;


  function TGIS_CUDAProgram.enqueue(
    const _kernel : String
  ) : Boolean ;
  var
    ker : T_cuKernel ;
  begin
    Result := False ;

    if not assigned( oProgram ) then
      exit ;

    ker := T_cuProgram( oProgram ).CreateKernel( _kernel ) ;

    if cudaError <> T_cuResult.CUDA_SUCCESS then begin
      FreeObject( ker ) ;
      exit ;
    end ;

    oKernelList.Add( ker ) ;
    FKernelIndex := oKernelList.Count - 1 ;

    Result := True ;
  end ;


  function TGIS_CUDAProgram.LoadFromString(
    const _source : String ;
    const _kernel : String
  ) : Boolean ;
  begin
    Result := prepare( _source, _kernel ) ;
  end ;


  function TGIS_CUDAProgram.LoadFromFile(
    const _path   : String ;
    const _kernel : String
  ) : Boolean ;
  var
    {$IFDEF DCC}
      tf  : TextFile ;
      buf : String ;
    {$ENDIF}
    src : String ;
  begin
    Result := False ;

    if not FileExists( _path ) then
      exit ;

    {$IFDEF DCC}
      AssignFile( tf, _path ) ;
      Reset( tf ) ;
      src := '' ;
      while not( EOF( tf ) ) do begin
        Readln( tf, buf ) ;
        src := src + buf + #10#13 ;
      end ;
      CloseFile( tf ) ;
    {$ENDIF}
    {$IFDEF CLR}
      src := File.ReadAllText( _path ) ;
    {$ENDIF}

    Result := prepare( src, _kernel ) ;
  end ;


  function TGIS_CUDAProgram.EnqueueKernel(
    const _kernel : String
  ) : Boolean ;
  begin
    Result := enqueue( _kernel ) ;
  end ;


  function TGIS_CUDAProgram.SetArgument(
    const _idx    : Integer ;
    const _size   : Integer ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : Object
    {$ENDIF}
  ) : Boolean ;
  var
    ker   : T_cuKernel ;
    {$IFDEF CLR}
      gch : GCHandle ;
    {$ENDIF}
  begin
    ker := T_cuKernel( oKernelList[FKernelIndex] ) ;
    {$IFDEF DCC}
      Result := ker.SetArgument( _idx, _size, _data ) ;
    {$ENDIF}
    {$IFDEF CLR}
      gch := GCHandle.Alloc( _data, GCHandleType.Pinned ) ;
      Result := ker.SetArgument( _idx, _size, gch.AddrOfPinnedObject ) ;
      gch.Free ;
    {$ENDIF}
  end ;


  function TGIS_CUDAProgram.ShareArgument(
    const _idx    : Integer ;
    const _keridx : Integer ;
    const _argidx : Integer
  ) : Boolean ;
  var
    dst : T_cuKernel ;
    src : T_cuKernel ;
  begin
    dst := T_cuKernel( oKernelList[FKernelIndex] ) ;
    src := T_cuKernel( oKernelList[_keridx] ) ;

    dst.SetArgument( _idx, src.Argument[_argidx] ) ;
    Result := True ;
  end ;


  function TGIS_CUDAProgram.CheckArgument(
    const _idx : Integer
  ) : Boolean ;
  var
    ker : T_cuKernel ;
  begin
    ker := T_cuKernel( oKernelList[FKernelIndex] ) ;
    Result := ker.IsArgumentSet[_idx] ;
  end ;


  function TGIS_CUDAProgram.Execute : Boolean ;
  var
    ker : T_cuKernel ;
    i   : Integer ;
  begin
    Result := False ;

    for i := 0 to oKernelList.Count - 1 do begin
      ker := T_cuKernel( oKernelList[i] ) ;
      if not ker.Launch then
        exit ;
    end ;

    Result := True ;
  end ;


  function TGIS_CUDAProgram.ReadArgument(
    const _idx    : Integer ;
    const _size   : Integer ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : Object
    {$ENDIF}
  ) : Boolean ;
  var
    ker   : T_cuKernel ;
    buf   : T_cuBuffer ;
    {$IFDEF CLR}
      gch : GCHandle ;
    {$ENDIF}
  begin
    ker := T_cuKernel( oKernelList[FKernelIndex] ) ;
    buf := ker.Argument[_idx] ;
    {$IFDEF DCC}
      buf.ReadBuffer( _size, _data ) ;
    {$ENDIF}
    {$IFDEF CLR}
      gch := GCHandle.Alloc( _data, GCHandleType.Pinned ) ;
      buf.ReadBuffer( _size, gch.AddrOfPinnedObject ) ;
      gch.Free ;
    {$ENDIF}
    Result := True ;
  end ;


{$IFDEF DCC}

initialization
// do nothing

finalization
  FreeObject( cudaDevices ) ;
  FreeObject( cudaEngine ) ;
  freeNVCUDA ;

{$ENDIF}

{$ENDIF}

//==================================== END =====================================
end.
