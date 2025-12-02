//==============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//==============================================================================
{
  OpenCL wrapper.
}

{$IFDEF DCC}
  unit GisOpenCL ;
  {$HPPEMIT '#pragma link "GisOpenCL"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.OpenCL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.opencl ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.NDK.OpenCL ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.IO,
    System.Security,
    System.Runtime.InteropServices,
    System.Collections.Generic,
    TatukGIS.RTL,
    TatukGIS.NDK,
    TatukGIS.NDK.CUDA;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Generics.Collections,
    GisBaseObject;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}

type

  /// <summary>
  ///   Memory flags for defining OpenCL buffers.
  /// </summary>
  TGIS_OpenCLMemoryFlag =
    {$IFDEF OXYGENE} public {$IFNDEF JAVA} flags {$ENDIF} {$ENDIF} (
    /// <summary>
    ///   The buffer is readable and writable for the kernel.
    /// </summary>
    ReadWrite,

    /// <summary>
    ///   The buffer is write-only for the kernel.
    /// </summary>
    WriteOnly,

    /// <summary>
    ///   The buffer is read-only for the kernel.
    /// </summary>
    ReadOnly,

    /// <summary>
    ///   The buffer uses host memory referenced by the pointer.
    /// </summary>
    UseHostPtr,

    /// <summary>
    ///   The buffer is allocated in the host accessible memory.
    /// </summary>
    AllocHostPtr,

    /// <summary>
    ///   The buffer is initialized with the content specified by the host
    ///   pointer.
    /// </summary>
    CopyHostPtr,

    /// <summary>
    ///   The buffer is write-only for the host.
    /// </summary>
    HostWriteOnly,

    /// <summary>
    ///   The buffer is read-only for the host.
    /// </summary>
    HostReadOnly,

    /// <summary>
    ///   The host has no access to the buffer.
    /// </summary>
    HostNoAccess
  ) ;


  /// <summary>
  ///   Set of memory flags for defining OpenCL buffers.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      TGIS_OpenCLMemoryFlags = public set of TGIS_OpenCLMemoryFlag ;
    {$ELSE}
      TGIS_OpenCLMemoryFlags = TGIS_OpenCLMemoryFlag ;
    {$ENDIF}
  {$ELSE}
    TGIS_OpenCLMemoryFlags = set of TGIS_OpenCLMemoryFlag ;
  {$ENDIF}


  /// <summary>
  ///   Types of OpenCL devices.
  /// </summary>
  TGIS_OpenCLDeviceType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   The device is a processor.
    /// </summary>
    CPU,

    /// <summary>
    ///   The device is a graphic card.
    /// </summary>
    GPU,

    /// <summary>
    ///   The device is an OpenCL hardware accelerator.
    /// </summary>
    Accelerator
  ) ;


  /// <summary>
  ///   Provides information about an OpenCL device.
  /// </summary>
  TGIS_OpenCLDeviceInfo = {$IFDEF OXYGENE} public {$ENDIF}
                          class ( TGIS_Object )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oDevice : TObject ;
    private
      function  fget_Name           : String ;
      function  fget_Vendor         : String ;
      function  fget_Version        : String ;
      function  fget_Profile        : String ;
      function  fget_OpenCLCVersion : String ;
      function  fget_DriverVersion  : String ;
      function  fget_WorkGroupSize  : NativeUInt ;
      function  fget_ClockFrequency : Cardinal ;
      function  fget_MemoryAllocationSize
                                    : UInt64 ;
      function  fget_ComputeUnits   : Cardinal ;
      function  fget_Available      : Boolean ;
      function  fget_DeviceType     : TGIS_OpenCLDeviceType ;
      function  fget_ExtensionCount : Integer ;
      function  fget_Extensions     ( const _idx : Integer
                                    ) : String ;
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
      property Name              : String
                                   read  fget_Name ;
      /// <summary>
      ///   Device vendor.
      /// </summary>
      property Vendor            : String
                                   read  fget_Vendor ;
      /// <summary>
      ///   Device version.
      /// </summary>
      property Version           : String
                                   read  fget_Version ;
      /// <summary>
      ///   Device profile.
      /// </summary>
      property Profile           : String
                                   read  fget_Profile ;
      /// <summary>
      ///   Supported version of the OpenCL C language.
      /// </summary>
      property OpenCLCVersion    : String
                                   read  fget_OpenCLCVersion ;
      /// <summary>
      ///   Driver version.
      /// </summary>
      property DriverVersion     : String
                                   read  fget_DriverVersion ;
      /// <summary>
      ///   Maximum work group size.
      /// </summary>
      property WorkGroupSize     : NativeUInt
                                   read  fget_WorkGroupSize ;
      /// <summary>
      ///   Clock frequency.
      /// </summary>
      property ClockFrequency    : Cardinal
                                   read  fget_ClockFrequency ;
      /// <summary>
      ///   Maximum memory allocation size.
      /// </summary>
      property MemoryAllocationSize
                                 : UInt64
                                   read  fget_MemoryAllocationSize ;
      /// <summary>
      ///   Number of the compute units.
      /// </summary>
      property ComputeUnits      : Cardinal
                                   read  fget_ComputeUnits ;
      /// <summary>
      ///   True if the device is available (can be used).
      /// </summary>
      property Available         : Boolean
                                   read  fget_Available ;
      /// <summary>
      ///   Device type (GPU or CPU).
      /// </summary>
      property DeviceType        : TGIS_OpenCLDeviceType
                                   read  fget_DeviceType ;
      /// <summary>
      ///   Number of supported OpenCL extensions.
      /// </summary>
      property ExtensionCount    : Integer
                                   read  fget_ExtensionCount ;
      /// <summary>
      ///   List of supported OpenCL extensions.
      /// </summary>
      /// <param name="_idx">
      ///   index of the extension
      /// </param>
      property Extensions        [const _idx : Integer]
                                 : String
                                   read  fget_Extensions ;
  end ;


  /// <summary>
  ///   Provides information about an OpenCL platform.
  /// </summary>
  TGIS_OpenCLPlatformInfo = {$IFDEF OXYGENE} public {$ENDIF}
                            class ( TGIS_Object )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oPlatform : TObject ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      {$IFDEF DCC}
        FDevices : TObjectList<TGIS_OpenCLDeviceInfo> ;
      {$ENDIF}
      {$IFDEF CLR}
        FDevices : TList<TGIS_OpenCLDeviceInfo> ;
      {$ENDIF}
    private
      function  fget_Profile        : String ;
      function  fget_Version        : String ;
      function  fget_Name           : String ;
      function  fget_Vendor         : String ;
      function  fget_DeviceCount    : Cardinal ;
      function  fget_Devices        ( const _idx : Integer
                                    ) : TGIS_OpenCLDeviceInfo ;
      function  fget_ExtensionCount : Integer ;
      function  fget_Extensions     ( const _idx : Integer
                                    ) : String ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Platform name.
      /// </summary>
      property Name           : String
                                read  fget_Name ;
      /// <summary>
      ///   Platform vendor.
      /// </summary>
      property Vendor         : String
                                read  fget_Vendor ;
      /// <summary>
      ///   Platform version.
      /// </summary>
      property Version        : String
                                read  fget_Version ;
      /// <summary>
      ///   Platform profile.
      /// </summary>
      property Profile        : String
                                read  fget_Profile ;
      /// <summary>
      ///   Number of devices on the platform.
      /// </summary>
      property DeviceCount    : Cardinal
                                read  fget_DeviceCount ;
      /// <summary>
      ///   List of devices on the platform.
      /// </summary>
      /// <param name="_idx">
      ///   index of the device
      /// </param>
      property Devices        [const _idx : Integer]
                              : TGIS_OpenCLDeviceInfo
                                read  fget_Devices ;
      /// <summary>
      ///   Number of supported OpenCL extensions.
      /// </summary>
      property ExtensionCount : Integer
                                read  fget_ExtensionCount ;
      /// <summary>
      ///   List of supported OpenCL extensions.
      /// </summary>
      /// <param name="_idx">
      ///   index of the extension
      /// </param>
      property Extensions [const _idx : Integer]
                              : String
                                read  fget_Extensions ;
  end ;


  /// <summary>
  ///   Provides the means to initialize and enable the OpenCL computing
  ///   engine.
  /// </summary>
  TGIS_OpenCLEngine = {$IFDEF OXYGENE} public {$ENDIF}
                      class ( TGIS_BaseObjectDisposable )
    private
      {$IFDEF DCC}
        FPlatforms : TObjectList<TGIS_OpenCLPlatformInfo> ;
      {$ENDIF}
      {$IFDEF CLR}
        FPlatforms : TList<TGIS_OpenCLPlatformInfo> ;
      {$ENDIF}
    private
      function  fget_Available     : Boolean ;
      function  fget_Initialized   : Boolean ;
      function  fget_Enabled       : Boolean ;
      procedure fset_Enabled       ( const _val : Boolean
                                   ) ;
      function  fget_PlatformCount : Integer ;
      function  fget_Platforms     ( const _idx : Integer
                                   ) : TGIS_OpenCLPlatformInfo ;
      function  fget_ActiveDevice  : TGIS_OpenCLDeviceInfo ;
      procedure fset_ActiveDevice  ( const _val : TGIS_OpenCLDeviceInfo
                                   ) ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   True if OpenCL is available; calling this property attempts
      ///   to initialize OpenCL.
      /// </summary>
      property Available     : Boolean
                               read  fget_Available ;
      /// <summary>
      ///   True if OpenCL is initialized.
      /// </summary>
      property Initialized   : Boolean
                               read  fget_Initialized ;
      /// <summary>
      ///   Specifies if OpenCL should be used whenever possible.
      /// </summary>
      property Enabled       : Boolean
                               read  fget_Enabled
                               write fset_Enabled ;
      /// <summary>
      ///   Number of OpenCL platforms installed on the computer.
      /// </summary>
      property PlatformCount : Integer
                               read  fget_PlatformCount ;
      /// <summary>
      ///   List of OpenCL platforms installed on the computer.
      /// </summary>
      /// <param name="_idx">
      ///   index of the platform
      /// </param>
      property Platforms     [const _idx : Integer]
                             : TGIS_OpenCLPlatformInfo
                               read  fget_Platforms ;
      /// <summary>
      ///   Device used to execute the OpenCL programs; if set to nil then the
      ///   default device will be used.
      /// </summary>
      property ActiveDevice  : TGIS_OpenCLDeviceInfo
                               read  fget_ActiveDevice
                               write fset_ActiveDevice ;
  end ;


  /// <summary>
  ///   Returns a global OpenCL engine object.
  /// </summary>
  /// <returns>
  ///   OpenCL engine object
  /// </returns>
  function  GisOpenCLEngine : TGIS_OpenCLEngine ;


type

  /// <summary>
  ///   Provides the means to execute an OpenCL program.
  /// </summary>
  TGIS_OpenCLProgram = {$IFDEF OXYGENE} public {$ENDIF}
                       class ( TGIS_ObjectDisposable )
    private
      oDevice        : TObject ;
      oCommandQueue  : TObject ;
      oProgram       : TObject ;
      {$IFDEF DCC}
        oKernelList  : TObjectList<TObject> ;
      {$ENDIF}
      {$IFDEF CLR}
        oKernelList  : TList<TObject> ;
      {$ENDIF}
      sKernel        : String ;
      {$IFDEF DCC}
        oBufferList  : TObjectList<TObject> ;
      {$ENDIF}
      {$IFDEF CLR}
        oBufferList  : TList<TObject> ;
      {$ENDIF}
      bPrepared      : Boolean ;
    private
      FKernelIndex   : Integer ;
      FCacheBinaries : Boolean ;
    private
      function  fget_KernelCount     : Integer ;
      function  fget_KernelIndex     : Integer ;
      procedure fset_KernelIndex     ( const _val : Integer
                                     ) ;
      function  fget_KernelName      : String ;
      function  fget_ArgumentCount   : Integer ;
      function  fget_WorkDimension   : Integer ;
      procedure fset_WorkDimension   ( const _val : Integer
                                     ) ;
      function  fget_GlobalWorkSizes ( const _idx : Integer
                                     ) : NativeUInt ;
      procedure fset_GlobalWorkSizes ( const _idx : Integer ;
                                       const _val : NativeUInt
                                     ) ;
      function  fget_LocalWorkSizes  ( const _idx : Integer
                                     ) : NativeUInt ;
      procedure fset_LocalWorkSizes  ( const _idx : Integer ;
                                       const _val : NativeUInt
                                     ) ;
      function  fget_CacheBinaries   : Boolean ;
      procedure fset_CacheBinaries   ( const _val : Boolean
                                     ) ;
      function  fget_ErrorCode       : Integer ;
    private
      function  prepare ( const _source : String ;
                          const _kernel : String
                        ) : Boolean ;
      function  enqueue ( const _kernel : String
                        ) : Boolean ;
      function  ready   : Boolean ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Loads an OpenCL program from string and enqueues a kernel for
      ///   execution.
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
      ///   Loads an OpenCL program from a text file and enqueues a kernel for
      ///   execution.
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
      ///   Sets a non-buffer argument of the active kernel.
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
                               ) : Boolean ; overload ;
      /// <summary>
      ///   Sets a buffer argument of the active kernel.
      /// </summary>
      /// <param name="_idx">
      ///   index of the argument
      /// </param>
      /// <param name="_flags">
      ///   memory flags of the buffer
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
                                 const _flags  : TGIS_OpenCLMemoryFlags ;
                                 const _size   : Integer ;
                                 {$IFDEF DCC}
                                   const _data : Pointer
                                 {$ENDIF}
                                 {$IFDEF CLR}
                                   const _data : Object
                                 {$ENDIF}
                               ) : Boolean ; overload ;
      /// <summary>
      ///   Sets a buffer argument of the active kernel with an exiting buffer
      ///   already set for another kernel.
      /// </summary>
      /// <param name="_idx">
      ///   index of the argument
      /// </param>
      /// <param name="_keridx">
      ///   index of the owner kernel
      /// </param>
      /// <param name="_argidx">
      ///   index of the owner kernel buffer argument
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
      ///   Executes the OpenCL program.
      /// </summary>
      /// <returns>
      ///   True if the program has finished successfully
      /// </returns>
      function  Execute        : Boolean ;
      /// <summary>
      ///   Reads a buffer argument of the active kernel.
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
      function  ReadBuffer     ( const _idx    : Integer ;
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
      ///   Number of enqueued kernels of the OpenCL program.
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
      ///   Number of arguments of the active kernel.
      /// </summary>
      property ArgumentCount   : Integer
                                 read  fget_ArgumentCount ;
      /// <summary>
      ///   Work dimension of the active kernel.
      /// </summary>
      property WorkDimension   : Integer
                                 read  fget_WorkDimension
                                 write fset_WorkDimension ;
      /// <summary>
      ///   Global work sizes of the active kernel.
      /// </summary>
      /// <param name="_idx">
      ///   index of the global work size
      /// </param>
      property GlobalWorkSizes [const _idx : Integer]
                               : NativeUInt
                                 read  fget_GlobalWorkSizes
                                 write fset_GlobalWorkSizes ;
      /// <summary>
      ///   Local work sizes of the active kernel; if equal to zero or does not
      ///   divide global work sizes then work group sizes will be computed
      ///   automatically.
      /// </summary>
      /// <param name="_idx">
      ///   index of the local work size
      /// </param>
      property LocalWorkSizes  [const _idx : Integer]
                               : NativeUInt
                                 read  fget_LocalWorkSizes
                                 write fset_LocalWorkSizes ;
      /// <summary>
      ///   If True then the binary representation of the OpenCL program is
      ///   stored in the cache. If False then a fresh compilation is performed
      ///   upon every call to LoadFromString or LoadFromFile.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     The binary representation is associated with the kernel name.
      ///   </note>
      /// </remarks>
      property CacheBinaries   : Boolean
                                 read  fget_CacheBinaries
                                 write fset_CacheBinaries ;
      /// <summary>
      ///   Last error code returned by the OpenCL engine.
      /// </summary>
      property ErrorCode       : Integer
                                 read  fget_ErrorCode ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,

    GisRtl,
    GisClasses,
    GisResource,
    GisCUDA ;
{$ENDIF}

{$IFDEF DCC}
  type
    PCLAnsiChar   = Pointer ;
    PPCLAnsiChar  = Pointer ;
    TCLAnsiString = TBytes ;
{$ENDIF}
{$IFDEF CLR}
  type
    PCLAnsiChar   = String ;
    PPCLAnsiChar  = array of String ;
    TCLAnsiString = String ;
    PPointer      = IntPtr ;
    PInteger      = IntPtr ;
{$ENDIF}

const
  {$IFDEF MSWINDOWS_OS}
    OPENCL_LIB = 'OpenCL.dll' ;
  {$ENDIF}
  {$IFDEF MACOSX_OS}
    OPENCL_LIB = '/System/Library/Frameworks/OpenCL.framework/OpenCL' ;
  {$ENDIF}
  {$IFDEF IOS_OS}
    OPENCL_LIB = '/System/Library/Frameworks/OpenCL.framework/OpenCL' ;
  {$ENDIF}
  {$IFDEF LINUX_OS}
    { TODO : NOT TESTED }
    OPENCL_LIB = 'libOpenCL.so' ;
  {$ENDIF}
  {$IFDEF ANDROID_OS}
    { TODO : NOT TESTED }
    OPENCL_LIB = 'libOpenCL.so' ;
  {$ENDIF}
  {$IFDEF UNKNOWN_OS}
    OPENCL_LIB = '?';
  {$ENDIF}

var
  {$IFDEF DCC}
    libOpenCL      : IntPtr = 0 ;
  {$ENDIF}
  oclAvailable     : Boolean = False ;
  oclInitialized   : Boolean = False ;
  oclEnabled       : Boolean = False ;
  oclError         : Integer = 0 ;
  oclEngine        : TGIS_OpenCLEngine ;
  oclDefaultDevice : TGIS_OpenCLDeviceInfo ;
  oclActiveDevice  : TGIS_OpenCLDeviceInfo ;

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
// OpenCL constants
//==============================================================================

  T_clN = class
  public
  const
  CL_SUCCESS                                   =   0 ;
  CL_DEVICE_NOT_FOUND                          =  -1 ;
  CL_DEVICE_NOT_AVAILABLE                      =  -2 ;
  CL_COMPILER_NOT_AVAILABLE                    =  -3 ;
  CL_MEM_OBJECT_ALLOCATION_FAILURE             =  -4 ;
  CL_OUT_OF_RESOURCES                          =  -5 ;
  CL_OUT_OF_HOST_MEMORY                        =  -6 ;
  CL_PROFILING_INFO_NOT_AVAILABLE              =  -7 ;
  CL_MEM_COPY_OVERLAP                          =  -8 ;
  CL_IMAGE_FORMAT_MISMATCH                     =  -9 ;
  CL_IMAGE_FORMAT_NOT_SUPPORTED                = -10 ;
  CL_BUILD_PROGRAM_FAILURE                     = -11 ;
  CL_MAP_FAILURE                               = -12 ;
  CL_MISALIGNED_SUB_BUFFER_OFFSET              = -13 ;
  CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST = -14 ;
  CL_COMPILE_PROGRAM_FAILURE                   = -15 ;
  CL_LINKER_NOT_AVAILABLE                      = -16 ;
  CL_LINK_PROGRAM_FAILURE                      = -17 ;
  CL_DEVICE_PARTITION_FAILED                   = -18 ;
  CL_KERNEL_ARG_INFO_NOT_AVAILABLE             = -19 ;

  CL_INVALID_VALUE                             = -30 ;
  CL_INVALID_DEVICE_TYPE                       = -31 ;
  CL_INVALID_PLATFORM                          = -32 ;
  CL_INVALID_DEVICE                            = -33 ;
  CL_INVALID_CONTEXT                           = -34 ;
  CL_INVALID_QUEUE_PROPERTIES                  = -35 ;
  CL_INVALID_COMMAND_QUEUE                     = -36 ;
  CL_INVALID_HOST_PTR                          = -37 ;
  CL_INVALID_MEM_OBJECT                        = -38 ;
  CL_INVALID_IMAGE_FORMAT_DESCRIPTOR           = -39 ;
  CL_INVALID_IMAGE_SIZE                        = -40 ;
  CL_INVALID_SAMPLER                           = -41 ;
  CL_INVALID_BINARY                            = -42 ;
  CL_INVALID_BUILD_OPTIONS                     = -43 ;
  CL_INVALID_PROGRAM                           = -44 ;
  CL_INVALID_PROGRAM_EXECUTABLE                = -45 ;
  CL_INVALID_KERNEL_NAME                       = -46 ;
  CL_INVALID_KERNEL_DEFINITION                 = -47 ;
  CL_INVALID_KERNEL                            = -48 ;
  CL_INVALID_ARG_INDEX                         = -49 ;
  CL_INVALID_ARG_VALUE                         = -50 ;
  CL_INVALID_ARG_SIZE                          = -51 ;
  CL_INVALID_KERNEL_ARGS                       = -52 ;
  CL_INVALID_WORK_DIMENSION                    = -53 ;
  CL_INVALID_WORK_GROUP_SIZE                   = -54 ;
  CL_INVALID_WORK_ITEM_SIZE                    = -55 ;
  CL_INVALID_GLOBAL_OFFSET                     = -56 ;
  CL_INVALID_EVENT_WAIT_LIST                   = -57 ;
  CL_INVALID_EVENT                             = -58 ;
  CL_INVALID_OPERATION                         = -59 ;
  CL_INVALID_GL_OBJECT                         = -60 ;
  CL_INVALID_BUFFER_SIZE                       = -61 ;
  CL_INVALID_MIP_LEVEL                         = -62 ;
  CL_INVALID_GLOBAL_WORK_SIZE                  = -63 ;
  CL_INVALID_PROPERTY                          = -64 ;
  CL_INVALID_IMAGE_DESCRIPTOR                  = -65 ;
  CL_INVALID_COMPILER_OPTIONS                  = -66 ;
  CL_INVALID_LINKER_OPTIONS                    = -67 ;
  CL_INVALID_DEVICE_PARTITION_COUNT            = -68 ;

  CL_VERSION_1_0                               = 1 ;
  CL_VERSION_1_1                               = 1 ;
  CL_VERSION_1_2                               = 1 ;

  CL_FALSE                                     = 0 ;
  CL_TRUE                                      = 1 ;
  CL_BLOCKING                                  = CL_TRUE ;
  CL_NON_BLOCKING                              = CL_FALSE ;

  CL_PLATFORM_PROFILE                          = $0900 ;
  CL_PLATFORM_VERSION                          = $0901 ;
  CL_PLATFORM_NAME                             = $0902 ;
  CL_PLATFORM_VENDOR                           = $0903 ;
  CL_PLATFORM_EXTENSIONS                       = $0904 ;

  CL_DEVICE_TYPE_DEFAULT                       = ( 1 shl 0 ) ;
  CL_DEVICE_TYPE_CPU                           = ( 1 shl 1 ) ;
  CL_DEVICE_TYPE_GPU                           = ( 1 shl 2 ) ;
  CL_DEVICE_TYPE_ACCELERATOR                   = ( 1 shl 3 ) ;
  CL_DEVICE_TYPE_CUSTOM                        = ( 1 shl 4 ) ;
  CL_DEVICE_TYPE_ALL                           = $FFFFFFFF ;

  CL_DEVICE_TYPE                               = $1000 ;
  CL_DEVICE_VENDOR_ID                          = $1001 ;
  CL_DEVICE_MAX_COMPUTE_UNITS                  = $1002 ;
  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS           = $1003 ;
  CL_DEVICE_MAX_WORK_GROUP_SIZE                = $1004 ;
  CL_DEVICE_MAX_WORK_ITEM_SIZES                = $1005 ;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR        = $1006 ;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT       = $1007 ;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT         = $1008 ;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG        = $1009 ;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT       = $100A ;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE      = $100B ;
  CL_DEVICE_MAX_CLOCK_FREQUENCY                = $100C ;
  CL_DEVICE_ADDRESS_BITS                       = $100D ;
  CL_DEVICE_MAX_READ_IMAGE_ARGS                = $100E ;
  CL_DEVICE_MAX_WRITE_IMAGE_ARGS               = $100F ;
  CL_DEVICE_MAX_MEM_ALLOC_SIZE                 = $1010 ;
  CL_DEVICE_IMAGE2D_MAX_WIDTH                  = $1011 ;
  CL_DEVICE_IMAGE2D_MAX_HEIGHT                 = $1012 ;
  CL_DEVICE_IMAGE3D_MAX_WIDTH                  = $1013 ;
  CL_DEVICE_IMAGE3D_MAX_HEIGHT                 = $1014 ;
  CL_DEVICE_IMAGE3D_MAX_DEPTH                  = $1015 ;
  CL_DEVICE_IMAGE_SUPPORT                      = $1016 ;
  CL_DEVICE_MAX_PARAMETER_SIZE                 = $1017 ;
  CL_DEVICE_MAX_SAMPLERS                       = $1018 ;
  CL_DEVICE_MEM_BASE_ADDR_ALIGN                = $1019 ;
  CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE           = $101A ;
  CL_DEVICE_SINGLE_FP_CONFIG                   = $101B ;
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE              = $101C ;
  CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE          = $101D ;
  CL_DEVICE_GLOBAL_MEM_CACHE_SIZE              = $101E ;
  CL_DEVICE_GLOBAL_MEM_SIZE                    = $101F ;
  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE           = $1020 ;
  CL_DEVICE_MAX_CONSTANT_ARGS                  = $1021 ;
  CL_DEVICE_LOCAL_MEM_TYPE                     = $1022 ;
  CL_DEVICE_LOCAL_MEM_SIZE                     = $1023 ;
  CL_DEVICE_ERROR_CORRECTION_SUPPORT           = $1024 ;
  CL_DEVICE_PROFILING_TIMER_RESOLUTION         = $1025 ;
  CL_DEVICE_ENDIAN_LITTLE                      = $1026 ;
  CL_DEVICE_AVAILABLE                          = $1027 ;
  CL_DEVICE_COMPILER_AVAILABLE                 = $1028 ;
  CL_DEVICE_EXECUTION_CAPABILITIES             = $1029 ;
  CL_DEVICE_QUEUE_PROPERTIES                   = $102A ;
  CL_DEVICE_NAME                               = $102B ;
  CL_DEVICE_VENDOR                             = $102C ;
  CL_DRIVER_VERSION                            = $102D ;
  CL_DEVICE_PROFILE                            = $102E ;
  CL_DEVICE_VERSION                            = $102F ;
  CL_DEVICE_EXTENSIONS                         = $1030 ;
  CL_DEVICE_PLATFORM                           = $1031 ;
  CL_DEVICE_DOUBLE_FP_CONFIG                   = $1032 ;

  CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF        = $1034 ;
  CL_DEVICE_HOST_UNIFIED_MEMORY                = $1035 ;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR           = $1036 ;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT          = $1037 ;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_INT            = $1038 ;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG           = $1039 ;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT          = $103A ;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE         = $103B ;
  CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF           = $103C ;
  CL_DEVICE_OPENCL_C_VERSION                   = $103D ;
  CL_DEVICE_LINKER_AVAILABLE                   = $103E ;
  CL_DEVICE_BUILT_IN_KERNELS                   = $103F ;
  CL_DEVICE_IMAGE_MAX_BUFFER_SIZE              = $1040 ;
  CL_DEVICE_IMAGE_MAX_ARRAY_SIZE               = $1041 ;
  CL_DEVICE_PARENT_DEVICE                      = $1042 ;
  CL_DEVICE_PARTITION_MAX_SUB_DEVICES          = $1043 ;
  CL_DEVICE_PARTITION_PROPERTIES               = $1044 ;
  CL_DEVICE_PARTITION_AFFINITY_DOMAIN          = $1045 ;
  CL_DEVICE_PARTITION_TYPE                     = $1046 ;
  CL_DEVICE_REFERENCE_COUNT                    = $1047 ;
  CL_DEVICE_PREFERRED_INTEROP_USER_SYNC        = $1048 ;
  CL_DEVICE_PRINTF_BUFFER_SIZE                 = $1049 ;

  CL_FP_DENORM                                 = ( 1 shl 0 ) ;
  CL_FP_INF_NAN                                = ( 1 shl 1 ) ;
  CL_FP_ROUND_TO_NEAREST                       = ( 1 shl 2 ) ;
  CL_FP_ROUND_TO_ZERO                          = ( 1 shl 3 ) ;
  CL_FP_ROUND_TO_INF                           = ( 1 shl 4 ) ;
  CL_FP_FMA                                    = ( 1 shl 5 ) ;
  CL_FP_SOFT_FLOAT                             = ( 1 shl 6 ) ;
  CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT          = ( 1 shl 7 ) ;

  CL_NONE                                      = $0 ;
  CL_READ_ONLY_CACHE                           = $1 ;
  CL_READ_WRITE_CACHE                          = $2 ;

  CL_LOCAL                                     = $1 ;
  CL_GLOBAL                                    = $2 ;

  CL_EXEC_KERNEL                               = ( 1 shl 0 ) ;
  CL_EXEC_NATIVE_KERNEL                        = ( 1 shl 1 ) ;

  CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE       = ( 1 shl 0 ) ;
  CL_QUEUE_PROFILING_ENABLE                    = ( 1 shl 1 ) ;

  CL_CONTEXT_REFERENCE_COUNT                   = $1080 ;
  CL_CONTEXT_DEVICES                           = $1081 ;
  CL_CONTEXT_PROPERTIES                        = $1082 ;
  CL_CONTEXT_NUM_DEVICES                       = $1083 ;

  CL_CONTEXT_PLATFORM                          = $1084 ;
  CL_CONTEXT_INTEROP_USER_SYNC                 = $1085 ;

  CL_DEVICE_PARTITION_EQUALLY                  = $1086 ;
  CL_DEVICE_PARTITION_BY_COUNTS                = $1087 ;
  CL_DEVICE_PARTITION_BY_COUNTS_LIST_END       = $0    ;
  CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN       = $1088 ;

  CL_DEVICE_AFFINITY_DOMAIN_NUMA               = ( 1 shl 0 ) ;
  CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE           = ( 1 shl 1 ) ;
  CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE           = ( 1 shl 2 ) ;
  CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE           = ( 1 shl 3 ) ;
  CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE           = ( 1 shl 4 ) ;
  CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE = ( 1 shl 5 ) ;

  CL_QUEUE_CONTEXT                             = $1090 ;
  CL_QUEUE_DEVICE                              = $1091 ;
  CL_QUEUE_REFERENCE_COUNT                     = $1092 ;
  CL_QUEUE_PROPERTIES                          = $1093 ;

  CL_MEM_READ_WRITE                            = ( 1 shl 0 ) ;
  CL_MEM_WRITE_ONLY                            = ( 1 shl 1 ) ;
  CL_MEM_READ_ONLY                             = ( 1 shl 2 ) ;
  CL_MEM_USE_HOST_PTR                          = ( 1 shl 3 ) ;
  CL_MEM_ALLOC_HOST_PTR                        = ( 1 shl 4 ) ;
  CL_MEM_COPY_HOST_PTR                         = ( 1 shl 5 ) ;
  // reserved                                  = ( 1 shl 6 ) ;
  CL_MEM_HOST_WRITE_ONLY                       = ( 1 shl 7 ) ;
  CL_MEM_HOST_READ_ONLY                        = ( 1 shl 8 ) ;
  CL_MEM_HOST_NO_ACCESS                        = ( 1 shl 9 ) ;

  CL_MIGRATE_MEM_OBJECT_HOST                   = ( 1 shl 0 ) ;
  CL_MIGRATE_MEM_OBJECT_CONTENT_UNDEFINED      = ( 1 shl 1 ) ;

  CL_R                                         = $10B0 ;
  CL_A                                         = $10B1 ;
  CL_RG                                        = $10B2 ;
  CL_RA                                        = $10B3 ;
  CL_RGB                                       = $10B4 ;
  CL_RGBA                                      = $10B5 ;
  CL_BGRA                                      = $10B6 ;
  CL_ARGB                                      = $10B7 ;
  CL_INTENSITY                                 = $10B8 ;
  CL_LUMINANCE                                 = $10B9 ;
  CL_Rx                                        = $10BA ;
  CL_RGx                                       = $10BB ;
  CL_RGBx                                      = $10BC ;

  CL_SNORM_INT8                                = $10D0 ;
  CL_SNORM_INT16                               = $10D1 ;
  CL_UNORM_INT8                                = $10D2 ;
  CL_UNORM_INT16                               = $10D3 ;
  CL_UNORM_SHORT_565                           = $10D4 ;
  CL_UNORM_SHORT_555                           = $10D5 ;
  CL_UNORM_INT_101010                          = $10D6 ;
  CL_SIGNED_INT8                               = $10D7 ;
  CL_SIGNED_INT16                              = $10D8 ;
  CL_SIGNED_INT32                              = $10D9 ;
  CL_UNSIGNED_INT8                             = $10DA ;
  CL_UNSIGNED_INT16                            = $10DB ;
  CL_UNSIGNED_INT32                            = $10DC ;
  CL_HALF_FLOAT                                = $10DD ;
  CL_FLOAT                                     = $10DE ;

  CL_MEM_OBJECT_BUFFER                         = $10F0 ;
  CL_MEM_OBJECT_IMAGE2D                        = $10F1 ;
  CL_MEM_OBJECT_IMAGE3D                        = $10F2 ;
  CL_MEM_OBJECT_IMAGE2D_ARRAY                  = $10F3 ;
  CL_MEM_OBJECT_IMAGE1D                        = $10F4 ;
  CL_MEM_OBJECT_IMAGE1D_ARRAY                  = $10F5 ;
  CL_MEM_OBJECT_IMAGE1D_BUFFER                 = $10F6 ;

  CL_MEM_TYPE                                  = $1100 ;
  CL_MEM_FLAGS                                 = $1101 ;
  CL_MEM_SIZE                                  = $1102 ;
  CL_MEM_HOST_PTR                              = $1103 ;
  CL_MEM_MAP_COUNT                             = $1104 ;
  CL_MEM_REFERENCE_COUNT                       = $1105 ;
  CL_MEM_CONTEXT                               = $1106 ;
  CL_MEM_ASSOCIATED_MEMOBJECT                  = $1107 ;
  CL_MEM_OFFSET                                = $1108 ;

  CL_IMAGE_FORMAT                              = $1110 ;
  CL_IMAGE_ELEMENT_SIZE                        = $1111 ;
  CL_IMAGE_ROW_PITCH                           = $1112 ;
  CL_IMAGE_SLICE_PITCH                         = $1113 ;
  CL_IMAGE_WIDTH                               = $1114 ;
  CL_IMAGE_HEIGHT                              = $1115 ;
  CL_IMAGE_DEPTH                               = $1116 ;
  CL_IMAGE_ARRAY_SIZE                          = $1117 ;
  CL_IMAGE_BUFFER                              = $1118 ;
  CL_IMAGE_NUM_MIP_LEVELS                      = $1119 ;
  CL_IMAGE_NUM_SAMPLES                         = $111A ;

  CL_ADDRESS_NONE                              = $1130 ;
  CL_ADDRESS_CLAMP_TO_EDGE                     = $1131 ;
  CL_ADDRESS_CLAMP                             = $1132 ;
  CL_ADDRESS_REPEAT                            = $1133 ;
  CL_ADDRESS_MIRRORED_REPEAT                   = $1134 ;

  CL_FILTER_NEAREST                            = $1140 ;
  CL_FILTER_LINEAR                             = $1141 ;

  CL_SAMPLER_REFERENCE_COUNT                   = $1150 ;
  CL_SAMPLER_CONTEXT                           = $1151 ;
  CL_SAMPLER_NORMALIZED_COORDS                 = $1152 ;
  CL_SAMPLER_ADDRESSING_MODE                   = $1153 ;
  CL_SAMPLER_FILTER_MODE                       = $1154 ;

  CL_MAP_READ                                  = ( 1 shl 0 ) ;
  CL_MAP_WRITE                                 = ( 1 shl 1 ) ;
  CL_MAP_WRITE_INVALIDATE_REGION               = ( 1 shl 2 ) ;

  CL_PROGRAM_REFERENCE_COUNT                   = $1160 ;
  CL_PROGRAM_CONTEXT                           = $1161 ;
  CL_PROGRAM_NUM_DEVICES                       = $1162 ;
  CL_PROGRAM_DEVICES                           = $1163 ;
  CL_PROGRAM_SOURCE                            = $1164 ;
  CL_PROGRAM_BINARY_SIZES                      = $1165 ;
  CL_PROGRAM_BINARIES                          = $1166 ;
  CL_PROGRAM_NUM_KERNELS                       = $1167 ;
  CL_PROGRAM_KERNEL_NAMES                      = $1168 ;

  CL_PROGRAM_BUILD_STATUS                      = $1181 ;
  CL_PROGRAM_BUILD_OPTIONS                     = $1182 ;
  CL_PROGRAM_BUILD_LOG                         = $1183 ;
  CL_PROGRAM_BINARY_TYPE                       = $1184 ;

  CL_PROGRAM_BINARY_TYPE_NONE                  = $0 ;
  CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT       = $1 ;
  CL_PROGRAM_BINARY_TYPE_LIBRARY               = $2 ;
  CL_PROGRAM_BINARY_TYPE_EXECUTABLE            = $4 ;

  CL_BUILD_SUCCESS                             =  0 ;
  CL_BUILD_NONE                                = -1 ;
  CL_BUILD_ERROR                               = -2 ;
  CL_BUILD_IN_PROGRESS                         = -3 ;

  CL_KERNEL_FUNCTION_NAME                      = $1190 ;
  CL_KERNEL_NUM_ARGS                           = $1191 ;
  CL_KERNEL_REFERENCE_COUNT                    = $1192 ;
  CL_KERNEL_CONTEXT                            = $1193 ;
  CL_KERNEL_PROGRAM                            = $1194 ;
  CL_KERNEL_ATTRIBUTES                         = $1195 ;

  CL_KERNEL_ARG_ADDRESS_QUALIFIER              = $1196 ;
  CL_KERNEL_ARG_ACCESS_QUALIFIER               = $1197 ;
  CL_KERNEL_ARG_TYPE_NAME                      = $1198 ;
  CL_KERNEL_ARG_TYPE_QUALIFIER                 = $1199 ;
  CL_KERNEL_ARG_NAME                           = $119A ;

  CL_KERNEL_ARG_ADDRESS_GLOBAL                 = $119B ;
  CL_KERNEL_ARG_ADDRESS_LOCAL                  = $119C ;
  CL_KERNEL_ARG_ADDRESS_CONSTANT               = $119D ;
  CL_KERNEL_ARG_ADDRESS_PRIVATE                = $119E ;

  CL_KERNEL_ARG_ACCESS_READ_ONLY               = $11A0 ;
  CL_KERNEL_ARG_ACCESS_WRITE_ONLY              = $11A1 ;
  CL_KERNEL_ARG_ACCESS_READ_WRITE              = $11A2 ;
  CL_KERNEL_ARG_ACCESS_NONE                    = $11A3 ;

  CL_KERNEL_ARG_TYPE_NONE                      =           0 ;
  CL_KERNEL_ARG_TYPE_CONST                     = ( 1 shl 0 ) ;
  CL_KERNEL_ARG_TYPE_RESTRICT                  = ( 1 shl 1 ) ;
  CL_KERNEL_ARG_TYPE_VOLATILE                  = ( 1 shl 2 ) ;

  CL_KERNEL_WORK_GROUP_SIZE                    = $11B0 ;
  CL_KERNEL_COMPILE_WORK_GROUP_SIZE            = $11B1 ;
  CL_KERNEL_LOCAL_MEM_SIZE                     = $11B2 ;
  CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE = $11B3 ;
  CL_KERNEL_PRIVATE_MEM_SIZE                   = $11B4 ;
  CL_KERNEL_GLOBAL_WORK_SIZE                   = $11B5 ;

  CL_EVENT_COMMAND_QUEUE                       = $11D0 ;
  CL_EVENT_COMMAND_TYPE                        = $11D1 ;
  CL_EVENT_REFERENCE_COUNT                     = $11D2 ;
  CL_EVENT_COMMAND_EXECUTION_STATUS            = $11D3 ;
  CL_EVENT_CONTEXT                             = $11D4 ;

  CL_COMMAND_NDRANGE_KERNEL                    = $11F0 ;
  CL_COMMAND_TASK                              = $11F1 ;
  CL_COMMAND_NATIVE_KERNEL                     = $11F2 ;
  CL_COMMAND_READ_BUFFER                       = $11F3 ;
  CL_COMMAND_WRITE_BUFFER                      = $11F4 ;
  CL_COMMAND_COPY_BUFFER                       = $11F5 ;
  CL_COMMAND_READ_IMAGE                        = $11F6 ;
  CL_COMMAND_WRITE_IMAGE                       = $11F7 ;
  CL_COMMAND_COPY_IMAGE                        = $11F8 ;
  CL_COMMAND_COPY_IMAGE_TO_BUFFER              = $11F9 ;
  CL_COMMAND_COPY_BUFFER_TO_IMAGE              = $11FA ;
  CL_COMMAND_MAP_BUFFER                        = $11FB ;
  CL_COMMAND_MAP_IMAGE                         = $11FC ;
  CL_COMMAND_UNMAP_MEM_OBJECT                  = $11FD ;
  CL_COMMAND_MARKER                            = $11FE ;
  CL_COMMAND_ACQUIRE_GL_OBJECTS                = $11FF ;
  CL_COMMAND_RELEASE_GL_OBJECTS                = $1200 ;
  CL_COMMAND_READ_BUFFER_RECT                  = $1201 ;
  CL_COMMAND_WRITE_BUFFER_RECT                 = $1202 ;
  CL_COMMAND_COPY_BUFFER_RECT                  = $1203 ;
  CL_COMMAND_USER                              = $1204 ;
  CL_COMMAND_BARRIER                           = $1205 ;
  CL_COMMAND_MIGRATE_MEM_OBJECTS               = $1206 ;
  CL_COMMAND_FILL_BUFFER                       = $1207 ;
  CL_COMMAND_FILL_IMAGE                        = $1208 ;

  CL_COMPLETE                                  = $0 ;
  CL_RUNNING                                   = $1 ;
  CL_SUBMITTED                                 = $2 ;
  CL_QUEUED                                    = $3 ;

  CL_BUFFER_CREATE_TYPE_REGION                 = $1220 ;

  CL_PROFILING_COMMAND_QUEUED                  = $1280 ;
  CL_PROFILING_COMMAND_SUBMIT                  = $1281 ;
  CL_PROFILING_COMMAND_START                   = $1282 ;
  CL_PROFILING_COMMAND_END                     = $1283 ;
  end ;

type
//==============================================================================
// OpenCL type definitions
//==============================================================================

  {$IFDEF DCC}
    PPByte = ^PByte ;
  {$ENDIF}
  {$IFDEF CLR}
    PPByte = IntPtr ;
  {$ENDIF}

  TCL_char = ShortInt ;
  {$IFDEF DCC}
    PCL_char = ^TCL_char ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_char = IntPtr ;
  {$ENDIF}

  TCL_uchar = Byte ;
  {$IFDEF DCC}
    PCL_uchar = ^TCL_uchar ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uchar = IntPtr ;
  {$ENDIF}

  TCL_short = SmallInt ;
  {$IFDEF DCC}
    PCL_short = ^TCL_short ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_short = IntPtr ;
  {$ENDIF}

  TCL_ushort = Word ;
  {$IFDEF DCC}
    PCL_ushort = ^TCL_ushort ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ushort = IntPtr ;
  {$ENDIF}

  TCL_int = Integer ;
  {$IFDEF DCC}
    PCL_int = ^TCL_int ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_int = IntPtr ;
  {$ENDIF}

  TCL_uint = Cardinal ;
  {$IFDEF DCC}
    PCL_uint = ^TCL_uint ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uint = IntPtr ;
  {$ENDIF}

  TCL_long = Int64 ;
  {$IFDEF DCC}
    PCL_long = ^TCL_long ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_long = IntPtr ;
  {$ENDIF}

  TCL_ulong = UInt64 ;
  {$IFDEF DCC}
    PCL_ulong = ^TCL_ulong ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ulong = IntPtr ;
  {$ENDIF}

  TCL_half = TCL_ushort ;
  {$IFDEF DCC}
    PCL_half = ^TCL_half ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_half = IntPtr ;
  {$ENDIF}

  TCL_float = Single ;
  {$IFDEF DCC}
    PCL_float = ^TCL_float ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_float = IntPtr ;
  {$ENDIF}

  TCL_double = Double ;
  {$IFDEF DCC}
    PCL_double = ^TCL_double ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_double = IntPtr ;
  {$ENDIF}

  TCL_half2 = record
    i16 : array [0..1] of TCL_half ;
  end ;
  {$IFDEF DCC}
    PCL_half2 = ^TCL_half2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_half2 = IntPtr ;
  {$ENDIF}

  TCL_half4 = record
    i16 : array [0..3] of TCL_half ;
  end ;
  {$IFDEF DCC}
    PCL_half4 = ^TCL_half4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_half4 = IntPtr ;
  {$ENDIF}

  TCL_half8 = record
    i16 : array [0..7] of TCL_half ;
  end ;
  {$IFDEF DCC}
    PCL_half8 = ^TCL_half8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_half8 = IntPtr ;
  {$ENDIF}

  TCL_half16 = record
    i16 : array [0..15] of TCL_half ;
  end ;
  {$IFDEF DCC}
    PCL_half16 = ^TCL_half16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_half16 = IntPtr ;
  {$ENDIF}

  TCL_char2 = record
    i8 : array [0..1] of TCL_char ;
  end ;
  {$IFDEF DCC}
    PCL_char2 = ^TCL_char2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_char2 = IntPtr ;
  {$ENDIF}

  TCL_char4 = record
    i8 : array [0..3] of TCL_char ;
  end ;
  {$IFDEF DCC}
    PCL_char4 = ^TCL_char4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_char4 = IntPtr ;
  {$ENDIF}

  TCL_char8 = record
    i8 : array [0..7] of TCL_char ;
  end ;
  {$IFDEF DCC}
    PCL_char8 = ^TCL_char8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_char8 = IntPtr ;
  {$ENDIF}

  TCL_char16 = record
    i8 : array [0..15] of TCL_char ;
  end ;
  {$IFDEF DCC}
    PCL_char16 = ^TCL_char16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_char16 = IntPtr ;
  {$ENDIF}

  TCL_uchar2 = record
    u8 : array [0..1] of TCL_uchar ;
  end ;
  {$IFDEF DCC}
    PCL_uchar2 = ^TCL_uchar2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uchar2 = IntPtr ;
  {$ENDIF}

  TCL_uchar4 = record
    u8 : array [0..3] of TCL_uchar ;
  end ;
  {$IFDEF DCC}
    PCL_uchar4 = ^TCL_uchar4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uchar4 = IntPtr ;
  {$ENDIF}

  TCL_uchar8 = record
    u8 : array [0..7] of TCL_uchar ;
  end ;
  {$IFDEF DCC}
    PCL_uchar8 = ^TCL_uchar8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uchar8 = IntPtr ;
  {$ENDIF}

  TCL_uchar16 = record
    u8 : array [0..15] of TCL_uchar ;
  end ;
  {$IFDEF DCC}
    PCL_uchar16 = ^TCL_uchar16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uchar16 = IntPtr ;
  {$ENDIF}

  TCL_short2 = record
    i16 : array [0..1] of TCL_short ;
  end ;
  {$IFDEF DCC}
    PCL_short2 = ^TCL_short2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_short2 = IntPtr ;
  {$ENDIF}

  TCL_short4 = record
    i16 : array [0..3] of TCL_short ;
  end ;
  {$IFDEF DCC}
    PCL_short4 = ^TCL_short4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_short4 = IntPtr ;
  {$ENDIF}

  TCL_short8 = record
    i16 : array [0..7] of TCL_short ;
  end ;
  {$IFDEF DCC}
    PCL_short8 = ^TCL_short8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_short8 = IntPtr ;
  {$ENDIF}

  TCL_short16 = record
    i16 : array [0..15] of TCL_short ;
  end ;
  {$IFDEF DCC}
    PCL_short16 = ^TCL_short16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_short16 = IntPtr ;
  {$ENDIF}

  TCL_ushort2 = record
    u16 : array [0..1] of TCL_ushort ;
  end ;
  {$IFDEF DCC}
    PCL_ushort2 = ^TCL_ushort2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ushort2 = IntPtr ;
  {$ENDIF}

  TCL_ushort4 = record
    u16 : array [0..3] of TCL_ushort ;
  end ;
  {$IFDEF DCC}
    PCL_ushort4 = ^TCL_ushort4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ushort4 = IntPtr ;
  {$ENDIF}

  TCL_ushort8 = record
    u16 : array [0..7] of TCL_ushort ;
  end ;
  {$IFDEF DCC}
    PCL_ushort8 = ^TCL_ushort8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ushort8 = IntPtr ;
  {$ENDIF}

  TCL_ushort16 = record
    u16 : array [0..15] of TCL_ushort ;
  end ;
  {$IFDEF DCC}
    PCL_ushort16 = ^TCL_ushort16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ushort16 = IntPtr ;
  {$ENDIF}

  TCL_int2 = record
    i32 : array [0..1] of TCL_int ;
  end ;
  {$IFDEF DCC}
    PCL_int2 = ^TCL_int2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_int2 = IntPtr ;
  {$ENDIF}

  TCL_int4 = record
    i32 : array [0..3] of TCL_int ;
  end ;
  {$IFDEF DCC}
    PCL_int4 = ^TCL_int4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_int4 = IntPtr ;
  {$ENDIF}

  TCL_int8 = record
    i32 : array [0..7] of TCL_int ;
  end ;
  {$IFDEF DCC}
    PCL_int8 = ^TCL_int8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_int8 = IntPtr ;
  {$ENDIF}

  TCL_int16 = record
    i32 : array [0..15] of TCL_int ;
  end ;
  {$IFDEF DCC}
    PCL_int16 = ^TCL_int16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_int16 = IntPtr ;
  {$ENDIF}

  TCL_uint2 = record
    u32 : array [0..1] of TCL_uint ;
  end ;
  {$IFDEF DCC}
    PCL_uint2 = ^TCL_uint2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uint2 = IntPtr ;
  {$ENDIF}

  TCL_uint4 = record
    u32 : array [0..3] of TCL_uint ;
  end ;
  {$IFDEF DCC}
    PCL_uint4 = ^TCL_uint4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uint4 = IntPtr ;
  {$ENDIF}

  TCL_uint8 = record
    u32 : array [0..7] of TCL_uint ;
  end ;
  {$IFDEF DCC}
    PCL_uint8 = ^TCL_uint8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uint8 = IntPtr ;
  {$ENDIF}

  TCL_uint16 = record
    u32 : array [0..15] of TCL_uint ;
  end ;
  {$IFDEF DCC}
    PCL_uint16 = ^TCL_uint16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_uint16 = IntPtr ;
  {$ENDIF}

  TCL_long2 = record
    i64 : array [0..1] of TCL_long ;
  end ;
  {$IFDEF DCC}
    PCL_long2 = ^TCL_long2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_long2 = IntPtr ;
  {$ENDIF}

  TCL_long4 = record
    i64 : array [0..3] of TCL_long ;
  end ;
  {$IFDEF DCC}
    PCL_long4 = ^TCL_long4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_long4 = IntPtr ;
  {$ENDIF}

  TCL_long8 = record
    i64 : array [0..7] of TCL_long ;
  end ;
  {$IFDEF DCC}
    PCL_long8 = ^TCL_long8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_long8 = IntPtr ;
  {$ENDIF}

  TCL_long16 = record
    i64 : array [0..15] of TCL_long ;
  end ;
  {$IFDEF DCC}
    PCL_long16 = ^TCL_long16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_long16 = IntPtr ;
  {$ENDIF}

  TCL_ulong2 = record
    u64 : array [0..1] of TCL_ulong ;
  end ;
  {$IFDEF DCC}
    PCL_ulong2 = ^TCL_ulong2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ulong2 = IntPtr ;
  {$ENDIF}

  TCL_ulong4 = record
    u64 : array [0..3] of TCL_ulong ;
  end ;
  {$IFDEF DCC}
    PCL_ulong4 = ^TCL_ulong4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ulong4 = IntPtr ;
  {$ENDIF}

  TCL_ulong8 = record
    u64 : array [0..7] of TCL_ulong ;
  end ;
  {$IFDEF DCC}
    PCL_ulong8 = ^TCL_ulong8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ulong8 = IntPtr ;
  {$ENDIF}

  TCL_ulong16 = record
    u64 : array [0..15] of TCL_ulong ;
  end ;
  {$IFDEF DCC}
    PCL_ulong16 = ^TCL_ulong16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_ulong16 = IntPtr ;
  {$ENDIF}

  TCL_float2 = record
    f32 : array [0..1] of TCL_float ;
  end ;
  {$IFDEF DCC}
    PCL_float2 = ^TCL_float2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_float2 = IntPtr ;
  {$ENDIF}

  TCL_float4 = record
    f32 : array [0..3] of TCL_float ;
  end ;
  {$IFDEF DCC}
    PCL_float4 = ^TCL_float4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_float4 = IntPtr ;
  {$ENDIF}

  TCL_float8 = record
    f32 : array [0..7] of TCL_float ;
  end ;
  {$IFDEF DCC}
    PCL_float8 = ^TCL_float8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_float8 = IntPtr ;
  {$ENDIF}

  TCL_float16 = record
    f32 : array [0..15] of TCL_float ;
  end ;
  {$IFDEF DCC}
    PCL_float16 = ^TCL_float16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_float16 = IntPtr ;
  {$ENDIF}

  TCL_double2 = record
    f64 : array [0..1] of TCL_double ;
  end ;
  {$IFDEF DCC}
    PCL_double2 = ^TCL_double2 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_double2 = IntPtr ;
  {$ENDIF}

  TCL_double4 = record
    f64 : array [0..3] of TCL_double ;
  end ;
  {$IFDEF DCC}
    PCL_double4 = ^TCL_double4 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_double4 = IntPtr ;
  {$ENDIF}

  TCL_double8 = record
    f64 : array [0..7] of TCL_double ;
  end ;
  {$IFDEF DCC}
    PCL_double8 = ^TCL_double8 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_double8 = IntPtr ;
  {$ENDIF}

  TCL_double16 = record
    f64 : array [0..15] of TCL_double ;
  end ;
  {$IFDEF DCC}
    PCL_double16 = ^TCL_double16 ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_double16 = IntPtr ;
  {$ENDIF}


type
//==============================================================================
// OpenCL type definitions
//==============================================================================

  TSize_t = NativeUInt ;
  {$IFDEF DCC}
    PSize_t = ^TSize_t ;
  {$ENDIF}
  {$IFDEF CLR}
    PSize_t = array of IntPtr ;
  {$ENDIF}
  TArraySize_t = array of TSize_t ;

  {$IFDEF DCC}
    TIntPtr_t = ^Integer ;
    PIntPtr_t = ^TIntPtr_t ;
  {$ENDIF}
  {$IFDEF CLR}
    TIntPtr_t = IntPtr ;
    PIntPtr_t = IntPtr ;
  {$ENDIF}

  TSizet = array [0..2] of TSize_t ;
  {$IFDEF DCC}
    PSizet = ^TSizet ;
  {$ENDIF}
  {$IFDEF CLR}
    PSizet = array of IntPtr ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_platform_id = record
    end ;
    PCL_platform_id = ^TCL_platform_id ;
    PPCL_platform_id = ^PCL_platform_id ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
    TCL_platform_id = record
      public handle : IntPtr ;
    end ;
    PCL_platform_id = IntPtr ;
    PPCL_platform_id = array of TCL_platform_id ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_device_id = record
    end ;
    PCL_device_id = ^TCL_device_id ;
    PPCL_device_id = ^PCL_device_id ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
    TCL_device_id = record
      public handle : IntPtr ;
    end ;
    PCL_device_id = IntPtr ;
    PPCL_device_id = array of TCL_device_id ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_context = record
    end ;
    PCL_context = ^TCL_context ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
    TCL_context = record
      public handle : IntPtr ;
    end ;
    PCL_context = IntPtr ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_command_queue = record
    end ;
    PCL_command_queue = ^TCL_command_queue ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
    TCL_command_queue = record
      public handle : IntPtr ;
    end ;
    PCL_command_queue = IntPtr ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_mem = record
    end ;
    PCL_mem = ^TCL_mem ;
    PPCL_mem = ^PCL_mem ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
    TCL_mem = record
      public handle : IntPtr ;
    end ;
    PCL_mem = IntPtr ;
    PPCL_mem = array of TCL_mem ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_program = record
    end ;
    PCL_program = ^TCL_program ;
    PPCL_program = ^PCL_program ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
    TCL_program = record
      public handle : IntPtr ;
    end ;
    PCL_program = IntPtr ;
    PPCL_program = array of TCL_program ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_kernel = record
    end ;
    PCL_kernel = ^TCL_kernel ;
    PPCL_kernel = ^PCL_kernel ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
    TCL_kernel = record
      public handle : IntPtr ;
    end ;
    PCL_kernel = IntPtr ;
    PPCL_kernel = array of TCL_kernel ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_event = record
    end ;
    PCL_event = ^TCL_event ;
    PPCL_event = ^PCL_event ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
    TCL_event = record
      public handle : IntPtr ;
    end ;
//?    TCL_event = procedure ( _sender : Object ; args : EventArgs ) ;
    PCL_event = IntPtr ;
    PPCL_event = array of TCL_event ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_sampler = record
    end ;
    PCL_sampler = ^TCL_sampler ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
    TCL_sampler = record
      public handle : IntPtr ;
    end ;
    PCL_sampler = IntPtr ;
  {$ENDIF}

  TCL_bool = TCL_uint ;
  {$IFDEF DCC}
    PCL_bool = ^TCL_bool ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_bool = IntPtr ;
  {$ENDIF}

  TCL_bitfield = TCL_ulong ;
  {$IFDEF DCC}
    PCL_bitfield = ^TCL_bitfield ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_bitfield = IntPtr ;
  {$ENDIF}

  TCL_device_type = TCL_bitfield ;
  {$IFDEF DCC}
    PCL_device_type = ^TCL_device_type ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_device_type = IntPtr ;
  {$ENDIF}

  TCL_platform_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_platform_info = ^TCL_platform_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_platform_info = IntPtr ;
  {$ENDIF}

  TCL_device_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_device_info = ^TCL_device_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_device_info = IntPtr ;
  {$ENDIF}

  TCL_device_address_info = TCL_bitfield ;
  {$IFDEF DCC}
    PCL_device_address_info = ^TCL_device_address_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_device_address_info = IntPtr ;
  {$ENDIF}

  TCL_device_fp_config = TCL_bitfield ;
  {$IFDEF DCC}
    PCL_device_fp_config = ^TCL_device_fp_config ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_device_fp_config = IntPtr ;
  {$ENDIF}

  TCL_device_mem_cache_type = TCL_uint ;
  {$IFDEF DCC}
    PCL_device_mem_cache_type = ^TCL_device_mem_cache_type ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_device_mem_cache_type = IntPtr ;
  {$ENDIF}

  TCL_device_local_mem_type = TCL_uint ;
  {$IFDEF DCC}
    PCL_device_local_mem_type = ^TCL_device_local_mem_type ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_device_local_mem_type = IntPtr ;
  {$ENDIF}

  TCL_device_exec_capabilities = TCL_bitfield ;
  {$IFDEF DCC}
    PCL_device_exec_capabilities = ^TCL_device_exec_capabilities ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_device_exec_capabilities = IntPtr ;
  {$ENDIF}

  TCL_command_queue_properties = TCL_bitfield ;
  {$IFDEF DCC}
    PCL_command_queue_properties = ^TCL_command_queue_properties ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_command_queue_properties = IntPtr ;
  {$ENDIF}

  TCL_device_partition_property = TIntPtr_t ;
  {$IFDEF DCC}
    PCL_device_partition_property = ^TCL_device_partition_property ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_device_partition_property = IntPtr ;
  {$ENDIF}

  TCL_device_affinity_domain = TCL_bitfield ;
  {$IFDEF DCC}
    PCL_device_affinity_domain = ^TCL_device_affinity_domain ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_device_affinity_domain = IntPtr ;
  {$ENDIF}

  {$IFDEF DCC}
    PCL_context_properties = PInteger ;
    PPCL_context_properties = ^PCL_context_properties ;
  {$ELSE}
    [StructLayout(LayoutKind.Sequential)]
    TCL_context_properties = record
      public name  : TCL_uint ;
      public value : IntPtr ;
    end ;
    PCL_context_properties = IntPtr ;
    PPCL_context_properties = array of TCL_context_properties ;
  {$ENDIF}

  TCL_context_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_context_info = ^TCL_context_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_context_info = IntPtr ;
  {$ENDIF}

  TCL_command_queue_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_command_queue_info = ^TCL_command_queue_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_command_queue_info = IntPtr ;
  {$ENDIF}

  TCL_channel_order = TCL_uint ;
  {$IFDEF DCC}
    PCL_channel_order = ^TCL_channel_order ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_channel_order = IntPtr ;
  {$ENDIF}

  TCL_channel_type = TCL_uint ;
  {$IFDEF DCC}
    PCL_channel_type = ^TCL_channel_type ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_channel_type = IntPtr ;
  {$ENDIF}

  TCL_mem_flags = TCL_bitfield ;
  {$IFDEF DCC}
    PCL_mem_flags = ^TCL_mem_flags ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_mem_flags = IntPtr ;
  {$ENDIF}

  TCL_mem_object_type = TCL_uint ;
  {$IFDEF DCC}
    PCL_mem_object_type = ^TCL_mem_object_type ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_mem_object_type = IntPtr ;
  {$ENDIF}

  TCL_mem_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_mem_info = ^TCL_mem_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_mem_info = IntPtr ;
  {$ENDIF}

  TCL_mem_migration_flags = TCL_bitfield ;
  {$IFDEF DCC}
    PCL_mem_migration_flags = ^TCL_mem_migration_flags ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_mem_migration_flags = IntPtr ;
  {$ENDIF}

  TCL_image_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_image_info = ^TCL_image_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_image_info = IntPtr ;
  {$ENDIF}

  TCL_buffer_create_type = TCL_uint ;
  {$IFDEF DCC}
    PCL_buffer_create_type = ^TCL_buffer_create_type ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_buffer_create_type = IntPtr ;
  {$ENDIF}

  TCL_addressing_mode = TCL_uint ;
  {$IFDEF DCC}
    PCL_addressing_mode = ^TCL_addressing_mode ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_addressing_mode = IntPtr ;
  {$ENDIF}

  TCL_filter_mode = TCL_uint ;
  {$IFDEF DCC}
    PCL_filter_mode = ^TCL_filter_mode ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_filter_mode = IntPtr ;
  {$ENDIF}

  TCL_sampler_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_sampler_info = ^TCL_sampler_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_sampler_info = IntPtr ;
  {$ENDIF}

  TCL_map_flags = TCL_bitfield ;
  {$IFDEF DCC}
    PCL_map_flags = ^TCL_map_flags ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_map_flags = IntPtr ;
  {$ENDIF}

  TCL_program_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_program_info = ^TCL_program_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_program_info = IntPtr ;
  {$ENDIF}

  TCL_program_build_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_program_build_info = ^TCL_program_build_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_program_build_info = IntPtr ;
  {$ENDIF}

  TCL_build_status = TCL_int ;
  {$IFDEF DCC}
    PCL_build_status = ^TCL_build_status ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_build_status = IntPtr ;
  {$ENDIF}

  TCL_kernel_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_kernel_info = ^TCL_kernel_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_kernel_info = IntPtr ;
  {$ENDIF}

  TCL_kernel_arg_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_kernel_arg_info = ^TCL_kernel_arg_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_kernel_arg_info = IntPtr ;
  {$ENDIF}

  TCL__kernel_arg_address_qualifier = TCL_uint ;
  {$IFDEF DCC}
    PCL__kernel_arg_address_qualifier = ^TCL__kernel_arg_address_qualifier ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL__kernel_arg_address_qualifier = IntPtr ;
  {$ENDIF}

  TCL__kernel_arg_access_qualifier = TCL_uint ;
  {$IFDEF DCC}
    PCL__kernel_arg_access_qualifier = ^TCL__kernel_arg_access_qualifier ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL__kernel_arg_access_qualifier = IntPtr ;
  {$ENDIF}

  TCL_kernel_work_group_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_kernel_work_group_info = ^TCL_kernel_work_group_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_kernel_work_group_info = IntPtr ;
  {$ENDIF}

  TCL_event_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_event_info = ^TCL_event_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_event_info = IntPtr ;
  {$ENDIF}

  TCL_command_type = TCL_uint ;
  {$IFDEF DCC}
    PCL_command_type = ^TCL_command_type ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_command_type = IntPtr ;
  {$ENDIF}

  TCL_profiling_info = TCL_uint ;
  {$IFDEF DCC}
    PCL_profiling_info = ^TCL_profiling_info ;
  {$ENDIF}
  {$IFDEF CLR}
    PCL_profiling_info = IntPtr ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_image_format = packed record
      Image_channel_order     : TCL_channel_order ;
      Image_channel_data_type : TCL_channel_type ;
    end ;
    PCL_image_format = ^TCL_image_format ;
    PPCL_image_format = ^PCL_image_format ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    TCL_image_format = record
      Image_channel_order     : TCL_channel_order ;
      Image_channel_data_type : TCL_channel_type ;
    end ;
    PCL_image_format = IntPtr ;
    PPCL_image_format = array of TCL_image_format ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_image_desc = packed record
      image_type        : TCL_mem_object_type ;
      image_width       : TSize_t ;
      image_height      : TSize_t ;
      image_depth       : TSize_t ;
      image_array_size  : TSize_t ;
      image_row_pitch   : TSize_t ;
      image_slice_pitch : TSize_t ;
      num_mip_levels    : TCL_uint ;
      num_samples       : TCL_uint ;
      buffer            : PCL_mem ;
    end ;
    PCL_image_desc = ^TCL_image_desc ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    TCL_image_desc = record
      image_type        : TCL_mem_object_type ;
      image_width       : TSize_t ;
      image_height      : TSize_t ;
      image_depth       : TSize_t ;
      image_array_size  : TSize_t ;
      image_row_pitch   : TSize_t ;
      image_slice_pitch : TSize_t ;
      num_mip_levels    : TCL_uint ;
      num_samples       : TCL_uint ;
      buffer            : PCL_mem ;
    end ;
    PCL_image_desc = IntPtr ;
  {$ENDIF}

  {$IFDEF DCC}
    TCL_buffer_region = packed record
      origin : TSize_t ;
      size   : TSize_t ;
    end ;
    PCL_buffer_region = ^TCL_buffer_region ;
  {$ENDIF}
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    TCL_buffer_region = record
      origin : TSize_t ;
      size   : TSize_t ;
    end ;
    PCL_buffer_region = IntPtr ;
  {$ENDIF}

{$IFDEF MACOS}
  {$DEFINE CDECL}
{$ENDIF}

type
//==============================================================================
// OpenCL argument returned functions
//==============================================================================

  TContextNotify   = procedure ( const _name  : PCLAnsiChar ;
                                 const _data  : Pointer ;
                                       _size  : TSize_t ;
                                       _data2 : Pointer
                               ) ; {$IFDEF DCC}
                                     {$IFDEF CDECL}
                                       cdecl ;
                                     {$ELSE}
                                       stdcall ;
                                     {$ENDIF}
                                   {$ENDIF}

  TProgramNotify   = procedure (       _program   : PCL_program ;
                                       _user_data : Pointer
                               ) ; {$IFDEF DCC}
                                     {$IFDEF CDECL}
                                       cdecl ;
                                     {$ELSE}
                                       stdcall ;
                                     {$ENDIF}
                                   {$ENDIF}

  TMemObjectNotify = procedure (       _memob     : PCL_mem ;
                                       _user_data : Pointer
                               ) ; {$IFDEF DCC}
                                     {$IFDEF CDECL}
                                       cdecl ;
                                     {$ELSE}
                                       stdcall ;
                                     {$ENDIF}
                                   {$ENDIF}

  TEventNotify     = procedure (       _event  : PCL_event;
                                       _cl_int : TCL_int ;
                                       _p      : Pointer
                               ) ; {$IFDEF DCC}
                                     {$IFDEF CDECL}
                                       cdecl ;
                                     {$ELSE}
                                       stdcall ;
                                     {$ENDIF}
                                   {$ENDIF}

  TEnqueueUserProc = procedure (       _data   : Pointer
                               ) ; {$IFDEF DCC}
                                     {$IFDEF CDECL}
                                       cdecl ;
                                     {$ELSE}
                                       stdcall ;
                                     {$ENDIF}
                                   {$ENDIF}


{$IFDEF DCC}
type
//==============================================================================
// OpenCL 1.0 method definitions
//==============================================================================

  TclGetPlatformIDs = function (
          _num_entries   : TCL_uint ;
          _platforms     : PPCL_platform_id ;
          _num_platforms : PCL_uint
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetPlatformInfo = function (
          _platform             : PCL_platform_id ;
          _param_name           : TCL_platform_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetDeviceIDs = function (
          _platform    : PCL_platform_id ;
          _device_type : TCL_device_type ;
          _num_entries : TCL_uint ;
          _devices     : PPCL_device_id ;
          _num_devices : PCL_uint
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetDeviceInfo = function (
          _device               : PCL_device_id ;
          _param_name           : TCL_device_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateContext = function (
    const _properties  : PPCL_context_properties ;
          _num_devices : TCL_uint ;
    const _devices     : PPCL_device_id ;
          _pfn_notify  : TContextNotify ;
          _user_data   : Pointer ;
          _errcode_ret : PCL_int
  ) : PCL_context ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateContextFromType = function (
    const _properties  : PPCL_context_properties ;
          _device_type : TCL_device_type ;
          _pfn_notify  : TContextNotify ;
          _user_data   : Pointer ;
          _errcode_ret : PCL_int
  ) : PCL_context ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclRetainContext = function (
          _context : PCL_context
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclReleaseContext = function (
          _context : PCL_context
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetContextInfo = function (
          _context              : PCL_context ;
          _param_name           : TCL_context_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateCommandQueue = function (
          _context     : PCL_context ;
          _device      : PCL_device_id ;
          _properties  : TCL_command_queue_properties ;
          _errcode_ret : PCL_int
  ) : PCL_command_queue ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclRetainCommandQueue = function (
          _command_queue : PCL_command_queue
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclReleaseCommandQueue = function (
          _command_queue : PCL_command_queue
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetCommandQueueInfo = function (
          _command_queue        : PCL_command_queue ;
          _param_name           : TCL_command_queue_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateBuffer = function (
          _context     : PCL_context ;
          _flags       : TCL_mem_flags ;
          _size        : TSize_t ;
          _host_ptr    : Pointer ;
          _errcode_ret : PCL_int
  ) : PCL_mem ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclRetainMemObject = function (
          _memobj : PCL_mem
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclReleaseMemObject = function (
          _memobj : PCL_mem
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetSupportedImageFormats = function (
          _context           : PCL_context ;
          _flags             : TCL_mem_flags ;
          _image_type        : TCL_mem_object_type ;
          _num_entries       : TCL_uint ;
          _image_formats     : PPCL_image_format ;
          _num_image_formats : PCL_uint
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetMemObjectInfo = function (
          _memobj               : PCL_mem ;
          _param_name           : TCL_mem_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetImageInfo = function (
          _image                : PCL_mem ;
          _param_name           : TCL_image_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateSampler = function (
          _context           : PCL_context ;
          _normalized_coords : TCL_bool ;
          _addressing_mode   : TCL_addressing_mode ;
          _filter_mode       : TCL_filter_mode ;
          _errcode_ret       : PCL_int
  ) : PCL_sampler ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclRetainSampler = function (
          _sampler : PCL_sampler
  ) : TCL_sampler ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclReleaseSampler = function (
          _sampler : PCL_sampler
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetSamplerInfo = function (
          _sampler              : PCL_sampler ;
          _param_name           : TCL_sampler_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateProgramWithSource = function (
          _context     : PCL_context ;
          _count       : TCL_uint ;
    const _strings     : PPCLAnsiChar ;
    const _lengths     : PSize_t ;
          _errcode_ret : PCL_int
  ) : PCL_program ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateProgramWithBinary = function (
          _context       : PCL_context ;
          _num_devices   : TCL_uint ;
    const _device_list   : PPCL_device_id ;
    const _lengths       : PSize_t ;
    const _binaries      : PPByte ;
          _binary_status : PCL_int ;
          _errcode_ret   : PCL_int
  ) : PCL_program ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclRetainProgram = function (
          _program : PCL_program
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclReleaseProgram = function (
          _program : PCL_program
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclBuildProgram = function (
          _program     : PCL_program ;
          _num_devices : TCL_uint ;
    const _device_list : PPCL_device_id ;
    const _options     : PCLAnsiChar ;
          _pfn_notify  : TProgramNotify ;
          _user_data   : Pointer
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetProgramInfo = function (
          _program              : PCL_program ;
          _param_name           : TCL_program_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetProgramBuildInfo = function (
          _program              : PCL_program ;
          _device               : PCL_device_id ;
          _param_name           : TCL_program_build_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateKernel = function (
          _program     : PCL_program ;
    const _kernel_name : PCLAnsiChar ;
          _errcode_ret : PCL_int
  ) : PCL_kernel ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateKernelsInProgram = function (
          _program         : PCL_program ;
          _num_kernels     : TCL_uint ;
          _kernels         : PPCL_kernel ;
          _num_kernels_ret : PCL_uint
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclRetainKernel = function (
          _kernel : PCL_kernel
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclReleaseKernel = function (
          _kernel : PCL_kernel
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclSetKernelArg = function (
          _kernel    : PCL_kernel ;
          _arg_index : TCL_uint ;
          _arg_size  : TSize_t ;
    const _arg_value : Pointer
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetKernelInfo = function (
          _kernel               : PCL_kernel ;
          _param_name           : TCL_kernel_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetKernelWorkGroupInfo = function (
          _kernel               : PCL_kernel ;
          _device               : PCL_device_id ;
          _param_name           : TCL_kernel_work_group_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclWaitForEvents = function (
          _num_events : TCL_uint ;
    const _event_list : PPCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetEventInfo = function (
          _event                : PCL_event ;
          _param_name           : TCL_event_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclRetainEvent = function (
          _event : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclReleaseEvent = function (
          _event : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetEventProfilingInfo = function (
          _event                : PCL_event ;
          _param_name           : TCL_profiling_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclFlush = function (
          _command_queue : PCL_command_queue
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclFinish = function (
          _command_queue : PCL_command_queue
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueReadBuffer = function (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_read           : TCL_bool ;
          _offset                  : TSize_t ;
          _cb                      : TSize_t ;
          _ptr                     : Pointer ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueWriteBuffer = function (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_write          : TCL_bool ;
          _offset                  : TSize_t ;
          _cb                      : TSize_t ;
    const _ptr                     : Pointer ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueCopyBuffer = function (
          _command_queue           : PCL_command_queue ;
          _src_buffer              : PCL_mem ;
          _dst_buffer              : PCL_mem ;
          _src_offset              : TSize_t ;
          _dst_offset              : TSize_t ;
          _cb                      : TSize_t ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueReadImage = function (
          _command_queue           : PCL_command_queue ;
          _image                   : PCL_mem ;
          _blocking_read           : TCL_bool ;
    const _origin                  : PSizet ;
    const _region                  : PSizet ;
          _row_pitch               : TSize_t ;
          _slice_pitch             : TSize_t ;
          _ptr                     : Pointer ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueWriteImage = function (
          _command_queue           : PCL_command_queue ;
          _image                   : PCL_mem ;
          _blocking_write          : TCL_bool ;
    const _origin                  : PSizet ;
    const _region                  : PSizet ;
          _input_row_pitch         : TSize_t ;
          _input_slice_pitch       : TSize_t ;
    const _ptr                     : Pointer ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueCopyImage = function (
          _command_queue           : PCL_command_queue ;
          _src_image               : PCL_mem ;
          _dst_image               : PCL_mem ;
    const _src_origin              : PSizet ;
    const _dst_origin              : PSizet ;
    const _region                  : PSizet ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueCopyImageToBuffer = function (
          _command_queue           : PCL_command_queue ;
          _src_image               : PCL_mem ;
          _dst_buffer              : PCL_mem ;
    const _src_origin              : PSizet ;
    const _region                  : PSizet ;
          _dst_offset              : TSize_t ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueCopyBufferToImage = function (
          _command_queue           : PCL_command_queue ;
          _src_buffer              : PCL_mem ;
          _dst_image               : PCL_mem ;
          _src_offset              : TSize_t ;
    const _dst_origin              : PSizet ;
    const _region                  : PSizet ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueMapBuffer = function (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_map            : TCL_bool ;
          _map_flags               : TCL_map_flags ;
          _offset                  : TSize_t ;
          _cb                      : TSize_t ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event ;
          _errcode_ret             : PCL_int
  ) : Pointer ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueMapImage = function (
          _command_queue           : PCL_command_queue ;
          _image                   : PCL_mem ;
          _blocking_map            : TCL_bool ;
          _map_flags               : TCL_map_flags ;
    const _origin                  : PSizet ;
    const _region                  : PSizet ;
          _image_row_pitch         : PSize_t ;
          _image_slice_pitch       : PSize_t ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event ;
          _errcode_ret             : PCL_int
  ) : Pointer ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueUnmapMemObject = function (
          _command_queue           : PCL_command_queue ;
          _memobj                  : PCL_mem ;
          _mapped_ptr              : Pointer ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueNDRangeKernel = function (
          _command_queue           : PCL_command_queue ;
          _kernel                  : PCL_kernel ;
          _work_dim                : TCL_uint ;
    const _global_work_offset      : PSize_t ;
    const _global_work_size        : PSize_t ;
    const _local_work_size         : PSize_t ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueTask = function (
          _command_queue           : PCL_command_queue ;
          _kernel                  : PCL_kernel ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueNativeKernel = function (
          _command_queue           : PCL_command_queue ;
          _user_func               : TEnqueueUserProc ;
          _args                    : Pointer ;
          _cb_args                 : TSize_t ;
          _num_mem_objects         : TCL_uint ;
    const _mem_list                : PPCL_mem ;
    const _args_mem_loc            : PPointer ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

//==============================================================================
// OpenCL 1.1 method definitions
//==============================================================================

  TclCreateSubBuffer = function (
          _buffer             : PCL_mem ;
          _flags              : TCL_mem_flags ;
          _buffer_create_type : TCL_buffer_create_type ;
    const _buffer_create_info : Pointer ;
          _errcode_ret        : PCL_int
  ) : PCL_mem ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclSetMemObjectDestructorCallback = function (
          _memobj     : PCL_mem ;
          _pfn_notify : TMemObjectNotify ;
          _user_data  : Pointer
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateUserEvent = function (
          _context     : PCL_context ;
          _errcode_ret : PCL_int
  ) : PCL_event ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclSetUserEventStatus = function (
          _event            : PCL_event ;
          _execution_status : TCL_int
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclSetEventCallback = function (
          _event                      : PCL_event ;
          _command_exec_callback_type : TCL_int ;
          _pfn_notify                 : TEventNotify ;
          _user_data                  : Pointer
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueReadBufferRect = function (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_read           : TCL_bool ;
    const _buffer_origin           : PSizet ;
    const _host_origin             : PSizet ;
    const _region                  : PSizet ;
          _buffer_row_pitch        : TSize_t ;
          _buffer_slice_pitch      : TSize_t ;
          _host_row_pitch          : TSize_t ;
          _host_slice_pitch        : TSize_t ;
          _ptr                     : Pointer ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueWriteBufferRect = function (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_write          : TCL_bool ;
    const _buffer_origin           : PSizet ;
    const _host_origin             : PSizet ;
    const _region                  : PSizet ;
          _buffer_row_pitch        : TSize_t ;
          _buffer_slice_pitch      : TSize_t ;
          _host_row_pitch          : TSize_t ;
          _host_slice_pitch        : TSize_t ;
          _ptr                     : Pointer ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueCopyBufferRect = function (
          _command_queue           : PCL_command_queue ;
          _src_buffer              : PCL_mem ;
          _dst_buffer              : PCL_mem ;
    const _src_origin              : PSizet ;
    const _dst_origin              : PSizet ;
    const _region                  : PSizet ;
          _src_row_pitch           : TSize_t ;
          _src_slice_pitch         : TSize_t ;
          _dst_row_pitch           : TSize_t ;
          _dst_slice_pitch         : TSize_t ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

// deprecated methods

  TclCreateImage2D = function (
          _context                 : PCL_context ;
          _flags                   : TCL_mem_flags ;
    const _image_format            : PCL_image_format ;
          _image_width             : TSize_t ;
          _image_height            : TSize_t ;
          _image_row_pitch         : TSize_t ;
          _host_ptr                : Pointer ;
          _errcode_ret             : PCL_int
  ) : PCL_mem ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateImage3D = function (
          _context                 : PCL_context ;
          _flags                   : TCL_mem_flags ;
    const _image_format            : PCL_image_format ;
          _image_width             : TSize_t ;
          _image_height            : TSize_t ;
          _image_depth             : TSize_t ;
          _image_row_pitch         : TSize_t ;
          _image_slice_pitch       : TSize_t ;
          _host_ptr                : Pointer ;
          _errcode_ret             : PCL_int
  ) : PCL_mem ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclUnloadCompiler = function
    : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueMarker = function (
          _command_queue           : PCL_command_queue ;
          _event                   : PPCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueWaitForEvents = function (
          _command_queue           : PCL_command_queue ;
          _num_events              : TCL_uint ;
    const _event_list              : PPCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueBarrier = function (
          _command_queue           : PCL_command_queue
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetExtensionFunctionAddress = function (
    const _func_name               : PCLAnsiChar
  ) : Pointer ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

//==============================================================================
// OpenCL 1.2 method definitions
//==============================================================================

  TclCreateSubDevices = function (
          _in_device       : PCL_device_id ;
    const _properties      : PCL_device_partition_property ;
          _num_devices     : TCL_uint ;
          _out_devices     : PPCL_device_id ;
          _num_devices_ret : PCL_uint
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclRetainDevice = function (
          _device : PCL_device_id
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclReleaseDevice = function (
          _device : PCL_device_id
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateImage = function (
          _context      : PCL_context ;
          _flags        : TCL_mem_flags ;
    const _image_format : PCL_image_format ;
    const _image_desc   : PCL_image_desc ;
          _host_ptr     : Pointer ;
          _errcode_ret  : PCL_int
  ) : PCL_mem ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCreateProgramWithBuiltInKernels = function (
          _context      : PCL_context ;
          _num_devices  : TCL_uint ;
    const _device_list  : PPCL_device_id ;
    const _kernel_names : PPCLAnsiChar ;
          _errcode_ret  : PCL_int
  ) : PCL_program ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclCompileProgram = function (
          _program              : PCL_program ;
          _num_devices          : TCL_uint ;
    const _device_list          : PPCL_device_id ;
    const _options              : PCLAnsiChar ;
          _num_input_headers    : TCL_uint ;
    const _input_headers        : PPCL_program ;
    const _header_include_names : PPCLAnsiChar ;
          _pfn_notify           : TProgramNotify ;
          _user_data            : Pointer
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclLinkProgram = function (
          _context            : PCL_context ;
          _num_devices        : TCL_uint ;
    const _device_list        : PPCL_device_id ;
    const _options            : PCLAnsiChar ;
          _num_input_programs : TCL_uint ;
    const _input_programs     : PPCL_program ;
          _pfn_notify         : TProgramNotify ;
          _user_data          : Pointer ;
          _errcode_ret        : PCL_int
  ) : PCL_program ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclUnloadPlatformCompiler = function (
          _platform : PCL_platform_id
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetKernelArgInfo = function (
          _kernel               : PCL_kernel ;
          _arg_indx             : TCL_uint ;
          _param_name           : TCL_kernel_arg_info ;
          _param_value_size     : TSize_t ;
          _param_value          : Pointer ;
          _param_value_size_ret : PSize_t
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueFillBuffer = function (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
    const _pattern                 : Pointer ;
          _pattern_size            : TSize_t ;
          _offset                  : TSize_t ;
          _size                    : TSize_t ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueFillImage = function (
          _command_queue           : PCL_command_queue ;
          _image                   : PCL_mem ;
    const _fill_color              : Pointer ;
    const _origin                  : PSizet ;
    const _region                  : PSizet ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueMigrateMemObjects = function (
          _command_queue           : PCL_command_queue ;
          _num_mem_objects         : TCL_uint ;
    const _mem_objects             : PPCL_mem ;
          _flags                   : TCL_mem_migration_flags ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueMarkerWithWaitList = function (
          _command_queue           : PCL_command_queue ;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PPCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclEnqueueBarrierWithWaitList = function (
          _command_queue           : PCL_command_queue;
          _num_events_in_wait_list : TCL_uint ;
    const _event_wait_list         : PPCL_event ;
          _event                   : PCL_event
  ) : TCL_int ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

  TclGetExtensionFunctionAddressForPlatform = function (
          _platform : PCL_platform_id ;
    const func_name   : PCLAnsiChar
  ) : Pointer ; {$IFDEF CDECL} cdecl ; {$ELSE} stdcall ; {$ENDIF}

{$ENDIF}


{$IFDEF DCC}
var
//==============================================================================
// OpenCL 1.0 method declarations
//==============================================================================

  clGetPlatformIDs                  : TclGetPlatformIDs ;
  clGetPlatformInfo                 : TclGetPlatformInfo ;
  clGetDeviceIDs                    : TclGetDeviceIDs ;
  clGetDeviceInfo                   : TclGetDeviceInfo ;
  clCreateContext                   : TclCreateContext ;
  clCreateContextFromType           : TclCreateContextFromType ;
  clRetainContext                   : TclRetainContext ;
  clReleaseContext                  : TclReleaseContext ;
  clGetContextInfo                  : TclGetContextInfo ;
  clCreateCommandQueue              : TclCreateCommandQueue ;
  clRetainCommandQueue              : TclRetainCommandQueue ;
  clReleaseCommandQueue             : TclReleaseCommandQueue ;
  clGetCommandQueueInfo             : TclGetCommandQueueInfo ;
  clCreateBuffer                    : TclCreateBuffer ;
  clRetainMemObject                 : TclRetainMemObject ;
  clReleaseMemObject                : TclReleaseMemObject ;
  clGetSupportedImageFormats        : TclGetSupportedImageFormats ;
  clGetMemObjectInfo                : TclGetMemObjectInfo ;
  clGetImageInfo                    : TclGetImageInfo ;
  clCreateSampler                   : TclCreateSampler ;
  clRetainSampler                   : TclRetainSampler ;
  clReleaseSampler                  : TclReleaseSampler ;
  clGetSamplerInfo                  : TclGetSamplerInfo ;
  clCreateProgramWithSource         : TclCreateProgramWithSource ;
  clCreateProgramWithBinary         : TclCreateProgramWithBinary ;
  clRetainProgram                   : TclRetainProgram ;
  clReleaseProgram                  : TclReleaseProgram ;
  clBuildProgram                    : TclBuildProgram ;
  clGetProgramInfo                  : TclGetProgramInfo ;
  clGetProgramBuildInfo             : TclGetProgramBuildInfo ;
  clCreateKernel                    : TclCreateKernel ;
  clCreateKernelsInProgram          : TclCreateKernelsInProgram ;
  clRetainKernel                    : TclRetainKernel ;
  clReleaseKernel                   : TclReleaseKernel ;
  clSetKernelArg                    : TclSetKernelArg ;
  clGetKernelInfo                   : TclGetKernelInfo ;
  clGetKernelWorkGroupInfo          : TclGetKernelWorkGroupInfo ;
  clWaitForEvents                   : TclWaitForEvents ;
  clGetEventInfo                    : TclGetEventInfo ;
  clRetainEvent                     : TclRetainEvent ;
  clReleaseEvent                    : TclReleaseEvent ;
  clGetEventProfilingInfo           : TclGetEventProfilingInfo ;
  clFlush                           : TclFlush ;
  clFinish                          : TclFinish ;
  clEnqueueReadBuffer               : TclEnqueueReadBuffer ;
  clEnqueueWriteBuffer              : TclEnqueueWriteBuffer ;
  clEnqueueCopyBuffer               : TclEnqueueCopyBuffer ;
  clEnqueueReadImage                : TclEnqueueReadImage ;
  clEnqueueWriteImage               : TclEnqueueWriteImage ;
  clEnqueueCopyImage                : TclEnqueueCopyImage ;
  clEnqueueCopyImageToBuffer        : TclEnqueueCopyImageToBuffer ;
  clEnqueueCopyBufferToImage        : TclEnqueueCopyBufferToImage ;
  clEnqueueMapBuffer                : TclEnqueueMapBuffer ;
  clEnqueueMapImage                 : TclEnqueueMapImage ;
  clEnqueueUnmapMemObject           : TclEnqueueUnmapMemObject ;
  clEnqueueNDRangeKernel            : TclEnqueueNDRangeKernel ;
  clEnqueueTask                     : TclEnqueueTask ;
  clEnqueueNativeKernel             : TclEnqueueNativeKernel ;

//==============================================================================
// OpenCL 1.1 method declarations
//==============================================================================

  clCreateSubBuffer                 : TclCreateSubBuffer ;
  clSetMemObjectDestructorCallback  : TclSetMemObjectDestructorCallback ;
  clCreateUserEvent                 : TclCreateUserEvent ;
  clSetUserEventStatus              : TclSetUserEventStatus ;
  clSetEventCallback                : TclSetEventCallback ;
  clEnqueueReadBufferRect           : TclEnqueueReadBufferRect ;
  clEnqueueWriteBufferRect          : TclEnqueueWriteBufferRect ;
  clEnqueueCopyBufferRect           : TclEnqueueCopyBufferRect ;

// deprecated methods

  clCreateImage2D                   : TclCreateImage2D ;
  clCreateImage3D                   : TclCreateImage3D ;
  clUnloadCompiler                  : TclUnloadCompiler ;
  clEnqueueMarker                   : TclEnqueueMarker ;
  clEnqueueWaitForEvents            : TclEnqueueWaitForEvents ;
  clEnqueueBarrier                  : TclEnqueueBarrier ;
  clGetExtensionFunctionAddress     : TclGetExtensionFunctionAddress ;

//==============================================================================
// OpenCL 1.2 method declarations
//==============================================================================

  clCreateSubDevices                : TclCreateSubDevices ;
  clRetainDevice                    : TclRetainDevice ;
  clReleaseDevice                   : TclReleaseDevice ;
  clCreateImage                     : TclCreateImage ;
  clCreateProgramWithBuiltInKernels : TclCreateProgramWithBuiltInKernels ;
  clCompileProgram                  : TclCompileProgram ;
  clLinkProgram                     : TclLinkProgram ;
  clUnloadPlatformCompiler          : TclUnloadPlatformCompiler ;
  clGetKernelArgInfo                : TclGetKernelArgInfo ;
  clEnqueueFillBuffer               : TclEnqueueFillBuffer ;
  clEnqueueFillImage                : TclEnqueueFillImage ;
  clEnqueueMigrateMemObjects        : TclEnqueueMigrateMemObjects ;
  clEnqueueMarkerWithWaitList       : TclEnqueueMarkerWithWaitList ;
  clEnqueueBarrierWithWaitList      : TclEnqueueBarrierWithWaitList ;
  clGetExtensionFunctionAddressForPlatform :
    TclGetExtensionFunctionAddressForPlatform ;

{$ENDIF}


//==============================================================================
// OpenCL library loading
//==============================================================================

type
  T_OpenCLVersion = (
    OpenCL_1_0 = 0,
    OpenCL_1_1 = 1,
    OpenCL_1_2 = 2,
    OpenCL_2_0 = 3,
    OpenCL_2_1 = 4,
    OpenCL_2_2 = 5
  ) ;


  procedure loadOpenCL ;
  {$IFDEF CLR}
    var
      path : String ;
  {$ENDIF}
  begin
    oclAvailable := False ;
    {$IFDEF DCC}
      libOpenCL := LoadLibrary( OPENCL_LIB ) ;
      oclAvailable := libOpenCL <> 0 ;
    {$ENDIF}
    {$IFDEF CLR}
      path := System.Environment.SystemDirectory + '\' + OPENCL_LIB ;
      oclAvailable := FileExists( path ) ;
    {$ENDIF}
  end ;


  procedure freeOpenCL ;
  begin
    {$IFDEF DCC}
      if oclAvailable then
        FreeLibrary( HMODULE( libOpenCL ) ) ;
    {$ENDIF}
  end ;


  procedure initOpenCL(
    const _ver : T_OpenCLVersion ;
    const _dpr : Boolean
  ) ;
    {$IFDEF DCC}
      function attach(
        const _name : String
      ) : Pointer ;
      begin
        Result := GetProcAddress( libOpenCL, PChar( _name ) ) ;
      end ;
   {$ENDIF}
  begin
    {$IFDEF DCC}
      // OpenCL 1.0
      clGetPlatformIDs :=
        attach( 'clGetPlatformIDs' ) ;
      clGetPlatformInfo :=
        attach( 'clGetPlatformInfo' ) ;
      clGetDeviceIDs :=
        attach( 'clGetDeviceIDs' ) ;
      clGetDeviceInfo :=
        attach( 'clGetDeviceInfo' ) ;
      clCreateContext :=
        attach( 'clCreateContext' ) ;
      clCreateContextFromType :=
        attach( 'clCreateContextFromType' ) ;
      clRetainContext :=
        attach( 'clRetainContext' ) ;
      clReleaseContext :=
        attach( 'clReleaseContext' ) ;
      clGetContextInfo :=
        attach( 'clGetContextInfo' ) ;
      clCreateCommandQueue :=
        attach( 'clCreateCommandQueue' ) ; // deprecated in OpenCL 2.0
      clRetainCommandQueue :=
        attach( 'clRetainCommandQueue' ) ;
      clReleaseCommandQueue :=
        attach( 'clReleaseCommandQueue' ) ;
      clGetCommandQueueInfo :=
        attach( 'clGetCommandQueueInfo' ) ;
      clCreateBuffer :=
        attach( 'clCreateBuffer' ) ;
      clRetainMemObject :=
        attach( 'clRetainMemObject' ) ;
      clReleaseMemObject :=
        attach( 'clReleaseMemObject' ) ;
      clGetSupportedImageFormats :=
        attach( 'clGetSupportedImageFormats' ) ;
      clGetMemObjectInfo :=
        attach( 'clGetMemObjectInfo' ) ;
      clGetImageInfo :=
        attach( 'clGetImageInfo' ) ;
      clCreateSampler :=
        attach( 'clCreateSampler' ) ; // deprecated in OpenCL 2.0
      clRetainSampler :=
        attach( 'clRetainSampler' ) ;
      clReleaseSampler :=
        attach( 'clReleaseSampler' ) ;
      clGetSamplerInfo :=
        attach( 'clGetSamplerInfo' ) ;
      clCreateProgramWithSource :=
        attach( 'clCreateProgramWithSource' ) ;
      clCreateProgramWithBinary :=
        attach( 'clCreateProgramWithBinary' ) ;
      clRetainProgram :=
        attach( 'clRetainProgram' ) ;
      clReleaseProgram :=
        attach( 'clReleaseProgram' ) ;
      clBuildProgram :=
        attach( 'clBuildProgram' ) ;
      clGetProgramInfo :=
        attach( 'clGetProgramInfo' ) ;
      clGetProgramBuildInfo :=
        attach( 'clGetProgramBuildInfo' ) ;
      clCreateKernel :=
        attach( 'clCreateKernel' ) ;
      clCreateKernelsInProgram :=
        attach( 'clCreateKernelsInProgram' ) ;
      clRetainKernel :=
        attach( 'clRetainKernel' ) ;
      clReleaseKernel :=
        attach( 'clReleaseKernel' ) ;
      clSetKernelArg :=
        attach( 'clSetKernelArg' ) ;
      clGetKernelInfo :=
        attach( 'clGetKernelInfo' ) ;
      clGetKernelWorkGroupInfo :=
        attach( 'clGetKernelWorkGroupInfo' ) ;
      clWaitForEvents :=
        attach( 'clWaitForEvents' ) ;
      clGetEventInfo :=
        attach( 'clGetEventInfo' ) ;
      clRetainEvent :=
        attach( 'clRetainEvent' ) ;
      clReleaseEvent :=
        attach( 'clReleaseEvent' ) ;
      clGetEventProfilingInfo :=
        attach( 'clGetEventProfilingInfo' ) ;
      clFlush :=
        attach( 'clFlush' ) ;
      clFinish :=
        attach( 'clFinish' ) ;
      clEnqueueReadBuffer :=
        attach( 'clEnqueueReadBuffer' ) ;
      clEnqueueWriteBuffer :=
        attach( 'clEnqueueWriteBuffer' ) ;
      clEnqueueCopyBuffer :=
        attach( 'clEnqueueCopyBuffer' ) ;
      clEnqueueReadImage :=
        attach( 'clEnqueueReadImage' ) ;
      clEnqueueWriteImage :=
        attach( 'clEnqueueWriteImage' ) ;
      clEnqueueCopyImage :=
        attach( 'clEnqueueCopyImage' ) ;
      clEnqueueCopyImageToBuffer :=
        attach( 'clEnqueueCopyImageToBuffer' ) ;
      clEnqueueCopyBufferToImage :=
        attach( 'clEnqueueCopyBufferToImage' ) ;
      clEnqueueMapBuffer :=
        attach( 'clEnqueueMapBuffer' ) ;
      clEnqueueMapImage :=
        attach( 'clEnqueueMapImage' ) ;
      clEnqueueUnmapMemObject :=
        attach( 'clEnqueueUnmapMemObject' ) ;
      clEnqueueNDRangeKernel :=
        attach( 'clEnqueueNDRangeKernel' ) ;
      clEnqueueTask :=
        attach( 'clEnqueueTask' ) ; // deprecated in OpenCL 2.0
      clEnqueueNativeKernel :=
        attach( 'clEnqueueNativeKernel' ) ;

      if _ver < T_OpenCLVersion.OpenCL_1_1 then
        exit ;

      // OpenCL 1.1
      clCreateSubBuffer :=
        attach( 'clCreateSubBuffer' ) ;
      clSetMemObjectDestructorCallback :=
        attach( 'clSetMemObjectDestructorCallback' ) ;
      clCreateUserEvent :=
        attach( 'clCreateUserEvent' ) ;
      clSetUserEventStatus :=
        attach( 'clSetUserEventStatus' ) ;
      clSetEventCallback :=
        attach( 'clSetEventCallback' ) ;
      clEnqueueReadBufferRect :=
        attach( 'clEnqueueReadBufferRect' ) ;
      clEnqueueWriteBufferRect :=
        attach( 'clEnqueueWriteBufferRect' ) ;
      clEnqueueCopyBufferRect :=
        attach( 'clEnqueueCopyBufferRect' ) ;

      // deprecated methods
      if _dpr then begin
        clCreateImage2D :=
          attach( 'clCreateImage2D' ) ;
        clCreateImage3D :=
          attach( 'clCreateImage3D' ) ;
        clUnloadCompiler :=
          attach( 'clUnloadCompiler' ) ;
        clEnqueueMarker :=
          attach( 'clEnqueueMarker' ) ;
        clEnqueueWaitForEvents :=
          attach( 'clEnqueueWaitForEvents' ) ;
        clEnqueueBarrier :=
          attach( 'clEnqueueBarrier' ) ;
        clGetExtensionFunctionAddress :=
          attach( 'clGetExtensionFunctionAddress' ) ;
      end ;

      if _ver < T_OpenCLVersion.OpenCL_1_2 then
        exit ;

      // OpenCL 1.2
      clCreateSubDevices :=
        attach( 'clCreateSubDevices' ) ;
      clRetainDevice :=
        attach( 'clRetainDevice' ) ;
      clReleaseDevice :=
        attach( 'clReleaseDevice' ) ;
      clCreateImage :=
        attach( 'clCreateImage' ) ;
      clCreateProgramWithBuiltInKernels :=
        attach( 'clCreateProgramWithBuiltInKernels' ) ;
      clCompileProgram :=
        attach( 'clCompileProgram' ) ;
      clLinkProgram :=
        attach( 'clLinkProgram' ) ;
      clUnloadPlatformCompiler :=
        attach( 'clUnloadPlatformCompiler' ) ;
      clGetKernelArgInfo :=
        attach( 'clGetKernelArgInfo' ) ;
      clEnqueueFillBuffer :=
        attach( 'clEnqueueFillBuffer' ) ;
      clEnqueueFillImage :=
        attach( 'clEnqueueFillImage' ) ;
      clEnqueueMigrateMemObjects :=
        attach( 'clEnqueueMigrateMemObjects' ) ;
      clEnqueueMarkerWithWaitList :=
        attach( 'clEnqueueMarkerWithWaitList' ) ;
      clEnqueueBarrierWithWaitList :=
        attach( 'clEnqueueBarrierWithWaitList' ) ;
      clGetExtensionFunctionAddressForPlatform :=
        attach( 'clGetExtensionFunctionAddressForPlatform' ) ;
    {$ENDIF}
  end ;


{$IFDEF CLR}
type
//==============================================================================
// OpenCL 1.0 method definitions
//==============================================================================

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetPlatformIDs (
          _num_entries   : TCL_uint ;
          [&Out, MarshalAs( UnmanagedType.LPArray )]
          _platforms     : PPCL_platform_id ;
      out _num_platforms : TCL_uint
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetPlatformInfo (
          _platform             : PCL_platform_id ;
          _param_name           : TCL_platform_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetDeviceIDs (
          _platform    : PCL_platform_id ;
          _device_type : TCL_device_type ;
          _num_entries : TCL_uint ;
          [&Out, MarshalAs( UnmanagedType.LPArray )]
          _devices     : PPCL_device_id ;
      out _num_devices : TCL_uint
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetDeviceInfo (
          _device               : PCL_device_id ;
          _param_name           : TCL_device_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateContext (
          [&In, MarshalAs( UnmanagedType.LPArray )]
          _properties  : PPCL_context_properties ;
          _num_devices : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray )]
          _devices     : PPCL_device_id ;
          _pfn_notify  : TContextNotify ;
          _user_data   : IntPtr ;
      out _errcode_ret : TCL_int
  ) : PCL_context ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateContextFromType (
          [&In, MarshalAs( UnmanagedType.LPArray )]
          _properties  : PPCL_context_properties ;
          _device_type : TCL_device_type ;
          _pfn_notify  : TContextNotify ;
          _user_data   : IntPtr ;
          [&Out, MarshalAs( UnmanagedType.I4 )]
      out _errcode_ret : TCL_int
  ) : PCL_context ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clRetainContext (
          _context : PCL_context
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clReleaseContext (
          _context : PCL_context
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetContextInfo (
          _context              : PCL_context ;
          _param_name           : TCL_context_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateCommandQueue (
          _context     : PCL_context ;
          _device      : PCL_device_id ;
          [MarshalAs( UnmanagedType.U8 )]
          _properties  : TCL_command_queue_properties ;
      out _errcode_ret : TCL_int
  ) : PCL_command_queue ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clRetainCommandQueue (
          _command_queue : PCL_command_queue
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clReleaseCommandQueue (
          _command_queue : PCL_command_queue
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetCommandQueueInfo (
          _command_queue        : PCL_command_queue ;
          [MarshalAs( UnmanagedType.U4 )]
          _param_name           : TCL_command_queue_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateBuffer (
          _context     : PCL_context ;
          _flags       : TCL_mem_flags ;
          _size        : IntPtr ;
          _host_ptr    : IntPtr ;
          [&Out, MarshalAs( UnmanagedType.I4 )]
      out _errcode_ret : TCL_int
  ) : PCL_mem ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clRetainMemObject (
          _memobj : PCL_mem
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clReleaseMemObject (
          _memobj : PCL_mem
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetSupportedImageFormats (
          _context           : PCL_context ;
          _flags             : TCL_mem_flags ;
          _image_type        : TCL_mem_object_type ;
          _num_entries       : TCL_uint ;
          [&Out, MarshalAs( UnmanagedType.LPArray )]
          _image_formats     : PPCL_image_format ;
      out _num_image_formats : TCL_uint
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetMemObjectInfo (
          _memobj               : PCL_mem ;
          _param_name           : TCL_mem_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetImageInfo (
          _image                : PCL_mem ;
          _param_name           : TCL_image_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateSampler (
          _context           : PCL_context ;
          _normalized_coords : TCL_bool ;
          _addressing_mode   : TCL_addressing_mode ;
          _filter_mode       : TCL_filter_mode ;
      out _errcode_ret       : TCL_int
  ) : PCL_sampler ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clRetainSampler (
          _sampler : PCL_sampler
  ) : TCL_sampler ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clReleaseSampler (
          _sampler : PCL_sampler
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetSamplerInfo (
          _sampler              : PCL_sampler ;
          _param_name           : TCL_sampler_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateProgramWithSource (
          _context     : PCL_context ;
          _count       : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.LPStr, SizeParamIndex = 1 )]
          _strings     : PPCLAnsiChar ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1 )]
          _lengths     : PSize_t ;
      out _errcode_ret : TCL_int
  ) : PCL_program ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateProgramWithBinary (
          _context       : PCL_context ;
          _num_devices   : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1 )]
          _device_list   : PPCL_device_id ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1 )]
          _lengths       : PSize_t ;
          [&In, MarshalAs( UnmanagedType.SysUInt )]
          _binaries      : PPByte ;
          [&Out, MarshalAs( UnmanagedType.SysUInt )]
          _binary_status : IntPtr ;
      out _errcode_ret   : TCL_int
  ) : PCL_program ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clRetainProgram (
          _program : PCL_program
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clReleaseProgram (
          _program : PCL_program
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clBuildProgram (
          _program     : PCL_program ;
          _num_devices : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1 )]
          _device_list : PPCL_device_id ;
          [&In, MarshalAs( UnmanagedType.LPStr )]
          _options     : PCLAnsiChar ;
          _pfn_notify  : TProgramNotify ;
          _user_data   : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetProgramInfo (
          _program              : PCL_program ;
          _param_name           : TCL_program_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetProgramBuildInfo (
          _program              : PCL_program ;
          _device               : PCL_device_id ;
          _param_name           : TCL_program_build_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateKernel (
          _program     : PCL_program ;
          [&In,MarshalAs(UnmanagedType.LPStr)]
          _kernel_name : PCLAnsiChar ;
      out _errcode_ret : TCL_int
  ) : PCL_kernel ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateKernelsInProgram (
          _program         : PCL_program ;
          _num_kernels     : TCL_uint ;
          [&Out, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1 )]
          _kernels         : PPCL_kernel ;
      out _num_kernels_ret : TCL_uint
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clRetainKernel (
          _kernel : PCL_kernel
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clReleaseKernel (
          _kernel : PCL_kernel
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clSetKernelArg (
          _kernel    : PCL_kernel ;
          _arg_index : TCL_uint ;
          _arg_size  : IntPtr ;
          _arg_value : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetKernelInfo (
          _kernel               : PCL_kernel ;
          _param_name           : TCL_kernel_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetKernelWorkGroupInfo (
          _kernel               : PCL_kernel ;
          _device               : PCL_device_id ;
          _param_name           : TCL_kernel_work_group_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clWaitForEvents (
          _num_events : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 0 )]
          _event_list : PPCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetEventInfo (
          _event                : PCL_event ;
          _param_name           : TCL_event_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clRetainEvent (
          _event : PCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clReleaseEvent (
          _event : PCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetEventProfilingInfo (
          _event                : PCL_event ;
          _param_name           : TCL_profiling_info ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clFlush (
          _command_queue : PCL_command_queue
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clFinish (
          _command_queue : PCL_command_queue
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueReadBuffer (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_read           : TCL_bool ;
          _offset                  : IntPtr ;
          _cb                      : IntPtr ;
          _ptr                     : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueWriteBuffer (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_write          : TCL_bool ;
          _offset                  : IntPtr ;
          _cb                      : IntPtr ;
          _ptr                     : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueCopyBuffer (
          _command_queue           : PCL_command_queue ;
          _src_buffer              : PCL_mem ;
          _dst_buffer              : PCL_mem ;
          _src_offset              : IntPtr ;
          _dst_offset              : IntPtr ;
          _cb                      : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueReadImage (
          _command_queue           : PCL_command_queue ;
          _image                   : PCL_mem ;
          _blocking_read           : TCL_bool ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _origin                  : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _region                  : PSizet ;
          _row_pitch               : IntPtr ;
          _slice_pitch             : IntPtr ;
          _ptr                     : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueWriteImage (
          _command_queue           : PCL_command_queue ;
          _image                   : PCL_mem ;
          _blocking_write          : TCL_bool ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _origin                  : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _region                  : PSizet ;
          _input_row_pitch         : IntPtr ;
          _input_slice_pitch       : IntPtr ;
          _ptr                     : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueCopyImage (
          _command_queue           : PCL_command_queue ;
          _src_image               : PCL_mem ;
          _dst_image               : PCL_mem ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _src_origin              : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _dst_origin              : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _region                  : PSizet ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueCopyImageToBuffer (
          _command_queue           : PCL_command_queue ;
          _src_image               : PCL_mem ;
          _dst_buffer              : PCL_mem ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _src_origin              : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _region                  : PSizet ;
          _dst_offset              : TSize_t ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueCopyBufferToImage (
          _command_queue           : PCL_command_queue ;
          _src_buffer              : PCL_mem ;
          _dst_image               : PCL_mem ;
          _src_offset              : TSize_t ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _dst_origin              : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _region                  : PSizet ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueMapBuffer (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_map            : TCL_bool ;
          _map_flags               : TCL_map_flags ;
          _offset                  : IntPtr ;
          _cb                      : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event ;
      out _errcode_ret             : TCL_int
  ) : IntPtr ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueMapImage (
          _command_queue           : PCL_command_queue ;
          _image                   : PCL_mem ;
          _blocking_map            : TCL_bool ;
          _map_flags               : TCL_map_flags ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _origin                  : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _region                  : PSizet ;
      out _image_row_pitch         : IntPtr ;
      out _image_slice_pitch       : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event ;
      out _errcode_ret             : TCL_int
  ) : IntPtr ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueUnmapMemObject (
          _command_queue           : PCL_command_queue ;
          _memobj                  : PCL_mem ;
          _mapped_ptr              : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueNDRangeKernel (
          _command_queue           : PCL_command_queue ;
          _kernel                  : PCL_kernel ;
          _work_dim                : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeParamIndex = 2 )]
          _global_work_offset      : PSize_t ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeParamIndex = 2 )]
          _global_work_size        : PSize_t ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeParamIndex = 2 )]
          _local_work_size         : PSize_t ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueTask (
          _command_queue           : PCL_command_queue ;
          _kernel                  : PCL_kernel ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

//?  [ SuppressUnmanagedCodeSecurity,
//?    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
//?    CharSet = CharSet.Ansi
//?    )
//?  ]
//?  function clEnqueueNativeKernel (
//?          _command_queue           : PCL_command_queue ;
//?          _user_func               : TEnqueueUserProc ;
//?          _args                    : IntPtr ;
//?          _cb_args                 : TSize_t ;
//?          _num_mem_objects         : TCL_uint ;
//?    const _mem_list                : PPCL_mem ;
//?    const _args_mem_loc            : IntPtr ;
//?          _num_events_in_wait_list : TCL_uint ;
//?          [&In, MarshalAs( UnmanagedType.LPArray,
//?            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
//?          _event_wait_list         : PPCL_event ;
//?          [&Out, MarshalAs( UnmanagedType.Struct )]
//?      out _event                   : TCL_event
//?  ) : TCL_int ; external ;

//==============================================================================
// OpenCL 1.1 method definitions
//==============================================================================

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateSubBuffer (
          _buffer             : PCL_mem ;
          _flags              : TCL_mem_flags ;
          _buffer_create_type : TCL_buffer_create_type ;
          _buffer_create_info : IntPtr ;
      out _errcode_ret        : TCL_int
  ) : PCL_mem ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clSetMemObjectDestructorCallback (
          _memobj     : PCL_mem ;
          _pfn_notify : TMemObjectNotify ;
          _user_data  : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateUserEvent (
          _context     : PCL_context;
      out _errcode_ret : TCL_int
  ) : PCL_event ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clSetUserEventStatus (
          _event            : PCL_event ;
          _execution_status : TCL_int
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clSetEventCallback (
          _event                      : PCL_event ;
          _command_exec_callback_type : TCL_int ;
          _pfn_notify                 : TEventNotify ;
          _user_data                  : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueReadBufferRect (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_read           : TCL_bool ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _buffer_origin           : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _host_origin             : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _region                  : PSizet ;
          _buffer_row_pitch        : IntPtr ;
          _buffer_slice_pitch      : IntPtr ;
          _host_row_pitch          : IntPtr ;
          _host_slice_pitch        : IntPtr ;
          _ptr                     : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueWriteBufferRect (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _blocking_write          : TCL_bool ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _buffer_origin           : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _host_origin             : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _region                  : PSizet ;
          _buffer_row_pitch        : IntPtr ;
          _buffer_slice_pitch      : IntPtr ;
          _host_row_pitch          : IntPtr ;
          _host_slice_pitch        : IntPtr ;
          _ptr                     : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueCopyBufferRect (
          _command_queue           : PCL_command_queue ;
          _src_buffer              : PCL_mem ;
          _dst_buffer              : PCL_mem ;
          [&In, MarshalAs(UnmanagedType.LPArray, SizeConst = 3)]
          _src_origin              : PSizet ;
          [&In, MarshalAs(UnmanagedType.LPArray, SizeConst = 3)]
          _dst_origin              : PSizet ;
          [&In, MarshalAs(UnmanagedType.LPArray, SizeConst = 3)]
          _region                  : PSizet ;
          _src_row_pitch           : IntPtr ;
          _src_slice_pitch         : IntPtr ;
          _dst_row_pitch           : IntPtr ;
          _dst_slice_pitch         : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs(UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6)]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs(UnmanagedType.Struct)]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

// deprecated methods

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateImage2D (
          _context                 : PCL_context ;
          _flags                   : TCL_mem_flags ;
          _image_format            : PCL_image_format ;
          _image_width             : IntPtr ;
          _image_height            : IntPtr ;
          _image_row_pitch         : IntPtr ;
          _host_ptr                : IntPtr ;
      out _errcode_ret             : TCL_int
  ) : PCL_mem ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateImage3D (
          _context                 : PCL_context ;
          _flags                   : TCL_mem_flags ;
          _image_format            : PCL_image_format ;
          _image_width             : IntPtr ;
          _image_height            : IntPtr ;
          _image_depth             : IntPtr ;
          _image_row_pitch         : IntPtr ;
          _image_slice_pitch       : IntPtr ;
          _host_ptr                : IntPtr ;
      out _errcode_ret             : TCL_int
  ) : PCL_mem ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clUnloadCompiler
    : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueMarker (
          _command_queue           : PCL_command_queue ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueWaitForEvents (
          _command_queue           : PCL_command_queue ;
          _num_events              : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1 )]
          _event_list              : PPCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueBarrier (
          _command_queue           : PCL_command_queue
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetExtensionFunctionAddress (
    const _func_name               : PCLAnsiChar
  ) : IntPtr ; external ;

//==============================================================================
// OpenCL 1.2 method definitions
//==============================================================================

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateSubDevices (
          _in_device       : PCL_device_id ;
          _properties      : TCL_device_partition_property ;
          _num_devices     : TCL_uint ;
          [&Out, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1 )]
          _out_devices     : PPCL_device_id ;
      out _num_devices_ret : TCL_uint
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clRetainDevice (
          _device : PCL_device_id
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clReleaseDevice (
          _device : PCL_device_id
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateImage (
          _context      : PCL_context ;
          _flags        : TCL_mem_flags ;
          _image_format : PCL_image_format ;
          _image_desc   : PCL_image_desc ;
          _host_ptr     : IntPtr ;
      out _errcode_ret  : TCL_int
  ) : PCL_mem ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCreateProgramWithBuiltInKernels (
          _context      : PCL_context ;
          _num_devices  : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1 )]
          _device_list  : PPCL_device_id ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.LPStr, SizeParamIndex = 1 )]
          _kernel_names : PPCLAnsiChar ;
      out _errcode_ret  : TCL_int
  ) : PCL_program ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clCompileProgram (
          _program              : PCL_program ;
          _num_devices          : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 1 )]
          _device_list          : PPCL_device_id ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.LPStr, SizeParamIndex = 1 )]
          _options              : PPCLAnsiChar ;
          _num_input_headers    : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray)]
          _input_headers        : PPCL_program ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.LPStr, SizeParamIndex = 1 )]
          _header_include_names : PPCLAnsiChar ;
          _pfn_notify           : TProgramNotify ;
          _user_data            : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clLinkProgram (
          _context            : PCL_context ;
          _num_devices        : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            SizeParamIndex = 1, ArraySubType = UnmanagedType.SysUInt )]
          _device_list        : PPCL_device_id ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.LPStr, SizeParamIndex = 1 )]
          _options            : PCLAnsiChar ;
          _num_input_programs : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray )]
          _input_programs     : PPCL_program ;
          _pfn_notify         : TProgramNotify ;
          _user_data          : IntPtr ;
      out _errcode_ret        : TCL_int
  ) : PCL_program ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clUnloadPlatformCompiler (
          _platform : PCL_platform_id
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetKernelArgInfo (
          _kernel               : PCL_kernel ;
          _arg_indx             : TCL_uint ;
          _param_name           : IntPtr ;
          _param_value_size     : IntPtr ;
          _param_value          : IntPtr ;
      out _param_value_size_ret : IntPtr
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueFillBuffer (
          _command_queue           : PCL_command_queue ;
          _buffer                  : PCL_mem ;
          _pattern                 : IntPtr ;
          _pattern_size            : IntPtr ;
          _offset                  : IntPtr ;
          _size                    : IntPtr ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueFillImage (
          _command_queue           : PCL_command_queue ;
          _image                   : PCL_mem ;
          _fill_color              : IntPtr ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _origin                  : PSizet ;
          [&In, MarshalAs( UnmanagedType.LPArray, SizeConst = 3 )]
          _region                  : PSizet ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueMigrateMemObjects (
          _command_queue           : PCL_command_queue ;
          _num_mem_objects         : TCL_uint ;
          _mem_objects             : PPCL_mem ;
          _flags                   : TCL_mem_migration_flags ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueMarkerWithWaitList (
          _command_queue           : PCL_command_queue ;
          _num_events_in_wait_list : TCL_uint ;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs( UnmanagedType.Struct )]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clEnqueueBarrierWithWaitList (
          _command_queue           : PCL_command_queue;
          _num_events_in_wait_list : TCL_uint;
          [&In, MarshalAs( UnmanagedType.LPArray,
            ArraySubType = UnmanagedType.SysUInt, SizeParamIndex = 6 )]
          _event_wait_list         : PPCL_event ;
          [&Out, MarshalAs(UnmanagedType.Struct)]
      out _event                   : TCL_event
  ) : TCL_int ; external ;

  [ SuppressUnmanagedCodeSecurity,
    DllImport( OPENCL_LIB, CallingConvention = CallingConvention.StdCall,
    CharSet = CharSet.Ansi
    )
  ]
  function clGetExtensionFunctionAddressForPlatform (
          _platform : PCL_platform_id ;
    const func_name : PCLAnsiChar
  ) : IntPtr ; external ;

{$ENDIF}


type
//==============================================================================
// OpenCL types wrappers
//==============================================================================

  T_clImage = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pImage : PCL_mem ;
    private
      FFormat      : TCL_image_format ;
      FDescription : TCL_image_desc ;
    public
      constructor Create ( const _ctx    : PCL_context ;
                           const _flags  : TGIS_OpenCLMemoryFlags ;
                           const _format : PCL_image_format ;
                           const _desc   : PCL_image_desc ;
                           {$IFDEF DCC}
                             const _data : Pointer
                           {$ENDIF}
                           {$IFDEF CLR}
                             const _data : IntPtr
                           {$ENDIF}
                         ) ;
    protected
      procedure doDestroy ; override ;
  end ;


  T_clImage2D = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pImage2D : PCL_mem ;
    private
      FFormat   : TCL_image_format ;
      FWidth    : TSize_t ;
      FHeight   : TSize_t ;
      FRowPitch : TSize_t ;
    public
      constructor Create ( const _ctx    : PCL_context ;
                           const _flags  : TGIS_OpenCLMemoryFlags ;
                           const _format : PCL_image_format ;
                           const _width  : TSize_t ;
                           const _height : TSize_t ;
                           const _rpitch : TSize_t ;
                           {$IFDEF DCC}
                             const _data : Pointer
                           {$ENDIF}
                           {$IFDEF CLR}
                             const _data : IntPtr
                           {$ENDIF}
                         ) ;
    protected
      procedure doDestroy ; override ;
  end ;


  T_clImage3D = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pImage3D : PCL_mem ;
    private
      FFormat     : TCL_image_format ;
      FWidth      : TSize_t ;
      FHeight     : TSize_t ;
      FDepth      : TSize_t ;
      FRowPitch   : TSize_t ;
      FSlicePitch : TSize_t ;
    public
      constructor Create ( const _ctx    : PCL_context ;
                           const _flags  : TGIS_OpenCLMemoryFlags ;
                           const _format : PCL_image_format ;
                           const _width  : TSize_t ;
                           const _height : TSize_t ;
                           const _depth  : TSize_t ;
                           const _rpitch : TSize_t ;
                           const _spitch : TSize_t ;
                           {$IFDEF DCC}
                             const _data : Pointer
                           {$ENDIF}
                           {$IFDEF CLR}
                             const _data : IntPtr
                           {$ENDIF}
                         ) ;
    protected
      procedure doDestroy ; override ;
  end ;


  T_clBuffer = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pBuffer : PCL_mem ;
    private
      FSize     : TSize_t ;
      FKerIndex : Integer ;
      FArgIndex : Integer ;
    public
      constructor Create ( const _ctx    : PCL_context ;
                           const _flags  : TGIS_OpenCLMemoryFlags ;
                           const _size   : TSize_t ;
                           {$IFDEF DCC}
                             const _data : Pointer ;
                           {$ENDIF}
                           {$IFDEF CLR}
                             const _data : IntPtr ;
                           {$ENDIF}
                           const _keridx : Integer ;
                           const _argidx : Integer
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      property Size          : TSize_t
                               read  FSize ;
      property KernelIndex   : Integer
                               read FKerIndex ;
      property ArgumentIndex : Integer
                               read FArgIndex ;
  end ;


  T_clKernel = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pKernel : PCL_kernel ;
    private
      FName : String ;
    private
      function  fget_ArgumentCount   : Integer ;
      function  fget_WorkDimension   : Integer ;
      procedure fset_WorkDimension   ( const _val : Integer
                                     ) ;
    public
      constructor Create( const _prog : PCL_program ;
                          const _name : String
                        ) ;
    protected
      procedure doDestroy ; override ;
    public
      procedure SetArgument ( const _idx    : TCL_uint ;
                              const _size   : TSize_t ;
                              {$IFDEF DCC}
                                const _data : Pointer
                              {$ENDIF}
                              {$IFDEF CLR}
                                const _data : IntPtr
                              {$ENDIF}
                            ) ; overload ;
      procedure SetArgument ( const _idx    : TCL_uint ;
                              const _data   : T_clBuffer
                            ) ; overload ;
      procedure SetArgument ( const _idx    : TCL_uint ;
                              const _data   : T_clImage
                            ) ; overload ;
    public
      property Name          : String
                               read  FName ;
      property ArgumentCount : Integer
                               read  fget_ArgumentCount ;
      property WorkDimension : Integer
                               read  fget_WorkDimension
                               write fset_WorkDimension ;
    public
      GlobalWorkSizes : array of NativeUInt ;
      LocalWorkSizes  : array of NativeUInt ;
      IsArgumentSet   : array of Boolean ;
  end ;


  T_clBinaries = class ;


  T_clProgram = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pProgram : PCL_program ;
    public
      constructor Create ( const _ctx : PCL_context ;
                           const _src : String ;
                           const _opt : String
                         ) ; overload ;
      constructor Create ( const _ctx : PCL_context ;
                           const _bin : T_clBinaries ;
                           const _opt : String
                         ) ; overload ;
    protected
      procedure doDestroy ; override ;
    public
      function  CreateKernel ( const _name : String
                             ) : T_clKernel ;
  end ;


  T_clCommandQueue = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pCommandQueue : PCL_command_queue ;
    public
      constructor Create ( const _dev  : PCL_device_id ;
                           const _ctx  : PCL_context ;
                           const _ooo  : Boolean ;
                           const _prof : Boolean
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      procedure ReadBuffer   ( const _buf    : T_clBuffer ;
                               const _size   : TSize_t ;
                               {$IFDEF DCC}
                                 const _data : Pointer
                               {$ENDIF}
                               {$IFDEF CLR}
                                 const _data : IntPtr
                               {$ENDIF}
                             ) ;
      procedure WriteBuffer  ( const _buf    : T_clBuffer ;
                               const _size   : TSize_t ;
                               {$IFDEF DCC}
                                 const _data : Pointer
                               {$ENDIF}
                               {$IFDEF CLR}
                                 const _data : IntPtr
                               {$ENDIF}
                             ) ;
      procedure ReadImage2D  ( const _image  : T_clImage2D ;
                               const _width  : TSize_t ;
                               const _height : TSize_t ;
                               {$IFDEF DCC}
                                 const _data : Pointer
                               {$ENDIF}
                               {$IFDEF CLR}
                                 const _data : IntPtr
                               {$ENDIF}
                             ) ;
      procedure WriteImage2D ( const _image  : T_clImage2D ;
                               const _width  : TSize_t ;
                               const _height : TSize_t ;
                               {$IFDEF DCC}
                                 const _data : Pointer
                               {$ENDIF}
                               {$IFDEF CLR}
                                 const _data : IntPtr
                               {$ENDIF}
                             ) ;
      procedure Enqueue      ( const _kern   : T_clKernel
                             ) ;
      procedure Execute      ; overload ;
  end ;


  T_clContext = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pContext : PCL_context ;
    private
      FDeviceCount : TCL_uint ;
    public
      constructor Create ( _id : PCL_device_id
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      property DeviceCount : TCL_uint
                             read  FDeviceCount ;
  end ;


  T_clDeviceVendor = (
    Nvidia,
    AMD,
    Intel,
    Unknown
  ) ;


  T_clDevice = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pDevice        : PCL_device_id ;
      oDeviceInfo    : TGIS_OpenCLDeviceInfo ;
    private
      FName                       : String ;
      FVendor                     : String ;
      FVersion                    : String ;
      FProfile                    : String ;
      FIsCPU                      : Boolean ;
      FIsGPU                      : Boolean ;
      FIsAccelerator              : Boolean ;
      FIsDefault                  : Boolean ;
      FMaxWorkGroupSize           : TSize_t ;
      FPreferredVectorWidthChar   : TCL_uint ;
      FPreferredVectorWidthShort  : TCL_uint ;
      FPreferredVectorWidthInt    : TCL_uint ;
      FPreferredVectorWidthLong   : TCL_uint ;
      FPreferredVectorWidthFloat  : TCL_uint ;
      FPreferredVectorWidthDouble : TCL_uint ;
      FPreferredVectorWidthHalf   : TCL_uint ;
      FNativeVectorWidthChar      : TCL_uint ;
      FNativeVectorWidthShort     : TCL_uint ;
      FNativeVectorWidthInt       : TCL_uint ;
      FNativeVectorWidthLong      : TCL_uint ;
      FNativeVectorWidthFloat     : TCL_uint ;
      FNativeVectorWidthDouble    : TCL_uint ;
      FNativeVectorWidthHalf      : TCL_uint ;
      FMaxClockFrequency          : TCL_uint ;
      FAddressBits                : TCL_uint ;
      FMaxMemAllocSize            : TCL_ulong ;
      FIsImageSupport             : Boolean ;
      FMaxReadImageArgs           : TCL_uint ;
      FMaxWriteImageArgs          : TCL_uint ;
      FImage2DMaxWidth            : TSize_t ;
      FImage2DMaxHeight           : TSize_t ;
      FImage3DMaxWidth            : TSize_t ;
      FImage3DMaxHeight           : TSize_t ;
      FImage3DMaxDepth            : TSize_t ;
      FMaxSamplers                : TCL_uint ;
      FMaxParameterSize           : TSize_t ;
      FMemBaseAddrAlign           : TCL_uint ;
      FMinDataTypeAlignSize       : TCL_uint ;
      FGlobalMemCacheLineSize     : TCL_uint ;
      FGlobalMemCacheSize         : TCL_ulong ;
      FGlobalMemSize              : TCL_ulong ;
      FMaxConstantBufferSize      : TCL_ulong ;
      FMaxConstantArgs            : TCL_uint ;
      FLocalMemSize               : TCL_ulong ;
      FIsErrorCorrectionSupport   : Boolean ;
      FIsHostUnifiedMemory        : Boolean ;
      FProfilingTimerResolution   : TSize_t ;
      FIsEndianLittle             : Boolean ;
      FIsAvailable                : Boolean ;
      FIsCompilerAvailable        : Boolean ;
      FVendorId                   : TCL_uint ;
      FMaxComputeUnits            : TCL_uint ;
      FMaxWorkItemDimensions      : TCL_uint ;
      FExtensionCount             : Integer ;
      FExtensions                 : array of String ;
      FOpenCLCVersion             : String ;
      FDriverVersion              : String ;
    private
      FDeviceVendor               : T_clDeviceVendor ;
      FDeviceType                 : TGIS_OpenCLDeviceType ;
      FContext                    : T_clContext ;
    private
      function  fget_Extensions ( const _idx : Integer
                                ) : String ;
    public
      constructor Create ( const _id : PCL_device_id
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      function  CreateCommandQueue ( const _ooo    : Boolean ;
                                     const _prof   : Boolean
                                   ) : T_clCommandQueue ;
      function  CreateProgram      ( const _src    : String ;
                                     const _opt    : String
                                   ) : T_clProgram ; overload ;
      function  CreateProgram      ( const _bin    : T_clBinaries ;
                                     const _opt    : String
                                   ) : T_clProgram ; overload ;
      function  CreateBuffer       ( const _flags  : TGIS_OpenCLMemoryFlags ;
                                     const _size   : TSize_t ;
                                     {$IFDEF DCC}
                                       const _data : Pointer ;
                                     {$ENDIF}
                                     {$IFDEF CLR}
                                       const _data : IntPtr ;
                                     {$ENDIF}
                                     const _keridx : Integer ;
                                     const _argidx : Integer
                                   ) : T_clBuffer ;
      function  CreateImage        ( const _flags  : TGIS_OpenCLMemoryFlags ;
                                     const _format : PCL_image_format ;
                                     const _desc   : PCL_image_desc ;
                                     {$IFDEF DCC}
                                       const _data : Pointer
                                     {$ENDIF}
                                     {$IFDEF CLR}
                                       const _data : IntPtr
                                     {$ENDIF}
                                   ) : T_clImage ;
      function  CreateImage2D      ( const _flags  : TGIS_OpenCLMemoryFlags ;
                                     const _format : PCL_image_format ;
                                     const _width  : TSize_t ;
                                     const _height : TSize_t ;
                                     const _rpitch : TSize_t ;
                                     {$IFDEF DCC}
                                       const _data : Pointer
                                     {$ENDIF}
                                     {$IFDEF CLR}
                                       const _data : IntPtr
                                     {$ENDIF}
                                   ) : T_clImage2D ;
      function  CreateImage3D      ( const _flags  : TGIS_OpenCLMemoryFlags ;
                                     const _format : PCL_image_format ;
                                     const _width  : TSize_t ;
                                     const _height : TSize_t ;
                                     const _depth  : TSize_t ;
                                     const _rpitch : TSize_t ;
                                     const _spitch : TSize_t ;
                                     {$IFDEF DCC}
                                       const _data : Pointer
                                     {$ENDIF}
                                     {$IFDEF CLR}
                                       const _data : IntPtr
                                     {$ENDIF}
                                   ) : T_clImage3D ;
    public
      property Name              : String
                                   read  FName ;
      property Vendor            : String
                                   read  FVendor ;
      property Version           : String
                                   read  FVersion ;
      property Profile           : String
                                   read  FProfile ;
      property OpenCLCVersion    : String
                                   read  FOpenCLCVersion ;
      property DriverVersion     : String
                                   read  FDriverVersion ;
      property MaxWorkGroupSize  : TSize_t
                                   read  FMaxWorkGroupSize ;
      property MaxClockFrequency : TCL_uint
                                   read  FMaxClockFrequency ;
      property MaxMemAllocSize   : TCL_ulong
                                   read  FMaxMemAllocSize ;
      property MaxComputeUnits   : TCL_uint
                                   read  FMaxComputeUnits ;
      property IsAvailable       : Boolean
                                   read  FIsAvailable ;
      property IsGPU             : Boolean
                                   read  FIsGPU ;
      property IsCPU             : Boolean
                                   read  FIsCPU ;
      property IsAccelerator     : Boolean
                                   read  FIsAccelerator ;
      property ExtensionCount    : Integer
                                   read  FExtensionCount ;
      property Extensions        [const _idx : Integer]
                                 : String
                                   read  fget_Extensions ;
    public
      property DeviceVendor      : T_clDeviceVendor
                                   read  FDeviceVendor ;
      property DeviceType        : TGIS_OpenCLDeviceType
                                   read  FDeviceType ;
      property Context           : T_clContext
                                   read  FContext ;
  end ;


  T_clPlatform = class ( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pPlatform       : PCL_platform_id ;
      oPlatformInfo   : TGIS_OpenCLPlatformInfo ;
    private
      FProfile        : String ;
      FVersion        : String ;
      FName           : String ;
      FVendor         : String ;
      FDeviceCount    : TCL_uint ;
      FDevices        : array of T_clDevice ;
      FExtensionCount : Integer ;
      FExtensions     : array of String ;
    private
      function  fget_Devices    ( const _idx : Integer
                                ) : T_clDevice ;
      function  fget_Extensions ( const _idx : Integer
                                ) : String ;
    public
      constructor Create ( const _id : PCL_platform_id
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      property Profile        : String
                                read  FProfile ;
      property Version        : String
                                read  FVersion ;
      property Name           : String
                                read  FName ;
      property Vendor         : String
                                read  FVendor ;
      property DeviceCount    : TCL_uint
                                read  FDeviceCount ;
      property Devices [const _idx : Integer]
                              : T_clDevice
                                read  fget_Devices ;
      property ExtensionCount : Integer
                                read  FExtensionCount ;
      property Extensions [const _idx : Integer]
                              : String
                                read  fget_Extensions ;
  end ;


  T_clPlatforms = class ( TGIS_ObjectDisposable )
    private
      FPlatformCount    : TCL_uint ;
      FPlatforms        : array of T_clPlatform ;
    private
      function  fget_Platforms ( const _idx : Integer
                               ) : T_clPlatform ;
      function  fget_AvailableDeviceCount
                                 : Integer ;
    public
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      function  GetBestAvailableDevice : T_clDevice ;
    public
      property PlatformCount : TCL_uint
                               read  FPlatformCount ;
      property Platforms     [const _idx : Integer]
                             : T_clPlatform
                               read  fget_Platforms ;
      property AvailableDeviceCount
                             : Integer
                               read  fget_AvailableDeviceCount ;
  end ;


//==============================================================================
// local type definitions
//==============================================================================

  {$IFDEF DCC}
    T_clDeviceArray = array of TCL_device_id ;
  {$ENDIF}
  {$IFDEF CLR}
    T_clDeviceArray = array of PCL_device_id ;
  {$ENDIF}
  T_clSizeArray = array of TSize_t ;
  T_clByteArray = array of Byte ;
  T_clBinariesArray = array of T_clByteArray ;


  T_clBinaries = class ( TGIS_ObjectDisposable )
    private
      FDeviceCount : TCL_uint ;
      FDevices     : T_clDeviceArray ;
      FBinarySizes : T_clSizeArray ;
      FBinaries    : T_clBinariesArray ;
      FKernel      : String ;
    public
      constructor Create ( const _prog : T_clProgram ;
                           const _kern : String
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      property DeviceCount : TCL_uint
                             read  FDeviceCount ;
      property Devices     : T_clDeviceArray
                             read FDevices ;
      property BinarySizes : T_clSizeArray
                             read  FBinarySizes ;
      property Binaries    : T_clBinariesArray
                             read  FBinaries ;
      property Kernel      : String
                             read  FKernel ;
  end ;


  T_clBinariesCache = class ( TGIS_ObjectDisposable )
    private
      {$IFDEF DCC}
        oBinaries : TObjectList<T_clBinaries> ;
      {$ENDIF}
      {$IFDEF CLR}
        oBinaries : TList<T_clBinaries> ;
      {$ENDIF}
    private
      function  indexOf ( const _kern : String
                        ) : Integer ;
    public
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      function  Find    ( const _kern : String
                        ) : T_clBinaries ;
      procedure Add     ( const _prog : T_clProgram ;
                          const _kern : String
                        ) ;
      procedure Delete  ( const _kern : String
                        ) ;
      procedure Clear   ;
  end ;


var
  oclPlatforms : T_clPlatforms ;
  oclBinaries  : T_clBinariesCache  ;


//==============================================================================
// T_clBinaries
//==============================================================================

  constructor T_clBinaries.Create(
    const _prog : T_clProgram ;
    const _kern : String
  ) ;
  var
    {$IFDEF CLR}
      abin  : array of IntPtr ;
      gch   : GCHandle ;
      val   : IntPtr ;
      dummy : IntPtr ;
      arr   : array of Integer ;
    {$ENDIF}
    i : Integer ;
  begin
    inherited Create ;

    {$IFDEF DCC}
      oclError := clGetProgramInfo( _prog.pProgram, T_clN.CL_PROGRAM_NUM_DEVICES,
                    sizeOf( FDeviceCount ), @FDeviceCount, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FDeviceCount ) ) ;
      oclError := clGetProgramInfo( _prog.pProgram, T_clN.CL_PROGRAM_NUM_DEVICES,
                    sizeOf( FDeviceCount ), val, dummy ) ;
      FDeviceCount := Marshal.ReadInt32( val ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    SetLength( FDevices, FDeviceCount ) ;
    {$IFDEF DCC}
      oclError := clGetProgramInfo( _prog.pProgram, T_clN.CL_PROGRAM_DEVICES,
                    sizeOf( FDevices ), @FDevices[0], nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FDevices ) ) ;
      oclError := clGetProgramInfo( _prog.pProgram, T_clN.CL_PROGRAM_DEVICES,
                    sizeOf( FDevices ), val, dummy ) ;
      Marshal.Copy( val, FDevices, 0, FDeviceCount ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    SetLength( FBinarySizes, FDeviceCount ) ;
    {$IFDEF DCC}
      oclError := clGetProgramInfo( _prog.pProgram, T_clN.CL_PROGRAM_BINARY_SIZES,
                    sizeOf( FBinarySizes ), @FBinarySizes[0], nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FBinarySizes ) ) ;
      oclError := clGetProgramInfo( _prog.pProgram, T_clN.CL_PROGRAM_BINARY_SIZES,
                    sizeOf( FBinarySizes ), val, dummy ) ;
      SetLength( arr, FDeviceCount ) ;
      Marshal.Copy( val, arr, 0, FDeviceCount ) ;
      Marshal.FreeHGlobal( val ) ;
      for i := 0 to FDeviceCount - 1 do
        FBinarySizes[i] := TSize_t( arr[i] ) ;
    {$ENDIF}

    SetLength( FBinaries, FDeviceCount ) ;
    for i := 0 to FDeviceCount - 1 do
      SetLength( FBinaries[i], FBinarySizes[i] ) ;
    {$IFDEF DCC}
      oclError := clGetProgramInfo( _prog.pProgram, T_clN.CL_PROGRAM_BINARIES,
                    sizeOf( FBinaries ), @FBinaries[0], nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      SetLength( abin, FDeviceCount ) ;
      for i := 0 to FDeviceCount - 1 do
        abin[i] := Marshal.AllocHGlobal( FBinarySizes[i] ) ;
      gch := GCHandle.Alloc( abin, GCHandleType.Pinned ) ;
      oclError := clGetProgramInfo( _prog.pProgram, T_clN.CL_PROGRAM_BINARIES,
                    sizeOf( FBinaries ), gch.AddrOfPinnedObject, dummy ) ;
      for i := 0 to FDeviceCount - 1 do begin
        Marshal.Copy( abin[i], FBinaries[i], 0, FBinarySizes[i] ) ;
        Marshal.FreeHGlobal( abin[i] ) ;
      end ;
      gch.Free ;
   {$ENDIF}

    FKernel := _kern ;
  end ;


  procedure T_clBinaries.doDestroy ;
  begin

    inherited ;
  end ;


//==============================================================================
// T_clBinariesCache
//==============================================================================

  constructor T_clBinariesCache.Create ;
  begin
    inherited ;

    {$IFDEF DCC}
      oBinaries := TObjectList<T_clBinaries>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      oBinaries := TList<T_clBinaries>.Create ;
    {$ENDIF}
  end ;


  procedure T_clBinariesCache.doDestroy ;
  begin
    FreeObject( oBinaries ) ;

    inherited ;
  end ;


  function T_clBinariesCache.indexOf(
    const _kern : String
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := -1 ;
    for i := 0 to oBinaries.Count - 1 do begin
      if oBinaries[i].Kernel = _kern then begin
        Result := i ;
        break ;
      end ;
    end ;
  end ;


  function T_clBinariesCache.Find(
    const _kern : String
  ) : T_clBinaries ;
  var
    i : Integer ;
  begin
    Result := nil ;

    i := indexOf( _kern ) ;
    if i >= 0 then
      Result := oBinaries[i] ;
  end ;


  procedure T_clBinariesCache.Add(
    const _prog : T_clProgram ;
    const _kern : String
  ) ;
  var
    o : T_clBinaries ;
  begin
    o := T_clBinaries.Create( _prog, _kern ) ;
    oBinaries.Add( o ) ;
  end ;


  procedure T_clBinariesCache.Delete(
    const _kern : String
  ) ;
  var
    i : Integer ;
  begin
    i := indexOf( _kern ) ;
    if i >= 0 then
      {$IFDEF DCC}
        oBinaries.Delete( i ) ;
      {$ENDIF}
      {$IFDEF CLR}
        oBinaries.RemoveAt( i ) ;
      {$ENDIF}
  end ;


  procedure T_clBinariesCache.Clear ;
  begin
    oBinaries.Clear ;
  end ;


//==============================================================================
// T_clImage
//==============================================================================

  constructor T_clImage.Create(
    const _ctx    : PCL_context ;
    const _flags  : TGIS_OpenCLMemoryFlags ;
    const _format : PCL_image_format ;
    const _desc   : PCL_image_desc ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) ;
  var
    flg  : TCL_mem_flags ;
  begin
    inherited Create ;

    flg := 0 ;
    if TGIS_OpenCLMemoryFlag.ReadWrite in _flags then
      flg := flg or T_clN.CL_MEM_READ_WRITE ;
    if TGIS_OpenCLMemoryFlag.WriteOnly in _flags then
      flg := flg or T_clN.CL_MEM_WRITE_ONLY ;
    if TGIS_OpenCLMemoryFlag.ReadOnly in _flags then
      flg := flg or T_clN.CL_MEM_READ_ONLY ;
    if TGIS_OpenCLMemoryFlag.UseHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_USE_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.AllocHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_ALLOC_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.CopyHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_COPY_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.HostWriteOnly in _flags then
      flg := flg or T_clN.CL_MEM_HOST_WRITE_ONLY ;
    if TGIS_OpenCLMemoryFlag.HostReadOnly in _flags then
      flg := flg or T_clN.CL_MEM_HOST_READ_ONLY ;
    if TGIS_OpenCLMemoryFlag.HostNoAccess in _flags then
      flg := flg or T_clN.CL_MEM_HOST_NO_ACCESS ;

    {$IFDEF DCC}
      FFormat := _format^ ;
      FDescription := _desc^ ;
    {$ENDIF}
    {$IFDEF CLR}
      FFormat := TCL_image_format(
        Marshal.PtrToStructure( _format, typeOf( TCL_image_format ) )
      ) ;
      FDescription := TCL_image_desc(
        Marshal.PtrToStructure( _desc, typeOf( TCL_image_desc ) )
      ) ;
    {$ENDIF}

    {$IFDEF DCC}
      pImage := clCreateImage(
        _ctx, flg, @FFormat, @FDescription, _data, @oclError
      ) ;
    {$ENDIF}
    {$IFDEF CLR}
      pImage := clCreateImage(
        _ctx, flg, _format, _desc, _data,  out oclError
      ) ;
    {$ENDIF}
  end ;


  procedure T_clImage.doDestroy ;
  begin
    oclError := clReleaseMemObject( pImage ) ;

    inherited ;
  end ;


//==============================================================================
// T_clImage2D
//==============================================================================

  constructor T_clImage2D.Create(
    const _ctx    : PCL_context ;
    const _flags  : TGIS_OpenCLMemoryFlags ;
    const _format : PCL_image_format ;
    const _width  : TSize_t ;
    const _height : TSize_t ;
    const _rpitch : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) ;
  var
    flg  : TCL_mem_flags ;
  begin
    inherited Create ;

    flg := 0 ;
    if TGIS_OpenCLMemoryFlag.ReadWrite in _flags then
      flg := flg or T_clN.CL_MEM_READ_WRITE ;
    if TGIS_OpenCLMemoryFlag.WriteOnly in _flags then
      flg := flg or T_clN.CL_MEM_WRITE_ONLY ;
    if TGIS_OpenCLMemoryFlag.ReadOnly in _flags then
      flg := flg or T_clN.CL_MEM_READ_ONLY ;
    if TGIS_OpenCLMemoryFlag.UseHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_USE_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.AllocHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_ALLOC_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.CopyHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_COPY_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.HostWriteOnly in _flags then
      flg := flg or T_clN.CL_MEM_HOST_WRITE_ONLY ;
    if TGIS_OpenCLMemoryFlag.HostReadOnly in _flags then
      flg := flg or T_clN.CL_MEM_HOST_READ_ONLY ;
    if TGIS_OpenCLMemoryFlag.HostNoAccess in _flags then
      flg := flg or T_clN.CL_MEM_HOST_NO_ACCESS ;

    {$IFDEF DCC}
      FFormat := _format^ ;
    {$ENDIF}
    {$IFDEF CLR}
      FFormat := TCL_image_format(
        Marshal.PtrToStructure( _format, typeOf( TCL_image_format ) )
      ) ;
    {$ENDIF}
    FWidth := _width ;
    FHeight := _height ;
    FRowPitch := _rpitch ;

    {$IFDEF DCC}
      pImage2D := clCreateImage2D( _ctx, flg, @FFormat,
        FWidth, FHeight, FRowPitch, _data, @oclError
      ) ;
    {$ENDIF}
    {$IFDEF CLR}
      pImage2D := clCreateImage2D( _ctx, flg, _format,
        FWidth, FHeight, FRowPitch, _data, out oclError
      ) ;
    {$ENDIF}
  end ;


  procedure T_clImage2D.doDestroy ;
  begin
    oclError := clReleaseMemObject( pImage2D ) ;

    inherited ;
  end ;


//==============================================================================
// T_clImage3D
//==============================================================================

  constructor T_clImage3D.Create(
    const _ctx    : PCL_context ;
    const _flags  : TGIS_OpenCLMemoryFlags ;
    const _format : PCL_image_format ;
    const _width  : TSize_t ;
    const _height : TSize_t ;
    const _depth  : TSize_t ;
    const _rpitch : TSize_t ;
    const _spitch : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) ;
  var
    flg  : TCL_mem_flags ;
  begin
    inherited Create ;

    flg := 0 ;
    if TGIS_OpenCLMemoryFlag.ReadWrite in _flags then
      flg := flg or T_clN.CL_MEM_READ_WRITE ;
    if TGIS_OpenCLMemoryFlag.WriteOnly in _flags then
      flg := flg or T_clN.CL_MEM_WRITE_ONLY ;
    if TGIS_OpenCLMemoryFlag.ReadOnly in _flags then
      flg := flg or T_clN.CL_MEM_READ_ONLY ;
    if TGIS_OpenCLMemoryFlag.UseHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_USE_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.AllocHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_ALLOC_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.CopyHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_COPY_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.HostWriteOnly in _flags then
      flg := flg or T_clN.CL_MEM_HOST_WRITE_ONLY ;
    if TGIS_OpenCLMemoryFlag.HostReadOnly in _flags then
      flg := flg or T_clN.CL_MEM_HOST_READ_ONLY ;
    if TGIS_OpenCLMemoryFlag.HostNoAccess in _flags then
      flg := flg or T_clN.CL_MEM_HOST_NO_ACCESS ;

    {$IFDEF DCC}
      FFormat := _format^ ;
    {$ENDIF}
    {$IFDEF CLR}
      FFormat := TCL_image_format(
        Marshal.PtrToStructure( _format, typeOf( TCL_image_format ) )
      ) ;
    {$ENDIF}
    FWidth := _width ;
    FHeight := _height ;
    FDepth := _depth ;
    FRowPitch := _rpitch ;
    FSlicePitch := _spitch ;

    {$IFDEF DCC}
      pImage3D := clCreateImage3D( _ctx, flg, @FFormat,
        FWidth, FHeight, FDepth, FRowPitch, FSlicePitch, _data, @oclError
      ) ;
    {$ENDIF}
    {$IFDEF CLR}
      pImage3D := clCreateImage3D( _ctx, flg, _format,
        FWidth, FHeight, FDepth, FRowPitch, FSlicePitch, _data, out oclError
      ) ;
    {$ENDIF}
  end ;


  procedure T_clImage3D.doDestroy ;
  begin
    oclError := clReleaseMemObject( pImage3D ) ;

    inherited ;
  end ;


//==============================================================================
// T_clBuffer
//==============================================================================

  constructor T_clBuffer.Create(
    const _ctx   : PCL_context ;
    const _flags : TGIS_OpenCLMemoryFlags ;
    const _size  : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer ;
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr ;
    {$ENDIF}
    const _keridx : Integer ;
    const _argidx : Integer
  ) ;
  var
    flg : TCL_mem_flags ;
  begin
    inherited Create ;

    flg := 0 ;
    if TGIS_OpenCLMemoryFlag.ReadWrite in _flags then
      flg := flg or T_clN.CL_MEM_READ_WRITE ;
    if TGIS_OpenCLMemoryFlag.WriteOnly in _flags then
      flg := flg or T_clN.CL_MEM_WRITE_ONLY ;
    if TGIS_OpenCLMemoryFlag.ReadOnly in _flags then
      flg := flg or T_clN.CL_MEM_READ_ONLY ;
    if TGIS_OpenCLMemoryFlag.UseHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_USE_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.AllocHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_ALLOC_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.CopyHostPtr in _flags then
      flg := flg or T_clN.CL_MEM_COPY_HOST_PTR ;
    if TGIS_OpenCLMemoryFlag.HostWriteOnly in _flags then
      flg := flg or T_clN.CL_MEM_HOST_WRITE_ONLY ;
    if TGIS_OpenCLMemoryFlag.HostReadOnly in _flags then
      flg := flg or T_clN.CL_MEM_HOST_READ_ONLY ;
    if TGIS_OpenCLMemoryFlag.HostNoAccess in _flags then
      flg := flg or T_clN.CL_MEM_HOST_NO_ACCESS ;

    FSize := _size ;
    FKerIndex := _keridx ;
    FArgIndex := _argidx ;

    {$IFDEF DCC}
      pBuffer := clCreateBuffer( _ctx, flg, _size, _data, @oclError ) ;
    {$ENDIF}
    {$IFDEF CLR}
      pBuffer := clCreateBuffer( _ctx, flg, _size, _data, out oclError ) ;
    {$ENDIF}
  end ;


  procedure T_clBuffer.doDestroy ;
  begin
    oclError := clReleaseMemObject( pBuffer ) ;

    inherited ;
  end ;


//==============================================================================
// T_clKernel
//==============================================================================

  constructor T_clKernel.Create(
    const _prog : PCL_program ;
    const _name : String
  ) ;
  var
    cnt : Integer ;
    i   : Integer ;
    {$IFDEF DCC}
      str : TCLAnsiString ;
    {$ENDIF}
  begin
    inherited Create ;

    FName := _name ;

    {$IFDEF DCC}
      str := AnsiStringToBytes( _name ) ;
      pKernel := clCreateKernel( _prog, @str[0], @oclError ) ;
    {$ENDIF}
    {$IFDEF CLR}
      pKernel := clCreateKernel( _prog, _name, out oclError ) ;
    {$ENDIF}

    SetLength( GlobalWorkSizes, 1 ) ;

    cnt := ArgumentCount ;
    SetLength( IsArgumentSet, cnt ) ;
    for i := 0 to cnt - 1 do
      IsArgumentSet[i] := False ;
  end ;


  procedure T_clKernel.doDestroy ;
  begin
    oclError := clReleaseKernel( pKernel ) ;

    inherited ;
  end ;


  function T_clKernel.fget_ArgumentCount
    : Integer ;
  var
    {$IFDEF CLR}
      val   : IntPtr ;
      dummy : IntPtr ;
    {$ENDIF}
    res  : TCL_uint ;
  begin
    Result := -1 ;

    {$IFDEF DCC}
      oclError := clGetKernelInfo( pKernel, T_clN.CL_KERNEL_NUM_ARGS,
                    sizeOf( res ), @res, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( res ) ) ;
      oclError := clGetKernelInfo( pKernel, T_clN.CL_KERNEL_NUM_ARGS,
                    sizeOf( res ), val, dummy ) ;
      res := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
    Result := Integer( res ) ;
  end ;


  function T_clKernel.fget_WorkDimension
    : Integer ;
  begin
    Result := length( GlobalWorkSizes ) ;
  end ;


  procedure T_clKernel.fset_WorkDimension(
    const _val : Integer
  ) ;
  begin
    if ( _val > 0 ) and ( _val < 4 ) then begin
      SetLength( GlobalWorkSizes, _val ) ;
      SetLength(  LocalWorkSizes, _val ) ;
    end ;
  end ;


  procedure T_clKernel.SetArgument(
    const _idx  : TCL_uint ;
    const _size : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) ;
  begin
    oclError := clSetKernelArg( pKernel, _idx, _size, _data ) ;
  end ;


  procedure T_clKernel.SetArgument(
    const _idx  : TCL_uint ;
    const _data : T_clBuffer
  ) ;
  {$IFDEF CLR}
    var
      mem : TCL_mem ;
      buf : IntPtr ;
  {$ENDIF}
  begin
    {$IFDEF DCC}
      SetArgument( _idx, sizeOf( @_data.pBuffer ), @_data.pBuffer ) ;
    {$ENDIF}
    {$IFDEF CLR}
      mem.handle := _data.pBuffer ;
      buf := Marshal.AllocHGlobal( sizeOf( mem ) ) ;
      Marshal.StructureToPtr( mem, buf, False ) ;
      SetArgument( _idx, sizeOf( _data.pBuffer ), buf ) ;
      Marshal.FreeHGlobal( buf ) ;
    {$ENDIF}
  end ;


  procedure T_clKernel.SetArgument(
    const _idx  : TCL_uint ;
    const _data : T_clImage
  ) ;
  begin
    {$IFDEF DCC}
      SetArgument( _idx, sizeOf( @_data.pImage ), @_data.pImage ) ;
    {$ENDIF}
    {$IFDEF CLR}
      SetArgument( _idx, sizeOf( _data.pImage ), _data.pImage ) ;
    {$ENDIF}
  end ;


//==============================================================================
// T_clProgram
//==============================================================================

  constructor T_clProgram.Create(
    const _ctx : PCL_context ;
    const _src : String ;
    const _opt : String
  ) ;
  {$IFDEF DCC}
    var
      src : TCLAnsiString ;
      arr : array of TCLAnsiString ;
      opt : TCLAnsiString ;
  {$ENDIF}
  {$IFDEF CLR}
    var
      arr : PPCLAnsiChar ;
  {$ENDIF}
  begin
    inherited Create ;

    {$IFDEF DCC}
      src := AnsiStringToBytes( _src ) ;
      SetLength( arr, 1 ) ;
      arr[0] := src ;
      pProgram := clCreateProgramWithSource(
                    _ctx, 1, @arr[0], nil, @oclError ) ;

      if not IsStringEmpty( _opt ) then begin
        opt := AnsiStringToBytes( _opt ) ;
        oclError := clBuildProgram( pProgram, 0, nil, @opt[0], nil, nil )
      end
      else
        oclError := clBuildProgram( pProgram, 0, nil, nil, nil, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      SetLength( arr, 1 ) ;
      arr[0] := _src ;
      pProgram := clCreateProgramWithSource(
                    _ctx, 1, arr, nil, out oclError ) ;

      if not IsStringEmpty( _opt ) then
        oclError := clBuildProgram( pProgram, 0, nil, _opt, nil, nil )
      else
        oclError := clBuildProgram( pProgram, 0, nil, nil, nil, nil ) ;
    {$ENDIF}
  end ;


  constructor T_clProgram.Create(
    const _ctx : PCL_context ;
    const _bin : T_clBinaries ;
    const _opt : String
  ) ;
  {$IFDEF DCC}
    var
      opt : TCLAnsiString ;
  {$ENDIF}
  {$IFDEF CLR}
    var
      pdev : PPCL_device_id ;
      psiz : PSize_t ;
      abin : array of IntPtr ;
      gch  : GCHandle ;
      i    : Integer ;
  {$ENDIF}
  begin
    inherited Create ;

    {$IFDEF DCC}
      pProgram := clCreateProgramWithBinary( _ctx, _bin.DeviceCount,
                    @_bin.Devices[0], @_bin.BinarySizes[0], @_bin.Binaries[0],
                    nil, @oclError ) ;

      if not IsStringEmpty( _opt ) then begin
        opt := AnsiStringToBytes( _opt ) ;
        oclError := clBuildProgram( pProgram, _bin.DeviceCount,
                      @_bin.Devices[0], @opt[0], nil, nil ) ;
      end
      else
        oclError := clBuildProgram( pProgram, _bin.DeviceCount,
                      @_bin.Devices[0], nil, nil, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      SetLength( pdev, _bin.DeviceCount ) ;
      SetLength( psiz, _bin.DeviceCount ) ;
      SetLength( abin, _bin.DeviceCount ) ;
      for i := 0 to _bin.DeviceCount - 1 do begin
        pdev[i].handle := _bin.Devices[i] ;
        psiz[i] := IntPtr( _bin.BinarySizes[i] ) ;
        abin[i] := Marshal.AllocHGlobal( _bin.BinarySizes[i] ) ;
        Marshal.Copy( _bin.Binaries[i], 0, abin[i], _bin.BinarySizes[i] ) ;
      end ;
      gch := GCHandle.Alloc( abin, GCHandleType.Pinned ) ;

      pProgram := clCreateProgramWithBinary( _ctx, _bin.DeviceCount,
                    pdev, psiz, gch.AddrOfPinnedObject,
                    nil, out oclError ) ;

      for i := 0 to _bin.DeviceCount - 1 do
        Marshal.FreeHGlobal( abin[i] ) ;

      gch.Free ;

      if not IsStringEmpty( _opt ) then
        oclError := clBuildProgram( pProgram, _bin.DeviceCount,
                      pdev, _opt, nil, nil )
      else
        oclError := clBuildProgram( pProgram, _bin.DeviceCount,
                      pdev, nil, nil, nil ) ;
    {$ENDIF}
  end ;


  procedure T_clProgram.doDestroy ;
  begin
    oclError := clReleaseProgram( pProgram ) ;

    inherited ;
  end ;


  function T_clProgram.CreateKernel(
    const _name : String
  ) : T_clKernel ;
  begin
    Result := T_clKernel.Create( pProgram, _name ) ;
  end ;


//==============================================================================
// T_clCommandQueue
//==============================================================================

  constructor T_clCommandQueue.Create(
    const _dev  : PCL_device_id ;
    const _ctx  : PCL_context ;
    const _ooo  : Boolean ;
    const _prof : Boolean
  ) ;
  var
    p : TCL_command_queue_properties ;
  begin
    inherited Create ;

    p := 0 ;
    if _ooo then
      p := p or T_clN.CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE ;
    if _prof then
      p := p or T_clN.CL_QUEUE_PROFILING_ENABLE ;

    {$IFDEF DCC}
      pCommandQueue := clCreateCommandQueue( _ctx, _dev, p, @oclError ) ;
    {$ENDIF}
    {$IFDEF CLR}
      pCommandQueue := clCreateCommandQueue( _ctx, _dev, p, out oclError ) ;
    {$ENDIF}
  end ;


  procedure T_clCommandQueue.doDestroy ;
  begin
    oclError := clReleaseCommandQueue( pCommandQueue ) ;

    inherited ;
  end ;


  procedure T_clCommandQueue.ReadBuffer(
    const _buf  : T_clBuffer ;
    const _size : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) ;
  {$IFDEF CLR}
    var
      ev : TCL_event ;
  {$ENDIF}
  begin
    {$IFDEF DCC}
      oclError := clEnqueueReadBuffer(
        pCommandQueue, _buf.pBuffer, T_clN.CL_TRUE, 0, _size, _data, 0, nil, nil
      ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clEnqueueReadBuffer(
        pCommandQueue, _buf.pBuffer, T_clN.CL_TRUE, 0, _size, _data, 0, nil, ev
      ) ;
    {$ENDIF}

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    oclError := clFinish( pCommandQueue ) ;
  end ;


  procedure T_clCommandQueue.WriteBuffer(
    const _buf  : T_clBuffer ;
    const _size : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) ;
  {$IFDEF CLR}
    var
      ev : TCL_event ;
  {$ENDIF}
  begin
    {$IFDEF DCC}
      oclError := clEnqueueWriteBuffer(
        pCommandQueue, _buf.pBuffer, T_clN.CL_TRUE, 0, _size, _data, 0, nil, nil
      ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clEnqueueWriteBuffer(
        pCommandQueue, _buf.pBuffer, T_clN.CL_TRUE, 0, _size, _data, 0, nil, ev
      ) ;
    {$ENDIF}

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    oclError := clFinish( pCommandQueue ) ;
  end ;


  procedure T_clCommandQueue.ReadImage2D(
    const _image  : T_clImage2D ;
    const _width  : TSize_t ;
    const _height : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) ;
  var
    origin : array [0..2] of TSize_t ;
    region : array [0..2] of TSize_t ;
    {$IFDEF CLR}
      op : array of IntPtr ;
      rp : array of IntPtr ;
      ev : TCL_event ;
      i  : Integer ;
    {$ENDIF}
  begin
    origin[0] := 0 ;
    origin[1] := 0 ;
    origin[2] := 0 ;

    region[0] := _width ;
    region[1] := _height ;
    region[2] := 1 ; // 2D

    {$IFDEF DCC}
      oclError := clEnqueueReadImage(
        pCommandQueue, _image.pImage2D, T_clN.CL_TRUE,
        @origin, @region, 0, 0, _data, 0, nil, nil
      ) ;
    {$ENDIF}
    {$IFDEF CLR}
      SetLength( op, 3 ) ;
      for i := 0 to 2 do
        Marshal.WriteInt32( op[i], origin[i] ) ;
      SetLength( op, 3 ) ;
      for i := 0 to 2 do
        Marshal.WriteInt32( rp[i], region[i] ) ;
      oclError := clEnqueueReadImage(
        pCommandQueue, _image.pImage2D, T_clN.CL_TRUE,
        op, rp, 0, 0, _data, 0, nil, out ev
      ) ;
    {$ENDIF}

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    oclError := clFinish( pCommandQueue ) ;
  end ;


  procedure T_clCommandQueue.WriteImage2D(
    const _image  : T_clImage2D ;
    const _width  : TSize_t ;
    const _height : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) ;
  var
    origin : array [0..2] of TSize_t ;
    region : array [0..2] of TSize_t ;
    {$IFDEF CLR}
      op : array of IntPtr ;
      rp : array of IntPtr ;
      ev : TCL_event ;
      i  : Integer ;
    {$ENDIF}
  begin
    origin[0] := 0 ;
    origin[1] := 0 ;
    origin[2] := 0 ;

    region[0] := _width ;
    region[1] := _height ;
    region[2] := 1 ; // 2D

    {$IFDEF DCC}
      oclError := clEnqueueWriteImage(
        pCommandQueue, _image.pImage2D, T_clN.CL_TRUE,
        @origin, @region, 0, 0, _data, 0, nil, nil
      ) ;
    {$ENDIF}
    {$IFDEF CLR}
      SetLength( op, 3 ) ;
      for i := 0 to 2 do
        Marshal.WriteInt32( op[i], origin[i] ) ;
      SetLength( op, 3 ) ;
      for i := 0 to 2 do
        Marshal.WriteInt32( rp[i], region[i] ) ;
      oclError := clEnqueueWriteImage(
        pCommandQueue, _image.pImage2D, T_clN.CL_TRUE,
        op, rp, 0, 0, _data, 0, nil, out ev
      ) ;
    {$ENDIF}

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    oclError := clFinish( pCommandQueue ) ;
  end ;


  procedure T_clCommandQueue.Enqueue(
    const _kern : T_clKernel
  ) ;
  var
    {$IFDEF CLR}
      len : Integer;
      gws : array of IntPtr ;
      lws : array of IntPtr ;
      ev  : TCL_event ;
    {$ENDIF}
    b   : Boolean ;
    i   : Integer ;
  begin
    b := True ;
    for i := 0 to _kern.WorkDimension - 1 do begin
      b := b and ( _kern.LocalWorkSizes[i] <> 0 ) ;
      if not b then
        break ;
      b := b and ( _kern.GlobalWorkSizes[i] mod _kern.LocalWorkSizes[i] = 0 ) ;
      if not b then
        break ;
    end ;

    {$IFDEF DCC}
      if b then
        oclError := clEnqueueNDRangeKernel( pCommandQueue, _kern.pKernel,
                      _kern.WorkDimension, nil, @_kern.GlobalWorkSizes[0],
                      @_kern.LocalWorkSizes[0], 0, nil, nil )
      else
        oclError := clEnqueueNDRangeKernel( pCommandQueue, _kern.pKernel,
                      _kern.WorkDimension, nil, @_kern.GlobalWorkSizes[0],
                      nil, 0, nil, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      len := _kern.WorkDimension ;
      SetLength( gws, len ) ;
      if b then
        SetLength( lws, len ) ;

      for i := 0 to len - 1 do begin
        gws[i] := IntPtr( _kern.GlobalWorkSizes[i] ) ;
        if b then
          lws[i] := IntPtr( _kern.LocalWorkSizes[i] ) ;
      end ;

      if b then
        oclError := clEnqueueNDRangeKernel( pCommandQueue, _kern.pKernel,
                      len, nil, gws, lws, 0, nil, ev )
      else
        oclError := clEnqueueNDRangeKernel( pCommandQueue, _kern.pKernel,
                      len, nil, gws, nil, 0, nil, ev ) ;
    {$ENDIF}
  end ;


  procedure T_clCommandQueue.Execute ;
  begin
    oclError := clFinish( pCommandQueue ) ;
  end ;



//==============================================================================
// T_clContext
//==============================================================================

  constructor T_clContext.Create(
    _id : PCL_device_id
  ) ;
  {$IFDEF CLR}
    var
      dev   : TCL_device_id ;
      pdev  : PPCL_device_id ;
      val   : IntPtr ;
      dummy : IntPtr ;
  {$ENDIF}
  begin
    inherited Create ;

    {$IFDEF DCC}
      pContext := clCreateContext( nil, 1, @_id, nil, nil, @oclError ) ;
      oclError := clGetContextInfo( pContext, T_clN.CL_CONTEXT_NUM_DEVICES,
                    SizeOf( FDeviceCount ), @FDeviceCount, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      dev.handle := _id ;
      SetLength( pdev, 1 ) ;
      pdev[0] := dev ;
      pContext := clCreateContext( nil, 1, pdev, nil, nil, out oclError ) ;
      val := Marshal.AllocHGlobal( sizeOf( FDeviceCount ) ) ;
      oclError := clGetContextInfo( pContext, T_clN.CL_CONTEXT_NUM_DEVICES,
                    sizeOf( FDeviceCount ), val, dummy ) ;
      FDeviceCount := Marshal.ReadInt32( val ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
  end ;


  procedure T_clContext.doDestroy ;
  begin
    oclError := clReleaseContext( pContext ) ;
    inherited ;
  end ;


//==============================================================================
// T_clDevice
//==============================================================================

  constructor T_clDevice.Create(
    const _id : PCL_device_id
  ) ;
  var
    {$IFDEF DCC}
      tmp   : TCLAnsiString ;
    {$ENDIF}
    {$IFDEF CLR}
      val   : IntPtr ;
      dummy : IntPtr ;
    {$ENDIF}
    dt  : TCL_device_type ;
    siz : TSize_t ;
    str : String ;
    b   : TCL_bool ;
    i   : Integer ;
    k   : Integer ;
    c   : Integer ;
  begin
    inherited Create ;

    pDevice := _id ;

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_ADDRESS_BITS,
        sizeOf( FAddressBits ),
        @FAddressBits, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FAddressBits ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_ADDRESS_BITS,
        sizeOf( FAddressBits ),
        val, dummy ) ;
      FAddressBits := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_AVAILABLE,
        sizeOf( b ),
        @b, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( b ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_AVAILABLE,
        sizeOf( b ),
        val, dummy ) ;
      b := TCL_bool( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
    if b <> 0 then FIsAvailable := True ;

    // CL_DEVICE_BUILT_IN_KERNELS

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_COMPILER_AVAILABLE,
        sizeOf( b ),
        @b, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( b ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_COMPILER_AVAILABLE,
        sizeOf( b ),
        val, dummy ) ;
      b := TCL_bool( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
    if b <> 0 then FIsCompilerAvailable := True ;

    // CL_DEVICE_DOUBLE_FP_CONFIG

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_ENDIAN_LITTLE,
        sizeOf( b ),
        @b, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( b ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_ENDIAN_LITTLE,
        sizeOf( b ),
        val, dummy ) ;
      b := TCL_bool( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
    if b <> 0 then FIsEndianLittle := True ;

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_ERROR_CORRECTION_SUPPORT,
        sizeOf( b ),
        @b, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( b ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_ERROR_CORRECTION_SUPPORT,
        sizeOf( b ),
        val, dummy ) ;
      b := TCL_bool( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
    if b <> 0 then FIsErrorCorrectionSupport := True ;

    // CL_DEVICE_EXECUTION_CAPABILITIES

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_EXTENSIONS,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_EXTENSIONS,
                    siz, @tmp[0], nil ) ;
      str := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_EXTENSIONS,
                    0, nil, val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_EXTENSIONS,
                    siz, val, dummy ) ;
      str := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    FExtensionCount := 0 ;
    for i := StringFirst to StringLast( str ) do begin
      if i > StringFirst then begin
        if ( str[i] = ' ' ) and ( str[i-1] <> ' ' ) then
          inc( FExtensionCount ) ;
      end ;
    end ;
    inc( FExtensionCount ) ;

    k := StringFirst ;
    c := 0 ;
    SetLength( FExtensions, FExtensionCount ) ;
    for i := StringFirst to StringLast( str ) do begin
      if i > StringFirst then begin
        if ( str[i] = ' ' ) and ( str[i-1] <> ' ' ) then begin
          FExtensions[c] := Copy( str, k, i - k ) ;
          k := i+1 ;
          inc( c ) ;
        end ;
      end ;
    end ;
    FExtensions[c] := Copy( str, k, length( str ) - k + StringFirst ) ;

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,
        sizeOf( FGlobalMemCacheSize ),
        @FGlobalMemCacheSize, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FGlobalMemCacheSize ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,
        sizeOf( FGlobalMemCacheSize ),
        val, dummy ) ;
      FGlobalMemCacheSize := TCL_ulong( Marshal.ReadInt64( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // CL_DEVICE_GLOBAL_MEM_CACHE_TYPE

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE,
        sizeOf( FGlobalMemCacheLineSize ),
        @FGlobalMemCacheLineSize, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FGlobalMemCacheLineSize ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE,
        sizeOf( FGlobalMemCacheLineSize ),
        val, dummy ) ;
      FGlobalMemCacheLineSize := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_GLOBAL_MEM_SIZE,
        sizeOf( FGlobalMemSize ),
        @FGlobalMemSize, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FGlobalMemSize ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_GLOBAL_MEM_SIZE,
        sizeOf( FGlobalMemSize ),
        val, dummy ) ;
      FGlobalMemSize := TCL_ulong( Marshal.ReadInt64( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // CL_DEVICE_HALF_FP_CONFIG

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_HOST_UNIFIED_MEMORY,
        sizeOf( b ),
        @b, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( b ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_HOST_UNIFIED_MEMORY,
        sizeOf( b ),
        val, dummy ) ;
      b := TCL_bool( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
    if b <> 0 then FIsHostUnifiedMemory := True ;

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE_SUPPORT,
        sizeOf( b ), @b, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( b ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE_SUPPORT,
        sizeOf( b ), val, dummy ) ;
      b := TCL_bool( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
    if b <> 0 then FIsImageSupport := True ;

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE2D_MAX_HEIGHT,
        sizeOf( FImage2DMaxHeight ),
        @FImage2DMaxHeight, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FImage2DMaxHeight ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE2D_MAX_HEIGHT,
        sizeOf( FImage2DMaxHeight ),
        val, dummy ) ;
      FImage2DMaxHeight := TSize_t( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE2D_MAX_WIDTH,
        sizeOf( FImage2DMaxWidth ),
        @FImage2DMaxWidth, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FImage2DMaxWidth ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE2D_MAX_WIDTH,
        sizeOf( FImage2DMaxWidth ),
        val, dummy ) ;
      FImage2DMaxWidth := TSize_t( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE3D_MAX_DEPTH,
        sizeOf( FImage3DMaxDepth ),
        @FImage3DMaxDepth, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FImage3DMaxDepth ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE3D_MAX_DEPTH,
        sizeOf( FImage3DMaxDepth ),
        val, dummy ) ;
      FImage3DMaxDepth := TSize_t( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE3D_MAX_HEIGHT,
        sizeOf( FImage3DMaxHeight ),
        @FImage3DMaxHeight, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FImage3DMaxHeight ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE3D_MAX_HEIGHT,
        sizeOf( FImage3DMaxHeight ),
        val, dummy ) ;
      FImage3DMaxHeight := TSize_t( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE3D_MAX_WIDTH,
        sizeOf( FImage3DMaxWidth ),
        @FImage3DMaxWidth, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FImage3DMaxWidth ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_IMAGE3D_MAX_WIDTH,
        sizeOf( FImage3DMaxWidth ),
        val, dummy ) ;
      FImage3DMaxWidth := TSize_t( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // CL_DEVICE_IMAGE_MAX_BUFFER_SIZE

    // CL_DEVICE_IMAGE_MAX_ARRAY_SIZE

    // CL_DEVICE_LINKER_AVAILABLE

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_LOCAL_MEM_SIZE,
        sizeOf( FLocalMemSize ),
        @FLocalMemSize, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FLocalMemSize ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_LOCAL_MEM_SIZE,
        sizeOf( FLocalMemSize ),
        val, dummy ) ;
      FLocalMemSize := TCL_ulong( Marshal.ReadInt64( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // CL_DEVICE_LOCAL_MEM_TYPE

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_CLOCK_FREQUENCY,
        sizeOf( FMaxClockFrequency ),
        @FMaxClockFrequency, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxClockFrequency ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_CLOCK_FREQUENCY,
        sizeOf( FMaxClockFrequency ),
        val, dummy ) ;
      FMaxClockFrequency := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_COMPUTE_UNITS,
        sizeOf( FMaxComputeUnits ),
        @FMaxComputeUnits, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxComputeUnits ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_COMPUTE_UNITS,
        sizeOf( FMaxComputeUnits ),
        val, dummy ) ;
      FMaxComputeUnits := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_CONSTANT_ARGS,
        sizeOf( FMaxConstantArgs ),
        @FMaxConstantArgs, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxConstantArgs ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_CONSTANT_ARGS,
        sizeOf( FMaxConstantArgs ),
        val, dummy ) ;
      FMaxConstantArgs := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE,
        sizeOf( FMaxConstantBufferSize ),
        @FMaxConstantBufferSize, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxConstantBufferSize ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE,
        sizeOf( FMaxConstantBufferSize ),
        val, dummy ) ;
      FMaxConstantBufferSize := TCL_ulong( Marshal.ReadInt64( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_MEM_ALLOC_SIZE,
        sizeOf( FMaxMemAllocSize ),
        @FMaxMemAllocSize, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxMemAllocSize ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_MEM_ALLOC_SIZE,
        sizeOf( FMaxMemAllocSize ),
        val, dummy ) ;
      FMaxMemAllocSize := TCL_ulong( Marshal.ReadInt64( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_PARAMETER_SIZE,
        sizeOf( FMaxParameterSize ),
        @FMaxParameterSize, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxParameterSize ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_PARAMETER_SIZE,
        sizeOf( FMaxParameterSize ),
        val, dummy ) ;
      FMaxParameterSize := TSize_t( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_READ_IMAGE_ARGS,
        sizeOf( FMaxReadImageArgs ),
        @FMaxReadImageArgs, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxReadImageArgs ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_READ_IMAGE_ARGS,
        sizeOf( FMaxReadImageArgs ),
        val, dummy ) ;
      FMaxReadImageArgs := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_SAMPLERS,
        sizeOf( FMaxSamplers ),
        @FMaxSamplers, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxSamplers ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_SAMPLERS,
        sizeOf( FMaxSamplers ),
        val, dummy ) ;
      FMaxSamplers := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_WORK_GROUP_SIZE,
        sizeOf( FMaxWorkGroupSize ),
        @FMaxWorkGroupSize, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxWorkGroupSize ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_WORK_GROUP_SIZE,
        sizeOf( FMaxWorkGroupSize ),
        val, dummy ) ;
      FMaxWorkGroupSize := TSize_t( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS,
        sizeOf( FMaxWorkItemDimensions ),
        @FMaxWorkItemDimensions, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxWorkItemDimensions ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS,
        sizeOf( FMaxWorkItemDimensions ),
        val, dummy ) ;
      FMaxWorkItemDimensions := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // CL_DEVICE_MAX_WORK_ITEM_SIZES

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_WRITE_IMAGE_ARGS,
        sizeOf( FMaxWriteImageArgs ),
        @FMaxWriteImageArgs, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMaxWriteImageArgs ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MAX_WRITE_IMAGE_ARGS,
        sizeOf( FMaxWriteImageArgs ),
        val, dummy ) ;
      FMaxWriteImageArgs := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MEM_BASE_ADDR_ALIGN,
        sizeOf( FMemBaseAddrAlign ),
        @FMemBaseAddrAlign, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMemBaseAddrAlign ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MEM_BASE_ADDR_ALIGN,
        sizeOf( FMemBaseAddrAlign ),
        val, dummy ) ;
      FMemBaseAddrAlign := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE,
        sizeOf( FMinDataTypeAlignSize ),
        @FMinDataTypeAlignSize, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FMinDataTypeAlignSize ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE,
        sizeOf( FMinDataTypeAlignSize ),
        val, dummy ) ;
      FMinDataTypeAlignSize := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_NAME,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_NAME,
                    siz, @tmp[0], nil ) ;
      FName := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_NAME,
                    0, nil, out val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_NAME,
                    siz, val, dummy ) ;
      FName := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR,
        sizeOf( FNativeVectorWidthChar ),
        @FNativeVectorWidthChar, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FNativeVectorWidthChar ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR,
        sizeOf( FNativeVectorWidthChar ),
        val, dummy ) ;
      FNativeVectorWidthChar := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT,
        sizeOf( FNativeVectorWidthShort ),
        @FNativeVectorWidthShort, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FNativeVectorWidthShort ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT,
        sizeOf( FNativeVectorWidthShort ),
        val, dummy ) ;
      FNativeVectorWidthShort := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_INT,
        sizeOf( FNativeVectorWidthInt ),
        @FNativeVectorWidthInt, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FNativeVectorWidthInt ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_INT,
        sizeOf( FNativeVectorWidthInt ),
        val, dummy ) ;
      FNativeVectorWidthInt := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG,
        sizeOf( FNativeVectorWidthLong ),
        @FNativeVectorWidthLong, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FNativeVectorWidthLong ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG,
        sizeOf( FNativeVectorWidthLong ),
        val, dummy ) ;
      FNativeVectorWidthLong := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT,
        sizeOf( FNativeVectorWidthFloat ),
        @FNativeVectorWidthFloat, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FNativeVectorWidthFloat ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT,
        sizeOf( FNativeVectorWidthFloat ),
        val, dummy ) ;
      FNativeVectorWidthFloat := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE,
        sizeOf( FNativeVectorWidthDouble ),
        @FNativeVectorWidthDouble, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FNativeVectorWidthDouble ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE,
        sizeOf( FNativeVectorWidthDouble ),
        val, dummy ) ;
      FNativeVectorWidthDouble := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF,
        sizeOf( FNativeVectorWidthHalf ),
        @FNativeVectorWidthHalf, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FNativeVectorWidthHalf ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF,
        sizeOf( FNativeVectorWidthHalf ),
        val, dummy ) ;
      FNativeVectorWidthHalf := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_OPENCL_C_VERSION,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_OPENCL_C_VERSION,
                    siz, @tmp[0], nil ) ;
      FOpenCLCVersion := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_OPENCL_C_VERSION,
                    0, nil, val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_OPENCL_C_VERSION,
                    siz, val, dummy ) ;
      FOpenCLCVersion := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // CL_DEVICE_PARENT_DEVICE

    // CL_DEVICE_PARTITION_MAX_SUB_DEVICES

    // CL_DEVICE_PARTITION_PROPERTIES

    // CL_DEVICE_PARTITION_AFFINITY_DOMAIN

    // CL_DEVICE_PARTITION_TYPE

    // CL_DEVICE_PLATFORM

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,
        sizeOf( FPreferredVectorWidthChar ),
        @FPreferredVectorWidthChar, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FPreferredVectorWidthChar ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,
        sizeOf( FPreferredVectorWidthChar ),
        val, dummy ) ;
      FPreferredVectorWidthChar := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT,
        sizeOf( FPreferredVectorWidthShort ),
        @FPreferredVectorWidthShort, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FPreferredVectorWidthShort ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT,
        sizeOf( FPreferredVectorWidthShort ),
        val, dummy ) ;
      FPreferredVectorWidthShort := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT,
        sizeOf( FPreferredVectorWidthInt ),
        @FPreferredVectorWidthInt, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FPreferredVectorWidthInt ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT,
        sizeOf( FPreferredVectorWidthInt ),
        val, dummy ) ;
      FPreferredVectorWidthInt := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG,
        sizeOf( FPreferredVectorWidthLong ),
        @FPreferredVectorWidthLong, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FPreferredVectorWidthLong ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG,
        sizeOf( FPreferredVectorWidthLong ),
        val, dummy ) ;
      FPreferredVectorWidthLong := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,
        sizeOf( FPreferredVectorWidthFloat ),
        @FPreferredVectorWidthFloat, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FPreferredVectorWidthFloat ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,
        sizeOf( FPreferredVectorWidthFloat ),
        val, dummy ) ;
      FPreferredVectorWidthFloat := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
        sizeOf( FPreferredVectorWidthDouble ),
        @FPreferredVectorWidthDouble, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FPreferredVectorWidthDouble ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
        sizeOf( FPreferredVectorWidthDouble ),
        val, dummy ) ;
      FPreferredVectorWidthDouble := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF,
        sizeOf( FPreferredVectorWidthHalf ),
        @FPreferredVectorWidthHalf, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FPreferredVectorWidthHalf ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF,
        sizeOf( FPreferredVectorWidthHalf ),
        val, dummy ) ;
      FPreferredVectorWidthHalf := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // CL_DEVICE_PRINTF_BUFFER_SIZE

    // CL_DEVICE_PREFERRED_INTEROP_USER_SYNC

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_PROFILE,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_PROFILE,
                    siz, @tmp[0], nil ) ;
      FProfile := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_PROFILE,
                    0, nil, val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_PROFILE,
                    siz, val, dummy ) ;
      FProfile := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PROFILING_TIMER_RESOLUTION,
        sizeOf( FProfilingTimerResolution ),
        @FProfilingTimerResolution, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FProfilingTimerResolution ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_PROFILING_TIMER_RESOLUTION,
        sizeOf( FProfilingTimerResolution ),
        val, dummy ) ;
      FProfilingTimerResolution := TSize_t( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // CL_DEVICE_QUEUE_PROPERTIES

    // CL_DEVICE_REFERENCE_COUNT

    // CL_DEVICE_SINGLE_FP_CONFIG

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_TYPE,
                    sizeOf( dt ), @dt, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( dt ) ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_TYPE,
                    sizeOf( dt ), val, dummy ) ;
      dt := TCL_device_type( Marshal.ReadInt64( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
    FIsCPU := False ;
    FIsGPU := False ;
    FIsAccelerator := False ;
    FIsDefault := False ;
    if ( dt and T_clN.CL_DEVICE_TYPE_GPU ) <> 0 then
      FIsGPU := True ;
    if ( dt and T_clN.CL_DEVICE_TYPE_CPU ) <> 0 then
      FIsCPU := True ;
    if ( dt and T_clN.CL_DEVICE_TYPE_ACCELERATOR ) <> 0 then
      FIsAccelerator := True ;
    if ( dt and T_clN.CL_DEVICE_TYPE_DEFAULT ) <> 0 then
      FIsDefault := True ;

    if FIsCPU then
      FDeviceType := TGIS_OpenCLDeviceType.CPU
    else
    if FIsGPU then
      FDeviceType := TGIS_OpenCLDeviceType.GPU
    else
    if FIsAccelerator then
      FDeviceType := TGIS_OpenCLDeviceType.Accelerator
    else
      FDeviceType := TGIS_OpenCLDeviceType.CPU ;

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_VENDOR,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_VENDOR,
                    siz, @tmp[0], nil ) ;
      FVendor := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_VENDOR,
                    0, nil, out val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_VENDOR,
                    siz, val, dummy ) ;
      FVendor := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    if length( FVendor ) > 0 then begin
      str := UpperCase( FVendor ) ;
      if str[StringFirst] = 'N' then
        FDeviceVendor := T_clDeviceVendor.Nvidia
      else
      if str[StringFirst] = 'A' then
        FDeviceVendor := T_clDeviceVendor.AMD
      else
      if str[StringFirst] = 'I' then
        FDeviceVendor := T_clDeviceVendor.Intel
      else
        FDeviceVendor := T_clDeviceVendor.Unknown ;
    end
    else
      FDeviceVendor := T_clDeviceVendor.Unknown ;

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_VENDOR_ID,
        sizeOf( FVendorId ),
        @FVendorId, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( FVendorId ) ) ;
      oclError := clGetDeviceInfo( pDevice,
        T_clN.CL_DEVICE_VENDOR_ID,
        sizeOf( FVendorId ),
        val, dummy ) ;
      FVendorId := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_VERSION,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_VERSION,
                    siz, @tmp[0], nil ) ;
      FVersion := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_VERSION,
                    0, nil, out val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DEVICE_VERSION,
                    siz, val, dummy ) ;
      FVersion := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    {$IFDEF DCC}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DRIVER_VERSION,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DRIVER_VERSION,
                    siz, @tmp[0], nil ) ;
      FDriverVersion := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DRIVER_VERSION,
                    0, nil, val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetDeviceInfo( pDevice, T_clN.CL_DRIVER_VERSION,
                    siz, val, dummy ) ;
      FDriverVersion := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    FContext := T_clContext.Create( pDevice ) ;
  end ;


  procedure T_clDevice.doDestroy ;
  begin
    SetLength( FExtensions, 0 ) ;
    FreeObject( FContext ) ;

    inherited ;
  end ;


  function T_clDevice.fget_Extensions(
    const _idx : Integer
  ) : String ;
  begin
    Result := FExtensions[_idx] ;
  end ;


  function T_clDevice.CreateCommandQueue(
    const _ooo  : Boolean ;
    const _prof : Boolean
  ) : T_clCommandQueue ;
  begin
    Result := T_clCommandQueue.Create(
                pDevice, FContext.pContext, _ooo, _prof
              ) ;
  end ;


  function T_clDevice.CreateProgram(
    const _src : String ;
    const _opt : String
  ) : T_clProgram ;
  begin
    Result := T_clProgram.Create( FContext.pContext, _src, _opt ) ;
  end ;


  function T_clDevice.CreateProgram(
    const _bin : T_clBinaries ;
    const _opt : String
  ) : T_clProgram ;
  begin
    Result := T_clProgram.Create( FContext.pContext, _bin, _opt ) ;
  end ;


  function T_clDevice.CreateBuffer(
    const _flags : TGIS_OpenCLMemoryFlags ;
    const _size  : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer ;
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr ;
    {$ENDIF}
    const _keridx : Integer ;
    const _argidx : Integer
  ) : T_clBuffer ;
  begin
    Result := T_clBuffer.Create(
      FContext.pContext, _flags, _size, _data, _keridx, _argidx
    ) ;
  end ;


  function T_clDevice.CreateImage(
    const _flags  : TGIS_OpenCLMemoryFlags ;
    const _format : PCL_image_format ;
    const _desc   : PCL_image_desc ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) : T_clImage ;
  begin
    Result := T_clImage.Create( FContext.pContext, _flags, _format,
      _desc, _data
    ) ;
  end ;


  function T_clDevice.CreateImage2D(
    const _flags  : TGIS_OpenCLMemoryFlags ;
    const _format : PCL_image_format ;
    const _width  : TSize_t ;
    const _height : TSize_t ;
    const _rpitch : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) : T_clImage2D ;
  begin
    Result := T_clImage2D.Create( FContext.pContext, _flags, _format,
      _width, _height, _rpitch, _data
    ) ;
  end ;


  function T_clDevice.CreateImage3D(
    const _flags  : TGIS_OpenCLMemoryFlags ;
    const _format : PCL_image_format ;
    const _width  : TSize_t ;
    const _height : TSize_t ;
    const _depth  : TSize_t ;
    const _rpitch : TSize_t ;
    const _spitch : TSize_t ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : IntPtr
    {$ENDIF}
  ) : T_clImage3D ;
  begin
    Result := T_clImage3D.Create( FContext.pContext, _flags, _format,
      _width, _height, _depth, _rpitch, _spitch, _data
    ) ;
  end ;


//==============================================================================
// T_clPlatform
//==============================================================================

  constructor T_clPlatform.Create(
    const _id : PCL_platform_id
  ) ;
  var
    {$IFDEF DCC}
      dev   : array of PCL_device_id ;
      tmp   : TCLAnsiString ;
    {$ENDIF}
    {$IFDEF CLR}
      dev    : PPCL_device_id ;
      val    : IntPtr ;
      dummy  : IntPtr ;
      dummy1 : TCL_uint ;
    {$ENDIF}
    siz : TSize_t ;
    str : String ;
    i   : Integer ;
    k   : Integer ;
    c   : Integer ;
  begin
    inherited Create ;

    pPlatform := _id ;

    // profile
    {$IFDEF DCC}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_PROFILE,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_PROFILE,
                    siz, @tmp[0], nil ) ;
      FProfile := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_PROFILE,
                    0, nil, val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_PROFILE,
                    siz, val, dummy ) ;
      FProfile := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // version
    {$IFDEF DCC}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_VERSION,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_VERSION,
                    siz, @tmp[0], nil ) ;
      FVersion := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_VERSION,
                    0, nil, val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_VERSION,
                    siz, val, dummy ) ;
      FVersion := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // name
    {$IFDEF DCC}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_NAME,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_NAME,
                    siz, @tmp[0], nil ) ;
      FName := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_NAME,
                    0, nil, val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_NAME,
                    siz, val, dummy ) ;
      FName := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // vendor
    {$IFDEF DCC}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_VENDOR,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_VENDOR,
                    siz, @tmp[0], nil ) ;
      FVendor := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_VENDOR,
                    0, nil, val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_VENDOR,
                    siz, val, dummy ) ;
      FVendor := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    // extensions
    {$IFDEF DCC}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_EXTENSIONS,
                    0, nil, @siz ) ;
      SetLength( tmp, siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_EXTENSIONS,
                    siz, @tmp[0], nil ) ;
      str := ConvertAnsiString( tmp ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_EXTENSIONS,
                    0, nil, val ) ;
      siz := TSize_t( val ) ;
      val := Marshal.AllocHGlobal( siz ) ;
      oclError := clGetPlatformInfo( pPlatform, T_clN.CL_PLATFORM_EXTENSIONS,
                    siz, val, dummy ) ;
      str := TCLAnsiString( Marshal.PtrToStringAnsi( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}

    FExtensionCount := 0 ;
    for i := StringFirst to StringLast( str ) do begin
      if i > StringFirst then begin
        if ( str[i] = ' ' ) and ( str[i-1] <> ' ' ) then
          inc( FExtensionCount ) ;
      end ;
    end ;
    inc( FExtensionCount ) ;

    k := StringFirst ;
    c := 0 ;
    SetLength( FExtensions, FExtensionCount ) ;
    for i := StringFirst to StringLast( str ) do begin
      if i > StringFirst then begin
        if ( str[i] = ' ' ) and ( str[i-1] <> ' ' ) then begin
          FExtensions[c] := Copy( str, k, i - k ) ;
          k := i+1 ;
          inc( c ) ;
        end ;
      end ;
    end ;
    FExtensions[c] := Copy( str, k, length( str ) - k + StringFirst ) ;

    {$IFDEF DCC}
      oclError := clGetDeviceIDs( pPlatform, T_clN.CL_DEVICE_TYPE_ALL,
                    0, nil, @FDeviceCount ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetDeviceIDs( pPlatform, T_clN.CL_DEVICE_TYPE_ALL,
                    0, nil, out FDeviceCount ) ;
    {$ENDIF}

    if FDeviceCount > 0 then begin
      SetLength( dev, FDeviceCount ) ;
      SetLength( FDevices, FDeviceCount ) ;

      {$IFDEF DCC}
        oclError := clGetDeviceIDs( pPlatform, T_clN.CL_DEVICE_TYPE_ALL,
                      FDeviceCount, @dev[0], nil ) ;

        for i := 0 to FDeviceCount - 1 do
          FDevices[i] := T_clDevice.Create( dev[i] ) ;
      {$ENDIF}
      {$IFDEF CLR}
        oclError := clGetDeviceIDs( pPlatform, T_clN.CL_DEVICE_TYPE_ALL,
                      FDeviceCount, dev, out dummy1 ) ;

        for i := 0 to FDeviceCount - 1 do
          FDevices[i] := T_clDevice.Create( dev[i].handle ) ;
      {$ENDIF}

      SetLength( dev, 0 ) ;
    end ;

  end ;


  procedure T_clPlatform.doDestroy ;
  var
    i : Integer ;
  begin
    SetLength( FExtensions, 0 ) ;

    if FDeviceCount > 0 then begin
      for i := 0 to FDeviceCount - 1 do
        FreeObject( FDevices[i] ) ;
    end ;

    SetLength( FDevices, 0 ) ;

    inherited ;
  end ;


  function T_clPlatform.fget_Devices(
    const _idx : Integer
  ) : T_clDevice ;
  begin
    Result := FDevices[_idx] ;
  end ;


  function T_clPlatform.fget_Extensions(
    const _idx : Integer
  ) : String ;
  begin
    Result := FExtensions[_idx] ;
  end ;


//==============================================================================
// T_clPlatform
//==============================================================================

  constructor T_clPlatforms.Create ;
  var
    {$IFDEF DCC}
      pplt  : array of PCL_platform_id ;
    {$ENDIF}
    {$IFDEF CLR}
      pplt  : PPCL_platform_id ;
      dummy : TCL_uint ;
    {$ENDIF}
    i   : Integer ;
  begin
    inherited ;

    {$IFDEF DCC}
      oclError := clGetPlatformIDs( 0, nil, @FPlatformCount ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetPlatformIDs( 0, nil, FPlatformCount ) ;
    {$ENDIF}

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    if FPlatformCount = 0 then
      exit ;

    SetLength( pplt, FPlatformCount ) ;
    SetLength( FPlatforms, FPlatformCount ) ;
    {$IFDEF DCC}
      oclError := clGetPlatformIDs( FPlatformCount, @pplt[0], nil ) ;

      for i := 0 to FPlatformCount - 1 do
        FPlatforms[i] := T_clPlatform.Create( pplt[i] ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oclError := clGetPlatformIDs( FPlatformCount, pplt, out dummy ) ;

      for i := 0 to FPlatformCount - 1 do
        FPlatforms[i] := T_clPlatform.Create( pplt[i].handle ) ;
    {$ENDIF}
  end ;


  procedure T_clPlatforms.doDestroy ;
  var
    i : Integer ;
  begin
    if FPlatformCount > 0 then begin
      for i := 0 to FPlatformCount - 1 do
        FreeObject( FPlatforms[i] ) ;
    end ;

    SetLength( FPlatforms, 0 ) ;

    inherited ;
  end ;


  function T_clPlatforms.fget_Platforms(
    const _idx : Integer
  ) : T_clPlatform ;
  begin
    Result := FPlatforms[_idx] ;
  end ;


  function T_clPlatforms.fget_AvailableDeviceCount : Integer ;
  var
    plt : T_clPlatform ;
    dev : T_clDevice ;
    cnt : Integer ;
    i   : Integer ;
    k   : Integer ;
  begin
    cnt := 0 ;
    for i := 0 to FPlatformCount - 1 do begin
      plt := FPlatforms[i] ;
      for k := 0 to plt.DeviceCount - 1 do begin
        dev := plt.Devices[k] ;
        if dev.IsAvailable then
          inc( cnt ) ;
      end ;
    end ;

    Result := cnt ;
  end ;


  function T_clPlatforms.GetBestAvailableDevice : T_clDevice ;
  var
    devs : TList<T_clDevice> ;

    function fill_list(
      const _v : T_clDeviceVendor ;
      const _t : TGIS_OpenCLDeviceType
    ) : Boolean ;
    var
      plt : T_clPlatform ;
      dev : T_clDevice ;
      ii  : Integer ;
      kk  : Integer ;
      rr  : Integer ;
    begin
      devs.Clear ;
      rr := 0 ;
      for ii := 0 to FPlatformCount - 1 do begin
        plt := FPlatforms[ii] ;
        for kk := 0 to plt.DeviceCount - 1 do begin

          dev := plt.Devices[kk] ;

          if not dev.IsAvailable then
            continue ;

          case _t of
            TGIS_OpenCLDeviceType.GPU :
              if not dev.IsGPU then continue ;
            TGIS_OpenCLDeviceType.Accelerator :
              if not dev.IsAccelerator then continue ;
          end ;

          if _v = T_clDeviceVendor.Unknown then begin
            devs.Add( dev ) ;
            inc( rr ) ;
          end
          else begin
            if dev.DeviceVendor = _v then begin
              devs.Add( dev ) ;
              inc( rr ) ;
            end ;
          end ;

        end ;
      end ;
      Result := rr > 0 ;
    end ;

    function pick_best( const _b : Boolean ) : T_clDevice ;
    var
      dev : T_clDevice ;
      wgs : NativeUInt ;
      cf  : Cardinal ;
      ii  : Integer ;
      kk  : Integer ;
    begin
      wgs := 0 ;
      cf := 0 ;
      kk := 0 ;
      for ii := 0 to devs.Count - 1 do begin
        dev := devs[ii] ;
        if _b then begin
          if dev.MaxWorkGroupSize > wgs then begin
            wgs := dev.MaxWorkGroupSize ;
            kk := ii ;
          end ;
        end
        else begin
          if dev.MaxClockFrequency > cf then begin
            cf := dev.MaxClockFrequency ;
            kk := ii ;
          end ;
        end ;
      end ;
      Result := devs[kk] ;
    end ;

  begin
    Result := nil ;
    devs := TList<T_clDevice>.Create ;
    try
      // scan for NVIDIA GPU devices, pick by MaxWorkGroupSize
      if fill_list( T_clDeviceVendor.Nvidia,
           TGIS_OpenCLDeviceType.GPU ) then begin
        Result := pick_best( True ) ;
        exit ;
      end ;

      // scan for AMD GPU devices, pick by MaxWorkGroupSize
      if fill_list( T_clDeviceVendor.AMD,
           TGIS_OpenCLDeviceType.GPU ) then begin
        Result := pick_best( True ) ;
        exit ;
      end ;

      // scan for all accelerators, pick by MaxWorkGroupSize
      if fill_list( T_clDeviceVendor.Unknown,
           TGIS_OpenCLDeviceType.Accelerator ) then begin
        Result := pick_best( True ) ;
        exit ;
      end ;

      // scan for all devices, pick by MaxClockFrequency
      if fill_list( T_clDeviceVendor.Unknown,
           TGIS_OpenCLDeviceType.CPU ) then begin
        Result := pick_best( False ) ;
        exit ;
      end ;
    finally
      FreeObject( devs ) ;
    end ;
  end ;


//==============================================================================
// TGIS_OpenCLDeviceInfo
//==============================================================================

  constructor TGIS_OpenCLDeviceInfo.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_OpenCLDeviceInfo.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_Name
    : String ;
  begin
    Result := T_clDevice( oDevice ).Name ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_Vendor
    : String ;
  begin
    Result := T_clDevice( oDevice ).Vendor ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_Version
    : String ;
  begin
    Result := T_clDevice( oDevice ).Version ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_Profile
    : String ;
  begin
    Result := T_clDevice( oDevice ).Profile ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_OpenCLCVersion
    : String ;
  begin
    Result := T_clDevice( oDevice ).OpenCLCVersion ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_DriverVersion
    : String ;
  begin
    Result := T_clDevice( oDevice ).DriverVersion ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_WorkGroupSize : NativeUInt ;
  begin
    Result := T_clDevice( oDevice ).MaxWorkGroupSize ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_ClockFrequency
    : Cardinal ;
  begin
    Result := T_clDevice( oDevice ).MaxClockFrequency ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_MemoryAllocationSize
    : UInt64 ;
  begin
    Result := T_clDevice( oDevice ).MaxMemAllocSize ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_ComputeUnits
    : Cardinal ;
  begin
    Result := T_clDevice( oDevice ).MaxComputeUnits ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_Available
    : Boolean ;
  begin
    Result := T_clDevice( oDevice ).IsAvailable ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_DeviceType
    : TGIS_OpenCLDeviceType ;
  begin
    Result := T_clDevice( oDevice ).DeviceType ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_ExtensionCount
    : Integer ;
  begin
    Result := T_clDevice( oDevice ).ExtensionCount ;
  end ;


  function TGIS_OpenCLDeviceInfo.fget_Extensions(
    const _idx : Integer
  ) : String ;
  begin
    if ( _idx < 0 ) or ( _idx >= ExtensionCount ) then
      Result := ''
    else
      Result := T_clDevice( oDevice ).Extensions[_idx] ;
  end ;


//==============================================================================
// TGIS_OpenCLPlatformInfo
//==============================================================================

  constructor TGIS_OpenCLPlatformInfo.Create ;
  begin
    inherited ;

    {$IFDEF DCC}
      FDevices := TObjectList<TGIS_OpenCLDeviceInfo>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      FDevices := TList<TGIS_OpenCLDeviceInfo>.Create ;
    {$ENDIF}
  end ;


  procedure TGIS_OpenCLPlatformInfo.doDestroy ;
  begin
    FreeObject( FDevices ) ;

    inherited ;
  end ;


  function TGIS_OpenCLPlatformInfo.fget_Profile
    : String ;
  begin
    Result := T_clPlatform( oPlatform ).Profile ;
  end ;


  function TGIS_OpenCLPlatformInfo.fget_Version
    : String ;
  begin
    Result := T_clPlatform( oPlatform ).Version ;
  end ;


  function TGIS_OpenCLPlatformInfo.fget_Name
    : String ;
  begin
    Result := T_clPlatform( oPlatform ).Name ;
  end ;


  function TGIS_OpenCLPlatformInfo.fget_Vendor
    : String ;
  begin
    Result := T_clPlatform( oPlatform ).Vendor ;
  end ;


  function TGIS_OpenCLPlatformInfo.fget_DeviceCount
    : Cardinal ;
  begin
    Result := T_clPlatform( oPlatform ).DeviceCount ;
  end ;


  function TGIS_OpenCLPlatformInfo.fget_Devices(
    const _idx : Integer
  ) : TGIS_OpenCLDeviceInfo ;
  begin
    if ( _idx < 0 ) or ( _idx >= DeviceCount ) then
      Result := nil
    else
      Result := FDevices[_idx] ;
  end ;


  function TGIS_OpenCLPlatformInfo.fget_ExtensionCount
    : Integer ;
  begin
    Result := T_clPlatform( oPlatform ).ExtensionCount ;
  end ;


  function TGIS_OpenCLPlatformInfo.fget_Extensions(
    const _idx : Integer
  ) : String ;
  begin
    if ( _idx < 0 ) or ( _idx >= ExtensionCount ) then
      Result := ''
    else
      Result := T_clPlatform( oPlatform ).Extensions[_idx] ;
  end ;


//==============================================================================
// TGIS_OpenCLEngine
//==============================================================================

  constructor TGIS_OpenCLEngine.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_OpenCLEngine.doDestroy ;
  begin
    FreeObject( FPlatforms ) ;

    inherited ;
  end ;


  function TGIS_OpenCLEngine.fget_Available
    : Boolean ;
  var
    plt : T_clPlatform ;
    pi  : TGIS_OpenCLPlatformInfo ;
    dev : T_clDevice ;
    di  : TGIS_OpenCLDeviceInfo ;
    i   : Integer ;
    k   : Integer ;
  begin
    Result := False ;

    if GisCUDAEngine.Initialized then
      exit ;

    if oclAvailable then begin
      Result := True ;
      exit ;
    end ;

    loadOpenCL ;
    if not oclAvailable then begin
      freeOpenCL ;
      exit ;
    end ;

    try
      initOpenCL( T_OpenCLVersion.OpenCL_1_0, False ) ;
    except
      freeOpenCL ;
      oclAvailable := False ;
      exit ;
    end ;

    oclPlatforms := T_clPlatforms.Create ;
    if ( oclPlatforms.PlatformCount        = 0 ) or
       ( oclPlatforms.AvailableDeviceCount = 0 ) then begin
      FreeObject( oclPlatforms ) ;
      freeOpenCL ;
      oclAvailable := False ;
      exit ;
    end ;

    {$IFDEF DCC}
      FPlatforms := TObjectList<TGIS_OpenCLPlatformInfo>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      FPlatforms := TList<TGIS_OpenCLPlatformInfo>.Create ;
    {$ENDIF}
    for i := 0 to oclPlatforms.PlatformCount - 1 do begin
      plt := oclPlatforms.Platforms[i] ;
      pi := TGIS_OpenCLPlatformInfo.Create ;
      pi.oPlatform := plt ;
      plt.oPlatformInfo := pi ;
      for k := 0 to pi.DeviceCount - 1 do begin
        dev := plt.Devices[k] ;
        di := TGIS_OpenCLDeviceInfo.Create ;
        di.oDevice := dev ;
        dev.oDeviceInfo := di ;
        pi.FDevices.Add( di ) ;
      end ;
      FPlatforms.Add( pi ) ;
    end ;

    oclDefaultDevice := oclPlatforms.GetBestAvailableDevice.oDeviceInfo ;
    oclActiveDevice := oclDefaultDevice ;

    oclBinaries := T_clBinariesCache.Create ;

    oclInitialized := True ;

    Result := True ;
  end ;


  function TGIS_OpenCLEngine.fget_Initialized
    : Boolean ;
  begin
    Result := oclInitialized ;
  end ;


  function TGIS_OpenCLEngine.fget_Enabled
    : Boolean ;
  begin
    if GisCUDAEngine.Initialized then
      oclEnabled := False ;

    Result := oclEnabled ;
  end ;


  procedure TGIS_OpenCLEngine.fset_Enabled(
    const _val : Boolean
  ) ;
  begin
    if Available then
      oclEnabled := _val ;
  end ;


  function TGIS_OpenCLEngine.fget_PlatformCount
    : Integer ;
  begin
    if assigned( FPlatforms ) then
      Result := FPlatforms.Count
    else
      Result := 0 ;
  end ;


  function TGIS_OpenCLEngine.fget_Platforms(
    const _idx : Integer
  ) : TGIS_OpenCLPlatformInfo ;
  begin
    if not assigned( FPlatforms ) then begin
      Result := nil ;
      exit ;
    end ;

    if ( _idx < 0 ) or ( _idx >= PlatformCount ) then
      Result := nil
    else
      Result := FPlatforms[_idx] ;
  end ;


  function TGIS_OpenCLEngine.fget_ActiveDevice
    : TGIS_OpenCLDeviceInfo ;
  begin
    Result := oclActiveDevice ;
  end ;


  procedure TGIS_OpenCLEngine.fset_ActiveDevice(
    const _val : TGIS_OpenCLDeviceInfo
  ) ;
  begin
    if not assigned( _val ) then begin
      oclActiveDevice := oclDefaultDevice ;
      oclBinaries.Clear ;
      exit ;
    end ;

    if not _val.Available then
      exit ;

    oclActiveDevice := _val ;
    oclBinaries.Clear ;
  end ;


  function GisOpenCLEngine
    : TGIS_OpenCLEngine ;
  begin
    if not assigned( oclEngine ) then
      oclEngine := TGIS_OpenCLEngine.Create ;

    Result := oclEngine ;
  end ;


//==============================================================================
// TGIS_OpenCLProgram
//==============================================================================

  constructor TGIS_OpenCLProgram.Create ;
  begin
    inherited ;

    if not GisOpenCLEngine.Available then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_OPENCL_NOT_AVAILABLE ), '', 0
            ) ;

    {$IFDEF DCC}
      oKernelList := TObjectList<TObject>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      oKernelList := TList<TObject>.Create ;
    {$ENDIF}

    {$IFDEF DCC}
      oBufferList := TObjectList<TObject>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      oBufferList := TList<TObject>.Create ;
    {$ENDIF}

    bPrepared := False ;

    FKernelIndex := -1 ;
    FCacheBinaries := True ;
  end ;


  procedure TGIS_OpenCLProgram.doDestroy ;
  begin
    FreeObject( oKernelList ) ;
    FreeObject( oProgram ) ;
    FreeObject( oCommandQueue ) ;
    FreeObject( oBufferList ) ;

    inherited ;
  end ;


  function TGIS_OpenCLProgram.fget_KernelCount
    : Integer ;
  begin
    Result := oKernelList.Count ;
  end ;


  function TGIS_OpenCLProgram.fget_KernelIndex
    : Integer ;
  begin
    Result := FKernelIndex ;
  end ;


  procedure TGIS_OpenCLProgram.fset_KernelIndex(
    const _val : Integer
  ) ;
  begin
    if ( _val >= 0 ) and ( _val < oKernelList.Count ) then
      FKernelIndex := _val ;
  end ;


  function TGIS_OpenCLProgram.fget_KernelName
    : String ;
  var
    ker : T_clKernel ;
  begin
    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    Result := ker.Name ;
  end ;


  function TGIS_OpenCLProgram.fget_ArgumentCount
    : Integer ;
  var
    {$IFDEF CLR}
      val   : IntPtr ;
      dummy : IntPtr ;
    {$ENDIF}
    kern : T_clKernel ;
    res  : TCL_uint ;
  begin
    Result := -1 ;
    if oKernelList.Count = 0 then
      exit ;

    kern := T_clKernel( oKernelList[0] ) ;
    {$IFDEF DCC}
      oclError := clGetKernelInfo( kern.pKernel, T_clN.CL_KERNEL_NUM_ARGS,
                    sizeOf( res ), @res, nil ) ;
    {$ENDIF}
    {$IFDEF CLR}
      val := Marshal.AllocHGlobal( sizeOf( res ) ) ;
      oclError := clGetKernelInfo( kern.pKernel, T_clN.CL_KERNEL_NUM_ARGS,
                    sizeOf( res ), val, dummy ) ;
      res := TCL_uint( Marshal.ReadInt32( val ) ) ;
      Marshal.FreeHGlobal( val ) ;
    {$ENDIF}
    Result := Integer( res ) ;
  end ;


  function TGIS_OpenCLProgram.fget_WorkDimension
    : Integer ;
  var
    ker : T_clKernel ;
  begin
    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    Result := ker.WorkDimension ;
  end ;


  procedure TGIS_OpenCLProgram.fset_WorkDimension(
    const _val : Integer
  ) ;
  var
    ker : T_clKernel ;
  begin
    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    ker.WorkDimension := _val ;
  end ;


  function TGIS_OpenCLProgram.fget_GlobalWorkSizes(
    const _idx : Integer
  ) : NativeUInt ;
  var
    ker : T_clKernel ;
  begin
    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    Result := ker.GlobalWorkSizes[_idx] ;
  end ;


  procedure TGIS_OpenCLProgram.fset_GlobalWorkSizes(
    const _idx : Integer ;
    const _val : NativeUInt
  ) ;
  var
    ker : T_clKernel ;
  begin
    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    ker.GlobalWorkSizes[_idx] := _val ;
  end ;

  function TGIS_OpenCLProgram.fget_LocalWorkSizes(
    const _idx : Integer
  ) : NativeUInt ;
  var
    ker : T_clKernel ;
  begin
    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    Result := ker.LocalWorkSizes[_idx] ;
  end ;


  procedure TGIS_OpenCLProgram.fset_LocalWorkSizes(
    const _idx : Integer ;
    const _val : NativeUInt
  ) ;
  var
    ker : T_clKernel ;
  begin
    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    ker.LocalWorkSizes[_idx] := _val ;
  end ;


  function TGIS_OpenCLProgram.fget_CacheBinaries
    : Boolean ;
  begin
    Result := FCacheBinaries ;
  end ;


  procedure TGIS_OpenCLProgram.fset_CacheBinaries(
    const _val : Boolean
  ) ;
  var
    bin : T_clBinaries ;
  begin
    if _val = FCacheBinaries then
      exit ;

    if bPrepared then begin
      if _val then begin
        bin := oclBinaries.Find( sKernel ) ;
        if not assigned( bin ) then
          oclBinaries.Add( T_clProgram( oProgram ), sKernel ) ;
      end
      else
        oclBinaries.Delete( sKernel ) ;
    end ;

    FCacheBinaries := _val ;
  end ;


  function TGIS_OpenCLProgram.fget_ErrorCode
    : Integer ;
  begin
    Result := oclError ;
  end ;


  function TGIS_OpenCLProgram.prepare(
    const _source : String ;
    const _kernel : String
  ) : Boolean ;
  var
    bin : T_clBinaries ;
    dev : T_clDevice ;
    ker : T_clKernel ;
  begin
    Result := False ;

    // persistent objects
    dev := T_clDevice( oclActiveDevice.oDevice ) ;
    oDevice := dev ;

    // temporary objects
    oCommandQueue := dev.CreateCommandQueue( False, False ) ;

    if oclError <> T_clN.CL_SUCCESS then begin
      FreeObject( oCommandQueue ) ;
      exit ;
    end ;

    bin := nil ;
    if FCacheBinaries then
      bin := oclBinaries.Find( _kernel ) ;

    if FCacheBinaries and assigned( bin ) then
      oProgram := dev.CreateProgram( bin, '' )
    else
      oProgram := dev.CreateProgram( _source, '' ) ;

    if oclError <> T_clN.CL_SUCCESS then begin
      FreeObject( oCommandQueue ) ;
      FreeObject( oProgram ) ;
      exit ;
    end ;

    ker := T_clProgram( oProgram ).CreateKernel( _kernel ) ;

    if oclError <> T_clN.CL_SUCCESS then begin
      FreeObject( oCommandQueue ) ;
      FreeObject( oProgram ) ;
      FreeObject( ker ) ;
      exit ;
    end ;

    oKernelList.Add( ker ) ;
    FKernelIndex := 0 ;
    sKernel := _kernel ;
    bPrepared := True ;

    if FCacheBinaries and ( not assigned( bin ) ) then
      oclBinaries.Add( T_clProgram( oProgram ), _kernel ) ;

    Result := True ;
  end ;


  function TGIS_OpenCLProgram.enqueue(
    const _kernel : String
  ) : Boolean ;
  var
    ker : T_clKernel ;
  begin
    Result := False ;

    if not assigned( oProgram ) then
      exit ;

    ker := T_clProgram( oProgram ).CreateKernel( _kernel ) ;

    if oclError <> T_clN.CL_SUCCESS then begin
      FreeObject( ker ) ;
      exit ;
    end ;

    oKernelList.Add( ker ) ;
    FKernelIndex := oKernelList.Count - 1 ;

    Result := True ;
  end ;


  function TGIS_OpenCLProgram.ready
    : Boolean ;
  var
    ker : T_clKernel ;
    b   : Boolean ;
    i   : Integer ;
    k   : Integer ;
  begin
    b := bPrepared ;
    for i := 0 to KernelCount - 1 do begin
      ker := T_clKernel( oKernelList[i] ) ;
      for k := 0 to length( ker.IsArgumentSet ) - 1 do
        b := b and ker.IsArgumentSet[k] ;
    end ;

    Result := b ;
  end ;


  function TGIS_OpenCLProgram.LoadFromString(
    const _source : String ;
    const _kernel : String
  ) : Boolean ;
  begin
    Result := prepare( _source, _kernel ) ;
  end ;


  function TGIS_OpenCLProgram.LoadFromFile(
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


  function TGIS_OpenCLProgram.EnqueueKernel(
    const _kernel : String
  ) : Boolean ;
  begin
    Result := enqueue( _kernel ) ;
  end ;


  function TGIS_OpenCLProgram.SetArgument(
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
    {$IFDEF CLR}
      gch : GCHandle ;
    {$ENDIF}
    ker   : T_clKernel ;
  begin
    Result := False ;

    if not bPrepared then
      exit ;

    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    {$IFDEF DCC}
      ker.SetArgument( TCL_uint( _idx ), TSize_t( _size ), _data ) ;
    {$ENDIF}
    {$IFDEF CLR}
      gch := GCHandle.Alloc( _data, GCHandleType.Pinned ) ;
      ker.SetArgument(
        TCL_uint( _idx ), TSize_t( _size ),
        gch.AddrOfPinnedObject
      ) ;
      gch.Free ;
    {$ENDIF}

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    ker.IsArgumentSet[_idx] := True ;

    Result := True ;
  end ;


  function TGIS_OpenCLProgram.SetArgument(
    const _idx    : Integer ;
    const _flags  : TGIS_OpenCLMemoryFlags ;
    const _size   : Integer ;
    {$IFDEF DCC}
      const _data : Pointer
    {$ENDIF}
    {$IFDEF CLR}
      const _data : Object
    {$ENDIF}
  ) : Boolean ;
  var
    {$IFDEF DCC}
      data : Pointer ;
    {$ENDIF}
    {$IFDEF CLR}
      data : Object ;
      gch  : GCHandle ;
    {$ENDIF}
    dev    : T_clDevice ;
    ker    : T_clKernel ;
    cq     : T_clCommandQueue ;
    buf    : T_clBuffer ;
    tmp    : T_clBuffer ;
    ar0    : TBytes ;
    i      : Integer ;
  begin
    Result := False ;

    if not bPrepared then
      exit ;

    dev := T_clDevice( oDevice ) ;
    cq := T_clCommandQueue( oCommandQueue ) ;

    buf := dev.CreateBuffer(
      _flags, TSize_t( _size ), nil, FKernelIndex, _idx
    ) ;
    if _data = nil then begin
      SetLength( ar0, _size ) ;
      {$IFDEF DCC}
        data := @ar0[0] ;
        FillChar( data^, _size, 0 ) ;
      {$ENDIF}
      {$IFDEF CLR}
        Array.Clear( ar0, 0, _size ) ;
        data := ar0 ;
      {$ENDIF}
    end
    else
      data := _data ;

    {$IFDEF DCC}
      cq.WriteBuffer( buf, TSize_t( _size ), data ) ;
    {$ENDIF}
    {$IFDEF CLR}
      gch := GCHandle.Alloc( data, GCHandleType.Pinned ) ;
      cq.WriteBuffer( buf, TSize_t( _size ), gch.AddrOfPinnedObject ) ;
      gch.Free ;
    {$ENDIF}

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    for i := 0 to oBufferList.Count - 1 do begin
      tmp := T_clBuffer( oBufferList[i] ) ;
      if ( tmp.KernelIndex = FKernelIndex ) and
         ( tmp.ArgumentIndex = _idx ) then begin
        {$IFDEF DCC}
          oBufferList.Delete( i ) ;
        {$ENDIF}
        {$IFDEF CLR}
          oBufferList.RemoveAt( i ) ;
        {$ENDIF}
        break ;
      end ;
    end ;
    oBufferList.Add( buf ) ;

    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    ker.SetArgument( TCL_uint( _idx ), buf ) ;

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    ker.IsArgumentSet[_idx] := True ;

    Result := True ;
  end ;


  function TGIS_OpenCLProgram.ShareArgument(
    const _idx    : Integer ;
    const _keridx : Integer ;
    const _argidx : Integer
  ) : Boolean ;
  var
    ker : T_clKernel ;
    buf : T_clBuffer ;
    i   : Integer ;
  begin
    Result := False ;
    buf := nil ;
    for i := 0 to oBufferList.Count - 1 do begin
      buf := T_clBuffer( oBufferList[i] ) ;
      if ( buf.KernelIndex   = _keridx ) and
         ( buf.ArgumentIndex = _argidx ) then
        break ;
    end ;

    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    ker.SetArgument( TCL_uint( _idx ), buf ) ;

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    ker.IsArgumentSet[_idx] := True ;

    Result := True ;
  end ;


  function TGIS_OpenCLProgram.CheckArgument(
    const _idx : Integer
  ) : Boolean ;
  var
    ker : T_clKernel ;
  begin
    ker := T_clKernel( oKernelList[FKernelIndex] ) ;
    Result := ker.IsArgumentSet[_idx] ;
  end ;


  function TGIS_OpenCLProgram.Execute
    : Boolean ;
  var
    ker : T_clKernel ;
    cq  : T_clCommandQueue ;
    i   : Integer ;
  begin
    Result := False ;

    if not ready then
      exit ;

    cq := T_clCommandQueue( oCommandQueue ) ;
    for i := 0 to oKernelList.Count - 1 do begin
      ker := T_clKernel( oKernelList[i] ) ;
      cq.Enqueue( ker ) ;
      if oclError <> T_clN.CL_SUCCESS then
        exit ;
    end ;

    cq.Execute ;

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    Result := True ;
  end ;


  function TGIS_OpenCLProgram.ReadBuffer(
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
    {$IFDEF CLR}
      gch : GCHandle ;
    {$ENDIF}
    cq    : T_clCommandQueue ;
    buf   : T_clBuffer ;
    i     : Integer ;
  begin
    Result := False ;

    cq := T_clCommandQueue( oCommandQueue ) ;

    buf := nil ;
    for i := 0 to oBufferList.Count - 1 do begin
      buf := T_clBuffer( oBufferList[i] ) ;
      if ( buf.KernelIndex = FKernelIndex ) and
         ( buf.ArgumentIndex = _idx ) then
        break ;
    end ;

    if not assigned( buf ) then
      exit ;

    {$IFDEF DCC}
      cq.ReadBuffer( buf, _size, _data ) ;
    {$ENDIF}
    {$IFDEF CLR}
      gch := GCHandle.Alloc( _data, GCHandleType.Pinned ) ;
      cq.ReadBuffer( buf, _size, gch.AddrOfPinnedObject ) ;
      gch.Free ;
    {$ENDIF}

    if oclError <> T_clN.CL_SUCCESS then
      exit ;

    Result := True ;
  end ;


{$IFDEF DCC}
{$IFNDEF GIS_XDK} // but not on XDK due to buggy Intel's drivers

initialization
  // do nothing

finalization
  FreeObject( oclBinaries ) ;
  FreeObject( oclPlatforms ) ;
  FreeObject( oclEngine ) ;
  freeOpenCL ;

{$ENDIF}
{$ENDIF}

//==================================== END =====================================
end.
