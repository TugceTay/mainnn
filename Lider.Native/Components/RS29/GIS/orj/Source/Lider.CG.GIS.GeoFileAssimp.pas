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
  Encapsulation of Open Asset Import Library (assimp) library file access.
}

{$IFDEF DCC}
  unit GisFileAssimp ;
  {$HPPEMIT '#pragma link "GisFileAssimp"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Security,
    System.Runtime.InteropServices,

    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Variants,
    System.Generics.Collections,

    GisRtl,
    GisTypes,
    GisLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  /// <summary>
  ///   Class for reading Assimp library format.
  /// </summary>
  TGIS_FileAssimp = {$IFDEF OXYGENE} public {$ENDIF}
                    class( TGIS_ObjectDisposable )
     {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FInit         : Boolean ;
      FModel        : TObject ;
      FAxisOrder    : Integer ;
      FUseMaterials : Boolean ;
      FScale        : TGIS_SingleVector ;
      FTranslate    : TGIS_SingleVector ;
      FRotate       : TGIS_SingleVector ;
    private
      /// <summary>
      ///   Initialize library.
      /// </summary>
      /// <returns>
      ///    True if file was initialized.
      /// </returns>
      function  initialize : Boolean ;
    protected
      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy ; override;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Open a model.
      /// </summary>
      /// <param name="_path">
      ///   path to a file
      /// </param>
      /// <param name="_flags">
      ///   Open mode defining post processing steps to be executed after
      ///   a successful import.
      ///   0 - default, 1 - Fast, 2 - Quality
      /// </param>
      /// <returns>
      ///    True if file was opened
      /// </returns>
      function  Open ( const _path  : String ;
                       const _flags : Integer
                     ) : Boolean ;

      /// <summary>
      ///   Build meshed on a layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer on which meshes will be built
      /// </param>
      procedure BuildMeshes( const _layer : TGIS_LayerVector
                            ) ;
      /// <summary>
      ///   Get list of supported extensions.
      /// </summary>
      /// <returns>
      ///    mask text (*.ext;...)
      /// </returns>
      function  GetExtensionsList : String ;
    public
      /// <summary>
      ///   If true, a model materials will be used
      /// </summary>
      property UseMaterials : Boolean read  FUseMaterials
                                      write FUseMaterials ;
      /// <summary>
      ///   Axis orientation 0 - XYZ, 1 - XZY
      /// </summary>
      property AxisOrder    : Integer read  FAxisOrder
                                      write FAxisOrder ;
      /// <summary>
      ///   Translate vector to offset coordinates
      /// </summary>
      property Translate    : TGIS_SingleVector read  FTranslate
                                                write FTranslate ;
      /// <summary>
      ///   Scale vector to scale coordinates
      /// </summary>
      property Scale        : TGIS_SingleVector read  FScale
                                                write FScale ;
      /// <summary>
      ///   Rotate vector to rotate coordinates
      /// </summary>
      property Rotate       : TGIS_SingleVector read  FRotate
                                                write FRotate ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.SysUtils,
    System.Math,
    System.Rtti,

    GisClasses,
    GisLogger,
    GisFunctions,
    GisInternals,
    GisTypesUI,
    GisResource ;
{$ENDIF}

type
  {$IFNDEF OXYGENE}
    {#gendoc:hide}
    taiptr = Pointer ;
    {$IFNDEF LEVEL_XE3_RTL}
      {#gendoc:hide}
      MarshaledAString = PAnsiChar;
    {$ENDIF}
    taicmd = taiptr ;
    taiptrArray = array of taiptr ;
  {$ELSE}
    {#gendoc:hide}
    {$IFDEF JAVA}
      taiptr  = com.sun.jna.Pointer ;
    {$ELSE}
      taiptr = IntPtr ;
    {$ENDIF}
    taicmd = String ;
    taiptrArray = taiptr ;
  {$ENDIF}

const
  ASSIMP_DLL_NAME       = 'assimp.dll' ;
  ASSIMP_DLL_NAME_NOEXT = 'assimp' ;

var
  FAssimpLib            : TObject ;
  FAssimpInstanceCount  : Integer = 0 ;

//==============================================================================
// helper functions
//==============================================================================

 {$IFDEF OXYGENE}
   // Convert string buffer to string
   {$IFDEF CLR}
   function asAiString( const _buf : IntPtr ) : String ;
   var
     buf : array of Byte ;
     i   : Integer ;
   begin
     if (_buf = IntPtr.Zero) then begin
       Result := '' ;
       exit ;
     end ;
     i := 0 ;
     while Marshal.ReadByte( IntPtr(Int(_buf)+i) ) <> 0 do
       inc(i);
     if (i = 0) then begin
       Result := '' ;
       exit ;
     end ;
     buf := new Byte[i] ;
     Marshal.Copy( _buf, buf, 0, i ) ;
     Result := TEncoding.UTF8.GetString( buf, 0, i ) ;
     if not assigned( Result ) then
       Result := '' ;
   end ;
   {$ENDIF}
   {$IFDEF JAVA}
     function asAiString( const _buf : String ) : String ;
     begin
       Result := _buf ;
     end ;

     function asAiString( const _buf : taiptr ) : String ;
     begin
       Result := _buf.getString(0) ;
     end ;
   {$ENDIF}
 {$ELSE}
   // Convert string buffer to string
   function asAiString( const _buf : PAnsiChar ) : String ;
   begin
     Result := String( UTF8String( PAnsiChar( _buf ) ) ) ;
   end ;
 {$ENDIF}

 {$IFDEF OXYGENE}
  function toaiAPI( const _str : String ) : String ;
  begin
    Result := _str  ;
  end ;
 {$ELSE}
  function toaiAPI( const _str : String ) : MarshaledAString ;
  begin
    Result := MarshaledAString( UTF8Encode(_str) ) ;
  end ;
 {$ENDIF}

type
  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiString =  record
     Length : Cardinal ;
     {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=1023)]{$ENDIF}
     Data   : array [0..1023] of Byte ;
  end ;

//==============================================================================
// Assimp API wrappers
//==============================================================================

  {$IFDEF JAVA}
    IAssimpLibrary = interface (com.sun.jna.Library)
        function  aiImportFile( _file   : String ;
                                _flags  : Integer
                             ) : taiptr ;
        procedure aiReleaseImport( _instance : taiptr )  ;
        function  aiGetErrorString : taiptr ;
        procedure aiGetExtensionList( var _str : T_aiString ) ;
    end ;
  {$ENDIF}

  // Assimp library helper.
  T_AssimpLib = {$IFDEF OXYGENE} static {$ENDIF}class
    private
      {$IFDEF JAVA}
        dllHandle : IAssimpLibrary ;
      {$ELSE}
        dllHandle : THandle ;
      {$ENDIF}
      libraryLoaded : Boolean ;
    public
      {$IFDEF CLR}
        [ SuppressUnmanagedCodeSecurity,
          DllImport( ASSIMP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function aiImportFile( [&In, MarshalAs(UnmanagedType.LPStr)]
                                _file   : String ;
                                _flags  : Integer
                             ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( ASSIMP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure aiReleaseImport( _scene : IntPtr )  ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( ASSIMP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function aiGetErrorString : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( ASSIMP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure aiGetExtensionList( var _str : T_aiString )  ; external ;
      {$ENDIF}
      {$IFDEF DCC}
        aiImportFile            : function  ( const _file   : PAnsiChar ;
                                              const _flags  : Integer
                                            ) : taiptr ; cdecl ;
        aiReleaseImport         : procedure ( const _scene : taiptr
                                            ) ; cdecl ;
        aiGetErrorString        : function : PAnsiChar ; cdecl ;
        aiGetExtensionList      : procedure ( var _str : T_aiString
                                            ) ; cdecl ;
      {$ENDIF}
      {$IFDEF JAVA}
        function  aiImportFile( _file   : String ;
                                _flags  : Integer
                             ) : taiptr ;
        procedure aiReleaseImport( _instance : taiptr )  ;
        function  aiGetErrorString : taiptr ;
        procedure aiGetExtensionList( var _str : T_aiString ) ;
      {$ENDIF}
    public
      constructor Create ;

      {$IFNDEF OXYGENE}
        destructor Destroy ; override ;
      {$ENDIF}

      // Load a dll.
      // _dllPath  path to a library
      function  LoadDLL     ( const _dllPath : String
                            ) : Boolean ;
  end ;

//=============================================================================
// Unmanaged defines
//=============================================================================

  T_aiPostProcessSteps = {$IFDEF OXYGENE} static {$ENDIF} class
    const
      CalcTangentSpace          = $1 ;
      JoinIdenticalVertices     = $2 ;
      MakeLeftHanded            = $4 ;
      Triangulate               = $8 ;
      RemoveComponent           = $10 ;
      GenNormals                = $20 ;
      GenSmoothNormals          = $40 ;
      SplitLargeMeshes          = $80 ;
      PreTransformVertices      = $100 ;
      LimitBoneWeights          = $200 ;
      ValidateDataStructure     = $400 ;
      ImproveCacheLocality      = $800 ;
      RemoveRedundantMaterials  = $1000 ;
      FixInfacingNormals        = $2000 ;
      PopulateArmatureData      = $4000 ;
      SortByPType               = $8000 ;
      FindDegenerates           = $10000 ;
      FindInvalidData           = $20000 ;
      GenUVCoords               = $40000 ;
      TransformUVCoords         = $80000 ;
      FindInstances             = $100000 ;
      OptimizeMeshes            = $200000 ;
      OptimizeGraph             = $400000 ;
      FlipUVs                   = $800000 ;
      FlipWindingOrder          = $1000000 ;
      SplitByBoneCount          = $2000000 ;
      Debone                    = $4000000 ;
      GlobalScale               = $8000000 ;
      EmbedTextures             = $10000000 ;
      ForceGenNormals           = $20000000 ;
      DropNormals               = $40000000 ;
      GenBoundingBoxes          = $80000000 ;

      Preset_ConvertToLeftHanded =
     //   MakeLeftHanded     or
     //   FlipUVs            or
        FlipWindingOrder   or
        0 ;

      Preset_TargetRealtime_Default =
        CalcTangentSpace      or
        DropNormals           or
        JoinIdenticalVertices or
        Triangulate           or
        GenUVCoords           or
        SortByPType           or
        FindInvalidData       or
        TransformUVCoords     or
        0 ;

      Preset_TargetRealtime_Fast =
        CalcTangentSpace      or
        GenNormals            or
        JoinIdenticalVertices or
        Triangulate           or
        GenUVCoords           or
        SortByPType           or
        0 ;

     Preset_TargetRealtime_Quality =
        CalcTangentSpace              or
        GenSmoothNormals              or
        JoinIdenticalVertices         or
        ImproveCacheLocality          or
        LimitBoneWeights              or
        RemoveRedundantMaterials      or
        SplitLargeMeshes              or
        Triangulate                   or
        GenUVCoords                   or
        SortByPType                   or
        FindDegenerates               or
        FindInvalidData               or
        0 ;

     Preset_TargetRealtime_MaxQuality =
        Preset_TargetRealtime_Quality  or
        FindInstances                  or
        ValidateDataStructure          or
        OptimizeMeshes                 or
        0 ;
  end ;

  T_aiDefines = class
    const
      AI_SLM_DEFAULT_MAX_TRIANGLES      = 1000000 ;
      AI_SLM_DEFAULT_MAX_VERTICES       = 1000000 ;
      AI_LBW_MAX_WEIGHTS                = $04;
      PP_ICL_PTCACHE_SIZE               = 12;
      AI_MAX_FACE_INDICES               = $7fff;
      AI_MAX_BONE_WEIGHTS               = $7fffffff;
      AI_MAX_VERTICES                   = $7fffffff;
      AI_MAX_FACES                      = $7fffffff;
      AI_MAX_NUMBER_OF_COLOR_SETS       = $08;
      AI_MAX_NUMBER_OF_TEXTURECOORDS    = $08;
      AI_SBBC_DEFAULT_MAX_BONES         = 60;
      AI_DEBONE_THRESHOLD               = 1.0;
      AI_MAX_LENGTH                     = 1024;
      AI_DEFAULT_MATERIAL_NAME          = 'DefaultMaterial';
      AI_DEFAULT_TEXTURED_MATERIAL_NAME = 'TexturedDefaultMaterial';
  end ;

//=============================================================================
// Unmanaged raw types
//=============================================================================

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiMatrix4x4 =  record
     a1, a2, a3, a4 : Single ;
     b1, b2, b3, b4 : Single ;
     c1, c2, c3, c4 : Single ;
     d1, d2, d3, d4 : Single ;
  end ;
  {$IFDEF DCC}
  P_aiMatrix4x4 = ^T_aiMatrix4x4 ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiVector2D =  record
     x, y : Single ;
  end ;

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiVector3D =  record
     x, y, z : Single ;
  end ;
  {$IFDEF DCC}
  P_aiVector3D = ^T_aiVector3D ;
  P_aiVector3DArray = array [0..0] of P_aiVector3D ;
  T_aiVector3DArray = array [0..0] of T_aiVector3D ;
  P_TaiVector3DArray = ^T_aiVector3DArray ;
  {$ELSE}
  P_TaiVector3DArray = taiptr ;
  {$ENDIF}

  T_aiMatrix4x4_Utils = class
    public
      class function Indentity : T_aiMatrix4x4 ;
      class function Multiply( const m : T_aiMatrix4x4 ;
                               const n : T_aiMatrix4x4
                              ) : T_aiMatrix4x4 ; overload ;
      class function Multiply( const m : T_aiMatrix4x4 ;
                               const v : T_aiVector3D
                              ) : T_aiVector3D ; overload ;
      class function Rotation( const r : TGIS_SingleVector
                              ) : T_aiMatrix4x4 ;
  end ;

  {$IFDEF DCC}
  P_CardinalArray = ^TGIS_CardinalArray ;
  {$ELSE}
  P_CardinalArray = taiptr ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiNode =  record
    Name            : T_aiString ;
    Transformation  : T_aiMatrix4x4 ;
    Parent          : taiptr ;
    NumChildren     : Cardinal ;
    Children        : taiptrArray ;
    NumMeshes       : Cardinal ;
    Meshes          : taiptr ;
    MetaData        : taiptr ;
  end ;
  {$IFDEF DCC}
  P_aiNode = ^T_aiNode ;
  {$ELSE}
  P_aiNode = T_aiNode ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiScene =  record
    &Flags        : Cardinal ;
    RootNode      : taiptr ;
    NumMeshes     : Cardinal ;
    Meshes        : taiptrArray ;
    NumMaterials  : Cardinal ;
    Materials     : taiptrArray ;
    NumAnimations : Cardinal ;
    Animations    : taiptrArray ;
    NumTextures   : Cardinal ;
    Textures      : taiptrArray ;
    NumLights     : Cardinal ;
    Lights        : taiptrArray ;
    NumCameras    : Cardinal ;
    Cameras       : taiptrArray ;
  end ;
  {$IFDEF OXYGENE}
  P_aiScene = T_aiScene ;
  {$ELSE}
  P_aiScene = ^T_aiScene ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiColor3D =  record
     r, g, b : Single ;
  end ;

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiColor4D =  record
     r, g, b, a : Single ;
  end ;
  {$IFDEF DCC}
  P_aiColor4D = ^T_aiColor4D ;
  T_aiColor4DArray = array[0..0] of T_aiColor4D ;
  P_TaiColor4DArray = ^T_aiColor4DArray ;
  {$ELSE}
  P_TaiColor4DArray = taiptr ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiFace =  record
     NumIndicies  : Cardinal ;
     Indices      : P_CardinalArray ;
  end ;
  {$IFDEF DCC}
  P_aiFace = ^T_aiFace ;
  T_aiFaceArray = array [0..0] of T_aiFace ;
  P_TaiFaceArray = ^T_aiFaceArray ;
  {$ELSE}
  P_aiFace = T_aiFace ;
  P_TaiFaceArray = taiptr ;
  {$ENDIF}

  T_aiPrimitiveTypes = class
    const
      POINT       = $01 ;
      LINE        = $02 ;
      TRIANGLE    = $04 ;
      POLYGON     = $08 ;
  end ;

  T_aiPrimitiveType = (
      POINT,
      LINE,
      TRIANGLE,
      POLYGON
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiVertexWeight =  record
     VertexId : Cardinal ;
     Weight   : Single ;
  end ;
  {$IFDEF DCC}
  P_aiVertexWeight = ^T_aiVertexWeight ;
  T_aiVertexWeightArray = array [0..0] of T_aiVertexWeight ;
  P_aiVertexWeightArray = ^T_aiVertexWeightArray ;
  {$ELSE}
  P_aiVertexWeightArray = taiptr ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiBone =  record
     Name         : T_aiString ;
     NumWeights   : Cardinal ;
     Armature     : taiptr ;
     Node         : taiptr ;
     Weights      : P_aiVertexWeightArray ;
     OffsetMatrix : T_aiMatrix4x4 ;
  end ;
  {$IFDEF DCC}
  P_aiBone = ^T_aiBone ;
  T_aiBoneArray = array [0..0] of P_aiBone ;
  P_aiBoneArray = ^T_aiBoneArray ;
  {$ELSE}
  P_aiBone = T_aiBone ;
  P_aiBoneArray = taiptr ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiMesh =  record
    PrimitiveTypes  : Cardinal ;
    NumVertices     : Cardinal ;
    NumFaces        : Cardinal ;
    Vertices        : taiptr ;
    Normals         : taiptr ;
    Tangents        : taiptr ;
    BiTangents      : taiptr ;
    {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=T_aiDefines.AI_MAX_NUMBER_OF_COLOR_SETS)]{$ENDIF}
    Colors          : array [0..T_aiDefines.AI_MAX_NUMBER_OF_COLOR_SETS-1] of P_TaiColor4DArray ;
    {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=T_aiDefines.AI_MAX_NUMBER_OF_TEXTURECOORDS)]{$ENDIF}
    TextureCoords   : array [0..T_aiDefines.AI_MAX_NUMBER_OF_TEXTURECOORDS-1] of P_TaiVector3DArray ;
    {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=T_aiDefines.AI_MAX_NUMBER_OF_TEXTURECOORDS)]{$ENDIF}
    NumUVComponents : array [0..T_aiDefines.AI_MAX_NUMBER_OF_TEXTURECOORDS-1] of Cardinal ;
    Faces           : P_TaiFaceArray ;
    NumBones        : Cardinal ;
    Bones           : P_aiBoneArray ;
    MaterialIndex   : Cardinal ;
    Name            : T_aiString ;
    NumAniMeshes    : Cardinal ;
    AniMeshes       : taiptr ;
  end ;
  {$IFDEF DCC}
  P_aiMesh = ^T_aiMesh ;
  {$ELSE}
  P_aiMesh = T_aiMesh ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiMaterial =  record
   Properties     : taiptr ;
   NumProperties  : Cardinal ;
   NumAllocated   : Cardinal ;
  end ;
  {$IFDEF DCC}
  P_aiMaterial = ^T_aiMaterial ;
  P_aiMaterialArray = array[0..0] of P_aiMaterial ;
  P_PaiMaterialArray = ^P_aiMaterialArray ;
  {$ELSE}
  P_aiMaterial = T_aiMaterial ;
  {$ENDIF}

  T_aiPropertyTypeInfo = (
     Float,
     &String,
     Integer,
     Buffer
   ) ;

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiMaterialProperty =  record
    Key        : T_aiString ;
    Semantic   : Cardinal ;
    Index      : Cardinal ;
    DataLength : Cardinal ;
    PType      : Cardinal ;
    Data       : taiptr ;
  end ;
  {$IFDEF DCC}
  P_aiMaterialProperty = ^T_aiMaterialProperty ;
  {$ELSE}
  P_aiMaterialProperty = T_aiMaterialProperty ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiTexel =  record
     b, g, r, a : Byte ;
  end ;
  {$IFDEF DCC}
  P_aiTexel = ^T_aiTexel ;
  T_aiTexelArray = array[0..0] of T_aiTexel ;
  P_aiTexelArray = ^T_aiTexelArray ;
  {$ELSE}
  P_aiTexelArray = taiptr ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiTexture =  record
     Width      : Cardinal ;
     Height     : Cardinal ;
     {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=3)]{$ENDIF}
     FormatHint : array [0..3] of Byte ;
     {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=7)]{$ENDIF}
     Gap        : array [0..7] of Byte ;
     Data       : P_aiTexelArray ;
     FileName   : T_aiString ;
  end ;
  {$IFDEF DCC}
  P_aiTexture = ^T_aiTexture ;
  P_aiTextureArray = array [0..0] of P_aiTexture ;
  P_PaiTextureArray = ^P_aiTextureArray ;
  {$ELSE}
  P_aiTexture = T_aiTexture ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_aiLight =  record
    public
      Name                  : T_aiString ;
      LightType             : Cardinal ;
      Position              : T_aiVector3D ;
      Direction             : T_aiVector3D ;
      Up                    : T_aiVector3D ;
      AttenuationConstant   : Single ;
      AttenuationLinear     : Single ;
      AttenuationQuadratic  : Single ;
      ColorDiffuse          : T_aiColor3D ;
      ColorSpecular         : T_aiColor3D ;
      ColorAmbient          : T_aiColor3D ;
      AngleInnerCone        : Single ;
      AngleOuterCone        : Single ;
      Size                  : T_aiVector2D ;
  end ;
  {$IFDEF DCC}
  P_aiLight = ^T_aiLight ;
  {$ELSE}
  P_aiLight = T_aiLight ;
  {$ENDIF}

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_AiMetadata = record
    public
      NumProperties : Cardinal ;
      Keys          : taiptr ;
      Values        : taiptr ;
  end ;
  {$IFDEF DCC}
  P_AiMetadata = ^T_AiMetadata ;
  {$ELSE}
  P_AiMetadata = T_AiMetadata ;
  {$ENDIF}

  T_MetaDataType = (
    dtBool      = 0,
    dtInt       = 1,
    dtUInt64    = 2,
    dtFloat     = 3,
    dtDouble    = 4,
    dtAiString  = 5,
    dtVector3D  = 6,
    dtMetaData  = 7,
    dtMetaMax   = 8
  ) ;

  {$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
  [CLSCompliant(false)]
  {$ENDIF}
  T_AiMetadataEntry = record
    public
      DataType : T_MetaDataType ;
      Data     : taiptr ;
  end ;

//=============================================================================
// Managed types
//=============================================================================

  T_Unmanaged = class( TGIS_ObjectDisposable )
    protected
      function isValidPtr       ( const _ptr : taiptr
                                ) : Boolean ;
      function fromAiString     ( const _aiString : T_aiString
                                ) : String ;
      function fromPtrArray<T>  ( const _ptr   : taiptr ;
                                  const _count : Integer
                                ) : TArray<T> ; overload ;
      function fromPtrArray<M,U>( const _ptr      : taiptr ;
                                  const _count    : Integer ;
                                  const _ptrArray : Boolean
                                ) : TArray<M> ; overload ;
      function marshalStructure<T>( const _ptr    : taiptr
                                  ) : T ;
      function readMemory<T>      ( const _ptr    : taiptr
                                  ): T ;
  end ;

  T_MetadataEntry = class
    public
      DataType : T_MetaDataType ;
      Data     : Variant ;
  end ;

  T_Metadata = class( T_Unmanaged )
    public
      NumProperties : Cardinal ;
      Properties    : TDictionary<String, T_MetadataEntry> ;
    protected
      procedure doDestroy    ; override ;
    public
      constructor Create( const _ptr    : taiptr
                        ) ;
  end ;

  T_ANode = class( T_Unmanaged )
    public
      Name            : String ;
      Transformation  : T_aiMatrix4x4 ;
      Parent          : T_ANode ;
      Children        : TObjectList<T_ANode> ;
      MeshIndices     : TList<Cardinal> ;
      MetaData        : T_Metadata ;
    protected
      procedure doDestroy    ; override ;
    public
      constructor Create( const _ptr    : taiptr
                        ) ;
  end ;

  T_Face = class( T_Unmanaged )
    public
     Indices : TArray<Cardinal> ;
    public
      {$IFDEF CLR}
      constructor Create( const _face    : taiptr
                        ) ;
      {$ELSE}
      constructor Create( const _face    : T_aiFace
                        ) ;
      {$ENDIF}
  end ;

  T_Bone = class( T_Unmanaged )
    public
     Name         : String ;
     Weights      : TList<T_aiVertexWeight> ;
     OffsetMatrix : T_aiMatrix4x4 ;
    protected
      procedure doDestroy    ; override ;
    public
      constructor Create( const _ptr : taiptr ) ;
  end ;

  T_AMesh = class( T_Unmanaged )
    public
      Name              : String ;
      PrimitiveTypes    : set of T_aiPrimitiveType ;
      MaterialIndex     : Integer ;
      Vertices          : TList<T_aiVector3D> ;
      Normals           : TList<T_aiVector3D> ;
      Tangents          : TList<T_aiVector3D> ;
      BiTangents        : TList<T_aiVector3D> ;
      Colors            : array of TList<T_aiColor4D> ;
      TextureCoords     : array of TList<T_aiVector3D> ;
      TextureComponents : array of Cardinal ;
      Faces             : TObjectList<T_Face> ;
      Bones             : TObjectList<T_Bone> ;
      MeshAttachments   : TObjectList<TObject> ;
    private
      function fget_HasNormals       : Boolean ;
      function fget_HasTextureCoords : Boolean ;
      function fget_HasColors        : Boolean ;
    protected
      procedure doDestroy    ; override ;
    public
      constructor Create( const _ptr : taiptr ) ;

      property HasNormals       : Boolean read fget_HasNormals ;
      property HasTextureCoords : Boolean read fget_HasTextureCoords ;
      property HasColors        : Boolean read fget_HasColors ;
  end ;

  T_MaterialProperty = class( T_Unmanaged )
    public
      Name          : String ;
      PropertyType  : T_aiPropertyTypeInfo ;
      TextureType   : Integer ;
      TextureIndex  : Integer ;
      RawDataCount  : Integer ;
      RawData       : TBytes ;
    public
      constructor Create( const _ptr : taiptr ) ;
      function    AsString  : String ;
      function    AsColor   : T_aiColor4D ;
      function    AsFloat   : Single ;
      function    AsInteger : Integer ;
    end ;

  T_Material = class( T_Unmanaged )
    public
      Properties : TDictionary<String, T_MaterialProperty> ;
    protected
      procedure doDestroy    ; override ;
    public
      constructor Create( const _ptr : taiptr ) ;
  end ;

  T_Animation = class( T_Unmanaged )
    public
      constructor Create( const _ptr : taiptr ) ;
  end ;

  T_Texture = class( T_Unmanaged )
    public
      IsCompressed      : Boolean ;
      Width             : Cardinal ;
      Height            : Cardinal ;
      NonCompressedData : array of T_aiTexel ;
      CompressedData    : TBytes ;
      FormatHint        : String ;
      FileName          : String ;
    public
      constructor Create( const _ptr : taiptr ) ;
  end ;

  T_Light = class( T_Unmanaged )
    public
      Name          : String ;
      LightType     : Cardinal ;
      Position      : T_aiVector3D ;
      Direction     : T_aiVector3D ;
      Up            : T_aiVector3D ;
      Size          : T_aiVector2D ;
      ColorDiffuse  : T_aiColor3D ;
      ColorSpecular : T_aiColor3D ;
      ColorAmbient  : T_aiColor3D ;
    public
      constructor Create( const _ptr : taiptr ) ;
  end ;

  T_Camera = class( T_Unmanaged )
    public
      constructor Create( const _ptr : taiptr ) ;
  end ;

  T_Scene = class( T_Unmanaged )
    public
      Path          : String ;
      Flags         : Cardinal ;
      RootNode      : T_ANode ;
      Meshes        : TObjectList<T_AMesh> ;
      Materials     : TObjectList<T_Material> ;
      Animations    : TObjectList<T_Animation> ;
      Textures      : TObjectList<T_Texture> ;
      Lights        : TObjectList<T_Light> ;
      Cameras       : TObjectList<T_Camera> ;
    protected
      procedure doDestroy    ; override ;
    public
      constructor Create( const _ptr : taiptr ) ;
  end ;

  T_AssimpContext = class( T_Unmanaged )
    public
      function  ImportFile( const _file  : String ;
                            const _flags : Integer
                          ) : T_Scene ;
  end ;

  T_AssimpRenderer = class
    private
      oLayer        : TGIS_LayerVector ;
      iAxisOrder    : Integer ;
      bUseMaterials : Boolean ;
      vScale        : TGIS_SingleVector ;
      vTranslate    : TGIS_SingleVector ;
      vRotate       : TGIS_SingleVector ;
    private
      procedure recursiveRender( const _scene : T_Scene ;
                                 const _node  : T_ANode ;
                                   var _mat   : T_aiMatrix4x4
                               ) ;
    public
      procedure Render        ( const _scene : T_Scene ;
                                const _layer : TGIS_LayerVector
                              ) ;
    public
      property UseMaterials : Boolean                     read  bUseMaterials
                                                          write bUseMaterials ;
      property AxisOrder    : Integer                     read  iAxisOrder
                                                          write iAxisOrder ;
      property Translate    : TGIS_SingleVector           read  vTranslate
                                                          write vTranslate ;
      property Scale        : TGIS_SingleVector           read  vScale
                                                          write vScale ;
      property Rotate       : TGIS_SingleVector           read  vRotate
                                                          write vRotate ;
  end ;

//=============================================================================
// T_aiMatrix4x4_Utils
//=============================================================================

  class function T_aiMatrix4x4_Utils.Indentity : T_aiMatrix4x4 ;
  begin
    Result.a1 := 1.0 ;
    Result.a2 := 0 ;
    Result.a3 := 0 ;
    Result.a4 := 0 ;
    Result.b1 := 0 ;
    Result.b2 := 1.0;
    Result.b3 := 0 ;
    Result.b4 := 0 ;
    Result.c1 := 0 ;
    Result.c2 := 0 ;
    Result.c3 := 1.0 ;
    Result.c4 := 0 ;
    Result.d1 := 0 ;
    Result.d2 := 0 ;
    Result.d3 := 0 ;
    Result.d4 := 1.0 ;
  end ;

  class function T_aiMatrix4x4_Utils.Multiply(
    const m : T_aiMatrix4x4 ;
    const n : T_aiMatrix4x4
  ) : T_aiMatrix4x4 ;
  begin
    Result.a1 := m.a1 * n.a1 + m.b1 * n.a2 + m.c1 * n.a3 + m.d1 * n.a4 ;
    Result.a2 := m.a2 * n.a1 + m.b2 * n.a2 + m.c2 * n.a3 + m.d2 * n.a4 ;
    Result.a3 := m.a3 * n.a1 + m.b3 * n.a2 + m.c3 * n.a3 + m.d3 * n.a4 ;
    Result.a4 := m.a4 * n.a1 + m.b4 * n.a2 + m.c4 * n.a3 + m.d4 * n.a4 ;
    Result.b1 := m.a1 * n.b1 + m.b1 * n.b2 + m.c1 * n.b3 + m.d1 * n.b4 ;
    Result.b2 := m.a2 * n.b1 + m.b2 * n.b2 + m.c2 * n.b3 + m.d2 * n.b4 ;
    Result.b3 := m.a3 * n.b1 + m.b3 * n.b2 + m.c3 * n.b3 + m.d3 * n.b4 ;
    Result.b4 := m.a4 * n.b1 + m.b4 * n.b2 + m.c4 * n.b3 + m.d4 * n.b4 ;
    Result.c1 := m.a1 * n.c1 + m.b1 * n.c2 + m.c1 * n.c3 + m.d1 * n.c4 ;
    Result.c2 := m.a2 * n.c1 + m.b2 * n.c2 + m.c2 * n.c3 + m.d2 * n.c4 ;
    Result.c3 := m.a3 * n.c1 + m.b3 * n.c2 + m.c3 * n.c3 + m.d3 * n.c4 ;
    Result.c4 := m.a4 * n.c1 + m.b4 * n.c2 + m.c4 * n.c3 + m.d4 * n.c4 ;
    Result.d1 := m.a1 * n.d1 + m.b1 * n.d2 + m.c1 * n.d3 + m.d1 * n.d4 ;
    Result.d2 := m.a2 * n.d1 + m.b2 * n.d2 + m.c2 * n.d3 + m.d2 * n.d4 ;
    Result.d3 := m.a3 * n.d1 + m.b3 * n.d2 + m.c3 * n.d3 + m.d3 * n.d4 ;
    Result.d4 := m.a4 * n.d1 + m.b4 * n.d2 + m.c4 * n.d3 + m.d4 * n.d4 ;
  end ;

  class function T_aiMatrix4x4_Utils.Multiply(
    const m : T_aiMatrix4x4 ;
    const v : T_aiVector3D
  ) : T_aiVector3D ;
  begin
    Result.x := m.a1 * v.x + m.a2 * v.y + m.a3 * v.z + m.a4 ;
    Result.y := m.b1 * v.x + m.b2 * v.y + m.b3 * v.z + m.b4 ;
    Result.z := m.c1 * v.x + m.c2 * v.y + m.c3 * v.z + m.c4 ;
  end ;

  class function T_aiMatrix4x4_Utils.Rotation(
    const r : TGIS_SingleVector
  ) : T_aiMatrix4x4 ;
  var
    rx, ry, rz : T_aiMatrix4x4 ;
  begin
    rx := Indentity ;
    rx.b2 := Cos(r.X) ;
    rx.c3 := rx.b2 ;
    rx.c2 := Sin(r.X) ;
    rx.b3 := -(rx.c2) ;

    ry := Indentity ;
    ry.a1 := Cos(r.Y);
    ry.c3 := ry.a1 ;
    ry.a3 := Sin(r.Y) ;
    ry.c1 := -(ry.a3);

    rz := Indentity ;
    rz.a1 := Cos(r.Z) ;
    rz.b2 := rz.a1 ;
    rz.b1 := Sin(r.Z) ;
    rz.a2 := -(rz.b1) ;

    Result := Indentity ;
    Result := Multiply( Result, ry ) ;
    Result := Multiply( Result, rx ) ;
    Result := Multiply( Result, rz ) ;
  end ;

//=============================================================================
// T_Unmanaged
//=============================================================================

  function T_Unmanaged.fromAiString(
    const _aiString : T_aiString
  ) : String ;
   var
     buf : TBytes ;
  begin
    Result := '' ;
    if _aiString.Length = 0 then exit ;

    SetLength( buf, _aiString.Length ) ;
    {$IFDEF DCC}
      Move( _aiString.Data[0], buf[0], _aiString.Length ) ;
      Result := TEncoding.UTF8.GetString( buf, 0, _aiString.Length ) ;
    {$ENDIF}
    {$IFDEF CLR}
      Buffer.BlockCopy( _aiString.Data, 0, buf, 0, _aiString.Length ) ;
      Result := TEncoding.UTF8.GetString( buf, 0, _aiString.Length ) ;
      if not assigned( Result ) then
        Result := '' ;
    {$ENDIF}
  end ;

  function T_Unmanaged.fromPtrArray<T>(
    const _ptr    : taiptr ;
    const _count  : Integer
  ) : TArray<T> ;
  {$IFDEF CLR}
    var
      i    : Integer ;
      cptr : taiptr ;
      off  : Integer ;
  {$ENDIF}
  begin
    if isValidPtr( _ptr ) then begin
      SetLength( Result, _count ) ;
      {$IFDEF DCC}
        Move( _ptr^, Result[0], _count * sizeOf(T) ) ;
      {$ENDIF}
      {$IFDEF CLR}
        off := Marshal.SizeOf(typeOf(T)) ;
        for i := 0 to _count-1 do begin
          cptr := new IntPtr( _ptr.ToInt64 + i*off ) ;
          Result[i] := marshalStructure<T>(cptr) ;
        end ;
      {$ENDIF}
    end
    else
      Result := nil ;
  end ;

  function T_Unmanaged.fromPtrArray<M, U>(
    const _ptr      : taiptr ;
    const _count    : Integer ;
    const _ptrArray : Boolean
  ) : TArray<M> ;
    {$IFDEF DCC}
    var
      i     : Integer ;
      parr  : taiptr ;
      oval  : TValue ;
      pval  : TValue ;
      ctx   : TRttiContext ;
      rType : TRttiType ;
      arr   : TArray<U> ;
      arrp  : TArray<taiptr> ;
    {$ENDIF}
    {$IFDEF CLR}
    var
      i    : Integer ;
      cptr : taiptr ;
      off  : Integer ;
    {$ENDIF}
  begin
    if isValidPtr( _ptr ) then begin
      SetLength( Result, _count ) ;

      {$IFDEF DCC}
        ctx := TRttiContext.Create;
        rType := ctx.GetType(TypeInfo(M)) ;

        if not _ptrArray then begin
          SetLength( arr, _count ) ;
          Move( _ptr^, arr[0], _count * sizeOf(U) ) ;
        end
        else begin
          SetLength( arrp, _count ) ;
          Move( _ptr^, arrp[0], _count * sizeOf(taiptr) ) ;
        end ;

        for i := 0 to _count-1 do begin
          if not _ptrArray then begin
            pval := TValue.From<U>(arr[i]) ;
          end
          else begin
            parr := arrp[i] ;
            pval := TValue.From<taiptr>(parr) ;
          end ;
          oval := rType.GetMethod('Create').Invoke(rType.AsInstance.MetaclassType, [pval]);
          Result[i] := oval.AsType<M> ;
        end ;
      {$ENDIF}
      {$IFDEF CLR}
        if not _ptrArray then begin
          SetLength( Result, _count ) ;
          off := Marshal.SizeOf(typeOf(U)) ;
          for i := 0 to _count-1 do begin
            cptr := new IntPtr( _ptr.ToInt64 + i*off ) ;
            Result[i] := M( Activator.CreateInstance( typeOf(M), [cptr] ) );
          end ;
        end
        else begin
          SetLength( Result, _count ) ;
          for i := 0 to _count-1 do begin
            cptr := new IntPtr( _ptr.ToInt64 + i*IntPtr.Size ) ;
            Result[i] := M( Activator.CreateInstance( typeOf(M), [Marshal.ReadIntPtr( cptr )] ) ) ;
          end ;
        end ;
      {$ENDIF}
    end
    else
      Result := nil ;
  end ;

  function T_Unmanaged.marshalStructure<T>(
    const _ptr    : taiptr
  ) : T ;
  begin
    {$IFDEF CLR}
      Result := T(Marshal.PtrToStructure( _ptr, typeOf(T) ))
    {$ENDIF}
    {$IFDEF DCC}
      Result := T(_ptr^) ;
    {$ENDIF}
  end ;

  function T_Unmanaged.isValidPtr(
    const _ptr : taiptr
  ) : Boolean ;
  begin
    {$IFDEF CLR}
      Result := _ptr <> IntPtr.Zero ;
    {$ENDIF}
    {$IFDEF JAVA}
      Result := _ptr <> nil ;
    {$ENDIF}
    {$IFDEF DCC}
      Result := AssignedPtr( _ptr ) ;
    {$ENDIF}
  end ;

  function T_Unmanaged.readMemory<T>(
    const _ptr    : taiptr
  ) : T ;
  begin
    {$IFDEF DCC}
    Result := T(_ptr^) ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := T(Marshal.PtrToStructure( _ptr, typeOf(T) )) ;
    {$ENDIF}
  end ;

//=============================================================================
// T_Metadata
//=============================================================================

  constructor T_Metadata.Create(
    const _ptr    : taiptr
  ) ;
  var
    pmeta   : P_AiMetadata ;
    i       : Integer ;
    entries : TArray<T_AiMetadataEntry> ;
    ekeys   : TArray<T_aiString> ;
    ientry  : T_AiMetadataEntry ;
    key     : String ;
    edata   : T_MetadataEntry ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    {$IFDEF OXYGENE}
      pmeta := marshalStructure<T_AiMetadata>(_ptr)  ;
    {$ELSE}
      pmeta := _ptr ;
    {$ENDIF}

    NumProperties := pmeta.NumProperties ;
    Properties    := nil ;
    if NumProperties = 0 then exit ;
    
    ekeys   := fromPtrArray<T_aiString>( pmeta.Keys, pmeta.NumProperties ) ;
    entries := fromPtrArray<T_AiMetadataEntry>( pmeta.Values, pmeta.NumProperties ) ;

    Properties := TDictionary<String, T_MetadataEntry>.Create ;

    for i := 0 to pmeta.NumProperties-1 do begin
      ientry := entries[i] ;
      key    := fromAiString( ekeys[i] ) ;
      edata  := T_MetadataEntry.Create ;
      edata.DataType := ientry.DataType ;
      if not isValidPtr( ientry.Data ) then
        edata.Data := Unassigned
      else begin
        case ientry.DataType of
          T_MetaDataType.dtBool     :
            edata.Data := readMemory<Boolean>(ientry.Data);
          T_MetaDataType.dtInt      :
            edata.Data := readMemory<Integer>(ientry.Data);
          T_MetaDataType.dtUInt64   :
            edata.Data := readMemory<UInt64>(ientry.Data);
          T_MetaDataType.dtFloat    :
            edata.Data := readMemory<Single>(ientry.Data);
          T_MetaDataType.dtAiString   :
            edata.Data := fromAiString(readMemory<T_aiString>(ientry.Data));
          T_MetaDataType.dtVector3D :
            //edata.Data := readMemory<T_aiVector3D>(ientry.Data);
        end ;
      end ;
      Properties.AddOrSetValue( key, edata ) ;
    end ;
  end ;

  procedure T_Metadata.doDestroy ;
  {$IFDEF DCC}
  var
    kv  : TPair<String, T_MetadataEntry> ;
  {$ENDIF}
  begin
    if assigned( Properties ) then begin
      for kv in Properties do
        FreeObjectNotNil( kv.Value ) ;

      FreeObject( Properties ) ;
    end ;

    inherited ;
  end ;

//=============================================================================
// T_Node
//=============================================================================

  constructor T_ANode.Create(
    const _ptr    : taiptr
  ) ;
  var
    pnode : P_aiNode ;
    i     : Integer ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    {$IFDEF OXYGENE}
      pnode := marshalStructure<T_aiNode>(_ptr)  ;
    {$ELSE}
      pnode := _ptr ;
    {$ENDIF}

    Name := fromAiString( pnode.Name ) ;
    Transformation := pnode.Transformation ;
    Parent      := nil ;
    MeshIndices := nil ;
    Children    := nil ;

    if ( pnode.NumChildren > 0 ) and isValidPtr( pnode.Children ) then begin
      Children := TObjectList<T_ANode>.Create ;
      Children.AddRange( fromPtrArray<T_ANode, T_aiNode>( pnode.Children, pnode.NumChildren, True ) ) ;
      for i := 0 to Children.Count-1 do
        Children[i].Parent := Self ;
    end ;

    if ( pnode.NumMeshes > 0 ) and isValidPtr( pnode.Meshes ) then begin
      MeshIndices := TList<Cardinal>.Create ;
      MeshIndices.AddRange( fromPtrArray<Cardinal>( pnode.Meshes, pnode.NumMeshes ) ) ;
    end ;

    if isValidPtr( pnode.MetaData )  then
      MetaData := T_Metadata.Create( pnode.MetaData ) ;
  end ;

  procedure T_ANode.doDestroy ;
  begin
    FreeObject( Children    )  ;
    FreeObject( MeshIndices )  ;
    FreeObject( MetaData    )  ;

    inherited ;

  end ;

//=============================================================================
// T_Face
//=============================================================================

  constructor T_Face.Create(
    {$IFDEF CLR}
    const _face : taiptr
    {$ELSE}
    const _face : T_aiFace
    {$ENDIF}
  ) ;
  {$IFDEF CLR}
    var
      pface : P_aiFace ;
  {$ENDIF}
  begin
    inherited Create ;

    {$IFDEF CLR}
      pface := marshalStructure<T_aiFace>(_face) ;
      if pface.NumIndicies > 0 then
        Indices := fromPtrArray<Cardinal>( pface.Indices, pface.NumIndicies ) ;
    {$ELSE}
      if _face.NumIndicies > 0 then
        Indices := fromPtrArray<Cardinal>( _face.Indices, _face.NumIndicies ) ;
    {$ENDIF}
  end ;

//=============================================================================
// T_Bone
//=============================================================================

  constructor T_Bone.Create(
    const _ptr : taiptr
  ) ;
  var
    pbone : P_aiBone ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    {$IFDEF OXYGENE}
      pbone := marshalStructure<T_aiBone>(_ptr)  ;
    {$ELSE}
      pbone := _ptr ;
    {$ENDIF}

    Weights := nil ;
    Name    := fromAiString( pbone.Name ) ;

    if ( pbone.NumWeights > 0 ) and isValidPtr( pbone.Weights ) then begin
      Weights := TList<T_aiVertexWeight>.Create ;
      Weights.AddRange( fromPtrArray<T_aiVertexWeight>(pbone.Weights, pbone.NumWeights ) ) ;
    end ;
    OffsetMatrix := pbone.OffsetMatrix ;
  end ;

  procedure T_Bone.doDestroy ;
  begin
    FreeObject( Weights ) ;
    inherited ;
  end ;

//=============================================================================
// T_Mesh
//=============================================================================

  constructor T_AMesh.Create(
    const _ptr : taiptr
  ) ;
  var
    pmesh  : P_aiMesh ;
    i, j   : Integer ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    {$IFDEF OXYGENE}
      pmesh := marshalStructure<T_aiMesh>(_ptr)  ;
    {$ELSE}
      pmesh := _ptr ;
    {$ENDIF}

    Name := fromAiString( pmesh.Name ) ;
    MaterialIndex := pmesh.MaterialIndex ;

    PrimitiveTypes := [] ;
    if (pmesh.PrimitiveTypes and T_aiPrimitiveTypes.POINT) = T_aiPrimitiveTypes.POINT then
      PrimitiveTypes := PrimitiveTypes + [T_aiPrimitiveType.POINT] ;
    if (pmesh.PrimitiveTypes and T_aiPrimitiveTypes.LINE) = T_aiPrimitiveTypes.LINE then
      PrimitiveTypes := PrimitiveTypes + [T_aiPrimitiveType.LINE] ;
    if (pmesh.PrimitiveTypes and T_aiPrimitiveTypes.TRIANGLE) = T_aiPrimitiveTypes.TRIANGLE then
      PrimitiveTypes := PrimitiveTypes + [T_aiPrimitiveType.TRIANGLE] ;
    if (pmesh.PrimitiveTypes and T_aiPrimitiveTypes.POLYGON) = T_aiPrimitiveTypes.POLYGON then
      PrimitiveTypes := PrimitiveTypes + [T_aiPrimitiveType.POLYGON] ;

    Vertices        := nil ;
    Normals         := nil ;
    Tangents        := nil ;
    BiTangents      := nil ;
    Colors          := nil ;
    TextureCoords   := nil ;
    Faces           := nil ;
    Bones           := nil ;
    MeshAttachments := nil ;

    if ( pmesh.NumVertices > 0 ) and isValidPtr( pmesh.Vertices ) then begin
      Vertices := TList<T_aiVector3D>.Create ;
      Vertices.AddRange( fromPtrArray<T_aiVector3D>( pmesh.Vertices, pmesh.NumVertices ) ) ;

      if isValidPtr( pmesh.Normals ) then begin
        Normals := TList<T_aiVector3D>.Create ;
        Normals.AddRange( fromPtrArray<T_aiVector3D>( pmesh.Normals, pmesh.NumVertices ) ) ;
      end ;

      if isValidPtr( pmesh.Tangents ) then begin
        Tangents := TList<T_aiVector3D>.Create ;
        Tangents.AddRange( fromPtrArray<T_aiVector3D>( pmesh.Tangents, pmesh.NumVertices ) ) ;
      end ;

      if isValidPtr( pmesh.BiTangents ) then begin
        BiTangents := TList<T_aiVector3D>.Create ;
        BiTangents.AddRange( fromPtrArray<T_aiVector3D>( pmesh.BiTangents, pmesh.NumVertices ) ) ;
      end ;

      SetLength( Colors, T_aiDefines.AI_MAX_NUMBER_OF_COLOR_SETS ) ;
      for i := 0 to T_aiDefines.AI_MAX_NUMBER_OF_COLOR_SETS-1 do begin
        if isValidPtr( pmesh.Colors[i] ) then begin
          Colors[i] := TList<T_aiColor4D>.Create ;
          Colors[i].AddRange( fromPtrArray<T_aiColor4D>( pmesh.Colors[i], pmesh.NumVertices ) ) ;
        end ;
      end ;

      SetLength( TextureCoords, T_aiDefines.AI_MAX_NUMBER_OF_TEXTURECOORDS ) ;
      for i := 0 to T_aiDefines.AI_MAX_NUMBER_OF_TEXTURECOORDS-1 do begin
        if isValidPtr( pmesh.TextureCoords[i] ) then begin
          TextureCoords[i] := TList<T_aiVector3D>.Create ;
          TextureCoords[i].AddRange( fromPtrArray<T_aiVector3D>(pmesh.TextureCoords[i], pmesh.NumVertices ) ) ;
        end ;
      end ;

      SetLength( TextureComponents, T_aiDefines.AI_MAX_NUMBER_OF_TEXTURECOORDS ) ;
      for j := 0 to T_aiDefines.AI_MAX_NUMBER_OF_TEXTURECOORDS-1 do
        TextureComponents[j] := pmesh.NumUVComponents[j] ;

    end ;

    if ( pmesh.NumFaces > 0 ) and isValidPtr( pmesh.Faces ) then begin
      Faces := TObjectList<T_Face>.Create ;
      Faces.AddRange( fromPtrArray<T_Face, T_aiFace>( pmesh.Faces, pmesh.NumFaces, false ) ) ;
    end ;

    if ( pmesh.NumBones > 0 ) and isValidPtr( pmesh.Bones ) then begin
      Bones := TObjectList<T_Bone>.Create ;
      Bones.AddRange( fromPtrArray<T_Bone, T_aiBone>( pmesh.Bones, pmesh.NumBones, True ) ) ;
    end ;

    if ( pmesh.NumAniMeshes > 0 ) and isValidPtr( pmesh.AniMeshes ) then begin
      // todo
    end ;
  end ;

  procedure T_AMesh.doDestroy ;
  var
    i : Integer ;
  begin
    FreeObject( Vertices    ) ;
    FreeObject( Normals     ) ;
    FreeObject( Tangents    ) ;
    FreeObject( BiTangents  ) ;
    for i := 0 to length( Colors )-1 do
      FreeObject( Colors[i] ) ;
    for i := 0 to length( TextureCoords )-1 do
      FreeObject( TextureCoords[i] ) ;
    FreeObject( Faces           ) ;
    FreeObject( Bones           ) ;
    FreeObject( MeshAttachments ) ;

    inherited ;
  end ;

  function T_AMesh.fget_HasNormals : Boolean ;
  begin
    Result := assigned( Normals ) ;
  end ;

  function T_AMesh.fget_HasTextureCoords : Boolean ;
  begin
    Result := assigned( TextureCoords ) and
              assigned( TextureCoords[0] ) ;
  end ;

  function T_AMesh.fget_HasColors : Boolean ;
  begin
    Result := assigned( Colors ) and
              assigned( Colors[0] ) ;
  end ;


//=============================================================================
// T_MaterialProperty
//=============================================================================

  constructor T_MaterialProperty.Create(
    const _ptr : taiptr
  ) ;
  var
    pmatprop : P_aiMaterialProperty ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    {$IFDEF OXYGENE}
      pmatprop := marshalStructure<T_aiMaterialProperty>(_ptr)  ;
    {$ELSE}
      pmatprop := _ptr ;
    {$ENDIF}

    Name := Format( '%s,%d,%d', [ fromAiString( pmatprop.Key ),
                                  pmatprop.Semantic, pmatprop.Index ]
                  )  ;

    case pmatprop.PType of
      $01 : PropertyType := T_aiPropertyTypeInfo.Float ;
      $03 : PropertyType := T_aiPropertyTypeInfo.&String ;
      $04 : PropertyType := T_aiPropertyTypeInfo.Integer ;
      $05 : PropertyType := T_aiPropertyTypeInfo.Buffer ;
    end ;
    TextureIndex := pmatprop.Index ;
    TextureType  := pmatprop.Semantic ;

    RawData := nil ;
    RawDataCount := 0 ;

    if ( pmatprop.DataLength > 0 ) and isValidPtr( pmatprop.Data ) then begin
      {$IFDEF JAVA}
        system.arraycopy( fromPtrArray<Byte>( pmatprop.Data, pmatprop.DataLength ), 0, RawData, 0, pmatprop.DataLength ) ;
      {$ELSE}
        RawData := fromPtrArray<Byte>( pmatprop.Data, pmatprop.DataLength ) ;
      {$ENDIF}
      RawDataCount := pmatprop.DataLength ;
    end ;
  end ;

  function T_MaterialProperty.AsString : String ;
  var
    len : Integer ;
  begin
    if ( PropertyType = T_aiPropertyTypeInfo.String ) and ( RawDataCount > 0 ) then
    begin
      {$IFDEF DCC}
        Move( RawData[0], len, 4 ) ;
      {$ENDIF}
      {$IFDEF CLR}
        len := BitConverter.ToInt32( RawData, 0 ) ;
      {$ENDIF}
      Result := TEncoding.UTF8.GetString( RawData, 4, len ) ;
    end
    else
      Result := '' ;
  end ;

  function T_MaterialProperty.AsColor : T_aiColor4D ;
  var
    arr  : TGIS_SingleArray ;
    nc   : Integer ;
    sof4 : Integer ;
    sof3 : Integer ;
  begin
    if ( PropertyType = T_aiPropertyTypeInfo.Float ) and ( RawDataCount > 0 ) then
    begin
      {$IFDEF JAVA}
      sof3 := 3 * sizeOf(Single) ;
      sof4 := 4 * sizeOf(Single) ;
      {$ELSE}
      sof3 := sizeOf( T_aiColor3D ) ;
      sof4 := sizeOf( T_aiColor4D ) ;
      {$ENDIF}
      if RawDataCount = sof4 then begin
        nc := RawDataCount div sof4 ;
        SetLength( arr, 4 ) ;
        {$IFDEF DCC}
          Move( RawData[0], arr[0], RawDataCount ) ;
        {$ENDIF}
        {$IFDEF CLR}
          arr[0] := BitConverter.ToSingle( RawData, 0 ) ;
          arr[1] := BitConverter.ToSingle( RawData, 4 ) ;
          arr[2] := BitConverter.ToSingle( RawData, 8 ) ;
          arr[3] := BitConverter.ToSingle( RawData, 12 ) ;
        {$ENDIF}
        Result.r := arr[0] ;
        Result.g := arr[1] ;
        Result.b := arr[2] ;
        Result.a := arr[3] ;
      end
      else if RawDataCount = sof3 then begin
        nc := RawDataCount div sof3 ;
        SetLength( arr, 3 ) ;
        {$IFDEF DCC}
          Move( RawData[0], arr[0], RawDataCount ) ;
        {$ENDIF}
        {$IFDEF CLR}
          arr[0] := BitConverter.ToSingle( RawData, 0 ) ;
          arr[1] := BitConverter.ToSingle( RawData, 4 ) ;
          arr[2] := BitConverter.ToSingle( RawData, 8 ) ;
        {$ENDIF}
        Result.r := arr[0] ;
        Result.g := arr[1] ;
        Result.b := arr[2] ;
        Result.a := 1.0 ;
      end ;
    end
    else begin
      Result.r := 1.0 ;
      Result.g := 1.0 ;
      Result.b := 1.0 ;
      Result.a := 1.0 ;
    end ;
  end ;

  function T_MaterialProperty.AsFloat : Single ;
  begin
    if ( PropertyType = T_aiPropertyTypeInfo.Float ) and ( RawDataCount > 0 ) then
      {$IFDEF DCC}
        Move( RawData[0], Result, 4 )
      {$ENDIF}
      {$IFDEF CLR}
        Result := BitConverter.ToSingle( RawData, 0 )
      {$ENDIF}
    else
      Result := 0 ;
  end ;

  function T_MaterialProperty.AsInteger : Integer ;
  begin
    if ( PropertyType = T_aiPropertyTypeInfo.Integer ) and ( RawDataCount > 0 ) then
      {$IFDEF DCC}
        Move( RawData[0], Result, 4 )
      {$ENDIF}
      {$IFDEF CLR}
        Result := BitConverter.ToInt32( RawData, 0 )
      {$ENDIF}
    else
      Result := 0 ;
  end ;


//=============================================================================
// T_Material
//=============================================================================

  constructor T_Material.Create(
    const _ptr : taiptr
  ) ;
  var
    pmaterial : P_aiMaterial ;
    lst       : TList<T_MaterialProperty> ;
    {$IFDEF DCC}
    mp        : T_MaterialProperty ;
    {$ENDIF}
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    {$IFDEF OXYGENE}
      pmaterial := marshalStructure<T_aiMaterial>(_ptr)  ;
    {$ELSE}
      pmaterial := _ptr ;
    {$ENDIF}

    if ( pmaterial.NumProperties > 0 ) and isValidPtr( pmaterial.Properties ) then begin
      lst := TList<T_MaterialProperty>.Create ;
      try
        Properties := TDictionary<String, T_MaterialProperty>.Create ;
        lst.AddRange( fromPtrArray<T_MaterialProperty, T_aiMaterialProperty>(
                        pmaterial.Properties, pmaterial.NumProperties, True
                      )
                    ) ;
        for mp in lst do
          Properties.Add( mp.Name, mp ) ;
      finally
        FreeObject( lst ) ;
      end ;
    end ;
  end ;

  procedure T_Material.doDestroy ;
  {$IFDEF DCC}
  var
    kv  : TPair<String, T_MaterialProperty> ;
  {$ENDIF}
  begin
    for kv in Properties do
      FreeObjectNotNil( kv.Value ) ;

    FreeObject( Properties ) ;

    inherited ;
  end ;

//=============================================================================
// T_Animation
//=============================================================================

  constructor T_Animation.Create(
    const _ptr : taiptr
  ) ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    // todo
  end ;

//=============================================================================
// T_Light
//=============================================================================

  constructor T_Light.Create(
    const _ptr : taiptr
  ) ;
  var
    plight : P_aiLight ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    {$IFDEF OXYGENE}
      plight := marshalStructure<T_aiLight>(_ptr)  ;
    {$ELSE}
      plight := _ptr ;
    {$ENDIF}

    Name          := fromAiString( plight.Name ) ;
    LightType     := plight.LightType ;
    Position      := plight.Position ;
    Direction     := plight.Direction ;
    Up            := plight.Up ;
    Size          := plight.Size ;
    ColorDiffuse  := plight.ColorDiffuse ;
    ColorSpecular := plight.ColorSpecular ;
    ColorAmbient  := plight.ColorAmbient ;
  end ;

//=============================================================================
// T_Texture
//=============================================================================

  constructor T_Texture.Create(
    const _ptr : taiptr
  ) ;
  var
    ptexture : P_aiTexture ;
    size     : Integer ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    {$IFDEF OXYGENE}
      ptexture := marshalStructure<T_aiTexture>(_ptr) ;
    {$ELSE}
      ptexture := _ptr ;
    {$ENDIF}

    IsCompressed := ptexture.Height = 0 ;
    if IsCompressed then begin
      Width  := 0 ;
      Height := 0 ;

      if ( ptexture.Width > 0 ) and isValidPtr( ptexture.Data ) then begin
        SetLength( CompressedData, ptexture.Width ) ;
        {$IFDEF DCC}
          Move( ptexture.Data[0], CompressedData[0], ptexture.Width ) ;
        {$ENDIF}
        {$IFDEF CLR}
          CompressedData := fromPtrArray<Byte>( ptexture.Data, ptexture.Width ) ;
        {$ENDIF}
      end ;
    end
    else begin
      Width  := ptexture.Width ;
      Height := ptexture.Height ;

      size := Width * Height * 4 ;
      if ( size > 0 ) and isValidPtr( ptexture.Data ) then begin
        SetLength( NonCompressedData, size ) ;
        {$IFDEF DCC}
          Move( ptexture.Data[0], NonCompressedData[0], size ) ;
        {$ENDIF}
        {$IFDEF CLR}
          NonCompressedData := fromPtrArray<T_aiTexel>( ptexture.Data, size ) ;
        {$ENDIF}
      end ;
    end ;
    {$IFDEF DCC}
      FormatHint := '   ' ;
      FormatHint[1] := Char(ptexture.FormatHint[0]) ;
      FormatHint[2] := Char(ptexture.FormatHint[1]) ;
      FormatHint[3] := Char(ptexture.FormatHint[2]) ;
    {$ENDIF}
    {$IFDEF CLR}
      FormatHint := TEncoding.ASCII.GetString( ptexture.FormatHint, 0, 3 ) ;
    {$ENDIF}
    FileName   := fromAiString( ptexture.FileName ) ;
  end ;

//=============================================================================
// T_Camera
//=============================================================================

  constructor T_Camera.Create(
    const _ptr : taiptr
  ) ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    // todo
  end ;

//=============================================================================
// T_aiScene
//=============================================================================

  constructor T_Scene.Create(
    const _ptr : taiptr
  ) ;
  var
    pscene : P_aiScene ;
  begin
    inherited Create ;

    if not isValidPtr( _ptr ) then exit ;

    {$IFDEF OXYGENE}
      pscene := marshalStructure<T_aiScene>(_ptr) ;
    {$ELSE}
      pscene := _ptr ;
    {$ENDIF}

    &Flags := pscene.&Flags ;

    RootNode    := nil ;
    Meshes      := nil ;
    Meshes      := nil ;
    Materials   := nil ;
    Animations  := nil ;
    Textures    := nil ;
    Lights      := nil ;
    Cameras     := nil ;

    if isValidPtr( pscene.RootNode ) then begin
      RootNode := T_ANode.Create( pscene.RootNode ) ;
      RootNode.Parent := nil ;
    end ;

    if ( pscene.NumMeshes > 0 ) and isValidPtr( pscene.Meshes ) then begin
      Meshes := TObjectList<T_AMesh>.Create ;
      Meshes.AddRange( fromPtrArray<T_AMesh, T_aiMesh>(
                         pscene.Meshes, pscene.NumMeshes, True
                       )
                     ) ;
    end ;

    if ( pscene.NumMaterials > 0 ) and isValidPtr( pscene.Materials ) then begin
      Materials := TObjectList<T_Material>.Create ;
      Materials.AddRange( fromPtrArray<T_Material, T_aiMaterial>(
                             pscene.Materials, pscene.NumMaterials, True
                           )
                         ) ;
    end  ;

    if ( pscene.NumAnimations > 0 ) and isValidPtr( pscene.Animations ) then begin
    // TODO
//      Animations := TObjectList<T_Animation>.Create ;
//      Animations.AddRange( fromPtrArray<T_Animation, T_aiAnimation>(
//                             pscene.Animations, pscene.NumAnimations, True
//                           )
//                         ) ;
    end ;

    if ( pscene.NumTextures > 0 ) and isValidPtr( pscene.Textures ) then begin
      Textures := TObjectList<T_Texture>.Create ;
      Textures.AddRange( fromPtrArray<T_Texture, T_aiTexture>(
                             pscene.Textures, pscene.NumTextures, True
                           )
                         ) ;
    end ;

    if ( pscene.NumLights > 0 ) and isValidPtr( pscene.Lights ) then begin
      Lights := TObjectList<T_Light>.Create ;
      Lights.AddRange( fromPtrArray<T_Light, T_aiLight>(
                           pscene.Lights, pscene.NumLights, True
                         )
                       ) ;
    end ;

    if ( pscene.NumCameras > 0 ) and isValidPtr( pscene.Cameras ) then begin
      Cameras := TObjectList<T_Camera>.Create ;
      // TODO
//      Cameras.AddRange( fromPtrArray<T_Camera, T_aiCamera>(
//                             pscene.Cameras, pscene.NumCameras, True
//                           )
//                         ) ;
    end ;

    // todo MetaData

  end ;

  procedure T_Scene.doDestroy ;
  begin
    FreeObject( RootNode   ) ;
    FreeObject( Meshes     ) ;
    FreeObject( Materials  ) ;
    FreeObject( Animations ) ;
    FreeObject( Textures   ) ;
    FreeObject( Lights     ) ;
    FreeObject( Cameras    ) ;

    inherited ;
  end ;

//=============================================================================
// T_AssimpContext
//=============================================================================

  function T_AssimpContext.ImportFile(
    const _file   : String ;
    const _flags  : Integer
  ) : T_Scene ;
  var
    ptr : taiptr ;
    cmd : taicmd ;
    msg : String ;
  begin
    cmd := toaiAPI( _file ) ;
    try
      ptr := T_AssimpLib(FAssimpLib).aiImportFile( cmd, _flags ) ;

      if isValidPtr( ptr ) then begin
        Result := T_Scene.Create( ptr ) ;
        Result.Path := _file ;
        T_AssimpLib(FAssimpLib).aiReleaseImport( ptr ) ;
      end
      else begin
        Result := nil ;
        msg := asAiString( T_AssimpLib(FAssimpLib).aiGetErrorString ) ;
        if not IsStringEmpty( msg ) then
          raise EGIS_Exception.Create( msg, '', 0 ) ;
      end ;
    except
      Result := nil ;
    end ;
  end ;

  procedure T_AssimpRenderer.recursiveRender(
    const _scene : T_Scene ;
    const _node  : T_ANode ;
      var _mat   : T_aiMatrix4x4
  ) ;
  var
    i,p,
    c,t,n     : Integer ;
    {$IFDEF DCC}
    idx       : Cardinal ;
    face      : T_Face ;
    {$ENDIF}
    mesh      : T_AMesh ;
    cnt       : Integer ;
    ptype     : TGIS_PartType ;
    stype     : TGIS_ShapeType ;
    indice    : Cardinal ;
    vcolor    : TGIS_Color ;
    nv        : T_aiVector3D ;
    uvw       : T_aiVector3D ;
    coord     : T_aiVector3D ;

    shp       : TGIS_Shape ;
    mshp      : TGIS_ShapeMultiPatch ;
    trafo     : T_aiMatrix4x4 ;
    rot_mat   : T_aiMatrix4x4 ;
    trafo_rot : T_aiMatrix4x4 ;
    material  : TGIS_Material ;

    function fromAiColorToGisColor(
      const _color     : T_aiColor4D
    ) : TGIS_Color ;
    begin
      Result := TGIS_Color.FromARGB(
                  Byte(TruncS(_color.a*255)),
                  Byte(TruncS(_color.r*255)),
                  Byte(TruncS(_color.g*255)),
                  Byte(TruncS(_color.b*255))
                )
    end ;

    function fromAiColorToSingleColor(
      const _color : T_aiColor4D
    ) : TGIS_SingleColor ; overload ;
    begin
      Result.r := _color.r ;
      Result.g := _color.g ;
      Result.b := _color.b ;
      Result.a := _color.a ;
    end ;

    procedure prepareMaterial ;
    begin
      material.HasColor           := False ;
      material.Color              := TGIS_Color.White ;
      material.HasEdgeColor       := False ;
      material.EdgeColor          := TGIS_Color.Black ;
      material.HasEdgeWidth       := False ;
      material.EdgeWidth          := 0 ;
      material.HasTransparency    := False ;
      material.Transparency       := 0 ;
      material.HasShininess       := False ;
      material.Shininess          := 0 ;
      material.HasCullBackFaces   := False ;
      material.SharedTextureIndex := 0 ;
      material.HasSharedTexture   := False ;
      material.Bpp                := 0 ;
      material.CompressionType    := TGIS_CompressionType.None ;
      material.HasTextureMap      := False ;
      material.Width              := 0 ;
      material.Height             := 0 ;
      material.Size               := 0 ;
      material.Buffer             := nil ;
      material.HasMaterialColor   := False ;
      SetLength( material.MaterialColors, 6 ) ;
      material.MaterialColors[0]  := GisSingleColor( 0, 0, 0, 1 ) ;
      material.MaterialColors[1]  := GisSingleColor( 0, 0, 0, 1 ) ;
      material.MaterialColors[2]  := GisSingleColor( 0, 0, 0, 1 ) ;
      material.MaterialColors[3]  := GisSingleColor( 0, 0, 0, 1 ) ;
      material.MaterialColors[4]  := GisSingleColor( 0, 0, 0, 1 ) ;
      material.MaterialColors[5]  := GisSingleColor( 0, 0, 0, 1 ) ;
    end ;

    function findTexture( const _name : String ) : Integer ;
    var
      t : Integer ;
    begin
      Result := -1 ;
      if not assigned( _scene.Textures ) then exit ;

      if ( Pos( '*', _name ) = StringFirst ) then begin
        Result := StrToInt( _name[StringFirst+1] ) ;
        if ( Result >= _scene.Textures.Count ) then
          Result := -1 ;
      end
      else begin
        for t := 0 to _scene.Textures.Count-1 do begin
          if _scene.Textures[t].FileName = _name then begin
            Result := t ;
            break ;
          end ;
        end ;
      end ;
    end ;

    procedure loadTexture( const _name : String ) ;
    var
      tpath     : String ;
      fname     : String ;
      bmp       : TGIS_Bitmap ;
      pix       : TGIS_Pixels ;
      idx       : Integer ;
      txt       : T_Texture ;
      stm       : TMemoryStream ;
    begin
      fname := Trim( _name ) ;
      if IsStringEmpty( fname ) then exit ;

      idx := findTexture( fname ) ;
      if ( idx > -1 ) then begin
        txt := _scene.Textures[idx] ;
        if not txt.IsCompressed then begin
          material.Bpp    := 4 ;
          material.Width  := txt.Width ;
          material.Height := txt.Height ;
          material.Size   := txt.Width * txt.Height * 4 ;
          SetLength( material.Buffer, txt.Width * txt.Height * 4 ) ;
          {$IFDEF DCC}
            System.Move( txt.NonCompressedData[0], material.Buffer[0], txt.Width * txt.Height * 4 ) ;
          {$ENDIF}
          {$IFDEF CLR}
            Buffer.BlockCopy( txt.NonCompressedData, 0,material.Buffer, 0, txt.Width * txt.Height * 4 ) ;
          {$ENDIF}
        end
        else begin
          bmp := TGIS_Bitmap.Create ;
          try
            stm := TMemoryStream.Create ;
            try
              stm.Write( txt.CompressedData, length( txt.CompressedData ) ) ;
              stm.Position := 0 ;
              bmp.LoadFromStream( stm ) ;
            finally
              FreeObject( stm ) ;
            end ;

            material.Bpp    := 4 ;
            material.Width  := bmp.Width ;
            material.Height := bmp.Height ;
            material.Size   := bmp.Width * bmp.Height * 4 ;
            SetLength( material.Buffer, bmp.Width * bmp.Height * 4 ) ;

            bmp.LockPixels( pix, false, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
            try
              {$IFDEF CLR}
                Buffer.BlockCopy( pix, 0,material.Buffer, 0, bmp.Width * bmp.Height * 4 ) ;
              {$ENDIF}
              {$IFDEF JAVA}
                System.arraycopy( pix, 0, material.Buffer, 0, bmp.Width * bmp.Height ) ;
              {$ENDIF}
              {$IFDEF DCC}
                System.Move( pix[0], material.Buffer[0], bmp.Width * bmp.Height * 4 ) ;
              {$ENDIF}
            finally
             bmp.UnlockPixels ;
            end ;
          finally
            FreeObject( bmp ) ;
          end ;
        end ;
        material.CompressionType := TGIS_CompressionType.ARGB ;
        material.HasTextureMap := True ;
      end
      else begin
        tpath := GetPathAbsolute( GetFilePath( _scene.Path ), fname ) ;
        if not SafeFileExists( tpath ) then
          tpath := GetFileName( fname ) ;

        if SafeFileExists( tpath ) then begin

          bmp := TGIS_Bitmap.Create ;
          try
            try
              bmp.LoadFromFile( tpath ) ;

              material.Bpp    := 4 ;
              material.Width  := bmp.Width ;
              material.Height := bmp.Height ;
              material.Size   := bmp.Width * bmp.Height * 4 ;
              SetLength( material.Buffer, bmp.Width * bmp.Height * 4 ) ;

              bmp.LockPixels( pix, false, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
              try
                {$IFDEF CLR}
                  Buffer.BlockCopy( pix, 0,material.Buffer, 0, bmp.Width * bmp.Height * 4 ) ;
                {$ENDIF}
                {$IFDEF JAVA}
                  System.arraycopy( pix, 0, material.Buffer, 0, bmp.Width * bmp.Height ) ;
                {$ENDIF}
                {$IFDEF DCC}
                  System.Move( pix[0], material.Buffer[0], bmp.Width * bmp.Height * 4 ) ;
                {$ENDIF}
              finally
               bmp.UnlockPixels ;
              end ;

              material.CompressionType := TGIS_CompressionType.ARGB ;
              material.HasTextureMap := True ;
            except
              // ignore errors
            end;
          finally
            FreeObject( bmp ) ;
          end ;

        end ;
      end ;
    end ;

    procedure makeColorMaterial( const _mp : T_MaterialProperty ) ;
    begin
       //  0 : diffuse
       //  1 : ambient
       //  2 : specular
       //  3 : emissive
       //  4 : transparent
       //  5 : reflective

      if ( Pos( '$clr.diffuse,0,0', _mp.Name ) >= StringFirst ) then begin
        material.HasMaterialColor  := True ;
        material.MaterialColors[0] := fromAiColorToSingleColor( _mp.AsColor ) ;
        material.HasColor  := True ;
        material.Color := fromAiColorToGisColor( _mp.AsColor ) ;
      end
      else if ( Pos( '$clr.ambient,0,0', _mp.Name ) >= StringFirst ) then begin
        material.HasMaterialColor  := True ;
        material.MaterialColors[1] := fromAiColorToSingleColor( _mp.AsColor ) ;
      end
      else if ( Pos( '$clr.specular,0,0', _mp.Name ) >= StringFirst ) then begin
        material.HasMaterialColor  := True ;
        material.MaterialColors[2] := fromAiColorToSingleColor( _mp.AsColor ) ;
      end
      else if ( Pos( '$clr.emissive,0,0', _mp.Name ) >= StringFirst ) then begin
        material.HasMaterialColor  := True ;
        material.MaterialColors[3] := fromAiColorToSingleColor( _mp.AsColor ) ;
      end
      else if ( Pos( '$clr.transparent,0,0', _mp.Name ) >= StringFirst ) then begin
        material.HasMaterialColor  := True ;
        material.MaterialColors[4] := fromAiColorToSingleColor( _mp.AsColor ) ;
      end
      else if ( Pos( '$clr.reflective,0,0', _mp.Name ) >= StringFirst ) then begin
        material.HasMaterialColor  := True ;
        material.MaterialColors[5] := fromAiColorToSingleColor( _mp.AsColor ) ;
      end
      else if ( Pos( '$mat.opacity,0,0', _mp.Name ) >= StringFirst ) then begin
     //   material.HasTransparency := True ;
     //   material.Transparency := Byte( TruncS( _mp.AsFloat ) * 100 ) ;
      end
      else if ( Pos( '$mat.shininess,0,0', _mp.Name ) >= StringFirst ) then begin
        material.HasShininess := True ;
        material.Shininess := Byte( TruncS( _mp.AsFloat * 100 ) ) ;
      end ;

    end ;

    procedure applyMaterial( const _material : T_Material ) ;
    var
      {$IFDEF DCC}
      kv        : TPair<String, T_MaterialProperty> ;
      {$ENDIF}
      mp        : T_MaterialProperty ;
    begin
      if not assigned( _material.Properties ) then exit ;

      prepareMaterial ;

      for kv in _material.Properties do begin
        mp := kv.Value ;
        if ( Pos( '$tex.file', mp.Name ) >= StringFirst ) {and
           ( mp.TextureType = $01 )} then
          loadTexture( mp.AsString )
        else
          makeColorMaterial( mp ) ;
      end ;

      mshp.HasMaterials := True ;
      mshp.Materials.Resize( 1 ) ;
      mshp.Materials.Material[0]  := material ;
    end ;

    procedure addMetadata(
      const _shp      : TGIS_Shape ;
      const _metadata : T_Metadata
    ) ;
    var
      e : TPair<String, T_MetadataEntry> ;
    begin
      for e in _metadata.Properties do begin
        if oLayer.FindField( e.Key ) = -1 then
          oLayer.AddFieldInternal( e.Key, TGIS_FieldType.String, 1, 0 ) ;

        _shp.SetField( e.Key, e.Value.Data ) ;
      end ;
    end ;

  begin
    if not assigned( _node ) then exit ;

    trafo     := T_aiMatrix4x4_Utils.Multiply( _node.Transformation, _mat ) ;
    rot_mat   := T_aiMatrix4x4_Utils.Rotation( vRotate ) ;
    trafo_rot := T_aiMatrix4x4_Utils.Multiply( trafo, rot_mat ) ;

    if assigned( _node.MeshIndices ) then begin
      for idx in _node.MeshIndices do begin
        mesh := _scene.Meshes[idx] ;

        if T_aiPrimitiveType.POINT in mesh.PrimitiveTypes then
          stype := TGIS_ShapeType.Point
        else if T_aiPrimitiveType.LINE in mesh.PrimitiveTypes then
          stype := TGIS_ShapeType.Arc
        else if (T_aiPrimitiveType.POLYGON in mesh.PrimitiveTypes ) or
                (T_aiPrimitiveType.TRIANGLE in mesh.PrimitiveTypes) then
          stype := TGIS_ShapeType.MultiPatch ;

        if T_aiPrimitiveType.TRIANGLE in mesh.PrimitiveTypes then
          ptype := TGIS_PartType.Triangle
        else
          ptype := TGIS_PartType.Ring ;

        if assigned( mesh.Faces ) then begin

          shp := oLayer.CreateShape( stype ) ;
          shp.Lock( TGIS_Lock.Projection ) ;
          shp.AddPart ;

          if ( stype = TGIS_ShapeType.MultiPatch ) then
            mshp := TGIS_ShapeMultiPatch( shp )
          else
            mshp := nil ;

          cnt := length( mesh.Faces[0].Indices ) ;
          if cnt = 3 then
            ptype := TGIS_PartType.Triangle
          else
            ptype := TGIS_PartType.Ring ;

          p := 1 ;
          shp.SetPartType( p-1, ptype ) ;

          if assigned( mshp ) and UseMaterials then begin
            applyMaterial( _scene.Materials[mesh.MaterialIndex] ) ;

            if mesh.HasTextureCoords then begin
              mshp.HasTextures := True ;
              mshp.Textures.Resize( 1, 2*6 * mesh.Faces.Count, 2 ) ;
              mshp.Textures.SetPartOffset( 0, 0 ) ;
            end ;

            if mesh.HasNormals then  begin
              mshp.HasNormals := True ;
              mshp.Normals.Resize( mesh.Faces.Count ) ;
              for i := 0 to mesh.Normals.Count-1 do begin
                nv := mesh.Normals[i] ;
                mshp.Normals.Normal[i] := GisSingleVector( nv.x, nv.y, nv.z ) ;
              end ;

            end ;

            if mesh.HasColors then begin
              mshp.HasVertexColors := True ;
              mshp.VertexColors.Resize( 1, 3 * mesh.Faces.Count ) ;
              mshp.VertexColors.SetPartOffset( 0, 0 ) ;
            end ;

          end ;

          c := 0 ;
          t := 0 ;
          n := 0 ;
          for face in mesh.Faces do begin
            cnt := length( face.Indices ) ;

            if cnt = 3 then begin
              if ptype <> TGIS_PartType.Triangle then begin
                ptype := TGIS_PartType.Triangle ;
                shp.AddPart ;
                inc( p ) ;
                shp.SetPartType( p-1, ptype ) ;
              end ;
            end
            else begin
              ptype := TGIS_PartType.Ring ;
              shp.AddPart ;
              inc( p ) ;
              shp.SetPartType( p-1, ptype ) ;
            end ;

            for i := 0 to cnt-1 do begin
              indice := face.Indices[i] ;

              if UseMaterials then begin

                if assigned( mshp ) and mesh.HasColors then begin
                  vcolor := fromAiColorToGisColor( mesh.Colors[0][indice] ) ;
                  mshp.VertexColors.SetVertexColor( c, vcolor.ARGB ) ;
                  inc( c ) ;
                end ;

              {  if mesh.HasNormals then begin
                  nv := mesh.Normals[indice] ;
                  mshp.Normals.Normal[n] := GisSingleVector( nv.x, nv.y, nv.z ) ;
                  inc( n ) ;
                end ;}

                if assigned( mshp ) and mesh.HasTextureCoords and mshp.HasMaterials then begin
                  uvw := mesh.TextureCoords[0][indice] ;
                  mshp.Textures.SetTextureCoord( t, uvw.x ) ;
                  inc( t ) ;
                  mshp.Textures.SetTextureCoord( t, 1-uvw.y ) ;
                  inc( t ) ;
                end ;
              end ;

              coord := T_aiMatrix4x4_Utils.Multiply( trafo_rot, mesh.Vertices[indice] ) ;
              case AxisOrder of
                0 : shp.AddPoint3D(
                      GisPoint3D( coord.x * vScale.X + vTranslate.X,
                                  coord.y * vScale.Y + vTranslate.Y,
                                  coord.z * vScale.Z + vTranslate.Z,
                                  0
                                 )
                    ) ;
                1 : shp.AddPoint3D( GisPoint3D( coord.x * vScale.X + vTranslate.X,
                                                coord.z * vScale.Y + vTranslate.Y,
                                                coord.y * vScale.Z + vTranslate.Z,
                                                0
                                               )
                                  ) ;
              end ;

            end ;
          end ;
          shp.Unlock ;

          shp.SetField( 'name', _node.Name ) ;
          if assigned(_node.Parent ) then
            shp.SetField( 'parent', _node.Parent.Name ) ;

          if assigned( _node.MetaData ) then
            addMetadata( shp, _node.MetaData ) ;

           if UseMaterials and assigned( mshp ) then begin
            if (mesh.HasColors or mesh.HasTextureCoords or mesh.HasNormals) or mshp.HasMaterials then begin
              mshp.HasPartDescriptors := True ;
              mshp.PartDescriptors.Resize( shp.GetNumParts ) ;
              for i := 0 to shp.GetNumParts-1 do
                mshp.PartDescriptors.PartDescriptor[i] := GisPartDescriptor( 6,0,0,0 ) ;
            end ;
           end ;

        end ;
      end ;
    end ;

    if assigned( _node.Children ) then
      for i := 0 to _node.Children.Count-1 do
        recursiveRender( _scene, _node.Children[i], trafo ) ;
  end ;

  procedure T_AssimpRenderer.Render(
    const _scene : T_Scene ;
    const _layer : TGIS_LayerVector
  ) ;
  var
    mat : T_aiMatrix4x4 ;
  begin
    oLayer := _layer ;
    mat    := T_aiMatrix4x4_Utils.Indentity ;

    if oLayer.FindField( 'name' ) < 0 then
      oLayer.AddFieldInternal( 'name', TGIS_FieldType.String, 1, 0 ) ;
    if oLayer.FindField( 'parent' ) < 0 then
      oLayer.AddFieldInternal( 'parent', TGIS_FieldType.String, 1, 0 ) ;

    recursiveRender( _scene, _scene.RootNode, mat ) ;
  end ;

//=============================================================================
// T_AssimpLib
//=============================================================================

  constructor T_AssimpLib.Create ;
  begin
    {$IFNDEF OXYGENE}
    inherited Create ;
    {$ENDIF}

    libraryLoaded := False ;
  end ;

  {$IFNDEF OXYGENE}
    destructor T_AssimpLib.Destroy ;
    begin
      if libraryLoaded and ( dllHandle <> 0 ) then begin
        FreeLibrary( dllHandle ) ;
        dllHandle := 0 ;
        FAssimpInstanceCount := 0 ;
        libraryLoaded := False ;
      end ;

      inherited ;
    end ;
  {$ENDIF}

  function T_AssimpLib.LoadDLL(
    const _dllPath : String
  ) : Boolean ;
  var
    dllok : Boolean ;

    {$IFNDEF JAVA}
      procedure mapFnc( {$IFDEF LEVEL_XE2_RTL}
                          var _fnc : Pointer ;
                        {$ELSE}
                          var _fnc : IntPtr ;
                        {$ENDIF}
                         const _name : String
                       ) ;
      begin
        {$IFDEF OXYGENE}
          _fnc := GetProcAddress( dllHandle, _name ) ;
        {$ELSE}
          _fnc := GetProcAddress( dllHandle, PChar( _name ) ) ;
        {$ENDIF}
        assert( _fnc <> nil ) ;
      end ;
    {$ENDIF}

  begin
    Result := libraryLoaded ;

    {$IFDEF JAVA}
      if libraryLoaded or (dllHandle <> nil ) then exit ;

      dllHandle := IAssimpLibrary(com.sun.jna.Native.loadLibrary( _dllPath, typeOf(IAssimpLibrary) ) );
      dllok := dllHandle <> nil ;
    {$ELSE}
      if libraryLoaded or (dllHandle <> 0 ) then exit ;

      dllHandle := LoadLibraryWithinHinstance( _dllPath ) ;
      dllok := dllHandle <> 0 ;
    {$ENDIF}

    libraryLoaded := False ;
    if not dllok then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                   _dllPath + #13#10 + SystemErrorMessage,
                                   0
                                  ) ;
    {$IFDEF OXYGENE}
    {$ELSE}
      mapFnc( @aiImportFile,              'aiImportFile'            ) ;
      mapFnc( @aiReleaseImport,           'aiReleaseImport'         ) ;
      mapFnc( @aiGetErrorString,          'aiGetErrorString'        ) ;
      mapFnc( @aiGetExtensionList,        'aiGetExtensionList'      ) ;
    {$ENDIF}
    libraryLoaded := True ;

    Result := libraryLoaded ;
  end ;

  {$IFDEF JAVA}
    function T_AssimpLib.aiImportFile(
      _file   : String ;
      _flags  : Integer
    ) : taiptr ;
    begin
      Result := dllHandle.aiImportFile( _file, _flags ) ;
    end ;

    procedure T_AssimpLib.aiReleaseImport( _instance : taiptr )  ;
    begin
      dllHandle.aiReleaseImport( _instance ) ;
    end ;

    function  T_AssimpLib.aiGetErrorString : taiptr ;
    begin
      Result := dllHandle.aiGetErrorString ;
    end ;

    procedure T_AssimpLib.aiGetExtensionList( var _str : T_aiString ) ;
    begin
      dllHandle.aiGetExtensionList( _str ) ;
    end ;

  {$ENDIF}

//=============================================================================
// TGIS_FileAssimp
//=============================================================================

  constructor TGIS_FileAssimp.Create ;
  begin
    inherited ;

    inc( FAssimpInstanceCount ) ;
    FUseMaterials := true ;
    FAxisOrder    := 0 ;
    FInit         := False ;
    FScale        := GisSingleVector( 1, 1, 1 ) ;
    FTranslate    := GisSingleVector( 0, 0, 0 ) ;
  end ;

  procedure TGIS_FileAssimp.doDestroy ;
  begin
    FreeObject( FModel ) ;

    dec( FAssimpInstanceCount ) ;

    if FAssimpInstanceCount <= 0 then
      FreeObject( FAssimpLib ) ;

    inherited ;
  end ;

  function TGIS_FileAssimp.initialize : Boolean ;
  var
    sdll : String ;
  begin
    Result := False ;

    if FInit then begin
      Result := True ;
      exit ;
    end ;

    {$IFDEF OXYGENE}
    {$ELSE}
      if not assigned( FAssimpLib ) then
        FAssimpLib := T_AssimpLib.Create ;
    {$ENDIF}

    {$IFDEF JAVA}
      sdll := ASSIMP_DLL_NAME_NOEXT ;
    {$ELSE}
      sdll := ASSIMP_DLL_NAME ;
    {$ENDIF}

    if not T_AssimpLib(FAssimpLib).LoadDLL( sdll ) then begin
      exit ;
    end
    else begin
      Result := True ;
      FInit := True ;
    end ;
  end ;

  function TGIS_FileAssimp.Open(
    const _path  : String ;
    const _flags : Integer
  ) : Boolean ;
  var
    ai_import : T_AssimpContext ;
    flg : Integer ;
  begin
    Result := initialize ;

    if not Result then exit ;

    ai_import := T_AssimpContext.Create ;
    try
      flg := 0 ;
      case _flags of
        0 : flg := T_aiPostProcessSteps.Preset_TargetRealtime_Default ;
        1 : flg := T_aiPostProcessSteps.Preset_TargetRealtime_Fast or
                   T_aiPostProcessSteps.Preset_ConvertToLeftHanded ;
        2 : flg := T_aiPostProcessSteps.Preset_TargetRealtime_Quality or
                   T_aiPostProcessSteps.Preset_ConvertToLeftHanded ;
        3 : flg := T_aiPostProcessSteps.Preset_TargetRealtime_MaxQuality or
                   T_aiPostProcessSteps.Preset_ConvertToLeftHanded ;
      end ;

      if Pos( '.IFC', UpperCase( GetFileExt( _path ) ) ) = StringFirst then
        flg := flg xor T_aiPostProcessSteps.Triangulate ;

      FModel := ai_import.ImportFile( _path, flg ) ;
      Result := assigned( FModel ) ;
    finally
      FreeObject( ai_import ) ;
    end ;

  end ;

  procedure TGIS_FileAssimp.BuildMeshes(
    const _layer : TGIS_LayerVector
  ) ;
  var
    ai_renderer : T_AssimpRenderer ;
  begin
    ai_renderer := T_AssimpRenderer.Create ;
    try
      if assigned( FModel ) then begin
        ai_renderer.UseMaterials := UseMaterials ;
        ai_renderer.AxisOrder    := AxisOrder ;
        ai_renderer.Translate    := Translate ;
        ai_renderer.Scale        := Scale ;
        ai_renderer.Rotate       := Rotate ;

        ai_renderer.Render( T_Scene(FModel), _layer ) ;
      end ;
    finally
      FreeObject( ai_renderer ) ;
    end ;
  end ;

  function TGIS_FileAssimp.GetExtensionsList : String ;
  var
    str  : T_aiString ;
    buf  : TBytes ;
  begin
    if not initialize then exit ;

    T_AssimpLib(FAssimpLib).aiGetExtensionList( str ) ;
    SetLength( buf, str.Length ) ;
    {$IFDEF DCC}
      System.Move( str.Data[0], buf[0], str.Length ) ;
    {$ENDIF}
    {$IFDEF CLR}
      Buffer.BlockCopy( str.Data, 0, buf, 0, str.Length ) ;
    {$ENDIF}

    Result := TEncoding.UTF8.GetString( buf, 0, str.Length ) ;
  end ;

//==================================== END =====================================

end.

