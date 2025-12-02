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
  Encapsulation of the TGIS_Renderer3DDirectX9 class.
}

{$IFDEF DCC}
  unit GisRenderer3DDirectX9 ;
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
    System.IO,
    System.Runtime.InteropServices,
    System.Drawing,

    {$IFDEF CLR}
      TatukGIS.NDK.WinForms,
    {$ELSE}
    {$ENDIF}
    TatukGIS.RTL,
    TatukGIS.RTL.SharpDX ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.SysUtils,
    System.Math,
    Winapi.Windows,
    GisRtl,
    GisInterfaces,
    GisFunctions,
    GisInternals,
    GisTypes,
    GisTypesUI,
    GisClasses,
    GisResource,
    GisParams,
    GisLayer,
    GisLayerVector,
    GisLayerPixel,
    GisLabelsArea,
    GisHtmlLabel,
    GisRendererAbstract,
    GisRenderer3DAbstract,
    GisRenderer3DDirectXAbstract,
    GisDirectXLite ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.util,
    tatukgis.rtl ;
{$ENDIF}

const

  /// <summary>
  ///   DirectX color vertex format.
  /// </summary>
  GISVIEWER3D_D3DFVF_GRAPH3DCOLOR   = D3DFVF_XYZ     or
                                      D3DFVF_NORMAL  or
                                      D3DFVF_DIFFUSE ;

  /// <summary>
  ///   DirectX texture vertex format.
  /// </summary>
  GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE = D3DFVF_XYZ     or
                                      D3DFVF_NORMAL  or
                                      D3DFVF_DIFFUSE or
                                      D3DFVF_TEX1    ;


type
  /// <summary>
  ///   Private TMesh class for vertex buffers management.
  /// </summary>
  T_Mesh = TGIS_Object ;

  /// <summary>
  ///   Array of T_Mesh.
  /// </summary>
  T_arMesh = array of T_Mesh ;

  /// <summary>
  ///   Structure storing label info.
  /// </summary>
  T_labelInfo  = record
    /// <summary>
    ///   Label X,Y coordinate.
    /// </summary>
    Centroid       : TGIS_Point        ;

    /// <summary>
    ///   Label Z coordinate.
    /// </summary>
    LabelZ         : Double            ;

    /// <summary>
    ///   Base level of labeled object.
    /// </summary>
    LabelB         : Double            ;

    /// <summary>
    ///   Label size.
    /// </summary>
    LabelSize      : TPoint            ;

    /// <summary>
    ///   Label position.
    /// </summary>
    LabelPos       : TGIS_LabelPositions ;

    /// <summary>
    ///   Bitmap size.
    /// </summary>
    BitmapSize      : TPoint            ;

    /// <summary>
    ///   Label texture.
    /// </summary>
    LabelTextureDX : IDirect3DTexture9 ;
  end;

  /// <summary>
  ///   Array of T_labelInfo.
  /// </summary>
  T_arLabelInfo = array of T_labelInfo ;

  /// <summary>
  ///   Structure storing necessary mesh tile parameters.
  /// </summary>
  T_texTileInfo = record

    /// <summary>
    ///   Mesh subset number.
    /// </summary>
    Subset : Integer ;

    /// <summary>
    ///   Subset texture.
    /// </summary>
    TexDX  : IDirect3DTexture9 ;

    /// <summary>
    ///   Transparency.
    /// </summary>
    Transp : Cardinal ;

    /// <summary>
    ///   Counter.
    /// </summary>
    Count : Cardinal ;
  end;

  /// <summary>
  ///   Array storing parameters of all mesh tiles.
  /// </summary>
  T_arMeshTileInfo = array of array of T_texTileInfo ;

type
  /// <summary>
  ///   3D DirectxX9 renderer.
  /// </summary>
  TGIS_Renderer3DDirectX9 = class ( TGIS_Renderer3DDirectXAbstract )

  {$IFNDEF CLR} private {$ELSE} unit {$ENDIF} //  private variables
    hWindow                : HWND   ;
    oldhWindow             : HWND   ;
    pD3D                   : IDirect3D9 ;       // Used to create the D3DDevice
    pD3DDevice             : IDirect3DDevice9 ; // Our  D3DDevice
    pdataVB                : IDirect3DVertexBuffer9 ; // Vertex Buffer data
    cdataVB                : IDirect3DVertexBuffer9 ; // Vertex Buffer data
    idataIB                : IDirect3DIndexBuffer9 ;  // Index Bffer data
    pdataTEX_d             : IDirect3DTexture9 ;      // Detail texture data
    pdataTEX_r             : IDirect3DTexture9 ;      // Rough texture data
    pdataTEX_w             : IDirect3DTexture9 ;      // Wall texture
    pdataTEX_vDX           : IDirect3DTexture9 ;      // Vector texture
    pdataTEX_tDX           : IDirect3DTexture9 ;      // TIN texture
    pdataTEX_bDX           : IDirect3DTexture9 ;      // Basement texture
    pdataTEX_sDX           : IDirect3DTexture9 ;      // SkyBox texture
    pdataTEX_sun           : IDirect3DTexture9 ;      // Sun texture
    labelInfo              : T_arLabelInfo     ;
    oMtrl                  : TD3DMaterial9     ;
    oLight                 : TD3DLight9        ;
    ambColorDX             : TD3DColor         ;
    backColor              : TD3DColor ;    // D3D window background color
    borderColor            : TD3DColor ;    // texture border color
    mpatchTileInfo         : T_arMeshTileInfo   ;
    meshTileInfoR          : T_arMeshTileInfo   ;
    meshTileInfoW          : T_arMeshTileInfo   ;
    curMeshTexR            : IDirect3DTexture9  ; //roof
    curMeshTexW            : IDirect3DTexture9  ; //wall
    mtxWorld               : TD3DXMatrix ;   // World D3DXMATRIX
    mtxView                : TD3DXMatrix ;   // View D3DXMATRIX
    mtxProjection          : TD3DXMatrix ;   // Projection D3DXMATRIX
    meshDemTiles           : T_arMesh           ;
    meshWallTiles          : T_arMesh           ;
    meshTinTilesC          : T_arMesh           ;
    meshTinTilesT          : T_arMesh           ;
    meshMPatchTilesC       : T_arMesh           ;
    meshMPatchTilesT       : T_arMesh           ;
    meshVectTilesCR        : T_arMesh           ;
    meshVectTilesCW        : T_arMesh           ;
    meshVectTilesTR        : T_arMesh           ;
    meshVectTilesTW        : T_arMesh           ;
    vPickRayOrig ,
    vPickRayDir            : TD3DXVector3       ;
    anisotropy_factor      : Cardinal           ;
    pixelSize3D            : Double             ;
    oldcutVectExtentV      : TGIS_Extent        ;
    printBmpAllInOne       : Boolean            ;
    arTexTinInfo           : array of IDirect3DTexture9 ;
    lasEx3D                : TGIS_Extent3D      ;
    lasCell                : Double             ;
    dist                   : Double             ;
    org                    : TGIS_Point3D       ;
    pntNo                  : Integer            ;


    protected
    /// <summary>
    ///   Release Textures in labelInfo and set it's length to 0.
    /// </summary>
    procedure releaseLabelInfo       ;

    /// <summary>
    ///   Set buffers length and create buffers.
    /// </summary>
    /// <returns>
    ///   S_OK if operation went successful
    /// </returns>
    function  setBuffers             : HResult ;

    /// <summary>
    ///   Create vertex buffer &amp; load texture if required.
    /// </summary>
    /// <param name="_vertexNum">
    ///   vertex buffer size
    /// </param>
    /// <param name="_vbuf">
    ///   created vertex buffer
    /// </param>
    /// <param name="_ibuf">
    ///   created index buffer
    /// </param>
    /// <returns>
    ///   S_OK if operation went successful
    /// </returns>
    function  createVertexBuffers    ( const _vertexNum : DWORD ;
                                         out _vbuf      : IDirect3DVertexBuffer9 ;
                                         out _ibuf      : IDirect3DIndexBuffer9
                                     ) : HResult;

    /// <summary>
    ///   Set Material parameters.
    /// </summary>
    procedure initMaterial           ;

    /// <summary>
    ///   Set Light parameters.
    /// </summary>
    procedure initLight              ;

    /// <summary>
    ///   Perform set of RenderState functions for color blending.
    /// </summary>
    procedure setTextureOperations   ;

    /// <summary>
    ///   Enable color blending if currently processed vector layer is
    ///   transparent.
    /// </summary>
    procedure setCVLTransparency     ;

    /// <summary>
    ///   Load texture from given Memory Stream.
    /// </summary>
    /// <param name="_bmp">
    ///   MemoryStream to be used to create texture
    /// </param>
    /// <param name="_detail">
    ///   True - detail texture, False - rough texture
    /// </param>
    /// <returns>
    ///   S_OK if operation went successful
    /// </returns>
    function  loadTexture            ( const _bmp    : TGIS_Bitmap ;
                                       const _detail : Boolean
                                     ) : HResult ;

    /// <summary>
    ///   Draw all previously stored DEM mesh tiles.
    /// </summary>
    procedure drawMeshTiles          ;

    /// <summary>
    ///   Draw all previously stored vector mesh tiles.
    /// </summary>
    procedure drawVectTiles          ;

    /// <summary>
    ///   Draw all previously stored mesh Line tiles.
    /// </summary>
    procedure drawMeshLinTiles       ;

    /// <summary>
    ///   Draw all previously stored mesh Point tiles.
    /// </summary>
    procedure drawMeshPntTiles       ;

    /// <summary>
    ///   Draw all previously stored MultiPatch tiles.
    /// </summary>
    procedure drawMPatchTiles        ;

    /// <summary>
    ///   Shape cache loader, callback procedure.
    /// </summary>
    /// <param name="_shp">
    ///   shape
    /// </param>
    /// <param name="_abort">
    ///   True if processing failed
    /// </param>
    procedure addShapeToCache        (       _shp   : TGIS_Shape  ;
                                       var   _abort : Boolean
                                     ) ;

    /// <summary>
    ///   Get label as texture.
    /// </summary>
    /// <param name="_org">
    ///   origin point in screen coordinates of orthographic view
    /// </param>
    /// <param name="_shp">
    ///   shape object
    /// </param>
    /// <param name="_shp_lt">
    ///   simplified (lite) object to receive TextureLabel
    /// </param>
    procedure prepareLabelTexture    ( const _org    : TPoint     ;
                                       const _shp    : TGIS_Shape ;
                                       const _shp_lt : TObject
                                     ) ; override;

    /// <summary>
    ///   Get screen coordinates of map point at Z = 0.
    /// </summary>
    /// <param name="_ptg">
    ///   map point position
    /// </param>
    /// <returns>
    ///   point in screen coordinates
    /// </returns>
    function  mapToScreen            ( const _ptg : TGIS_Point
                                     ) : TPoint ;

    /// <summary>
    ///   Draw MultiPatch shape.
    /// </summary>
    /// <param name="_shp">
    ///   MultiPatch shape to be drawn
    /// </param>
    /// <param name="_lbz">
    ///   Label Z level
    /// </param>
    /// <param name="_lbb">
    ///   Label base level
    /// </param>
    /// <returns>
    ///   True if operation went successful
    /// </returns>
    function  drawMultiPatchFromCache
                                     ( const _shp   : TGIS_Shape ;
                                       var   _lbz   : Double     ;
                                       var   _lbb   : Double
                                     ) : Boolean ;

    /// <summary>
    ///   Save and Draw textured triangle list from buffer.
    /// </summary>
    /// <param name="_part">
    ///   True if part
    /// </param>
    /// <param name="_buf">
    ///   buffer
    /// </param>
    /// <returns>
    ///   S_OK if operation went successful
    /// </returns>
    function  saveVTLBufT            ( const _part : Boolean       ;
                                       var   _buf  : TGIS_Renderer3DTextureTriangleListBuffer
                                     ) : HResult ;

    /// <summary>
    ///   Create &amp; Add single mesh tile for color Vector (from triangleList).
    /// </summary>
    /// <param name="_count">
    ///   count
    /// </param>
    /// <param name="_mesh">
    ///   mesh object
    /// </param>
    /// <param name="_buf">
    ///   buffer
    /// </param>
    /// <param name="_tri">
    ///   number of triangles to be stored as faces
    /// </param>
    /// <returns>
    ///   S_OK if operation went successful
    /// </returns>
    function  addVectMeshTileC       ( var   _count   : Integer              ;
                                       var   _mesh    : T_arMesh             ;
                                       var   _buf     : TGIS_Renderer3DColorTriangleListBuffer ;
                                       const _tri     : DWORD
                                     ) : HResult ;

    /// <summary>
    ///   Create &amp; Add single mesh tile for textured Vector (from
    ///   triangleList).
    /// </summary>
    /// <param name="_count">
    ///   count
    /// </param>
    /// <param name="_mesh">
    ///   mesh object
    /// </param>
    /// <param name="_buf">
    ///   buffer
    /// </param>
    /// <param name="_tri">
    ///   number of triangles to be stored as faces
    /// </param>
    /// <returns>
    ///   S_OK if operation went successful
    /// </returns>
    function  addVectMeshTileT       ( var   _count   : Integer              ;
                                       var   _mesh    : T_arMesh             ;
                                       var   _buf     : TGIS_Renderer3DTextureTriangleListBuffer;
                                       const _tri     : DWORD
                                     ) : HResult ;

    /// <summary>
    ///   Draw label from buffer.
    /// </summary>
    /// <param name="_k">
    ///   label number in labelInfo array
    /// </param>
    procedure drawLabel              ( const _k     : Integer
                                     ) ;

    /// <summary>
    ///   Add textured triangle to triangle list buffer.
    /// </summary>
    /// <param name="_ptg1">
    ///   first triangle point
    /// </param>
    /// <param name="_ptg2">
    ///   second triangle point
    /// </param>
    /// <param name="_ptg3">
    ///   third triangle point
    /// </param>
    /// <param name="_subs">
    ///   subset
    /// </param>
    procedure addToMPatchBufT        ( const _ptg1 : TGIS_Renderer3DVertex ;
                                       const _ptg2 : TGIS_Renderer3DVertex ;
                                       const _ptg3 : TGIS_Renderer3DVertex ;
                                       const _subs : Integer
                                     ) ;

    /// <summary>
    ///   Draw textured triangle list from buffer.
    /// </summary>
    /// <returns>
    ///   S_OK if operation went successful
    /// </returns>
    function  saveMPatchBufT         : HResult ;

    /// <summary>
    ///   Release all used interfaces
    /// </summary>
    procedure releaseAllInterfaces   ;

    {$IFDEF CLR}
      protected
        /// <inheritdoc/>
        function pdataVB_Unlock      ( var _strm : DirectXStream   ;
                                       var _data : T_arTextureData
                                     ) : Integer ; override;

        /// <inheritdoc/>
        function cdataVB_Unlock      ( var _strm : DirectXStream   ;
                                       var _data : T_arColorData
                                     ) : Integer ; override;
    {$ELSE}
      protected
        /// <inheritdoc/>
        function pdataVB_Unlock      ( var _strm : PByte        ;
                                       var _data : T_arTextureData
                                     ) : Integer ; override;

        /// <inheritdoc/>
        function cdataVB_Unlock      ( var _strm : PByte        ;
                                       var _data : T_arColorData
                                     ) : Integer ; override;
    {$ENDIF}

    {$IFDEF CLR}
      protected
        /// <inheritdoc/>
        function pdataVB_Lock        ( var _strm : DirectXStream
                                     ) : Integer ; override;
        /// <inheritdoc/>
        function cdataVB_Lock        ( var _strm : DirectXStream
                                     ) : Integer ; override;
    {$ELSE}
      protected
        /// <inheritdoc/>
        function pdataVB_Lock        ( var _strm : PByte
                                     ) : Integer ; override;

        /// <inheritdoc/>
        function cdataVB_Lock        (  var _strm : PByte
                                     ) : Integer ; override;
    {$ENDIF}

  protected

    /// <inheritdoc/>
    procedure doDestroy              ; override;

    /// <inheritdoc/>
    procedure prepareVertexBuffersToDraw ; override;

    /// <inheritdoc/>
    procedure reInit3D               ; override;

    /// <inheritdoc/>
    procedure blitToWindow           ; override;

    /// <inheritdoc/>
    procedure releaseGeometry        ; override;

    /// <inheritdoc/>
    procedure setAmbientLightColor   ( const _val  : Integer
                                     ) ; override;

    /// <inheritdoc/>
    procedure releaseDataVB          ; override;

    /// <summary>
    ///   Prepare DEM as Mesh for drawing.
    /// </summary>
    /// <param name="_lp">
    ///   layer containing DEM as Mesh
    /// </param>
    procedure prepareMesh            ( const _lp : TGIS_LayerPixel ) ;

    /// <inheritdoc/>
    function  draw3DVector           : Integer ; override;

    /// <inheritdoc/>
    procedure drawLabels             ; override;

    /// <inheritdoc/>
    function  northArrow             ( const _value : Boolean
                                     ) : Integer ; override;

    /// <inheritdoc/>
    function  doRender               ( const _rows     : DWORD ;
                                       const _detail   : Boolean
                                     ) : Integer ; override;

    /// <inheritdoc/>
    function  saveWTLBufT            : Integer ; override;

    /// <inheritdoc/>
    function  saveVTLBufC            ( const _part : Boolean       ;
                                       var   _buf  : TGIS_Renderer3DColorTriangleListBuffer
                                     ) : Integer ; override;

    /// <inheritdoc/>
    function  setD3D_Data            : Integer ; override;

    /// <inheritdoc/>
    procedure freeMeshStore          ; override;

    /// <inheritdoc/>
    procedure makeWallTexture        ; override;

    /// <inheritdoc/>
    function  transformSetting       : Integer ; override;

    /// <inheritdoc/>
    procedure makeBasementTex        ; override;

    /// <inheritdoc/>
    procedure drawBasement           ; override;

    /// <inheritdoc/>
    procedure makeSkyBoxTex          ; override;

    /// <inheritdoc/>
    procedure drawSkyBox             ; override;

    /// <inheritdoc/>
    function  tvValue                ( const _row : Integer
                                     ) : Single ; override;

    /// <inheritdoc/>
    procedure detailTexture          ( var   _pardata : T_arTextureData ;
                                       const _k       : Integer         ;
                                       const _kk      : Integer         ;
                                       const _jj      : Integer
                                     ) ; override;

    /// <inheritdoc/>
    function  addDemMeshTile         ( var   _pardata : T_arTextureData   ;
                                       const _first   : Integer           ;
                                       const _tri     : Integer
                                     ) : Integer ; override;

    /// <inheritdoc/>
    function  addWallMeshTile        ( const _tri     : Integer
                                     ) : Integer ; override;

    /// <inheritdoc/>
    function  getDemBufSize          ( const _row : Integer
                                     ) : Integer ; override;

    /// <inheritdoc/>
    function  getWallBufSize         ( const _row : Integer
                                     ) : Integer ; override;

    /// <inheritdoc/>
    procedure lightOn                ; override;

    /// <inheritdoc/>
    procedure lightOff               ; override;

    /// <inheritdoc/>
    procedure reset3DEnvironment     ; override;

    /// <inheritdoc/>
    function  restore3DEnvironment   : Integer ; override;

    /// <inheritdoc/>
    function  triangleNormal         ( const _v0 : TVector3f ;
                                       const _v1 : TVector3f ;
                                       const _v2 : TVector3f
                                     ) : TVector3f ; override;

    /// <inheritdoc/>
    procedure adjustTransformSetting ( _value : Boolean
                                     ) ; override;

    /// <inheritdoc/>
    function  screenTo3D             ( const _pt : TPoint
                                     ) : TGIS_Point ; override;

    /// <inheritdoc/>
    function  mapToScreen2           ( const _ptg : TGIS_Point ;
                                       const _z   : Double
                                     ) : TPoint ; override;

    /// <inheritdoc/>
    procedure clearScreen            ; override;

    /// <inheritdoc/>
    procedure blendOn                ; override;

    /// <inheritdoc/>
    procedure blendOff               ; override;

    /// <inheritdoc/>
    function  saveTinBufT            : Integer ; override;

    /// <inheritdoc/>
    function  addTinMeshTileC        ( const _tri     : Integer
                                     ) : Integer ; override;

    /// <inheritdoc/>
    function  addTinMeshTileT        ( const _tri     : Integer
                                     ) : Integer ; override;

    /// <summary>
    ///   Draw all previously stored mesh TIN tiles.
    /// </summary>
    procedure drawMeshTinTiles       ;

    /// <inheritdoc/>
    procedure scaleMeshTilesZ        ( const _value : Double
                                     ) ; override;

    /// <inheritdoc/>
    procedure scaleMeshTilesM        ( const _value : Double
                                     ) ; override;

    /// <inheritdoc/>
    function  drawVectorTexture      ( const _shp  : TObject           ;
                                       const _part : Boolean           ;
                                       const _data : T_arColorData     ;
                                       const _prim : Integer           ;
                                       const _num  : Integer           ;
                                       const _len  : Double
                                     ) : Integer ; override;

    /// <inheritdoc/>
    procedure setPolyRoofN           ( var   _ver1 : TGIS_Renderer3DVertex ;
                                       var   _ver2 : TGIS_Renderer3DVertex ;
                                       var   _ver3 : TGIS_Renderer3DVertex ;
                                       const _wind : Boolean
                                     ) ; override;

    /// <inheritdoc/>
    procedure addToVTLBufT           ( const _part : Boolean              ;
                                       var   _buf  : TGIS_Renderer3DTextureTriangleListBuffer;
                                       const _ptg1 : TGIS_Renderer3DVertex      ;
                                       const _ptg2 : TGIS_Renderer3DVertex      ;
                                       const _ptg3 : TGIS_Renderer3DVertex
                                     ) ; override;

    /// <inheritdoc/>
    function  saveMPatchBufC         : Integer ; override;

    /// <inheritdoc/>
    procedure markSelShape           ( const _layer : TGIS_Layer ;
                                       const _shpid : Integer    ;
                                       const _part  : Integer    ;
                                       const _color : TGIS_Color ;
                                       const _mode  : Boolean    ;
                                       const _update: Boolean
                                     ) ; override;

    /// <summary>
    ///   Update dynamic layers only.
    /// </summary>
    /// <returns>
    ///   True on success.
    /// </returns>
    function  partialRender          : Boolean ;

    /// <inheritdoc/>
    function  getWindowSize          : TPoint ; override;

    /// <inheritdoc/>
    function  getTimeTic             : DWORD ; override;

  public //  constructors

    /// <inheritdoc/>
    constructor Create               ; override;

    /// <summary>
    ///   Set window handler.
    /// </summary>
    /// <param name="_hwnd">
    ///   current window handle
    /// </param>
    procedure SetHWND                ( const _hwnd : HWND
                                     ) ;

    /// <inheritdoc/>
    function  Init3D                 : Boolean ; override;

    /// <inheritdoc/>
    procedure SetSunPosition         ( const _value : TGIS_Point
                                     ) ; override;

    /// <inheritdoc/>
    procedure SetInitValues          ( const _cutextent     : TGIS_Extent ;
                                       const _visibleextent : TGIS_Extent ;
                                       const _gis           : IGIS_Viewer ;
                                       const _viewer        : IGIS_Viewer3D
                                     ) ; override;

    /// <inheritdoc/>
    function  Select3DObject         ( const _pt    : TPoint             ;
                                       const _prec  : Integer            ;
                                       var   _layer : TGIS_LayerAbstract ;
                                       var   _ptg   : TGIS_Point3D       ;
                                       var   _shp   : TGIS_ShapeAbstract ;
                                       var   _part  : Integer            ;
                                       const _mode  : Boolean
                                     ) : Boolean ; override;


    /// <inheritdoc/>
    function  RayIntersectDem        (  const _orig    : TGIS_Point3D ;
                                        const _dir     : TGIS_Point3D ;
                                        out   _ptg     : TGIS_Point3D
                                      ) : Boolean ; override;

    /// <inheritdoc/>
    function  ScreenToMap            ( const _pt    : TPoint
                                     ) : TGIS_Point ; override;

    /// <inheritdoc/>
    function  GetUniverseColor       : TGIS_Color ; override;

    /// <inheritdoc/>
    procedure SetUniverseColor       ( const _value         : TGIS_Color
                                     ) ; override;

    /// <inheritdoc/>
    function  Draw                   : Integer ; override;

    /// <inheritdoc/>
    procedure PrintBmp               ( const _rect  : TRect ;
                                       const _bmp   : TGIS_Bitmap
                                     ) ; override;

    /// <inheritdoc/>
    function  PrintBegin             ( const _width  : Integer ;
                                       const _height : Integer
                                     ) : TPoint ; override;

    /// <inheritdoc/>
    procedure PrintTile              ( const _bmp      : TGIS_Bitmap ;
                                       const _offsetx  : Integer ;
                                       const _offsety  : Integer
                                     ) ; override;

    /// <inheritdoc/>
    procedure PrintEnd               ; override;

  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisTypes3D,
    GisTessellation,
    GisShapes3D ;
{$ENDIF}

type

  TMesh = class ( TGIS_ObjectDisposable )
    {$IFNDEF CLR} private {$ELSE} unit {$ENDIF}
      device    : IDirect3DDevice9 ;
      format    : DWORD ;
      faces     : TArray<Integer> ;
      tvertices : TArray<TGIS_Renderer3DVertex> ;
      cvertices : TArray<TGIS_Renderer3DVertex> ;
      indices   : TArray<Integer> ;
      subsets   : TArray<DWORD> ;
      pdataVB,
      cdataVB   : IDirect3DVertexBuffer9 ;
      idataIB   : IDirect3DIndexBuffer9 ;
      bDraw     : Boolean ;

    protected
      procedure doDestroy              ; override;
      {$IFDEF CLR}
        function pdataVB_Lock          ( var _strm : DirectXStream
                                       ) : Integer ;
      {$ELSE}
        function pdataVB_Lock          ( var _strm : PByte
                                       ) : Integer ;
      {$ENDIF}
      {$IFDEF CLR}
        function cdataVB_Lock          (  var _strm : DirectXStream
                                       ) : Integer ;
      {$ELSE}
        function cdataVB_Lock          (  var _strm : PByte
                                       ) : Integer ;
      {$ENDIF}
      {$IFDEF CLR}
        function pdataVB_Unlock          ( var _strm : DirectXStream ;
                                           var _data : T_arTextureData
                                         ) : Integer ;
      {$ELSE}
        function pdataVB_Unlock          ( var _strm : PByte        ;
                                           var _data : T_arTextureData
                                         ) : Integer ;
      {$ENDIF}
      {$IFDEF CLR}
        function cdataVB_Unlock          ( var _strm : DirectXStream ;
                                           var _data : T_arColorData
                                         ) : Integer ;
      {$ELSE}
        function cdataVB_Unlock          ( var _strm : PByte        ;
                                           var _data : T_arColorData
                                         ) : Integer ;
      {$ENDIF}
    public
      constructor Create               ( const _faces    : DWORD ;
                                         const _vertices : DWORD ;
                                         {$IFDEF CLR}
                                           const _format : D3DFVF ;
                                         {$ELSE}
                                           const _format   : DWORD ;
                                         {$ENDIF}
                                         const _device   : IDirect3DDevice9 ;
                                         const _pvb      : IDirect3DVertexBuffer9 ;
                                         const _cvb      : IDirect3DVertexBuffer9 ;
                                         const _iib      : IDirect3DIndexBuffer9
                                       ) ;

      function  GetNumFaces            : DWORD ;
      function  GetNumVertices         : DWORD ;
      procedure SortIndices            ( _num      : DWORD
                                       ) ;
      function  DrawSubset             ( AttribId   : DWORD
                                       ) : Integer ;
      procedure UpdateBuffer           ;
  end;


var
  maxTexSize : Integer         ;
  oLabelsReg : TGIS_LabelsArea ;

  function LabelsReg : TGIS_LabelsArea ;
  begin
    if not assigned( oLabelsReg ) then
      oLabelsReg := TGIS_LabelsArea.Create( nil ) ;

    Result := oLabelsReg ;
  end ;

  {$IFDEF CLR}
  function CreateMeshFVF(
    NumFaces    : DWORD              ;
    NumVertices : DWORD              ;
    Options     : D3DXMESH           ;
    FVF         : D3DFVF             ;
    pD3DDevice  : IDirect3DDevice9   ;
    out ppMesh  : TMesh              ;
    pvb         : IDirect3DVertexBuffer9 ;
    cvb         : IDirect3DVertexBuffer9 ;
    iib         : IDirect3DIndexBuffer9
  ) : Integer ;
  {$ELSE}
    function CreateMeshFVF(
      NumFaces    : DWORD              ;
      NumVertices : DWORD              ;
      Options     : DWORD              ;
      FVF         : DWORD              ;
      pD3DDevice  : IDirect3DDevice9   ;
      out ppMesh  : TMesh              ;
      pvb         : IDirect3DVertexBuffer9 ;
      cvb         : IDirect3DVertexBuffer9 ;
      iib         : IDirect3DIndexBuffer9
  ) : Integer ;
  {$ENDIF}
  begin
    ppMesh := TMesh.Create( NumFaces, NumVertices, FVF,
                            pD3DDevice, pvb, cvb, iib ) ;
    if assigned( ppMesh ) then
      Result := S_OK
    else
      Result := S_FALSE ;
  end;

  function Vec3Add(
    out   vOut : TD3DVector;
    const v1   : TD3DVector ;
    const v2   : TD3DVector
  ): PD3DVector;
  begin
    vOut.X:= v1.X + v2.X;
    vOut.Y:= v1.Y + v2.Y;
    vOut.Z:= v1.Z + v2.Z;
    {$IFDEF CLR}
      Result := vOut ;
    {$ELSE}
      Result := @vOut ;
    {$ENDIF}
  end;

  function Vec3Subtract(
    out   vOut : TD3DVector;
    const v1   : TD3DVector ;
    const v2   : TD3DVector
  ): PD3DVector; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  begin
    vOut.X:= v1.X - v2.X;
    vOut.Y:= v1.Y - v2.Y;
    vOut.Z:= v1.Z - v2.Z;
    {$IFDEF CLR}
      Result := vOut ;
    {$ELSE}
      Result := @vOut ;
    {$ENDIF}
  end;

  function Vec3Cross(
    out   vOut : TD3DVector;
    const v1   : TD3DVector ;
    const v2   : TD3DVector
  ): PD3DVector; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  begin
    vOut.X:= v1.Y * v2.Z - v1.Z * v2.Y;
    vOut.Y:= v1.Z * v2.X - v1.X * v2.Z;
    vOut.Z:= v1.X * v2.Y - v1.Y * v2.X;
    {$IFDEF CLR}
      Result := vOut ;
    {$ELSE}
      Result := @vOut ;
    {$ENDIF}
  end;

  function Vec3Dot(
    const v1, v2: TD3DXVector3
  ): Single;
  begin
    Result := v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z ;
  end;

  function Vec3Normalize(
    out   vOut        : TD3DVector         ;
    const v           : TD3DVector
  ): PD3DVector ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  var
    l : Double ;
  begin
    l := Sqrt(Sqr(v.X) + Sqr(v.Y) + Sqr(v.Z)) ;
    if l = 0 then begin
      vOut.Y := 1 ;
    end
    else begin
      vOut.X := vOut.X / l ;
      vOut.Y := vOut.Y / l ;
      vOut.Z := vOut.Z / l ;
    end ;
    {$IFDEF CLR}
      Result := vOut ;
    {$ELSE}
      Result := @vOut ;
    {$ENDIF}
  end;

  function MatrixRotationX(
    out   mOut         : TD3DMatrix        ;
          angle        : Single
  ) : PD3DMatrix ;
  var
    r : Single ;
  begin
    r := angle ; // * Pi / 180;

    {$IFDEF CLR}
      mOut.M11 := 1; mOut.M21 := 0;      mOut.M31 := 0;       mOut.M41 := 0;
      mOut.M12 := 0; mOut.M22 := Cos(r); mOut.M32 :=-Sin(r);  mOut.M42 := 0;
      mOut.M13 := 0; mOut.M23 := Sin(r); mOut.M33 := Cos(r);  mOut.M43 := 0;
      mOut.M14 := 0; mOut.M24 := 0;      mOut.M34 := 0;       mOut.M44 := 1;

      Result := mOut ;
    {$ELSE}
      mOut._11 := 1; mOut._21 := 0;      mOut._31 := 0;       mOut._41 := 0;
      mOut._12 := 0; mOut._22 := Cos(r); mOut._32 :=-Sin(r);  mOut._42 := 0;
      mOut._13 := 0; mOut._23 := Sin(r); mOut._33 := Cos(r);  mOut._43 := 0;
      mOut._14 := 0; mOut._24 := 0;      mOut._34 := 0;       mOut._44 := 1;

      Result := @mOut ;
    {$ENDIF}
  end;

  function MatrixRotationZ(
    out   mOut         : TD3DMatrix        ;
          angle        : Single
  ) : PD3DMatrix ;
  var
    r : Single ;
  begin
    r := angle ;

    {$IFDEF CLR}
      mOut.M11 := Cos(r); mOut.M21 :=-Sin(r); mOut.M31 := 0;  mOut.M41 := 0;
      mOut.M12 := Sin(r); mOut.M22 := Cos(r); mOut.M32 := 0;  mOut.M42 := 0;
      mOut.M13 := 0     ; mOut.M23 := 0     ; mOut.M33 := 1;  mOut.M43 := 0;
      mOut.M14 := 0     ; mOut.M24 := 0     ; mOut.M34 := 0;  mOut.M44 := 1;

      Result := mOut ;
    {$ELSE}
      mOut._11 := Cos(r); mOut._21 :=-Sin(r); mOut._31 := 0;  mOut._41 := 0;
      mOut._12 := Sin(r); mOut._22 := Cos(r); mOut._32 := 0;  mOut._42 := 0;
      mOut._13 := 0     ; mOut._23 := 0     ; mOut._33 := 1;  mOut._43 := 0;
      mOut._14 := 0     ; mOut._24 := 0     ; mOut._34 := 0;  mOut._44 := 1;

      Result := @mOut ;
    {$ENDIF}
  end;

  function MatrixTranslation(
    out   mOut        : TD3DMatrix         ;
          X           : Single             ;
          Y           : Single             ;
          Z           : Single
  ): PD3DMatrix ;
  begin
    {$IFDEF CLR}
      mOut.M11 := 1; mOut.M21 := 0; mOut.M31 := 0;  mOut.M41 := X;
      mOut.M12 := 0; mOut.M22 := 1; mOut.M32 := 0;  mOut.M42 := Y;
      mOut.M13 := 0; mOut.M23 := 0; mOut.M33 := 1;  mOut.M43 := Z;
      mOut.M14 := 0; mOut.M24 := 0; mOut.M34 := 0;  mOut.M44 := 1;

      Result := mOut ;
    {$ELSE}
      mOut._11 := 1; mOut._21 := 0; mOut._31 := 0;  mOut._41 := X;
      mOut._12 := 0; mOut._22 := 1; mOut._32 := 0;  mOut._42 := Y;
      mOut._13 := 0; mOut._23 := 0; mOut._33 := 1;  mOut._43 := Z;
      mOut._14 := 0; mOut._24 := 0; mOut._34 := 0;  mOut._44 := 1;

      Result := @mOut ;
    {$ENDIF}
  end;

  function MatrixScaling(
    out   mOut        : TD3DMatrix         ;
          sx          : Single             ;
          sy          : Single             ;
          sz          : Single
  ): PD3DMatrix;
  begin
    {$IFDEF CLR}
      mOut.M11 := sx; mOut.M21 := 0;  mOut.M31 := 0;   mOut.M41 := 0;
      mOut.M12 := 0;  mOut.M22 := sy; mOut.M32 := 0;   mOut.M42 := 0;
      mOut.M13 := 0;  mOut.M23 := 0;  mOut.M33 := sz;  mOut.M43 := 0;
      mOut.M14 := 0;  mOut.M24 := 0;  mOut.M34 := 0;   mOut.M44 := 1;

      Result := mOut ;
    {$ELSE}
      mOut._11 := sx; mOut._21 := 0;  mOut._31 := 0;   mOut._41 := 0;
      mOut._12 := 0;  mOut._22 := sy; mOut._32 := 0;   mOut._42 := 0;
      mOut._13 := 0;  mOut._23 := 0;  mOut._33 := sz;  mOut._43 := 0;
      mOut._14 := 0;  mOut._24 := 0;  mOut._34 := 0;   mOut._44 := 1;

      Result := @mOut ;
    {$ENDIF}
  end;

  function MatrixMultiply(
    out   mOut        : TD3DMatrix         ;
    const m1          : TD3DMatrix         ;
    const m2          : TD3DMatrix
  ): PD3DMatrix;
  var
    m : TD3DMatrix ;
    {$IFNDEF CLR}
    i, j : Integer ;
    {$ENDIF}
  begin
    {$IFDEF CLR}
      m := TD3DMatrix.Multiply( m1, m2 ) ;
    {$ELSE}
      for i := 0 to 3 do
        for j := 0 to 3 do
           m.m[i][j] := m1.m[i][0] * m2.m[0][j] + m1.m[i][1] * m2.m[1][j] + m1.m[i][2] * m2.m[2][j] + m1.m[i][3] * m2.m[3][j] ;
    {$ENDIF}

    {$IFDEF CLR}
      mOut.M11 := m.M11 ; mOut.M12 := m.M12 ; mOut.M13 := m.M13 ; mOut.M14 := m.M14 ;
      mOut.M21 := m.M21 ; mOut.M22 := m.M22 ; mOut.M23 := m.M23 ; mOut.M24 := m.M24 ;
      mOut.M31 := m.M31 ; mOut.M32 := m.M32 ; mOut.M33 := m.M33 ; mOut.M34 := m.M34 ;
      mOut.M41 := m.M41 ; mOut.M42 := m.M42 ; mOut.M43 := m.M43 ; mOut.M44 := m.M44 ;

      Result := mOut ;
    {$ELSE}
      mOut._11 := m._11 ; mOut._12 := m._12 ; mOut._13 := m._13 ; mOut._14 := m._14 ;
      mOut._21 := m._21 ; mOut._22 := m._22 ; mOut._23 := m._23 ; mOut._24 := m._24 ;
      mOut._31 := m._31 ; mOut._32 := m._32 ; mOut._33 := m._33 ; mOut._34 := m._34 ;
      mOut._41 := m._41 ; mOut._42 := m._42 ; mOut._43 := m._43 ; mOut._44 := m._44 ;

      Result := @mOut ;
    {$ENDIF}
  end;

  {$IFDEF CLR}
    function MatrixInverse(
      out   mOut    : TD3DXMatrix ;
      pfDeterminant : TObject     ;
      const m       : TD3DXMatrix
    ) : PD3DMatrix ;
    var
      inv : TD3DXMatrix ;
      det : Single ;
    begin
      mOut.M11 := 1;  mOut.M21 := 0;  mOut.M31 := 0;   mOut.M41 := 0;
      mOut.M12 := 0;  mOut.M22 := 1;  mOut.M32 := 0;   mOut.M42 := 0;
      mOut.M13 := 0;  mOut.M23 := 0;  mOut.M33 := 1;   mOut.M43 := 0;
      mOut.M14 := 0;  mOut.M24 := 0;  mOut.M34 := 0;   mOut.M44 := 1;

      inv.M11 :=   m.M22*m.M33*m.M44 - m.M22*m.M34*m.M43 - m.M32*m.M23*m.M44
      + m.M32*m.M24*m.M43  + m.M42*m.M23*m.M34 - m.M42*m.M24*m.M33 ;
      inv.M21 := - m.M21*m.M33*m.M44 + m.M21*m.M34*m.M43 + m.M31*m.M23*m.M44
      - m.M31*m.M24*m.M43  - m.M41*m.M23*m.M34 + m.M41*m.M24*m.M33;
      inv.M31 :=   m.M21*m.M32*m.M44  - m.M21*m.M34*m.M42 - m.M31*m.M22*m.M44
      + m.M31*m.M24*m.M42  + m.M41*m.M22*m.M34 - m.M41*m.M24*m.M32;
      inv.M41 := -m.M21*m.M32*m.M43  + m.M21*m.M33*m.M42 + m.M31*m.M22*m.M43
      - m.M31*m.M23*m.M42  - m.M41*m.M22*m.M33 + m.M41*m.M23*m.M32;
      inv.M12 :=  -m.M12*m.M33*m.M44 + m.M12*m.M34*m.M43 + m.M32*m.M13*m.M44
      - m.M32*m.M14*m.M43  - m.M42*m.M13*m.M34 + m.M42*m.M14*m.M33;
      inv.M22 :=   m.M11*m.M33*m.M44 - m.M11*m.M34*m.M43 - m.M31*m.M13*m.M44
      + m.M31*m.M14*m.M43  + m.M41*m.M13*m.M34 - m.M41*m.M14*m.M33;
      inv.M32 :=  -m.M11*m.M32*m.M44  + m.M11*m.M34*m.M42 + m.M31*m.M12*m.M44
      - m.M31*m.M14*m.M42  - m.M41*m.M12*m.M34 + m.M41*m.M14*m.M32;
      inv.M42 :=  m.M11*m.M32*m.M43  - m.M11*m.M33*m.M42 - m.M31*m.M12*m.M43
      + m.M31*m.M13*m.M42  + m.M41*m.M12*m.M33 - m.M41*m.M13*m.M32;
      inv.M13 :=   m.M12*m.M23*m.M44  - m.M12*m.M24*m.M43  - m.M22*m.M13*m.M44
      + m.M22*m.M14*m.M43  + m.M42*m.M13*m.M24  - m.M42*m.M14*m.M23;
      inv.M23 :=  -m.M11*m.M23*m.M44  + m.M11*m.M24*m.M43  + m.M21*m.M13*m.M44
      - m.M21*m.M14*m.M43  - m.M41*m.M13*m.M24  + m.M41*m.M14*m.M23;
      inv.M33 :=  m.M11*m.M22*m.M44  - m.M11*m.M24*m.M42  - m.M21*m.M12*m.M44
      + m.M21*m.M14*m.M42  + m.M41*m.M12*m.M24  - m.M41*m.M14*m.M22;
      inv.M43 := -m.M11*m.M22*m.M43  + m.M11*m.M23*m.M42  + m.M21*m.M12*m.M43
      - m.M21*m.M13*m.M42  - m.M41*m.M12*m.M23  + m.M41*m.M13*m.M22;
      inv.M14 :=  -m.M12*m.M23*m.M34  + m.M12*m.M24*m.M33  + m.M22*m.M13*m.M34
      - m.M22*m.M14*m.M33  - m.M32*m.M13*m.M24   + m.M32*m.M14*m.M23;
      inv.M24 :=   m.M11*m.M23*m.M34  - m.M11*m.M24*m.M33  - m.M21*m.M13*m.M34
      + m.M21*m.M14*m.M33  + m.M31*m.M13*m.M24   - m.M31*m.M14*m.M23;
      inv.M34 := -m.M11*m.M22*m.M34  + m.M11*m.M24*m.M32   + m.M21*m.M12*m.M34
      - m.M21*m.M14*m.M32   - m.M31*m.M12*m.M24   + m.M31*m.M14*m.M22;
      inv.M44 :=  m.M11*m.M22*m.M33  - m.M11*m.M23*m.M32   - m.M21*m.M12*m.M33
      + m.M21*m.M13*m.M32   + m.M31*m.M12*m.M23   - m.M31*m.M13*m.M22;

      det := m.M11*inv.M11 + m.M12*inv.M21 + m.M13*inv.M31 + m.M14*inv.M41;

      if det = 0 then begin
        Result := mOut ;
        exit;
      end;

      det := 1.0 / det ;

      mOut.M11 := inv.M11*det;  mOut.M21 := inv.M21*det;  mOut.M31 := inv.M31*det;   mOut.M41 := inv.M41*det;
      mOut.M12 := inv.M12*det;  mOut.M22 := inv.M22*det;  mOut.M32 := inv.M32*det;   mOut.M42 := inv.M42*det;
      mOut.M13 := inv.M13*det;  mOut.M23 := inv.M23*det;  mOut.M33 := inv.M33*det;   mOut.M43 := inv.M43*det;
      mOut.M14 := inv.M14*det;  mOut.M24 := inv.M24*det;  mOut.M34 := inv.M34*det;   mOut.M44 := inv.M44*det;

      Result := mOut ;
    end ;
  {$ELSE}
    function MatrixInverse(
      out   mOut    : TD3DXMatrix ;
      pfDeterminant : PSingle     ;
      const m       : TD3DXMatrix
    ) : PD3DMatrix ;
    var
      inv : TD3DXMatrix ;
      det : Single ;
    begin
      mOut._11 := 1;  mOut._21 := 0;  mOut._31 := 0;   mOut._41 := 0;
      mOut._12 := 0;  mOut._22 := 1;  mOut._32 := 0;   mOut._42 := 0;
      mOut._13 := 0;  mOut._23 := 0;  mOut._33 := 1;   mOut._43 := 0;
      mOut._14 := 0;  mOut._24 := 0;  mOut._34 := 0;   mOut._44 := 1;

      inv._11 :=   m._22*m._33*m._44 - m._22*m._34*m._43 - m._32*m._23*m._44
                + m._32*m._24*m._43  + m._42*m._23*m._34 - m._42*m._24*m._33 ;
      inv._21 := - m._21*m._33*m._44 + m._21*m._34*m._43 + m._31*m._23*m._44
                - m._31*m._24*m._43  - m._41*m._23*m._34 + m._41*m._24*m._33;
      inv._31 :=   m._21*m._32*m._44  - m._21*m._34*m._42 - m._31*m._22*m._44
                + m._31*m._24*m._42  + m._41*m._22*m._34 - m._41*m._24*m._32;
      inv._41 := -m._21*m._32*m._43  + m._21*m._33*m._42 + m._31*m._22*m._43
                - m._31*m._23*m._42  - m._41*m._22*m._33 + m._41*m._23*m._32;
      inv._12 :=  -m._12*m._33*m._44 + m._12*m._34*m._43 + m._32*m._13*m._44
                - m._32*m._14*m._43  - m._42*m._13*m._34 + m._42*m._14*m._33;
      inv._22 :=   m._11*m._33*m._44 - m._11*m._34*m._43 - m._31*m._13*m._44
                + m._31*m._14*m._43  + m._41*m._13*m._34 - m._41*m._14*m._33;
      inv._32 :=  -m._11*m._32*m._44  + m._11*m._34*m._42 + m._31*m._12*m._44
                - m._31*m._14*m._42  - m._41*m._12*m._34 + m._41*m._14*m._32;
      inv._42 :=  m._11*m._32*m._43  - m._11*m._33*m._42 - m._31*m._12*m._43
                + m._31*m._13*m._42  + m._41*m._12*m._33 - m._41*m._13*m._32;
      inv._13 :=   m._12*m._23*m._44  - m._12*m._24*m._43  - m._22*m._13*m._44
                + m._22*m._14*m._43  + m._42*m._13*m._24  - m._42*m._14*m._23;
      inv._23 :=  -m._11*m._23*m._44  + m._11*m._24*m._43  + m._21*m._13*m._44
                - m._21*m._14*m._43  - m._41*m._13*m._24  + m._41*m._14*m._23;
      inv._33 :=  m._11*m._22*m._44  - m._11*m._24*m._42  - m._21*m._12*m._44
                + m._21*m._14*m._42  + m._41*m._12*m._24  - m._41*m._14*m._22;
      inv._43 := -m._11*m._22*m._43  + m._11*m._23*m._42  + m._21*m._12*m._43
                - m._21*m._13*m._42  - m._41*m._12*m._23  + m._41*m._13*m._22;
      inv._14 :=  -m._12*m._23*m._34  + m._12*m._24*m._33  + m._22*m._13*m._34
                - m._22*m._14*m._33  - m._32*m._13*m._24   + m._32*m._14*m._23;
      inv._24 :=   m._11*m._23*m._34  - m._11*m._24*m._33  - m._21*m._13*m._34
                + m._21*m._14*m._33  + m._31*m._13*m._24   - m._31*m._14*m._23;
      inv._34 := -m._11*m._22*m._34  + m._11*m._24*m._32   + m._21*m._12*m._34
                - m._21*m._14*m._32   - m._31*m._12*m._24   + m._31*m._14*m._22;
      inv._44 :=  m._11*m._22*m._33  - m._11*m._23*m._32   - m._21*m._12*m._33
                + m._21*m._13*m._32   + m._31*m._12*m._23   - m._31*m._13*m._22;

      det := m._11*inv._11 + m._12*inv._21 + m._13*inv._31 + m._14*inv._41;

      if det = 0 then begin
          Result := @mOut ;
          exit;
      end;

      det := 1.0 / det ;

      mOut._11 := inv._11*det;  mOut._21 := inv._21*det;  mOut._31 := inv._31*det;   mOut._41 := inv._41*det;
      mOut._12 := inv._12*det;  mOut._22 := inv._22*det;  mOut._32 := inv._32*det;   mOut._42 := inv._42*det;
      mOut._13 := inv._13*det;  mOut._23 := inv._23*det;  mOut._33 := inv._33*det;   mOut._43 := inv._43*det;
      mOut._14 := inv._14*det;  mOut._24 := inv._24*det;  mOut._34 := inv._34*det;   mOut._44 := inv._44*det;


      Result := @mOut ;
    end ;
  {$ENDIF}

  function MatrixRotationYawPitchRoll(
    out   mOut        : TD3DMatrix         ;
          yaw         : Single             ;
          pitch       : Single             ;
          roll        : Single
  ): PD3DMatrix ;
  begin
    {$IFDEF CLR}
      mOut.M11 := 1;  mOut.M21 := 0;  mOut.M31 := 0;   mOut.M41 := 0;
      mOut.M12 := 0;  mOut.M22 := 1;  mOut.M32 := 0;   mOut.M42 := 0;
      mOut.M13 := 0;  mOut.M23 := 0;  mOut.M33 := 1;   mOut.M43 := 0;
      mOut.M14 := 0;  mOut.M24 := 0;  mOut.M34 := 0;   mOut.M44 := 1;
      mOut.M11 := ( Cos(roll) * Cos(yaw) ) + ( Sin(roll) * Sin(pitch) * Sin(yaw) );
      mOut.M12 := ( Sin(roll) * Cos(pitch) );
      mOut.M13 := ( Cos(roll) * -Sin(yaw) ) + ( Sin(roll) * Sin(pitch) * Cos(yaw) );
      mOut.M21 := (-Sin(roll) * Cos(yaw) ) + ( Cos(roll) * Sin(pitch) * Sin(yaw) );
      mOut.M22 := ( Cos(roll) * Cos(pitch) );
      mOut.M23 := ( Sin(roll) * Sin(yaw) ) + ( Cos(roll) * Sin(pitch) * Cos(yaw) );
      mOut.M31 := ( Cos(pitch) * Sin(yaw) );
      mOut.M32 := -Sin(pitch);
      mOut.M33 := ( Cos(pitch) * Cos(yaw) );
      mOut.M44 := 1;

      Result := mOut ;
    {$ELSE}
      mOut._11 := 1;  mOut._21 := 0;  mOut._31 := 0;   mOut._41 := 0;
      mOut._12 := 0;  mOut._22 := 1;  mOut._32 := 0;   mOut._42 := 0;
      mOut._13 := 0;  mOut._23 := 0;  mOut._33 := 1;   mOut._43 := 0;
      mOut._14 := 0;  mOut._24 := 0;  mOut._34 := 0;   mOut._44 := 1;
      mOut._11 := ( Cos(roll) * Cos(yaw) ) + ( Sin(roll) * Sin(pitch) * Sin(yaw) );
      mOut._12 := ( Sin(roll) * Cos(pitch) );
      mOut._13 := ( Cos(roll) * -Sin(yaw) ) + ( Sin(roll) * Sin(pitch) * Cos(yaw) );
      mOut._21 := (-Sin(roll) * Cos(yaw) ) + ( Cos(roll) * Sin(pitch) * Sin(yaw) );
      mOut._22 := ( Cos(roll) * Cos(pitch) );
      mOut._23 := ( Sin(roll) * Sin(yaw) ) + ( Cos(roll) * Sin(pitch) * Cos(yaw) );
      mOut._31 := ( Cos(pitch) * Sin(yaw) );
      mOut._32 := -Sin(pitch);
      mOut._33 := ( Cos(pitch) * Cos(yaw) );
      mOut._44 := 1;

      Result := @mOut ;
    {$ENDIF}
  end;

  function MatrixPerspectiveFovLH(
    out   mOut        : TD3DMatrix         ;
          flovy       : Single             ;
          aspect      : Single             ;
          zn          : Single             ;
          zf          : Single
  ): PD3DMatrix;
  var
    xs, ys : Single ;
  begin
    ys := 1/Tan(flovy/2) ;
    xs := ys / aspect ;

    {$IFDEF CLR}
      mOut.M11 := xs ;
      mOut.M21 := 0 ;
      mOut.M31 := 0 ;
      mOut.M41 := 0 ;

      mOut.M12 := 0 ;
      mOut.M22 := ys ;
      mOut.M32 := 0 ;
      mOut.M42 := 0 ;

      mOut.M13 := 0 ;
      mOut.M23 := 0 ;
      mOut.M33 :=  zf / (zf-zn) ;
      mOut.M43 := -zn*zf/(zf-zn) ;

      mOut.M14 := 0 ;
      mOut.M24 := 0 ;
      mOut.M34 := 1 ;
      mOut.M44 := 0 ;

      Result := mOut ;
    {$ELSE}
      mOut._11 := xs ;
      mOut._21 := 0 ;
      mOut._31 := 0 ;
      mOut._41 := 0 ;

      mOut._12 := 0 ;
      mOut._22 := ys ;
      mOut._32 := 0 ;
      mOut._42 := 0 ;

      mOut._13 := 0 ;
      mOut._23 := 0 ;
      mOut._33 :=  zf / (zf-zn) ;
      mOut._43 := -zn*zf/(zf-zn) ;

      mOut._14 := 0 ;
      mOut._24 := 0 ;
      mOut._34 := 1 ;
      mOut._44 := 0 ;

      Result := @mOut ;
    {$ENDIF}
  end;

  function MatrixLookAtLH(
    out   mOut        : TD3DMatrix         ;
    const Eye         : TD3DVector         ;
    const At          : TD3DVector         ;
    const Up          : TD3DVector
  ): PD3DMatrix ;
  var
    v, u, r : TD3DVector ;
  begin
    Vec3Subtract(v, At, Eye) ;
    Vec3Normalize(v, v) ;
    Vec3Cross(r, Up, v) ;
    Vec3Cross(u, v, r) ;
    Vec3Normalize(r, r) ;
    Vec3Normalize(u, u) ;

    {$IFDEF CLR}
      mOut.M11 :=  r.X ;
      mOut.M21 :=  r.Y ;
      mOut.M31 :=  r.Z ;
      mOut.M41 := -Vec3Dot(r, Eye) ;

      mOut.M12 :=  u.X ;
      mOut.M22 :=  u.Y ;
      mOut.M32 :=  u.Z ;
      mOut.M42 := -Vec3Dot(u, Eye) ;

      mOut.M13 :=  v.X ;
      mOut.M23 :=  v.Y ;
      mOut.M33 :=  v.Z ;
      mOut.M43 := -Vec3Dot(v, Eye) ;

      mOut.M14 :=  0.0 ;
      mOut.M24 :=  0.0 ;
      mOut.M34 :=  0.0 ;
      mOut.M44 :=  1.0 ;

      Result := mOut ;
    {$ELSE}
      mOut._11 :=  r.X ;
      mOut._21 :=  r.Y ;
      mOut._31 :=  r.Z ;
      mOut._41 := -Vec3Dot(r, Eye) ;

      mOut._12 :=  u.X ;
      mOut._22 :=  u.Y ;
      mOut._32 :=  u.Z ;
      mOut._42 := -Vec3Dot(u, Eye) ;

      mOut._13 :=  v.X ;
      mOut._23 :=  v.Y ;
      mOut._33 :=  v.Z ;
      mOut._43 := -Vec3Dot(v, Eye) ;

      mOut._14 :=  0.0 ;
      mOut._24 :=  0.0 ;
      mOut._34 :=  0.0 ;
      mOut._44 :=  1.0 ;

      Result := @mOut ;
    {$ENDIF}

  end;

  function Vec3TransformCoord(
    out   vOut        : TD3DVector         ;
    const v           : TD3DVector         ;
    const m           : TD3DMatrix
  ) : PD3DVector;
  var
    X, Y, Z, W : Single ;
  begin
    {$IFDEF CLR}
      X := v.X * m.M11 + v.Y * m.M21 + v.Z * m.M31 + m.M41 ;
      Y := v.X * m.M12 + v.Y * m.M22 + v.Z * m.M32 + m.M42 ;
      Z := v.X * m.M13 + v.Y * m.M23 + v.Z * m.M33 + m.M43 ;
      W := v.X * m.M14 + v.Y * m.M24 + v.Z * m.M34 + m.M44 ;
    {$ELSE}
      X := v.X * m._11 + v.Y * m._21 + v.Z * m._31 + m._41 ;
      Y := v.X * m._12 + v.Y * m._22 + v.Z * m._32 + m._42 ;
      Z := v.X * m._13 + v.Y * m._23 + v.Z * m._33 + m._43 ;
      W := v.X * m._14 + v.Y * m._24 + v.Z * m._34 + m._44 ;
    {$ENDIF}

    vOut.X := X / W;
    vOut.Y := Y / W;
    vOut.Z := Z / W;

    {$IFDEF CLR}
      Result := vOut ;
    {$ELSE}
      Result := @vOut ;
    {$ENDIF}
  end;

  function Vec3TransformNormal(
    out   vOut        : TD3DVector         ;
    const v           : TD3DVector         ;
    const m           : TD3DXMatrix
  ): PD3DVector;
  begin
    {$IFDEF CLR}
      vOut.X := m.M11 * v.X + m.M21 * v.Y + m.M31 * v.Z ;
      vOut.Y := m.M12 * v.X + m.M22 * v.Y + m.M32 * v.Z ;
      vOut.Z := m.M13 * v.X + m.M23 * v.Y + m.M33 * v.Z ;

      Result := vOut ;
    {$ELSE}
      vOut.X := m._11 * v.X + m._21 * v.Y + m._31 * v.Z ;
      vOut.Y := m._12 * v.X + m._22 * v.Y + m._32 * v.Z ;
      vOut.Z := m._13 * v.X + m._23 * v.Y + m._33 * v.Z ;

      Result := @vOut ;
    {$ENDIF}
  end;

  function Vec3Project(
    out   vOut        : TD3DVector         ;
    const v           : TD3DVector         ;
    const pViewport   : TD3DViewport9      ;
    const pProjection : TD3DMatrix         ;
    const pView       : TD3DMatrix         ;
    const pWorld      : TD3DMatrix
  ): PD3DVector;
  var
    m1, m2 : TD3DMatrix ;
    vec    : TD3DVector ;
  begin
    MatrixMultiply(m1, pWorld, pView);
    MatrixMultiply(m2, m1, pProjection);

    Vec3TransformCoord(vec, v, m2);

    vOut.X := pViewport.X + ( 1.0 + vec.X ) * pViewport.Width  / 2.0 ;
    vOut.Y := pViewport.Y + ( 1.0 - vec.Y ) * pViewport.Height / 2.0 ;
    {$IFDEF CLR}
      vOut.Z := pViewport.MinDepth + vec.Z * ( pViewport.MaxDepth - pViewport.MinDepth ) ;
    {$ELSE}
      vOut.Z := pViewport.MinZ + vec.Z * ( pViewport.MaxZ - pViewport.MinZ ) ;
    {$ENDIF}

    {$IFDEF CLR}
      Result := vOut ;
    {$ELSE}
      Result := @vOut ;
    {$ENDIF}
  end;

  function Vec3Unproject(
    out   vOut        : TD3DVector         ;
    const v           : TD3DVector         ;
    const pViewport   : TD3DViewport9      ;
    const pProjection : TD3DMatrix         ;
    const pView       : TD3DMatrix         ;
    const pWorld      : TD3DMatrix
  ): PD3DVector;
  var
    m1, m2, m3 : TD3DMatrix ;
    vec    : TD3DVector ;
  begin
    MatrixMultiply(m1, pWorld, pView) ;
    MatrixMultiply(m2, m1, pProjection) ;

    MatrixInverse(m3, nil, m2) ;

    vec.X := 2.0 * ( v.X - pViewport.X ) / pViewport.Width - 1.0 ;
    vec.Y := 1.0 - 2.0 * ( v.Y - pViewport.Y ) / pViewport.Height;
    {$IFDEF CLR}
      vec.Z := ( v.Z - pViewport.MinDepth) / ( pViewport.MaxDepth - pViewport.MinDepth ) ;
    {$ELSE}
      vec.Z := ( v.Z - pViewport.MinZ) / ( pViewport.MaxZ - pViewport.MinZ ) ;
    {$ENDIF}

    Vec3TransformCoord(vOut, vec, m3);

    {$IFDEF CLR}
      Result := vOut ;
    {$ELSE}
      Result := @vOut ;
    {$ENDIF}
  end;

  function PlaneFromPointNormal(
    out   pOut        : TD3DXPlane         ;
    const vPoint      : TD3DVector         ;
    const vNormal     : TD3DVector
  ): PD3DXPlane;
  begin
    {$IFDEF CLR}
      pOut := new TD3DXPlane(
                vNormal.X,
                vNormal.Y,
                vNormal.Z,
                -Vec3Dot( vPoint, vNormal )
              ) ;
    {$ELSE}
      pOut.a := vNormal.X ;
      pOut.b := vNormal.Y ;
      pOut.c := vNormal.Z ;
      pOut.d := -Vec3Dot( vPoint, vNormal ) ;
    {$ENDIF}

    {$IFDEF CLR}
      Result := pOut ;
    {$ELSE}
      Result := @pOut ;
    {$ENDIF}
  end;

  function PlaneIntersectLine(
    out   pOut        : TD3DVector         ;
    const p           : TD3DXPlane         ;
    const v1          : TD3DVector         ;
    const v2          : TD3DVector
  ) : PD3DVector ;
  var
    d, n : TD3DVector ;
    dp, tv : Single ;
  begin
    {$IFDEF CLR}
      n.X := p.Normal.X ;
      n.Y := p.Normal.Y ;
      n.Z := p.Normal.Z ;
    {$ELSE}
      Result := nil ;
      n.X := p.a ;
      n.Y := p.b ;
      n.Z := p.c ;
    {$ENDIF}
    d.X := v2.X -v1.X ;
    d.Y := v2.Y -v1.Y ;
    d.Z := v2.Z -v1.Z ;
    dp  := Vec3Dot(n, d) ;
    if (Abs(dp) < 0.0001) then exit ;    // Line and plane don't intersect

    tv := ( p.D + Vec3Dot(n, v1) ) / dp ;
    pOut.X := v1.X - tv * d.X ;
    pOut.Y := v1.Y - tv * d.Y ;
    pOut.Z := v1.Z - tv * d.Z ;

    {$IFDEF CLR}
      Result := pOut ;
    {$ELSE}
      Result := @pOut ;
    {$ENDIF}
  end;

  function Vec4Transform(
    out   vOut        : TD3DXVector4 ;
    const v           : TD3DXVector4 ;
    const m           : TD3DMatrix
  ) : PD3DVector4 ;
  var
    tmp : TD3DVector4 ;
  begin
    {$IFDEF CLR}
      tmp.X := v.X * m.M11 + v.Y * m.M12 + v.Z * m.M13 + v.W * m.M14 ;
      tmp.Y := v.X * m.M21 + v.Y * m.M22 + v.Z * m.M23 + v.W * m.M24 ;
      tmp.Z := v.X * m.M31 + v.Y * m.M32 + v.Z * m.M33 + v.W * m.M34 ;
      tmp.W := v.X * m.M41 + v.Y * m.M42 + v.Z * m.M43 + v.W * m.M44 ;
    {$ELSE}
      tmp.X := v.X * m._11 + v.Y * m._12 + v.Z * m._13 + v.W * m._14 ;
      tmp.Y := v.X * m._21 + v.Y * m._22 + v.Z * m._23 + v.W * m._24 ;
      tmp.Z := v.X * m._31 + v.Y * m._32 + v.Z * m._33 + v.W * m._34 ;
      tmp.W := v.X * m._41 + v.Y * m._42 + v.Z * m._43 + v.W * m._44 ;
    {$ENDIF}

    vOut.X := tmp.X ;
    vOut.Y := tmp.Y ;
    vOut.Z := tmp.Z ;
    vOut.W := tmp.W ;

    {$IFDEF CLR}
      Result := vOut ;
    {$ELSE}
      Result := @vOut ;
    {$ENDIF}
  end ;


  function  IntersectTri(
    const _v0, _v1, _v2 : TD3DXVector3 ;
    const _orig, _dir  : TD3DXVector3 ;
    out _u, _v, _d : Single
  ) : Boolean ;
  var
    edge1 : TD3DXVector3 ;
    edge2 : TD3DXVector3 ;
    pvec  : TD3DXVector3 ;
    det   : Single ;
    tvec  : TD3DXVector3 ;
    qvec  : TD3DXVector3 ;
    fInvDet : Single ;
  begin
    // Find vectors for two edges sharing vert0
    Vec3Subtract(edge1, _v1, _v0);
    Vec3Subtract(edge2, _v2, _v0);

    // Begin calculating determinant - also used to calculate U parameter
    Vec3Cross(pvec, _dir, edge2);

    // If determinant is near zero, ray lies in plane of triangle
    det:= Vec3Dot(edge1, pvec);
    if (det > -0.00001) and (det < 0.00001) then
    begin
      Result:= False;
      exit;
    end;

    // Calculate distance from vert0 to ray origin
    Vec3Subtract(tvec, _orig, _v0);

    // Calculate U parameter and test bounds
    fInvDet:= 1.0 / det;
    _u:= Vec3Dot(tvec, pvec) * fInvDet ;
    if(_u < 0.0) or (_u > 1.0) then
    begin
      Result:= False ;
      exit;
    end;

    // Prepare to test V parameter
    Vec3Cross(qvec, tvec, edge1);

    // Calculate V parameter and test bounds
    _v:= Vec3Dot(_dir, qvec) * fInvDet ;
    if (_v < 0.0) or (_u + _v > 1.0) then
    begin
      Result:= False ;
      exit;
    end;

    // Calculate t, scale parameters, ray intersects triangle
    _d := Vec3Dot(edge2, qvec);
    fInvDet:= 1.0 / det ;
    _d := _d * fInvDet ;

    Result:= True ;
  end;

  function IntersectTri1(   // see intersectTriRay
    const p0          : TD3DXVector3       ;
    const p1          : TD3DXVector3       ;
    const p2          : TD3DXVector3       ;
    const pRayPos     : TD3DXVector3       ;
    const pRayDir     : TD3DXVector3       ;
    out   pU          : Single             ;
    out   pV          : Single             ;
    out   pDist       : Single
  ): BOOL;
  var
    m : TD3DMatrix ;
    v : TD3DXVector4 ;
  begin
    Result := False ;
    {$IFDEF CLR}
      m.M11 := p1.X - p0.X ;
      m.M21 := p2.X - p0.X ;
      m.M31 :=  -pRayDir.X ;
      m.M41 := 0.0 ;
      m.M12 := p1.Y - p0.Y ;
      m.M22 := p2.Y - p0.Y ;
      m.M32 :=  -pRayDir.Y ;
      m.M42 := 0.0 ;
      m.M13 := p1.Z - p0.Z ;
      m.M23 := p2.Z - p0.Z ;
      m.M33 :=  -pRayDir.Z ;
      m.M43 := 0.0 ;
      m.M14 := 0.0 ;
      m.M24 := 0.0 ;
      m.M34 := 0.0 ;
      m.M44 := 1.0 ;
    {$ELSE}
      m._11 := p1.X - p0.X ;
      m._21 := p2.X - p0.X ;
      m._31 :=  -pRayDir.X ;
      m._41 := 0.0 ;
      m._12 := p1.Y - p0.Y ;
      m._22 := p2.Y - p0.Y ;
      m._32 :=  -pRayDir.Y ;
      m._42 := 0.0 ;
      m._13 := p1.Z - p0.Z ;
      m._23 := p2.Z - p0.Z ;
      m._33 :=  -pRayDir.Z ;
      m._43 := 0.0 ;
      m._14 := 0.0 ;
      m._24 := 0.0 ;
      m._34 := 0.0 ;
      m._44 := 1.0 ;
    {$ENDIF}


    v.X := pRayPos.X - p0.X ;
    v.Y := pRayPos.Y - p0.Y ;
    v.Z := pRayPos.Z - p0.Z ;
    v.W := 0.0 ;

    if MatrixInverse(m, nil, m) <> nil  then begin
      Vec4Transform(v, v, m);
      if ( (v.X >= 0.0) and (v.Y >= 0.0) and
           (v.X + v.Y <= 1.0) and (v.Z >= 0.0) ) then begin
        pU := v.X ;
        pV := v.Y ;
        pDist := Abs( v.Z ) ;
        Result := True ;
      end ;
    end ;

  end ;


  // TVector3f record initialization (portable against different platforms).
  function vector3InitDX9(
    const _x : Single ;
    const _y : Single ;
    const _z : Single
  ) : TVector3f ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  begin
    {$IFDEF CLR}
      Result := new TVector3f( _x, _y, _z ) ;
    {$ELSE}
      Result.X := _x ;
      Result.Y := _y ;
      Result.Z := _z ;
    {$ENDIF}
  end;

type
// GDB file material info
  T_mtrlInfo = record
    // material ID
    MtrlId   : Integer ;
    // Bitmap checksum
    CheckSum : Integer ;
    // corresponding texture
    TexDX  : IDirect3DTexture9 ;
  end ;

  // Array of T_mtrlInfo
  T_arMtrlInfo = array of T_mtrlInfo ;

type

  // Helper class to create and keep list of textures based on provided bitmap.
  // Internally keep list of T_textureHelperItem objects. DirectX version
  T_textureHelperDX = class ( TGIS_BaseObjectDisposable )
    private
      oLst : TStringList ;
    protected

      // Destroy an instance. Clear all managed textures.
      procedure doDestroy ; override;
    public

      // Create an instance
      constructor Create ;
    public

      // Fetch form the list texture corresponding to provided bitmap. If such
      // texture does not exist - create a texture then fetch.
      // Properly manage transparent bitmaps.
      // _ctx  DirectX context
      // _bmp  source bitmap
      // return texture
      function  Prepare( const _ctx : IDirect3DDevice9 ;
                         const _bmp : TGIS_Bitmap
                       ) : String ;

      // Get previously created texture.
      // _hash  hash code
      // return texture
      function  Get    ( const _hash : String
                       ) : IDirect3DTexture9 ;

      // Clear list of managed texture - free all textures.
      procedure Clear  ;
  end ;

  // Single item in a T_textureHelper list.  DirectX version.
  T_textureHelperItemDX = class ( TGIS_BaseObjectDisposable )
     {$IFNDEF CLR} private {$ELSE} unit {$ENDIF}
       oTexture : IDirect3DTexture9 ;
     protected

       //  Destroy an object
       procedure doDestroy ; override;
  end;

  // D3DXVector3 record initialization (portable against different platforms.
  function D3DXVector3Init(
    const _x : Single ;
    const _y : Single ;
    const _z : Single
  ) : TD3DXVector3 ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  begin
    Result.X := _x ;
    Result.Y := _y ;
    Result.Z := _z ;
  end;

  // Create a texture base on provided bitmap object.
  // _ctx  DirectX context
  // _bmp  source bitmap
  // return texture
  function create_texture(
      const _ctx : IDirect3DDevice9 ;
      const _bmp : TGIS_Bitmap
    ) : IDirect3DTexture9 ;
  var
    pix     : TGIS_Pixels ;
    rct     : D3DLOCKED_RECT ;
    {$IFDEF CLR}
      dxrct : DirectXRectangle ;
    {$ENDIF}
    cl : TGIS_Color ;
  begin
    if not assigned( _ctx ) or
      (_bmp.Width > maxTexSize) or
      (_bmp.Height > maxTexSize) then
    begin
      Result := nil ;
      exit;
    end;

    try
      _bmp.LockPixels( pix, False, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
      if _ctx.CreateTexture(
        _bmp.Width,                                  //Width
        _bmp.Height,                                 //Height
        0,                                           //MipLevels
        D3DUSAGE_AUTOGENMIPMAP,                      //Usage
        D3DFMT_A8R8G8B8,                             //Format
        D3DPOOL_MANAGED,                             //Pool
        Result,                                      //ppTexture
        nil
      ) <> S_OK then exit;

      {$IFDEF CLR}
        dxrct := new DirectXRectangle( 0, 0, _bmp.Width, _bmp.Height ) ;
        if Result.LockRect( 0, rct, dxrct, D3DLOCK_DISCARD ) <> S_OK then
          exit ;
        Marshal.Copy( pix, 0, rct.DataPointer, _bmp.Width * _bmp.Height ) ;
      {$ELSE}
        if Result.LockRect( 0, rct, nil, D3DLOCK_DISCARD ) <> S_OK then
          exit ;
        System.Move( pix[0], PByte( rct.pBits )^, _bmp.Width * _bmp.Height * 4 ) ;
      {$ENDIF}
      cl.ARGB := Cardinal(pix[0]) ;
      Result.UnlockRect( 0 ) ;
    finally
      _bmp.UnlockPixels ;
    end;
  end;

  // Create a texture base on provided material object. Properly manage
  // transparent bitmaps.
  // _ctx  DirectX context
  // _mat  source material
  // return texture
  function create_texture1(
      const _ctx : IDirect3DDevice9 ;
      const _mat : TGIS_Material
    ) : IDirect3DTexture9 ;
  var
    r, g, b : Byte ;
    r1, g1, b1 : Byte ;
    i, k : Integer ;
    strm : TMemoryStream ;
    bmp  : TGIS_Bitmap ;
    pix  : TGIS_Pixels ;
    cl,cl1 : TGIS_Color ;
  begin
    strm := TMemoryStream.Create  ;
    strm.Write( _mat.Buffer, _mat.Size ) ;
    strm.Position := 0 ;
    bmp := TGIS_Bitmap.Create ;
    bmp.LoadFromStream( strm ) ;
    if (_mat.Bpp = 4) and _mat.HasColor then begin
      r := _mat.Color.R ;
      g := _mat.Color.G ;
      b := _mat.Color.B ;
      k := bmp.Width * bmp.Height ;
      bmp.LockPixels( pix, True, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
      cl1 := TGIS_Color.FromARGB( 0, 0, 0, 0 ) ;
      for i := 0 to k -1 do begin
        cl.ARGB := Cardinal(pix[i]) ;
        r1 := cl.R ;
        g1 := cl.G ;
        b1 := cl.B ;
        if (r1=r) and (g1=g) and (b1=b) then
          pix[i] := Integer(cl1.ARGB) ;
      end;
      bmp.UnlockPixels ;
    end;

    Result := create_texture( _ctx, bmp ) ;
    FreeObject( bmp ) ;
    FreeObject( strm ) ;
  end ;

  // Create a texture base on provided material object.
  // _ctx  DirectX context
  // _mat  source material
  // return texture
  function create_texture2(
      const _ctx : IDirect3DDevice9 ;
      const _mat : TGIS_Material
    ) : IDirect3DTexture9 ;
  var
    rct     : D3DLOCKED_RECT ;
    {$IFDEF CLR}
      dxrct : DirectXRectangle ;
    {$ENDIF}
  begin
    if not assigned( _ctx ) or
      (_mat.Width > maxTexSize) or
      (_mat.Height > maxTexSize) then
    begin
      Result := nil ;
      exit;
    end;

    if _ctx.CreateTexture(
      _mat.Width,                                  //Width
      _mat.Height,                                 //Height
      0,                                           //MipLevels
      D3DUSAGE_AUTOGENMIPMAP,                      //Usage
      D3DFMT_A8R8G8B8,                             //Format
      D3DPOOL_MANAGED,                             //Pool
      Result,                                      //ppTexture
      nil
    ) <> S_OK then exit;

    {$IFDEF CLR}
      dxrct := new DirectXRectangle( 0, 0, _mat.Width, _mat.Height ) ;
      if Result.LockRect( 0, rct, dxrct, D3DLOCK_DISCARD ) <> S_OK then
        exit ;
      Marshal.Copy( _mat.Buffer, 0, rct.DataPointer, _mat.Width * _mat.Height * 4 ) ;
    {$ELSE}
      if Result.LockRect( 0, rct, nil, D3DLOCK_DISCARD ) <> S_OK then
        exit ;
      System.Move( _mat.Buffer[0], PByte( rct.pBits )^, _mat.Width * _mat.Height * 4 ) ;
    {$ENDIF}

    Result.UnlockRect( 0 ) ;
  end;

//==============================================================================
// T_textureHelperDX
//==============================================================================

  constructor T_textureHelperDX.Create ;
  begin
    inherited ;

    oLst := TStringList.Create ;
    oLst.Sorted := true ;
  end ;

  procedure T_textureHelperDX.doDestroy ;
  begin
    Clear ;
    FreeObject( oLst ) ;

    inherited ;
  end;

  function T_textureHelperDX.Prepare(
    const _ctx : IDirect3DDevice9 ;
    const _bmp : TGIS_Bitmap
  ) : String ;
  var
    bmp  : TGIS_Bitmap ;
    hash : String  ;
    idx  : Integer ;
    elm  : T_textureHelperItemDX ;
    tex  : IDirect3DTexture9   ;
  begin
    Result := '' ;

    if not assigned( _bmp ) then exit ;

    bmp := _bmp ;

    {$IFDEF CLR}
      hash := bmp.GetHashCode.ToString ; //?
    {$ELSE}
      hash := IntToStr( NativeInt( Pointer( bmp.NativeBitmap ) ) ) ;
    {$ENDIF}

    if oLst.Find( hash, idx ) then begin
      Result := hash ;
    end
    else begin
      tex := create_texture( _ctx, bmp ) ;
      if assigned( tex ) then
        Result := hash
      else
        Result := '' ;
      assert( Result <> '') ;
      elm := T_textureHelperItemDX.Create ;
      elm.oTexture := tex ;
      oLst.AddObject( hash, elm ) ;
    end ;
  end ;

  function  T_textureHelperDX.Get(
    const _hash : String
  ) : IDirect3DTexture9 ;
  var
    idx  : Integer ;
    elm  : T_textureHelperItemDX ;
  begin
    Result := nil ;
    if oLst.Find( _hash, idx ) then begin
      elm := T_textureHelperItemDX( oLst.Objects[ idx ] ) ;
      Result := elm.oTexture ;
    end;
  end;

  procedure T_textureHelperDX.Clear ;
  var
    idx : Integer             ;
    elm : T_textureHelperItemDX ;
  begin
    for idx := 0 to  oLst.Count -1 do begin
      elm := T_textureHelperItemDX( oLst.Objects[ idx ] ) ;
      assert( assigned( elm ) ) ;

      assert( assigned( elm.oTexture ) ) ;
      FreeObject( elm ) ;
    end;
    oLst.Clear ;
  end;


//==============================================================================
// T_textureHelperItemDX
//==============================================================================

  procedure T_textureHelperItemDX.doDestroy ;
  begin
    // force interface release
    ReleaseInterface( oTexture ) ;

    inherited ;
  end ;


//==============================================================================
// TGIS_Renderer3DDirectX9
//==============================================================================

  constructor TGIS_Renderer3DDirectX9.Create ;
  begin
    inherited;

    pD3D := nil ;             // Used to create the D3DDevice
    pD3DDevice := nil ;
    printBmpOutSize := Point( 0, 0 ) ;
    printBmpImgSize := 0 ;
    oldhWindow := 0 ;
    hWindow    := 0 ;
  end;

  procedure TGIS_Renderer3DDirectX9.doDestroy ;
  begin
    // Release Interfaces
    releaseAllInterfaces ;

    inherited;
  end;

  procedure TGIS_Renderer3DDirectX9.prepareVertexBuffersToDraw ;
  var
    rows         : Integer     ;
    hr           : HResult     ;
    render_start : Int64       ;
    oldrough     : Double      ;
  begin

    if pD3DDevice = nil then begin
      exit ;
    end ;

    {$IFNDEF CLR}
      assert( IsWindow( hWindow ) ) ;
    {$ENDIF}

    errorMsg := '' ;

    if generalLock > 0 then exit ;
    try
      setDefaultZ ;
      iFps := 0 ;

      if inPaint1 then exit ;
      inPaint1 := True ;

      reRenderCall := True ;
      lockReadingData := 0 ;

      if enDemCachedSize = TGIS_Viewer3DDemCacheSize.Window then begin
        if (noImg <> True) or (noDem <> True) then begin
          DemCachedSize := enDemCachedSize  ;
        end ;
      end ;

      wndXSize := windowWidth * dRadius * Tan(DegToRad(cameraHalfViewAngle))
        / windowHeight ;

      basicEyeAt.Z := dRadius ;
      eyeAt := basicEyeAt ;
      eyeTo := basicEyeTo ;

      demTransparency := 255;
      curDemTransp    := 255 ;
      updateLayerExistance ;
      setIgnoreCutValues ;
      updateCameraParams ;
      traceReferenceLevel(bTrace) ;
      sglIgnoreAbove := zScaleFactor * (dIgnoreAbove - zLevel) / zUnit ;
      sglIgnoreBelow := zScaleFactor * (dIgnoreBelow - zLevel) / zUnit ;
      sglCutBelow    := zScaleFactor * (dCutBelow    - zLevel) / zUnit ;

//      transformSetting ;

      // Set Material & Light
//      newSunPos ;
//      pD3DDevice.SetMaterial(oMtrl) ;
//      pD3DDevice.SetLight(0, oLight) ;

      timeStart := GetTickCount ;
      render_start := timeStart ;

      setD3D_Data ;
      setInitialLevel ;
      updateLayerExistance ;

      setLod ;

      rows := TruncS( ( 1.0 * demFrameSize ) / lodStep) ;
      if rows * lodStep >= demFrameSize then
        dec(rows) ;

      // vector drawing when DEM is transparent
      if ((transpPrior = TGIS_Viewer3DTransparencyPriority.Auto) and
            (demTransparency <> 255)) or
            (transpPrior = TGIS_Viewer3DTransparencyPriority.Dem) then begin
        timeStart := GetTickCount ;

        if draw3DVector <> 0 then exit ;
      end;


      // turn on the color blending if necessary
      if demTransparency <> 255 then
        setTextureOperations ;

      lockReadingData := 1 ;

      timeStart := GetTickCount ;

      if firstScene then begin
        if preciseSwitch then begin
          if checkExtent then begin
            setEmptyWindow ;
            hr := doRender(rows, False) ; // rough
            meshDemDetailIndex := meshDemTilesCounter ;
            hr := doRender(rows, True) ; // detail
            demMeshExists := True ;
          end
          else begin
            xMin1 := 0 ;
            xMax1 := 0 ;
            yMin1 := 0 ;
            yMax1 := 0 ;
            hr := doRender(rows, False) ;   // rough
            meshDemDetailIndex := meshDemTilesCounter ;
            demMeshExists := True ;
          end ;
        end
        else begin
          xMin1 := 0 ;
          xMax1 := 0 ;
          yMin1 := 0 ;
          yMax1 := 0 ;
          hr := doRender(rows, False) ; // rough
          meshDemDetailIndex := meshDemTilesCounter ;
          demMeshExists := True ;
        end ;
      end
      else begin
        if preciseSwitch then begin
          if reRenderCall then begin
            if checkExtent then begin
              setEmptyWindow ;
              hr := doRender(rows, False) ; // rough
              meshDemDetailIndex := meshDemTilesCounter ;
              hr := doRender(rows, True) ; // detail
              demMeshExists := True ;
            end
            else begin
              xMin1 := 0 ;
              xMax1 := 0 ;
              yMin1 := 0 ;
              yMax1 := 0 ;
              hr := doRender(rows, False) ; // rough
              meshDemDetailIndex := meshDemTilesCounter ;
              demMeshExists := True ;
            end
          end
          else begin
            oldrough      := roughCoeff ;
            roughCoeff    := detailCoeff ;
            {$IFDEF CLR}
              detWnd := Rect( 0, -1, demFrameSize - lodStep, rows ) ;
            {$ELSE}
              detWnd.Left   := 0 ;
              detWnd.Right  := demFrameSize - lodStep ;
              detWnd.Top    := -1 ;
              detWnd.Bottom := rows ;
            {$ENDIF}
            xMin :=  detailExtent.XMin ;
            yMin :=  detailExtent.YMin ;
            xMax :=  detailExtent.XMax ;
            yMax :=  detailExtent.YMax ;
            hr := doRender(rows, True) ;  // detail
            demMeshExists := True ;
            roughCoeff := oldrough ;
          end
        end
        else begin
          xMin1 := 0 ;
          xMax1 := 0 ;
          yMin1 := 0 ;
          yMax1 := 0 ;
          hr := doRender(rows, False) ; // rough
          meshDemDetailIndex := meshDemTilesCounter ;
          demMeshExists := True ;
        end;
      end ;
      saveWTLBufT ;

      // vector drawing when DEM not transparent
      if ((transpPrior = TGIS_Viewer3DTransparencyPriority.Auto) and
            (demTransparency = 255)) or
            (transpPrior = TGIS_Viewer3DTransparencyPriority.Vector) then begin
        timeStart := GetTickCount ;

        if draw3DVector <> 0 then  exit ;
      end ;

      timeStart := GetTickCount ;

      timeEnd := GetTickCount;
      if timeEnd - render_start = 0 then
        iFps := 1000
      else
        iFps := TruncS(1000.0 / (timeEnd - render_start)) ;

    finally
      inPaint1 := False ;
      reRenderCall := False ;
    end ;
    firstScene   := False ;
  end;

  procedure TGIS_Renderer3DDirectX9.reInit3D ;
  var
    d3ddm : TD3DDisplayMode       ;
    d3dpp : TD3DPresentParameters ;
    d3dcp : TD3DCaps9             ;
    hwd   : HWND ;
  begin
    hwd := hWindow ;
    {$IFNDEF CLR}
      assert( IsWindow( hwd ) ) ;
    {$ENDIF}

    // Release Interfaces
    releaseAllInterfaces ;

    // Create the D3D object, which is needed to create the D3DDevice.
    pD3D := Direct3DCreate9(D3D_SDK_VERSION) ;
    if pD3D = nil then begin
      // raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ), 'InitD3D', 1 ) ;
      exit;
    end;

    // Get the current desktop display mode
    if pD3D.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, d3ddm) <> 0 then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                   'ReInitD3D', 0
                                 ) ;
    end;

    {$IFDEF CLR}
      d3dpp := TD3DPresentParameters_Create ;
    {$ELSE}
      ZeroMemory(@d3dpp, sizeOf(d3dpp)) ;
    {$ENDIF}
    d3dpp.Windowed               := True                        ;
    d3dpp.SwapEffect             := D3DSWAPEFFECT_DISCARD       ;
    d3dpp.BackBufferFormat       := d3ddm.Format                ;
    d3dpp.BackBufferCount        := 1                           ;
    d3dpp.BackBufferWidth        := 0                           ;
    d3dpp.BackBufferHeight       := 0                           ;
    d3dpp.EnableAutoDepthStencil := True                        ;
    d3dpp.AutoDepthStencilFormat := D3DFMT_D24S8                ;
    d3dpp.PresentationInterval   := D3DPRESENT_INTERVAL_DEFAULT ;

    if pD3D.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT,
                                        D3DDEVTYPE_HAL,
                                        D3DFMT_X8R8G8B8,
                                        d3dpp.Windowed,
                                        D3DMULTISAMPLE_4_SAMPLES,
                                        nil
                                      ) = 0 then
      {$IFDEF CLR}
        d3dpp.SetMultiSampleType( D3DMULTISAMPLE_4_SAMPLES )
      {$ELSE}
        d3dpp.MultiSampleType := D3DMULTISAMPLE_4_SAMPLES
      {$ENDIF}
    else
    if pD3D.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT,
                                        D3DDEVTYPE_HAL,
                                        D3DFMT_X8R8G8B8,
                                        d3dpp.Windowed,
                                        D3DMULTISAMPLE_2_SAMPLES,
                                        nil
                                      ) = 0 then
      {$IFDEF CLR}
        d3dpp.SetMultiSampleType( D3DMULTISAMPLE_2_SAMPLES )
      {$ELSE}
        d3dpp.MultiSampleType := D3DMULTISAMPLE_2_SAMPLES
      {$ENDIF}
    else
      {$IFDEF CLR}
        d3dpp.SetMultiSampleType( D3DMULTISAMPLE_NONE ) ;
      {$ELSE}
        d3dpp.MultiSampleType := D3DMULTISAMPLE_NONE ;
      {$ENDIF}

      // Create the Direct3D device with HARDWARE_VERTEXPROCESSING.
    {$IFDEF CLR}
      if pD3D.CreateDevice( D3DADAPTER_DEFAULT,
                            D3DDEVTYPE_HAL,
                            hWindow,
                            D3DCREATE_HARDWARE_VERTEXPROCESSING or
                            D3DCREATE_FPU_PRESERVE,
                            d3dpp,
                            pD3DDevice ) <> 0 then begin
    {$ELSE}
      if pD3D.CreateDevice( D3DADAPTER_DEFAULT,
                            D3DDEVTYPE_HAL,
                            hWindow,
                            D3DCREATE_HARDWARE_VERTEXPROCESSING or
                            D3DCREATE_FPU_PRESERVE,
                            @d3dpp,
                            pD3DDevice ) <> 0 then begin
    {$ENDIF}

      // Create the Direct3D device with SOFTWARE_VERTEXPROCESSING.
      // Software vertex processing will work on all cards.
    {$IFDEF CLR}
      if pD3D.CreateDevice( D3DADAPTER_DEFAULT,
                            D3DDEVTYPE_HAL,
                            hWindow,
                            D3DCREATE_SOFTWARE_VERTEXPROCESSING or
                            D3DCREATE_FPU_PRESERVE,
                            d3dpp,
                            pD3DDevice ) <> 0 then begin
    {$ELSE}
      if pD3D.CreateDevice( D3DADAPTER_DEFAULT,
                            D3DDEVTYPE_HAL,
                            hWindow,
                            D3DCREATE_SOFTWARE_VERTEXPROCESSING or
                            D3DCREATE_FPU_PRESERVE,
                            @d3dpp,
                            pD3DDevice ) <> 0 then begin
    {$ENDIF}
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                   'ReInitD3D', 1
                                 ) ;
      end;
    end;

//    var mem := pD3DDevice.stubGetAvailableTextureMem ;

    pD3DDevice.GetDeviceCaps( d3dcp ) ;
    anisotropy_factor := d3dcp.MaxAnisotropy ;
    if anisotropy_factor >= 4 then
      anisotropy_factor := 4 ;

    // ReCreate Geometry
    if setBuffers <> 0 then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                   'ReInitD3D', 2
                                 ) ;
    end;

    arBitmap_w := TGIS_Bitmap.Create( 16, 256 ) ;
    {$IFDEF CLR}
      makeSkyBoxTex ;
    {$ENDIF}
    makeBasementTex ;

    if lightSwitch then
      lightOn
    else
      lightOff ;
  end;

  {$IFDEF CLR}
    function TGIS_Renderer3DDirectX9.pdataVB_Unlock(
      var _strm : DirectXStream   ;
      var _data : T_arTextureData
    ) : Integer ;
    begin
      if length(_data) > 0 then begin
        _strm.Seek( 0, SeekOrigin.Begin ) ;
        _strm.WriteRange( _data ) ;
      end;
      try
        pdataVB.Unlock ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;

    function TGIS_Renderer3DDirectX9.cdataVB_Unlock(
      var _strm : DirectXStream ;
      var _data : T_arColorData
    ) : Integer ;
    begin
      if length(_data) > 0 then begin
        _strm.Seek( 0, SeekOrigin.Begin ) ;
        _strm.WriteRange( _data ) ;
      end;
      try
        cdataVB.Unlock ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ELSE}
    function TGIS_Renderer3DDirectX9.pdataVB_Unlock(
      var _strm : PByte   ;
      var _data : T_arTextureData
    ) : Integer ;
    begin
      if length(_data) > 0 then
        System.Move( _data[0], _strm^ , length( _data ) * SizeOf( _data[0]) ) ;
      try
        pdataVB.Unlock ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;

    function TGIS_Renderer3DDirectX9.cdataVB_Unlock(
      var _strm : PByte ;
      var _data : T_arColorData
    ) : Integer ;
    begin
      if length(_data) > 0 then
        System.Move( _data[0], _strm^ , length( _data ) * SizeOf( _data[0]) ) ;
      try
        cdataVB.Unlock ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ENDIF}

  {$IFDEF CLR}
    function TGIS_Renderer3DDirectX9.pdataVB_Lock(
      var _strm : DirectXStream
    ) : Integer ;
    begin
      try
        pdataVB.Lock(0, 0, _strm, D3DLOCK_NONE) ;
        Result :=  0 ;
      except
        Result := -1 ;
      end;
    end ;

    function TGIS_Renderer3DDirectX9.cdataVB_Lock(
      var _strm : DirectXStream
    ) : Integer ;
    begin
      try
        cdataVB.Lock(0, 0, _strm, D3DLOCK_NONE) ;
        Result :=  0 ;
      except
        Result := -1 ;
      end;
    end ;
  {$ELSE}
    function TGIS_Renderer3DDirectX9.pdataVB_Lock(
      var _strm : PByte
    ) : Integer ;
    begin
      try
        pdataVB.Lock(0, 0, Pointer(_strm), D3DLOCK_NONE) ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;

    function TGIS_Renderer3DDirectX9.cdataVB_Lock(
      var _strm : PByte
    ) : Integer ;
    begin
      try
        cdataVB.Lock(0, 0, Pointer(_strm), D3DLOCK_NONE) ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ENDIF}

  procedure TGIS_Renderer3DDirectX9.releaseLabelInfo ;
  var
   i, k : Integer ;
  begin
    k := high( labelInfo ) ;
    if k <> -1 then begin
      for i := k downto 0 do
        ReleaseInterface( labelInfo[i].LabelTextureDX ) ;
      SetLength( labelInfo , 0 ) ;
    end;
  end;

  procedure TGIS_Renderer3DDirectX9.releaseGeometry ;
  begin
    // Release interfaces
    releaseAllInterfaces ;

    releaseLabelInfo ;
    SetLength(mpatchTileInfo     , 0, 0) ;
    SetLength(meshTileInfoR      , 0, 0) ;
    SetLength(meshTileInfoW      , 0, 0) ;

    inherited ;
  end;

  procedure TGIS_Renderer3DDirectX9.SetSunPosition(
    const _value : TGIS_Point
  ) ;
  var
    val   : Double       ;
    {$IFDEF CLR}
      vec : TD3DXVector3 ;
    {$ENDIF}
  begin
    if _value.X >  Pi/2 then sunPosition.X :=  Pi/2
    else
    if _value.X < -Pi/2 then sunPosition.X := -Pi/2
    else sunPosition.X := _value.X ;

    val := _value.Y ;
    val := truncateTo2Pi(val) ;
    if val >= 0  then
      sunPosition.Y := val
    else
      sunPosition.Y := 2*Pi + val ;

    val := Cos(sunPosition.X) ;

    {$IFDEF CLR}
      vec := new TD3DXVector3( -val * Sin(sunPosition.Y),
                                val * Cos(sunPosition.Y),
                                     -Sin(sunPosition.X)
                             ) ;
      Vec3Normalize( vec, vec ) ;
      oLight.Direction := vec ;
    {$ELSE}
      olight.Direction.X := -val * Sin(sunPosition.Y) ;
      olight.Direction.Y :=  val * Cos(sunPosition.Y) ;
      olight.Direction.Z :=       -Sin(sunPosition.X) ;
      Vec3Normalize(olight.Direction, olight.Direction) ;
    {$ENDIF}
  end;

  procedure TGIS_Renderer3DDirectX9.setAmbientLightColor(
    const _val  : Integer
  ) ;
  begin
    oLight.Ambient.R := _val / 255;
    oLight.Ambient.G := _val / 255;
    oLight.Ambient.B := _val / 255;

    oLight.Diffuse.R := 1 - _val / 255;
    oLight.Diffuse.G := 1 - _val / 255;
    oLight.Diffuse.B := 1 - _val / 255;
  end;

  procedure TGIS_Renderer3DDirectX9.releaseDataVB ;
  begin
  end;

  function TGIS_Renderer3DDirectX9.setBuffers
    : HResult;
  var
    hr             : HResult ;
    verticesnumber : DWORD   ;
  begin
    Result := S_OK ;
    verticesnumber := 2 * vertexBufferSize + 2 ;
    if assigned( pdataVB ) then ReleaseInterface( pdataVB ) ;
    if assigned( cdataVB ) then ReleaseInterface( cdataVB ) ;
    if assigned( idataIB ) then ReleaseInterface( idataIB ) ;

    hr := pD3DDevice.CreateVertexBuffer(
            sizeOf(TGIS_Renderer3DVertex) * videoVertexBufferSize,
            D3DUSAGE_DYNAMIC or D3DUSAGE_WRITEONLY,
            GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE,
            D3DPOOL_DEFAULT, pdataVB, nil
          ) ;

    if hr <> 0 then begin
      Result := hr ;
      exit ;
    end;

    hr := pD3DDevice.CreateVertexBuffer(
       sizeOf(TGIS_Renderer3DVertex) * videoVertexBufferSize ,  //videoVertexBufferSize,
       D3DUSAGE_DYNAMIC or D3DUSAGE_WRITEONLY,
       GISVIEWER3D_D3DFVF_GRAPH3DCOLOR,
       D3DPOOL_DEFAULT, cdataVB, nil) ;
    if hr <> 0 then begin
      Result := hr ;
      exit ;
    end;

    hr := pD3DDevice.CreateIndexBuffer(
       sizeOf(Integer) * videoVertexBufferSize ,  //videoVertexBufferSize,
       D3DUSAGE_DYNAMIC or D3DUSAGE_WRITEONLY,
       D3DFMT_INDEX32,
       D3DPOOL_DEFAULT, idataIB, nil) ;
     if hr <> 0 then begin
       Result := hr ;
       exit ;
     end;

    Result := S_OK ;
  end;

  function TGIS_Renderer3DDirectX9.createVertexBuffers(
    const _vertexNum: DWORD ;
    out _vbuf      : IDirect3DVertexBuffer9 ;
    out _ibuf      : IDirect3DIndexBuffer9
  ) : HResult;
  var
    hr : HResult ;
  begin
//    if assigned( pdataVB ) then ReleaseInterface( pdataVB ) ;
    if assigned( _vbuf ) then ReleaseInterface( _vbuf ) ;
    if assigned( _ibuf ) then ReleaseInterface( _ibuf ) ;
{
    hr := pD3DDevice.CreateVertexBuffer(
            sizeOf(TGIS_Renderer3DVertex) * _vertexNum,
            D3DUSAGE_DYNAMIC or D3DUSAGE_WRITEONLY,
            GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE,
            D3DPOOL_DEFAULT, pdataVB, nil
          ) ;

     if hr <> 0 then begin
       Result := hr ;
       exit ;
     end;
}
     hr := pD3DDevice.CreateVertexBuffer(
       sizeOf(TGIS_Renderer3DVertex) * _vertexNum ,  //videoVertexBufferSize,
       D3DUSAGE_DYNAMIC or D3DUSAGE_WRITEONLY,
       GISVIEWER3D_D3DFVF_GRAPH3DCOLOR,
       D3DPOOL_DEFAULT, _vbuf, nil) ;
     if hr <> 0 then begin
       Result := hr ;
       exit ;
     end;
//     _vbuf := cdataVB ;
     hr := pD3DDevice.CreateIndexBuffer(
       sizeOf(Integer) * _vertexNum ,  //videoVertexBufferSize,
       D3DUSAGE_DYNAMIC or D3DUSAGE_WRITEONLY,
       D3DFMT_INDEX32,
       D3DPOOL_DEFAULT, _ibuf, nil) ;
     if hr <> 0 then begin
       Result := hr ;
       exit ;
     end;
//     _ibuf := idataIB ;
    Result := S_OK ;
  end;

  procedure TGIS_Renderer3DDirectX9.SetHWND( const _hwnd : HWND )  ;
  begin
    oldhWindow := hWindow ;
    hWindow := _hwnd ;
  end;

  function TGIS_Renderer3DDirectX9.Init3D(
  ) : Boolean ;
  var
    d3ddm : TD3DDisplayMode       ;
    d3dpp : TD3DPresentParameters ;
    d3dcp : TD3DCaps9             ;
    hwd   : HWND ;
  begin
    Result := False ;

    hwd := hWindow ;
    {$IFNDEF CLR}
      assert( IsWindow( hwd ) ) ;
    {$ENDIF}
    // Create the D3D object, which is needed to create the D3DDevice.
    pD3D := Direct3DCreate9(D3D_SDK_VERSION) ;
    if pD3D = nil then begin
      // raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ), 'InitD3D', 1 ) ;
      exit;
    end;

    // Get the current desktop display mode
    if pD3D.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, d3ddm) <> 0 then begin
      // raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),'InitD3D', 2 ) ;
       ReleaseInterface( pD3D ) ;
       exit;
    end;
    {$IFDEF CLR}
      d3dpp := TD3DPresentParameters_Create ;
    {$ELSE}
      ZeroMemory(@d3dpp, sizeOf(d3dpp)) ;
    {$ENDIF}
    d3dpp.Windowed               := True                        ;
    d3dpp.SwapEffect             := D3DSWAPEFFECT_DISCARD       ;
    d3dpp.BackBufferFormat       := d3ddm.Format                ;
    d3dpp.BackBufferCount        := 1                           ;
    d3dpp.BackBufferWidth        := 0                           ;
    d3dpp.BackBufferHeight       := 0                           ;
    d3dpp.EnableAutoDepthStencil := True                        ;
    d3dpp.AutoDepthStencilFormat := D3DFMT_D24S8                ;
    d3dpp.PresentationInterval   := D3DPRESENT_INTERVAL_DEFAULT ;

    if pD3D.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT,
                                        D3DDEVTYPE_HAL,
                                        D3DFMT_X8R8G8B8,
                                        d3dpp.Windowed,
                                        D3DMULTISAMPLE_4_SAMPLES,
                                        nil
                                      ) = 0 then
      {$IFDEF CLR}
        d3dpp.SetMultiSampleType( D3DMULTISAMPLE_4_SAMPLES )
      {$ELSE}
        d3dpp.MultiSampleType := D3DMULTISAMPLE_4_SAMPLES
      {$ENDIF}
    else
    if pD3D.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT,
                                        D3DDEVTYPE_HAL,
                                        D3DFMT_X8R8G8B8,
                                        d3dpp.Windowed,
                                        D3DMULTISAMPLE_2_SAMPLES,
                                        nil
                                      ) = 0 then
      {$IFDEF CLR}
        d3dpp.SetMultiSampleType( D3DMULTISAMPLE_2_SAMPLES )
      {$ELSE}
        d3dpp.MultiSampleType := D3DMULTISAMPLE_2_SAMPLES
      {$ENDIF}
    else
      {$IFDEF CLR}
        d3dpp.SetMultiSampleType( D3DMULTISAMPLE_NONE ) ;
      {$ELSE}
        d3dpp.MultiSampleType := D3DMULTISAMPLE_NONE ;
      {$ENDIF}

      // Create the Direct3D device with HARDWARE_VERTEXPROCESSING.
    {$IFDEF CLR}
      if pD3D.CreateDevice( D3DADAPTER_DEFAULT,
                            D3DDEVTYPE_HAL,
                            hWindow,
                            D3DCREATE_HARDWARE_VERTEXPROCESSING or
                            D3DCREATE_FPU_PRESERVE,
                            d3dpp,
                            pD3DDevice ) <> 0 then begin
    {$ELSE}
     if pD3D.CreateDevice( D3DADAPTER_DEFAULT,
                            D3DDEVTYPE_HAL,
                            hWindow,
                            D3DCREATE_HARDWARE_VERTEXPROCESSING or
                            D3DCREATE_FPU_PRESERVE,
                            @d3dpp,
                            pD3DDevice ) <> 0 then  begin
    {$ENDIF}

      // Create the Direct3D device with SOFTWARE_VERTEXPROCESSING.
      // Software vertex processing will work on all cards.
    {$IFDEF CLR}
      if pD3D.CreateDevice( D3DADAPTER_DEFAULT,
                            D3DDEVTYPE_HAL,
                            hWindow,
                            D3DCREATE_SOFTWARE_VERTEXPROCESSING or
                            D3DCREATE_FPU_PRESERVE,
                            d3dpp,
                            pD3DDevice ) <> 0 then begin
    {$ELSE}
      if pD3D.CreateDevice( D3DADAPTER_DEFAULT,
                            D3DDEVTYPE_HAL,
                            hWindow,
                            D3DCREATE_SOFTWARE_VERTEXPROCESSING or
                            D3DCREATE_FPU_PRESERVE,
                            @d3dpp,
                            pD3DDevice ) <> 0 then begin
    {$ENDIF}
//      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),'InitD3D', 3 ) ;
        ReleaseInterface( pD3D ) ;
        exit;
      end;
    end;

    pD3DDevice.GetDeviceCaps( d3dcp ) ;
    anisotropy_factor := d3dcp.MaxAnisotropy ;
    if anisotropy_factor >= 4 then
      anisotropy_factor := 4 ;

    maxTexSize := Max(d3dcp.MaxTextureWidth, d3dcp.MaxTextureHeight) ;

    oPolyRoof := TGIS_Tessellation.Create ;
    if oPolyRoof = nil then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                   'InitD3D', 4
                                 ) ;
    end;
    oTextureHelper := T_textureHelperDX.Create ;
    Result := True ; //  Device successfully created and
                     //  oPolyRoof object successfully created
  end;

  procedure TGIS_Renderer3DDirectX9.initMaterial ;
  begin
    {$IFDEF CLR}
      oMtrl := new TD3DMaterial9 ;

      oMtrl.SetDiffuse(  GetDirectXColor( 1.0, 1.0, 1.0, 1.0 ) ) ;
      oMtrl.SetAmbient(  GetDirectXColor( 0.0, 0.0, 0.0, 1.0 ) ) ;
      oMtrl.SetSpecular( GetDirectXColor( 0.0, 0.0, 0.0, 1.0 ) ) ;
      oMtrl.SetEmissive( GetDirectXColor( 0.0, 0.0, 0.0, 1.0 ) ) ;

      oMtrl.SetPower( 1.0 ) ;
    {$ELSE}
      // Doesn't affect light/shadows
      oMtrl.Diffuse.r := 1 ;
      oMtrl.Diffuse.g := 1 ;
      oMtrl.Diffuse.b := 1 ;
      oMtrl.Diffuse.a := 1 ;

      oMtrl.Ambient.r := 0 ;
      oMtrl.Ambient.g := 0 ;
      oMtrl.Ambient.b := 0 ;
      oMtrl.Ambient.a := 1 ;

      oMtrl.Specular.r := 0 ;
      oMtrl.Specular.g := 0 ;
      oMtrl.Specular.b := 0 ;
      oMtrl.Specular.a := 1 ;

      oMtrl.Emissive.r := 0 ;
      oMtrl.Emissive.g := 0 ;
      oMtrl.Emissive.b := 0 ;
      oMtrl.Emissive.a := 1 ;

      oMtrl.Power := 100 ;
    {$ENDIF}
  end;

  procedure TGIS_Renderer3DDirectX9.initLight ;
  var
    pt    : TGIS_Point   ;
    val   : Double       ;
    {$IFDEF CLR}
      vec : TD3DXVector3 ;
    {$ENDIF}
  begin
    pt.X := DegToRad( 30 ) ;
    pt.Y := DegToRad( 225 ) ;
    sunPosition.X := pt.X ;
    sunPosition.Y := pt.Y ;
    val := Cos(sunPosition.X) ;

    {$IFDEF CLR}
      oLight := new TD3DLight9 ;

      oLight.SetDiffuse(  GetDirectXColor( 0.6, 0.6, 0.6, 1.0 ) ) ;
      oLight.SetAmbient(  GetDirectXColor( 0.4, 0.4, 0.4, 1.0 ) ) ;
      oLight.SetSpecular( GetDirectXColor( 0.5, 0.5, 0.5, 1.0 ) ) ;

      oLight.Type  := D3DLIGHT_DIRECTIONAL ;

      vec := new TD3DXVector3( -val * Sin(sunPosition.Y),
                                val * Cos(sunPosition.Y),
                                     -Sin(sunPosition.X)
                             ) ;

      Vec3Normalize( vec, vec ) ;

      oLight.Direction := vec ;
    {$ELSE}
      oLight.Diffuse.r := 0.6 ;
      oLight.Diffuse.g := 0.6 ;
      oLight.Diffuse.b := 0.6 ;
      oLight.Diffuse.a := 1.0 ;

      oLight.Ambient.r := 0.4 ;
      oLight.Ambient.g := 0.4 ;
      oLight.Ambient.b := 0.4 ;
      oLight.Ambient.a := 1.0 ;

      oLight.Specular.r := 0.5 ;
      oLight.Specular.g := 0.5 ;
      oLight.Specular.b := 0.5 ;
      oLight.Specular.a := 1.0 ;

      oLight._Type := D3DLIGHT_DIRECTIONAL ;

      oLight.Direction.X := -val * Sin(sunPosition.Y) ;
      oLight.Direction.Y :=  val * Cos(sunPosition.Y) ;
      oLight.Direction.Z :=       -Sin(sunPosition.X) ;

      Vec3Normalize(oLight.Direction, oLight.Direction) ;

      pD3DDevice.GetLight(0,oLight) ;
    {$ENDIF}

    oLight.Range := 2000 ;
  end;

  procedure TGIS_Renderer3DDirectX9.SetInitValues(
    const _cutextent     : TGIS_Extent ;
    const _visibleextent : TGIS_Extent ;
    const _gis           : IGIS_Viewer ;
    const _viewer        : IGIS_Viewer3D
  ) ;
  begin
    inherited ;

    ReleaseInterface( pdataVB    ) ;
    ReleaseInterface( cdataVB    ) ;
    ReleaseInterface( idataIB    ) ;
    ReleaseInterface( pdataTEX_d ) ;
    ReleaseInterface( pdataTEX_r ) ;
    ReleaseInterface( pdataTEX_w ) ;
    ReleaseInterface( pdataTEX_vDX ) ;
    ReleaseInterface( pdataTEX_tDX ) ;
    {$IFDEF CLR}
      backColor := D3DCOLOR_XRGB(oGIS.Color.B, oGIS.Color.G, oGIS.Color.R) ; //D3DCOLOR_XRGB(234, 217, 153) ;
      borderColor := D3DCOLOR_XRGB(oGIS.Color.B, oGIS.Color.G, oGIS.Color.R) ;
    {$ELSE}
      backColor := D3DCOLOR_XRGB(oGIS.Color.R, oGIS.Color.G, oGIS.Color.B) ; //D3DCOLOR_XRGB(153, 217, 234) ;
      borderColor := D3DCOLOR_XRGB(oGIS.Color.R, oGIS.Color.G, oGIS.Color.B) ;
    {$ENDIF}

   // videoVertexBufferSize must be a multiplication of number 6 !!!
    videoVertexBufferSize := 1*initVideoVBSize ;
    if videoVertexBufferSize mod 6 <> 0 then
      videoVertexBufferSize := TruncS(videoVertexBufferSize / 6) * 6 ;

    // Create Vertex buffer
    setBuffers ;

    // Create Material
    {$IFNDEF CLR}
      ZeroMemory(@oMtrl, sizeOf(TD3DMaterial9)) ;
    {$ENDIF}
    initMaterial ;

    // Create Light
    {$IFNDEF CLR}
      ZeroMemory(@oLight, sizeOf(TD3DLight9)) ;
    {$ENDIF}
    initLight ;
    cameraPosition.Y       := 180 ;
    setLeftColumn ;

    // Create additional vector buffers
{
-    SetLength( vTLBufCR.TriLst   ,  videoVertexBufferSize ) ;
-    SetLength( vTLBufCW.TriLst   ,  videoVertexBufferSize ) ;
-    SetLength( vTLBufTR.TriLst   ,  videoVertexBufferSize ) ;
-    SetLength( vTLBufTR.Subset   ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
-    SetLength( vTLBufTW.TriLst   ,  videoVertexBufferSize ) ;
-    SetLength( vTLBufTW.Subset   ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
-    SetLength( wTLBufT.TriLst    ,  videoVertexBufferSize ) ;
-    SetLength( tinBufT.TriLst    ,  videoVertexBufferSize ) ;
-    SetLength( tinBufC.TriLst    ,  videoVertexBufferSize ) ;
-    SetLength( mpatchBufC.TriLst ,  videoVertexBufferSize ) ;
-    SetLength( mpatchBufT.TriLst ,  videoVertexBufferSize ) ;
-    SetLength( mpatchBufT.Subset ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
-    SetLength( wPLBuf.PntLst     ,  videoVertexBufferSize ) ;
-    SetLength( wPBuf.PntLst      ,  videoVertexBufferSize ) ;}
    SetLength( arBufSelInfo      , 0 ) ;

    bufSelInfoCounter      := 0 ;
    meshDemTilesCounter    := 0 ;
    meshWallTilesCounter   := 0 ;
    meshTinTilesCounterC   := 0 ;
    meshTinTilesCounterT   := 0 ;
    meshMPatchTilesCounterC:= 0 ;
    meshMPatchTilesCounterT:= 0 ;
    meshVectTilesCounterCR := 0 ;
    meshVectTilesCounterCW := 0 ;
    arVectPolyRoofCounterC := 0 ;
    arVectPolyRoofCounterT := 0 ;
    meshVectTilesCounterTR := 0 ;
    meshVectTilesCounterTW := 0 ;
    meshLineTilesCounter   := 0 ;
    meshPointTilesCounter  := 0 ;
    mpatchSubsetCounter    := -1 ;
    SetLength( mpatchTileInfo  , 0, 0 ) ;
    SetLength( meshTileInfoR   , 0, 0 ) ;
    SetLength( meshTileInfoW   , 0, 0 ) ;
    SetLength( arVectPolyRoofT , 0, 0 ) ;
    SetLength( arVectPolyRoofC , 0, 0 ) ;
    SetLength( labelInfo , 0 ) ;
    SetLength( arTexTinInfo , 0 ) ;
    SetLength( transpInfo.TCT , 0 ) ;
    SetLength( transpInfo.TTT , 0 ) ;
    SetLength( transpInfo.MPC , 0 ) ;
    SetLength( transpInfo.MPT , 0 ) ;
    SetLength( transpInfo.PCTR, 0 ) ;
    SetLength( transpInfo.PCTW, 0 ) ;
    SetLength( transpInfo.PTTR, 0 ) ;
    SetLength( transpInfo.PTTW, 0 ) ;
    SetLength( transpInfo.LT  , 0 ) ;
    SetLength( transpInfo.PT  , 0 ) ;
    curMeshTexR            := nil ;
    curMeshTexW            := nil ;
    curMeshSubset          := -1  ;
    lastMeshSubsetR        := -1  ;
    lastMeshSubsetW        := -1  ;
    demMeshExists          := False ;
    firstScene             := True  ;
    bMustReRender          := False   ;
    lastRadius             := dRadius ;
    bLightVector           := True ;
    lastTurn               := dTurn   ;
    lastFly                := dFly    ;
    lastScaleZ             := zFactor ;
    currentVL              := nil ;
    renderer3D             := [TRenderer3D.DX9] ;
    keepSunCamera          := False ;
  end;

  function TGIS_Renderer3DDirectX9.Draw
  : Integer ;
  var
    rows         : Integer     ;
    hr           : HResult     ;
    {$IFDEF CLR}
      wsize        : TPoint ;
    {$ELSE}
      pwi        : TWindowInfo ;
    {$ENDIF}
    render_start : Int64       ;
    oldrough     : Double      ;

      function getNewEyePos(
        const _pnt : TD3DXVector3 ;
        const _rx  : Double ;
        const _ry  : Double ;
        const _rz  : Double
      ) : TD3DXVector3 ;
      var
        c1,c2,c3,s1,s2,s3 : Double ;
        X,Y,Z       : Double ;

        function rotx : TD3DXVector3 ;
        begin
          Result.X := X    + 0    + 0    ;
          Result.Y := 0    + c1*Y - s1*Z ;
          Result.Z := 0    + s1*Y + c1*Z ;
        end ;

        function roty : TD3DXVector3 ;
        begin
          Result.X := c2*X + 0    + s2*Z ;
          Result.Y := 0    + Y    + 0    ;
          Result.Z :=-s2*X + 0    + c2*Z ;
        end ;

        function rotz : TD3DXVector3 ;
        begin
          Result.X := c3*X - s3*Y + 0    ;
          Result.Y := s3*X + c3*Y + 0    ;
          Result.Z := 0    + 0    + Z    ;
        end ;

      begin
        SinCos( _rx, s1, c1 ) ;
        SinCos( _ry, s2, c2 ) ;
        SinCos( _rz, s3, c3 ) ;
        X  := _pnt.X ;
        Y  := _pnt.Y ;
        Z  := _pnt.Z ;
        Result := rotz ;
        X  := Result.X ;
        Y  := Result.Y ;
        Z  := Result.Z ;
        Result := roty ;
        X  := Result.X ;
        Y  := Result.Y ;
        Z  := Result.Z ;
        Result := rotx ;
      end ;

      procedure newSunPos;
      var
        val          : Double      ;
        {$IFDEF CLR}
          vec : TD3DXVector3 ;
        {$ENDIF}
      begin
        val := Cos(sunPosition.X) ;
      {$IFDEF CLR}
        vec := new TD3DXVector3( -val * Sin(sunPosition.Y),
                                  val * Cos(sunPosition.Y),
                                       -Sin(sunPosition.X)
                               ) ;

        oLight.Direction   := getNewEyePos( vec ,
                                            DegToRad(spinX)  ,
                                            DegToRad(spinY)  ,
                                            DegToRad(spinZ)
                                          ) ;
     {$ELSE}
        oLight.Direction.X :=  -val * Sin(sunPosition.Y) ;
        oLight.Direction.Y :=   val * Cos(sunPosition.Y) ;
        oLight.Direction.Z :=        -Sin(sunPosition.X) ;
        oLight.Direction   := getNewEyePos( oLight.Direction ,
                                            DegToRad(spinX)  ,
                                            DegToRad(spinY)  ,
                                            DegToRad(spinZ)
                                          ) ;
     {$ENDIF}
      end ;

  begin
    Result := E_FAIL ;

    if pD3DDevice = nil then begin
      exit ;
    end ;

    {$IFNDEF CLR}
      assert( IsWindow( hWindow ) ) ;
    {$ENDIF}

    Result := S_OK ;

    errorMsg := '' ;

    if generalLock > 0 then exit ;
    try
      setDefaultZ ;

      iFps := 0 ;

      {$IFDEF CLR}
        wsize := GetWindowControlSize( hWindow ) ;
        if ( wsize.X  <> windowWidth  ) or ( wsize.Y <> windowHeight ) then begin
          windowWidth  := wsize.X  ;
          windowHeight := wsize.Y ;
          windowSizeChanged := True ;
        end;
      {$ELSE}
        GetWindowInfo( hWindow, pwi ) ;
        if ((pwi.rcClient.Right  - pwi.rcClient.Left) <> windowWidth ) or
           ((pwi.rcClient.Bottom - pwi.rcClient.Top ) <> windowHeight) then begin
          windowWidth  := pwi.rcClient.Right  - pwi.rcClient.Left ;
          windowHeight := pwi.rcClient.Bottom - pwi.rcClient.Top  ;
          windowSizeChanged := True ;
        end;
      {$ENDIF}

      if inPaint1 then exit ;
      inPaint1 := True ;

      lockReadingData := 1 ;
      reRenderCall := False ;

      if enDemCachedSize = TGIS_Viewer3DDemCacheSize.Window then begin
        if (noImg <> True) or (noDem <> True) then begin
          DemCachedSize := enDemCachedSize  ;
        end ;
      end ;

      wndXSize := windowWidth * dRadius * Tan(DegToRad(cameraHalfViewAngle))
        / windowHeight ;

      basicEyeAt.Z := dRadius ;
      eyeAt := basicEyeAt ;
      eyeTo := basicEyeTo ;

      demTransparency := 255;
      curDemTransp    := 255 ;
      updateLayerExistance ;
      setIgnoreCutValues ;
      updateCameraParams ;
      traceReferenceLevel(bTrace) ;
      sglIgnoreAbove := zScaleFactor * (dIgnoreAbove - zLevel) / zUnit ;
      sglIgnoreBelow := zScaleFactor * (dIgnoreBelow - zLevel) / zUnit ;
      sglCutBelow    := zScaleFactor * (dCutBelow    - zLevel) / zUnit ;

      pD3DDevice.Clear(0, nil, D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER,
        backColor, 1.0, 0) ;

      transformSetting ;

      {$IFDEF CLR}
        try
          if dispMode then
            pD3DDevice.SetRenderState(D3DRS_FILLMODE, D3DFILL_WIREFRAME)
          else
            pD3DDevice.SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID) ;
          hr := S_OK ;
        except
          hr := S_FALSE ;
        end ;
      {$ELSE}
        if dispMode then
          hr := pD3DDevice.SetRenderState(D3DRS_FILLMODE, D3DFILL_WIREFRAME)
        else
          hr := pD3DDevice.SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID) ;
      {$ENDIF}

      if hr <> 0 then begin
        Result := Integer(hr) ;
        exit ;
      end ;

      // Turn off culling, so we see the front and back of the triangle
      {$IFDEF CLR}
        try
          pD3DDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE) ;
          hr := S_OK ;
        except
          hr := S_FALSE ;
        end ;
      {$ELSE}
        hr := pD3DDevice.SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE) ;
      {$ENDIF}
      if hr <> 0 then begin
        Result := Integer(hr) ;
        exit ;
      end ;

      // Set Material & Light
      newSunPos ;
      pD3DDevice.SetMaterial(oMtrl) ;
      pD3DDevice.SetLight(0, oLight) ;

      timeStart := GetTickCount ;
      render_start := timeStart ;

      setD3D_Data ;

      setInitialLevel ;

      setLod ;

      rows := TruncS( ( 1.0 * demFrameSize ) / lodStep) ;
      if rows * lodStep >= demFrameSize then
        dec(rows) ;

      drawSkyBox ;
      pD3DDevice.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA) ;
      pD3DDevice.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA) ;
      pD3DDevice.SetRenderState(D3DRS_BLENDOP, D3DBLENDOP_ADD) ;
      pD3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1) ;

      if basePlane.Level <= zMin then
        drawBasement ;

      // vector drawing when DEM is transparent
      if ((transpPrior = TGIS_Viewer3DTransparencyPriority.Auto) and
            (demTransparency <> 255)) or
            (transpPrior = TGIS_Viewer3DTransparencyPriority.Dem) then begin
        timeStart := GetTickCount ;

        if draw3DVector <> 0 then begin
          Result := -1 ;
          exit ;
        end ;

        drawLabels ;
        if bArrow then
          northArrow( True ) ;

      end ;

      // turn on the color blending if necessary
      if demTransparency <> 255 then
        setTextureOperations ;

      lockReadingData := 1 ;

      timeStart := GetTickCount ;

      if firstScene then begin
        if preciseSwitch then begin
          if checkExtent then begin
            setEmptyWindow ;
            hr := doRender(rows, False) ; // rough
            meshDemDetailIndex := meshDemTilesCounter ;
            hr := doRender(rows, True) ; // detail
            demMeshExists := True ;
          end
          else begin
            xMin1 := 0 ;
            xMax1 := 0 ;
            yMin1 := 0 ;
            yMax1 := 0 ;
            hr := doRender(rows, False) ;   // rough
            meshDemDetailIndex := meshDemTilesCounter ;
            demMeshExists := True ;
          end ;
        end
        else begin
          xMin1 := 0 ;
          xMax1 := 0 ;
          yMin1 := 0 ;
          yMax1 := 0 ;
          hr := doRender(rows, False) ; // rough
          meshDemDetailIndex := meshDemTilesCounter ;
          demMeshExists := True ;
        end ;
      end
      else begin
        if preciseSwitch then begin
          if reRenderCall then begin
            if checkExtent then begin
              setEmptyWindow ;
              hr := doRender(rows, False) ; // rough
              meshDemDetailIndex := meshDemTilesCounter ;
              hr := doRender(rows, True) ; // detail
              demMeshExists := True ;
            end
            else begin
              xMin1 := 0 ;
              xMax1 := 0 ;
              yMin1 := 0 ;
              yMax1 := 0 ;
              hr := doRender(rows, False) ; // rough
              meshDemDetailIndex := meshDemTilesCounter ;
              demMeshExists := True ;
            end
          end
          else begin
            oldrough      := roughCoeff ;
            roughCoeff    := detailCoeff ;
            {$IFDEF CLR}
              detWnd := Rect( 0, -1, demFrameSize - lodStep, rows ) ;
            {$ELSE}
              detWnd.Left   := 0 ;
              detWnd.Right  := demFrameSize - lodStep ;
              detWnd.Top    := -1 ;
              detWnd.Bottom := rows ;
            {$ENDIF}
            xMin :=  detailExtent.XMin ;
            yMin :=  detailExtent.YMin ;
            xMax :=  detailExtent.XMax ;
            yMax :=  detailExtent.YMax ;
            hr := doRender(rows, True) ;  // detail
            demMeshExists := True ;
            roughCoeff := oldrough ;
          end
        end
        else begin
          xMin1 := 0 ;
          xMax1 := 0 ;
          yMin1 := 0 ;
          yMax1 := 0 ;
          hr := doRender(rows, False) ; // rough
          meshDemDetailIndex := meshDemTilesCounter ;
          demMeshExists := True ;
        end;
      end ;
      saveWTLBufT ;

//      if demTransparency <> 255 then
        // turn off the color blending
//        blendOff ;

      // vector drawing when DEM not transparent
      if ((transpPrior = TGIS_Viewer3DTransparencyPriority.Auto) and
            (demTransparency = 255)) or
            (transpPrior = TGIS_Viewer3DTransparencyPriority.Vector) then begin
        timeStart := GetTickCount ;

        if draw3DVector <> 0 then begin
          Result := -1 ;
          exit ;
        end ;

        if bArrow then
          northArrow( True ) ;

      end ;

//      if firstScene then  drawBasement ;

      if flood.Active then
        northArrow( False ) ;

//      drawLabels ;

      pD3DDevice.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA) ;
      pD3DDevice.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA) ;
      pD3DDevice.SetRenderState(D3DRS_BLENDOP, D3DBLENDOP_ADD) ;
      pD3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1) ;
      if basePlane.Level > zMin then
        drawBasement ;
      blendOff ;

      drawLabels ;

      timeStart := GetTickCount ;
      blitToWindow ;

      timeEnd := GetTickCount;
      if timeEnd - render_start = 0 then
        iFps := 1000
      else
        iFps := TruncS(1000.0 / (timeEnd - render_start)) ;

    finally
      inPaint1 := False ;
      reRenderCall := False ;
    end ;
    firstScene   := False ;
    Result := 0 ;

    if safeRendering then
    if not safeRenderingDone then begin
      safeRenderingDone := True ;
      UpdateWholeMap ;
    end;
  end;

  procedure TGIS_Renderer3DDirectX9.prepareMesh(
  const _lp : TGIS_LayerPixel
  ) ;
  var
    i,j, ls : Integer ;
    shp : TGIS_ShapePolygon ;
    abort : Boolean ;
    pnt1, pnt2, pnt3 : TGIS_Point3D ;
    xm,ym,dx,dy,z : Double ;
    ext : TGIS_Extent ;

    procedure setMeshTex ;
    var
      plock, pixels : TGIS_Pixels ;
      bmp : TGIS_Bitmap ;
      wdth, hght : Integer ;
      ratio : Single ;
    begin
      ratio := (ext.XMax - ext.XMin)/(ext.YMax - ext.YMin) ;
      if ratio <= 1 then begin
        hght := 2048 ;
        wdth := TruncS( hght  * ratio ) ;
      end
      else begin
        wdth := 2048 ;
        hght := TruncS( wdth / ratio ) ;
      end;

      SetLength( plock, wdth * hght ) ;
      _lp.GetBitmap( ext, plock, wdth, hght) ;

      bmp := TGIS_Bitmap.Create( wdth, hght ) ;
      try
        bmp.LockPixels( pixels, True ) ;
        {$IFDEF OXYGENE}
          GisCopyPixels( plock, 0, pixels, 0, wdth * hght ) ;
        {$ELSE}
          System.Move( plock[0], pixels[0], wdth * hght * 4 ) ;
        {$ENDIF}
      bmp.UnlockPixels ;
      finally
        SetLength( plock, 0 ) ;
        if assigned( bmp ) then
          pdataTEX_tDX := create_texture( pD3DDevice, bmp ) ;
        FreeObject( bmp ) ;
      end;
    end;

    function setMeshExt : TGIS_Extent ;
    var
      dx,dy : Double ;
      ex    : TGIS_Extent ;
    begin
      dx := mapExtent.XMax - mapExtent.XMin ;
      dy := mapExtent.YMax - mapExtent.YMin ;
      ex := GisExtent( mapExtent.XMin - detailCoeff*dx,
                       mapExtent.YMin - detailCoeff*dy,
                       mapExtent.XMax + detailCoeff*dx,
                       mapExtent.YMax + detailCoeff*dy ) ;
      ex := GisCommonExtent( ex,  exExtent ) ;
      ex := GisCommonExtent( ex, _lp.Extent ) ;
      Result := ex ;
    end;

  begin

    shp := TGIS_ShapePolygon.Create(nil, nil, False, 1, nil, TGIS_DimensionType.XYZ) ;
    if _lp.IsGrid = False then begin  // Image
      tinBmp := True ;
      z := _lp.Params.FalseZ ;

      if GisIsCommonExtent( mapExtent, _lp.Extent ) = False then begin
        FreeObject( shp ) ;
        exit;
      end;
      ext := setMeshExt ;
      setMeshTex ;

      pnt1 := GisPoint3D(ext.XMin , ext.YMin , z) ;
      pnt2 := GisPoint3D(ext.XMin , ext.YMax , z) ;
      pnt3 := GisPoint3D(ext.XMax , ext.YMax , z) ;
      shp.Reset ;
      shp.Lock(TGIS_Lock.Projection) ;
      shp.AddPart ;
      shp.AddPoint3D( pnt1 ) ;
      shp.AddPoint3D( pnt2 ) ;
      shp.AddPoint3D( pnt3 ) ;
      shp.Params.Area.Color := TGIS_Color.White ;
      shp.Unlock ;
      abort := False ;
      doCallBackTin( shp, abort ) ;

      pnt1 := GisPoint3D(ext.XMin , ext.YMin , z) ;
      pnt2 := GisPoint3D(ext.XMax , ext.YMax , z) ;
      pnt3 := GisPoint3D(ext.XMax , ext.YMin , z) ;
      shp.Reset ;
      shp.Lock(TGIS_Lock.Projection) ;
      shp.AddPart ;
      shp.AddPoint3D( pnt1 ) ;
      shp.AddPoint3D( pnt2 ) ;
      shp.AddPoint3D( pnt3 ) ;
      shp.Params.Area.Color := TGIS_Color.White ;
      shp.Unlock ;
      doCallBackTin( shp, abort ) ;
    end
    else begin // Dem
      ls := lodStep ;
      ext := mapExtent ; // _lp.Extent ;
      init_arGrid( arGrid_w ) ;
      _lp.GetGrid( ext, arGrid_w ) ;
      xm := ext.XMin ;
      dx := (ext.XMax - ext.XMin) / (demFrameSize -1) ;
      ym := ext.YMax ;
      dy := (ext.YMax - ext.YMin) / (demFrameSize -1) ;
      i := 0 ;
      while True do begin
        j := ls;
        while True do begin
          if (arGrid_w[j][i] <>  GIS_GRID_NOVALUE) and
             (arGrid_w[j-ls][i] <>  GIS_GRID_NOVALUE) and
             (arGrid_w[j][i+ls] <>  GIS_GRID_NOVALUE) and
             (arGrid_w[j-ls][i+ls] <>  GIS_GRID_NOVALUE) then begin
            z := arGrid_w[j][i]*_lp.Params.ScaleZ + _lp.Params.FalseZ ;
            pnt1 := GisPoint3D(xm+i*dx     , ym-j*dy     , z) ;
            z := arGrid_w[j-ls][i]*_lp.Params.ScaleZ + _lp.Params.FalseZ ;
            pnt2 := GisPoint3D(xm+i*dx     , ym-(j-ls)*dy, z) ;
            z := arGrid_w[j][i+ls]*_lp.Params.ScaleZ + _lp.Params.FalseZ ;
            pnt3 := GisPoint3D(xm+(i+ls)*dx , ym-j*dy    , z) ;

            shp.Reset ;
            shp.Lock(TGIS_Lock.Projection) ;
            shp.AddPart ;
            shp.AddPoint3D( pnt1 ) ;
            shp.AddPoint3D( pnt2 ) ;
            shp.AddPoint3D( pnt3 ) ;
            shp.Params.Area.Color := TGIS_Color.White ;
            shp.Unlock ;
            abort := True ;
            doCallBackTin( shp, abort ) ;

            z := arGrid_w[j][i+ls]*_lp.Params.ScaleZ + _lp.Params.FalseZ ;
            pnt1 := GisPoint3D(xm+(i+ls)*dx , ym-j*dy    , z) ;
            z := arGrid_w[j-ls][i]*_lp.Params.ScaleZ + _lp.Params.FalseZ ;
            pnt2 := GisPoint3D(xm+i*dx     , ym-(j-ls)*dy, z) ;
            z := arGrid_w[j-ls][i+ls]*_lp.Params.ScaleZ + _lp.Params.FalseZ ;
            pnt3 := GisPoint3D(xm+(i+ls)*dx , ym-(j-ls)*dy, z) ;

            shp.Reset ;
            shp.Lock(TGIS_Lock.Projection) ;
            shp.AddPart ;
            shp.AddPoint3D( pnt1 ) ;
            shp.AddPoint3D( pnt2 ) ;
            shp.AddPoint3D( pnt3 ) ;
            shp.Params.Area.Color := TGIS_Color.White ;
            shp.Unlock ;
            abort := True ;
            doCallBackTin( shp, abort ) ;
          end;
          inc( j, ls ) ;
          if (j >= demFrameSize - ls) then break ;
        end;
        inc( i , ls ) ;
        if (i >= demFrameSize - ls)  then break;
      end;
    end;
    FreeObject( shp ) ;
  end;

  function TGIS_Renderer3DDirectX9.draw3DVector
    : Integer ;
  var
    i         : Integer          ;
    lv        : TGIS_LayerVector ;
    a         : Cardinal         ;
    r,g,b     : Byte             ;
    bmp       : TGIS_Bitmap      ;
    old_zu    : Double           ;
    call_type : Boolean          ;
    ext       : TGIS_Extent      ;

  begin
    Result := S_OK ;
    currentSHP := nil ;

    if curMeshSize > 0 then begin
      currentVL := nil ;
      pD3DDevice.BeginScene ;
      if reRenderCall or firstScene then begin
        for i := 0 to curMeshSize -1 do begin
          tinBmp := False ;
          currentML := curMesh[i].Lx ;
          projFactor := curMesh[i].Zu ;

          if curMesh[i].Lx.IsGrid then begin
            bmp := TGIS_Bitmap.Create( TruncS(imgFrameSize*curMesh[i].Lx.CellWidth/curMesh[i].Lx.CellHeight), imgFrameSize ) ;
            oGIS.ViewerParent.ControlDrawTexture( bmp, curMesh[i].Lx,
                                  curMesh[i].Lx.ProjectedExtent, oGIS.PPI ) ;
            pdataTEX_tDX := create_texture( pD3DDevice, bmp ) ;
            FreeObject( bmp ) ;
            tinBmp := True ;
            prepareMesh( curMesh[i].Lx ) ;
            saveTinBufT ;
          end
          else begin
            prepareMesh( curMesh[i].Lx ) ;
            saveTinBufT ;
          end;
        end;
        tinBmp := False ;
      end;

      if curVLSize = 0 then
        drawMeshTinTiles ;

      currentML := nil ;
      pD3DDevice.EndScene ;
    end;

    if curVLSize = 0 then begin
        exit ;
    end
    else begin
      SetLength( curVLlist, curVLSize ) ;
      curVLListSize := curVLSize ;
    end;

    if bPartialUpdate  then begin
      if vectorTilesInfoCounter > 0 then begin
        if partialRender then exit ;
      end
      else begin
        bPartialUpdate := False ;
        shpNo := 0 ;
      end;
    end;

    try
      call_type := reRenderCall ;
      a := 255 ;
      pixSize := GetPixelSize.X ;

      pD3DDevice.BeginScene;
      try
        pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
        pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
        pD3DDevice.SetTexture(0,nil) ;

        if lightSwitch then
          lightOn
        else
          lightOff ;

        if curVLListSize <> curVLSize then
        if shpNo <> 0 then
          shpNo := 0 ;

//?        if shpNo = 0 then reRenderCall := True ;

        old_zu := zUnit ;
        for i:= 0 to curVLSize-1 do begin
          lv := curVL[i].Lv ;
          currentVL := lv ;
          currentVLnz  := curVL[i].Nz  ;
          currentVLnm  := curVL[i].Nm  ;
          currentVLzmi := curVL[i].Zmi ;
          currentVLmmi := curVL[i].Mmi ;
          projFactor   := curVL[i].Zu  ;
          projMFactor  := curVL[i].Fx  ;

//          ext := GisCommonExtent( oGIS.VisibleExtent, currentVL.ProjectedExtent ) ;
          ext := currentVL.ProjectedExtent ;
          ext := currentVL.UnprojectExtent( ext ) ;
          cutVectExtentL := GisCommonExtent( ext, currentVL.Extent ) ;

          if not currentVL.CachedPaint then
            cutVectExtentV := cutVectExtentL
          else
            cutVectExtentV := oldcutVectExtentV ;

          a := TruncS (currentVL.Transparency/100*255) ;
          r := edgesColor.R ;
          g := edgesColor.G ;
          b := edgesColor.B ;

          edColor := color2ARGB(a,r,g,b) ;

          setCVLTransparency ;
          if lv.View3D.Mode = TGIS_3DLayerType.Dem then begin   // TIN
            tinBmp := False ;
            if not TGIS_Bitmap.IsNilOrEmpty( currentVL.Params.Area.Bitmap ) then
            begin
              bmp := currentVL.Params.Area.Bitmap ;
              tinBmp := True ;
//              if bmp <> curTxt then begin
                pdataTEX_tDX := T_textureHelperDX( oTextureHelper ).Get(
                  T_textureHelperDX( oTextureHelper ).Prepare( pD3DDevice, bmp)) ;
                if a <> 255 then setTextureOperations ;
//              end;
            end;

            if call_type or firstScene then begin

            {$IFDEF CLR}
              if not lv.ForEach( cutVectExtentL, '', nil, '', True,
                                 oGIS.ScaleAsFloat, @doCallBackTin ) then begin
            {$ELSE}
              if not lv.ForEach( cutVectExtentL, '', nil, '', True,
                                 oGIS.ScaleAsFloat, doCallBackTin ) then begin
            {$ENDIF}
                if shpNo > 0 then begin
                  // turn off the color blending
                  if currentVL.Transparency <> 100 then
                    blendOff ;
                  zUnit := old_zu ;
                  raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                               'draw3dVector', 0 ) ;
                end;
              end;
              if tinBmp then begin
                curTxt := bmp ;
                saveTinBufT ;
                pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
                pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
                pD3DDevice.SetTexture(0,nil) ;
              end
              else
                saveTinBufC ;
//              drawMeshTinTiles ;  // moved  below
            end
            else begin
              if tinBmp then curTxt := bmp ;
//              drawMeshTinTiles ; // moved  below
            end;
          end;

          if lv.View3D.Mode = TGIS_3DLayerType.Shapes then begin  // VECTOR
            if reRenderCall then begin   // reading
              pntNo := -1 ;
              LabelsReg.Reset ;
              shpIsTin := isTinLayer ;
              {$IFDEF CLR}
                if not lv.ForEach( cutVectExtentL, '', nil, '', True,
                                   oGIS.ScaleAsFloat, @addShapeToCache ) then begin
              {$ELSE}
                if not lv.ForEach( cutVectExtentL, '', nil, '', True,
                               oGIS.ScaleAsFloat,  addShapeToCache ) then begin
              {$ENDIF}
                if shpNo > 0 then begin
                  if assigned(currentVL) then
                  // turn off the color blending
                  if currentVL.Transparency <> 100 then
                    blendOff ;
                  zUnit := old_zu ;
                  raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                               'draw3dVector', 1 ) ;
                end;
              end
              else begin
                curVLlist[i] := shpNo ;
                if currentVL.CachedPaint = False then begin
                   if ((i = 0) and (shpNo = 0)) or
                      ((i > 0) and (curVLlist[i-1] = curVLlist[i])) then begin
                     addVectorTilesInfo(1,high(meshVectTilesCW)+1);
                     addVectorTilesInfo(2,high(meshVectTilesCR)+1);
                   end;
                end;
              end;

              addNewPolyRoofRow ;

              saveVTLBufC(True , vTLBufCR) ; // roofs , color
              saveVTLBufC(False, vTLBufCW) ; // walls , color
              saveVTLBufT(True , vTLBufTR) ; // roofs , texture
              saveVTLBufT(False, vTLBufTW) ; // walls , texture
              saveWPLBuf                   ; // edges
              saveWPBuf                    ; // points
              saveMPatchBufC               ; // multi patch, color
              saveMPatchBufT               ; // multi patch, texture

            end;
          end; // vector

        end; // for i

        if a <> 255 then setTextureOperations ;

        if not reRenderCall then begin
          drawVectTiles    ;
          drawMeshLinTiles ;
          drawMeshPntTiles ;
          drawMPatchTiles  ;
          drawMeshTinTiles ;   // moved from TIN section
        end;

        zUnit := old_zu ;

        if assigned(currentVL) then
        // turn off the color blending
        if currentVL.Transparency <> 100 then blendOff ;
        pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
      finally
        pD3DDevice.EndScene ;
      end;

      Result := S_OK ;
    except
      on e : Exception do begin
        if not assigned( currentVL ) then begin
          if not oGIS.NotifyPaintException( 'draw3DVector', e ) then
            Result := -1; // raise ;
        end
        else begin
          if not oGIS.NotifyPaintException( 'draw3DVector: ' + currentVL.Name, e ) then
            Result := -1; // raise
        end ;
      end ;
    end;
  end;

  procedure TGIS_Renderer3DDirectX9.drawLabels ;
  var
    i       : Integer       ;
    ptc     : TGIS_Point3D  ;
    r, ps3d ,
    radius  : Double        ;
    vp      : TD3DViewport9 ;
    vec1,
    vRayPos : TD3DXVector3  ;

      function dist( _x1,_y1,_z1 : Single) : Single ;
      var
        c1,c2,c3,s1,s2,s3 : Double ;
        X,Y,Z             : Double ;
        res               : TD3DXVector3 ;

        function rotx : TD3DXVector3 ;
        begin
          Result.X := X    + 0    + 0    ;
          Result.Y := 0    + c1*Y - s1*Z ;
          Result.Z := 0    + s1*Y + c1*Z ;
        end ;

        function roty : TD3DXVector3 ;
        begin
          Result.X := c2*X + 0    + s2*Z ;
          Result.Y := 0    + Y    + 0    ;
          Result.Z :=-s2*X + 0    + c2*Z ;
        end ;

        function rotz : TD3DXVector3 ;
        begin
          Result.X := c3*X - s3*Y + 0    ;
          Result.Y := s3*X + c3*Y + 0    ;
          Result.Z := 0    + 0    + Z    ;
        end ;

      begin
        SinCos( DegToRad(spinX), s1, c1 ) ;
        SinCos( DegToRad(spinY), s2, c2 ) ;
        SinCos( DegToRad(spinZ), s3, c3 ) ;
        X  := _x1 + dTurn ;
        Y  := _y1 + dFly ;
        Z  := _z1 - zEyeTo ;
        res := rotz ;
        X  := res.X ;
        Y  := res.Y ;
        Z  := res.Z ;
        res := roty ;
        X  := res.X ;
        Y  := res.Y ;
        Z  := res.Z ;
        res := rotx ;
        Result := radius - res.Z ;
      end;

  begin
    if labelModeEx and optionSelected  then
      labelMode := True ;

    if (curVLSize = 0) or (labelMode = False) or (high(labelInfo) = -1) then
      exit ;
    if reRenderCall then exit ;

    if newTransform then begin
      pD3DDevice.GetViewport(vp) ;
      vec1.X := vp.Width / 2 ;
      vec1.Y := vp.Height/ 2 ;
      vec1.Z := 0.0 ;
      Vec3Unproject(vRayPos,vec1,vp,mtxProjection,mtxView,mtxWorld) ;
      if vRayPos.Z = 0 then vRayPos.Z := cameraPositionEx.Z ;
      radius := Sqrt(Sqr(-dTurn - vRayPos.X) +
                     Sqr(  dFly + vRayPos.Y) +
                     Sqr(zEyeTo - vRayPos.Z) ) ;
    end
    else
      radius := dRadius ;

    ps3d := getPrinterPixelSize( radius ) ;

    pD3DDevice.BeginScene ;
    try
      if lightSwitch then
        lightOff ;
      pD3DDevice.SetStreamSource(0, pdataVB, 0, sizeOf(TGIS_Renderer3DVertex)) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
      pD3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1) ;
      pD3DDevice.SetRenderState(D3DRS_SRCBLEND,D3DBLEND_SRCALPHA) ;
      pD3DDevice.SetRenderState(D3DRS_DESTBLEND,D3DBLEND_INVSRCALPHA) ;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;

      pD3DDevice.SetSamplerState( 0, D3DSAMP_MAGFILTER    , D3DTEXF_NONE  ) ;

      for i := 0 to high(labelInfo) do begin
        ptc := GisPoint3D(labelInfo[i].Centroid.X, labelInfo[i].Centroid.Y, 0, 0) ;
        ptc := mapToD3D( ptc ) ;
        ptc.Z := labelInfo[i].LabelZ ;

        r := dist( ptc.X, -ptc.Y, ptc.Z ) ;
        r := r / radius ;
        pixelSize3D := ps3d * r ;
        drawLabel( i ) ;
      end;

      blendOff ;
      pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
      pD3DDevice.SetTexture(0,nil) ;
      if lightSwitch then
        lightOn ;

      pD3DDevice.SetSamplerState( 0, D3DSAMP_MAGFILTER    , D3DTEXF_LINEAR ) ;
    finally
      pD3DDevice.EndScene ;
    end;

    if labelModeEx and optionSelected  then  begin
      labelMode := False ;
      optionSelected := False ;
    end;
  end;


  procedure TGIS_Renderer3DDirectX9.drawLabel(
    const _k   : Integer
  ) ;
  var
    pnt       : TPoint          ;
    ptg       : TGIS_Point3D    ;
    pos       : TGIS_Point      ;
    pardata   : T_arTextureData ;
    {$IFDEF CLR}
      pdata   : DirectXStream   ;
    {$ELSE}
      pdata   : PByte           ;
    {$ENDIF}
    deltax,
    deltay    : Double          ;
    X,Y,Z     : Double          ;
    lz        : Double          ;
    oldworld,
    newworld,
    matrot,
    matrotx,
    matrotz,
    matrob,
    mattransl,
    matscale  : TD3DXMatrix     ;
    vp        : TD3DViewport9   ;
    vec1,
    vec2      : TD3DXVector3    ;
    ps : Integer ;
    margin : Integer ;
  begin
    if labelInfo[_k].LabelTextureDX = nil then
      exit
    else
      pdataTEX_vDX := labelInfo[_k].LabelTextureDX ;

    lz := labelInfo[_k].LabelZ ;

    margin := oGIS.OverlappedExtentMargin ;
    pos := labelInfo[_k].Centroid ;
    ptg.X := pos.X ;
    ptg.Y := pos.Y ;
    ptg.Z := 0 ;
    ptg.M := 0 ;
    ptg := mapToD3D(ptg) ;
    ptg.Y := -ptg.Y ;
    pnt := mapToScreen2(pos,lz) ;
    if ((pnt.X <  -margin) or
        (pnt.Y <  -margin) or
        (pnt.X >= windowWidth  + margin) or
        (pnt.Y >= windowHeight + margin)) and
         not captureScreen then exit ;

    if pdataVB_Lock( pdata ) <> 0 then exit;
    SetLength( pardata, 4 ) ;

    X := 0 ;
    Y := 0 ;
    Z := 0 ;

    deltax := (labelInfo[_k].LabelSize.X) * pixelSize3D ;
    deltay := (labelInfo[_k].LabelSize.Y) * pixelSize3D ;

    vec1.X := -deltax-pixelSize3D ;
    vec1.Y := -deltay-pixelSize3D ;
    vec1.Z := 0 ;

    if GisTestLabelPosition( TGIS_LabelPosition.MiddleCenter, labelInfo[_k].LabelPos ) then
      ps := 5
    else
    if GisTestLabelPosition( TGIS_LabelPosition.MiddleRight, labelInfo[_k].LabelPos ) then
      ps := 6
    else
    if GisTestLabelPosition( TGIS_LabelPosition.MiddleLeft, labelInfo[_k].LabelPos ) then
      ps := 4
    else
    if GisTestLabelPosition( TGIS_LabelPosition.UpCenter, labelInfo[_k].LabelPos ) then
      ps := 2
    else
    if GisTestLabelPosition( TGIS_LabelPosition.DownCenter, labelInfo[_k].LabelPos ) then
      ps := 8
    else
    if GisTestLabelPosition( TGIS_LabelPosition.UpRight, labelInfo[_k].LabelPos ) then
      ps := 3
    else
    if GisTestLabelPosition( TGIS_LabelPosition.UpLeft, labelInfo[_k].LabelPos ) then
      ps := 1
    else
    if GisTestLabelPosition( TGIS_LabelPosition.DownRight, labelInfo[_k].LabelPos ) then
      ps := 9
    else
    if GisTestLabelPosition( TGIS_LabelPosition.DownLeft, labelInfo[_k].LabelPos ) then
      ps := 7
    else
    ps := 2 ;

    case ps  of
       1 : begin
             vec1.X := -2.0*deltax ;
             vec1.Y := -2.0*deltay;
           end;
       2 : begin
             vec1.X := -1.0*deltax ;
             vec1.Y := -2.0*deltay ;
           end;
       3 : begin
             vec1.X :=  0.0*deltax ;
             vec1.Y := -2.0*deltay ;
           end;
       4 : begin
             vec1.X := -2.0*deltax ;
             vec1.Y := -1.0*deltay ;
           end;
       5 : begin
             vec1.X := -1.0*deltax ;
             vec1.Y := -1.0*deltay ;
           end;
       6 : begin
             vec1.X :=  0.0*deltax ;
             vec1.Y := -1.0*deltay ;
           end;
       7 : begin
             vec1.X := -2.0*deltax ;
             vec1.Y :=  0.0*deltay ;
           end;
       8 : begin
             vec1.X := -1.0*deltax ;
             vec1.Y :=  0.0*deltay ;
           end;
       9 : begin
             vec1.X :=  0.0*deltax ;
             vec1.Y :=  0.0*deltay ;
           end;
    end;

    pD3DDevice.GetViewport(vp) ;
    pD3DDevice.GetTransform(D3DTS_WORLD,oldworld) ;

    MatrixRotationX(matrotx,DegToRad(-spinX)) ;
    MatrixRotationZ(matrotz,DegToRad(-spinZ)) ;
    MatrixTranslation(mattransl, ptg.X, ptg.Y, lz + deltay) ;
    MatrixScaling(matscale, 1.0, 1.0, 1.0 ) ;
    MatrixMultiply(matrot   , matrotx  , matrotz) ;
    MatrixMultiply(matrob   , matrot   , mattransl) ;
    MatrixMultiply(newworld , matscale , matrob) ;
    MatrixMultiply(newworld , newworld , oldworld) ;

    Vec3Project(vec2,vec1,vp,mtxProjection,mtxView,newworld) ;
    vec2.X := RoundS(vec2.X) ;
    vec2.Y := RoundS(vec2.Y) ;
    Vec3Unproject(vec1,vec2,vp,mtxProjection,mtxView,newworld) ;

    pardata[0].P.X := vec1.X - pixelSize3D ;
    pardata[0].P.Y := vec1.Y - pixelSize3D ;
    pardata[0].P.Z := Z ;
    pardata[0].N   := vector3InitDX9(0,1,0) ;
    pardata[0].Tu  := 0.0 ;
    pardata[0].Tv  := 0.0 ;
    pardata[0].Color := D3DCOLOR_ARGB(255,255,255,255) ;

    pardata[1].P.X := pardata[0].P.X ;
    pardata[1].P.Y := pardata[0].P.Y + 2.0*deltay ;
    pardata[1].P.Z := Z ;
    pardata[1].N   := vector3InitDX9(0,1,0) ;
    pardata[1].Tu  := 0.0 ;
    pardata[1].Tv  := labelInfo[_k].LabelSize.Y / labelInfo[_k].BitmapSize.Y ;
    pardata[1].Color := D3DCOLOR_ARGB(255,255,255,255) ;

    pardata[2].P.X := pardata[0].P.X + 2.0*deltax ;
    pardata[2].P.Y := pardata[0].P.Y ;
    pardata[2].P.Z := Z ;
    pardata[2].N   := vector3InitDX9(0,1,0) ;
    pardata[2].Tu  := labelInfo[_k].LabelSize.X / labelInfo[_k].BitmapSize.X ;
    pardata[2].Tv  := 0.0 ;
    pardata[2].Color := D3DCOLOR_ARGB(255,255,255,255) ;

    pardata[3].P.X := pardata[0].P.X + 2.0*deltax ;
    pardata[3].P.Y := pardata[0].P.Y + 2.0*deltay ;
    pardata[3].P.Z := Z ;
    pardata[3].N   := vector3InitDX9(0,1,0) ;
    pardata[3].Tu  := labelInfo[_k].LabelSize.X / labelInfo[_k].BitmapSize.X ;
    pardata[3].Tv  := labelInfo[_k].LabelSize.Y / labelInfo[_k].BitmapSize.Y ;
    pardata[3].Color := D3DCOLOR_ARGB(255,255,255,255) ;

    pdataVB_Unlock( pdata, pardata ) ;

    pD3DDevice.SetTexture(0, pdataTEX_vDX) ;
    pD3DDevice.SetTransform(D3DTS_WORLD, newworld) ;
    pD3DDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2) ;
    pD3DDevice.SetTransform(D3DTS_WORLD,oldworld) ;
  end;

  function TGIS_Renderer3DDirectX9.northArrow(
    const _value : Boolean
  ) : Integer ;
  var
    {$IFDEF CLR}
      linelist   : Array of TGIS_Renderer3DVertex ;
    {$ELSE}
      linelist   : Array [0..17] of TGIS_Renderer3DVertex ;
    {$ENDIF}
    num_val,
    num_tri      : Integer                ;
    {$IFDEF CLR}
      vertices  : DirectXStream          ;
    {$ELSE}
      vertices  : PByte                  ;
    {$ENDIF}
    plinelist_vb : IDirect3DVertexBuffer9 ;
    dx, Z        : Double                 ;
    dnx, dny     : Double                 ;
    dt, df       : Double                 ;
    a            : Cardinal               ;
  begin
    Result := S_OK ;
    if reRenderCall then exit ;

    pD3DDevice.BeginScene ;
    try
      {$IFDEF CLR}
        SetLength( linelist, 18 ) ;
      {$ENDIF}
      dx := wndXSize ;
      if scrRatio > 1.0 then
        dx := dx * scrRatio ;
      num_val := 18;
      num_tri := 7;
      Z := zEyeTo+zOffset+1 ;
      if newTransform then begin
        dt :=  eyeX ;
        df := -eyeY ;
      end
      else begin
        dt  := -dTurn   ;
        df  := -dFly    ;
      end;

      // Black north arrow
      linelist[0].P := vector3InitDX9(dt,df-dx/10,Z) ;
      linelist[0].N := vector3InitDX9(0,1,0) ;
      linelist[0].Color := $FF000000 ;

      linelist[1].P := vector3InitDX9(dt-dx/120,df-dx/12.5,Z) ;
      linelist[1].N := vector3InitDX9(0,1,0) ;
      linelist[1].Color := $FF000000 ;

      linelist[2].P := vector3InitDX9(dt,df-dx/10,Z) ;
      linelist[2].N := vector3InitDX9(0,1,0) ;
      linelist[2].Color := $FF000000 ;

      linelist[3].P := vector3InitDX9(dt,df,Z) ;
      linelist[3].N := vector3InitDX9(0,1,0) ;
      linelist[3].Color := $FF000000 ;

      linelist[4].P := vector3InitDX9(dt,df,Z) ;
      linelist[4].N := vector3InitDX9(0,1,0) ;
      linelist[4].Color := $FF000000 ;

      linelist[5].P := vector3InitDX9(dt,df,defaultZ) ;
      linelist[5].N := vector3InitDX9(0,1,0) ;
      linelist[5].Color := $FF000000 ;

      linelist[6].P := vector3InitDX9(dt,df-dx/10,Z) ;
      linelist[6].N := vector3InitDX9(0,1,0) ;
      linelist[6].Color := $FF000000 ;

      linelist[7].P := vector3InitDX9(dt+dx/120,df-dx/12.5,Z) ;
      linelist[7].N := vector3InitDX9(0,1,0) ;
      linelist[7].Color := $FF000000 ;

      // Axes
      Z := zEyeTo+zOffset ;
      if newTransform then begin
        dnx :=  eyeX ;
        dny := -eyeY ;
        dt  := 0     ;
        df  := 0     ;
      end
      else begin
        dnx := eyeTo.X ;
        dny := eyeTo.Y ;
        dt  := -dTurn  ;
        df  := -dFly   ;
      end;
      linelist[8].P := vector3InitDX9(dt+dnx,df+dny,Z) ;
      linelist[8].N := vector3InitDX9(0,1,0) ;
      linelist[8].Color := $FFFF0000 ;

      linelist[9].P := vector3InitDX9(dt+dnx+dx,df+dny,Z) ;
      linelist[9].N := vector3InitDX9(0,1,0) ;
      linelist[9].Color := $FFFF0000 ;

      linelist[10].P := vector3InitDX9(dt+dnx,df+dny,Z) ;
      linelist[10].N := vector3InitDX9(0,1,0) ;
      linelist[10].Color := $FF00FF00 ;

      linelist[11].P := vector3InitDX9(dt+dnx,df+dny+dx,Z) ;
      linelist[11].N := vector3InitDX9(0,1,0) ;
      linelist[11].Color := $FF00FF00 ;

      linelist[12].P := vector3InitDX9(dt+dnx,df+dny,Z) ;
      linelist[12].N := vector3InitDX9(0,1,0) ;
      linelist[12].Color := $FF0000FF ;

      linelist[13].P := vector3InitDX9(dt+dnx,df+dny,Z+dx) ;
      linelist[13].N := vector3InitDX9(0,1,0) ;
      linelist[13].Color := $FF0000FF ;

      // flood  surface
      Z := zScaleFactor * (flood.Level- zLevel) / zUnit;
      a := TruncS(flood.Transparency/100.0*255.0) ;
      {$IFDEF CLR}
        a := D3DCOLOR_ARGB(a, flood.Color.B, flood.Color.G, flood.Color.R) ;
      {$ELSE}
        a := D3DCOLOR_ARGB(a, flood.Color.R, flood.Color.G, flood.Color.B) ;
      {$ENDIF}
      dnx := (normExt.XMax - normExt.XMin)/50.0 ;
      dny := (normExt.YMax - normExt.YMin)/50.0 ;

      linelist[14].P.X := normExt.XMin-dnx ;
      linelist[14].P.Y := normExt.YMin-dny ;
      linelist[14].P.Z := Z ;
      linelist[14].Color := a ;
      linelist[15].P.X := normExt.XMax+dnx ;
      linelist[15].P.Y := normExt.YMin-dny ;
      linelist[15].P.Z := Z ;
      linelist[15].Color := a ;

      linelist[16].P.X := normExt.XMin-dnx ;
      linelist[16].P.Y := normExt.YMax+dny ;
      linelist[16].P.Z := Z ;
      linelist[16].Color := a ;

      linelist[17].P.X := normExt.XMax+dnx ;
      linelist[17].P.Y := normExt.YMax+dny ;
      linelist[17].P.Z := Z ;
      linelist[17].Color := a ;

      linelist[14].N  := triangleNormal( linelist[16].P,
                                         linelist[14].P,
                                         linelist[15].P ) ;
      linelist[15].N  := triangleNormal( linelist[14].P,
                                         linelist[15].P,
                                         linelist[16].P ) ;
      linelist[16].N  := triangleNormal( linelist[17].P,
                                         linelist[16].P,
                                         linelist[15].P ) ;
      linelist[17].N  := triangleNormal( linelist[15].P,
                                         linelist[17].P,
                                         linelist[16].P ) ;

      // Create the vertex buffer
      if pD3DDevice.CreateVertexBuffer(num_val*sizeOf(TGIS_Renderer3DVertex),
                                       D3DUSAGE_DYNAMIC or D3DUSAGE_WRITEONLY,
                                       GISVIEWER3D_D3DFVF_GRAPH3DCOLOR,
                                       D3DPOOL_DEFAULT,
                                       plinelist_vb,
                                       nil) <> 0 then begin
        Result := E_FAIL ;
        exit ;
      end;

      {$IFDEF CLR}
        if plinelist_vb.Lock(0, 0, vertices, D3DLOCK_NONE) <> 0 then begin
      {$ELSE}
        if plinelist_vb.Lock(0, 0, Pointer(vertices), D3DLOCK_NONE) <> 0
                                                                 then begin
      {$ENDIF}
        Result := E_FAIL ;
        exit ;
      end;

      {$IFDEF CLR}
        vertices.WriteRange( linelist ) ;
      {$ELSE}
        Move(linelist, vertices^, sizeOf(linelist)) ;
      {$ENDIF}

      {$IFDEF CLR}
        try
          plinelist_vb.Unlock ;
        except
          Result := E_FAIL ;
          exit ;
        end ;
      {$ELSE}
        if plinelist_vb.Unlock <> 0 then begin
          Result := E_FAIL ;
          exit ;
        end;
      {$ENDIF}

      pD3DDevice.SetStreamSource( 0, plinelist_vb, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
      pD3DDevice.SetTexture(0,nil) ;

      if _value then
      if bArrow then
        pD3DDevice.DrawPrimitive( D3DPT_LINELIST, 0, num_tri) ;
      if not _value then
      if flood.Active then begin
        // turn on the color blending
        pD3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1) ;
        // set source factor
        pD3DDevice.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA) ;
        // set dest factor
        pD3DDevice.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA) ;
        // set the operation
        pD3DDevice.SetRenderState(D3DRS_BLENDOP, D3DBLENDOP_ADD) ;
        // draw
        pD3DDevice.DrawPrimitive( D3DPT_TRIANGLESTRIP, 14, 2) ;
        // turn off the color blending
        blendOff ;
      end;

      if assigned( plinelist_vb ) then
        ReleaseInterface( plinelist_vb ) ;
    finally
      pD3DDevice.EndScene ;
    end;
    Result := S_OK ;
  end;

  procedure TGIS_Renderer3DDirectX9.setTextureOperations ;
  begin
    // take the result from argument 1
    pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAOP,D3DTOP_SELECTARG1) ;
    // set D3DTSS_ALPHAARG1 to D3DTA_DIFFUSE
    pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_DIFFUSE) ;
    // set source factor
    pD3DDevice.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA) ;
    // set dest factor
    pD3DDevice.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA) ;
    // turn on the color blending
    pD3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1) ;
  end;

  procedure TGIS_Renderer3DDirectX9.setCVLTransparency ;
  begin
    if currentVL.Transparency <> 100 then begin
      // set source factor
      pD3DDevice.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA) ;
      // set dest factor
      pD3DDevice.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA) ;
      // set the operation
      pD3DDevice.SetRenderState(D3DRS_BLENDOP, D3DBLENDOP_ADD) ;
      // turn on the color blending
      pD3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1) ;
    end;
  end;

  function TGIS_Renderer3DDirectX9.doRender(
    const _rows   : DWORD ;
    const _detail : Boolean
  ) : Integer ;
  var
   i, ip, ik : Integer ;
   mode      : Boolean ;
   hr        : HResult ;
  begin
    {$IFDEF CLR}
      try
        pD3DDevice.BeginScene;
        hr := S_OK ;
      except
        hr := S_FALSE ;
      end ;
    {$ELSE}
      hr := pD3DDevice.BeginScene;
    {$ENDIF}
    if hr <> 0 then begin
      Result := Integer(hr) ;
      exit ;
    end;


    if imageTexture = False then begin
      {$IFDEF CLR}
        try
          pD3DDevice.SetTexture(0,pdataTEX_w) ;
          hr := S_OK ;
        except
          hr := S_FALSE ;
        end ;
      {$ELSE}
        hr := pD3DDevice.SetTexture(0,pdataTEX_w) ;
      {$ENDIF}
      curTEX := 0 ;
    end
    else begin
      if _detail then begin
        {$IFDEF CLR}
          try
            pD3DDevice.SetTexture(0,pdataTEX_d) ;
            hr := S_OK ;
          except
            hr := S_FALSE ;
          end ;
        {$ELSE}
          hr := pD3DDevice.SetTexture(0,pdataTEX_d) ;
        {$ENDIF}
        curTEX := 2 ;
      end
      else  begin
        {$IFDEF CLR}
          try
            pD3DDevice.SetTexture(0,pdataTEX_r) ;
            hr := S_OK ;
          except
            hr := S_FALSE ;
          end ;
        {$ELSE}
          hr := pD3DDevice.SetTexture(0,pdataTEX_r) ;
        {$ENDIF}
        curTEX := 1 ;
      end;
    end;

    if hr <> 0 then begin
      Result := Integer(hr) ;
      exit ;
    end;

    pD3DDevice.SetStreamSource(0, pdataVB, 0, sizeOf(TGIS_Renderer3DVertex)) ;
    {$IFDEF CLR}
      try
        pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
        hr := S_OK ;
      except
        hr := S_FALSE ;
      end ;
    {$ELSE}
      hr := pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
    {$ENDIF}
    if hr <> 0 then begin
      Result := Integer(hr) ;
      exit ;
    end;

    ip := 0 ;
    ik := _rows ;

    if _detail then begin
      ip := detWnd.Top + 1 ;
      ik := detWnd.Bottom ;
      mode := True ;
    end
    else begin
      xMin :=  roughExtent.XMin ;
      yMin :=  roughExtent.YMin ;
      xMax :=  roughExtent.XMax ;
      yMax :=  roughExtent.YMax ;
      {$IFDEF CLR}
        detWnd := Rect( vertexBufferSize, vertexBufferSize, 0, 0 ) ;
      {$ELSE}
        detWnd.Left   := vertexBufferSize ;
        detWnd.Right  := 0 ;
        detWnd.Top    := vertexBufferSize ;
        detWnd.Bottom := 0 ;
      {$ENDIF}
      mode := False ;
    end;

    if lightSwitch then
      lightOn
    else
      lightOff ;

    firstRowFound := False ;
    currentVL := nil ;
    if (noDem = False) or (noImg = False) then
    if demMeshExists then
      drawMeshTiles
    else
    for i:= ip to ik do  begin
      fillVertexBuffer(i, mode) ;
    end;

    {$IFDEF CLR}
      try
        pD3DDevice.EndScene ;
        hr := S_OK ;
      except
        hr := S_FALSE ;
      end ;
    {$ELSE}
      hr := pD3DDevice.EndScene ;
    {$ENDIF}

    Result := Integer(hr) ;
  end;

  function TGIS_Renderer3DDirectX9.saveWTLBufT
    : Integer;
  begin
    if demWall = TGIS_Viewer3DDemWall.Invisible then  begin
      wTLBufT.NumObjects := 0 ;
      wTLBufT.NumPoints  := 0 ;
      Result := S_OK ;
      exit ;
    end;
    if wTLBufT.NumObjects = 0 then begin
      Result := S_OK ;
      exit ;
    end;

    if assigned( pdataTEX_w ) then
      pD3DDevice.SetTexture(0,pdataTEX_w) ;

    Result := addWallMeshTile( wTLBufT.NumObjects) ;
    if Result <> S_OK then begin
      safeRendering := True ;
      wTLBufT.NumPoints  := 0 ;
      wTLBufT.NumObjects := 0 ;
      SetLength(wTLBufT.TriLst, 0 ) ;

      if Result = -2147024882 then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                            'Out of memory', 5 )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                              'saveWTLBufT', 5 );
    end;

    wTLBufT.NumPoints  := 0 ;
    wTLBufT.NumObjects := 0 ;
//    SetLength(wTLBufT.TriLst, 0 ) ;  //?

    // restore current texture
    case curTEX of
      1 : pD3DDevice.SetTexture(0, pdataTEX_r) ;
      2 : pD3DDevice.SetTexture(0, pdataTEX_d) ;
    end;

    Result := S_OK;
  end;

  procedure TGIS_Renderer3DDirectX9.blitToWindow ;
  begin
    if not captureScreen then
      pD3DDevice.Present( nil, nil, 0, nil );
  end;

  function TGIS_Renderer3DDirectX9.transformSetting
    : Integer ;
  var
    hr             : HResult ;
    matrot,
    matzscale,
    mattransl,
    matrob          : TD3DXMatrix ;
    pbackbuffer     : IDirect3DSurface9 ;
    d3dsdbackbuffer : TD3DSurfaceDesc ; // surface desc of the backbuffer
    faspect,
    znear, zfar     : Single ;
    val1, val2, dep : Double ;
  begin
    MatrixRotationYawPitchRoll( matrot,
                                DegToRad(spinY),
                                DegToRad(spinX),
                                DegToRad(spinZ) ) ;

    MatrixScaling( matzscale, 1.0, 1.0, 1.0 ) ;
    if not newTransform then begin   // oldTransform
      if zEyeTo = 0 then
        MatrixTranslation(mattransl, dTurn, dFly, 0)
      else
        MatrixTranslation(mattransl, dTurn, dFly, -(zEyeTo+zOffset)) ;
      MatrixMultiply(matrob, mattransl, matrot) ;
      MatrixMultiply(mtxWorld, matzscale, matrob) ;
    end
    else begin  // newTransform
      MatrixTranslation(mattransl, -eyeX, eyeY, -(zEyeTo+zOffset)) ;
      MatrixMultiply(matrob, mattransl, matrot) ;
      if zEyeTo = 0 then
        MatrixTranslation(mattransl, eyeX, -eyeY, 0)
      else
        MatrixTranslation(mattransl, eyeX, -eyeY, zEyeTo-zOffset) ;
      MatrixMultiply(mtxWorld, matrob, mattransl) ;
      MatrixMultiply(mtxWorld, mtxWorld, matzscale) ;
    end;

    {$IFDEF CLR}
      if mtxWorld.M41 = 0 then
        mtxWorld.M41 := 0.00001 ;
    {$ELSE}
      if mtxWorld._41 = 0 then
        mtxWorld._41 := 0.00001 ;
    {$ENDIF}

    if not captureScreen then
      pD3DDevice.SetTransform(D3DTS_WORLD, mtxWorld) ;

    MatrixLookAtLH(mtxView, TD3DVector(eyeAt), TD3DVector(eyeTo),
                   D3DXVector3Init(0.0, 0.0, 1.0)) ;

    if not captureScreen then
      pD3DDevice.SetTransform(D3DTS_VIEW, mtxView) ;

    // Store the render target's surface description.
      pD3DDevice.GetBackBuffer( 0, 0, D3DBACKBUFFER_TYPE_MONO, pbackbuffer ) ;
    {$IFDEF CLR}
      d3dsdbackbuffer := pbackbuffer.Description ;
    {$ELSE}
      hr := pbackbuffer.GetDesc( d3dsdbackbuffer ) ;
    {$ENDIF}
    ReleaseInterface( pbackbuffer ) ;

    // matrix, view angle, aspect, near, far
    if windowSizeChanged then
      faspect := ( 1.0 * windowWidth ) / windowHeight
    else
      faspect := ( 1.0 * d3dsdbackbuffer.Width ) / d3dsdbackbuffer.Height ;

    if dRadius < 1500 then
      zfar := 2000.0
    else
       zfar := dRadius + 500 ;

    if dRadius < 2.51 then
      znear := 0.05
    else
      znear := 1.0 ;
    dep := 2000;
    val1 := (2.0 * znear) / (zfar + znear - dep * (zfar - znear));
    val1 := znear * zfar / (zfar - dep * (zfar - znear));
    dep := 1;
    val2 := (2.0 * znear) / (zfar + znear - dep * (zfar - znear));
    val2 := znear * zfar / (zfar - dep * (zfar - znear));
//https://stackoverflow.com/questions/50209415/sampling-the-depth-buffer-and-normalizing-to-0-1-directx
    if not captureScreen then begin
      MatrixPerspectiveFovLH( mtxProjection,
                              DegToRad(cameraViewAngle),
                              faspect,
                              znear,
                              zfar ) ;
      {$IFDEF CLR}
        try
          pD3DDevice.SetTransform(D3DTS_PROJECTION, mtxProjection) ;
          hr := S_OK ;
        except
          hr := S_FALSE ;
        end ;
      {$ELSE}
        hr := pD3DDevice.SetTransform(D3DTS_PROJECTION, mtxProjection) ;
      {$ENDIF}
    end;
    Result := Integer(hr) ;
  end;

  function TGIS_Renderer3DDirectX9.setD3D_Data
    : Integer;
  var
    hr     : HResult ;
    i      : Integer ;
    dpi    : Integer ;
    bmp    : TGIS_Bitmap ;
    imgfs  : Integer ;
    ext3d  : TGIS_Extent3D ;
    exdx   : Double ;
    exdy   : Double ;
    exdlt  : Double ;

    function adjust (const _val : TGIS_Extent ) : TGIS_Extent;
    var
      res   : TGIS_Extent ;
      mean  : Double ;
      delta : Double ;
    begin
      if GisIsContainExtent( scrExtent, normExt ) then begin
        Result := _val ;
        exit ;
      end;
      res := _val ;
      if adjustRatio <= 1.0 then begin
        mean  := (_val.YMin + _val.YMax)/2 ;
        delta := (_val.YMax - _val.YMin)/2 ;
        res.YMin := mean - delta * adjustRatio ;
        res.YMax := mean + delta * adjustRatio ;
      end
      else begin
        mean  := (_val.XMin + _val.XMax)/2 ;
        delta := (_val.XMax - _val.XMin)/2 ;
        res.XMin := mean - delta/adjustRatio ;
        res.XMax := mean + delta/adjustRatio ;
      end;
      Result := res ;
    end;

    procedure addNewGrdToArray(
      const _lp     : TGIS_LayerPixel ;
      const _pos    : Integer
    ) ;
    var
      i1, j1 : Integer ;
      val    : Double  ;
      ptg    : TGIS_Point3D ;
      ptgex  : TGIS_Point3D ;
    begin
      ptgex := GisPoint3D( 0.5*(_lp.Extent.XMin + _lp.Extent.XMax),
                           0.5*(_lp.Extent.YMin + _lp.Extent.YMax),
                           0
                          ) ;
      for i1 := 0 to demFrameSize - 1 do begin
        for j1 := 0 to demFrameSize -1 do
          if arGrid_w[i1][j1] <> GIS_GRID_NOVALUE then begin
            val := arGrid_w[i1][j1] ;
{ TODO -cReview : check condition }
//?            if not bIgnoreEllipsoidHeight then begin
            ptg := ptgex ;
            ptg.Z := val ;
            _lp.Project3D_Ref(ptg);
            val := ptg.Z ;
//            end;
            val := val * _lp.Params.ScaleZ + _lp.Params.FalseZ ;
//            if arGrid_r[i1][j1] = noDataValue then
//              arGrid_r[i1][j1] := val
//            else
//            if arGrid_r[i1][j1] < val then
              arGrid_r[i1][j1] := val ;
          end ;
      end;
    end;

    procedure setShader( _val : Boolean ) ;
    var
      ii : Integer ;
    begin
      if noDem then
        exit ;
      for ii := 0 to curDemSize - 1 do  begin
        if _val then           // restore original GridShadow
          TGIS_ParamsSectionPixel( curDem[ii].Lx.Params ).Pixel.GridShadow := curDem[ii].Shd
        else begin
          if lightSwitch then  // switch off GridShadow
            TGIS_ParamsSectionPixel( curDem[ii].Lx.Params ).Pixel.GridShadow := False ;
        end;
      end;
    end;

  begin
    if lockReadingData = 1 then begin
      Result := S_OK ;
      exit ;
    end;

    hr := S_OK ;
    freeMeshStore ;
    if curDemSize > 0 then makeWallTexture ;

    cutVectExtentV := getVectorExtent ;
    // Set current segment extent  (rough)
    setNormExtent(False) ;
    mapExtent   := retGisExtent ;
    traceExtent := mapExtent ;
    vectExtent  := mapExtent ;

    if mustAdjust then
      cutVectExtentV := adjust(cutVectExtentV) ;

    if cutVectExtentV.XMin < exExtent.XMin then
      cutVectExtentV.XMin := exExtent.XMin ;
    if cutVectExtentV.XMax > exExtent.XMax then
      cutVectExtentV.XMax := exExtent.XMax ;
    if cutVectExtentV.YMin < exExtent.YMin then
      cutVectExtentV.YMin := exExtent.YMin ;
    if cutVectExtentV.YMax > exExtent.YMax then
      cutVectExtentV.YMax := exExtent.YMax ;

    oldcutVectExtentV := cutVectExtentV ;
    roughExtent := scrExtent ;
    updateFrameSizes ;
    setShader( False ) ;

    if printBmpImgSize <> 0 then
        imgFrameSize := printBmpImgSize
    else
        imgFrameSize := Max(windowWidth,windowHeight) ;

    if imageTexture then begin
      ext3d := GetVisibleExtent3D ;
      exdx  := ext3d.XMax - ext3d.XMin ;
      exdy  := ext3d.YMax - ext3d.YMin ;
      exdlt := Max(exdx, exdy) / Max( mapExtent.XMax-mapExtent.XMin, mapExtent.YMax-mapExtent.YMin) ;

      if printBmpImgSize <> 0 then begin
        dpi := RoundS(printBmpPPI * exdlt ) ;
        dpi := RoundS( dpi * ( imgFrameSize / Max( printBmpOutSize.X, printBmpOutSize.Y))) ;
      end
      else
        dpi := RoundS(oGIS.PPI * exdlt) ;

      if dpi<10 then dpi := 10 ;

      bmp := TGIS_Bitmap.Create( imgFrameSize, imgFrameSize ) ;
      try
        generalLock := 1 ;
        oGIS.ViewerParent.ControlDrawTexture( bmp, mapExtent, dpi ) ;
        generalLock := 0 ;

        hr := loadTexture(bmp, False) ;
        if hr <> 0 then begin
          Result := hr ;
          exit ;
        end;
      finally
        FreeObject( bmp ) ;
      end;
    end;

    init_arGrid( arGrid_r ) ;
    if curDemSize > 0 then begin
      for i := 0 to curDemSize - 1 do  begin
        init_arGrid( arGrid_w ) ;
        curDem[i].Lx.GetGrid( mapExtent, arGrid_w ) ;
        addNewGrdToArray( curDem[i].Lx, i ) ;
      end;
    end;

    if firstScene then begin
      rangeExtent := GetVisibleExtent3D ;
      setRangeExtent ;
      referencePointOffset := -(rangeExtent.ZMax -rangeExtent.ZMin)/2;
    end ;

    if not preciseSwitch then begin
      setShader( True ) ;
      Result := Integer(hr) ;
      exit ;
    end;

    // Set current segment extent  (detail)
    setNormExtent(True) ;
    mapExtent    := retGisExtent ;
    detailExtent := scrExtent    ;

    if not checkExtent then begin
      setShader( True ) ;
      Result := hr ;
      exit ;
    end;

    if imageTexture then begin
      imgfs := imgFrameSize ;
      if printBmpImgSize = 0 then
        dpi := RoundS(oGIS.PPI / detailCoeff)
      else begin
        dpi := RoundS(printBmpPPI / detailCoeff ) ;
        dpi := RoundS( dpi * ( imgFrameSize / Max( printBmpOutSize.X, printBmpOutSize.Y))) ;
      end;
      bmp := TGIS_Bitmap.Create( imgfs , imgfs ) ;
      try
        generalLock := 1 ;
        oGIS.ViewerParent.ControlDrawTexture( bmp, mapExtent, dpi ) ;
        generalLock := 0 ;

        hr := loadTexture(bmp, True) ;
        if hr <> 0 then begin
          Result := Integer(hr) ;
          exit ;
        end;
      finally
        FreeObject( bmp ) ;
      end;
    end;

    setShader( True ) ;
    Result := Integer(hr) ;
  end;

  function TGIS_Renderer3DDirectX9.loadTexture(
    const _bmp    : TGIS_Bitmap ;
    const _detail : Boolean
  ) : HResult;
  begin
    if _detail then begin
      if assigned( pdataTEX_d ) then
        ReleaseInterface( pdataTEX_d ) ;
        pdataTEX_d := create_texture( pD3DDevice, _bmp ) ;
    end
    else  begin
      if assigned( pdataTEX_r ) then
        ReleaseInterface( pdataTEX_r ) ;
        pdataTEX_r := create_texture( pD3DDevice, _bmp ) ;
    end;

    pD3DDevice.SetSamplerState( 0, D3DSAMP_MINFILTER    , D3DTEXF_ANISOTROPIC) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_MAGFILTER    , D3DTEXF_LINEAR     ) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_MIPFILTER    , D3DTEXF_ANISOTROPIC) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_ADDRESSU     , D3DTADDRESS_WRAP   ) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_ADDRESSV     , D3DTADDRESS_WRAP   ) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_BORDERCOLOR  , borderColor        ) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_MAXANISOTROPY, anisotropy_factor  ) ;

    Result := S_OK ;
  end;

  procedure TGIS_Renderer3DDirectX9.freeMeshStore ;
  var
    i : Integer ;
  begin
    if meshDemTilesCounter > 0 then begin
      for i := 0 to meshDemTilesCounter -1 do begin
        FreeObject( meshDemTiles[i] ) ;
      end;
      SetLength( meshDemTiles, 0 ) ;
      meshDemTilesCounter := 0 ;
    end;

    if meshWallTilesCounter > 0 then begin
      for i := 0 to meshWallTilesCounter -1 do begin
        FreeObject( meshWallTiles[i] ) ;
      end;
      SetLength( meshWallTiles, 0 ) ;
      meshWallTilesCounter := 0 ;
    end;

    if meshTinTilesCounterC > 0 then begin
      for i := 0 to meshTinTilesCounterC -1 do begin
         FreeObject( meshTinTilesC[i] ) ;
      end;
      SetLength( meshTinTilesC, 0 ) ;
      meshTinTilesCounterC := 0 ;
    end;

    if meshTinTilesCounterT > 0 then begin
      for i := 0 to meshTinTilesCounterT -1 do begin
        FreeObject( meshTinTilesT[i] ) ;
      end;
      SetLength( meshTinTilesT, 0 ) ;
      meshTinTilesCounterT := 0 ;
    end;

    if meshMPatchTilesCounterC > 0 then begin
      for i := 0 to meshMPatchTilesCounterC -1 do begin
        FreeObject( meshMPatchTilesC[i] ) ;
      end;
      SetLength( meshMPatchTilesC, 0 ) ;
      meshMPatchTilesCounterC := 0 ;
    end;

    if meshMPatchTilesCounterT > 0 then begin
      for i := 0 to meshMPatchTilesCounterT -1 do begin
        FreeObject( meshMPatchTilesT[i] ) ;
      end;
      SetLength( meshMPatchTilesT, 0 ) ;
      meshMPatchTilesCounterT := 0 ;
    end;

    if meshVectTilesCounterCR > 0 then begin
      for i := 0 to meshVectTilesCounterCR -1 do begin
        FreeObject( meshVectTilesCR[i] ) ;
      end;
      SetLength( meshVectTilesCR, 0 ) ;
      meshVectTilesCounterCR := 0 ;
    end;

    if meshVectTilesCounterCW > 0 then begin
      for i := 0 to meshVectTilesCounterCW -1 do begin
        FreeObject( meshVectTilesCW[i] ) ;
      end;
      SetLength( meshVectTilesCW, 0 ) ;
      meshVectTilesCounterCW := 0 ;
    end;

    if meshVectTilesCounterTR > 0 then begin
      for i := 0 to meshVectTilesCounterTR -1 do begin
        FreeObject( meshVectTilesTR[i] ) ;
      end;
      SetLength( meshVectTilesTR, 0 ) ;
      meshVectTilesCounterTR := 0 ;
    end;

    if meshVectTilesCounterTW > 0 then begin
      for i := 0 to meshVectTilesCounterTW -1 do begin
        FreeObject( meshVectTilesTW[i] ) ;
      end;
      SetLength( meshVectTilesTW, 0 ) ;
      meshVectTilesCounterTW := 0 ;
    end;

    if meshLineTilesCounter > 0 then begin
      for i := 0 to meshLineTilesCounter -1 do begin
        SetLength( meshLineTiles[i].Buffer, 0 ) ;
      end;
      SetLength( meshLineTiles, 0 ) ;
      meshLineTilesCounter := 0 ;
    end;

    if meshPointTilesCounter > 0 then begin
      for i := 0 to meshPointTilesCounter -1 do begin
        SetLength( meshPointTiles[i].Buffer, 0 ) ;
      end;
      SetLength( meshPointTiles, 0 ) ;
      meshPointTilesCounter := 0 ;
    end;

    if bufSelInfoCounter > 0 then begin
      for i := 0 to bufSelInfoCounter -1 do begin
        arBufSelInfo[i].ShpInfo.Clear ;
        FreeObject( arBufSelInfo[i].ShpInfo ) ;
        arBufSelInfo[i].SelIdx.Clear ;
        FreeObject( arBufSelInfo[i].SelIdx ) ;
      end;
      SetLength( arBufSelInfo, 0 ) ;
      bufSelInfoCounter := 0 ;
    end;

    if vectorTilesInfoCounter > 0 then begin
      SetLength( arVectorTilesInfo, 0 ) ;
      vectorTilesInfoCounter := 0 ;
    end;

    SetLength(transpInfo.TCT , 0 ) ;
    SetLength(transpInfo.TTT , 0 ) ;
    SetLength(transpInfo.MPC , 0 ) ;
    SetLength(transpInfo.MPT , 0 ) ;
    SetLength(transpInfo.PCTR, 0 ) ;
    SetLength(transpInfo.PCTW, 0 ) ;
    SetLength(transpInfo.PTTR, 0 ) ;
    SetLength(transpInfo.PTTW, 0 ) ;
    SetLength(transpInfo.LT  , 0 ) ;
    SetLength(transpInfo.PT  , 0 ) ;
    SetLength(arTexTinInfo , 0 ) ;

    SetLength(curVLlist, curVLSize) ;
    curVLListSize := curVLSize ;

    curMeshTexR   := nil ;
    curMeshTexW   := nil ;
    curMeshSubset := -1  ;
    lastMeshSubsetR := -1 ;
    lastMeshSubsetW := -1 ;
    SetLength(meshTileInfoR , meshVectTilesCounterTR +1, 0) ;
    SetLength(meshTileInfoW , meshVectTilesCounterTW +1, 0) ;
    SetLength(mpatchTileInfo, meshMPatchTilesCounterT+1, 0) ;
    mpatchSubsetCounter := -1 ;

    arVectPolyRoofCounterC := 0 ;
    SetLength(arVectPolyRoofC, 1) ;
    SetLength(arVectPolyRoofC[0], initVideoVBSize );
    arVectPolyRoofCounterT := 0 ;
    SetLength(arVectPolyRoofT, 1) ;
    SetLength(arVectPolyRoofT[0], initVideoVBSize );

    releaseLabelInfo ;
    if assigned( oTextureHelper ) then
      T_textureHelperDX( oTextureHelper ).Clear ;

    demMeshExists := False ;
    shpNo := 0 ;
  end;

procedure TGIS_Renderer3DDirectX9.makeWallTexture ;
  var
    pixel, pixel1  : TRGB_Pixel      ;
    cl             : TGIS_Color      ;
    i, k , markval : Integer         ;
    lp             : TGIS_LayerPixel ;
    z1,z2,dz,iso   : Double          ;
    mark           : Boolean         ;
    mltp           : Double          ;
    ptgex, ptg     : TGIS_Point3D    ;
    pix            : TGIS_Pixels     ;
    transp         : Byte            ;

  begin
    if noDem then exit ;
    if curDemSize = 0 then exit ;
    transp := TruncS(curDem[0].Lx.Transparency/100*255) ;

    mltp := 1 ;
    z1 := zLevelMin ;
    z2 := zLevelMax ;
    lp :=  curDem[0].Lx ;
    if not assigned (lp) then exit ;
    if lp.Params.ScaleZ = 0 then exit ;

    dz := (z2 - z1 )/256 ;

    iso := 0 ;
    if isolineGap <> 0 then begin
      if zLevelMin >= 0 then begin
        repeat
          iso := iso + isolineGap ;
        until iso > zLevelMin ;
      end
      else begin
        repeat
          iso := iso - isolineGap ;
        until iso < zLevelMin ;
        iso := iso + isolineGap ;
      end;
    end;

    markval := TruncS(5*isolineGap) ;
    if markval = 0 then begin
      repeat
        mltp := mltp * 10 ;
        markval := TruncS(5*mltp*isolineGap) ;
      until markval > 10 ;
    end;
    mark := False ;

    ptgex := GisPoint3D( 0.5*(lp.ProjectedExtent.XMin +
                              lp.ProjectedExtent.XMax),
                         0.5*(lp.ProjectedExtent.YMin +
                              lp.ProjectedExtent.YMax),
                         0
                        ) ;

    arBitmap_w.LockPixels( pix, True, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Up );

    for i := 0 to 255 do begin
      ptg := ptgex ;
      ptg.Z := z1 ;
      if not bIgnoreEllipsoidHeight then
        lp.Unproject3D_Ref(ptg) ;

      // texture from legend
      cl := lp.MapGridValue(TGIS_ParamsSectionPixel( lp.Params ),
                             (ptg.Z-lp.Params.FalseZ) / lp.Params.ScaleZ) ;

      pixel.R := cl.R ;
      pixel.G := cl.G ;
      pixel.B := cl.B ;

      // texture for solid wall
      cl := solidWallColor;
      pixel1.R := cl.R ;
      pixel1.G := cl.G ;
      pixel1.B := cl.B ;

      // isolines
      cl := isolineColor;
      if isolineGap <> 0 then begin
        if (Abs(iso-z1)<=dz/2) or mark then begin
          if mark then begin
            pixel.R := cl.R ;
            pixel.G := cl.G ;
            pixel.B := cl.B ;
            pixel1 := pixel ;
            mark := not mark ;
          end
          else begin
            if (TruncS(iso*mltp) mod markval) = 0 then
              mark := True ;
            pixel.R := cl.R ;
            pixel.G := cl.G ;
            pixel.B := cl.B ;
            pixel1 := pixel ;
            iso := iso + isolineGap ;
          end;
        end;
        if i = 0 then begin

          pixel.R := ( (borderColor shr 16) and $000000FF) ;
          pixel.G := ( (borderColor shr 08) and $000000FF) ;
          pixel.B := ( (borderColor shr  0) and $000000FF) ;
          pixel1 := pixel ;
        end;
      end;

      for k := 0 to 3 do begin
        cl :=  TGIS_Color.FromARGB( transp, pixel1.R, pixel1.G, pixel1.B ) ;
        pix[(255-i)*16 + k] := Integer(cl.ARGB) ;
      end;

      for k := 4 to 15 do begin
        cl :=  TGIS_Color.FromARGB( transp, pixel.R, pixel.G, pixel.B ) ;
        pix[(255-i)*16 + k] := Integer(cl.ARGB) ;
      end;

      z1 := z1 + dz ;
      z2 := z2 - dz ;
    end ;

    arBitmap_w.UnlockPixels ;

    if assigned( pdataTEX_w ) then
      ReleaseInterface( pdataTEX_w ) ;
    pdataTEX_w := create_texture( pD3DDevice, arBitmap_w ) ;

  end;

  procedure TGIS_Renderer3DDirectX9.makeBasementTex ;
  var
    bmp   : TGIS_Bitmap  ;
    ctx   : TGIS_RendererContext  ;
    ornd  : TGIS_RendererAbstract ;
    i, a  : Integer ;
    r,g,b : Integer ;
  begin
    ornd := TGIS_RendererAbstract(
              oGIS.ViewerParent.ControlRenderer
            ).CreateInstance ;
    try
      bmp := nil ;
      ctx := nil ;

      ctx := TGIS_RendererContext.Create ;
      try
        bmp := TGIS_Bitmap.Create( 256, 256 ) ;

        {$IFDEF CLR}
        ctx.AssignBaseMap( bmp.NativeBitmap, False ) ;
        {$ELSE}
        ctx.AssignBaseMap( bmp, False ) ;
        {$ENDIF}

        ornd.CreateContext( nil, nil, ctx, Point( 0, 0 ),
                            RoundS( bmp.Width), RoundS( bmp.Height ),
                            oGIS.PPI, oGIS.FontScale
                          ) ;
        ornd.PrepareDraw ;
        a := Byte(TruncS(basePlane.Transparency/100.0*255.0)) ;
        r := basePlane.BackgroundColor.R ;
        g := basePlane.BackgroundColor.G ;
        b := basePlane.BackgroundColor.B ;

        ornd.CanvasPen.Color := TGIS_Color.FromARGB( a, r, g, b )  ;
        for i := 0 to 255 do
          ornd.CanvasDrawLine(i, 0, i, 255);

        r := basePlane.GridColor.R ;
        g := basePlane.GridColor.G ;
        b := basePlane.GridColor.B ;

        ornd.CanvasPen.Color := TGIS_Color.FromARGB( a, r, g, b )  ;
        ornd.CanvasDrawLine(0, 0, 255, 0);
        ornd.CanvasDrawLine(0, 0, 0, 255);
        ornd.AfterDraw;
        ornd.ReleaseContext ;

        if assigned( pdataTEX_bDX ) then
          ReleaseInterface( pdataTEX_bDX ) ;
        pdataTEX_bDX := create_texture( pD3DDevice, bmp ) ;
      finally
        FreeObject( bmp ) ;
        FreeObject( ctx ) ;
      end ;

    finally
      FreeObject( ornd ) ;
    end ;
  end ;

  procedure TGIS_Renderer3DDirectX9.drawBasement ;
  var
    Z, z1, rr    : Double                 ;
    dn, dnx, dnx2: Double                 ;
    linelist     : Array [0..3] of TGIS_Renderer3DVertex ;
    {$IFDEF CLR}
      vertices   : DirectXStream ;
    {$ELSE}
      vertices   : PByte                  ;
    {$ENDIF}
    plinelist_vb : IDirect3DVertexBuffer9 ;
    i, n1, n2    : Integer                ;
    l, r, t, b   : Double                 ;
    ext          : TGIS_Extent            ;

  begin
    if not basePlane.Active then exit;
    lightOff ;

    Z := zScaleFactor * (basePlane.Level - zLevelMin) / zUnit ;
    ext := normExt ;
    ext.YMin := scrExtent.YMin ;
    ext.YMax := scrExtent.YMax ;
    if scrExtent.XMin > normExt.XMin then
      ext.XMin := scrExtent.XMin / grdRatio ;
    if scrExtent.XMax < normExt.XMax then
      ext.XMax := scrExtent.XMax / grdRatio ;
    ext := normExt ;
    dn := ext.XMax - ext.XMin ;
    dnx := dn * 10 + 100 ;
    dnx2 := 2 * dnx ;
    l := dn*(dnx-ext.XMax)/dnx2 ;
    r := dn*(dnx+ext.XMax)/dnx2 ;
    t := dn*(dnx-ext.YMax)/dnx2 ;
    b := dn*(dnx+ext.YMax)/dnx2 ;

//    drawSkyBox ;
    if pD3DDevice.CreateVertexBuffer(4*sizeOf(TGIS_Renderer3DVertex),
                                     D3DUSAGE_DYNAMIC or D3DUSAGE_WRITEONLY,
                                     GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE,
                                     D3DPOOL_DEFAULT,
                                     plinelist_vb,
                                     nil) <> 0 then  exit ;
    if noDem and noImg then begin
      n1 := 0 ;  n2 := 0 ;
    end
    else begin
      z1 := zScaleFactor * (rangeExtent.ZMin - zLevelMin) / zUnit ;
      rr := initRadius / dRadius ;
      if (Abs(Z-z1) * rr < 0.05)  then begin
        n1 := 1 ;  n2 := 4 ;
      end
      else begin
        n1 := 0 ;  n2 := 0 ;
      end;
    end;

    for i := n1 to n2 do begin
      case i of
        0 : begin
              linelist[0].P := TVector3f(D3DXVector3Init(-dnx, -dnx, Z)) ;
              linelist[0].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[0].Tu := 0 ;
              linelist[0].Tv := 0 ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(D3DXVector3Init( dnx, -dnx, Z)) ;
              linelist[1].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[1].Tu := dn ;
              linelist[1].Tv := 0 ;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(D3DXVector3Init(-dnx,  dnx, Z)) ;
              linelist[2].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[2].Tu := 0 ;
              linelist[2].Tv := dn ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(D3DXVector3Init( dnx,  dnx, Z)) ;
              linelist[3].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[3].Tu := dn ;
              linelist[3].Tv := dn ;
              linelist[3].Color := $FFFFFFFF ;

            end;
        1 : begin
              linelist[0].P := TVector3f(D3DXVector3Init(-dnx, -dnx, Z)) ;
              linelist[0].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[0].Tu := 0 ;
              linelist[0].Tv := 0 ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(D3DXVector3Init( dnx, -dnx, Z)) ;
              linelist[1].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[1].Tu := dn ;
              linelist[1].Tv := 0 ;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(D3DXVector3Init(-dnx,  ext.YMin  , Z)) ;
              linelist[2].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[2].Tu := 0 ;
              linelist[2].Tv := t ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(D3DXVector3Init( dnx,  ext.YMin  , Z)) ;
              linelist[3].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[3].Tu := dn ;
              linelist[3].Tv := t ;
              linelist[3].Color := $FFFFFFFF ;
            end;
        2 : begin
              linelist[0].P := TVector3f(D3DXVector3Init(-dnx,  ext.YMax  , Z)) ;
              linelist[0].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[0].Tu := 0 ;
              linelist[0].Tv := b ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(D3DXVector3Init( dnx,  ext.YMax  , Z)) ;
              linelist[1].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[1].Tu := dn ;
              linelist[1].Tv := b ;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(D3DXVector3Init(-dnx,  dnx, Z)) ;
              linelist[2].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[2].Tu := 0 ;
              linelist[2].Tv := dn ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(D3DXVector3Init( dnx,  dnx, Z)) ;
              linelist[3].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[3].Tu := dn ;
              linelist[3].Tv := dn ;
              linelist[3].Color := $FFFFFFFF ;
            end;
        3 : begin
              linelist[0].P := TVector3f(D3DXVector3Init(-dnx, ext.YMin, Z)) ;
              linelist[0].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[0].Tu := 0 ;
              linelist[0].Tv := t ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(D3DXVector3Init( ext.XMin,  ext.YMin, Z)) ;
              linelist[1].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[1].Tu := l ;
              linelist[1].Tv := t ;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(D3DXVector3Init(-dnx,  ext.YMax, Z)) ;
              linelist[2].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[2].Tu := 0 ;
              linelist[2].Tv := b ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(D3DXVector3Init( ext.XMin,  ext.YMax, Z)) ;
              linelist[3].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[3].Tu := l ;
              linelist[3].Tv := b ;
              linelist[3].Color := $FFFFFFFF ;
            end;
        4 : begin
              linelist[0].P := TVector3f(D3DXVector3Init(ext.XMax, ext.YMin, Z)) ;
              linelist[0].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[0].Tu := r ;
              linelist[0].Tv := t ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(D3DXVector3Init( dnx,  ext.YMin  , Z)) ;
              linelist[1].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[1].Tu := dn ;
              linelist[1].Tv := t ;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(D3DXVector3Init(ext.XMax,  ext.YMax, Z)) ;
              linelist[2].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[2].Tu := r ;
              linelist[2].Tv := b ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(D3DXVector3Init( dnx,  ext.YMax, Z)) ;
              linelist[3].N := TVector3f(D3DXVector3Init(0,1,0)) ;
              linelist[3].Tu := dn ;
              linelist[3].Tv := b ;
              linelist[3].Color := $FFFFFFFF ;
            end;
      end;

      {$IFDEF CLR}
        if plinelist_vb.Lock( 0, 0, vertices, D3DLOCK_NONE ) <> 0 then
          exit ;

        vertices.WriteRange<TGIS_Renderer3DVertex>( linelist ) ;

        plinelist_vb.Unlock ;
      {$ELSE}
        if plinelist_vb.Lock(0, 0, Pointer(vertices), D3DLOCK_NONE) <> 0 then
          exit ;

        Move(linelist, vertices^, sizeOf(linelist)) ;

        if plinelist_vb.Unlock <> 0 then
          exit ;
      {$ENDIF}

      pD3DDevice.SetStreamSource( 0, plinelist_vb, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
      pD3DDevice.SetTexture(0, pdataTEX_bDX) ;

      pD3DDevice.BeginScene ;
      try
        pD3DDevice.DrawPrimitive( D3DPT_TRIANGLESTRIP, 0, 2) ;
      finally
        pD3DDevice.EndScene ;
      end;
    end;

    pD3DDevice.SetTexture(0, nil) ;

    if assigned( plinelist_vb ) then
      ReleaseInterface( plinelist_vb ) ;

    if lightSwitch then lightOn ;
  end;

  procedure TGIS_Renderer3DDirectX9.makeSkyBoxTex ;
  var
    bmp : TGIS_Bitmap ;
  begin
    // skybox
    bmp := TGIS_Bitmap.Create ;
    try
      bmp.LoadFromResourceName( 'SKYBOX_SKY' ) ;
      if assigned( pdataTEX_sDX ) then
        ReleaseInterface( pdataTEX_sDX ) ;
      if (bmp.Width > 0) and (bmp.Height > 0) then
        pdataTEX_sDX := create_texture( pD3DDevice, bmp ) ;
    finally
      FreeObject( bmp ) ;
    end;

    // sun
    bmp := TGIS_Bitmap.Create ;
    try
      bmp.LoadFromResourceName( 'SKYBOX_SUN' ) ;
      bmp.Premultiplied := True ;
      if assigned( pdataTEX_sun ) then
        ReleaseInterface( pdataTEX_sun ) ;
      if (bmp.Width > 0) and (bmp.Height > 0) then
        pdataTEX_sun := create_texture( pD3DDevice, bmp ) ;
    finally
      FreeObject( bmp ) ;
    end;
  end;

  procedure TGIS_Renderer3DDirectX9.drawSkyBox ;
  const
    num = 22;
  var
    z, x, y, r   : Double                 ;
    linelist     : Array [0..num-1] of TGIS_Renderer3DVertex ;
    {$IFDEF CLR}
      vertices   : DirectXStream ;
    {$ELSE}
      vertices   : PByte                  ;
    {$ENDIF}
    plinelist_vb : IDirect3DVertexBuffer9 ;

    function vector3Init(
    const _x : Single ;
    const _y : Single ;
    const _z : Single) : TVector3f ;
    begin
      Result.X := _x ;
      Result.Y := _y ;
      Result.Z := _z ;
    end;

    procedure generate_box ;
      function getP( const _x : Double ;
                     const _y : Double ;
                     const _z : Double ;
                     const _u : Double ;
                     const _v : Double ) : TGIS_Renderer3DVertex ;
      begin
        Result.P := vector3Init( r*_x+x, r*_y+y, r*_z + z) ;
        Result.Color := $FFFFFFFF ;
        Result.N := vector3Init( 0, 1, 0 ) ;
        Result.Tu := _u ;
        Result.Tv := _v ;
      end;

    begin
      linelist[0]  := getP( -1,  1, -1, 0.001, 0.665 ) ;
      linelist[1]  := getP( -1,  1,  1, 0.001, 0.334 ) ;

      linelist[2]  := getP( -1, -1, -1, 0.251, 0.665 ) ;
      linelist[3]  := getP( -1, -1,  1, 0.251, 0.334 ) ;

      linelist[4]  := getP(  1, -1, -1, 0.499, 0.665 ) ;
      linelist[5]  := getP(  1, -1,  1, 0.499, 0.334 ) ;

      linelist[6]  := getP(  1,  1, -1, 0.749, 0.665 ) ;
      linelist[7]  := getP(  1,  1,  1, 0.749, 0.334 ) ;

      linelist[8]  := getP( -1,  1, -1, 0.999, 0.665 ) ;
      linelist[9]  := getP( -1,  1,  1, 0.999, 0.334 ) ;

      linelist[10] := getP( -1,  1,  1, 0.251, 0.001 ) ;
      linelist[11] := getP( -1, -1,  1, 0.251, 0.334 ) ;

      linelist[12] := getP(  1,  1,  1, 0.499, 0.001 ) ;
      linelist[13] := getP(  1, -1,  1, 0.499, 0.334 ) ;

      linelist[14] := getP( -1,  1, -1, 0.251, 0.999 ) ;
      linelist[15] := getP( -1, -1, -1, 0.251, 0.665 ) ;

      linelist[16] := getP(  1,  1, -1, 0.499, 0.999 ) ;
      linelist[17] := getP(  1, -1, -1, 0.499, 0.665 ) ;

      //sun
      linelist[18]   := getP( -0.1, -1,  0.1, 0.001, 0.999 ) ;
      linelist[19]   := getP( -0.1, -1,  0.3, 0.001, 0.001 ) ;
      linelist[20]   := getP(  0.1, -1,  0.1, 0.999, 0.999 ) ;
      linelist[21]   := getP(  0.1, -1,  0.3,  0.999, 0.001 ) ;

      linelist[18].P := vector3Init( -100, 100,  0.0) ;
      linelist[19].P := vector3Init( -100,-100,  0.0) ;
      linelist[20].P := vector3Init( 100,  100,  0.0) ;
      linelist[21].P := vector3Init( 100, -100,  0.0) ;
    end;

    procedure draw_sun ;
    var
      oldworld, newworld, matrot, matrotx, matrotz,
      matrob, mattransl, matscale  : TD3DXMatrix ;
      vp        : TD3DViewport9   ;
      ci, cj, si, sj : Double   ;
      p : TVector3f ;
    begin
      if not assigned( pdataTEX_sun ) then exit ;
      pD3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1) ;
      pD3DDevice.SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA) ;
      pD3DDevice.SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA) ;
      pD3DDevice.SetTextureStageState(0, D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
      pD3DDevice.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_NONE) ;
      pD3DDevice.SetTexture(0, pdataTEX_sun) ;

      pD3DDevice.GetViewport(vp) ;
      pD3DDevice.GetTransform(D3DTS_WORLD,oldworld) ;
      MatrixRotationX(matrotx,DegToRad(-spinX)) ;
      MatrixRotationZ(matrotz,DegToRad(-spinZ)) ;
      ci  := Cos( sunPosition.X ) ;
      cj  := Cos( sunPosition.Y ) ;
      si  := Sin( sunPosition.X ) ;
      sj  := Sin( sunPosition.Y ) ;
      p := vector3Init(x+r*ci*sj , y-r*cj, z+r*si  ) ;
      MatrixTranslation(mattransl, p.X, p.Y, p.Z) ;
      MatrixScaling(matscale, 1.0, 1.0, 1.0 ) ;
      MatrixMultiply(matrot   , matrotx  , matrotz) ;
      MatrixMultiply(matrob   , matrot   , mattransl) ;
      MatrixMultiply(newworld , matscale , matrob) ;
      MatrixMultiply(newworld , newworld , oldworld) ;
      pD3DDevice.SetTransform(D3DTS_WORLD, newworld) ;

      pD3DDevice.DrawPrimitive( D3DPT_TRIANGLESTRIP, 18, 2 ) ;

      pD3DDevice.SetTransform(D3DTS_WORLD,oldworld) ;
      blendOff ;
    end;

  begin
    if not assigned( pdataTEX_sDX ) then exit ;
    if not basePlane.Active then exit;

    if reRenderCall then exit ;

    lightOff ;

    r := 1000 ;
    x :=  cameraPositionEx.X ;
    y := -cameraPositionEx.Y ;
    z :=  cameraPositionEx.Z ;

    generate_box ;

    if pD3DDevice.CreateVertexBuffer(num*sizeOf(TGIS_Renderer3DVertex),
                                     D3DUSAGE_DYNAMIC or D3DUSAGE_WRITEONLY,
                                     GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE,
                                     D3DPOOL_DEFAULT,
                                     plinelist_vb,
                                     nil) <> 0 then  exit ;

    {$IFDEF CLR}
      if plinelist_vb.Lock( 0, 0, vertices, D3DLOCK_NONE ) <> 0 then
        exit ;

      vertices.WriteRange<TGIS_Renderer3DVertex>( linelist ) ;

      plinelist_vb.Unlock ;
    {$ELSE}
      if plinelist_vb.Lock(0, 0, Pointer(vertices), D3DLOCK_NONE) <> 0 then
        exit ;

      Move(linelist, vertices^, sizeOf(linelist)) ;

      if plinelist_vb.Unlock <> 0 then
        exit ;
    {$ENDIF}

    pD3DDevice.SetStreamSource( 0, plinelist_vb, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
    pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
    pD3DDevice.SetTexture(0, pdataTEX_sDX) ;

    pD3DDevice.BeginScene ;
    try
      pD3DDevice.SetRenderState(D3DRS_ZENABLE, 0) ;
      pD3DDevice.DrawPrimitive( D3DPT_TRIANGLESTRIP,  0, 8 ) ;
      pD3DDevice.DrawPrimitive( D3DPT_TRIANGLESTRIP, 10, 2 ) ;
      pD3DDevice.DrawPrimitive( D3DPT_TRIANGLESTRIP, 14, 2 ) ;

      if lightSwitch then
        draw_sun ;
    finally
      pD3DDevice.SetRenderState(D3DRS_ZENABLE, 1) ;
      pD3DDevice.SetTexture(0, nil) ;
      pD3DDevice.EndScene ;
    end;

    if assigned( plinelist_vb ) then
      ReleaseInterface( plinelist_vb ) ;

    if lightSwitch then lightOn ;
  end;

  function  TGIS_Renderer3DDirectX9.Select3DObject(
    const _pt       : TPoint             ;
    const _prec     : Integer            ;
    var   _layer    : TGIS_LayerAbstract ;
    var   _ptg      : TGIS_Point3D       ;
    var   _shp      : TGIS_ShapeAbstract ;
    var   _part     : Integer            ;
    const _mode     : Boolean
  ) : Boolean ;
  var
    pu, pv, pw, d, d0, pd   : Single  ;
    delta, dd0              : Single  ;
    i0, j0, bt, numvert     : Integer ;
    vbt                     : Integer ;
    shpid                   : Integer ;
    vPickRayEnd             : TD3DXVector3 ;
    pi0, pj0, pbt           : Integer ;
    pd0, testval, testlen   : Single  ;
    isendpoint              : Boolean ;
    lastd0                  : Single    ;
    lasti0, lastj0, lastbt  : Integer   ;
    lastptg                 : TGIS_Point3D ;
    layer                   : TGIS_Layer ;


      procedure getRay(const _pt : TPoint ;
                       out   _pp : TD3DXVector3 ;
                       out   _pk : TD3DXVector3  ) ;
      var
        mv, matInverse : TD3DXMatrix   ;
        v              : TD3DXVector3  ;
        vp             : TD3DViewport9 ;
        pp, pk         : TD3DXVector3 ;
      begin
        pD3DDevice.GetViewport( vp ) ;
        // Compute the vector of the pick ray in screen space
        {$IFDEF CLR}
          v.X :=   ( ( ( 2.0 * _pt.X ) / vp.Width  ) - 1 ) / mtxProjection.M11 ;
          v.Y :=  -( ( ( 2.0 * _pt.Y ) / vp.Height ) - 1 ) / mtxProjection.M22 ;
          v.Z :=  1.0 ; // ZFactor ;
        {$ELSE}
          v.X :=   ( ( ( 2.0 * _pt.X ) / vp.Width  ) - 1 ) / mtxProjection._11 ;
          v.Y :=  -( ( ( 2.0 * _pt.Y ) / vp.Height ) - 1 ) / mtxProjection._22 ;
          v.Z :=  1.0 ; // ZFactor ;
        {$ENDIF}

        // Get the inverse view matrix
        MatrixInverse( mv, nil, mtxView ) ;

        // Transform the screen space pick ray into 3D space
        {$IFDEF CLR}
          pk.X := v.X*mv.M11 + v.Y*mv.M21 + v.Z*mv.M31 ;
          pk.Y := v.X*mv.M12 + v.Y*mv.M22 + v.Z*mv.M32 ;
          pk.Z := v.X*mv.M13 + v.Y*mv.M23 + v.Z*mv.M33 ;
          pp.X := mv.M41 ;
          pp.Y := mv.M42 ;
          pp.Z := mv.M43 ;
        {$ELSE}
          pk.X := v.X*mv._11 + v.Y*mv._21 + v.Z*mv._31 ;
          pk.Y := v.X*mv._12 + v.Y*mv._22 + v.Z*mv._32 ;
          pk.Z := v.X*mv._13 + v.Y*mv._23 + v.Z*mv._33 ;
          pp.X := mv._41 ;
          pp.Y := mv._42 ;
          pp.Z := mv._43 ;
        {$ENDIF}

        // Use inverse of world matrix
        MatrixInverse( matInverse, nil, mtxWorld ) ;

        // Transform ray origin and direction by inv matrix
        Vec3TransformCoord ( _pp, pp, matInverse) ;
        Vec3TransformNormal( _pk , pk, matInverse) ;
        Vec3Normalize( _pk, _pk ) ;
      end;


      function comparelayers( const _vi0, _vj0 : Integer ) : Boolean ;
      var
        i, j, jj, vi0 : Integer ;
        lay : TGIS_Layer ;
      begin
        if not assigned(_layer) then begin
          Result := True ;
          exit;
        end;

        Result := False ;
        if vbt = 0  then exit;
        vi0 := _vi0 ;
        if vi0 > bufSelInfoCounter then vi0 := 0; //? shp size exceeds buffer size
        for i := 0 to bufSelInfoCounter -1 do begin
          if (arBufSelInfo[i].BufferType = vbt) and
             (arBufSelInfo[i].BufferIndex = vi0) then begin
            jj := arBufSelInfo[i].ShpInfo.Count -1;
            if jj = 0 then begin
              lay := arBufSelInfo[i].Layer ;
              if lay.Name = TGIS_Layer(_layer).Name  then
                Result := True ;
              exit;
            end
            else for j := 0 to jj do begin
              if arBufSelInfo[i].ShpInfo[j].Offset >= _vj0 then begin
                lay := arBufSelInfo[i].Layer ;
                if lay.Name = TGIS_Layer(_layer).Name  then
                  Result := True ;
                exit;
              end;
            end;
            if _vj0 >= arBufSelInfo[i].ShpInfo[jj].Offset then begin
                lay := arBufSelInfo[i].Layer ;
                if lay.Name = TGIS_Layer(_layer).Name  then
                  Result := True ;
                exit;
            end;
          end;
        end;
      end;

      // Calculate the shortest route between line P1P2 and point P.
      // Return False if distance greater than precision.
      function linepoint3D( const _p1 : TD3DXVector3 ; const _p2 : TD3DXVector3 ;
                            const _p  : TD3DXVector3 ; var   _ds : Single
                          ) : Boolean ;
      var
        ab, ap, bp, s, t, dst : Double ;
      begin
        Result := False ;
        if delta = 0 then exit;
        if not isendpoint then exit;

        ab := Sqrt(Sqr(_p1.X-_p2.X) + Sqr(_p1.Y-_p2.Y) + Sqr(_p1.Z-_p2.Z)) ;
        ap := Sqrt(Sqr(_p1.X-_p.X ) + Sqr(_p1.Y-_p.Y ) + Sqr(_p1.Z-_p.Z )) ;
        bp := Sqrt(Sqr(_p2.X-_p.X ) + Sqr(_p2.Y-_p.Y ) + Sqr(_p2.Z-_p.Z )) ;
        s  := (ab + ap + bp) / 2 ;
        t  := Sqrt(s*(s-ab)*(s-ap)*(s-bp)) ;
        dst := 2*t/ab ;
        if dst > delta then exit ;
        _ds := dst ;
        Result := True ;
      end;

      function intersect( const _p1 : TD3DXVector3 ; const _p2 : TD3DXVector3 ;
                          const _p3 : TD3DXVector3 ; const _i, _j : Integer
                        ) : Boolean ;
      begin
        Result := False ;
        if IntersectTri( _p1, _p2, _p3, vPickRayOrig, vPickRayDir ,
                              pu,  pv, d ) then begin

           if not comparelayers( _i, _j ) then exit;

              if dd0 > d then
                dd0 := d ;
              if d <= dd0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                pw := 1 - ( pu + pv ) ;
                _ptg.X := pw * _p1.X + pu * _p2.X + pv * _p3.X ;
                _ptg.Y := pw * _p1.Y + pu * _p2.Y + pv * _p3.Y ;
                _ptg.Z := pw * _p1.Z + pu * _p2.Z + pv * _p3.Z ;
                _ptg.M := d0 ;
                Result := True ;
                exit ;
              end;
         end;
         if dd0 < testval then  // IntersectTri positive
           exit ;
         if linepoint3D(vPickRayOrig, vPickRayEnd, _p1, d) then begin

           if not comparelayers( _i, _j ) then exit;

              if d <= d0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                _ptg.X := _p1.X ;
                _ptg.Y := _p1.Y ;
                _ptg.Z := _p1.Z ;
                _ptg.M := d0 ;
                Result := True ;
              end;
         end;
         if linepoint3D(vPickRayOrig, vPickRayEnd, _p2, d) then begin

           if not comparelayers( _i, _j ) then exit;

              if d <= d0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                _ptg.X := _p2.X ;
                _ptg.Y := _p2.Y ;
                _ptg.Z := _p2.Z ;
                _ptg.M := d0 ;
                Result := True ;
              end;
         end;
         if linepoint3D(vPickRayOrig, vPickRayEnd, _p3, d) then begin

           if not comparelayers( _i, _j ) then exit;

              if d <= d0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                _ptg.X := _p3.X ;
                _ptg.Y := _p3.Y ;
                _ptg.Z := _p3.Z ;
                _ptg.M := d0 ;
                Result := True ;
              end;
         end;
      end;

      function intersect1( const _p1 : TD3DXVector3 ; const _p2 : TD3DXVector3 ;
                           const _p3 : TD3DXVector3 ; const _i, _j : Integer
                         ) : Boolean ;
      begin
        Result := False ;
        if IsNan(vPickRayDir.X) then exit ;

        if IntersectTri( _p1, _p2, _p3, vPickRayOrig, vPickRayDir ,
                              pu,  pv, d ) then begin
              if d <= d0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                pw := 1 - ( pu + pv ) ;
                _ptg.X := pw * _p1.X + pu * _p2.X + pv * _p3.X ;
                _ptg.Y := pw * _p1.Y + pu * _p2.Y + pv * _p3.Y ;
                _ptg.Z := pw * _p1.Z + pu * _p2.Z + pv * _p3.Z ;
                _ptg.M := d0 ;
                Result := True ;
                exit ;
              end;
         end;
      end;

      function checkVectorColorBuf( const _counter : Integer ;
                                    const _mesh    : T_arMesh ;
                                    var _buf     : TGIS_Renderer3DColorTriangleListBuffer
                                   ) : Boolean ;
      var
        i, j : Integer ;
      begin
        Result := False ;
        if _counter > 0 then begin
          for i := 0 to _counter -1 do begin
            numvert := TMesh(_mesh[i]).GetNumVertices ;
            j := 0 ;
            while True do begin
              if intersect(TD3DVector(TMesh(_mesh[i]).cvertices[j].P),
                           TD3DVector(TMesh(_mesh[i]).cvertices[j+1].P),
                           TD3DVector(TMesh(_mesh[i]).cvertices[j+2].P),
                           i, j) then Result := True ;
              j := j + 3 ;
              if j >= numvert then break ;
            end;
          end;
        end;
      end;

      function checkVectorTexBuf( const _counter : Integer ;
                                  const _mesh    : T_arMesh ;
                                  const _buf     : TGIS_Renderer3DTextureTriangleListBuffer
                                 ) : Boolean ;
      var
        i, j : Integer ;
      begin
        Result := False ;
        if _counter > 0 then
        for i := 0 to _counter -1 do begin
          numvert := TMesh(_mesh[i]).GetNumVertices ;
          j := 0 ;
          while True do begin
            if intersect(TD3DVector( TMesh(_mesh[i]).tvertices[j].P ),
                         TD3DVector( TMesh(_mesh[i]).tvertices[j+1].P),
                         TD3DVector( TMesh(_mesh[i]).tvertices[j+2].P),
                         i, j) then Result := True ;
            j := j + 3 ;
            if j >= numvert then break ;
          end;
        end;
      end;

      function checkDemBuf( const _counter : Integer ;
                            const _mesh    : T_arMesh
                          ) : Boolean ;
      var
        i, j : Integer ;
        p1, p2, p3 : TD3DXVector3 ;
        mesh : TMesh ;
      begin
        Result := False ;
        if _counter > 0 then
          for i := 0 to _counter -1 do begin
            numvert := TMesh(_mesh[i]).GetNumVertices ;
            mesh := TMesh(_mesh[i]) ;
            j := 0 ;
            while True do begin
               p1 := TD3DVector(mesh.tvertices[j  ].P) ;
               p2 := TD3DVector(mesh.tvertices[j+1].P) ;
               p3 := TD3DVector(mesh.tvertices[j+2].P) ;
               if intersect(p1, p2, p3, i, j) then Result := True ;
              inc( j ) ;
              if j >= numvert -3 then break ;
            end;
          end;
      end;

      function checkDemWallBuf( const _counter : Integer ;
                                const _mesh    : T_arMesh ;
                                const _buf     : TGIS_Renderer3DDemTriangleListBuffer
                              ) : Boolean ;
      var
        i, j : Integer ;
        mesh : TMesh ;
      begin
        Result := False ;
        if _counter > 0 then
        for i := 0 to _counter -1 do begin
          numvert := TMesh(_mesh[i]).GetNumVertices ;
          mesh := TMesh(_mesh[i]) ;
          j := 0 ;
          while True do begin
            if intersect(TD3DVector(mesh.tvertices[j].P),
                         TD3DVector(mesh.tvertices[j+1].P),
                         TD3DVector(mesh.tvertices[j+2].P),
                         i, j) then Result := True ;
            j := j + 3 ;
            if j >= numvert then break ;
          end;
        end;
      end;

      function checkLineBuf(const _counter : Integer ;
                            const _mesh    : TGIS_Renderer3DVectorTilesInfo
                           ) : Boolean ;
      var
        i, j, k, l : Integer        ;
        p1, p2, p3 : TD3DXVector3   ;

        function checkIntermediatePoints(     _p1 : TD3DXVector3 ;
                                              _p2 : TD3DXVector3 ;
                                          var _p3 : TD3DXVector3 ;
                                          var  _d : Single
                                        ) : Boolean ;
        var
          len : Double ;
          dx, dy, dz : Double ;
          steps, n : Integer ;
        begin
          Result := False ;
          dx := _p1.X - _p2.X ;
          dy := _p1.Y - _p2.Y ;
          dz := _p1.Z - _p2.Z ;
          len := Sqrt( Sqr(dx) + Sqr(dy) + Sqr(dz) ) ;
          if len < 2 * delta then exit;

          steps := TruncS( len / delta ) ;
          for n := 1 to steps do begin
            _p3.X := _p2.X + n * dx / steps ;
            _p3.Y := _p2.Y + n * dy / steps ;
            _p3.Z := _p2.Z + n * dz / steps ;

            if linepoint3D(vPickRayOrig, vPickRayEnd, _p3, d) then  begin
              Result := True ;
              exit;
            end;

          end;
        end;

        function distToPoint( _pt : TD3DXVector3 ) : Boolean ;
        var
          val : Double ;
        begin
          Result := False ;
          val := Sqrt( Sqr( vPickRayOrig.X - _pt.X ) +
                       Sqr( vPickRayOrig.Y - _pt.Y ) +
                       Sqr( vPickRayOrig.Z - _pt.Z )
                     ) ;
          if val < testlen then begin
            testlen := val ;
            Result := True ;
          end;
        end;

      begin
        Result := False ;
        testlen := testval ;
        if _counter > 0 then
          for i := 0 to _counter -1 do begin
            for l := 0 to bufSelInfoCounter -1 do
              if (arBufSelInfo[l].BufferType = 11) and
                 (arBufSelInfo[l].BufferIndex = i) then begin
                k := _mesh[i].Count ;
                j := 0 ;
                while True do begin
                  p1 := TD3DVector(_mesh[i].Buffer[j  ].P) ;
                  p2 := TD3DVector(_mesh[i].Buffer[j+1].P) ;
                  if linepoint3D(vPickRayOrig, vPickRayEnd, p1, d) then
                    if (d <= d0) and distToPoint( p1 ) then begin
                      d0 := d ;
                      j0 := j ;
                      i0 := i ;
                      _ptg.X := p1.X ;
                      _ptg.Y := p1.Y ;
                      _ptg.Z := p1.Z ;
                      _ptg.M := d0 ;
                      Result := True ;
                    end;
                  if linepoint3D(vPickRayOrig, vPickRayEnd, p2, d) then
                    if (d <= d0) and distToPoint( p2 ) then begin
                      d0 := d ;
                      j0 := j ;
                      i0 := i ;
                      _ptg.X := p2.X ;
                      _ptg.Y := p2.Y ;
                      _ptg.Z := p2.Z ;
                      _ptg.M := d0 ;
                      Result := True ;
                    end;
                   if checkIntermediatePoints( p1, p2, p3, d ) then
                     if (d <= d0) and distToPoint( p3 ) then begin
                       d0 := d ;
                       j0 := j ;
                       i0 := i ;
                       _ptg.X := p3.X ;
                       _ptg.Y := p3.Y ;
                       _ptg.Z := p3.Z ;
                       _ptg.M := d0 ;
                       Result := True ;
                     end;
                  inc( j, 2 ) ;
                  if j >= k then break ;
                end;
              end;
          end; // for i
      end;

      function checkPointBuf(const _counter : Integer ;
                             const _mesh    : TGIS_Renderer3DVectorTilesInfo
                            ) : Boolean ;
      var
        i, j, k, l : Integer        ;
        p1         : TD3DXVector3   ;
      begin
        Result := False ;
        if _counter > 0 then
          for i := 0 to _counter -1 do begin
            for l := 0 to bufSelInfoCounter -1 do
              if (arBufSelInfo[l].BufferType = 12) and
                 (arBufSelInfo[l].BufferIndex = i) then begin
                k := _mesh[i].Count ;
                j := 0 ;
                while True do begin
                  p1 := TD3DVector(_mesh[i].Buffer[j  ].P) ;
                  if linepoint3D(vPickRayOrig, vPickRayEnd, p1, d) then
                    if d <= d0 then begin
                      d0 := d ;
                      j0 := j ;
                      i0 := i ;
                      _ptg.X := p1.X ;
                      _ptg.Y := p1.Y ;
                      _ptg.Z := p1.Z ;
                      _ptg.M := d0 ;
                      Result := True ;
                    end;
                  inc( j ) ;
                  if j >= k then break ;
                end;
              end;
          end; // for i
      end;

      function identifyobject : Boolean ;
      var
        i, j, jj, l : Integer ;
      begin
        Result := False ;
        if bt = 0  then exit;
        if i0 > bufSelInfoCounter then i0 := 0; //? shp size exceeds buffer size
        shpid := 0 ;
        for i := 0 to bufSelInfoCounter -1 do begin
          if (arBufSelInfo[i].BufferType = bt) and
             (arBufSelInfo[i].BufferIndex = i0) then begin
            jj := arBufSelInfo[i].ShpInfo.Count -1;
            if jj = 0 then begin
              layer := arBufSelInfo[i].Layer ;
              if layer is TGIS_LayerVector then begin
                shpid  := arBufSelInfo[i].ShpInfo[jj].Uid ;
                _shp   := TGIS_LayerVector(layer).GetShape(shpid) ;
              end;
              Result := True ;
              if assigned(_shp) then _part := arBufSelInfo[i].ShpInfo[jj].Part ;
              exit;
            end
            else for j := 0 to jj do begin
              if arBufSelInfo[i].ShpInfo[j].Offset >= j0 then begin
                layer := arBufSelInfo[i].Layer ;
                if layer is TGIS_LayerVector then begin
                  if arBufSelInfo[i].ShpInfo[j].Offset > j0 then begin
                    if j = 0 then
                      l := 0
                    else
                      l := j -1 ;
                    shpid  := arBufSelInfo[i].ShpInfo[l].Uid ;
                    _shp   := TGIS_LayerVector(layer).GetShape(shpid) ;
                    if assigned(_shp) then
                      _part := arBufSelInfo[i].ShpInfo[l].Part ;
                  end
                  else begin
                    shpid  := arBufSelInfo[i].ShpInfo[j].Uid ;
                    _shp   := TGIS_LayerVector(layer).GetShape(shpid) ;
                    if assigned(_shp) then
                      _part := arBufSelInfo[i].ShpInfo[j].Part ;
                  end;
                end;
                Result := True ;
                exit;
              end;
            end;
            if j0 >= arBufSelInfo[i].ShpInfo[jj].Offset then begin
                layer := arBufSelInfo[i].Layer ;
                if layer is TGIS_LayerVector then begin
                   shpid  := arBufSelInfo[i].ShpInfo[jj].Uid ;
                  _shp   := TGIS_LayerVector(layer).GetShape(shpid) ;
                  if assigned(_shp) then
                    _part := arBufSelInfo[i].ShpInfo[jj].Part ;
                end;
                Result := True ;
                exit;
            end;
          end;
        end;
      end;

      function checkBasement : Boolean ;
      var
        p1, p2, p3 : TD3DXVector3 ;
        Z   : Single ;
        dnx : Double ;
        val : Single ;
      begin
        Result := False ;
        val := 1000 ;
        if assigned(_layer) then exit;
        if bt > 0 then exit;

        dnx := (normExt.XMax - normExt.XMin) * 10 ;
//        Z := zScaleFactor * (rangeExtent.ZMin - zLevelMin) / zUnit ;
        Z := zScaleFactor * (basePlane.Level - zLevelMin) / zUnit ;

        p1 := D3DXVector3Init(-dnx-val, -dnx-val, Z) ;
        p2 := D3DXVector3Init( dnx+val, -dnx-val, Z) ;
        p3 := D3DXVector3Init(-dnx-val,  dnx+val, Z) ;
        if intersect1(p1, p2, p3, -1, -1) then begin
          Result := True ;
          exit;
        end;

        p1 := D3DXVector3Init( dnx+val, -dnx-val, Z) ;
        p2 := D3DXVector3Init(-dnx-val,  dnx+val, Z) ;
        p3 := D3DXVector3Init( dnx+val,  dnx+val, Z) ;
        if intersect1(p1, p2, p3, -1, -1) then Result := True ;
      end;

      function getendpoint : Boolean ;
      var
        p1, p2, p3 : TD3DXVector3 ;
        Z   : Single ;
        dnx : Double ;
        val : Single ;
      begin
        Result := False ;
        val := 1000 ;
        dnx := (normExt.XMax - normExt.XMin) ;
        Z := (rangeExtent.ZMin - zLevelMin) / zUnit ;
        p1 := D3DXVector3Init(-dnx-val, -dnx-val, Z) ;
        p2 := D3DXVector3Init( dnx+val, -dnx-val, Z) ;
        p3 := D3DXVector3Init(-dnx-val,  dnx+val, Z) ;
        if intersect1(p1, p2, p3, -1, -1) then begin
          Result := True ;
          exit;
        end;
        p1 := D3DXVector3Init( dnx+val, -dnx-val, Z) ;
        p2 := D3DXVector3Init(-dnx-val,  dnx+val, Z) ;
        p3 := D3DXVector3Init( dnx+val,  dnx+val, Z) ;
        if intersect1(p1, p2, p3, -1, -1) then Result := True ;
      end;

      procedure comparelastsetting ;
      var
        len1, len2 : Extended ;
      begin
        if (lastptg.X = 0) and (lastptg.Y = 0) and (lastptg.Z = 0) then exit ;
        len1 := Sqrt(Sqr(vPickRayOrig.X-lastptg.X ) +
                     Sqr(vPickRayOrig.Y-lastptg.Y ) +
                     Sqr(vPickRayOrig.Z-lastptg.Z ) ) ;
        len2 := Sqrt(Sqr(vPickRayOrig.X-_ptg.X ) +
                     Sqr(vPickRayOrig.Y-_ptg.Y ) +
                     Sqr(vPickRayOrig.Z-_ptg.Z ) ) ;
        if len1 < len2 then begin
          i0   := lasti0 ;
          j0   := lastj0 ;
          d0   := lastd0 ;
          bt   := lastbt ;
          _ptg := lastptg ;
        end;
      end;

      procedure savelastsetting ;
      begin
        if bt = 0 then
          exit ;
        lasti0  := i0 ;
        lastj0  := j0 ;
        lastd0  := d0 ;
        lastbt  := bt ;
        lastptg := _ptg ;
      end;

      procedure savecurrentsetting ;
      begin
         pi0 := i0 ;
         pj0 := j0 ;
         pd0 := d0 ;
         pbt := bt ;
      end;

      procedure restorecurrentsetting ;
      begin
         i0 := pi0 ;
         j0 := pj0 ;
         d0 := pd0 ;
         bt := pbt ;
      end;

      procedure checklayer( const _buftype : Integer ) ;
      begin
        if not assigned(_layer) then begin
          bt := _buftype ;
          pd := d0 ;
          exit;
        end
        else begin
          bt  := _buftype ;
//          if identifyobject and (layer = _layer) then begin
          if identifyobject then
          if (layer = _layer) then begin
            pd := d0 ;
            exit
          end
          else begin
            i0 := pi0 ;
            j0 := pj0 ;
            d0 := pd0 ;
            bt := pbt ;
          end;
        end;
      end;

      function local3DToMap ( const _pnt : TGIS_Point3D
       ) : TGIS_Point3D ;
      var
        npnt : TGIS_Point3D ;
        bs   : Double       ;
        val  : Double       ;
      begin
        val := (gridsExt.XMax - gridsExt.XMin) / (2.0*baseSize) ;
        npnt.X := baseSize + _pnt.X ;
        npnt.X := gridsExt.XMin + npnt.X * val ;
        if (roughExtent.YMax - roughExtent.YMin) <> 0 then
          npnt.Y := vectExtent.YMax -
            (vectExtent.YMax - vectExtent.YMin) *
            (_pnt.Y - roughExtent.YMin)/(roughExtent.YMax - roughExtent.YMin)
        else begin
          bs := baseSize * grdRatio ;
          npnt.Y := bs + _pnt.Y ;
          npnt.Y := gridsExt.YMin + npnt.Y * (gridsExt.YMax - gridsExt.YMin) /
                                           (2.0*bs) ;
        end;
        if curDemSize > 0 then
          npnt.Z := _pnt.Z * zUnit / (zScaleFactor * curDem[0].Nz) + zLevel
        else
          npnt.Z := _pnt.Z * zUnit / zScaleFactor + zLevel ;
        npnt.M := _pnt.M * val ;
        if checkCSWGS then
          npnt.M := npnt.M * gisCSzUnit ;
        Result := npnt ;
      end;

      function getturnfly : TGIS_Point ;
      var
        vec1, vec2, vRayDirection, vRayPos, vRayPos2, vOut: TD3DXVector3;
        vs1, vs2 : TD3DXVector3;
        vp: TD3DViewport9;
        plane: TD3DXPlane;
        pnt : TGIS_Point;
      begin
        // set the mouse location
        pD3DDevice.GetViewport(vp) ;
        vec1.X := vp.Width / 2 ;     vec2.X := vec1.X ;
        vec1.Y := vp.Height/ 2 ;     vec2.Y := vec1.Y ;
        vec1.Z := 0.0 ;              vec2.Z := 1.0 ;
        Vec3Unproject(vRayPos,vec1,vp,mtxProjection,mtxView,mtxWorld) ;
        Vec3Unproject(vRayDirection,vec2,vp,mtxProjection,mtxView,mtxWorld) ;
        vRayDirection.X := vRayDirection.X - vRayPos.X ;
        vRayDirection.Y := vRayDirection.Y - vRayPos.Y ;
        vRayDirection.Z := vRayDirection.Z - vRayPos.Z ;
        vRayPos2.X := vRayPos.X + vRayDirection.X ;
        vRayPos2.Y := vRayPos.Y + vRayDirection.Y ;
        vRayPos2.Z := vRayPos.Z + vRayDirection.Z ;
        vs1.X := 0.0; vs1.Y := 0.0; vs1.Z := 0 ; //fZplane
        vs2.X := 0.0; vs2.Y := 0.0; vs2.Z := 1.0 ;
        PlaneFromPointNormal(plane,vs1,vs2) ;
        PlaneIntersectLine(vOut,plane,vRayPos,vRayPos2) ;
        pnt.X := vOut.X ;
        pnt.Y := vOut.Y ;
        Result := pnt ;
      end;

      procedure setNewCameraParams ;
      var
        rx, ry, rz, xoff, yoff, zoff, pradius : Double ;
        ptg, ptg1, ptg2, psrc  : TGIS_Point3D ;
        pt, pt1                : TGIS_Point ;
        dx, dy                 : Integer ;
      begin
        spinX := spinX + cspinX ;
        cspinX := 0 ;

        if windowWidth/2 <> TruncS( windowWidth/2 ) then
          dx := 1
        else
          dx := 0 ;
        if windowHeight/2 <> TruncS( windowHeight/2 ) then
          dy := 1
        else
          dy := 0 ;

        pt1 := screenTo3D(Point( TruncS( windowWidth  / 2 ) + dx ,
                                 TruncS( windowHeight / 2 ) + dy  )
                          ) ;
        pt  := screenTo3D(Point(TruncS(windowWidth /2), TruncS(windowHeight/2))) ;

        xoff := (pt1.X - pt.X)/2 ;
        yoff := (pt1.Y - pt.Y)/2 ;
        zoff := zScaleFactor * zLevel / zUnit ;
        pt.X := pt.X + xoff ;
        pt.Y := pt.Y + yoff ;

        if spinX <> 90 then
          pt  := getturnfly ;

        psrc := _ptg ;
        psrc.X := _ptg.X - pt.X ;
        psrc.Y := _ptg.Y - pt.Y ;
        psrc.Z := _ptg.Z ;

        eyeX :=  _ptg.X ;
        eyeY := -_ptg.Y ;

        ptg1.X := -psrc.X ;
        ptg1.Y := -psrc.Y ;
        if spinX <> 90 then
          ptg1.Z := -psrc.Z
        else
          ptg1.Z := -psrc.Z + zEyeTo ;

        ptg2 := psrc ;
        ptg2.Z := 0 ;

        rx := DegToRad(spinX) ;
        ry := DegToRad(spinY) ;
        rz := DegToRad(spinZ) ;

        ptg := getNewPntPosition(ptg1, rx, ry, rz, ptg2) ;

        ptg.X := ptg.X + pt.X ;
        ptg.Y := ptg.Y + pt.Y ;

        basicEyeAt.X := ptg.X ;
        basicEyeAt.Y := ptg.Y + 0.01 ;

        pradius := Sqrt(Sqr(vPickRayOrig.X - pt.X ) +
                        Sqr(vPickRayOrig.Y - pt.Y ) +
                        Sqr(vPickRayOrig.Z - 0    ) ) ;

        basicEyeAt.Z := Abs(pradius + ptg.Z) ;

        if (spinX = 90) or
         ((spinX <= 90) and (vPickRayOrig.Z <= 0)) or
         ((spinX > 90 ) and (vPickRayOrig.Z >= 0)) then
           basicEyeAt.Z := Min(Abs(pradius + ptg.Z), Abs(pradius - ptg.Z)) ;
        basicEyeTo.X := ptg.X ;
        basicEyeTo.Y := ptg.Y ;
        basicEyeTo.Z := -_ptg.Z ;

        dRadius := Sqrt(Sqr(basicEyeAt.X-basicEyeTo.X)+
                        Sqr(basicEyeAt.Y-basicEyeTo.Y)+
                        Sqr(basicEyeAt.Z-basicEyeTo.Z)) ;

        referencePointMode := TGIS_Viewer3DReferenceMode.Zero ;
        SetReferencePointOffset((_ptg.Z + zoff) * zUnit / zScaleFactor) ;
        setReferencePoint(referencePointMode, referencePointOffset) ;
        Repaint ;
      end;

  // main function body
  begin
    Result := False ;
//    _layer := nil ;

    if inPaint1 then
      exit ;

    inPaint1 := True;

    testval := 1e+10 ;
    d0 := testval ;
    pd := d0 ;
    if _prec > 0 then
      delta  := 2 * _prec * wndXSize / windowWidth
    else
      delta  := 10 * wndXSize / windowWidth ; // 5 pixels

    getRay( _pt, vPickRayOrig, vPickRayDir ) ;
    if getendpoint then begin
      vPickRayEnd.X := _ptg.X ;
      vPickRayEnd.Y := _ptg.Y ;
      vPickRayEnd.Z := _ptg.Z ;
      isendpoint := True ;
    end
    else
      isendpoint := False ;

    layer  := nil ;
    _shp   := nil ;
    _part  := -1  ;
    _ptg.X := 0 ;
    _ptg.Y := 0 ;
    _ptg.Z := 0 ;
    _ptg.M := 0 ;
    getRay( _pt, vPickRayOrig, vPickRayDir ) ;
    bt :=  0 ; // buffer type
    i0 := -1 ;
    j0 := -1 ;
    d0 := testval ;
    pd := d0 ;
    dd0 := testval ;
    lastptg := GisPoint3D(0, 0, 0) ;

    savecurrentsetting ;
    vbt := 1 ;
    if checkVectorColorBuf(meshVectTilesCounterCW,meshVectTilesCW,vTLBufCW) then
    if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

    savecurrentsetting ;
    vbt := 2 ;
    if checkVectorColorBuf(meshVectTilesCounterCR,meshVectTilesCR,vTLBufCR) then
    if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

    savecurrentsetting ;
    vbt := 3 ;
    if checkVectorTexBuf(meshVectTilesCounterTW,meshVectTilesTW,vTLBufTW) then
    if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

    savecurrentsetting ;
    vbt := 4 ;
    if checkVectorTexBuf(meshVectTilesCounterTR,meshVectTilesTR,vTLBufTR) then
    if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

    savecurrentsetting ;
    vbt := 7 ;
    if checkVectorColorBuf(meshTinTilesCounterC,meshTinTilesC,tinBufC) then
    if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

    savecurrentsetting ;
    vbt := 8 ;
    if checkDemWallBuf(meshTinTilesCounterT,meshTinTilesT,tinBufT) then
    if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

    savecurrentsetting ;
    vbt := 9 ;
    if checkVectorColorBuf(meshMPatchTilesCounterC,meshMPatchTilesC,mpatchBufC) then
    if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

    savecurrentsetting ;
    vbt := 10 ;
    if checkVectorTexBuf(meshMPatchTilesCounterT,meshMPatchTilesT,mpatchBufT) then
    if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

    vbt := 11 ;
    savecurrentsetting ;
    if _mode then
      savelastsetting ;
    if _mode then
    if checkLineBuf(meshLineTilesCounter, meshLineTiles) then
    if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

//  do not check point buffer
//    savecurrentsetting ;
//    if _mode then
//    if checkPointBuf(meshPointTilesCounter, meshPointTiles) then
//    if pd > d0 then checklayer( 12 ) else restorecurrentsetting ;

    if d0 = testval then begin
      savecurrentsetting ;
      vbt := 5 ;
      if checkDemBuf(meshDemTilesCounter,meshDemTiles) then
      if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;

      savecurrentsetting ;
      vbt := 6 ;
      if checkDemWallBuf(meshWallTilesCounter,meshWallTiles,wTLBufT) then
      if pd > d0 then checklayer( vbt ) else restorecurrentsetting ;
    end;

    savecurrentsetting ;
    if checkBasement then begin
      if changeRotPoint then begin
        setNewCameraParams ;
        inPaint1 := False ;
        exit ;
      end;
      mousePos3D := _ptg ;
      _ptg.M := Sqrt(Sqr(vPickRayOrig.X-_ptg.X ) +
                     Sqr(vPickRayOrig.Y-_ptg.Y ) +
                     Sqr(vPickRayOrig.Z-_ptg.Z ) ) ;
      _ptg := local3DToMap( _ptg ) ;
      _layer := nil ;
      _shp   := nil ;
      _part  := -1  ;
      Result := True ;
      inPaint1 := False ;
      exit ;
    end;

    if changeRotPoint then begin
      setNewCameraParams ;
      inPaint1 := False ;
      exit ;
    end;

    if _mode then
     comparelastsetting ;

    mousePos3D := _ptg ;
    _ptg.M := Sqrt(Sqr(vPickRayOrig.X-_ptg.X ) +
                   Sqr(vPickRayOrig.Y-_ptg.Y ) +
                   Sqr(vPickRayOrig.Z-_ptg.Z ) ) ;
    _ptg := local3DToMap( _ptg ) ;

    if not _mode then begin // screen2map3d mode
      Result := True ;
      inPaint1 := False ;
      exit ;
    end;

    if identifyobject then begin
      if not assigned( _layer ) then begin
        _layer := layer ;
        Result := True ;
      end
      else begin
        if (_layer = layer) then
          Result := True ;
      end;
    end
    else begin
      _shp := nil ;
    end;
    inPaint1 := False ;
  end;

  function  TGIS_Renderer3DDirectX9.RayIntersectDem(
    const _orig    : TGIS_Point3D ;
    const _dir     : TGIS_Point3D ;
    out   _ptg     : TGIS_Point3D
  ) : Boolean ;
  var
    rayOrig, rayDir : TD3DXVector3 ;
    numvert, i0, j0 : Integer ;
    delta, testval : Single ;
    pu, pv, pw, d, d0, pd   : Single  ;

    function getLocalOrig( const _orig : TGIS_Point3D ) : TD3DXVector3 ;
    var
      ptg : TGIS_Point3D ;
      valx, valy : Double ;
    begin
      ptg := _orig ;
      valx := ((vectExtent.XMax - ptg.X) /
        (vectExtent.XMax - vectExtent.XMin)) ;
      valy := ((vectExtent.YMax - ptg.Y) /
        (vectExtent.YMax - vectExtent.YMin)) ;
      Result.X := roughExtent.XMax - valx *
        (roughExtent.XMax - roughExtent.XMin) ;
      Result.Y := roughExtent.YMin + valy *
        (roughExtent.YMax - roughExtent.YMin) ;
      Result.Z := projFactor * (ptg.Z - zLevel) * zFactor / zUnit ; // above 0
    end;

    function getLocalDir( const _dir : TGIS_Point3D ) : TD3DXVector3 ;
    var
      len : Double ;
    begin
      len := Sqrt( (_dir.X * _dir.X) + (_dir.Y * _dir.Y) + (_dir.Z * _dir.Z) ) ;
      Result.X :=  _dir.X / len ;
      Result.Y := -_dir.Y / len ;
      Result.Z :=  _dir.Z / len ;
    end;

    function local3DToMap ( const _pnt : TGIS_Point3D
       ) : TGIS_Point3D ;
      var
        npnt : TGIS_Point3D ;
        bs   : Double       ;
        val  : Double       ;
      begin
        val := (gridsExt.XMax - gridsExt.XMin) / (2.0*baseSize) ;
        npnt.X := baseSize + _pnt.X ;
        npnt.X := gridsExt.XMin + npnt.X * val ;
        if (roughExtent.YMax - roughExtent.YMin) <> 0 then
          npnt.Y := vectExtent.YMax -
            (vectExtent.YMax - vectExtent.YMin) *
            (_pnt.Y - roughExtent.YMin)/(roughExtent.YMax - roughExtent.YMin)
        else begin
          bs := baseSize * grdRatio ;
          npnt.Y := bs + _pnt.Y ;
          npnt.Y := gridsExt.YMin + npnt.Y * (gridsExt.YMax - gridsExt.YMin) /
                                           (2.0*bs) ;
        end;
        if curDemSize > 0 then
          npnt.Z := _pnt.Z * zUnit / (zScaleFactor * curDem[0].Nz) + zLevel
        else
          npnt.Z := _pnt.Z * zUnit / zScaleFactor + zLevel ;
        npnt.M := _pnt.M * val ;
        if checkCSWGS then
          npnt.M := npnt.M * gisCSzUnit ;
        Result := npnt ;
      end;

    function intersect( const _p1 : TD3DXVector3 ; const _p2 : TD3DXVector3 ;
                        const _p3 : TD3DXVector3 ; const _i, _j : Integer
                      ) : Boolean ;
    begin
      Result := False ;

      if IntersectTri( _p1, _p2, _p3, rayOrig, rayDir, pu,  pv, d ) then begin
        if d <= d0 then begin
          d0 := d ;
          j0 := _j ;
          i0 := _i ;
          pw := 1 - ( pu + pv ) ;
          _ptg.X := pw * _p1.X + pu * _p2.X + pv * _p3.X ;
          _ptg.Y := pw * _p1.Y + pu * _p2.Y + pv * _p3.Y ;
          _ptg.Z := pw * _p1.Z + pu * _p2.Z + pv * _p3.Z ;
          _ptg.M := d0 ;
          Result := True ;
          exit ;
        end;
      end;
    end;

    function checkDemBuf( const _counter : Integer ;
                          const _mesh    : T_arMesh
                        ) : Boolean ;
    var
      i, j : Integer ;
      p1, p2, p3 : TD3DXVector3 ;
      mesh : TMesh ;
    begin
      Result := False ;
      if _counter > 0 then
        for i := 0 to _counter -1 do begin
          numvert := TMesh(_mesh[i]).GetNumVertices ;
          mesh := TMesh(_mesh[i]) ;
          j := 0 ;
          while True do begin
             p1 := TD3DVector(mesh.tvertices[j  ].P) ;
             p2 := TD3DVector(mesh.tvertices[j+1].P) ;
             p3 := TD3DVector(mesh.tvertices[j+2].P) ;
             if intersect(p1, p2, p3, i, j) then Result := True ;
            inc( j ) ;
            if j >= numvert -3 then break ;
          end;
        end;
    end;

  begin
    Result := False ;
    _ptg := GisPoint3D( 0, 0, 0) ;
    delta  := 10 * wndXSize / windowWidth ; // 5 pixels
    if inPaint1 then
      exit ;

    testval := 1e+10 ;
    d0  := testval ;
    pd  := testval ;
    inPaint1 := True ;
    try
      rayOrig := getLocalOrig( _orig ) ;
      rayDir  := getLocalDir( _dir ) ;

      if checkDemBuf( meshDemTilesCounter, meshDemTiles ) then begin
        _ptg.M := 0 ;
        _ptg := local3DToMap( _ptg ) ;
        Result := True ;
      end;
    finally
      inPaint1 := False ;
    end;
  end;

  procedure TGIS_Renderer3DDirectX9.drawMeshTiles ;
  var
    i, mtc : Integer ;
  begin
    if meshDemTilesCounter = 0 then exit ;
    // Draw DEM mesh (rough)
    if meshDemTilesCounter > 0 then begin
      if imageTexture  then
        pD3DDevice.SetTexture(0, pdataTEX_r)
      else
        pD3DDevice.SetTexture(0, pdataTEX_w) ;
      mtc := meshDemDetailIndex -1 ;
      for i := 0 to mtc do begin
        TMesh(meshDemTiles[i]).UpdateBuffer ;
        TMesh(meshDemTiles[i]).DrawSubset(0) ;
      end;
    end;

    // Draw DEM mesh (detail)
    if meshDemDetailIndex < meshDemTilesCounter then begin
      if imageTexture  then
        pD3DDevice.SetTexture(0, pdataTEX_d)
      else
        pD3DDevice.SetTexture(0, pdataTEX_w) ;
      mtc := meshDemTilesCounter -1 ;
      for i := meshDemDetailIndex to mtc do begin
        TMesh(meshDemTiles[i]).UpdateBuffer ;
        TMesh(meshDemTiles[i]).DrawSubset(0) ;
      end;
    end;

    // Draw DEM walls
    if meshWallTilesCounter > 0 then begin
      if assigned( pdataTEX_w ) then pD3DDevice.SetTexture(0, pdataTEX_w) ;
      mtc := meshWallTilesCounter -1 ;
      for i := 0 to mtc do begin
        TMesh(meshWallTiles[i]).UpdateBuffer ;
        TMesh(meshWallTiles[i]).DrawSubset(0) ;
      end;

      // restore current texture
      case curTEX of
        1 : pD3DDevice.SetTexture(0, pdataTEX_r) ;
        2 : pD3DDevice.SetTexture(0, pdataTEX_d) ;
      end;
    end;
  end;

  function TGIS_Renderer3DDirectX9.tvValue(
    const _row : Integer
  ) : Single ;
  var
    i : Integer ;
  begin
    i := _row ;
    if i < 0 then
      i := 0 ;
    if i > demFrameSize -1 then
      i := demFrameSize -1 ;
    Result := 1.0 * i/( demFrameSize -1 )
  end;

  procedure TGIS_Renderer3DDirectX9.detailTexture(
    var   _pardata : T_arTextureData ;
    const _k       : Integer         ;
    const _kk      : Integer         ;
    const _jj      : Integer
  ) ;
  var
    deltaextx, deltaexty : Double;
  begin
    deltaextx := scrExtent.XMax-scrExtent.XMin ;
    deltaexty := scrExtent.YMax-scrExtent.YMin ;
    _pardata[_k].Tu := (_pardata[_k].P.X - scrExtent.XMin)/ deltaextx ;
    _pardata[_k].Tv := (_pardata[_k].P.Y - scrExtent.YMin)/ deltaexty
  end;

  function  TGIS_Renderer3DDirectX9.addDemMeshTile(
    var   _pardata : T_arTextureData   ;
    const _first   : Integer           ;
    const _tri     : Integer
  ) : Integer ;
  var
    i, mtc  : Integer ;
    numvert : DWORD ;
    numtri  : DWORD ;
    j       : Integer ;
    mesh : TMesh ;
  begin
    inc(meshDemTilesCounter) ;
    SetLength(meshDemTiles, meshDemTilesCounter) ;
    mtc := meshDemTilesCounter -1 ;

    Result := CreateMeshFVF( _tri,
                             _tri+2,
                             D3DXMESH_SYSTEMMEM,  // mo? inny MANAGED
                             GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE,
                             pD3DDevice,
                             mesh,
                             pdataVB, cdataVB, idataIB
                           ) ;
    meshDemTiles[mtc] := mesh ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // Vertices
    numvert := TMesh(meshDemTiles[mtc]).GetNumVertices ;
    {$IFDEF CLR}
      Array.Copy( _pardata, _first, mesh.tvertices, 0, numvert ) ;
    {$ELSE}
      System.Move( _pardata[_first], mesh.tvertices[0], numvert * SizeOf(TGIS_Renderer3DVertex)) ;
    {$ENDIF}

    // Indices
    numtri :=  TMesh(meshDemTiles[mtc]).GetNumFaces ;
    j := 0 ;
    for i := 0 to numtri - 1 do begin
      if i mod 2 = 0 then begin
        mesh.indices[j  ] := DWORD( i ) ;
        mesh.indices[j+1] := DWORD( i + 2 ) ;
        mesh.indices[j+2] := DWORD( i + 1 ) ;
      end
      else begin
        mesh.indices[j  ] := DWORD( i ) ;
        mesh.indices[j+1] := DWORD( i + 1 ) ;
        mesh.indices[j+2] := DWORD( i + 2 ) ;
      end ;
      j := j + 3 ;
    end ;

//    Result := TMesh(meshDemTiles[mtc]).DrawSubset(0) ;
    fillBufferInfoEntry(5, 0, 0, extractColorFromShape(5)) ;
  end;

  function  TGIS_Renderer3DDirectX9.addWallMeshTile(
    const _tri     : Integer
  ) : Integer ;
  var
    i, msc  : Integer ;
    numvert : DWORD ;
    j       : Integer ;
    mesh : TMesh ;
  begin
    inc(meshWallTilesCounter) ;
    SetLength(meshWallTiles, meshWallTilesCounter) ;
    msc := meshWallTilesCounter -1 ;

    Result := CreateMeshFVF( _tri ,
                             _tri *3,
                             D3DXMESH_SYSTEMMEM,  // mo? inny MANAGED
                             GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE,
                             pD3DDevice,
                             mesh,
                             pdataVB, cdataVB, idataIB
                           ) ;
    meshWallTiles[msc] := mesh ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // Vertices
    numvert := TMesh(meshWallTiles[msc]).GetNumVertices ;
    {$IFDEF CLR}
      Array.Copy( wTLBufT.TriLst, mesh.tvertices, numvert ) ;
    {$ELSE}
      System.Move( wTLBufT.TriLst[0], mesh.tvertices[0], numvert * SizeOf(TGIS_Renderer3DVertex)) ;
    {$ENDIF}

    // Indices
    j := 0 ;
    for i := 0 to _tri - 1 do begin
      mesh.indices[j  ] := j     ;
      mesh.indices[j+1] := j + 2 ;
      mesh.indices[j+2] := j + 1 ;
      j := j + 3 ;
    end ;

//    Result := TMesh(meshWallTiles[msc]).DrawSubset(0) ;
    fillBufferInfoEntry(6, 0, 0, extractColorFromShape(6)) ;
  end;

  function TGIS_Renderer3DDirectX9.getDemBufSize(
    const _row : Integer
  ) : Integer ;
  begin
    Result := TMesh(meshDemTiles[_row]).GetNumVertices ;
  end;

  function TGIS_Renderer3DDirectX9.getWallBufSize(
    const _row : Integer
  ) : Integer ;
  begin
    Result := TMesh(meshWallTiles[_row]).GetNumVertices ;
  end;

  procedure TGIS_Renderer3DDirectX9.lightOff ;
  begin
    pD3DDevice.LightEnable(0, FALSE) ;
    pD3DDevice.SetRenderState(D3DRS_LIGHTING, 0) ;
    pD3DDevice.SetRenderState(D3DRS_SPECULARENABLE, 0) ;
  end;

  procedure TGIS_Renderer3DDirectX9.lightOn ;
  begin
    pD3DDevice.LightEnable(0, TRUE) ;
    pD3DDevice.SetRenderState(D3DRS_LIGHTING, 1) ;
    pD3DDevice.SetRenderState(D3DRS_COLORVERTEX, 1) ;
    pD3DDevice.SetRenderState(D3DRS_AMBIENTMATERIALSOURCE, D3DMCS_COLOR1) ;
    pD3DDevice.SetRenderState(D3DRS_DIFFUSEMATERIALSOURCE, D3DMCS_COLOR1) ;
    pD3DDevice.SetRenderState(D3DRS_SPECULARMATERIALSOURCE, D3DMCS_COLOR2) ;
    pD3DDevice.SetRenderState(D3DRS_SPECULARENABLE, 1) ;
    pD3DDevice.SetRenderState(D3DRS_SHADEMODE, D3DSHADE_GOURAUD) ;
  end;

  procedure TGIS_Renderer3DDirectX9.reset3DEnvironment ;
  var
    {$IFDEF CLR}
      wsize        : TPoint ;
    {$ELSE}
      pwi        : TWindowInfo ;
    {$ENDIF}
  begin
    {$IFDEF CLR}
      wsize := GetWindowControlSize( hWindow ) ;
      if ( wsize.X  <> windowWidth  ) or
         ( wsize.Y <> windowHeight ) then begin
        windowWidth  := wsize.X  ;
        windowHeight := wsize.Y ;
      end;
    {$ELSE}
      GetWindowInfo( hWindow, pwi ) ;
      if ((pwi.rcClient.Right  - pwi.rcClient.Left) <> windowWidth ) or
         ((pwi.rcClient.Bottom - pwi.rcClient.Top ) <> windowHeight)then begin
        windowWidth  := pwi.rcClient.Right  - pwi.rcClient.Left ;
        windowHeight := pwi.rcClient.Bottom - pwi.rcClient.Top  ;
      end;
    {$ENDIF}

    oldwindowWidth  := windowWidth  ;
    oldwindowHeight := windowHeight ;
    windowSizeChanged := False ;

    if ( windowWidth  < 10 ) or ( windowHeight < 10 ) then exit ;

    scrRatio := ( 1.0 * windowHeight ) / windowWidth ;

    if restore3DEnvironment <> 0 then begin
      cleanUp ;
      Lock ;
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                   'restore3DEnvironment', 1
                                 ) ;
    end ;

    if enDemCachedSize = TGIS_Viewer3DDemCacheSize.Window then begin
      if (noImg <> True) or (noDem <> True) then begin
        DemCachedSize := enDemCachedSize  ;
      end ;
    end ;

    if lightSwitch then lightOn ;
  end ;

  function TGIS_Renderer3DDirectX9.restore3DEnvironment
  : Integer ;
  {$IFNDEF CLR}
    var
      d3ddm : TD3DDisplayMode       ;
      d3dpp : TD3DPresentParameters ;
      hr    : Integer               ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      reInit3D ;
      Result := S_OK ;
    {$ELSE}

    if oldhWindow <> hWindow then begin
      reInit3D ;
      Result := S_OK ;
      exit ;
    end ;

    assert( IsWindow( hWindow ) ) ;
    // Release Vertex Buffers
    if assigned( pdataVB ) then
      ReleaseInterface( pdataVB ) ;
    if assigned( cdataVB ) then
      ReleaseInterface( cdataVB ) ;
    if assigned( idataIB ) then
      ReleaseInterface( idataIB ) ;

    freeMeshStore ;

     // Get the current desktop display mode
    if pD3D.GetAdapterDisplayMode(D3DADAPTER_DEFAULT, d3ddm) <> 0 then begin
      Result:= E_FAIL ;
      exit ;
    end;

    // Get the current PresentParameters
    ZeroMemory(@d3dpp, sizeOf(d3dpp)) ;
    d3dpp.Windowed               := True                        ;
    d3dpp.SwapEffect             := D3DSWAPEFFECT_DISCARD       ;
    d3dpp.BackBufferFormat       := d3ddm.Format                ;
    d3dpp.BackBufferCount        := 1                           ;
    d3dpp.BackBufferWidth        := 0                           ;
    d3dpp.BackBufferHeight       := 0                           ;
    d3dpp.EnableAutoDepthStencil := True                        ;
    d3dpp.AutoDepthStencilFormat := D3DFMT_D24S8                ;
    d3dpp.PresentationInterval   := D3DPRESENT_INTERVAL_DEFAULT ;

    if pD3D.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT,
                                        D3DDEVTYPE_HAL,
                                        D3DFMT_X8R8G8B8,
                                        d3dpp.Windowed,
                                        D3DMULTISAMPLE_4_SAMPLES,
                                        nil
                                      ) = 0 then
      d3dpp.MultiSampleType := D3DMULTISAMPLE_4_SAMPLES
    else
    if pD3D.CheckDeviceMultiSampleType( D3DADAPTER_DEFAULT,
                                        D3DDEVTYPE_HAL,
                                        D3DFMT_X8R8G8B8,
                                        d3dpp.Windowed,
                                        D3DMULTISAMPLE_2_SAMPLES,
                                        nil
                                      ) = 0 then
      d3dpp.MultiSampleType := D3DMULTISAMPLE_2_SAMPLES
    else
      d3dpp.MultiSampleType := D3DMULTISAMPLE_NONE ;

    hr := Integer(pD3DDevice.Reset(d3dpp)) ;
      if hr <> S_OK then begin
        Result := Integer(hr) ;
        exit ;
      end;

    // ReCreate Geometry
    if setBuffers <> 0 then begin
      Result:= E_FAIL ;
      exit ;
    end;

    if lightSwitch then
      lightOn
    else
      lightOff ;

    pD3DDevice.SetSamplerState( 0, D3DSAMP_MINFILTER    , D3DTEXF_ANISOTROPIC) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_MAGFILTER    , D3DTEXF_LINEAR     ) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_MIPFILTER    , D3DTEXF_ANISOTROPIC) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_ADDRESSU     , D3DTADDRESS_WRAP   ) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_ADDRESSV     , D3DTADDRESS_WRAP   ) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_BORDERCOLOR  , borderColor        ) ;
    pD3DDevice.SetSamplerState( 0, D3DSAMP_MAXANISOTROPY, anisotropy_factor  ) ;

    Result := S_OK ;
    {$ENDIF}
  end;

  function TGIS_Renderer3DDirectX9.triangleNormal(
    const _v0 : TVector3f ;
    const _v1 : TVector3f ;
    const _v2 : TVector3f
  ) : TVector3f ;
  var
    v0, v1, v2     : TD3DXVector3 ;
    vout, vv1, vv2 : TD3DXVector3 ;
  begin
    v0 := TD3DXVector3( _v0 ) ;
    v1 := TD3DXVector3( _v1 ) ;
    v2 := TD3DXVector3( _v2 ) ;
    Vec3Subtract( vv1, v1, v0 ) ;
    Vec3Subtract( vv2, v2, v0 ) ;
    Vec3Cross( vout, vv1, vv2 ) ;
    Vec3Normalize( vout, vout ) ;
    Result := TVector3f( vout ) ;
 end;

  procedure TGIS_Renderer3DDirectX9.adjustTransformSetting(
    _value : Boolean
  ) ;
  var
    ptt  : TGIS_Point3D ;
    zoff : Double       ;

    function getturnfly : TGIS_Point3D ;
    var
      vec1, vec2, vRayDirection, vRayPos, vRayPos2, vOut: TD3DXVector3 ;
      vs1, vs2 : TD3DXVector3 ;
      vp: TD3DViewport9 ;
      plane: TD3DXPlane  ;
      pnt : TGIS_Point3D ;
    begin
      // set the mouse location
      pD3DDevice.GetViewport(vp) ;
      vec1.X := vp.Width / 2 ;     vec2.X := vec1.X ;
      vec1.Y := vp.Height/ 2 ;     vec2.Y := vec1.Y ;
      vec1.Z := 0.0 ;              vec2.Z := 10.0 ;
      Vec3Unproject(vRayPos,vec1,vp,mtxProjection,mtxView,mtxWorld) ;
      vPickRayOrig := vRayPos ;
      Vec3Unproject(vRayDirection,vec2,vp,mtxProjection,mtxView,mtxWorld) ;
      vRayDirection.X := vRayDirection.X - vRayPos.X ;
      vRayDirection.Y := vRayDirection.Y - vRayPos.Y ;
      vRayDirection.Z := vRayDirection.Z - vRayPos.Z ;
      vRayPos2.X := vRayPos.X + vRayDirection.X ;
      vRayPos2.Y := vRayPos.Y + vRayDirection.Y ;
      vRayPos2.Z := vRayPos.Z + vRayDirection.Z ;
      vs1.X := 0.0; vs1.Y := 0.0; vs1.Z := zEyeTo ;
      vs2.X := 0.0; vs2.Y := 0.0; vs2.Z := 1.0 ;
      PlaneFromPointNormal(plane,vs1,vs2) ;
      PlaneIntersectLine(vOut,plane,vRayPos,vRayPos2) ;
      pnt.X := vOut.X ;
      pnt.Y := vOut.Y ;
      pnt.Z := vOut.Z ;
      Result := pnt ;
    end;

    function getendpoint : TGIS_Point3D ;
    var
      vec1, vec2, vRayDirection, vRayPos, vRayPos2, vOut: TD3DXVector3;
      vs1, vs2 : TD3DXVector3;
      vp: TD3DViewport9;
      plane: TD3DXPlane;
      pnt : TGIS_Point3D;
    begin
      // set the mouse location
      pD3DDevice.GetViewport(vp) ;
      vec1.X := vp.Width / 2 ;     vec2.X := vec1.X ;
      vec1.Y := vp.Height/ 2 ;     vec2.Y := vec1.Y ;
      vec1.Z := 0.0 ;              vec2.Z := 1.0 ;
      Vec3Unproject(vRayPos,vec1,vp,mtxProjection,mtxView,mtxWorld) ;
      vPickRayOrig := vRayPos ;
      Vec3Unproject(vRayDirection,vec2,vp,mtxProjection,mtxView,mtxWorld) ;
      vRayDirection.X := vRayDirection.X - vRayPos.X ;
      vRayDirection.Y := vRayDirection.Y - vRayPos.Y ;
      vRayDirection.Z := vRayDirection.Z - vRayPos.Z ;
      vRayPos2.X := vRayPos.X + vRayDirection.X ;
      vRayPos2.Y := vRayPos.Y + vRayDirection.Y ;
      vRayPos2.Z := vRayPos.Z + vRayDirection.Z ;

      vs1.X := 0.0; vs1.Y := 0.0; vs1.Z := 0.0 ;
      if ((spinZ > 225) and (spinZ < 315)) or
         ((spinZ >  45) and (spinZ < 135)) then begin
        vs2.X := 1.0; vs2.Y := 0.0; vs2.Z := 0.0
      end
      else begin
        vs2.X := 0.0; vs2.Y := 1.0; vs2.Z := 0.0
      end;
      vs2.X := Sin(DegToRad(spinZ)); vs2.Y := Cos(DegToRad(spinZ)); vs2.Z := 0.0 ;

      PlaneFromPointNormal(plane,vs1,vs2) ;
      PlaneIntersectLine(vOut,plane,vRayPos,vRayPos2) ;
      pnt.X := vOut.X ;
      pnt.Y := vOut.Y ;
      pnt.Z := vOut.Z ;
      Result := pnt ;
    end;

    procedure exception_case ;
    begin
        zoff := zScaleFactor * zLevel / zUnit ;
        ptt := getendpoint ;
        dTurn := -ptt.X ;
        dFly  := -ptt.Y ;
        dRadius := Sqrt(Sqr(vPickRayOrig.X + dTurn ) +
                        Sqr(vPickRayOrig.Y + dFly  ) +
                        Sqr(vPickRayOrig.Z - ptt.Z ) ) + 1 ;

        referencePointMode := TGIS_Viewer3DReferenceMode.Zero ;
        SetReferencePointOffset((ptt.Z + zoff) * zUnit / zScaleFactor ) ;
        setReferencePoint(referencePointMode, referencePointOffset) ;

        basicEyeAt := vector3InitDX9( 0, 0.01, dRadius ) ;
        basicEyeTo := vector3InitDX9( 0, 0, 0 ) ;

//?        Draw ;
        Repaint ;
    end;

  begin
    newTransform := _value ;
    if not _value then begin // GISMouseUp
      if (spinX = 90) or
         ((spinX <= 90) and (vPickRayOrig.Z <= 0)) or
         ((spinX > 90 ) and (vPickRayOrig.Z >= 0)) then begin
        exception_case ;
        exit;
      end;

      ptt  := getturnfly ;
      if vPickRayOrig.Z < ptt.Z  then begin
        exception_case ;
        exit;
      end;

      dTurn := -ptt.X ;
      dFly  := -ptt.Y ;
      dRadius := Sqrt(Sqr(vPickRayOrig.X + dTurn  ) +
                      Sqr(vPickRayOrig.Y + dFly   ) +
                      Sqr(vPickRayOrig.Z - ptt.Z ) ) + 1 ;

      basicEyeAt := vector3InitDX9( 0, 0.01, dRadius ) ;
      basicEyeTo := vector3InitDX9( 0, 0, 0 ) ;
      if (dFly > normExt.YMin) and (dFly < normExt.YMax) then begin
        updateCameraParams ;
        SetCameraPosition( GetCameraPosition ) ;
      end;
//?      Draw ;
      Repaint ;
    end;

  end;

  function TGIS_Renderer3DDirectX9.screenTo3D(
    const _pt : TPoint
  ) : TGIS_Point ;
  var
    vec1, vec2, vRayDirection, vRayPos, vRayPos2, vOut: TD3DXVector3;
    vs1, vs2 : TD3DXVector3;
    vp: TD3DViewport9;
    fZplane: Single;
    plane: TD3DXPlane;
    pnt : TGIS_Point;
  begin
    fZplane := 0.0;
    // set the mouse location
    vec1.X := _pt.X;     vec2.X := vec1.X;
    vec1.Y := _pt.Y;     vec2.Y := vec1.Y;
    vec1.Z := 0.0;       vec2.Z := 1.0;
    pD3DDevice.GetViewport(vp);
    Vec3Unproject(vRayPos,vec1,vp,mtxProjection,mtxView,mtxWorld);
    Vec3Unproject(vRayDirection,vec2,vp,mtxProjection,mtxView,mtxWorld);
    vRayDirection.X := vRayDirection.X - vRayPos.X;
    vRayDirection.Y := vRayDirection.Y - vRayPos.Y;
    vRayDirection.Z := vRayDirection.Z - vRayPos.Z;
    vRayPos2.X := vRayPos.X + vRayDirection.X;
    vRayPos2.Y := vRayPos.Y + vRayDirection.Y;
    vRayPos2.Z := vRayPos.Z + vRayDirection.Z;
    vs1.X := 0.0; vs1.Y := 0.0; vs1.Z := fZplane;
    vs2.X := 0.0; vs2.Y := 0.0; vs2.Z := 1.0;
    PlaneFromPointNormal(plane,vs1,vs2);
    PlaneIntersectLine(vOut,plane,vRayPos,vRayPos2);
    pnt.X := vOut.X;
    pnt.Y := vOut.Y;
    Result := pnt;
  end;

  function TGIS_Renderer3DDirectX9.ScreenToMap(
    const _pt : TPoint
  ) : TGIS_Point ;
  var
    vec1, vec2, vraydirection,
    vraypos, vraypos2, vout    : TD3DXVector3 ;
    vs1, vs2                   : TD3DXVector3 ;
    vp                         : TD3DViewport9 ;
    fZplane                    : Single       ;
    m_flXMin, m_flYMin,
    m_flXMax, m_flYMax         : Double       ;
    x_ratio, y_ratio           : Double       ;
    plane                      : TD3DXPlane   ;
    pnt                        : TGIS_Point   ;
  begin
    m_flXMin := -baseSize ;
    m_flXMax :=  baseSize ;
    m_flYMin := -baseSize * grdRatio ;
    m_flYMax :=  baseSize * grdRatio ;
    fZplane  :=  0.0 ;
    x_ratio  :=  demFrameSize/(2.0*baseSize) ;
    y_ratio  :=  demFrameSize/(2.0*m_flYMax) ;
    // set the mouse location
    vec1.X := _pt.X ;     vec2.X := vec1.X ;
    vec1.Y := _pt.Y ;     vec2.Y := vec1.Y ;
    vec1.Z :=  0.0  ;     vec2.Z :=    1.0 ;
    pD3DDevice.GetViewport(vp) ;
    Vec3Unproject(vraypos,vec1,vp,mtxProjection,mtxView,mtxWorld) ;
    Vec3Unproject(vraydirection,vec2,vp,mtxProjection,mtxView,mtxWorld) ;
    vraydirection.X := vraydirection.X - vraypos.X ;
    vraydirection.Y := vraydirection.Y - vraypos.Y ;
    vraydirection.Z := vraydirection.Z - vraypos.Z ;
    vraypos2.X := vraypos.X + vraydirection.X ;
    vraypos2.Y := vraypos.Y + vraydirection.Y ;
    vraypos2.Z := vraypos.Z + vraydirection.Z ;
    vs1.X := 0.0 ; vs1.Y := 0.0 ; vs1.Z := fZplane ;
    vs2.X := 0.0 ; vs2.Y := 0.0 ; vs2.Z := 1.0 ;
    PlaneFromPointNormal(plane,vs1,vs2) ;
    PlaneIntersectLine(vout,plane,vraypos,vraypos2) ;

    pnt.X :=((vout.X-m_flXMin)*demFrameSize/(m_flXMax-m_flXMin))-
             (dTurn*x_ratio) ;
    pnt.Y :=((vout.Y-m_flYMin)*demFrameSize/(m_flYMax-m_flYMin))-
             (dFly*y_ratio) ;

    pnt.X := gridsExt.XMin+pnt.X*(gridsExt.XMax-gridsExt.XMin)/demFrameSize ;
    pnt.Y := gridsExt.YMax-pnt.Y*(gridsExt.YMax-gridsExt.YMin)/demFrameSize ;
    Result := pnt ;
  end;

  function TGIS_Renderer3DDirectX9.mapToScreen2(
    const _ptg : TGIS_Point;
    const _z   : Double
  ) : TPoint ;
  var
    vec1, vec2         : TD3DXVector3 ;
    vp                 : TD3DViewport9 ;
    m_flXMin, m_flYMin,
    m_flXMax, m_flYMax : Double       ;
    pt                 : TPoint       ;
    robx, roby         : Double       ;
  begin
    m_flXMin := -baseSize ;
    m_flXMax :=  baseSize ;
    m_flYMin := -baseSize * grdRatio ;
    m_flYMax :=  baseSize * grdRatio ;
    robx :=  demFrameSize*(_ptg.X -gridsExt.XMin)/(gridsExt.XMax-gridsExt.XMin);
    roby := -demFrameSize*(_ptg.Y -gridsExt.YMax)/(gridsExt.YMax-gridsExt.YMin);

    vec1.X := m_flXMin+robx*(m_flXMax-m_flXMin)/demFrameSize ;
    vec1.Y := m_flYMin+roby*(m_flYMax-m_flYMin)/demFrameSize ;
    vec1.Z := _z ;

    // get the point location
    pD3DDevice.GetViewport(vp) ;
    Vec3Project(vec2,vec1,vp,mtxProjection,mtxView,mtxWorld) ;
    pt.X := RoundS(vec2.X) ;
    pt.Y := RoundS(vec2.Y) ;

    Result := pt ;
  end;

  procedure TGIS_Renderer3DDirectX9.clearScreen ;
  begin
    pD3DDevice.Clear(0, nil, D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER,
                         backColor, 1.0, 0) ;
  end;

  procedure TGIS_Renderer3DDirectX9.blendOn ;
  begin
    setTextureOperations ;
  end;

  procedure TGIS_Renderer3DDirectX9.blendOff ;
  begin
//    pD3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 0) ;
  end;

  function TGIS_Renderer3DDirectX9.saveTinBufT
    : Integer ;
  begin
    if tinBufT.NumObjects = 0 then begin
      Result := S_OK ;
      exit ;
    end;
    pD3DDevice.SetStreamSource(0, pdataVB, 0, sizeOf(TGIS_Renderer3DVertex)) ;
    pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
    if assigned( pdataTEX_tDX ) then
      pD3DDevice.SetTexture(0,pdataTEX_tDX) ;

    inherited ;
    Result := S_OK ;
  end;

  procedure TGIS_Renderer3DDirectX9.drawMeshTinTiles ;
  var
    i  : Integer ;
  begin
    // Draw mesh TIN (color)
    if meshTinTilesCounterC > 0 then begin
      pD3DDevice.SetTexture( 0, nil ) ;
      for i := 0 to meshTinTilesCounterC -1 do begin
        if transpInfo.TCT[i] <> 255 then
          setTextureOperations
        else
          blendOff ;

        TMesh(meshTinTilesC[i]).DrawSubset(0) ;
      end;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

    // Draw mesh TIN (texture)
    if meshTinTilesCounterT > 0 then begin
      pD3DDevice.SetStreamSource( 0, pdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;

      for i := 0 to meshTinTilesCounterT -1 do begin
        if transpInfo.TTT[i] <> 255 then
          setTextureOperations
        else
          blendOff ;

        pD3DDevice.SetTexture(0,arTexTinInfo[i]) ;

        TMesh(meshTinTilesT[i]).DrawSubset(0) ;
      end;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

  end;

  function  TGIS_Renderer3DDirectX9.addTinMeshTileC(
    const _tri     : Integer
  ) : Integer ;
  var
    i, mtc  : Integer ;
    numvert : DWORD ;
    j       : DWORD ;
    mesh    : TMesh ;
    vbuf    : IDirect3DVertexBuffer9 ;
    ibuf    : IDirect3DIndexBuffer9 ;
    hr      : HResult ;
  begin
    hr := createVertexBuffers( tinBufC.NumPoints, vbuf, ibuf ) ;
    if hr <> 0 then  begin
      Result := hr ;
      exit ;
    end;

    Result := CreateMeshFVF( _tri,
                             _tri *3,
                             D3DXMESH_SYSTEMMEM,
                             GISVIEWER3D_D3DFVF_GRAPH3DCOLOR,
                             pD3DDevice,
                             mesh,
                             nil, vbuf, ibuf
                           ) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    inc(meshTinTilesCounterC) ;
    SetLength(meshTinTilesC, meshTinTilesCounterC) ;
    mtc := meshTinTilesCounterC -1 ;
    meshTinTilesC[mtc] := mesh ;

    SetLength(transpInfo.TCT, meshTinTilesCounterC ) ;
    if assigned (currentML ) then
      transpInfo.TCT[mtc] := TruncS( currentML.Transparency / 100 * 255 )
    else
      transpInfo.TCT[mtc] := TruncS( currentVL.Transparency / 100 * 255 ) ;

    // Vertices
    numvert := TMesh(meshTinTilesC[mtc]).GetNumVertices ;

    {$IFDEF CLR}
      Array.Copy( tinBufC.TriLst, mesh.cvertices, tinBufC.NumPoints ) ;
    {$ELSE}
      System.Move( tinBufC.TriLst[0], mesh.cvertices[0], numvert * SizeOf(TGIS_Renderer3DVertex)) ;
    {$ENDIF}

    // Indices
      j := 0 ;
      for i := 0 to _tri - 1 do begin
        mesh.indices[j  ] := j     ;
        mesh.indices[j+1] := j + 2 ;
        mesh.indices[j+2] := j + 1 ;
        j := j + 3 ;
      end ;

      Result := TMesh(meshTinTilesC[mtc]).DrawSubset(0) ;
  end;

  function  TGIS_Renderer3DDirectX9.addTinMeshTileT(
    const _tri     : Integer
  ) : Integer ;
  var
    i, mtc  : Integer ;
    numvert : DWORD ;
    j       : DWORD ;
    mesh    : TMesh ;
    vbuf    : IDirect3DVertexBuffer9 ;
    ibuf    : IDirect3DIndexBuffer9 ;
    hr      : HResult ;
  begin
    hr := createVertexBuffers( tinBufT.NumPoints, vbuf, ibuf ) ;
    if hr <> 0 then  begin
      Result := hr ;
      exit ;
    end;

    Result := CreateMeshFVF( _tri,
                             _tri *3,
                             D3DXMESH_SYSTEMMEM,
                             GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE,
                             pD3DDevice,
                             mesh,
                             vbuf, nil, ibuf
                           ) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    inc(meshTinTilesCounterT) ;
    SetLength(meshTinTilesT, meshTinTilesCounterT) ;
    mtc := meshTinTilesCounterT -1 ;
    meshTinTilesT[mtc] := mesh ;

    SetLength(transpInfo.TTT, meshTinTilesCounterT ) ;
    if assigned( currentML ) then
      transpInfo.TTT[mtc] := TruncS( currentML.Transparency / 100 * 255 )
    else
      transpInfo.TTT[mtc] := TruncS( currentVL.Transparency / 100 * 255 ) ;
    SetLength(arTexTinInfo, meshTinTilesCounterT ) ;
      arTexTinInfo[mtc] := pdataTEX_tDX ;

    // Vertices
    numvert := TMesh(meshTinTilesT[mtc]).GetNumVertices ;
    {$IFDEF CLR}
      Array.Copy( tinBufT.TriLst, mesh.tvertices, tinBufT.NumPoints ) ;
    {$ELSE}
      System.Move( tinBufT.TriLst[0], mesh.tvertices[0], numvert * SizeOf(TGIS_Renderer3DVertex)) ;
    {$ENDIF}

    // Indices
    j := 0 ;
    for i := 0 to _tri - 1 do begin
      mesh.indices[j  ] := j     ;
      mesh.indices[j+1] := j + 2 ;
      mesh.indices[j+2] := j + 1 ;
      j := j + 3 ;
    end ;

      Result := TMesh(meshTinTilesT[mtc]).DrawSubset(0) ;
  end;

  procedure TGIS_Renderer3DDirectX9.scaleMeshTilesZ(
    const _value : Double
  ) ;
  var
    i, j    : Integer ;
    k, k0, l: Integer ;
    numvert : Integer ;
    offset  : Integer ;
    first   : Boolean ;
    z0, z1,
    z4, z5  : Single  ;
    dlt     : Single  ;
    mesh    : TMesh   ;
    cv : TGIS_Renderer3DVertex;
  begin
    if meshDemTilesCounter > 0 then
      for i := 0 to meshDemTilesCounter -1 do begin
        numvert := TMesh(meshDemTiles[i]).GetNumVertices ;
        mesh := TMesh(meshDemTiles[i]) ;
        for j := 0 to numvert - 1 do
          mesh.tvertices[j].P.Z := mesh.tvertices[j].P.Z * _value ;
      end;

    if meshWallTilesCounter > 0 then
      for i := 0 to meshWallTilesCounter -1 do begin
        numvert := TMesh(meshWallTiles[i]).GetNumVertices ;
        mesh := TMesh(meshWallTiles[i]) ;
        for j := 0 to numvert - 1 do
          mesh.tvertices[j].P.Z := mesh.tvertices[j].P.Z * _value ;
      end;

    if meshTinTilesCounterC > 0 then
      for i := 0 to meshTinTilesCounterC -1 do begin
        numvert := TMesh(meshTinTilesC[i]).GetNumVertices ;
        mesh := TMesh(meshTinTilesC[i]) ;
        for j := 0 to numvert - 1 do
          mesh.cvertices[j].P.Z := mesh.cvertices[j].P.Z * _value ;
        mesh.UpdateBuffer ;
      end;

    if meshTinTilesCounterT > 0 then
      for i := 0 to meshTinTilesCounterT -1 do begin
        numvert := TMesh(meshTinTilesT[i]).GetNumVertices ;
        mesh := TMesh(meshTinTilesT[i]) ;
        for j := 0 to numvert - 1 do
          mesh.tvertices[j].P.Z := mesh.tvertices[j].P.Z * _value ;
        mesh.UpdateBuffer ;
      end;

      if meshMPatchTilesCounterC > 0 then
        for i := 0 to meshMPatchTilesCounterC -1 do begin
          numvert := TMesh(meshMPatchTilesC[i]).GetNumVertices ;
          mesh := TMesh(meshMPatchTilesC[i]) ;
          for j := 0 to numvert - 1 do
            mesh.cvertices[j].P.Z := mesh.cvertices[j].P.Z * _value ;
        end;

      if meshMPatchTilesCounterT > 0 then
        for i := 0 to meshMPatchTilesCounterT -1 do begin
          numvert := TMesh(meshMPatchTilesT[i]).GetNumVertices ;
          mesh := TMesh(meshMPatchTilesT[i]) ;
          for j := 0 to numvert - 1 do
            mesh.tvertices[j].P.Z := mesh.tvertices[j].P.Z * _value ;
        end;

      if meshVectTilesCounterCW > 0 then
      for i := 0 to meshVectTilesCounterCW -1 do begin
        numvert := TMesh(meshVectTilesCW[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesCW[i]) ;
        j := 0 ;
        while True do begin
          z0 := mesh.cvertices[j  ].P.Z ;
          z1 := mesh.cvertices[j+1].P.Z ;
          z4 := mesh.cvertices[j+4].P.Z ;
          z5 := mesh.cvertices[j+5].P.Z ;
          dlt := z1 - z0 ;

          cv := mesh.cvertices[j  ];
          cv.P.Z := z0 * _value ;
          mesh.cvertices[j  ] := cv;

          mesh.cvertices[j  ].P.Z := z0 * _value ;
          mesh.cvertices[j+1].P.Z := mesh.cvertices[j  ].P.Z + dlt ;
          mesh.cvertices[j+3].P.Z := mesh.cvertices[j+1].P.Z ;
          dlt := z5 - z4 ;
          mesh.cvertices[j+4].P.Z := z4 * _value ;
          mesh.cvertices[j+5].P.Z := mesh.cvertices[j+4].P.Z + dlt ;
          mesh.cvertices[j+2].P.Z := mesh.cvertices[j+4].P.Z ;
          j := j + 6 ;
          if j >= numvert then break ;
        end;
        mesh.UpdateBuffer ;

//        for j := 0 to numvert - 1 do
//            mesh.cvertices[j].P.Z := vTLBufCW.TriLst[j].P.Z ;
      end;

      if meshVectTilesCounterCR > 0 then
      for i := 0 to meshVectTilesCounterCR -1 do begin
        numvert := TMesh(meshVectTilesCR[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesCR[i]) ;
        for j := 0 to numvert - 1 do
          mesh.cvertices[j].P.Z := (mesh.cvertices[j].P.Z - arVectPolyRoofC[i,j]) *
                                     _value + arVectPolyRoofC[i,j] ;
        mesh.UpdateBuffer ;
      end;

      if meshVectTilesCounterTW > 0 then
      for i := 0 to meshVectTilesCounterTW -1 do begin
        numvert := TMesh(meshVectTilesTW[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesTW[i]) ;
        j := 0 ;
        while True do begin
          z0 := mesh.tvertices[j  ].P.Z ;
          z1 := mesh.tvertices[j+1].P.Z ;
          z4 := mesh.tvertices[j+4].P.Z ;
          z5 := mesh.tvertices[j+5].P.Z ;
          dlt := z1 - z0 ;
          mesh.tvertices[j  ].P.Z := z0 * _value ;
          mesh.tvertices[j+1].P.Z := mesh.tvertices[j  ].P.Z + dlt ;
          mesh.tvertices[j+3].P.Z := mesh.tvertices[j+1].P.Z ;
          dlt := z5 - z4 ;
          mesh.tvertices[j+4].P.Z := z4 * _value ;
          mesh.tvertices[j+5].P.Z := mesh.tvertices[j+4].P.Z + dlt ;
          mesh.tvertices[j+2].P.Z := mesh.tvertices[j+4].P.Z ;
          j := j + 6 ;
          if j >= numvert then break ;
        end;

//        for j := 0 to numvert - 1 do
//          mesh.tvertices[j].P.Z := vTLBufTW.TriLst[j].P.Z ;
        mesh.UpdateBuffer ;
      end;

      if meshVectTilesCounterTR > 0 then
      for i := 0 to meshVectTilesCounterTR -1 do begin
        numvert := TMesh(meshVectTilesTR[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesTR[i]) ;
        for j := 0 to numvert - 1 do
          mesh.tvertices[j].P.Z := (mesh.tvertices[j].P.Z - arVectPolyRoofT[i][j]) *
                                      _value + arVectPolyRoofT[i][j] ;
        mesh.UpdateBuffer ;
      end;

      if meshLineTilesCounter > 0 then
      for i := 0 to meshLineTilesCounter -1 do begin
        numvert := meshLineTiles[i].Count ;
        k  := 0 ;
        k0 := 0 ;
        while True do begin
          offset := 0 ;
          while True do begin
            if (meshLineTiles[i].Buffer[k].P.X=meshLineTiles[i].Buffer[k+1].P.X)
               and
               (meshLineTiles[i].Buffer[k].P.Y=meshLineTiles[i].Buffer[k+1].P.Y)
               then inc( offset, 2 )
            else break ;
            inc( k, 2 ) ;
            if k >= numvert then break ;
          end;

          if offset = 0 then begin  // offset = 0, horizontal 1 pix lines
            for j := 0 to numvert -1 do begin
              meshLineTiles[i].Buffer[j].P.Z :=
                meshLineTiles[i].Buffer[j].P.Z * _value ;
            end;
              l := numvert ;
          end
          else
          if offset = numvert then begin // vertical 1 pix lines only
            j := 0 ;
            while True do begin
               dlt := ( meshLineTiles[i].Buffer[j+1].P.Z -
                        meshLineTiles[i].Buffer[j].P.Z ) ;
               meshLineTiles[i].Buffer[j+1].P.Z :=
                ( meshLineTiles[i].Buffer[j+1].P.Z - dlt ) * _value + dlt ;
               meshLineTiles[i].Buffer[j].P.Z :=
                 meshLineTiles[i].Buffer[j].P.Z * _value ;
               inc( j, 2 ) ;
               if j >= offset then break ;
            end;
            l := j ;
          end
          else begin  // polygon edges
            j := k0 ;
            first := True ;
            while True do begin
              dlt := ( meshLineTiles[i].Buffer[j+1].P.Z -
                       meshLineTiles[i].Buffer[j].P.Z ) ;
              // vertical
              meshLineTiles[i].Buffer[j+1].P.Z :=
                ( meshLineTiles[i].Buffer[j+1].P.Z - dlt ) * _value + dlt ;
              meshLineTiles[i].Buffer[j].P.Z :=
                meshLineTiles[i].Buffer[j].P.Z * _value ;
              // horizontal
              if first then begin
                meshLineTiles[i].Buffer[j+offset].P.Z :=
                  meshLineTiles[i].Buffer[j+1].P.Z ;
                meshLineTiles[i].Buffer[j+offset+offset -1].P.Z :=
                  meshLineTiles[i].Buffer[j+1].P.Z ;
                first := False ;
              end
              else begin
                meshLineTiles[i].Buffer[j-1+offset].P.Z :=
                  meshLineTiles[i].Buffer[j+1].P.Z ;
                meshLineTiles[i].Buffer[j+offset].P.Z :=
                  meshLineTiles[i].Buffer[j+1].P.Z ;
              end;

              inc( j, 2 ) ;
              if j >= k0 + offset then break ;
            end;
            l := k0 + 2 * offset ;
          end;

          if l >= numvert then break ;
          k := l ;
          k0 := k ;
        end;
      end;

      if high(labelInfo)>= 0 then   // labels
      for i := 0 to high(labelInfo) do  begin
        dlt := labelInfo[i].LabelZ - labelInfo[i].LabelB ;
        labelInfo[i].LabelZ := ( labelInfo[i].LabelZ - dlt ) * _value + dlt ;
        labelInfo[i].LabelB := labelInfo[i].LabelB * _value ;
      end;

  end;

  procedure TGIS_Renderer3DDirectX9.scaleMeshTilesM(
    const _value : Double
  ) ;
  var
    i, j, k : Integer ;
    k0, l   : Integer ;
    numvert : Integer ;
    offset  : Integer ;
    first   : Boolean ;
    z0, z1,
    z4, z5  : Single  ;
    dlt     : Single  ;
    mesh    : TMesh   ;
  begin
    if meshVectTilesCounterCW > 0 then  // color wall triangle
      for i := 0 to meshVectTilesCounterCW -1 do begin
        numvert := TMesh(meshVectTilesCW[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesCW[i]) ;
        j := 0 ;
        while True do begin
          z0 := mesh.cvertices[j  ].P.Z ;
          z1 := mesh.cvertices[j+1].P.Z ;
          z4 := mesh.cvertices[j+4].P.Z ;
          z5 := mesh.cvertices[j+5].P.Z ;
          dlt := ( z1 - z0 ) * _value ;
          mesh.cvertices[j  ].P.Z := z0 ;
          mesh.cvertices[j+1].P.Z := mesh.cvertices[j  ].P.Z + dlt ;
          mesh.cvertices[j+3].P.Z := mesh.cvertices[j+1].P.Z ;
          dlt := ( z5 - z4 ) * _value ;
          mesh.cvertices[j+4].P.Z := z4 ;
          mesh.cvertices[j+5].P.Z := mesh.cvertices[j+4].P.Z + dlt ;
          mesh.cvertices[j+2].P.Z := mesh.cvertices[j+4].P.Z ;
          j := j + 6 ;
          if j >= numvert then break ;
        end;
        mesh.UpdateBuffer ;
      end;

    if meshVectTilesCounterCR > 0 then    // color roof triangle
      for i := 0 to meshVectTilesCounterCR -1 do begin
        numvert := TMesh(meshVectTilesCR[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesCR[i]) ;
        for j := 0 to numvert - 1 do begin
          mesh.cvertices[j].P.Z := (mesh.cvertices[j].P.Z - arVectPolyRoofC[i][j])
                          + arVectPolyRoofC[i][j] * _value ;
          arVectPolyRoofC[i][j] := arVectPolyRoofC[i][j] * _value ;
        end;
        mesh.UpdateBuffer ;
      end;

    if meshVectTilesCounterTW > 0 then  // textured wall triangle
      for i := 0 to meshVectTilesCounterTW -1 do begin
        numvert := TMesh(meshVectTilesTW[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesTW[i]) ;
        j := 0 ;
        while True do begin
          z0 := mesh.tvertices[j  ].P.Z ;
          z1 := mesh.tvertices[j+1].P.Z ;
          z4 := mesh.tvertices[j+4].P.Z ;
          z5 := mesh.tvertices[j+5].P.Z ;
          dlt := ( z1 - z0 ) * _value ;
          mesh.tvertices[j  ].P.Z := z0 ;
          mesh.tvertices[j+1].P.Z := mesh.tvertices[j  ].P.Z + dlt ;
          mesh.tvertices[j+3].P.Z := mesh.tvertices[j+1].P.Z ;
          dlt := ( z5 - z4 ) * _value ;
          mesh.tvertices[j+4].P.Z := z4 ;
          mesh.tvertices[j+5].P.Z := mesh.tvertices[j+4].P.Z + dlt ;
          mesh.tvertices[j+2].P.Z := mesh.tvertices[j+4].P.Z ;
          j := j + 6 ;
          if j >= numvert then break ;
        end;
//        for j := 0 to numvert - 1 do
//          mesh.tvertices[j].P.Z := vTLBufTW.TriLst[j].P.Z ;
        mesh.UpdateBuffer ;
    end;

    if meshVectTilesCounterTR > 0 then    // textured roof triangle
      for i := 0 to meshVectTilesCounterTR -1 do begin
        numvert := TMesh(meshVectTilesTR[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesTR[i]) ;
        for j := 0 to numvert - 1 do begin
          mesh.tvertices[j].P.Z := (mesh.tvertices[j].P.Z - arVectPolyRoofT[i][j])
                          + arVectPolyRoofT[i][j] * _value ;
          arVectPolyRoofT[i][j] := arVectPolyRoofT[i][j] * _value ;
        end;
        mesh.UpdateBuffer ;
      end;

    if meshLineTilesCounter > 0 then  // color edges
      for i := 0 to meshLineTilesCounter -1 do begin
        numvert := meshLineTiles[i].Count ;
        k  := 0 ;
        k0 := 0 ;
        while True do begin
          offset := 0 ;
          while True do begin
            if (meshLineTiles[i].Buffer[k].P.X=meshLineTiles[i].Buffer[k+1].P.X)
               and
               (meshLineTiles[i].Buffer[k].P.Y=meshLineTiles[i].Buffer[k+1].P.Y)
               then inc( offset, 2 )
            else break ;
            inc( k, 2 ) ;
            if k >= numvert then break ;
          end;

          if offset = 0 then begin  // offset = 0, horizontal 1 pix lines
              l := numvert ;
          end
          else
          if offset = numvert then begin // vertical 1 pix lines only
            j := 0 ;
            while True do begin
               dlt := (meshLineTiles[i].Buffer[j+1].P.Z -
                       meshLineTiles[i].Buffer[j].P.Z) * _value ;
               meshLineTiles[i].Buffer[j+1].P.Z :=
                 meshLineTiles[i].Buffer[j].P.Z + dlt ;
               inc( j, 2 ) ;
               if j >= offset then break ;
            end;
            l := j ;
          end
          else  begin  // polygon edges
            j := k0 ;
            first := True ;
            while True do begin
              dlt := (meshLineTiles[i].Buffer[j+1].P.Z -
                      meshLineTiles[i].Buffer[j].P.Z) * _value ;
              // vertical
              meshLineTiles[i].Buffer[j+1].P.Z :=
                meshLineTiles[i].Buffer[j].P.Z + dlt ;
              // horizontal
              if first then begin
                meshLineTiles[i].Buffer[j+offset].P.Z :=
                  meshLineTiles[i].Buffer[j+1].P.Z ;
                meshLineTiles[i].Buffer[j+offset+offset -1].P.Z :=
                  meshLineTiles[i].Buffer[j+1].P.Z ;
                first := False ;
              end
              else begin
                meshLineTiles[i].Buffer[j-1+offset].P.Z :=
                  meshLineTiles[i].Buffer[j+1].P.Z ;
                meshLineTiles[i].Buffer[j+offset].P.Z :=
                  meshLineTiles[i].Buffer[j+1].P.Z ;
              end;

              inc( j, 2 ) ;
              if j >= k0 + offset then break ;
            end;
            l := k0 + 2 * offset ;
          end;

          if l >= numvert then break ;
          k := l ;
          k0 := k ;
        end;
      end;

      if high(labelInfo)>= 0 then   // labels
      for i := 0 to high(labelInfo) do  begin
        dlt := ( labelInfo[i].LabelZ - labelInfo[i].LabelB ) * _value ;
        labelInfo[i].LabelZ := labelInfo[i].LabelB + dlt ;
      end;

  end;

  procedure TGIS_Renderer3DDirectX9.addShapeToCache(
          _shp   : TGIS_Shape ;
    var   _abort : Boolean
  ) ;
  var
    shp_type      : TGIS_ShapeType           ;
    shpsize       : Integer                  ;
    shp,shps      : TGIS_Shape               ;
    shpc          : TGIS_ShapeComplex        ;
    old_invisible : Integer                  ;
    oparams       : TGIS_ParamsSectionVector ;
    shpobj        : TGIS_Renderer3DShape     ;
    i,k           : Integer                  ;
    lbz, lbb      : Double                   ;
    lpos          : TGIS_Point               ;
    pt            : TPoint                   ;
    p3d, orig     : TGIS_Point3D             ;

    {$IFDEF CLR}
      BMP_TRANSPARENT : Color  ;
    {$ENDIF}

    procedure fillShpGeometry(
      const _shp    : TGIS_Shape    ;
      var   _shpobj : TGIS_Renderer3DShape
      ) ;
    var
      ptg       : TGIS_Point3D ;
      part_no   : Integer      ;
      point_no  : Integer      ;
      no_points : Integer      ;
      z, d      : Double       ;
      minz      : Double       ;
      minm      : Double       ;

    begin
      d := GisWholeWorld3D.XMax / 10 ;
      minz := currentVL.Extent3D.ZMin ;
      if minz < d then minz := rangeExtent.ZMin ;
      minm := currentVL.Extent3D.MMin ;
      if minm < d then minm := 0 ;

      if currentVL.Params.NormalizedZ = TGIS_3DNormalizationType.Range then
        minz := currentVLzmi ;

      if currentVL.Params.NormalizedM = TGIS_3DNormalizationType.Range then
        minm := currentVLmmi ;

      _shp.Lock( TGIS_Lock.Projection ) ;
      for part_no := 0 to _shp.GetNumParts - 1 do begin
        _shpobj.AddPart ;
        no_points := _shp.GetPartSize(part_no) ;
        for point_no := 0 to no_points - 1 do begin
          ptg := _shp.GetPoint3D(part_no, point_no) ;
//?          ptg := currentVL.CS.ToCS3D(oGIS.CS, ptg) ;  //? check it
          if (ptg.M < ZMMIN) or (ptg.M > ZMMAX) then ptg.M := 0 ;
          if (ptg.Z < ZMMIN) or (ptg.Z > ZMMAX) then ptg.Z := 0 ;
          if currentVL.Params.NormalizedZ = TGIS_3DNormalizationType.Range then
            ptg.Z := ( ptg.Z + _shpobj.FalseZ - minz ) * currentVLnz
          else
            ptg.Z := minz + ( ptg.Z + _shpobj.FalseZ - minz ) * currentVLnz ;
          z := ptg.Z ;

          currentVL.Project3D_Ref( ptg ) ;
          if currentVL.Params.NormalizedM = TGIS_3DNormalizationType.Range then
            ptg.M := ( ptg.M + _shpobj.FalseM - minm ) * currentVLnm
          else
            ptg.M := minm + ( ptg.M + _shpobj.FalseM - minm ) * currentVLnm ;

          if bIgnoreEllipsoidHeight then
            ptg.Z := z ;

          _shpobj.AddPoint3D(ptg) ;
        end;
      end;
      _shp.Unlock ;
    end;

    function validatePolyShp( const _shp : TGIS_Shape ) : Boolean ;
      var
        i : Integer ;
      begin
        for i := 0 to _shp.GetNumParts - 1 do begin
          if _shp.GetPartSize(i) < 3 then begin
            Result := False ;
            exit;
          end;
        end;
        Result := True ;
      end;

    function isPointInExtent( _pnt : TGIS_Point; _ext : TGIS_Extent)
      : Boolean;
    begin
      if ( _pnt.X >= _ext.XMin ) and ( _pnt.X <= _ext.XMax ) and
         ( _pnt.Y >= _ext.YMin ) and ( _pnt.Y <= _ext.YMax ) then
        Result := True
      else
        Result := False ;
    end;

  begin
    if _shp.IsHidden  or
       _shp.IsDeleted or
       not _shp.Layer.Active
    then
      exit;

    if (_shp.ShapeType = TGIS_ShapeType.Point) or
       (shp_type = TGIS_ShapeType.MultiPoint) then begin
       k := oGIS.TwipsToPixels(_shp.Params.Marker.SmartSize) ;
       inc( pntNo ) ;
       if k >= 0 then begin
         p3d := _shp.GetPoint3D( 0, 0 ) ;
         if pntNo = 0 then begin
           lasCell := pixSize * k ;
           lasEx3D := _shp.Layer.Extent3D ;
           orig := GetOriginPoint ;
           lasEx3D.XMin := orig.X - 0.5 * vectorCoeff * windowWidth * pixSize ;
           lasEx3D.XMax := orig.X + 0.5 * vectorCoeff * windowWidth * pixSize ;
           lasEx3D.YMin := orig.Y - 0.5 * vectorCoeff * windowHeight * pixSize ;
           lasEx3D.YMax := orig.Y + 0.5 * vectorCoeff * windowHeight * pixSize ;
           lasEx3D := GisCommonExtent3D( _shp.Layer.Extent3D, lasEx3D ) ;
         end;

         if not ((p3d.X >= lasEx3D.XMin ) and
                 (p3d.X <= lasEx3D.XMax ) and
                 (p3d.Y >= lasEx3D.YMin ) and
                 (p3d.Y <= lasEx3D.YMax )) then exit;

         dist := Sqrt( Sqr( org.X - p3d.X ) + Sqr( org.Y - p3d.Y ) ) ;
         if dist < lasCell  then
           exit
         else
           org := p3d ;
       end;
    end;

    {$IFDEF CLR}
      BMP_TRANSPARENT := Color.Maroon ;
    {$ENDIF}

    _abort   := False ;
    oparams  := _shp.Params ;
    shps     := nil ;
    shp      := nil ;
    shp_type := _shp.ShapeType ;
    currentSHP := _shp ;
    falseMultiPatch := False ;

    labelMode := oparams.Labels.Visible and not IsStringEmpty(_shp.GetLabel) ;

    if shp_type = TGIS_ShapeType.Complex then begin
      shpc := TGIS_ShapeComplex( _shp ) ;
      k := shpc.ShapesCount ;
      for i := 0 to k -1 do
        addShapeToCache(shpc.GetShape(i), _abort) ;
      exit ;
    end;

    if shp_type = TGIS_ShapeType.Arc then
      if oGIS.TwipsToPixels(_shp.Params.Line.Width ) = 0 then  begin
        _abort := False ;
        exit ;
      end;

    if shp_type = TGIS_ShapeType.Polygon then begin
      if not validatePolyShp(_shp) then begin
        _abort := False ;
        exit ;
      end;
      if shpIsTin then begin
        shp_type := TGIS_ShapeType.MultiPatch ;
        falseMultiPatch := True ;
      end;
    end;

    old_invisible := iVectorSmartSize ;
    if shp_type = TGIS_ShapeType.Polygon then
      iVectorSmartSize := oGIS.TwipsToPixels(currentVL.Params.Area.SmartSize)
    else
    if shp_type = TGIS_ShapeType.Arc then
      iVectorSmartSize := oGIS.TwipsToPixels(currentVL.Params.Line.SmartSize) ;

    if not shpIsTin then
    if bVectorSimplification then begin
      if (shp_type <> TGIS_ShapeType.Point) and
         (shp_type <> TGIS_ShapeType.MultiPoint) then
        // Check visibility
        if not isShpVisible(_shp.Extent3D, shpsize) then exit ;
    end;

    // shape context
    if shp_type = TGIS_ShapeType.Polygon then begin
      // variant 1 - for polygon : inside the initial extent
      // exExtent  initial visible extent context
      if not _shp.IsInsideExtent(cutVectExtentV, TGIS_InsideType.Full) then
        shps := _shp.GetIntersection(cutVectExtentV,True)
      else
        shps := _shp ;
    end
    else begin
      // variant 2  - cutting to the current rough ext. for the rest shp types
      if not _shp.IsInsideExtent(cutVectExtentV , TGIS_InsideType.Full) then
        shps := _shp.GetIntersection(cutVectExtentV,True)
      else
        shps := _shp ;

      if not assigned(shps) then begin
        if shps <> _shp then
          FreeObject(shps) ;
        _abort := False ;
        exit ;
      end;
    end;

    if not assigned(shps) then
      shps := _shp ;

    // fill shape list for renderer
    if ((shp_type = TGIS_ShapeType.Arc) or (shp_type = TGIS_ShapeType.Polygon))
        and bVectorSimplification then
      shp := doSimplify( shps )
    else
      shp := shps ;
    iVectorSmartSize := old_invisible ;

    if ( shps <> _shp ) and ( shps <> shp ) then
      FreeObject(shps) ;

    if not assigned( shp ) then begin
      if shp <> _shp then
        FreeObject(shp) ;
      exit ;
    end
    else begin
      shpobj := TGIS_Renderer3DShape.Create(shp_type, _shp.Uid, currentVL) ;
      if not assigned( shpobj ) then begin
        if shp <> _shp then
          FreeObject(shp) ;
        exit ;
      end;

      inc(shpNo) ;
      shpobj.Ground       := currentVL.Params.Ground   ;
      shpobj.Basement     := currentVL.Params.Basement ;

      shpobj.ScaleZ       := _shp.Params.ScaleZ   ;
      shpobj.FalseZ       := _shp.Params.FalseZ   ;

      shpobj.ScaleM       := _shp.Params.ScaleM   ;
      shpobj.FalseM       := _shp.Params.FalseM   ;

      shpobj.IsSelected   := _shp.IsSelected  ;

      {$IFDEF CLR}
        shpobj.LabelBitmap := nil ;
      {$ELSE}
        shpobj.LabelBitmap := nil ;
      {$ENDIF}

      if labelMode then begin
        lpos := _shp.PointOnShape ;
        if isPointInExtent( lpos, cutVectExtentV ) then  begin
          pt := mapToScreen( lpos ) ;
          if printBmpImgSize <> 0 then
            pt := Point(TruncS(pt.X * printBmpOutSize.X / windowWidth),
                        TruncS(pt.Y * printBmpOutSize.Y / windowHeight) ) ;
          prepareLabelTexture( pt,
                               _shp,
                               shpobj
                              ) ;
        end;
      end;

      case shp_type of
        TGIS_ShapeType.MultiPatch   :
          begin
            shpobj.Color := oparams.Area.Color;
            shpobj.OutlineColor := oparams.Area.OutlineColor;
            if oparams.Area.Pattern = TGIS_BrushStyle.Clear then
              shpobj.ShowCeiling := False
            else
              shpobj.ShowCeiling := True ;

            if oparams.Area.OutlinePattern = TGIS_BrushStyle.Clear then
              shpobj.ShowWalls := False
            else
              shpobj.ShowWalls := True ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Area.Bitmap ) then
              shpobj.Texture := T_textureHelperDX( oTextureHelper ).Prepare(
                              pD3DDevice, oparams.Area.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Area.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperDX( oTextureHelper).Prepare(
                              pD3DDevice, oparams.Area.OutlineBitmap ) ;
          end;
        TGIS_ShapeType.Polygon      :
          begin
            shpobj.Color := oparams.Area.Color;
            shpobj.OutlineColor := oparams.Area.OutlineColor;
            if oparams.Area.Pattern = TGIS_BrushStyle.Clear then
              shpobj.ShowCeiling := False
            else
              shpobj.ShowCeiling := True ;

            if oparams.Area.OutlinePattern = TGIS_BrushStyle.Clear then
              shpobj.ShowWalls := False
            else
              shpobj.ShowWalls := True ;

           if not TGIS_Bitmap.IsNilOrEmpty( oparams.Area.Bitmap ) then
              shpobj.Texture := T_textureHelperDX( oTextureHelper ).Prepare(
                              pD3DDevice, oparams.Area.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Area.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperDX( oTextureHelper).Prepare(
                              pD3DDevice, oparams.Area.OutlineBitmap ) ;
          end;
        TGIS_ShapeType.Arc          :
          begin
            shpobj.Color := oparams.Line.Color;
            shpobj.OutlineColor := oparams.Line.OutlineColor;
            shpobj.Size := oGIS.TwipsToPixels(oparams.Line.Width) ;
            if oparams.Line.Pattern = TGIS_BrushStyle.Clear then
              shpobj.ShowCeiling := False
            else
              shpobj.ShowCeiling := True ;

             if oparams.Line.OutlinePattern = TGIS_BrushStyle.Clear then
              shpobj.ShowWalls := False
            else
              shpobj.ShowWalls := True ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Line.Bitmap ) then
              shpobj.Texture := T_textureHelperDX( oTextureHelper ).Prepare(
                              pD3DDevice, oparams.Line.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Line.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperDX( oTextureHelper).Prepare(
                              pD3DDevice, oparams.Line.OutlineBitmap ) ;
          end;
        TGIS_ShapeType.MultiPoint   :
          begin
            shpobj.Color := oparams.Marker.Color;
            shpobj.OutlineColor := oparams.Marker.OutlineColor;
            shpobj.Size := oGIS.TwipsToPixels(oparams.Marker.Size) ;
            shpobj.MarkerStyle := oparams.Marker.Style;
            if oparams.Marker.Pattern = TGIS_BrushStyle.Clear then
              shpobj.ShowCeiling := False
            else
              shpobj.ShowCeiling := True ;

            if oparams.Marker.OutlinePattern = TGIS_BrushStyle.Clear then
              shpobj.ShowWalls := False
            else
              shpobj.ShowWalls := True ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Marker.Bitmap ) then
              shpobj.Texture := T_textureHelperDX( oTextureHelper ).Prepare(
                              pD3DDevice, oparams.Marker.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Marker.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperDX( oTextureHelper).Prepare(
                                pD3DDevice, oparams.Marker.OutlineBitmap ) ;

            if _shp is TGIS_ShapePointCloud then
              shpobj.VertexColors := TGIS_ShapePointCloud(shp).VertexColors ;
          end ;
        TGIS_ShapeType.Point   :
          begin
            shpobj.Color := oparams.Marker.Color;
            shpobj.OutlineColor := oparams.Marker.OutlineColor;
            shpobj.Size := oGIS.TwipsToPixels(oparams.Marker.Size) ;
            shpobj.MarkerStyle := oparams.Marker.Style;
            if oparams.Marker.Pattern = TGIS_BrushStyle.Clear then
              shpobj.ShowCeiling := False
            else
              shpobj.ShowCeiling := True ;

            if oparams.Marker.OutlinePattern = TGIS_BrushStyle.Clear then
              shpobj.ShowWalls := False
            else
              shpobj.ShowWalls := True ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Marker.Bitmap ) then
              shpobj.Texture := T_textureHelperDX( oTextureHelper ).Prepare(
                              pD3DDevice, oparams.Marker.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Marker.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperDX( oTextureHelper).Prepare(
                              pD3DDevice, oparams.Marker.OutlineBitmap ) ;
          end;
      end;
    end;

    if shp_type = TGIS_ShapeType.MultiPatch then begin
      if not drawMultiPatchFromCache( _shp, lbz, lbb ) then
        _abort   := True ;
      shpobj.LabelZ := lbz ;
      shpobj.LabelB := lbb ;
    end
    else begin
      // read parts/points
      fillShpGeometry(shp, shpobj) ;
      drawShapeFromCache( TGIS_Renderer3DShape(shpobj), _abort ) ;
    end;

    {$IFDEF CLR}
      if shpobj.LabelBitmap <> nil then begin // store labelInfo
    {$ELSE}
      if shpobj.LabelBitmap <> nil then begin // store labelInfo
    {$ENDIF}
      k := high( labelInfo) ;
      if  k = -1 then
        SetLength( labelInfo, 1 )
      else
        SetLength( labelInfo, k + 2 ) ;

      labelInfo[k + 1].Centroid   := _shp.Centroid          ;
      labelInfo[k + 1].LabelZ     := shpobj.LabelZ          ;
      labelInfo[k + 1].LabelB     := shpobj.LabelB          ;
      labelInfo[k + 1].LabelSize  := shpobj.LabelSize       ;
      labelInfo[k + 1].LabelPos   := _shp.Params.Labels.Position ;
      labelInfo[k + 1].BitmapSize := shpobj.BitmapSize      ;

      labelInfo[k + 1].LabelTextureDX := create_texture(
                                           pD3DDevice,
                                           shpobj.LabelBitmap
                                         ) ;
    end;

    FreeObject( shpobj ) ;
    if shp <> _shp then
      FreeObject(shp) ;
  end;

  procedure TGIS_Renderer3DDirectX9.drawVectTiles ;
  var
    i, j, k, mtc : Integer ;
  begin
    // Draw Vector color mesh Roofs
    if meshVectTilesCounterCR > 0 then begin
    pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
    pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
    pD3DDevice.SetTexture(0,nil) ;
      mtc := meshVectTilesCounterCR -1 ;
      for i := 0 to mtc do begin
        j := 0 ;
        if transpInfo.PCTR[i] <> 255 then begin
          setTextureOperations ;
          j := 1 ;
        end
        else
          blendOff ;

        TMesh(meshVectTilesCR[i]).DrawSubset(j) ;
        if (length (TMesh(meshVectTilesCR[i]).subsets) > 0) and (j = 0) then
          TMesh(meshVectTilesCR[i]).DrawSubset(1) ;
      end;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

    // Draw Vector color mesh Walls
    if meshVectTilesCounterCW > 0 then begin
    pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
    pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
    pD3DDevice.SetTexture(0,nil) ;
      mtc := meshVectTilesCounterCW -1 ;
      for i := 0 to mtc do begin
        j := 0 ;
        if transpInfo.PCTW[i] <> 255 then begin
          setTextureOperations ;
          j := 1 ;
        end
        else
          blendOff ;

        TMesh(meshVectTilesCW[i]).DrawSubset(j) ;
        if (length(TMesh(meshVectTilesCW[i]).subsets) > 0) and (j = 0) then
          TMesh(meshVectTilesCW[i]).DrawSubset(1) ;
      end;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

    // Draw Vector textured mesh Roofs
    if meshVectTilesCounterTR > 0 then begin
      pD3DDevice.SetStreamSource(0, pdataVB, 0, sizeOf(TGIS_Renderer3DVertex)) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
      mtc := meshVectTilesCounterTR -1 ;
      for j := 0 to mtc do begin

        if transpInfo.PTTR[j] <> 255 then
            setTextureOperations
          else
            blendOff ;
        setTextureOperations ;
        k := high(meshTileInfoR[j]) ;
        for i := 0 to k do begin
          pD3DDevice.SetTexture(0, meshTileInfoR[j][i].TexDX) ;
          TMesh(meshVectTilesTR[j]).DrawSubset(i) ;
        end;
      end;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

     // Draw Vector textured mesh Walls
    if meshVectTilesCounterTW > 0 then begin
      pD3DDevice.SetStreamSource(0, pdataVB, 0, sizeOf(TGIS_Renderer3DVertex)) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
      mtc := meshVectTilesCounterTW -1 ;
      for j := 0 to mtc do begin

        if transpInfo.PTTW[j] <> 255 then
            setTextureOperations
          else
            blendOff ;
        setTextureOperations ;
        k := high(meshTileInfoW[j]) ;
        for i := 0 to k do begin
          pD3DDevice.SetTexture(0, meshTileInfoW[j][i].TexDX) ;
          TMesh(meshVectTilesTW[j]).DrawSubset(i) ;
        end;
      end;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

  end;

  procedure TGIS_Renderer3DDirectX9.drawMeshLinTiles ;
  var
    i, k    : Integer        ;
    pardata : T_arColorData  ;
    {$IFDEF CLR}
      cdata : DirectXStream  ;
    {$ELSE}
      cdata : PByte          ;
    {$ENDIF}
  begin
    pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
    pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
    // Draw lines from mesh
    if meshLineTilesCounter > 0 then begin
      for i := 0 to meshLineTilesCounter -1  do begin
        cdataVB_Lock( cdata ) ;
        k := meshLineTiles[i].Count ;

        {$IFDEF CLR}
          cdata.WriteRange( meshLineTiles[i].Buffer ) ;
        {$ELSE}
          System.Move(
                       meshLineTiles[i].Buffer[0],
                       cdata^,
                       k * sizeOf(TGIS_Renderer3DVertex)
                      ) ;
        {$ENDIF}
        cdataVB_Unlock( cdata, pardata ) ;
        if transpInfo.LT[i] <> 255 then
          setTextureOperations
        else
          blendOff ;
        pD3DDevice.DrawPrimitive(D3DPT_LINELIST, 0, TruncS( k / 2 )) ;
      end;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

  end;

  procedure TGIS_Renderer3DDirectX9.drawMeshPntTiles ;
  var
    i, k    : Integer        ;
    {$IFDEF CLR}
      cdata : DirectXStream  ;
    {$ELSE}
      cdata : PByte          ;
    {$ENDIF}
    pardata : T_arColorData  ;

  begin
    pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
    pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
    // Draw lines from mesh
    if meshPointTilesCounter > 0 then begin
      for i := 0 to meshPointTilesCounter -1  do begin
        cdataVB_Lock( cdata ) ;
        k := meshPointTiles[i].Count ;

        {$IFDEF CLR}
          cdata.WriteRange( meshPointTiles[i].Buffer ) ;
        {$ELSE}
          System.Move(
                      meshPointTiles[i].Buffer[0],
                      cdata^,
                      k * sizeOf(TGIS_Renderer3DVertex)
                     );
        {$ENDIF}
        cdataVB_Unlock( cdata, pardata ) ;
        if transpInfo.PT[i] <> 255 then
          setTextureOperations
        else
          blendOff ;
        pD3DDevice.DrawPrimitive(D3DPT_POINTLIST, 0, k) ;
      end;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

  end;

  procedure TGIS_Renderer3DDirectX9.drawMPatchTiles ;
  var
    i, j, k  : Integer ;
  begin
    // Draw MultiPatch mesh (color)
    if meshMPatchTilesCounterC > 0 then begin
      pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
      pD3DDevice.SetTexture(0,nil) ;
      for i := 0 to meshMPatchTilesCounterC -1 do begin
        if transpInfo.MPC[i] <> 255 then
          setTextureOperations
        else
          blendOff ;

        k := length(TMesh(meshMPatchTilesC[i]).subsets) ;
        case k of
         0 : TMesh(meshMPatchTilesC[i]).DrawSubset(0) ;
         else
           if TMesh(meshMPatchTilesC[i]).subsets[0] > 0 then
             TMesh(meshMPatchTilesC[i]).DrawSubset(0) ;
        end ;
        setTextureOperations ;
        if drawTransparent then
          TMesh(meshMPatchTilesC[i]).DrawSubset(1) ;
        blendOff ;
      end;
      blendOff ;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

    // Draw MultiPatch mesh (texture)
    if meshMPatchTilesCounterT > 0 then begin
      pD3DDevice.SetStreamSource( 0, pdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
      blendOff ;
      for j := 0 to meshMPatchTilesCounterT -1 do begin
        if transpInfo.MPT[j] <> 255 then  //?
          setTextureOperations
        else
          blendOff ;

        k := high(mpatchTileInfo[j]) ;
        for i := 0 to k do begin
          case mpatchTileInfo[j][i].Transp of
            256 : begin
                    pD3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1) ;
                    pD3DDevice.SetRenderState(D3DRS_SRCBLEND,D3DBLEND_SRCALPHA) ;
                    pD3DDevice.SetRenderState(D3DRS_DESTBLEND,D3DBLEND_INVSRCALPHA) ;
                    pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
                  end ;
            255  : blendOff ;
            else   setTextureOperations ;
          end ;
          pD3DDevice.SetTexture(0, mpatchTileInfo[j][i].TexDX) ;

          if not((mpatchTileInfo[j][i].Transp <> 255) and ( not drawTransparent)) then
            TMesh(meshMPatchTilesT[j]).DrawSubset(i) ;
        end ;
      end ;
      pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    end;

  end;

  procedure TGIS_Renderer3DDirectX9.prepareLabelTexture(
    const _org    : TPoint     ;
    const _shp    : TGIS_Shape ;
    const _shp_lt : TObject
  ) ;
  var
    prm     : TGIS_ParamsLabel ;
    txt     : String           ;
    bmp     : TGIS_Bitmap      ;
    shp_lt  : TGIS_Renderer3DShape ;
    lbl     : TObject ;
    pnt     : TPoint  ;
  begin
    assert( assigned( _shp    ) ) ;
    assert( assigned( _shp_lt ) ) ;

    prm    := _shp.Params.Labels ;

    txt := _shp.GetLabel ;

    if IsStringEmpty( txt ) then
      exit ;

    if not prm.Duplicates then begin
      if LabelsReg.IsDuplicated( txt ) then  exit ;

      LabelsReg.AddDuplicated( txt ) ;
    end ;

    lbl := TGIS_RendererAbstract(
             oGIS.ViewerParent.ControlRenderer
           ).MeasureShieldTexture( _shp, pnt ) ;

    if assigned( lbl ) then begin
      if prm.Allocator then begin
        if not LabelsReg.Allocate( GisExtent(
                                     _org.X,
                                     _org.Y,
                                     _org.X + pnt.X,
                                     _org.Y + pnt.Y
                                   ),
                                   _shp.Layer.GetHashCode,
                                   _shp.Uid,
                                   1.5/oGIS.Zoom
                                 )
        then begin
          FreeObject(lbl) ;
          exit;
        end;
      end ;

      bmp := TGIS_RendererAbstract(
               oGIS.ViewerParent.ControlRenderer
             ).RenderShieldTexture( _shp, True, pnt, lbl ) ;

      if not assigned( bmp ) then
        exit ;

      shp_lt := TGIS_Renderer3DShape( _shp_lt ) ;
      shp_lt.BitmapSize := Point( bmp.Width, bmp.Height ) ;
      shp_lt.LabelSize := pnt ;
      shp_lt.LabelBitmap := bmp ;
    end;
  end;

  function TGIS_Renderer3DDirectX9.mapToScreen(
    const _ptg : TGIS_Point
  ) : TPoint ;
  var
    vec1, vec2          : TD3DXVector3 ;
    vp                  : TD3DViewport9 ;
    m_flXMin, m_flYMin,
    m_flXMax, m_flYMax  : Double ;
    x_ratio, y_ratio    : Double ;
    pt                  : TPoint ;
    robx, roby          : Double ;
  begin
    m_flXMin := -baseSize ;
    m_flXMax :=  baseSize ;
    m_flYMin := -baseSize*grdRatio ;
    m_flYMax :=  baseSize*grdRatio ;
    x_ratio  :=  demFrameSize/(2.0*baseSize) ;
    y_ratio  :=  demFrameSize/(2.0*m_flYMax) ;

    robx :=  demFrameSize*(_ptg.X -gridsExt.XMin)/(gridsExt.XMax-gridsExt.XMin);
    roby := -demFrameSize*(_ptg.Y -gridsExt.YMax)/(gridsExt.YMax-gridsExt.YMin);

    vec1.X := m_flXMin+(robx + dTurn*x_ratio)*(m_flXMax-m_flXMin)/demFrameSize ;
    vec1.Y := m_flYMin+(roby +  dFly*y_ratio)*(m_flYMax-m_flYMin)/demFrameSize ;
    vec1.Z := 0.0 ;

    // get the mouse location
    pD3DDevice.GetViewport(vp) ;
    Vec3Project(vec2,vec1,vp,mtxProjection,mtxView,mtxWorld) ;
    pt.X := RoundS(vec2.X) ;
    pt.Y := RoundS(vec2.Y) ;

    Result := pt ;
  end ;

  function  TGIS_Renderer3DDirectX9.drawVectorTexture(
    const _shp  : TObject           ;
    const _part : Boolean           ;
    const _data : T_arColorData     ;
    const _prim : Integer           ;
    const _num  : Integer           ;
    const _len  : Double
  ) : Integer ;
  var
    i, k    : Integer         ;
    pardata : T_arTextureData ;
    cur_lgth,
    lgth,
    val     : Double          ;
    a       : Cardinal        ;
    ext     : TGIS_Extent     ;
    pnt     : TGIS_Point      ;
    dir     : Boolean         ;
    shp     : TGIS_Renderer3DShape   ;
  begin
    shp := TGIS_Renderer3DShape(_shp) ;
    dir := texDir ;

    if _part then // roof
      pdataTEX_vDX := T_textureHelperDX( oTextureHelper ).Get( shp.Texture )
    else
      pdataTEX_vDX := T_textureHelperDX( oTextureHelper ).Get( shp.OutlineTexture ) ;

    case _prim of
        1 : SetLength( pardata, _num ) ;
        0 : SetLength( pardata, _num * 3) ;
    end;

    if shp.IsSelected then
      a := TruncS(oGIS.SelectionTransparency/100.0*255.0)
    else
    a := TruncS (currentVL.Transparency/100*255) ;
    k := _num ;
    if _part then begin //roof
      case _prim of
        1 : k := _num ;
        0 : k := _num * 3;
      end;
      ext := shpExt3D(_data, k) ;
      for i := 0 to k-1 do begin
        pardata[i].P := _data[i].P ;
        pardata[i].N := _data[i].N ;
        if shp.IsSelected then
          pardata[i].Color := D3DCOLOR_ARGB(a,
                                          oGIS.SelectionGisColor.R,
                                          oGIS.SelectionGisColor.G,
                                          oGIS.SelectionGisColor.B
                                         )
        else
          pardata[i].Color := D3DCOLOR_ARGB(a,
                                          shp.Color.R,
                                          shp.Color.G,
                                          shp.Color.B
                                         ) ;
        pnt.X := pardata[i].P.X ;
        pnt.Y := pardata[i].P.Y ;
        pnt := vectTexVal(ext,pnt) ;
        if dir then begin
          pardata[i].Tu := pnt.X ;
          pardata[i].Tv := pnt.Y ;
        end
        else begin
          pardata[i].Tu := 1 -pnt.X ;
          pardata[i].Tv :=    pnt.Y ;
        end;
      end;
      case _prim of
        1 : k := _num-2 ;
        0 : k := _num ;
      end;
    end
    else begin  //wall
      if not texMode then
        pnt := zRange(_data, _num) ;
        k   := _num - 2 ;
        lgth := _len ;
        if lgth = 0 then
          lgth := markerLgth ;
        cur_lgth := 0 ;
        for i := 0 to _num-1 do begin
          pardata[i].P := _data[i].P ;
          pardata[i].N := _data[i].N ;
          if shp.IsSelected then
            pardata[i].Color := D3DCOLOR_ARGB(a,
                                          oGIS.SelectionGisColor.R,
                                          oGIS.SelectionGisColor.G,
                                          oGIS.SelectionGisColor.B
                                         )
          else
            pardata[i].Color := D3DCOLOR_ARGB(a,
                                            shp.OutlineColor.R,
                                            shp.OutlineColor.G,
                                            shp.OutlineColor.B
                                           ) ;
          if i mod 2 = 0 then begin
            if i>=2 then begin
              val := Sqrt(Sqr(pardata[i].P.X - pardata[i-2].P.X) +
                          Sqr(pardata[i].P.Y - pardata[i-2].P.Y)) ;
              cur_lgth := cur_lgth + val ;
             end;
             if cur_lgth > lgth then begin
               cur_lgth := 0 ;
               dir := not dir ;
             end;
             if dir then
               pardata[i].Tu := cur_lgth / lgth
             else
               pardata[i].Tu := 1-cur_lgth / lgth ;
             if texMode then
               pardata[i].Tv := 1.0
             else
               pardata[i].Tv := 1-(pardata[i].P.Z - pnt.X)/(pnt.Y - pnt.X) ;
           end
           else begin
             pardata[i].Tu := pardata[i-1].Tu ;
             if texMode then
               pardata[i].Tv := 0.0
             else
               pardata[i].Tv := 1-(pardata[i].P.Z - pnt.X)/(pnt.Y - pnt.X) ;
           end;
        end;
      end;

    pD3DDevice.SetStreamSource(0, pdataVB, 0, sizeOf(TGIS_Renderer3DVertex)) ;
    pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
    pD3DDevice.SetTexture(0,pdataTEX_vDX) ;
    if a <> 255 then
      setTextureOperations ;

    case _prim of
      1 : triStripToListT(_part, pardata, k) ;
      0 : triListToListT (_part, pardata, k) ;
    end;

    pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
    pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
    pD3DDevice.SetTexture(0,nil) ;
    pD3DDevice.SetTextureStageState(0,D3DTSS_ALPHAARG1,D3DTA_TEXTURE) ;
    Result := S_OK ;
  end;

  procedure TGIS_Renderer3DDirectX9.setPolyRoofN(
    var   _ver1 : TGIS_Renderer3DVertex ;
    var   _ver2 : TGIS_Renderer3DVertex ;
    var   _ver3 : TGIS_Renderer3DVertex ;
    const _wind : Boolean
  ) ;
  var
    v0, v1, v2    : TVector3f  ;
  begin
    v0.X := _ver1.P.X ;
    v0.Y := _ver1.P.Y ;
    v0.Z := _ver1.P.Z ;
    v1.X := _ver2.P.X ;
    v1.Y := _ver2.P.Y ;
    v1.Z := _ver2.P.Z ;
    v2.X := _ver3.P.X ;
    v2.Y := _ver3.P.Y ;
    v2.Z := _ver3.P.Z ;
    _ver1.N := triangleNormal(v0, v1, v2) ;
    _ver2.N := triangleNormal(v1, v2, v0) ;
    _ver3.N := triangleNormal(v2, v0, v1) ;
    if not _wind then begin
        _ver1.N.X := -_ver1.N.X ;
        _ver1.N.Y := -_ver1.N.Y ;
        _ver1.N.Z := -_ver1.N.Z ;
        _ver2.N.X := -_ver2.N.X ;
        _ver2.N.Y := -_ver2.N.Y ;
        _ver2.N.Z := -_ver2.N.Z ;
        _ver3.N.X := -_ver3.N.X ;
        _ver3.N.Y := -_ver3.N.Y ;
        _ver3.N.Z := -_ver3.N.Z ;
      end;
  end;

  function TGIS_Renderer3DDirectX9.saveVTLBufC(
    const _part : Boolean ;
    var   _buf  : TGIS_Renderer3DColorTriangleListBuffer
  ) : Integer ;
  begin
    if _buf.NumObjects = 0 then
    begin
      Result := S_OK ;
      exit ;
    end;

    if _part then begin // Roofs
      Result := addVectMeshTileC( meshVectTilesCounterCR ,
                                    meshVectTilesCR        ,
                                    vTLBufCR               ,
                                    vTLBufCR.NumObjects
                                   ) ;
      if Result <> S_OK then begin
        safeRendering := True ;
        vTLBufCR.NumPoints  := 0 ;
        vTLBufCR.NumObjects := 0 ;
        SetLength( vTLBufCR.TriLst, 0 ) ;

        if (Result = -2147024882) then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                              'Out of memory', 3 )
        else
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                'saveVTLBufC', 3 ) ;
      end;

      SetLength(transpInfo.PCTR, meshVectTilesCounterCR ) ;
      transpInfo.PCTR[meshVectTilesCounterCR -1] :=
        TruncS( currentVL.Transparency / 100 * 255 ) ;
      addVectorTilesInfo( 2, meshVectTilesCounterCR -1 ) ;
    end
    else begin  // Walls
      Result := addVectMeshTileC( meshVectTilesCounterCW ,
                                    meshVectTilesCW        ,
                                    vTLBufCW               ,
                                    vTLBufCW.NumObjects
                                  ) ;
      if Result <> S_OK then begin
        safeRendering := True ;
        vTLBufCW.NumPoints  := 0 ;
        vTLBufCW.NumObjects := 0 ;
        SetLength( vTLBufCW.TriLst, 0 ) ;

        if (Result = -2147024882) then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                              'Out of memory', 4 )
        else
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                              'saveVTLBufC', 4 ) ;
      end;
      SetLength(transpInfo.PCTW, meshVectTilesCounterCW ) ;
      transpInfo.PCTW[meshVectTilesCounterCW -1] :=
        TruncS( currentVL.Transparency / 100 * 255 ) ;
      addVectorTilesInfo( 1, meshVectTilesCounterCW -1 ) ;
    end;

    _buf.NumPoints  := 0 ;
    _buf.NumObjects := 0 ;
    SetLength( _buf.TriLst, 0 ) ;

    Result := S_OK ;

  end;

  function TGIS_Renderer3DDirectX9.saveVTLBufT(
    const _part : Boolean       ;
    var   _buf  : TGIS_Renderer3DTextureTriangleListBuffer
  ) : HResult ;
  var
    hr : HResult ;
  begin
    if _buf.NumObjects = 0 then
      begin
        Result := S_OK ;
        exit ;
      end;

      if _part then begin // Roofs
        hr := addVectMeshTileT( meshVectTilesCounterTR , meshVectTilesTR ,
                                vTLBufTR , vTLBufTR.NumObjects ) ;
        if hr <> S_OK then begin
          safeRendering := True ;
          vTLBufTR.NumPoints  := 0 ;
          vTLBufTR.NumObjects := 0 ;
          SetLength(vTLBufTR.TriLst, 0 ) ;

          if (Result = -2147024882) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                 'Out of memory', 1 )
          else
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                 'saveVTLBufT', 1 ) ;
        end;

        SetLength(transpInfo.PTTR, meshVectTilesCounterTR ) ;
        transpInfo.PTTR[meshVectTilesCounterTR -1] :=
          TruncS( currentVL.Transparency / 100 * 255 ) ;
        addVectorTilesInfo( 4, meshVectTilesCounterTR -1 ) ;
      end
      else begin
        hr := addVectMeshTileT( meshVectTilesCounterTW , meshVectTilesTW ,
                                vTLBufTW , vTLBufTW.NumObjects ) ;
        if hr <> S_OK then begin
          safeRendering := True ;
          vTLBufTW.NumPoints  := 0 ;
          vTLBufTW.NumObjects := 0 ;
          SetLength(vTLBufTW.TriLst, 0 ) ;

          if (Result = -2147024882) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                 'Out of memory', 1 )
          else
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                 'saveVTLBufT', 1 ) ;

        end;

        SetLength(transpInfo.PTTW, meshVectTilesCounterTW ) ;
        transpInfo.PTTW[meshVectTilesCounterTW -1] :=
          TruncS( currentVL.Transparency / 100 * 255 ) ;
        addVectorTilesInfo( 3, meshVectTilesCounterTW -1 ) ;
      end;
      _buf.NumPoints  := 0 ;
      _buf.NumObjects := 0 ;
//      SetLength(_buf.TriLst, 0 ) ;

      Result := S_OK ;

  end;

  function  TGIS_Renderer3DDirectX9.addVectMeshTileC(
    var   _count   : Integer              ;
    var   _mesh    : T_arMesh             ;
    var   _buf     : TGIS_Renderer3DColorTriangleListBuffer ;
    const _tri     : DWORD
  ) : HResult ;
  var
    i, mtc  : Integer ;
    numvert : DWORD ;
    j       : DWORD ;
    aa      : Byte    ;
    subs    : Boolean ;
    mesh    : TMesh ;
    vbuf    : IDirect3DVertexBuffer9 ;
    ibuf    : IDirect3DIndexBuffer9 ;
    hr      : HResult ;
  begin
    hr := createVertexBuffers( _buf.NumPoints, vbuf, ibuf ) ;
    if hr <> 0 then  begin
      Result := hr ;
      exit ;
    end;

    Result := CreateMeshFVF( _tri,
                             _tri *3,
                             D3DXMESH_SYSTEMMEM,
                             GISVIEWER3D_D3DFVF_GRAPH3DCOLOR,
                             pD3DDevice,
                             mesh,
                             nil, vbuf, ibuf
                           ) ;

    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    inc(_count) ;
    SetLength(_mesh, _count) ;
    mtc := _count -1 ;
    _mesh[mtc] := mesh ;

    // Vertices
    numvert := TMesh(_mesh[mtc]).GetNumVertices ;

    {$IFDEF CLR}
      Array.Copy( _buf.TriLst, mesh.cvertices, numvert ) ;
    {$ELSE}
      System.Move( _buf.TriLst[0], mesh.cvertices[0], numvert * SizeOf(TGIS_Renderer3DVertex)) ;
    {$ENDIF}

    // Indices
    j := 0 ;
    for i := 0 to _tri - 1 do begin
      mesh.indices[j]   :=  j  ;
      mesh.indices[j+1] :=  j + 2 ;
      mesh.indices[j+2] :=  j + 1 ;
      j := j + 3 ;
    end ;

    // subsets
    subs := False ;
    for i := 0 to _tri - 1 do begin
      aa := Byte( _buf.TriLst[3*i].Color shr 24 ) ;
      if aa = 255 then
        mesh.faces[i] := 0
      else  begin
        subs := True ;
        mesh.faces[i] := 1 ;
      end;
    end ;

    if subs = True  then
      mesh.SortIndices( 2 );
  end;

  function  TGIS_Renderer3DDirectX9.addVectMeshTileT(
    var   _count   : Integer               ;
    var   _mesh    : T_arMesh              ;
    var   _buf     : TGIS_Renderer3DTextureTriangleListBuffer ;
    const _tri     : DWORD
  ) : HResult ;
  var
    i, mtc  : Integer ;
    numvert : DWORD ;
    j       : DWORD ;
    subs    : DWORD ;
    mesh    : TMesh ;
    vbuf    : IDirect3DVertexBuffer9 ;
    ibuf    : IDirect3DIndexBuffer9 ;
    hr      : HResult ;
  begin
    hr := createVertexBuffers( _buf.NumPoints, vbuf, ibuf ) ;
    if hr <> 0 then  begin
      Result := hr ;
      exit ;
    end;

    Result := CreateMeshFVF( _tri,
                                 _tri *3,
                                 D3DXMESH_SYSTEMMEM,
                                 GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE,
                                 pD3DDevice,
                                 mesh,
                                 vbuf, nil, ibuf
                               ) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    inc(_count) ;
    SetLength(_mesh, _count) ;
    mtc := _count -1 ;
    _mesh[mtc] := mesh ;

    // Vertices
    numvert := TMesh(_mesh[mtc]).GetNumVertices ;
    {$IFDEF CLR}
      Array.Copy( _buf.TriLst, mesh.tvertices, numvert ) ;
    {$ELSE}
      System.Move( _buf.TriLst[0], mesh.tvertices[0], numvert * SizeOf(TGIS_Renderer3DVertex)) ;
    {$ENDIF}

    // Indices
    j := 0 ;
    for i := 0 to _tri - 1 do begin
      mesh.indices[j  ] := j     ;
      mesh.indices[j+1] := j + 2 ;
      mesh.indices[j+2] := j + 1 ;
      j := j + 3 ;
    end ;

    // attribs
    subs := 0 ;
    for i := 0 to _tri - 1 do begin
      mesh.faces[i] := _buf.Subset[i] ;
      if _buf.Subset[i] > subs then
        subs := _buf.Subset[i] ;
    end ;

    TMesh(_mesh[mtc]).SortIndices( subs +1 ) ;
  end;

  procedure TGIS_Renderer3DDirectX9.addToVTLBufT(
    const _part : Boolean               ;
    var   _buf  : TGIS_Renderer3DTextureTriangleListBuffer ;
    const _ptg1 : TGIS_Renderer3DVertex       ;
    const _ptg2 : TGIS_Renderer3DVertex       ;
    const _ptg3 : TGIS_Renderer3DVertex
  ) ;
  var
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;

    procedure setSubsets ;
    begin
      if _part then begin   // roof
        if curMeshTexR <> pdataTEX_vDX then begin
          inc(curMeshSubset) ;
          SetLength(meshTileInfoR, meshVectTilesCounterTR +1) ;
          SetLength(meshTileInfoR[meshVectTilesCounterTR], curMeshSubset +1) ;
          meshTileInfoR[meshVectTilesCounterTR][curMeshSubset].Subset :=
            curMeshSubset ;
          meshTileInfoR[meshVectTilesCounterTR][curMeshSubset].TexDX    :=
            pdataTEX_vDX    ;
          curMeshTexR := pdataTEX_vDX ;
          vTLBufTR.Subset[vTLBufTR.NumObjects] := curMeshSubset ;
          lastMeshSubsetR := curMeshSubset ;
        end
        else begin
          vTLBufTR.Subset[vTLBufTR.NumObjects] := lastMeshSubsetR ;
        end;
      end
      else begin           // wall
        if curMeshTexW <> pdataTEX_vDX then begin
          inc(curMeshSubset) ;
          SetLength(meshTileInfoW, meshVectTilesCounterTW +1) ;
          SetLength(meshTileInfoW[meshVectTilesCounterTW], curMeshSubset +1) ;
          meshTileInfoW[meshVectTilesCounterTW][curMeshSubset].Subset :=
            curMeshSubset ;
          meshTileInfoW[meshVectTilesCounterTW][curMeshSubset].TexDX    :=
            pdataTEX_vDX    ;
          curMeshTexW := pdataTEX_vDX ;
          vTLBufTW.Subset[vTLBufTW.NumObjects] := curMeshSubset ;
          lastMeshSubsetW := curMeshSubset ;
        end
        else begin
          vTLBufTW.Subset[vTLBufTW.NumObjects] := lastMeshSubsetW ;
        end;
      end;
    end;

  begin
    {$IFDEF CLR}
       { TODO -cReview : check in CLR }
       //? SetLength( _buf.TriLst, _buf.NumPoints + 3 ) ;
    {$ENDIF}

    if length(_buf.TriLst) = 0 then begin
      SetLength( _buf.TriLst ,  initVideoVBSize ) ;
    end;
    if _buf.NumPoints+6 < videoVertexBufferSize then begin
      setSubsets ;
      _buf.TriLst[_buf.NumPoints] := _ptg1 ;
      inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := _ptg2 ;
      inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := _ptg3 ;
      inc(_buf.NumPoints) ;
      inc(_buf.NumObjects) ;
    end
    else begin
      ptg1 := _ptg1 ;
      ptg2 := _ptg2 ;
      ptg3 := _ptg3 ;
      SetLength( _buf.TriLst ,  length(_buf.TriLst) + videoVBIncrement ) ;
{
      saveVTLBufT( _part, _buf ) ;
      curMeshSubset := -1  ;
      lastMeshSubsetR := -1 ;
      lastMeshSubsetW := -1 ;
      curMeshTexR     := nil ;
      curMeshTexW     := nil ;
}
      setSubsets ;
      _buf.TriLst[_buf.NumPoints] := ptg1 ;
      inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := ptg2 ;
      inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := ptg3 ;
      inc(_buf.NumPoints) ;
      inc(_buf.NumObjects) ;
    end;
  end;

  function TGIS_Renderer3DDirectX9.saveMPatchBufC
    : Integer ;
  var
    hr : HResult ;
  begin
    Result := S_OK ;
    if mpatchBufC.NumObjects = 0 then
      begin
        Result := S_OK ;
        exit ;
      end;

    hr := addVectMeshTileC( meshMPatchTilesCounterC ,
                                 meshMPatchTilesC        ,
                                 mpatchBufC              ,
                                 mpatchBufC.NumObjects
                                ) ;

    if hr <> S_OK then begin
      safeRendering := True ;
      mpatchBufC.NumPoints  := 0 ;
      mpatchBufC.NumObjects := 0 ;
      SetLength(mpatchBufC.TriLst, 0 ) ;

      if (hr = -2147024882) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'Out of memory', 1 )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'saveMPatchBufC', 1 ) ;

    end;

    mpatchBufC.NumPoints  := 0 ;
    mpatchBufC.NumObjects := 0 ;
    SetLength(mpatchBufC.TriLst, 0 ) ;

    SetLength(transpInfo.MPC, meshMPatchTilesCounterC ) ;
    transpInfo.MPC[meshMPatchTilesCounterC -1] :=
      TruncS( currentVL.Transparency / 100 * 255 ) ;
    addVectorTilesInfo( 9, meshMPatchTilesCounterC -1 ) ;
    Result := S_OK ;
  end;

  function TGIS_Renderer3DDirectX9.saveMPatchBufT
    : HResult ;
  begin
    if mpatchBufT.NumObjects = 0 then
      begin
        Result := S_OK ;
        exit ;
      end;

    Result := addVectMeshTileT( meshMPatchTilesCounterT ,
                                meshMPatchTilesT        ,
                                mpatchBufT              ,
                                mpatchBufT.NumObjects
                               ) ;
    if Result <> S_OK then begin
      safeRendering := True ;
      mpatchBufT.NumPoints  := 0 ;
      mpatchBufT.NumObjects := 0 ;
      SetLength( mpatchBufT.TriLst, 0 ) ;

      if (Result = -2147024882) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'Out of memory', 2 )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'saveMPatchBufT', 2 ) ;
    end;

    mpatchBufT.NumPoints  := 0 ;
    mpatchBufT.NumObjects := 0 ;
    SetLength( mpatchBufT.TriLst, 0 ) ;

    SetLength(transpInfo.MPT, meshMPatchTilesCounterT ) ;
    transpInfo.MPT[meshMPatchTilesCounterT -1] :=
      TruncS( currentVL.Transparency / 100 * 255 ) ;

    mpatchSubsetCounter := -1 ;

    inc( mpatchSubsetCounter ) ;
    SetLength( mpatchTileInfo, meshMPatchTilesCounterT +1 ) ;
    SetLength( mpatchTileInfo[meshMPatchTilesCounterT],
                               mpatchSubsetCounter +1 ) ;
    mpatchTileInfo[meshMPatchTilesCounterT]
                  [mpatchSubsetCounter].Subset := mpatchSubsetCounter ;
    mpatchTileInfo[meshMPatchTilesCounterT]
                    [mpatchSubsetCounter].TexDX := pdataTEX_vDX ;
    addVectorTilesInfo( 10, meshMPatchTilesCounterT -1 ) ;
    Result := S_OK ;
  end;

  function  TGIS_Renderer3DDirectX9.drawMultiPatchFromCache
                                     ( const _shp   : TGIS_Shape ;
                                       var   _lbz   : Double     ;
                                       var   _lbb   : Double
                                     ) : Boolean ;
  var
    ptype       : TGIS_PartType ;
    part_no     : Integer       ;
    point_no    : Integer       ;
    num_parts   : Integer       ;
    num_points  : Integer       ;
    ptg, ptg0   : TGIS_Point3D  ;
    cptg        : TGIS_Point    ;
    first_pnt   : TGIS_Point3D  ;
    pardata     : T_arTextureData ;
    cl          : TGIS_Color    ;
    r,g,b       : Byte          ;
    a           : Cardinal      ;
    ar_color    : TD3DColor     ;
    ou_color    : TD3DColor     ;
    area        : Double        ;
    arXY        : Double        ;
    arXZ        : Double        ;
    arYZ        : Double        ;
    wasReversed : Boolean       ;
    iRingNum    : Integer       ;
    triMode     : Integer       ;
    prtdsc      : TGIS_PartDescriptor ;
    isgdb       : Boolean       ;
    isgdbtex    : Boolean       ; // textured gdb shape
    isnormal    : Boolean       ;
    istexture   : Boolean       ;
    ismaterial  : Boolean       ;
    isedge      : Boolean       ;
    num_normals : Integer       ;
    pnt_counter : Integer       ;
    lasta       : Cardinal      ;
    td          : Integer       ;
    lastTexDX   : IDirect3DTexture9 ;
    mtrlinfo    : T_arMtrlInfo  ;
    lparams     : TGIS_ParamsSectionVector ;
    lp_falseZ   : Double ;
    lp_falseM   : Double ;
    shp_minz    : Double ;
    min_valz    : Double ;
    groundType  : TGIS_3DGroundType ;
    basementType: TGIS_3DBasementType ;
    vedx, vedy,
    redx, redy  : Double ;
    {$IFDEF CLR}
      vec3      : TD3DVector ;
    {$ENDIF}

      procedure setScaleGlobals ;
      var
        valx, valy   : Double        ;
        i, j, kk, jj : Integer       ;
      begin
        shp_minz := _shp.Extent3D.ZMin ;
        vedx := vectExtent.XMax - vectExtent.XMin ;
        vedy := vectExtent.YMax - vectExtent.YMin ;
        redx := roughExtent.XMax - roughExtent.XMin ;
        redy := roughExtent.YMax - roughExtent.YMin ;
        groundType := _shp.Params.Ground ;
        basementType := _shp.Params.Basement ;
        min_valz := defaultZ ;
        if basementType = TGIS_3DBasementType.Lowest then begin
          num_parts  := _shp.GetNumParts  ;
          num_points := _shp.GetNumPoints ;
          for i := 0 to num_parts -1 do
            for j := 0 to num_points -1 do begin
              ptg := _shp.GetPoint3D(i, j) ;
              if ptg.Z = shp_minz then begin
                valx := ( vectExtent.XMax - ptg.X) / vedx ;
                valy := ( vectExtent.YMax - ptg.Y) / vedy ;
                kk := demFrameSize - TruncS(valx * demFrameSize) ;
                jj := TruncS(valy * demFrameSize) ;
                if (jj < 0) or (jj >= demFrameSize - 1) or (kk < 0) or
                   (kk >= demFrameSize - 1) then // point out of DEM extent
                  min_valz := defaultZ
                else begin                      // point within DEM extent
                  min_valz := zValue(kk, jj) ;
                end;
                exit;
              end;

            end;

        end;
      end;

      // _ptg in 3D coordinates
      function getScaledPoint3D( const _ptg : TGIS_Point3D ) : TGIS_Renderer3DVertex ;
      var
        valx, valy  : Double        ;
        Z, dz, valz : Double        ;
        pnt         : TGIS_Point3D  ;
        kk, jj      : Integer       ;

      begin
        pnt := _ptg ;
        Z := pnt.Z ;
        dz := zFactor * (Z - shp_minz) * lparams.ScaleZ ;

        currentVL.Project3D_Ref( pnt ) ;
        if bIgnoreEllipsoidHeight then
          pnt.Z := Z ;

        pnt.Z := shp_minz + lp_falseZ ;
        pnt.Z := pnt.Z * zFactor ;

        pnt.M := pnt.M + lp_falseM ;
        valx := ( vectExtent.XMax - pnt.X) / vedx ;
        valy := ( vectExtent.YMax - pnt.Y) / vedy ;
        Result.P.X := roughExtent.XMax - valx * redx ;
        Result.P.Y := roughExtent.YMin + valy * redy ;

        // get shp Z value from DEM
        kk := demFrameSize - TruncS(valx * demFrameSize) ;
        jj := TruncS(valy * demFrameSize) ;
        // gisGroundOnDem
        if (jj < 0) or (jj >= demFrameSize - 1) or (kk < 0) or
          (kk >= demFrameSize - 1) then // point out of DEM extent
          Result.P.Z := defaultZ + (dz + lp_falseZ) / zUnit
        else begin                      // point within DEM extent
          valz := zValue(kk, jj) ;
          if basementType = TGIS_3DBasementType.Lowest then
            valz := min_valz ;
          Result.P.Z := valz + (dz + lp_falseZ) / zUnit ;
        end;

        if groundType = TGIS_3DGroundType.AboveDem  then // above DEM
          Result.P.Z := Result.P.Z + projFactor * pnt.Z / zUnit
        else
        if groundType = TGIS_3DGroundType.AboveZero then // above 0
          Result.P.Z := projFactor * (pnt.Z + dz - zLevel) / zUnit ;

        if pnt.M <> 0 then
          Result.P.Z := Result.P.Z + (pnt.M * projFactor * mFactor) / zUnit ;
        Result.N   := vector3InitDX9( 0, 0 , 1 ) ;
        Result.Color := ar_color ;
      end;

      function reverseLight( var _ptg : TVector3f ) : TVector3f ;
      begin
        Result.X := -_ptg.X ;
        Result.Y := -_ptg.Y ;
        Result.Z := -_ptg.Z ;
      end;

      // closedRing checking
      function closedRing( const _num : Integer ) : Boolean ;
      begin
        if (num_points > 0) and (_num <= num_points) then begin
          if (pardata[0].P.X = pardata[_num-1].P.X) and
             (pardata[0].P.Y = pardata[_num-1].P.Y) and
             (pardata[0].P.Z = pardata[_num-1].P.Z) then
            Result := True
          else
            Result := False ;
        end
        else
          Result := False ;
      end;

      // closed Strip checking
      function closedStrip( const _num : Integer ) : Boolean ;
      begin
        if (num_points > 0) and (_num <= num_points) then begin
          if (pardata[0].P.X = pardata[_num-1].P.X) and
             (pardata[0].P.Y = pardata[_num-1].P.Y) and
             (pardata[0].P.Z = pardata[_num-1].P.Z) and
             (pardata[1].P.X = pardata[_num-2].P.X) and
             (pardata[1].P.Y = pardata[_num-2].P.Y) and
             (pardata[1].P.Z = pardata[_num-2].P.Z) then
            Result := True
          else
            Result := False ;
        end
        else
          Result := False ;
      end;

      // drawEdges
      procedure drawEdges( const _num : Integer ) ;
      var
        ptg1 : TGIS_Renderer3DVertex ;
        ptg2 : TGIS_Renderer3DVertex ;
        i    : Integer       ;
      begin
       if not vectorEdges then exit ;

        for i := 0 to _num - 2 do begin
          ptg1.P := pardata[i  ].P ;
          ptg1.N := pardata[i  ].N ;
          ptg1.Color := ou_color ;
          ptg2.P := pardata[i+1].P ;
          ptg2.N := pardata[i+1].N ;
          ptg2.Color := ou_color ;
          addToWPLBuf(ptg1, ptg2) ;
        end;
        ptg1.P := pardata[_num - 1].P ;
        ptg1.N := pardata[_num - 1].N ;
        ptg1.Color := ou_color ;
        ptg2.P := pardata[0].P ;
        ptg2.N := pardata[0].N ;
        ptg2.Color := ou_color ;
        addToWPLBuf(ptg1, ptg2) ;
      end;

    // Calculate normal vectors to triangle strip
    // _k position in vertex buffer
    // _pardata vertex buffer
      procedure calcNormals(
        const _k       : Integer       ;
        var   _pardata : T_arTextureData
      ) ;
      var
        nn, nn2, nn3   : TVector3f ;
        p0, p1, p2, p3 : TVector3f ;
        i, j, kk, k    : Integer      ;

          function getP(const _ii : Integer;
                        const _pardata : T_arTextureData ) : TVector3f ;
          var
            pp: TVector3f;
          begin
            pp.X := _pardata[_ii].P.X ;
            pp.Y := _pardata[_ii].P.Y ;
            pp.Z := _pardata[_ii].P.Z ;
            Result := pp ;
          end;

      begin
        i  := 0    ;
        k  := _k   ;
        kk := k -3 ;
        while True do begin
          if i>=kk then
            break
          else begin
            p0 := getP(i,_pardata) ;
            if i = 0 then
              j := k-4
            else
              j := i -2 ;
            p1 := getP(j  ,_pardata) ;
            p2 := getP(i+1,_pardata) ;
            p3 := getP(i+2,_pardata) ;

            if srcShpType = TGIS_ShapeType.Polygon then begin
              nn2 := triangleNormal(p0, p2, p1) ;
              nn3 := triangleNormal(p0, p3, p2) ;
            end
            else begin
              nn2 := triangleNormal(p0, p1, p2) ;
              nn3 := triangleNormal(p0, p2, p3) ;
            end;

            vectorAdd(nn,nn2,nn3) ;
            nn.X := nn.X/2.0 ;
            nn.Y := nn.Y/2.0 ;
            nn.Z := nn.Z/2.0 ;
            if not bLightVector then
              nn := reverseLight( nn ) ;
            _pardata[i].N   := nn ;
            _pardata[i+1].N := nn ;
          end;
          i := i +2 ;
        end;
        _pardata[i].N   := _pardata[0].N ;
        _pardata[i+1].N := _pardata[1].N ;
      end;

      // Normal vector for TGIS_Renderer3DVertex triangle
      procedure normVect(
        var   _ver1 : TGIS_Renderer3DVertex ;
        var   _ver2 : TGIS_Renderer3DVertex ;
        var   _ver3 : TGIS_Renderer3DVertex ;
        const _wind : Boolean
      ) ;
      var
        v0, v1, v2    : TVector3f  ;
      begin
        v0.X := _ver1.P.X ;
        v0.Y := _ver1.P.Y ;
        v0.Z := _ver1.P.Z ;
        v1.X := _ver2.P.X ;
        v1.Y := _ver2.P.Y ;
        v1.Z := _ver2.P.Z ;
        v2.X := _ver3.P.X ;
        v2.Y := _ver3.P.Y ;
        v2.Z := _ver3.P.Z ;
        _ver1.N := triangleNormal(v0, v1, v2) ;
        _ver2.N := triangleNormal(v1, v2, v0) ;
        _ver3.N := triangleNormal(v2, v0, v1) ;
        if not _wind then begin
            _ver1.N.X := -_ver1.N.X ;
            _ver1.N.Y := -_ver1.N.Y ;
            _ver1.N.Z := -_ver1.N.Z ;
            _ver2.N.X := -_ver2.N.X ;
            _ver2.N.Y := -_ver2.N.Y ;
            _ver2.N.Z := -_ver2.N.Z ;
            _ver3.N.X := -_ver3.N.X ;
            _ver3.N.Y := -_ver3.N.Y ;
            _ver3.N.Z := -_ver3.N.Z ;
          end;
      end;

      // Load triangulation buffer
      procedure loadTriBuffer(
        const _part : Integer ;
        const _num  : Integer ;
        const _area : Double
      ) ;
      var
        i : Integer ;
      begin
        if _area < 0 then
          for i := 0 to _num do
            case triMode of
              0 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.X, pardata[i].P.Y, pardata[i].P.Z, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
              1 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.X, pardata[i].P.Z, pardata[i].P.Y, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
              2 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.Y, pardata[i].P.Z, pardata[i].P.X, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
            end
        else
          for i := _num downto 0 do
            case triMode of
              0 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.X, pardata[i].P.Y, pardata[i].P.Z, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
              1 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.X, pardata[i].P.Z, pardata[i].P.Y, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
              2 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.Y, pardata[i].P.Z, pardata[i].P.X, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
            end
      end;

      // Add part to a triangulation buffer
      procedure addTriBuffer(
        const _part : Integer ;
        const _num  : Integer ;
        const _revs : Boolean
      ) ;
      var
        i : Integer ;
      begin
        if _revs then
          for i := _num downto 0 do
            case triMode of
              0 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.X, pardata[i].P.Y, pardata[i].P.Z, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
              1 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.X, pardata[i].P.Z, pardata[i].P.Y, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
              2 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.Y, pardata[i].P.Z, pardata[i].P.X, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
            end
        else
          for i := 0 to _num do
            case triMode of
              0 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.X, pardata[i].P.Y, pardata[i].P.Z, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
              1 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.X, pardata[i].P.Z, pardata[i].P.Y, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
              2 : TGIS_Tessellation(oPolyRoof).AddVertexTuv(
                   _part, -pardata[i].P.Y, pardata[i].P.Z, pardata[i].P.X, 0 ,
                   pardata[i].N.X, pardata[i].N.Y, pardata[i].N.Z ,
                   pardata[i].Tu, pardata[i].Tv ) ;
            end
      end;

      // TGIS_Renderer3DVertex to TGIS_Renderer3DVertex converter
      procedure bufTtoBufC1(
        const _ptg1 : TGIS_Renderer3DVertex ;
        const _ptg2 : TGIS_Renderer3DVertex
      ) ;
      var
        ptg1 : TGIS_Renderer3DVertex ;
        ptg2 : TGIS_Renderer3DVertex ;
      begin
        ptg1.P  := _ptg1.P ;
        ptg1.N  := _ptg1.N ;
        ptg1.Color := _ptg1.Color ;
        ptg2.P  := _ptg2.P ;
        ptg2.N  := _ptg2.N ;
        ptg2.Color := _ptg2.Color ;
        addToWPLBuf(ptg1, ptg2) ;
      end;

      // TGIS_Renderer3DVertex to TGIS_Renderer3DVertex converter
      procedure bufTtoBufC(
        const _ptg1 : TGIS_Renderer3DVertex ;
        const _ptg2 : TGIS_Renderer3DVertex ;
        const _ptg3 : TGIS_Renderer3DVertex
      ) ;
      var
        ptg1 : TGIS_Renderer3DVertex ;
        ptg2 : TGIS_Renderer3DVertex ;
        ptg3 : TGIS_Renderer3DVertex ;
      begin
        ptg1.P  := _ptg1.P ;
        ptg1.N  := _ptg1.N ;
        ptg1.Color := _ptg1.Color ;
        ptg2.P  := _ptg2.P ;
        ptg2.N  := _ptg2.N ;
        ptg2.Color := _ptg2.Color ;
        ptg3.P  := _ptg3.P ;
        ptg3.N  := _ptg3.N ;
        ptg3.Color := _ptg3.Color ;
        addToMPatchBufC(ptg1, ptg2, ptg3) ;
      end;

      // Triangulation
      procedure doTriangulation( _winding : Boolean ) ;
      var
        ptg1   : TGIS_Renderer3DVertex   ;
        ptg2   : TGIS_Renderer3DVertex   ;
        ptg3   : TGIS_Renderer3DVertex   ;
        num_tri, i : Integer ;
        s1, s2, s3 : TGIS_SingleVector ;
      begin
        TGIS_Tessellation(oPolyRoof).Execute ;
        num_tri := TGIS_Tessellation(oPolyRoof).TrianglesCount ;
        if num_tri = 0 then  exit ;

        for i := 0 to num_tri-1 do begin
          case triMode of
            0 : TGIS_Tessellation(oPolyRoof).GetTriangle(i,
                  ptg1.P.X, ptg1.P.Y, ptg1.P.Z,
                  ptg2.P.X, ptg2.P.Y, ptg2.P.Z,
                  ptg3.P.X, ptg3.P.Y, ptg3.P.Z) ;
            1 : TGIS_Tessellation(oPolyRoof).GetTriangle(i,
                  ptg1.P.X, ptg1.P.Z, ptg1.P.Y,
                  ptg2.P.X, ptg2.P.Z, ptg2.P.Y,
                  ptg3.P.X, ptg3.P.Z, ptg3.P.Y) ;
            2 : TGIS_Tessellation(oPolyRoof).GetTriangle(i,
                  ptg1.P.Y, ptg1.P.Z, ptg1.P.X,
                  ptg2.P.Y, ptg2.P.Z, ptg2.P.X,
                  ptg3.P.Y, ptg3.P.Z, ptg3.P.X) ;
          end;
          if triMode < 2 then begin
            ptg1.P.X := -ptg1.P.X ;
            ptg2.P.X := -ptg2.P.X ;
            ptg3.P.X := -ptg3.P.X ;
          end
          else begin
            ptg1.P.Y := -ptg1.P.Y ;
            ptg2.P.Y := -ptg2.P.Y ;
            ptg3.P.Y := -ptg3.P.Y ;
          end;
          ptg1.Color := ar_color ;
          ptg2.Color := ar_color ;
          ptg3.Color := ar_color ;

          TGIS_Tessellation(oPolyRoof).GetTriangleTuv(
            i, s1, s2, s3 ,
            ptg1.Tu, ptg1.Tv, ptg2.Tu, ptg2.Tv, ptg3.Tu, ptg3.Tv
          ) ;

          ptg1.N.X := s1.X ;
          ptg1.N.Y := s1.Y ;
          ptg1.N.Z := s1.Z ;
          ptg2.N.X := s2.X ;
          ptg2.N.Y := s2.Y ;
          ptg2.N.Z := s2.Z ;
          ptg3.N.X := s3.X ;
          ptg3.N.Y := s3.Y ;
          ptg3.N.Z := s3.Z ;

          if (ptg1.N.X = 0) and (ptg1.N.Y = 0) then begin
            if bLightVector then
              normVect(ptg1, ptg2, ptg3, _winding )
            else
              normVect(ptg1, ptg2, ptg3, not _winding ) ;
          end;

          if isgdbtex then
            addToMPatchBufT(ptg1, ptg2, ptg3, subsetTexMP)
          else
            bufTtoBufC(ptg1, ptg2, ptg3) ;
        end;
      end;

      // Strip
      procedure saveStrip( const _tri : Integer ) ;
      var
        i, j ,k : Integer       ;
        ptg1 : TGIS_Renderer3DVertex ;
        ptg2 : TGIS_Renderer3DVertex ;
        ptg3 : TGIS_Renderer3DVertex ;
      begin
        if _tri < 1 then exit;
        if not isnormal then
          calcNormals(_tri+2, pardata) ;
        for i := 0 to _tri - 1 do begin
          ptg1 := pardata[i  ] ;
          ptg2 := pardata[i+1] ;
          ptg3 := pardata[i+2] ;
          if isgdbtex then
            addToMPatchBufT(ptg1, ptg2, ptg3, subsetTexMP)
          else
            bufTtoBufC(ptg1, ptg2, ptg3) ;
        end;

        k := TruncS((_tri+2)/2) - 1 ;
        if vectorEdges then
        for i := 0 to k do begin
          j := i * 2 ;
          ptg1 := pardata[j  ] ;
          ptg2 := pardata[j+1] ;
          ptg1.Color := ou_color ;
          ptg2.Color := ou_color ;
          bufTtoBufC1(ptg1, ptg2) ;
        end;
      end;

      // Fan
      procedure saveFan( const _tri : Integer ) ;
      var
        i, j : Integer ;
        ptg1 : TGIS_Renderer3DVertex ;
        ptg2 : TGIS_Renderer3DVertex ;
        ptg3 : TGIS_Renderer3DVertex ;
      begin
        if _tri < 1 then exit;
        j := 1 ;
        ptg1 := pardata[0  ] ;
        for i := 0 to _tri - 1 do begin
          ptg2 := pardata[j] ;
          ptg3 := pardata[j+1] ;
          if (ptg1.N.X = 0) and (ptg1.N.Y = 0) then begin
            ptg1.N := triangleNormal(ptg1.P, ptg2.P, ptg3.P) ;
            ptg2.N := triangleNormal(ptg2.P, ptg3.P, ptg1.P) ;
            ptg3.N := triangleNormal(ptg3.P, ptg1.P, ptg2.P) ;
            if not bLightVector then begin
              ptg1.N := reverseLight( ptg1.N ) ;
              ptg2.N := reverseLight( ptg2.N ) ;
              ptg3.N := reverseLight( ptg3.N ) ;
            end;
          end;
          if isgdbtex then
            addToMPatchBufT(ptg1, ptg2, ptg3, subsetTexMP)
          else
            bufTtoBufC(ptg1, ptg2, ptg3) ;
          inc( j ) ;

          if vectorEdges then begin
            ptg1.Color := ou_color ;
            ptg2.Color := ou_color ;
            ptg3.Color := ou_color ;
            bufTtoBufC1(ptg1, ptg2) ;
            bufTtoBufC1(ptg2, ptg3) ;
            bufTtoBufC1(ptg3, ptg1) ;
          end;
        end;
      end;

      // Outer Ring
      procedure saveORing( const _num : Integer ) ;
      var
        k   : Integer ;
        prt : Integer ;
      begin
        iRingNum := 0 ;
        if _num < 3 then exit;
        if closedRing(_num) then
          k := _num -2
        else
          k := _num -1 ;

        TGIS_Tessellation(oPolyRoof).Reset ;
        loadTriBuffer( 0, k, area );
        drawEdges( _num ) ;

        prt := part_no +1 ;
        if ( prt < num_parts ) then
        if ( _shp.GetPartType( prt ) = TGIS_PartType.InnerRing ) then begin
          if area < 0 then
            wasReversed := False
          else
            wasReversed := True ;
          exit ;
        end;
        doTriangulation( area < 0 ) ;
      end;

      // Inner Ring
      procedure saveIRing( const _num : Integer ) ;
      var
        k   : Integer ;
        prt : Integer ;
      begin
        if _num < 3 then exit;
        inc( iRingNum ) ;
        if closedRing(_num) then
          k := _num -2
        else
          k := _num -1 ;

        prt := part_no +1 ;
        if ( prt < num_parts ) then
          if ( _shp.GetPartType( part_no +1 ) = TGIS_PartType.InnerRing ) then begin
            addTriBuffer( iRingNum, k, wasReversed ) ;
            drawEdges( _num ) ;
            exit;
          end ;

        addTriBuffer( iRingNum, k, wasReversed ) ;
        doTriangulation( not wasReversed ) ;
        drawEdges( _num ) ;
      end;

      // First Ring
      procedure saveFRing( const _num : Integer ) ;
      var
        k   : Integer ;
        prt : Integer ;
      begin
        if _num < 3 then exit;
        if closedRing(_num) then
          k := _num -2
        else
          k := _num -1 ;
        TGIS_Tessellation(oPolyRoof).Reset ;
        loadTriBuffer( 0, k, area );
        drawEdges( _num ) ;

        prt := part_no +1 ;
        if ( prt < num_parts ) then
        if ( _shp.GetPartType( prt ) = TGIS_PartType.Ring ) then begin
          if area < 0 then
            wasReversed := False
          else
            wasReversed := True ;
          exit ;
        end;
        doTriangulation( area < 0 ) ;
      end;

      // Ring
      procedure saveRing( const _num : Integer ) ;
      var
        k : Integer ;
      begin
       if _num < 3 then exit;
       if closedRing(_num) then
         k := _num - 2
       else
         k := _num - 1 ;

       if part_no > 0 then
       if ( _shp.GetPartType( part_no -1 ) = TGIS_PartType.FirstRing ) then begin
         addTriBuffer( 1, k, wasReversed ) ;
         doTriangulation( not wasReversed ) ;
         drawEdges( _num ) ;
         exit;
       end;

       TGIS_Tessellation(oPolyRoof).Reset ;
       loadTriBuffer( 0, k, area );
       doTriangulation( area < 0 ) ;
       drawEdges( _num ) ;
      end;

      // Triangle
      procedure saveTriangle( const _num : Integer ) ;
      var
       i, j, k : Integer    ;
       ptg1 : TGIS_Renderer3DVertex ;
       ptg2 : TGIS_Renderer3DVertex ;
       ptg3 : TGIS_Renderer3DVertex ;
      begin
        if _num < 3 then exit;
        k := TruncS( _num / 3 ) - 1 ;
        for i := 0 to k do begin
          j := i * 3 ;
          ptg1 := pardata[j    ] ;
          ptg2 := pardata[j + 1] ;
          ptg3 := pardata[j + 2] ;

          if (ptg1.N.X = 0) and (ptg1.N.Y = 0) then begin
            if bLightVector then
              normVect( ptg1, ptg2, ptg3, False )
            else
              normVect( ptg1, ptg2, ptg3, True ) ;
          end;

          if isgdbtex then
            addToMPatchBufT(ptg1, ptg2, ptg3, subsetTexMP)
          else
            bufTtoBufC(ptg1, ptg2, ptg3) ;

          if vectorEdges then begin
            ptg1.Color := ou_color ;
            ptg2.Color := ou_color ;
            ptg3.Color := ou_color ;
            bufTtoBufC1(ptg1, ptg2) ;
            bufTtoBufC1(ptg2, ptg3) ;
            bufTtoBufC1(ptg3, ptg1) ;
          end;
        end;
      end;

      // mode types 0: arXY, 1: -arXZ, 2: arYZ
      procedure setTriMode ;
      begin
        if (ptype = TGIS_PartType.TriangleStrip ) or
           (ptype = TGIS_PartType.TriangleFan   ) or
           (ptype = TGIS_PartType.Triangle      ) then exit ;

        area := arXY ;
        triMode := 0 ;

        if Abs(area) < 0.5 * Abs(arXZ) then begin
          area := -arXZ ;
          triMode := 1 ;
        end;
        if Abs(area) < 0.5 * Abs(arYZ) then begin
          area := arYZ ;
          triMode := 2 ;
        end;
      end;

      // Set subset value
      function setSubset : Integer ;
      var
        i : Integer ;
      begin
        if mpatchSubsetCounter = -1 then begin
          inc( mpatchSubsetCounter ) ;
          SetLength( mpatchTileInfo, meshMPatchTilesCounterT +1 ) ;
          SetLength( mpatchTileInfo[meshMPatchTilesCounterT],
                                   mpatchSubsetCounter +1 ) ;
          mpatchTileInfo[meshMPatchTilesCounterT]
                        [mpatchSubsetCounter].Subset := mpatchSubsetCounter ;
          mpatchTileInfo[meshMPatchTilesCounterT]
                          [mpatchSubsetCounter].TexDX := pdataTEX_vDX ;
          mpatchTileInfo[meshMPatchTilesCounterT]
                        [mpatchSubsetCounter].Transp := a ;
          Result := mpatchSubsetCounter ;
          exit;
        end
        else begin
          for i := 0 to mpatchSubsetCounter do begin
            if mpatchTileInfo[meshMPatchTilesCounterT][i].TexDX
               = pdataTEX_vDX then begin
               Result := i ;
               exit;
            end;
          end;
          inc( mpatchSubsetCounter ) ;
          SetLength( mpatchTileInfo[meshMPatchTilesCounterT],
                                    mpatchSubsetCounter +1 ) ;
          mpatchTileInfo[meshMPatchTilesCounterT]
                        [mpatchSubsetCounter].Subset := mpatchSubsetCounter ;
          mpatchTileInfo[meshMPatchTilesCounterT]
                          [mpatchSubsetCounter].TexDX := pdataTEX_vDX ;
          mpatchTileInfo[meshMPatchTilesCounterT]
                        [mpatchSubsetCounter].Transp := a ;
          Result := mpatchSubsetCounter ;
        end;
      end;

      // GDB shape part analyse
      procedure analyseGDB( const _part : Integer ) ;
      var
        material : TGIS_Material ;
        matid    : Integer ;
        bmp      : TGIS_Bitmap ;
        j, l, l1 : Integer ;
        chcks    : Integer ;
        pix      : TGIS_Pixels ;
        cl32     : Int32 ;
        cl16     : UInt16 ;
        {$IFDEF CLR}
          cl8    : Byte ;
        {$ELSE}
          cl8    : UInt8 ;
        {$ENDIF}

          function checksum : Integer ;
          var
            k, k1 : Integer ;
          begin
            if material.Size > 2000 then
              k1 := 2000
            else
              k1 := material.Size ;
            Result := 0 ;
            for k := 0 to k1-1 do
              Result := Result + material.Buffer[k] ;
          end;

          procedure setbmp ;
          var
            l, l1 : Integer ;
          begin
            for l := 0 to material.Height -1 do begin
              for l1 := 0 to material.Width -1 do begin
                a := 255 ;
                case material.Bpp of
                  1 : begin
                        j := l*material.Width + l1 ;
                        r := material.Buffer[ j ] ;
                        cl8 := r ;
                        cl32 := RoundS(((cl8 shr $A) and $1F) * 8.225806);
                        if cl32 < 0 then r := 0
                        else if cl32 > 255 then r := 255
                        else r := cl32 ;
                        cl32 := RoundS(((cl8 shr $5) and $1F) * 8.225806);
                        if cl32 < 0 then g := 0
                        else if cl32 > 255 then g := 255
                        else g := cl32 ;
                        cl32 := RoundS((cl8 and $1F) * 8.225806);
                        if cl32 < 0 then b := 0
                        else if cl32 > 255 then b := 255
                        else b := cl32 ;
                      end ;
                  2 : begin
                        j := 2*(l*material.Width + l1) ;
                        r := material.Buffer[ j ] ;
                        inc( j ) ;
                        g := material.Buffer[ j ] ;
                        cl16 := 256 * r + g ;
                        cl32 := RoundS(((cl16 shr $A) and $1F) * 8.225806);
                        if cl32 < 0 then r := 0
                        else if cl32 > 255 then cl32 := 255
                        else r := cl32 ;
                        g := RoundS(((cl16 shr $5) and $1F) * 8.225806);
                        if cl32 < 0 then g := 0
                        else if cl32 > 255 then g := 255
                        else g := cl32 ;
                        b := RoundS((cl16 and $1F) * 8.225806);
                        if cl32 < 0 then b := 0
                        else if cl32 > 255 then b := 255
                        else b := cl32 ;
                      end ;
                  3 : begin
                        j := 3*(l*material.Width + l1) ;
                        r := material.Buffer[ j ] ;
                        inc( j ) ;
                        g := material.Buffer[ j ] ;
                        inc( j ) ;
                        b := material.Buffer[ j ] ;
                      end ;
                  4 : begin
                        j := 4*(l*material.Width + l1) ;
                        a := material.Buffer[ j ] ;
                        inc( j ) ;
                        r := material.Buffer[ j ] ;
                        inc( j ) ;
                        g := material.Buffer[ j ] ;
                        inc( j ) ;
                        b := material.Buffer[ j ] ;
                      end ;
                end;
                cl := TGIS_Color.FromARGB(a,r,g,b);
                pix[l*material.Width + l1] := Integer(cl.ARGB) ;
              end;
            end;
          end;

      begin
        if (mpatchSubsetCounter =-1) and (meshMPatchTilesCounterT >0) then begin
          pdataTEX_vDX := lastTexDX ;
          a := lasta ;
          setSubset ;
        end;

        istexture  := False ;
        ismaterial := False ;
        isedge     := False ;
        isgdbtex   := False ;
        if assigned( pdataTEX_vDX ) then
          ReleaseInterface( pdataTEX_vDX ) ;
        pdataTEX_vDX := nil ;
        a := 255 ;

        prtdsc := TGIS_ShapeMultiPatch(_shp).PartDescriptors.
                    PartDescriptor[_part];
        matid := prtdsc.Material ;
        if (matid < 0) or (matid >= TGIS_ShapeMultiPatch(_shp).Materials.NumMaterials)
          then exit ;

        if _shp.Layer.IgnoreShapeParams then exit ;

        material := TGIS_ShapeMultiPatch(_shp).Materials.Material[matid] ;

        if TGIS_ShapeMultiPatch(_shp).HasTextures and
           material.HasTextureMap and
           (TGIS_ShapeMultiPatch(_shp).Textures.GetPartSize(_part)>0) then begin
            if high(mtrlinfo) = -1 then begin
              SetLength(mtrlinfo, 1);
              mtrlinfo[high(mtrlinfo)].MtrlId := matid ;
              mtrlinfo[high(mtrlinfo)].CheckSum := checksum ;
            end
            else begin
              for l := 0 to high(mtrlinfo) do
                 if mtrlinfo[l].MtrlId = matid then begin
                   pdataTEX_vDX := mtrlinfo[l].TexDX ;
                   break;
                 end;
              if l > high(mtrlinfo) then begin
                chcks := checksum ;
                SetLength( mtrlinfo, high(mtrlinfo) + 2 ) ;
                mtrlinfo[high(mtrlinfo)].MtrlId := matid ;
                mtrlinfo[high(mtrlinfo)].CheckSum := chcks ;

                for l1 := 0 to high(mtrlinfo) -1 do
                  if mtrlinfo[l1].CheckSum = chcks then begin
                    pdataTEX_vDX := mtrlinfo[l1].TexDX ;
                    mtrlinfo[high(mtrlinfo)].TexDX := pdataTEX_vDX ;
                    break;
                  end;
              end;
            end;
            td := TGIS_ShapeMultiPatch(_shp).Textures.TextureDimension ;
            isgdbtex := True ;
        end;

        // transparency
        if material.HasTransparency then
          a := TruncS( material.Transparency/100*255) ;
        if material.Bpp = 4 then
          a := 256 ;       //poprawic ?
        if currentVL.Transparency <> 100 then
          a := TruncS( currentVL.Transparency/100*255) ;
        // area color
        if material.HasColor then begin
          cl := material.Color ;
          r := cl.R ;
          g := cl.G ;
          b := cl.B ;
          ar_color := color2ARGB(a,r,g,b) ;
        end
        else
          ar_color := color2ARGB(a,255,255,255) ;
        // edges color
        if material.HasEdgeColor then begin
          isedge := True ;
          cl := material.EdgeColor ;
          r := cl.R ;
          g := cl.G ;
          b := cl.B ;
          ou_color := color2ARGB(a,r,g,b) ;
        end;

        if _shp.IsSelected then begin
          a := Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)) ;
          r := oGIS.SelectionGisColor.R ;
          g := oGIS.SelectionGisColor.G ;
          b := oGIS.SelectionGisColor.B ;
          ar_color := color2ARGB(a, r ,g, b) ;
          ou_color := ar_color ;
        end;

        // bitmap texture
        if isgdbtex then begin
          if (not assigned( pdataTEX_vDX )) then begin
            case material.CompressionType of
              TGIS_CompressionType.JPEG :
                        pdataTEX_vDX := create_texture1(pD3DDevice, material ) ;
              TGIS_CompressionType.JPEGPlus :
                        pdataTEX_vDX := create_texture1(pD3DDevice, material ) ;
              TGIS_CompressionType.None : begin
                bmp := TGIS_Bitmap.Create( material.Width, material.Height ) ;
                bmp.LockPixels( pix, True, TGIS_BitmapFormat.ABGR,
                                TGIS_BitmapLinesOrder.Down
                              ) ;
                setbmp ;
                bmp.UnlockPixels ;
                pdataTEX_vDX := create_texture( pD3DDevice, bmp ) ;
                FreeObject( bmp ) ;
              end;
              TGIS_CompressionType.ARGB :
                        pdataTEX_vDX := create_texture2(pD3DDevice, material ) ;
              else begin
                pdataTEX_vDX := nil
              end;
            end;  // case

            if assigned( pdataTEX_vDX ) then
              mtrlinfo[high(mtrlinfo)].TexDX := pdataTEX_vDX ;

          end; // not assigned( pdataTEX_v )

          if assigned( pdataTEX_vDX ) then begin
            subsetTexMP := setSubset ;
            lastTexDX := mpatchTileInfo[meshMPatchTilesCounterT]
                         [mpatchSubsetCounter].TexDX ;
            lasta   := mpatchTileInfo[meshMPatchTilesCounterT]
                         [mpatchSubsetCounter].Transp
          end
          else
            isgdbtex := False ;
        end; // isgdbtex

      end;

      function arcolorToDxcolor( const _col : DWORD )
        : TGIS_Color ;
      var
        a, r, g, b : Byte ;
      begin
        a := ( (_col shr 24) and $000000FF) ;
        r := ( (_col shr 16) and $000000FF) ;
        g := ( (_col shr 08) and $000000FF) ;
        b := ( (_col shr  0) and $000000FF) ;
        Result := TGIS_Color.FromARGB(a, r, g, b ) ;
      end;

  // main function
  begin
    Result := True ;

    triMode := 0 ;
    pnt_counter := 0 ;
    num_normals := 0 ;
    isnormal := False ;
    isedge   := False ;
    isgdb    := False ;
    isgdbtex := False ;

    setScaleGlobals ;

    if falseMultiPatch then
      isgdb  := False
    else begin
      if TGIS_ShapeMultiPatch(_shp).HasPartDescriptors then
        isgdb := True
      else begin
        isgdb := False ;
        isgdbtex := False ;
      end;
    end;

    lparams   := _shp.Params ;
    lp_falseZ := lparams.FalseZ ;
    lp_falseM := lparams.FalseM ;

    if _shp.IsSelected then
        cl := oGIS.SelectionGisColor
      else
        cl := _shp.Params.Area.Color ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    if not isgdb or _shp.Layer.IgnoreShapeParams then begin
      if _shp.IsSelected then
        cl := oGIS.SelectionGisColor
      else
        cl := _shp.Params.Area.Color ;

      if _shp.IsSelected then
        a := Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0))
      else begin
        if currentVL.Transparency <> 100 then
          a := TruncS( currentVL.Transparency / 100 * cl.A )
        else
          a := cl.A ;
      end ;

      r := cl.R ;
      g := cl.G ;
      b := cl.B ;
      ar_color := color2ARGB(a, r ,g, b) ;

      cl := _shp.Params.Area.OutlineColor ;
      r := cl.R ;
      g := cl.G ;
      b := cl.B ;
      ou_color := color2ARGB(a, r, g, b) ;
    end;

    if ar_color = 0 then
      ar_color := color2ARGB(cl.A, cl.R, cl.G, cl.B) ;

    if ou_color = 0 then
      ou_color := color2ARGB(GetEdgesColor.A,GetEdgesColor.R,GetEdgesColor.G,GetEdgesColor.B) ;

    if isgdb then begin
      num_parts := TGIS_ShapeMultiPatch(_shp).PartDescriptors.NumPartDescriptors;
      if TGIS_ShapeMultiPatch(_shp).HasNormals then begin
        isnormal := True ;
        num_normals := TGIS_ShapeMultiPatch(_shp).Normals.NumNormals ;
      end
    end
    else begin
      num_parts := _shp.GetNumParts ;
      if not falseMultiPatch then begin
        num_normals := TGIS_ShapeMultiPatch(_shp).Normals.NumNormals ;
        if (num_normals > 0) and (num_normals = num_parts) then
          isnormal := True ;
      end;
    end;

    for part_no := 0 to num_parts -1 do begin
      if isgdb then analyseGDB( part_no ) ;
      if isgdbtex then
        fillBufferInfoEntry(10, _shp.Uid, part_no, arcolorToDxcolor(ar_color))
      else
        fillBufferInfoEntry(9, _shp.Uid, part_no, arcolorToDxcolor(ar_color));
      ptype := _shp.GetPartType( part_no ) ;
      num_points := _shp.GetPartSize(part_no) ;
      SetLength( pardata, num_points ) ;
      area := 0 ;
      arXY := 0 ;
      arXZ := 0 ;
      arYZ := 0 ;
      for point_no := 0 to num_points - 1 do begin
        ptg := _shp.GetPoint3D( part_no, point_no ) ;
        inc( pnt_counter ) ;
        if (ptg.M < ZMMIN) or (ptg.M > ZMMAX) then ptg.M := 0 ;
        if (ptg.Z < ZMMIN) or (ptg.Z > ZMMAX) then ptg.Z := 0 ;
        if ( ptype <> TGIS_PartType.TriangleStrip ) and
           ( ptype <> TGIS_PartType.TriangleFan   ) and
           ( ptype <> TGIS_PartType.Triangle      ) then begin
          if point_no = 0 then
            first_pnt := ptg
          else begin
            arXY := arXY + ( ptg0.X*ptg.Y - ptg.X*ptg0.Y ) ;
            arXZ := arXZ + ( ptg0.X*ptg.Z - ptg.X*ptg0.Z ) ;
            arYZ := arYZ + ( ptg0.Y*ptg.Z - ptg.Y*ptg0.Z ) ;
          end;
          ptg0 := ptg ;
        end;
        pardata[point_no] := getScaledPoint3D( ptg ) ;

        if isgdb then begin
          if currentVL.Transparency <> 100 then
            pardata[point_no].Color := ar_color
          else if TGIS_ShapeMultiPatch(_shp).HasVertexColors then
            pardata[point_no].Color := TGIS_ShapeMultiPatch(_shp).VertexColors.GetVertexColor( part_no, point_no )
          else
            pardata[point_no].Color := ar_color ;
        end
        else
          pardata[point_no].Color := ar_color ;

        if isgdb then begin
          if isgdbtex then begin
            pardata[point_no].Tu :=
              TGIS_ShapeMultiPatch(_shp).Textures.GetTextureCoord(
                                                   part_no, td*point_no) ;
            pardata[point_no].Tv :=
              TGIS_ShapeMultiPatch(_shp).Textures.GetTextureCoord(
                                                  part_no, td*point_no+1) ;
          end;
          if num_normals > pnt_counter then begin
            pardata[point_no].N.X := TGIS_ShapeMultiPatch(_shp).
                                     Normals.Normal[pnt_counter].X ;
            pardata[point_no].N.Y := TGIS_ShapeMultiPatch(_shp).
                                     Normals.Normal[pnt_counter].Y ;
            pardata[point_no].N.Z := TGIS_ShapeMultiPatch(_shp).
                                     Normals.Normal[pnt_counter].Z ;
            if not bLightVector then begin
              pardata[point_no].N.X := -pardata[point_no].N.X ;
              pardata[point_no].N.Y := -pardata[point_no].N.Y ;
              pardata[point_no].N.Z := -pardata[point_no].N.Z ;
            end ;
            {$IFDEF CLR}
              vec3 := TD3DVector( pardata[point_no].N ) ;
              Vec3Normalize( vec3, vec3 ) ;
              pardata[point_no].N := vec3 ;
            {$ELSE}
              Vec3Normalize( TD3DVector( pardata[point_no].N ),
                             TD3DVector( pardata[point_no].N )
                           ) ;
            {$ENDIF}
          end ;
        end ;

        if (not falseMultiPatch) and isnormal then begin
          pardata[point_no].N.X := TGIS_ShapeMultiPatch(_shp).
                                   Normals.Normal[part_no].X ;
          pardata[point_no].N.Y := TGIS_ShapeMultiPatch(_shp).
                                   Normals.Normal[part_no].Y ;
          pardata[point_no].N.Z := TGIS_ShapeMultiPatch(_shp).
                                   Normals.Normal[part_no].Z ;
          if not bLightVector then begin
            pardata[point_no].N.X := -pardata[point_no].N.X ;
            pardata[point_no].N.Y := -pardata[point_no].N.Y ;
            pardata[point_no].N.Z := -pardata[point_no].N.Z ;
          end ;
          {$IFDEF CLR}
            vec3 := TD3DVector( pardata[point_no].N ) ;
            Vec3Normalize( vec3, vec3 ) ;
            pardata[point_no].N := vec3 ;
          {$ELSE}
            Vec3Normalize( TD3DVector( pardata[point_no].N ),
                           TD3DVector( pardata[point_no].N )
                         ) ;
          {$ENDIF}
        end ;
      end ;

      if ( ptype <> TGIS_PartType.TriangleStrip ) and   // rings only
         ( ptype <> TGIS_PartType.TriangleFan   ) and
         ( ptype <> TGIS_PartType.Triangle      ) then begin

          if not closedRing( num_points ) then begin
            arXY := arXY + ( ptg.X*first_pnt.Y - first_pnt.X*ptg.Y ) ;
            arXZ := arXZ + ( ptg.X*first_pnt.Z - first_pnt.X*ptg.Z ) ;
            arYZ := arYZ + ( ptg.Y*first_pnt.Z - first_pnt.Y*ptg.Z ) ;
          end;

        setTriMode ;
      end;

      case ptype of
        TGIS_PartType.TriangleStrip: begin
                                       if closedStrip( num_points ) then
                                         saveStrip( num_points -2 )
                                       else
                                         saveStrip( num_points -3 )
                                     end;
        TGIS_PartType.TriangleFan  : saveFan  ( num_points -2 ) ;
        TGIS_PartType.OuterRing    : saveORing( num_points    ) ;
        TGIS_PartType.InnerRing    : saveIRing( num_points    ) ;
        TGIS_PartType.FirstRing    : saveFRing( num_points    ) ;
        TGIS_PartType.Ring         : saveRing ( num_points    ) ;
        TGIS_PartType.Triangle     : saveTriangle( num_points ) ;
      end;

    end;   // parts
    _shp.Unlock ;

    if labelMode then begin
      cptg := _shp.Centroid ;
      ptg := GisPoint3D( cptg.X, cptg.Y, _shp.PointsZMax ) ;
      _lbz := getScaledPoint3D( ptg ).P.Z ;
      _lbb := _lbz ;
    end
    else begin
      _lbz := 0 ;
      _lbb := 0 ;
    end ;

    SetLength( mtrlinfo, 0 ) ;
  end;

  procedure TGIS_Renderer3DDirectX9.markSelShape(
    const _layer : TGIS_Layer ;
    const _shpid : Integer    ;
    const _part  : Integer    ;
    const _color : TGIS_Color ;
    const _mode  : Boolean    ;
    const _update: Boolean
  ) ;
  var
    i, i0, bt : Integer    ;
    bufposi   : Integer    ;
    mark_color: TD3DColor  ;
    r,g,b,a   : Byte       ;

    procedure getMarkColor ;
    begin
      if _mode then begin
        r := _color.R ;
        g := _color.G ;
        b := _color.B ;
        {$IFDEF CLR}
//          mark_color := D3DCOLOR_ARGB(a, b, g, r) ;
          mark_color := D3DCOLOR_ARGB(Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)),
           b, g, r) ;
        {$ELSE}
//          mark_color := D3DCOLOR_ARGB(a, r, g, b) ;
          mark_color := D3DCOLOR_ARGB(Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)),
           r, g, b) ;
        {$ENDIF}
      end;
    end;

    function found( var   _j1   : Integer;
                    var   _j2   : Integer;
                    const _mesh : T_arMesh
                  ) : Boolean ;
    var
      j : Integer ;
      cl : TGIS_Color ;
      rec : TGIS_Renderer3DShapeSelectionInfo ;
    begin
      Result := False ;

      _j1 := -1 ;
      for j := 0 to arBufSelInfo[bufposi].ShpInfo.Count -1 do begin
        if _part <> -1 then begin
          if (arBufSelInfo[bufposi].ShpInfo[j].Uid = _shpid) and
             (arBufSelInfo[bufposi].ShpInfo[j].Part = _part) then begin
            _j1 := arBufSelInfo[bufposi].ShpInfo[j].Offset ;
            cl := arBufSelInfo[bufposi].ShpInfo[j].Color ;
            if (bt=9) or (bt=10) then
              a := cl.A ;
            r := cl.R ;
            g := cl.G ;
            b := cl.B ;
            {$IFDEF CLR}
              mark_color := D3DCOLOR_ARGB(a, b, g, r) ;
            {$ELSE}
              mark_color := D3DCOLOR_ARGB(a, r, g, b) ;
            {$ENDIF}

            rec := arBufSelInfo[bufposi].ShpInfo[j] ;
            if _mode then
              rec.Selected := True
            else
              rec.Selected := False ;

            arBufSelInfo[bufposi].ShpInfo[j] := rec ;
            break ;
          end;
        end
        else begin
          if  arBufSelInfo[bufposi].ShpInfo[j].Uid = _shpid then begin
            _j1 := arBufSelInfo[bufposi].ShpInfo[j].Offset ;
            cl := arBufSelInfo[bufposi].ShpInfo[j].Color ;
            if (bt=9) or (bt=10) then
              a := cl.A ;
            r := cl.R ;
            g := cl.G ;
            b := cl.B ;
            {$IFDEF CLR}
              mark_color := D3DCOLOR_ARGB(a, b, g, r) ;
            {$ELSE}
              mark_color := D3DCOLOR_ARGB(a, r, g, b) ;
            {$ENDIF};
            rec := arBufSelInfo[bufposi].ShpInfo[j] ;
            if _mode then
              rec.Selected := True
            else
              rec.Selected := False ;
            arBufSelInfo[bufposi].ShpInfo[j] := rec ;
            break ;
          end;
        end;
      end;
      if _j1 = -1 then exit; // not found
      while True do begin
        if (bt=9) and (_mode = False ) then begin
          cl := arBufSelInfo[bufposi].ShpInfo[j].Color ;
             TMesh(_mesh[i0]).cvertices[j].Color :=
              D3DCOLOR_ARGB(cl.A,cl.R,cl.G,cl.B );
        end;
        if _part <> -1 then begin
          if (arBufSelInfo[bufposi].ShpInfo[j].Uid <> _shpid) or
             (arBufSelInfo[bufposi].ShpInfo[j].Part <> _part) then
            break ;
        end
        else begin
          if  arBufSelInfo[bufposi].ShpInfo[j].Uid <> _shpid then
            break ;
        end;

        if j < arBufSelInfo[bufposi].ShpInfo.Count -1 then
          inc( j )
        else
          break ;
      end;
      _j2 := arBufSelInfo[bufposi].ShpInfo[j].Offset ;

      if assigned(_mesh) then
      if arBufSelInfo[bufposi].ShpInfo.Count-1 = j then
        _j2 := TMesh(_mesh[i0]).GetNumVertices ;
      if _mode then
        getMarkColor ;

      Result := True ;
    end;

    function found1( var   _j1   : Integer;
                     var   _j2   : Integer;
                     const _mesh : T_arMesh
                   ) : Boolean ;
    var
      i, j , k: Integer ;
      cl, curcl : TGIS_Color ;
      rec : TGIS_Renderer3DShapeSelectionInfo ;
    begin
      Result := False ;
      k := 0 ;
      _j1 := -1 ;
      while True do begin
        for j := k to arBufSelInfo[bufposi].ShpInfo.Count -1 do begin
          if _part <> -1 then begin
            if (arBufSelInfo[bufposi].ShpInfo[j].Uid = _shpid) and
               (arBufSelInfo[bufposi].ShpInfo[j].Part = _part) then begin
              _j1 := arBufSelInfo[bufposi].ShpInfo[j].Offset ;
              cl := arBufSelInfo[bufposi].ShpInfo[j].Color ;
              curcl := cl ;
              if (bt=9) or (bt=10) then
                a := cl.A ;
              r := cl.R ;
              g := cl.G ;
              b := cl.B ;
              {$IFDEF CLR}
                mark_color := D3DCOLOR_ARGB(a, b, g, r) ;
              {$ELSE}
                mark_color := D3DCOLOR_ARGB(a, r, g, b) ;
              {$ENDIF}

              rec := arBufSelInfo[bufposi].ShpInfo[j] ;
              if _mode then
                rec.Selected := True
              else
                rec.Selected := False ;

              arBufSelInfo[bufposi].ShpInfo[j] := rec ;
              break ;
            end;
          end
          else begin
            if  arBufSelInfo[bufposi].ShpInfo[j].Uid = _shpid then begin
              _j1 := arBufSelInfo[bufposi].ShpInfo[j].Offset ;
              cl := arBufSelInfo[bufposi].ShpInfo[j].Color ;
              curcl := cl ;
              if (bt=9) or (bt=10) then
                a := cl.A ;
              r := cl.R ;
              g := cl.G ;
              b := cl.B ;
              {$IFDEF CLR}
                mark_color := D3DCOLOR_ARGB(a, b, g, r) ;
              {$ELSE}
                mark_color := D3DCOLOR_ARGB(a, r, g, b) ;
              {$ENDIF};
              rec := arBufSelInfo[bufposi].ShpInfo[j] ;
              if _mode then
                rec.Selected := True
              else
                rec.Selected := False ;
              arBufSelInfo[bufposi].ShpInfo[j] := rec ;
              break ;
            end;
          end;
        end;
        if _j1 = -1 then exit; // not found
        while True do begin
          if _part <> -1 then begin
            if (arBufSelInfo[bufposi].ShpInfo[j].Uid <> _shpid) or
               (arBufSelInfo[bufposi].ShpInfo[j].Part <> _part) then
              break ;
          end
          else begin
            if (arBufSelInfo[bufposi].ShpInfo[j].Uid <> _shpid)  then
              break ;
            if ( not _mode) and
               (arBufSelInfo[bufposi].ShpInfo[j].Color <> curcl) then
              break ;
          end;

          if j < arBufSelInfo[bufposi].ShpInfo.Count -1 then
            inc( j )
          else
            break ;
        end;
        _j2 := arBufSelInfo[bufposi].ShpInfo[j].Offset ;

        if assigned(_mesh) then
        if arBufSelInfo[bufposi].ShpInfo.Count-1 = j then
          _j2 := TMesh(_mesh[i0]).GetNumVertices ;

        case bt of
           9 : for i := _j1 to _j2 -1 do TMesh(_mesh[i0]).cvertices[i].Color := mark_color ;
          10 : for i := _j1 to _j2 -1 do TMesh(_mesh[i0]).tvertices[i].Color := mark_color ;
        end;

        _j1 := -1 ;
         k := j ;
         if (arBufSelInfo[bufposi].ShpInfo[j].Uid <> _shpid)  then
            break ;
         if arBufSelInfo[bufposi].ShpInfo.Count-1 = j then
           break;
      end;

      if _mode then
        getMarkColor ;

      Result := True ;
    end;

    procedure markLine( var _mesh    : TGIS_Renderer3DVectorTilesInfo ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, nil) then exit ;
        if j1 = arBufSelInfo[bufposi].ShpInfo[arBufSelInfo[bufposi].ShpInfo.Count-1].Offset then
          j2 := _mesh[i0].Count ;

        for j := j1 to j2 -1 do
          _mesh[i0].Buffer[j].Color := mark_color ;
      end;

    procedure markPoint( var _mesh    : TGIS_Renderer3DVectorTilesInfo ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, nil) then exit ;
        if j1 = arBufSelInfo[bufposi].ShpInfo[arBufSelInfo[bufposi].ShpInfo.Count-1].Offset then
          j2 := _mesh[i0].Count ;

        for j := j1 to j2 -1 do
          _mesh[i0].Buffer[j].Color := mark_color ;
      end;

      procedure markTriangleC( var _mesh    : T_arMesh ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, _mesh) then exit ;

        for j := j1 to j2 - 1 do
          TMesh(_mesh[i0]).cvertices[j].Color := mark_color ;
        TMesh(_mesh[i0]).UpdateBuffer ;
      end;

      procedure markMPatch ( var _mesh    : T_arMesh ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if _mode then begin
          if not found(j1, j2, _mesh) then exit ;
          case bt of
            9 : for j := j1 to j2 - 1 do TMesh(_mesh[i0]).cvertices[j].Color := mark_color ;
           10 : for j := j1 to j2 - 1 do TMesh(_mesh[i0]).tvertices[j].Color := mark_color ;
          end;
        end
        else
        if not found1(j1, j2, _mesh) then exit ;

        TMesh(_mesh[i0]).UpdateBuffer ;
      end;

      procedure markTriangleT( var _mesh    : T_arMesh  ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, _mesh) then exit ;

        for j := j1 to j2 - 1 do
          TMesh(_mesh[i0]).tvertices[j].Color := mark_color ;
        TMesh(_mesh[i0]).UpdateBuffer ;
      end;

      procedure markTriangleT1( var _mesh    : T_arMesh  ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, _mesh) then exit ;

        for j := j1 to j2 - 1 do
          TMesh(_mesh[i0]).tvertices[j].Color := mark_color ;
        TMesh(_mesh[i0]).UpdateBuffer ;
      end;

  // main body
  begin
    for i := 0 to bufSelInfoCounter -1 do begin    //for all buffers
      if arBufSelInfo[i].Layer = _layer then begin //if buffer contain _layer
        bt := arBufSelInfo[i].BufferType  ;
        i0 := arBufSelInfo[i].BufferIndex ;
        a := TruncS(_layer.Transparency / 100 * 255) ;
        bufposi := i ;
        case bt of
          1  : markTriangleC (meshVectTilesCW ) ;
          2  : markTriangleC (meshVectTilesCR ) ;
          3  : markTriangleT (meshVectTilesTW ) ;
          4  : markTriangleT (meshVectTilesTR ) ;
          6  : markTriangleT1(meshWallTiles   ) ;
          7  : markTriangleC (meshTinTilesC   ) ;
          8  : markTriangleT1(meshTinTilesT   ) ;
          9  : markMPatch    (meshMPatchTilesC) ;
          10 : markMPatch    (meshMPatchTilesT) ;
          11 : markLine      (meshLineTiles   ) ;
          12 : markPoint     (meshPointTiles  ) ;
        end; // case
      end;   // if arBufSelInfo
    end;     // for i
    if _update then
      Repaint ;
  end;

  procedure TGIS_Renderer3DDirectX9.addToMPatchBufT(
    const _ptg1 : TGIS_Renderer3DVertex ;
    const _ptg2 : TGIS_Renderer3DVertex ;
    const _ptg3 : TGIS_Renderer3DVertex ;
    const _subs : Integer
  ) ;
  var
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    {$IFDEF CLR}
      { TODO -cReview : check in CLR }
       //? SetLength( mpatchBufT.TriLst, mpatchBufT.NumPoints + 3 ) ;
    {$ENDIF}

    if length(mpatchBufT.TriLst) = 0 then
      SetLength( mpatchBufT.TriLst ,  initVideoVBSize ) ;
    if length(mpatchBufT.Subset) = 0 then
      SetLength( mpatchBufT.Subset ,  TruncS(length(mpatchBufT.TriLst) / 3) +1 ) ;

    ptg1 := _ptg1 ;
    ptg2 := _ptg2 ;
    ptg3 := _ptg3 ;
    if mpatchBufT.NumPoints+3 < initVideoVBSize then begin
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg1 ;
      inc(mpatchBufT.NumPoints) ;
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg2 ;
      inc(mpatchBufT.NumPoints) ;
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg3 ;
      mpatchBufT.Subset[mpatchBufT.NumObjects] := _subs ;
      inc(mpatchTileInfo[meshMPatchTilesCounterT][_subs].Count) ;
      inc(mpatchBufT.NumPoints) ;
      inc(mpatchBufT.NumObjects) ;
    end
    else begin
      if safeRendering or (length(mpatchBufT.TriLst) >= maxVertexBufferSize) then begin
        saveMPatchBufT ;
        SetLength( mpatchBufT.TriLst ,  initVideoVBSize ) ;
        SetLength( mpatchBufT.Subset ,  TruncS(length(mpatchBufT.TriLst) / 3) +1 ) ;
      end
      else begin
        SetLength( mpatchBufT.TriLst ,  length(mpatchBufT.TriLst) + videoVBIncrement ) ;
        SetLength( mpatchBufT.Subset ,  TruncS(length(mpatchBufT.TriLst) / 3) +1 ) ;
      end;

      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg1 ;
      inc(mpatchBufT.NumPoints) ;
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg2 ;
      inc(mpatchBufT.NumPoints) ;
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg3 ;
      mpatchBufT.Subset[mpatchBufT.NumObjects] := mpatchSubsetCounter ;
      inc(mpatchTileInfo[meshMPatchTilesCounterT][mpatchSubsetCounter].Count) ;
      inc(mpatchBufT.NumPoints) ;
      inc(mpatchBufT.NumObjects) ;
      subsetTexMP := mpatchSubsetCounter ;
    end;
  end;

  function TGIS_Renderer3DDirectX9.GetUniverseColor
    : TGIS_Color ;
  var
    r, g, b : Byte ;
  begin
    r := ( (backColor shr 16) and $000000FF) ;
    g := ( (backColor shr 08) and $000000FF) ;
    b := ( (backColor shr  0) and $000000FF) ;
    Result := TGIS_Color.FromRGB( b, g, r ) ;
  end;

  procedure TGIS_Renderer3DDirectX9.SetUniverseColor(
    const _value : TGIS_Color
  ) ;
  var
    r, g, b : Byte ;
  begin
    r := _value.R ;
    g := _value.G ;
    b := _value.B ;
    {$IFDEF CLR}
      backColor := D3DCOLOR_XRGB(b, g, r) ;
    {$ELSE}
      backColor := D3DCOLOR_XRGB(r, g, b) ;
    {$ENDIF}
  end;

  procedure TGIS_Renderer3DDirectX9.PrintBmp(
    const _rect          : TRect ;
    const _bmp           : TGIS_Bitmap
  ) ;
  var
    hr              : HResult           ;
    pbackbuffer     : IDirect3DSurface9 ;
    d3dsdbackbuffer : TD3DSurfaceDesc   ;   // surface desc of the backbuffer
    backbuf         : IDirect3DSurface9 ;
    imagewidth,
    imageheight     : Integer           ;
    i, j            : Integer           ;
    i1, j1, i2, j2  : Integer           ;
    k, l            : Integer           ;
    kk, ii          : Integer           ;
    oldproj         : TD3DXMatrix       ;
    destrect        : TRect             ;
    r_left, r_right,
    r_top, r_bottom : Cardinal          ;
    num_tiles       : Double            ;
    bmp             : TGIS_Bitmap       ;
    xratio, yratio  : Double            ;
    pix, pix1       : TGIS_Pixels       ;
    tile            : TGIS_Bitmap       ;

      procedure addtiletobmp( const _roffset : Integer;
                               const _loffset : Integer
                             );
      var
        rr, r1, r2, l, l1, l2, m, n : Integer ;
        p, p1   : TGIS_Pixels ;
      begin
        tile.LockPixels( p, False, TGIS_BitmapFormat.Native, TGIS_BitmapLinesOrder.Down ) ;
        bmp.LockPixels( p1, True, TGIS_BitmapFormat.Native, TGIS_BitmapLinesOrder.Down ) ;
        r1 := _roffset ;
        r2 := r1 + d3dsdbackbuffer.Height -1 ;
        l1 := 0 ;
        l2 := l1 + d3dsdbackbuffer.Width ;
        m := 0 ;
        n := 0 ;
        for rr := r1 to r2 do begin
          n := rr*imagewidth+_loffset ;
          for l := l1 to l2-1 do begin
            assert( n < bmp.Width * bmp.Height ) ;
            assert( m < tile.Width * tile.Height ) ;
            p1[n] := p[m] ;
            inc(m);
            inc(n);
          end;
        end;

        bmp.UnlockPixels ;
        tile.UnlockPixels ;
      end;


      function get_bmp_from_tex( const _roffset : Integer ;
                                 const _loffset : Integer ): Boolean ;
      var
        renderTarget    : IDirect3DSurface9 ;
        resolvedSurface : IDirect3DSurface9 ;
        offscreenSurface: IDirect3DSurface9 ;
        rtDesc          : TD3DSurfaceDesc ;
        lr              : D3DLOCKED_RECT ;
        {$IFDEF CLR}
          rct           : DirectXRectangle ;
        {$ELSE}
          rct           : TRect ;
        {$ENDIF}
      begin
        Result := False ;
        hr := pD3DDevice.GetRenderTarget( 0, renderTarget ) ;
        if (hr <> S_OK) or (not assigned(renderTarget)) then exit;
        renderTarget.GetDesc( rtDesc ) ;
        if( rtDesc.MultiSampleType <> D3DMULTISAMPLE_NONE ) then begin
          hr := pD3DDevice.CreateRenderTarget(
            rtDesc.Width, rtDesc.Height, rtDesc.Format,
            D3DMULTISAMPLE_NONE, 0, False, resolvedSurface, nil
          );
          if (hr <> S_OK) or (not assigned(resolvedSurface)) then begin
            ReleaseInterface( renderTarget ) ;
            exit;
          end;

          {$IFDEF CLR}
            rct := new DirectXRectangle( 0, 0, rtDesc.Width, rtDesc.Height ) ;
            hr := pD3DDevice.StretchRect( renderTarget, rct, resolvedSurface, rct, D3DTEXF_NONE );
          {$ELSE}
            hr := pD3DDevice.StretchRect( renderTarget, nil, resolvedSurface, nil, D3DTEXF_NONE );
          {$ENDIF}
          if (hr <> S_OK) then begin
            ReleaseInterface( resolvedSurface ) ;
            ReleaseInterface( renderTarget ) ;
            exit;
          end;
          renderTarget := resolvedSurface;
        end;

        hr := pD3DDevice.CreateOffscreenPlainSurface( rtDesc.Width, rtDesc.Height,
          rtDesc.Format, D3DPOOL_SYSTEMMEM, offscreenSurface, nil );
        if (hr <> S_OK) then begin
          ReleaseInterface( offscreenSurface ) ;
          ReleaseInterface( resolvedSurface ) ;
          ReleaseInterface( renderTarget ) ;
          exit;
        end;

        {$IFDEF CLR}
          hr := S_OK ;
          pD3DDevice.GetRenderTargetData( renderTarget, offscreenSurface ) ;
        {$ELSE}
          hr := pD3DDevice.GetRenderTargetData( renderTarget, offscreenSurface ) ;
        {$ENDIF}
        if (hr <> S_OK) then begin
          ReleaseInterface( offscreenSurface ) ;
          ReleaseInterface( resolvedSurface ) ;
          ReleaseInterface( renderTarget ) ;
          exit;
        end;
        {$IFDEF CLR}
          rct := new DirectXRectangle( 0, 0, rtDesc.Width, rtDesc.Height ) ;
          hr := offscreenSurface.LockRect( lr, rct, D3DLOCK_READONLY );
        {$ELSE}
          rct.left := 0 ;
          rct.right := rtDesc.Width ;
          rct.top := 0 ;
          rct.bottom := rtDesc.Height ;
          hr := offscreenSurface.LockRect( lr, @rct, D3DLOCK_READONLY );
        {$ENDIF}
        if (hr <> S_OK) then begin
          ReleaseInterface( offscreenSurface ) ;
          ReleaseInterface( resolvedSurface ) ;
          ReleaseInterface( renderTarget ) ;
          exit;
        end;
        tile := TGIS_Bitmap.Create(d3dsdbackbuffer.Width, d3dsdbackbuffer.Height) ;
        tile.LockPixels( pix, True, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
        {$IFDEF CLR}
          Marshal.Copy( lr.DataPointer, pix, 0, d3dsdbackbuffer.Width * d3dsdbackbuffer.Height ) ;
        {$ELSE}
          System.Move( PByte(lr.pBits)^, pix[0], d3dsdbackbuffer.Width * d3dsdbackbuffer.Height * 4 ) ;
        {$ENDIF}
        tile.UnlockPixels ;

        hr := offscreenSurface.UnlockRect ;

        ReleaseInterface( offscreenSurface ) ;
        ReleaseInterface( resolvedSurface ) ;
        ReleaseInterface( renderTarget ) ;

        if (hr = S_OK) then addtiletobmp(_roffset, _loffset ) ;
        FreeObject( tile ) ;

        Result := True ;
      end;

  begin
    if not assigned( _bmp ) then exit ;

    if (_bmp.Width < 3) or (_bmp.Height < 3) then exit ;

    if (_rect.Left < 0) or (_rect.Top  < 0) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ),
                                   'PrintBmp', 0
                                 ) ;
    end ;

    xratio := ( 1.0 * _bmp.Width  ) / (_rect.Right  - _rect.Left ) ;
    yratio := ( 1.0 * _bmp.Height ) / (_rect.Bottom - _rect.Top  ) ;

    num_tiles := Min( xratio, yratio ) ;

    pD3DDevice.GetBackBuffer( 0, 0, D3DBACKBUFFER_TYPE_MONO, pbackbuffer ) ;
    pbackbuffer.GetDesc( d3dsdbackbuffer ) ;
    ReleaseInterface( pbackbuffer ) ;

    i1 := TruncS( num_tiles * _rect.Left   / d3dsdbackbuffer.Width  ) ;
    i2 := TruncS( num_tiles * _rect.Right  / d3dsdbackbuffer.Width  ) ;
    j1 := TruncS( num_tiles * _rect.Top    / d3dsdbackbuffer.Height ) ;
    j2 := TruncS( num_tiles * _rect.Bottom / d3dsdbackbuffer.Height ) ;

    imagewidth := Integer (d3dsdbackbuffer.Width ) * ( i2 - i1 + 1 ) ;
    imageheight:= Integer (d3dsdbackbuffer.Height) * ( j2 - j1 + 1 ) ;

    // save the old projection matrix
    oldproj := mtxProjection ;

    // scale the axis
    {$IFDEF CLR}
      mtxProjection.M11 := mtxProjection.M11 * num_tiles ;
      mtxProjection.M22 := mtxProjection.M22 * num_tiles ;
    {$ELSE}
      mtxProjection._11 := mtxProjection._11 * num_tiles ;
      mtxProjection._22 := mtxProjection._22 * num_tiles ;
    {$ENDIF}

    captureScreen := True ;

    bmp := TGIS_Bitmap.Create( imagewidth, imageheight ) ;
    // loop through the tiles
    for i := i1 to i2 do begin
      {$IFDEF CLR}
        mtxProjection.M31 := (num_tiles-1) - i * 2.0 ;
      {$ELSE}
        mtxProjection._31 := (num_tiles-1) - i * 2.0 ;
      {$ENDIF}
      for j := j1 to j2 do begin
        {$IFDEF CLR}
          mtxProjection.M32 := -( (num_tiles-1) - j * 2.0)  ;
        {$ELSE}
          mtxProjection._32 := -( (num_tiles-1) - j * 2.0)  ;
        {$ENDIF}
        pD3DDevice.SetTransform(D3DTS_PROJECTION, mtxProjection) ;
        Repaint ; //? Draw ;   // rough view
        hr := pD3DDevice.GetBackBuffer(0,0,D3DBACKBUFFER_TYPE_MONO,backbuf) ;
        if hr = 0 then begin

          r_left   := Integer(d3dsdbackbuffer.Width ) * ( i - i1 ) ;
          r_right  := r_left + d3dsdbackbuffer.Width  ;
          r_top    := Integer(d3dsdbackbuffer.Height) * ( j - j1 ) ;
          r_bottom := r_top  + d3dsdbackbuffer.Height ;

          destrect := Rect( r_left, r_top, r_right, r_bottom ) ;
          get_bmp_from_tex(d3dsdbackbuffer.Height*j , d3dsdbackbuffer.Width*i ) ;

          ReleaseInterface( backbuf ) ;
        end
        else
          exit ;
      end;  //j
    end; //i

    // restore original Projection matrix after done saving
    mtxProjection := oldproj ;
    captureScreen := False ;

    i  := j1 * Integer (d3dsdbackbuffer.Height) ;
    j  := i1 * Integer (d3dsdbackbuffer.Width ) ;
    i1 := TruncS( _rect.Top    * num_tiles ) - i ;
    i2 := TruncS( _rect.Bottom * num_tiles ) - i ;
    j1 := TruncS( _rect.Left   * num_tiles ) - j ;
    j2 := TruncS( _rect.Right  * num_tiles ) - j ;
    k  := TruncS((_bmp.Height - Integer (i2 - i1))/2.0) ;

    bmp.LockPixels( pix, False, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
    _bmp.LockPixels( pix1, True, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;

    for i := i1 to i2 -1 do begin
      l := TruncS((_bmp.Width - Integer (j2 - j1))/2.0) ;
      kk := k * _bmp.Width ;
      ii := i *  bmp.Width ;
      for j := j1 to j2 -1 do begin
        pix1[kk+l] := pix[ii+j];
        inc( l ) ;
        if j = imagewidth then break ;
      end;
      inc( k ) ;
      if i = imageheight then break ;
    end;

    _bmp.UnlockPixels ;
    bmp.UnlockPixels ;

    FreeObject( bmp ) ;
  end;

  function  TGIS_Renderer3DDirectX9.PrintBegin(
    const _width  : Integer ;
    const _height : Integer
  ) : TPoint ;
  var
    pbackbuffer     : IDirect3DSurface9 ;
    d3dsdbackbuffer : TD3DSurfaceDesc   ;
  begin
    printBmpOutSize.X := _width ;
    printBmpOutSize.Y := _height ;

    printBmpPPI := oGIS.PPI ;   // required print PPI

    pD3DDevice.GetBackBuffer( 0, 0, D3DBACKBUFFER_TYPE_MONO, pbackbuffer ) ;
    {$IFDEF CLR}
      d3dsdbackbuffer := pbackbuffer.Description ;
    {$ELSE}
      pbackbuffer.GetDesc( d3dsdbackbuffer ) ;
    {$ENDIF}
    ReleaseInterface( pbackbuffer ) ;
    Result := Point( d3dsdbackbuffer.Width, d3dsdbackbuffer.Height ) ;
    printBmpImgSize := 4096 ;
    prepareVertexBuffersToDraw ;
  end;

  procedure TGIS_Renderer3DDirectX9.PrintTile(
    const _bmp      : TGIS_Bitmap ;
    const _offsetx  : Integer ;
    const _offsety  : Integer
  ) ;
  var
    hr              : HResult           ;
    pbackbuffer     : IDirect3DSurface9 ;
    d3dsdbackbuffer : TD3DSurfaceDesc   ;   // surface desc of the backbuffer
    backbuf         : IDirect3DSurface9 ;
    imagewidth,
    imageheight     : Integer           ;
    i, j            : Integer           ;
    i1, j1, i2, j2  : Integer           ;
    ni, nj, nk, nl  : Integer           ;
    ni1, nj1,
    ni2, nj2        : Integer           ;
    oldproj         : TD3DXMatrix       ;
    num_tiles       : Double            ;
    xratio, yratio  : Double            ;
    pix             : TGIS_Pixels       ;
    tile            : TGIS_Bitmap       ;

      procedure addtiletobmp( const _roffset : Integer;
                              const _loffset : Integer
                             );
      var
        rr, r1, r2, l, l1, l2, m, n : Integer ;
        p, p1   : TGIS_Pixels ;
        bwh , rwh : Integer ;
      begin
        tile.LockPixels( p, False, TGIS_BitmapFormat.Native, TGIS_BitmapLinesOrder.Down ) ;
        _bmp.LockPixels( p1, True, TGIS_BitmapFormat.Native, TGIS_BitmapLinesOrder.Down ) ;

        r1 := _roffset ;
        r2 := r1 + d3dsdbackbuffer.Height -1 ;
        if r2 >= ni2 then
          r2 := ni2 ;

        l1 := _loffset ;
        l2 := l1 + d3dsdbackbuffer.Width -1 ;

        m := 0 ;
        bwh := _bmp.Width * _bmp.Height ;
        rwh := tile.Width * tile.Height ;
        for rr := r1 to r2 do begin
          n := (nk + rr) * printBmpOutSize.X + _loffset ;          for l := l1 to l2 do begin
            if printBmpAllInOne then begin
              assert( n < bwh ) ;
              assert( m < rwh ) ;
              if l < nj2 then begin
                p1[n+nl] := p[m] ;
                inc( n ) ;
              end;
            end
            else begin
              if l < nj2 then begin
                p1[m] := p[m] ;
                inc( n ) ;
              end;
            end;
            inc( m ) ;
          end;
        end;

        _bmp.UnlockPixels ;
        tile.UnlockPixels ;
      end;


      function get_bmp_from_tex( const _roffset : Integer ;
                                 const _loffset : Integer ): Boolean ;
      var
        renderTarget    : IDirect3DSurface9 ;
        resolvedSurface : IDirect3DSurface9 ;
        offscreenSurface: IDirect3DSurface9 ;
        rtDesc          : TD3DSurfaceDesc ;
        lr              : D3DLOCKED_RECT ;
        {$IFDEF CLR}
          rct           : DirectXRectangle ;
        {$ELSE}
          rct           : TRect ;
        {$ENDIF}
      begin
        Result := False ;
        hr := pD3DDevice.GetRenderTarget( 0, renderTarget ) ;
        if (hr <> S_OK) or (not assigned(renderTarget)) then exit;
        renderTarget.GetDesc( rtDesc ) ;
        if( rtDesc.MultiSampleType <> D3DMULTISAMPLE_NONE ) then begin
          hr := pD3DDevice.CreateRenderTarget(
            rtDesc.Width, rtDesc.Height, rtDesc.Format,
            D3DMULTISAMPLE_NONE, 0, False, resolvedSurface, nil
          );
          if (hr <> S_OK) or (not assigned(resolvedSurface)) then begin
            ReleaseInterface( renderTarget ) ;
            exit;
          end;

          {$IFDEF CLR}
            rct := new DirectXRectangle( 0, 0, rtDesc.Width, rtDesc.Height ) ;
            hr := pD3DDevice.StretchRect( renderTarget, rct, resolvedSurface, rct, D3DTEXF_NONE );
          {$ELSE}
            hr := pD3DDevice.StretchRect( renderTarget, nil, resolvedSurface, nil, D3DTEXF_NONE );
          {$ENDIF}
          if (hr <> S_OK) then begin
            ReleaseInterface( resolvedSurface ) ;
            ReleaseInterface( renderTarget ) ;
            exit;
          end;
          renderTarget := resolvedSurface;
        end;

        hr := pD3DDevice.CreateOffscreenPlainSurface( rtDesc.Width, rtDesc.Height,
          rtDesc.Format, D3DPOOL_SYSTEMMEM, offscreenSurface, nil );
        if (hr <> S_OK) then begin
          ReleaseInterface( offscreenSurface ) ;
          ReleaseInterface( resolvedSurface ) ;
          ReleaseInterface( renderTarget ) ;
          exit;
        end;

        {$IFDEF CLR}
          hr := S_OK ;
          pD3DDevice.GetRenderTargetData( renderTarget, offscreenSurface ) ;
        {$ELSE}
          hr := pD3DDevice.GetRenderTargetData( renderTarget, offscreenSurface ) ;
        {$ENDIF}
        if (hr <> S_OK) then begin
          ReleaseInterface( offscreenSurface ) ;
          ReleaseInterface( resolvedSurface ) ;
          ReleaseInterface( renderTarget ) ;
          exit;
        end;
        {$IFDEF CLR}
          rct := new DirectXRectangle( 0, 0, rtDesc.Width, rtDesc.Height ) ;
          hr := offscreenSurface.LockRect( lr, rct, D3DLOCK_READONLY );
        {$ELSE}
          rct.left := 0 ;
          rct.right := rtDesc.Width ;
          rct.top := 0 ;
          rct.bottom := rtDesc.Height ;
          hr := offscreenSurface.LockRect( lr, @rct, D3DLOCK_READONLY );
        {$ENDIF}
        if (hr <> S_OK) then begin
          ReleaseInterface( offscreenSurface ) ;
          ReleaseInterface( resolvedSurface ) ;
          ReleaseInterface( renderTarget ) ;
          exit;
        end;
        tile := TGIS_Bitmap.Create(d3dsdbackbuffer.Width, d3dsdbackbuffer.Height) ;
        tile.LockPixels( pix, True, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
        {$IFDEF CLR}
          Marshal.Copy( lr.DataPointer, pix, 0, d3dsdbackbuffer.Width * d3dsdbackbuffer.Height ) ;
        {$ELSE}
          System.Move( PByte(lr.pBits)^, pix[0], d3dsdbackbuffer.Width * d3dsdbackbuffer.Height * 4 ) ;
        {$ENDIF}
        tile.UnlockPixels ;

        hr := offscreenSurface.UnlockRect ;

        ReleaseInterface( offscreenSurface ) ;
        ReleaseInterface( resolvedSurface ) ;
        ReleaseInterface( renderTarget ) ;

        if (hr = S_OK) then addtiletobmp(_roffset, _loffset ) ;
        FreeObject( tile ) ;

        Result := True ;
      end;

  begin
    if printBmpOutSize = Point( 0, 0 ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ),
                                   'PrintTile: Size of output bitmap not defined', 0
                                 ) ;
    end;

    if ( _bmp.Width  = printBmpOutSize.X ) and
       ( _bmp.Height = printBmpOutSize.Y ) then
      printBmpAllInOne := True
    else
      printBmpAllInOne := False ;

    if (_bmp.Width < 3) or (_bmp.Height < 3) then exit ;

    pD3DDevice.GetBackBuffer( 0, 0, D3DBACKBUFFER_TYPE_MONO, pbackbuffer ) ;
    pbackbuffer.GetDesc( d3dsdbackbuffer ) ;
    ReleaseInterface( pbackbuffer ) ;

    i := RoundS( _offsetx / d3dsdbackbuffer.Width ) ;
    j := RoundS( _offsety / d3dsdbackbuffer.Height ) ;
    if ((i * d3dsdbackbuffer.Width  - _offsetx) <> 0) or
       ((j * d3dsdbackbuffer.Height - _offsety) <> 0) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ),
                                   'PrintTile: offset not equal to multiple tile size not allowed', 0
                                 ) ;
    end;

    xratio := ( 1.0 * printBmpOutSize.X ) / (d3dsdbackbuffer.Width ) ;
    yratio := ( 1.0 * printBmpOutSize.Y ) / (d3dsdbackbuffer.Height ) ;

    num_tiles := Min( xratio, yratio ) ;

    i1 := 0 ;
    i2 := RoundS( num_tiles * d3dsdbackbuffer.Width  / d3dsdbackbuffer.Width  ) ;
    j1 := 0 ;
    j2 := RoundS( num_tiles * d3dsdbackbuffer.Height / d3dsdbackbuffer.Height ) ;

    imagewidth := Integer (d3dsdbackbuffer.Width ) * ( i2 - i1 + 1 ) ;
    imageheight:= Integer (d3dsdbackbuffer.Height) * ( j2 - j1 + 1 ) ;


    ni  := j1 * Integer (d3dsdbackbuffer.Height) ;
    nj  := i1 * Integer (d3dsdbackbuffer.Width ) ;
    ni1 := RoundS( 0    * num_tiles ) - ni ;
    ni2 := RoundS( d3dsdbackbuffer.Height * num_tiles ) - ni ;
    nj1 := RoundS( 0 * num_tiles ) - nj ;
    nj2 := RoundS( d3dsdbackbuffer.Width  * num_tiles ) - nj ;
    nk  := RoundS((printBmpOutSize.Y - Integer (ni2 - ni1))/2.0) ;
    nl  := RoundS((printBmpOutSize.X - Integer (nj2 - nj1))/2.0) ;

    // save the old projection matrix
    oldproj := mtxProjection ;
    // do not display tiles
    captureScreen := True ;
    try
      // scale the axis
      {$IFDEF CLR}
        mtxProjection.M11 := mtxProjection.M11 * num_tiles ;
        mtxProjection.M22 := mtxProjection.M22 * num_tiles ;
      {$ELSE}
        mtxProjection._11 := mtxProjection._11 * num_tiles ;
        mtxProjection._22 := mtxProjection._22 * num_tiles ;
      {$ENDIF}

      if printBmpAllInOne then begin
        // loop through all tiles
        for i := i1 to i2 do begin
          {$IFDEF CLR}
            mtxProjection.M31 := (num_tiles-1) - i * 2.0 ;
          {$ELSE}
            mtxProjection._31 := (num_tiles-1) - i * 2.0 ;
          {$ENDIF}
          for j := j1 to j2 do begin
            {$IFDEF CLR}
              mtxProjection.M32 := -( (num_tiles-1) - j * 2.0)  ;
            {$ELSE}
              mtxProjection._32 := -( (num_tiles-1) - j * 2.0)  ;
            {$ENDIF}
            pD3DDevice.SetTransform(D3DTS_PROJECTION, mtxProjection) ;
            Repaint ;
            hr := pD3DDevice.GetBackBuffer(0,0,D3DBACKBUFFER_TYPE_MONO,backbuf) ;
            if hr = 0 then begin
              get_bmp_from_tex(d3dsdbackbuffer.Height*j , d3dsdbackbuffer.Width*i ) ;
              ReleaseInterface( backbuf ) ;
            end
            else
              exit ;
          end;  //j
        end; //i
      end
      else begin
        // required tile
        i := RoundS( _offsetx / d3dsdbackbuffer.Width ) ;
        if i > i2 then exit ;
        j := RoundS( _offsety / d3dsdbackbuffer.Height ) ;
        if j > j2 then exit ;
        {$IFDEF CLR}
          mtxProjection.M31 := (num_tiles-1) - i * 2.0 ;
        {$ELSE}
          mtxProjection._31 := (num_tiles-1) - i * 2.0 ;
        {$ENDIF}
        {$IFDEF CLR}
          mtxProjection.M32 := -( (num_tiles-1) - j * 2.0)  ;
        {$ELSE}
          mtxProjection._32 := -( (num_tiles-1) - j * 2.0)  ;
        {$ENDIF}
        pD3DDevice.SetTransform(D3DTS_PROJECTION, mtxProjection) ;
        Repaint ;
        hr := pD3DDevice.GetBackBuffer(0,0,D3DBACKBUFFER_TYPE_MONO,backbuf) ;
        if hr = 0 then begin
          get_bmp_from_tex(d3dsdbackbuffer.Height*j , d3dsdbackbuffer.Width*i ) ;
          ReleaseInterface( backbuf ) ;
        end
        else
          exit ;
      end;
    finally
      // restore original Projection matrix after scene rendering
      mtxProjection := oldproj ;
      captureScreen := False ;
    end;
  end;

  procedure TGIS_Renderer3DDirectX9.PrintEnd ;
  begin
    printBmpOutSize := Point( 0, 0 ) ;
    printBmpImgSize := 0 ;
    imgFrameSize := Max(windowWidth,windowHeight) ;
    prepareVertexBuffersToDraw ;
  end;

  function  TGIS_Renderer3DDirectX9.partialRender
    : Boolean ;
  var
    i, j, k   : Integer          ;
    lv        : TGIS_LayerVector ;
    la        : TGIS_Layer       ;
    a         : Cardinal         ;
    r,g,b     : Byte             ;
    old_zu    : Double           ;

      procedure updateTiles ;
      begin
         LabelsReg.Reset ;
         T_textureHelperDX( oTextureHelper ).Clear ;
         cutVectExtentL := currentVL.Extent ;
         cutVectExtentV := cutVectExtentL ;
        {$IFDEF CLR}
          if not lv.ForEach( cutVectExtentL, '', nil, '', True,
                             oGIS.ScaleAsFloat, @addShapeToCache ) then begin
        {$ELSE}
          if not lv.ForEach( cutVectExtentL, '', nil, '', True,
                             oGIS.ScaleAsFloat,  addShapeToCache ) then begin
        {$ENDIF}
            if shpNo > 0 then begin
              if assigned(currentVL) then
              if currentVL.Transparency <> 100 then
                // turn off the color blending
                blendOff ;
              pD3DDevice.EndScene ;
              zUnit := old_zu ;
              raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                           'partialRender', 1 ) ;
            end ;
          end
          else
            curVLlist[j] := shpNo ;

          addNewPolyRoofRow ;
          saveVTLBufC(True , vTLBufCR) ; // roofs , color
          saveVTLBufC(False, vTLBufCW) ; // walls , color
          saveVTLBufT(True , vTLBufTR) ; // roofs , texture
          saveVTLBufT(False, vTLBufTW) ; // walls , texture
          saveWPLBuf                   ; // edges
          saveWPBuf                    ; // points
          saveMPatchBufC               ; // multi patch, color
          saveMPatchBufT               ; // multi patch, texture
      end;

      procedure releaseBuffer( const _k : Integer ) ;
      begin
        case arVectorTilesInfo[_k].TileType of
            1  : begin // vTLBufCW polygon wall
                   if meshVectTilesCounterCW = 0 then exit;
                   if high(meshVectTilesCW) <
                        arVectorTilesInfo[_k].TileIndex then exit;
                   FreeObject(
                            meshVectTilesCW[arVectorTilesInfo[_k].TileIndex] ) ;
                   dec( meshVectTilesCounterCW ) ;
                   SetLength( meshVectTilesCW, meshVectTilesCounterCW ) ;
                 end;
            2  : begin // vTLBufCR polygon roof
                   if meshVectTilesCounterCR = 0 then exit;
                   if high(meshVectTilesCR) <
                        arVectorTilesInfo[_k].TileIndex then exit;
                   FreeObject(
                            meshVectTilesCR[arVectorTilesInfo[_k].TileIndex] ) ;
                   dec( meshVectTilesCounterCR ) ;
                   SetLength( meshVectTilesCR, meshVectTilesCounterCR ) ;
                 end;
            3  : begin // vTLBufCW polygon wall
                   if meshVectTilesCounterTW = 0 then exit;
                   if high(meshVectTilesTW) <
                        arVectorTilesInfo[_k].TileIndex then exit;
                   FreeObject(
                            meshVectTilesTW[arVectorTilesInfo[_k].TileIndex] ) ;
                   dec( meshVectTilesCounterTW ) ;
                   SetLength( meshVectTilesTW, meshVectTilesCounterTW ) ;
                 end;
            4  : begin // vTLBufCR polygon roof
                   if meshVectTilesCounterTR = 0 then exit;
                   if high(meshVectTilesTR) <
                        arVectorTilesInfo[_k].TileIndex then exit;
                   FreeObject(
                            meshVectTilesTR[arVectorTilesInfo[_k].TileIndex] ) ;
                   dec( meshVectTilesCounterTR ) ;
                   SetLength( meshVectTilesTR, meshVectTilesCounterTR ) ;
                 end;
{ TODO -cReview : to be tested }
//?            5  : // DEM
//?           setentry(k, meshDemTilesCounter -1  ,
//?                        meshDemTiles[meshDemTilesCounter -1].GetNumVertices ) ;
//?             6  : // DEM wall
//?            setentry(k, meshWallTilesCounter -1 ,
//?                        meshWallTiles[meshWallTilesCounter -1].GetNumVertices ) ;
//?             7  : // Color TIN
//?            setentry(k, meshTinTilesCounterC    , tinBufC.NumPoints ) ;
//?             8  : // Textured TIN
//?            setentry(k, meshTinTilesCounterT    , tinBufT.NumPoints ) ;

            9  : begin // Color Multipatch
                   if meshMPatchTilesCounterC = 0 then exit;
                   if high(meshMPatchTilesC) <
                        arVectorTilesInfo[_k].TileIndex then exit;
                   FreeObject(
                            meshMPatchTilesC[arVectorTilesInfo[_k].TileIndex] ) ;
                   dec( meshMPatchTilesCounterC ) ;
                   SetLength( meshMPatchTilesC, meshMPatchTilesCounterC ) ;
                 end;
            10 : begin // Textured Multipatch
                   if meshMPatchTilesCounterT = 0 then exit;
                   if high(meshMPatchTilesT) <
                        arVectorTilesInfo[_k].TileIndex then exit;
                   FreeObject(
                            meshMPatchTilesT[arVectorTilesInfo[_k].TileIndex] ) ;
                   dec( meshMPatchTilesCounterT ) ;
                   SetLength( meshMPatchTilesT, meshMPatchTilesCounterT ) ;
                 end;
            11 : begin // lines, edges
                   if meshLineTilesCounter = 0 then exit;
                   if high(meshLineTiles) <
                        arVectorTilesInfo[_k].TileIndex then exit;
                   SetLength( meshLineTiles[arVectorTilesInfo[_k].TileIndex].Buffer, 0 ) ;
                   dec( meshLineTilesCounter ) ;
                   SetLength( meshLineTiles, meshLineTilesCounter ) ;
                 end;
            12 : begin // points
                   if meshPointTilesCounter = 0 then exit;
                   if high(meshPointTiles) <
                        arVectorTilesInfo[_k].TileIndex then exit;
                   SetLength( meshPointTiles[arVectorTilesInfo[_k].TileIndex].Buffer, 0 ) ;
                   dec( meshPointTilesCounter ) ;
                   SetLength( meshPointTiles, meshPointTilesCounter ) ;
                 end;
          end;

      end;

  begin
    Result := False ;
    if vectorTilesInfoCounter = 0 then begin // no dynamic layer
      bPartialUpdate := False ;
      exit ;
    end;

    for j := oGIS.Items.Count -1 downto 0 do begin
      la :=  TGIS_Layer(oGIS.Items[j]) ;
      if assigned(la) then begin
        if (la is TGIS_LayerVector) and (not la.CachedPaint ) then begin
          for i := 0 to vectorTilesInfoCounter -1 do begin
            if ( la = arVectorTilesInfo[i].Layer ) then begin
              releaseBuffer( i ) ;
              arVectorTilesInfo[i].ToDraw := True ;
            end;
          end;
        end
        else
          break;
      end;
    end;

      shpNo := 0 ;
      old_zu := zUnit ;
      if lightSwitch then
        lightOn
      else
        lightOff ;
      pixSize := GetPixelSize.X;
      currentVL := nil ;
      releaseLabelInfo ;
      for k := 0 to vectorTilesInfoCounter -1 do begin
        if currentVL = arVectorTilesInfo[k].Layer then continue ;
        currentVL := arVectorTilesInfo[k].Layer ;
        for j := 0 to curVLSize-1 do begin
          if curVL[j].Lv = currentVL then begin
            lv := currentVL ;
            projFactor  := curVL[j].Zu ;
            projMFactor := curVL[j].Fx ;
            cutVectExtentL :=
                         oGIS.CS.ExtentToCS( currentVL.CS, cutVectExtentV ) ;
            a := TruncS (currentVL.Transparency/100*255) ;
            r := edgesColor.R ;
            g := edgesColor.G ;
            b := edgesColor.B ;
            edColor := D3DCOLOR_ARGB(a,r,g,b) ;
            setCVLTransparency;
            break ;
          end;
        end;
        if arVectorTilesInfo[k].ToDraw then
          updateTiles ;
      end;

    bPartialUpdate := False ;

    pD3DDevice.BeginScene;
    try
      pD3DDevice.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
      pD3DDevice.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
      pD3DDevice.SetTexture( 0, nil ) ;

      drawVectTiles    ;
      drawMeshLinTiles ;
      drawMeshPntTiles ;
      drawMPatchTiles  ;

      if assigned(currentVL) then
      if currentVL.Transparency <> 100 then
        // turn off the color blending
        blendOff ;
    finally
      pD3DDevice.EndScene ;
    end;

    zUnit := old_zu ;
    Result := True ;

  end;

  function  TGIS_Renderer3DDirectX9.getWindowSize : TPoint ;
    {$IFDEF CLR}
    {$ELSE}
    var
      pwi      : TWindowInfo ;
    {$ENDIF}
  begin
    {$IFDEF CLR}
      Result := GetWindowControlSize( hWindow ) ;
    {$ELSE}
      GetWindowInfo( hWindow, pwi ) ;
      Result.X := pwi.rcClient.Right  - pwi.rcClient.Left ;
      Result.Y := pwi.rcClient.Bottom - pwi.rcClient.Top  ;
    {$ENDIF}
  end;

  function  TGIS_Renderer3DDirectX9.getTimeTic : DWORD ;
  begin
    Result := GetTickCount ;
  end;

  procedure TGIS_Renderer3DDirectX9.releaseAllInterfaces ;
  begin
    if assigned( pdataTEX_d ) then
      ReleaseInterface( pdataTEX_d ) ;

    if assigned( pdataTEX_r ) then
      ReleaseInterface( pdataTEX_r ) ;

    if assigned( pdataTEX_w ) then
      ReleaseInterface( pdataTEX_w ) ;

    if assigned( pdataTEX_vDX ) then
      ReleaseInterface( pdataTEX_vDX ) ;

    if assigned( pdataTEX_tDX ) then
      ReleaseInterface( pdataTEX_tDX ) ;

    if assigned( pdataTEX_bDX ) then
      ReleaseInterface( pdataTEX_bDX ) ;

    if assigned( pdataTEX_sDX ) then
      ReleaseInterface( pdataTEX_sDX ) ;

    if assigned( pdataTEX_sun ) then
      ReleaseInterface( pdataTEX_sun ) ;

    if assigned( pdataVB ) then
      ReleaseInterface( pdataVB ) ;

    if assigned( cdataVB ) then
      ReleaseInterface( cdataVB ) ;

    if assigned( idataIB ) then
      ReleaseInterface( idataIB ) ;

    if assigned( pD3DDevice ) then
      ReleaseInterface( pD3DDevice ) ;

    if assigned( pD3D ) then
      ReleaseInterface( pD3D ) ;

    if assigned( arBitmap_w ) then
      FreeObject( arBitmap_w ) ;
  end;

  {$IFDEF CLR}
    constructor TMesh.Create(
      const _faces    : DWORD ;
      const _vertices : DWORD ;
      const _format   : D3DFVF ;
      const _device   : IDirect3DDevice9 ;
      const _pvb      : IDirect3DVertexBuffer9 ;
      const _cvb      : IDirect3DVertexBuffer9 ;
      const _iib      : IDirect3DIndexBuffer9
    ) ;
  {$ELSE}
    constructor TMesh.Create(
      const _faces    : DWORD ;
      const _vertices : DWORD ;
      const _format   : DWORD ;
      const _device   : IDirect3DDevice9 ;
      const _pvb      : IDirect3DVertexBuffer9 ;
      const _cvb      : IDirect3DVertexBuffer9 ;
      const _iib      : IDirect3DIndexBuffer9
    ) ;
  {$ENDIF}
  begin
    device := _device ;
    pdataVB := _pvb ;
    cdataVB := _cvb ;
    idataIB := _iib ;
    format :=  Integer(_format) ;
    SetLength( faces, _faces ) ;
    SetLength( indices, 3 * _faces ) ;
    SetLength( subsets, 0 ) ;
    case format of
      DWORD(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) : SetLength( tvertices, _vertices ) ;
      DWORD(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR)   : SetLength( cvertices, _vertices ) ;
    end ;
  end ;

  procedure TMesh.doDestroy ;
  begin
    SetLength( faces, 0 ) ;
    SetLength( indices, 0 ) ;
    SetLength( tvertices, 0) ;
    SetLength( cvertices, 0) ;
    SetLength( subsets, 0 ) ;

    cdataVB := nil ;
    idataIB := nil ;
   // if assigned( cdataVB ) then ReleaseInterface( cdataVB ) ;
   // if assigned( idataIB ) then ReleaseInterface( idataIB ) ;
    inherited ;
  end;

  {$IFDEF CLR}
    function TMesh.pdataVB_Lock(
      var _strm : DirectXStream
    ) : Integer ;
    begin
      try
        pdataVB.Lock(0, 0, _strm, D3DLOCK_NONE) ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ELSE}
    function TMesh.pdataVB_Lock(
      var _strm : PByte
    ) : Integer ;
    begin
      try
        pdataVB.Lock(0, 0, Pointer(_strm), D3DLOCK_NONE) ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ENDIF}

  {$IFDEF CLR}
    function TMesh.cdataVB_Lock(
      var _strm : DirectXStream
    ) : Integer ;
    begin
      try
        cdataVB.Lock(0, 0, _strm, D3DLOCK_NONE) ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ELSE}
    function TMesh.cdataVB_Lock(
      var _strm : PByte
    ) : Integer ;
    begin
      try
        cdataVB.Lock(0, 0, Pointer(_strm), D3DLOCK_NONE) ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ENDIF}

  {$IFDEF CLR}
    function TMesh.pdataVB_Unlock(
      var _strm : DirectXStream ;
      var _data : T_arTextureData
    ) : Integer ;
    begin
      if length( _data ) > 0 then
        _strm.WriteRange<TGIS_Renderer3DVertex>( _data ) ;

      try
        pdataVB.Unlock ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ELSE}
    function TMesh.pdataVB_Unlock(
      var _strm : PByte        ;
      var _data : T_arTextureData
    ) : Integer ;
    begin
      if length( _data ) > 0 then
        System.Move( _data[0], _strm^ , length( _data ) * SizeOf( _data[0]) ) ;

      try
        pdataVB.Unlock ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ENDIF}

  {$IFDEF CLR}
    function TMesh.cdataVB_Unlock(
      var _strm : DirectXStream ;
      var _data : T_arColorData
    ) : Integer ;
    begin
      if length( _data ) > 0 then
        _strm.WriteRange<TGIS_Renderer3DVertex>( _data ) ;

      try
        cdataVB.Unlock ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ELSE}
    function TMesh.cdataVB_Unlock(
      var _strm : PByte        ;
      var _data : T_arColorData
    ) : Integer ;
    begin
      if length(_data) > 0 then
        System.Move( _data[0], _strm^ , length( _data ) * SizeOf( _data[0]) ) ;

      try
        cdataVB.Unlock ;
        Result :=  0 ;
      except
        Result := -1 ;
      end ;
    end ;
  {$ENDIF}

  function  TMesh.GetNumVertices : DWORD ;
  begin
    Result := 0 ;
    case format of
      DWORD(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) : Result := length( tvertices ) ;
      DWORD(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR)   : Result := length( cvertices ) ;
    end;
  end;

  function  TMesh.GetNumFaces : DWORD ;
  begin
    Result := length( faces ) ;
  end;

  procedure TMesh.SortIndices(
    _num      : DWORD
  ) ;
  var
    i,j,ii,jj,k,l,ll : Integer ;

    procedure swap_faces(const _i : Integer; const _j : Integer ) ;
    var
      a : DWORD ;
    begin
      a := faces[_i] ;
      faces[_i] := faces[_j] ;
      faces[_j] := a ;
    end;

    procedure swap_indices(const _i : DWORD; const _j : DWORD ) ;
    var
      a : DWORD ;
      m,n : DWORD ;
    begin
      m := _i * 3 ;
      n := _j * 3 ;
      a := indices[m] ;
      indices[m] := indices[n] ;
      indices[n] := a ;
      m := m +1 ;
      n := n +1 ;
      a := indices[m] ;
      indices[m] := indices[n] ;
      indices[n] := a ;
      m := m +1 ;
      n := n +1 ;
      a := indices[m] ;
      indices[m] := indices[n] ;
      indices[n] := a ;
    end;

    procedure sortIndicesEx ;
    var
      i, j, jj : DWORD ;
    begin
      i := 0 ; // subset searched
      l := 0 ; // number of subset found
      for j := 0 to k -1 do begin
        if faces[j] = i then begin  // subset found
          inc( l ) ;
          if l = subsets[i] then begin
            l := 0 ;
            inc( i ) ;
            if i = _num then break;
          end;
        end
        else begin                 // subset not found
          for jj := j to k-1 do begin
            if faces[jj] = i then begin
              swap_faces( j, jj ) ;
              swap_indices(j, jj ) ;
              inc( l ) ;
              if l = subsets[i] then begin
                l := 0 ;
                inc( i ) ;
                if i = _num then break;
              end;
              break;
            end;
          end;
        end;
      end;
    end;

  begin
    if _num = 1 then exit
    else
      SetLength( subsets, _num ) ;
    k := GetNumFaces ;
    for i := 0 to k -1 do
      inc( subsets[faces[i]] );

    if _num > 2 then begin
      sortIndicesEx ;
      exit;
    end;

    if (subsets[0] = 0) or (subsets[1] = 0) then exit;

    i  := 0 ; // subset searched
    l  := 0 ; // number of subset i found
    ll := 0 ; // number of subset i=1 found
    ii := subsets[i] ;

    for j := 0 to subsets[0] -1 do begin
      if faces[j] = i then begin  // subset found
        inc( l ) ;
      end
      else begin                  // subset not found
        for jj := ii to k-1 do begin
          if faces[jj] = i then begin
            swap_faces( j, jj ) ;
            swap_indices(j, jj ) ;
            inc( l ) ;
            inc( ll ) ;
            ii := jj +1 ;
            break;
          end
          else
            inc( ll ) ;
        end;
      end;
      if ii = k then break;
    end;
  end;

  procedure TMesh.UpdateBuffer ;
  begin
    bDraw := False
  end;

  function  TMesh.DrawSubset(
    AttribId   : DWORD
  ) : Integer ;
  var
    tpardata : T_arTextureData ;
    cpardata : T_arColorData  ;
    {$IFDEF CLR}
      idata  : DirectXStream  ;
      cdata  : DirectXStream  ;
      pdata  : DirectXStream  ;
    {$ELSE}
      idata  : PByte          ;
      cdata  : PByte          ;
      pdata  : PByte          ;
    {$ENDIF}
    k, l     : DWORD          ;

    function findex( const _id : DWORD ) : DWORD ;
    var
      i   : Integer ;
      sum : DWORD ;
    begin
      if _id <> 0  then begin
        sum := 0 ;
        for i := 0 to _id -1 do
          sum := sum + subsets[i] *3 ;
        Result := sum ;
      end
      else
        Result := 0 ;
    end;

  begin
    Result := 0 ;
    k := GetNumVertices ;
    l := GetNumFaces ;
    case format of
      DWORD(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) : begin
        device.SetStreamSource( 0, pdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
        device.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DTEXTURE) ;
        if not bDraw then begin
          pdataVB_Lock( pdata ) ;
          {$IFDEF CLR}
            pdata.WriteRange<TGIS_Renderer3DVertex>( tvertices ) ;
          {$ELSE}
            System.Move( tvertices[0], pdata^, k * sizeOf(TGIS_Renderer3DVertex) ) ;
          {$ENDIF}
          pdataVB_Unlock( pdata, tpardata ) ;
        end;
      end;
      DWORD(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR)   : begin
        device.SetStreamSource( 0, cdataVB, 0, sizeOf(TGIS_Renderer3DVertex) ) ;
        device.SetFVF(GISVIEWER3D_D3DFVF_GRAPH3DCOLOR) ;
        if not bDraw then begin
          cdataVB_Lock( cdata ) ;
          {$IFDEF CLR}
            cdata.WriteRange<TGIS_Renderer3DVertex>( cvertices ) ;
          {$ELSE}
            System.Move( cvertices[0], cdata^, k * sizeOf(TGIS_Renderer3DVertex) ) ;
          {$ENDIF}
          cdataVB_Unlock( cdata, cpardata ) ;
        end;
      end;
    end;

    if not bDraw then begin
    {$IFDEF CLR}
      if idataIB.Lock( 0, 0, idata, D3DLOCK_NONE ) <> 0 then
        exit ;

      idata.WriteRange<Int32>( indices ) ;

      idataIB.Unlock ;
    {$ELSE}
      if idataIB.Lock( 0, 0, Pointer( idata ), D3DLOCK_NONE ) <> 0 then
        exit ;

      System.Move( indices[0], idata^, length( indices ) * sizeOf( indices[0] ) ) ;

      if idataIB.Unlock <> 0 then
        exit ;
    {$ENDIF}
    end;
    device.SetIndices( idataIB ) ;


    if length( subsets ) = 0 then
      device.DrawIndexedPrimitive( D3DPT_TRIANGLELIST,        // prim. type
                                   0,                         // base vertex index
                                   0,                         // min  vertex index
                                   GetNumVertices,            // num vertices
                                   0,                         // start index
                                   GetNumFaces                // primitive count
                                  )
    else
      device.DrawIndexedPrimitive( D3DPT_TRIANGLELIST,        // prim. type
                                   0,                         // base vertex index
                                   0,                         // min  vertex index
                                   GetNumVertices,            // num vertices
                                   findex(AttribId),             // start index
                                   subsets[AttribId]          // primitive count
                                  ) ;

    Result := 0 ;
    if (AttribId = 0) or (length( subsets ) = 0) then
      bDraw := True ;
  end;

//==============================================================================
// initialization / finalization
//==============================================================================

{$IFDEF DCC}
initialization
  oLabelsReg := TGIS_LabelsArea.Create( nil )  ;

finalization
  FreeObject( oLabelsReg ) ;
{$ENDIF}

//==================================== END =====================================
end.
