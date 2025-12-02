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
  Encapsulation of a FMX.TGIS_Renderer3D class.
}

unit Lider.CG.GIS.FMX.GeoRenderer3D ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoRenderer3D"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  FMX.Types,
  FMX.Types3D,
  FMX.Graphics,
  FMX.Materials,
  FMX.Controls3D,
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypes3D,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoLayerPixel,
  Lider.CG.GIS.GeoLabelsArea,
  Lider.CG.GIS.GeoHtmlLabel,
  Lider.CG.GIS.GeoShapes3D,
  Lider.CG.GIS.GeoRendererAbstract,
  Lider.CG.GIS.GeoRenderer3DAbstract ;


{$IFNDEF GENDOC}
const

  /// <summary>
  ///   Color vertex format definition.
  /// </summary>
  Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DCOLOR   =
    [TVertexFormat.Vertex, TVertexFormat.Normal, TVertexFormat.Color0, TVertexFormat.TexCoord0];
  /// <summary>
  ///   Texture vertex format definition.
  /// </summary>
  Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE =
    [TVertexFormat.Vertex, TVertexFormat.Normal, TVertexFormat.Color0, TVertexFormat.TexCoord0];
{$ENDIF}

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
  ///   Structure storing label info
  /// </summary>
  T_labelInfo  = record

    /// <summary>
    ///   Label x,y coordinate.
    /// </summary>
    Centroid       : TGIS_Point        ;

    /// <summary>
    ///   Label z coordinate.
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
    BitmapSize     : TPoint            ;

    /// <summary>
    ///   Label texture.
    /// </summary>
    LabelTextureDX : TTexture          ;

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
    TexDX  : TTexture ;

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
  ///   Color definition for compatibility purpose.
  /// </summary>
  TD3DColor = DWORD ;

type
  /// <summary>
  ///   3D FireMonkey renderer.
  /// </summary>
  TGIS_Renderer3DFMX = class ( TGIS_Renderer3DAbstract )
  private
    oContext               : TContext3D ;
    oLight                 : TLight        ;
    oCamera                : TCamera  ;
    color_mtrl             : TColorMaterial   ;
    texture_mtrl           : TTextureMaterial ;
    light_mtrl             : TLightMaterial   ;
    pdataVB, cdataVB       : TVertexBuffer ; // Vertex Buffer data
    idataIB                : TIndexBuffer ;  // Index Bffer data
    pdataTEX_d             : TTexture ;      // Detail texture data
    pdataTEX_r             : TTexture ;      // Rough texture data
    pdataTEX_w             : TTexture ;      // Wall texture
    pdataTEX_vDX           : TTexture ;      // Vector texture
    pdataTEX_tDX           : TTexture ;      // TIN texture
    pdataTEX_bDX           : TTexture ;      // Basement texture
    pdataTEX_cDX           : TTexture ;      // color texture
    pdataTEX_sDX           : TTexture ;      // SkyBox texture
    pdataTEX_sun           : TTexture ;      // Sun texture
    labelInfo              : T_arLabelInfo     ;
    oMtrl                  : TColorMaterial    ;
    ambColorDX             : TD3DColor         ;
    ambColor               : TD3DColor        ;
    backColor              : TD3DColor ;    // D3D window background color
    borderColor            : TD3DColor ;    // texture border color
    mpatchTileInfo         : T_arMeshTileInfo   ;
    meshTileInfoR          : T_arMeshTileInfo   ;
    meshTileInfoW          : T_arMeshTileInfo   ;
    curMeshTexR            : TTexture  ; //roof
    curMeshTexW            : TTexture  ; //wall
    mtxWorld               : TMatrix3D ;   // World D3DXMATRIX
    mtxView                : TMatrix3D ;   // View D3DXMATRIX
    mtxProjection          : TMatrix3D ;   // Projection D3DXMATRIX
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
    vPickRayDir            : TPoint3D           ;
    anisotropy_factor      : Cardinal           ;
    oColorHelper           : TObject            ;
    pixelSize3D            : Double             ;
    printBmpAllInOne       : Boolean            ;
    arTexTinInfo           : array of TTexture  ;
    mtxMVP                 : TMatrix3D          ;
    current_tex            : TTexture           ;
    current_opc            : Single             ;
    current_lght           : Boolean            ;
    current_cntx           : TContext3D         ;
    wndWdth                : Integer            ;
    wndHght                : Integer            ;
    lasEx3D                : TGIS_Extent3D      ;
    lasCell                : Double             ;
    dist                   : Double             ;
    org                    : TGIS_Point3D       ;
    pntNo                  : Integer            ;

  private
    procedure fset_Light    ( const _value : TLight ) ;

    procedure fset_Camera   ( const _value : TCamera ) ;

    /// <summary>
    ///   Release Textures in labelInfo and set it's length to 0.
    /// </summary>
    procedure releaseLabelInfo       ;

    /// <summary>
    ///   Release vertex buffers.
    /// </summary>
    procedure releaseDataVB          ; override;

    /// <summary>
    ///   Set buffers length and create buffers.
    /// </summary>
    function  setBuffers             : Integer ;

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
    function  createVertexBuffers    ( const _vertexNum : DWORD ;
                                       out _vbuf        : TVertexBuffer ;
                                       out _ibuf        : TIndexBuffer
                                     ) : Integer;

    /// <summary>
    ///   Set Light parameters.
    /// </summary>
    procedure initLight              ;

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
    function  drawMultiPatchFromCache
                                     ( const _shp   : TGIS_Shape ;
                                       var   _lbz   : Double     ;
                                       var   _lbb   : Double
                                     ) : Boolean ;

    /// <summary>
    ///   Load texture from given TGIS_Bitmap.
    /// </summary>
    /// <param name="_bmp">
    ///   TGIS_Bitmap to be used to create texture
    /// </param>
    /// <param name="_detail">
    ///   True - detail texture, False - rough texture
    /// </param>
    function  loadTexture            ( const _bmp    : TGIS_Bitmap ;
                                       const _detail : Boolean
                                     ) : Integer ;

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
    ///   Save color triangle list
    /// </summary>
    /// <param name="_part">
    ///   True means Roof, False means Wall
    /// </param>
    /// <param name="_buf">
    ///   buffer storing triangles to be moved to mesh
    /// </param>
    function  saveVTLBufC            ( const _part : Boolean       ;
                                       var   _buf  : TGIS_Renderer3DColorTriangleListBuffer
                                     ) : Integer ; override;

    /// <summary>
    ///   Save and draw textured triangle list from buffer.
    /// </summary>
    function  saveVTLBufT            ( const _part : Boolean       ;
                                       var   _buf  : TGIS_Renderer3DTextureTriangleListBuffer
                                     ) : Integer ;

    /// <summary>
    ///   Create &amp; Add single mesh tile for color Vector (from triangleList).
    /// </summary>
    /// <param name="_tri">
    ///   number of triangles to be stored as faces
    /// </param>
    function  addVectMeshTileC       ( var   _count   : Integer              ;
                                       var   _mesh    : T_arMesh             ;
                                       var   _buf     : TGIS_Renderer3DColorTriangleListBuffer ;
                                       const _tri     : Integer
                                     ) : Integer ;
    /// <summary>
    ///   Create &amp; Add single mesh tile for textured Vector (from
    ///   triangleList).
    /// </summary>
    /// <param name="_tri">
    ///   number of triangles to be stored as faces
    /// </param>
    function  addVectMeshTileT       ( var   _count   : Integer              ;
                                       var   _mesh    : T_arMesh             ;
                                       var   _buf     : TGIS_Renderer3DTextureTriangleListBuffer;
                                       const _tri     : Integer
                                     ) : Integer ;

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
    procedure addToMPatchBufT        ( const _ptg1 : TGIS_Renderer3DVertex ;
                                       const _ptg2 : TGIS_Renderer3DVertex ;
                                       const _ptg3 : TGIS_Renderer3DVertex ;
                                       const _subs : Integer
                                     ) ;

    /// <summary>
    ///   Save and Draw textured triangle list from buffer.
    /// </summary>
    function  saveMPatchBufT         : Integer ;

    /// <summary>
    ///   Draw color triangle list from buffer.
    /// </summary>
    function  saveMPatchBufC         : Integer ; override;

    /// <summary>
    ///   Draw textured triangle list from buffer.
    /// </summary>
    function  saveTinBufT            : Integer ; override;

    /// <summary>
    ///   Create &amp; Add single mesh tile for TIN (from triangleList).
    /// </summary>
    /// <param name="_tri">
    ///   number of triangles to be stored as faces
    /// </param>
    function  addTinMeshTileC        ( const _tri     : Integer
                                     ) : Integer ; override;

    /// <summary>
    ///   Create &amp; Add single mesh tile for TIN (from triangleList).
    /// </summary>
    /// <param name="_tri">
    ///   number of triangles to be stored as faces
    /// </param>
    function  addTinMeshTileT        ( const _tri     : Integer
                                     ) : Integer ; override;

    /// <summary>
    ///   Draw all previously stored mesh TIN tiles.
    /// </summary>
    procedure drawMeshTinTiles       ;

    /// <summary>
    ///   Scale mesh Z coordinate by given value.
    /// </summary>
    /// <param name="_value">
    ///   scale factor
    /// </param>
    procedure scaleMeshTilesZ        ( const _value : Double
                                     ) ; override;

    /// <summary>
    ///   Scale mesh M coordinate by given value.
    /// </summary>
    /// <param name="_value">
    ///   scale factor
    /// </param>
    procedure scaleMeshTilesM        ( const _value : Double
                                     ) ; override;

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
    function  mapToScreen            ( const _ptg : TGIS_Point
                                     ) : TPoint ;

    /// <summary>
    ///   Get screen coordinates of map point at Z.
    /// </summary>
    /// <param name="_ptg">
    ///   point position (map units)
    /// </param>
    /// <param name="_z">
    ///   z value in 3D units
    /// </param>
    function  mapToScreen2           ( const _ptg : TGIS_Point ;
                                       const _z   : Double
                                     ) : TPoint ; override;

    /// <summary>
    ///   Get 3D coordinates of screen pixel.
    /// </summary>
    /// <param name="_pt">
    ///   screen pixel position
    /// </param>
    function  screenTo3D             ( const _pt : TPoint
                                     ) : TGIS_Point ; override;

  protected
    /// <inheritdoc/>
    procedure doDestroy              ; override;

    /// <inheritdoc/>
    procedure prepareVertexBuffersToDraw ; override;

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
    function  setD3D_Data            : Integer ; override;

    /// <inheritdoc/>
    procedure freeMeshStore          ; override;

    /// <inheritdoc/>
    function  transformSetting       : Integer ; override;

    /// <inheritdoc/>
    procedure adjustTransformSetting ( _value : Boolean
                                     ) ; override;

    /// <inheritdoc/>
    procedure makeBasementTex        ; override;

    /// <inheritdoc/>
    procedure drawBasement           ; override;

    /// <inheritdoc/>
    procedure makeSkyBoxTex          ; override;

    /// <inheritdoc/>
    procedure drawSkyBox             ; override;

    /// <inheritdoc/>
    procedure makeWallTexture        ; override;

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
    function  northArrow             ( const _value : Boolean
                                     ) : Integer ; override;

    /// <inheritdoc/>
    function  doRender               ( const _rows     : DWORD ;
                                       const _detail   : Boolean
                                     ) : Integer ; override;

    /// <inheritdoc/>
    function  saveWTLBufT            : Integer ; override;

    /// <inheritdoc/>
    function  triangleNormal         ( const _v0 : TVector3f ;
                                       const _v1 : TVector3f ;
                                       const _v2 : TVector3f
                                     ) : TVector3f ; override;

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
    function  getWindowSize          : TPoint ; override;

    /// <inheritdoc/>
    function  getTimeTic             : DWORD ; override;

    /// <inheritdoc/>
    procedure lightOn                ; override;

    /// <inheritdoc/>
    procedure lightOff               ; override;

    /// <inheritdoc/>
    procedure clearScreen            ; override;

    /// <inheritdoc/>
    procedure blendOn                ; override;

    /// <inheritdoc/>
    procedure blendOff               ; override;

    /// <inheritdoc/>
    procedure blitToWindow           ; override;

    /// <inheritdoc/>
    procedure setAmbientLightColor   ( const _val  : Integer
                                     ) ; override;

    /// <inheritdoc/>
    procedure drawLabels             ; override;

    /// <summary>
    ///   Draw label from buffer.
    /// </summary>
    /// <param name="_k">
    ///   label number in labelInfo array
    /// </param>
    procedure drawLabel              ( const _k     : Integer
                                     ) ;

    /// <inheritdoc/>
    procedure markSelShape           ( const _layer : TGIS_Layer ;
                                       const _shpid : Integer    ;
                                       const _part  : Integer    ;
                                       const _color : TGIS_Color ;
                                       const _mode  : Boolean    ;
                                       const _update: Boolean
                                     ) ; override;

    /// <inheritdoc/>
    function  reverseValue          ( const _value : Single
                                    ) : Single ; override;

    /// <inheritdoc/>
    function pdataVB_Unlock      ( var _strm : PByte        ;
                                   var _data : T_arTextureData
                                 ) : Integer ; override;

    /// <inheritdoc/>
    function cdataVB_Unlock      ( var _strm : PByte        ;
                                   var _data : T_arColorData
                                 ) : Integer ; override;

    /// <inheritdoc/>
    function pdataVB_Lock        ( var _strm : PByte
                                 ) : Integer ; override;

    /// <inheritdoc/>
    function cdataVB_Lock        (  var _strm : PByte
                                 ) : Integer ; override;

    /// <inheritdoc/>
    procedure reset3DEnvironment     ; override;

    /// <inheritdoc/>
    function  restore3DEnvironment   : Integer ; override;


  public //  constructors

    /// <inheritdoc/>
    constructor Create               ; override;

    /// <inheritdoc/>
    function  Init3D                 : Boolean ; override;

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
    function  Draw                   : Integer ; override;

    /// <inheritdoc/>
    procedure SetSunPosition         ( const _value : TGIS_Point
                                     ) ; override;

    /// <inheritdoc/>
    function  GetUniverseColor       : TGIS_Color ; override;

    /// <inheritdoc/>
    procedure SetUniverseColor       ( const _value         : TGIS_Color
                                     ) ; override;

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

    /// <summary>
    ///   Store current Context.
    /// </summary>
    /// <param name="_value">
    ///   current context3D
    /// </param>
    procedure BeginContext           ( const _value : TContext3D ) ;

    /// <summary>
    ///   Calculates camera position for current scene.
    /// </summary>
    /// <returns>
    ///   Current camera position in 3D units.
    /// </returns>
    function  GetNewCameraPosition3DEx : TGIS_Point3D ;

    /// <summary>
    ///   Set Camera parameters for current scene rendering.
    /// </summary>
    procedure SetCameraParams        ;


  public

    /// <summary>
    ///   Current light object.
    /// </summary>
    property Light   : TLight      write fset_Light ;

    /// <summary>
    ///   Current camera object.
    /// </summary>
    property Camera  : Tcamera     write fset_Camera ;
  end ;

//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoTessellation ;

type
 PDWORD = ^DWORD ;

   var kkk : TPixelFormat ;

type
  TMesh = class ( TGIS_ObjectDisposable )
    private
      format    : TVertexFormats ;
      faces     : array of Integer ;
      tvertices : array of TGIS_Renderer3DVertex ;
      cvertices : array of TGIS_Renderer3DVertex   ;
      indices   : array of DWORD ;
      subsets   : array of DWORD ;
      pdataVBm,
      cdataVBm  : TVertexBuffer ;
      idataIBm  : TIndexBuffer  ;
      FAppend   : Boolean ;
      FSaved    : Boolean ;
      FNumVert  : DWORD ;
      FNumFace  : DWORD ;
      rnd       : TGIS_Renderer3DFMX ;

    protected
      procedure doDestroy              ; override;

      function pdataVB_Lock            ( var _strm : PByte
                                       ) : Integer ;
      function cdataVB_Lock            ( var _strm : PByte
                                       ) : Integer ;
      function pdataVB_Unlock          ( var _strm : PByte        ;
                                         var _data : T_arTextureData
                                       ) : Integer ;
      function cdataVB_Unlock          ( var _strm : PByte        ;
                                         var _data : T_arColorData
                                       ) : Integer ;

    public
      constructor Create               ( const _faces    : DWORD ;
                                         const _vertices : DWORD ;
                                         const _format   : TVertexFormats ;
                                         const _pvb      : TVertexBuffer ;
                                         const _cvb      : TVertexBuffer ;
                                         const _iib      : TIndexBuffer ;
                                         const _rnd      : TGIS_Renderer3DFMX
                                       ) ;

      function  GetNumFaces            : DWORD ;
      function  GetNumVertices         : DWORD ;
      procedure SortIndices            ( const _num      : Integer
                                       ) ;
      procedure DrawSubset             ( const _attribId : DWORD
                                       ) ;
      function  LockVertexBuffer       ( const Flags     : DWORD;
                                         out ppData : Pointer
                                       ) : Integer ;
      function  UnlockVertexBuffer     : Integer ;
      function  LockIndexBuffer        ( const Flags     : DWORD;
                                         out ppData : Pointer
                                       ) : Integer ;
      function  UnlockIndexBuffer      : Integer ;
      function  LockAttributeBuffer    ( const Flags     : DWORD;
                                         out ppData : PDWORD
                                       ) : Integer ;
      function  UnlockAttributeBuffer  : Integer ;

    public
      property Append                  : Boolean read  FAppend
                                                 write FAppend ;
      property Saved                   : Boolean read  FSaved
                                                 write FSaved  ;
      property NumVert                 : DWORD   read  FNumVert
                                                 write FNumVert ;
      property NumFace                 : DWORD   read  FNumFace
                                                 write FNumFace ;

  end;

var
  oLabelsReg   : TGIS_LabelsArea  ;

type
  // GDB file material info
  T_mtrlInfo = record
    // material ID
    MtrlId   : Integer ;
    // Bitmap checksum
    CheckSum : Integer ;
    // corresponding texture
    TexDX  : TTexture  ;
  end ;

  // Array of T_mtrlInfo
  T_arMtrlInfo = array of T_mtrlInfo ;

type

  // Helper class to create and keep list of textures based on provided bitmap.
  // Internally keep list of T_textureHelperItem objects. FMX version
  T_textureHelperFX = class ( TGIS_BaseObjectDisposable )
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
      // _bmp  source bitmap
      // return texture
      function  Prepare( const _bmp : TGIS_Bitmap
                       ) : String ;

      // Get previously created texture.
      // _hash  hash code
      // return texture
      function  Get    ( const _hash : String
                       ) : TTexture ;

      // Clear list of managed texture - free all textures.
      procedure Clear  ;
  end ;

  // Single item in a T_textureHelper list.
  T_textureHelperItemFX = class ( TGIS_BaseObjectDisposable )
     private
       oTexture : TTexture ;

     protected
       //  Destroy an object
       procedure doDestroy ; override;
  end;

  // Helper class to create and keep list of colors to make color materials.
  T_colorHelperFX = class ( TGIS_BaseObjectDisposable )
    private
      oLst : TStringList  ;

    protected
      // Destroy an instance.
      procedure doDestroy ; override;

    public
      // Create an instance
      constructor Create ;
      // Add color to list of colors
      // _color added color
      procedure AddColor( const _color : Cardinal ) ;
      // Get color position in color list
      // _color searched color
      // return position of color in  color list
      function  Get    ( const _color : Cardinal
                       ) : Integer ;
      // Clear list of stored colors.
      procedure Clear  ;
  end;

  // Single item in a T_colorHelperFX list.
  T_colorHelperItemFX = class ( TGIS_BaseObjectDisposable )
     private
       position : Integer ;

     protected
       //  Destroy an object
       procedure doDestroy ; override;
  end;


  function LabelsReg : TGIS_LabelsArea ;
  begin
    if not Assigned( oLabelsReg ) then
      oLabelsReg := TGIS_LabelsArea.Create( nil ) ;

    Result := oLabelsReg ;
  end ;

  // TVector3f record initialization (portable against different platforms).
  function vector3Init(
    const _x : Single ;
    const _y : Single ;
    const _z : Single
  ) : TVector3f ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  begin
    Result.x := _x ;
    Result.y := _y ;
    Result.z := _z ;
  end;

  function Vec3Subtract(
    out   vOut : TVector3f;
    const v1   : TVector3f ;
    const v2   : TVector3f
  ): TVector3f;
  begin
    with vOut do
    begin
      x:= v1.x - v2.x;
      y:= v1.y - v2.y;
      z:= v1.z - v2.z;
    end;
    Result:= vOut;
  end;

  function Vec3Cross(
    out   vOut : TVector3f;
    const v1   : TVector3f ;
    const v2   : TVector3f
  ): TVector3f;
  begin
    vOut.x:= v1.y * v2.z - v1.z * v2.y;
    vOut.y:= v1.z * v2.x - v1.x * v2.z;
    vOut.z:= v1.x * v2.y - v1.y * v2.x;
    Result:= vOut;
  end;

  function Vec3Dot(
    const v1, v2: TVector3f
  ): Single;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z ;
  end;

  function Vec3Normalize(
    out   vOut        : TVector3f         ;
    const v           : TVector3f
  ): TVector3f ;
  var
    l : Double ;
  begin
    l := Sqrt(Sqr(v.x) + Sqr(v.y) + Sqr(v.z)) ;
    if l = 0 then begin
      vOut.y := 1 ;
      Result := vOut ;
      exit;
    end;
    vOut.x := v.x / l ;
    vOut.y := v.y / l ;
    vOut.z := v.z / l ;
    Result := vOut ;
  end;

  function Vec3TransformCoord(
    out   vOut        : TVector3f         ;
    const v           : TVector3f         ;
    const m           : TMatrix3D
  ) : TVector3f;
  var
    x, y, z, w : Single ;
  begin
    x := v.x * m.m11 + v.y * m.m21 + v.z * m.m31 + m.m41 ;
    y := v.x * m.m12 + v.y * m.m22 + v.z * m.m32 + m.m42 ;
    z := v.x * m.m13 + v.y * m.m23 + v.z * m.m33 + m.m43 ;
    w := v.x * m.m14 + v.y * m.m24 + v.z * m.m34 + m.m44 ;

    vOut.x := x / w;
    vOut.y := y / w;
    vOut.z := z / w;
    Result := vOut ;
  end;

  function Vec3Project(
    out   vOut        : TVector3f         ;
    const v           : TVector3f         ;
    const ctx         : TContext3D        ;
    const mat         : TMatrix3D         ;
    const wdth        : Integer           ;
    const hght        : Integer
  ): TVector3f;
  var
    vec    : TVector3f ;
  begin
    Vec3TransformCoord(vec, v, mat) ;
    vOut.x := ( 1.0 + vec.x ) * wdth / 2.0 ;
    vOut.y := ( 1.0 - vec.y ) * hght / 2.0 ;
    vOut.z := vec.z ;
    Result := vOut ;
  end;

  function Vec3Unproject(
    out   vOut        : TVector3f         ;
    const v           : TVector3f         ;
    const ctx         : TContext3D        ;
    const mProjection : TMatrix3D         ;
    const mView       : TMatrix3D         ;
    const mWorld      : TMatrix3D         ;
    const wdth        : Integer           ;
    const hght        : Integer
  ): TVector3f;
  var
    m1, m2, m3 : TMatrix3D ;
    vec    : TVector3f ;
  begin
    m1 := mWorld * mView ;
    m2 := m1 * mProjection ;
    m3 := m2.Inverse ;

    vec.x := 2.0 * ( v.x - 0 ) / wdth - 1.0 ;
    vec.y := 1.0 - 2.0 * ( v.y - 0 ) / hght ;
    vec.z := ( v.z - 0) / ( 1 );

    Vec3TransformCoord(vOut, vec, m3);

    Result := vOut ;
  end;

  function  IntersectTri(
    const _v0, _v1, _v2 : TVector3f ;
    const _orig, _dir  : TPoint3D ;
    out _u, _v, _d : Single
  ) : Boolean ;
  var
    edge1 : TVector3f ;
    edge2 : TVector3f ;
    pvec  : TVector3f ;
    det   : Single ;
    tvec  : TVector3f ;
    qvec  : TVector3f ;
    fInvDet : Single ;
  begin
    // Find vectors for two edges sharing vert0
    Vec3Subtract(edge1, _v1, _v0);
    Vec3Subtract(edge2, _v2, _v0);

    // Begin calculating determinant - also used to calculate U parameter
    Vec3Cross(pvec, TVector3f(_dir), edge2);

    // If determinant is near zero, ray lies in plane of triangle
    det:= Vec3Dot(edge1, pvec);
    if (det > -0.00001) and (det < 0.00001) then
    begin
      Result:= False;
      exit;
    end;

    // Calculate distance from vert0 to ray origin
    Vec3Subtract(tvec, TVector3f(_orig), _v0);

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
    _v:= Vec3Dot(TVector3f(_dir), qvec) * fInvDet ;
    if (_v < 0.0) or (_u + _v > 1.0) then
    begin
      Result:= False ;
      exit;
    end;

    // Calculate t, scale parameters, ray intersects triangle
    _d := vec3Dot(edge2, qvec);
    fInvDet:= 1.0 / det ;
    _d := _d * fInvDet ;

    Result:= True ;
  end;

  function PlaneFromPointNormal(
    out   pOut        : TGIS_Point3D      ;
    const vPoint      : TVector3f         ;
    const vNormal     : TVector3f
  ): TGIS_Point3D;
  begin
    pOut.x := vNormal.x ;
    pOut.y := vNormal.y ;
    pOut.z := vNormal.z ;
    pOut.m := -Vec3Dot(vPoint, vNormal) ;

    Result := pOut ;
  end;

  function PlaneIntersectLine(
    out   pOut        : TVector3f         ;
    const p           : TGIS_Point3D      ;
    const v1          : TVector3f         ;
    const v2          : TVector3f
  ): TVector3f;
  var
    d, n : TVector3f ;
    dp, tv : Single ;
  begin
    n.x := p.x ;
    n.y := p.y ;
    n.z := p.z ;
    d.x := v2.x -v1.x ;
    d.y := v2.y -v1.y ;
    d.z := v2.z -v1.z ;
    dp  := Vec3Dot(n, d) ;
    if (Abs(dp) < 0.0001) then exit ;    // Line and plane don't intersect

    tv := ( p.m + Vec3Dot(n, v1) ) / dp ;
    pOut.x := v1.x - tv * d.x ;
    pOut.y := v1.y - tv * d.y ;
    pOut.z := v1.z - tv * d.z ;

    Result := pOut ;
  end;


  function CreateMeshFVF(
    NumFaces    : DWORD              ;
    NumVertices : DWORD              ;
    Options     : DWORD              ;
    FVF         : TVertexFormats     ;
    out   ppMesh: TMesh              ;
    pvb         : TVertexBuffer      ;
    cvb         : TVertexBuffer      ;
    iib         : TIndexBuffer       ;
    rnd         : TGIS_Renderer3DFMX
  ): Integer;
  begin
    ppMesh := TMesh.Create( NumFaces, NumVertices, FVF, pvb, cvb, iib, rnd ) ;
    if Assigned( ppMesh ) then
      Result := S_OK
    else
      Result := S_False ;
  end;

  // Create a texture base on provided bitmap object.
  // _bmp  source bitmap
  // return texture
  function create_texture(
    const _bmp : TGIS_Bitmap
  ) : TTexture ;
  var
    pix  : TGIS_Pixels ;
    strm : TMemoryStream ;
    f : TPixelFormat ;
  begin
    Result := TTexture.Create ;
    Result.SetSize( _bmp.Width, _bmp.Height ) ;
    Result.Style := [TTextureStyle.MipMaps, TTextureStyle.Dynamic] ;

    {$IFDEF NEXTGEN}
      { TODO -cReview : verify on iOS and ANDROID }
      _bmp.LockPixels( pix, False, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
      try
        Result.UpdateTexture(@pix[0], _bmp.Width * 4);
      finally
        _bmp.UnlockPixels ;
      end;
    {$ELSE}
    if kkk = TPixelFormat.BGRA then
      _bmp.LockPixels( pix, False, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native )
    else
      _bmp.LockPixels( pix, False, TGIS_BitmapFormat.ABGR, TGIS_BitmapLinesOrder.Native ) ;

      Result.PixelFormat := TPixelFormat.BGRA ;
      try
        Result.UpdateTexture(@pix[0], _bmp.Width * 4);
      finally
        _bmp.UnlockPixels ;
      end;
    {$ENDIF}
  end;

  // Create a texture base on provided TGIS_Material object. Properly manage
  // transparent bitmaps.
  // _mat  source material
  // return texture
  function create_texture1(
    const _mat : TGIS_Material
  ) : TTexture ;
  var
    r, g, b : Byte ;
    r1, g1, b1 : Byte ;
    i, k : Integer ;
    strm : TMemoryStream ;
    bmp  : TGIS_Bitmap ;
    pix  : TGIS_Pixels ;
    cl   : TGIS_Color ;
  begin
    strm := TMemoryStream.Create  ;
    strm.Write( _mat.Buffer, _mat.Size ) ;
    strm.Position := 0 ;
    bmp := TGIS_Bitmap.Create ;
    bmp.LoadFromStream( strm ) ;
    if (_mat.Bpp = 4) then begin
      r := _mat.Color.R ;
      g := _mat.Color.G ;
      b := _mat.Color.B ;
      k := bmp.Width * bmp.Height ;
      bmp.LockPixels( pix, True, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
      for i := 0 to k -1 do begin
        cl.ARGB := Cardinal(pix[i]) ;
        r1 := cl.R ;
        g1 := cl.G ;
        b1 := cl.B ;
        if (r1=r) and (g1=g) and (b1=b) then begin
          cl :=  TGIS_Color.FromARGB( 0, 0, 0, 0 ) ;
          pix[i] := Integer(cl.ARGB) ;
        end
      end;
      bmp.UnlockPixels ;
    end;

    Result := create_texture( bmp ) ;
    FreeObject( bmp ) ;
    FreeObject( strm ) ;
  end;

  // Create a texture base on provided material object.
  // _mat  source material
  // return texture
  function create_texture2(
      const _mat : TGIS_Material
    ) : TTexture ;
  begin
    Result := TTexture.Create ;
    Result.SetSize( _mat.Width, _mat.Height ) ;
    Result.Style := [TTextureStyle.MipMaps, TTextureStyle.Dynamic] ;
    Result.UpdateTexture(@_mat.Buffer[0], _mat.Width * 4) ;
  end;

//==============================================================================
// T_textureHelperFX
//==============================================================================

  constructor T_textureHelperFX.Create ;
  begin
    inherited ;

    oLst := TStringList.Create ;
    oLst.Sorted := true ;
  end ;

  procedure T_textureHelperFX.doDestroy ;
  begin
    Clear ;
    FreeObject( oLst ) ;

    inherited ;
  end;

  function T_textureHelperFX.Prepare(
    const _bmp : TGIS_Bitmap
  ) : String ;
  var
    bmp  : TGIS_Bitmap ;
    hash : String  ;
    idx  : Integer ;
    elm  : T_textureHelperItemFX ;
    tex  : TTexture  ;
  begin
    Result := '' ;

    if not Assigned( _bmp ) then exit ;

    bmp := _bmp ;
    hash := IntToStr( NativeInt( Pointer( bmp.NativeBitmap ) ) ) ;

    if oLst.Find( hash, idx ) then begin
      Result := hash ;
    end
    else begin
      tex := create_texture( bmp ) ;
      if Assigned( tex ) then
        Result := hash
      else
        Result := '' ;
      Assert( Result <> '') ;
      elm := T_textureHelperItemFX.Create ;
      elm.oTexture := tex ;
      oLst.AddObject( hash, elm ) ;
    end ;
  end ;

  function  T_textureHelperFX.Get(
    const _hash : String
  ) : TTexture ;
  var
    idx  : Integer ;
    elm  : T_textureHelperItemFX ;
  begin
    Result := nil ;
    if oLst.Find( _hash, idx ) then begin
      elm := T_textureHelperItemFX( oLst.Objects[ idx ] ) ;
      Result := elm.oTexture ;
    end;
  end;

  procedure T_textureHelperFX.Clear ;
  var
    idx : Integer             ;
    elm : T_textureHelperItemFX ;
  begin
    for idx := 0 to  oLst.Count -1 do begin
      elm := T_textureHelperItemFX( oLst.Objects[ idx ] ) ;
      Assert( Assigned( elm ) ) ;

      Assert( Assigned( elm.oTexture ) ) ;
      FreeObject( elm ) ;
    end;
    oLst.Clear ;
  end;

//==============================================================================
// T_textureHelperItemFX
//==============================================================================

  procedure T_textureHelperItemFX.doDestroy ;
  begin
    // force interface release
    FreeObject( oTexture ) ;

    inherited ;
  end;

//==============================================================================
// T_colorHelperFX
//==============================================================================

  constructor T_colorHelperFX.Create ;
  begin
    inherited ;

    oLst := TStringList.Create ;
    oLst.Sorted := True ;
  end ;

  procedure T_colorHelperFX.doDestroy ;
  begin
    Clear ;
    FreeObject( oLst ) ;

    inherited ;
  end;

  procedure T_colorHelperFX.AddColor(
    const _color : Cardinal
  )  ;
  var
    cl  : String ;
    idx : Integer ;
    elm  : T_colorHelperItemFX ;
  begin
    cl := UIntToStr(_color) ;
    if oLst.Find( cl, idx ) then
      exit
    else begin
      elm := T_colorHelperItemFX.Create ;
      elm.position := oLst.Count ;
      oLst.AddObject( cl, elm ) ;
    end;
  end;

  function T_colorHelperFX.Get(
    const _color : Cardinal
  ) : Integer ;
  var
    cl  : String ;
    idx : Integer ;
    elm : T_colorHelperItemFX ;
  begin
    cl := UIntToStr(_color) ;
    if oLst.Find( cl, idx ) then begin
      elm := T_colorHelperItemFX( oLst.Objects[ idx ] ) ;
      Result := elm.position ;
    end
    else
      Result := 0 ;
  end;

  procedure T_colorHelperFX.Clear  ;
  var
    idx : Integer             ;
    elm : T_colorHelperItemFX ;
  begin
    for idx := 0 to  oLst.Count -1 do begin
      elm := T_colorHelperItemFX( oLst.Objects[ idx ] ) ;
      Assert( Assigned( elm ) ) ;
      FreeObject( elm ) ;
    end;
    oLst.Clear ;
  end;

//==============================================================================
// T_colorHelperItemFX
//==============================================================================

procedure T_colorHelperItemFX.doDestroy ;
  begin

    inherited ;
  end;


//==============================================================================
// TGIS_Renderer3DFMX
//==============================================================================

  constructor TGIS_Renderer3DFMX.Create ;
  begin
    inherited;

    oContext     := nil ;
    color_mtrl   := TColorMaterial.Create ;
    texture_mtrl := TTextureMaterial.Create ;
    light_mtrl   := TLightMaterial.Create ;
    printBmpOutSize := Point( 0, 0 ) ;
    printBmpImgSize := 0 ;

  end;

  function TGIS_Renderer3DFMX.Init3D : Boolean ;
  begin
    oPolyRoof := TGIS_Tessellation.Create ;
    if oPolyRoof = nil then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                   'InitD3D', 4
                                 ) ;
    end;
    oTextureHelper := T_textureHelperFX.Create ;
    oColorHelper   := T_colorHelperFX.Create ;
    current_tex := nil ;
    current_opc := 1.0 ;
    Result := True ;
  end;

  procedure TGIS_Renderer3DFMX.SetInitValues(
    const _cutextent     : TGIS_Extent ;
    const _visibleextent : TGIS_Extent ;
    const _gis           : IGIS_Viewer ;
    const _viewer        : IGIS_Viewer3D
  ) ;
  var
    dx         : Double      ;
    wrkextent  : TGIS_Extent ;
    dbl        : Double      ;
    excolumns ,
    exrows     : Integer     ;
  begin
    inherited ;

    ambColorDX := color2ARGB(255,
                             TruncS( shadowsLevel*255 ),
                             TruncS( shadowsLevel*255 ),
                             TruncS( shadowsLevel*255 )) ;
    ambColor   := ambColorDX ;
    FreeObject( pdataVB    ) ;
    FreeObject( cdataVB    ) ;
    FreeObject( idataIB    ) ;
    FreeObject( pdataTEX_d ) ;
    FreeObject( pdataTEX_r ) ;
    FreeObject( pdataTEX_w ) ;
    FreeObject( pdataTEX_vDX ) ;
    FreeObject( pdataTEX_tDX ) ;

    backColor := color2ARGB(255, 153, 217, 234) ;
    borderColor := color2ARGB(255, oGIS.Color.R, oGIS.Color.G, oGIS.Color.B) ;

    // videoVertexBufferSize must be a multiplication of number 6 !!!
    videoVertexBufferSize := 1*initVideoVBSize ;
    if videoVertexBufferSize mod 6 <> 0 then
      videoVertexBufferSize := TruncS(videoVertexBufferSize / 6) * 6 ;

    // Create Vertex buffer
    setBuffers ;

    // Create Light
    initLight ;

    if Assigned( oLight ) then
    if oLight.Enabled = True then
      lightSwitch := True ;

    current_lght := lightSwitch ;
    cameraPosition.Y       := 180 ;
    setLeftColumn ;

    // Create additional vector buffers
{    SetLength( vTLBufCR.TriLst   ,  videoVertexBufferSize ) ;
    SetLength( vTLBufCW.TriLst   ,  videoVertexBufferSize ) ;
    SetLength( vTLBufTR.TriLst   ,  videoVertexBufferSize ) ;
    SetLength( vTLBufTR.Subset   ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
    SetLength( vTLBufTW.TriLst   ,  videoVertexBufferSize ) ;
    SetLength( vTLBufTW.Subset   ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
    SetLength( wTLBufT.TriLst    ,  videoVertexBufferSize ) ;
    SetLength( tinBufT.TriLst    ,  videoVertexBufferSize ) ;
    SetLength( tinBufC.TriLst    ,  videoVertexBufferSize ) ;
    SetLength( mpatchBufC.TriLst ,  videoVertexBufferSize ) ;
    SetLength( mpatchBufT.TriLst ,  videoVertexBufferSize ) ;
    SetLength( mpatchBufT.Subset ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
    SetLength( wPLBuf.PntLst     ,  videoVertexBufferSize ) ;
    SetLength( wPBuf.PntLst      ,  videoVertexBufferSize ) ;}
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
    renderer3D             := [TRenderer3D.FMX] ;

  end;

  function  TGIS_Renderer3DFMX.Select3DObject(
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
    shpid                   : Integer ;
    vPickRayEnd             : TVector3f ;
    pi0, pj0, pbt           : Integer ;
    pd0, testval, testlen   : Single  ;
    isendpoint              : Boolean ;
    lastd0                  : Single    ;
    lasti0, lastj0, lastbt  : Integer   ;
    lastptg                 : TGIS_Point3D ;
    layer                   : TGIS_Layer ;
    vb                      : P_textureVertex ;
    vc                      : P_colorVertex   ;
    skip                    : Boolean ;

      procedure getRay(const _pt : TPoint ;
                       out   _pp : TVector3f ;
                       out   _pk : TVector3f  ) ;
      var
        mwi, mvi : TMatrix3D  ;
        v        : TVector3f  ;

      begin
        // Compute the vector of the pick ray in screen space
        v.x :=   ( ( ( 2.0 * _pt.X ) / oContext.Width  ) - 1 ) / mtxProjection.m11 ;
        v.y :=   ( ( ( 2.0 * _pt.Y ) / oContext.Height ) - 1 ) / mtxProjection.m22 ;
        v.z :=  1.0 ; // ZFactor ;

        mwi := mtxWorld.Inverse ;
        mvi := mtxView.Inverse ;

        _pp.x := mvi.m41 ;
        _pp.y := mvi.m42 ;
        _pp.z := mvi.m43 ;

        Vec3TransformCoord ( _pp, _pp, mwi) ;
        // Transform the screen space pick ray into 3D space
        _pk.x := v.x*mwi.m11 - v.y*mwi.m21 - v.z*mwi.m31 ;
        _pk.y := v.x*mwi.m12 - v.y*mwi.m22 - v.z*mwi.m32 ;
        _pk.z := v.x*mwi.m13 - v.y*mwi.m23 - v.z*mwi.m33 ;

        Vec3Normalize( _pk, _pk ) ;
      end;

      // Calculate the shortest route between line P1P2 and point P.
      // Return False if distance greater than precision.

      function linepoint3D( const _p1 : TVector3f ; const _p2 : TVector3f ;
                            const _p  : TVector3f ; var   _ds : Single
                          ) : Boolean ;
      var
        ab, ap, bp, s, t, dst : Double ;
      begin
        Result := False ;
        if delta = 0 then exit;
        if not isendpoint then exit;

        ab := Sqrt(Sqr(_p1.x-_p2.X) + Sqr(_p1.Y-_p2.Y) + Sqr(_p1.Z-_p2.Z)) ;
        ap := Sqrt(Sqr(_p1.x-_p.X ) + Sqr(_p1.Y-_p.Y ) + Sqr(_p1.Z-_p.Z )) ;
        bp := Sqrt(Sqr(_p2.x-_p.X ) + Sqr(_p2.Y-_p.Y ) + Sqr(_p2.Z-_p.Z )) ;
        s  := (ab + ap + bp) / 2 ;
        t  := Sqrt(s*(s-ab)*(s-ap)*(s-bp)) ;
        dst := 2*t/ab ;
        if dst > delta then exit ;
        _ds := dst ;
        Result := True ;
      end;

      function intersect( const _p1 : TVector3f ; const _p2 : TVector3f ;
                          const _p3 : TVector3f ; const _i, _j : Integer
                        ) : Boolean ;
      begin
        Result := False ;
        if IntersectTri( _p1, _p2, _p3, vPickRayOrig, vPickRayDir ,
                              pu,  pv, d ) then begin

              if skip = False then // DEM specific
                if(Abs(_p1.x)=100) then
                  if(Abs(_p2.x)=100) then
                    if(Abs(_p3.x)=100) then exit;

              if dd0 > d then
                dd0 := d ;
              if d <= dd0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                pw := 1 - ( pu + pv ) ;

                _ptg.x := pw * _p1.x + pu * _p2.x + pv * _p3.x ;
                _ptg.y := pw * _p1.y + pu * _p2.y + pv * _p3.y ;
                _ptg.z := pw * _p1.z + pu * _p2.z + pv * _p3.z ;

                _ptg.M := d0 ;
                Result := True ;
                exit ;
              end;
         end;
         if dd0 < testval then  // IntersectTri positive
           exit ;
         if linepoint3D(TVector3f(vPickRayOrig), TVector3f(vPickRayEnd), _p1, d) then begin
              if d <= d0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                _ptg.x := _p1.x ;
                _ptg.y := _p1.y ;
                _ptg.z := _p1.z ;
                _ptg.M := d0 ;
                Result := True ;
              end;
         end;
         if linepoint3D(TVector3f(vPickRayOrig), TVector3f(vPickRayEnd), _p2, d) then begin
              if d <= d0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                _ptg.x := _p2.x ;
                _ptg.y := _p2.y ;
                _ptg.z := _p2.z ;
                _ptg.M := d0 ;
                Result := True ;
              end;
         end;
         if linepoint3D(TVector3f(vPickRayOrig), TVector3f(vPickRayEnd), _p3, d) then begin
              if d <= d0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                _ptg.x := _p3.x ;
                _ptg.y := _p3.y ;
                _ptg.z := _p3.z ;
                _ptg.M := d0 ;
                Result := True ;
              end;
         end;
      end;

      function intersect1( const _p1 : TVector3f ; const _p2 : TVector3f ;
                           const _p3 : TVector3f ; const _i, _j : Integer
                         ) : Boolean ;
      begin
        Result := False ;
        if isNAN(vPickRayDir.X) then exit ;

        if IntersectTri( _p1, _p2, _p3, vPickRayOrig, vPickRayDir ,
                              pu,  pv, d ) then begin
              if d <= d0 then begin
                d0 := d ;
                j0 := _j ;
                i0 := _i ;
                pw := 1 - ( pu + pv ) ;

                _ptg.x := pw * _p1.x + pu * _p2.x + pv * _p3.x ;
                _ptg.y := pw * _p1.y + pu * _p2.y + pv * _p3.y ;
                _ptg.z := pw * _p1.z + pu * _p2.z + pv * _p3.z ;

                _ptg.M := d0 ;
                Result := True ;
                exit ;
              end;
         end;
      end;

      function checkVectorColorBuf( const _counter : Integer ;
                                    const _mesh    : T_arMesh ;
                                    const _buf     : TGIS_Renderer3DColorTriangleListBuffer
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
            if intersect(TVector3f(TMesh(_mesh[i]).tvertices[j].P),
                         TVector3f(TMesh(_mesh[i]).tvertices[j+1].P),
                         TVector3f(TMesh(_mesh[i]).tvertices[j+2].P),
                         i, j) then Result := True ;
            j := j + 3 ;
            if j >= numvert then break ;
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
            if intersect(TVector3f( TMesh(_mesh[i]).tvertices[j].P ),
                         TVector3f( TMesh(_mesh[i]).tvertices[j+1].P),
                         TVector3f( TMesh(_mesh[i]).tvertices[j+2].P),
                         i, j) then Result := True ;
            j := j + 3 ;
            if j >= numvert then break ;
          end;
          TMesh(_mesh[i]).UnlockVertexBuffer ;
        end;
      end;

      function checkDemBuf( const _counter : Integer ;
                            const _mesh    : T_arMesh
                          ) : Boolean ;
      var
        i, j : Integer ;
        p1, p2, p3 : TVector3f ;
        mesh : TMesh ;
      begin
        Result := False ;
        if _counter > 0 then
          for i := 0 to _counter -1 do begin
            numvert := TMesh(_mesh[i]).GetNumVertices ;
            mesh := TMesh(_mesh[i]) ;
            j := 0 ;
            while True do begin
               p1 := TVector3f(mesh.tvertices[j  ].P) ;
               p2 := TVector3f(mesh.tvertices[j+1].P) ;
               p3 := TVector3f(mesh.tvertices[j+2].P) ;
               if intersect(p1, p2, p3, i, j) then Result := True ;
              Inc( j ) ;
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
            if intersect(TVector3f(mesh.tvertices[j].P),
                         TVector3f(mesh.tvertices[j+1].P),
                         TVector3f(mesh.tvertices[j+2].P),
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
        i, j, k, l : Integer     ;
        p1, p2, p3 : TVector3f   ;

        function checkIntermediatePoints(     _p1 : TVector3f ;
                                              _p2 : TVector3f ;
                                          var _p3 : TVector3f ;
                                          var  _d : Single
                                        ) : Boolean ;
        var
          len : Double ;
          dx, dy, dz : Double ;
          steps, n : Integer ;
        begin
          Result := False ;
          dx := _p1.x - _p2.x ;
          dy := _p1.y - _p2.y ;
          dz := _p1.z - _p2.z ;
          len := sqrt( sqr(dx) + sqr(dy) + sqr(dz) ) ;
          if len < 2 * delta then exit;

          steps := TruncS( len / delta ) ;
          for n := 1 to steps do begin
            _p3.x := _p2.x + n * dx / steps ;
            _p3.y := _p2.y + n * dy / steps ;
            _p3.z := _p2.z + n * dz / steps ;

            if linepoint3D(TVector3f(vPickRayOrig), TVector3f(vPickRayEnd), _p3, d) then  begin
              Result := True ;
              exit;
            end;

          end;
        end;

        function distToPoint( _pt : TVector3f ) : Boolean ;
        var
          val : Double ;
        begin
          Result := False ;
          val := sqrt( sqr( vPickRayOrig.x - _pt.x ) +
                       sqr( vPickRayOrig.y - _pt.y ) +
                       sqr( vPickRayOrig.z - _pt.z )
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
                  p1 := TVector3f(_mesh[i].Buffer[j  ].P) ;
                  p2 := TVector3f(_mesh[i].Buffer[j+1].P) ;
                  if linepoint3D(TVector3f(vPickRayOrig), TVector3f(vPickRayEnd), p1, d) then
                    if (d <= d0) and distToPoint( p1 ) then begin
                      d0 := d ;
                      j0 := j ;
                      i0 := i ;
                      _ptg.x := p1.x ;
                      _ptg.y := p1.y ;
                      _ptg.z := p1.z ;
                      _ptg.M := d0 ;
                      Result := True ;
                    end;
                  if linepoint3D(TVector3f(vPickRayOrig), TVector3f(vPickRayEnd), p2, d) then
                    if (d <= d0) and distToPoint( p2 ) then begin
                      d0 := d ;
                      j0 := j ;
                      i0 := i ;
                      _ptg.x := p2.x ;
                      _ptg.y := p2.y ;
                      _ptg.z := p2.z ;
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
                  Inc( j, 2 ) ;
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
        p1         : TVector3f   ;
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
                  p1 := TVector3f(_mesh[i].Buffer[j  ].P) ;
                  if linepoint3D(TVector3f(vPickRayOrig), TVector3f(vPickRayEnd), p1, d) then
                    if d <= d0 then begin
                      d0 := d ;
                      j0 := j ;
                      i0 := i ;
                      _ptg.x := p1.x ;
                      _ptg.y := p1.y ;
                      _ptg.z := p1.z ;
                      _ptg.M := d0 ;
                      Result := True ;
                    end;
                  Inc( j ) ;
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
            jj := arBufSelInfo[i].ShpInfo.Count -1 ;
            if jj = 0 then begin
              layer := arBufSelInfo[i].Layer ;
              if layer is TGIS_LayerVector then begin
                shpid  := arBufSelInfo[i].ShpInfo[jj].Uid ;
                _shp   := TGIS_LayerVector(layer).GetShape(shpid) ;
              end;
              Result := True ;
              if Assigned(_shp) then _part := arBufSelInfo[i].ShpInfo[jj].Part ;
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
                    if Assigned(_shp) then
                      _part := arBufSelInfo[i].ShpInfo[l].Part ;
                  end
                  else begin
                    shpid  := arBufSelInfo[i].ShpInfo[j].Uid ;
                    _shp   := TGIS_LayerVector(layer).GetShape(shpid) ;
                    if Assigned(_shp) then
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
                  if Assigned(_shp) then
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
        p1, p2, p3 : TVector3f ;
        z   : Single ;
        dnx : Double ;
        val : Single ;
      begin
        Result := False ;
        val := 1000 ;
        if Assigned(_layer) then exit;
        if bt > 0 then exit;

        dnx := (normExt.XMax - normExt.XMin) * 10 ;
        z := zScaleFactor * (rangeExtent.ZMin - zLevelMin) / zUnit ;

        p1 := vector3Init(-dnx-val, -dnx-val, z) ;
        p2 := vector3Init( dnx+val, -dnx-val, z) ;
        p3 := vector3Init(-dnx-val,  dnx+val, z) ;
        if intersect1(p1, p2, p3, -1, -1) then begin
          Result := True ;
          exit;
        end;

        p1 := vector3Init( dnx+val, -dnx-val, z) ;
        p2 := vector3Init(-dnx-val,  dnx+val, z) ;
        p3 := vector3Init( dnx+val,  dnx+val, z) ;
        if intersect1(p1, p2, p3, -1, -1) then Result := True ;
      end;

      function getendpoint : Boolean ;
      var
        p1, p2, p3 : TVector3f ;
        z   : Single ;
        dnx : Double ;
        val : Single ;
      begin
        Result := False ;
        val := 1000 ;
        dnx := (normExt.XMax - normExt.XMin) ;
        z := (rangeExtent.ZMin - zLevelMin) / zUnit ;
        p1 := vector3Init(-dnx-val, -dnx-val, z) ;
        p2 := vector3Init( dnx+val, -dnx-val, z) ;
        p3 := vector3Init(-dnx-val,  dnx+val, z) ;
        if intersect1(p1, p2, p3, -1, -1) then begin
          Result := True ;
          exit;
        end;
        p1 := vector3Init( dnx+val, -dnx-val, z) ;
        p2 := vector3Init(-dnx-val,  dnx+val, z) ;
        p3 := vector3Init( dnx+val,  dnx+val, z) ;
        if intersect1(p1, p2, p3, -1, -1) then Result := True ;
      end;

      procedure comparelastsetting ;
      var
        len1, len2 : Extended ;
      begin
        if (lastptg.X = 0) and (lastptg.Y = 0) and (lastptg.Z = 0) then exit ;
        len1 := Sqrt(Sqr(vPickRayOrig.x-lastptg.X ) +
                     Sqr(vPickRayOrig.Y-lastptg.Y ) +
                     Sqr(vPickRayOrig.Z-lastptg.Z ) ) ;
        len2 := Sqrt(Sqr(vPickRayOrig.x-_ptg.X ) +
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
        if not Assigned(_layer) then begin
          bt := _buftype ;
          pd := d0 ;
          exit;
        end
        else begin
          bt  := _buftype ;
          if identifyobject and (layer = _layer) then begin
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
          npnt.Y := vectExtent.YMin +
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
        if CheckCSWGS then
          npnt.M := npnt.M * gisCSzUnit ;
        Result := npnt ;
      end;

      function getturnfly : TGIS_Point ;
      var
        vec1, vec2, vRayDirection, vRayPos, vRayPos2, vOut: TVector3f;
        vs1, vs2 : TVector3f;
        plane: TGIS_Point3D;
        pnt : TGIS_Point;
      begin
        // set the mouse location
        vec1.x := oContext.Width / 2 ;     vec2.x := vec1.x ;
        vec1.y := oContext.Height/ 2 ;     vec2.y := vec1.y ;
        vec1.z := 0.0 ;              vec2.z := 1.0 ;
        Vec3Unproject(vRayPos,vec1,oContext,mtxProjection,mtxView,mtxWorld,wndWdth,wndHght) ;
        Vec3Unproject(vRayDirection,vec2,oContext,mtxProjection,mtxView,mtxWorld,wndWdth,wndHght) ;
        vRayDirection.x := vRayDirection.x - vRayPos.x ;
        vRayDirection.y := vRayDirection.y - vRayPos.y ;
        vRayDirection.z := vRayDirection.z - vRayPos.z ;
        vRayPos2.x := vRayPos.x + vRayDirection.x ;
        vRayPos2.y := vRayPos.y + vRayDirection.y ;
        vRayPos2.z := vRayPos.z + vRayDirection.z ;
        vs1.x := 0.0; vs1.y := 0.0; vs1.z := 0 ; //fZplane
        vs2.x := 0.0; vs2.y := 0.0; vs2.z := 1.0 ;
        PlaneFromPointNormal(plane,vs1,vs2) ;
        PlaneIntersectLine(vOut,plane,vRayPos,vRayPos2) ;
        pnt.X :=  vOut.x ;
        pnt.Y := -vOut.y ;
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

        pt.X := -pt.X ;

        if spinX <> 90 then
          pt  := getturnfly ;

        psrc := _ptg ;
        psrc.Y := -psrc.Y ;
        psrc.X := psrc.X - pt.X ;
        psrc.Y := psrc.Y - pt.Y ;
        psrc.Z := psrc.Z ;

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

        basicEyeAt.x := ptg.X ;
        basicEyeAt.y := ptg.Y + 0.01 ;

        pRadius := Sqrt(Sqr( vPickRayOrig.x - pt.X ) +
                        Sqr(-vPickRayOrig.Y - pt.Y ) +
                        Sqr( vPickRayOrig.Z - 0    ) ) ;

        basicEyeAt.z := Abs(pRadius + ptg.Z) ;

        if (spinX = 90) or
         ((spinX <= 90) and (vPickRayOrig.z <= 0)) or
         ((spinX > 90 ) and (vPickRayOrig.z >= 0)) then
           basicEyeAt.z := Min(Abs(pRadius + ptg.Z), Abs(pRadius - ptg.Z)) ;
        basicEyeTo.x := ptg.X ;
        basicEyeTo.y := ptg.Y ;
        basicEyeTo.z := -_ptg.Z ;

        basicEyeAt.y := -basicEyeAt.y ;
        basicEyeTo.y := -basicEyeTo.y ;

        dRadius := sqrt(sqr(basicEyeAt.x-basicEyeTo.x)+
                        sqr(basicEyeAt.y-basicEyeTo.y)+
                        sqr(basicEyeAt.z-basicEyeTo.z)) ;

        referencePointMode := TGIS_Viewer3DReferenceMode.Zero ;
        SetReferencePointOffset((_ptg.Z + zoff) * zUnit / zScaleFactor) ;
        SetReferencePoint(referencePointMode, referencePointOffset) ;
        Repaint ;
      end;

  // main function body
  begin
    Result := False ;
    skip   := True ;
    _layer := nil ;
    testval := 1e+10 ;
    d0 := testval ;
    pd := d0 ;
    if _prec > 0 then
      delta  := 2 * _prec * wndXSize / windowWidth
    else
      delta  := 10 * wndXSize / windowWidth ;
    getRay( _pt, TVector3f(vPickRayOrig), TVector3f(vPickRayDir)) ;
    if getendpoint then begin
      vPickRayEnd.x := _ptg.X ;
      vPickRayEnd.y := _ptg.Y ;
      vPickRayEnd.z := _ptg.Z ;
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
    getRay( _pt, TVector3f(vPickRayOrig), TVector3f(vPickRayDir)) ;
    bt :=  0 ; // buffer type
    i0 := -1 ;
    j0 := -1 ;
    d0 := testval ;
    pd := d0 ;
    dd0 := testval ;
    lastptg := GISPoint3D(0, 0, 0) ;

    savecurrentsetting ;
    if checkVectorColorBuf(meshVectTilesCounterCW,meshVectTilesCW,vTLBufCW) then
    if pd > d0 then checklayer( 1 ) else restorecurrentsetting ;

    savecurrentsetting ;
    if checkVectorColorBuf(meshVectTilesCounterCR,meshVectTilesCR,vTLBufCR) then
    if pd > d0 then checklayer( 2 ) else restorecurrentsetting ;

    savecurrentsetting ;
    if checkVectorTexBuf(meshVectTilesCounterTW,meshVectTilesTW,vTLBufTW) then
    if pd > d0 then checklayer( 3 ) else restorecurrentsetting ;

    savecurrentsetting ;
    if checkVectorTexBuf(meshVectTilesCounterTR,meshVectTilesTR,vTLBufTR) then
    if pd > d0 then checklayer( 4 ) else restorecurrentsetting ;

    savecurrentsetting ;
    if checkVectorColorBuf(meshTinTilesCounterC,meshTinTilesC,tinBufC) then
    if pd > d0 then checklayer( 7 ) else restorecurrentsetting ;

    savecurrentsetting ;
    if checkDemWallBuf(meshTinTilesCounterT,meshTinTilesT,tinBufT) then
    if pd > d0 then checklayer( 8 ) else restorecurrentsetting ;

    savecurrentsetting ;
    if checkVectorColorBuf(meshMPatchTilesCounterC,meshMPatchTilesC,mpatchBufC) then
    if pd > d0 then checklayer( 9 ) else restorecurrentsetting ;

    savecurrentsetting ;
    if checkVectorTexBuf(meshMPatchTilesCounterT,meshMPatchTilesT,mpatchBufT) then
    if pd > d0 then checklayer( 10 ) else restorecurrentsetting ;

    savecurrentsetting ;
    if _mode then
      savelastsetting ;
    if _mode then
    if checkLineBuf(meshLineTilesCounter, meshLineTiles) then
    if pd > d0 then checklayer( 11 ) else restorecurrentsetting ;

    savecurrentsetting ;
    if _mode then
    if checkPointBuf(meshPointTilesCounter, meshPointTiles) then
    if pd > d0 then checklayer( 12 ) else restorecurrentsetting ;

    if d0 = testval then begin
      savecurrentsetting ;
      skip := False ;
      if checkDemBuf(meshDemTilesCounter,meshDemTiles) then
      if pd > d0 then checklayer( 5 ) else restorecurrentsetting ;
      skip := True ;
      savecurrentsetting ;
      if checkDemWallBuf(meshWallTilesCounter,meshWallTiles,wTLBufT) then
      if pd > d0 then checklayer( 6 ) else restorecurrentsetting ;
    end;

    savecurrentsetting ;
    if checkBasement then begin
      if changeRotPoint then begin
        setNewCameraParams ;
        exit ;
      end;
      mousePos3D := _ptg ;
      _ptg.M := Sqrt(Sqr(vPickRayOrig.x-_ptg.X ) +
                     Sqr(vPickRayOrig.Y-_ptg.Y ) +
                     Sqr(vPickRayOrig.Z-_ptg.Z ) ) ;
      _ptg := local3DToMap( _ptg ) ;
      _layer := nil ;
      _shp   := nil ;
      _part  := -1  ;
      Result := True ;
      exit ;
    end;

    if changeRotPoint then begin
      setNewCameraParams ;
      exit ;
    end;

    if _mode then
     comparelastsetting ;

    mousePos3D := _ptg ;
    _ptg.M := Sqrt(Sqr(vPickRayOrig.x-_ptg.X ) +
                   Sqr(vPickRayOrig.Y-_ptg.Y ) +
                   Sqr(vPickRayOrig.Z-_ptg.Z ) ) ;
    _ptg := local3DToMap( _ptg ) ;

    if not _mode then begin // screen2map3d mode
      Result := True ;
      exit ;
    end;

    if identifyobject then  begin
      _layer := layer ;
      Result := True ;
    end;
  end;

  function  TGIS_Renderer3DFMX.RayIntersectDem(
    const _orig    : TGIS_Point3D ;
    const _dir     : TGIS_Point3D ;
    out   _ptg     : TGIS_Point3D
  ) : Boolean ;
  var
    rayOrig, rayDir : TPoint3D ;
    numvert, i0, j0 : Integer ;
    delta, testval : Single ;
    pu, pv, pw, d, d0, pd   : Single  ;
    vb                      : P_textureVertex ;

    function getLocalOrig( const _orig : TGIS_Point3D ) : TPoint3D ;
    var
      ptg : TGIS_Point3D ;
      valx, valy : Double ;
    begin
      ptg := _orig ;
      valx := ((vectExtent.XMax - ptg.X) /
        (vectExtent.XMax - vectExtent.XMin)) ;
      valy := ((vectExtent.YMax - ptg.Y) /
        (vectExtent.YMax - vectExtent.YMin)) ;
      Result.x := roughExtent.XMax - valx *
        (roughExtent.XMax - roughExtent.XMin) ;
      Result.y := roughExtent.YMin + valy *
        (roughExtent.YMax - roughExtent.YMin) ;
      Result.y := -Result.y ;
      Result.z := projFactor * (ptg.Z - zLevel) * zFactor / zUnit ; // above 0
    end;

    function getLocalDir( const _dir : TGIS_Point3D ) : TPoint3D ;
    var
      len : Double ;
    begin
      len := sqrt( (_dir.X * _dir.X) + (_dir.Y * _dir.Y) + (_dir.Z * _dir.Z) ) ;
      Result.X := _dir.X / len ;
      Result.Y := _dir.Y / len ;
      Result.Z := _dir.Z / len ;
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
        npnt.Y := vectExtent.YMin +
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
      if CheckCSWGS then
        npnt.M := npnt.M * gisCSzUnit ;
      Result := npnt ;
    end;

    function intersect( const _p1 : TVector3f ; const _p2 : TVector3f ;
                        const _p3 : TVector3f ; const _i, _j : Integer
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
      p1, p2, p3 : TVector3f ;
    begin
        Result := False ;
        if _counter > 0 then
          for i := 0 to _counter -1 do begin
            numvert := TMesh(_mesh[i]).GetNumVertices ;

            TMesh(_mesh[i]).LockVertexBuffer(0, Pointer(vb)) ;

            j := 0 ;
            while True do begin
               p1 := TVector3f(vb^.P);
               Inc( vb ) ;
               p2 := TVector3f(vb^.P);
               Inc( vb ) ;
               p3 := TVector3f(vb^.P);
               Dec( vb ) ;
               if intersect(p1, p2, p3, i, j) then Result := True ;
              Inc( j ) ;
              if j >= numvert -3 then break ;
            end;
            TMesh(meshDemTiles[i]).UnlockVertexBuffer ;
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

  function TGIS_Renderer3DFMX.Draw
  : Integer ;
  var
    rows         : Integer     ;
    hr           : Integer     ;
    render_start : Int64       ;
    oldrough     : Double      ;
  begin
    Result := Integer($80004005);

    if oContext = nil then begin
      exit ;
    end ;

    Result := S_OK ;

    errorMsg := '' ;

    if generalLock > 0 then exit ;

    iFps := 0 ;

    if (getWindowSize.X <> windowWidth) or
       (getWindowSize.Y <> windowHeight) then begin
      windowWidth  := getWindowSize.X ;
      windowHeight := getWindowSize.Y ;
    end;

    if enDemCachedSize = TGIS_Viewer3DDemCacheSize.Window then begin
        if (noImg <> True) or (noDem <> True) then begin
          DemCachedSize := enDemCachedSize  ;
//?          demFrameSize  := vertexBufferSize ;
        end ;
      end ;

    wndXSize := windowWidth * dRadius * Tan(DegToRad(cameraHalfViewAngle))
        / windowHeight ;

    if firstScene then
      reRenderCall := True ;

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

    transformSetting ;

    oContext.Clear(backColor) ;
    oContext.SetContextState( TContextState.csAllFace ) ;
    oContext.SetContextState( TContextState.cs3DScene ) ;

    //?  no wireframe

    timeStart := GetTickCount ;
    render_start := timeStart ;

    setD3D_Data ;
    setInitialLevel ;

    setLod ;

    rows := TruncS( ( 1.0 * demFrameSize ) / lodStep) ;
    if rows * lodStep >= demFrameSize then
      Dec(rows) ;

    drawSkyBox ;
    blendOn ;
    if basePlane.Level <= zmin then begin
      current_opc := basePlane.Transparency / 100 ;
      drawBasement ;
      current_opc := 1.0 ;
    end;

    // vector drawing when DEM is transparent
    if ((transpPrior = TGIS_Viewer3DTransparencyPriority.Auto) and
          (demTransparency <> 255)) or
          (transpPrior = TGIS_Viewer3DTransparencyPriority.Dem) then begin

      if draw3DVector <> 0 then begin
        Result := -1 ;
        exit ;
      end ;

      if not firstScene then drawLabels ;
      if bArrow then
        northArrow( True ) ;

    end ;

    // turn on the color blending if necessary
    if demTransparency <> 255 then begin
//      oContext.SetContextState( TContextState.csAlphaBlendOn ) ;
      current_opc := demTransparency / 255 ;
    end;

    lockReadingData := 1 ;

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

          detWnd.Left   := 0 ;
          detWnd.Right  := demFrameSize - lodStep ;
          detWnd.Top    := -1 ;
          detWnd.Bottom := rows ;

          xMin :=  detailExtent.XMin ;
          YMin :=  detailExtent.YMin ;
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

    // turn off the color blending
    if demTransparency <> 255 then begin
//      oContext.SetContextState( TContextState.csAlphaBlendOff ) ;
      current_opc := 1.0 ;
    end;

    // vector drawing when DEM not transparent
    if ((transpPrior = TGIS_Viewer3DTransparencyPriority.Auto) and
          (demTransparency = 255)) or
          (transpPrior = TGIS_Viewer3DTransparencyPriority.Vector) then begin

      if draw3DVector <> 0 then begin
        Result := -1 ;
        exit ;
      end ;

      if bArrow then
        northArrow( True ) ;

    end ;

//    if firstScene then drawBasement ;

    if Flood.Active then
      northArrow( False ) ;

//    oContext.SetContextState( TContextState.csAlphaBlendOn ) ;
    if basePlane.Level > zmin then begin
      current_opc := basePlane.Transparency / 100 ;
      drawBasement ;
      current_opc := 1.0 ;
    end
    else begin
      blendOff ;
      current_opc := 1.0 ;
    end;

    if not firstScene then drawLabels ;

    timeEnd := getTimeTic;
    if timeEnd - render_start = 0 then
      iFps := 1000
    else
      iFps := TruncS(1000.0 / (timeEnd - render_start)) ;

    firstScene   := False ;
    reRenderCall := False ;
    Result := S_OK ;

  end;

  procedure TGIS_Renderer3DFMX.SetSunPosition(
    const _value : TGIS_Point
  ) ;
  var
    val  : Double     ;
    sp   : TGIS_Point ; // sun position in deg
    pos2 : TPoint3D   ; // dTurn, dFly, dRadius
    pos3 : TPoint3D   ; // spinx, spiny, spinz

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

    sp.X := RadToDeg( _value.X ) ;
    sp.Y := RadToDeg( _value.Y ) ;

    pos2 := Point3D(0,0,10) ;
    pos2.X := -pos2.X ;
    pos3 := Point3D(90-sp.X, 0, -sp.Y);
    pos3.X := pos3.X ;

    oLight.Position.X :=  pos2.X + (pos2.Z * Sin(DegToRad(pos3.Z))*
        Sin(DegToRad(0))) ;
    oLight.Position.Y :=  pos2.Y - (pos2.Z * Cos(DegToRad(pos3.Z))*
        Sin(DegToRad(0))) ;
    oLight.Position.Z := -pos2.Z * Cos(DegToRad(0)) ;

    oLight.RotationAngle.X := 0 ;
    oLight.RotationAngle.Z := pos3.Z ;

    oLight.Position.X :=  pos2.X + (pos2.Z * Sin(DegToRad(pos3.Z))*
        Sin(DegToRad(pos3.X))) ;
    oLight.Position.Y :=  pos2.Y - (pos2.Z * Cos(DegToRad(pos3.Z))*
        Sin(DegToRad(pos3.X))) ;
    oLight.Position.Z := -pos2.Z * Cos(DegToRad(pos3.X)) ;


    oLight.RotationAngle.X := pos3.X - 180 ;
    oLight.RotationAngle.Z := pos3.Z ;
    oLight.Color := TAlphaColorRec.White ;
  end;

  procedure TGIS_Renderer3DFMX.setAmbientLightColor(
    const _val  : Integer
  ) ;
  begin
    ambColorDX := color2ARGB( 255, _val, _val, _val ) ;
    ambColor := ambColorDX ;
  end;

  function  TGIS_Renderer3DFMX.GetUniverseColor : TGIS_Color ;
  var
    r, g, b : Byte ;
  begin
    r := ( (backColor shr 16) and $000000FF) ;
    g := ( (backColor shr 08) and $000000FF) ;
    b := ( (backColor shr  0) and $000000FF) ;
    Result := TGIS_Color.FromRGB( b, g, r ) ;
  end;

  procedure TGIS_Renderer3DFMX.SetUniverseColor(
    const _value         : TGIS_Color
  ) ;
  var
    r, g, b : Byte ;
  begin
    r := _value.R ;
    g := _value.G ;
    b := _value.B ;
    backColor := color2ARGB(255, r, g, b) ;
  end;

  procedure TGIS_Renderer3DFMX.PrintBmp(
    const _rect  : TRect ;
    const _bmp   : TGIS_Bitmap
  ) ;
 var
    bmp : TBitmap ;
    rct : TRectF ;
    sc  : Single ;
  begin
    sc := Min( _bmp.Width/_rect.Width, _bmp.Height/_rect.Height ) ;

    rct := oCamera.ScreenBounds ;
    bmp := TBitmap.Create ;

    oCamera.CreateTileSnapshot(bmp,                              // bitmap
                               RoundS(_rect.Width*sc),           // width
                               RoundS(_rect.Height*sc),          // height
                               TruncS(-rct.Left * sc),           // offset x
                               TruncS(-rct.Top * sc),            // offset y
                               sc,                               // scale
                               backColor);                       // background

    _bmp.LoadFromBitmap(bmp,'');
    bmp.Free ;
  end;

  function  TGIS_Renderer3DFMX.PrintBegin(
    const _width  : Integer ;
    const _height : Integer
  ) : TPoint ;
  begin
    printBmpOutSize.X := _width ;
    printBmpOutSize.Y := _height ;

    printBmpPPI := oGIS.PPI ;   // required print PPI

    Result := Point( windowWidth, windowHeight ) ;
    printBmpImgSize := 4096 ;
    inPaint := True ;
    reRenderCall := True ;
    lockReadingData := 0 ;
    prepareVertexBuffersToDraw ;
    lockReadingData := 1 ;
    reRenderCall   := False ;
    bMustReRender  := False ;
    bPartialUpdate := False ;
    inPaint := False ;
  end;

  procedure TGIS_Renderer3DFMX.PrintTile(
    const _bmp      : TGIS_Bitmap ;
    const _offsetx  : Integer ;
    const _offsety  : Integer
  ) ;
 var
    bmp : TBitmap ;
    rct : TRectF ;
    sc  : Single ;
    i, j : integer ;
  begin
    if printBmpOutSize = Point( 0, 0 ) then begin
//      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ),
//                                   'PrintTile: Size of output bitmap not defined', 0
//                                 ) ;
      exit;
    end;

    if ( _bmp.Width  = printBmpOutSize.X ) and
       ( _bmp.Height = printBmpOutSize.Y ) then
      printBmpAllInOne := True
    else
      printBmpAllInOne := False ;

    sc := Min( printBmpOutSize.X/windowWidth, printBmpOutSize.Y/windowHeight ) ;

    rct := oCamera.ScreenBounds ;
    bmp := TBitmap.Create ;

    captureScreen := True ;

    if printBmpAllInOne then
      oCamera.CreateTileSnapshot(bmp,                              // bitmap
                                 RoundS(windowWidth*sc),           // width
                                 RoundS(windowHeight*sc),          // height
                                 TruncS(-rct.Left * sc),           // offset x
                                 TruncS(-rct.Top * sc),            // offset y
                                 sc,                               // scale
                                 backColor)                        // background
    else begin
      // required tile
      i := RoundS( _offsetx / windowWidth ) ;
      j := RoundS( _offsety / windowHeight ) ;
      oCamera.CreateTileSnapshot(bmp,                              // bitmap
                                 RoundS(windowWidth),              // width
                                 RoundS(windowHeight),             // height
                                 TruncS(-rct.Left * sc + i * windowWidth  ),           // offset x
                                 TruncS(-rct.Top  * sc + j * windowHeight ),            // offset y
                                 sc,                               // scale
                                 backColor) ;                       // background

    end;
    _bmp.LoadFromBitmap(bmp,'');
    captureScreen := False ;
    bmp.Free ;
  end;

  procedure TGIS_Renderer3DFMX.PrintEnd ;
  begin
    printBmpOutSize := Point( 0, 0 ) ;
    printBmpImgSize := 0 ;
    imgFrameSize := Max(windowWidth,windowHeight) ;
    inPaint := True ;
    reRenderCall := True ;
    lockReadingData := 0 ;
    prepareVertexBuffersToDraw ;
    lockReadingData := 1 ;
    reRenderCall   := False ;
    bMustReRender  := False ;
    bPartialUpdate := False ;
    inPaint := False ;
  end;

  procedure TGIS_Renderer3DFMX.BeginContext(
    const _value : TContext3D
  ) ;
  begin
    oContext := _value ;
    kkk := _value.PixelFormat ;
    current_cntx := _value ;
    wndWdth := _value.Width  ;
    wndHght := _value.Height ;
  end;

  function TGIS_Renderer3DFMX.GetNewCameraPosition3DEx : TGIS_Point3D ;
  var
    ptg,ptg1,ptg2 : TGIS_Point3D ;
    pp,pp1 : TVector3f ;

    function newPntPosition(
      const _pnt : TGIS_Point3D ;
      const _rx  : Double ;
      const _ry  : Double ;
      const _rz  : Double ;
      const _vec : TGIS_Point3D ;
      const _mod : Boolean
      ) : TGIS_Point3D ;
    var
      c1,c2,c3,s1,s2,s3 : Double ;
      x,y,z       : Double ;

      function rotx : TGIS_Point3D ;
      begin
        Result.x := x    + 0    + 0    ;
        Result.y := 0    + c1*y - s1*z ;
        Result.z := 0    + s1*y + c1*z ;
      end;

      function roty : TGIS_Point3D ;
      begin
        Result.x := c2*x + 0    + s2*z ;
        Result.y := 0    + y    + 0    ;
        Result.z :=-s2*x + 0    + c2*z ;
      end;

      function rotz : TGIS_Point3D ;
      begin
        Result.x := c3*x - s3*y + 0    ;
        Result.y := s3*x + c3*y + 0    ;
        Result.z := 0    + 0    + z    ;
      end;

    begin
      c1 := Cos( _rx ) ;
      s1 := Sin( _rx ) ;
      c2 := Cos( _ry ) ;
      s2 := Sin( _ry ) ;
      c3 := Cos( _rz ) ;
      s3 := Sin( _rz ) ;
      if _mod = True then begin
        x  := _pnt.x - _vec.X ;
        y  := _pnt.y - _vec.Y ;
        z  := _pnt.z - _vec.Z ;
      end
      else begin
        x  := _pnt.x ;
        y  := _pnt.y ;
        z  := _pnt.z ;
      end;
      Result := rotx ;
      x  := Result.x ;
      y  := Result.y ;
      z  := Result.z ;
      Result := roty ;
      x  := Result.x ;
      y  := Result.y ;
      z  := Result.z ;
      Result := rotz ;

      Result.x := Result.x + _vec.X ;
      Result.y := Result.y + _vec.Y ;
      Result.z := Result.z + _vec.Z ;
    end ;

  begin
    ptg1.X := basicEyeAt.x ;
    ptg1.Y := basicEyeAt.y ;
    ptg1.Z := basicEyeAt.z ;

    if newTransform then begin
      ptg2.X :=  eyeX ;
      ptg2.Y := -eyeY ;
      ptg2.Z := zEyeTo-zOffset ;
      Result := newPntPosition(
                  ptg1,
                  DegToRad(spinX), DegToRad(spinY), DegToRad(spinZ),
                  ptg2,
                  True
                ) ;
    end
    else begin
      ptg2.X := -dTurn ;
      ptg2.Y := dFly ;
      ptg2.Z := zEyeTo-zOffset ;
      Result := newPntPosition(
                  ptg1,
                  DegToRad(spinX), DegToRad(spinY), DegToRad(spinZ),
                  ptg2,
                  False
                ) ;
    end;
  end;

  procedure TGIS_Renderer3DFMX.SetCameraParams ;
  var
    pos1 : TGIS_Point3D ;
    pos2 : TGIS_Point3D ;
  begin
    basicEyeAt.z := dRadius ;
    pos1 := GetNewCameraPosition3DEx ;
    pos2 := GetCameraRotationEx ;
    pos2.X := pos2.X + 180 ;
    if pos2.X > 270 then pos2.X := 270;

    oCamera.Position.X :=  pos1.X ;
    oCamera.Position.Y :=  pos1.Y ;
    oCamera.Position.Z :=  pos1.Z ;

    oCamera.RotationAngle.X := 0 ;
    oCamera.RotationAngle.Y := 0 ;
    oCamera.RotationAngle.Z := pos2.Z ;
    oCamera.RotationAngle.X := pos2.X ;

  end;

  procedure TGIS_Renderer3DFMX.prepareMesh(
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
          pdataTEX_tDX := create_texture( bmp ) ;
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

  function TGIS_Renderer3DFMX.draw3DVector
    : Integer ;
  var
    i,j       : Integer          ;
    lv        : TGIS_LayerVector ;
    a         : Cardinal         ;
    r,g,b     : Byte             ;
    bmp       : TGIS_Bitmap      ;
    old_zu    : Double           ;
    call_type : Boolean          ;
    shp       : TGIS_Shape       ;
    res       : Boolean          ;
    ext       : TGIS_Extent      ;
  begin
    Result := S_OK ;
    currentSHP := nil ;

    if curMeshSize > 0 then begin
      currentVL := nil ;
      if reRenderCall or firstScene then begin
        for i := 0 to curMeshSize -1 do begin
          tinBmp := False ;
          currentML := curMesh[i].Lx ;
          projFactor := curMesh[i].Zu ;

          if curMesh[i].Lx.IsGrid then begin
            bmp := TGIS_Bitmap.Create( TruncS(imgFrameSize*curMesh[i].Lx.CellWidth/curMesh[i].Lx.CellHeight), imgFrameSize ) ;
            oGIS.ViewerParent.ControlDrawTexture( bmp, curMesh[i].Lx,
                                  curMesh[i].Lx.ProjectedExtent, oGIS.PPI ) ;
            pdataTEX_tDX := create_texture( bmp ) ;
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
    end;

    if curVLSize = 0 then begin
        exit ;
    end
    else begin
      SetLength( curVLlist, curVLSize ) ;
      curVLListSize := curVLSize ;
    end;

    try
      call_type := reRenderCall ;
      a := 255 ;
      pixSize := GetPixelSize.X;

      if lightSwitch then
        lightOn
      else
        lightOff ;

      current_lght := lightSwitch ;

      if curVLListSize <> curVLSize then
      if shpNo <> 0 then
        shpNo := 0 ;

//      if shpNo = 0 then reRenderCall := True ;

      old_zu := zUnit ;

      for i:= 0 to curVLSize-1 do begin
        lv := curVL[i].Lv ;
        currentVL    := lv ;
        currentVLnz  := curVL[i].Nz ;
        currentVLnm  := curVL[i].Nm ;
        currentVLzmi := curVL[i].Zmi ;
        currentVLmmi := curVL[i].Mmi ;
        projFactor   := curVL[i].Zu ;
        projMFactor  := curVL[i].Fx ;

        ext := GisCommonExtent( oGIS.VisibleExtent, currentVL.ProjectedExtent ) ;
        ext := currentVL.UnprojectExtent( ext ) ;
        cutVectExtentL := GisCommonExtent( ext, currentVL.Extent ) ;

        a := TruncS (currentVL.Transparency/100*255) ;
        r := edgesColor.R ;
        g := edgesColor.G ;
        b := edgesColor.B ;

        edColor := color2ARGB(a,r,g,b) ;

        if lv.View3D.Mode = TGIS_3DLayerType.Dem then begin   // TIN
          tinBmp := False ;
          if not TGIS_Bitmap.IsNilOrEmpty( currentVL.Params.Area.Bitmap ) then
          begin
            bmp := currentVL.Params.Area.Bitmap ;
            tinBmp := True ;
              pdataTEX_tDX := T_textureHelperFX( oTextureHelper ).Get(
                T_textureHelperFX( oTextureHelper ).Prepare( bmp)) ;
//?              if a <> 255 then setTextureOperations ;
          end;

          if call_type or firstScene then begin

            if not lv.ForEach( cutVectExtentL, '', nil, '', True,
                               oGIS.ScaleAsFloat, doCallBackTin ) then begin
              if shpNo > 0 then begin
                // turn off the color blending
                zUnit := old_zu ;
                raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'draw3dVector', 0 ) ;
              end;
            end;
            if tinBmp then begin
              curTxt := bmp ;
              saveTinBufT ;
            end
            else
              saveTinBufC ;
//            drawMeshTinTiles ;
          end
          else begin
            if tinBmp then curTxt := bmp ;
//            drawMeshTinTiles ;
          end;
        end;

        if lv.View3D.Mode = TGIS_3DLayerType.Shapes then begin  // VECTOR
          if reRenderCall then begin   // reading
            pntNo := -1 ;
            LabelsReg.Reset ;
            shpIsTin := isTinLayer ;
            if not lv.ForEach( cutVectExtentL, '', nil, '', True,
                             oGIS.ScaleAsFloat,  addShapeToCache ) then begin
              if shpNo > 0 then begin
                if Assigned(currentVL) then
                  zUnit := old_zu ;
                  raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'draw3dVector', 1 ) ;
              end;
            end
            else
              curVLlist[i] := shpNo ;

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

      if not reRenderCall then begin
        drawVectTiles    ;
        drawMeshLinTiles ;
        drawMeshPntTiles ;
        drawMPatchTiles  ;
        drawMeshTinTiles ;
      end;

      zUnit := old_zu ;

      Result := S_OK ;
    except
      on e : Exception do begin
        if not oGIS.NotifyPaintException( currentVL.Name, e ) then
          raise;
      end;
    end;
  end;

  function TGIS_Renderer3DFMX.setD3D_Data
    : Integer;
  var
    hr     : Integer ;
    i      : Integer ;
    dpi    : Integer ;
    rdr    : TGIS_RendererAbstract ;
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
      ptgex := GISPoint3D( 0.5*(_lp.Extent.XMin + _lp.Extent.XMax),
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
            if arGrid_r[i1][j1] = noDataValue then
              arGrid_r[i1][j1] := val
            else
            if arGrid_r[i1][j1] < val then
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
      current_lght := lightSwitch ;
    end;

    function pow2compatible( const _val : Integer ) : Integer ;
    var
      i : Integer ;
    begin
      i := 32 ;
      While True do begin
        if _val < i then break ;
        i := i * 2 ;
      end ;
      Result := i ;

      if Result > 2048 then
        Result := 2048 ;
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

    roughExtent := scrExtent ;
    updateFrameSizes ;
    setShader( False ) ;

    if printBmpImgSize <> 0 then
        imgfs := printBmpImgSize
    else
        imgfs := pow2compatible(imgFrameSize) ;

    if imageTexture then begin
      ext3d := GetVisibleExtent3D ;
      exdx  := ext3d.XMax - ext3d.XMin ;
      exdy  := ext3d.YMax - ext3d.YMin ;
      exdlt := Max(exdx, exdy) / Max( mapExtent.XMax-mapExtent.xmin, mapExtent.YMax-mapExtent.YMin) ;

      if printBmpImgSize <> 0 then begin;
        dpi := RoundS(printBmpPPI * exdlt ) ;
        dpi := RoundS( dpi * ( imgfs / Max( printBmpOutSize.X, printBmpOutSize.Y))) ;
      end
      else
        dpi := RoundS(oGIS.PPI * exdlt * imgfs / imgFrameSize ) ;

      if dpi<10 then dpi := 10 ;

      bmp := TGIS_Bitmap.Create( imgfs , imgfs ) ;
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
//        bmp.SaveToFile('C:\Project_wrk\DK11\TestVCLGDI\rob-multidem\dumpfmx.jpg');
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
      Result := hr ;
      exit ;
    end;

    // Set current segment extent  (detail)
    setNormExtent(True) ;
    mapExtent    := retGisExtent ;
    detailExtent := scrExtent    ;

    if not checkextent then begin
      setShader( True ) ;
      Result := hr ;
      exit ;
    end;

    if imageTexture then begin
      if printBmpImgSize = 0 then
        dpi := RoundS((oGIS.PPI / detailCoeff) * (imgfs / imgFrameSize))
      else begin
        dpi := RoundS(printBmpPPI / detailCoeff ) ;
        dpi := RoundS( dpi * ( imgfs / Max( printBmpOutSize.X, printBmpOutSize.Y))) ;
      end;

      bmp := TGIS_Bitmap.Create( imgfs , imgfs ) ;
      try
        generalLock := 1 ;
        oGIS.ViewerParent.ControlDrawTexture( bmp, mapExtent, dpi ) ;
        generalLock := 0 ;
        hr := loadTexture(bmp, True) ;
        if hr <> 0 then begin
          Result := hr ;
          exit ;
        end;
      finally
        FreeObject( bmp ) ;
      end;
    end;

    setShader( True ) ;
    Result := hr ;
  end;

  procedure TGIS_Renderer3DFMX.freeMeshStore ;
  var
    i,j : Integer ;
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

    if High(mpatchTileInfo) >= 0 then
      for j:= 0 to meshMPatchTilesCounterT -1 do
        for i := 0 to High(mpatchTileInfo[j]) do
          if mpatchTileInfo[j][i].TexDX.Handle <> 0 then
             FreeObject(mpatchTileInfo[j][i].TexDX) ;
    SetLength(mpatchTileInfo, meshMPatchTilesCounterT+1, 0) ;
    mpatchSubsetCounter := -1 ;

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

    shpNo := 0 ;
    SetLength(curVLlist, curVLSize) ;
    curVLListSize := curVLSize ;

    curMeshTexR   := nil ;
    curMeshTexW   := nil ;
    curMeshSubset := -1  ;
    lastMeshSubsetR := -1 ;
    lastMeshSubsetW := -1 ;
    SetLength(meshTileInfoR , meshVectTilesCounterTR +1, 0) ;
    SetLength(meshTileInfoW , meshVectTilesCounterTW +1, 0) ;

    arVectPolyRoofCounterC := 0 ;
    SetLength(arVectPolyRoofC, 1) ;
    SetLength(arVectPolyRoofC[0], initVideoVBSize );
    arVectPolyRoofCounterT := 0 ;
    SetLength(arVectPolyRoofT, 1) ;
    SetLength(arVectPolyRoofT[0], initVideoVBSize );

    releaseLabelInfo ;

    if Assigned( oTextureHelper ) then
      T_textureHelperFX( oTextureHelper ).Clear ;

    demMeshExists := False ;
    shpNo := 0 ;
  end;

  function TGIS_Renderer3DFMX.transformSetting
    : Integer ;
  var
    mat,matrotx,matrotz,mattransl,matrob : TMatrix3D ;
    pos  : TPoint3D ;
    znear, zfar     : Single ;
  begin
    mat := TMatrix3D.Identity ;
    if not newTransform then begin   // oldTransform
      pos.X := dTurn ;
      pos.Y := -dFly  ;
      if zEyeTo = 0 then
        pos.Z := zEyeTo
      else
        pos.Z := -(zEyeTo+zOffset) ;

      mattransl := TMatrix3D.CreateTranslation( pos ) ;
      matrotx := TMatrix3D.CreateRotationX(DegToRad(-spinX));
      matrotz := TMatrix3D.CreateRotationZ(DegToRad(-spinZ));
      mat :=  matrotz * matrotx ;
      mtxWorld := mattransl * mat ;
    end
    else begin  // newTransform
      pos.X := -eyeX ;
      pos.Y := eyeY  ;
      pos.Z := -(zEyeTo+zOffset) ;

      mattransl := TMatrix3D.CreateTranslation( pos ) ;
      matrotx := TMatrix3D.CreateRotationX(DegToRad(-spinX));
      matrotz := TMatrix3D.CreateRotationZ(DegToRad(-spinZ));

      mat :=  matrotz * matrotx ;
      matrob := mattransl * mat ;

      if zEyeTo = 0 then begin
        pos.X :=  eyeX ;
        pos.Y := -eyeY ;
        pos.Z := 0 ;
        mattransl := TMatrix3D.CreateTranslation( pos ) ;
      end
      else begin
        pos.X :=  eyeX ;
        pos.Y := -eyeY ;
        pos.Z := zEyeTo-zOffset ;
        mattransl := TMatrix3D.CreateTranslation( pos ) ;
      end;
      mtxWorld := matrob * mattransl ;
    end;

    mtxView := TMatrix3D.CreateLookAtRH(TPoint3D(eyeAt), TPoint3D(eyeTo), TPoint3D.Create(0,1,0)) ;

    if dRadius < 1500 then
      zfar := 2000.0
    else
       zfar := dRadius + 500 ;

    if dRadius < 2.51 then
      znear := 0.05
    else
      znear := 1.0 ;
    mtxProjection := TMatrix3D.CreatePerspectiveFovRH(DegToRad(cameraViewAngle),
//    mtxProjection := TMatrix3D.CreatePerspectiveFovRH(DegToRad(45),
                                        oContext.Width/oContext.Height,
//                                        1.0,1000,False) ;
                                        znear,zfar,False) ;

//?    oContext.SetMatrix( mtxWorld ) ;
//?    oContext.SetCameraMatrix( mtxView ) ;
    mtxMVP := mtxWorld * mtxView * mtxProjection ;
    Result := 0 ;
  end;

  procedure TGIS_Renderer3DFMX.adjustTransformSetting(
    _value : Boolean
  ) ;
  var
    ptt  : TGIS_Point3D ;
    zoff : Double       ;

    function getturnfly : TGIS_Point3D ;
    var
      vec1, vec2, vRayDirection, vRayPos, vRayPos2, vOut: TVector3f ;
      vs1, vs2 : TVector3f ;
      plane: TGIS_Point3D ;
      pnt  : TGIS_Point3D ;
    begin
      // set the mouse location
      vec1.x := oContext.Width / 2 ;     vec2.x := vec1.x ;
      vec1.y := oContext.Height/ 2 ;     vec2.y := vec1.y ;
      vec1.z := 0.0                ;     vec2.z := 10.0 ;
      Vec3Unproject(vRayPos,vec1,oContext,mtxProjection,mtxView,mtxWorld,wndWdth,wndHght) ;
      vPickRayOrig.X := vRayPos.x ;
      vPickRayOrig.Y := vRayPos.y ;
      vPickRayOrig.Z := vRayPos.z ;
      Vec3Unproject(vRayDirection,vec2,oContext,mtxProjection,mtxView,mtxWorld,wndWdth,wndHght) ;
      vRayDirection.x := vRayDirection.x - vRayPos.x ;
      vRayDirection.y := vRayDirection.y - vRayPos.y ;
      vRayDirection.z := vRayDirection.z - vRayPos.z ;
      vRayPos2.x := vRayPos.x + vRayDirection.x ;
      vRayPos2.y := vRayPos.y + vRayDirection.y ;
      vRayPos2.z := vRayPos.z + vRayDirection.z ;
      vs1.x := 0.0; vs1.y := 0.0; vs1.z := zEyeTo ;
      vs2.x := 0.0; vs2.y := 0.0; vs2.z := 1.0 ;
      PlaneFromPointNormal(plane,vs1,vs2) ;
      PlaneIntersectLine(vOut,plane,vRayPos,vRayPos2) ;
      pnt.X := vOut.x ;
      pnt.Y := vOut.y ;
      pnt.Z := vOut.z ;
      Result := pnt ;
    end;

    function getendpoint : TGIS_Point3D ;
    var
      vec1, vec2, vRayDirection, vRayPos, vRayPos2, vOut: TVector3f;
      vs1, vs2 : TVector3f;
      plane: TGIS_Point3D ;
      pnt  : TGIS_Point3D ;
    begin
      // set the mouse location
      vec1.x := oContext.Width / 2 ;     vec2.x := vec1.x ;
      vec1.y := oContext.Height/ 2 ;     vec2.y := vec1.y ;
      vec1.z := 0.0                ;     vec2.z := 1.0 ;
      Vec3Unproject(vRayPos,vec1,oContext,mtxProjection,mtxView,mtxWorld,wndWdth,wndHght) ;
      vPickRayOrig.X := vRayPos.x ;
      vPickRayOrig.Y := vRayPos.y ;
      vPickRayOrig.Z := vRayPos.z ;
      Vec3Unproject(vRayDirection,vec2,oContext,mtxProjection,mtxView,mtxWorld,wndWdth,wndHght) ;
      vRayDirection.x := vRayDirection.x - vRayPos.x ;
      vRayDirection.y := vRayDirection.y - vRayPos.y ;
      vRayDirection.z := vRayDirection.z - vRayPos.z ;
      vRayPos2.x := vRayPos.x + vRayDirection.x ;
      vRayPos2.y := vRayPos.y + vRayDirection.y ;
      vRayPos2.z := vRayPos.z + vRayDirection.z ;

      vs1.x := 0.0; vs1.y := 0.0; vs1.z := 0.0 ;
      if ((spinZ > 225) and (spinZ < 315)) or
         ((spinZ >  45) and (spinZ < 135)) then begin
        vs2.x := 1.0; vs2.y := 0.0; vs2.z := 0.0
      end
      else begin
        vs2.x := 0.0; vs2.y := 1.0; vs2.z := 0.0
      end;
      vs2.x := Sin(DegToRad(SpinZ)); vs2.y := Cos(DegToRad(SpinZ)); vs2.z := 0.0 ;

      PlaneFromPointNormal(plane,vs1,vs2) ;
      PlaneIntersectLine(vOut,plane,vRayPos,vRayPos2) ;
      pnt.X := vOut.x ;
      pnt.Y := vOut.y ;
      pnt.Z := vOut.z ;
      Result := pnt ;
    end;

    procedure exception_case ;
    begin
        zoff := zScaleFactor * zLevel / zUnit ;
        ptt := getendpoint ;
        dTurn := -ptt.X ;
        dFly  := -ptt.Y ;
        dRadius := Sqrt(Sqr(vPickRayOrig.x + dTurn ) +
                        Sqr(vPickRayOrig.Y + dFly  ) +
                        Sqr(vPickRayOrig.z - ptt.Z ) ) + 1 ;

        referencePointMode := TGIS_Viewer3DReferenceMode.Zero ;
        SetReferencePointOffset((ptt.Z + zoff) * zUnit / zScaleFactor ) ;
        SetReferencePoint(referencePointMode, referencePointOffset) ;

        basicEyeAt := vector3Init( 0, 0.01, dRadius ) ;
        basicEyeTo := vector3Init( 0, 0, 0 ) ;
        Repaint ;
    end;

  begin
    newTransform := _value ;
    if not _value then begin // GISMouseUp
      if (spinX = 90) or
         ((spinX <= 90) and (vPickRayOrig.z <= 0)) or
         ((spinX > 90 ) and (vPickRayOrig.z >= 0)) then begin
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
      dRadius := Sqrt(Sqr(vPickRayOrig.x + dTurn  ) +
                      Sqr(vPickRayOrig.Y + dFly   ) +
                      Sqr(vPickRayOrig.Z - ptt.Z ) ) + 1 ;

      dFly  := -dFly ;
      basicEyeAt := vector3Init( 0, 0.01, dRadius ) ;
      basicEyeTo := vector3Init( 0, 0, 0 ) ;
      if (dFly > normExt.YMin) and (dFly < normExt.YMax) then begin
        updateCameraParams ;
        SetCameraPosition( GetCameraPosition ) ;
      end;
      Repaint ;
    end;

  end;

  procedure TGIS_Renderer3DFMX.makeBasementTex ;
  var
    bmp     : TGIS_Bitmap ;
    i,j,k,l : Integer ;
    dat     : TBitmapData ;
    cl      : TAlphaColor ;
    mn      : Single      ;
    a       : Integer ;
  begin
    bmp := TGIS_Bitmap.Create( 256, 256 ) ;
    try
      a := Byte(TruncS(basePlane.Transparency/100.0*255.0)) ;
      TBitmap( bmp.NativeBitmap ).Clear(
        TAlphaColorF.Create(
        basePlane.BackgroundColor.R/255,
        basePlane.BackgroundColor.G/255,
        basePlane.BackgroundColor.B/255,
        a/255 ).ToAlphaColor
      ) ;

      if Assigned( pdataTEX_bDX ) then
        FreeObject( pdataTEX_bDX ) ;
      pdataTEX_bDX := create_texture( bmp ) ;
    finally
      FreeObject( bmp ) ;
    end ;

    bmp := TGIS_Bitmap.Create( 512, 512 ) ;
    try
      TBitmap( bmp.NativeBitmap ).Map(TMapAccess.Write, dat) ;
      try
        l := 0 ;
        mn := 4/256 ;
        for i:= 0 to 63 do begin        //B
          for j := 0 to 63 do begin     //G
            for k := 0 to 63 do begin   //R
              cl := TAlphaColorF.Create( k*mn, j*mn, i*mn, 1 ).ToAlphaColor ;
              dat.SetPixel( l div 512, l mod 512, cl ) ;
              Inc( l ) ;
            end;
          end;
        end ;
      finally
        TBitmap( bmp.NativeBitmap ).Unmap(dat) ;
      end;

      if Assigned( pdataTEX_cDX ) then
        FreeObject( pdataTEX_cDX ) ;
      pdataTEX_cDX := create_texture( bmp ) ;
    finally
      FreeObject( bmp ) ;
    end ;
  end;

  procedure TGIS_Renderer3DFMX.drawBasement ;
  var
    z, z1, rr    : Double                 ;
    dnx, dx      : Double                 ;
    dn, dnx2     : Double                 ;
    linelist     : Array [0..3] of TGIS_Renderer3DVertex ;
    plinelist_vb : TVertexBuffer          ;
    plinelist_ib : TIndexBuffer           ;
    x, y         : Single                 ;
    i, n1, n2    : Integer                ;
    l, r, t, b   : Double                 ;
    ext          : TGIS_Extent            ;
    p1, p2       : TPoint3D               ;
    cl           : TAlphaColor            ;
    a            : Integer                ;
  begin
    if reRenderCall then exit;
    if not basePlane.Active then exit ;
    lightOff ;

    z := zScaleFactor * (basePlane.Level - zLevelMin) / zUnit ;
    dnx := (normExt.XMax - normExt.XMin) * 10 ;
    dx := 100 ;

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

    l := dn*(dnx-ext.XMax)/dnx2 ;   l := l / dn ;
    r := dn*(dnx+ext.XMax)/dnx2 ;   r := r / dn ;
    t := dn*(dnx-ext.YMax)/dnx2 ;   t := t / dn ;
    b := dn*(dnx+ext.YMax)/dnx2 ;   b := b / dn ;
    dn := 1 ;

    plinelist_vb := TVertexBuffer.Create(Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE, 4) ;
    plinelist_ib := TIndexBuffer.Create(6, TIndexFormat.UInt16);

    if noDem and noIMG then begin
      n1 := 0 ;  n2 := 0 ;
    end
    else begin
      z1 := zScaleFactor * (rangeExtent.ZMin - zLevelMin) / zUnit ;
      rr := initRadius / dRadius ;
      if (Abs(z-z1) * rr < 0.05)  then begin
        n1 := 1 ;  n2 := 4 ;
      end
      else begin
        n1 := 0 ;  n2 := 0 ;
      end;
    end;

    for i := n1 to n2 do begin
      case i of
        0 : begin
              linelist[0].P := TVector3f(vector3Init(-dnx, -dnx, z)) ;
              linelist[0].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[0].Tu := 0 ;
              linelist[0].Tv := 0 ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(vector3Init( dnx, -dnx, z)) ;
              linelist[1].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[1].Tu := 1 ;
              linelist[1].Tv := 0 ;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(vector3Init(-dnx,  dnx, z)) ;
              linelist[2].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[2].Tu := 0 ;
              linelist[2].Tv := 1 ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(vector3Init( dnx,  dnx, z)) ;
              linelist[3].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[3].Tu := 1 ;
              linelist[3].Tv := 1 ;
              linelist[3].Color := $FFFFFFFF ;
            end;
        1 : begin
              linelist[0].P := TVector3f(vector3Init(-dnx, -dnx, z)) ;
              linelist[0].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[0].Tu := 0 ;
              linelist[0].Tv := 0 ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(vector3Init( dnx, -dnx, z)) ;
              linelist[1].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[1].Tu := 1 ;
              linelist[1].Tv := 0 ;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(vector3Init(-dnx,  ext.YMin, z)) ;
              linelist[2].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[2].Tu := 0 ;
              linelist[2].Tv := t ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(vector3Init( dnx,  ext.YMin, z)) ;
              linelist[3].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[3].Tu := 1 ;
              linelist[3].Tv := t ;
              linelist[3].Color := $FFFFFFFF ;
            end;
        2 : begin
              linelist[0].P := TVector3f(vector3Init(-dnx, ext.YMax, z)) ;
              linelist[0].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[0].Tu := 0 ;
              linelist[0].Tv := b ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(vector3Init( dnx, ext.YMax, z)) ;
              linelist[1].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[1].Tu := 1 ;
              linelist[1].Tv := b;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(vector3Init(-dnx,  dnx, z)) ;
              linelist[2].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[2].Tu := 0 ;
              linelist[2].Tv := 1 ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(vector3Init( dnx,  dnx, z)) ;
              linelist[3].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[3].Tu := 1 ;
              linelist[3].Tv := 1 ;
              linelist[3].Color := $FFFFFFFF ;
            end;
        3 : begin
              linelist[0].P := TVector3f(vector3Init(-dnx, ext.YMin, z)) ;
              linelist[0].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[0].Tu := 0 ;
              linelist[0].Tv := t ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(vector3Init( ext.XMin, ext.YMin, z)) ;
              linelist[1].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[1].Tu := l ;
              linelist[1].Tv := t ;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(vector3Init(-dnx,  ext.YMax, z)) ;
              linelist[2].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[2].Tu := 0 ;
              linelist[2].Tv := b ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(vector3Init( ext.XMin,  ext.YMax, z)) ;
              linelist[3].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[3].Tu := l ;
              linelist[3].Tv := b ;
              linelist[3].Color := $FFFFFFFF ;
            end;
        4 : begin
              linelist[0].P := TVector3f(vector3Init(ext.XMax, ext.YMin, z)) ;
              linelist[0].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[0].Tu := r ;
              linelist[0].Tv := t ;
              linelist[0].Color := $FFFFFFFF ;
              linelist[1].P := TVector3f(vector3Init( dnx,  ext.YMin, z)) ;
              linelist[1].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[1].Tu := 1 ;
              linelist[1].Tv := t ;
              linelist[1].Color := $FFFFFFFF ;
              linelist[2].P := TVector3f(vector3Init(ext.XMax,  ext.YMax, z)) ;
              linelist[2].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[2].Tu := r ;
              linelist[2].Tv := b ;
              linelist[2].Color := $FFFFFFFF ;
              linelist[3].P := TVector3f(vector3Init( dnx,  ext.YMax, z)) ;
              linelist[3].N := TVector3f(vector3Init(0,1,0)) ;
              linelist[3].Tu := 1 ;
              linelist[3].Tv := b ;
              linelist[3].Color := $FFFFFFFF ;
            end;
      end;

      System.Move(linelist[0], plinelist_vb.Buffer^, 4 * sizeOf(linelist[0])) ;

      plinelist_ib[0] := 0 ;
      plinelist_ib[1] := 1 ;
      plinelist_ib[2] := 2 ;
      plinelist_ib[3] := 2 ;
      plinelist_ib[4] := 1 ;
      plinelist_ib[5] := 3 ;
      current_tex := pdataTEX_bDX ;
      texture_mtrl.Texture := current_tex ;
      oContext.DrawTriangles(plinelist_vb, plinelist_ib, texture_mtrl, 1.0);
    end;

    current_tex := nil ;

    if Assigned( plinelist_vb ) then FreeObject( plinelist_vb ) ;
    if Assigned( plinelist_ib ) then FreeObject( plinelist_ib ) ;

    if lightSwitch then lightOn ;
    current_lght := lightSwitch ;

    a := Byte(TruncS(basePlane.Transparency/100.0*255.0)) ;
    cl := TAlphaColorF.Create(
      basePlane.GridColor.R/255,
      basePlane.GridColor.G/255,
      basePlane.GridColor.B/255,
      a/255 ).ToAlphaColor ;
    n1 := 60 ; n2 := 20 ;
    z := z + 0.1 * zScaleFactor * ( rangeextent.ZMin - basePlane.Level ) / zUnit ;

    for i := -n1 to n1 do begin
      p1.X := -n1*n2 ;
      p1.Y := i*n2 ;
      p1.Z := z ;
      p2.X := -p1.X ;
      p2.Y := p1.Y ;
      p2.Z := z ;
      oContext.DrawLine(p1, p2, 0.5, cl ) ;
      p1.X := i*n2 ;
      p1.Y := -n1*n2 ;
      p1.Z := z ;
      p2.X := p1.X ;
      p2.Y := -p1.Y ;
      p2.Z := z ;
      oContext.DrawLine(p1, p2, 0.5, cl ) ;
    end;

  end;

  procedure TGIS_Renderer3DFMX.makeSkyBoxTex ;
  var
    bmp : TGIS_Bitmap ;
  begin
    // skybox
    bmp := TGIS_Bitmap.Create ;
    try
      bmp.LoadFromResourceName( 'SKYBOX_SKY' ) ;
      if Assigned( pdataTEX_sDX ) then
          FreeObject( pdataTEX_sDX ) ;
      if (bmp.Width > 0) and (bmp.Height > 0) then
        pdataTEX_sDX := create_texture( bmp ) ;
    finally
      FreeObject( bmp ) ;
    end;

    // sun
    bmp := TGIS_Bitmap.Create ;
    try
      bmp.LoadFromResourceName( 'SKYBOX_SUN' ) ;
      if assigned( pdataTEX_sun ) then
        FreeObject( pdataTEX_sun ) ;
      if (bmp.Width > 0) and (bmp.Height > 0) then
        pdataTEX_sun := create_texture( bmp ) ;
    finally
      FreeObject( bmp ) ;
    end;

  end;

  procedure TGIS_Renderer3DFMX.drawSkyBox ;
  const
    num = 36 ;
  var
    z, x, y, r   : Double                 ;
    linelist     : Array [0..num-1] of TGIS_Renderer3DVertex ;
    vertxlist_vb : TVertexBuffer ;
    indexlist_ib : TIndexBuffer  ;
    n            : Integer  ;

    procedure generate_box ;
    var
      xx, yy, zz, v : Double ;

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
      linelist[0]  := getP( -1, -1, -1, 0.001, 0.665 ) ;
      linelist[1]  := getP( -1, -1,  1, 0.001, 0.334 ) ;
      linelist[2]  := getP( -1,  1, -1, 0.251, 0.665 ) ;
      linelist[3]  := getP( -1, -1,  1, 0.001, 0.334 ) ;
      linelist[4]  := getP( -1,  1, -1, 0.251, 0.665 ) ;
      linelist[5]  := getP( -1,  1,  1, 0.251, 0.334 ) ;

      linelist[6]  := getP( -1,  1, -1, 0.251, 0.665 ) ;
      linelist[7]  := getP( -1,  1,  1, 0.251, 0.334 ) ;
      linelist[8]  := getP(  1,  1, -1, 0.499, 0.665 ) ;
      linelist[9]  := getP( -1,  1,  1, 0.251, 0.334 ) ;
      linelist[10] := getP(  1,  1, -1, 0.499, 0.665 ) ;
      linelist[11] := getP(  1,  1,  1, 0.499, 0.334 ) ;

      linelist[12] := getP(  1,  1, -1, 0.499, 0.665 ) ;
      linelist[13] := getP(  1,  1,  1, 0.499, 0.334 ) ;
      linelist[14] := getP(  1, -1, -1, 0.749, 0.665 ) ;
      linelist[15] := getP(  1,  1,  1, 0.499, 0.334 ) ;
      linelist[16] := getP(  1, -1, -1, 0.749, 0.665 ) ;
      linelist[17] := getP(  1, -1,  1, 0.749, 0.334 ) ;

      linelist[18] := getP(  1, -1, -1, 0.749, 0.665 ) ;
      linelist[19] := getP(  1, -1,  1, 0.749, 0.334 ) ;
      linelist[20] := getP( -1, -1, -1, 0.999, 0.665 ) ;
      linelist[21] := getP(  1, -1,  1, 0.749, 0.334 ) ;
      linelist[22] := getP( -1, -1, -1, 0.999, 0.665 ) ;
      linelist[23] := getP( -1, -1,  1, 0.999, 0.334 ) ;

      linelist[24] := getP( -1, -1,  1, 0.251, 0.001 ) ;
      linelist[25] := getP( -1,  1,  1, 0.251, 0.334 ) ;
      linelist[26] := getP(  1, -1,  1, 0.499, 0.001 ) ;
      linelist[27] := getP( -1,  1,  1, 0.251, 0.334 ) ;
      linelist[28] := getP(  1, -1,  1, 0.499, 0.001 ) ;
      linelist[29] := getP(  1,  1,  1, 0.499, 0.334 ) ;

      linelist[30] := getP( -1, -1, -1, 0.251, 0.999 ) ;
      linelist[31] := getP( -1,  1, -1, 0.251, 0.665 ) ;
      linelist[32] := getP(  1, -1, -1, 0.499, 0.999 ) ;
      linelist[33] := getP( -1,  1, -1, 0.251, 0.665 ) ;
      linelist[34] := getP(  1, -1, -1, 0.499, 0.999 ) ;
      linelist[35] := getP(  1,  1, -1, 0.499, 0.665 ) ;
    end;

    procedure draw_sun ;
    var
      oldworld, newworld, matrot, matrotx, matrotz,
      matrob, mattransl, matscale  : TMatrix3D ;
      ci, cj, si, sj, r : Double   ;
      p : TVector3f ;
      n : Integer  ;

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
      if not assigned( pdataTEX_sun ) then exit ;
      if not basePlane.Active then exit;

      r := 100 ;
      linelist[0]   := getP( -0.1, -1,  0.1, 0.001, 0.999 ) ;
      linelist[1]   := getP( -0.1, -1,  0.3, 0.001, 0.001 ) ;
      linelist[2]   := getP(  0.1, -1,  0.1, 0.999, 0.999 ) ;
      linelist[3]   := getP( -0.1, -1,  0.3, 0.001, 0.001 ) ;
      linelist[4]   := getP(  0.1, -1,  0.1, 0.999, 0.999 ) ;
      linelist[5]   := getP(  0.1, -1,  0.3,  0.999, 0.001 ) ;

      linelist[0].P := vector3Init( -100, -100,  0.0) ;
      linelist[1].P := vector3Init( -100,  100,  0.0) ;
      linelist[2].P := vector3Init(  100, -100,  0.0) ;
      linelist[3].P := vector3Init( -100,  100,  0.0) ;
      linelist[4].P := vector3Init(  100, -100,  0.0) ;
      linelist[5].P := vector3Init(  100,  100,  0.0) ;

      vertxlist_vb := TVertexBuffer.Create(Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE, 6) ;
      System.Move(linelist[0], vertxlist_vb.Buffer^, 6 * sizeOf(linelist[0])) ;

      indexlist_ib := TIndexBuffer.Create(6, TIndexFormat.UInt16);
      for n := 0 to 6 -1 do
        indexlist_ib[n] := n;

      blendOn ;
      oldworld := oContext.CurrentMatrix ;
      matrotx := TMatrix3D.CreateRotationX( DegToRad(spinX) ) ;
      matrotz := TMatrix3D.CreateRotationZ( DegToRad(spinZ) ) ;
      ci  := Cos( sunPosition.X ) ;
      cj  := Cos( sunPosition.Y ) ;
      si  := Sin( sunPosition.X ) ;
      sj  := Sin( sunPosition.Y ) ;
      r := 500 ;
      p := vector3Init(x+r*ci*sj , r*cj+y, z+r*si  ) ;
      mattransl := TMatrix3D.CreateTranslation( Point3D(p.X, p.Y, p.Z)) ;
      matscale := TMatrix3D.CreateScaling( Point3D(0.5, 0.5, 0.5));
      matrot := matrotx * matrotz ;
      matrob := matrot * mattransl ;
      newworld := matscale * matrob ;
      newworld := newworld * oldworld ;
      oContext.SetMatrix( newworld ) ;

      current_tex := pdataTEX_sun ;
      texture_mtrl.Texture := current_tex ;
      oContext.DrawTriangles(vertxlist_vb, indexlist_ib, texture_mtrl, 1.0);
      oContext.SetMatrix( oldworld ) ;
      blendOff ;
      current_tex := nil ;
      if Assigned( vertxlist_vb ) then FreeObject( vertxlist_vb ) ;
      if Assigned( indexlist_ib ) then FreeObject( indexlist_ib ) ;
    end;

  begin
    if not assigned( pdataTEX_sDX ) then exit ;

    if reRenderCall then exit ;

    lightOff ;

    r := 500 ;
    x :=  cameraPositionEx.X ;
    y :=  cameraPositionEx.Y ;
    z :=  cameraPositionEx.Z ;

    generate_box ;

    vertxlist_vb := TVertexBuffer.Create(Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE, num) ;
    System.Move(linelist[0], vertxlist_vb.Buffer^, num * sizeOf(linelist[0])) ;

    indexlist_ib := TIndexBuffer.Create(num, TIndexFormat.UInt16);
    for n := 0 to num -1 do
      indexlist_ib[n] := n;

    current_tex := pdataTEX_sDX ;
    texture_mtrl.Texture := current_tex ;
    oContext.SetContextState(TContextState.csZTestOff);
    oContext.SetContextState(TContextState.csZWriteOff);
    oContext.DrawTriangles(vertxlist_vb, indexlist_ib, texture_mtrl, 1.0);

    current_tex := nil ;

    if Assigned( vertxlist_vb ) then FreeObject( vertxlist_vb ) ;
    if Assigned( indexlist_ib ) then FreeObject( indexlist_ib ) ;

    if lightSwitch then
      draw_sun ;

    oContext.SetContextState(TContextState.csZWriteOn);
    oContext.SetContextState(TContextState.csZTestOn);

    if lightSwitch then lightOn ;
  end;

  procedure TGIS_Renderer3DFMX.makeWallTexture ;
  var
    line           : IntPtr          ;
    pixel, pixel1  : TRGB_Pixel      ;
    cl             : TGIS_Color      ;
    i, k , markval : Integer         ;
    lp             : TGIS_LayerPixel ;
    z1,z2,dz,iso   : Double          ;
    mark           : Boolean         ;
    mltp           : Double          ;
    ptgex, ptg     : TGIS_Point3D    ;
    pix            : TGIS_Pixels     ;

  begin
    if noDem then exit ;
    if curDemSize = 0 then exit ;

    mltp := 1 ;
    z1 := zLevelMin ;
    z2 := zLevelMax ;
    lp :=  curDem[0].Lx ;
    if not Assigned (lp) then exit ;
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

    ptgex := GISPoint3D( 0.5*(lp.ProjectedExtent.XMin +
                              lp.ProjectedExtent.XMax),
                         0.5*(lp.ProjectedExtent.YMin +
                              lp.ProjectedExtent.YMax),
                         0
                        ) ;
    arBitmap_w.LockPixels( pix, True, TGIS_BitmapFormat.Native, TGIS_BitmapLinesOrder.Down );  // ARGB


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
        cl :=  TGIS_Color.FromARGB( 255, pixel1.R, pixel1.G, pixel1.B ) ;
        pix[i*16 + k] := Integer(cl.ARGB) ;
      end;

      for k := 4 to 15 do begin
        cl :=  TGIS_Color.FromARGB( 255, pixel.R, pixel.G, pixel.B ) ;
        pix[i*16 + k] := Integer(cl.ARGB) ;
      end;

      z1 := z1 + dz ;
      z2 := z2 - dz ;
    end ;

    arBitmap_w.UnlockPixels ;

    if Assigned( pdataTEX_w ) then
      FreeObject( pdataTEX_w ) ;
    pdataTEX_w := create_texture( arBitmap_w ) ;

  end;

  function TGIS_Renderer3DFMX.tvValue(
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
    Result := 1.0 * i/( demFrameSize -1 ) ;
  end;

  procedure TGIS_Renderer3DFMX.detailTexture(
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
    _pardata[_k].Tv := (_pardata[_k].P.Y - scrExtent.YMin)/ deltaexty ;
  end;

  function  TGIS_Renderer3DFMX.addDemMeshTile(
    var   _pardata : T_arTextureData   ;
    const _first   : Integer           ;
    const _tri     : Integer
  ) : Integer ;
  var
    i, mtc  : Integer ;
    numvert : Integer ;
    numtri  : Integer ;
    vb      : P_textureVertex ;
    ib      : PDWord ;
    begV    : DWORD ;
    begI    : DWORD ;
    begF    : DWORD ;
    cl      : TGIS_Color;
  const
    siz = 18000 ;

    function createnewmesh : Integer ;
    begin
      Inc(meshDemTilesCounter) ;
      SetLength(meshDemTiles, meshDemTilesCounter) ;
      mtc := meshDemTilesCounter -1 ;

      Result := CreateMeshFVF( siz,
                               siz,
                               0,
                               Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE,
                               TMesh(meshDemTiles[mtc]),
                               pdataVB, cdataVB, idataIB, self
                              ) ;
    end;

  begin
    mtc := meshDemTilesCounter -1 ;
    if _tri = 0 then begin
      Result := 0 ;
      if TMesh(meshDemTiles[mtc]).Saved then exit ;
      cl := extractColorFromShape(5);
      fillBufferInfoEntry(5, 0, 0, cl) ;
      TMesh(meshDemTiles[mtc]).Saved := True ;
      exit;
    end;
    if meshDemTilesCounter = 0 then begin
      Result := createnewmesh ;
      if Result <> 0 then begin
        freeMeshStore ;
        exit ;
      end;
      TMesh(meshDemTiles[mtc]).Append := True ;
    end
    else
    if (TMesh(meshDemTiles[mtc]).NumVert +_tri + 2 > siz) or
        TMesh(meshDemTiles[mtc]).Saved = True then begin
      if TMesh(meshDemTiles[mtc]).Saved = False then begin
        cl := extractColorFromShape(5);
        fillBufferInfoEntry(5, 0, 0, cl) ;
      end;
      Result := createnewmesh ;
      if Result <> 0 then begin
        freeMeshStore ;
        exit ;
      end;
      TMesh(meshDemTiles[mtc]).Append := True ;
    end;

    begF := TMesh(meshDemTiles[mtc]).NumFace ;
    begV := TMesh(meshDemTiles[mtc]).NumVert ;
    begI := 3 * begF ;

    TMesh(meshDemTiles[mtc]).NumFace := TMesh(meshDemTiles[mtc]).NumFace + _tri ;
    TMesh(meshDemTiles[mtc]).NumVert := TMesh(meshDemTiles[mtc]).NumVert +_tri + 2 ;

    // Vertices
    numvert := TMesh(meshDemTiles[mtc]).GetNumVertices ;
    Result :=  TMesh(meshDemTiles[mtc]).LockVertexBuffer(begV, Pointer(vb)) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // adjust received data
    for i := 0 to _tri +1 do begin
       _pardata[i].P.y := -_pardata[i].P.y ;
       _pardata[i].N.x := -_pardata[i].N.x ;
       _pardata[i].N.y := -_pardata[i].N.y ;
    end;


    System.Move( _pardata[_first], vb^, (_tri + 2) * SizeOf(TGIS_Renderer3DVertex)) ;
    Result := TMesh(meshDemTiles[mtc]).UnlockVertexBuffer ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // Indices
    numtri :=  TMesh(meshDemTiles[mtc]).GetNumFaces ;
    Result :=  TMesh(meshDemTiles[mtc]).LockIndexBuffer(begI, Pointer(ib)) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    for i := 0 to _tri - 1 do begin
      if i mod 2 = 0 then begin
        ib^ := DWORD( i + begV ) ;
        Inc( ib ) ;
        ib^ := DWORD( i + begV + 2 ) ;
        Inc( ib ) ;
        ib^ := DWORD( i + begV + 1 ) ;
        Inc( ib ) ;
      end
      else begin
        ib^ := DWORD( i + begV ) ;
        Inc( ib ) ;
        ib^ := DWORD( i + begV + 1 ) ;
        Inc( ib ) ;
        ib^ := DWORD( i + begV + 2 ) ;
        Inc( ib ) ;
      end ;
    end ;

    Result := TMesh(meshDemTiles[mtc]).UnlockIndexBuffer ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;
  end;

  function  TGIS_Renderer3DFMX.addWallMeshTile(
    const _tri     : Integer
  ) : Integer ;
  var
    i, msc  : Integer ;
    numvert : Integer ;
    j       : Integer ;
    vb      : P_textureVertex ;
    cl      : TGIS_Color;
  begin
    Inc(meshWallTilesCounter) ;
    SetLength(meshWallTiles, meshWallTilesCounter) ;
    msc := meshWallTilesCounter -1 ;

    Result := CreateMeshFVF( _tri ,
                             _tri *3,
                             0,
                             Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE,
                             TMesh(meshWallTiles[msc]),
                             pdataVB, cdataVB, idataIB, self
                           ) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // Vertices
    numvert := TMesh(meshWallTiles[msc]).GetNumVertices ;
    Result :=  TMesh(meshWallTiles[msc]).LockVertexBuffer(0, Pointer(vb)) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // adjust received data
    for i := 0 to numvert -1 do begin
       wTLBufT.TriLst[i].N.x := -wTLBufT.TriLst[i].N.x ;
       wTLBufT.TriLst[i].N.y := -wTLBufT.TriLst[i].N.y ;
    end;

    System.Move( wTLBufT.TriLst[0], vb^, numvert * SizeOf(TGIS_Renderer3DVertex)) ;
    Result :=  TMesh(meshWallTiles[msc]).UnlockVertexBuffer ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // Indices
    j := 0 ;
    for i := 0 to _tri - 1 do begin
      TMesh(meshWallTiles[msc]).indices[j]   := WORD(j) ;
      TMesh(meshWallTiles[msc]).indices[j+1] := WORD(j+2) ;
      TMesh(meshWallTiles[msc]).indices[j+2] := WORD(j+1) ;
      j := j + 3 ;
    end;

    cl := extractColorFromShape(6);
    fillBufferInfoEntry(6, 0, 0, cl) ;
  end;

  function TGIS_Renderer3DFMX.getDemBufSize(
    const _row : Integer
  ) : Integer ;
  begin
    Result := TMesh(meshDemTiles[_row]).GetNumVertices ;
  end;

  function TGIS_Renderer3DFMX.getWallBufSize(
    const _row : Integer
  ) : Integer ;
  begin
    Result := TMesh(meshWallTiles[_row]).GetNumVertices ;
  end;

  function TGIS_Renderer3DFMX.northArrow(
    const _value : Boolean
  ) : Integer ;
  var
    linelist   : Array [0..17] of TGIS_Renderer3DVertex ;
    num_val, i   : Integer        ;
    linelist_vb  : TVertexBuffer  ;
    indexlist_ib : TIndexbuffer   ;
    dx, z        : Double         ;
    dnx, dny     : Double         ;
    dt, df       : Double         ;
    a            : Cardinal       ;
  begin
    Result := S_OK ;
    if reRenderCall then exit;
    if (bArrow = False) and (Flood.Active = False) then exit;

    dx := wndXSize ;
    if scrRatio > 1.0 then
      dx := dx * scrRatio ;
    num_val := 18;
    z := zEyeTo+zOffset+1 ;
    if newTransform then begin
      dt := eyeX ;
      df := eyeY ;
    end
    else begin
      dt  := -dTurn   ;
      df  := -dFly    ;
    end;

    // Black north arrow
    linelist[0].P := vector3Init(dt,-(df-dx/10),z) ;
    linelist[0].N := vector3Init(0,1,0) ;
    linelist[0].Color := $FF000000 ;

    linelist[1].P := vector3Init(dt-dx/120,-(df-dx/12.5),z) ;
    linelist[1].N := vector3Init(0,1,0) ;
    linelist[1].Color := $FF000000 ;

    linelist[2].P := vector3Init(dt,-(df-dx/10),z) ;
    linelist[2].N := vector3Init(0,1,0) ;
    linelist[2].Color := $FF000000 ;

    linelist[3].P := vector3Init(dt,-df,z) ;
    linelist[3].N := vector3Init(0,1,0) ;
    linelist[3].Color := $FF000000 ;

    linelist[4].P := vector3Init(dt,-df,z) ;
    linelist[4].N := vector3Init(0,1,0) ;
    linelist[4].Color := $FF000000 ;

    linelist[5].P := vector3Init(dt,-df,defaultZ) ;
    linelist[5].N := vector3Init(0,1,0) ;
    linelist[5].Color := $FF000000 ;

    linelist[6].P := vector3Init(dt,-(df-dx/10),z) ;
    linelist[6].N := vector3Init(0,1,0) ;
    linelist[6].Color := $FF000000 ;

    linelist[7].P := vector3Init(dt+dx/120,-(df-dx/12.5),z) ;
    linelist[7].N := vector3Init(0,1,0) ;
    linelist[7].Color := $FF000000 ;

    // Axes
    z := z - 1 ;
    if newTransform then begin
      dnx := eyeX ;
      dny := eyeY ;
      dt  := 0     ;
      df  := 0     ;
    end
    else begin
      dnx := eyeTo.X ;
      dny := eyeTo.Y ;
      dt  := -dTurn   ;
      df  := -dFly   ;
    end;
    linelist[8].P := vector3Init(dt+dnx,-(df+dny),z) ;
    linelist[8].N := vector3Init(0,1,0) ;
    linelist[8].Color := color2ARGB(255, 255, 0, 0) ;

    linelist[9].P := vector3Init(dt+dnx+dx,-(df+dny),z) ;
    linelist[9].N := vector3Init(0,1,0) ;
    linelist[9].Color := linelist[8].Color ;

    linelist[10].P := vector3Init(dt+dnx,-(df+dny),z) ;
    linelist[10].N := vector3Init(0,1,0) ;
    linelist[10].Color := color2ARGB(255, 0, 255, 0) ;

    linelist[11].P := vector3Init(dt+dnx,-(df+dny+dx),z) ;
    linelist[11].N := vector3Init(0,1,0) ;
    linelist[11].Color := linelist[10].Color ;

    linelist[12].P := vector3Init(dt+dnx,-(df+dny),z) ;
    linelist[12].N := vector3Init(0,1,0) ;
    linelist[12].Color := color2ARGB(255, 0, 0, 255) ;

    linelist[13].P := vector3Init(dt+dnx,-(df+dny),z+dx) ;
    linelist[13].N := vector3Init(0,1,0) ;
    linelist[13].Color := linelist[12].Color ;

    // flood  surface
    z := zScaleFactor * (flood.Level- zLevel) / zUnit;
    a := TruncS(flood.Transparency/100.0*255.0) ;
    a := color2ARGB(a, flood.Color.R, flood.Color.G, flood.Color.B) ;
    dnx := (normExt.XMax - normExt.XMin)/50.0 ;
    dny := (normExt.YMax - normExt.YMin)/50.0 ;

    linelist[14].P.X := normExt.XMin-dnx ;
    linelist[14].P.Y := normExt.YMin-dny ;
    linelist[14].P.Z := z ;
    linelist[14].Color := a ;

    linelist[15].P.X := normExt.XMax+dnx ;
    linelist[15].P.Y := normExt.YMin-dny ;
    linelist[15].P.Z := z ;
    linelist[15].Color := a ;

    linelist[16].P.X := normExt.XMin-dnx ;
    linelist[16].P.Y := normExt.YMax+dny ;
    linelist[16].P.Z := z ;
    linelist[16].Color := a ;

    linelist[17].P.X := normExt.XMax+dnx ;
    linelist[17].P.Y := normExt.YMax+dny ;
    linelist[17].P.Z := z ;
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
    linelist_vb := TVertexBuffer.Create( Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DCOLOR,
                                          num_val
                                        ) ;
    if not Assigned (linelist_vb)  then begin
      Result := Integer($80004005);
      exit ;
    end;

    System.Move( linelist[0], linelist_vb.Buffer^, num_val * sizeOf(TGIS_Renderer3DVertex) ) ;
    if _value then
    if bArrow then begin
      // Create the index buffer
      indexlist_ib := TIndexBuffer.Create(8, TIndexFormat.UInt16);
      for i := 0 to 7 do
        indexlist_ib[i] := i ;
      color_mtrl.Color := linelist[0].Color ;
      indexlist_ib.Length := 8 ;
      oContext.DrawLines(linelist_vb, indexlist_ib, color_mtrl, 1.0);

      for i := 0 to 1 do
        indexlist_ib[i] := 8+i ;
      color_mtrl.Color := linelist[8].Color ;
      indexlist_ib.Length := 2 ;
      oContext.DrawLines(linelist_vb, indexlist_ib, color_mtrl, 1.0);

      for i := 0 to 1 do
        indexlist_ib[i] := 10+i ;
      color_mtrl.Color := linelist[10].Color ;
      oContext.DrawLines(linelist_vb, indexlist_ib, color_mtrl, 1.0);

      for i := 0 to 1 do
        indexlist_ib[i] := 12+i ;
      color_mtrl.Color := linelist[12].Color ;
      oContext.DrawLines(linelist_vb, indexlist_ib, color_mtrl, 1.0);
    end;

    if not _value then
    if flood.Active then begin
      blendOn ;
      indexlist_ib := TIndexBuffer.Create(6, TIndexFormat.UInt16);
      indexlist_ib[0] := 14 ;
      indexlist_ib[1] := 15 ;
      indexlist_ib[2] := 16 ;
      indexlist_ib[3] := 16 ;
      indexlist_ib[4] := 15 ;
      indexlist_ib[5] := 17 ;
      color_mtrl.Color := linelist[14].Color ;
      oContext.DrawTriangles(linelist_vb, indexlist_ib, color_mtrl, 1.0);
      blendOff ;
    end;

    if Assigned( indexlist_ib ) then
      FreeObject( indexlist_ib ) ;
    if Assigned( linelist_vb ) then
      FreeObject( linelist_vb ) ;

    Result := S_OK ;
  end;

  function  TGIS_Renderer3DFMX.doRender(
    const _rows     : DWORD ;
    const _detail   : Boolean
  ) : Integer ;
  var
   i, ip, ik : Integer ;
   mode      : Boolean ;
   hr        : Integer ;
   dat       : T_arTextureData ;
  begin
    if imageTexture = False then begin
      current_tex := pdataTEX_w ;
      curTEX := 0 ;
    end
    else begin
      if _detail then begin
        current_tex := pdataTEX_d ;
        curTEX := 2 ;
      end
      else  begin
        current_tex := pdataTEX_r ;
        curTEX := 1 ;
      end;
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
      YMin :=  roughExtent.YMin ;
      xMax :=  roughExtent.XMax ;
      yMax :=  roughExtent.YMax ;

        detWnd.Left   := vertexBufferSize ;
        detWnd.Right  := 0 ;
        detWnd.Top    := vertexBufferSize ;
        detWnd.Bottom := 0 ;

      mode := False ;
    end;

    if lightSwitch then
      lightOn
    else
      lightOff ;

    current_lght := lightSwitch ;
    firstRowFound := False ;
    currentVL := nil ;
    if (noDem = False) or (noImg = False) then
    if demMeshExists then
      drawMeshTiles
    else begin
      for i:= ip to ik do
        fillVertexBuffer(i, mode) ;

      addDemMeshTile(dat, 0, 0) ;
    end;
    Result := 0 ;
  end;

  function TGIS_Renderer3DFMX.saveWTLBufT
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

    if Assigned( pdataTEX_w ) then
      current_tex := pdataTEX_w ;

    Result := addWallMeshTile( wTLBufT.NumObjects) ;
    if Result <> S_OK then begin
      if Result = -2147024882 then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                            'Out of memory', 5 )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                              'saveWTLBufT', 5 )
    end;

    wTLBufT.NumPoints  := 0 ;
    wTLBufT.NumObjects := 0 ;

    // restore current texture
    case curTEX of
      1 : current_tex := pdataTEX_r ;
      2 : current_tex := pdataTEX_d ;
    end;

    Result := S_OK;
  end;

  function TGIS_Renderer3DFMX.triangleNormal(
    const _v0 : TVector3f ;
    const _v1 : TVector3f ;
    const _v2 : TVector3f
  ) : TVector3f ;
  var
    v0, v1, v2     : TPoint3D ;
    vout, vv1, vv2 : TPoint3D ;
    vout1 : TVector3f ;
  begin
    v0 :=  TPoint3D(_v0) ;
    v1 :=  TPoint3D(_v1) ;
    v2 :=  TPoint3D(_v2) ;
    vv1 := v1 - v0 ;
    vv2 := v2 - v0 ;
    vout := vv1.CrossProduct(vv2) ;
    vout := vout.Normalize ;

    vout1.x :=  vout.x ;
    vout1.y := -vout.y ;
    vout1.z :=  vout.z ;
    Result := vout1 ;
  end;

  function  TGIS_Renderer3DFMX.drawVectorTexture(
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
    pdata : PByte           ;
    hr      : Integer         ;
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
      pdataTEX_vDX := T_textureHelperFX( oTextureHelper ).Get( shp.Texture )
    else
      pdataTEX_vDX := T_textureHelperFX( oTextureHelper ).Get( shp.OutlineTexture ) ;

    hr := pdataVB_Lock( pdata ) ;
    if hr <> 0 then begin
      Result := hr ;
      exit ;
    end;

    case _prim of
        1 : SetLength( pardata, _num ) ;
        0 : SetLength( pardata, _num * 3) ;
    end;

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
        pardata[i].Color := color2ARGB(a,255,255,255) ;
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
          pardata[i].Color := color2ARGB(a,255,255,255) ;
          if i mod 2 = 0 then begin
            if i>=2 then begin
              val := sqrt(sqr(pardata[i].P.X - pardata[i-2].P.X) +
                          sqr(pardata[i].P.Y - pardata[i-2].P.Y)) ;
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

    pdataVB_Unlock( pdata, pardata ) ;

    case _prim of
      1 : triStripToListT(_part, pardata, k) ;
      0 : triListToListT (_part, pardata, k) ;
    end;

    Result := S_OK ;
  end;

  procedure TGIS_Renderer3DFMX.setPolyRoofN(
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
      _ver1.N.x := -_ver1.N.x ;
      _ver1.N.y := -_ver1.N.y ;
      _ver1.N.z := -_ver1.N.z ;
      _ver2.N.x := -_ver2.N.x ;
      _ver2.N.y := -_ver2.N.y ;
      _ver2.N.z := -_ver2.N.z ;
      _ver3.N.x := -_ver3.N.x ;
      _ver3.N.y := -_ver3.N.y ;
      _ver3.N.z := -_ver3.N.z ;
      end;
  end;

  procedure TGIS_Renderer3DFMX.addToVTLBufT(
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
          Inc(curMeshSubset) ;
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
          Inc(curMeshSubset) ;
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
    if length(_buf.TriLst) = 0 then begin
      SetLength( _buf.TriLst ,  initVideoVBSize ) ;
    end;

    if _buf.NumPoints+6 < videoVertexBufferSize then begin
      setSubsets ;
      _buf.TriLst[_buf.NumPoints] := _ptg1 ;
      Inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := _ptg2 ;
      Inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := _ptg3 ;
      Inc(_buf.NumPoints) ;
      Inc(_buf.NumObjects) ;
    end
    else begin
      ptg1 := _ptg1 ;
      ptg2 := _ptg2 ;
      ptg3 := _ptg3 ;
      saveVTLBufT( _part, _buf ) ;
      curMeshSubset := -1  ;
      lastMeshSubsetR := -1 ;
      lastMeshSubsetW := -1 ;
      curMeshTexR     := nil ;
      curMeshTexW     := nil ;
      setSubsets ;
      _buf.TriLst[_buf.NumPoints] := ptg1 ;
      Inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := ptg2 ;
      Inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := ptg3 ;
      Inc(_buf.NumPoints) ;
      Inc(_buf.NumObjects) ;
    end;
  end;

  function TGIS_Renderer3DFMX.saveVTLBufT(
    const _part : Boolean       ;
    var   _buf  : TGIS_Renderer3DTextureTriangleListBuffer
  ) : Integer ;
  begin
    if _buf.NumObjects = 0 then
      begin
        Result := S_OK ;
        exit ;
      end;

      if _part then begin
        addVectMeshTileT( meshVectTilesCounterTR , meshVectTilesTR ,
                            vTLBufTR , vTLBufTR.NumObjects ) ;
        SetLength(transpInfo.PTTR, meshVectTilesCounterTR ) ;
        transpInfo.PTTR[meshVectTilesCounterTR -1] :=
          TruncS( currentVL.Transparency / 100 * 255 ) ;
        addVectorTilesInfo( 4, meshVectTilesCounterTR -1 ) ;
      end
      else begin
        addVectMeshTileT( meshVectTilesCounterTW , meshVectTilesTW ,
                            vTLBufTW , vTLBufTW.NumObjects ) ;
        SetLength(transpInfo.PTTW, meshVectTilesCounterTW ) ;
        transpInfo.PTTW[meshVectTilesCounterTW -1] :=
          TruncS( currentVL.Transparency / 100 * 255 ) ;
        addVectorTilesInfo( 3, meshVectTilesCounterTW -1 ) ;
      end;
      _buf.NumPoints  := 0 ;
      _buf.NumObjects := 0 ;

      Result := S_OK ;

  end;

  function  TGIS_Renderer3DFMX.getWindowSize : TPoint ;
  begin
    Result.X := oContext.Width  ;
    Result.Y := oContext.Height ;
  end;

  function  TGIS_Renderer3DFMX.getTimeTic : DWORD ;
  begin
    Result := GetTickCount ;
  end;

  procedure TGIS_Renderer3DFMX.clearScreen ;
  begin
    oContext.Clear(backColor) ;
  end;

  procedure TGIS_Renderer3DFMX.blendOn ;
  begin
     oContext.SetContextState( TContextState.csAlphaBlendOn ) ;
  end;

  procedure TGIS_Renderer3DFMX.blendOff ;
  begin
//     oContext.SetContextState( TContextState.csAlphaBlendOff ) ;
  end;

  procedure TGIS_Renderer3DFMX.blitToWindow ;
  begin
    // do nothing
  end;

  procedure TGIS_Renderer3DFMX.drawLabels ;
  var
    i       : Integer       ;
    ptc     : TGIS_Point3D  ;
    r, ps3d ,
    radius  : Double        ;
    vec1,
    vRayPos : TVector3f     ;

      function dist( _x1,_y1,_z1 : Single) : Single ;
      var
        c1,c2,c3,s1,s2,s3 : Double ;
        x,y,z             : Double ;
        res               : TVector3f ;

        function rotx : TVector3f ;
        begin
          Result.x := x    + 0    + 0    ;
          Result.y := 0    + c1*y - s1*z ;
          Result.z := 0    + s1*y + c1*z ;
        end ;

        function roty : TVector3f ;
        begin
          Result.x := c2*x + 0    + s2*z ;
          Result.y := 0    + y    + 0    ;
          Result.z :=-s2*x + 0    + c2*z ;
        end ;

        function rotz : TVector3f ;
        begin
          Result.x := c3*x - s3*y + 0    ;
          Result.y := s3*x + c3*y + 0    ;
          Result.z := 0    + 0    + z    ;
        end ;

      begin
        c1 := Cos( DegToRad(spinX) ) ;
        s1 := Sin( DegToRad(spinX) ) ;
        c2 := Cos( DegToRad(spinY) ) ;
        s2 := Sin( DegToRad(spinY) ) ;
        c3 := Cos( DegToRad(spinZ) ) ;
        s3 := Sin( DegToRad(spinZ) ) ;
        x  := _x1 + dTurn ;
        y  := _y1 + dFly ;
        z  := _z1 - zEyeTo ;
        res := rotz ;
        x  := res.x ;
        y  := res.y ;
        z  := res.z ;
        res := roty ;
        x  := res.x ;
        y  := res.y ;
        z  := res.z ;
        res := rotx ;
        Result := radius - res.z ;
      end;

  begin
    if labelModeEx  and ( wasTimer or optionSelected) then
      labelMode := True ;

    if (curVLSize = 0) or (labelMode = False) or (High(labelInfo) = -1) then
      exit ;

    if newTransform then begin
      vec1.x := oContext.Width / 2 ;
      vec1.y := oContext.Height/ 2 ;
      vec1.z := 0.0 ;
      Vec3Unproject(vRayPos,vec1,oContext,mtxProjection,mtxView,mtxWorld,wndWdth,wndHght) ;
      if vRayPos.Z = 0 then vRayPos.Z := cameraPositionEx.Z ;
      radius := Sqrt(Sqr(-dTurn - vRayPos.X) +
                     Sqr(  dFly + vRayPos.Y) +
                     Sqr(zEyeTo - vRayPos.Z) ) ;
    end
    else
      radius := dRadius ;

    ps3d := getPrinterPixelSize( radius ) ;

    if lightSwitch then lightOff ;
    current_lght := lightSwitch ;

    blendOn ;
    for i := 0 to High(labelInfo) do begin
      ptc := GISPoint3D(labelInfo[i].Centroid.X, labelInfo[i].Centroid.Y, 0, 0) ;
      ptc := mapToD3D( ptc ) ;
      ptc.Z := labelInfo[i].LabelZ ;
      r := dist( ptc.X, -ptc.Y, ptc.Z ) ;
      r := r / radius ;
      pixelSize3D := ps3d * r ;
      drawLabel( i ) ;
    end;
    blendOff ;
    if lightSwitch then lightOn ;
    current_lght := lightSwitch ;

    if labelModeEx then begin
      wasTimer  := False ;
      labelMode := False ;
      optionSelected := False ;
    end;
  end;

  procedure TGIS_Renderer3DFMX.drawLabel(
    const _k     : Integer
  ) ;
  var
    ptg       : TGIS_Point3D    ;
    pos       : TGIS_Point      ;
    pardata   : T_arTextureData ;
    lz        : Double          ;
    deltax,
    deltay    : Double          ;
    x,y,z     : Double          ;
    vec1,
    vec2      : TVector3f       ;
    cl        : TGIS_Color      ;
    oldworld,
    newworld,
    matrot,
    matrotx,
    matrotz,
    matrob,
    mattransl,
    matscale  : TMatrix3D     ;
    vertxlist_vb : TVertexBuffer  ;
    indexlist_ib : TIndexbuffer   ;
    ps, margin : Integer ;
  begin
    if labelInfo[_k].LabelTextureDX = nil then
      exit
    else
      pdataTEX_vDX := labelInfo[_k].LabelTextureDX ;

    lz := labelInfo[_k].LabelZ ;

    pos := labelInfo[_k].Centroid ;
    ptg.X := pos.X ;
    ptg.Y := pos.Y ;
    ptg.Z := 0 ;
    ptg.M := 0 ;
    ptg := mapToD3D(ptg) ;

    SetLength( pardata, 4 ) ;

    x := 0 ;
    y := 0 ;
    z := 0 ;

    deltax := (labelInfo[_k].LabelSize.X) * pixelSize3D ;
    deltay := (labelInfo[_k].LabelSize.Y) * pixelSize3D ;

    vec1.X := -deltax-pixelSize3D ;
    vec1.Y := -deltay-pixelSize3D ;
    vec1.Z :=  0 ;

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
             vec1.Y :=  0.0*deltay;
           end;
       2 : begin
             vec1.X := -1.0*deltax ;
             vec1.Y :=  0.0*deltay ;
           end;
       3 : begin
             vec1.X :=  0.0*deltax ;
             vec1.Y :=  0.0*deltay ;
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
             vec1.Y := -2.0*deltay ;
           end;
       8 : begin
             vec1.X := -1.0*deltax ;
             vec1.Y := -2.0*deltay ;
           end;
       9 : begin
             vec1.X :=  0.0*deltax ;
             vec1.Y := -2.0*deltay ;
           end;
    end;

    oldworld := oContext.CurrentMatrix ;
    matrotx := TMatrix3D.CreateRotationX( DegToRad(spinX) ) ;
    matrotz := TMatrix3D.CreateRotationZ( DegToRad(spinZ) ) ;
    mattransl := TMatrix3D.CreateTranslation( Point3D(ptg.X, ptg.Y, lz + deltay)) ;
    matscale := TMatrix3D.CreateScaling( Point3D(1.0, 1.0, 1.0));
    matrot := matrotx * matrotz ;
    matrob := matrot * mattransl ;
    newworld := matscale * matrob ;
    newworld := newworld * oldworld ;
    oContext.SetMatrix( newworld ) ;

    cl := TGIS_Color.FromARGB(255,255,255,255) ;

    pardata[0].P.X := vec1.x-pixelSize3D ;
    pardata[0].P.Y := vec1.y-pixelSize3D ;
    pardata[0].P.Z := z ;
    pardata[0].N   := vector3Init(0,1,0) ;
    pardata[0].Tu  := 0.0 ;
    pardata[0].Tv  := labelInfo[_k].LabelSize.Y / labelInfo[_k].BitmapSize.Y ;
    pardata[0].Color := cl.ARGB ;

    pardata[1].P.X := pardata[0].P.X ;
    pardata[1].P.Y := pardata[0].P.Y + 2.0*deltay ;
    pardata[1].P.Z := z ;
    pardata[1].N   := vector3Init(0,1,0) ;
    pardata[1].Tu  := 0.0 ;
    pardata[1].Tv  := 0.0 ;
    pardata[1].Color := cl.ARGB ;

    pardata[2].P.X := pardata[0].P.X + 2.0*deltax ;
    pardata[2].P.Y := pardata[0].P.Y ;
    pardata[2].P.Z := z ;
    pardata[2].N   := vector3Init(0,1,0) ;
    pardata[2].Tu  := labelInfo[_k].LabelSize.X / labelInfo[_k].BitmapSize.X ;
    pardata[2].Tv  := labelInfo[_k].LabelSize.Y / labelInfo[_k].BitmapSize.Y ;
    pardata[2].Color := cl.ARGB ;

    pardata[3].P.X := pardata[0].P.X + 2.0*deltax ;
    pardata[3].P.Y := pardata[0].P.Y + 2.0*deltay ;
    pardata[3].P.Z := z ;
    pardata[3].N   := vector3Init(0,1,0) ;
    pardata[3].Tu  := labelInfo[_k].LabelSize.X / labelInfo[_k].BitmapSize.X ;
    pardata[3].Tv  := 0.0 ;
    pardata[3].Color := cl.ARGB ;

    // Create the vertex buffer
    vertxlist_vb := TVertexBuffer.Create(Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE,4) ;
    System.Move(pardata[0], vertxlist_vb.Buffer^, 4 * sizeOf(TGIS_Renderer3DVertex)) ;
    // Create the index buffer
    indexlist_ib := TIndexBuffer.Create(6, TIndexFormat.UInt16);
    indexlist_ib[0] := 0 ;
    indexlist_ib[1] := 1 ;
    indexlist_ib[2] := 2 ;
    indexlist_ib[3] := 2 ;
    indexlist_ib[4] := 1 ;
    indexlist_ib[5] := 3 ;
    texture_mtrl.Texture := pdataTEX_vDX ;
    oContext.DrawTriangles(vertxlist_vb, indexlist_ib, texture_mtrl, 1.0);
    oContext.SetMatrix( oldworld ) ;

    SetLength( pardata, 0 ) ;
    if Assigned( indexlist_ib ) then
      FreeObject( indexlist_ib ) ;
    if Assigned( vertxlist_vb ) then
      FreeObject( vertxlist_vb ) ;

  end;

  function  TGIS_Renderer3DFMX.reverseValue(
    const _value : Single
  ) : Single ;
  begin
    Result := -_value ;
  end;

  procedure TGIS_Renderer3DFMX.markSelShape(
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
    r,g,b, a  : Byte       ;

    procedure getMarkColor ;
    var
      cl : TGIS_Color ;
      st : TGIS_ShapeType ;
    begin
      if _mode then begin
          r := _color.R ;
          g := _color.G ;
          b := _color.B ;
          mark_color := color2ARGB(Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)), r, g, b) ;
        exit;
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
            mark_color := color2ARGB(a, r, g, b) ;
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
            mark_color := color2ARGB(a, r, g, b) ;
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
          if  arBufSelInfo[bufposi].ShpInfo[j].Uid <> _shpid then
            break ;
        end;

        if j < arBufSelInfo[bufposi].ShpInfo.Count -1 then
          Inc( j )
        else
          break ;
      end;
      _j2 := arBufSelInfo[bufposi].ShpInfo[j].Offset ;

      if Assigned(_mesh) then
      if arBufSelInfo[bufposi].ShpInfo.Count-1 = j  then
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
                mark_color := color2ARGB(a, b, g, r) ;
              {$ELSE}
                mark_color := color2ARGB(a, r, g, b) ;
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
                mark_color := color2ARGB(a, b, g, r) ;
              {$ELSE}
                mark_color := color2ARGB(a, r, g, b) ;
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
          9 : for i := _j1 to _j2 -1 do begin
            TMesh(_mesh[i0]).tvertices[i].Color := mark_color ;
            colorToTexture(TMesh(_mesh[i0]).tvertices[i].Color,
                           TMesh(_mesh[i0]).tvertices[i].Tu,
                           TMesh(_mesh[i0]).tvertices[i].Tv ) ;
          end;
          10 : for i := _j1 to _j2 - 1 do begin
            TMesh(_mesh[i0]).tvertices[i].Color := mark_color ;
          end;
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

    procedure markMPatch ( var _mesh    : T_arMesh ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if _mode then begin
          if not found(j1, j2, _mesh) then exit ;
          case bt of
            9 : for j := j1 to j2 - 1 do begin
              TMesh(_mesh[i0]).tvertices[j].Color := mark_color ;
              colorToTexture(TMesh(_mesh[i0]).tvertices[j].Color,
                             TMesh(_mesh[i0]).tvertices[j].Tu,
                             TMesh(_mesh[i0]).tvertices[j].Tv ) ;
            end;
            10 : for j := j1 to j2 - 1 do begin
              TMesh(_mesh[i0]).tvertices[j].Color := mark_color ;
            end;
          end;
        end
        else
        if not found1(j1, j2, _mesh) then exit ;
      end;

    procedure markLine( var _mesh    : TGIS_Renderer3DVectorTilesInfo ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, nil) then exit ;
        if j1 = arBufSelInfo[bufposi].ShpInfo[arBufSelInfo[bufposi].ShpInfo.Count-1].Offset then
          j2 := _mesh[i0].Count ;

        for j := j1 to j2 -1 do begin
          _mesh[i0].Buffer[j].Color := mark_color ;
          colorToTexture(_mesh[i0].Buffer[j].Color,
                         _mesh[i0].Buffer[j].Tu,
                         _mesh[i0].Buffer[j].Tv ) ;
        end;
      end;

    procedure markPoint( var _mesh    : TGIS_Renderer3DVectorTilesInfo ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, nil) then exit ;
        if j1 = arBufSelInfo[bufposi].ShpInfo[arBufSelInfo[bufposi].ShpInfo.Count-1].Offset then
          j2 := _mesh[i0].Count ;

        for j := j1 to j2 -1 do begin
          _mesh[i0].Buffer[j].Color := mark_color ;
          colorToTexture(_mesh[i0].Buffer[j].Color,
                         _mesh[i0].Buffer[j].Tu,
                         _mesh[i0].Buffer[j].Tv ) ;
        end;
      end;

      procedure markTriangleC( var _mesh    : T_arMesh ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, _mesh) then exit ;

        for j := j1 to j2 - 1 do begin
          TMesh(_mesh[i0]).tvertices[j].Color := mark_color ;
          colorToTexture(TMesh(_mesh[i0]).tvertices[j].Color,
                         TMesh(_mesh[i0]).tvertices[j].Tu,
                         TMesh(_mesh[i0]).tvertices[j].Tv ) ;
        end;
      end;

      procedure markTriangleT( var _mesh    : T_arMesh ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, _mesh) then exit ;

        for j := j1 to j2 - 1 do
          TMesh(_mesh[i0]).tvertices[j].Color := mark_color ;
      end;

      procedure markTriangleT1( var _mesh    : T_arMesh ) ;
      var
        j, j1, j2 : Integer ;
      begin
        if not found(j1, j2, _mesh) then exit ;

        for j := j1 to j2 - 1 do
          TMesh(_mesh[i0]).tvertices[j].Color := mark_color ;
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

  procedure TGIS_Renderer3DFMX.lightOn ;
  begin
    if Assigned( oLight ) then
      oLight.Enabled := True ;
  end;

  procedure TGIS_Renderer3DFMX.lightOff ;
  begin
    if Assigned( oLight ) then
      oLight.Enabled := True ;
  end;

  function TGIS_Renderer3DFMX.pdataVB_Unlock(
    var _strm : PByte   ;
    var _data : T_arTextureData
  ) : Integer ;
  begin
    Result := 0 ;
  end;

  function TGIS_Renderer3DFMX.cdataVB_Unlock(
    var _strm : PByte        ;
    var _data : T_arColorData
  ) : Integer ;
  begin
    Result := 0 ;
  end;

  function TGIS_Renderer3DFMX.pdataVB_Lock(
    var _strm : PByte
  ) : Integer ;
  begin
    Result := 0 ;
  end;

  function TGIS_Renderer3DFMX.cdataVB_Lock(
    var _strm : PByte
  ) : Integer ;
  begin
    Result := 0 ;
  end;

  procedure TGIS_Renderer3DFMX.reset3DEnvironment ;
  begin
    // do nothing
  end;

  function  TGIS_Renderer3DFMX.restore3DEnvironment   : Integer ;
  begin
    Result := 0 ;
  end;

  procedure TGIS_Renderer3DFMX.fset_Light( const _value : TLight ) ;
  begin
    oLight := _value ;
  end;

  procedure TGIS_Renderer3DFMX.fset_Camera( const _value : TCamera ) ;
  begin
    oCamera := _value ;
  end;

  procedure TGIS_Renderer3DFMX.releaseLabelInfo ;
  var
   i, k : Integer ;
  begin
    k := High( labelInfo ) ;
    if k <> -1 then begin
      for i := k downto 0 do
        FreeObject( labelInfo[i].LabelTextureDX ) ;
      SetLength( labelInfo , 0 ) ;
    end;
  end;

  procedure TGIS_Renderer3DFMX.releaseDataVB ;
  begin
    exit;
    if Assigned( pdataVB ) then
      FreeObject( pdataVB ) ;
    if Assigned( cdataVB ) then
      FreeObject( cdataVB ) ;
    if Assigned( idataIB ) then
      FreeObject( idataIB    ) ;

    // ReCreate Geometry
    if setBuffers <> 0 then
        exit ;
  end;

  function TGIS_Renderer3DFMX.setBuffers
    : Integer;
  var
    hr             : Integer ;
    verticesnumber : DWORD   ;
  begin
    verticesnumber := 2 * vertexBufferSize + 2 ;

    if Assigned( pdataVB ) then FreeObject( pdataVB ) ;
    if Assigned( cdataVB ) then FreeObject( cdataVB ) ;
    if Assigned( idataIB ) then FreeObject( idataIB ) ;

    pdataVB := TVertexBuffer.Create( Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE,
                                     videoVertexBufferSize
                                   ) ;

     if not Assigned(pdataVB) then begin
       hr := -1 ;
       exit ;
     end;

     cdataVB := TVertexBuffer.Create( Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DCOLOR,
                                     videoVertexBufferSize
                                   ) ;

     if not Assigned(cdataVB) then begin
       hr := -1 ;
       exit ;
     end;

     idataIB := TIndexBuffer.Create( videoVertexBufferSize,
                                     TIndexFormat.UInt32
                                   ) ;
     if not Assigned(idataIB) then begin
       hr := -1 ;
       exit ;
     end;
    Result := S_OK ;
  end;

  function TGIS_Renderer3DFMX.createVertexBuffers(
    const _vertexNum: DWORD ;
    out _vbuf        : TVertexBuffer ;
    out _ibuf        : TIndexBuffer
  ) : Integer;
  var
    hr : Integer ;
  begin
    if Assigned( _vbuf ) then FreeObject( _vbuf ) ;
    if Assigned( _ibuf ) then FreeObject( _ibuf ) ;

    _vbuf := TVertexBuffer.Create( Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DCOLOR,
                                     videoVertexBufferSize
                                    ) ;

    if not Assigned(_vbuf) then begin
      Result := -1 ;
      exit ;
    end;

    _ibuf := TIndexBuffer.Create( videoVertexBufferSize,
                                    TIndexFormat.UInt32
                                   ) ;
    if not Assigned(_ibuf) then begin
      Result := -1 ;
      exit ;
    end;
    Result := S_OK ;
  end;

  procedure TGIS_Renderer3DFMX.initLight ;
  var
    val : Double      ;
  begin
    sunposition.X := DegToRad( 30 ) ;
    sunposition.Y := DegToRad( 225 ) ;
    SetSunPosition(GisPoint(sunposition.X,sunposition.Y));
  end;

  procedure TGIS_Renderer3DFMX.addShapeToCache(
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
      minm      : DOuble       ;
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
//          ptg.Z := minz + (ptg.Z - minz) * currentVLnz ;
          if currentVL.Params.NormalizedZ = TGIS_3DNormalizationType.Range then
            ptg.Z := ( ptg.Z + _shpobj.FalseZ - minz ) * currentVLnz
          else
            ptg.Z := minz + ( ptg.Z + _shpobj.FalseZ - minz ) * currentVLnz ;
          z := ptg.Z ;

          currentVL.Project3D_Ref( ptg ) ;
//          ptg.M := minm + (ptg.M - minm) * currentVLnm * projFactor * projMFactor;
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

    _abort   := False ;
    oparams  := _shp.Params ;
    shps     := nil ;
    shp      := nil ;
    shp_type := _shp.ShapeType ;
    currentSHP := _shp ;
    falseMultiPatch := False ;

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
        shps := _shp.GetIntersection(cutVectExtentV, True)  //? check false
      else
        shps := _shp ;
    end
    else begin
      // variant 2  - cutting to the current rough ext. for the rest shp types
      if not _shp.IsInsideExtent(cutVectExtentV , TGIS_InsideType.Full) then
        shps := _shp.GetIntersection(cutVectExtentV, True) //? check false
      else
        shps := _shp ;

      if not Assigned(shps) then begin
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

    if not Assigned( shp ) then begin
      if shp <> _shp then
        FreeObject(shp) ;
      exit ;
    end
    else begin
      shpobj := TGIS_Renderer3DShape.Create(shp_type, _shp.Uid, currentVL) ;
      if not Assigned( shpobj ) then begin
        if shp <> _shp then
          FreeObject(shp) ;
        exit ;
      end;

      Inc(shpNo) ;
      shpobj.Ground       := currentVL.Params.Ground   ;
      shpobj.Basement     := currentVL.Params.Basement ;

      shpobj.ScaleZ       := _shp.Params.ScaleZ ;
      shpobj.FalseZ       := _shp.Params.FalseZ ;

      shpobj.ScaleM       := _shp.Params.ScaleM ;
      shpobj.FalseM       := _shp.Params.FalseM ;

      shpobj.IsSelected   := _shp.IsSelected  ;
      shpobj.LabelBitmap := nil ;

      if labelModeEx then  labelMode := True ;

      if labelMode and not IsStringEmpty( _shp.Params.Labels.Value )   then begin
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
              shpobj.Texture := T_textureHelperFX( oTextureHelper ).Prepare(
                              oparams.Area.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Area.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperFX( oTextureHelper).Prepare(
                              oparams.Area.OutlineBitmap ) ;
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
              shpobj.Texture := T_textureHelperFX( oTextureHelper ).Prepare(
                              oparams.Area.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Area.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperFX( oTextureHelper).Prepare(
                              oparams.Area.OutlineBitmap ) ;
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
              shpobj.Texture := T_textureHelperFX( oTextureHelper ).Prepare(
                              oparams.Line.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Line.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperFX( oTextureHelper).Prepare(
                              oparams.Line.OutlineBitmap ) ;
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
              shpobj.Texture := T_textureHelperFX( oTextureHelper ).Prepare(
                              oparams.Marker.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Marker.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperFX( oTextureHelper).Prepare(
                                oparams.Marker.OutlineBitmap ) ;
            if _shp is TGIS_ShapePointCloud then
              shpobj.VertexColors := TGIS_ShapePointCloud(_shp).VertexColors ;
          end;
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
              shpobj.Texture := T_textureHelperFX( oTextureHelper ).Prepare(
                              oparams.Marker.Bitmap ) ;

            if not TGIS_Bitmap.IsNilOrEmpty( oparams.Marker.OutlineBitmap ) then
              shpobj.OutlineTexture := T_textureHelperFX( oTextureHelper).Prepare(
                              oparams.Marker.OutlineBitmap ) ;
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

    if shpobj.LabelBitmap <> nil then begin // store labelInfo
      k := High( labelInfo) ;
      if  k = -1 then
        SetLength( labelInfo, 1 )
      else
        SetLength( labelInfo, k + 2 ) ;

      labelInfo[k + 1].Centroid   := _shp.Centroid     ;
      labelInfo[k + 1].LabelZ     := shpobj.LabelZ     ;
      labelInfo[k + 1].LabelB     := shpobj.LabelB     ;
      labelInfo[k + 1].LabelSize  := shpobj.LabelSize  ;
      labelInfo[k + 1].LabelPos   := _shp.Params.Labels.Position ;
      labelInfo[k + 1].BitmapSize := shpobj.BitmapSize ;
      labelInfo[k + 1].LabelTextureDX := create_texture( shpobj.LabelBitmap ) ;
    end;

    FreeObject( shpobj ) ;
    if shp <> _shp then
      FreeObject(shp) ;
  end;

  function  TGIS_Renderer3DFMX.drawMultiPatchFromCache(
    const _shp   : TGIS_Shape ;
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
    first_pnt   : TGIS_Point3D  ;
    pardata     : T_arTextureData ;
    cl          : TGIS_Color    ;
    r,g,b       : Byte          ;
    a           : Cardinal      ;
    ar_color    : DWORD         ;
    ou_color    : DWORD         ;
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
    lastTexDX   : TTexture      ;
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
        z, dz, valz : Double        ;
        pnt         : TGIS_Point3D  ;
        kk, jj      : Integer       ;

      begin
        pnt := _ptg ;
        z := pnt.Z ;
        dz := zFactor * (z - shp_minz) * lparams.ScaleZ ;

        currentVL.Project3D_Ref( pnt ) ;
        if bIgnoreEllipsoidHeight then
          pnt.Z := z ;

        pnt.Z := shp_minz + lp_falseZ ;
        pnt.Z := pnt.Z * zFactor ;

        pnt.M := pnt.M + lp_falseM ;
        valx := ( vectExtent.XMax - pnt.X) / vedx ;
        valy := ( vectExtent.YMax - pnt.Y) / vedy ;
        Result.P.X := roughExtent.XMax - valx * redx ;
        Result.P.Y := roughExtent.YMin + valy * redy ;

        // get shp z value from DEM
        kk := demFrameSize - TruncS(valx * demFrameSize) ;
        jj := TruncS(valy * demFrameSize) ;
        // gisGroundOnDem
        if (jj < 0) or (jj >= demFrameSize - 1) or (kk < 0) or
          (kk >= demFrameSize - 1) then // point out of DEM extent
          Result.P.z := defaultZ + (dz + lp_falseZ) / zUnit
        else begin                      // point within DEM extent
          valz := zValue(kk, jj) ;
          if basementType = TGIS_3DBasementType.Lowest then
            valz := min_valz ;
          Result.P.z := valz + (dz + lp_falseZ) / zUnit ;
        end;

        if groundType = TGIS_3DGroundType.AboveDem  then // above DEM
          Result.P.z := Result.P.z + projFactor * pnt.Z / zUnit
        else
        if groundType = TGIS_3DGroundType.AboveZero then // above 0
          Result.P.z := projFactor * (pnt.Z + dz - zLevel) / zUnit ;

        if pnt.M <> 0 then
          Result.P.z := Result.P.z + (pnt.M * projFactor * mFactor) / zUnit ;
        Result.N   := vector3Init( 0, 0 , 1 ) ;
        Result.Color := ar_color ;
      end;

      // closedRing checking
      function closedRing( const _num : Integer ) : Boolean ;
      begin
        if (num_points > 0) and (_num <= num_points) then begin
          if (pardata[0].P.x = pardata[_num-1].P.x) and
             (pardata[0].P.y = pardata[_num-1].P.y) and
             (pardata[0].P.z = pardata[_num-1].P.z) then
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
          ptg1.P.y := -ptg1.P.y ;
          ptg2.P.y := -ptg2.P.y ;
          addToWPLBuf(ptg1, ptg2) ;
        end;
        ptg1.P := pardata[_num - 1].P ;
        ptg1.N := pardata[_num - 1].N ;
        ptg1.Color := ou_color ;
        ptg2.P := pardata[0].P ;
        ptg2.N := pardata[0].N ;
        ptg2.Color := ou_color ;
        ptg1.P.y := -ptg1.P.y ;
        ptg2.P.y := -ptg2.P.y ;
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
        if _wind then begin
          _ver1.N.x := -_ver1.N.x ;
          _ver1.N.y := -_ver1.N.y ;
          _ver2.N.x := -_ver2.N.x ;
          _ver2.N.y := -_ver2.N.y ;
          _ver3.N.x := -_ver3.N.x ;
          _ver3.N.y := -_ver3.N.y ;
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
        ptg1.P.y := -ptg1.P.y ;
        ptg2.P.y := -ptg2.P.y ;
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
        ptg1.P.y := -ptg1.P.y ;
        ptg2.P.y := -ptg2.P.y ;
        ptg3.P.y := -ptg3.P.y ;
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

          TGIS_Tessellation(oPolyRoof).GetTriangleTuv( i, s1, s2, s3,
                        ptg1.Tu, ptg1.Tv, ptg2.Tu, ptg2.Tv, ptg3.Tu, ptg3.Tv ) ;

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
          end;
          if isgdbtex then
            addToMPatchBufT(ptg1, ptg2, ptg3, subsetTexMP)
          else
            bufTtoBufC(ptg1, ptg2, ptg3) ;
          Inc( j ) ;

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
        Inc( iRingNum ) ;
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
          ptg1.Color := ar_color ;
          ptg2.Color := ar_color ;
          ptg3.Color := ar_color ;

          if (ptg1.N.X = 0) and (ptg1.N.Y = 0) then begin
            if bLightVector then
              normVect( ptg1, ptg2, ptg3, True )
            else
              normVect( ptg1, ptg2, ptg3, False ) ;
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
        if (ptype = TGIS_PartType.TriangleStrip) or
           (ptype = TGIS_PartType.TriangleFan  ) or
           (ptype = TGIS_PartType.Triangle     ) then exit ;

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
          Inc( mpatchSubsetCounter ) ;
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
          Inc( mpatchSubsetCounter ) ;
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
        ptr      : Pointer ;
        j, l, l1 : Integer ;
        chcks    : Integer ;
        pix      : TGIS_Pixels ;
        cnt      : Integer ;
        cl32     : Int32 ;
        cl16     : UInt16 ;
        cl8      : UInt8 ;

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
                        Inc( j ) ;
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
                        Inc( j ) ;
                        g := material.Buffer[ j ] ;
                        Inc( j ) ;
                        b := material.Buffer[ j ] ;
                      end ;
                  4 : begin
                        j := 4*(l*material.Width + l1) ;
                        a := material.Buffer[ j ] ;
                        Inc( j ) ;
                        r := material.Buffer[ j ] ;
                        Inc( j ) ;
                        g := material.Buffer[ j ] ;
                        Inc( j ) ;
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
          setSubset ;
        end;

        istexture  := False ;
        ismaterial := False ;
        isedge     := False ;
        isgdbtex   := False ;
        pdataTEX_vDX := nil   ;
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
            if High(mtrlinfo) = -1 then begin
              SetLength(mtrlinfo, 1);
              mtrlinfo[High(mtrlinfo)].MtrlId := matid ;
              mtrlinfo[High(mtrlinfo)].CheckSum := checksum ;
            end
            else begin
              for l := 0 to High(mtrlinfo) do
                 if mtrlinfo[l].MtrlId = matid then begin
                   pdataTEX_vDX := mtrlinfo[l].TexDX ;
                   break;
                 end;
              if l > High(mtrlinfo) then begin
                chcks := checksum ;
                SetLength( mtrlinfo, High(mtrlinfo) + 2 ) ;
                mtrlinfo[High(mtrlinfo)].MtrlId := matid ;
                mtrlinfo[High(mtrlinfo)].CheckSum := chcks ;

                for l1 := 0 to High(mtrlinfo) -1 do
                  if mtrlinfo[l1].CheckSum = chcks then begin
                    pdataTEX_vDX := mtrlinfo[l1].TexDX ;
                    mtrlinfo[High(mtrlinfo)].TexDX := pdataTEX_vDX ;
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
//?        if material.Bpp = 4 then
//?          a := 256 ;       //fix it ?

        // area color
        if material.HasColor then begin
          cl := material.Color ;
          r := cl.R ;
          g := cl.G ;
          b := cl.B ;
          ar_color := color2ARGB(a,r,g,b) ;
        end;

        // edges color
        if material.HasEdgeColor then begin
          isedge := True ;
          cl := material.EdgeColor ;
          r := cl.R ;
          g := cl.G ;
          b := cl.B ;
          ou_color := color2ARGB(a,r,g,b) ;
        end;

        // bitmap texture
        if isgdbtex then begin
          if (not Assigned( pdataTEX_vDX )) then begin
            case material.CompressionType of
              TGIS_CompressionType.JPEG :
                        pdataTEX_vDX := create_texture1( material ) ;
              TGIS_CompressionType.JPEGPlus :
                        pdataTEX_vDX := create_texture1( material ) ;
              TGIS_CompressionType.None : begin
                bmp := TGIS_Bitmap.Create( material.Width, material.Height ) ;
                bmp.LockPixels( pix, False, TGIS_BitmapFormat.ABGR,
                                TGIS_BitmapLinesOrder.Down
                              ) ;
                setbmp ;
                bmp.UnlockPixels ;
                pdataTEX_vDX := create_texture( bmp ) ;
                FreeObject( bmp ) ;
              end;
              TGIS_CompressionType.ARGB :
                        pdataTEX_vDX := create_texture2( material ) ;
              else begin
                pdataTEX_vDX := nil
              end;
            end;  // case

            if Assigned( pdataTEX_vDX ) then
              mtrlinfo[High(mtrlinfo)].TexDX := pdataTEX_vDX ;

          end; // not Assigned( pdataTEX_v )

          if Assigned( pdataTEX_vDX ) then begin
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
      ar_color := color2ARGB(a,r,g,b) ;

      cl := _shp.Params.Area.OutlineColor ;
      r := cl.R ;
      g := cl.G ;
      b := cl.B ;
      ou_color := color2ARGB(a,r,g,b) ;
    end;

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
        Inc( pnt_counter ) ;
        if (ptg.M < ZMMIN) or (ptg.M > ZMMAX) then ptg.M := 0 ;
        if (ptg.Z < ZMMIN) or (ptg.Z > ZMMAX) then ptg.Z := 0 ;
        if (ptype <> TGIS_PartType.TriangleStrip) and
           (ptype <> TGIS_PartType.TriangleFan  ) and
           (ptype <> TGIS_PartType.Triangle     ) then begin
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
          if isgdbtex then begin
            pardata[point_no].Tu :=
              TGIS_ShapeMultiPatch(_shp).Textures.GetTextureCoord(
                                                    part_no, td*point_no) ;
            pardata[point_no].Tv :=
              TGIS_ShapeMultiPatch(_shp).Textures.GetTextureCoord(
                                                    part_no, td*point_no+1) ;
          end;
          if num_normals > pnt_counter then begin
            pardata[point_no].N.x := TGIS_ShapeMultiPatch(_shp).
                                     Normals.Normal[pnt_counter].X ;
            pardata[point_no].N.y := TGIS_ShapeMultiPatch(_shp).
                                     Normals.Normal[pnt_counter].Y ;
            pardata[point_no].N.z := TGIS_ShapeMultiPatch(_shp).
                                     Normals.Normal[pnt_counter].Z ;
            if not bLightVector then begin
              pardata[point_no].N.x := -pardata[point_no].N.x ;
              pardata[point_no].N.y := -pardata[point_no].N.y ;
              pardata[point_no].N.z := -pardata[point_no].N.z ;
            end;
            Vec3Normalize(pardata[point_no].N, pardata[point_no].N) ;
          end;
        end;

        if (not falseMultiPatch) and isnormal then begin
          pardata[point_no].N.x := TGIS_ShapeMultiPatch(_shp).
                                   Normals.Normal[part_no].X ;
          pardata[point_no].N.y := TGIS_ShapeMultiPatch(_shp).
                                   Normals.Normal[part_no].Y ;
          pardata[point_no].N.z := TGIS_ShapeMultiPatch(_shp).
                                   Normals.Normal[part_no].Z ;
          if not bLightVector then begin
              pardata[point_no].N.x := -pardata[point_no].N.x ;
              pardata[point_no].N.y := -pardata[point_no].N.y ;
              pardata[point_no].N.z := -pardata[point_no].N.z ;
            end;
          Vec3Normalize(pardata[point_no].N, pardata[point_no].N) ;
        end ;

      end;

      if (ptype <> TGIS_PartType.TriangleStrip) and   // rings only
         (ptype <> TGIS_PartType.TriangleFan  ) and
         (ptype <> TGIS_PartType.Triangle     ) then begin

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
    ptg := GISPoint3D( _shp.Centroid.X, _shp.Centroid.Y, _shp.PointsZMax ) ;
    _lbz := getScaledPoint3D( ptg ).P.Z ;
    _lbb := _lbz ;

    SetLength( mtrlinfo, 0 ) ;
  end;

  function TGIS_Renderer3DFMX.loadTexture(
    const _bmp    : TGIS_Bitmap ;
    const _detail : Boolean
  ) : Integer;
  var
    hr : Integer ;
  begin
    if _detail then begin
      if Assigned( pdataTEX_d ) then
        FreeObject( pdataTEX_d ) ;
        pdataTEX_d := create_texture( _bmp ) ;
    end
    else  begin
      if Assigned( pdataTEX_r ) then
        FreeObject( pdataTEX_r ) ;
        pdataTEX_r := create_texture( _bmp ) ;
    end;

    Result := S_OK ;
  end;

  procedure TGIS_Renderer3DFMX.drawMeshTiles ;
  var
    i, mtc : Integer ;
  begin
    if meshDemTilesCounter = 0 then exit ;
    if reRenderCall then exit;

    // Draw DEM mesh (rough)
    if meshDemTilesCounter > 0 then begin
      if imageTexture  then
        current_tex := pdataTEX_r
      else
        current_tex := pdataTEX_w ;
      mtc := meshDemDetailIndex -1 ;
      for i := 0 to mtc do
        TMesh(meshDemTiles[i]).DrawSubset(0) ;
    end;

    // Draw DEM mesh (detail)
    if meshDemDetailIndex < meshDemTilesCounter then begin
     if imageTexture  then
        current_tex := pdataTEX_d
      else
        current_tex := pdataTEX_w ;
      mtc := meshDemTilesCounter -1 ;
      for i := meshDemDetailIndex to mtc do
        TMesh(meshDemTiles[i]).DrawSubset(0) ;
    end;

    // Draw DEM walls
    if meshWallTilesCounter > 0 then begin
      if Assigned( pdataTEX_w ) then current_tex := pdataTEX_w ;
      mtc := meshWallTilesCounter -1 ;
      for i := 0 to mtc do
        TMesh(meshWallTiles[i]).DrawSubset(0) ;

      // restore current texture
      case curTEX of
        1 : current_tex := pdataTEX_r ;
        2 : current_tex := pdataTEX_d ;
      end;
    end;
  end;

  procedure TGIS_Renderer3DFMX.drawVectTiles ;
  var
    i, j, k, mtc : Integer ;
  begin
//    current_tex := nil ;
    current_tex := pdataTEX_cDX ;
    // Draw Vector color mesh Roofs
    if meshVectTilesCounterCR > 0 then begin
      mtc := meshVectTilesCounterCR -1 ;
      for i := 0 to mtc do begin
        k := Length(TMesh(meshVectTilesCR[i]).subsets) ;
        if k = 0 then k := 1;
        for j := 0 to k -1 do begin
          if transpInfo.PCTR[i] <> 255 then begin
            blendOn ;
            current_opc := transpInfo.PCTR[i] / 255 ;
          end
          else begin
            blendOff ;
            current_opc := 1.0 ;
          end;

          TMesh(meshVectTilesCR[i]).DrawSubset(j) ;
        end;
      end;
      blendOff ;
      current_opc := 1.0 ;
    end;

    // Draw Vector color mesh Walls
    if meshVectTilesCounterCW > 0 then begin
      mtc := meshVectTilesCounterCW -1 ;
      for i := 0 to mtc do begin
        k := Length(TMesh(meshVectTilesCW[i]).subsets) ;
        if k = 0 then k := 1;
        for j := 0 to k -1 do begin
          if transpInfo.PCTW[i] <> 255 then begin
            blendOn ;
            current_opc := transpInfo.PCTW[i] / 255 ;
          end
          else begin
            blendOff ;
            current_opc := 1.0 ;
          end;

          TMesh(meshVectTilesCW[i]).DrawSubset(j) ;
        end;
      end;
      blendOff ;
      current_opc := 1.0 ;
    end;

    // Draw Vector textured mesh Roofs
    if meshVectTilesCounterTR > 0 then begin
      mtc := meshVectTilesCounterTR -1 ;
      for j := 0 to mtc do begin

        if transpInfo.PTTR[j] <> 255 then begin
          blendOn ;
          current_opc := transpInfo.PTTR[j] / 255 ;
        end
        else begin
          blendOff ;
          current_opc := 1.0 ;
        end;

        k := High(meshTileInfoR[j]) ;
        for i := 0 to k do begin
          current_tex := meshTileInfoR[j][i].TexDX ;
          TMesh(meshVectTilesTR[j]).DrawSubset(i) ;
        end;
      end;
      blendOff ;
      current_opc := 1.0 ;
    end;

     // Draw Vector textured mesh Walls
    if meshVectTilesCounterTW > 0 then begin
      mtc := meshVectTilesCounterTW -1 ;
      for j := 0 to mtc do begin

        if transpInfo.PTTW[j] <> 255 then begin
          blendOn ;
          current_opc := transpInfo.PTTW[j] / 255 ;
        end
        else begin
          blendOff ;
          current_opc := 1.0 ;
        end;

        k := High(meshTileInfoW[j]) ;
        for i := 0 to k do begin
          current_tex := meshTileInfoW[j][i].TexDX ;
          TMesh(meshVectTilesTW[j]).DrawSubset(i) ;
        end;
      end;
      blendOff ;
      current_opc := 1.0 ;
    end;

  end;

  procedure TGIS_Renderer3DFMX.drawMeshLinTiles ;
  var
    i, j, k : Integer        ;
    pardata : T_arColorData  ;
    cdata : PByte            ;
    ibuf     : TIndexBuffer  ;
  begin
    // Draw lines from mesh
    if meshLineTilesCounter > 0 then begin
      current_tex := pdataTEX_cDX ;
      for i := 0 to meshLineTilesCounter -1  do begin
        k := meshLineTiles[i].Count ;
        cdataVB.Length := k ;
        System.Move(
                     meshLineTiles[i].Buffer[0],
                     cdataVB.Buffer^,
                     k * sizeOf(TGIS_Renderer3DVertex)
                    ) ;

       ibuf := TIndexBuffer.Create( k, TIndexFormat.UInt16 ) ;
       for j := 0 to k -1 do
         ibuf[j] := j ;

        if transpInfo.LT[i] <> 255 then begin
          blendOn ;
          current_opc := transpInfo.LT[i] / 255 ;
        end
        else begin
          blendOff ;
          current_opc := 1.0 ;
        end;

       texture_mtrl.Texture := current_tex ;
       oContext.DrawLines(cdataVB, ibuf, texture_mtrl, 1.0);
       FreeObject( ibuf ) ;
      end;
      blendOff ;
      current_opc := 1.0 ;
    end;

  end;

  procedure TGIS_Renderer3DFMX.drawMeshPntTiles ;
  var
    i, j ,k : Integer        ;
    cdata   : PByte          ;
    pardata : T_arColorData  ;
    ibuf    : TIndexBuffer   ;
  begin
    // Draw lines from mesh
    if meshPointTilesCounter > 0 then begin
      current_tex := pdataTEX_cDX ;
      for i := 0 to meshPointTilesCounter -1  do begin
        k := meshPointTiles[i].Count ;
        System.Move(
                    meshPointTiles[i].Buffer[0],
                    cdataVB.Buffer^,
                    k * sizeOf(TGIS_Renderer3DVertex)
                   );
        ibuf := TIndexBuffer.Create( k, TIndexFormat.UInt16 ) ;
        for j := 0 to k -1 do
          ibuf[j] := j ;

        if transpInfo.PT[i] <> 255 then begin
          blendOn ;
          current_opc := transpInfo.LT[i] / 255 ;
        end
        else begin
          blendOff ;
          current_opc := 1.0 ;
        end;

        texture_mtrl.Texture := current_tex ;
        oContext.DrawPoints(cdataVB, ibuf, texture_mtrl, 1.0);
        FreeObject( ibuf ) ;
      end;
      blendOff ;
      current_opc := 1.0 ;
    end;

  end;

  procedure TGIS_Renderer3DFMX.drawMPatchTiles ;
  var
    i, j, k  : Integer ;
  begin
    // Draw MultiPatch mesh (color)
    if meshMPatchTilesCounterC > 0 then begin
      for i := 0 to meshMPatchTilesCounterC -1 do begin
        k := Length( TMesh(meshMPatchTilesC[i]).subsets ) ;
        if k = 0 then k := 1 ;
        for j := 0 to k -1 do begin
          if transpInfo.MPC[i] <> 255 then begin
            blendOn ;
            current_opc := transpInfo.MPC[i] / 255 ;
          end
          else begin
            blendOff ;
            current_opc := 1.0 ;
          end;

          TMesh(meshMPatchTilesC[i]).DrawSubset(j) ;
        end;
      end;
      blendOff ;
      current_opc := 1.0 ;
    end;

    // Draw MultiPatch mesh (texture)
    if meshMPatchTilesCounterT > 0 then begin
      for j := 0 to meshMPatchTilesCounterT -1 do begin
        k := High(mpatchTileInfo[j]) ;
        for i := 0 to k do begin
          if transpInfo.MPT[j] <> 255 then begin
            current_opc := transpInfo.MPT[j] / 255 ;
            blendOn ;
          end
          else begin
            if mpatchTileInfo[j][i].Transp <> 255 then begin
              current_opc := mpatchTileInfo[j][i].Transp / 255 ;
              blendOn ;
            end
            else begin
              current_opc := 1.0 ;
              blendOff ;
            end;
          end;
          current_tex := mpatchTileInfo[j][i].TexDX ;
          TMesh(meshMPatchTilesT[j]).DrawSubset(i) ;
        end ;
      end ;
      current_opc := 1.0 ;
      blendOff ;
    end;
  end;

  function TGIS_Renderer3DFMX.saveVTLBufC(
    const _part : Boolean ;
    var   _buf  : TGIS_Renderer3DColorTriangleListBuffer
  ) : Integer ;
  begin
    if _buf.NumObjects = 0 then
      begin
        Result := S_OK ;
        exit ;
      end;

      if _part then begin
        Result := addVectMeshTileC( meshVectTilesCounterCR ,
                                      meshVectTilesCR        ,
                                      vTLBufCR               ,
                                      vTLBufCR.NumObjects
                                     ) ;
        if Result <> S_OK then begin
          if (Result = -2147024882) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                'Out of memory', 3 )
          else
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                'saveVTLBufC', 3 )
        end;

        SetLength(transpInfo.PCTR, meshVectTilesCounterCR ) ;
        transpInfo.PCTR[meshVectTilesCounterCR -1] :=
          TruncS( currentVL.Transparency / 100 * 255 ) ;
        addVectorTilesInfo( 2, meshVectTilesCounterCR -1 ) ;
      end
      else begin
        Result := addVectMeshTileC( meshVectTilesCounterCW ,
                                      meshVectTilesCW        ,
                                      vTLBufCW               ,
                                      vTLBufCW.NumObjects
                                    ) ;
        if Result <> S_OK then begin
          if (Result = -2147024882) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                'Out of memory', 4 )
          else
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                'saveVTLBufC', 4 )
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

  function  TGIS_Renderer3DFMX.addVectMeshTileC(
    var   _count   : Integer              ;
    var   _mesh    : T_arMesh             ;
    var   _buf     : TGIS_Renderer3DColorTriangleListBuffer ;
    const _tri     : Integer
  ) : Integer ;
  var
    i, mtc  : Integer ;
    numvert : Integer ;
    j,k,l   : Integer ;
    aa, bb  : Byte    ;
    subs    : Boolean ;
    vb      : P_colorVertex ;
    id      : PDWord ;
    mesh    : TMesh ;
    vbuf    : TVertexBuffer ;
    ibuf    : TIndexBuffer ;
    hr      : Integer ;
  begin
    vbuf := nil ;
    ibuf := nil ;
    hr := createVertexBuffers( _buf.NumPoints, vbuf, ibuf ) ;
    if hr <> 0 then  begin
      Result := hr ;
      exit ;
    end;

    Result := CreateMeshFVF( _tri,
                             _tri *3,
                             0,
                             Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DCOLOR,
                             mesh,
                             vbuf, nil, ibuf, self
                           ) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    Inc(_count) ;
    SetLength(_mesh, _count) ;
    mtc := _count -1 ;
    _mesh[mtc] := mesh ;


    // Vertices
    numvert := TMesh(_mesh[mtc]).GetNumVertices ;
    Result :=  TMesh(_mesh[mtc]).LockVertexBuffer(0, Pointer(vb)) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;
    // set texture coordinates from color ramp
    for i := 0 to numvert -1 do
      colorToTexture(_buf.TriLst[i].Color,_buf.TriLst[i].Tu,_buf.TriLst[i].Tv) ;

    System.Move( _buf.TriLst[0], vb^, numvert * SizeOf(TGIS_Renderer3DVertex)) ;
    Result :=  TMesh(_mesh[mtc]).UnlockVertexBuffer ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // Indices
    j := 0 ;
    for i := 0 to _tri - 1 do begin
      TMesh(_mesh[mtc]).indices[j]   := DWORD(j) ;
      TMesh(_mesh[mtc]).indices[j+1] := DWORD(j+2) ;
      TMesh(_mesh[mtc]).indices[j+2] := DWORD(j+1) ;
      j := j + 3 ;
    end;

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

  function  TGIS_Renderer3DFMX.addVectMeshTileT(
    var   _count   : Integer               ;
    var   _mesh    : T_arMesh              ;
    var   _buf     : TGIS_Renderer3DTextureTriangleListBuffer ;
    const _tri     : Integer
  ) : Integer ;
  var
    i, mtc  : Integer ;
    numvert : Integer ;
    j       : Integer ;
    subs    : DWORD ;
    vb      : P_textureVertex ;
    id      : PDWord ;
    mesh    : TMesh ;
    vbuf    : TVertexBuffer ;
    ibuf    : TIndexBuffer ;
    hr      : Integer ;
  begin
    vbuf := nil ;
    ibuf := nil ;
    hr := createVertexBuffers( _buf.NumPoints, vbuf, ibuf ) ;
    if hr <> 0 then  begin
      Result := hr ;
      exit ;
    end;


    Result := CreateMeshFVF( _tri,
                             _tri *3,
                             0,
                             Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE,
                             mesh,
                             vbuf, nil, ibuf, self
                           ) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    Inc(_count) ;
    SetLength(_mesh, _count) ;
    mtc := _count -1 ;
    _mesh[mtc] := mesh ;

    // Vertices
    numvert := TMesh(_mesh[mtc]).GetNumVertices ;
    Result :=  TMesh(_mesh[mtc]).LockVertexBuffer(0, Pointer(vb)) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    System.Move( _buf.TriLst[0], vb^, numvert * SizeOf(TGIS_Renderer3DVertex)) ;

    Result := TMesh(_mesh[mtc]).UnlockVertexBuffer ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // Indices
    j := 0 ;
    for i := 0 to _tri - 1 do begin
      TMesh(_mesh[mtc]).indices[j]   := DWORD(j) ;
      TMesh(_mesh[mtc]).indices[j+1] := DWORD(j+2) ;
      TMesh(_mesh[mtc]).indices[j+2] := DWORD(j+1) ;
      j := j + 3 ;
    end;

    // subsets
    Result :=  TMesh(_mesh[mtc]).LockAttributeBuffer(0, id) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    subs := 0 ;

    for i := 0 to _tri - 1 do begin
      id^ := _buf.Subset[i] ;
      if _buf.Subset[i] > subs then
        subs := _buf.Subset[i] ;
      Inc( id ) ;
    end ;

    Result := TMesh(_mesh[mtc]).UnlockAttributeBuffer ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    TMesh(_mesh[mtc]).SortIndices( subs +1 ) ;

  end;

  procedure TGIS_Renderer3DFMX.addToMPatchBufT(
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
    if length(mpatchBufT.TriLst) = 0 then
      SetLength( mpatchBufT.TriLst ,  initVideoVBSize ) ;
    if length(mpatchBufT.Subset) = 0 then
      SetLength( mpatchBufT.Subset ,  TruncS(length(mpatchBufT.TriLst) / 3) +1 ) ;

    ptg1 := _ptg1 ;
    ptg2 := _ptg2 ;
    ptg3 := _ptg3 ;
    ptg1.P.y := - ptg1.P.y ;
    ptg2.P.y := - ptg2.P.y ;
    ptg3.P.y := - ptg3.P.y ;

    if mpatchBufT.NumPoints+3 < videoVertexBufferSize then begin
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg1 ;
      Inc(mpatchBufT.NumPoints) ;
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg2 ;
      Inc(mpatchBufT.NumPoints) ;
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg3 ;
      mpatchBufT.Subset[mpatchBufT.NumObjects] := _subs ;
      Inc(mpatchTileInfo[meshMPatchTilesCounterT][_subs].Count) ;
      Inc(mpatchBufT.NumPoints) ;
      Inc(mpatchBufT.NumObjects) ;
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
      Inc(mpatchBufT.NumPoints) ;
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg2 ;
      Inc(mpatchBufT.NumPoints) ;
      mpatchBufT.TriLst[mpatchBufT.NumPoints] := ptg3 ;
      mpatchBufT.Subset[mpatchBufT.NumObjects] := mpatchSubsetCounter ;
      Inc(mpatchTileInfo[meshMPatchTilesCounterT][mpatchSubsetCounter].Count) ;
      Inc(mpatchBufT.NumPoints) ;
      Inc(mpatchBufT.NumObjects) ;
      subsetTexMP := mpatchSubsetCounter ;
    end;
  end;

  function TGIS_Renderer3DFMX.saveMPatchBufT
    : Integer ;
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
      if (Result = -2147024882) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'Out of memory', 2 )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'saveMPatchBufT', 2 )
    end;

    mpatchBufT.NumPoints  := 0 ;
    mpatchBufT.NumObjects := 0 ;
    SetLength( mpatchBufT.TriLst, 0 ) ;

    SetLength(transpInfo.MPT, meshMPatchTilesCounterT ) ;
    transpInfo.MPT[meshMPatchTilesCounterT -1] :=
      TruncS( currentVL.Transparency / 100 * 255 ) ;

    mpatchSubsetCounter := -1 ;

    Inc( mpatchSubsetCounter ) ;
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

  function TGIS_Renderer3DFMX.saveMPatchBufC
    : Integer ;
  begin
    if mpatchBufC.NumObjects = 0 then
      begin
        Result := S_OK ;
        exit ;
      end;

     Result := addVectMeshTileC( meshMPatchTilesCounterC ,
                                 meshMPatchTilesC        ,
                                 mpatchBufC              ,
                                 mpatchBufC.NumObjects
                                ) ;

    if Result <> S_OK then begin
      if (Result = -2147024882) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'Out of memory', 1 )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                             'saveMPatchBufC', 1 )
    end;

    mpatchBufC.NumPoints  := 0 ;
    mpatchBufC.NumObjects := 0 ;
    SetLength( mpatchBufC.TriLst, 0 ) ;

    SetLength(transpInfo.MPC, meshMPatchTilesCounterC ) ;
    transpInfo.MPC[meshMPatchTilesCounterC -1] :=
      TruncS( currentVL.Transparency / 100 * 255 ) ;
    addVectorTilesInfo( 9, meshMPatchTilesCounterC -1 ) ;
    Result := S_OK ;
  end;

  function TGIS_Renderer3DFMX.saveTinBufT
    : Integer;
  begin
    if tinBufT.NumObjects = 0 then begin
      Result := S_OK ;
      exit ;
    end;
    if Assigned( pdataTEX_tDX ) then
      current_tex := pdataTEX_tDX ;

    inherited ;
    Result := S_OK ;
  end;

  function  TGIS_Renderer3DFMX.addTinMeshTileC(
    const _tri     : Integer
  ) : Integer ;
  var
    i, mtc  : Integer ;
    numvert : Integer ;
    j,k,l   : Integer ;
    vb      : P_colorVertex ;
    subs    : Boolean ;
    id      : PDWord  ;
    mesh    : TMesh ;
    vbuf    : TVertexBuffer ;
    ibuf    : TIndexBuffer ;
    hr      : Integer ;
 begin
   vbuf := nil ;
   ibuf := nil ;
   hr := createVertexBuffers( tinBufC.NumPoints, vbuf, ibuf ) ;
   if hr <> 0 then  begin
     Result := hr ;
     exit ;
   end;

   Result := CreateMeshFVF( _tri,
                            _tri *3,
                            0,
                            Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DCOLOR,
                            mesh,
                            vbuf, nil, ibuf, self
                          ) ;
   if Result <> 0 then begin
     freeMeshStore ;
     exit ;
   end;

   Inc(meshTinTilesCounterC) ;
   SetLength(meshTinTilesC, meshTinTilesCounterC) ;
   mtc := meshTinTilesCounterC -1 ;
   TMesh(meshTinTilesC[mtc]) := mesh ;

   SetLength(transpInfo.TCT, meshTinTilesCounterC ) ;

   if assigned (currentML ) then
     transpInfo.TCT[mtc] := TruncS( currentML.Transparency / 100 * 255 )
   else
     transpInfo.TCT[mtc] := TruncS( currentVL.Transparency / 100 * 255 ) ;

   // Vertices
   numvert := TMesh(meshTinTilesC[mtc]).GetNumVertices ;

   Result :=  TMesh(meshTinTilesC[mtc]).LockVertexBuffer(0, Pointer(vb)) ;
   if Result <> 0 then begin
     freeMeshStore ;
     exit ;
   end;

   // set texture coordinates from color ramp
   for i := 0 to numvert -1 do
     colorToTexture(tinBufC.TriLst[i].Color,
                    tinBufC.TriLst[i].Tu, tinBufC.TriLst[i].Tv) ;

   System.Move( tinBufC.TriLst[0], vb^, numvert * SizeOf(TGIS_Renderer3DVertex)) ;
   Result :=  TMesh(meshTinTilesC[mtc]).UnlockVertexBuffer ;
   if Result <> 0 then begin
     freeMeshStore ;
     exit ;
   end;

   // Indices
   j := 0 ;
   for i := 0 to _tri - 1 do begin
     TMesh(meshTinTilesC[mtc]).indices[j]   := DWORD(j) ;
     TMesh(meshTinTilesC[mtc]).indices[j+1] := DWORD(j+2) ;
     TMesh(meshTinTilesC[mtc]).indices[j+2] := DWORD(j+1) ;
     j := j + 3 ;
   end;

   TMesh(meshTinTilesC[mtc]).DrawSubset(0) ;
 end;

  function  TGIS_Renderer3DFMX.addTinMeshTileT(
    const _tri     : Integer
  ) : Integer ;
  var
    i, mtc  : Integer ;
    numvert : Integer ;
    j       : Integer ;
    vb      : P_textureVertex ;
    mesh    : TMesh ;
    vbuf    : TVertexBuffer ;
    ibuf    : TIndexBuffer ;
    hr      : HResult ;
  begin
    hr := createVertexBuffers( tinBufT.NumPoints, vbuf, ibuf ) ;
    if hr <> 0 then  begin
      Result := hr ;
      exit ;
    end;

    Result := CreateMeshFVF( _tri,
                             _tri *3,
                             0,
                             Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE,
                             mesh,
                             vbuf, nil, ibuf, self
                           ) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    Inc(meshTinTilesCounterT) ;
    SetLength(meshTinTilesT, meshTinTilesCounterT) ;
    mtc := meshTinTilesCounterT -1 ;
    TMesh(meshTinTilesT[mtc]) := mesh ;

    SetLength(transpInfo.TTT, meshTinTilesCounterT ) ;
    if assigned( currentML ) then
      transpInfo.TTT[mtc] := TruncS( currentML.Transparency / 100 * 255 )
    else
      transpInfo.TTT[mtc] := TruncS( currentVL.Transparency / 100 * 255 ) ;

    SetLength(arTexTinInfo, meshTinTilesCounterT ) ;
      arTexTinInfo[mtc] := pdataTEX_tDX ;

    // Vertices
    numvert := TMesh(meshTinTilesT[mtc]).GetNumVertices ;

    Result :=  TMesh(meshTinTilesT[mtc]).LockVertexBuffer(0, Pointer(vb)) ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    System.Move( tinBufT.TriLst[0], vb^, numvert * SizeOf(TGIS_Renderer3DVertex)) ;

    Result := TMesh(meshTinTilesT[mtc]).UnlockVertexBuffer ;
    if Result <> 0 then begin
      freeMeshStore ;
      exit ;
    end;

    // Indices
    j := 0 ;
    for i := 0 to _tri - 1 do begin
      TMesh(meshTinTilesT[mtc]).indices[j]   := DWORD(j) ;
      TMesh(meshTinTilesT[mtc]).indices[j+1] := DWORD(j+2) ;
      TMesh(meshTinTilesT[mtc]).indices[j+2] := DWORD(j+1) ;
      j := j + 3 ;
    end;

    TMesh(meshTinTilesT[mtc]).DrawSubset(0) ;
  end;

  procedure TGIS_Renderer3DFMX.drawMeshTinTiles ;
  var
    i,j,k : Integer ;
  begin
    // Draw mesh TIN (color)
    if meshTinTilesCounterC > 0 then begin
      current_tex := pdataTEX_cDX ;
      for i := 0 to meshTinTilesCounterC -1 do begin
        if transpInfo.TCT[i] <> 255 then begin
          blendOn ;
          current_opc := transpInfo.TCT[i] / 255 ;
        end;
        k := Length(TMesh(meshTinTilesC[i]).subsets) ;
        if k = 0 then k := 1;
        for j := 0 to k -1 do begin
          TMesh(meshTinTilesC[i]).DrawSubset(j) ;
        end;
        if transpInfo.TCT[i] <> 255 then begin
          blendOff ;
          current_opc := 1 ;
        end;
      end;
    end;

    // Draw mesh TIN (texture)
    if meshTinTilesCounterT > 0 then begin
      for i := 0 to meshTinTilesCounterT -1 do begin
        if transpInfo.TTT[i] <> 255 then begin
          blendOn ;
          current_opc := transpInfo.TTT[i] / 255 ;
        end;

        current_tex := arTexTinInfo[i] ;
        TMesh(meshTinTilesT[i]).DrawSubset(0) ;

        if transpInfo.TTT[i] <> 255 then begin
          blendOff ;
          current_opc := 1 ;
        end;
      end;
    end;

  end;

  procedure TGIS_Renderer3DFMX.scaleMeshTilesZ(
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
    vb      : P_textureVertex ;
    vc      : P_colorVertex ;
    mesh    : TMesh   ;
  begin
    if meshDemTilesCounter > 0 then
      for i := 0 to meshDemTilesCounter -1 do begin
        numvert := TMesh(meshDemTiles[i]).GetNumVertices ;
        TMesh(meshDemTiles[i]).LockVertexBuffer(0, Pointer(vb)) ;
        for j := 0 to numvert - 1 do begin
          vb^.P.z := vb^.P.z * _value ;
          Inc( vb ) ;
        end;
        TMesh(meshDemTiles[i]).UnlockVertexBuffer ;
      end;

    if meshWallTilesCounter > 0 then
      for i := 0 to meshWallTilesCounter -1 do begin
        numvert := TMesh(meshWallTiles[i]).GetNumVertices ;
        TMesh(meshWallTiles[i]).LockVertexBuffer(0, Pointer(vb)) ;
        for j := 0 to numvert - 1 do begin
          vb^.P.z := vb^.P.z * _value ;
          Inc( vb ) ;
        end;
        TMesh(meshWallTiles[i]).UnlockVertexBuffer ;
      end;

    if meshTinTilesCounterC > 0 then
      for i := 0 to meshTinTilesCounterC -1 do begin
        numvert := TMesh(meshTinTilesC[i]).GetNumVertices ;
        TMesh(meshTinTilesC[i]).LockVertexBuffer(0, Pointer(vc)) ;
        for j := 0 to numvert - 1 do begin
          vc^.P.z := vc^.P.z * _value ;
          Inc( vc ) ;
        end;
        TMesh(meshTinTilesC[i]).UnlockVertexBuffer ;
      end;

    if meshTinTilesCounterT > 0 then
      for i := 0 to meshTinTilesCounterT -1 do begin
        numvert := TMesh(meshTinTilesT[i]).GetNumVertices ;
        TMesh(meshTinTilesT[i]).LockVertexBuffer(0, Pointer(vb)) ;
        for j := 0 to numvert - 1 do begin
          vb^.P.z := vb^.P.z * _value ;
          Inc( vb ) ;
        end;
        TMesh(meshTinTilesT[i]).UnlockVertexBuffer ;
      end;

      if meshMPatchTilesCounterC > 0 then
        for i := 0 to meshMPatchTilesCounterC -1 do begin
          numvert := TMesh(meshMPatchTilesC[i]).GetNumVertices ;
          TMesh(meshMPatchTilesC[i]).LockVertexBuffer(0, Pointer(vc)) ;
          for j := 0 to numvert - 1 do begin
            vc^.P.z := vc^.P.z * _value ;
            Inc( vc ) ;
          end;
          TMesh(meshMPatchTilesC[i]).UnlockVertexBuffer ;
        end;

      if meshMPatchTilesCounterT > 0 then
        for i := 0 to meshMPatchTilesCounterT -1 do begin
          numvert := TMesh(meshMPatchTilesT[i]).GetNumVertices ;
          TMesh(meshMPatchTilesT[i]).LockVertexBuffer(0, Pointer(vb)) ;
          for j := 0 to numvert - 1 do begin
            vb^.P.z := vb^.P.z * _value ;
            Inc( vb ) ;
          end;
          TMesh(meshMPatchTilesT[i]).UnlockVertexBuffer ;
        end;

      if meshVectTilesCounterCW > 0 then
      for i := 0 to meshVectTilesCounterCW -1 do begin
        numvert := TMesh(meshVectTilesCW[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesCW[i] ) ;
        j := 0 ;
        while True do begin
          z0 := mesh.tvertices[j  ].P.z ;
          z1 := mesh.tvertices[j+1].P.z ;
          z4 := mesh.tvertices[j+4].P.z ;
          z5 := mesh.tvertices[j+5].P.z ;
          dlt := z1 - z0 ;
          mesh.tvertices[j  ].P.z := z0 * _value ;
          mesh.tvertices[j+1].P.z := mesh.tvertices[j  ].P.z + dlt ;
          mesh.tvertices[j+3].P.z := mesh.tvertices[j+1].P.z ;
          dlt := z5 - z4 ;
          mesh.tvertices[j+4].P.z := z4 * _value ;
          mesh.tvertices[j+5].P.z := mesh.tvertices[j+4].P.z + dlt ;
          mesh.tvertices[j+2].P.z := mesh.tvertices[j+4].P.z ;
          j := j + 6 ;
          if j >= numvert then break ;
        end;
      end;

      if meshVectTilesCounterCR > 0 then
      for i := 0 to meshVectTilesCounterCR -1 do begin
        numvert := TMesh(meshVectTilesCR[i]).GetNumVertices ;
        TMesh(meshVectTilesCR[i]).LockVertexBuffer(0, Pointer(vc)) ;
        for j := 0 to numvert - 1 do begin
          vc^.P.z := (vc^.P.z - arVectPolyRoofC[i,j]) * _value +
                      arVectPolyRoofC[i,j] ;
          Inc( vc ) ;
        end;
        TMesh(meshVectTilesCR[i]).UnlockVertexBuffer ;
      end;

      if meshVectTilesCounterTW > 0 then
      for i := 0 to meshVectTilesCounterTW -1 do begin
        numvert := TMesh(meshVectTilesTW[i]).GetNumVertices ;
        TMesh(meshVectTilesTW[i]).LockVertexBuffer(0, Pointer(vb)) ;
        System.Move( vb^, vTLBufTW.TriLst[0],
                     numvert * SizeOf(TGIS_Renderer3DVertex)) ;
        j := 0 ;
        while True do begin
          z0 := vTLBufTW.TriLst[j  ].P.z ;
          z1 := vTLBufTW.TriLst[j+1].P.z ;
          z4 := vTLBufTW.TriLst[j+4].P.z ;
          z5 := vTLBufTW.TriLst[j+5].P.z ;
          dlt := z1 - z0 ;
          vTLBufTW.TriLst[j  ].P.z := z0 * _value ;
          vTLBufTW.TriLst[j+1].P.z := vTLBufTW.TriLst[j  ].P.z + dlt ;
          vTLBufTW.TriLst[j+3].P.z := vTLBufTW.TriLst[j+1].P.z ;
          dlt := z5 - z4 ;
          vTLBufTW.TriLst[j+4].P.z := z4 * _value ;
          vTLBufTW.TriLst[j+5].P.z := vTLBufTW.TriLst[j+4].P.z + dlt ;
          vTLBufTW.TriLst[j+2].P.z := vTLBufTW.TriLst[j+4].P.z ;
          j := j + 6 ;
          if j >= numvert then break ;
        end;

        for j := 0 to numvert - 1 do begin
          vb^.P.z := vTLBufTW.TriLst[j].P.z ;
          Inc( vb ) ;
        end;
        TMesh(meshVectTilesTW[i]).UnlockVertexBuffer ;
      end;

      if meshVectTilesCounterTR > 0 then
      for i := 0 to meshVectTilesCounterTR -1 do begin
        numvert := TMesh(meshVectTilesTR[i]).GetNumVertices ;
        TMesh(meshVectTilesTR[i]).LockVertexBuffer(0, Pointer(vb)) ;
        for j := 0 to numvert - 1 do begin
          vb^.P.z := (vb^.P.z - arVectPolyRoofT[i][j]) * _value
                        + arVectPolyRoofT[i][j] ;
          Inc( vb ) ;
        end;
        TMesh(meshVectTilesTR[i]).UnlockVertexBuffer ;
      end;

      if meshLineTilesCounter > 0 then
      for i := 0 to meshLineTilesCounter -1 do begin
        numvert := meshLineTiles[i].Count ;
        k  := 0 ;
        k0 := 0 ;
        while True do begin
          offset := 0 ;
          while True do begin
            if (meshLineTiles[i].Buffer[k].P.x=meshLineTiles[i].Buffer[k+1].P.x)
               and
               (meshLineTiles[i].Buffer[k].P.y=meshLineTiles[i].Buffer[k+1].P.y)
               then Inc( offset, 2 )
            else break ;
            Inc( k, 2 ) ;
            if k >= numvert then break ;
          end;

          if offset = 0 then begin  // offset = 0, horizontal 1 pix lines
            for j := 0 to numvert -1 do begin
              meshLineTiles[i].Buffer[j].P.z :=
                meshLineTiles[i].Buffer[j].P.z * _value ;
            end;
              l := numvert ;
          end
          else
          if offset = numvert then begin // vertical 1 pix lines only
            j := 0 ;
            while True do begin
               dlt := ( meshLineTiles[i].Buffer[j+1].P.z -
                        meshLineTiles[i].Buffer[j].P.z ) ;
               meshLineTiles[i].Buffer[j+1].P.z :=
                ( meshLineTiles[i].Buffer[j+1].P.z - dlt ) * _value + dlt ;
               meshLineTiles[i].Buffer[j].P.z :=
                 meshLineTiles[i].Buffer[j].P.z * _value ;
               Inc( j, 2 ) ;
               if j >= offset then break ;
            end;
            l := j ;
          end
          else begin  // polygon edges
            j := k0 ;
            first := True ;
            while True do begin
              dlt := ( meshLineTiles[i].Buffer[j+1].P.z -
                       meshLineTiles[i].Buffer[j].P.z ) ;
              // vertical
              meshLineTiles[i].Buffer[j+1].P.z :=
                ( meshLineTiles[i].Buffer[j+1].P.z - dlt ) * _value + dlt ;
              meshLineTiles[i].Buffer[j].P.z :=
                meshLineTiles[i].Buffer[j].P.z * _value ;
              // horizontal
              if first then begin
                meshLineTiles[i].Buffer[j+offset].P.z :=
                  meshLineTiles[i].Buffer[j+1].P.z ;
                meshLineTiles[i].Buffer[j+offset+offset -1].P.z :=
                  meshLineTiles[i].Buffer[j+1].P.z ;
                first := False ;
              end
              else begin
                meshLineTiles[i].Buffer[j-1+offset].P.z :=
                  meshLineTiles[i].Buffer[j+1].P.z ;
                meshLineTiles[i].Buffer[j+offset].P.z :=
                  meshLineTiles[i].Buffer[j+1].P.z ;
              end;

              Inc( j, 2 ) ;
              if j >= k0 + offset then break ;
            end;
            l := k0 + 2 * offset ;
          end;

          if l >= numvert then break ;
          k := l ;
          k0 := k ;
        end;
      end;

      if High(labelInfo)>= 0 then   // labels
      for i := 0 to High(labelInfo) do  begin
        dlt := labelInfo[i].LabelZ - labelInfo[i].LabelB ;
        labelInfo[i].LabelZ := ( labelInfo[i].LabelZ - dlt ) * _value + dlt ;
        labelInfo[i].LabelB := labelInfo[i].LabelB * _value ;
      end;

  end;

  procedure TGIS_Renderer3DFMX.scaleMeshTilesM(
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
    vb      : P_textureVertex ;
    vc      : P_colorVertex ;
    mesh    : TMesh   ;
  begin
    if meshVectTilesCounterCW > 0 then  // color wall triangle
      for i := 0 to meshVectTilesCounterCW -1 do begin
        numvert := TMesh(meshVectTilesCW[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesCW[i]) ;
        j := 0 ;
        while True do begin
          z0 := mesh.tvertices[j  ].P.z ;
          z1 := mesh.tvertices[j+1].P.z ;
          z4 := mesh.tvertices[j+4].P.z ;
          z5 := mesh.tvertices[j+5].P.z ;
          dlt := ( z1 - z0 ) * _value ;
          mesh.tvertices[j  ].P.z := z0 ;
          mesh.tvertices[j+1].P.z := mesh.tvertices[j  ].P.z + dlt ;
          mesh.tvertices[j+3].P.z := mesh.tvertices[j+1].P.z ;
          dlt := ( z5 - z4 ) * _value ;
          mesh.tvertices[j+4].P.z := z4 ;
          mesh.tvertices[j+5].P.z := mesh.tvertices[j+4].P.z + dlt ;
          mesh.tvertices[j+2].P.z := mesh.tvertices[j+4].P.z ;
          j := j + 6 ;
          if j >= numvert then break ;
        end;
    end;

    if meshVectTilesCounterCR > 0 then    // color roof triangle
      for i := 0 to meshVectTilesCounterCR -1 do begin
        numvert := TMesh(meshVectTilesCR[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesCR[i]) ;
        for j := 0 to numvert - 1 do begin
          mesh.tvertices[j].P.Z := ( mesh.tvertices[j].P.Z - arVectPolyRoofC[i][j])
                        + arVectPolyRoofC[i][j] * _value ;
          Inc( vc ) ;
          arVectPolyRoofC[i][j] := arVectPolyRoofC[i][j] * _value ;
        end;
      end;

    if meshVectTilesCounterTW > 0 then  // textured wall triangle
      for i := 0 to meshVectTilesCounterTW -1 do begin
        numvert := TMesh(meshVectTilesTW[i]).GetNumVertices ;

        mesh := TMesh(meshVectTilesTW[i]) ;
        j := 0 ;
        while True do begin
          z0 := mesh.tvertices[j  ].P.z ;
          z1 := mesh.tvertices[j+1].P.z ;
          z4 := mesh.tvertices[j+4].P.z ;
          z5 := mesh.tvertices[j+5].P.z ;
          dlt := ( z1 - z0 ) * _value ;
          mesh.tvertices[j  ].P.z := z0 ;
          mesh.tvertices[j+1].P.z := mesh.tvertices[j  ].P.z + dlt ;
          mesh.tvertices[j+3].P.z := mesh.tvertices[j+1].P.z ;
          dlt := ( z5 - z4 ) * _value ;
          mesh.tvertices[j+4].P.z := z4 ;
          mesh.tvertices[j+5].P.z := mesh.tvertices[j+4].P.z + dlt ;
          mesh.tvertices[j+2].P.z := mesh.tvertices[j+4].P.z ;
          j := j + 6 ;
          if j >= numvert then break ;
        end;
      end;

    if meshVectTilesCounterTR > 0 then    // textured roof triangle
      for i := 0 to meshVectTilesCounterTR -1 do begin
        numvert := TMesh(meshVectTilesTR[i]).GetNumVertices ;
        mesh := TMesh(meshVectTilesTR[i]) ;
        for j := 0 to numvert - 1 do begin
          mesh.tvertices[j].P.z := (mesh.tvertices[j].P.z - arVectPolyRoofT[i][j])
                        + arVectPolyRoofT[i][j] * _value ;
          Inc( vb ) ;
          arVectPolyRoofT[i][j] := arVectPolyRoofT[i][j] * _value ;
        end;
      end;

    if meshLineTilesCounter > 0 then  // color edges
      for i := 0 to meshLineTilesCounter -1 do begin
        numvert := meshLineTiles[i].Count ;
        k  := 0 ;
        k0 := 0 ;
        while True do begin
          offset := 0 ;
          while True do begin
            if (meshLineTiles[i].Buffer[k].P.x=meshLineTiles[i].Buffer[k+1].P.x)
               and
               (meshLineTiles[i].Buffer[k].P.y=meshLineTiles[i].Buffer[k+1].P.y)
               then Inc( offset, 2 )
            else break ;
            Inc( k, 2 ) ;
            if k >= numvert then break ;
          end;

          if offset = 0 then begin  // offset = 0, horizontal 1 pix lines
              l := numvert ;
          end
          else
          if offset = numvert then begin // vertical 1 pix lines only
            j := 0 ;
            while True do begin
               dlt := (meshLineTiles[i].Buffer[j+1].P.z -
                       meshLineTiles[i].Buffer[j].P.z) * _value ;
               meshLineTiles[i].Buffer[j+1].P.z :=
                 meshLineTiles[i].Buffer[j].P.z + dlt ;
               Inc( j, 2 ) ;
               if j >= offset then break ;
            end;
            l := j ;
          end
          else  begin  // polygon edges
            j := k0 ;
            first := True ;
            while True do begin
              dlt := (meshLineTiles[i].Buffer[j+1].P.z -
                      meshLineTiles[i].Buffer[j].P.z) * _value ;
              // vertical
              meshLineTiles[i].Buffer[j+1].P.z :=
                meshLineTiles[i].Buffer[j].P.z + dlt ;
              // horizontal
              if first then begin
                meshLineTiles[i].Buffer[j+offset].P.z :=
                  meshLineTiles[i].Buffer[j+1].P.z ;
                meshLineTiles[i].Buffer[j+offset+offset -1].P.z :=
                  meshLineTiles[i].Buffer[j+1].P.z ;
                first := False ;
              end
              else begin
                meshLineTiles[i].Buffer[j-1+offset].P.z :=
                  meshLineTiles[i].Buffer[j+1].P.z ;
                meshLineTiles[i].Buffer[j+offset].P.z :=
                  meshLineTiles[i].Buffer[j+1].P.z ;
              end;

              Inc( j, 2 ) ;
              if j >= k0 + offset then break ;
            end;
            l := k0 + 2 * offset ;
          end;

          if l >= numvert then break ;
          k := l ;
          k0 := k ;
        end;
      end;

      if High(labelInfo)>= 0 then   // labels
      for i := 0 to High(labelInfo) do  begin
        dlt := ( labelInfo[i].LabelZ - labelInfo[i].LabelB ) * _value ;
        labelInfo[i].LabelZ := labelInfo[i].LabelB + dlt ;
      end;

  end;

  procedure TGIS_Renderer3DFMX.prepareLabelTexture(
    const _org    : TPoint     ;
    const _shp    : TGIS_Shape ;
    const _shp_lt : TObject
  ) ;
  var
    prm     : TGIS_ParamsLabel ;
    txt     : String           ;
    bmp     : TGIS_Bitmap      ;
    shp_lt  : TGIS_Renderer3DShape  ;
    ctx     : TGIS_RendererContext  ;
    ornd    : TGIS_RendererAbstract ;
    lbl     : TObject ;
    w       : Integer ;
    pnt     : TPoint  ;
  begin
    Assert( Assigned( _shp    ) ) ;
    Assert( Assigned( _shp_lt ) ) ;

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

    if Assigned( lbl ) then begin
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

      if not Assigned( bmp ) then
        exit ;

      shp_lt := TGIS_Renderer3DShape( _shp_lt ) ;
      shp_lt.BitmapSize := Point( bmp.Width, bmp.Height ) ;
      shp_lt.LabelSize := pnt ;
      shp_lt.LabelBitmap := bmp ;
    end;

  end;

  function TGIS_Renderer3DFMX.mapToScreen(
    const _ptg : TGIS_Point
  ) : TPoint ;
  var
    vec1, vec2          : TVector3f;
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

    Vec3Project( vec2, vec1, oContext, mtxMVP, wndWdth, wndHght ) ;
    pt.X := RoundS(vec2.X) ;
    pt.Y := RoundS(vec2.Y) ;

    Result := pt ;
  end ;

  function TGIS_Renderer3DFMX.mapToScreen2(
    const _ptg : TGIS_Point;
    const _z   : Double
  ) : TPoint ;
  var
    vec1, vec2         : TVector3f ;
    m_flXMin, m_flYMin,
    m_flXMax, m_flYMax : Double    ;
    pt                 : TPoint    ;
    robx, roby         : Double    ;
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

    Vec3Project( vec2, vec1, oContext, mtxMVP, wndWdth, wndHght ) ;
    pt.X := RoundS(vec2.X) ;
    pt.Y := RoundS(vec2.Y) ;

    Result := pt ;
  end;

  function TGIS_Renderer3DFMX.screenTo3D(
    const _pt : TPoint
  ) : TGIS_Point ;
  var
    vec1, vec2, vRayDirection, vRayPos, vRayPos2, vOut: TVector3f;
    vs1, vs2 : TVector3f;
    fZplane: Single;
    plane: TGIS_Point3D;
    pnt : TGIS_Point;
  begin
    fZplane := 0.0;
    // set the mouse location
    vec1.x := _pt.X;     vec2.x := vec1.x;
    vec1.y := _pt.Y;     vec2.y := vec1.y;
    vec1.z := 0.0;       vec2.z := 1.0;
    Vec3Unproject(vRayPos,vec1,oContext,mtxProjection,mtxView,mtxWorld,wndWdth,wndHght) ;
    Vec3Unproject(vRayDirection,vec2,oContext,mtxProjection,mtxView,mtxWorld,wndWdth,wndHght) ;
    vRayDirection.x := vRayDirection.x - vRayPos.x;
    vRayDirection.y := vRayDirection.y - vRayPos.y;
    vRayDirection.z := vRayDirection.z - vRayPos.z;
    vRayPos2.x := vRayPos.x + vRayDirection.x;
    vRayPos2.y := vRayPos.y + vRayDirection.y;
    vRayPos2.z := vRayPos.z + vRayDirection.z;
    vs1.x := 0.0; vs1.y := 0.0; vs1.z := fZplane;
    vs2.x := 0.0; vs2.y := 0.0; vs2.z := 1.0;
    PlaneFromPointNormal(plane,vs1,vs2);
    PlaneIntersectLine(vOut,plane,vRayPos,vRayPos2);
    pnt.X := vOut.x;
    pnt.Y := vOut.y;
    Result := pnt;
  end;

  procedure TGIS_Renderer3DFMX.prepareVertexBuffersToDraw;
  var
    rows         : Integer     ;
    hr           : Integer     ;
    render_start : Int64       ;
    oldrough     : Double      ;
  begin
    if oContext = nil then begin
      exit ;
    end ;

    errorMsg := '' ;

    if generalLock > 0 then exit ;

    iFps := 0 ;

    if (getWindowSize.X <> windowWidth) or
       (getWindowSize.Y <> windowHeight) then begin
      windowWidth  := getWindowSize.X ;
      windowHeight := getWindowSize.Y ;
    end;

    if enDemCachedSize = TGIS_Viewer3DDemCacheSize.Window then begin
        if (noImg <> True) or (noDem <> True) then begin
          DemCachedSize := enDemCachedSize  ;
//?          demFrameSize  := vertexBufferSize ;
        end ;
      end ;

    wndXSize := windowWidth * dRadius * Tan(DegToRad(cameraHalfViewAngle))
        / windowHeight ;

    if firstScene then
      reRenderCall := True ;

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

    transformSetting ;

    oContext.SetContextState( TContextState.csAllFace ) ;
    oContext.SetContextState( TContextState.cs3DScene ) ;

    timeStart := GetTickCount ;
    render_start := timeStart ;

    setD3D_Data ;
    setInitialLevel ;
    updateLayerExistance ;

    setLod ;

    rows := TruncS( ( 1.0 * demFrameSize ) / lodStep) ;
    if rows * lodStep >= demFrameSize then
      Dec(rows) ;

    // vector drawing when DEM is transparent
    if ((transpPrior = TGIS_Viewer3DTransparencyPriority.Auto) and
          (demTransparency <> 255)) or
          (transpPrior = TGIS_Viewer3DTransparencyPriority.Dem) then begin

      draw3DVector ;
    end ;

    // turn on the color blending if necessary
    if demTransparency <> 255 then begin
      blendOn ;
      current_opc := demTransparency / 255 ;
    end;

    lockReadingData := 1 ;

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

          detWnd.Left   := 0 ;
          detWnd.Right  := demFrameSize - lodStep ;
          detWnd.Top    := -1 ;
          detWnd.Bottom := rows ;

          xMin :=  detailExtent.XMin ;
          YMin :=  detailExtent.YMin ;
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

    // turn off the color blending
    if demTransparency <> 255 then begin
      blendOff ;
      current_opc := 1.0 ;
    end;

    // vector drawing when DEM not transparent
    if ((transpPrior = TGIS_Viewer3DTransparencyPriority.Auto) and
          (demTransparency = 255)) or
          (transpPrior = TGIS_Viewer3DTransparencyPriority.Vector) then begin

      draw3DVector ;
    end ;

    timeEnd := getTimeTic;
    if timeEnd - render_start = 0 then
      iFps := 1000
    else
      iFps := TruncS(1000.0 / (timeEnd - render_start)) ;

    firstScene   := False ;
    reRenderCall := False ;
  end;

  procedure TGIS_Renderer3DFMX.doDestroy ;
  begin
    light_mtrl.Free ;
    texture_mtrl.Free ;
    color_mtrl.Free ;
    FreeObject( pdataVB ) ;
    FreeObject( cdataVB ) ;
    FreeObject( idataIB ) ;
    if Assigned( pdataTEX_r ) then
      FreeObject( pdataTEX_r ) ;
    if Assigned( pdataTEX_d ) then
      FreeObject( pdataTEX_d ) ;
    if Assigned( pdataTEX_w ) then
      FreeObject( pdataTEX_w ) ;
    if Assigned( pdataTEX_bDX ) then
      FreeObject( pdataTEX_bDX ) ;
    if Assigned( pdataTEX_sDX ) then
      FreeObject( pdataTEX_sDX ) ;
    if Assigned( pdataTEX_sun ) then
      FreeObject( pdataTEX_sun ) ;
    if Assigned( pdataTEX_cDX ) then
      FreeObject( pdataTEX_cDX ) ;
    if Assigned( oColorHelper ) then
      FreeObject( oColorHelper ) ;

    inherited;
  end;

  constructor TMesh.Create(
    const _faces    : DWORD ;
    const _vertices : DWORD ;
    const _format   : TVertexFormats ;
    const _pvb      : TVertexBuffer ;
    const _cvb      : TVertexBuffer ;
    const _iib      : TIndexBuffer ;
    const _rnd      : TGIS_Renderer3DFMX
  ) ;
  begin
    rnd       := _rnd ;
    pdataVBm  := _pvb ;
    cdataVBm  := _cvb ;
    idataIBm  := _iib ;
    format    := _format ;
    SetLength( faces, _faces ) ;
    SetLength( indices, 3 * _faces ) ;
    SetLength( subsets, 0 ) ;
    FAppend  := False ;
    FSaved   := False ;
    FNumVert := 0 ;
    FNumFace := 0 ;

    if format = Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE then
      SetLength( tvertices, _vertices )
    else
      SetLength( cvertices, _vertices ) ;
  end;

  procedure TMesh.doDestroy;
  begin
    SetLength( faces, 0 ) ;
    SetLength( indices, 0 ) ;
    SetLength( tvertices, 0) ;
    SetLength( cvertices, 0) ;
    SetLength( subsets, 0 ) ;

    inherited ;
  end;

  function TMesh.pdataVB_Lock(
    var _strm : PByte
  ) : Integer ;
  begin
    Result :=  0 ;
  end;

  function TMesh.cdataVB_Lock(
    var _strm : PByte
  ) : Integer ;
  begin
    Result :=  0 ;
  end;

  function TMesh.pdataVB_Unlock(
    var _strm : PByte        ;
    var _data : T_arTextureData
  ) : Integer ;
  begin
    Result :=  0 ;
  end;

  function TMesh.cdataVB_Unlock(
    var _strm : PByte        ;
    var _data : T_arColorData
  ) : Integer ;
  begin
    Result :=  0 ;
  end;

  function  TMesh.GetNumVertices : DWORD ;
  begin
    if FAppend = False then begin
      if format = Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE then
        Result := Length( tvertices )
      else
        Result := Length( cvertices ) ;
    end
    else
      Result := FNumVert ;
  end;

  function  TMesh.GetNumFaces : DWORD ;
  begin
    if FAppend = False then
      Result := Length( faces )
    else
      Result := FNumFace ;
  end;

  procedure TMesh.SortIndices(
    const _num      : Integer
  ) ;
  var
    i,j,ii,jj,k,l,ll : DWORD ;

    procedure swap_faces(const _i : Integer; const _j : Integer ) ;
    var
      a : DWORD ;
    begin
      a := faces[_i] ;
      faces[_i] := faces[_j] ;
      faces[_j] := a ;
    end;

    procedure swap_indices(const _i : Integer; const _j : Integer ) ;
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
    SetLength( subsets, 0 ) ;
    if _num = 1 then exit
    else
      SetLength( subsets, _num ) ;
    k := GetNumFaces ;
    for i := 0 to k -1 do
      Inc( subsets[faces[i]] );

    if _num > 2 then begin
      sortIndicesEx ;
      exit;
    end;

    if (subsets[0] = 0) or (subsets[1] = 0) then exit;
        i  := 0 ; // subset searched
    l  := 0 ; // number of subset i found
    ll := 0 ; // number of subset i=1 found
    ii := subsets[i] ;

    for j := 0 to k -1 do begin //subsets[0] -1 do begin
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

  procedure TMesh.DrawSubset(
    const _attribId   : DWORD
  ) ;
  var
    tpardata : T_arTextureData ;
    cpardata : T_arColorData  ;
    cdata    : PByte          ;
    pdata    : PByte          ;
    idata    : PByte          ;
    k, l     : Integer        ;
    mtrl     : TMaterial      ;
    attridx  : DWORD          ;

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
    FSaved := True ;
    k := GetNumVertices ;
    l := GetNumFaces ;
    attridx := 0 ;

    if format = Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE then
      begin
        pdataVBm.Length := k ;
        if rnd.current_lght then begin
          rnd.light_mtrl.Ambient  := rnd.ambColor ;
          if not Assigned( rnd.current_tex ) then begin
            if Length( subsets ) = 0 then
              rnd.light_mtrl.Diffuse  := tvertices[0].Color
            else  begin
              attridx := findex(_attribId) ;
              rnd.light_mtrl.Diffuse := tvertices[indices[attridx]].Color ;
            end;
          end
          else begin
            if Length( subsets ) = 0 then
              rnd.light_mtrl.Diffuse  := TAlphaColorRec.White
            else begin
              attridx := findex(_attribId) ;
              rnd.light_mtrl.Diffuse := tvertices[indices[attridx]].Color ;
            end;
          end;

          rnd.light_mtrl.Emissive := TAlphaColorRec.Black ;
          rnd.light_mtrl.Specular := TAlphaColorRec.Black ;
          rnd.light_mtrl.Texture  := rnd.current_tex ;
          mtrl := rnd.light_mtrl
        end
        else begin
          if Length( subsets ) = 0 then begin
            rnd.texture_mtrl.Texture := rnd.current_tex ;
            rnd.color_mtrl.Color := tvertices[0].Color ;
            mtrl := rnd.texture_mtrl ;
          end
          else begin
            attridx := findex(_attribId) ;
            rnd.texture_mtrl.Texture := rnd.current_tex ;
            rnd.color_mtrl.Color := tvertices[indices[attridx]].Color ;
            mtrl := rnd.texture_mtrl ;
            rnd.current_opc := (rnd.color_mtrl.Color shr 24 ) / 255 ;
          end;
        end;

        if Length( subsets ) = 0 then begin
          idataIBm.Length := l * 3 ;
          rnd.current_cntx.DrawPrimitives(TPrimitivesKind.Triangles, @tvertices[0], @indices[0],
            pdataVBm.GetVertexDeclarations, pdataVBm.VertexSize, pdataVBm.Length,
            sizeOf(indices[0]), Length( indices ),
            mtrl, rnd.current_opc )
        end
        else begin
          rnd.current_cntx.DrawPrimitives(TPrimitivesKind.Triangles, @tvertices[0], @indices[attridx],
            pdataVBm.GetVertexDeclarations, pdataVBm.VertexSize, pdataVBm.Length,
            sizeOf(indices[0]), subsets[_attribId] * 3 ,
            mtrl, rnd.current_opc )
        end;

      end
      else begin
        cdataVBm.Length := k ;
        if rnd.current_lght then begin
          rnd.light_mtrl.Ambient  := rnd.ambColor ;
          if Length( subsets ) = 0 then
            rnd.light_mtrl.Diffuse  := cvertices[0].Color
          else  begin
            attridx := findex(_attribId) ;
            rnd.light_mtrl.Diffuse := cvertices[indices[attridx]].Color ;
          end;
          rnd.light_mtrl.Emissive := TAlphaColorRec.Black ;
          rnd.light_mtrl.Specular := TAlphaColorRec.Black ;
          rnd.light_mtrl.Texture  := nil ;
          mtrl := rnd.light_mtrl
        end
        else begin
          if Length( subsets ) = 0 then
            rnd.color_mtrl.Color := cvertices[0].Color
          else  begin
            attridx := findex(_attribId) ;
            rnd.color_mtrl.Color := cvertices[indices[attridx]].Color ;
          end;

          mtrl := rnd.color_mtrl ;
          rnd.current_opc := (rnd.color_mtrl.Color shr 24 ) / 255 ;
        end;

        if Length( subsets ) = 0 then begin
          idataIBm.Length := l * 3 ;
          rnd.current_cntx.DrawPrimitives(TPrimitivesKind.Triangles, @cvertices[0], @indices[0],
            cdataVBm.GetVertexDeclarations, cdataVBm.VertexSize, cdataVBm.Length,
            sizeOf(indices[0]), Length( indices ),
            mtrl, rnd.current_opc ) ;
        end
        else begin
          idataIBm.Length := subsets[_attribId] * 3 ;
          rnd.current_cntx.DrawPrimitives(TPrimitivesKind.Triangles, @cvertices[0], @indices[attridx],
            cdataVBm.GetVertexDeclarations, cdataVBm.VertexSize, cdataVBm.Length,
            sizeOf(indices[0]), subsets[_attribId] * 3 ,
            mtrl, rnd.current_opc )
        end;
      end;

  end;

  function  TMesh.LockVertexBuffer(
    const Flags      : DWORD;
    out ppData : Pointer
  ) : Integer ;
  begin
    if format = Lider.CG.GIS.GeoViewer3D_D3DFVF_GRAPH3DTEXTURE then
      ppData := @tvertices[Flags]
    else
      ppData := @cvertices[Flags] ;

    if Assigned(ppData) then
      Result := S_OK
    else
      Result := S_False ;
  end;

  function  TMesh.UnlockVertexBuffer : Integer ;
  begin
    Result := S_OK ;
  end;

  function  TMesh.LockIndexBuffer(
    const Flags      : DWORD;
    out ppData : Pointer
  ) : Integer ;
  begin
    ppData := @indices[Flags] ;
    if Assigned(ppData) then
      Result := S_OK
    else
      Result := S_False ;
  end;

  function  TMesh.UnlockIndexBuffer : Integer ;
  begin
    Result := S_OK ;
  end;

  function  TMesh.LockAttributeBuffer(
    const Flags      : DWORD;
    out ppData : PDWORD
  ) : Integer ;
  begin
    ppData := @faces[Flags] ;
    if Assigned(ppData) then
      Result := S_OK
    else
      Result := S_False ;
  end;

  function  TMesh.UnlockAttributeBuffer : Integer ;
  begin
    Result := S_OK ;
  end;


//==============================================================================
// initialization / finalization
//==============================================================================

initialization
  oLabelsReg := TGIS_LabelsArea.Create( nil )  ;

finalization
  FreeObject( oLabelsReg ) ;

//==================================== END =====================================
end.

