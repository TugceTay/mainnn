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
  Encapsulation of the TGIS_Renderer3DAbstract class.
}

{$IFDEF DCC}
  unit GisRenderer3DAbstract ;
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
    System.Runtime.InteropServices,
    System.Drawing,
    System.Timers,
    TatukGIS.RTL,
    TatukGIS.RTL.SharpDX ;
{$ENDIF}
{$IFDEF DCC}
  uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  GisRtl,
  GisInterfaces,
  GisFunctions,
  GisTypes,
  GisTypesUI,
  GisClasses,
  GisParams,
  GisResource,
  GisLayer,
  GisLayerProject,
  GisLayerVector,
  GisLayerPixel,
  GisLayerCompound,
  GisTopology,
  GisCsSystems,
  GisTypes3D ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.util,
    tatukgis.rtl ;
{$ENDIF}

const

  /// <summary>
  ///   Helper constant to calculate level of detail.
  /// </summary>
  GISVIEWER3D_LOD_FACTOR = 256 ;

  /// <summary>
  ///   Waiting time until Update rendering.
  /// </summary>
  GISVIEWER3D_DELAY_TIME = 1500  ;

  /// <summary>
  ///   Normalization Z value for DEM/Vector layers (50 means 25% of screen height).
  /// </summary>
  GISVIEWER3D_NORM_VALUE = 20.0 ;

  /// <summary>
  ///   Minimal value used to Z,M range checking.
  /// </summary>
  ZMMIN = -1.0e30 ;

  /// <summary>
  ///   Maximal value used to Z,M range checking.
  /// </summary>
  ZMMAX =  1.0e30 ;

type

  /// <summary>
  ///   24 bits color pixel components.
  /// </summary>
  TRGB_Pixel = {$IFDEF OXYGENE} assembly {$ENDIF} packed record
    /// <summary>
    ///   Byte representing Blue component.
    /// </summary>
    B : Byte ;

    /// <summary>
    ///   Byte representing Green component.
    /// </summary>
    G : Byte ;

    /// <summary>
    ///   Byte representing Red component.
    /// </summary>
    R : Byte ;
  end ;

  {$IFNDEF CLR}
    /// <summary>
    ///   Pointer to 24 bits color pixel record.
    /// </summary>
    PRGB_Pixel = ^TRGB_Pixel ;
  {$ENDIF}

  /// <summary>
  ///   32 bits color pixel components.
  /// </summary>
  TRGBA_Pixel = {$IFDEF OXYGENE} assembly {$ENDIF} packed record
    /// <summary>
    ///   Byte representing Blue component.
    /// </summary>
    B : Byte ;

    /// <summary>
    ///   Byte representing Green component.
    /// </summary>
    G : Byte ;

    /// <summary>
    ///   Byte representing Red component.
    /// </summary>
    R : Byte ;

    /// <summary>
    ///   Byte representing Alpha component.
    /// </summary>
    A : Byte ;
  end ;

  {$IFNDEF CLR}
    /// <summary>
    ///   Pointer to 32 bits color pixel record.
    /// </summary>
    PRGBA_Pixel = ^TRGBA_Pixel ;
  {$ENDIF}

  {$IFNDEF CLR}
    /// <summary>
    ///   Single precision 3D vector.
    /// </summary>
    TVector3f = record
      /// <summary>
      ///   X coordinate.
      /// </summary>
      X : Single ;

      /// <summary>
      ///   Y coordinate.
      /// </summary>
      Y : Single ;

      /// <summary>
      ///   Z coordinate.
      /// </summary>
      Z : Single ;
    end ;
  {$ENDIF}

  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential, Pack=2)]
  {$ENDIF}
  /// <summary>
  ///   Vertex structure.
  /// </summary>
  TGIS_Renderer3DVertex = {$IFDEF OXYGENE} public {$ENDIF} record

      /// <summary>
      ///   point coordinates X,Y,Z.
      /// </summary>
      P     : TVector3f ;

      /// <summary>
      ///   normal in point X,Y,Z.
      /// </summary>
      N     : TVector3f ;

      /// <summary>
      ///   color.
      /// </summary>
      Color : DWORD        ;

      /// <summary>
      ///   texture in X direction.
      /// </summary>
      Tu    : Single    ;

      /// <summary>
      ///   texture in Y direction.
      /// </summary>
      Tv    : Single    ;
  end;

  {$IFDEF CLR}
    P_colorVertex =  {$IFDEF OXYGENE} public {$ENDIF} TGIS_Renderer3DVertex ;
  {$ELSE}
    /// <summary>
    ///   Pointer to Color vertex record.
    /// </summary>
    P_colorVertex = ^TGIS_Renderer3DVertex ;
  {$ENDIF}

  /// <summary>
  ///   Color vertex array.
  /// </summary>
  T_arColorData = {$IFDEF OXYGENE} public {$ENDIF} array of TGIS_Renderer3DVertex ;

  {$IFDEF CLR}
    P_textureVertex = {$IFDEF OXYGENE} public {$ENDIF} TGIS_Renderer3DVertex ;
  {$ELSE}
    /// <summary>
    ///   Pointer to Texture vertex record.
    /// </summary>
    P_textureVertex = ^TGIS_Renderer3DVertex ;
  {$ENDIF}

  /// <summary>
  ///   Texture vertex array.
  /// </summary>
  T_arTextureData = {$IFDEF OXYGENE} public {$ENDIF} array of TGIS_Renderer3DVertex ;

  /// <summary>
  ///   Color vertex buffer.
  /// </summary>
  TGIS_Renderer3DColorTriangleListBuffer = {$IFDEF OXYGENE} public {$ENDIF} record

      /// <summary>
      ///   Number of points in buffer.
      /// </summary>
      NumPoints  : Integer                ;

      /// <summary>
      ///   Number of objects in buffer.
      /// </summary>
      NumObjects : Integer                ;

      /// <summary>
      ///   Buffer array.
      /// </summary>
      TriLst     : array of TGIS_Renderer3DVertex ;
  end;

  /// <summary>
  ///   Texture vertex buffer for DEM.
  /// </summary>
  TGIS_Renderer3DDemTriangleListBuffer ={$IFDEF OXYGENE} public {$ENDIF}  record

      /// <summary>
      ///   Number of points in buffer.
      /// </summary>
      NumPoints  : Integer                  ;

      /// <summary>
      ///   Number of objects in buffer.
      /// </summary>
      NumObjects : Integer                  ;

      /// <summary>
      ///   Buffer array.
      /// </summary>
      TriLst     : array of TGIS_Renderer3DVertex ;
  end;

  /// <summary>
  ///   Texture vertex buffer for Vector.
  /// </summary>
  TGIS_Renderer3DTextureTriangleListBuffer = {$IFDEF OXYGENE} public {$ENDIF} record

      /// <summary>
      ///   Number of points in buffer.
      /// </summary>
      NumPoints  : Integer                  ;

      /// <summary>
      ///   Number of objects in buffer.
      /// </summary>
      NumObjects : Integer                  ;

      /// <summary>
      ///   Buffer array.
      /// </summary>
      TriLst     : array of TGIS_Renderer3DVertex ;

      /// <summary>
      ///   Subset number.
      /// </summary>
      Subset     : array of Integer         ;
  end;

  /// <summary>
  ///   Color vertex buffer for points.
  /// </summary>
  TGIS_Renderer3DPointBuffer = {$IFDEF OXYGENE} public {$ENDIF} record

      /// <summary>
      ///   Number of points in buffer.
      /// </summary>
      NumObjects  : Integer                ;

      /// <summary>
      ///   Buffer array
      /// </summary>
      PntLst     : array of TGIS_Renderer3DVertex ;
  end;

  /// <summary>
  ///   Color vertex buffer for lines.
  /// </summary>
  TGIS_Renderer3DPointListBuffer = {$IFDEF OXYGENE} public {$ENDIF} record

      /// <summary>
      ///   Number of points in buffer.
      /// </summary>
      NumPoints  : Integer                ;

      /// <summary>
      ///   Number of objects in buffer.
      /// </summary>
      NumObjects : Integer                ;

      /// <summary>
      ///   Buffer array.
      /// </summary>
      PntLst     : array of TGIS_Renderer3DVertex ;
  end;

  /// <summary>
  ///   Structure storing some vector layer params.
  /// </summary>
  TGIS_Renderer3DVectorLayerInfo = {$IFDEF OXYGENE} public {$ENDIF} record

     /// <summary>
     ///   Vector layer.
     /// </summary>
     Lv : TGIS_LayerVector ;

     /// <summary>
     ///   Z unit for vector layer.
     /// </summary>
     Zu : Double           ;

     /// <summary>
     ///   Z Normalization factor for vector layer.
     /// </summary>
     Nz  : Double           ;

     /// <summary>
     ///   M Normalization factor for vector layer.
     /// </summary>
     Nm  : Double           ;

     /// <summary>
     ///   CS scale factor.
     /// </summary>
     Fx  : Double           ;

     /// <summary>
     ///   Zmin in layer
     /// </summary>
     Zmi : Double           ;

     /// <summary>
     ///   Zmax in layer
     /// </summary>
     Zmx : Double           ;

     /// <summary>
     ///   Mmin in layer
     /// </summary>
     Mmi : Double           ;

     /// <summary>
     ///   Mmax in layer
     /// </summary>
     Mmx : Double           ;
  end;

  /// <summary>
  ///   Structure storing some DEM layer params.
  /// </summary>
  TGIS_Renderer3DDemLayerInfo = {$IFDEF OXYGENE} public {$ENDIF} record

     /// <summary>
     ///   Pixel layer.
     /// </summary>
     Lx  : TGIS_LayerPixel ;

     /// <summary>
     ///   Z unit for pixel layer.
     /// </summary>
     Zu  : Double           ;

     /// <summary>
     ///   Normalization factor for pixel layer.
     /// </summary>
     Nz  : Double           ;

     /// <summary>
     ///   Pixel shader state in 2D.
     /// </summary>
     Shd : Boolean          ;
  end;

  /// <summary>
  ///   Record storing shape info.
  /// </summary>
  TGIS_Renderer3DShapeSelectionInfo = {$IFDEF OXYGENE} public {$ENDIF} record

    /// <summary>
    ///   Shape UID.
    /// </summary>
    Uid  : TGIS_Uid ;

    /// <summary>
    ///   Shape part.
    /// </summary>
    Part : Integer ;

    /// <summary>
    ///   Shape beginning offset.
    /// </summary>
    Offset : Integer ;

    /// <summary>
    ///   Shape Color.
    /// </summary>
    Color : TGIS_Color ;

    /// <summary>
    ///   True if shape is selected.
    /// </summary>
    Selected : Boolean ;
  end;

  /// <summary>
  ///   Record storing info necessary to identify selected object.
  /// </summary>
  TGIS_Renderer3DSelectionInfo = {$IFDEF OXYGENE} public {$ENDIF} record

    /// <summary>
    ///   Buffer storing data.
    /// </summary>
    BufferType  : ShortInt ;

    /// <summary>
    ///   Buffer tile number.
    /// </summary>
    BufferIndex : Integer  ;

    /// <summary>
    ///   Layer.
    /// </summary>
    Layer       : TGIS_Layer ;

    /// <summary>
    ///   Array of Shape UIDs, Parts and offsets.
    /// </summary>
    ShpInfo     : TList<TGIS_Renderer3DShapeSelectionInfo>;

    /// <summary>
    ///   Index list of selected shapes in buffer.
    /// </summary>
    SelIdx      : TList<Integer>;
  end;

  /// <summary>
  ///   Array of T_bufSelInfo.
  /// </summary>
  TGIS_Renderer3DSelectionInfoArray = {$IFDEF OXYGENE} public {$ENDIF} array of
                                                  TGIS_Renderer3DSelectionInfo ;

  /// <summary>
  ///   Structure storing transparency value of meshtiles.
  /// </summary>
  TGIS_Renderer3DMeshTransparencyInfo = {$IFDEF OXYGENE} public {$ENDIF} record

    /// <summary>
    ///   TIN color triangle.
    /// </summary>
    TCT            : array of Cardinal ;

    /// <summary>
    ///   TIN textured triangle.
    /// </summary>
    TTT            : array of Cardinal ;

    /// <summary>
    ///   MultiPatch color.
    /// </summary>
    MPC            : array of Cardinal ;

    /// <summary>
    ///   MultiPatch textured.
    /// </summary>
    MPT            : array of Cardinal ;

    /// <summary>
    ///   Polygon color triangle - roof.
    /// </summary>
    PCTR           : array of Cardinal ;

    /// <summary>
    ///   Polygon color triangle - wall.
    /// </summary>
    PCTW           : array of Cardinal ;

    /// <summary>
    ///   Polygon textured triangle - roof.
    /// </summary>
    PTTR           : array of Cardinal ;

    /// <summary>
    ///   Polygon textured triangle - wall.
    /// </summary>
    PTTW           : array of Cardinal ;

    /// <summary>
    ///   Color line.
    /// </summary>
    LT             : array of Cardinal ;

    /// <summary>
    ///   Color point.
    /// </summary>
    PT             : array of Cardinal ;
  end;

  /// <summary>
  ///   Array storing info about level of polygon (roofs).
  /// </summary>
  TGIS_Renderer3DPolyRoofLevel = {$IFDEF OXYGENE} public {$ENDIF} array of array of Single ;

  /// <summary>
  ///   Structure storing vertex buffers for lines and points.
  /// </summary>
  TGIS_Renderer3DVectorTileInfo = {$IFDEF OXYGENE} public {$ENDIF} record

    /// <summary>
    ///   Number of verices in tile.
    /// </summary>
    Count  : Integer ;

    /// <summary>
    ///   Vertex buffer.
    /// </summary>
    Buffer : array of TGIS_Renderer3DVertex ;
  end;

  /// <summary>
  ///   Array of records storing vertex buffers for lines and points.
  /// </summary>
  TGIS_Renderer3DVectorTilesInfo = {$IFDEF OXYGENE} public {$ENDIF} array of
                                                 TGIS_Renderer3DVectorTileInfo ;

  /// <summary>
  ///   Dynamic vector layer tile description.
  /// </summary>
  TGIS_Renderer3DTrackingLayerInfo = {$IFDEF OXYGENE} public {$ENDIF} record
    /// <summary>
    ///   Vector layer.
    /// </summary>
    Layer     : TGIS_LayerVector ;

    /// <summary>
    ///   Vector tile type.
    /// </summary>
    TileType  : Integer          ;

    /// <summary>
    ///   Vector tile index.
    /// </summary>
    TileIndex : Integer          ;

    /// <summary>
    ///   To be drawn.
    /// </summary>
    ToDraw    : Boolean          ;
  end ;

  /// <summary>
  ///   Array of T_vectorTilesInfo records.
  /// </summary>
  TGIS_Renderer3DTrackingLayersInfo = {$IFDEF OXYGENE} public {$ENDIF} array of
                                              TGIS_Renderer3DTrackingLayerInfo ;

  /// <summary>
  ///   Available renderers.
  /// </summary>
  TRenderer3D = {$IFDEF OXYGENE} assembly {$ENDIF} (
    /// <summary>
    ///   DirectX9.
    /// </summary>
    DX9,

    /// <summary>
    ///   OpenGL.
    /// </summary>
    OGL,

    /// <summary>
    ///   FireMonkey.
    /// </summary>
    FMX
  ) ;

  /// <summary>
  ///   Set of defined renderers.
  /// </summary>
  TRenderers3D = {$IFDEF OXYGENE} assembly {$ENDIF} set of TRenderer3D;

type

  /// <summary>
  ///   Base class for all 3D rendering engines.
  /// </summary>
  TGIS_Renderer3DAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF} class ( TGIS_ObjectDisposable )

    {$IFNDEF CLR} private {$ELSE} unit {$ENDIF} // property values
      // Extent in which DEM is displayed in max LevelOfDetail
      //  (must be >= 0.5, 1 means screen width).

      dDemDetailExtentFactor : Double       ;
      // Extent in which DEM is displayed in lower LevelOfDetail
      // (must be >= 1.0, 4 means 4 times screen width).

      dDemDraftExtentFactor  : Double       ;
      // Extent from which vector is displayed
      // (must be >= 1.0, 2 means 2 times screen width).

      dVectorExtentFactor    : Double       ;
      // DEM grid size setting.
      iDemGridSize           : Integer      ;

      oTimer                 : TGIS_Timer   ;
      uponDestroy            : Boolean ;

      {$IFDEF CLR}
        FOnUpdate            : EventHandler ;
      {$ELSE}
        FOnUpdate            : TNotifyEvent ;
      {$ENDIF}

    {$IFNDEF CLR} private {$ELSE} unit {$ENDIF}//property access functions

      function  fget_DemDetailExtentFactor : Double ;
      procedure fset_DemDetailExtentFactor ( const _value : Double
                                           ) ;
      function  fget_DemDraftExtentFactor  : Double ;
      procedure fset_DemDraftExtentFactor  ( const _value : Double
                                           ) ;
      function  fget_VectorExtentFactor    : Double ;
      procedure fset_VectorExtentFactor    ( const _value : Double
                                           ) ;
      function  fget_VectorSimplification  : Boolean ;
      procedure fset_VectorSimplification  ( const _value : Boolean
                                           ) ;
      function  fget_VectorSmartSize       : Integer ;
      procedure fset_VectorSmartSize       ( const _value : Integer
                                           ) ;
      function  fget_DemGridSize           : Integer ;
      function  fget_DemCachedSize         : TGIS_Viewer3DDemCacheSize ;
      procedure fset_DemCachedSize         ( const _value :
                                             TGIS_Viewer3DDemCacheSize
                                           ) ;
      function  fget_IsBusy                : Boolean ;
      function  fget_MapRefreshLock        : Boolean ;
    private  // private variables

      xxMin, xxMax           : Double  ;
      yyMin, yyMax           : Double  ;
      lightControl           : Boolean ;
      drawArrow              : Boolean ;     // depends on bArrow
      gisScale               : Double  ;
      gisZoom                : Double  ;
      zDelta                 : Double  ;
      maxExt                 : Double  ;
      origOffset             : Double  ;
      wasReferenceSet        : Boolean ;
      allowDemTransp         : Boolean ;
      maxLod, maxLodStep     : Integer ;
      oldLodStep             : Integer ;
      leftColumn             : Integer ;
      bWasProjected          : Boolean ;
      unitDistance           : Double  ;
      errorTreatment         : Boolean ;
      heightRange            : Double  ;
      heightRangeMin         : Double  ;
      heightRangeMax         : Double  ;
      grdColumns             : Integer ;
      grdRows                : Integer ;
      groundType             : TGIS_3DGroundType ;
      basementType           : TGIS_3DBasementType ;
      minBasement            : Double  ;
      minBasement1           : Double  ;
      drawStrip              : Boolean ;

    private  // private functions

      {$IFDEF CLR}
        /// <summary>
        ///   Perform rerender threshold.
        /// </summary>
        procedure doOnTimer          ( sender : System.Object
//?                                       e      : EventArgs ElapsedEventArgs
                                     ) ;
      {$ELSE}
        /// <summary>
        ///   Perform rerender threshold.
        /// </summary>
        procedure doOnTimer          ( sender : TObject ) ;
      {$ENDIF}

    protected  // variables
      /// <summary>
      ///   Viewer 2D graphic window.
      /// </summary>
      oGIS                   : IGIS_Viewer ;

      /// <summary>
      ///   Viewer 3D graphic window.
      /// </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      oViewer                : IGIS_Viewer3D ;

      /// <summary>
      ///   Triangulation class object used to triangulate polygons.
      /// </summary>
      oPolyRoof              : TObject ;

      /// <summary>
      ///   Helper class object to create and keep list of textures.
      /// </summary>
      oTextureHelper         : TObject ;

      /// <summary>
      ///   Current 3D Viewer working mode.
      /// </summary>
      viewerMode             : TGIS_Viewer3DMode ;

      /// <summary>
      ///   Flag, if True update data buffer, if False use current data buffer.
      /// </summary>
      reRenderCall           : Boolean ;

      /// <summary>
      ///   Grid (DEM) array to be rendered.
      /// </summary>
      arGrid_r               : TGIS_GridArray ;

      /// <summary>
      ///   Temporary grid array used if there is more then 1 DEM in project.
      /// </summary>
      arGrid_w               : TGIS_GridArray ;

      /// <summary>
      ///   Bitmap used to create texture for DEM walls.
      /// </summary>
      arBitmap_w             : TGIS_Bitmap ;

      /// <summary>
      ///   Size of displayed DEM.
      /// </summary>
      demFrameSize           : Integer ;

      /// <summary>
      ///   Size of displayed image (BMP, JPG etc).
      /// </summary>
      imgFrameSize           : Integer ;

      /// <summary>
      ///   Size of one row of DEM.
      /// </summary>
      vertexBufferSize       : Integer ;

      /// <summary>
      ///   Default Z value.
      /// </summary>
      defaultZ               : Single  ;

      /// <summary>
      ///   Flag, if True light is on.
      /// </summary>
      lightSwitch            : Boolean ;

      /// <summary>
      ///   Flag, 1 prevents data buffer from unnecessary update.
      /// </summary>
      lockReadingData        : Integer ;

      /// <summary>
      ///   Camera position: height, azimuth in Deg, distance in 3D units.
      ///   See GetCameraPosition for details.
      /// </summary>
      cameraPosition         : TGIS_Point3D ;

      /// <summary>
      ///   Camera rotation in Deg.
      ///   See GetCameraRotation for details.
      /// </summary>
      cameraRotation         : TGIS_Point3D ;

      /// <summary>
      ///   Camera focal length in millimeters.
      /// </summary>
      cameraFocus            : Double ;

      /// <summary>
      ///   Camera angle of view in Deg.
      /// </summary>
      cameraViewAngle        : Double ;

      /// <summary>
      ///   Camera half angle of view in Deg.
      /// </summary>
      cameraHalfViewAngle    : Double ;

      /// <summary>
      ///   Camera position in 3D coordinates.
      ///   See GetCameraPositionEx for details.
      /// </summary>
      cameraPositionEx       : TGIS_Point3D ;

      /// <summary>
      ///   Light position: height, azimuth in radians.
      ///   See GetSunPosition for details.
      /// </summary>
      sunPosition            : TGIS_Point ;

      /// <summary>
      ///   Check if sun should stay stil in accordance to camera.
      ///   See GetKeepSunCamera for details.
      /// </summary>
      keepSunCamera          : Boolean ;

      /// <summary>
      ///   Shadow intensity.
      /// </summary>
      shadowsLevel           : Single ;

      /// <summary>
      ///   Vertex buffer storing color triangles (3D polygon interior).
      /// </summary>
      vTLBufCR               : TGIS_Renderer3DColorTriangleListBuffer  ;

      /// <summary>
      ///   Vertex buffer storing color triangles (3D polygon walls).
      /// </summary>
      vTLBufCW               : TGIS_Renderer3DColorTriangleListBuffer  ;

      /// <summary>
      ///   Vertex buffer storing textured triangles (3D polygon interior).
      /// </summary>
      vTLBufTR               : TGIS_Renderer3DTextureTriangleListBuffer ;

      /// <summary>
      ///   Vertex buffer storing textured triangles (3D polygon walls).
      /// </summary>
      vTLBufTW               : TGIS_Renderer3DTextureTriangleListBuffer ;

      /// <summary>
      ///   Vertex buffer storing textured triangles (DEM).
      /// </summary>
      wTLBufT                : TGIS_Renderer3DDemTriangleListBuffer ;

      /// <summary>
      ///   Vertex buffer storing color points.
      /// </summary>
      wPBuf                  : TGIS_Renderer3DPointBuffer ;

      /// <summary>
      ///   Vertex buffer storing color lines.
      /// </summary>
      wPLBuf                 : TGIS_Renderer3DPointListBuffer ;

      /// <summary>
      ///   Vertex buffer storing color triangles (TIN).
      /// </summary>
      tinBufC                : TGIS_Renderer3DColorTriangleListBuffer ;

      /// <summary>
      ///   Vertex buffer storing textured triangles (TIN).
      /// </summary>
      tinBufT                : TGIS_Renderer3DDemTriangleListBuffer ;

      /// <summary>
      ///   Vertex buffer storing color triangles (MultiPatch).
      /// </summary>
      mpatchBufC             : TGIS_Renderer3DColorTriangleListBuffer ;

      /// <summary>
      ///   Vertex buffer storing textured triangles (MultiPatch).
      /// </summary>
      mpatchBufT             : TGIS_Renderer3DTextureTriangleListBuffer ;

      /// <summary>
      ///   Array storing info necessary to identify selected object.
      /// </summary>
      arBufSelInfo           : TGIS_Renderer3DSelectionInfoArray ;

      /// <summary>
      ///   Number of arrays storing info necessary to identify selected object.
      /// </summary>
      bufSelInfoCounter      : Integer ;

      /// <summary>
      ///   Number of DEM vertex buffers in higher level of detail.
      /// </summary>
      meshDemDetailIndex     : Integer ;

      /// <summary>
      ///   Number of DEM vertex buffers.
      /// </summary>
      meshDemTilesCounter    : Integer ;

      /// <summary>
      ///   Number of DEM walls vertex buffers.
      /// </summary>
      meshWallTilesCounter   : Integer ;

      /// <summary>
      ///   Number of color TIN vertex buffers.
      /// </summary>
      meshTinTilesCounterC   : Integer ;

      /// <summary>
      ///   Number of textured TIN vertex buffers.
      /// </summary>
      meshTinTilesCounterT   : Integer ;

      /// <summary>
      ///   Number of color MultiPatch vertex buffers.
      /// </summary>
      meshMPatchTilesCounterC: Integer ;

      /// <summary>
      ///   Number of textured MultiPatch vertex buffers.
      /// </summary>
      meshMPatchTilesCounterT: Integer ;

      /// <summary>
      ///   Number of MultiPatch subsets.
      /// </summary>
      mpatchSubsetCounter    : Integer ;

      /// <summary>
      ///   Number of color 3D polygon interior vertex buffers.
      /// </summary>
      meshVectTilesCounterCR : Integer ;

      /// <summary>
      ///   Number of color 3D polygon wall vertex buffers.
      /// </summary>
      meshVectTilesCounterCW : Integer ;

      /// <summary>
      ///   Number of textured 3D polygon interior vertex buffers.
      /// </summary>
      meshVectTilesCounterTR : Integer ;

      /// <summary>
      ///   Number of textured 3D polygon wall vertex buffers.
      /// </summary>
      meshVectTilesCounterTW : Integer ;

      /// <summary>
      ///   Number of color line vertex buffers.
      /// </summary>
      meshLineTilesCounter   : Integer ;

      /// <summary>
      ///   Number of color point vertex buffers.
      /// </summary>
      meshPointTilesCounter  : Integer ;

      /// <summary>
      ///   Array storing info about zlevel of color 3D polygon interior.
      /// </summary>
      arVectPolyRoofC        : TGIS_Renderer3DPolyRoofLevel ;

      /// <summary>
      ///   Number of rows in arVectPolyRoofC array.
      /// </summary>
      arVectPolyRoofCounterC : Integer ;

      /// <summary>
      ///   Array storing info about zlevel of textured 3D polygon interior.
      /// </summary>
      arVectPolyRoofT        : TGIS_Renderer3DPolyRoofLevel ;

      /// <summary>
      ///   Number of rows in arVectPolyRoofT array.
      /// </summary>
      arVectPolyRoofCounterT : Integer ;

      /// <summary>
      ///   Array of records storing vertex buffers for lines.
      /// </summary>
      meshLineTiles          : TGIS_Renderer3DVectorTilesInfo   ;

      /// <summary>
      ///   Array of records storing vertex buffers for points.
      /// </summary>
      meshPointTiles         : TGIS_Renderer3DVectorTilesInfo   ;

      /// <summary>
      ///   Array of records storing structure of vertex buffers for lines and points.
      /// </summary>
      arVectorTilesInfo      : TGIS_Renderer3DTrackingLayersInfo ;

      /// <summary>
      ///   Number of arVectorTilesInfo.
      /// </summary>
      vectorTilesInfoCounter : Integer ;

      /// <summary>
      ///   Structure storing transparency of meshtiles.
      /// </summary>
      transpInfo             : TGIS_Renderer3DMeshTransparencyInfo ;

      /// <summary>
      ///   Index of mesh subset currently processed.
      /// </summary>
      curMeshSubset          : Integer ;

      /// <summary>
      ///   Index of last mesh subset processed (roof, polygon interior).
      /// </summary>
      lastMeshSubsetR        : Integer ;

      /// <summary>
      ///   Index of last mesh subset processed (polygon wall.
      /// </summary>
      lastMeshSubsetW        : Integer ;

      /// <summary>
      ///   Flag, True means that DEM mesh can be rendered.
      /// </summary>
      demMeshExists          : Boolean ;

      /// <summary>
      ///   Flag, True means that initial rendering is currently running.
      /// </summary>
      firstScene             : Boolean ;

      /// <summary>
      ///   Flag, True means that location of first DEM row is found after
      ///   sequence of preceding nodata values.
      /// </summary>
      firstRowFound          : Boolean ;

      /// <summary>
      ///   Last camera to rotation center distance.
      /// </summary>
      lastRadius             : Double  ;

      /// <summary>
      ///   Last offset on X axis.
      /// </summary>
      lastTurn               : Double  ;

      /// <summary>
      ///   Last offset on Y axis.
      /// </summary>
      lastFly                : Double  ;

      /// <summary>
      ///   Last value of scale along Z axis.
      /// </summary>
      lastScaleZ             : Double  ;

      /// <summary>
      ///   Flag, True forces scene to be updated.
      /// </summary>
      bMustReRender          : Boolean ;

      /// <summary>
      ///   Distance camera to center.
      /// </summary>
      dRadius                : Double ;

      /// <summary>
      ///   X offset, used to move surface left/right.
      /// </summary>
      dTurn                  : Double ;

      /// <summary>
      ///   Y offset, used to move surface fwd/backwd.
      /// </summary>
      dFly                   : Double ;

      /// <summary>
      ///   Initial camera to rotation center value.
      /// </summary>
      initRadius             : Double ;

      /// <summary>
      ///   Initial X offset.
      /// </summary>
      initTurn               : Double ;

      /// <summary>
      ///   Initial Y offset.
      /// </summary>
      initFly                : Double ;

      /// <summary>
      ///   Z Value magnification coefficient.
      /// </summary>
      zFactor                : Double ;

      /// <summary>
      ///   M Value magnification coefficient.
      /// </summary>
      mFactor                : Double ;

      /// <summary>
      ///   Rotation angle along X axis.
      /// </summary>
      spinX                  : Double ;

      /// <summary>
      ///   Rotation angle along Y axis.
      /// </summary>
      spinY                  : Double ;

      /// <summary>
      ///   Additional part of rotation angle along X axis (used in specific cases).
      /// </summary>
      spinZ                  : Double ;

      /// <summary>
      ///   Rotation angle along X axis.
      /// </summary>
      cspinX                 : Double ;

      /// <summary>
      ///   Flag, True means that normal is calculated from CW ordered points, False CCW.
      /// </summary>
      bLightVector           : Boolean ;

      /// <summary>
      ///   Shape currently processed.
      /// </summary>
      currentSHP             : TGIS_Shape ;

      /// <summary>
      ///   Vector layer currently processed.
      /// </summary>
      currentVL              : TGIS_LayerVector ;

      /// <summary>
      ///   DEM as Mesh layer currently processed.
      /// </summary>
      currentML              : TGIS_LayerPixel ;

      /// <summary>
      ///   3D renderer currently in use (DX9, OGL, FMX).
      /// </summary>
      renderer3D             : TRenderers3D ;

      /// <summary>
      ///   Error message .
      /// </summary>
      errorMsg               : String ;

      /// <summary>
      ///   Flag, Enable/disable polygon simplification process.
      /// </summary>
      bVectorSimplification  : Boolean ;

      /// <summary>
      ///   Shape size in pixels, less or equal will not be displayed,
      ///   ( must be >= 0 ).
      /// </summary>
      iVectorSmartSize       : Integer ;

      /// <summary>
      ///   Timer marker used to calculate frames per sec rate.
      /// </summary>
      timeStart              : Int64   ;

      /// <summary>
      ///   Timer marker used to calculate frames per sec rate.
      /// </summary>
      timeEnd                : Int64   ;

      /// <summary>
      ///   Flag, 1 means that lock functions are blocked.
      /// </summary>
      generalLock            : Integer ;

      /// <summary>
      ///   Flag indicated Rerender paint stage.
      /// </summary>
      inPaint                : Boolean ;

      /// <summary>
      ///   Flag indicated Draw paint stage.
      /// </summary>
      inPaint1                : Boolean ;

      /// <summary>
      ///   Grid size used to display regular DEMs or IMAGEs.
      /// </summary>
      enDemCachedSize        : TGIS_Viewer3DDemCacheSize  ;

      /// <summary>
      ///   Flag, advance navigation On/Off switch.
      /// </summary>
      advNavigation          : Boolean ;

      /// <summary>
      ///   Flag, orthogonal view On/Off switch.
      /// </summary>
      orthoMode              : Boolean ;

      /// <summary>
      ///   Flag, solid/wireframe On/Off switch.
      /// </summary>
      dispMode               : Boolean ;

      /// <summary>
      ///   Flag, label On/Off switch.
      /// </summary>
      labelMode              : Boolean ;

      /// <summary>
      ///   Flag, label On/Off switch.
      /// </summary>
      labelModeEx            : Boolean ;

      /// <summary>
      ///   Flag, precise window On/Off switch.
      /// </summary>
      preciseSwitch          : Boolean ;

      /// <summary>
      ///   Flag, vector edges drawing On/Off switch.
      /// </summary>
      vectorEdges            : Boolean ;

      /// <summary>
      ///   Flag, transparent faces drawing On/Off switch.
      /// </summary>
      drawTransparent        : Boolean ;

      /// <summary>
      ///   Flag, northArrow drawing On/Off switch.
      /// </summary>
      bArrow                 : Boolean ;

      /// <summary>
      ///   Flag, dynamic layer drawing On/Off switch.
      /// </summary>
      bPartialUpdate         : Boolean ;

      /// <summary>
      ///   Flag, if True assign DEM Z value to origin point.
      /// </summary>
      bTrace                 : Boolean ;

      /// <summary>
      ///   Flag, if True no DEM in current scene.
      /// </summary>
      noDem                  : Boolean ;

      /// <summary>
      ///   Flag, if True no Pixel layer in current scene.
      /// </summary>
      noImg                  : Boolean ;

      /// <summary>
      ///   Flag, if True no 3D Polygon interiors will be drawn.
      /// </summary>
      noRoof                 : Boolean ;

      /// <summary>
      ///   Flag, if True no 3D Polygon walls will be drawn.
      /// </summary>
      noWall                 : Boolean ;

      /// <summary>
      ///   Flag, if True scene will not be updated.
      /// </summary>
      bFastMode              : Boolean ;

      /// <summary>
      ///   Flag, if True ellipsoid height will not be added to Z value.
      /// </summary>
      bIgnoreEllipsoidHeight : Boolean ;

      /// <summary>
      ///   Flag, AdvNavigation On/Off switch.
      /// </summary>
      newTransform           : Boolean ;

      /// <summary>
      ///   Flag, used during AdvNavigation.
      /// </summary>
      changeRotPoint         : Boolean ;

      /// <summary>
      ///   Flag, True TIN has overlaid bitmap.
      /// </summary>
      tinBmp                 : Boolean ;

      /// <summary>
      ///   Flag, True shape interior has overlaid bitmap.
      /// </summary>
      arBmp                  : Boolean ;

      /// <summary>
      ///   Flag, True shape outline has overlaid bitmap.
      /// </summary>
      ouBmp                  : Boolean ;

      /// <summary>
      ///   DEM  Zvalue normalization factor.
      /// </summary>
      zDEMNormCoeff          : Double ;      // Z DEM normalization factor

      /// <summary>
      ///   zFactor * projection proportional coefficient.
      /// </summary>
      zScaleFactor           : Double ;

      /// <summary>
      ///   Value of Zunit in current coordinate system.
      /// </summary>
      gisCSzUnit             : Double ;

      /// <summary>
      ///   Current viewport width.
      /// </summary>
      windowWidth            : Integer ;

      /// <summary>
      ///   Current viewport height.
      /// </summary>
      windowHeight           : Integer ;

      /// <summary>
      ///   Previous viewport width.
      /// </summary>
      oldwindowWidth         : Integer ;

      /// <summary>
      ///   Previous viewport height.
      /// </summary>
      oldwindowHeight        : Integer ;

      /// <summary>
      ///   Scene size along X axis in 3D units.
      /// </summary>
      wndXSize               : Double ;

      /// <summary>
      ///   Z reference level.
      /// </summary>
      zLevel                 : Double ;

      /// <summary>
      ///   Min Z value.
      /// </summary>
      zLevelMin              : Double ;

      /// <summary>
      ///   Max Z value.
      /// </summary>
      zLevelMax              : Double ;

      /// <summary>
      ///   Unit on Z axis.
      /// </summary>
      zUnit                  : Double ;

      /// <summary>
      ///   Screen height to width ratio.
      /// </summary>
      scrRatio               : Double ;

      /// <summary>
      ///   Value used to adjust cut extent.
      /// </summary>
      adjustRatio            : Double ;

      /// <summary>
      ///   Grid height to width ratio.
      /// </summary>
      grdRatio               : Double ;

      /// <summary>
      ///   Undefined grid data.
      /// </summary>
      noDataValue            : Double ;

      /// <summary>
      ///   Half of grid size in normalized units.
      /// </summary>
      baseSize               : Double ;

      /// <summary>
      ///   Extent of all opened grids.
      /// </summary>
      gridsExt               : TGIS_Extent ;

      /// <summary>
      ///   Current extent of all opened layers.
      /// </summary>
      rangeExtent            : TGIS_Extent3D ;

      /// <summary>
      ///   Extent of detail window (DEM).
      /// </summary>
      detailExtent           : TGIS_Extent ;

      /// <summary>
      ///   Extent visible in scene (DEM).
      /// </summary>
      roughExtent            : TGIS_Extent ;

      /// <summary>
      ///   Extent of vector layers.
      /// </summary>
      vectExtent             : TGIS_Extent ;

      /// <summary>
      ///   Cut extent of vector layers (viewer context).
      /// </summary>
      cutVectExtentV         : TGIS_Extent ;

      /// <summary>
      ///   Cut extent of vector layers (layer context).
      /// </summary>
      cutVectExtentL         : TGIS_Extent ;

      /// <summary>
      ///   Extent used for reference point moving above DEM.
      /// </summary>
      traceExtent            : TGIS_Extent ;

      /// <summary>
      ///   Initial visible extent context, full extent of GIS window.
      /// </summary>
      exExtent               : TGIS_Extent ;

      /// <summary>
      ///   Full extent of GIS window in normalized units.
      /// </summary>
      normExt                : TGIS_Extent ;

      /// <summary>
      ///   Current screen extent in normalized units.
      /// </summary>
      scrExtent              : TGIS_Extent ;

      /// <summary>
      ///   Current screen extent in map units.
      /// </summary>
      mapExtent              : TGIS_Extent ;

      /// <summary>
      ///   Vector outline color.
      /// </summary>
      edgesColor             : TGIS_Color ;

      /// <summary>
      ///   Basic camera position.
      /// </summary>
      basicEyeAt             : TVector3f ;

      /// <summary>
      ///   Basic rotation center.
      /// </summary>
      basicEyeTo             : TVector3f ;

      /// <summary>
      ///   Current camera position.
      /// </summary>
      eyeAt                  : TVector3f ;

      /// <summary>
      ///   Current rotation center.
      /// </summary>
      eyeTo                  : TVector3f ;

      /// <summary>
      ///   Current X rotation center coordinate.
      /// </summary>
      eyeX                   : Single ;

      /// <summary>
      ///   Current Y rotation center coordinate.
      /// </summary>
      eyeY                   : Single ;

      /// <summary>
      ///   Current Z rotation center coordinate.
      /// </summary>
      zEyeTo                 : Double ;

      /// <summary>
      ///   Current Z offset of rotation center coordinate.
      /// </summary>
      zOffset                : Double ;

      /// <summary>
      ///   Current reference point mode ( Z value of rotation point).
      /// </summary>
      referencePointMode     : TGIS_Viewer3DReferenceMode ;

      /// <summary>
      ///   Current reference point Z offset.
      /// </summary>
      referencePointOffset  : Double ;

      /// <summary>
      ///   Current reference level mode (type of reference level).
      /// </summary>
      referenceMode          : TGIS_Viewer3DReferenceLevelMode ;


      /// <summary>
      ///   Current camera position restriction.
      /// </summary>
      viewRestriction        : TGIS_Viewer3DViewRestriction ;

      /// <summary>
      ///   Total number of vector layers on scene.
      /// </summary>
      curVLSize              : Integer ;

      /// <summary>
      ///   Current number of vector layers on scene.
      /// </summary>
      curVLListSize          : Integer ;

      /// <summary>
      ///   Array storing some current vector layer parameters.
      /// </summary>
      curVL                  : array of TGIS_Renderer3DVectorLayerInfo ;

      /// <summary>
      ///   Array storing number of shapes displayed from current vector layer.
      /// </summary>
      curVLlist              : array of Integer ;

      /// <summary>
      ///   Current number of DEM layers on scene.
      /// </summary>
      curDemSize             : Integer ;

      /// <summary>
      ///   Array storing some current DEM layer parameters.
      /// </summary>
      curDem                 : array of TGIS_Renderer3DDemLayerInfo ;

      /// <summary>
      ///   Array storing some current DEM/Mesh layer parameters.
      /// </summary>
      curMesh                : array of TGIS_Renderer3DDemLayerInfo ;

      /// <summary>
      ///   Current number of DEM/Mesh layers on scene.
      /// </summary>
      curMeshSize            : Integer ;

      /// <summary>
      ///   Texture currently used for texturing.
      /// </summary>
      curTEX                 : Integer ;

      /// <summary>
      ///   Grid Cell size in world units (X axis).
      /// </summary>
      xDemSize               : Double  ;

      /// <summary>
      ///   Grid Cell size in world units (Y axis).
      /// </summary>
      yDemSize               : Double  ;

      /// <summary>
      ///   Grid Cell size in normalized units (X axis).
      /// </summary>
      nxDemSize              : Double  ;

      /// <summary>
      ///   Grid Cell size in normalized units (Y axis).
      /// </summary>
      nyDemSize              : Double  ;


      /// <summary>
      ///   Min Z value in 3D units.
      /// </summary>
      zMin                   : Double  ;

      /// <summary>
      ///   Flag indicating if Ignore/Cut default value is to be used .
      ///   False means that Ignore/Cut values were set by user.
      /// </summary>
      bDefaultIgnoreCut     : Boolean ;

      /// <summary>
      ///   Z value in map units. Values above will not be drawn.
      /// </summary>
      dIgnoreAbove           : Double  ;

      /// <summary>
      ///   Z value in map units (single prec). Values above will not be drawn.
      /// </summary>
      sglIgnoreAbove         : Single  ;

      /// <summary>
      ///   Z value in map units. Values below will not be drawn.
      /// </summary>
      dIgnoreBelow           : Double  ;

      /// <summary>
      ///   Z value in map units (single prec). Values below will not be drawn.
      /// </summary>
      sglIgnoreBelow         : Single  ;

      /// <summary>
      ///   Z value in map units. Values above will have this value.
      /// </summary>
      dCutAbove              : Double  ;

      /// <summary>
      ///   Z value in map units. Values below will have this value.
      /// </summary>
      dCutBelow              : Double  ;

      /// <summary>
      ///   Z value in map units (single prec). Values below will have this value.
      /// </summary>
      sglCutBelow            : Single  ;

      /// <summary>
      ///   3D position pointed by the current mouse location .
      /// </summary>
      mousePos3D             : TGIS_Point3D ;

      /// <summary>
      ///   Last mouse down position.
      /// </summary>
      lastMouseDownPos       : TPoint ;

      /// <summary>
      ///   Way of DEM wall rendering.
      /// </summary>
      demWall                : TGIS_Viewer3DDemWall ;

      /// <summary>
      ///   Way of DEM texture rendering.
      ///   True - surface texture, False - wall texture (with isolines).
      /// </summary>
      imageTexture           : Boolean ;

      /// <summary>
      ///   Distance between isolines.
      /// </summary>
      isolineGap             : Double ;

      /// <summary>
      ///   Isoline color.
      /// </summary>
      isolineColor           : TGIS_Color ;

      /// <summary>
      ///   DEM wall color.
      /// </summary>
      solidWallColor         : TGIS_Color ;

      /// <summary>
      ///   Object used for flood simulation.
      /// </summary>
      flood                  : TGIS_Viewer3DFlood ;

      /// <summary>
      ///   Object used for plane rendering.
      /// </summary>
      basePlane              : TGIS_Viewer3DBasePlane ;

      /// <summary>
      ///   Final DEM transparency.
      /// </summary>
      demTransparency        : Cardinal ;

      /// <summary>
      ///   Current DEM transparency.
      /// </summary>
      curDemTransp           : Cardinal ;

      /// <summary>
      ///   Transparency priority, order of DEM/vector rendering.
      /// </summary>
      transpPrior            : TGIS_Viewer3DTransparencyPriority ;

      /// <summary>
      ///   see DemDraftExtentFactor for details.
      /// </summary>
      roughCoeff             : Double ;

      /// <summary>
      ///   see DemDetailExtentFactor  for details.
      /// </summary>
      detailCoeff            : Double ;

      /// <summary>
      ///   see  Vector ExtentFactor for details.
      /// </summary>
      vectorCoeff            : Double  ;

      /// <summary>
      ///   Level Of Detail.
      /// </summary>
      lod                    : Integer ;

      /// <summary>
      ///   Level Of Detail.
      /// </summary>
      lodStep                : Integer ;

      /// <summary>
      ///   Frame per second rate.
      /// </summary>
      iFps                   : Integer ;

      /// <summary>
      ///   Flag, used if extents must be recalculated.
      /// </summary>
      mustAdjust             : Boolean ;

      /// <summary>
      ///   Flag, used to set vector texture direction rendering.
      ///  True - left right .
      /// </summary>
      texDir                 : Boolean ;

      /// <summary>
      ///   Flag, used to set vector texture direction rendering.
      ///  True - up down.
      /// </summary>
      texMode                : Boolean ;

      /// <summary>
      ///   Flag, used during screenshot.
      /// </summary>
      captureScreen          : Boolean ;

      /// <summary>
      ///   Original shapetype.
      /// </summary>
      srcShpType             : TGIS_ShapeType ;

      /// <summary>
      ///   Current vector layer normalized Z value.
      /// </summary>
      currentVLnz            : Double ;

      /// <summary>
      ///   Current vector layer normalized m value.
      /// </summary>
      currentVLnm            : Double ;

      /// <summary>
      ///   Current vector layer min Z value.
      /// </summary>
      currentVLzmi            : Double ;

      /// <summary>
      ///   Current vector layer min m value.
      /// </summary>
      currentVLmmi            : Double ;


      /// <summary>
      ///   MultiPatch texture subset number.
      /// </summary>
      subsetTexMP            : Integer ;

      /// <summary>
      ///   Detail window params.
      /// </summary>
      detWnd                 : TRect ;

      /// <summary>
      ///   Current shape number to be rendered.
      /// </summary>
      shpNo                  : Integer ;

      /// <summary>
      ///   Texture to be used during rendering.
      /// </summary>
      curTxt                 : TGIS_Bitmap ;

      /// <summary>
      ///   Current pixel size in map units.
      /// </summary>
      pixSize                : Double ;

      /// <summary>
      ///   Vector layer projected Z unit.
      /// </summary>
      projFactor             : Double ;

      /// <summary>
      ///   Vector layer projected m unit.
      /// </summary>
      projMFactor            : Double ;

      /// <summary>
      ///   Vector outline color currently used.
      /// </summary>
      edColor                : DWORD ;

      /// <summary>
      ///   Flag, True - shape consists of triangles only.
      /// </summary>
      shpIsTin               : Boolean ;

      /// <summary>
      ///   Flag, True - shape consists of triangles only (False MultiPatch).
      /// </summary>
      falseMultiPatch        : Boolean ;

      /// <summary>
      ///   length of 3D marker (point) representation.
      /// </summary>
      markerLgth             : Double  ;

      /// <summary>
      ///   Normalized surface spatial XMin extent value.
      /// </summary>
      xMin1                  : Double  ;

      /// <summary>
      ///   Normalized surface spatial XMax extent value.
      /// </summary>
      xMax1                  : Double  ;

      /// <summary>
      ///   Normalized surface spatial YMin extent value.
      /// </summary>
      yMin1                  : Double  ;

      /// <summary>
      ///   Normalized surface spatial YMax extent value.
      /// </summary>
      yMax1                  : Double  ;

      /// <summary>
      ///   Normalized surface spatial XMin extent value.
      /// </summary>
      xMin                   : Double  ;

      /// <summary>
      ///   Normalized surface spatial XMax extent value.
      /// </summary>
      xMax                   : Double  ;

      /// <summary>
      ///   Normalized surface spatial YMin extent value.
      /// </summary>
      yMin                   : Double  ;

      /// <summary>
      ///   Normalized surface spatial YMax extent value.
      /// </summary>
      yMax                   : Double  ;

      /// <summary>
      ///   Flag indicating window resize.
      /// </summary>
      windowSizeChanged      : Boolean  ;

      /// <summary>
      ///   Printer PPI used during PrintTile rendering.
      /// </summary>
      printBmpPPI            : Integer ;

      /// <summary>
      ///   Bitmap size used during PrintTile rendering.
      /// </summary>
      printBmpImgSize        : Integer ;

      /// <summary>
      ///   Output size of printout (in pixels) used during PrintTile rendering.
      /// </summary>
      printBmpOutSize        : TPoint  ;

      /// <summary>
      ///   Flag informing FMX that Timer was used (for proper labelModeEx action).
      /// </summary>
      wasTimer               : Boolean ;

      /// <summary>
      ///   Flag informing that option was selected (for proper labelModeEx action).
      /// </summary>
      optionSelected         : Boolean ;

      /// <summary>
      ///   Initial video vertex buffer size.
      /// </summary>
      initVideoVBSize            : Integer ;

      /// <summary>
      ///   Video vertex buffer increment size.
      /// </summary>
      videoVBIncrement          : Integer ;

      /// <summary>
      ///   Default vertex buffer size. Must be a multiplication of number 6.
      /// </summary>
      videoVertexBufferSize     : Integer ;

      /// <summary>
      ///   Default max vertex buffer size. Must be a multiplication of number 6.
      /// </summary>
      maxVertexBufferSize       : Integer ;

      /// <summary>
      ///   If True size of vertex buffer is limited to the initVideoVBSize value,
      ///   otherwise attempt to load whole layer into one big buffer.
      /// </summary>
      safeRendering             : Boolean ;

      /// <summary>
      ///   If True safeRendering id already switched on
      /// </summary>
      safeRenderingDone         : Boolean ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy              ; override;

      /// <summary>
      ///   Fill all vertex buffers with data according to current scene setting.
      /// </summary>
      procedure prepareVertexBuffersToDraw ; virtual; abstract;

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
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Release previously allocated arrays &amp; objects.
      /// </summary>
      procedure releaseGeometry        ; virtual;

      /// <summary>
      ///   Release previously allocated meshes.
      /// </summary>
      procedure freeMeshStore          ; virtual; abstract;

      /// <summary>
      ///   Scale all Zvalues in mesh tiles by _value.
      /// </summary>
      /// <param name="_value">
      ///   Z scaling coefficient
      /// </param>
      procedure scaleMeshTilesZ        ( const _value : Double
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Scale all Mvalues in mesh tiles by _value.
      /// </summary>
      /// <param name="_value">
      ///   M scaling coefficient
      /// </param>
      procedure scaleMeshTilesM        ( const _value : Double
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Convert 3D coordinates to map.
      /// </summary>
      /// <param name="_pnt">
      ///   3Dpoint in 3D units
      /// </param>
      /// <returns>
      ///   3Dpoint in map units.
      /// </returns>
      function  d3DToMap               ( const _pnt : TGIS_Point3D
                                       ) : TGIS_Point3D ;

      /// <summary>
      ///   Convert map coordinates to 3D.
      /// </summary>
      /// <param name="_pnt">
      ///   3Dpoint in map units
      /// </param>
      /// <returns>
      ///   Map coordinates converted to 3D units.
      /// </returns>
      function  mapToD3D               ( const _pnt : TGIS_Point3D
                                       ) : TGIS_Point3D ;

      /// <summary>
      ///   Set reference point parameters.
      /// </summary>
      /// <param name="_mode">
      ///   reference mode
      /// </param>
      /// <param name="_offset">
      ///   value added to reference point
      /// </param>
      procedure setReferencePoint      ( const _mode :
                                           TGIS_Viewer3DReferenceMode ;
                                         const _offset : Double
                                       ) ;

      /// <summary>
      ///   Set reference level parameters.
      /// </summary>
      /// <param name="_mode">
      ///   reference mode
      /// </param>
      /// <param name="_offset">
      ///   value added to ReferenceLevel
      /// </param>
      procedure setReferenceParams     ( const _mode :
                                           TGIS_Viewer3DReferenceLevelMode ;
                                         const _offset : Double
                                       ) ;

      /// <summary>
      ///   Set Z value above zero.
      /// </summary>
      /// <param name="_value">
      ///   Z value above zero
      /// </param>
      procedure setReferenceLevel      ( const _value : Double
                                       ) ;

      /// <summary>
      ///   Get Z value at origin point.
      /// </summary>
      /// <returns>
      ///   Reference level value.
      /// </returns>
      function  getReferenceLevel      : Double ;

      /// <summary>
      ///   Set reference point on DEM.
      /// </summary>
      /// <param name="_offset">
      ///   offset above DEM
      /// </param>
      procedure setPointOnDEM          ( const _offset : Double
                                       ) ;

      /// <summary>
      ///   Assign DEM Z value to origin point.
      /// </summary>
      /// <param name="_value">
      ///   True if tracing is active
      /// </param>
      procedure traceReferenceLevel    ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Returns X coordinate of grid cell in normalized units.
      /// </summary>
      /// <param name="_col">
      ///   grid column number
      /// </param>
      /// <returns>
      ///   X coordinate of grid cell.
      /// </returns>
      function  xValue                 ( const _col : Integer
                                       ) : Double ;

      /// <summary>
      ///   Returns Y coordinate of grid cell in normalized units.
      /// </summary>
      /// <param name="_row">
      ///   grid row number
      /// </param>
      /// <returns>
      ///   Y coordinate of grid cell.
      /// </returns>
      function  yValue                 ( const _row : Integer
                                       ) : Double ;

      /// <summary>
      ///   Returns Z coordinate of grid cell (zero for borders &amp; nodata).
      /// </summary>
      /// <param name="_col">
      ///   grid column number
      /// </param>
      /// <param name="_row">
      ///   grid row number
      /// </param>
      /// <returns>
      ///   Z coordinate of grid cell.
      /// </returns>
      function  zValue                 ( const _col : Integer ;
                                         const _row : Integer
                                       ) : Single ;

      /// <summary>
      ///   Get value of texture of grid cell, horizontal parameter.
      /// </summary>
      /// <param name="_col">
      ///   grid column number
      /// </param>
      /// <returns>
      ///   X texture coordinate of grid cell.
      /// </returns>
      function  tuValue                ( const _col : Integer
                                       ) : Single ;

      /// <summary>
      ///   Get value of texture of grid cell, vertical parameter.
      /// </summary>
      /// <param name="_row">
      ///   grid row number
      /// </param>
      /// <returns>
      ///   Y texture coordinate of grid cell.
      /// </returns>
      function  tvValue                ( const _row : Integer
                                       ) : Single ; virtual; abstract;

      /// <summary>
      ///   Check if current Coordinate System is or is assumed to be geographic.
      /// </summary>
      /// <returns>
      ///   True if Coordinate System is geographic, False if not.
      /// </returns>
      function  checkCSWGS             : Boolean ;

      /// <summary>
      ///   Truncate angle to 2Pi value.
      /// </summary>
      /// <param name="_value">
      ///   angle to be truncated
      /// </param>
      /// <returns>
      ///   Value truncated to 2Pi.
      /// </returns>
      function  truncateTo2Pi          ( const _value : Double
                                       ) : Double ;

      /// <summary>
      ///   Truncate angle to 360 value.
      /// </summary>
      /// <param name="_value">
      ///   angle to be truncated
      /// </param>
      /// <returns>
      ///   Value truncated to 360 Deg.
      /// </returns>
      function  truncateTo360          ( const _value : Double
                                       ) : Double ;

      /// <summary>
      ///   Set camera rotation along X axis.
      /// </summary>
      /// <param name="_value">
      ///   rotation angle
      /// </param>
      procedure setCameraRotationX     ( const _value : Double
                                       ) ;

      /// <summary>
      ///   Reverse given value.
      /// </summary>
      /// <param name="_value">
      ///   value to be reversed
      /// </param>
      /// <returns>
      ///   Reversed value.
      /// </returns>
      function  reverseValue           ( const _value : Single
                                       ) : Single ; virtual; abstract;

      /// <summary>
      ///   Set required portion of Ambient Light.
      /// </summary>
      /// <param name="_val">
      ///   shadow level (0 - light, 100 - dark)
      /// </param>
      procedure setAmbientLightColor   ( const _val  : Integer
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Create texture for DEM walls.
      /// </summary>
      procedure makeWallTexture        ; virtual; abstract;

      /// <summary>
      ///   Create texture for basement plane walls.
      /// </summary>
      procedure makeBasementTex        ; virtual; abstract;

      /// <summary>
      ///   Create textures for SkyBox and Sun.
      /// </summary>
      procedure makeSkyBoxTex          ; virtual; abstract;

      /// <summary>
      ///   Add dynamic vector layer tile info entry (layer.CachedPaint is False).
      /// </summary>
      /// <param name="_tp">
      ///   tile type
      /// </param>
      /// <param name="_no">
      ///   tile index
      /// </param>
      procedure addVectorTilesInfo     ( const _tp  : Integer ;
                                         const _no  : Integer
                                       ) ;

      /// <summary>
      ///   Release all previously allocated vertex buffers.
      /// </summary>
      procedure releaseDataVB          ; virtual; abstract;

      /// <summary>
      ///   Fill grid array with noDataValue.
      /// </summary>
      /// <param name="_array">
      ///   grid array to be initialized
      /// </param>
      procedure init_arGrid            ( var _array    : TGIS_GridArray
                                       );

      /// <summary>
      ///   Calculate DEM left column position.
      /// </summary>
      procedure setLeftColumn          ;

      /// <summary>
      ///   Reinit lost 3D environment.
      /// </summary>
      procedure reInit3D               ; virtual; abstract;

      /// <summary>
      ///   Calculate scene params for current rotation point position.
      /// </summary>
      /// <param name="_value">
      ///   if True then new rotation point is set, otherwise screen center is set.
      /// </param>
      procedure adjustTransformSetting ( _value : Boolean
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Set and execute current transformation.
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  transformSetting       : Integer ; virtual; abstract;

      /// <summary>
      ///   Set initial DEM Z values to be ignored or cut.
      /// </summary>
      procedure setIgnoreCutValues     ;

      /// <summary>
      ///   Mark/unmark shape with color.
      /// </summary>
      /// <param name="_layer">
      ///   layer contained shape
      /// </param>
      /// <param name="_shpid">
      ///   shape to be marked
      /// </param>
      /// <param name="_part">
      ///   shape part to be marked, -1 if entire shape
      /// </param>
      /// <param name="_color">
      ///   color used to mark shape
      /// </param>
      /// <param name="_mode">
      ///   True - mark, False - unmark
      /// </param>
      /// <param name="_update">
      ///   if True redraw scene after mark, do not redraw if False
      /// </param>
      procedure markSelShape           ( const _layer : TGIS_Layer ;
                                         const _shpid : Integer    ;
                                         const _part  : Integer    ;
                                         const _color : TGIS_Color ;
                                         const _mode  : Boolean    ;
                                         const _update: Boolean
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Set initial extent.
      /// </summary>
      procedure setExtent              ;

      /// <summary>
      ///   Set initial view extent.
      /// </summary>
      /// <param name="_extent">
      ///   current visible extent
      /// </param>
      /// <param name="_xval">
      ///   X of screen center in normalized coordinates
      /// </param>
      /// <param name="_yval">
      ///   Y of screen center in normalized coordinates
      /// </param>
      /// <param name="_dx">
      ///   X range in normalized coordinates
      /// </param>
      procedure setViewExtent          ( const _extent : TGIS_Extent ;
                                         var _xval     : Double ;
                                         var _yval     : Double ;
                                         var _dx       : Double
                                       ) ;

      /// <summary>
      ///   Get zUnit value as normalize factor for individual layer. All Z
      ///   values in layer will be normalized to this zUnit .
      /// </summary>
      /// <param name="_value">
      ///   coordinate system
      /// </param>
      /// <returns>
      ///   Z normalization value.
      /// </returns>
      function  getZUnit               ( const _value : TGIS_CSCoordinateSystem
                                       ) : Double ;

      /// <summary>
      ///   Set zUnit value as normalized factor. All Z values will be
      ///   normalized to zUnit .
      /// </summary>
      procedure setZUnit               ;

      /// <summary>
      ///   Set all layers params/existence.
      /// </summary>
      /// <param name="_mode">
      ///   if True only Active layer is analyzed
      /// </param>
      procedure layersRangeSetting     ( const _mode : Boolean
                                       ) ;

      /// <summary>
      ///   Check LayerPixel params/existence.
      /// </summary>
      /// <param name="_la">
      ///   LayerAbstract
      /// </param>
      /// <param name="_lx">
      ///   LayerPixel
      /// </param>
      /// <param name="_mode">
      ///   if True only Active layer is analyzed
      /// </param>
      procedure layerPixelParams       ( const _la   : TGIS_Layer      ;
                                         const _lx   : TGIS_LayerPixel ;
                                         const _mode : Boolean
                                       ) ;

      /// <summary>
      ///   Check LayerPixel as Mesh params/existence.
      /// </summary>
      /// <param name="_la">
      ///   LayerAbstract
      /// </param>
      /// <param name="_lx">
      ///   LayerPixel
      /// </param>
      /// <param name="_mode">
      ///   if True only Active layer is analyzed
      /// </param>
      procedure layerMeshParams        ( const _la   : TGIS_Layer      ;
                                         const _lx   : TGIS_LayerPixel ;
                                         const _mode : Boolean
                                       ) ;

      /// <summary>
      ///   Check LayerProject params/existence.
      /// </summary>
      /// <param name="_la">
      ///   LayerAbstract
      /// </param>
      /// <param name="_lp">
      ///   LayerProject
      /// </param>
      /// <param name="_mode">
      ///   if True only Active layer is analyzed
      /// </param>
      procedure layerProjectParams     ( const _la   : TGIS_Layer        ;
                                         const _lp   : TGIS_LayerProject ;
                                         const _mode : Boolean
                                       ) ;

      /// <summary>
      ///   Check LayerCompound params/existence.
      /// </summary>
      /// <param name="_la">
      ///   LayerAbstract
      /// </param>
      /// <param name="_lc">
      ///   LayerCompound
      /// </param>
      /// <param name="_mode">
      ///   if True only Active layer is analyzed
      /// </param>
      procedure layerCompoundParams    ( const _la   : TGIS_Layer ;
                                         const _lc   : TGIS_LayerCompoundAbstract ;
                                         const _mode : Boolean
                                       ) ;

      /// <summary>
      ///   Check LayerVector params/existence.
      /// </summary>
      /// <param name="_la">
      ///   LayerAbstract
      /// </param>
      /// <param name="_mode">
      ///   if True only Active layer is analyzed
      /// </param>
      procedure layerVectorParams      ( const _la   : TGIS_Layer ;
                                         const _mode : Boolean
                                       ) ;

      /// <summary>
      ///   Set DEM transparency value , 0- transparent, 100- opaque.
      /// </summary>
      /// <param name="_value">
      ///   DEM transparency setting (range 0..100)
      /// </param>
      procedure setDemTransparency1    ( const _value : Integer
                                       ) ;

      /// <summary>
      ///   Restore 3D environment on window resize.
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  restore3DEnvironment   : Integer ; virtual; abstract;

      /// <summary>
      ///   Free all object created in instance. To be used before raise an
      ///   EGIS_Exception.
      /// </summary>
      procedure cleanUp                ;

      /// <summary>
      ///   Switch Light On.
      /// </summary>
      procedure lightOn                ; virtual; abstract;

      /// <summary>
      ///   Switch Light Off.
      /// </summary>
      procedure lightOff               ; virtual; abstract;

      /// <summary>
      ///   Check status of all layers in legend.
      /// </summary>
      procedure updateLayerExistance   ;

      /// <summary>
      ///   Update Camera parameters for current scene.
      /// </summary>
      procedure updateCameraParams     ;

      /// <summary>
      ///   Set grid size for DEM &amp; IMG rendering.
      /// </summary>
      procedure updateFrameSizes       ;

      /// <summary>
      ///   Prepare current data segments (DEM/texture/IMG) for rendering.
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  setD3D_Data            : Integer ; virtual; abstract;

      /// <summary>
      ///   Set initial reference level on DEM if exists, if no DEM, then at
      ///   highest point among existing vector layers.
      /// </summary>
      procedure setInitialLevel        ; virtual;

      /// <summary>
      ///   Calculate initial level from vector layers.
      /// </summary>
      procedure calcInitialLevel       ;

      /// <summary>
      ///   CallBack procedure to check Z range.
      /// </summary>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <param name="_abort">
      ///   True if processing failed
      /// </param>
      procedure checkInitialLevel      (       _shp   : TGIS_Shape ;
                                         var   _abort : Boolean
                                       ) ;
      /// <summary>
      ///   Get layer z m range based on FalseZ, FalseM.
      /// </summary>
      procedure getLayerZMRange ;

      /// <summary>
      ///   CallBack procedure to check ZM range.
      /// </summary>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <param name="_abort">
      ///   True if processing failed
      /// </param>
      procedure getShapeZMRange         (      _shp   : TGIS_Shape ;
                                         var   _abort : Boolean
                                       ) ;

      /// <summary>
      ///   Draw BasePlane grid.
      /// </summary>
      procedure drawBasement           ; virtual; abstract;

      /// <summary>
      ///   Draw SkyBox and Sun.
      /// </summary>
      procedure drawSkyBox             ; virtual; abstract;

      /// <summary>
      ///   Get current extent for vector display.
      /// </summary>
      /// <returns>
      ///   Required extent.
      /// </returns>
      function  getVectorExtent        : TGIS_Extent ;

      /// <summary>
      ///   Calculates extent for rendering (in map units).
      /// </summary>
      /// <returns>
      ///   Required extent.
      /// </returns>
      function  retGisExtent           : TGIS_Extent ;

      /// <summary>
      ///   Sets rendering parameters.
      /// </summary>
      /// <param name="_detail">
      ///   detail/rough rendering switch
      /// </param>
      procedure setNormExtent          ( const _detail   : Boolean
                                       ) ;

      /// <summary>
      ///   Calculates Z range in current scene (used by GetVisibleExtent3D).
      /// </summary>
      procedure setRangeExtent         ;

      /// <summary>
      ///   Subtracts two vectors.
      /// </summary>
      /// <param name="_out">
      ///   result of vector subtraction
      /// </param>
      /// <param name="_in1">
      ///   first vector
      /// </param>
      /// <param name="_in2">
      ///   second vector
      /// </param>
      /// <returns>
      ///   Result of vector subtraction.
      /// </returns>
      function  vectorSubtract         ( var   _out : TVector3f ;
                                         const _in1 : TVector3f ;
                                         const _in2 : TVector3f
                                       ) : TVector3f ;

      /// <summary>
      ///   Dot product of two vectors.
      /// </summary>
      /// <param name="_in1">
      ///   first vector
      /// </param>
      /// <param name="_in2">
      ///   second vector
      /// </param>
      /// <returns>
      ///   Dot product value.
      /// </returns>
      function  vectorDot              ( const _in1 : TVector3f ;
                                         const _in2 : TVector3f
                                       ) : Single ;

      /// <summary>
      ///   Cross product of two vectors.
      /// </summary>
      /// <param name="_out">
      ///   Cross product vector
      /// </param>
      /// <param name="_in1">
      ///   first vector
      /// </param>
      /// <param name="_in2">
      ///   second vector
      /// </param>
      /// <returns>
      ///   Cross product vector.
      /// </returns>
      function  vectorCross            ( var   _out : TVector3f ;
                                         const _in1 : TVector3f ;
                                         const _in2 : TVector3f
                                       ) : TVector3f ;


      /// <summary>
      ///   Intersects triangle and line.
      /// </summary>
      /// <param name="_v0">
      ///   coordinates of first triangle point
      /// </param>
      /// <param name="_v1">
      ///   coordinates of second triangle point
      /// </param>
      /// <param name="_v2">
      ///   coordinates of third triangle point
      /// </param>
      /// <param name="_orig">
      ///   line origin point
      /// </param>
      /// <param name="_dir">
      ///   line direction
      /// </param>
      /// <param name="_u">
      ///   first barycentric coordinate
      /// </param>
      /// <param name="_v">
      ///   second barycentric coordinate
      /// </param>
      /// <param name="_d">
      ///   distance between line origin and intersection point
      /// </param>
      /// <returns>
      ///   True it line intersects triangle.
      /// </returns>
      function  intersectTriRay        ( const _v0, _v1, _v2 : TVector3f ;
                                         const _orig, _dir   : TVector3f ;
                                         out _u, _v, _d : Single
                                       ) : Boolean ;

      /// <summary>
      ///   Calculate intersection between line and a plane.
      /// </summary>
      /// <param name="_p1">
      ///   line first point coordinates
      /// </param>
      /// <param name="_p2">
      ///   line second point coordinates
      /// </param>
      /// <param name="_pa">
      ///   first point on plane coordinates
      /// </param>
      /// <param name="_pb">
      ///   second point on plane coordinates
      /// </param>
      /// <param name="_pc">
      ///   third point on plane coordinates
      /// </param>
      /// <param name="_p">
      ///   intersection point coordinates
      /// </param>
      /// <param name="_t">
      ///   distance between _p1 and _p
      /// </param>
      /// <returns>
      ///   True it line intersects plane.
      /// </returns>
      function  intersectLinePlane     ( const _p1, _p2      : TVector3f ;
                                         const _pa, _pb, _pc : TVector3f ;
                                         out   _p            : TVector3f ;
                                         out   _t            : Double
                                       ) : Boolean ;

      /// <summary>
      ///   Normalize 3D vector.
      /// </summary>
      /// <param name="_in">
      ///   vector to be normalized
      /// </param>
      /// <param name="_out">
      ///   normalized vector
      /// </param>
      procedure normalizeVector        ( var _in  : TVector3f ;
                                         var _out : TVector3f
                                       ) ;

      /// <summary>
      ///   Screen units to 3D units converter.
      /// </summary>
      /// <param name="_pt">
      ///   screen coordinates
      /// </param>
      /// <returns>
      ///   Screen coordinates converted to 3D units.
      /// </returns>
      function  screenTo3D             ( const _pt : TPoint
                                       ) : TGIS_Point ; virtual; abstract;

      /// <summary>
      ///   Get screen coordinates of map point at Z level.
      /// </summary>
      /// <param name="_ptg">
      ///   point position (map units)
      /// </param>
      /// <param name="_z">
      ///   Z value in 3D units
      /// </param>
      /// <returns>
      ///   Map coordinates converted to 3D units.
      /// </returns>
      function  mapToScreen2           ( const _ptg : TGIS_Point ;
                                         const _z   : Double
                                       ) : TPoint ; virtual; abstract;

      /// <summary>
      ///   Check if detail window exists.
      /// </summary>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  checkExtent            : Boolean ;

      /// <summary>
      ///   Calculate room for detail rendering.
      /// </summary>
      procedure setEmptyWindow         ;

      /// <summary>
      ///   Update external triangle textures to avoid invisible Dem walls.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer data
      /// </param>
      /// <param name="_len">
      ///   vertex buffer length
      /// </param>
      procedure updateDemTexture       ( var   _pardata : T_arTextureData ;
                                                _len    : Integer
                                       ) ;

      /// <summary>
      ///   Fill DEM vertex buffer with the specified row data.
      /// </summary>
      /// <param name="_row">
      ///   required row
      /// </param>
      /// <param name="_mode">
      ///   rough/detail rendering switch
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  fillVertexBuffer       ( const _row  : Integer ;
                                         const _mode : Boolean
                                       ) : Integer ;

      /// <summary>
      ///   Get grid point coordinates.
      /// </summary>
      /// <param name="_col">
      ///   grid column
      /// </param>
      /// <param name="_row">
      ///   grid row
      /// </param>
      /// <returns>
      ///   Grid point coordinates.
      /// </returns>
      function  getGrdP                ( const _col : Integer ;
                                         const _row : Integer
                                       ) : TVector3f ;

      /// <summary>
      ///   Calculate normal vector to a grid point.
      /// </summary>
      /// <param name="_col">
      ///   grid column
      /// </param>
      /// <param name="_row">
      ///   gridrow
      /// </param>
      /// <returns>
      ///   Grid point normal vector.
      /// </returns>
      function  setGrdN                ( const _col : Integer ;
                                         const _row : Integer
                                       ) : TVector3f ;

      /// <summary>
      ///   Get shape point coordinates.
      /// </summary>
      /// <param name="_i">
      ///   position in vertex buffer
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <returns>
      ///   Shape point coordinates.
      /// </returns>
      function  getShpP                ( const _i       : Integer ;
                                         const _pardata : T_arColorData
                                       ) : TVector3f ;

      /// <summary>
      ///   Calculate normal vectors to polygon wall in position _k.
      /// </summary>
      /// <param name="_k">
      ///   position in vertex buffer
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      procedure setShpN                ( const _k     : Integer ;
                                         var _pardata : T_arColorData
                                       ) ;

      /// <summary>
      ///   Calculate normal vector to a triangle.
      /// </summary>
      /// <param name="_v0">
      ///   coordinate ot the first point of triangle
      /// </param>
      /// <param name="_v1">
      ///   coordinate ot the second point of triangle
      /// </param>
      /// <param name="_v2">
      ///   coordinate ot the third point of triangle
      /// </param>
      /// <returns>
      ///   Vector normal to triangle.
      /// </returns>
      function  triangleNormal         ( const _v0 : TVector3f ;
                                         const _v1 : TVector3f ;
                                         const _v2 : TVector3f
                                       ) : TVector3f ; virtual; abstract;

      /// <summary>
      ///   Adds two 3D vectors.
      /// </summary>
      /// <param name="_out">
      ///   result of operation
      /// </param>
      /// <param name="_in1">
      ///   first vctor
      /// </param>
      /// <param name="_in2">
      ///   second vector
      /// </param>
      /// <returns>
      ///   Result of adding operation.
      /// </returns>
      function  vectorAdd              ( var   _out : TVector3f ;
                                         const _in1 : TVector3f ;
                                         const _in2 : TVector3f
                                       ) : TVector3f ;

      /// <summary>
      ///   Set texture for point _k of vertex buffer.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer (single row)
      /// </param>
      /// <param name="_k">
      ///   current vertex buffer point number
      /// </param>
      /// <param name="_col">
      ///   column number of DEM grid
      /// </param>
      /// <param name="_row">
      ///   row number of DEM grid
      /// </param>
      /// <param name="_z">
      ///   Z value in current vertex buffer point
      /// </param>
      procedure textureValue           ( var   _pardata : T_arTextureData ;
                                         const _k       : Integer         ;
                                         const _col     : Integer         ;
                                         const _row     : Integer         ;
                                         const _z       : Double
                                       ) ;

      /// <summary>
      ///   Set texture for detail rendering.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer (single row)
      /// </param>
      /// <param name="_k">
      ///   current vertex buffer point number
      /// </param>
      /// <param name="_kk">
      ///   column number of DEM grid
      /// </param>
      /// <param name="_jj">
      ///   row number of DEM grid
      /// </param>
      procedure detailTexture          ( var   _pardata : T_arTextureData ;
                                         const _k       : Integer         ;
                                         const _kk      : Integer         ;
                                         const _jj      : Integer
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Calculate 32bits color.
      /// </summary>
      /// <param name="_a">
      ///   alpha value
      /// </param>
      /// <param name="_r">
      ///   red value
      /// </param>
      /// <param name="_g">
      ///   green value
      /// </param>
      /// <param name="_b">
      ///   blue value
      /// </param>
      /// <returns>
      ///   32bits color.
      /// </returns>
      function  color2ARGB             ( const _a : DWORD ;
                                         const _r : DWORD ;
                                         const _g : DWORD ;
                                         const _b : DWORD
                                       ) : DWORD ;

      /// <summary>
      ///   Calculate 24bits color.
      /// </summary>
      /// <param name="_r">
      ///   red value
      /// </param>
      /// <param name="_g">
      ///   green value
      /// </param>
      /// <param name="_b">
      ///   blue value
      /// </param>
      /// <returns>
      ///   24bits color.
      /// </returns>
      function  color2RGB              ( const _r : DWORD ;
                                         const _g : DWORD ;
                                         const _b : DWORD
                                       ) : DWORD ;

      /// <summary>
      ///   Control DEM display.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_len">
      ///   vertex buffer length
      /// </param>
      /// <param name="_check">
      ///   if True, then check if vertical wall exists
      /// </param>
      procedure setDemTexture          ( var   _pardata : T_arTextureData ;
                                         const _len     : Integer         ;
                                         const _check   : Boolean
                                       ) ;

      /// <summary>
      ///   Control left DEM wall existence/display.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      procedure makeLeftWall           ( var   _pardata : T_arTextureData
                                       ) ;

      /// <summary>
      ///   Control right DEM wall existence/display.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_len">
      ///   vertex buffer length
      /// </param>
      procedure makeRightWall          ( var   _pardata : T_arTextureData ;
                                         const _len   : Integer
                                       ) ;

      /// <summary>
      ///   Control upper DEM wall existence/display.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_len">
      ///   vertex buffer length
      /// </param>
      procedure makeUpperWall          ( var   _pardata : T_arTextureData ;
                                         const _len     : Integer
                                       ) ;

      /// <summary>
      ///   Control bottom DEM wall existence/display.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_len">
      ///   vertex buffer length
      /// </param>
      procedure makeBottomWall         ( var   _pardata : T_arTextureData ;
                                         const _len     : Integer
                                       ) ;

      /// <summary>
      ///   Draw single line of vertex buffer when ignored values occur.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_len">
      ///   vertex buffer length
      /// </param>
      procedure drawDemWalls           ( var _pardata : T_arTextureData ;
                                         const _len   : Integer
                                       ) ;

      /// <summary>
      ///   Create &amp; Add single mesh tile for DEM (from triangleStrip).
      /// </summary>
      /// <param name="_pardata">
      ///   data to be stored in mesh
      /// </param>
      /// <param name="_first">
      ///   position of first vertex to be stored
      /// </param>
      /// <param name="_tri">
      ///   number of triangles to be stored as faces
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  addDemMeshTile         ( var   _pardata : T_arTextureData   ;
                                         const _first   : Integer           ;
                                         const _tri     : Integer
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Create &amp; Add single mesh tile for DEM wall (from triangleList).
      /// </summary>
      /// <param name="_tri">
      ///   number of triangles to be stored as faces
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  addWallMeshTile        ( const _tri     : Integer
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Add textured triangle to triangle list buffer (DEM walls).
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
      procedure addToWTLBufT           ( const _ptg1 : TGIS_Renderer3DVertex ;
                                         const _ptg2 : TGIS_Renderer3DVertex ;
                                         const _ptg3 : TGIS_Renderer3DVertex
                                       ) ; virtual;

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
      procedure addToTinBufT           ( const _ptg1 : TGIS_Renderer3DVertex ;
                                         const _ptg2 : TGIS_Renderer3DVertex ;
                                         const _ptg3 : TGIS_Renderer3DVertex
                                       ) ;

      /// <summary>
      ///   Add color triangle to triangle list buffer.
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
      procedure addToTinBufC           ( const _ptg1 : TGIS_Renderer3DVertex ;
                                         const _ptg2 : TGIS_Renderer3DVertex ;
                                         const _ptg3 : TGIS_Renderer3DVertex
                                       ) ;

      /// <summary>
      ///   Add color triangle to triangle list buffer.
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
      procedure addToMPatchBufC        ( const _ptg1 : TGIS_Renderer3DVertex ;
                                         const _ptg2 : TGIS_Renderer3DVertex ;
                                         const _ptg3 : TGIS_Renderer3DVertex
                                       ) ;

      /// <summary>
      ///   Add textured triangle to a triangle list buffer (for vector layer).
      /// </summary>
      /// <param name="_part">
      ///   part of vector (True means roof, False means wall )
      /// </param>
      /// <param name="_buf">
      ///   triangle list buffer
      /// </param>
      /// <param name="_ptg1">
      ///   first triangle point
      /// </param>
      /// <param name="_ptg2">
      ///   second triangle point
      /// </param>
      /// <param name="_ptg3">
      ///   third triangle point
      /// </param>
      procedure addToVTLBufT           ( const _part : Boolean              ;
                                         var   _buf  : TGIS_Renderer3DTextureTriangleListBuffer;
                                         const _ptg1 : TGIS_Renderer3DVertex      ;
                                         const _ptg2 : TGIS_Renderer3DVertex      ;
                                         const _ptg3 : TGIS_Renderer3DVertex
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Save and draw textured triangle list from buffer (DEM wall).
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  saveWTLBufT            : Integer ; virtual; abstract;

      /// <summary>
      ///   Save and draw textured triangle list from buffer (TIN).
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  saveTinBufT            : Integer ; virtual;

      /// <summary>
      ///   Save and draw color triangle list from buffer (TIN).
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  saveTinBufC            : Integer ;

      /// <summary>
      ///   Save and draw color triangle list from buffer (MultiPatch).
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  saveMPatchBufC         : Integer ; virtual; abstract;

      /// <summary>
      ///   Create &amp; Add single color mesh tile for TIN.
      /// </summary>
      /// <param name="_tri">
      ///   number of triangles to be stored as faces
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  addTinMeshTileC        ( const _tri     : Integer
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Create &amp; Add single textured mesh tile for TIN.
      /// </summary>
      /// <param name="_tri">
      ///   number of triangles to be stored as faces
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  addTinMeshTileT        ( const _tri     : Integer
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Make vertical DEM wall segment.
      /// </summary>
      /// <param name="_ptg1">
      ///   first point on DEM
      /// </param>
      /// <param name="_ptg2">
      ///   second point on DEM
      /// </param>
      /// <param name="_side">
      ///   side of extent box (0 left, 1 right, 2 top, 3 bottom)
      /// </param>
      procedure makeVerticalWall       ( var   _ptg1 : TGIS_Renderer3DVertex ;
                                         var   _ptg2 : TGIS_Renderer3DVertex ;
                                         const _side : Integer
                                       ) ;

      /// <summary>
      ///   Get texture value in vertical direction (Z).
      /// </summary>
      /// <param name="_z">
      ///   value of Z at current DEM point
      /// </param>
      /// <returns>
      ///   Required texture value.
      /// </returns>
      function  getTvValue             ( const _z : Double
                                       ) : Single ;

      /// <summary>
      ///   Set proper DEM triangle texture to be displayed.
      /// </summary>
      /// <param name="_p1">
      ///   first DEM triangle point
      /// </param>
      /// <param name="_p2">
      ///   second DEM triangle point
      /// </param>
      /// <param name="_p3">
      ///   third DEM triangle point
      /// </param>
      procedure changeWallTexture      ( var _p1 : TGIS_Renderer3DVertex ;
                                         var _p2 : TGIS_Renderer3DVertex ;
                                         var _p3 : TGIS_Renderer3DVertex
                                       ) ;

      /// <summary>
      ///   Calculate DEM wall segment normals.
      /// </summary>
      /// <param name="_ptg1">
      ///   first point of DEM wall triangle
      /// </param>
      /// <param name="_ptg2">
      ///   second point of DEM wall triangle
      /// </param>
      /// <param name="_ptg3">
      ///   third point of DEM wall triangle
      /// </param>
      procedure setDemWallNormals      ( var _ptg1 : TGIS_Renderer3DVertex ;
                                         var _ptg2 : TGIS_Renderer3DVertex ;
                                         var _ptg3 : TGIS_Renderer3DVertex
                                       ) ;

      /// <summary>
      ///   Shape drawing procedure.
      /// </summary>
      /// <param name="_buftype">
      ///   type of buffer storing data
      /// </param>
      /// <param name="_shpid">
      ///   Uid of shape
      /// </param>
      /// <param name="_part">
      ///   shape part number
      /// </param>
      /// <param name="_cl">
      ///   shape color
      /// </param>
      procedure fillBufferInfoEntry    ( const _buftype : ShortInt ;
                                         const _shpid   : TGIS_Uid  ;
                                         const _part    : Integer;
                                         const _cl      : TGIS_Color
                                       ) ;

      /// <summary>
      ///  Extract color from shape
      /// </summary>
      function extractColorFromShape   ( const _buftype : ShortInt
                                       ) : TGIS_Color;

      /// <summary>
      ///   Get size of required DEM tile.
      /// </summary>
      /// <param name="_row">
      ///   DEM tile number
      /// </param>
      /// <returns>
      ///   Number of vertices in DEM tile.
      /// </returns>
      function  getDemBufSize          ( const _row : Integer
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Get size of required DEM wall tile.
      /// </summary>
      /// <param name="_row">
      ///   DEM wall tile number
      /// </param>
      /// <returns>
      ///   Number of vertices in DEM wall tile.
      /// </returns>
      function  getWallBufSize         ( const _row : Integer
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Check if data refresh is necessary.
      /// </summary>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  checkReRender          : Boolean ;

      /// <summary>
      ///   Immediately display 3D scene.
      /// </summary>
      procedure blitToWindow           ; virtual; abstract;

      /// <summary>
      ///   Clear 3D viewport and prepare to display next scene.
      /// </summary>
      procedure clearScreen            ; virtual; abstract;

      /// <summary>
      ///   Render all vector layers.
      /// </summary>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  draw3DVector           : Integer ; virtual; abstract;

      /// <summary>
      ///   Render all pixel layers (DEM, images).
      /// </summary>
      /// <param name="_rows">
      ///   number of DEM/Image rows to be rendered
      /// </param>
      /// <param name="_detail">
      ///   render mode True - detail, False - rough
      /// </param>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  doRender               ( const _rows     : DWORD ;
                                         const _detail   : Boolean
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Display origin pointer/flood level on scene.
      /// </summary>
      /// <param name="_value">
      ///   True - origin pointer is displayed, False - flood
      /// </param>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  northArrow             ( const _value : Boolean
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Draw labels from buffer.
      /// </summary>
      procedure drawLabels             ; virtual; abstract;

      /// <summary>
      ///   Switch On AlphaBlend (Platform compatibility purpose).
      /// </summary>
      procedure blendOn                ; virtual; abstract;

      /// <summary>
      ///   Switch Off AlphaBlend (Platform compatibility purpose).
      /// </summary>
      procedure blendOff               ; virtual; abstract;

      /// <summary>
      ///   TIN call back procedure, draw one triangle of TIN.
      /// </summary>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <param name="_abort">
      ///   True if processing failed
      /// </param>
      procedure doCallBackTin          (       _shp   : TGIS_Shape        ;
                                         var   _abort : Boolean
                                       ) ;

      /// <summary>
      ///   Check if current vector layer is TIN.
      /// </summary>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  isTinLayer             : Boolean ;

      /// <summary>
      ///   Force adding new row to a arVectPolyRoof array.
      /// </summary>
      procedure addNewPolyRoofRow      ;

      /// <summary>
      ///   Check if shape is visible.
      /// </summary>
      /// <param name="_ext">
      ///   shape extent
      /// </param>
      /// <param name="_size">
      ///   size of shape in pixels
      /// </param>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  isShpVisible           ( const _ext   : TGIS_Extent3D ;
                                         var   _size  : Integer
                                       ) : Boolean ;

      /// <summary>
      ///   Simplify 3Dshapes on scene.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be simplified
      /// </param>
      /// <returns>
      ///   Simplified shape.
      /// </returns>
      function  doSimplify             ( const _shp  : TGIS_Shape
                                       ) : TGIS_Shape ;

      /// <summary>
      ///   Simplify 3Dshapes on scene using shp.Simplify method.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be simplified
      /// </param>
      /// <returns>
      ///   Simplified shape.
      /// </returns>
      function  doSimplify1            ( const _shp  : TGIS_Shape
                                       ) : TGIS_Shape ;

      /// <summary>
      ///   Simplify 3Dshapes on scene using distance criterion.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be simplified
      /// </param>
      /// <returns>
      ///   Simplified shape.
      /// </returns>
      function  doSimplify2            ( const _shp  : TGIS_Shape
                                       ) : TGIS_Shape ;

      /// <summary>
      ///   Check if total number of shape points exceeds VertexBuffer limit.
      /// </summary>
      /// <param name="_shp">
      ///   shape which size is to be checked
      /// </param>
      /// <returns>
      ///   True if exceeds.
      /// </returns>
      function  checkGeometrySize      ( const _shp : TGIS_Shape
                                       ) : Boolean ;

      /// <summary>
      ///   Shape drawing procedure.
      /// </summary>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <param name="_abort">
      ///   True if processing failed
      /// </param>
      procedure drawShapeFromCache     ( const _shp : TObject ;
                                         var _abort : Boolean
                                       ) ;

      /// <summary>
      ///   Draw polygon from cache.
      /// </summary>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  drawPolyFromCache      ( const _shp  : TObject
                                       ) : Boolean ;

      /// <summary>
      ///   Draw arc from cache.
      /// </summary>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  drawArcFromCache       ( const _shp  : TObject
                                       ) : Boolean ;

      /// <summary>
      ///   Draw point from cache.
      /// </summary>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <returns>
      ///   True on success.
      /// </returns>
      function  drawPointFromCache     ( const _shp  : TObject
                                       ) : Boolean ;

      /// <summary>
      ///   Draw line as volumetric feature.
      /// </summary>
      /// <param name="_part">
      ///   shape part number
      /// </param>
      /// <param name="_num">
      ///   number of data in buffer
      /// </param>
      /// <param name="_pardata">
      ///   buffer containing data
      /// </param>
      /// <param name="_color">
      ///   volumetric line color
      /// </param>
      /// <param name="_size">
      ///   line width
      /// </param>
      procedure drawVolumetricLine     ( const _part    : Integer       ;
                                         const _num     : Integer       ;
                                         const _pardata : T_arColorData ;
                                         const _color   : Cardinal      ;
                                         const _size    : Integer
                                       ) ;

      /// <summary>
      ///   Prepare volumetric 3DMarker to display.
      /// </summary>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_sym">
      ///   marker symbol
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_color">
      ///   marker area color
      /// </param>
      /// <returns>
      ///   Height of generated marker.
      /// </returns>
      function  drawVolumetricPoint    ( const _siz      : Integer          ;
                                         const _sym      : TGIS_MarkerStyle ;
                                         var   _pardata  : T_arColorData    ;
                                         const _px       : Double           ;
                                         const _color    : DWORD
                                       ) : Single ;

      /// <summary>
      ///   Adjust shape basement.
      /// </summary>
      /// <param name="_mode">
      ///   0 CW else CCW vertex order
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_cbuffer">
      ///   additional vertex buffer
      /// </param>
      /// <param name="_j">
      ///   first point
      /// </param>
      /// <param name="_k">
      ///   last point
      /// </param>
      /// <param name="_val">
      ///   Z level
      /// </param>
      /// <param name="_val1">
      ///   Z level
      /// </param>
      procedure adjustBasement         ( const _mode    : Integer           ;
                                         var   _pardata : T_arColorData     ;
                                         var   _cbuffer : T_arColorData     ;
                                         const _j       : Integer           ;
                                         const _k       : Integer           ;
                                         const _val     : Double            ;
                                         const _val1    : Double
                                       ) ;

      /// <summary>
      ///   Load data for triangulation process.
      /// </summary>
      /// <param name="_part">
      ///   shape part number
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_k">
      ///   part points number
      /// </param>
      /// <param name="_dir">
      ///   loading order (clockwise / counterclockwise)
      /// </param>
      procedure loadMesh               ( const _part    : Integer       ;
                                         const _pardata : T_arColorData ;
                                         const _k       : Integer       ;
                                         const _dir     : Boolean
                                       ) ;

      /// <summary>
      ///   Convert triangle Strip to a List buffer (Color).
      /// </summary>
      /// <param name="_part">
      ///   True - roof, False - wall
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_tri">
      ///   number of triangles
      /// </param>
      procedure triStripToListC        ( const _part    : Boolean           ;
                                         const _pardata : T_arColorData     ;
                                         const _tri     : Integer
                                       ) ;

      /// <summary>
      ///   Convert triangle List to a List buffer (Color).
      /// </summary>
      /// <param name="_part">
      ///   True - roof, False - wall
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_tri">
      ///   number of triangles
      /// </param>
      procedure triListToListC         ( const _part    : Boolean           ;
                                         const _pardata : T_arColorData     ;
                                         const _tri     : Integer
                                       ) ;

      /// <summary>
      ///   Convert triangle Strip to a List buffer (Texture).
      /// </summary>
      /// <param name="_part">
      ///   True - roof, False - wall
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_tri">
      ///   number of triangles
      /// </param>
      procedure triStripToListT        ( const _part    : Boolean           ;
                                         const _pardata : T_arTextureData   ;
                                         const _tri     : Integer
                                       ) ;

      /// <summary>
      ///   Convert triangle List to a List buffer (Texture).
      /// </summary>
      /// <param name="_part">
      ///   True - roof, False - wall
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_tri">
      ///   number of triangles
      /// </param>
      procedure triListToListT         ( const _part    : Boolean           ;
                                         const _pardata : T_arTextureData   ;
                                         const _tri     : Integer
                                       ) ;

      /// <summary>
      ///   Load LineStrip to a WPLBuf List buffer.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_lin">
      ///   number of triangles
      /// </param>
      procedure linStripToList         ( const _pardata : T_arColorData     ;
                                         const _lin     : Integer
                                       ) ;

      /// <summary>
      ///   Load horizontal LineStrip to a WPLBuf List buffer.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_lin">
      ///   number of triangles
      /// </param>
      procedure linStripToList1        ( const _pardata : T_arColorData     ;
                                         const _lin     : Integer
                                       ) ;

      /// <summary>
      ///   Load LineList to a WPLBuf List buffer.
      /// </summary>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_lin">
      ///   number of triangles
      /// </param>
      procedure linListToList          ( const _pardata : T_arColorData     ;
                                         const _lin     : Integer
                                       ) ;

      /// <summary>
      ///   Get extent in D3D units.
      /// </summary>
      /// <param name="_data">
      ///   vertex buffer data
      /// </param>
      /// <param name="_num">
      ///   vertex buffer length
      /// </param>
      /// <returns>
      ///   Shape extent in 3D units.
      /// </returns>
      function  shpExt3D               ( const _data : T_arColorData ;
                                         const _num  : Integer
                                       ) : TGIS_Extent ;

      /// <summary>
      ///   Get texture value for polygon roof.
      /// </summary>
      /// <param name="_ext">
      ///   extent in D3D units
      /// </param>
      /// <param name="_pnt">
      ///   point coordinates in D3D units
      /// </param>
      /// <returns>
      ///   TGIS_Point containing Tu and Tv texture coordinates.
      /// </returns>
      function  vectTexVal             ( const _ext : TGIS_Extent ;
                                         const _pnt : TGIS_Point
                                       ) : TGIS_Point ;

      /// <summary>
      ///   Get vector Z range in 3D units.
      /// </summary>
      /// <param name="_data">
      ///   vertex buffer data
      /// </param>
      /// <param name="_num">
      ///   vertex buffer length
      /// </param>
      /// <returns>
      ///   Z range of given data segment.
      /// </returns>
      function  zRange                 ( const _data : T_arColorData ;
                                         const _num  : Integer
                                       ) : TGIS_Point ;

      /// <summary>
      ///   Draw vector with texture.
      /// </summary>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <param name="_part">
      ///   True - roof, False - wall
      /// </param>
      /// <param name="_data">
      ///   vertex buffer content
      /// </param>
      /// <param name="_prim">
      ///   primitive type ( 1 TRIANGLESTRIP, 0 TRIANGLELIST )
      /// </param>
      /// <param name="_num">
      ///   part points number
      /// </param>
      /// <param name="_len">
      ///   vector length in D3D units
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  drawVectorTexture      ( const _shp  : TObject           ;
                                         const _part : Boolean           ;
                                         const _data : T_arColorData     ;
                                         const _prim : Integer           ;
                                         const _num  : Integer           ;
                                         const _len  : Double
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Draw vector edges.
      /// </summary>
      /// <param name="_num_pnts">
      ///   number of points in buffer
      /// </param>
      /// <param name="_data">
      ///   vertex buffer
      /// </param>
      procedure drawVectorEdges        ( const _num_pnts : Integer ;
                                         var _data     : T_arColorData
                                       ) ;

      /// <summary>
      ///   Draw polygon roof
      /// </summary>
      /// <param name="_ar_color">
      ///   area color
      /// </param>
      /// <param name="_mltp">
      ///   multiplication factor
      /// </param>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <param name="_winding">
      ///   vertex order True - CW, False - CCW
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  drawPolygonRoof        ( const _ar_color : DWORD         ;
                                         const _mltp     : Double        ;
                                         const _shp      : TObject       ;
                                         const _winding  : Boolean
                                       ) : Boolean ;

      /// <summary>
      ///   Draw polyline roof.
      /// </summary>
      /// <param name="_cbuf_siz">
      ///   buffer size
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_cbuffer">
      ///   additional vertex buffer
      /// </param>
      /// <param name="_ar_color">
      ///   shape area color
      /// </param>
      /// <param name="_isarc">
      ///   True if source shape if Arc
      /// </param>
      /// <param name="_shp_type">
      ///   shape type
      /// </param>
      /// <param name="_shp">
      ///   shape itself
      /// </param>
      procedure drawPolyLineRoof       ( const _cbuf_siz : Integer        ;
                                         var   _pardata  : T_arColorData  ;
                                         var   _cbuffer  : T_arColorData  ;
                                         const _ar_color : DWORD          ;
                                         var   _isarc    : Boolean        ;
                                         var   _shp_type : TGIS_ShapeType ;
                                         const _shp      : TObject
                                       ) ;

      /// <summary>
      ///   Store info about M coordinate for individual roof triangle.
      /// </summary>
      /// <param name="_count">
      ///   buffer number
      /// </param>
      /// <param name="_buf">
      ///   buffer data are to be added to
      /// </param>
      /// <param name="_m1">
      ///   M for first vertex
      /// </param>
      /// <param name="_m2">
      ///   M for second vertex
      /// </param>
      /// <param name="_m3">
      ///   M for third vertex
      /// </param>
      procedure addToVectPolyRoof      ( var   _count   : Integer           ;
                                         var   _buf     : TGIS_Renderer3DPolyRoofLevel  ;
                                         const _m1      : Single            ;
                                         const _m2      : Single            ;
                                         const _m3      : Single
                                       ) ;

      /// <summary>
      ///   Add color triangle to a triangle list buffer (for vector layer).
      /// </summary>
      /// <param name="_part">
      ///   True means Roof, False means Wall
      /// </param>
      /// <param name="_buf">
      ///   buffer to store data
      /// </param>
      /// <param name="_ptg1">
      ///   first triangle point
      /// </param>
      /// <param name="_ptg2">
      ///   second triangle point
      /// </param>
      /// <param name="_ptg3">
      ///   third triangle point
      /// </param>
      procedure addToVTLBufC           ( const _part : Boolean              ;
                                         var   _buf  : TGIS_Renderer3DColorTriangleListBuffer ;
                                         const _ptg1 : TGIS_Renderer3DVertex        ;
                                         const _ptg2 : TGIS_Renderer3DVertex        ;
                                         const _ptg3 : TGIS_Renderer3DVertex
                                       ) ;
      /// <summary>
      ///   Save color triangle list.
      /// </summary>
      /// <param name="_part">
      ///   True means Roof, False means Wall
      /// </param>
      /// <param name="_buf">
      ///   buffer storing triangles to be moved to mesh
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  saveVTLBufC            ( const _part : Boolean       ;
                                         var   _buf  : TGIS_Renderer3DColorTriangleListBuffer
                                       ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Add color line to point list buffer.
      /// </summary>
      /// <param name="_ptg1">
      ///   first point
      /// </param>
      /// <param name="_ptg2">
      ///   second point
      /// </param>
      procedure addToWPLBuf            ( const _ptg1 : TGIS_Renderer3DVertex ;
                                         const _ptg2 : TGIS_Renderer3DVertex
                                       ) ;

      /// <summary>
      ///   Save color line buffer.
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  saveWPLBuf             : Integer ;

      /// <summary>
      ///   Save color point buffer.
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  saveWPBuf              : Integer ;

      /// <summary>
      ///   Add color point to point buffer.
      /// </summary>
      /// <param name="_ptg1">
      ///   point
      /// </param>
      procedure addToWPBuf             ( const _ptg1 : TGIS_Renderer3DVertex
                                       ) ;

      /// <summary>
      ///   Create &amp; Add single mesh tile for line list drawing.
      /// </summary>
      /// <param name="_lin">
      ///   number of lines to be stored
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  addLineMeshTile        ( const _lin     : Integer
                                       ) : Integer ;

      /// <summary>
      ///   Create &amp; Add single mesh tile for point list drawing.
      /// </summary>
      /// <param name="_num">
      ///   number of points to be stored
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  addPointMeshTile       ( const _num     : Integer
                                       ) : Integer ;

      /// <summary>
      ///   Set normal to a triangle of polygon interior.
      /// </summary>
      /// <param name="_ver1">
      ///   first point of triangle
      /// </param>
      /// <param name="_ver2">
      ///   second point of triangle
      /// </param>
      /// <param name="_ver3">
      ///   third point of triangle
      /// </param>
      /// <param name="_wind">
      ///   triangle vertex winding True - CW, Flase CCW
      /// </param>
      procedure setPolyRoofN           ( var   _ver1 : TGIS_Renderer3DVertex ;
                                         var   _ver2 : TGIS_Renderer3DVertex ;
                                         var   _ver3 : TGIS_Renderer3DVertex ;
                                         const _wind : Boolean
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Prepare 3DMarker to display.
      /// </summary>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_sym">
      ///   marker symbol
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_po_color">
      ///   marker outline color
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  make3dMarker           ( const _siz      : Integer          ;
                                         const _sym      : TGIS_MarkerStyle ;
                                         var   _pardata  : T_arColorData    ;
                                         const _px       : Double           ;
                                         const _po_color : DWORD
                                       ) : Integer ;

      /// <summary>
      ///   Prepare 3DMarkerBox to display.
      /// </summary>
      /// <param name="_x">
      ///   marker X coordinate
      /// </param>
      /// <param name="_y">
      ///   marker Y coordinate
      /// </param>
      /// <param name="_z1">
      ///   marker bottom Z value
      /// </param>
      /// <param name="_z2">
      ///   marker upper Z value
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_po_color">
      ///   marker outline color
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  makeBox                ( const _x        : Double    ;
                                         const _y        : Double    ;
                                         const _z1       : Double    ;
                                         const _z2       : Double    ;
                                         const _px       : Double    ;
                                         const _siz      : Double    ;
                                         const _po_color : DWORD     ;
                                         var   _pardata  : T_arColorData
                                       ) : Integer ;

      /// <summary>
      ///   Prepare 3DMarkerTriangleUp to display.
      /// </summary>
      /// <param name="_x">
      ///   marker X coordinate
      /// </param>
      /// <param name="_y">
      ///   marker Y coordinate
      /// </param>
      /// <param name="_z1">
      ///   marker bottom Z value
      /// </param>
      /// <param name="_z2">
      ///   marker upper Z value
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_po_color">
      ///   marker outline color
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  makeTUp                ( const _x        : Double    ;
                                         const _y        : Double    ;
                                         const _z1       : Double    ;
                                         const _z2       : Double    ;
                                         const _px       : Double    ;
                                         const _siz      : Double    ;
                                         const _po_color : DWORD     ;
                                         var   _pardata  : T_arColorData
                                       ) : Integer ;

      /// <summary>
      ///   Prepare 3DMarkerTriangleDown to display.
      /// </summary>
      /// <param name="_x">
      ///   marker X coordinate
      /// </param>
      /// <param name="_y">
      ///   marker Y coordinate
      /// </param>
      /// <param name="_z1">
      ///   marker bottom Z value
      /// </param>
      /// <param name="_z2">
      ///   marker upper Z value
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_po_color">
      ///   marker outline color
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  makeTDw                ( const _x        : Double    ;
                                         const _y        : Double    ;
                                         const _z1       : Double    ;
                                         const _z2       : Double    ;
                                         const _px       : Double    ;
                                         const _siz      : Double    ;
                                         const _po_color : DWORD     ;
                                         var   _pardata  : T_arColorData
                                       ) : Integer ;

      /// <summary>
      ///   Prepare 3DMarkerTriangleLeft to display.
      /// </summary>
      /// <param name="_x">
      ///   marker X coordinate
      /// </param>
      /// <param name="_y">
      ///   marker Y coordinate
      /// </param>
      /// <param name="_z1">
      ///   marker bottom Z value
      /// </param>
      /// <param name="_z2">
      ///   marker upper Z value
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_po_color">
      ///   marker outline color
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  makeTLf                ( const _x        : Double    ;
                                         const _y        : Double    ;
                                         const _z1       : Double    ;
                                         const _z2       : Double    ;
                                         const _px       : Double    ;
                                         const _siz      : Double    ;
                                         const _po_color : DWORD     ;
                                         var   _pardata  : T_arColorData
                                       ) : Integer ;

      /// <summary>
      ///   prepare 3DMarkerTriangleRight to display.
      /// </summary>
      /// <param name="_x">
      ///   marker X coordinate
      /// </param>
      /// <param name="_y">
      ///   marker Y coordinate
      /// </param>
      /// <param name="_z1">
      ///   marker bottom Z value
      /// </param>
      /// <param name="_z2">
      ///   marker upper Z value
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_po_color">
      ///   marker outline color
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  makeTRg                ( const _x        : Double    ;
                                         const _y        : Double    ;
                                         const _z1       : Double    ;
                                         const _z2       : Double    ;
                                         const _px       : Double    ;
                                         const _siz      : Double    ;
                                         const _po_color : DWORD     ;
                                         var   _pardata  : T_arColorData
                                       ) : Integer ;

      /// <summary>
      ///   Prepare 3DMarkerCross to display.
      /// </summary>
      /// <param name="_x">
      ///   marker X coordinate
      /// </param>
      /// <param name="_y">
      ///   marker Y coordinate
      /// </param>
      /// <param name="_z1">
      ///   marker bottom Z value
      /// </param>
      /// <param name="_z2">
      ///   marker upper Z value
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_po_color">
      ///   marker outline color
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  makeCrs                ( const _x        : Double    ;
                                         const _y        : Double    ;
                                         const _z1       : Double    ;
                                         const _z2       : Double    ;
                                         const _px       : Double    ;
                                         const _siz      : Double    ;
                                         const _po_color : DWORD     ;
                                         var   _pardata  : T_arColorData
                                       ) : Integer ;

      /// <summary>
      ///   Prepare 3DMarkerDiagonalCross to display.
      /// </summary>
      /// <param name="_x">
      ///   marker X coordinate
      /// </param>
      /// <param name="_y">
      ///   marker Y coordinate
      /// </param>
      /// <param name="_z1">
      ///   marker bottom Z value
      /// </param>
      /// <param name="_z2">
      ///   marker upper Z value
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_po_color">
      ///   marker outline color
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  makeDcr                ( const _x        : Double    ;
                                         const _y        : Double    ;
                                         const _z1       : Double    ;
                                         const _z2       : Double    ;
                                         const _px       : Double    ;
                                         const _siz      : Double    ;
                                         const _po_color : DWORD     ;
                                         var   _pardata  : T_arColorData
                                       ) : Integer ;

      /// <summary>
      ///   Prepare 3DMarkerCircle to display.
      /// </summary>
      /// <param name="_x">
      ///   marker X coordinate
      /// </param>
      /// <param name="_y">
      ///   marker Y coordinate
      /// </param>
      /// <param name="_z1">
      ///   marker bottom Z value
      /// </param>
      /// <param name="_z2">
      ///   marker upper Z value
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      /// <param name="_siz">
      ///   marker size
      /// </param>
      /// <param name="_po_color">
      ///   marker outline color
      /// </param>
      /// <param name="_pardata">
      ///   vertex buffer
      /// </param>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  makeCir                ( const _x        : Double    ;
                                         const _y        : Double    ;
                                         const _z1       : Double    ;
                                         const _z2       : Double    ;
                                         const _px       : Double    ;
                                         const _siz      : Double    ;
                                         const _po_color : DWORD     ;
                                         var   _pardata  : T_arColorData
                                       ) : Integer ;

      /// <summary>
      ///   Calculate points for 3Dline drawing.
      /// </summary>
      /// <param name="_x1">
      ///   X coordinate of point 1
      /// </param>
      /// <param name="_y1">
      ///   Y coordinate of point 1
      /// </param>
      /// <param name="_x2">
      ///   X coordinate of point 2
      /// </param>
      /// <param name="_y2">
      ///   Y coordinate of point 2
      /// </param>
      /// <param name="_x3">
      ///   X coordinate of point 3
      /// </param>
      /// <param name="_y3">
      ///   Y coordinate of point 3
      /// </param>
      /// <param name="_dis">
      ///   line thickness
      /// </param>
      /// <param name="_xl">
      ///   X coordinate of point left to point 2
      /// </param>
      /// <param name="_yl">
      ///   Y coordinate of point left to point 2
      /// </param>
      /// <param name="_xr">
      ///   X coordinate of point right to point 2
      /// </param>
      /// <param name="_yr">
      ///   Y coordinate of point right to point 2
      /// </param>
      procedure lineBufPnts            ( const _x1  : Double ;
                                         const _y1  : Double ;
                                         const _x2  : Double ;
                                         const _y2  : Double ;
                                         const _x3  : Double ;
                                         const _y3  : Double ;
                                         const _dis : Double ;
                                         var   _xl  : Double ;
                                         var   _yl  : Double ;
                                         var   _xr  : Double ;
                                         var   _yr  : Double
                                       ) ;

      /// <summary>
      ///   Fill vertex buffer with 3Dline.
      /// </summary>
      /// <param name="_size">
      ///   line width
      /// </param>
      /// <param name="_k">
      ///   input vertex buffer length
      /// </param>
      /// <param name="_cbuffer">
      ///   output vertex buffer
      /// </param>
      /// <param name="_pardata">
      ///   input vertex buffer
      /// </param>
      /// <param name="_px">
      ///   distance visible in current 3D scene
      /// </param>
      procedure make3dLine             ( const _size  : Integer       ;
                                         const _k     : Integer       ;
                                         var _cbuffer : T_arColorData ;
                                         var _pardata : T_arColorData ;
                                         const _px    : Double
                                       ) ;

      /// <summary>
      ///   Get point position after rotation &amp; translation.
      /// </summary>
      /// <param name="_pnt">
      ///   point position to be processed
      /// </param>
      /// <param name="_rx">
      ///   rotation along X axis
      /// </param>
      /// <param name="_ry">
      ///   rotation along Y axis
      /// </param>
      /// <param name="_rz">
      ///   rotation along Z axis
      /// </param>
      /// <param name="_vec">
      ///   translation vector
      /// </param>
      /// <returns>
      ///   Point position after operations.
      /// </returns>
      function  getNewPntPosition      ( const _pnt : TGIS_Point3D ;
                                         const _rx  : Double ;
                                         const _ry  : Double ;
                                         const _rz  : Double ;
                                         const _vec : TGIS_Point3D
                                        ) : TGIS_Point3D ;

      /// <summary>
      ///   Returns current window size.
      /// </summary>
      /// <returns>
      ///   current window size (height,width).
      /// </returns>
      function  getWindowSize          : TPoint ; virtual; abstract;

      /// <summary>
      ///   Check if current window size has changed.
      /// </summary>
      /// <returns>
      ///   True if window size has changed.
      /// </returns>
      function  checkWindowSize        : Boolean ;

      /// <summary>
      ///   Set new 3D environment after window size changed.
      /// </summary>
      procedure reset3DEnvironment     ; virtual; abstract;

      /// <summary>
      ///   Overrides GetTickCount.
      /// </summary>
      /// <returns>
      ///   Current time value.
      /// </returns>
      function  getTimeTic             : DWORD ; virtual; abstract;

      /// <summary>
      ///   Get texture coordinates for required color.
      /// </summary>
      /// <param name="_cl">
      ///   required color
      /// </param>
      /// <param name="_t1">
      ///   texture TU value
      /// </param>
      /// <param name="_t2">
      ///   texture TV value
      /// </param>
      procedure colorToTexture         ( const _cl : Cardinal ;
                                         var   _t1 : Single ;
                                         var   _t2 : Single
                                       ) ;

      {$IFDEF CLR}
        /// <summary>
        ///   Unlock texture vertex buffer.
        /// </summary>
        /// <param name="_strm">
        ///   pointer to a stream
        /// </param>
        /// <param name="_data">
        ///   vertex buffer
        /// </param>
        /// <returns>
        ///   0 on success.
        /// </returns>
        function pdataVB_Unlock        ( var _strm : DirectXStream   ;
                                         var _data : T_arTextureData
                                       ) : Integer ; virtual; abstract;

        /// <summary>
        ///   Unlock color vertex buffer.
        /// </summary>
        /// <param name="_strm">
        ///   pointer to a stream
        /// </param>
        /// <param name="_data">
        ///   vertex buffer
        /// </param>
        /// <returns>
        ///   0 on success.
        /// </returns>
        function cdataVB_Unlock        ( var _strm : DirectXStream   ;
                                         var _data : T_arColorData
                                       ) : Integer ; virtual; abstract;
    {$ELSE}
        /// <summary>
        ///   Unlock texture vertex buffer.
        /// </summary>
        /// <param name="_strm">
        ///   pointer to a stream
        /// </param>
        /// <param name="_data">
        ///   vertex buffer
        /// </param>
        /// <returns>
        ///   0 on success.
        /// </returns>
        function pdataVB_Unlock        ( var _strm : PByte        ;
                                         var _data : T_arTextureData
                                       ) : Integer ; virtual; abstract;

        /// <summary>
        ///   Unlock color vertex buffer.
        /// </summary>
        /// <param name="_strm">
        ///   pointer to a stream
        /// </param>
        /// <param name="_data">
        ///   vertex buffer
        /// </param>
        /// <returns>
        ///   0 on success.
        /// </returns>
        function cdataVB_Unlock        ( var _strm : PByte        ;
                                         var _data : T_arColorData
                                       ) : Integer ; virtual; abstract;
    {$ENDIF}

    {$IFDEF CLR}
        /// <summary>
        ///   Lock texture vertex buffer.
        /// </summary>
        /// <param name="_strm">
        ///   pointer to a stream
        /// </param>
        /// <returns>
        ///   0 on success.
        /// </returns>
        function pdataVB_Lock          ( var _strm : DirectXStream
                                       ) : Integer ; virtual; abstract;

        /// <summary>
        ///   Lock color vertex buffer.
        /// </summary>
        /// <param name="_strm">
        ///   pointer to a stream
        /// </param>
        /// <returns>
        ///   0 on success.
        /// </returns>
        function cdataVB_Lock          ( var _strm : DirectXStream
                                       ) : Integer ; virtual; abstract;
    {$ELSE}
        /// <summary>
        ///   Lock texture vertex buffer.
        /// </summary>
        /// <param name="_strm">
        ///   pointer to a stream
        /// </param>
        /// <returns>
        ///   0 on success.
        /// </returns>
        function pdataVB_Lock          ( var _strm : PByte
                                       ) : Integer ; virtual; abstract;

        /// <summary>
        ///   Lock color vertex buffer.
        /// </summary>
        /// <param name="_strm">
        ///   pointer to a stream
        /// </param>
        /// <returns>
        ///   0 on success.
        /// </returns>
        function cdataVB_Lock          ( var _strm : PByte
                                       ) : Integer ; virtual; abstract;

    {$ENDIF}

    /// <summary>
    ///   Return pixel size for current printer PPI (in 3D units).
    /// </summary>
    /// <param name="_radius">
    ///   current distance from camera to scene center (in 3D units)
    /// </param>
    /// <returns>
    ///   Pixel size for current printer PPI
    /// </returns>
    function getPrinterPixelSize       ( const _radius : Double
                                       ) : Double ;

    /// <summary>
    ///   Set defaultZ value based on BasePlane.Level.
    /// </summary>
    procedure setDefaultZ              ;

    /// <summary>
    ///   Set Level Of Detail.
    /// </summary>
    procedure setLod                   ;


    public //  constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create               ; virtual;

    published //events
      {$IFDEF CLR}
        event    UpdateEvent           : EventHandler
                                         delegate FOnUpdate ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Update event. Will be fired for Update operation.
        /// </summary>
        property UpdateEvent           : TNotifyEvent
                                         read  FOnUpdate
                                         write FOnUpdate ;
      {$ENDIF}

    public //  public functions

      /// <summary>
      ///   Perform detailed rendering for Update.
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  ReRender               : Integer ;


      /// <summary>
      ///   Set 2d/3d viewers.
      /// </summary>
      /// <param name="_viewer2d">
      ///   2D viewer
      /// </param>
      /// <param name="_viewer3d">
      ///   3D viewer
      /// </param>
      procedure SetViewer              ( const _viewer2d : IGIS_Viewer ;
                                         const _viewer3d : IGIS_Viewer3D
                                       ) ;
      /// <summary>
      ///   Initialize 3D environment.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_3D_FAIL
      /// </exception>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  Init3D                 : Boolean ; virtual; abstract;

      /// <summary>
      ///   Set initial values of all variables.
      /// </summary>
      /// <param name="_cutextent">
      ///   all objects beyond this extent will not be visible
      /// </param>
      /// <param name="_visibleextent">
      ///   visible extent of graphic window
      /// </param>
      /// <param name="_gis">
      ///   reference viewer
      /// </param>
      /// <param name="_viewer">
      ///   3D viewer
      /// </param>
      procedure SetInitValues          ( const _cutextent     : TGIS_Extent ;
                                         const _visibleextent : TGIS_Extent ;
                                         const _gis           : IGIS_Viewer ;
                                         const _viewer        : IGIS_Viewer3D
                                       ) ; virtual;

      /// <summary>
      ///   Draw current vertex buffers content.
      /// </summary>
      /// <returns>
      ///   0 on success.
      /// </returns>
      function  Draw                   : Integer ; virtual; abstract;

      /// <summary>
      ///   Viewer current navigation type info (False - normal, True -
      ///   advanced).
      /// </summary>
      /// <param name="_value">
      ///   mode in which viewer currently works
      /// </param>
      procedure SetNavigationType      ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Viewer current mode info.
      /// </summary>
      /// <param name="_value">
      ///   mode in which viewer currently works
      /// </param>
      procedure SetNavigationMode      ( const _value : TGIS_Viewer3DMode
                                       ) ;

      /// <summary>
      ///   Current viewer orthogonal view mode (False - off, True - On).
      /// </summary>
      /// <param name="_value">
      ///   orthogonal view mode status
      /// </param>
      procedure SetOrthoView           ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Get Z coordinate multiplication value.
      /// </summary>
      /// <returns>
      ///   Current Z coordinate multiplication value.
      /// </returns>
      function  GetScaleZ              : Double ;

      /// <summary>
      ///   Set Z coordinate multiplication value.
      /// </summary>
      /// <param name="_value">
      ///   multiplication value
      /// </param>
      procedure SetScaleZ              ( const _value : Double
                                       ) ;

      /// <summary>
      ///   Get M coordinate multiplication value.
      /// </summary>
      /// <returns>
      ///   Current M coordinate multiplication value.
      /// </returns>
      function  GetScaleM              : Double;

      /// <summary>
      ///   Set M coordinate multiplication value.
      /// </summary>
      /// <param name="_value">
      ///   multiplication value
      /// </param>
      procedure SetScaleM              ( const _value : Double
                                       ) ;

      /// <summary>
      ///   Get 3D scale, same as TGIS_Viewer.Scale.
      /// </summary>
      /// <returns>
      ///   Current scale value in 3D.
      /// </returns>
      function  GetScale3D             : Double ;

      /// <summary>
      ///   Set 3D scale, same as TGIS_Viewer.Scale.
      /// </summary>
      /// <param name="_value">
      ///   scale value
      /// </param>
      procedure SetScale3D             ( const _value         : Double
                                       ) ;

      /// <summary>
      ///   Get 3D scale in format 1:10000, same as TGIS_Viewer.ScaleAsText.
      /// </summary>
      /// <returns>
      ///   String containing 3D scale.
      /// </returns>
      function  GetScale3DAsText       : String ;

      /// <summary>
      ///   Set 3D scale in format 1:10000, same as TGIS_Viewer.ScaleAsText.
      /// </summary>
      /// <param name="_value">
      ///   scale value
      /// </param>
      procedure SetScale3DAsText       ( const _value         : String
                                       ) ;

      /// <summary>
      ///   Horizontal size of pixel.
      /// </summary>
      /// <returns>
      ///   Pixel size along X and Y axis.
      /// </returns>
      function  GetPixelSize           : TGIS_Point ;

      /// <summary>
      ///   Return wireframe mode.
      /// </summary>
      /// <returns>
      ///   True if wireframe mode is on.
      /// </returns>
      function  GetWireframe           : Boolean ;

      /// <summary>
      ///   Set wireframe mode.
      /// </summary>
      /// <param name="_value">
      ///   True if wireframe mode is active, False if not
      /// </param>
      procedure SetWireframe           ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Return lights mode.
      /// </summary>
      /// <returns>
      ///   True if lights are on.
      /// </returns>
      function  GetLights              : Boolean ;

      /// <summary>
      ///   Set lights mode.
      /// </summary>
      /// <param name="_value">
      ///   True if lights mode is active, False if not
      /// </param>
      procedure SetLights              ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Return labels mode.
      /// </summary>
      /// <returns>
      ///   True if labels are on.
      /// </returns>
      function  GetLabels              : Boolean ;

      /// <summary>
      ///   Set labels mode.
      /// </summary>
      /// <param name="_value">
      ///   True : labels are drawn, False not drawn
      /// </param>
      procedure SetLabels              ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Return labelsEx mode.
      /// </summary>
      /// <returns>
      ///   True if labelsEx mode is active.
      /// </returns>
      function  GetLabelsEx            : Boolean ;

      /// <summary>
      ///   Set labelsEx mode.
      /// </summary>
      /// <param name="_value">
      ///   True : labels are not drawn during scene rotation/drag/zoom.
      /// </param>
      procedure SetLabelsEx            ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Get vector edges drawing mode, True edges are drawn, False not.
      /// </summary>
      /// <returns>
      ///   True if edges are drawn.
      /// </returns>
      function  GetVectorEdges         : Boolean ;

      /// <summary>
      ///   Set vector edges drawing mode, True edges are drawn, False not.
      /// </summary>
      /// <param name="_value">
      ///   required vector edges drawing mode
      /// </param>
      procedure SetVectorEdges         ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Get draw transparent faces drawing mode, True faces are drawn, False not.
      /// </summary>
      /// <returns>
      ///   True if transparent faces are drawn.
      /// </returns>
      function GetDrawTransparent      : Boolean ;

      /// <summary>
      ///   Set draw transparent faces drawing mode, True faces are drawn, False not.
      /// </summary>
      /// <param name="_value">
      ///   required transparent faces drawing mode
      /// </param>
      procedure SetDrawTransparent     ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Get 3D vector edges color.
      /// </summary>
      /// <returns>
      ///   Current edges drawing color.
      /// </returns>
      function  GetEdgesColor          : TGIS_Color ;

      /// <summary>
      ///   Set 3D vector edges color.
      /// </summary>
      /// <param name="_value">
      ///   edge color
      /// </param>
      procedure SetEdgesColor          ( const _value : TGIS_Color
                                       ) ;

      /// <summary>
      ///   Get OriginPointer drawing mode, True pointer is drawn, False not.
      /// </summary>
      /// <returns>
      ///   True if origin pointer is drawn.
      /// </returns>
      function  GetOriginPointer       : Boolean ;

      /// <summary>
      ///   Set OriginPointer drawing mode, True pointer is drawn, False not.
      /// </summary>
      /// <param name="_value">
      ///   required OriginPointer drawing mode
      /// </param>
      procedure SetOriginPointer       ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Get OriginPoint location.
      /// </summary>
      /// <returns>
      ///   Origin point 3D coordinates.
      /// </returns>
      function  GetOriginPoint         : TGIS_Point3D ;

      /// <summary>
      ///   Set OriginPoint location.
      /// </summary>
      /// <param name="_value">
      ///   required Origin point 3D coordinates.
      /// </param>
      procedure SetOriginPoint         ( const _value : TGIS_Point3D
                                       ) ;

      /// <summary>
      ///   Get reference mode currently in use. See TGIS_Viewer3DReferenceMode.
      ///   for more information.
      /// </summary>
      /// <returns>
      ///   Current reference mode.
      /// </returns>
      function  GetReferencePointMode  : TGIS_Viewer3DReferenceMode ;

      /// <summary>
      ///   Set reference mode to be used See TGIS_Viewer3DReferenceMode for
      ///   more information.
      /// </summary>
      /// <param name="_value">
      ///   required reference mode
      /// </param>
      procedure SetReferencePointMode  ( const _value :
                                           TGIS_Viewer3DReferenceMode
                                       ) ;

      /// <summary>
      ///   Get value added to reference point Z coordinate.
      /// </summary>
      /// <returns>
      ///   Current reference offset.
      /// </returns>
      function  GetReferencePointOffset: Double ;

      /// <summary>
      ///   Set value to be added to reference point Z coordinate.
      /// </summary>
      /// <param name="_value">
      ///   offset value
      /// </param>
      procedure SetReferencePointOffset( const _value : Double ) ;

      /// <summary>
      ///   Get camera position.
      /// </summary>
      /// <returns>
      ///   Current camera position as elevation, azimuth and distance.
      /// </returns>
      /// <remarks>
      ///   Camera position is specified as:
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Term</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>X</term>
      ///       <description>elevation angle of the camera above horizon in
      ///         radians (0..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>Y</term>
      ///       <description>azimuth to the camera from natural north in
      ///         radians (0..2Pi)</description>
      ///     </item>
      ///     <item>
      ///       <term>Z</term>
      ///       <description>distance to the camera in map units</description>
      ///     </item>
      ///     <item>
      ///       <term>M</term>
      ///       <description>for future use</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      function  GetCameraPosition      : TGIS_Point3D ;

      /// <summary>
      ///   Set camera position.
      /// </summary>
      /// <param name="_value">
      ///   camera position as elevation, azimuth and distance.
      /// </param>
      /// <remarks>
      ///   Camera position is specified as:
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Term</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>X</term>
      ///       <description>elevation angle of the camera above horizon in
      ///         radians (0..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>Y</term>
      ///       <description>azimuth to the camera from natural north in
      ///         radians (0..2Pi)</description>
      ///     </item>
      ///     <item>
      ///       <term>Z</term>
      ///       <description>distance to the camera in map units</description>
      ///     </item>
      ///     <item>
      ///       <term>M</term>
      ///       <description>for future use</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      procedure SetCameraPosition      ( const _value : TGIS_Point3D
                                       ) ;

      /// <summary>
      ///   Get camera position coordinates.
      /// </summary>
      /// <returns>
      ///   Current camera position in map units.
      /// </returns>
      /// <remarks>
      ///   Camera position is specified as:
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Term</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>X</term>
      ///       <description>X coordinate in map units</description>
      ///     </item>
      ///     <item>
      ///       <term>Y</term>
      ///       <description>Y coordinate in map units</description>
      ///     </item>
      ///     <item>
      ///       <term>Z</term>
      ///       <description>Z coordinate in map units</description>
      ///     </item>
      ///     <item>
      ///       <term>M</term>
      ///       <description>for future use</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      function  GetCameraPositionEx    : TGIS_Point3D ;

      /// <summary>
      ///   Get camera position coordinates.
      /// </summary>
      /// <returns>
      ///   Current camera position in 3D units.
      /// </returns>
      /// <remarks>
      ///   Camera position is specified as:
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Term</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>X</term>
      ///       <description>X coordinate in 3D units</description>
      ///     </item>
      ///     <item>
      ///       <term>Y</term>
      ///       <description>Y coordinate in 3D units</description>
      ///     </item>
      ///     <item>
      ///       <term>Z</term>
      ///       <description>Z coordinate in 3D units</description>
      ///     </item>
      ///     <item>
      ///       <term>M</term>
      ///       <description>for future use</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      function  GetCameraPosition3DEx  : TGIS_Point3D ;

      /// <summary>
      ///   Set camera position coordinates in map units.
      /// </summary>
      /// <param name="_value">
      ///   camera position
      /// </param>
      /// <remarks>
      ///   Camera position is specified as:
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Term</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>X</term>
      ///       <description>X coordinate in map units</description>
      ///     </item>
      ///     <item>
      ///       <term>Y</term>
      ///       <description>Y coordinate in map units</description>
      ///     </item>
      ///     <item>
      ///       <term>Z</term>
      ///       <description>Z coordinate in map units</description>
      ///     </item>
      ///     <item>
      ///       <term>M</term>
      ///       <description>for future use</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      procedure SetCameraPositionEx    ( const _value : TGIS_Point3D
                                       ) ;

      /// <summary>
      ///   Get camera rotation.
      /// </summary>
      /// <returns>
      ///   Current camera rotation angles.
      /// </returns>
      /// <remarks>
      ///   Camera rotation angles are specified as:
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Term</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>X</term>
      ///       <description>elevation (vertical) angle of the camera
      ///         in radians (-Pi/2..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>Y</term>
      ///       <description>inclination angle angle of the camera
      ///         in radians (-Pi/2..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>Z</term>
      ///       <description>horizontal angle of the camera
      ///         in radians (0..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>M</term>
      ///       <description>camera focal length in mm, default 50</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      function  GetCameraRotation      : TGIS_Point3D ;

      /// <summary>
      ///   Set camera rotation.
      /// </summary>
      /// <param name="_value">
      ///   camera rotation
      /// </param>
      /// <remarks>
      ///   Camera rotation angles are specified as:
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Term</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>X</term>
      ///       <description>elevation (vertical) angle of the camera
      ///         in radians (-Pi/2..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>Y</term>
      ///       <description>inclination angle angle of the camera
      ///         in radians (-Pi/2..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>Z</term>
      ///       <description>horizontal angle of the camera
      ///         in radians (0..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>M</term>
      ///       <description>camera focal length in mm, default 50</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      procedure SetCameraRotation      ( const _value : TGIS_Point3D
                                       ) ;

      /// <summary>
      ///   Get internal camera rotation parameters.
      /// </summary>
      /// <returns>
      ///   Current camera rotation angles in DEG.
      /// </returns>
      function  GetCameraRotationEx    : TGIS_Point3D ;

      /// <summary>
      ///   Get sun (light) position.
      /// </summary>
      /// <returns>
      ///   Current sun (light) position.
      /// </returns>
      /// <remarks>
      ///   Sun (light) position is specified as:
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Term</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>X</term>
      ///       <description>elevation (vertical) angle of the sun above horizon
      ///         in radians (-Pi/2..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>Y</term>
      ///       <description>azimuth to the sun from natural north,
      ///         in radians (0..Pi/2)</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      function  GetSunPosition         : TGIS_Point ;

      /// <summary>
      ///   Set sun (light) position.
      /// </summary>
      /// <param name="_value">
      ///   sun position
      /// </param>
      /// <remarks>
      ///   Sun (light) position is specified as:
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Term</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>X</term>
      ///       <description>elevation (vertical) angle of the sun above horizon
      ///         in radians (-Pi/2..Pi/2)</description>
      ///     </item>
      ///     <item>
      ///       <term>Y</term>
      ///       <description>azimuth to the sun from natural north,
      ///         in radians (0..Pi/2)</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      procedure SetSunPosition         ( const _value : TGIS_Point
                                       ) ; virtual; abstract;


      /// <summary>
      ///   Get sun and camera position check value.
      /// </summary>
      function  GetKeepSunCamera       : Boolean ;

      /// <summary>
      ///   Set keeping sun and camera position check.
      /// </summary>
      procedure SetKeepSunCamera       ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Select object in 3D.
      /// </summary>
      /// <param name="_pt">
      ///   screen coordinate
      /// </param>
      /// <param name="_prec">
      ///   precision in pixels (must be &gt;= 0)
      /// </param>
      /// <param name="_layer">
      ///   input : layer to be searched, if nil all layers will be searched
      ///   output : layer contained located object, if nil no object located
      /// </param>
      /// <param name="_ptg">
      ///   hitting point
      /// </param>
      /// <param name="_shp">
      ///   located shape or nil
      /// </param>
      /// <param name="_part">
      ///   located shape part number, -1 if not
      /// </param>
      /// <param name="_mode">
      ///   True - object selection, False - screen to map
      /// </param>
      /// <returns>
      ///   True if object is selected.
      /// </returns>
      function  Select3DObject         ( const _pt    : TPoint             ;
                                         const _prec  : Integer            ;
                                         var   _layer : TGIS_LayerAbstract ;
                                         var   _ptg   : TGIS_Point3D       ;
                                         var   _shp   : TGIS_ShapeAbstract ;
                                         var   _part  : Integer            ;
                                         const _mode  : Boolean
                                       ) : Boolean ; virtual; abstract;

      /// <summary>
      ///   Get shadow level.
      /// </summary>
      /// <returns>
      ///   Current shadow level.
      /// </returns>
      /// <remarks>
      ///   Shadow level is specified as:
      ///   0- deep shadows, 100- light shadows
      /// </remarks>
      function  GetShadowsLevel        : Integer ;

      /// <summary>
      ///   Set shadow level.
      /// </summary>
      /// <param name="_value">
      ///   level to be set (range 0..100)
      /// </param>
      /// <remarks>
      ///   Shadow level is specified as:
      ///   0- deep shadows, 100- light shadows
      /// </remarks>
      procedure SetShadowsLevel        ( const _value : Integer
                                       ) ;

      /// <summary>
      ///   Get current DEM wall presentation type.
      /// </summary>
      /// <returns>
      ///   Current DEM wall presentation type.
      /// </returns>
      /// <remarks>
      ///   Presentation type is specified as:
      ///   textured, invisible, solid.
      /// </remarks>
      function  GetDemWallType         : TGIS_Viewer3DDemWall ;

      /// <summary>
      ///   Set DEM wall presentation type.
      /// </summary>
      /// <param name="_value">
      ///   type to be used
      /// </param>
      /// <remarks>
      ///   Presentation type is specified as:
      ///   textured, invisible, solid.
      /// </remarks>
      procedure SetDemWallType         ( const _value : TGIS_Viewer3DDemWall
                                       ) ;

      /// <summary>
      ///   Get background color.
      /// </summary>
      /// <returns>
      ///   Current background color (color of area which exceeds map extent).
      /// </returns>
      function  GetUniverseColor       : TGIS_Color ; virtual; abstract;

      /// <summary>
      ///   Set the background color.
      /// </summary>
      /// <param name="_value">
      ///   color to be set
      /// </param>
      procedure SetUniverseColor       ( const _value         : TGIS_Color
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Get DEM texture mode info.
      /// </summary>
      /// <returns>
      ///   True means, that DEM is textured by image, False by Z value.
      /// </returns>
      function  GetTexture             : Boolean ;

      /// <summary>
      ///   Set DEM texture mode.
      /// </summary>
      /// <param name="_value">
      ///   required texture mode
      /// </param>
      /// <remarks>
      ///   DEM texture mode is specified as:
      ///   True - texture from image, False - from Z value.
      /// </remarks>
      procedure SetTexture             ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Get gap used to create isolines.
      /// </summary>
      /// <returns>
      ///   Distance between islolines.
      /// </returns>
      function  GetIsolineGap          : Double ;

      /// <summary>
      ///   Set gap to be used to create isolines.
      /// </summary>
      /// <param name="_value">
      ///   gap value
      /// </param>
      procedure SetIsolineGap          ( const _value : Double
                                       ) ;

      /// <summary>
      ///   Get color used to create isolines.
      /// </summary>
      /// <returns>
      ///   Isoline drawing color.
      /// </returns>
      function  GetIsolineColor        : TGIS_Color ;

      /// <summary>
      ///   Set color to be used to create isolines.
      /// </summary>
      /// <param name="_value">
      ///   color
      /// </param>
      procedure SetIsolineColor        ( const _value : TGIS_Color
                                       ) ;

      /// <summary>
      ///   Set color to be used to draw the DEM solid wall.
      /// </summary>
      /// <param name="_value">
      ///   color to be set
      /// </param>
      procedure SetSolidWallColor      ( const _value : TGIS_Color
                                       ) ;

      /// <summary>
      ///   Get flood settings. See TGIS_Viewer3DFlood for more information.
      /// </summary>
      /// <returns>
      ///   Flood settings.
      /// </returns>
      function  GetFlood               : TGIS_Viewer3DFlood ;

      /// <summary>
      ///   Set flood settings. See TGIS_Viewer3DFlood for more information.
      /// </summary>
      /// <param name="_value">
      ///   flood params to be set
      /// </param>
      procedure SetFlood               ( const _value : TGIS_Viewer3DFlood
                                       ) ;

      /// <summary>
      ///   Get BasePlane settings. See TGIS_Viewer3DBasePlane for more
      ///   information.
      /// </summary>
      /// <returns>
      ///   Base plane settings.
      /// </returns>
      function  GetBasePlane           : TGIS_Viewer3DBasePlane ;

      /// <summary>
      ///   Set BasePlane settings. See TGIS_Viewer3DBasePlane for more
      ///   information.
      /// </summary>
      /// <param name="_value">
      ///   BasePlane params to be set
      /// </param>
      procedure SetBasePlane           ( const _value         :
                                           TGIS_Viewer3DBasePlane
                                       ) ;

      /// <summary>
      ///   Get 3D extent visible in 3D window.
      /// </summary>
      /// <returns>
      ///   Current 3D visible extent.
      /// </returns>
      function  GetVisibleExtent3D     : TGIS_Extent3D ;

      /// <summary>
      ///   Get 2D extent visible in 3D window.
      /// </summary>
      /// <returns>
      ///   Current 2D visible extent.
      /// </returns>
      function  GetVisibleExtent       : TGIS_Extent ;

      /// <summary>
      ///   Set 2D visible extent preserving camara peremeters.
      /// </summary>
      /// <param name="_value">
      ///   Required visible extent.
      /// </param>
      procedure SetVisibleExtent       ( const _value          :
                                           TGIS_Extent
                                       ) ;

      /// <summary>
      ///   Get current 3D view restriction.
      /// </summary>
      /// <returns>
      ///   Current view restriction setting.
      /// </returns>
      /// <remarks>
      ///   Camera restriction is specified as:
      ///   TGIS_Viewer3DViewRestriction.None - camera can be placed anywhere
      ///   TGIS_Viewer3DViewRestriction.AboveHorizon - camera can be placed only above horizon.
      /// </remarks>
      function  GetRestriction         : TGIS_Viewer3DViewRestriction ;

      /// <summary>
      ///   Set required 3D view restriction.
      /// </summary>
      /// <param name="_value">
      ///   required setting
      /// </param>
      /// <remarks>
      ///   Camera restriction is specified as:
      ///   TGIS_Viewer3DViewRestriction.None - camera can be placed anywhere
      ///   TGIS_Viewer3DViewRestriction.AboveHorizon - camera can be placed only above horizon.
      /// </remarks>
      procedure SetRestriction         ( const _value
                                           : TGIS_Viewer3DViewRestriction
                                       ) ;

      /// <summary>
      ///   Get DEM transparency.
      /// </summary>
      /// <returns>
      ///   True if DEM is transparent, Flase if not.
      /// </returns>
      function  GetDemTransparency     : Boolean ;

      /// <summary>
      ///   Set DEM transparency.
      /// </summary>
      /// <param name="_value">
      ///   DEM transparency setting
      /// </param>
      /// <remarks>
      ///   DEM transparency is specified as:
      ///   True - transparent DEM
      ///   False - not transparent DEM
      /// </remarks>
      procedure SetDemTransparency     ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Get transparency priority, order of DEM/vector rendering. See
      ///   TGIS3D_TransparencyPriority for more information.
      /// </summary>
      /// <returns>
      ///   Current TransparencyPriority setting.
      /// </returns>
      function  GetTranspPrior         : TGIS_Viewer3DTransparencyPriority ;

      /// <summary>
      ///   Set transparency priority, order of DEM/vector rendering. See
      ///   TGIS3D_TransparencyPriority for more information.
      /// </summary>
      /// <param name="_value">
      ///   transparency priority to be used (Auto, Vector, DEM)
      /// </param>
      procedure SetTranspPrior         ( const _value :
                                           TGIS_Viewer3DTransparencyPriority
                                       ) ;

      /// <summary>
      ///   Get error message String.
      /// </summary>
      /// <returns>
      ///   Error message if any, empty string if no error.
      /// </returns>
      function  GetErrorMsg            : String ;

      /// <summary>
      ///   Get IgnoreAbove value, values above will not be drawn.
      /// </summary>
      /// <returns>
      ///   Current IgnoreAbove setting.
      /// </returns>
      function  GetIgnoreAbove         : Double ;

      /// <summary>
      ///   Set IgnoreAbove value, values above will not be drawn.
      /// </summary>
      /// <param name="_value">
      ///   required IgnoreAbove value
      /// </param>
      procedure SetIgnoreAbove         ( const _value         : Double
                                       ) ;

      /// <summary>
      ///   Get IgnoreBelow value, values below will not be drawn.
      /// </summary>
      /// <returns>
      ///   Current IgnoreBelow setting.
      /// </returns>
      function  GetIgnoreBelow         : Double ;

      /// <summary>
      ///   Set IgnoreBelow value.
      /// </summary>
      /// <param name="_value">
      ///   required IgnoreBelow value
      /// </param>
      procedure SetIgnoreBelow         ( const _value         : Double
                                       ) ;

      /// <summary>
      ///   Get CutAbove value, values above will have this value.
      /// </summary>
      /// <returns>
      ///   Current CutAbove setting.
      /// </returns>
      function  GetCutAbove            : Double ;

      /// <summary>
      ///   Set CutAbove value, values abowe will have this value.
      /// </summary>
      /// <param name="_value">
      ///   required CutAbove value
      /// </param>
      procedure SetCutAbove            ( const _value         : Double
                                       ) ;

      /// <summary>
      ///   Get CutBelow value, values below will have this value.
      /// </summary>
      /// <returns>
      ///   Current CutBelow  setting.
      /// </returns>
      function  GetCutBelow            : Double ;

      /// <summary>
      ///   Set CutBelow value, values below will have this value.
      /// </summary>
      /// <param name="_value">
      ///   required CutBelow value
      /// </param>
      procedure SetCutBelow            ( const _value         : Double
                                       ) ;

      /// <summary>
      ///   Get FastMode value.
      /// </summary>
      /// <returns>
      ///   True if fast mode is active.
      /// </returns>
      function  GetFastMode            : Boolean ;

      /// <summary>
      ///   Set FastMode value.
      /// </summary>
      /// <param name="_value">
      ///   True to activate FastMode
      /// </param>
      procedure SetFastMode            ( const _value         : Boolean
                                       ) ;

      /// <summary>
      ///   Get Light normal vector setting.
      /// </summary>
      /// <returns>
      ///   True if normal is calculated from CW ordered points, False if CCW.
      /// </returns>
      function  GetLightVector         : Boolean ;

      /// <summary>
      ///   Set Light normal vector.
      /// </summary>
      /// <param name="_value">
      ///   required light normal vector.
      /// </param>
      /// <remarks>
      ///   True means that normal is calculated from CW ordered points, False CCW
      /// </remarks>
      procedure SetLightVector         ( const _value         : Boolean
                                       ) ;

      /// <summary>
      ///   Get IgnoreEllipsoidHeihgt value.
      /// </summary>
      /// <returns>
      ///   True means that EllipsoidHeihgt is ignored.
      /// </returns>
      function  GetIgnoreEllipsoidHeight : Boolean  ;

      /// <summary>
      ///   Set IgnoreEllipsoidHeihgt value
      /// </summary>
      /// <param name="_value">
      ///   True to Ignore Ellipsoid Height
      /// </param>
      procedure SetIgnoreEllipsoidHeight ( const _value       : Boolean
                                         ) ;

      /// <summary>
      ///   Drag scene left, right, forward or backward.
      /// </summary>
      /// <param name="_delta">
      ///   movement parameters
      /// </param>
      procedure Drag3D                 ( const _delta : TGIS_Point3D
                                       ) ;

      /// <summary>
      ///   Drag scene left, right, forward+up or backward+down.
      /// </summary>
      /// <param name="_delta">
      ///   movement parameters
      /// </param>
      procedure Drag3DEx               ( const _delta : TGIS_Point3D
                                       ) ;

      /// <summary>
      ///   Move scene along XY axis.
      /// </summary>
      /// <param name="_delta">
      ///   movement parameters
      /// </param>
      procedure Move3D                 ( const _delta : TGIS_Point3D
                                       ) ;

      /// <summary>
      ///   Rotate scene along Z axis.
      /// </summary>
      /// <param name="_delta">
      ///   movement parameters
      /// </param>
      procedure Rotate3D               ( const _delta : TGIS_Point3D
                                       ) ;

      /// <summary>
      ///   Block / UnBlock updates upon trigger went off.
      /// </summary>
      /// <param name="_value">
      ///   if set to True then auto update option is turn off
      /// </param>
      procedure LockTimer              ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Stop rendering and wait for Unlock.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Must be paired with Unlock.
      ///    </note>
      /// </remarks>
      procedure Lock                   ;

      /// <summary>
      ///   Cancels Lock, continue rendering.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Must be paired with Lock.
      ///    </note>
      /// </remarks>
      procedure Unlock                 ;

      /// <summary>
      ///   Screen to Map coordinate converter.
      /// </summary>                ;
      /// <param name="_pt">
      ///   screen pixel position
      /// </param>
      /// <returns>
      ///   Screen position in map units.
      /// </returns>
      function  ScreenToMap            ( const _pt    : TPoint
                                       ) : TGIS_Point ; virtual; abstract;

      /// <summary>
      ///   Get map coordinates of screen pixel.
      /// </summary>
      /// <param name="_pt">
      ///   screen pixel position
      /// </param>
      /// <returns>
      ///   Screen position in 3D units.
      /// </returns>
      function  ScreenToMap3D          ( const _pt    : TPoint
                                       ) : TGIS_Point3D ;

      /// <summary>
      ///   Get intersection point between a ray and a DEM
      /// </summary>
      /// <param name="_orig">
      ///   ray origin coordinates in map units
      /// </param>
      /// <param name="_dir">
      ///   ray direction
      /// </param>
      /// <param name="_ptg">
      ///   intersection point between a ray and a DEM if Result = True
      /// </param>
      /// <returns>
      ///   True if ray intersects DEM, False if not
      /// </returns>
      /// <remarks>
      ///   ScaleZ property must be set to 1.0
      ///   AdvNavigation must be set to False
      /// </remarks>
      function  RayIntersectDem       (  const _orig    : TGIS_Point3D ;
                                         const _dir     : TGIS_Point3D ;
                                         out   _ptg     : TGIS_Point3D
                                      ) : Boolean ; virtual; abstract;

      /// <summary>
      ///   Dump required screen rectangle to a bitmap.
      /// </summary>
      /// <param name="_rect">
      ///   required screen rectangle to be returned as TGIS_Bitmap
      /// </param>
      /// <param name="_bmp">
      ///   32 bits TBitmap returned as a result
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BAD_CALL
      /// </exception>
      procedure PrintBmp               ( const _rect  : TRect ;
                                         const _bmp   : TGIS_Bitmap
                                       ) ; virtual; abstract;

      /// <summary>
      ///   Get size of tile used while tiling.
      /// </summary>
      /// <param name="_width">
      ///   output bitmap width
      /// </param>
      /// <param name="_height">
      ///   output bitmap height
      /// </param>
      /// <returns>
      ///   Tile size to be used as a param in PrintTile method
      /// </returns>
      function  PrintBegin             ( const _width  : Integer ;
                                         const _height : Integer
                                       ) : TPoint ; virtual; abstract;

      /// <summary>
      ///   Dump visible screen rectangle to a bitmap
      /// </summary>
      /// <param name="_bmp">
      ///   32 bits TBitmap filled with tile data
      /// </param>
      /// <param name="_offsetx">
      ///   offset in x direction (in pixels)
      /// </param>
      /// <param name="_offsety">
      ///   offset in x direction (in pixels)
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    if aspect ratios of _rect and _bmp are different procedure will
      ///    preserve _rect ratio (empty margins will occur on _bmp)
      ///    </note>
      /// </remarks>
      procedure PrintTile             ( const _bmp      : TGIS_Bitmap ;
                                        const _offsetx  : Integer ;
                                        const _offsety  : Integer
                                      ) ; virtual; abstract;


      /// <summary>
      ///   Close printing using tiles.
      /// </summary>
      procedure PrintEnd               ; virtual; abstract;

      /// <summary>
      ///   Zoom in/out by _value factor.
      /// </summary>
      /// <param name="_value">
      ///   zoom factor
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    _value should be positive (&gt;0)
      ///    </note>
      /// </remarks>
      procedure SetRadius              ( const _value : Double
                                       ) ;

      /// <summary>
      ///   Mouse position (necessary in ZoomMode only).
      /// </summary>
      /// <param name="_value">
      ///   last mouseDown position
      /// </param>
      procedure StoreMousePos          ( const _value : TPoint
                                       ) ;

      /// <summary>
      ///   Set current mouse position as a scene rotation point. Active when
      ///   True is passed to a NewRotationPoint procedure. Works until MouseUp
      ///   occurs.
      /// </summary>
      /// <param name="_pt">
      ///   current mouse position on screen
      /// </param>
      procedure ChangeRotationPoint    ( const _pt : TPoint
                                       ) ;

      /// <summary>
      ///   Enable/Disable RotationPoint changing.
      /// </summary>
      /// <param name="_value">
      ///   if True enable RotationPoint changing
      /// </param>
      procedure NewRotationPoint       ( const _value : Boolean
                                       ) ;

      /// <summary>
      ///   Set view to initial state.
      /// </summary>
      procedure ResetView              ;

      /// <summary>
      ///   Get height where reference pointer crosses DEM.
      /// </summary>
      /// <returns>
      ///   Reference pointer Z coordinate.
      /// </returns>
      function  GetPointOnDEM          : Double ;

      /// <summary>
      ///   Get rendering speed in frames-per-second.
      /// </summary>
      /// <returns>
      ///   Frame per second rate.
      /// </returns>
      function  GetFps                 : Integer ;

      /// <summary>
      ///   Select object in 3D (as in 2D mode).
      /// </summary>
      /// <param name="_pt">
      ///   screen coordinate
      /// </param>
      /// <param name="_prec">
      ///   precision in pixels (must be &gt;= 0)
      /// </param>
      /// <returns>
      ///   Located shape or nil.
      /// </returns>
      function  Locate                 ( const _pt       : TPoint             ;
                                         const _prec     : Integer
                                       ) : TGIS_ShapeAbstract ;

      /// <summary>
      ///   Select object in 3D.
      /// </summary>
      /// <param name="_pt">
      ///   screen coordinate
      /// </param>
      /// <param name="_prec">
      ///   precision in pixels (must be &gt;= 0)
      /// </param>
      /// <param name="_layer">
      ///   input : layer to be searched, if nil all layers will be searched
      ///   output : layer contained located object, if nil no object located
      /// </param>
      /// <param name="_ptg">
      ///   hitting point
      /// </param>
      /// <param name="_shp">
      ///   located shape or nil
      /// </param>
      /// <param name="_part">
      ///   located shape part number
      /// </param>
      /// <returns>
      ///   True if shape is located, False if not.
      /// </returns>
      function  Locate3D               ( const _pt       : TPoint             ;
                                         const _prec     : Integer            ;
                                         var   _layer    : TGIS_LayerAbstract ;
                                         var   _ptg      : TGIS_Point3D       ;
                                         var   _shp      : TGIS_ShapeAbstract ;
                                         var   _part     : Integer
                                       ) : Boolean ;

      /// <summary>
      ///   Mark shape with color.
      /// </summary>
      /// <param name="_layer">
      ///   layer contained shape
      /// </param>
      /// <param name="_shpid">
      ///   shape to be marked
      /// </param>
      /// <param name="_part">
      ///   shape part to be marked, -1 if entire shape
      /// </param>
      /// <param name="_color">
      ///   color used to mark shape
      /// </param>
      /// <param name="_color">
      ///   color used to mark shape
      /// </param>
      /// <param name="_update">
      ///   if True redraw scene after mark, do not redraw if False
      /// </param>
      procedure MarkShape              ( const _layer : TGIS_Layer ;
                                         const _shpid : Integer    ;
                                         const _part  : Integer    ;
                                         const _color : TGIS_Color ;
                                         const _update: Boolean
                                       ) ;

      /// <summary>
      ///   Cancel previously marked shape with color.
      /// </summary>
      /// <param name="_layer">
      ///   layer contained shape
      /// </param>
      /// <param name="_shpid">
      ///   shape to be unmarked
      /// </param>
      /// <param name="_update">
      ///   if True redraw scene after mark, do not redraw if False
      /// </param>
      procedure UnMarkShape            ( const _layer : TGIS_Layer ;
                                         const _shpid : Integer    ;
                                         const _update: Boolean
                                       ) ;

      /// <summary>
      ///   Redraw all selected objects.
      /// </summary>
      procedure UpdateAllSelectedObjects ;

      /// <summary>
      ///   Force updating content of all vertex buffers.
      /// </summary>
      procedure UpdateWholeMap ;

      /// <summary>
      ///   Force updating content of topmost vertex buffers.
      /// </summary>
      procedure UpdateTopmost ;

      /// <summary>
      ///   Repaint scene with current settings.
      /// </summary>
      procedure Repaint ;

    public

      /// <summary>
      ///   Extent in which DEM is displayed in max LevelOfDetail (must be
      ///   &gt;= 0.5, 1 means screen width).
      /// </summary>
      property DemDetailExtentFactor   : Double
                                           read  fget_DemDetailExtentFactor
                                           write fset_DemDetailExtentFactor ;

      /// <summary>
      ///   Extent in which DEM is displayed in lower LevelOfDetail (must be
      ///   &gt;= 1.0, 4 means 4 times screen width).
      /// </summary>
      property DemDraftExtentFactor    : Double
                                           read  fget_DemDraftExtentFactor
                                           write fset_DemDraftExtentFactor  ;

      /// <summary>
      ///   Extent from which vector is displayed (must be &gt;= 1.0, 2 means 2
      ///   times screen width).
      /// </summary>
      property VectorExtentFactor      : Double
                                           read  fget_VectorExtentFactor
                                           write fset_VectorExtentFactor    ;

      /// <summary>
      ///   Enable/disable polygon simplification process.
      /// </summary>
      property VectorSimplification    : Boolean
                                           read  fget_VectorSimplification
                                           write fset_VectorSimplification  ;

      /// <summary>
      ///   Shape size in pixels, less or equal will not be displayed ( must be
      ///   &gt;= 0 ).
      /// </summary>
      property VectorSmartSize         : Integer
                                           read  fget_VectorSmartSize
                                           write fset_VectorSmartSize       ;

      /// <summary>
      ///   Current grid size setting.
      /// </summary>
      property DemGridSize             : Integer
                                           read  fget_DemGridSize           ;

      /// <summary>
      ///   Grid size used to display regular DEMs &amp; IMAGEs.
      /// </summary>
      property DemCachedSize           : TGIS_Viewer3DDemCacheSize
                                           read  fget_DemCachedSize
                                           write fset_DemCachedSize         ;

      /// <summary>
      ///   Flag. If True, Renderer is not ready to paint.
      /// </summary>
      property IsBusy                  : Boolean
                                           read fget_IsBusy ;

      /// <summary>
      ///   Flag. If True, Renderer will not perform deep map refresh.
      /// </summary>
      property MapRefreshLock          : Boolean
                                           read fget_MapRefreshLock ;
end;

type
//------------------------------------------------------------------------------
// TGIS_Renderer3DShape
//------------------------------------------------------------------------------

  /// <summary>
  ///   Simplified shape object for fast 3D representation.
  /// </summary>
  TGIS_Renderer3DShape = class ( TGIS_BaseObjectDisposable )
    private
      FPoints         : TGIS_Point3DList ;
      FParts          : array of Integer      ;
      FShapeType      : TGIS_ShapeType        ;
      FUid            : TGIS_Uid              ;
      FLayer          : TGIS_Layer            ;
      FIsSelected     : Boolean               ;
      FSize           : Integer               ;
      FShowCeiling    : Boolean               ;
      FShowWalls      : Boolean               ;
      FColor          : TGIS_Color            ;
      FOutlineColor   : TGIS_Color            ;
      FTexture        : String                ;
      FOutlineTexture : String                ;
      FMarkerStyle    : TGIS_MarkerStyle      ;
      FGround         : TGIS_3DGroundType     ;
      FBasement       : TGIS_3DBasementType   ;
      FScaleZ         : Double                ;
      FFalseZ         : Double                ;
      FScaleM         : Double                ;
      FFalseM         : Double                ;
      FCentroid       : TGIS_Point            ;
      FLabelBitmap    : TGIS_Bitmap           ;
      FLabelSize      : TPoint                ;
      FBitmapSize     : TPoint                ;
      FLabelZ         : Double                ;
      FLabelB         : Double                ;
      FVertexColors   : TGIS_Pixels           ;
    protected

      /// <summary>
      ///   Destroy an object.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Create an object.
      /// </summary>
      /// <param name="_type">
      ///   shape type of the object
      /// </param>
      /// <param name="_uid">
      ///   uid identifier of the object
      /// </param>
      /// <param name="_layer">
      ///   layer to which shape belongs
      /// </param>
      constructor Create       ( const _type  : TGIS_ShapeType ;
                                 const _uid   : TGIS_Uid       ;
                                 const _layer : TGIS_Layer
                               ) ;
    public

      /// <summary>
      ///   See TGIS_Shape.Reset for documentation.
      /// </summary>
      procedure Reset          ;

      /// <summary>
      ///   See TGIS_Shape.AddPart for documentation.
      /// </summary>
      procedure AddPart        ;

      /// <summary>
      ///   See TGIS_Shape.AddPoint3D for documentation.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be added
      /// </param>
      procedure AddPoint3D     ( const _ptg  : TGIS_Point3D
                               ) ;

      /// <summary>
      ///   See TGIS_Shape.GetPoint3D for documentation.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_pos">
      ///   point number
      /// </param>
      /// <returns>
      ///   Point from shape.
      /// </returns>
      function  GetPoint3D     ( const _part : Integer;
                                 const _pos  : Integer
                               ) : TGIS_Point3D ;

      /// <summary>
      ///   See TGIS_Shape.GetNumPoints for documentation.
      /// </summary>
      /// <returns>
      ///   Number of vertices in shape.
      /// </returns>
      function  GetNumPoints   : Integer ;

      /// <summary>
      ///   See TGIS_Shape.GetNumParts for documentation.
      /// </summary>
      /// <returns>
      ///   Number of parts in shape.
      /// </returns>
      function  GetNumParts    : Integer ;

      /// <summary>
      ///   See TGIS_Shape.GetPartSize for documentation.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <returns>
      ///   Number of vertices in part.
      /// </returns>
      function  GetPartSize    ( const _part : Integer
                               ) : Integer ;

    public

        /// <summary>
        ///   Get type of shape.
        /// </summary>
        property  ShapeType      : TGIS_ShapeType
                                   read  FShapeType ;

        /// <summary>
        ///   Shape unique Id.
        /// </summary>
        property  Uid            : TGIS_Uid
                                   read FUid ;

        /// <summary>
        ///   Layer to which shape belongs.
        /// </summary>
        property  Layer          : TGIS_Layer
                                   read  FLayer
                                   write FLayer ;

        /// <summary>
        ///   True if shape is in selected state.
        /// </summary>
        property  IsSelected     : Boolean
                                   read  FIsSelected
                                   write FIsSelected ;

        /// <summary>
        ///   Size of shape line width, marker size etc.
        /// </summary>
        property  Size           : Integer
                                   read  FSize
                                   write FSize ;

        /// <summary>
        ///   True if shapes's ceiling should be visible.
        /// </summary>
        property  ShowCeiling    : Boolean
                                   read  FShowCeiling
                                   write FShowCeiling ;

        /// <summary>
        ///   True if shapes's walls should be visible.
        /// </summary>
        property  ShowWalls      : Boolean
                                   read  FShowWalls
                                   write FShowWalls ;

        /// <summary>
        ///   Color of shape's ceiling; used only if Texture is nil.
        /// </summary>
        property  Color          : TGIS_Color
                                   read  FColor
                                   write FColor ;

        /// <summary>
        ///   Color of shape's walls; used only if Texture is nil.
        /// </summary>
        property  OutlineColor   : TGIS_Color
                                   read  FOutlineColor
                                   write FOutlineColor ;

        /// <summary>
        ///   Texture hashcode of shape's ceiling; if '' then solid Color used
        ///   instead.
        /// </summary>
        property  Texture        : String
                                   read  FTexture
                                   write FTexture ;

        /// <summary>
        ///   Texture hashcode of shape's walls; if '' then solid OutlineColor
        ///   used instead.
        /// </summary>
        property  OutlineTexture : String
                                   read  FOutlineTexture
                                   write FOutlineTexture ;

        /// <summary>
        ///   Marker style (for point only).
        /// </summary>
        property  MarkerStyle    : TGIS_MarkerStyle
                                   read  FMarkerStyle
                                   write FMarkerStyle ;

        /// <summary>
        ///   Z value relations (AboveZero, AboveDem, OnDem).
        /// </summary>
        property  Ground         : TGIS_3DGroundType
                                   read  FGround
                                   write FGround ;

        /// <summary>
        ///   3D object basement type (Off, Lowest).
        /// </summary>
        property  Basement       : TGIS_3DBasementType
                                   read  FBasement
                                   write FBasement ;

        /// <summary>
        ///   Scale Z.
        /// </summary>
        property  ScaleZ         : Double
                                   read  FScaleZ
                                   write FScaleZ ;

        /// <summary>
        ///   False Z.
        /// </summary>
        property  FalseZ         : Double
                                   read  FFalseZ
                                   write FFalseZ ;

        /// <summary>
        ///   Scale M.
        /// </summary>
        property  ScaleM         : Double
                                   read  FScaleM
                                   write FScaleM ;

        /// <summary>
        ///   False M.
        /// </summary>
        property  FalseM         : Double
                                   read  FFalseM
                                   write FFalseM ;

        /// <summary>
        ///   Centroid.
        /// </summary>
        property  Centroid       : TGIS_Point
                                   read  FCentroid
                                   write FCentroid ;

        /// <summary>
        ///   Bitmap containing label.
        /// </summary>
        property  LabelBitmap    : TGIS_Bitmap
                                   read  FLabelBitmap
                                   write FLabelBitmap ;

        /// <summary>
        ///   Label size (width, height).
        /// </summary>
        property  LabelSize      : TPoint
                                   read  FLabelSize
                                   write FLabelSize ;

        /// <summary>
        ///   Size of bitmap containing label (width, height).
        /// </summary>
        property  BitmapSize     : TPoint
                                   read  FBitmapSize
                                   write FBitmapSize ;

        /// <summary>
        ///   Texture label Z level.
        /// </summary>
        property  LabelZ         : Double
                                   read  FLabelZ
                                   write FLabelZ ;

        /// <summary>
        ///   Base level of labeled object.
        /// </summary>
        property  LabelB         : Double
                                   read  FLabelB
                                   write FLabelB ;

        /// <summary>
        ///   Array of vertex colors.
        /// </summary>
        property VertexColors    : TGIS_Pixels read  FVertexColors
                                               write FVertexColors ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
uses
  GisTessellation ;
{$ENDIF}

  // TVector3f record initialization (portable against different platforms).
  function vector3Init(
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

//==============================================================================
// TGIS_Renderer3DShape
//==============================================================================

  constructor TGIS_Renderer3DShape.Create(
    const _type  : TGIS_ShapeType ;
    const _uid   : TGIS_Uid       ;
    const _layer : TGIS_Layer
  ) ;
  begin
    assert( _type in [ TGIS_ShapeType.Point,
                       TGIS_ShapeType.MultiPoint,
                       TGIS_ShapeType.Arc,
                       TGIS_ShapeType.Polygon,
                       TGIS_ShapeType.Complex,
                       TGIS_ShapeType.MultiPatch
                     ]
          ) ;

    FShapeType := _type  ;
    FUid       := _uid   ;
    FLayer     := _layer ;
    FPoints    := TGIS_Point3DList.Create    ;
    FParts     := nil    ;
  end ;

  procedure TGIS_Renderer3DShape.doDestroy ;
  begin
    // force interface release
    FreeObject( FPoints ) ;
    FreeObject( FLabelBitmap ) ;

    inherited ;
  end ;


procedure TGIS_Renderer3DShape.Reset ;
  begin
    FPoints.Clear ;
    FParts  := nil ;
  end ;

procedure TGIS_Renderer3DShape.AddPart ;
  var
    prev_size : Integer ;
    part_pos  : Integer ;
  begin
    // test if we are not adding too many parts for points
    assert(
      ( not ( FShapeType in [ TGIS_ShapeType.Point, TGIS_ShapeType.MultiPoint ] ) )
      or
      ( GetNumParts < 1 )
    ) ;

    if GetNumParts = 0 then
      prev_size := 0
    else
      prev_size := FParts[ GetNumParts -1 ] ;

    part_pos := length( FParts ) ;
    SetLength( FParts, part_pos + 1 ) ;
    FParts[ part_pos ] := prev_size  ;
  end ;

  procedure TGIS_Renderer3DShape.AddPoint3D(
    const _ptg : TGIS_Point3D
  ) ;
  var
    point_pos : Integer ;
    part_pos  : Integer ;
  begin
    // test if we are not adding too many parts for points
    assert(
      ( FShapeType <> TGIS_ShapeType.Point )
      or
      ( GetNumPoints < 1 )
    ) ;

    assert( GetNumParts > 0 ) ;

    point_pos := FPoints.Count ;
    FPoints.Add( _ptg ) ;
    part_pos := length( FParts ) - 1 ;
    FParts[ part_pos ] := FParts[ part_pos ] + 1  ;
  end ;

  function TGIS_Renderer3DShape.GetPoint3D(
    const _part : Integer;
    const _pos  : Integer
  ) : TGIS_Point3D ;
  var
    prev_size : Integer ;
    point_pos : Integer ;
  begin
    assert( _part >= 0 ) ;
    assert( _pos  >= 0 ) ;
    assert( _part < GetNumParts ) ;
    assert( GetPartSize( _part ) > 0 ) ;

    if _part = 0 then
      prev_size := 0
    else
      prev_size := FParts[ _part -1 ] ;

    point_pos := prev_size + _pos ;

    Result := FPoints[ point_pos ]
  end ;

  function TGIS_Renderer3DShape.GetNumPoints
  : Integer ;
  begin
    Result := FPoints.Count ;
  end ;

  function TGIS_Renderer3DShape.GetNumParts
  : Integer ;
  begin
    Result := length( FParts ) ;
  end ;

  function TGIS_Renderer3DShape.GetPartSize(
    const _part : Integer
  ) : Integer ;
  var
    prev_size : Integer ;
  begin
    assert( _part >= 0 ) ;
    assert( _part < GetNumParts ) ;

    if _part = 0 then
      prev_size := 0
    else
      prev_size := FParts[ _part -1 ] ;

    Result := FParts[ _part ] - prev_size ;
  end ;

//==============================================================================
// TGIS_Renderer3DAbstract
//==============================================================================

  constructor TGIS_Renderer3DAbstract.Create ;
  begin
    inherited;

    uponDestroy := False ;

    oPolyRoof := nil ;

    oTimer := TGIS_Timer.Create ;
    oTimer.Interval := GISVIEWER3D_DELAY_TIME ;
    {$IFDEF OXYGENE}
      oTimer.OnTimer += doOnTimer ;
    {$ELSE}
      oTimer.OnTimer := doOnTimer ;
    {$ENDIF}
    oTimer.Enabled := False ;
    inPaint  := False ;
    inPaint1 := False ;
    windowSizeChanged := False ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_Renderer3DAbstract.doOnTimer(
      sender : System.Object
//?      e      : ElapsedEventArgs
    ) ;
  {$ELSE}

    procedure TGIS_Renderer3DAbstract.doOnTimer(
      sender : TObject
    ) ;
  {$ENDIF}
  begin
    if uponDestroy then exit ;

    if assigned( oTimer ) then
      oTimer.Enabled := False ;

    if labelModeEx then begin
      labelMode := True ;
      Repaint ;
      if renderer3D <> [TRenderer3D.FMX] then
        labelMode := False
      else
        wasTimer := True ;
    end
    else
      Repaint ;
    if not checkReRender then exit ;
    UpdateWholeMap ;
    if renderer3D = [TRenderer3D.FMX] then
      wasTimer := True ;
  end;

  function TGIS_Renderer3DAbstract.checkReRender
    : Boolean ;
  var
    val : Double ;
  begin
    val := 0.25 ;
    Result := True    ;
    if not bMustReRender then begin
      if bFastMode then begin
        Result := False ;
        exit;
      end;
      if (Abs(lastRadius - dRadius) > val * dRadius) or
         (Abs(lastTurn - dTurn) > val * wndXSize) or
         (Abs(lastFly - dFly) > val * wndXSize) or
         (lastScaleZ <> zFactor) then begin
           lastRadius := dRadius ;
           lastTurn   := dTurn   ;
           lastFly    := dFly    ;
           lastScaleZ := zFactor ;
           exit
        end
      else
        Result := False ;
    end;

  end;

  function TGIS_Renderer3DAbstract.ReRender
    : Integer ;
  var
    wasresize   : Boolean ;
  begin
    Result := S_OK ;
    wasresize := False ;

    if renderer3D <> [TRenderer3D.FMX] then begin
      if checkWindowSize then begin
        reset3DEnvironment ;
        wasresize := True ;
      end;
    end;

    if inPaint then exit ;
    try
      inPaint := True ;
      if renderer3D <> [TRenderer3D.FMX] then
        oGIS.HourglassPrepare ;
      if generalLock > 0  then exit ;
      reRenderCall := True ;
      lockReadingData := 0 ;
      prepareVertexBuffersToDraw ;
      lockReadingData := 1 ;
      reRenderCall   := False ;
      bMustReRender  := False ;
      bPartialUpdate := False ;
    finally
      inPaint := False ;
      if renderer3D <> [TRenderer3D.FMX] then
        oGIS.HourglassRelease ;
      Repaint ;
    end;
      Result := S_OK ;
  end;

  procedure TGIS_Renderer3DAbstract.SetViewer(
    const _viewer2d : IGIS_Viewer ;
    const _viewer3d : IGIS_Viewer3D
  ) ;
  begin
    oGIS := _viewer2d ;
    oViewer := _viewer3d ;
  end;

  procedure TGIS_Renderer3DAbstract.doDestroy ;
  begin
    uponDestroy := True ;
    oTimer.Enabled := False;

    FreeObject( oTimer ) ;

    releaseGeometry ;

    FreeObject( oPolyRoof ) ;
    FreeObject( oTextureHelper ) ;

    {$IFDEF CLR}
      FreeDirectXManager ;
    {$ENDIF}

    inherited ;
  end ;

  procedure TGIS_Renderer3DAbstract.SetInitValues(
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
    a          : Cardinal    ;
  begin
    oGIS      := _gis ;
    oViewer   := _viewer ;
    gisScale  := oGIS.ScaleAsFloat ;
    gisZoom   := oGIS.Zoom ;

    windowWidth  := getWindowSize.X ;
    windowHeight := getWindowSize.Y ;
    oldwindowWidth  := windowWidth  ;
    oldwindowHeight := windowHeight ;

    if (_cutextent.XMin = _visibleextent.XMin) and
       (_cutextent.YMin = _visibleextent.YMin) and
       (_cutextent.XMax = _visibleextent.XMax) and
       (_cutextent.YMax = _visibleextent.YMax) then
      mustAdjust := True
    else
      mustAdjust := False ;

    newTransform := False ;
    changeRotPoint := False ;
    transpPrior := TGIS_Viewer3DTransparencyPriority.Auto ;
    texDir  := True ;
    texMode := True ;
    viewRestriction   := TGIS_Viewer3DViewRestriction.AboveHorizon ;
    shadowsLevel := 0.4 ;

    imageTexture := True ;
    preciseSwitch := False ;
    referenceMode := TGIS_Viewer3DReferenceLevelMode.AboveZero ;
    referencePointMode := TGIS_Viewer3DReferenceMode.Highest ;
    referencePointOffset := 0 ;
    wasReferenceSet := False ;
    bIgnoreEllipsoidHeight := False ;
    bWasProjected := False ;
    unitDistance := 1.0 ;
    origOffset := 0 ;
    bTrace := False ;
    generalLock := 0 ;
    lockReadingData := 0 ;
    lightSwitch := False ;
    lightControl := False ;
    vectorEdges  := False ;
    drawTransparent := True ;
    errorTreatment := False ;
    demWall := TGIS_Viewer3DDemWall.Texture ;
    baseSize := 100.0 ;
    detailCoeff := 1.5 ;
    roughCoeff  := 3.0 ;
    vectorCoeff := 2.0 ;
    zDEMNormCoeff := 1.0 ;
    dDemDetailExtentFactor := detailCoeff ;
    dDemDraftExtentFactor  := roughCoeff  ;
    dVectorExtentFactor    := vectorCoeff ;
    bFastMode := False ;
    bVectorSimplification  := True ;
    iVectorSmartSize       := 2    ;

    // Camera settings
    cameraFocus := 50 ;
    cameraViewAngle := 1700.0 / Power(cameraFocus + 5.0, 0.89 ) - 2.0 ;
    cameraHalfViewAngle := 0.5 * cameraViewAngle ;

    dbl := GIS_MAX_SINGLE ;
    gridsExt := GisExtent( dbl, dbl, -dbl, -dbl ) ;

    zLevel      :=  GIS_MAX_SINGLE ;
    zLevelMax   := -GIS_MAX_SINGLE ;
    dIgnoreBelow:= -1e18 ;
    dCutBelow   := -1e18 ;
    zMin        := 0.0 ;
    xDemSize    := GIS_MAX_SINGLE ;
    yDemSize    := GIS_MAX_SINGLE ;
    noDataValue := GIS_GRID_NOVALUE ;
    noDem := True ;
    noImg := True ;
    demTransparency := 255 ;
    curDemTransp    := 255 ;
    allowDemTransp  := True ;

    heightRange    := 0 ;
    heightRangeMin :=  GIS_MAX_SINGLE ;
    heightRangeMax := -GIS_MAX_SINGLE ;

    exExtent := _visibleextent ;

    layersRangeSetting(True) ;
    if zLevel < -1E37 then begin // empty grid error protection
      zLevel := 0 ;
      zLevelMax := 0 ;
    end;

    curVLListSize := curVLSize ;

    if (_cutextent.XMin = _cutextent.XMax) or
       (_cutextent.YMin = _cutextent.YMax) then
       wrkextent := _visibleextent
    else
       wrkextent := _cutextent ;

    gridsExt  := wrkextent ;
    exExtent  := wrkextent ;

    if noDem then begin // noDem among layers
      exExtent := wrkextent ;
      zLevel := 0 ;
      zLevelMax := 0 ;
      gridsExt := exExtent ;
      xDemSize := (exExtent.XMax-exExtent.XMin)/baseSize ;
      yDemSize := (exExtent.YMax-exExtent.YMin)/baseSize ;
      isolineGap := 0 ;
      flood.Level  := 1 ;
    end;

    zMin := zLevel ;
    zLevelMin := zLevel ;

    solidWallColor := TGIS_Color.FromARGB( 255, 100, 100, 100 ) ;
    isolineColor   := TGIS_Color.FromARGB( 255, 249, 170,   6 ) ;

    edgesColor     := TGIS_Color.FromARGB( 255,  64,  64,  64 ) ;

    if noDem = False then begin   // DEM exists
      if zLevelMax = zLevelMin then
        isolineGap := 1
      else  begin
        isolineGap := 1.0*TruncS(Log10(zLevelMax-zLevelMin)-1) ;
        isolineGap := Power(10,isolineGap) ;
        if (zLevelMax-zLevelMin)/isolineGap > 50 then
          isolineGap := 10*isolineGap ;
      end;
    end ;

    flood.Active       := False            ;
    flood.Level        := zLevel           ;
    flood.Color        := TGIS_Color.FromRGB( 0, 0, 255 ) ;
    flood.Transparency := 70               ;

    if (noImg = False) and (noDem = True) then
      basePlane.Active        := False
    else
      basePlane.Active        := True ;
    basePlane.Level           := 0    ;
    basePlane.Transparency    := 70 ;
    a := TruncS(basePlane.Transparency/100.0*255.0) ;
    basePlane.BackgroundColor := TGIS_Color.FromARGB( a, 60, 60, 60 )    ;
    basePlane.GridColor       := TGIS_Color.FromARGB( a, 255, 255, 255 ) ;

    setZUnit ;
    updateLayerExistance ;
    setIgnoreCutValues ;

    grdRatio := (gridsExt.YMax - gridsExt.YMin)/(gridsExt.XMax - gridsExt.XMin);
    scrRatio := ( 1.0 * windowHeight )/windowWidth ;
    adjustRatio := scrRatio ;

    xMin := -baseSize ;
    xMax :=  baseSize ;
    yMin := -baseSize*grdRatio ;
    yMax :=  baseSize*grdRatio ;

    if (noImg = True) and (noDem = True) then begin
      vertexBufferSize := 512 ;
      enDemCachedSize := TGIS_Viewer3DDemCacheSize.Medium ;
    end
    else begin
      vertexBufferSize := TruncS(detailCoeff * Max(windowWidth,windowHeight)) ;
      if vertexBufferSize > 2048 then vertexBufferSize := 2048 ;
      enDemCachedSize := TGIS_Viewer3DDemCacheSize.Window ;
    end;

    iDemGridSize   := vertexBufferSize ;

    demFrameSize := TruncS((wrkextent.XMax - wrkextent.XMin)/xDemSize) ;
    if demFrameSize mod 2 <> 0 then
      demFrameSize := demFrameSize + 1 ;
    if demFrameSize > vertexBufferSize then
      demFrameSize := vertexBufferSize ;
    if demFrameSize = 0 then
      demFrameSize := 128 ;

    imgFrameSize := vertexBufferSize ;
    setExtent;

    xMin := xxMin ;
    xMax := xxMax ;
    yMin := yyMin ;
    yMax := yyMax ;

    maxExt := -xxMin ;
    if maxExt < -yyMin  then maxExt := -yyMin ;
    if maxExt <  xxMax  then maxExt :=  xxMax ;
    if maxExt <  yyMax  then maxExt :=  yyMax ;

    normExt := GisExtent( xxMin, yyMin, xxMax, yyMax ) ;

    grdColumns := TruncS((gridsExt.XMax - gridsExt.XMin) / xDemSize) ;
    if grdColumns = 0 then grdColumns := 128 ;

    grdRows    := TruncS((gridsExt.YMax - gridsExt.YMin) / yDemSize) ;
    if grdRows = 0 then grdRows := TruncS( grdColumns * grdRatio ) ;

    excolumns  := TruncS((exExtent.XMax - exExtent.XMin) / xDemSize) ;
    if excolumns = 0 then excolumns := grdColumns ;

    exrows     := TruncS((exExtent.YMax - exExtent.YMin) / yDemSize) ;
    if exrows = 0 then exrows := grdRows ;

    nxDemSize  := (normExt.XMax - normExt.XMin) / excolumns ;
    nyDemSize  := (normExt.YMax - normExt.YMin) / exrows ;

    SetLength( arGrid_r, demFrameSize, demFrameSize ) ;
    SetLength( arGrid_w, demFrameSize, demFrameSize ) ;
    init_arGrid( arGrid_r ) ;

    arBitmap_w := TGIS_Bitmap.Create( 16, 256 ) ;

    setLod ;

    maxLod := lod ;
    maxLodStep := TruncS(Power(2.0,maxLod*1.0)) ;

    if( heightRange / unitDistance ) > 2 then
      zFactor := 0.01
    else
      zFactor := 1.0 ;
    mFactor := 1.0 ;
    dispMode := False ;
    labelMode := True ;

    setViewExtent(_visibleextent, dTurn, dFly, dx) ;
    initTurn := dTurn ;
    initFly  := dFly ;
    dRadius := 1.0 * dx/Tan(DegToRad(cameraHalfViewAngle)) ;
    dRadius := 0.5 * dRadius * scrRatio ;
    initRadius := dRadius ;

    basicEyeAt.X := 0.0    ;
    basicEyeAt.Y := 0.01   ;
    basicEyeAt.Z := dRadius ;
    basicEyeTo.X := 0.0    ;
    basicEyeTo.Y := 0.0    ;
    basicEyeTo.Z := 0.0    ;
    zEyeTo       := 0.0    ;
    zOffset      := 0.0    ;

    spinX  := 0.0 ;
    spinY  := 0.0 ;
    spinZ  := 0.0 ;
    cspinX := 0.0 ;
    captureScreen := False ;
    bArrow := True ;
    drawArrow := True ;
    scrExtent := GisExtent( 0, 0, 0, 0 ) ;
    roughExtent := scrExtent ;
    defaultZ := zScaleFactor * (zMin-zLevel)/zUnit ;

    SetLength( arVectorTilesInfo , 0 ) ;
    vectorTilesInfoCounter := 0 ;
    bPartialUpdate         := False   ;
    orthoMode              := False   ;
    bDefaultIgnoreCut      := True ;
    labelModeEx            := False ;
    wasTimer               := False ;

    // Video vertex buffer parameters
    initVideoVBSize        := 120000 ;
    videoVBIncrement       := 10*6000 ;
    maxVertexBufferSize    := 48000000 ;
    safeRendering          := False ; // one big buffer per layer
    safeRenderingDone      := False ; // not already switched on
  end;

  procedure TGIS_Renderer3DAbstract.releaseGeometry ;
  begin
    freeMeshStore ;

    SetLength(wTLBufT.TriLst     , 0) ;
    SetLength(vTLBufCR.TriLst    , 0) ;
    SetLength(vTLBufCW.TriLst    , 0) ;
    SetLength(vTLBufTR.TriLst    , 0) ;
    SetLength(vTLBufTR.Subset    , 0) ;
    SetLength(vTLBufTW.TriLst    , 0) ;
    SetLength(vTLBufTW.Subset    , 0) ;
    SetLength(tinBufT.TriLst     , 0) ;
    SetLength(tinBufC.TriLst     , 0) ;
    SetLength(mpatchBufC.TriLst  , 0) ;
    SetLength(mpatchBufT.TriLst  , 0) ;
    SetLength(mpatchBufT.Subset  , 0) ;
    SetLength(wPLBuf.PntLst      , 0) ;
    SetLength(wPBuf.PntLst       , 0) ;

    SetLength(arVectPolyRoofC    , 0, 0) ;
    SetLength(arVectPolyRoofT    , 0, 0) ;

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
    // release rough Grid array
    SetLength( arGrid_r, 0 ) ;
    SetLength( arGrid_w, 0 ) ;

    // release layers store
    SetLength( curDem , 0 ) ;
    SetLength( curMesh, 0 ) ;
    SetLength( curVL  , 0 ) ;
    SetLength( curVLlist , 0 ) ;

    if assigned( arBitmap_w ) then
     FreeObject( arBitmap_w ) ;

  end;

  procedure TGIS_Renderer3DAbstract.SetNavigationType(
    const _value : Boolean
  ) ;
  begin
    advNavigation := _value ;
  end;

  procedure TGIS_Renderer3DAbstract.SetNavigationMode(
    const _value : TGIS_Viewer3DMode
  ) ;
  begin
    viewerMode := _value ;
    optionSelected := True ;
  end;

  procedure TGIS_Renderer3DAbstract.SetOrthoView(
    const _value : Boolean
  ) ;
  begin
    if orthoMode = _value then exit;
    orthoMode := _value ;
    ResetView ;
    Repaint ;
  end;

  function TGIS_Renderer3DAbstract.GetScaleZ : Double;
  begin
    Result := zFactor ;
  end;

  procedure TGIS_Renderer3DAbstract.SetScaleZ(
    const _value : Double
  ) ;
  begin
    if _value = 0 then exit ;
    scaleMeshTilesZ( _value / zFactor ) ;
    zFactor := _value ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetScaleM
    : Double;
  begin
    Result := mFactor ;
  end;

  procedure TGIS_Renderer3DAbstract.SetScaleM(
    const _value : Double
  ) ;
  begin
    if _value = 0 then exit ;
    scaleMeshTilesM( _value / mFactor ) ;
    mFactor := _value ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetScale3D
    : Double ;
  var
    z1 : Double ;
  begin
    z1 := 1.0 / GetPixelSize.X ;
    Result := gisScale * z1 / gisZoom ;
  end;

  procedure TGIS_Renderer3DAbstract.SetScale3D(
    const _value : Double
  ) ;
  var
    scl : Double ;
    dlt : Double ;
    r1  : Double ;
    r2  : Double ;
    i   : Integer ;

    procedure downSearch(
      var   _r1    : Double ;
      var   _r2    : Double ;
      const _value : Double ;
      var   _step  : Double
    ) ;
    var
      dbl : Double ;
    begin
      while True do begin
        dbl := _r2 ;
        dRadius := (_r1 + _r2) / 2.0 ;
        _step  := dbl - dRadius ;
        if GetScale3D >= _value then begin
          _r1 := dRadius ;
          break ;
        end ;
        _r2 := dRadius ;
      end;
    end;

  begin
    if _value <= 0 then exit ;
    if _value >  1e10 then exit ;

    i   := 2 ;
    scl := GetScale3D ;

    if Abs( 1/scl - 1/_value ) <= 0.1 then exit;

    if scl <= _value then begin
      r1  := 0 ;
      r2  := dRadius ;
    end
    else while True do begin
           r1 := dRadius ;
           dRadius := i * dRadius ;
           r2 := dRadius ;
           scl := GetScale3D ;
           if scl < _value then break ;
           inc( i ) ;
    end;

    while ( Abs( 1/scl - 1/_value ) > 0.1 )
      do begin
        if scl < _value then begin
          downSearch(r1, r2, _value, dlt) ;
        end
        else begin
          dRadius := dRadius + dlt ;
        end ;
        scl := GetScale3D ;
      end ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetScale3DAsText
    : String ;
  var
    dval : Double ;
  begin
    dval := GetScale3D ;

    if      dval =  0     then Result := ''
    else if dval >  1e30  then Result := ''
    else if dval <  1     then Result := Format( '1:%.0f'  , [ 1/dval ] )
    else if dval >= 1     then Result := Format( '%.0f:1'  , [ dval/1 ] ) ;
  end ;

  procedure TGIS_Renderer3DAbstract.SetScale3DAsText(
    const _value : String
  ) ;
  var
    i    : Integer  ;
    a, b : Double ;
  begin
    i := Pos( String(':'), _value ) ;
    if i = StringFirst - 1 then  i := Pos( String('/'), _value ) ;

    if i > StringFirst - 1 then begin
      a := DotStrToFloat( Copy( _value, StringFirst    , i - StringFirst ) ) ;
      b := DotStrToFloat( Copy( _value, i + 1, 1024  ) ) ;
      SetScale3D( a/b ) ;
    end
    else
      SetScale3D( DotStrToFloat( _value ) ) ;
    optionSelected := True ;
  end ;

  function TGIS_Renderer3DAbstract.GetPixelSize
    : TGIS_Point ;
  var
    pt       : TGIS_Point   ;
    pt1, pt2 : TGIS_Point3D ;
    dd       : Double       ;
  begin
    dd := windowWidth*dRadius*Tan(DegToRad(cameraHalfViewAngle))/
          windowHeight;
    pt1.X := -dd ;
    pt1.Y := 0 ;
    pt1.Z := zLevel ;
    pt1.M := 0 ;
    pt1   := d3DToMap(pt1) ;
    pt2.X := dd ;
    pt2.Y := 0 ;
    pt2.Z := zLevel ;
    pt2.M := 0 ;
    pt2   := d3DToMap(pt2) ;
    pt.X  := (pt2.X - pt1.X) / windowWidth ;

    pt1.X := 0 ;
    pt1.Y := -dd*scrRatio ;
    pt1.Z := zLevel ;
    pt1.M := 0 ;
    pt1   := d3DToMap(pt1) ;
    pt2.X := 0 ;
    pt2.Y := dd*scrRatio ; ;
    pt2.Z := zLevel ;
    pt2.M := 0 ;
    pt2   := d3DToMap(pt2) ;
    pt.Y  := (pt2.Y - pt1.Y) / windowHeight ;

    Result := pt ;
  end;

  function TGIS_Renderer3DAbstract.d3DToMap (
    const _pnt : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    npnt : TGIS_Point3D ;
    bs   : Double       ;
  begin
    npnt.X := baseSize + _pnt.X ;
    npnt.X := gridsExt.XMin + npnt.X * (gridsExt.XMax - gridsExt.XMin) /
                                       (2.0*baseSize) ;
    bs := baseSize * grdRatio ;
    npnt.Y := bs + _pnt.Y ;
    npnt.Y := gridsExt.YMin + npnt.Y * (gridsExt.YMax - gridsExt.YMin) /
                                       (2.0*bs) ;
    npnt.Z := _pnt.Z * (gridsExt.XMax - gridsExt.XMin) / (2.0*baseSize) ;
    npnt.M := _pnt.M * (gridsExt.XMax - gridsExt.XMin) / (2.0*baseSize) ;
    Result := npnt ;
  end;

  function TGIS_Renderer3DAbstract.GetWireframe
    : Boolean ;
  begin
    Result := dispMode ;
  end;

  procedure TGIS_Renderer3DAbstract.SetWireframe(
    const _value : Boolean
  ) ;
  begin
    dispMode := _value ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetLights
    : Boolean ;
  begin
    Result := lightSwitch ;
  end;

  procedure TGIS_Renderer3DAbstract.SetLights(
    const _value : Boolean
  ) ;
  begin
    lightSwitch := _value ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetLabels
    : Boolean ;
  begin
    Result := labelMode ;
  end;

  procedure TGIS_Renderer3DAbstract.SetLabels(
    const _value : Boolean
  ) ;
  begin
    labelMode := _value ;
  end;

  function TGIS_Renderer3DAbstract.GetLabelsEx
    : Boolean ;
  begin
    Result := labelModeEx ;
  end;

  procedure TGIS_Renderer3DAbstract.SetLabelsEx(
    const _value : Boolean
  ) ;
  begin
    labelModeEx := _value ;
    if not labelModeEx then
      labelMode := True
    else
      labelMode := False ;
  end;

  function TGIS_Renderer3DAbstract.GetVectorEdges
    : Boolean ;
  begin
    Result := vectorEdges ;
  end;

  procedure TGIS_Renderer3DAbstract.SetVectorEdges(
    const _value : Boolean
  ) ;
  begin
    vectorEdges   := _value ;
    bMustReRender := True   ;
    ReRender ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetDrawTransparent
    : Boolean ;
  begin
    Result := drawTransparent ;
  end;

  procedure TGIS_Renderer3DAbstract.SetDrawTransparent(
    const _value : Boolean
  ) ;
  begin
    drawTransparent   := _value ;
    Draw ;
  end;

  function TGIS_Renderer3DAbstract.GetEdgesColor
    : TGIS_Color ;
  begin
    Result := edgesColor ;
  end;

  procedure TGIS_Renderer3DAbstract.SetEdgesColor(
    const _value : TGIS_Color
  ) ;
  begin
    edgesColor := _value ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetOriginPointer
    : Boolean ;
  begin
    Result := drawArrow ;
  end;

  procedure TGIS_Renderer3DAbstract.SetOriginPointer(
    const _value : Boolean
  ) ;
  begin
    drawArrow := _value ;
    bArrow := False ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetOriginPoint
    : TGIS_Point3D ;
  var
    pt3d : TGIS_Point3D ;
  begin
    pt3d.X := -dTurn ;
    pt3d.Y := dFly ;
    pt3d.Z := 0 ;
    pt3d.M := 0 ;
    pt3d := d3DToMap(pt3d) ;
    if zScaleFactor <> 0 then
      pt3d.Z := (zEyeTo+zOffset)*zUnit/zScaleFactor
    else
      pt3d.Z := zLevel ;
    Result := pt3d ;
  end;

  procedure TGIS_Renderer3DAbstract.SetOriginPoint(
    const _value : TGIS_Point3D
  ) ;
  var
    pt3d, cp : TGIS_Point3D ;
    off : Double ;

    procedure adjust ;
    begin
      cp := cameraPositionEx ;
      dRadius := Sqrt(Sqr(cp.X + dTurn) +
                      Sqr(cp.Y - dFly ) +
                      Sqr(cp.Z) ) ;

      spinX := RadToDeg(ArcCos(cp.Z / dRadius)) ;
      if spinX <> 0.0 then
        spinZ := ArcCos((dFly - cp.Y)/(dRadius * Sin(DegToRad(spinX))))
      else
        spinZ := ArcCos((dFly - cp.Y)/(dRadius * Sin(DegToRad(0.01)))) ;
      spinZ := RadToDeg(spinZ) ;
      spinZ := truncateTo360(360 - spinZ) ;
    end;

  begin
    pt3d   := mapToD3D( _value ) ;
    dTurn  := -pt3d.X ;
    dFly   :=  pt3d.Y ;
    off    := 0 ;
    case GetReferencePointMode of
      TGIS_Viewer3DReferenceMode.Base     :
          off := pt3d.Z * zUnit / zScaleFactor ;
      TGIS_Viewer3DReferenceMode.Zero     :
          off := pt3d.Z * zUnit / zScaleFactor + zLevel ;
      TGIS_Viewer3DReferenceMode.OnDem    : begin
          off := pt3d.Z * zUnit ;
          setPointOnDEM( off ) ;
          adjust ;
          Repaint ; //?  Draw ;
          exit;
        end;
      TGIS_Viewer3DReferenceMode.Lowest   :
          off := pt3d.Z * zUnit / zScaleFactor + zLevel - rangeExtent.ZMin ;
      TGIS_Viewer3DReferenceMode.Highest  :
          off := pt3d.Z * zUnit / zScaleFactor + zLevel - rangeExtent.ZMax ;
      TGIS_Viewer3DReferenceMode.FlyOnDem :
          off := pt3d.Z * zUnit ;
    end;

    SetReferencePointOffset( off ) ;
    adjust ;
    Repaint ; //? Draw ;
  end;

  function TGIS_Renderer3DAbstract.GetReferencePointMode
    : TGIS_Viewer3DReferenceMode ;
  begin
    Result := referencePointMode ;
  end;

  procedure TGIS_Renderer3DAbstract.SetReferencePointMode(
    const _value : TGIS_Viewer3DReferenceMode
  ) ;
  var
    offset : Double ;
  begin
    offset := GetOriginPoint.Z ;
    referencePointMode := _value ;

    if referencePointMode = TGIS_Viewer3DReferenceMode.FlyOnDem then
      setReferencePoint( TGIS_Viewer3DReferenceMode.OnDem, 0)
    else
      setReferencePoint( referencePointMode, 0) ;

    referencePointOffset := offset - GetOriginPoint.Z  ;
    setReferencePoint( referencePointMode, referencePointOffset) ;
    optionSelected := True ;
  end;

  procedure TGIS_Renderer3DAbstract.setReferencePoint(
    const _mode   : TGIS_Viewer3DReferenceMode ;
    const _offset : Double
  ) ;
  begin
    case _mode of
      TGIS_Viewer3DReferenceMode.Base     :
        setReferenceParams( TGIS_Viewer3DReferenceLevelMode.Default,
                            _offset
                          ) ;
      TGIS_Viewer3DReferenceMode.Zero     :
        setReferenceParams( TGIS_Viewer3DReferenceLevelMode.AboveZero,
                            _offset
                          ) ;
      TGIS_Viewer3DReferenceMode.OnDem    :
        setPointOnDEM( _offset ) ;
      TGIS_Viewer3DReferenceMode.Lowest   :
        setReferenceParams( TGIS_Viewer3DReferenceLevelMode.AboveZero,
                            _offset +  rangeExtent.ZMin
                          ) ;
      TGIS_Viewer3DReferenceMode.Highest  :
        setReferenceParams( TGIS_Viewer3DReferenceLevelMode.AboveZero,
                            _offset + rangeExtent.ZMax
                          ) ;
      TGIS_Viewer3DReferenceMode.FlyOnDem :
        setReferenceParams( TGIS_Viewer3DReferenceLevelMode.AboveDem,
                            _offset
                          ) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.setReferenceParams(
    const _mode   : TGIS_Viewer3DReferenceLevelMode ;
    const _offset : Double
  ) ;
  begin
    referenceMode := _mode;
    origOffset := _offset ;
    if _mode = TGIS_Viewer3DReferenceLevelMode.AboveZero then begin
      zOffset := 0 ;
      zEyeTo := zScaleFactor*(_offset - zLevel)/ zUnit ;
      bTrace := False ;
    end
    else   // above DEM
    if _mode = TGIS_Viewer3DReferenceLevelMode.AboveDem then begin
      zOffset := _offset / zUnit ;
      bTrace := True ;
    end
    else begin
      zOffset := 0 ;
      zEyeTo := zScaleFactor*origOffset / zUnit ;
      bTrace := False ;
    end
  end;

  function TGIS_Renderer3DAbstract.getReferenceLevel
    : Double ;
  begin
    if zFactor <> 0 then
      Result := ( zEyeTo + zOffset ) * zUnit / zScaleFactor + zLevel
    else
      Result := zLevel ;
  end;

  procedure TGIS_Renderer3DAbstract.setReferenceLevel (
    const _value : Double
  ) ;
  begin
    referenceMode := TGIS_Viewer3DReferenceLevelMode.AboveZero;
    origOffset := _value ;
  end;

  procedure TGIS_Renderer3DAbstract.setPointOnDEM(
    const _offset : Double
  ) ;
  begin
    traceReferenceLevel(True) ;
    setReferenceLevel(getReferenceLevel + _offset) ;
    traceReferenceLevel(False) ;
    bTrace := False ;
  end;

  procedure TGIS_Renderer3DAbstract.traceReferenceLevel(
    const _value : Boolean
  ) ;
  var
    valx, valy : Double       ;
    val        : Double       ;
    pnt        : TGIS_Point3D ;
    jj, kk     : Integer      ;
    wrk_ext    : TGIS_Extent  ;
  begin
    if not _value then begin
        if referenceMode <> TGIS_Viewer3DReferenceLevelMode.AboveDem then
           setReferenceParams( referenceMode, origOffset ) ;
        exit ;
    end;
    if noDem then exit ;
    if roughExtent.XMin = roughExtent.XMax then exit ;

    wrk_ext := traceExtent ;
    pnt := GetOriginPoint ;

    valx := (( wrk_ext.XMax - pnt.X) /( wrk_ext.XMax - wrk_ext.XMin)) ;
    valy := (( wrk_ext.YMax - pnt.Y) /( wrk_ext.YMax - wrk_ext.YMin)) ;

    //get Z value from DEM
    kk := demFrameSize - TruncS(valx * demFrameSize) ;
    jj := TruncS(valy * demFrameSize) ;
    if (jj<0) or (jj>=demFrameSize-1) or (kk<0) or (kk>=demFrameSize-1) then
      val := defaultZ
    else
      val := zValue(kk, jj) ;

    // keep last zEyeTo if current val = defaultZ and FlyOnDem is on
    if not((val = defaultZ) and
           (referenceMode = TGIS_Viewer3DReferenceLevelMode.AboveDem)) then
      zEyeTo := val ;

    // value in GIS units
    pnt.Z := zEyeTo * zUnit + zLevel ;
  end;

  function TGIS_Renderer3DAbstract.xValue(
    const _col : Integer
  ) : Double ;
  begin
    Result := xMin + _col * (xMax - xMin)/(demFrameSize - 1) ;
  end;

  function TGIS_Renderer3DAbstract.yValue(
    const _row : Integer
  ) : Double ;
  begin
    Result :=  yMin + _row * (yMax - yMin)/(demFrameSize - 1) ;
  end;

  function TGIS_Renderer3DAbstract.zValue(
    const _col : Integer ;
    const _row : Integer
  ) : Single ;
  var
    k, j : Integer ;
  begin
    k := _col ;
    j := _row ;
    if j > (demFrameSize -1) then j := demFrameSize -1 ;
    if k > (demFrameSize -1) then k := demFrameSize -1 ;
    if j < 0 then j := 0 ;
    if k < 0 then k := 0 ;

    if arGrid_r[j][k] = noDataValue then begin
      Result := defaultZ ;
      exit;
    end
    else if arGrid_r[j][k] > dIgnoreAbove then begin
      if dIgnoreAbove <= dCutAbove then begin
        Result := sglIgnoreAbove ;
        exit;
      end
      else begin
        Result := zScaleFactor * curDem[0].Nz *(dCutAbove - zLevel) / zUnit ;
        exit;
      end
    end
    else if arGrid_r[j][k] < dIgnoreBelow then begin
      if dIgnoreBelow > dCutBelow then begin
        Result := sglIgnoreBelow ;
        exit;
      end
      else begin
        Result := sglCutBelow ;
        exit;
      end;
    end
    else if (arGrid_r[j][k] < dCutBelow) then begin
      Result := sglCutBelow ;
      exit;
    end
    else if (arGrid_r[j][k] > dCutAbove) then begin
      Result := zScaleFactor * (dCutAbove - zLevel) / zUnit ;
      exit;
    end
    else
      Result := zScaleFactor * curDem[0].Nz * (arGrid_r[j][k] - zLevel) / zUnit ;
  end ;

  function TGIS_Renderer3DAbstract.tuValue(
    const _col : Integer
  ) : Single ;
  var
    i : Integer ;
  begin
    i := _col ;
    if i < 0 then
      i := 0 ;
    if i > demFrameSize -1 then
      i := demFrameSize -1 ;
    Result := 1.0 * i/( demFrameSize -1 ) ;
  end;

  function TGIS_Renderer3DAbstract.GetReferencePointOffset
    : Double ;
  begin
    Result := referencePointOffset ;
  end;

  procedure TGIS_Renderer3DAbstract.SetReferencePointOffset(
    const _value : Double
  ) ;
  begin
    referencePointOffset := _value ;
    setReferencePoint( referencePointMode, referencePointOffset) ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetCameraPosition
    : TGIS_Point3D ;
  var
    cp : TGIS_Point3D ;
  begin
    cp.X := DegToRad( cameraPosition.X ) ;
    cp.Y := DegToRad( cameraPosition.Y ) ;
    cp.Z := dRadius * (gridsExt.XMax - gridsExt.XMin) / (2.0 * baseSize) ;
    if checkCSWGS then
      cp.Z := cp.Z * gisCSzUnit ;
    cp.M := cameraPosition.M ;
    Result := cp ;
  end;

  procedure TGIS_Renderer3DAbstract.SetCameraPosition(
    const _value         : TGIS_Point3D
  ) ;
  var
    pt : TPoint ;

  function screenToMap3DIntrnl(
    const _pt : TPoint
  ) : TGIS_Point3D ;
  var
    ptg : TGIS_Point3D       ;
    la  : TGIS_LayerAbstract ;
    shp : TGIS_ShapeAbstract ;
    prt : Integer            ;
  begin
    la := nil ;
    if Select3DObject(_pt, 0, la, ptg, shp, prt, False ) then
      Result := ptg
    else begin
      Result.X := 0 ;
      Result.Y := 0 ;
      Result.Z := 0 ;
      Result.M := 0 ;
    end;
  end;

  procedure setoffsets ;
  var
   dt, df, dz : Double ;
   dd, dl, len, l1, l2  : Double ;
   a, b, c, d, a1, b1 : Double ;
   x1, y1, x2, y2, X  : Double ;
   zoff : Double ;
  begin
    mousePos3D.Y := reverseValue( mousePos3D.Y ) ;
    dt := -dTurn ;
    df := -dFly  ;
    dd := Sqrt(Sqr( mousePos3D.X - dt) + Sqr(mousePos3D.Y - df) ) ;
    if (Abs( dd ) < 1) or (Abs(cameraPosition.Z - dRadius) < 0.1)  then exit ;
    dl := cameraPosition.Z / dRadius ;
    len := dd / dl ;
    a1 := (df - mousePos3D.Y)/(dt - mousePos3D.X) ;
    b1 := -a1 * dt + df ;
    X  := mousePos3D.X ;
    a  := a1 * a1 + 1 ;
    b  := -2 * (X + a1 * a1 * X ) ;
    c  := (a1 * a1 + 1) * X*X - len*len ;
    d  := b*b - 4*a*c ;
    x1 := (-b + Sqrt(d))/(2*a) ;
    y1 := a1*x1 + b1 ;
    l1 := Sqrt(Sqr(x1 - dt) + Sqr(y1 - df)) ;
    x2:= (-b - Sqrt(d))/(2*a) ;
    y2:= a1*x2 + b1 ;
    l2 := Sqrt(Sqr(x2 - dt) + Sqr(y2 - df)) ;
    if l1 <= l2 then begin
      dTurn := -x1 ;
      dFly  := -y1 ;
    end
    else begin
      dTurn := -x2 ;
      dFly  := -y2 ;
    end;
    zoff := zScaleFactor * zLevel / zUnit ;
    dz := (mousePos3D.Z - zEyeTo) / dl ;
    zEyeTo := mousePos3D.Z - dz ;
    referencePointMode := TGIS_Viewer3DReferenceMode.Zero ;
    SetReferencePointOffset((zEyeTo + zoff) * zUnit / zScaleFactor ) ;
    setReferencePoint(referencePointMode, referencePointOffset) ;
  end;

  begin
    dRadius := (_value.Z * 2.0 * baseSize  ) /
               (gridsExt.XMax - gridsExt.XMin) ;
    if dRadius < 0.75 then dRadius := 0.75 ;

    if renderer3D = [TRenderer3D.FMX] then
      if dRadius > 900 then dRadius := 900 ;

    if checkCSWGS then
      dRadius := dRadius / gisCSzUnit ;

    if advNavigation then
    if Abs(cameraPosition.Z - dRadius) > 0.05 then begin // zooming !

      pt := lastMouseDownPos ;

      if (pt.X < 0) or (pt.X > windowWidth ) or
         (pt.Y < 0) or (pt.Y > windowHeight) then
        pt := Point(TruncS(windowWidth/2), TruncS(windowHeight/2)) ;
      screenToMap3DIntrnl( pt ) ;
      setoffsets ;
    end;

    spinX  := RadToDeg( _value.X  ) ;
    if spinX > 90 then spinX := 90 ;
    if not ( viewRestriction = TGIS_Viewer3DViewRestriction.AboveHorizon ) then begin
      if spinX < -90 then
        spinX := -89.90 ;
    end
    else begin
      if spinX < 0 then
        spinX := 0 ;
    end;
    cameraPosition.X := spinX ;
    spinX := 90 - spinX ;

    cameraPosition.Y := truncateTo360( RadToDeg( _value.Y ) ) ;
    spinZ := truncateTo360( 180 - cameraPosition.Y ) ;
    cameraPosition.Z := dRadius ;
    cameraPosition.M := _value.M ;
  end;

  function TGIS_Renderer3DAbstract.GetCameraPositionEx
    : TGIS_Point3D ;
  var
    xs   : Double       ;
    ys   : Double       ;
    cpex : TGIS_Point3D ;
  begin
    xs := (gridsExt.XMax + gridsExt.XMin)/ 2.0 ;
    ys := (gridsExt.YMax + gridsExt.YMin)/ 2.0 ;
    cameraPositionEx.X := -dTurn + (dRadius * Sin(DegToRad(spinZ))*
                            Sin(DegToRad(spinX))) ;
    cpex.X := xs + cameraPositionEx.X * (gridsExt.XMax - gridsExt.XMin)/(
                2.0*baseSize) ;
    cameraPositionEx.Y := dFly - (dRadius * Cos(DegToRad(spinZ))*
                            Sin(DegToRad(spinX))) ;
    cpex.Y := ys + cameraPositionEx.Y * (gridsExt.XMax - gridsExt.XMin)/
                           (2.0*baseSize) ;
    cameraPositionEx.Z := dRadius * Cos(DegToRad(spinX)) ;

    if checkCSWGS then
      cpex.Z := getReferenceLevel + zUnit * cameraPositionEx.Z
    else
      cpex.Z := getReferenceLevel + cameraPositionEx.Z *
                (gridsExt.XMax - gridsExt.XMin)/(2.0*baseSize) ;

    cameraPositionEx.M := 0.0 ;
    cpex.M := cameraPositionEx.M ;
    Result := cpex ;
  end;

  function TGIS_Renderer3DAbstract.GetCameraPosition3DEx
    : TGIS_Point3D ;
  begin
    updateCameraParams ;
    Result := cameraPositionEx ;
  end;

  procedure TGIS_Renderer3DAbstract.SetCameraPositionEx(
    const _value : TGIS_Point3D
  ) ;
  var
    xs   : Double ;
    ys   : Double ;
    rl   : Double ;
    cz   : Double ;
    dz   : Double ;
  begin
    xs := (gridsExt.XMax + gridsExt.XMin)/ 2.0 ;
    ys := (gridsExt.YMax + gridsExt.YMin)/ 2.0 ;
    rl := getReferenceLevel ;
    if checkCSWGS then
      cameraPositionEx.Z := (_value.Z - rl) / zUnit
    else
      cameraPositionEx.Z := (_value.Z - rl) /
                ((gridsExt.XMax - gridsExt.XMin)/(2.0*baseSize)) ;
    if not ((cameraPositionEx.Z = 0) or (spinX = 90)) then begin
      dRadius := cameraPositionEx.Z / Cos(DegToRad(spinX)) ;
      if dRadius < 0.75 then
        dRadius := 0.75 ;
    end;

    if (cameraPositionEx.Z = 0) or (spinX = 90) then begin
      cz := GetCameraPositionEx.Z ;
      dz := cz - _value.Z ;
      cameraPositionEx.X := 2.0 * baseSize * (_value.X - xs) /
                            (gridsExt.XMax - gridsExt.XMin) ;
      dTurn := -cameraPositionEx.X + (dRadius * Sin(DegToRad(spinZ))*
                                              Sin(DegToRad(spinX))) ;
      cameraPositionEx.Y := 2.0 * baseSize * (_value.Y - ys) /
                            (gridsExt.XMax - gridsExt.XMin) ;
      dFly  :=  cameraPositionEx.Y + (dRadius * Cos(DegToRad(spinZ))*
                                              Sin(DegToRad(spinX))) ;
      cameraPositionEx.Z := dRadius * Cos(DegToRad(spinX)) ;
      cameraPositionEx.M := 0.0 ;
      SetReferencePointOffset(GetReferencePointOffset - dz) ;
      exit;
    end;

    cameraPositionEx.X := 2.0 * baseSize * (_value.X - xs) /
                          (gridsExt.XMax - gridsExt.XMin) ;
    dTurn := -cameraPositionEx.X + (dRadius * Sin(DegToRad(spinZ))*
                                            Sin(DegToRad(spinX))) ;

    cameraPositionEx.Y := 2.0 * baseSize * (_value.Y - ys) /
                          (gridsExt.XMax - gridsExt.XMin) ;
    dFly  :=  cameraPositionEx.Y + (dRadius * Cos(DegToRad(spinZ))*
                                            Sin(DegToRad(spinX))) ;

    cameraPositionEx.M := 0.0 ;
  end;

  function  TGIS_Renderer3DAbstract.checkCSWGS : Boolean ;
  begin
    Result := False ;
    if oGIS.CS is TGIS_CSGeographicCoordinateSystem then begin
      Result := True ;
      exit;
    end;
    {$IFDEF CLR}
    if GisIsContainExtent( exExtent, TGIS_Utils.GisExtent( -180, -90, 180, 90 ))
    {$ELSE}
    if GisIsContainExtent( exExtent, GisFunctions.GisExtent( -180, -90, 180, 90 ) )
    {$ENDIF}
    then begin
      Result := True ;
      exit;
    end;
  end;

  function TGIS_Renderer3DAbstract.GetCameraRotation
    : TGIS_Point3D ;
  var
    cr : TGIS_Point3D ;
  begin
    cr.X := DegToRad( cameraRotation.X ) ;
    cr.Y := DegToRad( cameraRotation.Y ) ;
    cr.Z := DegToRad( cameraRotation.Z ) ;
    cr.M := cameraRotation.M  ;
    Result := cr ;
  end;

  procedure TGIS_Renderer3DAbstract.setCameraRotationX(
    const _value : Double
  ) ;
  var
    val                 : Double  ;
    rad, t1, f1, l      : Double  ;
    spx                 : Double  ;
  begin
    l := 500.0 ;
    val  := RadToDeg( _value ) ;  // new cameraRotation.X
    if val <= -90 then val := -89.9 ;
    if val >   90 then val :=  89.9   ;
    spx := 90 - val ; // potential new spinX

    if spx <> spinX then
      cspinX := spx - spinX
    else
      cspinX := 0 ;

    if cspinX >= 90 then
      cspinX := 89.90 ;

    basicEyeTo.Y := 0 ;
    if cspinX <> 0 then begin
      if spinX = 90 then
        rad := dRadius
      else
        rad := dRadius * Cos( DegToRad( spinX )) / Cos( DegToRad( spx ));
      t1 := rad * Sin(DegToRad(spinZ))* Sin(DegToRad(spx)) +
           dTurn - dRadius * Sin(DegToRad(spinZ))* Sin(DegToRad(spinX));
      f1 := dFly - dRadius * Cos(DegToRad(spinZ))* Sin(DegToRad(spinX))+
           rad * Cos(DegToRad(spinZ))* Sin(DegToRad(spx)) ;
      if ((t1>l) or (t1<-l) or (f1>l) or (f1<-l) or (spinX = 90))
        and (cspinX > 0) then begin
          basicEyeTo.Y := - dRadius * Tan( DegToRad ( cspinX)) ;
          basicEyeTo.Y := reverseValue( basicEyeTo.Y ) ;
      end
      else begin
        if (basicEyeTo.Y = 0) and (spx < 90) then begin
          dRadius := rad ;
          spinX  := spx ;
          cspinX := 0 ;
        end
        else begin
          basicEyeTo.Y := - dRadius * Tan( DegToRad ( cspinX)) ;
          basicEyeTo.Y := reverseValue( basicEyeTo.Y ) ;
        end;
      end;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.SetCameraRotation(
    const _value : TGIS_Point3D
  ) ;
  var
    val : Double  ;
  begin
    spinY  := RadToDeg( _value.Y ) ;
    cameraRotation.Y   := spinY  ;

    spinZ := -RadToDeg( _value.Z ) ;
    spinZ :=  truncateTo360( spinZ ) ;
    cameraRotation.Z := spinZ ;

    val  := RadToDeg( _value.X ) ;  // new cameraRotation.X
    if val <= -90 then
      val := -89.9 ;
    if val >=   90 then
      val :=  89.9   ;

    setCameraRotationX(DegToRad(val)) ;

    cameraRotation.X   := 90 - (spinX + cspinX ) ;
    cameraFocus := _value.M ;
    dTurn   := -cameraPositionEx.X + dRadius * Sin(DegToRad(spinZ))*
              Sin(DegToRad(spinX)) ;
    dFly    :=  cameraPositionEx.Y + dRadius * Cos(DegToRad(spinZ))*
              Sin(DegToRad(spinX)) ;
  end;

  function  TGIS_Renderer3DAbstract.GetCameraRotationEx  : TGIS_Point3D ;
  begin
    Result.X := spinX ;
    Result.Y := spinY ;
    Result.Z := spinZ ;
    Result.M := cspinX ;
  end;

  function TGIS_Renderer3DAbstract.GetSunPosition
    : TGIS_Point ;
  begin
     Result := sunPosition ;
  end;

  function TGIS_Renderer3DAbstract.GetKeepSunCamera
    : Boolean ;
  begin
     Result := keepSunCamera ;
  end;

  procedure TGIS_Renderer3DAbstract.SetKeepSunCamera(const _value : Boolean);
  begin
    keepSunCamera := _value;
  end;

  function TGIS_Renderer3DAbstract.truncateTo2Pi(
    const _value: Double
  ) : Double ;
  var
    value  : Double ;
    val2pi : Double ;
  begin
    val2pi := 2 * Pi ;
    value := _value ;
    if value > val2pi then begin
      while True do begin
        value := value - val2pi ;
        if value < val2pi then
          break;
      end;
      Result := value ;
      exit ;
    end;
    if value < -val2pi then begin
      while True do begin
        value := value + val2pi ;
        if value > -val2pi then
          break ;
      end;
      Result := value ;
      exit ;
    end;
    if value < 0 then
      value := val2pi + value;
    Result := value ;
  end;

  function TGIS_Renderer3DAbstract.truncateTo360(
    const _value: Double
  ) : Double ;
  var
    value : Double ;
  begin
    value := _value ;
    if value > 360.0 then begin
      while True do begin
        value := value - 360 ;
        if value < 360 then
          break;
      end;
      Result := value ;
      exit ;
    end;
    if value < -360.0 then begin
      while True do begin
        value := value + 360 ;
        if value > -360 then
          break ;
      end;
      Result := value ;
      exit ;
    end;
    if value < 0 then
      value := 360 + value;
    Result := value ;
  end;

  function TGIS_Renderer3DAbstract.GetShadowsLevel
    : Integer ;
  begin
    Result := TruncS(shadowsLevel*100) ;
  end;

  procedure TGIS_Renderer3DAbstract.SetShadowsLevel(
    const _value : Integer
  ) ;
  var
    val : Integer;
  begin
    shadowsLevel := _value ;
    if shadowsLevel < 0 then
      shadowsLevel := 0
    else
    if shadowsLevel > 100 then
      shadowsLevel := 1
    else
      shadowsLevel := _value / 100.0 ;

    val := TruncS (shadowsLevel * 255) ;
    setAmbientLightColor( val ) ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetDemWallType
    : TGIS_Viewer3DDemWall;
  begin
    Result := demWall ;
  end;

  procedure TGIS_Renderer3DAbstract.SetDemWallType(
    const _value : TGIS_Viewer3DDemWall
  ) ;
  begin
    demWall       := _value ;
    bMustReRender := True   ;
    ReRender ;
  end;

  function TGIS_Renderer3DAbstract.GetTexture
    : Boolean ;
  begin
    Result := imageTexture ;
  end;

  procedure TGIS_Renderer3DAbstract.SetTexture(
    const _value : Boolean
  ) ;
  begin
    if noDem then exit ;
    imageTexture  := _value ;
    bMustReRender := True ;
    ReRender ;
  end;

  function TGIS_Renderer3DAbstract.GetIsolineGap
    : Double ;
  begin
    Result := isolineGap ;
  end;

  procedure TGIS_Renderer3DAbstract.SetIsolineGap(
    const _value : Double
  ) ;
  begin
    if _value <=0 then
      isolineGap   := 0
    else
      isolineGap   := _value ;

    makeWallTexture ;
  end;

  function TGIS_Renderer3DAbstract.GetIsolineColor
    : TGIS_Color ;
  begin
    Result := isolineColor ;
  end;

  procedure TGIS_Renderer3DAbstract.SetIsolineColor(
    const _value : TGIS_Color
  ) ;
  begin
    isolineColor := _value ;
    makeWallTexture ;
  end;

  procedure TGIS_Renderer3DAbstract.SetSolidWallColor(
    const _value : TGIS_Color
  ) ;
  begin
    solidWallColor := _value ;
    makeWallTexture ;
  end;

  function TGIS_Renderer3DAbstract.GetFlood
    : TGIS_Viewer3DFlood ;
  begin
    Result := flood ;
  end;

  procedure TGIS_Renderer3DAbstract.SetFlood(
    const _value : TGIS_Viewer3DFlood
  ) ;
  begin
    flood.Active := _value.Active ;
    flood.Level  := _value.Level  ;
    flood.Color  := _value.Color  ;
    flood.Transparency := _value.Transparency ;
    if flood.Transparency <= 0 then
      flood.Transparency := 0 ;
    if flood.Transparency >= 100 then
      flood.Transparency := 100 ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetBasePlane
    : TGIS_Viewer3DBasePlane ;
  begin
    Result := basePlane ;
  end;

  procedure TGIS_Renderer3DAbstract.SetBasePlane(
    const _value : TGIS_Viewer3DBasePlane
  ) ;
  begin
    if (basePlane.BackgroundColor <> _value.BackgroundColor ) or
       (basePlane.GridColor       <> _value.GridColor       ) or
       (basePlane.Transparency    <> _value.Transparency    ) then
    begin
      basePlane.BackgroundColor := _value.BackgroundColor ;
      basePlane.GridColor       := _value.GridColor ;
      basePlane.Transparency    := _value.Transparency ;
      makeBasementTex ;
    end;
    basePlane.Active  := _value.Active ;
    basePlane.Level   := _value.Level  ;
    setDefaultZ ;
    if not bMustReRender then
      bMustReRender := True ;
    optionSelected := True ;
  end;

  function TGIS_Renderer3DAbstract.GetVisibleExtent3D
    : TGIS_Extent3D ;
  begin
    Result := GisExtent3D(
                GetOriginPoint.X - 0.5 * windowWidth  * GetPixelSize.X,
                GetOriginPoint.Y - 0.5 * windowHeight * GetPixelSize.Y,
                rangeExtent.ZMin,
                GetOriginPoint.X + 0.5 * windowWidth  * GetPixelSize.X,
                GetOriginPoint.Y + 0.5 * windowHeight * GetPixelSize.Y,
                rangeExtent.ZMax
              ) ;
  end;

  function TGIS_Renderer3DAbstract.GetVisibleExtent
    : TGIS_Extent ;
  begin
    Result := GisExtent2DFrom3D(  GetVisibleExtent3D ) ;
  end;

  procedure TGIS_Renderer3DAbstract.SetVisibleExtent(
    const _value : TGIS_Extent
  ) ;
  var
    ext : TGIS_Extent ;
    vext : TGIS_Extent ;
    pnt1, pnt2, cam : TGIS_Point3D ;
    dx, dy : Double ;
    dx1, dx2 : Double ;
  begin
    ext :=  _value ;
    cam := GetCameraPositionEx ;
    pnt1 := GetOriginPoint ;
    pnt2.X := ( ext.XMax + ext.XMin ) / 2.0 ;
    pnt2.Y := ( ext.YMax + ext.YMin ) / 2.0 ;

    dx := pnt2.X - pnt1.X ;
    dy := pnt2.Y - pnt1.Y ;

    vext := GetVisibleExtent ;
    dx1 := ext.XMax - ext.XMin ;
    dx2 := vext.XMax - vext.XMin ;


    cam.X := cam.X + dx ;
    cam.Y := cam.Y + dy ;
    SetCameraPositionEx( cam ) ;
    cam := GetCameraPosition ;
    cam.Z := cam.Z * dx1 / dx2 ;
    SetCameraPosition( cam ) ;
    ReRender ;
  end;

  function TGIS_Renderer3DAbstract.GetRestriction
    : TGIS_Viewer3DViewRestriction ;
  begin
    Result := viewRestriction ;
  end;

  procedure TGIS_Renderer3DAbstract.SetRestriction(
    const _value : TGIS_Viewer3DViewRestriction
  ) ;
  begin
    viewRestriction := _value ;
  end;

  function TGIS_Renderer3DAbstract.GetDemTransparency
    : Boolean ;
  begin
    Result := allowDemTransp ;
  end;

  procedure TGIS_Renderer3DAbstract.SetDemTransparency(
    const _value : Boolean
  ) ;
  begin
    allowDemTransp := _value ;
  end;

  function TGIS_Renderer3DAbstract.GetTranspPrior
    : TGIS_Viewer3DTransparencyPriority ;
  begin
    Result := transpPrior ;
  end;

  procedure TGIS_Renderer3DAbstract.SetTranspPrior(
    const _value : TGIS_Viewer3DTransparencyPriority
  ) ;
  begin
    transpPrior := _value ;
  end;

  function TGIS_Renderer3DAbstract.GetErrorMsg
    : String ;
  begin
    Result := errorMsg ;
  end;

  function TGIS_Renderer3DAbstract.fget_DemDetailExtentFactor
    : Double ;
  begin
    Result := dDemDetailExtentFactor ;
  end;

  procedure TGIS_Renderer3DAbstract.fset_DemDetailExtentFactor (
    const _value : Double
  ) ;
  var
    val : Double ;
  begin
    val := _value ;
    if val < 0.5 then val := 0.5 ;
    if val > 2.0 then val := 2.0 ;
    dDemDetailExtentFactor := val ;
    detailCoeff := val ;
  end;

  function TGIS_Renderer3DAbstract.fget_DemDraftExtentFactor
    : Double ;
  begin
    Result := dDemDraftExtentFactor ;
  end;

  procedure TGIS_Renderer3DAbstract.fset_DemDraftExtentFactor (
    const _value : Double
  ) ;
  var
    val : Double ;
  begin
    val := _value ;
    if val < 1.0 then val := 1.0 ;
    dDemDraftExtentFactor := val ;
    roughCoeff := val ;
  end;

  function TGIS_Renderer3DAbstract.fget_VectorExtentFactor
    : Double ;
  begin
    Result := dVectorExtentFactor ;
  end;

  procedure TGIS_Renderer3DAbstract.fset_VectorExtentFactor (
    const _value : Double
  ) ;
  var
    val : Double ;
  begin
    val := _value ;
    if val < 1.0 then val := 1.0 ;
    dVectorExtentFactor := val ;
    vectorCoeff := val ;
  end;

  function TGIS_Renderer3DAbstract.fget_VectorSimplification
    : Boolean ;
  begin
    Result := bVectorSimplification ;
  end;

  procedure TGIS_Renderer3DAbstract.fset_VectorSimplification (
    const _value : Boolean
  ) ;
  begin
    bVectorSimplification := _value ;
  end;

  function TGIS_Renderer3DAbstract.fget_VectorSmartSize
    : Integer ;
  begin
    Result := iVectorSmartSize ;
  end;

  procedure TGIS_Renderer3DAbstract.fset_VectorSmartSize (
    const _value : Integer
  ) ;
  var
    val : Integer ;
  begin
    val := _value ;
    if val < 0 then val := 0 ;
    iVectorSmartSize := val ;
  end;

  function TGIS_Renderer3DAbstract.fget_DemGridSize
    : Integer ;
  begin
    Result := iDemGridSize ;
  end;

  function TGIS_Renderer3DAbstract.fget_DemCachedSize
    : TGIS_Viewer3DDemCacheSize ;
  begin
    Result := enDemCachedSize ;
  end;

  procedure TGIS_Renderer3DAbstract.fset_DemCachedSize(
    const _value : TGIS_Viewer3DDemCacheSize
  ) ;
  var
    oldframesize : Integer ;
  begin
    oldframesize   := vertexBufferSize ;
    enDemCachedSize := _value ;
    if (noDem = True) and (noImg = True) then begin
      enDemCachedSize := TGIS_Viewer3DDemCacheSize.Medium ;
      vertexBufferSize := 512 ;
    end
    else
    case  enDemCachedSize of
      TGIS_Viewer3DDemCacheSize.Medium : vertexBufferSize :=  512 ;
      TGIS_Viewer3DDemCacheSize.Window :
        begin
          vertexBufferSize := TruncS(detailCoeff*Max(windowWidth,windowHeight)) ;
         if vertexBufferSize > 2048 then vertexBufferSize := 2048 ;
        end;
    end;
    iDemGridSize := vertexBufferSize ;
    imgFrameSize := Max(windowWidth,windowHeight) ;
    scrRatio := ( 1.0 * windowHeight )/windowWidth ;
    adjustRatio := scrRatio ;
  end;

  function TGIS_Renderer3DAbstract.fget_IsBusy
    : Boolean ;
  begin
    if (inPaint = True) or (inPaint1 = True) then
      Result := True
    else
      Result := False ;
  end;

  function TGIS_Renderer3DAbstract.fget_MapRefreshLock
    : Boolean ;
  var
    lv : TGIS_LayerVector ;
  begin
    Result := False ;
    if windowSizeChanged then exit;
    if (curDemSize + curVLSize) > 1 then exit;
    if not bFastMode then exit ;
    if curVLSize = 1 then begin
        lv := curVL[0].Lv;
        if lv.GetLastUid = 1 then
          Result := True ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.init_arGrid(
    var _array    : TGIS_GridArray
  ) ;
  var
    i, j : Integer ;
  begin
    for i := 0 to demFrameSize -1 do
      for j := 0 to demFrameSize -1 do
        _array[i][j] := noDataValue ;
  end;

  procedure TGIS_Renderer3DAbstract.setLeftColumn ;
  var
    dx, dy    : Double ;
    dx1, dy1  : Double ;
    diff, val : Double ;
    wrk_ext   : TGIS_Extent ;
    i         : Integer ;
  begin
    xMin := -baseSize ;
    xMax :=  baseSize ;
    yMin := -baseSize * grdRatio ;
    yMax :=  baseSize * grdRatio ;

    scrExtent := GisExtent( -maxExt, -maxExt, maxExt, maxExt ) ;

    dx := gridsExt.XMax - gridsExt.XMin ;
    dy := gridsExt.YMax - gridsExt.YMin ;

    dx1 := xMax - xMin ;
    dy1 := yMax - yMin ;

    wrk_ext := GisExtent( gridsExt.XMin - dx * ( xMin - scrExtent.XMin ) / dx1,
                          gridsExt.YMax - dy * ( scrExtent.YMax - yMin ) / dy1,
                          gridsExt.XMin + dx * ( scrExtent.XMax - xMin ) / dx1,
                          gridsExt.YMax + dy * ( yMin - scrExtent.YMin ) / dy1
                         ) ;

    diff := (wrk_ext.XMax - wrk_ext.XMin)/(demFrameSize-1) ;
    for i := 0 to demFrameSize -1 do begin
      val := wrk_ext.XMin + i*diff ;
      if val >= gridsExt.XMin then
        break ;
    end;
    leftColumn := i ;
  end;

  function  TGIS_Renderer3DAbstract.GetIgnoreAbove
    : Double ;
  begin
    Result := dIgnoreAbove ;
  end;

  procedure  TGIS_Renderer3DAbstract.SetIgnoreAbove (
    const _value : Double
  ) ;
  begin
    if dIgnoreBelow < _value then begin
      dIgnoreAbove := _value ;
      bDefaultIgnoreCut := False ;
    end;
  end;

  function  TGIS_Renderer3DAbstract.GetIgnoreBelow
    : Double ;
  begin
    Result := dIgnoreBelow ;
  end;

  procedure  TGIS_Renderer3DAbstract.SetIgnoreBelow (
    const _value : Double
  ) ;
  begin
    if dIgnoreAbove > _value then begin
      dIgnoreBelow := _value ;
      bDefaultIgnoreCut := False ;
    end;
  end;

  function  TGIS_Renderer3DAbstract.GetCutAbove
    : Double ;
  begin
    Result := dCutAbove ;
  end;

  procedure  TGIS_Renderer3DAbstract.SetCutAbove (
    const _value : Double
  ) ;
  begin
    if dCutBelow < _value then begin
      dCutAbove := _value ;
      bDefaultIgnoreCut := False ;
    end;
  end;

  function  TGIS_Renderer3DAbstract.GetCutBelow
    : Double ;
  begin
    Result := dCutBelow ;
  end;

  procedure  TGIS_Renderer3DAbstract.SetCutBelow (
    const _value : Double
  ) ;
  begin
    if dCutAbove > _value then begin
      dCutBelow := _value ;
      bDefaultIgnoreCut := False ;
    end;
  end;

  function  TGIS_Renderer3DAbstract.GetFastMode
   : Boolean ;
  begin
    Result := bFastMode ;
  end;

  procedure TGIS_Renderer3DAbstract.SetFastMode (
    const _value         : Boolean
  ) ;
  begin
    if (not _value) and bFastMode then begin
      bFastMode := _value ;
      UpdateWholeMap ;
    end
    else
      bFastMode := _value ;
  end;

  function  TGIS_Renderer3DAbstract.GetLightVector
   : Boolean ;
  begin
    Result := bLightVector ;
  end;

  procedure TGIS_Renderer3DAbstract.SetLightVector (
    const _value         : Boolean
  ) ;
  begin
    bLightVector := _value ;
  end;

  function  TGIS_Renderer3DAbstract.GetIgnoreEllipsoidHeight
    : Boolean  ;
  begin
    Result := bIgnoreEllipsoidHeight ;
  end;

  procedure TGIS_Renderer3DAbstract.SetIgnoreEllipsoidHeight (
    const _value : Boolean
  ) ;
  begin
    bIgnoreEllipsoidHeight := _value ;
    lockReadingData := 0;
    wasReferenceSet := False ;
    Repaint ; //? Draw ;
    Drag3DEx(GisPoint3D(0,0,0,0)) ;
    bMustReRender := True ;
    ReRender ;
  end;

  procedure TGIS_Renderer3DAbstract.Drag3D(
    const _delta : TGIS_Point3D
  ) ;
  var
    dx, dy, dd : Double ;
  begin
    dd := 1.0 ;
    if dRadius/initRadius < 0.05 then
      dd := 0.05 * initRadius / dRadius ;
    dx := _delta.X / GetPixelSize.X ;
    dx := Abs( 2 * dx * wndXSize / windowWidth ) * dd ;
    dy := _delta.Y / GetPixelSize.Y;
    dy := Abs( 2 * dy * wndXSize / windowWidth ) * dd ;


    if _delta.X > 0 then begin
      dTurn := dTurn +  dx * Cos(DegToRad(spinZ)) ;
      dFly  := dFly  -  dx * Sin(DegToRad(spinZ)) ;
    end
    else begin
      dTurn := dTurn -  dx * Cos(DegToRad(spinZ)) ;
      dFly  := dFly  +  dx * Sin(DegToRad(spinZ)) ;
    end;
    if _delta.Y > 0 then begin
      dFly  := dFly  +  dy * Sin(DegToRad(spinZ+90)) ;
      dTurn := dTurn -  dy * Cos(DegToRad(spinZ+90)) ;
    end
    else begin
      dFly  := dFly  -  dy * Sin(DegToRad(spinZ+90)) ;
      dTurn := dTurn +  dy * Cos(DegToRad(spinZ+90)) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.Drag3DEx(
    const _delta : TGIS_Point3D
  ) ;
  var
    dx, dy, dd : Double ;
  begin
    if referencePointMode = TGIS_Viewer3DReferenceMode.FlyOnDem then begin
      Drag3D( _delta ) ;
      exit;
    end ;

    dd := 1.0 ;
    if dRadius/initRadius < 0.05 then
      dd := 0.05 * initRadius / dRadius ;
    dx := _delta.X / GetPixelSize.X ;
    dx := Abs( 2 * dx * wndXSize / windowWidth ) * dd ;
    dy := _delta.Y / GetPixelSize.Y;
    dy := Abs( 2 * dy * wndXSize / windowWidth ) * dd ;

    if (zFactor > 1) and (spinX <> 0) then
      dy := dy / Abs(zFactor * Sin(DegToRad(spinX))) ;

    if _delta.X > 0 then begin
      dTurn := dTurn +  dx * Cos(DegToRad(spinZ)) ;
      dFly  := dFly  -  dx * Sin(DegToRad(spinZ)) ;
    end
    else begin
      dTurn := dTurn -  dx * Cos(DegToRad(spinZ)) ;
      dFly  := dFly  +  dx * Sin(DegToRad(spinZ)) ;
    end;

    if _delta.Y > 0 then begin
      dTurn := dTurn -  dy * Cos(DegToRad(spinZ+90)) *
                           Sin(DegToRad(90-spinX)) ;
      dFly  := dFly  +  dy * Sin(DegToRad(spinZ+90)) *
                           Sin(DegToRad(90-spinX)) ;
      SetReferencePointOffset(GetReferencePointOffset +
                              zUnit*dy*Cos(DegToRad(90-spinX)));
    end
    else begin
      dTurn := dTurn +  dy * Cos(DegToRad(spinZ+90)) *
                           Sin(DegToRad(90-spinX)) ;
      dFly  := dFly  -  dy * Sin(DegToRad(spinZ+90)) *
                           Sin(DegToRad(90-spinX)) ;
      SetReferencePointOffset(GetReferencePointOffset -
                              zUnit*dy*Cos(DegToRad(90-spinX)));
    end;
  end;

  procedure TGIS_Renderer3DAbstract.Move3D(
    const _delta : TGIS_Point3D
  ) ;
  begin
    if _delta.X <> 0 then
      dTurn := dTurn -  _delta.X * (roughExtent.XMax - roughExtent.XMin) /
        ( gridsExt.XMax - gridsExt.XMin ) ;
    if _delta.Y <> 0 then
      dFly  := dFly  +  _delta.Y * (roughExtent.XMax - roughExtent.XMin) /
        ( gridsExt.XMax - gridsExt.XMin ) ;
  end;

  procedure TGIS_Renderer3DAbstract.Rotate3D(
    const _delta : TGIS_Point3D
  ) ;
  begin
    if _delta.X <> 0 then
      spinX := spinX + RadToDeg( _delta.X) ;
    spinX := truncateTo360( spinX ) ;
    if spinX < 0 then
      spinX := 0 ;
    if viewRestriction = TGIS_Viewer3DViewRestriction.AboveHorizon then
    if spinX > 90 then
      spinX := 90 ;

    if _delta.Y <> 0 then
      spinY := spinY + RadToDeg( _delta.Y) ;
    spinY := truncateTo360( spinY ) ;
    if spinY < -90 then
      spinY := -90 ;
    if spinY >  90 then
      spinY :=  90 ;

    if _delta.Z <> 0 then
      spinZ := spinZ + RadToDeg( _delta.Z) ;
    spinZ := truncateTo360( spinZ ) ;
  end;

  procedure TGIS_Renderer3DAbstract.LockTimer(
    const _value : Boolean
  ) ;
  begin
    oTimer.Enabled := _value ;

    if not drawArrow then
      bArrow := False
    else
      bArrow := True ;
  end;

  procedure TGIS_Renderer3DAbstract.Lock ;
  begin
    assert( generalLock >= 0 ) ;
    inc( generalLock );
  end;

  procedure TGIS_Renderer3DAbstract.Unlock ;
  begin
    assert( generalLock > 0);
    dec( generalLock );

    if generalLock < 0 then
      generalLock := 0;

    if generalLock = 0 then
      Repaint ; //? Draw ;
  end;

  function TGIS_Renderer3DAbstract.ScreenToMap3D(
    const _pt : TPoint
  ) : TGIS_Point3D ;
  var
    ptg : TGIS_Point3D       ;
    la  : TGIS_LayerAbstract ;
    shp : TGIS_ShapeAbstract ;
    prt : Integer            ;
  begin
    la := nil ;
    Result.X := 0 ;
    Result.Y := 0 ;
    Result.Z := 0 ;
    if Select3DObject( _pt, 0, la, ptg, shp, prt, False ) then
      Result := ptg ;
    Result.M := 0 ;
  end;

  procedure TGIS_Renderer3DAbstract.SetRadius(
    const _value : Double
  ) ;
  begin
    if _value = 0 then exit ;
    if dRadius / _value < 0.5 then exit ;

    dRadius := dRadius / _value ;
  end;

  procedure TGIS_Renderer3DAbstract.StoreMousePos(
    const _value : TPoint
  ) ;
  begin
    lastMouseDownPos := _value ;
  end;

  procedure TGIS_Renderer3DAbstract.ChangeRotationPoint(
    const _pt : TPoint
  ) ;
  begin
    if not newTransform then exit ;
    changeRotPoint := True ;
    ScreenToMap3D( _pt ) ;
    changeRotPoint := False ;
  end;

  procedure TGIS_Renderer3DAbstract.NewRotationPoint(
    const _value : Boolean
  ) ;
  begin
    adjustTransformSetting( _value ) ;
  end;

  procedure TGIS_Renderer3DAbstract.ResetView ;
  begin
    dTurn    := initTurn ;
    dFly     := initFly ;
    dRadius  := initRadius ;

    basicEyeAt.X := 0.0     ;
    basicEyeAt.Y := 0.01    ;
    basicEyeAt.Z := dRadius ;
    basicEyeTo.X := 0.0     ;
    basicEyeTo.Y := 0.0     ;
    basicEyeTo.Z := 0.0     ;
    zEyeTo       := 0.0     ;
    zOffset      := 0.0     ;

    spinX   := 0.0 ;
    spinY   := 0.0 ;
    spinZ   := 0.0 ;
    cspinX  := 0.0 ;
    zFactor := 1 ;
    mFactor := 1 ;

    // Camera settings
    cameraFocus := 50 ;
    cameraViewAngle := 1700.0 / Power(cameraFocus + 5.0, 0.89 ) - 2.0 ;
    cameraHalfViewAngle := 0.5 * cameraViewAngle ;
    cameraPosition.Y := 180;

    setIgnoreCutValues;
    Repaint ; //? Draw ;
  end;

  procedure TGIS_Renderer3DAbstract.setIgnoreCutValues ;
  var
    val : Double ;
  begin
    if (not reRenderCall) or (not bDefaultIgnoreCut) then exit;
    val := Abs( zLevelMax + zLevelMin ) / 2  ;
    dIgnoreAbove := zLevelMax + val ;
    dCutAbove    := dIgnoreAbove ;
    dIgnoreBelow := zLevelMin - val ;
    dCutBelow    := dIgnoreBelow ;
  end;

  function TGIS_Renderer3DAbstract.GetPointOnDEM
    : Double ;
  var
    valx, valy : Double       ;
    val        : Double       ;
    pnt        : TGIS_Point3D ;
    jj, kk     : Integer      ;
    wrk_ext    : TGIS_Extent  ;
  begin
    if noDem then begin
      Result := 0;
      exit ;
    end;
    if roughExtent.XMin = roughExtent.XMax then begin
      Result := 0;
      exit ;
    end;

    wrk_ext := traceExtent ;
    pnt := GetOriginPoint ;

    valx := (( wrk_ext.XMax - pnt.X) /( wrk_ext.XMax - wrk_ext.XMin)) ;
    valy := (( wrk_ext.YMax - pnt.Y) /( wrk_ext.YMax - wrk_ext.YMin)) ;

    //get Z value from DEM
    kk := demFrameSize - TruncS(valx * demFrameSize) ;
    jj := TruncS(valy * demFrameSize) ;
    if (jj<0) or (jj>=demFrameSize-1) or (kk<0) or (kk>=demFrameSize-1) then
      val := defaultZ
    else
      val := zValue(kk, jj) ;

    // height in GIS units
    Result := val * zUnit / zScaleFactor + zLevel ;
  end;

  function  TGIS_Renderer3DAbstract.GetFps
    : Integer ;
  begin
    Result := iFps ;
  end;

  function  TGIS_Renderer3DAbstract.Locate(
    const _pt       : TPoint             ;
    const _prec     : Integer
  ) : TGIS_ShapeAbstract ;
  var
    _layer    : TGIS_LayerAbstract ;
    _shp      : TGIS_ShapeAbstract ;
    _ptg      : TGIS_Point3D       ;
    _part     : Integer            ;
  begin
    _layer := nil ;
    Select3DObject( _pt, _prec, _layer, _ptg, _shp , _part, True );
    Result :=  _shp ;
  end;

  function  TGIS_Renderer3DAbstract.Locate3D(
    const _pt       : TPoint             ;
    const _prec     : Integer            ;
    var   _layer    : TGIS_LayerAbstract ;
    var   _ptg      : TGIS_Point3D       ;
    var   _shp      : TGIS_ShapeAbstract ;
    var   _part     : Integer
  ) : Boolean ;
  begin
    Result := Select3DObject( _pt, _prec, _layer, _ptg, _shp, _part, True );
  end;

  procedure TGIS_Renderer3DAbstract.MarkShape(
    const _layer : TGIS_Layer ;
    const _shpid : Integer    ;
    const _part  : Integer    ;
    const _color : TGIS_Color ;
    const _update: Boolean
  ) ;
  begin
    markSelShape( _layer, _shpid, _part, _color, True, _update ) ;
  end ;

  procedure TGIS_Renderer3DAbstract.UnMarkShape(
    const _layer : TGIS_Layer ;
    const _shpid : Integer    ;
    const _update: Boolean
  ) ;
  begin
    markSelShape( _layer, _shpid, -1, TGIS_Color.FromARGB(255,255,255,255), False, _update ) ;
  end ;

  procedure TGIS_Renderer3DAbstract.UpdateAllSelectedObjects ;
  var
    i,j,k,l: Integer ;
    lv  : TGIS_LayerVector ;
    fuid, uid : TGIS_Uid ;
    {$IFNDEF CLR}
      elm : TPair< TGIS_Uid, Boolean > ;
    {$ENDIF}
  begin
    for i:=0 to oGIS.Items.Count -1 do begin
      if not ( oGIS.Items[i] is TGIS_LayerVector ) then
        continue;

      lv := TGIS_LayerVector( oGIS.Items[i] ) ;
      // unmark all shapes on layer if any has been marked so far
      for j := 0 to bufSelInfoCounter -1 do begin    //for all buffers
        if arBufSelInfo[j].Layer = lv then begin
          fuid := -1 ;
          for k := 0 to arBufSelInfo[j].ShpInfo.Count -1 do begin
            if arBufSelInfo[j].ShpInfo[k].Selected = True then begin
              uid := arBufSelInfo[j].ShpInfo[k].Uid ;
              if fuid <> uid then begin
                markSelShape(lv, uid, -1, TGIS_Color.FromARGB(255,255,255,255), False, False ) ;
                fuid := uid ;
              end ;
            end;
          end;
        end;
      end;

      // mark all currently selected shapes
      for l := 0 to lv.SelectedList.Count -1 do begin
        uid  := lv.SelectedList[l] ;
        markSelShape(lv, uid, -1, oGIS.SelectionGisColor, True, False ) ;
      end;
//      Repaint ;
    end;
    Repaint ;
  end;

  procedure  TGIS_Renderer3DAbstract.UpdateWholeMap ;
  begin
    if MapRefreshLock then begin
      Repaint ;
      exit;
    end;
    oTimer.Enabled := False ;
    ReRender ;
  end;

  procedure  TGIS_Renderer3DAbstract.UpdateTopmost ;
  begin
    bPartialUpdate := True ;

    Repaint ;
  end;

  procedure TGIS_Renderer3DAbstract.Repaint ;
  begin
    if labelModeEx then begin
      labelMode := True ;
      oViewer.ControlRepaint ;
      if renderer3D = [TRenderer3D.FMX] then
        oViewer.Draw
      else
        labelMode := False ;
    end
    else
      oViewer.ControlRepaint ;
  end;

  procedure TGIS_Renderer3DAbstract.setExtent ;
  var
    ex_dx    : Double ;
    ex_dx_gr : Double ;
    x_gr_cnt,
    y_gr_cnt : Double ;
  begin
    ex_dx := exExtent.XMax-exExtent.XMin ;
    ex_dx_gr := ex_dx*grdRatio ;
    exExtent.YMax := exExtent.YMin + ex_dx_gr ;

    x_gr_cnt :=  (gridsExt.XMin + gridsExt.XMax) / 2.0 ;
    y_gr_cnt :=  (gridsExt.YMin + gridsExt.YMax) / 2.0 ;

    xxMin := xMin * (x_gr_cnt-exExtent.XMin)/(x_gr_cnt-gridsExt.XMin) ;
    xxMax := xMax * (exExtent.XMax-x_gr_cnt)/(gridsExt.XMax-x_gr_cnt) ;
    yyMin := yMin * (exExtent.YMax-y_gr_cnt)/(gridsExt.YMax-y_gr_cnt) ;
    yyMax := yMax * (y_gr_cnt-exExtent.YMin)/(y_gr_cnt-gridsExt.YMin) ;
  end;

  procedure TGIS_Renderer3DAbstract.setViewExtent(
    const _extent : TGIS_Extent;
    var _xval     : Double ;
    var _yval     : Double ;
    var _dx       : Double
  ) ;
  var
    ex_dx,
    ex_dy,
    ex_dx1 : Double ;
    x_cnt,
    y_cnt  : Double ;
  begin
    // View extent center
    x_cnt :=  (_extent.XMin + _extent.XMax) / 2.0 ;
    y_cnt :=  (_extent.YMin + _extent.YMax) / 2.0 ;
    // Entire extent range
    ex_dx := exExtent.XMax - exExtent.XMin ;
    ex_dy := exExtent.YMax - exExtent.YMin ;

    _xval := -(xMin+((x_cnt - exExtent.XMin)/ex_dx)*(xMax-xMin)) ;
    _yval := -(yMax-((y_cnt - exExtent.YMin)/ex_dy)*(yMax-yMin)) ;

    ex_dx1 := _extent.XMax-_extent.XMin ;
    _dx    := (ex_dx1/ex_dx)*(xMax-xMin) ;
  end;

  function TGIS_Renderer3DAbstract.getZUnit(
    const _value : TGIS_CSCoordinateSystem
  ) : Double ;
  var
    pcs           : TGIS_CSProjectedCoordinateSystem  ;
    gcs           : TGIS_CSGeographicCoordinateSystem ;
    distance      : Double ;
    distance_in_m : Double ;
    pt_a,
    pt_b          : TGIS_Point ;
    pt_a_rad,
    pt_b_rad      : TGIS_Point ;
  begin
    pt_a.X   := exExtent.XMin ;
    pt_a.Y   := 0.5 * (exExtent.YMax + exExtent.YMin) ;
    pt_b.X   := exExtent.XMax ;
    pt_b.Y   := 0.5 * (exExtent.YMax + exExtent.YMin) ;

    pt_a := oGIS.CS.ToCS( _value, pt_a ) ;
    pt_b := oGIS.CS.ToCS( _value, pt_b ) ;

    if _value is TGIS_CSProjectedCoordinateSystem then begin
      pcs := TGIS_CSProjectedCoordinateSystem( _value ) ;
      distance := Sqrt( Sqr( pt_b.X - pt_a.X ) + Sqr( pt_b.Y - pt_a.Y ) ) ;
      distance_in_m := pcs.Units.ToBase( distance ) ;
      Result := 0.5 * distance_in_m / baseSize ;

      if oGIS.CS is TGIS_CSProjectedCoordinateSystem then begin
        pcs := TGIS_CSProjectedCoordinateSystem( oGIS.CS ) ;
        if pcs.Units.Factor <> 1 then
          Result := Result / pcs.Units.Factor ;
        Result := Result * unitDistance / distance_in_m  ;
      end;
    end
    else
    if _value is TGIS_CSGeographicCoordinateSystem then begin
      gcs := TGIS_CSGeographicCoordinateSystem( _value ) ;
      pt_a_rad := gcs.ToWGS( pt_a ) ;
      pt_b_rad := gcs.ToWGS( pt_b ) ;
      pt_b_rad.X := (pt_a_rad.X + pt_b_rad.X) / 2 ;
      pt_b_rad.Y := (pt_a_rad.Y + pt_b_rad.Y) / 2 ;
      distance_in_m := 2 * gcs.Datum.Ellipsoid.Distance( pt_a_rad, pt_b_rad ) ;
//      distance_in_m := gcs.Datum.Ellipsoid.Distance( pt_a_rad, pt_b_rad ) ;
      Result := 0.5 * distance_in_m / baseSize ;
      Result := Result * unitDistance / distance_in_m  ;
    end
    else begin
      // assume metric
      Result := 0.5*(exExtent.XMax-exExtent.XMin)/baseSize ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.setZUnit ;
  var
    pcs           : TGIS_CSProjectedCoordinateSystem  ;
    gcs           : TGIS_CSGeographicCoordinateSystem ;
    distance      : Double ;
    distance_in_m : Double ;
    pt_a,
    pt_b          : TGIS_Point  ;
    pt_a_rad,
    pt_b_rad      : TGIS_Point  ;
    ext           : TGIS_Extent ;
  begin
    if not (oGIS.CS is TGIS_CSUnknownCoordinateSystem) then begin
      gcs := CSGeographicCoordinateSystemList.ByEPSG( 4326 ) ;
      ext := oGIS.CS.ExtentToWGS( exExtent ) ;
      pt_a.X   := ext.XMin ;
      pt_a.Y   := 0.5 * (ext.YMax + ext.YMin) ;
      pt_b.X   := ext.XMax ;
      pt_b.Y   := 0.5 * (ext.YMax + ext.YMin) ;
      unitDistance := gcs.Datum.Ellipsoid.Distance( pt_a, pt_b ) ;
    end;

    pt_a.X   := exExtent.XMin ;
    pt_a.Y   := 0.5 * (exExtent.YMax + exExtent.YMin) ;
    pt_b.X   := exExtent.XMax ;
    pt_b.Y   := 0.5 * (exExtent.YMax + exExtent.YMin) ;

    if oGIS.CS is TGIS_CSProjectedCoordinateSystem then begin
      pcs := TGIS_CSProjectedCoordinateSystem( oGIS.CS ) ;
      distance := Sqrt( Sqr( pt_b.X - pt_a.X ) + Sqr( pt_b.Y - pt_a.Y ) ) ;
      distance_in_m := pcs.Units.ToBase( distance ) ;
      zUnit := 0.5 * distance_in_m / baseSize ;
    end
    else
    if oGIS.CS is TGIS_CSGeographicCoordinateSystem then begin
      gcs := TGIS_CSGeographicCoordinateSystem( oGIS.CS ) ;
      pt_a_rad := gcs.ToWGS( pt_a ) ;
      pt_b_rad := gcs.ToWGS( pt_b ) ;
      pt_b_rad.X := (pt_a_rad.X + pt_b_rad.X) / 2 ;
      pt_b_rad.Y := (pt_a_rad.Y + pt_b_rad.Y) / 2 ;
      distance_in_m := 2 * gcs.Datum.Ellipsoid.Distance( pt_a_rad, pt_b_rad ) ;
//      distance_in_m := gcs.Datum.Ellipsoid.Distance( pt_a_rad, pt_b_rad ) ;
      zUnit := 0.5 * distance_in_m / baseSize ;
    end
    else begin
      // If layer has an empty CS (like CAD drawings) then Z value should have
      // the same scaling as XY
      distance_in_m := exExtent.XMax-exExtent.XMin ;
      zUnit := 0.5*(exExtent.XMax-exExtent.XMin)/baseSize ;
      if unitDistance = 0 then
        unitDistance := 1 ;
    end;
    gisCSzUnit := distance_in_m / ( exExtent.XMax - exExtent.XMin ) ;
  end;

  procedure TGIS_Renderer3DAbstract.layersRangeSetting(
    const _mode : Boolean
  ) ;
  var
    i  : Integer                    ;
    la : TGIS_Layer                 ;
    lc : TGIS_LayerCompoundAbstract ;
    lp : TGIS_LayerProject          ;
    lx : TGIS_LayerPixel            ;
    a  : Double                     ;
  begin
    if _mode then begin
      curDemSize  := 0 ;
      curMeshSize := 0 ;
      curVLSize   := 0 ;
      SetLength( curDem , 0 ) ;
      SetLength( curMesh, 0 ) ;
      SetLength( curVL  , 0 ) ;
    end;
     // Search all opened layers
    for i:=0 to oGIS.Items.Count -1 do  begin
      la := TGIS_Layer( oGIS.Items[i] ) ;
      if not la.Active then continue ;

      // LayerPixel
      if la is TGIS_LayerPixel then begin
        lx := TGIS_LayerPixel ( oGIS.Items[i] ) ;
        if lx.View3D.Mode = TGIS_3DLayerType.Dem then  //? replace by DEM as Mesh layer porp. value
          layerPixelParams( la, lx, _mode )
        else
        if lx.View3D.Mode = TGIS_3DLayerType.Shapes then
          layerMeshParams( la, lx, _mode )
        else
          noImg := False
      end
      // Layer Project
      else if la is TGIS_LayerProject then begin
        lp := TGIS_LayerProject( oGIS.Items[i] ) ;
        layerProjectParams( la, lp, _mode ) ;
      end
      // Layer Vector
      else if la is TGIS_LayerVector then
        layerVectorParams( la, _mode )
      // Layer Compound
      else if la is TGIS_LayerCompoundAbstract then begin
        lc := TGIS_LayerCompoundAbstract( oGIS.Items[i] ) ;
        layerCompoundParams( la, lc, _mode ) ;
      end
    end;

    if zLevel > zLevelMax then begin
      a := zLevel ;
      zLevel := zLevelMax ;
      zLevelMax := a ;
    end;
    if noDem then begin
      if high(curVL) >=0 then
        zScaleFactor := zFactor * curVL[0].Zu
      else
        zScaleFactor := 1.0
    end
    else
      zScaleFactor := zFactor * curDem[0].Zu ;
  end;

  procedure TGIS_Renderer3DAbstract.layerPixelParams(
    const _la   : TGIS_Layer      ;
    const _lx   : TGIS_LayerPixel ;
    const _mode : Boolean
  ) ;
  var
    rob   : Double ;
    wrkz  : Double ;
    val   : Double ;
    ptg   : TGIS_Point3D ;
    nz  : Double ;

    procedure setHeightRange(
      const _zVal : Double
    ) ;
    begin
      if _zVal > heightRange then
        heightRange := _zVal ;
    end;

    procedure getIgnoreCutValues ;
    var
      pt  : TGIS_Point3D ;
    begin
      if bIgnoreEllipsoidHeight then begin
        if not bWasProjected then exit ;
        pt := GisPoint3D( 0.5*(_lx.ProjectedExtent.XMin +
                               _lx.ProjectedExtent.XMax),
                          0.5*(_lx.ProjectedExtent.YMin +
                               _lx.ProjectedExtent.YMax),
                          dIgnoreBelow
                        ) ;
        _lx.Unproject3D_Ref(pt) ;
        dIgnoreBelow := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.ProjectedExtent.XMin +
                               _lx.ProjectedExtent.XMax),
                          0.5*(_lx.ProjectedExtent.YMin +
                               _lx.ProjectedExtent.YMax),
                          dIgnoreAbove
                        ) ;
        _lx.Unproject3D_Ref(pt) ;
        dIgnoreAbove := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.ProjectedExtent.XMin +
                               _lx.ProjectedExtent.XMax),
                          0.5*(_lx.ProjectedExtent.YMin +
                               _lx.ProjectedExtent.YMax),
                          dCutBelow
                        ) ;
        _lx.Unproject3D_Ref(pt) ;
        dCutBelow := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.ProjectedExtent.XMin +
                               _lx.ProjectedExtent.XMax),
                          0.5*(_lx.ProjectedExtent.YMin +
                               _lx.ProjectedExtent.YMax),
                          dCutAbove
                        ) ;
        _lx.Unproject3D_Ref(pt) ;
        dCutAbove := pt.Z ;

        bWasProjected := False ;
      end
      else begin
        if bWasProjected then exit;
        pt := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                          0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                          dIgnoreBelow
                         ) ;
        _lx.Project3D_Ref(pt) ;
        dIgnoreBelow := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                          0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                          dIgnoreAbove
                         ) ;
        _lx.Project3D_Ref(pt) ;
        dIgnoreAbove := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                          0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                          dCutBelow
                         ) ;
        _lx.Project3D_Ref(pt) ;
        dCutBelow := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                          0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                          dCutAbove
                         ) ;
        _lx.Project3D_Ref(pt) ;
        dCutAbove := pt.Z ;

        bWasProjected := True ;
      end;
    end;

    function calculateNz : Double ;
    var
      val : Double ;
    begin
      Result := 1.0;
      val := _lx.MaxHeight - _lx.MinHeight ;

      if (zUnit = 0) or (val = 0) then
        exit ;

      val := val / zUnit ;
      if zUnit >= 1.0 then
        Result := Max(GISVIEWER3D_NORM_VALUE / val, val / GISVIEWER3D_NORM_VALUE )
      else
        Result := Min(GISVIEWER3D_NORM_VALUE / val, val / GISVIEWER3D_NORM_VALUE )
    end;

  begin
    if _mode and (not _la.Active) then
      exit ;

    if _la.View3D.Mode = TGIS_3DLayerType.Dem then begin
      noDem := False ;
      val := _lx.Params.ScaleZ ;
      ptg := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                         0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                         0
                        ) ;
      ptg.Z := _lx.MinHeight ;
{ TODO -cReview : check condition }
//?      if not bIgnoreEllipsoidHeight then
        _lx.Project3D_Ref(ptg) ;

      wrkz := ptg.Z * val + _lx.Params.FalseZ ;
      if wrkz < zLevel then
        zLevel := wrkz ;

      ptg := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                         0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                         0
                        ) ;
      ptg.Z := _lx.MaxHeight ;
{ TODO -cReview : check condition }
//?      if not bIgnoreEllipsoidHeight then
        _lx.Project3D_Ref(ptg) ;

      if _lx.Params.NormalizedZ <> TGIS_3DNormalizationType.Off then
        nz := calculateNz
      else
        nz := 1.0 ;

      wrkz := nz * ptg.Z * val + _lx.Params.FalseZ ;
      if wrkz > zLevelMax then
        zLevelMax := wrkz ;

      zLevelMin := zLevel ;

      getIgnoreCutValues ;

      if dIgnoreBelow >= dCutBelow then
        val := dIgnoreBelow
      else
        val := dCutBelow ;

      if (zLevel < val) and (zLevelMax >= val) then
        zLevel := val ;

      setHeightRange(zLevelMax - zLevel) ;

      rob := (_lx.ProjectedExtent.XMax - _lx.ProjectedExtent.XMin) /
                                                                _lx.CellWidth ;
      if xDemSize >  rob then
        xDemSize := rob ;

      rob := (_lx.ProjectedExtent.YMax - _lx.ProjectedExtent.YMin) /
                                                                _lx.CellHeight ;
      if yDemSize >  rob then
        yDemSize := rob ;

      if _mode then begin
        inc(curDemSize) ;
        SetLength(curDem, curDemSize) ;
        curDem[curDemSize-1].Lx  := _lx ;
        curDem[curDemSize-1].Zu  :=  zUnit/getZUnit( _lx.CS ) ;
        if _lx.Params.NormalizedZ <> TGIS_3DNormalizationType.Off then
          curDem[curDemSize-1].Nz  :=  nz
        else
          curDem[curDemSize-1].Nz  :=  1.0 ;
        curDem[curDemSize-1].Shd := TGIS_ParamsSectionPixel( _lx.Params ).Pixel.GridShadow ;
      end;

      if allowDemTransp then begin
        setDemTransparency1 (_lx.Transparency ) ;
        if demTransparency < curDemTransp   then
          curDemTransp := demTransparency
        else
          demTransparency := curDemTransp ;
      end
      else
        demTransparency := 255 ;
    end
    else begin
      noImg := False ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.layerMeshParams(
    const _la   : TGIS_Layer      ;
    const _lx   : TGIS_LayerPixel ;
    const _mode : Boolean
  ) ;
  var
    wrkz  : Double ;
    val   : Double ;
    ptg   : TGIS_Point3D ;
    nz  : Double ;

    procedure setHeightRange(
      const _zVal : Double
    ) ;
    begin
      if _zVal > heightRange then
        heightRange := _zVal ;
    end;

    procedure getIgnoreCutValues ;
    var
      pt  : TGIS_Point3D ;
    begin
      if bIgnoreEllipsoidHeight then begin
        if not bWasProjected then exit ;
        pt := GisPoint3D( 0.5*(_lx.ProjectedExtent.XMin +
                               _lx.ProjectedExtent.XMax),
                          0.5*(_lx.ProjectedExtent.YMin +
                               _lx.ProjectedExtent.YMax),
                          dIgnoreBelow
                        ) ;
        _lx.Unproject3D_Ref(pt) ;
        dIgnoreBelow := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.ProjectedExtent.XMin +
                               _lx.ProjectedExtent.XMax),
                          0.5*(_lx.ProjectedExtent.YMin +
                               _lx.ProjectedExtent.YMax),
                          dIgnoreAbove
                        ) ;
        _lx.Unproject3D_Ref(pt) ;
        dIgnoreAbove := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.ProjectedExtent.XMin +
                               _lx.ProjectedExtent.XMax),
                          0.5*(_lx.ProjectedExtent.YMin +
                               _lx.ProjectedExtent.YMax),
                          dCutBelow
                        ) ;
        _lx.Unproject3D_Ref(pt) ;
        dCutBelow := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.ProjectedExtent.XMin +
                               _lx.ProjectedExtent.XMax),
                          0.5*(_lx.ProjectedExtent.YMin +
                               _lx.ProjectedExtent.YMax),
                          dCutAbove
                        ) ;
        _lx.Unproject3D_Ref(pt) ;
        dCutAbove := pt.Z ;

        bWasProjected := False ;
      end
      else begin
        if bWasProjected then exit;
        pt := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                          0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                          dIgnoreBelow
                         ) ;
        _lx.Project3D_Ref(pt) ;
        dIgnoreBelow := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                          0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                          dIgnoreAbove
                         ) ;
        _lx.Project3D_Ref(pt) ;
        dIgnoreAbove := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                          0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                          dCutBelow
                         ) ;
        _lx.Project3D_Ref(pt) ;
        dCutBelow := pt.Z ;

        pt := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                          0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                          dCutAbove
                         ) ;
        _lx.Project3D_Ref(pt) ;
        dCutAbove := pt.Z ;

        bWasProjected := True ;
      end;
    end;

    function calculateNz : Double ;
    var
      val : Double ;
    begin
      Result := 1.0;
      val := _lx.MaxHeight - _lx.MinHeight ;

      if (zUnit = 0) or (val = 0) then
        exit ;

      val := val / zUnit ;
      if zUnit >= 1.0 then
        Result := Max(GISVIEWER3D_NORM_VALUE / val, val / GISVIEWER3D_NORM_VALUE )
      else
        Result := Min(GISVIEWER3D_NORM_VALUE / val, val / GISVIEWER3D_NORM_VALUE )
    end;

  begin
    if _mode and (not _la.Active) then
      exit ;

    nz := 0 ;
//    if _la.View3D.Mode = TGIS_3DLayerType.Dem then begin
    if _la.IsGrid then begin
      val := _lx.Params.ScaleZ ;
      ptg := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                         0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                         0
                        ) ;
      ptg.Z := _lx.MinHeight ;
      _lx.Project3D_Ref(ptg) ;

      wrkz := ptg.Z * val + _lx.Params.FalseZ ;
      if wrkz < zLevel then
        zLevel := wrkz ;

      ptg := GisPoint3D( 0.5*(_lx.Extent.XMin + _lx.Extent.XMax),
                         0.5*(_lx.Extent.YMin + _lx.Extent.YMax),
                         0
                        ) ;
      ptg.Z := _lx.MaxHeight ;
      _lx.Project3D_Ref(ptg) ;

      if _lx.Params.NormalizedZ <> TGIS_3DNormalizationType.Off then
        nz := calculateNz
      else
        nz := 1.0 ;

      wrkz := nz * ptg.Z * val + _lx.Params.FalseZ ;
      if wrkz > zLevelMax then
        zLevelMax := wrkz ;

      zLevelMin := zLevel ;

      getIgnoreCutValues ;

      if dIgnoreBelow >= dCutBelow then
        val := dIgnoreBelow
      else
        val := dCutBelow ;

      if (zLevel < val) and (zLevelMax >= val) then
        zLevel := val ;

      setHeightRange(zLevelMax - zLevel) ;

      if _mode then begin
        inc(curMeshSize) ;
        SetLength(curMesh, curMeshSize) ;
        curMesh[curMeshSize-1].Lx  := _lx ;
        curMesh[curMeshSize-1].Zu  :=  zUnit/getZUnit( _lx.CS ) ;
        if _lx.Params.NormalizedZ <> TGIS_3DNormalizationType.Off then
          curMesh[curMeshSize-1].Nz  :=  nz
        else
          curMesh[curMeshSize-1].Nz  :=  1.0 ;
      end;
    end
    else begin // Image
      if _mode then begin
        inc(curMeshSize) ;
        SetLength(curMesh, curMeshSize) ;
        curMesh[curMeshSize-1].Lx  := _lx ;
        curMesh[curMeshSize-1].Zu  :=  zUnit/getZUnit( _lx.CS ) ;
        if _lx.Params.NormalizedZ <> TGIS_3DNormalizationType.Off then
          curMesh[curMeshSize-1].Nz  :=  nz
        else
          curMesh[curMeshSize-1].Nz  :=  1.0 ;
      end;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.getShapeZMRange(
         _shp   : TGIS_Shape ;
    var  _abort : Boolean
    ) ;
    begin
      if curVL[curVLSize -1].Nz  < _shp.Params.FalseZ then begin
        curVL[curVLSize -1].Nz  := _shp.Params.FalseZ ;
        curVL[curVLSize -1].Zmx := _shp.Params.FalseZ ;
      end;
      if curVL[curVLSize -1].Zmi > _shp.Params.FalseZ then
        curVL[curVLSize -1].Zmi := _shp.Params.FalseZ ;

      if curVL[curVLSize -1].Nm < _shp.Params.FalseM then begin
        curVL[curVLSize -1].Nm  := _shp.Params.FalseM ;
        curVL[curVLSize -1].Mmx := _shp.Params.FalseM ;
      end;
      if curVL[curVLSize -1].Mmi > _shp.Params.FalseM then
        curVL[curVLSize -1].Mmi := _shp.Params.FalseM ;
    end;

    procedure TGIS_Renderer3DAbstract.getLayerZMRange ;
    var
      lv  : TGIS_LayerVector ;
    begin
      lv := curVL[curVLSize -1].Lv ;
      {$IFDEF CLR}
        if not lv.ForEach( lv.Extent, '', nil, '', True, oGIS.ScaleAsFloat,
                           @getShapeZMRange ) then exit ;
      {$ELSE}
        if not lv.ForEach( lv.Extent, '', nil, '', True, oGIS.ScaleAsFloat,
                            getShapeZMRange ) then exit ;
      {$ENDIF}
    end;

  procedure TGIS_Renderer3DAbstract.layerVectorParams(
    const _la   : TGIS_Layer ;
    const _mode : Boolean
  ) ;
  var
    pcs : TGIS_CSProjectedCoordinateSystem  ;

    function calculateNz : Double ;
    var
      val, oldVal : Double ;
    begin
      Result := 1.0 ;
      curVL[curVLSize -1].Zmi := curVL[curVLSize -1].Lv.Extent3D.ZMin ;
      if zUnit = 0 then exit;
      val := curVL[curVLSize -1].Lv.Extent3D.ZMax -
             curVL[curVLSize -1].Lv.Extent3D.ZMin ;
      val := (val + curVL[curVLSize -1].Lv.Params.FalseZ ) / curVL[curVLSize -1].Zu  ;
      if val = 0.0 then begin
        oldVal := curVL[curVLSize -1].Nm ;
        curVL[curVLSize -1].Nz  :=  ZMMIN ;
        curVL[curVLSize -1].Zmi :=  ZMMAX ;
        getLayerZMRange ;
        curVL[curVLSize -1].Nm := oldVal ;
        if curVL[curVLSize -1].Lv.Params.NormalizedZ = TGIS_3DNormalizationType.Max then
          val := curVL[curVLSize -1].Nz
        else   // range
          val := curVL[curVLSize -1].Zmx - curVL[curVLSize -1].Zmi ;
      end;

{
      if curVL[curVLSize -1].lv.CS.EPSG = oGIS.CS.EPSG then
          val := val / getZUnit( curVL[curVLSize -1].lv.CS )
        else }
          val := val / getZUnit( oGIS.CS ) ;

      if val <> 0 then
        Result := GISVIEWER3D_NORM_VALUE / val ;
    end;

    function calculateNm : Double ;
    var
      val, oldVal : Double ;
    begin
      Result := 1.0 ;
      curVL[curVLSize -1].Mmi := curVL[curVLSize -1].Lv.Extent3D.MMin ;
      if zUnit = 0 then exit;
      val := curVL[curVLSize -1].Lv.Extent3D.MMax -
             curVL[curVLSize -1].Lv.Extent3D.MMin ;
      val := (val + curVL[curVLSize -1].Lv.Params.FalseM ) / curVL[curVLSize -1].Zu  ;
      if val = 0.0 then begin
        oldVal := curVL[curVLSize -1].Nz ;
        curVL[curVLSize -1].Nm  :=  ZMMIN ;
        curVL[curVLSize -1].Mmi :=  ZMMAX ;
        getLayerZMRange ;
        curVL[curVLSize -1].Nz := oldVal ;
        if curVL[curVLSize -1].Lv.Params.NormalizedM = TGIS_3DNormalizationType.Max then
          val := curVL[curVLSize -1].Nm
        else // range
          val := curVL[curVLSize -1].Mmx - curVL[curVLSize -1].Mmi ;
      end;
{      if curVL[curVLSize -1].lv.CS.EPSG = oGIS.CS.EPSG then
        val := val / getZUnit( curVL[curVLSize -1].lv.CS )
      else }
        val := val / getZUnit( oGIS.CS ) ;
      if val <> 0 then
        Result := GISVIEWER3D_NORM_VALUE / val ;
    end;

  begin
    if _mode then
    if not _la.Active then exit ;
    if _la.View3D.Mode = TGIS_3DLayerType.Off then begin
      noImg := False ;
    end
    else
    if _mode then begin
      inc(curVLSize) ;
      SetLength(curVL, curVLSize) ;
      curVL[curVLSize -1].Lv := TGIS_LayerVector(_la) ;
//?      curVL[curVLSize -1].Zu := zUnit/getZUnit( TGIS_LayerVector( _la ).CS ) ;
      curVL[curVLSize -1].Zu := zUnit/getZUnit( oGIS.CS ) ;
      if (curVL[curVLSize -1].Lv.Params.NormalizedZ <> TGIS_3DNormalizationType.Off)
        and reRenderCall then
          curVL[curVLSize -1].Nz := calculateNz
      else
        curVL[curVLSize -1].Nz := 1.0 ;
      if (curVL[curVLSize -1].Lv.Params.NormalizedM <> TGIS_3DNormalizationType.Off)
        and reRenderCall then
          curVL[curVLSize -1].Nm := calculateNm
      else
        curVL[curVLSize -1].Nm := 1 ;
      if _la.CS is TGIS_CSProjectedCoordinateSystem then begin
        pcs := TGIS_CSProjectedCoordinateSystem(_la.CS) ;
        curVL[curVLSize -1].Fx := pcs.Units.Factor ;
        if oGIS.CS is TGIS_CSProjectedCoordinateSystem then begin
          pcs := TGIS_CSProjectedCoordinateSystem(oGIS.CS) ;
          curVL[curVLSize -1].Fx := curVL[curVLSize -1].Fx / pcs.Units.Factor ;
        end;
      end
      else
        curVL[curVLSize -1].Fx := 1 ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.layerProjectParams(
    const _la : TGIS_Layer        ;
    const _lp : TGIS_LayerProject ;
    const _mode : Boolean
  ) ;
  var
    j  : Integer            ;
    la : TGIS_Layer         ;
    lx : TGIS_LayerPixel    ;
    lp : TGIS_LayerProject  ;
  begin
    if _mode then
    if not _la.Active then exit ;
    for j := 0 to _lp.Items.Count -1 do begin
      la := TGIS_Layer( _lp.Items[j] ) ;
      if _lp.Items[j] is TGIS_LayerPixel then begin
        lx := TGIS_LayerPixel ( _lp.Items[j] ) ;
        layerPixelParams( la, lx , _mode ) ;
      end
      else
        if _lp.Items[j] is TGIS_LayerVector then
          layerVectorParams( la , _mode)
        else
        if _lp.Items[j] is TGIS_LayerProject then begin
          lp := TGIS_LayerProject( _lp.Items[j] ) ;
          layerProjectParams( la, lp , _mode) ;
        end;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.layerCompoundParams(
    const _la : TGIS_Layer ;
    const _lc : TGIS_LayerCompoundAbstract ;
    const _mode : Boolean
  ) ;
  var
    j  : Integer         ;
    la : TGIS_Layer      ;
    lx : TGIS_LayerPixel ;
  begin
    if _mode then
    if not _la.Active then exit ;
    for j := 0 to _lc.LayersCount -1 do begin
      la := TGIS_Layer( _lc.Layer[j] ) ;
      if _lc.Layer[j] is TGIS_LayerPixel then begin
        lx := TGIS_LayerPixel ( _lc.Layer[j] ) ;
        layerPixelParams( la, lx , _mode ) ;
      end
      else
        if _lc.Layer[j] is TGIS_LayerVector then
          layerVectorParams( la , _mode)
    end;
  end;

  procedure TGIS_Renderer3DAbstract.setDemTransparency1(
    const _value : Integer
  ) ;
  begin
    if _value <= 0   then begin
      demTransparency := 0 ;
      exit ;
    end;
    if _value >= 100 then begin
      demTransparency := 255 ;
      exit ;
    end;
    demTransparency := TruncS( _value * 255.0 / 100.0 ) ;
  end;

  procedure TGIS_Renderer3DAbstract.cleanUp ;
  begin
    try
      releaseGeometry ;
      FreeObject( oPolyRoof ) ;
      FreeObject( oTimer ) ;
      FreeObject( oTextureHelper ) ;
    except
    //do nothing
    end;
  end;

  procedure TGIS_Renderer3DAbstract.updateLayerExistance ;
  begin
    noDem     := True    ;
    noImg     := True    ;
    zLevel    :=  GIS_MAX_SINGLE ;
    zLevelMax := -GIS_MAX_SINGLE ;

    layersRangeSetting(True) ;

    if zLevel < -1E37 then begin // empty grid error protection
      zLevel := basePlane.Level ;
      zLevelMax := basePlane.Level ;
    end;

    if noDem then begin // noDem among layers
      zLevel     := rangeExtent.ZMin ;
      zLevelMax  := zLevel ;
      gridsExt   := exExtent ;
//?      xDemSize   := (exExtent.XMax-exExtent.XMin)/baseSize ;
//?      yDemSize   := (exExtent.YMax-exExtent.YMin)/baseSize ;
      isolineGap := 0 ;
    end;

    zMin := zLevel ;
    zLevelMin := zLevel ;
    zDelta := zLevelMax - zLevelMin ;
    if noDem = False then  begin
      if zLevelMax = zLevelMin then
        isolineGap := 1
      else  begin
        isolineGap := 1.0*TruncS(Log10(zLevelMax-zLevelMin)-1) ;
        isolineGap := Power(10,isolineGap) ;
        if (zLevelMax-zLevelMin)/isolineGap > 50 then
         isolineGap := 10*isolineGap ;
      end;
    end;

    if noImg or (not imageTexture) then
      preciseSwitch := False
    else
      preciseSwitch := True ;

    setZUnit ;
    setDefaultZ ;
//    defaultZ := zScaleFactor * (zMin - zLevel) / zUnit ;
  end;

  procedure TGIS_Renderer3DAbstract.updateCameraParams ;
  begin
    if cameraFocus < 20 then
      cameraFocus := 20 ;
    if cameraFocus > 500 then
      cameraFocus := 500 ;

    cameraViewAngle := 1700.0 / Power(cameraFocus + 5.0, 0.89 ) - 2.0 ;
    cameraHalfViewAngle := 0.5 * cameraViewAngle ;

    cameraPosition.X   := 90 - spinX  ;

    spinZ := truncateTo360( spinZ ) ;
    cameraPosition.Y   := 180 - spinZ ;
    if cameraPosition.Y < 0 then
      cameraPosition.Y := 360 + cameraPosition.Y ;
    cameraPosition.Y := truncateTo360( cameraPosition.Y ) ;

    cameraPosition.Z   := dRadius ;
  //  cameraPosition.M   := for future use

    cameraPositionEx.X := -dTurn + (dRadius * Sin(DegToRad(spinZ))*
      Sin(DegToRad(spinX))) ;
    cameraPositionEx.Y := dFly - (dRadius * Cos(DegToRad(spinZ))*
      Sin(DegToRad(spinX))) ;
    cameraPositionEx.Z := dRadius * Cos(DegToRad(spinX)) ;
  //  cameraPositionEx.M := for future use

    cameraRotation.X   := 90 - (spinX + cspinX ) ;
    cameraRotation.Y   := spinY ;
    cameraRotation.Z   := spinZ ;
    cameraRotation.Z   :=  truncateTo360(-cameraRotation.Z) ;
    if cameraRotation.Z < 0 then
      cameraRotation.Z := 360 + cameraRotation.Z ;

    cameraRotation.M   := cameraFocus;

    if assigned( UpdateEvent ) then
      {$IFDEF CLR}
        UpdateEvent( Self, EventArgs.Empty ) ;
      {$ELSE}
        UpdateEvent( Self ) ;
      {$ENDIF}
  end;

  procedure TGIS_Renderer3DAbstract.updateFrameSizes ;
  var
    size : Integer ;
    val  : Double  ;
  begin
    val  := mapExtent.XMax - mapExtent.XMin ;
    size := TruncS( val / xDemSize ) ;

    if size mod 2 <> 0 then
      size := size + 1 ;
    if size < 32 then
      size := 32 ;

    if size > vertexBufferSize then
    if demFrameSize = vertexBufferSize then
      exit ;
    if demFrameSize = size then
      exit ;

    demFrameSize := size ;
    if demFrameSize > vertexBufferSize then
      demFrameSize := vertexBufferSize ;
    SetLength( arGrid_r, demFrameSize, demFrameSize ) ;
    SetLength( arGrid_w, demFrameSize, demFrameSize ) ;
    setLeftColumn ;
  end;

  procedure TGIS_Renderer3DAbstract.setInitialLevel ;
  begin
    if wasReferenceSet then exit ;
    if noDem = False then begin // is DEM
      if not wasReferenceSet then begin
        if curVLSize > 0 then  // is vector
          referencePointMode :=  TGIS_Viewer3DReferenceMode.OnDem
        else                   // DEM only
          referencePointMode := TGIS_Viewer3DReferenceMode.Highest ;
      end;
    end
    else begin
      if not wasReferenceSet then begin
        referencePointMode := TGIS_Viewer3DReferenceMode.Highest ;
      end;
    end;

    zLevel    :=  GIS_MAX_SINGLE ;
    zLevelMax := -GIS_MAX_SINGLE ;

    layersRangeSetting(True) ;

    if zLevel < -1E37 then begin   // empty grid error protection
      zLevel := basePlane.Level ;
      zLevelMax := basePlane.Level ;
      referencePointMode := TGIS_Viewer3DReferenceMode.Highest ;
    end;



    setReferencePoint(referencePointMode, referencePointOffset) ;
    updateCameraParams ;
    transformSetting ;
    wasReferenceSet := True ;
//    if Abs((rangeExtent.ZMax - rangeExtent.ZMin) / 10.0) < Abs(rangeExtent.ZMin) then
//      basePlane.Level := rangeExtent.ZMin - (rangeExtent.ZMax - rangeExtent.ZMin) / 10.0
//    else
      basePlane.Level := rangeExtent.ZMin - zUnit; //*unitDistance ;
//      basePlane.Level := rangeExtent.ZMin - (rangeExtent.ZMax - rangeExtent.ZMin) / 10.0 ;
//      basePlane.Level := rangeExtent.ZMin - Max( zUnit, (rangeExtent.ZMax - rangeExtent.ZMin) / 10.0) ;
    makeBasementTex ;
    makeSkyBoxTex ;
  end;

  function TGIS_Renderer3DAbstract.getVectorExtent
    : TGIS_Extent;
  var
    dd  : Double     ;
    pt  : TPoint     ;
    ptg : TGIS_Point ;
  begin
    dd := wndXSize ;
    dd := dd*(dRadius+zEyeTo+zOffset)/dRadius ;
    if scrRatio > 1.0 then
      dd := dd * scrRatio ;
    dd := vectorCoeff*detailCoeff*dd ;
    if referenceMode = TGIS_Viewer3DReferenceLevelMode.Default then begin
      pt.X := TruncS(0.50*windowWidth ) ;
      pt.Y := TruncS((0.50 + 0.25*Cos(DegToRad(90-spinX)))*windowHeight) ;
      ptg  := screenTo3D(pt) ;
      scrExtent := GisExtent(  ptg.X - dd,
                              -ptg.Y - dd,
                               ptg.X + dd,
                              -ptg.Y + dd
                            ) ;
    end
    else begin
      scrExtent := GisExtent( -dTurn - dd,
                              -dFly  - dd,
                              -dTurn + dd,
                              -dFly  + dd
                            ) ;
    end;

    if scrExtent.XMin > scrExtent.XMax then begin
      dd := scrExtent.XMin ;
      scrExtent.XMin := scrExtent.XMax ;
      scrExtent.XMax := dd ;
    end;
    if scrExtent.YMin > scrExtent.YMax then begin
      dd := scrExtent.YMin ;
      scrExtent.YMin := scrExtent.YMax ;
      scrExtent.YMax := dd ;
    end;
    Result := retGisExtent ;
  end;

  function TGIS_Renderer3DAbstract.retGisExtent
    : TGIS_Extent;
  var
    dx, dy   : Double        ;
    dx1, dy1 : Double        ;
    diff     : Double        ;
    wrk_extent : TGIS_Extent ;
  begin
    xMin := -baseSize ;
    xMax :=  baseSize ;
    yMin := -baseSize*grdRatio ;
    yMax :=  baseSize*grdRatio ;

    if scrExtent.XMax > xxMax then begin
      diff := scrExtent.XMax - xxMax ;
      scrExtent.XMax := xxMax ;
      scrExtent.XMin := scrExtent.XMin - diff ;
    end;
    if scrExtent.XMin < xxMin then begin
      diff := scrExtent.XMin - xxMin ;
      scrExtent.XMin := xxMin ;
      scrExtent.XMax := scrExtent.XMax - diff ;
    end;
    if scrExtent.YMax > yyMax then begin
      diff := scrExtent.YMax - yyMax ;
      scrExtent.YMax := yyMax ;
      scrExtent.YMin := scrExtent.YMin - diff ;
    end;
    if scrExtent.YMin < yyMin then begin
      diff := scrExtent.YMin - yyMin ;
      scrExtent.YMin := yyMin ;
      scrExtent.YMax := scrExtent.YMax - diff ;
    end;

    if (scrExtent.XMax - scrExtent.XMin)>(xxMax-xxMin) then begin
      scrExtent := GisExtent( -maxExt, -maxExt, maxExt, maxExt ) ;
    end;

    dx := gridsExt.XMax - gridsExt.XMin ;
    dy := gridsExt.YMax - gridsExt.YMin ;

    dx1 := xMax - xMin ;
    dy1 := yMax - yMin ;

    wrk_extent := GisExtent(
                    gridsExt.XMin - dx * ( xMin - scrExtent.XMin ) / dx1,
                    gridsExt.YMax - dy * ( scrExtent.YMax - yMin ) / dy1,
                    gridsExt.XMin + dx * ( scrExtent.XMax - xMin ) / dx1,
                    gridsExt.YMax + dy * ( yMin - scrExtent.YMin ) / dy1
                  ) ;
    Result := wrk_extent ;
  end;

  procedure TGIS_Renderer3DAbstract.setNormExtent(
    const _detail: Boolean
  ) ;
  var
    dd,
    dx,
    dy,
    Z,
    dis : Double     ;
    ptg : TGIS_Point ;
    p   : TVector3f  ;
  begin
    dd := wndXSize ;
    Z := (GetOriginPoint.Z - zLevel)/zUnit ;
    if Z > 0 then
      dd := dd*(dRadius+zEyeTo+zOffset)/dRadius ;
    if scrRatio > 1.0 then
      dd := dd * scrRatio ;
    dd := detailCoeff*dd ;
      if _detail then begin
        dx := 0.5*(-dTurn - cameraPositionEx.X) ;
        dy := 0.5*(  dFly - cameraPositionEx.Y) ;
        if (Z<0) or (cameraPosition.X <= cameraHalfViewAngle) then begin
          if intersectLinePlane( vector3Init(cameraPositionEx.X,
                                             cameraPositionEx.Y,
                                             cameraPositionEx.Z),
                                 vector3Init(-dTurn-dx, dFly-dy, 0),
                                 vector3Init(20,0,0),
                                 vector3Init(0,20,0),
                                 vector3Init(-20,-20,0),
                                 p, dis
                              ) then
            ptg := GisPoint(-p.X, p.Y)
          else
            ptg := GisPoint(-0.5*(-dTurn + cameraPositionEx.X),
                             0.5*(  dFly + cameraPositionEx.Y)) ;
        end
        else begin
          if intersectLinePlane( vector3Init(cameraPositionEx.X,
                                             cameraPositionEx.Y,
                                             cameraPositionEx.Z),
                                 vector3Init(-dTurn, dFly, 0),
                                 vector3Init(20,0,0),
                                 vector3Init(0,20,0),
                                 vector3Init(-20,-20,0),
                                 p, dis
                                ) then
            ptg := GisPoint(-p.X, p.Y)
          else
            ptg := GisPoint(-0.5*(-dTurn + cameraPositionEx.X),
                             0.5*(  dFly + cameraPositionEx.Y)) ;

        end;
        scrExtent := GisExtent( -ptg.X - dd,
                                -ptg.Y - dd,
                                -ptg.X + dd,
                                -ptg.Y + dd
                               ) ;
      end // detail
      else begin
        dd := roughCoeff*dd ;
        scrExtent := GisExtent( -dTurn - dd,
                                -dFly  - dd,
                                -dTurn + dd,
                                -dFly  + dd
                              ) ;
      end;

    if scrExtent.XMin > scrExtent.XMax then begin
      dd := scrExtent.XMin ;
      scrExtent.XMin := scrExtent.XMax ;
      scrExtent.XMax := dd ;
    end;
    if scrExtent.YMin > scrExtent.YMax then begin
      dd := scrExtent.YMin ;
      scrExtent.YMin := scrExtent.YMax ;
      scrExtent.YMax := dd ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.setRangeExtent ;
  var
    i, j : Integer ;
    dbl  : Double  ;
  begin
    if noDem and (curVLSize = 0) then begin
      rangeExtent.ZMin := 0 ;
      rangeExtent.ZMax := 0 ;
      heightRange := 0 ;
      exit ;
    end;

    heightRangeMin   :=  GIS_MAX_SINGLE ;
    heightRangeMax   := -GIS_MAX_SINGLE ;
    rangeExtent.ZMin := heightRangeMin ;
    rangeExtent.ZMax := heightRangeMax ;

    if noDem = False then begin
      for i := 0 to demFrameSize -1 do
      for j := 0 to demFrameSize -1 do begin
        if arGrid_r[i][j] <> noDataValue then begin
          if arGrid_r[i][j] < rangeExtent.ZMin then
            rangeExtent.ZMin := arGrid_r[i][j] ;
          if arGrid_r[i][j] > rangeExtent.ZMax then
            rangeExtent.ZMax := arGrid_r[i][j] ;
        end;
      end;
    end;

    heightRangeMin   :=  GIS_MAX_SINGLE ;
    heightRangeMax   := -GIS_MAX_SINGLE ;
    if curVLSize > 0 then begin
      calcInitialLevel ;
      if groundType = TGIS_3DGroundType.AboveDem then begin
        if noDem = False then begin
          if heightRangeMax > 0 then
            rangeExtent.ZMax := rangeExtent.ZMax + heightRangeMax ;
          if heightRangeMin < rangeExtent.ZMin then
            rangeExtent.ZMin := heightRangeMin ;
        end
        else begin
          rangeExtent.ZMin := heightRangeMin ;
          rangeExtent.ZMax := heightRangeMax ;
        end;
      end
      else if groundType = TGIS_3DGroundType.AboveZero then begin
        if noDem = False then begin
          if heightRangeMax > rangeExtent.ZMax then
            rangeExtent.ZMax := heightRangeMax ;
          if heightRangeMin < rangeExtent.ZMin then
            rangeExtent.ZMin := heightRangeMin ;
        end
        else begin
          rangeExtent.ZMin := heightRangeMin ;
          rangeExtent.ZMax := heightRangeMax ;
        end;
      end
      else if groundType = TGIS_3DGroundType.OnDem then begin
        if noDem = False then begin
          if heightRangeMax > 0 then
            rangeExtent.ZMax := rangeExtent.ZMax + heightRangeMax ;
        end
        else begin
          rangeExtent.ZMin := heightRangeMin ;
          rangeExtent.ZMax := heightRangeMax ;
        end;
      end;
    end;

    dbl := GIS_MAX_SINGLE ;
    if rangeExtent.ZMin = heightRangeMin then
    if heightRangeMin =  dbl then
      rangeExtent.ZMin := zLevelMin ;
    if rangeExtent.ZMax = heightRangeMax then
    if heightRangeMax = -dbl then
      rangeExtent.ZMax := zLevelMax ;

  end;

  function  TGIS_Renderer3DAbstract.vectorSubtract(
    var   _out : TVector3f ;
    const _in1 : TVector3f ;
    const _in2 : TVector3f
  ) : TVector3f ;
  begin
    _out.X := _in1.X - _in2.X ;
    _out.Y := _in1.Y - _in2.Y ;
    _out.Z := _in1.Z - _in2.Z ;
    Result := _out ;
  end;

  function  TGIS_Renderer3DAbstract.vectorDot(
    const _in1 : TVector3f ;
    const _in2 : TVector3f
  ) : Single ;
  begin
    Result := _in1.X * _in2.X + _in1.Y * _in2.Y + _in1.Z * _in2.Z ;
  end;

  function  TGIS_Renderer3DAbstract.vectorCross(
    var   _out : TVector3f ;
    const _in1 : TVector3f ;
    const _in2 : TVector3f
  ) : TVector3f ;
  begin
    _out.X := _in1.Y * _in2.Z - _in1.Z * _in2.Y ;
    _out.Y := _in1.Z * _in2.X - _in1.X * _in2.Z ;
    _out.Z := _in1.X * _in2.Y - _in1.Y * _in2.X ;
    Result := _out ;
  end;

  function  TGIS_Renderer3DAbstract.intersectTriRay(
    const _v0, _v1, _v2 : TVector3f ;
    const _orig, _dir  : TVector3f ;
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
    vectorSubtract(edge1, _v1, _v0);
    vectorSubtract(edge2, _v2, _v0);

    // Begin calculating determinant - also used to calculate U parameter
    vectorCross(pvec, _dir, edge2);

    // If determinant is near zero, ray lies in plane of triangle
    det:= vectorDot(edge1, pvec);
    if (det > -0.00001) and (det < 0.00001) then
    begin
      Result:= False;
      exit;
    end;

    // Calculate distance from vert0 to ray origin
    vectorSubtract(tvec, _orig, _v0);

    // Calculate U parameter and test bounds
    fInvDet:= 1.0 / det;
    _u:= vectorDot(tvec, pvec) * fInvDet ;
    if(_u < 0.0) or (_u > 1.0) then
    begin
      Result:= False ;
      exit;
    end;

    // Prepare to test V parameter
    vectorCross(qvec, tvec, edge1);

    // Calculate V parameter and test bounds
    _v:= vectorDot(_dir, qvec) * fInvDet ;
    if (_v < 0.0) or (_u + _v > 1.0) then
    begin
      Result:= False ;
      exit;
    end;

    // Calculate t, scale parameters, ray intersects triangle
    _d := vectorDot(edge2, qvec);
    fInvDet:= 1.0 / det ;
    _d := _d * fInvDet ;

    Result:= True ;
  end;

  function TGIS_Renderer3DAbstract.intersectLinePlane(
    const _p1, _p2      : TVector3f ;
    const _pa, _pb, _pc : TVector3f ;
    out   _p            : TVector3f ;
    out _t              : Double
  ) : Boolean ;
  var
    p, n : TVector3f ;
    denom, mu, d : Double ;
  begin
    // Calculate the parameters for the plane
    n.X := (_pb.Y - _pa.Y)*(_pc.Z - _pa.Z) - (_pb.Z - _pa.Z)*(_pc.Y - _pa.Y) ;
    n.Y := (_pb.Z - _pa.Z)*(_pc.X - _pa.X) - (_pb.X - _pa.X)*(_pc.Z - _pa.Z) ;
    n.Z := (_pb.X - _pa.X)*(_pc.Y - _pa.Y) - (_pb.Y - _pa.Y)*(_pc.X - _pa.X) ;

    normalizeVector( n, n ) ;

    d := - n.X * _pa.X - n.Y * _pa.Y - n.Z * _pa.Z ;
    // Calculate the position on the line that intersects the plane
    denom := n.X * (_p2.X - _p1.X) +
             n.Y * (_p2.Y - _p1.Y) +
             n.Z * (_p2.Z - _p1.Z) ;

    if (Abs(denom) < 0.0001) then        // Line and plane don't intersect
    begin
      Result := False ;
      exit;
    end;

    mu := - (d + n.X * _p1.X + n.Y * _p1.Y + n.Z * _p1.Z) / denom ;

    p.X := _p1.X + mu * (_p2.X - _p1.X) ;
    p.Y := _p1.Y + mu * (_p2.Y - _p1.Y) ;
    p.Z := _p1.Z + mu * (_p2.Z - _p1.Z) ;
    _p := p ;
    _t := Sqrt(Sqr(p.X-_p1.X) + Sqr(p.Y-_p1.Y) + Sqr(p.Z-_p1.Z));
    Result := True ;
  end;

  procedure TGIS_Renderer3DAbstract.normalizeVector(
    var _in  : TVector3f ;
    var _out : TVector3f
  ) ;
  var
    l : Double ;
  begin
    l := Sqrt(Sqr(_in.X) + Sqr(_in.Y) + Sqr(_in.Z)) ;
    if l = 0 then begin
      _out.Y := 1 ;
      exit;
    end;
    _out.X := _out.X / l ;
    _out.Y := _out.Y / l ;
    _out.Z := _out.Z / l ;
  end;

  procedure TGIS_Renderer3DAbstract.calcInitialLevel ;
  var
    i   : Integer ;
    lv  : TGIS_LayerVector ;
    ext : TGIS_Extent ;
  begin
    heightRange  := 0 ;
    for i:= 0 to curVLSize-1 do begin
      lv := curVL[i].Lv ;
      currentVL := lv ;

      ext := GisCommonExtent( oGIS.VisibleExtent, currentVL.ProjectedExtent ) ;
      ext := currentVL.UnprojectExtent( ext ) ;
      cutVectExtentL := GisCommonExtent( ext, currentVL.Extent ) ;

      {$IFDEF CLR}
        if not lv.ForEach( cutVectExtentL, '', nil, '', True, oGIS.ScaleAsFloat,
                           @checkInitialLevel ) then exit ;
      {$ELSE}
        if not lv.ForEach( cutVectExtentL, '', nil, '', True, oGIS.ScaleAsFloat,
                            checkInitialLevel ) then exit ;
      {$ENDIF}
    end;
  end;

  procedure TGIS_Renderer3DAbstract.checkInitialLevel(
          _shp   : TGIS_Shape ;
    var   _abort : Boolean
  ) ;
  var
    ptg  : TGIS_Point3D;
    Z    : Double;
    shpc : TGIS_ShapeComplex;
    i, k : Integer ;
    oparams : TGIS_ParamsSectionVector ;


    procedure setHeightRange(
      const _zMin : Double ;
      const _zMax : Double
    ) ;
    var
      zval    : Double ;
    begin
      if _zMin < heightRangeMin then
        heightRangeMin := _zMin ;
      if _zMax > heightRangeMax then
        heightRangeMax := _zMax ;
      zval := Abs(heightRangeMax - heightRangeMin) ;
      if zval > heightRange then
        heightRange := zval ;
    end;

  begin
    if not _shp.Layer.Active then exit;

    if _shp.ShapeType = TGIS_ShapeType.Complex then begin
      shpc := TGIS_ShapeComplex(_shp);
      k := shpc.ShapesCount;
      for i := 0 to k - 1 do
        checkInitialLevel(shpc.GetShape(i), _abort);
      exit;
    end;

    _shp.Lock( TGIS_Lock.Projection ) ;

    ptg := _shp.GetPoint3D(0, 0) ;

    if (_shp.PointsZMin > ZMMIN) and (_shp.PointsZMin < ZMMAX) then
      ptg.Z := _shp.PointsZMin ;
    if (_shp.PointsMMin > ZMMIN) and (_shp.PointsMMin < ZMMAX) then
      ptg.M := _shp.PointsMMin ;

    if (ptg.M < ZMMIN) or (ptg.M > ZMMAX) then ptg.M := 0 ;
    if (ptg.Z < ZMMIN) or (ptg.Z > ZMMAX) then ptg.Z := 0 ;

    Z := ptg.Z ;
    currentVL.Project3D_Ref( ptg ) ;
    if bIgnoreEllipsoidHeight then
        ptg.Z := Z ;

    oparams := _shp.Params ;

    ptg.Z := ptg.Z * oparams.ScaleZ + oparams.FalseZ ;
    Z := ptg.Z + mFactor*(ptg.M * oparams.ScaleM + oparams.FalseM) ;

    groundType := currentVL.Params.Ground ;
    if groundType <> TGIS_3DGroundType.OnDem then
      setHeightRange(ptg.Z, Z)
    else
      setHeightRange(0, Abs(ptg.Z - Z)) ;

    if (_shp.PointsZMax > ZMMIN) and (_shp.PointsZMax < ZMMAX) then
      ptg.Z := _shp.PointsZMax ;
    if (_shp.PointsMMax > ZMMIN) and (_shp.PointsMMax < ZMMAX) then
      ptg.M := _shp.PointsMMax ;

    if (ptg.M < ZMMIN) or (ptg.M > ZMMAX) then ptg.M := 0 ;
    if (ptg.Z < ZMMIN) or (ptg.Z > ZMMAX) then ptg.Z := 0 ;

    Z := ptg.Z ;
    currentVL.Project3D_Ref( ptg ) ;
    if bIgnoreEllipsoidHeight then
        ptg.Z := Z ;

    ptg.Z := ptg.Z * oparams.ScaleZ + oparams.FalseZ ;
    Z := ptg.Z + mFactor*(ptg.M * oparams.ScaleM + oparams.FalseM) ;

    if groundType <> TGIS_3DGroundType.OnDem then
      setHeightRange(ptg.Z, Z)
    else
      setHeightRange(0, Abs(ptg.Z - Z)) ;

    _shp.Unlock ;
  end;

  function TGIS_Renderer3DAbstract.mapToD3D (
    const _pnt : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    npnt : TGIS_Point3D ;
    bs   : Double       ;
  begin
    npnt.X := (2.0*baseSize)*(_pnt.X - gridsExt.XMin)/
                             (gridsExt.XMax - gridsExt.XMin) ;
    npnt.X := -baseSize + npnt.X ;
    bs := baseSize * grdRatio ;
    npnt.Y := (2.0*bs)*(_pnt.Y - gridsExt.YMin)/(gridsExt.YMax - gridsExt.YMin);
    npnt.Y := -bs + npnt.Y ;
    npnt.Z := (2.0*baseSize)* _pnt.Z /(gridsExt.XMax - gridsExt.XMin) ;
    npnt.M := (2.0*baseSize)* _pnt.M /(gridsExt.XMax - gridsExt.XMin) ;
    Result := npnt ;
  end;

  function TGIS_Renderer3DAbstract.checkExtent
    : Boolean ;
  begin
    Result := True ;
    if detailExtent.XMin = -maxExt then
    if detailExtent.XMax =  maxExt then
    if detailExtent.YMin = -maxExt then
    if detailExtent.YMax =  maxExt then
      Result := False ;
  end;

  procedure TGIS_Renderer3DAbstract.setEmptyWindow ;
  var
    kk   : Integer ;
    val,
    valb : Double  ;
    rdx,
    rdy  : Double  ;
  begin
    rdx := (roughExtent.XMax  - roughExtent.XMin  )/(demFrameSize-1) ;
    rdy := (roughExtent.YMax  - roughExtent.YMin  )/(demFrameSize-1) ;
    if detailExtent.XMin > normExt.XMin then
      xMin1 := detailExtent.XMin
    else begin
      valb := normExt.XMin ;
      kk := 0 ;
      repeat
        kk  := kk+lodStep ;
        val := roughExtent.XMin+kk*rdx ;
      until  val>valb ;
      xMin1 := roughExtent.XMin+kk*rdx ;
    end;

    if detailExtent.XMax < normExt.XMax then
      xMax1 := detailExtent.XMax
    else begin
      valb := normExt.XMax ;
      kk   := demFrameSize ;
      repeat
        kk  := kk-lodStep ;
        val := roughExtent.XMin+kk*rdx ;
      until  val<valb ;
      xMax1 := roughExtent.XMin+kk*rdx ;
    end;

    if detailExtent.YMin > normExt.YMin then
      yMin1 := detailExtent.YMin
    else begin
      valb := normExt.YMin ;
      kk := 0 ;
      repeat
        kk  := kk+lodStep ;
        val := roughExtent.YMin+kk*rdy ;
      until  val>valb ;
      yMin1 := roughExtent.YMin+kk*rdy ;
    end;

    if detailExtent.YMax < normExt.YMax then
      yMax1 := detailExtent.YMax
    else begin
      valb := normExt.YMax ;
      kk   := demFrameSize ;
      repeat
        kk  := kk-lodStep ;
        val := roughExtent.YMin+kk*rdy ;
      until  val<valb ;
      yMax1 := roughExtent.YMin+kk*rdy ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.updateDemTexture(
    var   _pardata : T_arTextureData ;
          _len     : Integer
  ) ;
  var
    i : Integer ;
    d : Single ;
  begin
    d := 2 /( demFrameSize -1 ) ;
    i := 1 ;
    while True do begin
      if ((_pardata[i-1].P.Z =  defaultZ) and
          (_pardata[i].P.Z   <> defaultZ)) then begin
           _pardata[i-1].Tv := _pardata[i].Tv ;
           _pardata[i-1].Tu := _pardata[i].Tu ;
         end
      else
      if ((_pardata[i].P.Z   =  defaultZ) and
          (_pardata[i-1].P.Z <> defaultZ)) then begin
           _pardata[i-1].Tv := _pardata[i-1].Tv + d ;
           _pardata[i].Tv := _pardata[i-1].Tv  ;
         end;

      if (i > 1) and (i < _len-2) then begin
        if ((_pardata[i].P.Z   <>  defaultZ) and
            (_pardata[i-2].P.Z = defaultZ)) then begin
             _pardata[i-2].Tu := _pardata[i].Tu ;
             _pardata[i-2].Tv := _pardata[i].Tv ;
            end;

        if ((_pardata[i-1].P.Z   <>  defaultZ) and
            (_pardata[i-3].P.Z = defaultZ)) then begin
             _pardata[i-3].Tu := _pardata[i-1].Tu ;
             _pardata[i-3].Tv := _pardata[i-1].Tv ;
            end;

        if ((_pardata[i-1].P.Z   <>  defaultZ) and
            (_pardata[i+1].P.Z = defaultZ)) then begin
             _pardata[i+1].Tu := _pardata[i-1].Tu ;
             _pardata[i+1].Tv := _pardata[i-1].Tv ;
            end;

        if ((_pardata[i].P.Z   <>  defaultZ) and
            (_pardata[i+2].P.Z = defaultZ)) then begin
             _pardata[i+2].Tu := _pardata[i].Tu ;
             _pardata[i+2].Tv := _pardata[i].Tv ;
            end;
      end;
      inc( i , 2 ) ;
      if i >= _len then break;
    end;
  end;

  function TGIS_Renderer3DAbstract.fillVertexBuffer(
    const _row  : Integer;
    const _mode : Boolean
  ) : Integer ;
  var
    hr        : Integer         ;
    ii, loops,
    startrow  : DWORD           ;
    pardata   : T_arTextureData ;
    i, k,
    val,
    idx,
    pidx,
    offset,
    kk, jj,
    end_val,
    c1, row   : Integer         ;
    isout     : Boolean         ;
  begin
    row := _row ;
    hr  := 0 ;
    loops := 1 ;
    startrow := row;
    lodStep := TruncS(Power(2.0, lod*1.0)) ;
    c1 := 0 ;

    for ii := 0 to loops-1 do begin
      row := startrow+ii ;
      isout := True ;

      SetLength( pardata, TruncS( 2.0 * demFrameSize / lodStep ) + 4 ) ;

      k := 0 ;
      i := TruncS( ( 1.0 * leftColumn )/lodStep ) ;
      end_val := demFrameSize - leftColumn ;

      if _mode then begin
        i := TruncS( ( 1.0 * detWnd.Left )/lodStep ) ;
        end_val := detWnd.Right+lodStep;
      end;

      offset:= row*lodStep*demFrameSize ;
      val := i + demFrameSize ;
      idx := val*lodStep ;
      kk := idx mod demFrameSize ;
      jj := TruncS( ( 1.0 * (idx + offset) ) / demFrameSize ) ;
      pardata[k].P := getGrdP(kk,jj) ;
      pardata[k].N := setGrdN(kk,jj) ;
      textureValue(pardata,k,kk,jj,pardata[k].P.Z) ;
      if _mode then
        detailTexture(pardata,k,kk,jj) ;
      inc(k) ;

      repeat
        val := i ;
        idx := val*lodStep ;
        kk := idx mod demFrameSize ;
        jj := TruncS( ( 1.0 * (idx + offset) ) / demFrameSize ) ;
        pardata[k].P := getGrdP(kk,jj) ;
        pardata[k].N := setGrdN(kk,jj) ;
        textureValue(pardata,k,kk,jj,pardata[k].P.Z) ;
        if _mode then
          detailTexture(pardata,k,kk,jj) ;
        inc(k) ;

        if k = 2 then begin
          if (pardata[1].P.Y >= normExt.YMax ) or
             (pardata[0].P.Y <  normExt.YMin ) then begin
            Result := 0 ;
            exit ;
          end;
        end;

        val := i + demFrameSize +1 ;
        idx := val*lodStep ;
        kk := idx mod demFrameSize ;
        jj := TruncS( ( 1.0 * (idx + offset) ) / demFrameSize ) ;
        if k = 1 then  k:=0 ;
        pardata[k].P := getGrdP(kk,jj) ;
        pardata[k].N := setGrdN(kk,jj) ;
        textureValue(pardata,k,kk,jj,pardata[k].P.Z) ;
        if _mode then
         detailTexture(pardata,k,kk,jj) ;

        if _mode = False then begin
          if pardata[k].P.X >= xMin1 then
          if pardata[k].P.Y >= yMin1 then
          if pardata[k].P.X <= xMax1 then
          if pardata[k].P.Y <= yMax1 then begin
            if isout  then begin
              if k > 2 then begin
                if firstRowFound then begin
                  inc(c1) ;
                  if c1 = 2 then begin // rows left to detail
                    setDemTexture( pardata, k, True ) ;
                    isout := False ;
                  end;
                end;
              end;
              if firstRowFound and (c1>=2) then
                k := -1 ;
              {$IFDEF CLR}
                if detWnd.Left   > kk  then
                  detWnd := Rect( kk, detWnd.Top, detWnd.Right, detWnd.Bottom ) ;
                if detWnd.Right  < kk  then
                  detWnd := Rect( detWnd.Left, detWnd.Top, kk, detWnd.Bottom ) ;
                if detWnd.Top    > row then
                  detWnd := Rect( detWnd.Left, row, detWnd.Right, detWnd.Bottom ) ;
                if detWnd.Bottom < row then
                  detWnd := Rect( detWnd.Left, detWnd.Top, detWnd.Right, row ) ;
              {$ELSE}
                if detWnd.Left   > kk  then detWnd.Left   := kk  ;
                if detWnd.Right  < kk  then detWnd.Right  := kk  ;
                if detWnd.Top    > row then detWnd.Top    := row ;
                if detWnd.Bottom < row then detWnd.Bottom := row ;
              {$ENDIF}
            end  // isout
            else begin
              if firstRowFound then
              if xValue(kk+lodStep) < xMax1 then
               k := -1 ;
            end;

            {$IFDEF CLR}
              if detWnd.Left   > kk  then
                detWnd := Rect( kk, detWnd.Top, detWnd.Right, detWnd.Bottom ) ;
              if detWnd.Right  < kk  then
                detWnd := Rect( detWnd.Left, detWnd.Top, kk, detWnd.Bottom ) ;
              if detWnd.Top    > row then
                detWnd := Rect( detWnd.Left, row, detWnd.Right, detWnd.Bottom ) ;
              if detWnd.Bottom < row then
                detWnd := Rect( detWnd.Left, detWnd.Top, detWnd.Right, row ) ;
            {$ELSE}
              if detWnd.Left   > kk  then detWnd.Left   := kk  ;
              if detWnd.Right  < kk  then detWnd.Right  := kk  ;
              if detWnd.Top    > row then detWnd.Top    := row ;
              if detWnd.Bottom < row then detWnd.Bottom := row ;
            {$ENDIF}
          end
          else
           isout := True ;
        end; // _mode = False

        inc(k) ;

        val := (i + 1) * lodStep ;
        inc(i) ;
      until  (val+lodStep>end_val-1) ;

      dec(i) ;
      val := i + 1 ;
      idx := val*lodStep;
      kk := idx mod demFrameSize;
      jj := TruncS( ( 1.0 * (idx + offset) ) / demFrameSize ) ;
      pardata[k].P := getGrdP(kk,jj) ;
      pardata[k].N := setGrdN(kk,jj) ;
      textureValue(pardata,k,kk,jj,pardata[k].P.Z) ;
      if _mode then
        detailTexture(pardata,k,kk,jj) ;
      pidx := idx ;
      inc(k) ;

      if (pidx < demFrameSize-1) and (end_val = demFrameSize) then begin
        idx := (lodStep*demFrameSize+demFrameSize-1) ;
        kk := idx mod demFrameSize ;
        jj := TruncS( ( 1.0 * (idx + offset) ) / demFrameSize ) ;
        pardata[k].P := getGrdP(kk,jj) ;
        pardata[k].N := setGrdN(kk,jj) ;
        textureValue(pardata,k,kk,jj,pardata[k].P.Z) ;
        if _mode then
          detailTexture(pardata,k,kk,jj) ;
        inc(k) ;
        idx := demFrameSize-1 ;
        kk := idx mod demFrameSize;
        jj := TruncS( ( 1.0 * (idx + offset) ) / demFrameSize ) ;
        pardata[k].P := getGrdP(kk,jj) ;
        pardata[k].N := setGrdN(kk,jj) ;
        textureValue(pardata,k,kk,jj,pardata[k].P.Z) ;
        if _mode then
          detailTexture(pardata,k,kk,jj) ;
        inc(k) ;
      end;
      updateDemTexture( pardata, k ) ;
      setDemTexture( pardata, k, True) ;

    end;
    if detWnd.Top <> vertexBufferSize then
      firstRowFound := True ;
    Result := 0 ;
  end;

  procedure TGIS_Renderer3DAbstract.addVectorTilesInfo(
    const _tp  : Integer  ;
    const _no  : Integer
  ) ;
  var
    i : Integer ;
  begin
    if assigned( currentML ) then begin
      if currentML.CachedPaint then  exit
    end
    else
      if currentVL.CachedPaint then exit ;
    if bPartialUpdate then
      exit ;
    inc( vectorTilesInfoCounter );
    SetLength( arVectorTilesInfo, vectorTilesInfoCounter ) ;
    i := vectorTilesInfoCounter -1 ;
    arVectorTilesInfo[i].Layer     := currentVL ;
    arVectorTilesInfo[i].TileType  := _tp ;
    arVectorTilesInfo[i].TileIndex := _no ;
    arVectorTilesInfo[i].ToDraw    := False ;
  end;

  function TGIS_Renderer3DAbstract.getGrdP(
    const _col : Integer;
    const _row : Integer
  ) : TVector3f ;
  var
   pp : TVector3f ;
  begin
    pp.X := xValue(_col) ;
    pp.Y := yValue(_row) ;
    pp.Z := zValue(_col, _row) ;
    Result := pp ;
  end;

  function TGIS_Renderer3DAbstract.setGrdN(
    const _col: Integer ;
    const _row: Integer
  ) : TVector3f ;
  var
    nn,nn1,nn2,nn3,nn4, an1, an2 : TVector3f ;
    p0, p1, p2, p3, p4           : TVector3f ;
  begin
    nn.X := 0.0 ;
    nn.Y := 0.0 ;
    nn.Z := 0.0 ;
    p0.Z := 0 ;
    p1.Z := 0 ;
    p2.Z := 0 ;
    p3.Z := 0 ;
    p4.Z := 0 ;
    p0 := getGrdP(_col, _row) ;
    p0.Z := p0.Z * zScaleFactor ;
    if _col >= lodStep then begin
      p1   := getGrdP(_col-lodStep,_row) ;
      p1.Z := p1.Z*zScaleFactor ;
    end;
    if _row >= lodStep then  begin
      p2   := getGrdP(_col,_row-lodStep) ;
      p2.Z := p2.Z*zScaleFactor ;
    end;
    if _col+lodStep <= demFrameSize then  begin
      p3   := getGrdP(_col+lodStep,_row) ;
      p3.Z := p3.Z*zScaleFactor ;
    end;
    if _row+lodStep <= demFrameSize then  begin
      p4   := getGrdP(_col,_row+lodStep) ;
      p4.Z := p4.Z*zScaleFactor ;
    end;
    // left column
    if _col = 0  then  begin
      if _row = 0 then begin // upper row
        nn := triangleNormal(p0, p3, p4) ;
        Result := nn ;
        exit ;
      end
      else if _row = demFrameSize then begin // bottom row
        nn := triangleNormal(p0, p2, p3) ;
        Result := nn ;
        exit ;
      end
      else begin  //middle row
        nn2 := triangleNormal(p0, p2, p3) ;
        nn3 := triangleNormal(p0, p3, p4) ;
        vectorAdd(nn,nn2,nn3) ;
        nn.X := nn.X/2.0 ;
        nn.Y := nn.Y/2.0 ;
        nn.Z := nn.Z/2.0 ;
        Result := nn ;
        exit ;
      end;
    end
    // right column
    else  if _col = demFrameSize-1  then  begin
      if _row = 0 then begin // upper row
        nn := triangleNormal(p0, p4, p1) ;
        Result := nn ;
        exit ;
      end
      else  if _row = demFrameSize then begin // bottom row
        nn := triangleNormal(p0, p1, p2) ;
        Result := nn ;
        exit ;
      end
      else begin  //middle row
        nn1 := triangleNormal(p0, p1, p2) ;
        nn4 := triangleNormal(p0, p4, p1) ;
        vectorAdd(nn,nn1,nn4) ;
        nn.X := nn.X/2.0 ;
        nn.Y := nn.Y/2.0 ;
        nn.Z := nn.Z/2.0 ;
        Result := nn ;
        exit ;
      end;
    end
    // bottom row , middle column
    else if _row = demFrameSize then begin
      nn1 := triangleNormal(p0, p1, p2) ;
      nn2 := triangleNormal(p0, p2, p3) ;
      vectorAdd(nn,nn1,nn2) ;
      nn.X := nn.X/2.0 ;
      nn.Y := nn.Y/2.0 ;
      nn.Z := nn.Z/2.0 ;
      Result := nn ;
      exit ;
    end
    // upper row , middle column
    else if _row = 0 then begin
      nn3 := triangleNormal(p0, p3, p4) ;
      nn4 := triangleNormal(p0, p4, p1) ;
      vectorAdd(nn,nn3,nn4) ;
      nn.X := nn.X/2.0 ;
      nn.Y := nn.Y/2.0 ;
      nn.Z := nn.Z/2.0 ;
      Result := nn ;
      exit ;
    end
    else begin
      nn1 := triangleNormal(p0, p1, p2) ;
      nn2 := triangleNormal(p0, p2, p3) ;
      nn3 := triangleNormal(p0, p3, p4) ;
      nn4 := triangleNormal(p0, p4, p1) ;
      vectorAdd(an1,nn1,nn2) ;
      vectorAdd(an2,nn3,nn4) ;
      vectorAdd(nn,an1,an2) ;
      nn.X := nn.X/4.0 ;
      nn.Y := nn.Y/4.0 ;
      nn.Z := nn.Z/4.0 ;
      Result := nn ;
      exit ;
    end ;
  end ;

  function TGIS_Renderer3DAbstract.getShpP(
    const _i       : Integer       ;
    const _pardata : T_arColorData
  ) : TVector3f ;
  var
   pp: TVector3f;
  begin
    pp.X := _pardata[_i].P.X ;
    pp.Y := _pardata[_i].P.Y ;
    pp.Z := _pardata[_i].P.Z ;
    Result := pp ;
  end;

  procedure TGIS_Renderer3DAbstract.setShpN(
    const _k       : Integer       ;
    var   _pardata : T_arColorData
  ) ;
  var
    nn, nn2, nn3   : TVector3f ;
    p0, p1, p2, p3 : TVector3f ;
    i, j, kk, k    : Integer      ;
  begin
      i  := 0    ;
      k  := _k   ;
      kk := k -3 ;
      while True do begin
        if i>=kk then
          break
        else begin
          p0 := getShpP(i,_pardata) ;
          if i = 0 then
            j := k-4
          else
            j := i -2 ;
          p1 := getShpP(j,_pardata) ;
          p2 := getShpP(i+1,_pardata) ;
          p3 := getShpP(i+2,_pardata) ;

          if srcShpType = TGIS_ShapeType.Polygon then begin
            nn2 := triangleNormal(p0, p2, p1) ;
            nn3 := triangleNormal(p0, p3, p2) ;
            if renderer3D = [TRenderer3D.FMX] then begin
              nn2.X := -nn2.X ;
              nn3.X := -nn3.X ;
            end
            else if renderer3D = [TRenderer3D.OGL] then begin
              nn2.Y := -nn2.Y ;
              nn3.Y := -nn3.Y ;
            end;
          end
          else begin
            nn2 := triangleNormal(p0, p1, p2) ;
            nn3 := triangleNormal(p0, p2, p3) ;
            if renderer3D = [TRenderer3D.FMX] then begin
              nn2.X := -nn2.X ;
              nn3.X := -nn3.X ;
            end
            else if renderer3D = [TRenderer3D.OGL] then begin
              nn2.Y := -nn2.Y ;
              nn3.Y := -nn3.Y ;
            end;
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

  function  TGIS_Renderer3DAbstract.vectorAdd(
    var   _out : TVector3f ;
    const _in1 : TVector3f ;
    const _in2 : TVector3f
  ) : TVector3f ;
  begin
    _out.X := _in1.X + _in2.X ;
    _out.Y := _in1.Y + _in2.Y ;
    _out.Z := _in1.Z + _in2.Z ;
    Result := _out ;
  end;

  procedure TGIS_Renderer3DAbstract.textureValue(
    var   _pardata : T_arTextureData ;
    const _k       : Integer         ;
    const _col     : Integer         ;
    const _row      : Integer         ;
    const _z       : Double
  ) ;
  var
    pp    : TVector3f ;
    isset : Boolean   ;
  begin
    if imageTexture then begin
      if noImg then begin
        if _z = defaultZ then begin
          isset := False ;
          if (_k mod 2)= 0 then begin // even
            pp := getGrdP(_col, _row-lodStep) ;
            if pp.Z <> defaultZ then begin
              _pardata[_k].Tu := tuValue(_col) ;
              _pardata[_k].Tv := tvValue(_row-lodStep) ;
               isset := True ;
            end;
            if isset = False then begin
              pp := getGrdP(_col+lodStep, _row) ;
              if pp.Z <> defaultZ then begin
                _pardata[_k].Tu := tuValue(_col+lodStep) ;
                _pardata[_k].Tv := tvValue(_row) ;
                isset := True ;
              end;
            end;
            if (isset = False) and (_col>=lodStep) then begin
              pp := getGrdP(_col-lodStep, _row) ;
              if pp.Z <> defaultZ then begin
                _pardata[_k].Tu := tuValue(_col-lodStep) ;
                _pardata[_k].Tv := tvValue(_row) ;
                isset := True ;
              end;
            end;
            if (isset = False) and (_col>=lodStep)
               and (_row>=lodStep) then  begin
              pp := getGrdP(_col-lodStep, _row-lodStep) ;
              if pp.Z <> defaultZ then begin
                _pardata[_k].Tu := tuValue(_col-lodStep) ;
                _pardata[_k].Tv := tvValue(_row-lodStep) ;
                isset := True ;
              end;
            end;
            if isset = False then begin
              _pardata[_k].Tu := tuValue(_col) ;
              _pardata[_k].Tv := tvValue(_row) ;
            end;
          end
          else begin  // odd
            pp := getGrdP(_col, _row+lodStep) ;
            if pp.Z <> defaultZ then begin
              _pardata[_k].Tu := tuValue(_col) ;
              _pardata[_k].Tv := tvValue(_row+lodStep) ;
              isset := True ;
            end;
            if isset = False then begin
              pp := getGrdP(_col+lodStep, _row) ;
              if pp.Z <> defaultZ then begin
                _pardata[_k].Tu := tuValue(_col+lodStep) ;
                _pardata[_k].Tv := tvValue(_row) ;
                isset := True ;
              end;
            end;
            if isset = False then begin
              pp := getGrdP(_col+lodStep, _row+lodStep) ;
              if pp.Z <> defaultZ then begin
                _pardata[_k].Tu := tuValue(_col+lodStep) ;
                _pardata[_k].Tv := tvValue(_row+lodStep) ;
                isset := True ;
              end;
            end;
            if (isset = False) and (_col >= lodStep) then begin
              pp := getGrdP(_col-lodStep, _row) ;
              if pp.Z <> defaultZ then begin
                _pardata[_k].Tu := tuValue(_col-lodStep) ;
                _pardata[_k].Tv := tvValue(_row) ;
                isset := True ;
              end;
            end;
            if isset = False then begin
              _pardata[_k].Tu := tuValue(_col) ;
              _pardata[_k].Tv := tvValue(_row) ;
            end;
          end;
          if demWall = TGIS_Viewer3DDemWall.Invisible then begin
            _pardata[_k].Tu := tuValue(_col) ;
            _pardata[_k].Tv := tvValue(_row) ;
          end;
        end
        else begin // _z <> defaultZ
          _pardata[_k].Tu := tuValue(_col) ;
          _pardata[_k].Tv := tvValue(_row) ;
        end
      end        // noImg
      else begin // isImg
        _pardata[_k].Tu := tuValue(_col) ;
        _pardata[_k].Tv := tvValue(_row) ;
      end
    end          // imageTexture
    else begin
      _pardata[_k].Tu := 0.5 ;
      _pardata[_k].Tv := (_z*zUnit+zLevel-zLevelMin)/zDelta ;
      if zScaleFactor <> 0 then begin
        _pardata[_k].Tv := _pardata[_k].Tv / zScaleFactor ;
        if _pardata[_k].Tv > 1 then _pardata[_k].Tv := 0.99 ;
        if _pardata[_k].Tv < 0 then _pardata[_k].Tv := 0.01 ;
      end
      else
        _pardata[_k].Tv := _pardata[_k].Tv / 0.001 ;
    end;
      _pardata[_k].Color := color2ARGB(demTransparency,255,255,255);
  end;

  function TGIS_Renderer3DAbstract.color2ARGB(
    const _a : DWORD ;
    const _r : DWORD ;
    const _g : DWORD ;
    const _b : DWORD
  ) : DWORD ;
  begin
    Result := ( _a shl 24 ) or
              ( _r shl 16 ) or
              ( _g shl  8 ) or
                _b ;
  end;

  function TGIS_Renderer3DAbstract.color2RGB(
    const _r : DWORD ;
    const _g : DWORD ;
    const _b : DWORD
  ) : DWORD ;
  begin
    Result := ( _r shl 16 ) or
              ( _g shl  8 ) or
                _b ;
  end;

  procedure TGIS_Renderer3DAbstract.makeLeftWall(
    var   _pardata : T_arTextureData
  ) ;
  var
    ptg1, ptg2, ptg3 : TGIS_Renderer3DVertex ;
    delta, delta1    : Double          ;
  begin
    delta  := Abs(_pardata[2].P.X - _pardata[0].P.X) ;
    delta1 := Abs(_pardata[0].P.X - normExt.XMin) ;
    if (_pardata[0].P.Z <> defaultZ) and
       (_pardata[1].P.Z <> defaultZ) then begin
        ptg1 := _pardata[0] ;
        ptg1.Tu := 0 ;
        ptg1.Tv := 0 ;
        ptg2 := _pardata[1] ;
        ptg2.Tu := 0 ;
        ptg2.Tv := 0 ;
        makeVerticalWall(ptg1, ptg2, 0) ;
    end
    else if (_pardata[0].P.Z <> defaultZ) and
            (_pardata[1].P.Z  = defaultZ) then begin
      ptg1 := _pardata[0] ;
      ptg1.Tu := 0.5 ;
      ptg1.Tv := 0 ;
      ptg2 := _pardata[1] ;
      ptg2.Tu := 0.5 ;
      ptg2.Tv := 0 ;
      ptg3 := ptg1 ;
      ptg3.P.Z := defaultZ ;
      ptg3.Tu := 0.5 ;
      ptg3.Tv := 0 ;
      ptg1.Tv := getTvValue(ptg1.P.Z) ;
      if _pardata[1].P.Y > normExt.YMin then begin
        if demWall = TGIS_Viewer3DDemWall.Solid then
          changeWallTexture(ptg3,ptg2,ptg1) ;
      setDemWallNormals(ptg3,ptg2,ptg1) ;
      addToWTLBufT(ptg3,ptg2,ptg1) ;
      end;
    end
    else if (_pardata[0].P.Z = defaultZ) and
      (_pardata[1].P.Z <> defaultZ) then begin
      ptg1 := _pardata[0] ;
      ptg1.Tu := 0.5 ;
      ptg1.Tv := 0 ;
      ptg2 := _pardata[1];
      ptg2.Tu := 0.5 ;
      ptg2.Tv := 0 ;
      ptg3 := ptg2 ;
      ptg3.P.Z := defaultZ ;
      ptg3.Tu := 0.5 ;
      ptg3.Tv := 0 ;
      ptg2.Tv := getTvValue(ptg2.P.Z) ;
      if (_pardata[0].P.Y < normExt.YMax) and
         (delta1 < delta)  then  begin
        if demWall = TGIS_Viewer3DDemWall.Solid then
          changeWallTexture(ptg3,ptg2,ptg1) ;
        setDemWallNormals(ptg3,ptg2,ptg1) ;
        addToWTLBufT(ptg3,ptg2,ptg1) ;
      end;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.makeRightWall(
    var   _pardata : T_arTextureData ;
    const _len     : Integer
  ) ;
  var
    ptg1, ptg2, ptg3 : TGIS_Renderer3DVertex ;
    k                : Integer         ;
    delta, delta1    : Double          ;
  begin
    k := _len ;
    delta  := Abs(_pardata[2].P.X - _pardata[0].P.X) ;
    delta1 := Abs(_pardata[k].P.X - normExt.XMax) ;
    if (_pardata[k].P.Z <> defaultZ) and
       (_pardata[k+1].P.Z <> defaultZ) then begin
      ptg1 := _pardata[k] ;
      ptg1.Tu := 0 ;
      ptg1.Tv := 0 ;
      ptg2 := _pardata[k+1] ;
      ptg2.Tu := 0 ;
      ptg2.Tv := 0 ;
      makeVerticalWall(ptg1, ptg2, 1) ;
    end
    else if (_pardata[k].P.Z <> defaultZ) and
            (_pardata[k+1].P.Z = defaultZ) then begin  // RightWall
      ptg1 := _pardata[k] ;
      ptg1.P.Z := defaultZ ;
      ptg1.Tu := 0.5 ;
      ptg1.Tv := 0 ;
      ptg2 := _pardata[k] ;
      ptg2.Tu := 0.5 ;
      ptg2.Tv := getTvValue(ptg2.P.Z) ;
      ptg3 := _pardata[k+1] ;
      ptg3.Tu := 0.5 ;
      ptg3.Tv := 0 ;
      if (_pardata[k+1].P.Y > normExt.YMin) and (delta1 < delta) then begin
        if demWall = TGIS_Viewer3DDemWall.Solid then
          changeWallTexture(ptg3,ptg2,ptg1) ;
        setDemWallNormals(ptg1,ptg2,ptg3) ;
        addToWTLBufT(ptg1,ptg2,ptg3) ;
      end;
    end
    else if (_pardata[k].P.Z = defaultZ) and
            (_pardata[k+1].P.Z <> defaultZ) then begin
      ptg1 := _pardata[k] ;
      ptg1.Tu := 0.5 ;
      ptg1.Tv := 0 ;
      ptg2 := _pardata[k+1] ;
      ptg2.Tu := 0.5 ;
      ptg2.Tv := 0 ;
      ptg2.Tv := getTvValue(ptg2.P.Z) ;
      ptg3 := ptg2 ;
      ptg3.P.Z := defaultZ ;
      ptg3.Tu := 0.5 ;
      ptg3.Tv := 0 ;
      if (_pardata[k].P.Y < normExt.YMax) and (delta1 < delta) then begin
        if demWall = TGIS_Viewer3DDemWall.Solid then
          changeWallTexture(ptg3,ptg2,ptg1) ;
        setDemWallNormals(ptg1,ptg2,ptg3) ;
        addToWTLBufT(ptg1,ptg2,ptg3) ;
      end;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.makeUpperWall(
    var   _pardata : T_arTextureData ;
    const _len     : Integer
  ) ;
  var
    ptg1, ptg2, ptg3 : TGIS_Renderer3DVertex ;
    i, k             : Integer         ;
  begin
    drawStrip := False ;
    k := _len ;
    for i := 2 to k do begin // UpperWall
      if i mod 2 <> 0 then
        continue ;

      if (_pardata[i-1].P.Z = defaultZ) and
         (_pardata[i+1].P.Z = defaultZ) then begin
        if (_pardata[i-2].P.Z = defaultZ) and
           (_pardata[i ].P.Z <> defaultZ) then begin
          ptg1 := _pardata[i-2] ;
          ptg1.Tu := 0.5 ;
          ptg1.Tv := 0 ;
          ptg3 := _pardata[i] ;
          ptg2 := ptg3 ;
          ptg2.P.Z := defaultZ ;
          ptg2.Tu := 0.5 ;
          ptg2.Tv := 0 ;
          ptg3.Tu := 0.5 ;
          ptg3.Tv := getTvValue(ptg3.P.Z) ;
          if demWall = TGIS_Viewer3DDemWall.Solid then
            changeWallTexture(ptg3,ptg2,ptg1) ;
          setDemWallNormals(ptg3,ptg2,ptg1) ;
          addToWTLBufT(ptg3,ptg2,ptg1) ;
        end
        else if (_pardata[i-2].P.Z <> defaultZ) and
                (_pardata[i ].P.Z <> defaultZ) then begin
          ptg1 := _pardata[i-2] ;
          ptg1.Tu := 0 ;
          ptg1.Tv := 0 ;
          ptg2 := _pardata[i ] ;
          ptg2.Tu := 0 ;
          ptg2.Tv := 0 ;
          makeVerticalWall(ptg1, ptg2, 2) ;
        end
        else if (_pardata[i-2].P.Z <> defaultZ) and
                (_pardata[i ].P.Z = defaultZ) then begin
          ptg1 := _pardata[i] ;
          ptg1.Tu := 0.5 ;
          ptg1.Tv := 0 ;
          ptg3 := _pardata[i-2] ;
          ptg2 := ptg3 ;
          ptg2.P.Z := defaultZ ;
          ptg2.Tu := 0.5 ;
          ptg2.Tv := 0 ;
          ptg3.Tu := 0.5 ;
          ptg3.Tv := getTvValue(ptg3.P.Z) ;
          if demWall = TGIS_Viewer3DDemWall.Solid then
            changeWallTexture(ptg3,ptg2,ptg1) ;
          setDemWallNormals(ptg3,ptg2,ptg1) ;
          addToWTLBufT(ptg3,ptg2,ptg1) ;
        end;
      end
      else if (_pardata[i-1].P.Z <> defaultZ) and
              (_pardata[i+1].P.Z <> defaultZ) then begin
        ptg1 := _pardata[i-1] ;
        ptg1.Tu := 0 ;
        ptg1.Tv := 0 ;
        ptg2 := _pardata[i+1] ;
        ptg2.Tu := 0 ;
        ptg2.Tv := 0 ;
        makeVerticalWall(ptg1, ptg2, 2) ;
        drawStrip := True ;
      end
      else if (_pardata[i-1].P.Z = defaultZ) and
              (_pardata[i+1].P.Z <> defaultZ) then begin
        ptg1 := _pardata[i-1] ;
        ptg1.Tu := 0.5 ;
        ptg1.Tv := 0 ;
        ptg3 := _pardata[i+1] ;
        ptg2 := ptg3 ;
        ptg2.P.Z := defaultZ ;
        ptg2.Tu := 0.5 ;
        ptg2.Tv := 0 ;
        ptg3.Tu := 0.5 ;
        ptg3.Tv := getTvValue(ptg3.P.Z) ;
        if demWall = TGIS_Viewer3DDemWall.Solid then
          changeWallTexture(ptg3,ptg2,ptg1) ;
        setDemWallNormals(ptg3,ptg2,ptg1) ;
        addToWTLBufT(ptg3,ptg2,ptg1) ;
        drawStrip := True ;
      end
      else if (_pardata[i-1].P.Z <> defaultZ) and
              (_pardata[i+1].P.Z = defaultZ) then begin
        ptg1 := _pardata[i+1] ;
        ptg1.Tu := 0.5 ;
        ptg1.Tv := 0 ;
        ptg3 := _pardata[i-1] ;
        ptg2 := ptg3 ;
        ptg2.P.Z := defaultZ ;
        ptg2.Tu := 0.5 ;
        ptg2.Tv := 0 ;
        ptg3.Tu := 0.5 ;
        ptg3.Tv := getTvValue(ptg3.P.Z) ;
        if demWall = TGIS_Viewer3DDemWall.Solid then
          changeWallTexture(ptg3,ptg2,ptg1) ;
        setDemWallNormals(ptg1,ptg2,ptg3) ;
        addToWTLBufT(ptg1,ptg2,ptg3) ;
        drawStrip := True ;
      end;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.makeBottomWall(
    var   _pardata : T_arTextureData ;
    const _len     : Integer
  ) ;
  var
    ptg1, ptg2, ptg3 : TGIS_Renderer3DVertex ;
    i, k             : Integer         ;
  begin
    drawStrip := False ;
    k :=_len ;
    for i := 2 to k do begin // BottomWall
      if i mod 2 <> 0 then
        continue ;

      if (_pardata[i-2].P.Z = defaultZ) and
         (_pardata[i].P.Z = defaultZ) then begin
        if (_pardata[i-1].P.Z = defaultZ) and
           (_pardata[i+1].P.Z <> defaultZ) then begin
          ptg1 := _pardata[i-1] ;
          ptg1.Tu := 0.5 ;
          ptg1.Tv := 0 ;
          ptg2 := _pardata[i+1] ;
          ptg2.Tu := 0.5 ;
          ptg2.Tv := getTvValue(ptg2.P.Z) ;
          ptg3 := ptg2 ;
          ptg3.P.Z := defaultZ ;
          ptg3.Tu := 0.5 ;
          ptg3.Tv := 0 ;
          if demWall = TGIS_Viewer3DDemWall.Solid then
            changeWallTexture(ptg3,ptg2,ptg1) ;
          setDemWallNormals(ptg3,ptg2,ptg1) ;
          addToWTLBufT(ptg3,ptg2,ptg1) ;
        end
        else if (_pardata[i-1].P.Z <> defaultZ) and
                (_pardata[i+1].P.Z = defaultZ) then begin
          ptg1 := _pardata[i+1] ;
          ptg1.Tu := 0.5 ;
          ptg1.Tv := 0 ;
          ptg2 := _pardata[i-1] ;
          ptg2.Tu := 0.5 ;
          ptg2.Tv := getTvValue(ptg2.P.Z) ;
          ptg3 := ptg2 ;
          ptg3.P.Z := defaultZ ;
          ptg3.Tu := 0.5 ;
          ptg3.Tv := 0 ;
          if demWall = TGIS_Viewer3DDemWall.Solid then
            changeWallTexture(ptg3,ptg2,ptg1) ;
          setDemWallNormals(ptg3,ptg2,ptg1) ;
          addToWTLBufT(ptg3,ptg2,ptg1) ;
        end
        else if (_pardata[i-1].P.Z <> defaultZ) and
                (_pardata[i+1].P.Z <> defaultZ) then begin
          ptg1 := _pardata[i-1] ;
          ptg2 := _pardata[i+1] ;
          makeVerticalWall(ptg1, ptg2, 3) ;
        end
      end // both lower = defaultZ
      else if (_pardata[i-2].P.Z <> defaultZ) and
              (_pardata[i ].P.Z <> defaultZ) then begin
        ptg1 := _pardata[i-2] ;
        ptg2 := _pardata[i  ] ;
        makeVerticalWall(ptg1, ptg2, 3) ;
        drawStrip := True ;
      end
      else if (_pardata[i-2].P.Z = defaultZ) and
              (_pardata[i  ].P.Z <> defaultZ) then begin
        ptg1 := _pardata[i-2] ;
        ptg1.Tu := 0.5 ;
        ptg1.Tv := 0 ;
        ptg2 := _pardata[i  ] ;
        ptg2.Tu := 0.5 ;
        ptg2.Tv := getTvValue(ptg2.P.Z) ;
        ptg3 := ptg2 ;
        ptg3.P.Z := defaultZ ;
        ptg3.Tu := 0.5 ;
        ptg3.Tv := 0 ;
        if demWall = TGIS_Viewer3DDemWall.Solid then
          changeWallTexture(ptg3,ptg2,ptg1) ;
        setDemWallNormals(ptg3,ptg2,ptg1) ;
        addToWTLBufT(ptg3,ptg2,ptg1) ;
        drawStrip := True ;
      end
      else if (_pardata[i-2].P.Z <> defaultZ) and
              (_pardata[i ].P.Z = defaultZ) then begin
        ptg1 := _pardata[i  ] ;
        ptg1.Tu := 0.5 ;
        ptg1.Tv := 0 ;
        ptg2 := _pardata[i-2] ;
        ptg2.Tu := 0.5 ;
        ptg2.Tv := getTvValue(ptg2.P.Z) ;
        ptg3 := ptg2 ;
        ptg3.P.Z := defaultZ ;
        ptg3.Tu := 0.5 ;
        ptg3.Tv := 0 ;
        if demWall = TGIS_Viewer3DDemWall.Solid then
          changeWallTexture(ptg3,ptg2,ptg1) ;
        setDemWallNormals(ptg3,ptg2,ptg1) ;
        addToWTLBufT(ptg3,ptg2,ptg1) ;
        drawStrip := True ;
      end;
    end; //for
  end;

  procedure TGIS_Renderer3DAbstract.setDemTexture(
    var   _pardata : T_arTextureData ;
    const _len     : Integer   ;
    const _check   : Boolean
  ) ;
  var
    k         : Integer     ;
    test_ext  : TGIS_Extent ;
    delta,
    delta1    : Double      ;
    isset     : Boolean     ;
  begin
    if noDem then begin
      addDemMeshTile(_pardata, 0, _len -2) ;
      exit;
    end;
    k := _len - 2 ;
    test_ext := normExt ;

    if _pardata[1].P.Y > test_ext.YMax then exit ;
    if _pardata[0].P.Y < test_ext.YMin then exit ;

    if demWall = TGIS_Viewer3DDemWall.Invisible then begin
      drawDemWalls(_pardata, k) ;
      exit ;
    end;

    isset     := False ;
    drawStrip := True ;

    if _check then begin // check if vertical wall exists
      delta  := Abs(_pardata[2].P.X - _pardata[0].P.X) ;
      delta1 := Abs(_pardata[0].P.X - test_ext.XMin) ;
      if delta1 <= delta then begin // LeftWall
        makeLeftWall( _pardata ) ;
      end;

      k := _len - 2 ;
      delta1 := Abs(_pardata[k].P.X - test_ext.XMax) ;
      if delta1 <= delta then begin // RightWall
        makeRightWall(_pardata, k) ;
      end;

      // UpperWall
      delta  := Abs(_pardata[1].P.Y - _pardata[0].P.Y) ;
      delta1 := Abs(_pardata[1].P.Y - test_ext.YMin) ;
      if delta1 < 0.01 then
        delta1 := 0 ;

      k := _len - 2 ;
      if ((_pardata[1].P.Y < test_ext.YMin) and (delta1 < delta)) or
          (delta1 = 0) then begin
        isset := True ;
        makeUpperWall(_pardata, k) ;
      end;

      // BottomWall
      delta1 := Abs(_pardata[0].P.Y - test_ext.YMax) ;
      k := _len - 2 ;
      if ((_pardata[0].P.Y > test_ext.YMax) and (delta1 < delta)) or
          (delta1 = 0) then begin
        isset := True ;
        makeBottomWall(_pardata, k) ;
      end;
    end;

    if drawStrip then
      drawDemWalls(_pardata, k) ;

    if isset or (wTLBufT.NumObjects > demFrameSize/2) then
      saveWTLBufT ;
  end;

  procedure TGIS_Renderer3DAbstract.drawDemWalls(
    var   _pardata : T_arTextureData ;
    const _len     : Integer
  ) ;
  var
    k, i, first, tri : Integer ;
    instrip          : Boolean ;
    function check( const _i      : Integer ;
                    const _mode   : Integer
                  ) : Boolean ;
    begin
      Result := False ;
      case _mode of
        1 : if not((_pardata[_i-1].P.Z = sglIgnoreAbove) and  // no IgnoreAbove
                   (_pardata[_i  ].P.Z = sglIgnoreAbove) and
                   (_pardata[_i+1].P.Z = sglIgnoreAbove)) then Result := True ;
        2 : if not((_pardata[_i-1].P.Z = sglIgnoreBelow) and  // no IgnoreBelow
                   (_pardata[_i  ].P.Z = sglIgnoreBelow) and
                   (_pardata[_i+1].P.Z = sglIgnoreBelow)) then Result := True ;
      end;
    end;

  begin
    first   := 0 ;
    tri     := 0 ;
    k       := _len ;
    instrip := False ;
    for i := 1 to k do begin
      if check(i,1) and check(i,2) then begin  // DemZ only
        instrip := True ;
        inc( tri ) ;
      end
      else begin    // differentZ
        if instrip then begin
          addDemMeshTile(_pardata, first, tri) ;
          instrip := False ;
          tri := 0 ;
        end;
        first := i ;
      end;
    end;
    if tri > 0 then addDemMeshTile(_pardata, first, tri)
  end;

  procedure TGIS_Renderer3DAbstract.makeVerticalWall(
    var _ptg1   : TGIS_Renderer3DVertex ;
    var _ptg2   : TGIS_Renderer3DVertex ;
    const _side : Integer
  ) ;
  var
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    if demWall = TGIS_Viewer3DDemWall.Invisible then exit ;
    ptg3 := _ptg2 ;
    ptg3.P.Z := defaultZ ;
    ptg3.Tu := 0.5 ;
    ptg3.Tv := 0 ;

    _ptg1.Tu := 0.5 ;
    _ptg1.Tv := getTvValue(_ptg1.P.Z) ;

    _ptg2.Tu := 0.5 ;
    _ptg2.Tv := getTvValue(_ptg2.P.Z) ;

    if demWall = TGIS_Viewer3DDemWall.Solid then
      changeWallTexture(_ptg1, _ptg2, ptg3) ;

    case _side of
      0 : begin   // left
            setDemWallNormals(ptg3,_ptg2,_ptg1) ;
            addToWTLBufT(ptg3,_ptg2,_ptg1) ;
          end;
      1 : begin   // right
            setDemWallNormals(_ptg1,_ptg2,ptg3) ;
            addToWTLBufT(_ptg1,_ptg2,ptg3) ;
          end;
      2 : begin  // top
             setDemWallNormals(ptg3,_ptg2,_ptg1) ;
            addToWTLBufT(ptg3,_ptg2,_ptg1) ;
          end;
      3 : begin  // bottom
            setDemWallNormals(_ptg1,_ptg2,ptg3) ;
            addToWTLBufT(_ptg1,_ptg2,ptg3) ;
          end;
    end;

    _ptg2 := _ptg1 ;
    _ptg2.P.Z := defaultZ ;
    _ptg2.Tv := 0 ;

    case _side of
      0 : begin   // left
            setDemWallNormals(ptg3,_ptg1,_ptg2) ;
            addToWTLBufT(ptg3,_ptg1,_ptg2) ;
          end;
      1 : begin   // right
            setDemWallNormals(_ptg2,_ptg1,ptg3) ;
            addToWTLBufT(_ptg2,_ptg1,ptg3) ;
          end;
      2 : begin  // top
            setDemWallNormals(ptg3,_ptg1,_ptg2) ;
            addToWTLBufT(ptg3,_ptg1,_ptg2) ;
          end;
      3 : begin  // bottom
            setDemWallNormals(_ptg2,_ptg1,ptg3) ;
            addToWTLBufT(_ptg2,_ptg1,ptg3) ;
          end;
    end;
  end;

  function TGIS_Renderer3DAbstract.getTvValue(
    const _z : Double
  ) : Single ;
  var
    val : Double ;
    tv  : Single ;
  begin
    if zDelta = 0 then begin
      Result := 0.5 ;
      exit;
    end;

    val := _z * zUnit + zLevel ;
    tv := ( val - zLevelMin ) / zDelta ;
    if zScaleFactor <> 0 then
      tv := tv / zScaleFactor
    else
      tv := tv / 0.001 ;
    Result := tv ;
  end;

  procedure TGIS_Renderer3DAbstract.changeWallTexture(
    var _p1 : TGIS_Renderer3DVertex ;
    var _p2 : TGIS_Renderer3DVertex ;
    var _p3 : TGIS_Renderer3DVertex
  ) ;
  begin
    _p1.Tu := 0.1 ;
    _p2.Tu := 0.1 ;
    _p3.Tu := 0.1 ;
  end;

  procedure TGIS_Renderer3DAbstract.setDemWallNormals(
    var _ptg1 : TGIS_Renderer3DVertex ;
    var _ptg2 : TGIS_Renderer3DVertex ;
    var _ptg3 : TGIS_Renderer3DVertex
  ) ;
  var
    v0, v1, v2 : TVector3f;
  begin
    v0.X := _ptg1.P.X ;
    v0.Y := _ptg1.P.Y ;
    v0.Z := _ptg1.P.Z ;
    v1.X := _ptg2.P.X ;
    v1.Y := _ptg2.P.Y ;
    v1.Z := _ptg2.P.Z ;
    v2.X := _ptg3.P.X ;
    v2.Y := _ptg3.P.Y ;
    v2.Z := _ptg3.P.Z ;
    _ptg1.N := triangleNormal(v2, v0, v1) ;
    _ptg2.N := triangleNormal(v0, v1, v2) ;
    _ptg3.N := triangleNormal(v1, v2, v0) ;
  end;

  procedure TGIS_Renderer3DAbstract.addToWTLBufT(
    const _ptg1 : TGIS_Renderer3DVertex;
    const _ptg2 : TGIS_Renderer3DVertex;
    const _ptg3 : TGIS_Renderer3DVertex
  ) ;
  var
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    if length( wTLBufT.TriLst ) = 0 then
      SetLength( wTLBufT.TriLst    ,  videoVertexBufferSize ) ;

    ptg1 := _ptg1 ;
    ptg2 := _ptg2 ;
    ptg3 := _ptg3 ;
    if renderer3D <> [TRenderer3D.DX9] then begin
      ptg1.P.Y := -ptg1.P.Y ;
      ptg2.P.Y := -ptg2.P.Y ;
      ptg3.P.Y := -ptg3.P.Y ;
    end;
    if wTLBufT.NumPoints+6 < videoVertexBufferSize then begin
      wTLBufT.TriLst[wTLBufT.NumPoints] := ptg1 ;
      inc(wTLBufT.NumPoints) ;
      wTLBufT.TriLst[wTLBufT.NumPoints] := ptg2 ;
      inc(wTLBufT.NumPoints) ;
      wTLBufT.TriLst[wTLBufT.NumPoints] := ptg3 ;
      inc(wTLBufT.NumPoints) ;
      inc(wTLBufT.NumObjects) ;
    end
    else begin
      saveWTLBufT ;
      wTLBufT.TriLst[wTLBufT.NumPoints] := ptg1 ;
      inc(wTLBufT.NumPoints) ;
      wTLBufT.TriLst[wTLBufT.NumPoints] := ptg2 ;
      inc(wTLBufT.NumPoints) ;
      wTLBufT.TriLst[wTLBufT.NumPoints] := ptg3 ;
      inc(wTLBufT.NumPoints) ;
      inc(wTLBufT.NumObjects) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.addToTinBufT(
    const _ptg1 : TGIS_Renderer3DVertex ;
    const _ptg2 : TGIS_Renderer3DVertex ;
    const _ptg3 : TGIS_Renderer3DVertex
  ) ;
  var
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    if length( tinBufT.TriLst ) = 0 then
      SetLength( tinBufT.TriLst    ,  initVideoVBSize ) ;

    if tinBufT.NumPoints+3 < length( tinBufT.TriLst ) then begin
      tinBufT.TriLst[tinBufT.NumPoints] := _ptg1 ;
      inc(tinBufT.NumPoints) ;
      tinBufT.TriLst[tinBufT.NumPoints] := _ptg2 ;
      inc(tinBufT.NumPoints) ;
      tinBufT.TriLst[tinBufT.NumPoints] := _ptg3 ;
      inc(tinBufT.NumPoints) ;
      inc(tinBufT.NumObjects) ;
    end
    else begin
      ptg1 := _ptg1 ;
      ptg2 := _ptg2 ;
      ptg3 := _ptg3 ;

      if safeRendering then begin
        saveTinBufT ;
        SetLength( tinBufT.TriLst ,  initVideoVBSize )
      end
      else
        SetLength( tinBufT.TriLst ,  length(tinBufT.TriLst) + videoVBIncrement  ) ;

      tinBufT.TriLst[tinBufT.NumPoints] := ptg1 ;
      inc(tinBufT.NumPoints) ;
      tinBufT.TriLst[tinBufT.NumPoints] := ptg2 ;
      inc(tinBufT.NumPoints) ;
      tinBufT.TriLst[tinBufT.NumPoints] := ptg3 ;
      inc(tinBufT.NumPoints) ;
      inc(tinBufT.NumObjects) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.addToTinBufC(
    const _ptg1 : TGIS_Renderer3DVertex;
    const _ptg2 : TGIS_Renderer3DVertex;
    const _ptg3 : TGIS_Renderer3DVertex
  ) ;
  var
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    if length( tinBufC.TriLst ) = 0 then
      SetLength( tinBufC.TriLst    ,  initVideoVBSize ) ;

    if tinBufC.NumPoints+3 < length( tinBufC.TriLst ) then begin
      tinBufC.TriLst[tinBufC.NumPoints] := _ptg1 ;
      inc(tinBufC.NumPoints) ;
      tinBufC.TriLst[tinBufC.NumPoints] := _ptg2 ;
      inc(tinBufC.NumPoints) ;
      tinBufC.TriLst[tinBufC.NumPoints] := _ptg3 ;
      inc(tinBufC.NumPoints) ;
      inc(tinBufC.NumObjects) ;
    end
    else begin
      ptg1 := _ptg1 ;
      ptg2 := _ptg2 ;
      ptg3 := _ptg3 ;

      if safeRendering then begin
        saveTinBufC ;
        SetLength( tinBufC.TriLst ,  initVideoVBSize )
      end
      else
        SetLength( tinBufC.TriLst ,  length(tinBufC.TriLst) + videoVBIncrement  ) ;

      tinBufC.TriLst[tinBufC.NumPoints] := ptg1 ;
      inc(tinBufC.NumPoints) ;
      tinBufC.TriLst[tinBufC.NumPoints] := ptg2 ;
      inc(tinBufC.NumPoints) ;
      tinBufC.TriLst[tinBufC.NumPoints] := ptg3 ;
      inc(tinBufC.NumPoints) ;
      inc(tinBufC.NumObjects) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.addToMPatchBufC(
    const _ptg1 : TGIS_Renderer3DVertex;
    const _ptg2 : TGIS_Renderer3DVertex;
    const _ptg3 : TGIS_Renderer3DVertex
  ) ;
  var
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    if length(mpatchBufC.TriLst) = 0 then begin
      SetLength( mpatchBufC.TriLst ,  initVideoVBSize ) ;
    end;

    if mpatchBufC.NumPoints+3 < length(mpatchBufC.TriLst) then begin
      mpatchBufC.TriLst[mpatchBufC.NumPoints] := _ptg1 ;
      inc(mpatchBufC.NumPoints) ;
      mpatchBufC.TriLst[mpatchBufC.NumPoints] := _ptg2 ;
      inc(mpatchBufC.NumPoints) ;
      mpatchBufC.TriLst[mpatchBufC.NumPoints] := _ptg3 ;
      inc(mpatchBufC.NumPoints) ;
      inc(mpatchBufC.NumObjects) ;
    end
    else begin
      ptg1 := _ptg1 ;
      ptg2 := _ptg2 ;
      ptg3 := _ptg3 ;

      if safeRendering or (length(mpatchBufC.TriLst) >= maxVertexBufferSize) then begin
        saveMPatchBufC ;
        SetLength( mpatchBufC.TriLst ,  initVideoVBSize )
      end
      else
        SetLength( mpatchBufC.TriLst ,  length(mpatchBufC.TriLst) + videoVBIncrement  ) ;

      mpatchBufC.TriLst[mpatchBufC.NumPoints] := ptg1 ;
      inc(mpatchBufC.NumPoints) ;
      mpatchBufC.TriLst[mpatchBufC.NumPoints] := ptg2 ;
      inc(mpatchBufC.NumPoints) ;
      mpatchBufC.TriLst[mpatchBufC.NumPoints] := ptg3 ;
      inc(mpatchBufC.NumPoints) ;
      inc(mpatchBufC.NumObjects) ;
    end;
  end;

  function TGIS_Renderer3DAbstract.saveTinBufT
    : Integer;
  begin
    if tinBufT.NumObjects = 0 then begin
      Result := S_OK ;
      exit ;
    end;

    Result := addTinMeshTileT(tinBufT.NumObjects) ;
    if Result <> S_OK then begin
      safeRendering := True ;
      tinBufT.NumPoints  := 0 ;
      tinBufT.NumObjects := 0 ;
      SetLength( tinBufT.TriLst, 0 ) ;

      if (Result = -2147024882) or (Result = $0505)  then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                            'Out of memory', 6 )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                              'saveTinBufT', 6 ) ;
    end;
    fillBufferInfoEntry(8, 0, 0, extractColorFromShape(8)) ;
    tinBufT.NumPoints  := 0 ;
    tinBufT.NumObjects := 0 ;
    SetLength( tinBufT.TriLst, 0 ) ;
    addVectorTilesInfo( 8, meshTinTilesCounterT -1 ) ;
    Result := S_OK ;
  end;

  function TGIS_Renderer3DAbstract.saveTinBufC
    : Integer ;
  begin
    if tinBufC.NumObjects = 0 then
      begin
        Result := S_OK ;
        exit ;
      end;

    Result := addTinMeshTileC(tinBufC.NumObjects) ;
    if Result <> S_OK then begin
      safeRendering := True ;
      tinBufC.NumPoints  := 0 ;
      tinBufC.NumObjects := 0 ;
      SetLength( tinBufC.TriLst, 0 ) ;

      if (Result = -2147024882) or (Result = $0505)  then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                            'Out of memory', 7 )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                              'saveTinBufC', 7 );
    end;

    fillBufferInfoEntry(7, 0, 0, extractColorFromShape(7)) ;
    tinBufC.NumPoints  := 0 ;
    tinBufC.NumObjects := 0 ;
    SetLength( tinBufC.TriLst, 0 ) ;
    addVectorTilesInfo( 7, meshTinTilesCounterC -1 ) ;
    Result := S_OK ;
  end;

  function TGIS_Renderer3DAbstract.extractColorFromShape(
    const _buftype : ShortInt
  ) : TGIS_Color;
  var
    prm : TGIS_ParamsSectionVector;
    shp : TGIS_Shape;
    cl  : TGIS_Color;
  begin
    shp := currentSHP;
    if not assigned( shp ) then exit;

    cl := TGIS_Color.FromARGB( 255, 255, 255, 255 ) ;
    prm := currentSHP.Params;

    case _buftype of
      1,3        :
        case currentSHP.ShapeType of
          TGIS_ShapeType.Point     : Result := prm.Marker.OutlineColor ;
          TGIS_ShapeType.MultiPoint: Result := prm.Marker.OutlineColor ;
          TGIS_ShapeType.Arc       : Result := prm.Line.OutlineColor ;
          TGIS_ShapeType.Polygon   : Result := prm.Area.OutlineColor ;
        end;
      2,4,7,8,11 :
        case currentSHP.ShapeType of
          TGIS_ShapeType.Point     : Result := prm.Marker.Color ;
          TGIS_ShapeType.MultiPoint: Result := prm.Marker.Color ;
          TGIS_ShapeType.Arc       : Result := prm.Line.Color ;
          TGIS_ShapeType.Polygon   : Result := prm.Area.Color ;
        end;
      9,10      :
        case currentSHP.ShapeType of
          TGIS_ShapeType.Arc       : Result := prm.Line.Color ;
          TGIS_ShapeType.Point     : Result := prm.Marker.Color ;
          TGIS_ShapeType.MultiPoint: Result := prm.Marker.Color
          else                       Result := prm.Area.Color ;
        end;
      12        :
        Result := TGIS_Color.FromARGB(255, 255, 255, 255);
      else Result := cl;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.fillBufferInfoEntry(
    const _buftype : ShortInt ;
    const _shpid   : TGIS_Uid  ;
    const _part    : Integer ;
    const _cl      : TGIS_Color
  ) ;
  var
    k : Integer ;

    function getentry : Integer ;
    var
      i, j, l : Integer ;
    begin
      if bufSelInfoCounter = 0 then begin
        SetLength( arBufSelInfo, 1 ) ;
        bufSelInfoCounter := 1 ;
        Result := bufSelInfoCounter -1 ;
        exit;
      end;
      j := -1 ;
      l :=  0 ;
      case _buftype of
        1  : l := meshVectTilesCounterCW  ;
        2  : l := meshVectTilesCounterCR  ;
        3  : l := meshVectTilesCounterTW  ;
        4  : l := meshVectTilesCounterTR  ;
        5  : l := meshDemTilesCounter     ;
        6  : l := meshWallTilesCounter    ;
        7  : l := meshTinTilesCounterC    ;
        8  : l := meshTinTilesCounterT    ;
        9  : l := meshMPatchTilesCounterC ;
        10 : l := meshMPatchTilesCounterT ;
        11 : l := meshLineTilesCounter    ;
        12 : l := meshPointTilesCounter   ;
      end;
      for i := 0 to bufSelInfoCounter -1 do begin
        if (arBufSelInfo[i].BufferType = _buftype) and
           (arBufSelInfo[i].BufferIndex = l ) then
        j := i ;
      end;
      if j = -1 then begin
        inc( bufSelInfoCounter ) ;
        SetLength( arBufSelInfo, bufSelInfoCounter ) ;
        Result := bufSelInfoCounter -1 ;
      end
      else
        Result := j ;
    end;

    procedure setEntry( const _k   : Integer ;
                        const _cnt : Integer ;
                        const _val : Integer ;
                        const _cl  : TGIS_Color) ;
    var
      lgt : Integer ;
      rec : TGIS_Renderer3DShapeSelectionInfo ;
      shp : TGIS_Shape;
    begin
      shp := currentSHP;
      arBufSelInfo[_k].BufferType := _buftype ;
      arBufSelInfo[_k].BufferIndex := _cnt ;
      if currentVL = nil then begin
        if curDemSize > 0 then
          arBufSelInfo[_k].Layer := curDem[0].Lx
      end
      else
        arBufSelInfo[_k].Layer := currentVL ;
      if not assigned( arBufSelInfo[_k].ShpInfo) then begin
        arBufSelInfo[_k].ShpInfo := TList<TGIS_Renderer3DShapeSelectionInfo>.Create ;
        arBufSelInfo[_k].SelIdx := TList<Integer>.Create ;
      end;
      lgt := arBufSelInfo[_k].ShpInfo.Count -1;
      rec.Uid := _shpid ;
      rec.Part := _part ;
      rec.Offset := _val ;
      if _shpid > 0 then begin
        rec.Selected := shp.IsSelected;
        rec.Color := _cl ;
      end;
      arBufSelInfo[_k].ShpInfo.Add( rec ) ;
    end;

  begin
    k := getentry ;
    case _buftype of
      1  : // vTLBufCW polygon wall
           setEntry(k, meshVectTilesCounterCW  , vTLBufCW.NumPoints, _cl) ;
      2  : // vTLBufCR polygon roof
           setEntry(k, meshVectTilesCounterCR  , vTLBufCR.NumPoints, _cl ) ;
      3  : // vTLBufCW polygon wall
           setEntry(k, meshVectTilesCounterTW  , vTLBufTW.NumPoints, _cl ) ;
      4  : // vTLBufCR polygon roof
           setEntry(k, meshVectTilesCounterTR  , vTLBufTR.NumPoints, _cl ) ;
      5  : // DEM
           setEntry(k, meshDemTilesCounter  -1 , getDemBufSize(meshDemTilesCounter -1), _cl) ;
      6  : // DEM wall
           setEntry(k, meshWallTilesCounter -1 , getWallBufSize(meshWallTilesCounter -1), _cl) ;
      7  : // Color TIN
           setEntry(k, meshTinTilesCounterC    , tinBufC.NumPoints, _cl ) ;
      8  : // Textured TIN
           setEntry(k, meshTinTilesCounterT    , tinBufT.NumPoints, _cl ) ;
      9  : // Color Multipatch
           setEntry(k, meshMPatchTilesCounterC , mpatchBufC.NumPoints, _cl ) ;
      10 : // Textured Multipatch
           setEntry(k, meshMPatchTilesCounterT , mpatchBufT.NumPoints, _cl ) ;
      11 : // lines, edges
           setEntry(k, meshLineTilesCounter    , wPLBuf.NumPoints, _cl ) ;
      12 : // points
           setEntry(k, meshPointTilesCounter   , wPBuf.NumObjects, _cl ) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.doCallBackTin(
           _shp   : TGIS_Shape ;
    var   _abort : Boolean
  ) ;
  var
    shp_type   : TGIS_ShapeType  ;
    ptg        : TGIS_Point3D    ;
    ptg1       : TGIS_Renderer3DVertex   ;
    ar_color   : DWORD           ;
    cl         : TGIS_Color      ;
    r,g,b      : Byte        ;
    part_no,
    no_parts,
    point_no,
    no_points  : Integer         ;
    valx, valy : Double          ;
    cbuffer    : array of TGIS_Renderer3DVertex   ;
    tbuffer    : array of TGIS_Renderer3DVertex ;
    v0, v1, v2 : TVector3f       ;
    a          : Cardinal        ;
    dex, dey, z,
    exxmi, exymi : Double          ;

      procedure storeTriangle ;
      begin
        if tinBmp then begin
          v0 := tbuffer[0].P ;
          v1 := tbuffer[1].P ;
          v2 := tbuffer[2].P ;
          if renderer3D = [TRenderer3D.DX9] then begin
            tbuffer[0].N := triangleNormal(v0, v1, v2) ;
            tbuffer[1].N := triangleNormal(v1, v2, v0) ;
            tbuffer[2].N := triangleNormal(v2, v0, v1) ;
          end
          else begin
            tbuffer[0].N := triangleNormal(v2, v1, v0) ;
            tbuffer[0].N.Y := -tbuffer[0].N.Y ;
            tbuffer[1].N := tbuffer[0].N ;
            tbuffer[2].N := tbuffer[0].N ;
          end;
        end
        else begin
          v0 := cbuffer[0].P ;
          v1 := cbuffer[1].P ;
          v2 := cbuffer[2].P ;
          if renderer3D = [TRenderer3D.DX9] then begin
            cbuffer[0].N := triangleNormal(v0, v1, v2) ;
            cbuffer[1].N := triangleNormal(v1, v2, v0) ;
            cbuffer[2].N := triangleNormal(v2, v0, v1) ;
          end
          else begin
            cbuffer[0].N := triangleNormal(v2, v1, v0) ;
            cbuffer[0].N.X := -cbuffer[0].N.X ;
            cbuffer[1].N := cbuffer[0].N ;
            cbuffer[2].N := cbuffer[0].N ;
          end;
        end;

        if tinBmp then
          addToTinBufT(tbuffer[0], tbuffer[1], tbuffer[2])
        else
          addToTinBufC(cbuffer[0], cbuffer[1], cbuffer[2]) ;
      end;

  begin
    currentSHP := _shp ;
    SetLength(cbuffer, 3) ;
    SetLength(tbuffer, 3) ;
    shp_type := _shp.ShapeType ;
    if shp_type = TGIS_ShapeType.Polygon then begin
      SetLength( cbuffer, 3) ;
      SetLength( tbuffer, 3) ;

      if _shp.IsSelected then
        cl := oGIS.SelectionGisColor
      else
        cl := _shp.Params.Area.Color ;

      if assigned( currentML ) then begin
         a := TruncS( currentML.Transparency / 100 * 255 ) ;
        if not _abort then begin
          dex := _shp.GetPoint3D( 0, 2 ).X  - _shp.GetPoint3D( 0, 0 ).X ;
          dey := _shp.GetPoint3D( 0, 1 ).Y  - _shp.GetPoint3D( 0, 0 ).Y ;
          exxmi := _shp.GetPoint3D( 0, 0 ).X ;
          exymi := _shp.GetPoint3D( 0, 0 ).Y ;
        end
        else begin
          dex := currentML.ProjectedExtent.XMax - currentML.ProjectedExtent.XMin ;
          dey := currentML.ProjectedExtent.YMax - currentML.ProjectedExtent.YMin ;
          exxmi := currentML.ProjectedExtent.XMin ;
          exymi := currentML.ProjectedExtent.YMin ;
        end
      end
      else begin
        a := TruncS( currentVL.Transparency / 100 * 255 ) ;
        dex := currentVL.ProjectedExtent.XMax - currentVL.ProjectedExtent.XMin ;
        dey := currentVL.ProjectedExtent.YMax - currentVL.ProjectedExtent.YMin ;
        exxmi := currentVL.ProjectedExtent.XMin ;
        exymi := currentVL.ProjectedExtent.YMin ;
      end;
      r := cl.R ;
      g := cl.G ;
      b := cl.B ;
      if renderer3D = [TRenderer3D.OGL] then
        ar_color := color2ARGB(a,b,g,r)
      else
        ar_color := color2ARGB(a,r,g,b) ;

      no_parts := _shp.GetNumParts -1 ;

      _shp.Lock( TGIS_Lock.Projection ) ;
      for part_no := 0 to no_parts do begin
        no_points := _shp.GetPartSize( part_no )-1 ;
        for point_no := 0 to no_points do begin
          if point_no > 2 then
            break ;
          ptg := _shp.GetPoint3D( part_no, point_no ) ;
          if (ptg.M < ZMMIN) or (ptg.M > ZMMAX) then ptg.M := 0 ;
          if (ptg.Z < ZMMIN) or (ptg.Z > ZMMAX) then ptg.Z := 0 ;
          z := ptg.Z ;
{
          if assigned( currentML ) then
            currentML.Project3D_Ref( ptg )
          else
            currentVL.Project3D_Ref( ptg ) ;
}
          if bIgnoreEllipsoidHeight then
            ptg.Z := z ;

          ptg.Z := ptg.Z * _shp.Params.ScaleZ + _shp.Params.FalseZ ;
//          ptg.Z := ptg.Z * zFactor ;
          valx := (( vectExtent.XMax - ptg.X) /
                   ( vectExtent.XMax - vectExtent.XMin)) ;
          valy := (( vectExtent.YMax - ptg.Y) /
                   ( vectExtent.YMax - vectExtent.YMin)) ;
          ptg1.P.X := roughExtent.XMax - valx *
                     (roughExtent.XMax - roughExtent.XMin) ;
          ptg1.P.Y := roughExtent.YMin + valy *
                     (roughExtent.YMax - roughExtent.YMin) ;
          if renderer3D <> [TRenderer3D.DX9] then ptg1.P.Y := -ptg1.P.Y ;
          ptg1.P.Z := projFactor * (ptg.Z - zLevel) / zUnit ;
          ptg1.P.Z := ptg1.P.Z * zFactor ;
          ptg1.N   := vector3Init(0,1,0) ;
          ptg1.Color := ar_color ;
          if tinBmp then begin
            tbuffer[point_no].P := ptg1.P ;
            tbuffer[point_no].N := ptg1.N ;
            tbuffer[point_no].Color := ptg1.Color ;
            tbuffer[point_no].Tu :=   (ptg.X - exxmi)/dex ;
            if renderer3D = [TRenderer3D.DX9] then
              tbuffer[point_no].Tv := 1-(ptg.Y - exymi)/dey
            else
              tbuffer[point_no].Tv := 1-(ptg.Y - exymi)/dey ; //???
          end
          else
            cbuffer[point_no] := ptg1 ;
        end;
        storeTriangle ;
      end;
      _shp.Unlock ;
    end;
  end;

  function TGIS_Renderer3DAbstract.isTinLayer : Boolean ;
  var
    i         : Integer ;
    no_parts  : Integer ;
    no_points : Integer ;
    shp       : TGIS_Shape ;
    fptg, ptg : TGIS_Point3D ;
    no_m      : Boolean ;
    const_z   : Boolean ;
  begin
    Result := True ;
    no_m := True ;
    const_z := True ;
    if not (( currentVL.DefaultShapeType = TGIS_ShapeType.Polygon ) or
            ( TGIS_ShapeType.Polygon in currentVL.SupportedShapes )) then begin
      Result := False ;
      exit;
    end ;

    shp := currentVL.FindFirst ;
    if not assigned( shp ) then begin
      Result := False ;
      exit;
    end
    else
//    if assigned( shp ) then
    if shp.ShapeType <> TGIS_ShapeType.Polygon then begin
      Result := False ;
      exit;
    end ;
    while assigned( shp ) do begin
      no_parts := shp.GetNumParts -1 ;
      for i := 0 to no_parts do begin
        no_points := shp.GetPartSize(i) ;
        fptg := shp.GetPoint3D(i, 0) ;
        case no_points of
          3 :  ptg  := shp.GetPoint3D(i, 2) ;
          4 :  begin
                 ptg  := shp.GetPoint3D(i, 3) ;
                 if (fptg.X <> ptg.X) or
                    (fptg.Y <> ptg.Y) or
                    (fptg.Z <> ptg.Z) then begin
                   Result := False ;
                   exit ;
                 end ;
               end ;
        else   begin
                 Result := False ;
                 exit ;
               end ;
        end ;
        if (fptg.M <> 0) or (shp.Params.FalseM <> 0) then begin
          Result := False ;
          exit ;
        end;
        if fptg.Z <> ptg.Z then const_z := False ;
      end ;
      if not Result then break ;
      shp := currentVL.FindNext ;
    end ;

    if not Result then
      if no_m and not const_z then
        Result := True ;
  end;

  procedure TGIS_Renderer3DAbstract.addNewPolyRoofRow ;
  var
    k : Integer ;
  begin
    if arVectPolyRoofCounterC > 0 then begin
      k := high( arVectPolyRoofC ) + 1 ;
      SetLength(arVectPolyRoofC, k + 1) ;
      SetLength(arVectPolyRoofC[k], videoVertexBufferSize );
    end;
    if arVectPolyRoofCounterT > 0  then begin
      k := high( arVectPolyRoofT ) + 1 ;
      SetLength(arVectPolyRoofT, k + 1) ;
      SetLength(arVectPolyRoofT[k], videoVertexBufferSize );
    end;
  end;

  function TGIS_Renderer3DAbstract.isShpVisible(
    const _ext  : TGIS_Extent3D;
    var   _size : Integer
  ) : Boolean ;
  var
    dd : Double;
  begin
    dd := Sqrt( Sqr(_ext.XMax - _ext.XMin ) +
                Sqr(_ext.YMax - _ext.YMin ) +
                Sqr(_ext.ZMax - _ext.ZMin ) ) ;

    if oGIS.CS.EPSG = currentVL.CS.EPSG then
      _size := TruncS(dd/GetPixelSize.X)
    else
      _size := TruncS(windowWidth*dd/(cutVectExtentL.XMax-cutVectExtentL.XMin)) ;

    if _size >= iVectorSmartSize then
      Result := True
    else
      Result := False ;
  end;

  function TGIS_Renderer3DAbstract.doSimplify1(
    const _shp : TGIS_Shape
  ) : TGIS_Shape ;
  var
    shp, res : TGIS_Shape  ;
    ex       : TGIS_Extent ;
    dd, fact : Double      ;
  begin
    if not assigned( _shp ) then begin
      Result := nil ;
      exit ;
    end;
    if not((_shp.ShapeType = TGIS_ShapeType.Arc) or
           (_shp.ShapeType = TGIS_ShapeType.Polygon)) then begin
      Result := _shp ;
      exit ;
    end;

    ex := _shp.ProjectedExtent ;
    dd := Sqrt( ( ex.XMax - ex.XMin ) * ( ex.XMax - ex.XMin ) +
                ( ex.YMax - ex.YMin ) * ( ex.YMax - ex.YMin ) ) ;

    fact := dd * 0.001 ;
    res := _shp.Simplify( fact, True ) ;

    if assigned( res ) then begin
      shp := _shp.CreateCopy ;
      shp.Reset ;
      shp.CopyGeometry(res) ;
      FreeObject(res) ;
      Result := shp ;
    end
    else
      Result := nil ;

  end;

  function TGIS_Renderer3DAbstract.doSimplify2(
    const _shp : TGIS_Shape
  ) : TGIS_Shape ;
  var
    res, res1   : TGIS_Shape   ;
    ex          : TGIS_Extent  ;
    dd          : Double       ;
    point_num   : Integer      ;
    part_num    : Integer      ;
    ptg,
    first_pnt,
    cntrl_pnt   : TGIS_Point3D ;
    tolres      : Boolean      ;

    // Calculate acceptable distance between points
    // _pt1   first point
    // _pt2   second point
    //        true if acceptable
    function tolerance(
      const _pt1  : TGIS_Point3D ;
      const _pt2  : TGIS_Point3D
    ) : Boolean ;
    var
      distance : Double ;
    begin
      distance := Sqrt( Sqr( _pt1.X - _pt2.X ) + Sqr( _pt1.Y - _pt2.Y ) ) ;
      if distance > pixSize * iVectorSmartSize then
       Result := True
      else
       Result := False ;
    end;

  begin
    tolres := True ;
    if not((_shp.ShapeType = TGIS_ShapeType.Arc) or
           (_shp.ShapeType = TGIS_ShapeType.Polygon)) then begin
      Result := _shp ;
      exit ;
    end;

    case _shp.ShapeType of
      TGIS_ShapeType.Polygon : res := TGIS_ShapePolygon.Create(
                              nil, nil, False, 0, currentVL, TGIS_DimensionType.XYZM
                              ) ;
      TGIS_ShapeType.Arc     : res := TGIS_ShapeArc.Create(
                              nil, nil, False, 0, currentVL, TGIS_DimensionType.XYZM
                              ) ;
    end;

    res.Lock( TGIS_Lock.Extent ) ;
    try
      for part_num := 0 to _shp.GetNumParts -1 do begin
        res.AddPart ;
        for point_num := 0 to _shp.GetPartSize(part_num) -1 do begin
          ptg := _shp.GetPoint3D( part_num, point_num ) ;
          if point_num = 0  then begin
            res.AddPoint3D(ptg) ;
            first_pnt := ptg ;
            cntrl_pnt := first_pnt ;
            tolres := True ;
          end
          else if point_num = _shp.GetPartSize(part_num) -1  then
            res.AddPoint3D(ptg)
          else
            tolres := tolerance(cntrl_pnt, ptg) ;

          if point_num < _shp.GetPartSize( part_num ) -1 then // not last
          if tolres then begin
            cntrl_pnt := ptg ;
            res.AddPoint3D(ptg) ;
          end;
        end;
      end ;
    finally
      res.Unlock ;
      ex := _shp.ProjectedExtent ;
      dd := Sqrt( ( ex.XMax - ex.XMin ) * ( ex.XMax - ex.XMin ) +
                  ( ex.YMax - ex.YMin ) * ( ex.YMax - ex.YMin ) ) ;

      if dd < pixSize*iVectorSmartSize then begin
        FreeObject(res) ;
        Result := nil ;
      end
      else begin
        res1 := _shp.CreateCopy ;
        res1.Reset ;
        res1.CopyGeometry( res ) ;
        FreeObject(res) ;
        Result := res1 ;
      end;
    end;
  end;

  function TGIS_Renderer3DAbstract.doSimplify(
    const _shp : TGIS_Shape
  ) : TGIS_Shape ;
  var
    res  : TGIS_Shape ;
    k    : Integer    ;
    tpl  : TGIS_Topology ;
  begin
    if not bVectorSimplification then begin
      Result := _shp ;
      if not checkGeometrySize ( Result ) then
        Result := nil ;
      exit ;
    end;

    res := doSimplify1( _shp ) ;

    // repeat until success or > 5 steps
    k := 0 ;
    if assigned ( res) then  begin
      if not checkGeometrySize ( res ) then
      repeat
        iVectorSmartSize :=
          iVectorSmartSize +1 ;
        res := doSimplify2( _shp ) ;
        if not assigned ( res) then begin
          Result := nil ;
          exit ;
        end;
        inc(k) ;
        if k > 5 then  begin
          Result := nil ;
          exit ;
        end;
      until checkGeometrySize( res ) ;
    end;

    if assigned( res ) then begin
      tpl := TGIS_Topology.Create ;
      try
        tpl.FixShape( res ) ;
      finally
        FreeObject( tpl ) ;
      end ;
    end;

    Result := res ;
  end;

  function TGIS_Renderer3DAbstract.checkGeometrySize(
    const _shp: TGIS_Shape
  ) : Boolean ;
  var
    part_no : Integer ;
    sum     : Integer ;
  begin

    if _shp.ShapeType = TGIS_ShapeType.Arc then begin
      if oGIS.TwipsToPixels(_shp.Params.Line.Width) > 1 then begin
        for part_no := 0 to _shp.GetNumParts -1 do
          if _shp.GetPartSize( part_no ) >= videoVertexBufferSize/4 -2 then begin
            Result := False ;
            errorMsg := Format( _rsrc( GIS_RS_ERR_3D_GEOMETRYSIZE ),
                                [ currentVL.Name, _shp.Uid ]
                              ) ;
            exit ;
          end;
      end
      else begin
        for part_no := 0 to _shp.GetNumParts -1 do
          if _shp.GetPartSize( part_no ) >= videoVertexBufferSize/2 then begin
            Result := False ;
            errorMsg := Format( _rsrc( GIS_RS_ERR_3D_GEOMETRYSIZE ),
                                [ currentVL.Name, _shp.Uid ]
                              ) ;
            exit ;
          end;
      end;
    end
    else  begin
      sum := 0 ;
      for part_no := 0 to _shp.GetNumParts -1 do begin
        sum := sum + _shp.GetPartSize( part_no ) ;
      end;
      if sum >= videoVertexBufferSize/2 then begin
        Result := False ;
        errorMsg := Format( _rsrc( GIS_RS_ERR_3D_GEOMETRYSIZE ),
                            [ currentVL.Name, _shp.Uid ]
                          ) ;
        exit ;
      end;
    end;
    Result := True ;
  end;

  procedure TGIS_Renderer3DAbstract.drawShapeFromCache(
    const _shp   : TObject ;
    var   _abort : Boolean
  ) ;
  var
    shp : TGIS_Renderer3DShape;
  begin
    shp := TGIS_Renderer3DShape(_shp) ;
    _abort := False ;
    case shp.ShapeType of
      TGIS_ShapeType.Polygon    : if not drawPolyFromCache(shp)  then
                                    _abort := True ;
      TGIS_ShapeType.Arc        : if not drawArcFromCache(shp)   then
                                    _abort := True ;
      TGIS_ShapeType.Point      : if not drawPointFromCache(shp) then
                                    _abort := True ;
      TGIS_ShapeType.MultiPoint : if not drawPointFromCache(shp) then
                                    _abort := True ;
    end;
    if shpNo mod GIS_PROGRESS_TRESHOLD = 0 then begin
      if renderer3D <> [TRenderer3D.FMX] then  //?
        if oGIS.HourglassShake then exit ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.adjustBasement(
    const _mode  : Integer        ;
    var _pardata : T_arColorData  ;
    var _cbuffer : T_arColorData  ;
    const _j     : Integer        ;
    const _k     : Integer        ;
    const _val   : Double         ;
    const _val1  : Double
  ) ;
  var
    i, k   : Integer ;
    deltaZ : Double  ;
  begin
    deltaZ:= 0 ;
    k := _k - 1 ;

      if _mode = 0 then
        for i := 0 to k do begin
          if i mod 2 = 0 then begin
            deltaZ := _pardata[i].P.Z - _val ;
            _pardata[i].P.Z := _val ;
          end
          else begin
            _pardata[i].P.Z := _pardata[i].P.Z - deltaZ ;
          end;
        end
        else
        for i := 0 to k do begin
          if i mod 2 = 0 then begin
            deltaZ := _cbuffer[i].P.Z - _val ;
            _cbuffer[i].P.Z := _val ;
          end
          else begin
            _cbuffer[i].P.Z := _cbuffer[i].P.Z - deltaZ ;
          end;
        end;

  end;

  function TGIS_Renderer3DAbstract.drawPolyFromCache(
    const _shp   : TObject
  ) : Boolean ;
  var
    pardata    : T_arColorData  ;
    cbuffer    : T_arColorData  ;
    k,kk,jj    : Integer        ;
    shp_type   : TGIS_ShapeType ;
    ptg        : TGIS_Point3D   ;
    last_ptg   : TGIS_Point3D   ;
    part_no    : Integer        ;
    point_no   : Integer        ;
    valx, valy : Double         ;
    ar_color   : DWORD          ;
    ou_color   : DWORD          ;
    mltp       : Double         ;
    cl         : TGIS_Color     ;
    r,g,b      : Byte           ;
    shp        : TGIS_Renderer3DShape  ;
    a          : Cardinal       ;
    lgth       : Double         ;
    area       : Double         ;
    winding    : Boolean        ;
    lab_z      : Double         ;
    lab_b      : Double         ;
    zero_m     : Boolean        ;
    col1, col2 : TGIS_Color     ;

    procedure reverse_pardata( _k : Integer ) ;
    var
      i, j, m : Integer ;
      p : TVector3f ;
    begin
      m := TruncS( _k / 2 ) ;
      for i := 2 to m -1 do begin

        if i mod 2 = 0 then
          j := _k -2 -i
        else
          j := _k -i ;

        p := pardata[i].P ;
        pardata[i].P := pardata[j].P ;
        pardata[j].P := p ;
      end;
    end;

  begin
    if not assigned(_shp) then begin
      Result := True ;
      exit ;
    end;

    lab_z := 0 ;
    lab_b := 0 ;
    lgth  := 0 ;
    zero_m := True ;
    shp := TGIS_Renderer3DShape(_shp) ;

    // shape context
    groundType := shp.Ground ;
    basementType := shp.Basement ;

    a := TruncS(currentVL.Transparency / 100 * 255) ;
    minBasement  := GIS_MAX_SINGLE ;
    minBasement1 := GIS_MAX_SINGLE ;

    shp_type := shp.ShapeType ;
    srcShpType := shp_type ;

      mltp := -1.0 ;

      cl := shp.OutlineColor ;
      r := cl.R ;
      g := cl.G ;
      b := cl.B ;
      if renderer3D = [TRenderer3D.OGL] then
        ou_color := color2ARGB(a, b, g, r)
      else
        ou_color := color2ARGB(a, r, g, b) ;

      edColor := ou_color ;

      cl := shp.Color ;
      r := cl.R ;
      g := cl.G ;
      b := cl.B ;
      if renderer3D = [TRenderer3D.OGL] then
        ar_color := color2ARGB(a, b, g, r)
      else
        ar_color := color2ARGB(a, r, g, b) ;

      if not shp.ShowCeiling then
        noRoof := True
      else
        noRoof := False ;
      if not shp.ShowWalls then
        noWall := True
      else
        noWall := False ;

      if IsStringEmpty( shp.Texture ) then
        arBmp := False
      else
        arBmp := True ;
      if IsStringEmpty( shp.OutlineTexture ) then
        ouBmp := False
      else
        ouBmp := True ;

      // Mark object selected in 2D
     if shp.IsSelected then begin
       r := oGIS.SelectionGisColor.R ;
       g := oGIS.SelectionGisColor.G ;
       b := oGIS.SelectionGisColor.B ;
       if renderer3D = [TRenderer3D.OGL] then
         ar_color := color2ARGB(Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)), b, g, r)
       else
         ar_color := color2ARGB(Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)), r, g, b) ;
       ou_color := ar_color ;
     end;
     TGIS_Tessellation(oPolyRoof).Reset ;

    area := 0 ;
    winding := True ;

    col1 := extractColorFromShape(1);
    col2 := extractColorFromShape(3);
    for part_no := 0 to shp.GetNumParts - 1 do begin
      SetLength( pardata, 2 * shp.GetPartSize(part_no) ) ;

      k := 0 ; // vertex counter
      for point_no := 0 to shp.GetPartSize(part_no) - 1 do  begin
        ptg := shp.GetPoint3D(part_no, point_no) ;
        if (ptg.M < ZMMIN) or (ptg.M > ZMMAX) then ptg.M := 0 ;
        if (ptg.Z < ZMMIN) or (ptg.Z > ZMMAX) then ptg.Z := 0 ;
        if part_no  = 0 then begin
          if point_no > 0 then begin
            area := area + (last_ptg.X*ptg.Y - ptg.X*last_ptg.Y) ;
          end ;
          last_ptg := ptg;
        end ;

        ptg.Z := ptg.Z * shp.ScaleZ ;
        ptg.M := ptg.M * shp.ScaleM ;

        if ptg.M <> 0 then zero_m := False ;

        valx := ((vectExtent.XMax - ptg.X) /
          (vectExtent.XMax - vectExtent.XMin)) ;
        valy := ((vectExtent.YMax - ptg.Y) /
          (vectExtent.YMax - vectExtent.YMin)) ;
        pardata[k].P.X := roughExtent.XMax - valx *
          (roughExtent.XMax - roughExtent.XMin) ;
        pardata[k].P.Y := roughExtent.YMin + valy *
            (roughExtent.YMax - roughExtent.YMin) ;
        if renderer3D <> [TRenderer3D.DX9] then
          pardata[k].P.Y := -pardata[k].P.Y ;

        // DEM exists among layers, get shp Z value from grid
        kk := demFrameSize - TruncS(valx * demFrameSize) ;
        jj := TruncS(valy * demFrameSize) ;
        // gisGroundOnDem
        if (jj < 0) or (jj >= demFrameSize - 1) or (kk < 0) or
          (kk >= demFrameSize - 1) Then
          pardata[k].P.Z := defaultZ
        else
          pardata[k].P.Z := zValue(kk, jj) ;

        if groundType = TGIS_3DGroundType.AboveDem then // above DEM
          pardata[k].P.Z := pardata[k].P.Z + projFactor * ptg.Z * zFactor / zUnit
        else // above 0
          if groundType = TGIS_3DGroundType.AboveZero then // above 0
            pardata[k].P.Z := projFactor * (ptg.Z - zLevel) * zFactor / zUnit ;

        if pardata[k].P.Z < minBasement then
          minBasement := pardata[k].P.Z ;

        pardata[k].Color := ou_color ;

        if point_no >= 1 then
          lgth := lgth + Sqrt(Sqr(pardata[k].P.X - pardata[k - 2].P.X) +
            Sqr(pardata[k].P.Y - pardata[k - 2].P.Y)) ;

        inc(k) ;
        // next point
        pardata[k].P.Z := pardata[k - 1].P.Z + projFactor * (ptg.M * mFactor) / zUnit;
        if pardata[k].P.Z < minBasement1 then
          minBasement1 := pardata[k].P.Z ;
        pardata[k].Color := ou_color ;

        pardata[k].P.X := pardata[k - 1].P.X ;
        pardata[k].P.Y := pardata[k - 1].P.Y ;
        inc(k) ;
      end; // end for point

      if part_no = 0 then begin
        if area < 0 then
          winding := True
        else
          winding := False ;
      end ;

      if renderer3D = [TRenderer3D.DX9] then begin
        if not winding then
          reverse_pardata( k ) ;
      end
      else begin
        if winding then
          reverse_pardata( k ) ;
      end ;

      // draw walls
      setShpN(k, pardata) ;
      if basementType = TGIS_3DBasementType.Lowest then
        if ((groundType = TGIS_3DGroundType.OnDem) or
          (groundType = TGIS_3DGroundType.AboveDem)) then
          adjustBasement(0, pardata, cbuffer, 0, k,
                         minBasement, minBasement1) ;

      lab_z := pardata[1].P.Z;
      lab_b := pardata[0].P.Z ;

      if (srcShpType = TGIS_ShapeType.Polygon) and (not noRoof) then
          loadMesh(part_no, pardata, k, True) ;

        // draw walls
      if not zero_m then begin
        if k > 1 then
        if not noWall then
        if not ouBmp then begin
          fillBufferInfoEntry(1, shp.Uid, part_no, col1) ;
          triStripToListC(False, pardata, k - 2)
        end
        else begin
          fillBufferInfoEntry(3, shp.Uid, part_no, col2) ;
          drawVectorTexture(shp, False, pardata, 1, k, lgth) ;
        end;
      end;

        // draw vectorEdges
      if vectorEdges then begin
        drawVectorEdges(k, pardata) ;
      end;
    end; // end for part

    // Draw polygon roof
    if not noRoof then begin

      if not arBmp then
        fillBufferInfoEntry(2, shp.Uid, 0, extractColorFromShape(2))
      else
        fillBufferInfoEntry(4, shp.Uid, 0, extractColorFromShape(4)) ;

      if not drawPolygonRoof(ar_color, mltp, shp, True) then begin
         Result := False ;
         exit ;
       end;
    end;
    shp.LabelZ := lab_z ;
    shp.LabelB := lab_b ;

    Result := True ;
  end;

  function TGIS_Renderer3DAbstract.drawArcFromCache(
    const _shp   : TObject
  ) : Boolean ;
  var
    pardata         : T_arColorData  ;
    cbuffer         : T_arColorData  ;
    cbuf_siz        : Integer        ;
    k,kk,jj         : Integer        ;
    shp_type        : TGIS_ShapeType ;
    ptg             : TGIS_Point3D   ;
    part_no         : Integer        ;
    point_no        : Integer        ;
    valx, valy      : Double         ;
    ar_color        : DWORD          ;
    ou_color        : DWORD          ;
    differentZ      : Boolean        ;
    isarc           : Boolean        ;
    px              : Double         ;
    r,g,b           : Byte           ;
    shp             : TGIS_Renderer3DShape  ;
    a               : Cardinal       ;
    lgth            : Double         ;
    line_width      : Integer        ;
    lab_z           : Double         ;
    lab_b           : Double         ;
    lab_index       : Integer        ;
    zero_m          : Boolean        ;
    cl              : TGIS_Color     ;
    cl1,cl2,cl3,cl4,cl9,cl11 : TGIS_Color     ;
  begin
    if not assigned(_shp) then begin
      Result := True ;
      exit ;
    end;
    lab_z := 0 ;
    lab_b := 0 ;
    cbuf_siz := 0 ;
    lgth := 0 ;
    shp := TGIS_Renderer3DShape(_shp) ;

    // shape context
    groundType   := shp.Ground ;
    basementType := shp.Basement ;

    a := TruncS(currentVL.Transparency / 100 * 255) ;
    minBasement  := GIS_MAX_SINGLE ;
    minBasement1 := GIS_MAX_SINGLE ;

    shp_type := shp.ShapeType ;
    srcShpType := shp_type ;

      differentZ := False ;
      zero_m     := True  ;

      cl := shp.OutlineColor ;
      r := cl.R ;
      g := cl.G ;
      b := cl.B ;
      if renderer3D = [TRenderer3D.OGL] then
        ou_color := color2ARGB(a, b, g, r)
      else
        ou_color := color2ARGB(a, r, g, b) ;

      cl := shp.Color ;
      r := cl.R ;
      g := cl.G ;
      b := cl.B ;
      if renderer3D = [TRenderer3D.OGL] then
        ar_color := color2ARGB(a, b, g, r)
      else
        ar_color := color2ARGB(a, r, g, b) ;

      px := getPrinterPixelSize( dRadius ) ;
      if scrRatio > 1.0 then
        px := px * scrRatio ;
      if not shp.ShowCeiling then
        noRoof := True
      else
        noRoof := False ;
      if not shp.ShowWalls then
        noWall := True
      else
        noWall := False ;

      if IsStringEmpty( shp.Texture ) then
        arBmp := False
      else
        arBmp := True ;
      if IsStringEmpty( shp.OutlineTexture ) then
        ouBmp := False
      else
        ouBmp := True ;

    // Mark object selected in 2D
    if shp.IsSelected then begin
      r := oGIS.SelectionGisColor.R ;
      g := oGIS.SelectionGisColor.G ;
      b := oGIS.SelectionGisColor.B ;
      if renderer3D = [TRenderer3D.OGL] then
         ar_color := color2ARGB(Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)), b, g, r)
       else
         ar_color := color2ARGB(Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)), r, g, b) ;
      ou_color := ar_color ;
    end ;

    line_width := shp.Size ;

    cl1  := extractColorFromShape(1);
    cl2  := extractColorFromShape(2);
    cl3  := extractColorFromShape(3);
    cl4  := extractColorFromShape(4);
    cl9  := extractColorFromShape(9);
    cl11 := extractColorFromShape(11);

    for part_no := 0 to shp.GetNumParts - 1 do begin
      if line_width > 1 then
        SetLength( pardata, 4 * shp.GetPartSize(part_no) + 2 )
      else
        SetLength( pardata, 2 * shp.GetPartSize(part_no) ) ;

      k := 0 ; // vertex counter
      isarc := False ;
      lab_index := TruncS( shp.GetPartSize(part_no)/2.0 ) ;
      for point_no := 0 to shp.GetPartSize(part_no) - 1 do begin
        ptg := shp.GetPoint3D(part_no, point_no) ;
        if (ptg.M < ZMMIN) or (ptg.M > ZMMAX) then ptg.M := 0 ;
        if (ptg.Z < ZMMIN) or (ptg.Z > ZMMAX) then ptg.Z := 0 ;

        ptg.Z := ptg.Z * shp.ScaleZ ;
        ptg.M := ptg.M * shp.ScaleM ;

        if ptg.M <> 0 then zero_m := False ;

        valx := ((vectExtent.XMax - ptg.X) /
          (vectExtent.XMax - vectExtent.XMin)) ;
        valy := ((vectExtent.YMax - ptg.Y) /
          (vectExtent.YMax - vectExtent.YMin)) ;
        pardata[k].P.X := roughExtent.XMax - valx *
          (roughExtent.XMax - roughExtent.XMin) ;
        pardata[k].P.Y := roughExtent.YMin + valy *
          (roughExtent.YMax - roughExtent.YMin) ;
        if renderer3D <> [TRenderer3D.DX9] then
          pardata[k].P.Y := -pardata[k].P.Y ;
        // DEM exists among layers, get shp Z value from grid
        kk := demFrameSize - TruncS(valx * demFrameSize) ;
        jj := TruncS(valy * demFrameSize) ;
        // gisGroundOnDem
        if (jj < 0) or (jj >= demFrameSize - 1) or (kk < 0) or
          (kk >= demFrameSize - 1) Then
          pardata[k].P.Z := defaultZ
        else
          pardata[k].P.Z := zValue(kk, jj) ;

        if groundType = TGIS_3DGroundType.AboveDem then // above DEM
          pardata[k].P.Z := pardata[k].P.Z + projFactor * ptg.Z * zFactor / zUnit
        else // above 0
          if groundType = TGIS_3DGroundType.AboveZero then // above 0
            pardata[k].P.Z := projFactor * (ptg.Z - zLevel) * zFactor / zUnit ;

        if pardata[k].P.Z < minBasement then
          minBasement := pardata[k].P.Z ;

        pardata[k].Color := ou_color;

        if point_no >= 1 then
          lgth := lgth + Sqrt(Sqr(pardata[k].P.X - pardata[k - 2].P.X) +
            Sqr(pardata[k].P.Y - pardata[k - 2].P.Y)) ;

        inc(k) ;
        // next point
        pardata[k].P.Z := pardata[k - 1].P.Z + projFactor * (ptg.M * mFactor) / zUnit;
        if pardata[k].P.Z < minBasement1 then
          minBasement1 := pardata[k].P.Z ;
        pardata[k].Color := ou_color ;

        pardata[k].P.X := pardata[k - 1].P.X ;
        pardata[k].P.Y := pardata[k - 1].P.Y ;
        if (pardata[k - 1].P.Z <> pardata[k].P.Z) then
          differentZ := True ;
        if point_no = lab_index then
          lab_z := pardata[k].P.Z ;
          lab_b := pardata[k-1].P.Z ;
        inc(k) ;
      end; // end for point

      if (line_width > 1) and zero_m and noWall then begin
        fillBufferInfoEntry(9, shp.Uid, part_no, cl9);
        drawVolumetricLine( part_no, k, pardata, ar_color, shp.Size ) ;
        lab_z := lab_z + px * shp.Size ;
        continue ;
      end;

      // Arc as Polygon
      if line_width > 1 then begin
        {$IFDEF CLR}
          cbuffer := new TGIS_Renderer3DVertex[ 2 * k + 2 ] ;
        {$ELSE}
          SetLength(cbuffer, 2 * k + 2 ) ;
        {$ENDIF}
        make3dLine(shp.Size, k, cbuffer, pardata, px) ;
        // Restore pardata from cbuffer
        for kk := 0 to 2 * k + 1 do begin
          pardata[kk].P.X := cbuffer[kk].P.X ;
          pardata[kk].P.Y := cbuffer[kk].P.Y ;
          pardata[kk].P.Z := cbuffer[kk].P.Z ;
          pardata[kk].Color := ou_color ;
        end;
        k := k * 2 + 2 ;
        cbuf_siz := k ;
        isarc := True ;
      end;

      // start drawing polyline
      if line_width <= 1 then begin
        if differentZ then begin
          setShpN(k, pardata) ;
          if basementType = TGIS_3DBasementType.Lowest then
          if ((groundType = TGIS_3DGroundType.OnDem) or
              (groundType = TGIS_3DGroundType.AboveDem)) then
            adjustBasement(0, pardata, cbuffer, 0, k,
                           minBasement, minBasement1) ;

          if k > 1 then
            if not noWall then
              if not ouBmp then begin
                fillBufferInfoEntry(1, shp.Uid, part_no, cl1) ;
                triStripToListC(False, pardata, k - 2)
              end
              else begin
                fillBufferInfoEntry(3, shp.Uid, part_no, cl3) ;
                drawVectorTexture(shp, False, pardata, 1, k, lgth) ;
              end;
          // draw vectorEdges
          if vectorEdges then begin
            drawVectorEdges(k, pardata) ;
          end; // end VectorEdges
        end // differentZ
        else begin // constantZ
          if lightSwitch then
            lightOff ;
          if basementType = TGIS_3DBasementType.Lowest then
          if ((groundType = TGIS_3DGroundType.OnDem) or
              (groundType = TGIS_3DGroundType.AboveDem)) then
            adjustBasement(0, pardata, cbuffer, 0, k,
                           minBasement, minBasement1) ;
          if k > 1 then
          if not noRoof then begin
            for kk := 0 to k - 1 do  pardata[kk].Color := ar_color;
            fillBufferInfoEntry(11, shp.Uid, part_no, cl11) ;
            linStripToList1(pardata, k -1) ;
          end;
          if lightSwitch then
            lightOn ;
        end;
      end
      else begin
        // draw walls
        setShpN(k, pardata) ;
        if basementType = TGIS_3DBasementType.Lowest then
        if (groundType = TGIS_3DGroundType.OnDem) or
           (groundType = TGIS_3DGroundType.AboveDem) then
          adjustBasement(0, pardata, cbuffer, 0, k,
                         minBasement, minBasement1) ;

        if (k > 1) and not zero_m then
        if not noWall then
        if not ouBmp then begin
          fillBufferInfoEntry(1, shp.Uid, part_no, cl1) ;
          triStripToListC(False, pardata, k - 2)
        end
        else begin
          fillBufferInfoEntry(3, shp.Uid, part_no, cl3) ;
          drawVectorTexture(shp, False, pardata, 1, k, lgth) ;
        end;
        // draw VectorEdges
        if vectorEdges then
          drawVectorEdges(k, pardata) ;
      end;

      // draw polyline roof
      if isarc then begin
        if not noRoof then begin
          if not arBmp then
            fillBufferInfoEntry(2, shp.Uid, part_no, cl2)
          else
            fillBufferInfoEntry(4, shp.Uid, part_no, cl4) ;
          drawPolyLineRoof(cbuf_siz, pardata, cbuffer, ar_color, isarc,
                           shp_type, shp) ;
        end;
      end;
    end; // end for part
    shp.LabelZ := lab_z ;
    shp.LabelB := lab_b ;

    Result := True ;
  end;

  procedure TGIS_Renderer3DAbstract.drawVolumetricLine(
    const _part    : Integer       ;
    const _num     : Integer       ;
    const _pardata : T_arColorData ;
    const _color   : Cardinal      ;
    const _size    : Integer
  ) ;
  var
    k, stp : Integer ;
    ptg1, ptg2 : TGIS_Renderer3DVertex ;
    lgth, ry, rz, dx, dz, siz  : Double ;
    vec     : TGIS_Point3D ;

    function getrotz(
      const _pnt : TGIS_Renderer3DVertex ;
      const _vec : TGIS_Point3D  ;
      const _ry  : Double
    ) : Double ;
    var
      c2, s2, X, Y : Double ;
    begin
      SinCos( _ry, s2, c2 ) ;
      X := c2*(_pnt.P.X-_vec.X) + s2*(_pnt.P.Z-_vec.Z) ;
      Y := _pnt.P.Y-_vec.Y ;
      if X = 0 then
        Result := -Pi/2
      else
        Result := -ArcTan(Y/X);
    end;

    // Get new point position after rotation
    // _pnt   point to be rotated
    // _ry    vertical rotation angle
    // _rz    horizontal rotation angle
    // _vec   translation vector
    function getNewPntPos(
      const _pnt : TGIS_Renderer3DVertex ;
      const _ry  : Double ;
      const _rz  : Double ;
      const _vec : TGIS_Point3D
    ) : TGIS_Renderer3DVertex ;
    var
      c2,c3,s2,s3 : Double ;
      X,Y,Z       : Double ;

      function roty : TGIS_Renderer3DVertex ;
      begin
        Result.P.X := c2*X + 0    + s2*Z ;
        Result.P.Y := 0    + Y    + 0    ;
        Result.P.Z :=-s2*X + 0    + c2*Z ;
      end;

      function rotz : TGIS_Renderer3DVertex ;
      begin
        Result.P.X := c3*X - s3*Y + 0    ;
        Result.P.Y := s3*X + c3*Y + 0    ;
        Result.P.Z := 0    + 0    + Z    ;
      end;

    begin
      SinCos( -_ry, s2, c2 ) ;
      SinCos( -_rz, s3, c3 ) ;
      X  := _pnt.P.X ;
      Y  := _pnt.P.Y ;
      Z  := _pnt.P.Z ;
      Result := rotz ;
      X  := Result.P.X ;
      Y  := Result.P.Y ;
      Z  := Result.P.Z ;
      Result := roty ;

      Result.P.X := Result.P.X + _vec.X ;
      Result.P.Y := Result.P.Y + _vec.Y ;
      Result.P.Z := Result.P.Z + _vec.Z ;
      Result.N   := _pnt.N ;
      Result.Color := _pnt.Color ;
    end ;

    //  Volumetric section generator
    // _center     : sphere center in map units
    // _radius     : sphere radius in map units
    // _angstepx   : horizontal angle step in decimal degrees (0 - 180)
    // _angstepy   : vertical   angle step in decimal degrees (0 -  90)
    // _side       : left / right end of line section
    // _vec        : translation vector
    // _ry         : rotation along Y axis
    // _rz         : rotation along Z axis
    procedure genVolLine(
      const _center   : TGIS_Point3D     ;
      const _radius   : Double           ;
      const _angstepx : Integer          ;
      const _angstepy : Integer          ;
      const _side     : Boolean          ;
      const _vec      : TGIS_Point3D     ;
      const _ry       : Double           ;
      const _rz       : Double
      ) ;
    var
      p0, p1, p2, p3, p: TGIS_Renderer3DVertex ;
      i, j, i1, j1     : Integer       ;
      ci, ci1, cj, cj1 : Double        ;
      si, si1, sj, sj1 : Double        ;
      ip, ik           : Integer       ;

    begin
      if (_angstepx < 1) or (_angstepx > 180) then exit;
      if (_angstepy < 1) or (_angstepy > 90 ) then exit;

      j := 0 ; // lat
      if _side then begin
        ip := 90 ;
        ik := 270 ;
      end
      else begin
        ip := 270 ;
        ik := 450 ;
      end;

      while True do begin
        i := ip ; // lon
        j1 := j + _angstepy ;

        while True do begin
          i1  := i + _angstepx ;
          ci  := Cos(DegToRad(i) ) ;
          ci1 := Cos(DegToRad(i1)) ;
          cj  := Cos(DegToRad(j) ) ;
          cj1 := Cos(DegToRad(j1)) ;
          si  := Sin(DegToRad(i) ) ;
          si1 := Sin(DegToRad(i1)) ;
          sj  := Sin(DegToRad(j) ) ;
          sj1 := Sin(DegToRad(j1)) ;

          p0.P := vector3Init( _center.X + _radius * ci * sj   ,
                               _center.Y + _radius * si * sj   ,
                               _center.Z + _radius * cj    )   ;
          p0.Color := _color ;
          p1.P := vector3Init( _center.X + _radius * ci * sj1  ,
                               _center.Y + _radius * si * sj1  ,
                               _center.Z + _radius * cj1    )  ;
          p1.Color := _color ;
          p2.P := vector3Init( _center.X + _radius * ci1 * sj  ,
                               _center.Y + _radius * si1 * sj  ,
                               _center.Z + _radius * cj     )  ;
          p2.Color := _color ;
          p3.P := vector3Init( _center.X + _radius * ci1 * sj1 ,
                               _center.Y + _radius * si1 * sj1 ,
                               _center.Z + _radius * cj1    )  ;
          p3.Color := _color ;

          p0.N  := triangleNormal(p2.P, p0.P, p1.P) ;
          p1.N  := triangleNormal(p0.P, p1.P, p2.P) ;
          p2.N  := triangleNormal(p1.P, p2.P, p0.P) ;
//          if _side then
            addToMPatchBufC(
                         getNewPntPos(p0,_ry,_rz,_vec),
                         getNewPntPos(p2,_ry,_rz,_vec),
                         getNewPntPos(p1,_ry,_rz,_vec) ) ;
          p1.N  := triangleNormal(p2.P, p1.P, p3.P) ;
          p2.N  := triangleNormal(p3.P, p2.P, p1.P) ;
          p3.N  := triangleNormal(p1.P, p3.P, p2.P) ;
//          if _side then
            addToMPatchBufC(
                          getNewPntPos(p1,_ry,_rz,_vec),
                          getNewPntPos(p2,_ry,_rz,_vec),
                          getNewPntPos(p3,_ry,_rz,_vec) ) ;

          inc( i, _angstepx ) ;
          if i >= ik then break ; //360
        end;

        p := p3 ;
        p.P.X := -p.P.X ;
        p3.N  := triangleNormal(p2.P, p3.P, p.P ) ;
        p2.N  := triangleNormal(p.P , p2.P, p3.P) ;
        p.N   := triangleNormal(p3.P, p.P , p2.P) ;
        addToMPatchBufC(
                        getNewPntPos(p3,_ry,_rz,_vec),
                        getNewPntPos(p2,_ry,_rz,_vec),
                        getNewPntPos(p ,_ry,_rz,_vec) ) ;
        p3 := p ;
        p  := p2 ;
        p.P.X := -p2.P.X;
        p3.N  := triangleNormal(p2.P, p3.P, p.P ) ;
        p2.N  := triangleNormal(p.P , p2.P, p3.P) ;
        p.N   := triangleNormal(p3.P, p.P , p2.P) ;
        addToMPatchBufC(
                        getNewPntPos(p3,_ry,_rz,_vec),
                        getNewPntPos(p2,_ry,_rz,_vec),
                        getNewPntPos(p ,_ry,_rz,_vec) ) ;
        inc( j, _angstepy ) ;
        if j >= 180 then break ; //180
      end;
    end;

  // main body
  begin
    siz := _size * wndXSize / windowWidth ;
    if printBmpImgSize = 0 then
      siz := _size * wndXSize / windowWidth
    else
      siz := _size * wndXSize / printBmpOutSize.X ;
    stp := 30 ;
    k := 0 ;
    while True do begin
       ptg1 := _pardata[k  ] ;
       ptg2 := _pardata[k+2] ;
       lgth := Sqrt(Sqr(ptg1.P.X-ptg2.P.X) +
                    Sqr(ptg1.P.Y-ptg2.P.Y) +
                    Sqr(ptg1.P.Z-ptg2.P.Z)) / 2.0 ;
       vec := GisPoint3D((ptg1.P.X + ptg2.P.X)/2,
                         (ptg1.P.Y + ptg2.P.Y)/2,
                         (ptg1.P.Z + ptg2.P.Z)/2 ) ;
       dx := (ptg2.P.X - ptg1.P.X) ;
       dz := (ptg2.P.Z - ptg1.P.Z) ;

       if dx = 0 then
         ry := -Pi/2
       else
         ry := ArcTan(dz/dx) ;

       rz := getrotz(ptg1, vec, ry) ;

       genVolLine(GisPoint3D( -lgth, 0, 0), siz, stp, stp, True , vec, ry, rz );
       genVolLine(GisPoint3D(  lgth, 0, 0), siz, stp, stp, False, vec, ry, rz );
       inc(k, 2) ;
       if k +2 >= _num then break ;
    end;
  end;

  function TGIS_Renderer3DAbstract.drawVolumetricPoint(
    const _siz      : Integer          ;
    const _sym      : TGIS_MarkerStyle ;
    var   _pardata  : T_arColorData    ;
    const _px       : Double           ;
    const _color    : DWORD
  ) : Single ;
  var
    x, y, z, r : Double;
    p0, p1, p2, p3,
    p4, p5, p6, p7 : TGIS_Renderer3DVertex ;

    function generate_sphere : Single ;
    var
     i, i1, j, j1, stp : Integer ;
     ci, ci1, cj, cj1 : Double   ;
     si, si1, sj, sj1 : Double   ;
    begin
      stp := 30 ;
      j := 0 ;
      while True do begin
        i := 0 ; // lon
        j1 := j + stp ;
        while True do begin
          i1  := i + stp ;
          ci  := Cos(DegToRad(i) ) ;
          ci1 := Cos(DegToRad(i1)) ;
          cj  := Cos(DegToRad(j) ) ;
          cj1 := Cos(DegToRad(j1)) ;
          si  := Sin(DegToRad(i) ) ;
          si1 := Sin(DegToRad(i1)) ;
          sj  := Sin(DegToRad(j) ) ;
          sj1 := Sin(DegToRad(j1)) ;
          p0.P := vector3Init( x+r*ci*sj  , y+r*si*sj  , z+r*cj  ) ;
          p1.P := vector3Init( x+r*ci*sj1 , y+r*si*sj1 , z+r*cj1 ) ;
          p2.P := vector3Init( x+r*ci1*sj , y+r*si1*sj , z+r*cj  ) ;
          p3.P := vector3Init( x+r*ci1*sj1, y+r*si1*sj1, z+r*cj1 ) ;
          p0.Color := _color ;
          p1.Color := _color ;
          p2.Color := _color ;
          p3.Color := _color ;
          p0.N := triangleNormal( p2.P, p0.P, p1.P ) ;
          p1.N := triangleNormal( p0.P, p1.P, p3.P ) ;
          p2.N := triangleNormal( p3.P, p2.P, p0.P ) ;
          p3.N := triangleNormal( p1.P, p3.P, p2.P ) ;
          if renderer3D <> [TRenderer3D.DX9] then begin
            p0.N.Y := -p0.N.Y ;
            p1.N.Y := -p1.N.Y ;
            p2.N.Y := -p2.N.Y ;
            p3.N.Y := -p3.N.Y ;
          end;
          addToMPatchBufC( p0, p2, p1) ;
          addToMPatchBufC( p1, p2, p3) ;
          inc( i, stp ) ;
          if i >= 360 then break ;
        end;
        inc( j, stp ) ;
        if j >= 180 then break ;
      end;
      Result := z + r ;
    end;

    function generate_cross( const _value : Boolean ) : Single ;
    var
      i : Integer ;
    begin
      i := 0 ;
      while True do begin
        addToMPatchBufC( _pardata[i  ], _pardata[i+1], _pardata[i+2]) ;
        addToMPatchBufC( _pardata[i+2], _pardata[i+1], _pardata[i+3]) ;
        inc(i,2) ;
        if i >= 24 then break ;
      end;
      if _value then begin
        _pardata[ 1].N := triangleNormal( _pardata[ 3].P, _pardata[ 1].P,
                                          _pardata[ 5].P ) ;
        i := 3 ;
        while True do begin
          _pardata[i].N := _pardata[ 1].N ;
          inc(i,2) ;
          if i > 23 then break ;
        end;
        addToMPatchBufC( _pardata[ 3], _pardata[ 1], _pardata[ 5]) ;
        addToMPatchBufC( _pardata[ 5], _pardata[ 1], _pardata[23]) ;
        addToMPatchBufC( _pardata[ 7], _pardata[21], _pardata[ 9]) ;
        addToMPatchBufC( _pardata[ 9], _pardata[21], _pardata[19]) ;
        addToMPatchBufC( _pardata[11], _pardata[17], _pardata[13]) ;
        addToMPatchBufC( _pardata[13], _pardata[17], _pardata[15]) ;

        _pardata[ 0].N := triangleNormal( _pardata[ 4].P, _pardata[ 0].P,
                                          _pardata[ 2].P ) ;
        i := 2 ;
        while True do begin
          _pardata[i].N := _pardata[ 0].N ;
          inc(i,2) ;
          if i > 22 then break ;
        end;
        addToMPatchBufC( _pardata[ 4], _pardata[ 0], _pardata[ 2]) ;
        addToMPatchBufC( _pardata[22], _pardata[ 0], _pardata[ 4]) ;
        addToMPatchBufC( _pardata[ 8], _pardata[20], _pardata[ 6]) ;
        addToMPatchBufC( _pardata[18], _pardata[20], _pardata[ 8]) ;
        addToMPatchBufC( _pardata[12], _pardata[16], _pardata[10]) ;
        addToMPatchBufC( _pardata[14], _pardata[16], _pardata[12]) ;
      end
      else begin
        _pardata[ 1].N := triangleNormal( _pardata[ 3].P, _pardata[ 1].P,
                                          _pardata[23].P ) ;
        i := 3 ;
        while True do begin
          _pardata[i].N := _pardata[ 1].N ;
          inc(i,2) ;
          if i > 23 then break ;
        end;
        addToMPatchBufC( _pardata[ 3], _pardata[ 1], _pardata[23]) ;
        addToMPatchBufC( _pardata[ 3], _pardata[23], _pardata[21]) ;
        addToMPatchBufC( _pardata[ 5], _pardata[19], _pardata[ 7]) ;
        addToMPatchBufC( _pardata[ 7], _pardata[19], _pardata[17]) ;
        addToMPatchBufC( _pardata[11], _pardata[ 9], _pardata[15]) ;
        addToMPatchBufC( _pardata[11], _pardata[15], _pardata[13]) ;

        _pardata[ 0].N := triangleNormal( _pardata[22].P, _pardata[ 0].P,
                                          _pardata[ 2].P ) ;
        i := 2 ;
        while True do begin
          _pardata[i].N := _pardata[ 0].N ;
          inc(i,2) ;
          if i > 22 then break ;
        end;
        addToMPatchBufC( _pardata[22], _pardata[ 0], _pardata[ 2]) ;
        addToMPatchBufC( _pardata[20], _pardata[22], _pardata[ 2]) ;
        addToMPatchBufC( _pardata[ 6], _pardata[18], _pardata[ 4]) ;
        addToMPatchBufC( _pardata[16], _pardata[18], _pardata[ 6]) ;
        addToMPatchBufC( _pardata[14], _pardata[ 8], _pardata[10]) ;
        addToMPatchBufC( _pardata[12], _pardata[14], _pardata[10]) ;
      end;
      Result := z + r ;
    end;

  // main body
  begin
    if printBmpImgSize = 0 then
      r := _siz * wndXSize / windowWidth
    else
      r := _siz * wndXSize / printBmpOutSize.X ;
    x := _pardata[0].P.X ;
    y := _pardata[0].P.Y ;
    z := _pardata[0].P.Z ;
    case _sym of
      TGIS_MarkerStyle.Box  :
        begin
          p0.P := vector3Init( x - r, y + r, z + r ) ;
          p0.Color := _color ;
          p1.P := vector3Init( x + r, y + r, z + r ) ;
          p1.Color := _color ;
          p2.P := vector3Init( x + r, y - r, z + r ) ;
          p2.Color := _color ;
          p3.P := vector3Init( x - r, y - r, z + r ) ;
          p3.Color := _color ;

          p4.P := vector3Init( x - r, y + r, z - r ) ;
          p4.Color := _color ;
          p5.P := vector3Init( x + r, y + r, z - r ) ;
          p5.Color := _color ;
          p6.P := vector3Init( x + r, y - r, z - r ) ;
          p6.Color := _color ;
          p7.P := vector3Init( x - r, y - r, z - r ) ;
          p7.Color := _color ;
          if renderer3D = [TRenderer3D.DX9] then begin  // DX
            p0.N := vector3Init( -0.577,  0.577,  0.577 ) ;
            p1.N := vector3Init(  0.577,  0.577,  0.577 ) ;
            p2.N := vector3Init(  0.577, -0.577,  0.577 ) ;
            p3.N := vector3Init( -0.577, -0.577,  0.577 ) ;
            p4.N := vector3Init( -0.577,  0.577, -0.577 ) ;
            p5.N := vector3Init(  0.577,  0.577, -0.577 ) ;
            p6.N := vector3Init(  0.577, -0.577, -0.577 ) ;
            p7.N := vector3Init( -0.577, -0.577, -0.577 ) ;
          end
          else begin                // GL
            p0.N := vector3Init(  0.577,  0.577, -0.577 ) ;
            p1.N := vector3Init( -0.577,  0.577, -0.577 ) ;
            p2.N := vector3Init( -0.577, -0.577, -0.577 ) ;
            p3.N := vector3Init(  0.577, -0.577, -0.577 ) ;
            p4.N := vector3Init(  0.577,  0.577,  0.577 ) ;
            p5.N := vector3Init( -0.577,  0.577,  0.577 ) ;
            p6.N := vector3Init( -0.577, -0.577,  0.577 ) ;
            p7.N := vector3Init(  0.577, -0.577,  0.577 ) ;
          end;

          addToMPatchBufC( p0, p3, p1) ;
          addToMPatchBufC( p1, p3, p2) ;
          addToMPatchBufC( p5, p6, p4) ;
          addToMPatchBufC( p4, p6, p7) ;

          addToMPatchBufC( p4, p0, p5) ;
          addToMPatchBufC( p5, p0, p1) ;
          addToMPatchBufC( p5, p1, p6) ;
          addToMPatchBufC( p6, p1, p2) ;
          addToMPatchBufC( p6, p2, p7) ;
          addToMPatchBufC( p7, p2, p3) ;
          addToMPatchBufC( p7, p3, p4) ;
          addToMPatchBufC( p4, p3, p0) ;

          Result := p1.P.Z ;
        end;
      TGIS_MarkerStyle.Circle      :
        begin
          Result := generate_sphere ;
        end;
      TGIS_MarkerStyle.Cross       :
        begin
          Result := generate_cross( True ) ;
        end;
      TGIS_MarkerStyle.DiagCross   :
        begin
          Result := generate_cross( False ) ;
        end;
      TGIS_MarkerStyle.TriangleUp  :
        begin
          r := 2 * r ;
          p0.P := vector3Init( x            , y - 0.40825*r, z - 0.2041*r ) ;
          p0.Color := _color ;
          p1.P := vector3Init( x + 0.40825*r, y + 0.40825*r, z - 0.2041*r ) ;
          p1.Color := _color ;
          p2.P := vector3Init( x - 0.40825*r, y + 0.40825*r, z - 0.2041*r ) ;
          p2.Color := _color ;
          p3.P := vector3Init( x            , y            , z + 0.6124*r ) ;
          p3.Color := _color ;
          p0.N := triangleNormal( p1.P, p2.P, p3.P ) ;
          p1.N := triangleNormal( p2.P, p0.P, p3.P ) ;
          p2.N := triangleNormal( p2.P, p3.P, p0.P ) ;
          p3.N := triangleNormal( p0.P, p1.P, p2.P ) ;
          addToMPatchBufC( p0, p2, p1) ;
          addToMPatchBufC( p1, p3, p0) ;
          addToMPatchBufC( p2, p3, p1) ;
          addToMPatchBufC( p0, p3, p2) ;
          Result := p3.P.Z ;
        end
        else begin
          r := 2 * r ;
          p0.P := vector3Init( x            , y - 0.40825*r, z + 0.2041*r ) ;
          p0.Color := _color ;
          p1.P := vector3Init( x + 0.40825*r, y + 0.40825*r, z + 0.2041*r ) ;
          p1.Color := _color ;
          p2.P := vector3Init( x - 0.40825*r, y + 0.40825*r, z + 0.2041*r ) ;
          p2.Color := _color ;
          p3.P := vector3Init( x            , y            , z - 0.6124*r ) ;
          p3.Color := _color ;
          p0.N := triangleNormal( p1.P, p2.P, p3.P ) ;
          p1.N := triangleNormal( p2.P, p0.P, p3.P ) ;
          p2.N := triangleNormal( p2.P, p3.P, p0.P ) ;
          p3.N := triangleNormal( p0.P, p1.P, p2.P ) ;
          addToMPatchBufC( p1, p2, p0) ;
          addToMPatchBufC( p0, p3, p1) ;
          addToMPatchBufC( p1, p3, p2) ;
          addToMPatchBufC( p2, p3, p0) ;
          Result := p0.P.Z ;
        end;
    end;
  end;

  function TGIS_Renderer3DAbstract.drawPointFromCache(
    const _shp   : TObject
  ) : Boolean ;
  var
    pardata    : T_arColorData  ;
    cbuffer    : T_arColorData  ;
    k,kk,jj,ci : Integer        ;
    shp_type   : TGIS_ShapeType ;
    ptg        : TGIS_Point3D   ;
    part_no    : Integer        ;
    point_no   : Integer        ;
    valx, valy : Double         ;
    ar_color   : DWORD          ;
    ou_color   : DWORD          ;
    px, mltp   : Double         ;
    mz         : Double         ;
    cl         : TGIS_Color     ;
    cl1,cl2    : TGIS_Color     ;
    cl3,cl4    : TGIS_Color     ;
    cl9,cl11   : TGIS_Color     ;
    r,g,b      : Byte           ;
    shp        : TGIS_Renderer3DShape  ;
    a          : Cardinal       ;
    lab_z      : Double         ;
    lab_b      : Double         ;
    has_vcolor : Boolean        ;
    vc_len     : Integer        ;
  begin
    if not assigned(_shp) then begin
      Result := True ;
      exit ;
    end;
    lab_z := 0 ;
    lab_b := 0 ;
    shp := TGIS_Renderer3DShape(_shp) ;

    // shape context
    groundType := shp.Ground ;
    basementType := shp.Basement ;

    a := TruncS(currentVL.Transparency / 100 * 255) ;
    minBasement  := GIS_MAX_SINGLE ;
    minBasement1 := GIS_MAX_SINGLE ;

    shp_type := shp.ShapeType ;
    srcShpType := shp_type ;

    mltp := 1.0 ;

    cl := shp.OutlineColor ;
    r := cl.R ;
    g := cl.G ;
    b := cl.B ;
    if renderer3D = [TRenderer3D.OGL] then
      ou_color := color2ARGB(a, b, g, r)
    else
      ou_color := color2ARGB(a, r, g, b) ;

    cl := shp.Color ;
    r := cl.R ;
    g := cl.G ;
    b := cl.B ;
    if renderer3D = [TRenderer3D.OGL] then
      ar_color := color2ARGB(a, b, g, r)
    else
      ar_color := color2ARGB(a, r, g, b) ;

    vc_len     := length( shp.VertexColors ) ;
    has_vcolor := vc_len > 0 ;

    px := getPrinterPixelSize( dRadius ) ;
    if scrRatio > 1.0 then
      px := px * scrRatio;
    if not shp.ShowCeiling then
      noRoof := True
    else
      noRoof := False ;
    if not shp.ShowWalls then
      noWall := True
    else
      noWall := False ;

    if IsStringEmpty( shp.Texture ) then
      arBmp := False
    else
      arBmp := True ;
    if IsStringEmpty( shp.OutlineTexture ) then
      ouBmp := False
    else
      ouBmp := True ;

//    TGIS_Tessellation(oPolyRoof).InitVariables ;

    // Mark object selected in 2D
    if shp.IsSelected then begin
      r := oGIS.SelectionGisColor.R ;
      g := oGIS.SelectionGisColor.G ;
      b := oGIS.SelectionGisColor.B ;
      if renderer3D = [TRenderer3D.OGL] then
         ar_color := color2ARGB(Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)), b, g, r)
       else
         ar_color := color2ARGB(Byte(TruncS(oGIS.SelectionTransparency/100.0*255.0)), r, g, b) ;
      ou_color := ar_color ;
    end ;

    ci := 0 ; // color counter
    cl1 := extractColorFromShape(1);
    cl2 := extractColorFromShape(2);
    cl3 := extractColorFromShape(3);
    cl4 := extractColorFromShape(4);
    cl9 := extractColorFromShape(9);
    cl11 := extractColorFromShape(11);
    SetLength( pardata, 26 ) ;
    for part_no := 0 to shp.GetNumParts - 1 do begin
      for point_no := 0 to shp.GetPartSize(part_no) - 1 do begin

        k := 0 ; // vertex counter

        ptg := shp.GetPoint3D(part_no, point_no) ;
        if (ptg.M < ZMMIN) or (ptg.M > ZMMAX) then ptg.M := 0 ;
        if (ptg.Z < ZMMIN) or (ptg.Z > ZMMAX) then ptg.Z := 0 ;

        ptg.Z := ptg.Z * shp.ScaleZ ;
        ptg.M := ptg.M * shp.ScaleM ;

        valx := ((vectExtent.XMax - ptg.X) /
          (vectExtent.XMax - vectExtent.XMin)) ;
        valy := ((vectExtent.YMax - ptg.Y) /
          (vectExtent.YMax - vectExtent.YMin)) ;
        pardata[k].P.X := roughExtent.XMax - valx *
          (roughExtent.XMax - roughExtent.XMin) ;
        pardata[k].P.Y := roughExtent.YMin + valy *
          (roughExtent.YMax - roughExtent.YMin) ;
        if renderer3D <> [TRenderer3D.DX9] then
          pardata[k].P.Y := -pardata[k].P.Y ;
        // DEM exists among layers, get shp Z value from grid
        kk := demFrameSize - TruncS(valx * demFrameSize) ;
        jj := TruncS(valy * demFrameSize) ;
        // TGIS_3DGroundType.OnDem
        if (jj < 0) or (jj >= demFrameSize - 1) or (kk < 0) or
          (kk >= demFrameSize - 1) then
          pardata[k].P.Z := defaultZ
        else
          pardata[k].P.Z := zValue(kk, jj) ;

        if groundType = TGIS_3DGroundType.AboveDem then // above DEM
          pardata[k].P.Z := pardata[k].P.Z + projFactor * ptg.Z * zFactor / zUnit
        else // above 0
          if groundType = TGIS_3DGroundType.AboveZero then // above 0
            pardata[k].P.Z := projFactor * (ptg.Z - zLevel) * zFactor / zUnit ;

        if pardata[k].P.Z < minBasement then
          minBasement := pardata[k].P.Z ;

        pardata[k].Color := ou_color ;
        inc(k) ;
        inc(ci) ;
        // next point
        pardata[k].P := pardata[k-1].P ;
        pardata[k].P.Z := pardata[k - 1].P.Z + projFactor * (ptg.M * mFactor) / zUnit ;
        if pardata[k].P.Z < minBasement1 then
          minBasement1 := pardata[k].P.Z ;
        pardata[k].Color := ou_color ;
        lab_z := pardata[k].P.Z ;
        lab_b := pardata[k-1].P.Z ;
        if pardata[k].P.Z = pardata[k-1].P.Z then
          mz := 0
        else
          mz := pardata[k].P.Z - pardata[k-1].P.Z ;

        // for points with marker size < 1.2pt
        if shp.Size < 2 then begin
          if has_vcolor and ( ci < vc_len ) then begin
            cl := TGIS_Color.FromRGB( Cardinal(shp.VertexColors[ci-1]) ) ;
            if renderer3D = [TRenderer3D.OGL] then
               pardata[k-1].Color := color2ARGB(cl.A, cl.B, cl.G, cl.R)
             else
               pardata[k-1].Color := cl.ARGB ;

            cl := TGIS_Color.FromRGB( Cardinal(shp.VertexColors[ci]) ) ;
            if renderer3D = [TRenderer3D.OGL] then
               pardata[k].Color := color2ARGB(cl.A, cl.B, cl.G, cl.R)
             else
               pardata[k].Color := cl.ARGB ;
          end
          else begin
            pardata[k-1].Color := ar_color ;
            pardata[k  ].Color := ar_color ;
          end ;

          if ptg.M = 0 then begin
//?         do not load point into Info buffer for mass points
//?            fillBufferInfoEntry(12, shp.Uid, part_no) ;
            addToWPBuf( pardata[k-1] ) ;
          end
          else begin
            fillBufferInfoEntry(11, shp.Uid, part_no, cl11) ;
            addToWPLBuf( pardata[k-1], pardata[k] );
          end;
          shp.LabelZ := lab_z ;
          shp.LabelB := lab_b ;
          continue ;
        end;

        TGIS_Tessellation(oPolyRoof).Reset;

        if (shp.Size >= 2) and (ptg.M = 0) and noWall then begin

          if (shp.MarkerStyle = TGIS_MarkerStyle.Cross) or
             (shp.MarkerStyle = TGIS_MarkerStyle.DiagCross) then begin
            pardata[0].P.Z := pardata[0].P.Z - px*shp.Size/2 ;
            pardata[1].P.Z := pardata[1].P.Z + px*shp.Size/2 ;
            k := make3dMarker(shp.Size, shp.MarkerStyle, pardata, px, ar_color);
            setShpN(k, pardata) ;
          end;

          fillBufferInfoEntry(9, shp.Uid, part_no, cl9) ;
          lab_z := drawVolumetricPoint(shp.Size,shp.MarkerStyle,
                     pardata, px, ar_color) ;
          continue;
        end;

        k := make3dMarker(shp.Size, shp.MarkerStyle, pardata, px, ou_color) ;

        // Point as Polygon
        shp_type := TGIS_ShapeType.Polygon ;
        jj := 1 ;
        while True do begin
          try
            if jj >= k - 3 then
              break;
          finally
            TGIS_Tessellation(oPolyRoof).AddVertex( 0,
                                               pardata[jj].P.X,
                                               pardata[jj].P.Y,
                                               pardata[jj].P.Z,
                                               mz
                                              ) ;
            jj := jj + 2 ;
          end;
        end;

        // draw walls
        setShpN(k, pardata) ;
        if basementType = TGIS_3DBasementType.Lowest then
          if (groundType = TGIS_3DGroundType.OnDem) or
             (groundType = TGIS_3DGroundType.AboveDem) then
            adjustBasement(0, pardata, cbuffer, 0, k,
              minBasement, minBasement1) ;

        if k > 1 then
          if (not noWall) and (ptg.M <> 0) then
            if not ouBmp then begin
              fillBufferInfoEntry(1, shp.Uid, part_no, cl1) ;
              triStripToListC(False, pardata, k - 2)
            end
            else begin
              fillBufferInfoEntry(3, shp.Uid, part_no, cl3) ;
              drawVectorTexture(shp, False, pardata, 1, k, 0) ;
            end;

        // draw VectorEdges
        if vectorEdges then
          drawVectorEdges(k, pardata) ;

        // Draw polygon roof
        if shp_type = TGIS_ShapeType.Polygon then
        if not noRoof then begin
          if not arBmp then
            fillBufferInfoEntry(2, shp.Uid, part_no, cl2)
          else
            fillBufferInfoEntry(4, shp.Uid, part_no, cl4) ;
          if not drawPolygonRoof(ar_color, mltp, shp, True) then begin
            Result := False ;
            exit ;
          end;
        end;
      end; // end for point
    end; // end for part
    shp.LabelZ := lab_z ;
    shp.LabelB := lab_b ;

    Result := True ;
  end;

  procedure TGIS_Renderer3DAbstract.loadMesh(
    const _part    : Integer       ;
    const _pardata : T_arColorData ;
    const _k       : Integer       ;
    const _dir     : Boolean
  ) ;
  var
    i, k : Integer ;
    ref  : Single  ;
    mz   : Double  ;
  begin
    k := _k - 2 ;
    if _dir then begin
      for i := 1 to k do begin
        if i mod 2 <> 0 then begin
          ref := _pardata[i-1].P.Z ;
          if ref = _pardata[i].P.Z then
            mz := 0
          else
            mz := _pardata[i].P.Z - ref ;

          TGIS_Tessellation(oPolyRoof).AddVertex( _part,
                                             -_pardata[i].P.X,
                                             _pardata[i].P.Y,
                                             _pardata[i].P.Z,
                                              mz
                                            )
        end;
      end;
    end
    else begin
      for i := k downto 1 do begin
        if i mod 2 <> 0 then begin
          ref := _pardata[i-1].P.Z ;
          if ref = _pardata[i].P.Z then
            mz := 0
          else
            mz := _pardata[i].P.Z - ref ;

          TGIS_Tessellation(oPolyRoof).AddVertex( _part,
                                             -_pardata[i].P.X,
                                             _pardata[i].P.Y,
                                             _pardata[i].P.Z,
                                             mz
                                           ) ;
        end;
      end;
    end;
  end;

  function TGIS_Renderer3DAbstract.shpExt3D (
    const _data : T_arColorData ;
    const _num  : Integer
  ) : TGIS_Extent ;
  var
    xmi,xma,
    ymi,yma : Double      ;
    i       : Integer     ;
  begin
    xmi :=  GIS_MAX_SINGLE ;
    ymi := xmi ;
    xma := -GIS_MAX_SINGLE ;
    yma := xma ;

    for i := 0 to _num -1 do begin
      if xmi > _data[i].P.X then
        xmi := _data[i].P.X ;
      if ymi > _data[i].P.Y then
        ymi := _data[i].P.Y ;
      if xma < _data[i].P.X then
        xma := _data[i].P.X ;
      if yma < _data[i].P.Y then
        yma := _data[i].P.Y ;
    end;

    Result := GisExtent( xmi, ymi, xma, yma ) ;
  end;

  function TGIS_Renderer3DAbstract.vectTexVal(
    const _ext : TGIS_Extent;
    const _pnt : TGIS_Point
  ) : TGIS_Point;
  var
    pt : TGIS_Point;
    dx : Double ;
    dy : Double ;
  begin
    dx := (_ext.XMax - _ext.XMin) ;
    if dx = 0 then
      dx := 1 ;
    dy := (_ext.YMax - _ext.YMin) ;
    if dy = 0 then
      dy := 1 ;

    pt.X := (_pnt.X - _ext.XMin) / dx ;
    pt.Y := (_pnt.Y - _ext.YMin) / dy ;
    Result := pt ;
  end;

  function TGIS_Renderer3DAbstract.zRange(
    const _data : T_arColorData ;
    const _num  : Integer
  ) : TGIS_Point ;
  var
    xmi,
    xma : Double     ;
    pnt : TGIS_Point ;
    i   : Integer    ;
  begin
     xmi :=  GIS_MAX_SINGLE ;
     xma := -GIS_MAX_SINGLE ;

     for i := 0 to _num -1 do begin
       if xmi > _data[i].P.Z then
         xmi := _data[i].P.Z ;
       if xma < _data[i].P.Z then
         xma := _data[i].P.Z ;
     end ;
     pnt.X := xmi ;
     pnt.Y := xma ;
    Result := pnt ;
  end ;

  procedure TGIS_Renderer3DAbstract.triStripToListC(
    const _part    : Boolean           ;
    const _pardata : T_arColorData     ;
    const _tri     : Integer
  ) ;
  var
    i    : Integer       ;
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    if _part then begin
      if length( vTLBufCR.TriLst ) = 0 then
        SetLength( vTLBufCR.TriLst   ,  videoVertexBufferSize )
    end
    else begin
      if length( vTLBufCW.TriLst ) = 0 then
        SetLength( vTLBufCW.TriLst   ,  videoVertexBufferSize )
    end;

    for i := 0 to _tri - 1 do begin
      ptg1 := _pardata[i  ] ;
      ptg2 := _pardata[i+1] ;
      ptg3 := _pardata[i+2] ;
      if _part then
        addToVTLBufC(_part, vTLBufCR, ptg1, ptg2, ptg3)
      else
        addToVTLBufC(_part, vTLBufCW, ptg1, ptg2, ptg3) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.triListToListC(
    const _part    : Boolean           ;
    const _pardata : T_arColorData     ;
    const _tri     : Integer
  ) ;
  var
    i, j : Integer       ;
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    if _part then begin
      if length( vTLBufCR.TriLst ) = 0 then
        SetLength( vTLBufCR.TriLst   ,  videoVertexBufferSize )
    end
    else begin
      if length( vTLBufCW.TriLst ) = 0 then
        SetLength( vTLBufCW.TriLst   ,  videoVertexBufferSize )
    end;

    j := 0 ;
    for i := 0 to _tri - 1 do begin
      ptg1 := _pardata[j  ] ;
      ptg2 := _pardata[j+1] ;
      ptg3 := _pardata[j+2] ;
      if _part then
        addToVTLBufC(_part, vTLBufCR, ptg1, ptg2, ptg3)
      else
        addToVTLBufC(_part, vTLBufCW, ptg1, ptg2, ptg3) ;
      j := j + 3 ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.triStripToListT(
    const _part    : Boolean         ;
    const _pardata : T_arTextureData ;
    const _tri     : Integer
  ) ;
  var
    i    : Integer       ;
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    if _part then begin
      if length( vTLBufTR.TriLst ) = 0 then begin
        SetLength( vTLBufTR.TriLst   ,  videoVertexBufferSize ) ;
        SetLength( vTLBufTR.Subset   ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
      end;
    end
    else begin
      if length( vTLBufTW.TriLst ) = 0 then begin
        SetLength( vTLBufTW.TriLst   ,  videoVertexBufferSize ) ;
        SetLength( vTLBufTW.Subset   ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
      end;
    end;

    for i := 0 to _tri - 1 do begin
      ptg1 := _pardata[i  ] ;
      ptg2 := _pardata[i+1] ;
      ptg3 := _pardata[i+2] ;
      if _part then
        addToVTLBufT(_part, vTLBufTR, ptg1, ptg2, ptg3)
      else
        addToVTLBufT(_part, vTLBufTW, ptg1, ptg2, ptg3) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.triListToListT(
    const _part    : Boolean         ;
    const _pardata : T_arTextureData ;
    const _tri     : Integer
  ) ;
  var
    i, j : Integer       ;
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    if _part then begin
      if length( vTLBufTR.TriLst ) = 0 then begin
        SetLength( vTLBufTR.TriLst   ,  videoVertexBufferSize ) ;
        SetLength( vTLBufTR.Subset   ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
      end;
    end
    else begin
      if length( vTLBufTW.TriLst ) = 0 then begin
        SetLength( vTLBufTW.TriLst   ,  videoVertexBufferSize ) ;
        SetLength( vTLBufTW.Subset   ,  TruncS(videoVertexBufferSize / 3) +1 ) ;
      end;
    end;

    j := 0 ;
    for i := 0 to _tri - 1 do begin
      ptg1 := _pardata[j  ] ;
      ptg2 := _pardata[j+1] ;
      ptg3 := _pardata[j+2] ;
      if _part then
        addToVTLBufT(_part, vTLBufTR, ptg1, ptg2, ptg3)
      else
        addToVTLBufT(_part, vTLBufTW, ptg1, ptg2, ptg3) ;
      j := j + 3 ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.linStripToList(
    const _pardata : T_arColorData     ;
    const _lin     : Integer
  ) ;
  var
    i    : Integer       ;
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
  begin
    for i := 0 to _lin - 1 do begin
      ptg1 := _pardata[i  ] ;
      ptg2 := _pardata[i+1] ;
      addToWPLBuf(ptg1, ptg2) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.linStripToList1(
    const _pardata : T_arColorData     ;
    const _lin     : Integer
  ) ;
  var
    i    : Integer       ;
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
  begin
    i := 0 ;
    while True do begin
      ptg1 := _pardata[i  ] ;
      ptg2 := _pardata[i+2] ;
      if renderer3D = [TRenderer3D.FMX] then begin
        colorToTexture(ptg1.Color, ptg1.Tu, ptg1.Tv ) ;
        colorToTexture(ptg2.Color, ptg2.Tu, ptg2.Tv ) ;
      end ;
      addToWPLBuf(ptg1, ptg2) ;
      inc( i, 2 ) ;
      if (i +2 > _lin -1) then break ;
    end ;
  end ;

  procedure TGIS_Renderer3DAbstract.linListToList(
    const _pardata : T_arColorData     ;
    const _lin     : Integer
  ) ;
  var
    i, j : Integer       ;
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
  begin
    for i := 0 to _lin - 1 do begin
      j := i * 2 ;
      ptg1 := _pardata[j  ] ;
      ptg2 := _pardata[j+1] ;
      addToWPLBuf(ptg1, ptg2) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.addToVTLBufC(
    const _part : Boolean              ;
    var   _buf  : TGIS_Renderer3DColorTriangleListBuffer ;
    const _ptg1 : TGIS_Renderer3DVertex        ;
    const _ptg2 : TGIS_Renderer3DVertex        ;
    const _ptg3 : TGIS_Renderer3DVertex
  ) ;
  var
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
    ptg3 : TGIS_Renderer3DVertex ;
  begin
    // videoVertexBufferSize  must be multiplication of number 6 !!!!
    if length(_buf.TriLst) = 0 then begin
      SetLength( _buf.TriLst ,  initVideoVBSize ) ;
    end;
    if _buf.NumPoints + 6 < length(_buf.TriLst ) then begin
      _buf.TriLst[_buf.NumPoints] := _ptg1 ;
      inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := _ptg2 ;
      inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := _ptg3 ;
      inc(_buf.NumPoints) ;
      inc(_buf.NumObjects) ;
    end
    else begin
      SetLength( _buf.TriLst ,  length(_buf.TriLst ) + videoVBIncrement ) ;
      ptg1 := _ptg1 ;
      ptg2 := _ptg2 ;
      ptg3 := _ptg3 ;
//      saveVTLBufC( _part, _buf ) ;
      _buf.TriLst[_buf.NumPoints] := ptg1 ;
      inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := ptg2 ;
      inc(_buf.NumPoints) ;
      _buf.TriLst[_buf.NumPoints] := ptg3 ;
      inc(_buf.NumPoints) ;
      inc(_buf.NumObjects) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.addToWPLBuf(
    const _ptg1 : TGIS_Renderer3DVertex ;
    const _ptg2 : TGIS_Renderer3DVertex
  ) ;
  var
    ptg1 : TGIS_Renderer3DVertex ;
    ptg2 : TGIS_Renderer3DVertex ;
  begin
    if length( wPLBuf.PntLst ) = 0 then
      SetLength( wPLBuf.PntLst ,  initVideoVBSize ) ;

    ptg1 := _ptg1 ;
    ptg2 := _ptg2 ;
    if renderer3D = [TRenderer3D.FMX] then begin
      colorToTexture(_ptg1.Color, ptg1.Tu, ptg1.Tv) ;
      colorToTexture(_ptg2.Color, ptg2.Tu, ptg2.Tv) ;
    end;
    if wPLBuf.NumPoints+2 < length( wPLBuf.PntLst ) then begin
      wPLBuf.PntLst[wPLBuf.NumPoints] := ptg1 ;
      inc(wPLBuf.NumPoints) ;
      wPLBuf.PntLst[wPLBuf.NumPoints] := ptg2 ;
      inc(wPLBuf.NumPoints) ;
      inc(wPLBuf.NumObjects) ;
    end
    else begin
//      SetLength( wPLBuf.PntLst ,  length( wPLBuf.PntLst ) + videoVBIncrement ) ;
      saveWPLBuf ;
      wPLBuf.PntLst[wPLBuf.NumPoints] := ptg1 ;
      inc(wPLBuf.NumPoints) ;
      wPLBuf.PntLst[wPLBuf.NumPoints] := ptg2 ;
      inc(wPLBuf.NumPoints) ;
      inc(wPLBuf.NumObjects) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.addToWPBuf(
    const _ptg1 : TGIS_Renderer3DVertex
  ) ;
  var
    ptg1 : TGIS_Renderer3DVertex ;
  begin
    if length( wPBuf.PntLst ) = 0 then
      SetLength( wPBuf.PntLst      ,  videoVertexBufferSize ) ;

    ptg1 := _ptg1 ;
    if renderer3D = [TRenderer3D.FMX] then
      colorToTexture(_ptg1.Color, ptg1.Tu, ptg1.Tv) ;

    if wPBuf.NumObjects < videoVertexBufferSize then begin
      wPBuf.PntLst[wPBuf.NumObjects] := ptg1 ;
      inc(wPBuf.NumObjects) ;
    end
    else begin
      saveWPBuf ;
      wPBuf.PntLst[wPBuf.NumObjects] := ptg1 ;
      inc(wPBuf.NumObjects) ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.drawVectorEdges(
    const _num_pnts : Integer   ;
    var   _data     : T_arColorData
  ) ;
  var
    k, kk, jj  : Integer       ;
    pardata    : T_arColorData ;
  begin
    if _num_pnts < 3 then  exit ;
    if lightSwitch then
      lightOff ;

    pardata := _data ;
    k := _num_pnts ;
    k := k -1 ;
    for kk:=0 to k do begin
      pardata[kk].Color := edColor ;
    end ;
    k  := k-1 ;
    kk := TruncS(k/2.0) ;
    // vertical edges

    if wPLBuf.NumPoints + 4 * kk + 1> videoVertexBufferSize then
      saveWPLBuf ;

    linListToList(pardata, kk) ;

    kk := 0 ;
    jj := 1 ;
    while True do begin
        try
          if jj>=k then break;
        finally
          pardata[kk].P.X := pardata[jj].P.X ;
          pardata[kk].P.Y := pardata[jj].P.Y ;
          pardata[kk].P.Z := pardata[jj].P.Z ;
          pardata[kk].Color := edColor ;
          inc(kk) ;
          jj := jj +2 ;
        end ;
    end ;

    // horizontal edges
    linStripToList(pardata, kk -1) ;

    if lightSwitch then
      lightOn ;
  end ;

  function TGIS_Renderer3DAbstract.saveWPLBuf
    : Integer ;
  begin
    if wPLBuf.NumObjects = 0 then
      begin
        Result := S_OK ;
        exit ;
      end;

      Result := addLineMeshTile(wPLBuf.NumObjects);
      if Result <> S_OK then begin
        if Result = -2147024882 then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                              'Out of memory', 8 )
        else
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                'saveWPLBuf', 8 )
      end;
      wPLBuf.NumPoints  := 0 ;
      wPLBuf.NumObjects := 0 ;

      Result := S_OK ;
  end;

  function TGIS_Renderer3DAbstract.saveWPBuf
    : Integer ;
  begin
    if wPBuf.NumObjects = 0 then
      begin
        Result := S_OK ;
        exit ;
      end;

      Result := addPointMeshTile(wPBuf.NumObjects);
      if Result <> S_OK then begin
        if Result = -2147024882 then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                              'Out of memory', 9 )
        else
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_3D_FAIL ),
                                                'saveWPBuf', 9 )
      end;
      wPBuf.NumObjects := 0 ;
      Result := S_OK ;

  end;

  function  TGIS_Renderer3DAbstract.addLineMeshTile(
    const _lin     : Integer
  ) : Integer ;
  var
    mtc     : Integer ;
    numvert : Integer ;
  begin
    inc(meshLineTilesCounter) ;
    SetLength(meshLineTiles, meshLineTilesCounter) ;
    mtc := meshLineTilesCounter -1 ;

    SetLength(transpInfo.LT, meshLineTilesCounter ) ;
    transpInfo.LT[mtc] := TruncS( currentVL.Transparency / 100 * 255 ) ;

    numvert := _lin * 2 ;
    meshLineTiles[mtc].Count := numvert ;
    SetLength(meshLineTiles[mtc].Buffer, numvert ) ;

    {$IFDEF CLR}
      Array.Copy( wPLBuf.PntLst,
                  meshLineTiles[mtc].Buffer,
                  numvert
                ) ;
    {$ELSE}
      System.Move( wPLBuf.PntLst[0],
                   meshLineTiles[mtc].Buffer[0],
                   numvert * SizeOf(TGIS_Renderer3DVertex)
                 ) ;
    {$ENDIF}
    addVectorTilesInfo( 11, mtc ) ;
    Result := 0 ;

  end;

  function  TGIS_Renderer3DAbstract.addPointMeshTile(
    const _num    : Integer
  ) : Integer ;
  var
    mtc     : Integer ;
    numvert : Integer ;
  begin
    inc(meshPointTilesCounter) ;
    SetLength(meshPointTiles, meshPointTilesCounter) ;
    mtc := meshPointTilesCounter -1 ;

    SetLength(transpInfo.PT, meshPointTilesCounter ) ;
    transpInfo.PT[mtc] := TruncS( currentVL.Transparency / 100 * 255 ) ;

    numvert := _num ;
    meshPointTiles[mtc].Count := numvert ;
    SetLength(meshPointTiles[mtc].Buffer, numvert ) ;

    {$IFDEF CLR}
      Array.Copy( wPBuf.PntLst,
                  meshPointTiles[mtc].Buffer,
                  numvert
                ) ;
    {$ELSE}
      System.Move(
                   wPBuf.PntLst[0],
                   meshPointTiles[mtc].Buffer[0],
                   numvert * SizeOf(TGIS_Renderer3DVertex)
                 ) ;
    {$ENDIF}
    addVectorTilesInfo( 12, mtc ) ;
    Result := 0 ;

  end;

  procedure TGIS_Renderer3DAbstract.addToVectPolyRoof(
    var   _count   : Integer              ;
    var   _buf     : TGIS_Renderer3DPolyRoofLevel     ;
    const _m1      : Single               ;
    const _m2      : Single               ;
    const _m3      : Single
  ) ;
  var
    i : Integer ;
  begin
    i := high( _buf ) ;
    if _count + 6 < videoVertexBufferSize then begin
      _buf[i][_count] := _m1 ;
      inc( _count ) ;
      _buf[i][_count] := _m2 ;
      inc( _count ) ;
      _buf[i][_count] := _m3 ;
      inc( _count ) ;
    end
    else begin
      inc( i ) ;
      SetLength(_buf, i + 1) ;
      SetLength(_buf[i], videoVertexBufferSize );
      _count := 0;
      _buf[i][_count] := _m1 ;
      inc( _count ) ;
      _buf[i][_count] := _m2 ;
      inc( _count ) ;
      _buf[i][_count] := _m3 ;
      inc( _count ) ;
    end;
  end;

  function TGIS_Renderer3DAbstract.drawPolygonRoof(
    const _ar_color : DWORD         ;
    const _mltp     : Double        ;
    const _shp      : TObject       ;
    const _winding  : Boolean
  ) : Boolean ;
  var
    num_tri       : Integer       ;
    j, jj, kk     : Integer       ;
    shp           : TGIS_Renderer3DShape ;
    pardata       : T_arColorData ;
    m0, m1, m2    : Single        ;
    prismVBSize   : Integer       ;
  begin
    shp := TGIS_Renderer3DShape(_shp) ;
    Result := True ;
    TGIS_Tessellation(oPolyRoof).Execute ;
    num_tri := TGIS_Tessellation(oPolyRoof).TrianglesCount ;
    if num_tri = 0 then  exit ;
    if TGIS_Tessellation(oPolyRoof).ExecuteCode > 0 then begin
      errorMsg := Format( _rsrc( GIS_RS_ERR_3D_TOPOLOGY ),
                          [ currentVL.Name, shp.Uid ]
                        ) ;
      errorTreatment := True ;
    end;

    prismVBSize := 3 * num_tri ;
    if prismVBSize > videoVertexBufferSize then
      prismVBSize := videoVertexBufferSize ;

    SetLength( pardata, prismVBSize ) ;

    j  := 0 ;
    kk := 0 ;
    for jj := 0 to num_tri-1 do begin
      TGIS_Tessellation(oPolyRoof).GetTriangle(jj,
               pardata[kk].P.X  , pardata[kk].P.Y  , pardata[kk].P.Z,
               pardata[kk+1].P.X, pardata[kk+1].P.Y, pardata[kk+1].P.Z,
               pardata[kk+2].P.X, pardata[kk+2].P.Y, pardata[kk+2].P.Z) ;
      pardata[kk].P.X   := _mltp * pardata[kk].P.X ;
      pardata[kk+1].P.X := _mltp * pardata[kk+1].P.X ;
      pardata[kk+2].P.X := _mltp * pardata[kk+2].P.X ;
      pardata[kk].Color   := _ar_color ;
      pardata[kk+1].Color := _ar_color ;
      pardata[kk+2].Color := _ar_color ;

      setPolyRoofN( pardata[kk], pardata[kk+1], pardata[kk+2], _winding ) ;

      if srcShpType <> TGIS_ShapeType.Polygon then begin
        pardata[kk].N.Z     :=  -pardata[kk].N.Z ;
        pardata[kk+1].N.Z   :=  -pardata[kk+1].N.Z ;
        pardata[kk+2].N.Z   :=  -pardata[kk+2].N.Z ;
      end;

      TGIS_Tessellation(oPolyRoof).GetTriangleM(jj, m0, m1, m2);

      if arBmp then
        addToVectPolyRoof( arVectPolyRoofCounterT, arVectPolyRoofT,
                           m0, m1, m2 )
      else
        addToVectPolyRoof( arVectPolyRoofCounterC, arVectPolyRoofC,
                           m0, m1, m2 ) ;

      kk := kk + 3 ;
      inc(j) ;
      if kk >= videoVertexBufferSize then begin
        if arBmp then
          drawVectorTexture(_shp, True, pardata, 0, j, 0)
        else
          triListToListC(True, pardata, j) ;
        j  := 0 ;
        kk := 0 ;

        SetLength( pardata, prismVBSize ) ;

      end;
    end;

    if arBmp then
      drawVectorTexture(_shp, True, pardata, 0, j, 0)
    else
      triListToListC(True, pardata, j) ;
  end;

  procedure TGIS_Renderer3DAbstract.drawPolyLineRoof(
    const _cbuf_siz : Integer        ;
    var   _pardata  : T_arColorData  ;
    var   _cbuffer  : T_arColorData  ;
    const _ar_color : DWORD          ;
    var   _isarc    : Boolean        ;
    var   _shp_type : TGIS_ShapeType ;
    const _shp      : TObject
  ) ;
  var
    j, jj, k, kk   : Integer       ;
    m0, m1, m2, m3 : Single        ;
    first          : Boolean       ;
    pardata        : T_arColorData ;
  begin
    SetLength( pardata, TruncS( _cbuf_siz / 2.0 ) ) ;

    if basementType = TGIS_3DBasementType.Lowest then
    if (groundType = TGIS_3DGroundType.OnDem) or
       (groundType = TGIS_3DGroundType.AboveDem) then
      adjustBasement( 1,
                      pardata, _cbuffer,
                      0,
                      _cbuf_siz,
                      minBasement,
                      minBasement1 ) ;

    kk := 3;
    jj := _cbuf_siz-1 ;
    k  := TruncS(_cbuf_siz/2.0) ;
    j  := 0 ;
    first := True ;
    m0 := _cbuffer[kk].P.Z - _cbuffer[kk-1].P.Z ;
    m1 := _cbuffer[jj].P.Z - _cbuffer[jj-1].P.Z ;
    while True do begin
      if kk > k then
        break
      else begin
        pardata[j].P.X := _cbuffer[kk].P.X ;
        pardata[j].P.Y := _cbuffer[kk].P.Y ;
        pardata[j].P.Z := _cbuffer[kk].P.Z ;
        pardata[j].Color := _ar_color ;
        inc(j) ;
        pardata[j].P.X := _cbuffer[jj].P.X ;
        pardata[j].P.Y := _cbuffer[jj].P.Y ;
        pardata[j].P.Z := _cbuffer[jj].P.Z ;
        pardata[j].Color := _ar_color ;
        inc(j) ;

        m2 := _cbuffer[kk].P.Z - _cbuffer[kk-1].P.Z ;
        m3 := _cbuffer[jj].P.Z - _cbuffer[jj-1].P.Z ;
        if not first then begin
          if arBmp then begin
            addToVectPolyRoof( arVectPolyRoofCounterT, arVectPolyRoofT,
                               m0, m1, m2 ) ;
            addToVectPolyRoof( arVectPolyRoofCounterT, arVectPolyRoofT,
                               m1, m2, m3 )
          end
          else begin
            addToVectPolyRoof( arVectPolyRoofCounterC, arVectPolyRoofC,
                               m0, m1, m2 ) ;
            addToVectPolyRoof( arVectPolyRoofCounterC, arVectPolyRoofC,
                               m1, m2, m3 )
          end;
          m0 := m2 ;
          m1 := m3 ;
        end;
        first := False ;

        jj := jj -2 ;
        kk := kk +2 ;
      end;
    end;

    setShpN(j, pardata) ;

    if arBmp then
        drawVectorTexture(_shp, True, pardata, 1, j, 0)
    else
      triStripToListC(True, pardata, j - 2) ;

     _isarc := False ;
     _shp_type := TGIS_ShapeType.Arc ;
  end;

  function TGIS_Renderer3DAbstract.make3dMarker(
    const _siz      : Integer          ;
    const _sym      : TGIS_MarkerStyle ;
    var   _pardata  : T_arColorData    ;
    const _px       : Double           ;
    const _po_color : DWORD
  ) : Integer ;
  var
    k : Integer;
    X,Y,z1,z2 : Double;
  begin
    k := 0 ;
    X  := _pardata[0].P.X ;
    Y  := _pardata[0].P.Y ;
    z1 := _pardata[0].P.Z ;
    z2 := _pardata[1].P.Z ;
    case _sym of
      TGIS_MarkerStyle.Box           :
        k := makeBox(X,Y,z1,z2,_px,_siz,_po_color,_pardata) ;
      TGIS_MarkerStyle.Circle        :
        k := makeCir(X,Y,z1,z2,_px,_siz,_po_color,_pardata) ;
      TGIS_MarkerStyle.Cross         :
        k := makeCrs(X,Y,z1,z2,_px,_siz,_po_color,_pardata) ;
      TGIS_MarkerStyle.DiagCross     :
        k := makeDcr(X,Y,z1,z2,_px,_siz,_po_color,_pardata) ;
      TGIS_MarkerStyle.TriangleUp    :
        k := makeTUp(X,Y,z1,z2,_px,_siz,_po_color,_pardata) ;
      TGIS_MarkerStyle.TriangleDown  :
        k := makeTDw(X,Y,z1,z2,_px,_siz,_po_color,_pardata) ;
      TGIS_MarkerStyle.TriangleLeft  :
        k := makeTLf(X,Y,z1,z2,_px,_siz,_po_color,_pardata) ;
      TGIS_MarkerStyle.TriangleRight :
        k := makeTRg(X,Y,z1,z2,_px,_siz,_po_color,_pardata) ;
    end;
    Result := k ;
  end;

  function TGIS_Renderer3DAbstract.makeBox(
    const _x        : Double    ;
    const _y        : Double    ;
    const _z1       : Double    ;
    const _z2       : Double    ;
    const _px       : Double    ;
    const _siz      : Double    ;
    const _po_color : DWORD     ;
    var   _pardata  : T_arColorData
  ) : Integer ;
  var
    k : Integer;
    r : Double;
  begin
    r := _px * _siz ;
    k := 0 ;
    markerLgth := 0 ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    Result := k ;
  end;

  function TGIS_Renderer3DAbstract.makeTDw(
    const _x        : Double    ;
    const _y        : Double    ;
    const _z1       : Double    ;
    const _z2       : Double    ;
    const _px       : Double    ;
    const _siz      : Double    ;
    const _po_color : DWORD     ;
    var   _pardata  : T_arColorData
  ) : Integer ;
  var
    k : Integer;
    r : Double;
  begin
    r := _px * _siz ;
    k := 0 ;
    markerLgth := 0 ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    Result := k ;
  end;

  function TGIS_Renderer3DAbstract.makeTUp(
    const _x        : Double    ;
    const _y        : Double    ;
    const _z1       : Double    ;
    const _z2       : Double    ;
    const _px       : Double    ;
    const _siz      : Double    ;
    const _po_color : DWORD     ;
    var   _pardata  : T_arColorData
  ) : Integer ;
  var
    k : Integer;
    r : Double;
  begin
    r := _px * _siz ;
    k := 0 ;
    markerLgth := 0 ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    Result := k ;
  end;

  function TGIS_Renderer3DAbstract.makeTLf(
    const _x        : Double    ;
    const _y        : Double    ;
    const _z1       : Double    ;
    const _z2       : Double    ;
    const _px       : Double    ;
    const _siz      : Double    ;
    const _po_color : DWORD     ;
    var   _pardata  : T_arColorData
  ) : Integer ;
  var
    k : Integer;
    r : Double;
  begin
    r := _px * _siz ;
    k := 0 ;
    markerLgth := 0 ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    Result := k ;
  end;

  function TGIS_Renderer3DAbstract.makeTRg(
    const _x        : Double    ;
    const _y        : Double    ;
    const _z1       : Double    ;
    const _z2       : Double    ;
    const _px       : Double    ;
    const _siz      : Double    ;
    const _po_color : DWORD     ;
    var   _pardata  : T_arColorData
  ) : Integer ;
  var
    k : Integer;
    r : Double;
  begin
    r := _px * _siz ;
    k := 0 ;
    markerLgth := 0 ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    Result := k ;
  end;

  function TGIS_Renderer3DAbstract.makeCrs(
    const _x        : Double    ;
    const _y        : Double    ;
    const _z1       : Double    ;
    const _z2       : Double    ;
    const _px       : Double    ;
    const _siz      : Double    ;
    const _po_color : DWORD     ;
    var   _pardata  : T_arColorData
  ) : Integer ;
  var
    k : Integer;
    r : Double;
  begin
    r := _px * _siz ;
    k := 0 ;
    markerLgth := 0 ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + 0.25*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + 0.25*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - 0.25*r ;
    _pardata[k].P.Y := _y + 0.25*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - 0.25*r ;
    _pardata[k].P.Y := _y + 0.25*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - 0.25*r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - 0.25*r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + 0.25*r ;
    _pardata[k].P.Y := _y + r;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + 0.25*r;
    _pardata[k].P.Y := _y + r;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + 0.25*r ;
    _pardata[k].P.Y := _y + 0.25*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + 0.25*r ;
    _pardata[k].P.Y := _y + 0.25*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + 0.25*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + 0.25*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + 0.25*r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + 0.25*r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + 0.25*r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + 0.25*r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - 0.25*r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - 0.25*r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - 0.25*r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - 0.25*r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - 0.25*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    Result := k ;
  end;

  function TGIS_Renderer3DAbstract.makeDcr(
    const _x        : Double    ;
    const _y        : Double    ;
    const _z1       : Double    ;
    const _z2       : Double    ;
    const _px       : Double    ;
    const _siz      : Double    ;
    const _po_color : DWORD     ;
    var   _pardata  : T_arColorData
  ) : Integer ;
  var
    k : Integer;
    r : Double;
  begin
    r := _px * _siz ;
    k := 0 ;
    markerLgth := 0 ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - 0.5*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - 0.5*r ;
    _pardata[k].P.Z := _z2;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - 0.5*r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - 0.5*r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + 0.5*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y + 0.5*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - 0.5*r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - 0.5*r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y + 0.5*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y + 0.5*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + 0.5*r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + 0.5*r ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + 0.5*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y + 0.5*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + 0.5*r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + 0.5*r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - 0.5*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y - 0.5*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + 0.5*r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + 0.5*r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y - 0.5*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y - 0.5*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - 0.5*r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - 0.5*r ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - 0.5*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y - 0.5*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    Result := k ;
  end;

  function TGIS_Renderer3DAbstract.makeCir(
    const _x        : Double    ;
    const _y        : Double    ;
    const _z1       : Double    ;
    const _z2       : Double    ;
    const _px       : Double    ;
    const _siz      : Double    ;
    const _po_color : DWORD     ;
    var   _pardata  : T_arColorData
  ) : Integer ;
  var
    k : Integer;
    r : Double;
  begin
    r := _px * _siz ;
    k := 0 ;
    markerLgth := 0 ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - 0.707*r ;
    _pardata[k].P.Y := _y + 0.707*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - 0.707*r ;
    _pardata[k].P.Y := _y + 0.707*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y + r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + 0.707*r ;
    _pardata[k].P.Y := _y + 0.707*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + 0.707*r ;
    _pardata[k].P.Y := _y + 0.707*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x + 0.707*r ;
    _pardata[k].P.Y := _y - 0.707*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x + 0.707*r ;
    _pardata[k].P.Y := _y - 0.707*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x ;
    _pardata[k].P.Y := _y - r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - 0.707*r ;
    _pardata[k].P.Y := _y - 0.707*r ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - 0.707*r ;
    _pardata[k].P.Y := _y - 0.707*r ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z1 ;
    _pardata[k].Color := _po_color ;
    markerLgth := markerLgth + Sqrt(Sqr(_pardata[k].P.X - _pardata[k-2].P.X) +
                                    Sqr(_pardata[k].P.Y - _pardata[k-2].P.Y)) ;
    inc(k) ;
    _pardata[k].P.X := _x - r ;
    _pardata[k].P.Y := _y ;
    _pardata[k].P.Z := _z2 ;
    _pardata[k].Color := _po_color ;
    inc(k) ;

    Result := k ;
  end;

  procedure TGIS_Renderer3DAbstract.lineBufPnts(
    const _x1  : Double ;
    const _y1  : Double ;
    const _x2  : Double ;
    const _y2  : Double ;
    const _x3  : Double ;
    const _y3  : Double ;
    const _dis : Double ;
    var   _xl  : Double ;
    var   _yl  : Double ;
    var   _xr  : Double ;
    var   _yr  : Double
  ) ;
  var
    a, a1, bbl, bbr, bbl1, bbr1, xs, ys, d,X, Y: Double;
    l1, l2: Integer;
  begin
    l1   := 0 ;
    l2   := 0 ;
    a    := 0 ;
    a1   := 0 ;
    bbl  := 0 ;
    bbr  := 0 ;
    bbl1 := 0 ;
    bbr1 := 0 ;

    if _x1 <> _x2 then begin // line P1 -> P2
      a := ( _y2 - _y1) / ( _x2 - _x1 ) ;

      xs := ( _x1 + _x2 ) *0.5;
      ys := ( _y1 + _y2 ) *0.5;
      d  := Sqrt((_x2 - _x1)*(_x2 - _x1) + (_y2 - _y1)*(_y2 - _y1)) ;

      X := xs - _dis * ( _y2 - _y1 ) / d ;
      Y := ys + _dis * ( _x2 - _x1 ) / d ;
      bbl := Y - a * X ;

      X := xs + _dis * ( _y2 - _y1 ) / d ;
      Y := ys - _dis * ( _x2 - _x1 ) / d ;
      bbr := Y - a * X ;
      if Abs( a ) > 10 then
        l1 := 1 ;
    end
    else
      l1 := 1 ;

    if _x2 <> _x3 then begin // line P2 -> P3
      a1 := ( _y3 - _y2 ) / ( _x3 - _x2 ) ;

      xs := ( _x2 + _x3 ) *0.5 ;
      ys := ( _y2 + _y3 ) *0.5 ;
      d  := Sqrt((_x3 - _x2)*(_x3 - _x2) + (_y3 - _y2)*(_y3 - _y2)) ;

      X := xs - _dis * ( _y3 - _y2 ) / d ;
      Y := ys + _dis * ( _x3 - _x2 ) / d ;
      bbl1 := Y - a1 * X ;

      X := xs + _dis * ( _y3 - _y2 ) / d ;
      Y := ys - _dis * ( _x3 - _x2 ) / d ;
      bbr1 := Y - a1 * X ;
      if Abs( a1 ) > 10 then
        l2 := 1 ;
    end
    else
      l2 := 1 ;

    if (l1=0) and (l2=0) and (Abs(a-a1)>0.15) then begin
      _xl := ( bbl - bbl1 ) / ( a1 - a ) ;
      _yl := a * _xl + bbl ;
      _xr := ( bbr - bbr1 ) / ( a1 - a ) ;
      _yr := a * _xr + bbr ;
    end
    else begin
      d  := Sqrt((_x3 - _x1)*(_x3 - _x1) + (_y3 - _y1)*(_y3 - _y1)) ;
      if d = 0.0 then d := 1.0 ;
      _xl := _x2 - _dis * ( _y3 - _y1 ) / d ;
      _yl := _y2 + _dis * ( _x3 - _x1 ) / d ;
      _xr := _x2 + _dis * ( _y3 - _y1 ) / d ;
      _yr := _y2 - _dis * ( _x3 - _x1 ) / d ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.make3dLine(
    const _size    : Integer       ;
    const _k       : Integer       ;
    var   _cbuffer : T_arColorData ;
    var   _pardata : T_arColorData ;
    const _px      : Double
  ) ;
  var
    r, siz   : Double  ;
    i,j,jj,k : Integer ;
    x1,x2,x3,
    y1,y2,y3,
    xl,yl,
    xr,yr    : Double  ;
    closed   : Boolean ;
  begin
    k := _k ;
    if k < 4 then exit ; // less then 2 points

    siz := _size;
    r   := 1.0 * _px * siz ;
    j   := k*2+1 ;
    i   := -1 ;
    jj  := 0 ;

    if (_pardata[0].P.X = _pardata[k-1].P.X) and
       (_pardata[0].P.Y = _pardata[k-1].P.Y) then
      closed := True
    else
      closed := False ;

      while True do begin  // walls
        if i >= k-2 then
          break
        else begin
          if i = -1 then begin //first point
            i  := 0 ;
            x2 := _pardata[i+1].P.X ;
            y2 := _pardata[i+1].P.Y ;
            x3 := _pardata[i+3].P.X ;
            y3 := _pardata[i+3].P.Y ;
            if closed then begin
              x1 := _pardata[k-3].P.X ;
              y1 := _pardata[k-3].P.Y ;
            end
            else begin
              x1 := 2.0*x2-x3 ;
              y1 := 2.0*y2-y3 ;
            end;
            lineBufPnts(x1,y1,x2,y2,x3,y3,r,xl,yl,xr,yr) ;
            _cbuffer[jj].P.X := xr ;
            _cbuffer[jj].P.Y := yr ;
            _cbuffer[jj].P.Z := _pardata[i].P.Z ;
            _cbuffer[jj].Color := _pardata[i].Color ;
            inc(jj) ;
            _cbuffer[jj].P.X := xr ;
            _cbuffer[jj].P.Y := yr ;
            _cbuffer[jj].P.Z := _pardata[i+1].P.Z ;
            _cbuffer[jj].Color := _pardata[i+1].Color ;
            inc(jj) ;
            _cbuffer[jj].P.X := xl ;
            _cbuffer[jj].P.Y := yl ;
            _cbuffer[jj].P.Z := _pardata[i].P.Z ;
            _cbuffer[jj].Color := _pardata[i].Color ;
            inc(jj) ;
            _cbuffer[jj].P.X := xl ;
            _cbuffer[jj].P.Y := yl ;
            _cbuffer[jj].P.Z := _pardata[i+1].P.Z ;
            _cbuffer[jj].Color := _pardata[i+1].Color ;
            inc(jj) ;
            _cbuffer[j].P.X := xr ;
            _cbuffer[j].P.Y := yr ;
            _cbuffer[j].P.Z := _pardata[i+1].P.Z ;
            _cbuffer[j].Color := _pardata[i+1].Color ;
            dec(j) ;
            _cbuffer[j].P.X := xr ;
            _cbuffer[j].P.Y := yr ;
            _cbuffer[j].P.Z := _pardata[i].P.Z ;
            _cbuffer[j].Color := _pardata[i].Color ;
            dec(j) ;
            i := i-2 ;
          end
          else if i = k-4 then begin // last point
            x1 := _pardata[i+1].P.X ;
            y1 := _pardata[i+1].P.Y ;
            x2 := _pardata[i+3].P.X ;
            y2 := _pardata[i+3].P.Y ;
            if closed then begin
              x3 := _pardata[3].P.X ;
              y3 := _pardata[3].P.Y ;
            end
            else begin
              x3 := 2.0*x2-x1 ;
              y3 := 2.0*y2-y1 ;
            end;
            lineBufPnts(x1,y1,x2,y2,x3,y3,r,xl,yl,xr,yr) ;
            _cbuffer[jj].P.X := xl ;
            _cbuffer[jj].P.Y := yl ;
            _cbuffer[jj].P.Z := _pardata[k-2].P.Z ;
            _cbuffer[jj].Color := _pardata[k-2].Color ;
            inc(jj) ;
            _cbuffer[jj].P.X := xl ;
            _cbuffer[jj].P.Y := yl ;
            _cbuffer[jj].P.Z := _pardata[k-1].P.Z ;
            _cbuffer[jj].Color := _pardata[k-1].Color ;
            inc(jj) ;
            _cbuffer[j].P.X := xr ;
            _cbuffer[j].P.Y := yr ;
            _cbuffer[j].P.Z := _pardata[k-1].P.Z ;
            _cbuffer[j].Color := _pardata[k-1].Color ;
            dec(j) ;
            _cbuffer[j].P.X := xr ;
            _cbuffer[j].P.Y := yr ;
            _cbuffer[j].P.Z := _pardata[k-2].P.Z ;
            _cbuffer[j].Color := _pardata[k-2].Color ;
            dec(j) ;
          end
          else begin // middle points
            x1 := _pardata[i+1].P.X ;
            y1 := _pardata[i+1].P.Y ;
            x2 := _pardata[i+3].P.X ;
            y2 := _pardata[i+3].P.Y ;
            x3 := _pardata[i+5].P.X ;
            y3 := _pardata[i+5].P.Y ;
            lineBufPnts(x1,y1,x2,y2,x3,y3,r,xl,yl,xr,yr) ;
            _cbuffer[jj].P.X := xl ;
            _cbuffer[jj].P.Y := yl ;
            _cbuffer[jj].P.Z := _pardata[i+2].P.Z ;
            _cbuffer[jj].Color := _pardata[i+2].Color ;
            inc(jj) ;
            _cbuffer[jj].P.X := xl ;
            _cbuffer[jj].P.Y := yl ;
            _cbuffer[jj].P.Z := _pardata[i+3].P.Z ;
            _cbuffer[jj].Color := _pardata[i+3].Color ;
            inc(jj) ;
            _cbuffer[j].P.X := xr ;
            _cbuffer[j].P.Y := yr ;
            _cbuffer[j].P.Z := _pardata[i+3].P.Z ;
            _cbuffer[j].Color := _pardata[i+3].Color ;
            dec(j) ;
            _cbuffer[j].P.X := xr ;
            _cbuffer[j].P.Y := yr ;
            _cbuffer[j].P.Z := _pardata[i+2].P.Z ;
            _cbuffer[j].Color := _pardata[i+2].Color ;
            dec(j) ;
          end;
          i := i+2 ;
        end;// finally
      end;  // while
  end;

  function TGIS_Renderer3DAbstract.getNewPntPosition(
    const _pnt : TGIS_Point3D ;
    const _rx  : Double ;
    const _ry  : Double ;
    const _rz  : Double ;
    const _vec : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    c1,c2,c3,s1,s2,s3 : Double ;
    X,Y,Z       : Double ;

    function rotx : TGIS_Point3D ;
    begin
      Result.X := X    + 0    + 0    ;
      Result.Y := 0    + c1*Y - s1*Z ;
      Result.Z := 0    + s1*Y + c1*Z ;
    end;

    function roty : TGIS_Point3D ;
    begin
      Result.X := c2*X + 0    + s2*Z ;
      Result.Y := 0    + Y    + 0    ;
      Result.Z :=-s2*X + 0    + c2*Z ;
    end;

    function rotz : TGIS_Point3D ;
    begin
      Result.X := c3*X - s3*Y + 0    ;
      Result.Y := s3*X + c3*Y + 0    ;
      Result.Z := 0    + 0    + Z    ;
    end;

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

    Result.X := Result.X + _vec.X ;
    Result.Y := Result.Y + _vec.Y ;
    Result.Z := Result.Z + _vec.Z ;
  end ;

  function  TGIS_Renderer3DAbstract.checkWindowSize : Boolean ;
  begin
    if (oldwindowWidth <> windowWidth) or (oldwindowHeight <> windowHeight) then
      Result := True
    else
      Result := False ;
  end;

  procedure TGIS_Renderer3DAbstract.colorToTexture(
    const _cl : Cardinal ;
    var   _t1 : Single ;
    var   _t2 : Single
  ) ;
  var
    r,g,b : Byte ;
    l : Integer ;
  begin
    r := TruncS(((_cl shr 16) and $000000FF) / 4) ;
    g := TruncS(((_cl shr 08) and $000000FF) / 4) ;
    b := TruncS(((_cl shr  0) and $000000FF) / 4) ;
    l := r + g*64 + b*4096 ;
    _t1 := (0.5 + (l div 512)) / 512 ;
    _t2 := (0.5 + (l mod 512)) / 512 ;
  end;

  function TGIS_Renderer3DAbstract.getPrinterPixelSize(
    const _radius : Double
  ) : Double ;
  begin
    Result := _radius * Tan(DegToRad(cameraHalfViewAngle)) / windowHeight ;
    if ( printBmpImgSize <> 0 ) then  begin
      Result := 0.9*( Result * 4096 / printBmpOutSize.Y ) ;
      Result := Result * printBmpPPI / 600 ;
      Result := Result * 96 / oGIS.PPI ;
    end;
  end;

  procedure TGIS_Renderer3DAbstract.setDefaultZ ;
  begin
    defaultZ := zScaleFactor * (zMin - zLevel) / zUnit ;
//    defaultZ := zScaleFactor * (basePlane.Level - zMin) / zUnit ;
  end;

  procedure TGIS_Renderer3DAbstract.setLod;
  begin
    lod := 0 ;
    repeat
      lodStep := TruncS(Power(2.0, lod * 1.0)) ;
      inc(lod) ;
    until (demFrameSize/lodStep) < GISVIEWER3D_LOD_FACTOR ;
    dec(lod) ;
  end;


//==================================== END =====================================
end.

