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
  Encapsulation of Open Asset Import Library (assimp).
}

{$IFDEF DCC}
  unit GisLayerAssimp ;
  {$HPPEMIT '#pragma link "GisLayerAssimp"'}
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

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,
    System.Classes,

    System.Generics.Collections,

    GisClasses,
    GisTypes,

    GisStreams,
    GisLayerVector ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerAssimp = class
    public
      class procedure SelfRegisterLayer() ;
  end ;


  /// <summary>
  ///   Layer which can read 3D models via Assimp library.
  /// </summary>
  TGIS_LayerAssimp = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      translate     : TGIS_SingleVector ;
      scale         : TGIS_SingleVector ;
      rotate        : TGIS_SingleVector ;
    private
      FAxisOrder    : Integer ;
      FUseMaterials : Boolean ;
      FOpenMode     : Integer ;
    private
      procedure checkBoxFile ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      procedure fset_UseRTree         ( const _value : Boolean
                                      ) ; override;

     /// <inheritdoc/>
      procedure setUp ; override;

     // cursor access function(s)
    protected
      // destructors

      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  DrawEx            ( const _extent : TGIS_Extent
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      function  PreRecognize      ( const _path   : String ;
                                    var _new_path : String
                                  ) : Boolean ; override;
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
      ///   Open mode defines processing steps to be executed after a successful import.
      ///   0 - default, 1 - Fast mode, 2 - Quality mode
      /// </summary>
      property OpenMode     : Integer read  FOpenMode
                                      write FOpenMode ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.IOUtils,
    GisRtl,
    GisTypesUI,
    GisFunctions,
    GisInternals,
    GisInterfaces,
    GisLayer,
    GisFileAssimp,
    GisResource,
    GisRegistredLayers ;
{$ENDIF}

const
  GIS_ASSIMP_SUPPORTED_EXT =
    '.3d;.3ds;.3mf;.ac;.ac3d;.acc;.amf;.ase;.ask;.assbin;.b3d;.blend;.bvh;.cob;'+
    '.csm;.dae;.enff;.fbx;.glb;.gltf;.hmp;.ifc;.ifczip;.irr;.irrmesh;.lwo;.lws;'+
    '.lxo;.m3d;.md2;.md3;.md5anim;.md5camera;.md5mesh;.mdc;.mdl;.mesh;'+
    '.mot;.ms3d;.ndo;.nff;.obj;.off;.ogex;.pk3;.pmx;.prj;.q3o;.q3s;.raw;.scn;.sib;'+
    '.smd;.stp;.ter;.uc;.vta;.x;.x3d;.x3db;.xgl;.zae;.zgl' ;


//=============================================================================
// TGIS_LayerAssimp
//=============================================================================

  constructor TGIS_LayerAssimp.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory ] ;

    FAxisOrder    := 0 ;
    FUseMaterials := True ;
    FOpenMode     := 1 ;
  end ;

  procedure TGIS_LayerAssimp.doDestroy ;
  begin

    inherited ;
  end ;

  procedure TGIS_LayerAssimp.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_LayerAssimp.checkBoxFile ;
  var
    lst   : TStringList ;
    fname : String ;
    tkn   : TGIS_Tokenizer ;
    i     : Integer ;
  begin
    translate := GisSingleVector( 0, 0, 0 ) ;
    scale     := GisSingleVector( 1, 1, 1 ) ;
    rotate    := GisSingleVector( 0, 0, 0 ) ;

    fname  := GetPathDirSep( GetFileDir(Path) ) + GetFileNameNoExt( Path ) + '.box' ;
    if not SafeFileExists( fname ) then exit ;

    lst := TStringList.Create ;
    try
      lst.LoadFromFile( fname ) ;

      tkn := TGIS_Tokenizer.Create ;
      try
        for i := 0 to lst.Count-1 do begin
          tkn.Execute( lst[i], [' '] ) ;
            if CompareText( tkn.Result[0], 'translate' ) = 0 then begin
              if tkn.Result.Count > 3 then begin
                translate.X := DotStrToFloat( tkn.Result[1] ) ;
                translate.Y := DotStrToFloat( tkn.Result[2] ) ;
                translate.Z := DotStrToFloat( tkn.Result[3] ) ;
              end
            end
            else if CompareText( tkn.Result[0], 'scale' ) = 0 then begin
              if tkn.Result.Count > 3 then begin
                scale.X := DotStrToFloat( tkn.Result[1] ) ;
                scale.Y := DotStrToFloat( tkn.Result[2] ) ;
                scale.Z := DotStrToFloat( tkn.Result[3] ) ;
              end
            end
            else if CompareText( tkn.Result[0], 'rotate' ) = 0 then begin
              if tkn.Result.Count > 3 then begin
                rotate.X := DotStrToFloat( tkn.Result[1] ) ;
                rotate.Y := DotStrToFloat( tkn.Result[2] ) ;
                rotate.Z := DotStrToFloat( tkn.Result[3] ) ;
              end
            end
            else if CompareText( tkn.Result[0], 'axis' ) = 0 then begin
              if tkn.Result.Count > 1 then
                FAxisOrder := StrToInt( tkn.Result[1] ) ;
            end
            else if CompareText( tkn.Result[0], 'materials' ) = 0 then begin
              if tkn.Result.Count > 1 then
                FUseMaterials := StrToInt( tkn.Result[1] ) = 1 ;
            end
            else if CompareText( tkn.Result[0], 'openmode' ) = 0 then begin
              if tkn.Result.Count > 1 then
                FOpenMode := StrToInt( tkn.Result[1] )  ;
            end ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_LayerAssimp.setUp ;
  var
    aiFile : TGIS_FileAssimp ;
  begin
    inherited ;

    FSupportedShapes       := GisGetShapeType( TGIS_ShapeType.MultiPatch ) +
                              GisGetShapeType( TGIS_ShapeType.Point      ) +
                              GisGetShapeType( TGIS_ShapeType.MultiPoint ) ;

    FSupportedDimensions   := GisGetDimensionType( TGIS_DimensionType.XYZM ) +
                              GisGetDimensionType( TGIS_DimensionType.XYZ ) ;

    // block selection to avoid gdi hanging
    FUnSupportedOperations := GisAddOperationType( FUnSupportedOperations,
                                                   TGIS_OperationType.Select
                                                  ) ;
    // block editing to avoid gdi hanging
    FUnSupportedOperations := GisAddOperationType( FUnSupportedOperations,
                                                   TGIS_OperationType.Edit
                                                  ) ;

    if not IsStringEmpty( Path ) and SafeFileExists( Path ) then begin
      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
      Lock ;
      try
        aiFile := TGIS_FileAssimp.Create ;
        try
          if aiFile.Open( Path, FOpenMode ) then begin
            if Pos( '.IFC', UpperCase( GetFileExt( Path ) ) ) = StringFirst then
              FAxisOrder := 1 ;

            checkBoxFile ;
            aiFile.AxisOrder    := FAxisOrder ;
            aiFile.UseMaterials := FUseMaterials ;
            aiFile.Translate    := translate ;
            aiFile.Scale        := scale ;
            aiFile.Rotate       := rotate ;
            aiFile.BuildMeshes( Self ) ;
          end ;

          RecalcProjectedExtent ;

          FFileInfo := 'Open Asset Import Library'  ;
        finally
          FreeObject( aiFile ) ;
        end ;
      finally
        Unlock ;
        RaiseBusyRelease( Self ) ;
      end ;
    end ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;
  end ;

  function TGIS_LayerAssimp.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    shp : TGIS_Shape ;
  begin
    Result := IsVisible( _extent ) ;
    if not Result then exit ;

    // just a fake area covering the model to avoid drawing mess in gdi
    shp := TGIS_ShapePolygon.Create( nil, nil, False, -1, self, TGIS_DimensionType.XY ) ;
    try
      shp.Lock( TGIS_Lock.Internal ) ;
      shp.AddPart ;
      shp.AddPoint( GisPoint( Extent.XMin, Extent.YMin ) ) ;
      shp.AddPoint( GisPoint( Extent.XMax, Extent.YMin ) ) ;
      shp.AddPoint( GisPoint( Extent.XMax, Extent.YMax ) ) ;
      shp.AddPoint( GisPoint( Extent.XMin, Extent.YMax ) ) ;
      shp.Unlock ;
      shp.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      shp.Draw ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  function TGIS_LayerAssimp.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  var
    ext  : String ;
  begin
    Result := inherited PreRecognize( _path, _new_path );

    ext := LowerCase( GetFileExt( _path ) ) ;
    Result := Pos( ext, GIS_ASSIMP_SUPPORTED_EXT ) >= StringFirst ;
  end;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerAssimp.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-ASSIMP', 'Open Asset Import Library',
                   TGIS_LayerAssimp, '.*',
                   TGIS_RegisteredLayerType.Vector3D,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                    GIS_LOWER_LAYER_PRIORITY, False
                  ) ;
   end ;

{$IFDEF DCC}
  initialization
    Unit_GisLayerAssimp.SelfRegisterLayer() ;
{$ENDIF}


end.

