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
  Encapsulation of the VPF 3.0 file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerVPF ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerVPF"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}


{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerCompound,
    Lider.CG.GIS.GeoLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerVPF = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  /// Layer that can read VPF dataset.
  /// </summary>
  TGIS_LayerVPF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerCompoundVector )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      // vpf file handle.
      VPF       : TObject ;

      // current shape.
      oShape    : TGIS_Shape ;

      // current layer.
      oLayer    : TGIS_LayerVector ;

      // current category.
      FCategory : String ;

      // current feature.
      FFeature  : String ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private routines

      /// <summary>
      ///   On set layer event method.
      /// </summary>
      procedure onSetLayer  ( const _name     : String ;
                              const _caption  : String ;
                              const _path     : String ;
                              const _shpType  : TGIS_ShapeType
                            ) ;

      /// <summary>
      ///   On begin shape event method.
      /// </summary>
      procedure onBeginShape( const _shpType  : TGIS_ShapeType ;
                              const _dim      : TGIS_DimensionType
                            );

      /// <summary>
      ///   On add part event method.
      /// </summary>
      procedure onAddPart   ;

      /// <summary>
      ///   On add coordinate event method.
      /// </summary>
      procedure onAddCoord  ( const _ptg      : TGIS_Point3D
                            );

      /// <summary>
      ///   On end shape event method.
      /// </summary>
      procedure onEndShape ;

      /// <summary>
      ///   On add attribute event method.
      /// </summary>
      procedure onAddAttr   ( const _name     : String ;
                              const _type     : TGIS_FieldType ;
                              const _val      : Variant
                            ) ;
      {$IFDEF OXYGENE}
        /// <summary>
        ///   On busy event method.
        /// </summary>
        procedure onBusy    ( const _sender    : TObject ;
                              const _e         : TGIS_BusyEventArgs
                             ) ;
      {$ELSE}
        /// <summary>
        ///   On busy event method.
        /// </summary>
        procedure onBusy    (       _sender    : TObject ;
                                    _pos, _end : Integer ;
                              var   _abort     : Boolean
                             ) ;
      {$ENDIF}

      /// <summary>
      ///   Check category settings.
      /// </summary>
      procedure checkCategory( const _path : String ) ;

      /// <summary>
      ///   Check feature settings.
      /// </summary>
      procedure checkFeature ( const _cat  : String ) ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      procedure setUp     ; override;

    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      /// <inheritdoc/>
      constructor Create  ; override;

      /// <inheritdoc/>
      function  PreRecognize     ( const _path     : String ;
                                     var _new_path : String
                                  ) : Boolean ; override;
     /// <inheritdoc/>
     function GetAvailableLayers : TGIS_LayerInfoList ;  override;
    public

      /// <summary>
      ///   Set a library category name to read.
      /// </summary>
      property Category : String read FCategory ;
                                 {$IFDEF OXYGENE}reintroduce ;{$ENDIF}

      /// <summary>
      ///   Set a category feature name to read.
      /// </summary>
      property Feature  : String read FFeature ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.SysUtils,

    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoFileVPF ;
{$ENDIF}

//=============================================================================
// TGIS_LayerVPF
//=============================================================================

  constructor TGIS_LayerVPF.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;
  end ;

  procedure TGIS_LayerVPF.doDestroy ;
  begin

    inherited ;
  end ;

  procedure TGIS_LayerVPF.onSetLayer(
    const _name    : String ;
    const _caption : String ;
    const _path    : String ;
    const _shpType : TGIS_ShapeType
   ) ;
  var
    lv : TGIS_LayerVector ;
  begin
    oLayer := TGIS_LayerVector( Get( _name ) ) ;
    if oLayer <> nil then exit ;

    lv := TGIS_LayerVector.Create ;
    lv.Name             := _name ;
    lv.Caption          := _caption ;
    lv.DefaultShapeType := _shpType ;
    lv.DefaultDimension := TGIS_DimensionType.XY ;
    lv.SetCSByEPSG( GIS_EPSG_WGS84 ) ;
    lv.Path := Path ;
    Add( lv ) ;
    lv.Path := GetPathDirSep(Path) + _path ;
    lv.SupportedShapes     := GisGetShapeType( _shpType ) ;
    lv.SupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XY ) ;
    oLayer := lv ;
  end ;

  procedure TGIS_LayerVPF.onBeginShape(
    const _shpType  : TGIS_ShapeType ;
    const _dim      : TGIS_DimensionType
  ) ;
  begin
    oShape := oLayer.CreateShape( _shpType, _dim ) ;
    oShape.Lock( TGIS_Lock.Projection ) ;
  end ;

  procedure TGIS_LayerVPF.onAddAttr(
    const _name   : String ;
    const _type   : TGIS_FieldType ;
    const _val    : Variant
  ) ;
  begin
    if assigned( oShape ) and ( oShape.Layer.FindField( _name ) < 0 ) then
      case _type of
        TGIS_FieldType.String  :
          oShape.Layer.AddFieldInternal( _name, TGIS_FieldType.String, 1 , 0 ) ;
        TGIS_FieldType.Number  :
          oShape.Layer.AddFieldInternal( _name, TGIS_FieldType.Number, 10 , 2 ) ;
        TGIS_FieldType.Float   :
          oShape.Layer.AddFieldInternal( _name, TGIS_FieldType.Float, 20 , 5 ) ;
        TGIS_FieldType.Boolean :
          oShape.Layer.AddFieldInternal( _name, TGIS_FieldType.Boolean, 1 , 0 ) ;
        TGIS_FieldType.Date    :
          oShape.Layer.AddFieldInternal( _name, TGIS_FieldType.Date, 1 , 0 ) ;
      end ;

    oShape.SetField( _name, _val ) ;
  end ;

  procedure TGIS_LayerVPF.onAddCoord(
    const _ptg : TGIS_Point3D
  ) ;
  begin
    oShape.AddPoint3D( _ptg ) ;
  end ;

  procedure TGIS_LayerVPF.onAddPart ;
  begin
    oShape.AddPart ;
  end ;

  procedure TGIS_LayerVPF.onEndShape ;
  begin
    oShape.Unlock ;
  end ;

  {$IFDEF OXYGENE}
    procedure TGIS_LayerVPF.onBusy(
      const _sender    : TObject ;
      const _e         : TGIS_BusyEventArgs
    ) ;
  {$ELSE}
    procedure TGIS_LayerVPF.onBusy(
            _sender    : TObject ;
            _pos, _end : Integer ;
      var   _abort     : Boolean
     ) ;
  {$ENDIF}
  begin
    HourglassShake ;

    {$IFDEF OXYGENE}
      _e.Abort := RaiseBusyShake( _sender, _e.Pos, _e.EndPos  ) ;
    {$ELSE}
      _abort := RaiseBusyShake( _sender, _pos, _end  ) ;
    {$ENDIF}
  end ;

  procedure TGIS_LayerVPF.setUp ;
  var
    sdir : String ;
  begin
    inherited ;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;

    try
      sdir := GetFilePath( Path ) ;
      if IsStringEmpty( Driver ) then
        checkCategory( sdir )
      else
        checkCategory( sdir + '?' + Driver ) ;

      if not assigned( VPF ) then
        VPF := TGIS_FileVPF.Create( sdir ) ;

      try
        // events
        TGIS_FileVPF(VPF).NewShapeEvent := {$IFDEF OXYGENE}@{$ENDIF}onBeginShape ;
        TGIS_FileVPF(VPF).EndShapeEvent := {$IFDEF OXYGENE}@{$ENDIF}onEndShape   ;
        TGIS_FileVPF(VPF).AddPartEvent  := {$IFDEF OXYGENE}@{$ENDIF}onAddPart    ;
        TGIS_FileVPF(VPF).AddCoordEvent := {$IFDEF OXYGENE}@{$ENDIF}onAddCoord   ;
        TGIS_FileVPF(VPF).AddAttrEvent  := {$IFDEF OXYGENE}@{$ENDIF}onAddAttr    ;
        TGIS_FileVPF(VPF).SetLayerEvent := {$IFDEF OXYGENE}@{$ENDIF}onSetLayer   ;
        TGIS_FileVPF(VPF).BusyEvent     := {$IFDEF OXYGENE}@{$ENDIF}onBusy       ;
        TGIS_FileVPF(VPF).Category   := FCategory ;
        TGIS_FileVPF(VPF).Feature    := FFeature ;
        TGIS_FileVPF(VPF).ReadAll ;

        FFileInfo := 'VPF (Vector Product Format)' + #13#10 +
                     TGIS_FileVPF(VPF).GetInfo ;

      finally
        FreeObject( VPF ) ;
      end ;
    finally
      RaiseBusyRelease( Self ) ;

      FIsModified := False ;
    end ;

    SetCSByEPSG( GIS_EPSG_WGS84 ) ;

  end ;

  procedure TGIS_LayerVPF.checkCategory(
    const _path : String
  ) ;
  var
    k : Integer  ;
  begin
    if not IsServerPath( _path ) then begin
      k := Pos( '?', _path ) ;
      if k < StringFirst then
        FCategory := ''
      else
        checkFeature( Copy( _path, k+1, MaxInt ) ) ;
    end
    else
      FCategory := '' ;
  end ;

  procedure TGIS_LayerVPF.checkFeature(
    const _cat : String
  ) ;
  var
    k : Integer  ;
  begin
    k := Pos( ';', _cat ) ;
    if k < StringFirst then begin
      FFeature  := '' ;
      FCategory := _cat ;
    end
    else begin
      FFeature  := Copy( _cat, k+1, MaxInt ) ;
      FCategory := Copy( _cat, StringFirst, k-StringFirst ) ;
    end ;
  end ;

  function TGIS_LayerVPF.PreRecognize(
    const _path     : String ;
      var _new_path : String
   ) : Boolean ;
  begin
    Result := inherited PreRecognize( _path, _new_path );

    checkCategory( _path ) ;
  end ;

  function TGIS_LayerVPF.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    sdir : String ;
  begin
    sdir := GetFilePath( Path ) ;
    if IsStringEmpty( Driver ) then
      checkCategory( sdir )
    else
      checkCategory( sdir + '?' + Driver ) ;

    if not assigned( VPF ) then
      VPF := TGIS_FileVPF.Create( sdir ) ;

    try
      Result := TGIS_FileVPF(VPF).GetAvailableLayers ;
    finally
      FreeObject( VPF ) ;
    end ;
  end ;

  class procedure GisLayerVPF.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-VPF', 'Vector Product Format VPF', TGIS_LayerVPF,
                   'DHT;LHT', TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    GisLayerVPF.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

