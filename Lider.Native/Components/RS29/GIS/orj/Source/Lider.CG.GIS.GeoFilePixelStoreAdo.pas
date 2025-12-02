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
  Encapsulation of PixelStore - DBX access.
}

{$IFDEF DCC}
  unit GisFilePixelStoreAdo ;
  {$HPPEMIT '#pragma link "GisFilePixelStoreAdo"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE GisInclude.inc}

{$IFDEF GIS_NOADO}
  {$Message Error 'Unit not supported - no ADO support' }
{$ENDIF}

interface

{$IFDEF OXYGENE}
  uses
    TatukGIS.RTL,
    ADODB ;
{$ELSE}
  uses
    System.SysUtils,
    System.Classes,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Variants,
    {$IFDEF ADOINTUNIT}
      AdoInt,
    {$ELSE}
      GisAdoInt,
    {$ENDIF}

    GisTypes,
    GisStreams,
    GisFilePixelStore,
    GisCsSystems;
{$ENDIF}

type

  /// <summary>
  ///   Encapsulation of PixelStore access via ADO.
  /// </summary>
  TGIS_FilePixelStoreAdo = {$IFDEF OXYGENE} public {$ENDIF}
                           class ( TGIS_FilePixelStoreAbstract )
    protected
        procedure fset_SharedConnection   ( const _obj : _Connection );
        function  fget_SharedConnection : _Connection ;
    protected

      /// <inheritdoc/>
      function  sqlQueryGetCell       ( const _name        : String
                                      ) : TStream ; override;

      /// <inheritdoc/>
      procedure sqlTableOpenWrite     ( const _table       : String ;
                                        const _filter      : String
                                      ) ; override;

      /// <inheritdoc/>
      procedure sqlTableAppend        ( const _table       : String
                                      ) ; override;

      /// <inheritdoc/>
      procedure sqlTableSetCell       ( const _name        : String            ;
                                        const _blob        : TGIS_MemoryStream
                                      ) ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy             ; override;
    public

      /// <inheritdoc/>
      constructor Create              ; overload; override;

      /// <inheritdoc/>
      constructor Create              ( const _path        : String      ;
                                        const _ext         : TGIS_Extent ;
                                        const _width       : Integer     ;
                                        const _height      : Integer     ;
                                        const _subformat   : TGIS_LayerPixelSubFormat     ;
                                        const _ppi         : Integer     ;
                                        const _cs          : TGIS_CSCoordinateSystem
                                      ) ; overload; override;

      /// <inheritdoc/>
      function    Prerecognize        ( const _path        : String
                                      ) : Boolean ; override;
    public

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   If assigned then external assigned connection will be used.
      /// </summary>
      property SharedConnection : _Connection    read  fget_SharedConnection
                                                 write fset_SharedConnection ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    GisRtl,
    GisInterfaces,
    GisInternals,
    GisDbAdo,
    GisResource,
    GisConfig,
    GisUtils ;
{$ENDIF}

  constructor TGIS_FilePixelStoreAdo.Create;
  begin
    inherited ;

    oGisDb := TGIS_DbAdo.Create ;
  end ;

  procedure TGIS_FilePixelStoreAdo.fset_SharedConnection(
    const _obj : _Connection
  ) ;
  begin
    TGIS_DbAdo( oGisDb ).SharedConnection := _obj ;
  end ;

  function TGIS_FilePixelStoreAdo.fget_SharedConnection : _Connection;
  begin
    Result := TGIS_DbAdo( oGisDb ).SharedConnection ;
  end ;

  function TGIS_FilePixelStoreAdo.sqlQueryGetCell(
    const _name : String
  ) : TStream ;
  begin
    Result := oGisDb.sqlQueryGetBlob( _name,0 ) ;
  end ;

  procedure TGIS_FilePixelStoreAdo.sqlTableOpenWrite(
    const _table  : String  ;
    const _filter : String
  ) ;
  begin
    oGisDb.sqlTableOpenWrite( 0, prepareSelectCommand( _table, _filter ) );
  end ;

  procedure TGIS_FilePixelStoreAdo.sqlTableAppend(
    const _table : String
  ) ;
  var
    query : String ;
  begin
    query := prepareSelectCommand( _table, prepareFilterCell( -1, -1, -1 ) );
    oGisDb.sqlTableAppend( 0, query );
  end ;

  procedure TGIS_FilePixelStoreAdo.sqlTableSetCell(
    const _name   : String            ;
    const _blob   : TGIS_MemoryStream
  ) ;
  var
    store : Variant ;
    {$IFDEF CLR}
      vr    : TBytes  ;
    {$ELSE}
      pbuf  : Pointer ;
    {$ENDIF}
  begin
    store := VarArrayCreate([0,_blob.Size], varByte ) ;

    {$IFDEF CLR}
      SetLength( vr, _blob.Size ) ;
      System.Array.Copy( _blob.Memory, vr, _blob.Size ) ;
      store := vr ;
    {$ELSE}
      pbuf := VarArrayLock( store ) ;
      CopyMemory( pbuf, _blob.Memory, _blob.Size ) ;
      VarArrayUnlock( store ) ;
    {$ENDIF}

    oGisDb.sqlTableSetGeometry( 0, _name, store, nil );

    store := Unassigned ;
  end ;

  constructor TGIS_FilePixelStoreAdo.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat     ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  var
    layername     : String   ;
    canonicalname : String   ;
    ttkpspath     : String   ;
    provider      : String   ;
    cfg           : TGIS_Config ;
    mdbpath       : String   ;
  begin
    {$IFDEF OXYGENE}
      inherited Create( _path, _ext, _width, _height,
                        _subformat, _ppi, _cs
                      ) ;
    {$ENDIF}

    try
      if GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_LAYER, layername ) = GIS_SQL_PROVIDER_ADO then
      begin
        if not IsStringEmpty( layername ) then // only new TTKPS will issue MDB file creation
          exit ;

        if SafeFileExists( ttkpspath ) then
          exit ;

        ttkpspath := GetPathAbsolute( '', _path ) ;

        layername     := GetFileName( GetPathNoExt( _path ) ) ;
        canonicalname := TGIS_Utils.GisCanonicalSQLName( layername )  ;

        mdbpath := GetFilePath( ttkpspath ) +
                   GIS_INI_LAYERSQL_DEFAULT_DATABASE ;

        if SafeFileExists( mdbpath ) then begin
          provider := CreateMSJET( mdbpath ) ;
        end
        else begin
          mdbpath := GetFilePath( ttkpspath ) +
                     Format( GIS_INI_LAYERSQL_MDB_DATABASE, [ layername ] ) ;
          provider := CreateMSJET( mdbpath ) ;
        end ;

        cfg := TGIS_ConfigFactory.CreateConfig( nil, _path ) ;
        try
          if cfg.ConfigFormat = TGIS_ConfigFormat.Ini then
            cfg.Section := GIS_INI_LAYER_HEADER
          else
            cfg.Section := GIS_INI_LAYERSQL_CONNECTOR ;

          cfg.WriteString( GIS_INI_LAYERSQL_STORAGE,
                           GIS_INI_LAYERSQL_PIXELSTORE2,
                           ''
                         ) ;
          cfg.WriteString( GIS_INI_LAYERSQL_LAYER,
                           canonicalname,
                           ''
                         ) ;
          cfg.WriteString( GIS_INI_LAYERSQL_DIALECT,
                           GIS_SQL_DIALECT_NAME_MSJET,
                           ''
                         ) ;
          cfg.WriteString( GIS_INI_LAYERSQL_CONNECTOR_ADO,
                           provider,
                           ''
                         ) ;
          cfg.WriteString( GIS_INI_LAYERSQL_CONNECTOR_ADO64,
                           provider,
                           ''
                         ) ;
        finally
          cfg.Save ;
          FreeObject( cfg ) ;
        end ;

      end ;
    finally
      {$IFNDEF OXYGENE}
        inherited Create( _path, _ext, _width, _height,
                          _subformat, _ppi, _cs
                        ) ;
      {$ENDIF}
      oGisDb := TGIS_DbAdo.Create ;

      doCreateForWrite( _path, _ext, _width, _height,
                        _subformat, _ppi, _cs
                      );
      oGisDb.UseTextParameters := False ;
    end;
  end ;

  procedure TGIS_FilePixelStoreAdo.doDestroy ;
  begin
    FreeObject( oGisDb );

    inherited ;
  end ;

  function TGIS_FilePixelStoreAdo.Prerecognize(
    const _path : String
  ) : Boolean ;
  begin
    Result := doPrerecognize( _path ) = GIS_SQL_PROVIDER_ADO ;
  end ;

//==================================== END =====================================
end.

