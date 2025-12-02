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
  Encapsulation of PixelStore - DBX (dbExpress) access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFilePixelStoreDbx ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFilePixelStoreDbx"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF GIS_NODB}
  {$Message Warn 'Unit Lider.CG.GIS.GeoFilePixelStoreDbx not supported - no DB support' }
  interface
  implementation
{$ELSE}

interface

uses
  System.Classes,
  System.Variants,

  {$IFNDEF GIS_NODB}
    {$WARNINGS OFF}
      Data.SqlExpr,
    {$WARNINGS ON}
  {$ENDIF}

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoStreams,
  Lider.CG.GIS.GeoFilePixelStore,
  Lider.CG.GIS.GeoCsSystems;

type

  /// <summary>
  ///   Encapsulation of PixelStore access via DBX (dbExpress).
  /// </summary>
  TGIS_FilePixelStoreDbx = class ( TGIS_FilePixelStoreAbstract )
    protected

        procedure fset_SharedConnection ( const _obj         : TSQLConnection );
        function  fget_SharedConnection : TSQLConnection ;

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
      procedure sqlTableSetCell       ( const _name        : String        ;
                                        const _blob        : TGIS_MemoryStream
                                      ) ; override;
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
                                      ) ; override;

      /// <inheritdoc/>
      destructor  Destroy             ; override;

      /// <inheritdoc/>
      function    Prerecognize        ( const _path        : String
                                      ) : Boolean ; override;
    public

      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   If assigned then external assigned connection will be used.
      /// </summary>
      property SharedConnection : TSQLConnection read  fget_SharedConnection
                                                 write fset_SharedConnection ;
  end ;

//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoDbDbx ;

  procedure TGIS_FilePixelStoreDbx.fset_SharedConnection(
    const _obj : TSQLConnection
  );
  begin
    TGIS_DBDBX( oGisDb ).SharedConnection := _obj ;
  end;

  function TGIS_FilePixelStoreDbx.fget_SharedConnection : TSQLConnection;
  begin
    Result := TGIS_DBDBX( oGisDb ).SharedConnection ;
  end;

  function TGIS_FilePixelStoreDbx.sqlQueryGetCell(
    const _name : String
  ) : TStream ;
  begin
    Result := oGisDb.sqlQueryGetBlob( _name,0 ) ;
  end ;

  procedure TGIS_FilePixelStoreDbx.sqlTableOpenWrite(
    const _table  : String  ;
    const _filter : String
  ) ;
  begin
    oGisDb.sqlUpdateStart( 0, prepareSelectCommand( _table, _filter ) );
    oGisDb.sqlTableOpenWrite( 0, prepareUpdateCommand( _table, _filter ) );
  end ;

  procedure TGIS_FilePixelStoreDbx.sqlTableAppend(
    const _table : String
  ) ;
  begin
    oGisDb.sqlTableAppend( 0, prepareAppendCommand( _table ) );
  end ;

  procedure TGIS_FilePixelStoreDbx.sqlTableSetCell(
    const _name   : String ;
    const _blob   : TGIS_MemoryStream
  ) ;
  begin
    oGisDb.sqlTableSetGeometry( 0, _name, Unassigned, _blob );
  end ;

  constructor TGIS_FilePixelStoreDbx.Create;
  begin
    inherited ;

    oGisDb := TGIS_DBDBX.Create ;
  end ;

  constructor TGIS_FilePixelStoreDbx.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat     ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  begin
    inherited Create( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    ) ;
    oGisDb := TGIS_DBDBX.Create ;

    doCreateForWrite( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    );
  end;

  destructor TGIS_FilePixelStoreDbx.Destroy ;
  begin
    FreeObject( oGisDb );

    inherited ;
  end ;

  function TGIS_FilePixelStoreDbx.Prerecognize(
    const _path : String
  ) : Boolean ;
  begin
    Result := doPrerecognize( _path ) = GIS_SQL_PROVIDER_DBX ;
  end ;

{$ENDIF} //GIS_NODB

//==================================== END =====================================
end.


