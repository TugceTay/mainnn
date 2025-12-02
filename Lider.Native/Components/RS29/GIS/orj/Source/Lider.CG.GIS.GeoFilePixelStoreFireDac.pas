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
  Encapsulation of PixelStore - FireDac access.
}

{$IFDEF DCC}
  unit GisFilePixelStoreFireDac ;
  {$HPPEMIT '#pragma link "GisFilePixelStoreFireDac"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE GisInclude.inc}

{$IFDEF GIS_NODB}
  {$Message Warn 'Unit GisFilePixelStoreFireDac not supported - no DB support' }
  interface
  implementation
{$ELSE}

interface

uses
  System.Classes,
  System.Variants,

  {$IFNDEF GIS_NODB}
    FireDAC.Comp.Client,
  {$ENDIF}

  GisTypes,
  GIsClasses,
  GisStreams,
  GisFilePixelStore,
  GisCsSystems;

type

  /// <summary>
  ///   Encapsulation of PixelStore access via FireDac (dbExpress).
  /// </summary>
  TGIS_FilePixelStoreFireDac = class ( TGIS_FilePixelStoreAbstract )
    protected
        procedure fset_SharedConnection ( const _obj         : TFDConnection );
        function  fget_SharedConnection : TFDConnection ;
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
      property SharedConnection : TFDConnection read  fget_SharedConnection
                                                write fset_SharedConnection ;
  end ;

//##############################################################################
implementation

uses
  GisRtl,
  GisDbFireDac ;

  procedure TGIS_FilePixelStoreFireDac.fset_SharedConnection(
    const _obj : TFDConnection
  );
  begin
    TGIS_DbFireDac( oGisDb ).SharedConnection := _obj ;
  end;

  function TGIS_FilePixelStoreFireDac.fget_SharedConnection : TFDConnection;
  begin
    Result := TGIS_DbFireDac( oGisDb ).SharedConnection ;
  end;

  function TGIS_FilePixelStoreFireDac.sqlQueryGetCell(
    const _name : String
  ) : TStream ;
  begin
    Result := oGisDb.sqlQueryGetBlob( _name,0 ) ;
  end ;

  procedure TGIS_FilePixelStoreFireDac.sqlTableOpenWrite(
    const _table  : String  ;
    const _filter : String
  ) ;
  begin
    oGisDb.sqlUpdateStart( 0, prepareSelectCommand( _table, _filter ) );
    oGisDb.sqlTableOpenWrite( 0, prepareUpdateCommand( _table, _filter ) );
  end ;

  procedure TGIS_FilePixelStoreFireDac.sqlTableAppend(
    const _table : String
  ) ;
  begin
    oGisDb.sqlTableAppend( 0, prepareAppendCommand( _table ) );
  end ;

  procedure TGIS_FilePixelStoreFireDac.sqlTableSetCell(
    const _name   : String ;
    const _blob   : TGIS_MemoryStream
  ) ;
  begin
    oGisDb.sqlTableSetGeometry( 0, _name, Unassigned, _blob );
  end ;

  constructor TGIS_FilePixelStoreFireDac.Create;
  begin
    inherited ;

    oGisDb := TGIS_DbFireDac.Create ;
  end ;

  constructor TGIS_FilePixelStoreFireDac.Create(
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
    oGisDb := TGIS_DbFireDac.Create ;

    doCreateForWrite( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    );
  end;

  destructor TGIS_FilePixelStoreFireDac.Destroy ;
  begin
    FreeObject( oGisDb );

    inherited ;
  end ;

  function TGIS_FilePixelStoreFireDac.Prerecognize(
    const _path : String
  ) : Boolean ;
  begin
    Result := doPrerecognize( _path ) = GIS_SQL_PROVIDER_FIREDAC ;
  end ;

{$ENDIF} //GIS_NODB

//==================================== END =====================================
end.


