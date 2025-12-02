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
  Encapsulation of PixelStore - AdoNet  access.
}

{$IFDEF DCC}
  unit GisFilePixelStoreAdoNet ;
  {$HPPEMIT '#pragma link "GisFilePixelStoreAdoNet"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF OXYGENE}
  uses
    TatukGIS.RTL;
{$ELSE}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Classes,
    System.Variants,

    GisTypes,
    GisStreams,
    GisFilePixelStore,
    GisCsSystems;
{$ENDIF}

type

  /// <summary>
  ///   Encapsulation of PixelStore layer via AdoNet.
  /// </summary>
  TGIS_FilePixelStoreAdoNet = {$IFDEF OXYGENE} public {$ENDIF}
                                class ( TGIS_FilePixelStoreAbstract )
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
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    GisRtl,
    GisDbAdoNet ;
{$ENDIF}

  function TGIS_FilePixelStoreAdoNet.sqlQueryGetCell(
    const _name : String
   ) : TStream ;
  begin
    Result := oGisDb.sqlQueryGetBlob( _name,0 ) ;
  end ;

  procedure TGIS_FilePixelStoreAdoNet.sqlTableOpenWrite(
    const _table  : String  ;
    const _filter : String
  ) ;
  begin
    oGisDb.sqlUpdateStart( 0, prepareSelectCommand( _table, _filter ) );
    oGisDb.sqlTableOpenWrite( 0, prepareUpdateCommand( _table, _filter ) );
    macroAppendParams( _table ) ;
  end ;

  procedure TGIS_FilePixelStoreAdoNet.sqlTableAppend(
    const _table : String
  ) ;
  begin
    if not oGisDb.sqlTablePrepared( 0 ) then begin
      oGisDb.sqlTableAppend( 0, prepareAppendParams( _table ) );
      macroAppendParams( _table ) ;
    end
    else begin
      oGisDb.sqlTableAppend( 0, prepareAppendParams( _table ) );
    end;
  end ;

  procedure TGIS_FilePixelStoreAdoNet.sqlTableSetCell(
    const _name   : String ;
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
      {$IFDEF MSWINDOWS}
        CopyMemory( pbuf, _blob.Memory, _blob.Size ) ;
      {$ELSE}
        Move( _blob.Memory^, pbuf^, _blob.Size ) ;
      {$ENDIF}
      VarArrayUnlock( store ) ;
    {$ENDIF}

    oGisDb.sqlTableSetGeometry( 0, _name, store, nil );

    store := Unassigned ;
  end ;

  constructor TGIS_FilePixelStoreAdoNet.Create;
  begin
    inherited ;
    
    oGisDb := TGIS_DbAdoNet.Create ;
  end ;

  constructor TGIS_FilePixelStoreAdoNet.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat     ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  begin
    { TODO : Implement config creation }

    inherited Create( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    ) ;
    oGisDb := TGIS_DbAdoNet.Create ;

    doCreateForWrite( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    );
  end;

  procedure TGIS_FilePixelStoreAdoNet.doDestroy ;
  begin
    FreeObject( oGisDb );

    inherited ;
  end ;

  function TGIS_FilePixelStoreAdoNet.Prerecognize( const _path : String
                                                ) : Boolean ;
  begin
    Result := doPrerecognize( _path ) = GIS_SQL_PROVIDER_ADONET ;
  end ;

//==================================== END =====================================
end.

