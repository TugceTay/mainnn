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
  Encapsulation of a FGDB file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileFGDB ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileFGDB"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Security,
    System.Runtime.InteropServices,

    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Variants,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoXmlDoc ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}

type

  {$IFNDEF OXYGENE}
    {#gendoc:hide}
    tgdbptr = Pointer ;
    {$IFNDEF LEVEL_XE3_RTL}
      {#gendoc:hide}
      MarshaledAString = PAnsiChar;
    {$ENDIF}
  {$ELSE}
    {#gendoc:hide}
    {$IFDEF JAVA}
      tgdbptr  = com.sun.jna.Pointer ;
    {$ELSE}
      tgdbptr = IntPtr ;
    {$ENDIF}
  {$ENDIF}

  /// <summary>
  ///   Class for reading FileGeodatabase format.
  /// </summary>
  TGIS_FileFGDB = {$IFDEF OXYGENE} public {$ENDIF}
                  class( TGIS_ObjectDisposable )
     {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FInit           : Boolean ;
      {$IFDEF OXYGENE}
        FDriverH        : tgdbptr ;
        FLayerH         : tgdbptr ;
        FTableH         : tgdbptr ;
      {$ELSE}
        FDriverH        : IntPtr ;
        FLayerH         : IntPtr ;
        FTableH         : IntPtr ;
      {$ENDIF}
      FLayerInfo      : String ;
      FLayerExtent    : TGIS_Extent ;
      FConfig         : TStringList ;
      FGDBConnection  : TObject ;
      FLayerPath      : String ;
      FDatasetPath    : String ;
      FDatasetName    : String ;
      FDatabasePath   : String ;
    private

      /// <summary>
      ///   Initialize library.
      /// </summary>
      /// <returns>
      ///    True if file was initialized.
      /// </returns>
      function  initialize : Boolean ;

      /// <summary>
      ///   Check status of read operations.
      /// </summary>
      /// <param name="_status">
      ///   status code
      /// </param>
      procedure checkR( const _status : Integer ) ;

      /// <summary>
      ///   Check status of write operations.
      /// </summary>
      /// <param name="_status">
      ///   status code
      /// </param>
      procedure checkW( const _status : Integer ) ;

      /// <summary>
      ///   Check if pointer is valid.
      /// </summary>
      /// <param name="_ptr">
      ///   pointer
      /// </param>
      /// <returns>
      ///    True if pointer is valid
      /// </returns>
      {$IFDEF OXYGENE}
      function  isValidPtr( _ptr : tgdbptr ) : Boolean ;
      {$ELSE}
      function  isValidPtr( _ptr : IntPtr ) : Boolean ;
      {$ENDIF}

      /// <summary>
      ///   Build new Feature Dataset.
      /// </summary>
      /// <param name="_path">
      ///   path to database
      /// </param>
      /// <param name="_def">
      ///   dataset definition
      /// </param>
      procedure createFeatureDataset( const _path   : String ;
                                      const _def    : String
                                    ) ;

      /// <summary>
      ///   Prepare new Feature Dataset definition.
      /// </summary>
      /// <param name="_catalogPath">
      ///   path to database
      /// </param>
      /// <param name="_datasetName">
      ///   dataset name
      /// </param>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <param name="_extent">
      ///   layer extent
      /// </param>
      /// <returns>
      ///    dataset xml definition
      /// </returns>
      function prepareFeatureDatasetDef( const _catalogPath  : String ;
                                         const _datasetName  : String ;
                                         const _cs           : TGIS_CSCoordinateSystem ;
                                         const _extent       : TGIS_Extent
                                       ) : String ;

      /// <summary>
      ///   Prepare new layer definition.
      /// </summary>
      /// <param name="_catalogPath">
      ///   path to database
      /// </param>
      /// <param name="_layerName">
      ///   dataset definition
      /// </param>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <param name="_extent">
      ///   layer extent
      /// </param>
      /// <param name="_type">
      ///   shape type
      /// </param>
      /// <param name="_dim">
      ///   dimension
      /// </param>
      /// <returns>
      ///    layer xml definition
      /// </returns>
      function prepareLayerDef      ( const _catalogPath  : String ;
                                      const _layerName    : String ;
                                      const _cs           : TGIS_CSCoordinateSystem ;
                                      const _extent       : TGIS_Extent ;
                                      const _type         : TGIS_ShapeType ;
                                      const _dim          : TGIS_DimensionType
                                     ) : String ;

      /// <summary>
      ///   Prepare new Spatial Reference definition.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <param name="_extent">
      ///   layer extent
      /// </param>
      /// <param name="_node">
      ///   parent node
      /// </param>
      procedure prepareSpatialReference( const _cs      : TGIS_CSCoordinateSystem ;
                                         const _extent  : TGIS_Extent ;
                                         const _node    : IXMLNode
                                        ) ;

      /// <summary>
      ///   Prepare new Spatial Reference definition.
      /// </summary>
      /// <param name="_path">
      ///   database path
      /// </param>
      /// <param name="_name">
      ///   layer name
      /// </param>
      procedure parseConfig        ( const _path : String ;
                                     const _name : String
                                    ) ;

      /// <summary>
      ///   Set record field value.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_value">
      ///   field value
      /// </param>
      /// <returns>
      ///    status code
      /// </returns>
      function  setRecordFieldInteger ( const _field  : String ;
                                        const _value  : Integer
                                      ) : Integer ;
      /// <summary>
      ///   Set record field value.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_value">
      ///   field value
      /// </param>
      /// <returns>
      ///    status code
      /// </returns>
      function  setRecordFieldFloat   ( const _field  : String ;
                                        const _value  : Single
                                      ) : Integer ;
      /// <summary>
      ///   Set record field value.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_value">
      ///   field value
      /// </param>
      /// <returns>
      ///    status code
      /// </returns>
      function  setRecordFieldDouble  ( const _field  : String ;
                                        const _value  : Double
                                      ) : Integer ;
      /// <summary>
      ///   Set record field value.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_value">
      ///   field value
      /// </param>
      /// <returns>
      ///    status code
      /// </returns>
      function  setRecordFieldString  ( const _field  : String ;
                                        const _value  : String
                                      ) : Integer ;
      /// <summary>
      ///   Set record field value.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_value">
      ///   field value
      /// </param>
      /// <returns>
      ///    status code
      /// </returns>
      function  setRecordFieldDateTime( const _field  : String ;
                                        const _value  : String
                                      ) : Integer ;
      /// <summary>
      ///   Set record field value.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <returns>
      ///    status code
      /// </returns>
      function  setRecordFieldNull    ( const _field  : String
                                      ) : Integer ;
      /// <summary>
      ///   Set record field value.
      /// </summary>
      /// <param name="_geom">
      ///   geometry pointer
      /// </param>
      /// <param name="_size">
      ///   geometry size
      /// </param>
      /// <returns>
      ///    status code
      /// </returns>
      function  setRecordGeometry     ( const _geom   : tgdbptr ;
                                        const _size   : Integer
                                      ) : Integer ;

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
      ///   Open a connection.
      /// </summary>
      /// <param name="_path">
      ///   path to database
      /// </param>
      /// <param name="_name">
      ///   layer name
      /// </param>
      /// <returns>
      ///    True if file was opened
      /// </returns>
      function  OpenDatabase( const _path : String ;
                              const _name : String
                            ) : Boolean ;

      /// <summary>
      ///   Open a table.
      /// </summary>
      /// <param name="_path">
      ///   path to database
      /// </param>
      /// <param name="_name">
      ///   table name
      /// </param>
      /// <returns>
      ///    True if file was opened
      /// </returns>
      function  OpenTable( const _path : String ;
                           const _name : String
                         ) : Boolean ;

      /// <summary>
      ///   Close a table.
      /// </summary>
      procedure CloseTable ;

      /// <summary>
      ///   Create new database.
      /// </summary>
      /// <param name="_path">
      ///   path to database
      /// </param>
      /// <param name="_name">
      ///   table name
      /// </param>
      /// <returns>
      ///   True if succeeded.
      /// </returns>
      function  NewDatabase( const _path : String ;
                             const _name : String
                            ) : Boolean ;

      /// <summary>
      ///   Close connection.
      /// </summary>
      procedure CloseDatabase ;

      /// <summary>
      ///   Build new structure.
      /// </summary>
      /// <param name="_path">
      ///   path to database
      /// </param>
      /// <param name="_name">
      ///   layer name
      /// </param>
      /// <param name="_extent">
      ///   layer extent
      /// </param>
      /// <param name="_type">
      ///   layer shape type
      /// </param>
      /// <param name="_dim">
      ///   layer dimension
      /// </param>
      /// <param name="_cs">
      ///   layer coordinate system
      /// </param>
      procedure Build( const _path   : String ;
                       const _name   : String ;
                       const _extent : TGIS_Extent    ;
                       const _type   : TGIS_ShapeType ;
                       const _dim    : TGIS_DimensionType ;
                       const _cs     : TGIS_CSCoordinateSystem
                      ) ;

      /// <summary>
      ///   Get main layer.
      /// </summary>
      /// <returns>
      ///   True if succeeded.
      /// </returns>
      function  GetLayer : Boolean ;

      /// <summary>
      ///   Build new layer.
      /// </summary>
      /// <param name="_defn">
      ///   layer xml definition
      /// </param>
      /// <param name="_path">
      ///   path to database
      /// </param>
      procedure CreateLayer( const _defn : String ;
                             const _path : String
                            ) ;
      /// <summary>
      ///   Delete a layer.
      /// </summary>
      /// <param name="_path">
      ///   path to database
      /// </param>
      procedure DeleteLayer( const _path : String
                            ) ;

      /// <summary>
      ///   Open a cursor.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure CursorOpen( const _cursor : Integer
                           ) ;

      /// <summary>
      ///   Close a cursor.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure CursorClose( const _cursor : Integer
                            ) ;

      /// <summary>
      ///   Check if a cursor is in EOF state.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   True if eof reached.
      /// </returns>
      function  CursorEof( const _cursor : Integer
                          ) : Boolean ;

      /// <summary>
      ///   Move to the first shape in the cursor.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor identifier (see TGIS_LayerVector.cursorOpen)
      /// </param>
      /// <param name="_xmin">
      ///   extent of item to be found
      /// </param>
      /// <param name="_xmax">
      ///   extent of item to be found
      /// </param>
      /// <param name="_ymin">
      ///   extent of item to be found
      /// </param>
      /// <param name="_ymax">
      ///   extent of item to be found
      /// </param>
      /// <param name="_query">
      ///   query which must be matched by item; closely mimics SQL WHERE
      ///   clause; for examples you can use 'AGE &gt;= 18'; empty (default)
      ///   means that no all items will match.
      /// </param>
      procedure CursorFirst( const _cursor : Integer ;
                             const _xmin   : Double ;
                             const _xmax   : Double ;
                             const _ymin   : Double ;
                             const _ymax   : Double ;
                             const _query  : String
                            ) ;

      /// <summary>
      ///   Move to the next shape in the cursor.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure CursorNext( const _cursor : Integer ) ;

      /// <summary>
      ///   Get current shape of the cursor
      /// </summary>
      /// <param name="_cursor">
      ///   cursor identifier
      /// </param>
      /// <param name="_size">
      ///   buffer size
      /// </param>
      /// <returns>
      ///   Pointer to shape data
      /// </returns>
      {$IFDEF OXYGENE}
      function  CursorShape( const _cursor : Integer ;
                               var _size : Integer
                            ) : tgdbptr ;
      {$ELSE}
      function  CursorShape( const _cursor : Integer ;
                               var _size : Integer
                            ) : IntPtr ;
      {$ENDIF}

      /// <summary>
      ///   Get a row OID value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  GetOID( const _cursor : Integer
                       ) : Integer ;

      /// <summary>
      ///   Get a row integer value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  GetInteger( const _cursor : Integer ;
                            const _field  : String
                           ) : Integer ;

      /// <summary>
      ///   Get a row double value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  GetDouble( const _cursor : Integer ;
                           const _field  : String
                          ) : Double ;

      /// <summary>
      ///   Get a row string value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_size">
      ///   field size
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  GetString( const _cursor : Integer ;
                           const _field  : String  ;
                           const _size   : Integer
                          ) : String ;

      /// <summary>
      ///   Get a row date time value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  GetDateTime( const _cursor : Integer ;
                             const _field  : String
                            ) : String ;

      /// <summary>
      ///   Get a row null value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  GetIsNull  ( const _cursor : Integer ;
                             const _field  : String
                            ) : Boolean ;

      /// <summary>
      ///   Get a row float value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  GetFloat( const _cursor : Integer ;
                          const _field  : String
                         ) : Single ;

      /// <summary>
      ///   Delete a field from table.
      /// </summary>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_field">
      ///   field name
      /// </param>
      procedure DeleteField( const _table  : String ;
                             const _field  : String
                           ) ;

      /// <summary>
      ///   Delete a field from table.
      /// </summary>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_fieldType">
      ///   field type
      /// </param>
      /// <param name="_fieldWidth">
      ///   field width
      /// </param>
      /// <param name="_fieldDecimal">
      ///   field decimal
      /// </param>
      procedure AddField   ( const _table        : String ;
                             const _field        : String ;
                             const _fieldType    : TGIS_FieldType ;
                             const _fieldWidth   : Integer ;
                             const _fieldDecimal : Integer
                           ) ;

      /// <summary>
      ///   Create new record.
      /// </summary>
      procedure CreateRecord ;

      /// <summary>
      ///   Insert record into table.
      /// </summary>
      procedure InsertRecord ;

      /// <summary>
      ///   Get last inserted record OID.
      /// </summary>
      /// <returns>
      ///   OID value
      /// </returns>
      function GetLastRecordOID : TGIS_Uid ;

      /// <summary>
      ///   Get record from table.
      /// </summary>
      /// <param name="_query">
      ///   filter query
      /// </param>
      /// <returns>
      ///   True if any record was found
      /// </returns>
      function GetRecord         ( const _query  : String
                                  ) : Boolean ;
      /// <summary>
      ///   Update record in table.
      /// </summary>
      procedure UpdateRecord ;

      /// <summary>
      ///   Delete record in table.
      /// </summary>
      procedure DeleteRecord ;

      /// <summary>
      ///   Set record field.
      /// </summary>
      /// <param name="_name">
      ///   field name
      /// </param>
      /// <param name="_type">
      ///   field type
      /// </param>
      /// <param name="_size">
      ///   field size
      /// </param>
      /// <param name="_scale">
      ///   field scale
      /// </param>
      /// <param name="_val">
      ///   field value
      /// </param>
      procedure RecordSetField    ( const _name   : String  ;
                                    const _type   : TGIS_FieldType ;
                                    const _size   : Integer ;
                                    const _scale  : Integer ;
                                    const _val    : Variant
                                  ) ;
      /// <summary>
      ///   Set record geometry.
      /// </summary>
      /// <param name="_geom">
      ///   geometry data
      /// </param>
      procedure RecordSetGeometry ( const _geom   : Variant
                                  ) ;

      /// <summary>
      ///   Start bulk transaction (faster import).
      /// </summary>
      procedure StartBulkTransaction ;

      /// <summary>
      ///   End bulk transaction.
      /// </summary>
      procedure EndBulkTransaction ;

      /// <summary>
      ///   Get available layers with their types from database.
      /// </summary>
      /// <remarks>
      ///    List Objects property keeps layer type as TGIS_ShapeType value
      /// </remarks>
      /// <param name="_path">
      ///   path to database
      /// </param>
      /// <returns>
      ///   list of layers info
      /// </returns>
      function  GetAvailableLayers( const _path : String
                                  ) : TGIS_LayerInfoList ;
    public
      {$IFDEF OXYGENE}
        /// <summary>
        ///   Layer handle.
        /// </summary>
        property LayerH      : tgdbptr       read FLayerH ;
        /// <summary>
        ///   Table handle.
        /// </summary>
        property TableH      : tgdbptr       read FTableH ;
      {$ELSE}
        /// <summary>
        ///   Layer handle.
        /// </summary>
        property LayerH      : IntPtr       read FLayerH ;
        /// <summary>
        ///   Table handle.
        /// </summary>
        property TableH      : IntPtr       read FTableH ;
      {$ENDIF}
      /// <summary>
      ///   Layer information.
      /// </summary>
      property LayerInfo   : String       read FLayerInfo ;
      /// <summary>
      ///   Layer extent.
      /// </summary>
      property LayerExtent : TGIS_Extent  read FLayerExtent ;
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

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoSharedConnections ;
{$ENDIF}

const
  FGDB_DLL_NAME       = 'ttkFileGDB.dll' ;
  FGDB_DLL_NAME_NOEXT = 'ttkFileGDB' ;
  FGDB_DLL_VERSION    = 1.3 ;

  FGDB_METADATA_HIGH_PRECISION = 'TGIS_LayerFGDB.HighPrecision' ;
  FGDB_METADATA_XY_SCALE       = 'TGIS_LayerFGDB.XYScale' ;
  FGDB_METADATA_XY_TOLERANCE   = 'TGIS_LayerFGDB.XYTolerance' ;

var
  FLib            : TObject ;
  FInstanceCount  : Integer = 0 ;

//==============================================================================
// helper functions
//==============================================================================

 {$IFDEF OXYGENE}
   // Convert string buffer to string
   {$IFDEF CLR}
   function asFGDBString( const _buf : IntPtr ) : String ;
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
     function asFGDBString( const _buf : String ) : String ;
     begin
       Result := _buf ;
     end ;

     function asFGDBString( const _buf : tgdbptr ) : String ;
     begin
       Result := _buf.getString(0) ;
     end ;
   {$ENDIF}
 {$ELSE}
   // Convert string buffer to string
   function asFGDBString( const _buf : PAnsiChar ) : String ;
   begin
     Result := String( UTF8String( PAnsiChar( _buf ) ) ) ;
   end ;
 {$ENDIF}

type

//==============================================================================
// FileGDB API wrappers
//==============================================================================

  {$IFDEF JAVA}
    IFGDBLibrary = interface (com.sun.jna.Library)
        function FGDB_initialize : tgdbptr ;
        procedure FGDB_finalize( _instance : tgdbptr )  ;
        function FGDB_version( _instance : tgdbptr ) : Double ;
        function FGDB_errmsg( _instance : tgdbptr ) : tgdbptr ;
        function FGDB_connect( _instance : tgdbptr ;
                               _params   : String
                              ) : Integer ;
        function FGDB_new( _instance : tgdbptr ;
                           _params   : String
                          ) : Integer ;
        procedure FGDB_disconnect( _instance : tgdbptr ) ;
        function FGDB_getlayer( _instance : tgdbptr ;
                                _index    : Integer
                               ) : tgdbptr ;
        procedure FGDB_getalllayers( _instance : tgdbptr ) ;
        function FGDB_getlayerbyname( _instance : tgdbptr ;
                                      _name     : String
                                     ) : tgdbptr ;
        function FGDB_getlayerscount( _instance : tgdbptr ) : Integer ;
        function FGDB_layer_info( _layerh : tgdbptr ) : tgdbptr ;
        procedure FGDB_layer_extent( _layerh  : tgdbptr ;
                                     _xmin : com.sun.jna.ptr.DoubleByReference ;
                                     _xmax : com.sun.jna.ptr.DoubleByReference ;
                                     _ymin : com.sun.jna.ptr.DoubleByReference ;
                                     _ymax : com.sun.jna.ptr.DoubleByReference
                                  )  ;
        function FGDB_createlayer     ( _instance : tgdbptr ;
                                        _defn     : String ;
                                        _path     : String
                                      ) : Integer ;
        function FGDB_deletelayer     ( _instance : tgdbptr ;
                                        _path     : String
                                      ) : Integer ;
        function FGDB_createdataset   ( _instance : tgdbptr ;
                                        _defn     : String ;
                                        _path     : String
                                      ) : Integer ;
        function FGDB_gettable        ( _instance : tgdbptr ;
                                        _name     : String
                                      ) : tgdbptr ;
        function FGDB_findspatialref  ( _instance : tgdbptr ;
                                        _srid     : Integer
                                      ) : tgdbptr ;
        function FGDB_table_createfield   ( _tableh : tgdbptr ;
                                            _defn   : String
                                          ) : Integer ;
        function FGDB_table_deletefield   ( _tableh : tgdbptr ;
                                            _field  : String
                                          ) : Integer ;
        function FGDB_table_createrow   ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_insertrow   ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_getlastrowoid   ( _tableh : tgdbptr ;
                                               _oid : com.sun.jna.ptr.IntByReference
                                           ) : Integer ;
        function FGDB_table_getrow      ( _tableh : tgdbptr ;
                                          _query  : String
                                          ) : Integer ;
        function FGDB_table_updaterow    ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_deleterow    ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_starttransaction    ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_endtransaction    ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_close   ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_row_setinteger    ( _tableh  : tgdbptr ;
                                          _field   : String ;
                                          _value   : Integer
                                        ) : Integer ;
        function FGDB_row_setfloat    ( _tableh  : tgdbptr ;
                                        _field   : String ;
                                        _value   : Single
                                      ) : Integer ;
        function FGDB_row_setdouble   ( _tableh  : tgdbptr ;
                                        _field   : String ;
                                        _value   : Double
                                      ) : Integer ;
        function FGDB_row_setstring   ( _tableh  : tgdbptr ;
                                        _field   : String ;
                                        _value   : String
                                      ) : Integer ;
        function FGDB_row_setdatetime   ( _tableh  : tgdbptr ;
                                          _field   : String ;
                                          _value   : String
                                        ) : Integer ;
        function FGDB_row_setnull   ( _tableh  : tgdbptr ;
                                      _field   : String
                                    ) : Integer ;
        function FGDB_row_setgeometry   ( _tableh  : tgdbptr ;
                                          _value   : tgdbptr ;
                                          _size    : Integer
                                        ) : Integer ;
        procedure FGDB_cursor_open( _layerh   : tgdbptr ;
                                    _cursor   : Integer
                                  )  ;
        procedure FGDB_cursor_close( _layerh   : tgdbptr ;
                                     _cursor   : Integer
                                   )  ;
        procedure FGDB_cursor_first( _layerh : tgdbptr ;
                                     _cursor : Integer ;
                                     _xmin   : Double ;
                                     _xmax   : Double ;
                                     _ymin   : Double ;
                                     _ymax   : Double ;
                                     _query  : String
                                  )  ;
        procedure FGDB_cursor_next( _layerh   : tgdbptr ;
                                    _cursor   : Integer
                                  )  ;
        function FGDB_cursor_eof( _layerh   : tgdbptr ;
                                  _cursor   : Integer
                                 ) : Integer ;
        function FGDB_cursor_shape( _layerh   : tgdbptr ;
                                    _cursor   : Integer ;
                                     _size : com.sun.jna.ptr.IntByReference
                                  ) : tgdbptr ;
        function FGDB_row_getoid( _layerh   : tgdbptr ;
                                  _cursor   : Integer
                                 ) : Integer ;
        function FGDB_row_getinteger( _layerh  : tgdbptr ;
                                      _cursor  : Integer ;
                                      _field   : String
                                    ) : Integer ;
        function FGDB_row_getfloat( _layerh  : tgdbptr ;
                                    _cursor  : Integer ;
                                    _field   : String
                                  ) : Single ;
        function FGDB_row_getdouble( _layerh  : tgdbptr ;
                                     _cursor  : Integer ;
                                     _field   : String
                                    ) : Double ;
        function FGDB_row_getstring( _layerh  : tgdbptr ;
                                     _cursor  : Integer ;
                                     _field   : String  ;
                                     _size    : Integer
                                    ) : tgdbptr ;
        function FGDB_row_getdatetime( _layerh  : tgdbptr ;
                                       _cursor  : Integer ;
                                       _field   : String
                                     ) : tgdbptr ;
        function FGDB_row_getnull    ( _layerh  : tgdbptr ;
                                       _cursor  : Integer ;
                                       _field   : String
                                     ) : Integer ;
    end;
  {$ENDIF}

  // ttkFileGDB library helper.
  T_FGDBLib = {$IFDEF OXYGENE} static {$ENDIF}class
    private
      {$IFDEF JAVA}
        DLLHandle : IFGDBLibrary ;
      {$ELSE}
        DLLHandle : THandle ;
      {$ENDIF}
      DLLLoaded : Boolean ;
    public
      {$IFDEF CLR}
        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_initialize : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure FGDB_finalize( _instance : IntPtr )  ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_version( _instance : IntPtr ) : Double ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_errmsg( _instance : IntPtr ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_connect( _instance : IntPtr ;
                               _params   : TBytes
                              ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_new( _instance : IntPtr ;
                           _params   : TBytes
                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure FGDB_disconnect( _instance : IntPtr ) ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_getlayer( _instance : IntPtr ;
                                _index    : Integer
                               ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure FGDB_getalllayers( _instance : IntPtr ) ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_getlayerbyname( _instance : IntPtr ;
                                      _name     : TBytes
                                     ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_getlayerscount( _instance : IntPtr ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_layer_info( _layerh : IntPtr ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure FGDB_layer_extent( _layerh  : IntPtr ;
                                    var _xmin : Double ;
                                    var _xmax : Double ;
                                    var _ymin : Double ;
                                    var _ymax : Double
                                  )  ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_createlayer     ( _instance : IntPtr ;
                                        _defn     : TBytes ;
                                        _path     : TBytes
                                      ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_deletelayer     ( _instance : IntPtr ;
                                        _path     : TBytes
                                      ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_createdataset   ( _instance : IntPtr ;
                                        _defn     : TBytes ;
                                        _path     : TBytes
                                      ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_gettable        ( _instance : IntPtr ;
                                        _name     : TBytes
                                      ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_findspatialref  ( _instance : IntPtr ;
                                        _srid     : Integer
                                      ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_createfield   ( _tableh : IntPtr ;
                                            _defn   : TBytes
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_deletefield   ( _tableh : IntPtr ;
                                            _field  : TBytes
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_createrow   ( _tableh : IntPtr
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_insertrow   ( _tableh : IntPtr
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_getlastrowoid   ( _tableh : IntPtr ;
                                              var _oid : Integer
                                           ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_getrow      ( _tableh : IntPtr ;
                                          _query  : TBytes
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_updaterow    ( _tableh : IntPtr
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_deleterow    ( _tableh : IntPtr
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_starttransaction    ( _tableh : IntPtr
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_endtransaction    ( _tableh : IntPtr
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_table_close   ( _tableh : IntPtr
                                          ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_setinteger    ( _tableh  : IntPtr ;
                                          _field   : TBytes ;
                                          _value   : Integer
                                        ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_setfloat    ( _tableh  : IntPtr ;
                                        _field   : TBytes ;
                                        _value   : Single
                                      ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_setdouble   ( _tableh  : IntPtr ;
                                        _field   : TBytes ;
                                        _value   : Double
                                      ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_setstring   ( _tableh  : IntPtr ;
                                        _field   : TBytes ;
                                        _value   : TBytes
                                      ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_setdatetime   ( _tableh  : IntPtr ;
                                          _field   : TBytes ;
                                          _value   : TBytes
                                        ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_setnull   ( _tableh  : IntPtr ;
                                      _field   : TBytes
                                    ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_setgeometry   ( _tableh  : IntPtr ;
                                          _value   : IntPtr ;
                                          _size    : Integer
                                        ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure FGDB_cursor_open( _layerh   : IntPtr ;
                                    _cursor   : Integer
                                  )  ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure FGDB_cursor_close( _layerh   : IntPtr ;
                                     _cursor   : Integer
                                   )  ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure FGDB_cursor_first( _layerh : IntPtr ;
                                     _cursor : Integer ;
                                     _xmin   : Double ;
                                     _xmax   : Double ;
                                     _ymin   : Double ;
                                     _ymax   : Double ;
                                     _query  : TBytes
                                  )  ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        procedure FGDB_cursor_next( _layerh   : IntPtr ;
                                    _cursor   : Integer
                                  )  ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_cursor_eof( _layerh   : IntPtr ;
                                  _cursor   : Integer
                                 ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_cursor_shape( _layerh   : IntPtr ;
                                    _cursor   : Integer ;
                                    var _size : Integer
                                  ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_getoid( _layerh   : IntPtr ;
                                  _cursor   : Integer
                                 ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_getinteger( _layerh  : IntPtr ;
                                      _cursor  : Integer ;
                                      _field   : TBytes
                                    ) : Integer ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_getfloat( _layerh  : IntPtr ;
                                    _cursor  : Integer ;
                                    _field   : TBytes
                                  ) : Single ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_getdouble( _layerh  : IntPtr ;
                                     _cursor  : Integer ;
                                     _field   : TBytes
                                    ) : Double ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_getstring( _layerh  : IntPtr ;
                                     _cursor  : Integer ;
                                     _field   : TBytes ;
                                     _size    : Integer
                                    ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_getdatetime( _layerh  : IntPtr ;
                                       _cursor  : Integer ;
                                       _field   : TBytes
                                     ) : IntPtr ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( FGDB_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function FGDB_row_getnull    ( _layerh  : IntPtr ;
                                       _cursor  : Integer ;
                                       _field   : TBytes
                                     ) : Integer ; external ;
      {$ENDIF}
      {$IFDEF DCC}
        FGDB_initialize      : function : IntPtr ; cdecl;

        FGDB_finalize        : procedure( _instance : IntPtr
                                         ) ; cdecl;

        FGDB_version         : function( _instance : IntPtr
                                        ) : Double ; cdecl;

        FGDB_errmsg          : function( _instance : IntPtr
                                        ) : PAnsiChar ; cdecl;

        FGDB_connect         : function( _instance : IntPtr ;
                                         _params   : tgdbptr
                                        ) : Integer ; cdecl;

        FGDB_new             : function( _instance : IntPtr ;
                                         _params   : PAnsiChar
                                        ) : Integer ; cdecl;

        FGDB_disconnect      : procedure( _instance : IntPtr
                                         )  ; cdecl;

        FGDB_getlayer        : function( _instance : IntPtr ;
                                         _index   : Integer
                                        ) : IntPtr ; cdecl;

        FGDB_getalllayers    : procedure( _instance : IntPtr
                                        ) ; cdecl;

        FGDB_getlayerbyname  : function( _instance : IntPtr ;
                                         _name     : PAnsiChar
                                        ) : IntPtr ; cdecl;

        FGDB_getlayerscount  : function( _instance : IntPtr
                                        ) : Integer ; cdecl;

        FGDB_layer_extent    : procedure( _layerh   : IntPtr ;
                                          var _xmin : Double ;
                                          var _xmax : Double ;
                                          var _ymin : Double ;
                                          var _ymax : Double
                                        ) ; cdecl;

        FGDB_createlayer     : function( _instance : IntPtr ;
                                         _defn     : PAnsiChar ;
                                         _path     : PAnsiChar
                                        ) : Integer ; cdecl;

        FGDB_deletelayer     : function( _instance : IntPtr ;
                                         _path     : PAnsiChar
                                        ) : Integer ; cdecl;

        FGDB_createdataset   : function( _instance : IntPtr ;
                                         _defn     : PAnsiChar ;
                                         _path     : PAnsiChar
                                        ) : Integer ; cdecl;

        FGDB_gettable        : function( _instance : IntPtr ;
                                         _name     : PAnsiChar
                                        ) : IntPtr ; cdecl;

        FGDB_findspatialref  : function( _instance : IntPtr ;
                                         _srid     : Integer
                                        ) : PAnsiChar ; cdecl;

        FGDB_table_createfield   : function( _tableh : IntPtr ;
                                             _defn   : PAnsiChar
                                            ) : Integer ; cdecl;

        FGDB_table_deletefield   : function( _tableh : IntPtr ;
                                             _field  : PAnsiChar
                                            ) : Integer ; cdecl;

        FGDB_table_createrow   : function( _tableh : IntPtr
                                            ) : Integer ; cdecl;

        FGDB_table_insertrow   : function( _tableh : IntPtr
                                          ) : Integer ; cdecl;

        FGDB_table_getlastrowoid   : function( _tableh : IntPtr ;
                                            var _oid : Integer
                                            ) : Integer ; cdecl;

        FGDB_table_getrow      : function( _tableh : IntPtr ;
                                           _query  : PAnsiChar
                                            ) : Integer ; cdecl;

        FGDB_table_updaterow   : function( _tableh : IntPtr
                                            ) : Integer ; cdecl;

        FGDB_table_deleterow   : function( _tableh : IntPtr
                                            ) : Integer ; cdecl;

        FGDB_table_starttransaction   : function( _tableh : IntPtr
                                            ) : Integer ; cdecl;

        FGDB_table_endtransaction   : function( _tableh : IntPtr
                                            ) : Integer ; cdecl;

        FGDB_table_close         : function( _tableh : IntPtr
                                            ) : Integer ; cdecl;

        FGDB_row_setinteger   : function( _tableh  : IntPtr ;
                                          _field   : PAnsiChar ;
                                          _value   : Integer
                                        ) : IntPtr ; cdecl;

        FGDB_row_setfloat   : function( _tableh  : IntPtr ;
                                          _field   : PAnsiChar ;
                                          _value   : Single
                                        ) : IntPtr ; cdecl;

        FGDB_row_setdouble   : function( _tableh  : IntPtr ;
                                          _field   : PAnsiChar ;
                                          _value   : Double
                                        ) : IntPtr ; cdecl;

        FGDB_row_setstring   : function( _tableh  : IntPtr ;
                                          _field   : PAnsiChar ;
                                          _value   : tgdbptr
                                        ) : IntPtr ; cdecl;

        FGDB_row_setdatetime   : function( _tableh  : IntPtr ;
                                          _field   : PAnsiChar ;
                                          _value   : tgdbptr
                                        ) : IntPtr ; cdecl;

        FGDB_row_setnull   : function( _tableh  : IntPtr ;
                                          _field   : PAnsiChar
                                        ) : IntPtr ; cdecl;

        FGDB_row_setgeometry   : function( _tableh  : IntPtr ;
                                          _value   : tgdbptr ;
                                          _size    : Integer
                                        ) : IntPtr ; cdecl;

        FGDB_layer_info      : function( _layerh : IntPtr
                                        ) : PAnsiChar ; cdecl;

        FGDB_cursor_open     : procedure( _layerh : IntPtr ;
                                          _cursor : Integer
                                         ) ; cdecl;

        FGDB_cursor_close    : procedure( _layerh : IntPtr ;
                                          _cursor : Integer
                                         ) ; cdecl;

        FGDB_cursor_first    : procedure( _layerh : IntPtr ;
                                          _cursor : Integer ;
                                          _xmin   : Double ;
                                          _xmax   : Double ;
                                          _ymin   : Double ;
                                          _ymax   : Double ;
                                          _query  : PAnsiChar
                                         ) ; cdecl;

        FGDB_cursor_next     : procedure( _layerh  : IntPtr ;
                                          _cursor  : Integer
                                         ) ; cdecl;

        FGDB_cursor_eof      : function( _layerh   : IntPtr ;
                                         _cursor   : Integer
                                        ) : Integer ; cdecl;

        FGDB_cursor_shape    : function( _layerh   : IntPtr ;
                                         _cursor   : Integer  ;
                                         var _size : Integer
                                        ) : IntPtr ; cdecl;

        FGDB_row_getoid      : function( _layerh : IntPtr ;
                                         _cursor : Integer
                                        ) : Integer ; cdecl;

        FGDB_row_getinteger  : function( _layerh  : IntPtr ;
                                         _cursor  : Integer ;
                                         _field   : PAnsiChar
                                       ) : Integer ; cdecl;

        FGDB_row_getfloat    : function( _layerh  : IntPtr ;
                                         _cursor  : Integer  ;
                                         _field   : PAnsiChar
                                        ) : Single ; cdecl;

        FGDB_row_getdouble   : function( _layerh  : IntPtr ;
                                         _cursor  : Integer  ;
                                         _field   : PAnsiChar
                                        ) : Double ; cdecl;

        FGDB_row_getstring   : function( _layerh  : IntPtr ;
                                         _cursor  : Integer  ;
                                         _field   : PAnsiChar ;
                                         _size    : Integer
                                        ) : PAnsiChar ; cdecl;

        FGDB_row_getdatetime : function( _layerh  : IntPtr ;
                                         _cursor  : Integer  ;
                                         _field   : PAnsiChar
                                        ) : PAnsiChar ; cdecl;

        FGDB_row_getnull     : function( _layerh  : IntPtr ;
                                         _cursor  : Integer ;
                                         _field   : PAnsiChar
                                       ) : Integer ; cdecl;
      {$ENDIF}
      {$IFDEF JAVA}
        function FGDB_initialize : tgdbptr ;
        procedure FGDB_finalize( _instance : tgdbptr )  ;
        function FGDB_version( _instance : tgdbptr ) : Double ;
        function FGDB_errmsg( _instance : tgdbptr ) : tgdbptr ;
        function FGDB_connect( _instance : tgdbptr ;
                               _params   : TBytes
                              ) : Integer ;
        function FGDB_new( _instance : tgdbptr ;
                           _params   : TBytes
                          ) : Integer ;
        procedure FGDB_disconnect( _instance : tgdbptr ) ;
        function FGDB_getlayer( _instance : tgdbptr ;
                                _index    : Integer
                               ) : tgdbptr ;
        procedure FGDB_getalllayers( _instance : tgdbptr ) ;
        function FGDB_getlayerbyname( _instance : tgdbptr ;
                                      _name     : TBytes
                                     ) : tgdbptr ;
        function FGDB_getlayerscount( _instance : tgdbptr ) : Integer ;
        function FGDB_layer_info( _layerh : tgdbptr ) : tgdbptr ;
        procedure FGDB_layer_extent( _layerh  : tgdbptr ;
                                    var _xmin : Double ;
                                    var _xmax : Double ;
                                    var _ymin : Double ;
                                    var _ymax : Double
                                  )  ;
        function FGDB_createlayer     ( _instance : tgdbptr ;
                                        _defn     : TBytes ;
                                        _path     : TBytes
                                      ) : Integer ;
        function FGDB_deletelayer     ( _instance : tgdbptr ;
                                        _path     : TBytes
                                      ) : Integer ;
        function FGDB_createdataset   ( _instance : tgdbptr ;
                                        _defn     : TBytes ;
                                        _path     : TBytes
                                      ) : Integer ;
        function FGDB_gettable        ( _instance : tgdbptr ;
                                        _name     : TBytes
                                      ) : tgdbptr ;
        function FGDB_findspatialref  ( _instance : tgdbptr ;
                                        _srid     : Integer
                                      ) : tgdbptr ;
        function FGDB_table_createfield   ( _tableh : tgdbptr ;
                                            _defn   : TBytes
                                          ) : Integer ;
        function FGDB_table_deletefield   ( _tableh : tgdbptr ;
                                            _field  : TBytes
                                          ) : Integer ;
        function FGDB_table_createrow   ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_insertrow   ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_getlastrowoid   ( _tableh : tgdbptr ;
                                              var _oid : Integer
                                           ) : Integer ;
        function FGDB_table_getrow      ( _tableh : tgdbptr ;
                                          _query  : TBytes
                                          ) : Integer ;
        function FGDB_table_updaterow    ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_deleterow    ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_starttransaction    ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_endtransaction    ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_table_close   ( _tableh : tgdbptr
                                          ) : Integer ;
        function FGDB_row_setinteger    ( _tableh  : tgdbptr ;
                                          _field   : TBytes ;
                                          _value   : Integer
                                        ) : Integer ;
        function FGDB_row_setfloat    ( _tableh  : tgdbptr ;
                                        _field   : TBytes ;
                                        _value   : Single
                                      ) : Integer ;
        function FGDB_row_setdouble   ( _tableh  : tgdbptr ;
                                        _field   : TBytes ;
                                        _value   : Double
                                      ) : Integer ;
        function FGDB_row_setstring   ( _tableh  : tgdbptr ;
                                        _field   : TBytes ;
                                        _value   : TBytes
                                      ) : Integer ;
        function FGDB_row_setdatetime   ( _tableh  : tgdbptr ;
                                          _field   : TBytes ;
                                          _value   : TBytes
                                        ) : Integer ;
        function FGDB_row_setnull   ( _tableh  : tgdbptr ;
                                      _field   : TBytes
                                    ) : Integer ;
        function FGDB_row_setgeometry   ( _tableh  : tgdbptr ;
                                          _value   : tgdbptr ;
                                          _size    : Integer
                                        ) : Integer ;
        procedure FGDB_cursor_open( _layerh   : tgdbptr ;
                                    _cursor   : Integer
                                  )  ;
        procedure FGDB_cursor_close( _layerh   : tgdbptr ;
                                     _cursor   : Integer
                                   )  ;
        procedure FGDB_cursor_first( _layerh : tgdbptr ;
                                     _cursor : Integer ;
                                     _xmin   : Double ;
                                     _xmax   : Double ;
                                     _ymin   : Double ;
                                     _ymax   : Double ;
                                     _query  : TBytes
                                  )  ;
        procedure FGDB_cursor_next( _layerh   : tgdbptr ;
                                    _cursor   : Integer
                                  )  ;
        function FGDB_cursor_eof( _layerh   : tgdbptr ;
                                  _cursor   : Integer
                                 ) : Integer ;
        function FGDB_cursor_shape( _layerh   : tgdbptr ;
                                    _cursor   : Integer ;
                                    var _size : Integer
                                  ) : tgdbptr ;
        function FGDB_row_getoid( _layerh   : tgdbptr ;
                                  _cursor   : Integer
                                 ) : Integer ;
        function FGDB_row_getinteger( _layerh  : tgdbptr ;
                                      _cursor  : Integer ;
                                      _field   : TBytes
                                    ) : Integer ;
        function FGDB_row_getfloat( _layerh  : tgdbptr ;
                                    _cursor  : Integer ;
                                    _field   : TBytes
                                  ) : Single ;
        function FGDB_row_getdouble( _layerh  : tgdbptr ;
                                     _cursor  : Integer ;
                                     _field   : TBytes
                                    ) : Double ;
        function FGDB_row_getstring( _layerh  : tgdbptr ;
                                     _cursor  : Integer ;
                                     _field   : TBytes  ;
                                     _size    : Integer
                                    ) : tgdbptr ;
        function FGDB_row_getdatetime( _layerh  : tgdbptr ;
                                       _cursor  : Integer ;
                                       _field   : TBytes
                                     ) : tgdbptr ;
        function FGDB_row_getnull    ( _layerh  : tgdbptr ;
                                       _cursor  : Integer ;
                                       _field   : TBytes
                                     ) : Integer ;
      {$ENDIF}
    public

      constructor Create ;

      {$IFNDEF OXYGENE}
        destructor Destroy ; override;
      {$ENDIF}

      // Load a dll.
      // _dllPath  path to a library
      function  LoadDLL     ( const _dllPath : String
                            ) : Boolean ;
  end ;

//=============================================================================
// TFGDBConnection
//=============================================================================

  // TFGDBConnection class.
  TFGDBConnection = class( TGIS_AnyConnection )
    private
      {$IFDEF OXYGENE}
        FDB   : tgdbptr  ;
      {$ELSE}
        FDB   : IntPtr ;
      {$ENDIF}
    private
      // Check status.
      // _status  status code
      procedure check( const _status : Integer ) ;

    protected

      procedure doDestroy ; override;
    public

      constructor Create ;  override;

      // Open connection.
      procedure Open ; override;

      // Close connection.
      procedure Close ; override;
    public
      {$IFDEF OXYGENE}
        property DB : tgdbptr read FDB ;
      {$ELSE}
        property DB : IntPtr read FDB ;
      {$ENDIF}
  end ;

  constructor TFGDBConnection.Create ;
  begin
    inherited ;

    {$IFDEF LEVEL_XE2_RTL}
      FDB := IntPtr(0) ;
    {$ELSE}
      FDB := nil ;
    {$ENDIF}
  end ;

  procedure TFGDBConnection.doDestroy ;
  begin
    Close ;
    {$IFDEF LEVEL_XE2_RTL}
      FDB := IntPtr(0) ;
    {$ELSE}
      FDB := nil ;
    {$ENDIF}

    inherited ;
  end ;

  procedure TFGDBConnection.check(
    const _status : Integer
  ) ;
  var
    err : String ;
  begin
    if _status <> 0 then begin
      err := asFGDBString( T_FGDBLib(FLib).FGDB_errmsg( FDB ) ) ;

      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ) + #13#10 +
                                    Format( '%s', [err] ),
                                    '', 0
                                  ) ;
    end ;
  end ;

  procedure TFGDBConnection.Open ;
  var
    ver     : Double ;
    spath   : String ;
    {$IFDEF OXYGENE}
      cmd   : TBytes ;
    {$ELSE}
      cmd   : tgdbptr ;
    {$ENDIF}
  begin
    FDB := T_FGDBLib(FLib).FGDB_initialize ;

    ver := T_FGDBLib(FLib).FGDB_version( FDB ) ;
    if DotFloatToStr(ver) <> DotFloatToStr(FGDB_DLL_VERSION) then
      raise EGIS_Exception.Create( GIS_RS_ERR_FILEMAPPING, FGDB_DLL_NAME, 30 ) ;

    spath := GetPathAbsolute( GetFileDir( Params.Values[ GIS_INI_CONFIG ] ),
                                Server
                              ) ;

    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode( spath ) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( spath ) ;
    {$ENDIF}

    check( T_FGDBLib(FLib).FGDB_connect(
              FDB,
              cmd
           )
         ) ;
  end ;

  procedure TFGDBConnection.Close ;
  begin
    {$IFNDEF JAVA}
      if FDB <> IntPtr(0)   then begin
    {$ELSE}
      if assigned( FDB ) then begin
    {$ENDIF}
        try
          T_FGDBLib(FLib).FGDB_disconnect( FDB ) ;
          T_FGDBLib(FLib).FGDB_finalize( FDB ) ;
        except
          // may return strange errors
        end ;

        {$IFDEF LEVEL_XE2_RTL}
          FDB := IntPtr(0) ;
        {$ELSE}
          FDB := nil ;
        {$ENDIF}
    end ;
  end ;

//=============================================================================
// TGIS_FileFGDB
//=============================================================================

  constructor TGIS_FileFGDB.Create ;
  begin
    inc( FInstanceCount ) ;

    inherited ;

    FConfig := TStringList.Create ;
    FGDBConnection := nil ;

    {$IFNDEF JAVA}
      FLayerH := IntPtr(0) ;
    {$ELSE}
      FLayerH := nil ;
    {$ENDIF}
    {$IFNDEF JAVA}
      FTableH := IntPtr(0) ;
    {$ELSE}
      FTableH := nil ;
    {$ENDIF}
    {$IFNDEF JAVA}
      FDriverH := IntPtr(0) ;
    {$ELSE}
      FDriverH := nil ;
    {$ENDIF}

    FLayerPath    := '' ;
    FDatasetPath  := '' ;
    FDatabasePath := '' ;
    FDatasetName  := '' ;
  end ;

  procedure TGIS_FileFGDB.doDestroy ;
  begin
    dec( FInstanceCount ) ;

    FreeObject( FConfig ) ;
    if FInstanceCount <= 0 then
      FreeObject( FLib ) ;

    inherited ;
  end ;

  function TGIS_FileFGDB.initialize : Boolean ;
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
      if not assigned( FLib ) then
        FLib := T_FGDBLib.Create ;
    {$ENDIF}

    {$IFDEF JAVA}
      sdll := FGDB_DLL_NAME_NOEXT ;
    {$ELSE}
      sdll := FGDB_DLL_NAME ;
    {$ENDIF}

    if not T_FGDBLib(FLib).LoadDLL( sdll ) then begin
      exit ;
    end
    else begin
      Result := True ;
      FInit := True ;
    end ;
  end ;

  procedure TGIS_FileFGDB.checkR(
    const _status : Integer
  ) ;
  var
    err : String ;
  begin
    if _status <> 0 then begin
      err := asFGDBString( T_FGDBLib(FLib).FGDB_errmsg( FDriverH ) ) ;

      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ) + #13#10 +
                                    Format( '%s', [err] ),
                                    FDatabasePath, 0
                                  ) ;
    end ;
  end ;

  procedure TGIS_FileFGDB.checkW(
    const _status : Integer
  ) ;
  var
    err : String ;
  begin
    if _status <> 0 then begin
      err := asFGDBString( T_FGDBLib(FLib).FGDB_errmsg( FDriverH ) ) ;

      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ) + #13#10 +
                                    Format( '%s', [err] ),
                                    FDatabasePath, 0
                                  ) ;
    end ;
  end ;

  {$IFDEF OXYGENE}
  function TGIS_FileFGDB.isValidPtr(
    _ptr : tgdbptr
  ) : Boolean ;
  {$ELSE}
  function TGIS_FileFGDB.isValidPtr(
    _ptr : IntPtr
  ) : Boolean ;
  {$ENDIF}
  begin
    {$IFNDEF JAVA}
      Result := ( _ptr <> 0 ) ;
    {$ELSE}
      Result := assigned( _ptr ) ;
    {$ENDIF}
  end ;

  function TGIS_FileFGDB.OpenDatabase(
    const _path : String ;
    const _name : String
  ) : Boolean ;
  begin
    Result := initialize ;

    if not Result then exit ;

    parseConfig( _path, _name ) ;

    FGDBConnection := SharedConnections.OpenAny(
                          '',
                          '',
                          FConfig.Values[ GIS_INI_PATH ],
                          '',
                          FConfig,
                          TFGDBConnection
                        ) ;
    if assigned( FGDBConnection ) then
      FDriverH := TFGDBConnection(FGDBConnection).DB ;

    Result := True ;
  end ;

  function TGIS_FileFGDB.OpenTable(
    const _path : String ;
    const _name : String
  ) : Boolean ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(FLayerPath) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( FLayerPath ) ;
    {$ENDIF}

    FTableH := T_FGDBLib(FLib).FGDB_gettable(
                 FDriverH,
                 cmd
                ) ;
    if not isValidPtr( FTableH ) then
      checkW( -1 ) ;

    Result := True ;
  end ;

  procedure TGIS_FileFGDB.CloseTable ;
  begin
    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_table_close(
                 FTableH
                )
            ) ;
  end ;

  function TGIS_FileFGDB.GetLayer : Boolean ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
    str : String ;
  begin
    str := FConfig.Values[ GIS_INI_LAYERSQL_LAYER ] ;

    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(str) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( str ) ;
    {$ENDIF}

    FLayerH := T_FGDBLib(FLib).FGDB_getlayerbyname(
                  FDriverH,
                  cmd
                ) ;
    if isValidPtr( FLayerH ) then begin
      FLayerInfo := asFGDBString( T_FGDBLib(FLib).FGDB_layer_info( FLayerH ) ) ;
      FLayerExtent := GisNoWorld ;
      T_FGDBLib(FLib).FGDB_layer_extent( FLayerH,
                                          FLayerExtent.XMin, FLayerExtent.XMax,
                                          FLayerExtent.YMin, FLayerExtent.YMax
                                        ) ;
      if IsNan( FLayerExtent.XMin ) then
        FLayerExtent := GisNoWorld ;
    end
    else
      checkR( -1 ) ;

    Result := True ;
  end ;

  function TGIS_FileFGDB.NewDatabase(
    const _path : String ;
    const _name : String
  ) : Boolean ;
  var
    fpath  : String ;
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    Result := initialize ;

    if not Result then exit ;

    parseConfig( _path, _name ) ;

    FDriverH := T_FGDBLib(FLib).FGDB_initialize ;

    fpath := GetPathAbsolute( GetFileDir( _path ), FDatabasePath ) ;

    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(fpath) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( fpath ) ;
    {$ENDIF}

    if not SafeDirectoryExists( fpath ) then begin
      checkW( T_FGDBLib(FLib).FGDB_new(
                FDriverH,
                cmd
              )
            ) ;
    end ;

    Result := True ;
  end ;

  procedure TGIS_FileFGDB.CloseDatabase ;
  begin
    if assigned( FGDBConnection ) then
      SharedConnections.CloseAny( TFGDBConnection( FGDBConnection ) )
    else if isValidPtr( FDriverH ) then
      T_FGDBLib(FLib).FGDB_finalize( FDriverH ) ;

    FGDBConnection := nil ;
    {$IFDEF LEVEL_XE2_RTL}
      FLayerH := IntPtr(0) ;
    {$ELSE}
      FLayerH := nil ;
    {$ENDIF}
    {$IFDEF LEVEL_XE2_RTL}
      FTableH := IntPtr(0) ;
    {$ELSE}
      FTableH := nil ;
    {$ENDIF}
    {$IFDEF LEVEL_XE2_RTL}
      FDriverH := IntPtr(0) ;
    {$ELSE}
      FDriverH := nil ;
    {$ENDIF}
  end ;

  procedure TGIS_FileFGDB.CreateLayer(
    const _defn : String ;
    const _path : String
  ) ;
  var
    {$IFDEF OXYGENE}
      cmd1 : TBytes ;
      cmd2 : TBytes ;
    {$ELSE}
      cmd1 : tgdbptr ;
      cmd2 : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd1 := MarshaledAString( UTF8Encode(_defn) ) ;
      cmd2 := MarshaledAString( UTF8Encode(_path) ) ;
    {$ELSE}
      cmd1 := TEncoding.UTF8.GetBytes( _defn ) ;
      cmd2 := TEncoding.UTF8.GetBytes( _path ) ;
    {$ENDIF}

    checkW( T_FGDBLib(FLib).FGDB_createlayer(
                      FDriverH,
                      cmd1,
                      cmd2
                    )
          ) ;
  end ;

  procedure TGIS_FileFGDB.CreateRecord ;
  begin
    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_table_createrow(
                        FTableH
                      )
            ) ;
  end ;

  procedure TGIS_FileFGDB.InsertRecord ;
  begin
    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_table_insertrow(
                        FTableH
                      )
            ) ;
  end ;

  function TGIS_FileFGDB.GetLastRecordOID : TGIS_Uid ;
  var
    oid : Integer ;
  begin
    oid := -1 ;
    if isValidPtr( FTableH ) then
      checkR( T_FGDBLib(FLib).FGDB_table_getlastrowoid(
                        FTableH,
                        oid
                      )
            ) ;
    Result := oid ;
  end ;

  function TGIS_FileFGDB.GetRecord(
    const _query  : String
  ) : Boolean ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    Result := False ;

    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_query) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _query ) ;
    {$ENDIF}

    if isValidPtr( FTableH ) then
      Result := T_FGDBLib(FLib).FGDB_table_getrow(
                    FTableH,
                    cmd
                  ) = 0 ;
  end ;

  procedure TGIS_FileFGDB.UpdateRecord ;
  begin
    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_table_updaterow(
                        FTableH
                      )
            ) ;
  end ;

  procedure TGIS_FileFGDB.DeleteRecord ;
  begin
    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_table_deleterow(
                        FTableH
                      )
            ) ;
  end ;

  procedure TGIS_FileFGDB.RecordSetField(
    const _name   : String  ;
    const _type   : TGIS_FieldType ;
    const _size   : Integer ;
    const _scale  : Integer ;
    const _val    : Variant
  ) ;
  var
    y,m,d   : Word ;
    h,min,
    s, ms   : Word ;
    dfmt    : String ;
  begin
    if GetVariantType( _val ) = TGIS_VariantType.Nothing then
      setRecordFieldNull( _name )
    else begin
      case _type of
        TGIS_FieldType.String  :
            setRecordFieldString( _name, VarToString( _val ) ) ;
        TGIS_FieldType.Boolean     :
          if VarToBoolean( _val ) then
            setRecordFieldInteger( _name, 1 )
          else
            setRecordFieldInteger( _name, 0 ) ;
        TGIS_FieldType.Float :
            setRecordFieldDouble( _name, VarToDouble( _val )  ) ;
        TGIS_FieldType.Number :
          if _scale = 0 then
            setRecordFieldInteger( _name, VarToInt32( _val ) )
          else
            setRecordFieldDouble( _name, VarToDouble( _val )  ) ;
        TGIS_FieldType.Date    :
          begin
            DecodeDate( VarToDateTime( _val ), y, m, d       ) ;
            DecodeTime( VarToDateTime( _val ), h, min, s, ms ) ;

            {$IFDEF OXYGENE}
            if length( VarToDateTime( _val ).ToString ) > 10 then
            {$ELSE}
            if length( _val ) > 10 then
            {$ENDIF}
              dfmt := 'yyyy-MM-dd hh:nn:ss'
            else
              dfmt := 'yyyy-MM-dd' ;

            setRecordFieldDateTime(
              _name,
              {$IFNDEF OXYGENE}
                FormatDateTime(dfmt, EncodeDate(y,m,d)+EncodeTime(h,min,s,ms) )
              {$ELSE}
                FormatDateTime(dfmt, EncodeDate(y,m,d).AddHours(h).AddMinutes(min).AddSeconds(s) )
              {$ENDIF}
            ) ;
          end ;
      end ;
    end ;

  end ;

  procedure TGIS_FileFGDB.RecordSetGeometry(
    const _geom : Variant
  ) ;
  var
    ptr : tgdbptr ;
    len : Integer ;
    {$IFDEF OXYGENE}
    buf : TBytes ;
    {$ENDIF}
  begin
    len := VarArrayHighBound( _geom, 1 )+1 ;
    {$IFDEF OXYGENE}
      ptr := Marshal.AllocHGlobal( len ) ;
      try
        buf := TBytes(_geom) ;
        {$IFDEF JAVA}
          ptr.write( 0, buf, 0, len ) ;
        {$ELSE}
          Marshal.Copy( buf, 0, ptr, len ) ;
        {$ENDIF}
        setRecordGeometry( ptr, len ) ;
      finally
        Marshal.FreeHGlobal( ptr ) ;
      end;
    {$ELSE}
      ptr := VarArrayLock( _geom ) ;
      try
        setRecordGeometry( ptr, len ) ;
      finally
        VarArrayUnlock( _geom ) ;
      end;
    {$ENDIF}
  end ;

  procedure TGIS_FileFGDB.DeleteLayer(
    const _path : String
  ) ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_path) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _path ) ;
    {$ENDIF}

    if isValidPtr( FDriverH ) then
      checkW( T_FGDBLib(FLib).FGDB_deletelayer(
                        FDriverH,
                        cmd
                      )
            ) ;
  end ;

  procedure TGIS_FileFGDB.createFeatureDataset(
    const _path   : String ;
    const _def    : String
  ) ;
  var
    res : Integer ;
    {$IFDEF OXYGENE}
      cmd1 : TBytes ;
      cmd2 : TBytes ;
    {$ELSE}
      cmd1 : tgdbptr ;
      cmd2 : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd1 := MarshaledAString( UTF8Encode(_def) ) ;
      cmd2 := MarshaledAString( UTF8Encode(_path) ) ;
    {$ELSE}
      cmd1 := TEncoding.UTF8.GetBytes( _def ) ;
      cmd2 := TEncoding.UTF8.GetBytes( _path ) ;
    {$ENDIF}

    res := 0 ;
    if isValidPtr( FDriverH ) then
      res := T_FGDBLib(FLib).FGDB_createdataset(
                        FDriverH,
                        cmd1,
                        cmd2
                      ) ;
    if res = 2 then
      // dataset already exists
    else
      checkW( res ) ;
  end ;

  procedure TGIS_FileFGDB.parseConfig(
    const _path : String ;
    const _name : String
  );
  var
    tkn   : TGIS_Tokenizer ;
    lname : String ;
  begin
    FConfig.Clear ;

    if IsStringEmpty( _name ) then
      lname := 'LAYER'
    else begin
      if Pos( '\', _name ) >= StringFirst then
        lname := _name
      else
        lname := GisCanonicalSQLName( _name ) ;
    end;

    if ( CompareText( GetFileExt( _path ), GIS_TTKLS_EXT    ) = 0 ) or
       ( CompareText( GetFileExt( _path ), GIS_TTKLAYER_EXT ) = 0 ) then begin
      ReadSQLParamsFromPath( _path, FConfig ) ;
      FConfig.Values[ GIS_INI_CONFIG ] := _path ;
      FDatabasePath := FConfig.Values[ GIS_INI_PATH ] ;
    end
    else begin
      FConfig.Values[ GIS_INI_PATH           ] := _path ;
      FConfig.Values[ GIS_INI_CONFIG         ] := _path ;
      FConfig.Values[ GIS_INI_LAYERSQL_LAYER ] := lname ;
      FDatabasePath := _path ;
    end ;

    lname := FConfig.Values[ GIS_INI_LAYERSQL_LAYER ] ;
    if IsStringEmpty( lname ) then
      lname := GisCanonicalSQLName( _name ) ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( lname, '\' ) ;
      if tkn.Result.Count = 1 then begin
        FLayerPath   := tkn.Result[0] ;
        FDatasetPath := '\' + lname ;
        FDatasetName := lname ;
      end
      else if tkn.Result.Count = 2 then begin
        FDatasetName := tkn.Result[0] ;
        FDatasetPath := '\' + FDatasetName ;
        FLayerPath   := tkn.Result[1] ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;

  end ;

  function TGIS_FileFGDB.prepareFeatureDatasetDef(
    const _catalogPath  : String ;
    const _datasetName  : String ;
    const _cs           : TGIS_CSCoordinateSystem ;
    const _extent       : TGIS_Extent
  ) : String ;
  var
    xdoc : TGIS_XMLDocument ;
    node : IXMLNode ;
  begin
    xdoc := TGIS_XMLDocument.Create ;
    try
      node := xdoc.AddChild( 'esri:DataElement' ) ;
      node.Attributes['xmlns:xsi' ] := 'http://www.w3.org/2001/XMLSchema-instance' ;
      node.Attributes['xmlns:xs'  ] := 'http://www.w3.org/2001/XMLSchema' ;
      node.Attributes['xmlns:esri'] := 'http://www.esri.com/schemas/ArcGIS/10.1' ;
      node.Attributes['xsi:type'  ] := 'esri:DEFeatureDataset' ;

      node.AddChild('CatalogPath'     ).NodeValue := _catalogPath ;
      node.AddChild('Name'            ).NodeValue := _datasetName ;
      node.AddChild('ChildrenExpanded').NodeValue := 'false' ;
      node.AddChild('DatasetType'     ).NodeValue := 'esriDTFeatureDataset' ;
      node.AddChild('Versioned'       ).NodeValue := 'false' ;
      node.AddChild('CanVersion'      ).NodeValue := 'false' ;
      node.AddChild('Extent'          ).Attributes['xsi:nil'] := 'true' ;

      prepareSpatialReference( _cs, _extent, xdoc.DocumentElement ) ;

      xdoc.SaveToXML( Result ) ;
    finally
      FreeObject( xdoc ) ;
    end ;
  end ;

  procedure TGIS_FileFGDB.prepareSpatialReference(
    const _cs     : TGIS_CSCoordinateSystem ;
    const _extent : TGIS_Extent ;
    const _node   : IXMLNode
  ) ;
  const
    SPATIAL_DOMAIN     = 9007199254740990 ;
    SPATIAL_DOMAIN_LOW = -2147483647 ;
  var
    node      : IXMLNode ;
    ztol      : Double ;
    xytol     : Double ;
    zscale    : Integer ;
    wkt       : String ;
    xres      : Double ;
    yres      : Double ;
    res       : Double ;
    xorg      : Integer ;
    yorg      : Integer ;
    ext       : TGIS_Extent ;
    maxres    : Int64 ;
    bhighres  : Boolean ;
  begin
    node := _node.AddChild( 'SpatialReference' ) ;
    if _cs is TGIS_CSUnknownCoordinateSystem then begin
      node.Attributes['xsi:type'] := 'esri:UnknownCoordinateSystem' ;
    end
    else begin
      if _cs is TGIS_CSProjectedCoordinateSystem then
        node.Attributes['xsi:type'] := 'esri:ProjectedCoordinateSystem'
      else
        node.Attributes['xsi:type'] := 'esri:GeographicCoordinateSystem' ;

      // strangly ESRI WKT doesn't work
      wkt := asFGDBString( T_FGDBLib(FLib).FGDB_findspatialref( FDriverH, _cs.EPSG ) ) ;
      if IsStringEmpty( wkt ) then
        wkt := _cs.FullWKT ;

      node.AddChild('WKT').NodeValue := wkt ;
    end ;

    bhighres := GisMetadataAsBoolean( FGDB_METADATA_HIGH_PRECISION, True ) ;

    if bhighres then begin
      if not ( _cs is TGIS_CSUnknownCoordinateSystem ) then begin
        ext  := _cs.ValidityExtent ;
      end
      else begin
        ext  := _TGIS_Extent(_extent) ;
        ext.XMin := 2 * _extent.XMin - _extent.XMax ;
        ext.XMax := 2 * _extent.XMax - _extent.XMin ;
        ext.YMin := 2 * _extent.YMin - _extent.YMax ;
        ext.YMax := 2 * _extent.YMax - _extent.YMin ;

        ext := GisExtent(
                _extent.XMin - 1e9,
                _extent.YMin - 1e9,
                _extent.XMax + 1e9,
                _extent.XMax + 1e9
              ) ;
      end ;
      xorg := FloorS( ext.XMin ) ;
      yorg := FloorS( ext.YMin ) ;

      xres := 1 / ( ( ext.XMax - xorg ) / SPATIAL_DOMAIN ) ;
      yres := 1 / ( ( ext.XMax - xorg ) / SPATIAL_DOMAIN ) ;

      res := Min( xres, yres ) ;
      maxres := 1 ;
      while ( maxres * 10 < res ) and ( maxres < 1e17 ) do
        maxres := maxres * 10 ;

      xytol := 1 / maxres * 10 ;
    end
    else begin
      if ( _cs is TGIS_CSUnknownCoordinateSystem   ) or
         ( _cs is TGIS_CSProjectedCoordinateSystem ) then begin

        if ( _cs is TGIS_CSProjectedCoordinateSystem ) then
          xytol := 0.001 * TGIS_CSProjectedCoordinateSystem(_cs).Units.Factor
        else
          xytol := 0.001 ;

        xorg   := SPATIAL_DOMAIN_LOW ;
        yorg   := SPATIAL_DOMAIN_LOW ;
        maxres := TruncS(1 / xytol * 10) ;
      end
      else begin
        xorg   := -400 ;
        yorg   := -400 ;
        maxres := 1000000000 ;
        xytol  := 0.000000008983153 ;
      end ;
    end ;

    ztol   := 0.001 ;
    zscale := TruncS(1 / ztol * 10) ;
    maxres := TruncS( GisMetadataAsFloat( FGDB_METADATA_XY_SCALE, maxres ) ) ;
    xytol  := GisMetadataAsFloat( FGDB_METADATA_XY_TOLERANCE, xytol ) ;

    node.AddChild('XOrigin'      ).NodeValue := xorg  ;
    node.AddChild('YOrigin'      ).NodeValue := yorg  ;
    node.AddChild('XYScale'      ).NodeValue := maxres ;
    node.AddChild('ZOrigin'      ).NodeValue := -100000 ;
    node.AddChild('ZScale'       ).NodeValue := zscale ;
    node.AddChild('XYTolerance'  ).NodeValue := DotFloatToStr( xytol ) ;
    node.AddChild('ZTolerance'   ).NodeValue := DotFloatToStr( ztol ) ;
    node.AddChild('HighPrecision').NodeValue := 'true' ;

    if _cs.EPSG > 0 then
      node.AddChild('WKID').NodeValue := _cs.EPSG ;
  end ;

  function TGIS_FileFGDB.setRecordFieldDateTime(
    const _field  : String ;
    const _value  : String
  ) : Integer ;
  var
    {$IFDEF OXYGENE}
      cmd1 : TBytes ;
      cmd2 : TBytes ;
    {$ELSE}
      cmd1 : tgdbptr ;
      cmd2 : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd1 := MarshaledAString(UTF8Encode(VarToString( _field )) ) ;
      cmd2 := MarshaledAString(UTF8Encode(VarToString( _value )) ) ;
    {$ELSE}
      cmd1 := TEncoding.UTF8.GetBytes( VarToString( _field ) ) ;
      cmd2 := TEncoding.UTF8.GetBytes( VarToString( _value ) ) ;
    {$ENDIF}

    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_row_setdatetime(
                        FTableH,
                        cmd1,
                        cmd2
                      )
            ) ;
    Result := 0 ;
  end ;

  function TGIS_FileFGDB.setRecordFieldDouble(
    const _field  : String ;
    const _value  : Double
  ) : Integer ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_row_setdouble(
                        FTableH,
                        cmd,
                        _value
                      )
            ) ;
    Result := 0 ;
  end ;

  function TGIS_FileFGDB.setRecordFieldFloat(
    const _field  : String ;
    const _value  : Single
  ) : Integer ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_row_setfloat(
                        FTableH,
                        cmd,
                        _value
                      )
            ) ;
    Result := 0 ;
  end ;

  function TGIS_FileFGDB.setRecordFieldInteger(
    const _field  : String ;
    const _value  : Integer
  ) : Integer ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_row_setinteger(
                        FTableH,
                        cmd,
                        _value
                      )
            ) ;
    Result := 0 ;
  end ;

  function TGIS_FileFGDB.setRecordFieldNull(
    const _field : String
  ) : Integer ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_row_setnull(
                        FTableH,
                        cmd
                      )
            ) ;
    Result := 0 ;
  end ;

  function TGIS_FileFGDB.setRecordGeometry(
    const _geom   : tgdbptr ;
    const _size   : Integer
  ) : Integer ;
  begin
    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_row_setgeometry(
                        FTableH,
                        _geom,
                        _size
                      )
            ) ;
    Result := 0 ;
  end ;

  function TGIS_FileFGDB.setRecordFieldString(
    const _field  : String ;
    const _value  : String
  ) : Integer ;
  var
    {$IFDEF OXYGENE}
      cmd1 : TBytes ;
      cmd2 : TBytes ;
    {$ELSE}
      cmd1 : tgdbptr ;
      cmd2 : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd1 := MarshaledAString(UTF8Encode( _field ) ) ;
      cmd2 := MarshaledAString(UTF8Encode(VarToString( _value )) ) ;
    {$ELSE}
      cmd1 := TEncoding.UTF8.GetBytes(  _field ) ;
      cmd2 := TEncoding.UTF8.GetBytes( VarToString( _value ) ) ;
    {$ENDIF}

    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_row_setstring(
                        FTableH,
                        cmd1,
                        cmd2
                      )
            ) ;
    Result := 0 ;
  end ;

  function TGIS_FileFGDB.prepareLayerDef(
    const _catalogPath  : String ;
    const _layerName    : String ;
    const _cs           : TGIS_CSCoordinateSystem ;
    const _extent       : TGIS_Extent ;
    const _type         : TGIS_ShapeType ;
    const _dim          : TGIS_DimensionType
  ) : String ;
  var
    xdoc      : TGIS_XMLDocument ;
    node      : IXMLNode ;
    fnode     : IXMLNode ;
    gnode     : IXMLNode ;
    afn       : IXMLNode ;
    esri_type : String ;
    has_z     : Boolean ;
    has_m     : Boolean ;
  begin
    case _type of
      TGIS_ShapeType.Point      : esri_type := 'esriGeometryPoint' ;
      TGIS_ShapeType.MultiPoint : esri_type := 'esriGeometryMultipoint';
      TGIS_ShapeType.Arc        : esri_type := 'esriGeometryPolyline';
      TGIS_ShapeType.Polygon    : esri_type := 'esriGeometryPolygon';
      TGIS_ShapeType.MultiPatch : esri_type := 'esriGeometryMultiPatch'
    else                          esri_type := ''
    end ;

    has_z := (_dim = TGIS_DimensionType.XYZ ) or (_dim = TGIS_DimensionType.XYZM ) ;
    has_m := (_dim = TGIS_DimensionType.XYM ) or (_dim = TGIS_DimensionType.XYZM ) ;

    xdoc := TGIS_XMLDocument.Create ;
    try
      node := xdoc.AddChild( 'esri:DataElement' ) ;
      node.Attributes['xmlns:xsi' ] := 'http://www.w3.org/2001/XMLSchema-instance' ;
      node.Attributes['xmlns:xs'  ] := 'http://www.w3.org/2001/XMLSchema' ;
      node.Attributes['xmlns:esri'] := 'http://www.esri.com/schemas/ArcGIS/10.1' ;
      node.Attributes['xsi:type'  ] := 'esri:DEFeatureClass' ;

      node.AddChild('CatalogPath'     ).NodeValue := _catalogPath ;
      node.AddChild('Name'            ).NodeValue := _layerName ;
      node.AddChild('ChildrenExpanded').NodeValue := 'false' ;
      node.AddChild('DatasetType'     ).NodeValue := 'esriDTFeatureClass' ;
      node.AddChild('Versioned'       ).NodeValue := 'false' ;
      node.AddChild('CanVersion'      ).NodeValue := 'false' ;
      node.AddChild('HasOID'          ).NodeValue := 'true' ;
      node.AddChild('OIDFieldName'    ).NodeValue := 'OBJECTID' ;

      fnode := node.AddChild('Fields') ;
      fnode.Attributes['xsi:type'] := 'esri:Fields' ;
        afn := fnode.AddChild('FieldArray'    ) ;
        afn.Attributes['xsi:type'] := 'esri:ArrayOfField' ;

        // geometry
        fnode := afn.AddChild('Field') ;
        fnode.Attributes['xsi:type'] := 'esri:Field' ;

          fnode.AddChild('Name'      ).NodeValue := 'SHAPE' ;
          fnode.AddChild('Type'      ).NodeValue := 'esriFieldTypeGeometry' ;
          fnode.AddChild('IsNullable').NodeValue := 'false' ;
          fnode.AddChild('Length'    ).NodeValue := '0' ;
          fnode.AddChild('Precision' ).NodeValue := '0' ;
          fnode.AddChild('Scale'     ).NodeValue := '0' ;
          fnode.AddChild('Required'  ).NodeValue := 'true' ;

          gnode := fnode.AddChild('GeometryDef') ;
          gnode.Attributes['xsi:type'] := 'esri:GeometryDef' ;

            gnode.AddChild('AvgNumPoints').NodeValue := '0' ;
            gnode.AddChild('GeometryType').NodeValue := esri_type ;
            if has_m then
              gnode.AddChild('HasM'      ).NodeValue := 'true'
            else
              gnode.AddChild('HasM'      ).NodeValue := 'false' ;
            if has_z then
              gnode.AddChild('HasZ'      ).NodeValue := 'true'
            else
              gnode.AddChild('HasZ'      ).NodeValue := 'false' ;

            prepareSpatialReference( _cs, _extent, gnode ) ;

          // OBJECTID
          fnode := afn.AddChild('Field') ;
          fnode.Attributes['xsi:type'] := 'esri:Field' ;

            fnode.AddChild('Name'      ).NodeValue := 'OBJECTID' ;
            fnode.AddChild('Type'      ).NodeValue := 'esriFieldTypeOID' ;
            fnode.AddChild('IsNullable').NodeValue := 'false' ;
            fnode.AddChild('Length'    ).NodeValue := '12' ;
            fnode.AddChild('Precision' ).NodeValue := '0' ;
            fnode.AddChild('Scale'     ).NodeValue := '0' ;
            fnode.AddChild('Required'  ).NodeValue := 'true' ;

      fnode := node.AddChild('Indexes') ;
      fnode.Attributes['xsi:type'] := 'esri:Indexes' ;
        afn := fnode.AddChild('IndexArray') ;
        afn.Attributes['xsi:type'] := 'esri:ArrayOfIndex' ;

       node.AddChild('CLSID'   ).NodeValue := '{52353152-891A-11D0-BEC6-00805F7C4268}' ;
       node.AddChild('EXTCLSID').NodeValue := '' ;

      // declare feature type
      node.AddChild('FeatureType'   ).NodeValue := 'esriFTSimple' ;
      node.AddChild('ShapeType'     ).NodeValue := esri_type ;
      node.AddChild('ShapeFieldName').NodeValue := 'SHAPE' ;
      if has_m then
        node.AddChild('HasM'        ).NodeValue := 'true'
      else
        node.AddChild('HasM'        ).NodeValue := 'false' ;
      if has_z then
        node.AddChild('HasZ'        ).NodeValue := 'true'
      else
        node.AddChild('HasZ'        ).NodeValue := 'false' ;

      node.AddChild('HasSpatialIndex').NodeValue := 'false' ;
      node.AddChild('AreaFieldName'  ) ;
      node.AddChild('LengthFieldName') ;
      node.AddChild('Extent'         ).Attributes['xsi:nil'] := 'true' ;

      prepareSpatialReference( _cs, _extent, node ) ;

      xdoc.SaveToXML( Result ) ;
    finally
      FreeObject( xdoc ) ;
    end ;
  end ;

  procedure TGIS_FileFGDB.Build(
    const _path   : String ;
    const _name   : String ;
    const _extent : TGIS_Extent ;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType ;
    const _cs     : TGIS_CSCoordinateSystem
  ) ;
  var
    ds_def  : String ;
    lay_def : String ;
  begin
    ds_def := prepareFeatureDatasetDef( FDatasetPath, FDatasetName, _cs, _extent ) ;
    createFeatureDataset( FDatasetPath, ds_def ) ;

    try
      DeleteLayer( FLayerPath ) ;
    except
      // layer may not exist
    end ;

    // layer spatial reference should be the same as feature dataset
    lay_def := prepareLayerDef( FDatasetPath, FLayerPath, _cs, _extent, _type, _dim ) ;
    CreateLayer( lay_def, FDatasetPath ) ;
  end ;

  procedure TGIS_FileFGDB.StartBulkTransaction ;
  begin
    if isValidPtr( FTableH ) then
      T_FGDBLib(FLib).FGDB_table_starttransaction( FTableH ) ;
  end ;

  procedure TGIS_FileFGDB.EndBulkTransaction ;
  begin
    try
      if isValidPtr( FTableH ) then
        T_FGDBLib(FLib).FGDB_table_endtransaction( FTableH ) ;
    except
      // ignore float exception from esri sdk upon saving a single point
    end;
  end ;

  procedure TGIS_FileFGDB.CursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if isValidPtr( FLayerH ) then
      T_FGDBLib(FLib).FGDB_cursor_open( FLayerH, _cursor ) ;
  end ;

  procedure TGIS_FileFGDB.CursorClose(
    const _cursor : Integer
  ) ;
  begin
    if isValidPtr( FLayerH ) then
      T_FGDBLib(FLib).FGDB_cursor_close( FLayerH, _cursor ) ;
  end ;

  function  TGIS_FileFGDB.CursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    if isValidPtr( FLayerH ) then
      Result := T_FGDBLib(FLib).FGDB_cursor_eof( FLayerH, _cursor ) = 1
    else
      Result := False ;
  end ;

  procedure TGIS_FileFGDB.CursorFirst(
    const _cursor : Integer ;
    const _xmin   : Double ;
    const _xmax   : Double ;
    const _ymin   : Double ;
    const _ymax   : Double ;
    const _query  : String
  ) ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_query) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _query ) ;
    {$ENDIF}

    if isValidPtr( FLayerH ) then
      T_FGDBLib(FLib).FGDB_cursor_first(
        FLayerH,
        _cursor,
        _xmin,
        _xmax,
        _ymin,
        _ymax,
        cmd
      ) ;
  end ;

  procedure TGIS_FileFGDB.CursorNext(
    const _cursor : Integer
  ) ;
  begin
    if isValidPtr( FLayerH ) then
      T_FGDBLib(FLib).FGDB_cursor_next( FLayerH, _cursor ) ;
  end ;

  {$IFDEF OXYGENE}
  function  TGIS_FileFGDB.CursorShape(
    const _cursor : Integer ;
      var _size   : Integer
  ) : tgdbptr ;
  {$ELSE}
  function  TGIS_FileFGDB.CursorShape(
    const _cursor : Integer ;
      var _size   : Integer
  ) : IntPtr ;
  {$ENDIF}
  begin
    if isValidPtr( FLayerH ) then
      Result := T_FGDBLib(FLib).FGDB_cursor_shape( FLayerH, _cursor, _size )
    else
      {$IFDEF LEVEL_XE2_RTL}
        Result := 0 ;
      {$ELSE}
        Result := nil ;
      {$ENDIF}
  end ;

  function  TGIS_FileFGDB.GetOID(
    const _cursor : Integer
  ) : Integer ;
  begin
    if isValidPtr( FLayerH ) then
      Result := T_FGDBLib(FLib).FGDB_row_getoid( FLayerH, _cursor )
    else
      Result := 0 ;
  end ;

  function  TGIS_FileFGDB.GetInteger(
    const _cursor : Integer ;
    const _field  : String
  ) : Integer ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FLayerH ) then
      Result := T_FGDBLib(FLib).FGDB_row_getinteger(
                                      FLayerH,
                                      _cursor,
                                      cmd
                                    )
    else
      Result := 0 ;
  end ;

  function  TGIS_FileFGDB.GetDouble(
    const _cursor : Integer ;
    const _field  : String
  ) : Double ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FLayerH ) then
      Result := T_FGDBLib(FLib).FGDB_row_getdouble(
                                      FLayerH,
                                      _cursor,
                                      cmd
                                    )
    else
      Result := 0 ;
  end ;

  function  TGIS_FileFGDB.GetString(
    const _cursor : Integer ;
    const _field  : String ;
    const _size   : Integer
  ) : String ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FLayerH ) then
      Result := asFGDBString( T_FGDBLib(FLib).FGDB_row_getstring(
                                                    FLayerH,
                                                    _cursor,
                                                    cmd,
                                                    _size
                                                   )
                             )
    else
      Result := '' ;
  end ;

  function  TGIS_FileFGDB.GetDateTime(
    const _cursor : Integer ;
    const _field  : String
  ) : String ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FLayerH ) then
      Result := asFGDBString( T_FGDBLib(FLib).FGDB_row_getdatetime(
                                                    FLayerH,
                                                    _cursor,
                                                    cmd
                                                  )
                             )
    else
      Result := '' ;
  end ;

  function  TGIS_FileFGDB.GetIsNull(
    const _cursor : Integer ;
    const _field  : String
  ) : Boolean ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
    res : Integer ;
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FLayerH ) then begin
      res := T_FGDBLib(FLib).FGDB_row_getnull( FLayerH,
                                               _cursor,
                                               cmd
                                             ) ;
      Result := res = 1 ;
    end
    else
      Result := False ;
  end ;

  function  TGIS_FileFGDB.GetFloat(
    const _cursor : Integer ;
    const _field  : String
  ) : Single ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FLayerH ) then
      Result := T_FGDBLib(FLib).FGDB_row_getfloat(
                                      FLayerH,
                                      _cursor,
                                      cmd
                                    )
    else
      Result := 0 ;
  end ;

  procedure TGIS_FileFGDB.DeleteField(
    const _table  : String ;
    const _field  : String
  ) ;
  var
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(_field) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( _field ) ;
    {$ENDIF}

    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_table_deletefield(
                        FTableH,
                        cmd
                      )
            ) ;
  end ;

  procedure TGIS_FileFGDB.AddField(
    const _table        : String ;
    const _field        : String ;
    const _fieldType    : TGIS_FieldType ;
    const _fieldWidth   : Integer ;
    const _fieldDecimal : Integer
  ) ;
  var
    fdef    : String ;
    xdoc    : TGIS_XMLDocument ;
    node    : IXMLNode ;
    gdtype  : String ;
    width   : Integer ;
    prec    : Integer ;
    {$IFDEF OXYGENE}
      cmd : TBytes ;
    {$ELSE}
      cmd : tgdbptr ;
    {$ENDIF}
  begin
    width  := 0 ;
    prec   := 0 ;
    case _fieldType of
      TGIS_FieldType.String   :
        begin
          gdtype := 'esriFieldTypeString' ;
          width  := _fieldWidth ;
          prec   := 0 ;
        end ;
      TGIS_FieldType.Number   :
        begin
          if _fieldDecimal = 0 then begin
            gdtype := 'esriFieldTypeInteger' ;
            width  := 4 ;
            prec   := 0 ;
          end
          else begin
            gdtype := 'esriFieldTypeDouble' ;
            width  := 8 ;
            prec   := 15 ;
          end ;
        end;
      TGIS_FieldType.Float    :
        begin
          gdtype := 'esriFieldTypeDouble' ;
          width  := 8 ;
          prec   := 15 ;
        end ;
      TGIS_FieldType.Boolean  :
        begin
          gdtype := 'esriFieldTypeInteger' ;
          width  := 4 ;
          prec   := 0 ;
        end ;
      TGIS_FieldType.Date     :
        begin
          gdtype := 'esriFieldTypeDate' ;
          width  := 8 ;
          prec   := 0 ;
        end ;
    end;

    xdoc := TGIS_XMLDocument.Create ;
    try
      node := xdoc.AddChild( 'esri:Field' ) ;
      node.Attributes['xmlns:xsi' ] := 'http://www.w3.org/2001/XMLSchema-instance' ;
      node.Attributes['xmlns:xs'  ] := 'http://www.w3.org/2001/XMLSchema' ;
      node.Attributes['xmlns:esri'] := 'http://www.esri.com/schemas/ArcGIS/10.1' ;
      node.Attributes['xsi:type'  ] := 'esri:Field' ;

        node.AddChild('Name'      ).NodeValue := _field ;
        node.AddChild('Type'      ).NodeValue := gdtype ;
        node.AddChild('IsNullable').NodeValue := 'true' ;
        node.AddChild('Length'    ).NodeValue := width ;
        node.AddChild('Precision' ).NodeValue := prec ;
        node.AddChild('Scale'     ).NodeValue := '0' ;

      xdoc.SaveToXML( fdef ) ;
    finally
      FreeObject( xdoc ) ;
    end ;


    {$IFNDEF OXYGENE}
      cmd := MarshaledAString( UTF8Encode(fdef) ) ;
    {$ELSE}
      cmd := TEncoding.UTF8.GetBytes( fdef ) ;
    {$ENDIF}

    if isValidPtr( FTableH ) then
      checkW( T_FGDBLib(FLib).FGDB_table_createfield(
                        FTableH,
                        cmd
                      )
            ) ;
  end ;

  function TGIS_FileFGDB.GetAvailableLayers(
    const _path : String
  ) : TGIS_LayerInfoList ;
  var
    i      : Integer ;
    xDoc   : IXMLDocument ;
    xroot  : IXMLNode ;
    xnode  : IXMLNode ;
    lname  : String ;
    ldset  : String ;
    j      : Integer;
    isfea  : Boolean ;
    ltype  : TGIS_ShapeType ;

      procedure parsegeodef( const _xgeodef : IXMLNodeList ) ;
      var
        p     : Integer ;
        gtype : String ;
      begin
        for p := 0 to _xgeodef.Count-1 do begin
          if _xgeodef[p].NodeName = 'GeometryType' then begin
            gtype := VarToString( _xgeodef[p].NodeValue ) ;
            break ;
          end;
        end ;

        if ( gtype = 'esriGeometryPoint' ) then
          ltype := TGIS_ShapeType.Point
        else if ( gtype = 'esriGeometryMultipoint' ) then
          ltype := TGIS_ShapeType.MultiPoint
        else if ( gtype = 'esriGeometryLine' ) then
          ltype := TGIS_ShapeType.Arc
        else if ( gtype = 'esriGeometryPolyline' ) then
          ltype := TGIS_ShapeType.Arc
        else if ( gtype = 'esriGeometryPolygon' ) then
          ltype := TGIS_ShapeType.Polygon
        else if ( gtype = 'esriGeometryMultiPatch' ) then
          ltype := TGIS_ShapeType.MultiPatch
        else
          ltype := TGIS_ShapeType.Unknown ;
      end;

      procedure parsefields( const _fields : IXMLNode ) ;
      var
        l,m :Integer  ;
        xchild, xfield : IXMLNode  ;
      begin
        for l := 0 to _fields.ChildNodes.Count-1 do begin
          xchild := _fields.ChildNodes[l] ;
          if xchild.NodeName = 'Field' then begin
            for m := 0 to xchild.ChildNodes.Count-1 do begin
              xfield := xchild.ChildNodes[m] ;
              if xfield.NodeName = 'GeometryDef' then begin
                parsegeodef( xfield.ChildNodes ) ;
              end ;
            end ;

          end ;
        end ;
      end;

  begin
    Result := TGIS_LayerInfoList.Create ;
    try
      if not initialize then exit ;

      OpenDatabase( _path, '' ) ;

      xDoc := TGIS_XMLDocument.Create ;
      try

        xDoc.Active    := True ;
        xDoc.Version   := '1.0' ;
        xDoc.Encoding  := 'utf-8' ;

        T_FGDBLib(FLib).FGDB_getalllayers( FDriverH ) ;

        for i := 0 to T_FGDBLib(FLib).FGDB_getlayerscount( FDriverH ) - 1
        do begin
          FLayerH := T_FGDBLib(FLib).FGDB_getlayer( FDriverH, i ) ;
          if isValidPtr( FLayerH ) then begin
            FLayerInfo := asFGDBString(
              T_FGDBLib(FLib).FGDB_layer_info( FLayerH )
            ) ;
            xDoc.LoadFromXML( FLayerInfo ) ;
            xroot := xDoc.DocumentElement ;
            isfea := False ;
            for j := 0 to xroot.ChildNodes.Count - 1 do begin
              if xroot.ChildNodes[j].NodeName = 'CatalogPath' then begin
                ldset := VarToString( xroot.ChildNodes[j].Text ) ;
              end
              else if xroot.ChildNodes[j].NodeName = 'Name' then begin
                lname := VarToString( xroot.ChildNodes[j].Text ) ;
              end
              else if xroot.ChildNodes[j].NodeName = 'DatasetType' then begin
                isfea := VarToString( xroot.ChildNodes[j].Text ) =
                  'esriDTFeatureClass' ;
              end
              else if xroot.ChildNodes[j].NodeName = 'Fields' then begin
                xnode := xroot.ChildNodes[j].ChildNodes[0] ;
                parsefields( xnode ) ;
              end
              else if xroot.ChildNodes[j].NodeName = 'FieldArray' then begin
                parsefields( xroot.ChildNodes[j] ) ;
              end ;
            end ;

            if ldset = '\' then
              lname := ldset + lname
            else
              lname := ldset ;

            if isfea then
              Result.Add(
                TGIS_LayerInfo.Create( lname, TGIS_RegisteredLayerType.Vector, ltype )
              ) ;
          end ;
        end  ;
      finally
        FreeObject( xDoc ) ;
        CloseDatabase ;
      end ;
    except
      // wrong connection
      on E : EGIS_Exception do
        TGIS_Logger.AsError( GetClassName(Self), E.Message ) ;
    end ;
  end ;

//=============================================================================
// T_FGDBLib
//=============================================================================

  constructor T_FGDBLib.Create ;
  begin
    {$IFNDEF OXYGENE}
    inherited Create ;
    {$ENDIF}

    DLLLoaded := False ;
  end ;

  {$IFNDEF OXYGENE}
    destructor T_FGDBLib.Destroy ;
    begin
      if DLLLoaded and ( DLLHandle <> 0 ) then begin
        FreeLibrary( DLLHandle ) ;
        DLLHandle := 0 ;
        FInstanceCount := 0 ;
        DLLLoaded := False ;
      end ;

      inherited ;
    end ;
  {$ENDIF}

  function T_FGDBLib.LoadDLL(
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
        _fnc := GetProcAddress( DLLHandle, _name ) ;
        {$ELSE}
        _fnc := GetProcAddress( DLLHandle, PChar( _name ) ) ;
        {$ENDIF}
        assert( _fnc <> nil ) ;
      end ;
    {$ENDIF}
  begin
    Result := DLLLoaded ;

    {$IFDEF JAVA}
      if DLLLoaded or (DLLHandle <> nil ) then exit ;

      DLLHandle := IFGDBLibrary(com.sun.jna.Native.loadLibrary( _dllPath, typeOf(IFGDBLibrary) ) );
      dllok := DLLHandle <> nil ;
    {$ELSE}
      if DLLLoaded or (DLLHandle <> 0 ) then exit ;

      DLLHandle := LoadLibraryWithinHinstance( _dllPath ) ;
      dllok := DLLHandle <> 0 ;
    {$ENDIF}

    DLLLoaded := False ;
    if not dllok then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                   _dllPath + #13#10 + SystemErrorMessage,
                                   0
                                  ) ;
    {$IFDEF OXYGENE}
    {$ELSE}
      mapFnc( @FGDB_initialize,              'FGDB_initialize'            ) ;
      mapFnc( @FGDB_finalize,                'FGDB_finalize'              ) ;
      mapFnc( @FGDB_version,                 'FGDB_version'               ) ;
      mapFnc( @FGDB_errmsg,                  'FGDB_errmsg'                ) ;
      mapFnc( @FGDB_connect,                 'FGDB_connect'               ) ;
      mapFnc( @FGDB_new,                     'FGDB_new'                   ) ;
      mapFnc( @FGDB_disconnect,              'FGDB_disconnect'            ) ;
      mapFnc( @FGDB_getlayer,                'FGDB_getlayer'              ) ;
      mapFnc( @FGDB_getalllayers,            'FGDB_getalllayers'          ) ;
      mapFnc( @FGDB_getlayerbyname,          'FGDB_getlayerbyname'        ) ;
      mapFnc( @FGDB_getlayerscount,          'FGDB_getlayerscount'        ) ;
      mapFnc( @FGDB_createlayer,             'FGDB_createlayer'           ) ;
      mapFnc( @FGDB_deletelayer,             'FGDB_deletelayer'           ) ;
      mapFnc( @FGDB_createdataset,           'FGDB_createdataset'         ) ;
      mapFnc( @FGDB_table_createfield,       'FGDB_table_createfield'     ) ;
      mapFnc( @FGDB_table_deletefield,       'FGDB_table_deletefield'     ) ;
      mapFnc( @FGDB_cursor_open,             'FGDB_cursor_open'           ) ;
      mapFnc( @FGDB_cursor_close,            'FGDB_cursor_close'          ) ;
      mapFnc( @FGDB_cursor_eof,              'FGDB_cursor_eof'            ) ;
      mapFnc( @FGDB_cursor_shape,            'FGDB_cursor_shape'          ) ;
      mapFnc( @FGDB_cursor_first,            'FGDB_cursor_first'          ) ;
      mapFnc( @FGDB_cursor_next,             'FGDB_cursor_next'           ) ;
      mapFnc( @FGDB_layer_info,              'FGDB_layer_info'            ) ;
      mapFnc( @FGDB_layer_extent,            'FGDB_layer_extent'          ) ;
      mapFnc( @FGDB_row_getoid,              'FGDB_row_getoid'            ) ;
      mapFnc( @FGDB_row_getinteger,          'FGDB_row_getinteger'        ) ;
      mapFnc( @FGDB_row_getfloat,            'FGDB_row_getfloat'          ) ;
      mapFnc( @FGDB_row_getdouble,           'FGDB_row_getdouble'         ) ;
      mapFnc( @FGDB_row_getstring,           'FGDB_row_getstring'         ) ;
      mapFnc( @FGDB_row_getdatetime,         'FGDB_row_getdatetime'       ) ;
      mapFnc( @FGDB_row_getnull,             'FGDB_row_getnull'           ) ;
      mapFnc( @FGDB_gettable,                'FGDB_gettable'              ) ;
      mapFnc( @FGDB_findspatialref,          'FGDB_findspatialref'        ) ;
      mapFnc( @FGDB_table_createfield,       'FGDB_table_createfield'     ) ;
      mapFnc( @FGDB_table_deletefield,       'FGDB_table_deletefield'     ) ;
      mapFnc( @FGDB_table_createrow,         'FGDB_table_createrow'       ) ;
      mapFnc( @FGDB_table_insertrow,         'FGDB_table_insertrow'       ) ;
      mapFnc( @FGDB_table_getlastrowoid,     'FGDB_table_getlastrowoid'   ) ;
      mapFnc( @FGDB_table_getrow,            'FGDB_table_getrow'          ) ;
      mapFnc( @FGDB_table_updaterow,         'FGDB_table_updaterow'       ) ;
      mapFnc( @FGDB_table_deleterow,         'FGDB_table_deleterow'       ) ;
      mapFnc( @FGDB_table_starttransaction,  'FGDB_table_starttransaction') ;
      mapFnc( @FGDB_table_endtransaction,    'FGDB_table_endtransaction'  ) ;
      mapFnc( @FGDB_table_close,             'FGDB_table_close'           ) ;
      mapFnc( @FGDB_row_setinteger,          'FGDB_row_setinteger'        ) ;
      mapFnc( @FGDB_row_setfloat,            'FGDB_row_setfloat'          ) ;
      mapFnc( @FGDB_row_setdouble,           'FGDB_row_setdouble'         ) ;
      mapFnc( @FGDB_row_setstring,           'FGDB_row_setstring'         ) ;
      mapFnc( @FGDB_row_setdatetime,         'FGDB_row_setdatetime'       ) ;
      mapFnc( @FGDB_row_setnull,             'FGDB_row_setnull'           ) ;
      mapFnc( @FGDB_row_setgeometry,         'FGDB_row_setgeometry'       ) ;
    {$ENDIF}
    DLLLoaded := True ;

    Result := DLLLoaded ;
  end ;

  {$IFDEF JAVA}
    function T_FGDBLib.FGDB_initialize : tgdbptr ;
    begin
      Result := DLLHandle.FGDB_initialize ;
    end;

    procedure T_FGDBLib.FGDB_finalize( _instance : tgdbptr )  ;
    begin
      DLLHandle.FGDB_finalize( _instance ) ;
    end;

    function T_FGDBLib.FGDB_version( _instance : tgdbptr ) : Double ;
    begin
      Result := DLLHandle.FGDB_version( _instance ) ;
    end;

    function T_FGDBLib.FGDB_errmsg( _instance : tgdbptr ) : tgdbptr ;
    begin
      Result := DLLHandle.FGDB_errmsg( _instance ) ;
    end;

    function T_FGDBLib.FGDB_connect( _instance : tgdbptr ;
                            _params   : TBytes
                          ) : Integer ;
    begin
      Result := DLLHandle.FGDB_connect( _instance, TEncoding.UTF8.GetString(_params) ) ;
    end;

    function T_FGDBLib.FGDB_new( _instance : tgdbptr ;
                        _params   : TBytes
                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_new( _instance, TEncoding.UTF8.GetString(_params) ) ;
    end;

    procedure T_FGDBLib.FGDB_disconnect( _instance : tgdbptr ) ;
    begin
      DLLHandle.FGDB_disconnect( _instance ) ;
    end;

    function T_FGDBLib.FGDB_getlayer( _instance : tgdbptr ;
                            _index    : Integer
                            ) : tgdbptr ;
    begin
      Result := DLLHandle.FGDB_getlayer( _instance, _index ) ;
    end;

    procedure T_FGDBLib.FGDB_getalllayers( _instance : tgdbptr ) ;
    begin
      DLLHandle.FGDB_getalllayers( _instance ) ;
    end;

    function T_FGDBLib.FGDB_getlayerbyname( _instance : tgdbptr ;
                                  _name     : TBytes
                                  ) : tgdbptr ;
    begin
      Result := DLLHandle.FGDB_getlayerbyname( _instance, TEncoding.UTF8.GetString(_name) ) ;
    end;

    function T_FGDBLib.FGDB_getlayerscount( _instance : tgdbptr ) : Integer ;
    begin
      Result := DLLHandle.FGDB_getlayerscount( _instance ) ;
    end;

    function T_FGDBLib.FGDB_layer_info( _layerh : tgdbptr ) : tgdbptr ;
    begin
      Result := DLLHandle.FGDB_layer_info( _layerh ) ;
    end;

    procedure T_FGDBLib.FGDB_layer_extent( _layerh  : tgdbptr ;
                                var _xmin : Double ;
                                var _xmax : Double ;
                                var _ymin : Double ;
                                var _ymax : Double
                              )  ;
    var
      xmin, xmax, ymin, ymax : com.sun.jna.ptr.DoubleByReference ;
    begin
      xmin := new com.sun.jna.ptr.DoubleByReference() ;
      xmax := new com.sun.jna.ptr.DoubleByReference() ;
      ymin := new com.sun.jna.ptr.DoubleByReference() ;
      ymax := new com.sun.jna.ptr.DoubleByReference() ;

      DLLHandle.FGDB_layer_extent( _layerh, xmin, xmax, ymin, ymax ) ;
      _xmin := xmin.Value ;
      _xmax := xmax.Value ;
      _ymin := ymin.Value ;
      _ymax := ymax.Value ;
    end;

    function T_FGDBLib.FGDB_createlayer     ( _instance : tgdbptr ;
                                    _defn     : TBytes ;
                                    _path     : TBytes
                                  ) : Integer ;
    begin
      Result := DLLHandle.FGDB_createlayer( _instance, TEncoding.UTF8.GetString(_defn), TEncoding.UTF8.GetString(_path) ) ;
    end;

    function T_FGDBLib.FGDB_deletelayer     ( _instance : tgdbptr ;
                                    _path     : TBytes
                                  ) : Integer ;
    begin
      Result := DLLHandle.FGDB_deletelayer( _instance, TEncoding.UTF8.GetString(_path) ) ;
    end;

    function T_FGDBLib.FGDB_createdataset   ( _instance : tgdbptr ;
                                    _defn     : TBytes ;
                                    _path     : TBytes
                                  ) : Integer ;
    begin
      Result := DLLHandle.FGDB_createdataset( _instance, TEncoding.UTF8.GetString(_defn), TEncoding.UTF8.GetString(_path) ) ;
    end;

    function T_FGDBLib.FGDB_gettable        ( _instance : tgdbptr ;
                                    _name     : TBytes
                                  ) : tgdbptr ;
    begin
      Result := DLLHandle.FGDB_gettable( _instance, TEncoding.UTF8.GetString(_name) ) ;
    end;

    function T_FGDBLib.FGDB_findspatialref  ( _instance : tgdbptr ;
                                    _srid     : Integer
                                  ) : tgdbptr ;
    begin
      Result := DLLHandle.FGDB_findspatialref( _instance, _srid ) ;
    end;

    function T_FGDBLib.FGDB_table_createfield   ( _tableh : tgdbptr ;
                                        _defn   : TBytes
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_createfield( _tableh, TEncoding.UTF8.GetString(_defn) ) ;
    end;

    function T_FGDBLib.FGDB_table_deletefield   ( _tableh : tgdbptr ;
                                        _field  : TBytes
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_deletefield( _tableh, TEncoding.UTF8.GetString(_field) ) ;
    end;

    function T_FGDBLib.FGDB_table_createrow   ( _tableh : tgdbptr
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_createrow( _tableh ) ;
    end;

    function T_FGDBLib.FGDB_table_insertrow   ( _tableh : tgdbptr
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_insertrow( _tableh ) ;
    end;

    function T_FGDBLib.FGDB_table_getlastrowoid   ( _tableh : tgdbptr ;
                                          var _oid : Integer
                                        ) : Integer ;
    var
      oid : com.sun.jna.ptr.IntByReference ;
    begin
      oid := new com.sun.jna.ptr.IntByReference() ;

      Result := DLLHandle.FGDB_table_getlastrowoid( _tableh, oid ) ;
      _oid := oid.Value ;
    end;

    function T_FGDBLib.FGDB_table_getrow      ( _tableh : tgdbptr ;
                                      _query  : TBytes
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_getrow( _tableh, TEncoding.UTF8.GetString(_query) ) ;
    end;

    function T_FGDBLib.FGDB_table_updaterow    ( _tableh : tgdbptr
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_updaterow( _tableh ) ;
    end;

    function T_FGDBLib.FGDB_table_deleterow    ( _tableh : tgdbptr
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_deleterow( _tableh ) ;
    end;

    function T_FGDBLib.FGDB_table_starttransaction    ( _tableh : tgdbptr
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_starttransaction( _tableh ) ;
    end;

    function T_FGDBLib.FGDB_table_endtransaction    ( _tableh : tgdbptr
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_endtransaction( _tableh ) ;
    end;

    function T_FGDBLib.FGDB_table_close   ( _tableh : tgdbptr
                                      ) : Integer ;
    begin
      Result := DLLHandle.FGDB_table_close( _tableh ) ;
    end;

    function T_FGDBLib.FGDB_row_setinteger    ( _tableh  : tgdbptr ;
                                      _field   : TBytes ;
                                      _value   : Integer
                                    ) : Integer ;
    begin
      Result := DLLHandle.FGDB_row_setinteger( _tableh, TEncoding.UTF8.GetString(_field), _value ) ;
    end;

    function T_FGDBLib.FGDB_row_setfloat    ( _tableh  : tgdbptr ;
                                    _field   : TBytes ;
                                    _value   : Single
                                  ) : Integer ;
    begin
      Result := DLLHandle.FGDB_row_setfloat( _tableh, TEncoding.UTF8.GetString(_field), _value ) ;
    end;

    function T_FGDBLib.FGDB_row_setdouble   ( _tableh  : tgdbptr ;
                                    _field   : TBytes ;
                                    _value   : Double
                                  ) : Integer ;
    begin
      Result := DLLHandle.FGDB_row_setdouble( _tableh, TEncoding.UTF8.GetString(_field), _value ) ;
    end;

    function T_FGDBLib.FGDB_row_setstring   ( _tableh  : tgdbptr ;
                                    _field   : TBytes ;
                                    _value   : TBytes
                                  ) : Integer ;
    begin
      Result := DLLHandle.FGDB_row_setstring( _tableh, TEncoding.UTF8.GetString(_field), TEncoding.UTF8.GetString(_value) ) ;
    end;

    function T_FGDBLib.FGDB_row_setdatetime   ( _tableh  : tgdbptr ;
                                      _field   : TBytes ;
                                      _value   : TBytes
                                    ) : Integer ;
    begin
      Result := DLLHandle.FGDB_row_setdatetime( _tableh, TEncoding.UTF8.GetString(_field), TEncoding.UTF8.GetString(_value) ) ;
    end;

    function T_FGDBLib.FGDB_row_setnull   ( _tableh  : tgdbptr ;
                                  _field   : TBytes
                                ) : Integer ;
    begin
      Result := DLLHandle.FGDB_row_setnull( _tableh, TEncoding.UTF8.GetString(_field) ) ;
    end;

    function T_FGDBLib.FGDB_row_setgeometry   ( _tableh  : tgdbptr ;
                                      _value   : tgdbptr ;
                                      _size    : Integer
                                    ) : Integer ;
    begin
      Result := DLLHandle.FGDB_row_setgeometry( _tableh, _value, _size ) ;
    end;

    procedure T_FGDBLib.FGDB_cursor_open( _layerh   : tgdbptr ;
                                _cursor   : Integer
                              )  ;
    begin
      DLLHandle.FGDB_cursor_open( _layerh, _cursor ) ;
    end;

    procedure T_FGDBLib.FGDB_cursor_close( _layerh   : tgdbptr ;
                                  _cursor   : Integer
                                )  ;
    begin
      DLLHandle.FGDB_cursor_close( _layerh, _cursor ) ;
    end;

    procedure T_FGDBLib.FGDB_cursor_first( _layerh : tgdbptr ;
                                  _cursor : Integer ;
                                  _xmin   : Double ;
                                  _xmax   : Double ;
                                  _ymin   : Double ;
                                  _ymax   : Double ;
                                  _query  : TBytes
                              )  ;
    begin
      DLLHandle.FGDB_cursor_first( _layerh, _cursor, _xmin, _xmax, _ymin, _ymax, TEncoding.UTF8.GetString(_query) ) ;
    end;

    procedure T_FGDBLib.FGDB_cursor_next( _layerh   : tgdbptr ;
                                _cursor   : Integer
                              )  ;
    begin
      DLLHandle.FGDB_cursor_next( _layerh, _cursor ) ;
    end;

    function T_FGDBLib.FGDB_cursor_eof( _layerh   : tgdbptr ;
                              _cursor   : Integer
                              ) : Integer ;
    begin
       Result := DLLHandle.FGDB_cursor_eof( _layerh, _cursor ) ;
    end;

    function T_FGDBLib.FGDB_cursor_shape( _layerh   : tgdbptr ;
                                _cursor   : Integer ;
                                var _size : Integer
                              ) : tgdbptr ;
    var
      size : com.sun.jna.ptr.IntByReference ;
    begin
      size := new com.sun.jna.ptr.IntByReference() ;

      Result := DLLHandle.FGDB_cursor_shape( _layerh, _cursor, size ) ;
      _size := size.Value ;
    end;

    function T_FGDBLib.FGDB_row_getoid( _layerh   : tgdbptr ;
                              _cursor   : Integer
                              ) : Integer ;
    begin
       Result := DLLHandle.FGDB_row_getoid( _layerh, _cursor ) ;
    end;

    function T_FGDBLib.FGDB_row_getinteger( _layerh  : tgdbptr ;
                                  _cursor  : Integer ;
                                  _field   : TBytes
                                ) : Integer ;
    begin
       Result := DLLHandle.FGDB_row_getinteger( _layerh, _cursor, TEncoding.UTF8.GetString(_field) ) ;
    end;

    function T_FGDBLib.FGDB_row_getfloat( _layerh  : tgdbptr ;
                                _cursor  : Integer ;
                                _field   : TBytes
                              ) : Single ;
    begin
       Result := DLLHandle.FGDB_row_getfloat( _layerh, _cursor, TEncoding.UTF8.GetString(_field) ) ;
    end;

    function T_FGDBLib.FGDB_row_getdouble( _layerh  : tgdbptr ;
                                  _cursor  : Integer ;
                                  _field   : TBytes
                                ) : Double ;
    begin
       Result := DLLHandle.FGDB_row_getdouble( _layerh, _cursor, TEncoding.UTF8.GetString(_field) ) ;
    end;

    function T_FGDBLib.FGDB_row_getstring( _layerh  : tgdbptr ;
                                  _cursor  : Integer ;
                                  _field   : TBytes ;
                                  _size    : Integer
                                ) : tgdbptr ;
    begin
       Result := DLLHandle.FGDB_row_getstring( _layerh, _cursor, TEncoding.UTF8.GetString(_field), _size ) ;
    end;

    function T_FGDBLib.FGDB_row_getdatetime( _layerh  : tgdbptr ;
                                    _cursor  : Integer ;
                                    _field   : TBytes
                                  ) : tgdbptr ;
    begin
       Result := DLLHandle.FGDB_row_getdatetime( _layerh, _cursor, TEncoding.UTF8.GetString(_field) ) ;
    end;

    function T_FGDBLib.FGDB_row_getnull( _layerh  : tgdbptr ;
                                    _cursor  : Integer ;
                                    _field   : TBytes
                                  ) : Integer ;
    begin
       Result := DLLHandle.FGDB_row_getnull( _layerh, _cursor, TEncoding.UTF8.GetString(_field) ) ;
    end;

  {$ENDIF}

//==================================== END =====================================
end.

