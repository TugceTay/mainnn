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
  Encapsulation of a OGR file access.

  You can treat this as an sample of how to add native support to other
  GIS file formats.
}

{$IFDEF DCC}
  unit GisLayerOGR ;
  {$HPPEMIT '#pragma link "GisLayerOGR"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.IO,
    System.Security,
    System.Runtime.InteropServices,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.SysUtils,
    System.Variants,
    System.Classes,

    GisTypes,
    GisLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerOGR = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  {$IFDEF OXYGENE}
    T_cursorOgr nested in TGIS_LayerOGR = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public
        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record-by-record access.
        /// </summary>
        currShape       : TGIS_Shape ;

        /// <summary>
        ///   Pre-allocated shape. Recreating the shape on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currPoint       : TGIS_ShapePoint ;

        /// <summary>
        ///   Pre-allocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currArc         : TGIS_ShapeArc ;

        /// <summary>
        ///   Pre-allocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currPolygon     : TGIS_ShapePolygon ;

        /// <summary>
        ///   Pre-allocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currMultipoint  : TGIS_ShapeMultiPoint  ;

        /// <summary>
        ///   Pre-allocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currComplex  : TGIS_ShapeComplex  ;
    end ;
  {$ENDIF}

  {#gendoc:hide}
  {$IFDEF JAVA}
    OGRPointerH  = com.sun.jna.Pointer ;
  {$ELSE}
    OGRPointerH  = IntPtr ;
  {$ENDIF}

  /// <summary>
  ///   Layer that can read OGR files.
  /// </summary>
  TGIS_LayerOGR = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private

        /// <summary>
        ///   Last UID in a shape file. If not set then -1.
        /// </summary>
        lastUid         : TGIS_Uid ;
    private
    {$IFDEF OXYGENE}
      cursorOgr : array of T_cursorOgr ;
    {$ELSE}
      cursorOgr : array of record
        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record-by-record access.
        /// </summary>
        currShape       : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating the shape on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currPoint       : TGIS_ShapePoint ;

        /// <summary>
        ///   Pre-allocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currArc         : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currPolygon     : TGIS_ShapePolygon ;

        /// <summary>
        ///   Pre-allocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currMultipoint  : TGIS_ShapeMultiPoint  ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currComplex     : TGIS_ShapeComplex ;

      end ;
    {$ENDIF}
    protected // other protected functions
      /// <summary>
      ///   Internal pointer to library handles.
      /// </summary>
      hDS           : OGRPointerH  ;
      /// <summary>
      ///   Internal pointer to library handles.
      /// </summary>
      pahDriver     : OGRPointerH ;
      /// <summary>
      ///   Internal pointer to library handles.
      /// </summary>
      poLayer       : OGRPointerH ;
      /// <summary>
      ///   Internal pointer to library handles.
      /// </summary>
      poFeature     : OGRPointerH ;
      /// <summary>
      ///   Internal pointer to library handles.
      /// </summary>
      poGeometry    : OGRPointerH ;
      /// <summary>
      ///   Internal pointer to library handles.
      /// </summary>
      poFeatureDef  : OGRPointerH ;
      /// <summary>
      ///   Internal pointer to library handles.
      /// </summary>
      poFieldDef    : OGRPointerH ;
      /// <summary>
      ///   Internal pointer to library handles.
      /// </summary>
      poFieldType   : Integer ;
      /// <summary>
      ///   Internal pointer to library handles.
      /// </summary>
      poLayerSR     : OGRPointerH ;
      /// <summary>
      ///   GDAL DLL path.
      /// </summary>
      FDLLPath      : String ;
      /// <summary>
      ///   GDAL SQL path.
      /// </summary>
      FSQLPath      : String ;
    private // private methods

      /// <summary>
      ///   Parse geometry.
      /// </summary>
      procedure parseGeometry ( const _uid       : TGIS_Uid ;
                                const _cursor    : Integer
                               ) ;

      /// <summary>
      ///   Find driver name by extension.
      /// </summary>
      /// <returns>
      ///   Driver name.
      /// </returns>
      function  findDriver    ( const _extension : String
                               ) : String ;


      /// <summary>
      ///   Shape type to ogr type.
      /// </summary>
      /// <param name="_type">
      ///   ogr type
      /// </param>
      /// <returns>
      ///   Shape type.
      /// </returns>
      function  shpTypeToOgr  ( const _type      : TGIS_ShapeType
                               ) : Integer ;

      /// <summary>
      ///   Ogr type to Shape type.
      /// </summary>
      /// <param name="_type">
      ///   ogr type
      /// </param>
      /// <returns>
      ///   Shape type.
      /// </returns>
      function  ogrTypeToShp  ( const _type      : Integer
                               ) : TGIS_ShapeType ;

      /// <summary>
      ///   Parse configuration file.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      /// <param name="_params">
      ///   read parameters
      /// </param>
      procedure parseConfig   ( const _path      : String ;
                                const _params    : TStrings
                               ) ;

      /// <summary>
      ///   Process tokens in a SQLParameters property.
      /// </summary>
      function  passwordCallBack      ( const _token  : String
                                       ) : String ;

      /// <summary>
      ///   Get last error message.
      /// </summary>
      function  getError      : String ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

       procedure fset_UseRTree           ( const _value : Boolean
                                       ) ; override;

       /// <inheritdoc/>
       function  getFieldInternal      ( const _uid      : TGIS_Uid;
                                         const _name     : String ;
                                         const _cursor   : Integer
                                       ) : Variant ; override;

      /// <inheritdoc/>
      function  getBindedFieldInternal( const _shape   : TObject ;
                                        const _field   : Integer ;
                                        const _cursor  : Integer
                                      ) : Variant ; override;

       /// <inheritdoc/>
       procedure setUp                 ; override;

       /// <inheritdoc/>
       function  cursorOpen :  Integer ; override;

       /// <inheritdoc/>
       procedure cursorClose( const _cursor      : Integer
                             ) ; override;

       /// <inheritdoc/>
       procedure cursorFirst( const _cursor      : Integer          ;
                              const _viewerCS    : Boolean          ;
                              const _extent      : TGIS_Extent      ;
                              const _query       : String           ;
                              const _shape       : TGIS_Shape       ;
                              const _de9im       : String           ;
                              const _skipDeleted : Boolean
                            ) ; override;

       /// <inheritdoc/>
       procedure cursorNext ( const _cursor      : Integer
                            ) ; override;

       /// <inheritdoc/>
       function  cursorEof  ( const _cursor      : Integer
                            ) : Boolean ; override;

       /// <inheritdoc/>
       function  cursorShape( const _cursor      : Integer
                            ) : TGIS_Shape ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;

    public

      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  PreRecognize    ( const _path     : String ;
                                   var _new_path  : String
                                ) : Boolean ; override;

      // shape access function(s)

      /// <inheritdoc/>
      function  GetShape   ( const _uid    : TGIS_Uid ;
                            const _cursor : Integer
                          ) : TGIS_Shape ; override;

      /// <inheritdoc/>
      function  GetLastUid : TGIS_Uid ; override;
      // procedures and functions

      /// <inheritdoc/>
      procedure Build       ( const _path      : String ;
                              const _extent    : TGIS_Extent      ;
                              const _type      : TGIS_ShapeType   ;
                              const _dim       : TGIS_DimensionType
                            ) ; override;

      /// <inheritdoc/>
      procedure ImportLayerEx( const _layer       : TGIS_LayerVector  ;
                               const _extent      : TGIS_Extent       ;
                               const _type        : TGIS_ShapeType    ;
                               const _scope       : String            ;
                               const _shape       : TGIS_Shape        ;
                               const _de9im       : String            ;
                               const _truncated   : Boolean
                             ) ; override;

     /// <inheritdoc/>
     function GetAvailableLayers : TGIS_LayerInfoList ; override;

     /// <inheritdoc/>
     procedure RevertShapes ; override;
    public

      /// <summary>
      ///   GDAL DLL path.
      /// </summary>
      property DLLPath : String read FDLLPath write FDLLPath ;

      /// <summary>
      ///   GDAL SQL path.
      /// </summary>
      property SQLPath : String read FSQLPath write FSQLPath ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.SyncObjs,
    GisRtl,
    GisLogger,
    GisClasses,
    GisResource,
    GisFunctions,
    GisLayer,
    GisInternals,
    GisRegistredLayers,
    GisGeometryFactory ;
{$ENDIF}

const
  GDAL_DEFAULT_DLL_NAME_OGR       = 'gdal300.dll' ;
  GDAL_DEFAULT_DLL_NAME_OGR_NOEXT = 'gdal300' ;

type
    {$IFDEF JAVA}
    OGRGeometryH                  = com.sun.jna.Pointer ;
    OGRSpatialReferenceH          = com.sun.jna.Pointer ;

    OGRFieldDefnH                 = com.sun.jna.Pointer ;
    OGRFeatureDefnH               = com.sun.jna.Pointer ;
    OGRFeatureH                   = com.sun.jna.Pointer ;

    OGRLayerH                     = com.sun.jna.Pointer ;
    OGRDataSourceH                = com.sun.jna.Pointer ;
    OGRSFDriverH                  = com.sun.jna.Pointer ;
    {$ELSE}
    OGRGeometryH                  = IntPtr ;
    OGRSpatialReferenceH          = IntPtr ;

    OGRFieldDefnH                 = IntPtr ;
    OGRFeatureDefnH               = IntPtr ;
    OGRFeatureH                   = IntPtr ;

    OGRLayerH                     = IntPtr ;
    OGRDataSourceH                = IntPtr ;
    OGRSFDriverH                  = IntPtr ;
    {$ENDIF}


  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
  {$ENDIF}
    OGREnvelope = record
      MinX  : Double;
      MaxX  : Double;
      MinY  : Double;
      MaxY  : Double;
    end ;

  OGRErr              = Integer ;

  OGRwkbGeometryType  = Integer ;
  OGRwkbByteOrder     = Integer ;
  OGRFieldType        = Integer ;

  {$IFDEF JAVA}
    IOGRLibrary = interface (com.sun.jna.Library)

      function OGRRegisterAll : Integer ;
      function OGR_G_WkbSize ( _hGeom : OGRGeometryH
                               ) : Integer ;
      function OGR_G_ExportToWkb( _phGeometry     : OGRGeometryH ;
                                   _eOrder         : OGRwkbByteOrder;
                                   _pabyDstBuffer  : TBytes
                                   ) : OGRErr ;
      function OGR_L_GetExtent( _hLayer   : OGRLayerH ;
                                 _psExtent : OGRPointerH;
                                 _bForce   : Integer
                                 ) : OGRErr ;
      function OGR_L_GetFeature( _hLayer     : OGRLayerH ;
                                  _nFeatureId : Int64
                                 ) : OGRFeatureH ;
      function OGR_L_GetGeomType( _hLayer     : OGRLayerH
                                  ) : OGRwkbGeometryType ;
      function OGR_F_GetFieldIndex( _hFeat : OGRFeatureH ;
                                     _name  : String
                                    ) : Integer ;
      function OGR_F_GetFieldAsString( _hFeat : OGRFeatureH ;
                                        _index : Integer
                                       ) : String ;
      function OGR_F_GetFieldAsDouble( _hFeat : OGRFeatureH ;
                                        _index : Integer
                                       ) : Double ;
      function OGR_F_GetFieldAsInteger( _hFeat : OGRFeatureH ;
                                         _index : Integer
                                       ) : Integer ;
      function OGR_F_GetFID( _hFeat : OGRFeatureH
                            ) : Int64 ;
      function OGROpen( _pszName        : String;
                          _bUpdate        : Integer;
                             _pahDriverList  : com.sun.jna.ptr.PointerByReference
                         ) : OGRDataSourceH ;
      function OGR_DS_GetLayerCount( _hDS : OGRDataSourceH
                                      ): Integer ;
      function OGR_DS_GetLayer( _hDS     : OGRDataSourceH ;
                                 _iLayer  : Integer
                                ) : OGRLayerH ;
      function OGR_DS_GetLayerByName( _hDS     : OGRDataSourceH ;
                                       _psName  : String
                                     ) : OGRLayerH ;
      function OGR_L_GetLayerDefn( _hLayer : OGRLayerH
                                    ) : OGRFeatureDefnH ;
      function OGR_L_GetName( _hLayer : OGRLayerH
                              ) : String ;
      function OGR_FD_GetFieldCount( _hDefn : OGRFeatureDefnH
                                      ) : Integer ;
      function OGR_FD_GetFieldDefn( _hDefn  : OGRFeatureDefnH ;
                                      _iField : Integer
                                     ) : OGRFieldDefnH ;
      function OGR_Fld_GetNameRef( _hDefn : OGRFieldDefnH
                                    ) : String ;

      function OGR_Fld_GetType( _hDefn : OGRFieldDefnH
                                  ) : OGRFieldType ;

      function OGR_Fld_GetWidth( _hDefn : OGRFieldDefnH
                                  ) : Integer ;
      function OGR_Fld_GetPrecision( _hDefn : OGRFieldDefnH
                                      ) : Integer ;
      function OGR_L_GetSpatialRef( _hLayer : OGRLayerH
                                     ) : OGRSpatialReferenceH ;
      function OGR_Dr_GetName( hDriver : OGRSFDriverH
                                ) : String ;
      procedure OGR_L_ResetReading( _hLayer : OGRLayerH
                                    ) ;
      procedure OGR_L_SetSpatialFilterRect( _hLayer : OGRLayerH ;
                                             _minX   : Double ;
                                             _minY   : Double ;
                                             _maxX   : Double ;
                                             _maxY   : Double
                                            ) ;
      function OGR_L_GetNextFeature( _hLayer : OGRLayerH
                                      ) : OGRFeatureH ;
      function OGR_F_GetGeometryRef( _hFeat : OGRFeatureH
                                      ) : OGRGeometryH ;
      procedure OGR_F_Destroy( _hFeat : OGRFeatureH
                                ) ;
      procedure OGR_DS_Destroy( _hDS : OGRDataSourceH
                                 )  ;
      procedure OGRCleanupAll ;
      function OSRExportToWkt( _hSRS    : OGRSpatialReferenceH ;
                                   _ppszWkt  : OGRPointerH
                                  ) : OGRErr  ;
      function OGR_Dr_TestCapability( _hDriver : OGRSFDriverH ;
                                       _pszCap  : String
                                      ) : Integer ;
      function OGR_Dr_CreateDataSource( _hDriver       : OGRSFDriverH ;
                                         _pszName       : String ;
                                         _papszOptions  : String
                                        ) : OGRDataSourceH ;
      function OGRReleaseDataSource( _hDS : OGRDataSourceH
                                        ) : OGRErr ;
      function OGRGetDriverCount : Integer ;

      function OGRGetDriver( _iDriver : Integer
                                ) : OGRSFDriverH ;
      function OGRGetDriverByName( _pszName : String
                                    ) : OGRSFDriverH ;
      function OGR_DS_CreateLayer( _hDS          : OGRDataSourceH ;
                                    _pszName      : String;
                                    _hSpatialRef  : OGRSpatialReferenceH ;
                                    _eType        : OGRwkbGeometryType ;
                                    _papszOptions : String
                                    ) : OGRLayerH ;
      function OGR_F_Create( _hDefn : OGRFeatureDefnH
                               ) : OGRFeatureH ;
      function OGR_F_SetGeometry( _hFeat : OGRFeatureH ;
                                     _hGeom : OGRGeometryH
                                    ) : OGRErr ;
      function OGR_L_CreateFeature( _hLayer : OGRLayerH ;
                                       _hFeat  : OGRFeatureH
                                     ) : OGRErr ;
      function OGR_Fld_Create( _pszName : String ;
                                  _eType   : OGRFieldType
                                ) : OGRFieldDefnH ;
      procedure OGR_Fld_Set( _hDefn      : OGRFieldDefnH ;
                                _pszNameIn  : String ;
                                _eTypeIn    : OGRFieldType ;
                                _nWidthIn   : Integer ;
                                _nPrecision : Integer ;
                                _eJustifyIn : Integer
                               ) ;
      function OGR_L_CreateField( _hLayer     : OGRLayerH;
                                    _hField     : OGRFieldDefnH;
                                    _bApproxOK  : Integer
                                   ) : OGRErr ;
      function OGR_G_CreateFromWkb( _pabyData       : OGRPointerH ;
                                       _hSRS           : OGRSpatialReferenceH ;
                                        _phGeometry : OGRGeometryH ;
                                       _nBytes         : Integer
                                      ) : OGRErr ;
      procedure OGR_F_SetFieldInteger( _hFeat   : OGRFeatureH ;
                                          _iField  : Integer ;
                                          _nValue  : Integer
                                         ) ;
      procedure OGR_F_SetFieldDouble( _hFeat   : OGRFeatureH ;
                                         _iField  : Integer ;
                                         _dfValue : Double
                                        ) ;
      procedure OGR_F_SetFieldString( _hFeat   : OGRFeatureH ;
                                         _iField  : Integer ;
                                         _pszValue: String
                                        )  ;
      procedure OGR_F_SetFieldDateTime( _hFeat    : OGRFeatureH ;
                                           _iField   : Integer ;
                                           _nYear    : Integer ;
                                           _nMonth   : Integer ;
                                           _nDay     : Integer ;
                                           _nHour    : Integer ;
                                           _nMinute  : Integer ;
                                           _nSecond  : Integer ;
                                           _nTZFlag  : Integer
                                          )  ;
      function OSRNewSpatialReference( _pszWKT : String
                                          ) : OGRSpatialReferenceH ;
      procedure OSRDestroySpatialReference( _hSRS : OGRSpatialReferenceH
                                               )   ;
      function CPLGetLastErrorMsg : String ;
    end;

    T_OGRMapper = class( com.sun.jna.win32.StdCallFunctionMapper )
      public
        method getFunctionName(&library: com.sun.jna.NativeLibrary; &method: &Method): String; override;
    end ;
  {$ENDIF}

const
    OFTInteger          =  0 ;
    OFTIntegerList      =  1 ;
    OFTReal             =  2 ;
    OFTRealList         =  3 ;
    OFTString           =  4 ;
    OFTStringList       =  5 ;
    OFTWideString       =  6 ;
    OFTWideStringList   =  7 ;
    OFTBinary           =  8 ;
    OFTDate             =  9 ;
    OFTTime             = 10 ;
    OFTDateTime         = 11 ;
    OFTInteger64        = 12 ;
    OFTInteger64List    = 13 ;

    OGRERR_NONE         = 0 ;
    OGRERR_DEBUG        = 1 ;
    OGRERR_WARNING      = 2 ;
    OGRERR_FAILURE      = 3 ;
    OGRERR_FATAL        = 4 ;

    wkbNDR              = 1 ;
    wkb25DBit           = $80000000 ;

var
  {$IFDEF OXYGENE}
    _ogr_G_WkbSizeProc                : IntPtr ;
    _ogr_G_ExportToWkbProc            : IntPtr ;
    _ogr_L_GetExtentProc              : IntPtr ;
    _ogr_L_GetFeatureProc             : IntPtr ;
    _ogr_L_GetGeomTypeProc            : IntPtr ;
    _ogr_F_GetFieldIndexProc          : IntPtr ;
    _ogr_F_GetFieldAsStringProc       : IntPtr ;
    _ogr_F_GetFieldAsIntegerProc      : IntPtr ;
    _ogr_F_GetFieldAsDoubleProc       : IntPtr ;
    _ogr_F_GetFIDProc                 : IntPtr ;
    _ogr_RegisterAllProc              : IntPtr ;
    _ogr_OpenProc                     : IntPtr ;
    _ogr_DS_GetLayerCountProc         : IntPtr ;
    _ogr_DS_GetLayerProc              : IntPtr ;
    _ogr_DS_GetLayerByNameProc        : IntPtr ;
    _ogr_L_GetLayerDefnProc           : IntPtr ;
    _ogr_L_GetNameProc                : IntPtr ;
    _ogr_FD_GetFieldCountProc         : IntPtr ;
    _ogr_FD_GetFieldDefnProc          : IntPtr ;
    _ogr_Fld_GetNameRefProc           : IntPtr ;
    _ogr_Fld_GetTypeProc              : IntPtr ;
    _ogr_Fld_GetWidthProc             : IntPtr ;
    _ogr_Fld_GetPrecisionProc         : IntPtr ;
    _ogr_L_GetSpatialRefProc          : IntPtr ;
    _ogr_Dr_GetNameProc               : IntPtr ;
    _ogr_L_ResetReadingProc           : IntPtr ;
    _ogr_L_SetSpatialFilterRectProc   : IntPtr ;
    _ogr_L_GetNextFeatureProc         : IntPtr ;
    _ogr_F_GetGeometryRefProc         : IntPtr ;
    _ogr_F_DestroyProc                : IntPtr ;
    _ogr_DS_DestroyProc               : IntPtr ;
    _ogr_CleanupAllProc               : IntPtr ;
    _osr_ExportToWktProc              : IntPtr ;
    _ogr_Dr_OpenProc                  : IntPtr ;
    _ogr_Dr_TestCapabilityProc        : IntPtr ;
    _ogr_Dr_CreateDataSourceProc      : IntPtr ;
    _ogr_ReleaseDatasourceProc        : IntPtr ;
    _ogr_GetDriverCountProc           : IntPtr ;
    _ogr_GetDriverProc                : IntPtr ;
    _ogr_GetDriverByNameProc          : IntPtr ;
    _ogr_DS_CreateLayerProc           : IntPtr ;
    _ogr_F_CreateProc                 : IntPtr ;
    _ogr_F_SetGeometryProc            : IntPtr ;
    _ogr_L_CreateFeatureProc          : IntPtr ;
    _ogr_Fld_CreateProc               : IntPtr ;
    _ogr_Fld_SetProc                  : IntPtr ;
    _ogr_L_CreateFieldProc            : IntPtr ;
    _ogr_G_CreateFromWkbProc          : IntPtr ;
    _ogr_F_SetFieldIntegerProc        : IntPtr ;
    _ogr_F_SetFieldDoubleProc         : IntPtr ;
    _ogr_F_SetFieldStringProc         : IntPtr ;
    _ogr_F_SetFieldDateTimeProc       : IntPtr ;
    _osr_NewSpatialReferenceProc      : IntPtr ;
    _osr_DestroySpatialReferenceProc  : IntPtr ;
    _cpl_GetLastErrorMsgProc          : IntPtr ;
  {$ELSE}
    // Returns size of related binary representation.
    // _hGeom   handle on the geometry to get the binary size from
    // return  size of binary representation in bytes.
    _ogr_G_WkbSize : function( _hGeom : OGRGeometryH
                             ) : Integer ; cdecl ;

    // Convert a geometry into well known binary format.
    // _hGeom           handle on the geometry to convert to a WKB data from.
    // _eOrder          One of wkbXDR or wkbNDR indicating MSB or LSB byte order.
    // _pabyDstBuffer   a buffer into which the binary representation is written.
    //                  This buffer must be at least _ogr_G_WkbSize() byte in size.
    // return           Currently OGRERR_NONE is always returned.
    _ogr_G_ExportToWkb : function( _phGeometry     : OGRGeometryH ;
                                   _eOrder         : OGRWKBBYTEORDER;
                                   _pabyDstBuffer  : IntPtr
                                 ) : OGRErr ; cdecl ;

    // Fetch the extent of this layer.
    // _hLayer     handle to the layer from which to get extent.
    // _psExtent   the structure in which the extent value will be returned.
    // _bForce     Flag indicating whether the extent should be computed even
    //             if it is expensive.
    // return      OGRERR_NONE on success, OGRERR_FAILURE if extent not known.
    _ogr_L_GetExtent : function( _hLayer   : OGRLayerH ;
                                  var _psExtent : OGRENVELOPE;
                                 _bForce   : Integer
                               ) : OGRErr ; cdecl ;

    // Fetch a feature by its identifier.
    // _hLayer       handle to the layer that owned the feature.
    // _nFeatureId  the feature id of the feature to read.
    // return       a handle to a feature now owned by the caller, or NULL.
    _ogr_L_GetFeature: function( _hLayer     : OGRLayerH ;
                                 _nFeatureId : Int64
                               ) : OGRFeatureH ; cdecl ;

    // Return the layer geometry type.
    // _hLayer       handle to the layer that owned the feature.
    // return       layer geometry type.
    _ogr_L_GetGeomType : function( _hLayer     : OGRLayerH
                                  ) : OGRwkbGeometryType ; cdecl ;

    // Fetch the field index given field name.
    _ogr_F_GetFieldIndex: function( _hFeat : OGRFeatureH ;
                                    _name  : PAnsiChar
                                  ) : Integer ; cdecl ;

    // Fetch field value as a String.
    _ogr_F_GetFieldAsString: function( _hFeat : OGRFeatureH ;
                                       _index : Integer
                                     ) : PAnsiChar ; cdecl ;

    // Fetch field value as a Double.
    _ogr_F_GetFieldAsDouble: function( _hFeat : OGRFeatureH ;
                                       _index : Integer
                                     ) : Double ; cdecl ;

    // Fetch field value as a Integer.
    _ogr_F_GetFieldAsInteger: function( _hFeat : OGRFeatureH ;
                                        _index : Integer
                                       ) : Integer ; cdecl ;

    // Get feature identifier.
    _ogr_F_GetFID          : function( _hFeat : OGRFeatureH
                                     ) : Int64; cdecl ;

    // Register all drivers.
    _ogr_RegisterAll : function : Integer ; cdecl ;

    // Open a file / data source with one of the registered drivers.
    // _pszName         the name of the file, or data source to open.
    // _bUpdate         FALSE for read-only access (the default) or TRUE for
    //                  read-write access.
    // _pahDriverList   if non-NULL, this argument will be updated with a
    //                  IntPtr to the driver which was used to open the data source.
    // return           NULL on error or if the pass name is not supported by
    //                  this driver, otherwise an handle to an OGRDataSource.
    //                  This OGRDataSource should be closed by deleting the
    //                  object when it is no longer needed.
    _ogr_Open : function( _pszName        : PAnsiChar;
                          _bUpdate        : Integer;
                          var _pahDriverList  : OGRSFDriverH
                       ) : OGRDataSourceH ; cdecl ;

    // Get the number of layers in this data source.
    _ogr_DS_GetLayerCount : function( _hDS : OGRDataSourceH
                                    ): Integer ; cdecl ;

    // Fetch a layer by index.
    // The returned layer remains owned by the OGRDataSource and should not be
    // deleted by the application.
    // _hDS      handle to the data source from which to get the layer.
    // _iLayer   a layer number between 0 and _ogr_DS_GetLayerCount()-1.
    // return    an handle to the layer, or NULL if iLayer is out of range
    //           or an error occurs.
    _ogr_DS_GetLayer: function( _hDS     : OGRDataSourceH ;
                                _iLayer  : Integer
                              ) : OGRLayerH ; cdecl ;

    // Fetch a layer by name.
    // The returned layer remains owned by the OGRDataSource and should not be
    // deleted by the application.
    // _hDS      handle to the data source from which to get the layer.
    // _pszName  a layer name.
    // return    an handle to the layer, or NULL if iLayer is out of range
    //           or an error occurs.
    _ogr_DS_GetLayerByName: function( _hDS     : OGRDataSourceH ;
                                      _pszName : PAnsiChar
                                     ) : OGRLayerH ; cdecl ;

    // Return the layer name.
    // _hLayer   handle to the layer
    // return   the layer name
    _ogr_L_GetName        : function( _hLayer : OGRLayerH
                                    ) : PAnsiChar ; cdecl ;

    // Fetch the schema information for this layer.
    _ogr_L_GetLayerDefn : function( _hLayer : OGRLayerH
                                  ) : OGRFeatureDefnH ; cdecl ;

    // Fetch number of fields on the passed feature definition.
    // _hDefn   handle to the feature definition.
    // return   count of fields.
    _ogr_FD_GetFieldCount : function( _hDefn : OGRFeatureDefnH
                                    ) : Integer ; cdecl ;

    // Fetch field definition of the passed feature definition.
    // _hDefn    handle to the feature definition.
    // _iField   the field to fetch, between 0 and GetFieldCount()-1.
    // return    an handle to an internal field definition object.
    //           This object should not be modified or freed by the application.
    _ogr_FD_GetFieldDefn : function( _hDefn  : OGRFeatureDefnH ;
                                     _iField : Integer
                                   ) : OGRFieldDefnH ; cdecl ;

    // Fetch name of this field.
    // _hDefn   handle to the field definition.
    _ogr_Fld_GetNameRef : function( _hDefn : OGRFieldDefnH
                                  ) : PAnsiChar ; cdecl ;

    // Fetch type of this field.
    // _hDefn   handle to the field definition.
    // return   field type.
    _ogr_Fld_GetType : function( _hDefn : OGRFieldDefnH
                                ) : OGRFIELDTYPE ; cdecl ;

    // Get the formatting width for this field.
    // _hDefn   handle to the field definition.
    // return   the width, zero means no specified width.
    _ogr_Fld_GetWidth : function( _hDefn : OGRFieldDefnH
                                ) : Integer ; cdecl ;

    // Get the formatting precision for this field.
    // This should normally be zero for fields of types other than OFTReal.
    // _hDefn   handle to the field definition.
    // return   the precision.
    _ogr_Fld_GetPrecision : function( _hDefn : OGRFieldDefnH
                                    ) : Integer ; cdecl ;

    // Fetch the spatial reference system for this layer.
    _ogr_L_GetSpatialRef : function( _hLayer : OGRLayerH
                                   ) : OGRSpatialReferenceH ; cdecl ;

    // Fetch name of driver (file format).
    _ogr_Dr_GetName : function( _hDriver : OGRSFDriverH
                              ) : PAnsiChar ; cdecl ;

    // Reset feature reading to start on the first feature.
    _ogr_L_ResetReading : procedure( _hLayer : OGRLayerH

                                  ) ; cdecl ;

    // Fetch the next available feature from this layer.
    // The returned feature becomes the responsibility of the caller to delete.
    // _hLayer   handle to the layer from which feature are read.
    // return    n handle to a feature, or NULL if no more features are available.
    _ogr_L_GetNextFeature : function( _hLayer : OGRLayerH
                                    ) : OGRFeatureH ; cdecl ;

    // Set spatial filter for the layer.
    _ogr_L_SetSpatialFilterRect : procedure( _hLayer : OGRLayerH ;
                                             _minX   : Double ;
                                             _minY   : Double ;
                                             _maxX   : Double ;
                                             _maxY   : Double
                                  ) ; cdecl ;

    // Fetch an handle to feature geometry.
    _ogr_F_GetGeometryRef : function( _hFeat : OGRFeatureH
                                    ) : OGRGeometryH ; cdecl ;

    // Destroy feature.
    _ogr_F_Destroy : procedure( _hFeat : OGRFeatureH
                              ) ; cdecl ;

    // Closes opened datasource and releases allocated resources.
    _ogr_DS_Destroy : procedure( _hDS : OGRDataSourceH
                               ) ; cdecl ;

    // Cleanup all OGR related resources.
    _ogr_CleanupAll : procedure ; cdecl ;

    // Free resource.
    _ogr_Free : procedure( _ptr : IntPtr
                          ) ; cdecl ;
    // Export to Wkt.
    _osr_ExportToWkt : function( _hSRS         : OGRSpatialReferenceH ;
                                 var _ppszWkt  : IntPtr
                                ) : OGRErr ; stdcall ;

    // Attempt to open file with this driver.
    _ogr_Dr_Open : function( _hDriver : OGRSFDriverH ;
                             _pszName : PAnsiChar ;
                             _bUpdate : Integer
                            ) : OGRDataSourceH ; cdecl ;

    // Test if capability is available.
    _ogr_Dr_TestCapability : function( _hDriver : OGRSFDriverH ;
                                       _pszCap  : PAnsiChar
                                      ) : Integer ; cdecl ;

    // This function attempts to create a new data source based on the passed
    // driver.
    _ogr_Dr_CreateDataSource : function( _hDriver       : OGRSFDriverH ;
                                         _pszName       : PAnsiChar ;
                                         _papszOptions  : PAnsiChar
                                        ) : OGRDataSourceH ; cdecl ;

    // Drop a reference to this datasource, and if the reference count drops
    // to zero close (destroy) the datasource
    _ogr_ReleaseDataSource : function( _hDS : OGRDataSourceH
                                      ) : OGRErr ; cdecl;

    // Fetch the number of registered drivers.
    _ogr_GetDriverCount : function : Integer ; cdecl;

    // Fetch the indicated driver.
    _ogr_GetDriver : function( _iDriver : Integer
                              ) : OGRSFDriverH ; cdecl;

    // Fetch the indicated driver.
    _ogr_GetDriverByName : function( _pszName : PAnsiChar
                                    ) : OGRSFDriverH ; cdecl;

    // This function attempts to create a new layer on the data source with
    // the indicated name, coordinate system, geometry type.
    _ogr_DS_CreateLayer : function( _hDS          : OGRDataSourceH ;
                                    _pszName      : PAnsiChar;
                                    _hSpatialRef  : OGRSpatialReferenceH ;
                                    _eType        : OGRwkbGeometryType ;
                                    _papszOptions : PAnsiChar
                                   ) : OGRLayerH ; cdecl ;

    // Feature factory
    _ogr_F_Create : function( _hDefn : OGRFeatureDefnH
                             ) : OGRFeatureH ; cdecl ;

    // Set feature geometry
    _ogr_F_SetGeometry : function( _hFeat : OGRFeatureH ;
                                   _hGeom : OGRGeometryH
                                  ) : OGRErr ; cdecl ;

    // Create and write a new feature within a layer
    _ogr_L_CreateFeature : function( _hLayer : OGRLayerH ;
                                     _hFeat  : OGRFeatureH
                                   ) : OGRErr ; cdecl ;

    // Create a new field definition
    _ogr_Fld_Create : function( _pszName : PAnsiChar ;
                                _eType   : OGRFieldType
                              ) : OGRFieldDefnH ; cdecl ;

    // Set defining parameters for a field in one call }
    _ogr_Fld_Set : procedure( _hDefn      : OGRFieldDefnH ;
                              _pszNameIn  : PAnsiChar ;
                              _eTypeIn    : OGRFieldType ;
                              _nWidthIn   : Integer ;
                              _nPrecision : Integer ;
                              _eJustifyIn : Integer
                             ) ; cdecl ;

    // Create a new field on a layer
    _ogr_L_CreateField : function( _hLayer     : OGRLayerH;
                                  _hField     : OGRFieldDefnH;
                                  _bApproxOK  : Integer
                                 ) : OGRErr ; cdecl ;

    // Create a geometry object of the appropriate type from it's well known binary
    // representation.
    _ogr_G_CreateFromWkb : function( _pabyData       : IntPtr ;
                                     _hSRS           : OGRSpatialReferenceH ;
                                     var _phGeometry : OGRGeometryH ;
                                     _nBytes         : Integer
                                    ) : OGRErr ; cdecl ;

    // Set field to integer value
    _ogr_F_SetFieldInteger : procedure( _hFeat   : OGRFeatureH ;
                                        _iField  : Integer ;
                                        _nValue  : Integer
                                       ) ; cdecl ;

    // Set field to double value
    _ogr_F_SetFieldDouble : procedure( _hFeat   : OGRFeatureH ;
                                       _iField  : Integer ;
                                       _dfValue : Double
                                      ) ; cdecl ;

    // Set field to string value
    _ogr_F_SetFieldString : procedure( _hFeat   : OGRFeatureH ;
                                       _iField  : Integer ;
                                       _pszValue: PAnsiChar
                                      ) ; cdecl ;

    // Set field to datetime
    _ogr_F_SetFieldDateTime : procedure( _hFeat    : OGRFeatureH ;
                                         _iField   : Integer ;
                                         _nYear    : Integer ;
                                         _nMonth   : Integer ;
                                         _nDay     : Integer ;
                                         _nHour    : Integer ;
                                         _nMinute  : Integer ;
                                         _nSecond  : Integer ;
                                         _nTZFlag  : Integer
                                        ) ; cdecl ;

    // Constructor
    _osr_NewSpatialReference : function( _pszWKT : PAnsiChar
                                        ) : OGRSpatialReferenceH ; stdcall ;

    // OGRSpatialReference destructor
    _osr_DestroySpatialReference : procedure( _hSRS : OGRSpatialReferenceH
                                             ) ; stdcall ;
    // Get last error message.
    _cpl_GetLastErrorMsg   : function : PAnsiChar ; stdcall ;

  {$ENDIF}


  {$IFDEF CLR}
    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGRRegisterAll'
                )
    ]
    function _ogr_RegisterAll : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_G_WkbSize'
                )
    ]
    function _ogr_G_WkbSize ( _hGeom : OGRGeometryH
                             ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_G_ExportToWkb'
                )
    ]
    function _ogr_G_ExportToWkb( _phGeometry     : OGRGeometryH ;
                                 _eOrder         : OGRwkbByteOrder;
                                 _pabyDstBuffer  : TBytes
                                 ) : OGRErr ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_GetExtent'
                )
    ]
    function _ogr_L_GetExtent( _hLayer   : OGRLayerH ;
                               _psExtent : IntPtr;
                               _bForce   : Integer
                               ) : OGRErr ;   external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_GetFeature'
                )
    ]
    function _ogr_L_GetFeature( _hLayer     : OGRLayerH ;
                                _nFeatureId : Int64
                               ) : OGRFeatureH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_GetGeomType'
                )
    ]
    function _ogr_L_GetGeomType( _hLayer     : OGRLayerH
                                ) : OGRwkbGeometryType ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_GetFieldIndex'
                )
    ]
    function _ogr_F_GetFieldIndex( _hFeat : OGRFeatureH ;
                                   _name  : String
                                  ) : Integer ;   external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_GetFieldAsString'
                )
    ]
    function _ogr_F_GetFieldAsString( _hFeat : OGRFeatureH ;
                                      _index : Integer
                                     ) : IntPtr ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_GetFieldAsDouble'
                )
    ]
    function _ogr_F_GetFieldAsDouble( _hFeat : OGRFeatureH ;
                                      _index : Integer
                                     ) : Double ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_GetFieldAsInteger'
                )
    ]
    function _ogr_F_GetFieldAsInteger( _hFeat : OGRFeatureH ;
                                       _index : Integer
                                     ) : Integer ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_GetFID'
                )
    ]
    function _ogr_F_GetFID( _hFeat : OGRFeatureH
                          ) : Int64 ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGROpen'
                )
    ]
    function _ogr_Open( _pszName        : String;
                        _bUpdate        : Integer;
                          var _pahDriverList  : OGRSFDriverH
                       ) : OGRDataSourceH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_DS_GetLayerCount'
                )
    ]
    function _ogr_DS_GetLayerCount( _hDS : OGRDataSourceH
                                    ): Integer ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_DS_GetLayer'
                )
    ]
    function _ogr_DS_GetLayer( _hDS     : OGRDataSourceH ;
                               _iLayer  : Integer
                              ) : OGRLayerH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_DS_GetLayerByName'
                )
    ]
    function _ogr_DS_GetLayerByName( _hDS     : OGRDataSourceH ;
                                     _psName  : String
                                   ) : OGRLayerH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_GetLayerDefn'
                )
    ]
    function _ogr_L_GetLayerDefn( _hLayer : OGRLayerH
                                  ) : OGRFeatureDefnH ;   external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_GetName'
                )
    ]
    function _ogr_L_GetName( _hLayer : OGRLayerH
                            ) : IntPtr ;   external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_FD_GetFieldCount'
                )
    ]
    function _ogr_FD_GetFieldCount( _hDefn : OGRFeatureDefnH
                                    ) : Integer ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_FD_GetFieldDefn'
                )
    ]
    function _ogr_FD_GetFieldDefn( _hDefn  : OGRFeatureDefnH ;
                                    _iField : Integer
                                   ) : OGRFieldDefnH ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_Fld_GetNameRef'
                )
    ]
    function _ogr_Fld_GetNameRef( _hDefn : OGRFieldDefnH
                                  ) : IntPtr ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_Fld_GetType'
                )
    ]
    function _ogr_Fld_GetType( _hDefn : OGRFieldDefnH
                                ) : OGRFieldType ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_Fld_GetWidth'
                )
    ]
    function _ogr_Fld_GetWidth( _hDefn : OGRFieldDefnH
                                ) : Integer ;   external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_Fld_GetPrecision'
                )
    ]
    function _ogr_Fld_GetPrecision( _hDefn : OGRFieldDefnH
                                    ) : Integer ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_GetSpatialRef'
                )
    ]
    function _ogr_L_GetSpatialRef( _hLayer : OGRLayerH
                                   ) : OGRSpatialReferenceH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_Dr_GetName'
                )
    ]
    function _ogr_Dr_GetName( hDriver : OGRSFDriverH
                              ) : IntPtr ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_ResetReading'
                )
    ]
    procedure _ogr_L_ResetReading( _hLayer : OGRLayerH
                                  ) ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_SetSpatialFilterRect'
                )
    ]
    procedure _ogr_L_SetSpatialFilterRect( _hLayer : OGRLayerH ;
                                           _minX   : Double ;
                                           _minY   : Double ;
                                           _maxX   : Double ;
                                           _maxY   : Double
                                          ) ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_GetNextFeature'
                )
    ]
    function _ogr_L_GetNextFeature( _hLayer : OGRLayerH
                                    ) : OGRFeatureH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_GetGeometryRef'
                )
    ]
    function _ogr_F_GetGeometryRef( _hFeat : OGRFeatureH
                                    ) : OGRGeometryH ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_Destroy'
                )
    ]
    procedure _ogr_F_Destroy( _hFeat : OGRFeatureH
                              ) ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_DS_Destroy'
                )
    ]
    procedure _ogr_DS_Destroy( _hDS : OGRDataSourceH
                               )  ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGRCleanupAll'
                )
    ]
    procedure _ogr_CleanupAll ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.StdCall,
                 CharSet = CharSet.Ansi, EntryPoint = '_OSRExportToWkt@8'
                )
    ]
    function _osr_ExportToWkt_32( _hSRS    : OGRSpatialReferenceH ;
                                var _ppszWkt  : IntPtr
                                ) : OGRErr  ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.StdCall,
                 CharSet = CharSet.Ansi, EntryPoint = 'OSRExportToWkt'
                )
    ]
    function _osr_ExportToWkt_64( _hSRS    : OGRSpatialReferenceH ;
                                var _ppszWkt  : IntPtr
                                ) : OGRErr  ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_Dr_TestCapability'
                )
    ]
    function _ogr_Dr_TestCapability( _hDriver : OGRSFDriverH ;
                                     _pszCap  : String
                                    ) : Integer ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_Dr_CreateDataSource'
                )
    ]
    function _ogr_Dr_CreateDataSource( _hDriver       : OGRSFDriverH ;
                                       _pszName       : String ;
                                       _papszOptions  : String
                                      ) : OGRDataSourceH ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGRReleaseDataSource'
                )
    ]
    function _ogr_ReleaseDataSource( _hDS : OGRDataSourceH
                                      ) : OGRErr ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGRGetDriverCount'
                )
    ]
    function _ogr_GetDriverCount : Integer ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGRGetDriver'
                )
    ]
    function _ogr_GetDriver( _iDriver : Integer
                              ) : OGRSFDriverH ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGRGetDriverByName'
                )
    ]
    function _ogr_GetDriverByName( _pszName : String
                                  ) : OGRSFDriverH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_DS_CreateLayer'
                )
    ]
    function _ogr_DS_CreateLayer( _hDS          : OGRDataSourceH ;
                                  _pszName      : String;
                                  _hSpatialRef  : OGRSpatialReferenceH ;
                                  _eType        : OGRwkbGeometryType ;
                                  _papszOptions : String
                                  ) : OGRLayerH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_Create'
                )
    ]
    function _ogr_F_Create( _hDefn : OGRFeatureDefnH
                             ) : OGRFeatureH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_SetGeometry'
                )
    ]
    function _ogr_F_SetGeometry( _hFeat : OGRFeatureH ;
                                   _hGeom : OGRGeometryH
                                  ) : OGRErr ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_CreateFeature'
                )
    ]
    function _ogr_L_CreateFeature( _hLayer : OGRLayerH ;
                                     _hFeat  : OGRFeatureH
                                   ) : OGRErr ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_Fld_Create'
                )
    ]
    function _ogr_Fld_Create( _pszName : String ;
                                _eType   : OGRFieldType
                              ) : OGRFieldDefnH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_Fld_Set'
                )
    ]
    procedure _ogr_Fld_Set( _hDefn      : OGRFieldDefnH ;
                              _pszNameIn  : String ;
                              _eTypeIn    : OGRFieldType ;
                              _nWidthIn   : Integer ;
                              _nPrecision : Integer ;
                              _eJustifyIn : Integer
                             ) ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_L_CreateField'
                )
    ]
    function _ogr_L_CreateField( _hLayer     : OGRLayerH;
                                  _hField     : OGRFieldDefnH;
                                  _bApproxOK  : Integer
                                 ) : OGRErr ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_G_CreateFromWkb'
                )
    ]
    function _ogr_G_CreateFromWkb( _pabyData       : IntPtr ;
                                     _hSRS           : OGRSpatialReferenceH ;
                                     var _phGeometry : OGRGeometryH ;
                                     _nBytes         : Integer
                                    ) : OGRErr ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_SetFieldInteger'
                )
    ]
    procedure _ogr_F_SetFieldInteger( _hFeat   : OGRFeatureH ;
                                        _iField  : Integer ;
                                        _nValue  : Integer
                                       ) ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_SetFieldDouble'
                )
    ]
    procedure _ogr_F_SetFieldDouble( _hFeat   : OGRFeatureH ;
                                       _iField  : Integer ;
                                       _dfValue : Double
                                      ) ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_SetFieldString'
                )
    ]
    procedure _ogr_F_SetFieldString( _hFeat   : OGRFeatureH ;
                                       _iField  : Integer ;
                                       _pszValue: String
                                      )  ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.Cdecl,
                 CharSet = CharSet.Ansi, EntryPoint = 'OGR_F_SetFieldDateTime'
                )
    ]
    procedure _ogr_F_SetFieldDateTime( _hFeat    : OGRFeatureH ;
                                         _iField   : Integer ;
                                         _nYear    : Integer ;
                                         _nMonth   : Integer ;
                                         _nDay     : Integer ;
                                         _nHour    : Integer ;
                                         _nMinute  : Integer ;
                                         _nSecond  : Integer ;
                                         _nTZFlag  : Integer
                                        )  ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.StdCall,
                 CharSet = CharSet.Ansi, EntryPoint = '_OSRNewSpatialReference@4'
                )
    ]
    function _osr_NewSpatialReference_32( _pszWKT : String
                                        ) : OGRSpatialReferenceH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.StdCall,
                 CharSet = CharSet.Ansi, EntryPoint = 'OSRNewSpatialReference'
                )
    ]
    function _osr_NewSpatialReference_64( _pszWKT : String
                                        ) : OGRSpatialReferenceH ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.StdCall,
                 CharSet = CharSet.Ansi, EntryPoint = '_OSRDestroySpatialReference@4'
                )
    ]
    procedure _osr_DestroySpatialReference_32( _hSRS : OGRSpatialReferenceH
                                             )   ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.StdCall,
                 CharSet = CharSet.Ansi, EntryPoint = 'OSRDestroySpatialReference'
                )
    ]
    procedure _osr_DestroySpatialReference_64( _hSRS : OGRSpatialReferenceH
                                             )   ;  external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.StdCall,
                 CharSet = CharSet.Ansi, EntryPoint = '_CPLGetLastErrorMsg@0'
                )
    ]
    function _cpl_GetLastErrorMsg_32 : IntPtr ; external ;

    [ SuppressUnmanagedCodeSecurity,
      DllImport( GDAL_DEFAULT_DLL_NAME_OGR, CallingConvention = CallingConvention.StdCall,
                 CharSet = CharSet.Ansi, EntryPoint = 'CPLGetLastErrorMsg'
                )
    ]
    function _cpl_GetLastErrorMsg_64 : IntPtr ; external ;

  {$ENDIF}

var
  {$IFDEF JAVA}
  DLLHandle : IOGRLibrary ;
  {$ELSE}
  DLLHandle : THandle ;
  {$ENDIF}

  {$IFNDEF OXYGENE}
    DLLLoaded : Boolean  = False ;
  {$ELSE}
    DLLLoaded : Boolean := False ;
  {$ENDIF}

//==============================================================================
// helper functions
//==============================================================================

 {$IFDEF OXYGENE}
   // Convert String buffer to String
   {$IFDEF CLR}
   function asOGRString( const _buf : IntPtr ) : String ;
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
     function asOGRString( const _buf : String ) : String ;
     begin
       Result := _buf ;
     end ;

     function asOGRString( const _buf : OGRPointerH ) : String ;
     begin
       Result := _buf.getString(0) ;
     end ;
   {$ENDIF}
 {$ELSE}
   // Convert String buffer to String
   function asOGRString( const _buf : PAnsiChar ) : String ;
   begin
     Result := String( UTF8String( PAnsiChar( _buf ) ) ) ;
   end ;
 {$ENDIF}

 {$IFDEF OXYGENE}
   // Convert String to String buffer.
   function toORGAPI( const _str : String ) : String ;
   begin
     Result := _str ;
   end ;
 {$ELSE}
   // Convert String to String buffer.
   function toORGAPI( const _str : String ) : PAnsiChar ;
   begin
     Result := PAnsiChar( UTF8String( _str ) ) ;
   end ;
 {$ENDIF}

  {$IFDEF JAVA}

  {$IFDEF JAVA}
    function _ogr_RegisterAll : Integer ;
    begin
      Result := DLLHandle.OGRRegisterAll ;
    end;

    function _ogr_G_WkbSize ( _hGeom : OGRGeometryH
                             ) : Integer ;
    begin
      Result := DLLHandle.OGR_G_WkbSize( _hGeom ) ;
    end;

    function _ogr_G_ExportToWkb( _phGeometry     : OGRGeometryH ;
                                 _eOrder         : OGRwkbByteOrder;
                                 _pabyDstBuffer  : TBytes
                                 ) : OGRErr ;
    begin
      Result := DLLHandle.OGR_G_ExportToWkb( _phGeometry, _eOrder, _pabyDstBuffer) ;
    end;

    function _ogr_L_GetExtent( _hLayer   : OGRLayerH ;
                               _psExtent : OGRPointerH;
                               _bForce   : Integer
                               ) : OGRErr ;
    begin
      Result := DLLHandle.OGR_L_GetExtent(_hLayer, _psExtent, _bForce) ;
    end;

    function _ogr_L_GetFeature( _hLayer     : OGRLayerH ;
                                _nFeatureId : Int64
                               ) : OGRFeatureH ;
    begin
      Result := DLLHandle.OGR_L_GetFeature( _hLayer, _nFeatureId ) ;
    end;

    function _ogr_L_GetGeomType( _hLayer     : OGRLayerH
                                ) : OGRwkbGeometryType ;
    begin
      Result := DLLHandle.OGR_L_GetGeomType( _hLayer ) ;
    end;

    function _ogr_F_GetFieldIndex( _hFeat : OGRFeatureH ;
                                   _name  : String
                                  ) : Integer ;
    begin
      Result := DLLHandle.OGR_F_GetFieldIndex(_hFeat, _name);
    end;

    function _ogr_F_GetFieldAsString( _hFeat : OGRFeatureH ;
                                      _index : Integer
                                     ) : String ;
    begin
      Result := DLLHandle.OGR_F_GetFieldAsString( _hFeat, _index ) ;
    end;

    function _ogr_F_GetFieldAsDouble( _hFeat : OGRFeatureH ;
                                      _index : Integer
                                     ) : Double ;
    begin
      Result := DLLHandle.OGR_F_GetFieldAsDouble( _hFeat, _index ) ;
    end;

    function _ogr_F_GetFieldAsInteger( _hFeat : OGRFeatureH ;
                                       _index : Integer
                                     ) : Integer ;
    begin
      Result := DLLHandle.OGR_F_GetFieldAsInteger( _hFeat, _index ) ;
    end;

    function _ogr_F_GetFID( _hFeat : OGRFeatureH
                          ) : Int64 ;
    begin
      Result := DLLHandle.OGR_F_GetFID( _hFeat ) ;
    end;

    function _ogr_Open( _pszName        : String;
                        _bUpdate        : Integer;
                          var _pahDriverList  : OGRSFDriverH
                       ) : OGRDataSourceH ;
    var
      ptr : com.sun.jna.ptr.PointerByReference ;
    begin
      ptr := new com.sun.jna.ptr.PointerByReference() ;
      Result := DLLHandle.OGROpen( _pszName, _bUpdate, ptr ) ;
      _pahDriverList := ptr.Value ;
    end;

    function _ogr_DS_GetLayerCount( _hDS : OGRDataSourceH
                                    ): Integer ;
    begin
      Result := DLLHandle.OGR_DS_GetLayerCount( _hDS ) ;
    end;

    function _ogr_DS_GetLayer( _hDS     : OGRDataSourceH ;
                               _iLayer  : Integer
                              ) : OGRLayerH ;
    begin
      Result := DLLHandle.OGR_DS_GetLayer( _hDS, _iLayer ) ;
    end;

    function _ogr_DS_GetLayerByName( _hDS     : OGRDataSourceH ;
                                     _psName  : String
                                   ) : OGRLayerH ;
    begin
      Result := DLLHandle.OGR_DS_GetLayerByName(_hDS, _psName) ;
    end;

    function _ogr_L_GetLayerDefn( _hLayer : OGRLayerH
                                  ) : OGRFeatureDefnH ;
    begin
      Result := DLLHandle.OGR_L_GetLayerDefn( _hLayer ) ;
    end;

    function _ogr_L_GetName( _hLayer : OGRLayerH
                            ) : String ;
    begin
      Result := DLLHandle.OGR_L_GetName( _hLayer ) ;
    end;

    function _ogr_FD_GetFieldCount( _hDefn : OGRFeatureDefnH
                                    ) : Integer ;
    begin
      Result := DLLHandle.OGR_FD_GetFieldCount(_hDefn) ;
    end;

    function _ogr_FD_GetFieldDefn( _hDefn  : OGRFeatureDefnH ;
                                    _iField : Integer
                                   ) : OGRFieldDefnH ;
    begin
      Result := DLLHandle.OGR_FD_GetFieldDefn(_hDefn, _iField) ;
    end;

    function _ogr_Fld_GetNameRef( _hDefn : OGRFieldDefnH
                                  ) : String ;

    begin
      Result := DLLHandle.OGR_Fld_GetNameRef(_hDefn) ;
    end;

    function _ogr_Fld_GetType( _hDefn : OGRFieldDefnH
                                ) : OGRFieldType ;

    begin
      Result := DLLHandle.OGR_Fld_GetType(_hDefn) ;
    end;

    function _ogr_Fld_GetWidth( _hDefn : OGRFieldDefnH
                                ) : Integer ;
    begin
      Result := DLLHandle.OGR_Fld_GetWidth(_hDefn) ;
    end;

    function _ogr_Fld_GetPrecision( _hDefn : OGRFieldDefnH
                                    ) : Integer ;
    begin
      Result := DLLHandle.OGR_Fld_GetPrecision(_hDefn) ;
    end;

    function _ogr_L_GetSpatialRef( _hLayer : OGRLayerH
                                   ) : OGRSpatialReferenceH ;
    begin
      Result := DLLHandle.OGR_L_GetSpatialRef(_hLayer) ;
    end;

    function _ogr_Dr_GetName( hDriver : OGRSFDriverH
                              ) : String ;
    begin
      Result := DLLHandle.OGR_Dr_GetName(hDriver) ;
    end;

    procedure _ogr_L_ResetReading( _hLayer : OGRLayerH
                                  ) ;
    begin
      DLLHandle.OGR_L_ResetReading(_hLayer) ;
    end;

    procedure _ogr_L_SetSpatialFilterRect( _hLayer : OGRLayerH ;
                                           _minX   : Double ;
                                           _minY   : Double ;
                                           _maxX   : Double ;
                                           _maxY   : Double
                                          ) ;
    begin
      DLLHandle.OGR_L_SetSpatialFilterRect( _hLayer, _minX, _minY, _maxX, _maxY ) ;
    end;

    function _ogr_L_GetNextFeature( _hLayer : OGRLayerH
                                    ) : OGRFeatureH ;
    begin
      Result := DLLHandle.OGR_L_GetNextFeature(_hLayer) ;
    end;

    function _ogr_F_GetGeometryRef( _hFeat : OGRFeatureH
                                    ) : OGRGeometryH ;
    begin
      Result := DLLHandle.OGR_F_GetGeometryRef(_hFeat) ;
    end;

    procedure _ogr_F_Destroy( _hFeat : OGRFeatureH
                              ) ;
    begin
      DLLHandle.OGR_F_Destroy(_hFeat) ;
    end;

    procedure _ogr_DS_Destroy( _hDS : OGRDataSourceH
                               )  ;
    begin
      DLLHandle.OGR_DS_Destroy(_hDS) ;
    end;

    procedure _ogr_CleanupAll ;
    begin
      DLLHandle.OGRCleanupAll ;
    end;

    function _osr_ExportToWkt_32( _hSRS    : OGRSpatialReferenceH ;
                                var _ppszWkt  : OGRPointerH
                                ) : OGRErr  ;
    begin
      Result := DLLHandle.OSRExportToWkt( _hSRS, _ppszWkt) ;
    end;

    function _osr_ExportToWkt_64( _hSRS    : OGRSpatialReferenceH ;
                                var _ppszWkt  : OGRPointerH
                                ) : OGRErr  ;
    begin
      Result := DLLHandle.OSRExportToWkt( _hSRS, _ppszWkt) ;
    end;

    function _ogr_Dr_TestCapability( _hDriver : OGRSFDriverH ;
                                     _pszCap  : String
                                    ) : Integer ;
    begin
      Result := DLLHandle.OGR_Dr_TestCapability(_hDriver, _pszCap) ;
    end;

    function _ogr_Dr_CreateDataSource( _hDriver       : OGRSFDriverH ;
                                       _pszName       : String ;
                                       _papszOptions  : String
                                      ) : OGRDataSourceH ;
    begin
      Result := DLLHandle.OGR_Dr_CreateDataSource(_hDriver,_pszName,_papszOptions) ;
    end;

    function _ogr_ReleaseDataSource( _hDS : OGRDataSourceH
                                      ) : OGRErr ;
    begin
      Result := DLLHandle.OGRReleaseDataSource(_hDS) ;
    end;

    function _ogr_GetDriverCount : Integer ;
    begin
      Result := DLLHandle.OGRGetDriverCount
    end;

    function _ogr_GetDriver( _iDriver : Integer
                              ) : OGRSFDriverH ;
    begin
      Result := DLLHandle.OGRGetDriver(_iDriver);
    end;

    function _ogr_GetDriverByName( _pszName : String
                                  ) : OGRSFDriverH ;
    begin
      Result := DLLHandle.OGRGetDriverByName(_pszName) ;
    end;

    function _ogr_DS_CreateLayer( _hDS          : OGRDataSourceH ;
                                  _pszName      : String;
                                  _hSpatialRef  : OGRSpatialReferenceH ;
                                  _eType        : OGRwkbGeometryType ;
                                  _papszOptions : String
                                  ) : OGRLayerH ;
    begin
      Result := DLLHandle.OGR_DS_CreateLayer(_hDS,_pszName,_hSpatialRef,_eType,_papszOptions) ;
    end;

    function _ogr_F_Create( _hDefn : OGRFeatureDefnH
                             ) : OGRFeatureH ;
    begin
      Result := DLLHandle.OGR_F_Create(_hDefn) ;
    end;

    function _ogr_F_SetGeometry( _hFeat : OGRFeatureH ;
                                   _hGeom : OGRGeometryH
                                  ) : OGRErr ;
    begin
      Result := DLLHandle.OGR_F_SetGeometry(_hFeat,_hGeom) ;
    end;

    function _ogr_L_CreateFeature( _hLayer : OGRLayerH ;
                                     _hFeat  : OGRFeatureH
                                   ) : OGRErr ;
    begin
      Result := DLLHandle.OGR_L_CreateFeature(_hLayer,_hFeat) ;
    end;

    function _ogr_Fld_Create( _pszName : String ;
                                _eType   : OGRFieldType
                              ) : OGRFieldDefnH ;
    begin
      Result := DLLHandle.OGR_Fld_Create(_pszName, _eType) ;
    end;

    procedure _ogr_Fld_Set( _hDefn      : OGRFieldDefnH ;
                              _pszNameIn  : String ;
                              _eTypeIn    : OGRFieldType ;
                              _nWidthIn   : Integer ;
                              _nPrecision : Integer ;
                              _eJustifyIn : Integer
                             ) ;
    begin
      DLLHandle.OGR_Fld_Set(_hDefn, _pszNameIn, _eTypeIn, _nWidthIn, _nPrecision, _eJustifyIn ) ;
    end;

    function _ogr_L_CreateField( _hLayer     : OGRLayerH;
                                  _hField     : OGRFieldDefnH;
                                  _bApproxOK  : Integer
                                 ) : OGRErr ;
    begin
      Result := DLLHandle.OGR_L_CreateField(_hLayer, _hField, _bApproxOK) ;
    end;

    function _ogr_G_CreateFromWkb( _pabyData       : OGRGeometryH ;
                                     _hSRS           : OGRSpatialReferenceH ;
                                     var _phGeometry : OGRGeometryH ;
                                     _nBytes         : Integer
                                    ) : OGRErr ;
    begin
      Result := DLLHandle.OGR_G_CreateFromWkb(_pabyData, _hSRS, _phGeometry, _nBytes) ;
    end;

    procedure _ogr_F_SetFieldInteger( _hFeat   : OGRFeatureH ;
                                        _iField  : Integer ;
                                        _nValue  : Integer
                                       ) ;
    begin
      DLLHandle.OGR_F_SetFieldInteger(_hFeat, _iField, _nValue ) ;
    end;

    procedure _ogr_F_SetFieldDouble( _hFeat   : OGRFeatureH ;
                                       _iField  : Integer ;
                                       _dfValue : Double
                                      ) ;
    begin
      DLLHandle.OGR_F_SetFieldDouble(_hFeat, _iField, _dfValue) ;
    end;

    procedure _ogr_F_SetFieldString( _hFeat   : OGRFeatureH ;
                                       _iField  : Integer ;
                                       _pszValue: String
                                      )  ;
    begin
      DLLHandle.OGR_F_SetFieldString(_hFeat, _iField, _pszValue) ;
    end;

    procedure _ogr_F_SetFieldDateTime( _hFeat    : OGRFeatureH ;
                                         _iField   : Integer ;
                                         _nYear    : Integer ;
                                         _nMonth   : Integer ;
                                         _nDay     : Integer ;
                                         _nHour    : Integer ;
                                         _nMinute  : Integer ;
                                         _nSecond  : Integer ;
                                         _nTZFlag  : Integer
                                        )  ;
    begin
      DLLHandle.OGR_F_SetFieldDateTime(_hFeat,_iField, _nYear, _nMonth, _nDay,_nHour,  _nMinute, _nSecond, _nTZFlag) ;
    end;

    function _osr_NewSpatialReference_32( _pszWKT : String
                                        ) : OGRSpatialReferenceH ;
    begin
      Result := DLLHandle.OSRNewSpatialReference(_pszWKT) ;
    end;


    function _osr_NewSpatialReference_64( _pszWKT : String
                                        ) : OGRSpatialReferenceH ;
    begin
      Result := DLLHandle.OSRNewSpatialReference(_pszWKT) ;
    end;

    procedure _osr_DestroySpatialReference_32( _hSRS : OGRSpatialReferenceH
                                             )   ;
    begin
       DLLHandle.OSRDestroySpatialReference(_hSRS) ;
    end;

    procedure _osr_DestroySpatialReference_64( _hSRS : OGRSpatialReferenceH
                                             )   ;
    begin
      DLLHandle.OSRDestroySpatialReference(_hSRS) ;
    end;

    function _cpl_GetLastErrorMsg_32 : String ;
    begin
      Result := DLLHandle.CPLGetLastErrorMsg ;
    end;

    function _cpl_GetLastErrorMsg_64 : String ;
    begin
      Result := DLLHandle.CPLGetLastErrorMsg ;
    end;

  {$ENDIF}

  method T_OGRMapper.getFunctionName(&library: com.sun.jna.NativeLibrary; &method: &Method): String;
  begin
//    if &method.getName().equals('GDALAllRegister') then begin
//      exit '_GDALAllRegister@0' ;
//    end ;

    exit inherited getFunctionName( &library, &method) ;
  end ;
  {$ENDIF}

  function OGRwkbFlatten( const _gType : OGRwkbGeometryType
                         ) : OGRwkbGeometryType ;
  begin
    Result := OGRwkbGeometryType( ord( _gType ) and not wkb25DBit ) ;
  end;

  function LoadDLL( const _path : String ) : Boolean ;
  var
    dir   : String ;
    dllok : Boolean ;

    {$IFNDEF JAVA}
      {$IFDEF CLR}
      function stringLast( const _val : String ) : Integer ;
      {$ELSE}
      function stringLast( const _val : AnsiString ) : Integer ;
      {$ENDIF}
      begin
        {$IFDEF OXYGENE}
          if assigned( _val ) then
            Result := _val.Length - 1
          else
            Result := -1 ;
        {$ELSE}
          Result := Length( _val ) ;
        {$ENDIF}
      end ;

      {$IFDEF CLR}
        function translateName( const _name : String ) : String ;
      {$ELSE}
        function translateName( const _name : AnsiString ) : AnsiString ;
      {$ENDIF}
        var
          i : Integer ;
        begin
          if GisEnvironmentInfo.Is32 then
            Result := _name
          else begin
            Result := '' ;
            for i := StringFirst to stringLast( _name ) do begin
              if (i = StringFirst) and (_name[i] = '_' ) then continue ;

              if CharInSet( _name[i], ['a'..'z','A'..'Z', '_'] ) then
                Result := Result + _name[i] ;
            end ;
          end ;
        end ;

      {$IFDEF CLR}
        procedure mapFnc( var _var    : IntPtr ;
                          const _name : String
                         ) ;
        begin
          _var := GetProcAddress( DLLHandle, translateName(_name) ) ;
          assert( _var <> nil ) ;
        end;
      {$ELSE}
        {$IFDEF LEVEL_XE2_RTL}
          procedure mapFnc( var _fnc : Pointer ; const _name : AnsiString ) ;
        {$ELSE}
          procedure mapFnc( var _fnc : IntPtr  ; const _name : AnsiString ) ;
        {$ENDIF}
        var
          nn : String ;
        begin
          nn := translateName(_name) ;
          _fnc := GetProcAddress( DLLHandle, PChar( nn ) ) ;
          Assert( _fnc <> nil ) ;
        end;
      {$ENDIF}
    {$ENDIF}

  begin
    Result := DLLLoaded ;
    if DLLLoaded then Exit;

    dir := GetFilePath( _path ) ;
    if not IsStringEmpty( dir ) then
      SetCurrentDirEx( dir ) ;

    DLLLoaded := False ;
    {$IFDEF JAVA}
      var options: java.util.Map<String,com.sun.jna.FunctionMapper> := new java.util.HashMap<String,com.sun.jna.FunctionMapper>();
      options.put(com.sun.jna.&Library.OPTION_FUNCTION_MAPPER, new T_OGRMapper());

      try
        DLLHandle := IOGRLibrary(com.sun.jna.Native.loadLibrary( _path, typeOf(IOGRLibrary), options ) );
        dllok := DLLHandle <> nil ;
      except
        dllok := False ;
      end ;
    {$ELSE}
      DLLHandle := LoadLibraryWithinHinstance( _path ) ;
      dllok := DLLHandle >= 32 ;
    {$ENDIF}

    if dllok then begin
      DLLLoaded := True ;
      Result := DLLLoaded ;

      {$IFDEF CLR}
        mapFnc( _ogr_RegisterAllProc,         'OGRRegisterAll'              ) ;
        mapFnc( _ogr_CleanupAllProc,          'OGRCleanupAll'               ) ;
        mapFnc( _ogr_OpenProc,                'OGROpen'                     ) ;
        mapFnc( _ogr_DS_DestroyProc,          'OGR_DS_Destroy'              ) ;
        mapFnc( _ogr_Dr_GetNameProc,          'OGR_Dr_GetName'              ) ;
        mapFnc( _ogr_DS_GetLayerProc,         'OGR_DS_GetLayer'             ) ;
        mapFnc( _ogr_DS_GetLayerByNameProc,   'OGR_DS_GetLayerByName'       ) ;
        mapFnc( _ogr_DS_GetLayerCountProc,    'OGR_DS_GetLayerCount'        ) ;
        mapFnc( _ogr_FD_GetFieldCountProc,    'OGR_FD_GetFieldCount'        ) ;
        mapFnc( _ogr_FD_GetFieldDefnProc,     'OGR_FD_GetFieldDefn'         ) ;
        mapFnc( _ogr_F_DestroyProc,           'OGR_F_Destroy'               ) ;
        mapFnc( _ogr_F_GetFieldAsStringProc,  'OGR_F_GetFieldAsString'      ) ;
        mapFnc( _ogr_F_GetFieldAsIntegerProc, 'OGR_F_GetFieldAsInteger'     ) ;
        mapFnc( _ogr_F_GetFieldAsDoubleProc,  'OGR_F_GetFieldAsDouble'      ) ;
        mapFnc( _ogr_F_GetFieldIndexProc,     'OGR_F_GetFieldIndex'         ) ;
        mapFnc( _ogr_F_GetFIDProc,            'OGR_F_GetFID'                ) ;
        mapFnc( _ogr_F_GetGeometryRefProc,    'OGR_F_GetGeometryRef'        ) ;
        mapFnc( _ogr_Fld_GetNameRefProc,      'OGR_Fld_GetNameRef'          ) ;
        mapFnc( _ogr_Fld_GetPrecisionProc,    'OGR_Fld_GetPrecision'        ) ;
        mapFnc( _ogr_Fld_GetTypeProc,         'OGR_Fld_GetType'             ) ;
        mapFnc( _ogr_Fld_GetWidthProc,        'OGR_Fld_GetWidth'            ) ;
        mapFnc( _ogr_G_ExportToWkbProc,       'OGR_G_ExportToWkb'           ) ;
        mapFnc( _ogr_G_WkbSizeProc,           'OGR_G_WkbSize'               ) ;
        mapFnc( _ogr_L_GetExtentProc,         'OGR_L_GetExtent'             ) ;
        mapFnc( _ogr_L_GetFeatureProc,        'OGR_L_GetFeature'            ) ;
        mapFnc( _ogr_L_GetGeomTypeProc,       'OGR_L_GetGeomType'           ) ;
        mapFnc( _ogr_L_GetLayerDefnProc,      'OGR_L_GetLayerDefn'          ) ;
        mapFnc( _ogr_L_GetNameProc,           'OGR_L_GetName'               ) ;
        mapFnc( _ogr_L_GetNextFeatureProc,    'OGR_L_GetNextFeature'        ) ;
        mapFnc( _ogr_L_GetSpatialRefProc,     'OGR_L_GetSpatialRef'         ) ;
        mapFnc( _ogr_L_ResetReadingProc,      'OGR_L_ResetReading'          ) ;
        mapFnc( _ogr_L_SetSpatialFilterRectProc,'OGR_L_SetSpatialFilterRect') ;
        mapFnc( _osr_ExportToWktProc,         '_OSRExportToWkt@8'           ) ;
        mapFnc( _ogr_Dr_OpenProc,                'OGR_Dr_Open'                 ) ;
        mapFnc( _ogr_Dr_TestCapabilityProc,      'OGR_Dr_TestCapability'       ) ;
        mapFnc( _ogr_Dr_CreateDataSourceProc,    'OGR_Dr_CreateDataSource'     ) ;
        mapFnc( _ogr_ReleaseDatasourceProc,      'OGRReleaseDataSource'        ) ;
        mapFnc( _ogr_GetDriverCountProc,         'OGRGetDriverCount'           ) ;
        mapFnc( _ogr_GetDriverProc,              'OGRGetDriver'                ) ;
        mapFnc( _ogr_GetDriverByNameProc,        'OGRGetDriverByName'          ) ;
        mapFnc( _ogr_DS_CreateLayerProc,         'OGR_DS_CreateLayer'          ) ;
        mapFnc( _ogr_F_CreateProc,               'OGR_F_Create'                ) ;
        mapFnc( _ogr_F_SetGeometryProc,          'OGR_F_SetGeometry'           ) ;
        mapFnc( _ogr_L_CreateFeatureProc,        'OGR_L_CreateFeature'         ) ;
        mapFnc( _ogr_Fld_CreateProc,             'OGR_Fld_Create'              ) ;
        mapFnc( _ogr_Fld_SetProc,                'OGR_Fld_Set'                 ) ;
        mapFnc( _ogr_L_CreateFieldProc,          'OGR_L_CreateField'           ) ;
        mapFnc( _ogr_G_CreateFromWkbProc,        'OGR_G_CreateFromWkb'         ) ;
        mapFnc( _ogr_F_SetFieldIntegerProc,      'OGR_F_SetFieldInteger'       ) ;
        mapFnc( _ogr_F_SetFieldDoubleProc,       'OGR_F_SetFieldDouble'        ) ;
        mapFnc( _ogr_F_SetFieldStringProc,       'OGR_F_SetFieldString'        ) ;
        mapFnc( _ogr_F_SetFieldDateTimeProc,     'OGR_F_SetFieldDateTime'      ) ;
        mapFnc( _osr_NewSpatialReferenceProc,    '_OSRNewSpatialReference@4'   ) ;
        mapFnc( _osr_DestroySpatialReferenceProc,'_OSRDestroySpatialReference@4') ;
        mapFnc( _cpl_GetLastErrorMsgProc,        '_CPLGetLastErrorMsg@0'      ) ;
     {$ENDIF}
     {$IFDEF DCC}
        mapFnc( @_ogr_RegisterAll,            'OGRRegisterAll'              ) ;
        mapFnc( @_ogr_CleanupAll,             'OGRCleanupAll'               ) ;
        mapFnc( @_ogr_Free,                   'OGRFree'                     ) ;
        mapFnc( @_ogr_Open,                   'OGROpen'                     ) ;
        mapFnc( @_ogr_DS_Destroy,             'OGR_DS_Destroy'              ) ;
        mapFnc( @_ogr_Dr_GetName,             'OGR_Dr_GetName'              ) ;
        mapFnc( @_ogr_DS_GetLayer,            'OGR_DS_GetLayer'             ) ;
        mapFnc( @_ogr_DS_GetLayerByName,      'OGR_DS_GetLayerByName'       ) ;
        mapFnc( @_ogr_DS_GetLayerCount,       'OGR_DS_GetLayerCount'        ) ;
        mapFnc( @_ogr_FD_GetFieldCount,       'OGR_FD_GetFieldCount'        ) ;
        mapFnc( @_ogr_FD_GetFieldDefn,        'OGR_FD_GetFieldDefn'         ) ;
        mapFnc( @_ogr_F_Destroy,              'OGR_F_Destroy'               ) ;
        mapFnc( @_ogr_F_GetFieldAsString,     'OGR_F_GetFieldAsString'      ) ;
        mapFnc( @_ogr_F_GetFieldAsDouble,     'OGR_F_GetFieldAsDouble'      ) ;
        mapFnc( @_ogr_F_GetFieldAsInteger,    'OGR_F_GetFieldAsInteger'     ) ;
        mapFnc( @_ogr_F_GetFID,               'OGR_F_GetFID'                ) ;
        mapFnc( @_ogr_F_GetFieldIndex,        'OGR_F_GetFieldIndex'         ) ;
        mapFnc( @_ogr_F_GetGeometryRef,       'OGR_F_GetGeometryRef'        ) ;
        mapFnc( @_ogr_Fld_GetNameRef,         'OGR_Fld_GetNameRef'          ) ;
        mapFnc( @_ogr_Fld_GetPrecision,       'OGR_Fld_GetPrecision'        ) ;
        mapFnc( @_ogr_Fld_GetType,            'OGR_Fld_GetType'             ) ;
        mapFnc( @_ogr_Fld_GetWidth,           'OGR_Fld_GetWidth'            ) ;
        mapFnc( @_ogr_G_ExportToWkb,          'OGR_G_ExportToWkb'           ) ;
        mapFnc( @_ogr_G_WkbSize,              'OGR_G_WkbSize'               ) ;
        mapFnc( @_ogr_L_GetName,              'OGR_L_GetName'               ) ;
        mapFnc( @_ogr_L_GetExtent,            'OGR_L_GetExtent'             ) ;
        mapFnc( @_ogr_L_GetFeature,           'OGR_L_GetFeature'            ) ;
        mapFnc( @_ogr_L_GetGeomType,          'OGR_L_GetGeomType'           ) ;
        mapFnc( @_ogr_L_GetLayerDefn,         'OGR_L_GetLayerDefn'          ) ;
        mapFnc( @_ogr_L_GetNextFeature,       'OGR_L_GetNextFeature'        ) ;
        mapFnc( @_ogr_L_GetSpatialRef,        'OGR_L_GetSpatialRef'         ) ;
        mapFnc( @_ogr_L_ResetReading,         'OGR_L_ResetReading'          ) ;
        mapFnc( @_ogr_L_SetSpatialFilterRect, 'OGR_L_SetSpatialFilterRect'  ) ;
        mapFnc( @_osr_ExportToWkt,            '_OSRExportToWkt@8'           ) ;
        mapFnc( @_ogr_Dr_Open,                'OGR_Dr_Open'                 ) ;
        mapFnc( @_ogr_Dr_TestCapability,      'OGR_Dr_TestCapability'       ) ;
        mapFnc( @_ogr_Dr_CreateDataSource,    'OGR_Dr_CreateDataSource'     ) ;
        mapFnc( @_ogr_ReleaseDatasource,      'OGRReleaseDataSource'        ) ;
        mapFnc( @_ogr_GetDriverCount,         'OGRGetDriverCount'           ) ;
        mapFnc( @_ogr_GetDriver,              'OGRGetDriver'                ) ;
        mapFnc( @_ogr_GetDriverByName,        'OGRGetDriverByName'          ) ;
        mapFnc( @_ogr_DS_CreateLayer,         'OGR_DS_CreateLayer'          ) ;
        mapFnc( @_ogr_F_Create,               'OGR_F_Create'                ) ;
        mapFnc( @_ogr_F_SetGeometry,          'OGR_F_SetGeometry'           ) ;
        mapFnc( @_ogr_L_CreateFeature,        'OGR_L_CreateFeature'         ) ;
        mapFnc( @_ogr_Fld_Create,             'OGR_Fld_Create'              ) ;
        mapFnc( @_ogr_Fld_Set,                'OGR_Fld_Set'                 ) ;
        mapFnc( @_ogr_L_CreateField,          'OGR_L_CreateField'           ) ;
        mapFnc( @_ogr_G_CreateFromWkb,        'OGR_G_CreateFromWkb'         ) ;
        mapFnc( @_ogr_F_SetFieldInteger,      'OGR_F_SetFieldInteger'       ) ;
        mapFnc( @_ogr_F_SetFieldDouble,       'OGR_F_SetFieldDouble'        ) ;
        mapFnc( @_ogr_F_SetFieldString,       'OGR_F_SetFieldString'        ) ;
        mapFnc( @_ogr_F_SetFieldDateTime,     'OGR_F_SetFieldDateTime'      ) ;
        mapFnc( @_osr_NewSpatialReference,    '_OSRNewSpatialReference@4'   ) ;
        mapFnc( @_osr_DestroySpatialReference,'_OSRDestroySpatialReference@4') ;
        mapFnc( @_cpl_GetLastErrorMsg,        '_CPLGetLastErrorMsg@0'        ) ;
      {$ENDIF}
     end
    else
      DLLLoaded := False ;
  end ;

//=============================================================================
// TGIS_LayerOGR
//=============================================================================

  constructor TGIS_LayerOGR.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;
    FSQLPath := '' ;
    FDLLPath := '' ;
  end ;

  procedure TGIS_LayerOGR.doDestroy ;
  var
    i : Integer ;
  begin
    for i := 0 to high( cursorOgr ) do begin
      FreeObject( cursorOgr[i].currPoint      ) ;
      FreeObject( cursorOgr[i].currMultipoint ) ;
      FreeObject( cursorOgr[i].currArc        ) ;
      FreeObject( cursorOgr[i].currPolygon    ) ;
      FreeObject( cursorOgr[i].currComplex    ) ;
    end ;

    {$IFNDEF JAVA}
      if poFeature <> IntPtr(0) then
    {$ELSE}
      if assigned( poFeature ) then
    {$ENDIF}
        _ogr_F_Destroy( poFeature );

    {$IFNDEF JAVA}
      if hDS <> IntPtr(0) then begin
    {$ELSE}
      if hDS <> nil then begin
    {$ENDIF}
      _ogr_DS_Destroy( hDS ) ;
      {$IFNDEF JAVA}
        hDS := IntPtr(0) ;
      {$ELSE}
        hDS := nil ;
      {$ENDIF}
    end;
    {$IFNDEF JAVA}
      poLayer := IntPtr(0) ;
    {$ELSE}
      poLayer := nil ;
    {$ENDIF}

    inherited ;
  end ;

  procedure TGIS_LayerOGR.parseGeometry(
    const _uid     : TGIS_Uid ;
    const _cursor  : Integer
  ) ;
  var
    {$IFDEF OXYGENE}
      ptrvar    : TBytes    ;
    {$ELSE}
      ptrvar    : IntPtr    ;
    {$ENDIF}
    size      : Integer ;
    varStore  : OleVariant;
  begin
    {$IFNDEF JAVA}
      if poGeometry <> IntPtr(0)   then begin
    {$ELSE}
      if assigned( poGeometry ) then begin
    {$ENDIF}

      size     := _ogr_G_WkbSize( poGeometry );
      varStore := VarArrayCreate( [0,size], varByte ) ;
      {$IFDEF OXYGENE}
        ptrvar   := TBytes( TObject( varStore ) ) ;
      {$ELSE}
        {$IFNDEF JAVA}
          ptrvar := IntPtr( VarArrayLock( varStore ) ) ;
        {$ELSE}
          ptrvar := VarArrayLock( varStore ) ;
        {$ENDIF}
      {$ENDIF}
      try
        _ogr_G_ExportToWkb( poGeometry, wkbNDR, ptrvar );

        try
          cursorOgr[_cursor].currShape := TGIS_GeometryFactory.GisCreateShapeFromWKB(
            varStore, nil, nil, False, 0, nil
          ) ;
        except
          cursorOgr[_cursor].currShape := nil ;
        end ;
      finally
        {$IFDEF OXYGENE}
        {$ELSE}
          VarArrayUnLock( varStore ) ;
        {$ENDIF}
      end;
    end;

    if not assigned( cursorOgr[_cursor].currShape ) then exit;

    case cursorOgr[_cursor].currShape.ShapeType of
      TGIS_ShapeType.Point :
         begin
           cursorOgr[_cursor].currPoint.Recreate(
             cursorOgr[_cursor].currShape, nil, False, _uid, self
            ) ;
           FreeObject( cursorOgr[_cursor].currShape ) ;
           cursorOgr[_cursor].currShape := cursorOgr[_cursor].currPoint ;
         end ;
      TGIS_ShapeType.MultiPoint :
         begin
           cursorOgr[_cursor].currMultipoint.Recreate(
             cursorOgr[_cursor].currShape, nil, False, _uid, self
           ) ;
           FreeObject( cursorOgr[_cursor].currShape ) ;
           cursorOgr[_cursor].currShape := cursorOgr[_cursor].currMultipoint ;
         end ;
      TGIS_ShapeType.Arc :
         begin
           cursorOgr[_cursor].currArc.Recreate(
             cursorOgr[_cursor].currShape, nil, False, _uid, self
           ) ;
           FreeObject( cursorOgr[_cursor].currShape ) ;
           cursorOgr[_cursor].currShape := cursorOgr[_cursor].currArc ;
         end ;
      TGIS_ShapeType.Polygon :
         begin
           cursorOgr[_cursor].currPolygon.Recreate(
             cursorOgr[_cursor].currShape, nil, False, _uid, self
            ) ;
           FreeObject( cursorOgr[_cursor].currShape ) ;
           cursorOgr[_cursor].currShape := cursorOgr[_cursor].currPolygon ;
         end ;
      TGIS_ShapeType.Complex :
         begin
           cursorOgr[_cursor].currComplex.Recreate(
             cursorOgr[_cursor].currShape, nil, False, _uid, self
            ) ;
           FreeObject( cursorOgr[_cursor].currShape ) ;
           cursorOgr[_cursor].currShape := cursorOgr[_cursor].currComplex ;
         end ;
      else
         begin
           cursorOgr[_cursor].currShape := nil ;
         end ;
    end ;

    cursorOgr[_cursor].currShape := getEdited( cursorOgr[_cursor].currShape ) ;
  end ;

  function TGIS_LayerOGR.getFieldInternal(
    const _uid      : TGIS_Uid;
    const _name     : String ;
    const _cursor   : Integer
  ) : Variant ;
  var
    idx, nr : Integer ;
    fld     : TGIS_FieldInfo ;
    val     : String ;
    dt    : TDateTime ;
    {$IFDEF DCC}
    v     : Variant ;
    {$ENDIF}
  begin
    lockThread ;
    try
      {$IFNDEF JAVA}
        if (poFeature <> IntPtr(0))
      {$ELSE}
        if assigned( poFeature )
      {$ENDIF}
        and ( _uid = (_ogr_F_GetFID( poFeature )+1) ) then
        else begin
          _ogr_F_Destroy( poFeature );
          poFeature := _ogr_L_GetFeature( poLayer, _uid-1 ) ;
        end;

      {$IFNDEF JAVA}
        if poFeature <> IntPtr(0)   then begin
      {$ELSE}
        if assigned( poFeature ) then begin
      {$ENDIF}
        try
          idx := _ogr_F_GetFieldIndex( poFeature, toORGAPI( _name ) ) ;
          nr  := FindFieldInternal( _name ) ;
          fld := FieldInfo( nr ) ;

          case fld.FieldType of
            TGIS_FieldType.String  :
              Result := asOGRString( _ogr_F_GetFieldAsString( poFeature, idx ) ) ;
            TGIS_FieldType.Number  :
              if fld.Decimal = 0 then
                Result := _ogr_F_GetFieldAsInteger( poFeature, idx )
              else
                Result := _ogr_F_GetFieldAsDouble( poFeature, idx ) ;
            TGIS_FieldType.Float   :
              Result := _ogr_F_GetFieldAsDouble( poFeature, idx ) ;
            TGIS_FieldType.Boolean :
              Result := asOGRString( _ogr_F_GetFieldAsString( poFeature, idx ) ) ;
            TGIS_FieldType.Date    : begin
              val := asOGRString( _ogr_F_GetFieldAsString( poFeature, idx ) ) ;
              if not IsStringEmpty( val ) then begin
                {$IFDEF JAVA}
                  Result := StrToDateTime( val ) ;
                {$ENDIF}
                {$IFDEF CLR}
                  if DateTime.TryParse( val, dt ) then
                    Result := dt  // date as text
                {$ENDIF}
                {$IFDEF DCC}
                  if TryStrToDateTime( val, dt ) then
                    Result := dt  // date as text
                  else begin
                    v := val ;    // date as double
                    Result := VarAsType( v, varDate ) ;
                  end ;
                {$ENDIF}
              end
              else
                Result := Unassigned ;
            end ;
          end ;
        finally
          //_ogr_F_Destroy( poFeature ) ;
        end ;
      end
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerOGR.getBindedFieldInternal(
    const _shape  : TObject ;
    const _field  : Integer ;
    const _cursor : Integer
  ) : Variant ;
  var
    shp   : TGIS_Shape ;
    idx   : Integer ;
    fld   : TGIS_FieldInfo ;
  begin
    lockThread ;
    try
      shp := TGIS_Shape( _shape ) ;
      if not assigned( shp ) then begin
        Result := Unassigned ;
        exit ;
      end ;

      {$IFNDEF JAVA}
        if (poFeature <> IntPtr(0))
      {$ELSE}
        if assigned( poFeature )
      {$ENDIF}
        and ( shp.Uid = (_ogr_F_GetFID( poFeature )+1) ) then
        else begin
          _ogr_F_Destroy( poFeature );
          poFeature := _ogr_L_GetFeature( poLayer, shp.Uid-1 ) ;
        end;

      {$IFNDEF JAVA}
        if poFeature <> IntPtr(0)   then begin
      {$ELSE}
        if assigned( poFeature ) then begin
      {$ENDIF}
        try
          fld := FieldInfo( _field ) ;
          idx := _ogr_F_GetFieldIndex( poFeature, toORGAPI( fld.Name ) ) ;

          case fld.FieldType of
            TGIS_FieldType.String  :
              Result := asOGRString( _ogr_F_GetFieldAsString( poFeature, idx ) ) ;
            TGIS_FieldType.Number  :
              if fld.Decimal = 0 then
                Result := _ogr_F_GetFieldAsInteger( poFeature, idx )
              else
                Result := _ogr_F_GetFieldAsDouble( poFeature, idx ) ;
            TGIS_FieldType.Float   :
              Result := _ogr_F_GetFieldAsDouble( poFeature, idx ) ;
            TGIS_FieldType.Boolean :
              Result := asOGRString( _ogr_F_GetFieldAsString( poFeature, idx ) ) ;
            TGIS_FieldType.Date    :
              Result := StrToDateTime( asOGRString( _ogr_F_GetFieldAsString( poFeature, idx ) ) );
          end ;
        finally
          //_ogr_F_Destroy( poFeature ) ;
        end ;
      end
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerOGR.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    // do nothing
  end ;

  function TGIS_LayerOGR.passwordCallBack(
    const _token : String
  ) : String ;
  begin
    Result := GisPasswordList.Get( Name, _token ) ;
    if IsStringEmpty( Result ) then begin
      if assigned( FOnPassword ) then
        {$IFDEF OXYGENE}
          Result := FOnPassword( Self,
                                 TGIS_TemplateProducerEventArgs.Create( _token )
                               )
        {$ELSE}
          Result := PasswordEvent( Self, _token )
        {$ENDIF}
      else
        Result := _token ;
      GisPasswordList.Add( Name, _token, Result ) ;
    end ;
  end ;

  procedure TGIS_LayerOGR.parseConfig(
    const _path   : String ;
    const _params : TStrings
  ) ;
  begin
    ReadSQLParamsFromPath( _path, _params  ) ;

    // resolve any ID/Password tokens
    {$IFNDEF OXYGENE}
      _params.Text  := TemplateProducer( _params.Text, nil,
                                         passwordCallBack, False
                                        ) ;
    {$ELSE}
      _params.Text  := TemplateProducer( _params.Text, nil,
                                         @passwordCallBack, False
                                        ) ;
    {$ENDIF}
  end ;

  function TGIS_LayerOGR.getError : String ;
  begin
    {$IFDEF OXYGENE}
      if sizeOf( IntPtr ) = 8 then
        Result := asOGRString( _cpl_GetLastErrorMsg_64 )
      else
        Result := asOGRString( _cpl_GetLastErrorMsg_32 )
    {$ELSE}
      Result := asOGRString( _cpl_GetLastErrorMsg )
    {$ENDIF}
  end ;

  procedure TGIS_LayerOGR.setUp ;
  var
    env     : OGREnvelope ;
    i       : Integer ;
    fCnt    : Integer ;
    fName   : String ;
    fWidth  : Integer ;
    fPrec   : Integer ;
    wktFull : String ;
   {$IFDEF CLR}
     buf    : TBytes ;
   {$ENDIF}
     ptr    : OGRPointerH ;
    cpath   : String ;
    layer   : String ;
    config  : TStringList ;
  begin
    inherited ;

    if IsStringEmpty( FDLLPath ) then
      FDLLPath := GDAL_DEFAULT_DLL_NAME_OGR ;

    lastUid   := -1 ;
    if not LoadDLL( FDLLPath ) then
      raise EGIS_Exception.Create( Format( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                           [ FDLLPath, 0 ] ), Path, 0
                                  ) ;
    try
      _ogr_RegisterAll ;

      if ( CompareText( GetFileExt( Path ), GIS_TTKLS_EXT    ) = 0 ) or
         ( CompareText( GetFileExt( Path ), GIS_TTKLAYER_EXT ) = 0 ) then begin
        config := TStringList.Create ;
        try
          parseConfig( Path, config ) ;
          cpath := config.Values[GIS_INI_PATH] ;
          layer := config.Values[GIS_INI_LAYERSQL_LAYER] ;

          cpath := GetPathAbsolute( GetFileDir( Path ), cpath ) ;

          hDS := _ogr_Open( toORGAPI(cpath), 0, pahDriver ) ;
        finally
          FreeObject( config ) ;
        end ;
      end
      else begin
        layer := '' ;
        if not IsStringEmpty( FSQLPath ) then
          hDS := _ogr_Open( toORGAPI( FSQLPath ), 0, pahDriver )
        else
          hDS := _ogr_Open( toORGAPI( Path ), 0, pahDriver ) ;
      end ;

      {$IFNDEF JAVA}
        if ( hDS = IntPtr(0) ) then
      {$ELSE}
        if ( hDS = nil ) then
      {$ENDIF}
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ) + #13#10 +
            Format( '%s', [getError] ),
            Path, 0
          ) ;

      if not IsStringEmpty( layer ) then begin
        poLayer := _ogr_DS_GetLayerByName( hDS, toORGAPI( layer ) ) ;
        {$IFNDEF JAVA}
          if ( poLayer = IntPtr(0) ) then
        {$ELSE}
          if ( poLayer = nil ) then
        {$ENDIF}
            poLayer := _ogr_DS_GetLayer( hDS, 0 )
        else
          Name := layer ;
      end
      else
        poLayer := _ogr_DS_GetLayer( hDS, 0 ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ) + #13#10 +
        Format( '%s', [getError] ),
        Path, 0
      ) ;
    end;

   {$IFDEF OXYGENE}
     {$IFDEF CLR}
       SetLength( buf, sizeOf(OGREnvelope) ) ;
       if _ogr_L_GetExtent( poLayer,
                            Marshal.UnsafeAddrOfPinnedArrayElement(buf,0),1
                           ) = OGRERR_NONE then begin
         env.MinX := BitConverter.ToDouble( buf, 0 ) ;
         env.MaxX := BitConverter.ToDouble( buf, 8 ) ;
         env.MinY := BitConverter.ToDouble( buf, 16 ) ;
         env.MaxY := BitConverter.ToDouble( buf, 24 ) ;
         Extent := GisExtent( env.MinX, env.MinY, env.MaxX, env.MaxY ) ;
       end ;
      {$ELSE}
        ptr := new com.sun.jna.Memory(4*sizeOf(Double)) ;
        if ( _ogr_L_GetExtent( poLayer, ptr, 1 ) = OGRERR_NONE ) then begin
           env.MinX := Marshal.ReadDouble( ptr, 0 ) ;
           env.MaxX := Marshal.ReadDouble( ptr, 8 ) ;
           env.MinY := Marshal.ReadDouble( ptr, 16 ) ;
           env.MaxY := Marshal.ReadDouble( ptr, 24 ) ;
           Extent := GisExtent( env.MinX, env.MinY, env.MaxX, env.MaxY ) ;
        end ;
      {$ENDIF}
   {$ELSE}
     if ( _ogr_L_GetExtent( poLayer, env, 1 ) = OGRERR_NONE ) then begin
       Extent := GisExtent( env.MinX, env.MinY, env.MaxX, env.MaxY ) ;
     end ;
   {$ENDIF}

    poFeatureDef := _ogr_L_GetLayerDefn( poLayer ) ;
    {$IFNDEF JAVA}
      if poFeatureDef <> IntPtr(0)   then begin
    {$ELSE}
      if assigned( poFeatureDef ) then begin
    {$ENDIF}
      fCnt := _ogr_FD_GetFieldCount( poFeatureDef ) ;

      for i := 0 to fCnt - 1 do begin
        poFieldDef := _ogr_FD_GetFieldDefn( poFeatureDef, i  ) ;

        {$IFNDEF JAVA}
          if poFieldDef <> IntPtr(0)   then begin
        {$ELSE}
          if assigned( poFieldDef ) then begin
        {$ENDIF}
          fName       := asOGRString( _ogr_Fld_GetNameRef( poFieldDef ) ) ;
          poFieldType := _ogr_Fld_GetType( poFieldDef ) ;
          fWidth      := _ogr_Fld_GetWidth( poFieldDef ) ;
          fPrec       := _ogr_Fld_GetPrecision( poFieldDef ) ;

          if FindField( fName ) <> -1 then continue ;

          case poFieldType of
            OFTInteger,
            OFTInteger64  : begin
              if fWidth = 0 then
                fWidth := 10 ;
              AddFieldInternal( fName, TGIS_FieldType.Number, fWidth, fPrec ) ;
            end;
            OFTReal       : begin
              if fWidth = 0 then
                fWidth := 10 ;
              AddFieldInternal( fName, TGIS_FieldType.Float, fWidth, fPrec ) ;
            end ;
            OFTString,
            OFTWideString : begin
              if fWidth = 0 then
                fWidth := 255 ;
              AddFieldInternal( fName, TGIS_FieldType.String, fWidth, 0 ) ;
            end ;
            OFTDate,
            OFTTime,
            OFTDateTime   : AddFieldInternal( fName, TGIS_FieldType.Date, fWidth, 0 ) ;
          end ;
        end ;
      end ;
    end ;

    poLayerSR := _ogr_L_GetSpatialRef( poLayer ) ;
    {$IFNDEF JAVA}
      if poLayerSR <> IntPtr(0)   then begin
    {$ELSE}
      if assigned( poLayerSR ) then begin
    {$ENDIF}
     {$IFDEF OXYGENE}
       if sizeOf( IntPtr ) = 8 then
         _osr_ExportToWkt_64( poLayerSR, ptr )
       else
         _osr_ExportToWkt_32( poLayerSR, ptr ) ;

       wktFull := asOGRString( ptr ) ;
     {$ELSE}
       _osr_ExportToWkt( poLayerSR, ptr ) ;
       {$IFNDEF JAVA}
         wktFull := asOGRString( PAnsiChar(ptr) ) ;
       {$ELSE}
         wktFull := asOGRString( ptr ) ;
       {$ENDIF}
       _ogr_Free( ptr ) ;
     {$ENDIF}

      SetCSByWKT( wktFull ) ;
    end;

    DefaultShapeType := ogrTypeToShp(OGRwkbFlatten(_ogr_L_GetGeomType(poLayer))) ;

    FFileInfo := 'OGR Driver: ' + asOGRString( _ogr_Dr_GetName( pahDriver ) ) ;
  end ;

  function  TGIS_LayerOGR.cursorOpen   :  Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length(cursorOgr )  then
        SetLength( cursorOgr, Result + 1 ) ;
      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorOgr[Result] ) then
          cursorOgr[Result] := new T_cursorOgr ;
      {$ENDIF}
      cursorOgr[Result].curInUse := True ;

      cursorOgr[Result].currPoint      := TGIS_ShapePoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorOgr[Result].currMultipoint := TGIS_ShapeMultiPoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorOgr[Result].currArc        := TGIS_ShapeArc.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorOgr[Result].currPolygon    := TGIS_ShapePolygon.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorOgr[Result].currComplex    := TGIS_ShapeComplex.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerOGR.cursorClose(
    const _cursor      : Integer
  ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorOgr[_cursor].curInUse := False ;
      FreeObject( cursorOgr[_cursor].currPoint      ) ;
      FreeObject( cursorOgr[_cursor].currMultipoint ) ;
      FreeObject( cursorOgr[_cursor].currArc        ) ;
      FreeObject( cursorOgr[_cursor].currPolygon    ) ;
      FreeObject( cursorOgr[_cursor].currComplex    ) ;

      // truncate cursorState at the tail;
      for i := length( cursorOgr ) - 1 downto 0 do begin
        if not cursorOgr[i].curInUse then begin
          SetLength( cursorOgr, i ) ;
        end
        else
          break ;
      end ;

      inherited cursorClose( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerOGR.cursorFirst(
     const _cursor      : Integer          ;
     const _viewerCS    : Boolean          ;
     const _extent      : TGIS_Extent      ;
     const _query       : String           ;
     const _shape       : TGIS_Shape       ;
     const _de9im       : String           ;
     const _skipDeleted : Boolean
   ) ;
  var
    ex : TGIS_Extent ;
  begin
    lockThread ;
    try
      cursorOgr[_cursor].currShape := nil ;

      if GisIsNoWorld( _extent ) then
        exit ;

      inherited cursorFirstInternal(
                  _cursor, _viewerCS,
                  _extent, _query, _shape, _de9im, _skipDeleted
                ) ;

      ex := GisCommonExtent( cursorState[ _cursor ].curExtent,
                             GisExtent( -1E37, -1E37, 1E37, 1E37 )
                           ) ;
      _ogr_L_SetSpatialFilterRect( poLayer, ex.XMin, ex.YMin, ex.XMax, ex.YMax ) ;
      _ogr_L_ResetReading( poLayer ) ;

      cursorNext( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerOGR.cursorNext(
    const _cursor : Integer
  ) ;
  var
    uid     : TGIS_Uid ;
    ihglass : Integer ;
  begin
    lockThread ;
    try
      ihglass := 0 ;
      while True do begin
        inc( ihglass ) ;
        if ihglass mod GIS_PROGRESS_TRESHOLD = 0 then begin
          if HourglassShake then begin
            cursorOgr[ _cursor ].currShape := nil ;
            break ;
          end;
        end ;

        {$IFNDEF JAVA}
          if poFeature <> IntPtr(0) then
        {$ELSE}
          if assigned( poFeature ) then
        {$ENDIF}
            _ogr_F_Destroy( poFeature );

        poFeature := _ogr_L_GetNextFeature( poLayer ) ;

        cursorOgr[_cursor].currShape := nil ;
        {$IFNDEF JAVA}
          if poFeature = IntPtr(0) then
        {$ELSE}
          if not assigned( poFeature ) then
        {$ENDIF}
          exit;

        uid := _ogr_F_GetFID( poFeature ) + 1 ;

        poGeometry := _ogr_F_GetGeometryRef( poFeature ) ;
        try
          parseGeometry( uid, _cursor ) ;
        finally
  //        _ogr_F_Destroy( poFeature );
        end;

        if cursorOgr[_cursor].currShape = nil then begin
         // using an xxxInternalVersion may be a bit too secure
         // but better is to be too restrictive than too lose
         if not inherited cursorEofInternal( _cursor ) then begin
           cursorOgr[_cursor].currShape
             := inherited cursorShapeInternal( _cursor ) ;
           inherited cursorNextInternal( _cursor ) ;
         end ;
         if cursorOgr[_cursor].currShape = nil then exit ;
        end ;

        if cursorState[_cursor].curSkipDeleted and
          cursorOgr[_cursor].currShape.IsDeleted then
        begin
         continue ;
        end ;

        if not isInScope( cursorOgr[_cursor].currShape, _cursor ) then
         continue
        else
         exit ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerOGR.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorOgr[_cursor].currShape = nil ;
  end ;

  function TGIS_LayerOGR.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorOgr[_cursor].currShape ) then
       Result := cursorOgr[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerOGR.GetShape(
    const _uid    : TGIS_Uid ;
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    lockThread ;
    try
      Result := nil ;

      if _uid <= 0 then exit ;

      // if it is in edited list
      Result := inherited GetShape( _uid, _cursor ) ;
      if Result <> nil then exit ;

      // is it a current shape
      if ( cursorOgr[_cursor].currShape     <> nil ) and
         ( cursorOgr[_cursor].currShape.Uid = _uid ) then begin
        Result := cursorOgr[_cursor].currShape ;
        exit ;
      end ;

      {$IFNDEF JAVA}
        if (poFeature <> IntPtr(0))
      {$ELSE}
        if assigned( poFeature )
      {$ENDIF}
      and ( _uid = (_ogr_F_GetFID( poFeature )+1) ) then
      else begin
        _ogr_F_Destroy( poFeature );
          poFeature := _ogr_L_GetFeature( poLayer, _uid-1 ) ;
      end;

      {$IFNDEF JAVA}
        if poFeature <> IntPtr(0)
      {$ELSE}
        if assigned( poFeature )
      {$ENDIF}
      then begin
        poGeometry := _ogr_F_GetGeometryRef( poFeature ) ;
        try
          parseGeometry( _uid, _cursor ) ;
          Result := cursorOgr[_cursor].currShape ;
        finally
          //_ogr_F_Destroy( poFeature );
        end;
        exit ;
      end;

      // if no index file, traverse normally
      cursorFirst( _cursor, False,
                   GisWholeWorld, '', nil, '', True
                 ) ;

      while not cursorEof( _cursor ) do begin
        if cursorShape(_cursor).Uid = _uid then begin
          Result := cursorShape(_cursor) ;
          exit ;
        end ;
        cursorNext(_cursor) ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerOGR.GetLastUid : TGIS_Uid ;
  var
    shp       : TGIS_Shape ;
    old_scope : String     ;
  begin
    lockThread ;
    try
      old_scope := Scope ;
      try
        if lastUid < 0 then begin
          shp := nil ;
          cursorFirst( 0, False,
                       GisWholeWorld, '', nil, '', True
                     ) ;

          while not cursorEof(0) do begin // iterate all shapes
            shp := cursorShape(0) ;
            cursorNext(0) ;
          end ;

          if assigned( shp ) then
            lastUid := shp.Uid
          else
            lastUid := 0 ;
        end ;

        Result := lastUid ;
      finally
        Scope := old_scope ;
      end;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerOGR.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    inherited ;

    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;
  end ;

  function TGIS_LayerOGR.findDriver(
    const _extension : String
  ) : String ;
  var
    ext : String ;
  begin
    Result := '' ;

    ext := UpperCase( _extension ) ;

    if ( ext = '.AVCBin' ) then
      Result := 'AVCBin'
    else if ( ext = '.E00' ) then
      Result := 'AVCE00'
    else if ( ext = '.BNA' ) then
      Result := 'BNA'
    else if ( ext = '.CSV' ) then
      Result := 'CSV'
    else if ( ext = '.MDB' ) then
      Result := 'PGeo'
    else if ( ext = '.SHP' ) then
      Result := 'ESRI Shapefile'
    else if ( ext = '.FDD' ) then
      Result := 'FMEObjects Gateway'
    else if ( ext = '.GEOJSON' ) then
      Result := 'GeoJSON'
    else if ( ext = '.GML' ) then
      Result := 'GML'
    else if ( ext = '.GMT' ) then
      Result := 'GMT'
    else if ( ext = '.GPX' ) then
      Result := 'GPX'
    else if ( ext = '.ILI' ) then
      Result := 'Interlis 1'
    else if ( ext = '.KML' ) then
      Result := 'KML'
    else if ( ext = '.TAB' ) then
      Result := 'MapInfo File'
    else if ( ext = '.MIF' ) then
      Result := 'MapInfo File'
    else if ( ext = '.DGN' ) then
      Result := 'DGN'
    else if ( ext = '.000' ) then
      Result := 'S57'
    else if ( ext = '.DDF' ) then
      Result := 'SDTS'
    else if ( ext = '.SQLITE' ) then
      Result := 'SQLite'
    else if ( ext = '.VRT' ) then
      Result := 'VRT'
    else if ( ext = '.GXT' ) then
      Result := 'Geoconcept'
    else if ( ext = '.DAT' ) then
      Result := 'XPlane'
    else if ( ext = '.DXF' ) then
      Result := 'DXF'
    else if ( ext = '.GPKG') then
      Result := 'GPKG'
    else if ( ext = '.VFK') then
      Result := 'VFK'
    else if ( ext = '.PBF') then
      Result := 'PBF'
    else if ( ext = '.SUA') then
      Result := 'SUA'
    else if ( ext = '.HTF') then
      Result := 'HTF'
    else if ( ext = '.SVG') then
      Result := 'SVG'
    else if ( ext = '.PDF') then
      Result := 'PDF' ;
  end;

  function TGIS_LayerOGR.shpTypeToOgr(
    const _type : TGIS_ShapeType
  ) : Integer ;
  begin
    case _type of
      TGIS_ShapeType.Point       : Result := 1 ;
      TGIS_ShapeType.MultiPoint  : Result := 4 ;
      TGIS_ShapeType.Arc         : Result := 2 ;
      TGIS_ShapeType.Polygon     : Result := 3 ;
      TGIS_ShapeType.Complex     : Result := 7 ;
    else                           Result := 0 ;
    end ;
  end ;

  function TGIS_LayerOGR.ogrTypeToShp(
    const _type : Integer
  ) : TGIS_ShapeType ;
  begin
    case _type of
      1 : Result := TGIS_ShapeType.Point        ;
      4 : Result := TGIS_ShapeType.MultiPoint   ;
      2 : Result := TGIS_ShapeType.Arc          ;
      5 : Result := TGIS_ShapeType.Arc          ;
      3 : Result := TGIS_ShapeType.Polygon      ;
      6 : Result := TGIS_ShapeType.Polygon      ;
      7 : Result := TGIS_ShapeType.Complex      ;
    else  Result := TGIS_ShapeType.Unknown
    end ;
  end ;

  procedure TGIS_LayerOGR.ImportLayerEx(
    const _layer       : TGIS_LayerVector  ;
    const _extent      : TGIS_Extent       ;
    const _type        : TGIS_ShapeType    ;
    const _scope       : String            ;
    const _shape       : TGIS_Shape        ;
    const _de9im       : String            ;
    const _truncated   : Boolean
  ) ;
  var
    drv         : OGRSFDriverH ;
    drvName     : String ;
    drvRO       : Boolean ;
    i, j        : Integer ;
    ogrDS       : OGRDataSourceH ;
    ogrLayer    : OGRLayerH ;
    ogrField    : OGRFieldDefnH ;
    ogrFieldTp  : Integer ;
    ogrFeatureD : OGRFeatureDefnH ;
    ogrFeature  : OGRFeatureH ;
    ogrGeometry : OGRGeometryH ;
    ogrSR       : OGRSpatialReferenceH ;
    err         : OGRErr ;
    shp         : TGIS_Shape      ;
    shp_tmp     : TGIS_Shape      ;
    ex          : TGIS_Extent     ;
    shape_no    : Integer         ;
    end_uid     : TGIS_Uid         ;
    abort       : Boolean         ;
    old_scope   : String          ;
    eloop       : TGIS_LayerVectorEnumerator ;
    fld         : TGIS_FieldInfo ;
    {$IFDEF OXYGENE}
      obj       : TObject ;
    {$ENDIF}
    store       : OleVariant ;
    ptr         : OGRPointerH ;
    v           : Variant ;
    year        : Word ;
    month       : Word ;
    day         : Word ;
    h           : Word ;
    m           : Word ;
    s           : Word ;
    ms          : Word ;
    config      : TStringList ;
    cpath       : String ;
    clayer      : String ;
  begin
    if not assigned( _layer ) then exit ;

    if not CheckFileWriteAccessEx( Path, True, True, True ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    if IsStringEmpty( FDLLPath ) then
      FDLLPath := GDAL_DEFAULT_DLL_NAME_OGR ;

    if not LoadDLL( FDLLPath ) then
      raise EGIS_Exception.Create( Format( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                           [ FDLLPath, 0 ] ), Path, 0
                                  ) ;
    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try
      ImportStructure( _layer ) ;
      PrepareExportFieldNames( 10 ) ;

      _ogr_RegisterAll ;

      if ( CompareText( GetFileExt( Path ), GIS_TTKLS_EXT    ) = 0 ) or
         ( CompareText( GetFileExt( Path ), GIS_TTKLAYER_EXT ) = 0 ) then begin
        config := TStringList.Create ;
        try
          parseConfig( Path, config ) ;
          cpath   := config.Values[GIS_INI_PATH] ;
          clayer  := config.Values[GIS_INI_LAYERSQL_LAYER] ;
          drvName := findDriver( GetFileExt( cpath ) ) ;
        finally
          FreeObject( config ) ;
        end ;
      end
      else begin
        drvName := findDriver( GetFileExt( Path ) ) ;
        cpath   := Path ;
        clayer  := Name ;
      end ;

      drv := _ogr_GetDriverByName( toORGAPI( drvName ) ) ;
      {$IFNDEF JAVA}
        if drv = IntPtr(0) then
      {$ELSE}
        if not assigned( drv ) then
      {$ENDIF}
          raise EGIS_Exception.Create(
            Format( _rsrc( GIS_RS_ERR_FILEMAPPING )+ #13#10 +
            Format( '%s', [getError] ),
            [ FDLLPath, 0 ] ), Path, 0
          ) ;
      drvRO := _ogr_Dr_TestCapability( drv, 'CreateDataSource') = 0 ;
      if drvRO then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), Path, 0 ) ;

      ogrDS := _ogr_Dr_CreateDataSource( drv, toORGAPI( cpath ), nil ) ;
      {$IFNDEF JAVA}
        if ogrDS = IntPtr(0) then
      {$ELSE}
        if not assigned( ogrDS ) then
      {$ENDIF}
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE )+ #13#10 +
            Format( '%s', [getError] ), cpath, 0 ) ;

      {$IFDEF OXYGENE}
        if sizeOf( IntPtr ) = 8 then
          ogrSR := _osr_NewSpatialReference_64( toORGAPI( CS.FullWKT ) )
        else
          ogrSR := _osr_NewSpatialReference_32( toORGAPI( CS.FullWKT ) ) ;
      {$ELSE}
        ogrSR := _osr_NewSpatialReference( toORGAPI( CS.FullWKT ) ) ;
      {$ENDIF}

      ogrLayer := _ogr_DS_CreateLayer( ogrDS, toORGAPI( clayer ), ogrSR, shpTypeToOgr( _type ), nil ) ;
      {$IFNDEF JAVA}
        if ogrLayer = IntPtr(0) then
      {$ELSE}
        if not assigned( ogrLayer ) then
      {$ENDIF}
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE )+ #13#10 +
            Format( '%s', [getError] ), Path, 0 ) ;

      for i := 0 to Fields.Count-1 do begin
          fld := FieldInfo( i ) ;

          if fld.Deleted    then continue ;
          if fld.Temporary  then continue ;

          case fld.FieldType of
            TGIS_FieldType.String  : ogrFieldTp := OFTString ;
            TGIS_FieldType.Number  : if fld.NewDecimal = 0 then
                                       ogrFieldTp := OFTInteger
                                    else
                                      ogrFieldTp := OFTReal;
            TGIS_FieldType.Float   : ogrFieldTp := OFTReal;
            TGIS_FieldType.Boolean : ogrFieldTp := OFTString;
            TGIS_FieldType.Date    : ogrFieldTp := OFTDateTime
          else                       ogrFieldTp := OFTString ;
          end;

          ogrField := _ogr_Fld_Create( toORGAPI( fld.ExportName ), ogrFieldTp ) ;
          _ogr_Fld_Set( ogrField, toORGAPI( fld.ExportName ), ogrFieldTp,
                      fld.NewWidth, fld.NewDecimal, 0
                      ) ;
          _ogr_L_CreateField( ogrLayer, ogrField, 1 ) ;
      end ;

      ExportStructureToFLD ;

      ogrFeatureD := _ogr_L_GetLayerDefn( ogrLayer ) ;

      ex := GisCommonExtent( _layer.Extent, _extent ) ;
      try
          old_scope := _layer.Scope ;
          _layer.Scope := '' ;

          eloop := _layer.Loop( _extent, _scope, _shape, _de9im ).GetEnumerator() ;
          try
          while eloop.MoveNext do begin // iterate all shapes
              shp := eloop.GetCurrent ;
              shp_tmp := shp.PrepareExportShape(
                              CS, _extent, _truncated, True
                              ) ;
              try
              if assigned( shp_tmp ) and
                  ( not shp_tmp.IsDeleted ) and
                  ( ( _type = shp_tmp.ShapeType   ) or
                      ( _type = TGIS_ShapeType.Unknown )
                  ) then
              begin
                  ogrFeature := _ogr_F_Create( ogrFeatureD ) ;
                  try
                    j := 0 ;
                    for i := 0 to Fields.Count-1 do begin
                      fld := FieldInfo( i ) ;

                      if fld.Deleted    then continue ;
                      if fld.Temporary  then continue ;

                      v := shp_tmp.GetFieldEx( fld.NewName ) ;

                      case fld.FieldType of
                        TGIS_FieldType.String  :
                            _ogr_F_SetFieldString( ogrFeature, j, toORGAPI( VarToString( v ) ) ) ;
                        TGIS_FieldType.Number  :
                            if fld.NewDecimal = 0 then
                            _ogr_F_SetFieldInteger( ogrFeature, j, VarToInt32( v ) )
                            else
                            _ogr_F_SetFieldDouble( ogrFeature, j, VarToDouble( v ) ) ;
                        TGIS_FieldType.Float   :
                            _ogr_F_SetFieldDouble( ogrFeature, j, VarToDouble( v ) ) ;
                        TGIS_FieldType.Boolean :
                            if VarToBoolean( v ) then
                            _ogr_F_SetFieldString( ogrFeature, j, toORGAPI( 'Y' ) )
                            else
                            _ogr_F_SetFieldString( ogrFeature, j, toORGAPI( 'N' ) ) ;
                        TGIS_FieldType.Date    :
                            begin
                              if not VarIsNull(v) then begin
                                DecodeDate( VarToDateTime( v ), year, month, day ) ;
                                DecodeTime( VarToDateTime( v ), h, m, s, ms ) ;
                                _ogr_F_SetFieldDateTime( ogrFeature, j, year, month, day,
                                                            h, m, s, 0
                                                        ) ;
                              end ;
                            end
                      end;
                      inc( j ) ;
                    end ;

                    {$IFDEF OXYGENE}
                      shp_tmp.ExportToWKB( obj ) ;
                      store := OleVariant( obj ) ;
                    {$ELSE}
                      shp_tmp.ExportToWKB( store ) ;
                    {$ENDIF}

                    {$IFDEF OXYGENE}
                      {$IFDEF CLR}
                        ptr := Marshal.UnsafeAddrOfPinnedArrayElement( TBytes( store ), 0 ) ;
                        err := _ogr_G_CreateFromWkb( ptr, ogrSR, ogrGeometry, -1 ) ;
                        if err >= OGRERR_FAILURE then
                          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE )+ #13#10 +
                            Format( '%s', [getError] ), Path, 0 ) ;
                      {$ELSE}
                        ptr := com.sun.jna.Memory.Create( length(TBytes( store )) ) ;
                        ptr.write( 0, TBytes( store ), 0, length(TBytes( store ))) ;
                        err := _ogr_G_CreateFromWkb( ptr, ogrSR, ogrGeometry, -1 ) ;
                        if err >= OGRERR_FAILURE then
                          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE )+ #13#10 +
                            Format( '%s', [getError] ), Path, 0 ) ;
                      {$ENDIF}
                    {$ELSE}
                      {$IFNDEF JAVA}
                        ptr := IntPtr( VarArrayLock( store ) ) ;
                      {$ELSE}
                        ptr := VarArrayLock( store ) ;
                      {$ENDIF}
                      try
                        err := _ogr_G_CreateFromWkb( ptr, ogrSR, ogrGeometry, -1 ) ;
                        if err >= OGRERR_FAILURE then
                          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE )+ #13#10 +
                            Format( '%s', [getError] ), Path, 0 ) ;
                      finally
                        VarArrayUnlock( store ) ;
                      end;
                   {$ENDIF}

                   err := _ogr_F_SetGeometry(ogrFeature, ogrGeometry);
                   if err >= OGRERR_FAILURE then
                     raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE )+ #13#10 +
                       Format( '%s', [getError] ), Path, 0 ) ;

                   err := _ogr_L_CreateFeature( ogrLayer, ogrFeature ) ;
                   if err >= OGRERR_FAILURE then
                     raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE )+ #13#10 +
                       Format( '%s', [getError] ), Path, 0 ) ;
                 finally
                   _ogr_F_Destroy(ogrFeature);
                 end;
              end ;
              finally
                if shp <> shp_tmp then
                    FreeObject( shp_tmp ) ;
              end ;

              if shape_no mod 100 = 1 then begin
                abort := RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
                if abort then break ;
              end ;
              inc( shape_no ) ;
          end ;
          finally
            FreeObject( eloop ) ;
          end ;
      finally
        _ogr_ReleaseDataSource( ogrDS ) ;
        {$IFDEF OXYGENE}
          if sizeOf( IntPtr ) = 8 then
            _osr_DestroySpatialReference_64( ogrSR )
          else
            _osr_DestroySpatialReference_32( ogrSR ) ;
        {$ELSE}
          _osr_DestroySpatialReference( ogrSR ) ;
        {$ENDIF}
        _ogr_CleanupAll ;

        _layer.Scope := old_scope ;

        Items.Clear ;
        Fields.Clear ;

        FIsModified := False ;
        FIsOpened   := False ;
      end ;

    finally
      RaiseBusyRelease( _layer ) ;
    end ;

  end ;

  function TGIS_LayerOGR.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    cnt     : Integer ;
    i       : Integer ;
    ol      : OGRPointerH ;
    cpath   : String ;
    config  : TStringList ;
  begin
    Result := TGIS_LayerInfoList.Create ;

    try
      if IsStringEmpty( FDLLPath ) then
        FDLLPath := GDAL_DEFAULT_DLL_NAME_OGR ;

      if not LoadDLL( FDLLPath ) then exit ;

      _ogr_RegisterAll ;

      if ( CompareText( GetFileExt( Path ), GIS_TTKLS_EXT    ) = 0 ) or
         ( CompareText( GetFileExt( Path ), GIS_TTKLAYER_EXT ) = 0 ) then begin
        config := TStringList.Create ;
        try
          parseConfig( Path, config ) ;
          cpath  := config.Values[GIS_INI_PATH] ;

          cpath := GetPathAbsolute( GetFileDir( Path ), cpath ) ;

          hDS := _ogr_Open( toORGAPI(cpath), 0, pahDriver ) ;
        finally
          FreeObject( config ) ;
        end;
      end
      else begin
        cpath := Path ;
        hDS   := _ogr_Open( toORGAPI(Path), 0, pahDriver ) ;
      end ;

      {$IFNDEF JAVA}
        if ( hDS <> IntPtr(0) ) then begin
      {$ELSE}
        if ( hDS <> nil ) then begin
      {$ENDIF}
          cnt := _ogr_DS_GetLayerCount( hDS ) ;
          for i := 0 to cnt-1 do begin
            ol := _ogr_DS_GetLayer( hDS, i ) ;
            {$IFNDEF JAVA}
              if ( ol <> IntPtr(0) ) then
            {$ELSE}
              if ( ol <> nil ) then
            {$ENDIF}
                Result.Add(
                  TGIS_LayerInfo.Create(
                    asOGRString(_ogr_L_GetName( ol )),
                    TGIS_RegisteredLayerType.Vector,
                    ogrTypeToShp(OGRwkbFlatten(_ogr_L_GetGeomType(ol)))
                  )
                ) ;
          end ;
        _ogr_DS_Destroy( hDS ) ;
        {$IFNDEF JAVA}
          hDS := IntPtr(0) ;
        {$ELSE}
          hDS := nil ;
        {$ENDIF}
      end
    except
      // wrong connection
      on E : EGIS_Exception do
        TGIS_Logger.AsError( GetClassName(Self), E.Message ) ;
    end ;
  end ;

  function TGIS_LayerOGR.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  var
    cpath   : String ;
    config  : TStringList ;
  begin
    Result := inherited PreRecognize( _path, _new_path );

    if IsStringEmpty( FDLLPath ) then
      FDLLPath := GDAL_DEFAULT_DLL_NAME_OGR ;

    if ( CompareText( GetFileExt( _path ), GIS_TTKLS_EXT    ) = 0 ) or
       ( CompareText( GetFileExt( _path ), GIS_TTKLAYER_EXT ) = 0 ) then begin
      config := TStringList.Create ;
      try
        parseConfig( _path, config ) ;
        cpath  := config.Values[GIS_INI_PATH] ;

        if not LoadDLL( FDLLPath ) then begin
          Result := False ;
          exit ;
        end;
        _ogr_RegisterAll ;

        cpath := GetPathAbsolute( GetFileDir( _path ), cpath ) ;

        hDS := _ogr_Open( toORGAPI(cpath), 0, pahDriver ) ;
        {$IFNDEF JAVA}
          if ( hDS = IntPtr(0) ) then
        {$ELSE}
          if ( hDS = nil ) then
        {$ENDIF}
          Result := False
        else begin
          _ogr_DS_Destroy( hDS ) ;
          {$IFNDEF JAVA}
            hDS := IntPtr(0) ;
          {$ELSE}
            hDS := nil ;
          {$ENDIF}
        end ;
      finally
        FreeObject( config ) ;
      end;
    end
    else begin
      if SafeFileExists( _path ) then begin
        if not LoadDLL( FDLLPath ) then begin
          Result := False ;
          exit ;
        end;
        _ogr_RegisterAll ;

        hDS := _ogr_Open( toORGAPI(_path), 0, pahDriver ) ;
        {$IFNDEF JAVA}
          if ( hDS = IntPtr(0) ) then
        {$ELSE}
          if ( hDS = nil ) then
        {$ENDIF}
          Result := False
        else begin
          _ogr_DS_Destroy( hDS ) ;
          {$IFNDEF JAVA}
            hDS := IntPtr(0) ;
          {$ELSE}
            hDS := nil ;
          {$ENDIF}
        end ;
      end
      else if IsServerPath( _path ) or ( CompareText( GetFileExt( _path ), GIS_TTKPS_EXT ) = 0 ) then
        Result := False ;
    end;
  end;

  procedure TGIS_LayerOGR.RevertShapes ;
  var
    i : Integer ;
  begin
    inherited ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorOgr[i].currShape := nil ;
  end ;

  class procedure Unit_GisLayerOGR.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'OGR', 'OGR Wrapper for vector formats', TGIS_LayerOGR,
                   '.*;' + GIS_TTKLAYER_VECTOR_FILTER,
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.&Create ],
                   GIS_OTHER_LAYER_PRIORITY+1, False
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerOGR.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

