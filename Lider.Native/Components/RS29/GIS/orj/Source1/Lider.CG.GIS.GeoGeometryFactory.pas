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
  Encapsulation of geometry translation class.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoGeometryFactory ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoGeometryFactory"'}
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
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Drawing,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Variants,
    System.Classes,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoCsSystems ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}


type

  /// <summary>
  ///   Factory for converting different formats into a shape and reverse.
  /// </summary>
  TGIS_GeometryFactory = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      class function buildShapeFromEWKB(
        {$IFDEF MANAGED}
          const _wkb  : TBytes ;
        {$ELSE}
          const _wkb  : Pointer    ;
        {$ENDIF}
        const _source : TGIS_Shape ;
        {$IFDEF MANAGED}
          const _ptr  : TGIS_Bytes ;
        {$ELSE}
          const _ptr  : Pointer    ;
        {$ENDIF}
        const _mapped : Boolean    ;
        const _uid    : TGIS_Uid    ;
        const _layer  : TGIS_LayerVector
      ) : TGIS_Shape ;

    public

      /// <summary>
      ///   Create the shape from a Well Known Text (WKT).
      /// </summary>
      /// <param name="_wkt">
      ///   WKT text
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDWKT
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function will do basic check; if shape can not be create then
      ///    nil will be returned. Faster version of this function is an
      ///    overload which requires all parameters.
      ///    </note>
      ///    <para>
      ///      For WKT specification see: www.opengis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromWKT
                                   ( const _wkt     : String
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from a Well Known Text (WKT).
      /// </summary>
      /// <param name="_wkt">
      ///   WKT text
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_PARSING
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDWKT
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_DIM
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This overloaded function has similar meaning to TGIS_Shape.Create
      ///    </note>
      ///    <para>
      ///      For WKT specification see: www.opengis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromWKT
                                   ( const _wkt     : String                   ;
                                     const _source  : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       const _ptr   : TGIS_Bytes               ;
                                     {$ELSE}
                                       const _ptr   : Pointer                  ;
                                     {$ENDIF}
                                     const _mapped  : Boolean                  ;
                                     const _uid     : TGIS_Uid                  ;
                                     const _layer   : TGIS_LayerVector
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from a Well Known Text (WKT)
      ///   or an Extended Well Known Text (EWKT) representation of the
      ///   geometry with SRID metadata.
      /// </summary>
      /// <param name="_ewkt">
      ///   EWKT text
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDWKT GIS_RS_ERR_UNSUPPORTEDEWKT
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function will do basic check; if shape can not be create then
      ///    nil will be returned. Faster version of this function is an
      ///    overload which requires all parameters.
      ///    </note>
      ///    <para>
      ///      For WKT specification see: www.opengis.org.
      ///    <para>
      ///    </para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromEWKT
                                   ( const _ewkt    : String
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape geometry from a Well Known Text (WKT)
      ///   or an Extended Well Known Text (EWKT) representation of
      ///   the geometry with SRID metadata.
      /// </summary>
      /// <param name="_ewkt">
      ///   EWKT text
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_PARSING
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDEWKT
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_DIM
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This overloaded function has similar meaning to TGIS_Shape.Create
      ///    </note>
      ///    <para>
      ///      For WKT specification see: www.opengis.org.
      ///    </para>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromEWKT
                                   ( const _ewkt    : String                   ;
                                     const _source  : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       const _ptr   : TGIS_Bytes               ;
                                     {$ELSE}
                                       const _ptr   : Pointer                  ;
                                     {$ENDIF}
                                     const _mapped  : Boolean                  ;
                                     const _uid     : TGIS_Uid                  ;
                                     const _layer   : TGIS_LayerVector
                                   ) : TGIS_Shape ; overload;  static;

      /// <summary>
      ///   Create the shape from Well Known Binary (WKB).
      /// </summary>
      /// <param name="_wkb">
      ///   WKB array
      /// </param>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function will do basic check; if shape can not be create then
      ///    nil will be returned. Faster version of this function is an
      ///    overload which requires all parameters.
      ///    </note>
      ///    <para>
      ///      For WKB specification see: www.opengis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromWKB
                                   ( const _wkb     : OleVariant
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from Well Known Binary (WKB).
      /// </summary>
      /// <param name="_wkb">
      ///   WKB array
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_PARSING
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDWKB
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEWKB
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This overloaded function has similar meaning to TGIS_Shape.Create
      ///    </note>
      ///    <para>
      ///      For WKB specification see: www.opengis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromWKB
                                   ( const _wkb     : OleVariant               ;
                                     const _source  : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       const _ptr   : TGIS_Bytes               ;
                                     {$ELSE}
                                       const _ptr   : Pointer                  ;
                                     {$ENDIF}
                                     const _mapped  : Boolean                  ;
                                     const _uid     : TGIS_Uid                  ;
                                     const _layer   : TGIS_LayerVector
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from Well Known Binary (WKB).
      /// </summary>
      /// <param name="_wkb">
      ///   WKB array
      /// </param>
      /// <param name="_size">
      ///   buffer size
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_PARSING
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDWKB
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEWKB
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This overloaded function has similar meaning to TGIS_Shape.Create
      ///    </note>
      ///    <para>
      ///      For WKB specification see: www.opengis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromWKB
                                   ( {$IFDEF MANAGED}
                                       const _wkb   : TBytes                   ;
                                     {$ELSE}
                                       const _wkb   : Pointer                  ;
                                     {$ENDIF}
                                     const _size    : Integer
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_ewkb">
      ///   EWKB array
      /// </param>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function will do basic check; if shape can not be create then
      ///    nil will be returned. Faster version of this function is an
      ///    overload which requires all parameters.
      ///    </note>
      ///    <para>
      ///      For EWKB specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromEWKB
                                   ( const _ewkb    : OleVariant
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape geometry from an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_ewkb">
      ///   EWKB array
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_PARSING
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDWKB
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEWKB
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This overloaded function has similar meaning to TGIS_Shape.Create
      ///    </note>
      ///    <para>
      ///      For EWKB specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromEWKB
                                   ( const _ewkb    : OleVariant               ;
                                     const _source  : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       const _ptr   : TGIS_Bytes               ;
                                     {$ELSE}
                                       const _ptr   : Pointer                  ;
                                     {$ENDIF}
                                     const _mapped  : Boolean                  ;
                                     const _uid     : TGIS_Uid                  ;
                                     const _layer   : TGIS_LayerVector
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape geometry from an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_ewkb">
      ///   EWKB array
      /// </param>
      /// <param name="_size">
      ///   buffer size
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_PARSING
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDWKB
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEWKB
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This overloaded function has similar meaning to TGIS_Shape.Create
      ///    </note>
      ///    <para>
      ///      For EWKB specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function GisCreateShapeFromEWKB
                                   ( {$IFDEF MANAGED}
                                       const _ewkb  : TBytes                   ;
                                     {$ELSE}
                                       const _ewkb  : Pointer                  ;
                                     {$ENDIF}
                                     const _size    : Integer
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from GeoMedia Database Object (GDO).
      /// </summary>
      /// <param name="_gdo">
      ///   GDO array
      /// </param>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function will do basic check; if shape can not be create
      ///    then nil will be returned. Faster version of this function is an
      ///    overload which requires all parameters.
      ///    </note>
      /// </remarks>
      class function GisCreateShapeFromGDO
                                 ( const _gdo     : OleVariant
                                 ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from GeoMedia Database Object (GDO).
      /// </summary>
      /// <param name="_gdo">
      ///   GDO array
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDGDO
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This overloaded function has similar meaning to TGIS_Shape.Create
      ///    </note>
      /// </remarks>
      class function GisCreateShapeFromGDO
                                 ( const _gdo     : OleVariant               ;
                                   const _source  : TGIS_Shape               ;
                                   {$IFDEF MANAGED}
                                     const _ptr   : TGIS_Bytes               ;
                                   {$ELSE}
                                     const _ptr   : Pointer                  ;
                                   {$ENDIF}
                                   const _mapped  : Boolean                  ;
                                   const _uid     : TGIS_Uid                  ;
                                   const _layer   : TGIS_LayerVector
                                 ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from JSON.
      /// </summary>
      /// <param name="_json">
      ///   JSON text
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDJSON
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_PARSING
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function will do basic check; if shape can not be create then
      ///    nil will be returned. Faster version of this function is an
      ///    overload which requires all parameters.
      ///    </note>
      /// </remarks>
      class function GisCreateShapeFromJSON
                                   ( const _json    : String
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from GeoJSON.
      /// </summary>
      /// <param name="_json">
      ///   JSON text
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDJSON
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTED_PARSING
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This overloaded function has similar meaning to TGIS_Shape.Create
      ///    </note>
      /// </remarks>
      class function GisCreateShapeFromJSON
                                   ( const _json    : String                   ;
                                     const _source  : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       const _ptr   : TGIS_Bytes               ;
                                     {$ELSE}
                                       const _ptr   : Pointer                  ;
                                     {$ENDIF}
                                     const _mapped  : Boolean                  ;
                                     const _uid     : TGIS_Uid                  ;
                                     const _layer   : TGIS_LayerVector
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from GML.
      /// </summary>
      /// <param name="_gml">
      ///   GML text
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDGML
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function will do basic check; if shape can not be create then
      ///    nil will be returned. Faster version of this function is an
      ///    overload which requires all parameters.
      ///    </note>
      /// </remarks>
      class function GisCreateShapeFromGML
                                   ( const _gml    : String
                                   ) : TGIS_Shape ; overload; static;

      /// <summary>
      ///   Create the shape from GML.
      /// </summary>
      /// <param name="_gml">
      ///   GML text
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDGML
      /// </exception>
      /// <returns>
      ///   A new shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This overloaded function has similar meaning to TGIS_Shape.Create
      ///    </note>
      /// </remarks>
      class function GisCreateShapeFromGML
                                   ( const _gml     : String                   ;
                                     const _source  : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       const _ptr   : TGIS_Bytes               ;
                                     {$ELSE}
                                       const _ptr   : Pointer                  ;
                                     {$ENDIF}
                                     const _mapped  : Boolean                  ;
                                     const _uid     : TGIS_Uid                  ;
                                     const _layer   : TGIS_LayerVector
                                   ) : TGIS_Shape ; overload; static;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Create the shape from Shape extended format used in PGDB and
      ///   FileGDB.
      /// </summary>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <returns>
      ///   A new shape.
      /// </returns>
      class function GisCreateShapeFromShapeEx
                                   ( const _ptr   : TGIS_Bytes
                                   ) : TGIS_Shape ; static;

      /// <summary>
      ///   Export the Shape geometry to Shape extended format used in PGDB and
      ///   FileGDB.
      /// </summary>
      /// <param name="_shp">
      ///   exported Shape
      /// </param>
      /// <param name="_gdb">
      ///  exported VAR array
      /// </param>
      class procedure GisExportGeometryToShapeEx
                                   (  const _shp    : TGIS_Shape ;
                                     {$IFDEF MANAGED}
                                       var _gdb    : Object
                                     {$ELSE}
                                       var _gdb    : OleVariant
                                     {$ENDIF}
                                   ) ; static;

      /// <summary>
      ///   Export the Shape geometry into variant geometry. For PostgeSQL ODBC
      ///   driver purposes
      /// </summary>
      /// <param name="_shp">
      ///   exported Shape
      /// </param>
      /// <returns>
      ///   A hex text representation of the shape.
      /// </returns>
      class function  GisExportGeometry2Hex
                                   (  const _shp    : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (WKB).
      /// </summary>
      /// <param name="_shp">
      ///   exported Shape
      /// </param>
      /// <returns>
      ///   A hex text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For WKB specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportWKB2Hex
                                   (  const _shp    : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ver">
      ///   version of EWKT format
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///   <note type="note">
      ///    _ver=0 for concatenated geometry type name and dim type
      ///    e.g.(POINTZ , LINESTRINGM , POLYGONZM )
      ///    </note>
      ///   <note type="note">
      ///    _ver=1 for disjointed geometry type name and dim type e.g.(POINT
      ///    Z, LINESTRING M, POLYGON ZM)
      ///    </note>
      ///    <para>
      ///      For EWKB specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportPointToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     const _ver     : Integer
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKB specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportPointToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportPointToEWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (WKT).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For WKT specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportPointToWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into a GML.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GML specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportPointToGML
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportPointToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportPointToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload;  static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (WKB).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      /// <remarks>
      ///    For WKB specification see: www.opengis.org.
      /// </remarks>
      class procedure GisExportPointToWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _wkb     : Object
                                     {$ELSE}
                                       var _wkb     : OleVariant
                                     {$ENDIF}
                                   ) ; static;

      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportPointToGDO
                                 ( const _shp     : TGIS_Shape               ;
                                   {$IFDEF MANAGED}
                                     var _gdo     : Object
                                   {$ELSE}
                                     var _gdo     : OleVariant
                                   {$ENDIF}
                                 ) ;  static;

      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportPointToVAR
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _var     : Object
                                     {$ELSE}
                                       var _var     : OleVariant
                                     {$ENDIF}
                                   ) ; static;

      /// <summary>
      ///   Export the Shape geometry into a GeoJSON.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GeoJSON specification see: www.geojson.org.
      /// </remarks>
      class function  GisExportPointToJSON
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ver">
      ///   version of EWKT format
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///   <note type="note">
      ///    _ver=0 for concatenated geometry type name and dim type
      ///    e.g.(POINTZ , LINESTRINGM , POLYGONZM )
      ///    </note>
      ///   <note type="note">
      ///    _ver=1 for disjointed geometry type name and dim type e.g.(POINT
      ///    Z, LINESTRING M, POLYGON ZM)
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportMultiPointToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     const _ver     : Integer
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportMultiPointToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportMultiPointToEWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (WKT).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For WKT specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportMultiPointToWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into a GML.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GML specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportMultiPointToGML
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportMultiPointToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportMultiPointToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (WKB).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      /// <remarks>
      ///    For WKB specification see: www.opengis.org.
      /// </remarks>
      class procedure GisExportMultiPointToWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _wkb     : Object
                                     {$ELSE}
                                       var _wkb     : OleVariant
                                     {$ENDIF}
                                   ) ;  static;

      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportMultiPointToGDO
                                 ( const _shp     : TGIS_Shape               ;
                                   {$IFDEF MANAGED}
                                     var _gdo     : Object
                                   {$ELSE}
                                     var _gdo     : OleVariant
                                   {$ENDIF}
                                 ) ;  static;

      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportMultiPointToVAR
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _var     : Object
                                     {$ELSE}
                                       var _var     : OleVariant
                                     {$ENDIF}
                                   ) ;  static;

      /// <summary>
      ///   Export the Shape geometry into a GeoJSON
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GeoJSON specification see: www.geojson.org.
      /// </remarks>
      class function  GisExportMultiPointToJSON
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ver">
      ///   version of EWKT format
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt;= 0
      ///    </note>
      ///   <note type="note">
      ///    _ver=0 for concatenated geometry type name and dim type
      ///    e.g.(POINTZ , LINESTRINGM , POLYGONZM )
      ///    </note>
      ///   <note type="note">
      ///    _ver=1 for disjointed geometry type name and dim type e.g.(POINT
      ///    Z, LINESTRING M, POLYGON ZM)
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportArcToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     const _ver     : Integer
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportArcToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer
                                   ) : String ; overload;static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportArcToEWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (WKT).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For WKT specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportArcToWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ;  static;

      /// <summary>
      ///   Export the Shape geometry into a GML.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GML specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportArcToGML
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_multi">
      ///   force multi geometry structure (multiline envelope)
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportArcToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     const _multi   : Boolean                  ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportArcToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportArcToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload;  static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (WKB).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      /// <remarks>
      ///    For WKB specification see: www.opengis.org.
      /// </remarks>
      class procedure GisExportArcToWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _wkb     : Object
                                     {$ELSE}
                                       var _wkb     : OleVariant
                                     {$ENDIF}
                                   ) ;  static;

      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportArcToGDO
                                 ( const _shp     : TGIS_Shape               ;
                                   {$IFDEF MANAGED}
                                     var _gdo     : Object
                                   {$ELSE}
                                     var _gdo     : OleVariant
                                   {$ENDIF}
                                 ) ; static;

      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportArcToVAR
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _var     : Object
                                     {$ELSE}
                                       var _var     : OleVariant
                                     {$ENDIF}
                                   ) ; static;

      /// <summary>
      ///   Export the Shape geometry into a GeoJSON.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GeoJSON specification see: www.geojson.org.
      /// </remarks>
      class function  GisExportArcToJSON
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ver">
      ///   version of EWKT format
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///   <note type="note">
      ///    _ver=0 for concatenated geometry type name and dim type
      ///    e.g.(POINTZ , LINESTRINGM , POLYGONZM )
      ///    </note>
      ///   <note type="note">
      ///    _ver=1 for disjointed geometry type name and dim type e.g.(POINT
      ///    Z, LINESTRING M, POLYGON ZM)
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportPolygonToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     const _ver     : Integer
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportPolygonToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportPolygonToEWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (WKT).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For WKT specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportPolygonToWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into a GML.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GML specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportPolygonToGML
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_multi">
      ///   force multi geometry structure (multipolygon envelope)
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportPolygonToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     const _multi   : Boolean                  ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportPolygonToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportPolygonToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (WKB).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      /// <remarks>
      ///    For WKB specification see: www.opengis.org.
      /// </remarks>
      class procedure GisExportPolygonToWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _wkb     : Object
                                     {$ELSE}
                                       var _wkb     : OleVariant
                                     {$ENDIF}
                                   ) ; static;

      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportPolygonToVAR
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _var     : Object
                                     {$ELSE}
                                       var _var     : OleVariant
                                     {$ENDIF}
                                   ) ; static;

      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportPolygonToGDO
                                 ( const _shp     : TGIS_Shape               ;
                                   {$IFDEF MANAGED}
                                     var _gdo     : Object
                                   {$ELSE}
                                     var _gdo     : OleVariant
                                   {$ENDIF}
                                 ) ; static;

      /// <summary>
      ///   Export the Shape geometry into a GeoJSON.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GeoJSON specification see: www.geojson.org.
      /// </remarks>
      class function  GisExportPolygonToJSON
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ver">
      ///   version of EWKT format
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///   <note type="note">
      ///    _ver=0 for concatenated geometry type name and dim type
      ///    e.g.(POINTZ , LINESTRINGM , POLYGONZM )
      ///    </note>
      ///   <note type="note">
      ///    _ver=1 for disjointed geometry type name and dim type e.g.(POINT
      ///    Z, LINESTRING M, POLYGON ZM)
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportComplexToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     const _ver     : Integer
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportComplexToEWKT
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Text (EWKT)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    This function return Well Known Text (WKT) for SRID &lt; 0
      ///    </note>
      ///    <para>
      ///      For EWKT specification see: www.postgis.org.
      ///    </para>
      /// </remarks>
      class function  GisExportComplexToEWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (WKT).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For WKT specification see: www.opengis.org.
      /// </remarks>
     class function  GisExportComplexToWKT
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into a GML.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GML specification see: www.opengis.org.
      /// </remarks>
      class function  GisExportComplexToGML
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_srid">
      ///   spatial referencing system identifier
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportComplexToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     const _srid    : Integer                  ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into an Extended Well Known Binary (EWKB)
      ///   representation of the geometry with SRID metadata.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_ewkb">
      ///   exported EWKB array
      /// </param>
      /// <remarks>
      ///    For EWKB specification see: www.postgis.org.
      /// </remarks>
      class procedure GisExportComplexToEWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _ewkb    : Object
                                     {$ELSE}
                                       var _ewkb    : OleVariant
                                     {$ENDIF}
                                   ) ; overload; static;

      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (WKB).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      /// <remarks>
      ///    For WKB specification see: www.opengis.org.
      /// </remarks>
      class procedure GisExportComplexToWKB
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _wkb     : Object
                                     {$ELSE}
                                       var _wkb     : OleVariant
                                     {$ENDIF}
                                   ) ; static;

      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportComplexToGDO
                                 ( const _shp     : TGIS_Shape               ;
                                   {$IFDEF MANAGED}
                                     var _gdo     : Object
                                   {$ELSE}
                                     var _gdo     : OleVariant
                                   {$ENDIF}
                                 ) ;  static;

      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportComplexToVAR
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _var     : Object
                                     {$ELSE}
                                       var _var     : OleVariant
                                     {$ENDIF}
                                   ) ; static;

      /// <summary>
      ///   Export the Shape geometry into a GeoJSON.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   A text representation of the shape.
      /// </returns>
      /// <remarks>
      ///    For GeoJSON specification see: www.geojson.org.
      /// </remarks>
      class function  GisExportComplexToJSON
                                   ( const _shp     : TGIS_Shape
                                   ) : String ; static;

      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportMultiPatchToVAR
                                   ( const _shp     : TGIS_Shape               ;
                                     {$IFDEF MANAGED}
                                       var _var     : Object
                                     {$ELSE}
                                       var _var     : OleVariant
                                     {$ENDIF}
                                   ) ; static;

      /// <summary>
      ///   Create a reprojected shape.
      /// </summary>
      /// <param name="_shp">
      ///   source shape to reproject
      /// </param>
      /// <param name="_src_cs">
      ///   source coordinate system
      /// </param>
      /// <param name="_dst_cs">
      ///   destination coordinate system
      /// </param>
      /// <returns>
      ///   A new shape.
      /// </returns>
      class function  GisCreateReprojectedShape
                                   ( const _shp     : TGIS_Shape               ;
                                     const _src_cs  : TGIS_CSCoordinateSystem  ;
                                     const _dst_cs  : TGIS_CSCoordinateSystem
                                   ) : TGIS_Shape ; static;

      /// <summary>
      ///   Build a polygon from a list of non complex edges.
      /// </summary>
      /// <param name="_edges">
      ///   list of non complex edges of TGIS_ShapeArc type
      /// </param>
      /// <param name="_shapeType">
      ///   output shape type, TGIS_ShapeType.Arc or TGIS_ShapeType.Polygon
      /// </param>
      /// <param name="_tolerance">
      ///   tolerance into which two arcs are close enough to be joined
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <param name="_fixShape">
      ///   if True, the newly created shape will be fixed using TGIS_Topology.FixShape
      /// </param>
      /// <returns>
      ///   A new shape.
      /// </returns>
      class function  GisBuildShapeFromEdges
                                   ( const _edges     : TGIS_ObjectList ;
                                     const _shapeType : TGIS_ShapeType ;
                                     const _tolerance : Double ;
                                     const _source    : TGIS_Shape ;
                                     {$IFDEF MANAGED}
                                       const _ptr     : TGIS_Bytes ;
                                     {$ELSE}
                                       const _ptr     : Pointer    ;
                                     {$ENDIF}
                                     const _mapped    : Boolean    ;
                                     const _uid       : TGIS_Uid    ;
                                     const _layer     : TGIS_LayerVector ;
                                     const _fixShape  : Boolean
                                   ) : TGIS_Shape ; static;

      /// <summary>
      ///   Calculate circle based on any three points from the circle outline.
      /// </summary>
      /// <param name="_ptg1">
      ///   first provided point
      /// </param>
      /// <param name="_ptg2">
      ///   second provided point
      /// </param>
      /// <param name="_ptg3">
      ///   third provided point
      /// </param>
      /// <param name="_center">
      ///   center of the calculated string
      /// </param>
      /// <param name="_radius">
      ///   radius of the calculated string
      /// </param>
      /// <param name="_start">
      ///   start angle for pie (currently always 0)
      /// </param>
      /// <param name="_stop">
      ///   stop angle for pie (currently always 2Pi)
      /// </param>
      /// <returns>
      ///   True if can build a circle from 3 points.
      /// </returns>
      class function GisCircleFrom3Points( const _ptg1   : TGIS_Point ;
                                           const _ptg2   : TGIS_Point ;
                                           const _ptg3   : TGIS_Point ;
                                           var _center   : TGIS_Point ;
                                           var _radius   : Double     ;
                                           var _start    : Double     ;
                                           var _stop     : Double
                                          ) : Boolean; static;

      /// <summary>
      ///   Calculate circle based on any three 3D points from the circle
      ///   outline.
      /// </summary>
      /// <param name="_ptg1">
      ///   first provided point
      /// </param>
      /// <param name="_ptg2">
      ///   second provided point
      /// </param>
      /// <param name="_ptg3">
      ///   third provided point
      /// </param>
      /// <param name="_center">
      ///   center of the calculated string
      /// </param>
      /// <param name="_radius">
      ///   radius of the calculated string
      /// </param>
      /// <param name="_start">
      ///   start angle for pie (currently always 0)
      /// </param>
      /// <param name="_stop">
      ///   stop angle for pie (currently always 2Pi)
      /// </param>
      /// <returns>
      ///   True if can build a circle from 3 points.
      /// </returns>
      class function GisCircleFrom3Points3D( const _ptg1   : TGIS_Point3D ;
                                             const _ptg2   : TGIS_Point3D ;
                                             const _ptg3   : TGIS_Point3D ;
                                             var _center   : TGIS_Point3D ;
                                             var _radius   : Double     ;
                                             var _start    : Double     ;
                                             var _stop     : Double
                                            ) : Boolean; static;

      /// <summary>
      ///   Calculate arc based on any three 3D points from the circle outline.
      /// </summary>
      /// <param name="_ptg1">
      ///   first provided point
      /// </param>
      /// <param name="_ptg2">
      ///   second provided point
      /// </param>
      /// <param name="_ptg3">
      ///   third provided point
      /// </param>
      /// <param name="_center">
      ///   center of the calculated string
      /// </param>
      /// <param name="_radius">
      ///   radius of the calculated string
      /// </param>
      /// <param name="_start">
      ///   start angle for pie
      /// </param>
      /// <param name="_stop">
      ///   stop angle for pie
      /// </param>
      /// <returns>
      ///   True if can build an arc from 3 points.
      /// </returns>
      class function GisArcFrom3Points3D   ( const _ptg1   : TGIS_Point3D ;
                                             const _ptg2   : TGIS_Point3D ;
                                             const _ptg3   : TGIS_Point3D ;
                                             var _center   : TGIS_Point3D ;
                                             var _radius   : Double     ;
                                             var _start    : Double     ;
                                             var _stop     : Double
                                            ) : Boolean ; static;

      /// <summary>
      ///   Calculate arc based on any three points from the circle outline.
      /// </summary>
      /// <param name="_ptg1">
      ///   first provided point
      /// </param>
      /// <param name="_ptg2">
      ///   second provided point
      /// </param>
      /// <param name="_ptg3">
      ///   third provided point
      /// </param>
      /// <param name="_center">
      ///   center of the calculated string
      /// </param>
      /// <param name="_radius">
      ///   radius of the calculated string
      /// </param>
      /// <param name="_start">
      ///   start angle for pie
      /// </param>
      /// <param name="_stop">
      ///   stop angle for pie
      /// </param>
      /// <returns>
      ///   True if can build an arc from 3 points.
      /// </returns>
      class function GisArcFrom3Points     ( const _ptg1   : TGIS_Point ;
                                             const _ptg2   : TGIS_Point ;
                                             const _ptg3   : TGIS_Point ;
                                             var _center   : TGIS_Point ;
                                             var _radius   : Double     ;
                                             var _start    : Double     ;
                                             var _stop     : Double
                                            ) : Boolean ; static;

      /// <summary>
      ///   Check polygon parts to verify main part, islands, holes and if it is
      ///  a single polygon or multipolygon shape.
      /// </summary>
      /// <param name="_shp">
      ///   shape instance
      /// </param>
      /// <param name="_parts">
      ///   array of parts, will be allocated and filled with status value :
      ///   0 for main part or island, 1 for hole
      /// </param>
      /// <param name="_winding">
      ///   array of windings, will be allocated and filled with status value :
      ///   -1 for counter-clockwise, 1 for clockwise orientation
      /// </param>
      /// <returns>
      ///   True if shape is multipolygon, False if polygon.
      /// </returns>
      /// <remarks>
      ///   Function is used upon shape write in various formats to decide
      ///   whether to use multipolygon or single polygon and based on parts
      ///   status how to define rings and winding orientation.
      /// </remarks>
      class function GisPolygonPartsStatus(  const _shp    : TGIS_Shape ;
                                             var _parts    : TGIS_IntegerArray ;
                                             var _winding  : TGIS_IntegerArray
                                            ) : Boolean ; static;

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.SysUtils,

    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoFileJSON,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoTopology,
    Lider.CG.GIS.GeoXmlDoc ;
{$ENDIF}

const
  { Basic offset in WKB Geometry. }
    OFFSET_WKB         = sizeOf( Byte ) + sizeOf( DWORD ) ;
  { Basic offset in WKB Multi-Geometry. }
    OFFSET_WKB_MULTI   = sizeOf( Byte ) + sizeOf( DWORD ) + sizeOf( DWORD ) ;
  { Basic offset in EWKB Geometry. }
    OFFSET_EWKB        = sizeOf( Byte ) + sizeOf( DWORD ) + sizeOf( DWORD ) ;
  { Basic offset in EWKB Multi-Geometry. }
    OFFSET_EWKB_MULTI  = sizeOf( Byte ) + sizeOf( DWORD ) + sizeOf( DWORD ) +
                         sizeOf( DWORD ) ;

  { Basic offsets in geometry binary representation -  size of GUID. }
    OFFSET_GDO_GUID    = 16 ;
  { Basic offsets in geometry binary representation -  size of GDO point. }
    OFFSET_GDO_POINT   = 3 * sizeOf( Double ) ;

  // Flags applied in EWKB to indicate Z/M dimensions and
  // presence/absence of SRID and bounding boxes
  { All flags in 5th byte of EWKB format. }
    EWKB_ALL_FLAGS    = $E0000000 ;
  { Z offset flag for EWKB format. }
    EWKB_ZOFFSET_FLAG = $80000000 ;
  { M offset flag for EWKB format. }
    EWKB_MOFFSET_FLAG = $40000000 ;
  { SRID flag for EWKB format. }
    EWKB_SRID_FLAG    = $20000000 ;
  { BBOX flag for EWKB format. }
    EWKB_BBOX_FLAG    = $10000000 ;

//==============================================================================
// utility private functions
//==============================================================================

  {$IFDEF MANAGED}

    // Read entity from WKB memory.
    // _b     source memory
    // _o     offset in source memory (will be incremented after operation)
    // _v     variable to be read
    // _count number of bytes
    procedure read_wkb( const _b      : TBytes        ;
                        var   _o      : Integer       ;
                        var   _v      : array of Byte ;
                        const _count  : Cardinal )    ;
    begin
      GisCopyMemory( _b, _o, _v, 0, _count );
      _o := _o + Integer(_count) ;
    end ;
  {$ELSE}

    // Read entity from WKB memory.
    // _p     memory pointer (will be incremented after operation)
    // _v     variable to be read
    // _count number of bytes
    procedure read_wkb( var _p : Pointer; var _v; const _count : Cardinal ) ;
    begin
      Move( _p^, _v, _count ) ;
      _p := Pointer( NativeInt( _p ) + Integer(_count) ) ;
    end ;
  {$ENDIF}

  {$IFDEF MANAGED}

    // Write entity into WKB memory.
    // _b     destination memory
    // _o     offset in destination memory (will be incremented after
    //              operation)
    // _v     variable to be written
    // _count number of bytes
    procedure write_wkb( var   _b     : TBytes        ;
                         var   _o     : Integer       ;
                         const _v     : array of Byte ;
                         const _count : Cardinal )    ;
    begin
      GisCopyMemory( _v, 0, _b, _o, _count ) ;
      _o := _o + Integer(_count) ;
    end ;

    procedure write_wkb( var   _b     : TBytes        ;
                         var   _o     : Integer       ;
                         const _v     : array of Byte ;
                         const _vo    : Integer       ;
                         const _count : Cardinal )    ;
    begin
      GisCopyMemory( _v, _vo, _b, _o, _count ) ;
      _o := _o + Integer(_count) ;
    end ;
  {$ELSE}

    // Write entity into WKB memory.
    // _p     memory pointer (will be incremented after operation)
    // _v     variable to be written
    // _count number of bytes
    procedure write_wkb(
      var   _p     : Pointer  ;
      const _v                ;
      const _count : Cardinal
    ) ;
    begin
      Move( _v, _p^, _count ) ;
      _p := Pointer( NativeInt( _p ) + Integer(_count) ) ;
    end ;
  {$ENDIF}

  {$IFDEF MANAGED}
   procedure write_fakebuf( var   _p    : TBytes ;
                            var   _o    : Integer ;
                            const _size : Cardinal
                          ) ;
  {$ELSE}
   procedure write_fakebuf( var   _p    : Pointer ;
                            const _size : Cardinal
                          ) ;
  {$ENDIF}
   var
     buf : TBytes ;
    {$IFDEF MANAGED}
      i : Integer ;
    {$ENDIF}
   begin
     if _size > 0 then begin
        SetLength( buf, _size ) ;
       {$IFDEF MANAGED}
         for i := 0 to _size - 1 do
           buf[ i ] := 0 ;
         write_wkb( _p, _o, buf, _size );
       {$ELSE}
         FillChar( buf[0], _size, 0 ) ;
         {$IFDEF MSWINDOWS}
           CopyMemory( _p, @buf[0], _size ) ;
         {$ELSE}
           Move( buf, _p^, _size ) ;
         {$ENDIF}
         _p := Pointer( NativeInt( _p ) + Integer(_size) ) ;
       {$ENDIF}
       buf := nil ;
     end ;
   end ;

  {$IFDEF MANAGED}

    // Read entity from GDO memory.
    // _b     source memory
    // _o     offset in source memory (will be incremented after
    //               operation)
    // _v     variable to be read
    // _count number of bytes
    procedure read_gdo(
      const _b      : TBytes        ;
      var   _o      : Integer       ;
      var   _v      : array of Byte ;
      const _count  : Cardinal
    ) ;
    begin
      GisCopyMemory( _b, _o, _v, 0, _count );
      _o := _o + Integer( _count ) ;
    end ;
  {$ELSE}

    // Read entity from GDO memory.
    // _p     memory pointer (will be incremented after operation)
    // _v     variable to be read
    // _count number of bytes
    procedure read_gdo(
      var _p     : Pointer  ;
      var _v                ; const
          _count : Cardinal
    ) ;
    begin
      Move( _p^, _v, _count ) ;
      _p := Pointer( NativeInt( _p ) + Integer(_count) ) ;
    end ;
  {$ENDIF}

  {$IFDEF MANAGED}

    // Write entity into GDO memory.
    // _b     destination memory
    // _o     offset in destination memory (will be incremented after
    //               operation)
    // _v     variable to be written
    // _count number of bytes
    procedure write_gdo(
      var   _b     : TBytes        ;
      var   _o     : Integer       ;
      const _v     : array of Byte ;
      const _count : Cardinal
    )  ;
    begin
      GisCopyMemory( _v, 0, _b, _o, _count ) ;
      _o := _o + Integer( _count ) ;
    end ;
  {$ELSE}

    // Write entity into GDO memory.
    // _p     memory pointer (will be incremented after operation)
    // _v     variable to be written
    // _count number of bytes
    procedure write_gdo(
      var   _p     : Pointer ;
      const _v               ;
      const _count : Cardinal
    ) ;
    begin
      Move( _v, _p^, _count ) ;
      _p := Pointer( NativeInt( _p ) + Integer(_count)) ;
    end ;
  {$ENDIF}

  {$IFDEF MANAGED}
    procedure write_dword (
      var _b : TBytes  ;
      var _o : Integer ;
          _v : DWORD
    ) ;
    var
      a : array of Byte ;
    begin
      SetLength( a, 4 ) ;
      a[0] := Byte(_v)        ;
      a[1] := Byte(_v shr 8)  ;
      a[2] := Byte(_v shr 16) ;
      a[3] := Byte(_v shr 24) ;
      write_gdo( _b, _o, a, 4 ) ;
    end ;

    procedure write_double(
     var _b : TBytes ;
     var _o : Integer ;
         _v : Double
    ) ;
    var
      a : array of Byte ;
    begin
      SetLength( a, 8 ) ;
      a := BitConverter.GetBytes( _v ) ;
      write_gdo( _b, _o, a, 8 ) ;
    end ;

    procedure write_point(
      var _b : TBytes ;
      var _o : Integer ;
          _v : TGIS_Point
    ) ;
    var
      a : array of Byte ;
    begin
      SetLength( a, 8 ) ;
      a := BitConverter.GetBytes( _v.X ) ;
      write_gdo( _b, _o, a, 8 ) ;
      a := BitConverter.GetBytes( _v.Y ) ;
      write_gdo( _b, _o, a, 8 ) ;
    end ;
  {$ENDIF}

  function isClockwisePart(
    const _shp    : TGIS_Shape ;
    const _partNo : Integer
  ) : Boolean ;
  var
    point_no     : Integer ;
    next_pt      : Integer ;
    points_count : Integer ;
    area2        : Double ;
    line_a       : TGIS_Point ;
    line_b       : TGIS_Point ;
    point_origin : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
  begin
    // result is absolute value of count of area of all trapezoids
    // makes by each polygon line
    area2 := 0 ;

    // to avoid big numbers - make it relative to (0,0)

    point_origin.X := -_shp.ProjectedExtent.XMin;
    point_origin.Y := -_shp.ProjectedExtent.YMin;

    Result := False ;

    points_count := _shp.GetPartSize( _partNo );
    for point_no := 0 to points_count - 2 do begin  // all points
      next_pt := point_no +1;
      line_a := _shp.GetPoint(_partNo, point_no) ; // beginning of line
      line_a.X := line_a.X + point_origin.X;
      line_a.Y := line_a.Y + point_origin.Y;
      line_b := _shp.GetPoint(_partNo, next_pt);   // end of line
      line_b.X := line_b.X + point_origin.X;
      line_b.Y := line_b.Y + point_origin.Y;
      area2 := area2 + (line_b.Y*line_a.X -line_a.Y*line_b.X) ;
    end ;

    if area2 < 0 then
      Result := True ;
  end ;

    function circularSegmentizeEx(
      const _p1      : TGIS_Point3D ;
      const _p2      : TGIS_Point3D ;
      const _p3      : TGIS_Point3D ;
      const _center  : TGIS_Point3D ;
      const _radius  : Double ;
      const _perQuad : Integer
     ) : TGIS_Point3DList ;
     const
      M_PI   = Pi ;
      M_PI_2 = Pi/2 ;
    var
       a1, a2,
       a3, sweep,
       increment,
       angle       : Double ;
       ptcount, i  : Integer ;
       rsin, rcos  : Double ;
    begin
      Result := TGIS_Point3DList.Create ;

      if _radius < 0 then begin
        Result.Add( _p1 ) ;
        Result.Add( _p2 ) ;
        exit ;
      end ;

      a1 := ArcTan2( _p1.Y - _center.Y, _p1.X - _center.X ) ;
      a2 := ArcTan2( _p2.Y - _center.Y, _p2.X - _center.X ) ;
      a3 := ArcTan2( _p3.Y - _center.Y, _p3.X - _center.X ) ;

      if ( Abs( _p1.X - _p3.X ) < 1.0e-8 ) and ( Abs( _p1.Y - _p3.Y ) < 1.0e-8 ) then
        sweep := 2*Pi
      else if(a1 > a2) and (a2 > a3) then
        sweep := a3 - a1
      else if (a1 < a2) and (a2 < a3) then
        sweep := a3 - a1
      else if ((a1 < a2) and (a1 > a3)) or ((a2 < a3) and (a1 > a3)) then
        sweep := a3 - a1 + 2*Pi
      else if ((a1 > a2) and (a1 < a3)) or ((a2 > a3) and (a1 < a3)) then
        sweep := a3 - a1 - 2*Pi
      else
        sweep := 0.0;

      ptcount := FloorS( Abs(sweep * _perQuad / M_PI_2 ) ) - 1 ;

      increment := M_PI_2 / Max( 4, _perQuad ) ;
      if (sweep < 0) then
        increment := increment * -1.0 ;
      angle := a1 ;

      Result.Add( _p1 )  ;
      for i := 0 to ptcount - 1 do begin
        angle := angle + increment ;
        if (increment > 0.0) and ( angle > Pi) then
          angle := angle - 2*Pi;
        if (increment < 0.0 ) and ( angle < -1*Pi) then
          angle := angle - 2*Pi;

        SinCos( angle, rsin, rcos ) ;
        Result.Add( GisPoint3D( _center.X + _radius * rcos,
                                _center.Y + _radius * rsin,
                                0
                              )
                   ) ;
      end ;
      Result.Add( _p3 )  ;
    end ;

    function circularSegmentize(
      const _p1 : TGIS_Point3D ;
      const _p2 : TGIS_Point3D ;
      const _p3 : TGIS_Point3D
     ) : TGIS_Point3DList ;
    var
      center  : TGIS_Point3D ;
      dradius : Double ;
      start   : Double ;
      stop    : Double ;
    begin
      if TGIS_GeometryFactory.GisArcFrom3Points3D( _p1, _p2, _p3, center, dradius, start, stop ) then
        Result := circularSegmentizeEx( _p1, _p2, _p3, center, dradius, 32 )
      else begin
        Result := TGIS_Point3DList.Create ;
        Result.Add(_p1) ;
        Result.Add(_p3) ;
      end ;
    end ;

//==============================================================================
// public functions
//==============================================================================

  class function TGIS_GeometryFactory.GisCreateShapeFromGDO(
    const _gdo    : OleVariant ;
    const _source : TGIS_Shape ;
    {$IFDEF MANAGED}
      const _ptr  : TGIS_Bytes ;
    {$ELSE}
      const _ptr  : Pointer ;
    {$ENDIF}
    const _mapped : Boolean ;
    const _uid    : TGIS_Uid ;
    const _layer  : TGIS_LayerVector
  ) : TGIS_Shape ;
  var
    {$IFDEF MANAGED}
      vr         : TBytes  ;
      vr_off     : Integer ;
      vr_off_tmp : Integer ;
    {$ELSE}
      ptvar      : Pointer ;
      ptvar_tmp  : Pointer ;
    {$ENDIF}
    gdoType    : Integer ;
    le         : Boolean ;
    multiParts : Boolean ;
    collection : Boolean ;
    res        : TGIS_Shape ;

    {$IFDEF MANAGED}
      procedure skip_bytes(
        var   _o     : Integer ;
        const _count : Cardinal
      ) ;
      begin
        _o := _o + Integer(_count) ;
      end ;
    {$ELSE}
      procedure skip_bytes(
        var   _p     : Pointer ;
        const _count : Cardinal
      ) ;
      begin
        _p := Pointer( NativeInt( _p ) + Integer(_count) ) ;
      end ;
    {$ENDIF}

    function read_byte
      : Byte ;
    {$IFDEF MANAGED}
      var
        {$IFDEF OXYGENE}
          b : array of Byte := new Byte[ sizeOf( Byte ) ] ;
        {$ELSE}
          b : array[0..0] of Byte ;
        {$ENDIF}
    {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_gdo( vr, vr_off, b, b.Length ) ;
        Result := b[0] ;
      {$ELSE}
        read_gdo( ptvar, Result, sizeOf( Result ) ) ;
      {$ENDIF}
    end ;

    function read_word_LE
      : Word ;
    {$IFDEF MANAGED}
      var
        {$IFDEF OXYGENE}
          a : array of Byte := new Byte[ sizeOf( Word ) ] ;
        {$ELSE}
          a : array[0..1] of Byte ;
        {$ENDIF}
        ret : Integer ;
    {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_gdo( vr, vr_off, a, a.Length ) ;
        ret := Integer(($FF and a[0]) or (($FF and a[1]) shl 8)) ;
        Result := ret ;
      {$ELSE}
        read_gdo( ptvar, Result, sizeOf( Result ) ) ;
      {$ENDIF}
    end ;

    function read_word_BE
      : Word ;
    var
      {$IFDEF MANAGED}
        {$IFDEF OXYGENE}
          a : array of Byte := new Byte[ sizeOf( Word ) ] ;
        {$ELSE}
          a : array[0..1] of Byte ;
        {$ENDIF}
      {$ELSE}
        p : Pointer ;
        b : Byte ;
      {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_gdo( vr, vr_off, a, a.Length ) ;
        Result := Integer(($FF and a[1]) or (($FF and a[0]) shl 8));
      {$ELSE}
        read_gdo( ptvar, Result, sizeOf( Result ) ) ;

        p := @Result ;

        b := Byte(Pointer(NativeInt(p) + 0)^) ;
        Byte(Pointer(NativeInt(p) + 0)^) := Byte(Pointer(NativeInt(p) + 1)^) ;
        Byte(Pointer(NativeInt(p) + 1)^) := b ;
      {$ENDIF}
    end ;

    function read_word
      : Word ;
    begin
     if le then Result := read_word_LE
           else Result := read_word_BE ;
    end ;

    function read_dword_LE
      : DWORD ;
    {$IFDEF MANAGED}
      var
        {$IFDEF OXYGENE}
          a : array of Byte := new Byte[ sizeOf( DWORD ) ] ;
        {$ELSE}
          a : array[0..3] of Byte ;
        {$ENDIF}
    {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_gdo( vr, vr_off, a, a.Length ) ;
        Result := Integer(  ($FF and a[0])         or
                           (($FF and a[1]) shl 8 ) or
                           (($FF and a[2]) shl 16) or
                           (($FF and a[3]) shl 24)
                         );
      {$ELSE}
        read_gdo( ptvar, Result, sizeOf( Result ) ) ;
      {$ENDIF}
    end ;

    function read_dword_BE
      : DWORD ;
    var
      {$IFDEF MANAGED}
        {$IFDEF OXYGENE}
          a : array of Byte := new Byte[ sizeOf( DWORD ) ] ;
        {$ELSE}
          a : array[0..3] of Byte ;
        {$ENDIF}
      {$ELSE}
        p : Pointer ;
        b : Byte ;
      {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_gdo( vr, vr_off, a, a.Length ) ;
        Result := Integer(  ($FF and a[3])         or
                           (($FF and a[2]) shl 8 ) or
                           (($FF and a[1]) shl 16) or
                           (($FF and a[0]) shl 24)
                         );
      {$ELSE}
        read_gdo( ptvar, Result, sizeOf( Result ) ) ;

        p := @Result ;

        b := Byte(Pointer(NativeInt(p) + 0)^) ;
        Byte(Pointer(NativeInt(p) + 0)^) := Byte(Pointer(NativeInt(p) + 3)^) ;
        Byte(Pointer(NativeInt(p) + 3)^) := b ;

        b := Byte(Pointer(NativeInt(p) + 1)^) ;
        Byte(Pointer(NativeInt(p) + 1)^) := Byte(Pointer(NativeInt(p) + 2)^) ;
        Byte(Pointer(NativeInt(p) + 2)^) := b ;
      {$ENDIF}
    end ;

    function read_dword
      : DWORD ;
    begin
     if le then Result := read_dword_LE
           else Result := read_dword_BE ;
    end ;

    function read_double_LE
      : Double ;
    {$IFDEF MANAGED}
      var
        {$IFDEF OXYGENE}
          a : Array of Byte := new Byte[ sizeOf( Double ) ] ;
        {$ELSE}
          a : Array[0..7] of Byte ;
        {$ENDIF}
    {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_gdo( vr, vr_off, a, a.Length ) ;
        Result := BitConverter.ToDouble( a, 0 ) ;
      {$ELSE}
        read_gdo( ptvar, Result, sizeOf( Result ) ) ;
      {$ENDIF}
    end ;

    function read_double_BE
      : Double ;
    var
      {$IFDEF OXYGENE}
        a : Array of Byte := new Byte[ sizeOf( Double ) ] ;
      {$ELSE}
        a : Array[0..7] of Byte ;
      {$ENDIF}
      i   : Integer ;
      {$IFDEF MANAGED}
        b : Byte ;
      {$ENDIF}
    begin
      {$IFDEF MANAGED}
        assert( a.Length = 8 ) ;
        assert( sizeOf(Double) = 8 ) ;

        read_gdo( vr, vr_off, a, 8 ) ;
        for i := 0 to 3 do begin
          b      := a[i  ] ;
          a[i  ] := a[7-i] ;
          a[7-i] := b      ;
        end ;
        Result := BitConverter.ToDouble( a, 0 ) ;
      {$ELSE}
        assert( sizeOf(a     ) = 8 ) ;
        assert( sizeOf(Double) = 8 ) ;

        read_gdo( ptvar, a, 8 ) ;

        for i:=7 downto 0 do // swap bytes
          Byte(Pointer( NativeInt( @Result ) + i)^) := a[7-i] ;
      {$ENDIF}
    end ;

    function read_double
      : Double ;
    begin
     if le then Result := read_double_LE
           else Result := read_double_BE ;
    end ;

    function read_gdotype
      : Word ;
    begin
      Result := read_word ;
      {$IFDEF MANAGED}
        skip_bytes( vr_off, 14 ) ;
      {$ELSE}
        skip_bytes( ptvar, 14 ) ;
      {$ENDIF}
    end ;

    function read_guid
      : TGUID ;
    var
      {$IFDEF MANAGED}
        d1 : Integer ;
        d2 : SmallInt ;
        d3 : SmallInt ;
        {$IFDEF OXYGENE}
          {$IFDEF JAVA OR ISLAND}
            d4 : array of Byte := new Byte[ 7 ] ;
          {$ENDIF}
          {$IFDEF CLR}
            d4 : array of Byte := new Byte[ sizeOf( TGUID ) ] ;
          {$ENDIF}
        {$ELSE}
          d4 : array[0..7] of Byte ;
        {$ENDIF}
      {$ELSE}
        i: Integer ;
      {$ENDIF}
    begin
      {$IFDEF MANAGED}
        d1 := read_dword ;
        d2 := read_word ;
        d3 := read_word ;
        read_gdo( vr, vr_off, d4, d4.Length ) ;
        {$IFDEF JAVA}
          Result := TGUID.randomUUID() ;
        {$ELSE}
          Result := TGUID.Create( d1, d2, d3, d4 ) ;
        {$ENDIF}
      {$ELSE}
        Result.D1 := read_dword ;
        Result.D2 := read_word ;
        Result.D3 := read_word ;
        for i:=0 to 7 do
          Result.D4[i] := read_byte ;
      {$ENDIF}
    end ;

    function read_ptg_LE
      : TGIS_Point ;
    begin
      {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
      {$ENDIF}
      Result.X := read_double_LE ;
      Result.Y := read_double_LE ;
    end ;

    function read_ptg_BE
      : TGIS_Point ;
    begin
      {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
      {$ENDIF}
      Result.X := read_double_BE ;
      Result.Y := read_double_BE ;
    end ;

    function read_ptg
      : TGIS_Point ;
    begin
     if le then Result := read_ptg_LE
           else Result := read_ptg_BE ;
    end ;

    function read_ptg3D_LE
      : TGIS_Point3D ;
    begin
      {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
      {$ENDIF}
      Result.X := read_double_LE ;
      Result.Y := read_double_LE ;
      Result.Z := read_double_LE ;
      Result.M := 0 ;
    end ;

    function read_ptg3D_BE
      : TGIS_Point3D ;
    begin
      {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
      {$ENDIF}
      Result.X := read_double_BE ;
      Result.Y := read_double_BE ;
      Result.Z := read_double_BE ;
      Result.M := 0 ;
    end ;

    function read_ptg3D
      : TGIS_Point3D ;
    begin
     if le then Result := read_ptg3D_LE
           else Result := read_ptg3D_BE ;
    end ;

    function read_string(
      const _size: Integer
    ) : String ;
    var
      {$IFDEF MANAGED}
        b   : TBytes ;
      {$ENDIF}
      opt : Integer ;
    begin
      Result := '' ;
      if _size > 0 then begin
        opt := $000F ;
        {$IFDEF MANAGED}
          SetLength( b, _size ) ;
          GisCopyMemory( vr, vr_off, b, 0, _size ) ;
          if TatukGIS.RTL.IsTextUnicode( b, _size, opt ) then
            {$IFDEF JAVA OR ISLAND}
              Result := TEncoding.UTF16LE.GetString( b, 0, _size )
            {$ELSE}
              Result := TEncoding.Unicode.GetString( b, 0, _size )
          {$ENDIF}
          else
            Result := ConvertAnsiString( b ) ;
        {$ELSE}
          {$IFDEF MSWINDOWS}
            if IsTextUnicode( ptvar, _size, @opt ) then
              SetString( Result, PWideChar(ptvar), _size div 2 )
            else
              SetString( Result, PAnsiChar(ptvar), _size ) ;
          {$ELSE}
            SetString( Result, PWideChar(ptvar), _size div 2 ) ;
          {$ENDIF}
        {$ENDIF}
        Result := Trim( Result ) ;
      end ;
    end ;

    // Geometry structure
    procedure parse_gdo_point ;
    var
       rotation : Double ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( (gdoType = $FFC0) or (gdoType = $FFC8) or (gdoType = $FFC9) ) ;

      if ( gdoType = $FFC0 ) or         // 65472 - Point Geometry
         ( gdoType = $FFC8 ) then begin // 65480 - Oriented Point Geometry
        if res.IsEmpty or multiParts then
          res.AddPart ;
        res.AddPoint3D( read_ptg3D ) ;
      end else
      if ( gdoType = $FFC9 ) then begin // 65481 - Text Point Geometry
        if res.IsEmpty or multiParts then
          res.AddPart ;

        res.AddPoint3D( read_ptg3D ) ;

        rotation := -DegToRad( read_double ) ;

        {$IFDEF MANAGED}
          skip_bytes( vr_off, 26 ) ;
        {$ELSE}
          skip_bytes( ptvar, 26 ) ;
        {$ENDIF}
        // If format type <> 1 then indicates anchor point without label
        if (read_byte in [1, 2]) then begin

          try
            if assigned( _layer )                          and
               IsStringEmpty( _layer.Params.Labels.Value ) and
               ( not _layer.IgnoreShapeParams   )
            then
            begin
              with res.Params do begin
                Labels.Rotate := rotation ;

                case read_byte of
                    0 : Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.MiddleCenter ) ;
                    1 : Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.MiddleLeft   ) ;
                    2 : Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.MiddleRight  ) ;
                    4 : Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.UpCenter     ) ;
                    5 : Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.UpLeft       ) ;
                    6 : Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.UpRight      ) ;
                    8 : Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.DownCenter   ) ;
                    9 : Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.DownLeft     ) ;
                   10 : Labels.Position := GisGetLabelPosition( TGIS_LabelPosition.DownRight    ) ;
                end ;
                Labels.Width  := -1000 ;
                Labels.Value  := read_string( read_dword ) ;
              end ;
            end
            else
              read_byte ;
          except
            // indicates anchor point without label
          end ;
        end ;

      end ;
    end ;

    procedure parse_gdo_arc ;
    var
      ptStart  : TGIS_Point3D ;
      ptEnd    : TGIS_Point3D ;
      ptNormal : TGIS_Point3D ;
      r        : Double ;
      c,c1,c2  : TGIS_Point3D ;
      q,sa,ea  : Double ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( gdoType = $FFCA ) ;

      if res.IsEmpty or multiParts then
        res.AddPart ;

        ptStart  := read_ptg3D ;
        ptEnd    := read_ptg3D ;
        ptNormal := read_ptg3D ;
        r        := Abs(read_double) ;

      q := Sqrt( Sqr( ptEnd.X - ptStart.X ) + Sqr( ptEnd.Y - ptStart.Y) ) ;
      if q = 0 then
        q := 0.001 ;

      {$IFDEF GIS_NORECORDS}
      c  := new TGIS_Point3D ;
      c1 := new TGIS_Point3D ;
      c2 := new TGIS_Point3D ;
      {$ENDIF}

      c.X := ( ptStart.X + ptEnd.X ) / 2 ;
      c.Y := ( ptStart.Y + ptEnd.Y ) / 2 ;

      c1.X := c.X + Sqrt( Abs(r*r-Sqr(q/2)) )*( ptStart.Y - ptEnd.Y   )/q ;
      c1.Y := c.Y + Sqrt( Abs(r*r-Sqr(q/2)) )*( ptEnd.X   - ptStart.X )/q ;

      c2.X := c.X - Sqrt( Abs(r*r-Sqr(q/2)) )*( ptStart.Y - ptEnd.Y   )/q ;
      c2.Y := c.Y - Sqrt( Abs(r*r-Sqr(q/2)) )*( ptEnd.X   - ptStart.X )/q ;

      if ptNormal.Z = 1 then begin
        sa := -1*ArcTan2( ptStart.Y - c1.Y, ptStart.X - c1.X ) * 180/Pi ;
        ea := -1*ArcTan2( ptEnd.Y   - c1.Y, ptEnd.X   - c1.X ) * 180/Pi ;

        if sa < 0 then
          sa := sa + 360 ;
        // CCW
        res.StrokeArc( c1, r, r, DegToRad(sa), DegToRad(ea), 0 );
      end
      else begin
        sa := -1*ArcTan2( ptStart.Y - c2.Y, ptStart.X - c2.X ) * 180/Pi ;
        ea := -1*ArcTan2( ptEnd.Y   - c2.Y, ptEnd.X   - c2.X ) * 180/Pi ;

        if ea < 0 then
          ea := ea + 360 ;

        // CW
        res.StrokeArc( c2, r, r, DegToRad(sa), DegToRad(ea), 0 ) ;
      end ;
    end ;

    procedure parse_gdo_line ;
    var
      p : Integer ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( gdoType = $FFC1 ) ;

      if res.IsEmpty or multiParts then
        res.AddPart ;

      for p:=0 to 1 do begin
        res.AddPoint3D( read_ptg3D ) ;
      end ;
    end ;

    procedure parse_gdo_polyline ;
    var
      p, points : Integer ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( gdoType = $FFC2 ) ;

      if res.IsEmpty or multiParts then
        res.AddPart ;

      points := read_dword ;

      for p := 0 to points - 1 do begin
        res.AddPoint3D( read_ptg3D ) ;
      end ;
    end ;

    procedure parse_gdo_polygon ;
    var
      p, points : Integer ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( gdoType = $FFC3 ) ;

      res.AddPart ;

      points := read_dword ;
      for p:=0 to points - 1 do begin
        res.AddPoint3D( read_ptg3D ) ;
      end ;
    end ;

    procedure parse_gdo_rectangle ;
    var
      w, h : Double ;
      ptg  : TGIS_Point3D ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( gdoType = $FFC7 ) ;

      if res.IsEmpty then
        res.AddPart ;

      ptg := read_ptg3D ;
      w   := read_double ;
      h   := read_double ;

      res.AddPoint( GisPoint( ptg.X    , ptg.Y     ) ) ;
      res.AddPoint( GisPoint( ptg.X + w, ptg.Y     ) ) ;
      res.AddPoint( GisPoint( ptg.X + w, ptg.Y + h ) ) ;
      res.AddPoint( GisPoint( ptg.X    , ptg.Y + h ) ) ;
    end ;

    //DOM-IGNORE-BEGIN
      procedure parse_gdo_object ; forward ;
    //DOM-IGNORE-END

    procedure parse_gdo_multilinestring ;
    var
      l, linestrings : Integer ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( gdoType = $FFCB ) ;

      multiParts := True ;

      linestrings := read_dword ;

      for l := 0 to linestrings - 1 do begin
        parse_gdo_object ;
      end ;

      multiParts := False ;
    end ;

    procedure parse_gdo_multipolygon ;
    var
      p, polygons : Integer ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( gdoType = $FFCC ) ;

      res.AddPart ;

      polygons := read_dword ;

      for p := 0 to polygons - 1 do
        parse_gdo_object ;
    end ;

    procedure parse_gdo_collection ;
    var
      o, objects : Integer ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( gdoType = $FFC6 ) ;

      objects := read_dword ;

      if collection and ( objects > 1 ) then
        multiParts := True ;

      for o := 0 to objects - 1 do
        parse_gdo_object ;

      multiParts := False ;
    end ;

    procedure parse_gdo_boundary ;
    var
      b : Integer ;
    begin
      le := True ;
      gdoType := read_gdotype ;
      assert( gdoType = $FFC5 ) ;

      for b := 1 to 2 do
        parse_gdo_object ;

    end ;

    procedure parse_gdo_object ;
    var
      size : Integer ;
      {$IFDEF MANAGED}
        vr_off_obj_tmp : Integer ;
      {$ELSE}
        ptvar_obj_tmp  : Pointer ;
      {$ENDIF}
    begin
       size := read_dword ;

       {$IFDEF MANAGED}
         vr_off_obj_tmp := vr_off ;
       {$ELSE}
         ptvar_obj_tmp := ptvar ;
       {$ENDIF}

       gdoType := read_gdotype ;

       {$IFDEF MANAGED}
         vr_off := vr_off_obj_tmp ;
       {$ELSE}
         ptvar := ptvar_obj_tmp ;
       {$ENDIF}
       if collection then begin
         case gdoType of
           $FFC0,  // Point Geometry
           $FFC8,  // Oriented Point Geometry
           $FFC9 : // Text Point Geometry
             begin
               res := TGIS_ShapeMultiPoint.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                         ) ;
               res.Lock( TGIS_Lock.Internal ) ;
               collection := False ;
             end ;
           $FFC1,  // Line Geometry
           $FFC2,  // Polyline Geometry
           $FFCA,  // Arc
           $FFCB : // Composite Polyline Geometry
             begin
               res := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                         ) ;
               res.Lock( TGIS_Lock.Internal ) ;
               collection := False ;
             end ;
           $FFC3,  // Polygon Geometry
           $FFC5,  // Boundary Geometry
           $FFC7,  // Rectangle Geometry
           $FFCC : // Composite Polygon Geometry
             begin
               res := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                         ) ;
               res.Lock( TGIS_Lock.Internal ) ;
               collection := False ;
             end ;
         end ;
       end ;

       case gdoType of
         $FFC0,  // Point Geometry
         $FFC8,  // Oriented Point Geometry
         $FFC9 : // Text Point Geometry
            parse_gdo_point ;
         $FFC1 :  // Line Geometry
            parse_gdo_line ;
         $FFC2 : // Polyline Geometry
            parse_gdo_polyline ;
         $FFC3 : // Polygon Geometry
            parse_gdo_polygon ;
         $FFC5 : // Boundary Geometry
            parse_gdo_boundary ;
         $FFC6 : // Geometry Collection (Heterogeneous)
            parse_gdo_collection ;
         $FFC7 : // Rectangle Geometry
            parse_gdo_rectangle ;
         $FFCA : // Arc
            parse_gdo_arc ;
         $FFCB : // Composite Polyline Geometry
            parse_gdo_multilinestring ;
         $FFCC : // Composite Polygon Geometry
            parse_gdo_multipolygon ;
       end ;
       {$IFDEF MANAGED}
         vr_off := vr_off_obj_tmp ;
         skip_bytes( vr_off, size ) ;
       {$ELSE}
         ptvar := ptvar_obj_tmp ;
         skip_bytes( ptvar, size ) ;
       {$ENDIF}
    end ;

  begin
    res    := nil ;

    {$IFDEF MANAGED}
      {$IFNDEF OXYGENE}
        assert( VarArrayElementsIsType(_gdo, varByte) ) ;
      {$ENDIF}
      vr := TBytes( TObject( _gdo ) ) ;
      vr_off := 0  ;
      vr_off_tmp := vr_off ;
    {$ELSE}
      ptvar := VarArrayLock( _gdo ) ;
      ptvar_tmp := ptvar ;
    {$ENDIF}

    try
      le := True ;
      multiParts := False ;
      collection := False ;
      gdoType := 0 ;
      gdoType := read_gdotype ;

      {$IFDEF MANAGED}
        vr_off := vr_off_tmp ;
      {$ELSE}
        ptvar := ptvar_tmp ;
      {$ENDIF}

      case gdoType of
        $FFC0 :    // 65472 "0FD2FFC0-8CBC-11CF-DEAB-08003601B769"
             begin
               // Point Geometry (One-to-One conversion)
               res := TGIS_ShapePoint.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_point ;
               res.Unlock ;
             end ;
        $FFC8 :    // 65480 "0FD2FFC8-8CBC-11CF-DEAB-08003601B769"
             begin
               // Oriented Point Geometry
               res := TGIS_ShapePoint.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_point ;
               res.Unlock ;
             end ;
        $FFC9 :    // 65481 "0FD2FFC9-8CBC-11CF-DEAB-08003601B769"
             begin
               // Text Point Geometry (One-to-One conversion)
               res := TGIS_ShapePoint.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_point ;
               res.Unlock ;
             end ;
        $FFC1 :    // 65473 "0FD2FFC1-8CBC-11CF-DEAB-08003601B769"
             begin
               // Line Geometry (One-to-One conversion)
               res := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_line ;
               res.Unlock ;
             end ;
        $FFC2 :    // 65474 "0FD2FFC2-8CBC-11CF-DEAB-08003601B769"
             begin
               // Polyline Geometry (One-to-One conversion)
               res := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_polyline ;
               res.Unlock ;
             end ;
        $FFC3 :    // 65475 "0FD2FFC3-8CBC-11CF-DEAB-08003601B769"
             begin //
               // Polygon Geometry (One-to-One conversion)
               res := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_polygon ;
               res.Unlock ;
             end ;
        $FFC5 :    // 65477 "0FD2FFC5-8CBC-11CF-DEAB-08003601B769"
             begin
               // Boundary Geometry
               res := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_boundary ;
               res.Unlock ;
             end ;
        $FFC6 :    // 65478 "0FD2FFC6-8CBC-11CF-DEAB-08003601B769"
             begin
               // Geometry Collection (Heterogeneous)
               collection := True ;
               parse_gdo_collection ;
               if res <> nil then
                 res.Unlock ;
             end ;
        $FFC7 :    // 65479 "0FD2FFC7-8CBC-11CF-DEAB-08003601B769"
             begin
               // Rectangle Geometry (Interpret rectangles as simple polygons)
               res := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_rectangle ;
               res.Unlock ;
             end ;
        $FFCA :    // 65482 "0FD2FFCA-8CBC-11CF-DEAB-08003601B769"
              begin
                // Arc Geometry (One-to-One conversion)
                  res := TGIS_ShapeArc.Create(
                              _source, _ptr, _mapped, _uid, _layer,
                             TGIS_DimensionType.XYZ
                            ) ;
                  res.Lock( TGIS_Lock.Extent ) ;
                    parse_gdo_arc ;
                  res.Unlock ;
             end ;
        $FFCB :    // 65483 "0FD2FFCB-8CBC-11CF-DEAB-08003601B769"
             begin
               // Composite Polyline Geometry (One-to-One conversion)
               res := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_multilinestring ;
               res.Unlock ;
             end ;
        $FFCC :    // 65484 "0FD2FFCC-8CBC-11CF-DEAB-08003601B769"
             begin
               // Composite Polygon Geometry (One-to-One conversion)
               res := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZ
                          ) ;
               res.Lock( TGIS_Lock.Internal ) ;
                 parse_gdo_multipolygon ;
               res.Unlock ;
             end ;
        else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDGDO ), '',
                                          gdoType
                                        );

      end ;
      if (res <> nil ) and res.IsEmpty then
        FreeObject( res ) ;
    finally
      Result := res ;
      {$IFNDEF MANAGED}
        VarArrayUnLock( _gdo ) ;
      {$ENDIF}
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromWKT(
    const _wkt : String
  ) : TGIS_Shape ;
  var
    shp : TGIS_ShapePoint ;
  begin
    shp := TGIS_ShapePoint.Create( nil, nil, False, 0, nil );
    try
      try
        Result := shp.CreateFromWKT( _wkt ) ;
      except
        Result := nil ;
      end ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromEWKT(
    const _ewkt : String
  ) : TGIS_Shape ;
  var
    shp : TGIS_ShapePoint ;
  begin
    shp := TGIS_ShapePoint.Create( nil, nil, False, 0, nil );
    try
      try
        Result := shp.CreateFromEWKT( _ewkt ) ;
      except
        Result := nil ;
      end ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromEWKT(
    const _ewkt   : String     ;
    const _source : TGIS_Shape ;
    {$IFDEF MANAGED}
      const _ptr  : TGIS_Bytes ;
    {$ELSE}
      const _ptr  : Pointer    ;
    {$ENDIF}
    const _mapped : Boolean    ;
    const _uid    : TGIS_Uid    ;
    const _layer  : TGIS_LayerVector
  ) : TGIS_Shape ;
  var
    ipos   : Integer ;
    ilen   : Integer ;
    sname  : String  ;
    ewkt   : Boolean ;
    bempty : Boolean ;
    part   : TGIS_Shape ;

    procedure skip_spaces ;
    begin
      while ipos < ilen do begin
        case _ewkt[ ipos ] of
          ' ',
          #9 ,
          #10,
          #13,
          ';'  : ;
          else break ;
        end ;
        inc( ipos ) ;
      end ;
    end ;

    function collect_subname : String ;
    var
      c : Char ;
    begin
      skip_spaces ;
      Result := '' ;
      while ipos <= ilen do begin
        c := _ewkt[ ipos ] ;
        case c of
          '(',
          ' ',
          #9 ,
          #10,
          #13,
          '=',
          ','  : Break ;
          else   Result := Result + UpCase( c ) ;
        end ;
        inc( ipos ) ;
      end ;
      skip_spaces ;
    end ;

    procedure collect_name ;
    var
      c   : Char   ;
      sn  : String ;
    begin
      bempty := False ;
      sname := collect_subname ;
      if ipos > StringLast( _ewkt ) then
        Abort ;
      c := _ewkt[ ipos ] ;
      if      c =  '=' then
        inc( ipos )
      else if c <> '(' then begin
        sn := collect_subname ;
        if sn = 'EMPTY' then
          bempty := True
        else if (sn = 'Z') or (sn = 'M') or (sn = 'ZM') then begin
          sname := sname + sn ;
          // check again if be empty
          sn := collect_subname ;
          if sn = 'EMPTY' then
            bempty := True
        end ;
      end ;
    end ;

    procedure test_brace_open ;
    begin
      if _ewkt[ ipos ] <> '(' then Abort ;
      inc( ipos ) ;
      skip_spaces ;
    end ;

    function parse_element
      : Double ;
    var
      tmp : String ;
      c   : Char   ;
    begin
      tmp := '' ;
      while ipos < ilen do begin
        c := _ewkt[ ipos ] ;
        case c of
          ')',
          ' ',
          #9 ,
          #13,
          #10,
          ',',
          ';'  : break ;
          else   tmp := tmp + c ;
        end ;
        inc( ipos ) ;
      end ;
      Result := DotStrToFloat( tmp ) ;
      skip_spaces ;
    end ;

    procedure parse_srid( var _srid : Integer ) ;
    begin
      collect_name ;
      if sname = 'SRID' then begin
        ewkt := True ;
        skip_spaces ;
        _srid := TruncS( parse_element ) ;
        skip_spaces ;

        collect_name ;
      end ;
    end ;

    function parse_points: TGIS_Point3DList ;
    var
     x, y, z, m : Double ;
     chs : TCharSet ;
    begin
      Result := TGIS_Point3DList.Create ;

      test_brace_open ;
      chs := PrepareCharSet( [',',')'] ) ;
      while ipos < ilen do begin
        x := parse_element ;
        y := parse_element ;
        z := 0 ;
        m := 0 ;
        if not InCharSet( _ewkt[ipos], chs ) then
          z := parse_element ;
        if not InCharSet( _ewkt[ipos], chs ) then
          m := parse_element ;

        Result.Add( GisPoint3D( x, y, z, m ) ) ;

        if _ewkt[ipos] = ')' then begin
          inc( ipos ) ;
          skip_spaces ;
          break ;
        end
        else begin
          inc( ipos ) ;
          skip_spaces ;
        end ;
      end ;
    end ;

    function circularSegmentizeEx(
      const _p1      : TGIS_Point3D ;
      const _p2      : TGIS_Point3D ;
      const _p3      : TGIS_Point3D ;
      const _center  : TGIS_Point3D ;
      const _radius  : Double ;
      const _perQuad : Integer
     ) : TGIS_Point3DList ;
     const
      M_PI   = Pi ;
      M_PI_2 = Pi/2 ;
    var
       a1, a2,
       a3, sweep,
       increment,
       angle       : Double ;
       ptcount, i  : Integer ;
       rsin, rcos  : Double ;
    begin
      Result := TGIS_Point3DList.Create ;

      if _radius < 0 then begin
        Result.Add( _p1 ) ;
        Result.Add( _p2 ) ;
        exit ;
      end ;

      a1 := ArcTan2( _p1.Y - _center.Y, _p1.X - _center.X ) ;
      a2 := ArcTan2( _p2.Y - _center.Y, _p2.X - _center.X ) ;
      a3 := ArcTan2( _p3.Y - _center.Y, _p3.X - _center.X ) ;

      if ( Abs( _p1.X - _p3.X ) < 1.0e-8 ) and ( Abs( _p1.Y - _p3.Y ) < 1.0e-8 ) then
        sweep := 2*Pi
      else if(a1 > a2) and (a2 > a3) then
        sweep := a3 - a1
      else if (a1 < a2) and (a2 < a3) then
        sweep := a3 - a1
      else if ((a1 < a2) and (a1 > a3)) or ((a2 < a3) and (a1 > a3)) then
        sweep := a3 - a1 + 2*Pi
      else if ((a1 > a2) and (a1 < a3)) or ((a2 > a3) and (a1 < a3)) then
        sweep := a3 - a1 - 2*Pi
      else
        sweep := 0.0;

      ptcount := FloorS( Abs(sweep * _perQuad / M_PI_2 ) ) - 1 ;

      increment := M_PI_2 / Max( 4, _perQuad ) ;
      if (sweep < 0) then
        increment := increment * -1.0 ;
      angle := a1 ;

      Result.Add( _p1 )  ;
      for i := 0 to ptcount - 1 do begin
        angle := angle + increment ;
        if (increment > 0.0) and ( angle > Pi) then
          angle := angle - 2*Pi;
        if (increment < 0.0 ) and ( angle < -1*Pi) then
          angle := angle - 2*Pi;

        SinCos( angle, rsin, rcos ) ;
        Result.Add( GisPoint3D( _center.X + _radius * rcos,
                                _center.Y + _radius * rsin,
                                0
                              )
                   ) ;
      end ;
      Result.Add( _p3 )  ;
    end ;

    function circularSegmentize(
      const _p1 : TGIS_Point3D ;
      const _p2 : TGIS_Point3D ;
      const _p3 : TGIS_Point3D
     ) : TGIS_Point3DList ;
    var
      center : TGIS_Point3D ;
      radius : Double ;
      start  : Double ;
      stop   : Double ;
    begin
      if GisArcFrom3Points3D( _p1, _p2, _p3, center, radius, start, stop ) then
        Result := circularSegmentizeEx( _p1, _p2, _p3, center, radius, 32 )
      else begin
        Result := TGIS_Point3DList.Create ;
        Result.Add(_p1) ;
        Result.Add(_p3) ;
      end ;
    end ;

    function parse_circular_string : TGIS_Point3DList ;
    var
      list     : TGIS_Point3DList ;
      segments : TGIS_Point3DList ;
      i        : Integer ;
      j        : Integer ;
    begin
      Result := TGIS_Point3DList.Create ;

      list := parse_points ;
      try
        i := 0 ;
        while i < list.Count-2 do begin
          segments := circularSegmentize(list[i], list[i+1], list[i+2]) ;
          try
            for j := 0 to segments.Count-1 do
              Result.Add( segments[ j ] ) ;
          finally
            FreeObject( segments ) ;
          end ;
          inc( i, 2 ) ;
        end ;
      finally
        FreeObject( list ) ;
      end ;
    end ;

    procedure parse_curve(
      const _shp : TGIS_Shape
    ) ;
    var
      list : TGIS_Point3DList ;
      i    : Integer ;
    begin
      if bempty then exit ;
      list := parse_circular_string ;
      try
        for i := 0 to list.Count-1 do
          _shp.AddPoint3D( list[ i ] ) ;
      finally
        FreeObject( list ) ;
      end ;
    end ;

    function parse_compound_curve : TGIS_ObjectList ;
    var
      list : TGIS_Point3DList ;
    begin
      Result := TGIS_ObjectList.Create ;

      test_brace_open ;
      while ipos < ilen do begin
        if _ewkt[ipos] = '(' then begin
          list := parse_points ;
          Result.Add( list ) ;
        end
        else begin
          collect_name ;
          if sname = 'CIRCULARSTRING' then begin
            list := parse_circular_string ;
            Result.Add( list ) ;
          end ;
        end ;

        if _ewkt[ipos] = ')' then begin
          inc( ipos ) ;
          skip_spaces ;
          break ;
        end
        else begin
          inc( ipos ) ;
          skip_spaces ;
        end ;
      end ;
    end ;

    procedure parse_multicurve(
      const _shp : TGIS_Shape
    ) ;
    var
      list : TGIS_ObjectList ;
      i, j : Integer ;
    begin
      if bempty then exit ;
      list := parse_compound_curve ;
      try
        for i := 0 to list.Count-1 do begin
          for j := 0 to TGIS_Point3DList(list[i]).Count-1 do
            _shp.AddPoint3D( TGIS_Point3DList(list[i])[ j ] ) ;
        end ;
      finally
        FreeObject( list ) ;
      end ;
    end ;

    procedure parse_compound_curves(
      const _shp : TGIS_Shape
    ) ;
    var
      list   : TGIS_Point3DList ;
      listEx : TGIS_ObjectList ;
      i, j   : Integer ;
    begin
      if bempty then exit ;
      test_brace_open ;
      while ipos < ilen do begin
        if _ewkt[ipos] = '(' then begin
          list := parse_points ;
          try
            _shp.AddPart ;

            for i := 0 to list.Count-1 do
              _shp.AddPoint3D( list[i] ) ;
          finally
            FreeObject( list ) ;
          end ;
        end
        else begin
          collect_name ;
          if sname = 'CIRCULARSTRING' then begin
            list := parse_circular_string ;
            try
              _shp.AddPart ;

              for i := 0 to list.Count-1 do
                _shp.AddPoint3D( list[i] ) ;
            finally
              FreeObject( list ) ;
            end ;
          end
          else if sname = 'COMPOUNDCURVE' then begin
            listEx := parse_compound_curve ;
            try
              _shp.AddPart ;

              for i := 0 to listEx.Count-1 do begin
                for j := 0 to TGIS_Point3DList(listEx[i]).Count-1 do
                  _shp.AddPoint3D( TGIS_Point3DList(listEx[i])[ j ] ) ;
              end ;
            finally
              FreeObject( listEx ) ;
            end ;
          end
          else if sname = 'CURVEPOLYGON' then begin
            parse_compound_curves( _shp ) ;
            inc( ipos ) ;
          end ;
        end ;

        if _ewkt[ipos] = ')' then begin
          inc( ipos ) ;
          skip_spaces ;
          break ;
        end
        else begin
          inc( ipos ) ;
          skip_spaces ;
        end ;
      end ;
    end ;

    procedure parse_point(
      var _shp : TGIS_Shape
    ) ;
    var
      x, y, z, m : Double ;
      dim : TGIS_DimensionType ;
    begin
      x := parse_element ;
      y := parse_element ;

      case _shp.Dimension of
        TGIS_DimensionType.Unknown :
          begin
            z := 0 ;
            m := 0 ;
            dim := TGIS_DimensionType.XY ;
            if (_ewkt[ipos] <> ')') and (_ewkt[ipos] <> ',') then begin
              z := parse_element ;
              dim := TGIS_DimensionType.XYZ ;

              if (_ewkt[ipos] <> ')') and (_ewkt[ipos] <> ',') then begin
                m := parse_element ;
                dim := TGIS_DimensionType.XYZM ;
              end ;
            end ;

            _shp.Recreate( nil, nil, False, -1, nil, dim ) ;
            if _shp.GetNumParts <= 0 then _shp.AddPart ;

            case dim of
              TGIS_DimensionType.XY :
                _shp.AddPoint   ( GisPoint ( x, y       ) ) ;
              TGIS_DimensionType.XYZ :
                _shp.AddPoint3D( GisPoint3D( x, y, z    ) ) ;
              TGIS_DimensionType.XYZM :
                _shp.AddPoint3D( GisPoint3D( x, y, z, m ) ) ;
            end ;
          end ;
        TGIS_DimensionType.XY :
          begin
            _shp.AddPoint  ( GisPoint  ( x, y       ) ) ;
          end ;
        TGIS_DimensionType.XYZ :
          begin
            z := parse_element ;
            _shp.AddPoint3D( GisPoint3D( x, y, z    ) ) ;
          end ;
        TGIS_DimensionType.XYM :
          begin
            m := parse_element ;
            _shp.AddPoint3D( GisPoint3D( x, y, 0, m ) ) ;
          end ;
        TGIS_DimensionType.XYZM :
          begin
            z := parse_element ;
            m := parse_element ;
            _shp.AddPoint3D( GisPoint3D( x, y, z, m ) ) ;
          end ;
      end ;
    end ;

    procedure parse_part(
      var _shp : TGIS_Shape
    ) ;
    var
      new_part : Boolean ;
    begin
      if bempty then exit ;

      new_part := True ;
      test_brace_open ;
      while ipos < ilen do begin
        if new_part then begin
          _shp.AddPart ;
          new_part := False ;
        end ;
        parse_point( _shp ) ;
        if _ewkt[ipos] = ')' then begin
          inc( ipos ) ;
          skip_spaces ;
          break ;
        end
        else begin
          inc( ipos ) ;
          skip_spaces ;
        end ;
      end ;
    end ;

    procedure parse_multipart(
      var _shp : TGIS_Shape
    ) ;
    begin
      if bempty then exit ;

      test_brace_open ;
      while ipos < ilen do begin
        parse_part( _shp ) ;
        if _ewkt[ipos] = ')' then begin
          inc( ipos ) ;
          skip_spaces ;
          break ;
        end
        else begin
          inc( ipos ) ;
          skip_spaces ;
        end ;
      end ;
    end ;

    procedure parse_multipoint(
      var _shp : TGIS_Shape
    ) ;
    var
      new_part : Boolean ;
      dbl_brace : Boolean ;
    begin
      if bempty then exit ;

      new_part  := True ;
      dbl_brace := False ;

      test_brace_open ;
      while ipos < ilen do begin
        if new_part then begin
          _shp.AddPart ;
          new_part := False ;
        end ;

        if _ewkt[ipos] = '(' then begin
          test_brace_open ;
          dbl_brace := True ;
        end ;

        parse_point( _shp ) ;

        if _ewkt[ipos] = ')' then begin
          inc( ipos ) ;
          skip_spaces ;
          if dbl_brace then begin
            inc( ipos ) ;
            skip_spaces ;
          end
          else
            break ;
        end
        else begin
          inc( ipos ) ;
          skip_spaces ;
        end ;
      end ;
    end ;

    procedure parse_multisegment(
      var _shp : TGIS_Shape
    ) ;
    begin
      if bempty then exit ;

      test_brace_open ;
      while ipos < ilen do begin
        parse_multipart( _shp ) ;
        if _ewkt[ipos] = ')' then begin
          inc( ipos ) ;
          skip_spaces ;
          break ;
        end
        else begin
          inc( ipos ) ;
          skip_spaces ;
        end ;
      end ;
    end ;

    function parse_shape
      : TGIS_Shape;
    var
      srid : Integer ;
    begin
      parse_srid( srid ) ;
      if      ( sname = 'POINT'                 ) then begin
                Result := TGIS_ShapePoint.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_part( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'POINTZ'                ) then begin
                Result := TGIS_ShapePoint.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZ
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_part( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'POINTM'                ) then begin
                Result := TGIS_ShapePoint.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_part( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'POINTZM'               ) then begin
                Result := TGIS_ShapePoint.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_part( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTIPOINT'            ) then begin
                Result := TGIS_ShapeMultiPoint.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipoint( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTIPOINTZ'           ) then begin
                Result := TGIS_ShapeMultiPoint.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZ
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipoint( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTIPOINTM'           ) then begin
                Result := TGIS_ShapeMultiPoint.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipoint( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTIPOINTZM'          ) then begin
                Result := TGIS_ShapeMultiPoint.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipoint( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'LINEARRING'           ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_part( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'LINESTRING'            ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_part( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'LINESTRINGZ'           ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZ
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_part( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'LINESTRINGM'           ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_part( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'LINESTRINGZM'          ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_part( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTILINESTRING'       ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipart( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTILINESTRINGZ'      ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZ
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipart( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTILINESTRINGM'      ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipart( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTILINESTRINGZM'     ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipart( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'POLYGON'               ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipart( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'POLYGONZ'              ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZ
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipart( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'POLYGONM'              ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipart( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'POLYGONZM'             ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multipart( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTIPOLYGON'          ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multisegment( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTIPOLYGONZ'         ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZ
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multisegment( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTIPOLYGONM'         ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multisegment( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTIPOLYGONZM'        ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_multisegment( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'GEOMETRYCOLLECTION'    ) then begin
                Result := TGIS_ShapeComplex.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  if not bempty then begin
                    test_brace_open ;
                    while ipos < ilen do begin
                      part := parse_shape ;
                      if TGIS_ShapeComplex( Result ).ShapesCount = 0 then
                        Result.Recreate( nil, nil, False, -1, nil, part.Dimension ) ;
                      if part.Dimension <> Result.Dimension then
                        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTED_DIM ), '', 0 ) ;

                      TGIS_ShapeComplex( Result ).AddShape( part ) ;
                      if _ewkt[ipos] = ')' then begin
                        inc( ipos ) ;
                        skip_spaces ;
                        break ;
                      end
                      else begin
                        inc( ipos ) ;
                        skip_spaces ;
                      end ;
                    end ;
                  end ;

                Result.Unlock ;
              end
      else if ( sname = 'GEOMETRYCOLLECTIONZ'   ) then begin
                Result := TGIS_ShapeComplex.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZ
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  if not bempty then begin
                    test_brace_open ;
                    while ipos < ilen do begin
                      part := parse_shape ;
                      if part.Dimension <> Result.Dimension then
                        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTED_DIM ), '', 0 ) ;
                      TGIS_ShapeComplex( Result ).AddShape( part ) ;
                      if _ewkt[ipos] = ')' then begin
                        inc( ipos ) ;
                        skip_spaces ;
                        break ;
                      end
                      else begin
                        inc( ipos ) ;
                        skip_spaces ;
                      end ;
                    end ;
                  end ;

                Result.Unlock ;
              end
      else if ( sname = 'GEOMETRYCOLLECTIONM'   ) then begin
                Result := TGIS_ShapeComplex.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  if not bempty then begin
                    test_brace_open ;
                    while ipos < ilen do begin
                      part := parse_shape ;
                      if part.Dimension <> Result.Dimension then
                        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTED_DIM ), '', 0 ) ;
                      TGIS_ShapeComplex( Result ).AddShape( part ) ;
                      if _ewkt[ipos] = ')' then begin
                        inc( ipos ) ;
                        skip_spaces ;
                        break ;
                      end
                      else begin
                        inc( ipos ) ;
                        skip_spaces ;
                      end ;
                    end ;
                  end ;

                Result.Unlock ;
              end
      else if ( sname = 'GEOMETRYCOLLECTIONZM'  ) then begin
                Result := TGIS_ShapeComplex.Create(
                            _source, _ptr, _mapped, _uid, _layer,
                            TGIS_DimensionType.XYZM
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  if not bempty then begin
                    test_brace_open ;
                    while ipos < ilen do begin
                      part := parse_shape ;
                      if part.Dimension <> Result.Dimension then
                        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTED_DIM ), '', 0 ) ;
                      TGIS_ShapeComplex( Result ).AddShape( part ) ;
                      if _ewkt[ipos] = ')' then begin
                        inc( ipos ) ;
                        skip_spaces ;
                        break ;
                      end
                      else begin
                        inc( ipos ) ;
                        skip_spaces ;
                      end ;
                    end ;
                  end ;

                Result.Unlock ;
              end
      else if ( sname = 'CIRCULARSTRING'        ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  Result.AddPart ;
                  parse_curve( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'COMPOUNDCURVE'         ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  Result.AddPart ;
                  parse_multicurve( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'CURVEPOLYGON'          ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_compound_curves( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTICURVE'            ) then begin
                Result := TGIS_ShapeArc.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_compound_curves( Result ) ;
                Result.Unlock ;
              end
      else if ( sname = 'MULTISURFACE'          ) then begin
                Result := TGIS_ShapePolygon.Create(
                            _source, _ptr, _mapped, _uid, _layer
                          ) ;
                Result.Lock( TGIS_Lock.Internal ) ;
                  parse_compound_curves( Result ) ;
                Result.Unlock ;
              end
      else begin
        if ewkt then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDEWKT ), sname, 0 )
        else
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDWKT  ), sname, 0 ) ;
      end ;
    end ;

  begin
    ewkt := False ;
    ilen := StringLast( _ewkt ) ;
    ipos := StringFirst ;

    try
      if ilen = 0 then
        Result := nil
      else
        Result := parse_shape ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTED_PARSING ), '', ipos ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromWKT(
    const _wkt    : String     ;
    const _source : TGIS_Shape ;
    {$IFDEF MANAGED}
      const _ptr  : TGIS_Bytes ;
    {$ELSE}
      const _ptr  : Pointer    ;
    {$ENDIF}
    const _mapped : Boolean    ;
    const _uid    : TGIS_Uid    ;
    const _layer  : TGIS_LayerVector
  ) : TGIS_Shape ;
  begin
    Result := GisCreateShapeFromEWKT( _wkt, _source, _ptr, _mapped, _uid, _layer ) ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromWKB(
    const _wkb : OleVariant
  ) : TGIS_Shape ;
  var
    shp : TGIS_ShapePoint ;
  begin
    shp := TGIS_ShapePoint.Create( nil, nil, False, 0, nil ) ;
    try
      try
        if VarIsNull( _wkb ) then
          Result := nil
        else
          Result := shp.CreateFromWKB( _wkb ) ;
      except
        Result := nil ;
      end ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromEWKB(
    const _ewkb : OleVariant
  ) : TGIS_Shape ;
  var
    shp : TGIS_ShapePoint ;
  begin
    shp := TGIS_ShapePoint.Create( nil, nil, False, 0, nil ) ;
    try
      try
        if VarIsNull( _ewkb ) then
          Result := nil
        else
          Result := shp.CreateFromEWKB( _ewkb ) ;
      except
        Result := nil ;
      end ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  // Common function to create the shape from Well Known Binary (WKB)
  //  or an Extended Well Known Binary (EWKB) representation of the geometry with
  //  SRID metadata..
  //  note For internal use only
  // _wkb    WKB or EWKB array
  // _source If not nil, then the base shape will be based on this shape.
  //               Otherwise _ptr, _uid and _layer will be used.
  // _ptr    Address in memory where shape data exists.
  // _mapped True if pointer is mapped to the file
  // _uid    Unique identifier for shape.
  // _layer  Reference to the layer on which the shape will be created.
  // return                newly constructed object
  // raises EGIS_Exception GIS_RS_ERR_UNSUPPORTED_PARSING
  // raises EGIS_Exception GIS_RS_ERR_UNSUPPORTEDWKB
  // raises EGIS_Exception GIS_RS_ERR_UNSUPPORTEWKB
  class function TGIS_GeometryFactory.buildShapeFromEWKB(
    {$IFDEF MANAGED}
      const _wkb  : TBytes ;
    {$ELSE}
      const _wkb  : Pointer    ;
    {$ENDIF}
    const _source : TGIS_Shape ;
    {$IFDEF MANAGED}
      const _ptr  : TGIS_Bytes ;
    {$ELSE}
      const _ptr  : Pointer    ;
    {$ENDIF}
    const _mapped : Boolean    ;
    const _uid    : TGIS_Uid    ;
    const _layer  : TGIS_LayerVector
  ) : TGIS_Shape ;
  var
    {$IFDEF MANAGED}
      vr_off      : Integer ;
    {$ELSE}
      ptvar       : Pointer ;
      ptvar_tmp   : Pointer ;
    {$ENDIF}
    wkbtype       : DWORD   ;
    ewkb          : Boolean ;
    le            : Boolean ;
    srid          : Integer ;
    wkbdim        : Integer ;
    hasz, hasm    : Boolean ;
    arpart        : array of TGIS_Point   ;
    arpart3D      : array of TGIS_Point3D ;

    function read_byte
      : Byte ;
    {$IFDEF MANAGED}
      var
        {$IFDEF OXYGENE}
          b : array of Byte := new Byte[ sizeOf( Byte ) ] ;
        {$ELSE}
          b : array[0..0] of Byte ;
        {$ENDIF}
    {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_wkb( _wkb, vr_off, b, b.Length ) ;
        Result := b[0] ;
      {$ELSE}
        read_wkb( ptvar, Result, sizeOf( Result ) ) ;
      {$ENDIF}
    end ;

    function read_dword_LE
      : DWORD ;
    {$IFDEF MANAGED}
      var
        {$IFDEF OXYGENE}
          a : array of Byte := new Byte[ sizeOf( DWORD ) ] ;
        {$ELSE}
          a : array[0..3] of Byte ;
        {$ENDIF}
    {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_wkb( _wkb, vr_off, a, a.Length ) ;
        {$IFDEF JAVA}
          Result := Integer((Integer(a[3]) shl 24) or
                            (Integer($ff and a[2]) shl 16) or
                            (Integer($ff and a[1]) shl 8) or
                            (Integer($ff and a[0]) ))
        {$ELSE}
          Result := Integer(  a[0]         or
                             (a[1] shl 8 ) or
                             (a[2] shl 16) or
                             (a[3] shl 24)
                           ) ;
        {$ENDIF}
      {$ELSE}
        read_wkb( ptvar, Result, sizeOf( Result ) ) ;
      {$ENDIF}
    end ;

    function read_dword_BE
      : DWORD ;
    var
      {$IFDEF MANAGED}
        {$IFDEF OXYGENE}
          a : array of Byte := new Byte[ sizeOf( DWORD ) ] ;
        {$ELSE}
          a : array[0..3] of Byte ;
        {$ENDIF}
      {$ELSE}
        p : Pointer ;
        b : Byte ;
      {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_wkb( _wkb, vr_off, a, a.Length ) ;
        {$IFDEF JAVA}
          Result := Integer((Integer(a[0]) shl 24) or
                            (Integer($ff and a[1]) shl 16) or
                            (Integer($ff and a[2]) shl 8) or
                            (Integer($ff and a[3]) ))
        {$ELSE}
          Result := Integer(  a[3]          or
                             (a[2] shl 8  ) or
                             (a[1] shl 16 ) or
                             (a[0] shl 24)
                           );
        {$ENDIF}
      {$ELSE}
        read_wkb( ptvar, Result, sizeOf( Result ) ) ;

        p := @Result ;

        b := Byte(Pointer(NativeInt(p) + 0)^) ;
        Byte(Pointer(NativeInt(p) + 0)^) := Byte(Pointer(NativeInt(p) + 3)^) ;
        Byte(Pointer(NativeInt(p) + 3)^) := b ;

        b := Byte(Pointer(NativeInt(p) + 1)^) ;
        Byte(Pointer(NativeInt(p) + 1)^) := Byte(Pointer(NativeInt(p) + 2)^) ;
        Byte(Pointer(NativeInt(p) + 2)^) := b ;
      {$ENDIF}
    end ;

    function read_dword
      : DWORD ;
    begin
     if le then Result := read_dword_LE
           else Result := read_dword_BE ;
    end ;

    function read_double_LE
      : Double ;
    {$IFDEF MANAGED}
      var
        {$IFDEF OXYGENE}
          a : array of Byte := new Byte[ sizeOf( Double ) ] ;
        {$ELSE}
          a : array[0..7] of Byte ;
        {$ENDIF}
    {$ENDIF}
    begin
      {$IFDEF MANAGED}
        read_wkb( _wkb, vr_off, a, a.Length ) ;
        Result := BitConverter.ToDouble( a, 0 ) ;
      {$ELSE}
        read_wkb( ptvar, Result, sizeOf( Result ) ) ;
      {$ENDIF}
    end ;

    function read_double_BE
      : Double ;
    var
      {$IFDEF OXYGENE}
        a : array of Byte := new Byte[ sizeOf( Double ) ] ;
      {$ELSE}
        a : array[0..7] of Byte ;
      {$ENDIF}
      i   : Integer ;
      {$IFDEF MANAGED}
        b : Byte ;
      {$ENDIF}
    begin
      {$IFDEF MANAGED}
        assert( a.Length = 8 ) ;
        assert( sizeOf(Double) = 8 ) ;

        read_wkb( _wkb, vr_off, a, 8 ) ;

        for i := 0 to 3 do // swap bytes
        begin
          b      := a[i  ] ;
          a[i  ] := a[7-i] ;
          a[7-i] := b      ;
        end ;
        Result := BitConverter.ToDouble( a, 0 ) ;
      {$ELSE}
        assert( sizeOf(a     ) = 8 ) ;
        assert( sizeOf(Double) = 8 ) ;

        read_wkb( ptvar, a, 8 ) ;

        for i:=0 to 7 do // swap bytes
          Byte(Pointer( NativeInt( @Result ) + i)^) := a[7-i] ;
      {$ENDIF}

    end ;

    function read_ptg_LE
      : TGIS_Point ;
    begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Point ;
      {$ENDIF}
      Result.X := read_double_LE ;
      Result.Y := read_double_LE ;
    end ;

    function read_ptg_BE
      : TGIS_Point ;
    begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Point ;
      {$ENDIF}
      Result.X := read_double_BE ;
      Result.Y := read_double_BE ;
    end ;

    function read_ptg
      : TGIS_Point ;
    begin
     if le then Result := read_ptg_LE
           else Result := read_ptg_BE ;
    end ;

    function read_ptg3D_LE
      : TGIS_Point3D ;
    begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Point3D ;
      {$ENDIF}
      Result.X := read_double_LE ;
      Result.Y := read_double_LE ;
      case wkbdim of
        1 : Result.Z := read_double_LE ;
        2 : Result.M := read_double_LE ;
        3 : begin
              Result.Z := read_double_LE ;
              Result.M := read_double_LE ;
            end ;
      end ;
    end ;

    function read_ptg3D_BE
      : TGIS_Point3D ;
    begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Point3D ;
      {$ENDIF}
      Result.X := read_double_BE ;
      Result.Y := read_double_BE ;
      case wkbdim of
        1 : Result.Z := read_double_BE ;
        2 : Result.M := read_double_BE ;
        3 : begin
              Result.Z := read_double_BE ;
              Result.M := read_double_BE ;
            end ;
      end ;
    end ;

    function read_ptg3D
      : TGIS_Point3D ;
    begin
     if le then Result := read_ptg3D_LE
           else Result := read_ptg3D_BE ;
    end ;

    procedure readType ;
    begin
      wkbtype := read_dword ;

      if ( wkbtype and EWKB_SRID_FLAG ) = EWKB_SRID_FLAG then
        srid := read_dword ;

      if ( wkbtype and EWKB_ZOFFSET_FLAG = EWKB_ZOFFSET_FLAG ) then
        hasz := True ;

      if ( wkbtype and EWKB_MOFFSET_FLAG = EWKB_MOFFSET_FLAG ) then
        hasm := True ;

      wkbtype := wkbtype and not EWKB_ALL_FLAGS ;

      if wkbtype < 1000 then begin
        if hasz then
          wkbtype := wkbtype + 1000 ;
        if hasm then
          wkbtype := wkbtype + 2000 ;
      end ;
    end ;

    procedure parse_wkb_point(
      const _shp : TGIS_Shape
    ) ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 1    ) or
              ( wkbtype = 1001 ) or
              ( wkbtype = 2001 ) or
              ( wkbtype = 3001 )
            ) ;

      if _shp.IsEmpty then
        _shp.AddPart ;

      if wkbdim = 0 then
        _shp.AddPoint( read_ptg )
      else
        _shp.AddPoint3D( read_ptg3D ) ;
    end ;

    procedure parse_wkb_linestring(
      const _shp      : TGIS_Shape ;
      const _addPart  : Boolean
    ) ;
    var
      p, points : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 2    ) or ( wkbtype = 1002 ) or
              ( wkbtype = 2002 ) or ( wkbtype = 3002 )
            ) ;

      if _addPart then
        _shp.AddPart ;

      points := read_dword ;

      for p := 0 to points - 1 do
        if wkbdim = 0 then
          _shp.AddPoint( read_ptg )
        else
          _shp.AddPoint3D( read_ptg3D ) ;
    end ;

    procedure parse_wkb_polygon(
      const _shp : TGIS_Shape
    ) ;
    var
      r, rings  : Integer ;
      p, points : Integer ;
      rewind    : Boolean ;
      area      : Double  ;
      xa, ya    : Double  ;
      xb, yb    : Double  ;
      ptg_s,
      ptg_a,
      ptg_b     : TGIS_Point   ;
      ptg3D_s,
      ptg3D_a,
      ptg3D_b   : TGIS_Point3D ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 3    ) or
              ( wkbtype = 1003 ) or
              ( wkbtype = 2003 ) or
              ( wkbtype = 3003 )
            ) ;

      if _shp.IsEmpty then
        _shp.AddPart ;

      rings := read_dword ;

      if wkbdim = 0 then begin
        for r := 0 to rings - 1 do begin
          _shp.AddPart ;
          points := read_dword ;
          rewind := False ;

          if points > 0 then begin
            {$IFDEF GIS_NORECORDS}
              arpart := new TGIS_Point[points] ;
              ptg_s  := new TGIS_Point ;
            {$ELSE}
              SetLength( arpart, points );
            {$ENDIF}

            ptg_s.X := 1e308 ;
            ptg_s.Y := 1e308 ;

            for p := 0 to points - 1 do begin
              ptg_a := read_ptg ;
              arpart[p] := ptg_a ;
              ptg_s.X := Min( ptg_s.X, ptg_a.X ) ;
              ptg_s.Y := Min( ptg_s.Y, ptg_a.Y ) ;
            end ;

            area  := 0 ;
            xa    := 0 ;
            ya    := 0 ;
            ptg_a := arpart[0] ;

            for p := 1 to points - 1 do begin
              ptg_b := arpart[p] ;

              if p = 1 then begin
                xa := ptg_a.X - ptg_s.X ;
                ya := ptg_a.Y - ptg_s.Y ;
              end ;

              xb := ptg_b.X - ptg_s.X ;
              yb := ptg_b.Y - ptg_s.Y ;

              area := area + (yb*xa -ya*xb) ;

              ptg_a := ptg_b ;
              xa    := xb ;
              ya    := yb ;
            end ;
            // add last segment
            xb := ptg_b.X - ptg_s.X ;
            yb := ptg_b.Y - ptg_s.Y ;
            area := area + (yb*xa -ya*xb) ;

            if r = 0
              then rewind := area >= 0 // first ring must be clockwise
              else rewind := area <  0 // other rings must be counterclockwise
          end ;

          if rewind then begin
            for p := points - 1 downto 0 do
              _shp.AddPoint( arpart[p] )
          end
          else begin
            for p := 0 to points - 1 do
              _shp.AddPoint( arpart[p] )
          end ;
        end ;
      end
      else begin
        for r := 0 to rings - 1 do begin
          _shp.AddPart ;
          points := read_dword ;
          rewind := False ;

          if points > 0 then begin
            {$IFDEF GIS_NORECORDS}
              arpart3D := new TGIS_Point3D[points] ;
              ptg3D_s  := new TGIS_Point3D ;
            {$ELSE}
              SetLength( arpart3D, points );
            {$ENDIF}

            ptg3D_s.X := 1e308 ;
            ptg3D_s.Y := 1e308 ;

            for p := 0 to points - 1 do begin
              ptg3D_a := read_ptg3D ;
              arpart3D[p] := ptg3D_a ;
              ptg3D_s.X := Min( ptg3D_s.X, ptg3D_a.X ) ;
              ptg3D_s.Y := Min( ptg3D_s.Y, ptg3D_a.Y ) ;
            end ;

            area    := 0 ;
            xa      := 0 ;
            ya      := 0 ;
            ptg3D_a := arpart3D[0] ;

            for p := 1 to points - 1 do begin
              ptg3D_b  := arpart3D[p] ;

              if p = 1 then begin
                xa := ptg3D_a.X - ptg3D_s.X ;
                ya := ptg3D_a.Y - ptg3D_s.Y ;
              end ;

              xb := ptg3D_b.X - ptg3D_s.X ;
              yb := ptg3D_b.Y - ptg3D_s.Y ;

              area := area + (yb*xa -ya*xb) ;

              ptg3D_a := ptg3D_b ;
              xa      := xb ;
              ya      := yb ;
            end ;
            // add last segment
            xb := ptg3D_b.X - ptg3D_s.X ;
            yb := ptg3D_b.Y - ptg3D_s.Y ;
            area := area + (yb*xa -ya*xb) ;

            if r = 0
              then rewind := area >= 0 // first ring must be clockwise
              else rewind := area <  0 // other rings must be counterclockwise
          end ;

          if rewind then begin
            for p := points - 1 downto 0 do
              _shp.AddPoint3D( arpart3D[p] )
          end
          else begin
            for p := 0 to points - 1 do
              _shp.AddPoint3D( arpart3D[p] )
          end ;
        end ;
      end ;
    end ;

    procedure parse_wkb_multipoint(
      const _shp : TGIS_Shape
    ) ;
    var
      p, points : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 4    ) or
              ( wkbtype = 1004 ) or
              ( wkbtype = 2004 ) or
              ( wkbtype = 3004 )
            ) ;

      points := read_dword ;

      for p := 0 to points - 1 do
        parse_wkb_point( _shp ) ;
    end ;

    procedure parse_wkb_multilinestring(
      const _shp : TGIS_Shape
    ) ;
    var
      l, linestrings : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 5    ) or
              ( wkbtype = 1005 ) or
              ( wkbtype = 2005 ) or
              ( wkbtype = 3005 )
            ) ;

      linestrings := read_dword ;

      for l := 0 to linestrings - 1 do
        parse_wkb_linestring( _shp, True ) ;
    end ;

    procedure parse_wkb_multilipolygon(
      const _shp : TGIS_Shape
    ) ;
    var
      p, polygons : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 6    ) or
              ( wkbtype = 1006 ) or
              ( wkbtype = 2006 ) or
              ( wkbtype = 3006 )
            ) ;

      polygons := read_dword ;

      for p := 0 to polygons - 1 do
        parse_wkb_polygon( _shp ) ;
    end ;

    procedure buildArc(
      const _first : TGIS_Point ;
      const _axis  : TGIS_Point ;
      const _last  : TGIS_Point ;
      const _shp   : TGIS_Shape
    ) ;
    var
      radius,
      start_angle,
      end_angle   : Double ;
      center      : TGIS_Point;
    begin
      if TGIS_GeometryFactory.GisArcFrom3Points(
          _first, _axis, _last, center, radius, start_angle, end_angle
         )
      then
        _shp.StrokeArc( GisPoint3DFrom2D( center ), radius, radius, start_angle, end_angle, 0 )
      else begin
        _shp.AddPoint( _first ) ;
        _shp.AddPoint( _last  ) ;
      end ;
    end ;

    procedure buildArc3D(
      const _first : TGIS_Point3D ;
      const _axis  : TGIS_Point3D ;
      const _last  : TGIS_Point3D ;
      const _shp   : TGIS_Shape
    ) ;
    var
      radius,
      start_angle,
      end_angle   : Double ;
      center      : TGIS_Point3D;
    begin
      if TGIS_GeometryFactory.GisArcFrom3Points3D(
          _first, _axis, _last, center, radius, start_angle, end_angle
         )
      then
        _shp.StrokeArc( center, radius, radius, start_angle, end_angle, 0 )
      else begin
        _shp.AddPoint3D( _first ) ;
        _shp.AddPoint3D( _last  ) ;
      end ;
    end ;

    procedure parse_wkb_circularstring(
      const _shp      : TGIS_Shape ;
      const _addPart  : Boolean
    ) ;
    var
      p, points        : Integer    ;
      p1, p2, p3       : TGIS_Point ;
      parr             : array of TGIS_Point ;
      p13d, p23d, p33d : TGIS_Point3D ;
      parr3d           : array of TGIS_Point3D ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 8    ) or ( wkbtype = 1008 ) or
              ( wkbtype = 2008 ) or ( wkbtype = 3008 )
            ) ;

      if _addPart then
        _shp.AddPart ;

      points := read_dword ;

      if wkbdim = 0 then
        SetLength( parr, points )
      else
        SetLength( parr3d, points ) ;

      for p := 0 to points - 1 do begin
        if wkbdim = 0 then
          parr[p] := read_ptg
        else
          parr3d[p] := read_ptg3D ;
      end ;

      // use GisArcFrom3Points
      p := 0 ;
      while p < points-2 do begin
        if wkbdim = 0 then begin
          p1 := parr[p] ;
          p2 := parr[p+1] ;
          p3 := parr[p+2] ;
          buildArc( p1, p2, p3, _shp ) ;
        end
        else begin
          p13d := parr3d[p] ;
          p23d := parr3d[p+1] ;
          p33d := parr3d[p+2] ;
          buildArc3D( p13d, p23d, p33d, _shp ) ;
        end ;

        inc( p, 2 ) ;
      end ;

      // uncomment for simplified tests
//      for p := 0 to points - 1 do
//        if wkbdim = 0 then
//          _shp.AddPoint( parr[p] )
//        else
//          _shp.AddPoint3D( parr3d[p] ) ;
    end ;

    procedure parse_wkb_compoundcurve(
      const _shp      : TGIS_Shape ;
      const _addPart  : Boolean
    ) ;
    var
      p, geometryCount : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 9    ) or ( wkbtype = 1009 ) or
              ( wkbtype = 2009 ) or ( wkbtype = 3009 )
            ) ;

      if _addPart then
        _shp.AddPart ;

      geometryCount := read_dword ;

      for p := 0 to geometryCount - 1 do begin
        le := read_byte = 1 ;
        readType ;
         {$IFDEF MANAGED}
           vr_off := vr_off - OFFSET_WKB ;
         {$ELSE}
           ptvar := Pointer( NativeInt( ptvar ) - OFFSET_WKB ) ;
         {$ENDIF}
        case wkbtype of
          2,
          1002,
          2002,
          3002 : //  LineString
            parse_wkb_linestring( _shp, _addPart ) ;
          8,
          1008,
          2008,
          3008 : //  CircularString
            parse_wkb_circularstring( _shp, _addPart ) ;
        end ;
      end ;
    end ;

    procedure parse_wkb_multicurve(
      const _shp      : TGIS_Shape ;
      const _addPart  : Boolean
    ) ;
    var
      p, ringCount : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 10   ) or
              ( wkbtype = 1010 ) or
              ( wkbtype = 2010 ) or
              ( wkbtype = 3010 ) or
              ( wkbtype = 11   ) or
              ( wkbtype = 1011 ) or
              ( wkbtype = 2011 ) or
              ( wkbtype = 3011 )
            ) ;

      ringCount := read_dword ;

      for p := 0 to ringCount - 1 do begin
        if _addPart then
          _shp.AddPart ;

        le := read_byte = 1 ;
        readType ;
         {$IFDEF MANAGED}
           vr_off := vr_off - OFFSET_WKB ;
         {$ELSE}
           ptvar := Pointer( NativeInt( ptvar ) - OFFSET_WKB ) ;
         {$ENDIF}

        case wkbtype of
          2,
          1002,
          2002,
          3002 : //  LineString
            parse_wkb_linestring( _shp, False ) ;
          8,
          1008,
          2008,
          3008 : //  CircularString
            parse_wkb_circularstring( _shp, False ) ;
          9,
          1009,
          2009,
          3009 : //  CompoundCurve
            parse_wkb_compoundcurve( _shp, False ) ;
        end ;
      end ;
    end ;

    procedure parse_wkb_multisurface(
      const _shp : TGIS_Shape
    ) ;
    var
      p, geometryCount : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 12   ) or ( wkbtype = 1012 ) or
              ( wkbtype = 2012 ) or ( wkbtype = 3012 )
            ) ;

      geometryCount := read_dword ;

      for p := 0 to geometryCount - 1 do begin
        le := read_byte = 1 ;
        readType ;
         {$IFDEF MANAGED}
           vr_off := vr_off - OFFSET_WKB ;
         {$ELSE}
           ptvar := Pointer( NativeInt( ptvar ) - OFFSET_WKB ) ;
         {$ENDIF}
        case wkbtype of
          3,
          1003,
          2003,
          3003 : //  Polygon
            parse_wkb_polygon( _shp ) ;
          10,
          1010,
          2010,
          3010 : //  CurvePolygon
            parse_wkb_multicurve( _shp, True ) ;
        end ;
      end ;
    end ;

    procedure parse_wkb_polyhedralsurface(
      const _shp : TGIS_Shape
    ) ;
    var
      p, polygons : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 15   ) or
              ( wkbtype = 1015 ) or
              ( wkbtype = 2015 ) or
              ( wkbtype = 3015 )
            ) ;

      polygons := read_dword ;

      for p := 0 to polygons - 1 do
        parse_wkb_polygon( _shp ) ;
    end ;

    procedure parse_wkb_triangle(
      const _shp      : TGIS_Shape ;
      const _addPart  : Boolean
    ) ;
    var
      p, points, r, ringCount : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 17   ) or ( wkbtype = 1017 ) or
              ( wkbtype = 2017 ) or ( wkbtype = 3017 )
            ) ;

      ringCount := read_dword ;
      for r := 0 to ringCount - 1 do begin
        if _addPart then
          _shp.AddPart ;

        points := read_dword ;
        for p := 0 to points - 1 do
          if wkbdim = 0 then
            _shp.AddPoint( read_ptg )
          else
            _shp.AddPoint3D( read_ptg3D ) ;
      end ;
    end ;

    procedure parse_wkb_tin(
      const _shp : TGIS_Shape
    ) ;
    var
      p, triangles : Integer    ;
    begin
      le := read_byte = 1 ;
      readType ;
      assert( ( wkbtype = 16   ) or
              ( wkbtype = 1016 ) or
              ( wkbtype = 2016 ) or
              ( wkbtype = 3016 )
            ) ;

      triangles := read_dword ;

      for p := 0 to triangles - 1 do
        parse_wkb_triangle( _shp, True ) ;
    end ;

    function getDim(
      const _val : Integer
    ) : TGIS_DimensionType ;
    begin
      case _val of
        1 : Result := TGIS_DimensionType.XYZ  ;
        2 : Result := TGIS_DimensionType.XYM  ;
        3 : Result := TGIS_DimensionType.XYZM ;
      else  Result := TGIS_DimensionType.XY   ;
      end ;
    end ;

    function parseShape
      : TGIS_Shape ;
    var
      num : Integer ;
      i   : Integer ;
    begin
      wkbdim := wkbtype div 1000 ;

      case wkbtype of
        1,
        1001,
        2001,
        3001 : //  Point = 1
             begin
               Result := TGIS_ShapePoint.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_point( Result ) ;
               Result.Unlock ;
             end ;
        2,
        1002,
        2002,
        3002 : //  LineString = 2
             begin
               Result := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_linestring( Result, True ) ;
               Result.Unlock ;
             end ;
        3,
        1003,
        2003,
        3003 : //  Polygon = 3
             begin
               Result := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_polygon( Result ) ;
               Result.Unlock ;
             end ;
        4,
        1004,
        2004,
        3004 : //  MultiPoint = 4
             begin
               Result := TGIS_ShapeMultiPoint.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_multipoint( Result ) ;
               Result.Unlock ;
             end ;
        5,
        1005,
        2005,
        3005 : //  MultiLineString = 5
             begin
               Result := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_multilinestring( Result ) ;
               Result.Unlock ;
             end ;
        6,
        1006,
        2006,
        3006 : //  MultiPolygon = 6
             begin
               Result := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_multilipolygon( Result ) ;
               Result.Unlock ;
             end ;
        7,
        1007,
        2007,
        3007 : //  GeometryCollection = 7
             begin
               Result := TGIS_ShapeComplex.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 le  := read_byte = 1 ;
                 readType ;
                 num := read_dword ;
                 for i := 0 to num - 1 do begin
                   le := read_byte = 1 ;
                   readType ;
                   {$IFDEF MANAGED}
                     vr_off := vr_off - OFFSET_WKB ;
                   {$ELSE}
                     ptvar := Pointer( NativeInt( ptvar ) - OFFSET_WKB ) ;
                   {$ENDIF}
                   TGIS_ShapeComplex( Result ).AddShape( parseShape ) ;
                 end ;
               Result.Unlock ;
             end ;
        8,
        1008,
        2008,
        3008 : //  CircularString = 8
             begin
               Result := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_circularstring( Result, True ) ;
               Result.Unlock ;
             end ;
        9,
        1009,
        2009,
        3009 : //  CompoundCurve = 9
             begin
               Result := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 Result.AddPart ;
                 parse_wkb_compoundcurve( Result, False ) ;
               Result.Unlock ;
             end ;
        10,
        1010,
        2010,
        3010 : //  CurvePolygon = 10
             begin
               Result := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_multicurve( Result, True ) ;
               Result.Unlock ;
             end ;
        11,
        1011,
        2011,
        3011 : //  MultiCurve = 11
             begin
               Result := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_multicurve( Result, True ) ;
               Result.Unlock ;
             end ;
        12,
        1012,
        2012,
        3012 : //  MultiSurface = 12
             begin
               Result := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_multisurface( Result ) ;
               Result.Unlock ;
             end ;
        15,
        1015,
        2015,
        3015 : //  PolyhedralSurface = 15
             begin
               Result := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_polyhedralsurface( Result ) ;
               Result.Unlock ;
             end ;
        16,
        1016,
        2016,
        3016 : //  Tin = 16
             begin
               Result := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_tin( Result ) ;
               Result.Unlock ;
             end ;
        17,
        1017,
        2017,
        3017 : //  Triangle = 17
             begin
               Result := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           getDim( wkbdim )
                         ) ;
               Result.Lock( TGIS_Lock.Internal ) ;
                 parse_wkb_triangle( Result, True ) ;
               Result.Unlock ;
             end ;
        else begin
          if ewkb then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDEWKB ), '', wkbtype )
          else
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDWKB  ), '', wkbtype ) ;
        end ;
      end ;
    end ;

  begin
    {$IFDEF MANAGED}
      vr_off := 0 ;
    {$ELSE}
      ptvar     := _wkb ;
      ptvar_tmp := ptvar ;
    {$ENDIF}
    le      := read_byte = 1 ;
    wkbtype := read_dword ;
    ewkb    := False ;
    hasz    := False ;
    hasm    := False ;
    srid    := 0 ;

    if ( wkbtype and EWKB_ZOFFSET_FLAG = EWKB_ZOFFSET_FLAG ) then
      hasz := True ;

    if ( wkbtype and EWKB_MOFFSET_FLAG = EWKB_MOFFSET_FLAG ) then
      hasm := True ;

    if ( wkbtype and not EWKB_ALL_FLAGS = 7 ) then begin
      if ( wkbtype and EWKB_SRID_FLAG ) = EWKB_SRID_FLAG then begin
        ewkb := True ;
        srid := read_dword ;
      end ;
    end ;

    // Remove flags for compatibility with standard WKB
    if hasz or hasm or ( wkbtype and EWKB_SRID_FLAG = EWKB_SRID_FLAG ) then
      wkbtype := (wkbtype and not EWKB_ALL_FLAGS) ;

    if hasz then
      wkbtype := wkbtype + 1000 ;
    if hasm then
      wkbtype := wkbtype + 2000 ;

    {$IFDEF MANAGED}
      vr_off := 0 ;
    {$ELSE}
      ptvar := ptvar_tmp ;
    {$ENDIF}

    try
      Result  := parseShape ;
    except
      {$IFDEF MANAGED}
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTED_PARSING ),
                                     '',
                                     vr_off
                                   ) ;
      {$ELSE}
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTED_PARSING ),
                                     '',
                                     NativeInt(ptvar) - NativeInt(ptvar_tmp)
                                   ) ;
      {$ENDIF}
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromWKB(
     {$IFDEF MANAGED}
       const _wkb   : TBytes               ;
     {$ELSE}
       const _wkb   : Pointer              ;
     {$ENDIF}
       const _size  : Integer
    ) : TGIS_Shape ;
  begin
    Result := buildShapeFromEWKB( _wkb, nil, nil, False, -1, nil ) ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromWKB(
    const _wkb    : OleVariant ;
    const _source : TGIS_Shape ;
    {$IFDEF MANAGED}
      const _ptr  : TGIS_Bytes ;
    {$ELSE}
      const _ptr  : Pointer    ;
    {$ENDIF}
    const _mapped : Boolean    ;
    const _uid    : TGIS_Uid    ;
    const _layer  : TGIS_LayerVector
  ) : TGIS_Shape ;
  var
    {$IFDEF MANAGED}
      vr          : TBytes  ;
    {$ELSE}
      ptvar       : Pointer ;
    {$ENDIF}
  begin
    {$IFDEF MANAGED}
      vr := TBytes(TObject( _wkb )) ;
    {$ELSE}
      ptvar := VarArrayLock( _wkb ) ;
    {$ENDIF}

    try
      {$IFDEF MANAGED}
        Result := buildShapeFromEWKB( vr, _source, _ptr, _mapped, _uid, _layer ) ;
      {$ELSE}
        Result := buildShapeFromEWKB( ptvar, _source, _ptr, _mapped, _uid, _layer ) ;
      {$ENDIF}
    finally
      {$IFNDEF MANAGED}
        VarArrayUnLock( _wkb ) ;
      {$ENDIF}
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromEWKB(
     {$IFDEF MANAGED}
       const _ewkb  : TBytes  ;
     {$ELSE}
       const _ewkb  : Pointer ;
     {$ENDIF}
       const _size  : Integer
  ) : TGIS_Shape ;
  begin
    Result := buildShapeFromEWKB( _ewkb, nil, nil, False, -1, nil ) ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromEWKB(
    const _ewkb   : OleVariant ;
    const _source : TGIS_Shape ;
    {$IFDEF MANAGED}
      const _ptr  : TGIS_Bytes ;
    {$ELSE}
      const _ptr  : Pointer    ;
    {$ENDIF}
    const _mapped : Boolean    ;
    const _uid    : TGIS_Uid    ;
    const _layer  : TGIS_LayerVector
  ) : TGIS_Shape ;
  var
    {$IFDEF MANAGED}
      vr          : TBytes  ;
    {$ELSE}
      ptvar       : Pointer ;
    {$ENDIF}
  begin
    {$IFDEF MANAGED}
      vr := TBytes(TObject( _ewkb )) ;
    {$ELSE}
      ptvar := VarArrayLock( _ewkb ) ;
    {$ENDIF}

    try
      {$IFDEF MANAGED}
        Result := buildShapeFromEWKB( vr, _source, _ptr, _mapped, _uid, _layer ) ;
      {$ELSE}
        Result := buildShapeFromEWKB( ptvar, _source, _ptr, _mapped, _uid, _layer ) ;
      {$ENDIF}
    finally
      {$IFNDEF MANAGED}
        VarArrayUnLock( _ewkb ) ;
      {$ENDIF}
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromGDO(
    const _gdo : OleVariant
  ) : TGIS_Shape ;
  var
    shp : TGIS_ShapePoint ;
  begin
    shp := TGIS_ShapePoint.Create( nil, nil, False, 0, nil ) ;
    try
      try
        if VarIsNull( _gdo ) then
          Result := nil
        else
          Result := shp.CreateFromGDO( _gdo ) ;
      except
        Result := nil ;
      end ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromJSON(
    const _json : String
  ) : TGIS_Shape ;
  var
    shp : TGIS_ShapePoint ;
  begin
    shp := TGIS_ShapePoint.Create( nil, nil, False, 0, nil );
    try
      try
        Result := shp.CreateFromJSON( _json ) ;
      except
        Result := nil ;
      end ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromJSON(
    const _json   : String     ;
    const _source : TGIS_Shape ;
    {$IFDEF MANAGED}
      const _ptr  : TGIS_Bytes ;
    {$ELSE}
      const _ptr  : Pointer    ;
    {$ENDIF}
    const _mapped : Boolean    ;
    const _uid    : TGIS_Uid    ;
    const _layer  : TGIS_LayerVector
  ) : TGIS_Shape ;
  var
    tkn  : TGIS_JSONTokenizer ;
    obj  : TGIS_JSONObject ;

    function geoJSONGetDimension(
      const _obj : TGIS_JSONObject
    ) : TGIS_DimensionType ;
    begin
      Result := TGIS_DimensionType.Unknown ;
      if JSONObjectGetType( _obj ) = TGIS_JSONType.Array then
        case _obj.AsArray.Count of
          2 : Result := TGIS_DimensionType.XY   ;
          3 : Result := TGIS_DimensionType.XYZ  ;
          4 : Result := TGIS_DimensionType.XYZM ;
        else  Result := TGIS_DimensionType.Unknown ;
        end ;
    end ;

    procedure geoJSONReadRawPoint(
      const _obj : TGIS_JSONObject ;
      const _shp : TGIS_Shape
    ) ;
    var
      numPtg : Integer ;
      ptg    : TGIS_Point3D ;
    begin
      if JSONObjectGetType( _obj ) = TGIS_JSONType.Array then begin

        numPtg := _obj.AsArray.Count ;
        {$IFDEF GIS_NORECORDS}
          ptg := new TGIS_Point3D ;
        {$ENDIF}
        if numPtg > 1  then begin
          ptg.X := TGIS_JSONObject( _obj.AsArray[ 0 ] ).AsDouble ;
          ptg.Y := TGIS_JSONObject( _obj.AsArray[ 1 ] ).AsDouble ;
          ptg.Z := 0 ;
          ptg.M := 0 ;
        end ;

        if numPtg > 2  then
          ptg.Z := TGIS_JSONObject( _obj.AsArray[ 2 ] ).AsDouble ;

        if numPtg > 3  then
          ptg.M := TGIS_JSONObject( _obj.AsArray[ 3 ] ).AsDouble ;

        _shp.AddPoint3D( ptg ) ;
      end ;
    end ;

    function geoJSONReadPoint(
      const _obj : TGIS_JSONObject
    ) : TGIS_Shape ;
    var
      itr       : TGIS_JSONIter ;
      currShape : TGIS_Shape ;
      dim       : TGIS_DimensionType ;
    begin
      currShape := nil ;
      try
        if JSONObjectFindFirst( 'coordinates', _obj, itr ) then begin
          dim := geoJSONGetDimension( itr.val ) ;
          currShape := TGIS_ShapePoint.Create(
                         _source, _ptr, _mapped, _uid, _layer, dim
                       ) ;
          try
            currShape.Lock( TGIS_Lock.Projection ) ;
            currShape.AddPart ;

            geoJSONReadRawPoint( itr.val, currShape ) ;
          finally
            currShape.Unlock ;
          end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
      Result := currShape ;
    end ;

    function geoJSONReadMultiPoint(
      const _obj : TGIS_JSONObject
    ) : TGIS_Shape ;
    var
      itr       : TGIS_JSONIter ;
      i         : Integer ;
      currShape : TGIS_Shape ;
      dim       : TGIS_DimensionType ;
    begin
      currShape := nil ;
      try
        if JSONObjectFindFirst( 'coordinates', _obj, itr ) then begin
          if (JSONObjectGetType( itr.val ) = TGIS_JSONType.Array) and
             ( itr.val.AsArray.Count > 0 ) then begin

            dim := geoJSONGetDimension( TGIS_JSONObject( itr.val.AsArray[ 0 ] ) ) ;
            currShape := TGIS_ShapeMultiPoint.Create(
                           _source, _ptr, _mapped, _uid, _layer, dim
                         ) ;
          try
              currShape.Lock( TGIS_Lock.Projection ) ;
              for i := 0 to itr.val.AsArray.Count - 1 do begin
                currShape.AddPart ;
                geoJSONReadRawPoint(
                  TGIS_JSONObject( itr.val.AsArray[ i ] ),
                  currShape
                ) ;
              end ;
          finally
            currShape.Unlock ;
          end ;
        end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
      Result := currShape ;
    end ;

    function geoJSONReadLineString(
      const _obj : TGIS_JSONObject
    ) : TGIS_Shape ;
    var
      itr       : TGIS_JSONIter ;
      i         : Integer ;
      currShape : TGIS_Shape ;
      dim       : TGIS_DimensionType ;
    begin
      currShape := nil ;
      try
        dim := TGIS_DimensionType.XY ;
        if JSONObjectFindFirst( 'coordinates', _obj, itr ) then begin
          if ( itr.val.AsArray.Count > 0 ) then
            dim := geoJSONGetDimension( TGIS_JSONObject( itr.val.AsArray[ 0 ] ) ) ;
          currShape := TGIS_ShapeArc.Create(
                         _source, _ptr, _mapped, _uid, _layer, dim
                       ) ;
          try
            currShape.Lock( TGIS_Lock.Projection ) ;
            currShape.AddPart ;
            if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
              for i := 0 to itr.val.AsArray.Count - 1 do begin
                geoJSONReadRawPoint(
                  TGIS_JSONObject( itr.val.AsArray[ i ] ),
                  currShape
                ) ;
              end ;
            end ;
          finally
            currShape.Unlock ;
          end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
      Result := currShape ;
    end ;

    function geoJSONReadMultiLineString(
      const _obj : TGIS_JSONObject
    ) : TGIS_Shape ;
    var
      itr       : TGIS_JSONIter ;
      i, j      : Integer ;
      currShape : TGIS_Shape ;
      dim       : TGIS_DimensionType ;
    begin
      currShape := nil ;
      try
        if JSONObjectFindFirst( 'coordinates', _obj, itr ) then begin
          if (JSONObjectGetType( itr.val ) = TGIS_JSONType.Array) and
             ( itr.val.AsArray.Count > 0 ) and
             ( TGIS_JSONObject(itr.val.AsArray[0]).AsArray.Count > 0 ) then begin

            dim := geoJSONGetDimension( TGIS_JSONObject(TGIS_JSONObject(itr.val.AsArray[0]).AsArray[0])) ;
            currShape := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer, dim
                         ) ;
          try
              currShape.Lock( TGIS_Lock.Projection ) ;
              for i := 0 to itr.val.AsArray.Count - 1 do begin
                currShape.AddPart ;
                for j := 0 to TGIS_JSONObject(itr.val.AsArray[i]).AsArray.Count-1 do
                  geoJSONReadRawPoint(
                    TGIS_JSONObject(
                      TGIS_JSONObject( itr.val.AsArray[ i ] ).AsArray[ j ]
                    ),
                    currShape
                  ) ;
              end ;
          finally
            currShape.Unlock ;
          end ;
        end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
      Result := currShape ;
    end ;

    function geoJSONReadPolygon(
      const _obj : TGIS_JSONObject
    ) : TGIS_Shape ;
    var
      itr       : TGIS_JSONIter ;
      i, j      : Integer ;
      currShape : TGIS_Shape ;
      dim       : TGIS_DimensionType ;
    begin
      currShape := nil ;
      try
        if JSONObjectFindFirst( 'coordinates', _obj, itr ) then begin
          if (JSONObjectGetType( itr.val ) = TGIS_JSONType.Array) and
             ( itr.val.AsArray.Count > 0 ) and
             ( TGIS_JSONObject(itr.val.AsArray[0]).AsArray.Count > 0 ) then begin

            dim := geoJSONGetDimension( TGIS_JSONObject(TGIS_JSONObject(itr.val.AsArray[0]).AsArray[0])) ;
            currShape := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer, dim
                         ) ;
          try
              currShape.Lock( TGIS_Lock.Projection ) ;
              for i := 0 to itr.val.AsArray.Count - 1 do begin
                currShape.AddPart ;
                for j := 0 to TGIS_JSONObject(itr.val.AsArray[i]).AsArray.Count-1 do
                  geoJSONReadRawPoint(
                    TGIS_JSONObject(
                      TGIS_JSONObject( itr.val.AsArray[ i ] ).AsArray[ j ]
                    ),
                    currShape
                  ) ;
              end ;
          finally
            currShape.Unlock ;
          end ;
        end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
      Result := currShape ;
    end ;

    function geoJSONReadMultiPolygon(
      const _obj : TGIS_JSONObject
    ) : TGIS_Shape ;
    var
      itr       : TGIS_JSONIter ;
      i, j, k   : Integer ;
      oPoly     : TGIS_JSONObject ;
      oRing     : TGIS_JSONObject ;
      currShape : TGIS_Shape ;
      dim       : TGIS_DimensionType ;
    begin
      currShape := nil ;
      try
        if JSONObjectFindFirst( 'coordinates', _obj, itr ) then begin
          if ( JSONObjectGetType( itr.val ) = TGIS_JSONType.Array ) then begin
            dim := TGIS_DimensionType.XYZM ;
            for i := 0 to Min( 0, itr.val.AsArray.Count-1 ) do begin
              oPoly := TGIS_JSONObject( itr.val.AsArray[ i ] ) ;
              if JSONObjectGetType( oPoly ) = TGIS_JSONType.Array then begin
                for j := 0 to Min( 0, oPoly.AsArray.Count-1 ) do begin
                  oRing := TGIS_JSONObject( oPoly.AsArray[ j ] );
                  if JSONObjectGetType( oRing ) = TGIS_JSONType.Array then begin
                    for k := 0 to Min( 0, oRing.AsArray.Count-1 ) do begin
                      dim := geoJSONGetDimension( TGIS_JSONObject( oRing.AsArray[ k ] ) ) ;
                      break ;
                    end ;
                  end ;
                end ;
              end ;
            end ;

            currShape := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer, dim
                         ) ;
          try
              currShape.Lock( TGIS_Lock.Projection ) ;
              for i := 0 to itr.val.AsArray.Count - 1 do begin
                oPoly := TGIS_JSONObject( itr.val.AsArray[ i ] ) ;
                if JSONObjectGetType( oPoly ) = TGIS_JSONType.Array then begin

                  for j := 0 to oPoly.AsArray.Count - 1 do begin
                    oRing := TGIS_JSONObject( oPoly.AsArray[ j ] );
                    currShape.AddPart ;
                    if JSONObjectGetType( oRing ) = TGIS_JSONType.Array then
                    begin
                      for k := 0 to oRing.AsArray.Count - 1 do
                        geoJSONReadRawPoint(
                          TGIS_JSONObject( oRing.AsArray[ k ] ),
                          currShape
                        ) ;
                    end ;
                  end ;
                end ;
              end ;
          finally
            currShape.Unlock ;
          end ;
        end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
      Result := currShape ;
    end ;

    function geoJSONreadGeometry(
      const _obj : TGIS_JSONObject
    ) : TGIS_Shape ; forward ;

    function geoJSONReadGeometryCollection(
      const _obj : TGIS_JSONObject
    ) : TGIS_Shape  ;
    var
      itr       : TGIS_JSONIter ;
      i         : Integer ;
      currShape : TGIS_Shape ;
    begin
      currShape := TGIS_ShapeComplex.Create(
                     _source, _ptr, _mapped, _uid, _layer, TGIS_DimensionType.XYZM
                   ) ;
      try
        if JSONObjectFindFirst( 'geometries', _obj, itr ) then begin
          if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
            for i := 0 to itr.val.AsArray.Count - 1 do begin
              TGIS_ShapeComplex( currShape ).AddShape(
                geoJSONreadGeometry(
                  TGIS_JSONObject( itr.val.AsArray[ i ] ) )
              ) ;
            end ;
          end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
      Result := currShape ;
    end ;

    function geoJSONreadGeometry(
      const _obj : TGIS_JSONObject
    ) : TGIS_Shape ;
    var
      itr     : TGIS_JSONIter ;
      tName   : String ;
    begin
      Result := nil ;
      try
        if JSONObjectFindFirst( 'type', _obj, itr ) then begin
          tName := itr.val.AsString ;

          if      tName = 'Point' then
            Result := geoJSONReadPoint( _obj )
          else if tName = 'LineString' then
            Result := geoJSONReadLineString( _obj )
          else if tName = 'Polygon' then
            Result := geoJSONReadPolygon( _obj )
          else if tName = 'MultiPoint' then
            Result := geoJSONReadMultiPoint( _obj )
          else if tName = 'MultiLineString' then
            Result := geoJSONReadMultiLineString( _obj )
          else if tName = 'MultiPolygon' then
            Result := geoJSONReadMultiPolygon( _obj )
          else if tName = 'GeometryCollection' then
            Result := geoJSONReadGeometryCollection( _obj )
          else
            raise EGIS_Exception.Create( GIS_RS_ERR_UNSUPPORTEDJSON, tName, 0 );
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
    end ;

  begin
    Result := nil ;

    tkn := TGIS_JSONTokenizer.Create ;
    try
      obj := tkn.Parse( _json ) ;
      if assigned( obj ) then begin
        Result := geoJSONreadGeometry( obj ) ;
        FreeObject( obj ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromGML(
    const _gml : String
  ) : TGIS_Shape ;
  var
    shp : TGIS_ShapePoint ;
  begin
    shp := TGIS_ShapePoint.Create( nil, nil, False, 0, nil );
    try
      try
        Result := shp.CreateFromGML( _gml ) ;
      except
        Result := nil ;
      end ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromGML(
     const _gml     : String                   ;
     const _source  : TGIS_Shape               ;
     {$IFDEF MANAGED}
       const _ptr   : TGIS_Bytes               ;
     {$ELSE}
       const _ptr   : Pointer                  ;
     {$ENDIF}
     const _mapped  : Boolean                  ;
     const _uid     : TGIS_Uid                  ;
     const _layer   : TGIS_LayerVector
   ) : TGIS_Shape ;
  var
    ldim  : Integer ;
    gdim  : Integer ;

      function parseGMLCoords( const _gnode : IXMLNode ;
                               const _shape : TGIS_Shape
                              ) : Integer ;
      var
        cnode    : IXMLNode ;
        xnode    : IXMLNode ;
        ynode    : IXMLNode ;
        znode    : IXMLNode ;
        cstr     : String ;
        str      : String ;
        i        : Integer ;
        x,y,z,m  : Double ;
        coordDim : Integer ;
        tkn      : TGIS_Tokenizer ;
        found    : Boolean ;
      begin
        coordDim := 0 ;
        Result   := coordDim ;
        if not assigned( _gnode ) then exit ;

        cnode := _gnode.ChildNodes.FindNode('coordinates') ;

        if assigned( cnode ) then begin
          cstr := TrimLeft( cnode.Text ) ;
          coordDim := 2 ;

          tkn := TGIS_Tokenizer.Create;
          try
            str := Copy( cstr, StringFirst, Pos( ' ', cstr )-StringFirst ) ;
            if IsStringEmpty( str ) then
              str := cstr ;
            tkn.Execute( str, [ ',' ] ) ;
            if tkn.Result.Count<2 then begin
              str := Copy( cstr, StringFirst, Pos( ',', cstr )-StringFirst ) ;
              if IsStringEmpty( str ) then
                str := cstr ;
              tkn.Execute( str, [ ' ' ] ) ;
            end ;

            if tkn.Result.Count > coordDim then
              coordDim := Min( tkn.Result.Count, 4 ) ;

            tkn.Execute( cstr, [ ' ', ',' ] );
            i := 0 ;

            while i < tkn.Result.Count - 1 do begin
              x := DotStrToFloat( tkn.Result[ i ] ) ;
              inc( i );
              y := DotStrToFloat( tkn.Result[ i ] ) ;
              inc( i );
              if coordDim > 2 then begin
                z := DotStrToFloat( tkn.Result[ i ] ) ;
                inc( i );
              end
              else
                z := 0 ;
              if coordDim > 3 then begin
                m := DotStrToFloat( tkn.Result[ i ] ) ;
                inc( i );
              end
              else
                m := 0 ;

              if coordDim > 2 then
                _shape.AddPoint3D( GisPoint3D( x, y, z, m ) )
              else
                _shape.AddPoint( GisPoint( x, y ) ) ;
            end ;
          finally
            FreeObject( tkn ) ;
          end ;
          Result := coordDim ;
          exit ;
        end ;

        cnode := _gnode.ChildNodes.FindNode('posList') ;
        if assigned( cnode ) then begin
          coordDim := 2 ;
          if cnode.HasAttribute( 'srsDimension' ) then
            coordDim := VarToInt32( cnode.Attributes['srsDimension'] )
          else if (gdim > 0) then
            coordDim := gdim ;

          cstr := cnode.Text ;
          tkn := TGIS_Tokenizer.Create ;
          try
            tkn.Execute( cstr, [ ' ', ',' ] );
            i := 0 ;

            while i < tkn.Result.Count - 1 do begin
              x := DotStrToFloat( tkn.Result[ i ] ) ;
              inc( i );
              y := DotStrToFloat( tkn.Result[ i ] ) ;
              inc( i );
              if coordDim > 2 then begin
                z := DotStrToFloat( tkn.Result[ i ] ) ;
                inc( i );
              end
              else
                z := 0 ;

              if coordDim > 3 then begin
                m := DotStrToFloat( tkn.Result[ i ] ) ;
                inc( i );
              end
              else
                m
                 := 0 ;
              if coordDim > 2 then
                _shape.AddPoint3D( GisPoint3D( x, y, z, m ) )
              else
                _shape.AddPoint( GisPoint( x, y ) ) ;
            end ;
          finally
            FreeObject( tkn ) ;
          end ;
          Result := coordDim ;
          exit ;
        end ;

        found := False ;
        for i := 0 to _gnode.ChildNodes.Count-1 do begin
          cnode := _gnode.ChildNodes[i] ;
          if cnode.LocalName <> 'pos' then continue ;
          cstr := cnode.Text ;

          tkn := TGIS_Tokenizer.Create ;
          try
            tkn.Execute( cstr, [ ' ', ',' ] );
            if tkn.Result.Count > 2 then begin
              x := DotStrToFloat( tkn.Result[ 0 ] ) ;
              y := DotStrToFloat( tkn.Result[ 1 ] ) ;
              z := DotStrToFloat( tkn.Result[ 2 ] ) ;
              if tkn.Result.Count > 3 then
                m := DotStrToFloat( tkn.Result[ 3 ] )
              else
                m := 0 ;

              _shape.AddPoint3D( GisPoint3D( x, y, z, m ) ) ;
              coordDim := Min( tkn.Result.Count, 4 ) ;
            end
            else if tkn.Result.Count > 1 then begin
              x := DotStrToFloat( tkn.Result[ 0 ] ) ;
              y := DotStrToFloat( tkn.Result[ 1 ] ) ;
              _shape.AddPoint( GisPoint( x, y ) ) ;
              coordDim := 2 ;
            end
            else
              assert( tkn.Result.Count > 1, 'Not enough tuple values.' ) ;

            found := True ;
          finally
            FreeObject( tkn ) ;
          end ;
          Result := coordDim ;
        end ;
        if found then exit ;

        for i := 0 to _gnode.ChildNodes.Count-1 do begin
          cnode := _gnode.ChildNodes[i] ;
          if cnode.LocalName <> 'coord' then continue ;

          xnode := cnode.ChildNodes.FindNode('X') ;
          ynode := cnode.ChildNodes.FindNode('Y') ;
          znode := cnode.ChildNodes.FindNode('Z') ;

          if assigned( xnode ) and assigned( ynode ) then begin
            x := DotStrToFloat( xnode.Text ) ;
            y := DotStrToFloat( ynode.Text ) ;
            if assigned( znode ) then begin
              z := DotStrToFloat( znode.Text ) ;
              _shape.AddPoint3D( GisPoint3D( x, y, z ) ) ;
              coordDim := 3 ;
            end
            else begin
              _shape.AddPoint( GisPoint( x, y ) ) ;
              coordDim := 2 ;
            end ;
          end ;
        end ;
        Result := coordDim ;
      end ;

      procedure makeArc( const _shp : TGIS_Shape ) ;
      var
        radius, start, stop : Double ;
        center              : TGIS_Point ;
      begin
        assert( _shp.GetNumPoints=3 ) ;

        if GisArcFrom3Points( _shp.GetPoint(0,0),
                              _shp.GetPoint(0,1),
                              _shp.GetPoint(0,2),
                              center, radius, start, stop
                             ) then begin
          _shp.Reset ;
          _shp.StrokeArc( GisPoint3DFrom2D(center), radius, radius, start, stop, 0, 45 )
        end ;
      end ;

      procedure makeBox( const _shp : TGIS_Shape ) ;
      var
        p0, p1 : TGIS_Point3D ;
      begin
        assert( _shp.GetNumPoints>=2 ) ;
        p0 := _shp.GetPoint3D(0,0) ;
        p1 := _shp.GetPoint3D(0,1) ;

        _shp.Reset ;
        _shp.AddPart ;
        _shp.AddPoint3D( GisPoint3D( p0.X, p0.Y, p0.Z ) ) ;
        _shp.AddPoint3D( GisPoint3D( p1.X, p0.Y, p0.Z ) ) ;
        _shp.AddPoint3D( GisPoint3D( p1.X, p1.Y, p1.Z ) ) ;
        _shp.AddPoint3D( GisPoint3D( p0.X, p1.Y, p0.Z ) ) ;
        _shp.AddPoint3D( GisPoint3D( p0.X, p0.Y, p0.Z ) ) ;
      end ;

      function getDim( const _dim : Integer ) : TGIS_DimensionType ;
      begin
        case _dim of
          2 : Result := TGIS_DimensionType.XY   ;
          3 : Result := TGIS_DimensionType.XYZ  ;
          4 : Result := TGIS_DimensionType.XYZM ;
        else  Result := TGIS_DimensionType.XY   ;
        end ;
      end ;

      procedure append_geometry_noparts(
        const _shapeA  : TGIS_Shape ;
        const _shapeB  : TGIS_Shape
      ) ;
      var
        part_no  : Integer ;
        point_no : Integer ;
      begin
        if not assigned( _shapeB ) then exit ;

        _shapeA.Lock( TGIS_Lock.Extent );
        try
          for part_no := 0 to _shapeB.GetNumParts - 1 do begin
            _shapeA.SetPartType( _shapeA.GetNumParts-1, _shapeB.GetPartType( part_no ) ) ;
            for point_no := 0 to _shapeB.GetPartSize( part_no ) - 1 do
              _shapeA.AddPoint3D( _shapeB.GetPoint3D( part_no, point_no) ) ;
          end ;
        finally
          _shapeA.Unlock ;
        end ;
      end ;

      function gml2shape( const _node : IXMLNode ) : TGIS_Shape ;
      var
        baseGeom : String ;
        inode    : IXMLNode ;
        jnode    : IXMLNode ;
        cnode    : IXMLNode ;
        nodelist : IXMLNodeList ;
        i, j     : Integer ;
        ishp     : TGIS_Shape ;
        oshp     : TGIS_Shape ;
      begin
        Result := nil ;
        if not assigned( _node ) then exit ;

        oshp := nil ;
        baseGeom := _node.LocalName ;
        if _node.HasAttribute( 'srsDimension' ) then
          gdim := VarToInt32( _node.Attributes['srsDimension'] ) ;
        try
          if ( baseGeom = 'Polygon'      ) or
             ( baseGeom = 'PolygonPatch' ) or
             ( baseGeom = 'Triangle'     ) or
             ( baseGeom = 'Rectangle'    ) then begin
               oshp := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 oshp.AddPart ;

                 inode := _node.ChildNodes.FindNode('outerBoundaryIs') ;
                 if not assigned( inode ) then
                   inode := _node.ChildNodes.FindNode('exterior') ;
                 assert( inode <> nil, 'Missing outerBoundaryIs property' ) ;

                 cnode := inode.ChildNodes.FindNode( 'LinearRing' ) ;
                 if assigned( cnode ) then
                   ldim := parseGMLCoords( cnode, oshp )
                 else begin
                   ishp := gml2shape( inode.ChildNodes.First ) ;
                   if assigned( ishp ) then begin
                     oshp.AppendGeometry( ishp ) ;
                     FreeObject( ishp ) ;
                   end ;
                 end ;

                 for i := 0 to _node.ChildNodes.Count-1 do begin
                   inode := _node.ChildNodes[i] ;
                   if ( inode.LocalName = 'innerBoundaryIs' ) or
                      ( inode.LocalName = 'interior' ) then begin
                      oshp.AddPart ;
                      cnode := inode.ChildNodes.FindNode( 'LinearRing' ) ;
                      if assigned( cnode ) then
                        ldim := parseGMLCoords( cnode, oshp )
                      else begin
                        ishp := gml2shape( inode.ChildNodes.First ) ;
                        if assigned( ishp ) then begin
                          oshp.AppendGeometry( ishp ) ;
                          FreeObject( ishp ) ;
                        end ;
                      end ;
                   end ;
                 end ;

               oshp.Unlock ;
               exit ;
          end ;

          if ( baseGeom = 'LineString'        ) or
             ( baseGeom = 'LineStringSegment' ) then begin
               oshp := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 oshp.AddPart ;
                 ldim := parseGMLCoords( _node, oshp ) ;
               oshp.Unlock ;
               exit ;
          end ;

          if ( baseGeom = 'Arc'    ) or
             ( baseGeom = 'Circle' ) then begin
               oshp := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 oshp.AddPart ;
                 ldim := parseGMLCoords( _node, oshp ) ;
                 makeArc( oshp ) ;
               oshp.Unlock ;
               exit ;
          end ;

          if ( baseGeom = 'PointType'       ) or
             ( baseGeom = 'Point'           ) or
             ( baseGeom = 'ConnectionPoint' ) then begin
               oshp := TGIS_ShapePoint.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 oshp.AddPart ;
                 ldim := parseGMLCoords( _node, oshp ) ;

               oshp.Unlock ;

               exit ;
          end ;

          if ( baseGeom = 'BoxType' ) or
             ( baseGeom = 'Box'     ) then begin
               oshp := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 oshp.AddPart ;
                 ldim := parseGMLCoords( _node, oshp ) ;
                 makeBox( oshp ) ;
               oshp.Unlock ;
               exit ;
          end ;

          if ( baseGeom = 'MultiPolygon'     ) or
             ( baseGeom = 'MultiSurface'     ) or
             ( baseGeom = 'CompositeSurface' ) then begin
               oshp := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 for i := 0 to _node.ChildNodes.Count-1 do begin
                   inode := _node.ChildNodes[i] ;
                   if ( inode.LocalName = 'polygonMember'  ) or
                      ( inode.LocalName = 'surfaceMember'  ) or
                      ( inode.LocalName = 'surfaceMembers' ) then begin
                     j := 0  ;
                     nodelist := inode.ChildNodes ;
                     while assigned( nodelist ) and (j < nodelist.Count) do begin
                       jnode := nodelist[j] ;
                       ishp := gml2shape( jnode ) ;
                       if assigned( ishp ) then begin
                         oshp.AppendGeometry( ishp ) ;
                         FreeObject( ishp ) ;
                         inc( j ) ;
                       end
                       else begin
                        if assigned( jnode ) then
                          nodelist := jnode.ChildNodes
                        else
                          nodelist := nil ;
                         j := 0 ;
                       end ;
                     end ;
                   end ;
                 end ;
               oshp.Unlock ;
               exit ;
          end ;

          if ( baseGeom = 'MultiPoint' ) then begin
               oshp := TGIS_ShapeMultiPoint.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 for i := 0 to _node.ChildNodes.Count-1 do begin
                   inode := _node.ChildNodes[i] ;
                   if ( inode.LocalName = 'pointMember' ) then begin
                      oshp.AddPart ;
                      cnode := inode.ChildNodes.FindNode( 'Point' ) ;
                      ldim := parseGMLCoords( cnode, oshp ) ;
                   end
                   else if ( inode.LocalName = 'pointMembers' ) then begin
                     for j := 0 to inode.ChildNodes.Count-1 do begin
                       jnode := inode.ChildNodes[j] ;
                       oshp.AddPart ;
                       if jnode.LocalName = 'Point' then
                         ldim := parseGMLCoords( jnode, oshp ) ;
                     end ;
                   end ;
                 end ;
               oshp.Unlock ;
               exit ;
          end ;

          if ( baseGeom = 'MultiLineString' ) then begin
               oshp := TGIS_ShapeArc.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 for i := 0 to _node.ChildNodes.Count-1 do begin
                   inode := _node.ChildNodes[i] ;
                   if ( inode.LocalName = 'lineStringMember' ) then begin
                     for j := 0 to inode.ChildNodes.Count-1 do begin
                       jnode := inode.ChildNodes[j] ;
                       oshp.AddPart ;
                       if jnode.LocalName = 'LineString' then
                        ldim := parseGMLCoords( jnode, oshp ) ;
                     end ;
                   end ;
                 end ;
               oshp.Unlock ;
               exit ;
          end ;

          if ( baseGeom = 'MultiCurve' ) then begin
             oshp := TGIS_ShapeArc.Create(
                         _source, _ptr, _mapped, _uid, _layer,
                         TGIS_DimensionType.XYZM
                       ) ;
             oshp.Lock( TGIS_Lock.Internal ) ;
               oshp.AddPart ;
               for i := 0 to _node.ChildNodes.Count-1 do begin
                 inode := _node.ChildNodes[i] ;
                 if ( inode.LocalName = 'curveMember'  ) or
                    ( inode.LocalName = 'curveMembers' ) then begin
                   for j := 0 to inode.ChildNodes.Count-1 do begin
                     jnode := inode.ChildNodes[j] ;
                     ishp := gml2shape( jnode ) ;
                     if assigned( ishp ) then begin
                       oshp.AppendGeometry( ishp ) ;
                       FreeObject( ishp ) ;
                     end ;
                   end ;
                 end ;
               end ;
             oshp.Unlock ;
             exit ;
          end ;

          if ( baseGeom = 'Curve' ) then begin
             oshp := TGIS_ShapeArc.Create(
                         _source, _ptr, _mapped, _uid, _layer,
                         TGIS_DimensionType.XYZM
                       ) ;
             oshp.Lock( TGIS_Lock.Internal ) ;
               oshp.AddPart ;
               inode := _node.ChildNodes.FindNode('segments') ;
               if assigned( inode ) then begin
                 for j := 0 to inode.ChildNodes.Count-1 do begin
                   jnode := inode.ChildNodes[j] ;
                   if ( jnode.LocalName = 'LineStringSegment' ) or
                      ( jnode.LocalName = 'Arc'               ) or
                      ( jnode.LocalName = 'Circle'            ) then begin
                     ishp := gml2shape( jnode ) ;
                     if assigned( ishp ) then begin
                       append_geometry_noparts( oshp, ishp ) ;
                       FreeObject( ishp ) ;
                     end ;
                   end ;
                 end ;
               end ;
             oshp.Unlock ;
             exit ;
          end ;

          if ( baseGeom = 'MultiGeometry'       ) or
             ( baseGeom = 'GeometryCollection'  ) then begin
               oshp := TGIS_ShapeComplex.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 for i := 0 to _node.ChildNodes.Count-1 do begin
                   inode := _node.ChildNodes[i] ;
                   if ( inode.LocalName = 'geometryMember' ) then begin
                     for j := 0 to inode.ChildNodes.Count-1 do begin
                       jnode := inode.ChildNodes[j] ;
                       ishp := gml2shape( jnode ) ;
                       if assigned( ishp ) then
                         TGIS_ShapeComplex( oshp ).AddShape( ishp ) ;
                     end ;
                   end ;
                 end ;
               oshp.Unlock ;
               exit ;
          end ;

          if ( baseGeom = 'Ring' ) then begin
             oshp := TGIS_ShapeArc.Create(
                         _source, _ptr, _mapped, _uid, _layer,
                         TGIS_DimensionType.XYZM
                       ) ;
             oshp.Lock( TGIS_Lock.Internal ) ;
               oshp.AddPart ;
               for i := 0 to _node.ChildNodes.Count-1 do begin
                 inode := _node.ChildNodes[i] ;
                 if assigned( inode ) and ( inode.LocalName = 'curveMember' ) then begin
                   for j := 0 to inode.ChildNodes.Count-1 do begin
                     jnode := inode.ChildNodes[j] ;
                     ishp := gml2shape( jnode ) ;
                     if assigned( ishp ) then begin
                       append_geometry_noparts( oshp, ishp ) ;
                       FreeObject( ishp ) ;
                     end ;
                   end ;
                 end ;
               end ;
             oshp.Unlock ;
             exit ;
          end ;

          if ( baseGeom = 'directedEdge' ) then begin
             oshp := TGIS_ShapeArc.Create(
                         _source, _ptr, _mapped, _uid, _layer,
                         TGIS_DimensionType.XYZM
                       ) ;
             oshp.Lock( TGIS_Lock.Internal ) ;
               oshp.AddPart ;
               inode := _node.ChildNodes.FindNode('Edge') ;
               if assigned( inode ) then begin
                 jnode := inode.ChildNodes.FindNode('curveProperty') ;
                 if assigned( jnode ) then begin
                   cnode := jnode.ChildNodes.FindNode('LineString') ;
                   if not assigned( cnode ) then
                     cnode := jnode.ChildNodes.FindNode('Curve') ;

                   ishp := gml2shape( cnode ) ;
                   if assigned( ishp ) then begin
                     oshp.AppendGeometry( ishp ) ;
                     FreeObject( ishp ) ;
                   end ;
                 end;
               end ;
             oshp.Unlock ;
             exit ;
          end ;

          if ( baseGeom = 'TopoCurve' ) then begin
             oshp := TGIS_ShapeArc.Create(
                         _source, _ptr, _mapped, _uid, _layer,
                         TGIS_DimensionType.XYZM
                       ) ;
             oshp.Lock( TGIS_Lock.Internal ) ;

             for j := 0 to _node.ChildNodes.Count-1 do begin
               jnode := _node.ChildNodes[j] ;
               if ( jnode.LocalName = 'directedEdge' ) then begin
                 ishp := gml2shape( jnode ) ;
                 if assigned( ishp ) then begin
                   oshp.AppendGeometry( ishp ) ;
                   FreeObject( ishp ) ;
                 end ;
               end ;
             end ;
             oshp.Unlock ;
             exit ;
          end ;

          if ( baseGeom = 'TopoSurface' ) then begin
            // TODO
          end ;

          if ( baseGeom = 'Surface' ) then begin
             oshp := TGIS_ShapePolygon.Create(
                         _source, _ptr, _mapped, _uid, _layer,
                         TGIS_DimensionType.XYZM
                       ) ;
             oshp.Lock( TGIS_Lock.Internal ) ;

               inode := _node.ChildNodes.FindNode('patches') ;
               if not assigned( inode ) then
                 inode := _node.ChildNodes.FindNode('polygonPatches') ;
               if not assigned( inode ) then
                 inode := _node.ChildNodes.FindNode('trianglePatches') ;
               assert( inode <> nil ) ;
               if assigned( inode ) then begin
                 for j := 0 to inode.ChildNodes.Count-1 do begin
                   jnode := inode.ChildNodes[j] ;
                   if ( jnode.LocalName = 'PolygonPatch' ) or
                      ( jnode.LocalName = 'Triangle'     ) or
                      ( jnode.LocalName = 'Rectangle'    ) then begin
                     ishp := gml2shape( jnode ) ;
                     if assigned( ishp ) then begin
                       oshp.AppendGeometry( ishp ) ;
                       FreeObject( ishp ) ;
                     end ;
                   end ;
                 end ;
               end ;
             oshp.Unlock ;
             exit ;
          end ;

          if ( baseGeom = 'TriangulatedSurface' ) or
             ( baseGeom = 'Tin'                 ) then begin
             oshp := TGIS_ShapePolygon.Create(
                         _source, _ptr, _mapped, _uid, _layer,
                         TGIS_DimensionType.XYZM
                       ) ;
             oshp.Lock( TGIS_Lock.Internal ) ;

               inode := _node.ChildNodes.FindNode('trianglePatches') ;
               if not assigned( inode ) then
                 inode := _node.ChildNodes.FindNode('patches') ;
               assert( inode <> nil ) ;
               if assigned( inode ) then begin
                 for j := 0 to inode.ChildNodes.Count-1 do begin
                   jnode := inode.ChildNodes[j] ;
                   if ( jnode.LocalName = 'Triangle' ) then begin
                     ishp := gml2shape( jnode ) ;
                     if assigned( ishp ) then begin
                       oshp.AppendGeometry( ishp ) ;
                       FreeObject( ishp ) ;
                     end ;
                   end ;
                 end ;
               end ;
             oshp.Unlock ;
             exit ;
          end ;

          if ( baseGeom = 'Solid' ) then begin
             oshp := TGIS_ShapePolygon.Create(
                         _source, _ptr, _mapped, _uid, _layer,
                         TGIS_DimensionType.XYZM
                       ) ;
             oshp.Lock( TGIS_Lock.Internal ) ;
               inode := _node.ChildNodes.FindNode('exterior') ;
               if assigned( inode ) then begin
                 for j := 0 to inode.ChildNodes.Count-1 do begin
                   jnode := inode.ChildNodes[j] ;
                   ishp := gml2shape( jnode ) ;
                   if assigned( ishp ) then begin
                     oshp.AppendGeometry( ishp ) ;
                     FreeObject( ishp ) ;
                   end ;
                 end ;
               end ;
             oshp.Unlock ;
             exit ;

          end ;

          if ( baseGeom = 'OrientableSurface' ) then begin
             oshp := TGIS_ShapePolygon.Create(
                         _source, _ptr, _mapped, _uid, _layer,
                         TGIS_DimensionType.XYZM
                       ) ;
             oshp.Lock( TGIS_Lock.Internal ) ;
               inode := _node.ChildNodes.FindNode('baseSurface') ;
               if assigned( inode ) then begin
                 for j := 0 to inode.ChildNodes.Count-1 do begin
                   jnode := inode.ChildNodes[j] ;
                   ishp := gml2shape( jnode ) ;
                   if assigned( ishp ) then begin
                     oshp.AppendGeometry( ishp ) ;
                     FreeObject( ishp ) ;
                   end ;
                 end ;
               end ;
             oshp.Unlock ;
             exit ;
          end ;

          if ( baseGeom = 'SimplePolygon'   ) or
             ( baseGeom = 'SimpleRectangle' ) or
             ( baseGeom = 'SimpleTriangle'  ) then begin
               oshp := TGIS_ShapePolygon.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                 oshp.AddPart ;
                 ldim := parseGMLCoords( _node, oshp ) ;
               oshp.Unlock ;
               exit ;
          end ;

          if ( baseGeom = 'SimpleMultiPoint' ) then begin
               oshp := TGIS_ShapeMultiPoint.Create(
                           _source, _ptr, _mapped, _uid, _layer,
                           TGIS_DimensionType.XYZM
                         ) ;
               oshp.Lock( TGIS_Lock.Internal ) ;
                oshp.AddPart ;
                ldim := parseGMLCoords( _node, oshp ) ;
               oshp.Unlock ;
               exit ;
          end ;
        finally
          if assigned( oshp ) then begin
            case oshp.ShapeType of
              TGIS_ShapeType.Point :
                 Result := TGIS_ShapePoint.Create( _source, _ptr, _mapped, _uid,
                                                   _layer, getDim( ldim )
                                                  ) ;
              TGIS_ShapeType.MultiPoint :
                 Result := TGIS_ShapeMultiPoint.Create( _source, _ptr, _mapped,
                                                        _uid, _layer, getDim( ldim )
                                                       ) ;
              TGIS_ShapeType.Arc :
                 Result := TGIS_ShapeArc.Create( _source, _ptr, _mapped, _uid,
                                                 _layer, getDim( ldim )
                                                ) ;
              TGIS_ShapeType.Polygon :
                 Result := TGIS_ShapePolygon.Create( _source, _ptr, _mapped,
                                                     _uid, _layer, getDim( ldim )
                                                    ) ;
              TGIS_ShapeType.Complex :
                 Result := TGIS_ShapeComplex.Create( _source, _ptr, _mapped,
                                                     _uid, _layer, getDim( ldim )
                                                    ) ;
              TGIS_ShapeType.MultiPatch :
                 Result := TGIS_ShapeMultiPatch.Create( _source, _ptr, _mapped,
                                                     _uid, _layer, getDim( ldim )
                                                    ) ;
            end ;

            Result.CopyGeometry( oshp ) ;
            FreeObject( oshp ) ;
          end ;
        end ;
      end ;

  const
    XML_TPL = '<ttkGML xmlns:gml="http://www.opengis.net/gml">%s</ttkGML>' ;
  var
    xDoc : IXMLDocument ;
  begin
    xDoc := TGIS_XMLDocument.Create ;
    try
      try
        xDoc.LoadFromXML( Format( XML_TPL, [ _gml ] ) ) ;
        gdim := 0 ;
        ldim := 0 ;

        Result := gml2shape( xDoc.DocumentElement.ChildNodes.First ) ;
      except
        on e : Exception do
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDGML ), '', 0, e ) ;
      end ;
    finally
      FreeObject( xDoc ) ;
    end ;
  end ;

  type
    T_curve = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      startPoint : Integer ;
      segmentType: Integer ;
      v1         : Double ;
      v2         : Double ;
      v3         : Double ;
      v4         : Double ;
      v5         : Double ;
      bits       : Integer ;
    end ;

  class function TGIS_GeometryFactory.GisCreateShapeFromShapeEx(
    const _ptr   : TGIS_Bytes
  ) : TGIS_Shape ;
  const
    shapeNull               =  0;
    shapePoint              =  1;
    shapePointM             = 21;
    shapePointZM            = 11;
    shapePointZ             =  9;
    shapeMultipoint         =  8;
    shapeMultipointM        = 28;
    shapeMultipointZM       = 18;
    shapeMultipointZ        = 20;
    shapePolyline           =  3;
    shapePolylineM          = 23;
    shapePolylineZM         = 13;
    shapePolylineZ          = 10;
    shapePolygon            =  5;
    shapePolygonM           = 25;
    shapePolygonZM          = 15;
    shapePolygonZ           = 19;
    shapeMultiPatchM        = 31;
    shapeMultiPatch         = 32;
    shapeGeneralPolyline    = 50;
    shapeGeneralPolygon     = 51;
    shapeGeneralPoint       = 52;
    shapeGeneralMultipoint  = 53;
    shapeGeneralMultiPatch  = 54;

    shapeHasZs                  = $80000000;
    shapeHasMs                  = 1073741824;
    shapeHasCurves              = 536870912;
    shapeHasIDs                 = 268435456;
    shapeHasNormals             = 134217728;
    shapeHasTextures            = 67108864;
    shapeHasPartIDs             = 33554432;
    shapeHasMaterials           = 16777216;
    shapeIsCompressed           = 8388608;
    shapeModifierMask           = -16777216;
    shapeMultiPatchModifierMask = 15728640;
    shapeBasicTypeMask          = 255;
    shapeBasicModifierMask      = -1073741824;
    shapeNonBasicModifierMask   = 1056964608;
    shapeExtendedModifierMask   = -587202560;

    geometryNull            = 0;
    geometryPoint           = 1;
    geometryMultipoint      = 2;
    geometryPolyline        = 3;
    geometryPolygon         = 4;
    geometryMultiPatch      = 9;

    curveTypeCircularArc    = 1;
    curveTypeBezier         = 4;
    curveTypeEllipticArc    = 5;

  var
    shapetype  : Cardinal ;
    x, y, z, m : Double ;
    hasZ, hasM : Boolean ;
    hasCurves  : Boolean ;
    hasIDs     : Boolean ;
    hasNormals : Boolean ;
    hasTextures: Boolean ;
    hasMaterials: Boolean ;

    numpoints  : Integer ;
    numparts   : Integer ;
    numcurves  : Integer ;
    numids     : Integer ;
    numms      : Integer ;
    numnormals : Integer ;
    numtext    : Integer ;
    nummat     : Integer ;
    textdim    : Integer ;
    textcomprt : Integer ;
    matsize    : Integer ;
    partType   : Integer ;
    i, j, c    : Integer ;
    offZ, offM : Integer ;
    offPoints  : Integer ;
    offParts   : Integer ;
    offPartDesc: Integer ;
    offCurves  : Integer ;
    offIDs     : Integer ;
    offNormals : Integer ;
    offTextCoord: Integer ;
    offTextParts  : Integer ;
    offMaterials: Integer ;
    offMaterialb: Integer ;
    offset     : Integer ;
    cnt        : Integer ;
    oldPoint   : TGIS_Point ;
    ptn        : Integer ;
    curves     : array of T_curve ;
    useCurve   : Boolean ;
    radius     : Double ;
    start      : Double ;
    stop       : Double ;
    center     : TGIS_Point ;
    a          : Double ;
    arpt       : array of TGIS_Point ;
    material   : TGIS_Material ;
    isccw      : Boolean ;
    isline     : Boolean ;
    ispoint    : Boolean ;
    isempty    : Boolean ;
    isip       : Boolean ;

    function hasflag( const _flag : Cardinal ) : Boolean ;
    begin
      Result := (shapetype and _flag ) = _flag ;
    end ;

    function getDim : TGIS_DimensionType ;
    begin
      if not hasZ and not hasM then
        Result := TGIS_DimensionType.XY
      else if hasZ and hasM then
        Result := TGIS_DimensionType.XYZM
      else if not hasZ and hasM then
        Result := TGIS_DimensionType.XYM
      else if hasZ and not hasM then
        Result := TGIS_DimensionType.XYZ
      else
        Result := TGIS_DimensionType.XY ;
    end ;

    function factoral( const _value : Integer ) : Double ;
    var
      t : Integer ;
    begin
          if _value = 0 then begin
            Result := 1 ;
            exit ;
          end ;

      Result := _value ;
      t := _value-1 ;
      while (t>0) do begin
        Result := Result * t ;
        t := t-1 ;
      end ;
    end ;

    function bezierPoint( const _pos      : Double ;
                          const _points : array of TGIS_Point
                         ) : TGIS_Point ;
    var
      xx,yy, factn : Double ;
      n,ll  : Integer ;
      b, w : Double ;
    begin
      xx := 0 ;
      yy := 0 ;
      n  := length( _points )-1 ;
      factn := factoral(n) ;

      for ll := 0 to n do begin
        b := factn/(factoral(ll)*factoral(n-ll));
        w := Power(1-_pos, n-ll)*Power(_pos, ll);
        xx := xx + b*w*_points[ll].X ;
        yy := yy + b*w*_points[ll].Y ;
      end ;
      Result := GisPoint(xx, yy) ;
    end ;

    procedure buildArcFromTwoPoints(
      const _shp    : TGIS_Shape ;
      const _ptg1   : TGIS_Point ;
      const _ptg2   : TGIS_Point ;
      const _center : TGIS_Point ;
      const _ccw    : Boolean ;
      const _r      : Double
     ) ;
    var
      sa, ea : Double ;
    begin
      if _ccw then begin
        sa := -1*ArcTan2( _ptg1.Y - _center.Y, _ptg1.X - _center.X ) ;
        ea := -1*ArcTan2( _ptg2.Y - _center.Y, _ptg2.X - _center.X ) ;

        if sa < 0 then
          sa := sa + 2*Pi ;
        // CCW
        _shp.StrokeArc( GisPoint3DFrom2D(_center), _r, _r, sa, ea, 0 ) ;
      end
      else begin
        sa := -1*ArcTan2( _ptg1.Y - _center.Y, _ptg1.X - _center.X ) ;
        ea := -1*ArcTan2( _ptg2.Y - _center.Y, _ptg2.X - _center.X ) ;

        if ea < 0 then
          ea := ea + 2*Pi ;
        // CW
        _shp.StrokeArc( GisPoint3DFrom2D(_center), _r, _r, sa, ea, 0 ) ;
      end ;
    end ;

  begin
    Result := nil ;

    if not assigned( _ptr ) then exit ;

    shapetype := _ptr.ReadUInt32( 0 ) ;

    if shapetype = shapeNull then exit ;

    hasZ := hasflag( shapeHasZs ) ;
    hasM := hasflag( shapeHasMs ) ;

    if      ( shapetype = shapePoint   ) or
            ( shapetype = shapePointM  ) or
            ( shapetype = shapePointZM ) or
            ( shapetype = shapePointZ  ) or
            ( (shapetype and shapeBasicTypeMask) = shapeGeneralPoint ) then
    begin
      hasZ := hasZ or ( shapetype = shapePointZM ) or
                      ( shapetype = shapePointZ  ) ;
      hasM := hasM or ( shapetype = shapePointM  ) or
                      ( shapetype = shapePointZM ) ;

      Result := TGIS_ShapePoint.Create( getDim ) ;
      Result.Lock( TGIS_Lock.Internal ) ;
      Result.AddPart ;

        x := _ptr.ReadDouble( 4  ) ;
        y := _ptr.ReadDouble( 12 ) ;

        if hasZ then
          z := _ptr.ReadDouble( 20 )
        else
          z := 0 ;

        if hasM then begin
          if hasZ then
            m := _ptr.ReadDouble( 28 )
          else
            m := _ptr.ReadDouble( 20 )
        end
        else
          m := 0 ;

        if IsNan(m) then
          m := 0 ;

      if hasZ or hasM then
        Result.AddPoint3D( GisPoint3D( x, y, z, m ) )
      else
        Result.AddPoint( GisPoint( x, y ) ) ;

      Result.Unlock ;
    end
    else if ( shapetype = shapeMultipoint   ) or
            ( shapetype = shapeMultipointM  ) or
            ( shapetype = shapeMultipointZM ) or
            ( shapetype = shapeMultipointZ  ) or
            ( (shapetype and shapeBasicTypeMask) = shapeGeneralMultipoint ) then
    begin
      hasZ := hasZ or ( shapetype = shapeMultipointZM ) or
                      ( shapetype = shapeMultipointZ  ) ;
      hasM := hasM or ( shapetype = shapeMultipointM  ) or
                      ( shapetype = shapeMultipointZM ) ;

      Result := TGIS_ShapeMultiPoint.Create( getDim ) ;
      Result.Lock( TGIS_Lock.Internal ) ;
      Result.AddPart ;

        numpoints := _ptr.ReadInt32( 36 ) ;
        offZ   := 40+16*numpoints+16 ;
        if hasZ then
          offM := offZ+8*numpoints+16
        else
          offM := 40+16*numpoints+16 ;

        for i := 0 to numpoints-1 do begin

          x := _ptr.ReadDouble( 40+i*16   ) ;
          y := _ptr.ReadDouble( 40+i*16+8 ) ;

          if hasZ then
            z := _ptr.ReadDouble( offZ+i*8 )
          else
            z := 0 ;

          if hasM then
            m := _ptr.ReadDouble( offM+i*8 )
          else
            m := 0 ;

          if IsNan(m) then
            m := 0 ;

          if hasZ or hasM then
            Result.AddPoint3D( GisPoint3D( x, y, z, m ) )
          else
            Result.AddPoint( GisPoint( x, y ) ) ;
        end ;

      Result.Unlock ;
    end
    else if ( shapetype = shapePolyline   ) or
            ( shapetype = shapePolylineM  ) or
            ( shapetype = shapePolylineZM ) or
            ( shapetype = shapePolylineZ  ) or
            ( (shapetype and shapeBasicTypeMask) = shapeGeneralPolyline ) or

            ( shapetype = shapePolygon   ) or
            ( shapetype = shapePolygonM  ) or
            ( shapetype = shapePolygonZM ) or
            ( shapetype = shapePolygonZ  ) or
            ( (shapetype and shapeBasicTypeMask) = shapeGeneralPolygon ) then
    begin
      hasZ := hasZ or ( shapetype = shapePolylineZM ) or
                      ( shapetype = shapePolylineZ  ) or
                      ( shapetype = shapePolygonZM  ) or
                      ( shapetype = shapePolygonZ   ) ;
      hasM := hasM or ( shapetype = shapePolylineM  ) or
                      ( shapetype = shapePolylineZM ) or
                      ( shapetype = shapePolygonM   ) or
                      ( shapetype = shapePolygonZM  ) ;
      hasCurves := hasflag( shapeHasCurves ) ;

      if ( shapetype = shapePolyline   ) or
         ( shapetype = shapePolylineM  ) or
         ( shapetype = shapePolylineZM ) or
         ( shapetype = shapePolylineZ  ) or
         ( (shapetype and shapeBasicTypeMask) = shapeGeneralPolyline ) then
        Result := TGIS_ShapeArc.Create( getDim )
      else
        Result := TGIS_ShapePolygon.Create( getDim ) ;

      Result.Lock( TGIS_Lock.Internal ) ;

        numparts  := _ptr.ReadInt32( 36 ) ;
        numpoints := _ptr.ReadInt32( 40 ) ;
        offPoints := 44+4*numparts ;
        offset    := offPoints+16*numpoints;

        if hasZ then begin
          offZ   := offset+16 ;
          offset := offset+16 + 8*numpoints ;
        end
        else
          offZ := 0 ;

        if hasM then begin
          offM   := offset+16 ;
          offset := offset+16 + 8*numpoints ;
        end
        else
          offM := 0 ;

        if hasCurves then begin
          numcurves := _ptr.ReadInt32( offset ) ;
          offset    := offset+4 ;
          offCurves := offset ;
        end
        else  begin
          offCurves := 0 ;
          numcurves := 0 ;
        end ;

        if hasCurves then begin
          SetLength( curves, numcurves ) ;

          for i := 0 to numcurves-1 do begin
            {$IFDEF GIS_NORECORDS}
               curves[i] := new T_curve ;
            {$ENDIF}
            curves[i].startPoint  := _ptr.ReadInt32( offCurves ) ;
            inc( offCurves, 4 ) ;
            curves[i].segmentType := _ptr.ReadInt32( offCurves ) ;
            inc( offCurves, 4 ) ;

            case curves[i].segmentType of
              curveTypeCircularArc  :
                begin
                  curves[i].v1   := _ptr.ReadDouble( offCurves ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].v2   := _ptr.ReadDouble( offCurves ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].bits := _ptr.ReadInt32( offCurves ) ;
                  inc( offCurves, 4 ) ;
                end ;
              curveTypeBezier       :
                begin
                  curves[i].v1   := _ptr.ReadDouble( offCurves  ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].v2   := _ptr.ReadDouble( offCurves ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].v3   := _ptr.ReadDouble( offCurves ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].v4   := _ptr.ReadDouble( offCurves ) ;
                  inc( offCurves, 8 ) ;
                end ;
              curveTypeEllipticArc  :
                begin
                  curves[i].v1   := _ptr.ReadDouble( offCurves  ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].v2   := _ptr.ReadDouble( offCurves ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].v3   := _ptr.ReadDouble( offCurves ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].v4   := _ptr.ReadDouble( offCurves ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].v5   := _ptr.ReadDouble( offCurves ) ;
                  inc( offCurves, 8 ) ;
                  curves[i].bits := _ptr.ReadInt32( offCurves ) ;
                  inc( offCurves, 4 ) ;
                end ;
            end ;
          end ;

          {$IFDEF GIS_NORECORDS}
            oldPoint := new TGIS_Point ;
          {$ENDIF}

          ptn := 0 ;
          c   := 0 ;
          useCurve := False ;
          for i := 0 to numparts-1 do begin
            Result.AddPart ;
            if i = (numparts-1) then
              cnt := numpoints - _ptr.ReadInt32( 44+i*4 )
            else
              cnt := _ptr.ReadInt32( 44+(i+1)*4 )-_ptr.ReadInt32( 44+i*4 ) ;

            offParts := _ptr.ReadInt32( 44+i*4 ) ;
            for j := 0 to cnt-1 do begin
              x := _ptr.ReadDouble( offPoints+offParts*16+j*16   ) ;
              y := _ptr.ReadDouble( offPoints+offParts*16+j*16+8 ) ;

              if hasZ then
                z := _ptr.ReadDouble( offZ+offParts*8+j*8 )
              else
                z := 0 ;

              if hasM then
                m := _ptr.ReadDouble( offM+offParts*8+j*8 )
              else
                m := 0 ;

              if IsNan(m) then
                m := 0 ;

              if hasCurves and ( c < numcurves ) then begin
                if ptn = (curves[c].startPoint+1) then
                  useCurve := True ;
              end ;

              if useCurve then begin
                case curves[c].segmentType of
                  curveTypeCircularArc  :
                    begin
                      isempty := ( curves[c].bits and 1   ) = 1 ;
                      isccw   := ( curves[c].bits and 8   ) = 8 ;
                      isline  := ( curves[c].bits and 32  ) = 32 ;
                      ispoint := ( curves[c].bits and 64  ) = 64 ;
                      isip    := ( curves[c].bits and 128 ) = 128 ;

                      if isempty then begin  // arc is undefined
                        if hasZ or hasM then
                          Result.AddPoint3D( GisPoint3D( x, y, z, m ) )
                        else
                          Result.AddPoint( GisPoint( x, y ) ) ;
                      end
                      else if isline and not ispoint then begin // only SP and EP
                        if hasZ or hasM then
                          Result.AddPoint3D( GisPoint3D( x, y, z, m ) )
                        else
                          Result.AddPoint( GisPoint( x, y ) ) ;
                      end
                      else if ispoint then begin // SP=CP=EP, angles are stored
                        Result.StrokeArc( GisPoint3D( x, y, z, m ), radius, radius,
                                          curves[c].v1, curves[c].v2, 0, 45
                                         )
                      end
                      else if isip then begin // interior point, 3 point based
                        if GisArcFrom3Points( oldPoint,
                                              GisPoint( curves[c].v1, curves[c].v2 ),
                                              GisPoint( x, y ),
                                              center, radius, start, stop
                                             ) then
                          Result.StrokeArc( GisPoint3DFrom2D( center ), radius, radius,
                                            start, stop, 0, 45
                                           )
                        else begin
                          if hasZ or hasM then
                            Result.AddPoint3D( GisPoint3D( x, y, z, m ) )
                          else
                            Result.AddPoint( GisPoint( x, y ) ) ;
                        end ;
                      end
                      else begin // 2 point based with CP
                        center := GisPoint( curves[c].v1, curves[c].v2 ) ;
                        radius := GisPoint2Point( oldPoint, center ) ;
                        buildArcFromTwoPoints( Result,
                                               oldPoint,
                                               GisPoint(x,y),
                                               GisPoint(curves[c].v1,curves[c].v2),
                                               isccw, radius
                                              ) ;
                      end ;
                    end ;
                  curveTypeBezier       :
                    begin
                      {$IFDEF GIS_NORECORDS}
                        arpt := new TGIS_Point[4] ;
                      {$ELSE}
                        SetLength( arpt, 4 ) ;
                      {$ENDIF}
                      arpt[0] := _TGIS_Point(oldPoint) ;
                      arpt[1] := GisPoint(curves[c].v1,curves[c].v2) ;
                      arpt[2] := GisPoint(curves[c].v3,curves[c].v4) ;
                      arpt[3] := GisPoint(x,y) ;
                      a := 0.1 ;
                      while a <= 1 do begin
                        Result.AddPoint( bezierPoint(a, arpt) ) ;
                        a := a + 0.1 ;
                      end ;
                    end ;
                  curveTypeEllipticArc  :
                    begin
                      a := -1*ArcTan2( oldPoint.Y - curves[c].v1, oldPoint.X - curves[c].v1 ) ;
                      Result.StrokeArc( GisPoint3DFrom2D( GisPoint(curves[c].v1,curves[c].v2) ),
                                        curves[c].v4, curves[c].v4 * curves[c].v5,
                                        a, a+2*Pi, -curves[c].v3, 90
                                       )
                    end ;
                end ;
                useCurve := False ;
                inc( c ) ;
              end
              else begin
                if hasZ or hasM then
                  Result.AddPoint3D( GisPoint3D( x, y, z, m ) )
                else
                  Result.AddPoint( GisPoint( x, y ) ) ;
              end ;

              oldPoint.X := x ;
              oldPoint.Y := y ;
              inc( ptn ) ;

            end ;
          end ;
        end
        else begin
          for i := 0 to numparts-1 do begin
            Result.AddPart ;
            if i = (numparts-1) then
              cnt := numpoints - _ptr.ReadInt32( 44+i*4 )
            else
              cnt := _ptr.ReadInt32( 44+(i+1)*4 )-_ptr.ReadInt32( 44+i*4 ) ;

            offParts := _ptr.ReadInt32( 44+i*4 ) ;
            for j := 0 to cnt-1 do begin
              x := _ptr.ReadDouble( offPoints+offParts*16+j*16   ) ;
              y := _ptr.ReadDouble( offPoints+offParts*16+j*16+8 ) ;

              if hasZ then
                z := _ptr.ReadDouble( offZ+offParts*8+j*8 )
              else
                z := 0 ;

              if hasM then
                m := _ptr.ReadDouble( offM+offParts*8+j*8 )
              else
                m := 0 ;

              if IsNan(m) then
                m := 0 ;

              if hasZ or hasM then
                Result.AddPoint3D( GisPoint3D( x, y, z, m ) )
              else
                Result.AddPoint( GisPoint( x, y ) ) ;
            end ;
          end ;
        end ;

      Result.Unlock ;
    end
    else if ( shapetype = shapeMultiPatch   ) or
            ( shapetype = shapeMultiPatchM  ) or
            ( (shapetype and shapeBasicTypeMask) = shapeGeneralMultiPatch ) then
    begin
      hasZ := hasZ or ( shapetype = shapeMultiPatch   ) or
                      ( shapetype = shapeMultiPatchM  ) ;
      hasM := hasM or ( shapetype = shapeMultiPatchM  ) ;

      hasIDs        := hasflag( shapeHasIDs ) ;
      hasNormals    := hasflag( shapeHasNormals ) ;
      hasTextures   := hasflag( shapeHasTextures ) ;
      hasMaterials  := hasflag( shapeHasMaterials ) ;

      Result := TGIS_ShapeMultiPatch.Create( getDim ) ;
      Result.Lock( TGIS_Lock.Internal ) ;
        numparts    := _ptr.ReadInt32( 36 ) ;
        numpoints   := _ptr.ReadInt32( 40 ) ;
        offPartDesc := 44+4*numparts ;
        offPoints   := offPartDesc+4*numparts ;
        offset      := offPoints+16*numpoints;

        if hasZ then begin
          offZ   := offset+16 ;
          offset := offset+16 + 8*numpoints ;
        end
        else
          offZ := 0 ;

        numms := _ptr.ReadInt32( offset ) ;
        if hasM then begin
          offM   := offset+20 ;
          if offM >= _ptr.Size then begin
            offM := 0 ;
            offset := offset+4 ;
            hasM := False ; // fake M
          end
          else
            offset := offset+20 + 8*numpoints ;
        end
        else begin
          offM := 0 ;
          offset := offset+4 ;
        end ;

        numids := _ptr.ReadInt32( offset ) ;
        if hasIDs then begin
          offIDs := offset+4 ;
          offset := offset+4+4*numids ;
        end
        else begin
          offIDs := 0 ;
          offset := offset+4 ;
        end ;

        numnormals := _ptr.ReadInt32( offset ) ;
        if hasNormals then begin
          offNormals := offset+4 ;
          offset := offset+4+12*numnormals ;
        end
        else begin
          offNormals := 0 ;
          offset := offset+4 ;
        end ;

        numtext := _ptr.ReadInt32( offset ) ;
        if hasTextures then begin
          offset := offset+4 ;
          textdim := _ptr.ReadInt32( offset ) ;
          offset := offset+4 ;
          offTextParts := offset ;
          offTextCoord := offset+4*numparts ;
          offset := offTextCoord + 4*textdim*numtext ;
        end
        else begin
          offTextParts := 0 ;
          offTextCoord := 0 ;
          textdim := 0 ;
          offset := offset+4 ;
        end ;

        nummat := _ptr.ReadInt32( offset ) ;
        if hasMaterials then begin
          offset := offset+4 ;
          textcomprt := _ptr.ReadInt32( offset ) ;
          offset := offset+4 ;
          offMaterials := offset ;
          offset := offset+(nummat+1)*4 ;
          offMaterialb := offset ;
        end
        else begin
          offMaterials := 0 ;
          offMaterialb := 0 ;
          textcomprt := 0 ;
        end ;

        TGIS_ShapeMultiPatch(Result).HasPartDescriptors := True ;
        TGIS_ShapeMultiPatch(Result).PartDescriptors.Resize( numparts ) ;

        for i := 0 to numparts-1 do begin
          Result.AddPart ;
          if i = (numparts-1) then
            cnt := numpoints - _ptr.ReadInt32( 44+i*4 )
          else
            cnt := _ptr.ReadInt32( 44+(i+1)*4 )-_ptr.ReadInt32( 44+i*4 ) ;

          offParts := _ptr.ReadInt32( 44+i*4 ) ;
          partType := _ptr.ReadInt32( offPartDesc+i*4 ) ;
          Result.SetPartType( i, TGIS_PartType(partType and $F) ) ;

          TGIS_ShapeMultiPatch(Result).PartDescriptors.PartDescriptor[i] :=
            GisPartDescriptor(  partType         and $F  ,
                               (partType shr 4 ) and $3F ,
                               (partType shr 10) and $3F ,
                               (partType shr 16) and $FFFF
                              ) ;

          for j := 0 to cnt-1 do begin
            x := _ptr.ReadDouble( offPoints+offParts*16+j*16   ) ;
            y := _ptr.ReadDouble( offPoints+offParts*16+j*16+8 ) ;

            if hasZ then
              z := _ptr.ReadDouble( offZ+offParts*8+j*8 )
            else
              z := 0 ;

            if hasM then
              m := _ptr.ReadDouble( offM+offParts*8+j*8 )
            else
              m := 0 ;

            if IsNan(m) then
              m := 0 ;

            if hasZ or hasM then
              Result.AddPoint3D( GisPoint3D( x, y, z, m ) )
            else
              Result.AddPoint  ( GisPoint  ( x, y       ) ) ;
          end ;
        end ;

        if hasNormals then begin
          TGIS_ShapeMultiPatch(Result).HasNormals := True ;
          TGIS_ShapeMultiPatch(Result).Normals.Resize( numnormals ) ;
          for i := 0 to numnormals-1 do begin
            TGIS_ShapeMultiPatch(Result).Normals.Normal[i] :=
              GisSingleVector( _ptr.ReadSingle( offNormals + i*12   ),
                               _ptr.ReadSingle( offNormals + i*12+4 ),
                               _ptr.ReadSingle( offNormals + i*12+8 )
                              ) ;
          end ;
        end
        else
          TGIS_ShapeMultiPatch(Result).HasNormals := False ;

        if hasTextures then begin
          TGIS_ShapeMultiPatch(Result).HasTextures := True ;
          TGIS_ShapeMultiPatch(Result).Textures.Resize( numparts, numtext, textdim ) ;
          for i := 0 to numparts-1 do
            TGIS_ShapeMultiPatch(Result).Textures.SetPartOffset(i, _ptr.ReadInt32( offTextParts + i*4 ) ) ;
          for i := 0 to numtext*textdim-1 do
            TGIS_ShapeMultiPatch(Result).Textures.SetTextureCoord( i, _ptr.ReadSingle( offTextCoord + i*4 ) ) ;
        end
        else
          TGIS_ShapeMultiPatch(Result).HasTextures := False ;

        if hasMaterials then begin
          TGIS_ShapeMultiPatch(Result).HasMaterials := True ;
          TGIS_ShapeMultiPatch(Result).Materials.Resize( nummat ) ;
          case textcomprt of
            3 : TGIS_ShapeMultiPatch(Result).Materials.CompressionType := TGIS_CompressionType.JPEG ;
            4 : TGIS_ShapeMultiPatch(Result).Materials.CompressionType := TGIS_CompressionType.JPEGPlus
          else
            TGIS_ShapeMultiPatch(Result).Materials.CompressionType := TGIS_CompressionType.None ;
          end ;

          for i := 0 to nummat-1 do begin
            offset  := _ptr.ReadInt32( offMaterials + i*4 ) ;
            matsize := _ptr.ReadInt32( offMaterials + (i+1)*4 ) ;

            material.CompressionType := TGIS_CompressionType.None ;
            material.HasColor := False ;
            material.Color := TGIS_Color.Black ;
            material.HasEdgeColor := False ;
            material.EdgeColor := TGIS_Color.Black ;
            material.HasEdgeWidth := False ;
            material.EdgeWidth := 0 ;
            material.HasTransparency := False ;
            material.Transparency := 0 ;
            material.HasShininess := False ;
            material.Shininess := 0 ;
            material.HasTextureMap := False ;
            material.Bpp := 0 ;
            material.Width := 0 ;
            material.Height := 0 ;
            material.Size := 0 ;
            material.Buffer := nil ;
            material.HasCullBackFaces := False ;
            material.SharedTextureIndex := 0 ;
            material.HasSharedTexture   := False ;

            while (offset<matsize) do begin
              j := _ptr.ReadByte( offMaterialb + offset ) ;
              inc( offset, 1 ) ;
              case j of
                1 : begin
                      material.Color  := TGIS_Color.FromRGB(
                                           _ptr.ReadByte( offMaterialb +offset  ),
                                           _ptr.ReadByte( offMaterialb +offset+1),
                                           _ptr.ReadByte( offMaterialb +offset+2)
                                         ) ;
                      material.HasColor := True ;
                      inc( offset, 3 ) ;
                    end ;
                9 : begin
                      material.EdgeColor:= TGIS_Color.FromRGB(
                                             _ptr.ReadByte( offMaterialb +offset  ),
                                             _ptr.ReadByte( offMaterialb +offset+1),
                                             _ptr.ReadByte( offMaterialb +offset+2)
                                           ) ;
                      material.HasEdgeColor := True ;
                      inc( offset, 3 ) ;
                    end ;
                2 : begin
                      material.Bpp := _ptr.ReadByte( offMaterialb+offset) ;
                      inc( offset, 1 ) ;
                      material.Width := _ptr.ReadWord( offMaterialb+offset) ;
                      inc( offset, 2 ) ;
                      material.Height := _ptr.ReadWord( offMaterialb+offset) ;
                      inc( offset, 2 ) ;
                      material.Size := _ptr.ReadInt32( offMaterialb+offset) ;
                      inc( offset, 4 ) ;
                      c := _ptr.ReadInt32( offMaterialb+offset) ;
                      case c of
                        3 : material.CompressionType := TGIS_CompressionType.JPEG ;
                        4 : material.CompressionType := TGIS_CompressionType.JPEGPlus
                      else
                        material.CompressionType := TGIS_CompressionType.None ;
                      end ;
                      inc( offset, 4 ) ;
                      SetLength( material.Buffer, material.Size ) ;
                      {$IFDEF MANAGED}
                        _ptr.CopyTo( material.Buffer, 0, offMaterialb+offset, material.Size ) ;
                      {$ELSE}
                        _ptr.CopyTo( @material.Buffer[0], 0, offMaterialb+offset, material.Size ) ;
                      {$ENDIF}
                      material.HasTextureMap := True ;
                      inc( offset, material.Size ) ;
                    end ;
                3 :  begin
                      material.Transparency := _ptr.ReadByte( offMaterialb+offset) ;
                      material.HasTransparency := True ;
                      inc( offset,1);
                     end ;
                4 :  begin
                      material.Shininess := _ptr.ReadByte( offMaterialb+offset) ;
                      material.HasShininess := True ;
                      inc( offset,1);
                     end ;
                10 :  begin
                      material.EdgeWidth := _ptr.ReadByte( offMaterialb+offset) ;
                      material.HasEdgeWidth := True ;
                      inc( offset,1);
                     end ;
                5 : begin
                      material.SharedTextureIndex := _ptr.ReadInt32( offMaterialb+offset) ;
                      material.HasSharedTexture   := True ;
                      inc( offset,4) ;
                    end ;
                6 : material.HasCullBackFaces := True ;
                11: ;
              end;
            end ;
            TGIS_ShapeMultiPatch(Result).Materials.Material[i] := material ;
          end ;
        end
        else
          TGIS_ShapeMultiPatch(Result).HasMaterials := False ;

      Result.Unlock ;
    end ;

  end ;

  class procedure TGIS_GeometryFactory.GisExportGeometryToShapeEx(
    const _shp  : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _gdb : Object
    {$ELSE}
      var _gdb : OleVariant
    {$ENDIF}
  ) ;
  const
    SHPT_NULL               =  0;
    SHPT_POINT              =  1;
    SHPT_POINTM             = 21;
    SHPT_POINTZM            = 11;
    SHPT_POINTZ             =  9;
    SHPT_MULTIPOINT         =  8;
    SHPT_MULTIPOINTM        = 28;
    SHPT_MULTIPOINTZM       = 18;
    SHPT_MULTIPOINTZ        = 20;
    SHPT_POLYLINE           =  3;
    SHPT_POLYLINEM          = 23;
    SHPT_POLYLINEZM         = 13;
    SHPT_POLYLINEZ          = 10;
    SHPT_POLYGON            =  5;
    SHPT_POLYGONM           = 25;
    SHPT_POLYGONZM          = 15;
    SHPT_POLYGONZ           = 19;
    SHPT_MULTIPATCH         = 32;
  var
    shpType   : Integer ;
    {$IFDEF DCC}
      pbuf  : Pointer ;
    {$ENDIF}
    geom      : OleVariant ;
    buf       : TGIS_Bytes ;
    geombuf   : TBytes ;
    exsize    : Integer ;
    dim       : TGIS_DimensionType ;
    b3d       : Boolean ;
    bHasM     : Boolean ;
  begin
    dim   := _shp.Dimension ;
    b3d   := (dim = TGIS_DimensionType.XYZ ) or (dim = TGIS_DimensionType.XYZM) ;
    bHasM := (dim = TGIS_DimensionType.XYM ) or (dim = TGIS_DimensionType.XYZM) ;

    case _shp.ShapeType of
      TGIS_ShapeType.Point      :
        begin
          if (b3d and bHasM) then
            shpType := SHPT_POINTZM
          else if b3d then
            shpType := SHPT_POINTZ
          else if bHasM then
            shpType := SHPT_POINTM
          else
            shpType := SHPT_POINT ;
        end ;
      TGIS_ShapeType.Arc        :
        begin
          if (b3d and bHasM) then
            shpType := SHPT_POLYLINEZM
          else if b3d then
            shpType := SHPT_POLYLINEZ
          else if bHasM then
            shpType := SHPT_POLYLINEM
          else
            shpType := SHPT_POLYLINE ;
        end ;
      TGIS_ShapeType.Polygon    :
        begin
          if (b3d and bHasM) then
            shpType := SHPT_POLYGONZM
          else if b3d then
            shpType := SHPT_POLYGONZ
          else if bHasM then
            shpType := SHPT_POLYGONM
          else
            shpType := SHPT_POLYGON ;
        end ;
      TGIS_ShapeType.MultiPoint :
        begin
          if (b3d and bHasM) then
            shpType := SHPT_MULTIPOINTZM
          else if b3d then
            shpType := SHPT_MULTIPOINTZ
          else if bHasM then
            shpType := SHPT_MULTIPOINTM
          else
            shpType := SHPT_MULTIPOINT ;
        end ;
      TGIS_ShapeType.MultiPatch :
      begin
        shpType := SHPT_MULTIPATCH ;
      end ;
    else
      shpType := SHPT_NULL ;
    end ;

    if _shp.ShapeType = TGIS_ShapeType.MultiPatch then begin
      if TGIS_ShapeMultiPatch(_shp).HasNormals then
        shpType := Integer(shpType or $08000000) ;
      if TGIS_ShapeMultiPatch(_shp).HasTextures then
        shpType := Integer(shpType or $04000000) ;
      if TGIS_ShapeMultiPatch(_shp).HasNormals then
        shpType := Integer(shpType or $08000000) ;
      if TGIS_ShapeMultiPatch(_shp).HasMaterials then
        shpType := Integer(shpType or $01000000) ;
    end ;

    _shp.ExportToVAR( geom ) ;
    geombuf := TBytes(geom) ;
    exsize := length( geombuf ) ;

    if _shp.ShapeType = TGIS_ShapeType.Point then
      exsize := exsize + 4
    else
      exsize := exsize + 4 + 4*8 ;

    buf := TGIS_Bytes.Create( exsize ) ;
    try
      buf.WriteInt32( 0, shpType ) ;
      buf.CopyFrom( geombuf, 0, 4, length( geombuf ) ) ;

      {$IFDEF OXYGENE}
        _gdb := buf.Memory ;
      {$ELSE}
        _gdb := VarArrayCreate( [0,exsize], varByte ) ;
        pbuf := VarArrayLock( _gdb ) ;
        try
          buf.CopyTo( pbuf, 0, 0, buf.Size ) ;
        finally
          VarArrayUnlock( _gdb ) ;
        end ;
      {$ENDIF}
    finally
      FreeObject( buf ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportGeometry2Hex(
    const _shp : TGIS_Shape
  ) : String ;
  var
    {$IFDEF MANAGED}
      obj : Object ;
    {$ENDIF}
    store : OleVariant ;
    b     : String     ;
    i     : Integer    ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;
    try
      {$IFDEF MANAGED}
        _shp.ExportToVAR( obj ) ;
        store := OleVariant( obj ) ;
      {$ELSE}
        _shp.ExportToVAR( store ) ;
      {$ENDIF}

      for i:=0 to VarArrayHighBound( store, 1 ) do begin
        {$IFDEF OXYGENE}
          b := IntToHex( VarToInt32( VarArrayGetItem( store, i ) ), 2 ) ;
        {$ELSE}
          b := IntToHex( Integer( store[i] ), 2 ) ;
        {$ENDIF}
        Result := Result + b ;
      end ;

      store := Unassigned ;
    except
      Result := '' ;
      store := Unassigned ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportWKB2Hex(
    const _shp : TGIS_Shape
  ) : String ;
  var
    {$IFDEF MANAGED}
      obj : Object ;
    {$ENDIF}
    store : OleVariant ;
    b     : String     ;
    i     : Integer    ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;
    try
      {$IFDEF MANAGED}
        _shp.ExportToWKB( obj ) ;
        store := OleVariant( obj ) ;
      {$ELSE}
        _shp.ExportToWKB( store ) ;
      {$ENDIF}

      for i:=0 to VarArrayHighBound( store, 1 ) do begin
        {$IFDEF OXYGENE}
          b := IntToHex( VarToInt32( VarArrayGetItem( store, i ) ), 2 ) ;
        {$ELSE}
          b := IntToHex( Integer( store[i] ), 2 ) ;
        {$ENDIF}
        Result := Result + b ;
      end ;

      store := Unassigned ;
    except
      Result := '' ;
      store := Unassigned ;
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportArcToGDO(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _gdo : Object
    {$ELSE}
      var _gdo : OleVariant
    {$ENDIF}
  ) ;
  var
    size      : Cardinal   ;
    value     : DWORD      ;
    {$IFDEF MANAGED}
      vr      : TBytes     ;
      vr_off  : Integer    ;
    {$ELSE}
      ptvar   : Pointer    ;
      dtmp    : Integer    ;
    {$ENDIF}
    ptg       : TGIS_Point ;
    ptg3D     : TGIS_Point3D ;
    p, points : DWORD      ;
    r, rings  : DWORD      ;
  begin
    if not assigned( _shp ) then exit ;
    if _shp.IsEmpty then exit ;

    rings  := _shp.GetNumParts ;

    if rings = 1 then begin
      // write Polyline Geometry record
      points := _shp.GetPartSize( 0 ) ;

      size := OFFSET_GDO_GUID + sizeOf( DWORD ) +
              _shp.GetNumPoints * OFFSET_GDO_POINT ;

      _gdo  := VarArrayCreate([0, size-1], varByte ) ;
      {$IFDEF MANAGED}
        vr := TBytes(TObject(_gdo));
        vr_off := 0  ;
      {$ELSE}
        ptvar := VarArrayLock( _gdo ) ;
      {$ENDIF}

      // Polyline Geometry
      // GUID: "0FD2FFC2-8CBC-11CF-DEAB-08003601B769"
      value := $0FD2FFC2 ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $11CF8CBC ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $0008DEAB ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $69B70136 ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}

      value := points ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}

      for p := 0 to points - 1 do begin
        if _shp.Dimension in [ TGIS_DimensionType.XYZ,TGIS_DimensionType.XYM,TGIS_DimensionType.XYZM ] then
        begin
          ptg3D := _shp.GetPoint3D( 0, p ) ;

          if _shp.Dimension in [ TGIS_DimensionType.XYM ] then begin
          {$IFDEF MANAGED}
            write_double( vr, vr_off, ptg3D.X ) ;
            write_double( vr, vr_off, ptg3D.Y ) ;
            write_double( vr, vr_off, ptg3D.M ) ;
          {$ELSE}
            write_gdo( ptvar, ptg3D.X, 8 ) ;
            write_gdo( ptvar, ptg3D.Y, 8 ) ;
            write_gdo( ptvar, ptg3D.M, 8 ) ;
          {$ENDIF}
          end
          else begin
          {$IFDEF MANAGED}
            write_double( vr, vr_off, ptg3D.X ) ;
            write_double( vr, vr_off, ptg3D.Y ) ;
            write_double( vr, vr_off, ptg3D.Z ) ;
          {$ELSE}
            write_gdo( ptvar, ptg3D, 24 ) ;
          {$ENDIF}
          end ;
        end
        else begin
          ptg := _shp.GetPoint( 0, p ) ;
          {$IFDEF MANAGED}
            write_point( vr, vr_off, ptg ) ;
            write_double( vr, vr_off, 0 ) ;
          {$ELSE}
            write_gdo( ptvar, ptg, sizeOf( ptg ) ) ;
            dtmp := 0 ;
            write_gdo( ptvar, dtmp, 8 ) ;
          {$ENDIF}
        end ;
      end ;
      {$IFNDEF MANAGED}
        VarArrayUnLock( _gdo ) ;
      {$ENDIF}

    end
    else begin
      // write Composite Polyline Geometry record

      size := OFFSET_GDO_GUID + sizeOf( DWORD ) ;
      for r := 0 to rings - 1 do begin
        points := _shp.GetPartSize( r ) ;
        size := size + ( sizeOf( DWORD ) + OFFSET_GDO_GUID + sizeOf( DWORD )
                         + points * OFFSET_GDO_POINT
                       ) ;
      end ;

      _gdo  := VarArrayCreate([0, size-1], varByte ) ;
      {$IFDEF MANAGED}
        vr := TBytes(TObject(_gdo));
        vr_off := 0  ;
      {$ELSE}
        ptvar := VarArrayLock( _gdo ) ;
      {$ENDIF}

      // Composite Polyline Geometry
      // GUID: "0FD2FFCB-8CBC-11CF-DEAB-08003601B769"
      value := $0FD2FFCB ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $11CF8CBC ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $0008DEAB ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $69B70136 ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}

      value := rings ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}

      for r := 0 to rings - 1 do begin
        // write Polyline Geometry record

        points := _shp.GetPartSize( r ) ;

        // size of Polyline Geometry record
        value := OFFSET_GDO_GUID + sizeOf( DWORD ) + points * OFFSET_GDO_POINT ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        // Polyline Geometry
        // GUID: "0FD2FFC2-8CBC-11CF-DEAB-08003601B769"
        value := $0FD2FFC2 ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $11CF8CBC ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $0008DEAB ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $69B70136 ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        value := points ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        for p:= 0 to points - 1 do begin
          if _shp.Dimension in [ TGIS_DimensionType.XYZ,TGIS_DimensionType.XYM,TGIS_DimensionType.XYZM ] then
          begin
            ptg3D := _shp.GetPoint3D( r, p ) ;

            if _shp.Dimension in [ TGIS_DimensionType.XYM ] then begin
            {$IFDEF MANAGED}
              write_double( vr, vr_off, ptg3D.X ) ;
              write_double( vr, vr_off, ptg3D.Y ) ;
              write_double( vr, vr_off, ptg3D.M ) ;
            {$ELSE}
              write_gdo( ptvar, ptg3D.X, 8 ) ;
              write_gdo( ptvar, ptg3D.Y, 8 ) ;
              write_gdo( ptvar, ptg3D.M, 8 ) ;
            {$ENDIF}
            end
            else begin
            {$IFDEF MANAGED}
              write_double( vr, vr_off, ptg3D.X ) ;
              write_double( vr, vr_off, ptg3D.Y ) ;
              write_double( vr, vr_off, ptg3D.Z ) ;
            {$ELSE}
              write_gdo( ptvar, ptg3D, 24 ) ;
            {$ENDIF}
            end ;
          end
          else begin
            ptg := _shp.GetPoint( r, p ) ;
            {$IFDEF MANAGED}
              write_point( vr, vr_off, ptg ) ;
              write_double( vr, vr_off, 0 ) ;
            {$ELSE}
              write_gdo( ptvar, ptg, sizeOf( ptg ) ) ;
              dtmp := 0 ;
              write_gdo( ptvar, dtmp, 8 ) ;
            {$ENDIF}
          end ;
        end ;
      end ;

      {$IFNDEF MANAGED}
        VarArrayUnLock( _gdo ) ;
      {$ENDIF}
    end ;

  end ;

  class function TGIS_GeometryFactory.GisExportArcToGML(
    const _shp : TGIS_Shape
  ) : String;
  var
    ptg  : TGIS_Point3D ;
    i    : Integer     ;
    icnt : Integer     ;
    j    : Integer     ;
    jcnt : Integer     ;
    hdr  : String      ;
    sb   : TStringBuilder ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;
    if _shp.IsEmpty then exit ;

    sb := TStringBuilder.Create ;
    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      icnt := _shp.GetNumParts - 1 ;
      if icnt > 0 then hdr := '<gml:MultiLineString>'
                  else hdr := '<gml:LineString>' ;

      for i:= 0 to icnt do begin
        if icnt > 0 then
          sb.Append( '<gml:lineStringMember><gml:LineString>' ) ;
        jcnt := _shp.GetPartSize( i ) - 1 ;
        case _shp.Dimension of
          TGIS_DimensionType.XYZ : sb.Append( '<gml:posList srsDimension="3">' ) ;
          TGIS_DimensionType.XYZM: sb.Append( '<gml:posList srsDimension="4">' ) ;
        else
          sb.Append( '<gml:posList>' ) ;
        end ;

        for j:= 0 to jcnt do begin
          ptg  := _shp.GetPoint3D( i, j ) ;
          case _shp.Dimension of
            TGIS_DimensionType.XYZ :
              sb.Append( Format( '%s %s %s',
                                [ DotFloatToStr(ptg.X),
                                  DotFloatToStr(ptg.Y),
                                  DotFloatToStr(ptg.Z)
                                ]
                               ) ) ;

            TGIS_DimensionType.XYZM :
              sb.Append( Format( '%s %s %s %s',
                                [ DotFloatToStr(ptg.X),
                                  DotFloatToStr(ptg.Y),
                                  DotFloatToStr(ptg.Z),
                                  DotFloatToStr(ptg.M)
                                ]
                               ) )
            else
              sb.Append( Format( '%s %s',
                                [ DotFloatToStr(ptg.X),
                                  DotFloatToStr(ptg.Y)
                                ]
                               ) ) ;
          end ;
          if j < jcnt then
            sb.Append( ' ' );
        end ;
        sb.Append( '</gml:posList>' );
        if icnt > 0 then
          sb.Append( '</gml:LineString></gml:lineStringMember>' ) ;
      end ;
      if icnt > 0 then Result :=  hdr + sb.ToString + '</gml:MultiLineString>'
                  else Result :=  hdr + sb.ToString + '</gml:LineString>' ;
    finally
      _shp.Unlock ;
      FreeObject( sb ) ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportArcToJSON(
    const _shp : TGIS_Shape
  ) : String ;
  var
    ptg   : TGIS_Point3D ;
    i, j  : Integer      ;
    icnt  : Integer      ;
    jcnt  : Integer      ;
    istr  : String       ;
    jstr  : String       ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;

    istr := '' ;
    icnt := _shp.GetNumParts - 1 ;

    for i := 0 to icnt do begin
      jcnt := _shp.GetPartSize( i ) -1;
      jstr := '' ;
      for j := 0 to jcnt do begin
        ptg := _shp.GetPoint3D( i, j ) ;

        case _shp.Dimension of
          TGIS_DimensionType.Unknown,
          TGIS_DimensionType.XY   :
            jstr := jstr + Format( '[%s,%s]',
                                   [ DotFloatToStr( ptg.X ),
                                     DotFloatToStr( ptg.Y )
                                   ]
                                 ) ;
          TGIS_DimensionType.XYZ  :
            jstr := jstr + Format( '[%s,%s,%s]',
                                   [ DotFloatToStr( ptg.X ),
                                     DotFloatToStr( ptg.Y ),
                                     DotFloatToStr( ptg.Z )
                                   ]
                                 ) ;
          TGIS_DimensionType.XYM  :
            jstr := jstr + Format( '[%s,%s,%s]',
                                   [ DotFloatToStr( ptg.X ),
                                     DotFloatToStr( ptg.Y ),
                                     DotFloatToStr( ptg.M )
                                   ]
                                 ) ;
          TGIS_DimensionType.XYZM :
            jstr := jstr + Format( '[%s,%s,%s,%s]',
                                   [ DotFloatToStr( ptg.X ),
                                     DotFloatToStr( ptg.Y ),
                                     DotFloatToStr( ptg.Z ),
                                     DotFloatToStr( ptg.M )
                                   ]
                                 ) ;
        end ;

        if j < jcnt then
          jstr := jstr + ',' ;
      end ;

      if icnt > 0 then
        istr := istr + '[' + jstr + ']'
      else
        istr := istr + jstr ;

      if i < icnt then
        istr := istr + ',' ;
    end ;

    if icnt > 0 then
      Result := Format( '{"type":"MultiLineString","coordinates":[%s]}', [istr] )
    else
      Result := Format( '{"type":"LineString","coordinates":[%s]}', [istr] ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportArcToEWKB(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    const _multi : Boolean    ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  var
    size      : Cardinal   ;
    {$IFDEF OXYGENE}
      byteOrder : array of Byte := new Byte[ sizeOf( Byte ) ] ;
    {$ELSE}
      byteOrder : Byte     ;
    {$ENDIF}
    wkbtype   : DWORD      ;
    {$IFDEF MANAGED}
      vr      : TBytes     ;
      vr_off  : Integer    ;
    {$ELSE}
      ptvar   : Pointer    ;
    {$ENDIF}
    ptg       : TGIS_Point ;
    ptg3D     : TGIS_Point3D ;
    p, points : DWORD      ;
    rings     : DWORD      ;
    r, nrings : Integer    ;
    srid      : Integer    ;
  begin
    if not assigned( _shp ) then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if assigned( _shp.Layer ) and assigned( _shp.Layer.CS ) and
         ( _shp.Layer.CS.EPSG  > 0 ) and ( _shp.Layer.CS.EPSG  = _srid ) then
        srid := _shp.Layer.CS.EPSG
      else
        srid := _srid ;

      rings := _shp.GetNumParts ;
      nrings := rings ;
      if ( rings = 1 ) and not ( _multi ) then begin
        // write WKBLineString record
        points := _shp.GetPartSize( 0 ) ;

        case _shp.Dimension of
          TGIS_DimensionType.XYZ   : begin
            if srid > 0 then begin
              wkbtype :=    2 or EWKB_ZOFFSET_FLAG ;
              size    := OFFSET_EWKB_MULTI +
                         points * ( SIZEOF_TGIS_POINT + sizeOf( Double ) ) ;
            end
            else begin
              wkbtype := 1002 ;
              size    := OFFSET_WKB_MULTI +
                         points * ( SIZEOF_TGIS_POINT + sizeOf( Double ) ) ;
            end ;
          end ;
          TGIS_DimensionType.XYM   : begin
            if srid > 0 then begin
              wkbtype :=    2 or EWKB_MOFFSET_FLAG ;
              size    := OFFSET_EWKB_MULTI +
                         points * ( SIZEOF_TGIS_POINT + sizeOf( Double ) ) ;
            end
            else begin
              wkbtype := 2002 ;
              size    := OFFSET_WKB_MULTI +
                         points * ( SIZEOF_TGIS_POINT + sizeOf( Double ) ) ;
            end ;
          end ;
          TGIS_DimensionType.XYZM  : begin
            if srid > 0 then begin
              wkbtype :=    2 or EWKB_ZOFFSET_FLAG or EWKB_MOFFSET_FLAG ;
              size    := OFFSET_EWKB_MULTI +
                         points * SIZEOF_TGIS_POINT3D ;
            end
            else begin
              wkbtype := 3002 ;
              size    := OFFSET_WKB_MULTI +
                         points * SIZEOF_TGIS_POINT3D ;
            end ;
          end
          else begin
            wkbtype := 2    ;
            if srid > 0 then
              size    := OFFSET_EWKB_MULTI +
                         points * SIZEOF_TGIS_POINT
            else
              size    := OFFSET_WKB_MULTI +
                         points * SIZEOF_TGIS_POINT ;
          end ;
        end ;

        _ewkb  := VarArrayCreate([0,size-1], varByte ) ;
        {$IFDEF MANAGED}
          {$IFNDEF OXYGENE}
            assert( VarArrayElementsIsType( OleVariant(_ewkb), varByte ) ) ;
          {$ENDIF}
          vr := TBytes(_ewkb);
          vr_off := 0  ;
        {$ELSE}
          ptvar := VarArrayLock( _ewkb ) ;
        {$ENDIF}

        {$IFDEF OXYGENE}
          byteOrder[0] := 1 ;
        {$ELSE}
          byteOrder    := 1 ;
        {$ENDIF}
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
          if srid > 0 then begin
            wkbtype := wkbtype or EWKB_SRID_FLAG ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(   srid), sizeOf(DWORD) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes( points), sizeOf(DWORD) ) ;
          end
          else begin
            write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes( points), sizeOf(DWORD) ) ;
          end ;
        {$ELSE}
          write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
          if srid > 0 then begin
            wkbtype := wkbtype or EWKB_SRID_FLAG ;
            write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
            write_wkb( ptvar,    srid, sizeOf( Integer ) ) ;
            write_wkb( ptvar,  points, sizeOf(  points ) ) ;
          end
          else begin
            write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
            write_wkb( ptvar,  points, sizeOf(  points ) ) ;
          end ;
        {$ENDIF}
        //wkbtype := wkbtype and not EWKB_ALL_FLAGS ;

        if _shp.IsEmpty then begin
          {$IFNDEF MANAGED}
            VarArrayUnLock( _ewkb ) ;
          {$ENDIF}
          exit ;
        end ;

        for p := 0 to points - 1 do begin
          case _shp.Dimension of
            TGIS_DimensionType.XYZ : begin
              ptg3D := _shp.GetPoint3D( 0, p ) ;
              {$IFDEF MANAGED}
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.X),
                           sizeOf( Double )
                         ) ;
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.Y),
                           sizeOf( Double )
                         ) ;
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.Z),
                           sizeOf( Double )
                         ) ;
              {$ELSE}
                write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
                write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
                write_wkb( ptvar, ptg3D.Z, sizeOf( Double ) ) ;
              {$ENDIF}
            end ;
            TGIS_DimensionType.XYM : begin
              ptg3D := _shp.GetPoint3D( 0, p ) ;
              {$IFDEF MANAGED}
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.X),
                           sizeOf( Double )
                         ) ;
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.Y),
                           sizeOf( Double )
                         ) ;
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.M),
                           sizeOf( Double )
                         ) ;
              {$ELSE}
                write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
                write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
                write_wkb( ptvar, ptg3D.M, sizeOf( Double ) ) ;
              {$ENDIF}
            end ;
            TGIS_DimensionType.XYZM : begin
              ptg3D := _shp.GetPoint3D( 0, p ) ;
              {$IFDEF MANAGED}
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.X),
                           sizeOf( Double )
                         ) ;
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.Y),
                           sizeOf( Double )
                         ) ;
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.Z),
                           sizeOf( Double )
                         ) ;
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg3D.M),
                           sizeOf( Double )
                         ) ;
              {$ELSE}
                write_wkb( ptvar, ptg3D, SIZEOF_TGIS_POINT3D ) ;
              {$ENDIF}
            end
            else begin
              ptg := _shp.GetPoint( 0, p ) ;
              {$IFDEF MANAGED}
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg.X),
                           sizeOf( Double )
                         ) ;
                write_wkb( vr, vr_off,
                           BitConverter.GetBytes(ptg.Y),
                           sizeOf( Double )
                         ) ;
              {$ELSE}
                write_wkb( ptvar, ptg, sizeOf( ptg ) ) ;
              {$ENDIF}
            end ;
          end ;
        end ;
        {$IFNDEF MANAGED}
          VarArrayUnLock( _ewkb ) ;
        {$ENDIF}

      end
      else begin
        // write WKBMultiLineString record

        if srid > 0 then
          size := OFFSET_EWKB_MULTI
        else
          size := OFFSET_WKB_MULTI ;

        case _shp.Dimension of
          TGIS_DimensionType.XYZ   : begin
            if srid > 0 then
              wkbtype :=    5 or EWKB_ZOFFSET_FLAG
            else
              wkbtype := 1005 ;

            for r := 0 to nrings - 1 do begin
              points := _shp.GetPartSize( r ) ;
              size   := size +
                        ( OFFSET_WKB_MULTI +
                          points * ( SIZEOF_TGIS_POINT + sizeOf( Double ) )
                        ) ;
            end ;
          end ;
          TGIS_DimensionType.XYM   : begin
            if srid > 0 then
              wkbtype :=    5 or EWKB_MOFFSET_FLAG
            else
              wkbtype := 2005 ;

            for r := 0 to nrings - 1 do begin
              points := _shp.GetPartSize( r ) ;
              size   := size +
                        ( OFFSET_WKB_MULTI +
                          points * ( SIZEOF_TGIS_POINT + sizeOf( Double ) )
                        ) ;
            end ;
          end ;
          TGIS_DimensionType.XYZM  : begin
            if srid > 0 then
              wkbtype :=    5 or EWKB_ZOFFSET_FLAG or EWKB_MOFFSET_FLAG
            else
              wkbtype := 3005 ;

            for r := 0 to nrings - 1 do begin
              points := _shp.GetPartSize( r ) ;
              size   := size +
                        ( OFFSET_WKB_MULTI +
                          points * SIZEOF_TGIS_POINT3D
                        ) ;
            end ;
          end
          else begin
            wkbtype := 5 ;
            for r := 0 to nrings - 1 do begin
              points := _shp.GetPartSize( r ) ;
              size   := size +
                        ( OFFSET_WKB_MULTI +
                          points * SIZEOF_TGIS_POINT
                        ) ;
            end ;
          end ;
        end ;

        _ewkb  := VarArrayCreate([0,size-1], varByte ) ;
        {$IFDEF MANAGED}
          {$IFNDEF OXYGENE}
            assert( VarArrayElementsIsType( OleVariant(_ewkb), varByte ) ) ;
          {$ENDIF}
          vr := TBytes(_ewkb);
          vr_off := 0  ;
        {$ELSE}
          ptvar := VarArrayLock( _ewkb ) ;
        {$ENDIF}

        {$IFDEF OXYGENE}
          byteOrder[0] := 1 ;
        {$ELSE}
          byteOrder    := 1 ;
        {$ENDIF}
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
          if srid > 0 then begin
            wkbtype := wkbtype or EWKB_SRID_FLAG ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(   srid), sizeOf(DWORD) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(  rings), sizeOf(DWORD) ) ;
          end
          else begin
            write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(  rings), sizeOf(DWORD) ) ;
          end ;
        {$ELSE}
          write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
          if srid > 0 then begin
            wkbtype := wkbtype or EWKB_SRID_FLAG ;
            write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
            write_wkb( ptvar,    srid, sizeOf( Integer ) ) ;
            write_wkb( ptvar,   rings, sizeOf(   rings ) ) ;
          end
          else begin
            write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
            write_wkb( ptvar,   rings, sizeOf(   rings ) ) ;
          end ;
        {$ENDIF}
        //wkbtype := wkbtype and not EWKB_ALL_FLAGS ;

        if _shp.IsEmpty then begin
          {$IFNDEF MANAGED}
            VarArrayUnLock( _ewkb ) ;
          {$ENDIF}
          exit ;
        end ;

        for r := 0 to nrings - 1 do begin
          // write WKBLineString record

          points := _shp.GetPartSize( r ) ;

          {$IFDEF OXYGENE}
            byteOrder[0] := 1 ;
          {$ELSE}
            byteOrder    := 1 ;
          {$ENDIF}

          if srid > 0 then
            case _shp.Dimension of
              TGIS_DimensionType.XYZ  : wkbtype :=    2 or EWKB_ZOFFSET_FLAG ;
              TGIS_DimensionType.XYM  : wkbtype :=    2 or EWKB_MOFFSET_FLAG ;
              TGIS_DimensionType.XYZM : wkbtype :=    2 or EWKB_ZOFFSET_FLAG
                                                        or EWKB_MOFFSET_FLAG ;
              else                      wkbtype :=    2 ;
            end
          else
            case _shp.Dimension of
              TGIS_DimensionType.XYZ  : wkbtype := 1002 ;
              TGIS_DimensionType.XYM  : wkbtype := 2002 ;
              TGIS_DimensionType.XYZM : wkbtype := 3002 ;
              else                      wkbtype :=    2 ;
            end ;

          {$IFDEF MANAGED}
            write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype),
                       sizeOf( DWORD ) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(points),
                       sizeOf( DWORD ) ) ;
          {$ELSE}
            write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
            write_wkb( ptvar, wkbtype  , sizeOf( wkbtype   ) ) ;
            write_wkb( ptvar, points   , sizeOf( points    ) ) ;
          {$ENDIF}

          for p:= 0 to points - 1 do begin
            case _shp.Dimension of

              TGIS_DimensionType.XYZ : begin
                ptg3D := _shp.GetPoint3D( r, p ) ;
                {$IFDEF MANAGED}
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.X),
                             sizeOf( Double )
                           ) ;
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.Y),
                             sizeOf( Double )
                           ) ;
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.Z),
                             sizeOf( Double )
                           ) ;
                {$ELSE}
                  write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
                  write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
                  write_wkb( ptvar, ptg3D.Z, sizeOf( Double ) ) ;
                {$ENDIF}
              end ;
              TGIS_DimensionType.XYM : begin
                ptg3D := _shp.GetPoint3D( r, p ) ;
                {$IFDEF MANAGED}
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.X),
                             sizeOf( Double )
                           ) ;
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.Y),
                             sizeOf( Double )
                           ) ;
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.M),
                             sizeOf( Double )
                           ) ;
                {$ELSE}
                  write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
                  write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
                  write_wkb( ptvar, ptg3D.M, sizeOf( Double ) ) ;
                {$ENDIF}
              end ;
              TGIS_DimensionType.XYZM : begin
                ptg3D := _shp.GetPoint3D( r, p ) ;
                {$IFDEF MANAGED}
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.X),
                             sizeOf( Double )
                           ) ;
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.Y),
                             sizeOf( Double )
                           ) ;
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.Z),
                             sizeOf( Double )
                           ) ;
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg3D.M),
                             sizeOf( Double )
                           ) ;
                {$ELSE}
                  write_wkb( ptvar, ptg3D, SIZEOF_TGIS_POINT3D ) ;
                {$ENDIF}
              end
              else begin
                ptg := _shp.GetPoint( r, p ) ;
                {$IFDEF MANAGED}
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg.X),
                             sizeOf( Double )
                           ) ;
                  write_wkb( vr, vr_off,
                             BitConverter.GetBytes(ptg.Y),
                             sizeOf( Double )
                           ) ;
                {$ELSE}
                  write_wkb( ptvar, ptg, sizeOf( ptg ) ) ;
                {$ENDIF}
             end ;
            end ;

          end ;
        end ;

        {$IFNDEF MANAGED}
          VarArrayUnLock( _ewkb ) ;
        {$ENDIF}
      end ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportArcToEWKB(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    GisExportArcToEWKB( _shp, _srid, false, _ewkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportArcToEWKB(
    const _shp  : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    if assigned( _shp ) and assigned( _shp.Layer ) then
      GisExportArcToEWKB( _shp, _shp.Layer.CS.EPSG, false, _ewkb )
    else
      GisExportArcToEWKB( _shp, 0, False, _ewkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportArcToWKB(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _wkb : Object
    {$ELSE}
      var _wkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    GisExportArcToEWKB( _shp, 0, false, _wkb ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportArcToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    const _ver  : Integer
  ) : String ;
  var
    space : String       ;
    ptg   : TGIS_Point   ;
    ptg3D : TGIS_Point3D ;
    i     : Integer      ;
    icnt  : Integer      ;
    j     : Integer      ;
    jcnt  : Integer      ;
    sb    : TStringBuilder ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;

    sb := TStringBuilder.Create ;
    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if ( _srid > 0 ) then begin
        if ( _shp.Layer         <>   nil ) and
           ( _shp.Layer.CS      <>   nil ) and
           ( _shp.Layer.CS.EPSG  >     0 ) and
           ( _shp.Layer.CS.EPSG  = _srid ) then
          Result := Format( 'SRID=%d;', [_shp.Layer.CS.EPSG] )
        else
          Result := Format( 'SRID=%d;', [_srid] ) ;
      end
      else
        Result := '' ;

      icnt := _shp.GetNumParts - 1 ;
      if icnt > 0 then
        Result := Result + 'MULTILINESTRING%s%s'
      else
        Result := Result + 'LINESTRING%s%s' ;

      if _ver = 0 then space := ''
                  else space := ' ' ;

      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY      : Result := Format( Result, [''   ,''  ] ) ;
        TGIS_DimensionType.XYZ     : Result := Format( Result, [space,'Z' ] ) ;
        TGIS_DimensionType.XYM     : Result := Format( Result, [space,'M' ] ) ;
        TGIS_DimensionType.XYZM    : Result := Format( Result, [space,'ZM'] ) ;
      end ;

      if _shp.IsEmpty then begin
        Result := Result + ' EMPTY' ;
        exit ;
      end ;

      sb.Append( Result ) ;
      sb.Append( '(' ) ;
      for i:= 0 to icnt do begin
        if icnt > 0 then
          sb.Append( '(' ) ;
        jcnt := _shp.GetPartSize( i ) - 1 ;
        for j:= 0 to jcnt do begin
          case _shp.Dimension of
            TGIS_DimensionType.Unknown,
            TGIS_DimensionType.XY :
              begin
                ptg := _shp.GetPoint( i, j ) ;
                sb.Append( Format( '%s %s',
                                   [ DotFloatToStr(ptg.X),
                                     DotFloatToStr(ptg.Y)
                                   ]
                                 ) ) ;
              end ;
            TGIS_DimensionType.XYZ :
              begin
                ptg3D  := _shp.GetPoint3D( i, j ) ;
                sb.Append( Format( '%s %s %s',
                                   [ DotFloatToStr(ptg3D.X),
                                     DotFloatToStr(ptg3D.Y),
                                     DotFloatToStr(ptg3D.Z)
                                   ]
                                 ) );
              end ;
            TGIS_DimensionType.XYM :
              begin
                ptg3D  := _shp.GetPoint3D( i, j ) ;
                sb.Append( Format( '%s %s %s',
                                   [ DotFloatToStr(ptg3D.X),
                                     DotFloatToStr(ptg3D.Y),
                                     DotFloatToStr(ptg3D.M)
                                   ]
                                 ) ) ;
              end ;
            TGIS_DimensionType.XYZM :
              begin
                ptg3D  := _shp.GetPoint3D( i, j ) ;
                sb.Append( Format( '%s %s %s %s',
                                   [ DotFloatToStr(ptg3D.X),
                                     DotFloatToStr(ptg3D.Y),
                                     DotFloatToStr(ptg3D.Z),
                                     DotFloatToStr(ptg3D.M)
                                   ]
                                 ) );
              end ;
          end ;

          if j <> jcnt then
            sb.Append( ',' )
        end ;
        if icnt > 0 then
          sb.Append( ')' ) ;
        if i <> icnt then
          sb.Append( ',' )
      end ;
      sb.Append( ')' ) ;
      Result := sb.ToString ;
    finally
      FreeObject( sb ) ;
      _shp.Unlock ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportArcToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer
  ) : String ;
  begin
    Result := GisExportArcToEWKT( _shp, _srid, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportArcToEWKT(
    const _shp  : TGIS_Shape
  ) : String ;
  begin
    Result := GisExportArcToEWKT( _shp, 0, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportArcToWKT(
    const _shp : TGIS_Shape
  ) : String ;
  begin
    Result := GisExportArcToEWKT( _shp, 0, 1 ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportArcToVAR(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _var : Object
    {$ELSE}
      var _var : OleVariant
    {$ENDIF}
  ) ;
  var
    {$IFDEF MANAGED}
      vr     : TBytes  ;
      vr_off : Integer ;
    {$ELSE}
      pbuf   : Pointer ;
    {$ENDIF}
    val      : Double ;
    size     : Integer ;
  begin
    if _shp.IsEmpty then exit ;

    // calculate size
    size := _shp.GeometrySize ;
    if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then
      size := size + 16 + _shp.PointsZSize + 16 + _shp.PointsMSize
    else if _shp.Dimension in [ TGIS_DimensionType.XYM ] then
      size := size + 16 + _shp.PointsMSize ;

    _var := VarArrayCreate([0,size], varByte ) ;
    {$IFDEF MANAGED}
      vr   := TBytes(TObject(_var));
      vr_off := 0 ;
    {$ELSE}
      pbuf  := VarArrayLock( _var ) ;
    {$ENDIF}

    try
      {$IFDEF MANAGED}
        write_wkb( vr, vr_off, _shp.Parts, _shp.PartsSize ) ;
        write_wkb( vr, vr_off, _shp.Points, _shp.PointsSize ) ;
      {$ELSE}
        write_wkb( pbuf, _shp.Parts^, _shp.PartsSize ) ;
        write_wkb( pbuf, _shp.Points^, _shp.PointsSize ) ;
      {$ENDIF}

      if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then begin
        val := _shp.PointsZMin ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := _shp.PointsZMax ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_wkb( vr, vr_off, _shp.PointsZ, _shp.PointsZSize ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_wkb( pbuf, _shp.PointsZ^, _shp.PointsZSize ) ;
        {$ENDIF}
      end ;

      if _shp.Dimension in [ TGIS_DimensionType.XYM, TGIS_DimensionType.XYZM ] then begin
        val := _shp.PointsMMin ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := _shp.PointsMMax ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_wkb( vr, vr_off, _shp.PointsM, _shp.PointsMSize ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_wkb( pbuf, _shp.PointsM^, _shp.PointsMSize ) ;
        {$ENDIF}
      end
      else if ( _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] ) then
      begin
        val := 0 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := 0 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_fakebuf( vr, vr_off, _shp.PointsZSize  ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_fakebuf( pbuf, _shp.PointsZSize  ) ;
        {$ENDIF}
      end ;
    finally
      {$IFNDEF MANAGED}
        VarArrayUnlock( _var ) ;
      {$ENDIF}
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportMultiPointToGDO(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _gdo : Object
    {$ELSE}
      var _gdo : OleVariant
    {$ENDIF}
  ) ;
  var
    size      : Cardinal   ;
    value     : DWORD      ;
    {$IFDEF MANAGED}
      vr      : TBytes     ;
      vr_off  : Integer    ;
    {$ELSE}
      ptvar   : Pointer    ;
      dtmp    : Double     ;
    {$ENDIF}
    ptg       : TGIS_Point ;
    ptg3D     : TGIS_Point3D ;
    p, points : DWORD      ;
  begin
    if not assigned( _shp ) then exit ;
    if _shp.IsEmpty then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      points := _shp.GetPartSize( 0 ) ;

      size := OFFSET_GDO_GUID + sizeOf( DWORD )
              + points * ( sizeOf( DWORD ) +
                           OFFSET_GDO_GUID + OFFSET_GDO_POINT
                         ) ;

      _gdo  := VarArrayCreate([0, size-1], varByte ) ;

      {$IFDEF MANAGED}
        vr := TBytes(TObject(_gdo));
        vr_off := 0  ;
      {$ELSE}
        ptvar := VarArrayLock( _gdo ) ;
      {$ENDIF}

      // Geometry Collection
      // GUID: "0FD2FFC6-8CBC-11CF-DEAB-08003601B769"
      value := $0FD2FFC6 ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $11CF8CBC ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $0008DEAB ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $69B70136 ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}

      value := points ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}

      for p:= 0 to points - 1 do begin
        // size of Point Geometry record
        value := OFFSET_GDO_GUID + OFFSET_GDO_POINT ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        // write Point Geometry record

        // Point Geometry
        // GUID: "0FD2FFC0-8CBC-11CF-DEAB-08003601B769"
        value := $0FD2FFC0 ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $11CF8CBC ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $0008DEAB ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $69B70136 ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        if _shp.Dimension in [ TGIS_DimensionType.XYZ,TGIS_DimensionType.XYM,TGIS_DimensionType.XYZM ] then
        begin
          ptg3D := _shp.GetPoint3D( 0, p ) ;

          if _shp.Dimension in [ TGIS_DimensionType.XYM ] then begin
          {$IFDEF MANAGED}
            write_double( vr, vr_off, ptg3D.X ) ;
            write_double( vr, vr_off, ptg3D.Y ) ;
            write_double( vr, vr_off, ptg3D.M ) ;
          {$ELSE}
            write_gdo( ptvar, ptg3D.X, 8 ) ;
            write_gdo( ptvar, ptg3D.Y, 8 ) ;
            write_gdo( ptvar, ptg3D.M, 8 ) ;
          {$ENDIF}
          end
          else begin
          {$IFDEF MANAGED}
            write_double( vr, vr_off, ptg3D.X ) ;
            write_double( vr, vr_off, ptg3D.Y ) ;
            write_double( vr, vr_off, ptg3D.Z ) ;
          {$ELSE}
            write_gdo( ptvar, ptg3D, 24 ) ;
          {$ENDIF}
          end ;
        end
        else begin
          ptg := _shp.GetPoint( 0, p ) ;
          {$IFDEF MANAGED}
            write_point( vr, vr_off, ptg ) ;
            write_double( vr, vr_off, 0 ) ;
          {$ELSE}
            write_gdo( ptvar, ptg , sizeOf( ptg ) ) ;
            dtmp := 0 ;
            write_gdo( ptvar, dtmp, 8 ) ;
          {$ENDIF}
        end ;
      end ;

      {$IFNDEF MANAGED}
        VarArrayUnLock( _gdo ) ;
      {$ENDIF}
    finally
      _shp.Unlock ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportMultiPointToGML(
    const _shp : TGIS_Shape
  ) : String ;
  var
    ptg  : TGIS_Point  ;
    i    : Integer     ;
    icnt : Integer     ;
    hdr  : String      ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;
    if _shp.IsEmpty then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      icnt := _shp.GetPartSize( 0 ) - 1 ;
      hdr := '<gml:MultiPoint><gml:pointMembers>';
      for i:= 0 to icnt do begin
        ptg := _shp.GetPoint( 0, i ) ;
        Result := Result +
                  Format( '<gml:Point><gml:pos>%s %s</gml:pos>'+
                          '</gml:Point>',
                          [ DotFloatToStr(ptg.X),
                            DotFloatToStr(ptg.Y)
                          ]
                        ) ;
      end ;
      Result :=  hdr + Result + '</gml:pointMembers></gml:MultiPoint>' ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportMultiPointToJSON(
    const _shp : TGIS_Shape
  ) : String ;
  var
    ptg : TGIS_Point3D ;
    i   : Integer      ;
    cnt : Integer      ;
    str : String       ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;

    str := '' ;
    cnt := _shp.GetPartSize( 0 ) - 1 ;

    for i := 0 to cnt do begin
      ptg := _shp.GetPoint3D( 0, i ) ;

      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY   :
          str := str + Format( '[%s,%s]',
                               [ DotFloatToStr( ptg.X ),
                                 DotFloatToStr( ptg.Y )
                               ]
                             ) ;
        TGIS_DimensionType.XYZ  :
          str := str + Format( '[%s,%s,%s]',
                               [ DotFloatToStr( ptg.X ),
                                 DotFloatToStr( ptg.Y ),
                                 DotFloatToStr( ptg.Z )
                               ]
                             ) ;
        TGIS_DimensionType.XYM  :
          str := str + Format( '[%s,%s,%s]',
                               [ DotFloatToStr( ptg.X ),
                                 DotFloatToStr( ptg.Y ),
                                 DotFloatToStr( ptg.M )
                               ]
                             ) ;
        TGIS_DimensionType.XYZM :
          str := str + Format( '[%s,%s,%s,%s]',
                               [ DotFloatToStr( ptg.X ),
                                 DotFloatToStr( ptg.Y ),
                                 DotFloatToStr( ptg.Z ),
                                 DotFloatToStr( ptg.M )
                               ]
                             ) ;
      end ;

      if i < cnt then
        str := str + ',' ;
    end ;

    Result := Format( '{"type":"MultiPoint","coordinates":[%s]}', [str] ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportMultiPointToEWKB(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  var
    size      : Cardinal   ;
    {$IFDEF OXYGENE}
      byteOrder : array of Byte := new Byte[ sizeOf( Byte ) ] ;
    {$ELSE}
      byteOrder : Byte     ;
    {$ENDIF}
    wkbtype   : DWORD      ;
    {$IFDEF MANAGED}
      vr      : TBytes     ;
      vr_off  : Integer    ;
    {$ELSE}
      ptvar   : Pointer    ;
    {$ENDIF}
    ptg       : TGIS_Point ;
    ptg3D     : TGIS_Point3D ;
    p, points : DWORD      ;
    srid      : Integer    ;
  begin
    if not assigned( _shp ) then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if assigned( _shp.Layer ) and assigned( _shp.Layer.CS ) and
         ( _shp.Layer.CS.EPSG  > 0 ) and ( _shp.Layer.CS.EPSG  = _srid ) then
        srid := _shp.Layer.CS.EPSG
      else
        srid := _srid ;

      points := _shp.GetPartSize( 0 ) ;

      case _shp.Dimension of
        TGIS_DimensionType.XYZ   : begin
          if srid > 0 then begin
            wkbtype :=    4 or EWKB_ZOFFSET_FLAG ;
            size    := OFFSET_EWKB_MULTI +
                       points * ( OFFSET_WKB + SIZEOF_TGIS_POINT +
                                  sizeOf( Double )
                                ) ;
          end
          else begin
            wkbtype := 1004 ;
            size    := OFFSET_WKB_MULTI +
                       points * ( OFFSET_WKB  + SIZEOF_TGIS_POINT +
                                  sizeOf( Double )
                                ) ;
          end ;
        end ;
        TGIS_DimensionType.XYM   : begin
          if srid > 0 then begin
            wkbtype :=    4 or EWKB_MOFFSET_FLAG ;
            size    := OFFSET_EWKB_MULTI +
                       points * ( OFFSET_WKB + SIZEOF_TGIS_POINT +
                                  sizeOf( Double )
                                ) ;
          end
          else begin
            wkbtype := 2004 ;
            size    := OFFSET_WKB_MULTI +
                       points * ( OFFSET_WKB  + SIZEOF_TGIS_POINT +
                                  sizeOf( Double )
                                ) ;
          end ;
        end ;
        TGIS_DimensionType.XYZM  : begin
          if srid > 0 then begin
            wkbtype :=    4 or EWKB_ZOFFSET_FLAG or EWKB_MOFFSET_FLAG ;
            size    := OFFSET_EWKB_MULTI +
                       points * ( OFFSET_WKB + SIZEOF_TGIS_POINT3D ) ;
          end
          else begin
            wkbtype := 3004 ;
            size    := OFFSET_WKB_MULTI +
                       points * ( OFFSET_WKB  + SIZEOF_TGIS_POINT3D ) ;
          end ;
        end
        else begin
          wkbtype := 4    ;
          if srid > 0 then
            size := OFFSET_EWKB_MULTI +
                    points * ( OFFSET_WKB + SIZEOF_TGIS_POINT
                             )
          else
            size := OFFSET_WKB_MULTI +
                    points * ( OFFSET_WKB  + SIZEOF_TGIS_POINT
                             ) ;
        end ;
      end ;

      _ewkb  := VarArrayCreate([0,size-1], varByte ) ;
      {$IFDEF MANAGED}
        {$IFNDEF OXYGENE}
          assert( VarArrayElementsIsType( OleVariant(_ewkb), varByte ) ) ;
        {$ENDIF}
        vr := TBytes(_ewkb);
        vr_off := 0  ;
      {$ELSE}
        ptvar := VarArrayLock( _ewkb ) ;
      {$ENDIF}

      // write WKBMultiPoint record
      {$IFDEF OXYGENE}
        byteOrder[0] := 1 ;
      {$ELSE}
        byteOrder    := 1 ;
      {$ENDIF}
      {$IFDEF MANAGED}
        write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
        if srid > 0 then begin
          wkbtype := wkbtype or EWKB_SRID_FLAG ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(   srid), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes( points), sizeOf(DWORD) ) ;
        end
        else begin
          write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes( points), sizeOf(DWORD) ) ;
        end ;
      {$ELSE}
        write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
        if srid > 0 then begin
          wkbtype := wkbtype or EWKB_SRID_FLAG ;
          write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
          write_wkb( ptvar,    srid, sizeOf( Integer ) ) ;
          write_wkb( ptvar,  points, sizeOf(  points ) ) ;
        end
        else begin
          write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
          write_wkb( ptvar,  points, sizeOf(  points ) ) ;
        end ;
      {$ENDIF}
      //wkbtype := wkbtype and not EWKB_ALL_FLAGS ;

      if _shp.IsEmpty then begin
        {$IFNDEF MANAGED}
          VarArrayUnLock( _ewkb ) ;
        {$ENDIF}
        exit ;
      end ;

      for p:= 0 to points - 1 do begin
        // write whole WKBPoint record
        if srid > 0 then
          case _shp.Dimension of
            TGIS_DimensionType.XYZ  : wkbtype :=    1 or EWKB_ZOFFSET_FLAG ;
            TGIS_DimensionType.XYM  : wkbtype :=    1 or EWKB_MOFFSET_FLAG ;
            TGIS_DimensionType.XYZM : wkbtype :=    1 or EWKB_ZOFFSET_FLAG
                                                      or EWKB_MOFFSET_FLAG ;
            else                      wkbtype :=    1 ;
          end
        else
          case _shp.Dimension of
            TGIS_DimensionType.XYZ  : wkbtype := 1001 ;
            TGIS_DimensionType.XYM  : wkbtype := 2001 ;
            TGIS_DimensionType.XYZM : wkbtype := 3001 ;
            else                      wkbtype :=    1 ;
          end ;

        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
        {$ELSE}
          write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
          write_wkb( ptvar, wkbtype  , sizeOf( wkbtype   ) ) ;
        {$ENDIF}

        case _shp.Dimension of
          TGIS_DimensionType.XYZ : begin
            ptg3D := _shp.GetPoint3D( 0, p ) ;
            {$IFDEF MANAGED}
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.X),
                         sizeOf( Double ) );
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Y),
                         sizeOf( Double ) );
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Z),
                         sizeOf( Double ) );
            {$ELSE}
              write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
              write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
              write_wkb( ptvar, ptg3D.Z, sizeOf( Double ) ) ;
            {$ENDIF}
          end ;
          TGIS_DimensionType.XYM : begin
            ptg3D := _shp.GetPoint3D( 0, p ) ;
            {$IFDEF MANAGED}
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.X),
                         sizeOf( Double ) );
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Y),
                         sizeOf( Double ) );
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.M),
                         sizeOf( Double ) );
            {$ELSE}
              write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
              write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
              write_wkb( ptvar, ptg3D.M, sizeOf( Double ) ) ;
            {$ENDIF}
          end ;
          TGIS_DimensionType.XYZM : begin
            ptg3D := _shp.GetPoint3D( 0, p ) ;
            {$IFDEF MANAGED}
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.X),
                         sizeOf( Double ) );
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Y),
                         sizeOf( Double ) );
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Z),
                         sizeOf( Double ) );
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.M),
                         sizeOf( Double ) );
            {$ELSE}
              write_wkb( ptvar, ptg3D, SIZEOF_TGIS_POINT3D ) ;
            {$ENDIF}
        end
        else begin
          ptg := _shp.GetPoint( 0, p ) ;
          {$IFDEF MANAGED}
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg.X),
                       sizeOf( Double ) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg.Y),
                       sizeOf( Double ) ) ;
          {$ELSE}
            write_wkb( ptvar, ptg , sizeOf( ptg ) ) ;
          {$ENDIF}
          end ;
        end ;

      end ;

      {$IFNDEF MANAGED}
        VarArrayUnLock( _ewkb ) ;
      {$ENDIF}
    finally
      _shp.Unlock ;
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportMultiPointToEWKB(
    const _shp  : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    if assigned( _shp ) and assigned( _shp.Layer ) then
      GisExportMultiPointToEWKB( _shp, _shp.Layer.CS.EPSG, _ewkb )
    else
      GisExportMultiPointToEWKB( _shp, 0, _ewkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportMultiPointToWKB(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _wkb : Object
    {$ELSE}
      var _wkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    GisExportMultiPointToEWKB( _shp, 0, _wkb ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportMultiPointToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    const _ver  : Integer
  ) : String ;
  var
    space : String       ;
    ptg   : TGIS_Point   ;
    ptg3D : TGIS_Point3D ;
    i     : Integer      ;
    icnt  : Integer      ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if ( _srid > 0 ) then begin
        if ( _shp.Layer         <>   nil ) and
           ( _shp.Layer.CS      <>   nil ) and
           ( _shp.Layer.CS.EPSG  >     0 ) and
           ( _shp.Layer.CS.EPSG  = _srid ) then
          Result := Format( 'SRID=%d;', [_shp.Layer.CS.EPSG] )
        else
          Result := Format( 'SRID=%d;', [_srid] ) ;
      end
      else
        Result := '' ;

      Result := Result + 'MULTIPOINT%s%s' ;

      if _ver = 0 then space := ''
                  else space := ' ' ;

      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY      : Result := Format( Result, [''   ,''  ] ) ;
        TGIS_DimensionType.XYZ     : Result := Format( Result, [space,'Z' ] ) ;
        TGIS_DimensionType.XYM     : Result := Format( Result, [space,'M' ] ) ;
        TGIS_DimensionType.XYZM    : Result := Format( Result, [space,'ZM'] ) ;
      end ;

      if _shp.IsEmpty then begin
        Result := Result + ' EMPTY' ;
        exit ;
      end ;

      Result := Result + '(' ;
      icnt := _shp.GetPartSize( 0 ) - 1 ;
      for i:= 0 to icnt do begin
        case _shp.Dimension of
          TGIS_DimensionType.Unknown,
          TGIS_DimensionType.XY :
            begin
              ptg := _shp.GetPoint( 0, i ) ;
              Result := Result + Format( '%s %s',
                                         [ DotFloatToStr(ptg.X),
                                           DotFloatToStr(ptg.Y)
                                         ]
                                       ) ;
            end ;
          TGIS_DimensionType.XYZ :
            begin
              ptg3D  := _shp.GetPoint3D( 0, i ) ;
              Result := Result + Format( '%s %s %s',
                                         [ DotFloatToStr(ptg3D.X),
                                           DotFloatToStr(ptg3D.Y),
                                           DotFloatToStr(ptg3D.Z)
                                         ]
                                       ) ;
            end ;
          TGIS_DimensionType.XYM :
            begin
              ptg3D  := _shp.GetPoint3D( 0, i ) ;
              Result := Result + Format( '%s %s %s',
                                         [ DotFloatToStr(ptg3D.X),
                                           DotFloatToStr(ptg3D.Y),
                                           DotFloatToStr(ptg3D.M)
                                         ]
                                       ) ;
            end ;
          TGIS_DimensionType.XYZM :
            begin
              ptg3D  := _shp.GetPoint3D( 0, i ) ;
              Result := Result + Format( '%s %s %s %s',
                                         [ DotFloatToStr(ptg3D.X),
                                           DotFloatToStr(ptg3D.Y),
                                           DotFloatToStr(ptg3D.Z),
                                           DotFloatToStr(ptg3D.M)
                                         ]
                                       ) ;
            end ;
        end ;
        if i <> icnt then
          Result := Result + ','
      end ;
      Result :=  Result + ')' ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportMultiPointToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer
  ) : String ;
  begin
    Result := GisExportMultiPointToEWKT( _shp, _srid, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportMultiPointToEWKT(
    const _shp  : TGIS_Shape
  ) : String ;
  begin
    Result := GisExportMultiPointToEWKT( _shp, 0, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportMultiPointToWKT(
    const _shp : TGIS_Shape
  ) : String ;
  begin
    Result := GisExportMultiPointToEWKT( _shp, 0, 1 ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportMultiPointToVAR(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _var : Object
    {$ELSE}
      var _var : OleVariant
    {$ENDIF}
  ) ;
  var
    {$IFDEF MANAGED}
      vr     : TBytes  ;
      vr_off : Integer ;
    {$ELSE}
      pbuf   : Pointer ;
    {$ENDIF}
    val      : Double ;
    size     : Integer ;
  begin
    if _shp.IsEmpty then exit ;

    // calculate size
    size := _shp.GeometrySize ;
    if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then
      size := size + 16 + _shp.PointsZSize + 16 + _shp.PointsMSize
    else if _shp.Dimension in [ TGIS_DimensionType.XYM ] then
      size := size + 16 + _shp.PointsMSize ;

    _var := VarArrayCreate([0,size], varByte ) ;
    {$IFDEF MANAGED}
      vr := TBytes(TObject(_var));
      vr_off := 0 ;
    {$ELSE}
      pbuf  := VarArrayLock( _var ) ;
    {$ENDIF}
    try
      {$IFDEF MANAGED}
        write_wkb( vr, vr_off, _shp.Parts, _shp.PartsSize ) ;
        write_wkb( vr, vr_off, _shp.Points, _shp.PointsSize ) ;
      {$ELSE}
        write_wkb( pbuf, _shp.Parts^, _shp.PartsSize ) ;
        write_wkb( pbuf, _shp.Points^, _shp.PointsSize ) ;
      {$ENDIF}

      if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then begin
        val := _shp.PointsZMin ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := _shp.PointsZMax ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_wkb( vr, vr_off, _shp.PointsZ, _shp.PointsZSize ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_wkb( pbuf, _shp.PointsZ^, _shp.PointsZSize ) ;
        {$ENDIF}
      end ;

      if _shp.Dimension in [ TGIS_DimensionType.XYM, TGIS_DimensionType.XYZM ] then begin
        val := _shp.PointsMMin ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := _shp.PointsMMax ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_wkb( vr, vr_off, _shp.PointsM, _shp.PointsMSize ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_wkb( pbuf, _shp.PointsM^, _shp.PointsMSize ) ;
        {$ENDIF}
      end
      else if ( _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] ) then
      begin
        val := 0 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := 0 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_fakebuf( vr, vr_off, _shp.PointsZSize  ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_fakebuf( pbuf, _shp.PointsZSize  ) ;
        {$ENDIF}
      end ;
    finally
      {$IFNDEF MANAGED}
        VarArrayUnlock( _var ) ;
      {$ENDIF}
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportPointToGDO(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _gdo : Object
    {$ELSE}
      var _gdo : OleVariant
    {$ENDIF}
  ) ;
  var
    size      : Cardinal   ;
    {$IFDEF MANAGED}
      vr      : TBytes     ;
      vr_off  : Integer    ;
    {$ELSE}
      ptvar   : Pointer    ;
      dtmp    : Double     ;
    {$ENDIF}
    value     : DWORD      ;
    ptg       : TGIS_Point ;
    ptg3D     : TGIS_Point3D ;
  begin
    if not assigned( _shp ) then exit ;
    if _shp.IsEmpty then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      size := OFFSET_GDO_GUID + OFFSET_GDO_POINT ;

      _gdo  := VarArrayCreate([0, size-1], varByte ) ;
      {$IFDEF MANAGED}
        vr := TBytes(TObject(_gdo));
        vr_off := 0  ;
      {$ELSE}
        ptvar := VarArrayLock( _gdo ) ;
      {$ENDIF}

      // Point Geometry
      // GUID: "0FD2FFC0-8CBC-11CF-DEAB-08003601B769"
      value := $0FD2FFC0 ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $11CF8CBC ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $0008DEAB ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $69B70136 ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}

      if _shp.Dimension in [ TGIS_DimensionType.XYZ,TGIS_DimensionType.XYM,TGIS_DimensionType.XYZM ] then begin
        ptg3D := _shp.GetPoint3D( 0, 0 ) ;

        if _shp.Dimension in [ TGIS_DimensionType.XYM ] then begin
        {$IFDEF MANAGED}
          write_double( vr, vr_off, ptg3D.X ) ;
          write_double( vr, vr_off, ptg3D.Y ) ;
          write_double( vr, vr_off, ptg3D.M ) ;
        {$ELSE}
          write_gdo( ptvar, ptg3D.X, 8 ) ;
          write_gdo( ptvar, ptg3D.Y, 8 ) ;
          write_gdo( ptvar, ptg3D.M, 8 ) ;
        {$ENDIF}
        end
        else begin
        {$IFDEF MANAGED}
          write_double( vr, vr_off, ptg3D.X ) ;
          write_double( vr, vr_off, ptg3D.Y ) ;
          write_double( vr, vr_off, ptg3D.Z ) ;
        {$ELSE}
          write_gdo( ptvar, ptg3D, 24 ) ;
        {$ENDIF}
        end ;
      end
      else begin
        ptg := _shp.GetPoint( 0, 0 ) ;
        {$IFDEF MANAGED}
          write_point( vr, vr_off, ptg ) ;
        {$ELSE}
          write_gdo( ptvar, ptg  , sizeOf( ptg ) ) ;
        {$ENDIF}
        {$IFDEF MANAGED}
          write_double( vr, vr_off, 0 ) ;
        {$ELSE}
          dtmp := 0 ;
          write_gdo( ptvar, dtmp, 8 ) ;
        {$ENDIF}
      end ;
      {$IFNDEF MANAGED}
        VarArrayUnLock( _gdo ) ;
      {$ENDIF}
    finally
      _shp.Unlock ;
    end ;

  end ;

  class function TGIS_GeometryFactory.GisExportPointToGML(
    const _shp : TGIS_Shape
  ) : String;
  var
    ptg   : TGIS_Point3D  ;
    coord : String ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;
    if _shp.IsEmpty then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      ptg := _shp.GetPoint3D( 0, 0 ) ;
      case _shp.Dimension of
        TGIS_DimensionType.XYZ  :
          coord := Format( '%s %s %s', [ DotFloatToStr(ptg.X), DotFloatToStr(ptg.Y),
                                         DotFloatToStr(ptg.Z) ]
                          ) ;
        TGIS_DimensionType.XYZM :
          coord := Format( '%s %s %s %s', [ DotFloatToStr(ptg.X), DotFloatToStr(ptg.Y),
                                            DotFloatToStr(ptg.Z), DotFloatToStr(ptg.M) ]
                          )
        else
          coord := Format( '%s %s', [ DotFloatToStr(ptg.X), DotFloatToStr(ptg.Y) ] ) ;
      end ;
      Result := Format( '<gml:Point><gml:pos>%s</gml:pos></gml:Point>',
                        [ coord ]
                      ) ;
    finally
      _shp.Unlock ;
    end ;

  end ;

  class procedure TGIS_GeometryFactory.GisExportPointToEWKB(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  var
    size      : Cardinal   ;
    {$IFDEF OXYGENE}
      byteOrder : array of Byte := new Byte[ sizeOf( Byte ) ] ;
    {$ELSE}
      byteOrder : Byte     ;
    {$ENDIF}
    wkbtype   : DWORD      ;
    {$IFDEF MANAGED}
      vr      : TBytes     ;
      vr_off  : Integer    ;
    {$ELSE}
      ptvar   : Pointer    ;
    {$ENDIF}
    ptg       : TGIS_Point ;
    ptg3D     : TGIS_Point3D ;
    srid      : Integer    ;
  begin
    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if assigned( _shp.Layer ) and assigned( _shp.Layer.CS ) and
         ( _shp.Layer.CS.EPSG  > 0 ) and ( _shp.Layer.CS.EPSG  = _srid ) then
        srid := _shp.Layer.CS.EPSG
      else
        srid := _srid ;
      {$IFDEF OXYGENE}
        byteOrder[0] := 1 ;
      {$ELSE}
        byteOrder    := 1 ;
      {$ENDIF}
      case _shp.Dimension of
        TGIS_DimensionType.XYZ   : begin
          if srid > 0 then begin
            wkbtype :=    1 or EWKB_ZOFFSET_FLAG ;
            size    := OFFSET_EWKB + _shp.PartsSize + _shp.PointsSize +
                       _shp.PointsZSize ;
          end
          else begin
            wkbtype := 1001 ;
            size    := OFFSET_WKB  + _shp.PartsSize + _shp.PointsSize +
                       _shp.PointsZSize ;
          end ;
        end ;
        TGIS_DimensionType.XYM   : begin
          if srid > 0 then begin
            wkbtype :=    1 or EWKB_MOFFSET_FLAG ;
            size    := OFFSET_EWKB + _shp.PartsSize + _shp.PointsSize +
                       _shp.PointsMSize ;
          end
          else begin
            wkbtype := 2001 ;
            size    := OFFSET_WKB  + _shp.PartsSize + _shp.PointsSize +
                       _shp.PointsMSize ;
          end ;
        end ;
        TGIS_DimensionType.XYZM  : begin
          if srid > 0 then begin
            wkbtype :=    1 or EWKB_ZOFFSET_FLAG or EWKB_MOFFSET_FLAG ;
            size    := OFFSET_EWKB + _shp.PartsSize + _shp.PointsSize +
                       _shp.PointsZSize + _shp.PointsMSize ;
          end
          else begin
            wkbtype := 3001 ;
            size    := OFFSET_WKB  + _shp.PartsSize + _shp.PointsSize +
                       _shp.PointsZSize + _shp.PointsMSize ;
          end ;
        end
        else begin
          wkbtype := 1    ;
          if srid > 0 then
            size := OFFSET_EWKB + _shp.PartsSize + _shp.PointsSize
          else
            size := OFFSET_WKB  + _shp.PartsSize + _shp.PointsSize ;
        end ;
      end ;

      _ewkb  := VarArrayCreate([0,size-1], varByte ) ;
      {$IFDEF MANAGED}
        vr     := TBytes(TObject(_ewkb));
        vr_off := 0 ;
      {$ELSE}
        ptvar  := VarArrayLock( _ewkb ) ;
      {$ENDIF}

      {$IFDEF MANAGED}
        write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
        if srid > 0 then begin
          wkbtype := wkbtype or EWKB_SRID_FLAG ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(   srid), sizeOf(DWORD) ) ;
        end
        else
          write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
      {$ELSE}
        write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
        if srid > 0 then begin
          wkbtype := wkbtype or EWKB_SRID_FLAG ;
          write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
          write_wkb( ptvar,    srid, sizeOf( Integer ) ) ;
        end
        else
          write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
      {$ENDIF}

      if _shp.IsEmpty then begin
        {$IFNDEF MANAGED}
          VarArrayUnLock( _ewkb ) ;
        {$ENDIF}
        exit ;
      end ;

      case _shp.Dimension of
        TGIS_DimensionType.XYZ : begin
          ptg3D := _shp.GetPoint3D( 0, 0 ) ;
          {$IFDEF MANAGED}
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.X),
                       sizeOf( Double ) );
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Y),
                       sizeOf( Double ) );
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Z),
                       sizeOf( Double ) );
          {$ELSE}
            write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
            write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
            write_wkb( ptvar, ptg3D.Z, sizeOf( Double ) ) ;
          {$ENDIF}
        end ;
        TGIS_DimensionType.XYM : begin
          ptg3D := _shp.GetPoint3D( 0, 0 ) ;
          {$IFDEF MANAGED}
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.X),
                       sizeOf( Double ) );
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Y),
                       sizeOf( Double ) );
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.M),
                       sizeOf( Double ) );
          {$ELSE}
            write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
            write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
            write_wkb( ptvar, ptg3D.M, sizeOf( Double ) ) ;
          {$ENDIF}
        end ;
        TGIS_DimensionType.XYZM : begin
          ptg3D := _shp.GetPoint3D( 0, 0 ) ;
          {$IFDEF MANAGED}
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.X),
                       sizeOf( Double ) );
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Y),
                       sizeOf( Double ) );
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Z),
                       sizeOf( Double ) );
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.M),
                       sizeOf( Double ) );
          {$ELSE}
            write_wkb( ptvar, ptg3D, SIZEOF_TGIS_POINT3D ) ;
          {$ENDIF}
        end
        else begin
          ptg := _shp.GetPoint( 0, 0 ) ;
          {$IFDEF MANAGED}
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg.X),
                       sizeOf( Double ) );
            write_wkb( vr, vr_off, BitConverter.GetBytes(ptg.Y),
                       sizeOf( Double ) );
          {$ELSE}
            write_wkb( ptvar, ptg, sizeOf( ptg ) ) ;
          {$ENDIF}
        end ;
      end ;

      {$IFNDEF MANAGED}
        VarArrayUnLock( _ewkb ) ;
      {$ENDIF}
    finally
      _shp.Unlock ;
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportPointToEWKB(
    const _shp  : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    if assigned( _shp ) and assigned( _shp.Layer ) then
      GisExportPointToEWKB( _shp, _shp.Layer.CS.EPSG, _ewkb )
    else
      GisExportPointToEWKB( _shp, 0, _ewkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportPointToWKB(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _wkb : Object
    {$ELSE}
      var _wkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    GisExportPointToEWKB( _shp, 0, _wkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportPointToVAR(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _var : Object
    {$ELSE}
      var _var : OleVariant
    {$ENDIF}
  ) ;
  var
    {$IFDEF MANAGED}
      vr     : TBytes  ;
      vr_off : Integer ;
    {$ELSE}
      pbuf   : Pointer ;
    {$ENDIF}
    val      : Double ;
    size     : Integer ;
  begin
    if _shp.IsEmpty then exit ;

    // calculate size
    size := _shp.GeometrySize ;
    if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then
      size := size + _shp.PointsZSize + _shp.PointsMSize
    else if _shp.Dimension in [ TGIS_DimensionType.XYM ] then
      size := size + _shp.PointsMSize ;

    _var := VarArrayCreate([0,size], varByte ) ;
    {$IFDEF MANAGED}
      vr := TBytes(TObject(_var));
      vr_off := 0 ;
    {$ELSE}
      pbuf  := VarArrayLock( _var ) ;
    {$ENDIF}
    try
      {$IFDEF MANAGED}
        write_wkb( vr, vr_off, _shp.Points, _shp.PointsSize ) ;
      {$ELSE}
        write_wkb( pbuf, _shp.Points^, _shp.PointsSize ) ;
      {$ENDIF}

      if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then begin
        val := _shp.GetPoint3D( 0, 0 ).Z ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
      end ;

      if _shp.Dimension in [ TGIS_DimensionType.XYM, TGIS_DimensionType.XYZM ] then begin
        val := _shp.GetPoint3D( 0, 0 ).M ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
      end
      else if ( _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] ) then
      begin
        val := 0 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
      end ;
    finally
      {$IFNDEF MANAGED}
        VarArrayUnlock( _var ) ;
      {$ENDIF}
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportPointToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    const _ver  : Integer
  ) : String ;
  var
    space : String        ;
    ptg   : TGIS_Point    ;
    ptg3D : TGIS_Point3D  ;
  begin
    Result := '' ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if ( _srid > 0 ) then begin
        if ( _shp.Layer         <>   nil ) and
           ( _shp.Layer.CS      <>   nil ) and
           ( _shp.Layer.CS.EPSG  >     0 ) and
           ( _shp.Layer.CS.EPSG  = _srid ) then
          Result := Format( 'SRID=%d;', [_shp.Layer.CS.EPSG] )
        else
          Result := Format( 'SRID=%d;', [_srid] ) ;
      end
      else
        Result := '' ;

      Result := Result + 'POINT%s%s' ;

      if _ver = 0 then space := ''
                  else space := ' ' ;

      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY      : Result := Format( Result, [''   ,''  ] ) ;
        TGIS_DimensionType.XYZ     : Result := Format( Result, [space,'Z' ] ) ;
        TGIS_DimensionType.XYM     : Result := Format( Result, [space,'M' ] ) ;
        TGIS_DimensionType.XYZM    : Result := Format( Result, [space,'ZM'] ) ;
      end ;

      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY  :
          begin
            if _shp.IsEmpty then
              Result := 'POINT EMPTY'
            else begin
              ptg := _shp.GetPoint( 0, 0 ) ;
              Result := Result + Format( '(%s %s)',
                                         [ DotFloatToStr(ptg.X),
                                           DotFloatToStr(ptg.Y)
                                         ]
                                       ) ;
            end ;
          end ;
        TGIS_DimensionType.XYZ :
          begin
            if _shp.IsEmpty then
              Result := 'POINT Z EMPTY'
            else begin
              ptg3D  := _shp.GetPoint3D( 0, 0 ) ;
              Result := Result + Format( '(%s %s %s)',
                                         [ DotFloatToStr(ptg3D.X),
                                           DotFloatToStr(ptg3D.Y),
                                           DotFloatToStr(ptg3D.Z)
                                         ]
                                       ) ;
            end ;
          end ;
        TGIS_DimensionType.XYM :
          begin
            if _shp.IsEmpty then
              Result := 'POINT M EMPTY'
            else begin
              ptg3D  := _shp.GetPoint3D( 0, 0 ) ;
              Result := Result + Format( '(%s %s %s)',
                                         [ DotFloatToStr(ptg3D.X),
                                           DotFloatToStr(ptg3D.Y),
                                           DotFloatToStr(ptg3D.M)
                                         ]
                                       ) ;
            end ;
          end ;
        TGIS_DimensionType.XYZM :
          begin
            if _shp.IsEmpty then
              Result := 'POINT ZM EMPTY'
            else begin
              ptg3D  := _shp.GetPoint3D( 0, 0 ) ;
              Result := Result + Format( '(%s %s %s %s)',
                                         [ DotFloatToStr(ptg3D.X),
                                           DotFloatToStr(ptg3D.Y),
                                           DotFloatToStr(ptg3D.Z),
                                           DotFloatToStr(ptg3D.M)
                                         ]
                                       ) ;
            end ;
          end ;
      end ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportPointToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer
  ) : String ;
  begin
    Result := GisExportPointToEWKT( _shp, _srid, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportPointToEWKT(
    const _shp  : TGIS_Shape
  ) : String ;
  begin
    Result := GisExportPointToEWKT( _shp, 0, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportPointToWKT(
    const _shp : TGIS_Shape
  ) : String ;
  begin
    Result := GisExportPointToEWKT( _shp, 0, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportPointToJSON(
    const _shp : TGIS_Shape
  ) : String ;
  var
    ptg : TGIS_Point3D ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;

    ptg := _shp.GetPoint3D( 0, 0 ) ;

    case _shp.Dimension of
      TGIS_DimensionType.Unknown,
      TGIS_DimensionType.XY   :
        Result := Format( '{"type":"Point","coordinates":[%s,%s]}',
                          [ DotFloatToStr( ptg.X ),
                            DotFloatToStr( ptg.Y )
                          ]
                        ) ;
      TGIS_DimensionType.XYZ  :
        Result := Format( '{"type":"Point","coordinates":[%s,%s,%s]}',
                          [ DotFloatToStr( ptg.X ),
                            DotFloatToStr( ptg.Y ),
                            DotFloatToStr( ptg.Z )
                          ]
                        ) ;
      TGIS_DimensionType.XYM  :
        Result := Format( '{"type":"Point","coordinates":[%s,%s,%s]}',
                          [ DotFloatToStr( ptg.X ),
                            DotFloatToStr( ptg.Y ),
                            DotFloatToStr( ptg.M )
                          ]
                        ) ;
      TGIS_DimensionType.XYZM :
        Result := Format( '{"type":"Point","coordinates":[%s,%s,%s,%s]}',
                          [ DotFloatToStr( ptg.X ),
                            DotFloatToStr( ptg.Y ),
                            DotFloatToStr( ptg.Z ),
                            DotFloatToStr( ptg.M )
                          ]
                        ) ;
    end ;

  end ;

  class procedure TGIS_GeometryFactory.GisExportPolygonToGDO(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _gdo : Object
    {$ELSE}
      var _gdo : OleVariant
    {$ENDIF}
  ) ;
  {$IFNDEF OXYGENE}
    {$HINTS OFF}  // for some reasons a number of variables are hinted as unused
  {$ENDIF}
    var
    size      : Cardinal      ;
    count     : DWORD         ;
    value     : DWORD         ;
    {$IFDEF MANAGED}
      vr        : TBytes      ;
      vr_off    : Integer     ;
      vr_ext    : Integer     ;
      vr_int    : Integer     ;
    {$ELSE}
      ptvar     : Pointer     ;
      ptvar_ext : Pointer     ;
      ptvar_int : Pointer     ;
      dtmp      : Double      ;
    {$ENDIF}
    ptg       : TGIS_Point    ;
    ptg3D     : TGIS_Point3D  ;
    p, points : DWORD         ;
    r, rings  : DWORD         ;
    num       : DWORD         ;
    part_no   : DWORD         ;
    ext_size  : DWORD         ;
    int_size  : DWORD         ;
    int_count : DWORD         ;
    fixed     : Boolean       ;
    shp       : TGIS_ShapePolygon ;

    {$IFDEF MANAGED}
      procedure skip_bytes(
        var   _o     : Integer ;
        const _count : Cardinal
      ) ;
      begin
        _o := Cardinal( _o ) + _count ;
      end ;
    {$ELSE}
      procedure skip_bytes(
        var   _p     : Pointer ;
        const _count : Cardinal
      ) ;
      begin
        _p := Pointer( NativeInt( _p ) + Integer(_count) ) ;
      end ;
    {$ENDIF}

    function fixPolygon(
      var _fixed : Boolean
    ) : TGIS_ShapePolygon ;
    var
      tpl : TGIS_Topology ;
    begin
      tpl := TGIS_Topology.Create ;
      try
        Result := TGIS_ShapePolygon( tpl.FixShapeEx( _shp, True, _fixed ) ) ;
      finally
        FreeObject( tpl ) ;
      end ;
    end ;

    function isExtContainIntPart(
      const _extPartNo : Integer ;
      const _intPartNo : Integer
    ) : Boolean ;
    var
      point_no     : Integer ;
      points_count : Integer ;
      next_pt      : Integer ;
      npar, p1     : Integer ;
      line_a       : TGIS_Point ;
      line_b       : TGIS_Point ;
      ptg1         : TGIS_Point ;
    begin
      Result := False ;

      for p1 := 0 to shp.GetPartSize( _intPartNo )-1 do begin

        ptg1 := shp.GetPoint( _intPartNo, p1 ) ;
        with shp.Extent do begin
          if (XMax   = Xmin) and (YMax   = YMin) and
             (ptg1.X = Xmin) and (ptg1.X = YMin) then begin
            Result := True ;
            exit ;
          end ;
        end ;

        //for count := start to stop do begin
        points_count := shp.GetPartSize( _extPartNo ) ;
        next_pt := points_count - 1 ;
        npar := 0 ;
        for point_no:=0 to points_count-1 do  begin // all points
          // point will be tested on every line of polygon
          // test is based on odd/even algorithm
          line_a := shp.GetPoint( _extPartNo, point_no ) ;
          line_b := shp.GetPoint( _extPartNo, next_pt  ) ;
          next_pt := point_no ;
          try
            if ( ( ( ( line_a.Y <= ptg1.Y ) and ( ptg1.Y < line_b.Y ) ) or
                   ( ( line_b.Y <= ptg1.Y ) and ( ptg1.Y < line_a.Y ) )
                 ) and
                 ( ptg1.X < ( line_b.X - line_a.X ) * ( ptg1.Y - line_a.Y ) /
                            ( line_b.Y - line_a.Y ) + line_a.X
                 )
               ) then
              npar := npar + 1 ;
          except
          end ;
        end ;

        if ( npar mod 2) = 1 then
          Result := True ; // even - point is inside part
        //end ;

        if not Result then Break ;

      end ;
    end ;

    procedure calcBoundary(
      const _partNo   : DWORD ;
      var   _extSize  : DWORD ;
      var   _intSize  : DWORD ;
      var   _intCount : DWORD
    ) ;
    var
       k          : DWORD   ;
       partPoints : DWORD   ;
    begin
      if ( _partNo > rings-1 ) then exit ;

      // size of Exterior geometry record
      _extSize  := 0 ;
      // size of Holes geometry record
      _intSize  := 0 ;
      // count of holes
      _intCount := 0 ;

      for k:=_partNo to rings-1 do begin
        partPoints := shp.GetPartSize( k ) ;

        if ( k = _partNo ) then begin
          // size of Polygon Geometry record - Exterior geometry record
          _extSize := _extSize +
                      ( OFFSET_GDO_GUID +
                        sizeOf( DWORD ) + partPoints * OFFSET_GDO_POINT
                      ) ;
        end
        else if isExtContainIntPart( _partNo, k ) then begin
          inc( _intCount ) ;
          // size of Geometry Collection record - Holes geometry record
          _intSize := _intSize +
                      ( sizeOf( DWORD ) + OFFSET_GDO_GUID +
                        sizeOf( DWORD ) + partPoints * OFFSET_GDO_POINT
                      ) ;
        end
        else
          Break ;
      end ;

      if _intCount > 0 then
        _intSize := _intSize +
                    OFFSET_GDO_GUID + sizeOf( DWORD ) ;
    end ;

    procedure calcCollection(
      var _size  : DWORD ;
      var _count : DWORD
    ) ;
    var
       k           : DWORD   ;
       extNo       : DWORD   ;
       partPoints  : DWORD   ;
       firstHole   : Boolean ;
       newExterior : Boolean ;
    begin
      _count    := 0 ;
      extNo     := 0 ;
      firstHole := True;
      // size of Geometry Collection record
      _size := OFFSET_GDO_GUID + sizeOf( DWORD ) ;

      newExterior := True ;

      for k:=0 to rings-1 do begin

        if newExterior then begin
          extNo := k ;
          inc( _count ) ;
          partPoints := shp.GetPartSize( k ) ;
          // size of Polygon Geometry record or Exterior geometry record
          _size := _size + ( sizeOf( DWORD ) + OFFSET_GDO_GUID +
                             sizeOf( DWORD ) + partPoints * OFFSET_GDO_POINT
                           ) ;
          newExterior := False ;
          firstHole   := True ;
        end
        else if isExtContainIntPart( extNo, k ) then begin

          if firstHole then _size := _size + 44 ;
          partPoints := shp.GetPartSize( k ) ;
          // size of Geometry Collection record - Holes geometry record
          _size := _size + ( sizeOf( DWORD ) + OFFSET_GDO_GUID +
                             sizeOf( DWORD ) + partPoints * OFFSET_GDO_POINT
                           ) ;
          firstHole   := False ;
        end ;

        if k < rings-1 then
          newExterior := not isExtContainIntPart( extNo, k+1 ) ;
      end ;

    end ;

  begin
    if not assigned( _shp ) then exit ;
    if _shp.IsEmpty then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      // Compute a polygon without self crossings (loops) and with a proper
      // winding (clockwise, and opposite for holes).
      // AR - this sometimes changes order and winding of parts improperly
      shp := fixPolygon( fixed ) ;

      points := 0           ;  // only to turn off hint messages ;
      rings  := shp.GetNumParts ;

      // some invalid shapes loose points after fixPolygon
      if rings = 0 then begin
        _gdo  := VarArrayCreate([0, 0], varByte ) ;
        exit ;
      end ;

      if ( rings = 1 ) then begin
        // write Polygon Geometry record
        points := shp.GetPartSize( 0 ) ;

        size := OFFSET_GDO_GUID + sizeOf( DWORD ) +
                points * OFFSET_GDO_POINT ;

        _gdo  := VarArrayCreate([0, size-1], varByte ) ;

        {$IFDEF MANAGED}
          vr := TBytes(TObject(_gdo));
          vr_off := 0  ;
        {$ELSE}
          ptvar := VarArrayLock( _gdo ) ;
        {$ENDIF}

        // Polygon Geometry
        // GUID: "0FD2FFC3-8CBC-11CF-DEAB-08003601B769"
        value := $0FD2FFC3 ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $11CF8CBC ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $0008DEAB ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $69B70136 ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        value := points ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        for p := 0 to points - 1 do begin
          if _shp.Dimension in [ TGIS_DimensionType.XYZ,TGIS_DimensionType.XYM,TGIS_DimensionType.XYZM ] then
          begin
            ptg3D := _shp.GetPoint3D( 0, p ) ;

            if _shp.Dimension in [ TGIS_DimensionType.XYM ] then begin
            {$IFDEF MANAGED}
              write_double( vr, vr_off, ptg3D.X ) ;
              write_double( vr, vr_off, ptg3D.Y ) ;
              write_double( vr, vr_off, ptg3D.M ) ;
            {$ELSE}
              write_gdo( ptvar, ptg3D.X, 8 ) ;
              write_gdo( ptvar, ptg3D.Y, 8 ) ;
              write_gdo( ptvar, ptg3D.M, 8 ) ;
            {$ENDIF}
            end
            else begin
            {$IFDEF MANAGED}
              write_double( vr, vr_off, ptg3D.X ) ;
              write_double( vr, vr_off, ptg3D.Y ) ;
              write_double( vr, vr_off, ptg3D.Z ) ;
            {$ELSE}
              write_gdo( ptvar, ptg3D, 24 ) ;
            {$ENDIF}
            end ;
          end
          else begin
            ptg := shp.GetPoint( 0, p ) ;
            {$IFDEF MANAGED}
              write_point( vr, vr_off, ptg ) ;
              write_double( vr, vr_off, 0 ) ;
            {$ELSE}
              write_gdo( ptvar, ptg  , sizeOf( ptg ) ) ;
              dtmp := 0 ;
              write_gdo( ptvar, dtmp, 8 ) ;
            {$ENDIF}
          end ;
        end ;
        {$IFNDEF MANAGED}
          VarArrayUnLock( _gdo ) ;
        {$ENDIF}
      end
      else begin
      // MultiPolygon Geometry record with or without holes

        // Calculation of overall Geometry Collection size
        calcCollection( size, count ) ;

        // write Geometry Collection record
        _gdo  := VarArrayCreate([0, size-1], varByte ) ;
        {$IFDEF MANAGED}
          vr := TBytes(TObject(_gdo));
          vr_off := 0  ;
        {$ELSE}
          ptvar := VarArrayLock( _gdo ) ;
        {$ENDIF}

        // Geometry Collection
        // GUID: "0FD2FFC6-8CBC-11CF-DEAB-08003601B769"
        value := $0FD2FFC6 ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $11CF8CBC ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $0008DEAB ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}
        value := $69B70136 ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        value := count ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        part_no := 0 ;
        for num := 1 to count do begin
          // Calculation parameters of one item from Geometry Collection
          calcBoundary( part_no, ext_size, int_size, int_count ) ;

          // MultiPolygon Geometry record with holes
          if ( int_count > 0 ) then begin
            // Write Item size
            value :=  24 + ext_size + int_size ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_off, value ) ;
            {$ELSE}
              write_gdo( ptvar, value, 4 ) ;
            {$ENDIF}

            // Write Boundary Geometry
            {$IFDEF MANAGED}
              vr_ext := vr_off ;
              vr_int := vr_ext ;
              skip_bytes( vr_int,
                          OFFSET_GDO_GUID + sizeOf( DWORD ) + ext_size
                        ) ;
            {$ELSE}
              ptvar_ext := ptvar ;
              ptvar_int := ptvar_ext ;
              skip_bytes( ptvar_int,
                          OFFSET_GDO_GUID + sizeOf( DWORD ) + ext_size
                        ) ;
            {$ENDIF}

            // Boundary Geometry
            // GUID: "0FD2FFC5-8CBC-11CF-DEAB-08003601B769"
            value := $0FD2FFC5 ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_ext, value ) ;
            {$ELSE}
              write_gdo( ptvar_ext, value, 4 ) ;
            {$ENDIF}
            value := $11CF8CBC ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_ext, value ) ;
            {$ELSE}
              write_gdo( ptvar_ext, value, 4 ) ;
            {$ENDIF}
            value := $0008DEAB ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_ext, value ) ;
            {$ELSE}
              write_gdo( ptvar_ext, value, 4 ) ;
            {$ENDIF}
            value := $69B70136 ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_ext, value ) ;
            {$ELSE}
              write_gdo( ptvar_ext, value, 4 ) ;
            {$ENDIF}

            // Size of Exterior Geometry BLOB
            value := ext_size ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_ext, value ) ;
            {$ELSE}
              write_gdo( ptvar_ext, value, 4 ) ;
            {$ENDIF}

            // Size of Interior Geometry BLOB
            value := int_size ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_int, value ) ;
            {$ELSE}
              write_gdo( ptvar_int, value, 4 ) ;
            {$ENDIF}

            // Geometry Collection - Holes geometry record
            // GUID: "0FD2FFC6-8CBC-11CF-DEAB-08003601B769"
            value := $0FD2FFC6 ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_int, value ) ;
            {$ELSE}
              write_gdo( ptvar_int, value, 4 ) ;
            {$ENDIF}
            value := $11CF8CBC ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_int, value ) ;
            {$ELSE}
              write_gdo( ptvar_int, value, 4 ) ;
            {$ENDIF}
            value := $0008DEAB ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_int, value ) ;
            {$ELSE}
              write_gdo( ptvar_int, value, 4 ) ;
            {$ENDIF}
            value := $69B70136 ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_int, value ) ;
            {$ELSE}
              write_gdo( ptvar_int, value, 4 ) ;
            {$ENDIF}

            value := int_count ;

            {$IFDEF MANAGED}
              write_dword( vr, vr_int, value ) ;
            {$ELSE}
              write_gdo( ptvar_int, value, 4 ) ;
            {$ENDIF}

            for r := part_no to part_no + int_count do begin

              if ( r = part_no ) then begin

                points := shp.GetPartSize( r ) ;

                // Polygon Geometry
                // GUID: "0FD2FFC3-8CBC-11CF-DEAB-08003601B769"
                value := $0FD2FFC3 ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_ext, value ) ;
                {$ELSE}
                  write_gdo( ptvar_ext, value, 4 ) ;
                {$ENDIF}
                value := $11CF8CBC ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_ext, value ) ;
                {$ELSE}
                  write_gdo( ptvar_ext, value, 4 ) ;
                {$ENDIF}
                value := $0008DEAB ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_ext, value ) ;
                {$ELSE}
                  write_gdo( ptvar_ext, value, 4 ) ;
                {$ENDIF}
                value := $69B70136 ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_ext, value ) ;
                {$ELSE}
                  write_gdo( ptvar_ext, value, 4 ) ;
                {$ENDIF}

                value := points ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_ext, value ) ;
                {$ELSE}
                  write_gdo( ptvar_ext, value, 4 ) ;
                {$ENDIF}

                for p := 0 to points-1 do begin
                  if _shp.Dimension in [ TGIS_DimensionType.XYZ,TGIS_DimensionType.XYM,TGIS_DimensionType.XYZM ]
                  then begin
                    ptg3D := _shp.GetPoint3D( r, p ) ;

                    if _shp.Dimension in [ TGIS_DimensionType.XYM ] then begin
                    {$IFDEF MANAGED}
                      write_double( vr, vr_off, ptg3D.X ) ;
                      write_double( vr, vr_off, ptg3D.Y ) ;
                      write_double( vr, vr_off, ptg3D.M ) ;
                    {$ELSE}
                      write_gdo( ptvar, ptg3D.X, 8 ) ;
                      write_gdo( ptvar, ptg3D.Y, 8 ) ;
                      write_gdo( ptvar, ptg3D.M, 8 ) ;
                    {$ENDIF}
                    end
                    else begin
                    {$IFDEF MANAGED}
                      write_double( vr, vr_off, ptg3D.X ) ;
                      write_double( vr, vr_off, ptg3D.Y ) ;
                      write_double( vr, vr_off, ptg3D.Z ) ;
                    {$ELSE}
                      write_gdo( ptvar, ptg3D, 24 ) ;
                    {$ENDIF}
                    end ;
                  end
                  else begin
                    ptg := shp.GetPoint( r, p ) ;
                    {$IFDEF MANAGED}
                      write_point( vr, vr_ext, ptg ) ;
                      write_double( vr, vr_ext, 0 ) ;
                    {$ELSE}
                      write_gdo( ptvar_ext, ptg, sizeOf( ptg ) ) ;
                      dtmp := 0;
                      write_gdo( ptvar_ext, dtmp, 8 ) ;
                    {$ENDIF}
                  end ;
                end ;

              end
              else if isExtContainIntPart( part_no, r ) then begin

                points := shp.GetPartSize( r ) ;

                // size of Polygon Geometry record
                value := OFFSET_GDO_GUID + sizeOf( DWORD ) +
                         points * OFFSET_GDO_POINT ;

                {$IFDEF MANAGED}
                  write_dword( vr, vr_int, value ) ;
                {$ELSE}
                  write_gdo( ptvar_int, value, 4 ) ;
                {$ENDIF}

                // Polygon Geometry
                // GUID: "0FD2FFC3-8CBC-11CF-DEAB-08003601B769"
                value := $0FD2FFC3 ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_int, value ) ;
                {$ELSE}
                  write_gdo( ptvar_int, value, 4 ) ;
                {$ENDIF}
                value := $11CF8CBC ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_int, value ) ;
                {$ELSE}
                  write_gdo( ptvar_int, value, 4 ) ;
                {$ENDIF}
                value := $0008DEAB ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_int, value ) ;
                {$ELSE}
                  write_gdo( ptvar_int, value, 4 ) ;
                {$ENDIF}
                value := $69B70136 ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_int, value ) ;
                {$ELSE}
                  write_gdo( ptvar_int, value, 4 ) ;
                {$ENDIF}

                value := points ;
                {$IFDEF MANAGED}
                  write_dword( vr, vr_int, value ) ;
                {$ELSE}
                  write_gdo( ptvar_int, value, 4 ) ;
                {$ENDIF}

                for p := 0 to points-1 do begin
                  if _shp.Dimension in [ TGIS_DimensionType.XYZ,TGIS_DimensionType.XYM,TGIS_DimensionType.XYZM ] then begin
                    ptg3D := _shp.GetPoint3D( r, p ) ;

                    if _shp.Dimension in [ TGIS_DimensionType.XYM ] then begin
                    {$IFDEF MANAGED}
                      write_double( vr, vr_off, ptg3D.X ) ;
                      write_double( vr, vr_off, ptg3D.Y ) ;
                      write_double( vr, vr_off, ptg3D.M ) ;
                    {$ELSE}
                      write_gdo( ptvar, ptg3D.X, 8 ) ;
                      write_gdo( ptvar, ptg3D.Y, 8 ) ;
                      write_gdo( ptvar, ptg3D.M, 8 ) ;
                    {$ENDIF}
                    end
                    else begin
                    {$IFDEF MANAGED}
                      write_double( vr, vr_off, ptg3D.X ) ;
                      write_double( vr, vr_off, ptg3D.Y ) ;
                      write_double( vr, vr_off, ptg3D.Z ) ;
                    {$ELSE}
                      write_gdo( ptvar, ptg3D, 24 ) ;
                    {$ENDIF}
                    end ;
                  end
                  else begin
                    ptg := shp.GetPoint( r, p ) ;
                    {$IFDEF MANAGED}
                      write_point( vr, vr_int, ptg ) ;
                      write_double( vr, vr_int, 0 ) ;
                    {$ELSE}
                      write_gdo( ptvar_int, ptg, sizeOf( ptg ) ) ;
                      dtmp := 0;
                      write_gdo( ptvar_int, dtmp, 8 ) ;
                    {$ENDIF}
                  end ;
                end ;
              end ;

            end ;
            {$IFDEF MANAGED}
              vr_off := vr_int ;
            {$ELSE}
              ptvar := ptvar_int ;
            {$ENDIF}
          end
          else begin
            // MultiPolygon Geometry record without holes

            // Write Item size
            value :=  ext_size ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_off, value ) ;
            {$ELSE}
              write_gdo( ptvar, value, 4 ) ;
            {$ENDIF}

            points := shp.GetPartSize( part_no ) ;

            // Polygon Geometry
            // GUID: "0FD2FFC3-8CBC-11CF-DEAB-08003601B769"
            value := $0FD2FFC3 ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_off, value ) ;
            {$ELSE}
              write_gdo( ptvar, value, 4 ) ;
            {$ENDIF}
            value := $11CF8CBC ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_off, value ) ;
            {$ELSE}
              write_gdo( ptvar, value, 4 ) ;
            {$ENDIF}
            value := $0008DEAB ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_off, value ) ;
            {$ELSE}
              write_gdo( ptvar, value, 4 ) ;
            {$ENDIF}
            value := $69B70136 ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_off, value ) ;
            {$ELSE}
              write_gdo( ptvar, value, 4 ) ;
            {$ENDIF}

            value := points ;
            {$IFDEF MANAGED}
              write_dword( vr, vr_off, value ) ;
            {$ELSE}
              write_gdo( ptvar, value, 4 ) ;
            {$ENDIF}

            for p := 0 to points - 1 do begin
              if _shp.Dimension in [ TGIS_DimensionType.XYZ,TGIS_DimensionType.XYM,TGIS_DimensionType.XYZM ]
              then begin
                ptg3D := _shp.GetPoint3D( part_no, p ) ;

                if _shp.Dimension in [ TGIS_DimensionType.XYM ] then begin
                {$IFDEF MANAGED}
                  write_double( vr, vr_off, ptg3D.X ) ;
                  write_double( vr, vr_off, ptg3D.Y ) ;
                  write_double( vr, vr_off, ptg3D.M ) ;
                {$ELSE}
                  write_gdo( ptvar, ptg3D.X, 8 ) ;
                  write_gdo( ptvar, ptg3D.Y, 8 ) ;
                  write_gdo( ptvar, ptg3D.M, 8 ) ;
                {$ENDIF}
                end
                else begin
                {$IFDEF MANAGED}
                  write_double( vr, vr_off, ptg3D.X ) ;
                  write_double( vr, vr_off, ptg3D.Y ) ;
                  write_double( vr, vr_off, ptg3D.Z ) ;
                {$ELSE}
                  write_gdo( ptvar, ptg3D, 24 ) ;
                {$ENDIF}
                end ;
              end
              else begin
                ptg := shp.GetPoint( part_no, p ) ;
                {$IFDEF MANAGED}
                  write_point( vr, vr_off, ptg ) ;
                  write_double( vr, vr_off, 0 ) ;
                {$ELSE}
                  write_gdo( ptvar, ptg  , sizeOf( ptg ) ) ;
                  dtmp := 0;
                  write_gdo( ptvar, dtmp, 8 ) ;
                {$ENDIF}
              end ;
            end ;
          end ;
          part_no := part_no + int_count + 1 ;
        end ;
        {$IFNDEF MANAGED}
          VarArrayUnLock( _gdo ) ;
        {$ENDIF}
      end ;
    finally
      FreeObject( shp );
      _shp.Unlock ;
    end ;
  end ;
  {$IFNDEF OXYGENE}
    {$HINTS ON}
  {$ENDIF}

  class function TGIS_GeometryFactory.GisExportPolygonToGML(
    const _shp : TGIS_Shape
  ) : String;
    var
      count     : DWORD         ;
      p, points : DWORD         ;
      r, rings  : DWORD         ;
      num       : DWORD         ;
      part_no   : DWORD         ;
      int_count : DWORD         ;
      sb        : TStringBuilder ;

    function isExtContainIntPart(
      const _extPartNo : Integer ;
      const _intPartNo : Integer
    ) : Boolean ;
    var
      point_no     : Integer ;
      points_count : Integer ;
      next_pt      : Integer ;
      npar, p1     : Integer ;
      line_a       : TGIS_Point ;
      line_b       : TGIS_Point ;
      ptg1         : TGIS_Point ;
    begin
      Result := False ;

      for p1 := 0 to _shp.GetPartSize( _intPartNo )-1 do begin

        ptg1 := _shp.GetPoint( _intPartNo, p1 ) ;
        with _shp.Extent do begin
          if (XMax   = Xmin) and (YMax   = YMin) and
             (ptg1.X = Xmin) and (ptg1.X = YMin) then begin
            Result := True ;
            exit ;
          end ;
        end ;

        //for count := start to stop do begin
        points_count := _shp.GetPartSize( _extPartNo ) ;
        next_pt := points_count - 1 ;
        npar := 0 ;
        for point_no:=0 to points_count-1 do  begin // all points
          // point will be tested on every line of polygon
          // test is based on odd/even algorithm
          line_a := _shp.GetPoint( _extPartNo, point_no ) ;
          line_b := _shp.GetPoint( _extPartNo, next_pt  ) ;
          next_pt := point_no ;
          try
            if ( ( ( ( line_a.Y <= ptg1.Y ) and ( ptg1.Y < line_b.Y ) ) or
                   ( ( line_b.Y <= ptg1.Y ) and ( ptg1.Y < line_a.Y ) )
                 ) and
                 ( ptg1.X < ( line_b.X - line_a.X ) * ( ptg1.Y - line_a.Y ) /
                            ( line_b.Y - line_a.Y ) + line_a.X
                 )
               ) then
              npar := npar + 1 ;
          except
          end ;
        end ;

        if ( npar mod 2) = 1 then
          Result := True ; // even - point is inside part

        if not Result then Break ;
      end ;
    end ;

    procedure calcBoundary( const _partNo   : DWORD ;
                            var   _intCount : DWORD
                          ) ;
    var
       k          : DWORD   ;
    begin
      if ( _partNo > rings-1 ) then exit ;
      _intCount := 0 ;
      for k := _partNo to rings-1 do begin
        if ( k = _partNo ) then
        else if isExtContainIntPart( _partNo, k ) then begin
          inc( _intCount ) ;
        end
        else
          Break ;
      end ;
    end ;

    procedure calcCollection( var _count : DWORD ) ;
    var
       k           : DWORD   ;
       extNo       : DWORD   ;
       newExterior : Boolean ;
    begin
      _count      := 0 ;
      extNo       := 0 ;
      newExterior := True ;
      for k := 0 to rings-1 do begin
        if newExterior then begin
          extNo := k ;
          inc( _count ) ;
          newExterior := False ;
        end
        else if isExtContainIntPart( extNo, k ) then begin
        end ;
        if k < rings-1 then
          newExterior := not isExtContainIntPart( extNo, k+1 ) ;
      end ;
    end ;

    procedure appendCoordinate( const _i : Integer ;
                                const _j : Integer
                              ) ;
    var
      ptg   : TGIS_Point    ;
      ptg3D : TGIS_Point3D  ;
    begin
      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY :
          begin
            ptg := _shp.GetPoint( _i, _j ) ;
            sb.Append( Format( '%s %s',
                              [ DotFloatToStr(ptg.X),
                                DotFloatToStr(ptg.Y) ]
                             )
                       ) ;
          end ;
        TGIS_DimensionType.XYZ :
          begin
            ptg3D  := _shp.GetPoint3D( _i, _j ) ;
            sb.Append( Format( '%s %s %s',
                               [ DotFloatToStr(ptg3D.X),
                                 DotFloatToStr(ptg3D.Y),
                                 DotFloatToStr(ptg3D.Z) ]
                             )
                      ) ;
          end ;
        TGIS_DimensionType.XYM :
          begin
            ptg3D  := _shp.GetPoint3D( _i, _j ) ;
            sb.Append( Format( '%s %s %s',
                              [ DotFloatToStr(ptg3D.X),
                                DotFloatToStr(ptg3D.Y),
                                DotFloatToStr(ptg3D.M)
                              ]
                            )
                      ) ;
          end ;
        TGIS_DimensionType.XYZM :
          begin
            ptg3D  := _shp.GetPoint3D( _i, _j ) ;
            sb.Append( Format( '%s %s %s %s',
                              [ DotFloatToStr(ptg3D.X),
                                DotFloatToStr(ptg3D.Y),
                                DotFloatToStr(ptg3D.Z),
                                DotFloatToStr(ptg3D.M)
                              ]
                            )
                      ) ;
          end ;
      end ;
    end ;

    function setDimension( const _name : String ) : String ;
    begin
      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XYM,
        TGIS_DimensionType.XY      : Result := Format( _name, [''  ] ) ;
        TGIS_DimensionType.XYZ     : Result := Format( _name, [' srsDimension="3"' ] ) ;
        TGIS_DimensionType.XYZM    : Result := Format( _name, [' srsDimension="4"' ] ) ;
      end ;
    end ;

    procedure exportPolygon ;
    var
      i     : Integer      ;
      icnt  : Integer      ;
      j     : Integer      ;
      jcnt  : Integer      ;
    begin
      if _shp.IsEmpty then exit ;

      icnt := _shp.GetNumParts - 1 ;
      sb.Append( '<gml:Polygon>' ) ;

      for i:= 0 to icnt do begin
        if i = 0 then
          sb.Append( '<gml:exterior>' )
        else
          sb.Append( '<gml:interior>' ) ;

        sb.Append( '<gml:LinearRing>'+setDimension('<gml:posList%s>') ) ;
        jcnt := _shp.GetPartSize( i ) - 1 ;
        for j:= 0 to jcnt do begin
          appendCoordinate( i, j ) ;
          if j <> jcnt then
            sb.Append( ' ' )
        end ;
        sb.Append( '</gml:posList></gml:LinearRing>' ) ;
        if i = 0 then
          sb.Append( '</gml:exterior>' )
        else
          sb.Append( '</gml:interior>' ) ;
      end ;
      sb.Append( '</gml:Polygon>' ) ;
    end ;

  begin
    if not assigned( _shp ) then exit ;
    if _shp.IsEmpty then exit ;

    sb := TStringBuilder.Create ;
    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      rings  := _shp.GetNumParts ;

      if ( rings = 1 ) then begin
        // write Polygon Geometry record
       exportPolygon ;
      end
      else begin
        calcCollection( count ) ;
        // write Polygon Geometry record
        if count = 1 then begin
          exportPolygon ;
          exit ;
        end ;

        // MultiPolygon Geometry record with or without holes
        sb.Append( '<gml:MultiPolygon>' ) ;
        // Geometry Collection
        part_no := 0 ;
        for num := 1 to count do begin
          // Calculation parameters of one item from Geometry Collection
          calcBoundary( part_no, int_count ) ;

          // Polygon Geometry record with holes
          if ( int_count > 0 ) then begin
            sb.Append( '<gml:polygonMember><gml:Polygon>' ) ;

            for r := part_no to part_no + int_count do begin
              if ( r = part_no ) then begin
                sb.Append( '<gml:exterior><gml:LinearRing>'+setDimension('<gml:posList%s>') ) ;
                points := _shp.GetPartSize( r ) ;
                // Polygon Geometry
                for p := 0 to points-1 do begin
                  appendCoordinate( r, p ) ;
                  if p <> points-1 then
                    sb.Append( ' ' )
                end ;
                sb.Append( '</gml:posList></gml:LinearRing></gml:exterior>' ) ;
              end
              else if isExtContainIntPart( part_no, r ) then begin
                sb.Append( '<gml:interior><gml:LinearRing>'+setDimension('<gml:posList%s>') ) ;
                points := _shp.GetPartSize( r ) ;
                // Polygon Geometry
                for p := 0 to points-1 do begin
                  appendCoordinate( r, p ) ;
                  if p <> points-1 then
                    sb.Append( ' ' )
                end ;
                sb.Append( '</gml:posList></gml:LinearRing></gml:interior>' ) ;
              end ;
            end ;
            sb.Append( '</gml:Polygon></gml:polygonMember>' ) ;
          end
          else begin
            // Polygon Geometry record without holes
            sb.Append( '<gml:polygonMember><gml:Polygon>' ) ;
            sb.Append( '<gml:exterior><gml:LinearRing>'+setDimension('<gml:posList%s>') ) ;
            points := _shp.GetPartSize( part_no ) ;
            // Polygon Geometry
            for p := 0 to points - 1 do begin
              appendCoordinate( part_no, p ) ;
              if p <> points-1 then
                sb.Append( ' ' )
            end ;
            sb.Append( '</gml:posList></gml:LinearRing></gml:exterior>' ) ;
            sb.Append( '</gml:Polygon></gml:polygonMember>' ) ;
          end ;
          part_no := part_no + int_count + 1 ;
        end ;
        sb.Append( '</gml:MultiPolygon>' ) ;
      end ;
    finally
      Result := sb.ToString ;
      FreeObject( sb ) ;
      _shp.Unlock ;
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportPolygonToEWKB(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    const _multi : Boolean    ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  var
    size        : Cardinal ;
    {$IFDEF OXYGENE}
      byteOrder : array of Byte := new Byte[ sizeOf( Byte ) ] ;
    {$ELSE}
      byteOrder : Byte     ;
    {$ENDIF}
    wkbtype    : DWORD     ;
    {$IFDEF MANAGED}
      vr       : TBytes    ;
      vr_off   : Integer   ;
    {$ELSE}
      ptvar    : Pointer   ;
    {$ENDIF}
    pn, points : DWORD     ;
    rn, rings  : DWORD     ;
    count      : DWORD     ;
    num        : DWORD     ;
    part_no    : DWORD     ;
    int_count  : DWORD     ;
    srid       : Integer   ;

    function isExtContainIntPart(
      const _extPartNo : Integer ;
      const _intPartNo : Integer
    ) : Boolean ;
    var
      point_no     : Integer ;
      points_count : Integer ;
      next_pt      : Integer ;
      npar, p1     : Integer ;
      line_a       : TGIS_Point ;
      line_b       : TGIS_Point ;
      ptg1         : TGIS_Point ;
    begin
      Result := False ;

      for p1 := 0 to _shp.GetPartSize( _intPartNo )-1 do begin

        ptg1 := _shp.GetPoint( _intPartNo, p1 ) ;
        with _shp.Extent do begin
          if (XMax   = Xmin) and (YMax   = YMin) and
             (ptg1.X = Xmin) and (ptg1.X = YMin) then begin
            Result := True ;
            exit ;
          end ;
        end ;

        //for count := start to stop do begin
        points_count := _shp.GetPartSize( _extPartNo ) ;
        next_pt := points_count - 1 ;
        npar := 0 ;
        for point_no:=0 to points_count-1 do  begin // all points
          // point will be tested on every line of polygon
          // test is based on odd/even algorithm
          line_a := _shp.GetPoint( _extPartNo, point_no ) ;
          line_b := _shp.GetPoint( _extPartNo, next_pt  ) ;
          next_pt := point_no ;
          try
            if ( ( ( ( line_a.Y <= ptg1.Y ) and ( ptg1.Y < line_b.Y ) ) or
                   ( ( line_b.Y <= ptg1.Y ) and ( ptg1.Y < line_a.Y ) )
                 ) and
                 ( ptg1.X < ( line_b.X - line_a.X ) * ( ptg1.Y - line_a.Y ) /
                            ( line_b.Y - line_a.Y ) + line_a.X
                 )
               ) then
              npar := npar + 1 ;
          except
          end ;
        end ;

        if ( npar mod 2) = 1 then
          Result := True ; // even - point is inside part

        if not Result then Break ;
      end ;
    end ;

    procedure calcBoundary( const _partNo   : DWORD ;
                            var   _intCount : DWORD
                          ) ;
    var
       k          : DWORD   ;
    begin
      if ( _partNo > rings-1 ) then exit ;
      _intCount := 0 ;
      for k := _partNo to rings-1 do begin
        if ( k = _partNo ) then
        else if isExtContainIntPart( _partNo, k ) then begin
          inc( _intCount ) ;
        end
        else
          Break ;
      end ;
    end ;

    function calcPartSize( const _part : Integer ) : DWORD ;
    begin
      points  := _shp.GetPartSize( _part ) ;
      case _shp.Dimension of
        TGIS_DimensionType.XYZ   : begin
          if srid > 0 then
            wkbtype :=    3 or EWKB_ZOFFSET_FLAG
          else
            wkbtype := 1003 ;
          Result := sizeOf(DWORD)+points*(SIZEOF_TGIS_POINT+sizeOf(Double)) ;
        end ;
        TGIS_DimensionType.XYM   : begin
          if srid > 0 then
            wkbtype :=    3 or EWKB_MOFFSET_FLAG
          else
            wkbtype := 2003 ;
          Result := sizeOf(DWORD)+points*(SIZEOF_TGIS_POINT+sizeOf(Double)) ;
        end ;
        TGIS_DimensionType.XYZM  : begin
          if srid > 0 then
            wkbtype :=    3 or EWKB_ZOFFSET_FLAG or EWKB_MOFFSET_FLAG
          else
            wkbtype := 3003 ;
          Result := sizeOf(DWORD)+points*SIZEOF_TGIS_POINT3D ;
        end
        else begin
          wkbtype := 3 ;
          Result  := sizeOf(DWORD)+points*SIZEOF_TGIS_POINT;
        end ;
      end ;

    end ;

    procedure calcCollection( var _count : DWORD ;
                              var _size  : DWORD
                             ) ;
    var
       k           : DWORD   ;
       extNo       : DWORD   ;
       newExterior : Boolean ;
    begin
      _count      := 0 ;
      if srid > 0 then
        _size       := OFFSET_EWKB_MULTI
      else
        _size       := OFFSET_WKB_MULTI ;
      extNo       := 0 ;
      newExterior := True ;

      for k := 0 to rings-1 do begin
        if newExterior then begin
          extNo := k ;
          inc( _count ) ;
          newExterior := False ;
          if srid > 0 then
            _size := _size + OFFSET_EWKB_MULTI + calcPartSize( k )
          else
            _size := _size + OFFSET_WKB_MULTI  + calcPartSize( k ) ;
        end
        else if isExtContainIntPart( extNo, k ) then begin
          _size := _size + calcPartSize( k ) ;
        end ;
        if k < rings-1 then
          newExterior := not isExtContainIntPart( extNo, k+1 ) ;
      end ;
    end ;

    procedure appendCoordinate( const _i : Integer ;
                                const _j : Integer
                              ) ;
    var
      ptg       : TGIS_Point ;
      ptg3D     : TGIS_Point3D ;
    begin
      case wkbtype of
        3 : begin
            ptg := _shp.GetPoint( _i, _j ) ;
            {$IFDEF MANAGED}
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg.X),
                         sizeOf( Double ) ) ;
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg.Y),
                         sizeOf( Double ) ) ;
            {$ELSE}
              write_wkb( ptvar, ptg, sizeOf( ptg ) ) ;
            {$ENDIF}
            end ;
        1003 : begin
              ptg3D := _shp.GetPoint3D( _i, _j ) ;
            {$IFDEF MANAGED}
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.X),
                         sizeOf( Double ) ) ;
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Y),
                         sizeOf( Double ) ) ;
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Z),
                         sizeOf( Double ) ) ;
            {$ELSE}
              write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
              write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
              write_wkb( ptvar, ptg3D.Z, sizeOf( Double ) ) ;
            {$ENDIF}
            end ;
        2003 : begin
              ptg3D := _shp.GetPoint3D( _i, _j ) ;
            {$IFDEF MANAGED}
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.X),
                         sizeOf( Double ) ) ;
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Y),
                         sizeOf( Double ) ) ;
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.M),
                         sizeOf( Double ) ) ;
            {$ELSE}
              write_wkb( ptvar, ptg3D.X, sizeOf( Double ) ) ;
              write_wkb( ptvar, ptg3D.Y, sizeOf( Double ) ) ;
              write_wkb( ptvar, ptg3D.M, sizeOf( Double ) ) ;
            {$ENDIF}
            end ;
        3003 : begin
              ptg3D := _shp.GetPoint3D( _i, _j ) ;
            {$IFDEF MANAGED}
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.X),
                         sizeOf( Double ) ) ;
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Y),
                         sizeOf( Double ) ) ;
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.Z),
                         sizeOf( Double ) ) ;
              write_wkb( vr, vr_off, BitConverter.GetBytes(ptg3D.M),
                         sizeOf( Double ) ) ;
            {$ELSE}
              write_wkb( ptvar, ptg3D, SIZEOF_TGIS_POINT3D ) ;
            {$ENDIF}
            end ;
      end ;
    end ;

    procedure exportPolygon ;
    var
      r, p : Integer ;
    begin
      rings := _shp.GetNumParts ;
      if srid > 0 then
        size  := OFFSET_EWKB_MULTI
      else
        size  := OFFSET_WKB_MULTI ;

      if rings > 0 then
        for r := 0 to rings - 1 do
          size := size + calcPartSize( r ) ;

      _ewkb  := VarArrayCreate([0,size-1], varByte ) ;
      {$IFDEF MANAGED}
        {$IFNDEF OXYGENE}
          assert( VarArrayElementsIsType( OleVariant(_ewkb), varByte ) ) ;
        {$ENDIF}
        vr := TBytes(_ewkb);
        vr_off := 0  ;
      {$ELSE}
        ptvar := VarArrayLock( _ewkb ) ;
      {$ENDIF}

      // write WKBPolygonRecord
      {$IFDEF OXYGENE}
        byteOrder[0] := 1 ;
      {$ELSE}
        byteOrder    := 1 ;
      {$ENDIF}
      {$IFDEF MANAGED}
        write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
        if srid > 0 then begin
          wkbtype := wkbtype or EWKB_SRID_FLAG ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(   srid), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(  rings), sizeOf(DWORD) ) ;
          wkbtype := wkbtype and not EWKB_ALL_FLAGS ;
        end
        else begin
          write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(  rings), sizeOf(DWORD) ) ;
        end ;
      {$ELSE}
        write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
        if srid > 0 then begin
          wkbtype := wkbtype or EWKB_SRID_FLAG ;
          write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
          write_wkb( ptvar,    srid, sizeOf( Integer ) ) ;
          write_wkb( ptvar,   rings, sizeOf(   rings ) ) ;
          wkbtype := wkbtype and not EWKB_ALL_FLAGS ;
        end
        else begin
          write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
          write_wkb( ptvar,   rings, sizeOf(   rings ) ) ;
        end ;
      {$ENDIF}

      if _shp.IsEmpty then begin
        {$IFNDEF MANAGED}
          VarArrayUnLock( _ewkb ) ;
        {$ENDIF}
        exit ;
      end ;

      if srid > 0 then begin
        case _shp.Dimension of
          TGIS_DimensionType.XYZ  : wkbtype := wkbtype + 1000 ;
          TGIS_DimensionType.XYM  : wkbtype := wkbtype + 2000 ;
          TGIS_DimensionType.XYZM : wkbtype := wkbtype + 3000 ;
        end ;
      end ;

      for r := 0 to rings - 1 do begin
        points := _shp.GetPartSize( r ) ;
        // write whole LineRing
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(points),
                     sizeOf( DWORD ) ) ;
        {$ELSE}
          write_wkb( ptvar, points, sizeOf( points ) ) ;
        {$ENDIF}

        if ( r = 0 ) then begin
          // outer ring must be counter-clockwise
          if not isClockwisePart( _shp, r ) then begin
            for p := 0 to points - 1 do
              appendCoordinate( r, p ) ;
          end
          else begin
            for p := points - 1 downto 0 do
              appendCoordinate( r, p ) ;
          end ;
        end
        else begin
          // inner rings must be clockwise
          if isClockwisePart( _shp, r ) then begin
            for p := 0 to points - 1 do
              appendCoordinate( r, p ) ;
          end
          else begin
            for p := points - 1 downto 0 do
              appendCoordinate( r, p ) ;
          end ;
        end ;
      end ;

      {$IFNDEF MANAGED}
        VarArrayUnLock( _ewkb ) ;
      {$ENDIF}
    end ;

  begin
    if not assigned( _shp ) then exit ;
    //if _shp.IsEmpty then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if assigned( _shp.Layer ) and assigned( _shp.Layer.CS ) and
         ( _shp.Layer.CS.EPSG  > 0 ) and ( _shp.Layer.CS.EPSG  = _srid ) then
        srid := _shp.Layer.CS.EPSG
      else
        srid := _srid ;

      rings  := _shp.GetNumParts ;

      if ( rings = 1 ) then begin
        // write Polygon Geometry record
        exportPolygon ;
      end
      else begin
        calcCollection( count, size ) ;
        // write Polygon Geometry record
        if count = 1 then begin
          exportPolygon ;
          exit ;
        end ;

        _ewkb  := VarArrayCreate([0,size-1], varByte ) ;
        {$IFDEF MANAGED}
          {$IFNDEF OXYGENE}
            assert( VarArrayElementsIsType( OleVariant(_ewkb), varByte ) ) ;
          {$ENDIF}
          vr := TBytes(_ewkb);
          vr_off := 0  ;
        {$ELSE}
          ptvar := VarArrayLock( _ewkb ) ;
        {$ENDIF}

        // MultiPolygon Geometry record with or without holes
        {$IFDEF OXYGENE}
          byteOrder[0] := 1 ;
        {$ELSE}
          byteOrder    := 1 ;
        {$ENDIF}
        wkbtype := wkbtype + 3 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
          if srid > 0 then begin
            wkbtype := wkbtype or EWKB_SRID_FLAG ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(   srid), sizeOf(DWORD) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(  count), sizeOf(DWORD) ) ;
            wkbtype := wkbtype and not EWKB_ALL_FLAGS ;
          end
          else begin
            write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(  count), sizeOf(DWORD) ) ;
          end ;
        {$ELSE}
          write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
          if srid > 0 then begin
            wkbtype := wkbtype or EWKB_SRID_FLAG ;
            write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
            write_wkb( ptvar,    srid, sizeOf( Integer ) ) ;
            write_wkb( ptvar,   count, sizeOf(   count ) ) ;
            wkbtype := wkbtype and not EWKB_ALL_FLAGS ;
          end
          else begin
            write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
            write_wkb( ptvar,   count, sizeOf(   count ) ) ;
          end ;
        {$ENDIF}

        if _shp.IsEmpty then begin
          {$IFNDEF MANAGED}
            VarArrayUnLock( _ewkb ) ;
          {$ENDIF}
          exit ;
        end ;

        if srid > 0 then begin
          case _shp.Dimension of
            TGIS_DimensionType.XYZ  : wkbtype := wkbtype + 1000 ;
            TGIS_DimensionType.XYM  : wkbtype := wkbtype + 2000 ;
            TGIS_DimensionType.XYZM : wkbtype := wkbtype + 3000 ;
          end ;
        end ;

        // Geometry Collection
        part_no := 0 ;
        wkbtype := wkbtype - 3 ;
        for num := 1 to count do begin
          // Calculation parameters of one item from Geometry Collection
          calcBoundary( part_no, int_count ) ;
          inc( int_count ) ;
          {$IFDEF MANAGED}
            write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype),
                       sizeOf( DWORD ) ) ;
            write_wkb( vr, vr_off, BitConverter.GetBytes(int_count),
                       sizeOf( DWORD ) ) ;
          {$ELSE}
            write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
            write_wkb( ptvar, wkbtype  , sizeOf( wkbtype   ) ) ;
            write_wkb( ptvar, int_count, sizeOf( int_count ) ) ;
          {$ENDIF}
          dec( int_count ) ;
          // Polygon Geometry record with holes
          if ( int_count > 0 ) then begin
            for rn := part_no to part_no + int_count do begin
              if ( rn = part_no ) then begin
                points := _shp.GetPartSize( rn ) ;
                // write whole LineRing
                {$IFDEF MANAGED}
                  write_wkb( vr, vr_off, BitConverter.GetBytes(points),
                             sizeOf( DWORD ) ) ;
                {$ELSE}
                  write_wkb( ptvar, points, sizeOf( points ) ) ;
                {$ENDIF}
                // Polygon Geometry
                // outer ring must be counter-clockwise
                if not isClockwisePart( _shp, rn ) then begin
                  for pn := 0 to points - 1 do
                    appendCoordinate( rn, pn ) ;
                end
                else begin
                  for pn := points - 1 downto 0 do
                    appendCoordinate( rn, pn ) ;
                end ;
              end
              else if isExtContainIntPart( part_no, rn ) then begin
                points := _shp.GetPartSize( rn ) ;
                // write whole LineRing
                {$IFDEF MANAGED}
                  write_wkb( vr, vr_off, BitConverter.GetBytes(points),
                             sizeOf( DWORD ) ) ;
                {$ELSE}
                  write_wkb( ptvar, points, sizeOf( points ) ) ;
                {$ENDIF}
                // Polygon Geometry
                // inner rings must be clockwise
                if isClockwisePart( _shp, rn ) then begin
                  for pn := 0 to points - 1 do
                    appendCoordinate( rn, pn ) ;
                end
                else begin
                  for pn := points - 1 downto 0 do
                    appendCoordinate( rn, pn ) ;
                end ;
              end ;
            end ;
          end
          else begin
            points := _shp.GetPartSize( part_no ) ;
            // write whole LineRing
            {$IFDEF MANAGED}
              write_wkb( vr, vr_off, BitConverter.GetBytes(points),
                         sizeOf( DWORD ) ) ;
            {$ELSE}
              write_wkb( ptvar, points, sizeOf( points ) ) ;
            {$ENDIF}
            // Polygon Geometry
            // outer ring must be counter-clockwise
            if not isClockwisePart( _shp, part_no ) then begin
              for pn := 0 to points - 1 do
                appendCoordinate( part_no, pn ) ;
            end
            else begin
              for pn := points - 1 downto 0 do
                appendCoordinate( part_no, pn ) ;
            end ;

          end ;
          part_no := part_no + int_count + 1 ;
        end ;
        {$IFNDEF MANAGED}
          VarArrayUnLock( _ewkb ) ;
        {$ENDIF}
      end ;

    finally
      _shp.Unlock ;
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportPolygonToEWKB(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    GisExportPolygonToEWKB( _shp, _srid, false, _ewkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportPolygonToEWKB(
    const _shp  : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    if assigned( _shp ) and assigned( _shp.Layer ) then
      GisExportPolygonToEWKB( _shp, _shp.Layer.CS.EPSG, False, _ewkb )
    else
      GisExportPolygonToEWKB( _shp, 0, False, _ewkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportPolygonToWKB(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _wkb : Object
    {$ELSE}
      var _wkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    GisExportPolygonToEWKB( _shp, 0, false, _wkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportPolygonToVAR(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _var : Object
    {$ELSE}
      var _var : OleVariant
    {$ENDIF}
  ) ;
  var
    {$IFDEF MANAGED}
      vr     : TBytes  ;
      vr_off : Integer ;
    {$ELSE}
      pbuf   : Pointer ;
    {$ENDIF}
    val      : Double ;
    size     : Integer ;
  begin
    if _shp.IsEmpty then exit ;

    // calculate size
    size := _shp.GeometrySize ;
    if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then
      size := size + 16 + _shp.PointsZSize + 16 + _shp.PointsMSize
    else if _shp.Dimension in [ TGIS_DimensionType.XYM ] then
      size := size + 16 + _shp.PointsMSize ;

    _var := VarArrayCreate([0,size], varByte ) ;
    {$IFDEF MANAGED}
      vr := TBytes(TObject(_var));
      vr_off := 0 ;
    {$ELSE}
      pbuf  := VarArrayLock( _var ) ;
    {$ENDIF}
    try
      {$IFDEF MANAGED}
        write_wkb( vr, vr_off, _shp.Parts, _shp.PartsSize ) ;
        write_wkb( vr, vr_off, _shp.Points, _shp.PointsSize ) ;
      {$ELSE}
        write_wkb( pbuf, _shp.Parts^, _shp.PartsSize ) ;
        write_wkb( pbuf, _shp.Points^, _shp.PointsSize ) ;
      {$ENDIF}

      if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then begin
        val := _shp.PointsZMin ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := _shp.PointsZMax ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_wkb( vr, vr_off, _shp.PointsZ, _shp.PointsZSize ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_wkb( pbuf, _shp.PointsZ^, _shp.PointsZSize ) ;
        {$ENDIF}
      end ;

      if _shp.Dimension in [ TGIS_DimensionType.XYM, TGIS_DimensionType.XYZM ] then begin
        val := _shp.PointsMMin ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := _shp.PointsMMax ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_wkb( vr, vr_off, _shp.PointsM, _shp.PointsMSize ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_wkb( pbuf, _shp.PointsM^, _shp.PointsMSize ) ;
        {$ENDIF}
      end
      else if ( _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] ) then
      begin
        val := 0 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := 0 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_fakebuf( vr, vr_off, _shp.PointsZSize  ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_fakebuf( pbuf, _shp.PointsZSize  ) ;
        {$ENDIF}
      end ;
    finally
      {$IFNDEF MANAGED}
        VarArrayUnlock( _var ) ;
      {$ENDIF}
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportPolygonToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    const _ver  : Integer
  ) : String ;
    var
      space     : String         ;
      count     : DWORD          ;
      p, points : DWORD          ;
      r, rings  : DWORD          ;
      num       : DWORD          ;
      part_no   : DWORD          ;
      int_count : DWORD          ;
      sb        : TStringBuilder ;

    function isExtContainIntPart(
      const _extPartNo : Integer ;
      const _intPartNo : Integer
    ) : Boolean ;
    var
      point_no     : Integer ;
      points_count : Integer ;
      next_pt      : Integer ;
      npar, p1     : Integer ;
      line_a       : TGIS_Point ;
      line_b       : TGIS_Point ;
      ptg1         : TGIS_Point ;
    begin
      Result := False ;

      for p1 := 0 to _shp.GetPartSize( _intPartNo )-1 do begin

        ptg1 := _shp.GetPoint( _intPartNo, p1 ) ;
        with _shp.Extent do begin
          if (XMax   = Xmin) and (YMax   = YMin) and
             (ptg1.X = Xmin) and (ptg1.X = YMin) then begin
            Result := True ;
            exit ;
          end ;
        end ;

        //for count := start to stop do begin
        points_count := _shp.GetPartSize( _extPartNo ) ;
        next_pt := points_count - 1 ;
        npar := 0 ;
        for point_no:=0 to points_count-1 do  begin // all points
          // point will be tested on every line of polygon
          // test is based on odd/even algorithm
          line_a := _shp.GetPoint( _extPartNo, point_no ) ;
          line_b := _shp.GetPoint( _extPartNo, next_pt  ) ;
          next_pt := point_no ;
          try
            if ( ( ( ( line_a.Y <= ptg1.Y ) and ( ptg1.Y < line_b.Y ) ) or
                   ( ( line_b.Y <= ptg1.Y ) and ( ptg1.Y < line_a.Y ) )
                 ) and
                 ( ptg1.X < ( line_b.X - line_a.X ) * ( ptg1.Y - line_a.Y ) /
                            ( line_b.Y - line_a.Y ) + line_a.X
                 )
               ) then
              npar := npar + 1 ;
          except
          end ;
        end ;

        if ( npar mod 2) = 1 then
          Result := True ; // even - point is inside part

        if not Result then Break ;
      end ;
    end ;

    procedure calcBoundary( const _partNo   : DWORD ;
                            var   _intCount : DWORD
                          ) ;
    var
       k          : DWORD   ;
    begin
      if ( _partNo > rings-1 ) then exit ;
      _intCount := 0 ;
      for k := _partNo to rings-1 do begin
        if ( k = _partNo ) then
        else if isExtContainIntPart( _partNo, k ) then begin
          inc( _intCount ) ;
        end
        else
          Break ;
      end ;
    end ;

    procedure calcCollection( var _count : DWORD ) ;
    var
       k           : DWORD   ;
       extNo       : DWORD   ;
       newExterior : Boolean ;
    begin
      _count      := 0 ;
      extNo       := 0 ;
      newExterior := True ;
      for k := 0 to rings-1 do begin
        if newExterior then begin
          extNo := k ;
          inc( _count ) ;
          newExterior := False ;
        end
        else if isExtContainIntPart( extNo, k ) then begin
        end ;
        if k < rings-1 then
          newExterior := not isExtContainIntPart( extNo, k+1 ) ;
      end ;
    end ;

    function appendDimension( const _name : String ) : String ;
    begin
      if _ver = 0 then space := ''
                  else space := ' ' ;

      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY      : Result := Format( _name, [''   ,''  ] ) ;
        TGIS_DimensionType.XYZ     : Result := Format( _name, [space,'Z' ] ) ;
        TGIS_DimensionType.XYM     : Result := Format( _name, [space,'M' ] ) ;
        TGIS_DimensionType.XYZM    : Result := Format( _name, [space,'ZM'] ) ;
      end ;
    end ;

    procedure appendCoordinate( const _i : Integer ;
                                const _j : Integer
                              ) ;
    var
      ptg   : TGIS_Point    ;
      ptg3D : TGIS_Point3D  ;
    begin
      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY :
          begin
            ptg := _shp.GetPoint( _i, _j ) ;
            sb.Append( Format( '%s %s',
                              [ DotFloatToStr(ptg.X),
                                DotFloatToStr(ptg.Y) ]
                             )
                       ) ;
          end ;
        TGIS_DimensionType.XYZ :
          begin
            ptg3D  := _shp.GetPoint3D( _i, _j ) ;
            sb.Append( Format( '%s %s %s',
                               [ DotFloatToStr(ptg3D.X),
                                 DotFloatToStr(ptg3D.Y),
                                 DotFloatToStr(ptg3D.Z) ]
                             )
                      ) ;
          end ;
        TGIS_DimensionType.XYM :
          begin
            ptg3D  := _shp.GetPoint3D( _i, _j ) ;
            sb.Append( Format( '%s %s %s',
                              [ DotFloatToStr(ptg3D.X),
                                DotFloatToStr(ptg3D.Y),
                                DotFloatToStr(ptg3D.M)
                              ]
                            )
                      ) ;
          end ;
        TGIS_DimensionType.XYZM :
          begin
            ptg3D  := _shp.GetPoint3D( _i, _j ) ;
            sb.Append( Format( '%s %s %s %s',
                              [ DotFloatToStr(ptg3D.X),
                                DotFloatToStr(ptg3D.Y),
                                DotFloatToStr(ptg3D.Z),
                                DotFloatToStr(ptg3D.M)
                              ]
                            )
                      ) ;
          end ;
      end ;
    end ;

    procedure exportPolygon ;
    var
      i     : Integer      ;
      icnt  : Integer      ;
      j     : Integer      ;
      jcnt  : Integer      ;
    begin
      icnt := _shp.GetNumParts - 1 ;
      sb.Append( appendDimension( 'POLYGON%s%s' ) ) ;
      sb.Append( '(' ) ;
      for i:= 0 to icnt do begin
        sb.Append( '(' ) ;
        jcnt := _shp.GetPartSize( i ) - 1 ;
        for j:= 0 to jcnt do begin
          appendCoordinate( i, j ) ;
          if j <> jcnt then
            sb.Append( ',' )
        end ;
        sb.Append( ')' ) ;
        if i <> icnt then
          sb.Append( ',' )
      end ;
      sb.Append( ')' ) ;
    end ;

  begin
    if not assigned( _shp ) then exit ;

    sb := TStringBuilder.Create ;
    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if ( _srid > 0 ) then begin
        if ( _shp.Layer         <>   nil ) and
           ( _shp.Layer.CS      <>   nil ) and
           ( _shp.Layer.CS.EPSG  >     0 ) and
           ( _shp.Layer.CS.EPSG  = _srid ) then
          sb.Append( Format( 'SRID=%d;', [_shp.Layer.CS.EPSG] ) )
        else
          sb.Append( Format( 'SRID=%d;', [_srid] ) ) ;
      end ;

      if _shp.IsEmpty then begin
        sb.Append( appendDimension('POLYGON%s%s EMPTY') ) ;
        exit ;
      end ;

      rings  := _shp.GetNumParts ;

      if ( rings = 1 ) then begin
        // write Polygon Geometry record
       exportPolygon ;
      end
      else begin
        calcCollection( count ) ;
        // write Polygon Geometry record
        if count = 1 then begin
          exportPolygon ;
          exit ;
        end ;

        if _shp.IsEmpty then begin
          exit ;
        end ;

        // MultiPolygon Geometry record with or without holes
        sb.Append( appendDimension( 'MULTIPOLYGON%s%s(' ) ) ;

        // Geometry Collection
        part_no := 0 ;
        for num := 1 to count do begin
          // Calculation parameters of one item from Geometry Collection
          calcBoundary( part_no, int_count ) ;

          // Polygon Geometry record with holes
          if ( int_count > 0 ) then begin
            sb.Append( '(' ) ;

            for r := part_no to part_no + int_count do begin
              sb.Append( '(' ) ;
              if ( r = part_no ) then begin
                points := _shp.GetPartSize( r ) ;
                // Polygon Geometry
                for p := 0 to points-1 do begin
                  appendCoordinate( r, p ) ;
                  if p <> points-1 then
                    sb.Append( ',' )
                end ;
              end
              else if isExtContainIntPart( part_no, r ) then begin
                points := _shp.GetPartSize( r ) ;
                // Polygon Geometry
                for p := 0 to points-1 do begin
                  appendCoordinate( r, p ) ;
                  if p <> points-1 then
                    sb.Append( ',' )
                end ;
              end ;
              sb.Append( ')' ) ;
              if r < part_no + int_count then
                sb.Append( ',' ) ;
            end ;
            sb.Append( ')' ) ;
          end
          else begin
            // Polygon Geometry record without holes
            sb.Append( '((' ) ;
            points := _shp.GetPartSize( part_no ) ;
            // Polygon Geometry
            for p := 0 to points - 1 do begin
              appendCoordinate( part_no, p ) ;
              if p <> points-1 then
                sb.Append( ',' )
            end ;
            sb.Append( '))' );
          end ;
          if num < count then
            sb.Append( ',' ) ;
          part_no := part_no + int_count + 1 ;
        end ;
        sb.Append( ')' ) ;
      end ;
    finally
      Result := sb.ToString ;
      FreeObject( sb ) ;
      _shp.Unlock ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportPolygonToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer
  ) : String ;
  begin
    Result := GisExportPolygonToEWKT( _shp, _srid, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportPolygonToEWKT(
    const _shp  : TGIS_Shape
  ) : String ;
  begin
    Result := GisExportPolygonToEWKT( _shp, 0, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportPolygonToWKT(
    const _shp : TGIS_Shape
  ) : String ;
  begin
    Result := GisExportPolygonToEWKT( _shp, 0, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportPolygonToJSON(
    const _shp : TGIS_Shape
  ) : String ;
  var
      count     : DWORD          ;
      p, points : DWORD          ;
      r, rings  : DWORD          ;
      num       : DWORD          ;
      part_no   : DWORD          ;
      int_count : DWORD          ;
      sb        : TStringBuilder ;

    function isExtContainIntPart(
      const _extPartNo : Integer ;
      const _intPartNo : Integer
    ) : Boolean ;
    var
      point_no     : Integer ;
      points_count : Integer ;
      next_pt      : Integer ;
      npar, p1     : Integer ;
      line_a       : TGIS_Point ;
      line_b       : TGIS_Point ;
      ptg1         : TGIS_Point ;
    begin
      Result := False ;

      for p1 := 0 to _shp.GetPartSize( _intPartNo )-1 do begin

        ptg1 := _shp.GetPoint( _intPartNo, p1 ) ;
        with _shp.Extent do begin
          if (XMax   = Xmin) and (YMax   = YMin) and
             (ptg1.X = Xmin) and (ptg1.X = YMin) then begin
            Result := True ;
            exit ;
          end ;
        end ;

        //for count := start to stop do begin
        points_count := _shp.GetPartSize( _extPartNo ) ;
        next_pt := points_count - 1 ;
        npar := 0 ;
        for point_no:=0 to points_count-1 do  begin // all points
          // point will be tested on every line of polygon
          // test is based on odd/even algorithm
          line_a := _shp.GetPoint( _extPartNo, point_no ) ;
          line_b := _shp.GetPoint( _extPartNo, next_pt  ) ;
          next_pt := point_no ;
          try
            if ( ( ( ( line_a.Y <= ptg1.Y ) and ( ptg1.Y < line_b.Y ) ) or
                   ( ( line_b.Y <= ptg1.Y ) and ( ptg1.Y < line_a.Y ) )
                 ) and
                 ( ptg1.X < ( line_b.X - line_a.X ) * ( ptg1.Y - line_a.Y ) /
                            ( line_b.Y - line_a.Y ) + line_a.X
                 )
               ) then
              npar := npar + 1 ;
          except
          end ;
        end ;

        if ( npar mod 2) = 1 then
          Result := True ; // even - point is inside part

        if not Result then Break ;
      end ;
    end ;

    procedure calcBoundary( const _partNo   : DWORD ;
                            var   _intCount : DWORD
                          ) ;
    var
       k          : DWORD   ;
    begin
      if ( _partNo > rings-1 ) then exit ;
      _intCount := 0 ;
      for k := _partNo to rings-1 do begin
        if ( k = _partNo ) then
        else if isExtContainIntPart( _partNo, k ) then begin
          inc( _intCount ) ;
        end
        else
          Break ;
      end ;
    end ;

    procedure calcCollection( var _count : DWORD ) ;
    var
       k           : DWORD   ;
       extNo       : DWORD   ;
       newExterior : Boolean ;
    begin
      _count      := 0 ;
      extNo       := 0 ;
      newExterior := True ;
      for k := 0 to rings-1 do begin
        if newExterior then begin
          extNo := k ;
          inc( _count ) ;
          newExterior := False ;
        end
        else if isExtContainIntPart( extNo, k ) then begin
        end ;
        if k < rings-1 then
          newExterior := not isExtContainIntPart( extNo, k+1 ) ;
      end ;
    end ;

    procedure appendCoordinate( const _i : Integer ;
                                const _j : Integer
                              ) ;
    var
      ptg   : TGIS_Point    ;
      ptg3D : TGIS_Point3D  ;
    begin
      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY:
          begin
            ptg := _shp.GetPoint( _i, _j ) ;
            sb.Append( Format( '[%s, %s]',
                              [ DotFloatToStr(ptg.X),
                                DotFloatToStr(ptg.Y) ]
                             )
                       ) ;
          end ;
        TGIS_DimensionType.XYZ:
          begin
            ptg3D  := _shp.GetPoint3D( _i, _j ) ;
            sb.Append( Format( '[%s, %s, %s]',
                               [ DotFloatToStr(ptg3D.X),
                                 DotFloatToStr(ptg3D.Y),
                                 DotFloatToStr(ptg3D.Z) ]
                             )
                      ) ;
          end ;
        TGIS_DimensionType.XYM:
          begin
            ptg3D  := _shp.GetPoint3D( _i, _j ) ;
            sb.Append( Format( '[%s, %s, %s]',
                              [ DotFloatToStr(ptg3D.X),
                                DotFloatToStr(ptg3D.Y),
                                DotFloatToStr(ptg3D.M)
                              ]
                            )
                      ) ;
          end ;
        TGIS_DimensionType.XYZM:
          begin
            ptg3D  := _shp.GetPoint3D( _i, _j ) ;
            sb.Append( Format( '[%s, %s, %s, %s]',
                              [ DotFloatToStr(ptg3D.X),
                                DotFloatToStr(ptg3D.Y),
                                DotFloatToStr(ptg3D.Z),
                                DotFloatToStr(ptg3D.M)
                              ]
                            )
                      ) ;
          end ;
      end ;
    end ;

    procedure exportPolygon ;
    var
      i     : Integer      ;
    icnt  : Integer      ;
      j     : Integer      ;
    jcnt  : Integer      ;
  begin
      icnt := _shp.GetNumParts - 1 ;
      sb.Append( '{"type":"Polygon","coordinates":[' ) ;
      for i:= 0 to icnt do begin
        sb.Append( '[' ) ;
        jcnt := _shp.GetPartSize( i ) - 1 ;
        for j:= 0 to jcnt do begin
          appendCoordinate( i, j ) ;
          if j <> jcnt then
            sb.Append( ',' )
        end ;

        sb.Append( ']' ) ;
        if i <> icnt then
          sb.Append( ',' )
      end ;
      sb.Append( ']}' ) ;
    end ;

  begin
    if not assigned( _shp ) then exit ;

    sb := TStringBuilder.Create ;
    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      rings  := _shp.GetNumParts ;

      if ( rings = 1 ) then begin
        // write Polygon Geometry record
       exportPolygon ;
      end
      else begin
        calcCollection( count ) ;
        // write Polygon Geometry record
        if count = 1 then begin
          exportPolygon ;
          exit ;
        end ;

        if _shp.IsEmpty then begin
          exit ;
        end ;

        // MultiPolygon Geometry record with or without holes
        sb.Append( '{"type":"MultiPolygon","coordinates":[' ) ;

        // Geometry Collection
        part_no := 0 ;
        for num := 1 to count do begin
          // Calculation parameters of one item from Geometry Collection
          calcBoundary( part_no, int_count ) ;

          // Polygon Geometry record with holes
          if ( int_count > 0 ) then begin
            sb.Append( '[' ) ;

            for r := part_no to part_no + int_count do begin
              sb.Append( '[' ) ;
              if ( r = part_no ) then begin
                points := _shp.GetPartSize( r ) ;
                // Polygon Geometry
                for p := 0 to points-1 do begin
                  appendCoordinate( r, p ) ;
                  if p <> points-1 then
                    sb.Append( ',' )
                end ;
              end
              else if isExtContainIntPart( part_no, r ) then begin
                points := _shp.GetPartSize( r ) ;
                // Polygon Geometry
                for p := 0 to points-1 do begin
                  appendCoordinate( r, p ) ;
                  if p <> points-1 then
                    sb.Append( ',' )
                end ;
              end ;
              sb.Append( ']' ) ;
              if r < part_no + int_count then
                sb.Append( ',' ) ;
            end ;
            sb.Append( ']' ) ;
          end
          else begin
            // Polygon Geometry record without holes
            sb.Append( '[[' ) ;
            points := _shp.GetPartSize( part_no ) ;
            // Polygon Geometry
            for p := 0 to points - 1 do begin
              appendCoordinate( part_no, p ) ;
              if p <> points-1 then
                sb.Append( ',' )
            end ;
            sb.Append( ']]' );
          end ;
          if num < count then
            sb.Append( ',' ) ;
          part_no := part_no + int_count + 1 ;
        end ;
        sb.Append( ']}' ) ;
      end ;
    finally
      Result := sb.ToString ;
      FreeObject( sb ) ;
      _shp.Unlock ;
    end ;
    end ;


  class function TGIS_GeometryFactory.GisExportComplexToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    const _ver  : Integer
  ) : String ;
  var
    space : String  ;
    i     : Integer ;
    icnt  : Integer ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;
    icnt   := TGIS_ShapeComplex( _shp ).ShapesCount - 1 ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if ( _srid > 0 ) then begin
        if ( _shp.Layer         <>   nil ) and
           ( _shp.Layer.CS      <>   nil ) and
           ( _shp.Layer.CS.EPSG  >     0 ) and
           ( _shp.Layer.CS.EPSG  = _srid ) then
          Result := Format( 'SRID=%d;', [_shp.Layer.CS.EPSG] )
        else
          Result := Format( 'SRID=%d;', [_srid] ) ;
      end
      else
        Result := '' ;

      Result := Result + 'GEOMETRYCOLLECTION%s%s' ;

      if _ver = 0 then space := ''
                  else space := ' ' ;

      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY      : Result := Format( Result, [''   ,''  ] ) ;
        TGIS_DimensionType.XYZ     : Result := Format( Result, [space,'Z' ] ) ;
        TGIS_DimensionType.XYM     : Result := Format( Result, [space,'M' ] ) ;
        TGIS_DimensionType.XYZM    : Result := Format( Result, [space,'ZM'] ) ;
      end ;

      if icnt < 0 then begin
        Result := Result + ' EMPTY' ;
        exit ;
      end ;

      Result := Result + '(' ;
      for i := 0 to icnt do begin
        Result := Result + TGIS_ShapeComplex( _shp ).GetShape( i ).ExportToWKT ;
        if i <> icnt then
          Result := Result + ','
      end ;
      Result :=  Result + ')' ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportComplexToEWKT(
    const _shp  : TGIS_Shape ;
    const _srid : Integer
  ) : String ;
  begin
    Result := GisExportComplexToEWKT( _shp, _srid, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportComplexToEWKT(
    const _shp  : TGIS_Shape
  ) : String ;
  begin
    Result := GisExportComplexToEWKT( _shp, 0, 1 ) ;
  end ;

  class function TGIS_GeometryFactory.GisExportComplexToWKT(
    const _shp : TGIS_Shape
  ) : String ;
  var
    i     : Integer ;
    icnt  : Integer ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;
    icnt   := TGIS_ShapeComplex( _shp ).ShapesCount - 1 ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      Result := 'GEOMETRYCOLLECTION%s' ;
      case _shp.Dimension of
        TGIS_DimensionType.Unknown,
        TGIS_DimensionType.XY      : Result := Format( Result, [''   ] ) ;
        TGIS_DimensionType.XYZ     : Result := Format( Result, [' Z' ] ) ;
        TGIS_DimensionType.XYM     : Result := Format( Result, [' M' ] ) ;
        TGIS_DimensionType.XYZM    : Result := Format( Result, [' ZM'] ) ;
      end ;

      if icnt < 0 then begin
        Result := Result + ' EMPTY' ;
        exit ;
      end ;

      Result := Result + '(' ;
      for i := 0 to icnt do begin
        Result := Result + TGIS_ShapeComplex( _shp ).GetShape( i ).ExportToWKT ;
        if i <> icnt then
          Result := Result + ','
      end ;
      Result :=  Result + ')' ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportComplexToGML(
    const _shp : TGIS_Shape
  ) : String;
  var
    i    : Integer     ;
    icnt : Integer     ;
    hdr  : String      ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;
    icnt   := TGIS_ShapeComplex( _shp ).ShapesCount - 1 ;

    if icnt < 0 then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      hdr := '<MultiGeometry>';
      for i := 0 to icnt do begin
        Result := Result + '<geometryMember>' +
                  TGIS_ShapeComplex( _shp ).GetShape( i ).ExportToGML ;
        Result := Result + '</geometryMember>';
      end ;

      Result :=  hdr + Result + '</MultiGeometry>' ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportComplexToEWKB(
    const _shp  : TGIS_Shape ;
    const _srid : Integer    ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  var
    size      : Cardinal   ;
    sizeTab   : Cardinal   ;
    {$IFDEF OXYGENE}
      byteOrder : array of Byte := new Byte[ sizeOf( Byte ) ] ;
      byteFlags : array of Byte := new Byte[ sizeOf( Byte ) ] ;
    {$ELSE}
      byteOrder : Byte     ;
      byteFlags : Byte     ;
    {$ENDIF}
    wkbtype   : DWORD      ;
    {$IFDEF MANAGED}
      vr      : TBytes     ;
      vr_off  : Integer    ;
      vr_off2 : Integer    ;
      ptvar2  : TBytes     ;
    {$ELSE}
      ptvar   : Pointer    ;
      ptvar2  : Pointer    ;
    {$ENDIF}
    {$IFDEF MANAGED}
      {$IFNDEF OXYGENE}
        tabObj  : TObject  ;
      {$ENDIF}
    {$ENDIF}
    r, elems  : DWORD      ;
    nelems    : Integer    ;
    tab       : array of OleVariant ;
    i         : Integer    ;
    srid      : Integer    ;

    {$IFDEF MANAGED}
      procedure skip_bytes(
        var   _o     : Integer ;
        const _count : Cardinal
      ) ;
      begin
        _o := _o + Integer(_count) ;
      end ;
    {$ELSE}
      procedure skip_bytes(
        var   _p     : Pointer ;
        const _count : Cardinal
      ) ;
      begin
        _p := Pointer( NativeInt( _p ) + Integer(_count) ) ;
      end ;
    {$ENDIF}

  begin
    if not assigned( _shp ) then exit ;
    elems  := TGIS_ShapeComplex( _shp ).ShapesCount ;
    nelems := elems ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      if assigned( _shp.Layer ) and assigned( _shp.Layer.CS ) and
         ( _shp.Layer.CS.EPSG  > 0 ) and ( _shp.Layer.CS.EPSG  = _srid ) then
        srid := _shp.Layer.CS.EPSG
      else
        srid := _srid ;

      SetLength( tab, nelems ) ;
      for i := 0 to nelems - 1 do begin
        {$IFDEF MANAGED}
          {$IFNDEF OXYGENE}
            tabObj := TObject( tab[ i ] ) ;
            if srid > 0 then begin
              case TGIS_ShapeComplex( _shp ).GetShape( i ).ShapeType of
                TGIS_ShapeType.Point :
                  GisExportPointToEWKB     ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tabObj ) ;

                TGIS_ShapeType.MultiPoint :
                  GisExportMultiPointToEWKB( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tabObj ) ;

                TGIS_ShapeType.Arc :
                  GisExportArcToEWKB       ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tabObj ) ;

                TGIS_ShapeType.Polygon :
                  GisExportPolygonToEWKB   ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tabObj ) ;

                TGIS_ShapeType.Complex :
                  GisExportComplexToEWKB   ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tabObj ) ;
              end ;
            end
            else
              TGIS_ShapeComplex( _shp ).GetShape( i ).ExportToWKB( tabObj ) ;
          {$ELSE}
            tab[i] := new OleVariant ;
            if srid > 0 then begin
              case TGIS_ShapeComplex( _shp ).GetShape( i ).ShapeType of
                TGIS_ShapeType.Point :
                  GisExportPointToEWKB     ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;

                TGIS_ShapeType.MultiPoint :
                  GisExportMultiPointToEWKB( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;

                TGIS_ShapeType.Arc :
                  GisExportArcToEWKB       ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;

                TGIS_ShapeType.Polygon :
                  GisExportPolygonToEWKB   ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;

                TGIS_ShapeType.Complex :
                  GisExportComplexToEWKB   ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;
              end ;
            end
            else
              TGIS_ShapeComplex( _shp ).GetShape( i ).ExportToWKB( tab[i] ) ;
          {$ENDIF}
        {$ELSE}
          if srid > 0 then begin
            case TGIS_ShapeComplex( _shp ).GetShape( i ).ShapeType of
              TGIS_ShapeType.Point :
                GisExportPointToEWKB     ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;

              TGIS_ShapeType.MultiPoint :
                GisExportMultiPointToEWKB( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;

              TGIS_ShapeType.Arc :
                GisExportArcToEWKB       ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;

              TGIS_ShapeType.Polygon :
                GisExportPolygonToEWKB   ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;

              TGIS_ShapeType.Complex :
                GisExportComplexToEWKB   ( TGIS_ShapeComplex( _shp ).GetShape( i ), srid, tab[i] ) ;
            end ;
          end
          else
            TGIS_ShapeComplex( _shp ).GetShape( i ).ExportToWKB( tab[i] ) ;
        {$ENDIF}
      end ;

      if srid > 0 then
        size := OFFSET_EWKB_MULTI
      else
        size := OFFSET_WKB_MULTI ;

      if nelems > 0 then
        for r := 0 to nelems - 1 do
          if srid > 0 then
            size := size + Cardinal( VarArrayHighBound( tab[ r ], 1 ) + 1 ) - 4
          else
            size := size + Cardinal( VarArrayHighBound( tab[ r ], 1 ) + 1 ) ;

      _ewkb  := VarArrayCreate( [0,size-1], varByte ) ;
      {$IFDEF MANAGED}
        {$IFNDEF OXYGENE}
          assert( VarArrayElementsIsType( OleVariant(_ewkb), varByte ) ) ;
        {$ENDIF}
        vr := TBytes(_ewkb);
        vr_off := 0  ;
      {$ELSE}
        ptvar := VarArrayLock( _ewkb ) ;
      {$ENDIF}

      // write WKBComplexRecord
      {$IFDEF OXYGENE}
        byteOrder[0] := 1 ;
      {$ELSE}
        byteOrder    := 1 ;
      {$ENDIF}
      //wkbtype   := 7 ;
      case _shp.Dimension of
        TGIS_DimensionType.XYZ   : begin
          if srid > 0 then begin
            wkbtype :=    7 or EWKB_ZOFFSET_FLAG ;
          end
          else begin
            wkbtype := 1007 ;
          end ;
        end ;
        TGIS_DimensionType.XYM   : begin
          if srid > 0 then begin
            wkbtype :=    7 or EWKB_MOFFSET_FLAG ;
          end
          else begin
            wkbtype := 2007 ;
          end ;
        end ;
        TGIS_DimensionType.XYZM  : begin
          if srid > 0 then begin
            wkbtype :=    7 or EWKB_ZOFFSET_FLAG or EWKB_MOFFSET_FLAG ;
          end
          else begin
            wkbtype := 3007 ;
          end ;
        end
        else begin
          wkbtype :=      7 ;
        end ;
      end ;
      {$IFDEF MANAGED}
        write_wkb( vr, vr_off, byteOrder, sizeOf( Byte ) ) ;
        if srid > 0 then begin
          wkbtype := wkbtype or EWKB_SRID_FLAG ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(   srid), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(  elems), sizeOf(DWORD) ) ;
          wkbtype := wkbtype and not EWKB_ALL_FLAGS ;
        end
        else begin
          write_wkb( vr, vr_off, BitConverter.GetBytes(wkbtype), sizeOf(DWORD) ) ;
          write_wkb( vr, vr_off, BitConverter.GetBytes(  elems), sizeOf(DWORD) ) ;
        end ;
      {$ELSE}
        write_wkb( ptvar, byteOrder, sizeOf( byteOrder ) ) ;
        if srid > 0 then begin
          wkbtype := wkbtype or EWKB_SRID_FLAG ;
          write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
          write_wkb( ptvar,    srid, sizeOf( Integer ) ) ;
          write_wkb( ptvar,   elems, sizeOf(   elems ) ) ;
          wkbtype := wkbtype and not EWKB_ALL_FLAGS ;
        end
        else begin
          write_wkb( ptvar, wkbtype, sizeOf( wkbtype ) ) ;
          write_wkb( ptvar,   elems, sizeOf(   elems ) ) ;
        end ;
      {$ENDIF}

      if nelems = 0 then begin
        {$IFNDEF MANAGED}
          VarArrayUnLock( _ewkb ) ;
        {$ENDIF}
        exit ;
      end ;

      for r := 0 to nelems - 1 do begin
        {$IFDEF MANAGED}
          ptvar2  := TBytes( tab[ r ] ) ;
          vr_off2  := 0  ;
          sizeTab := VarArrayHighBound( tab[ r ], 1 ) + 1 ;
          if srid > 0 then begin
            write_wkb( vr, vr_off, ptvar2, 4 ) ;
            vr_off2 := vr_off2 + 4 ;
            read_wkb( ptvar2, vr_off2, byteFlags, 1 ) ;
            byteFlags[0] := byteFlags[0] and not $20 ;
            write_wkb( vr, vr_off, byteFlags, sizeOf(Byte) ) ;
            vr_off2 := vr_off2 + 4 ;
            write_wkb( vr, vr_off, ptvar2, vr_off2, sizeTab-9 ) ;
          end
          else
            write_wkb( vr, vr_off, ptvar2, sizeTab ) ;
        {$ELSE}
          ptvar2  := VarArrayLock( tab[ r ] ) ;
          sizeTab := VarArrayHighBound( tab[ r ], 1 ) + 1 ;
          if srid > 0 then begin
            write_wkb( ptvar, ptvar2^, 4 ) ;
            skip_bytes( ptvar2, 4 ) ;
            read_wkb( ptvar2, byteFlags, 1 ) ;
            byteFlags := byteFlags and not $20 ;
            write_wkb( ptvar, byteFlags, sizeOf(Byte) ) ;
            skip_bytes( ptvar2, 4 ) ;
            write_wkb( ptvar, ptvar2^, sizeTab-9 ) ;
          end
          else
            write_wkb( ptvar, ptvar2^, sizeTab ) ;
           VarArrayUnLock( tab[ r ] ) ;
        {$ENDIF}
      end ;

      {$IFNDEF MANAGED}
        VarArrayUnLock( _ewkb ) ;
      {$ENDIF}
      tab := nil ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportComplexToEWKB(
    const _shp  : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _ewkb : Object
    {$ELSE}
      var _ewkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    if assigned( _shp ) and assigned( _shp.Layer ) then
      GisExportComplexToEWKB( _shp, _shp.Layer.CS.EPSG, _ewkb )
    else
      GisExportComplexToEWKB( _shp, 0, _ewkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportComplexToWKB(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _wkb : Object
    {$ELSE}
      var _wkb : OleVariant
    {$ENDIF}
  ) ;
  begin
    GisExportComplexToEWKB( _shp, 0, _wkb ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportComplexToVAR(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _var : Object
    {$ELSE}
      var _var : OleVariant
    {$ENDIF}
  ) ;
  begin
    { TODO -cReview : to be implemented }
  end ;

  class procedure TGIS_GeometryFactory.GisExportComplexToGDO(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _gdo : Object
    {$ELSE}
      var _gdo : OleVariant
    {$ENDIF}
  ) ;
  var
    size      : Cardinal   ;
    sizeTab   : Cardinal   ;
    {$IFDEF MANAGED}
      vr      : TBytes     ;
      vr_off  : Integer    ;
      ptvar2  : TBytes     ;
    {$ELSE}
      ptvar   : Pointer    ;
      ptvar2  : Pointer    ;
    {$ENDIF}
    {$IFDEF MANAGED}
      tabObj  : TObject    ;
    {$ENDIF}
    r, elems  : DWORD      ;
    tab       : array of OleVariant ;
    i         : Integer;
    value     : DWORD         ;
  begin
    if not assigned( _shp ) then exit ;
    elems := TGIS_ShapeComplex( _shp ).ShapesCount ;

    if elems = 0 then exit ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      SetLength( tab, elems ) ;
      for i := 0 to elems - 1 do begin
        {$IFDEF MANAGED}
          tabObj := TObject( tab[ i ] ) ;
          TGIS_ShapeComplex( _shp ).GetShape( i ).ExportToGDO( tabObj ) ;
        {$ELSE}
          TGIS_ShapeComplex( _shp ).GetShape( i ).ExportToGDO( tab[ i ] ) ;
        {$ENDIF}
      end ;

      size := OFFSET_GDO_GUID + sizeOf( DWORD ) ;

      for r := 0 to elems - 1 do
        size := size + Cardinal( VarArrayHighBound( tab[ r ], 1 ) + 1 + 4 ) ;

      _gdo := VarArrayCreate([0,size-1], varByte ) ;
      {$IFDEF MANAGED}
        {$IFNDEF OXYGENE}
          assert( VarArrayElementsIsType( OleVariant(_gdo), varByte ) ) ;
        {$ENDIF}
        vr := TBytes(_gdo);
        vr_off := 0  ;
      {$ELSE}
        ptvar := VarArrayLock( _gdo ) ;
      {$ENDIF}

      // Geometry Collection
      // GUID: "0FD2FFC6-8CBC-11CF-DEAB-08003601B769"
      value := $0FD2FFC6 ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $11CF8CBC ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $0008DEAB ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}
      value := $69B70136 ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}

      value := elems ;
      {$IFDEF MANAGED}
        write_dword( vr, vr_off, value ) ;
      {$ELSE}
        write_gdo( ptvar, value, 4 ) ;
      {$ENDIF}

      for r := 0 to elems - 1 do begin
        {$IFDEF MANAGED}
          ptvar2 := TBytes( tab[ r ] ) ;
        {$ELSE}
          ptvar2   := VarArrayLock( tab[ r ] ) ;
        {$ENDIF}
        sizeTab := VarArrayHighBound( tab[ r ], 1 ) + 1 ;

        value := sizeTab ;
        {$IFDEF MANAGED}
          write_dword( vr, vr_off, value ) ;
        {$ELSE}
          write_gdo( ptvar, value, 4 ) ;
        {$ENDIF}

        {$IFDEF MANAGED}
          write_gdo( vr, vr_off, ptvar2, sizeTab ) ;
        {$ELSE}
          write_gdo( ptvar, ptvar2^, sizeTab ) ;
        {$ENDIF}

        {$IFDEF MANAGED}
        {$ELSE}
          VarArrayUnLock( tab[ r ] ) ;
        {$ENDIF}
      end ;

      {$IFNDEF MANAGED}
        VarArrayUnLock( _gdo ) ;
      {$ENDIF}
      tab := nil ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisExportComplexToJSON(
    const _shp : TGIS_Shape
  ) : String ;
  var
    i   : Integer ;
    cnt : Integer ;
    str : String ;
  begin
    Result := '' ;
    if not assigned( _shp ) then exit ;

    cnt := TGIS_ShapeComplex( _shp ).ShapesCount - 1 ;
    str := '' ;

    for i := 0 to cnt do begin
      str := str + TGIS_ShapeComplex( _shp ).GetShape( i ).ExportToJSON ;

      if i < cnt then
        str := str + ',' ;
    end ;

    Result := Format( '{"type":"GeometryCollection","geometries": [%s]}',
                      [str]
                    ) ;
  end ;

  class procedure TGIS_GeometryFactory.GisExportMultiPatchToVAR(
    const _shp : TGIS_Shape ;
    {$IFDEF MANAGED}
      var _var : Object
    {$ELSE}
      var _var : OleVariant
    {$ENDIF}
  ) ;
  var
    {$IFDEF MANAGED}
      vr     : TBytes  ;
      vr_off : Integer ;
    {$ELSE}
      pbuf   : Pointer ;
    {$ENDIF}
    val      : Double ;
    size     : Integer ;
  begin
    if _shp.IsEmpty then exit ;

    // calculate size
    size := _shp.GeometrySize ;
    if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then
      size := size + 16 + _shp.PointsZSize + 16 + _shp.PointsMSize
    else if _shp.Dimension in [ TGIS_DimensionType.XYM ] then
      size := size + 16 + _shp.PointsMSize ;

    _var := VarArrayCreate([0,size], varByte ) ;
    {$IFDEF MANAGED}
      vr := TBytes(TObject(_var));
      vr_off := 0 ;
    {$ELSE}
      pbuf  := VarArrayLock( _var ) ;
    {$ENDIF}
    try
      {$IFDEF MANAGED}
        write_wkb( vr, vr_off, _shp.Parts, _shp.PartsSize ) ;
        write_wkb( vr, vr_off, _shp.PartTypes, _shp.PartTypesSize ) ;
        write_wkb( vr, vr_off, _shp.Points, _shp.PointsSize ) ;
      {$ELSE}
        write_wkb( pbuf, _shp.Parts^, _shp.PartsSize ) ;
        write_wkb( pbuf, _shp.PartTypes^, _shp.PartTypesSize ) ;
        write_wkb( pbuf, _shp.Points^, _shp.PointsSize ) ;
      {$ENDIF}

      if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then begin
        val := _shp.PointsZMin ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := _shp.PointsZMax ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_wkb( vr, vr_off, _shp.PointsZ, _shp.PointsZSize ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_wkb( pbuf, _shp.PointsZ^, _shp.PointsZSize ) ;
        {$ENDIF}
      end ;

      if _shp.Dimension in [ TGIS_DimensionType.XYM, TGIS_DimensionType.XYZM ] then begin
        val := _shp.PointsMMin ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := _shp.PointsMMax ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_wkb( vr, vr_off, _shp.PointsM, _shp.PointsMSize ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_wkb( pbuf, _shp.PointsM^, _shp.PointsMSize ) ;
        {$ENDIF}
      end
      else if ( _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] ) then
      begin
        val := 0 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
        {$ENDIF}
        val := 0 ;
        {$IFDEF MANAGED}
          write_wkb( vr, vr_off, BitConverter.GetBytes(val), sizeOf(Double) ) ;
          write_fakebuf( vr, vr_off, _shp.PointsZSize  ) ;
        {$ELSE}
          write_wkb( pbuf, val, sizeOf ( Double ) ) ;
          write_fakebuf( pbuf, _shp.PointsZSize  ) ;
        {$ENDIF}
      end ;
    finally
      {$IFNDEF MANAGED}
        VarArrayUnlock( _var ) ;
      {$ENDIF}
    end ;

  end ;

  class function TGIS_GeometryFactory.GisCreateReprojectedShape(
    const _shp    : TGIS_Shape              ;
    const _src_cs : TGIS_CSCoordinateSystem ;
    const _dst_cs : TGIS_CSCoordinateSystem
  ) : TGIS_Shape ;
  var
    part_no    : Integer      ;
    point_no   : Integer      ;
    i          : Integer      ;
    ptg        : TGIS_Point3D ;
    btransform : Boolean      ;
  begin
    Result := nil ;

    if not assigned( _shp    ) or
       not assigned( _src_cs ) or
       not assigned( _dst_cs ) then exit ;

    case _shp.ShapeType of
      TGIS_ShapeType.Point       : Result := TGIS_ShapePoint.Create(
                                            nil, nil, False, _shp.Uid, nil,
                                            _shp.Dimension
                                          ) ;
      TGIS_ShapeType.MultiPoint  : Result := TGIS_ShapeMultiPoint.Create(
                                            nil, nil, False, _shp.Uid, nil,
                                            _shp.Dimension
                                          ) ;
      TGIS_ShapeType.Arc         : Result := TGIS_ShapeArc.Create(
                                            nil, nil, False, _shp.Uid, nil,
                                            _shp.Dimension
                                          ) ;
      TGIS_ShapeType.Polygon     : Result := TGIS_ShapePolygon.Create(
                                            nil, nil, False, _shp.Uid, nil,
                                            _shp.Dimension
                                          ) ;
      TGIS_ShapeType.MultiPatch  : Result := TGIS_ShapeMultiPatch.Create(
                                            nil, nil, False, _shp.Uid, nil,
                                            _shp.Dimension
                                          ) ;
      TGIS_ShapeType.Complex     : Result := TGIS_ShapeComplex.Create(
                                            nil, nil, False, _shp.Uid, nil,
                                            _shp.Dimension
                                          ) ;
    end ;

    if assigned( Result ) then begin
      if Result.ShapeType = TGIS_ShapeType.Complex then begin
        for i := 0 to TGIS_ShapeComplex( _shp ).ShapesCount - 1 do
          TGIS_ShapeComplex( Result ).AddShape(
            GisCreateReprojectedShape(
              TGIS_ShapeComplex( _shp ).GetShape( i ),
              _src_cs, _dst_cs
            )
          ) ;
      end
      else begin
        Result.Lock( TGIS_Lock.Internal ) ;
        _shp.Lock( TGIS_Lock.Internal ) ;
        try
          btransform := assigned( _shp.Layer ) and
                        assigned( _shp.Layer.Transform ) and
                        _shp.Layer.Transform.Active ;

          for part_no := 0 to _shp.GetNumParts - 1 do begin
            Result.AddPart;
            for point_no := 0 to _shp.GetPartSize( part_no ) - 1 do begin
              ptg := _shp.GetPoint3D( part_no, point_no) ;
              if btransform then
                 _shp.Layer.Transform.Transform3D_Ref( ptg ) ;

              ptg := _dst_cs.FromCS3D( _src_cs, ptg ) ;
              if not GisIsValidPtg3D( ptg ) then
                raise EGIS_Exception.Create(
                        _rsrc( GIS_RS_ERR_PRJ_NOTSOLVABLE ), '', 0
                      ) ;
              Result.AddPoint3D( ptg ) ;
            end ;
          end ;
        finally
          Result.Unlock ;
          _shp.Unlock ;
        end ;
      end ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisBuildShapeFromEdges(
    const _edges     : TGIS_ObjectList ;
    const _shapeType : TGIS_ShapeType ;
    const _tolerance : Double ;
    const _source    : TGIS_Shape ;
   {$IFDEF MANAGED}
     const _ptr      : TGIS_Bytes ;
   {$ELSE}
     const _ptr      : Pointer    ;
   {$ENDIF}
    const _mapped    : Boolean    ;
    const _uid       : TGIS_Uid    ;
    const _layer     : TGIS_LayerVector ;
    const _fixShape  : Boolean
  ) : TGIS_Shape ;
  var
    nEdges          : Integer ;
    nLeftEdges      : Integer ;
    iEdge           : Integer ;
    iRing           : Integer ;
    edgeConsumed    : array of Boolean ;
    bWorkDone       : Boolean ;
    bestDist        : Double ;
    dis             : Double ;
    iBestEdge       : Integer ;
    bReverse        : Boolean ;
    poLine          : TGIS_ShapeArc ;
    tpl             : TGIS_Topology ;
    dim             : TGIS_DimensionType ;
    res             : TGIS_Shape ;

      function checkPoints( const _poLine1 : TGIS_Shape ;
                            const _iPart1  : Integer ;
                            const _iPoint1 : Integer ;
                            const _poLine2 : TGIS_Shape ;
                            const _iPart2  : Integer ;
                            const _iPoint2 : Integer ;
                              var _distance: Double
                           ) : Boolean ;
      var
        dfDeltaX, dfDeltaY, dfDistance :  Double ;
        ptg1, ptg2 : TGIS_Point ;
      begin
        ptg1 := _poLine1.GetPoint( _iPart1, _iPoint1 ) ;
        ptg2 := _poLine2.GetPoint( _iPart2, _iPoint2 ) ;

        if _distance = 0 then begin
          Result := ( ptg1.X = ptg2.X ) and ( ptg1.Y = ptg2.Y ) ;
          exit ;
        end ;

        dfDeltaX := Abs( ptg1.X - ptg2.X ) ;
        dfDeltaY := Abs( ptg1.Y - ptg2.Y ) ;

        if ( dfDeltaX > _distance ) or ( dfDeltaY > _distance ) then begin
          Result := False ;
          exit ;
        end ;

        dfDistance := Sqrt( dfDeltaX * dfDeltaX + dfDeltaY * dfDeltaY ) ;

        if ( dfDistance < _distance ) then begin
          _distance := dfDistance ;
          Result := True ;
          exit ;
        end
        else
           Result := False ;
      end ;

      procedure addEdgeToRing( const _edge    : TGIS_ShapeArc ;
                               const _reverse : Boolean
                             ) ;
      var
        nVertToAdd, i : Integer ;
      begin
        nVertToAdd := _edge.GetPartSize( 0 ) ;

        if not _reverse then begin
          for i := 0 to nVertToAdd - 1 do
            res.AddPoint3D( _edge.GetPoint3D( 0, i ) ) ;
        end
        else begin
          for i := nVertToAdd - 1 downto 0 do
            res.AddPoint3D( _edge.GetPoint3D( 0, i ) ) ;
        end ;
      end ;

  begin
    Result := nil ;
    res    := nil ;

    if ( _edges = nil ) or ( _edges.Count = 0 ) then exit ;

    nEdges     := _edges.Count ;
    nLeftEdges := nEdges ;
    iRing      := -1 ;
    SetLength( edgeConsumed, nEdges ) ;

    if nEdges > 0 then
      dim := TGIS_ShapeArc( _edges[ 0 ] ).Dimension
    else
      dim := TGIS_DimensionType.XY ;

    case _shapeType of
      TGIS_ShapeType.Arc:
        res := TGIS_ShapeArc.Create( _source, _ptr, _mapped, _uid, _layer, dim ) ;
      TGIS_ShapeType.Polygon:
        res := TGIS_ShapePolygon.Create( _source, _ptr, _mapped, _uid, _layer, dim )
    else
      exit ;
    end ;

    try
      res.Lock( TGIS_Lock.Projection ) ;
      while ( nLeftEdges > 0 ) do begin
        iEdge := 0 ;
        while edgeConsumed[iEdge] do inc( iEdge ) ;

        poLine := TGIS_ShapeArc( _edges[ iEdge ] ) ;
        assert( TGIS_Shape( _edges[ iEdge ] ).ShapeType = TGIS_ShapeType.Arc ) ;

        res.AddPart ;
        inc( iRing ) ;
        addEdgeToRing( poLine, False ) ;
        edgeConsumed[iEdge] := TRUE ;
        dec( nLeftEdges ) ;
        bWorkDone := TRUE ;
        bestDist  := _tolerance ;
        dis       := 0 ;

        while ( not checkPoints( res, 0, iRing, res,
                                 iRing, res.GetPartSize( iRing )-1, dis ) and
               ( nLeftEdges > 0 ) and bWorkDone ) do
        begin
          iBestEdge  := -1 ;
          bReverse   := False ;
          bWorkDone  := False ;
          bestDist := _tolerance ;

          for iEdge := 0 to nEdges-1 do begin
            if ( edgeConsumed[iEdge] ) then continue ;
            poLine := TGIS_ShapeArc( _edges[ iEdge ] ) ;

            if checkPoints( poLine, 0, 0, res, iRing, res.GetPartSize( iRing )-1,
                            bestDist ) then begin
              iBestEdge := iEdge ;
              bReverse  := False ;
            end ;
            if checkPoints( poLine, 0, poLine.GetPartSize( 0 )-1,
                            res, iRing, res.GetPartSize( iRing )-1,
                            bestDist ) then begin
              iBestEdge := iEdge ;
              bReverse  := True ;
            end
          end ;

          if ( iBestEdge <> -1 ) then begin
            poLine := TGIS_ShapeArc( _edges[ iBestEdge ] ) ;
            addEdgeToRing( poLine, bReverse ) ;
            edgeConsumed[iBestEdge] := True ;
            dec( nLeftEdges ) ;
            bWorkDone := True ;
          end
        end ;

        bestDist := _tolerance ;
      end ;
    finally
      res.Unlock ;
      Result := res ;
    end ;

    if _fixShape then begin
      tpl := TGIS_Topology.Create ;
      try
        tpl.FixShape( Result ) ;
      finally
        FreeObject( tpl ) ;
      end ;
    end ;
  end ;

  class function TGIS_GeometryFactory.GisCircleFrom3Points(
    const _ptg1   : TGIS_Point ;
    const _ptg2   : TGIS_Point ;
    const _ptg3   : TGIS_Point ;
    var _center   : TGIS_Point ;
    var _radius   : Double     ;
    var _start    : Double     ;
    var _stop     : Double
  ) : Boolean;
  var
    ma, mb     : Double     ;
    p1, p2, p3 : TGIS_Point ;

    procedure swap_ptg( var _ptga, _ptgb : TGIS_Point ) ;
    var
      tmp : TGIS_Point;
    begin
      tmp   := _TGIS_Point( _ptga ) ;
      _ptga := _TGIS_Point( _ptgb ) ;
      _ptgb := tmp   ;
    end ;

    function get_radius : Double;
    var
      s, a, b, c : Double;
    begin
      a := Sqrt( Sqr( p1.X - p2.X ) + Sqr( p1.Y - p2.Y ) );
      b := Sqrt( Sqr( p2.X - p3.X ) + Sqr( p2.Y - p3.Y ) );
      c := Sqrt( Sqr( p3.X - p1.X ) + Sqr( p3.Y - p1.Y ) );
      s := ( a + b + c ) / 2;

      Result := ( a * b * c ) / ( 4 * Sqrt( s*( s-a )*( s-b )*( s-c ) ) );
    end ;

  begin
    ma      := 0    ;
    mb      := 0    ;
    Result  := True ;

    p1 := _ptg1 ;
    p2 := _ptg2 ;
    p3 := _ptg3 ;

    {$IFDEF GIS_NORECORDS}
      _center := new TGIS_Point ;
    {$ENDIF}
    if ( Abs(p1.X - p3.X) < 1.0e-8 ) and
       ( Abs(p1.Y - p3.Y) < 1.0e-8 ) then begin

      _center.X := p1.X + (p2.X - p1.X) / 2.0 ;
      _center.Y := p1.Y + (p2.Y - p1.Y) / 2.0 ;
      _radius   := Sqrt( (_center.X - p1.X) * (_center.X - p1.X) +
                         (_center.Y - p1.Y) * (_center.Y - p1.Y)
                        ) ;
    end
    else begin
      if ( p1.X = p2.X ) or ( p1.Y = p2.Y ) then
        swap_ptg( p2, p3 ) ;
      if ( p2.X = p3.X ) then
        swap_ptg( p1, p2 ) ;

      if p1.X <> p2.X then
        ma := ( p2.Y - p1.Y ) / ( p2.X - p1.X )
      else
        Result := false ;

      if p2.X <> p3.X then
        mb := ( p3.Y - p2.Y ) / ( p3.X - p2.X )
      else
        Result := false ;

      if ( ( ma = 0 ) and ( mb = 0 ) ) or ( Abs( mb - ma ) < 1e-8 ) then
        Result := false ;

      if Result then begin
        _center.X := ( ma * mb * ( p1.Y - p3.Y ) + mb * ( p1.X + p2.X ) -
                       ma * ( p2.X + p3.X )
                      ) / ( 2 * ( mb - ma ) ) ;
        if ma <> 0 then
          _center.Y := -( _center.X - ( p1.X + p2.X ) / 2 ) / ma +
                        ( p1.Y + p2.Y ) / 2
        else
          _center.Y := -( _center.X - ( p2.X + p3.X ) / 2 ) / mb +
                        ( p2.Y + p3.Y ) / 2 ;

        _radius := get_radius ;
      end ;
    end ;

    _start := 0    ;
    _stop  := 2*Pi ;
  end ;

  class function TGIS_GeometryFactory.GisCircleFrom3Points3D(
    const _ptg1   : TGIS_Point3D ;
    const _ptg2   : TGIS_Point3D ;
    const _ptg3   : TGIS_Point3D ;
    var _center   : TGIS_Point3D ;
    var _radius   : Double     ;
    var _start    : Double     ;
    var _stop     : Double
  ) : Boolean ;
  var
    ma, mb     : Double     ;
    p1, p2, p3 : TGIS_Point3D ;

    procedure swap_ptg( var _ptga, _ptgb : TGIS_Point3D ) ;
    var
      tmp : TGIS_Point3D ;
    begin
      tmp   := _TGIS_Point3D( _ptga ) ;
      _ptga := _TGIS_Point3D( _ptgb ) ;
      _ptgb := tmp   ;
    end ;

    function get_radius : Double ;
    var
      s, a, b, c, e : Double ;
      sa, sb, sc : Double ;
    begin
      a := Sqrt( Sqr( p1.X - p2.X ) + Sqr( p1.Y - p2.Y ) );
      b := Sqrt( Sqr( p2.X - p3.X ) + Sqr( p2.Y - p3.Y ) );
      c := Sqrt( Sqr( p3.X - p1.X ) + Sqr( p3.Y - p1.Y ) );
      s := ( a + b + c ) / 2;
      sa := ( s-a ) ;
      if sa = 0 then
        sa := 1 ;
      sb := ( s-b ) ;
      if sb = 0 then
        sb := 1 ;
      sc := ( s-c ) ;
      if sc = 0 then
        sc := 1 ;

      e := ( 4 * Sqrt( s*sa*sb*sc ) ) ;
      if e <> 0 then
        Result := ( a * b * c ) / e
      else
        Result := 0
    end ;

  begin
    ma      := 0    ;
    mb      := 0    ;
    Result  := True ;

    p1 := _ptg1 ;
    p2 := _ptg2 ;
    p3 := _ptg3 ;

    {$IFDEF GIS_NORECORDS}
      _center := new TGIS_Point3D ;
    {$ENDIF}
    if ( Abs(p1.X - p3.X) < 1.0e-8 ) and
       ( Abs(p1.Y - p3.Y) < 1.0e-8 ) then begin

      _center.X := p1.X + (p2.X - p1.X) / 2.0 ;
      _center.Y := p1.Y + (p2.Y - p1.Y) / 2.0 ;
      _center.Z := p1.Z + (p2.Z - p1.Z) / 2.0 ;
      _radius   := Sqrt( (_center.X - p1.X) * (_center.X - p1.X) +
                         (_center.Y - p1.Y) * (_center.Y - p1.Y)
                        ) ;
    end
    else if ( Abs(p2.X - p3.X) < 1.0e-8 ) and
            ( Abs(p2.Y - p3.Y) < 1.0e-8 ) then begin

      if Abs((p1.X - p2.X)*(p2.Y - p3.Y) - (p2.X - p3.X)*(p1.Y - p2.Y)) < 1.0e-8
      then begin
        // Points are collinear
        Result := false ;

        _center.X := p1.X ;
        _center.Y := p1.Y ;
        _center.Z := p1.Z ;
        _radius   := 0 ;
      end
      else begin
        _center.X := p1.X + (p2.X - p1.X) / 2.0 ;
        _center.Y := p1.Y + (p2.Y - p1.Y) / 2.0 ;
        _center.Z := p1.Z + (p2.Z - p1.Z) / 2.0 ;
        _radius   := Sqrt( (_center.X - p1.X) * (_center.X - p1.X) +
                           (_center.Y - p1.Y) * (_center.Y - p1.Y)
                          ) ;
      end
    end
    else begin
      if ( p1.X = p2.X ) or ( p1.Y = p2.Y ) then
        swap_ptg( p2, p3 ) ;
      if ( p2.X = p3.X ) then
        swap_ptg( p1, p2 ) ;

      if (p1.X <> p2.X) then
        ma := ( p2.Y - p1.Y ) / ( p2.X - p1.X )
      else
        Result := false ;

      if (p2.X <> p3.X) then
        mb := ( p3.Y - p2.Y ) / ( p3.X - p2.X )
      else
        Result := false ;

      if ( ( ma = 0 ) and ( mb = 0 ) ) or ( Abs( mb - ma ) < 1e-8 ) then
        Result := false ;

      if Abs((p1.X - p2.X)*(p2.Y - p3.Y) - (p2.X - p3.X)*(p1.Y - p2.Y)) < 1.0e-8
      then
        Result := false ;  // Points are collinear

      if Result then begin
        _center.X := ( ma * mb * ( p1.Y - p3.Y ) + mb * ( p1.X + p2.X ) -
                       ma * ( p2.X + p3.X )
                      ) / ( 2 * ( mb - ma ) ) ;
        if ma <> 0 then
          _center.Y := -( _center.X - ( p1.X + p2.X ) / 2 ) / ma +
                        ( p1.Y + p2.Y ) / 2
        else
          _center.Y := -( _center.X - ( p2.X + p3.X ) / 2 ) / mb +
                        ( p2.Y + p3.Y ) / 2 ;
        _center.Z := p1.Z + (p3.Z - p2.Z) / 2.0 ;

        _radius := get_radius ;
      end ;
    end ;

    _start := 0    ;
    _stop  := 2*Pi ;
  end ;

  // Calculate start and stop angle for arc based on three points.
  // _ptg1   first provided point
  // _ptg2   second provided point
  // _ptg3   third provided point
  // _center center of the calculated string
  // _start  start angle for pie
  // _stop   stop angle for pie
  procedure calculateArcAngles(  const _ptg1   : TGIS_Point ;
                                 const _ptg2   : TGIS_Point ;
                                 const _ptg3   : TGIS_Point ;
                                 const _center : TGIS_Point ;
                                   var _start  : Double     ;
                                   var _stop   : Double
                                ) ;
  var
    p1_angle,
    p2_angle,
    p3_angle  : Double ;
  begin
    p1_angle := -1*ArcTan2( _ptg1.Y - _center.Y , _ptg1.X - _center.X ) ;
    p2_angle := -1*ArcTan2( _ptg2.Y - _center.Y , _ptg2.X - _center.X ) ;
    p3_angle := -1*ArcTan2( _ptg3.Y - _center.Y , _ptg3.X - _center.X ) ;

    // Try positive (clockwise?) winding.
    while( p2_angle < p1_angle ) do
      p2_angle := p2_angle + 2*Pi ;

    while( p3_angle < p2_angle ) do
      p3_angle := p3_angle + 2*Pi ;

    // If that doesn't work out, then go anticlockwise.
    if( p3_angle - p1_angle > 2*Pi ) then begin
      while( p2_angle > p1_angle ) do
        p2_angle := p2_angle - 2*Pi ;

      while( p3_angle > p2_angle ) do
        p3_angle := p3_angle - 2*Pi ;
    end ;

    _start := p1_angle ;
    _stop  := p3_angle ;
  end ;

  class function TGIS_GeometryFactory.GisArcFrom3Points(
    const _ptg1   : TGIS_Point ;
    const _ptg2   : TGIS_Point ;
    const _ptg3   : TGIS_Point ;
    var _center   : TGIS_Point ;
    var _radius   : Double     ;
    var _start    : Double     ;
    var _stop     : Double
   ) : Boolean ;
  begin
    Result := GisCircleFrom3Points( _ptg1, _ptg2, _ptg3, _center, _radius,
                                    _start, _stop
                                   ) ;
    if Result then
      calculateArcAngles( _ptg1, _ptg2, _ptg3, _center, _start, _stop ) ;
  end ;

  class function TGIS_GeometryFactory.GisArcFrom3Points3D(
    const _ptg1   : TGIS_Point3D ;
    const _ptg2   : TGIS_Point3D ;
    const _ptg3   : TGIS_Point3D ;
    var _center   : TGIS_Point3D ;
    var _radius   : Double     ;
    var _start    : Double     ;
    var _stop     : Double
  ) : Boolean ;
  begin
    Result := GisCircleFrom3Points3D( _ptg1, _ptg2, _ptg3, _center, _radius,
                                      _start, _stop
                                     ) ;
    if Result then
      calculateArcAngles( GisPoint2DFrom3D( _ptg1 ),
                          GisPoint2DFrom3D( _ptg2 ),
                          GisPoint2DFrom3D( _ptg3 ),
                          GisPoint2DFrom3D( _center ),
                          _start,
                          _stop
                         ) ;
  end ;

  class function TGIS_GeometryFactory.GisPolygonPartsStatus(
    const _shp    : TGIS_Shape ;
    var _parts    : TGIS_IntegerArray ;
    var _winding  : TGIS_IntegerArray
  ) : Boolean ;

    const
      PART_STATUS_MAIN =  0 ;
      PART_STATUS_HOLE =  1 ;
      PART_WINDING_CW  =  1 ;
      PART_WINDING_CCW = -1 ;

    var
      count     : Integer   ;
      r, rings  : Integer   ;
      num       : Integer   ;
      part_no   : Integer   ;
      int_count : Integer   ;

    function isExtContainIntPart(
      const _extPartNo : Integer ;
      const _intPartNo : Integer
    ) : Boolean ;
    var
      point_no     : Integer ;
      points_count : Integer ;
      next_pt      : Integer ;
      npar, p1     : Integer ;
      line_a       : TGIS_Point ;
      line_b       : TGIS_Point ;
      ptg1         : TGIS_Point ;
    begin
      Result := False ;

      for p1 := 0 to _shp.GetPartSize( _intPartNo )-1 do begin

        ptg1 := _shp.GetPoint( _intPartNo, p1 ) ;
        with _shp.Extent do begin
          if (XMax   = Xmin) and (YMax   = YMin) and
             (ptg1.X = Xmin) and (ptg1.X = YMin) then begin
            Result := True ;
            exit ;
          end ;
        end ;

        points_count := _shp.GetPartSize( _extPartNo ) ;
        next_pt := points_count - 1 ;
        npar := 0 ;
        for point_no:=0 to points_count-1 do  begin // all points
          line_a := _shp.GetPoint( _extPartNo, point_no ) ;
          line_b := _shp.GetPoint( _extPartNo, next_pt  ) ;
          next_pt := point_no ;
          try
            if ( ( ( ( line_a.Y < ptg1.Y ) and ( ptg1.Y < line_b.Y ) ) or
                   ( ( line_b.Y < ptg1.Y ) and ( ptg1.Y < line_a.Y ) )
                 ) and
                 ( ptg1.X < ( line_b.X - line_a.X ) * ( ptg1.Y - line_a.Y ) /
                            ( line_b.Y - line_a.Y ) + line_a.X
                 )
               ) then
              npar := npar + 1 ;
          except
          end ;
        end ;

        if ( npar mod 2) = 1 then
          Result := True ; // even - point is inside part

        if not Result then Break ;
      end ;
    end ;

    procedure calcBoundary(
      const _partNo   : Integer ;
      var   _intCount : Integer
    ) ;
    var
      k : Integer   ;
    begin
      if ( _partNo > rings-1 ) then exit ;
      _intCount := 0 ;
      for k := _partNo to rings-1 do begin
        if ( k = _partNo ) then
        else if isExtContainIntPart( _partNo, k ) then
          inc( _intCount )
        else
          Break ;
      end ;
    end ;

    procedure calcCollection(
      var _count : Integer
    ) ;
    var
      k           : Integer   ;
      extNo       : Integer   ;
      newExterior : Boolean ;
    begin
      _count      := 0 ;
      extNo       := 0 ;
      newExterior := True ;
      for k := 0 to rings-1 do begin
        if newExterior then begin
          extNo := k ;
          inc( _count ) ;
          newExterior := False ;
        end ;
        if k < rings-1 then
          newExterior := not isExtContainIntPart( extNo, k+1 ) ;
      end ;
    end ;

    function isPartClockwise(
      const _partNo : Integer
    ) : Boolean ;
    var
      point_no     : Integer ;
      next_pt      : Integer ;
      area2        : Double ;
      line_a       : TGIS_Point ;
      line_b       : TGIS_Point ;
      point_origin : TGIS_Point ;
    begin
      Result := False ;
      area2 := 0 ;
      point_origin := _shp.GetPoint( _partNo, 0 ) ;

      for point_no := 0 to _shp.GetPartSize( _partNo ) - 2 do begin
        next_pt := point_no +1;
        line_a   := _shp.GetPoint( _partNo, point_no) ;
        line_a.X := line_a.X - point_origin.X ;
        line_a.Y := line_a.Y - point_origin.Y ;
        line_b   := _shp.GetPoint( _partNo, next_pt ) ;
        line_b.X := line_b.X - point_origin.X ;
        line_b.Y := line_b.Y - point_origin.Y ;
        area2    := area2 + ( line_b.Y*line_a.X - line_a.Y*line_b.X ) ;
      end ;

      if area2 < 0 then
        Result := True ;
    end ;


  begin
    _parts  := nil ;
    Result  := False ;

    if not assigned( _shp ) then exit ;
    if _shp.IsEmpty then exit ;

    rings := _shp.GetNumParts ;
    SetLength( _parts, rings ) ;
    SetLength( _winding, rings ) ;

    for r := 0 to rings-1 do
      if isPartClockwise( r ) then
        _winding[r] := PART_WINDING_CW
      else
        _winding[r] := PART_WINDING_CCW ;

    if ( rings = 1 ) then
      _parts[0] := PART_STATUS_MAIN
    else begin
      calcCollection( count ) ;
      if count = 1 then begin
        _parts[0] := PART_STATUS_MAIN ;
        for r := 1 to rings-1 do
          _parts[r] := PART_STATUS_HOLE ;
        exit ;
      end ;

      Result := True ;
      part_no := 0 ;
      for num := 1 to count do begin
        calcBoundary( part_no, int_count ) ;
        if ( int_count > 0 ) then begin
          for r := part_no to part_no + int_count do begin
            if ( r = part_no ) then
              _parts[r] := PART_STATUS_MAIN
            else if isExtContainIntPart( part_no, r ) then
              _parts[r] := PART_STATUS_HOLE ;
          end ;
        end
        else
          _parts[part_no] := PART_STATUS_MAIN ;
        part_no := part_no + int_count + 1 ;
      end ;
    end ;
  end ;

//==================================== END =====================================
end.

