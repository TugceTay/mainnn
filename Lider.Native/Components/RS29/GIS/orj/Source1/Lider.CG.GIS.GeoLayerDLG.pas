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
  Encapsulation of a DLG (Digital Line Graphs) file access.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerDLG ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerDLG"'}
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
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoStreams ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerDLG = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Layer which can read &amp; write a DLG (Digital Line Graphs) file.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     <list type="bullet">
  ///       <item>
  ///         Only .OPT extension can be used for saving.
  ///       </item>
  ///       <item>
  ///         Supported shape types: TGIS_ShapeType.Point,
  ///         TGIS_ShapeType.Arc
  ///       </item>
  ///       <item>
  ///         Upon importing shape of type TGIS_ShapeType.MultiPoint the
  ///         shape will be converted to a set of TGIS_ShapeType.Point
  ///         shapes.
  ///       </item>
  ///       <item>
  ///         Upon importing shapes of type TGIS_ShapeType.Polygon only
  ///         the outline of shape will be imported as a
  ///         TGIS_ShapeType.Arc shape.
  ///       </item>
  ///     </list>
  ///   </note>
  /// </remarks>
  TGIS_LayerDLG = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private

        /// <summary>
        ///   Shapes file.
        /// </summary>
        FdlgFile   : TGIS_BufferedFileStream ;

        /// <summary>
        ///   Shapes file line.
        /// </summary>
        FdlgRecNo  : Integer ;

        /// <summary>
        ///   Number of nodes
        /// </summary>
        FNodeCount : Integer;

        /// <summary>
        ///   Number of areas
        /// </summary>
        FAreaCount : Integer;

        /// <summary>
        ///   Number of lines
        /// </summary>
        FLineCount : Integer;

        /// <summary>
        ///   Current shape
        /// </summary>
        oShp       : TGIS_Shape ;

        /// <summary>
        ///   Textual information about the file.
        /// </summary>
        sCategory  : String;

        /// <summary>
        ///   Recognized file format - for DLG opt is true
        /// </summary>
        bDlgOpt    : Boolean;

    private

      /// <summary>
      ///   Read line from DLG file. Record is from #A to #A char or 80 chars
      ///   long.
      /// </summary>
      function  readRec        ( out _str        : String
                               ) : Boolean;

      /// <summary>
      ///   Read category from DLG file.
      /// </summary>
      procedure readCategory   ;

      /// <summary>
      ///   Add attributes
      /// </summary>
      /// <param name="_tkn">
      ///   tokenizer object
      /// </param>
      procedure addAttributes  ( const _tkn      : TGIS_Tokenizer
                               ) ;

      /// <summary>
      ///   Create a multi-element TGIS_ShapeArc.
      /// </summary>
      /// <param name="_param">
      ///   string with number of coordinate pairs
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure buildLine      ( const _t        : String           ;
                                 const _param    : String
                               ) ;

      /// <summary>
      ///   Create TGIS_ShapePoint.
      /// </summary>
      /// <param name="_t">
      ///   feature type
      /// </param>
      /// <param name="_x">
      ///   longitude
      /// </param>
      /// <param name="_y">
      ///   latitude
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure buildPoint     ( const _t        : String           ;
                                 const _x        : String           ;
                                 const _y        : String
                               ) ;

      /// <summary>
      ///   Write DLG header and category.
      /// </summary>
      /// <param name="_file">
      ///   file to be written
      /// </param>
      /// <param name="_layer">
      ///   layer for which the header will be written
      /// </param>
      /// <param name="_nodes">
      ///   number of nodes
      /// </param>
      /// <param name="_areas">
      ///   number of areas
      /// </param>
      procedure writeHeader    ( const _file     : TGIS_Stream      ;
                                 const _layer    : TGIS_LayerVector ;
                                 const _nodes    : Integer          ;
                                 const _areas    : Integer          ;
                                 const _lines    : Integer
                               ) ;

      /// <summary>
      ///   Write shape geometry to the output stream (only points and lines) .
      /// </summary>
      /// <param name="_file">
      ///   file to be written
      /// </param>
      /// <param name="_shp">
      ///   shape for which the data will be written
      /// </param>
      /// <param name="_node">
      ///   index of the number
      /// </param>
      /// <param name="_area">
      ///   index of the area
      /// </param>
      /// <param name="_line">
      ///   index of the line
      /// </param>
      procedure writeGeometry  ( const _file     : TGIS_Stream      ;
                                 const _shp      : TGIS_Shape       ;
                                 var   _node     : Integer          ;
                                 var   _area     : Integer          ;
                                 var   _line     : Integer
                               ) ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

         /// <summary>
         ///   Makes a general layer setup.
         /// </summary>
         procedure setUp       ; override;
    protected
      // destructor

         /// <summary>
         ///   Destroy a layer instance.
         /// </summary>
         procedure doDestroy ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create ; override;
      // new layer builder

         /// <inheritdoc/>
         procedure Build      ( const _path      : String           ;
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
         procedure SaveData   ; override;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.Variants,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoCsProjections,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

type
  T_AttCodes = record
    _ma  : Integer ;
    _mi  : Integer ;
    _de  : String ;
  end;

  { DLG attributes container class.
  }
  {$IFNDEF OXYGENE}
    T_ArraysDLG_class =  class
  {$ELSE}
    T_ArraysDLG = static class
  {$ENDIF}
    public
      AttTabXX  : array [0..28]  of T_AttCodes ;
      AttTab50  : array [0..106] of T_AttCodes ;
      AttTab90  : array [0..30]  of T_AttCodes ;
      AttTab170 : array [0..74]  of T_AttCodes ;
      AttTab180 : array [0..24]  of T_AttCodes ;
      AttTab190 : array [0..22]  of T_AttCodes ;
      AttTab300 : array [0..39]  of T_AttCodes ;
    public
      // <summary>
      //   Prepare all AttTab### arrays.
      // </summary>
      {$IFDEF OXYGENE} class {$ENDIF} constructor Create ;
    private
      // <summary>
      //   Fill AttTabXX array.
      // </summary>
      {$IFDEF OXYGENE} class {$ENDIF} procedure fill_AttTabXX  ;
      // <summary>
      //   Fill AttTab50 array.
      // </summary>
      {$IFDEF OXYGENE} class {$ENDIF} procedure fill_AttTab50  ;
      // <summary>
      //   Fill AttTab90 array.
      // </summary>
      {$IFDEF OXYGENE} class {$ENDIF} procedure fill_AttTab90  ;
      // <summary>
      //   Fill AttTab170 array.
      // </summary>
      {$IFDEF OXYGENE} class {$ENDIF} procedure fill_AttTab170 ;
      // <summary>
      //   Fill AttTab180 array.
      // </summary>
      {$IFDEF OXYGENE} class {$ENDIF} procedure fill_AttTab180 ;
      // <summary>
      //   Fill AttTab190 array.
      // </summary>
      {$IFDEF OXYGENE} class {$ENDIF} procedure fill_AttTab190 ;
      // <summary>
      //   Fill AttTab300 array.
      // </summary>
      {$IFDEF OXYGENE} class {$ENDIF} procedure fill_AttTab300 ;
  end ;

{$IFNDEF OXYGENE}
  var
    // variable called as a class for OXYGENE compatibility to avoid conditional compilation
    // T_ArraysDLG represents a static class in OXYGENE
    T_ArraysDLG : T_ArraysDLG_class ;
{$ENDIF}

const
  // DLG identifiers
     DLG_AREA      = 'A'          ;
     DLG_LINE      = 'L'          ;
     DLG_NODE      = 'N'          ;
  // DLG fields
     MAJOR_FIELD1  = 'DLG_MAJOR1' ;
     MINOR_FIELD1  = 'DLG_MINOR1' ;
     MAJOR_FIELD2  = 'DLG_MAJOR2' ;
     MINOR_FIELD2  = 'DLG_MINOR2' ;
     MAJOR_FIELD3  = 'DLG_MAJOR3' ;
     MINOR_FIELD3  = 'DLG_MINOR3' ;
     DLG_FEA_TYPE  = 'DLG_TYPE'   ;
     DLG_FEA_DESC  = 'DLG_DESC'   ;
  // DLG version extension
     C_OPT_EXT     = '.OPT'       ;
     C_DLG_EXT     = '.DLG'       ;

//=============================================================================
// T_ArraysDLG
//=============================================================================

  {$IFNDEF OXYGENE}
    constructor T_ArraysDLG_class.Create ;
  {$ELSE}
    class constructor T_ArraysDLG.Create ;
  {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
    inherited Create;
    {$ENDIF}

    fill_AttTabXX  ;
    fill_AttTab50  ;
    fill_AttTab90  ;
    fill_AttTab170 ;
    fill_AttTab180 ;
    fill_AttTab190 ;
    fill_AttTab300 ;
  end ;

  {$IFNDEF OXYGENE}
    procedure T_ArraysDLG_class.fill_AttTabXX ;
  {$ELSE}
    class procedure T_ArraysDLG.fill_AttTabXX ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      AttTabXX := new T_AttCodes[29] ;
    {$ENDIF}
    AttTabXX[ 0]._ma := 20  ; AttTabXX[ 0]._mi := -1 ; AttTabXX[ 0]._de := 'Contour' ;
    AttTabXX[ 1]._ma := 51  ; AttTabXX[ 1]._mi := -1 ; AttTabXX[ 1]._de := 'Water Surface Elev.;Whole Feet; 9999 or Less' ;
    AttTabXX[ 2]._ma := 52  ; AttTabXX[ 2]._mi := -1 ; AttTabXX[ 2]._de := 'Water Surface Elev.;Whole Meters' ;
    AttTabXX[ 3]._ma := 54  ; AttTabXX[ 3]._mi := -1 ; AttTabXX[ 3]._de := 'Water Surface Elev.;Whole Feet Greater Than 9999' ;
    AttTabXX[ 4]._ma := 55  ; AttTabXX[ 4]._mi := -1 ; AttTabXX[ 4]._de := 'River Mile Mark' ;
    AttTabXX[ 5]._ma := 56  ; AttTabXX[ 5]._mi := -1 ; AttTabXX[ 5]._de := 'Water Surface Elev.; Whole Feet Below Datum' ;
    AttTabXX[ 6]._ma := 57  ; AttTabXX[ 6]._mi := -1 ; AttTabXX[ 6]._de := 'Water Surface Elev.; Whole Meters Below Datum' ;
    AttTabXX[ 7]._ma := 91  ; AttTabXX[ 7]._mi := -1 ; AttTabXX[ 7]._de := 'State or State Equivalent FIPS Code' ;
    AttTabXX[ 8]._ma := 92  ; AttTabXX[ 8]._mi := -1 ; AttTabXX[ 8]._de := 'County or County Equivalent FIPS Code' ;
    AttTabXX[ 9]._ma := 93  ; AttTabXX[ 9]._mi := -1 ; AttTabXX[ 9]._de := 'Civil Township or Civil Township Equivalent' ;
    AttTabXX[10]._ma := 94  ; AttTabXX[10]._mi := -1 ; AttTabXX[10]._de := 'Civil Township or Civil Township Equivalent' ;
    AttTabXX[11]._ma := 95  ; AttTabXX[11]._mi := -1 ; AttTabXX[11]._de := 'Monument Number' ;
    AttTabXX[12]._ma := 96  ; AttTabXX[12]._mi := -1 ; AttTabXX[12]._de := 'Alphabetic Portion of Any Monument Number' ;
    AttTabXX[13]._ma := 172 ; AttTabXX[13]._mi := -1 ; AttTabXX[13]._de := 'Interstate Route Number' ;
    AttTabXX[14]._ma := 173 ; AttTabXX[14]._mi := -1 ; AttTabXX[14]._de := 'U.S. Route Number' ;
    AttTabXX[15]._ma := 174 ; AttTabXX[15]._mi := -1 ; AttTabXX[15]._de := 'State Route Number' ;
    AttTabXX[16]._ma := 175 ; AttTabXX[16]._mi := -1 ; AttTabXX[16]._de := 'Reservation; Park; or Military Route Number' ;
    AttTabXX[17]._ma := 176 ; AttTabXX[17]._mi := -1 ; AttTabXX[17]._de := 'County Route Number' ;
    AttTabXX[18]._ma := 177 ; AttTabXX[18]._mi := -1 ; AttTabXX[18]._de := 'Alphabetic Portion of Any Route Number' ;
    AttTabXX[19]._ma := 179 ; AttTabXX[19]._mi := -1 ; AttTabXX[19]._de := 'Road or Street, Class 3' ;
    AttTabXX[20]._ma := 181 ; AttTabXX[20]._mi := -1 ; AttTabXX[20]._de := 'Number of Tracks ' ;
    AttTabXX[21]._ma := 301 ; AttTabXX[21]._mi := -1 ; AttTabXX[21]._de := 'Section Number' ;
    AttTabXX[22]._ma := 302 ; AttTabXX[22]._mi := -1 ; AttTabXX[22]._de := 'Township Number North of the Baseline' ;
    AttTabXX[23]._ma := 303 ; AttTabXX[23]._mi := -1 ; AttTabXX[23]._de := 'Township Number South of the Baseline' ;
    AttTabXX[24]._ma := 304 ; AttTabXX[24]._mi := -1 ; AttTabXX[24]._de := 'Range Number East of Principal Meridian' ;
    AttTabXX[25]._ma := 305 ; AttTabXX[25]._mi := -1 ; AttTabXX[25]._de := 'Range Number West of Principal Meridian' ;
    AttTabXX[26]._ma := 306 ; AttTabXX[26]._mi := -1 ; AttTabXX[26]._de := 'Origin of Survey' ;
    AttTabXX[27]._ma := 307 ; AttTabXX[27]._mi := -1 ; AttTabXX[27]._de := 'Identifier; Nonsection' ;
    AttTabXX[28]._ma := 308 ; AttTabXX[28]._mi := -1 ; AttTabXX[28]._de := 'Land Grant; Location or Mineral Monument Number' ;
  end ;

  {$IFNDEF OXYGENE}
    procedure T_ArraysDLG_class.fill_AttTab50 ;
  {$ELSE}
    class procedure T_ArraysDLG.fill_AttTab50 ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      AttTab50 := new T_AttCodes[107] ;
    {$ENDIF}
    AttTab50[  0]._ma := 50  ; AttTab50[  0]._mi :=   1 ; AttTab50[  0]._de := 'Upper Origin' ;
    AttTab50[  1]._ma := 50  ; AttTab50[  1]._mi :=   2 ; AttTab50[  1]._de := 'Upper Origin at Water Body' ;
    AttTab50[  2]._ma := 50  ; AttTab50[  2]._mi :=   3 ; AttTab50[  2]._de := 'Sink' ;
    AttTab50[  3]._ma := 50  ; AttTab50[  3]._mi :=   4 ; AttTab50[  3]._de := 'Entering Water Body' ;
    AttTab50[  4]._ma := 50  ; AttTab50[  4]._mi :=   5 ; AttTab50[  4]._de := 'Exiting Water Body' ;
    AttTab50[  5]._ma := 50  ; AttTab50[  5]._mi := 100 ; AttTab50[  5]._de := 'Alkali Flat' ;
    AttTab50[  6]._ma := 50  ; AttTab50[  6]._mi := 101 ; AttTab50[  6]._de := 'Reservoir' ;
    AttTab50[  7]._ma := 50  ; AttTab50[  7]._mi := 102 ; AttTab50[  7]._de := 'Covered Reservoir' ;
    AttTab50[  8]._ma := 50  ; AttTab50[  8]._mi := 103 ; AttTab50[  8]._de := 'Glacier or Permanent Snow Field' ;
    AttTab50[  9]._ma := 50  ; AttTab50[  9]._mi := 104 ; AttTab50[  9]._de := 'Salt Evaporator' ;
    AttTab50[ 10]._ma := 50  ; AttTab50[ 10]._mi := 105 ; AttTab50[ 10]._de := 'Inundation Area' ;
    AttTab50[ 11]._ma := 50  ; AttTab50[ 11]._mi := 106 ; AttTab50[ 11]._de := 'Aquaculture Pond' ;
    AttTab50[ 12]._ma := 50  ; AttTab50[ 12]._mi := 107 ; AttTab50[ 12]._de := 'Industrial Water Impoundment' ;
    AttTab50[ 13]._ma := 50  ; AttTab50[ 13]._mi := 108 ; AttTab50[ 13]._de := 'Area to be Submerged' ;
    AttTab50[ 14]._ma := 50  ; AttTab50[ 14]._mi := 109 ; AttTab50[ 14]._de := 'Sewage Disposal Pond' ;
    AttTab50[ 15]._ma := 50  ; AttTab50[ 15]._mi := 110 ; AttTab50[ 15]._de := 'Tailings Pond' ;
    AttTab50[ 16]._ma := 50  ; AttTab50[ 16]._mi := 111 ; AttTab50[ 16]._de := 'Marsh, Wetland, Swamp or Bog' ;
    AttTab50[ 17]._ma := 50  ; AttTab50[ 17]._mi := 112 ; AttTab50[ 17]._de := 'Mangrove Area' ;
    AttTab50[ 18]._ma := 50  ; AttTab50[ 18]._mi := 114 ; AttTab50[ 18]._de := 'Cranberry Bog' ;
    AttTab50[ 19]._ma := 50  ; AttTab50[ 19]._mi := 115 ; AttTab50[ 19]._de := 'Flat (Tidal, Sand, Gravel, Mud, etcetera)' ;
    AttTab50[ 20]._ma := 50  ; AttTab50[ 20]._mi := 116 ; AttTab50[ 20]._de := 'Bay, Estuary, Gulf, Ocean, or Sea' ;
    AttTab50[ 21]._ma := 50  ; AttTab50[ 21]._mi := 117 ; AttTab50[ 21]._de := 'Shoal' ;
    AttTab50[ 22]._ma := 50  ; AttTab50[ 22]._mi := 118 ; AttTab50[ 22]._de := 'Soda Evaporator' ;
    AttTab50[ 23]._ma := 50  ; AttTab50[ 23]._mi := 119 ; AttTab50[ 23]._de := 'Duck Pond' ;
    AttTab50[ 24]._ma := 50  ; AttTab50[ 24]._mi := 120 ; AttTab50[ 24]._de := 'Void Area' ;
    AttTab50[ 25]._ma := 50  ; AttTab50[ 25]._mi := 121 ; AttTab50[ 25]._de := 'Obstruction Area in Water Area' ;
    AttTab50[ 26]._ma := 50  ; AttTab50[ 26]._mi := 122 ; AttTab50[ 26]._de := 'Gut' ;
    AttTab50[ 27]._ma := 50  ; AttTab50[ 27]._mi := 123 ; AttTab50[ 27]._de := 'Drydock Chamber' ;
    AttTab50[ 28]._ma := 50  ; AttTab50[ 28]._mi := 124 ; AttTab50[ 28]._de := 'Filtration Pond' ;
    AttTab50[ 29]._ma := 50  ; AttTab50[ 29]._mi := 125 ; AttTab50[ 29]._de := 'Foul Ground' ;
    AttTab50[ 30]._ma := 50  ; AttTab50[ 30]._mi := 126 ; AttTab50[ 30]._de := 'Mine Danger Area' ;
    AttTab50[ 31]._ma := 50  ; AttTab50[ 31]._mi := 200 ; AttTab50[ 31]._de := 'Shoreline' ;
    AttTab50[ 32]._ma := 50  ; AttTab50[ 32]._mi := 201 ; AttTab50[ 32]._de := 'Manmade Shoreline' ;
    AttTab50[ 33]._ma := 50  ; AttTab50[ 33]._mi := 202 ; AttTab50[ 33]._de := 'Closure Line' ;
    AttTab50[ 34]._ma := 50  ; AttTab50[ 34]._mi := 203 ; AttTab50[ 34]._de := 'Indefinite Shoreline' ;
    AttTab50[ 35]._ma := 50  ; AttTab50[ 35]._mi := 205 ; AttTab50[ 35]._de := 'Outline of a Carolina Bay' ;
    AttTab50[ 36]._ma := 50  ; AttTab50[ 36]._mi := 206 ; AttTab50[ 36]._de := 'Limiting Danger Line' ;
    AttTab50[ 37]._ma := 50  ; AttTab50[ 37]._mi := 207 ; AttTab50[ 37]._de := 'Apparent Shoreline' ;
    AttTab50[ 38]._ma := 50  ; AttTab50[ 38]._mi := 208 ; AttTab50[ 38]._de := 'Sounding Datum' ;
    AttTab50[ 39]._ma := 50  ; AttTab50[ 39]._mi := 209 ; AttTab50[ 39]._de := 'Low-Water Line' ;
    AttTab50[ 40]._ma := 50  ; AttTab50[ 40]._mi := 210 ; AttTab50[ 40]._de := 'Airboat Trail' ;
    AttTab50[ 41]._ma := 50  ; AttTab50[ 41]._mi := 299 ; AttTab50[ 41]._de := 'Processing Line' ;
    AttTab50[ 42]._ma := 50  ; AttTab50[ 42]._mi := 300 ; AttTab50[ 42]._de := 'Spring' ;
    AttTab50[ 43]._ma := 50  ; AttTab50[ 43]._mi := 301 ; AttTab50[ 43]._de := 'Nonflowing Well' ;
    AttTab50[ 44]._ma := 50  ; AttTab50[ 44]._mi := 302 ; AttTab50[ 44]._de := 'Flowing Well' ;
    AttTab50[ 45]._ma := 50  ; AttTab50[ 45]._mi := 303 ; AttTab50[ 45]._de := 'Riser' ;
    AttTab50[ 46]._ma := 50  ; AttTab50[ 46]._mi := 304 ; AttTab50[ 46]._de := 'Geyser' ;
    AttTab50[ 47]._ma := 50  ; AttTab50[ 47]._mi := 305 ; AttTab50[ 47]._de := 'Windmill' ;
    AttTab50[ 48]._ma := 50  ; AttTab50[ 48]._mi := 400 ; AttTab50[ 48]._de := 'Rapids' ;
    AttTab50[ 49]._ma := 50  ; AttTab50[ 49]._mi := 401 ; AttTab50[ 49]._de := 'Falls' ;
    AttTab50[ 50]._ma := 50  ; AttTab50[ 50]._mi := 403 ; AttTab50[ 50]._de := 'Gaging Station' ;
    AttTab50[ 51]._ma := 50  ; AttTab50[ 51]._mi := 404 ; AttTab50[ 51]._de := 'Pumping Station' ;
    AttTab50[ 52]._ma := 50  ; AttTab50[ 52]._mi := 405 ; AttTab50[ 52]._de := 'Water Intake' ;
    AttTab50[ 53]._ma := 50  ; AttTab50[ 53]._mi := 406 ; AttTab50[ 53]._de := 'Dam or Weir' ;
    AttTab50[ 54]._ma := 50  ; AttTab50[ 54]._mi := 407 ; AttTab50[ 54]._de := 'Lock Chamber' ;
    AttTab50[ 55]._ma := 50  ; AttTab50[ 55]._mi := 408 ; AttTab50[ 55]._de := 'Spillway' ;
    AttTab50[ 56]._ma := 50  ; AttTab50[ 56]._mi := 409 ; AttTab50[ 56]._de := 'Gate' ;
    AttTab50[ 57]._ma := 50  ; AttTab50[ 57]._mi := 410 ; AttTab50[ 57]._de := 'Rock' ;
    AttTab50[ 58]._ma := 50  ; AttTab50[ 58]._mi := 411 ; AttTab50[ 58]._de := 'Crevasse' ;
    AttTab50[ 59]._ma := 50  ; AttTab50[ 59]._mi := 412 ; AttTab50[ 59]._de := 'Stream' ;
    AttTab50[ 60]._ma := 50  ; AttTab50[ 60]._mi := 413 ; AttTab50[ 60]._de := 'Braided Stream' ;
    AttTab50[ 61]._ma := 50  ; AttTab50[ 61]._mi := 414 ; AttTab50[ 61]._de := 'Ditch or Canal' ;
    AttTab50[ 62]._ma := 50  ; AttTab50[ 62]._mi := 415 ; AttTab50[ 62]._de := 'Aqueduct or Pipeline' ;
    AttTab50[ 63]._ma := 50  ; AttTab50[ 63]._mi := 416 ; AttTab50[ 63]._de := 'Flume' ;
    AttTab50[ 64]._ma := 50  ; AttTab50[ 64]._mi := 417 ; AttTab50[ 64]._de := 'Penstock' ;
    AttTab50[ 65]._ma := 50  ; AttTab50[ 65]._mi := 418 ; AttTab50[ 65]._de := 'Siphon' ;
    AttTab50[ 66]._ma := 50  ; AttTab50[ 66]._mi := 419 ; AttTab50[ 66]._de := 'Channel' ;
    AttTab50[ 67]._ma := 50  ; AttTab50[ 67]._mi := 420 ; AttTab50[ 67]._de := 'Wash' ;
    AttTab50[ 68]._ma := 50  ; AttTab50[ 68]._mi := 421 ; AttTab50[ 68]._de := 'Lake or Pond' ;
    AttTab50[ 69]._ma := 50  ; AttTab50[ 69]._mi := 422 ; AttTab50[ 69]._de := 'Reef' ;
    AttTab50[ 70]._ma := 50  ; AttTab50[ 70]._mi := 423 ; AttTab50[ 70]._de := 'Sand in Open Water' ;
    AttTab50[ 71]._ma := 50  ; AttTab50[ 71]._mi := 424 ; AttTab50[ 71]._de := 'Spoil Area; Dredged Area; or Dump Area' ;
    AttTab50[ 72]._ma := 50  ; AttTab50[ 72]._mi := 425 ; AttTab50[ 72]._de := 'Fish Ladders' ;
    AttTab50[ 73]._ma := 50  ; AttTab50[ 73]._mi := 426 ; AttTab50[ 73]._de := 'Holiday Area' ;
    AttTab50[ 74]._ma := 50  ; AttTab50[ 74]._mi := 601 ; AttTab50[ 74]._de := 'Underground' ;
    AttTab50[ 75]._ma := 50  ; AttTab50[ 75]._mi := 602 ; AttTab50[ 75]._de := 'Overpassing' ;
    AttTab50[ 76]._ma := 50  ; AttTab50[ 76]._mi := 603 ; AttTab50[ 76]._de := 'Elevated' ;
    AttTab50[ 77]._ma := 50  ; AttTab50[ 77]._mi := 604 ; AttTab50[ 77]._de := 'Tunnel' ;
    AttTab50[ 78]._ma := 50  ; AttTab50[ 78]._mi := 605 ; AttTab50[ 78]._de := 'Right Bank' ;
    AttTab50[ 79]._ma := 50  ; AttTab50[ 79]._mi := 606 ; AttTab50[ 79]._de := 'Left Bank' ;
    AttTab50[ 80]._ma := 50  ; AttTab50[ 80]._mi := 607 ; AttTab50[ 80]._de := 'Under Construction' ;
    AttTab50[ 81]._ma := 50  ; AttTab50[ 81]._mi := 608 ; AttTab50[ 81]._de := 'Salt' ;
    AttTab50[ 82]._ma := 50  ; AttTab50[ 82]._mi := 609 ; AttTab50[ 82]._de := 'Unsurveyed' ;
    AttTab50[ 83]._ma := 50  ; AttTab50[ 83]._mi := 610 ; AttTab50[ 83]._de := 'Intermittent' ;
    AttTab50[ 84]._ma := 50  ; AttTab50[ 84]._mi := 612 ; AttTab50[ 84]._de := 'Submerged or Sunken' ;
    AttTab50[ 85]._ma := 50  ; AttTab50[ 85]._mi := 614 ; AttTab50[ 85]._de := 'Dry' ;
    AttTab50[ 86]._ma := 50  ; AttTab50[ 86]._mi := 615 ; AttTab50[ 86]._de := 'Mineral or Hot' ;
    AttTab50[ 87]._ma := 50  ; AttTab50[ 87]._mi := 617 ; AttTab50[ 87]._de := 'Underpassing' ;
    AttTab50[ 88]._ma := 50  ; AttTab50[ 88]._mi := 620 ; AttTab50[ 88]._de := 'Decimal Fractions of 0.0 Feet or Meters' ;
    AttTab50[ 89]._ma := 50  ; AttTab50[ 89]._mi := 621 ; AttTab50[ 89]._de := 'Decimal Fractions of 0.1 Feet or Meters' ;
    AttTab50[ 90]._ma := 50  ; AttTab50[ 90]._mi := 622 ; AttTab50[ 90]._de := 'Decimal Fractions of 0.2 Feet or Meters' ;
    AttTab50[ 91]._ma := 50  ; AttTab50[ 91]._mi := 623 ; AttTab50[ 91]._de := 'Decimal Fractions of 0.3 Feet or Meters' ;
    AttTab50[ 92]._ma := 50  ; AttTab50[ 92]._mi := 624 ; AttTab50[ 92]._de := 'Decimal Fractions of 0.4 Feet or Meters' ;
    AttTab50[ 93]._ma := 50  ; AttTab50[ 93]._mi := 625 ; AttTab50[ 93]._de := 'Decimal Fractions of 0.5 Feet or Meters' ;
    AttTab50[ 94]._ma := 50  ; AttTab50[ 94]._mi := 626 ; AttTab50[ 94]._de := 'Decimal Fractions of 0.6 Feet or Meters' ;
    AttTab50[ 95]._ma := 50  ; AttTab50[ 95]._mi := 627 ; AttTab50[ 95]._de := 'Decimal Fractions of 0.7 Feet or Meters' ;
    AttTab50[ 96]._ma := 50  ; AttTab50[ 96]._mi := 628 ; AttTab50[ 96]._de := 'Decimal Fractions of 0.8 Feet or Meters' ;
    AttTab50[ 97]._ma := 50  ; AttTab50[ 97]._mi := 629 ; AttTab50[ 97]._de := 'Decimal Fractions of 0.9 Feet or Meters' ;
    AttTab50[ 98]._ma := 50  ; AttTab50[ 98]._mi := 630 ; AttTab50[ 98]._de := 'Boulders' ;
    AttTab50[ 99]._ma := 50  ; AttTab50[ 99]._mi := 631 ; AttTab50[ 99]._de := 'Sand' ;
    AttTab50[100]._ma := 50  ; AttTab50[100]._mi := 632 ; AttTab50[100]._de := 'Gravel' ;
    AttTab50[101]._ma := 50  ; AttTab50[101]._mi := 633 ; AttTab50[101]._de := 'Rock (Flat or Reef)' ;
    AttTab50[102]._ma := 50  ; AttTab50[102]._mi := 634 ; AttTab50[102]._de := 'Mud' ;
    AttTab50[103]._ma := 50  ; AttTab50[103]._mi := 635 ; AttTab50[103]._de := 'Shell' ;
    AttTab50[104]._ma := 50  ; AttTab50[104]._mi := 636 ; AttTab50[104]._de := 'Coral' ;
    AttTab50[105]._ma := 50  ; AttTab50[105]._mi := 637 ; AttTab50[105]._de := 'Tide' ;
    AttTab50[106]._ma := 50  ; AttTab50[106]._mi := 639 ; AttTab50[106]._de := 'Undredged' ;
  end ;

  {$IFNDEF OXYGENE}
    procedure T_ArraysDLG_class.fill_AttTab90 ;
  {$ELSE}
    class procedure T_ArraysDLG.fill_AttTab90 ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      AttTab90 := new T_AttCodes[31] ;
    {$ENDIF}
    AttTab90[ 0]._ma := 90 ; AttTab90[ 0]._mi :=   0 ; AttTab90[ 0]._de := 'Photorevised Feature' ;
    AttTab90[ 1]._ma := 90 ; AttTab90[ 1]._mi :=   1 ; AttTab90[ 1]._de := 'Boundary Monument' ;
    AttTab90[ 2]._ma := 90 ; AttTab90[ 2]._mi := 100 ; AttTab90[ 2]._de := 'Civil Township; District; Precinct; or Barrio' ;
    AttTab90[ 3]._ma := 90 ; AttTab90[ 3]._mi := 101 ; AttTab90[ 3]._de := 'Incorporated City;Village;Town;Borough;Hamlet' ;
    AttTab90[ 4]._ma := 90 ; AttTab90[ 4]._mi := 103 ; AttTab90[ 4]._de := 'National Park' ;
    AttTab90[ 5]._ma := 90 ; AttTab90[ 5]._mi := 104 ; AttTab90[ 5]._de := 'National Forest' ;
    AttTab90[ 6]._ma := 90 ; AttTab90[ 6]._mi := 105 ; AttTab90[ 6]._de := 'National Wildlife Area' ;
    AttTab90[ 7]._ma := 90 ; AttTab90[ 7]._mi := 106 ; AttTab90[ 7]._de := 'National Wilderness Area' ;
    AttTab90[ 8]._ma := 90 ; AttTab90[ 8]._mi := 107 ; AttTab90[ 8]._de := 'Indian Reservation' ;
    AttTab90[ 9]._ma := 90 ; AttTab90[ 9]._mi := 108 ; AttTab90[ 9]._de := 'Military Reservation' ;
    AttTab90[10]._ma := 90 ; AttTab90[10]._mi := 110 ; AttTab90[10]._de := 'Federal Prison' ;
    AttTab90[11]._ma := 90 ; AttTab90[11]._mi := 111 ; AttTab90[11]._de := 'Miscellaneous Federal Reservation' ;
    AttTab90[12]._ma := 90 ; AttTab90[12]._mi := 129 ; AttTab90[12]._de := 'Miscellaneous State Reservation' ;
    AttTab90[13]._ma := 90 ; AttTab90[13]._mi := 130 ; AttTab90[13]._de := 'State Park' ;
    AttTab90[14]._ma := 90 ; AttTab90[14]._mi := 131 ; AttTab90[14]._de := 'State Wildlife Area' ;
    AttTab90[15]._ma := 90 ; AttTab90[15]._mi := 132 ; AttTab90[15]._de := 'State Forest' ;
    AttTab90[16]._ma := 90 ; AttTab90[16]._mi := 133 ; AttTab90[16]._de := 'State Prison' ;
    AttTab90[17]._ma := 90 ; AttTab90[17]._mi := 134 ; AttTab90[17]._de := 'Miscellaneous County Reservation' ;
    AttTab90[18]._ma := 90 ; AttTab90[18]._mi := 135 ; AttTab90[18]._de := 'Ahupuaa (Hawaii)' ;
    AttTab90[19]._ma := 90 ; AttTab90[19]._mi := 136 ; AttTab90[19]._de := 'Hawaiian Homestead' ;
    AttTab90[20]._ma := 90 ; AttTab90[20]._mi := 150 ; AttTab90[20]._de := 'Large Park' ;
    AttTab90[21]._ma := 90 ; AttTab90[21]._mi := 151 ; AttTab90[21]._de := 'Small Park (City; County; or Private)' ;
    AttTab90[22]._ma := 90 ; AttTab90[22]._mi := 197 ; AttTab90[22]._de := 'Canada' ;
    AttTab90[23]._ma := 90 ; AttTab90[23]._mi := 198 ; AttTab90[23]._de := 'Mexico' ;
    AttTab90[24]._ma := 90 ; AttTab90[24]._mi := 199 ; AttTab90[24]._de := 'Open Water' ;
    AttTab90[25]._ma := 90 ; AttTab90[25]._mi := 201 ; AttTab90[25]._de := 'Indefinite or Approximate Boundary' ;
    AttTab90[26]._ma := 90 ; AttTab90[26]._mi := 202 ; AttTab90[26]._de := 'Disputed Boundary' ;
    AttTab90[27]._ma := 90 ; AttTab90[27]._mi := 203 ; AttTab90[27]._de := 'Historical Line' ;
    AttTab90[28]._ma := 90 ; AttTab90[28]._mi := 204 ; AttTab90[28]._de := 'Boundary Closure Line' ;
    AttTab90[29]._ma := 90 ; AttTab90[29]._mi := 299 ; AttTab90[29]._de := 'Processing Line' ;
    AttTab90[30]._ma := 90 ; AttTab90[30]._mi := 301 ; AttTab90[30]._de := 'Reference Monument for Boundary Point' ;
  end ;

  {$IFNDEF OXYGENE}
    procedure T_ArraysDLG_class.fill_AttTab170 ;
  {$ELSE}
    class procedure T_ArraysDLG.fill_AttTab170 ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      AttTab170 := new T_AttCodes[75] ;
    {$ENDIF}
    AttTab170[ 0]._ma := 170 ; AttTab170[ 0]._mi :=   0 ; AttTab170[ 0]._de := 'Photorevised Feature' ;
    AttTab170[ 1]._ma := 170 ; AttTab170[ 1]._mi :=   1 ; AttTab170[ 1]._de := 'Bridge Abutment' ;
    AttTab170[ 2]._ma := 170 ; AttTab170[ 2]._mi :=   2 ; AttTab170[ 2]._de := 'Tunnel Portal' ;
    AttTab170[ 3]._ma := 170 ; AttTab170[ 3]._mi :=   4 ; AttTab170[ 3]._de := 'Gate' ;
    AttTab170[ 4]._ma := 170 ; AttTab170[ 4]._mi :=   5 ; AttTab170[ 4]._de := 'Cul-De-Sac' ;
    AttTab170[ 5]._ma := 170 ; AttTab170[ 5]._mi :=   7 ; AttTab170[ 5]._de := 'Drawbridge' ;
    AttTab170[ 6]._ma := 170 ; AttTab170[ 6]._mi := 100 ; AttTab170[ 6]._de := 'Void Area' ;
    AttTab170[ 7]._ma := 170 ; AttTab170[ 7]._mi := 200 ; AttTab170[ 7]._de := 'Collect as Road.' ;
    AttTab170[ 8]._ma := 170 ; AttTab170[ 8]._mi := 201 ; AttTab170[ 8]._de := 'Primary Route; Class 1' ;
    AttTab170[ 9]._ma := 170 ; AttTab170[ 9]._mi := 202 ; AttTab170[ 9]._de := 'Primary Route; Class 1' ;
    AttTab170[10]._ma := 170 ; AttTab170[10]._mi := 203 ; AttTab170[10]._de := 'Primary Route; Class 1' ;
    AttTab170[11]._ma := 170 ; AttTab170[11]._mi := 204 ; AttTab170[11]._de := 'Primary Route; Class 1' ;
    AttTab170[12]._ma := 170 ; AttTab170[12]._mi := 205 ; AttTab170[12]._de := 'Secondary Route; Class 2' ;
    AttTab170[13]._ma := 170 ; AttTab170[13]._mi := 206 ; AttTab170[13]._de := 'Secondary Route; Class 2' ;
    AttTab170[14]._ma := 170 ; AttTab170[14]._mi := 207 ; AttTab170[14]._de := 'Secondary Route; Class 2' ;
    AttTab170[15]._ma := 170 ; AttTab170[15]._mi := 208 ; AttTab170[15]._de := 'Secondary Route; Class 2' ;
    AttTab170[16]._ma := 170 ; AttTab170[16]._mi := 209 ; AttTab170[16]._de := 'Road; Class 3' ;
    AttTab170[17]._ma := 170 ; AttTab170[17]._mi := 210 ; AttTab170[17]._de := 'Road; Class 4' ;
    AttTab170[18]._ma := 170 ; AttTab170[18]._mi := 211 ; AttTab170[18]._de := 'Trail' ;
    AttTab170[19]._ma := 170 ; AttTab170[19]._mi := 212 ; AttTab170[19]._de := 'Road; Class 5; Four-Wheel-Drive Vehicle' ;
    AttTab170[20]._ma := 170 ; AttTab170[20]._mi := 213 ; AttTab170[20]._de := 'Footbridge' ;
    AttTab170[21]._ma := 170 ; AttTab170[21]._mi := 214 ; AttTab170[21]._de := 'Road Ferry Crossing' ;
    AttTab170[22]._ma := 170 ; AttTab170[22]._mi := 217 ; AttTab170[22]._de := 'Road or Street; Class 3' ;
    AttTab170[23]._ma := 170 ; AttTab170[23]._mi := 218 ; AttTab170[23]._de := 'Road; Class 3' ;
    AttTab170[24]._ma := 170 ; AttTab170[24]._mi := 219 ; AttTab170[24]._de := 'Road; Class 4; One Way' ;
    AttTab170[25]._ma := 170 ; AttTab170[25]._mi := 221 ; AttTab170[25]._de := 'Road; Class 3; One Way' ;
    AttTab170[26]._ma := 170 ; AttTab170[26]._mi := 222 ; AttTab170[26]._de := 'Road in Transition' ;
    AttTab170[27]._ma := 170 ; AttTab170[27]._mi := 223 ; AttTab170[27]._de := 'Road in Service Facility; Rest Area; Viewpoint' ;
    AttTab170[28]._ma := 170 ; AttTab170[28]._mi := 299 ; AttTab170[28]._de := 'Processing Line' ;
    AttTab170[29]._ma := 170 ; AttTab170[29]._mi := 401 ; AttTab170[29]._de := 'Traffic Circle' ;
    AttTab170[30]._ma := 170 ; AttTab170[30]._mi := 402 ; AttTab170[30]._de := 'Ramp in Interchange' ;
    AttTab170[31]._ma := 170 ; AttTab170[31]._mi := 403 ; AttTab170[31]._de := 'Tollgate' ;
    AttTab170[32]._ma := 170 ; AttTab170[32]._mi := 404 ; AttTab170[32]._de := 'Weigh Station' ;
    AttTab170[33]._ma := 170 ; AttTab170[33]._mi := 405 ; AttTab170[33]._de := 'Nonstandard Section of Road' ;
    AttTab170[34]._ma := 170 ; AttTab170[34]._mi := 601 ; AttTab170[34]._de := 'In Tunnel' ;
    AttTab170[35]._ma := 170 ; AttTab170[35]._mi := 602 ; AttTab170[35]._de := 'Overpassing; On Bridge' ;
    AttTab170[36]._ma := 170 ; AttTab170[36]._mi := 603 ; AttTab170[36]._de := 'Under Construction' ;
    AttTab170[37]._ma := 170 ; AttTab170[37]._mi := 605 ; AttTab170[37]._de := 'Labeled "Old Railroad Grade"' ;
    AttTab170[38]._ma := 170 ; AttTab170[38]._mi := 606 ; AttTab170[38]._de := 'Submerged or in Ford' ;
    AttTab170[39]._ma := 170 ; AttTab170[39]._mi := 607 ; AttTab170[39]._de := 'Underpassing' ;
    AttTab170[40]._ma := 170 ; AttTab170[40]._mi := 609 ; AttTab170[40]._de := 'Toll' ;
    AttTab170[41]._ma := 170 ; AttTab170[41]._mi := 610 ; AttTab170[41]._de := 'Privately Operated or Restricted Use' ;
    AttTab170[42]._ma := 170 ; AttTab170[42]._mi := 612 ; AttTab170[42]._de := 'Double-Decked' ;
    AttTab170[43]._ma := 170 ; AttTab170[43]._mi := 614 ; AttTab170[43]._de := 'Elevated' ;
    AttTab170[44]._ma := 170 ; AttTab170[44]._mi := 615 ; AttTab170[44]._de := 'Bypass' ;
    AttTab170[45]._ma := 170 ; AttTab170[45]._mi := 616 ; AttTab170[45]._de := 'Alternate' ;
    AttTab170[46]._ma := 170 ; AttTab170[46]._mi := 617 ; AttTab170[46]._de := 'Business' ;
    AttTab170[47]._ma := 170 ; AttTab170[47]._mi := 618 ; AttTab170[47]._de := 'On Drawbridge' ;
    AttTab170[48]._ma := 170 ; AttTab170[48]._mi := 619 ; AttTab170[48]._de := 'Spur' ;
    AttTab170[49]._ma := 170 ; AttTab170[49]._mi := 620 ; AttTab170[49]._de := 'Loop' ;
    AttTab170[50]._ma := 170 ; AttTab170[50]._mi := 621 ; AttTab170[50]._de := 'Connector' ;
    AttTab170[51]._ma := 170 ; AttTab170[51]._mi := 622 ; AttTab170[51]._de := 'Truck Route' ;
    AttTab170[52]._ma := 170 ; AttTab170[52]._mi := 624 ; AttTab170[52]._de := 'Covered Bridge' ;
    AttTab170[53]._ma := 170 ; AttTab170[53]._mi := 630 ; AttTab170[53]._de := 'Rural' ;
    AttTab170[54]._ma := 170 ; AttTab170[54]._mi := 631 ; AttTab170[54]._de := 'Rural Principal Arterial' ;
    AttTab170[55]._ma := 170 ; AttTab170[55]._mi := 632 ; AttTab170[55]._de := 'Rural Minor Arterial' ;
    AttTab170[56]._ma := 170 ; AttTab170[56]._mi := 634 ; AttTab170[56]._de := 'Rural Minor Collector' ;
    AttTab170[57]._ma := 170 ; AttTab170[57]._mi := 635 ; AttTab170[57]._de := 'Rural Local' ;
    AttTab170[58]._ma := 170 ; AttTab170[58]._mi := 636 ; AttTab170[58]._de := 'Urban Interstate' ;
    AttTab170[59]._ma := 170 ; AttTab170[59]._mi := 637 ; AttTab170[59]._de := 'Urban Freeway or Expressway' ;
    AttTab170[60]._ma := 170 ; AttTab170[60]._mi := 638 ; AttTab170[60]._de := 'Urban Principal Arterial' ;
    AttTab170[61]._ma := 170 ; AttTab170[61]._mi := 639 ; AttTab170[61]._de := 'Urban Minor Arterial' ;
    AttTab170[62]._ma := 170 ; AttTab170[62]._mi := 640 ; AttTab170[62]._de := 'Urban Collector' ;
    AttTab170[63]._ma := 170 ; AttTab170[63]._mi := 641 ; AttTab170[63]._de := 'Urban Local' ;
    AttTab170[64]._ma := 170 ; AttTab170[64]._mi := 642 ; AttTab170[64]._de := 'Not Classified' ;
    AttTab170[65]._ma := 170 ; AttTab170[65]._mi := 650 ; AttTab170[65]._de := 'Road Width 0.025 Inch' ;
    AttTab170[66]._ma := 170 ; AttTab170[66]._mi := 651 ; AttTab170[66]._de := 'Road Width 0.030 Inch' ;
    AttTab170[67]._ma := 170 ; AttTab170[67]._mi := 652 ; AttTab170[67]._de := 'Road Width 0.035 Inch' ;
    AttTab170[68]._ma := 170 ; AttTab170[68]._mi := 653 ; AttTab170[68]._de := 'Road Width 0.040 Inch' ;
    AttTab170[69]._ma := 170 ; AttTab170[69]._mi := 654 ; AttTab170[69]._de := 'Road Width 0.045 Inch' ;
    AttTab170[70]._ma := 170 ; AttTab170[70]._mi := 655 ; AttTab170[70]._de := 'Road Width 0.050 Inch' ;
    AttTab170[71]._ma := 170 ; AttTab170[71]._mi := 656 ; AttTab170[71]._de := 'Road Width 0.055 Inch' ;
    AttTab170[72]._ma := 170 ; AttTab170[72]._mi := 657 ; AttTab170[72]._de := 'Road Width 0.060 Inch' ;
    AttTab170[73]._ma := 170 ; AttTab170[73]._mi := 658 ; AttTab170[73]._de := 'Road Width 0.065 Inch' ;
    AttTab170[74]._ma := 170 ; AttTab170[74]._mi := 659 ; AttTab170[74]._de := 'Road Width 0.070 Inch' ;
  end ;

  {$IFNDEF OXYGENE}
    procedure T_ArraysDLG_class.fill_AttTab180 ;
  {$ELSE}
    class procedure T_ArraysDLG.fill_AttTab180 ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      AttTab180 := new T_AttCodes[25] ;
    {$ENDIF}
    AttTab180[ 0]._ma := 180 ; AttTab180[ 0]._mi :=   0 ; AttTab180[ 0]._de := 'Photorevised Feature' ;
    AttTab180[ 1]._ma := 180 ; AttTab180[ 1]._mi :=   1 ; AttTab180[ 1]._de := 'Bridge Abutment' ;
    AttTab180[ 2]._ma := 180 ; AttTab180[ 2]._mi :=   2 ; AttTab180[ 2]._de := 'Tunnel Portal' ;
    AttTab180[ 3]._ma := 180 ; AttTab180[ 3]._mi :=   7 ; AttTab180[ 3]._de := 'Drawbridge' ;
    AttTab180[ 4]._ma := 180 ; AttTab180[ 4]._mi := 100 ; AttTab180[ 4]._de := 'Void Area' ;
    AttTab180[ 5]._ma := 180 ; AttTab180[ 5]._mi := 201 ; AttTab180[ 5]._de := 'Railroad' ;
    AttTab180[ 6]._ma := 180 ; AttTab180[ 6]._mi := 202 ; AttTab180[ 6]._de := 'Railroad in Road' ;
    AttTab180[ 7]._ma := 180 ; AttTab180[ 7]._mi := 204 ; AttTab180[ 7]._de := 'Carline' ;
    AttTab180[ 8]._ma := 180 ; AttTab180[ 8]._mi := 205 ; AttTab180[ 8]._de := 'Cog Railroad; Incline Railway; or Logging Tram' ;
    AttTab180[ 9]._ma := 180 ; AttTab180[ 9]._mi := 207 ; AttTab180[ 9]._de := 'Railroad Ferry Crossing' ;
    AttTab180[10]._ma := 180 ; AttTab180[10]._mi := 208 ; AttTab180[10]._de := 'Railroad Siding' ;
    AttTab180[11]._ma := 180 ; AttTab180[11]._mi := 209 ; AttTab180[11]._de := 'Railroad Yard' ;
    AttTab180[12]._ma := 180 ; AttTab180[12]._mi := 299 ; AttTab180[12]._de := 'Processing Line' ;
    AttTab180[13]._ma := 180 ; AttTab180[13]._mi := 400 ; AttTab180[13]._de := 'Railroad Station' ;
    AttTab180[14]._ma := 180 ; AttTab180[14]._mi := 401 ; AttTab180[14]._de := 'Turntable' ;
    AttTab180[15]._ma := 180 ; AttTab180[15]._mi := 402 ; AttTab180[15]._de := 'Roundhouse' ;
    AttTab180[16]._ma := 180 ; AttTab180[16]._mi := 601 ; AttTab180[16]._de := 'In Tunnel' ;
    AttTab180[17]._ma := 180 ; AttTab180[17]._mi := 602 ; AttTab180[17]._de := 'Overpassing; On Bridge' ;
    AttTab180[18]._ma := 180 ; AttTab180[18]._mi := 605 ; AttTab180[18]._de := 'Underpassing' ;
    AttTab180[19]._ma := 180 ; AttTab180[19]._mi := 606 ; AttTab180[19]._de := 'Narrow Gauge' ;
    AttTab180[20]._ma := 180 ; AttTab180[20]._mi := 609 ; AttTab180[20]._de := 'Elevated' ;
    AttTab180[21]._ma := 180 ; AttTab180[21]._mi := 610 ; AttTab180[21]._de := 'Rapid Transit' ;
    AttTab180[22]._ma := 180 ; AttTab180[22]._mi := 611 ; AttTab180[22]._de := 'On Drawbridge' ;
    AttTab180[23]._ma := 180 ; AttTab180[23]._mi := 612 ; AttTab180[23]._de := 'Private' ;
    AttTab180[24]._ma := 180 ; AttTab180[24]._mi := 613 ; AttTab180[24]._de := 'U.S. Government' ;
  end ;

  {$IFNDEF OXYGENE}
    procedure T_ArraysDLG_class.fill_AttTab190 ;
  {$ELSE}
    class procedure T_ArraysDLG.fill_AttTab190 ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      AttTab190 := new T_AttCodes[23] ;
    {$ENDIF}
    AttTab190[ 0]._ma := 190 ; AttTab190[ 0]._mi := 0   ; AttTab190[ 0]._de := 'Photorevised Feature' ;
    AttTab190[ 1]._ma := 190 ; AttTab190[ 1]._mi := 100 ; AttTab190[ 1]._de := 'Void Area' ;
    AttTab190[ 2]._ma := 190 ; AttTab190[ 2]._mi := 201 ; AttTab190[ 2]._de := 'Pipeline' ;
    AttTab190[ 3]._ma := 190 ; AttTab190[ 3]._mi := 202 ; AttTab190[ 3]._de := 'Power Transmission Line' ;
    AttTab190[ 4]._ma := 190 ; AttTab190[ 4]._mi := 203 ; AttTab190[ 4]._de := 'Telephone Line' ;
    AttTab190[ 5]._ma := 190 ; AttTab190[ 5]._mi := 208 ; AttTab190[ 5]._de := 'Monorail' ;
    AttTab190[ 6]._ma := 190 ; AttTab190[ 6]._mi := 209 ; AttTab190[ 6]._de := 'Ski Lift' ;
    AttTab190[ 7]._ma := 190 ; AttTab190[ 7]._mi := 299 ; AttTab190[ 7]._de := 'Processing Line' ;
    AttTab190[ 8]._ma := 190 ; AttTab190[ 8]._mi := 400 ; AttTab190[ 8]._de := 'Power Station or Power Plant' ;
    AttTab190[ 9]._ma := 190 ; AttTab190[ 9]._mi := 401 ; AttTab190[ 9]._de := 'Substation' ;
    AttTab190[10]._ma := 190 ; AttTab190[10]._mi := 402 ; AttTab190[10]._de := 'Hydroelectric Plant' ;
    AttTab190[11]._ma := 190 ; AttTab190[11]._mi := 403 ; AttTab190[11]._de := 'Landing Strip; Runway; Apron,Taxiway' ;
    AttTab190[12]._ma := 190 ; AttTab190[12]._mi := 404 ; AttTab190[12]._de := 'Helipad' ;
    AttTab190[13]._ma := 190 ; AttTab190[13]._mi := 405 ; AttTab190[13]._de := 'Launch Complex' ;
    AttTab190[14]._ma := 190 ; AttTab190[14]._mi := 406 ; AttTab190[14]._de := 'Pumping Station or Compressor Station' ;
    AttTab190[15]._ma := 190 ; AttTab190[15]._mi := 409 ; AttTab190[15]._de := 'Seaplane Ramp' ;
    AttTab190[16]._ma := 190 ; AttTab190[16]._mi := 410 ; AttTab190[16]._de := 'Seaplane Landing Area' ;
    AttTab190[17]._ma := 190 ; AttTab190[17]._mi := 601 ; AttTab190[17]._de := 'Under Construction' ;
    AttTab190[18]._ma := 190 ; AttTab190[18]._mi := 602 ; AttTab190[18]._de := 'Abandoned' ;
    AttTab190[19]._ma := 190 ; AttTab190[19]._mi := 603 ; AttTab190[19]._de := 'Aboveground' ;
    AttTab190[20]._ma := 190 ; AttTab190[20]._mi := 605 ; AttTab190[20]._de := 'Unpaved' ;
    AttTab190[21]._ma := 190 ; AttTab190[21]._mi := 606 ; AttTab190[21]._de := 'Submerged' ;
    AttTab190[22]._ma := 190 ; AttTab190[22]._mi := 607 ; AttTab190[22]._de := 'Nuclear ' ;
  end ;

  {$IFNDEF OXYGENE}
    procedure T_ArraysDLG_class.fill_AttTab300 ;
  {$ELSE}
    class procedure T_ArraysDLG.fill_AttTab300 ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      AttTab300 := new T_AttCodes[40] ;
    {$ENDIF}
    AttTab300[ 0]._ma := 300 ; AttTab300[ 0]._mi :=   1 ; AttTab300[ 0]._de := 'Found PLSS Section Corner' ;
    AttTab300[ 1]._ma := 300 ; AttTab300[ 1]._mi :=   4 ; AttTab300[ 1]._de := 'Meander Corner' ;
    AttTab300[ 2]._ma := 300 ; AttTab300[ 2]._mi :=   7 ; AttTab300[ 2]._de := 'Witness Corner' ;
    AttTab300[ 3]._ma := 300 ; AttTab300[ 3]._mi :=   8 ; AttTab300[ 3]._de := 'Witness Point' ;
    AttTab300[ 4]._ma := 300 ; AttTab300[ 4]._mi :=   9 ; AttTab300[ 4]._de := 'Angle Point' ;
    AttTab300[ 5]._ma := 300 ; AttTab300[ 5]._mi :=  10 ; AttTab300[ 5]._de := 'Amended Monument' ;
    AttTab300[ 6]._ma := 300 ; AttTab300[ 6]._mi :=  12 ; AttTab300[ 6]._de := 'Found Quarter-Section Corner' ;
    AttTab300[ 7]._ma := 300 ; AttTab300[ 7]._mi :=  14 ; AttTab300[ 7]._de := 'Land Grant or Other Special Survey Corner' ;
    AttTab300[ 8]._ma := 300 ; AttTab300[ 8]._mi := 101 ; AttTab300[ 8]._de := 'Homestead Entry Survey' ;
    AttTab300[ 9]._ma := 300 ; AttTab300[ 9]._mi := 102 ; AttTab300[ 9]._de := 'Donation Land Claims' ;
    AttTab300[10]._ma := 300 ; AttTab300[10]._mi := 103 ; AttTab300[10]._de := 'Land Grant' ;
    AttTab300[11]._ma := 300 ; AttTab300[11]._mi := 104 ; AttTab300[11]._de := 'Private Extension of Public Land Survey' ;
    AttTab300[12]._ma := 300 ; AttTab300[12]._mi := 105 ; AttTab300[12]._de := 'Area of Public and Private Survey Overlap' ;
    AttTab300[13]._ma := 300 ; AttTab300[13]._mi := 106 ; AttTab300[13]._de := 'Overlapping Land Grants' ;
    AttTab300[14]._ma := 300 ; AttTab300[14]._mi := 108 ; AttTab300[14]._de := 'Private Survey in Ohio' ;
    AttTab300[15]._ma := 300 ; AttTab300[15]._mi := 110 ; AttTab300[15]._de := 'PLSS Area' ;
    AttTab300[16]._ma := 300 ; AttTab300[16]._mi := 111 ; AttTab300[16]._de := 'Tract' ;
    AttTab300[17]._ma := 300 ; AttTab300[17]._mi := 112 ; AttTab300[17]._de := 'U.S. Survey' ;
    AttTab300[18]._ma := 300 ; AttTab300[18]._mi := 113 ; AttTab300[18]._de := 'Indian Allotment' ;
    AttTab300[19]._ma := 300 ; AttTab300[19]._mi := 114 ; AttTab300[19]._de := 'Area Outside of the Public Domain' ;
    AttTab300[20]._ma := 300 ; AttTab300[20]._mi := 198 ; AttTab300[20]._de := 'Water' ;
    AttTab300[21]._ma := 300 ; AttTab300[21]._mi := 201 ; AttTab300[21]._de := 'Approximate Position' ;
    AttTab300[22]._ma := 300 ; AttTab300[22]._mi := 202 ; AttTab300[22]._de := 'Protracted Position' ;
    AttTab300[23]._ma := 300 ; AttTab300[23]._mi := 203 ; AttTab300[23]._de := 'Closure Line' ;
    AttTab300[24]._ma := 300 ; AttTab300[24]._mi := 299 ; AttTab300[24]._de := 'Processing Line' ;
    AttTab300[25]._ma := 300 ; AttTab300[25]._mi := 300 ; AttTab300[25]._de := 'Location or Mineral Monument' ;
    AttTab300[26]._ma := 300 ; AttTab300[26]._mi := 301 ; AttTab300[26]._de := 'Isolated Found Section Corner' ;
    AttTab300[27]._ma := 300 ; AttTab300[27]._mi := 600 ; AttTab300[27]._de := 'Connecticut Western Reserve' ;
    AttTab300[28]._ma := 300 ; AttTab300[28]._mi := 601 ; AttTab300[28]._de := 'Virginia Military District' ;
    AttTab300[29]._ma := 300 ; AttTab300[29]._mi := 602 ; AttTab300[29]._de := 'Ohio Company Purchase' ;
    AttTab300[30]._ma := 300 ; AttTab300[30]._mi := 603 ; AttTab300[30]._de := 'Symmes Purchase' ;
    AttTab300[31]._ma := 300 ; AttTab300[31]._mi := 604 ; AttTab300[31]._de := 'French Grants' ;
    AttTab300[32]._ma := 300 ; AttTab300[32]._mi := 605 ; AttTab300[32]._de := 'Donation Tract' ;
    AttTab300[33]._ma := 300 ; AttTab300[33]._mi := 606 ; AttTab300[33]._de := 'Old Seven Ranges' ;
    AttTab300[34]._ma := 300 ; AttTab300[34]._mi := 607 ; AttTab300[34]._de := 'Congress Lands North of Old Seven Ranges' ;
    AttTab300[35]._ma := 300 ; AttTab300[35]._mi := 608 ; AttTab300[35]._de := 'Congress Lands East of Scioto River' ;
    AttTab300[36]._ma := 300 ; AttTab300[36]._mi := 609 ; AttTab300[36]._de := 'Between the Miamis; North of Symmes Purchase' ;
    AttTab300[37]._ma := 300 ; AttTab300[37]._mi := 610 ; AttTab300[37]._de := 'West of the Great Miami' ;
    AttTab300[38]._ma := 300 ; AttTab300[38]._mi := 612 ; AttTab300[38]._de := 'Refugee Lands' ;
    AttTab300[39]._ma := 300 ; AttTab300[39]._mi := 625 ; AttTab300[39]._de := 'Fraction One-Half for Land Grant Corner' ;
  end ;

//=============================================================================
// TGIS_LayerDLG
//=============================================================================

  constructor TGIS_LayerDLG.Create ;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                           ]  ;
  end ;

  procedure TGIS_LayerDLG.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_LayerDLG.readRec( out _str : String ) : Boolean;
  var
    abort : Boolean ;
    ar_c  : TBytes ;
    j      : Integer ;
  begin

    if bDlgOpt then begin   // optional extension
      SetLength( ar_c, 150 ) ;
      j := 0 ;
      if FdlgFile.Position < FdlgFile.Size then
         {$IFNDEF OXYGENE}
           FdlgFile.ReadBuffer( ar_c, 80 ) ;
         {$ELSE}
           FdlgFile.Read( ar_c, 0, 80 ) ;
         {$ENDIF}

      _str := ConvertAnsiString( ar_c, 79 ) ;
    end
    else begin             // standard extension
      SetLengthStr( _str, 149 ) ;
      _str := FdlgFile.ReadLine ;
      while ( IsStringEmpty( _str ) ) and ( FdlgFile.Position < FdlgFile.Size ) do
        _str := FdlgFile.ReadLine ;
    end;

    inc( FdlgRecNo ) ;

    if FdlgRecNo mod GIS_PROGRESS_TRESHOLD = 1 then begin
      abort := RaiseBusyShake( Self, FdlgFile.Position , FdlgFile.Size ) ;
    end ;
    Result := FdlgFile.Position < FdlgFile.Size ;
  end ;

  procedure TGIS_LayerDLG.readCategory;
  var
     line   : String;
     rs     : Integer ;
     utmz   : Integer ;
     dtm    : Integer ;
     prj    : Integer ;
     pparam : TGIS_CSProjParameters ;
     gcs    : TGIS_CSAbstract ;
     tkn    : TGIS_Tokenizer ;
     dparam : TGIS_DoubleArray ;
     k      : Integer ;

     procedure copy_params ;
     var
       i : Integer ;
     begin
       line := StringReplaceAll( line, 'D', 'E' ) ;
        tkn.ExecuteEx( line, ' ' ) ;
        for i := 0 to tkn.Result.Count-1 do begin
          try
            dparam[k] := DotStrToFloat( tkn.Result[i] ) ;
          except
            dparam[k] := 0 ;
          end;
          inc( k ) ;
        end ;
     end ;

    function defdeg(
      const _default : Double ;
      const _value1  : Double
    ) : Double ;
    begin
      if _value1 <> 0 then
        Result := DegToRad( _value1 )
      else
        Result := DegToRad( _default ) ;
    end;

  begin
    try
      readRec( line ) ; // record 1 - banner
      readRec( line ) ; // record 2 - cell name, date, qualifier, scale, sectional indicator
      readRec( line ) ; // record 3 - contour interval info, status flags
      readRec( line ) ; // record 4 - codes, resolution, transformation info
      if not bDlgOpt then
        readRec( line ) ; // record 5 - projection

      rs   := StrToInt( Trim(Copy ( line, StringFirst +  6, 6 )) ) ;
      utmz := StrToInt( Trim(Copy ( line, StringFirst + 12, 6 )) ) ;
      dtm  := StrToInt( Trim(Copy ( line, StringFirst + 65, 3 )) ) ;

      tkn := TGIS_Tokenizer.Create ;
      try
        // record 6-9 - Projection parameters for map transformation
        k := 0 ;
        SetLength( dparam, 15 ) ;
        readRec( line ) ;
        copy_params  ;
        readRec( line ) ;
        copy_params ;
        readRec( line ) ;
        copy_params ;
        readRec( line ) ;
        copy_params ;
      finally
        FreeObject( tkn ) ;
      end ;
      prj := 0 ;
      // 1 = UTM, 3 = Albers
      if rs = 1 then begin
        prj := CSPROJ_Transverse_Mercator ;
        pparam := CSProjectedCoordinateSystemList.DefaultParamsForUTM( utmz ) ;
      end
      else if rs = 3 then begin
        prj := CSPROJ_Albers ;
        pparam := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

        pparam.StandardParallel_1 := defdeg( 29.5, dparam[2] ) ;
        pparam.StandardParallel_2 := defdeg( 45.4, dparam[3] ) ;
        pparam.CentralMeridian    := defdeg( 0,    dparam[4] ) ;
        pparam.LatitudeOfOrigin   := defdeg( 0,    dparam[5] ) ;
        pparam.FalseEasting       := dparam[6] ;
        pparam.FalseNorthing      := dparam[7] ;
      end ;

      gcs := nil ;
      if dtm = 0 then
        gcs := CSGeographicCoordinateSystemList.ByEPSG( 4267 )  // NAD27
      else
        gcs := CSGeographicCoordinateSystemList.ByEPSG( 4269 ) ;// NAD83

      if not assigned( gcs ) then
        gcs := CSGeographicCoordinateSystemList.ByEPSG(
                 GIS_EPSG_WGS84
               ) ;
      FCS := CSProjectedCoordinateSystemList.Prepare(
              -1, '', gcs.EPSG, 9001, prj, pparam
             ) ;

      readRec( line ) ; // record 10-11 - Internal file-to-map projection transformation parameters
      readRec( line ) ;

      readRec( line ) ; // record 12-15 - extent SW NW NE SE
      readRec( line ) ;
      readRec( line ) ;
      readRec( line ) ;

      if not readRec( line ) then exit ;
      sCategory  := Trim(     Copy ( line, StringFirst     , 20 ) ) ;
      FNodeCount := StrToInt( Copy ( line, StringFirst + 24,  6 ) ) ;
      FAreaCount := StrToInt( Copy ( line, StringFirst + 40,  6 ) ) ;
      FLineCount := StrToInt( Copy ( line, StringFirst + 56,  6 ) ) ;
    except
      FNodeCount := 0 ;
      FAreaCount := 0 ;
      FLineCount := 0 ;
    end ;
  end;

  procedure TGIS_LayerDLG.buildLine( const _t : String; const _param : String ) ;
  var
    i,j      : Integer ;
    tkn      : TGIS_Tokenizer ;
    line     : String ;
    part_no  : Integer ;
  begin
    try
      oShp := CreateShape( TGIS_ShapeType.Arc ) ;
      oShp.Lock( TGIS_Lock.Extent ) ;
      oShp.AddPart;
      try
          part_no := 2 * StrToInt( _param ) ;
          tkn := TGIS_Tokenizer.Create ;
          try
             j := 0;
             while ( j < part_no ) do begin
              if not readRec( line ) then exit ;

              tkn.ExecuteEx( line, ' ' ) ;
              i := 0;
              while i < tkn.Result.Count do begin
                 oShp.AddPoint( GisPoint( DotStrToFloat( tkn.Result[i  ] ),
                                          DotStrToFloat( tkn.Result[i+1] )
                                        )
                              ) ;
                 inc( i, 2 ) ;
              end;
              inc( j, tkn.Result.Count ) ;
             end;
             oShp.SetField( DLG_FEA_TYPE, _t );
          finally
            FreeObject( tkn ) ;
          end ;
      finally
        oShp.Unlock ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, FdlgRecNo ) ;
    end ;
  end ;

  procedure TGIS_LayerDLG.addAttributes( const _tkn : TGIS_Tokenizer ) ;
  var
    j  : Integer ;
    ma : Integer;
    mi : Integer ;

    function GetDesc( const _tab : array of T_AttCodes;
                      const _ma  : Integer ;
                      const _mi  : Integer
                     ) : String;
    var i : Integer ;
    begin
       Result := '' ;

       for i := low( _tab ) to high( _tab ) do begin
          if ( _tab[i]._ma = _ma ) and
             (( _tab[i]._mi = _mi ) or ( _tab[i]._mi = -1)) then
          begin
              Result := _tab[i]._de;
              break;
          end;
       end;
    end;

  begin
    if _tkn.Result.Count > 0 then begin
      ma := Abs( StrToInt( _tkn.Result[ 0 ] ) ) ;
      if _tkn.Result.Count > 1 then
        mi := Abs( StrToInt( _tkn.Result[ 1 ] ) )
      else
        mi := -1;

      case ma of
        50 : oShp.SetField( DLG_FEA_DESC, GetDesc( T_ArraysDLG.AttTab50 , ma, mi ) );
        90 : oShp.SetField( DLG_FEA_DESC, GetDesc( T_ArraysDLG.AttTab90 , ma, mi ) );
       170 : oShp.SetField( DLG_FEA_DESC, GetDesc( T_ArraysDLG.AttTab170, ma, mi ) );
       180 : oShp.SetField( DLG_FEA_DESC, GetDesc( T_ArraysDLG.AttTab180, ma, mi ) );
       190 : oShp.SetField( DLG_FEA_DESC, GetDesc( T_ArraysDLG.AttTab190, ma, mi ) );
       300 : oShp.SetField( DLG_FEA_DESC, GetDesc( T_ArraysDLG.AttTab300, ma, mi ) );
      else
             oShp.SetField( DLG_FEA_DESC, GetDesc( T_ArraysDLG.AttTabXX , ma, mi ) );
      end ;
    end ;

    j := 0;
    while j < _tkn.Result.Count-1 do begin
      if not IsStringEmpty( Trim( _tkn.Result[j] ) ) then
        oShp.SetField( MAJOR_FIELD1, Abs( StrToInt( _tkn.Result[j  ] ) ) ) ;
      if ( j + 1 ) < _tkn.Result.Count then
        oShp.SetField( MINOR_FIELD1, Abs( StrToInt( _tkn.Result[j + 1] ) ) ) ;
      if ( j + 2 ) < _tkn.Result.Count then
        oShp.SetField( MINOR_FIELD2, Abs( StrToInt( _tkn.Result[j + 2] ) ) ) ;
      if ( j + 3 ) < _tkn.Result.Count then
        oShp.SetField( MINOR_FIELD2, Abs( StrToInt( _tkn.Result[j + 3] ) ) ) ;
      if ( j + 4 ) < _tkn.Result.Count then
        oShp.SetField( MINOR_FIELD2, Abs( StrToInt( _tkn.Result[j + 4] ) ) ) ;
      if ( j + 5 ) < _tkn.Result.Count then
        oShp.SetField( MINOR_FIELD2, Abs( StrToInt( _tkn.Result[j + 5] ) ) ) ;

      inc( j, 5 ) ;
    end;
  end;

  procedure TGIS_LayerDLG.buildPoint( const _t : String ;
                                      const _x : String ;
                                      const _y : String
                                    ) ;
  begin
    try
      oShp := CreateShape( TGIS_ShapeType.Point ) ;
      oShp.Lock( TGIS_Lock.Extent ) ;
      try
        oShp.AddPart ;
        oShp.AddPoint( GisPoint( DotStrToFloat( _x ) , DotStrToFloat( _y ) ) ) ;
        oShp.SetField( DLG_FEA_TYPE, _t );
      finally
        oShp.Unlock ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, FdlgRecNo ) ;
    end ;

    if Items.Count = 1 then Extent := oShp.Extent ;
  end ;

  procedure TGIS_LayerDLG.writeHeader( const _file  : TGIS_Stream      ;
                                       const _layer : TGIS_LayerVector ;
                                       const _nodes : Integer          ;
                                       const _areas : Integer          ;
                                       const _lines : Integer
                                     ) ;
  var i : Integer ;

    procedure my_write( const _format : String ) ;
    begin
      _file.WriteString( Format( '%-80s', [_format]) ) ;
    end ;

  begin
    my_write(
      '*** DLG-OPTIONAL FORMAT PRODUCED BY TATUKGIS' ) ;
    my_write(
      '                                         0           Unknown    0') ;
    my_write(
      '                                            01,  01  202,  02    bbbbbbbb');
    my_write(
      '     3     1    16     2     0.00000000000     4     0     4     1  1  0') ;
    for i := 0 to 4 do
     my_write(
      '     0.000000000000000       0.000000000000000       0.000000000000000') ;
    my_write(
      '   1.00000000000     0.00000000000     0.00000000000     0.00000000000') ;
    my_write(
      'SW       000000000  0000000000         000000000  0000000000') ;
    my_write(
      'NW       000000000  0000000000         000000000  0000000000') ;
    my_write(
      'NE       000000000  0000000000         000000000  0000000000') ;
    my_write(
      'SE       000000000  0000000000         000000000  0000000000') ;
    my_write( Format( '%-24s%6d%6d 01 %6d%6d 010%6d%6d 000%6d%2d',
              [ 'VECTOR_DATA',
                _nodes, _nodes,
                _areas, _areas,
                _lines, _lines,
                0,
                0
              ]
             )
            ) ;
  end ;

{$IFDEF OXYGENE}
  type
    T_store_DLG = record
      DecimalSeparator  : String ;
      ThousandSeparator : String ;
    end ;
{$ENDIF}

  procedure TGIS_LayerDLG.writeGeometry( const _file : TGIS_Stream;
                                         const _shp  : TGIS_Shape ;
                                         var   _node : Integer    ;
                                         var   _area : Integer    ;
                                         var   _line : Integer
                                       ) ;
  var
    part_no,j : Integer ;
    point_no  : Integer ;
    part_size : Integer ;
    num_parts : Integer ;
    ptg       : TGIS_Point ;
    stab      : String ;
    ftype     : String ;
    cnt       : Integer ;
    {$IFDEF OXYGENE}
      store   : T_store_DLG ;
    {$ELSE}
      store   : record
        {$IFDEF CLR}
          DecimalSeparator  : String ;
          ThousandSeparator : String ;
        {$ELSE}
          DecimalSeparator : Char   ;
          ThousandSeparator : Char   ;
        {$ENDIF}
      end ;
    {$ENDIF}

    procedure my_write( const _format : String ; const _args: array of {$IFNDEF OXYGENE}
                                                                         const
                                                                       {$ELSE}
                                                                         Object
                                                                       {$ENDIF}
                                                                       ) ;
    begin
      _file.WriteString( Format( '%-80s', [Format( _format, _args )]) ) ;
    end ;

  begin
    store.DecimalSeparator  := {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator  ;
    store.ThousandSeparator := {$IFDEF DCC}FormatSettings.{$ENDIF}ThousandSeparator ;
    try
      {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator := '.' ;
      assert( assigned( _shp ) ) ;

      num_parts := _shp.GetNumParts ;
      assert( num_parts > 0 ) ;

      _shp.Lock( TGIS_Lock.Extent ) ;
      try
        ftype := VarToString( _shp.GetField( DLG_FEA_TYPE ) ) ;
        if IsStringEmpty( ftype ) then
          case _shp.ShapeType of
            TGIS_ShapeType.Point       : ftype := DLG_NODE ;
            TGIS_ShapeType.MultiPoint  : ftype := DLG_NODE ;
            TGIS_ShapeType.Arc         : ftype := DLG_LINE ;
            TGIS_ShapeType.Polygon     : ftype := DLG_LINE ;
          end;

        if      ftype = DLG_AREA then cnt := _area
        else if ftype = DLG_NODE then cnt := _node
        else                          cnt := _line ;

        if      _shp is TGIS_ShapePoint  then
            begin
              ptg := _shp.GetPoint( 0, 0 ) ;
              my_write( '%s%5d%12f%12f%6d%6d%6d%6d%6d%6d',
                        [ ftype, cnt, ptg.X, ptg.Y, 0, 1 , 0, 3, 0, 0 ]
                       );
            end
        else if _shp is TGIS_ShapeMultiPoint then
            begin
              part_size := _shp.GetPartSize( 0 ) ;
              for point_no := 0 to part_size - 1 do begin
                ptg := _shp.GetPoint( 0, point_no ) ;
                my_write( '%s%5d%12f%12f%6d%6d%6d%6d%6d%6d',
                          [ ftype, cnt, ptg.X, ptg.Y, 0, 0 , 0, 3, 0, 0  ]
                        ) ;
              end ;
            end
        else if ( _shp is TGIS_ShapeArc ) or ( _shp is TGIS_ShapePolygon ) then
            begin
               for part_no := 0 to num_parts - 1 do begin
                part_size := _shp.GetPartSize( part_no ) ;

                my_write( Format( '%s%5d%6d%6d%6d%6d%12s%6d%6d%6d',
                          [ DLG_LINE, cnt, 0, 0 , 0, 0, ' ', part_size, 3, 0 ]
                          ),[]
                        ) ;

                stab := '';
                j := 0 ;
                for point_no := 0 to part_size - 1 do begin
                  ptg := _shp.GetPoint( 0, point_no ) ;
                  stab := stab + Format( '%12f%12f', [ ptg.X , ptg.Y ] ) ;
                  inc( j ) ;
                  if j > 2 then begin  // write complete (3 coords)
                    my_write( stab, [] );
                    j    := 0;
                    stab := '';
                  end;
                end;

                if j <> 0 then  //write the rest
                    my_write( stab, [] );

              end
            end ;

        // write attributes
        my_write( Format('%6d%6d%6d%6d%6d%6d',
                          [
                           VarToInt32( _shp.GetField( MAJOR_FIELD1 )) ,
                           VarToInt32( _shp.GetField( MINOR_FIELD1 )) ,
                           VarToInt32( _shp.GetField( MAJOR_FIELD2 )) ,
                           VarToInt32( _shp.GetField( MINOR_FIELD2 )) ,
                           VarToInt32( _shp.GetField( MAJOR_FIELD3 )) ,
                           VarToInt32( _shp.GetField( MINOR_FIELD3 ))
                          ]
                         ), []
                );

        if      ftype = DLG_AREA then inc( _area )
        else if ftype = DLG_NODE then inc( _node )
        else                          inc( _line );

      finally
        _shp.Unlock ;
      end ;
    finally
      {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator  := store.DecimalSeparator ;
      {$IFDEF DCC}FormatSettings.{$ENDIF}ThousandSeparator := store.ThousandSeparator ;
    end ;
  end ;

  procedure TGIS_LayerDLG.setUp ;
  var
    line  : String         ;
    tkn   : TGIS_Tokenizer ;
    i     : Integer        ;

    function _t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( tkn.Result[0], _val ) = 0 ;
    end ;

  begin
    inherited ;

    FSupportedShapes := GisGetEmptyShapeType ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Point ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Arc ) ;

    if not IsStringEmpty( Path ) then begin
      FdlgFile  := TGIS_BufferedFileStream.Create( Path, TGIS_StreamMode.Read ) ;
      bDlgOpt   := UpperCase( GetFileExt( Path ) ) = C_OPT_EXT;

      AddFieldInternal( DLG_FEA_TYPE, TGIS_FieldType.String,   1, 0 ) ;
      AddFieldInternal( DLG_FEA_DESC, TGIS_FieldType.String,   1, 0 ) ;
      AddFieldInternal( MAJOR_FIELD1, TGIS_FieldType.Number,  10, 0 ) ;
      AddFieldInternal( MINOR_FIELD1, TGIS_FieldType.Number,  10, 0 ) ;
      AddFieldInternal( MAJOR_FIELD2, TGIS_FieldType.Number,  10, 0 ) ;
      AddFieldInternal( MINOR_FIELD2, TGIS_FieldType.Number,  10, 0 ) ;
      AddFieldInternal( MAJOR_FIELD3, TGIS_FieldType.Number,  10, 0 ) ;
      AddFieldInternal( MINOR_FIELD3, TGIS_FieldType.Number,  10, 0 ) ;

      tkn := TGIS_Tokenizer.Create ;

      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;

      FdlgRecNo  := 0 ;
      Lock ;
      try
        readCategory ;

        //read nodes and areas
        i := 0;
        while i < ( FNodeCount + FAreaCount ) do begin
          if not readRec( line ) then exit ;

          tkn.ExecuteEx( line, ' ' ) ;
          if tkn.Result.Count > 0 then begin
            if _t( DLG_NODE ) or _t( DLG_AREA ) then begin
              buildPoint( tkn.Result[0], tkn.Result[2], tkn.Result[3] ) ;
              inc( i ) ;
            end
            else  // attributes
              addAttributes( tkn ) ;
          end ;
        end ;

        // read lines
        i := 0;
        while i < FLineCount do begin
          if not readRec( line ) then exit ;

          tkn.ExecuteEx( line, ' ' ) ;
          if tkn.Result.Count > 0 then begin
            if _t( DLG_LINE ) then begin
              buildLine( tkn.Result[0], tkn.Result[6] ) ;
              inc( i );
            end
            else  // attributes
              addAttributes( tkn ) ;
          end ;
        end ;
      finally
        Unlock ;
        FIsModified := False ;

        RaiseBusyRelease( Self ) ;

        FreeObject( tkn ) ;
        FreeObject( FdlgFile );
      end ;
    end
    else
      sCategory := 'Unknown' ;

    if bDlgOpt then
       FFileInfo := 'DLG Optional Format - ' + sCategory
    else
       FFileInfo := 'DLG Standard Format - ' + sCategory;
  end ;

  procedure TGIS_LayerDLG.Build( const _path   : String ;
                                 const _extent : TGIS_Extent;
                                 const _type   : TGIS_ShapeType ;
                                 const _dim    : TGIS_DimensionType) ;
  begin
    inherited ;

    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;
  end ;

  procedure TGIS_LayerDLG.ImportLayerEx(
    const _layer       : TGIS_LayerVector  ;
    const _extent      : TGIS_Extent       ;
    const _type        : TGIS_ShapeType    ;
    const _scope       : String            ;
    const _shape       : TGIS_Shape        ;
    const _de9im       : String            ;
    const _truncated   : Boolean
  ) ;
  var
    shape_file : TGIS_BufferedFileStream    ;
    {$IFNDEF OXYGENE}
      shp      : TGIS_Shape  ;
    {$ENDIF}
    shp_tmp    : TGIS_Shape  ;
    same_name  : Boolean     ;
    ex         : TGIS_Extent ;
    shape_no   : Cardinal    ;
    end_uid    : TGIS_Uid    ;
    abort      : Boolean     ;
    inode      : Integer     ;
    iarea      : Integer     ;
    iline      : Integer     ;
    ftype      : String      ;
    oldscope   : String      ;
    cs         : TGIS_CSProjectedCoordinateSystem ;
  begin
    if not assigned( _layer ) then exit ;

    if not CheckFileWriteAccessEx( Path, True, True, True ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    if not (Self.CS is TGIS_CSProjectedCoordinateSystem) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_PRJ_NOTSAVEABLE ), Path, 0 ) ;

    cs := TGIS_CSProjectedCoordinateSystem(Self.CS) ;

    if (cs.ProjectionEPSG <> CSPROJ_Transverse_Mercator) and
       (cs.ProjectionEPSG <> CSPROJ_Albers             ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_PRJ_NOTSAVEABLE ), Path, 0 ) ;

    inode      := 1 ;
    iarea      := 1 ;
    iline      := 1 ;
    FNodeCount := 0 ;
    FAreaCount := 0 ;
    FLineCount := 0 ;
    shape_no   := 0 ;
    end_uid    := _layer.GetLastUid ;
    abort      := False ;

    if CompareText( GetFileExt( Path ), C_OPT_EXT ) <> 0 then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXTENSION ), Path, 0 ) ;
    end;

    same_name := CompareText( GetPathAbsolute( '', Path        ),
                              GetPathAbsolute( '', _layer.Path )
                            ) = 0  ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try

      // count number of areas, nodes and lines
      try
        oldscope := _layer.Scope ;
        if same_name then
          _layer.Scope := '' ;

        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
            _layer.Loop( _extent, _scope, _shape, _de9im ) do
        begin
          shp_tmp := shp.PrepareExportShape(
                           cs, _extent, _truncated, True
                         ) ;
          try
            if assigned( shp_tmp ) and
               ( not shp_tmp.IsDeleted ) and
               ( ( _type = shp_tmp.ShapeType   ) or
                 ( _type = TGIS_ShapeType.Unknown )
               )
            then begin
              ftype := VarToString( shp_tmp.GetField( DLG_FEA_TYPE ) ) ;
              case shp_tmp.ShapeType of
                TGIS_ShapeType.Point      :
                  if ftype = DLG_AREA then
                    inc( FAreaCount )
                  else
                    inc( FNodeCount ) ;
                TGIS_ShapeType.MultiPoint :
                  if ftype = DLG_AREA then
                    inc( FAreaCount )
                  else
                    inc( FNodeCount, shp_tmp.GetPartSize(0) ) ;
                TGIS_ShapeType.Arc,
                TGIS_ShapeType.Polygon    :
                  inc( FLineCount ) ;
              end;
            end ;
          finally
            if shp <> shp_tmp then
              FreeObject( shp_tmp ) ;
          end ;
        end ;
      finally
        _layer.Scope := oldscope ;
      end ;

      _layer.PrepareExportFieldNames( 32 ) ;

      ex := GisCommonExtent( _layer.Extent, _extent ) ;
      // prepare temporary geometry
      try
        shape_file := TGIS_BufferedFileStream.Create( GetTemporaryName( Path ),
                                                      TGIS_StreamMode.&Create
                                                     ) ;
        shape_file.CodePage    := CodePage    ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                     GetTemporaryName( Path ),
                                     GetLastError
                                   ) ;
      end ;

      try
        writeHeader( shape_file, _layer, FNodeCount, FAreaCount, FLineCount ) ;

        oldscope := _layer.Scope ;
        _layer.Scope := '' ;

        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
            _layer.Loop( ex, _scope, _shape, _de9im ) do
        begin
          shp_tmp := shp.PrepareExportShape(
                           cs, _extent, _truncated, True
                         ) ;
          try
            if assigned( shp_tmp )
               and
               ( not shp_tmp.IsDeleted )
               and
               ( ( _type = shp_tmp.ShapeType   ) or
                 ( _type = TGIS_ShapeType.Unknown )
               )
            then begin
              writeGeometry( shape_file, shp_tmp, inode, iarea, iline ) ;
            end ;
          finally
            if shp <> shp_tmp then FreeObject( shp_tmp ) ;
          end ;
          if shape_no mod 100 = 1 then begin
            abort := RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
            if abort then break ;
          end ;
          inc( shape_no ) ;
        end ;
      finally
        _layer.Scope := oldscope ;

        FreeObject( shape_file ) ;

        if abort then begin
          DeleteFile( GetTemporaryName( Path ) ) ;
        end
        else begin
          DeleteFile( GetBackupName( Path ) ) ;
          RenameFile( Path, GetBackupName( Path ) ) ;
          try
            if not RenameFile( GetTemporaryName( Path ), Path ) then
              raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                           Path,
                                           GetLastError
                                         ) ;
          except
            // recover ;
            RenameFile( GetBackupName( Path ), Path ) ;
            raise ;
          end ;
        end ;

        if not IsOpened then begin
          Items.Clear ;
          Fields.Clear ;

          Open ;
        end;
      end ;
    finally
      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  procedure TGIS_LayerDLG.SaveData  ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;


//==============================================================================
// Lider.CG.GIS.GeoLayerDLG
//==============================================================================

  class procedure GisLayerDLG.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-DLG', 'Digital Line Graphs DLG',
                   TGIS_LayerDLG, '.opt;.dlg',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                    False
                  ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    T_ArraysDLG := T_ArraysDLG_class.Create ;
    GisLayerDLG.SelfRegisterLayer() ;

  finalization
    FreeObject( T_ArraysDLG ) ;
{$ENDIF}

//==================================== END =====================================
end.

