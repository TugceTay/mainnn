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
  Support for Geocoding process.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoGeocoding ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoGeocoding"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoAddressMatching ;
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
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Event checks house numbers.
  /// </summary>
  /// <param name="_match">
  ///   matching scope
  /// </param>
  /// <param name="_houseNumber">
  ///   address found
  /// </param>
  TGIS_AddressEvent = {$IFDEF OXYGENE} public {$ENDIF} function (
    const _match        : String ;
    const _houseNumber  : String
  ) : Boolean of object ;

  /// <summary>
  ///   Class encapsulates Geocoding support.
  /// </summary>
  TGIS_Geocoding = {$IFDEF OXYGENE} public {$ENDIF}
                   class( TGIS_BaseObjectDisposable )

    private // property internal values
      FFormulasString : String ;
      FRoadName       : String ;
      FLFrom          : String ;
      FLTo            : String ;
      FRFrom          : String ;
      FRTo            : String ;
      FCityName       : String ;
      FStreetName     : String ;
      FHouseNumber    : String ;
      FUniqueName     : String ;
      FOffset         : Double ;
      FUid            : TGIS_ObjectList ;
      FUidEx          : TGIS_ObjectList ;
      FPoint          : TGIS_ObjectList ;
      FQuery          : TStrings ;
      FQueryEx        : TStrings ;
      FReverseRestrictQuery : String ;
      FOSMGeocoding   : Boolean ;

    {$IFDEF OXYGENE}
      protected // properties events
    {$ENDIF}
      /// <summary>
      ///   Event which will be fired on each found address.
      /// </summary>
      FOnAddress  : TGIS_AddressEvent ;

    protected  // property access routines

      function  fget_Uid      ( _idx : Integer ) : TGIS_Uid ;
      function  fget_UidEx    ( _idx : Integer ) : TGIS_Uid ;
      function  fget_Point    ( _idx : Integer ) : TGIS_Point ;
      function  fget_Query    ( _idx : Integer ) : String ;
      function  fget_QueryEx  ( _idx : Integer ) : String ;
      procedure fset_Formulas ( const _value : String ) ;

    private // private variables
      layerObj   : TGIS_LayerVector ;
      layerGeo   : TGIS_LayerVector ;
      shpType    : TGIS_ShapeType   ;
      matcherObj : TGIS_AddressMatching ;
      readyList  : TStringList ; // list of ready (found) addresses
      fieldList  : TGIS_ObjectList ;
      uniqueId   : TStrings ;
      stdAddress : String ;
      osmGeo     : TObject ;

    private // private routines

      /// <summary>
      ///   Clear the entire result list (and free allocated items)
      /// </summary>
      procedure clearResult          ;

      /// <summary>
      ///   Removes binding between address formulas fields and layer fields.
      /// </summary>
      procedure clearFields          ;

      /// <summary>
      ///   Sets binding between address formulas fields and layer fields.
      /// </summary>
      procedure loadFields           ( const _fieldsSection : TStrings
                                     ) ;

      /// <summary>
      ///   Prepare Equal comparison string based on field types - different
      ///   for string types and different for other
      /// </summary>
      /// <param name="_name">
      ///   field name
      /// </param>
      function  getEqualHelper       ( const _name          : String
                                     ) : String ;

      /// <summary>
      ///   <para>
      ///     Put fields in found matches in order conforming to the FIELDS
      ///     section.
      ///   </para>
      ///   <para>
      ///     The order the fields are placed in the section defines also how
      ///     the fields will be placed in the queries. It enables making
      ///     efficient queries.
      ///   </para>
      /// </summary>
      /// <param name="_resolvedAddresses">
      ///   list of found matches
      /// </param>
      procedure sortResolvedAddresses( const _resolvedAddresses : TGIS_ObjectList
                                     ) ;

      /// <summary>
      ///   Trim spaces. All spaces before and after text will be truncated.
      ///   All multiple spaces within text will be replaced by single space.
      /// </summary>
      /// <param name="_txt">
      ///   line with string to be trimmed out
      /// </param>
      function  trimSpace            ( const _txt        : String
                                     ) : String ;

      /// <summary>
      ///   Separate an address line based on a street crossing sign ('&amp;').
      /// </summary>
      /// <param name="_addr">
      ///   address line in the form of: "street &amp; streetB"
      /// </param>
      /// <param name="_nameA">
      ///   separated first street
      /// </param>
      /// <param name="_nameB">
      ///   separated second street
      /// </param>
      function  splitCrossing        ( const _addr       : String ;
                                       var   _nameA      : String ;
                                       var   _nameB      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Separate an address line based on number.
      /// </summary>
      /// <param name="_addr">
      ///   address line in the form of: "name 12xxx"; all characters after
      ///   last digit will be ignored
      /// </param>
      /// <param name="_name">
      ///   separated street name
      /// </param>
      /// <param name="_number">
      ///   separated number ("12")
      /// </param>
      function  splitNumber          ( const _addr       : String ;
                                       var   _name       : String ;
                                       var   _number     : Integer
                                     ) : Boolean ;

      /// <summary>
      ///   Find all intersections of a street given by name.
      /// </summary>
      /// <param name="_nameA">
      ///   first street
      /// </param>
      /// <param name="_nameB">
      ///   second street
      /// </param>
      /// <param name="_firstMatch">
      ///   if True, then only first matching address will be found; if not
      ///   then first 256 results will be returned
      /// </param>
      function  findCrossing         ( const _nameA      : String  ;
                                       const _nameB      : String  ;
                                       const _firstMatch : Boolean
                                     ) : Integer ;

      /// <summary>
      ///   Find all address points for a given address line.
      /// </summary>
      /// <param name="_name">
      ///   first street
      /// </param>
      /// <param name="_number">
      ///   street number
      /// </param>
      /// <param name="_firstMatch">
      ///   if True, then only first matching address will be found; if not
      ///   then first 256 results will be returned
      /// </param>
      /// <param name="_addToLayer">
      ///   if True, then parsed address will be added to the LayerGeocoded;
      ///   street will be split at the address to allow proper routing
      /// </param>
      function  findNumber           ( const _name       : String  ;
                                       const _number     : Integer ;
                                       const _firstMatch : Boolean ;
                                       const _addToLayer : Boolean
                                     ) : Integer ;

      {$IFDEF OXYGENE}
        function  get_output_txt     ( const _uid        : TGIS_Uid
                                     ) : String ; overload;
        function  get_output_txt     ( const _uid        : TGIS_Uid ;
                                       const _bestside   : Integer ;
                                       const _number     : Integer
                                     ) : String ; overload;
      {$ELSE}
        function  get_output_txt     ( const _uid        : TGIS_Uid     ;
                                       const _bestside   : Integer = 0 ;
                                       const _number     : Integer = 0
                                     ) : String ;
      {$ENDIF}

      /// <summary>
      ///   Separate an address line based on a street crossing sign ('&amp;').
      ///   Used by the address formulas mode.
      /// </summary>
      /// <param name="_addr">
      ///   address line in the form of: "street &amp; streetB"
      /// </param>
      /// <param name="_nameA">
      ///   separated first street
      /// </param>
      /// <param name="_nameB">
      ///   separated second street
      /// </param>
      function  splitCrossingEx      ( const _addr       : String ;
                                       var   _nameA      : String ;
                                       var   _nameB      : String
                                     ) : Boolean ;

      /// <summary>
      ///   Remove from the lists prepared for finding crossings matches with
      ///   empty street names.
      /// </summary>
      /// <param name="_resolvedAddresses">
      ///   first list of found matches
      /// </param>
      /// <param name="_resolvedAddresses2">
      ///   second list of found matches
      /// </param>
      procedure prepareForCrossing   ( var _resolvedAddresses    : TGIS_ObjectList ;
                                       var _resolvedAddresses2   : TGIS_ObjectList
                                     ) ;

      /// <summary>
      ///   <para>
      ///     Find all intersections of streets given by the two lists.
      ///   </para>
      ///   <para>
      ///     Used by the address formulas mode. Processes results produced
      ///     by address matching class.
      ///   </para>
      /// </summary>
      /// <param name="_resolvedAddresses">
      ///   matches for first street
      /// </param>
      /// <param name="_resolvedAddresses2">
      ///   matches for second street
      /// </param>
      /// <param name="_firstMatch">
      ///   if True, then only first matching intersection will be found; if
      ///   not then first 256 results will be returned
      /// </param>
      /// <param name="_extendedScope">
      ///   if True, then street- and city names in found matches are treated
      ///   as initial strings of the names, otherwise as exact names
      /// </param>
      function  findCrossingEx       ( const _resolvedAddresses  : TGIS_ObjectList ;
                                       const _resolvedAddresses2 : TGIS_ObjectList ;
                                       const _firstMatch         : Boolean  ;
                                       const _extendedScope      : Boolean
                                     ) : Integer ;

      /// <summary>
      ///   <para>
      ///     Finds all address points given by the list.
      ///   </para>
      ///   <para>
      ///     Used by the address formulas mode. Processes result produced by
      ///     address matching class.
      ///   </para>
      /// </summary>
      /// <param name="_resolvedAddresses">
      ///   found matches
      /// </param>
      /// <param name="_firstMatch">
      ///   if True, then only first matching address will be found; if not
      ///   then first 256 results will be returned
      /// </param>
      /// <param name="_extendedScope">
      ///   if True, then street- and city names in found matches are treated
      ///   as initial strings of the names, otherwise as exact names
      /// </param>
      /// <param name="_addToLayer">
      ///   if True, then parsed address will be added to the LayerGeocoded;
      ///   street will be split at the address to allow proper routing
      /// </param>
      function  findAddressEx        ( const _resolvedAddresses  : TGIS_ObjectList ;
                                       const _firstMatch         : Boolean  ;
                                       const _extendedScope      : Boolean  ;
                                       const _addToLayer         : Boolean
                                     ) : Integer ;
    protected
      procedure doDestroy ; override;

    public // public methods
      /// <summary>
      ///   Construct instance. This is a constructor for layers with shapes of
      ///   type TGIS_ShapeType.Arc.
      /// </summary>
      /// <param name="_layer">
      ///   vector layer on which geocoding will be performed
      /// </param>
      /// <remarks>
      ///   See Parse for example.
      /// </remarks>
      constructor Create             ( const _layer    : TGIS_LayerVector
                                     ) ; overload;

      /// <summary>
      ///   Construct instance.
      /// </summary>
      /// <param name="_layer">
      ///   vector layer on which geocoding will be performed
      /// </param>
      /// <param name="_shpType">
      ///   type of analyzed shapes
      /// </param>
      /// <remarks>
      ///   See Parse for example.
      /// </remarks>
      constructor Create             ( const _layer    : TGIS_LayerVector ;
                                       const _shpType  : TGIS_ShapeType
                                     ) ; overload;
    public

      /// <summary>
      ///   Clear any results from geocoding.
      /// </summary>
      /// <remarks>
      ///   See Parse for example.
      /// </remarks>
      procedure   Clear              ;

      /// <summary>
      ///   Sets address formulas definition and other settings used by
      ///   geocoding and reverse geocoding.
      /// </summary>
      /// <param name="_languageFileName">
      ///   name of an INI file with address formulas for a country
      /// </param>
      /// <param name="_fmtFileName">
      ///   name of an INI file with more detailed address formulas
      ///   definition
      /// </param>
      procedure LoadFormulas       ( const _languageFileName : String        ;
                                     const _fmtFileName      : String
                                   ) ; overload;
      /// <summary>
      ///   Sets address formulas definition and other settings used by
      ///   geocoding and reverse geocoding.
      /// </summary>
      /// <param name="_languageFileName">
      ///   name of an INI file with address formulas for a country
      /// </param>
      /// <param name="_fmtFileName">
      ///   name of an INI file with more detailed address formulas
      ///   definition
      /// </param>
      /// <param name="_customDefinition">
      ///   additional modification in form of the INI file contents
      /// </param>
      procedure LoadFormulas       ( const _languageFileName : String        ;
                                     const _fmtFileName      : String        ;
                                     const _customDefinition : String
                                   ) ; overload;

      /// <summary>
      ///   <para>
      ///     Parses given address line.
      ///   </para>
      ///   <para>
      ///     Finds matches according to defined address formulas.
      ///     Intersections should be entered as "addressPointA &amp;
      ///     addressPointB".
      ///   </para>
      /// </summary>
      /// <param name="_addr">
      ///   address line
      /// </param>
      /// <param name="_resolvedAddresses">
      ///   list containing all found matches; every element is a single match
      ///   in the form of string list: 'field1=value1' 'field2=value2' '...';
      ///   if the reference equals nil, the object will be created ;
      /// </param>
      /// <param name="_resolvedAddresses2">
      ///   list containing all found matches for the second street (if
      ///   present); every element is a single match in the form of string
      ///   list: 'field1=value1' 'field2=value2' '...'; if the reference
      ///   equals nil, the object will be created ;
      /// </param>
      /// <remarks>
      ///   Internally uses TGIS_AddressMatching class. (see
      ///   TGIS_AddressMatching for more information about address formulas).
      /// </remarks>
      /// <returns>
      ///   True if match was found
      /// </returns>
      function  Match                ( const _addr               : String      ;
                                       var   _resolvedAddresses  : TGIS_ObjectList ;
                                       var   _resolvedAddresses2 : TGIS_ObjectList
                                     ) : Boolean ;

      /// <summary>
      ///   Parse an address line. See the overloaded version of the method.
      /// </summary>
      /// <param name="_addr">
      ///   address line in the form of: "streetA &amp; streetB"
      ///   (intersections) or "street 1111xxx" (address matching); street
      ///   name can be in any case, and can consist of only the first few
      ///   characters; for street addresses only the first part of a number
      ///   (built from digits) will be used; if the street name contains a
      ///   number at the tail (that is part of the street name, not the
      ///   street number), you can use the character code 255 as a
      ///   non-breaking space placed between the text part of the street
      ///   name and the numeric part of the street name (not placed between
      ///   the street name ending with the number and the street number)
      /// </param>
      /// <returns>
      ///   number of points or intersections found
      /// </returns>
      function  Parse              ( const _addr            : String
                                   ) : Integer ; overload;

      /// <summary>
      ///   <para>
      ///     Parse an address line. Resolve address point(s), or
      ///     intersection(s).
      ///   </para>
      ///   <para>
      ///     Resolved intersections or addresses will be stored in
      ///     properties: Uid, UidEx and Point.
      ///   </para>
      /// </summary>
      /// <param name="_addr">
      ///   address line in the form of: "streetA &amp; streetB"
      ///   (intersections) or "street 1111xxx" (address matching); street
      ///   name can be in any case, and can consist of only the first few
      ///   characters; for street addresses only the first part of a number
      ///   (built from digits) will be used; if the street name contains a
      ///   number at the tail (that is part of the street name, not the
      ///   street number), you can use the character code 255 as a
      ///   non-breaking space placed between the text part of the street
      ///   name and the numeric part of the street name (not placed between
      ///   the street name ending with the number and the street number)
      /// </param>
      /// <param name="_firstMatch">
      ///   if True, then only first matching address will be found; if not
      ///   then first 256 results will be returned
      /// </param>
      /// <param name="_addToLayer">
      ///   if True, then parsed address will be added to the LayerGeocoded;
      ///   street will be split at the address to allow proper routing
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Internally it uses findCrossing and findAddress methods.
      ///   </para>
      ///   <note type="caution">
      ///     Only TGIS_ShapeType.Arc can be analyzed.
      ///   </note>
      /// </remarks>
      /// <returns>
      ///   number of points or intersections found
      /// </returns>
      function  Parse              ( const _addr            : String         ;
                                     const _firstMatch      : Boolean        ;
                                     const _addToLayer      : Boolean
                                   ) : Integer ; overload;

      /// <summary>
      ///   <para>
      ///     Parses given address line.
      ///   </para>
      ///   <para>
      ///     Resolves address point(s), or intersection(s). Address points
      ///     in the address line according to defined address formulas. (see
      ///     TGIS_AddressMatching for more information about address
      ///     formulas).
      ///   </para>
      /// </summary>
      /// <param name="_addr">
      ///   address line
      /// </param>
      /// <param name="_firstMatch">
      ///   if True, then only first matching address will be found; if not
      ///   then first 256 results will be returned
      /// </param>
      /// <param name="_extendedScope">
      ///   if True, then street- and city names in found matches are treated
      ///   as initial strings of the names, otherwise as exact names
      /// </param>
      /// <param name="_addToLayer">
      ///   if True, then parsed address will be added to the LayerGeocoded;
      ///   street will be split at the address to allow proper routing
      /// </param>
      /// <remarks>
      ///   Internally uses findCrossingEx and findAddressEx methods.
      ///   <note type="caution">
      ///     Only TGIS_ShapeType.Arc or TGIS_ShapeType.Point can be
      ///     analyzed.
      ///   </note>
      /// </remarks>
      /// <returns>
      ///   number of points or intersections found
      /// </returns>
      function    ParseEx            ( const _addr            : String         ;
                                       {$IFNDEF OXYGENE}
                                         const _firstMatch    : Boolean        ;
                                         const _extendedScope : Boolean        ;
                                         const _addToLayer    : Boolean
                                       {$ELSE}
                                         const _firstMatch    : Boolean := True ;
                                         const _extendedScope : Boolean := True ;
                                         const _addToLayer    : Boolean := True
                                       {$ENDIF}
                                     ) : Integer ; overload;

      /// <summary>
      ///   <para>
      ///     Finds address point(s), or intersection(s) according to given
      ///     matches.
      ///   </para>
      ///   <para>
      ///     Intersections can be resolved only for shapes of type
      ///     TGIS_ShapeType.Arc.
      ///   </para>
      /// </summary>
      /// <param name="_resolvedAddresses">
      ///   list containing all found matches; every string is a single match
      ///   in form of 'fieldld1=value1#13#10field2=value2#13#10...'; if the
      ///   reference equals nil, the object will be created ;
      /// </param>
      /// <param name="_resolvedAddresses2">
      ///   list containing all found matches for the second street (if
      ///   present); every string is a single match in form of
      ///   'fieldld1=value1#13#10field2=value2#13#10...'; if the reference
      ///   equals nil, the object will be created ;
      /// </param>
      /// <param name="_firstMatch">
      ///   if True, then only first matching address will be found
      /// </param>
      /// <param name="_extendedScope">
      ///   if True, then street- and city names in found matches are treated
      ///   as initial strings of the names, otherwise as exact names
      /// </param>
      /// <param name="_addToLayer">
      ///   if True, then parsed address will be added to the LayerGeocoded;
      ///   street will be split at the address to allow proper routing
      /// </param>
      /// <remarks>
      ///  Internally uses findCrossingEx and findAddressEx methods.
      ///   <note type="caution">
      ///     Only TGIS_ShapeType.Arc or TGIS_ShapeType.Point can be
      ///     analyzed.
      ///   </note>
      /// </remarks>
      /// <returns>
      ///   number of points or intersections found
      /// </returns>
      function    ParseEx            ( {$IFNDEF OXYGENE}
                                         const _resolvedAddresses
                                                              : TGIS_ObjectList     ;
                                         const _resolvedAddresses2
                                                              : TGIS_ObjectList     ;
                                         const _firstMatch    : Boolean         ;
                                         const _extendedScope : Boolean         ;
                                         const _addToLayer    : Boolean
                                       {$ELSE}
                                         const _resolvedAddresses
                                                              : TGIS_ObjectList ;
                                         const _resolvedAddresses2
                                                              : TGIS_ObjectList ;
                                         const _firstMatch    : Boolean := True ;
                                         const _extendedScope : Boolean := True ;
                                         const _addToLayer    : Boolean := True
                                       {$ENDIF}
                                     ) : Integer ; overload;

      /// <summary>
      ///   Returns the closest address to the given point.
      ///   See the overloaded version of the method.
      /// </summary>
      /// <param name="_ptg">
      ///   given point
      /// </param>
      /// <param name="_prec">
      ///   precision /not a longer distance than/;
      /// </param>
      /// <param name="_shp">
      ///   returned shape object
      /// </param>
      /// <param name="_address">
      ///   returned address string created according to the address formulas
      ///   definition
      /// </param>
      /// <returns>
      ///   distance to the nearest address
      /// </returns>
      function  ReverseGeocode     ( const _ptg             : TGIS_Point     ;
                                     const _prec            : Double         ;
                                     var   _shp             : TGIS_Shape     ;
                                     var   _address         : String
                                   ) : Double ; overload;

      /// <summary>
      ///   <para>
      ///     Returns the closest address to the given point.
      ///   </para>
      ///   <para>
      ///     Only TGIS_ShapeType.Arc or TGIS_ShapeType.Point can be analyzed.
      ///   </para>
      /// </summary>
      /// <param name="_ptg">
      ///   given point
      /// </param>
      /// <param name="_prec">
      ///   precision /not a longer distance than/;
      /// </param>
      /// <param name="_shp">
      ///   returned shape object
      /// </param>
      /// <param name="_address">
      ///   returned address string created according to the address formulas
      ///   definition
      /// </param>
      /// <param name="_additionalQuery">
      ///   query that can more precisely define geocoded shapes ( is
      ///   assembled with ReverseRestrictQuery )
      /// </param>
      /// <returns>
      ///   distance to the nearest address
      /// </returns>
      function  ReverseGeocode     ( const _ptg             : TGIS_Point     ;
                                     const _prec            : Double         ;
                                     var   _shp             : TGIS_Shape     ;
                                     var   _address         : String         ;
                                     const _additionalQuery : String
                                   ) : Double ; overload;

      /// <summary>
      ///   Add a new node connection point to the network
      /// </summary>
      /// <param name="_ptg">
      ///   location of connection point
      /// </param>
      /// <param name="_prec">
      ///   precision; point not close enough will be ignored
      /// </param>
      /// <param name="_shape">
      ///   if null then closest shape on the layer associated to the geocoder
      ///   will be used; if not null then connection will be added to this
      ///   shape
      /// </param>
      /// <returns>
      ///   True if the operation succeeded
      /// </returns>
      function    AddPoint           ( const _ptg             : TGIS_Point     ;
                                       const _prec            : Double         ;
                                       const _shape           : TGIS_ShapeArc
                                     ) : Boolean ;

    public // properties

      /// <summary>
      ///   <para>
      ///     String in form of an INI file contents with address formulas
      ///     definition.
      ///   </para>
      ///   <para>
      ///     Writing sets address formulas ( cancels all previous settings
      ///     ). Reading makes current settings available.
      ///   </para>
      /// </summary>
      property FormulasString : String read FFormulasString write fset_Formulas ;

      /// <summary>
      ///   Layer with geocoded address.
      /// </summary>
      property LayerGeocoded : TGIS_LayerVector read layerGeo ;

      /// <summary>
      ///   <para>
      ///     Offset describes the size of the space between street and
      ///     address point.
      ///   </para>
      ///   <para>
      ///     For address at the end of the street at least 1.5*Offset will
      ///     be used. The value is in the layer units.
      ///   </para>
      /// </summary>
      property Offset : Double read FOffset write FOffset ;

      /// <summary>
      ///   Field for the feature name. Default is 'NAME'.
      /// </summary>
      property RoadName : String read FRoadName write FRoadName ;

      /// <summary>
      ///   <para>
      ///     Field name for the starting number on the left side of the
      ///     street.
      ///   </para>
      ///   <para>
      ///     Default is 'LFROM'. Used only with old style functionality.
      ///   </para>
      /// </summary>
      property LFrom : String read FLFrom write FLFrom ;

      /// <summary>
      ///   Field name for the ending number on the left side of the street.
      ///   Default is 'LTO'. Used only with old style functionality.
      /// </summary>
      property LTo : String read FLTo write FLTo ;

      /// <summary>
      ///   Field name for the starting number on the right side of the
      ///   street. Default is 'RFROM'. Used only with old style functionality.
      /// </summary>
      property RFrom : String read FRFrom write FRFrom ;

      /// <summary>
      ///   Field name for the ending number on the right side of the street.
      ///   Default is 'RTO'. Used only with old style functionality.
      /// </summary>
      property RTo : String read FRTo write FRTo ;

      /// <summary>
      ///   <para>
      ///     Field for the city name.
      ///   </para>
      ///   <para>
      ///     Used to indicate the city name field that can be searched
      ///     with like-operator. It must be a name taken from address
      ///     formulas definition. Default value is 'CITYNAME'. Used only
      ///     with address formulas mode.
      ///   </para>
      /// </summary>
      property CityName : String read FCityName write FCityName ;

      /// <summary>
      ///   <para>
      ///     Field for the street name.
      ///   </para>
      ///   <para>
      ///     Used to indicate the street name field that can be searched
      ///     with like-operator. It must be a name taken from address
      ///     formulas definition. Default value is 'STREETNAME'. Used only
      ///     with address formulas mode.
      ///   </para>
      /// </summary>
      property StreetName : String read FStreetName write FStreetName ;

      /// <summary>
      ///   <para>
      ///     Field for the house number
      ///   </para>
      ///   <para>
      ///     Used to indicate the house number field that is processed in
      ///     special way. It must be a name taken from address formulas
      ///     definition. Default value is 'HOUSENUMBER'. Used only with
      ///     address formulas mode.
      ///   </para>
      /// </summary>
      property HouseNumber : String read FHouseNumber write FHouseNumber ;

      /// <summary>
      ///   Field for a name of a special key indicating a unique street
      ///   identifier. Default is 'UNIQUENAME'. Used only with address
      ///   formulas
      /// </summary>
      property UniqueName : String read FUniqueName write FUniqueName ;

      /// <summary>
      ///   List of resolved streets Uids.
      /// </summary>
      /// <param name="_idx">
      ///   index of the value within the list of results
      /// </param>
      property Uid  [ _idx: Integer] : TGIS_Uid read fget_Uid   ;

      /// <summary>
      ///   List of resolved street Uids. Only for crossings - Uid for a
      ///   second street on an intersection.
      /// </summary>
      /// <param name="_idx">
      ///   index of the value within the list of results
      /// </param>
      property UidEx[ _idx: Integer] : TGIS_Uid read fget_UidEx ;

      /// <summary>
      ///   List of address points. Only for finding addresses.
      /// </summary>
      /// <param name="_idx">
      ///   index of the value within the list of results
      /// </param>
      property Point[ _idx: Integer] : TGIS_Point read fget_Point ;

      /// <summary>
      ///   <para>
      ///     String with processed query.
      ///   </para>
      ///   <para>
      ///     Query will be reformatted to the form defined at the formulas
      ///     definition in the section called OUTPUT. The second street of
      ///     crossroads will be added after the '&amp;' sign.
      ///   </para>
      /// </summary>
      /// <param name="_idx">
      ///   index of value within the list of results
      /// </param>
      property Query[ _idx: Integer] : String read fget_Query ;

      /// <summary>
      ///   <para>
      ///     String with process query.
      ///   </para>
      ///   <para>
      ///     Query will be represented in a decomposed form
      ///     (featurename1=featurevalue1;featurename2=featurevalue2;..).
      ///     For crossroads parameters for the second street will be added
      ///     after the '&amp;' sign.
      ///   </para>
      /// </summary>
      /// <param name="_idx">
      ///   index of the value within the list of results
      /// </param>
      property QueryEx[ _idx: Integer] : String read fget_QueryEx ;

      /// <summary>
      ///   String with a query defined for reverse geocoding at the address
      ///   formulas definition. It can be assembly with an additional query
      ///   passed as a parameter to the ReverseGeocode method.
      /// </summary>
      property ReverseRestrictQuery : String read FReverseRestrictQuery ;

      /// <summary>
      ///   <para>
      ///     If true the local geocoding resource is ignored and the requests
      ///     are send to the TatukGIS online geocoding service.
      ///   </para>
      ///   <para>
      ///     Internally uses the TGIS_OSMGeocoding class.
      ///   </para>
      /// </summary>
      property OSMGeocoding : Boolean read FOSMGeocoding write FOSMGeocoding ;

    published //events

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   <para>
        ///     Event which will be fired on each found address.
        ///   </para>
        ///   <para>
        ///     It must be used when an algorithm of checking the house
        ///     number is more heuristic and can not be only a simply data
        ///     base query.
        ///   </para>
        /// </summary>
        event AddressEvent    : TGIS_AddressEvent
                                delegate FOnAddress ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   <para>
        ///     Event which will be fired on each found address.
        ///   </para>
        ///   <para>
        ///     It must be used when an algorithm of checking the house
        ///     number is more heuristic and can not be only a simply data
        ///     base query.
        ///   </para>
        /// </summary>
        property AddressEvent   : TGIS_AddressEvent
                                  read  FOnAddress
                                  write FOnAddress ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.IniFiles,
    System.Variants,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoCsFactory,
    Lider.CG.GIS.GeoOSMServices ;
{$ENDIF}

const
  GIS_GEO_FIELDS_SECTION    = 'FIELDS';
  GIS_GEO_WORDCHARS_SECTION = 'WORDCHARS';

  GIS_GEO_OUTPUT_SECTION    = 'OUTPUT';
  GIS_GEO_STDADDRESS_KEY    = 'StandardAddress';
  GIS_GEO_STDADDRESS_VAL    = '<HOUSENUMBER>< ><DIRPREFIX>< ><STREETNAME>< >' +
                              '<STREETTYPE>< ><DIRSUFFIX><,>< ><CITYNAME><,>' +
                              '< ><ZIPCODE><-><ZIP4CODE>'                     ;

  GIS_GEO_REVERSE_SECTION   = 'REVERSE';
  GIS_GEO_RESTRICTQUERY_KEY = 'RestrictQuery';
  GIS_GEO_RESTRICTQUERY_VAL = '';

  // field names used for old style functionality
  GIS_GEO_OLD_ROADNAME = 'name'  ;
  GIS_GEO_OLD_LFROM    = 'lfrom' ;
  GIS_GEO_OLD_LTO      = 'lto'   ;
  GIS_GEO_OLD_RFROM    = 'rfrom' ;
  GIS_GEO_OLD_RTO      = 'rto'   ;

  // field names used for new style functionality
  GIS_GEO_CITYNAME     = 'CITYNAME'    ;
  GIS_GEO_STREETNAME   = 'STREETNAME'  ;
  GIS_GEO_HOUSENUMBER  = 'HOUSENUMBER' ;
  GIS_GEO_UNIQUENAME   = 'UNIQUENAME'  ;

  // max number of records returned by ParseEx
  GIS_GEO_RETURNED_LIMIT = 256 ;

  //name of the internal storage layer for online service response
  GIS_GEO_OSM_LAYER : String = 'osmgeocoding' ;

type

  /// <summary>
  ///   Wrapper of the TGIS_OSMGeocoding class.
  /// </summary>
  T_osmGeocoding = class( TGIS_ObjectDisposable )
    protected
      function  fget_Uid     ( _idx : Integer
                             ) : TGIS_Uid ;
      function  fget_UidEx   ( _idx : Integer
                             ) : TGIS_Uid ;
      function  fget_Point   ( _idx : Integer
                             ) : TGIS_Point ;
      function  fget_Query   ( _idx : Integer
                             ) : String ;
      function  fget_QueryEx ( _idx : Integer
                             ) : String ;
    private
      layerObj : TGIS_LayerVector ;
      layerGeo : TGIS_LayerVector ;
    private
      initGeo : Boolean ;
      oLayer  : TGIS_LayerVector ;
    private
      procedure init              ( const _layer         : TGIS_LayerVector
                                  ) ;
      function  makeAddressString (  const _shp          : TGIS_Shape
                                  ) : String ;
      function  makeAddressList   (  const _shp          : TGIS_Shape
                                  ) : TStringList ;
      function  parseInternal     ( const _addr          : String  ;
                                    const _firstMatch    : Boolean ;
                                    const _extendedScope : Boolean ;
                                    const _addToLayer    : Boolean ;
                                    const _append        : Boolean
                                  ) : Integer ;

    protected
      procedure doDestroy ; override ;

    public
      constructor Create         ( const _layer   : TGIS_LayerVector
                                 ) ; overload ;
      constructor Create         ( const _layer   : TGIS_LayerVector ;
                                   const _shpType : TGIS_ShapeType
                                 ) ; overload ;
    public
      procedure Clear            ;
      function  Match            ( const _addr               : String ;
                                   var   _resolvedAddresses  : TGIS_ObjectList ;
                                   var   _resolvedAddresses2 : TGIS_ObjectList
                                 ) : Boolean ;
      {$IFDEF OXYGENE}
        function  Parse          ( const _addr       : String
                                 ) : Integer ; overload ;
        function  Parse          ( const _addr       : String ;
                                   const _firstMatch : Boolean ;
                                   const _addToLayer : Boolean
                                 ) : Integer ; overload ;
      {$ELSE}
        function  Parse          ( const _addr       : String ;
                                   const _firstMatch : Boolean = True ;
                                   const _addToLayer : Boolean = True
                                 ) : Integer ;
      {$ENDIF}
      function    ParseEx        ( const _addr            : String ;
                                   {$IFNDEF OXYGENE}
                                     const _firstMatch    : Boolean ;
                                     const _extendedScope : Boolean ;
                                     const _addToLayer    : Boolean
                                   {$ELSE}
                                     const _firstMatch    : Boolean := True ;
                                     const _extendedScope : Boolean := True ;
                                     const _addToLayer    : Boolean := True
                                   {$ENDIF}
                                 ) : Integer ; overload ;
      function  ParseEx          ( {$IFNDEF OXYGENE}
                                     const _resolvedAddresses
                                                          : TGIS_ObjectList ;
                                     const _resolvedAddresses2
                                                          : TGIS_ObjectList ;
                                     const _firstMatch    : Boolean         ;
                                     const _extendedScope : Boolean         ;
                                     const _addToLayer    : Boolean
                                   {$ELSE}
                                     const _resolvedAddresses
                                                          : TGIS_ObjectList ;
                                     const _resolvedAddresses2
                                                          : TGIS_ObjectList ;
                                     const _firstMatch    : Boolean := True ;
                                     const _extendedScope : Boolean := True ;
                                     const _addToLayer    : Boolean := True
                                   {$ENDIF}
                                 ) : Integer ; overload ;
      {$IFDEF OXYGENE}
        function  ReverseGeocode ( const _ptg             : TGIS_Point ;
                                   const _prec            : Double ;
                                   var   _shp             : TGIS_Shape ;
                                   var   _address         : String
                                 ) : Double ; overload ;
        function  ReverseGeocode ( const _ptg             : TGIS_Point ;
                                   const _prec            : Double ;
                                   var   _shp             : TGIS_Shape ;
                                   var   _address         : String ;
                                   const _additionalQuery : String
                                 ) : Double ; overload ;
      {$ELSE}
        function  ReverseGeocode ( const _ptg             : TGIS_Point ;
                                   const _prec            : Double ;
                                   var   _shp             : TGIS_Shape ;
                                   var   _address         : String ;
                                   const _additionalQuery : String = ''
                                 ) : Double ;
      {$ENDIF}
    public
      property LayerGeocoded           : TGIS_LayerVector
                                         read  layerGeo ;
      property Uid[ _idx: Integer]     : TGIS_Uid
                                         read  fget_Uid   ;
      property UidEx[ _idx: Integer]   : TGIS_Uid
                                         read  fget_UidEx ;
      property Point[ _idx: Integer]   : TGIS_Point
                                         read  fget_Point ;
      property Query[ _idx: Integer]   : String
                                         read  fget_Query ;
      property QueryEx[ _idx: Integer] : String
                                         read  fget_QueryEx ;
  end ;


//==============================================================================
// T_osmGeocoding
//==============================================================================


  constructor T_osmGeocoding.Create(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    inherited Create ;

    init( _layer ) ;
  end ;


  constructor T_osmGeocoding.Create(
    const _layer   : TGIS_LayerVector ;
    const _shpType : TGIS_ShapeType
  ) ;
  begin
    inherited Create ;

    init( _layer ) ;
  end ;


  procedure T_osmGeocoding.doDestroy ;
  begin
    FreeObject( oLayer ) ;

    inherited ;
  end ;


  procedure T_osmGeocoding.init(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    layerObj := _layer ;

    if assigned( layerObj ) then
      layerGeo := TGIS_LayerVector(
        layerObj.Viewer.Ref.Get( GIS_GEO_LAYERNAME )
      ) ;

    if not assigned( layerGeo ) then begin
      layerGeo := TGIS_LayerVector.Create ;
      layerGeo.Name := GIS_GEO_LAYERNAME ;
      layerGeo.Active := False ;
      layerGeo.CS := layerObj.CS ;
      if assigned( layerObj ) then
        layerObj.Viewer.Ref.Add( layerGeo ) ;
    end ;
  end ;


  function T_osmGeocoding.fget_Uid(
    _idx : Integer
  ) : TGIS_Uid ;
  begin
    Result := oLayer.GetShape( _idx + 1 ).Uid ;
  end ;


  function T_osmGeocoding.fget_UidEx(
    _idx : Integer
  ) : TGIS_Uid ;
  begin
    Result := Self.Uid[_idx] ;
  end ;


  function T_osmGeocoding.fget_Point(
    _idx : Integer
  ) : TGIS_Point ;
  var
    shp : TGIS_Shape ;
  begin
    shp := oLayer.GetShape( _idx + 1 ) ;

    if assigned( shp ) then
      Result := shp.Centroid
    else
      Result := GisPoint( -GIS_MAX_DOUBLE, -GIS_MAX_DOUBLE ) ;
  end ;


  function T_osmGeocoding.fget_Query(
    _idx : Integer
  ) : String ;
  var
    shp : TGIS_Shape ;
  begin
    shp := oLayer.GetShape( _idx + 1 ) ;

    if assigned( shp ) then
      Result := makeAddressString( shp )
    else
      Result := '' ;
  end ;


  function T_osmGeocoding.fget_QueryEx(
    _idx : Integer
  ) : String ;
  begin
    Result := Self.Query[_idx] ;
  end ;


  function T_osmGeocoding.makeAddressString(
    const _shp : TGIS_Shape
  ) : String ;
  var
    str : String ;

    procedure add_with_comma( var _s : String ) ;
    begin
      if not IsStringEmpty( str ) then begin
        if IsStringEmpty( _s ) then
          _s := str
        else
          _s := _s + ', ' + str ;
      end ;
    end ;

  begin
    str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_NAME ) ) ;
    add_with_comma( Result ) ;
    str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_STREET ) ) ;
    add_with_comma( Result ) ;
    if not IsStringEmpty( str ) then begin
      str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_HOUSENUMBER ) ) ;
      Result := Result + ' ' + str ;
    end ;
    str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_CITY ) ) ;
    add_with_comma( Result ) ;
    str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_COUNTRY ) ) ;
    add_with_comma( Result ) ;
  end ;


  function T_osmGeocoding.makeAddressList(
    const _shp : TGIS_Shape
  ) : TStringList ;
  var
    lst : TStringList ;
    str : String ;
  begin
    lst := TStringList.Create ;

    str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_NAME ) ) ;
    if not IsStringEmpty( str ) then
      lst.Add( GIS_GEO_OSM_FIELD_NAME + '=' + str ) ;

    str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_STREET ) ) ;
    if not IsStringEmpty( str ) then
      lst.Add( GIS_GEO_OSM_FIELD_STREET + '=' + str ) ;

    str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_HOUSENUMBER ) ) ;
    if not IsStringEmpty( str ) then
      lst.Add( GIS_GEO_OSM_FIELD_HOUSENUMBER + '=' + str ) ;

    str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_CITY ) ) ;
    if not IsStringEmpty( str ) then
      lst.Add( GIS_GEO_OSM_FIELD_CITY + '=' + str ) ;

    str := VarToString( _shp.GetField( GIS_GEO_OSM_FIELD_COUNTRY ) ) ;
    if not IsStringEmpty( str ) then
      lst.Add( GIS_GEO_OSM_FIELD_COUNTRY + '=' + str ) ;

    Result := lst ;
  end ;


  function T_osmGeocoding.parseInternal(
    const _addr          : String  ;
    const _firstMatch    : Boolean ;
    const _extendedScope : Boolean ;
    const _addToLayer    : Boolean ;
    const _append        : Boolean
  ) : Integer ;
  var
    ogeo : TGIS_OSMGeocoding ;
    lgeo : TGIS_LayerVector ;
    {$IFNDEF OXYGENE}
      shp  : TGIS_Shape ;
    {$ENDIF}
    shpc : TGIS_Shape ;
    cnt  : Integer ;
    cs   : TGIS_CSCoordinateSystem ;
    pt   : TGIS_Point ;
  begin
    Result := 0 ;

    ogeo := TGIS_OSMGeocoding.Create ;
    try
      if _firstMatch then
        ogeo.Limit := 1
      else
        ogeo.Limit := 256 ;

      if assigned( layerObj ) then begin
        cs := TGIS_CSFactory.ByEPSG( 4326 ) ;
        pt := GisPoint(
                0.5*( layerObj.Extent.XMin + layerObj.Extent.XMax ),
                0.5*( layerObj.Extent.YMin + layerObj.Extent.YMax )
              ) ;

        ogeo.BiasPoint :=
          cs.FromCS( layerObj.CS, pt ) ;
        ogeo.RestrictedExtent :=
          cs.ExtentFromCS( layerObj.CS, layerObj.Extent ) ;
      end ;

      lgeo := ogeo.Forward( _addr ) ;

      if not _append then begin
        Clear ;
        oLayer := TGIS_LayerVector.Create ;
        oLayer.Name := GIS_GEO_OSM_LAYER ;
      end ;

      if not assigned( lgeo ) then
        exit ;

      if not _append then begin
        oLayer.CS := lgeo.CS ;
        oLayer.Open ;
        oLayer.ImportStructure( lgeo ) ;
      end ;

      for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in lgeo.Loop( lgeo.Extent ) do begin
        shpc := oLayer.AddShape( shp ) ;
        shpc.CopyFields( shp ) ;
      end ;

      if not initGeo then
        layerGeo.ImportStructure( oLayer ) ;

      if _addToLayer then begin
        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in oLayer.Loop( oLayer.Extent ) do begin
          shpc := layerGeo.AddShape( shp ) ;
          shpc.CopyFields( shp ) ;
        end ;
      end ;

      cnt := lgeo.GetLastUid ;

    finally
      FreeObject( ogeo ) ;
      FreeObject( lgeo ) ;
    end ;

    Result := cnt ;
  end ;


  procedure T_osmGeocoding.Clear ;
  begin
    FreeObject( oLayer ) ;
    if assigned( layerGeo ) then layerGeo.RevertShapes ;
  end ;


  function T_osmGeocoding.Match(
    const _addr               : String          ;
    var   _resolvedAddresses  : TGIS_ObjectList ;
    var   _resolvedAddresses2 : TGIS_ObjectList
  ) : Boolean ;
  var
    ogeo : TGIS_OSMGeocoding ;
    lgeo : TGIS_LayerVector ;
    {$IFNDEF OXYGENE}
      shp  : TGIS_Shape ;
    {$ENDIF}
    lst  : TStringList ;
  begin
    Result := False ;

    ogeo := TGIS_OSMGeocoding.Create ;
    try
      lgeo := ogeo.Forward( _addr ) ;

      if not assigned( lgeo ) then
        exit ;

      if lgeo.GetLastUid > 0 then begin

        if not assigned( _resolvedAddresses ) then
          _resolvedAddresses := TGIS_ObjectList.Create ;

        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in lgeo.Loop( lgeo.Extent ) do begin
          lst := makeAddressList( shp ) ;
          _resolvedAddresses.Add( lst ) ;
        end ;
      end
      else
        exit ;

    finally
      FreeObject( ogeo ) ;
      FreeObject( lgeo ) ;
    end ;

    Result := True ;
  end ;


  {$IFDEF OXYGENE}
    function T_osmGeocoding.Parse(
      const _addr : String
    ) : Integer ;
    begin
      Result := Parse( _addr, True, True ) ;
    end ;

    function T_osmGeocoding.Parse(
      const _addr        : String         ;
      const _firstMatch  : Boolean        ;
      const _addToLayer  : Boolean
    ) : Integer ;
  {$ELSE}
    function T_osmGeocoding.Parse(
      const _addr       : String          ;
      const _firstMatch : Boolean = True  ;
      const _addToLayer : Boolean = True
    ) : Integer ;
  {$ENDIF}
  begin
    Result := parseInternal(
                _addr,
                _firstMatch,
                False,
                _addToLayer,
                False
              ) ;
  end ;


  function T_osmGeocoding.ParseEx(
    const _addr            : String          ;
    {$IFNDEF OXYGENE}
      const _firstMatch    : Boolean         ;
      const _extendedScope : Boolean         ;
      const _addToLayer    : Boolean
    {$ELSE}
      const _firstMatch    : Boolean := True ;
      const _extendedScope : Boolean := True ;
      const _addToLayer    : Boolean := True
    {$ENDIF}
  ) : Integer ;
  begin
    Result := parseInternal(
                _addr,
                _firstMatch,
                _extendedScope,
                _addToLayer,
                False
              ) ;
  end ;


  {$IFDEF OXYGENE}
    function T_osmGeocoding.ParseEx(
      const _resolvedAddresses  : TGIS_ObjectList ;
      const _resolvedAddresses2 : TGIS_ObjectList ;
      const _firstMatch         : Boolean := True ;
      const _extendedScope      : Boolean := True ;
      const _addToLayer         : Boolean := True
    ) : Integer ;
  {$ELSE}
    function T_osmGeocoding.ParseEx(
      const _resolvedAddresses  : TGIS_ObjectList     ;
      const _resolvedAddresses2 : TGIS_ObjectList     ;
      {$IFNDEF OXYGENE}
        const _firstMatch       : Boolean         ;
        const _extendedScope    : Boolean         ;
        const _addToLayer       : Boolean
      {$ELSE}
        const _firstMatch       : Boolean := True ;
        const _extendedScope    : Boolean := True ;
        const _addToLayer       : Boolean := True
      {$ENDIF}
    ) : Integer ;
  {$ENDIF}
  var
    lst  : TStrings ;
    addr : String ;
    key  : String ;
    val  : String ;
    cnt  : Integer ;
    i    : Integer ;
    k    : Integer ;
  begin
    cnt := 0 ;

    for i := 0 to _resolvedAddresses.Count - 1 do begin
      lst := TStrings( _resolvedAddresses[i] ) ;
      addr := '' ;
      for k := 0 to lst.Count - 1 do begin
        key := lst.Names[k] ;
        val := lst.Values[key] ;
        if IsStringEmpty( val ) then
          continue ;
        if IsStringEmpty( addr ) then
          addr := val
        else
          addr := addr + ', ' + val ;
      end ;

      cnt := cnt + parseInternal(
                     addr,
                     _firstMatch,
                     _extendedScope,
                     _addToLayer,
                     True
                   ) ;
    end ;

    for i := 0 to _resolvedAddresses2.Count - 1 do begin
      lst := TStrings( _resolvedAddresses2[i] ) ;
      addr := '' ;
      for k := 0 to lst.Count - 1 do begin
        key := lst.Names[k] ;
        val := lst.Values[key] ;
        if IsStringEmpty( val ) then
          continue ;
        if IsStringEmpty( addr ) then
          addr := val
        else
          addr := addr + ', ' + val ;
      end ;

      cnt := cnt + parseInternal(
                     addr,
                     _firstMatch,
                     _extendedScope,
                     _addToLayer,
                     True
                   ) ;
    end ;

    Result := cnt ;
  end ;


  {$IFDEF OXYGENE}
    function T_osmGeocoding.ReverseGeocode(
      const _ptg     : TGIS_Point ;
      const _prec    : Double     ;
      var   _shp     : TGIS_Shape ;
      var   _address : String
    ) : Double ;
    begin
      Result := ReverseGeocode( _ptg, _prec, _shp, _address, '' ) ;
    end;

    function T_osmGeocoding.ReverseGeocode(
      const _ptg             : TGIS_Point ;
      const _prec            : Double     ;
      var   _shp             : TGIS_Shape ;
      var   _address         : String     ;
      const _additionalQuery : String
    ) : Double ;
  {$ELSE}
    function T_osmGeocoding.ReverseGeocode(
      const _ptg             : TGIS_Point ;
      const _prec            : Double     ;
      var   _shp             : TGIS_Shape ;
      var   _address         : String     ;
      const _additionalQuery : String = ''
    ) : Double ;
  {$ENDIF}
  var
    ogeo : TGIS_OSMGeocoding ;
  begin
    Result := -1.0 ;

    ogeo := TGIS_OSMGeocoding.Create ;
    try
      Clear ;
      oLayer := ogeo.Reverse( _ptg ) ;

      if not assigned( oLayer ) then
        exit ;

      if not initGeo then
        layerGeo.ImportStructure( oLayer ) ;

      _shp := oLayer.GetShape( 1 ) ;
      _address := makeAddressString( _shp ) ;

    finally
      FreeObject( ogeo ) ;
    end ;

    Result := _shp.Distance( _ptg, _prec ) ;
  end ;


type
  // Class handles street crossings.
  T_node = class
    public
      Ptg : TGIS_Point ;
      Uid : TGIS_Uid    ;
      constructor Create ( const _ptg : TGIS_Point ;
                           const _uid : TGIS_Uid
                         ) ;
  end ;

  // Class for list of all street crossings.
  T_nodeList = class ( TGIS_ObjectList )
    public

      // Add a new node to a list; list will be sorted by _ptg.X and then by _ptg.Y
      // to speedup process.
      // _ptg  node to be added
      // _uid  uid associated with the node
      procedure  Add     ( const _ptg : TGIS_Point ;
                           const _uid : TGIS_Uid
                         ) ;

      // Find a first node given by _ptg on list.
      // _ptg         node to be found
      // return       found node, or nil
      procedure  Find    ( const _ptg : TGIS_Point ;
                           var list   : TStrings
                         ) ;
  end ;

  // Class handles found street crossings.
  T_foundNode = class
    public
      Ptg   : TGIS_Point ;
      Uid   : TGIS_Uid    ;
      UidEx : TGIS_Uid    ;
      constructor Create ( const _ptg   : TGIS_Point ;
                           const _uid   : TGIS_Uid    ;
                           const _uidex : TGIS_Uid
                         ) ;
  end ;

  // Class for list of all found street crossings. }
  T_foundNodeList = class ( TGIS_ObjectList )
    public

      // Add a new found crossing to the list;
      // _ptg    node to be added
      // _uid    uid associated with the first node
      // _uidex  uid associated with the second node
      procedure  Add     ( const _ptg   : TGIS_Point ;
                           const _uid   : TGIS_Uid    ;
                           const _uidex : TGIS_Uid
                         ) ;
  end ;

  T_GeoField = class
    public
      Name   : String ;
      Left   : String ;
      Left2  : String ;
      Right  : String ;
      Right2 : String ;
  end ;

  // Class handles an address uid.
  T_GeoInteger = class
    public
      Value : TGIS_Uid ;
      constructor Create ( const _value : TGIS_Uid
                         ) ;
  end ;

  // Class handles an address point.
  T_GeoPoint = class
    public
      Value : TGIS_Point ;
      constructor Create ( const _value : TGIS_Point
                         ) ;
  end ;


//==============================================================================
// T_node
//==============================================================================

  constructor T_node.Create( const _ptg : TGIS_Point ;
                             const _uid : TGIS_Uid
                           ) ;
  begin
    inherited Create ;
    Ptg := _ptg ;
    Uid := _uid ;
  end ;


//==============================================================================
// T_nodeList
//==============================================================================

  procedure T_nodeList.Add(
    const _ptg : TGIS_Point ;
    const _uid : TGIS_Uid
  ) ;
  var
    i    : Integer ;
    ipos : Integer ;
    node : T_node  ;
  begin
    ipos := -1 ;
    for i:= 0 to Count -1 do begin
      node := T_node( Items[i] ) ;
      if node.Ptg.X < _ptg.X then continue ;
      if ( node.Ptg.X = _ptg.X ) and
         ( node.Ptg.Y < _ptg.Y ) then continue ;
      if ( node.Uid = _uid ) and
         ( node.Ptg.X = _ptg.X ) and
         ( node.Ptg.Y = _ptg.Y ) then begin
        ipos := -2 ;
        break ;
      end else begin
        ipos := i ;
        break ;
      end ;
    end ;

    if ipos > -2 then begin
      node := T_node.Create( _ptg, _uid ) ;

      if ipos < 0 then inherited Add   ( node )
                  else inherited Insert( ipos, node ) ;
    end ;

  end ;

  procedure T_nodeList.Find(
    const _ptg : TGIS_Point ;
    var   list : TStrings
  ) ;
  var
    i    : Integer ;
    node : T_node  ;
  begin
    for i:= 0 to Count -1 do begin
      node := T_node( Items[i] ) ;
      if node.Ptg.X <  _ptg.X then continue ;
      if node.Ptg.X >  _ptg.X then break ;
      if node.Ptg.Y <  _ptg.Y then continue ;
      if node.Ptg.Y =  _ptg.Y then
        list.Add ( IntToStr( node.Uid ) ) ;
    end ;
  end ;


//==============================================================================
// T_foundNode
//==============================================================================

  constructor T_foundNode.Create(
    const _ptg   : TGIS_Point ;
    const _uid   : TGIS_Uid    ;
    const _uidex : TGIS_Uid
  ) ;
  begin
    inherited Create ;
    Ptg   := _ptg   ;
    Uid   := _uid   ;
    UidEx := _uidex ;
  end ;


//==============================================================================
// T_foundNodeList
//==============================================================================

  procedure  T_foundNodeList.Add(
    const _ptg   : TGIS_Point ;
    const _uid   : TGIS_Uid    ;
    const _uidex : TGIS_Uid
  ) ;
  begin
    inherited Add( T_foundNode.Create( _ptg, _uid, _uidex ) ) ;
  end ;


//==============================================================================
// T_GeoInteger
//==============================================================================

  constructor T_GeoInteger.Create(
    const _value : TGIS_Uid
  ) ;
  begin
    inherited Create ;
    Value := _value ;
  end ;


//==============================================================================
// T_GeoPoint
//==============================================================================

  constructor T_GeoPoint.Create(
    const _value : TGIS_Point
  ) ;
  begin
    inherited Create ;
    Value := _value ;
  end ;


//==============================================================================
// TGIS_Geocoding
//==============================================================================

  constructor TGIS_Geocoding.Create(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    Create ( _layer, TGIS_ShapeType.Arc ) ;
  end ;

  constructor TGIS_Geocoding.Create(
    const _layer   : TGIS_LayerVector ;
    const _shpType : TGIS_ShapeType
  ) ;
  begin
    inherited Create ;

    readyList := TStringList.Create ;
    readyList.Sorted := True ;

    layerObj := _layer ;

    layerGeo := TGIS_LayerVector( layerObj.Viewer.Ref.Get( GIS_GEO_LAYERNAME ) ) ;
    if not assigned( layerGeo ) then begin
      layerGeo := TGIS_LayerVector.Create ;
      layerGeo.Name := GIS_GEO_LAYERNAME ;
      layerGeo.Active := False ;
      layerGeo.ImportStructure( layerObj ) ;
      layerGeo.AddField( GIS_GEO_SPLITMASTER , TGIS_FieldType.Number, 9, 0 );
      layerGeo.AddField( GIS_GEO_SPLITTYPE   , TGIS_FieldType.Number, 1, 0 );
      layerGeo.AddField( GIS_GEO_SPLITVALUE  , TGIS_FieldType.Float , 1, 0 );
      layerGeo.CS := layerObj.CS ;
      layerObj.Viewer.Ref.Add( layerGeo ) ;
    end ;

    shpType  := _shpType ;

    matcherObj := TGIS_AddressMatching.Create ;

    FOffset  := 0 ;

    // for old style functionality
    RoadName := GIS_GEO_OLD_ROADNAME ;
    LFrom    := GIS_GEO_OLD_LFROM    ;
    LTo      := GIS_GEO_OLD_LTO      ;
    RFrom    := GIS_GEO_OLD_RFROM    ;
    RTo      := GIS_GEO_OLD_RTO      ;

    // default key names used in address formulas definition
    FCityName     := GIS_GEO_CITYNAME    ;
    FStreetName   := GIS_GEO_STREETNAME  ;
    FHouseNumber  := GIS_GEO_HOUSENUMBER ;
    FUniqueName   := GIS_GEO_UNIQUENAME  ;

    FUid     := TGIS_ObjectList.Create ;
    FUidEx   := TGIS_ObjectList.Create ;
    FPoint   := TGIS_ObjectList.Create ;
    FQuery   := TStringList.Create ;
    FQueryEx := TStringList.Create ;

    FOSMGeocoding := False ;

    fieldList := TGIS_ObjectList.Create ;
    uniqueId  := TStringList.Create ;

    osmGeo := T_osmGeocoding.Create( _layer, _shpType ) ;
  end ;

  procedure TGIS_Geocoding.doDestroy ;
  begin
    clearResult ;
    FreeObject( FUid     ) ;
    FreeObject( FUidEx   ) ;
    FreeObject( FPoint   ) ;
    FreeObject( FQuery   ) ;
    FreeObject( FQueryEx ) ;

    FreeObject( matcherObj ) ;
    FreeObject( readyList  ) ;
    FreeObject( fieldList ) ;
    FreeObject( uniqueId ) ;

    FreeObject( osmGeo ) ;

    inherited ;
  end ;

  //----------------------------------------------------------------------------
  // property access routines
  //----------------------------------------------------------------------------

  function TGIS_Geocoding.fget_Uid(
    _idx : Integer
  ) : TGIS_Uid ;
  begin
    if OSMGeocoding then
      Result := T_osmGeocoding( osmGeo ).fget_Uid( _idx )
    else
      Result := T_GeoInteger( FUid.Items[ _idx ] ).Value ;
  end ;

  function TGIS_Geocoding.fget_UidEx(
    _idx : Integer
  ) : TGIS_Uid ;
  begin
    if OSMGeocoding then
      Result := T_osmGeocoding( osmGeo ).fget_UidEx( _idx )
    else
      Result := T_GeoInteger( FUidEx.Items[ _idx ] ).Value ;
  end ;

  function TGIS_Geocoding.fget_Point(
    _idx : Integer
  ) : TGIS_Point ;
  begin
    if OSMGeocoding then
      Result := T_osmGeocoding( osmGeo ).fget_Point( _idx )
    else
      Result := T_GeoPoint( FPoint.Items[ _idx ] ).Value ;
  end ;

  function TGIS_Geocoding.fget_Query(
    _idx : Integer
  ) : String ;
  begin
    if OSMGeocoding then
      Result := T_osmGeocoding( osmGeo ).fget_Query( _idx )
    else
      Result := FQuery.Strings[ _idx ] ;
  end ;

  function TGIS_Geocoding.fget_QueryEx(
    _idx : Integer
  ) : String ;
  begin
    if OSMGeocoding then
      Result := T_osmGeocoding( osmGeo ).fget_QueryEx( _idx )
    else
      Result := FQueryEx.Strings[ _idx ] ;
  end ;

  procedure TGIS_Geocoding.fset_Formulas(
    const _value : String
  ) ;
  var
   iniFile        : TMemIniFile ;
   iniContent     : TStrings    ;
   sectionStrings : TStrings    ;
  begin
    if IsStringEmpty( _value ) then
      exit ;
    if not assigned( fieldList ) then
      exit ;

    try
      clearFields ;
      iniFile := TMemIniFile.Create( 'first' ) ;
      iniContent := TStringList.Create ;
      try
        iniContent.Text := _value ;
        iniFile.SetStrings( iniContent ) ;
        sectionStrings := TStringList.Create ;
        iniFile.ReadSectionValues( GIS_GEO_FIELDS_SECTION, sectionStrings ) ;
        loadFields( sectionStrings ) ;
        FreeObject( sectionStrings ) ;

        iniFile.GetStrings( iniContent )  ;
        FFormulasString := iniContent.Text ;
        matcherObj.FormulasString := FFormulasString ;

        iniFile.GetStrings( iniContent )  ;
        FFormulasString := iniContent.Text ;

        stdAddress := iniFile.ReadString(
          GIS_GEO_OUTPUT_SECTION,
          GIS_GEO_STDADDRESS_KEY,
          GIS_GEO_STDADDRESS_VAL
        ) ;
        FReverseRestrictQuery := iniFile.ReadString(
          GIS_GEO_REVERSE_SECTION,
          GIS_GEO_RESTRICTQUERY_KEY,
          GIS_GEO_RESTRICTQUERY_VAL
        ) ;

      finally
        FreeObject( iniContent ) ;
        FreeObject( iniFile    ) ;
      end ;
    except
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_FILEREAD ),
              'custom definition', 0
            ) ;
    end ;
    matcherObj.FormulasString := FFormulasString ;
  end ;

  procedure TGIS_Geocoding.clearFields ;
  begin
    fieldList.Clear ;
  end ;

  procedure TGIS_Geocoding.loadFields(
    const _fieldsSection : TStrings
  ) ;
  var
    key,
    value   : String  ;
    sname   : String  ;
    value1,
    value2  : String  ;
    i, j    : Integer ;
    p       : Integer ;
    left,
    right   : Integer ;
    fld     : T_GeoField ;

    procedure setUniqueId( _value : String ) ;
    var
      fld1 : String ;
    begin
      uniqueId.Clear ;
      p := Pos( '+', _value );
      while p > StringFirst - 1 do begin
        fld1 := Copy( _value, StringFirst, p-StringFirst ) ;
        if not IsStringEmpty( fld1 ) then
          uniqueId.Add( fld1 ) ;
        _value := Copy( _value, p+1, StringLast(_value)-p ) ;
        p := Pos( '+', _value ) ;
      end ;
      if not IsStringEmpty( _value ) then
        uniqueId.Add( _value ) ;
    end ;
  begin
    if _fieldsSection.Count = 0 then
      exit ;

    clearFields ;
    for i := 0 to _fieldsSection.Count-1 do begin
      key := _fieldsSection.Names[i] ;
      value := _fieldsSection.Values[key] ;
      if key = FUniqueName then begin
        setUniqueId( value ) ;
        continue ;
      end ;
      left := 0 ;
      right := 0 ;
      p := Pos( ',', key ) ;
      if p > StringFirst - 1 then begin
        // left, right
        sname := Copy( key, StringFirst, p-StringFirst ) ;
        if key[p+1] = 'L' then
          left := 1
        else if key[p+1] = 'R' then
          right := 1 ;
      end else
        sname := key ;
      value1 := '' ;
      value2 := '' ;
      p := Pos( ',', value ) ;
      if p > StringFirst - 1 then begin
        // range
        value1 := Copy( value, StringFirst, p-StringFirst ) ;
        value2 := Copy( value, p+1, StringLast(value)-p ) ;
      end ;
      j := 0 ;
      fld := nil ;
      while j < fieldList.Count do begin
        fld := T_GeoField( fieldList[j] ) ;
        if T_GeoField(fieldList[j]).Name = sname then
          break ;
        inc(j) ;
      end ;
      if j = fieldList.Count then begin
        fld := T_GeoField.Create ;
        fieldList.Add( fld ) ;
        fld.Name := sname ;
      end;
      if IsStringEmpty( value1 ) then begin
        if left = 1 then begin
          fld.Left   := value ;
          fld.Left2  := ''    ;
        end else if right = 1 then begin
          fld.Right  := value ;
          fld.Right2 := ''    ;
        end else begin
          fld.Left   := value ;
          fld.Left2  := ''    ;
          fld.Right  := value ;
          fld.Right2 := ''    ;
        end ;
      end else begin
        if left = 1 then begin
          fld.Left   := value1 ;
          fld.Left2  := value2 ;
        end else if right = 1 then begin
          fld.Right  := value1 ;
          fld.Right2 := value2 ;
        end else begin
          fld.Left   := value1 ;
          fld.Left2  := value2 ;
          fld.Right  := value1 ;
          fld.Right2 := value2 ;
        end ;
      end ;
    end ;
  end ;

  function  TGIS_Geocoding.getEqualHelper(
    const _name   : String
  ) : String ;
  var
    i     : Integer ;
    btest : Boolean ;
  begin
    i := layerObj.FindField( _name ) ;
    if i >= 0 then
      btest := layerObj.FieldInfo( i ).FieldType = TGIS_FieldType.String
    else
      btest := False ;

    if btest then
      Result := '(%s LIKE ''%s'')'
    else
      Result := '(%s = %s)'

  end ;

  procedure TGIS_Geocoding.LoadFormulas(
    const _languageFileName : String        ;
    const _fmtFileName      : String
  ) ;
  begin
    LoadFormulas( _languageFileName, _fmtFileName, '' ) ;
  end;

  procedure TGIS_Geocoding.LoadFormulas(
    const _languageFileName : String ;
    const _fmtFileName      : String ;
    const _customDefinition : String
  ) ;
  var
    iniFile        : TMemIniFile ;
    finalIni       : TMemIniFile ;
    iniContent     : TStrings ;
    sectionStrings : TStrings ;
    tmpFile        : String   ;

    procedure updateIni ;
    var
      sectionStrings1 : TStrings ;
      sectionStrings2 : TStrings ;
      sections  : TStrings ;
      section   : String   ;
      key       : String   ;
      value     : String   ;
      i, j      : Integer  ;
    begin
      sectionStrings1 := TStringList.Create ;
      sectionStrings2 := TStringList.Create ;
      sections := TStringList.Create ;
      iniFile.ReadSections( sections ) ;
      for i := 0 to sections.Count-1 do begin
        section := sections.Strings[i] ;
        iniFile.ReadSectionValues( section, sectionStrings2 ) ;
        if section = GIS_GEO_FIELDS_SECTION then begin
          finalIni.EraseSection( section ) ;
          for j := 0 to sectionStrings2.Count-1 do begin
            key := sectionStrings2.Names[j] ;
            value := sectionStrings2.Values[key] ;
            if not IsStringEmpty( value ) then
              finalIni.WriteString( section, key, value ) ;
          end ;
          loadFields( sectionStrings2 ) ;
        end else if section = GIS_GEO_WORDCHARS_SECTION then begin
          finalIni.EraseSection( section ) ;
          for j := 0 to sectionStrings2.Count-1 do begin
            key := sectionStrings2.Names[j] ;
            value := sectionStrings2.Values[key] ;
            if not IsStringEmpty( value ) then
              finalIni.WriteString( section, key, value ) ;
          end ;
        end else if section = GIS_GEO_OUTPUT_SECTION then begin
          finalIni.EraseSection( section ) ;
          for j := 0 to sectionStrings2.Count-1 do begin
            key := sectionStrings2.Names[j] ;
            value := sectionStrings2.Values[key] ;
            finalIni.WriteString( section, key, value ) ;
          end ;
        end else if section = GIS_GEO_OUTPUT_SECTION then begin
          finalIni.EraseSection( section ) ;
          for j := 0 to sectionStrings2.Count-1 do begin
            key := sectionStrings2.Names[j] ;
            value := sectionStrings2.Values[key] ;
            finalIni.WriteString( section, key, value ) ;
          end ;
        end else begin
          finalIni.ReadSectionValues( section, sectionStrings1 );
          for j := 0 to sectionStrings2.Count-1 do begin
            key := sectionStrings2.Names[j] ;
            value := sectionStrings2.Values[key] ;
            if sectionStrings1.IndexOfName(key) > -1 then
              finalIni.DeleteKey( section, key ) ;
            if not IsStringEmpty( value ) then
              finalIni.WriteString( section, key, value ) ;
          end ;
        end ;
      end ;
      FreeObject( sectionStrings1 ) ;
      FreeObject( sectionStrings2 ) ;
      FreeObject( sections        ) ;
    end ;

  begin
    if not assigned( fieldList ) then
      exit ;

    try
      finalIni := TMemIniFile.Create( 'final' ) ;
      iniFile  := TMemIniFile.Create( 'second' ) ;
      iniContent := TStringList.Create ;
      try

        // language file
        if not IsStringEmpty( _languageFileName ) then begin
          tmpFile := _languageFileName ;
          iniContent.Text := GetCommonFilesItem(
                               GIS_COMMONFILES_GEOCODING,
                               _languageFileName
                             ) ;
          if IsStringEmpty( iniContent.Text ) then Abort ;

          finalIni.SetStrings( iniContent ) ;
          sectionStrings := TStringList.Create ;
          try
            finalIni.ReadSectionValues(
              GIS_GEO_FIELDS_SECTION,
              sectionStrings
            ) ;
            loadFields( sectionStrings ) ;
          finally
            FreeObject( sectionStrings ) ;
          end;
        end ;

        // format file
        if not IsStringEmpty( _fmtFileName ) then begin
          tmpFile := _fmtFileName ;
          iniContent.Text := GetCommonFilesItem(
                               GIS_COMMONFILES_GEOCODING,
                               _fmtFileName
                            ) ;
          if IsStringEmpty( iniContent.Text ) then Abort ;
          iniFile.SetStrings( iniContent ) ;
          updateIni ;
        end ;

        // custom
        if not IsStringEmpty( _customDefinition ) then begin
          tmpFile := 'custom definition' ;
          iniContent.Text := _customDefinition ;
          iniFile.SetStrings( iniContent ) ;
          updateIni ;
        end ;

        iniContent.Clear ;
        finalIni.GetStrings( iniContent ) ;
        FFormulasString := iniContent.Text ;
        stdAddress := finalIni.ReadString(
          GIS_GEO_OUTPUT_SECTION,
          GIS_GEO_STDADDRESS_KEY,
          GIS_GEO_STDADDRESS_VAL
        ) ;
        FReverseRestrictQuery := finalIni.ReadString(
          GIS_GEO_REVERSE_SECTION,
          GIS_GEO_RESTRICTQUERY_KEY,
          GIS_GEO_RESTRICTQUERY_VAL
        ) ;

      finally
        FreeObject( iniContent ) ;
        FreeObject( iniFile    ) ;
        FreeObject( finalIni   ) ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), tmpFile, 0 ) ;
    end ;

    matcherObj.FormulasString := FFormulasString ;

  end ;

  procedure TGIS_Geocoding.clearResult ;
  begin
    FUid.Clear ;
    FUidEx.Clear ;

    FPoint.Clear ;

    FQuery.Clear   ;
    FQueryEx.Clear ;
  end ;

  function TGIS_Geocoding.trimSpace(
    const _txt : String
  ) : String ;
  var
    i     : Integer ;
    state : Integer ;
  begin
    Result := '' ;

    state := 0 ; // first char
    for i:=StringFirst to StringLast( _txt ) do begin
      case state of
        0 : if _txt[i] = ' ' then continue
                             else begin
                                    Result := Result + _txt[i] ;
                                    state := 1 ; // middle of the word
                                  end ;
        1 : if _txt[i] = ' ' then begin
                                    state := 2 ; // space between
                                  end
                             else begin
                                    Result := Result + _txt[i] ;
                                  end ;
        2 : if _txt[i] = ' ' then begin
                                    continue
                                  end
                             else begin
                                    Result := Result + ' ' + _txt[i] ;
                                    state  := 1 ;
                                  end ;
      end ;
    end ;
  end ;

  function TGIS_Geocoding.splitCrossing(
    const _addr   : String ;
    var   _nameA  : String ;
    var   _nameB  : String
  ) : Boolean ;

  var
    i      : Integer ;
    found  : Boolean ;
    tmpA   : String  ;
    tmpB   : String  ;
    tmpNum : Integer ;
    sb     : TStringBuilder ;
  begin
    tmpA := '' ;
    tmpB := '' ;

    found := False ;
    for i := StringFirst to StringLast( _addr ) do begin
      if _addr[i] = '&' then begin
        found := True ;
        continue ;
      end ;
      if not found then tmpA := tmpA + _addr[i]
                   else tmpB := tmpB + _addr[i] ;
    end ;

    sb := TStringBuilder.Create( tmpA ) ;
    try
      for i := 0 to sb.Length - 1 do begin
        {$IFDEF CLR}
          if sb[i] = #160 then sb[i] := ' ' ;
        {$ELSE}
          if sb[i] = #255 then sb[i] := ' ' ;
        {$ENDIF}
      end ;
      tmpA := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;

    sb := TStringBuilder.Create( tmpB ) ;
    try
      for i := 0 to sb.Length - 1 do begin
        {$IFDEF CLR}
          if sb[i] = #160 then sb[i] := ' ' ;
        {$ELSE}
          if sb[i] = #255 then sb[i] := ' ' ;
        {$ENDIF}
      end ;
      tmpB := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;

    splitNumber( trimSpace( tmpA ), _nameA, tmpNum ) ;
    splitNumber( trimSpace( tmpB ), _nameB, tmpNum ) ;

    Result := found ;
  end ;

  function TGIS_Geocoding.splitNumber(
    const _addr   : String ;
    var   _name   : String ;
    var   _number : Integer
  ) : Boolean ;
  var
    i      : Integer ;
    sname  : String  ;
    nmbr   : String  ;
    state  : Integer ;
    c      : Char ;
    sb     : TStringBuilder ;
  begin
    sname := '' ;
    nmbr  := '' ;

    state := 0 ; // first char
    for i := StringFirst to StringLast( _addr ) do begin
      c := _addr[i] ;
      case state of
        0 : if c in ['0'..'9']  then begin
                                       // still in number before name
                                       sname  := sname + String(c) ;
                                     end
                                else begin
                                       // so we are collecting the name
                                       sname  := sname + String(c) ;
                                       state := 1 ;
                                     end ;
        1 : if ( c = ' ' )      then begin
                                       // still name but ready to collect numbers
                                       sname  := sname + String(c) ;
                                       state := 2 ;
                                     end
                                else begin
                                       // still in name
                                       sname := sname + String(c) ;
                                     end ;
        2 : if c in ['0'..'9']  then begin
                                       //start collect the number
                                       nmbr  := nmbr + String(c) ;
                                       state := 3 ;
                                     end
                                else begin
                                       // still in name
                                       sname := sname + String(c) ;
                                     end ;
        3 : if c in ['0'..'9']  then begin
                                       // still in numbers
                                       nmbr := nmbr + String(c) ;
                                     end
                                else begin
                                       // end of number
                                       break ;
                                     end ;
      end ;
    end ;

    _name := trimSpace( sname ) ;
    sb := TStringBuilder.Create( _name ) ;
    try
      for i := 0 to sb.Length - 1 do begin
        {$IFDEF CLR}
          if sb[i] = #160 then sb[i] := ' ' ;
        {$ELSE}
          if sb[i] = #255 then sb[i] := ' ' ;
        {$ENDIF}
      end ;
      _name := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;

    if not IsStringEmpty( nmbr ) then try
                         _number := StrToInt( nmbr ) ;
                       except
                         _number := high(Integer) ;
                         nmbr := '' ;
                       end
                  else _number := high(Integer ) ;

    Result := True ;
  end ;

  function TGIS_Geocoding.findCrossing(
    const _nameA      : String  ;
    const _nameB      : String  ;
    const _firstMatch : Boolean
  ) : Integer ;
  var
    {$IFDEF DCC}
      shp0  : TGIS_Shape   ;
    {$ENDIF}
    shp   : TGIS_ShapeArc  ;
    scope : String         ;
    ptg   : TGIS_Point     ;
    lst   : T_nodeList     ;
    nodeList  : TStrings   ;
    j     : Integer        ;

    procedure add_to_result(
      const _uid   : TGIS_Uid ;
      const _uidex : TGIS_Uid ;
      const _ptg   : TGIS_Point
    ) ;
    var
      i    : Integer    ;
    begin
      if layerObj.GetField( _uid, FRoadName ) =
         layerObj.GetField( _uidex, FRoadName )
      then
        exit ;

      for i:= FPoint.Count -1 downto 0 do
        if GisIsSamePoint( T_GeoPoint( FPoint[i] ).Value, _ptg ) then
          exit ;

      FUid.Add( T_GeoInteger.Create( _uid ) ) ;
      FUidEx.Add( T_GeoInteger.Create( _uidex ) ) ;

      FPoint.Add( T_GeoPoint.Create( _ptg ) ) ;

      FQuery.Add( Format( '%s & %s',
                          [
                            String( layerObj.GetField( _uid  , FRoadName ) ),
                            String( layerObj.GetField( _uidex, FRoadName ) )
                          ]
                         )
                ) ;
      FQueryEx.Add( '' ) ;
    end ;

  begin
    Result := 0 ;

    lst := T_nodeList.Create ;
    try
      if Pos( '+', _nameA ) = StringFirst then
        scope :=  Format(
                    '%s like ''%s''',
                    [ FRoadName, Copy(_nameA,StringFirst+1,1024) ]
                  )
      else
        scope :=  Format(
                    '%s like ''%s%%''',
                    [ FRoadName, _nameA ]
                  ) ;

      for shp0 in layerObj.Loop( GisWholeWorld, scope ) do begin
        if not ( shp0 is TGIS_ShapeArc ) then continue ;
        shp := shp0 as TGIS_ShapeArc ;

        if shp.TagInternal <> 0 then continue ;

        with shp do begin
          ptg := GetPoint( 0, 0 ) ;
          lst.Add( ptg, shp.Uid ) ;

          ptg := GetPoint( GetNumParts-1, GetPartSize(GetNumParts-1)-1 ) ;
          lst.Add( ptg, shp.Uid ) ;
        end ;
      end ;

      if lst.Count = 0 then // nothing yet - nothing later :>
        exit ;

      if Pos( '+', _nameB ) = StringFirst then
        scope :=  Format(
                    '%s like ''%s''',
                    [ FRoadName, Copy(_nameB,StringFirst+1,1024) ]
                  )
      else
        scope :=  Format(
                    '%s like ''%s%%''',
                    [ FRoadName, _nameB ]
                  ) ;

      for shp0 in layerObj.Loop( GisWholeWorld, scope ) do begin
        if shp0 is TGIS_ShapeArc then begin
          shp := shp0 as TGIS_ShapeArc ;

          with shp do begin
            ptg := GetPoint( 0, 0 ) ;
            nodeList := TStringList.Create ;
            lst.Find( ptg, nodeList ) ;
            for j := 0 to nodeList.Count-1 do begin
              add_to_result( StrToInt(nodeList.Strings[j]), shp.Uid, ptg ) ;

              if FQuery.Count >= GIS_GEO_RETURNED_LIMIT then begin
                FreeObject( nodeList ) ;
                exit ;
              end ;
              if _firstMatch then begin
                FreeObject( nodeList ) ;
                exit ;
              end ;
            end ;

            ptg := GetPoint( GetNumParts-1, GetPartSize(GetNumParts-1)-1 ) ;
            nodeList.Clear ;
            lst.Find( ptg, nodeList ) ;
            for j := 0 to nodeList.Count-1 do begin
              add_to_result( StrToInt(nodeList.Strings[j]), shp.Uid, ptg ) ;

              if FQuery.Count >= GIS_GEO_RETURNED_LIMIT then begin
                FreeObject( nodeList ) ;
                exit ;
              end ;
              if _firstMatch then begin
                FreeObject( nodeList ) ;
                exit ;
              end ;
            end ;
            FreeObject( nodeList ) ;
          end ;
        end ;
      end ;
    finally
      FreeObject( lst ) ;
      Result := FQuery.Count ;
    end ;
  end ;

  function TGIS_Geocoding.findNumber(
    const _name       : String  ;
    const _number     : Integer ;
    const _firstMatch : Boolean ;
    const _addToLayer : Boolean
  ) : Integer ;
  var
    {$IFDEF DCC}
      shp0    : TGIS_Shape     ;
    {$ENDIF}
    shp       : TGIS_ShapeArc  ;
    scope     : String         ;
    bestshp   : TGIS_Shape     ;
    bestuid   : TGIS_Uid       ;
    bestpos   : Double         ;
    bestside  : Integer        ;
    nlfrom    : Integer        ;
    nlto      : Integer        ;
    nrfrom    : Integer        ;
    nrto      : Integer        ;
    inleft    : Boolean        ;
    inright   : Boolean        ;
    matchside : Boolean        ;
    sname     : String         ;
    found     : Boolean        ;
    res       : Integer        ;

    procedure add_to_result    ;
    var
      ptg       : TGIS_Point    ;
      dist      : Double        ;
      shplen    : Double        ;
      off       : Double        ;
      partsize  : Integer       ;
      newshp1   : TGIS_Shape    ;
      newshp2   : TGIS_Shape    ;
      newshp3   : TGIS_Shape    ;
      point_no  : Integer       ;
      point_tmp : Integer       ;
      len       : Double        ;
      pt_a      : TGIS_Point    ;
      pt_b      : TGIS_Point    ;
      stmp      : String        ;
      idx       : Integer       ;
      perc      : Double        ;
    begin
      shp := TGIS_ShapeArc( bestshp ) ;
      if not assigned( shp) then
        exit ;

      found := True ;
      inc( res ) ;

      shp.Lock( TGIS_Lock.Projection ) ;
      try
        shplen := shp.Length ;

        dist := bestpos ;

        off := 1.5*FOffset ;

        if      off = 0                           then
                dist := shplen / 2
        else if (shplen < 2*off) or (dist < 0)     then
                dist := shplen / 2
        else    begin
                  if      dist*shplen < off        then
                          dist := off
                  else if dist*shplen > shplen-off then
                          dist := shplen-off
                  else    dist := dist*shplen ;
                end ;

        case bestside of
            1   : ptg := shp.GetPointOnLine( dist,  FOffset ) ;
           -1   : ptg := shp.GetPointOnLine( dist, -FOffset ) ;
           else   ptg := shp.GetPointOnLine( dist, 0        ) ;
        end  ;
      finally
        shp.Unlock ;
      end ;

      FUid.Add( T_GeoInteger.Create( bestshp.Uid ) ) ;

      if bestside = 0 then FUidEx.Add( T_GeoInteger.Create( -2 ) )
                      else FUidEx.Add( T_GeoInteger.Create( -1 ) ) ;

      FPoint.Add( T_GeoPoint.Create( ptg ) ) ;

      if bestside <> 0 then
        stmp := Format( '%s %d',
                        [ String( shp.GetField( FRoadName ) ), _number ]
                      )
      else
        stmp := Format( '%s',
                        [ String( shp.GetField( FRoadName ) ) ]
                      ) ;

      FQuery.Add( stmp ) ;
      FQueryEx.Add( '' ) ;

      if not _addToLayer then
        exit ;

      // protect against multiple adding same object
      stmp := UpperCase( stmp ) + ' UID:' + IntToStr( shp.Uid ) ;
      if readyList.Find( stmp, idx ) then
        exit ;
      readyList.Add( stmp ) ;

      // add temporary shapes
      if shp.GetNumParts < 0 then
        exit ;
      partsize := shp.GetPartSize( 0 ) ;
      if partsize < 0 then
        exit ;

      shp.Lock( TGIS_Lock.Projection );
      try
        if Abs( shplen ) < 1E-7       then perc := 0
                                      else perc := dist / shplen ;

        if   perc > 1                 then perc := 1
        else if Abs( perc -1 ) < 1E-7 then perc := 1 ;
        if   perc < 0                 then perc := 0
        else if Abs( perc    ) < 1E-7 then perc := 0 ;

        if ( perc > 0 ) and ( perc < 1 ) then begin
          // creates first subsegment from the shape
          newshp1 := layerGeo.AddShape( shp, True ) ;
          newshp1.Lock( TGIS_Lock.Projection ) ;
          try
            newshp1.SetField( GIS_GEO_SPLITMASTER , shp.Uid  ) ;
            newshp1.SetField( GIS_GEO_SPLITTYPE   , 1        ) ; // left
            newshp1.SetField( GIS_GEO_SPLITVALUE  , perc     ) ;

            newshp1.TagInternal := 1 ;
            newshp1.Reset ;
            newshp1.AddPart ;

            len := 0 ;

            pt_a := shp.GetPoint( 0, 0 ) ;
            newshp1.AddPoint( pt_a ) ;

            point_tmp := 0 ;
            for point_no := 1 to partsize -1 do  begin // all points
              pt_b := shp.GetPoint(0,point_no) ;
              len := len + GisPoint2Point( pt_a, pt_b ) ;
              if len < dist then begin
                newshp1.AddPoint( pt_b ) ;
                pt_a := pt_b ;
              end
              else begin
                point_tmp := point_no ;
                break ;
              end ;
            end ;
            newshp1.AddPoint( shp.GetPointOnLine( dist,  0 ) );
          finally
            newshp1.Unlock ;
          end ;

          // creates second subsegment from the shape
          newshp2 := layerGeo.AddShape( shp, True ) ;
          newshp2.Lock( TGIS_Lock.Projection ) ;
          try
            newshp2.SetField( GIS_GEO_SPLITMASTER , shp.Uid  ) ;
            newshp2.SetField( GIS_GEO_SPLITTYPE   , 2        ) ; // right
            newshp2.SetField( GIS_GEO_SPLITVALUE  , perc     ) ;

            newshp2.TagInternal := 1 ;
            newshp2.Reset ;
            newshp2.AddPart ;

            newshp2.AddPoint( shp.GetPointOnLine( dist,  0 ) );

            for point_no := point_tmp to partsize -1 do  begin // all points
              pt_b := shp.GetPoint(0,point_no) ;
              newshp2.AddPoint( pt_b ) ;
            end ;
          finally
            newshp2.Unlock ;
          end ;

        end;

      finally
        shp.Unlock;
      end ;

      if ( bestside = 1 ) or ( bestside = -1 ) then begin
        shp.Lock( TGIS_Lock.Projection );
        try
          // final connector from road to shape
          newshp3 := layerGeo.AddShape( shp ) ;
          newshp3.Lock( TGIS_Lock.Projection );
          try
            newshp3.SetField( GIS_GEO_SPLITMASTER , shp.Uid  ) ;
            newshp3.SetField( GIS_GEO_SPLITTYPE   , 3        ) ; // connector
            newshp3.SetField( GIS_GEO_SPLITVALUE  , perc     ) ;

            newshp3.TagInternal := 2 ;
            newshp3.Reset ;
            newshp3.AddPart ;

            newshp3.AddPoint( shp.GetPointOnLine( dist,  0 ) ) ;
            newshp3.AddPoint( ptg ) ;
          finally
            newshp3.Unlock;
          end ;
        finally
          shp.Unlock ;
        end ;
      end ;
    end ;

  begin
    res := 0 ;

    bestshp  := nil ;
    bestuid  := - 1 ;
    bestpos  := - 1 ;
    bestside := 0   ;
    found := False  ;

    shp := nil ;

    if _number <> high(Integer) then begin
      if Pos( '+', _name ) = StringFirst then begin
        scope :=  '(' +FRoadName+ ' like ''%s'') and ' ;
        sname := Copy( _name, StringFirst+1, 1024 ) ;
      end
      else begin
        scope :=  '(' +FRoadName+ ' like ''%s%%'') and ' ;
        sname := _name ;
      end ;

      scope :=  scope                                                    +
                '( ( (' +LFrom+ ' <= %d) and (' +LTo  + ' >= %d ) ) or ' +
                '  ( (' +LTo  + ' <= %d) and (' +LFrom+ ' >= %d ) ) or ' +
                '  ( (' +RFrom+ ' <= %d) and (' +RTo  + ' >= %d ) ) or ' +
                '  ( (' +RTo  + ' <= %d) and (' +RFrom+ ' >= %d ) ) '    +
                ')'                                         ;
      scope := Format( scope, [ sname,
                                _number, _number,
                                _number, _number,
                                _number, _number,
                                _number, _number
                              ]
                      ) ;

      res := 0 ;
      for shp0 in layerObj.Loop( GisWholeWorld, scope ) do begin
        if shp0 is TGIS_ShapeArc then begin
          shp := shp0 as TGIS_ShapeArc ;
          if shp.Length > 0 then begin

            try
              nlfrom := StrToInt( VarToString( shp.GetField( LFrom ) ) ) ;
            except
              nlfrom := 0 ;
            end ;

            try
              nlto   := StrToInt( VarToString( shp.GetField( LTo ) ) ) ;
            except
              nlto   := 0 ;
            end ;

            try
              nrfrom := StrToInt( VarToString( shp.GetField( RFrom ) ) ) ;
            except
              nrfrom := 0 ;
            end ;

            try
              nrto   := StrToInt( VarToString( shp.GetField( RTo ) ) ) ;
            except
              nrto   := 0 ;
            end ;

            bestshp := shp ;
            bestuid := shp.Uid ;

            inleft  := ( ( nlfrom <= _number ) and ( _number <= nlto   ) ) or
                       ( ( nlto   <= _number ) and ( _number <= nlfrom ) ) ;

            // same side of street - bit tricky we are checking
            // for odd/even matching
            matchside := ( ( nlfrom and 1) - (_number and 1) ) =
                         ( (_number and 1) - ( nlto   and 1) ) ;

            if inleft and matchside then begin
              bestside := -1 ;
              if nlto <> nlfrom
                then bestpos := ( 1.0*( _number-nlfrom ) )/( nlto-nlfrom )
                else bestpos := 0.5 ;
              add_to_result ;
              Result := res ;
              if res >= GIS_GEO_RETURNED_LIMIT then
                exit ;
              if _firstMatch then
                exit ;
            end ;

            inright := ( ( nrfrom <= _number ) and ( _number <= nrto   ) ) or
                       ( ( nrto   <= _number ) and ( _number <= nrfrom ) ) ;

            // same side of street - bit tricky we are checking
            // for odd/even matching
            matchside := ( (nrfrom  and 1) - (_number and 1) ) =
                         ( (_number and 1) - (nrto    and 1) ) ;

            if inright and matchside then begin
              bestside := 1 ;
              if nrto <> nrfrom
                then bestpos := ( 1.0*( _number-nrfrom ) )/( nrto-nrfrom )
                else bestpos := 0.5 ;

              add_to_result ;
              Result := res ;
              if _firstMatch then
                exit ;
            end ;

            // not on left/right, maybe not this segment, but we will store
            // our position
            if inleft then begin
              bestside := -1 ;
              if nlto <> nlfrom
                then bestpos := ( 1.0*( _number-nlfrom ) )/( nlto-nlfrom )
                else bestpos := 0.5 ;
            end
            else begin // not in left zone, so it MUST be in right zone
              bestside := 1 ;
              if nrto <> nrfrom
                then bestpos := ( 1.0*( _number-nrfrom ) )/( nrto-nrfrom )
                else bestpos := 0.5 ;
            end ;

          end ;

        end ;
      end ;

      // maybe we missed something ? (street address is not an odd/even, etc
        if not found then begin
          if bestpos >=0 then begin
            // restore bestshp shape
            bestshp := layerObj.GetShape( bestuid ) ;
            add_to_result ;
            Result := res ;
            if res >= GIS_GEO_RETURNED_LIMIT then
              exit ;
            if _firstMatch then
              exit ;
          end ;
        end ;
    end ;

    if _number = high( _number ) then begin
      // nothing found - just find any street segment
      if Pos('+', _name) = StringFirst then begin
        scope :=  '(' +FRoadName+ ' like ''%s'') ' ;
        sname := Copy( _name, StringFirst+1, 1024 ) ;
      end
      else begin
        scope :=  '(' +FRoadName+ ' like ''%s%%'') ' ;
        sname := _name ;
      end ;
      scope := Format( scope, [ sname ] ) ;

      for shp0 in layerObj.Loop( GisWholeWorld, scope ) do begin
        if shp0 is TGIS_ShapeArc then begin
          shp := shp0 as TGIS_ShapeArc ;
          if shp.Length > 0 then begin
            bestshp  := shp ;
            bestpos  := 0.5 ;
            bestside := 0   ;
            add_to_result ;
            Result := res ;
            if res >= GIS_GEO_RETURNED_LIMIT then
              exit ;
            if _firstMatch then
              exit ;
          end ;
        end ;
      end ;
    end ;
    Result := res ;
  end ;

  function TGIS_Geocoding.splitCrossingEx(
    const _addr   : String ;
    var   _nameA  : String ;
    var   _nameB  : String
  ) : Boolean ;
  var
    i      : Integer ;
    found  : Boolean ;
    tmpA   : String  ;
    tmpB   : String  ;
  begin
    tmpA := '' ;
    tmpB := '' ;

    found := False ;
    for i := StringFirst to StringLast( _addr ) do begin
      if _addr[i] = '&' then begin
        found := True ;
        continue ;
      end ;
      if not found then tmpA := tmpA + _addr[i]
                   else tmpB := tmpB + _addr[i] ;
    end ;
    _nameA := tmpA;
    _nameB := tmpB;

    Result := found ;
  end ;

  procedure TGIS_Geocoding.prepareForCrossing(
    var   _resolvedAddresses  : TGIS_ObjectList ;
    var   _resolvedAddresses2 : TGIS_ObjectList
  ) ;
  var
    i       : Integer  ;
    strings : TStrings ;
  begin
    if IsStringEmpty( FStreetName ) then begin
      _resolvedAddresses.Clear  ;
      _resolvedAddresses2.Clear ;
      exit ;
    end ;

    i := 0 ;
    while i < _resolvedAddresses.Count do begin
      strings := TStrings( _resolvedAddresses[i] ) ;
      if IsStringEmpty( strings.Values[FStreetName] ) then begin
        // ignore matches with empty street name
        _resolvedAddresses.Delete(i) ;
      end else
        inc(i) ;
    end ;
    if _resolvedAddresses.Count > 0 then begin
      i := 0 ;
      while i < _resolvedAddresses2.Count do begin
        strings := TStrings( _resolvedAddresses2[i] ) ;
        if IsStringEmpty( strings.Values[FStreetName] ) then begin
          // ignore matches with empty street name
          _resolvedAddresses2.Delete(i) ;
        end else
          inc(i) ;
      end ;
    end ;
  end ;

  procedure TGIS_Geocoding.sortResolvedAddresses(
    const _resolvedAddresses : TGIS_ObjectList
  ) ;
  var
    i, j,
    k       : Integer  ;
    ipos    : Integer  ;
    strings : TStrings ;
    sname   : String   ;
    tmp     : String   ;
  begin
    for i := 0 to _resolvedAddresses.Count-1 do begin
      strings := TStrings( _resolvedAddresses[i] ) ;
      ipos := 0 ;
      for j := 0 to fieldList.Count-1 do begin
        sname := T_GeoField(fieldList[j]).Name ;
        k := strings.IndexOfName(sname) ;
        if k >= 0 then begin
          if k <> ipos then begin
            tmp := strings.Strings[k] ;
            strings.Delete(k) ;
            strings.Insert(ipos, tmp) ;
          end ;
          inc(ipos) ;
          if ipos = strings.Count then break ;
        end ;
      end ;
    end ;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_Geocoding.get_output_txt(
      const _uid      : TGIS_Uid
    ) : String ;
    begin
      Result := get_output_txt( _uid, 0, 0 ) ;
    end ;

    function TGIS_Geocoding.get_output_txt(
      const _uid      : TGIS_Uid     ;
      const _bestside : Integer     ;
      const _number   : Integer
    ) : String ;
  {$ELSE}
    function TGIS_Geocoding.get_output_txt(
      const _uid      : TGIS_Uid     ;
      const _bestside : Integer = 0 ;
      const _number   : Integer = 0
    ) : String ;
  {$ENDIF}
  var
    i      : Integer ;
    fldstr : String  ;
    valstr : String  ;
    addspace  : Boolean ;
    addhyphen : Boolean ;

    function get_field_name(
      const _name : String
    ) : String ;
    var
      i1  : Integer ;
      fld : T_GeoField ;
    begin
      Result := '' ;
      for i1 := 0 to fieldList.Count - 1 do begin
        fld := T_GeoField(fieldList[i1]) ;
        if fld.Name = _name then begin
          if layerObj.GetShape( _uid ) is TGIS_ShapeArc then begin
            if _bestside = -1 then
              Result := fld.Left
            else if _bestside = 1 then
              Result := fld.Right
            else
              Result := fld.Right ;
          end else
            Result := fld.Right ;
          break ;
        end ;
      end ;
    end ;

  begin
    Result := '' ;
    i := StringFirst ;
    addspace  := False ;
    addhyphen := False ;
    while i <= StringLast( stdAddress ) do begin
      case stdAddress[i] of
        '<' :
        begin
          inc(i) ;
          fldstr := '' ;
          while ( i <= StringLast( stdAddress ) ) do begin
            if stdAddress[i] <> '>'  then
              fldstr := fldstr + stdAddress[i]
            else
              break ;
            inc(i) ;
          end;
          if fldstr = ',' then begin
            if ( not IsStringEmpty( Result ) ) and
               ( Result[StringLast(Result)] <> ',' ) then
            begin
              Result := Result + fldstr ;
            end ;
            addspace := False ;
            addhyphen := False ;
          end else if fldstr = ' ' then begin
            if not IsStringEmpty( Result ) then begin
              addspace := True ;
            end else begin
              addspace := False ;
            end ;
            addhyphen := False ;
          end else if fldstr = '-' then begin
            if not IsStringEmpty( Result ) then begin
              addhyphen := True ;
            end else begin
              addhyphen := False ;
            end ;
          end else if ( fldstr = GIS_GEO_HOUSENUMBER ) and
                      ( layerObj.GetShape( _uid ) is TGIS_ShapeArc ) then begin
            if _number = 0 then
              valstr := ''
            else
              valstr := IntToStr ( _number ) ;
            if not IsStringEmpty( valstr ) then begin
              if addspace then
                Result := Result + ' ' ;
              if addhyphen then
                Result := Result + '-' ;
              Result := Result + valstr ;
            end ;
            addspace := False ;
            addhyphen := False ;
          end else begin
            valstr := VarToString( layerObj.GetField(
                        _uid,
                        get_field_name(fldstr) )
                      ) ;
            if not IsStringEmpty( valstr ) then begin
              if addspace then
                Result := Result + ' ' ;
              if addhyphen then
                Result := Result + '-' ;
              Result := Result + valstr ;
            end;
            addspace := False ;
            addhyphen := False ;
          end;
        end ;
        '\' :
        begin
          inc(i) ;
          if i <= StringLast(stdAddress) then begin
            if addspace then
              Result := Result + ' ' ;
            Result := Result + stdAddress[i] ;
            addspace := False ;
            addhyphen := False ;
          end ;
        end
        else
        begin
          if addspace then
            Result := Result + ' ' ;
          Result := Result + stdAddress[i] ;
          addspace := False ;
          addhyphen := False ;
        end;
      end;
      inc(i) ;
    end ;
  end ;

  function TGIS_Geocoding.findCrossingEx(
    const _resolvedAddresses  : TGIS_ObjectList ;
    const _resolvedAddresses2 : TGIS_ObjectList ;
    const _firstMatch         : Boolean     ;
    const _extendedScope      : Boolean
  ) : Integer ;
  var
    shp       : TGIS_ShapeArc ;
    lscope    : String        ;
    noleft    : Integer       ;
    rscope    : String        ;
    noright   : Integer       ;
    ptg       : TGIS_Point    ;
    lst       : T_nodeList    ;
    i, j, k   : Integer       ;
    strings   : TStrings      ;
    nodeList  : TStrings      ;
    value     : String        ;
    fld       : T_GeoField    ;
    crossings : T_foundNodeList ;

    procedure add_to_result(
      const _uid   : TGIS_Uid ;
      const _uidex : TGIS_Uid ;
      const _ptg   : TGIS_Point
    ) ;
    var
      i1     : Integer ;
      stmp   : String  ;
      strtmp : String  ;
    begin

      for i1 := 0 to uniqueId.Count-1 do begin
        if layerObj.GetField( _uid, uniqueId.Strings[i1] ) <>
           layerObj.GetField( _uidex, uniqueId.Strings[i1] ) then
          break ;
        if i1 = uniqueId.Count-1 then
          exit ;
      end ;

      for i1 := FUid.Count -1 downto 0 do
        // eliminate duplicates
        if ( T_GeoInteger( FUid[i1]   ).Value = _uid   ) and
           ( T_GeoInteger( FUidEx[i1] ).Value = _uidex ) and
           GisIsSamePoint( T_GeoPoint( FPoint[i1] ).Value , _ptg )
        then
          exit ;

      FUid.Add( T_GeoInteger.Create( _uid ) ) ;
      FUidEx.Add( T_GeoInteger.Create( _uidex ) ) ;

      FPoint.Add( T_GeoPoint.Create( _ptg ) ) ;

      FQuery.Add( get_output_txt( _uid ) + ' & ' + get_output_txt( _uidex ) ) ;

      stmp := '';
      for i1 := 0 to fieldList.Count-1 do begin
        fld := T_GeoField( fieldList[i1] ) ;
        if fld.Name = FHouseNumber then begin
          stmp := stmp + fld.Name + ',L=' + ';' ;
          stmp := stmp + fld.Name + ',R=' + ';' ;
        end else begin
          strtmp := VarToString( layerObj.GetField( _uid, fld.Left ) ) ;
          if fld.Left = fld.Right then
            stmp := stmp + fld.Name + '=' + strtmp + ';'
          else begin
            stmp := stmp + fld.Name + ',L=' + strtmp + ';' ;
            strtmp := VarToString( layerObj.GetField( _uid, fld.Right ) ) ;
            stmp := stmp + fld.Name + ',R=' + strtmp + ';' ;
          end ;
        end ;
      end ;
      stmp := stmp + '&;' ;
      for i1 := 0 to fieldList.Count-1 do begin
        fld := T_GeoField( fieldList[i1] ) ;
        if fld.Name = FHouseNumber then begin
          stmp := stmp + fld.Name + ',L=' + ';' ;
          stmp := stmp + fld.Name + ',R=' + ';' ;
        end else begin
          strtmp := VarToString( layerObj.GetField( _uidex, fld.Left ) ) ;
          if fld.Left = fld.Right then
            stmp := stmp + fld.Name + '=' + strtmp + ';'
          else begin
            stmp := stmp + fld.Name + ',L=' + strtmp + ';' ;
            strtmp := VarToString( layerObj.GetField( _uidex, fld.Right ) ) ;
            stmp := stmp + fld.Name + ',R=' + strtmp + ';' ;
          end ;
        end ;
      end ;
      FQueryEx.Add( stmp ) ;

    end ;

    procedure get_scope(
      var   _scope : String ;
      const _name  : String
    ) ;
    var
      fmt : String  ;
      p   : Integer ;
    begin
      if ( fld.Name = FStreetName ) or
         ( fld.Name = FCityName   ) then
      begin
        if ( value[StringFirst] = '*' ) and ( value[StringLast(value)] = '*' )
        then
          fmt := Format(
                   '(%s like ''%%%s%%'')',
                   [ _name, Copy ( value, StringFirst+1, length(value)-2 ) ]
                 )
        else if value[StringFirst] = '*' then
          fmt := Format(
                   '(%s like ''%%%s'')',
                   [ _name, Copy ( value, StringFirst+1, length(value)-1 ) ]
                 )
        else if value[StringLast(value)] = '*' then
          fmt := Format(
                   '(%s like ''%s%%'')',
                   [ _name, Copy ( value, StringFirst, length(value)-1 ) ]
                 )
        else if _extendedScope = True then
          fmt := Format(
                   '(%s like ''%s%%'')',
                   [ _name, value ]
                 )
        else
          fmt := Format(
                   getEqualHelper( _name ),
                   [ _name, value ]
                 ) ;

        if IsStringEmpty( _scope ) then
          _scope := fmt
        else
          _scope := _scope + ' and ' + fmt ;
      end else if fld.Name = FHouseNumber then
        exit
      else begin
        //any other one
        p := Pos ( '|', value ) ;
        if p > StringFirst-1 then begin
          fmt := '' ;
          repeat
            if IsStringEmpty( fmt ) then
              fmt := '('
            else
              fmt := fmt + ' or ' ;
            fmt := fmt + Format(
                           getEqualHelper( _name ),
                           [ _name, Copy(value,StringFirst,p-StringFirst) ]
                         ) ;
            value := Copy( value, p+1, StringLast(value)-p ) ;
            p := Pos( '|', value ) ;
          until p = StringFirst-1 ;
          fmt := fmt + ' or ' + Format(
                                  getEqualHelper( _name ),
                                  [ _name, value ]
                                ) ;
          fmt := fmt + ')' ;
        end else
          fmt := Format(
                   getEqualHelper( _name ),
                   [ _name, value ]
                 ) ;
        if IsStringEmpty( _scope ) then
          _scope := fmt
        else
          _scope := _scope + ' and ' + fmt ;
      end ;
    end ;

    procedure find_results(
      const _scope : String
    ) ;
    {$IFDEF DCC}
      var
        shp0 : TGIS_Shape ;
    {$ENDIF}
    begin
      for shp0 in layerObj.Loop( GisWholeWorld, _scope ) do begin
        if not ( shp0 is TGIS_ShapeArc ) then continue ;
        shp := shp0 as TGIS_ShapeArc ;

        if shp.TagInternal <> 0 then continue ;

        with shp do begin
          ptg := GetPoint( 0, 0 ) ;
          lst.Add( ptg, shp.Uid ) ;

          ptg := GetPoint( GetNumParts-1, GetPartSize(GetNumParts-1)-1 ) ;
          lst.Add( ptg, shp.Uid ) ;
        end ;
      end;
    end ;

    procedure find_final_results(
      const _scope : String
    ) ;
    var
      {$IFDEF DCC}
        shp0 : TGIS_Shape ;
      {$ENDIF}
      j1   : Integer     ;
      node : T_foundNode ;
    begin
      for shp0 in layerObj.Loop( GisWholeWorld, _scope ) do begin
        if shp0 is TGIS_ShapeArc then begin
          shp := shp0 as TGIS_ShapeArc ;

          with shp do begin
            ptg := GetPoint( 0, 0 ) ;
            nodeList := TStringList.Create ;
            lst.Find( ptg, nodeList ) ;
            for j1 := 0 to nodeList.Count-1 do begin
              crossings.Add( ptg, StrToInt(nodeList.Strings[j1]), shp.Uid ) ;
              if FQuery.Count >= GIS_GEO_RETURNED_LIMIT then
                break ;
              if _firstMatch then
                break ;
            end ;

            ptg := GetPoint( GetNumParts-1, GetPartSize(GetNumParts-1)-1 ) ;
            nodeList.Clear ;
            lst.Find( ptg, nodeList ) ;
            for j1 := 0 to nodeList.Count-1 do begin
              crossings.Add( ptg, StrToInt(nodeList.Strings[j1]), shp.Uid ) ;
              if FQuery.Count >= GIS_GEO_RETURNED_LIMIT then
                break ;
              if _firstMatch then
                break ;
            end ;
            FreeObject( nodeList ) ;
            if FQuery.Count >= GIS_GEO_RETURNED_LIMIT then
              break ;
            if ( crossings.Count > 0 ) and _firstMatch then
              break ;
          end ;
        end ;
      end ;

      for j1 := 0 to crossings.Count -1 do begin
        node := T_foundNode( crossings.Items[j1] ) ;
        add_to_result( node.Uid, node.UidEx, node.Ptg ) ;
        if _firstMatch then
          break ;
      end ;

    end ;

  begin
    Result := 0;

    if (_resolvedAddresses.Count = 0) or (_resolvedAddresses2.Count = 0) then
      exit;

    sortResolvedAddresses( _resolvedAddresses  ) ;
    sortResolvedAddresses( _resolvedAddresses2 ) ;

    lst := T_nodeList.Create ;
    crossings := T_foundNodeList.Create ;
    try
      for i := 0 to _resolvedAddresses.Count - 1 do begin

        strings := TStrings( _resolvedAddresses[i] ) ;

        lscope  := '' ;
        noleft  := 0  ;
        rscope  := '' ;
        noright := 0  ;
        fld := nil  ;

        for j := 0 to strings.Count-1 do begin
          for k := 0 to fieldList.Count-1 do begin
            fld := T_GeoField( fieldList[k] ) ;
            if T_GeoField(fieldList[k]).Name = strings.Names[j] then begin
              fld := T_GeoField( fieldList[k] ) ;
              break ;
            end ;
          end ;
          if not assigned( fld ) then continue ;

          value := strings.Values[fld.Name] ;
          if ( noleft = 0 )  and ( not IsStringEmpty( fld.Left ) )  then
            get_scope( lscope, fld.Left )
          else
            noleft := 1 ;
          if ( noright = 0 ) and ( not IsStringEmpty( fld.Right ) ) then
            get_scope( rscope, fld.Right )
          else
            noright := 1 ;
        end ;

        if noleft = 0 then begin
          find_results( lscope ) ;
          if lscope = rscope then
            //everything done
            noright := 1 ;
        end ;
        if noright = 0 then begin
          find_results ( rscope ) ;
        end ;
      end ;

      if lst.Count = 0 then // nothing yet - nothing later :>
        exit ;

      for i := 0 to _resolvedAddresses2.Count-1 do begin

        strings := TStrings( _resolvedAddresses2[i] ) ;

        lscope  := '' ;
        noleft  := 0  ;
        rscope  := '' ;
        noright := 0  ;
        fld := nil  ;

        for j := 0 to strings.Count-1 do begin
          for k := 0 to fieldList.Count-1 do begin
            if T_GeoField(fieldList[k]).Name = strings.Names[j] then begin
              fld := T_GeoField( fieldList[k] ) ;
              break ;
            end ;
          end ;
          if not assigned( fld ) then continue ;

          value := strings.Values[fld.Name] ;
          if ( noleft = 0 )  and ( not IsStringEmpty( fld.Left ) )  then
            get_scope( lscope, fld.Left )
          else
            noleft := 1 ;
          if ( noright = 0 ) and ( not IsStringEmpty( fld.Right ) ) then
            get_scope( rscope, fld.Right )
          else
            noright := 1 ;
        end;

        if noleft = 0 then begin
          find_final_results( lscope ) ;
          if FQuery.Count >= GIS_GEO_RETURNED_LIMIT then
            exit ;
          if _firstMatch and ( FQuery.Count > 0 ) then
            exit ;
          if lscope = rscope then
            //everything done
            noright := 1 ;
        end ;
        if noright = 0 then begin
          find_final_results( rscope ) ;
          if FQuery.Count >= GIS_GEO_RETURNED_LIMIT then
            exit ;
          if _firstMatch and ( FQuery.Count > 0 ) then
            exit ;
        end ;
      end ;
    finally
      FreeObject( crossings ) ;
      FreeObject( lst       ) ;
      Result := FQuery.Count ;
    end ;
  end ;

  function TGIS_Geocoding.findAddressEx(
    const _resolvedAddresses : TGIS_ObjectList ;
    const _firstMatch        : Boolean     ;
    const _extendedScope     : Boolean     ;
    const _addToLayer        : Boolean
  ) : Integer ;
  var
    i, j, k   : Integer    ;
    strings   : TStrings   ;
    value     : String     ;
    number    : Integer    ;
    shp       : TGIS_Shape ;
    lscope    : String     ;
    noleft    : Integer    ;
    rscope    : String     ;
    noright   : Integer    ;
    bestshp   : TGIS_Shape ;
    bestpos   : Double     ;
    bestside  : Integer    ;
    addrange  : Integer    ;
    doOnAddress : Integer  ;
    nlfrom    : Integer    ;
    nlto      : Integer    ;
    nrfrom    : Integer    ;
    nrto      : Integer    ;
    matchside : Boolean    ;
    hnmatch   : String     ;
    hnlfield  : String     ;
    hnrfield  : String     ;
    fld       : T_GeoField ;
    found     : Boolean    ;
    res       : Integer    ;

    procedure add_arc_to_result ;
    var
      ptg       : TGIS_Point ;
      ipos      : Double     ;
      shplen    : Double     ;
      off       : Double     ;
      partsize  : Integer    ;
      newshp1   : TGIS_Shape ;
      newshp2   : TGIS_Shape ;
      newshp3   : TGIS_Shape ;
      point_no  : Integer    ;
      point_tmp : Integer    ;
      len       : Double     ;
      pt_a      : TGIS_Point ;
      pt_b      : TGIS_Point ;
      stmp      : String     ;
      strtmp    : String     ;
      idx       : Integer    ;
      i1        : Integer    ;
      perc      : Double     ;
    begin
      shp := bestshp ;
      if not assigned( shp) then
        exit ;

      shp.Lock( TGIS_Lock.Projection ) ;
      try
        shplen := shp.Length ;

        ipos := bestpos ;

        off := 1.5*FOffset ;

        if      off = 0                           then
                ipos := shplen / 2
        else if (shplen < 2*off) or (ipos < 0)     then
                ipos := shplen / 2
        else    begin
                  if      ipos*shplen < off        then
                          ipos := off
                  else if ipos*shplen > shplen-off then
                          ipos := shplen-off
                  else    ipos := ipos*shplen ;
                end ;

        case bestside of
            1   : ptg := TGIS_ShapeArc(shp).GetPointOnLine( ipos,  FOffset ) ;
           -1   : ptg := TGIS_ShapeArc(shp).GetPointOnLine( ipos, -FOffset ) ;
           else   ptg := TGIS_ShapeArc(shp).GetPointOnLine( ipos, 0        ) ;
        end  ;
      finally
        shp.Unlock ;
      end ;

      for i1 := 0 to FUid.Count-1 do
        if  T_GeoInteger( FUid[i1] ).Value = bestshp.Uid then
          exit ;

      found  := True ;
      inc( res ) ;

      FUid.Add( T_GeoInteger.Create( bestshp.Uid ) ) ;

      if bestside = 0 then FUidEx.Add( T_GeoInteger.Create( -2 ) )
                      else FUidEx.Add( T_GeoInteger.Create( -1 ) ) ;

      FPoint.Add( T_GeoPoint.Create( ptg ) ) ;

      FQuery.Add( get_output_txt( bestshp.Uid, bestside, number ) ) ;

      stmp := '' ;
      for i1 := 0 to fieldList.Count-1 do begin
        fld := T_GeoField( fieldList[i1] ) ;
        if fld.Name = FHouseNumber then begin
          if addrange = 1 then begin
            strtmp := IntToStr ( number ) ;
            if (( number >= nlfrom ) and ( number <= nlto )) or
               (( number >= nlto ) and ( number <= nlfrom )) then
              stmp := stmp + fld.Name + ',L=' + strtmp + ';' ;
            if (( number >= nrfrom ) and ( number <= nrto )) or
               (( number >= nrto ) and ( number <= nrfrom )) then
              stmp := stmp + fld.Name + ',R=' + strtmp + ';' ;
          end else begin
            stmp := stmp + fld.Name + ',L=' + ';' ;
            stmp := stmp + fld.Name + ',R=' + ';' ;
          end ;
        end else begin
          strtmp := VarToString( shp.GetField( fld.Left ) ) ;
          if fld.Left = fld.Right then
            stmp := stmp + fld.Name + '=' + strtmp + ';'
          else begin
            stmp := stmp + fld.Name + ',L=' + strtmp + ';' ;
            strtmp := VarToString( shp.GetField( fld.Right ) ) ;
            stmp := stmp + fld.Name + ',R=' + strtmp + ';' ;
          end ;
        end ;
      end ;
      FQueryEx.Add( stmp ) ;

      if not _addToLayer then
        exit ;

      // protect against multiple adding same object
      stmp := UpperCase( stmp ) + ' UID:' + IntToStr( shp.Uid ) ;
      if readyList.Find( stmp, idx ) then
        exit ;
      readyList.Add( stmp ) ;

      // add temporary shapes
      if shp.GetNumParts < 0 then
        exit ;
      partsize := shp.GetPartSize( 0 ) ;
      if partsize < 0 then
        exit ;

      shp.Lock( TGIS_Lock.Projection );
      try
        perc := ipos / shp.Length ;

        // creates first subsegment from the shape
        newshp1 := layerGeo.AddShape( shp, True ) ;
        newshp1.Lock( TGIS_Lock.Projection ) ;
        try
          newshp1.SetField( GIS_GEO_SPLITMASTER , shp.Uid  ) ;
          newshp1.SetField( GIS_GEO_SPLITTYPE   , 1        ) ; //left
          newshp1.SetField( GIS_GEO_SPLITVALUE  , perc     ) ;

          newshp1.TagInternal := 1 ;
          newshp1.Reset ;
          newshp1.AddPart ;

          len := 0 ;

          pt_a := shp.GetPoint( 0, 0 ) ;
          newshp1.AddPoint( pt_a ) ;

          point_tmp := 0 ;
          for point_no := 1 to partsize -1 do  begin // all points
            pt_b := shp.GetPoint(0,point_no) ;
            len := len + GisPoint2Point( pt_a, pt_b ) ;
            if len < ipos then begin
              newshp1.AddPoint( pt_b ) ;
              pt_a := pt_b ;
            end
            else begin
              point_tmp := point_no ;
              break ;
            end ;
          end ;
          newshp1.AddPoint( TGIS_ShapeArc(shp).GetPointOnLine( ipos,  0 ) );
        finally
          newshp1.Unlock ;
        end ;

        // creates second subsegment from the shape
        newshp2 := layerGeo.AddShape( shp, True ) ;
        newshp2.Lock( TGIS_Lock.Projection ) ;
        try
          newshp2.SetField( GIS_GEO_SPLITMASTER , shp.Uid  ) ;
          newshp2.SetField( GIS_GEO_SPLITTYPE   , 2        ) ; // right
          newshp2.SetField( GIS_GEO_SPLITVALUE  , perc     ) ;

          newshp2.TagInternal := 1 ;
          newshp2.Reset ;
          newshp2.AddPart ;

          newshp2.AddPoint( TGIS_ShapeArc(shp).GetPointOnLine( ipos,  0 ) );
          for point_no := point_tmp to partsize -1 do  begin // all points
            pt_b := shp.GetPoint(0,point_no) ;
            newshp2.AddPoint( pt_b ) ;
          end ;
        finally
          newshp2.Unlock ;
        end ;

      finally
        shp.Unlock;
      end ;

      if ( bestside = 1 ) or ( bestside = -1 ) then begin
        shp.Lock( TGIS_Lock.Projection );
        try
          // final connector from road to shape
          newshp3 := layerGeo.AddShape( shp ) ;
          newshp3.Lock( TGIS_Lock.Projection );
          try
            newshp3.SetField( GIS_GEO_SPLITMASTER , shp.Uid  ) ;
            newshp3.SetField( GIS_GEO_SPLITTYPE   , 3        ) ; // connector
            newshp3.SetField( GIS_GEO_SPLITVALUE  , perc     ) ;

            newshp3.TagInternal := 2 ;
            newshp3.Reset ;
            newshp3.AddPart ;

            newshp3.AddPoint( TGIS_ShapeArc(shp).GetPointOnLine( ipos,  0 ) ) ;
            newshp3.AddPoint( ptg ) ;
          finally
            newshp3.Unlock;
          end ;
        finally
          shp.Unlock ;
        end ;
      end ;
    end ;

    procedure add_point_to_result ;
    var
      ptg    : TGIS_Point    ;
      stmp   : String        ;
      strtmp : String        ;
      k1     : Integer       ;
    begin
      shp := TGIS_ShapePoint( bestshp ) ;
      if not assigned( shp ) then
        exit ;

      found  := True ;
      inc( res ) ;

      ptg := shp.GetPoint(0,0) ;

      FUid.Add( T_GeoInteger.Create( bestshp.Uid ) ) ;
      FUidEx.Add( T_GeoInteger.Create( 0 ) ) ;

      FPoint.Add( T_GeoPoint.Create( ptg ) ) ;

      FQuery.Add( get_output_txt( bestshp.Uid ) ) ;

      stmp := '';
      for k1 := 0 to fieldList.Count-1 do begin
        fld := T_GeoField( fieldList[k1] ) ;
        strtmp := VarToString( shp.GetField( fld.Left ) ) ;
        if fld.Left = fld.Right then
          stmp := stmp + fld.Name + '=' + strtmp + ';'
        else begin
          stmp := stmp + fld.Name + ',L=' + strtmp + ';' ;
          strtmp := VarToString( shp.GetField( fld.Right ) ) ;
          stmp := stmp + fld.Name + ',R=' + strtmp + ';' ;
        end ;
      end ;
      FQueryEx.Add( stmp ) ;

    end ;

    procedure get_name_fmt(
      var   _scope : String ;
      const _name  : String
    ) ;
    var
      fmt : String ;

      procedure validateApostrophes ;
      var
        i2     : Integer ;
        valtmp : String  ;
      begin
        valtmp := '' ;
        for i2 := StringFirst to StringLast(value) do begin
          if value[i2] = '''' then begin
            valtmp := valtmp + '''''' ;
          end else
            valtmp := valtmp + value[i2] ;
        end ;
        value := valtmp ;
      end ;

    begin
      validateApostrophes ;
      if ( value[StringFirst] = '*' ) and ( value[StringLast(value)] = '*' ) then
        fmt := Format(
                 '(%s like ''%%%s%%'')',
                 [ _name, Copy( value, StringFirst+1, length(value)-2 ) ]
               )
      else if value[StringFirst] = '*' then
        fmt := Format(
                 '(%s like ''%%%s'')',
                 [ _name, Copy( value, StringFirst+1, length(value)-1 ) ]
               )
      else if  value[StringLast(value)] = '*' then
        fmt := Format(
                 '(%s like ''%s%%'')',
                 [ _name, Copy( value, StringFirst, length(value)-1 ) ]
               )
      else if _extendedScope = True then
        fmt := Format(
                 '(%s like ''%s%%'')',
                 [ _name, value ]
              )
      else
        fmt := Format(
                 getEqualHelper( _name ),
                 [ _name, value ]
               ) ;

      if IsStringEmpty( _scope ) then
        _scope := fmt
      else
        _scope := _scope + ' and ' + fmt;
    end ;

    procedure get_addr_fmt(
      var   _scope : String ;
      const _name  : String
    ) ;
    var
      fmt : String ;
    begin
      fmt := Format( getEqualHelper( _name ), [ _name, value ] ) ;
      if IsStringEmpty( _scope ) then
        _scope := fmt
      else
        _scope := _scope + ' and ' + fmt;
    end ;

    procedure get_other_fmt(
      var   _scope : String ;
      const _name  : String
    ) ;
    var
      fmt : String  ;
      p   : Integer ;
    begin
      p := Pos( '|', value );
      if p > StringFirst-1 then begin
        fmt := '';
        repeat
          if IsStringEmpty( fmt ) then
            fmt := '('
          else
            fmt := fmt + ' or ' ;

          fmt := fmt + Format(
                         getEqualHelper( _name ),
                         [ _name, Copy(value,StringFirst,p-StringFirst) ]
                       ) ;

          value := Copy( value, p+1, StringLast(value)-p );
          p := Pos( '|', value ) ;
        until p = StringFirst-1 ;

        fmt := fmt + ' or ' +
               Format( getEqualHelper( _name ), [ _name, value ] ) ;

        fmt := fmt + ')' ;
      end else
        fmt := Format( getEqualHelper( _name ), [ _name, value ] ) ;
      if IsStringEmpty( _scope ) then
        _scope := fmt
      else
        _scope := _scope + ' and ' + fmt;
    end ;

    procedure get_scope(
      var   _scope   : String ;
      var   _hnfield : String ;
      const _name    : String
    ) ;
    begin
      if ( fld.Name = FStreetName ) or
         ( fld.Name = FCityName   ) then
      begin
        get_name_fmt( _scope, _name ) ;

      end else if fld.Name = FHouseNumber then begin
        if shpType = TGIS_ShapeType.Arc then begin
          //more complicated
          //the condition must be checked later
          addrange := 1;
          number := StrToInt(value);
        end else if not assigned( FOnAddress ) then begin
          get_addr_fmt( _scope, _name ) ;
        end else begin
          hnmatch  := value ;
          _hnfield := _name ;
          doOnAddress := 1;
        end ;
      end else begin
        //any other one
        get_other_fmt( _scope, _name ) ;
      end ;
    end ;

    procedure find_results(
      const _scope   : String ;
      const _hnfield : String
    ) ;
    var
      k1    : Integer ;
      laddr : Integer ;
      raddr : Integer ;
      {$IFDEF DCC}
        shp0 : TGIS_Shape ;
      {$ENDIF}
    begin

      laddr := 0 ;
      raddr := 0 ;
      if addrange = 1 then begin
        if _scope = lscope then
          laddr := 1 ;
        if _scope = rscope then
          raddr := 1 ;
      end ;

      for shp0 in layerObj.Loop( GisWholeWorld, _scope ) do begin
        if ( shpType = TGIS_ShapeType.Arc ) and
           ( shp0 is TGIS_ShapeArc ) then
        begin
          shp := shp0 as TGIS_ShapeArc ;
          if shp.Length > 0 then begin
            bestshp := shp ;
            if addrange = 0 then begin
              bestpos  := 0.5 ;
              bestside := 0   ;
              add_arc_to_result ;
              if res >= GIS_GEO_RETURNED_LIMIT then
                exit ;
              if _firstMatch then
                exit;
            end else begin
              fld := nil ;
              for k1 := 0 to fieldList.Count-1 do begin
                fld := T_GeoField( fieldList[k1] ) ;
                if fld.Name = FHouseNumber then
                  break ;
              end ;
              try
                nlfrom := StrToInt( VarToString( shp.GetField( fld.Left ) ) ) ;
              except
                nlfrom := 0 ;
              end ;
              try
                nlto   := StrToInt( VarToString( shp.GetField( fld.Left2 ) ) ) ;
              except
                nlto   := 0 ;
              end ;
              try
                nrfrom := StrToInt( VarToString( shp.GetField( fld.Right ) ) ) ;
              except
                nrfrom := 0 ;
              end ;
              try
                nrto   := StrToInt( VarToString( shp.GetField( fld.Right2 ) ) ) ;
              except
                nrto   := 0 ;
              end ;

              if ( ( ( laddr = 1 ) and ( raddr = 1 ) ) and
                   ( ( nlfrom = nrfrom ) and ( nlto = nrto ) ) and
                   ( ( nlfrom <= number ) and ( number <= nlto   ) or
                     ( nlto   <= number ) and ( number <= nlfrom ) ) ) then
              begin
                bestside := 0 ;
                if nlto <> nlfrom
                  then bestpos := ( 1.0*( number-nlfrom ) )/( nlto-nlfrom )
                  else bestpos := 0.5 ;
                add_arc_to_result ;
                if res >= GIS_GEO_RETURNED_LIMIT then
                  exit ;
                if _firstMatch then
                  exit ;
              end ;

              if ( ( laddr = 1 ) and
                   ( ( nlfrom <= number ) and ( number <= nlto   ) or
                     ( nlto   <= number ) and ( number <= nlfrom ) ) ) then
              begin
                // same side of street - bit tricky we are checking
                // for odd/even matching
                  matchside := ( (nlfrom and 1) - (number and 1) ) =
                               ( (number and 1) - (nlto   and 1) ) ;
                if matchside then begin
                  bestside := -1 ;
                  if nlto <> nlfrom
                    then bestpos := ( 1.0*( number-nlfrom ) )/( nlto-nlfrom )
                    else bestpos := 0.5 ;
                  add_arc_to_result ;
                  if res >= GIS_GEO_RETURNED_LIMIT then
                    exit ;
                  if _firstMatch then
                    exit ;
                end ;

              end ;

              if ( ( raddr = 1 ) and
                   ( ( nrfrom <= number ) and ( number <= nrto   ) or
                     ( nrto   <= number ) and ( number <= nrfrom ) ) ) then
              begin
                // same side of street - bit tricky we are checking
                // for odd/even matching
                  matchside := ( (nrfrom and 1) - (number and 1) ) =
                               ( (number and 1) - (nrto   and 1) ) ;

                if matchside then begin
                  bestside := 1 ;
                  if nrto <> nrfrom
                    then bestpos := ( 1.0*( number-nrfrom ) )/( nrto-nrfrom )
                    else bestpos := 0.5 ;
                  add_arc_to_result ;
                  if res >= GIS_GEO_RETURNED_LIMIT then
                    exit ;
                  if _firstMatch then
                    exit ;
                end ;
              end ;

            end ;
          end ;
        end else if (shpType = TGIS_ShapeType.Point) and
                    (shp0 is TGIS_ShapePoint) then begin
          shp := shp0 as TGIS_ShapePoint ;
          if ( doOnAddress = 0 ) or
             ( ( doOnAddress = 1 )
               and
               ( FOnAddress( hnmatch, VarToString( shp.GetField(_hnfield) )
               ) = True )
             ) then
          begin
            bestshp := shp ;
            add_point_to_result ;
            if res >= GIS_GEO_RETURNED_LIMIT then
              exit ;
            if _firstMatch then
              exit ;
          end ;
        end ;
      end ;
    end ;

  begin
    res := 0 ;

    number   := 0  ;
    bestshp  := nil ;
    bestpos  := -1 ;
    bestside := 0  ;
    found    := False ;

    shp := nil ;

    sortResolvedAddresses( _resolvedAddresses ) ;

    for i := 0 to _resolvedAddresses.Count - 1 do begin

      strings := TStrings( _resolvedAddresses[i] ) ;

      noleft  := 0 ;
      lscope  := '' ;
      noright := 0 ;
      rscope  := '' ;
      fld     := nil  ;
      addrange    := 0 ;
      doOnAddress := 0 ;
      hnlfield    := '' ;
      hnrfield    := '' ;

      for j := 0 to strings.Count-1 do begin
        fld := nil ;
        for k := 0 to fieldList.Count-1 do begin
          if T_GeoField(fieldList[k]).Name = strings.Names[j] then begin
            fld := T_GeoField( fieldList[k] ) ;
            break ;
          end ;
        end ;
        if not assigned( fld ) then break ;

        value := strings.Values[fld.Name];
        if ( noleft  = 0 ) and ( not IsStringEmpty( fld.Left ) )  then
          get_scope( lscope, hnlfield, fld.Left  )
        else
          noleft := 1 ;
        if ( noright = 0 ) and ( not IsStringEmpty( fld.Right ) ) then
          get_scope( rscope, hnrfield, fld.Right )
        else
          noright := 1 ;
      end;
      if not assigned( fld ) then continue ;

      if noleft = 0 then begin
        find_results( lscope, hnlfield ) ;

        Result := res ;

        if res >= GIS_GEO_RETURNED_LIMIT then
          exit ;
        if _firstMatch and found then
          exit ;

        if lscope = rscope then
          //everything done
          noright := 1 ;
      end ;
      if noright = 0 then begin
        find_results( rscope, hnrfield ) ;
        Result := res ;
        if res >= GIS_GEO_RETURNED_LIMIT then
          exit ;
        if _firstMatch and found then
          exit ;
      end ;
    end ;
    Result := res ;
  end ;

  function TGIS_Geocoding.Parse(
    const _addr : String
  ) : Integer ;
  begin
    if OSMGeocoding then
      Result := T_osmGeocoding( osmGeo ).Parse( _addr )
    else
      Result := Parse( _addr, True, True ) ;
  end ;

  function TGIS_Geocoding.Parse(
    const _addr        : String         ;
    const _firstMatch  : Boolean        ;
    const _addToLayer  : Boolean
  ) : Integer ;
  var
    nameA  : String  ;
    nameB  : String  ;
    number : Integer ;
  begin
    if OSMGeocoding then begin
      Result := T_osmGeocoding( osmGeo ).Parse(
                  _addr, _firstMatch, _addToLayer
                ) ;
      exit ;
    end ;

    Result := 0 ;

    layerObj.RaiseBusyPrepare( Self, _rsrc( GIS_RS_BUSY_GEOCODE ) ) ;
    try

      clearResult ;
      readyList.Clear ;

      if shpType <> TGIS_ShapeType.Arc then
        exit;

      if splitCrossing( _addr, nameA, nameB ) then begin
        Result := findCrossing( nameA, nameB, _firstMatch ) ;
      end
      else begin
        // find number
        if not splitNumber( _addr, nameA, number ) then
          exit ;

        Result := findNumber( nameA, number, _firstMatch, _addToLayer ) ;
      end ;

    finally
      layerObj.RaiseBusyRelease( Self ) ;
    end;
  end ;

  function TGIS_Geocoding.Match(
    const _addr               : String          ;
    var   _resolvedAddresses  : TGIS_ObjectList ;
    var   _resolvedAddresses2 : TGIS_ObjectList
  ) : Boolean ;
  var
    nameA  : String ;
    nameB  : String ;

  begin
    if OSMGeocoding then begin
      Result := T_osmGeocoding( osmGeo ).Match(
                  _addr, _resolvedAddresses, _resolvedAddresses2
                ) ;
      exit ;
    end ;

    Result := False ;
    if not assigned( matcherObj ) or
       ( fieldList.Count = 0 ) then
         exit ;

    if ( shpType = TGIS_ShapeType.Arc ) and
       splitCrossingEx( _addr, nameA, nameB ) then begin
      // match two address points
      if ( matcherObj.Match( nameA, _resolvedAddresses  ) > 0 ) and
         ( matcherObj.Match( nameB, _resolvedAddresses2 ) > 0 ) then begin
        // ignore features with empty feature names
        prepareForCrossing( _resolvedAddresses, _resolvedAddresses2 );
        if ( _resolvedAddresses.Count  > 0 )  and
           ( _resolvedAddresses2.Count > 0 ) then
          Result := True ;
      end ;
    end else
      // match the input address
      if matcherObj.Match( _addr, _resolvedAddresses ) > 0 then
        Result := True ;
  end;

  function TGIS_Geocoding.ParseEx(
    const _addr            : String          ;
    {$IFNDEF OXYGENE}
      const _firstMatch    : Boolean         ;
      const _extendedScope : Boolean         ;
      const _addToLayer    : Boolean
    {$ELSE}
      const _firstMatch    : Boolean := True ;
      const _extendedScope : Boolean := True ;
      const _addToLayer    : Boolean := True
    {$ENDIF}
  ) : Integer ;
  var
    nameA  : String ;
    nameB  : String ;
    resolvedAddresses  : TGIS_ObjectList;
    resolvedAddresses2 : TGIS_ObjectList;
  begin
    if OSMGeocoding then begin
      Result := T_osmGeocoding( osmGeo ).ParseEx(
                  _addr, _firstMatch, _extendedScope, _addToLayer
                ) ;
      exit ;
    end ;

    Result := 0 ;

    layerObj.RaiseBusyPrepare( Self, _rsrc( GIS_RS_BUSY_GEOCODE ) ) ;
    try

      if not assigned( matcherObj ) or
         ( fieldList.Count = 0 ) then
           exit ;

      clearResult ;
      readyList.Clear ;

      if (shpType <> TGIS_ShapeType.Arc) and (shpType <> TGIS_ShapeType.Point) then
        exit;

      resolvedAddresses  := nil ;
      resolvedAddresses2 := nil ;
      try

        if ( shpType = TGIS_ShapeType.Arc ) and
           splitCrossingEx( _addr, nameA, nameB ) then begin
          // match two address points
          if ( matcherObj.Match( nameA, resolvedAddresses  ) > 0 ) and
             ( matcherObj.Match( nameB, resolvedAddresses2 ) > 0 ) then
          begin
            // ignore features with empty feature names
            prepareForCrossing( resolvedAddresses, resolvedAddresses2 );
            if ( resolvedAddresses.Count  > 0 )  and
               ( resolvedAddresses2.Count > 0 ) then
              // search for common points
              Result := findCrossingEx( resolvedAddresses, resolvedAddresses2,
                                        _firstMatch, _extendedScope ) ;
          end ;
        end else
          // match the input address
          if matcherObj.Match( _addr, resolvedAddresses ) > 0 then
            Result := findAddressEx( resolvedAddresses, _firstMatch,
                                     _extendedScope, _addToLayer
                                   ) ;
      finally
        if assigned( resolvedAddresses ) then
          FreeObject( resolvedAddresses ) ;
        if assigned( resolvedAddresses2 ) then
          FreeObject( resolvedAddresses2 ) ;
      end;

    finally
      layerObj.RaiseBusyRelease( Self ) ;
    end;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_Geocoding.ParseEx(
      const _resolvedAddresses  : TGIS_ObjectList ;
      const _resolvedAddresses2 : TGIS_ObjectList ;
      const _firstMatch         : Boolean := True ;
      const _extendedScope      : Boolean := True ;
      const _addToLayer         : Boolean := True
    ) : Integer ;
  {$ELSE}

    function TGIS_Geocoding.ParseEx(
      const _resolvedAddresses  : TGIS_ObjectList     ;
      const _resolvedAddresses2 : TGIS_ObjectList     ;
      {$IFNDEF OXYGENE}
        const _firstMatch       : Boolean         ;
        const _extendedScope    : Boolean         ;
        const _addToLayer       : Boolean
      {$ELSE}
        const _firstMatch       : Boolean := True ;
        const _extendedScope    : Boolean := True ;
        const _addToLayer       : Boolean := True
      {$ENDIF}
    ) : Integer ;
  {$ENDIF}
  begin
    if OSMGeocoding then begin
      Result := T_osmGeocoding( osmGeo ).ParseEx(
                  _resolvedAddresses, _resolvedAddresses2,
                  _firstMatch, _extendedScope, _addToLayer
                ) ;
      exit ;
    end ;

    Result := 0 ;

    layerObj.RaiseBusyPrepare( Self, _rsrc( GIS_RS_BUSY_GEOCODE ) ) ;
    try

      if not assigned( matcherObj ) or
         ( fieldList.Count = 0 ) then
           exit ;

      clearResult ;
      readyList.Clear ;

      if (shpType <> TGIS_ShapeType.Arc) and (shpType <> TGIS_ShapeType.Point) then
        exit ;
      if not assigned( _resolvedAddresses ) or
         ( _resolvedAddresses.Count = 0 ) then
           exit ;

      if ( shpType = TGIS_ShapeType.Arc ) and
         assigned( _resolvedAddresses2 ) and
         ( _resolvedAddresses2.Count > 0 ) then
        Result := findCrossingEx( _resolvedAddresses,
                                  _resolvedAddresses2,
                                  _firstMatch,
                                  _extendedScope
                                )
      else
        Result := findAddressEx( _resolvedAddresses,
                                 _firstMatch,
                                 _extendedScope,
                                 _addToLayer
                               ) ;

    finally
      layerObj.RaiseBusyRelease( Self ) ;
    end;
  end ;

  procedure TGIS_Geocoding.Clear ;
  begin
    if OSMGeocoding then
      T_osmGeocoding( osmGeo ).Clear ;

    clearResult ;
    readyList.Clear ;
    if assigned( layerGeo ) then layerGeo.RevertShapes ;
  end ;

  function TGIS_Geocoding.ReverseGeocode(
    const _ptg     : TGIS_Point ;
    const _prec    : Double     ;
    var   _shp     : TGIS_Shape ;
    var   _address : String
  ) : Double ;
  begin
    if OSMGeocoding then
      Result := T_osmGeocoding( osmGeo ).ReverseGeocode(
                  _ptg, _prec, _shp, _address
                )
    else
      Result := ReverseGeocode( _ptg, _prec, _shp, _address, '' ) ;
  end ;

  function TGIS_Geocoding.ReverseGeocode(
    const _ptg             : TGIS_Point ;
    const _prec            : Double     ;
    var   _shp             : TGIS_Shape ;
    var   _address         : String     ;
    const _additionalQuery : String
  ) : Double ;
  var
    qr          : String  ;
    {$IFDEF DCC}
      shp0      : TGIS_Shape ;
    {$ENDIF}
    shp         : TGIS_Shape ;
    id          : TGIS_Uid ;
    sidetmp     : Integer ;       // -1 - left of the line; 0 - on the line;
                                  // 1 - right of the line; 2 - undefined
    locationtmp : Integer ;       // -1 - before; 0 - within; 1 - beyond
    gaptmp      : Double  ;       // between the point and the line
    distancetmp : Double  ;       // if location = -1 then 0
                                  // if location = 1  then Length() of the line
    side        : Integer ;
    gap         : Double  ;
    distance    : Double  ;

    function get_number : Integer ;
    var
      arcshp : TGIS_ShapeArc ;
      fld    : T_GeoField ;
      i      : Integer ;
      nfrom  : Integer ;
      nto    : Integer ;
    begin
      Result := 0 ;
      arcshp := TGIS_ShapeArc(  _shp ) ;
      fld := nil ;
      for i := 0 to fieldList.Count-1 do begin
        fld := T_GeoField( fieldList[i] ) ;
        if fld.Name = FHouseNumber then
          break ;
      end ;
      if not assigned( fld ) then
        exit ;
      if side = -1 then begin
        try
          nfrom := StrToInt( VarToString( arcshp.GetField( fld.Left ) ) ) ;
        except
          nfrom := 0 ;
        end ;
        try
          nto   := StrToInt( VarToString( arcshp.GetField( fld.Left2 ) ) ) ;
        except
          nto   := 0 ;
        end ;
      end else begin
        try
          nfrom := StrToInt( VarToString( arcshp.GetField( fld.Right ) ) ) ;
        except
          nfrom := 0 ;
        end ;
        try
          nto   := StrToInt( VarToString( arcshp.GetField( fld.Right2 ) ) ) ;
        except
          nto   := 0 ;
        end ;
      end ;
      if      distance = 0             then Result := nfrom
      else if distance = arcshp.Length then Result := nto
      else begin
        if nfrom < nto then begin
          Result := nfrom +
                    RoundS(
                      ( distance * ( nto - nfrom ) / arcshp.Length ) / 2
                    ) * 2 ;
        end else
          Result := nfrom -
                    RoundS(
                      ( distance * ( nfrom - nto ) / arcshp.Length ) / 2
                    ) * 2 ;
      end ;
    end ;

  begin
    if OSMGeocoding then begin
      Result := T_osmGeocoding( osmGeo ).ReverseGeocode(
        _ptg, _prec, _shp, _address, _additionalQuery
      ) ;
      exit ;
    end ;

    _shp     := nil ;
    _address := ''  ;

    qr := FReverseRestrictQuery ;
    if not IsStringEmpty( _additionalQuery ) then begin
      if not IsStringEmpty( qr ) then
        qr := '(' + qr + ') and (' + _additionalQuery + ')'
      else
        qr := _additionalQuery ;
    end ;

    gap := GIS_MAX_DOUBLE ;
    id := -1 ;
    for shp0 in layerObj.Loop(
                 GisExtent( _ptg.X-_prec,_ptg.Y-_prec, _ptg.X+_prec, _ptg.Y+_prec ),
                 qr
               ) do begin
      if shpType = TGIS_ShapeType.Arc then begin
        shp := shp0 as TGIS_ShapeArc ;
        distancetmp := TGIS_ShapeArc(shp).GetDistanceOnLine(
          _ptg, sidetmp, locationtmp, gaptmp, -1
        ) ;
        if (gaptmp < gap) and (gaptmp <= _prec) then begin
          id := shp.Uid ;
          gap := gaptmp ;
          distance := distancetmp ;
          side := sidetmp ;
        end ;
      end else if shpType = TGIS_ShapeType.Point then begin
        shp := shp0 as TGIS_ShapePoint ;
        gaptmp := shp.Distance( _ptg, _prec ) ;
        if (gaptmp < gap) and (gaptmp <= _prec) then begin
          id := shp.Uid ;
          gap := gaptmp ;
        end ;
      end ;
    end ;
    if id <> -1 then begin
      _shp := layerObj.GetShape( id ) ;
      if shpType = TGIS_ShapeType.Arc then begin
        _address := get_output_txt( id, side, get_number ) ;
        if IsStringEmpty( _address ) then begin
          if side = -1 then side :=  1
                       else side := -1 ;
          _address := get_output_txt( id, side, get_number ) ;
        end ;
      end else if shpType = TGIS_ShapeType.Point then begin
        _address := get_output_txt( id ) ;
      end;
    end ;
    Result := gap ;
  end;

  function TGIS_Geocoding.AddPoint(
    const _ptg   : TGIS_Point     ;
    const _prec  : Double         ;
    const _shape : TGIS_ShapeArc
  ) : Boolean ;
  var
    shp       : TGIS_ShapeArc    ;
    dtmp      : Double           ;
    ptg       : TGIS_Point       ;
    itmp1     : Integer          ;
    itmp2     : Integer          ;
    dist      : Double           ;
    partsize  : Integer          ;
    newshp1   : TGIS_Shape       ;
    newshp2   : TGIS_Shape       ;
    newshp3   : TGIS_Shape       ;
    pt_a      : TGIS_Point       ;
    pt_b      : TGIS_Point       ;
    point_tmp : Integer          ;
    point_no  : Integer          ;
    shplen    : Double           ;
    perc      : Double           ;
  begin
    Result := False ;

    if assigned( _shape ) then begin
      dtmp := _shape.Distance2Part( _ptg, GIS_MAX_DOUBLE, 0, ptg ) ;

      if dtmp > _prec then
        exit ;

      shp := _shape ;
    end
    else begin
      // find shape to which we want connect
      shp := TGIS_ShapeArc(
               layerObj.LocateEx( _ptg, _prec, -1, dtmp, itmp1, ptg )
             ) ;
    end ;

    if not assigned( shp ) then
      exit ;

    shp.Lock( TGIS_Lock.Projection );
    try
      dist := shp.GetDistanceOnLine( ptg, itmp1, itmp2, dtmp ) ;

      partsize := shp.GetPartSize( 0 ) ;
      point_tmp := 0 ;

      shplen := shp.Length ;
      if Abs( shplen ) < 1E-7       then perc := 0
                                    else perc := dist / shplen ;

      if   perc > 1                 then perc := 1
      else if Abs( perc -1 ) < 1E-7 then perc := 1 ;
      if   perc < 0                 then perc := 0
      else if Abs( perc    ) < 1E-7 then perc := 0 ;

      if ( perc > 0 )and ( perc < 1 ) then begin
        // creates first subsegment from the shape
        newshp1 := layerGeo.AddShape( shp, True ) ;
        newshp1.Lock( TGIS_Lock.Projection ) ;
        try
          newshp1.SetField( GIS_GEO_SPLITMASTER , shp.Uid  ) ;
          newshp1.SetField( GIS_GEO_SPLITTYPE   , 1        ) ; // left
          newshp1.SetField( GIS_GEO_SPLITVALUE  , perc     ) ;

          newshp1.TagInternal := 1 ;
          newshp1.Reset ;
          newshp1.AddPart ;

          dtmp := 0 ;

          pt_a := shp.GetPoint( 0, 0 ) ;
          newshp1.AddPoint( pt_a ) ;

          for point_no := 1 to partsize -1 do  begin // all points
            pt_b := shp.GetPoint(0,point_no) ;
            dtmp := dtmp + GisPoint2Point( pt_a, pt_b ) ;
            if dtmp < dist then begin
              newshp1.AddPoint( pt_b ) ;
              pt_a := pt_b ;
            end
            else begin
              point_tmp := point_no ;
              break ;
            end ;
          end ;
          newshp1.AddPoint( shp.GetPointOnLine( dist,  0 ) );
        finally
          newshp1.Unlock ;
        end ;

        // creates second subsegment from the shape
        newshp2 := layerGeo.AddShape( shp, True ) ;
        newshp2.Lock( TGIS_Lock.Projection ) ;
        try
          newshp2.SetField( GIS_GEO_SPLITMASTER , shp.Uid  ) ;
          newshp2.SetField( GIS_GEO_SPLITTYPE   , 2        ) ;  // right
          newshp2.SetField( GIS_GEO_SPLITVALUE  , perc     ) ;

          newshp2.TagInternal := 1 ;
          newshp2.Reset ;
          newshp2.AddPart ;

          newshp2.AddPoint( shp.GetPointOnLine( dist,  0 ) );

          for point_no := point_tmp to partsize -1 do  begin // all points
            pt_b := shp.GetPoint(0,point_no) ;
            newshp2.AddPoint( pt_b ) ;
          end ;
        finally
          newshp2.Unlock ;
        end ;
      end ;

      // final connector from road to shape
      newshp3 := layerGeo.AddShape( shp, True ) ;
      newshp3.Lock( TGIS_Lock.Projection ) ;
      try
        newshp3.SetField( GIS_GEO_SPLITMASTER , shp.Uid  ) ;
        newshp3.SetField( GIS_GEO_SPLITTYPE   , 3        ) ; // connector
        newshp3.SetField( GIS_GEO_SPLITVALUE  , perc     ) ;

        newshp3.TagInternal := 2 ;
        newshp3.Reset ;
        newshp3.AddPart ;

        newshp3.AddPoint( shp.GetPointOnLine( dist,  0 ) );
        newshp3.AddPoint( _ptg ) ;
      finally
        newshp3.Unlock ;
      end;
    finally
      shp.Unlock;
    end ;

    Result := True ;
  end;

//==================================== END =====================================
end.
