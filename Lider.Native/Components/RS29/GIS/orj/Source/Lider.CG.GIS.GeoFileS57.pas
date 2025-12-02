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

{$IFDEF DCC}
  unit GisFileS57 ;
  {$HPPEMIT '#pragma link "GisFileS57"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Variants,
    GisRtl,
    GisTypesUI,
    GisTypes ;
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

 // Events

  // Create and prepare a new shape.
  {#GENDOC:HIDE}
  T_ExecNewShapeEvent = procedure(
    const _shp_type : TGIS_ShapeType ;
    const _dim      : TGIS_DimensionType
  ) of object ;

  // Add coordinates.
  {#GENDOC:HIDE}
  T_ExecAddCoordEvent = procedure(
    const _ptg      : TGIS_Point3D
  ) of object ;

  // Add attribute to a new shape.
  {#GENDOC:HIDE}
  T_ExecAddAttrEvent = procedure(
    const _name     : String ;
    const _val      : String ;
    const _tp       : Char
  ) of object ;

  // Set layer for a new shape.
  {#GENDOC:HIDE}
  T_ExecSetLayerEvent = procedure(
    const _name     : String  ;
    const _caption  : String  ;
    const _gtype    : Integer
  ) of object ;

  // Add part to a new shape.
  {#GENDOC:HIDE}
  T_ExecAddPartEvent = procedure of object ;

  // Finish creating a new shape.
  {#GENDOC:HIDE}
  T_ExecEndShapeEvent = procedure of object ;

//====================================================================
//  TGIS_FileS57 class
//====================================================================

  /// <summary>
  ///   This clsss reads S-57 files.
  /// </summary>
  TGIS_FileS57 = class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      // Visual flags

      FUseS57Draw : Boolean ;

     // Attributes lists.
      FS57Metadata : TObject ;

      // Events handles.

      FOnNewShape        : T_ExecNewShapeEvent ;
      FOnEndShape        : T_ExecEndShapeEvent ;
      FOnAddPart         : T_ExecAddPartEvent  ;
      FOnAddCoord        : T_ExecAddCoordEvent ;
      FOnAddAttr         : T_ExecAddAttrEvent  ;
      FOnSetLayer        : T_ExecSetLayerEvent ;
      FOnBusy            : TGIS_BusyEvent      ;

      // Layer filter

      FLayerName  : String  ;
      fname       : String ;
      FSkipObject : Boolean ;
    private

      /// <summary>
      ///   Initialize attributes and object class lists with values.
      /// </summary>
      procedure InitializeLists;

    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      /// <summary>
      ///    Find object class record
      /// </summary>
      /// <param name="_objl">
      ///   object id
      /// </param>
      /// <param name="_objIdx">
      ///   found record index
      /// </param>
      /// <returns>
      ///    True if class is found
      /// </returns>
      function  findObjClass        ( const _objl     : Integer ;
                                      out _objIdx     : TObject
                                    ) : Boolean ;

      /// <summary>
      ///   Find attribute name by index.
      /// </summary>
      /// <param name="_attrIdx">
      ///   attribute id
      /// </param>
      /// <param name="_attrName">
      ///   found attribute name
      /// </param>
      /// <param name="_attrType">
      ///   found attribute type
      /// </param>
      /// <returns>
      ///    True if attribute is found
      /// </returns>
      function  findAttrName        ( const _attrIdx  : Integer;
                                      out   _attrName : String ;
                                      out   _attrType : Char
                                    ): Boolean;

      /// <summary>
      ///   Add object attributes.
      /// </summary>
      /// <param name="_objl">
      ///   object code
      /// </param>
      procedure addAttrPrim         ( const _objl     : Integer
                                    );

      /// <summary>
      ///   Test layer if can be read
      /// </summary>
      /// <param name="_objl">
      ///   object code
      /// </param>
      procedure testLayer           ( const _objl     : Integer
                                    );

      /// <summary>
      ///   Get object class display priority
      /// </summary>
      /// <param name="_objl">
      ///   object id
      /// </param>
      /// <returns>
      ///    priority
      /// </returns>
      function  getLayerPriority    ( const _objl     : Integer
                                    ) : Integer ;

      /// <summary>
      ///  Prepare a layer.
      /// </summary>
      /// <param name="_objl">
      ///   object code
      /// </param>
      procedure prepareLayer        ( const _objl     : Integer
                                     );
    protected

      ///  <summary>
      ///    Destructor.
      ///  </summary>
      procedure doDestroy ; override;
    public

       /// <summary>
       ///   Constructor that reads S-57 file header.
       /// </summary>
       /// <param name="_file">
       ///   S-57 file name
       /// </param>
      constructor Create( const _file : String );

      /// <summary>
      ///  Read and parse S-57 file format.
      /// </summary>
      procedure ParseS57 ;

    public // properties
      /// <summary>
      ///   Use Sea map like drawing (colors, styles, etc.)
      /// </summary>
      property UseS57Draw  : Boolean read FUseS57Draw write FUseS57Draw ;

      /// <summary>
      ///   Layer name to load
      /// </summary>
      property LayerName   : String read FLayerName  write FLayerName  ;


    published //events
      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Busy event. Will be fired regularly during long-drawn operations.
        /// </summary>
        event    BusyEvent      : TGIS_BusyEvent delegate FOnBusy ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Busy event. Will be fired regularly during long-drawn operations.
        /// </summary>
        property BusyEvent      : TGIS_BusyEvent read  FOnBusy
                                                 write FOnBusy ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   NewShape event. Will be fired when a new shape is created.
        /// </summary>
        event    NewShapeEvent  : T_ExecNewShapeEvent
                                                 delegate FOnNewShape ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   NewShape event. Will be fired when a new shape is created.
        /// </summary>
        property NewShapeEvent  : T_ExecNewShapeEvent
                                                 read  FOnNewShape
                                                 write FOnNewShape ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   EndShape event. Will be fired when a new shape is built.
        /// </summary>
        event    EndShapeEvent  : T_ExecEndShapeEvent
                                                 delegate FOnEndShape ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   EndShape event. Will be fired when a new shape is built.
        /// </summary>
        property EndShapeEvent  : T_ExecEndShapeEvent
                                                 read  FOnEndShape
                                                 write FOnEndShape ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   AddPart event. Will be fired when a new part is added to shape.
        /// </summary>
        event    AddPartEvent   : T_ExecAddPartEvent
                                                 delegate FOnAddPart ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   AddPart event. Will be fired when a new part is added to shape.
        /// </summary>
        property AddPartEvent   : T_ExecAddPartEvent
                                                 read  FOnAddPart
                                                 write FOnAddPart ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   AddCoord event. Will be fired when coordinates are added to shape.
        /// </summary>
        event    AddCoordEvent  : T_ExecAddCoordEvent
                                                 delegate FOnAddCoord ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   AddCoord event. Will be fired when coordinates are added to shape.
        /// </summary>
        property AddCoordEvent  : T_ExecAddCoordEvent
                                                 read  FOnAddCoord
                                                   write FOnAddCoord ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   AddAttr event. Will be fired when attributes are added to shape.
        /// </summary>
        event    AddAttrEvent   : T_ExecAddAttrEvent
                                                 delegate FOnAddAttr ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   AddAttr event. Will be fired when attributes are added to shape.
        /// </summary>
        property AddAttrEvent   : T_ExecAddAttrEvent
                                                 read  FOnAddAttr
                                                 write FOnAddAttr ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   SetLayer event. Will be fired when layer is set for shape.
        /// </summary>
        event    SetLayerEvent  : T_ExecSetLayerEvent
                                                 delegate FOnSetLayer ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   SetLayer event. Will be fired when layer is set for shape.
        /// </summary>
        property SetLayerEvent  : T_ExecSetLayerEvent
                                                 read  FOnSetLayer
                                                 write FOnSetLayer ;
      {$ENDIF}
  end ;

const
  {#GENDOC:HIDE}
  S57_SOUNDING_FIELD = 'SOUNDING'  ;
  {#GENDOC:HIDE}
  S57_OBJ_FIELD      = 'OBJL'      ;
  {#GENDOC:HIDE}
  S57_LAYER_FIELD    = 'LAYER'     ;
  {#GENDOC:HIDE}
  S57_DESC_FIELD     = 'LAYER_DESC';

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    {$IFDEF CLR}
      System.IO,
    {$ENDIF}
    System.Math,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisClasses,
    GisInternals,
    GisFile8211 ;
{$ENDIF}

type
  // This clsss reads S-57 format.
  T_S57Reader = class( TGIS_ObjectDisposable )
    private
      parent        : TObject ;
      nFDefnCount   : Integer ;
      pszModuleName : String ;
      pszDSNM       : String ;
      poModule      : TGIS_DDFModule ;
      nCOMF         : Integer ;
      nSOMF         : Integer ;
      oVI_Index     : TDictionary<Integer,TObject> ;
      oVC_Index     : TDictionary<Integer,TObject> ;
      oVE_Index     : TDictionary<Integer,TObject> ;
      oVF_Index     : TDictionary<Integer,TObject> ;
      nNextVIIndex  : Integer ;
      nNextVCIndex  : Integer ;
      nNextVEIndex  : Integer ;
      nNextVFIndex  : Integer ;
      nNextFEIndex  : Integer ;
      oFE_Index     : TGIS_ObjectList ;
      nNextDSIDIndex: Integer ;
      szUPDNUpdate  : String ;
      nOptionFlags  : Integer ;
      iPointOffset  : Integer ;
    protected

      // Apply updates.
      procedure findAndApplyUpdates ;

      // Rewind.
      procedure rewind ;

      // Read features.
      procedure readFeatures ;
      procedure applyUpdates         ( const _poUModule : TGIS_DDFModule
                                      ) ;
      // Build soundings.
      procedure buildSoundingGeometry( const _poRecord : TGIS_DDFRecord ;
                                       const _nOBJL    : Integer
                                      ) ;
      // Build points.
      procedure buildPointGeometry   ( const _poRecord : TGIS_DDFRecord ;
                                       const _nOBJL    : Integer
                                      ) ;
      // Build lines.
      procedure buildLineGeometry    ( const _poRecord : TGIS_DDFRecord ;
                                       const _nOBJL    : Integer
                                      ) ;
      // Build areas.
      procedure buildAreaGeometry    ( const _poRecord : TGIS_DDFRecord ;
                                       const _nOBJL    : Integer
                                      ) ;
      // Apply attributes.
      procedure applyAttributes      ( const _poRecord : TGIS_DDFRecord ;
                                       const _nOBJL    : Integer
                                      ) ;
      // Generate feature attributes.
      procedure generateAttributes   ( const _poRecord : TGIS_DDFRecord ;
                                       const _nOBJL    : Integer
                                      ) ;
      // Parse name value.
      function  parseName            ( const _poRecord : TGIS_DDFRecord ;
                                       const _poField  : TGIS_DDFField ;
                                       const _nIndex   : Integer ;
                                         var _pnRCNM   : Integer
                                      ) : Integer ;
      function  fetchPoint           ( const _nRCNM     : Integer ;
                                       const _nRCID     : Integer ;
                                         var _ptg       : TGIS_Point3D
                                      ) : Boolean ;

      // Fetch line.
      function  fetchLine            ( const _poSRecord : TGIS_DDFRecord ;
                                       const _iDirection  : Integer
                                      ) : Boolean ;
    protected

      // Destroy an instance.
      procedure doDestroy ; override;
    public

      // Create an instance
      constructor Create( const _fileName : String ;
                          const _parent   : TObject
                         ) ;

      // Open reader.
      function  Open : Boolean ;

      // Close reader.
      procedure Close ;

      // Build features.
      procedure BuildFeatures ;
  end ;

  // Object class.
  T_S57ObjectClass = class
    ic  : Integer ;
    oc  : String  ;
    ac  : String  ;
    gt  : Integer ;
    dp  : Integer ;
  end ;

  // Attribute class.
  T_S57Attr = class
    ai  : Integer ;
    an  : String  ;
    at  : Char    ;
  end ;

  // S57 tables container class.
  T_S57Metadata = class( TGIS_Object )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}

      // Object class table, keeps object code, name.
      lstOC : TDictionary<Integer, T_S57ObjectClass> ;

      // Attributes names table, keeps attribute code and name.
      lstAT : TDictionary<Integer, T_S57Attr> ;
    private

      // Prepare all S57 tables.
      procedure initS57Metadata ;
    protected

      // destroy an instance.
      procedure doDestroy ; override;
    public
      constructor Create ;
  end ;

const
  RCNM_FE    =     100  ;  // Feature record
  RCNM_VI    =     110  ;  // Isolated Node
  RCNM_VC    =     120  ;  // Connected Node
  RCNM_VE    =     130  ;  // Edge
  RCNM_VF    =     140  ;  // Face
  RCNM_DSID  =     10   ;

  PRIM_P     =     1    ;   // point feature
  PRIM_L     =     2    ;   // line feature
  PRIM_A     =     3    ;   // area feature
  PRIM_N     =     4    ;   // non-spatial feature

//=============================================================================
// T_S57Metadata
//=============================================================================

  constructor T_S57Metadata.Create ;
  begin
    {$IFNDEF OXYGENE}
    inherited Create;
    {$ENDIF}

    lstOC := TDictionary<Integer, T_S57ObjectClass>.Create ;
    lstAT := TDictionary<Integer, T_S57Attr>.Create ;

    initS57Metadata ;
    end ;

  procedure T_S57Metadata.doDestroy ;
  {$IFNDEF OXYGENE}
  var
    p  : TPair<Integer,T_S57ObjectClass>;
    a  : TPair<Integer,T_S57Attr>;
  {$ENDIF}
  begin
    for p in lstOC do begin
      FreeObjectNotNil(p.Value )  ;
    end;

    lstOC.Clear ;
    FreeObject( lstOC ) ;

    for a in lstAT do begin
      FreeObjectNotNil( a.Value ) ;
    end;

    lstAT.Clear ;
    FreeObject( lstAT ) ;

    // do nothing
    inherited ;
  end ;

  procedure T_S57Metadata.initS57Metadata ;

    function _oc(
      const _ic : Integer ;
      const _ac : String ;
      const _oc : String ;
      const _gt : Integer ;
      const _dp : Integer
    ) : T_S57ObjectClass ;
    begin
      Result := T_S57ObjectClass.Create ;
      Result.ic := _ic ;
      Result.ac := _ac ;
      Result.oc := _oc ;
      Result.gt := _gt ;
      Result.dp := _dp ;
    end ;

    function _a(
      const _ai : Integer ;
      const _an : String ;
      const _at : Char
    ) : T_S57Attr ;
    begin
      Result := T_S57Attr.Create ;
      Result.ai := _ai ;
      Result.an := _an ;
      Result.at := _at ;
    end ;

  begin
    lstOC.Add( 1,_oc(1,'ADMARE','Administration area (Named)',3,2 ) ) ;
    lstOC.Add( 10,_oc(10,'BERTHS','Berth',3,3 ) ) ;
    lstOC.Add( 100,_oc(100,'RADRNG','Radar range',3,3) ) ;
    lstOC.Add( 1003,_oc(1003,'ACHPNT','Anchor',1,8 ) ) ;
    lstOC.Add( 101,_oc(101,'RADRFL','Radar reflector',1,6 ) ) ;
    lstOC.Add( 1012,_oc(1012,'BUIREL','Building, religous',1,6 ) ) ;
    lstOC.Add( 102,_oc(102,'RADSTA','Radar station',1,5 ) ) ;
    lstOC.Add( 1027,_oc(1027,'CHNWIR','Chain/Wire',2,5 ) ) ;
    lstOC.Add( 103,_oc(103,'RTPBCN','Radar transponder beacon',1,6 ) ) ;
    lstOC.Add( 104,_oc(104,'RDOCAL','Radio calling-in point',2,6 ) ) ;
    lstOC.Add( 105,_oc(105,'RDOSTA','Radio station',1,4 ) ) ;
    lstOC.Add( 106,_oc(106,'RAILWY','Railway',2,4 ) ) ;
    lstOC.Add( 107,_oc(107,'RAPIDS','Rapids',3,3 ) ) ;
    lstOC.Add( 1075,_oc(1075,'_extgn','Light, extinguished',1,8) ) ;
    lstOC.Add( 108,_oc(108,'RCRTCL','Recommended route centerline',2,6 ) ) ;
    lstOC.Add( 1083,_oc(1083,'MONUMT','Monument',1,6 ) ) ;
    lstOC.Add( 109,_oc(109,'RECTRC','Recommended track',3,6 ) ) ;
    lstOC.Add( 11,_oc(11,'BRIDGE','Bridge',3,8 ) ) ;
    lstOC.Add( 110,_oc(110,'RCTLPT','Recommended Traffic Lane Part',3,4 ) ) ;
    lstOC.Add( 111,_oc(111,'RSCSTA','Rescue station',1,7 ) ) ;
    lstOC.Add( 112,_oc(112,'RESARE','Restricted area',3,5 ) ) ;
    lstOC.Add( 113,_oc(113,'RETRFL','Retro-reflector',1,6 ) ) ;
    lstOC.Add( 114,_oc(114,'RIVERS','River',3,2 ) ) ;
    lstOC.Add( 1144,_oc(1144,'TOWERS','Tower',1,6 ) ) ;
    lstOC.Add( 115,_oc(115,'RIVBNK','River bank',2,0 ) ) ;
    lstOC.Add( 1159,_oc(1159,'ZEMCNT','Zero meter contour',2,5 ) ) ;
    lstOC.Add( 116,_oc(116,'ROADWY','Road',3,4 ) ) ;
    lstOC.Add( 117,_oc(117,'RUNWAY','Runway',3,5 ) ) ;
    lstOC.Add( 118,_oc(118,'SNDWAV','Sand waves',3,4) ) ;
    lstOC.Add( 119,_oc(119,'SEAARE','Sea area / named water area',3,0) ) ;
    lstOC.Add( 12,_oc(12,'BUISGL','Building, single',3,4 ) ) ;
    lstOC.Add( 120,_oc(120,'SPLARE','Sea-plane landing area',3,4) ) ;
    lstOC.Add( 121,_oc(121,'SBDARE','Seabed area',3,3 ) ) ;
    lstOC.Add( 122,_oc(122,'SLCONS','Shoreline Construction',3,7 ) ) ;
    lstOC.Add( 123,_oc(123,'SISTAT','Signal station, traffic',1,7 ) ) ;
    lstOC.Add( 124,_oc(124,'SISTAW','Signal station, warning',1,7) ) ;
    lstOC.Add( 125,_oc(125,'SILTNK','Silo / tank',3,4 ) ) ;
    lstOC.Add( 126,_oc(126,'SLOTOP','Slope topline',2,4 ) ) ;
    lstOC.Add( 127,_oc(127,'SLOGRD','Sloping ground',3,3 ) ) ;
    lstOC.Add( 128,_oc(128,'SMCFAC','Small craft facility',3,4) ) ;
    lstOC.Add( 129,_oc(129,'SOUNDG','Sounding',1,6 ) ) ;
    lstOC.Add( 13,_oc(13,'BUAARE','Built-up area',3,3) ) ;
    lstOC.Add( 130,_oc(130,'SPRING','Spring',1,4 ) ) ;
    lstOC.Add( 131,_oc(131,'SQUARE','Square',0,0 ) ) ;
    lstOC.Add( 132,_oc(132,'STSLNE','Straight territorial sea baseline',2,3 ) ) ;
    lstOC.Add( 133,_oc(133,'SUBTLN','Submarine transit lane',3,4 ) ) ;
    lstOC.Add( 134,_oc(134,'SWPARE','Swept Area',3,4 ) ) ;
    lstOC.Add( 135,_oc(135,'TESARE','Territorial sea area',3,2 ) ) ;
    lstOC.Add( 136,_oc(136,'TS_PRH','Tidal stream - harmonic prediction',3,2 ) ) ;
    lstOC.Add( 137,_oc(137,'TS_PNH','Tidal stream - non-harmonic prediction',3,2 ) ) ;
    lstOC.Add( 138,_oc(138,'TS_PAD','Tidal stream panel data',3,2 ) ) ;
    lstOC.Add( 139,_oc(139,'TS_TIS','Tidal stream - time series',3,2) ) ;
    lstOC.Add( 14,_oc(14,'BOYCAR','Buoy, cardinal',1,8 ) ) ;
    lstOC.Add( 140,_oc(140,'T_HMON','Tide - harmonic prediction',3,2 ) ) ;
    lstOC.Add( 141,_oc(141,'T_NHMN','Tide - non-harmonic prediction',3,2 ) ) ;
    lstOC.Add( 142,_oc(142,'T_TIMS','Tidal stream - time series',3,2 ) ) ;
    lstOC.Add( 143,_oc(143,'TIDEWY','Tideway',3,7 ) ) ;
    lstOC.Add( 144,_oc(144,'TOPMAR','Top mark',1,4 ) ) ;
    lstOC.Add( 145,_oc(145,'TSELNE','Traffic Separation Line',2,8 ) ) ;
    lstOC.Add( 146,_oc(146,'TSSBND','Traffic Separation Scheme  Boundary',2,7 ) ) ;
    lstOC.Add( 147,_oc(147,'TSSCRS','Traffic Separation Scheme Crossing',3,6 ) ) ;
    lstOC.Add( 148,_oc(148,'TSSLPT','Traffic Separation Scheme  Lane part',3,6 ) ) ;
    lstOC.Add( 149,_oc(149,'TSSRON','Traffic Separation Scheme  Roundabout',3,6) ) ;
    lstOC.Add( 15,_oc(15,'BOYINB','Buoy, installation',1,8 ) ) ;
    lstOC.Add( 150,_oc(150,'TSEZNE','Traffic Separation Zone',3,4 ) ) ;
    lstOC.Add( 151,_oc(151,'TUNNEL','Tunnel',3,4 ) ) ;
    lstOC.Add( 152,_oc(152,'TWRTPT','Two-way route  part',3,4 ) ) ;
    lstOC.Add( 153,_oc(153,'UWTROC','Underwater rock / awash rock',1,4 ) ) ;
    lstOC.Add( 154,_oc(154,'UNSARE','Unsurveyed area',3,0 ) ) ;
    lstOC.Add( 155,_oc(155,'VEGATN','Vegetation',3,3 ) ) ;
    lstOC.Add( 156,_oc(156,'WATTUR','Water turbulence',3,4 ) ) ;
    lstOC.Add( 157,_oc(157,'WATFAL','Waterfall',2,3 ) ) ;
    lstOC.Add( 158,_oc(158,'WEDKLP','Weed/Kelp',3,3 ) ) ;
    lstOC.Add( 159,_oc(159,'WRECKS','Wreck',3,4 ) ) ;
    lstOC.Add( 16,_oc(16,'BOYISD','Buoy, isolated danger',1,8) ) ;
    lstOC.Add( 160,_oc(160,'TS_FEB','Tidal stream - flood/ebb',3,4 ) ) ;
    lstOC.Add( 17,_oc(17,'BOYLAT','Buoy, lateral',1,8 ) ) ;
    lstOC.Add( 17000,_oc(3,'ACHBRT','Anchor berth',3,5 ) ) ;
    lstOC.Add( 17001,_oc(4,'ACHARE','Anchorage area',3,3 ) ) ;
    lstOC.Add( 17002,_oc(24,'CANBNK','Canal bank',2,0 ) ) ;
    lstOC.Add( 17003,_oc(42,'DEPARE','Depth area',3,1 ) ) ;
    lstOC.Add( 17004,_oc(44,'DISMAR','Distance mark',1,7 ) ) ;
    lstOC.Add( 17005,_oc(112,'RESARE','Restricted area',3,5 ) ) ;
    lstOC.Add( 17006,_oc(115,'RIVBNK','River bank',2,0 ) ) ;
    lstOC.Add( 17007,_oc(123,'SISTAT','Signal station, traffic',1,7 ) ) ;
    lstOC.Add( 17008,_oc(124,'SISTAW','Signal station, warning',1,7 ) ) ;
    lstOC.Add( 17009,_oc(144,'TOPMAR','Top mark',1,4 ) ) ;
    lstOC.Add( 17010,_oc(10,'BERTHS','Berth',3,3 ) ) ;
    lstOC.Add( 17011,_oc(11,'BRIDGE','Bridge',3,8 ) ) ;
    lstOC.Add( 17012,_oc(21,'CBLOHD','Cable, overhead',2,8 ) ) ;
    lstOC.Add( 17013,_oc(53,'FERYRT','Ferry route',3,3 ) ) ;
    lstOC.Add( 17014,_oc(63,'HRBARE','Harbour area (administrative)',3,2 ) ) ;
    lstOC.Add( 17015,_oc(64,'HRBFAC','Harbour facility',3,4 ) ) ;
    lstOC.Add( 17016,_oc(79,'LOKBSN','Lock basin',3,2 ) ) ;
    lstOC.Add( 17017,_oc(104,'RDOCAL','Radio calling-in point',2,6 ) ) ;
    lstOC.Add( 17018,_oc(306,'M_NSYS','Navigational system of marks',3,4 ) ) ;
    lstOC.Add( 17019,_oc(36,'CURENT','Current - non - gravitational',1,5 ) ) ;
    lstOC.Add( 17020,_oc(65,'HULKES','Hulk',3,5 ) ) ;
    lstOC.Add( 17021,_oc(95,'PONTON','Pontoon',3,5 ) ) ;
    lstOC.Add( 17022,_oc(309,'M_SDAT','Sounding datum',3,0 ) ) ;
    lstOC.Add( 17023,_oc(312,'M_VDAT','Vertical datum of data',3,0 ) ) ;
    lstOC.Add( 17024,_oc(93,'PIPOHD','Pipeline, overhead',2,8 ) ) ;
    lstOC.Add( 17025,_oc(57,'FLODOC','Floating dock',3,5 ) ) ;
    lstOC.Add( 17027,_oc(28,'CHKPNT','Checkpoint',3,4 ) ) ;
    lstOC.Add( 17028,_oc(7,'BCNLAT','Beacon, lateral',1,8 ) ) ;
    lstOC.Add( 17029,_oc(17,'BOYLAT','Buoy, lateral',1,8 ) ) ;
    lstOC.Add( 17030,_oc(35,'CRANES','Crane',3,4 ) ) ;
    lstOC.Add( 17031,_oc(61,'GATCON','Gate',3,8 ) ) ;
    lstOC.Add( 17032,_oc(122,'SLCONS','Shoreline Construction',3,7 ) ) ;
    lstOC.Add( 17033,_oc(153,'UWTROC','Underwater rock / awash rock',1,4 ) ) ;
    lstOC.Add( 17034,_oc(34,'CONVYR','Conveyor',3,8 ) ) ;
    lstOC.Add( 17050,_oc(17050,'notmrk','Notice mark',1,8) ) ;
    lstOC.Add( 17051,_oc(17051,'wtwaxs','waterway axis',2,3 ) ) ;
    lstOC.Add( 17052,_oc(17052,'wtwprf','waterway profile',2,3 ) ) ;
    lstOC.Add( 17053,_oc(17053,'brgare','Bridge area',0,0 ) ) ;
    lstOC.Add( 17054,_oc(17054,'bunsta','Bunker station',3,6 ) ) ;
    lstOC.Add( 17055,_oc(17055,'comare','Communication area',0,0 ) ) ;
    lstOC.Add( 17056,_oc(17056,'hrbbsn','Harbour basin',3,2 ) ) ;
    lstOC.Add( 17057,_oc(17057,'lokare','Lock area',0,0 ) ) ;
    lstOC.Add( 17058,_oc(17058,'lkbspt','Lock basin part',3,2 ) ) ;
    lstOC.Add( 17059,_oc(17059,'prtare','port area',0,0 ) ) ;
    lstOC.Add( 17060,_oc(17060,'bcnwtw','Beacon water-way',1,0 ) ) ;
    lstOC.Add( 17061,_oc(17061,'boywtw','Buoy water-way',1,0 ) ) ;
    lstOC.Add( 17062,_oc(17062,'refdmp','refuse dump',1,7) ) ;
    lstOC.Add( 17063,_oc(17063,'rtplpt','Route planning point',0,0 ) ) ;
    lstOC.Add( 17064,_oc(17064,'termnl','terminal',3,0 ) ) ;
    lstOC.Add( 17065,_oc(17065,'trnbsn','turning basin',3,3 ) ) ;
    lstOC.Add( 17066,_oc(17066,'wtware','Waterway area',0,0) ) ;
    lstOC.Add( 17067,_oc(17067,'wtwgag','Waterway gauge',1,7 ) ) ;
    lstOC.Add( 17068,_oc(17068,'tisdge','Time Schedule - in general',0,0) ) ;
    lstOC.Add( 17069,_oc(17069,'vehtrf','Vehicle transfer',3,6) ) ;
    lstOC.Add( 17070,_oc(17070,'excnst','Exceptional navigation strcuture',3,6 ) ) ;
    lstOC.Add( 18,_oc(18,'BOYSAW','Buoy, safe water',1,8) ) ;
    lstOC.Add( 18001,_oc(18001,'lg_sdm','Maximum permitted ship dimensions',0,0 ) ) ;
    lstOC.Add( 18002,_oc(18002,'lg_vsp','Maximum permitted vessel speed',0,0 ) ) ;
    lstOC.Add( 19,_oc(19,'BOYSPP','Buoy, special purpose/general',1,8 ) ) ;
    lstOC.Add( 2,_oc(2,'AIRARE','Airport / airfield',3,2 ) ) ;
    lstOC.Add( 20,_oc(20,'CBLARE','Cable area',3,3 ) ) ;
    lstOC.Add( 21,_oc(21,'CBLOHD','Cable, overhead',2,8 ) ) ;
    lstOC.Add( 22,_oc(22,'CBLSUB','Cable, submarine',2,6 ) ) ;
    lstOC.Add( 23,_oc(23,'CANALS','Canal',3,2 ) ) ;
    lstOC.Add( 24,_oc(24,'CANBNK','Canal bank',2,0 ) ) ;
    lstOC.Add( 25,_oc(25,'CTSARE','Cargo transshipment area',3,3 ) ) ;
    lstOC.Add( 26,_oc(26,'CAUSWY','Causeway',3,5) ) ;
    lstOC.Add( 27,_oc(27,'CTNARE','Caution area',3,3 ) ) ;
    lstOC.Add( 28,_oc(28,'CHKPNT','Checkpoint',3,4 ) ) ;
    lstOC.Add( 29,_oc(29,'CGUSTA','Coastguard station',1,7 ) ) ;
    lstOC.Add( 3,_oc(3,'ACHBRT','Anchor berth',3,5 ) ) ;
    lstOC.Add( 30,_oc(30,'COALNE','Coastline',2,7) ) ;
    lstOC.Add( 300,_oc(300,'M_ACCY','Accuracy of data',3,0 ) ) ;
    lstOC.Add( 301,_oc(301,'M_CSCL','Compilation scale of data',3,1 ) ) ;
    lstOC.Add( 302,_oc(302,'M_COVR','Coverage',3,1 ) ) ;
    lstOC.Add( 303,_oc(303,'M_HDAT','Horizontal datum of data',0,0 ) ) ;
    lstOC.Add( 304,_oc(304,'M_HOPA','Horizontal datum shift parameters',3,0 ) ) ;
    lstOC.Add( 305,_oc(305,'M_NPUB','Nautical publication information',3,0 ) ) ;
    lstOC.Add( 306,_oc(306,'M_NSYS','Navigational system of marks',3,4) ) ;
    lstOC.Add( 307,_oc(307,'M_PROD','Production information',3,0 ) ) ;
    lstOC.Add( 308,_oc(308,'M_QUAL','Quality of data',3,4) ) ;
    lstOC.Add( 309,_oc(309,'M_SDAT','Sounding datum',3,0 ) ) ;
    lstOC.Add( 31,_oc(31,'CONZNE','Contiguous zone',3,2 ) ) ;
    lstOC.Add( 310,_oc(310,'M_SREL','Survey reliability',3,0 ) ) ;
    lstOC.Add( 311,_oc(311,'M_UNIT','Units of measurement of data',0,0 ) ) ;
    lstOC.Add( 312,_oc(312,'M_VDAT','Vertical datum of data',3,0 ) ) ;
    lstOC.Add( 32,_oc(32,'COSARE','Continental shelf area',3,2 ) ) ;
    lstOC.Add( 33,_oc(33,'CTRPNT','Control point',1,4 ) ) ;
    lstOC.Add( 34,_oc(34,'CONVYR','Conveyor',3,8 ) ) ;
    lstOC.Add( 35,_oc(35,'CRANES','Crane',3,4) ) ;
    lstOC.Add( 36,_oc(36,'CURENT','Current - non - gravitational',1,5 ) ) ;
    lstOC.Add( 37,_oc(37,'CUSZNE','Custom zone',3,2 ) ) ;
    lstOC.Add( 38,_oc(38,'DAMCON','Dam',3,6 ) ) ;
    lstOC.Add( 39,_oc(39,'DAYMAR','Daymark',1,7 ) ) ;
    lstOC.Add( 4,_oc(4,'ACHARE','Anchorage area',3,3 ) ) ;
    lstOC.Add( 40,_oc(40,'DWRTCL','Deep water route centerline',2,6 ) ) ;
    lstOC.Add( 400,_oc(400,'C_AGGR','Aggregation',0,0) ) ;
    lstOC.Add( 401,_oc(401,'C_ASSO','Association',0,0 ) ) ;
    lstOC.Add( 402,_oc(402,'C_STAC','Stacked on/stacked under',0,0 ) ) ;
    lstOC.Add( 41,_oc(41,'DWRTPT','Deep water route part',3,4 ) ) ;
    lstOC.Add( 42,_oc(42,'DEPARE','Depth area',3,1 ) ) ;
    lstOC.Add( 43,_oc(43,'DEPCNT','Depth contour',2,5 ) ) ;
    lstOC.Add( 44,_oc(44,'DISMAR','Distance mark',1,7) ) ;
    lstOC.Add( 45,_oc(45,'DOCARE','Dock area',3,2 ) ) ;
    lstOC.Add( 46,_oc(46,'DRGARE','Dredged area',3,1 ) ) ;
    lstOC.Add( 47,_oc(47,'DRYDOC','Dry dock',3,4 ) ) ;
    lstOC.Add( 48,_oc(48,'DMPGRD','Dumping ground',3,3 ) ) ;
    lstOC.Add( 49,_oc(49,'DYKCON','Dyke',3,3 ) ) ;
    lstOC.Add( 5,_oc(5,'BCNCAR','Beacon, cardinal',1,8 ) ) ;
    lstOC.Add( 50,_oc(50,'EXEZNE','Exclusive Economic Zone',3,2 ) ) ;
    lstOC.Add( 500,_oc(500,'$AREAS','Cartographic area',3,2 ) ) ;
    lstOC.Add( 501,_oc(501,'$LINES','Cartographic line',2,6 ) ) ;
    lstOC.Add( 502,_oc(502,'$CSYMB','Cartographic symbol',1,5 ) ) ;
    lstOC.Add( 503,_oc(503,'$COMPS','Compass',0,0 ) ) ;
    lstOC.Add( 504,_oc(504,'$TEXTS','Text',3,7 ) ) ;
    lstOC.Add( 51,_oc(51,'FAIRWY','Fairway',3,4 ) ) ;
    lstOC.Add( 52,_oc(52,'FNCLNE','Fence/wall',2,3 ) ) ;
    lstOC.Add( 53,_oc(53,'FERYRT','Ferry route',3,3 ) ) ;
    lstOC.Add( 54,_oc(54,'FSHZNE','Fishery zone',3,2 ) ) ;
    lstOC.Add( 55,_oc(55,'FSHFAC','Fishing facility',3,4 ) ) ;
    lstOC.Add( 56,_oc(56,'FSHGRD','Fishing ground',3,3 ) ) ;
    lstOC.Add( 57,_oc(57,'FLODOC','Floating dock',3,5 ) ) ;
    lstOC.Add( 58,_oc(58,'FOGSIG','Fog signal',1,6 ) ) ;
    lstOC.Add( 59,_oc(59,'FORSTC','Fortified structure',3,4) ) ;
    lstOC.Add( 6,_oc(6,'BCNISD','Beacon, isolated danger',1,8 ) ) ;
    lstOC.Add( 60,_oc(60,'FRPARE','Free port area',3,2 ) ) ;
    lstOC.Add( 61,_oc(61,'GATCON','Gate',3,8 ) ) ;
    lstOC.Add( 62,_oc(62,'GRIDRN','Gridiron',3,5 ) ) ;
    lstOC.Add( 63,_oc(63,'HRBARE','Harbour area (administrative)',3,2) ) ;
    lstOC.Add( 64,_oc(64,'HRBFAC','Harbour facility',3,4 ) ) ;
    lstOC.Add( 65,_oc(65,'HULKES','Hulk',3,5 ) ) ;
    lstOC.Add( 66,_oc(66,'ICEARE','Ice area',3,3 ) ) ;
    lstOC.Add( 67,_oc(67,'ICNARE','Incineration area',3,3) ) ;
    lstOC.Add( 68,_oc(68,'ISTZNE','Inshore traffic zone',3,5 ) ) ;
    lstOC.Add( 69,_oc(69,'LAKARE','Lake',3,2 ) ) ;
    lstOC.Add( 7,_oc(7,'BCNLAT','Beacon, lateral',1,8) ) ;
    lstOC.Add( 70,_oc(70,'LAKSHR','Lake shore',2,5 ) ) ;
    lstOC.Add( 71,_oc(71,'LNDARE','Land area',3,1 ) ) ;
    lstOC.Add( 72,_oc(72,'LNDELV','Land elevation',2,4 ) ) ;
    lstOC.Add( 73,_oc(73,'LNDRGN','Land region',3,3 ) ) ;
    lstOC.Add( 74,_oc(74,'LNDMRK','Landmark',3,4 ) ) ;
    lstOC.Add( 75,_oc(75,'LIGHTS','Light',1,8 ) ) ;
    lstOC.Add( 76,_oc(76,'LITFLT','Light float',1,8) ) ;
    lstOC.Add( 77,_oc(77,'LITVES','Light vessel',1,8 ) ) ;
    lstOC.Add( 78,_oc(78,'LOCMAG','Local magnetic anomaly',3,4 ) ) ;
    lstOC.Add( 79,_oc(79,'LOKBSN','Lock basin',3,2 ) ) ;
    lstOC.Add( 8,_oc(8,'BCNSAW','Beacon, safe water',1,8 ) ) ;
    lstOC.Add( 80,_oc(80,'LOGPON','Log pond',3,5 ) ) ;
    lstOC.Add( 81,_oc(81,'MAGVAR','Magnetic variation',3,4) ) ;
    lstOC.Add( 82,_oc(82,'MARCUL','Marine farm/culture',3,3 ) ) ;
    lstOC.Add( 83,_oc(83,'MIPARE','Military practice area',3,4 ) ) ;
    lstOC.Add( 84,_oc(84,'MORFAC','Mooring/warping facility',3,6 ) ) ;
    lstOC.Add( 85,_oc(85,'NAVLNE','Navigation line',2,4 ) ) ;
    lstOC.Add( 86,_oc(86,'OBSTRN','Obstruction',3,4 ) ) ;
    lstOC.Add( 87,_oc(87,'OFSPLF','Offshore platform',3,5 ) ) ;
    lstOC.Add( 88,_oc(88,'OSPARE','Offshore production area',3,4 ) ) ;
    lstOC.Add( 89,_oc(89,'OILBAR','Oil barrier',2,4 ) ) ;
    lstOC.Add( 9,_oc(9,'BCNSPP','Beacon, special purpose/general',1,8 ) ) ;
    lstOC.Add( 90,_oc(90,'PILPNT','Pile',1,5 ) ) ;
    lstOC.Add( 91,_oc(91,'PILBOP','Pilot boarding place',3,4 ) ) ;
    lstOC.Add( 92,_oc(92,'PIPARE','Pipeline area',3,3 ) ) ;
    lstOC.Add( 93,_oc(93,'PIPOHD','Pipeline, overhead',2,8 ) ) ;
    lstOC.Add( 94,_oc(94,'PIPSOL','Pipeline, submarine/on land',2,6 ) ) ;
    lstOC.Add( 95,_oc(95,'PONTON','Pontoon',3,5) ) ;
    lstOC.Add( 96,_oc(96,'PRCARE','Precautionary area',3,4 ) ) ;
    lstOC.Add( 97,_oc(97,'PRDARE','Production / storage area',3,4 ) ) ;
    lstOC.Add( 98,_oc(98,'PYLONS','Pylon/bridge support',3,8 ) ) ;
    lstOC.Add( 99,_oc(99,'RADLNE','Radar line',2,6 ) ) ;

    lstAT.Add( 1,_a(1,'AGENCY','F') ) ;
    lstAT.Add( 2,_a(2,'BCNSHP','F') ) ;
    lstAT.Add( 3,_a(3,'BUISHP','F') ) ;
    lstAT.Add( 4,_a(4,'BOYSHP','F') ) ;
    lstAT.Add( 5,_a(5,'BURDEP','F') ) ;
    lstAT.Add( 6,_a(6,'CALSGN','F') ) ;
    lstAT.Add( 7,_a(7,'CATAIR','F') ) ;
    lstAT.Add( 8,_a(8,'CATACH','F') ) ;
    lstAT.Add( 9,_a(9,'CATBRG','F') ) ;
    lstAT.Add( 10,_a(10,'CATBUA','F') ) ;
    lstAT.Add( 11,_a(11,'CATCBL','F') ) ;
    lstAT.Add( 12,_a(12,'CATCAN','F') ) ;
    lstAT.Add( 13,_a(13,'CATCAM','F') ) ;
    lstAT.Add( 14,_a(14,'CATCHP','F') ) ;
    lstAT.Add( 15,_a(15,'CATCOA','F') ) ;
    lstAT.Add( 16,_a(16,'CATCTR','F') ) ;
    lstAT.Add( 17,_a(17,'CATCON','F') ) ;
    lstAT.Add( 18,_a(18,'CATCOV','F') ) ;
    lstAT.Add( 19,_a(19,'CATCRN','F') ) ;
    lstAT.Add( 20,_a(20,'CATDAM','F') ) ;
    lstAT.Add( 21,_a(21,'CATDIS','F') ) ;
    lstAT.Add( 22,_a(22,'CATDOC','F') ) ;
    lstAT.Add( 23,_a(23,'CATDPG','F') ) ;
    lstAT.Add( 24,_a(24,'CATFNC','F') ) ;
    lstAT.Add( 25,_a(25,'CATFRY','F') ) ;
    lstAT.Add( 26,_a(26,'CATFIF','F') ) ;
    lstAT.Add( 27,_a(27,'CATFOG','F') ) ;
    lstAT.Add( 28,_a(28,'CATFOR','F') ) ;
    lstAT.Add( 29,_a(29,'CATGAT','F') ) ;
    lstAT.Add( 30,_a(30,'CATHAF','F') ) ;
    lstAT.Add( 31,_a(31,'CATHLK','F') ) ;
    lstAT.Add( 32,_a(32,'CATICE','F') ) ;
    lstAT.Add( 33,_a(33,'CATINB','F') ) ;
    lstAT.Add( 34,_a(34,'CATLND','F') ) ;
    lstAT.Add( 35,_a(35,'CATLMK','F') ) ;
    lstAT.Add( 36,_a(36,'CATLAM','F') ) ;
    lstAT.Add( 37,_a(37,'CATLIT','F') ) ;
    lstAT.Add( 38,_a(38,'CATMFA','F') ) ;
    lstAT.Add( 39,_a(39,'CATMPA','F') ) ;
    lstAT.Add( 40,_a(40,'CATMOR','F') ) ;
    lstAT.Add( 41,_a(41,'CATNAV','F') ) ;
    lstAT.Add( 42,_a(42,'CATOBS','F') ) ;
    lstAT.Add( 43,_a(43,'CATOFP','F') ) ;
    lstAT.Add( 44,_a(44,'CATOLB','F') ) ;
    lstAT.Add( 45,_a(45,'CATPLE','F') ) ;
    lstAT.Add( 46,_a(46,'CATPIL','F') ) ;
    lstAT.Add( 47,_a(47,'CATPIP','F') ) ;
    lstAT.Add( 48,_a(48,'CATPRA','F') ) ;
    lstAT.Add( 49,_a(49,'CATPYL','F') ) ;
    lstAT.Add( 50,_a(50,'CATQUA','F') ) ;
    lstAT.Add( 51,_a(51,'CATRAS','F') ) ;
    lstAT.Add( 52,_a(52,'CATRTB','F') ) ;
    lstAT.Add( 53,_a(53,'CATROS','F') ) ;
    lstAT.Add( 54,_a(54,'CATTRK','F') ) ;
    lstAT.Add( 55,_a(55,'CATRSC','F') ) ;
    lstAT.Add( 56,_a(56,'CATREA','F') ) ;
    lstAT.Add( 57,_a(57,'CATROD','F') ) ;
    lstAT.Add( 58,_a(58,'CATRUN','F') ) ;
    lstAT.Add( 59,_a(59,'CATSEA','F') ) ;
    lstAT.Add( 60,_a(60,'CATSLC','F') ) ;
    lstAT.Add( 61,_a(61,'CATSIT','F') ) ;
    lstAT.Add( 62,_a(62,'CATSIW','F') ) ;
    lstAT.Add( 63,_a(63,'CATSIL','F') ) ;
    lstAT.Add( 64,_a(64,'CATSLO','F') ) ;
    lstAT.Add( 65,_a(65,'CATSCF','F') ) ;
    lstAT.Add( 66,_a(66,'CATSPM','F') ) ;
    lstAT.Add( 67,_a(67,'CATTSS','F') ) ;
    lstAT.Add( 68,_a(68,'CATVEG','F') ) ;
    lstAT.Add( 69,_a(69,'CATWAT','F') ) ;
    lstAT.Add( 70,_a(70,'CATWED','F') ) ;
    lstAT.Add( 71,_a(71,'CATWRK','F') ) ;
    lstAT.Add( 72,_a(72,'CATZOC','F') ) ;
    lstAT.Add( 73,_a(73,'$SPACE','$') ) ;
    lstAT.Add( 74,_a(74,'$CHARS','$') ) ;
    lstAT.Add( 75,_a(75,'COLOUR','F') ) ;
    lstAT.Add( 76,_a(76,'COLPAT','F') ) ;
    lstAT.Add( 77,_a(77,'COMCHA','F') ) ;
    lstAT.Add( 78,_a(78,'$CSIZE','$') ) ;
    lstAT.Add( 79,_a(79,'CPDATE','F') ) ;
    lstAT.Add( 80,_a(80,'CSCALE','F') ) ;
    lstAT.Add( 81,_a(81,'CONDTN','F') ) ;
    lstAT.Add( 82,_a(82,'CONRAD','F') ) ;
    lstAT.Add( 83,_a(83,'CONVIS','F') ) ;
    lstAT.Add( 84,_a(84,'CURVEL','F') ) ;
    lstAT.Add( 85,_a(85,'DATEND','F') ) ;
    lstAT.Add( 86,_a(86,'DATSTA','F') ) ;
    lstAT.Add( 87,_a(87,'DRVAL1','F') ) ;
    lstAT.Add( 88,_a(88,'DRVAL2','F') ) ;
    lstAT.Add( 89,_a(89,'DUNITS','F') ) ;
    lstAT.Add( 90,_a(90,'ELEVAT','F') ) ;
    lstAT.Add( 91,_a(91,'ESTRNG','F') ) ;
    lstAT.Add( 92,_a(92,'EXCLIT','F') ) ;
    lstAT.Add( 93,_a(93,'EXPSOU','F') ) ;
    lstAT.Add( 94,_a(94,'FUNCTN','F') ) ;
    lstAT.Add( 95,_a(95,'HEIGHT','F') ) ;
    lstAT.Add( 96,_a(96,'HUNITS','F') ) ;
    lstAT.Add( 97,_a(97,'HORACC','F') ) ;
    lstAT.Add( 98,_a(98,'HORCLR','F') ) ;
    lstAT.Add( 99,_a(99,'HORLEN','F') ) ;
    lstAT.Add( 100,_a(100,'HORWID','F') ) ;
    lstAT.Add( 101,_a(101,'ICEFAC','F') ) ;
    lstAT.Add( 102,_a(102,'INFORM','F') ) ;
    lstAT.Add( 103,_a(103,'JRSDTN','F') ) ;
    lstAT.Add( 104,_a(104,'$JUSTH','$') ) ;
    lstAT.Add( 105,_a(105,'$JUSTV','$') ) ;
    lstAT.Add( 106,_a(106,'LIFCAP','F') ) ;
    lstAT.Add( 107,_a(107,'LITCHR','F') ) ;
    lstAT.Add( 108,_a(108,'LITVIS','F') ) ;
    lstAT.Add( 109,_a(109,'MARSYS','F') ) ;
    lstAT.Add( 110,_a(110,'MLTYLT','F') ) ;
    lstAT.Add( 111,_a(111,'NATION','F') ) ;
    lstAT.Add( 112,_a(112,'NATCON','F') ) ;
    lstAT.Add( 113,_a(113,'NATSUR','F') ) ;
    lstAT.Add( 114,_a(114,'NATQUA','F') ) ;
    lstAT.Add( 115,_a(115,'NMDATE','F') ) ;
    lstAT.Add( 116,_a(116,'OBJNAM','F') ) ;
    lstAT.Add( 117,_a(117,'ORIENT','F') ) ;
    lstAT.Add( 118,_a(118,'PEREND','F') ) ;
    lstAT.Add( 119,_a(119,'PERSTA','F') ) ;
    lstAT.Add( 120,_a(120,'PICREP','F') ) ;
    lstAT.Add( 121,_a(121,'PILDST','F') ) ;
    lstAT.Add( 122,_a(122,'PRCTRY','F') ) ;
    lstAT.Add( 123,_a(123,'PRODCT','F') ) ;
    lstAT.Add( 124,_a(124,'PUBREF','F') ) ;
    lstAT.Add( 125,_a(125,'QUASOU','F') ) ;
    lstAT.Add( 126,_a(126,'RADWAL','F') ) ;
    lstAT.Add( 127,_a(127,'RADIUS','F') ) ;
    lstAT.Add( 128,_a(128,'RECDAT','F') ) ;
    lstAT.Add( 129,_a(129,'RECIND','F') ) ;
    lstAT.Add( 130,_a(130,'RYRMGV','F') ) ;
    lstAT.Add( 131,_a(131,'RESTRN','F') ) ;
    lstAT.Add( 132,_a(132,'SCAMAX','F') ) ;
    lstAT.Add( 133,_a(133,'SCAMIN','F') ) ;
    lstAT.Add( 134,_a(134,'SCVAL1','F') ) ;
    lstAT.Add( 135,_a(135,'SCVAL2','F') ) ;
    lstAT.Add( 136,_a(136,'SECTR1','F') ) ;
    lstAT.Add( 137,_a(137,'SECTR2','F') ) ;
    lstAT.Add( 138,_a(138,'SHIPAM','F') ) ;
    lstAT.Add( 139,_a(139,'SIGFRQ','F') ) ;
    lstAT.Add( 140,_a(140,'SIGGEN','F') ) ;
    lstAT.Add( 141,_a(141,'SIGGRP','F') ) ;
    lstAT.Add( 142,_a(142,'SIGPER','F') ) ;
    lstAT.Add( 143,_a(143,'SIGSEQ','F') ) ;
    lstAT.Add( 144,_a(144,'SOUACC','F') ) ;
    lstAT.Add( 145,_a(145,'SDISMX','F') ) ;
    lstAT.Add( 146,_a(146,'SDISMN','F') ) ;
    lstAT.Add( 147,_a(147,'SORDAT','F') ) ;
    lstAT.Add( 148,_a(148,'SORIND','F') ) ;
    lstAT.Add( 149,_a(149,'STATUS','F') ) ;
    lstAT.Add( 150,_a(150,'SURATH','F') ) ;
    lstAT.Add( 151,_a(151,'SUREND','F') ) ;
    lstAT.Add( 152,_a(152,'SURSTA','F') ) ;
    lstAT.Add( 153,_a(153,'SURTYP','F') ) ;
    lstAT.Add( 154,_a(154,'$SCALE','$') ) ;
    lstAT.Add( 155,_a(155,'$SCODE','$') ) ;
    lstAT.Add( 156,_a(156,'TECSOU','F') ) ;
    lstAT.Add( 157,_a(157,'$TXSTR','$') ) ;
    lstAT.Add( 158,_a(158,'TXTDSC','F') ) ;
    lstAT.Add( 159,_a(159,'TS_TSP','F') ) ;
    lstAT.Add( 160,_a(160,'TS_TSV','F') ) ;
    lstAT.Add( 161,_a(161,'T_ACWL','F') ) ;
    lstAT.Add( 162,_a(162,'T_HWLW','F') ) ;
    lstAT.Add( 163,_a(163,'T_MTOD','F') ) ;
    lstAT.Add( 164,_a(164,'T_THDF','F') ) ;
    lstAT.Add( 165,_a(165,'T_TINT','F') ) ;
    lstAT.Add( 166,_a(166,'T_TSVL','F') ) ;
    lstAT.Add( 167,_a(167,'T_VAHC','F') ) ;
    lstAT.Add( 168,_a(168,'TIMEND','F') ) ;
    lstAT.Add( 169,_a(169,'TIMSTA','F') ) ;
    lstAT.Add( 170,_a(170,'$TINTS','$') ) ;
    lstAT.Add( 171,_a(171,'TOPSHP','F') ) ;
    lstAT.Add( 172,_a(172,'TRAFIC','F') ) ;
    lstAT.Add( 173,_a(173,'VALACM','F') ) ;
    lstAT.Add( 174,_a(174,'VALDCO','F') ) ;
    lstAT.Add( 175,_a(175,'VALLMA','F') ) ;
    lstAT.Add( 176,_a(176,'VALMAG','F') ) ;
    lstAT.Add( 177,_a(177,'VALMXR','F') ) ;
    lstAT.Add( 178,_a(178,'VALNMR','F') ) ;
    lstAT.Add( 179,_a(179,'VALSOU','F') ) ;
    lstAT.Add( 180,_a(180,'VERACC','F') ) ;
    lstAT.Add( 181,_a(181,'VERCLR','F') ) ;
    lstAT.Add( 182,_a(182,'VERCCL','F') ) ;
    lstAT.Add( 183,_a(183,'VERCOP','F') ) ;
    lstAT.Add( 184,_a(184,'VERCSA','F') ) ;
    lstAT.Add( 185,_a(185,'VERDAT','F') ) ;
    lstAT.Add( 186,_a(186,'VERLEN','F') ) ;
    lstAT.Add( 187,_a(187,'WATLEV','F') ) ;
    lstAT.Add( 188,_a(188,'CAT_TS','F') ) ;
    lstAT.Add( 189,_a(189,'PUNITS','F') ) ;
    lstAT.Add( 300,_a(300,'NINFOM','N') ) ;
    lstAT.Add( 301,_a(301,'NOBJNM','N') ) ;
    lstAT.Add( 302,_a(302,'NPLDST','N') ) ;
    lstAT.Add( 303,_a(303,'$NTXST','N') ) ;
    lstAT.Add( 304,_a(304,'NTXTDS','N') ) ;
    lstAT.Add( 400,_a(400,'HORDAT','S') ) ;
    lstAT.Add( 401,_a(401,'POSACC','S') ) ;
    lstAT.Add( 402,_a(402,'QUAPOS','S') ) ;
    lstAT.Add( 17000,_a(17000,'catach','F') ) ;
    lstAT.Add( 17001,_a(17001,'catdis','F') ) ;
    lstAT.Add( 17002,_a(17002,'catsit','F') ) ;
    lstAT.Add( 17003,_a(17003,'catsiw','F') ) ;
    lstAT.Add( 17004,_a(17004,'restrn','F') ) ;
    lstAT.Add( 17005,_a(17005,'verdat','F') ) ;
    lstAT.Add( 17006,_a(17006,'catbrg','F') ) ;
    lstAT.Add( 17007,_a(17007,'catfry','F') ) ;
    lstAT.Add( 17008,_a(17008,'cathaf','F') ) ;
    lstAT.Add( 17009,_a(17009,'marsys','F') ) ;
    lstAT.Add( 17010,_a(17010,'catchp','F') ) ;
    lstAT.Add( 17011,_a(17011,'catlam','F') ) ;
    lstAT.Add( 17012,_a(17012,'catslc','F') ) ;
    lstAT.Add( 17050,_a(17050,'addmrk','F') ) ;
    lstAT.Add( 17051,_a(17051,'catbnk','F') ) ;
    lstAT.Add( 17052,_a(17052,'catnmk','F') ) ;
    lstAT.Add( 17055,_a(17055,'clsdng','F') ) ;
    lstAT.Add( 17056,_a(17056,'dirimp','F') ) ;
    lstAT.Add( 17057,_a(17057,'disbk1','F') ) ;
    lstAT.Add( 17058,_a(17058,'disbk2','F') ) ;
    lstAT.Add( 17059,_a(17059,'disipu','F') ) ;
    lstAT.Add( 17060,_a(17060,'disipd','F') ) ;
    lstAT.Add( 17061,_a(17061,'eleva1','F') ) ;
    lstAT.Add( 17062,_a(17062,'eleva2','F') ) ;
    lstAT.Add( 17063,_a(17063,'fnctnm','F') ) ;
    lstAT.Add( 17064,_a(17064,'wtwdis','F') ) ;
    lstAT.Add( 17065,_a(17065,'bunves','F') ) ;
    lstAT.Add( 17066,_a(17066,'catbrt','F') ) ;
    lstAT.Add( 17067,_a(17067,'catbun','F') ) ;
    lstAT.Add( 17068,_a(17068,'catccl','F') ) ;
    lstAT.Add( 17069,_a(17069,'catcom','F') ) ;
    lstAT.Add( 17070,_a(17070,'cathbr','F') ) ;
    lstAT.Add( 17071,_a(17071,'catrfd','F') ) ;
    lstAT.Add( 17072,_a(17072,'cattml','F') ) ;
    lstAT.Add( 17073,_a(17073,'comctn','F') ) ;
    lstAT.Add( 17074,_a(17074,'horcll','F') ) ;
    lstAT.Add( 17075,_a(17075,'horclw','F') ) ;
    lstAT.Add( 17076,_a(17076,'trshgd','F') ) ;
    lstAT.Add( 17077,_a(17077,'unlocd','F') ) ;
    lstAT.Add( 17078,_a(17078,'catgag','F') ) ;
    lstAT.Add( 17080,_a(17080,'higwat','F') ) ;
    lstAT.Add( 17081,_a(17081,'hignam','F') ) ;
    lstAT.Add( 17082,_a(17082,'lowwat','F') ) ;
    lstAT.Add( 17083,_a(17083,'lownam','F') ) ;
    lstAT.Add( 17084,_a(17084,'meawat','F') ) ;
    lstAT.Add( 17085,_a(17085,'meanam','F') ) ;
    lstAT.Add( 17086,_a(17086,'othwat','F') ) ;
    lstAT.Add( 17087,_a(17087,'othnam','F') ) ;
    lstAT.Add( 17088,_a(17088,'reflev','F') ) ;
    lstAT.Add( 17089,_a(17089,'sdrlev','F') ) ;
    lstAT.Add( 17090,_a(17090,'vcrlev','F') ) ;
    lstAT.Add( 17091,_a(17091,'catvtr','F') ) ;
    lstAT.Add( 17092,_a(17092,'cattab','F') ) ;
    lstAT.Add( 17093,_a(17093,'schref','F') ) ;
    lstAT.Add( 17094,_a(17094,'useshp','F') ) ;
    lstAT.Add( 17095,_a(17095,'curvhw','F') ) ;
    lstAT.Add( 17096,_a(17096,'curvlw','F') ) ;
    lstAT.Add( 17097,_a(17097,'curvmw','F') ) ;
    lstAT.Add( 17098,_a(17098,'curvow','F') ) ;
    lstAT.Add( 17099,_a(17099,'aptref','F') ) ;
    lstAT.Add( 17100,_a(17100,'catexs','F') ) ;
    lstAT.Add( 17101,_a(17101,'catcbl','F') ) ;
    lstAT.Add( 17102,_a(17102,'cathlk','F') ) ;
    lstAT.Add( 17103,_a(17103,'hunits','F') ) ;
    lstAT.Add( 17104,_a(17104,'watlev','F') ) ;
    lstAT.Add( 17112,_a(17112,'catwwm','F') ) ;
    lstAT.Add( 18001,_a(18001,'lg_spd','F') ) ;
    lstAT.Add( 18002,_a(18002,'lg_spr','F') ) ;
    lstAT.Add( 18003,_a(18003,'lg_bme','F') ) ;
    lstAT.Add( 18004,_a(18004,'lg_lgs','F') ) ;
    lstAT.Add( 18005,_a(18005,'lg_drt','F') ) ;
    lstAT.Add( 18006,_a(18006,'lg_wdp','F') ) ;
    lstAT.Add( 18007,_a(18007,'lg_wdu','F') ) ;
    lstAT.Add( 18008,_a(18008,'lg_rel','F') ) ;
    lstAT.Add( 18009,_a(18009,'lg_fnc','F') ) ;
    lstAT.Add( 18010,_a(18010,'lg_des','F') ) ;
    lstAT.Add( 18011,_a(18011,'lg_pbr','F') ) ;
    lstAT.Add( 18012,_a(18012,'lc_csi','F') ) ;
    lstAT.Add( 18013,_a(18013,'lc_cse','F') ) ;
    lstAT.Add( 18014,_a(18014,'lc_asi','F') ) ;
    lstAT.Add( 18015,_a(18015,'lc_ase','F') ) ;
    lstAT.Add( 18016,_a(18016,'lc_cci','F') ) ;
    lstAT.Add( 18017,_a(18017,'lc_cce','F') ) ;
    lstAT.Add( 18018,_a(18018,'lc_bm1','F') ) ;
    lstAT.Add( 18019,_a(18019,'lc_bm2','F') ) ;
    lstAT.Add( 18020,_a(18020,'lc_lg1','F') ) ;
    lstAT.Add( 18021,_a(18021,'lc_lg2','F') ) ;
    lstAT.Add( 18022,_a(18022,'lc_dr1','F') ) ;
    lstAT.Add( 18023,_a(18023,'lc_dr2','F') ) ;
    lstAT.Add( 18024,_a(18024,'lc_sp1','F') ) ;
    lstAT.Add( 18025,_a(18025,'lc_sp2','F') ) ;
    lstAT.Add( 18026,_a(18026,'lc_wd1','F') ) ;
    lstAT.Add( 18027,_a(18027,'lc_wd2','F') ) ;
    lstAT.Add( 33066,_a(33066,'shptyp','F') ) ;
    lstAT.Add( 40000,_a(40000,'updmsg','F') ) ;
  end ;

//==============================================================================
// TGIS_FileS57
//==============================================================================

  constructor TGIS_FileS57.Create(
    const _file : String
  );
  begin
    inherited Create ;

    fname := _file ;
    InitializeLists ;
  end ;

  procedure TGIS_FileS57.doDestroy ;
  begin
    FreeObject( FS57Metadata ) ;

    inherited ;
  end ;

  procedure TGIS_FileS57.InitializeLists;
  begin
    FS57Metadata := T_S57Metadata.Create ;
  end ;

  function TGIS_FileS57.findObjClass(
    const _objl : Integer;
    out _objIdx : TObject
  ): Boolean;
  var
    oc : T_S57ObjectClass ;
  begin
    Result := False ;

    if T_S57Metadata(FS57Metadata).lstOC.TryGetValue( _objl, oc ) then begin
       Result := True ;
       _objIdx := oc ;
    end ;
  end ;

  function TGIS_FileS57.getLayerPriority(
    const _objl : Integer
  ) : Integer ;
  var
     p : TObject ;
  begin
    if findObjClass( _objl, p ) then
      Result := T_S57ObjectClass(p).dp
    else
      Result := 999 ;
  end;

  function TGIS_FileS57.findAttrName(
    const _attrIdx : Integer;
    out _attrName  : String ;
    out _attrType  : Char
   ): Boolean;
  var
    o : T_S57Attr ;
  begin
    Result := False ;
    _attrName := '' ;
    _attrType := ' ' ;
    if T_S57Metadata(FS57Metadata).lstAT.TryGetValue( _attrIdx, o ) then begin
      _attrName := o.an ;
      _attrType := o.at ;
      Result := True ;
    end
  end ;


  procedure TGIS_FileS57.testLayer(
    const _objl : Integer
  );
  var
     p : TObject ;
  begin
    {$IFDEF OXYGENE}
      if not assigned( LayerName ) then
        LayerName := '' ;
    {$ENDIF}

    if findObjClass( _objl, p ) then
      if not IsStringEmpty( LayerName ) then
        FSkipObject := Trim( UpperCase( T_S57ObjectClass(p).ac ) )<>
                       Trim( UpperCase( LayerName ) );
  end ;

  procedure TGIS_FileS57.addAttrPrim(
    const _objl : Integer
  );
  begin
    if assigned( AddAttrEvent ) then
      AddAttrEvent( S57_OBJ_FIELD, IntToStr( _objl ), 'I' ) ;
  end ;

  procedure TGIS_FileS57.prepareLayer(
    const _objl : Integer
  ) ;
  var
     p : TObject ;
  begin
    if assigned( AddAttrEvent ) then begin
      if findObjClass( _objl, p ) then
        SetLayerEvent( T_S57ObjectClass( p ).ac,
                       T_S57ObjectClass( p ).oc,
                       T_S57ObjectClass( p ).gt
                     )
      else
        SetLayerEvent( IntToStr( _objl ), IntToStr( _objl ), 0 ) ;
    end ;
  end ;

  procedure TGIS_FileS57.ParseS57;
  var
    S57Reader : T_S57Reader ;
  begin
    S57Reader := T_S57Reader.Create( fname, self ) ;
    try
      S57Reader.Open ;
      S57Reader.BuildFeatures ;
    finally
      FreeObject( S57Reader ) ;
    end ;
  end ;

  // T_S57Reader

  constructor T_S57Reader.Create(
    const _fileName : String ;
    const _parent   : TObject
  ) ;
  begin
    inherited Create ;

    parent        := _parent ;
    pszModuleName := _fileName ;
    pszDSNM       := '' ;
    poModule      := nil ;
    nFDefnCount   := 0 ;
    nCOMF         := 1000000 ;
    nSOMF         := 10 ;

    nNextFEIndex  := 0 ;
    nNextVIIndex  := 0 ;
    nNextVCIndex  := 0 ;
    nNextVEIndex  := 0 ;
    nNextVFIndex  := 0 ;
    nNextDSIDIndex:= 0 ;

    szUPDNUpdate := '' ;

    iPointOffset := 0 ;
    nOptionFlags := 1 ;

    oVI_Index := TDictionary<Integer,TObject>.Create ;
    oVC_Index := TDictionary<Integer,TObject>.Create ;
    oVE_Index := TDictionary<Integer,TObject>.Create ;
    oFE_Index := TGIS_ObjectList.Create( False ) ;
    oVF_Index := TDictionary<Integer,TObject>.Create ;
  end ;

  procedure T_S57Reader.doDestroy;
  begin
    Close ;

    FreeObject( oVI_Index ) ;
    FreeObject( oVC_Index ) ;
    FreeObject( oVE_Index ) ;
    FreeObject( oFE_Index ) ;
    FreeObject( oVF_Index ) ;

    inherited ;
  end ;

  procedure T_S57Reader.rewind ;
  begin
    nNextFEIndex    := 0 ;
    nNextVIIndex    := 0 ;
    nNextVCIndex    := 0 ;
    nNextVEIndex    := 0 ;
    nNextVFIndex    := 0 ;
    nNextDSIDIndex  := 0 ;
  end ;

  procedure T_S57Reader.readFeatures ;
  var
    poRecord    : TGIS_DDFRecord ;
    poKeyField  : TGIS_DDFField ;
    nRCNM       : Integer ;
    nRCID       : Integer ;
  begin
    assert( poModule <> nil ) ;

    poRecord := poModule.ReadRecord ;
    while poRecord <> nil do begin

      poKeyField := poRecord.GetField(1) ;

      if poKeyField.FieldDefn.Name = 'VRID' then begin

        nRCNM := poRecord.GetIntSubfield( 'VRID', 0, 'RCNM', 0 ) ;
        nRCID := poRecord.GetIntSubfield( 'VRID', 0, 'RCID', 0 ) ;

        case nRCNM of
          RCNM_VI : oVI_Index.Add( nRCID, poRecord.Clone ) ;
          RCNM_VC : oVC_Index.Add( nRCID, poRecord.Clone ) ;
          RCNM_VE : oVE_Index.Add( nRCID, poRecord.Clone ) ;
          RCNM_VF : oVF_Index.Add( nRCID, poRecord.Clone ) ;
        end
      end
      else if poKeyField.FieldDefn.Name = 'FRID' then begin

        oFE_Index.Add( poRecord.Clone ) ;
      end
      else if poKeyField.FieldDefn.Name = 'DSID' then begin

      end
      else if poKeyField.FieldDefn.Name = 'DSPM' then begin

        nCOMF := Max( 1, poRecord.GetIntSubfield( 'DSPM', 0, 'COMF', 0 ) ) ;
        nSOMF := Max( 1, poRecord.GetIntSubfield( 'DSPM', 0, 'SOMF', 0 ) ) ;
      end ;

      poRecord := poModule.ReadRecord ;
    end ;

    findAndApplyUpdates;
  end ;

  function T_S57Reader.fetchLine(
    const _poSRecord    : TGIS_DDFRecord ;
    const _iDirection   : Integer
  ) : Boolean ;
  var
    nVCount, i,
    nBytesRemaining   : Integer ;
    poSG2D, poAR2D    : TGIS_DDFField ;
    poXCOO, poYCOO    : TGIS_DDFSubfieldDefn ;
    bStandardFormat   : Boolean ;
    pachData          : TBytes ;
    ptg               : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    nXCOO, nYCOO,
    off, nc           : Integer ;
    ptgList           : TGIS_Point3DList ;
  begin
    Result := True ;

    bStandardFormat := True ;
    poSG2D := _poSRecord.FindField( 'SG2D', 0 ) ;
    poAR2D := _poSRecord.FindField( 'AR2D', 0 ) ;

    if ( poSG2D = nil ) and ( poAR2D <> nil ) then
      poSG2D := poAR2D ;

    if ( poSG2D <> nil ) then begin
      poXCOO := poSG2D.FieldDefn.FindSubfieldDefn('XCOO') ;
      poYCOO := poSG2D.FieldDefn.FindSubfieldDefn('YCOO') ;

      if ( poXCOO = nil ) and ( poYCOO = nil ) then begin
        Result := False ;
        exit ;
      end ;

      nVCount := poSG2D.GetRepeatCount( _poSRecord ) ;
    end
    else exit ;

    if nVCount = 0 then exit ;

    if ( poSG2D.FieldDefn.SubfieldCount <> 2 ) then
      bStandardFormat := False ;

    if ( poXCOO.Format <> 'b24' ) or ( poYCOO.Format <> 'b24' ) then
      bStandardFormat := False ;

    ptgList := TGIS_Point3DList.Create ;
    try
      if ( bStandardFormat ) then begin
        pachData := Copy( _poSRecord.Data,
                          poSG2D.GetSubfieldData( _poSRecord, poYCOO, nBytesRemaining, 0 ),
                          length(_poSRecord.Data) ) ;
        off := 0 ;
        for i := 0 to nVCount-1 do begin
          {$IFDEF OXYGENE}
            nYCOO := BitConverter.ToInt32( pachData, off ) ;
          {$ELSE}
            nYCOO := PInteger( @pachData[ off ] )^ ;
          {$ENDIF}
          inc( off, 4 ) ;

          {$IFDEF OXYGENE}
            nXCOO := BitConverter.ToInt32( pachData, off ) ;
          {$ELSE}
            nXCOO := PInteger( @pachData[ off ] )^ ;
          {$ENDIF}
          inc( off, 4 ) ;

          ptg.X := ( 1.0 * nXCOO ) / ( 1.0 * nCOMF ) ;
          ptg.Y := ( 1.0 * nYCOO ) / ( 1.0 * nCOMF ) ;
          ptg.Z := 0 ;
          ptg.M := 0 ;
          ptgList.Add( _TGIS_Point3D(ptg) ) ;
        end
      end
      else begin
        for i := 0 to nVCount-1 do begin
          pachData := Copy( _poSRecord.Data,
                            poSG2D.GetSubfieldData( _poSRecord, poXCOO, nBytesRemaining, i ),
                            length(_poSRecord.Data) ) ;
          ptg.X := ( 1.0 * poXCOO.GetIntData( pachData ,nBytesRemaining, nc ) ) / ( 1.0 * nCOMF ) ;

          pachData := Copy( _poSRecord.Data,
                            poSG2D.GetSubfieldData( _poSRecord, poYCOO, nBytesRemaining, i ),
                            length(_poSRecord.Data) ) ;
          ptg.Y := ( 1.0 * poXCOO.GetIntData(pachData,nBytesRemaining,nc) ) / ( 1.0 * nCOMF ) ;

          ptg.Z := 0 ;
          ptg.M := 0 ;

          ptgList.Add( ptg ) ;
        end
      end ;

      if _iDirection > 0 then
        for i := 0 to ptgList.Count - 1 do
          TGIS_FileS57( parent ).AddCoordEvent( ptgList[ i ] )
      else
        for i := ptgList.Count - 1 downto 0 do
          TGIS_FileS57( parent ).AddCoordEvent( ptgList[ i ] )

    finally
      FreeObject( ptgList ) ;
    end ;

  end ;

  function T_S57Reader.fetchPoint(
    const _nRCNM     : Integer ;
    const _nRCID     : Integer ;
      var _ptg       : TGIS_Point3D
  ) : Boolean ;
  var
    poSRecord : TGIS_DDFRecord ;
    poObj     : TObject ;
  begin
    Result := False ;

    if ( _nRCNM = RCNM_VI ) then
      oVI_Index.TryGetValue( _nRCID, poObj )
    else
      oVC_Index.TryGetValue( _nRCID, poObj ) ;

    poSRecord := TGIS_DDFRecord( poObj ) ;

    if poSRecord = nil then exit ;

    _ptg.X := 0.0 ;
    _ptg.Y := 0.0 ;
    _ptg.Z := 0.0 ;
    _ptg.M := 0.0 ;

    if poSRecord.FindField( 'SG2D', 0 ) <> nil then begin

      _ptg.X := ( 1.0 * poSRecord.GetIntSubfield( 'SG2D', 0, 'XCOO', 0 ) ) / ( 1.0 * nCOMF ) ;
      _ptg.Y := ( 1.0 * poSRecord.GetIntSubfield( 'SG2D', 0, 'YCOO', 0 ) ) / ( 1.0 * nCOMF ) ;
    end
    else if poSRecord.FindField( 'SG3D', 0 ) <> nil then begin

      _ptg.X := ( 1.0 * poSRecord.GetIntSubfield( 'SG3D', 0, 'XCOO', 0 ) ) / ( 1.0 * nCOMF ) ;
      _ptg.Y := ( 1.0 * poSRecord.GetIntSubfield( 'SG3D', 0, 'YCOO', 0 ) ) / ( 1.0 * nCOMF ) ;
      _ptg.Z := ( 1.0 * poSRecord.GetIntSubfield( 'SG3D', 0, 'VE3D', 0 ) ) / ( 1.0 * nCOMF ) ;
    end
    else
      exit ;

    Result := True ;
  end ;

  procedure T_S57Reader.applyUpdates(
    const _poUModule : TGIS_DDFModule
  ) ;
  begin

  end ;

  procedure T_S57Reader.findAndApplyUpdates ;
  var
    pszPath       : TStringBuilder ;
    i             : Integer        ;
    oUpdateModule : TGIS_DDFModule ;
  begin
    pszPath := TStringBuilder.Create( pszModuleName ) ;
    try
      for i := 1 to 9 do begin
        pszPath[ pszPath.Length - 1 ] := chr( ord('0') + i ) ;
        if SafeFileExists( pszPath.ToString ) then begin
          oUpdateModule := TGIS_DDFModule.Create ;
          try
            if oUpdateModule.Open( pszPath.ToString ) then
              applyUpdates( oUpdateModule ) ;
          finally
            FreeObject( oUpdateModule ) ;
          end ;
        end
        else
          break ;
      end ;
    finally
      FreeObject( pszPath ) ;
    end ;
  end ;

  function T_S57Reader.Open : Boolean ;
  var
    poFSPT : TGIS_DDFFieldDefn ;
  begin
    Result := True ;

    if ( poModule <> nil ) then begin
      rewind ;
      Result := True ;
      exit ;
    end ;

    poModule := TGIS_DDFModule.Create ;
    if not poModule.Open( pszModuleName ) then begin
      FreeObject( poModule ) ;
      Result := False ;
      exit ;
    end ;

    if poModule.FindFieldDefn( 'DSID' ) = nil then begin
      FreeObject( poModule ) ;
      Result := False ;
      exit ;
    end ;

    poFSPT := poModule.FindFieldDefn( 'FSPT' );
    if ( poFSPT <> nil ) and ( not poFSPT.IsRepeating ) then
      poFSPT.IsRepeating := True ;

    nNextFEIndex    := 0 ;
    nNextVIIndex    := 0 ;
    nNextVCIndex    := 0 ;
    nNextVEIndex    := 0 ;
    nNextVFIndex    := 0 ;
    nNextDSIDIndex  := 0 ;
  end ;

  function T_S57Reader.parseName(
    const _poRecord : TGIS_DDFRecord ;
    const _poField  : TGIS_DDFField ;
    const _nIndex   : Integer ;
      var _pnRCNM   : Integer
  ) : Integer ;
  var
    pabyData : TBytes ;
    mb       : Integer ;
    off      : Integer ;
  begin
    off := _poField.GetSubfieldData( _poRecord, _poField.FieldDefn.FindSubfieldDefn( 'NAME' ),
                                     mb, _nIndex
                                    ) ;
    pabyData := Copy( _poRecord.Data, off, MaxInt ) ;
    _pnRCNM := pabyData[0] ;

    Result := pabyData[1] +
              pabyData[2] * 256 +
              pabyData[3] * 256 * 256 +
              pabyData[4] * 256 * 256 * 256 ;
  end ;

  procedure T_S57Reader.buildPointGeometry(
    const _poRecord : TGIS_DDFRecord ;
    const _nOBJL    : Integer
  ) ;
  var
    poFSPT : TGIS_DDFField ;
    nRCNM,
    nRCID  : Integer ;
    ptg    : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
  begin
    poFSPT := _poRecord.FindField( 'FSPT', 0 ) ;
    if ( poFSPT = nil ) then exit ;

    nRCNM := 0 ;
    nRCID := parseName( _poRecord, poFSPT, 0, nRCNM ) ;

    if not fetchPoint( nRCNM, nRCID, ptg ) then exit ;

    TGIS_FileS57( parent ).prepareLayer( _nOBJL ) ;
    TGIS_FileS57( parent ).NewShapeEvent( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
    TGIS_FileS57( parent ).AddPartEvent() ;
    TGIS_FileS57( parent ).AddCoordEvent( ptg ) ;
    TGIS_FileS57( parent ).EndShapeEvent() ;

    applyAttributes( _poRecord, _nOBJL ) ;
  end ;

  procedure T_S57Reader.buildSoundingGeometry(
    const _poRecord : TGIS_DDFRecord;
    const _nOBJL    : Integer
  ) ;
  var
    poFSPT        : TGIS_DDFField ;
    nRCNM, nRCID  : Integer ;
    poSRecord     : TGIS_DDFRecord ;
    poField       : TGIS_DDFField ;
    poXCOO,
    poYCOO,
    poVE3D        : TGIS_DDFSubfieldDefn ;
    nPointCount, i,
    nBytesLeft    : Integer ;
    nBytesConsumed: Integer ;
    pachData      : TBytes ;
    ptg           : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    poObj         : TObject ;
  begin
    poFSPT := _poRecord.FindField( 'FSPT', 0 ) ;
    if ( poFSPT = nil ) then exit ;

    nRCID := parseName( _poRecord, poFSPT, 0, nRCNM ) ;

    if ( nRCNM = RCNM_VI ) then
      oVI_Index.TryGetValue( nRCID, poObj )
    else
      oVC_Index.TryGetValue( nRCID, poObj ) ;

    poSRecord := TGIS_DDFRecord( poObj ) ;

    if ( poSRecord = nil ) then exit ;

    poField := poSRecord.FindField( 'SG2D', 0 );
    if ( poField = nil ) then
        poField := poSRecord.FindField( 'SG3D', 0 );
    if ( poField = nil ) then exit ;

    poXCOO := poField.FieldDefn.FindSubfieldDefn( 'XCOO' );
    poYCOO := poField.FieldDefn.FindSubfieldDefn( 'YCOO' );
    poVE3D := poField.FieldDefn.FindSubfieldDefn( 'VE3D' );

    nPointCount := poField.GetRepeatCount( poSRecord );

    pachData    := Copy( poSRecord.Data, poField.Data, length(poSRecord.Data) ) ;
    nBytesLeft  := poField.DataSize ;

    TGIS_FileS57( parent ).prepareLayer( _nOBJL ) ;
    for i := 0 to nPointCount - 1 do begin
      TGIS_FileS57( parent ).NewShapeEvent( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
      TGIS_FileS57( parent ).AddPartEvent() ;

      ptg.Y := ( 1.0 * poYCOO.GetIntData( pachData, nBytesLeft, nBytesConsumed ) ) / ( 1.0 * nCOMF ) ;
      dec( nBytesLeft, nBytesConsumed ) ;
      pachData := Copy( pachData, nBytesConsumed, length(pachData) ) ;

      ptg.X := ( 1.0 * poXCOO.GetIntData( pachData, nBytesLeft, nBytesConsumed ) ) / ( 1.0 * nCOMF ) ;
      dec( nBytesLeft, nBytesConsumed ) ;
      pachData := Copy( pachData, nBytesConsumed, length(pachData) ) ;

      if ( poVE3D <> nil ) then begin
        ptg.Z := ( 1.0 * poVE3D.GetIntData( pachData, nBytesLeft, nBytesConsumed ) ) / ( 1.0 * nSOMF ) ;
        dec( nBytesLeft, nBytesConsumed ) ;
        pachData := Copy( pachData, nBytesConsumed, length(pachData) ) ;
      end
      else
        ptg.Z := 0 ;

      ptg.M := 0 ;

      TGIS_FileS57( parent ).AddCoordEvent( ptg ) ;
      TGIS_FileS57( parent ).EndShapeEvent() ;

      TGIS_FileS57( parent ).AddAttrEvent( S57_SOUNDING_FIELD, DotFloatToStr( ptg.Z),'F' );
      applyAttributes( _poRecord, _nOBJL ) ;
    end ;
  end ;

  procedure T_S57Reader.buildLineGeometry(
    const _poRecord : TGIS_DDFRecord;
    const _nOBJL    : Integer
  ) ;
  var
    poFSPT            : TGIS_DDFField ;
    nEdgeCount,
    nRCID, nRCNM      : Integer ;
    poSRecord         : TGIS_DDFRecord ;
    nVCount, nStart,
    nEnd, nInc, iEdge : Integer ;
    poSG2D, poAR2D    : TGIS_DDFField ;
    poXCOO, poYCOO    : TGIS_DDFSubfieldDefn ;
    nVC_RCID, i,
    nBytesRemaining,
    nc                : Integer ;
    ptg               : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    pachData          : TBytes ;
    poObj             : TObject ;
  begin
    poFSPT := _poRecord.FindField( 'FSPT', 0 ) ;
    if ( poFSPT = nil ) then exit ;

    nEdgeCount := poFSPT.GetRepeatCount( _poRecord ) ;

    TGIS_FileS57( parent ).prepareLayer( _nOBJL ) ;
    TGIS_FileS57( parent ).NewShapeEvent( TGIS_ShapeType.Arc, TGIS_DimensionType.XY ) ;
    TGIS_FileS57( parent ).AddPartEvent() ;

    for iEdge := 0 to nEdgeCount-1 do begin

      nRCID := parseName( _poRecord, poFSPT, iEdge, nRCNM ) ;

      oVE_Index.TryGetValue( nRCID, poObj ) ;
      poSRecord := TGIS_DDFRecord( poObj ) ;

      if poSRecord = nil  then continue ;

      poSG2D := poSRecord.FindField( 'SG2D', 0 );
      poAR2D := poSRecord.FindField( 'AR2D', 0 );

      poXCOO := nil ;
      poYCOO := nil ;

      if ( poSG2D = nil ) and ( poAR2D <> nil ) then
        poSG2D := poAR2D ;

      if ( poSG2D <> nil ) then begin
          poXCOO := poSG2D.FieldDefn.FindSubfieldDefn('XCOO') ;
          poYCOO := poSG2D.FieldDefn.FindSubfieldDefn('YCOO') ;

          nVCount := poSG2D.GetRepeatCount( poSRecord ) ;
      end
      else
          nVCount := 0;

      if ( _poRecord.GetIntSubfield( 'FSPT', 0, 'ORNT', iEdge ) = 2 ) then begin
          nStart := nVCount-1 ;
          nEnd   := 0 ;
          nInc   := -1 ;
      end
      else begin
          nStart := 0 ;
          nEnd   := nVCount-1 ;
          nInc   := 1 ;
      end ;

      if ( iEdge = 0 ) then begin
          if ( nInc = 1 ) then
            nVC_RCID := parseName( poSRecord, poSRecord.FindField( 'VRPT', 0 ), 0, nRCNM )
          else
            nVC_RCID := parseName( poSRecord, poSRecord.FindField( 'VRPT', 0 ), 1, nRCNM );

          if fetchPoint( RCNM_VC, nVC_RCID, ptg ) then
            TGIS_FileS57( parent ).AddCoordEvent( ptg ) ;
      end ;

      i := nStart ;
      while i <> (nEnd+nInc) do begin
        pachData := Copy( poSRecord.Data,
                          poSG2D.GetSubfieldData( poSRecord, poXCOO, nBytesRemaining, i ),
                          length(poSRecord.Data) ) ;
        ptg.X := ( 1.0 * poXCOO.GetIntData( pachData, nBytesRemaining, nc ) ) / ( 1.0 * nCOMF );

        pachData := Copy( poSRecord.Data,
                          poSG2D.GetSubfieldData( poSRecord, poYCOO, nBytesRemaining, i ),
                          length(poSRecord.Data) ) ;
        ptg.Y := ( 1.0 * poXCOO.GetIntData( pachData, nBytesRemaining, nc ) ) / ( 1.0 * nCOMF ) ;
        ptg.Z := 0 ;
        ptg.M := 0 ;

        TGIS_FileS57( parent ).AddCoordEvent( ptg ) ;
        i := i + nInc ;
      end ;

      if ( nInc = 1 ) then
        nVC_RCID := parseName( poSRecord, poSRecord.FindField( 'VRPT', 0 ), 1, nRCNM )
      else
        nVC_RCID := parseName( poSRecord, poSRecord.FindField( 'VRPT', 0 ), 0, nRCNM );

      if fetchPoint( RCNM_VC, nVC_RCID, ptg ) then
        TGIS_FileS57( parent ).AddCoordEvent( ptg ) ;
    end ;

     TGIS_FileS57( parent ).EndShapeEvent() ;
     applyAttributes( _poRecord, _nOBJL ) ;
  end ;

  procedure T_S57Reader.applyAttributes(
    const _poRecord : TGIS_DDFRecord;
    const _nOBJL    : Integer
  ) ;
  var
    poATTF, poNATF : TGIS_DDFField ;
    nAttrCount,
    iAttr, nAttrId : Integer ;
    name, val      : String ;
    tp : Char ;
  begin
    TGIS_FileS57( parent ).addAttrPrim( _nOBJL ) ;

    poATTF := _poRecord.FindField( 'ATTF', 0 ) ;

    if ( poATTF = nil ) then exit ;

    nAttrCount := poATTF.GetRepeatCount( _poRecord ) ;
    for iAttr := 0 to nAttrCount-1 do begin

      nAttrId := _poRecord.GetIntSubfield( 'ATTF', 0 , 'ATTL', iAttr ) ;
      val     := _poRecord.GetStringSubfield( 'ATTF', 0, 'ATVL', iAttr ) ;
      tp := ' ' ;
      if TGIS_FileS57( parent ).findAttrName( nAttrId, name, tp ) then
        TGIS_FileS57( parent ).AddAttrEvent( name, val, tp ) ;
    end ;

    poNATF := _poRecord.FindField( 'NATF', 0 ) ;

    if ( poNATF = nil ) then exit ;

    nAttrCount := poNATF.GetRepeatCount( _poRecord ) ;
    for iAttr := 0 to nAttrCount-1 do begin

      nAttrId := _poRecord.GetIntSubfield( 'NATF', 0 , 'ATTL', iAttr ) ;
      val     := _poRecord.GetStringSubfield( 'NATF', 0, 'ATVL', iAttr ) ;
      tp := ' ' ;

      if TGIS_FileS57( parent ).findAttrName( nAttrId, name, tp ) then
        TGIS_FileS57( parent ).AddAttrEvent( name, val, tp ) ;
    end ;

  end ;

  procedure T_S57Reader.generateAttributes(
    const _poRecord : TGIS_DDFRecord ;
    const _nOBJL    : Integer
   ) ;
  var
    poATTF, poNATF : TGIS_DDFField ;
    nAttrCount,
    iAttr, nAttrId : Integer ;
    name, val      : String ;
    tp : Char ;
  begin
    poATTF := _poRecord.FindField( 'ATTF', 0 ) ;

    if ( poATTF = nil ) then exit ;

    nAttrCount := poATTF.GetRepeatCount( _poRecord ) ;
    for iAttr := 0 to nAttrCount-1 do begin

      nAttrId := _poRecord.GetIntSubfield( 'ATTF', 0 , 'ATTL', iAttr ) ;
      tp := ' ' ;

      if TGIS_FileS57( parent ).findAttrName( nAttrId, name, tp ) then
        TGIS_FileS57( parent ).AddAttrEvent( name, val, tp ) ;
    end ;

    poNATF := _poRecord.FindField( 'NATF', 0 ) ;

    if ( poNATF = nil ) then exit ;

    nAttrCount := poNATF.GetRepeatCount( _poRecord ) ;
    for iAttr := 0 to nAttrCount-1 do begin

      nAttrId := _poRecord.GetIntSubfield( 'NATF', 0 , 'ATTL', iAttr ) ;
      tp := ' ' ;

      if TGIS_FileS57( parent ).findAttrName( nAttrId, name, tp ) then
        TGIS_FileS57( parent ).AddAttrEvent( name, val, tp ) ;
    end ;

  end ;

  procedure T_S57Reader.buildAreaGeometry(
    const _poRecord : TGIS_DDFRecord ;
    const _nOBJL    : Integer
  ) ;
  var
    poFSPT      : TGIS_DDFField ;
    iFSPT,
    nEdgeCount,
    iEdge,
    nRCID,
    nRCNM, dir  : Integer ;
    poSRecord   : TGIS_DDFRecord ;
    nVC_RCID,
    f, g        : Integer ;
    ptg1        : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptg2        : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    poObj       : TObject ;
    bfirst      : Boolean ;
  begin
    TGIS_FileS57( parent ).prepareLayer( _nOBJL ) ;

    TGIS_FileS57( parent ).NewShapeEvent( TGIS_ShapeType.Polygon, TGIS_DimensionType.XY ) ;

    iFSPT := 0 ;
    poFSPT := _poRecord.FindField( 'FSPT', iFSPT ) ;
    while assigned( poFSPT ) do begin

      nEdgeCount := poFSPT.GetRepeatCount( _poRecord ) ;

      for iEdge := 0 to nEdgeCount-1 do begin
        nRCID := parseName( _poRecord, poFSPT, iEdge, nRCNM ) ;

        oVE_Index.TryGetValue( nRCID, poObj ) ;
        poSRecord := TGIS_DDFRecord( poObj ) ;

        if poSRecord = nil then continue ;

        TGIS_FileS57( parent ).AddPartEvent() ;

        if ( _poRecord.GetIntSubfield( 'FSPT', 0, 'ORNT', iEdge ) = 2 ) then begin
          f   := 1;
          g   := 0;
          dir := -1
        end
        else begin
          f   := 0 ;
          g   := 1 ;
          dir := 1
        end ;

        nVC_RCID := parseName( poSRecord, poSRecord.FindField( 'VRPT', 0 ), f, nRCNM  ) ;

        bfirst := False ;
        if fetchPoint( RCNM_VC, nVC_RCID, ptg1 ) then begin
          TGIS_FileS57( parent ).AddCoordEvent( ptg1 ) ;
          bfirst := True ;
        end ;

        fetchLine( poSRecord, dir ) ;

        nVC_RCID := parseName( poSRecord, poSRecord.FindField( 'VRPT', 0 ), g, nRCNM ) ;

        if fetchPoint( RCNM_VC, nVC_RCID, ptg2 ) then
          TGIS_FileS57( parent ).AddCoordEvent( ptg2 ) ;

        if bfirst then
          TGIS_FileS57( parent ).AddCoordEvent( ptg1 ) ;
      end ;
       inc( iFSPT ) ;
      poFSPT := _poRecord.FindField( 'FSPT', iFSPT ) ;
    end ;

     TGIS_FileS57( parent ).EndShapeEvent() ;
     applyAttributes( _poRecord, _nOBJL ) ;
  end ;

  procedure T_S57Reader.Close ;
  var
    i : Integer ;
    {$IFDEF DCC}
    o : TPair<Integer, TObject> ;
    {$ENDIF}
  begin
    if ( poModule <> nil ) then begin
      {$IFNDEF NEXTGEN}
        {$IFDEF DCC}
          for o in oVI_Index do
            o.Value.Free ;
          for o in oVC_Index do
            o.Value.Free ;
          for o in oVE_Index do
            o.Value.Free ;
          for o in oVF_Index do
            o.Value.Free ;
        {$ENDIF}

        for i := 0 to oFE_Index.Count-1 do
          FreeObjectNotNil( oFE_Index[i] ) ;
      {$ENDIF}

      FreeObject( poModule ) ;
    end
  end ;

  procedure T_S57Reader.BuildFeatures ;
  var
    i,
    nPRIM,
    nOBJL     : Integer ;
    poRecord  : TGIS_DDFRecord ;
    lastobjl  : Integer ;

    function fcompare( _item1, _item2 : TObject ) : Integer ;
    var
      poRecord1, poRecord2 : TGIS_DDFRecord;
      nOBJL1, nOBJL2 : Integer ;
      p1, p2 : Integer ;
    begin
      poRecord1 := TGIS_DDFRecord( _item1 ) ;
      poRecord2 := TGIS_DDFRecord( _item2 ) ;

      nOBJL1 := poRecord1.GetIntSubfield( 'FRID', 0, 'OBJL', 0 ) ;
      nOBJL2 := poRecord2.GetIntSubfield( 'FRID', 0, 'OBJL', 0 ) ;

      p1 := TGIS_FileS57( parent ).getLayerPriority( nOBJL1 ) ;
      p2 := TGIS_FileS57( parent ).getLayerPriority( nOBJL2 ) ;

      if      p1 = p2 then
              Result :=  0
      else if p1 < p2 then
              Result :=  -1
      else    Result := 1 ;
    end ;

    procedure quickSortFeatures(L, R : Integer );
    var
      I, J: Integer;
      P, T: TObject;
    begin
      repeat
        I := L;
        J := R;
        P := oFE_Index[(L + R) shr 1];
        repeat
          while fcompare(oFE_Index[I], P) < 0 do
            inc(I);
          while fcompare(oFE_Index[J], P) > 0 do
            dec(J);
          if I <= J then
          begin
            if I <> J then
            begin
              T := oFE_Index[I];
              oFE_Index[I] := oFE_Index[J];
              oFE_Index[J] := T;
            end;
            inc(I);
            dec(J);
          end;
        until I > J;
        if L < J then
          quickSortFeatures(L, J);
        L := I;
      until I >= R;
    end;

  begin
    readFeatures ;

    quickSortFeatures( 0, oFE_Index.Count - 1 ) ;

    lastobjl := -1 ;
    // build sublayers
    for i := 0 to oFE_Index.Count - 1 do begin

      poRecord := TGIS_DDFRecord( oFE_Index[ i ] ) ;

      nOBJL := poRecord.GetIntSubfield( 'FRID', 0, 'OBJL', 0 ) ;
      TGIS_FileS57( parent ).testLayer( nOBJL ) ;
      if TGIS_FileS57( parent ).FSkipObject then continue ;
      if lastobjl = nOBJL then continue ;

      TGIS_FileS57( parent ).prepareLayer( nOBJL ) ;
      generateAttributes( poRecord, nOBJL ) ;
      lastobjl := nOBJL ;
    end ;

    for i := 0 to oFE_Index.Count - 1 do begin

      poRecord := TGIS_DDFRecord( oFE_Index[ i ] ) ;

      nOBJL := poRecord.GetIntSubfield( 'FRID', 0, 'OBJL', 0 ) ;
      nPRIM := poRecord.GetIntSubfield( 'FRID', 0, 'PRIM', 0 ) ;

      TGIS_FileS57( parent ).testLayer( nOBJL ) ;
      if TGIS_FileS57( parent ).FSkipObject then continue ;

      if nPRIM = PRIM_P then begin
        if ( nOBJL = 129 ) then
         buildSoundingGeometry( poRecord, nOBJL )
        else
          buildPointGeometry( poRecord, nOBJL ) ;
      end
      else if nPRIM = PRIM_L then
        buildLineGeometry( poRecord, nOBJL )
      else if nPRIM = PRIM_A then
        buildAreaGeometry( poRecord, nOBJL ) ;
    end ;
  end ;

//==================================== END =====================================
end.
