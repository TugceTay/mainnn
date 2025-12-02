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
  Encapsulation of a VPF file access.
}

{$IFDEF DCC}
  unit GisFileVPF;
  {$HPPEMIT '#pragma link "GisFileVPF"}
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
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisRtl,
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
  {#GENDOC:HIDE}
  /// <summary>
  ///   Create and prepare a new shape.
  /// </summary>
  TGIS_VPFExecNewShapeEvent = procedure(
    const _shp_type : TGIS_ShapeType ;
    const _dim      : TGIS_DimensionType
  ) of object ;

  {#GENDOC:HIDE}
  /// <summary>
  ///   Add coordinates.
  /// </summary>
  TGIS_VPFExecAddCoordEvent = procedure(
    const _ptg : TGIS_Point3D
  ) of object ;

  {#GENDOC:HIDE}
  /// <summary>
  ///   Add attribute to a new shape.
  /// </summary>
  TGIS_VPFExecAddAttrEvent = procedure(
    const _name     : String ;
    const _type     : TGIS_FieldType ;
    const _val      : Variant
  ) of object ;

  {#GENDOC:HIDE}
  /// <summary>
  ///   Set layer for a new shape.
  /// </summary>
  TGIS_VPFExecSetLayerEvent = procedure(
    const _name     : String ;
    const _caption  : String ;
    const _path     : String  ;
    const _shpType  : TGIS_ShapeType
  ) of object ;

  /// <summary>
  ///   Add part to a new shape.
  /// </summary>
  TGIS_VPFExecAddPartEvent = procedure of object ;

  /// <summary>
  ///   Finish creating a new shape.
  /// </summary>
  TGIS_VPFExecEndShapeEvent = procedure of object ;

  /// <summary>
  ///   Encapsulation of VPF-file low-level access.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Only for internal purposes of TGIS_LayerVPF.
  ///    </note>
  /// </remarks>
  TGIS_FileVPF = class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      VPFDb : TObject ;

      { Event handlers. }
      FOnNewShape : TGIS_VPFExecNewShapeEvent ;
      FOnEndShape : TGIS_VPFExecEndShapeEvent ;
      FOnAddPart  : TGIS_VPFExecAddPartEvent  ;
      FOnAddCoord : TGIS_VPFExecAddCoordEvent ;
      FOnAddAttr  : TGIS_VPFExecAddAttrEvent  ;
      FOnBusy     : TGIS_BusyEvent            ;
      FOnSetLayer : TGIS_VPFExecSetLayerEvent ;

      FBusyCount  : Integer ;
      FBusyPos    : Integer ;
      FCategory   : String ;
      FFeature    : String ;
    protected

      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Create a VPF file.
      /// </summary>
      /// <param name="_path">
      ///   path to VPF database
      /// </param>
      constructor Create( const _path : String ) ;

      /// <summary>
      ///   Read all features.
      /// </summary>
      procedure ReadAll ;

      /// <summary>
      ///   Return a text description of an object.
      /// </summary>
      /// <returns>
      ///    file information
      /// </returns>
      function GetInfo : String ;

     /// <summary>
     ///   Get a list of layers available in storage.
     /// </summary>
     /// <returns>
     ///   list of layers available in storage
     /// </returns>
      function GetAvailableLayers : TGIS_LayerInfoList ;
    public
      /// <summary>
      ///   Current features count.
      /// </summary>
      property BusyCount  : Integer read FBusyCount write FBusyCount ;
      /// <summary>
      ///   Current position.
      /// </summary>
      property BusyPos    : Integer read FBusyPos   write FBusyPos ;
      /// <summary>
      ///   Current category.
      /// </summary>
      property Category   : String  read FCategory  write FCategory ;
      /// <summary>
      ///   Current feature.
      /// </summary>
      property Feature    : String  read FFeature   write FFeature ;

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
        event    NewShapeEvent  : TGIS_VPFExecNewShapeEvent
                                                 delegate FOnNewShape ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   NewShape event. Will be fired when a new shape is created.
        /// </summary>
        property NewShapeEvent  : TGIS_VPFExecNewShapeEvent
                                                 read  FOnNewShape
                                                 write FOnNewShape ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   EndShape event. Will be fired when a new shape is built.
        /// </summary>
        event    EndShapeEvent  : TGIS_VPFExecEndShapeEvent
                                                 delegate FOnEndShape ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   EndShape event. Will be fired when a new shape is built.
        /// </summary>
        property EndShapeEvent  : TGIS_VPFExecEndShapeEvent
                                                 read  FOnEndShape
                                                 write FOnEndShape ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   AddPart event. Will be fired when a new part is added to shape.
        /// </summary>
        event    AddPartEvent   : TGIS_VPFExecAddPartEvent
                                                 delegate FOnAddPart ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   AddPart event. Will be fired when a new part is added to shape.
        /// </summary>
        property AddPartEvent   : TGIS_VPFExecAddPartEvent
                                                 read  FOnAddPart
                                                 write FOnAddPart ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   AddCoord event. Will be fired when coordinates are added to shape.
        /// </summary>
        event    AddCoordEvent  : TGIS_VPFExecAddCoordEvent
                                                 delegate FOnAddCoord ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   AddCoord event. Will be fired when coordinates are added to shape.
        /// </summary>
        property AddCoordEvent  : TGIS_VPFExecAddCoordEvent
                                                 read  FOnAddCoord
                                                 write FOnAddCoord ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   AddAttr event. Will be fired when attributes are added to shape.
        /// </summary>
        event    AddAttrEvent   : TGIS_VPFExecAddAttrEvent
                                                 delegate FOnAddAttr ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   AddAttr event. Will be fired when attributes are added to shape.
        /// </summary>
        property AddAttrEvent   : TGIS_VPFExecAddAttrEvent
                                                 read  FOnAddAttr
                                                 write FOnAddAttr ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   SetLayer event. Will be fired when layer is set for shape.
        /// </summary>
        event    SetLayerEvent  : TGIS_VPFExecSetLayerEvent
                                                 delegate FOnSetLayer ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   SetLayer event. Will be fired when layer is set for shape.
        /// </summary>
        property SetLayerEvent  : TGIS_VPFExecSetLayerEvent
                                                 read  FOnSetLayer
                                                 write FOnSetLayer ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.SysUtils,
    System.Classes,
    System.Math,
    System.Variants,
    GisClasses,
    GisFunctions,
    GisInternals,
    GisStreams  ;
{$ENDIF}

//=============================================================================
// VPF defines
//=============================================================================

const

  { LEAST_SIGNIF_FIRST stores code for indicator of byte order
    least-significant-byte-first used during creating table. It is little-endian
    byte order used on Intel x86 based PCs but not in JVM.
  }
  LEAST_SIGNIF_FIRST = 'L' ;
  { Variable constant LITTLE_ENDIAN_ORDER keeps value of
  }
  LITTLE_ENDIAN_ORDER = LEAST_SIGNIF_FIRST ;
  { MOST_SIGNIF_FIRST stores code for indicator of byte order
    most-significant-byte-first used during creating table. It is big-endian byte
    order used on Motorola CPU based machines and in JVM.
  }
  MOST_SIGNIF_FIRST = 'M' ;
  { Variable constant BIG_ENDIAN_ORDER keeps value of }
  BIG_ENDIAN_ORDER = MOST_SIGNIF_FIRST ;
  { Variable constant DATA_TEXT keeps value of }
  DATA_TEXT = 'T';
  { Variable constant DATA_LEVEL1_TEXT keeps value of }
  DATA_LEVEL1_TEXT = 'L';
  { Variable constant DATA_LEVEL2_TEXT keeps value of }
  DATA_LEVEL2_TEXT = 'N';
  { Variable constant DATA_LEVEL3_TEXT keeps value of }
  DATA_LEVEL3_TEXT = 'M';
  { Variable constant DATA_SHORT_FLOAT keeps value of }
  DATA_SHORT_FLOAT = 'F';
  { Variable constant DATA_LONG_FLOAT keeps value of }
  DATA_LONG_FLOAT = 'R';
  { Variable constant DATA_SHORT_INTEGER keeps value of }
  DATA_SHORT_INTEGER = 'S';
  { Variable constant DATA_LONG_INTEGER keeps value of }
  DATA_LONG_INTEGER = 'I';
  { Variable constant DATA_2_COORD_F keeps value of }
  DATA_2_COORD_F = 'C';
  { Variable constant DATA_2_COORD_R keeps value of }
  DATA_2_COORD_R = 'B';
  { Variable constant DATA_3_COORD_F keeps value of }
  DATA_3_COORD_F = 'Z';
  { Variable constant DATA_3_COORD_R keeps value of }
  DATA_3_COORD_R = 'Y';
  { Variable constant DATA_DATE_TIME keeps value of }
  DATA_DATE_TIME = 'D';
  { Variable constant DATA_NULL_FIELD keeps value of }
  DATA_NULL_FIELD = 'X';
  { Variable constant DATA_TRIPLET_ID keeps value of }
  DATA_TRIPLET_ID = 'K';
  { Variable constant DATA_SHORT_FLOAT_LEN keeps value of }
  DATA_SHORT_FLOAT_LEN = 4;
  { Variable constant DATA_LONG_FLOAT_LEN keeps value of }
  DATA_LONG_FLOAT_LEN = 8;
  { Variable constant DATA_SHORT_INTEGER_LEN keeps value of }
  DATA_SHORT_INTEGER_LEN = 2;
  { Variable constant DATA_LONG_INTEGER_LEN keeps value of }
  DATA_LONG_INTEGER_LEN = 4;
  { Variable constant DATA_DATE_TIME_LEN keeps value of }
  DATA_DATE_TIME_LEN = 20;
  { Variable constant DATA_2_COORD_F_LEN keeps value of }
  DATA_2_COORD_F_LEN = 8;
  { Variable constant DATA_2_COORD_R_LEN keeps value of }
  DATA_2_COORD_R_LEN = 16;
  { Variable constant DATA_3_COORD_F_LEN keeps value of }
  DATA_3_COORD_F_LEN = 12;
  { Variable constant DATA_3_COORD_R_LEN keeps value of }
  DATA_3_COORD_R_LEN = 24;
  { Variable constant DATA_TRIPLET_ID_LEN keeps value of }
  DATA_TRIPLET_ID_LEN = -2;
  { Variable constant DATA_NULL_FIELD_LEN keeps value of }
  DATA_NULL_FIELD_LEN = 0;
  { Variable constant STRING_NULL_VALUE keeps value of }
  STRING_NULL_VALUE = '-';
  { Variable constant STRING_NULL_VALUES keeps value of }
  STRING_NULL_VALUES : array [0..2] of String = (  '-', '--', 'N/A' ) ;
  { Variable constant CHAR_NULL_VALUE keeps value of }
  CHAR_NULL_VALUE = '-';
  { Array of attributes the FACC Code information is allowed to be stored under. }
  ALLOWED_FCODE_ATTRIBUTES : array [0..1] of String = (  'f_code', 'facc' ) ;
  { Feature types. }
  FEATURE_POINT     = 'P';
  FEATURE_LINE      = 'L';
  FEATURE_TEXT      = 'T';
  FEATURE_AREA      = 'A';
  { Field defines. }
  FIELD_CLASS       = 'fclass';
  FIELD_TYPE        = 'type';
  FIELD_DESCRIPTION = 'descr';
  { Variable constant VPF_ELEMENT_SEPARATOR keeps value of }
  VPF_ELEMENT_SEPARATOR = ',';
  { Variable constant VPF_FIELD_SEPARATOR keeps value of }
  VPF_FIELD_SEPARATOR = ':';
  { Variable constant VPF_RECORD_SEPARATOR keeps value of }
  VPF_RECORD_SEPARATOR = ';';
  { KEY_PRIMARY stores code for primary key identification. }
  KEY_PRIMARY = 'P';
  { KEY_UNIQUE stores code for unique key identification. }
  KEY_UNIQUE = 'U';
  { KEY_NON_UNIQUE stores code for non unique key identification.  }
  KEY_NON_UNIQUE = 'N';
  { COLUMN_OPTIONAL stores code for optional column. }
  COLUMN_OPTIONAL = 'O';
  { COLUMN_OPTIONAL_FP stores code for optional feature pointer column. }
  COLUMN_OPTIONAL_FP = 'OF';
  { COLUMN_MANDATORY stores code for mandatory column. }
  COLUMN_MANDATORY = 'M';
  { COLUMN_MANDATORY_AT_LEVEL_0 stores code for mandatory column at topology level 0. }
  COLUMN_MANDATORY_AT_LEVEL_0 = 'M0';
  { COLUMN_MANDATORY_AT_LEVEL_1 stores code for mandatory column at topology level 1. }
  COLUMN_MANDATORY_AT_LEVEL_1 = 'M1';
  { COLUMN_MANDATORY_AT_LEVEL_2 stores code for mandatory column at topology level 2. }
  COLUMN_MANDATORY_AT_LEVEL_2 = 'M2';
  { COLUMN_MANDATORY_AT_LEVEL_3 stores code for mandatory column at topology level 3.   }
  COLUMN_MANDATORY_AT_LEVEL_3 = 'M3';
  { COLUMN_MANDATORY_IF_TILES stores code for mandatory columnif tiles exists. }
  COLUMN_MANDATORY_IF_TILES = 'MT';
  { Variable constant COVERAGE_ATTRIBUTE_TABLE keeps value of }
  COVERAGE_ATTRIBUTE_TABLE = 'cat';
  { Variable constant TABLE_CAT keeps value of }
  TABLE_CAT = COVERAGE_ATTRIBUTE_TABLE;
  { Variable constant CONNECTED_NODE_PRIMITIVE keeps value of }
  CONNECTED_NODE_PRIMITIVE = 'cnd';
  { Variable constant TABLE_CND keeps value of }
  TABLE_CND = CONNECTED_NODE_PRIMITIVE;
  { Variable constant CONNECTED_NODE_SPATIAL_INDEX keeps value of }
  CONNECTED_NODE_SPATIAL_INDEX = 'csi';
  { Variable constant TABLE_CSI keeps value of }
  TABLE_CSI = CONNECTED_NODE_SPATIAL_INDEX;
  { Variable constant DATABASE_HEADER_TABLE keeps value of }
  DATABASE_HEADER_TABLE = 'dht';
  { Variable constant TABLE_DHT keeps value of }
  TABLE_DHT = DATABASE_HEADER_TABLE;
  { Variable constant DATA_QUALITY_TABLE keeps value of }
  DATA_QUALITY_TABLE = 'dqt';
  { Variable constant TABLE_DQT keeps value of }
  TABLE_DQT = DATA_QUALITY_TABLE;
  { Variable constant EDGE_BOUNDING_RECTANGLE keeps value of }
  EDGE_BOUNDING_RECTANGLE = 'ebr';
  { Variable constant TABLE_EBR keeps value of }
  TABLE_EBR = EDGE_BOUNDING_RECTANGLE;
  { Variable constant EDGE_PRIMITIVE keeps value of }
  EDGE_PRIMITIVE = 'edg';
  { Variable constant TABLE_EDG keeps value of }
  TABLE_EDG = EDGE_PRIMITIVE;
  { Variable constant ENTITY_NODE_PRIMITIVE keeps value of }
  ENTITY_NODE_PRIMITIVE = 'end';
  { Variable constant TABLE_END keeps value of }
  TABLE_END = ENTITY_NODE_PRIMITIVE;
  { Variable constant EDGE_SPATIAL_INDEX keeps value of }
  EDGE_SPATIAL_INDEX = 'esi';
  { Variable constant TABLE_ESI keeps value of }
  TABLE_ESI = EDGE_SPATIAL_INDEX;
  { Variable constant FACE_PRIMITIVE keeps value of }
  FACE_PRIMITIVE = 'fac';
  { Variable constant TABLE_FAC keeps value of }
  TABLE_FAC = FACE_PRIMITIVE;
  { Variable constant FACE_BOUNDING_RECTANGLE keeps value of   }
  FACE_BOUNDING_RECTANGLE = 'fbr';
  { Variable constant TABLE_FBR keeps value of }
  TABLE_FBR = FACE_BOUNDING_RECTANGLE;
  { Variable constant FEATURE_CLASS_ATTRIBUTE_TABLE keeps value of   }
  FEATURE_CLASS_ATTRIBUTE_TABLE = 'fca';
  { Variable constant TABLE_FCA keeps value of }
  TABLE_FCA = FEATURE_CLASS_ATTRIBUTE_TABLE;
  { Variable constant FEATURE_CLASS_SCHEMA_TABLE keeps value of }
  FEATURE_CLASS_SCHEMA_TABLE = 'fcs';
  { Variable constant TABLE_FCS keeps value of }
  TABLE_FCS = FEATURE_CLASS_SCHEMA_TABLE;
  { Variable constant FACE_SPATIAL_INDEX keeps value of }
  FACE_SPATIAL_INDEX = 'fsi';
  { Variable constant TABLE_FSI keeps value of }
  TABLE_FSI = FACE_SPATIAL_INDEX;
  { Variable constant GEOGRAPHIC_REFERENCE_TABLE keeps  value of }
  GEOGRAPHIC_REFERENCE_TABLE = 'grt';
  { Variable constant TABLE_GRT keeps value of }
  TABLE_GRT = GEOGRAPHIC_REFERENCE_TABLE;
  { Variable constant LIBRARY_ATTTIBUTE_TABLE keeps value of }
  LIBRARY_ATTTIBUTE_TABLE = 'lat';
  { Variable constant TABLE_LAT keeps value of }
  TABLE_LAT = LIBRARY_ATTTIBUTE_TABLE;
  { Variable constant LIBRARY_HEADER_TABLE keeps value of }
  LIBRARY_HEADER_TABLE = 'lht';
  { Variable constant TABLE_LHT keeps value of }
  TABLE_LHT = LIBRARY_HEADER_TABLE;
  { Variable constant ENTITY_NODE_SPATIAL_INDEX keeps value of }
  ENTITY_NODE_SPATIAL_INDEX = 'nsi';
  { Variable constant TABLE_NSI keeps value of }
  TABLE_NSI = ENTITY_NODE_SPATIAL_INDEX;
  { Variable constant RING_TABLE keeps value of }
  RING_TABLE = 'rng';
  { Variable constant TABLE_RNG keeps value of }
  TABLE_RNG = RING_TABLE;
  { Variable constant TEXT_PRIMITIVE keeps value of }
  TEXT_PRIMITIVE = 'txt';
  { Variable constant TABLE_TXT keeps value of }
  TABLE_TXT = TEXT_PRIMITIVE;
  { Variable constant TEXT_SPATIAL_INDEX keeps value of }
  TEXT_SPATIAL_INDEX = 'tsi';
  { Variable constant TABLE_TSI keeps value of }
  TABLE_TSI = TEXT_SPATIAL_INDEX;
  { Variable constant CHARACTER_VALUE_DESCRIPTION_TABLE keeps value of    }
  CHARACTER_VALUE_DESCRIPTION_TABLE = 'char.vdt';
  { Variable constant TABLE_CHAR keeps value of }
  TABLE_CHAR = CHARACTER_VALUE_DESCRIPTION_TABLE;
  { Variable constant INTEGER_VALUE_DESCRIPTION_TABLE keeps value of   }
  INTEGER_VALUE_DESCRIPTION_TABLE = 'int.vdt';
  { Variable constant TABLE_INT keeps value of }
  TABLE_INT = INTEGER_VALUE_DESCRIPTION_TABLE;
  { Variable constant AREA_BOUMDING_RECTANGLE_TABLE keeps value of }
  AREA_BOUMDING_RECTANGLE_TABLE = '.abr';
  { Variable constant EXT_ABR keeps value of }
  EXT_ABR = AREA_BOUMDING_RECTANGLE_TABLE;
  { Variable constant AREA_FEATURE_TABLE keeps value of }
  AREA_FEATURE_TABLE = '.aft';
  { Variable constant EXT_AFT keeps value of }
  EXT_AFT = AREA_FEATURE_TABLE;
  { Variable constant AREA_JOIN_TABLE keeps value of }
  AREA_JOIN_TABLE = '.ajt';
  { Variable constant EXT_AJT keeps value of }
  EXT_AJT = AREA_JOIN_TABLE;
  { Variable constant AREA_THEMATIC_INDEX keeps value of }
  AREA_THEMATIC_INDEX = '.ati';
  { Variable constant EXT_ATI keeps value of }
  EXT_ATI = AREA_THEMATIC_INDEX;
  { Variable constant COMPLEX_BOUNDING_RECTANGLE_TABLE keeps value of }
  COMPLEX_BOUNDING_RECTANGLE_TABLE = '.cbr';
  { Variable constant EXT_CBR keeps value of }
  EXT_CBR = COMPLEX_BOUNDING_RECTANGLE_TABLE;
  { Variable constant COMPLEX_FEATURE_TABLE keeps value of }
  COMPLEX_FEATURE_TABLE = '.cft';
  { Variable constant EXT_CFT keeps value of }
  EXT_CFT = COMPLEX_FEATURE_TABLE;
  { Variable constant COMPLEX_JOIN_TABLE keeps value of }
  COMPLEX_JOIN_TABLE = '.cjt';
  { Variable constant EXT_CJT keeps value of }
  EXT_CJT = COMPLEX_JOIN_TABLE;
  { Variable constant COMPLEX_THEMATIC_INDEX keeps value of }
  COMPLEX_THEMATIC_INDEX = '.cti';
  { Variable constant EXT_CTI keeps value of }
  EXT_CTI = COMPLEX_THEMATIC_INDEX;
  { Variable constant NARRATIVE_TABLE keeps value of }
  NARRATIVE_TABLE = '.doc';
  { Variable constant EXT_DOC keeps value of }
  EXT_DOC = NARRATIVE_TABLE;
  { Variable constant DIAGNOSITC_POINT_TABLE keeps value of }
  DIAGNOSITC_POINT_TABLE = '.dpt';
  { Variable constant EXT_DPT keeps value of }
  EXT_DPT = DIAGNOSITC_POINT_TABLE;
  { Variable constant FEATURE_INDEX_TABLE keeps value of }
  FEATURE_INDEX_TABLE = '.fit';
  { Variable constant EXT_FIT keeps value of }
  EXT_FIT = FEATURE_INDEX_TABLE;
  { Variable constant FEATURE_THEMATIC_INDEX keeps value of }
  FEATURE_THEMATIC_INDEX = '.fti';
  { Variable constant EXT_FTI keeps value of }
  EXT_FTI = FEATURE_THEMATIC_INDEX;
  { Variable constant JOIN_THEMATIC_INDEX keeps value of }
  JOIN_THEMATIC_INDEX = '.jti';
  { Variable constant EXT_JTI keeps value of }
  EXT_JTI = JOIN_THEMATIC_INDEX;
  { Variable constant LINE_BOUNDING_RECTANGLE_TABLE keeps value of  }
  LINE_BOUNDING_RECTANGLE_TABLE = '.lbr';
  { Variable constant EXT_LBR keeps value of }
  EXT_LBR = LINE_BOUNDING_RECTANGLE_TABLE;
  { Variable constant LINE_FEATURE_TABLE keeps value of }
  LINE_FEATURE_TABLE = '.lft';
  { Variable constant EXT_LFT keeps value of }
  EXT_LFT = LINE_FEATURE_TABLE;
  { Variable constant LINE_JOIN_TABLE keeps value of }
  LINE_JOIN_TABLE = '.ljt';
  { Variable constant EXT_LJT keeps value of }
  EXT_LJT = LINE_JOIN_TABLE;
  { Variable constant LINE_THEMATIC_INDEX keeps value of }
  LINE_THEMATIC_INDEX = '.lti';
  { Variable constant EXT_LTI keeps value of }
  EXT_LTI = LINE_THEMATIC_INDEX;
  { Variable constant POINT_BOUNDING_RECTANGLE_TABLE keepsvalue of }
  POINT_BOUNDING_RECTANGLE_TABLE = '.pbr';
  { Variable constant EXT_PBR keeps value of }
  EXT_PBR = POINT_BOUNDING_RECTANGLE_TABLE;
  { Variable constant POINT_FEATURE_TABLE keeps value of }
  POINT_FEATURE_TABLE = '.pft';
  { Variable constant EXT_PFT keeps value of }
  EXT_PFT = POINT_FEATURE_TABLE;
  { Variable constant POINT_JOIN_TABLE keeps value of }
  POINT_JOIN_TABLE = '.pjt';
  { Variable constant EXT_PJT keeps value of }
  EXT_PJT = POINT_JOIN_TABLE;
  { Variable constant POINT_THEMATIC_INDEX keeps value of }
  POINT_THEMATIC_INDEX = '.pti';
  { Variable constant EXT_PTI keeps value of }
  EXT_PTI = POINT_THEMATIC_INDEX;
  { Variable constant RELATED_ATTRIBUTE_TABLE keeps value of  }
  RELATED_ATTRIBUTE_TABLE = '.rat';
  { Variable constant EXT_RAT keeps value of }
  EXT_RAT = RELATED_ATTRIBUTE_TABLE;
  { Variable constant REGISTRATION_POINT_TABLE keeps value of  }
  REGISTRATION_POINT_TABLE = '.rpt';
  { Variable constant EXT_RPT keeps value of }
  EXT_RPT = REGISTRATION_POINT_TABLE;
  { Variable constant TEXT_FEATURE_TABLE keeps value of }
  TEXT_FEATURE_TABLE = '.tft';
  { Variable constant EXT_TFT keeps value of }
  EXT_TFT = TEXT_FEATURE_TABLE;
  { Variable constant TEXT_THEMATIC_TABLE keeps value of }
  TEXT_THEMATIC_TABLE = '.tti';
  { Variable constant EXT_TTI keeps value of }
  EXT_TTI = TEXT_THEMATIC_TABLE;
  { Variable constant LIBRARY_REFERENCE_COVERAGE keeps value of }
  LIBRARY_REFERENCE_COVERAGE = 'libref';
  { Variable constant DIR_LIBREF keeps value of }
  DIR_LIBREF = LIBRARY_REFERENCE_COVERAGE;
  { Variable constant DATA_QUALITY_COVERAGE keeps value of }
  DATA_QUALITY_COVERAGE = 'dq';
  { Variable constant DIR_DQ keeps value of }
  DIR_DQ = DATA_QUALITY_COVERAGE;
  { Variable constant TILE_REFERENCE_COVERAGE keeps value of }
  TILE_REFERENCE_COVERAGE = 'tileref';
  { Variable constant DIR_TILEREF keeps value of }
  DIR_TILEREF = TILE_REFERENCE_COVERAGE;
  { Variable constant NAMES_REFERENCE_COVERAGE keeps value of }
  NAMES_REFERENCE_COVERAGE = 'gazette';
  { Variable constant DIR_GAZETTE keeps value of }
  DIR_GAZETTE = NAMES_REFERENCE_COVERAGE;
  { Field coverage defines. }
  FIELD_COVERAGE_NAME   = 'coverage_name';
  FIELD_COVERAGE_DESC   = 'description';
  FIELD_COVERAGE_LEVEL  = 'level';
  { Describe constant FIELD_XMIN here.}
  FIELD_XMIN = 'xmin';
  { Describe constant FIELD_YMIN here.}
  FIELD_YMIN = 'ymin';
  { Describe constant FIELD_XMAX here. }
  FIELD_XMAX = 'xmax';
  { Describe constant FIELD_YMAX here.}
  FIELD_YMAX = 'ymax';
  { Describe constant FIELD_LIB_NAME here.}
  FIELD_LIB_NAME  = 'library_name';
  FIELD_TILE_NAME = 'tile_name';
  FIELD_TILE_ID   = 'id';

//=============================================================================
// VPF types
//=============================================================================

type
  { Row class.
  }
  VPFRow = class

  end ;

  { Header class.
  }
  VPFHeader = class

      // Get a record length.
      // return length
      function GetLength     : Integer ; virtual;

      // Get a record size.
      // return size
      function GetRecordSize : Integer ; virtual;
  end ;

  { Variable index header class.
  }
  VariableIndexHeader = class( VPFHeader )
    private
      FentriesNumber : Integer ;
      FvpfHeaderLen  : Integer ;
    public

      // Create a variable index header.
      // _entriesNumber   number of elements
      // _vpfHeaderLen    header length
      constructor Create( const _entriesNumber  : Integer ;
                          const _vpfHeaderLen   : Integer
                         ) ;

      // Get a record length.
      // return length
      function GetLength     : Integer ; override;

          // Get a record size.
      // return size
      function GetRecordSize : Integer ; override;
  end ;

  { Spatia index header class.
  }
  SpatialIndexHeader = class( VPFHeader )
    private
      FnumPrims  : Integer ;
      Fxmin      : Double ;
      Fymin      : Double ;
      Fxmax      : Double ;
      Fymax      : Double ;
      FnumNodes  : Integer ;
    public
      constructor Create( const _numPrims  : Integer ;
                          const _xmin      : Double ;
                          const _ymin      : Double ;
                          const _xmax      : Double ;
                          const _ymax      : Double ;
                          const _numNodes  : Integer
                         ) ;

          // Get a record length.
      // return length
      function GetLength     : Integer ; override;

           // Get a record size.
       // return size
      function GetRecordSize : Integer ; override;
  end ;

  { Input stream class.
  }
  VPFInputStream = class( TGIS_ObjectDisposable )
    protected
      Finput         : TGIS_Stream ;
      Fheader        : VPFHeader ;
      FstreamFile    : String ;
      FvariableIndex : VPFInputStream ;
      FbyteOrder     : Char ;
    protected
      function readHeader : VPFHeader ; virtual;
      function getVariableIndexFileName : String ;
      function readInteger : Integer ;
      function readFloat : Double ;
      function readNumber( const _cnt : Integer )    : TBytes ;

      procedure doDestroy ; override;
    public
      constructor Create( const _file      : String ;
                          const _byteOrder : Char
                         ) ; virtual;

      function  ReadRow( const _index : Integer ) : VPFRow ; overload;virtual;
      function  ReadRow : VPFRow ; overload; virtual;
      procedure SetPosition( const _pos : Int64 ) ; virtual;
  end ;

  { Variable input stream class.
  }
  VariableIndexInputStream = class( VPFInputStream )
    protected
      function readHeader : VPFHeader ; override;
    public
      constructor Create( const _file      : String ;
                          const _byteOrder : Char
                         ) ; override;
      function  ReadRow : VPFRow ; override;
  end ;

  { Spatial index input stream class.
  }
  SpatialIndexInputStream = class( VPFInputStream )
    protected
      function readHeader : VPFHeader ; override;
    public
      constructor Create( const _file      : String ;
                          const _byteOrder : Char
                         ) ; override;
      function  ReadRow : VPFRow ; override;
      procedure SetPosition( const _pos : Int64 ) ; override;
  end ;

  { Column class.
  }
  VPFColumn = class
    private
      FName             : String ;
      FattemptLookup    : Boolean ;
      FElementsNumber   : Integer ;
      FnarrTable        : String ;
      FkeyType          : Char ;
      FcolDesc          : String ;
      FthematicIdx      : String ;
      FTypeChar         : Char ;
      FvalDescTableName : String ;
    private
      function getColumnSize : Integer ;
    public
      constructor Create( const _name : String ;
                          const _type : Char ;
                          const _elementsNumber : Integer ;
                          const _keyType : Char ;
                          const _colDesc : String ;
                          const _valDescTableName : String ;
                          const _thematicIdx : String ;
                          const _narrTable : String
                         ) ;

      // Return a text description of an object.
      // return   description
      function GetInfo : String ;
    public
      property Name             : String  read FName ;
      property TypeChar         : Char    read FTypeChar ;
      property ElementsNumber   : Integer read FElementsNumber ;
      property ColumnSize       : Integer read getColumnSize ;
  end ;

  { Tripleid class.
  }
  TripletId = class
  private
    FRawData : TBytes ;
  public
    class function calculateDataSize(const _definition : Byte ) : Integer ;
    constructor Create( const _data : TBytes ) ;

    function GetId    : Integer ;
  public
    property IntValue : Integer  read GetId;
    property RawData  : TBytes   read FRawData ;
  end ;

  { Field value class.
  }
  FieldValue = class( TGIS_Object )
    private
      FValue  : Variant ;
      FObj    : TObject ;
      FName   : String ;
      FType   : TGIS_FieldType ;
    protected
      function getDouble      : Double ;
      function getString      : String ;
      function getInteger     : Integer ;
      function getIsGeometry  : Boolean ;
      function getTripletId   : TripletId ;
      function getGeometry    : TGIS_Point3DList ;
      function getIsTripletid : Boolean;

      procedure doDestroy; override;
    public
      {$IFDEF OXYGENE}
      constructor Create( const _name  : String ;
                          const _value : String ;
                          const _type  : TGIS_FieldType
                         ) ; overload;
      constructor Create( const _name  : String ;
                          const _value : Double ;
                          const _type  : TGIS_FieldType
                         ) ; overload;
      constructor Create( const _name  : String ;
                          const _value : Integer ;
                          const _type  : TGIS_FieldType
                         ) ; overload;
      constructor Create( const _name  : String ;
                          const _value : TObject ;
                          const _type  : TGIS_FieldType
                         ) ; overload;
      {$ELSE}
      constructor Create( const _name  : String ;
                          const _value : Variant ;
                          const _type  : TGIS_FieldType
                         ) ; overload;
      {$ENDIF}
      constructor Create( const _name  : String ;
                          const _value : TObject
                         ) ; overload;
      constructor Create( const _name : String ;
                          const _type : TGIS_FieldType
                         ) ; overload;

      function  Clone    : FieldValue ;

      // Return a text description of an object.
      // return   description
          function  GetInfo : String ;
      procedure SetObject( const _obj : TObject ) ;
      procedure SetValue ( const _val : Variant ) ;
    public
      property Name         : String            read FName ;
      property FieldType    : TGIS_FieldType    read FType ;
      property Value        : Variant           read FValue ;
      property AsDouble     : Double            read getDouble ;
      property AsString     : String            read getString;
      property AsInteger    : Integer           read getInteger ;
      property AsTripletId  : TripletId         read getTripletId ;
      property AsGeometry   : TGIS_Point3DList  read getGeometry ;
      property IsGeometry   : Boolean           read getIsGeometry ;
      property IsTripletId  : Boolean           read getIsTripletid ;
  end ;

  {
  }
  TGIS_Point3DListArray = array of TGIS_Point3DList ;

  { Simple feature class.
  }
  SimpleFeature = class( TGIS_Object )
    private
      Values        : TStringList ;
      Geometry      : TGIS_Point3DList ;
      GeometryArray : TGIS_Point3DListArray ;
      ShapeType     : TGIS_ShapeType ;
    protected
      procedure doDestroy; override;
    public
      constructor Create ;

      procedure AddValue( const _value : FieldValue ) ;
      function  GetValue( const _name : String ) : FieldValue ;
      function  HasValue( const _name : String ) : Boolean ;
      procedure SetGeometry( const _geometry : TGIS_Point3DList ) ; overload;
      procedure SetGeometry( const _geometryArray : TGIS_Point3DListArray ) ; overload;
      procedure SetShapeType( const _type : TGIS_ShapeType ) ;
      procedure CopyValues( const _feature : SimpleFeature ) ;
      procedure ClearValues ;

          // Return a text description of an object.
          // return   description
          function  GetInfo : String ;
      procedure BuildShape( const _parent : TGIS_FileVPF ) ;
  end ;

  { Data utils class.
  }
  DataUtils = class

    // Convert data to format.
    // _bytes   buffer
    // return   value
    class function toBigEndian( const _bytes : TBytes ) : TBytes ;

    // Convert data to format.
    // _bytes   buffer
    // return   value
    class function decodeShort( const _bytes : TBytes ) : Word ;

    // Convert data to format.
    // _bytes   buffer
    // return   value
    class function decodeInt( const _bytes : TBytes ) : Integer ;

    // Convert data to format.
    // _bytes   buffer
    // return   value
    class function decodeDouble( const _bytes : TBytes ) : Double ;

    // Convert data to format.
    // _bytes   buffer
    // return   value
    class function decodeFloat( const _bytes : TBytes ) : Single ;

    // Convert data to format.
    // _bytes   buffer
    // return   value
    class function decodeDate(const _bytes : TBytes ) : TDateTime ;

    // Convert data to format.
    // _bytes   buffer
    // _type    type value
    // return    value
        class function decodeData(const _bytes : TBytes ;const _type : Char ) : Variant ;

   // Get data type size.
   // _type  type value
   // return size
   class function getDataTypeSize(const _type : Char ) : Integer ;
  end ;

  { VPF file class.
  }
  VPFFile = class( TGIS_ObjectDisposable )
    private
     { Keeps value of  byte order in which table is written. }
      FbyteOrder      : Char ;
     { The columns of the file. This list shall contain objects of type VPFColumn.}
      Fcolumns        : TGIS_ObjectList ;
     { Keeps value of text description of the table's contents. }
      Fdescription    : String ;
     { Keeps value of length of ASCII header string. }
      FheaderLength   : Integer ;
     { The associated stream. }
      FinputStream    : TGIS_Stream ;
     { Keeps value of an optional narrative file which contains miscellaneous
       information about the table. }
      FnarrativeTable : String ;
     { The path name. }
      FpathName       : String ;
     { Variable index stream. }
      FvariableIndex  : VPFInputStream ;

      FBufFloat      : TBytes ;
      FBufInt        : TBytes ;
    private
      procedure readHeader ;
      function  readChar      : Char ;
      function  readDouble    : Double ;
      function  readFloat     : Double ;
      function  readColumn    : VPFColumn ;
      function  readInteger   : Integer ;
      function  readString    ( const _terminators : String ) : String ;
      function  readNumber( const _cnt : Integer )    : TBytes ;
      function  readShort : Word ;
      function  readTripletId : TripletId ;
      function  readFixedSizeData( const _name : String ;
                                   const _dataType : Char ;
                                   const _instancesCount : Integer
                                  ) : FieldValue ;
      function  readVariableSizeData( const _name : String ;
                                      const _dataType : Char ) : FieldValue ;
      function  readGeometry( const _instancesCount : Integer ;
                              const _dimensionality : Integer ;
                              const _readDoubles    : Boolean
                             ) : TObject ;
      function  getRecordSize : Integer ;
      function  getVariableIndexFileName : String ;

      procedure unread( const _bytes : Integer ) ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create( const _path : String ) ;

      function  HasNext : Boolean ;
      function  ReadFeature : SimpleFeature ;
      function  ReadAllRows : TGIS_ObjectList ;
      function  GetRowFromId( const _idName : String ;
                              const _id     : Integer
                             ) : SimpleFeature ;
      procedure Reset ;
      procedure Close ;
      function  GetColumn( const _name  : String  ) : VPFColumn ; overload;
      function  GetColumn( const _index : Integer ) : VPFColumn ; overload;
      procedure SetPosition( const _pos : Int64 ) ;

      // Return a text description of an object.
      // return   description
      function GetInfo : String ;
  end ;

  { VPF file store.
  }
  VPFFileStore = class( TGIS_ObjectDisposable )
    private
      Ffiles : TStringList ;
    protected
      procedure doDestroy ; override;
    public


      constructor Create ;

          // Get a file from the FileStore.
      // _path  file path
      // return        file handle
      function GetFile( const _path : String ) : VPFFile ;
  end ;

  VPFCoverage       = class ;
  VPFFeatureType    = class ;
  VPFFeatureReader  = class ;
  VPFLibrary        = class ;

  { VPF database class.
  }
  VPFDataBase = class( TGIS_Object )
    private
      libraries : TGIS_ObjectList ;
    protected
      function getLibCount : Integer ;
      function getLibrary( const _idx : Integer ) : VPFLibrary ;

      procedure doDestroy ; override;
    public
      constructor Create( const _path : String ) ;

      // Return a text description of an object.
      // return   description
      function GetInfo : String ;
    public
      property LibraryCount : Integer read getLibCount ;
      property VLibrary[ const _idx : Integer ] : VPFLibrary read getLibrary ;
  end ;

  { VPF library class.
  }
  VPFLibrary = class( TGIS_Object )
    private
      xmin        : Double ;
      ymin        : Double ;
      xmax        : Double ;
      ymax        : Double ;
      directory   : String;
      libraryName : String ;
      coverages   : TGIS_ObjectList ;
      FtileMap     : TStringList ;
      FisValid     : Boolean ;
    private
      procedure setCoverages ;
      procedure createTilingSchema( const _coverage : VPFCoverage ) ;
      function  getSchema( const _typeName : String ) : VPFFeatureType ;
    protected
      function  getCoverageCount : Integer ;
      function  getCoverage( const _idx : Integer ) : VPFCoverage ;
      function  getTileMap( const _name : String ) : String ;
      procedure doDestroy ; override;
    public
      constructor Create( const _libraryFeature : SimpleFeature ;
                          const _path    : String
                         ) ; overload;
      constructor Create( const _path    : String
                         ) ; overload;

          // Return a text description of an object.
          // return   description
      function  GetInfo : String ;
      function  GetFeatureReader( const _typeName : String ) : VPFFeatureReader ;
      procedure ReadAll( const _parent : TGIS_FileVPF ) ;
    public
      property CoverageCount : Integer read getCoverageCount ;
      property Coverage[ const _idx : Integer ] : VPFCoverage read getCoverage ;
      property TileMap[ const _name : String ] : String read getTileMap ;
      property IsValid : Boolean read FisValid ;
  end ;

  VPFFeatureClass = class;

  { VPF coverage class.
  }
  VPFCoverage = class( TGIS_Object )
    private
      fdescription   : String ;
      featureClasses : TGIS_ObjectList ;
      featureTypes   : TGIS_ObjectList ;
      libraryHandle  : VPFLibrary ;
      pathName       : String ;
      topologyLevel  : Integer ;
    private
      procedure discoverFeatureClasses;
      procedure discoverFeatureTypes;
    protected
      function getName : String ;
      function getFeatureClassCount : Integer ;
      function getFeatureClass( const _idx : Integer ) : VPFFeatureClass ;
      function getFeatureTypeCount : Integer ;
      function getFeatureType( const _idx : Integer ) : VPFFeatureType ;

      procedure doDestroy ; override;
    public
      constructor Create( const _library : VPFLibrary ;
                          const _feature : SimpleFeature ;
                          const _path    : String
                         ) ;

      procedure ReadAll( const _parent : TGIS_FileVPF ;
                           var _abort  : Boolean
                        ) ;

          // Return a text description of an object.
          // return   description
      function  GetInfo : String ;
    public
      property Name : String read getName ;
      property Description : String read fdescription ;
      property FeatureClassCount : Integer read getFeatureClassCount ;
      property FeatureClass[ const _idx : Integer ] : VPFFeatureClass read getFeatureClass ;
      property FeatureTypeCount : Integer read getFeatureTypeCount ;
      property FeatureType[ const _idx : Integer ] : VPFFeatureType read getFeatureType ;
      property VLibrary : VPFLibrary read libraryHandle;
  end ;

  { VPF geometry factory class.
  }
  VPFGeometryFactory = class

    // Create a geometry.
    // _featureType   feature type handle
    // _values        feature type data
    procedure CreateGeometry( const _featureType : VPFFeatureType ;
                              const _values      : SimpleFeature
                             ) ; virtual;
  end ;

  { VPF geometry factory class.
  }
  EntityNodeGeometryFactory = class ( VPFGeometryFactory )

    // Create a geometry.
    // _featureType   feature type handle
    // _values        feature type data
    procedure CreateGeometry( const _featureType : VPFFeatureType ;
                              const _values      : SimpleFeature
                             ) ; override;
  end ;

  { VPF geometry factory class.
  }
  ConnectedNodeGeometryFactory = class ( VPFGeometryFactory )

    // Create a geometry.
    // _featureType   feature type handle
    // _values        feature type data
    procedure CreateGeometry( const _featureType : VPFFeatureType ;
                              const _values      : SimpleFeature
                             ) ; override;
  end ;

  { VPF geometry factory class.
  }
  TextGeometryFactory = class ( VPFGeometryFactory )

    // Create a geometry.
    // _featureType   feature type handle
    // _values        feature type data
    procedure CreateGeometry( const _featureType : VPFFeatureType ;
                              const _values      : SimpleFeature
                             ) ; override;
  end ;

  { VPF geometry factory class.
  }
  LineGeometryFactory = class ( VPFGeometryFactory )

    // Create a geometry.
    // _featureType   feature type handle
    // _values        feature type data
    procedure CreateGeometry( const _featureType : VPFFeatureType ;
                              const _values      : SimpleFeature
                             ) ; override;
  end ;

  { VPF geometry factory class.
  }
  AreaGeometryFactory = class ( VPFGeometryFactory )

    // Create a geometry.
    // _featureType   feature type handle
    // _values        feature type data
    procedure CreateGeometry( const _featureType : VPFFeatureType ;
                              const _values      : SimpleFeature
                             ) ; override;
  end ;

  { Feature type class.
  }
  VPFFeatureType = class
    private
      FFaccCode     : String ;
      FTypeName     : String ;
      FTableName    : String ;
      FDescription  : String ;
      FFeatureClass : VPFFeatureClass ;
      FShapeType    : TGIS_ShapeType ;
    public
      constructor Create( const _featureClass : VPFFeatureClass ;
                          const _feature     : SimpleFeature
                         ) ; overload;
      constructor Create( const _featureClass : VPFFeatureClass
                         ) ; overload;
      procedure ReadAll( const _parent : TGIS_FileVPF ;
                           var _abort  : Boolean
                        ) ;

      // Return a text description of an object.
      // return   description
      function GetInfo : String ;
    public
      property FaccCode     : String          read FFaccCode ;
      property TypeName     : String          read FTypeName ;
      property TableName    : String          read FTableName ;
      property Description  : String          read FDescription ;
      property FeatureClass : VPFFeatureClass read FFeatureClass ;
      property ShapeType    : TGIS_ShapeType  read FShapeType ;
  end ;

  { Feature class type.
  }
  VPFFeatureClass = class( TGIS_Object )
    private
      Fcoverage        : VPFCoverage ;
      FdirectoryName   : String ;
      FfileList        : TGIS_ObjectList ;
      FjoinList        : TGIS_ObjectList ;
      FtypeName        : String ;
      FgeometryFactory : VPFGeometryFactory ;
      FfileStore       : VPFFileStore ;
    private
      procedure addFCS            ( const _row : SimpleFeature ) ;
      procedure addFileToTable    ( const _vpfFile : VPFFile ) ;
      procedure setGeometryFactory( const _table : String ) ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create( const _coverage : VPFCoverage ;
                          const _name     : String ;
                          const _path     : String
                         ) ;

      // Return a text description of an object.
      // return   description
      function  GetInfo : String ;
    public
      property Coverage        : VPFCoverage        read Fcoverage ;
      property DirectoryName   : String             read FdirectoryName ;
      property FileList        : TGIS_ObjectList        read FfileList;
      property JoinList        : TGIS_ObjectList        read FjoinList;
      property TypeName        : String             read FtypeName;
      property GeometryFactory : VPFGeometryFactory read FgeometryFactory;
      property FileStore       : VPFFileStore       read FfileStore;

  end ;

  { Feature reader class.
  }
  VPFFeatureReader = class( TGIS_Object )
    private
      fhasNext       : Boolean ;
      nextCalled     : Boolean ;
      currentFeature : SimpleFeature ;
      currentRow     : SimpleFeature ;
      featureType    : VPFFeatureType ;
    private
      procedure retrieveObject( const _file : VPFFile ;
                                const _row  : SimpleFeature
                               ) ;
      function  generateFileRowMap( const _file : VPFFile ;
                                    const _row  : SimpleFeature
                                   ) : TGIS_ObjectList ;
      function  getVPFFile( const _column : VPFColumn ) : VPFFile ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create( const _type : VPFFeatureType ) ;

      function  HasNext : Boolean ;
      function  ReadNext : Boolean ;
      procedure Reset ;
      function  GetCurrent : SimpleFeature ;
   end ;

  { Column pair class.
  }
  ColumnPair = class
    column1 : VPFColumn ;
    column2 : VPFColumn ;
    public
      constructor Create( const _c1 : VPFColumn ;
                          const _c2 : VPFColumn
                         ) ;
  end ;

  { Variable index row class.
  }
  VariableIndexRow = class( VPFRow )
    private
      FOffset : Integer ;
      FSize   : Integer ;
    public

          // Create a variable index row.
      // _offset  row offset
      // _size    row size
      constructor Create( const _offset : Integer ;
                          const _size   : Integer
                         ) ;
    public
      property Offset : Integer read FOffset ;
      property Size   : Integer read FSize ;
  end ;

  { Mbr index row class.
  }
  MbrIndexRow = class( VPFRow )
    private
      Fid     : Integer ;
      FExtent : TGIS_Extent ;
    public
      constructor Create( const _id        : Integer ;
                          const _xmin      : Double ;
                          const _ymin      : Double ;
                          const _xmax      : Double ;
                          const _ymax      : Double
                         ) ;
    public
      property Id     : Integer     read Fid ;
      property Extent : TGIS_Extent read FExtent ;
  end ;

  procedure Log( const _text : String );
  begin
    {$IFDEF CONSOLE}
      {$IFDEF OXYGENE}
//        Console.WriteLine( _text ) ;
      {$ELSE}
//        WriteLn( _text ) ;
      {$ENDIF}
    {$ENDIF}
  end ;

//=============================================================================
// VPFFile
//=============================================================================

  { Constructor.
  }
  constructor VPFFile.Create(
    const _path : String
  ) ;
  begin
    inherited Create ;

    Fcolumns      := TGIS_ObjectList.Create ;
    FheaderLength := 0 ;
    FpathName     := _path ;
    FinputStream  := TGIS_BufferedFileStream.Create( FpathName, TGIS_StreamMode.Read ) ;

    readHeader ;
    SetLength( FBufFloat, 4 ) ;
    SetLength( FBufInt, 4 ) ;
  end ;

  { Destructor.
  }
  procedure VPFFile.doDestroy ;
  begin
    FreeObject( FinputStream ) ;
    FreeObject( Fcolumns     ) ;

    if assigned( FvariableIndex ) then
      FreeObject( FvariableIndex ) ;

    inherited ;
  end ;

  { Get column.
  }
  function VPFFile.GetColumn(
    const _index : Integer
  ) : VPFColumn ;
  begin
    if ( _index >=0 ) and ( _index <  Fcolumns.Count ) then
      Result := VPFColumn( Fcolumns[ _index ] )
    else
      Result := nil ;
  end ;

  { Get column.
  }
  function VPFFile.GetColumn(
    const _name : String
  ) : VPFColumn ;
  var
    column : VPFColumn ;
    i      : Integer ;
  begin
    Result := nil ;
    for i := 0 to Fcolumns.Count-1 do begin
      column := VPFColumn( Fcolumns[ i ] ) ;
      if column.Name = _name then begin
        Result := column ;
        break ;
      end
    end ;
  end ;

  { Get record size.
  }
  function VPFFile.getRecordSize : Integer ;
  var
    column : VPFColumn ;
    i      : Integer ;
    length : Integer ;
  begin
    Result := 0 ;

    for i := 0 to Fcolumns.Count-1 do begin
      column := VPFColumn( Fcolumns[ i ] ) ;
      length := column.ColumnSize ;
      if length > -1 then
        Result := Result + length
      else begin
        Result := -1 ;
        break ;
      end ;
    end ;
  end ;

  { Get row from id.
  }
  function VPFFile.GetRowFromId(
    const _idName : String ;
    const _id     : Integer
  ) : SimpleFeature ;
  var
    firstColumnName : String;
    value : FieldValue ;
    row : SimpleFeature ;
  begin
    Result := nil ;

    firstColumnName := VPFColumn( Fcolumns[0] ).Name ;
    if _idName = firstColumnName then begin
      SetPosition( _id ) ;
      Result := ReadFeature ;
      value := Result.GetValue( _idName ) ;
      if ( value = nil ) or ( value.AsInteger <> _id ) then
        Result := nil ;
    end ;

    if Result = nil then begin
      SetPosition(1);
      row := ReadFeature ;
      while row <> nil do begin
        if row.GetValue(_idName).AsInteger = _id then begin
          Result := row ;
          break ;
        end ;
        FreeObject( row ) ;
        if HasNext then
          row := ReadFeature
        else
          row := nil ;
      end ;
    end ;
  end ;

  { Get variable index file name.
  }
  function VPFFile.getVariableIndexFileName : String ;
  var
    filename : String ;
  begin
    Result := '' ;

    filename := ExtractFileName( FpathName ) ;

    if LowerCase( filename ) = FEATURE_CLASS_SCHEMA_TABLE then
      Result := GetPathDirSep( ExtractFileDir( FpathName ) ) + 'fcz'
    else
      Result := GetPathDirSep( ExtractFileDir( FpathName ) ) +
                Copy( filename, StringFirst, length(filename)-1 ) + 'x' ;
  end ;

  { Has next.
  }
  function VPFFile.HasNext : Boolean ;
  var
    recordSize : Integer ;
  begin
    recordSize := getRecordSize ;
    if recordSize > 0 then
      Result := FinputStream.Size >= FinputStream.Position + recordSize
    else
      Result := FinputStream.Size >= FinputStream.Position + 1 ;
  end ;

  { Read all rows.
  }
  function VPFFile.ReadAllRows : TGIS_ObjectList ;
  var
    row : SimpleFeature ;
  begin
    Result := TGIS_ObjectList.Create(True) ;

    SetPosition(1);

    row := ReadFeature ;
    while row <> nil do begin
      Result.Add(row);
      if HasNext then
        row := ReadFeature
      else
        row := nil ;
    end ;

  end ;

  { Read column.
  }
  function VPFFile.readColumn : VPFColumn ;
  var
    ctrl    : Char ;
    cname   : String ;
    ctype   : Char ;
    elemstr : String ;
    elements: Integer ;
    key     : Char ;
    colDesc : String ;
    descTableName : String ;
    indexFile  : String ;
    narrTable : String ;
  begin
    Result := nil ;

    ctrl := readChar ;

    if ( ctrl = VPF_RECORD_SEPARATOR ) then exit ;

    cname := ctrl + readString('=') ;
    ctype := readChar ;
    ctrl  := readChar ;

    if ( ctrl <> VPF_ELEMENT_SEPARATOR ) then
      raise EGIS_Exception.Create( 'Header format does not fit VPF file definition.','',0);

    elemstr := Trim( readString( VPF_ELEMENT_SEPARATOR ) ) ;

    if elemstr = '*' then
      elemstr := '-1' ;

    elements := StrToInt( elemstr ) ;
    key  := readChar ;
    ctrl := readChar ;

    if ( ctrl <> VPF_ELEMENT_SEPARATOR ) then
      raise EGIS_Exception.Create( 'Header format does not fit VPF file definition.','',0);

    colDesc       := readString( VPF_ELEMENT_SEPARATOR + VPF_FIELD_SEPARATOR );
    descTableName := readString( VPF_ELEMENT_SEPARATOR + VPF_FIELD_SEPARATOR );
    indexFile     := readString( VPF_ELEMENT_SEPARATOR + VPF_FIELD_SEPARATOR );
    narrTable     := readString( VPF_ELEMENT_SEPARATOR + VPF_FIELD_SEPARATOR );

    Result := VPFColumn.Create( cname, ctype, elements, key, colDesc, descTableName,
                                indexFile, narrTable
                               ) ;
  end ;

  { Read char.
  }
  function VPFFile.readChar : Char ;
  var
    c : Byte ;
  begin
    FinputStream.ReadByte( c, 1 ) ;
    Result := Char( c ) ;
  end ;

  { Read double.
  }
  function VPFFile.readDouble : Double ;
  begin
    Result := DataUtils.decodeDouble(readNumber(DATA_LONG_FLOAT_LEN));
  end ;

  { Read fixed size data.
  }
  function VPFFile.readFixedSizeData(
    const _name           : String ;
    const _dataType       : Char ;
    const _instancesCount : Integer
  ) : FieldValue ;
  var
    databytes : TBytes ;
    cnt       : Integer ;
  begin
    Result := nil ;

    case _dataType of
       DATA_TEXT,
       DATA_LEVEL1_TEXT,
       DATA_LEVEL2_TEXT,
       DATA_LEVEL3_TEXT,
       DATA_DATE_TIME:
        begin
          cnt := _instancesCount * DataUtils.getDataTypeSize(_dataType) ;
          SetLength( databytes, cnt );
          if cnt > 0 then begin
            FinputStream.ReadBytesCnt( databytes, cnt );
            Result := FieldValue.Create( _name,
                                         VarToString( DataUtils.decodeData(databytes, _dataType) ),
                                         TGIS_FieldType.String
                                        )  ;
          end
          else
            Result := FieldValue.Create( _name, NullVar, TGIS_FieldType.String );
        end ;
      DATA_SHORT_FLOAT:
          Result := FieldValue.Create( _name,readFloat, TGIS_FieldType.Float ) ;
      DATA_LONG_FLOAT:
          Result := FieldValue.Create( _name,readDouble, TGIS_FieldType.Float ) ;
      DATA_SHORT_INTEGER:
          Result := FieldValue.Create( _name,readShort, TGIS_FieldType.Number ) ;
      DATA_LONG_INTEGER:
          Result := FieldValue.Create( _name,readInteger, TGIS_FieldType.Number ) ;
       DATA_NULL_FIELD:
          Result := FieldValue.Create( _name, NullVar, TGIS_FieldType.String );
       DATA_TRIPLET_ID:
          Result := FieldValue.Create( _name, readTripletId );
       DATA_2_COORD_F:
          Result := FieldValue.Create( _name,readGeometry(_instancesCount, 2, false));
       DATA_2_COORD_R:
          Result := FieldValue.Create( _name,readGeometry(_instancesCount, 2, true));
       DATA_3_COORD_F:
          Result := FieldValue.Create( _name,readGeometry(_instancesCount, 3, false));
       DATA_3_COORD_R:
          Result := FieldValue.Create( _name,readGeometry(_instancesCount, 3, true));
    end ;
  end ;

  { Read float.
  }
  function VPFFile.readFloat : Double ;
  var
    tmp : Byte ;
  begin
    FinputStream.ReadBytesCnt( FBufFloat, DATA_SHORT_FLOAT_LEN ) ;

    if ( FbyteOrder = LITTLE_ENDIAN_ORDER ) then begin
      tmp := FBufFloat[0] ;
      FBufFloat[0] := FBufFloat[3] ;
      FBufFloat[3] := tmp ;
      tmp := FBufFloat[1] ;
      FBufFloat[1] := FBufFloat[2] ;
      FBufFloat[2] := tmp ;
    end ;

    try
      Result := DataUtils.decodeFloat( FBufFloat ) ;
      if IsNan( Result ) then
        Result := 0 ;
    except
      Result := 0 ;
    end ;
  end ;

  { Read geometry.
  }
  function VPFFile.readGeometry(
    const _instancesCount : Integer ;
    const _dimensionality : Integer ;
    const _readDoubles    : Boolean
  ) : TObject ;
  var
    coordinates : TGIS_Point3DList ;
    ptg : TGIS_Point3D ;
    inx : Integer ;
  begin
    coordinates := TGIS_Point3DList.Create ;

    for inx := 0 to _instancesCount-1 do begin
      case _dimensionality of
         2: if _readDoubles then
              ptg := GisPoint3D( readDouble, readDouble, 0 )
            else
              ptg := GisPoint3D( readFloat, readFloat, 0 ) ;
         3: if _readDoubles then
              ptg := GisPoint3D( readDouble, readDouble, readDouble )
            else
              ptg := GisPoint3D( readFloat, readFloat, readFloat ) ;
      end ;

      coordinates.Add( ptg ) ;
    end ;

    Result := coordinates ;
  end ;

  { Read header.
  }
  procedure VPFFile.readHeader ;
  var
    fourbytes : TBytes ;
    ctrl      : Char ;
    column    : VPFColumn ;
  begin
    SetLength( fourbytes, 4 ) ;
    FinputStream.ReadBytesCnt( fourbytes, 4 ) ;

    FbyteOrder := readChar ;
    ctrl      := FbyteOrder ;

    if ( FbyteOrder = VPF_RECORD_SEPARATOR ) then
      FbyteOrder := LITTLE_ENDIAN_ORDER
    else
      ctrl := readChar ;

    if ( FbyteOrder = LITTLE_ENDIAN_ORDER ) then
      fourbytes := DataUtils.toBigEndian( fourbytes ) ;

    FheaderLength := DataUtils.decodeInt( fourbytes ) ;

    if ( ctrl <> VPF_RECORD_SEPARATOR ) then
      raise EGIS_Exception.Create( 'Header format does not fit VPF file definition.', '', 0 ) ;

    Fdescription    := readString( VPF_RECORD_SEPARATOR ) ;
    FnarrativeTable := readString( VPF_RECORD_SEPARATOR ) ;

    column := readColumn ;

    while ( column <> nil ) do begin
      Fcolumns.Add( column ) ;
      ctrl := readChar ;

      if ( ctrl <> VPF_FIELD_SEPARATOR ) then
        raise EGIS_Exception.Create( 'Header format does not fit VPF file definition.', '', 0 ) ;

      column := readColumn ;
    end ;

    if getRecordSize < 0 then
      FvariableIndex := VariableIndexInputStream.Create(
                          getVariableIndexFileName,
                          FbyteOrder
                       ) ;
  end ;

  { Read Integer.
  }
  function VPFFile.readInteger : Integer ;
  var
    tmp : Byte ;
  begin
    FinputStream.ReadBytesCnt( FBufInt, DATA_LONG_INTEGER_LEN ) ;

    if ( FbyteOrder = LITTLE_ENDIAN_ORDER ) then begin
      tmp := FBufInt[0] ;
      FBufInt[0] := FBufInt[3] ;
      FBufInt[3] := tmp ;
      tmp := FBufInt[1] ;
      FBufInt[1] := FBufInt[2] ;
      FBufInt[2] := tmp ;
    end ;

    try
      Result := DataUtils.decodeInt( FBufInt ) ;
      if IsNan( Result ) then
        Result := 0 ;
    except
      Result := 0 ;
    end ;
  end ;

  { Read number.
  }
  function VPFFile.readNumber(
    const _cnt : Integer
  ) : TBytes ;
  var
    databytes : TBytes ;
  begin
    SetLength( databytes, _cnt ) ;
    FinputStream.ReadBytesCnt( databytes, _cnt ) ;

    if ( FbyteOrder = LITTLE_ENDIAN_ORDER ) then
      databytes := DataUtils.toBigEndian( databytes ) ;

    Result := databytes ;
  end ;

  { Read short.
  }
  function VPFFile.readShort : Word ;
  begin
    Result := DataUtils.decodeShort(readNumber(DATA_SHORT_INTEGER_LEN));
  end ;

  { Read string.
  }
  function VPFFile.readString(
    const _terminators : String
  ) : String ;
  var
    text : TStringBuilder ;
    ctrl : Char ;
  begin
    text := TStringBuilder.Create ;
    try
      ctrl := readChar ;

      if Pos( ctrl, _terminators ) >= StringFirst then begin
        if ( ctrl = VPF_FIELD_SEPARATOR ) then
          unread(1) ;

        Result := '' ;
        exit ;
      end ;

      while ( Pos( ctrl, _terminators ) < StringFirst ) do begin
        text.Append( ctrl ) ;
        ctrl := readChar ;
      end ;

      if text.ToString = STRING_NULL_VALUE then begin
        Result := '' ;
        exit ;
      end
      else begin
        Result := text.ToString ;
        exit ;
      end ;
    finally
      FreeObject( text ) ;
    end ;
  end ;

  { Read tripleid.
  }
  function VPFFile.readTripletId : TripletId ;
  var
    tripletDef : Byte ;
    dataSize   : Integer ;
    tripletData: TBytes;
  begin
    {$IFDEF OXYGENE}
    FinputStream.ReadByte( tripletDef, 1 ) ;
    {$ELSE}
    FinputStream.Read( tripletDef, 1 ) ;
    {$ENDIF}
    dataSize := TripletId.calculateDataSize(tripletDef);
    SetLength( tripletData, dataSize + 1);
    tripletData[0] := tripletDef;

    if (dataSize > 0) then
      FinputStream.ReadBytesCnt(tripletData, dataSize, 1);

    Result := TripletId.Create(tripletData);
  end ;

  { Read variable size data.
  }
  function VPFFile.readVariableSizeData(
    const _name     : String ;
    const _dataType : Char
  ) : FieldValue ;
  var
    instances : Integer ;
  begin
    instances := readInteger ;

    Result := readFixedSizeData(_name, _dataType, instances) ;
  end ;

  { Set position.
  }
  procedure VPFFile.SetPosition(
    const _pos : Int64
  ) ;
  var
    varRow : VariableIndexRow ;
  begin
    if getRecordSize < 0 then begin
      varRow := VariableIndexRow( FvariableIndex.ReadRow( _pos ) ) ;
      FinputStream.Position := varRow.Offset ;
      FreeObject( varRow ) ;
    end
    else
      FinputStream.Position := FheaderLength + 4 + ((_pos - 1) * getRecordSize ) ;
  end ;

  function VPFFile.GetInfo : String ;
  var
    i : Integer ;
  begin
    Result := GetFileName( FpathName ) + ' [';
    for i := 0 to Fcolumns.Count-1 do begin
      Result := Result + VPFColumn( Fcolumns[i] ).GetInfo ;
      if i < Fcolumns.Count-1 then
        Result := Result + ', ' ;
    end ;
    Result := Result + ']' ;
  end ;

  { Unread.
  }
  procedure VPFFile.unread(
    const _bytes : Integer
  ) ;
  begin
    FinputStream.Position := FinputStream.Position - _bytes ;
  end ;

  {  Read feature.
  }
  function VPFFile.ReadFeature : SimpleFeature ;
  var
    column : VPFColumn ;
    inx    : Integer ;
    csize  : Integer ;
  begin
    Result := SimpleFeature.Create ;

    for inx := 0 to Fcolumns.Count-1 do begin
      column := VPFColumn( Fcolumns[inx] ) ;

      csize := column.ColumnSize ;
      if ( csize > 0 ) or ( csize = -2 ) then
        SimpleFeature(Result).AddValue(
          readFixedSizeData( column.Name,
                             column.TypeChar,
                             column.ElementsNumber
                            )
        )
      else if csize = -1 then
        SimpleFeature(Result).AddValue(
          readVariableSizeData( column.Name, column.TypeChar )
        )
      else
        ; // do nothing for null

    end ;
  end ;

  { Close.
  }
  procedure VPFFile.Close;
  begin

  end ;

  { Reset.
  }
  procedure VPFFile.Reset;
  begin
    SetPosition(1);
  end ;

//=============================================================================
// VPFInputStream
//=============================================================================

  {  Constructor.
  }
  constructor VPFInputStream.Create(
    const _file       : String ;
    const _byteOrder  : Char
  ) ;
  begin
    inherited Create ;

    FstreamFile := _file ;
    FbyteOrder  := _byteOrder ;

    Finput  := TGIS_BufferedFileStream.Create( FstreamFile, TGIS_StreamMode.Read ) ;
    Fheader := readHeader ;

    if assigned( Fheader ) and ( Fheader.GetRecordSize < 0 ) then
      FvariableIndex := VariableIndexInputStream.Create(
                          getVariableIndexFileName,
                          FbyteOrder
                        ) ;
  end ;

  { Destructor.
  }
  procedure VPFInputStream.doDestroy ;
  begin
    FreeObject( Finput  ) ;
    FreeObject( Fheader ) ;
    FreeObject( FvariableIndex ) ;

    inherited ;
  end ;

  { Read row.
  }
  function VPFInputStream.ReadRow(
    const _index : Integer
  ) : VPFRow ;
  begin
    SetPosition( _index ) ;

    Result := ReadRow ;
  end ;

  { Read row.
  }
  function VPFInputStream.ReadRow : VPFRow ;
  begin
    Result := nil ;
  end ;

  { Set position.
  }
  procedure VPFInputStream.SetPosition(
    const _pos : Int64
  ) ;
  var
    varRow : VariableIndexRow ;
  begin
    if Fheader.GetRecordSize < 0 then begin
      varRow := VariableIndexRow( FvariableIndex.ReadRow( _pos ) ) ;
      Finput.Position := varRow.Offset ;
      FreeObject( varRow ) ;
    end
    else
      Finput.Position := Fheader.GetLength + (_pos - 1) * Fheader.GetRecordSize ;
  end ;

  {  read header.
  }
  function VPFInputStream.readHeader : VPFHeader ;
  begin
    Result := nil ;
  end ;

  { Get variable index file name.
  }
  function VPFInputStream.getVariableIndexFileName : String ;
  begin
    if FstreamFile = 'fcs' then
      Result := 'fcz'
    else
      Result := Copy( FstreamFile, StringFirst, length( FstreamFile )-1 ) + 'x' ;
  end ;

  {  Read float.
  }
  function VPFInputStream.readFloat : Double ;
  begin
    Result := DataUtils.decodeFloat(readNumber(DATA_SHORT_FLOAT_LEN));
  end ;

  {  Read Integer.
  }
  function VPFInputStream.readInteger : Integer ;
  begin
    Result := DataUtils.decodeInt(readNumber(DATA_LONG_INTEGER_LEN));
  end ;

  {  Read number.
  }
  function VPFInputStream.readNumber(
    const _cnt : Integer
  ) : TBytes ;
  var
    databytes : TBytes ;
  begin
    SetLength( databytes, _cnt ) ;
    Finput.ReadBytesCnt( databytes, _cnt ) ;

    if ( FbyteOrder = LITTLE_ENDIAN_ORDER ) then
      databytes := DataUtils.toBigEndian( databytes ) ;

    Result := databytes ;
  end ;

//=============================================================================
// VariableIndexInputStream
//=============================================================================

  {  Constructor.
  }
  constructor VariableIndexInputStream.Create(
    const _file       : String ;
    const _byteOrder  : Char
  ) ;
  begin
    inherited Create( _file, _byteOrder ) ;
  end ;

  { Read row.
  }
  function VariableIndexInputStream.ReadRow : VPFRow ;
  var
    off  : Integer ;
    size : Integer ;
  begin
    off  := readInteger ;
    size := readInteger ;
    Result := VariableIndexRow.Create( off, size ) ;
  end ;

  { Read header.
  }
  function VariableIndexInputStream.readHeader : VPFHeader ;
  var
    ent    : Integer ;
    hdrlen : Integer ;
  begin
    ent    := readInteger ;
    hdrlen := readInteger ;

    Result := VariableIndexHeader.Create( ent, hdrlen ) ;
  end ;

//=============================================================================
// SpatialIndexInputStream
//=============================================================================

  {  Constructor.
  }
  constructor SpatialIndexInputStream.Create(
    const _file       : String ;
    const _byteOrder  : Char
  ) ;
  begin
    inherited Create( _file, _byteOrder ) ;
  end ;

  { read header.
  }
  function SpatialIndexInputStream.readHeader : VPFHeader ;
  var
    np    : Integer ;
    xmin  : Double ;
    ymin  : Double ;
    xmax  : Double ;
    ymax  : Double ;
    nn    : Integer ;
  begin
    np   := readInteger ;
    xmin := readFloat ;
    ymin := readFloat ;
    xmax := readFloat ;
    ymax := readFloat ;
    nn   := readInteger ;

    Result := SpatialIndexHeader.Create( np, xmin, ymin, xmax, ymax, nn ) ;
  end ;

  {  Read row.
  }
  function SpatialIndexInputStream.ReadRow : VPFRow ;
  begin
    Result := nil ;
  end ;

  { Set position.
  }
  procedure SpatialIndexInputStream.SetPosition(
    const _pos : Int64
  ) ;
  begin
    Finput.Position := 8 * _pos ;
  end ;

//=============================================================================
// DataUtils
//=============================================================================


  class function DataUtils.decodeData(
    const _bytes : TBytes ;
    const _type   : Char
  ) : Variant ;
  var
    i      : Integer ;
    isnull : Boolean ;
    val    : String ;
  begin
    case _type of
       DATA_TEXT,
       DATA_LEVEL1_TEXT,
       DATA_LEVEL2_TEXT,
       DATA_LEVEL3_TEXT:
        begin
          val := Trim( ConvertAnsiString( _bytes ) ) ;
          isnull := false;
          for i := low(STRING_NULL_VALUES) to high(STRING_NULL_VALUES) do
            isnull := isnull or ( CompareText( val, STRING_NULL_VALUES[i] )=0 );
          if isnull then
            Result := NullVar
          else
            Result := val ;
        end ;
     DATA_SHORT_FLOAT:
        Result := decodeFloat(_bytes);
     DATA_LONG_FLOAT:
        Result := decodeDouble(_bytes);
     DATA_SHORT_INTEGER:
        Result := decodeShort(_bytes);
     DATA_LONG_INTEGER:
        Result := decodeInt(_bytes);
     DATA_DATE_TIME:
        Result := decodeDate(_bytes)
    else
        Result := Unassigned ;
    end ;
  end ;

  class function DataUtils.decodeDate(
    const _bytes : TBytes
  ) : TDateTime ;
  var
    date : String ;
    time : TDateTime ;
  begin
    try
      // yyyyMMddHHmmss
      date := Trim( ConvertAnsiString( _bytes ) ) ;
      if date = '00000000000000.00000' then
        {$IFDEF OXYGENE}
          Result := new DateTime(1899, 12, 30,0,0,0)
        {$ELSE}
          Result := 0
        {$ENDIF}
      else begin
        if length( date ) >= 14 then
          time   := EncodeTime( StrToIntDef( Copy( date, StringFirst+8,  2 ),0 ),
                                StrToIntDef( Copy( date, StringFirst+10, 2 ),0 ),
                                StrToIntDef( Copy( date, StringFirst+12, 2 ),0 ),
                                0
                               )
        else
          {$IFDEF OXYGENE}
            time := new DateTime(1899, 12, 30,0,0,0);
          {$ELSE}
            time := 0 ;
          {$ENDIF}

        Result := EncodeDate( StrToIntDef( Copy( date, StringFirst,   4 ),0 ),
                              StrToIntDef( Copy( date, StringFirst+4, 2 ),0 ),
                              StrToIntDef( Copy( date, StringFirst+6, 2 ),0 )
                             ) ;
        {$IFDEF OXYGENE}
          if Result < new DateTime(1899, 12, 30,0,0,0) then
            Result := Result.AddHours(-time.Hour).AddMinutes(-time.Minute).AddSeconds(-time.Second)
          else
            Result := Result.AddHours(time.Hour).AddMinutes(time.Minute).AddSeconds(time.Second) ;
        {$ELSE}
          if Result < 0 then
            Result := Result - time
          else
            Result := Result + time ;
        {$ENDIF}
      end ;
    except
      {$IFDEF OXYGENE}
        Result := new DateTime(1899, 12, 30,0,0,0)
      {$ELSE}
        Result := 0
      {$ENDIF}
    end ;
  end ;

  class function DataUtils.decodeDouble(
    const _bytes : TBytes
  ) : Double ;
  var
    res   : Int64 ;
    shift : Integer ;
    i     : Integer ;
  begin
    res   := 0 ;
    shift := 56 ;
    i     := 0 ;

    while (i < length( _bytes ) ) and (shift >= 0) do begin
      res := res or Int64( (_bytes[i] and $ff) shl shift ) ;
      shift := shift - 8 ;
      inc(i) ;
    end ;
    {$IFDEF OXYGENE}
      Result := BitConverter.Int64BitsToDouble(res);
    {$ELSE}
      Result := Double(Pointer(@res)^) ;
    {$ENDIF}
  end ;

  class function DataUtils.decodeFloat(
    const _bytes : TBytes
  ) : Single ;
  var
    res   : Integer ;
    shift : Integer ;
    i     : Integer ;
    cnt   : Integer ;
  begin
    res   := 0 ;
    shift := 24 ;
    i     := 0 ;
    cnt   := length(_bytes) ;

    while (i < cnt) and (shift >= 0) do begin
      res := res or ((_bytes[i] and $ff) shl shift) ;
      shift := shift - 8 ;
      inc( i ) ;
    end ;

    {$IFDEF OXYGENE}
      Result := BitConverter.ToSingle( BitConverter.GetBytes( res ), 0 );
    {$ELSE}
      Result := Single(Pointer(@res)^) ;
    {$ENDIF}
  end ;

  class function DataUtils.decodeInt(
    const _bytes : TBytes
  ) : Integer ;
  var
    res   : Integer ;
    shift : Integer ;
    i     : Integer ;
    cnt   : Integer ;
  begin
    res   := 0 ;
    shift := 24 ;
    i     := 0 ;
    cnt   := length(_bytes) ;

    while ( i < cnt ) and ( shift >= 0 ) do begin
      res := res or ((_bytes[i] and $ff) shl shift) ;
      dec( shift, 8 ) ;
      inc( i ) ;
    end ;

    Result := res ;
  end ;

  class function DataUtils.decodeShort(
    const _bytes : TBytes
  ) : Word ;
  var
    res   : Word ;
    shift : Integer ;
    i     : Integer ;
  begin
    res   := 0 ;
    shift := 8 ;
    i     := 0 ;

    while (i < length(_bytes)) and (shift >= 0) do begin
      res := res or Word( (_bytes[i] and $ff) shl shift) ;
      dec( shift, 8 ) ;
      inc( i ) ;
    end ;

    Result := res ;
  end ;

  class function DataUtils.getDataTypeSize(
    const _type : Char
  ) : Integer ;
  begin
    case _type of
       DATA_TEXT,
       DATA_LEVEL1_TEXT,
       DATA_LEVEL2_TEXT,
       DATA_LEVEL3_TEXT:
          Result := 1;
       DATA_SHORT_FLOAT:
          Result := DATA_SHORT_FLOAT_LEN;
       DATA_LONG_FLOAT:
          Result := DATA_LONG_FLOAT_LEN;
       DATA_SHORT_INTEGER:
          Result := DATA_SHORT_INTEGER_LEN;
       DATA_LONG_INTEGER:
          Result := DATA_LONG_INTEGER_LEN;
       DATA_2_COORD_F:
          Result := DATA_2_COORD_F_LEN;
       DATA_2_COORD_R:
          Result := DATA_2_COORD_R_LEN;
       DATA_3_COORD_F:
          Result := DATA_3_COORD_F_LEN;
       DATA_3_COORD_R:
          Result := DATA_3_COORD_R_LEN;
       DATA_DATE_TIME:
          Result := DATA_DATE_TIME_LEN;
       DATA_NULL_FIELD:
          Result := DATA_NULL_FIELD_LEN;
       DATA_TRIPLET_ID:
          Result := DATA_TRIPLET_ID_LEN
       else Result := -1
    end ;
  end ;

  class function DataUtils.toBigEndian(
    const _bytes : TBytes
  ) : TBytes ;
  var
    i, cnt : Integer ;
  begin
    cnt := length( _bytes ) ;
    SetLength( Result, cnt ) ;

    for i := 0 to cnt-1 do
      Result[i] := _bytes[cnt - (i + 1)] ;
  end ;

//=============================================================================
// VPFColumn
//=============================================================================

  { Constructor.
  }
  constructor VPFColumn.Create(
    const _name : String ;
    const _type : Char ;
    const _elementsNumber : Integer ;
    const _keyType : Char ;
    const _colDesc : String ;
    const _valDescTableName : String ;
    const _thematicIdx : String ;
    const _narrTable : String
  ) ;
  begin
    inherited Create ;

    FattemptLookup     := False ;
    FElementsNumber    := _elementsNumber ;
    FnarrTable         := _narrTable ;
    FkeyType           := _keyType ;
    FcolDesc           := _colDesc ;
    FthematicIdx       := _thematicIdx ;
    FTypeChar          := _type ;
    FvalDescTableName  := _valDescTableName ;
    FName              := _name ;
  end ;

  { Get column size.
  }
  function VPFColumn.getColumnSize : Integer ;
  begin
    if ElementsNumber > -1 then
      Result := DataUtils.getDataTypeSize(TypeChar) * ElementsNumber
    else
      Result := -1 ;
  end ;

  function VPFColumn.GetInfo : String ;
  begin
    Result := Name + '(' + TypeChar + ')' ;
  end ;

//=============================================================================
// TripletId
//=============================================================================

  { Constructor.
  }
  constructor TripletId.Create(
    const _data : TBytes
  ) ;
  begin
    inherited Create ;

    FRawData := _data ;
  end ;

  { Calculate data size.
  }
  class function TripletId.calculateDataSize(
    const _definition : Byte
  ) : Integer ;
  var
    pieces : TBytes;
    i      : Integer ;
  begin
    Result := 0;

    SetLength( pieces, 3 ) ;
    pieces[0] := (_definition shr 2) and 3;
    pieces[1] := (_definition shr 4) and 3;
    pieces[2] := (_definition shr 6) and 3;

    for i := 0 to 2 do
      case pieces[i] of
         0 : ;
         1 : inc( Result   ) ;
         2 : inc( Result,2 ) ;
         3 : inc( Result,4 ) ;
      end ;
  end ;

  { Get id.
  }
  function TripletId.GetId : Integer ;
  var
    length : Integer ;
    piece  : Integer ;
    inx    : Integer ;
  begin
    Result := 0;
    length := (FRawData[0] shr 6) and 3 ;

    if (length > 0) then begin
      for inx := 0 to length-1 do begin
        piece := FRawData[inx + 1];
        if ( piece < 0 ) then
          piece := piece + RoundS(-2 * (-2e7));

        Result := Result + (piece shl (8 * inx));
      end ;
    end ;
  end ;

//=============================================================================
// FieldValue
//=============================================================================

  {$IFDEF OXYGENE}
  { Constructor.
  }
  constructor FieldValue.Create(
    const _name   : String ;
    const _value  : String ;
    const _type  : TGIS_FieldType
  ) ;
  begin
    inherited Create ;

    FName    := _name ;
    FValue   := _value ;
    FType    := _type ;

  end ;

  { Constructor.
  }
  constructor FieldValue.Create(
    const _name   : String ;
    const _value  : Integer;
    const _type  : TGIS_FieldType
  ) ;
  begin
    inherited Create ;

    FName    := _name ;
    FValue   := _value ;
    FType    := _type ;

  end ;

  { Constructor.
  }
  constructor FieldValue.Create(
    const _name   : String ;
    const _value  : Double ;
    const _type  : TGIS_FieldType
  ) ;
  begin
    inherited Create ;

    FName    := _name ;
    FValue   := _value ;
    FType    := _type ;

  end ;

  { Constructor.
  }
  constructor FieldValue.Create(
    const _name   : String ;
    const _value  : TObject;
    const _type  : TGIS_FieldType
  ) ;
  begin
    inherited Create ;

    FName    := _name ;
    FObj     := _value ;
    FType    := _type ;

  end ;
  {$ELSE}
  { Constructor.
  }
  constructor FieldValue.Create(
    const _name   : String ;
    const _value  : Variant ;
    const _type  : TGIS_FieldType
  ) ;
  begin
    inherited Create ;

    FName    := _name ;
    FValue   := _value ;
    FType    := _type ;
  end ;
  {$ENDIF}

  { Constructor.
  }
  constructor FieldValue.Create(
    const _name   : String ;
    const _value  : TObject
  ) ;
  begin
    inherited Create ;

    FName    := _name ;
    FObj     := _value ;
  end ;

  { Constructor.
  }
  constructor FieldValue.Create(
    const _name  : String ;
    const _type  : TGIS_FieldType
  ) ;
  begin
    inherited Create ;

    FName    := _name ;
    FValue   := Unassigned ;
    FType    := _type ;
  end ;

  { Destructor.
  }
  procedure FieldValue.doDestroy ;
  begin
    FreeObject( FObj ) ;

    inherited;
  end ;

  { Get double.
  }
  function FieldValue.getDouble : Double ;
  begin
    Result := VarToDouble( FValue ) ;
  end ;

  { Get geometry.
  }
  function FieldValue.getGeometry : TGIS_Point3DList ;
  begin
    Result := FObj as TGIS_Point3DList ;
  end ;

  { Get value as Integer.
  }
  function FieldValue.getInteger : Integer ;
  begin
    Result := VarToInt32( FValue ) ;
  end ;

  { Is field a geometry.
  }
  function FieldValue.getIsGeometry : Boolean;
  begin
    Result := assigned( FObj ) and ( FObj is TGIS_Point3DList ) ;
  end ;

  { Is field a tripleid.
  }
  function FieldValue.getIsTripletid : Boolean ;
  begin
    Result := assigned( FObj ) and ( FObj is TripletId ) ;
  end ;

  { Get field as string.
  }
  function FieldValue.getString : String ;
  begin
    Result := VarToString( FValue ) ;
  end ;

  { Get field as tripleid.
  }
  function FieldValue.getTripletId : TripletId ;
  begin
    Result := FObj as TripletId ;
  end ;

  { Set object.
  }
  procedure  FieldValue.SetObject(
    const _obj : TObject
   ) ;
  begin
    FObj := _obj ;
  end ;

  {  Set value.
  }
  procedure  FieldValue.SetValue(
    const _val : Variant
   ) ;
  begin
    FValue := _val ;
  end ;

  { Close field.
  }
  function FieldValue.Clone : FieldValue ;
  begin
    Result := FieldValue.Create( FName, FType ) ;
    Result.SetValue( FValue ) ;
    if assigned( FObj ) then
      if IsTripletId then
        Result.SetObject( TripletId.Create( TripletId(FObj).RawData ) ) ;
  end ;

  function FieldValue.GetInfo : String ;
  begin
    if IsGeometry then
      Result := ''
    else if IsTripletId then
      Result := ''
    else
      Result := Format( '%s=%s', [ FName, AsString ] ) ;
  end ;

//=============================================================================
// SimpleFeature
//=============================================================================

  { Constructor.
  }
  constructor SimpleFeature.Create ;
  begin
    inherited ;

    Values := TStringList.Create ;
    Values.Sorted := True ;
    Geometry  := nil ;
    GeometryArray := nil ;
    ShapeType := TGIS_ShapeType.Unknown ;
  end ;

  { Destructor.
  }
  procedure SimpleFeature.doDestroy ;
  var
    i   : Integer ;
    val : FieldValue ;
  begin
    for i := 0 to Values.Count-1 do begin
      val := FieldValue( Values.Objects[i] ) ;
      FreeObject( val ) ;
    end ;

    FreeObject( Values ) ;
    FreeObject( Geometry ) ;
    for i := low(GeometryArray) to high(GeometryArray) do
      FreeObject( GeometryArray[i] ) ;

    inherited;
  end ;

  { Clear values.
  }
  procedure SimpleFeature.ClearValues ;
  var
    i   : Integer ;
    val : FieldValue ;
  begin
    for i := 0 to Values.Count-1 do begin
      val := FieldValue( Values.Objects[i] ) ;
      FreeObject( val ) ;
    end ;
    Values.Clear ;
    FreeObject( Geometry ) ;

    for i := low(GeometryArray) to high(GeometryArray) do
      FreeObject( GeometryArray[i] ) ;
  end ;

  { Copy values.
  }
  procedure SimpleFeature.CopyValues(
    const _feature : SimpleFeature
  ) ;
  var
    i, j : Integer ;
  begin
    if not assigned( _feature ) then exit ;

    for i := 0 to _feature.Values.Count-1 do begin
      if not Values.Find( _feature.Values[ i ], j ) then
        Values.AddObject( _feature.Values[ i ],
                          FieldValue( _feature.Values.Objects[ i ] ).Clone
                         ) ;
    end ;
  end ;

  { Add value.
  }
  procedure SimpleFeature.AddValue(
    const _value : FieldValue
  ) ;
  begin
    Values.AddObject( _value.Name, _value ) ;
  end ;

  { Get value.
  }
  function SimpleFeature.GetValue(
    const _name : String
  ) : FieldValue ;
  var
    i : Integer ;
  begin
    if Values.Find( _name, i ) then
      Result := Values.Objects[ i ] as FieldValue
    else
      Result := nil ;
  end ;

  { Has feature a value.
  }
  function SimpleFeature.HasValue(
    const _name : String
  ) : Boolean ;
  var
    i : Integer ;
  begin
    Result := Values.Find( _name, i ) ;
  end ;

  { Set geometry.
  }
  procedure SimpleFeature.SetGeometry(
    const _geometryArray : TGIS_Point3DListArray
  ) ;
  begin
    GeometryArray := _geometryArray ;
  end ;

  { Set geometry.
  }
  procedure SimpleFeature.SetGeometry(
    const _geometry : TGIS_Point3DList
  ) ;
  var
    i : Integer ;
  begin
    Geometry := TGIS_Point3DList.Create ;
    for i := 0 to _geometry.Count-1 do
      Geometry.Add( _geometry[i] ) ;
  end ;

  {  Set shape type.
  }
  procedure SimpleFeature.SetShapeType(
    const _type : TGIS_ShapeType
  ) ;
  begin
    ShapeType := _type ;
  end ;

  function SimpleFeature.GetInfo : String ;
  var
    i : Integer ;

    procedure geomToString( const _list : TGIS_Point3DList; var _str : String );
    var
      j : Integer ;
    begin
      for j := 0 to _list.Count-1 do
        _str := _str + DotFloatToStr( _list[j].X ) + ' ' +
                       DotFloatToStr( _list[j].Y ) + #13#10 ;
    end ;

  begin
    Result := '';
    for i := 0 to Values.Count-1 do
      Result := Result + (Values.Objects[ i ] as FieldValue).GetInfo + '| ' ;

    Result := Result + #13#10 ;
    case ShapeType of
      TGIS_ShapeType.Point     : Result := Result + 'Point feature' ;
      TGIS_ShapeType.MultiPoint: Result := Result + 'MultiPoint feature' ;
      TGIS_ShapeType.Arc       : Result := Result + 'Line feature' ;
      TGIS_ShapeType.Polygon   : Result := Result + 'Polygon feature' ;
      TGIS_ShapeType.Complex   : Result := Result + 'Complex feature'
    else                         Result := Result + 'Unknown feature'
    end ;

    Result := Result + ', coords:' + #13#10 ;
    if assigned( Geometry ) then
      geomToString( Geometry, Result ) ;

    for i := 0 to length( GeometryArray )-1 do
      if assigned( GeometryArray[i] ) then
        geomToString( GeometryArray[i], Result ) ;
  end ;

  { Build shape.
  }
  procedure SimpleFeature.BuildShape(
    const _parent : TGIS_FileVPF
  ) ;
  var
    i   : Integer ;
    j   : Integer ;
    fv  : FieldValue ;
  begin
    _parent.NewShapeEvent( ShapeType, TGIS_DimensionType.XY ) ;

    if assigned( Geometry ) then begin
      _parent.AddPartEvent() ;
      for j := 0 to Geometry.Count-1 do
        _parent.AddCoordEvent( Geometry[j] ) ;
    end ;

    for i := 0 to length( GeometryArray )-1 do
      if assigned( GeometryArray[i] ) then  begin
        _parent.AddPartEvent() ;
        for j := 0 to GeometryArray[i].Count-1 do
          _parent.AddCoordEvent( GeometryArray[i][j] ) ;
      end ;
    _parent.EndShapeEvent() ;

    for i := 0 to Values.Count-1 do begin
      fv := Values.Objects[ i ] as FieldValue ;
      _parent.AddAttrEvent( fv.Name, fv.FieldType, fv.Value ) ;
    end ;
  end ;

//=============================================================================
// VPFDataBase
//=============================================================================

  { Constructor.
  }
  constructor VPFDataBase.Create(
    const _path : String
  ) ;
  var
    table     : VPFFile;
    items     : TGIS_ObjectList ;
    i         : Integer ;
    lib       : VPFLibrary ;
    feature   : SimpleFeature ;
    lpath     : String ;
  begin
    inherited Create ;

    libraries := TGIS_ObjectList.Create ;

    lpath := GetPathDirSep( _path ) + LIBRARY_ATTTIBUTE_TABLE ;
    if SafeFileExists( lpath ) then begin
      table := VPFFile.Create( lpath ) ;
      try
        items := table.ReadAllRows ;
        try
          for i := 0 to items.Count-1 do begin
            feature := SimpleFeature( items[ i ] ) ;

            lib := VPFLibrary.Create( feature, _path ) ;
            libraries.Add( lib ) ;
          end ;
        finally
          FreeObject( items ) ;
        end ;
      finally
        FreeObject( table ) ;
      end ;
    end
    else begin
      // try to find a library
      lib := VPFLibrary.Create( _path ) ;
      libraries.Add( lib ) ;
    end ;
  end ;

  { Destructor.
  }
  procedure VPFDataBase.doDestroy ;
  begin
    FreeObject( libraries ) ;

    inherited ;
  end ;

  { Get library count.
  }
  function VPFDataBase.getLibCount : Integer ;
  begin
    Result := libraries.Count ;
  end ;

  { Get library.
  }
  function VPFDataBase.getLibrary(
    const _idx : Integer
  ) : VPFLibrary ;
  begin
    if ( _idx >=0 ) and ( _idx < libraries.Count ) then
      Result := VPFLibrary( libraries[_idx] )
    else
      Result := nil ;
  end ;

  function VPFDataBase.GetInfo : String ;
  var
    i : Integer ;
  begin
    Result := 'VPF database with ' + IntToStr( LibraryCount ) +
              ' libraries:'+ #10#13 ;

    for i := 0 to LibraryCount-1 do
      if VLibrary[i].IsValid then
        Result := Result + VLibrary[i].GetInfo + #13#10 ;
  end ;

//=============================================================================
// VPFLibrary
//=============================================================================

  { Constructor.
  }
  constructor VPFLibrary.Create(
    const _libraryFeature : SimpleFeature ;
    const _path           : String
  ) ;
  begin
    inherited Create ;

    xmin := _libraryFeature.GetValue(FIELD_XMIN).AsDouble ;
    ymin := _libraryFeature.GetValue(FIELD_YMIN).AsDouble ;
    xmax := _libraryFeature.GetValue(FIELD_XMAX).AsDouble ;
    ymax := _libraryFeature.GetValue(FIELD_YMAX).AsDouble ;

    libraryName := _libraryFeature.GetValue(FIELD_LIB_NAME).AsString ;
    directory   := _path + libraryName ;
    FisValid     := DirectoryExists( directory ) ;

    coverages   := TGIS_ObjectList.Create ;
    FtileMap     := TStringList.Create ;

    setCoverages ;
  end ;

  { Constructor.
  }
  constructor VPFLibrary.Create(
    const _path : String
  ) ;
  var
    vpfTableName : String ;
    lhtFile      : VPFFile ;
    sf           : SimpleFeature ;
  begin
    inherited Create ;

    vpfTableName := _path + LIBRARY_HEADER_TABLE ;
    if SafeFileExists( vpfTableName ) then begin
      lhtFile := VPFFile.Create(vpfTableName) ;
      try
        lhtFile.Reset ;
        sf := lhtFile.ReadFeature ;
        try
          if sf.HasValue( FIELD_XMIN ) then
            xmin := sf.GetValue(FIELD_XMIN).AsDouble
          else
            xmin := -180 ;
          if sf.HasValue( FIELD_YMIN ) then
            ymin := sf.GetValue(FIELD_YMIN).AsDouble
          else
            ymin := -90 ;
          if sf.HasValue( FIELD_XMAX ) then
            xmax := sf.GetValue(FIELD_XMAX).AsDouble
          else
            xmax := 180 ;
          if sf.HasValue( FIELD_YMAX ) then
            ymax := sf.GetValue(FIELD_YMAX).AsDouble
          else
            ymax := 90 ;

          libraryName := sf.GetValue(FIELD_LIB_NAME).AsString ;
        finally
          FreeObject( sf ) ;
        end ;
      finally
        FreeObject( lhtFile ) ;
      end ;
    end
    else begin
      xmin := -180;
      ymin := -90 ;
      xmax := 180 ;
      ymax := 90  ;
      libraryName := ExtractFilePath(directory) ;
    end ;

    directory := _path;
    FisValid     := DirectoryExists( directory ) ;
    coverages := TGIS_ObjectList.Create ;
    FtileMap := TStringList.Create ;

    setCoverages;
  end ;

  { Destructor.
  }
  procedure VPFLibrary.doDestroy ;
  begin
    FreeObject( coverages ) ;
    FreeObject( FtileMap   ) ;

    inherited ;
  end ;

  { Get coverage.
  }
  function VPFLibrary.getCoverage(
    const _idx : Integer
  ) : VPFCoverage ;
  begin
    if ( _idx >=0 ) and ( _idx < coverages.Count ) then
      Result := VPFCoverage( coverages[_idx] )
    else
      Result := nil ;
  end ;

  { Get coverage count.
  }
  function VPFLibrary.getCoverageCount: Integer;
  begin
    Result := coverages.Count ;
  end ;

  { Get tile map.
  }
  function VPFLibrary.getTileMap(
    const _name : String
  ) : String ;
  begin
    Result := FtileMap.Values[ _name ] ;
  end ;

  {
  }
  function VPFLibrary.getSchema(
    const _typeName : String
  ) : VPFFeatureType ;
  var
    i, j : Integer ;
    temp : VPFFeatureType ;
    stop : Boolean ;
  begin
    Result  := nil ;
    stop    := False ;

    for i := 0 to coverages.Count-1 do begin
      for j := 0 to VPFCoverage(coverages[i]).FeatureTypeCount-1 do begin
        temp := VPFFeatureType(VPFCoverage(coverages[i]).FeatureType[j]) ;
        if temp.TypeName = _typeName then begin
          Result  := temp ;
          stop    := True ;
          break ;
        end ;
      end ;
      if stop then break ;
    end ;
  end ;

  { Set coverages.
  }
  procedure VPFLibrary.setCoverages ;
  var
    icoverage     : VPFCoverage ;
    feature       : SimpleFeature ;
    vpfTableName  : String ;
    vpfFileh      : VPFFile ;
    items         : TGIS_ObjectList ;
    i             : Integer ;
  begin
    if not IsValid then exit ;

    if ExtractFileName( directory ) <> 'rference' then begin
      vpfTableName := GetPathDirSep(directory) + COVERAGE_ATTRIBUTE_TABLE ;
      vpfFileh     := VPFFile.Create(vpfTableName) ;
      try
        items := vpfFileh.ReadAllRows ;
        try
          for i := 0 to items.Count-1 do begin
            feature  := SimpleFeature( items[ i ] ) ;
            icoverage := VPFCoverage.Create( self, feature, GetPathDirSep( directory ) );

            coverages.Add( icoverage ) ;
            if CompareText( icoverage.Name, 'tileref') = 0 then
              createTilingSchema( icoverage ) ;
          end ;
        finally
          FreeObject( items ) ;
        end ;
      finally
        FreeObject( vpfFileh ) ;
      end ;
    end ;
  end ;

  { Create tiling schema.
  }
  procedure VPFLibrary.createTilingSchema(
    const _coverage : VPFCoverage
  ) ;
  var
    tileType  : VPFFeatureType ;
    tileFile  : VPFFile ;
    items     : TGIS_ObjectList ;
    i         : Integer ;
    row       : SimpleFeature ;
    rowId     : String ;
    value     : String ;
  begin
    tileType := VPFFeatureType( _coverage.FeatureType[0] ) ;
    tileFile := VPFFile(tileType.FeatureClass.FileList[0]);

    items := tileFile.ReadAllRows ;
    try
      for i := 0 to items.Count-1 do begin
        row := SimpleFeature( items[i] ) ;
        rowId := row.GetValue('id').AsString ;
        value := row.GetValue(FIELD_TILE_NAME).AsString;
        FtileMap.Values[rowId] := value ;
      end ;
    finally
      FreeObject( items ) ;
    end ;

  end ;

  { Get features reader.
  }
  function VPFLibrary.GetFeatureReader(
    const _typeName : String
  ) : VPFFeatureReader ;
  var
    featureType : VPFFeatureType ;
  begin
    featureType := getSchema( _typeName );
    VPFFile(featureType.FeatureClass.FileList[0]).Reset ;

    Result := VPFFeatureReader.Create( featureType ) ;
  end ;

  {  Read all features.
  }
  procedure VPFLibrary.ReadAll(
    const _parent : TGIS_FileVPF
  ) ;
  var
    i    : Integer ;
    abrt : Boolean ;
  begin
    abrt := False ;

    for i := 0 to CoverageCount-1 do begin
      if IsStringEmpty( _parent.Category ) then
        Coverage[i].ReadAll( _parent, abrt )
      else if Coverage[i].Name = _parent.Category then
        Coverage[i].ReadAll( _parent, abrt ) ;

      if abrt then break ;
    end ;
  end ;

  function VPFLibrary.GetInfo: String;
  var
    i : Integer ;
  begin
    Result := Format(' Library %s (%f,%f,%f,%f) with %d coverages:'+ #13#10,
                     [libraryName, xmin, ymin,xmax,ymax, coverages.Count]
                     ) ;
    for i := 0 to coverages.Count-1 do
      Result := Result + VPFCoverage(coverages[i]).GetInfo + #13#10 ;
  end ;

//=============================================================================
// VPFCoverage
//=============================================================================

  {  Constructor.
  }
  constructor VPFCoverage.Create(
    const _library  : VPFLibrary ;
    const _feature  : SimpleFeature ;
    const _path     : String
  ) ;
  begin
    inherited Create ;

    featureClasses := TGIS_ObjectList.Create ;
    featureTypes   := TGIS_ObjectList.Create ;

    topologyLevel  := _feature.GetValue(FIELD_COVERAGE_LEVEL).AsInteger ;
    libraryHandle  := _library;
    fdescription   := _feature.GetValue(FIELD_COVERAGE_DESC).AsString ;
    pathName       := GetPathDirSep( _path ) +
                      _feature.GetValue(FIELD_COVERAGE_NAME).AsString ;

    discoverFeatureClasses ;
    discoverFeatureTypes ;
  end ;

  { Destructor.
  }
  procedure VPFCoverage.doDestroy ;
  begin
    FreeObject( featureClasses ) ;
    FreeObject( featureTypes   ) ;

    inherited ;
  end ;

  { Discover feature classes.
  }
  procedure VPFCoverage.discoverFeatureClasses;
  var
    ifeatureClass   : VPFFeatureClass ;
    fcsFileName     : String ;
    fcsFile         : VPFFile ;
    items           : TGIS_ObjectList ;
    i               : Integer ;
    row             : SimpleFeature ;
    featureClassName: String ;
  begin
    fcsFileName := GetPathDirSep( pathName ) + TABLE_FCS ;

    if SafeFileExists( fcsFileName ) then begin
    fcsFile := VPFFile.Create( fcsFileName ) ;
    try
      items := fcsFile.ReadAllRows ;
      try
        for i := 0 to items.Count-1 do begin
          row := SimpleFeature( items[i] ) ;
          featureClassName := Trim( row.GetValue('feature_class').AsString ) ;
          ifeatureClass    := VPFFeatureClass.Create(
                                self,
                                featureClassName,
                                pathName
                               ) ;
          featureClasses.Add( ifeatureClass ) ;
        end ;
      finally
        FreeObject( items ) ;
      end ;
    finally
      FreeObject( fcsFile ) ;
      end ;
    end ;
  end ;

  {  Discover feature types.
  }
  procedure VPFCoverage.discoverFeatureTypes ;
  var
    charvdt         : VPFFile ;
    charvdtFileName : String ;
    items           : TGIS_ObjectList ;
    i,j             : Integer ;
    row             : SimpleFeature ;
    attr            : String ;
    tableFileName   : String ;
    featureClassName: String ;
    ifeatureClass   : VPFFeatureClass ;
    ifeatureType    : VPFFeatureType ;
    skip            : Boolean ;
    coverageClass   : VPFFeatureClass ;
  begin
    charvdtFileName := GetPathDirSep( pathName ) + CHARACTER_VALUE_DESCRIPTION_TABLE;

    if FileExists( charvdtFileName ) then begin

      charvdt := VPFFile.Create(charvdtFileName);
      try
        items := charvdt.ReadAllRows ;
        try
          for i := 0 to items.Count-1 do begin
            row  := SimpleFeature( items[i] ) ;
            attr := LowerCase( Trim( row.GetValue('attribute').AsString ) ) ;

            skip := True ;
            for j := low(ALLOWED_FCODE_ATTRIBUTES) to high(ALLOWED_FCODE_ATTRIBUTES) do
              if attr = ALLOWED_FCODE_ATTRIBUTES[j] then
                skip := False ;

            if skip then continue ;

            tableFileName    := Trim( row.GetValue('table').AsString ) ;
            featureClassName := Copy( tableFileName, StringFirst, Pos( '.', tableFileName )-StringFirst ) ;

            for j := 0 to featureClasses.Count-1 do begin
              ifeatureClass := VPFFeatureClass( featureClasses[j] ) ;

              if featureClassName = ifeatureClass.TypeName then begin
                ifeatureType := VPFFeatureType.Create( ifeatureClass, row ) ;
                featureTypes.Add( ifeatureType ) ;
                break ;
              end ;

            end ;
          end ;
        finally
          FreeObject( items ) ;
        end ;
      finally
        FreeObject( charvdt ) ;
      end ;
    end
    else begin
      for j := 0 to featureClasses.Count-1 do begin
        coverageClass := VPFFeatureClass( featureClasses[j] ) ;
        ifeatureType  := VPFFeatureType.Create( coverageClass ) ;
        featureTypes.Add( ifeatureType ) ;
      end ;
    end ;
  end ;

  { Get feature class.
  }
  function VPFCoverage.getFeatureClass(
    const _idx : Integer
  ) : VPFFeatureClass ;
  begin
    if ( _idx >=0 ) and ( _idx < featureClasses.Count ) then
      Result := VPFFeatureClass( featureClasses[_idx] )
    else
      Result := nil ;
  end ;

  {  Get feature class count.
  }
  function VPFCoverage.getFeatureClassCount: Integer;
  begin
    Result := featureClasses.Count ;
  end ;

  { Get feature type.
  }
  function VPFCoverage.getFeatureType(
    const _idx : Integer
  ) : VPFFeatureType ;
  begin
    if ( _idx >=0 ) and ( _idx < featureTypes.Count ) then
      Result := VPFFeatureType( featureTypes[_idx] )
    else
      Result := nil ;
  end ;

  { Get feature type count.
  }
  function VPFCoverage.getFeatureTypeCount: Integer;
  begin
    Result := featureTypes.Count ;
  end ;

  { Get name.
  }
  function VPFCoverage.getName : String ;
  begin
    Result := ExtractFileName( pathName ) ;
  end ;

  { Read all features.
  }
  procedure VPFCoverage.ReadAll(
    const _parent : TGIS_FileVPF ;
      var _abort  : Boolean
  ) ;
  var
    i  : Integer ;
  begin
    Log('  ' + Name ) ;
    for i := 0 to FeatureTypeCount-1 do begin
      if IsStringEmpty( _parent.Feature ) then
        FeatureType[i].ReadAll( _parent, _abort )
      else if FeatureType[i].TableName = _parent.Feature then
        FeatureType[i].ReadAll( _parent, _abort ) ;

      if _abort then break ;
    end ;
  end ;

  function VPFCoverage.GetInfo : String;
  var
    i   : Integer ;
  begin
    Result := Format( '  Coverage %s (%s) in topology level %d with %d features:'+ #13#10,
                      [ Name, Description, topologyLevel, featureTypes.Count ]
                     ) ;
    for i := 0 to featureTypes.Count-1 do
      Result := Result + VPFFeatureType( featureTypes[i] ).GetInfo + #13#10 ;
  end ;

//=============================================================================
// VPFFeatureClass
//=============================================================================

  { Constructor.
  }
  constructor VPFFeatureClass.Create(
    const _coverage : VPFCoverage ;
    const _name     : String ;
    const _path     : String
  ) ;
  var
    fcsFileName : String ;
    fcsFile     : VPFFile ;
    items       : TGIS_ObjectList ;
    i           : Integer ;
    feature     : SimpleFeature ;
    featureClassName : String ;
  begin
    inherited Create ;

    Fcoverage      := _coverage ;
    FdirectoryName := _path ;
    FtypeName      := _name ;
    FfileList      := TGIS_ObjectList.Create( False ) ;
    FjoinList      := TGIS_ObjectList.Create ;
    FfileStore     := VPFFileStore.Create ;

    fcsFileName := GetPathDirSep( DirectoryName ) + TABLE_FCS ;
    if FileExists( fcsFileName ) then begin
      fcsFile := VPFFile.Create( fcsFileName ) ;
      try
        items := fcsFile.ReadAllRows ;
        try
          for i := 0 to items.Count-1 do begin
            feature := SimpleFeature( items[i] ) ;
            featureClassName := Trim( feature.GetValue('feature_class').AsString ) ;
            if TypeName = featureClassName then
              addFCS( feature ) ;
          end ;
        finally
          FreeObject( items ) ;
        end ;
      finally
        FreeObject( fcsFile ) ;
      end ;
    end ;
  end ;

  { Destructor.
  }
  procedure VPFFeatureClass.doDestroy ;
  begin
    FreeObject( FfileList ) ;
    FreeObject( FjoinList ) ;
    FreeObject( FgeometryFactory ) ;
    FreeObject( FfileStore ) ;

    inherited;
  end ;

  { Set geometry factory.
  }
  procedure VPFFeatureClass.setGeometryFactory(
    const _table : String
  ) ;
  begin
    if assigned( FgeometryFactory ) then exit ;

    if _table = EDGE_PRIMITIVE then
      FgeometryFactory := LineGeometryFactory.Create
    else if _table = FACE_PRIMITIVE then
      FgeometryFactory := AreaGeometryFactory.Create
    else if _table = CONNECTED_NODE_PRIMITIVE then
      FgeometryFactory := ConnectedNodeGeometryFactory.Create
    else if _table = ENTITY_NODE_PRIMITIVE then
      FgeometryFactory := EntityNodeGeometryFactory.Create
    else if _table = TEXT_PRIMITIVE then
      FgeometryFactory := TextGeometryFactory.Create
  end ;

  { Add FCS.
  }
  procedure VPFFeatureClass.addFCS(
    const _row : SimpleFeature
  ) ;
  var
    table1      : String ;
    table1Key   : String ;
    table2      : String ;
    table2Key   : String ;
    vpfFile1    : VPFFile ;
    vpfFile2    : VPFFile ;
    vpfFilen1   : String ;
    vpfFilen2   : String ;
    joinColumn1 : VPFColumn ;
    joinColumn2 : VPFColumn ;
    i           : Integer ;
    found       : Boolean ;
  begin
    table1    := _row.GetValue('table1').AsString;
    table1Key := _row.GetValue('table1_key').AsString;
    table2    := _row.GetValue('table2').AsString;
    table2Key := _row.GetValue('table2_key').AsString;

    vpfFilen1 := GetPathDirSep( DirectoryName) + table1 ;
    if SafeFileExists( vpfFilen1 ) then begin
      vpfFile1 := FileStore.GetFile( vpfFilen1 ) ;
      addFileToTable( vpfFile1 );
      joinColumn1 := vpfFile1.GetColumn( table1Key ) ;

      vpfFilen2 := GetPathDirSep( DirectoryName ) + table2 ;
      if SafeFileExists( vpfFilen2 ) then begin
        vpfFile2 := FileStore.GetFile( vpfFilen2 ) ;
        addFileToTable( vpfFile2 );
        joinColumn2 := vpfFile2.GetColumn( table2Key );

        found := False ;
        for i := 0 to JoinList.Count-1 do
          if ( ColumnPair(JoinList[i]).column1.Name = joinColumn2.Name ) and
             ( ColumnPair(JoinList[i]).column2.Name = joinColumn1.Name ) then begin
               found := True ;
               break ;
             end ;
        if not found then
          JoinList.Add( ColumnPair.Create(joinColumn1, joinColumn2) ) ;
      end ;

      setGeometryFactory( LowerCase( Trim( table2 ) ) ) ;
    end ;

  end ;

  { Add file handle to list.
  }
  procedure VPFFeatureClass.addFileToTable(
    const _vpfFile : VPFFile
  ) ;
  begin
    if FileList.IndexOf( _vpfFile ) < 0 then
      FileList.Add( _vpfFile ) ;
  end ;


  function VPFFeatureClass.GetInfo : String ;
  begin
    Result := TypeName ;
  end ;

//=============================================================================
// VPFFeatureType
//=============================================================================

  { Constructor.
  }
  constructor VPFFeatureType.Create(
    const _featureClass : VPFFeatureClass ;
    const _feature      : SimpleFeature
  );
  var
    tempTypeName : String ;
    ext          : String ;
  begin
    inherited Create ;

    FFeatureClass  := _featureClass ;
    FFaccCode      := Trim( _feature.GetValue('value').AsString );
    FTableName     := Trim( _feature.GetValue('table').AsString );
    FDescription   := Trim( _feature.GetValue('description').AsString );
    tempTypeName   := FDescription ;

    ext := UpperCase( GetFileExt(TableName) ) ;
    if not IsStringEmpty( ext ) then begin
      if ext[StringFirst+1] = 'A' then begin
        FShapeType := TGIS_ShapeType.Polygon ;
        tempTypeName := tempTypeName + ' Area'
      end
      else if ext[StringFirst+1] = 'L' then begin
        FShapeType := TGIS_ShapeType.Arc ;
        tempTypeName := tempTypeName + ' Line'
      end
      else if ext[StringFirst+1] = 'P' then begin
        FShapeType := TGIS_ShapeType.Point ;
        tempTypeName := tempTypeName + ' Point'
      end
      else begin
        FShapeType := TGIS_ShapeType.Point ;
        tempTypeName := tempTypeName + ' Text' ;
      end ;
    end ;

    tempTypeName := UpperCase( tempTypeName ) ;
    tempTypeName := StringReplace( tempTypeName, ' ', '_',
                    [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll]
                    ) ;
    tempTypeName := StringReplace( tempTypeName, '/', '_',
                    [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll]
                    ) ;
    tempTypeName := StringReplace( tempTypeName, '(', '_',
                    [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll]
                    ) ;
    tempTypeName := StringReplace( tempTypeName, ')', '_',
                    [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll]
                    ) ;
    FTypeName    := tempTypeName;
  end ;

  { Constructor.
  }
  constructor VPFFeatureType.Create(
    const _featureClass : VPFFeatureClass
  ) ;
  begin
    inherited Create ;

    FFeatureClass := _featureClass ;
    FFaccCode     := '';
    FTypeName     := UpperCase( _featureClass.TypeName ) ;
  end ;

  { Read all features.
  }
  procedure VPFFeatureType.ReadAll(
    const _parent : TGIS_FileVPF ;
      var _abort  : Boolean
  ) ;
  var
    rdr   : VPFFeatureReader ;
    fea   : SimpleFeature ;
    num   : Integer ;
  begin
    _parent.BusyPos := _parent.BusyPos + 1 ;

    _parent.BusyEvent( _parent,
                    {$IFDEF OXYGENE}
                     TGIS_BusyEventArgs.Create( _parent.BusyPos, _parent.BusyCount, _abort )
                    {$ELSE}
                     _parent.BusyPos, _parent.BusyCount, _abort
                    {$ENDIF}
                   ) ;
    num := 0 ;
    rdr := VPFFeatureReader.Create( Self ) ;
    try
      while rdr.HasNext do begin
        fea := rdr.GetCurrent ;
        inc( num ) ;
        if num = 1 then
          _parent.SetLayerEvent( FeatureClass.Coverage.Name+'_'+ TypeName,
                              FeatureClass.Coverage.Description + '\' + Description,
                              FeatureClass.Coverage.Name+'\'+FTableName,
                              FShapeType
                             ) ;
        fea.BuildShape( _parent ) ;
      end ;
    finally
      FreeObject( rdr ) ;
    end ;
  end ;

  function VPFFeatureType.GetInfo : String ;
  begin
    Result := TableName + ' (' + TypeName + ')' ;
  end ;

//=============================================================================
// ColumnPair
//=============================================================================

  { Constructor.
  }
  constructor ColumnPair.Create(
    const _c1, _c2: VPFColumn
  ) ;
  begin
    inherited Create ;

    column1 := _c1;
    column2 := _c2;
  end ;

//=============================================================================
// VPFFeatureReader
//=============================================================================

  { Constructor.
  }
  constructor VPFFeatureReader.Create(
    const _type : VPFFeatureType
  ) ;
  begin
    inherited Create ;

    featureType    := _type ;
    fhasNext       := True ;
    nextCalled     := True ;
    currentFeature := SimpleFeature.Create ;
    currentRow     := nil ;
  end ;

  { Destructor.
  }
  procedure VPFFeatureReader.doDestroy ;
  begin
    currentFeature.ClearValues ;

    FreeObject( currentFeature ) ;

    inherited ;
  end ;

  { Get current feature.
  }
  function VPFFeatureReader.GetCurrent : SimpleFeature ;
  begin
    nextCalled := True ;
    Result := currentFeature ;
  end ;

  { Generate file row map.
  }
  function VPFFeatureReader.generateFileRowMap(
    const _file : VPFFile ;
    const _row  : SimpleFeature
  ) : TGIS_ObjectList ;
  var
    i           : Integer ;
    columnp     : ColumnPair ;
    primaryFile : VPFFile ;
    joinFile    : VPFFile ;
    joinRow     : SimpleFeature ;
    joinId      : Integer ;
    column      : FieldValue ;
  begin
    Result := TGIS_ObjectList.Create ;

    for i := 0 to featureType.FeatureClass.JoinList.Count-1 do begin
      columnp := ColumnPair( featureType.FeatureClass.JoinList[i] ) ;

      primaryFile := getVPFFile(columnp.column1) ;
      joinFile    := getVPFFile(columnp.column2) ;

      if assigned( primaryFile ) and assigned( joinFile ) and
         assigned( columnp.column1 ) and assigned( columnp.column2 ) then
      begin
        column  := _row.GetValue( columnp.column1.Name ) ;
        if assigned( column ) then begin
          joinId  := column.AsInteger ;
          if ( joinFile <> _file ) then begin
            joinRow := joinFile.GetRowFromId( columnp.column2.Name, joinId ) ;
            if assigned( joinRow ) then
              Result.Add( joinRow ) ;
          end
          else begin
            column := _row.GetValue( columnp.column2.Name ) ;
            if assigned( column ) and ( column.AsInteger = joinId ) then
              ;
          end ;
        end ;
      end ;
    end ;
  end ;

  { read object.
  }
  procedure VPFFeatureReader.retrieveObject(
    const _file : VPFFile ;
    const _row  : SimpleFeature
  ) ;
  var
    rows  : TGIS_ObjectList ;
    i     : Integer ;
  begin
    // collect additional rows from joins
    rows := generateFileRowMap(_file, _row);
    try
      currentFeature.ClearValues ;
      currentFeature.CopyValues( _row ) ;

      for i := 0 to rows.Count-1 do
        currentFeature.CopyValues( SimpleFeature(rows[i]) ) ;

      featureType.FeatureClass.GeometryFactory.CreateGeometry(
        featureType,
        currentFeature
       ) ;
    finally
      FreeObject( rows ) ;
    end ;

  end ;

  { Get file handle.
  }
  function VPFFeatureReader.getVPFFile(
    const _column : VPFColumn
  ) : VPFFile ;
  var
    i    : Integer ;
    temp : VPFFile ;
  begin
    Result := nil ;
    for i := 0 to featureType.FeatureClass.FileList.Count-1 do begin
      temp := VPFFile( featureType.FeatureClass.FileList[i] ) ;
      if ( temp <> nil ) and ( temp.GetColumn( _column.Name ) <> nil ) then begin
        Result := temp ;
        break ;
      end ;
    end ;
  end ;

  { Has reader next element.
  }
  function VPFFeatureReader.HasNext : Boolean ;
  begin
    if nextCalled then begin
      while ReadNext do ;
      nextCalled := False;
    end ;
    Result := fhasNext ;
  end ;

  { Reset reader.
  }
  procedure VPFFeatureReader.Reset ;
  var
    ffile : VPFFile ;
  begin
    ffile := VPFFile( featureType.FeatureClass.FileList[0] ) ;
    ffile.Reset ;
  end ;

  { Read next feature
  }
  function VPFFeatureReader.ReadNext : Boolean ;
  var
    ffile : VPFFile ;
    i     : Integer ;
    temp  : FieldValue ;
  begin
    Result := True ;

    ffile := VPFFile( featureType.FeatureClass.FileList[0] ) ;
    fhasNext := False ;

    currentRow := nil ;

    if ffile.HasNext then
      currentRow := ffile.ReadFeature ;

    try
      if currentRow = nil then begin
        fhasNext := false ;
        Result   := false ;
      end
      else if not IsStringEmpty( featureType.FaccCode ) then begin
        i    := 0 ;
        temp := nil ;
        while ( temp = nil ) and ( i < length( ALLOWED_FCODE_ATTRIBUTES ) ) do begin
          temp := currentRow.GetValue( ALLOWED_FCODE_ATTRIBUTES[i] ) ;
        end ;

        if featureType.FaccCode = Trim( temp.AsString ) then begin
          retrieveObject(ffile, currentRow) ;
          fhasNext := true ;
          Result   := false ;
        end ;
      end ;
    finally
      FreeObject( currentRow ) ;
    end ;
  end ;

//=============================================================================
// VariableIndexRow
//=============================================================================

  constructor VariableIndexRow.Create(
    const _offset : Integer ;
    const _size   : Integer
  ) ;
  begin
    inherited Create ;

    FOffset := _offset ;
    FSize   := _size ;
  end ;

//=============================================================================
// MbrIndexRow
//=============================================================================

  { Constructor.
  }
  constructor MbrIndexRow.Create(
    const _id        : Integer ;
    const _xmin      : Double ;
    const _ymin      : Double ;
    const _xmax      : Double ;
    const _ymax      : Double
   ) ;
  begin
    Fid     := _id ;
    FExtent := GisExtent( _xmin, _ymin, _xmax, _ymax ) ;
  end ;

//=============================================================================
// VPFGeometryFactory
//=============================================================================

  procedure VPFGeometryFactory.CreateGeometry(
    const _featureType  : VPFFeatureType ;
    const _values       : SimpleFeature
  ) ;
  begin
    _values.SetShapeType( TGIS_ShapeType.Unknown ) ;
  end ;

//=============================================================================
// EntityNodeGeometryFactory
//=============================================================================

  procedure EntityNodeGeometryFactory.CreateGeometry(
    const _featureType  : VPFFeatureType ;
    const _values       : SimpleFeature
  ) ;
  var
    nodeId        : Integer ;
    baseDirectory : String ;
    tileDirectory : String ;
    tileId        : String ;
    nodeTableName : String ;
    nodeFile      : VPFFile ;
    row           : SimpleFeature ;
    geom          : TGIS_Point3DList ;
  begin
    nodeId := _values.GetValue('end_id').AsInteger ;

    baseDirectory := _featureType.FeatureClass.DirectoryName ;
    tileDirectory := baseDirectory ;

    if not FileExists( GetPathDirSep( tileDirectory ) + ENTITY_NODE_PRIMITIVE ) then begin
      tileId        := _values.GetValue('tile_id').AsString ;
      tileDirectory := GetPathDirSep( tileDirectory ) +
                      Trim(_featureType.FeatureClass.Coverage.VLibrary.TileMap[tileId]) ;
    end ;

    nodeTableName := GetPathDirSep( tileDirectory ) + ENTITY_NODE_PRIMITIVE ;
    nodeFile := _featureType.FeatureClass.FileStore.GetFile( nodeTableName ) ;
    row  := nodeFile.GetRowFromId( 'id', nodeId ) ;
    try
      geom := row.GetValue('coordinate').AsGeometry ;
      _values.SetGeometry( geom ) ;
    finally
      FreeObject( row ) ;
    end ;
    _values.SetShapeType( TGIS_ShapeType.Point ) ;
  end ;

//=============================================================================
// ConnectedNodeGeometryFactory
//=============================================================================

  procedure ConnectedNodeGeometryFactory.CreateGeometry(
    const _featureType  : VPFFeatureType ;
    const _values       : SimpleFeature
  ) ;
  var
    nodeId        : Integer ;
    baseDirectory : String ;
    tileDirectory : String ;
    tileId        : String ;
    nodeTableName : String ;
    nodeFile      : VPFFile ;
    row           : SimpleFeature ;
    geom          : TGIS_Point3DList ;
  begin
    nodeId := _values.GetValue('cnd_id').AsInteger ;

    baseDirectory := _featureType.FeatureClass.DirectoryName ;
    tileDirectory := baseDirectory ;

    if not FileExists( GetPathDirSep( tileDirectory ) + CONNECTED_NODE_PRIMITIVE ) then begin
      tileId        := _values.GetValue('tile_id').AsString ;
      tileDirectory := GetPathDirSep( tileDirectory ) +
                      Trim(_featureType.FeatureClass.Coverage.VLibrary.TileMap[tileId]) ;
    end ;

    nodeTableName := GetPathDirSep( tileDirectory ) + CONNECTED_NODE_PRIMITIVE ;
    nodeFile := _featureType.FeatureClass.FileStore.GetFile( nodeTableName ) ;
    row  := nodeFile.GetRowFromId( 'id', nodeId ) ;
    try
      geom := row.GetValue('coordinate').AsGeometry ;
      _values.SetGeometry( geom ) ;
    finally
      FreeObject( row ) ;
    end ;
    _values.SetShapeType( TGIS_ShapeType.Point ) ;
  end ;

//=============================================================================
// TextGeometryFactory
//=============================================================================

  procedure TextGeometryFactory.CreateGeometry(
    const _featureType  : VPFFeatureType ;
    const _values       : SimpleFeature
  ) ;
  var
    textId        : Integer ;
    baseDirectory : String ;
    tileDirectory : String ;
    tileId        : String ;
    textTableName : String ;
    textFile      : VPFFile ;
    row           : SimpleFeature ;
    geom          : TGIS_Point3DList ;
  begin
    textId := _values.GetValue('txt_id').AsInteger ;

    baseDirectory := _featureType.FeatureClass.DirectoryName ;
    tileDirectory := baseDirectory ;

    if not FileExists( GetPathDirSep( tileDirectory ) + TEXT_PRIMITIVE ) then begin
      tileId        := _values.GetValue('tile_id').AsString ;
      tileDirectory := GetPathDirSep( tileDirectory ) +
                      Trim(_featureType.FeatureClass.Coverage.VLibrary.TileMap[tileId]) ;
    end ;

    textTableName := GetPathDirSep( tileDirectory ) + TEXT_PRIMITIVE ;
    textFile := _featureType.FeatureClass.FileStore.GetFile( textTableName ) ;
    row  := textFile.GetRowFromId( 'id', textId ) ;
    try
      geom := row.GetValue('shape_line').AsGeometry ;
      _values.SetGeometry( geom ) ;
    finally
      FreeObject( row ) ;
    end ;
    _values.SetShapeType( TGIS_ShapeType.Point ) ;
  end ;

//=============================================================================
// LineGeometryFactory
//=============================================================================

  procedure LineGeometryFactory.CreateGeometry(
    const _featureType  : VPFFeatureType ;
    const _values       : SimpleFeature
  ) ;
  var
    edgeId        : Integer ;
    baseDirectory : String ;
    tileDirectory : String ;
    tileId        : String ;
    edgeTableName : String ;
    edgeFile      : VPFFile ;
    row           : SimpleFeature ;
    geom          : TGIS_Point3DList ;
  begin
    edgeId := _values.GetValue('edg_id').AsInteger ;

    baseDirectory := _featureType.FeatureClass.DirectoryName ;
    tileDirectory := baseDirectory ;

    if not FileExists( GetPathDirSep( tileDirectory ) + EDGE_PRIMITIVE ) then begin
      tileId        := _values.GetValue('tile_id').AsString ;
      tileDirectory := GetPathDirSep( tileDirectory ) +
                      Trim(_featureType.FeatureClass.Coverage.VLibrary.TileMap[tileId]) ;
    end ;

    edgeTableName := GetPathDirSep( tileDirectory ) + EDGE_PRIMITIVE ;
    edgeFile := _featureType.FeatureClass.FileStore.GetFile( edgeTableName ) ;
    row  := edgeFile.GetRowFromId( 'id', edgeId ) ;
    try
      geom := row.GetValue('coordinates').AsGeometry ;
      _values.SetGeometry( geom ) ;
    finally
      FreeObject( row ) ;
    end ;
    _values.SetShapeType( TGIS_ShapeType.Arc ) ;
  end ;

//=============================================================================
// AreaGeometryFactory
//=============================================================================

  procedure AreaGeometryFactory.CreateGeometry(
    const _featureType  : VPFFeatureType ;
    const _values       : SimpleFeature
  ) ;
  var
    faceId              : Integer ;
    tempEdgeId          : Integer ;
    isLeft              : Boolean ;
    baseDirectory       : String ;
    tileDirectory       : String ;
    tileId              : String ;
    edgeTableName       : String ;
    edgeFile            : VPFFile ;
    faceTableName       : String ;
    faceFile            : VPFFile ;
    ringTableName       : String ;
    ringFile            : VPFFile ;
    faceFeature         : SimpleFeature ;
    coordinates         : TGIS_Point3DList ;
    ringId              : Integer ;
    startEdgeId         : Integer ;
    nextEdgeId          : Integer ;
    prevNodeId          : Integer ;
    edgeRow             : SimpleFeature ;
    edgeRowEx           : SimpleFeature ;
    ringRow             : SimpleFeature ;

    leftFace            : Integer ;
    rightFace           : Integer ;
    startNode           : Integer ;
    endNode             : Integer ;
    leftEdge            : Integer ;
    rightEdge           : Integer ;
    addPoints           : Boolean ;
    leftEdgeStartNode   : Integer ;
    rightEdgeStartNode  : Integer ;
    edgeGeometry        : TGIS_Point3DList ;
    inx, cnx            : Integer ;
    coordinate          : TGIS_Point3D ;
    previousCoordinate  : TGIS_Point3D ;
    coordinateArray     : TGIS_Point3DListArray ;
  begin

    previousCoordinate := GisPoint3D(0,0,0,0);
    isLeft := False ;
    faceId := _values.GetValue('fac_id').AsInteger ;

    baseDirectory := _featureType.FeatureClass.DirectoryName ;
    tileDirectory := baseDirectory ;

    if not FileExists( GetPathDirSep( tileDirectory ) + FACE_PRIMITIVE ) then begin
      tileId        := _values.GetValue('tile_id').AsString ;
      tileDirectory := GetPathDirSep( tileDirectory ) +
                      Trim(_featureType.FeatureClass.Coverage.VLibrary.TileMap[tileId]) ;
    end ;

    edgeTableName := GetPathDirSep( tileDirectory ) + EDGE_PRIMITIVE ;
    edgeFile := _featureType.FeatureClass.FileStore.GetFile( edgeTableName ) ;

    faceTableName := GetPathDirSep( tileDirectory ) + FACE_PRIMITIVE ;
    faceFile := _featureType.FeatureClass.FileStore.GetFile( faceTableName ) ;
    faceFile.Reset ;

    ringTableName := GetPathDirSep( tileDirectory ) + RING_TABLE ;
    ringFile := _featureType.FeatureClass.FileStore.GetFile( ringTableName ) ;
    ringFile.Reset ;

    faceFile.SetPosition( faceId ) ;
    faceFeature := faceFile.ReadFeature ;

    if faceFeature <> nil then begin
      if (faceFeature.GetValue('id').AsInteger = faceId) then begin
        coordinates := TGIS_Point3DList.Create ;

        ringId      := faceFeature.GetValue('ring_ptr').AsInteger ;
        ringRow     := ringFile.GetRowFromId('id',ringId) ;
        try
          startEdgeId := ringRow.GetValue('start_edge').AsInteger ;
        finally
          FreeObject( ringRow ) ;
        end ;
        nextEdgeId  := startEdgeId ;
        prevNodeId  := -1 ;

        while (nextEdgeId > 0) do begin
          edgeRow := edgeFile.GetRowFromId('id', nextEdgeId) ;
          try
            leftFace  := edgeRow.GetValue('left_face').AsTripletId.GetId ;
            rightFace := edgeRow.GetValue('right_face').AsTripletId.GetId;
            startNode := edgeRow.GetValue('start_node').AsInteger;
            endNode   := edgeRow.GetValue('end_node').AsInteger;
            leftEdge  := edgeRow.GetValue('left_edge').AsTripletId.GetId;
            rightEdge := edgeRow.GetValue('right_edge').AsTripletId.GetId;
            addPoints := True ;

            if ( faceId = leftFace ) and ( faceId = rightFace ) then begin
                addPoints := false;

                if ( prevNodeId = startNode ) then begin
                    isLeft     := false ;
                    prevNodeId := endNode ;
                end
                else if (prevNodeId = endNode) then begin
                    isLeft     := true ;
                    prevNodeId := startNode ;
                end
                else if ( prevNodeId = -1 ) then begin
                    edgeRowEx := edgeFile.GetRowFromId('id', leftEdge) ;
                    leftEdgeStartNode  := edgeRowEx.GetValue('start_node').AsInteger ;
                    FreeObject( edgeRowEx ) ;

                    edgeRowEx := edgeFile.GetRowFromId('id', rightEdge );
                    rightEdgeStartNode := edgeRowEx.GetValue('start_node').AsInteger;
                    FreeObject( edgeRowEx ) ;

                    if (leftEdgeStartNode = endNode) then begin
                        isLeft     := true;
                        prevNodeId := startNode;
                    end
                    else if (rightEdgeStartNode = endNode) then begin
                        isLeft     := false;
                        prevNodeId := endNode;
                    end
                    else
                      assert( True, 'This edge is not part of this face.');
                end
                else
                  assert( True, 'This edge is not part of this face.');
            end
            else if (faceId = rightFace) then begin
                isLeft     := false;
                prevNodeId := endNode;
            end
            else if (faceId = leftFace) then begin
                isLeft     := true;
                prevNodeId := startNode;
            end
            else
              assert( True, 'This edge is not part of this face.');

            edgeGeometry := edgeRow.GetValue('coordinates').AsGeometry ;

            if ( addPoints ) then begin

                if (isLeft) then begin
                    for inx := edgeGeometry.Count-1 downto 0 do begin
                        coordinate := edgeGeometry[inx];

                        if GisIsSamePoint3D( previousCoordinate, GisPoint3D(0,0,0,0)) or
                            not GisIsSamePoint3D(coordinate,previousCoordinate)
                        then begin
                            coordinates.Add(coordinate);
                            previousCoordinate := coordinate;
                        end
                    end
                end
                else begin
                    for inx := 0 to edgeGeometry.Count-1 do begin
                        coordinate := edgeGeometry[inx];

                        if GisIsSamePoint3D( previousCoordinate, GisPoint3D(0,0,0,0)) or
                            not GisIsSamePoint3D(coordinate,previousCoordinate)
                        then begin
                            coordinates.Add(coordinate);
                            previousCoordinate := coordinate;
                        end
                    end
                end
            end
            else begin
                if isLeft then
                  coordinate := edgeGeometry[0]
                else
                  coordinate := edgeGeometry[edgeGeometry.Count-1];
            end ;
            if isLeft then
              tempEdgeId := leftEdge
            else
              tempEdgeId := rightEdge ;

            if (tempEdgeId = startEdgeId) then
                nextEdgeId := 0
            else
                nextEdgeId := tempEdgeId;
          finally
            FreeObject( edgeRow ) ;
          end ;
        end ;

        if (coordinates.Count>0) and not GisIsSamePoint3D( coordinate, coordinates[0] ) then
          coordinates.Add( coordinates[0] ) ;

        cnx := length( coordinateArray ) ;
        SetLength( coordinateArray, cnx + 1 ) ;
        coordinateArray[cnx] := coordinates ;
      end ;

      FreeObject( faceFeature ) ;
    end ;

    _values.SetGeometry( coordinateArray ) ;
    _values.SetShapeType( TGIS_ShapeType.Polygon ) ;
  end ;

//=============================================================================
// VPFFileStore
//=============================================================================

  { Create a file store.
  }
  constructor VPFFileStore.Create ;
  begin
    inherited ;

    Ffiles := TStringList.Create ;
    Ffiles.Sorted := True ;
  end ;

  { Destroy a file store.
  }
  procedure VPFFileStore.doDestroy ;
  var
    i  : Integer ;
    ff : VPFFile ;
  begin
    for i := 0 to Ffiles.Count-1 do begin
      ff := VPFFile( Ffiles.Objects[i] ) ;
      FreeObject( ff ) ;
    end ;

    FreeObject( Ffiles ) ;

    inherited;
  end ;

  function VPFFileStore.GetFile(
    const _path : String
  ) : VPFFile ;
  var
    idx : Integer ;
  begin
    Result := nil ;

    if Ffiles.Find( _path, idx ) then
      Result := VPFFile( Ffiles.Objects[idx] )
    else begin
      if FileExists( _path ) then begin
        Result := VPFFile.Create( _path ) ;
        Ffiles.AddObject( _path, Result ) ;
      end ;
    end ;
  end ;

//=============================================================================
// VPFHeader
//=============================================================================

  function VPFHeader.GetLength : Integer ;
  begin
    Result := -1 ;
  end ;

  function VPFHeader.GetRecordSize : Integer ;
  begin
    Result := -1 ;
  end ;

//=============================================================================
// VariableIndexHeader
//=============================================================================

  constructor VariableIndexHeader.Create(
    const _entriesNumber  : Integer ;
    const _vpfHeaderLen   : Integer
  ) ;
  begin
    FentriesNumber := _entriesNumber ;
    FvpfHeaderLen  := _vpfHeaderLen ;
  end ;

  function VariableIndexHeader.GetLength : Integer ;
  begin
    Result := 8 ;
  end ;

  function VariableIndexHeader.GetRecordSize : Integer ;
  begin
    Result := 8 ;
  end ;

//=============================================================================
// SpatialIndexHeader
//=============================================================================

  { Create a spatial index header.
  }
  constructor SpatialIndexHeader.Create(
    const _numPrims  : Integer ;
    const _xmin      : Double ;
    const _ymin      : Double ;
    const _xmax      : Double ;
    const _ymax      : Double ;
    const _numNodes  : Integer
  ) ;
  begin
    FnumPrims := _numPrims ;
    Fxmin     := _xmin ;
    Fymin     := _ymin ;
    Fxmax     := _xmax ;
    Fymax     := _ymax ;
    FnumNodes := _numNodes ;
  end ;

  function SpatialIndexHeader.GetLength : Integer ;
  begin
    Result := 24 ;
  end ;

  function SpatialIndexHeader.GetRecordSize : Integer ;
  begin
    Result := -1 ;
  end ;

//=============================================================================
// TGIS_FileVPF
//=============================================================================

  constructor TGIS_FileVPF.Create(
    const _path : String
  ) ;
  begin
    inherited Create ;

    VPFDb := VPFDataBase.Create( _path ) ;
  end ;

  procedure TGIS_FileVPF.doDestroy ;
  begin
    FreeObject( VPFDb ) ;

    inherited ;
  end ;

  procedure TGIS_FileVPF.ReadAll ;
  var
    i, j : Integer ;
  begin
    FBusyCount := 0 ;
    FBusyPos   := 0 ;

    for i := 0 to VPFDataBase(VPFDb).LibraryCount-1 do begin
      for j := 0 to  VPFDataBase(VPFDb).VLibrary[i].CoverageCount-1 do begin
        if IsStringEmpty( Category ) then
          inc( FBusyCount, VPFDataBase(VPFDb).VLibrary[i].Coverage[j].FeatureTypeCount )
        else if Category = VPFDataBase(VPFDb).VLibrary[i].Coverage[j].Name then
          inc( FBusyCount, VPFDataBase(VPFDb).VLibrary[i].Coverage[j].FeatureTypeCount )
      end ;
    end ;

    for i := 0 to VPFDataBase(VPFDb).LibraryCount-1 do
      VPFDataBase(VPFDb).VLibrary[i].ReadAll( Self ) ;
  end ;

  function TGIS_FileVPF.GetInfo : String ;
  begin
    Result := VPFDataBase(VPFDb).GetInfo ;
  end ;

  function TGIS_FileVPF.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    i, j, k : Integer ;
    db  : VPFDataBase ;
    lib : VPFLibrary ;
    cov : VPFCoverage ;
    ft  : VPFFeatureType ;
  begin
    Result := TGIS_LayerInfoList.Create ;

    db := VPFDataBase(VPFDb) ;
    for i := 0 to db.LibraryCount-1 do begin
      lib := db.VLibrary[i] ;
      for j := 0 to lib.CoverageCount-1 do begin
        cov := lib.Coverage[j] ;
        for k := 0 to cov.FeatureTypeCount-1 do begin
          ft := VPFFeatureType(cov.FeatureType[k]) ;
          Result.Add(
            TGIS_LayerInfo.Create(
              cov.Name + ';' + ft.TableName,
              TGIS_RegisteredLayerType.Vector,
              ft.ShapeType
            )
          ) ;
        end ;
      end ;
    end ;
  end ;
{==================================== END =====================================}
end.
