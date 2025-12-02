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
  Encapsulation of the GDF 3.0 file access.

  Support for:
    * XYZREC  – the coordinates  - node points & segments
    * Level 0 – the geometry     - nodes, edges, faces
    * Level 1 – simple features  - points, lines, areas & attributes
    * Level 2 – complex features - relationships, network topology
    ( Level 3 - All levels

  If there is a Record and Field definition table the parser tries to follow its
  definitions, otherwise uses default GDF structure definition. Also the parser
  tries to translate attribute' values from dictionaries if any found.
}

{$IFDEF DCC}
  unit GisLayerGDF ;
  {$HPPEMIT '#pragma link "GisLayerGDF"'}
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
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Variants,
    System.Generics.Collections,
    System.Generics.Defaults,
    GisTypes,
    GisClasses,
    
    GisLayerVector,
    GisStreams ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerGDF = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Layer that can read GDF file.
  /// </summary>
  TGIS_LayerGDF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private // various private variables

        /// <summary>
        ///   GDF file handle
        /// </summary>
        FGDFFile       : TGIS_BufferedFileStream ;

        /// <summary>
        ///   Eof marker
        /// </summary>
        FGDFEof        : Boolean    ;

        /// <summary>
        ///   Translated line
        /// </summary>
        FLineNo        : Integer    ;

        /// <summary>
        ///   Last Translated line
        /// </summary>
        FMaxLineNo     : Integer    ;

        /// <summary>
        ///   Last position in file
        /// </summary>
        FMaxPos        : Int64    ;

        /// <summary>
        ///   Name line
        /// </summary>
        FLineStr       : String     ;

        /// <summary>
        ///   Break char count
        /// </summary>
        FBreakCnt      : Integer ;

        /// <summary>
        ///   Current shape
        /// </summary>
        FCurrShape     : TGIS_Shape ;

        /// <summary>
        ///   Header info
        /// </summary>
        FInfoStr       : String     ;

        /// <summary>
        ///   Coordinates list
        /// </summary>
        mapCoord       : TDictionary<String, Int64> ;

        /// <summary>
        ///   Node list
        /// </summary>
        mapNode        : TDictionary<String, Int64> ;

        /// <summary>
        ///   Node list
        /// </summary>
        mapPoint       : TDictionary<String, Int64> ;

        /// <summary>
        ///   Edge list
        /// </summary>
        mapEdge        : TDictionary<String, Int64> ;

        /// <summary>
        ///   Line list
        /// </summary>
        mapLine        : TDictionary<String, Int64> ;

        /// <summary>
        ///   Attributes def. list
        /// </summary>
        lstAttrDef     : TStringList   ;

        /// <summary>
        ///   Attributes list
        /// </summary>
        mapAttr        : TDictionary<String, Int64> ;

        /// <summary>
        ///   Name list
        /// </summary>
        lstName        : TStringList ;

        /// <summary>
        ///   Attributes list
        /// </summary>
        mapObjAttr     : TStringList ;

        /// <summary>
        ///   Feature def. list
        /// </summary>
        lstFeaDef      : TStringList ;

        /// <summary>
        ///   Default Attribute Value list
        /// </summary>
        lstDefAttrVal  : TStringList ;

        /// <summary>
        ///   Attribute Value Def. list
        /// </summary>
        lstAttrValDef  : TStringList ;

        /// <summary>
        ///   Record definition list
        /// </summary>
        lstRecDef      : TStringList ;

        /// <summary>
        ///   Current primitive and feature handle
        /// </summary>
        FPoint         : TObject ;

        /// <summary>
        ///   Current primitive and feature handle
        /// </summary>
        FLine          : TObject ;

        /// <summary>
        ///   Current primitive and feature handle
        /// </summary>
        FNode          : TObject ;

        /// <summary>
        ///   Current primitive and feature handle
        /// </summary>
        FCplx          : TObject ;

        /// <summary>
        ///   Current primitive and feature handle
        /// </summary>
        FAttr          : TObject ;

        /// <summary>
        ///   Current primitive and feature handle
        /// </summary>
        currPtg        : TGIS_Point3D;

        /// <summary>
        ///   Current coordinates list.
        /// </summary>
        FPoints        : TGIS_Point3DList;

        /// <summary>
        ///   Record field size.
        /// </summary>
        FCoordXSize    : Integer ;

        /// <summary>
        ///   Record field size.
        /// </summary>
        FCoordYSize    : Integer ;

        /// <summary>
        ///   Record field size.
        /// </summary>
        FCoordZSize    : Integer ;

        /// <summary>
        ///   Record field size.
        /// </summary>
        FNumEdgeSize   : Integer ;

        /// <summary>
        ///   Record field size.
        /// </summary>
        FAbsRelSize    : Integer;

        /// <summary>
        ///   First coordinate flag.
        /// </summary>
        FIsFirstCoord  : Boolean;

        /// <summary>
        ///   Code pages.
        /// </summary>
        FCodePage      : Integer ;
        HasAttValNew   : Boolean ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private routines

      /// <summary>
      ///   Add attribute.
      /// </summary>
      procedure readAttribute      ;

      /// <summary>
      ///   Map coordinates.
      /// </summary>
      procedure mapCoordinates     ;

      /// <summary>
      ///   Map edges.
      /// </summary>
      procedure mapEdges           ;

      /// <summary>
      ///   Map nodes.
      /// </summary>
      procedure mapNodes           ;

      /// <summary>
      ///   Map attributes.
      /// </summary>
      procedure mapAttributes      ;

      /// <summary>
      ///   Add attribute definition.
      /// </summary>
      procedure addAttributeDefinition      ;

      /// <summary>
      ///   Add default attribute values.
      /// </summary>
      procedure addDefaultAttributeValue    ;

      /// <summary>
      ///   Add attribute value definition
      /// </summary>
      procedure addAttributeValueDefinition ;

      /// <summary>
      ///   Add feature definition.
      /// </summary>
      procedure addFeatureDefinition        ;

      /// <summary>
      ///   Read field definition
      /// </summary>
      procedure addFieldDefinition;

      /// <summary>
      ///   Add point.
      /// </summary>
      procedure addPoint          ;

      /// <summary>
      ///   Read point.
      /// </summary>
      procedure readPoint         ;

      /// <summary>
      ///   Add line.
      /// </summary>
      procedure addLine           ;

      /// <summary>
      ///   Read line.
      /// </summary>
      procedure readLine          ;

      /// <summary>
      ///   Add complex.
      /// </summary>
      procedure addComplex        ;

      /// <summary>
      ///   Read complex.
      /// </summary>
      procedure readComplex       ;

      /// <summary>
      ///   Add header.
      /// </summary>
      procedure addHeader         ;

      /// <summary>
      ///   Fetch line form GDF file.
      /// </summary>
      procedure gdfFetchLine      ;

      /// <summary>
      ///   Finish shape creation.
      /// </summary>
      procedure shapeEndAdding    ;

      /// <summary>
      ///   Prepare attributes.
      /// </summary>
      procedure prepareAttributes ;

      /// <summary>
      ///   Skip section part
      /// </summary>
      procedure skipSection       ;

      /// <summary>
      ///   Prepare lists
      /// </summary>
      procedure prepareLists      ;

      /// <summary>
      ///   Unprepare lists
      /// </summary>
      procedure unPrepareLists    ;

      /// <summary>
      ///   Read record definition.
      /// </summary>
      procedure addRecDefinition  ;

      function  fget_AttrNameByType ( const _tname   : String
                                  ): String;

      /// <summary>
      ///   Get Point by ID.
      /// </summary>
      /// <param name="_id">
      ///   Point id
      /// </param>
      procedure getPointByID      ( const _id       : String
                                  ) ;

      /// <summary>
      ///   Get Line by ID.
      /// </summary>
      /// <param name="_id">
      ///   Line id
      /// </param>
      procedure getLineByID       ( const _id      : String
                                  ) ;

      /// <summary>
      ///   Get edge index by ID
      /// </summary>
      /// <param name="_id">
      ///   edge id
      /// </param>
      /// <param name="_xyzid">
      ///   found edge coordinate ID
      /// </param>
      /// <param name="_fromid">
      ///   found node coordinate ID
      /// </param>
      /// <param name="_toid">
      ///   found node coordinate ID
      /// </param>
      function  getEdgeIdxByID    ( const _id      : String ;
                                    var   _xyzid   : Int64 ;
                                    var   _fromid  : Int64 ;
                                    var   _toid    : Int64
                                  ): Boolean ;

      /// <summary>
      ///   Build feature
      /// </summary>
      /// <param name="_id">
      ///   feature id
      /// </param>
      procedure buildFeature      ( const _id      : String
                                  ) ;

      /// <summary>
      ///   Get coordinates by pos and add to shape
      /// </summary>
      /// <param name="_pos">
      ///   coordinate position in file
      /// </param>
      procedure addCoordsByPos    ( const _pos     : Int64
                                  ) ; overload;

      /// <summary>
      ///   Get coordinates by pos and add to shape
      /// </summary>
      /// <param name="_pos">
      ///   coordinate position in file
      /// </param>
      procedure addCoordsByPos    ( const _pos     : Int64 ;
                                    const _reverse : Boolean
                                  ) ; overload;

      /// <summary>
      ///   Prepare shape creation.
      /// </summary>
      /// <param name="_shp_type">
      ///   shape type to create
      /// </param>
      procedure shapeBeginAdding  ( const _shp_type: TGIS_ShapeType
                                  ) ;

      /// <summary>
      ///   Test if _line will be continued.
      /// </summary>
      /// <param name="_line">
      ///   read line
      /// </param>
      function  gdfTestContinous  ( const _line    : String
                                  ) : Boolean ;

      /// <summary>
      ///   Test if _code and _name in the current line matches (first two
      ///   chars).
      /// </summary>
      /// <param name="_code">
      ///   code (first two chars)
      /// </param>
      /// <param name="_line">
      ///   read line
      /// </param>
      function  gdfTestLine       ( const _code    : String;
                                    const _line    : String
                                  ) : Boolean ;

      /// <summary>
      ///   Parse raw data.
      /// </summary>
      /// <param name="_off">
      ///   current line off
      /// </param>
      /// <param name="_cnt">
      ///   number of chars read
      /// </param>
      function  parseInternal     ( var  _off      : Integer;
                                    var  _cnt      : Integer
                                  ) : String;

      /// <summary>
      ///   Parse String format.
      /// </summary>
      /// <param name="_off">
      ///   current line off
      /// </param>
      /// <param name="_cnt">
      ///   number of chars read
      /// </param>
      function  parseStr          ( var   _off     : Integer ;
                                    const _cnt     : Integer
                                  ) : String ; overload;

      /// <summary>
      ///   Parse String format.
      /// </summary>
      /// <param name="_off">
      ///   current line off
      /// </param>
      /// <param name="_cnt">
      ///   number of chars read
      /// </param>
      function  parseStr          ( var   _off     : Integer ;
                                    const _cnt     : Integer ;
                                    const _cntdef  : Integer
                                  ) : String ; overload;

      /// <summary>
      ///   Parse number format.
      /// </summary>
      /// <param name="_off">
      ///   current line off
      /// </param>
      /// <param name="_cnt">
      ///   number of chars to read
      /// </param>
      function  parseNum          ( var    _off    : Integer ;
                                    const  _cnt    : Integer
                                  ) : Int64 ; overload;

      /// <summary>
      ///   Parse number format.
      /// </summary>
      /// <param name="_off">
      ///   current line off
      /// </param>
      /// <param name="_cnt">
      ///   number of chars to read
      /// </param>
      /// <param name="_cntdef">
      ///   number of chars to read if _cnt is 0
      /// </param>
      function  parseNum          ( var    _off    : Integer ;
                                    const  _cnt    : Integer ;
                                    const _cntdef  : Integer
                                  ) : Int64 ; overload;

      /// <summary>
      ///   Get coordinates by ID and add to shape
      /// </summary>
      /// <param name="_code">
      ///   attribute code
      /// </param>
      /// <param name="_val">
      ///   attribute code
      /// </param>
      function  getAttValDef      ( const _code    : String ;
                                    const _val     : String
                                   ) : String ;

      /// <summary>
      ///   Get break char count.
      /// </summary>
      function  GetBreakCount       : Integer ;

      /// <summary>
      ///   Add name.
      /// </summary>
      procedure addName ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure setUp         ; override;

         /// <inheritdoc/>
         function  getFieldInternal      ( const _uid      : TGIS_Uid;
                                           const _name     : String ;
                                           const _cursor   : Integer
                                         ) : Variant ; override;
    protected
      // destructor

         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create  ; override;
         
         /// <summary>
         ///   List of attributes.
         /// </summary>         
         {$IFDEF OXYGENE}
           property AttributeNames : TGIS_StringList read lstAttrDef;
         {$ELSE}
           property AttributeNames : TStringList     read lstAttrDef;
         {$ENDIF}
         
         /// <summary>
         ///   Get attribute by type.
         /// </summary>      
         /// <param name="_name">
         ///   attribute name
         /// </param>            
         property AttributeNameByType[ const _name : String ] : String
                                                     read fget_AttrNameByType;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Math,
    
    GisRtl,
    GisResource,
    GisRegistredLayers ;
{$ENDIF}

const
  // All-purpose media records
  GDF_NULLREC       = '  '; // [] Null Record
  GDF_CONTREC       = '00'; // [] Continuation Record
  GDF_CONTFLAG      = '1' ; // [] Continuation Flag
  GDF_NCONTFLAG     = '0' ; // [] Continuation Flag

  // Global records
  GDF_ALBHDREC      = '01'; // [] Album Header Record
  GDF_DSHDREC       = '02'; // [] Dataset Header Record
  GDF_FIELDEFREC    = '03'; // [] Field Definition Record
  GDF_RECDEFREC     = '04'; // [] Record Definition Record
  GDF_ATDEFREC      = '05'; // [] Attribute Definition Record
  GDF_DIREC         = '06'; // [] Directory Record
  GDF_FEATDEFREC    = '07'; // [] Feature Definition Record
  GDF_SPADOREC      = '08'; // [] Spatial Domain Record
  GDF_RELDEFREC     = '09'; // [] Relationship Definition Record
  GDF_ADMSTRDREC    = '10'; // [] Administrative Structure Definition Record
  GDF_ABBRREC       = '13'; // [] Abbreviation Record
  GDF_SRCEREC       = '14'; // [] Source Record
  GDF_DATTVALREC    = '15'; // [] Default Attribute Value Record
  GDF_SECHREC       = '16'; // [] Section Header Record
  GDF_LAYHREC       = '17'; // [] Layer Header Record
  GDF_ATTVALREC     = '18'; // [] Attribute Value Definition Record
  GDF_NWSPECSREC    = '19'; // [] Network Specification Record
  GDF_DATELREC      = '61'; // [] Datum & Ellipsoid Record
  GDF_VERDATREC     = '62'; // [] Vertical Datum Record
  GDF_PROJECREC     = '63'; // [] Projection Record
  GDF_NATGRIDREC    = '64'; // [] National Grid Record
  GDF_GEOIDREC      = '65'; // [] Geoid Undulation Record
  GDF_MAGNETREC     = '66'; // [] Earth Magnetic Field Record
  GDF_COMMENTREC    = '90'; // [] Comment Record
  GDF_VOLTERMREC    = '99'; // [] Volume Termination Record

  // Data records
  GDF_COORDREC      = '23'; // [] Coordinates Record
  GDF_EDGEREC       = '24'; // [] Edge Record
  GDF_NODEREC       = '25'; // [] Node Record
  GDF_FACEREC       = '29'; // [] Face Record
  GDF_NAMEREC       = '41'; // [] Name Record
  GDF_ATTREC        = '44'; // [] Attribute Record
  GDF_TIMEREC       = '45'; // [] Time Domain Record
  GDF_CONVERTREC    = '46'; // [] Conversion Record
  GDF_RELATREC      = '50'; // [] Relationship Record
  GDF_POFEREC       = '51'; // [] Point Feature Record
  GDF_LIFEREC       = '52'; // [] Line Feature Record
  GDF_ARFEREC       = '53'; // [] Area Feature Record
  GDF_COFEREC       = '54'; // [] Complex Feature Record
  GDF_OBJREFREC     = '83'; // [] Object Reference Record

  // Update Information records
  GDF_UPDINFREC     = '89'; // [] Edge Record

  // Custom Global settings
  GDF_MAX_LEN       = 80  ;
  GDF_NEW_LINE_OFF  = 3   ;
  GDF_UNDEF_FSIZE   = '*' ;
  GDF_NO_COORDS     = 0   ;
  GDF_CODE_NODE     = 25  ;
  GDF_CODE_EDGE     = 24  ;
  GDF_CODE_POINT    = 51  ;
  GDF_CODE_AREA     = 53  ;

  // ATTRIBTUES
  GDF_ATT_FEATURE   = 'FEATURE';

type

  { array of elements ID }
  T_ATabCard = array of String ;

  T_ObjAttr = class
    attrList : T_ATabCard ;
  end;

  { Record definition object }
  T_RecDefobj = class
    REC_TYPE  : Integer ;
    REC_STYPE : Integer ;
    REC_NAME  : String;
    AFlds     : array of String;
  end;

  { Volume Header Record (01) }
  T_HeaderRec = record
    REC_DESCR : Integer    ;
    SUP_NAME  : String ;
    STAN_NAME : String ;
    VERSION   : String ;
    CREA_DATE : String ;
    VOL_SIZE  : String ;
    ALBUM_ID  : Integer    ;
    TOT_VOL   : Integer    ;
    VOL_ID    : Integer    ;
    CHAR_SET  : String ;
  end ;

  { Face Record (29) }
  T_FaceEdgeRec = record
    EDGE_ID   : String  ;
    ORIENT    : Integer   ;
  end ;
  T_AFaceEdge   = array of T_FaceEdgeRec ;

  { Point Feature Record (51) }
  T_PointObj = class
    POINT_ID  : String  ;
    DESC_ID   : Integer   ;
    FEAT_CODE : String   ;
    NUM_KNOT  : Integer   ;
    KNOT_TAB  : T_ATabCard  ;
    NUM_ATT   : Integer   ;
    ATT_TAB   : T_ATabCard  ;
  end ;

  { Line Feature Record (52) }
  T_LineObj = class
    LIFE_ID   : String    ;
    DESC_ID   : Integer   ;
    FEAT_CODE : String   ;
    SPLIT_IND : Integer   ;
    NUM_EDGE  : Integer   ;
    EDGE_TAB  : T_AFaceEdge ;
    NUM_ATT   : Integer   ;
    ATTR_TAB  : T_ATabCard  ;
  end ;

  { Complex Feature Record (54) }
  T_ComplexFeatRec = record
    FEAT_CAT  : Integer   ;
    FEAT_ID   : String    ;
  end ;
  T_ACplxFeat = array of T_ComplexFeatRec ;

  { Complex Record  }
  T_Complexobj = class
    { TODO -cReview : Review }
    //REC_DESCR : Integer   ;
    COMF_ID   : Cardinal  ;
    DESC_ID   : Integer   ;
    FEAT_CODE : String   ;
    SPLIT_IND : Integer   ;
    NUM_PARTS : Integer   ;
    FeatTab   : T_ACplxFeat ;
    NUM_ATT   : Integer   ;
    ATT_TAB   : T_ATabCard  ;
  end ;

  { Segmented Attribute Record (44) }
  T_AttrAtt = record
    ATT_TYPE  : String;
    DESC_ID   : Integer   ;
    ATT_VAL   : String;
  end ;
  T_AAttrAtt = array of T_AttrAtt;

  { Segmented Attribute Record }
  T_Attrobj = class
    REC_DESCR : Integer   ;
    SATT_ID   : Cardinal  ;
    FROM_     : Integer   ;
    TO_       : Integer   ;
    ABS_REL   : String;
    ATT_DIR   : String;
    NUM_ATT   : Integer   ;
    ATT_TAB   : T_AAttrAtt  ;
  end ;

//=============================================================================
// TGIS_LayerGDF
//=============================================================================

  constructor TGIS_LayerGDF.Create ;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                           ] ;
    FPoints := TGIS_Point3DList.Create ;
  end ;

  procedure TGIS_LayerGDF.doDestroy ;
  var
    i : Integer ;
  begin
    for i := 0 to mapObjAttr.Count - 1 do
      FreeObjectNotNil( mapObjAttr.Objects[ i ] ) ;

    for i := 0 to lstRecDef.Count - 1 do
      FreeObjectNotNil( lstRecDef.Objects[ i ] ) ;

    FreeObject( mapObjAttr    ) ;
    FreeObject( mapAttr       ) ;
    FreeObject( FGDFFile      ) ;
    FreeObject( FAttr         ) ;
    FreeObject( lstAttrValDef ) ;
    FreeObject( lstRecDef     ) ;
    FreeObject( lstAttrDef    ) ;
    FreeObject( lstName       ) ;
    FreeObject( FPoints       ) ;

    inherited ;
  end ;

  function TGIS_LayerGDF.gdfTestLine( const _code : String;
                                      const _line : String
                                    ) : Boolean ;
  begin
    Result := ( _code[ StringFirst ]     = _line[ StringFirst ]     ) and
              ( _code[ StringFirst + 1 ] = _line[ StringFirst + 1 ] ) ;
  end ;

  function TGIS_LayerGDF.gdfTestContinous( const _line : String ) : Boolean ;
  begin
    Result := _line[ GDF_MAX_LEN + StringFirst - 1 ] = GDF_CONTFLAG ;
  end ;

  procedure TGIS_LayerGDF.gdfFetchLine ;
  var
    abort : Boolean ;
  begin
    if not FGDFEof then begin
      if FGDFFile.Position + GDF_MAX_LEN <= FGDFFile.Size then
        FGDFFile.ReadAsciiString( FLineStr, GDF_MAX_LEN )
      else
        FGDFFile.ReadAsciiString( FLineStr, FGDFFile.Size - FGDFFile.Position ) ;

      FGDFFile.Position := FGDFFile.Position + FBreakCnt;
      inc( FLineNo ) ;
      FGDFEof := FGDFFile.Position >= FGDFFile.Size ;
    end;

    FMaxLineNo := Max( FMaxLineNo, FLineNo ) ;

    if FMaxLineNo mod GIS_PROGRESS_TRESHOLD = 0 then begin
      if assigned( Viewer ) then begin
        abort := False ;
        if Viewer.Ref.BusyLevel > 0 then
          abort := RaiseBusyShake( Self, FMaxPos, FGDFFile.Size ) ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerGDF.shapeBeginAdding( const _shp_type : TGIS_ShapeType ) ;
  begin
    FCurrShape := CreateShape( _shp_type, TGIS_DimensionType.XYZ ) ;
    FCurrShape.Lock( TGIS_Lock.Internal ) ;
    FCurrShape.AddPart ;
  end ;

  procedure TGIS_LayerGDF.shapeEndAdding ;
  begin
    FCurrShape.Unlock;
  end ;

  procedure TGIS_LayerGDF.mapAttributes;
  var
    off : Integer;
    id  : String ;
    pos : Int64  ;
  begin
    off := StringFirst + 2 ;
    id  := parseStr( off, 0, 10 )  ;
    pos := FGDFFile.Position - GDF_MAX_LEN - FBreakCnt;

    mapAttr.Add( id, pos ) ;
  end;

  procedure TGIS_LayerGDF.mapCoordinates;
  var
    off : Integer;
    pos : Int64;
  begin
    off := StringFirst + 2 ;
    pos := FGDFFile.Position - GDF_MAX_LEN - FBreakCnt;
    mapCoord.Add( parseStr( off, 0,10 ), pos ) ;
  end;

  procedure TGIS_LayerGDF.mapNodes;
  var
    off : Integer   ;
    id,
    xyz : String ;
    idx : {$IFDEF JAVA} nullable {$ENDIF} Int64 ;
  begin
    off := StringFirst + 2 ;
    id  := parseStr( off, 0, 10 ) ;
    xyz := parseStr( off, 0, 10 ) ;

    if mapCoord.TryGetValue( xyz, idx ) then
      mapNode.Add( id, idx )
  end;

  procedure TGIS_LayerGDF.mapEdges;
  var
    off : Integer   ;
    pos : Int64;
  begin
    off := StringFirst + 2 ;
    pos := FGDFFile.Position - GDF_MAX_LEN - FBreakCnt;
    mapEdge.Add( parseStr( off, 0, 10 ), pos ) ;
  end;

  function TGIS_LayerGDF.GetBreakCount : Integer;
  var
    c    : Char ;
    iold : Integer ;
  begin
    Result := 0;
    iold := FGDFFile.Position;
    try
      if FGDFFile.Position + GDF_MAX_LEN + 2 < FGDFFile.Size then
        FGDFFile.Position := FGDFFile.Position + GDF_MAX_LEN ;

      while True do begin
        {$IFDEF OXYGENE}
        FGDFFile.ReadChar( c, 2 );
        {$ELSE}
        FGDFFile.ReadBuffer( c, 1 );
        {$ENDIF}
        if ( c = #13 ) or ( c = #10 ) then
          inc( Result )
        else
          break;
      end;
    finally
      FGDFFile.Position := iold ;
    end;
  end;

  procedure TGIS_LayerGDF.addName ;
  var
    off  : Integer   ;
    id,
    txt  : String ;
  begin
    off  := StringFirst + 2 ;
    id   := parseStr( off, 0, 10 ) ;
    off  := StringFirst + 20 ;
    txt  := parseStr( off, 0, GDF_MAX_LEN );

    lstName.Add( id + '=' + txt );
  end ;

  procedure TGIS_LayerGDF.readAttribute ;
  var
    off   : Integer     ;
    j     : Integer     ;
    i, k  : Integer     ;
    attr  : T_Attrobj   ;
    obj   : T_RecDefobj ;
  begin
    off  := StringFirst ;
    j    := 0 ;
    attr := FAttr as T_Attrobj ;

    { if record definition found try to follow the field list }
    if lstRecDef.Find( GDF_ATTREC, k ) then begin
      obj := T_RecDefobj( lstRecDef.Objects[ k ] ) ;
      while j < length( obj.AFlds ) do begin
        if      obj.AFlds[ j ] =  'REC_DESCR' then
          attr.REC_DESCR := parseNum( off, 0,  2 )
        else if obj.AFlds[ j ] =  'SATT_ID'   then
          attr.SATT_ID   := parseNum( off, 0, 10 )
        else if obj.AFlds[ j ] =  'FROM'      then
          attr.FROM_     := parseNum( off, 0,  5 )
        else if obj.AFlds[ j ] =  'TO'        then
          attr.TO_       := parseNum( off, 0,  5 )
        else if obj.AFlds[ j ] =  'ABS_REL'   then
          attr.ABS_REL   := parseStr( off, 0,  1 )
        else if obj.AFlds[ j ] =  'ATT_DIR'   then
          attr.ATT_DIR   := parseStr( off, 0,  1 )
        else if obj.AFlds[ j ] =  'NUM_ATT'   then begin
          attr.NUM_ATT   := parseNum( off, 0,  5 ) ;
          inc( j ) ;
          if attr.NUM_ATT > 0 then begin
            SetLength( attr.ATT_TAB, attr.NUM_ATT ) ;

            for i := 0 to attr.NUM_ATT - 1 do  begin
              {$IFDEF GIS_NORECORDS} 
                attr.ATT_TAB[i] := new T_AttrAtt ;
              {$ENDIF}
             if T_RecDefobj( obj ).AFlds[ j ] = '(ATT_TYPE)' then begin
               attr.ATT_TAB[i].ATT_TYPE := parseStr( off, 0, 2 ) ;
               inc( j ) ;
             end ;
             if T_RecDefobj( obj ).AFlds[ j ] = '(DESC_ID)' then begin
               attr.ATT_TAB[i].DESC_ID := parseNum( off, 0, 5 ) ;
               inc( j ) ;
             end ;
             if T_RecDefobj( obj ).AFlds[ j ] = '(ATT_VAL)' then begin
               attr.ATT_TAB[i].ATT_VAL := parseStr( off, 0, 10 ) ;
             end ;
             if i < attr.NUM_ATT - 1 then
               dec( j, 2 );
            end;
          end ;
        end;

        inc( j ) ;
      end;
    end
    else begin
      attr.REC_DESCR := parseNum( off, 0,  2 ) ;
      attr.SATT_ID   := parseNum( off, 0, 10 ) ;
      attr.FROM_     := parseNum( off, 0,  5 ) ;
      attr.TO_       := parseNum( off, 0,  5 ) ;
      attr.ABS_REL   := parseStr( off, 0,  1 ) ;
      attr.ATT_DIR   := parseStr( off, 0,  1 ) ;
      attr.NUM_ATT   := parseNum( off, 0,  5 ) ;

      if attr.NUM_ATT > 0 then begin
        SetLength( attr.ATT_TAB, attr.NUM_ATT ) ;

        for i := 0 to attr.NUM_ATT - 1 do begin
          {$IFDEF GIS_NORECORDS} 
            attr.ATT_TAB[i] := new T_AttrAtt ;
          {$ENDIF}
          attr.ATT_TAB[i].ATT_TYPE := parseStr( off, 0,  2 );
          attr.ATT_TAB[i].DESC_ID  := parseNum( off, 0,  5 );
          attr.ATT_TAB[i].ATT_VAL  := parseStr( off, 0, 10 );
        end
      end ;
   end;
  end ;

  procedure TGIS_LayerGDF.readComplex;
  var
    off  : Integer      ;
    i    : Integer      ;
    cplx : T_Complexobj ;
  begin
    off  := StringFirst + 2 ;
    cplx := FCplx as T_Complexobj ;

    cplx.COMF_ID   := parseNum( off, 0, 10 ) ;
    cplx.DESC_ID   := parseNum( off, 0,  5 ) ;
    cplx.FEAT_CODE := parseStr( off, 0,  4 ) ;
    cplx.SPLIT_IND := parseNum( off, 0,  1 ) ;
    cplx.NUM_PARTS := parseNum( off, 0,  5 ) ;

    if cplx.NUM_PARTS > 0 then begin
      SetLength( cplx.FeatTab, cplx.NUM_PARTS ) ;

      for i := 0 to cplx.NUM_PARTS - 1 do begin
        {$IFDEF GIS_NORECORDS} 
          cplx.FeatTab[i] := new T_ComplexFeatRec ;
        {$ENDIF}
        cplx.FeatTab[i].FEAT_CAT := parseNum( off, 2 );
        cplx.FeatTab[i].FEAT_ID  := parseStr( off, 0, 10 );
      end ;
    end ;

    cplx.NUM_ATT := parseNum( off,  5  ) ;

    if cplx.NUM_ATT > 0 then begin
      SetLength( cplx.ATT_TAB, cplx.NUM_ATT ) ;

      for i := 0 to cplx.NUM_ATT - 1 do
        cplx.ATT_TAB[i] := parseStr( off, 0, 10 ) ;
    end ;
  end;

  procedure TGIS_LayerGDF.addComplex ;
  var
    i       : Integer       ;
    cplx    : T_Complexobj ;
    oldpos  : Int64;
  begin
    oldpos := FGDFFile.Position ;
    try
      cplx := FCplx as T_Complexobj ;
      readComplex;

      for i := 0 to cplx.NUM_PARTS - 1 do begin
          case cplx.FeatTab[ i ].FEAT_CAT of
            1 : begin // Point
                  shapeBeginAdding( TGIS_ShapeType.MultiPoint ) ;
                  try
                    getPointByID( cplx.FeatTab[i].FEAT_ID ) ;
                  finally
                    shapeEndAdding ;
                  end;
                end ;
            2 : begin // Line
                  shapeBeginAdding( TGIS_ShapeType.Arc ) ;
                  try
                    getLineByID( cplx.FeatTab[i].FEAT_ID ) ;
                  finally
                    shapeEndAdding ;
                  end;
             end ;
            3 : begin // Area
                  shapeBeginAdding( TGIS_ShapeType.Arc ) ;
                  try
                    getLineByID( cplx.FeatTab[i].FEAT_ID ) ;
                  finally
                    shapeEndAdding ;
                  end;
                end ;
            4 : begin // Cplx
                  exit ;  // not implemented
                end ;
          end ;

       buildFeature( cplx.FEAT_CODE ) ;
      end ;
    finally
      FGDFFile.Position := oldpos ;
    end;
  end ;

  procedure TGIS_LayerGDF.readLine;
  var
    off  : Integer   ;
    j    : Integer   ;
    i    : Integer   ;
    line : T_LineObj ;
    str  : String    ;
  begin
    off   := StringFirst + 2 ;
    line  := FLine as T_LineObj;
    line.LIFE_ID   := parseStr( off, 0, 10 ) ;
    line.DESC_ID   := parseNum( off, 0,  5 ) ;
    line.FEAT_CODE := parseStr( off, 0,  4 ) ;
    line.SPLIT_IND := parseNum( off, 0,  1 ) ;

    str := Trim( parseStr( off, FNumEdgeSize, 5 ) ) ;
    if not IsStringEmpty( str ) then
      j := StrToInt( str )
    else begin
      inc( FNumEdgeSize, 5 );
      j := parseNum( off, 5, 5 );
    end;

    if j > line.NUM_EDGE then
      SetLength( line.EDGE_TAB, j ) ;

    line.NUM_EDGE := j;
    for i := 0 to line.NUM_EDGE - 1 do begin
      {$IFDEF GIS_NORECORDS} 
        line.EDGE_TAB[ i ] := new T_FaceEdgeRec ;
      {$ENDIF}
      line.EDGE_TAB[ i ].EDGE_ID := parseStr( off, 0, 10 ) ;
      line.EDGE_TAB[ i ].ORIENT  := parseNum( off, 0, 2  ) ;
    end ;

    line.NUM_ATT  := parseNum( off, 0, 5 ) ;

    if line.NUM_ATT > 0 then begin
      SetLength( line.ATTR_TAB, line.NUM_ATT ) ;
      for i := 0 to line.NUM_ATT-1 do begin
        line.ATTR_TAB[ i ] := parseStr( off, 0, 10 ) ;
      end ;
    end ;
  end;

  procedure TGIS_LayerGDF.addLine;
  var
    i      : Integer ;
    xyzid,
    fromid,
    toid   : Int64;
    oldpos : Int64;
    line   : T_LineObj;
    Pos    : Int64;
    attr   : T_ObjAttr;
  begin
    oldpos := FGDFFile.Position ;
    line   := FLine as T_LineObj;
    try
      readLine;
      shapeBeginAdding( TGIS_ShapeType.Arc ) ;
      try
        for i := 0 to line.NUM_EDGE - 1 do
          if getEdgeIdxByID( line.EDGE_TAB[i].EDGE_ID, xyzid, fromid, toid ) then
          begin
           if line.EDGE_TAB[i].ORIENT = 0 then
            begin
              addCoordsByPos( fromid  ) ;
              addCoordsByPos( xyzid   ) ;
              addCoordsByPos( toid    ) ;
            end
            else begin
              addCoordsByPos( toid        ) ;
              addCoordsByPos( xyzid, True ) ;
              addCoordsByPos( fromid      ) ;
            end;
          end;
      finally
        shapeEndAdding ;
      end;
      buildFeature( line.FEAT_CODE ) ;

      if ( line.NUM_ATT > 0 )  then
      begin
        attr := T_ObjAttr.Create;
        SetLength( attr.attrList, line.NUM_ATT );
        for i := 0 to line.NUM_ATT - 1 do
          attr.attrList[ i ] := line.ATTR_TAB[ i ] ;

        mapObjAttr.AddObject( IntToStr( FCurrShape.Uid ), attr );
      end;
    finally
      FGDFFile.Position := oldpos ;
      Pos := FGDFFile.Position - GDF_MAX_LEN - FBreakCnt;
      mapLine.Add( line.LIFE_ID, Pos ) ;
    end ;
  end ;

  procedure TGIS_LayerGDF.readPoint;
  var
    off   : Integer    ;
    i     : Integer    ;
    point : T_PointObj ;
  begin
    off   := StringFirst + 2 ;
    point := FPoint as T_PointObj;

    point.POINT_ID  := parseStr( off, 0, 10 ) ;
    point.DESC_ID   := parseNum( off, 0,  5 ) ;
    point.FEAT_CODE := parseStr( off, 0,  4 ) ;
    point.NUM_KNOT  := parseNum( off, 0,  5 ) ;

    if point.NUM_KNOT > 0 then begin
      SetLength( point.KNOT_TAB, point.NUM_KNOT ) ;

      for i := 0 to point.NUM_KNOT - 1 do begin
        point.KNOT_TAB[i] := parseStr( off, 0, 10 ) ;
      end ;
    end ;

    point.NUM_ATT := parseNum( off, 0, 5 ) ;

    if point.NUM_ATT > 0 then begin
      SetLength( point.ATT_TAB, point.NUM_ATT ) ;

      for i := 0 to point.NUM_ATT - 1 do
        point.ATT_TAB[i] := parseStr( off, 0, 10 ) ;
    end ;
  end;

  procedure TGIS_LayerGDF.addPoint ;
  var
    i      : Integer  ;
    oldpos : Int64    ;
    point  : T_PointObj;
    idx    : {$IFDEF JAVA} nullable {$ENDIF} Int64 ;
    attr   : T_ObjAttr;
  begin
    oldpos := FGDFFile.Position ;
    point  := FPoint as T_PointObj;
    try
      readPoint ;
      shapeBeginAdding( TGIS_ShapeType.MultiPoint ) ;
      try
        for i := 0 to point.NUM_KNOT - 1 do
          if mapNode.TryGetValue( point.KNOT_TAB[ i ], idx ) then
          begin
            addCoordsByPos( idx );
            mapPoint.Add( point.POINT_ID, idx ) ;
          end;
      finally
        shapeEndAdding ;
      end;
      buildFeature( point.FEAT_CODE ) ;

      if point.NUM_ATT > 0 then
      begin
        attr := T_ObjAttr.Create;
        SetLength( attr.attrList, point.NUM_ATT );
        for i := 0 to point.NUM_ATT - 1 do
          attr.attrList[ i ] := point.ATT_TAB[ i ] ;

        mapObjAttr.AddObject( IntToStr( FCurrShape.Uid ), attr );
      end;
    finally
      FGDFFile.Position := oldpos ;
    end;
  end ;

  procedure TGIS_LayerGDF.addAttributeDefinition ;
  var
    off     : Integer ;
    attType : String  ;
    attDesc : String  ;
  begin
    off     := StringFirst + 2 ;
    attType := parseStr( off,  2 ) ;
    off     := StringFirst + 43 ;
    attDesc := parseStr( off, GDF_MAX_LEN ) ;

    lstAttrDef.Values[ attType ] := attDesc;
    AddFieldInternal( attType, TGIS_FieldType.String, 1, 0 );
  end ;

  function TGIS_LayerGDF.parseInternal( var _off : Integer ;
                                        var _cnt : Integer
                                      ) : String ;
  var
    stmp : String ;
  begin
    SetLengthStr( Result, _cnt) ;

    if      ( _off + _cnt > GDF_MAX_LEN + StringFirst - 1 ) and
            ( gdfTestContinous( FLineStr )   ) then
            begin
              stmp := Trim( Copy( FLineStr,
                                  _off,
                                  GDF_MAX_LEN + StringFirst - 1 - _off
                                )
                          ) ;
              gdfFetchLine ;
              _off := GDF_NEW_LINE_OFF + StringFirst - 1 ;

              if IsStringEmpty( stmp ) then
                Result := Trim( Copy( FLineStr,
                                      _off,
                                      _cnt
                                    )
                              )
              else
                Result := Trim( stmp + Copy( FLineStr,
                                            _off,
                                            _cnt - length( stmp )
                                          )
                               )
            end
    else if ( _off + _cnt = GDF_MAX_LEN + StringFirst - 1   ) or
            ( ( _off + _cnt > GDF_MAX_LEN + StringFirst - 1   ) and
              ( not gdfTestContinous( FLineStr ) )
            ) then
            begin
              _cnt   := GDF_MAX_LEN + StringFirst - 1 - _off ;
              Result := Trim( Copy( FLineStr, _off, _cnt ) ) ;
            end
    else    Result := Trim( Copy( FLineStr, _off, _cnt ) ) ;
  end ;

  function TGIS_LayerGDF.parseNum( var   _off      : Integer ;
                                   const _cnt      : Integer
                                 ) : Int64 ;
  begin
    Result := parseNum( _off, _cnt, -1 ) ;
  end ;

  function TGIS_LayerGDF.parseNum( var   _off      : Integer ;
                                   const _cnt      : Integer ;
                                   const _cntdef   : Integer
                                 ) : Int64 ;
  var
    stmp : String ;
    cnt  : Integer    ;
  begin
    if _cnt = 0 then
      cnt := _cntdef
    else
      cnt := _cnt ;

    stmp := ( parseInternal( _off, cnt ) ) ;
    if not IsStringEmpty( stmp ) then
      Result := StrToInt64( stmp )
    else
      Result := 0;

    inc( _off, cnt ) ;
  end ;

  function TGIS_LayerGDF.parseStr( var   _off     : Integer ;
                                   const _cnt     : Integer
                                 ) : String ;
  begin
    Result := parseStr( _off, _cnt, -1 ) ;
  end ;

  function TGIS_LayerGDF.parseStr( var   _off     : Integer ;
                                   const _cnt     : Integer ;
                                   const _cntdef  : Integer
                                 ) : String ;
  var
    cnt : Integer ;
  begin
    if _cnt = 0 then
      cnt := _cntdef
    else
      cnt := _cnt ;

    Result := parseInternal (_off, cnt ) ;
    inc( _off, cnt ) ;
  end ;

  procedure TGIS_LayerGDF.addFeatureDefinition ;
  var
    off     : Integer ;
    feaCode : String  ;
    feaName : String;
  begin
    off     := StringFirst + 2 ;
    feaCode := parseStr( off, 0, 4 ) ;
    feaName := parseStr( off, 0, 20 ) ;

    lstFeaDef.Values[ feaCode ] := feaName ;
  end ;

  procedure TGIS_LayerGDF.addDefaultAttributeValue ;
  var
    off     : Integer ;
    attType : String  ;
    defVal  : String  ;
  begin
    off     := StringFirst + 2 ;
    attType := parseStr( off, 0,  2 ) ;
    defVal  := parseStr( off, 0, 10 ) ;

    lstDefAttrVal.Values[ attType ] := defVal ;
  end ;

  procedure TGIS_LayerGDF.addAttributeValueDefinition;
  var
    off : Integer ;
    v1  : String  ;
    v2  : String  ;
    val : String  ;
    dsc : String  ;
  begin
    off  := StringFirst + 2 ;
    v1   := parseStr( off, 0, 2 ) ;
    v2   := parseStr( off, 0, 10 ) ;
    val  := v1 + v2 ;
    if HasAttValNew then
      inc( off, 3 );
    dsc := parseStr( off, 0, GDF_MAX_LEN ) ;

    lstAttrValDef.Values[ val ] := dsc ;
  end ;

  procedure TGIS_LayerGDF.addHeader ;
  var
    off : Integer     ;
    hdr : T_HeaderRec ;
  begin
    off := StringFirst ;

    try
      hdr.REC_DESCR   := parseNum( off,  2 ) ;
      hdr.SUP_NAME    := parseStr( off, 20 ) ;
      hdr.STAN_NAME   := parseStr( off, 10 ) ;
      hdr.VERSION     := parseStr( off,  4 ) ;
      hdr.CREA_DATE   := parseStr( off,  6 ) ;
      hdr.VOL_SIZE    := parseStr( off, 10 ) ;
      hdr.ALBUM_ID    := parseNum( off, 10 ) ;
      hdr.TOT_VOL     := parseNum( off,  4 ) ;
      hdr.VOL_ID      := parseNum( off,  4 ) ;
      hdr.CHAR_SET    := parseStr( off, 10 ) ;

      FInfoStr := hdr.STAN_NAME + ' '   +
                  hdr.VERSION   + ' ( ' +
                  hdr.CREA_DATE + ' '   +
                  hdr.SUP_NAME  + ' )';
    except
      // nothing important
    end ;
  end ;

  function TGIS_LayerGDF.getAttValDef( const _code : String;
                                       const _val  : String
                                      ): String;
  {$IFDEF JAVA}
  const
    cAZCharSet : TSysCharSet =     
    ( 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
     ) ;
  {$ENDIF}
  var
    tmp : String  ;
  begin
    try
      if not CharInSet( UpCase( _val[StringFirst] ), {$IFDEF JAVA} cAZCharSet {$ELSE} ['A'..'Z'] {$ENDIF} ) then begin
        Result := lstName.Values[ _val ] ;
        if IsStringEmpty( Result ) then begin
          tmp := _code + _val ;
          Result := lstAttrValDef.Values[ tmp ] ;

          if IsStringEmpty( Result ) then
            Result := _val;
        end;
      end
      else
        Result := _val ;
    except
      Result := _val  ;
    end;
  end;

  procedure TGIS_LayerGDF.addCoordsByPos( const _pos      : Int64
                                        ) ;
  begin
    addCoordsByPos( _pos, False ) ;
  end ;

  procedure TGIS_LayerGDF.addCoordsByPos( const _pos      : Int64 ;
                                          const _reverse  : Boolean
                                        ) ;
  var
    off       : Integer ;
    i         : Integer ;
    numCoord  : Integer ;
    oldOff    : Integer ;
    x         : String  ;
    y         : String  ;
  begin
    {$IFDEF GIS_NORECORDS}
      currPtg := new TGIS_Point3D;
    {$ENDIF}
    if _pos = 0 then Exit;

    FGDFFile.Position := _pos ;
    gdfFetchLine;
    off := StringFirst + 20 ;
    numCoord := parseNum( off, 0, 5 ) ;

    // test size of coordinates for undefined data
    if FIsFirstCoord then begin
      oldOff := off ;
      while True do begin
        x := parseStr( off, FCoordXSize, 10 );
        y := parseStr( off, FCoordYSize, 10 );

        try
          StrToFloat( x ) ;
          StrToFloat( y ) ;
          break ;
        except
          // selected size is incorrect
        end;

        if ( FCoordXSize > 20 ) then break;

        off := oldOff ;
        inc( FCoordXSize );
        inc( FCoordYSize );
        inc( FCoordZSize );
      end;

      off := oldOff ;
      FIsFirstCoord := False;
    end ;

    if not _reverse then begin
      for i := 0 to numCoord - 1 do begin
        currPtg.X := parseNum( off, FCoordXSize, 10 ) ;
        currPtg.Y := parseNum( off, FCoordYSize, 10 ) ;
        currPtg.Z := parseNum( off, FCoordZSize, 10 ) ;

        FCurrShape.AddPoint3D( currPtg ) ;
      end ;
    end
    else begin
      FPoints.Clear;
      for i := 0 to numCoord - 1 do begin
        currPtg.X := parseNum( off, FCoordXSize, 10 ) ;
        currPtg.Y := parseNum( off, FCoordYSize, 10 ) ;
        currPtg.Z := parseNum( off, FCoordZSize, 10 ) ;

        FPoints.Add( currPtg ) ;
      end ;

      for i := FPoints.Count - 1 downto 0 do begin
        FCurrShape.AddPoint3D( FPoints[ i ] ) ;
      end;
    end;

  end;

  procedure TGIS_LayerGDF.getPointByID( const _id : String ) ;
  var
    idx : {$IFDEF JAVA} nullable {$ENDIF} Int64 ;
  begin
    if mapPoint.TryGetValue( _id, idx ) then
      addCoordsByPos( idx );
  end ;

  procedure TGIS_LayerGDF.getLineByID( const _id : String );
  var
    ipos   : Int64 ;
    idx    : {$IFDEF JAVA} nullable {$ENDIF} Int64 ;
    i      : Integer   ;
    line   : T_LineObj;
    xyzid,
    fromid,
    toid   : Int64;
  begin
    if mapLine.TryGetValue( _id, idx ) then begin
      ipos := idx ;
      FGDFFile.Position := ipos ;
      line := FLine as T_LineObj;
      gdfFetchLine;
      readLine;

      for i := 0 to line.NUM_EDGE - 1 do
        if getEdgeIdxByID( line.EDGE_TAB[i].EDGE_ID, xyzid, fromid, toid ) then
        begin
          addCoordsByPos( fromid ) ;
          addCoordsByPos( xyzid  ) ;
          addCoordsByPos( toid ) ;
        end;
    end;
  end;

  function TGIS_LayerGDF.getEdgeIdxByID( const _id     : String ;
                                         var   _xyzid  : Int64 ;
                                         var   _fromid : Int64 ;
                                         var   _toid   : Int64
                                         ) : Boolean ;
  var
    ipos    : {$IFDEF JAVA} nullable {$ENDIF} Int64 ;
    off     : Integer ;
    fknot   : String  ;
    tknot   : String  ;
    xyzid   : String  ;
  begin
    Result := False ;

    if mapEdge.TryGetValue( _id, ipos ) then begin
      FGDFFile.Position := ipos;
      gdfFetchLine;
      off := StringFirst + 12 ;
      xyzid  := parseStr( off, 0, 10 ) ;
      fknot  := parseStr( off, 0, 10 ) ;
      tknot  := parseStr( off, 0, 10 ) ;

      if mapCoord.TryGetValue( xyzid, ipos ) then
        _xyzid := ipos
      else
        _xyzid := 0;
      if mapNode.TryGetValue( fknot, ipos ) then
        _fromid := ipos
      else
        _fromid := 0;
      if mapNode.TryGetValue( tknot, ipos ) then
        _toid := ipos
      else
        _toid := 0;

      Result  := True ;
    end;
  end ;

  function TGIS_LayerGDF.getFieldInternal( const _uid    : TGIS_Uid;
                                           const _name   : String ;
                                           const _cursor : Integer
                                         ) : Variant;
  var
    i, j, k : Integer   ;
    ipos    : {$IFDEF JAVA} nullable {$ENDIF} Int64     ;
    attr    : T_Attrobj ;
    objAttr : T_ObjAttr ;
  begin
    Result := NullVar ;
    if mapObjAttr.Find( IntToStr( _uid ), k ) then begin
      objAttr := T_ObjAttr( mapObjAttr.Objects[ k ] );
      for i := 0 to length( objAttr.attrList ) - 1 do begin
        if mapAttr.TryGetValue( objAttr.attrList[ i ], ipos ) then begin
          FGDFFile.Position := ipos ;
          attr := FAttr as T_Attrobj;
          gdfFetchLine;
          readAttribute;

          for j := 0 to attr.NUM_ATT - 1 do begin
              if _name = attr.ATT_TAB[ j ].ATT_TYPE then begin
                Result := getAttValDef( attr.ATT_TAB[ j ].ATT_TYPE,
                                        attr.ATT_TAB[ j ].ATT_VAL
                                       );
                break;
              end;
          end;
        end;
      end;
    end;
  end;

  procedure TGIS_LayerGDF.prepareAttributes;
  begin
    AddFieldInternal( GDF_ATT_FEATURE, TGIS_FieldType.String, 1, 0 ) ;
  end ;

  function TGIS_LayerGDF.fget_AttrNameByType( const _tname : String
                                          ) : String ;
  begin
    Result := lstAttrDef.Values[ _tname ]
  end ;

  procedure TGIS_LayerGDF.buildFeature( const _id : String ) ;
  var
    fea : String ;
  begin
    fea := lstFeaDef.Values[ _id ] ;

    if IsStringEmpty( fea ) then
      fea := _id ;

    FCurrShape.SetField( GDF_ATT_FEATURE, fea );
  end ;

  procedure TGIS_LayerGDF.prepareLists;
  begin
    lstFeaDef     := TStringList.Create ;
    lstDefAttrVal := TStringList.Create ;
    lstAttrValDef := TStringList.Create ;
    lstRecDef     := TStringList.Create ;
    lstRecDef.Sorted := True ;
    lstAttrDef    := TStringList.Create ;
    lstName       := TStringList.Create ;
    lstName.Sorted := True ;
    mapAttr       := TDictionary<String, Int64>.Create ;
    mapObjAttr    := TStringList.Create ;
    mapObjAttr.Sorted := True ;

    mapCoord  := TDictionary<String, Int64>.Create(
                   {$IFDEF OXYGENE}
                     {$IFDEF JAVA}
                       java.lang.String.CASE_INSENSITIVE_ORDER
                     {$ELSE}
                       StringComparer.OrdinalIgnoreCase
                     {$ENDIF}
                   {$ELSE}
                     TIStringComparer.Ordinal
                   {$ENDIF}
                  ) ;
    mapNode   := TDictionary<String, Int64>.Create(
                   {$IFDEF OXYGENE}
                     {$IFDEF JAVA}
                       java.lang.String.CASE_INSENSITIVE_ORDER
                     {$ELSE}
                       StringComparer.OrdinalIgnoreCase
                     {$ENDIF}
                   {$ELSE}
                     TIStringComparer.Ordinal
                   {$ENDIF}
                  ) ;
    mapEdge   := TDictionary<String, Int64>.Create(
                   {$IFDEF OXYGENE}
                     {$IFDEF JAVA}
                       java.lang.String.CASE_INSENSITIVE_ORDER
                     {$ELSE}
                       StringComparer.OrdinalIgnoreCase
                     {$ENDIF}
                   {$ELSE}
                     TIStringComparer.Ordinal
                   {$ENDIF}
                  ) ;
    mapPoint  := TDictionary<String, Int64>.Create(
                   {$IFDEF OXYGENE}
                     {$IFDEF JAVA}
                       java.lang.String.CASE_INSENSITIVE_ORDER
                     {$ELSE}
                       StringComparer.OrdinalIgnoreCase
                     {$ENDIF}
                   {$ELSE}
                     TIStringComparer.Ordinal
                   {$ENDIF}
                  ) ;
    mapLine   := TDictionary<String, Int64>.Create(
                   {$IFDEF OXYGENE}
                     {$IFDEF JAVA}
                       java.lang.String.CASE_INSENSITIVE_ORDER
                     {$ELSE}
                       StringComparer.OrdinalIgnoreCase
                     {$ENDIF}
                   {$ELSE}
                     TIStringComparer.Ordinal
                   {$ENDIF}
                  ) ;

    FPoint        := T_PointObj.Create ;
    FLine         := T_LineObj.Create  ;
    FCplx         := T_Complexobj.Create ;
    FAttr         := T_Attrobj.Create ;

    SetLengthStr( FLineStr, GDF_MAX_LEN ) ;
  end ;

  procedure TGIS_LayerGDF.skipSection;
  begin
    if gdfTestLine( GDF_SECHREC , FLineStr ) then
      repeat
        gdfFetchLine ;
      until not gdfTestLine( GDF_SECHREC , FLineStr ) ;
  end ;

  procedure TGIS_LayerGDF.unPrepareLists ;
  begin
    FreeObject( lstFeaDef     ) ;
    FreeObject( lstDefAttrVal ) ;
    FreeObject( mapCoord      ) ;
    FreeObject( mapNode       ) ;
    FreeObject( mapEdge       ) ;
    FreeObject( mapPoint      ) ;
    FreeObject( mapLine       ) ;
    FreeObject( FPoint        ) ;
    FreeObject( FLine         ) ;
    FreeObject( FNode         ) ;
    FreeObject( FCplx         ) ;
  end ;

  procedure TGIS_LayerGDF.addFieldDefinition;
  var
    off   : Integer ;
    fname : String  ;
    size  : String  ;
    Fsize : Integer ;
  begin
    off   := StringFirst + 2 ;
    fname := Trim( parseStr( off, 10 ) )  ;
    size  := parseStr( off, 2 )  ;

    if Trim( size ) <> GDF_UNDEF_FSIZE then
       Fsize := StrToInt( size )
    else
       Fsize := 0;

    if      fname = 'X_COORD' then
      FCoordXSize   := Fsize
    else if fname = 'Y_COORD' then
      FCoordYSize   := Fsize
    else if fname = 'Z_COORD' then
      FCoordZSize   := Fsize
    else if fname = 'NUM_EDGE' then
      FNumEdgeSize  := Fsize
    else if fname = 'ABS_REL' then
      FAbsRelSize  := Fsize
  end;

  procedure TGIS_LayerGDF.addRecDefinition;
  var
    recdef : T_RecDefobj ;
    off    : Integer     ;
    i      : Integer     ;
    rtype  : Integer     ;
    len    : Integer     ;
  begin
    off := StringFirst + 2 ;

    rtype := parseNum( off, 2 ) ;
    if rtype = 44 then begin
      recdef := T_RecDefobj.Create;

      try
        recdef.REC_TYPE  := rtype ;
        recdef.REC_STYPE := parseNum( off, 2 ) ;
        recdef.REC_NAME  := Trim( parseStr( off, 10 ) ) ;

        SetLength( recdef.AFlds, parseNum( off, 2 ) ) ;

        for i := 0 to length( recdef.AFlds ) - 1 do
          recdef.AFlds[ i ] := Trim( parseStr( off, 10 ) ) ;

        lstRecDef.AddObject( IntToStr( recdef.REC_TYPE ), recdef ) ;
      except
        FreeObject( recdef ) ;
      end;
    end
    else if rtype = 18 then begin
      parseNum( off, 2 ) ;
      parseStr( off, 10 ) ;

      len := parseNum( off, 2 ) ;
      if len > 4 then
        HasAttValNew := True ;
    end;

  end;

  procedure TGIS_LayerGDF.setUp ;
  begin
    inherited ;

    FGDFFile := TGIS_BufferedFileStream.Create( Path, TGIS_StreamMode.Read ) ;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;

    prepareLists ;
    Lock ;
    try
      FLineNo      := 0 ;
      FMaxLineNo   := 0 ;
      FMaxPos      := 0 ;
      FGDFEof      := FGDFFile.Position >= FGDFFile.Size ;
      FBreakCnt    := GetBreakCount ;
      FCoordXSize  := 10;
      FCoordYSize  := 10;
      FCoordZSize  := 10;
      FNumEdgeSize := 5;
      FAbsRelSize  := 0;
      FIsFirstCoord:= True;
      FCodePage    := CodePage ;
      HasAttValNew := False;
      gdfFetchLine ;

      prepareAttributes;
      try
        while not FGDFEof do begin
          while not gdfTestLine( GDF_SECHREC   , FLineStr ) and
                not gdfTestLine( GDF_COORDREC  , FLineStr ) and not FGDFEof do
          begin
            if      gdfTestLine( GDF_ALBHDREC  , FLineStr ) then
                    addHeader
            else if gdfTestLine( GDF_FIELDEFREC, FLineStr ) then
                    addFieldDefinition
            else if gdfTestLine( GDF_RECDEFREC , FLineStr ) then
                    addRecDefinition
            else if gdfTestLine( GDF_ATDEFREC  , FLineStr ) then
                    addAttributeDefinition
            else if gdfTestLine( GDF_FEATDEFREC, FLineStr ) then
                    addFeatureDefinition
            else if gdfTestLine( GDF_DATTVALREC, FLineStr ) then
                    addDefaultAttributeValue
            else if gdfTestLine( GDF_ATTVALREC , FLineStr ) then
                    addAttributeValueDefinition ;

            FMaxPos := FGDFFile.Position ;
            gdfFetchLine ;
          end ;

          skipSection;
          while not FGDFEof do begin
            while not gdfTestLine( GDF_POFEREC, FLineStr ) and
                  not gdfTestLine( GDF_LIFEREC, FLineStr ) and
                  not gdfTestLine( GDF_ARFEREC, FLineStr ) and
                  not gdfTestLine( GDF_COFEREC, FLineStr ) and not FGDFEof
            do begin
              if      gdfTestLine( GDF_COORDREC, FLineStr ) then
                      mapCoordinates
              else if gdfTestLine( GDF_NODEREC , FLineStr ) then
                      mapNodes
              else if gdfTestLine( GDF_EDGEREC , FLineStr ) then
                      mapEdges;

              FMaxPos := FGDFFile.Position ;
              gdfFetchLine
            end ;

            while not FGDFEof and not
                ( gdfTestLine( GDF_LAYHREC, FLineStr ) or
                  gdfTestLine( GDF_SECHREC, FLineStr ) ) do
            begin
              if gdfTestLine( GDF_POFEREC  , FLineStr ) then
                      addPoint
              else if gdfTestLine( GDF_LIFEREC  , FLineStr ) then
                      addLine
              else if gdfTestLine( GDF_COFEREC  , FLineStr ) then
                      addComplex
              else if gdfTestLine( GDF_NAMEREC , FLineStr ) then
                      addName
              else if gdfTestLine( GDF_ATTREC  , FLineStr ) then
                      mapAttributes ;

              FMaxPos := FGDFFile.Position ;
              gdfFetchLine ;
            end ;
            mapCoord.Clear;
            mapNode.Clear;
            mapEdge.Clear;
            mapPoint.Clear;
            mapLine.Clear;
          end;
        end ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, FLineNo ) ;
      end ;
    finally
      Unlock ;
      FIsModified := False ;
      FGDFEof     := False ;

      RaiseBusyRelease( Self ) ;

      unPrepareLists;
    end ;

    FFileInfo := 'Geographic Data File ' + FInfoStr ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerGDF.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-GDF', 'Geographic Data Files', TGIS_LayerGDF, '.gdf',
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                 ) ;
  end;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerGDF.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

