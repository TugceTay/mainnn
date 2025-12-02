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
  Encapsulation of a FME file access.

  You can treat this as an sample of how to add native support to other
  GIS file formats.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerFME ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerFME"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.SysUtils,
  System.Variants,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoLayerVector;

type

  /// <summary>
  ///   Layer that can read FME file.
  /// </summary>
  TGIS_LayerFME = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      firstRec : Boolean ;
      cursorShp : array of record
        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse       : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record-by-record access.
        /// </summary>
        currShape      : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating the shape on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currPoint      : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currArc        : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currPolygon    : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currMultipoint : TGIS_ShapeMultiPoint ;

        /// <summary>
        ///   Last accessed shape type from FME file.
        /// </summary>
        currShapeType  : TGIS_ShapeType ;

        /// <summary>
        ///   Shape file encapsulation.
        /// </summary>
        fmeFile        : TObject ;

        /// <summary>
        ///   Last UID in a shape file. If not set then -1.
        /// </summary>
        lastUid        : Integer ;

      end ;
    protected // other protected functions
    private // private methods

      /// <summary>
      ///   Read a shape from the shape file and recreate it.
      /// </summary>
      procedure prepareShape(  const _uid   : Integer ;
                               const _type  : TGIS_ShapeType ;
                               const _cursor   : Integer
                              ) ;
    protected
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         function  getFieldInternal      ( const _uid      : TGIS_Uid;
                                           const _field    : string ;
                                           const _cursor   : Integer
                                         ) : Variant ; override;
         /// <inheritdoc/>
         procedure setUp                 ; override;
      // cursor access function(s)

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
    public
      // constructors

         /// <inheritdoc/>
         constructor Create ; override;

         /// <inheritdoc/>
         destructor  Destroy ; override;

         /// <inheritdoc/>
         procedure RecalcExtent   ; override;

         /// <inheritdoc/>
         function  GetShape  ( const _uid     : TGIS_Uid ;
                               const _cursor  : Integer
                             ) : TGIS_Shape ; override;

         /// <inheritdoc/>
         function  GetLastUid : TGIS_Uid ; override;
    public
  end ;

//##############################################################################
implementation

uses
  System.Math,
  {$IFDEF WINFORMS}
    System.Drawing,
  {$ENDIF}
  System.SyncObjs,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoFMEInt,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoRegistredLayers ;

const
  GIS_FME_FORMAT                  = 'FFS';

  GIS_FME_GEOMETRY                = 'fme_geometry';
  GIS_FME_GEOMETRY_UNDEFINED      = 0;
  GIS_FME_GEOMETRY_POINT          = 1;
  GIS_FME_GEOMETRY_LINE           = 2;
  GIS_FME_GEOMETRY_POLYGON        = 4;
  GIS_FME_GEOMETRY_DONUT          = 8;
  GIS_FME_GEOMETRY_POINTINPOLYGON = 256;
  GIS_FME_GEOMETRY_AGGREGATE      = 512;

  GIS_FME_TRUE                    = 1 ;
  GIS_FME_FALSE                   = 0 ;

  GIS_FME_TYPE                    = 'fme_type' ;
  GIS_FME_TYPE_POINT              = 'fme_point' ;
  GIS_FME_TYPE_ARC                = 'fme_arc' ;
  GIS_FME_TYPE_ELLIPSE            = 'fme_ellipse' ;
  GIS_FME_TYPE_TEXT               = 'fme_text' ;
  GIS_FME_TYPE_LINE               = 'fme_line' ;
  GIS_FME_TYPE_AREA               = 'fme_area' ;
  GIS_FME_TYPE_NO_GEOM            = 'fme_no_geom' ;

  GIS_FME_ATTR_ROTATION           = 'fme_rotation' ;
  GIS_FME_ATTR_PRIMARY_AXIS       = 'fme_primary_axis' ;
  GIS_FME_ATTR_SECONDARY_AXIS     = 'fme_secondary_axis' ;
  GIS_FME_ATTR_START_ANGLE        = 'fme_start_angle' ;
  GIS_FME_ATTR_SWEEP_ANGLE        = 'fme_sweep_angle' ;
  GIS_FME_ATTR_END_ANGLE          = 'fme_end_angle' ;
  GIS_FME_ATTR_TEXT_STRING        = 'fme_text_string';
  GIS_FME_ATTR_TEXT_SIZE          = 'fme_text_size';

  GIS_FME_SEARCH_TYPE             = 'fme_search_type';
  GIS_FME_SEARCH_ENVELOPE         = 'fme_envelope_intersects';
  GIS_FME_SEARCH_ALL_FEATURES     = 'fme_all_features';
  GIS_FME_SEARCH_CLOSEST          = 'fme_closest';

  GIS_FME_FIELD_TYPE_CHAR         = 'fme_char';
  GIS_FME_FIELD_TYPE_DATE         = 'fme_date';
  GIS_FME_FIELD_TYPE_DECIMAL      = 'fme_decimal';
  GIS_FME_FIELD_TYPE_REAL32       = 'fme_real32';
  GIS_FME_FIELD_TYPE_REAL64       = 'fme_real64';
  GIS_FME_FIELD_TYPE_INT16        = 'fme_int16';
  GIS_FME_FIELD_TYPE_INT32        = 'fme_int32';
  GIS_FME_FIELD_TYPE_BOOLEAN      = 'fme_boolean';

type

  // Class that can read FME file.
  TGIS_FMEObjects = class
    private
     { private declarations }
      fmeSession            : IFMEOSession ;
      fmeSessionDir         : IFMEOStringArray;
      fmeReader             : IFMEOReader;
      fmeReaderParms        : IFMEOStringArray;
      fmeSourceDataset      : WideString;
      fmeFeature            : IFMEOFeature;
      fmeSchema             : IFMEOFeature;
      fmeEndOfFile          : Boolean;
    private
      fmePath               : WideString ;
      FCurrShape            : TGIS_Shape ;
      FCurrLayer            : TGIS_LayerVector;
      FCurrFeatureId        : Integer ;
    protected
      // Get current feature type.
      // return   shape type
      function  getCurrentFeatureType : TGIS_ShapeType ;

      // Parse schema definition.
      procedure parseSchema ;

    protected
     { protected declarations }

     // Parse point feature.
     // _feature   FME feature
     // return     True if successful
     function  doPoint           ( const _feature : IFMEOFeature ) : Boolean ;

     // Parse line feature.
     // _feature   FME feature
     // return     True if successful
     function  doLine            ( const _feature : IFMEOFeature ) : Boolean ;

     // Parse polygon feature.
     // _feature   FME feature
     // return     True if successful
     function  doPolygon         ( const _feature : IFMEOFeature ) : Boolean ;

     // Parse donut feature.
     // _feature   FME feature
     // return     True if successful
     function  doDonut           ( const _feature : IFMEOFeature ) : Boolean ;

     // Parse point in polygon feature.
     // _feature   FME feature
     // return     True if successful
     function  doPointInPolygon  ( const _feature : IFMEOFeature ) : Boolean ;

     // Parse aggregate features.
     // _feature   FME feature
     // return     True if successful
     function  doAggregate       ( const _feature : IFMEOFeature ) : Boolean ;

     // Parse arc feature.
     // _feature   FME feature
     // return     True if successful
     function  doArc             ( const _feature : IFMEOFeature ) : Boolean ;

     // Parse ellipse feature.
     // _feature   FME feature
     // return     True if successful
     function  doEllipse         ( const _feature : IFMEOFeature ) : Boolean ;

     // Parse text feature.
     // _feature   FME feature
     // return     True if successful
     function  doText            ( const _feature : IFMEOFeature ) : Boolean ;

     // Draw line shape.
     // _feature   FME feature
     // return     True if successful
     function  drawLine          ( const _feature : IFMEOFeature ) : Boolean ;

     // Draw point feature.
     // _feature   FME feature
     // return     True if successful
     function  drawSimple        ( const _feature : IFMEOFeature ) : Boolean ;

     // Draw polygon shape.
     // _feature   FME feature
     // return     True if successful
     function  drawPolygon       ( const _feature : IFMEOFeature ) : Boolean ;

     // Build feature.
     // _feature   FME feature
     // return     True if successful
     function  doFeature         ( const _feature : IFMEOFeature ) : Boolean ;

    public
      { public declarations }

      // Constructor.
      // _layer vector layer handle
      constructor Create( const _layer : TGIS_LayerVector ) ;

      // Destructor.
      destructor  Destroy ; override;

      // Open FME reader.
      // _type     reader type ( 0 - full, 1 - envelope, 2 - closest )
      // _extent   envelope extent or closest point
      function  OpenReader( const _type   : Integer ;
                            const _extend : TGIS_Extent ;
                            const _first  : Boolean
                           ) : Boolean ;

      // Read next feature.
      function  ReadFeature : Boolean ;

      // Parse FME feature.
      // _shp   current shape
      procedure parseFeature( const _shp : TGIS_Shape );

      // Get dataset bounding box.
      // return   maximum extent
      function  GetBoundingBox : TGIS_Extent ;

      // Get field value.
      // _uid     shape uid
      // _field   shape field name
      // return   field value
      function  GetFieldValue( const _uid   : Integer ;
                               const _field : String
                             ) : Variant ;
    public
     { published declarations }
     property CurrentFeatureType : TGIS_ShapeType read  getCurrentFeatureType ;
     property ReaderEof          : Boolean        read  fmeEndOfFile ;
     property CurrentFeature     : IFMEOFeature   read  fmeFeature ;
     property CurrentShape       : TGIS_Shape     read  FCurrShape
                                                  write FCurrShape ;
  end;

//=============================================================================
// TGis_FeatureGraphics
//=============================================================================

  constructor TGIS_FMEObjects.Create( const _layer : TGIS_LayerVector );
  begin
    inherited Create;

    fmeSession    := CoFMEOSession.Create;
    fmeSessionDir := fmeSession.createStringArray;
    fmeSession.init( fmeSessionDir );

    fmePath     := _layer.Path ;
    FCurrLayer  := _layer;
  end;

  destructor TGIS_FMEObjects.Destroy;
  begin
    fmeSessionDir := nil;
    fmeSession    := nil ;
    fmeReader     := nil ;

    inherited;
  end;

  function TGIS_FMEObjects.OpenReader( const _type   : Integer;
                                       const _extend : TGIS_Extent ;
                                       const _first  : Boolean
                                      ) : Boolean;
  var
    fmeSourceFormat       : WideString;
    fmeSrcUserDirectives  : IFMEOStringArray;
    fmeScope              : IFMEOFeature;
  begin
    Result := False;

    if not Assigned( fmeReader ) then begin
      fmeSrcUserDirectives := fmeSession.createStringArray;
      fmeSourceFormat   := GIS_FME_FORMAT;
      fmeSourceDataset  := fmePath ;
      fmeReader := fmeSession.createReader( fmeSourceFormat, GIS_FME_TRUE,
                                            fmeSrcUserDirectives
                                           );
      fmeReaderParms := fmeSession.createStringArray;
      fmeReader.open( fmeSourceDataset, fmeReaderParms );

      fmeEndOfFile := False ;
      fmeFeature   := fmeSession.createFeature;

      if _first then
        parseSchema;

      Result := True;
    end
    else begin
      fmeScope := fmeSession.createFeature;
      case _type of
        0 : fmeScope.attribute[ GIS_FME_SEARCH_TYPE ] := GIS_FME_SEARCH_ALL_FEATURES;
        1 : fmeScope.attribute[ GIS_FME_SEARCH_TYPE ] := GIS_FME_SEARCH_ENVELOPE;
        2 : fmeScope.attribute[ GIS_FME_SEARCH_TYPE ] := GIS_FME_SEARCH_CLOSEST;
      end;

      fmeScope.addCoordinate( _extend.XMin, _extend.YMin, 0 );
      fmeScope.addCoordinate( _extend.XMax, _extend.YMax, 0 );
      fmeReader.setConstraints( fmeScope );

      fmeEndOfFile := False ;
    end;
  end;

  procedure TGIS_FMEObjects.parseSchema;
  var
    i, size, prec         : Integer ;
    fmeSchemaAttributes   : IFMEOStringArray;
    attrName, attrValue   : WideString;

    function _t( const _val : WideString ) : Boolean ;
    begin
      Result := Pos( _val, attrValue ) > 0 ;
    end;
  begin
    fmeSchema := fmeSession.createFeature;
    fmeReader.readSchema( fmeSchema );

    fmeSchemaAttributes := fmeSession.createStringArray ;
    fmeSchema.getSequencedAttributeList( fmeSchemaAttributes );

    if Assigned( FCurrLayer ) then begin
      FCurrLayer.AddFieldInternal( GIS_FME_GEOMETRY , TGIS_FieldType.String, 1, 0 );
      FCurrLayer.AddFieldInternal( GIS_FME_TYPE     , TGIS_FieldType.String, 1, 0 );
      FCurrLayer.AddFieldInternal( GIS_FME_TYPE_TEXT, TGIS_FieldType.String, 1, 0 );

      for i := 0 to fmeSchemaAttributes.entries - 1 do begin
        attrName  := fmeSchemaAttributes.element[ i ] ;
        attrValue := fmeSchema.attribute[ attrName ] ;

        if      _t( GIS_FME_GEOMETRY           ) then
                continue
        else if _t( GIS_FME_FIELD_TYPE_CHAR    ) then begin
                 size := StrToIntDef(
                           Copy( attrValue,
                                 Pos( '(', attrValue ) + 1,
                                 Length( attrValue )
                                 - Pos( '(', attrValue ) - 1
                               ),
                           1
                         ) ;
                 FCurrLayer.AddFieldInternal( attrName,
                                              TGIS_FieldType.String,
                                              size, 0
                                            ) ;
         end
        else if _t( GIS_FME_FIELD_TYPE_DATE    ) then
                FCurrLayer.AddFieldInternal( attrName, TGIS_FieldType.Date, 1, 0 )
        else if _t( GIS_FME_FIELD_TYPE_DECIMAL ) then begin
                size := StrToIntDef(
                           Copy( attrValue,
                                 Pos( '(', attrValue ) + 1,
                                 -Pos( '(', attrValue )
                                 + Pos(',', attrValue ) - 1
                               ),
                           10
                        ) ;
                prec := StrToIntDef(
                          Copy( attrValue,
                                Pos( ',', attrValue ) + 1,
                                Pos( ')', attrValue )
                                - Pos(',', attrValue ) - 1
                              ),
                          0
                        );
                FCurrLayer.AddFieldInternal( attrName,
                                             TGIS_FieldType.Number,
                                             size, prec
                                           );
        end
        else if _t( GIS_FME_FIELD_TYPE_REAL32  ) then
                FCurrLayer.AddFieldInternal( attrName,
                                             TGIS_FieldType.Float,
                                             10, 0
                                           )
        else if _t( GIS_FME_FIELD_TYPE_REAL64  ) then
                FCurrLayer.AddFieldInternal( attrName,
                                             TGIS_FieldType.Float,
                                             10, 0
                                           )
        else if _t( GIS_FME_FIELD_TYPE_INT16   ) then
                FCurrLayer.AddFieldInternal( attrName,
                                             TGIS_FieldType.Number,
                                             10, 0
                                           )
        else if _t( GIS_FME_FIELD_TYPE_INT32   ) then
                FCurrLayer.AddFieldInternal( attrName,
                                             TGIS_FieldType.Number,
                                             10, 0
                                           )
        else if _t( GIS_FME_FIELD_TYPE_BOOLEAN ) then
                FCurrLayer.AddFieldInternal( attrName,
                                             TGIS_FieldType.Boolean,
                                             1, 0
                                           ) ;
      end ;
    end ;

  end ;

  procedure TGIS_FMEObjects.parseFeature( const _shp : TGIS_Shape );
  begin
    FCurrShape := _shp ;

    if Assigned( FCurrShape ) then begin
      FCurrShape.Lock( TGIS_Lock.Projection );
      FCurrShape.Reset;
      try
        doFeature( fmeFeature ) ;
      finally
        FCurrShape.Unlock;
      end;
    end;
  end;

  function TGIS_FMEObjects.doFeature( const _feature : IFMEOFeature
                                     ) : Boolean;
  begin
    Result := True;
    case _feature.geometryType of
      GIS_FME_GEOMETRY_UNDEFINED      : ;
      GIS_FME_GEOMETRY_POINT          : Result := doPoint( _feature );
      GIS_FME_GEOMETRY_LINE           : Result := doLine( _feature );
      GIS_FME_GEOMETRY_POLYGON        : Result := doPolygon( _feature );
      GIS_FME_GEOMETRY_DONUT          : Result := doDonut( _feature );
      GIS_FME_GEOMETRY_POINTINPOLYGON : Result := doPointInPolygon( _feature );
      GIS_FME_GEOMETRY_AGGREGATE      : Result := doAggregate( _feature )
    else                                Result := False;
    end;
  end;

  function TGIS_FMEObjects.ReadFeature : Boolean ;
  begin
    fmeEndOfFile := ( fmeReader.read( fmeFeature ) = GIS_FME_TRUE ) ;

    Result := not fmeEndOfFile;
  end;

  function TGIS_FMEObjects.getCurrentFeatureType : TGIS_ShapeType;
  var
    fmeType : WideString    ;
  begin
    fmeType := fmeFeature.attribute[ GIS_FME_TYPE ];

    if fmeType = GIS_FME_TYPE_POINT then
      Result := TGIS_ShapeType.Point
    else if fmeType = GIS_FME_TYPE_ARC then
      Result := TGIS_ShapeType.Arc
    else if fmeType = GIS_FME_TYPE_ELLIPSE then
      Result := TGIS_ShapeType.Polygon
    else if fmeType = GIS_FME_TYPE_TEXT then
      Result := TGIS_ShapeType.Point
    else if fmeType = GIS_FME_TYPE_LINE then
      Result := TGIS_ShapeType.Arc
    else if fmeType = GIS_FME_TYPE_AREA then
      Result := TGIS_ShapeType.Polygon
    else if fmeType = GIS_FME_TYPE_NO_GEOM then
      Result := TGIS_ShapeType.Unknown
    else
      Result := TGIS_ShapeType.Unknown ;
  end;

  function TGIS_FMEObjects.GetFieldValue( const _uid    : Integer;
                                          const _field  : String
                                         ) : Variant;
  var
    id : Integer ;
  begin
    Result := Unassigned;

    if FCurrFeatureId = 0 then begin
      OpenReader( 0, GisWholeWorld, True );

      id := 1;
      while not ( fmeReader.read( fmeFeature ) = GIS_FME_TRUE ) do begin
        if id = _uid then begin
          Result := fmeFeature.attribute[ _field ] ;
          FCurrFeatureId := _uid ;
          break;
        end;
        inc( id );
      end;
    end
    else
       Result := fmeFeature.attribute[ _field ] ;
  end;

  function TGIS_FMEObjects.doPoint( const _feature : IFMEOFeature
                                   ) : Boolean;
  var
    fmeType : WideString    ;
    clone   : IFMEOFeature  ;
  begin
    fmeType := _feature.attribute[ GIS_FME_TYPE ];

    if ( fmeType = GIS_FME_TYPE_ARC ) then begin
      clone := fmeSession.createFeature ;
      _feature.clone( clone );

      Result := doArc( clone ) ;
      clone  := nil ;
    end
    else if ( fmeType = GIS_FME_TYPE_ELLIPSE ) then begin
      clone := fmeSession.createFeature ;
      _feature.clone( clone );

      Result := doEllipse( clone ) ;
      clone  := nil ;
    end
    else if ( fmeType = GIS_FME_TYPE_TEXT ) then begin
      Result := doText( _feature );
    end
    else
      Result := drawSimple( _feature );
  end;

  function TGIS_FMEObjects.drawSimple( const _feature : IFMEOFeature
                                      ) : Boolean;
   var
    num : Integer ;
    ptg : TGIS_Point ;
   begin
    FCurrShape.AddPart;

    for num := 0 to _feature.numCoords - 1 do begin
      ptg.X := _feature.xCoordinate[ num ];
      ptg.Y := _feature.yCoordinate[ num ];

      FCurrShape.AddPoint( ptg );
    end;

    Result := True;
  end;

  function TGIS_FMEObjects.doLine( const _feature : IFMEOFeature
                                   ) : Boolean;
  begin
    Result := drawLine( _feature ) ;
  end;

  function TGIS_FMEObjects.drawLine( const _feature : IFMEOFeature
                                     ) : Boolean;
  begin
    Result := drawSimple( _feature ) ;
  end;

  function TGIS_FMEObjects.doPolygon( const _feature : IFMEOFeature
                                      ) : Boolean;
  begin
    Result := drawPolygon( _feature ) ;
  end;

  function TGIS_FMEObjects.drawPolygon( const _feature : IFMEOFeature
                                        ) : Boolean;
  begin
    Result := drawSimple( _feature ) ;
  end;

  function TGIS_FMEObjects.GetBoundingBox : TGIS_Extent;
  var
    ext : TGIS_Extent;
  begin
    Result := GisNoWorld;
    while not ( fmeReader.read( fmeFeature ) = GIS_FME_TRUE ) do begin
      fmeFeature.boundingBox( ext.XMin, ext.XMax, ext.YMin, ext.YMax );
      Result := GisMaxExtent( Result, ext ) ;
    end;

  end;

  function TGIS_FMEObjects.doPointInPolygon( const _feature : IFMEOFeature
                                               ) : Boolean;
  begin
    Result := True;
  end;

  function TGIS_FMEObjects.doAggregate( const _feature : IFMEOFeature
                                        ) : Boolean;
  var
    aggregateParts : IFMEOFeatureVector;
    aggrPart       : IFMEOFeature;
    i              : Integer ;
  begin
    Result := True;

    aggregateParts := fmeSession.createFeatureVector;
    _feature.splitAggregate( aggregateParts, 0 );

    for i := 0 to aggregateParts.entries - 1 do begin
      aggrPart := aggregateParts.element( i );

      Result := doFeature( aggrPart ) ;
    end;
  end;

  function TGIS_FMEObjects.doArc( const _feature : IFMEOFeature
                                  ) : Boolean;
  var
    centerX,
    centerY,
    primaryAxis,
    secondaryAxis,
    rotation,
    startAngle,
    sweepAngle,
    endAngle        : Double;
  begin
    centerX := _feature.xCoordinate[ 0 ] ;
    centerY := _feature.yCoordinate[ 0 ] ;

    primaryAxis   := _feature.real64Attribute[ GIS_FME_ATTR_PRIMARY_AXIS   ] ;
    secondaryAxis := _feature.real64Attribute[ GIS_FME_ATTR_SECONDARY_AXIS ] ;
    rotation      := _feature.real64Attribute[ GIS_FME_ATTR_ROTATION       ] ;
    startAngle    := _feature.real64Attribute[ GIS_FME_ATTR_START_ANGLE    ] ;
    sweepAngle    := _feature.real64Attribute[ GIS_FME_ATTR_SWEEP_ANGLE    ] ;
    endAngle      := sweepAngle + startAngle;

    _feature.convertArcToPoints( centerX, centerY, primaryAxis, secondaryAxis,
                                 0, startAngle, endAngle, rotation
                                );
    Result := doLine( _feature );
  end;

  function TGIS_FMEObjects.doDonut( const _feature : IFMEOFeature
                                   ) : Boolean;
  var
    donutParts : IFMEOFeatureVector;
    donutPart  : IFMEOFeature;
    i          : Integer ;
  begin
    Result := True;
    donutParts := fmeSession.createFeatureVector;
    _feature.getDonutParts( donutParts );

    for i :=  0 to donutParts.entries - 1 do begin
      donutPart := donutParts.element( i );

      Result := doPolygon( donutPart );
    end;
  end;

  function TGIS_FMEObjects.doEllipse( const _feature : IFMEOFeature
                                     ) : Boolean;
  var
    centerX,
    centerY,
    primaryAxis,
    secondaryAxis,
    rotation      : Double;
  begin
    centerX := _feature.xCoordinate[ 0 ] ;
    centerY := _feature.yCoordinate[ 0 ] ;

    primaryAxis   := _feature.real64Attribute[ GIS_FME_ATTR_PRIMARY_AXIS   ] ;
    secondaryAxis := _feature.real64Attribute[ GIS_FME_ATTR_SECONDARY_AXIS ] ;
    rotation      := _feature.real64Attribute[ GIS_FME_ATTR_ROTATION       ] ;

    _feature.convertArcToPoints( centerX, centerY, primaryAxis, secondaryAxis,
                                 0, 0, 360, rotation
                                );
    Result := doPolygon( _feature );
  end;

  function TGIS_FMEObjects.doText( const _feature : IFMEOFeature
                                  ) : Boolean;
  var
    textSize      : Double ;
    textRotation  : Double ;
    textString    : WideString ;
  begin
    textSize      := _feature.real64Attribute[ GIS_FME_ATTR_TEXT_SIZE ] ;
    textRotation  := _feature.real64Attribute[ GIS_FME_ATTR_ROTATION  ] ;
    textString    := _feature.attribute[ GIS_FME_ATTR_TEXT_STRING ] ;

    drawSimple( _feature );

    FCurrShape.SetField( GIS_FME_TYPE_TEXT, textString );
    FCurrShape.Params.Labels.Field  := GIS_FME_TYPE_TEXT ;
    FCurrShape.Params.Labels.Rotate := -DegToRad( textRotation ) ;

    if textSize > 0 then begin
     {$IFDEF WINFORMS}
      FCurrShape.Params.Labels.Font
        := System.Drawing.Font.Create(
             FCurrShape.Params.Labels.Font.Name,
             RoundS( 8 + textSize ),
             System.Drawing.FontStyle.Regular,
             FCurrShape.Params.Labels.Font.&Unit,
             FCurrShape.Params.Labels.Font.GdiCharset,
             FCurrShape.Params.Labels.Font.GdiVerticalFont
           ) ;
     {$ELSE}
      FCurrShape.Params.Labels.FontSize  := RoundS( 8 + textSize );
     {$ENDIF}
    end;

    Result := True;
  end;

//=============================================================================
// TGIS_LayerFME
//=============================================================================

  constructor TGIS_LayerFME.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;
    firstRec := True ;
  end ;

  destructor  TGIS_LayerFME.Destroy ;
  begin

    inherited ;
  end ;

  procedure TGIS_LayerFME.prepareShape( const _uid    : Integer ;
                                        const _type   : TGIS_ShapeType ;
                                        const _cursor : Integer
                                        ) ;
  begin
    case _type of
      TGIS_ShapeType.Point :
         begin
           cursorShp[_cursor].currPoint.Recreate  ( nil, nil, False, _uid, self ) ;
           cursorShp[_cursor].currShape := getEdited( cursorShp[_cursor].currPoint ) ;
         end ;
      TGIS_ShapeType.MultiPoint :
         begin
           cursorShp[_cursor].currMultipoint.Recreate( nil, nil, False, _uid, self ) ;
           cursorShp[_cursor].currShape := getEdited( cursorShp[_cursor].currMultipoint ) ;
         end ;
      TGIS_ShapeType.Arc :
         begin
           cursorShp[_cursor].currArc.Recreate    ( nil, nil, False, _uid, self ) ;
           cursorShp[_cursor].currShape := getEdited( cursorShp[_cursor].currArc ) ;
         end ;
      TGIS_ShapeType.Polygon :
         begin
           cursorShp[_cursor].currPolygon.Recreate( nil, nil, False, _uid, self ) ;
           cursorShp[_cursor].currShape := getEdited( cursorShp[_cursor].currPolygon ) ;
         end ;
      else
         cursorShp[_cursor].currShapeType := TGIS_ShapeType.Unknown ;
    end ;
  end ;

  procedure TGIS_LayerFME.RecalcExtent;
  var
    ex : TGIS_Extent ;
  begin
    if Items.Count > 0 then
      inherited
    else begin
      ex := GisCommonExtent( cursorState[ 0 ].curExtent,
                             GisExtent( -1E37, -1E37, 1E37, 1E37 )
                           ) ;
      TGIS_FMEObjects( cursorShp[0].fmeFile ).OpenReader( 0, ex, firstRec ) ;

      if Assigned( cursorShp[0].fmeFile ) then
        Extent := TGIS_FMEObjects( cursorShp[0].fmeFile ).GetBoundingBox ;
    end;
  end;

  function TGIS_LayerFME.getFieldInternal( const _uid      : TGIS_Uid;
                                           const _field    : string ;
                                           const _cursor   : Integer
                                         ) : Variant ;
  begin
    lockThread ;
    try
      Result := TGIS_FMEObjects( cursorShp[_cursor].fmeFile ).GetFieldValue( _uid, _field ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerFME.setUp ;
  begin
    inherited ;

    try
      cursorShp[0].fmeFile := TGIS_FMEObjects.Create( self );
    except
      on e : Exception do
        raise EGIS_Exception.Create( GIS_RS_ERR_TXTFILESTRUCT + '. ' +
                                     e.Message, Path, 0
                                    ) ;
    end;

    FFileInfo := 'FME Feature Store ( FFS )' ;
  end ;

  function  TGIS_LayerFME.cursorOpen   :  Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length( cursorShp )  then
        SetLength( cursorShp, Result + 1 ) ;
      cursorShp[Result].curInUse := True ;

      cursorShp[Result].currPoint      := TGIS_ShapePoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorShp[Result].currMultipoint := TGIS_ShapeMultiPoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorShp[Result].currArc        := TGIS_ShapeArc.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorShp[Result].currPolygon    := TGIS_ShapePolygon.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorShp[Result].lastUid        := -1 ;
      if Result > 0 then
        cursorShp[Result].fmeFile := TGIS_FMEObjects.Create( self );
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerFME.cursorClose  ( const _cursor      : Integer
                                       ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorShp[_cursor].curInUse := False ;
      cursorShp[_cursor].currPoint.Free ;
      cursorShp[_cursor].currMultipoint.Free ;
      cursorShp[_cursor].currArc.Free ;
      cursorShp[_cursor].currPolygon.Free ;
      cursorShp[_cursor].fmeFile.Free ;

      // truncate cursorState at the tail;
      for i := Length( cursorShp ) - 1 downto 0 do begin
        if not cursorShp[i].curInUse then begin
          SetLength( cursorShp, i ) ;
        end
        else
          break ;
      end ;

      inherited cursorClose( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerFME.cursorFirst(
    const _cursor      : Integer     ;
    const _viewerCS    : Boolean     ;
    const _extent      : TGIS_Extent ;
    const _query       : String      ;
    const _shape       : TGIS_Shape  ;
    const _de9im       : String      ;
    const _skipDeleted : Boolean
  ) ;
  var
    ex : TGIS_Extent ;
  begin
    lockThread ;
    try
      cursorShp[_cursor].currShape := nil ;

      if GisIsNoWorld( _extent ) then
        exit ;
        
      inherited cursorFirstInternal(
                  _cursor, _viewerCS,
                  _extent, _query, _shape, _de9im, _skipDeleted
                ) ;

      ex := GisCommonExtent( cursorState[ _cursor ].curExtent,
                             GisExtent( -1E37, -1E37, 1E37, 1E37 )
                           ) ;

      TGIS_FMEObjects( cursorShp[_cursor].fmeFile ).FCurrFeatureId := 0;
      TGIS_FMEObjects( cursorShp[_cursor].fmeFile ).OpenReader( 0, ex, firstRec ) ;
      firstRec := False ;
      cursorNext( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerFME.cursorNext( const _cursor : Integer ) ;
  var
    uid     : Integer ;
    ihglass : Integer ;
  begin
    lockThread ;
    try
      ihglass := 0 ;
      while not TGIS_FMEObjects( cursorShp[_cursor].fmeFile ).ReaderEof do begin
        Inc( ihglass ) ;
        if ihglass mod GIS_PROGRESS_TRESHOLD = 0 then begin
          if HourglassShake then begin
            cursorShp[ _cursor ].currShape := nil ;
            break ;
          end;
        end ;

        if Assigned( cursorShp[_cursor].currShape ) then
          uid := cursorShp[_cursor].currShape.Uid + 1
        else
          uid := 1 ;

        cursorShp[_cursor].currShape := nil ;

        if Assigned( cursorShp[_cursor].fmeFile ) then begin
          if TGIS_FMEObjects( cursorShp[_cursor].fmeFile ).ReadFeature then
          begin
            prepareShape(
              uid,
              TGIS_FMEObjects( cursorShp[_cursor].fmeFile ).CurrentFeatureType,
              _cursor
            ) ;
            TGIS_FMEObjects(
              cursorShp[_cursor].fmeFile
            ).parseFeature( cursorShp[_cursor].currShape );
          end;
        end;

         // using an xxxInternalVersion may be a bit too secure
         // but better is to be too restrictive than too lose
         if not inherited cursorEofInternal( _cursor ) then begin
           cursorShp[_cursor].currShape
             := inherited cursorShapeInternal( _cursor ) ;
           inherited cursorNextInternal( _cursor ) ;
         end ;
         if cursorShp[_cursor].currShape = nil then exit ;

         if cursorState[_cursor].curSkipDeleted and
            cursorShp[_cursor].currShape.IsDeleted then
         begin
           continue ;
         end ;

         if not isInScope( cursorShp[_cursor].currShape, _cursor ) then
           continue
         else
           exit ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerFME.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorShp[_cursor].currShape = nil ;
  end ;

  function TGIS_LayerFME.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if Assigned( cursorShp[_cursor].currShape ) then
       Result := cursorShp[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerFME.GetShape(
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
      if not Assigned( cursorShp[_cursor].fmeFile ) then exit ;

      // is it a current shape
      if ( cursorShp[_cursor].currShape     <> nil ) and
         ( cursorShp[_cursor].currShape.Uid = _uid ) then
      begin
        Result := cursorShp[_cursor].currShape ;
        exit ;
      end ;

      // if no index file, traverse normally
      cursorFirst( _cursor, False,
                   Viewer.Ref.Extent, '', nil, '', True
                 ) ;

      while not not TGIS_FMEObjects( cursorShp[_cursor].fmeFile ).ReaderEof do
      begin
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

  function TGIS_LayerFME.GetLastUid : TGIS_Uid ;
  var
    shp       : TGIS_Shape ;
    old_scope : String     ;
  begin
    lockThread ;
    try
      old_scope := Scope ;
      try
        if cursorShp[0].lastUid < 0 then begin
          shp := nil ;
          cursorFirst( 0, False,
                       GisWholeWorld, '', nil, '', True
                     ) ;

          while not cursorEof(0) do begin // iterate all shapes
            shp := cursorShape(0) ;
            cursorNext(0) ;
          end ;

          if Assigned( shp ) then
            cursorShp[0].lastUid := shp.Uid
          else
            cursorShp[0].lastUid := 0 ;
        end ;

        Result := cursorShp[0].lastUid ;
      finally
        Scope := old_scope ;
      end;
    finally
      unlockThread ;
    end ;
  end ;

initialization
  RegisterLayer( 'DK-FME', 'FME Feature Store', TGIS_LayerFME, '.ffs',
                 TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Local,
                 [ TGIS_RegisteredOperationType.Read ],
                  False
                ) ;
//==================================== END =====================================
end.

