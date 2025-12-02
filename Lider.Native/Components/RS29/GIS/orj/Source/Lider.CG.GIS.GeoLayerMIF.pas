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
  Encapsulation of a MIF/MID file access.
}

{$IFDEF DCC}
  unit GisLayerMIF ;
  {$HPPEMIT '#pragma link "GisLayerMIF"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,

    GisTypes,

    GisCsSystems,
    GisLayerVector,
    GisStreams;
{$ENDIF}
{$IFDEF CLR}
  uses
    System.Drawing,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerMif = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Layer which can read a MIF/MID file.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     - Supported shape types: TGIS_ShapeType.Point, TGIS_ShapeType.Arc,
  ///     TGIS_ShapeType.Polygon.
  ///   </para>
  ///   <para>
  ///     - Upon importing shape of type TGIS_ShapeType.MultiPoint the shape
  ///     will be converted to a set of TGIS_ShapeType.Point shapes. <br />
  ///   </para>
  /// </remarks>
  TGIS_LayerMIF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private

        /// <summary>
        ///   Shapes file.
        /// </summary>
        mifFile   : TGIS_BufferedFileStream ;

        /// <summary>
        ///   Shapes file line.
        /// </summary>
        mifLineNo : Integer ;

        /// <summary>
        ///   Data file.
        /// </summary>
        midFile   : TGIS_BufferedFileStream ;

        /// <summary>
        ///   Data file line.
        /// </summary>
        midLineNo : Integer ;

        /// <summary>
        ///   Delimiter in mif file.
        /// </summary>
        delimMif  : String ;

        /// <summary>
        ///   Current shape.
        /// </summary>
        currShape : TGIS_Shape ;
    protected // property access functions

      function fget_Path      : String ; override;
      function fget_PathMID   : String ; virtual;
    protected // other protected functions

      /// <summary>
      ///   Get MID file extension.
      /// </summary>
      /// <returns>
      ///    extension text
      /// </returns>
      function getFileExtMID  : String ; virtual;
    private

      /// <summary>
      ///   Read line from MIF file.
      /// </summary>
      function  mifReadLine   : String ;

      /// <summary>
      ///   Read line from MID file.
      /// </summary>
      function  midReadLine   : String ;

      /// <summary>
      ///   Add new field contents.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure addRecord     ( const _shp       : TGIS_Shape
                              ) ;

      /// <summary>
      ///   Create projection.
      /// </summary>
      /// <param name="_param">
      ///   String with projection parameters
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doCoordSys    ( const _param     : String
                              ) ;

      /// <summary>
      ///   Create data fields.
      /// </summary>
      /// <param name="_param">
      ///   String with number of fields
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doColumns     ( const _param     : String
                              ) ;

      /// <summary>
      ///   Create a multi-part TGIS_ShapePolygon.
      /// </summary>
      /// <param name="_param">
      ///   String with number of parts
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doRegion      ( const _param     : String
                              ) ;

      /// <summary>
      ///   Create a rectangle TGIS_ShapePolygon.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doRectangle   ( const _ax        : String            ;
                                const _ay        : String            ;
                                const _bx        : String            ;
                                const _by        : String
                              ) ;

      /// <summary>
      ///   Create a multi-element TGIS_ShapeArc.
      /// </summary>
      /// <param name="_param1">
      ///   String with number of points or "Multiple" phrase
      /// </param>
      /// <param name="_param2">
      ///   String with number of points if _param1 is "Multiple"
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doPLine       ( const _param1    : String            ;
                                const _param2    : String
                              ) ;

      /// <summary>
      ///   Create a single-part for a TGIS_ShapeArc element.
      /// </summary>
      /// <param name="_shp">
      ///   shape for which the part will be added
      /// </param>
      /// <param name="_point_no">
      ///   number of points to be added
      /// </param>
      procedure doPLineElement( const _shp       : TGIS_Shape        ;
                                const _point_no  : Integer
                              ) ;

      /// <summary>
      ///   Create a single-element TGIS_ShapeArc.
      /// </summary>
      /// <param name="_ax">
      ///   begin longitude
      /// </param>
      /// <param name="_ay">
      ///   begin latitude
      /// </param>
      /// <param name="_bx">
      ///   end longitude
      /// </param>
      /// <param name="_by">
      ///   end latitude
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doLine        ( const _ax        : String            ;
                                const _ay        : String            ;
                                const _bx        : String            ;
                                const _by        : String
                              ) ;

      /// <summary>
      ///   Create TGIS_ShapePoint text.
      /// </summary>
      /// <param name="_param">
      ///   text String
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doText        ( const _param     : String
                              ) ;

      /// <summary>
      ///   Create an arc TGIS_ShapeArc.
      /// </summary>
      /// <param name="_param">
      ///   arc String
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doArc         ( const _param     : String
                              ) ;

      /// <summary>
      ///   Create TGIS_ShapePoint.
      /// </summary>
      /// <param name="_x">
      ///   longitude
      /// </param>
      /// <param name="_y">
      ///   latitude
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doPoint       ( const _x         : String            ;
                                const _y         : String
                              ) ;

      /// <summary>
      ///   Create an ellipse TGIS_ShapeArc.
      /// </summary>
      /// <param name="_ax">
      ///   begin longitude
      /// </param>
      /// <param name="_ay">
      ///   begin latitude
      /// </param>
      /// <param name="_bx">
      ///   end longitude
      /// </param>
      /// <param name="_by">
      ///   end latitude
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doEllipse     ( const _ax        : String            ;
                                const _ay        : String            ;
                                const _bx        : String            ;
                                const _by        : String
                              ) ;

      /// <summary>
      ///   Write MIF header.
      /// </summary>
      /// <param name="_strm">
      ///   stream to be written
      /// </param>
      /// <param name="_ex">
      ///   extent of file
      /// </param>
      procedure writeHeader   ( const _strm      : TGIS_Stream       ;
                                const _ex        : TGIS_Extent       ;
                                const _cs        : TGIS_CSCoordinateSystem
                              ) ;

      /// <summary>
      ///   Write shape geometry and data to the output stream.
      /// </summary>
      /// <param name="_mifstrm">
      ///   MIF stream to be written
      /// </param>
      /// <param name="_mifshp">
      ///   shape geometry (could be truncated)
      /// </param>
      /// <param name="_midstrm">
      ///   MID stream to be written
      /// </param>
      procedure writeShape    ( const _mifstrm   : TGIS_Stream       ;
                                const _mifshp    : TGIS_Shape        ;
                                const _midstrm   : TGIS_Stream       ;
                                const _midshp    : TGIS_Shape
                              ) ;

      /// <summary>
      ///   Parse color params.
      /// </summary>
      /// <param name="_param">
      ///   String with number of fields
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doColor       ( const _param     : String            ;
                                const _type      : Integer
                              );

      /// <summary>
      ///   Parse angle params.
      /// </summary>
      /// <param name="_param">
      ///   angle in degrees
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doAngle       ( const _param     : String
                              ) ;

      /// <summary>
      ///   Parse justify params.
      /// </summary>
      /// <param name="_param">
      ///   style
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doJustify     ( const _param     : String
                              ) ;

      /// <summary>
      ///   Parse font params.
      /// </summary>
      /// <param name="_param">
      ///   font params
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doFont        ( const _param     : String
                              ) ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure setUp      ; override;
    protected
      // destructor

         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create ; override;
      // new layer builder

         /// <inheritdoc/>
         procedure Build      ( const _path      : String            ;
                                const _extent    : TGIS_Extent       ;
                                const _type      : TGIS_ShapeType    ;
                                const _dim       : TGIS_DimensionType
                              ) ; override;

         /// <inheritdoc/>
         procedure ImportLayerEx( const _layer     : TGIS_LayerVector  ;
                                const _extent    : TGIS_Extent       ;
                                const _type      : TGIS_ShapeType    ;
                                const _scope     : String            ;
                                const _shape     : TGIS_Shape       ;
                                const _de9im     : String           ;
                                const _truncated : Boolean
                              ) ; override;

         /// <inheritdoc/>
         procedure SaveData   ; override;
    public

        /// <summary>
        ///   Path to MIF file.
        /// </summary>
        property PathMIF : String read fget_Path    ;

        /// <summary>
        ///   Path to MID file.
        /// </summary>
        property PathMID : String read fget_PathMID ;
  end ;

//##############################################################################
implementation

{$IFNDEF OXYGENE}
  uses
    System.Math,
    System.StrUtils,
    {$IFDEF LEVEL_XE3_RTL}
      System.UITypes,
    {$ENDIF}
    GisParams,
    GisClasses,
    GisRtl,
    GisFunctions,
    GisInternals,
    GisResource,
    GisTypesUI,
    GisRegistredLayers,
    GisCsMapInfo ;
{$ENDIF}

const
  // MIF file strings
     MIF_EXT_MID    = '.mid'       ;
     MIF_DELIMITER  = 'Delimiter'  ;
     MIF_REGION     = 'Region'     ;
     MIF_RECTANGLE  = 'Rect'       ;
     MIF_ROUNDRECT  = 'RoundRect'  ;
     MIF_PLINE      = 'PLine'      ;
     MIF_LINE       = 'Line'       ;
     MIF_ARC        = 'Arc'        ;
     MIF_ELLIPSE    = 'Ellipse'    ;
     MIF_POINT      = 'Point'      ;
     MIF_TEXT       = 'Text'       ;
     MIF_FONT       = 'Font'       ;
     MIF_JUSTIFY    = 'Justify'    ;
     MIF_MULTIPLE   = 'Multiple'   ;
     MIF_CENTER     = 'Center'     ;
     MIF_LEFT       = 'Left'       ;
     MIF_RIGHT      = 'Right'      ;
     MIF_COLUMNS    = 'Columns'    ;
     MIF_DECIMAL    = 'Decimal'    ;
     MIF_LOGICAL    = 'Logical'    ;
     MIF_SMALLINT   = 'Smallint'   ;
     MIF_INTEGER    = 'Integer'    ;
     MIF_FLOAT      = 'Float'      ;
     MIF_CHAR       = 'Char'       ;
     MIF_DATE       = 'Date'       ;
     MIF_BRUSH      = 'BRUSH'      ;
     MIF_PEN        = 'PEN'        ;
     MIF_SYMBOL     = 'SYMBOL'     ;
     MIF_ANGLE      = 'Angle'      ;
     MIF_COORDSYS   = 'CoordSys'   ;
     MIF_EARTH      = 'Earth'      ;
     MIF_NONEARTH   = 'NonEarth'   ;
     MIF_PROJECTION = 'Projection' ;
     MIF_UNITS      = 'Units'      ;
     MIF_BOUNDS     = 'Bounds'     ;
  // field names
     MIF_FLD_LINE   = 'MIF_LINE'   ;
     MIF_FLD_SYMBOL = 'MIF_SYMBOL' ;
     MIF_FLD_LABEL  = 'MIF_LABEL'  ;

//=============================================================================
// TGIS_LayerMIF
//=============================================================================

  constructor TGIS_LayerMIF.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory  ,
                             TGIS_LayerSubType.Exportable
                            ] ;
    SupportsAutoStyle := False ;
  end ;

  procedure TGIS_LayerMIF.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_LayerMIF.mifReadLine : String ;
  var
    abort : Boolean ;
  begin
    Result := mifFile.ReadLine ;
    inc( mifLineNo ) ;
    if mifLineNo mod 100 = 1 then
      abort := RaiseBusyShake( Self, mifFile.Position, mifFile.Size ) ;
  end ;

  function TGIS_LayerMIF.midReadLine : String ;
  begin
    Result := midFile.ReadLine ;
    inc( midLineNo ) ;
  end ;

  function TGIS_LayerMIF.fget_Path : String ;
  begin
    Result := inherited fget_Path;
  end ;

  function TGIS_LayerMIF.fget_PathMID : String ;
  begin
    Result := GetPathNoExt(Path) + getFileExtMID ;
  end ;

  function TGIS_LayerMIF.getFileExtMID : String ;
  begin
    Result := MIF_EXT_MID ;
  end ;

  procedure TGIS_LayerMIF.addRecord(
    const _shp : TGIS_Shape
  ) ;
  var
    i     : Integer        ;
    tkn   : TGIS_Tokenizer ;
    line  : String         ;
    tmp   : String         ;
    dat   : TDateTime      ;
    flg   : TReplaceFlags  ;
  begin
    if not assigned( midFile ) then exit ;
    try
      if Items.Count = 1 then
        Extent := _shp.Extent ;

      if midFile.Eof then exit ;

      line := midReadLine ;

      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.ExecuteEx( line, delimMif[StringFirst], '"' ) ;
        for i:= 0 to tkn.Result.Count - 1 do begin
          try
            case FieldInfo(i).FieldType of
              TGIS_FieldType.Number  ,
              TGIS_FieldType.Float   ,
              TGIS_FieldType.Boolean :
                begin
                  if ( IsStringEmpty( tkn.Result[i] )   ) or
                     ( tkn.Result[i][StringFirst] = '*' )
                  then
                    _shp.SetField( FieldInfo( i ).NewName, NullVar )
                  else begin
                    _shp.SetField( FieldInfo( i ).NewName, tkn.Result[i] ) ;
                  end;
                end;
              TGIS_FieldType.Date :
                begin
                  if not IsStringEmpty( tkn.Result[i] ) then begin
                    dat := EncodeDate( StrToInt( Copy( tkn.Result[i],
                                                       StringFirst, 4 ) ),
                                       StrToInt( Copy( tkn.Result[i],
                                                       4+StringFirst, 2 ) ),
                                       StrToInt( Copy( tkn.Result[i],
                                                       6+StringFirst, 2 ) )
                                     ) ;
                    _shp.SetField( FieldInfo( i ).NewName, dat ) ;
                  end
                  else
                    _shp.SetField( FieldInfo( i ).NewName, NullVar ) ;
                end;

              else
                begin
                  {$IFDEF OXYGENE}
                    flg := [ TReplaceFlag.rfReplaceAll ] ;
                  {$ELSE}
                    flg := [ rfReplaceAll ] ;
                  {$ENDIF}
                  tmp := StringReplace( tkn.Result[i], '\n', #13, flg ) ;
                  if IsStringEmpty( tmp ) then
                    _shp.SetField( FieldInfo( i ).NewName, NullVar )
                  else begin
                    _shp.SetField( FieldInfo( i ).NewName, tmp )
                  end;
                end;
            end;
          except
            _shp.SetField( FieldInfo( i ).NewName, NullVar ) ;
          end;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),
                                   PathMID, midLineNo ) ;
    end ;
  end ;

  procedure TGIS_LayerMIF.doAngle( const _param : String ) ;
  begin
    if not assigned( currShape ) then Exit;

    try
      currShape.Params.Labels.Rotate := -DegToRad( DotStrToFloat( _param ) ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),
                                   PathMIF, mifLineNo ) ;
    end ;
  end;

  procedure TGIS_LayerMIF.doJustify( const _param : String ) ;

    function _t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( _param, _val ) = 0 ;
    end ;

  begin
    if not assigned( currShape ) then Exit;

    try
      if      _t( MIF_CENTER ) then
        currShape.Params.Labels.Alignment :=
          TGIS_LabelAlignment.Center
      else if _t( MIF_LEFT   ) then
        currShape.Params.Labels.Alignment :=
          TGIS_LabelAlignment.LeftJustify
      else if _t( MIF_RIGHT  ) then
        currShape.Params.Labels.Alignment :=
          TGIS_LabelAlignment.RightJustify
      else
        currShape.Params.Labels.Alignment :=
          TGIS_LabelAlignment.Single ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),
                                   PathMIF, mifLineNo ) ;
    end ;
  end;

  procedure TGIS_LayerMIF.doFont( const _param : String ) ;
  var
    tmp     : String ;
    fName   : String ;
    fStyle  : Integer ;
    fSize   : Integer ;
    fFColor : Integer ;
    fBColor : Integer ;
    tkn     : TGIS_Tokenizer ;
  begin
    if not assigned( currShape ) then Exit;

    fName   := '';
    fSize   := 8 ;
    fFColor := 0 ;
    fBColor := 0 ;
    fStyle  := 0 ;
    try
      tmp := Trim( _param ) ;
      tmp := Copy( tmp, 6+StringFirst, length( tmp ) - 7 ) ;

      tkn := TGIS_Tokenizer.Create;
      try
        tkn.ExecuteEx( tmp, ',' );
        if tkn.Result.Count > 0 then fName   := tkn.Result[0];
        if tkn.Result.Count > 1 then fStyle  := StrToInt( tkn.Result[1] ) ;
        if tkn.Result.Count > 2 then fSize   := StrToInt( tkn.Result[2] ) ;
        if tkn.Result.Count > 3 then fFColor := StrToInt( tkn.Result[3] ) ;
        if tkn.Result.Count > 4 then fBColor := StrToInt( tkn.Result[4] ) ;
      finally
        FreeObject( tkn ) ;
      end;

      currShape.Params.Labels.FontName   := fName ;
      if fSize > 0 then
        currShape.Params.Labels.FontSize := fSize * 20 ;
      currShape.Params.Labels.FontColor  := TGIS_Color.FromRGB( fFColor ) ;
      if fBColor <> 0 then
        currShape.Params.Labels.Color     := TGIS_Color.FromRGB( fFColor )
      else
        currShape.Params.Labels.Color     := TGIS_Color.FromRGB( fFColor );


      case fStyle of
        1 : currShape.Params.Labels.FontStyle := GisGetFontStyle( TGIS_FontStyle.Bold   ) ;
        2 : currShape.Params.Labels.FontStyle := GisGetFontStyle( TGIS_FontStyle.Italic ) ;
        4 : currShape.Params.Labels.FontStyle := GisGetFontStyle( TGIS_FontStyle.Underline ) ;
      end;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), PathMIF, mifLineNo ) ;
    end ;
  end;

  procedure TGIS_LayerMIF.doColor( const _param : String ;
                                   const _type  : Integer
                                  );
  var
    flg   : TReplaceFlags  ;
    tmp   : String ;
    tkn   : TGIS_Tokenizer;
    cl    : Integer ;
    wht   : Integer ;
    sz    : Integer ;
    ptr   : Integer ;
    st    : Integer ;
    sym   : Integer ;
  begin
    if not assigned( currShape ) then Exit;

    {$IFDEF OXYGENE}
      flg := [ TReplaceFlag.rfReplaceAll ] ;
    {$ELSE}
      flg := [ rfReplaceAll ] ;
    {$ENDIF}
    tmp := StringReplace( _param, ' ', '', flg ) ;

    if _type = 0 then
      tmp := Copy( tmp, 6 + StringFirst, length( tmp ) - 7 )
    else if _type = 1 then
      tmp := Copy( tmp, 4 + StringFirst, length( tmp ) - 5 )
    else
      tmp := Copy( tmp, 7 + StringFirst, length( tmp ) - 8 );

    ptr := 0;
    wht := 1;
    sz  := 1;
    st  := 2;
    sym := 0;

    tkn := TGIS_Tokenizer.Create;
    try
      tkn.ExecuteEx( tmp, ',' );

      if ( _type = 0 ) then begin
        cl  := StrToIntDef( tkn.Result[ 1 ], 0 ) ;
        ptr := StrToIntDef( tkn.Result[ 0 ], 0 ) ;
      end
      else if ( _type = 2 ) then begin
        cl  := StrToIntDef( tkn.Result[ 1 ], 0 ) ;
        sz  := StrToIntDef( tkn.Result[ 2 ], 0 ) ;
        sym := StrToIntDef( tkn.Result[ 0 ], 0 ) ;
      end
      else begin
        cl  := StrToIntDef( tkn.Result[ 2 ], 0 ) ;
        wht := StrToIntDef( tkn.Result[ 0 ], 0 ) ;
        st  := StrToIntDef( tkn.Result[ 1 ], 0 ) ;
      end;

      if assigned( Viewer ) then
        currShape.Params.Area.Color := Viewer.Ref.Color ;

      if _type = 0 then begin
        currShape.Params.Area.Color         := TGIS_Color.FromRGB( cl ) ;
        case ptr of
          1 : currShape.Params.Area.Pattern := TGIS_BrushStyle.Clear      ;
          2 : currShape.Params.Area.Pattern := TGIS_BrushStyle.Solid      ;
          3 : currShape.Params.Area.Pattern := TGIS_BrushStyle.Horizontal ;
          4 : currShape.Params.Area.Pattern := TGIS_BrushStyle.Vertical   ;
          5 : currShape.Params.Area.Pattern := TGIS_BrushStyle.FDiagonal  ;
          6 : currShape.Params.Area.Pattern := TGIS_BrushStyle.BDiagonal  ;
          7 : currShape.Params.Area.Pattern := TGIS_BrushStyle.Cross      ;
          8 : currShape.Params.Area.Pattern := TGIS_BrushStyle.DiagCross  ;
        end;
      end
      else if _type = 1 then begin
        currShape.Params.Line.Color := TGIS_Color.FromRGB( cl ) ;
        currShape.Params.Area.OutlineColor := TGIS_Color.FromRGB( cl ) ;
        if wht >= 11 then
          wht := Max( 1, RoundS( ( ( wht - 10.0 ) / 10.0 ) * 72.0 / 1440.0 ) )
        else
          wht := -wht ;

        currShape.Params.Line.Width := wht;

        case st of
          2   : currShape.Params.Line.Style := TGIS_PenStyle.Solid      ;
          3,
          4,
          10  : currShape.Params.Line.Style := TGIS_PenStyle.Dot        ;
          5,
          6,
          7,
          8,
          9,
          11,
          12,
          13  : currShape.Params.Line.Style := TGIS_PenStyle.Dash       ;
          14,
          15,
          23,
          25  : currShape.Params.Line.Style := TGIS_PenStyle.DashDot    ;
          20,
          21,
          24  : currShape.Params.Line.Style := TGIS_PenStyle.DashDotDot ;
          193 : begin
                  currShape.Params.Line.Width        := 30 ;
                  currShape.Params.Line.OutlineWidth := 1  ;
                end ;
        end;

        currShape.SetField( MIF_FLD_LINE  , st  ) ;
      end
      else begin
        currShape.Params.Marker.Color := TGIS_Color.FromRGB( cl ) ;
        currShape.Params.Marker.Size  := sz*20;
        currShape.SetField( MIF_FLD_SYMBOL, sym ) ;
      end;
    finally
      FreeObject( tkn ) ;
    end;
  end;

  procedure TGIS_LayerMIF.doColumns( const _param : String ) ;
  var
    i        : Integer ;
    tkn      : TGIS_Tokenizer ;
    line     : String  ;
    field_no : Integer ;
    ftype    : String  ;
    fname    : String  ;
    width    : Integer ;
    decimal  : Integer ;

    function _t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( ftype, _val ) = 0 ;
    end ;

    // remove _DUMMY field if exists
    procedure remove_dummy_field ;
    var
      id   : Integer ;
      fld : TGIS_FieldInfo ;
    begin
      id := FindField( GIS_FIELD_DUMMY ) ;
      if id >= 0 then begin
        fld := FieldInfo( id ) ;
        fld.Deleted := True ;
      end
    end;

  begin
    try
      field_no := StrToInt( _param ) ;

      tkn := TGIS_Tokenizer.Create ;
      try
        for i:=1 to field_no do begin
          if mifFile.Eof then break ;

          line := mifReadLine ;
          tkn.Execute( line, [' ', #9, '(',')',','] ) ;

          fname   := '' ;
          width   := 0  ;
          decimal := 0  ;
          if tkn.Result.Count = 0 then continue ;
          if tkn.Result.Count > 0 then fname   := tkn.Result[0] ;
          if tkn.Result.Count > 1 then ftype   := tkn.Result[1] ;
          if tkn.Result.Count > 2 then width   := StrToInt(tkn.Result[2]) ;
          if tkn.Result.Count > 3 then decimal := StrToInt(tkn.Result[3]) ;

          if      _t( MIF_DECIMAL  ) then
            AddFieldInternal( fname, TGIS_FieldType.Number , width, decimal )
          else if _t( MIF_LOGICAL  ) then
            AddFieldInternal( fname, TGIS_FieldType.Boolean, 1    , 0       )
          else if _t( MIF_SMALLINT ) then
            AddFieldInternal( fname, TGIS_FieldType.Number , 4    , 0       )
          else if _t( MIF_INTEGER  ) then
            AddFieldInternal( fname, TGIS_FieldType.Number , 10   , 0       )
          else if _t( MIF_FLOAT    ) then
            AddFieldInternal( fname, TGIS_FieldType.Float  , 20   , 5       )
          else if _t( MIF_CHAR     ) then
            AddFieldInternal( fname, TGIS_FieldType.String , width, 0       )
          else if _t( MIF_DATE     ) then
            AddFieldInternal( fname, TGIS_FieldType.Date   , 8    , 0       ) ;

        end ;

        if FindField( MIF_FLD_LINE ) < 0 then
          AddFieldInternal( MIF_FLD_LINE  , TGIS_FieldType.Number, 5, 0 ) ;

        if FindField( MIF_FLD_SYMBOL ) < 0 then
          AddFieldInternal( MIF_FLD_SYMBOL, TGIS_FieldType.Number, 5, 0 ) ;

        if FindField( MIF_FLD_LABEL ) < 0 then
          AddFieldInternal( MIF_FLD_LABEL, TGIS_FieldType.String, 1, 0 ) ;

        remove_dummy_field ;
      finally
        FreeObject( tkn ) ;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), PathMIF, mifLineNo ) ;
    end ;
  end ;

  procedure TGIS_LayerMIF.doCoordSys( const _param : String ) ;
  var
    param  : String ;
    line   : String ;
    oldPos : Int64 ;
  begin
    param := _param ;
    // check if we have all parameters or need to read more lines
    if Pos( 'Bounds', _param ) < StringFirst then begin
      while not mifFile.Eof do begin
        oldPos := mifFile.Position ;
        line   := mifReadLine ;

        if Pos( 'COLUMNS', UpperCase( Trim(line) ) ) = StringFirst then begin
          mifFile.Position := oldPos ;
          break ;
        end ;
        param := param + line ;
      end ;
    end ;

    try
      CS := TGIS_CSFactoryMapInfo.BuildCs( param ) ;
    except
      CS := CSUnknownCoordinateSystem ;
      raise ;
    end;
  end;

  procedure TGIS_LayerMIF.doRectangle( const _ax, _ay, _bx, _by : String ) ;
  var
    shp : TGIS_Shape ;
  begin
    try
      shp := CreateShape( TGIS_ShapeType.Polygon ) ;
      try
        shp.Lock( TGIS_Lock.Projection ) ;
        shp.AddPart ;
        shp.AddPoint( GisPoint( DotStrToFloat( _ax ), DotStrToFloat( _ay ) ) ) ;
        shp.AddPoint( GisPoint( DotStrToFloat( _bx ), DotStrToFloat( _ay ) ) ) ;
        shp.AddPoint( GisPoint( DotStrToFloat( _bx ), DotStrToFloat( _by ) ) ) ;
        shp.AddPoint( GisPoint( DotStrToFloat( _ax ), DotStrToFloat( _by ) ) ) ;
      finally
        shp.Unlock ;
        currShape := shp ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),PathMIF,mifLineNo ) ;
    end ;

    addRecord( shp ) ;
    if Items.Count = 1 then Extent := shp.Extent ;

  end;

  procedure TGIS_LayerMIF.doRegion( const _param : String ) ;
  var
    i,j      : Integer ;
    tkn      : TGIS_Tokenizer ;
    shp      : TGIS_Shape ;
    line     : String ;
    part_no  : Integer ;
    point_no : Integer ;
  begin
    try
      shp := CreateShape( TGIS_ShapeType.Polygon ) ;
      part_no := StrToInt( _param ) ;

      shp.Lock( TGIS_Lock.Projection ) ;
      tkn := TGIS_Tokenizer.Create ;
      try
        for j:=1 to part_no do begin
          if mifFile.Eof then break ;

          line := mifReadLine ;
          try
            point_no := StrToInt( Trim( line ) ) ;
          except
            point_no := 0 ;
          end ;
          if point_no = 0 then continue ;

          shp.AddPart ;
          for i:=1 to point_no do begin
            if mifFile.Eof then break ;

            line := mifReadLine ;
            tkn.Execute( line, [ ' ', #9 ] ) ;
            if tkn.Result.Count <> 2 then exit ;
            shp.AddPoint( GisPoint( DotStrToFloat( tkn.Result[0] ) ,
                                    DotStrToFloat( tkn.Result[1] ) )
                        ) ;
          end ;
        end ;
      finally
        FreeObject( tkn ) ;
        shp.Unlock ;
        currShape := shp ;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), PathMIF, mifLineNo ) ;
    end ;

    addRecord( shp ) ;
    if Items.Count = 1 then Extent := shp.Extent ;
  end ;

  procedure TGIS_LayerMIF.doPLine( const _param1, _param2 : String ) ;
  var
    i        : Integer ;
    tkn      : TGIS_Tokenizer ;
    shp      : TGIS_Shape ;
    line     : String ;
    point_no : Integer ;
    part_no  : Integer ;
  begin
    try
      shp := CreateShape( TGIS_ShapeType.Arc ) ;

      shp.Lock( TGIS_Lock.Projection ) ;
      try
        if CompareText( _param1, MIF_MULTIPLE ) = 0 then begin
          part_no := StrToInt( _param2 ) ;

          tkn := TGIS_Tokenizer.Create ;
          try
            for i:=1 to part_no do begin
              if mifFile.Eof then break ;

              line := mifReadLine ;
              point_no := StrToInt( Trim( line ) ) ;
              doPLineElement( shp, point_no ) ;
            end ;
          finally
            FreeObject( tkn ) ;
          end ;
        end
        else begin
          if not IsStringEmpty( _param1 ) then
            point_no := StrToInt( _param1 )
          else
            point_no := StrToInt( Trim(mifReadLine) ) ;
          doPLineElement( shp, point_no ) ;
        end ;
      finally
        shp.Unlock ;
        currShape := shp ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), PathMIF, mifLineNo ) ;
    end ;

    addRecord( shp ) ;
    if Items.Count = 1 then Extent := shp.Extent ;
  end ;

  procedure TGIS_LayerMIF.doPLineElement( const _shp      : TGIS_Shape ;
                                          const _point_no : Integer
                                        ) ;
  var
    i    : Integer ;
    tkn  : TGIS_Tokenizer ;
    line : String ;
  begin
    _shp.AddPart ;

    tkn := TGIS_Tokenizer.Create ;
    try
      for i:=1 to _point_no do begin
        if mifFile.Eof then break ;

        line := mifReadLine ;
        tkn.Execute( line, [ ' ', #9 ] ) ;
        if tkn.Result.Count <> 2 then exit ;
        _shp.AddPoint( GisPoint( DotStrToFloat( tkn.Result[0] ) ,
                                 DotStrToFloat( tkn.Result[1] ) )
                     ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_LayerMIF.doLine( const _ax, _ay, _bx, _by : String ) ;
  var
    shp : TGIS_Shape ;
  begin
    try
      shp := CreateShape( TGIS_ShapeType.Arc ) ;
      shp.Lock( TGIS_Lock.Projection ) ;
      try
        shp.AddPart ;
        shp.AddPoint( GisPoint( DotStrToFloat( _ax ) , DotStrToFloat( _ay ) ) ) ;
        shp.AddPoint( GisPoint( DotStrToFloat( _bx ) , DotStrToFloat( _by ) ) ) ;
      finally
        shp.Unlock ;
        currShape := shp ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), PathMIF, mifLineNo ) ;
    end ;

    addRecord( shp ) ;
    if Items.Count = 1 then Extent := shp.Extent ;
  end ;

  procedure TGIS_LayerMIF.doArc( const _param : String ) ;
  var
    shp    : TGIS_Shape ;
    tkn    : TGIS_Tokenizer ;
    a , b  : Double ;
    xa, xb : Double ;
    ya, yb : Double ;
    sa, ea : Double ;
    sw, an : Double ;
    i      : Integer ;
    ctr    : TGIS_Point3D ;
    asin   : Double ;
    acos   : Double ;
  begin
    try
      shp := CreateShape( TGIS_ShapeType.Arc ) ;
      try
        shp.AddPart;
        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.ExecuteEx( _param, ' ' ) ;
          xa := DotStrToFloat( tkn.Result[ 1 ] ) ;
          ya := DotStrToFloat( tkn.Result[ 2 ] ) ;
          xb := DotStrToFloat( tkn.Result[ 3 ] ) ;
          yb := DotStrToFloat( tkn.Result[ 4 ] ) ;
          sa := DotStrToFloat( tkn.Result[ 5 ] ) ;
          ea := DotStrToFloat( tkn.Result[ 6 ] ) ;
        finally
          FreeObject( tkn ) ;
        end;

        ctr := GisPoint3D( ( xa + xb )/2, ( ya + yb )/2, 0 ) ;
        a := Abs( ctr.X - xa ) ;
        b := Abs( ctr.Y - ya ) ;

        sa := DegToRad( sa ) ;
        ea := DegToRad( ea ) ;

        if ( ea < sa ) then
          ea := ea + 2*Pi ;

        sw := ( ea-sa)/ 90 ;
        for i := 0 to 89 do begin
          an := sa + i*sw ;
          SinCos( an, asin, acos ) ;
          shp.AddPoint( GisPoint( ctr.X + a*acos, ctr.Y + b*asin ) ) ;
        end;

      finally
        currShape := shp ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), PathMIF, mifLineNo ) ;
    end ;
  end;

  procedure TGIS_LayerMIF.doEllipse( const _ax, _ay, _bx, _by : String ) ;
  var
    shp : TGIS_Shape ;
    a,b : Double ;
    ctr : TGIS_Point3D ;
  begin
    try
      shp := CreateShape( TGIS_ShapeType.Arc ) ;
      try
        ctr := GisPoint3D( (DotStrToFloat( _ax ) + DotStrToFloat( _bx ))/2,
                           (DotStrToFloat( _ay ) + DotStrToFloat( _by ))/2,0
                          ) ;
        a := Abs( ctr.X - DotStrToFloat( _ax ) ) ;
        b := Abs( ctr.Y - DotStrToFloat( _ay ) ) ;

        shp.StrokeArc( ctr, a, b, 0, 2*Pi, 0, 90 ) ;
      finally
        currShape := shp ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), PathMIF, mifLineNo ) ;
    end ;

    addRecord( shp ) ;
    if Items.Count = 1 then Extent := shp.Extent ;
  end;

  procedure TGIS_LayerMIF.doText( const _param : String ) ;
  var
    shp  : TGIS_Shape ;
    tkn  : TGIS_Tokenizer ;
    line : String ;
    str  : String ;
    flg  : TReplaceFlags  ;
  begin
    try
      shp := CreateShape( TGIS_ShapeType.Point ) ;
      shp.Params.Marker.Size := 0 ;
      shp.Lock( TGIS_Lock.Projection ) ;
      try
        shp.AddPart ;

        tkn := TGIS_Tokenizer.Create ;
        try
          if mifFile.Eof then exit ;

          line := mifReadLine ;
          if length( _param ) < 5 then begin
            str  := Trim( line ) ;
            line := mifReadLine ;
          end
          else
            str := '' ;

          tkn.Execute( line, [ ' ' ] ) ;
          if tkn.Result.Count < 2 then exit ;
          shp.AddPoint( GisPoint( DotStrToFloat( tkn.Result[0] ) ,
                                  DotStrToFloat( tkn.Result[3] )
                                )
                      ) ;
          shp.Params.Labels.Field     := MIF_FLD_LABEL;
          shp.Params.Labels.Alignment := TGIS_LabelAlignment.LeftJustify;
          shp.Params.Labels.Color     := shp.Params.Labels.FontColor;

          shp.Params.Labels.Position  := GisGetLabelPosition( TGIS_LabelPosition.DownRight ) ;

          if IsStringEmpty( str ) then
            line := Copy( _param, 6+StringFirst, length( _param ) - 7 )
          else
            line := Copy( str, 1+StringFirst, length( str ) - 2 ) ;

          {$IFDEF OXYGENE}
            flg := [ TReplaceFlag.rfReplaceAll ] ;
          {$ELSE}
            flg := [ rfReplaceAll ] ;
          {$ENDIF}
          line := StringReplace( line, '\n', #13, flg );
          shp.SetField( MIF_FLD_LABEL, line );
        finally
          FreeObject( tkn ) ;
        end ;
      finally
        shp.Unlock ;
        currShape := shp ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), PathMIF, mifLineNo ) ;
    end ;

    addRecord( shp ) ;
    if Items.Count = 1 then Extent := shp.Extent ;
  end ;

  procedure TGIS_LayerMIF.doPoint( const _x, _y : String ) ;
  var
    shp  : TGIS_Shape ;
    tkn  : TGIS_Tokenizer ;
    line : String ;
  begin
    try
      shp := CreateShape( TGIS_ShapeType.Point ) ;
      shp.Lock( TGIS_Lock.Projection ) ;
      try
        shp.AddPart ;

        if not IsStringEmpty( _x ) then begin
          shp.AddPoint( GisPoint( DotStrToFloat( _x ) ,
                                  DotStrToFloat( _y )
                                )
                      ) ;
        end
        else begin
          tkn := TGIS_Tokenizer.Create ;
          try
            if mifFile.Eof then exit ;

            line := mifReadLine ;
            tkn.Execute( line, [ ' ', #9 ] ) ;
            if tkn.Result.Count <> 2 then exit ;
            shp.AddPoint( GisPoint( DotStrToFloat( tkn.Result[0] ) ,
                                    DotStrToFloat( tkn.Result[1] )
                                  )
                        ) ;
          finally
            FreeObject( tkn ) ;
          end ;
        end ;
      finally
        shp.Unlock ;
        currShape := shp ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), PathMIF, mifLineNo ) ;
    end ;

    addRecord( shp ) ;
    if Items.Count = 1 then Extent := shp.Extent ;
  end ;

  procedure TGIS_LayerMIF.writeHeader( const _strm  : TGIS_Stream ;
                                       const _ex    : TGIS_Extent ;
                                       const _cs    : TGIS_CSCoordinateSystem
                                     ) ;
  var
    i    : Integer        ;
    fld  : TGIS_FieldInfo ;
    stmp : String         ;
    cnt  : Integer        ;
  begin
    _strm.WriteLine( 'Version 300' ) ;
    _strm.WriteLine( 'Charset "Neutral"' ) ;
    _strm.WriteLine( 'Delimiter ","' ) ;
    _strm.WriteLine( Format( '%s Bounds (%s, %s) (%s, %s)',
                             [ TGIS_CSFactoryMapInfo.CsToText( _cs ),
                               DotFloatToStr( _ex.XMin ),
                               DotFloatToStr( _ex.YMin ),
                               DotFloatToStr( _ex.XMax ),
                               DotFloatToStr( _ex.YMax )
                             ]
                           )
                   ) ;

    cnt := 0 ;
    for i:=0 to Fields.Count - 1 do begin
      fld := FieldInfo( i ) ;

      if fld.Deleted   then continue ;
      if fld.Temporary then continue ;

      inc( cnt ) ;
    end ;

    _strm.WriteLine( Format( 'Columns %d', [ cnt ] ) ) ;

    for i:=0 to Fields.Count - 1 do begin
      fld := FieldInfo( i ) ;

      if fld.Deleted    then continue ;
      if fld.Temporary  then continue ;

      case fld.FieldType of
        TGIS_FieldType.String :
                              begin
                                stmp := Format( ' %s %s(%d)',
                                                [ fld.ExportName,
                                                  MIF_CHAR,
                                                  fld.NewWidth
                                                ]
                                              ) ;
                              end ;
        TGIS_FieldType.Date    :
                              begin
                                stmp := Format( ' %s %s',
                                                [ fld.ExportName,
                                                  MIF_DATE
                                                ]
                                              ) ;
                              end ;
        TGIS_FieldType.Boolean :
                              begin
                                stmp := Format( ' %s %s',
                                                [ fld.ExportName,
                                                  MIF_LOGICAL
                                                ]
                                              ) ;
                              end ;
        TGIS_FieldType.Number  :
                              begin
                                stmp := Format( ' %s %s (%d,%d)',
                                                [ fld.ExportName,
                                                  MIF_DECIMAL,
                                                  fld.NewWidth,
                                                  fld.NewDecimal
                                                ]
                                              ) ;
                              end ;
        TGIS_FieldType.Float   :
                              begin
                                stmp := Format( ' %s %s',
                                                [ fld.ExportName,
                                                  MIF_FLOAT
                                                ]
                                              ) ;
                              end ;
      end ;
      _strm.WriteLine( stmp ) ;
    end ;
    _strm.WriteLine( 'Data' ) ;
    _strm.WriteLine( '' ) ;
  end ;

  procedure TGIS_LayerMIF.writeShape( const _mifstrm : TGIS_Stream ;
                                      const _mifshp  : TGIS_Shape  ;
                                      const _midstrm : TGIS_Stream ;
                                      const _midshp  : TGIS_Shape
                                    ) ;
  var
    part_no   : Integer ;
    point_no  : Integer ;
    part_size : Integer ;
    num_parts : Integer ;
    ptg       : TGIS_Point ;
    ext       : TGIS_Extent ;
    was_field : Boolean ;
    sval      : String ;

    function prepare_text( const _val : Variant ) : String ;
    var
      flg : TReplaceFlags ;
    begin
      if VarIsNull( _val ) then
        Result := ''
      else begin
        Result := VarToString( _val ) ;
        {$IFDEF OXYGENE}
          flg := [ TReplaceFlag.rfReplaceAll ] ;
        {$ELSE}
          flg := [ rfReplaceAll ] ;
        {$ENDIF}
        Result := StringReplace( Result, '"', '''', flg ) ;
        Result := StringReplace( Result, #13, '\n', flg ) ;
        Result := StringReplace( Result, #10, ''  , flg ) ;
      end ;
    end;

    procedure mif_write( const _format: String; const _args: array of {$IFNDEF OXYGENE}
                                                                        const
                                                                      {$ELSE}
                                                                        TObject
                                                                      {$ENDIF}
                                                                      ) ;
    begin
      _mifstrm.WriteLine( Format( _format, _args ) ) ;
    end ;

    procedure mid_write ;
    var
      field_no : Integer        ;
      fld      : TGIS_FieldInfo ;
      stmp     : String         ;
      dat      : TDateTime      ;
      bl       : Boolean        ;
      fl       : Double         ;
      v        : Variant        ;
    begin

      for field_no := 0 to Fields.Count -1 do begin
        fld := FieldInfo( field_no ) ;
        if fld.Deleted   then continue ;
        if fld.Temporary then continue ;
      end ;

      was_field := False ;

      for field_no := 0 to Fields.Count -1 do begin
        fld := FieldInfo( field_no ) ;
        if fld.Deleted   then continue ;
        if fld.Temporary then continue ;

        if was_field then
          _midstrm.WriteString( ',' ) ;
        was_field := True ;

        v := _midshp.GetFieldEx( fld.NewName ) ;
        case fld.FieldType of
          TGIS_FieldType.String  :
               begin
                 _midstrm.WriteString( Format('"%s"', [ prepare_text( v ) ] ) ) ;
               end ;
          TGIS_FieldType.Date :
               begin
                 if not VarIsNull( v ) then begin
                   dat := VarToDateTime( v ) ;
                   _midstrm.WriteString( '"' +
                                         FormatDateTime( 'yyyyMMdd', dat ) +
                                         '"'
                                       ) ;
                 end ;
               end ;
          TGIS_FieldType.Boolean :
               begin
                 if not VarIsNull( v ) then begin
                   try
                     bl := VarToBoolean( v ) ;
                   except
                     bl := False;
                   end ;
                   _midstrm.WriteString( IntToStr( Integer( bl ) ) ) ;
                 end ;
               end ;
          TGIS_FieldType.Number :
               begin
                 if not VarIsNull( v ) then begin
                   try
                     fl := VarToDouble( v ) ;
                   except
                     fl := 0;
                   end ;
                   stmp := Trim( Format( '%*.*f',
                                         [ fld.NewWidth, fld.NewDecimal, fl ]
                                       )
                               ) ;
                   stmp := StringReplace( stmp, ',', '.', [] ) ;

                   _midstrm.WriteString( stmp ) ;
                 end ;
               end ;
          TGIS_FieldType.Float :
               begin
                 if not VarIsNull( v ) then begin
                   try
                     fl := VarToDouble( v ) ;
                   except
                     fl := 0;
                   end ;
                   _midstrm.WriteString( DotFloatToStr( fl ) ) ;
                 end ;
               end ;
          else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '', 0 ) ;
        end ;

      end ;

      _midstrm.WriteLine( '' ) ;
    end ;

    function getStyle : Integer ;
    begin
      if TGIS_FontStyle.Bold in _mifshp.Params.Labels.Font.Style then
        Result := 1
      else if TGIS_FontStyle.Italic in _mifshp.Params.Labels.Font.Style then
        Result := 2
      else if TGIS_FontStyle.Underline in _mifshp.Params.Labels.Font.Style then
        Result := 4
      else
        Result := 0;
    end;

    function getJustify : String ;
    begin
      case _mifshp.Params.Labels.Alignment of
        TGIS_LabelAlignment.LeftJustify  : Result := MIF_LEFT;
        TGIS_LabelAlignment.Center       : Result := MIF_CENTER ;
        TGIS_LabelAlignment.RightJustify : Result := MIF_RIGHT
      else                                 Result := MIF_LEFT ;
      end;
    end;

    function writeColors( const _type : Integer ) : String ;
    var
      w     : Integer ;
      p     : Integer ;
      c     : Integer ;
      cw    : Integer ;
    begin
      if _type = 0 then begin //pen

        w := 1;
        p := 2;
        c := TGIS_Color.Black.ToRGB ;

        case _mifshp.ShapeType of
          TGIS_ShapeType.Polygon : begin
            w := Abs( _mifshp.Params.Area.OutlineWidth ) ;

            case _mifshp.Params.Area.OutlineStyle of
              TGIS_PenStyle.Solid :         p := 2;
              TGIS_PenStyle.Dash :          p := 5;
              TGIS_PenStyle.Dot :           p := 3;
              TGIS_PenStyle.DashDot :       p := 14;
              TGIS_PenStyle.DashDotDot :    p := 20;
              TGIS_PenStyle.Clear :         p := 1
            end;

            c := _mifshp.Params.Area.OutlineColor.ToRGB ;
          end;
          TGIS_ShapeType.Arc : begin
            w := Abs( _mifshp.Params.Line.Width ) ;

            case _mifshp.Params.Line.Style of
              TGIS_PenStyle.Solid :         p := 2;
              TGIS_PenStyle.Dash :          p := 5;
              TGIS_PenStyle.Dot :           p := 3;
              TGIS_PenStyle.DashDot :       p := 14;
              TGIS_PenStyle.DashDotDot :    p := 20;
              TGIS_PenStyle.Clear :         p := 1;
            end;

            c := _mifshp.Params.Line.Color.ToRGB ;
          end ;
        end;
        mif_write( '%s (%d,%d,%d)', [ MIF_PEN, w, p, c ] ) ;
      end
      else if _type = 1 then begin // brush
        p  := 1 ;
        c := _mifshp.Params.Area.Color.ToRGB ;
        cw := TGIS_Color.White.ToRGB ;

        case _mifshp.Params.Area.Pattern of
          TGIS_BrushStyle.Clear :         p := 1 ;
          TGIS_BrushStyle.Solid :         p := 2 ;
          TGIS_BrushStyle.Horizontal :    p := 3 ;
          TGIS_BrushStyle.Vertical :      p := 4 ;
          TGIS_BrushStyle.FDiagonal :     p := 6 ;
          TGIS_BrushStyle.BDiagonal :     p := 5 ;
          TGIS_BrushStyle.Cross :         p := 7 ;
          TGIS_BrushStyle.DiagCross :     p := 8 ;
        end;

        mif_write( '%s (%d,%d,%d)', [ MIF_BRUSH, p, c,  cw ] ) ;
      end
      else if _type = 2 then begin // color

        if _mifshp.Params.Marker.Size > 0 then
          w := Min( 12, Abs( _mifshp.Params.Marker.Size div 20 ) )
        else
          w := 12 ;

        c := _mifshp.Params.Marker.Color.ToRGB ;
        mif_write( '%s (%d,%d,%d)', [ MIF_SYMBOL, 35, c, w ] ) ;
      end;

    end;

    {$IFDEF OXYGENE}
      function PosEx( const _phrase : String ;
                      const _string : String ;
                      const _offset : Integer  = 1
                    ) : Integer ;
      var
        c          : Char    ;
        i, j       : Integer ;
        phrase_len : Integer ;
        string_len : Integer ;
       begin
         Result := 0;
         phrase_len := length(_phrase);
         string_len := length(_string);

         if string_len = 0 then exit ;
         if phrase_len = 0 then exit ;
         if ( _offset < 1 ) or ( _offset > string_len ) then exit ;

         c := _phrase[1];
         for i := _offset to string_len - phrase_len + 1 do begin
           if _string[i] = c then begin
             for j := 1 to phrase_len do begin
               if      _string[i + i - 1] <> _phrase[i] then
                         break
               else if i = phrase_len then begin
                         Result := i;
                         exit;
                       end;
             end ;
           end ;
         end ;
      end ;
    {$ENDIF}

    procedure updateMBR( const _ptg   : TGIS_Point ;
                         const _val   : String ;
                         const _angle : Double ;
                           var _ext   : TGIS_Extent
                       ) ;
    var
      i, p       : Integer ;
      dsin, dcos : Double  ;
      dx0, dy0   : Double  ;
      dx1, dy1   : Double  ;
      dx, dy     : array [0..3] of Double ;
      r, len     : Integer ;
      dwidth     : Double  ;
      dheight    : Double  ;
    begin
      dx0 := _ptg.X ;
      dy0 := _ptg.Y ;

      _ext.XMin := _ptg.X ;
      _ext.YMin := _ptg.Y ;
      _ext.XMax := _ptg.X ;
      _ext.YMax := _ptg.Y ;

      SinCos( _angle, dsin, dcos ) ;

      len := 1;
      r   := StringFirst ;
      p   := Pos( '\n', _val ) ;
      if p = StringFirst - 1 then
        len := length( _val ) + 1 ;

      dheight := 10 ;
      while p > StringFirst - 1 do begin
        dheight := dheight * 2 ;
        len := Max( len, length( Copy( _val, r, p-r ) ) ) ;
        r   := p ;
        p   := PosEx( '\n', _val, p+2 ) ;
      end;

      dwidth := 1.6 * len ;

      dx[0] := dx0 ;
      dy[0] := dy0 ;
      dx[1] := dx0 + dwidth  ;
      dy[1] := dy0 ;
      dx[2] := dx0 + dwidth  ;
      dy[2] := dy0 + dheight ;
      dx[3] := dx0 ;
      dy[3] := dy0 + dheight ;

      for i := 0 to 3 do begin
        dx1 := dx0 + ( dx[i] - dx0 )*dcos - ( dy[i] - dy0 )*dsin ;
        dy1 := dy0 + ( dx[i] - dx0 )*dsin + ( dy[i] - dy0 )*dcos ;

        if (dx1 < _ext.XMin) then _ext.XMin := dx1 ;
        if (dx1 > _ext.XMax) then _ext.XMax := dx1 ;
        if (dy1 < _ext.YMin) then _ext.YMin := dy1 ;
        if (dy1 > _ext.YMax) then _ext.YMax := dy1 ;
      end;

    end;

  begin
    assert( assigned( _mifshp ) ) ;
    assert( assigned( _midshp ) ) ;

    num_parts := _mifshp.GetNumParts ;
    assert( num_parts > 0 ) ;

    _mifshp.Lock( TGIS_Lock.Projection ) ;
    try
      if      _mifshp is TGIS_ShapePoint then
              begin
                ptg := _mifshp.GetPoint( 0, 0 ) ;
                // label
                if _mifshp.Params.Labels.Field = MIF_FLD_LABEL then begin
                  sval := prepare_text(
                            _mifshp.GetField( _mifshp.Params.Labels.Field )
                          ) ;
                  mif_write( MIF_TEXT, [] ) ;
                  mif_write( '"%s"', [ sval ] ) ;
                  updateMBR( ptg, sval, _mifshp.Params.Labels.Rotate, ext ) ;
                  mif_write( '%s %s %s %s', [ DotFloatToStr( ext.XMin ),
                                              DotFloatToStr( ext.YMax ),
                                              DotFloatToStr( ext.XMax ),
                                              DotFloatToStr( ext.YMin ) ]
                            );
                  mif_write( '%s ("%s",%d,%d,%d)',
                    [ MIF_FONT,
                      _mifshp.Params.Labels.FontName,
                      getStyle,
                      0,
                      _mifshp.Params.Labels.FontColor.ToRGB
                    ]
                  ) ;
                  if _mifshp.Params.Labels.Rotate <> 0 then
                    mif_write( '%s %f',
                               [ MIF_ANGLE,
                                 -RadToDeg( _mifshp.Params.Labels.Rotate )
                               ]
                             ) ;
                  mif_write('%s %s', [ MIF_JUSTIFY, getJustify ] ) ;
                end
                else
                  mif_write( '%s %s %s', [ MIF_POINT,
                                           DotFloatToStr( ptg.X ),
                                           DotFloatToStr( ptg.Y )
                                         ]
                          ) ;

                writeColors( 2 ) ;

                mid_write ;
              end
      else if _mifshp is TGIS_ShapeMultiPoint then
              begin
                part_size := _mifshp.GetPartSize( 0 ) ;
                for point_no := 0 to part_size - 1 do begin
                  ptg := _mifshp.GetPoint( 0, point_no ) ;
                  mif_write( '%s %s %s', [ MIF_POINT,
                                           DotFloatToStr( ptg.X ),
                                           DotFloatToStr( ptg.Y )
                                         ]
                           ) ;
                  mid_write ;
                end ;
                writeColors( 2 ) ;
              end
      else if _mifshp is TGIS_ShapeArc then
              begin
                if num_parts = 1 then begin
                  part_size := _mifshp.GetPartSize( 0 ) ;
                  mif_write( '%s %d', [MIF_PLINE, part_size] ) ;
                  for point_no := 0 to part_size - 1 do begin
                    ptg := _mifshp.GetPoint( 0, point_no ) ;
                    mif_write( '%s %s', [ DotFloatToStr( ptg.X ),
                                          DotFloatToStr( ptg.Y )
                                        ]
                             ) ;
                  end ;
                  mid_write ;
                end
                else begin
                  mif_write( '%s %s %d',
                             [ MIF_PLINE, MIF_MULTIPLE, num_parts ]
                           ) ;
                  for part_no := 0 to num_parts - 1 do begin
                    part_size := _mifshp.GetPartSize( part_no ) ;
                    mif_write( '%d', [ part_size ] ) ;
                    for point_no := 0 to part_size - 1 do begin
                      ptg := _mifshp.GetPoint( part_no, point_no ) ;
                      mif_write( '%s %s', [ DotFloatToStr( ptg.X ),
                                            DotFloatToStr( ptg.Y )
                                          ]
                               ) ;
                    end ;
                  end ;
                  mid_write ;
                end ;
                writeColors( 0 ) ;
              end
      else if (_mifshp is TGIS_ShapePolygon   ) or
              (_mifshp is TGIS_ShapeMultiPatch) then
              begin
                mif_write( '%s %d', [ MIF_REGION, num_parts ] ) ;
                for part_no := 0 to num_parts - 1 do begin
                  part_size := _mifshp.GetPartSize( part_no ) ;
                  mif_write( '%d', [ part_size ] ) ;
                  for point_no := 0 to part_size - 1 do begin
                    ptg := _mifshp.GetPoint( part_no, point_no ) ;
                    mif_write( '%s %s', [ DotFloatToStr( ptg.X ),
                                          DotFloatToStr( ptg.Y )
                                        ]
                             ) ;
                  end ;
                end ;
                mid_write ;
                writeColors( 0 ) ;
                writeColors( 1 ) ;
              end
      else    begin
                assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                Abort ;
              end ;
    finally
      _mifshp.Unlock ;
    end ;
  end ;

  procedure TGIS_LayerMIF.setUp ;
  var
    line  : String ;
    par1  : String ;
    par2  : String ;
    par3  : String ;
    par4  : String ;
    tkn   : TGIS_Tokenizer ;

    function _t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( tkn.Result[0], _val ) = 0 ;
    end ;

    function _p( const _val : String ) : Boolean ;
    begin
      Result := Pos( _val, UpperCase( line ) ) > 0 ;
    end ;

  begin
    inherited ;

    FSupportedShapes := GisGetEmptyShapeType ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Point ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Arc ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Polygon ) ;

    if not IsStringEmpty( PathMIF ) then begin
      mifFile := TGIS_BufferedFileStream.Create( PathMIF, TGIS_StreamMode.Read ) ;

      if SafeFileExists( PathMID ) then begin
        midFile := TGIS_BufferedFileStream.Create( PathMID, TGIS_StreamMode.Read ) ;
        midFile.CodePage    := CodePage ;
      end ;

      delimMif := ',' ;

      tkn := TGIS_Tokenizer.Create ;

      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
      Lock ;
      try
        if assigned( mifFile ) then
        while not mifFile.Eof do begin
          line := mifReadLine ;

          tkn.Execute( line, [' ', #9] ) ;

          par1 := '' ;
          par2 := '' ;
          par3 := '' ;
          par4 := '' ;
          if tkn.Result.Count = 0 then continue ;
          if tkn.Result.Count > 1 then par1 := tkn.Result[1] ;
          if tkn.Result.Count > 2 then par2 := tkn.Result[2] ;
          if tkn.Result.Count > 3 then par3 := tkn.Result[3] ;
          if tkn.Result.Count > 4 then par4 := tkn.Result[4] ;

          if      _t(MIF_DELIMITER) then delimMif   := par1
          else if _t(MIF_COORDSYS ) then doCoordSys ( line                   )
          else if _t(MIF_COLUMNS  ) then doColumns  ( par1                   )
          else if _t(MIF_REGION   ) then doRegion   ( par1                   )
          else if _t(MIF_RECTANGLE) then doRectangle( par1, par2, par3, par4 )
          else if _t(MIF_ROUNDRECT) then doRectangle( par1, par2, par3, par4 )
          else if _t(MIF_PLINE    ) then doPLine    ( par1, par2             )
          else if _t(MIF_LINE     ) then doLine     ( par1, par2, par3, par4 )
          else if _t(MIF_ELLIPSE  ) then doEllipse  ( par1, par2, par3, par4 )
          else if _t(MIF_POINT    ) then doPoint    ( par1, par2             )
          else if _t(MIF_TEXT     ) then doText     ( line                   )
          else if _t(MIF_ARC      ) then doArc      ( line                   )
          else if _p(MIF_BRUSH    ) then doColor    ( line, 0                )
          else if _p(MIF_SYMBOL   ) then doColor    ( line, 2                )
          else if _t(MIF_ANGLE    ) then doAngle    ( par1                   )
          else if _t(MIF_JUSTIFY  ) then doJustify  ( par1                   )
          else if _p(MIF_PEN      ) then doColor    ( line, 1                )
          else if _t(MIF_FONT     ) then doFont     ( line                   )
          else
            currShape := nil ;

        end ;
      finally
        Unlock ;
        FIsModified := False ;

        RaiseBusyRelease( Self ) ;

        FreeObject( tkn ) ;

        FreeObject( mifFile ) ;
        FreeObject( midFile ) ;
      end ;
    end ;

    if SafeFileExists( PathMIF ) then
      FAge := GisFileAge( PathMIF ) ;

    FFileInfo := 'MapInfo Interchange Format (MIF/MID)' ;
  end ;

  procedure TGIS_LayerMIF.Build( const _path   : String ;
                                 const _extent : TGIS_Extent;
                                 const _type   : TGIS_ShapeType ;
                                 const _dim    : TGIS_DimensionType) ;
  var
    mif_path : String ;
  begin
    inherited ;

    mif_path := GetPathNoExt(_path) + getFileExtMID ;

    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;
    if SafeFileExists( mif_path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), mif_path, 0 ) ;
    end ;

    ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;
  end ;

  procedure TGIS_LayerMIF.ImportLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent ;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape ;
    const _de9im     : String ;
    const _truncated : Boolean
  ) ;
  var
    shape_strm : TGIS_BufferedFileStream ;
    data_strm  : TGIS_BufferedFileStream ;
    {$IFNDEF OXYGENE}
      shp      : TGIS_Shape  ;
    {$ENDIF}
    shp_tmp    : TGIS_Shape  ;
    ex         : TGIS_Extent ;
    shape_no   : Cardinal    ;
    end_uid    : TGIS_Uid    ;
    abort      : Boolean     ;
    old_scope  : String      ;

    // add _DUMMY filed to ensure at leas one column
    procedure add_dummy_field ;
    var
      id  : Integer ;
      cnt : Integer ;
      fld : TGIS_FieldInfo ;
    begin
      cnt := 0 ;
      for id := 0 to Fields.Count -1 do begin
        fld := FieldInfo( id ) ;
        if fld.Temporary then continue ;
        if fld.Deleted   then continue ;
        inc( cnt ) ;
      end;

      if cnt > 0 then exit ;

      id := FindField( GIS_FIELD_DUMMY ) ;
      if id >= 0 then begin
        fld := _layer.FieldInfo( id ) ;
        fld.Deleted := False
      end
      else
        AddFieldInternal( GIS_FIELD_DUMMY, TGIS_FieldType.String, 1, 0 ) ;
    end ;

  begin
    if not assigned( _layer ) then exit ;

    if not ( CheckFileWriteAccessEx( Path   , True, True, True ) and
             CheckFileWriteAccessEx( PathMID, True, True, True )
           )
    then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try
      ImportStructure( _layer ) ;
      add_dummy_field ;
      PrepareExportFieldNames( 32 ) ;

      ExportStructureToFLD ;

      // prepare temporary geometry
      try
        shape_strm := TGIS_BufferedFileStream.Create(
                        GetTemporaryName( PathMIF ),
                        TGIS_StreamMode.&Create
                      ) ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                     GetTemporaryName( PathMIF ),
                                     GetLastError
                                   ) ;
      end ;

      // prepare temporary database
      try
        data_strm := TGIS_BufferedFileStream.Create(
                        GetTemporaryName( PathMID ),
                        TGIS_StreamMode.&Create
                      ) ;
        data_strm.CodePage    := CodePage    ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                     GetTemporaryName( PathMID ),
                                     GetLastError
                                   ) ;
      end ;

      ex := GisCommonExtent( _layer.Extent, _extent ) ;

      if _layer.CS.CanConvert( CS ) then
        ex := _layer.CS.ExtentToCS( CS, ex ) ;

      writeHeader( shape_strm, ex, CS ) ;

      try
        old_scope := _layer.Scope ;
        _layer.Scope := '' ;

        _layer.ParamsList.Selected := 0 ;

        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
            _layer.Loop( _extent, _scope, _shape, _de9im ) do
        begin
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
              writeShape( shape_strm, shp_tmp, data_strm, shp ) ;
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
        _layer.Scope := old_scope ;

        FreeObject( shape_strm ) ;
        FreeObject( data_strm  ) ;

        if abort then begin
          {$IFDEF DCC}System.SysUtils.{$ENDIF}DeleteFile( GetTemporaryName( PathMIF ) ) ;
          {$IFDEF DCC}System.SysUtils.{$ENDIF}DeleteFile( GetTemporaryName( PathMID ) ) ;
        end
        else begin
          {$IFDEF DCC}System.SysUtils.{$ENDIF}DeleteFile( GetBackupName( PathMIF ) ) ;
          {$IFDEF DCC}System.SysUtils.{$ENDIF}DeleteFile( GetBackupName( PathMID ) ) ;

          {$IFDEF DCC}System.SysUtils.{$ENDIF}RenameFile( PathMIF, GetBackupName( PathMIF ) ) ;
          {$IFDEF DCC}System.SysUtils.{$ENDIF}RenameFile( PathMID, GetBackupName( PathMID ) ) ;

          try
            if not RenameFile( GetTemporaryName( PathMIF ), PathMIF ) then
              raise EGIS_Exception.Create(
                      _rsrc( GIS_RS_ERR_FILEWRITE ), PathMIF, GetLastError
                    ) ;
            if not RenameFile( GetTemporaryName( PathMID ), PathMID ) then
              raise EGIS_Exception.Create(
                      _rsrc( GIS_RS_ERR_FILEWRITE ), PathMID, GetLastError
                    ) ;
          except
            // recover ;
            RenameFile( GetBackupName( PathMIF ), PathMIF ) ;
            RenameFile( GetBackupName( PathMID ), PathMID ) ;
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

  procedure TGIS_LayerMIF.SaveData  ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerMif.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-MIF', 'MapInfo Interchange Format', TGIS_LayerMIF, '.mif',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                    GIS_HIGH_LAYER_PRIORITY, True
                  );
  end;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerMif.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

