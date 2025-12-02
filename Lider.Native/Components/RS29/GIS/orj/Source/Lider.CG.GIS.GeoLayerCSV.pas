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
  Encapsulation of a CSV layer.
}

{$IFDEF DCC}
  unit GisLayerCSV ;
  {$HPPEMIT '#pragma link "GisLayerCSV"'}
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

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,
    System.Classes,

    GisTypes,
    GisStreams,

    GisLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerCSV = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Layer which can read a CSV text file.
  /// </summary>
  TGIS_LayerCSV = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      ftypes     : array of TGIS_FieldType ;
    private

      /// <summary>
      ///   Data file line.
      /// </summary>
      csvLineNo : Integer ;

      /// <summary>
      ///   Dimension.
      /// </summary>
      iDim : Integer ;

      /// <summary>
      ///   Column separator in a csv file.
      /// </summary>
      FSeparator : Char ;

      /// <summary>
      ///   Decimal point.
      /// </summary>
      FDecimalPoint : Char ;

      /// <summary>
      ///   True, if auto recognize separator and decimal point upon read.
      /// </summary>
      FAutoRecognize : Boolean ;
    private

      /// <summary>
      ///   Analyze line to verify what filed type can be utilized
      /// </summary>
      /// <param name="_line">
      ///   line to be interpreted
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure analyzeShape  ( const _line      : String
                              ) ;

      /// <summary>
      ///   Read a new shape and add it to the layer.
      /// </summary>
      /// <param name="_line">
      ///   line to be interpreted
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure readShape     ( const _line      : String
                              ) ;

      /// <summary>
      ///   Write fields header to the output stream.
      /// </summary>
      /// <param name="_csvfile">
      ///   CSV file to be written
      /// </param>
      procedure writeHeader   ( const _csvfile   : TGIS_Stream
                              ) ;

      /// <summary>
      ///   Write shape geometry and data to the output stream.
      /// </summary>
      /// <param name="_csvfile">
      ///   CSV file to be written
      /// </param>
      /// <param name="_csvshp">
      ///   shape geometry (could be truncated)
      /// </param>
      procedure writeShape    ( const _csvfile   : TGIS_Stream    ;
                                const _csvshp    : TGIS_Shape      ;
                                const _fullshp   : TGIS_Shape
                              ) ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp      ; override;
    protected
      // destructor

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
      procedure ImportLayerEx( const _layer     : TGIS_LayerVector ;
                               const _extent    : TGIS_Extent      ;
                               const _type      : TGIS_ShapeType   ;
                               const _scope     : String           ;
                               const _shape     : TGIS_Shape       ;
                               const _de9im     : String           ;
                               const _truncated : Boolean
                             ) ; override;

      /// <inheritdoc/>
      procedure SaveData   ; override;

    public
      /// <summary>
      ///   Decimal point symbol. Default is comma.
      /// </summary>
      property DecimalPoint : Char
               read  FDecimalPoint
               write FDecimalPoint ;

      /// <summary>
      ///   Column separator. Default is comma.
      /// </summary>
      property Separator : Char
               read  FSeparator
               write FSeparator ;

      /// <summary>
      ///   If true then DecimalPoint and Separator will be auto recognized upon
      ///   reading the file.
      /// </summary>
      /// <remarks>
      ///   After reading file AutoRecognize is set to false to ensure, that
      ///   upon writing, format will be preserved.
      /// </remarks>
      property AutoRecognize : Boolean
               read  FAutoRecognize
               write FAutoRecognize ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisRtl,
    GisClasses,
    GisFunctions,
    GisInternals,
    GisResource,
    GisRegistredLayers ;
{$ENDIF}

const
  // column canes
     CSV_X       = 'X'          ;
     CSV_LON     = 'LON'        ;
     CSV_LON2    = 'LONGITUDE'  ;
     CSV_LON3    = 'EASTING'    ;
     CSV_LON4    = 'EAST'       ;
     CSV_Y       = 'Y'          ;
     CSV_LAT     = 'LAT'        ;
     CSV_LAT2    = 'LATITUDE'   ;
     CSV_LAT3    = 'NORTHING'   ;
     CSV_LAT4    = 'NORTH'      ;
     CSV_Z       = 'Z'          ;
     CSV_Z2      = 'ELEVATION'  ;
     CSV_Z3      = 'LEVEL'      ;
     CSV_UNKNOWN = 'UNKNOWN_%d' ;

//=============================================================================
// TGIS_LayerCSV
//=============================================================================

  constructor TGIS_LayerCSV.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory  ,
                             TGIS_LayerSubType.Exportable
                           ] ;

    SupportedShapes     := GisGetShapeType( TGIS_ShapeType.Point ) ;
    SupportedDimensions := GisGetEmptyDimensionType ;
    SupportedDimensions := GisAddDimensionType( SupportedDimensions, TGIS_DimensionType.XY ) ;
    SupportedDimensions := GisAddDimensionType( SupportedDimensions, TGIS_DimensionType.XYZ ) ;

    FSeparator := ',' ;
    FDecimalPoint := '.' ;
    FAutoRecognize := True ;
  end ;

  procedure TGIS_LayerCSV.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_LayerCSV.analyzeShape(
    const _line : String
  ) ;
  var
    i     : Integer        ;
    j     : Integer        ;
    tkn   : TGIS_Tokenizer ;
    par   : String         ;
  begin
    try
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.ExecuteEx( _line, FSeparator ) ;
        for i:= 0 to tkn.Result.Count - 1 do begin
          par := tkn.Result[i] ;
          if ftypes[i] = TGIS_FieldType.Number then
            try
              StrToInt64( par ) ;
              // additional check to avoid hex format
              for j := StringFirst to StringLast( par ) do
                if not CharInSet( par[j], ['0','1','2','3','4','5','6','7','8','9','-'] ) then
                  raise Exception.Create(
                          Format('''%s'' is not a valid integer value',[par])
                         ) ;
            except
              ftypes[i] := TGIS_FieldType.Float ;
            end ;
          if ftypes[i] = TGIS_FieldType.Float then
            try
              if not FAutoRecognize then
                if Pos( FDecimalPoint, par ) < StringFirst then
                  Abort ;

              DotStrToFloat( par ) ;

              if FAutoRecognize then begin
                if Pos( ',', par ) > StringFirst then
                  FDecimalPoint := ','
                else
                  FDecimalPoint := '.'
              end;
            except
              ftypes[i] := TGIS_FieldType.String ;
            end ;
        end ;
      finally
        FreeObject( tkn ) ;
      end;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),Path,csvLineNo ) ;
    end ;
  end ;

  procedure TGIS_LayerCSV.readShape(
    const _line : String
  ) ;
  var
    i     : Integer        ;
    tkn   : TGIS_Tokenizer ;
    par   : String         ;
    shp   : TGIS_Shape     ;
    ptg   : TGIS_Point     ;
    state : Integer        ;
    fld   : TGIS_FieldInfo ;
    z     : Double         ;
  begin
    try
      if iDim = 2 then
        shp := CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XY )
      else
        shp := CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XYZ ) ;

      {$IFDEF GIS_NORECORDS}
        ptg := new TGIS_Point ;
      {$ENDIF}
      shp.AddPart ;
      shp.Lock( TGIS_Lock.Internal ) ;
      try
        state := 0 ;
        z := 0 ;
        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.ExecuteEx( _line, FSeparator ) ;

          for i:= 0 to tkn.Result.Count - 1 do begin
            par := tkn.Result[i] ;
            fld := FieldInfo( i ) ;
            if fld.FileFormat then begin
              if      fld.NewName = CSV_X then
                      begin
                        ptg.X := DotStrToFloat( par ) ;
                        inc( state ) ;
                      end
              else if fld.NewName = CSV_Y then
                      begin
                        ptg.Y := DotStrToFloat( par ) ;
                        inc( state ) ;
                      end
              else if fld.NewName = CSV_Z then
                      begin
                        z := DotStrToFloat( par ) ;
                      end
              else if fld.NewName = CSV_LON then
                      begin
                        ptg.X := DotStrToFloat( par ) ;
                        inc( state ) ;
                      end
              else if fld.NewName = CSV_LAT then
                      begin
                        ptg.Y := DotStrToFloat( par ) ;
                        inc( state ) ;
                      end
            end
            else begin

              shp.SetField( FieldInfo( i ).NewName, par ) ;
            end
          end ;
        finally
          FreeObject( tkn ) ;
        end;

        if state <> 2 then // both X and Y was set?
          Abort ;

        if iDim = 2 then
          shp.AddPoint( ptg )
        else
          shp.AddPoint3D( GisPoint3D( ptg.X, ptg.Y, z ) ) ;
      finally
        shp.Unlock ;
      end;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),Path,csvLineNo ) ;
    end ;
  end ;

  procedure TGIS_LayerCSV.writeHeader( const _csvfile : TGIS_Stream
                                     ) ;
  var
    i        : Integer        ;
    field_no : Integer        ;
    fld      : TGIS_FieldInfo ;
    cnt      : Integer        ;
    stmp     : String         ;
    stmp_sb  : TStringBuilder ;
  begin
    cnt := 0 ;

    for field_no := 0 to Fields.Count -1 do begin
      fld := FieldInfo( field_no ) ;
      if fld.Deleted then
        continue ;
      inc( cnt ) ;
    end ;

    for field_no := 0 to Fields.Count -1 do begin
      fld := FieldInfo( field_no ) ;
      if fld.Deleted then
        continue ;

      stmp := fld.NewName ;
      stmp_sb := TStringBuilder.Create( stmp ) ;
      try
        for i := 0 to length( stmp ) - 1 do
          if stmp_sb[i] = '"' then stmp_sb[i] := '''' ;
        stmp := stmp_sb.ToString ;
      finally
        FreeObject( stmp_sb ) ;
      end ;

      _csvfile.WriteString( '"' + stmp + '"' ) ;

      if field_no < cnt -1 then _csvfile.WriteString( FSeparator )
                           else _csvfile.WriteLine  ( '' ) ;
    end ;

   end ;

  procedure TGIS_LayerCSV.writeShape( const _csvfile : TGIS_Stream ;
                                      const _csvshp  : TGIS_Shape ;
                                      const _fullshp : TGIS_Shape
                                    ) ;
  var
    ptg       : TGIS_Point ;
    ptg_z     : Double ;

    function localFloatToStr( _val : Double ) : String ;
    var
      stmp : String ;
    begin
      stmp := DotFloatToStr( _val ) ;

      Result := StringReplace( stmp, '.', FDecimalPoint, [] ) ;
    end ;


    procedure csv_write ;
    var
      i        : Integer        ;
      field_no : Integer        ;
      fld      : TGIS_FieldInfo ;
      cnt      : Integer        ;
      stmp     : String         ;
      stmp_sb  : TStringBuilder ;
      dat      : TDateTime      ;
      bl       : Boolean        ;
      fl       : Double         ;
      v        : Variant        ;
    begin
      cnt := 0 ;

      for field_no := 0 to Fields.Count -1 do begin
        fld := FieldInfo( field_no ) ;
        if fld.Deleted then
          continue ;
        inc( cnt ) ;
      end ;

      for field_no := 0 to Fields.Count -1 do begin
        fld := FieldInfo( field_no ) ;
        if fld.Deleted then
          continue ;

        if      (fld.Name = CSV_X) or (fld.Name = CSV_LON) then
          _csvfile.WriteString( localFloatToStr( ptg.X ) )
        else if (fld.Name = CSV_Y) or (fld.Name = CSV_LAT) then
          _csvfile.WriteString( localFloatToStr( ptg.Y ) )
        else if (fld.Name = CSV_Z) then
          _csvfile.WriteString( localFloatToStr( ptg_z ) )
        else begin
          v := _fullshp.GetFieldEx( fld.NewName ) ;
          case fld.FieldType of
            TGIS_FieldType.String  :
                 begin
                   try
                     if VarIsNull( v ) then
                       stmp := ''
                     else
                       {$IFNDEF OXYGENE}
                         stmp := v ;
                       {$ELSE}
                         stmp := VarToString( v ) ;
                       {$ENDIF}
                   except
                     stmp := '' ;
                   end ;

                   stmp_sb := TStringBuilder.Create( stmp ) ;
                   try
                     for i := 0 to length( stmp ) - 1 do
                       if stmp_sb[i] = '"' then stmp_sb[i] := '''' ;
                       stmp := stmp_sb.ToString ;
                   finally
                     FreeObject( stmp_sb ) ;
                   end ;

                   if VarIsNull( v ) then
                     _csvfile.WriteString( stmp )
                   else
                     _csvfile.WriteString( '"' + stmp + '"' ) ;
                 end ;
            TGIS_FieldType.Date :
                 begin
                   if not VarIsNull( v ) then begin
                     {$IFNDEF OXYGENE}
                       dat := v ;
                     {$ELSE}
                       dat := VarToDateTime( v ) ;
                     {$ENDIF}
                     _csvfile.WriteString( '"' + DateTimeToXMLString(dat,0,False) + '"' ) ;
                   end ;
                 end ;
            TGIS_FieldType.Boolean :
                 begin
                   if not VarIsNull( v ) then begin
                     try
                       {$IFNDEF OXYGENE}
                         bl := v ;
                       {$ELSE}
                         bl := VarToBoolean( v ) ;
                       {$ENDIF}
                     except
                       bl := False;
                     end ;
                     if bl then
                       _csvfile.WriteString( '"T"' )
                     else
                       _csvfile.WriteString( '"F"' ) ;
                   end ;
                 end ;
            TGIS_FieldType.Number :
                 begin
                   if not VarIsNull( v ) then begin
                     try
                       {$IFNDEF OXYGENE}
                         fl := v ;
                       {$ELSE}
                         fl := VarToDouble( v ) ;
                       {$ENDIF}
                     except
                       fl := 0;
                     end ;
                     stmp := Trim( Format( '%*.*f',
                                          [ fld.NewWidth, fld.NewDecimal, fl ]
                                        )
                                ) ;
                     stmp := StringReplace( stmp, FDecimalPoint, '.', [] ) ;
                     _csvfile.WriteString( stmp ) ;
                   end ;
                 end ;
            TGIS_FieldType.Float :
                 begin
                   if not VarIsNull( v ) then begin
                     try
                       {$IFNDEF OXYGENE}
                         fl := v ;
                       {$ELSE}
                         fl := VarToDouble( v ) ;
                       {$ENDIF}
                     except
                       fl := 0;
                     end ;
                     _csvfile.WriteString( localFloatToStr( fl ) ) ;
                   end ;
                 end ;
            else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '', 0 ) ;
          end ;
        end;

        if field_no < cnt -1 then _csvfile.WriteString( FSeparator )
                             else _csvfile.WriteLine  ( ''  ) ;
      end ;
    end ;

  begin
    assert( assigned( _csvshp  ) ) ;
    assert( assigned( _fullshp ) ) ;

    _csvshp.Lock( TGIS_Lock.Projection ) ;
    try
      ptg   := _csvshp.Centroid   ;
      ptg_z := _csvshp.PointsZMax ;

      csv_write ;
    finally
      _csvshp.Unlock ;
    end ;
  end ;

  procedure TGIS_LayerCSV.setUp ;

    function _t( const _val1 : String; const _val2 : String ) : Boolean ;
    begin
      Result := CompareText( _val1, _val2 ) = 0 ;
    end ;

    function _countChar( const _text : String ; const _c : Char ) : Integer ;
    var
      i : Integer ;
    begin
      Result := 0 ;
      for i := StringFirst to StringLast(_text) do
        if _text[i] = _c then
          inc(Result);
    end ;

    procedure analyze_layer ;
    var
      i, cnt     : Integer ;
      line       : String  ;
      oldline    : String  ;
      abort      : Boolean ;
      tkn        : TGIS_Tokenizer ;
      icoma      : Integer ;
      isemicolon : Integer ;
      itab       : Integer ;
      ipipe      : Integer ;
      lst        : TStringList ;
    begin
      lst := TStringList.Create ;
      lst.LoadFromFile( Path ) ;
      try
        csvLineNo := 0 ;
        oldline := '' ;

        tkn := TGIS_Tokenizer.Create ;
        try
          while csvLineNo < lst.Count  do begin
            if not IsStringEmpty( oldline ) then
              line := oldline + lst[ csvLineNo ]
            else
              line := lst[ csvLineNo ] ;

            inc( csvLineNo ) ;
            if csvLineNo mod 100 = 1 then begin
              abort := RaiseBusyShake( Self,
                                       csvLineNo div 2,
                                       lst.Count
                                      ) ;
              if abort then break ;
            end ;

            if IsStringEmpty( line ) then continue ;

            if csvLineNo = 1 then begin
              if FAutoRecognize then begin
                tkn.ExecuteEx( line, ',' ) ;
                icoma := tkn.Result.Count ;

                tkn.ExecuteEx( line, ';' ) ;
                isemicolon := tkn.Result.Count ;

                tkn.ExecuteEx( line, #9 ) ;
                itab := tkn.Result.Count ;

                tkn.ExecuteEx( line, '|' ) ;
                ipipe := tkn.Result.Count ;

                if itab > 1 then
                  FSeparator := #9
                else
                if ipipe > 1 then
                  FSeparator := '|'
                else
                if icoma > isemicolon then begin
                  FSeparator := ',' ;
                  FDecimalPoint := '.';
                end
                else
                  FSeparator := ';'
              end ;

              tkn.ExecuteEx( line, FSeparator ) ;

              if ( tkn.Result.Count > 2 ) and
                 ( ( UpperCase( tkn.Result[2] ) = '"Z"' ) or
                   ( UpperCase( tkn.Result[2] ) = 'Z'   )
                 ) then
                DefaultDimension := TGIS_DimensionType.XYZ
              else
                DefaultDimension := TGIS_DimensionType.XY ;
              SetLength( ftypes, tkn.Result.Count ) ;
              for i:= 0 to tkn.Result.Count - 1 do begin
                ftypes[i] := TGIS_FieldType.Number ;
              end ;
            end
            else begin
              cnt := _countChar( line, '"' ) ;
              oldline := '' ;
              if (cnt mod 2) = 0 then
                analyzeShape( line )
              else
                oldline := line ;
            end ;
          end ;
        finally
          FreeObject( tkn ) ;
        end;
      finally
        FreeObject( lst ) ;
      end ;
    end ;

    procedure read_layer ;
    var
      i, cnt  : Integer ;
      fname   : String  ;
      line    : String  ;
      oldline : String  ;
      abort   : Boolean ;
      tkn     : TGIS_Tokenizer ;
      lst     : TStringList    ;
    begin
      lst := TStringList.Create ;
      lst.LoadFromFile( Path ) ;
      try
        csvLineNo := 0 ;
        iDim := 2 ;
        oldline := '' ;

        tkn := TGIS_Tokenizer.Create ;
        try
          while csvLineNo < lst.Count  do begin
            if not IsStringEmpty( oldline ) then
              line := oldline + lst[ csvLineNo ]
            else
              line := lst[ csvLineNo ] ;

            inc( csvLineNo ) ;
            if csvLineNo mod 100 = 1 then begin
              abort := RaiseBusyShake( Self,
                                       csvLineNo div 2 + lst.Count div 2,
                                       lst.Count
                                      ) ;
              if abort then break ;
            end ;

            if IsStringEmpty( line ) then continue ;

            if csvLineNo = 1 then begin
              tkn.ExecuteEx( line, FSeparator ) ;
              for i:= 0 to tkn.Result.Count - 1 do begin
                fname := tkn.Result[i] ;

                if      _t( ''      , fname ) then fname := Format(CSV_UNKNOWN,[i])
                else if _t( CSV_X   , fname ) then fname := CSV_X
                else if _t( CSV_LON , fname ) then fname := CSV_LON
                else if _t( CSV_LON2, fname ) then fname := CSV_LON
                else if _t( CSV_LON3, fname ) then fname := CSV_LON
                else if _t( CSV_LON4, fname ) then fname := CSV_LON
                else if _t( CSV_Y   , fname ) then fname := CSV_Y
                else if _t( CSV_LAT , fname ) then fname := CSV_LAT
                else if _t( CSV_LAT2, fname ) then fname := CSV_LAT
                else if _t( CSV_LAT3, fname ) then fname := CSV_LAT
                else if _t( CSV_LAT4, fname ) then fname := CSV_LAT
                else if _t( CSV_Z   , fname ) then fname := CSV_Z
                else if _t( CSV_Z2  , fname ) then fname := CSV_Z
                else if _t( CSV_Z3  , fname ) then fname := CSV_Z
                else

                fname := self.GetUniqueFieldName( fname ) ;

                case ftypes[i] of
                  TGIS_FieldType.Number :
                    AddFieldInternal(
                      fname,
                      TGIS_FieldType.Number, 20, 0
                    ) ;
                  TGIS_FieldType.Float  :
                    AddFieldInternal(
                      fname,
                      TGIS_FieldType.Float, 0, 0
                    ) ;
                  else
                    AddFieldInternal(
                      fname,
                      TGIS_FieldType.String, 1, 0
                    ) ;
                end ;

                if      _t( CSV_X, fname ) then
                        begin
                          with FieldInfo( FindField( CSV_X ) ) do begin
                            FileFormat  := True ;
                            {$IFDEF OXYGENE}
                              &ReadOnly := True ;
                            {$ELSE}
                              ReadOnly  := True ;
                            {$ENDIF}
                            Hidden      := True ;
                          end ;
                        end
                else if _t( CSV_Y, fname ) then
                        begin
                          with FieldInfo( FindField( CSV_Y ) ) do begin
                            FileFormat  := True ;
                            {$IFDEF OXYGENE}
                              &ReadOnly := True ;
                            {$ELSE}
                              ReadOnly  := True ;
                            {$ENDIF}
                            Hidden      := True ;
                          end ;
                        end
                else if _t( CSV_Z, fname ) then
                        begin
                          with FieldInfo( FindField( CSV_Z ) ) do begin
                            FileFormat  := True ;
                            {$IFDEF OXYGENE}
                              &ReadOnly := True ;
                            {$ELSE}
                              ReadOnly  := True ;
                            {$ENDIF}
                            Hidden      := True ;
                          end ;
                          iDim := 3 ;
                        end
                else if _t( CSV_LON, fname ) then
                        begin
                          if FindField( CSV_X ) < 0  then
                            with FieldInfo( FindField( CSV_LON ) ) do begin
                              FileFormat  := True ;
                              {$IFDEF OXYGENE}
                                &ReadOnly := True ;
                              {$ELSE}
                                ReadOnly  := True ;
                              {$ENDIF}
                              Hidden      := True ;
                            end ;
                        end
                else if _t( CSV_LAT, fname ) then
                        begin
                          if FindField( CSV_Y ) < 0  then
                            with FieldInfo( FindField( CSV_LAT ) ) do begin
                              FileFormat  := True ;
                              {$IFDEF OXYGENE}
                                &ReadOnly := True ;
                              {$ELSE}
                                ReadOnly  := True ;
                              {$ENDIF}
                              Hidden      := True ;
                            end ;
                       end ;
              end ;
            end
            else begin
              cnt := _countChar( line, '"' ) ;
              oldline := '' ;
              if ((cnt mod 2) = 0) or (csvLineNo = lst.Count) then
                readShape( line )
              else
                oldline := line + #13#10 ;
            end ;
          end ;
        finally
          FreeObject( tkn ) ;
        end;
      finally
        FIsModified := False ;

        FreeObject( lst ) ;

        FAutoRecognize := False ;
      end ;
    end;

  begin
    inherited ;

    FSupportedShapes :=  GisGetShapeType( TGIS_ShapeType.Point ) ;

    if not IsStringEmpty( Path ) and SafeFileExists( Path ) then begin
      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
      Lock ;
      try
        analyze_layer ;
        read_layer ;
      finally
        Unlock ;
        RaiseBusyRelease( Self ) ;
      end;
    end ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'Comma Separated Values point layer (CSV)' ;
  end ;

  procedure TGIS_LayerCSV.Build( const _path   : String ;
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

  procedure TGIS_LayerCSV.ImportLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent ;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape ;
    const _de9im     : String ;
    const _truncated : Boolean
  ) ;
  var
    shape_file : TGIS_BufferedFileStream    ;
    {$IFDEF DCC}
      shp      : TGIS_Shape  ;
    {$ENDIF}
    shp_tmp    : TGIS_Shape  ;
    ex         : TGIS_Extent ;
    shape_no   : Cardinal    ;
    end_uid    : TGIS_Uid     ;
    abort      : Boolean     ;
    old_scope  : String      ;
  begin
    if not assigned( _layer ) then exit ;

    if not CheckFileWriteAccessEx( Path, True, True, True ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try
      // add file format fields
      if FindField( CSV_X ) < 0 then begin
        if FindField( CSV_LON ) < 0 then begin
          AddFieldInternal( CSV_X, TGIS_FieldType.String, 1, 0 ) ;
          with FieldInfo( FindField( CSV_X ) ) do begin
            FileFormat  := True ;
            {$IFDEF OXYGENE}
              &ReadOnly := True ;
            {$ELSE}
              ReadOnly  := True ;
            {$ENDIF}
            Hidden      := True ;
          end ;
        end ;
      end ;

      if FindField( CSV_Y ) < 0 then begin
        if FindField( CSV_LAT ) < 0 then begin
          AddFieldInternal( CSV_Y, TGIS_FieldType.String, 1, 0 ) ;
          with FieldInfo( FindField( CSV_Y ) ) do begin
            FileFormat  := True ;
            {$IFDEF OXYGENE}
              &ReadOnly := True ;
            {$ELSE}
              ReadOnly  := True ;
            {$ENDIF}
            Hidden      := True ;
          end ;
        end ;
      end ;

      if (( _layer.DefaultDimension = TGIS_DimensionType.XYZ  ) or
          ( _layer.DefaultDimension = TGIS_DimensionType.XYZM )) and
         ( FindField( CSV_Z )  < 0                   ) then begin
        AddFieldInternal( CSV_Z, TGIS_FieldType.String, 1, 0 ) ;
        with FieldInfo( FindField( CSV_Z ) ) do begin
          FileFormat  := True ;
          {$IFDEF OXYGENE}
            &ReadOnly := True ;
          {$ELSE}
            ReadOnly  := True ;
          {$ENDIF}
          Hidden      := True ;
        end ;
      end ;

      ImportStructure( _layer ) ;

      PrepareExportFieldNames( 32 ) ;
      ExportStructureToFLD ;

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

      ex := GisCommonExtent( _layer.Extent, _extent ) ;

      writeHeader( shape_file ) ;

      try
        old_scope := _layer.Scope ;
        _layer.Scope := '' ;

        for shp {$IFNDEF DCC}: TGIS_Shape{$ENDIF}
            in _layer.Loop( ex, _scope, _shape, _de9im )
        do begin
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
              writeShape( shape_file, shp_tmp, shp ) ;
            end ;
          finally
            if shp <> shp_tmp then FreeObject( shp_tmp ) ;
          end ;

          if shape_no mod 100 = 1 then begin
            abort := RaiseBusyEvent( _layer, shp.Uid, end_uid ) ;
            if abort then break ;
          end ;
          inc( shape_no ) ;
        end ;
      finally
        _layer.Scope := old_scope ;

        FreeObject( shape_file ) ;

        if abort then begin
          DeleteFile( GetTemporaryName( Path ) ) ;
        end
        else begin
          DeleteFile( GetBackupName( Path ) ) ;

          RenameFile( Path, GetBackupName( Path ) ) ;

          try
            if not RenameFile( GetTemporaryName( Path ), Path ) then
              raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), Path, GetLastError ) ;
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

  procedure TGIS_LayerCSV.SaveData  ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerCSV.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-CSV', 'Comma Separated Values point layer (CSV)',
                   TGIS_LayerCSV, '.csv',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                    True
                  ) ;
   end;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerCSV.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

