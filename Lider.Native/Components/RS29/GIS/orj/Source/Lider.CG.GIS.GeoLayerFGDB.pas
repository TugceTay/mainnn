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
  Encapsulation of an Esri File Geodatabase API file access for vector.
}

{$IFDEF DCC}
  unit GisLayerFGDB ;
  {$HPPEMIT '#pragma link "GisLayerFGDB"'}
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
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisTypes,
    GisLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerFGDB = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {$IFDEF OXYGENE}
    T_cursorFGDB nested in TGIS_LayerFGDB = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public
        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse        : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record-by-record access.
        /// </summary>
        currShape       : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating the shape on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currPoint       : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currArc         : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///  faster than full Create constructor.
        /// </summary>
        currPolygon     : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currMultipoint  : TGIS_ShapeMultiPoint  ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currMultiPatch  : TGIS_ShapeMultiPatch ;

        /// <summary>
        ///   True if this is the first element.
        /// </summary>
        curFirst        : Boolean ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Layer that can read and write vector from Esri File Geodatabase API file.
  /// </summary>
  TGIS_LayerFGDB = {$IFDEF OXYGENE} public {$ENDIF}
                   class( TGIS_LayerVector )
    private

        /// <summary>
        ///   Last UID in a shape file. If not set then -1.
        /// </summary>
        FLastUid        : TGIS_Uid ;
        /// <summary>
        ///   Field name with OID.
        /// </summary>
        FOIDFieldName   : String ;
        /// <summary>
        ///   Field name with geometry.
        /// </summary>
        FShapeFieldName : String ;
    private
    {$IFDEF OXYGENE}
      cursorFGDB : array of T_cursorFGDB ;
    {$ELSE}
      cursorFGDB : array of record
        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse        : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record-by-record access.
        /// </summary>
        currShape       : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating the shape on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currPoint       : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currArc         : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///  faster than full Create constructor.
        /// </summary>
        currPolygon     : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currMultipoint  : TGIS_ShapeMultiPoint  ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currMultiPatch  : TGIS_ShapeMultiPatch ;

        /// <summary>
        ///   True if this is the first element.
        /// </summary>
        curFirst        : Boolean ;
      end ;
    {$ENDIF}
    protected // other protected functions
      /// <summary>
      ///   File handle.
      /// </summary>
      FGDB : TObject ;
    private // private methods

      /// <summary>
      ///   Parse geometry.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_buf">
      ///   geometry buffer
      /// </param>
      /// <param name="_size">
      ///   buffer size
      /// </param>
      /// <param name="_uid">
      ///   shape uid
      /// </param>
      procedure parseGeometry ( const _cursor  : Integer ;
                                {$IFDEF OXYGENE}
                                const _buf     : tgdbptr  ;
                                {$ELSE}
                                const _buf     : IntPtr  ;
                                {$ENDIF}
                                const _size    : Integer ;
                                const _uid     : TGIS_Uid
                               ) ;

      /// <summary>
      ///   Parse dataset info.
      /// </summary>
      /// <param name="_info">
      ///   layer info text
      /// </param>
      procedure parseInfo     ( const _info    : String ) ;

      /// <summary>
      ///   Fetch row with given uid.
      /// </summary>
      /// <param name="_uid">
      ///   shape uid
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure fetchRow      ( const _uid     : TGIS_Uid ;
                                const _cursor  : Integer
                               ) ;

      /// <summary>
      ///   Alter table structure.
      /// </summary>
      /// <param name="_layer">
      ///   layer with fields
      /// </param>
      procedure doTableAlter  ( const _layer  : TGIS_LayerVector
                               ) ;

      /// <summary>
      ///   Write shape.
      /// </summary>
      /// <param name="_shp">
      ///   shape
      /// </param>
      /// <param name="_import">
      ///   if True, do import, otherwise update shape
      /// </param>
      procedure doShapeUpdate ( const _shp     : TGIS_Shape ;
                                const _import  : Boolean
                              ) ;


    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure fset_UseRTree         ( const _value : Boolean
                                         ) ; override;

         /// <inheritdoc/>
         function  getFieldInternal      ( const _uid      : TGIS_Uid  ;
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
    protected
      // destructor

         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  PreRecognize    ( const _path     : String ;
                                    var _new_path : String
                                ) : Boolean ; override;

      /// <inheritdoc/>
      procedure Build      ( const _path        : String            ;
                             const _extent      : TGIS_Extent       ;
                             const _type        : TGIS_ShapeType    ;
                             const _dim         : TGIS_DimensionType
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

      // shape access function(s)

      /// <inheritdoc/>
      function  GetShape   ( const _uid    : TGIS_Uid   ;
                             const _cursor : Integer
                           ) : TGIS_Shape ; override;

      /// <inheritdoc/>
      function  GetLastUid : TGIS_Uid ; override;

      /// <inheritdoc/>
      function  GetNewUid  : TGIS_Uid ; override;

      /// <inheritdoc/>
      function GetAvailableLayers : TGIS_LayerInfoList ; override;

      /// <inheritdoc/>
      procedure RevertShapes ; override;

  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.SysUtils,
    System.Variants,
    System.Math,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.SyncObjs,
    GisRtl,
    GisFunctions,
    GisLayer,
    GisInternals,
    GisClasses,
    GisResource,
    GisRegistredLayers,
    GisFileFGDB,
    GisFieldRules,
    GisGeometryFactory,
    GisXmlDoc ;
{$ENDIF}

//=============================================================================
// TGIS_LayerFGDB
//=============================================================================

  constructor TGIS_LayerFGDB.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable ] ;
  end ;

  procedure TGIS_LayerFGDB.doDestroy ;
  var
    i : Integer ;
  begin
    for i := 0 to high( cursorFGDB ) do begin
      FreeObject( cursorFGDB[i].currPoint      ) ;
      FreeObject( cursorFGDB[i].currMultipoint ) ;
      FreeObject( cursorFGDB[i].currArc        ) ;
      FreeObject( cursorFGDB[i].currPolygon    ) ;
      FreeObject( cursorFGDB[i].currMultiPatch ) ;
    end ;

    for i := length( cursorFGDB ) - 1 downto 0 do
      cursorClose( i ) ;

    inherited ;

    if assigned( FGDB ) then
      TGIS_FileFGDB(FGDB).CloseDatabase ;

    FreeObject( FGDB ) ;
  end ;

  procedure TGIS_LayerFGDB.parseGeometry(
    const _cursor : Integer ;
    {$IFDEF OXYGENE}
    const _buf     : tgdbptr  ;
    {$ELSE}
    const _buf     : IntPtr  ;
    {$ENDIF}
    const _size   : Integer ;
    const _uid    : TGIS_Uid
  ) ;
  var
    ptrvar : TGIS_Bytes ;
  begin
    cursorFGDB[_cursor].currShape := nil ;

    {$IFDEF OXYGENE}
      {$IFDEF CLR}
        ptrvar := TGIS_Bytes.Create( _buf, 0, _size ) ;
      {$ELSE}
        var buf := new Byte[_size] ;
        _buf.read( 0, buf, 0, _size ) ;
        ptrvar := TGIS_Bytes.Create( buf, 0, _size ) ;
      {$ENDIF}
    {$ELSE}
      ptrvar := TGIS_Bytes.Create( Pointer(_buf), 0, _size ) ;
    {$ENDIF}
    try
      try
        cursorFGDB[_cursor].currShape := TGIS_GeometryFactory.GisCreateShapeFromShapeEx( ptrvar ) ;
      except
        cursorFGDB[_cursor].currShape := nil ;
      end ;
    finally
      FreeObject( ptrvar ) ;
    end ;

    if not assigned( cursorFGDB[_cursor].currShape ) then exit ;

    case cursorFGDB[_cursor].currShape.ShapeType of
      TGIS_ShapeType.Point :
         begin
           cursorFGDB[_cursor].currPoint.Recreate(
              cursorFGDB[_cursor].currShape, nil, False, _uid, self
            ) ;
           FreeObject( cursorFGDB[_cursor].currShape ) ;
           cursorFGDB[_cursor].currShape := cursorFGDB[_cursor].currPoint ;
         end ;
      TGIS_ShapeType.MultiPoint :
         begin
           cursorFGDB[_cursor].currMultipoint.Recreate(
              cursorFGDB[_cursor].currShape, nil, False, _uid, self
           ) ;
           FreeObject( cursorFGDB[_cursor].currShape ) ;
           cursorFGDB[_cursor].currShape := cursorFGDB[_cursor].currMultipoint ;
         end ;
      TGIS_ShapeType.Arc :
         begin
           cursorFGDB[_cursor].currArc.Recreate(
              cursorFGDB[_cursor].currShape, nil, False, _uid, self
           ) ;
           FreeObject( cursorFGDB[_cursor].currShape ) ;
           cursorFGDB[_cursor].currShape := cursorFGDB[_cursor].currArc ;
         end ;
      TGIS_ShapeType.Polygon :
         begin
           cursorFGDB[_cursor].currPolygon.Recreate(
              cursorFGDB[_cursor].currShape, nil, False, _uid, self
            ) ;
           FreeObject( cursorFGDB[_cursor].currShape ) ;
           cursorFGDB[_cursor].currShape := cursorFGDB[_cursor].currPolygon ;
         end ;
      TGIS_ShapeType.MultiPatch :
         begin
           cursorFGDB[_cursor].currMultiPatch.Recreate(
              cursorFGDB[_cursor].currShape, nil, False, _uid, self
            ) ;
           FreeObject( cursorFGDB[_cursor].currShape ) ;
           cursorFGDB[_cursor].currShape := cursorFGDB[_cursor].currMultiPatch ;
         end ;
      else
           cursorFGDB[_cursor].currShape := nil ;
    end ;

    cursorFGDB[_cursor].currShape := getEdited( cursorFGDB[_cursor].currShape ) ;
  end ;

  procedure TGIS_LayerFGDB.parseInfo(
    const _info : String
  ) ;
  var
    xDoc  : IXMLDocument ;
    xroot : IXMLNode ;
    xnode : IXMLNode ;
    xchild: IXMLNode ;
    xfield: IXMLNode ;
    i     : Integer ;
    flen,
    fprec : Integer ;
    fname : String ;
    ftype : String ;
    rule  : TGIS_FieldRule ;

      procedure createfield ;
      begin
        if (ftype = 'esriFieldTypeSmallInteger') then
          AddFieldInternal( fname, TGIS_FieldType.Number, 5, fprec )
        else if (ftype = 'esriFieldTypeInteger') then
          AddFieldInternal( fname, TGIS_FieldType.Number, 10, fprec )
        else if (ftype = 'esriFieldTypeSingle') or
                (ftype = 'esriFieldTypeDouble') then
          AddFieldInternal( fname, TGIS_FieldType.Float, flen, fprec )
        else if (ftype = 'esriFieldTypeString') or
                (ftype = 'esriFieldTypeXML') or
                (ftype = 'esriFieldTypeGUID') or
                (ftype = 'esriFieldTypeGlobalID') then begin
          if flen = 0 then
            flen := 1 ;
          AddFieldInternal( fname, TGIS_FieldType.String, flen, fprec )
        end
        else if (ftype = 'esriFieldTypeDate')  then
          AddFieldInternal( fname, TGIS_FieldType.Date, flen, fprec )
        else if (ftype = 'esriFieldTypeOID') then begin
          AddFieldInternal( fname, TGIS_FieldType.Number, flen, fprec ) ;
          if IsStringEmpty( FOIDFieldName ) then
            FOIDFieldName := fname ;
          with FieldInfo( Fields.Count -1 ) do begin
            FileFormat  := True ;
            {$IFDEF OXYGENE}
              &ReadOnly := True ;
            {$ELSE}
              ReadOnly  := True ;
            {$ENDIF}
            Hidden      := True ;
            IsUID       := True ;
          end ;
        end ;

        if assigned( rule ) then
          with FieldInfo( Fields.Count -1 ) do
            Rules := rule ;
      end ;

      procedure parsegeodef( const _xgeodef : IXMLNodeList ) ;
      var
        p     : Integer ;
        gtype : String ;
        hasz  : Boolean ;
        hasm  : Boolean ;
        wkt   : String ;
      begin
        hasz := False ;
        hasm := False ;
        wkt  := '' ;

        for p := 0 to _xgeodef.Count-1 do begin
          if _xgeodef[p].NodeName = 'GeometryType' then
            gtype := VarToString( _xgeodef[p].NodeValue )
          else if _xgeodef[p].NodeName = 'SpatialReference' then
            wkt := VarToString(_xgeodef[p].ChildNodes.Nodes['WKT'].NodeValue)
          else if _xgeodef[p].NodeName = 'HasM' then
            hasm := VarToBoolean( _xgeodef[p].NodeValue )
          else if _xgeodef[p].NodeName = 'HasZ' then
            hasz := VarToBoolean( _xgeodef[p].NodeValue )
        end ;

        SetCSByWKT( wkt ) ;

        if hasz or hasm then begin
          if hasz and hasm then
            DefaultDimension := TGIS_DimensionType.XYZM
          else if hasz and not hasm then
            DefaultDimension := TGIS_DimensionType.XYZ
          else if not hasz and hasm then
            DefaultDimension := TGIS_DimensionType.XYM
        end
        else
          DefaultDimension := TGIS_DimensionType.XY ;

        if ( gtype = 'esriGeometryPoint' ) then
          DefaultShapeType := TGIS_ShapeType.Point
        else if ( gtype = 'esriGeometryMultipoint' ) then
          DefaultShapeType := TGIS_ShapeType.MultiPoint
        else if ( gtype = 'esriGeometryLine' ) then
          DefaultShapeType := TGIS_ShapeType.Arc
        else if ( gtype = 'esriGeometryPolyline' ) then
          DefaultShapeType := TGIS_ShapeType.Arc
        else if ( gtype = 'esriGeometryPolygon' ) then
          DefaultShapeType := TGIS_ShapeType.Polygon
        else if ( gtype = 'esriGeometryMultiPatch' ) then
          DefaultShapeType := TGIS_ShapeType.MultiPatch
        else
          DefaultShapeType := TGIS_ShapeType.Unknown ;
      end ;

      procedure parseDomain( const _domain : IXMLNodeList ) ;
      var
        p, r  : Integer ;
        pnode : IXMLNode ;
        name  : String ;
        code  : String ;
      begin
        for p := 0 to _domain.Count-1 do begin
          if _domain[p].NodeName = 'CodedValues' then begin
            rule := TGIS_FieldRule.Create ;
            for r := 0 to _domain[p].ChildNodes.Count-1 do begin
              pnode := _domain[p].ChildNodes[r] ;
              if pnode.NodeName = 'CodedValue' then begin
                name := VarToString( pnode.ChildNodes.Nodes['Name'].NodeValue ) ;
                code := VarToString( pnode.ChildNodes.Nodes['Code'].NodeValue ) ;
                if not IsStringEmpty( name ) then
                  rule.ValueAliases.Aliases.Add( TGIS_FieldValueAlias.Create( name, code ) ) ;
              end ;
            end ;
          end ;
        end ;
      end ;

      procedure parsefields( const _fields : IXMLNode ) ;
      var
        j,k :Integer  ;
      begin
        for j := 0 to _fields.ChildNodes.Count-1 do begin
          xchild := _fields.ChildNodes[j] ;
          if xchild.NodeName = 'Field' then begin
            flen  := 0 ;
            fprec := 0 ;
            rule  := nil ;

            for k := 0 to xchild.ChildNodes.Count-1 do begin
              xfield := xchild.ChildNodes[k] ;
              if xfield.NodeName = 'Name' then
                fname := VarToString( xfield.NodeValue )
              else if xfield.NodeName = 'Type' then
                ftype := VarToString( xfield.NodeValue )
              else if xfield.NodeName = 'Length' then
                // Length is in bytes
                flen := VarToInt32( xfield.NodeValue )
              else if xfield.NodeName = 'Precision' then
                fprec := VarToInt32( xfield.NodeValue )
              else if xfield.NodeName = 'GeometryDef' then begin
                parsegeodef( xfield.ChildNodes ) ;
              end
              else if xfield.NodeName.StartsWith('Domain') then
                parseDomain( xfield.ChildNodes ) ;
            end ;
            createfield ;
          end ;
        end ;
      end ;

  begin
    xDoc := TGIS_XMLDocument.Create ;
    try
      xDoc.Active    := True ;
      xDoc.Version   := '1.0' ;
      xDoc.Encoding  := 'utf-8' ;
      xDoc.LoadFromXML( _info ) ;

      xroot := xDoc.DocumentElement ;

      for i := 0 to xroot.ChildNodes.Count-1 do begin
        xnode := xroot.ChildNodes[i] ;

        if xnode.NodeName = 'OIDFieldName' then
          FOIDFieldName := VarToString( xnode.NodeValue )
        else if xnode.NodeName = 'ShapeFieldName' then
          FShapeFieldName := VarToString( xnode.NodeValue )
        else if xnode.NodeName = 'Fields' then begin
          xnode := xnode.ChildNodes[0] ;
          parsefields( xnode ) ;
        end
        else if xnode.NodeName = 'FieldArray' then begin
          parsefields( xnode ) ;
        end ;

      end ;

    finally
      FreeObject( xDoc ) ;
    end ;
  end ;

  procedure TGIS_LayerFGDB.fetchRow(
    const _uid     : TGIS_Uid ;
    const _cursor  : Integer
   ) ;
  var
    oid : Integer ;
    ex  : TGIS_Extent ;
  begin
    oid := TGIS_FileFGDB(FGDB).GetOID( _cursor ) ;

    if oid <> _uid then begin
      ex := GisWholeWorld ;
      TGIS_FileFGDB(FGDB).CursorFirst(
          _cursor, ex.XMin, ex.XMax, ex.YMin, ex.YMax,
          Format( '%s=%d', [FOIDFieldName, _uid] )
       ) ;
    end ;

  end ;

  function TGIS_LayerFGDB.getFieldInternal(
    const _uid      : TGIS_Uid  ;
    const _name     : String ;
    const _cursor   : Integer
  ) : Variant ;
  var
    nr  : Integer ;
    fld : TGIS_FieldInfo ;
    val : String ;
  begin
    lockThread ;
    try
      fetchRow( _uid, _cursor ) ;

      nr  := FindFieldInternal( _name ) ;
      fld := FieldInfo( nr ) ;

      if TGIS_FileFGDB(FGDB).GetIsNull( _cursor, _name ) then begin
        Result := NullVar ;
        exit ;
      end ;

      case fld.FieldType of
        TGIS_FieldType.String  :
          Result := TGIS_FileFGDB(FGDB).GetString( _cursor, _name, fld.Width ) ;
        TGIS_FieldType.Number  :
          if fld.Decimal = 0 then
            Result := TGIS_FileFGDB(FGDB).GetInteger( _cursor, _name )
          else
            Result := TGIS_FileFGDB(FGDB).GetDouble( _cursor, _name ) ;
        TGIS_FieldType.Float   :
          if fld.Width = 4 then
            Result := TGIS_FileFGDB(FGDB).GetFloat( _cursor, _name )
          else
            Result := TGIS_FileFGDB(FGDB).GetDouble( _cursor, _name ) ;
        TGIS_FieldType.Boolean :
          Result := TGIS_FileFGDB(FGDB).GetString( _cursor, _name, fld.Width ) ;
        TGIS_FieldType.Date    : begin
          val := TGIS_FileFGDB(FGDB).GetDateTime( _cursor, _name ) ;
          if IsStringEmpty( val ) then
            Result := Unassigned
          else
            {$IFDEF OXYGENE}
              Result := StrToDateTime( val ) ;
            {$ELSE}
              Result := System.Variants.VarToDateTime( val ) ;
            {$ENDIF}
        end ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerFGDB.getBindedFieldInternal(
    const _shape  : TObject ;
    const _field  : Integer ;
    const _cursor : Integer
  ) : Variant ;
  var
    shp : TGIS_Shape ;
    fld : TGIS_FieldInfo ;
    val : String ;
  begin
    lockThread ;
    try
      shp := TGIS_Shape( _shape ) ;
      if assigned( shp ) then begin
        fetchRow( shp.Uid, _cursor ) ;
        fld := FieldInfo( _field ) ;

        case fld.FieldType of
          TGIS_FieldType.String  :
            Result := TGIS_FileFGDB(FGDB).GetString( _cursor, fld.Name, fld.Width ) ;
          TGIS_FieldType.Number  :
            if fld.Decimal = 0 then
              Result := TGIS_FileFGDB(FGDB).GetInteger( _cursor, fld.Name )
            else
              Result := TGIS_FileFGDB(FGDB).GetDouble( _cursor, fld.Name ) ;
          TGIS_FieldType.Float   :
            if fld.Width = 4 then
              Result := TGIS_FileFGDB(FGDB).GetFloat( _cursor, fld.Name )
            else
              Result := TGIS_FileFGDB(FGDB).GetDouble( _cursor, fld.Name ) ;
          TGIS_FieldType.Boolean :
            Result := TGIS_FileFGDB(FGDB).GetString( _cursor, fld.Name, fld.Width ) ;
          TGIS_FieldType.Date    : begin
            val := TGIS_FileFGDB(FGDB).GetDateTime( _cursor, fld.Name ) ;
            if IsStringEmpty( val ) then
              Result := Unassigned
            else
              {$IFDEF OXYGENE}
                Result := StrToDateTime( val ) ;
              {$ELSE}
                Result := System.Variants.VarToDateTime( val ) ;
              {$ENDIF}
          end ;
        end ;
      end
      else
        Result := Unassigned
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerFGDB.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_LayerFGDB.setUp ;
  var
    i : Integer ;
  begin
    inherited ;

    FLastUid := -1 ;
    if not assigned( FGDB ) then
      FGDB := TGIS_FileFGDB.Create ;

    TGIS_FileFGDB(FGDB).OpenDatabase( Path, Name ) ;
    TGIS_FileFGDB(FGDB).GetLayer ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      TGIS_FileFGDB(FGDB).CursorOpen( i ) ;

    parseInfo( TGIS_FileFGDB(FGDB).LayerInfo ) ;

    Extent := TGIS_FileFGDB(FGDB).LayerExtent ;

    FFileInfo := 'Esri File Geodatabase API for vector' ;
  end ;

  function  TGIS_LayerFGDB.cursorOpen : Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length( cursorFGDB )  then
        SetLength( cursorFGDB, Result + 1 ) ;

      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorFGDB[Result] ) then
          cursorFGDB[Result] := new T_cursorFGDB ;
      {$ENDIF}
      cursorFGDB[Result].curInUse := True ;

      cursorFGDB[Result].currPoint      := TGIS_ShapePoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorFGDB[Result].currMultipoint := TGIS_ShapeMultiPoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorFGDB[Result].currArc        := TGIS_ShapeArc.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorFGDB[Result].currPolygon    := TGIS_ShapePolygon.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorFGDB[Result].currMultiPatch := TGIS_ShapeMultiPatch.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      if assigned( FGDB ) then
        TGIS_FileFGDB(FGDB).CursorOpen( Result ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerFGDB.cursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorFGDB[_cursor].curInUse := False ;
      FreeObject( cursorFGDB[_cursor].currPoint ) ;
      FreeObject( cursorFGDB[_cursor].currMultipoint ) ;
      FreeObject( cursorFGDB[_cursor].currArc ) ;
      FreeObject( cursorFGDB[_cursor].currPolygon ) ;
      FreeObject( cursorFGDB[_cursor].currMultiPatch ) ;

      // truncate cursorState at the tail;
      for i := length( cursorFGDB ) - 1 downto 0 do begin
        if not cursorFGDB[i].curInUse then begin
          SetLength( cursorFGDB, i ) ;
        end
        else
          break ;
      end ;

      if assigned( FGDB ) then
        TGIS_FileFGDB(FGDB).CursorClose( _cursor ) ;

      inherited cursorClose( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerFGDB.cursorFirst(
     const _cursor      : Integer          ;
     const _viewerCS    : Boolean          ;
     const _extent      : TGIS_Extent      ;
     const _query       : String           ;
     const _shape       : TGIS_Shape       ;
     const _de9im       : String           ;
     const _skipDeleted : Boolean
   ) ;
  var
    sqlquery    : String ;
    ex          : TGIS_Extent ;
    full_search : Boolean ;
  begin
    lockThread ;
    try
      cursorFGDB[_cursor].currShape := nil ;

      if GisIsNoWorld( _extent ) then
        exit ;

      inherited cursorFirstInternal(
                  _cursor, _viewerCS,
                  _extent, _query, _shape, _de9im, _skipDeleted
                ) ;

      sqlquery    := ReplaceSQLToken( sqlquery, GIS_FIELD_UID, FOIDFieldName ) ;
      full_search := TestSQLTokens( sqlquery, GIS_FIELDS_PREDEFINED ) ;

      if Pos( GIS_FIELD_JOINPREFIX, sqlquery ) >= StringFirst then
        full_search := True ;

      if full_search then
        sqlquery := '' ;  // attribute query should be done on user side.

      ex := GisCommonExtent( cursorState[ _cursor ].curExtent,
                             GisExtent( -1E37, -1E37, 1E37, 1E37 )
                           ) ;

      TGIS_FileFGDB(FGDB).CursorFirst( _cursor, ex.XMin, ex.XMax,
                                       ex.YMin, ex.YMax, sqlquery
                                      ) ;
      cursorFGDB[_cursor].curFirst := True ;
      cursorNext( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerFGDB.cursorNext(
    const _cursor : Integer
  ) ;
  var
    {$IFDEF OXYGENE}
    buf   : tgdbptr ;
    {$ELSE}
    buf   : IntPtr ;
    {$ENDIF}
    size  : Integer ;
  begin
    lockThread ;
    try
      while True do begin
        if HourglassShake then begin
          cursorFGDB[_cursor].currShape := nil ;
          break ;
        end ;

        if cursorFGDB[_cursor].curFirst then
          cursorFGDB[_cursor].curFirst := False
        else
          TGIS_FileFGDB(FGDB).CursorNext( _cursor ) ;

        cursorFGDB[_cursor].currShape := nil ;

        if not TGIS_FileFGDB(FGDB).CursorEof( _cursor ) then begin
          buf := TGIS_FileFGDB(FGDB).CursorShape( _cursor, size ) ;
          parseGeometry( _cursor,
                         buf,
                         size,
                         TGIS_FileFGDB(FGDB).GetOID( _cursor )
                        ) ;
        end ;

        if cursorFGDB[_cursor].currShape = nil then begin
         // using an xxxInternalVersion may be a bit too secure
         // but better is to be too restrictive than too lose
         if not inherited cursorEofInternal( _cursor ) then begin
           cursorFGDB[_cursor].currShape := inherited cursorShapeInternal( _cursor ) ;
           inherited cursorNextInternal( _cursor ) ;
         end ;
         if cursorFGDB[_cursor].currShape = nil then exit ;
        end ;

        if cursorState[_cursor].curSkipDeleted and
          cursorFGDB[_cursor].currShape.IsDeleted then
        begin
         continue ;
        end ;

        if not isInScope( cursorFGDB[_cursor].currShape, _cursor ) then
         continue
        else
         exit ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerFGDB.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorFGDB[_cursor].currShape = nil ;
  end ;

  function TGIS_LayerFGDB.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorFGDB[_cursor].currShape ) then
       Result := cursorFGDB[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerFGDB.GetShape(
    const _uid    : TGIS_Uid   ;
    const _cursor : Integer
  ) : TGIS_Shape ;
  var
    ex    : TGIS_Extent;
    {$IFDEF OXYGENE}
    buf   : tgdbptr ;
    {$ELSE}
    buf   : IntPtr ;
    {$ENDIF}
    size  : Integer ;
  begin
    lockThread ;
    try
      Result := nil ;

      if _uid <= 0 then exit ;

      // if it is in edited list
      Result := inherited GetShape( _uid, _cursor ) ;
      if Result <> nil then exit ;

      // is it a current shape
      if assigned( cursorFGDB[_cursor].currShape ) and
         ( _uid = cursorFGDB[_cursor].currShape.Uid ) then
      begin
        Result := cursorFGDB[_cursor].currShape ;
        exit ;
      end ;

      if assigned( cursorFGDB[_cursor].currShape ) then begin
        // check if it is subsequent call
        while not TGIS_FileFGDB(FGDB).CursorEof( _cursor ) do begin
          TGIS_FileFGDB(FGDB).CursorNext( _cursor ) ;
          if TGIS_FileFGDB(FGDB).CursorEof( _cursor ) then break ;

          buf := TGIS_FileFGDB(FGDB).CursorShape( _cursor, size ) ;
          parseGeometry( _cursor, buf, size,
                         TGIS_FileFGDB(FGDB).GetOID( _cursor )
                        ) ;
          // is it a current shape
           if assigned( cursorFGDB[_cursor].currShape ) and
             ( _uid = cursorFGDB[_cursor].currShape.Uid ) then
           begin
             Result := cursorFGDB[_cursor].currShape ;
             exit ;
           end
           else if cursorFGDB[_cursor].currShape.Uid > _uid then
            break ;
        end ;
      end ;

      cursorFGDB[_cursor].currShape := nil ;
      ex := GisWholeWorld ;
      TGIS_FileFGDB(FGDB).CursorFirst( _cursor, ex.XMin, ex.XMax, ex.YMin, ex.YMax,
                                       Format( '(%s>=%d)', [FOIDFieldName, _uid] )
                                     ) ;
      if TGIS_FileFGDB(FGDB).CursorEof( _cursor ) then exit ;

      buf := TGIS_FileFGDB(FGDB).CursorShape( _cursor, size ) ;
      parseGeometry( _cursor, buf, size,
                     TGIS_FileFGDB(FGDB).GetOID( _cursor )
                    ) ;
      // is it a current shape
      if assigned( cursorFGDB[_cursor].currShape ) and
         ( _uid = cursorFGDB[_cursor].currShape.Uid ) then
      begin
        Result := cursorFGDB[_cursor].currShape ;
        exit ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerFGDB.GetLastUid : TGIS_Uid ;
  var
    shp       : TGIS_Shape ;
    old_scope : String     ;
  begin
    lockThread ;
    try
      old_scope := Scope ;
      try
        if FLastUid < 0 then begin
          shp := nil ;
          cursorFirst( 0, False,
                       GisWholeWorld, '', nil, '', True
                     ) ;

          while not cursorEof(0) do begin // iterate all shapes
            shp := cursorShape(0) ;
            cursorNext(0) ;
          end ;

          if assigned( shp ) then
            FLastUid := shp.Uid
          else
            FLastUid := 0 ;
        end ;

        Result := FLastUid ;
      finally
        Scope := old_scope ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerFGDB.GetNewUid : TGIS_Uid ;
  begin
    lockThread ;
    try
      GetLastUid ;
      inc( FLastUid ) ;
      Result := FLastUid ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerFGDB.GetAvailableLayers : TGIS_LayerInfoList ;
  begin
    if not assigned( FGDB ) then
      FGDB := TGIS_FileFGDB.Create ;

    Result := TGIS_FileFGDB(FGDB).GetAvailableLayers( Path ) ;
  end ;

  function TGIS_LayerFGDB.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  var
    storage : String ;
  begin
    GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_STORAGE, storage ) ;

    Result := CompareText( storage, GIS_INI_LAYERSQL_FILEGDB ) = 0 ;
  end ;

  procedure TGIS_LayerFGDB.RevertShapes;
  var
    i : Integer ;
  begin
    inherited ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorFGDB[i].currShape := nil ;
  end ;

  procedure TGIS_LayerFGDB.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
  ) ;
  var
    gdb : TGIS_FileFGDB ;
  begin
    inherited ;

    if IsReadOnly then exit ;

    gdb := TGIS_FileFGDB.Create ;
    try
      if SafeDirectoryExists( _path ) then
        TGIS_FileFGDB(gdb).OpenDatabase( _path, Name )
      else
        TGIS_FileFGDB(gdb).NewDatabase( _path, Name ) ;

      TGIS_FileFGDB(gdb).Build( _path, Name, _extent, _type, _dim, CS ) ;
    finally
      TGIS_FileFGDB(gdb).CloseDatabase ;
      FreeObject( gdb ) ;
    end ;
  end ;

  procedure TGIS_LayerFGDB.doTableAlter(
    const _layer : TGIS_LayerVector
  ) ;
  const
    N_ITER   = 255 ; // maximum number of tries in new name finding
  var
    i        : Integer ;
    cnt      : Integer ;
    fld      : TGIS_FieldInfo ;
    fname    : String  ;
    name_len : Integer ;
  begin
    if IsReadOnly then exit ;

    _layer.PrepareExportFieldNames( 32, Self <> _layer, False ) ;

    for i := 0 to _layer.Fields.Count - 1 do begin
      fld := _layer.FieldInfo( i ) ;

      fname    := fld.ExportName ;
      name_len := Min( length( fname ) + 2 , 32 ) ;

      if fld.Temporary then continue ;

      if     fld.Deleted then // delete field
      begin
        try
          TGIS_FileFGDB(FGDB).DeleteField( Name, fname ) ;
        except
          // name can not exist
        end ;
      end
      else if ( not fld.Saved ) or ( _layer <> Self ) then // create a field
      begin
        for cnt := 0 to N_ITER do begin
          try
            TGIS_FileFGDB(FGDB).AddField( Name, fname, fld.FieldType, fld.Width, fld.NewDecimal ) ;
            break ;
          except
            fname := Format( '%s%.2x',
                             [ Copy( fname, StringFirst, name_len - 2 ), cnt ]
                           ) ;
            if cnt = N_ITER then raise ;
          end ;
        end ;

          fld.Saved      := True  ;
          fld.ExportName := fname ;
          fld.Name       := fld.ExportName ;
          fld.Width      := fld.NewWidth   ;
          fld.Decimal    := fld.NewDecimal ;
      end
      else begin
          if fld.NewName <> fld.Name then begin // rename a files
            // column renaming is not implemented on most databases
            fld.NewName    := fld.Name ;
            fld.ExportName := fld.Name ;
          end ;
        end ;
    end ;

  end ;

  procedure TGIS_LayerFGDB.doShapeUpdate(
    const _shp    : TGIS_Shape ;
    const _import : Boolean
  ) ;
  var
    i         : Integer        ;
    fldinfo   : TGIS_FieldInfo ;
    fld       : Variant        ;
    skip_geom : Boolean        ;
    geom      : OleVariant     ;
    update    : Boolean        ;
    found     : Boolean        ;
  begin
    if IsReadOnly then exit ;

    // update geometry
    skip_geom := False ;
    update    := False ;

    if _import then
      TGIS_FileFGDB(FGDB).CreateRecord
    else begin
      skip_geom := not _shp.GeometryChanged and not _shp.IsNewShape ;

      // because we don't reserve a record
      if _shp.IsNewShape then
        TGIS_FileFGDB(FGDB).CreateRecord
      else begin
        found := TGIS_FileFGDB(FGDB).GetRecord( Format( 'OBJECTID=%d', [_shp.Uid] ) ) ;
        assert( found, Format( 'Record with oid %d not found', [_shp.Uid] ) ) ;
        update := True ;
      end ;
    end ;

    for i := 0 to Fields.Count - 1 do begin
      fldinfo := FieldInfo( i ) ;
      if not (TGIS_FieldFlags.Exportable in fldinfo.Flags) then continue ;

      fld := _shp.GetFieldEx( fldinfo.NewName ) ;

      TGIS_FileFGDB(FGDB).RecordSetField(
        fldinfo.ExportName,
        fldinfo.FieldType,
        fldinfo.NewWidth,
        fldinfo.NewDecimal,
        fld
      ) ;
    end ;

    if not skip_geom then begin
      TGIS_GeometryFactory.GisExportGeometryToShapeEx( _shp, geom ) ;
      TGIS_FileFGDB(FGDB).RecordSetGeometry( geom ) ;
    end ;

    if update then
      TGIS_FileFGDB(FGDB).UpdateRecord
    else
      TGIS_FileFGDB(FGDB).InsertRecord ;
  end ;

  procedure TGIS_LayerFGDB.ImportLayerEx(
    const _layer       : TGIS_LayerVector  ;
    const _extent      : TGIS_Extent       ;
    const _type        : TGIS_ShapeType    ;
    const _scope       : String            ;
    const _shape       : TGIS_Shape        ;
    const _de9im       : String            ;
    const _truncated   : Boolean
  ) ;
  var
    shp        : TGIS_Shape     ;
    shp_tmp    : TGIS_Shape     ;
    shp_type   : TGIS_ShapeType ;
    shp_dim    : TGIS_DimensionType ;
    first      : Boolean        ;
    shape_no   : Cardinal       ;
    end_uid    : TGIS_Uid        ;
    abort      : Boolean        ;
    eloop      : TGIS_LayerVectorEnumerator ;
  begin
    if not assigned( _layer ) then exit ;
    if IsReadOnly then exit ;

    assert( Self <> _layer ) ;

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    Extent   := GisCommonExtent( _layer.Extent, _extent ) ;
    shp_type := _type ;
    first    := True  ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;

    if shp_type = TGIS_ShapeType.Unknown then begin
      // find first matching shape
      shp_tmp := _layer.FindFirst( _extent, _scope, _shape, _de9im ) ;
      if assigned( shp_tmp ) then
        shp_type := shp_tmp.ShapeType ;
    end ;

    FGDB := TGIS_FileFGDB.Create ;
    try
      Build( Path, Extent, shp_type, _layer.DefaultDimension ) ;

      TGIS_FileFGDB(FGDB).OpenDatabase( Path, Name ) ;
      TGIS_FileFGDB(FGDB).OpenTable( Path, Name ) ;

      shp_dim := _layer.DefaultDimension ;
      SupportedDimensions := _layer.SupportedDimensions ;

      Fields.Clear ;
      ImportStructure( _layer ) ;

      PrepareExportFieldNames( 32, False, False ) ;
      doTableAlter( Self ) ;
      ExportStructureToFLD ;

      TGIS_FileFGDB(FGDB).StartBulkTransaction ;
      try
        eloop := _layer.Loop( _extent, _scope, _shape, _de9im ).GetEnumerator() ;
        try
          while eloop.MoveNext do begin // iterate all shapes
            shp := eloop.GetCurrent ;
            shp_tmp := shp.PrepareExportShape(
                             CS, _extent, _truncated, True
                           ) ;
            if assigned( shp_tmp ) then
              try
                if ( not shp_tmp.IsDeleted ) and
                   ( ( shp_tmp.ShapeType=shp_type   ) or
                     ( shp_type=TGIS_ShapeType.Unknown )
                   )
                then begin
                  // calculate extent
                     if first then begin
                       Extent   := _TGIS_Extent( shp_tmp.Extent ) ;
                       first    := False ;
                       shp_dim  := shp_tmp.Dimension ;
                     end
                     else
                       Extent := GisMaxExtent( Extent, shp_tmp.Extent ) ;
                  // insert record
                     doShapeUpdate( shp_tmp, True ) ;
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
        // update master table
        TGIS_FileFGDB(FGDB).EndBulkTransaction ;
        TGIS_FileFGDB(FGDB).CloseTable ;
        FIsModified := False ;
      end ;
    finally
      try
        TGIS_FileFGDB(FGDB).CloseDatabase ;
        FreeObject( FGDB ) ;

        Items.Clear ;
        Fields.Clear ;

        FIsModified := False ;
        FIsOpened   := False ;

        Open ;
      except
      end ;

      RaiseBusyRelease( _layer ) ;
    end ;

  end ;

  procedure TGIS_LayerFGDB.SaveData ;
  var
    i, c     : Integer    ;
    shp      : TGIS_Shape ;
    first    : Boolean    ;
    shape_no : Cardinal   ;
    end_uid  : Integer    ;
    abort    : Boolean    ;
    done     : Boolean    ;
  begin
    SaveFieldRules ;

    if IsReadOnly then exit ;

    shape_no := 0 ;
    end_uid  := Items.Count ;
    abort    := False ;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try
      // be sure that database is opened
        assert( assigned( FGDB ) ) ;
        TGIS_FileFGDB(FGDB).OpenTable( Path, Name ) ;

      // Alter table
         doTableAlter( Self ) ;

      // update all items
         first := True ;

      ExportStructureToFLD ;

      done := False ;
      //TGIS_FileFGDB(FGDB).StartBulkTransaction ;
      try
        for i := 0 to Items.Count-1 do begin // iterate all edited shapes
          shp := TGIS_Shape( Items[i] ) ;

          // ignore not touched shapes
             if not shp.IsModified then
               continue ;

          // delete deleted items
             if shp.IsDeleted then begin
               if TGIS_FileFGDB(FGDB).GetRecord( Format( 'OBJECTID=%d', [shp.Uid] ) ) then
                 TGIS_FileFGDB(FGDB).DeleteRecord ;
               continue ;
             end ;

          // calculate extent
             if first then begin
               if GisIsNoWorld( Extent ) or GisIsWholeWorld( Extent ) then
                 Extent := _TGIS_Extent( shp.Extent) ;
               first  := False ;
             end ;

             Extent := GisMaxExtent( Extent, shp.Extent ) ;

          doShapeUpdate( shp, False ) ;

          inc( shape_no ) ;
          if shape_no mod 100 = 1 then begin
            abort := RaiseBusyShake( Self, i + 1, end_uid ) ;
            if abort then break ;
          end ;
        end ;
        done := True ;
      finally
        if done then begin
          // update master table

         Items.Clear ;
        end ;
        //TGIS_FileFGDB(FGDB).EndBulkTransaction ;
        TGIS_FileFGDB(FGDB).CloseTable ;
        TGIS_FileFGDB(FGDB).CloseDatabase ;

        Fields.Clear ;
        FIsModified := False ;
        FIsOpened   := False ;

        for c := 0 to BUILTIN_CURSORS - 1 do
          cursorFGDB[c].currShape := nil ;

        Open ;
      end ;

    finally
      RaiseBusyRelease( Self ) ;
    end ;

    inherited ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerFGDB.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-FGDB', 'Esri File Geodatabase API for vector',
                   TGIS_LayerFGDB, GIS_TTKLAYER_VECTOR_FILTER,
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   GIS_DEFAULT_LAYER_PRIORITY, False
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerFGDB.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

