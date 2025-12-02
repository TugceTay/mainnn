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
  Encapsulation of a gridded XYZ or Point Cloud XYZ (RGB) vector layer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerXYZ ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerXYZ"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,
    System.Classes,

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoLayerVector ;
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

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerXYZ = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  {$IFDEF OXYGENE}
    T_cursorXYZ nested in TGIS_LayerXYZ = record
      public

        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse      : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record-by-record access.
        /// </summary>
        curShape      : TGIS_Shape ;

        /// <summary>
        ///   Pre-allocated shape. Recreating the shape on each cursor movement is
        ///   much faster than full Create constructor.
        /// </summary>
        curShapePoint : TGIS_ShapePoint ;

        /// <summary>
        ///   Current color.
        /// </summary>
        curRGB        : TGIS_Color ;

        /// <summary>
        ///   Current file position
        /// </summary>
        curPosition   : Int64 ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Layer which can read a XYZ or ASC text file.
  /// </summary>
  TGIS_LayerXYZ = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private

        xyzFile   : TGIS_BufferedFileStream ;

        /// <summary>
        ///   Last UID in a shape file. If not set then -1.
        /// </summary>
        lastUid   : Integer ;

        /// <summary>
        ///   Tokenizer to parse data.
        /// </summary>
        tkn       : TGIS_Tokenizer ;

        /// <summary>
        ///   True if file has RGB data.
        /// </summary>
        hasRGB    : Boolean ;

        /// <summary>
        ///   True if file has normals data.
        /// </summary>
        hasNormals: Boolean ;

        /// <summary>
        ///   True if file is in PLY format.
        /// </summary>
        isPLY     : Boolean ;

        /// <summary>
        ///   True if file is in XYZRGB format.
        /// </summary>
        isXYZRGB  : Boolean ;

        /// <summary>
        ///   Fields map.
        /// </summary>
        map       : TStringList ;

        /// <summary>
        ///   Start position of data.
        /// </summary>
        startPos  : Int64;
        
    private
    
      {$IFDEF OXYGENE}
        cursorXYZ : array of T_cursorXYZ ;
      {$ELSE}
        cursorXYZ : array of record
          /// <summary>
          ///   Is cursor in use.
          /// </summary>
          curInUse      : Boolean ;

          /// <summary>
          ///   Current shape. Layer access is based on record-by-record
          ///   access.
          /// </summary>
          curShape      : TGIS_Shape ;

          /// <summary>
          ///   Pre-allocated shape. Recreating the shape on each cursor
          //    movement is much faster than full Create constructor.
          /// </summary>
          curShapePoint : TGIS_ShapePoint ;

          /// <summary>
          ///   Current color.
          /// </summary>
          curRGB       : TGIS_Color ;

          /// <summary>
          ///   Current file position
          /// </summary>
          curPosition : Int64 ;
        end ;
      {$ENDIF}

      oThread : TGIS_ThreadClass ;

    private // private methods

      /// <summary>
      ///   Read a shape from the shape file and recreate it.
      /// </summary>
      /// <param name="_uid">
      ///   id of shape to be retrieved
      /// </param>
      /// <param name="_cursor">
      ///   cursor to be used
      /// </param>
      /// <returns>
      ///   false if there is nothing to read
      /// </returns>
      function  readShape                ( const _uid         : TGIS_Uid ;
                                           const _cursor      : Integer
                                         ) : Boolean ;
                                         
      procedure prepareXYZRGB ;
      procedure preparePLY ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // property access methods

      procedure fset_UseRTree            ( const _value       : Boolean
                                         ) ; override;
         
      
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      function  getFieldInternal         ( const _uid         : TGIS_Uid   ;
                                           const _name        : String  ;
                                           const _cursor      : Integer
                                         ) : Variant ; override;

      /// <inheritdoc/>
      function  getBindedFieldInternal   ( const _shape       : TObject ;
                                           const _field       : Integer ;
                                           const _cursor      : Integer
                                         ) : Variant ; override;

     /// <inheritdoc/>
      procedure setUp                   ; override;
         
     // cursor access function(s)

     /// <inheritdoc/>
     function  cursorOpen               :  Integer ; override;

     /// <inheritdoc/>
     procedure cursorClose              ( const _cursor      : Integer
                                        ) ; override;

     /// <inheritdoc/>
     procedure cursorFirst              ( const _cursor      : Integer     ;
                                          const _viewerCS    : Boolean     ;
                                          const _extent      : TGIS_Extent ;
                                          const _query       : String      ;
                                          const _shape       : TGIS_Shape  ;
                                          const _de9im       : String      ;
                                          const _skipDeleted : Boolean
                                        ) ; override;

     /// <inheritdoc/>
     procedure cursorNext               ( const _cursor      : Integer
                                        ) ; override;
 
     /// <inheritdoc/>
     function  cursorEof                ( const _cursor      : Integer
                                        ) : Boolean ; override;

     /// <inheritdoc/>
     function  cursorShape              ( const _cursor      : Integer
                                        ) : TGIS_Shape ; override;

    protected
      // destructors

      procedure doDestroy                ; override;
      
    public
      // constructors
    
      /// <inheritdoc/>
      constructor Create                 ; override;

    public
      // shape access function(s)

      /// <inheritdoc/>
      function  GetLastUid : TGIS_Uid ; override;

      /// <inheritdoc/>
      procedure RecalcExtent   ; override;

      /// <inheritdoc/>
      function  GetShape                 ( const _uid         : TGIS_Uid ;
                                           const _cursor      : Integer
                                         ) : TGIS_Shape ; override;

  end;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SyncObjs,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

//=============================================================================
// TGIS_LayerXYZ
//=============================================================================

  constructor TGIS_LayerXYZ.Create ;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;
    tkn     := TGIS_Tokenizer.Create ;
    map     := TStringList.Create ;
    xyzFile := nil ;
    oThread := TGIS_ThreadClass.Create ;
  end ;

  procedure TGIS_LayerXYZ.doDestroy ;
  var
    i : Integer ;
  begin
    for i := 0 to high( cursorXYZ ) do
      FreeObject( cursorXYZ[i].curShapePoint ) ;

    FreeObject( oThread ) ;
    FreeObject( xyzFile ) ;
    FreeObject( tkn     ) ;
    FreeObject( map     ) ;

    inherited ;
  end ;

  procedure TGIS_LayerXYZ.preparePLY ;
  var
    line : String ;
  begin
    // PLY files can contain :
    //  ply
    //  format ascii 1.0
    //  comment VCGLIB generated
    //  element vertex 91020
    //  property float x
    //  property float y
    //  property float z
    //  property float nx
    //  property float ny
    //  property float nz
    //  property int flags
    //  property uchar red
    //  property uchar green
    //  property uchar blue
    //  property uchar alpha
    //  element face 0
    //  property list uchar int vertex_indices
    //  end_header
    //  data in order of properties

    hasRGB     := False ;
    hasNormals := False ;
    line  := '' ;
    map.Clear;
    while line <> 'end_header' do begin
      line := Trim( xyzFile.ReadLine ) ;
      tkn.Execute( line, [',', ' ', #9, #10] ) ;
      if ( tkn.Result.Count = 3 ) and ( tkn.Result[0] = 'property' ) then
        map.Add( tkn.Result[2] ) ;
    end ;

    if (map.IndexOf('red')>-1) and
       (map.IndexOf('green')>-1) and
       (map.IndexOf('blue')>-1) then
       hasRGB := True ;

    if (map.IndexOf('nx')>-1) and
       (map.IndexOf('ny')>-1) and
       (map.IndexOf('nz')>-1) then
       hasNormals := True ;

    startPos := xyzFile.Position ;
  end ;

  procedure TGIS_LayerXYZ.prepareXYZRGB ;
  var
    firstline : String ;
    r,g,b,i   : Integer ;
  begin
    // ASC files can either contain :
    //  X, Y, Z
    //  X, Y, Z, R, G, B
    //  X, Y, Z, I, J, K
    //  X, Y, Z, R, G, B, I, J, K

    hasRGB      := False ;
    hasNormals  := False ;

    firstline := Trim( xyzFile.ReadLine ) ;
    tkn.Execute( firstline, [',', ' ', #9, #10] ) ;
    if tkn.Result.Count = 9 then begin
      hasRGB      := True ;
      hasNormals  := True ;
      exit ;
    end
    else if tkn.Result.Count = 3 then begin
      hasRGB      := False ;
      hasNormals  := False ;
      exit ;
    end
    else if tkn.Result.Count = 6 then begin
      try
        r := StrToInt( tkn.Result[3] ) ;
        g := StrToInt( tkn.Result[4] ) ;
        b := StrToInt( tkn.Result[5] ) ;
        if (r >= 0) and (g >=0) and (b >=0) then begin
          hasRGB := True ;
          hasNormals := False ;
          exit ;
        end
        else if (r < 0) or (g < 0) or (b < 0) then begin
          hasRGB := False ;
          hasNormals := True ;
          exit ;
        end;
      except
        hasRGB := False ;
        hasNormals := True ;
        exit;
      end;
    end
    else if tkn.Result.Count = 7 then begin
      try
        i := StrToInt( tkn.Result[3] ) ;
        r := StrToInt( tkn.Result[4] ) ;
        g := StrToInt( tkn.Result[5] ) ;
        b := StrToInt( tkn.Result[6] ) ;
        if (r >= 0) and (g >=0) and (b >=0) then begin
          hasRGB := True ;
          hasNormals := False ;
          exit ;
        end ;
      except
        hasRGB := False ;
        hasNormals := False ;
        exit;
      end;
    end ;
    startPos := 0 ;

  end ;

  procedure TGIS_LayerXYZ.setUp ;
  begin
    inherited ;

    FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Point   ) ;
    FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYZ ) ;

    if not IsStringEmpty( Path ) then begin
      lastUid := -1 ;
      if not assigned( xyzFile ) then
        xyzFile := TGIS_BufferedFileStream.Create( Path, TGIS_StreamMode.Read ) ;

      Params.Marker.Size         := 2 ;
      Params.Marker.OutlineWidth := 0 ;

      isXYZRGB := UpperCase( GetFileExt( Path ) ) = '.ASC' ;
      isPLY    := UpperCase( GetFileExt( Path ) ) = '.PLY' ;

      if isXYZRGB then
        prepareXYZRGB
      else if isPLY then
        preparePLY ;

      if hasRGB then begin
        AddFieldInternal( 'RGB', TGIS_FieldType.Number, 18, 0 ) ;
        Params.Marker.ColorAsText := 'FIELD:RGB' ;
      end ;
    end ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    if hasRGB then
      FFileInfo := 'XYZ RGB Point Cloud'
    else
      FFileInfo := 'XYZ Point Cloud' ;
  end ;

  procedure TGIS_LayerXYZ.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    // do nothing
  end ;

  function  TGIS_LayerXYZ.cursorOpen :  Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length(cursorXYZ )  then
        SetLength( cursorXYZ, Result + 1 ) ;
      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorXYZ[Result] ) then
          cursorXYZ[Result] := new T_cursorXYZ ;
      {$ENDIF}

      cursorXYZ[Result].curInUse := True ;

      cursorXYZ[Result].curShapePoint := TGIS_ShapePoint.Create(
                                          nil, nil, False, -1, self,
                                          TGIS_DimensionType.XYZ
                                        ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerXYZ.cursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorXYZ[_cursor].curInUse := False ;
      FreeObject( cursorXYZ[_cursor].curShapePoint ) ;

      // truncate cursorState at the tail;
      for i := length( cursorXYZ ) - 1 downto 0 do begin
        if not cursorXYZ[i].curInUse then begin
          SetLength( cursorXYZ, i ) ;
        end
        else
          break ;
      end ;

      inherited cursorClose( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerXYZ.cursorFirst(
    const _cursor      : Integer          ;
    const _viewerCS    : Boolean          ;
    const _extent      : TGIS_Extent      ;
    const _query       : String           ;
    const _shape       : TGIS_Shape       ;
    const _de9im       : String           ;
    const _skipDeleted : Boolean
  ) ;
  begin
    lockThread ;
    try
      cursorXYZ[_cursor].curShape := nil ;

      if GisIsNoWorld( _extent ) then
        exit ;

      inherited cursorFirstInternal(
                  _cursor, _viewerCS,
                  _extent, _query, _shape, _de9im, _skipDeleted
                ) ;

      cursorXYZ[_cursor].curPosition := startPos ;

      cursorNext( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerXYZ.cursorNext(
    const _cursor : Integer
  ) ;
  var
    uid : TGIS_Uid ;
  begin
    lockThread ;
    try
      // read each shape
      while True do begin
        if HourglassShake then begin
          cursorXYZ[_cursor].curShape := nil ;
          break ;
        end ;

        if assigned( cursorXYZ[_cursor].curShape ) then begin
          uid := cursorXYZ[_cursor].curShape.Uid + 1 ;
        end
        else
          uid := 1 ;

        cursorXYZ[_cursor].curShape := nil ;

        if assigned( xyzFile ) then begin
          if not readShape( uid, _cursor )
            then exit ;
        end ;

        if cursorXYZ[_cursor].curShape = nil then begin
          // using an xxxInternalVersion may be a bit too secure
          // but better is to be too restrictive than too lose
          if not inherited cursorEofInternal( _cursor ) then begin
            cursorXYZ[_cursor].curShape := inherited cursorShapeInternal( _cursor ) ;
            inherited cursorNextInternal( _cursor ) ;
          end ;
          if cursorXYZ[_cursor].curShape = nil then
            if cursorXYZ[_cursor].curPosition >= xyzFile.Size then
              exit
            else
              continue ;
        end ;

        if cursorState[_cursor].curSkipDeleted and
           cursorXYZ[_cursor].curShape.IsDeleted then
        begin
          continue ;
        end ;

        if not isInScope( cursorXYZ[_cursor].curShape, _cursor ) then
          continue
        else
          exit ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerXYZ.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorXYZ[_cursor].curShape = nil ;
  end ;

  function TGIS_LayerXYZ.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorXYZ[_cursor].curShape ) then
       Result := cursorXYZ[_cursor].curShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerXYZ.readShape(
    const _uid    : TGIS_Uid ;
    const _cursor : Integer
  ) : Boolean ;
  var
    line  : String ;
    ptg   : TGIS_Point3D ;
    r,g,b : Byte ;
  begin
    if cursorXYZ[_cursor].curPosition >= xyzFile.Size then begin
      Result := False ;
      exit ;
    end;

    Result := True ;

    oThread.LockThread ;
    try
      xyzFile.Position := cursorXYZ[_cursor].curPosition ;

      line := xyzFile.ReadLine ;

      cursorXYZ[_cursor].curPosition := xyzFile.Position ;

      tkn.Execute( line, [',', ' ', #9, #10], False ) ;
      try
        {$IFDEF GIS_NORECORDS}
          ptg := new TGIS_Point3D ;
        {$ENDIF}
        if isXYZRGB then begin
          if tkn.Result.Count > 2 then begin
            ptg.X := DotStrToFloat( tkn.Result[0] ) ;
            ptg.Y := DotStrToFloat( tkn.Result[1] ) ;
            if CompareText( tkn.Result[2], 'NAN' ) = 0 then
              ptg.Z := 0
            else
              ptg.Z := DotStrToFloat( tkn.Result[2] ) ;
            ptg.M := 0 ;
          end ;

          cursorXYZ[_cursor].curShapePoint.Recreate(
            nil, nil, False, _uid, Self, TGIS_DimensionType.XYZ
          ) ;
          cursorXYZ[_cursor].curShapePoint.Reset ;
          cursorXYZ[_cursor].curShapePoint.AddPart ;

          cursorXYZ[_cursor].curShapePoint.Lock( TGIS_Lock.Internal ) ;
          cursorXYZ[_cursor].curShapePoint.AddPoint3D( ptg ) ;
          cursorXYZ[_cursor].curShapePoint.Unlock ;
          cursorXYZ[_cursor].curShape := getEdited( cursorXYZ[_cursor].curShapePoint );

          if tkn.Result.Count = 9 then begin
            r := StrToInt( tkn.Result[3] ) ;
            g := StrToInt( tkn.Result[4] ) ;
            b := StrToInt( tkn.Result[5] ) ;
            cursorXYZ[_cursor].curRGB := TGIS_Color.FromRGB( r, g, b ) ;
          end
          else
          if tkn.Result.Count = 6 then begin
            if hasRGB then begin
              r := StrToInt( tkn.Result[3] ) ;
              g := StrToInt( tkn.Result[4] ) ;
              b := StrToInt( tkn.Result[5] ) ;
              cursorXYZ[_cursor].curRGB := TGIS_Color.FromRGB( r, g, b ) ;
            end ;
          end
          else if tkn.Result.Count = 7 then begin
            if hasRGB then begin
              r := StrToInt( tkn.Result[4] ) ;
              g := StrToInt( tkn.Result[5] ) ;
              b := StrToInt( tkn.Result[6] ) ;
              cursorXYZ[_cursor].curRGB := TGIS_Color.FromBGR( r, g, b ) ;
            end ;
          end ;
        end
        else
        if isPLY then begin
          if tkn.Result.Count > 2 then begin
            ptg.X := DotStrToFloat( tkn.Result[map.IndexOf('x')] ) ;
            ptg.Y := DotStrToFloat( tkn.Result[map.IndexOf('y')] ) ;
            ptg.Z := DotStrToFloat( tkn.Result[map.IndexOf('z')] ) ;
            ptg.M := 0 ;
          end ;

          cursorXYZ[_cursor].curShapePoint.Recreate(
            nil, nil, False, _uid, Self, TGIS_DimensionType.XYZ
          ) ;
          cursorXYZ[_cursor].curShapePoint.Reset ;
          cursorXYZ[_cursor].curShapePoint.AddPart ;

          cursorXYZ[_cursor].curShapePoint.Lock( TGIS_Lock.Internal ) ;
          cursorXYZ[_cursor].curShapePoint.AddPoint3D( ptg ) ;
          cursorXYZ[_cursor].curShapePoint.Unlock ;
          cursorXYZ[_cursor].curShape := getEdited( cursorXYZ[_cursor].curShapePoint );

          if hasRGB then begin
            r := StrToInt( tkn.Result[map.IndexOf('red')] ) ;
            g := StrToInt( tkn.Result[map.IndexOf('green')] ) ;
            b := StrToInt( tkn.Result[map.IndexOf('blue')] ) ;
            cursorXYZ[_cursor].curRGB := TGIS_Color.FromRGB( r, g, b ) ;
          end ;
        end
        else begin
          if tkn.Result.Count > 2 then begin
            ptg.X := DotStrToFloat( tkn.Result[0] ) ;
            ptg.Y := DotStrToFloat( tkn.Result[1] ) ;
            if CompareText( tkn.Result[2], 'NAN' ) = 0 then
              ptg.Z := 0
            else
              ptg.Z := DotStrToFloat( tkn.Result[2] ) ;
            ptg.M := 0 ;
          end ;

          cursorXYZ[_cursor].curShapePoint.Recreate(
            nil, nil, False, _uid, Self, TGIS_DimensionType.XYZ
          ) ;
          cursorXYZ[_cursor].curShapePoint.Reset ;
          cursorXYZ[_cursor].curShapePoint.AddPart ;

          cursorXYZ[_cursor].curShapePoint.Lock( TGIS_Lock.Internal ) ;
          cursorXYZ[_cursor].curShapePoint.AddPoint3D( ptg ) ;
          cursorXYZ[_cursor].curShapePoint.Unlock ;
          cursorXYZ[_cursor].curShape := getEdited( cursorXYZ[_cursor].curShapePoint );
        end ;
      except
        // just hide exception
      end ;
    finally
      oThread.UnlockThread ;
    end;
  end ;

  function TGIS_LayerXYZ.getFieldInternal(
    const _uid      : TGIS_Uid ;
    const _name     : String ;
    const _cursor   : Integer
  ) : Variant ;
  begin
    lockThread ;
    try
      if _name = 'RGB' then
        Result := cursorXYZ[_cursor].curRGB.ARGB
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerXYZ.getBindedFieldInternal(
    const _shape  : TObject ;
    const _field  : Integer ;
    const _cursor : Integer
  ) : Variant ;
  begin
    lockThread ;
    try
      if _field = 0 then
        Result := cursorXYZ[_cursor].curRGB.ARGB
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerXYZ.GetShape(
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
      if not assigned( xyzFile ) then exit ;

      // is it a current shape
      if ( cursorXYZ[_cursor].curShape <> nil ) and
         ( _uid = cursorXYZ[_cursor].curShape.Uid ) then begin
        Result := cursorXYZ[_cursor].curShape ;
        exit ;
      end ;

      // check if it is subsequent call
      while not cursorEof( _cursor ) do begin
        cursorNext( _cursor ) ;
        if cursorShape( _cursor ) <> nil then begin
          if cursorShape( _cursor ).Uid = _uid then begin
            Result := cursorShape( _cursor ) ;
            exit ;
          end
          else
            break ;
        end
        else
          break ;
      end ;

      // if no index file, traverse normally
      inherited cursorStateSave( _cursor ) ;
      try
        cursorFirst( _cursor, False,
                     GisWholeWorld, '', nil, '', True
                   ) ;

        while not cursorEof( _cursor ) do begin
          if cursorShape(_cursor).Uid = _uid then begin
            Result := cursorShape(_cursor) ;
            exit ;
          end ;
          cursorNext(_cursor) ;
        end ;
      finally
        inherited cursorStateRestore( _cursor ) ;
      end;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerXYZ.GetLastUid : TGIS_Uid ;
  var
    shp       : TGIS_Shape ;
    old_scope : String     ;
  begin
    lockThread ;
    try
      old_scope := Scope ;
      try
        if lastUid < 0 then begin
          shp := nil ;
          cursorFirst( 0, False,
                       GisWholeWorld, '', nil, '', True
                     ) ;

          while not cursorEof(0) do begin // iterate all shapes
            shp := cursorShape(0) ;
            cursorNext(0) ;
          end ;

          if assigned( shp ) then
            lastUid := shp.Uid
          else
            lastUid := 0 ;
        end ;

        Result := lastUid ;
      finally
        Scope := old_scope ;
      end;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerXYZ.RecalcExtent;
  begin
    if assigned( Viewer ) then
      Viewer.Ref.HourglassPrepare ;
    try
      inherited ;
    finally
      if assigned( Viewer ) then
        Viewer.Ref.HourglassRelease ;
    end;

  end;


  { Perform initialization section.
  }
  class procedure GisLayerXYZ.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-XYZ', 'XYZ Point Cloud data',
                   TGIS_LayerXYZ, '.xyz;.asc',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   GIS_LOWER_LAYER_PRIORITY, False
                  ) ;
   end ;

{$IFDEF DCC}
  initialization
    GisLayerXYZ.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

