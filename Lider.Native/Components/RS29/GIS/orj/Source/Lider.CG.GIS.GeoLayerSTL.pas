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
  Encapsulation of the Standard Tessellation Language (STL) file format
  (stereolitography CAD).
}

{$IFDEF DCC}
  unit GisLayerSTL ;
  {$HPPEMIT '#pragma link "GisLayerSTL"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Variants,
    {$IFDEF LEVEL_XE2_RTL}
      System.Types,
    {$ENDIF}
    GisTypes,
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
  Unit_GisLayerSTL = class
    public
      class procedure SelfRegisterLayer ;
  end ;

  /// <summary>
  ///   Encapsulation of the Standard Tessellation Language (STL) file format
  ///   (stereolitography CAD).
  /// </summary>
  TGIS_LayerSTL = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      isBinary    : Boolean ;
      FSaveBinary : Boolean ;
    private
      procedure checkType ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <inheritdoc/>
      procedure setUp ; override;
    protected
      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      /// <inheritdoc/>
      constructor Create ; override;
    public
      /// <inheritdoc/>
      procedure Build        ( const _path      : String           ;
                               const _extent    : TGIS_Extent      ;
                               const _type      : TGIS_ShapeType   ;
                               const _dim       : TGIS_DimensionType
                             ) ; override;
      /// <inheritdoc/>
      procedure ImportLayerEx( const _layer      : TGIS_LayerVector ;
                                const _extent    : TGIS_Extent      ;
                                const _type      : TGIS_ShapeType   ;
                                const _scope     : String           ;
                                const _shape     : TGIS_Shape       ;
                                const _de9im     : String           ;
                                const _truncated : Boolean
                             ) ; override;
      /// <inheritdoc/>
      function  PreRecognize ( const _path       : String           ;
                               var   _new_path   : String
                             ) : Boolean ; override;
    public
      /// <summary>
      ///   True if the file should be saved in the binary version.
      /// </summary>
      property SaveBinary : Boolean read  FSaveBinary
                                    write FSaveBinary ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisInternals,
    GisClasses,
    GisFunctions,
    GisRtl,
    GisResource,
    GisTessellation,
    GisRegistredLayers ;
{$ENDIF}

//==============================================================================
// TGIS_LayerSTL
//==============================================================================

  constructor TGIS_LayerSTL.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory  ,
                             TGIS_LayerSubType.Exportable
                           ] ;
  end ;

  procedure TGIS_LayerSTL.doDestroy ;
  begin

    inherited ;
  end ;

  procedure TGIS_LayerSTL.checkType ;
  var
    strm : TGIS_FileStream ;
    b    : Byte ;
    c    : Char ;
    str  : String ;
    flg  : Boolean ;
    i    : Integer ;
  begin
    strm := TGIS_FileStream.Create( Path,
                                    fmOpenRead or
                                    fmShareDenyWrite
                                  ) ;
    try
      isBinary := True ;

      str := '' ;
      flg := False ;
      i := 0 ;
      while True do begin
        strm.ReadByte( b, 1 ) ;
        c := Char( b ) ;
        if c <> ' ' then
          flg := True ;
        if flg then begin
          str := str + c ;
          inc( i ) ;
          if i = 5 then
            break ;
        end ;
      end ;
      str := Trim( str ) ;

      if UpperCase( str ) = 'SOLID' then
        isBinary := False ;
    finally
      FreeObject( strm ) ;
    end ;
  end ;

  procedure TGIS_LayerSTL.setUp ;
  var
    lst  : TStringList ;
    str  : String ;
    tkn  : TGIS_Tokenizer ;
    shp  : TGIS_Shape ;
    pt3  : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    err  : Boolean ;
    strm : TGIS_FileStream ;
    buf  : TBytes ;
    cnt  : DWORD ;
    sng  : array of Single ;

    procedure open_ascii ;
    var
      state : Integer ;
      i     : Integer ;
    begin
      err := True ;

      state := 0 ;

      lst := TStringList.Create ;
      try
        lst.LoadFromFile( Path ) ;

        if lst.Count < 9 then
          exit ;

        tkn := TGIS_Tokenizer.Create ;
        try
          shp := Self.CreateShape( TGIS_ShapeType.MultiPatch ) ;
          shp.Lock( TGIS_Lock.Projection ) ;
          try
            TGIS_ShapeMultiPatch( shp ).HasNormals := True ;

            SetLength( sng,  3 ) ;
            for i := 0 to lst.Count - 1 do begin
              str := Trim( lst.Strings[i] ) ;

              if str = '' then
                continue ;

              tkn.Execute( str, [ ' ' ] ) ;

              case state of
                0 : begin
                      if tkn.Result.Count > 1 then
                        Self.Name := Trim( tkn.Result[1] ) ;
                      inc( state ) ;
                    end ;
                1 : begin
                      if ( tkn.Result.Count = 1 ) or
                         ( tkn.Result.Count = 2 ) then begin
                        if UpperCase( tkn.Result[0] ) = 'ENDSOLID' then
                          break
                        else
                          exit ;
                      end
                      else
                      if tkn.Result.Count <> 5 then
                        exit ;

                      sng[0] := DotStrToFloat( tkn.Result[2] ) ;
                      sng[1] := DotStrToFloat( tkn.Result[3] ) ;
                      sng[2] := DotStrToFloat( tkn.Result[4] ) ;
                      TGIS_ShapeMultiPatch( shp ).Normals.Resize(
                        TGIS_ShapeMultiPatch( shp ).Normals.NumNormals + 1
                      ) ;
                      TGIS_ShapeMultiPatch( shp ).Normals.Normal[
                        TGIS_ShapeMultiPatch( shp ).Normals.NumNormals - 1
                      ] := GisSingleVector( sng[0], sng[1], sng[2] ) ;

                      inc( state ) ;
                    end ;
                2 : begin
                      if tkn.Result.Count <> 2 then
                        exit ;

                      shp.AddPart ;

                      inc( state ) ;
                    end ;
                3,
                4,
                5 : begin
                      if tkn.Result.Count <> 4 then
                        exit ;

                      pt3.X := DotStrToFloat( tkn.Result[1] ) ;
                      pt3.Y := DotStrToFloat( tkn.Result[2] ) ;
                      pt3.Z := DotStrToFloat( tkn.Result[3] ) ;
                      shp.AddPoint3D( pt3 ) ;
                      inc( state ) ;
                    end ;
                6 : begin
                      shp.SetPartType( shp.GetNumParts - 1, TGIS_PartType.Triangle ) ;
                      if tkn.Result.Count = 4 then begin
                        shp.AddPart ;
                        shp.AddPoint3D( shp.GetPoint3D( shp.GetNumParts - 2, 2 ) ) ;
                        pt3.X := DotStrToFloat( tkn.Result[1] ) ;
                        pt3.Y := DotStrToFloat( tkn.Result[2] ) ;
                        pt3.Z := DotStrToFloat( tkn.Result[3] ) ;
                        pt3.M := 0 ;
                        shp.AddPoint3D( pt3 ) ;
                        shp.AddPoint3D( shp.GetPoint3D( shp.GetNumParts - 2, 0 ) ) ;
                        TGIS_ShapeMultiPatch( shp ).Normals.Resize(
                          TGIS_ShapeMultiPatch( shp ).Normals.NumNormals + 1
                        ) ;
                        TGIS_ShapeMultiPatch( shp ).Normals.Normal[
                              TGIS_ShapeMultiPatch( shp ).Normals.NumNormals - 1
                            ] := TGIS_ShapeMultiPatch( shp ).Normals.Normal[
                                   TGIS_ShapeMultiPatch( shp ).Normals.NumNormals - 2
                                 ] ;
                        continue ;
                      end
                      else
                      if tkn.Result.Count <> 1 then
                        exit ;
                      inc( state ) ;
                    end ;
                7 : begin
                      if tkn.Result.Count <> 1 then
                        exit ;
                      state := 1 ;
                    end ;
              end ;
            end ;
          finally
            shp.Unlock ;
          end;
        finally
          FreeObject( tkn ) ;
        end ;
      finally
        FreeObject( lst ) ;
      end ;

      err := False ;
    end ;

    procedure open_binary ;
    var
      i : Integer ;
      j : Integer ;
      k : Integer ;
    begin
      strm := TGIS_FileStream.Create( Path,
                                      fmOpenRead or
                                      fmShareDenyWrite
                                    ) ;
      try
        SetLength( buf, 80 ) ;
        {$IFDEF OXYGENE}
          strm.Read( buf, 80 ) ;
        {$ELSE}
          strm.Read( buf[0], 80 ) ;
        {$ENDIF}

        strm.ReadCardinal( cnt, 4 ) ;

        SetLength( buf, 50 ) ;
        SetLength( sng,  3 ) ;

        shp := Self.CreateShape( TGIS_ShapeType.MultiPatch ) ;
        shp.Lock( TGIS_Lock.Projection ) ;
        try
          TGIS_ShapeMultiPatch( shp ).HasNormals := True ;
          TGIS_ShapeMultiPatch( shp ).Normals.Resize( cnt ) ;
          for i := 0 to cnt - 1 do begin
            {$IFDEF OXYGENE}
              strm.Read( buf, 50 ) ;
            {$ELSE}
              strm.Read( buf[0], 50 ) ;
            {$ENDIF}
            shp.AddPart ;
            for j := 0 to 3 do begin
              for k := 0 to 2 do
                {$IFDEF OXYGENE}
                  sng[k] := BitConverter.ToSingle( buf, j*12+k*4 ) ;
                {$ELSE}
                  sng[k] := PSingle( NativeInt( buf ) + j*12+k*4 )^ ;
                {$ENDIF}

              if j > 0 then begin
                pt3.X := sng[0] ;
                pt3.Y := sng[1] ;
                pt3.Z := sng[2] ;
                pt3.M := 0 ;
                shp.AddPoint3D( pt3 ) ;
              end
              else
                TGIS_ShapeMultiPatch( shp ).Normals.Normal[i] :=
                  GisSingleVector( sng[0], sng[1], sng[2] ) ;
            end ;
            shp.SetPartType( shp.GetNumParts - 1, TGIS_PartType.Triangle ) ;
          end ;
        finally
          shp.Unlock ;
        end ;
      finally
        FreeObject( strm ) ;
      end ;
    end ;

  begin
    inherited ;

    FSupportedShapes     := [ TGIS_ShapeType.MultiPatch ]  ;
    FSupportedDimensions := [ TGIS_DimensionType.XYZM ]  ;

    DefaultShapeType := TGIS_ShapeType.MultiPatch ;
    DefaultDimension := TGIS_DimensionType.XYZM ;

    if not IsStringEmpty( Path ) then begin
      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
      Lock ;
      try
        checkType ;

        if not isBinary then begin
          open_ascii ;
          if err then begin
            isBinary := True ;
            open_binary ;
          end ;
        end
        else
          open_binary ;
      finally
        Unlock ;
        RaiseBusyRelease( Self ) ;
      end;
    end ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'Standard Tessellation Language (STL)' ;

    if isBinary then
      FFileInfo := FFileInfo + ', binary'
    else
      FFileInfo := FFileInfo + ', ASCII' ;
  end ;

  procedure TGIS_LayerSTL.Build(
    const _path      : String           ;
    const _extent    : TGIS_Extent      ;
    const _type      : TGIS_ShapeType   ;
    const _dim       : TGIS_DimensionType
  ) ;
  var
    lst : TStringList ;
  begin
    inherited ;

    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    lst := TStringList.Create ;
    try
      lst.SaveToFile( _path ) ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_LayerSTL.ImportLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent      ;
    const _type      : TGIS_ShapeType   ;
    const _scope     : String           ;
    const _shape     : TGIS_Shape       ;
    const _de9im     : String           ;
    const _truncated : Boolean
  ) ;
  var
    en         : TGIS_LayerVectorEnumerator ;
    shp        : TGIS_Shape ;
    shape_no   : Cardinal   ;
    end_uid    : TGIS_Uid   ;
    abort      : Boolean    ;
    shape_file : TGIS_BufferedStream ;
    same_name  : Boolean     ;
    old_scope  : String      ;
    ex         : TGIS_Extent ;
    num_faces  : Integer ;

    procedure writeSolid( const _shp : TGIS_Shape ) ;
    var
      mb            : TGIS_MultiPatchBuilder ;
      i             : Integer ;
      f1, f2, f3    : Integer ;
      n, v1, v2, v3 : TGIS_SingleVector ;
    begin
      mb := TGIS_MultiPatchBuilder.Create ;
      try
        mb.BuildMesh( _shp ) ;
        num_faces := num_faces + mb.FacesCount ;

        for i := 0 to mb.FacesCount-1 do begin
          mb.GetFaceNormal( i, n ) ;

          shape_file.WriteSingle( n.X ) ;
          shape_file.WriteSingle( n.Y ) ;
          shape_file.WriteSingle( n.Z ) ;

          mb.GetFace( i, f1, f2, f3 ) ;
          mb.GetVertex( f3-1, v1 ) ;
          mb.GetVertex( f2-1, v2 ) ;
          mb.GetVertex( f1-1, v3 ) ;

          shape_file.WriteSingle( v1.X ) ;
          shape_file.WriteSingle( v1.Y ) ;
          shape_file.WriteSingle( v1.Z ) ;

          shape_file.WriteSingle( v2.X ) ;
          shape_file.WriteSingle( v2.Y ) ;
          shape_file.WriteSingle( v2.Z ) ;

          shape_file.WriteSingle( v3.X ) ;
          shape_file.WriteSingle( v3.Y ) ;
          shape_file.WriteSingle( v3.Z ) ;

          shape_file.WriteByte( 0 ) ;
          shape_file.WriteByte( 0 ) ;
        end ;
      finally
        FreeObject( mb ) ;
      end ;

    end ;

  begin
    if not assigned( _layer ) then exit ;

    if not CheckFileWriteAccessEx( Path, True , True, True ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try

      same_name := CompareText( GetPathAbsolute( '', Path        ),
                                GetPathAbsolute( '', _layer.Path )
                              ) = 0  ;
      ex := GisCommonExtent( _layer.Extent, _extent ) ;

      // prepare temporary geometry
      try
        shape_file := TGIS_BufferedFileStream.Create(
                        GetTemporaryName( Path ), TGIS_StreamMode.&Create
                      ) ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                     GetTemporaryName( Path ),
                                     GetLastError
                                   ) ;
      end ;

      shape_file.WriteStringCnt( '', 80 ) ;
      num_faces := 0 ;
      shape_file.WriteInteger( num_faces ) ;

      old_scope := _layer.Scope ;
      if same_name then
        _layer.Scope := '' ;

      try
        en := _layer.Loop( ex, _scope ).GetEnumerator ;
        try
          while en.MoveNext do begin
            shp := en.GetCurrent ;

            if ( shp.ShapeType <> TGIS_ShapeType.Polygon    ) and
               ( shp.ShapeType <> TGIS_ShapeType.MultiPatch ) then
              continue ;

            writeSolid( shp ) ;

            if shape_no mod 100 = 1 then begin
              abort := RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
              if abort then break ;
            end ;
            inc( shape_no ) ;
          end ;
        finally
          FreeObject( en ) ;
        end;
      finally
        shape_file.Position := 80 ;
        shape_file.WriteInteger( num_faces ) ;

        _layer.Scope := old_scope ;

        if abort then begin
           FreeObject( shape_file ) ;
           DeleteFile( GetTemporaryName( Path ) ) ;
        end
        else begin
           FreeObject( shape_file ) ;
           DeleteFile( GetBackupName( Path ) ) ;
           RenameFile( Path, GetBackupName( Path ) ) ;
           try
             if not RenameFile( GetTemporaryName( Path ), Path ) then
               raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), Path,
                                            GetLastError
                                          ) ;
           except
              // recover ;
              RenameFile( GetBackupName( Path ), Path ) ;
              raise ;
           end ;
        end ;
      end ;

      Items.Clear ;
      Fields.Clear ;

      FIsModified := False ;
      FIsOpened   := False ;

      Open ;

    finally
      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  function TGIS_LayerSTL.PreRecognize(
    const _path      : String ;
    var   _new_path  : String
  ) : Boolean ;
  begin
    Result := inherited ;
  end ;


//==============================================================================
// Unit_GisLayerSTL
//==============================================================================

  class procedure Unit_GisLayerSTL.SelfRegisterLayer ;
  begin
    RegisterLayer( 'DK-STL', 'Standard Tessellation Language',
                    TGIS_LayerSTL,
                   '.stl;.stla',
                   TGIS_RegisteredLayerType.Vector3D,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                    False
                  ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    Unit_GisLayerSTL.SelfRegisterLayer ;
{$ENDIF}

//==================================== END =====================================
end.

