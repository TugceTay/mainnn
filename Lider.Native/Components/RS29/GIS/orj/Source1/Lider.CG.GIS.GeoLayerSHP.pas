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
  Encapsulation of a SHP file access.

  You can treat this as an sample of how to add native support to other
  GIS file formats.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSHP ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSHP"'}
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

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.SysUtils,
    System.Variants,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoFileDBF,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoFileSHP ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  {$IFDEF MONO} [Preserve (AllMembers=true)] {$ENDIF}
  GisLayerShp = class
    public
      // Perform initialization section.
      class procedure SelfRegisterLayer() ;
  end ;

  {$IFDEF OXYGENE}
    T_cursorShp nested in TGIS_LayerSHP = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public

        curInUse : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record-by-record access.
        /// </summary>
        curShape : TGIS_Shape ;

        /// <summary>
        ///   Pre-allocated shape. Recreating the shape on each cursor movement is
        ///   much faster than full Create constructor.
        /// </summary>
        curShapePoint : TGIS_ShapePoint ;

        /// <summary>
        ///   Pre-allocated shape. Recreating the shape on each cursor movement is
        ///   much faster than full Create constructor.
        /// </summary>
        curShapeArc : TGIS_ShapeArc ;

        /// <summary>
        ///   Pre-allocated shape. Recreating the shape on each cursor movement is
        ///  much faster than full Create constructor.
        /// </summary>
        curShapePolygon : TGIS_ShapePolygon ;

        /// <summary>
        ///   Pre-allocated shape. Recreating the shape on each cursor movement is
        ///   much faster than full Create constructor.
        /// </summary>
        curShapeMultipoint : TGIS_ShapeMultiPoint  ;

        /// <summary>
        ///   Pre-allocated shape. Recreating the shape on each cursor movement is
        ///   much faster than full Create constructor.
        /// </summary>
        curShapeMultiPatch : TGIS_ShapeMultiPatch ;

        /// <summary>
        ///   Last accessed shape type from SHP file.}
        /// </summary>
        curShapeType : TGIS_ShapeType ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Layer that can read SHP file.
  /// </summary>
  TGIS_LayerSHP = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
        /// <summary>
        ///   Shape file encapsulation.
        /// </summary>
        shapeFile : TGIS_FileSHP ;

        /// <summary>
        ///   Index file encapsulation.
        /// </summary>
        indexFile : TGIS_FileSHX ;

        /// <summary>
        ///   Data file encapsulation.
        /// </summary>
        dataFile  : TGIS_FileDBF ;

        /// <summary>
        ///   Last UID in a shape file. If not set then -1.
        /// </summary>
        lastUid   : TGIS_Uid ;
    private
        /// <summary>
        ///   If True, shape type from header is ignored and for XYZM dimension
        ///   each record dimension is verified
        /// </summary>
        ignoreHeaderShapeType : Boolean ;

        /// <summary>
        ///   If True, shape of XYZM will be treated as XYZ.
        /// </summary>
        fixXYZM : Boolean ;

        /// <summary>
        ///   If True, first shape access is performed.
        /// </summary>
        firstXYZMRead : Boolean ;

        /// <summary>
        ///   If True, first shape log is performed.
        /// </summary>
        firstXYZMLog : Boolean ;

    private
      {$IFDEF OXYGENE}
        cursorShp : array of T_cursorShp ;
      {$ELSE}
        cursorShp : array of record

          /// <summary>
          ///   Is cursor in use.
          /// </summary>
          curInUse : Boolean ;

          /// <summary>
          ///   Current shape. Layer access is based on record-by-record access.
          /// </summary>
          curShape : TGIS_Shape ;

          /// <summary>
          ///   Pre-allocated shape. Recreating the shape on each cursor movement is
          ///   much faster than full Create constructor.
          /// </summary>
          curShapePoint : TGIS_ShapePoint ;

          /// <summary>
          ///   Pre-allocated shape. Recreating the shape on each cursor movement is
          ///   much faster than full Create constructor.
          /// </summary>
          curShapeArc : TGIS_ShapeArc ;

          /// <summary>
          ///   Pre-allocated shape. Recreating the shape on each cursor movement is
          ///  much faster than full Create constructor.
          /// </summary>
          curShapePolygon : TGIS_ShapePolygon ;

          /// <summary>
          ///   Pre-allocated shape. Recreating the shape on each cursor movement is
          ///   much faster than full Create constructor.
          /// </summary>
          curShapeMultiPatch : TGIS_ShapeMultiPatch ;

          /// <summary>
          ///   Pre-allocated shape. Recreating the shape on each cursor movement is
          ///   much faster than full Create constructor.
          /// </summary>
          curShapeMultipoint : TGIS_ShapeMultiPoint  ;

          /// <summary>
          ///   Last accessed shape type from SHP file.}
          /// </summary>
          curShapeType : TGIS_ShapeType ;
        end ;
      {$ENDIF}

    // property access functions
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      function  fget_CodePage  : Integer ; override;
      function  fget_PathDBF   : String  ; virtual  ;
      function  fget_Path      : String  ; override;
      function  fget_PathSHX   : String  ; virtual  ;
      function  fget_PathPRJ   : String  ; virtual  ;
      function  fget_PathFLD   : String  ; override  ;
      function  fget_PathCPG   : String  ; virtual  ;
      function  fget_PathRTree : String  ; override;

    protected // other protected functions

      /// <summary>
      ///   Get DBF file extension.
      /// </summary>
      /// <returns>
      ///    extension text
      /// </returns>
      function  getFileExtDBF : String ; virtual;

      /// <summary>
      ///   Get SXH file extension.
      /// </summary>
      /// <returns>
      ///    extension text
      /// </returns>
      function  getFileExtSHX : String ; virtual;

      /// <summary>
      ///   Get PRJ file extension.
      /// </summary>
      /// <returns>
      ///    extension text
      /// </returns>
      function  getFileExtPRJ : String ; virtual;

      /// <summary>
      ///   Get FLD file extension.
      /// </summary>
      /// <returns>
      ///    extension text
      /// </returns>
      function  getFileExtFLD : String ; virtual;

      /// <summary>
      ///   Get CPG file extension.
      /// </summary>
      /// <returns>
      ///    extension text
      /// </returns>
      function  getFileExtCPG : String ; virtual;
    private // private methods

      /// <summary>
      ///   Read a shape from the shape file and recreate it.
      /// </summary>
      procedure readShape                ( const _uid     : TGIS_Uid ;
                                           const _cursor  : Integer
                                         ) ;

      /// <summary>
      ///   Check if a content buffer has Z and M data.
      /// </summary>
      /// <param name="_ptr">
      ///   pointer to content buffer
      /// </param>
      /// <param name="_shptype">
      ///   content shape type
      /// </param>
      /// <param name="_len">
      ///   content length
      /// </param>
      /// <returns>
      ///    Dimension type of the record.
      /// </returns>
      function verifyDimensionFromBuffer ( {$IFDEF MANAGED}
                                             const _ptr   : TGIS_Bytes ;
                                           {$ELSE}
                                             const _ptr   : Pointer    ;
                                           {$ENDIF}
                                           const _shptype : TGIS_ShapeType ;
                                           const _len     : Integer
                                         ) : TGIS_DimensionType ;

      /// <summary>
      ///   Proceed allowed name length by verifying global metadata
      /// </summary>
      function  allowedNameLength        : Integer ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      function  getFieldInternal      ( const _uid     : TGIS_Uid   ;
                                        const _name    : String  ;
                                        const _cursor  : Integer
                                      ) : Variant ; override;

      /// <inheritdoc/>
      function  getBindedFieldInternal( const _shape   : TObject ;
                                        const _field   : Integer ;
                                        const _cursor  : Integer
                                      ) : Variant ; override;

      /// <inheritdoc/>
      procedure setUp                 ; override;

      /// <inheritdoc/>
      function  cursorOpen :  Integer ; override;

      /// <inheritdoc/>
      procedure cursorClose( const _cursor      : Integer
                            ) ; override;

      /// <inheritdoc/>
      procedure cursorFirst( const _cursor      : Integer     ;
                             const _viewerCS    : Boolean     ;
                             const _extent      : TGIS_Extent ;
                             const _query       : String      ;
                             const _shape       : TGIS_Shape  ;
                             const _de9im       : String      ;
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

      /// <summary>
      ///   Destroys a layer instance.
      /// </summary>
      procedure doDestroy  ; override;

    public

      /// <inheritdoc/>
      constructor Create    ; override;

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
      procedure MergeLayerEx ( const _layer       : TGIS_LayerVector  ;
                             const _extent      : TGIS_Extent       ;
                             const _type        : TGIS_ShapeType    ;
                             const _scope       : String            ;
                             const _shape       : TGIS_Shape        ;
                             const _de9im       : String            ;
                             const _truncated   : Boolean           ;
                             const _restrict    : Boolean
                           ) ; override;


      /// <inheritdoc/>
      function  GetShape   ( const _uid    : TGIS_Uid ;
                             const _cursor : Integer
                           ) : TGIS_Shape ; override;

      /// <inheritdoc/>
      function  GetLastUid : TGIS_Uid ; override;

      /// <inheritdoc/>
      function  GetNewUid  : TGIS_Uid ; override;

      /// <inheritdoc/>
      procedure SaveData   ; override;

      /// <inheritdoc/>
      procedure InitializeDirectWrite(
                                 const _path        : String           ;
                                 const _extent      : TGIS_Extent      ;
                                 const _type        : TGIS_ShapeType   ;
                                 const _dim         : TGIS_DimensionType
                               ) ; override;

      /// <inheritdoc/>
      procedure FinalizeDirectWrite    ; override;

      /// <inheritdoc/>
      procedure WriteShapeDirect( const _shape       : TGIS_Shape
                                     ) ; override;
    public

      /// <summary>
      ///   Path to a DBF file.
      /// </summary>
      property PathDBF : String read fget_PathDBF ;

      /// <summary>
      ///   Path to a SHP file.
      /// </summary>
      property PathSHP : String read fget_Path    ;

      /// <summary>
      ///   Path to a SHX file.
      /// </summary>
      property PathSHX : String read fget_PathSHX ;

      /// <summary>
      ///   Path to a PRJ file.
      /// </summary>
      property PathPRJ : String read fget_PathPRJ ;

      /// <summary>
      ///   Path to a CPG file.
      /// </summary>
      property PathCPG : String read fget_PathCPG ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.SyncObjs,
    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRTree,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoGeometryFactory,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

const
  SHP_MAX_NAME_LENGTH_SAFE        = 10 ; // maximum length of DBF field name
  SHP_MAX_NAME_LENGTH_MAX         = 11 ; // maximum length of DBF field name
  SHP_METADATA_SAFEFIELDNAME      = 'TGIS_LayerSHP.SafeFieldName' ;
  SHP_METADATA_CHECKWINDING       = 'TGIS_LayerSHP.CheckWinding' ;
  SHP_METADATA_DEFAULTCODEPAGE    = 'TGIS_LayerSHP.DefaultCodePage' ;
  SHP_METADATA_IGNORE_HDR_SHPTYPE = 'TGIS_LayerSHP.IgnoreHeaderShapeType' ;

  DBF_EXT         = '.dbf' ;
  SHX_EXT         = '.shx' ;
  FLD_EXT         = '.fld' ;
  CPG_EXT         = '.cpg' ;

type
  // CPG code page matching
  T_cpgCodePage = record
    public
      id : String ;
      cp : Integer ;
      {$IFDEF OXYGENE}
        constructor Create( _id : String ; _cp : Integer ) ;
      {$ENDIF}
  end ;

  {$IFDEF OXYGENE}
    constructor T_cpgCodePage.Create( _id : String ; _cp : Integer ) ;
    begin
      id := _id ;
      cp := _cp ;
    end ;
  {$ENDIF}

var
  // all possible CPG values
  cpgCodePages : array [0..47] of T_cpgCodePage =
  {$IFDEF DCC}
    (
      ( id:'437'    ; cp:437   ),
      ( id:'708'    ; cp:708   ),
      ( id:'720'    ; cp:720   ),
      ( id:'737'    ; cp:737   ),
      ( id:'775'    ; cp:775   ),
      ( id:'850'    ; cp:850   ),
      ( id:'852'    ; cp:852   ),
      ( id:'855'    ; cp:855   ),
      ( id:'857'    ; cp:857   ),
      ( id:'860'    ; cp:860   ),
      ( id:'861'    ; cp:861   ),
      ( id:'862'    ; cp:862   ),
      ( id:'863'    ; cp:863   ),
      ( id:'864'    ; cp:864   ),
      ( id:'865'    ; cp:865   ),
      ( id:'866'    ; cp:866   ),
      ( id:'869'    ; cp:869   ),
      ( id:'932'    ; cp:932   ),
      ( id:'936'    ; cp:936   ),
      ( id:'949'    ; cp:949   ),
      ( id:'950'    ; cp:950   ),
      ( id:'ALARABI'; cp:709   ),
      ( id:'1250'   ; cp:1250  ),
      ( id:'1251'   ; cp:1251  ),
      ( id:'1252'   ; cp:1252  ),
      ( id:'1253'   ; cp:1253  ),
      ( id:'1254'   ; cp:1254  ),
      ( id:'1255'   ; cp:1255  ),
      ( id:'1256'   ; cp:1256  ),
      ( id:'1257'   ; cp:1257  ),
      ( id:'1258'   ; cp:1258  ),
      ( id:'Big5'   ; cp:850   ),
      ( id:'SJIS'   ; cp:832   ),
      ( id:'88591'  ; cp:28591 ),
      ( id:'88592'  ; cp:28592 ),
      ( id:'88593'  ; cp:28593 ),
      ( id:'88594'  ; cp:28594 ),
      ( id:'88595'  ; cp:28595 ),
      ( id:'88596'  ; cp:28596 ),
      ( id:'88597'  ; cp:28597 ),
      ( id:'88598'  ; cp:28598 ),
      ( id:'88599'  ; cp:28599 ),
      ( id:'885910' ; cp:28600 ),
      ( id:'885911' ; cp:874   ),
      ( id:'885913' ; cp:28603 ),
      ( id:'885915' ; cp:28605 ),
      ( id:'UTF-8'  ; cp:65001 ),
      ( id:'UTF8'   ; cp:65001 )
    ) ;
  {$ENDIF}
  {$IFDEF OXYGENE}
    [
      new T_cpgCodePage( '437'    , 437   ),
      new T_cpgCodePage( '708'    , 708   ),
      new T_cpgCodePage( '720'    , 720   ),
      new T_cpgCodePage( '737'    , 737   ),
      new T_cpgCodePage( '775'    , 775   ),
      new T_cpgCodePage( '850'    , 850   ),
      new T_cpgCodePage( '852'    , 852   ),
      new T_cpgCodePage( '855'    , 855   ),
      new T_cpgCodePage( '857'    , 857   ),
      new T_cpgCodePage( '860'    , 860   ),
      new T_cpgCodePage( '861'    , 861   ),
      new T_cpgCodePage( '862'    , 862   ),
      new T_cpgCodePage( '863'    , 863   ),
      new T_cpgCodePage( '864'    , 864   ),
      new T_cpgCodePage( '865'    , 865   ),
      new T_cpgCodePage( '866'    , 866   ),
      new T_cpgCodePage( '869'    , 869   ),
      new T_cpgCodePage( '932'    , 932   ),
      new T_cpgCodePage( '936'    , 936   ),
      new T_cpgCodePage( '949'    , 949   ),
      new T_cpgCodePage( '950'    , 950   ),
      new T_cpgCodePage( 'ALARABI', 709   ),
      new T_cpgCodePage( '1250'   , 1250  ),
      new T_cpgCodePage( '1251'   , 1251  ),
      new T_cpgCodePage( '1252'   , 1252  ),
      new T_cpgCodePage( '1253'   , 1253  ),
      new T_cpgCodePage( '1254'   , 1254  ),
      new T_cpgCodePage( '1255'   , 1255  ),
      new T_cpgCodePage( '1256'   , 1256  ),
      new T_cpgCodePage( '1257'   , 1257  ),
      new T_cpgCodePage( '1258'   , 1258  ),
      new T_cpgCodePage( 'Big5'   , 850   ),
      new T_cpgCodePage( 'SJIS'   , 832   ),
      new T_cpgCodePage( '88591'  , 28591 ),
      new T_cpgCodePage( '88592'  , 28592 ),
      new T_cpgCodePage( '88593'  , 28593 ),
      new T_cpgCodePage( '88594'  , 28594 ),
      new T_cpgCodePage( '88595'  , 28595 ),
      new T_cpgCodePage( '88596'  , 28596 ),
      new T_cpgCodePage( '88597'  , 28597 ),
      new T_cpgCodePage( '88598'  , 28598 ),
      new T_cpgCodePage( '88599'  , 28599 ),
      new T_cpgCodePage( '885910' , 28600 ),
      new T_cpgCodePage( '885911' , 874   ),
      new T_cpgCodePage( '885913' , 28603 ),
      new T_cpgCodePage( '885915' , 28605 ),
      new T_cpgCodePage( 'UTF-8'  , 65001 ),
      new T_cpgCodePage( 'UTF8'   , 65001 )
    ] ;
  {$ENDIF}

  // Read CPG file
  // _path       file path to be read
  // _code_page  code page to be used if CPG does not exist or cannot
  //             be interpreted
  // return     code page
  function read_CPG(
    const _path      : String;
    const _code_page : Integer
  ) : Integer ;
  var
    {$IFDEF DCC}
    r   : T_cpgCodePage ;
    {$ENDIF}
    lst : TStringList ;
    id  : String      ;
  begin
    Result := _code_page ;
    if not FileExists( _path ) then
      exit ;

    lst := TStringList.Create ;
    try
      lst.LoadFromFile( _path );
      if lst.Count >= 1 then
        id := Trim( lst[0] )
      else
        id := '' ;
    finally
      FreeObject( lst ) ;
    end ;

    for r in cpgCodePages do begin
      if CompareText( r.id, id ) = 0 then begin
        Result := r.cp ;
        break ;
      end ;
    end ;
  end ;

  // Write CPG file
  // _path       file path to be write
  // _code_page  code page to be written
  procedure write_CPG(
    const _path      : String ;
    const _code_page : Integer
  ) ;
  var
    {$IFDEF DCC}
    r   : T_cpgCodePage ;
    {$ENDIF}
    lst : TStringList ;
    id  : String      ;
  begin
    id := '' ;
    for r in cpgCodePages do begin
      if r.cp = _code_page then begin
        id := r.id ;
        break ;
      end ;
    end ;

    if id  <> '' then begin
      lst := TStringList.Create ;
      try
        lst.Text := id ;
        lst.SaveToFile( _path );
      finally
        FreeObject( lst ) ;
      end ;
    end ;

  end ;

//=============================================================================
// TGIS_LayerSHP
//=============================================================================

  constructor TGIS_LayerSHP.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable
                            ] ;
    shapeFile  := nil ;
    dataFile   := nil ;
    indexFile  := nil ;

    FCodePage := 65001 ; // UTF-8 by default

    ignoreHeaderShapeType := False ;
    fixXYZM := False ;
    firstXYZMRead := True ;
    firstXYZMLog := True ;
  end ;

  procedure TGIS_LayerSHP.doDestroy ;
  var
    i : Integer ;
  begin
    FreeObject( shapeFile      ) ;
    FreeObject( indexFile      ) ;
    FreeObject( dataFile       ) ;

    for i := 0 to high( cursorShp ) do begin
      FreeObject( cursorShp[i].curShapePoint      ) ;
      FreeObject( cursorShp[i].curShapeMultipoint ) ;
      FreeObject( cursorShp[i].curShapeMultiPatch ) ;
      FreeObject( cursorShp[i].curShapeArc        ) ;
      FreeObject( cursorShp[i].curShapePolygon    ) ;
    end ;

    inherited ;
  end ;

  function TGIS_LayerSHP.fget_CodePage : Integer ;
  begin
    Result := FCodePage ;
    if Result > 0 then exit ;

    Result := GisMetadataAsInteger( SHP_METADATA_DEFAULTCODEPAGE, 0 ) ;

    if Result = 0 then
      Result := inherited fget_CodePage ;
  end ;

  function TGIS_LayerSHP.fget_PathDBF : String ;
  begin
    Result := GetPathNoExt(Path) + getFileExtDBF ;
  end ;

  function TGIS_LayerSHP.fget_Path : String ;
  begin
    Result := inherited fget_Path;
  end ;

  function TGIS_LayerSHP.fget_PathSHX : String ;
  begin
    Result := GetPathNoExt(Path) + getFileExtSHX ;
  end ;

  function TGIS_LayerSHP.fget_PathPRJ : String ;
  begin
    Result := GetPathNoExt(Path) + getFileExtPRJ ;
  end ;

  function TGIS_LayerSHP.fget_PathFLD : String ;
  begin
    Result := PathSHP + getFileExtFLD ;
  end ;

  function TGIS_LayerSHP.fget_PathCPG : String ;
  begin
    Result := GetPathNoExt(Path) + getFileExtCPG ;
  end ;

  function TGIS_LayerSHP.fget_PathRTree : String ;
  begin
    Result := ExpandForcedRTreePath(
                PathSHP + GIS_RTREE_EXT
              ) ;
  end ;

  function TGIS_LayerSHP.getFileExtDBF : String ;
  begin
    Result := DBF_EXT ;
  end ;

  function TGIS_LayerSHP.getFileExtSHX : String ;
  begin
    Result := SHX_EXT ;
  end ;

  function TGIS_LayerSHP.getFileExtPRJ : String ;
  begin
    Result := GIS_PRJ_EXT ;
  end ;

  function TGIS_LayerSHP.getFileExtFLD : String ;
  begin
    Result := FLD_EXT ;
  end ;

  function TGIS_LayerSHP.getFileExtCPG : String ;
  begin
    Result := CPG_EXT ;
  end ;

  function TGIS_LayerSHP.verifyDimensionFromBuffer(
   {$IFDEF MANAGED}
     const _ptr   : TGIS_Bytes     ;
   {$ELSE}
     const _ptr   : Pointer        ;
   {$ENDIF}
   const _shptype : TGIS_ShapeType ;
   const _len     : Integer
  ) : TGIS_DimensionType ;
  var
    len  : Integer ;
    off  : Integer ;
    zlen : Integer ;
    hasZ : Boolean ;
    hasM : Boolean ;
  begin
    Result := TGIS_DimensionType.XYZM ;

    len  := _len * 2 - 4 ;
    zlen := 0 ;

    case _shptype of
      TGIS_ShapeType.Arc,
      TGIS_ShapeType.Polygon     : begin
        {$IFDEF MANAGED}
          zlen := sizeOf(Double)*(_ptr.ReadInt32( 36 ) ) ;
          off  := 36 + sizeOf(DWORD)*(_ptr.ReadInt32( 32 ) + 1 ) +
                  SIZEOF_TGIS_POINT*(_ptr.ReadInt32( 36 ) + 1 )  ;
        {$ELSE}
          zlen := sizeOf(Double)*(PInteger( NativeInt(_ptr) + 36 )^ ) ;
          off  := 36 + sizeOf(DWORD)*(PInteger( NativeInt(_ptr) + 32 )^ +1 ) +
                  sizeOf(TGIS_Point)*(PInteger( NativeInt(_ptr) + 36 )^ + 1 ) ;
        {$ENDIF}
      end ;
      TGIS_ShapeType.Point       : begin
        zlen := 8 ;
        off  := SIZEOF_TGIS_POINT  ;
      end ;
      TGIS_ShapeType.MultiPoint  : begin
        {$IFDEF MANAGED}
          zlen := sizeOf(Double)*(_ptr.ReadInt32( SIZEOF_TGIS_EXTENT ) ) ;
          off  := 36 + SIZEOF_TGIS_POINT*(_ptr.ReadInt32(
                  SIZEOF_TGIS_EXTENT ) + 1 ) ;
        {$ELSE}
          zlen := sizeOf(Double)*(PInteger( NativeInt(_ptr) + sizeOf(TGIS_Extent) )^ ) ;
          off  := 36 + sizeOf(TGIS_Point)*(PInteger( NativeInt(_ptr) +
                  sizeOf(TGIS_Extent) )^ + 1 ) ;
        {$ENDIF}
      end ;
      TGIS_ShapeType.MultiPatch : begin
        {$IFDEF MANAGED}
          zlen := sizeOf(Double)*(_ptr.ReadInt32( 36 ) ) ;
          off  := 36 + 2*sizeOf(DWORD)*(_ptr.ReadInt32( 32 ) + 1 ) +
                  SIZEOF_TGIS_POINT*(_ptr.ReadInt32( 36 ) + 1 )  ;
        {$ELSE}
          zlen := sizeOf(Double)*(PInteger( NativeInt(_ptr) + 36 )^ ) ;
          off  := 36 + 2*sizeOf(DWORD)*(PInteger( NativeInt(_ptr) + 32 )^ +1 ) +
                  sizeOf(TGIS_Point)*(PInteger( NativeInt(_ptr) + 36 )^ + 1 ) ;
        {$ENDIF}

      end
    else  off := len ;
    end ;

    hasZ := len > off ;
    hasM := len > ( off + zlen ) ;

    if not hasM then
      Result := TGIS_DimensionType.XYZ ;
    if not hasZ then
      Result := TGIS_DimensionType.XY ;
  end ;

  function TGIS_LayerSHP.allowedNameLength : Integer ;
  begin
    if GisMetadataAsBoolean( SHP_METADATA_SAFEFIELDNAME, False ) then
      Result := SHP_MAX_NAME_LENGTH_MAX
    else
      Result := SHP_MAX_NAME_LENGTH_SAFE ;
  end ;

  procedure TGIS_LayerSHP.readShape(
    const _uid    : TGIS_Uid ;
    const _cursor : Integer
  ) ;
  var
    tmp_uid    : TGIS_Uid        ;
    {$IFDEF MANAGED}
      shaperec : TGIS_RecordSHP ;
    {$ELSE}
      shaperec : PGIS_RecordSHP ;
      pbuf     : Pointer        ;
    {$ENDIF}
  begin
    {$IFDEF MANAGED}
      {$IFDEF JAVA}
        shaperec := shapeFile.ReadRecord( _cursor ) ;
      {$ELSE}
        shapeFile.ReadRecord( shaperec, _cursor )     ; // get .shp record
      {$ENDIF}
      cursorShp[_cursor].curShapeType := shaperec.ShapeType  ;
    {$ELSE}
      shaperec := shapeFile.ReadRecord( _cursor ) ; // get .shp record
      cursorShp[_cursor].curShapeType := shaperec^.ShapeType ;
      pbuf := shapeFile.ReadOffset( _cursor ) ;
    {$ENDIF}

    if shaperec.Dimension = TGIS_DimensionType.XYZM then begin
      if fixXYZM then
        shaperec.Dimension := TGIS_DimensionType.XYZ
      else
      if ignoreHeaderShapeType or firstXYZMRead then begin
        shaperec.Dimension := verifyDimensionFromBuffer(
          {$IFDEF MANAGED}
            shaperec.Contents,
          {$ELSE}
            pbuf,
          {$ENDIF}
          shaperec.ShapeType,
          shaperec.ContentLength
        ) ;

        // verify the layer dimension based on first shape
        if firstXYZMRead then begin
          firstXYZMRead := False ;
          FSupportedDimensions := GisGetDimensionType( shaperec.Dimension ) ;
          DefaultDimension     := shaperec.Dimension ;

          if ( not ignoreHeaderShapeType ) and
             ( shaperec.Dimension <> TGIS_DimensionType.XYZM )
          then
            fixXYZM := True ;
        end ;

        // log the fix
        if firstXYZMLog and
           ( shaperec.Dimension <> TGIS_DimensionType.XYZM )
        then begin
          firstXYZMLog := False ;

          TGIS_Logger.AsMessage(
            GetClassName(self) + ' improper XYZM file detected, forced XYZ mode:',
            Path
          ) ;
        end ;

      end ;
    end ;

    if _uid < 0 then tmp_uid := shaperec.RecordNumber
                else tmp_uid := _uid ;
    case shaperec.ShapeType of
      TGIS_ShapeType.Point :
          begin
            {$IFDEF MANAGED}
              cursorShp[_cursor].curShapePoint.Recreate(
                nil, shaperec.Contents, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ELSE}
              cursorShp[_cursor].curShapePoint.Recreate(
                nil, pbuf, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ENDIF}
            cursorShp[_cursor].curShape :=
              getEdited( cursorShp[_cursor].curShapePoint ) ;
          end ;
      TGIS_ShapeType.MultiPoint :
          begin
            {$IFDEF MANAGED}
              cursorShp[_cursor].curShapeMultipoint.Recreate(
                nil, shaperec.Contents, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ELSE}
              cursorShp[_cursor].curShapeMultipoint.Recreate(
                nil, pbuf, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ENDIF}
            cursorShp[_cursor].curShape
              := getEdited( cursorShp[_cursor].curShapeMultipoint ) ;
          end ;
      TGIS_ShapeType.Arc :
          begin
            {$IFDEF MANAGED}
              cursorShp[_cursor].curShapeArc.Recreate(
                nil, shaperec.Contents, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ELSE}
              cursorShp[_cursor].curShapeArc.Recreate(
                nil, pbuf, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ENDIF}
            cursorShp[_cursor].curShape :=
              getEdited( cursorShp[_cursor].curShapeArc ) ;
          end ;
      TGIS_ShapeType.Polygon :
          begin
            {$IFDEF MANAGED}
              cursorShp[_cursor].curShapePolygon.Recreate(
                nil, shaperec.Contents, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ELSE}
              cursorShp[_cursor].curShapePolygon.Recreate(
                nil, pbuf, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ENDIF}
            cursorShp[_cursor].curShape :=
              getEdited( cursorShp[_cursor].curShapePolygon ) ;
          end ;
      TGIS_ShapeType.MultiPatch :
          begin
            {$IFDEF MANAGED}
              cursorShp[_cursor].curShapeMultiPatch.Recreate(
                nil, shaperec.Contents, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ELSE}
              cursorShp[_cursor].curShapeMultiPatch.Recreate(
                nil, pbuf, True, tmp_uid, self, shaperec.Dimension
              ) ;
            {$ENDIF}
            cursorShp[_cursor].curShape :=
              getEdited( cursorShp[_cursor].curShapeMultiPatch ) ;
          end ;
      else
        cursorShp[_cursor].curShapeType := TGIS_ShapeType.Unknown ;
    end ;
  end ;

  function TGIS_LayerSHP.getFieldInternal( const _uid      : TGIS_Uid;
                                           const _name     : String ;
                                           const _cursor   : Integer
                                         ) : Variant ;
  begin
    lockThread ;
    try
      Result := dataFile.GetField( _uid, _name, -1 ) ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSHP.getBindedFieldInternal( const _shape  : TObject ;
                                                 const _field  : Integer ;
                                                 const _cursor : Integer
                                               ) : Variant ;
  var
    shp : TGIS_Shape ;
  begin
    lockThread ;
    try
      shp := TGIS_Shape( _shape ) ;
      if assigned( shp ) then
        Result := dataFile.GetField( shp.Uid, '', _field )
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSHP.setUp ;
  var
    i : Integer ;
  begin
    inherited ;

    ignoreHeaderShapeType := GisMetadataAsBoolean( SHP_METADATA_IGNORE_HDR_SHPTYPE,
                                                   False
                                                 ) ;
    // open shape file
       lastUid   := -1 ;
       if not assigned( shapeFile ) then
         shapeFile := TGIS_FileSHP.Create(
                        PathSHP, TGIS_StreamMode.Read, FOnRead, FOnWrite
                      ) ;

       for i := 0 to length( cursorShp ) - 1 do begin
         if cursorShp[i].curInUse then begin
           shapeFile.CursorOpen( i ) ;
         end ;
       end ;

       shapeFile.ReadHeader(0) ;
       Extent3D := shapeFile.Extent ;

       case shapeFile.ShapeType of
         1  : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Point      ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XY ) ;
              end ;
         11 : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Point        ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYZM ) ;
              end ;
         21 : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Point       ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYM ) ;
              end ;
         3  : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Arc        ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XY ) ;
              end ;
         13 : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Arc          ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYZM ) ;
              end ;
         23 : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Arc         ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYM ) ;
              end ;
         5  : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Polygon    ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XY ) ;
              end ;
         15 : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Polygon      ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYZM ) ;
              end ;
         25 : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.Polygon     ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYM ) ;
              end ;
         8  : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.MultiPoint ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XY ) ;
              end ;
         18 : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.MultiPoint   ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYZM ) ;
              end ;
         28 : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.MultiPoint  ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYM ) ;
              end ;
         31 : begin
                FSupportedShapes     := GisGetShapeType( TGIS_ShapeType.MultiPatch   ) ;
                FSupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XYZM ) ;
              end ;
       else   begin
                FSupportedShapes     := GisGetEmptyShapeType ;
                FSupportedDimensions := GisGetEmptyDimensionType ;
              end ;
       end ;

    // open index file
       if SafeFileExists( PathSHX ) then begin
         if not assigned( indexFile ) then
           indexFile := TGIS_FileSHX.Create(
                          PathSHX, TGIS_StreamMode.Read, FOnRead, FOnWrite
                        ) ;
         for i := 0 to length( cursorShp ) - 1 do begin
           if cursorShp[i].curInUse then begin
             indexFile.CursorOpen( i ) ;
           end ;
         end ;
         indexFile.ReadHeader(0) ;
       end
       else
         indexFile := nil ;

    // open data file
      if not assigned( dataFile ) then
         dataFile  := TGIS_FileDBF.Create(
                        PathDBF, Self, TGIS_StreamMode.Read, FOnRead, FOnWrite
                      ) ;

    // set code page
      CodePage := read_CPG( PathCPG, CodePage ) ;

    // set projection
      self.SetCSByWKTFile( PathPRJ );

    if SafeFileExists( PathSHP ) then
      FAge := GisFileAge( PathSHP ) ;

    FFileInfo := 'ArcView Shape Files (SHP)' ;

    if GisTestDimensionType( TGIS_DimensionType.XYZ  , SupportedDimensions ) or
       GisTestDimensionType( TGIS_DimensionType.XYM  , SupportedDimensions ) or
       GisTestDimensionType( TGIS_DimensionType.XYZM , SupportedDimensions ) then
      FFileInfo := FFileInfo + ' 3D';
  end ;

  procedure TGIS_LayerSHP.Build( const _path   : String ;
                                 const _extent : TGIS_Extent;
                                 const _type   : TGIS_ShapeType ;
                                 const _dim    : TGIS_DimensionType
                                ) ;
  var
    shape_file : TGIS_FileSHP ;
    dbf_file   : TGIS_FileDBF ;
    dbf_path   : String ;
    prj_path   : String ;
  begin
    inherited ;

    PrepareExportFieldNames( allowedNameLength ) ;

    dbf_path := GetPathNoExt(_path) + getFileExtDBF ;
    prj_path := GetPathNoExt(_path) + getFileExtPRJ ;

    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;
    if SafeFileExists( dbf_path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), dbf_path, 0 ) ;
    end ;

    shape_file := TGIS_FileSHP.Create( _path, TGIS_StreamMode.&Create, FOnRead, FOnWrite ) ;
    try
      shape_file.Extent    := GisExtent3DFrom2D( _extent ) ;
      shape_file.ShapeType := shape_file.ShapeTypeToInt( _type, _dim ) ;
      shape_file.WriteHeader ;

      Extent3D            := shape_file.Extent ;
      SupportedShapes     := GisAddShapeType( FSupportedShapes, _type ) ;
      SupportedDimensions := GisAddDimensionType( FSupportedDimensions, _dim ) ;
    finally
      FreeObject( shape_file ) ;
    end ;

    dbf_file := TGIS_FileDBF.Create(
                  dbf_path, self, TGIS_StreamMode.&Create, FOnRead, FOnWrite
                ) ;
    try
      dbf_file.WriteHeader ;
      dbf_file.WriteStructure ;
      dbf_file.WriteHeader ;
    finally
      FreeObject( dbf_file ) ;
    end ;
    ExportStructureToFLD ;

    CS.SaveAsWKTFile( prj_path ) ;

    write_CPG( GetPathNoExt(_path) + getFileExtCPG, CodePage );
  end ;

  procedure TGIS_LayerSHP.ImportLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent ;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape ;
    const _de9im     : String ;
    const _truncated : Boolean
  ) ;
  var
    shape_file : TGIS_FileSHP   ;
    index_file : TGIS_FileSHX   ;
    data_file  : TGIS_FileDBF   ;
    rtree_file : TGIS_RTree     ;
    rec        : TGIS_RecordSHP ;
    {$IFNDEF OXYGENE}
      shp      : TGIS_Shape     ;
    {$ENDIF}
    shp_tmp    : TGIS_Shape     ;
    shp_type   : TGIS_ShapeType ;
    shp_dim    : TGIS_DimensionType ;
    empty      : Boolean        ;
    same_name  : Boolean        ;
    shape_no   : Cardinal       ;
    end_uid    : TGIS_Uid       ;
    abort      : Boolean        ;
    old_scope  : String         ;
    isrtree    : Boolean        ;
    isfirst    : Boolean        ;
    zsize      : Integer        ;
    msize      : Integer        ;
    rangesize  : Integer        ;
    bwinding   : Boolean        ;
    c          : Integer        ;

    procedure rename_files ;
    begin
      try
        DeleteFile( GetBackupName( PathSHP   ) ) ;
        DeleteFile( GetBackupName( PathSHX   ) ) ;
        DeleteFile( GetBackupName( PathDBF   ) ) ;
        if isrtree then
          DeleteFile( GetBackupName( PathRTree ) ) ;
        DeleteFile( GetBackupName( PathPRJ ) ) ;

        RenameFile( PathSHP  , GetBackupName( PathSHP   ) ) ;
        RenameFile( PathSHX  , GetBackupName( PathSHX   ) ) ;
        RenameFile( PathDBF  , GetBackupName( PathDBF   ) ) ;
        if SafeFileExists( PathPRJ ) then
          RenameFile( PathPRJ  , GetBackupName( PathPRJ   ) ) ;
        if isrtree then
          RenameFile( PathRTree, GetBackupName( PathRTree ) ) ;

        try
          if not RenameFile( GetTemporaryName( PathSHP ), PathSHP   ) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), PathSHP,
                                         GetLastError
                                       ) ;
          if not RenameFile( GetTemporaryName( PathSHX ), PathSHX   ) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), PathSHX,
                                         GetLastError
                                       ) ;
          if not RenameFile( GetTemporaryName( PathDBF ), PathDBF   ) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), PathDBF,
                                         GetLastError
                                       ) ;
          self.CS.SaveAsWKTFile( PathPRJ ) ;
          write_CPG( PathCPG, CodePage );
          if isrtree then
            if not RenameFile( GetTemporaryName( PathRTree ), PathRTree ) then
              raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), PathRTree,
                                           GetLastError
                                         ) ;
        except
          // recover ;
          RenameFile( GetBackupName( PathSHP ), PathSHP ) ;
          RenameFile( GetBackupName( PathSHX ), PathSHX ) ;
          RenameFile( GetBackupName( PathDBF ), PathDBF ) ;
          RenameFile( GetBackupName( PathPRJ ), PathPRJ ) ;
          if isrtree then begin
            RenameFile( GetBackupName( PathRTree ), PathRTree ) ;
            DeleteFile( GetTemporaryName( PathRTree ) ) ;
          end ;

          raise ;
        end ;
      finally
        if same_name then begin
          Items.Clear ;
          Fields.Clear ;

          setUp ;
          setUp2 ;
          UseRTree := isrtree ;
          if assigned( Viewer ) then
            Viewer.Ref.InvalidateWholeMap ;
        end ;
      end ;
    end ;

    procedure checkPolygonWinding(
      const _ref : TGIS_Shape
    ) ;
    var
      numParts  : Integer ;
      cShp      : TGIS_Shape ;
      i, j      : Integer ;
      rewind    : Boolean ;
      pstatus   : TGIS_IntegerArray ;
      pwinding  : TGIS_IntegerArray ;
      cw        : Boolean ;
    begin
      if shp_tmp.IsEmpty then exit ;

      cShp := TGIS_ShapePolygon.Create( shp_tmp.Dimension ) ;
      try
        cShp.Layer := shp_tmp.Layer ;
        cShp.CopyFields( shp_tmp ) ;
        cShp.Reset ;
        cShp.Lock( TGIS_Lock.Extent ) ;
        shp_tmp.Lock( TGIS_Lock.Projection ) ;

        TGIS_GeometryFactory.GisPolygonPartsStatus( shp_tmp, pstatus, pwinding ) ;

        numParts := shp_tmp.GetNumParts ;

        for i := 0 to numParts - 1 do begin
          cShp.AddPart ;

          cw := pwinding[i] = 1 ;

          if i = 0 then                           // first ring must be CW
            rewind := not cw
          else if ( pstatus[i] = 1 ) then         // holes must be CCW
            rewind := cw
          else                                    // islands must be CW
            rewind := not cw  ;

          if not rewind then
            for j := 0 to shp_tmp.GetPartSize( i ) - 1 do
              cShp.AddPoint3D( shp_tmp.GetPoint3D( i, j ) )
          else
            for j := shp_tmp.GetPartSize( i ) - 1 downto 0 do
              cShp.AddPoint3D( shp_tmp.GetPoint3D( i, j ) )
        end ;
      finally
        shp_tmp.Unlock ;
        cShp.Unlock ;
        if _ref <> shp_tmp then
          FreeObject( shp_tmp ) ;
        shp_tmp := cShp ;
      end ;
    end ;

  begin
    if not assigned( _layer ) then exit ;

    if not ( CheckFileWriteAccessEx( PathSHP   , True , True, True ) and
             CheckFileWriteAccessEx( PathDBF   , True , True, True ) and
             CheckFileWriteAccessEx( PathSHX   , True , True, True ) and
             CheckFileWriteAccessEx( PathRTree , False, True, True )
           )
    then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try

      ImportStructure( _layer ) ;
      PrepareExportFieldNames( allowedNameLength ) ;

      same_name := CompareText( GetPathAbsolute( '', Path        ),
                                GetPathAbsolute( '', _layer.Path )
                              ) = 0  ;

      shp_type := _type ;
      shp_dim  := _layer.DefaultDimension ;
      empty    := True ;

      // prepare temporary geometry
      shape_file := TGIS_FileSHP.Create( GetTemporaryName( PathSHP ),
                                         TGIS_StreamMode.&Create, FOnRead, FOnWrite
                                       ) ;
      shape_file.Extent := GisNoWorld3D ;
      shape_file.WriteHeader ;

      index_file := TGIS_FileSHX.Create( GetTemporaryName( PathSHX ),
                                         TGIS_StreamMode.&Create, FOnRead, FOnWrite
                                       ) ;
      index_file.WriteHeader ;

      // prepare temporary database
      data_file := TGIS_FileDBF.Create( GetTemporaryName( PathDBF ), Self,
                                        TGIS_StreamMode.&Create, FOnRead, FOnWrite
                                      ) ;
      data_file.CodePage    := CodePage    ;
      data_file.WriteHeader ;
      data_file.WriteStructure ;

      ExportStructureToFLD ;

      // prepare RTRee index
      isrtree := _layer.UseRTree ;
      if isrtree then
        rtree_file := TGIS_RTree.Create( GetTemporaryName( PathRTree ) )
      else
        rtree_file := nil ;

      try
        old_scope := _layer.Scope ;
        if same_name then
          _layer.Scope := '' ;

        {$IFDEF GIS_NORECORDS}
          rec := new TGIS_RecordSHP ;
        {$ENDIF}
        rec.RecordNumber := 1 ;

        isfirst := True ;
        bwinding := GisMetadataAsBoolean( SHP_METADATA_CHECKWINDING, False ) ;

        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
            _layer.Loop( _extent, _scope, _shape, _de9im ) do
        begin
          shp_tmp := shp.PrepareExportShape(
                           CS, _extent, _truncated, True
                         ) ;
          if assigned( shp_tmp ) then
            try
              if ( not shp_tmp.IsDeleted ) and
                 ( ( shp_tmp.ShapeType = shp_type   ) or
                   ( shp_type = TGIS_ShapeType.Unknown )
                 )
              then begin
                if bwinding and (shp_tmp.ShapeType = TGIS_ShapeType.Polygon) then
                  checkPolygonWinding( shp ) ;

                if isfirst then begin
                  shape_file.Extent := shp_tmp.Extent3D ;
                  isfirst := False ;
                end
                else begin
                  shape_file.Extent := GisMaxExtent3D( shape_file.Extent,
                                                     shp_tmp.Extent3D
                                                   ) ;
                end ;

                zsize := 0 ;
                msize := 0 ;
                if shp_tmp.ShapeType = TGIS_ShapeType.Point then
                  rangesize := 0
                else
                  rangesize := 16 ;

                if ( shp_tmp.Dimension in [ TGIS_DimensionType.XYZ,
                                            TGIS_DimensionType.XYZM ] ) or
                   ( shp_tmp.ShapeType = TGIS_ShapeType.MultiPatch  ) then
                  zsize := rangesize + shp_tmp.PointsZSize ;

                if ( shp_tmp.Dimension in [ TGIS_DimensionType.XYZ,
                                            TGIS_DimensionType.XYM,
                                            TGIS_DimensionType.XYZM ] ) or
                   ( shp_tmp.ShapeType = TGIS_ShapeType.MultiPatch  ) then
                  msize := rangesize + shp_tmp.PointsMSize ;

                rec.ContentLength := ( shp_tmp.PartsSize + shp_tmp.PartTypesSize +
                                       shp_tmp.PointsSize + zsize + msize + 4
                                     ) div 2 ;
                rec.ShapeType     := shp_tmp.ShapeType ;
                rec.Dimension     := shp_tmp.Dimension ;

                if shp_type = TGIS_ShapeType.Unknown then
                  shp_type := shp_tmp.ShapeType ;

                shp_dim := shp_tmp.Dimension ;
                index_file.WriteRecord( shape_file.Position, rec ) ;
                shape_file.WriteRecord( rec ) ;
                shape_file.WriteGeometry( shp_tmp ) ;

                if assigned( rtree_file ) then
                  rtree_file.Insert( shp_tmp.Extent, rec.RecordNumber );

                data_file.AddRecord( shp ) ;
                empty := False ;
                inc( rec.RecordNumber ) ;
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

        if abort then begin
          FreeObject( shape_file ) ;
          FreeObject( index_file ) ;
          FreeObject( data_file  ) ;
          FreeObject( rtree_file ) ;

          DeleteFile( GetTemporaryName( PathSHP   ) ) ;
          DeleteFile( GetTemporaryName( PathSHX   ) ) ;
          DeleteFile( GetTemporaryName( PathDBF   ) ) ;
          DeleteFile( GetTemporaryName( PathRTree ) ) ;
          DeleteFile( GetTemporaryName( PathPRJ   ) ) ;
        end
        else begin
          if empty and assigned( shapeFile ) then begin
            shape_file.Extent    := _TGIS_Extent3D(shapeFile.Extent)    ;
            shape_file.ShapeType := shapeFile.ShapeType ;
          end
          else
          if empty and ( _layer = self ) then begin // saving empty SHP file
            shape_file.Extent    := _layer.Extent3D ;
            shape_file.ShapeType := shape_file.ShapeTypeToInt(
                                      _layer.DefaultShapeType,
                                      _layer.DefaultDimension
                                    ) ; ;
          end
          else
            shape_file.ShapeType := shape_file.ShapeTypeToInt(
                                      shp_type,
                                      shp_dim
                                    ) ;
          shape_file.WriteHeader ;
          if same_name then FreeObject( shapeFile ) ;

          index_file.ShapeType := shape_file.ShapeTypeToInt( shp_type, shp_dim ) ;
          index_file.Extent    := shape_file.Extent    ;
          index_file.WriteHeader ;
          FreeObject( index_file ) ;
          FreeObject( shape_file ) ;
          if same_name then FreeObject( indexFile ) ;

          data_file.WriteHeader ;
          FreeObject( data_file ) ;
          if same_name then FreeObject( dataFile ) ;

          if assigned( rtree_file ) then begin
            rtree_file.Commit( True ) ;
            FreeObject( rtree_file ) ;
            if same_name then begin
              FreeObject( FRTreeFile ) ;
              UseRTree := False ;
            end ;
          end ;

          rename_files ;
          write_CPG( PathCPG, CodePage );
        end ;

        for c := 0 to BUILTIN_CURSORS - 1 do
          cursorShp[c].curShape := nil ;

        FIsModified := False ;

        if not FIsOpened then
          Open ;
      end ;

    finally
      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  procedure TGIS_LayerSHP.MergeLayerEx(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent;
    const _type        : TGIS_ShapeType ;
    const _scope       : String ;
    const _shape       : TGIS_Shape ;
    const _de9im       : String ;
    const _truncated   : Boolean;
    const _restrict    : Boolean
  )  ;
  var
    shape_file : TGIS_FileSHP   ;
    index_file : TGIS_FileSHX   ;
    data_fileT : TGIS_FileDBF   ;
    data_file  : TGIS_FileDBF   ;
    rec        : TGIS_RecordSHP ;
    {$IFNDEF OXYGENE}
      shp      : TGIS_Shape     ;
    {$ENDIF}
    shp_tmp    : TGIS_Shape     ;
    shp_dim    : TGIS_DimensionType;
    shp_type   : TGIS_ShapeType ;
    shape_no   : Cardinal       ;
    end_uid    : TGIS_Uid       ;
    abort      : Boolean        ;
    isrtree    : Boolean        ;
    last_uid   : TGIS_Uid       ;
    ll         : TGIS_LayerVector;
    zsize      : Integer        ;
    msize      : Integer        ;
    rangesize  : Integer        ;
  begin
    if not assigned( _layer ) then exit ;

    SaveData ; // save current data before the merge

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    last_uid := GetLastUid ;
    abort    := False ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;

    ll := TGIS_LayerVector.Create;
    try
      ll.ImportStructure( self );
      MergeStructure( _layer, _restrict ) ;
      PrepareExportFieldNames( allowedNameLength ) ;

      // close attached files before merge
      FreeObject( shapeFile );
      FreeObject( indexFile );
      FreeObject( dataFile  );
      FreeObject( FRTreeFile ) ;
      UseRTree := False ;

      shp_type := _type ;
      shp_dim  := _layer.DefaultDimension ;

      // prepare temporary geometry
      shape_file := TGIS_FileSHP.Create( PathSHP,
                                         TGIS_StreamMode.Append, FOnRead, FOnWrite
                                       ) ;
      shape_file.Extent := Extent3D ;
       shape_file.WriteHeader ;

      if SafeFileExists( PathSHX ) then
        index_file := TGIS_FileSHX.Create( PathSHX,
                                           TGIS_StreamMode.Append, FOnRead, FOnWrite )
      else
        index_file := TGIS_FileSHX.Create( PathSHX,
                                           TGIS_StreamMode.&Create, FOnRead, FOnWrite ) ;
      index_file.WriteHeader ;

      // prepare temporary database
      data_file := TGIS_FileDBF.Create( PathDBF, ll,
                                         TGIS_StreamMode.Read, FOnRead, FOnWrite
                                       ) ;
      data_fileT := TGIS_FileDBF.Create( GetTemporaryName( PathDBF ), Self,
                                         TGIS_StreamMode.&Create, FOnRead, FOnWrite
                                       ) ;
      // copy header
      data_file.CopyHeader( data_fileT );
      data_fileT.WriteHeader;

      // write structure
      data_fileT.WriteStructure ;
      data_fileT.WriteHeader;

      // copy records
      data_file.CopyRecords( data_fileT, self );
      FreeObject( data_file   );
      data_fileT.SkipEndMarker := True ;
      FreeObject( data_fileT  );

      // change original file name and attach a copy
      DeleteFile( GetBackupName( PathDBF ) );
      RenameFile( PathDBF, GetBackupName( PathDBF ) ) ;
      RenameFile( GetTemporaryName( PathDBF ), PathDBF ) ;

      data_file := TGIS_FileDBF.Create( PathDBF, Self,
                                        TGIS_StreamMode.Append, FOnRead, FOnWrite
                                      ) ;
      data_file.ReadHeader( False ) ;

      isrtree := _layer.UseRTree ;
      try
        {$IFDEF GIS_NORECORDS}
          rec := new TGIS_RecordSHP ;
        {$ENDIF}
        rec.RecordNumber := last_uid + 1 ;

        // move position to the end
        shape_file.Position := shape_file.Size  ;
        index_file.Position := index_file.Size  ;
        data_file.Position  := data_file.Size   ;

        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
            _layer.Loop( _extent, _scope, _shape, _de9im ) do
        begin
          shp_tmp := shp.PrepareExportShape(
                           CS, _extent, _truncated, True
                         ) ;
          if assigned( shp_tmp ) then
            try
              if ( not shp_tmp.IsDeleted ) and
                 ( ( shp_tmp.ShapeType = shp_type   ) or
                   ( shp_type = TGIS_ShapeType.Unknown )
                 )
              then begin
                shape_file.Extent := GisMaxExtent3D( shape_file.Extent,
                                                   shp_tmp.Extent3D
                                                 ) ;

                zsize := 0 ;
                msize := 0 ;
                if shp_tmp.ShapeType = TGIS_ShapeType.Point then
                  rangesize := 0
                else
                  rangesize := 16 ;

                if ( shp_tmp.Dimension in [ TGIS_DimensionType.XYZ,
                                            TGIS_DimensionType.XYZM ] ) or
                   ( shp_tmp.ShapeType = TGIS_ShapeType.MultiPatch  ) then
                  zsize := rangesize + shp_tmp.PointsZSize ;

                if ( shp_tmp.Dimension in [ TGIS_DimensionType.XYZ,
                                            TGIS_DimensionType.XYM,
                                            TGIS_DimensionType.XYZM ] ) or
                   ( shp_tmp.ShapeType = TGIS_ShapeType.MultiPatch  ) then
                  msize := rangesize + shp_tmp.PointsMSize ;

                rec.ContentLength := ( shp_tmp.PartsSize + shp_tmp.PartTypesSize +
                                       shp_tmp.PointsSize + zsize + msize + 4
                                     ) div 2 ;

                rec.Dimension     := shp_tmp.Dimension ;
                rec.ShapeType     := shp_tmp.ShapeType ;

                if shp_type = TGIS_ShapeType.Unknown then
                  shp_type := shp_tmp.ShapeType ;

                shp_dim := shp_tmp.Dimension ;

                index_file.WriteRecord( shape_file.Position, rec ) ;
                shape_file.WriteRecord( rec ) ;
                shape_file.WriteGeometry( shp_tmp ) ;

                data_file.AddRecord( shp ) ;
                inc( rec.RecordNumber ) ;
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
        shape_file.ShapeType := shape_file.ShapeTypeToInt( shp_type, shp_dim ) ;
        shape_file.WriteHeader ;

        index_file.ShapeType := shape_file.ShapeTypeToInt( shp_type, shp_dim ) ;
        index_file.Extent    := shape_file.Extent    ;
        index_file.WriteHeader ;
        FreeObject( index_file ) ;
        FreeObject( shape_file ) ;

        data_file.WriteHeader ;
        FreeObject( data_file ) ;

        DeleteFile( GetBackupName( PathDBF ) );

        Items.Clear ;
        Fields.Clear ;

        setUp ;
        UseRTree := isrtree ;

        if assigned( Viewer ) then
          Viewer.Ref.InvalidateWholeMap ;
        FIsModified := False ;
      end ;
    finally
      FreeObject( ll );

      RaiseBusyRelease( _layer ) ;
    end ;

  end ;

  function  TGIS_LayerSHP.cursorOpen   :  Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length( cursorShp )  then
        SetLength( cursorShp, Result + 1 ) ;
      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorShp[Result] ) then
          cursorShp[Result] := new T_cursorShp ;
      {$ENDIF}
      cursorShp[Result].curInUse := True ;

      cursorShp[Result].curShapePoint      := TGIS_ShapePoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorShp[Result].curShapeMultipoint := TGIS_ShapeMultiPoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorShp[Result].curShapeArc        := TGIS_ShapeArc.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorShp[Result].curShapePolygon    := TGIS_ShapePolygon.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorShp[Result].curShapeMultiPatch := TGIS_ShapeMultiPatch.Create(
                                                nil, nil, False, -1, nil
                                              ) ;

      if assigned( shapeFile ) then
        shapeFile.CursorOpen( Result );
      if assigned( indexFile ) then
        indexFile.CursorOpen( Result );
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSHP.cursorClose  ( const _cursor      : Integer
                                       ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorShp[_cursor].curInUse := False ;
      FreeObject( cursorShp[_cursor].curShapePoint      ) ;
      FreeObject( cursorShp[_cursor].curShapeMultipoint ) ;
      FreeObject( cursorShp[_cursor].curShapeArc        ) ;
      FreeObject( cursorShp[_cursor].curShapePolygon    ) ;
      FreeObject( cursorShp[_cursor].curShapeMultiPatch ) ;

      // truncate cursorState at the tail;
      for i := length( cursorShp ) - 1 downto 0 do begin
        if not cursorShp[i].curInUse then begin
          SetLength( cursorShp, i ) ;
        end
        else
          break ;
      end ;

      inherited cursorClose( _cursor ) ;

      if assigned( shapeFile ) then
        shapeFile.CursorClose( _cursor );
      if assigned( indexFile ) then
        indexFile.CursorClose( _cursor );
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSHP.cursorFirst( const _cursor      : Integer          ;
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
      cursorShp[_cursor].curShape := nil ;

      if GisIsNoWorld( _extent ) then
        exit ;

      inherited cursorFirstInternal(
                  _cursor, _viewerCS,
                  _extent, _query, _shape, _de9im, _skipDeleted
                ) ;

      if assigned( shapeFile ) then begin
        shapeFile.FlushBuffer ;
        shapeFile.ReadHeader( _cursor ) ;
      end ;
      if assigned( dataFile ) then
        dataFile.FlushBuffer ;
      if assigned( indexFile ) then begin
        indexFile.FlushBuffer ;
        indexFile.ReadHeader( _cursor ) ;
      end ;

      cursorNext( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSHP.cursorNext(
    const _cursor : Integer
  ) ;
  var
    {$IFDEF MANAGED}
      shx : TGIS_RecordSHX ;
    {$ELSE}
      shx : PGIS_RecordSHX ;
    {$ENDIF}
    uid     : TGIS_Uid ;
    shx_uid : TGIS_Uid ; // workaround for ignoring empty shapes
  begin
    lockThread ;
    try
      shx_uid := 0 ;

      // read each shape
         while True do begin
           if HourglassShake then begin
             cursorShp[_cursor].curShape := nil ;
             break ;
           end ;

           if assigned( cursorShp[_cursor].curShape ) then begin
             uid := cursorShp[_cursor].curShape.Uid + 1 ;
           end
           else
             uid := 1 ;

           if shx_uid > 0 then
             uid := shx_uid + 1 ;

           cursorShp[_cursor].curShape := nil ;

           if assigned( shapeFile ) then begin
             if assigned( indexFile ) then begin
               if uid <= indexFile.RecordsCount then begin
                 shx := indexFile.ReadRecord( uid, _cursor ) ;
                 {$IFDEF MANAGED}
                   shapeFile.Position := shx.Offset ;
                 {$ELSE}
                   shapeFile.Position := shx^.Offset ;
                 {$ENDIF}
                 shx_uid := uid ;
                 readShape( uid, _cursor ) ;
               end
               else
                 shx_uid := -1 ; // explicite empty index
             end
             else if shapeFile.Position < shapeFile.Size then begin
               readShape( uid, _cursor ) ;
             end
             else
               shapeFile.Position := shapeFile.Position + 1 ;
           end ;

           if cursorShp[_cursor].curShape = nil then begin
             // skip unsupported shape types
             if assigned( shapeFile ) and
                ( cursorShp[_cursor].curShapeType = TGIS_ShapeType.Unknown ) and
                ( ( shapeFile.Position < shapeFile.Size  ) and
                  ( shx_uid >= 0 )
                )
             then begin
               continue ;
             end
             else
               shx_uid := 0 ;

             // using an xxxInternalVersion may be a bit too secure
             // but better is to be too restrictive than too lose
             if not inherited cursorEofInternal( _cursor ) then begin
               cursorShp[_cursor].curShape
                 := inherited cursorShapeInternal( _cursor ) ;
               inherited cursorNextInternal( _cursor ) ;
             end ;
             if cursorShp[_cursor].curShape = nil then exit ;
           end ;

           if cursorState[_cursor].curSkipDeleted and
              cursorShp[_cursor].curShape.IsDeleted then
           begin
             continue ;
           end ;

           if not isInScope( cursorShp[_cursor].curShape, _cursor ) then
             continue
           else
             exit ;
         end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSHP.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorShp[_cursor].curShape = nil ;
  end ;

  function TGIS_LayerSHP.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorShp[_cursor].curShape ) then
       Result := cursorShp[_cursor].curShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerSHP.GetShape(
    const _uid    : TGIS_Uid  ;
    const _cursor : Integer
  ) : TGIS_Shape ;
  var
    {$IFDEF MANAGED}
      shx : TGIS_RecordSHX ;
    {$ELSE}
      shx : PGIS_RecordSHX ;
    {$ENDIF}
  begin
    lockThread ;
    try
      Result := nil ;

      if _uid <= 0 then exit ;

      // if it is in edited list
      Result := inherited GetShape( _uid, _cursor ) ;
      if Result <> nil then exit ;
      if not assigned( shapeFile ) then exit ;

      // is it a current shape
      if ( cursorShp[_cursor].curShape     <> nil ) and
         ( cursorShp[_cursor].curShape.Uid = _uid ) then
      begin
        Result := cursorShp[_cursor].curShape ;
        exit ;
      end ;

      // if index file
      if assigned( indexFile ) then begin
        if _uid <= indexFile.RecordsCount then begin
          shx := indexFile.ReadRecord( _uid, _cursor ) ;
          {$IFDEF MANAGED}
            shapeFile.Position := shx.Offset ;
          {$ELSE}
           shapeFile.Position := shx^.Offset ;
          {$ENDIF}
          readShape( _uid, _cursor ) ;
          Result := cursorShp[_cursor].curShape ;
        end ;
        exit ;
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
        inherited cursorStateRestore(_cursor) ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSHP.GetLastUid : TGIS_Uid ;
  var
    shp       : TGIS_Shape ;
    old_scope : String     ;
  begin
    lockThread ;
    try
      if assigned( indexFile ) then begin
        lastUid := Max( inherited GetLastUid, indexFile.RecordsCount ) ;
        Result  := lastUid ;
      end
      else begin
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
        end ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerSHP.GetNewUid : TGIS_Uid ;
  begin
    lockThread ;
    try
      GetLastUid ;
      inc( lastUid ) ;
      Result := lastUid ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerSHP.SaveData  ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSHP.InitializeDirectWrite(
    const _path        : String           ;
    const _extent      : TGIS_Extent      ;
    const _type        : TGIS_ShapeType   ;
    const _dim         : TGIS_DimensionType
  ) ;
  begin
    inherited InitializeDirectWrite( _path, _extent, _type, _dim ) ;

    if not ( CheckFileWriteAccessEx( PathSHP   , True , False, False ) and
             CheckFileWriteAccessEx( PathDBF   , True , False, False ) and
             CheckFileWriteAccessEx( PathSHX   , True , False, False )
           )
    then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    PrepareExportFieldNames( allowedNameLength ) ;

    shapeFile := TGIS_FileSHP.Create( PathSHP,
                                      TGIS_StreamMode.&Create, FOnRead, FOnWrite
                                     ) ;
    shapeFile.Extent    := GisExtent3DFrom2D( _extent ) ;
    shapeFile.ShapeType := shapeFile.ShapeTypeToInt( _type, _dim ) ;
    shapeFile.WriteHeader ;

    indexFile := TGIS_FileSHX.Create( PathSHX,
                                      TGIS_StreamMode.&Create, FOnRead, FOnWrite
                                      ) ;
    indexFile.Extent    := GisExtent3DFrom2D( _extent ) ;
    indexFile.ShapeType := shapeFile.ShapeTypeToInt( _type, _dim ) ;
    indexFile.WriteHeader ;

    dataFile := TGIS_FileDBF.Create( PathDBF, Self,
                                     TGIS_StreamMode.&Create, FOnRead, FOnWrite
                                   ) ;
    dataFile.WriteHeader ;
    dataFile.WriteStructure ;
    dataFile.WriteHeader ;

    CS.SaveAsWKTFile( PathPRJ ) ;

    write_CPG( GetPathNoExt(_path) + getFileExtCPG, CodePage );

    lastUid := 1 ;
  end ;

  procedure TGIS_LayerSHP.WriteShapeDirect(
    const _shape : TGIS_Shape
  ) ;
  var
    rec       : TGIS_RecordSHP ;
    zsize     : Integer        ;
    msize     : Integer        ;
    rangesize : Integer        ;
  begin
    assert( assigned( shapeFile ) ) ;
    assert( assigned( indexFile ) ) ;
    assert( assigned( dataFile ) ) ;
    assert( assigned( _shape ) ) ;

    {$IFDEF GIS_NORECORDS}
      rec := new TGIS_RecordSHP ;
    {$ENDIF}
    rec.RecordNumber := lastUid ;
    shapeFile.Extent := GisMaxExtent3D( shapeFile.Extent, _shape.Extent3D ) ;

    zsize := 0 ;
    msize := 0 ;
    if _shape.ShapeType = TGIS_ShapeType.Point then
      rangesize := 0
    else
      rangesize := 16 ;

    if ( _shape.Dimension in [ TGIS_DimensionType.XYZ,
                               TGIS_DimensionType.XYZM ] ) or
       ( _shape.ShapeType = TGIS_ShapeType.MultiPatch  ) then
      zsize := rangesize + _shape.PointsZSize ;

    if ( _shape.Dimension in [ TGIS_DimensionType.XYZ,
                               TGIS_DimensionType.XYM,
                               TGIS_DimensionType.XYZM ] ) or
       ( _shape.ShapeType = TGIS_ShapeType.MultiPatch  ) then
      msize := rangesize + _shape.PointsMSize ;

    rec.ContentLength := ( _shape.PartsSize + _shape.PartTypesSize +
                           _shape.PointsSize + zsize + msize + 4
                         ) div 2 ;

    rec.ShapeType     := _shape.ShapeType ;
    rec.Dimension     := _shape.Dimension ;

    indexFile.WriteRecord( shapeFile.Position, rec ) ;
    shapeFile.WriteRecord( rec ) ;
    shapeFile.WriteGeometry( _shape ) ;
    dataFile.AddRecord( _shape ) ;
    inc( lastUid ) ;
  end ;

  procedure TGIS_LayerSHP.FinalizeDirectWrite ;
  begin
    if assigned( shapeFile ) then
      shapeFile.WriteHeader ;
    FreeObject( shapeFile ) ;

    if assigned( indexFile ) then
      indexFile.WriteHeader ;
    FreeObject( indexFile ) ;

    if assigned( dataFile ) then
      dataFile.WriteHeader ;
    FreeObject( dataFile ) ;
  end ;


//==============================================================================
// Lider.CG.GIS.GeoLayerShp
//==============================================================================

  class procedure GisLayerShp.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-SHP', 'ArcView Shape Files', TGIS_LayerSHP, '.shp',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                    [ TGIS_RegisteredOperationType.Read,
                      TGIS_RegisteredOperationType.Write,
                      TGIS_RegisteredOperationType.&Create,
                      TGIS_RegisteredOperationType.Merge
                    ],
                   GIS_HIGH_LAYER_PRIORITY, True
                  ) ;
  end
  ;

//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    GisLayerShp.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.


