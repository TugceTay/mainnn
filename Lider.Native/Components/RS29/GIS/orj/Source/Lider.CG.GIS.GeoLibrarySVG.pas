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
  Symbol Library.
}

{$IFDEF DCC}
  unit GisLibrarySVG ;
  {$HPPEMIT '#pragma link "GisLibrarySVG"'}
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
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}


{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Resources,
    System.IO,
    System.IO.Compression,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Types,
    System.IOUtils,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisBaseObject,
    GisResource,
    GisRtl,
    GisStreams,
    GisClasses,
    GisTypesUI,
    GisSymbol;
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

  /// <summary>
  ///   Class to access SVG Symbol Library.
  /// </summary>
  TGIS_SymbolLibrarySVG = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    private
      FCategories : TStringList ;
      lstManifest : TDictionary< String, TObject > ;
      lstLoop     : TStringList ;
      oBitmap     : TGIS_Bitmap ;
      oStream     : TStream ;
      sCategory   : String ;

      lstLibFiles : TObjectList<TStream > ;

    public

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      /// <remarks>
      ///   <note type="caution">
      ///     Don not call this directly. Instead a class function
      ///     TGIS_SymbolLibrarySVG.Handle should be used
      ///     to obtain a global handle to the object.
      ///   </note>
      /// </remarks>
      constructor Create ;

    protected
      procedure doDestroy(); override;

    private
      class function fget_Handle : TGIS_SymbolLibrarySVG ; static;

    private
      /// <summary>
      ///   Read resource into the stream.
      /// </summary>
      /// <param name="_name">
      ///   Name of library resource
      /// </param>
      /// <returns>
      ///   Stream. Ownership managed by class.
      /// </returns>
      function getStream( const _name : String ) : TStream ;

      function getSubstream( const _strm : TStream; const _id : Cardinal ) : TStream ;

      /// <summary>
      ///   Register library stream into global cache.
      /// </summary>
      /// <param name="_strm">
      ///   library stream to be registered
      /// </param>
      procedure registerLibrary( const _strm : TStream ) ;

    public
      /// <summary>
      ///   Register file library.
      /// </summary>
      /// <param name="_path">
      ///   path to file library
      /// </param>
      procedure RegisterFileLibrary( const _path : String ) ;

      /// <summary>
      ///   Register resource library library.
      /// </summary>
      /// <param name="_name">
      ///   library name
      /// </param>
      procedure RegisterResourceLibrary( const _name : String ) ; overload;

      {$IFDEF CLR}
        /// <summary>
        ///   Register resource library library.
        /// </summary>
        /// <param name="_rm">
        ///   resource manager opened on embedded resource file
        ///   which contain the library
        /// </param>
        /// <param name="_name">
        ///   the library name within resource
        /// </param>
        /// <remarks>
        ///  This overload allows registering of resource libraries from
        ///  other assemblies than DK Library.
        /// </remarks>
        procedure RegisterResourceLibrary( const _rm   : ResourceManager;
                                           const _name : String
                                         ) ; overload;
      {$ENDIF}

      /// <summary>
      ///   Open library for browsing purposes - fills Categories.
      /// </summary>
      procedure OpenForBrowsing         ; overload ;

      /// <summary>
      ///   Open library for browsing purposes - fills Categories.
      /// </summary>
      /// <param name="_category">
      ///   if not empty then only symbols matching category will be available
      /// </param>
      procedure OpenForBrowsing         ( const _category : String
                                        ) ; overload ;

      /// <summary>
      ///   Prepare a preview image for an item.
      /// </summary>
      /// <param name="_name">
      ///   name of the library item
      /// </param>
      /// <returns>
      ///   Bitmap with symbol previewed.
      /// </returns>
      /// <remarks>
      ///   <list type="bullet">
      ///     <item>
      ///       Returned image is owned by class. Ownership managed by class.
      ///     </item>
      ///     <item>
      ///       Default PPI for platfom is used (96 for Windows).
      ///     </item>
      ///     <item>
      ///       Default image size is 64 pixels.
      ///     </item>
      ///   </list>
      /// </remarks>
      function GetPreview                ( const _name     : String
                                         ) : TGIS_Bitmap ; overload ;

      /// <summary>
      ///   Prepare a preview image for an item.
      /// </summary>
      /// <param name="_name">
      ///   name of the library item
      /// </param>
      /// <param name="_ppi">
      ///   pixels per inch; used to calculate proper line sizes etc.
      /// </param>
      /// <param name="_size">
      ///   image size.
      /// </param>
      /// <returns>
      ///   Bitmap with symbol previewed.
      /// </returns>
      /// <remarks>
      ///   Returned image is owned by class. Ownership managed by class.
      /// </remarks>
      function GetPreview                ( const _name     : String ;
                                           const _ppi      : Integer ;
                                           const _size     : Integer
                                         ) : TGIS_Bitmap ; overload ;

      /// <summary>
      ///   Prepare a preview image for an item.
      /// </summary>
      /// <param name="_name">
      ///   name of the library item
      /// </param>
      /// <param name="_ppi">
      ///   pixels per inch; used to calculate proper line sizes etc.
      /// </param>
      /// <param name="_size">
      ///   image size.
      /// </param>
      /// <param name="_areacolor">
      ///   color for interiors
      /// </param>
      /// <param name="_linecolor">
      ///   color for lines and outlines
      /// </param>
      /// <returns>
      ///   Bitmap with symbol previewed.
      /// </returns>
      /// <remarks>
      ///   Returned image is owned by class. Ownership managed by class.
      /// </remarks>
      function GetPreview                ( const _name     : String ;
                                           const _ppi      : Integer ;
                                           const _size     : Integer ;
                                           const _areacolor: TGIS_Color ;
                                           const _linecolor: TGIS_Color
                                         ) : TGIS_Bitmap ; overload ;

      /// <summary>
      ///   Get caption (user friendly name) of resource.
      /// </summary>
      /// <param name="_name">
      ///   _name of library item
      /// </param>
      /// <returns>
      ///   Symbol caption.
      /// </returns>
      function GetCaption( const _name : String ) : String ;

      /// <summary>
      ///   Get categories of resource.
      /// </summary>
      /// <param name="_name">
      ///   name of the library item
      /// </param>
      /// <returns>
      ///   Categories separated by semicolons.
      /// </returns>
      function GetCategories( const _name : String ) : String ;

      /// <summary>
      ///   Get categories of resource.
      /// </summary>
      /// <param name="_name">
      ///   name of the library item
      /// </param>
      /// <param name="_categories">
      ///   categories separated by semicolons
      /// </param>
      /// <remarks>
      ///   If empty categories provided then symbol will be hidden from
      ///   category browsing.
      /// </remarks>
      procedure SetCategories( const _name       : String;
                               const _categories : String
                             ) ;

      /// <summary>
      ///   Prepare list of all items within selected categories.
      /// </summary>
      /// <param name="_filter">
      ///   single category; if '*' then all symbols will be provided
      /// </param>
      /// <returns>
      ///   List with all symbols IDs matching filter.
      /// </returns>
      /// <remarks>
      ///   Returned list is owned by class. Do not free it.
      /// </remarks>
      function &Loop( const _filter : String ) : TStringList ;


      /// <summary>
      ///   List of categories in the library.
      /// </summary>
      property Categories : TStringList read FCategories ;


      /// <summary>
      ///   Prepare symbol as a ready SVG object.
      /// </summary>
      /// <param name="_name">
      ///   Symbol name
      /// </param>
      /// <returns>
      ///   Symbol. Remember to release.
      /// </returns>
      class function Prepare( const _name : String ) : TGIS_SymbolSVG ;

      /// <summary>
      ///   Global library object.
      /// </summary>
      /// <remarks>
      ///   Use this property instead of creation a private instances of this class.
      /// </remarks>
      class property Handle : TGIS_SymbolLibrarySVG read fget_Handle ;
  end;


implementation

{$IFDEF DCC}
  {$R SymbolLibrarySVG.RES}

  uses
    GisUtils,
    GisCompression;
{$ENDIF}

type
  T_Entry = class
    sValue  : String  ;
    sStream : TStream ;
  end;

var
  oLibrarySVG : TGIS_SymbolLibrarySVG ;


{$IFDEF CLR}
  type
    // As special version of binary reader which do not close the master stream
    T_BinaryReaderNoClosing = class (BinaryReader)
       protected
       procedure Dispose(disposing: Boolean); override;
     end ;

    procedure T_BinaryReaderNoClosing.Dispose(disposing: Boolean) ;
    begin
    end ;
{$ENDIF}



constructor TGIS_SymbolLibrarySVG.Create;
{$IFDEF DCC}
  var
    str : string ;
{$ENDIF}
{$IFDEF JAVA}
  var
    dir   : java.io.File    ;
    files : array of String ;
{$ENDIF}
begin
  lstLibFiles := TObjectList<TStream >.Create( True ) ;
  lstManifest := TDictionary< String, TObject >.Create(
                   {$IFDEF JAVA}
                     java.lang.String.CASE_INSENSITIVE_ORDER
                   {$ENDIF}
                   {$IFDEF CLR}
                     StringComparer.OrdinalIgnoreCase
                   {$ENDIF}
                   {$IFDEF DCC}
                     TIStringComparer.Ordinal
                   {$ENDIF}
                 ) ;

  FCategories := TStringList.Create ;
  FCategories.Sorted := True ;
  {$IFDEF DCC} // other alre always case insenistive
    FCategories.CaseSensitive := False ;
  {$ENDIF}

  lstLoop := TStringList.Create ;
  lstLoop.Sorted := True  ;
  oBitmap  := TGIS_Bitmap.Create( 65, 64 ) ;
  oStream  := nil ;

  RegisterResourceLibrary( 'std' );
  RegisterResourceLibrary( 's52' );
  RegisterResourceLibrary( 'shield' );

  {$IFDEF DCC}
    for str in TDirectory.GetFiles( TGIS_Utils.ExecutingFolder, '*' + GIS_SYMBOLLIBSVG_EXT ) do
      RegisterFileLibrary( str );
  {$ENDIF}
  {$IFDEF CLR}
    for str in Directory.GetFiles( TGIS_Utils.ExecutingFolder, '*' + GIS_SYMBOLLIBSVG_EXT ) do
      RegisterFileLibrary( str );
  {$ENDIF}
  {$IFDEF JAVA}
    dir := java.io.File.create(TGIS_Utils.ExecutingFolder) ;
    files := dir.list(
               new interface java.io.FilenameFilter(
                 accept := method(directory : java.io.File ; name : String)
                 begin
                   result := name.EndsWith( GIS_SYMBOLLIBSVG_EXT ) ;
                 end
               )
             ) ;

    for str in files do
      RegisterFileLibrary( str ) ;
  {$ENDIF}
end;

procedure TGIS_SymbolLibrarySVG.doDestroy;
{$IFDEF DCC}
  var
    pair : TPair<String,TObject> ;
{$ENDIF}
begin
  FreeObject( lstLibFiles );

  {$IFDEF DCC}
    for pair in lstManifest do
      FreeObjectNotNil( pair.Value );
  {$ENDIF}

  FreeObject( lstManifest );
  FreeObject( FCategories );
  FreeObject( lstLoop     );
  FreeObject( oBitmap     );
  FreeObject( oStream     );
  inherited;
end;

function TGIS_SymbolLibrarySVG.getSubstream(
  const _strm : TStream;
  const _id : Cardinal
) : TStream ;
{$IFDEF DCC}
  var
    strmres  : TStream ;
    idx : UInt32 ;
    siz : UInt32 ;
    buf : array of Byte ;
  begin
    FreeObject( oStream  ) ;

    strmres := TMemoryStream.Create ;
    try
      if _id = 0 then begin
        _strm.Position := 20 ;
        _strm.Read( idx, 4 ) ;
        _strm.Position := idx ;
      end
      else
        _strm.Position := _id ;

      _strm.Read( siz, 4 ) ;
      strmres.CopyFrom( _strm, siz ) ;

      oStream := DecompressGZipStream( strmres ) ;
      oStream.Position := 0;
    finally
      FreeObject( strmres ) ;
    end;

    Result := oStream ;
  end ;
{$ENDIF}

{$IFDEF CLR}
  const
    BUF_SIZE = 4096 ;
  var
    idx : UInt32 ;
    siz : UInt32 ;
  begin
    FreeObject( oStream  ) ;

    using rdr := new T_BinaryReaderNoClosing( _strm ) do begin
      using strmres := new MemoryStream() do begin
        if _id = 0 then begin
          _strm.Position := 20 ;
          idx := rdr.ReadInt32() ;
          _strm.Position := idx ;
        end
        else
          _strm.Position := _id ;

        siz := rdr.ReadInt32() ;
        strmres.CopyFrom( _strm, siz ) ;
        strmres.Position := 0 ;

        oStream := DecompressGZipStream( strmres ) ;
        oStream.Position := 0;
(*
        using strmgzip := new GZipStream(strmres, CompressionMode.Decompress) do begin
          oStream := new MemoryStream() ;

          var ar := new Byte[BUF_SIZE];
          var rd : Integer ;

          repeat
            rd := strmgzip.Read( ar, 0, BUF_SIZE);
            oStream.Write( ar, 0, rd ) ;
          until rd <= 0;

          oStream.Position := 0;
        end;
*)
      end;
      Result := oStream ;
    end ;
  end ;
{$ENDIF}

{$IFDEF JAVA}
  var
    siz     : Integer                         ;
    bytes   : array of Byte                   ;
    buf     : array of Byte                   ;
    i, idx  : Integer                         ;
    strm    : TGIS_BaseStream                 ;
    gzip    : java.util.zip.GZIPInputStream   ;
  begin
    FreeObject( oStream  ) ;

    strm := new TGIS_BaseStream( _strm ) ;

    buf := new array of Byte(strm.Size);
    try
      if _id = 0 then begin
        strm.Position := 20 ;
        strm.ReadInteger( idx ) ;
        strm.Position := idx ;
      end
      else
        strm.Position := _id ;

      strm.ReadInteger( siz ) ;

      bytes := new array of Byte( siz );
      strm.ReadBuffer( bytes, siz ) ;

      gzip := new java.util.zip.GZIPInputStream( new java.io.ByteArrayInputStream( bytes ) ) ;
      oStream := new TMemoryStream() ;

      repeat
        i := gzip.read( buf ) ;
        if i > 0 then
          oStream.write(buf, 0, i) ;
      until i <= 0;

    finally
      gzip.close ;
      FreeObject( strm ) ;
    end;

    Result := oStream ;
  end ;
{$ENDIF}
{$IFDEF ISLAND}
  begin
    {$WARNING '### Verify ISLAND code'}
  end ;
{$ENDIF}

function TGIS_SymbolLibrarySVG.getStream(
  const _name : String
) : TStream ;
var
  s : String ;
  idx : Integer ;
  o : TObject ;
  itm : T_Entry ;
begin
  if not lstManifest.TryGetValue( _name, o ) then begin
    Result := nil ;
    exit ;
  end;

  itm := T_Entry( o ) ;
  s := itm.sValue ;

  assert( s <> '' ) ;

  if s[StringFirst] = '@' then begin
    Result := getStream( Copy(s,StringFirst+1,255) ) ;
    exit ;
  end;


{$IFDEF JAVA OR ISLAND}
  idx := StrToInt( s.Split('|')[0] ) ;
{$ELSE}
  idx := StrToInt( s.Split(['|'])[0] ) ;
{$ENDIF}
  Result := getSubstream( itm.sStream, idx )  ;
end;


procedure TGIS_SymbolLibrarySVG.registerLibrary(
  const _strm : TStream
) ;
var
  lst     : TStringList ;
  i       : Integer     ;
  itm     : T_Entry     ;
begin
  lstLibFiles.Add( _strm );

  lst := TStringList.Create ;
  try
    lst.LoadFromStream( getSubstream( _strm, 0 ), TEncoding.UTF8 ) ;

    for i:=0 to lst.Count - 1 do begin
      if lstManifest.ContainsKey(lst.Names[i]) then
        continue ;
      itm := T_Entry.Create ;
      itm.sValue := lst.ValueFromIndex[i] ;
      itm.sStream := _strm ;

      lstManifest.Add( lst.Names[i], itm ) ;
    end ;
  finally
    FreeObject( lst ) ;
  end;
end;

function TGIS_SymbolLibrarySVG.GetCaption(
  const _name: String
) : String;
var
  tkn  : TGIS_Tokenizer ;
  name : String ;

  o  : TObject ;
begin
  Result := '' ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _name, [':'] );

    assert( ( tkn.Result.Count >= 0 ) and ( tkn.Result.Count <= 3  ) ) ;

    if tkn.Result.Count > 1 then
      name := tkn.Result[1] ;
    if tkn.Result.Count > 2 then
      name := name + ':' + tkn.Result[2] ;

    if lstManifest.TryGetValue( name, o ) then begin
      tkn.Execute( T_Entry( o ).sValue, ['|'] );

      assert( tkn.Result.Count >= 2 ) ;
      if tkn.Result.Count > 1  then begin
        Result := tkn.Result[1] ;
      end;
    end ;
  finally
    FreeObject( tkn );
  end;
end;

function TGIS_SymbolLibrarySVG.GetCategories(
  const _name: String
) : String;
var
  tkn  : TGIS_Tokenizer ;
  name : String ;

  o  : TObject ;
begin
  Result := '' ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _name, [':'] );

    assert( ( tkn.Result.Count >= 0 ) and ( tkn.Result.Count <= 3  ) ) ;

    if tkn.Result.Count > 1 then
      name := tkn.Result[1] ;
    if tkn.Result.Count > 2 then
      name := name + ':' + tkn.Result[2] ;

    if lstManifest.TryGetValue( name, o ) then begin
      tkn.Execute( T_Entry( o ).sValue, ['|'] );

      assert( tkn.Result.Count >= 2 ) ;
      if tkn.Result.Count > 1  then begin
        Result := tkn.Result[2];
      end;
    end ;
  finally
    FreeObject( tkn );
  end;
end;

procedure TGIS_SymbolLibrarySVG.SetCategories(
  const _name       : String ;
  const _categories : String
) ;
var
  tkn  : TGIS_Tokenizer ;
  name : String ;

  o  : TObject ;

  tmp : String ;
begin
  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _name, [':'] );

    assert( ( tkn.Result.Count >= 0 ) and ( tkn.Result.Count <= 3  ) ) ;

    if tkn.Result.Count > 1 then
      name := tkn.Result[1] ;
    if tkn.Result.Count > 2 then
      name := name + ':' + tkn.Result[2] ;

    if lstManifest.TryGetValue( name, o ) then begin
      tkn.Execute( T_Entry( o ).sValue, ['|'] );

      tmp := '' ;
      if tkn.Result.Count > 0 then
        tmp := tmp + tkn.Result[0];
      tmp := tmp + '|' ;
      if tkn.Result.Count > 1 then
        tmp := tmp + tkn.Result[1];
      tmp := tmp + '|' ;
      tmp := tmp + _categories ;
      T_Entry( o ).sValue := tmp ;
    end ;
  finally
    FreeObject( tkn );
  end;
end;

function TGIS_SymbolLibrarySVG.GetPreview(
  const _name: String
) : TGIS_Bitmap;
begin
  oBitmap.DrawSymbol( _name );

  Result := oBitmap ;
end;

function TGIS_SymbolLibrarySVG.GetPreview(
  const _name : String  ;
  const _ppi  : Integer ;
  const _size : Integer
) : TGIS_Bitmap;
begin
  if ( _size <> oBitmap.Width )
     or
     ( _size <> oBitmap.Height )
  then begin
    FreeObject( oBitmap ) ;
    oBitmap := TGIS_Bitmap.Create( _size, _size )
  end;

  oBitmap.DrawSymbol( _name, _ppi );

  Result := oBitmap ;
end ;

function TGIS_SymbolLibrarySVG.GetPreview(
  const _name     : String     ;
  const _ppi      : Integer    ;
  const _size     : Integer    ;
  const _areacolor: TGIS_Color ;
  const _linecolor: TGIS_Color
) : TGIS_Bitmap;
begin
  if ( _size <> oBitmap.Width )
     or
     ( _size <> oBitmap.Height )
  then begin
    FreeObject( oBitmap ) ;
    oBitmap := TGIS_Bitmap.Create( _size, _size )
  end;

  oBitmap.DrawSymbol( _name, _ppi, _areacolor, _linecolor );

  Result := oBitmap ;
end;

function TGIS_SymbolLibrarySVG.&Loop(
  const _filter: String
): TStringList;
var
  j    : Integer ;
  tkn  : TGIS_Tokenizer ;
  cat  : String  ;
  tmp  : String  ;
  tmp2 : String  ;
  bfind: Boolean ;
  {$IFDEF DCC}
    pair : TPair<String,TObject> ;
  {$ENDIF}
begin
  lstLoop.Clear ;

  tkn := TGIS_Tokenizer.Create ;
  try
    for pair in lstManifest do begin
      tmp := T_Entry( pair.Value ).sValue ;
      if IsStringEmpty( tmp ) then
        continue ;
      if tmp[StringFirst] = '@' then
        continue ;

      tmp := T_Entry( pair.Value ).sValue ;
      tkn.Execute( tmp, ['|'] );
      if tkn.Result.Count = 1 then
        continue ;
      if tkn.Result.Count <> 3 then begin
        assert( False ) ;
      end;
      tmp2 := tkn.Result[2] ;

      tkn.Execute( tmp2, [';'] );

      if sCategory <> '' then begin
        bfind := False ;
        for j := 0 to tkn.Result.Count - 1do begin
          cat := Trim( tkn.Result[j] ) ;

          if CompareText( cat, sCategory ) = 0 then begin
            bfind := True ;
            break ;
          end;
        end ;

        if not bfind then
          continue ;
      end;

      if ( _filter <> '*' ) then begin
        bfind := False ;
        for j := 0 to tkn.Result.Count - 1 do begin
          cat := Trim( tkn.Result[j] ) ;

          if CompareText( cat, _filter ) = 0 then begin
            bfind := True ;
            break ;
          end;
        end;

        if not bfind then
          continue ;
      end ;

      lstLoop.Add( 'LIBSVG:' + pair.Key ) ;
    end;
  finally
    FreeObject( tkn ) ;
  end;

  Result := lstLoop ;
end;

procedure TGIS_SymbolLibrarySVG.RegisterFileLibrary(
  const _path : String
) ;
var
  strm : TGIS_FileStream ;
begin
  strm := TGIS_FileStream.Create( _path,
                                  fmOpenRead or
                                  fmShareDenyWrite
                                ) ;

  if not assigned( strm ) then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                 _path,
                                 0
                               ) ;

  registerLibrary( strm ) ;
end;

procedure TGIS_SymbolLibrarySVG.RegisterResourceLibrary(
  const _name : String
) ;
{$IFDEF CLR}
  var
    strm : TStream;
  begin
    using rm := new ResourceManager(
                  'TatukGIS.NDK.Resources.SymbolLibrarySVG',
                  typeOf(TGIS_SymbolLibrarySVG).Assembly
                )
    do begin
      strm := rm.GetStream(_name) ;
    end ;

    if not assigned( strm ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                   _name,
                                   0
                                  ) ;

    registerLibrary( strm ) ;
  end ;
{$ENDIF}
{$IFDEF DCC}
  var
    strm : TStream ;
  begin
    strm := TResourceStream.Create(hInstance, 'LIBSVG_' + _name, RT_RCDATA) ;

    if not assigned( strm ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                   _name,
                                   0
                                  ) ;

    registerLibrary( strm ) ;
  end ;
{$ENDIF}
{$IFDEF JAVA}
  var
    strm            : TStream             ;
    inStream        : java.io.InputStream ;
    buf             : array of Byte       ;
  method copyToStream ;
  begin
    var len: Integer := 0;
    loop begin
      len := inStream.read(buf);
      if len < 0 then begin
        break;
      end;
      strm.write(buf, 0, len);
    end;
  end ;

  begin
    try
      inStream := self.Class.getResourceAsStream("/resources/" + _name + GIS_SYMBOLLIBSVG_EXT) ;
      strm := new TMemoryStream();

      buf := new array of Byte( 1024 ) ;

      copyToStream ;
    finally
      inStream.close ;
    end;

    if not assigned( strm ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                    _name,
                                    0
                                  ) ;

    registerLibrary( strm ) ;
  end ;
{$ENDIF}

{$IFDEF CLR}
  procedure TGIS_SymbolLibrarySVG.RegisterResourceLibrary(
    const _rm   : ResourceManager;
    const _name : String
  ) ;
  var
    strm : TStream;
  begin
    strm := _rm.GetStream(_name) ;

    if not assigned( strm ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                   _name,
                                   0
                                  ) ;
  registerLibrary( strm ) ;
  end ;
{$ENDIF}
{$IFDEF ISLAND}
  begin
    {$WARNING '### Verify ISLAND code'}
   end ;
{$ENDIF}


procedure TGIS_SymbolLibrarySVG.OpenForBrowsing;
begin
  OpenForBrowsing('') ;
end;

procedure TGIS_SymbolLibrarySVG.OpenForBrowsing(
  const _category : String
) ;
var
  j    : Integer ;
  tkn  : TGIS_Tokenizer ;
  tmp2 : String  ;
  cat  : String  ;
  idx  : Integer ;
  tmp  : String  ;
  bfind: Boolean ;
  {$IFDEF DCC}
    pair : TPair<String,TObject> ;
  {$ENDIF}
begin
  sCategory := _category ;
  FCategories.Clear ;
  tkn := TGIS_Tokenizer.Create ;
  try
    for pair in lstManifest do begin
      tmp := T_Entry( pair.Value ).sValue ;
      tkn.Execute( tmp, ['|'] );
      if tkn.Result.Count = 1 then
        continue ;
      if tkn.Result.Count <> 3 then begin
        assert( False ) ;
      end;
      tmp2 := tkn.Result[2] ;

      tkn.Execute( tmp2, [';'] );

      bfind := False ;
      for j := 0 to tkn.Result.Count - 1do begin
        cat := Trim( tkn.Result[j] ) ;

        if CompareText( cat, _category ) = 0 then begin
          bfind := True ;
          break ;
        end ;
      end;

      if not IsStringEmpty( _category ) then
        if not bfind then
          continue ;

      for j := 0 to tkn.Result.Count - 1do begin
        cat := Trim( tkn.Result[j] ) ;

        if not FCategories.Find( cat, idx ) then begin
          if Trim( cat ) <> '' then
            FCategories.Add( cat ) ;
        end;
      end;
    end;
  finally
    FreeObject( tkn ) ;
  end;
end;


class function TGIS_SymbolLibrarySVG.Prepare(
  const _name : String
) : TGIS_SymbolSVG ;
var
  tkn   : TGIS_Tokenizer ;
  fname : String ;
  strm  : TStream ;
begin
  Result := nil ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _name, [':'] );

    if tkn.Result.Count >= 2 then begin
      if CompareText( tkn.Result[0], 'LIBSVG' ) <> 0 then begin
        assert( False ) ;
      end;

      fname := tkn.Result[1] ;

      if tkn.Result.Count >= 3 then
        fname := tkn.Result[1] +':' + tkn.Result[2] ;
    end;
  finally
    FreeObject( tkn ) ;
  end;


  strm := TGIS_SymbolLibrarySVG.Handle.getStream( fname ) ;
  if assigned( strm ) then
    Result := TGIS_SymbolSVG.Create( _name, strm );
end;

class function TGIS_SymbolLibrarySVG.fget_Handle
  : TGIS_SymbolLibrarySVG ;
begin
  if not assigned ( oLibrarySVG ) then
    oLibrarySVG := TGIS_SymbolLibrarySVG.Create ;
  Result := oLibrarySVG ;
end;

{$IFDEF DCC}
  initialization
  finalization
     FreeObject( oLibrarySVG );
{$ENDIF}

{==================================== END =====================================}
end.
