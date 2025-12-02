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
  Compression and decompression utilities.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoCompression ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoCompression"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.IO.Compression,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    java.util;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

  /// <summary>
  ///   Decompress a deflate stream.
  /// </summary>
  /// <param name="_source">
  ///   compressed stream
  /// </param>
  /// <returns>
  ///   decompressed stream
  /// </returns>
  function  DecompressDeflateStream( const _source : TStream
                                   ) : TStream ;

  /// <summary>
  ///   Decompress a deflate buffer.
  /// </summary>
  /// <param name="_source">
  ///   compressed buffer
  /// </param>
  /// <param name="_offset">
  ///   offset in buffer
  /// </param>
  /// <param name="_size">
  ///   size of buffer
  /// </param>
  /// <param name="_output">
  ///   decompressed buffer
  /// </param>
  /// <param name="_length">
  ///   length of decompressed buffer
  /// </param>
  procedure DecompressDeflateBuffer( const _source : TBytes ;
                                     const _offset : Integer ;
                                     const _size   : Integer ;
                                       var _output : TBytes  ;
                                       var _length : Integer
                                   ) ;
  {$IFDEF OXYGENE}
  {$ELSE}
    {$IFNDEF NEXTGEN}
    /// <summary>
    ///   Decompress a deflate memory.
    /// </summary>
    /// <param name="_source">
    ///   compressed buffer
    /// </param>
    /// <param name="_size">
    ///   size of buffer
    /// </param>
    /// <param name="_output">
    ///   decompressed buffer
    /// </param>
    /// <param name="_length">
    ///   length of decompressed buffer
    /// </param>
    procedure DecompressDeflateMemory( const _source : Pointer ;
                                       const _size   : Integer ;
                                         var _output : TBytes  ;
                                         var _length : Integer
                                     ) ;
    {$ENDIF}
  {$ENDIF}

  /// <summary>
  ///   Compress a stream using deflate compression.
  /// </summary>
  /// <param name="_source">
  ///   stream to compress
  /// </param>
  /// <returns>
  ///   compressed stream
  /// </returns>
  function  CompressDeflateStream  ( const _source : TStream
                                   ) : TStream ;

  /// <summary>
  ///   Decompress a zip file and extract first file to stream.
  /// </summary>
  /// <param name="_path">
  ///   file path
  /// </param>
  /// <returns>
  ///   decompressed stream
  /// </returns>
  function DecompressZipFile( const _path : String
                            ) : TStream ; overload ;

  /// <summary>
  ///   Decompress a zip file and extract a file to stream.
  /// </summary>
  /// <param name="_path">
  ///   file path
  /// </param>
  /// <param name="_name">
  ///   internal file name to extract; if empty, first file is used
  /// </param>
  /// <returns>
  ///   decompressed stream
  /// </returns>
  function DecompressZipFile( const _path : String ;
                              const _name : String
                            ) : TStream ; overload ;

  /// <summary>
  ///   Decompress a zip file and extract files to directory.
  /// </summary>
  /// <param name="_filePath">
  ///   zip file path
  /// </param>
  /// <param name="_extractTo">
  ///   directory to extract files
  /// </param>
  /// <returns>
  ///   list of extracted file names
  /// </returns>
  function ExtractZipFiles  ( const _filePath  : String ;
                              const _extractTo : String
                            ) : TArray<String> ;

  /// <summary>
  ///   Decompress a zip stream and extract first file to stream.
  /// </summary>
  /// <param name="_source">
  ///   stream to decompress
  /// </param>
  /// <returns>
  ///   decompressed stream
  /// </returns>
  function DecompressZipFile( const _source : TStream
                            ) : TStream ; overload ;

  /// <summary>
  ///   Get internal files names from a zip file.
  /// </summary>
  /// <param name="_path">
  ///   file path
  /// </param>
  /// <returns>
  ///   array of names
  /// </returns>
  function GetZipFilesNames ( const _path : String
                            ) : TArray<String> ;

  /// <summary>
  ///   Compress a stream using GZip compression.
  /// </summary>
  /// <param name="_source">
  ///   stream to decompress
  /// </param>
  /// <returns>
  ///   decompressed stream
  /// </returns>
  function  DecompressGZipStream  ( const _source : TStream
                                   ) : TStream ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Zip,
    Lider.CG.GIS.GeoRtl,
    ZLib ;
{$ENDIF}

  {$IFDEF JAVA}
    function DecompressDeflateStream(
      const _source : TStream
    ) : TStream ;
    begin

    end;
  {$ELSE}
    function DecompressDeflateStream(
      const _source : TStream
    ) : TStream ;
    var
      dstream : TZDecompressionStream ;
      numread : Integer ;
      buffer  : TBytes ;
    begin
      if not assigned( _source ) then begin
        Result := nil ;
        exit ;
      end ;
      try
        {$IFDEF OXYGENE}
          Result := TGIS_MemoryStream.Create ;
        {$ELSE}
          Result  := TMemoryStream.Create ;
        {$ENDIF}
        _source.Position := 0 ;
        dstream := TZDecompressionStream.Create( _source ) ;
        try
          SetLength( buffer, 8192 ) ;
          repeat
            {$IFDEF OXYGENE}
             numread := dstream.Read( buffer, 0, length( buffer ) ) ;
            {$ELSE}
             numread := dstream.Read( buffer[0], length( buffer ) ) ;
            {$ENDIF}
            if numread > 0 then
            {$IFDEF OXYGENE}
              Result.Write( buffer, 0, numread ) ;
            {$ELSE}
              Result.Write( buffer[0], numread ) ;
            {$ENDIF}
          until ( numread = 0 ) ;
        finally
          FreeObject( dstream ) ;
        end ;
      except
        // avoid data errors
        FreeObject( Result ) ;
      end;
    end ;
  {$ENDIF}

  procedure DecompressDeflateBuffer(
    const _source : TBytes ;
    const _offset : Integer ;
    const _size   : Integer ;
      var _output : TBytes  ;
      var _length : Integer
  ) ;
  var
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
        instream  : TStream ;
        outstream : TStream ;
      {$ENDIF}
      {$IFDEF JAVA}
        decompresser : java.util.zip.Inflater ;
      {$ENDIF}
      {$IFDEF ISLAND}
        instream  : TStream ;
        outstream : TStream ;
      {$ENDIF}
    {$ELSE}
      ptr     : Pointer ;
      destLen : Integer ;
    {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
        instream := TGIS_MemoryStream.Create ;
        try
          instream.Write( _source, _offset, _size ) ;
          outstream := DecompressDeflateStream( instream ) ;
          if length( _output ) < outstream.Size then
            SetLength( _output, outstream.Size ) ;
          outstream.Position := 0 ;
          outstream.Read( _output, outstream.Size ) ;
          _length := outstream.Size ;
        finally
          FreeObject( outstream ) ;
          FreeObject( instream  ) ;
        end ;
      {$ENDIF}
      {$IFDEF JAVA}
        decompresser := new java.util.zip.Inflater();
        decompresser.setInput(_source, _offset, _size );

        var bos : java.io.ByteArrayOutputStream  := new java.io.ByteArrayOutputStream(_size);

        var buf := new Byte[1024] ;
        var cnt : Integer := 0 ;
        _length := 0 ;
        while ( not decompresser.finished()) do begin
          cnt := decompresser.inflate( buf ) ;
          _length := _length + cnt ;
          bos.write( buf, 0, cnt ) ;
        end ;
        bos.close() ;

        _output := bos.toByteArray();
        decompresser.end();
      {$ENDIF}
      {$IFDEF ISLAND}
        {$WARNING '### Verify ISLAND code'}
      {$ENDIF}
    {$ELSE}
      try
        {$IFDEF NEXTGEN}
          ZDecompress( copy( _source, _offset, _size), _output ) ;
          _length := length(_output) ;
        {$ELSE}
          ZDecompress( @_source[_offset], _size, ptr, destLen ) ;
          if length( _output ) < destLen then
            SetLength( _output, destLen ) ;
          Move( ptr^, _output[0], destLen ) ;
          _length := destLen ;
        {$ENDIF}
      finally
        FreeMem( ptr ) ;
      end ;
   {$ENDIF}
  end ;

  {$IFDEF OXYGENE}
  {$ELSE}
    {$IFNDEF NEXTGEN}
    procedure DecompressDeflateMemory( const _source : Pointer ;
                                       const _size   : Integer ;
                                         var _output : TBytes  ;
                                         var _length : Integer
                                     ) ;
    var
      ptr     : Pointer ;
      destLen : Integer ;
    begin
      ZDecompress( _source, _size, ptr, destLen ) ;
      try
        if length( _output ) < destLen then
          SetLength( _output, destLen ) ;
        Move( ptr^, _output[0], destLen ) ;
        _length := destLen ;
      finally
        FreeMem( ptr ) ;
      end ;
    end ;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF JAVA}
    function  CompressDeflateStream (
      const _source : TStream
    ) : TStream ;
    begin

    end ;
  {$ELSE}
    function  CompressDeflateStream (
      const _source : TStream
    ) : TStream ;
    var
      cstream : TZCompressionStream ;
      buffer  : array of Byte ;
      numread : Integer ;
    begin
      {$IFDEF OXYGENE}
        Result := TGIS_MemoryStream.Create ;
      {$ELSE}
        Result := TMemoryStream.Create ;
      {$ENDIF}
      cstream := TZCompressionStream.Create( Result ) ;
      try
        SetLength( buffer, 8192 ) ;
        repeat
          {$IFDEF OXYGENE}
            numread := _source.Read( buffer, 0, length( buffer ) ) ;
          {$ELSE}
            numread := _source.Read( buffer[0], length( buffer ) ) ;
          {$ENDIF}
          if numread > 0 then
            {$IFDEF OXYGENE}
              cstream.Write( buffer, 0, numread ) ;
            {$ELSE}
              cstream.Write( buffer[0], numread ) ;
            {$ENDIF}
        until ( numread = 0 ) ;
      finally
        FreeObject( cstream ) ;
      end ;
    end ;
  {$ENDIF}

  function DecompressZipFile(
    const _path : String
  ) : TStream ;
  begin
    Result := DecompressZipFile( _path, '' ) ;
  end ;

  function DecompressZipFile(
    const _path : String ;
    const _name : String
  ) : TStream ;
  var
    {$IFDEF DCC}
      zip  : TZipFile ;
      hdr  : TZipHeader ;
      zstm : TStream ;
    {$ENDIF}
    {$IFDEF CLR}
      zip  : ZipArchive ;
      zstm : TStream ;
    {$ENDIF}
    {$IFDEF ISLAND}
      zstm : TStream ;
    {$ENDIF}
    {$IFDEF JAVA}
      zip : java.util.zip.ZipInputStream ;
      ze  : java.util.zip.ZipEntry ;
    {$ENDIF}
     i    : Integer ;
  begin
    Result := TMemoryStream.Create ;

    {$IFDEF DCC}
      zip := TZipFile.Create ;
      try
        zip.Open( _path, TZipMode.zmRead ) ;
        zstm := nil ;
        if IsStringEmpty( _name ) then begin
          zip.Read( zip.FileName[0], zstm, hdr ) ;
          Result.CopyFrom( zstm, zstm.Size ) ;
        end
        else begin
          for i := 0 to zip.FileCount-1 do begin
            if CompareText( zip.FileName[i], _name ) = 0 then begin
              zip.Read( zip.FileName[i], zstm, hdr ) ;
              Result.CopyFrom( zstm, zstm.Size ) ;
              break ;
            end ;
          end ;
          if not assigned( zstm ) then begin
            zip.Read( zip.FileName[0], zstm, hdr ) ;
            Result.CopyFrom( zstm, zstm.Size ) ;
          end;
        end ;
      finally
        FreeObject( zstm ) ;
        FreeObject( zip  ) ;
      end ;
    {$ENDIF}
    {$IFDEF CLR}
      zip := ZipFile.OpenRead( _path ) ;
      try
        if assigned( zip ) then begin
          if zip.Entries.Count > 0 then begin
            if IsStringEmpty( _name ) then begin
              zstm := zip.Entries[0].Open ;
              zstm.CopyTo( Result ) ;
            end
            else begin
              zstm := nil ;
              for i := 0 to zip.Entries.Count-1 do begin
                if CompareText( zip.Entries[i].FullName, _name ) = 0 then begin
                  zstm := zip.Entries[i].Open ;
                  zstm.CopyTo( Result ) ;
                  break ;
                end ;
              end ;
              if not assigned( zstm ) then begin
                zstm := zip.Entries[0].Open ;
                zstm.CopyTo( Result ) ;
              end;
            end ;
          end ;
        end ;
      finally
        FreeObject( zip  ) ;
      end;
    {$ENDIF}
    {$IFDEF JAVA}
      i := 1 ;
      zip := new java.util.zip.ZipInputStream(new java.io.FileInputStream(_path));
      try
        if assigned( zip ) then begin
          if IsStringEmpty( _name ) then begin
            ze := zip.NextEntry ;
            if assigned( ze ) then begin
              var len: Integer;
              var buffer: array of SByte := new SByte[1024];

              len := zip.read(buffer);
              while len > 0 do begin
                Result.write(buffer, 0, len);
                len := zip.read(buffer);
              end;
            end ;
          end
          else begin
            i := 0 ;
            repeat
              ze := zip.NextEntry ;
              if (ze <> nil) and (CompareText( ze.Name, _name ) = 0) then begin
                var len: Integer;
                var buffer: array of SByte := new SByte[1024];

                len := zip.read(buffer);
                while len > 0 do begin
                  Result.write(buffer, 0, len);
                  len := zip.read(buffer);
                end;
                inc( i ) ;
                break ;
              end;
            until (ze = nil) ;

          end ;
        end ;
      finally
        FreeObject( zip  ) ;
      end ;
      if (i = 0) then
        Result := DecompressZipFile( _path, '' ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      {$WARNING '### Verify ISLAND code'}
    {$ENDIF}
  end;

  function DecompressZipFile(
    const _source : TStream
  ) : TStream ;
  var
    {$IFDEF DCC}
      zip  : TZipFile ;
      hdr  : TZipHeader ;
      zstm : TStream ;
    {$ENDIF}
    {$IFDEF CLR}
      zip  : ZipArchive ;
      zstm : TStream ;
    {$ENDIF}
    {$IFDEF JAVA}
      zip : java.util.zip.ZipInputStream ;
      ze  : java.util.zip.ZipEntry ;
    {$ENDIF}
    {$IFDEF ISLAND}
      zstm : TStream ;
    {$ENDIF}
  begin
    Result := TMemoryStream.Create ;

    {$IFDEF DCC}
      zip := TZipFile.Create ;
      try
        zip.Open( _source, TZipMode.zmRead ) ;
        zstm := nil ;
        if zip.FileCount > 0 then begin
          zip.Read( zip.FileName[0], zstm, hdr ) ;
          Result.CopyFrom( zstm, zstm.Size ) ;
        end ;
      finally
        FreeObject( zstm ) ;
        FreeObject( zip  ) ;
      end ;
    {$ENDIF}
    {$IFDEF CLR}
      zip := new ZipArchive( _source ) ;
      try
        if assigned( zip ) then begin
          if zip.Entries.Count > 0 then begin
            zstm := zip.Entries[0].Open ;
            zstm.CopyTo( Result ) ;
          end ;
        end ;
      finally
        FreeObject( zip  ) ;
      end;
    {$ENDIF}
    {$IFDEF JAVA}
      var buff := new array of Byte( _source.Size ) ;
      _source.Position := 0 ;
      _source.Read( buff ) ;

      zip := new java.util.zip.ZipInputStream(new java.io.ByteArrayInputStream( buff, 0, buff.length ) );
      try
        if assigned( zip ) then begin
          ze := zip.NextEntry ;
          if assigned( ze ) then begin
            var len: Integer;
            var buffer: array of SByte := new SByte[1024];

            len := zip.read(buffer);
            while len > 0 do begin
              Result.write(buffer, 0, len);
              len := zip.read(buffer);
            end;
          end ;
        end ;
      finally
        FreeObject( zip  ) ;
      end;
    {$ENDIF}
  end ;

  function GetZipFilesNames(
    const _path : String
  ) : TArray<String> ;
  var
    {$IFDEF DCC}
      zip  : TZipFile ;
    {$ENDIF}
    {$IFDEF CLR}
      zip  : ZipArchive ;
    {$ENDIF}
    {$IFDEF JAVA}
      zip : java.util.zip.ZipInputStream ;
      ze  : java.util.zip.ZipEntry ;
    {$ENDIF}
    {$IFDEF ISLAND}
      zstm : TStream ;
    {$ENDIF}
  begin
    {$IFDEF DCC}
      zip := TZipFile.Create ;
      try
        zip.Open( _path, TZipMode.zmRead ) ;
        Result := zip.FileNames ;
      finally
        FreeObject( zip  ) ;
      end ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := nil ;
      zip := ZipFile.OpenRead( _path ) ;
      try
        if assigned( zip ) then begin
          SetLength( Result, zip.Entries.Count ) ;
          var i := 0 ;
          for e in zip.Entries do begin
            Result[i] := e.FullName ;
            inc( i ) ;
          end ;
        end ;
      finally
        FreeObject( zip  ) ;
      end;
    {$ENDIF}
    {$IFDEF JAVA}
      Result := nil ;
      zip := new java.util.zip.ZipInputStream(new java.io.FileInputStream(_path));
      try
        if assigned( zip ) then begin
          var lst := new ArrayList<String>();
          repeat
            ze := zip.NextEntry ;
            if (ze <> nil) then
              lst.Add( ze.Name ) ;
          until (ze = nil) ;
          SetLength( Result, lst.size ) ;
          lst.toArray( Result ) ;
        end ;
      finally
        FreeObject( zip  ) ;
      end;
    {$ENDIF}
    {$IFDEF ISLAND}
      {$WARNING '### Verify ISLAND code'}
    {$ENDIF}
  end ;

  function DecompressGZipStream(
    const _source : TStream
  ) : TStream ;
    {$IFDEF DCC}
    var
      strmgzip : TStream ;
    {$ENDIF}
  begin
    Result := TMemoryStream.Create ;
    if not assigned( _source ) then exit ;
    _source.Position := 0 ;
    {$IFDEF DCC}
      strmgzip := TDecompressionStream.Create(_source, 15 + 16);  // gzip only mode
      try
        TMemoryStream(Result).LoadFromStream( strmgzip ) ;
      finally
        FreeObject( strmgzip ) ;
      end ;
    {$ENDIF}
    {$IFDEF CLR}
      using strmgzip := new GZipStream(_source, CompressionMode.Decompress, True) do begin
        var ar := new Byte[4096];
        var rd : Integer ;
        repeat
          rd := strmgzip.Read( ar, 0, 4096);
          Result.Write( ar, 0, rd ) ;
        until (rd = 0);
      end;
    {$ENDIF}
    {$IFDEF JAVA}
      var strm := new TGIS_BaseStream( _source ) ;
      var gzip : java.util.zip.GZIPInputStream := nil ;
      try
        var bytes := new array of Byte( strm.Size );
        var buf := new array of Byte( strm.Size );
        strm.ReadBuffer( bytes, strm.Size ) ;
        var i := 0 ;
        gzip := new java.util.zip.GZIPInputStream( new java.io.ByteArrayInputStream( bytes ) ) ;
        repeat
          i := gzip.read( buf ) ;
          if i > 0 then
            Result.write(buf, 0, i) ;
        until i <= 0;
      finally
        gzip.close ;
        FreeObject( strm ) ;
      end;
    {$ENDIF}
    {$IFDEF ISLAND}
      {$WARNING '### Verify ISLAND code'}
    {$ENDIF}
  end ;

  function ExtractZipFiles(
    const _filePath  : String ;
    const _extractTo : String
  ) : TArray<String> ;
  var
    {$IFDEF DCC}
      zip  : TZipFile ;
    {$ENDIF}
    {$IFDEF CLR}
      zip  : ZipArchive ;
    {$ENDIF}
    {$IFDEF JAVA}
      zip   : java.util.zip.ZipInputStream ;
      ze    : java.util.zip.ZipEntry ;
      fout  : java.io.FileOutputStream ;
    {$ENDIF}
    {$IFDEF ISLAND}
      zstm : TStream ;
    {$ENDIF}
  begin
    {$IFDEF DCC}
    zip := TZipFile.Create ;
    try
      if zip.IsValid( _filePath ) then begin
        zip.Open( _filePath, TZipMode.zmRead ) ;
        zip.ExtractAll( _extractTo ) ;
        Result := zip.FileNames ;
      end
      else
        Result := nil ;
    finally
      FreeObject( zip ) ;
    end ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := nil ;
      zip := ZipFile.OpenRead( _filePath ) ;
      try
        if assigned( zip ) then begin
          SetLength( Result, zip.Entries.Count ) ;
          var i := 0 ;
          for e in zip.Entries do begin
            Result[i] := e.FullName ;
            inc( i ) ;
          end ;
          zip.ExtractToDirectory( _extractTo ) ;
        end ;
      finally
        FreeObject( zip  ) ;
      end;
    {$ENDIF}
    {$IFDEF JAVA}
      Result := nil ;
      zip := new java.util.zip.ZipInputStream(new java.io.FileInputStream(_filePath));
      try
        if assigned( zip ) then begin
          var lst := new ArrayList<String>();
          repeat
            ze := zip.NextEntry ;
            if (ze <> nil) then begin
              lst.Add( ze.Name ) ;

              var newFile := new java.io.File(_extractTo, ze.Name);
              var parent  := newFile.ParentFile ;
              parent.mkdirs();
              fout := new java.io.FileOutputStream( newFile );

              var len: Integer;
              var buffer: array of SByte := new SByte[1024];

              len := zip.read(buffer);
              while len > 0 do begin
                fout.write(buffer, 0, len);
                len := zip.read(buffer);
              end ;
              zip.closeEntry();
              fout.close();
            end ;
          until (ze = nil) ;
          SetLength( Result, lst.size ) ;
          lst.toArray( Result ) ;
        end ;
      finally
        FreeObject( zip  ) ;
      end;
    {$ENDIF}
    {$IFDEF ISLAND}
      {$WARNING '### Verify ISLAND code'}
    {$ENDIF}
  end ;

{==================================== END =====================================}
end.
