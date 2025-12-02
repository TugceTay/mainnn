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
  This unit will provide support for storing a pixel layer into a SQL database.

  Any future image converter must support LoadFromStream & SaveToStream
  (and both operations must be sequential or see PNG fetchRecord support
  support for an explanation on how to solve this).
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFilePixelStore ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFilePixelStore"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Drawing,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Classes,
    System.SysUtils,
    System.Math,
    System.Types,
    System.Variants,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoDb,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoFilePixel,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoLayer ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type
  /// <summary>
  ///   PixelStore supported formats.
  /// </summary>
  TGIS_PixelStoreType = {$IFDEF OXYGENE} public {$ENDIF}
  (
    /// <summary>
    ///   Bitmap
    /// </summary>
    BMP,
    /// <summary>
    ///   Jpeg 24 bit
    /// </summary>
    JPEG24,
    /// <summary>
    ///   Jpeg 8 bit
    /// </summary>
    JPEG8,
    /// <summary>
    ///   Png 24 bit
    /// </summary>
    PNG24,
    /// <summary>
    ///   Png 8 bit
    /// </summary>
    PNG8,
    /// <summary>
    ///   Png 32 bit
    /// </summary>
    PNG32,
    /// <summary>
    ///   Grid array
    /// </summary>
    GRID
  ) ;

  /// <summary>
  ///   Class to manage grid data
  /// </summary>
  TGIS_GridClass = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FGrid : TGIS_GridArray ;
      FWidth : Integer ;
      FHeight : Integer ;
    public
      /// <summary>
      ///   Constructor
      /// </summary>
      /// <param name="_width">
      ///   array width
      /// </param>
      /// <param name="_height">
      ///   array height
      /// </param>
      constructor Create( const _width, _height : Integer ) ;

      /// <summary>
      ///   Resize grid.
      /// </summary>
      /// <param name="_width">
      ///   array width
      /// </param>
      /// <param name="_height">
      ///   array height
      /// </param>
      procedure Resize( const _width, _height : Integer ) ;
      /// <summary>
      ///   Clear data
      /// </summary>
      procedure Clear ;
      /// <summary>
      ///   Assign grid data.
      /// </summary>
      /// <param name="_grd">
      ///   grid data
      /// </param>
      procedure Assign( const _grd : TGIS_GridArray ) ; overload;
      /// <summary>
      ///   Assign grid data.
      /// </summary>
      /// <param name="_grd">
      ///   grid data
      /// </param>
      procedure Assign( const _grd : TGIS_GridClass ) ; overload;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   Read from stream
        /// </summary>
        /// <param name="_stream">
        ///   stream handle
        /// </param>
        procedure LoadFromStream( const _stream : TGIS_BaseStream ) ;
        /// <summary>
        ///   Save to stream
        /// </summary>
        /// <param name="_stream">
        ///   stream handle
        /// </param>
        procedure SaveToStream( const _stream : TGIS_BaseStream ) ;
      {$ELSE}
        /// <summary>
        ///   Read from stream
        /// </summary>
        /// <param name="_stream">
        ///   stream handle
        /// </param>
        procedure LoadFromStream( const _stream : TStream ) ;
        /// <summary>
        ///   Save to stream
        /// </summary>
        /// <param name="_stream">
        ///   stream handle
        /// </param>
        procedure SaveToStream( const _stream : TStream ) ;
      {$ENDIF}
      /// <summary>
      ///   Save to file
      /// </summary>
      /// <param name="_name">
      ///   file name
      /// </param>
      procedure SaveToFile( const _name: String);
      /// <summary>
      ///   Copy a region of grid to another grid
      /// </summary>
      /// <param name="_dest">
      ///   destination rectangle
      /// </param>
      /// <param name="_grid">
      ///   grid data to copy
      /// </param>
      /// <param name="_source">
      ///   source rectangle
      /// </param>
      procedure CopyRect( const _dest : TRect;
                          const _grid : TGIS_GridArray;
                          const _source : TRect
                         );

      /// <summary>
      ///   Grid data
      /// </summary>
      property Grid : TGIS_GridArray read FGrid ;
      /// <summary>
      ///   Grid width
      /// </summary>
      property Width : Integer read FWidth ;
      /// <summary>
      ///   Grid height
      /// </summary>
      property Height : Integer read FHeight ;
  end ;

  /// <summary>
  ///   Encapsulation of PixelStore storage within SQL database - single cell
  ///   storage.
  /// </summary>
  TGIS_PixelStoreCell = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FBitmap     : TGIS_Bitmap           ;
      {$IFDEF OXYGENE}
        FModified : Boolean ;
      {$ENDIF}
      FGrid       : TGIS_GridClass    ;
      FExtent     : TGIS_Extent       ;
      FLevel      : Integer           ;
      FStream     : TGIS_MemoryStream ;
      FProgress   : Double            ;

    protected
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create  ;

    public
      /// <summary>
      ///   Bitmap data.
      /// </summary>
      property Bitmap     : TGIS_Bitmap       read  FBitmap   ;

      /// <summary>
      ///   Grid data.
      /// </summary>
      property Grid       : TGIS_GridClass    read  FGrid     ;

      /// <summary>
      ///   Cell extent.
      /// </summary>
      property Extent     : TGIS_Extent       read  FExtent   ;

      /// <summary>
      ///   Cell pyramid level.
      /// </summary>
      property Level      : Integer           read  FLevel    ;

      /// <summary>
      ///   Stream with data.
      /// </summary>
      property Stream     : TGIS_MemoryStream read  FStream   ;
      /// <summary>
      ///   Current progress.
      /// </summary>
      property Progress   : Double            read  FProgress ;
      {$IFDEF OXYGENE}
        /// <summary>
        ///   If bitmap modified.
        /// </summary>
        property Bitmap_Modified : Boolean read FModified write FModified ;
      {$ENDIF}
  end ;

  /// <summary>
  ///   Encapsulation of PixelStore storage within SQL database.
  /// </summary>
  TGIS_FilePixelStoreAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                                 class ( TGIS_FilePixel )

    protected  // properties internal values
      /// <summary>
      ///   Parent layer.
      /// </summary>
      FGIS_Layer          : TGIS_Layer ;

      /// <summary>
      ///   Path to file with layer data.
      /// </summary>
      FPath : String ;

      /// <summary>
      ///   Name of the pixel store - used only if opened by
      ///   TGIS_LayerPixelStore.
      /// </summary>
      FName : String ;

      /// <summary>
      ///   Extent of the layer.
      /// </summary>
      FExtent : TGIS_Extent ;

      /// <summary>
      ///   Size in of the single pixel in layer units.
      /// </summary>
      FPixelSize : Double ;

      /// <summary>
      ///   Type of the image used as a storage.
      /// </summary>
      FImageType : TGIS_PixelStoreType ;

      /// <summary>
      ///   Is Grid?.
      /// </summary>
      FIsGrid : Boolean ;

      /// <summary>
      ///   Level of the hierarchy.
      /// </summary>
      FLevel : Integer ;

      /// <summary>
      ///   Maximum level of the hierarchy.
      /// </summary>
      FMaxLevel : Integer ;

      /// <summary>
      ///   Name of the table.
      /// </summary>
      FTable : String ;

      /// <summary>
      ///   Parsed catalog name.
      /// </summary>
      strCatalog : String ;

      /// <summary>
      ///   Parsed schema name.
      /// </summary>
      strSchema  : String ;

      /// <summary>
      ///   Parsed table name.
      /// </summary>
      strName    : String ;

      /// <summary>
      ///   Background color of the image.
      /// </summary>
      FColor : TGIS_Color ;

      /// <summary>
      ///   WKT projection String.
      /// </summary>
      FWKT   : String ;

      /// <summary>
      ///   Connection parameters.
      /// </summary>
      FSQLParameters : TStringList ;

      /// <summary>
      ///   SQL dialect (list of tokens) attached to the layer.
      /// </summary>
      FSQLDialectList : TStringList ;

      /// <summary>
      ///   List of sql commands.
      /// </summary>
      FSQLCommands : TStringList ;

      /// <summary>
      ///   Relative path to a project file.
      /// </summary>
      FRelativePath : String ;

    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      {$IFDEF CLR}

        /// <summary>
        ///   Event to be fired after dialect change.
        /// </summary>
        FOnBeforeDialectChange : EventHandler ;
      {$ELSE}

        /// <summary>
        ///   Event to be fired before dialect change.
        /// </summary>
        FOnBeforeDialectChange : TNotifyEvent ;
      {$ENDIF}
      {$IFDEF CLR}

        /// <summary>
        ///   Event to be fired after dialect change.
        /// </summary>
        FOnAfterDialectChange : EventHandler ;
      {$ELSE}

        /// <summary>
        ///   Event to be fired after dialect change.
        /// </summary>
        FOnAfterDialectChange : TNotifyEvent ;
      {$ENDIF}

      /// <summary>
      ///   Will be fired upon connecting to a database to resolve
      ///   &lt;#user#&gt;, &lt;#password#&gt; or any other &lt;#token#&gt;
      ///   in a SQLParameters property.
      /// </summary>
      FOnPassword : TGIS_TemplateProducerEvent ;

    protected // properties access routine

      function  fget_SQLParameter     ( const _name : String
                                      ) : String ; virtual;
      procedure fset_SQLParameter     ( const _name : String;
                                        const _value : String
                                      ) ; virtual;
      function  fget_TableMaster      : String ;
      function  fget_CellSize         : Integer ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      function  fget_Table            : String ;

    protected // other protected variables

      /// <summary>
      ///   Is table read only?
      /// </summary>
      isReadOnly   : Boolean ;

      /// <summary>
      ///   Database supporting class handle.
      /// </summary>
      oGisDb       : TGIS_DbAbstract ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various variables
      lruList        : TObject ;

      objBmp         : TGIS_Bitmap        ;
      objGrid        : TGIS_GridClass ;
      objCell        : TGIS_PixelStoreCell ;

      scopeRect      : TRect   ;
      scopePos       : TPoint  ;
      scopeFactor    : Integer ;
      scopeEof       : Boolean ;

      tmpPasswords   : TGIS_PasswordList ;

      directWrite    : Boolean ;

    protected // various routines

      /// <summary>
      ///   Called by Create and CreateForWrite methods.
      /// </summary>
      procedure doCreate             ;

      /// <summary>
      ///   Prerecognize PixelStore format.
      /// </summary>
      /// <param name="_path">
      ///   path; can contain also TGIS_FilePixelStoreAbstract.SQLParameter
      ///   separated by '|' character;
      /// </param>
      /// <returns>
      ///   Format type
      /// </returns>
      function  doPrerecognize       ( const _path   : String
                                     ) : Integer ; virtual;

      /// <summary>
      ///   Return a SQL select command.
      /// </summary>
      /// <param name="_table">
      ///   table to be selected
      /// </param>
      /// <param name="_filter">
      ///   filter expression
      /// </param>
      /// <returns>
      ///   command
      /// </returns>
      function  prepareSelectCommand ( const _table  : String ;
                                       const _filter : String
                                     ) : String ; virtual;

      /// <summary>
      ///   Return filter for selected cell
      /// </summary>
      /// <param name="_level">
      ///   uid value used to build filter
      /// </param>
      /// <param name="_col">
      ///   uid value used to build filter
      /// </param>
      /// <param name="_row">
      ///   uid value used to build filter
      /// </param>
      /// <returns>
      ///   filter
      /// </returns>
      function  prepareFilterCell    ( const _level  : Integer ;
                                       const _col    : Integer ;
                                       const _row    : Integer
                                     ) : String ; virtual;

      /// <summary>
      ///   Return a SQL parameterized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///   command
      /// </returns>
      function  prepareAppendCmd     ( const _table : String
                                     ) : String ;

      /// <summary>
      ///   Return a SQL parameterized update command.
      /// </summary>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_filter">
      ///   filter for table
      /// </param>
      /// <returns>
      ///   command
      /// </returns>
      function  prepareUpdateCmd     ( const _table  : String ;
                                       const _filter : String
                                     ) : String ;

      /// <summary>
      ///   Return a SQL parameterized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///   command
      /// </returns>
      function  prepareAppendCommand ( const _table  : String
                                     ) : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      /// <returns>
      ///   params
      /// </returns>
      function  prepareAppendParams   ( const _table : String
                                      ) : String ;

      /// <summary>
      ///   Return a SQL parametrized append command.
      /// </summary>
      /// <param name="_table">
      ///   table to be inserted
      /// </param>
      procedure macroAppendParams    ( const _table : String
                                      )  ;

      /// <summary>
      ///   Return a SQL parameterized update command.
      /// </summary>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_filter">
      ///   filter for table
      /// </param>
      /// <returns>
      ///   command
      /// </returns>
      function  prepareUpdateCommand ( const _table  : String ;
                                       const _filter : String
                                     ) : String ;

      /// <summary>
      ///   Update single parameter on SQLDialectList.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter; if not exist, then will be created
      /// </param>
      /// <param name="_value">
      ///   value of the parameter; if empty, then parameter will be deleted
      /// </param>
      procedure updateDialectList    ( const _name  : String ;
                                       const _value : String
                                     ) ;

      /// <summary>
      ///   Load a picture from the PixelStore.
      /// </summary>
      procedure readCell             ;

      /// <summary>
      ///   Calculate the scope that covers a given extent.
      /// </summary>
      /// <param name="_level">
      ///   level of hierarchy to be used
      /// </param>
      /// <param name="_extent">
      ///   extent to be calculated
      /// </param>
      /// <returns>
      ///   scope rectangle
      /// </returns>
      function  calculateScope       ( const _level  : Integer ;
                                       const _extent : TGIS_Extent
                                     ) : TRect ;

      /// <summary>
      ///   Calculate the tile to updaate.
      /// </summary>
      /// <param name="_level">
      ///   level of hierarchy to be used
      /// </param>
      /// <param name="_extent">
      ///   extent to be calculated
      /// </param>
      /// <returns>
      ///   tile scope to be updated
      /// </returns>
      function  calculateInsertTile  ( const _level  : Integer ;
                                       const _extent : TGIS_Extent
                                     ) : TRect ;
      /// <summary>
      ///   Update PixelStore factor
      /// </summary>
      procedure updateFactor         ;

      /// <summary>
      ///   Select quad for a given cell.
      /// </summary>
      /// <param name="_pos">
      ///   scope position
      /// </param>
      /// <param name="_zoom">
      ///   zoom at which quad scope must be calculated
      /// </param>
      procedure setScopeQuad         ( const _pos    : TPoint  ;
                                       const _zoom   : Double
                                     ) ;

      /// <summary>
      ///   Process tokens in a SQLParameters property.
      /// </summary>
      /// <param name="_token">
      ///   to be replaced
      /// </param>
      /// <returns>
      ///   token result value
      /// </returns>
      function  passwordCallBack     ( const _token  : String
                                     ) : String ;

      /// <summary>
      ///   Copy list from resources into the internal list of commands.
      /// </summary>
      procedure prepareCommandList   ; virtual;

      /// <summary>
      ///   Return a SQL command associated with a given identifier.
      /// </summary>
      /// <param name="_id">
      ///   command identifier
      /// </param>
      /// <returns>
      ///   command text
      /// </returns>
      function  getCmd               ( const _id     : Integer
                                     ) : String ;

      /// <inheritdoc/>
      procedure prepareCapabilities  ; override;

      /// <summary>
      ///   Create and open a new PixelStore file.
      /// </summary>
      /// <param name="_path">
      ///   path to TTKPS file; if points to to non-existent file then will be
      ///   treated as a list of CRLF or '\n' delimited parameters
      /// </param>
      /// <param name="_ext">
      ///   extent of the file to be written
      /// </param>
      /// <param name="_width">
      ///   width in pixels of a created bitmap
      /// </param>
      /// <param name="_height">
      ///   height in pixels of a created bitmap
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPIXELFORMAT
      /// </exception>
      procedure doCreateForWrite     ( const _path        : String      ;
                                       const _ext         : TGIS_Extent ;
                                       const _width       : Integer     ;
                                       const _height      : Integer
                                     ) ; overload;

      /// <summary>
      ///   Create and open a new PixelStore file.
      /// </summary>
      /// <param name="_path">
      ///   path to TTKPS file; if points to to non-existent file then will be
      ///   treated as a list of CRLF or '\n' delimited parameters
      /// </param>
      /// <param name="_ext">
      ///   extent of the file to be written
      /// </param>
      /// <param name="_width">
      ///   width in pixels of a created bitmap
      /// </param>
      /// <param name="_height">
      ///   height in pixels of a created bitmap
      /// </param>
      /// <param name="_subformat">
      ///   subformat number; use property Capabilities to obtain a list of
      ///   supported formats; use 0 to get default
      /// </param>
      /// <param name="_ppi">
      ///   density of output image; unused
      /// </param>
      /// <param name="_cs">
      ///   projection
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPIXELFORMAT
      /// </exception>
      procedure doCreateForWrite     ( const _path        : String      ;
                                       const _ext         : TGIS_Extent ;
                                       const _width       : Integer     ;
                                       const _height      : Integer     ;
                                       const _subformat   : TGIS_LayerPixelSubFormat     ;
                                       const _ppi         : Integer     ;
                                       const _cs          : TGIS_CSCoordinateSystem
                                     ) ; overload;
    protected // abstract methods

      /// <summary>
      ///   Read cell data.
      /// </summary>
      /// <param name="_name">
      ///   table name
      /// </param>
      /// <returns>
      ///   cell data in stream
      /// </returns>
      function  sqlQueryGetCell       ( const _name : String
                                      ) : TStream ; virtual; abstract;
      /// <summary>
      ///   Update data in table.
      /// </summary>
      /// <param name="_table">
      ///   table name
      /// </param>
      /// <param name="_filter">
      ///   table filter
      /// </param>
      procedure sqlTableOpenWrite     ( const _table  : String ;
                                        const _filter : String
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Append data to table.
      /// </summary>
      /// <param name="_table">
      ///   table name
      /// </param>
      procedure sqlTableAppend        ( const _table : String
                                      ) ; virtual; abstract;
      /// <summary>
      ///   Write cell data.
      /// </summary>
      /// <param name="_name">
      ///   table name
      /// </param>
      /// <param name="_blob">
      ///   data stream
      /// </param>
      procedure sqlTableSetCell       ( const _name   : String        ;
                                        const _blob   : TGIS_MemoryStream
                                      ) ; virtual; abstract;
    protected // macro methods

      /// <summary>
      ///   Macro for connecting to the database.
      /// </summary>
      procedure macroConnect          ; virtual;

      /// <summary>
      ///   Macro for disconnecting to the database.
      /// </summary>
      procedure macroDisconnect       ; virtual;

      /// <summary>
      ///   Macro for creating master table
      /// </summary>
      procedure macroMasterCreate     ; virtual;

      /// <summary>
      ///   Macro for creating table.
      /// </summary>
      /// <param name="_extent">
      ///   starting extent of layer
      /// </param>
      /// <param name="_type">
      ///   supported image type
      /// </param>
      /// <param name="_compression">
      ///   compression of the PixelStore; 0 means the minimal size (and the
      ///   lowest quality); 100 means the maximum size (and best quality).
      /// </param>
      /// <param name="_pixelsize">
      ///   size of single pixel
      /// </param>
      /// <param name="_srtext">
      ///   spatial reference text (WKT)
      /// </param>
      procedure macroTableCreate      ( const _extent      : TGIS_Extent ;
                                        const _type        : TGIS_PixelStoreType ;
                                        const _compression : Integer     ;
                                        const _pixelsize   : Double      ;
                                        const _srtext      : String
                                      ) ; virtual;

      /// <summary>
      ///   Macro for dropping table.
      /// </summary>
      procedure macroTableDrop        ; virtual;

      /// <summary>
      ///   Parse config.
      /// </summary>
      procedure parseConfigLayerName  ;

      /// <summary>
      ///   Get schema prefix.
      /// </summary>
      /// <returns>
      ///   prefix text
      /// </returns>
      function  getSchemaPrefix       : String ;
    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy             ; override;
    public

      /// <inheritdoc/>
      constructor Create              ; overload; virtual;

      /// <inheritdoc/>
      /// <param name="_ppi">
      ///   density of output image; unused
      /// </param>
      constructor Create              ( const _path        : String      ;
                                        const _ext         : TGIS_Extent ;
                                        const _width       : Integer     ;
                                        const _height      : Integer     ;
                                        const _subformat   : TGIS_LayerPixelSubFormat ;
                                        const _ppi         : Integer     ;
                                        const _cs          : TGIS_CSCoordinateSystem
                                      ) ; overload; override;

      /// <summary>
      ///   Open database connection to PixelStore
      /// </summary>
      procedure   Open                ;

      /// <summary>
      ///   Calculate pyramid level based on provided zoom factor
      /// </summary>
      /// <param name="_zoom">
      ///   zoom level
      /// </param>
      /// <returns>
      ///   calculated zoom level
      /// </returns>
      function CalculateLevel         ( const _zoom        : Double
                                      ) : Integer ;

      /// <summary>
      ///   Set scope based on scale and extent.
      /// </summary>
      /// <param name="_extent">
      ///   extent to be selected
      /// </param>
      /// <param name="_zoom">
      ///   zoom to be selected (will helps to select proper PixelStore level);
      ///   _zoom &lt; 0 is for internal use only
      /// </param>
      procedure   SetScope            ( const _extent : TGIS_Extent ;
                                        const _zoom   : Double
                                      ) ;

      /// <summary>
      ///   Move to the first cell in the scope.
      /// </summary>
      procedure   MoveFirst           ;

      /// <summary>
      ///   Move to the next cell in the scope.
      /// </summary>
      procedure   MoveNext            ;

      /// <summary>
      ///   Is the end of the scope?
      /// </summary>
      /// <returns>
      ///   True if no more data.
      /// </returns>
      function    Eof                 : Boolean ;

      /// <summary>
      ///   Get current cell in the scope.
      /// </summary>
      /// <returns>
      ///   cell object
      /// </returns>
      function    CurrentCell         : TGIS_PixelStoreCell ;

      /// <summary>
      ///   Write current cell content to the database.
      /// </summary>
      procedure   WriteCell           ;

      /// <summary>
      ///   Add a new cell based on a bitmap.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap data
      /// </param>
      /// <param name="_extent">
      ///   extent to write
      /// </param>
      /// <param name="_scale">
      ///   current scale
      /// </param>
      procedure   AddCell             ( const _bmp    : TGIS_Bitmap ;
                                        const _extent : TGIS_Extent ;
                                        const _scale  : Double
                                      ) ;

      /// <summary>
      ///   Add a new cell based on a bitmap.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap data
      /// </param>
      /// <param name="_extent">
      ///   extent to write
      /// </param>
      /// <param name="_scale">
      ///   current scale
      /// </param>
      procedure   AddCellEx           ( const _bmp    : TGIS_Bitmap ;
                                        const _extent : TGIS_Extent ;
                                        const _scale  : Double
                                      ) ;

      /// <summary>
      ///   Add a new cell based on a bitmap.
      /// </summary>
      /// <param name="_grd">
      ///   grid data
      /// </param>
      /// <param name="_extent">
      ///   extent to write
      /// </param>
      /// <param name="_scale">
      ///   current scale
      /// </param>
      procedure   AddCellGrid         ( const _grd    : TGIS_GridArray ;
                                        const _extent : TGIS_Extent    ;
                                        const _scale  : Double
                                      ) ;

      /// <inheritdoc/>
      procedure   Write              ( const _x       : Integer ;
                                       const _y       : Integer ;
                                       const _pixels  : TGIS_Pixels ;
                                       const _pformat : TGIS_PixelFormat ;
                                       const _width   : Integer ;
                                       const _height  : Integer
                                      ) ; overload; override;

      /// <summary>
      ///   Write a single chunk of a pixel file. The chunk can be any valid
      ///   bitmap that has the same pixel format as the pixel file.
      /// </summary>
      /// <param name="_ext">
      ///   extent to write
      /// </param>
      /// <param name="_level">
      ///   level value
      /// </param>
      /// <param name="_pixels">
      ///   buffer with data
      /// </param>
      /// <param name="_width">
      ///   buffer width
      /// </param>
      /// <param name="_height">
      ///   buffer height
      /// </param>
      procedure   Write              ( const _ext       : TGIS_Extent ;
                                       const _level     : Integer ;
                                       const _pixels    : TGIS_Pixels ;
                                       const _width     : Integer ;
                                       const _height    : Integer
                                      ) ; overload;

      /// <inheritdoc/>
      procedure   WriteGrid           ( const _x      : Integer ;
                                        const _y      : Integer ;
                                        const _grd    : TGIS_GridArray
                                      ) ; overload; override;

      /// <inheritdoc/>
      procedure   WriteGrid           ( const _ext     : TGIS_Extent ;
                                        const _x       : Integer ;
                                        const _y       : Integer ;
                                        const _level   : Integer ;
                                        const _grd     : TGIS_GridArray
                                      ) ; overload;

      /// <summary>
      ///   Check if format is supported.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      /// <returns>
      ///   True if file is recognized
      /// </returns>
      function    Prerecognize        ( const _path   : String
                                      ) : Boolean ; virtual; abstract;

      /// <summary>
      ///   Get available layers with their types from database.
      /// </summary>
      /// <remarks>
      ///    List Objects property keeps layer type as TGIS_ShapeType value
      /// </remarks>
      /// <returns>
      ///   registered layers names and types list
      /// </returns>
      function  GetAvailableLayers    : TGIS_LayerInfoList ;

      /// <inheritdoc/>
      procedure   InitializeWrite      ; override;

      /// <inheritdoc/>
      procedure   FinalizeWrite      ; override;

      /// <summary>
      ///   Update subformat type.
      /// </summary>
      /// <param name="_sformat">
      ///   subformat
      /// </param>
      procedure UpdateSubFormat( const _sformat : TGIS_LayerPixelSubFormat ) ;
    public    // properties

      /// <summary>
      ///   Connection parameters.
      /// </summary>
      /// <param name="_name">
      ///   parameter name
      /// </param>
      property SQLParameter[ const _name : String ] : String
                                                    read  fget_SQLParameter
                                                    write fset_SQLParameter ;
      {$IFDEF OXYGENE}
        /// <summary>
        ///   Dialect list in a form "token=replacement". Will be changed
        ///   after each change of SQLDialect property.
        /// </summary>
        property SQLDialectList : TGIS_StringList
                                  read  FSQLDialectList ;
      {$ELSE}

        /// <summary>
        ///   Dialect list in a form "token=replacement". Will be changed
        ///   after each change of SQLDialect property.
        /// </summary>
        property SQLDialectList : TStringList
                                  read  FSQLDialectList ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        /// <summary>
        ///   SQL Commands used for database operations.
        /// </summary>
        property SQLCommands : TGIS_StringList
                                 read  FSQLCommands ;
      {$ELSE}

        /// <summary>
        ///   SQL Commands used for database operations.
        /// </summary>
        property SQLCommands : TStringList
                                 read  FSQLCommands ;
      {$ENDIF}

      /// <summary>
      ///   Parent layer.
      /// </summary>
      property GIS_Layer : TGIS_Layer
                             read  FGIS_Layer
                             write FGIS_Layer ;

      /// <summary>
      ///   Path to file with layer data; can contain also
      ///   TGIS_FilePixelStoreAbstract.SQLParameter separated by CRLF or
      ///   '\n'.
      /// </summary>
      property Path : String
                      read FPath
                      write FPath ;
                      {$IFDEF OXYGENE} reintroduce ; {$ENDIF}

      /// <summary>
      ///   Name of the pixel store - used only if opened by
      ///   TGIS_LayerPixelStore.
      /// </summary>
      property Name : String
                      read FName
                      write FName ;

      /// <summary>
      ///   Extent of the layer.
      /// </summary>
      property Extent : TGIS_Extent
                        read FExtent ;
                        {$IFDEF OXYGENE} reintroduce ; {$ENDIF}

      /// <summary>
      ///   Name of the master table.
      /// </summary>
      property TableMaster : String
                             read fget_TableMaster ;

      /// <summary>
      ///   Name of the table.
      /// </summary>
      property Table : String
                       read fget_Table ;

      /// <summary>
      ///   Size in pixels of a single cell.
      /// </summary>
      property CellSize : Integer
                          read fget_CellSize ;

      /// <summary>
      ///   Size in of the single pixel in layer units.
      /// </summary>
      property PixelSize : Double
                           read FPixelSize ;

      /// <summary>
      ///   Type of the image used as a storage.
      /// </summary>
      property ImageType : TGIS_PixelStoreType
                           read FImageType ;

      /// <summary>
      ///   Is Grid.
      /// </summary>
      property IsGrid : Boolean
                        read FIsGrid ;

      /// <summary>
      ///   Level of the hierarchy.
      /// </summary>
      property Level : Integer
                       read FLevel ;

      /// <summary>
      ///   Level of the hierarchy.
      /// </summary>
      property WKT : String
                       read  FWKT
                       write FWKT ;

      /// <summary>
      ///   Color of unfilled space of the image.
      /// </summary>
      /// <remarks>
      ///   Default color is Black
      /// </remarks>
      property Color : TGIS_Color read  FColor
                              write FColor ;

      /// <summary>
      ///   Relative path to a project file.
      /// </summary>
      property RelativePath : String read  FRelativePath
                                     write FRelativePath  ;

    published // events

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Will be fired before every SQLDialect change. By changing
        ///   SQLDialectList inside handler for this event you will be able
        ///   to modify dialect dynamically.
        /// </summary>
        event BeforeDialectChangeEvent    : EventHandler
                                           delegate FOnBeforeDialectChange ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Will be fired before every SQLDialect change. By changing
        ///   SQLDialectList inside handler for this event you will be able
        ///   to modify dialect dynamically.
        /// </summary>
        property BeforeDialectChangeEvent   : TNotifyEvent
                                         read  FOnBeforeDialectChange
                                         write FOnBeforeDialectChange ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Will be fired after SQLDialect change. By changing
        ///   SQLCommandList inside handler for this event you will be able
        ///   to modify commands dynamically.
        /// </summary>
        event AfterDialectChangeEvent     : EventHandler
                                           delegate FOnAfterDialectChange ;
      {$ELSE}
          /// <event/>
          /// <summary>
          ///   Will be fired after SQLDialect change. By changing
          ///   SQLCommandList inside handler for this event you will be able
          ///   to modify commands dynamically.
          /// </summary>
          property AfterDialectChangeEvent    : TNotifyEvent
                                           read  FOnAfterDialectChange
                                           write FOnAfterDialectChange ;
      {$ENDIF}
      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Will be fired upon connecting to a database to resolve
        ///   &lt;#user#&gt;, &lt;#password#&gt; or any other &lt;#token#&gt;
        ///   in a SQLParameters property.
        /// </summary>
        event PasswordEvent               : TGIS_TemplateProducerEvent
                                           delegate FOnPassword ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Will be fired upon connecting to a database to resolve
        ///   &lt;#user#&gt;, &lt;#password#&gt; or any other &lt;#token#&gt;
        ///   in a SQLParameters property.
        /// </summary>
        property PasswordEvent              : TGIS_TemplateProducerEvent
                                           read  FOnPassword
                                           write FOnPassword ;
      {$ENDIF}

  end ;

  TGIS_FilePixelStoreAbstractClass = class of TGIS_FilePixelStoreAbstract ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisPixelStoreTypeBMP    = TGIS_PixelStoreType.BMP    ;
      gisPixelStoreTypeJPEG24 = TGIS_PixelStoreType.JPEG24 ;
      gisPixelStoreTypeJPEG8  = TGIS_PixelStoreType.JPEG8  ;
      gisPixelStoreTypePNG24  = TGIS_PixelStoreType.PNG24  ;
      gisPixelStoreTypePNG8   = TGIS_PixelStoreType.PNG8   ;
      gisPixelStoreTypePNG32  = TGIS_PixelStoreType.PNG32  ;
      gisPixelStoreTypeGRID   = TGIS_PixelStoreType.GRID   ;
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.ZLib,
    System.Generics.Collections,

    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoParams ;
{$ENDIF}

const
  CELL_SIZE = 512 ;
  IMG_BMP   = 0   ;
  IMG_JPG24 = 10  ;
  IMG_JPG8  = 11  ;
  IMG_PNG24 = 20  ;
  IMG_PNG8  = 21  ;
  IMG_PNG1  = 22  ;

type
  T_SQLPS = class
  public
  const
  ID_BEGIN = 0                                                          ;

  CREATE_MASTER =
    'CREATE TABLE %s<#ttkGISPixelStore2#> '                                   +
    '( <#NAME#> <#VARCHAR#>(64),'                                             +
    '<#XMIN#> <#DOUBLE#>, <#XMAX#> <#DOUBLE#>,'                               +
    '<#YMIN#> <#DOUBLE#>, <#YMAX#> <#DOUBLE#>,'                               +
    '<#PIXELSIZE#> <#DOUBLE#>, <#IMAGETYPE#> <#SMALLINT#>,'                   +
    '<#NCOMPRESS#> <#SMALLINT#>, <#NLEVEL#> <#SMALLINT#>,'                    +
    '<#SRTEXT#> <#VARCHAR(2048)#>)'                                           ;
    ID_CREATE_MASTER = ID_BEGIN                                   ;

  CREATE_TABLE_CELL =
    'CREATE TABLE %s ('                                                       +
    '<#NLEVEL#> <#INTEGER#>, <#NROW#> <#INTEGER#>, <#NCOL#> <#INTEGER#>,'     +
    '<#CELL#> <#BLOB#> )'                                                     ;
    ID_CREATE_TABLE_CELL = ID_CREATE_MASTER + 1                   ;

  DROP_TABLE =
    'DROP TABLE <#IF_EXISTS#> %s'                                             ;
    ID_DROP_TABLE = ID_CREATE_TABLE_CELL + 1                      ;

  CREATE_INDEX_NAME =
    'CREATE UNIQUE INDEX <#ttkGISPixelStore2#><#_IDX_#><#NAME#>'              +
    ' ON %s<#ttkGISPixelStore2#> (<#NAME#>)'                                    ;
    ID_CREATE_INDEX_NAME = ID_DROP_TABLE + 1                      ;

  CREATE_INDEX_PK =
    'CREATE UNIQUE INDEX %s<#_IDX_#><#PK#> ON %s (<#NLEVEL#>,<#NROW#>,<#NCOL#>)';
    ID_CREATE_INDEX_PK = ID_CREATE_INDEX_NAME + 1                 ;

  CREATE_INDEX_LEVEL =
    'CREATE INDEX %s<#_IDX_#><#NLEVEL#> ON %s (<#NLEVEL#>)'                   ;
    ID_CREATE_INDEX_LEVEL = ID_CREATE_INDEX_PK + 1                ;

  CREATE_INDEX_ROW =
    'CREATE INDEX %s<#_IDX_#><#NROW#> ON %s (<#NROW#>)'                       ;
    ID_CREATE_INDEX_ROW = ID_CREATE_INDEX_LEVEL + 1               ;

  CREATE_INDEX_COL =
    'CREATE INDEX %s<#_IDX_#><#NCOL#> ON %s (<#NCOL#>)'                       ;
    ID_CREATE_INDEX_COL = ID_CREATE_INDEX_ROW + 1                 ;

  INSERT_MASTER =
    'INSERT INTO %s<#ttkGISPixelStore2#>'                                       +
    ' VALUES(''%s'',%s,%s,%s,%s,%s,%d,%d,%d,''%s'')'                          ;
    ID_INSERT_MASTER = ID_CREATE_INDEX_COL + 1                    ;

  UPDATE_MASTER =
    'UPDATE %s<#ttkGISPixelStore2#>'                                            +
    ' SET <#NLEVEL#>=%D'                                                      +
    ' WHERE <#NAME#>=''%s'''                                                  ;
    ID_UPDATE_MASTER = ID_INSERT_MASTER + 1                       ;

  SELECT_MASTER =
    'SELECT * FROM %s<#ttkGISPixelStore2#> WHERE <#NAME#>=''%s'''               ;
    ID_SELECT_MASTER = ID_UPDATE_MASTER + 1                       ;

  SELECT_MASTER_ALL =
    'SELECT <#NAME#> FROM %s<#ttkGISPixelStore2#> '                             ;
    ID_SELECT_MASTER_ALL = ID_SELECT_MASTER + 1                   ;

  DELETE_TABLE =
    'DELETE FROM %s<#ttkGISPixelStore2#> WHERE <#NAME#>=''%s'''                 ;
    ID_DELETE_TABLE = ID_SELECT_MASTER_ALL + 1                    ;

  SELECT_TABLE_ALL =
    'SELECT * FROM %s'                                                        ;
    ID_SELECT_TABLE_ALL = ID_DELETE_TABLE + 1                     ;

  SELECT_TABLE_ALL_EX =
    'SELECT %s FROM %s'                                                       ;
    ID_SELECT_TABLE_ALL_EX = ID_SELECT_TABLE_ALL + 1              ;

  SELECT_TABLE_WHERE =
    'SELECT * FROM %s WHERE %s'                                               ;
    ID_SELECT_TABLE_WHERE = ID_SELECT_TABLE_ALL_EX + 1            ;

  SELECT_TABLE_WHERE_EX =
    'SELECT %s FROM %s WHERE %s'                                              ;
    ID_SELECT_TABLE_WHERE_EX = ID_SELECT_TABLE_WHERE + 1          ;

  INSERT_DBX_CELL_VALUE =
    '<#NLEVEL#>,<#NROW#>,<#NCOL#>,<#CELL#>'                                   ;
    ID_INSERT_DBX_CELL_VALUE = ID_SELECT_TABLE_WHERE_EX + 1       ;

  INSERT_DBX_CELL_PARAM =
    ':<#NLEVEL#>,:<#NROW#>,:<#NCOL#>,<#:CELL#>'                               ;
    ID_INSERT_DBX_CELL_PARAM = ID_INSERT_DBX_CELL_VALUE + 1       ;

  INSERT_DBX =
    'INSERT INTO %s ( %s ) VALUES ( %s ) %s'                                  ;
    ID_INSERT_DBX = ID_INSERT_DBX_CELL_PARAM + 1                  ;

  SELECT_DB_AND_USER =
    'SELECT CURRENT_DATABASE() <#F_TABLE_CATALOG#>,'                          +
    'CURRENT_SCHEMA <#F_TABLE_SCHEMA#>'                                        ;
    ID_SELECT_DB_AND_USER = ID_INSERT_DBX + 1         ;

  UPDATE_DBX_CELL =
    '<#NLEVEL#>=:<#NLEVEL#>,'                                                 +
    '<#NROW#>=:<#NROW#>,'                                                     +
    '<#NCOL#>=:<#NCOL#>,'                                                     +
    '<#CELL#>=:<#CELL#>'                                                      ;
    ID_UPDATE_DBX_CELL = ID_SELECT_DB_AND_USER + 1   ;

  UPDATE_CELL =
    '<#NLEVEL#>=$1,'                                                          +
    '<#NROW#>=$2,'                                                            +
    '<#NCOL#>=$3,'                                                            +
    '<#CELL#>=$4'                                                             ;
    ID_UPDATE_CELL = ID_UPDATE_DBX_CELL + 1                       ;

  UPDATE_DBX_ORA =
    'returning <#CELL#> into :<#CELL#>'                                       ;
    ID_UPDATE_DBX_ORA = ID_UPDATE_CELL + 1                        ;

  UPDATE_DBX =
    'UPDATE %s SET %s WHERE %s'                                               ;
    ID_UPDATE_DBX = ID_UPDATE_DBX_ORA + 1                         ;

  FILTER_CELL =
    '(<#NLEVEL#>=%d) AND (<#NROW#>=%d) AND (<#NCOL#>=%d)'                     ;
    ID_FILTER_CELL = ID_UPDATE_DBX + 1                            ;

  MASTERLAYER =
    '<#ttkGISPixelStore2#>'                                                   ;
    ID_MASTERLAYER = ID_FILTER_CELL + 1                           ;
  NAME =
    '<#NAME#>'                                                                ;
    ID_NAME = ID_MASTERLAYER + 1                                  ;
  XMIN =
    '<#XMIN#>'                                                                ;
    ID_XMIN = ID_NAME + 1                                         ;
  XMAX =
    '<#XMAX#>'                                                                ;
    ID_XMAX = ID_XMIN + 1                                         ;
  YMIN =
    '<#YMIN#>'                                                                ;
    ID_YMIN = ID_XMAX + 1                                         ;
  YMAX =
    '<#YMAX#>'                                                                ;
    ID_YMAX = ID_YMIN + 1                                         ;
  PIXELSIZE =
    '<#PIXELSIZE#>'                                                           ;
    ID_PIXELSIZE = ID_YMAX + 1                                    ;
  IMAGETYPE =
    '<#IMAGETYPE#>'                                                           ;
    ID_IMAGETYPE = ID_PIXELSIZE + 1                               ;
  COMPRESS =
    '<#NCOMPRESS#>'                                                           ;
    ID_COMPRESS = ID_IMAGETYPE + 1                                ;
  LEVEL =
    '<#NLEVEL#>'                                                              ;
    ID_LEVEL = ID_COMPRESS + 1                                    ;
  WKT =
    '<#SRTEXT#>'                                                              ;
    ID_WKT = ID_LEVEL + 1                                         ;
  COL =
    '<#NCOL#>'                                                                ;
    ID_COL = ID_WKT + 1                                           ;
  ROW =
    '<#NROW#>'                                                                ;
    ID_ROW = ID_COL + 1                                           ;
  CELL =
    '<#CELL#>'                                                                ;
    ID_CELL = ID_ROW + 1                                          ;

  ID_END = ID_CELL                                                ;
  end;

type
  { Item in LRU list.
  }
  T_lruItem = class( TGIS_ObjectDisposable )
    public
      ScopeFactor  : Integer           ;
      ScopePos     : TPoint            ;
      objStream    : TGIS_MemoryStream ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create  ;
  end ;

  { LRU List. For caching cells.
  }
  {$IFDEF JAVA}
  T_lruList = class( java.util.ArrayList<T_lruItem>, IDisposable )
  {$ELSE}
  T_lruList = class( TList<T_lruItem>
                     {$IFDEF OXYGENE}
                     , IDisposable
                     {$ENDIF}
                   )
  {$ENDIF}
    private
      FMaxSize : Integer ;
      FPixelStore : TGIS_FilePixelStoreAbstract ;
    private
      procedure fset_MaxSize( const _value       : Integer ) ;
      procedure moveTop   ( const _idx         : Integer ) ;
    {$IFDEF OXYGENE}
      protected
        procedure Dispose  ; virtual;
    {$ELSE}
      public
        destructor Destroy ; override;
    {$ENDIF}
    public
      constructor Create  ( const _pixelStore  : TGIS_FilePixelStoreAbstract ) ;

      function Fetch      ( const _scopeFactor : Integer ;
                            const _scopePos    : TPoint
                          ) : Boolean ;
      function Update     ( const _scopeFactor : Integer ;
                            const _scopePos    : TPoint
                          ) : Boolean ;
    public
      property MaxSize : Integer read FMaxSize write fset_MaxSize ;
    end ;

//==============================================================================
// T_lruItem
//==============================================================================

  constructor T_lruItem.Create ;
  begin
    {$IFDEF OXYGENE}
      inherited Create ;
    {$ENDIF}
    objStream := TGIS_MemoryStream.Create ;
    ScopePos  := Point( 0, 0 ) ;
  end ;

  procedure T_lruItem.doDestroy ;
  begin
    FreeObject( objStream ) ;
    inherited ;
  end ;

//==============================================================================
// T_lruList
//==============================================================================

  constructor T_lruList.Create(
    const _pixelStore : TGIS_FilePixelStoreAbstract
  ) ;
  begin
    inherited Create ;

    FPixelStore := _pixelStore ;
    {$IFDEF MSWINDOWS}
      FMaxSize := {$IFDEF OXYGENE}Math.{$ENDIF}Max( 15,
                       ( CeilS( ( 1.0 * GetSystemMetrics(SM_CXSCREEN) ) / CELL_SIZE ) + 1 ) *
                       ( CeilS( ( 1.0 * GetSystemMetrics(SM_CYSCREEN) ) / CELL_SIZE ) + 1 ) *
                       2
                     ) ;
    {$ELSE}
      FMaxSize := 15 ;
    {$ENDIF}
  end ;

  {$IFDEF OXYGENE}
    procedure T_lruList.Dispose ;
  {$ELSE}
    destructor T_lruList.Destroy ;
  {$ENDIF}
  var
    i   : Integer ;
    itm : T_lruItem ;
  begin
    for i:= Count-1 downto 0 do begin
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          itm := T_lruItem( Item[i] ) ;
        {$ELSE}
          itm := T_lruItem( get(i) ) ;
        {$ENDIF}
      {$ELSE}
        itm := T_lruItem( Items[i] ) ;
      {$ENDIF}
      {$IFNDEF NEXTGEN}
        FreeObject( itm ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          RemoveAt( i ) ;
        {$ELSE}
          &remove( i ) ;
        {$ENDIF}
      {$ELSE}
        Delete( i ) ;
      {$ENDIF}
    end ;

    {$IFDEF OXYGENE}
      {$IFDEF CLR}
      GC.SuppressFinalize( self ) ;
      {$ENDIF}
    {$ELSE}
      inherited ;
    {$ENDIF}
  end ;

  procedure T_lruList.fset_MaxSize(
    const _value : Integer
  ) ;
  var
    i   : Integer ;
    itm : T_lruItem ;
  begin
    if _value < 0 then MaxSize := 0
                  else MaxSize := _value ;

    for i:=Count-1 downto MaxSize do begin
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          itm := T_lruItem( Item[i] ) ;
        {$ELSE}
          itm := T_lruItem( get(i) ) ;
        {$ENDIF}
      {$ELSE}
        itm := T_lruItem( Items[i] ) ;
      {$ENDIF}
      {$IFNDEF NEXTGEN}
        FreeObject( itm ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          RemoveAt( i ) ;
        {$ELSE}
          &remove( i ) ;
        {$ENDIF}
      {$ELSE}
        Delete( i ) ;
      {$ENDIF}
    end ;
  end ;

  procedure T_lruList.moveTop(
    const _idx : Integer
  ) ;
  var
    itm : T_lruItem ;
  begin
    assert( _idx < Count ) ;
    if _idx = 0 then exit ;
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
        itm := T_lruItem( Item[ _idx ] ) ;
      {$ELSE}
        itm := T_lruItem( get(_idx) ) ;
      {$ENDIF}
      {$IFDEF CLR}
        RemoveAt( _idx ) ;
      {$ELSE}
        &remove( _idx ) ;
      {$ENDIF}
    {$ELSE}
      itm := T_lruItem( Items[ _idx ] ) ;
      Delete( _idx ) ;
    {$ENDIF}
    {$IFDEF JAVA}
    &add( 0, itm ) ;
    {$ELSE}
    Insert( 0, itm ) ;
    {$ENDIF}
  end ;

  function T_lruList.Fetch(
    const _scopeFactor : Integer ;
    const _scopePos    : TPoint
  ) : Boolean ;
  var
    i : Integer ;
    itm : T_lruItem ;
  begin
    Result := False ;

    i := 0 ;
    while i <= Count -1 do begin
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          itm := T_lruItem( Item[i] ) ;
        {$ELSE}
          itm := T_lruItem( get(i) ) ;
        {$ENDIF}
      {$ELSE}
        itm := T_lruItem( Items[i] ) ;
      {$ENDIF}
      with itm do begin
        if ( ScopeFactor = _scopeFactor ) and
           ( ScopePos.X  = _scopePos.X  ) and
           ( ScopePos.Y  = _scopePos.Y  )
        then begin
          // for some magic reasons it will cause faster and
          // more safe operation on Win 9x family
          if FPixelStore.IsGrid then begin
            objStream.Position := 0 ;
            FPixelStore.objGrid.LoadFromStream( objStream );
          end
          else begin
            FreeObject( FPixelStore.objBmp ) ;
            FPixelStore.objBmp := TGIS_Bitmap.Create ;
            objStream.Position := 0 ;
            FPixelStore.objBmp.LoadFromStream( objStream ) ;
          end ;

          moveTop( i ) ;
          Result := True ;
          exit ;
        end ;
      end ;
      inc( i ) ;
    end ;
  end ;

  function T_lruList.Update(
    const _scopeFactor : Integer ;
    const _scopePos    : TPoint
  ) : Boolean ;
  var
    i : Integer ;
    itm : T_lruItem ;

    procedure update_internal( const itm : T_lruItem ) ;
    begin
      with itm do begin
        ScopeFactor := _scopeFactor ;
        ScopePos.X  := _scopePos.X  ;
        ScopePos.Y  := _scopePos.Y  ;
        if FPixelStore.IsGrid then
          FPixelStore.objGrid.SaveToStream( objStream )
        else begin
          {$IFDEF JAVA}
            case FPixelStore.ImageType of
                TGIS_PixelStoreType.BMP    :
                  FPixelStore.objBmp.SaveToStream( objStream, TGIS_PixelFormat.ARGB, TGIS_PixelSubFormat.BMP, 0 ) ;
                TGIS_PixelStoreType.JPEG24 ,
                TGIS_PixelStoreType.JPEG8  :
                  FPixelStore.objBmp.SaveToStream( objStream, TGIS_PixelFormat.ARGB, TGIS_PixelSubFormat.JPEG, 0 ) ;
                TGIS_PixelStoreType.PNG24  ,
                TGIS_PixelStoreType.PNG8   ,
                TGIS_PixelStoreType.PNG32  :
                  FPixelStore.objBmp.SaveToStream( objStream, TGIS_PixelFormat.ARGB, TGIS_PixelSubFormat.PNG, 0 ) ;
            end;
          {$ELSE}
            FPixelStore.objBmp.SaveToStream( objStream ) ;
          {$ENDIF}
        end
      end ;
    end ;
  begin
    Result := False ;

    i := 0 ;
    while i <= Count -1 do begin
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          itm := T_lruItem( Item[i] ) ;
        {$ELSE}
          itm := T_lruItem( get(i) ) ;
        {$ENDIF}
      {$ELSE}
        itm := T_lruItem( Items[i] ) ;
      {$ENDIF}
      with itm do begin
        if ( ScopeFactor = _scopeFactor ) and
           ( ScopePos.X  = _scopePos.X  ) and
           ( ScopePos.Y  = _scopePos.Y  )
        then begin
          update_internal( itm ) ;
          Result := True ;
          moveTop( i ) ;
          exit ;
        end ;
      end ;
      inc( i ) ;
    end ;

    // not found -adding new one

    // but first delete last one if necessary
    if ( Count > 0 ) and ( Count >= MaxSize ) then begin
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          itm := T_lruItem( Item[Count-1] ) ;
        {$ELSE}
          itm := T_lruItem( get(Count-1) ) ;
        {$ENDIF}
      {$ELSE}
        itm := T_lruItem( Items[Count-1] ) ;
      {$ENDIF}
      {$IFNDEF NEXTGEN}
        FreeObject( itm ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          RemoveAt( Count-1 ) ;
        {$ELSE}
          &remove( Count-1 ) ;
        {$ENDIF}
      {$ELSE}
        Delete( Count-1 ) ;
      {$ENDIF}
    end ;

    // create a new item
    itm := T_lruItem.Create ;
    update_internal( itm ) ;

    // and add it to the top
    {$IFDEF JAVA}
      &add( 0, itm ) ;
    {$ELSE}
      Insert(0, itm ) ;
    {$ENDIF}
  end ;

  constructor TGIS_GridClass.Create(
    const _width, _height : Integer
  ) ;
  begin
    {$IFDEF OXYGENE}
      inherited Create ;
    {$ENDIF}
    SetLength( FGrid, _width, _height ) ;
    FWidth  := _width  ;
    FHeight := _height ;
  end ;

  procedure TGIS_GridClass.Resize(
    const _width, _height : Integer
  ) ;
  begin
    SetLength( FGrid, _width, _height ) ;
    FWidth  := _width  ;
    FHeight := _height ;
  end ;

  procedure TGIS_GridClass.Clear ;
  var
    i, j : Integer ;
  begin
    for i := 0 to FWidth -1 do
      for j := 0 to FHeight -1 do
        FGrid[i][j] := GIS_GRID_NOVALUE ;
  end ;

  procedure TGIS_GridClass.Assign(
    const _grd : TGIS_GridArray
  ) ;
  var
    i, j : Integer ;
  begin
    SetLength( FGrid, length( _grd ), length( _grd[0] ) ) ;

    for i := 0 to FWidth -1 do
      for j := 0 to FHeight -1 do
        FGrid[i][j] := _grd[i][j] ;
  end ;

  procedure TGIS_GridClass.Assign(
    const _grd : TGIS_GridClass
  ) ;
  begin
    Assign( _grd.Grid ) ;
  end ;

  {$IFDEF OXYGENE}
    procedure TGIS_GridClass.LoadFromStream(
      const _stream: TGIS_BaseStream
    );
  {$ELSE}
    procedure TGIS_GridClass.LoadFromStream(
      const _stream: TStream
    );
  {$ENDIF}
  var
    i,j        : Integer ;
    grd_header : DWORD   ;
    grd_width  : Word    ;
    grd_height : Word    ;
  begin
    {$IFDEF OXYGENE}
      _stream.ReadCardinal( grd_header, sizeOf( grd_header ) ) ;
    {$ELSE}
      _stream.Read( grd_header, sizeOf( grd_header ) ) ;
    {$ENDIF}
    assert( grd_header = 1 ) ;
    {$IFDEF OXYGENE}
      _stream.ReadWord( grd_width , sizeOf( grd_width  ) ) ;
    {$ELSE}
      _stream.Read( grd_width , sizeOf( grd_width  ) ) ;
    {$ENDIF}
    assert( grd_width = 512 ) ;
    {$IFDEF OXYGENE}
      _stream.ReadWord( grd_height, sizeOf( grd_height ) ) ;
    {$ELSE}
      _stream.Read( grd_height, sizeOf( grd_height ) ) ;
    {$ENDIF}
    assert( grd_height = 512 ) ;

    Resize( grd_width, grd_height ) ;

    for i := 0 to grd_width -1 do
      for j := 0 to grd_height -1 do
        {$IFDEF OXYGENE}
          _stream.ReadSingle( FGrid[i][j], sizeOf( FGrid[i][j] ) ) ;
        {$ELSE}
          _stream.Read( FGrid[i][j], sizeOf( FGrid[i][j] ) ) ;
        {$ENDIF}
  end ;

  procedure TGIS_GridClass.SaveToFile(
    const _name: String
  );
  var
    f : TGIS_FileStream ;
  begin
    f := TGIS_FileStream.Create( _name, fmCreate );
    SaveToStream( f ) ;
    FreeObject( f ) ;
  end ;

  {$IFDEF OXYGENE}
    procedure TGIS_GridClass.SaveToStream(
      const _stream: TGIS_BaseStream
    );
  {$ELSE}
    procedure TGIS_GridClass.SaveToStream(
      const _stream: TStream
    );
  {$ENDIF}
  var
    i,j        : Integer ;
    grd_header : DWORD   ;
    grd_width  : Word    ;
    grd_height : Word    ;
  begin
    grd_header := 1     ;
    assert( Width  = 512 ) ;
    assert( Height = 512 ) ;
    grd_width  := Width ;
    grd_height := Height ;
    {$IFDEF OXYGENE}
      _stream.WriteCardinal( grd_header, sizeOf( grd_header ) ) ;
      _stream.WriteWord( grd_width, sizeOf( grd_width ) ) ;
      _stream.WriteWord( grd_height, sizeOf( grd_height ) ) ;
    {$ELSE}
      _stream.Write( grd_header, sizeOf( grd_header ) ) ;
      _stream.Write( grd_width, sizeOf( grd_width ) ) ;
      _stream.Write( grd_height, sizeOf( grd_height ) ) ;
    {$ENDIF}

    for i := 0 to grd_width -1 do
      for j := 0 to grd_height -1 do
        {$IFDEF OXYGENE}
          _stream.WriteSingle( FGrid[i][j], sizeOf( FGrid[i][j] ) ) ;
        {$ELSE}
          _stream.Write( FGrid[i][j], sizeOf( FGrid[i][j] ) ) ;
        {$ENDIF}
  end ;

  procedure TGIS_GridClass.CopyRect(
    const _dest   : TRect;
    const _grid   : TGIS_GridArray;
    const _source : TRect
  );
  var
    max_w_src : Integer ;
    max_h_src : Integer ;
    w_ratio   : Double  ;
    h_ratio   : Double  ;
    si, sj    : Integer ;
    di, dj    : Integer ;
    li, lj    : Integer ;
  begin
    max_w_src := length( _grid ) ;
    max_h_src := length( _grid[0] ) ;

    w_ratio := ( 1.0 * ( _source.Right  - _source.Left ) ) / ( _dest.Right  - _dest.Left )  ;
    h_ratio := ( 1.0 * ( _source.Bottom - _source.Top  ) ) / ( _dest.Bottom - _dest.Top  )  ;

    li := 0 ;
    for si := Max( 0, _dest.Left ) to Min( Width, _dest.Right ) - 1 do begin
      lj := 0 ;
      for sj := Max( 0, _dest.Top ) to Min( Height, _dest.Bottom ) - 1 do begin
         di := RoundS( _source.Left + li* w_ratio ) ;
         if di < 0 then continue ;
         if di >= max_w_src -1 then continue ;

         dj := RoundS( _source.Top + lj* h_ratio ) ;
         if dj < 0 then continue ;
         if dj >= max_h_src -1 then continue ;

         FGrid[ sj][si ] := _grid[ dj][di ] ;
         inc ( lj );
      end ;
      inc ( li );
    end ;

  end ;

//==============================================================================
// TGIS_PixelStoreCell
//==============================================================================

  constructor TGIS_PixelStoreCell.Create ;
  begin
    {$IFDEF OXYGENE}
      inherited ;
    {$ENDIF}
    FStream := TGIS_MemoryStream.Create ;
    {$IFDEF OXYGENE}
      FModified := False ;
    {$ENDIF}
    {$IFDEF GIS_NORECORDS}
    FExtent := new TGIS_Extent ();
    {$ENDIF}
  end ;

  procedure TGIS_PixelStoreCell.doDestroy ;
  begin
    FreeObject( FStream ) ;

    inherited ;
  end ;

//==============================================================================
// TGIS_FilePixelStoreAbstract
//==============================================================================

   procedure TGIS_FilePixelStoreAbstract.doCreate ;
   begin
    scopeEof := True ;

    objBmp := TGIS_Bitmap.Create( CellSize, CellSize) ;//, pf24bit ) ;

    objGrid:= TGIS_GridClass.Create( CellSize, CellSize ) ;

    FColor  := TGIS_Color.Black ;

    objCell := TGIS_PixelStoreCell.Create ;

    lruList := TObject( T_lruList.Create( self ) ) ;

    FSQLParameters  := TStringList.Create ;
    FSQLDialectList := TStringList.Create ;
    FSQLCommands    := TStringList.Create ;

    tmpPasswords  := TGIS_PasswordList.Create ;
    FRelativePath := '' ;
    FMaxLevel     := 1 ;
    directWrite   := False ;
    scopePos      := Point( 0, 0 ) ;
   end ;

  constructor TGIS_FilePixelStoreAbstract.Create ;
  begin
    inherited ;

    FGIS_Layer := nil ;

    doCreate ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.doCreateForWrite(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer
  ) ;
  begin
    doCreateForWrite( _path, _ext, _width, _height,
                      TGIS_LayerPixelSubFormat.DefaultSubFormat, 300, nil
                     ) ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.doCreateForWrite(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat     ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;

    procedure do_build(
      const _type   : TGIS_PixelStoreType ;
      const _srtext : String
    ) ;
    var
      ps : TGIS_FilePixelStoreAbstract ;
    begin
      if not IsStringEmpty( _path ) then begin
        {$IFDEF OXYGENE}
          {$IFDEF JAVA}
            ps := TGIS_FilePixelStoreAbstract(&Class.forName(Self.Class.Name).getConstructor().newInstance());
          {$ELSE}
            ps := TGIS_FilePixelStoreAbstract( Activator.CreateInstance( Self.GetType() ) ) ;
          {$ENDIF}
        {$ELSE}
          ps := TGIS_FilePixelStoreAbstractClass( Self.ClassType ).Create ;
        {$ENDIF}
        try
          ps.FSQLParameters.Assign( FSQLParameters );
          ps.oGisDb.SQLExecuteEvent := oGisDb.SQLExecuteEvent ;
          ps.Path := _path ;
          ps.macroConnect ;
          try
            try
              ps.macroMasterCreate ;
            except
              // master can already exist
            end ;
            try
              ps.macroTableDrop ;
            except
              // table can not exist
            end ;
            ps.macroTableCreate( _ext       ,
                                 _type      ,
                                 Max(1, CompressionLevel ) ,
                                 FPixelSize ,
                                 _srtext
                               ) ;
          finally
            ps.macroDisconnect ;
          end ;
        finally
          FreeObject( ps ) ;
        end ;
      end
      else begin
        macroConnect ;
        try
          try
            macroMasterCreate ;
          except
            // master can already exist
          end ;
          ps.macroTableCreate( _ext         ,
                               _type        ,
                               FCompressionLevel ,
                               FPixelSize   ,
                               _srtext
                             ) ;
        finally
          macroDisconnect ;
        end ;
      end ;
    end ;

  begin

    doCreate ;

    FWriteTileWidth  := CELL_SIZE ;
    FWriteTileHeight := CELL_SIZE ;

    FPixelFormat      := SubFormat.PixelFormat ;
    if SubFormat.CompressionLevel > 0 then
      FCompressionLevel := SubFormat.CompressionLevel ;
    FPixelSize        := ( _ext.XMax - _ext.XMin ) / Width ;

    if assigned( _cs ) then
      FWKT := _cs.FullWKT ;

    if SubFormat.PixelFormat = TGIS_PixelFormat.Custom then begin
      do_build( TGIS_PixelStoreType.GRID, FWKT ) ;
      FIsGrid := True ;
    end
    else begin
      case SubFormat.Compression of
       TGIS_CompressionType.JPEG :
          begin
            case SubFormat.GrayScale of
              True  :
                do_build( TGIS_PixelStoreType.JPEG8 , FWKT ) ;
              False :
               do_build( TGIS_PixelStoreType.JPEG24, FWKT ) ;
            else
              begin
                assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
              end ;
            end ;
         end ;
        TGIS_CompressionType.PNG  :
          begin
            case SubFormat.PixelFormat of
              TGIS_PixelFormat.Bit8 :
                do_build( TGIS_PixelStoreType.PNG8, FWKT ) ;
              TGIS_PixelFormat.RGB  :
                do_build( TGIS_PixelStoreType.PNG24, FWKT ) ;
              TGIS_PixelFormat.ARGB :
                do_build( TGIS_PixelStoreType.PNG32, FWKT ) ;
           else
              begin
                assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
              end ;
            end ;
          end ;
       else
          do_build( TGIS_PixelStoreType.BMP, FWKT ) ;
      end ;
    end ;

    Path := _path ;
    Open ;

  end ;

  constructor TGIS_FilePixelStoreAbstract.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  begin
    inherited Create( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    ) ;

    strName    := '' ;
    strSchema  := '' ;
    strCatalog := '' ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.doDestroy ;
  begin
    FreeObject( FSQLParameters  ) ;
    FreeObject( FSQLDialectList ) ;
    FreeObject( FSQLCommands    ) ;

    FreeObject( objBmp  ) ;
    FreeObject( objGrid ) ;
    FreeObject( objCell ) ;

    FreeObject( lruList ) ;

    FreeObject( tmpPasswords ) ;

    inherited ;
  end ;

  function TGIS_FilePixelStoreAbstract.fget_SQLParameter(
    const _name : String
  ) : String ;
  begin
    Result := FSQLParameters.Values[ _name ] ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.fset_SQLParameter(
    const _name  : String ;
    const _value : String
  ) ;
  begin
    FSQLParameters.Values[ _name ] := _value ;
  end ;

  function TGIS_FilePixelStoreAbstract.fget_TableMaster : String ;
  begin
    Result := getCmd( T_SQLPS.ID_MASTERLAYER ) ;
  end ;

  function TGIS_FilePixelStoreAbstract.fget_Table : String ;
  begin
    if ( not IsStringEmpty( strSchema ) ) and not oGisDb.IsMsJet then
      Result := Format( '%s.%s', [ strSchema, FTable ] )
    else
      Result := FTable ;
  end ;

  function TGIS_FilePixelStoreAbstract.fget_CellSize : Integer ;
  begin
    Result := CELL_SIZE  ;
  end ;

  function TGIS_FilePixelStoreAbstract.doPrerecognize(
    const _path : String
  ) : Integer ;
  var
    tmp  : String   ;
    tp   : Integer  ;
  begin
    Result := 0 ;

    tp := GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_STORAGE, tmp ) ;

    if ( tp = 1 ) and
       ( not SafeFileExists( _path ) and not IsEmbeddedSQLPath( _path ) ) then begin
      Result := 1 ;
      exit ;
    end ;

    if CompareText( tmp, GIS_INI_LAYERSQL_PIXELSTORE2 ) = 0 then begin
      GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_CONNECTOR_ADO, tmp ) ;
      if not IsStringEmpty( tmp ) then
        Result := GIS_SQL_PROVIDER_ADO
      else begin
        GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_CONNECTOR_DBX, tmp ) ;
        if not IsStringEmpty( tmp ) then
          Result := GIS_SQL_PROVIDER_DBX
        else begin
          GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_CONNECTOR_SQLITE, tmp ) ;
          if not IsStringEmpty( tmp ) then
            Result := GIS_SQL_PROVIDER_SQLITE
          else begin
            GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_CONNECTOR_ADONET, tmp ) ;
            if not IsStringEmpty( tmp ) then
              Result := GIS_SQL_PROVIDER_ADONET
            else begin
              GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_CONNECTOR_LIBPQ, tmp ) ;
              if not IsStringEmpty( tmp ) then
                Result := GIS_SQL_PROVIDER_LIBPQ
              else begin
                GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_CONNECTOR_JDBC, tmp ) ;
                if not IsStringEmpty( tmp ) then
                  Result := GIS_SQL_PROVIDER_JDBC
                else begin
                  GetSQLParamFromPath( _path, GIS_INI_LAYERSQL_CONNECTOR_FIREDAC, tmp ) ;
                  if not IsStringEmpty( tmp ) then
                    Result := GIS_SQL_PROVIDER_FIREDAC
                  else
                    Result := 0 ;
                end
              end
            end
          end ;
        end ;
      end ;
    end ;

  end ;

  function TGIS_FilePixelStoreAbstract.prepareSelectCommand(
    const _table  : String ;
    const _filter : String
  ) : String ;
  begin
    if not IsStringEmpty( _filter ) then
      Result := Format( getCmd( T_SQLPS.ID_SELECT_TABLE_WHERE ), [ _table, _filter ] )
    else
      Result := Format( getCmd( T_SQLPS.ID_SELECT_TABLE_ALL ), [ _table ] ) ;
  end ;

  function TGIS_FilePixelStoreAbstract.prepareFilterCell(
    const _level : Integer ;
    const _col   : Integer ;
    const _row   : Integer
  ) : String ;
  begin
    Result := Format( getCmd( T_SQLPS.ID_FILTER_CELL ), [ _level, _row, _col ] ) ;
  end ;

  function TGIS_FilePixelStoreAbstract.prepareAppendCmd(
    const _table : String
  ) : String ;
  var
    value : String ;
  begin
    value := LowerCase( getCmd( T_SQLPS.ID_INSERT_DBX_CELL_VALUE ) ) ;

    Result := Format( getCmd( T_SQLPS.ID_SELECT_TABLE_WHERE_EX ),
                      [value, _table, prepareFilterCell(-1,-1,-1)]
                    ) +
              ';' +
              Format( getCmd( T_SQLPS.ID_INSERT_DBX ),
                      [_table, value, '$1,$2,$3,$4', '']
                    ) ;
  end ;

  function TGIS_FilePixelStoreAbstract.prepareAppendCommand(
    const _table : String
  ) : String ;
  var
    param : String  ;
    value : String  ;
    tmp   : String  ;
  begin
    value := getCmd( T_SQLPS.ID_INSERT_DBX_CELL_VALUE ) ;
    param := getCmd( T_SQLPS.ID_INSERT_DBX_CELL_PARAM ) ;
    if oGisDb.IsOracle then
      tmp   := getCmd( T_SQLPS.ID_UPDATE_DBX_ORA )
    else
      tmp := '' ;

    Result := Format( getCmd( T_SQLPS.ID_INSERT_DBX ),
                      [_table, value, param, tmp]
                    ) ;
  end ;

  function TGIS_FilePixelStoreAbstract.prepareAppendParams(
    const _table : String
   ) : String ;
  var
    param : TStringBuilder  ;
    value : TStringBuilder  ;
  begin
    param := TStringBuilder.Create ;
    value := TStringBuilder.Create ;
    try
      value.Append( getCmd( T_SQLPS.ID_INSERT_DBX_CELL_VALUE ) ) ;
      param.Append( oGisDb.safeParam( getCmd( T_SQLPS.ID_LEVEL ) ) ) ;
      param.Append( ',' ) ;
      param.Append( oGisDb.safeParam( getCmd( T_SQLPS.ID_ROW ) ) ) ;
      param.Append( ',' ) ;
      param.Append( oGisDb.safeParam( getCmd( T_SQLPS.ID_COL ) ) ) ;
      param.Append( ',' ) ;
      param.Append( oGisDb.safeParam( getCmd( T_SQLPS.ID_CELL ) ) ) ;

      Result := Format( getCmd( T_SQLPS.ID_INSERT_DBX ),
                        [_table, value.ToString, param.ToString, '']
                       ) ;
    finally
      FreeObject( param ) ;
      FreeObject( value ) ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.macroAppendParams(
    const _table : String
   ) ;
  begin
    oGisDb.sqlTableCreateParam(
      0,
      oGisDb.safeParam( getCmd( T_SQLPS.ID_LEVEL ) ),
      TGIS_DataType.&Integer,
      TGIS_SubDataType.Unknown,
      8
     ) ;
    oGisDb.sqlTableCreateParam(
      0,
      oGisDb.safeParam( getCmd( T_SQLPS.ID_ROW ) ),
      TGIS_DataType.&Integer,
      TGIS_SubDataType.Unknown,
      8
     ) ;
    oGisDb.sqlTableCreateParam(
      0,
      oGisDb.safeParam( getCmd( T_SQLPS.ID_COL ) ),
      TGIS_DataType.&Integer,
      TGIS_SubDataType.Unknown,
      8
     ) ;
    oGisDb.sqlTableCreateParam(
      0,
      oGisDb.safeParam( getCmd( T_SQLPS.ID_CELL ) ),
      TGIS_DataType.Blob,
      TGIS_SubDataType.Unknown,
      -1
     ) ;
  end ;

  function TGIS_FilePixelStoreAbstract.prepareUpdateCmd(
    const _table  : String ;
    const _filter : String
  ) : String ;
  var
    param : String ;
    value : String ;
  begin
    value := LowerCase( getCmd( T_SQLPS.ID_INSERT_DBX_CELL_VALUE ) ) ;
    param := getCmd( T_SQLPS.ID_UPDATE_CELL ) ;

    Result := Format( getCmd( T_SQLPS.ID_SELECT_TABLE_WHERE_EX ),
                      [value, _table, _filter]
                    ) +
              ';' +
              Format( getCmd( T_SQLPS.ID_UPDATE_DBX ),
                      [_table, param, _filter ]
                    ) ;
  end ;

  function TGIS_FilePixelStoreAbstract.prepareUpdateCommand(
    const _table  : String ;
    const _filter : String
  ) : String ;
  var
    param : String  ;
    tmp   : String  ;
  begin
    param := '' ;
    if oGisDb.IsPostgreSql then
      param := getCmd( T_SQLPS.ID_UPDATE_CELL )
    else
    param := getCmd( T_SQLPS.ID_UPDATE_DBX_CELL ) ;
    if oGisDb.IsOracle then
      tmp   := getCmd( T_SQLPS.ID_UPDATE_DBX_ORA )
    else
      tmp := '' ;

    Result := Format( getCmd( T_SQLPS.ID_UPDATE_DBX ),
                      [_table, param, _filter ]
                    ) ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.updateDialectList(
     const _name  : String ;
     const _value : String
  ) ;
  var
    idx : Integer     ;
  begin
    idx := FSQLDialectList.IndexOfName( _name ) ;
    if ( idx <> -1 ) then begin
      if ( _name <> _value ) then
        FSQLDialectList.Strings[ idx ] := _name+'='+_value
      else
        FSQLDialectList.Delete( idx )
    end
    else
      if ( _name <> _value ) then
        FSQLDialectList.Add( _name+'='+_value ) ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.readCell ;
  var
    blob : TStream ;
    cmpr : TCustomZStream ;
    memstream : TMemoryStream ;
  begin
    if T_lruList(lruList).Fetch( scopeFactor, scopePos ) then begin
      CurrentCell.Stream.Clear ;
      if FIsGrid then
        objGrid.SaveToStream( CurrentCell.Stream )
      else begin
          {$IFDEF JAVA}
            case FImageType of
                TGIS_PixelStoreType.BMP    :
                  objBmp.SaveToStream( CurrentCell.Stream, TGIS_PixelFormat.ARGB, TGIS_PixelSubFormat.BMP, 0 ) ;
                TGIS_PixelStoreType.JPEG24 ,
                TGIS_PixelStoreType.JPEG8  :
                  objBmp.SaveToStream( CurrentCell.Stream, TGIS_PixelFormat.ARGB, TGIS_PixelSubFormat.JPEG, 0 ) ;
                TGIS_PixelStoreType.PNG24  ,
                TGIS_PixelStoreType.PNG8   ,
                TGIS_PixelStoreType.PNG32   :
                  objBmp.SaveToStream( CurrentCell.Stream, TGIS_PixelFormat.ARGB, TGIS_PixelSubFormat.PNG, 0 ) ;
            end;
          {$ELSE}
            objBmp.SaveToStream( CurrentCell.Stream ) ;
          {$ENDIF}
        end ;

      exit ;
    end ;

    oGisDb.sqlQueryOpen( prepareSelectCommand(
                    Table,
                    prepareFilterCell( scopeFactor, scopePos.X, scopePos.Y  )
                  ), 0
                ) ;
    try
      try
        if not oGisDb.sqlQueryEof(0) then begin
          blob := sqlQueryGetCell( getCmd( T_SQLPS.ID_CELL ) ) ;
          try
            if blob.Size = 0 then begin
              if FIsGrid then begin
                objGrid.Resize( CellSize, CellSize );
                objGrid.Clear ; //?xxx
              end
              else begin
                FreeObject( objBmp ) ;
                objBmp := TGIS_Bitmap.Create( CellSize, CellSize) ;
              end ;
            end
            else begin
              case ImageType of

                TGIS_PixelStoreType.BMP    ,
                TGIS_PixelStoreType.JPEG24 ,
                TGIS_PixelStoreType.JPEG8  ,
                TGIS_PixelStoreType.PNG24  ,
                TGIS_PixelStoreType.PNG8   ,
                TGIS_PixelStoreType.PNG32  :
                  begin
                    blob.Position := 0 ;
                    objBmp.LoadFromStream( blob ) ;
                  end ;
                TGIS_PixelStoreType.GRID :
                  begin
                    {$IFDEF OXYGENE}
                      memstream := TMemoryStream.Create ;
                      try
                        memstream.CopyFrom( blob, blob.Size ) ;
                        memstream.Position := 0 ;
                    {$ENDIF}
                    {$IFDEF OXYGENE}
                      cmpr := TZDecompressionStream.Create( memstream ) ;
                    {$ELSE}
                      cmpr := TZDecompressionStream.Create( blob ) ;
                    {$ENDIF}
                    try
                      objGrid.LoadFromStream( cmpr ) ;
                    finally
                      FreeObject( cmpr ) ;
                    end ;
                    {$IFDEF OXYGENE}
                      finally
                        FreeObject( memstream ) ;
                      end ;
                    {$ENDIF}
                  end ;
                else
                  assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
              end ;
            end ;
          finally
            T_lruList(lruList).Update( scopeFactor, scopePos ) ;
            FreeObject( blob ) ;
          end ;
        end
        else begin
          if FIsGrid then begin
            objGrid.Resize( CellSize, CellSize );
            objGrid.Clear ;
          end
          else begin
            if (objBmp.Width <> CellSize) or
               (objBmp.Height <> CellSize) then
            begin
              FreeObject( objBmp ) ;
              objBmp := TGIS_Bitmap.Create( CellSize, CellSize) ;
            end ;
          end ;
        end ;

        CurrentCell.Stream.Clear ;
        if FIsGrid then
          objGrid.SaveToStream( CurrentCell.Stream )
        else
          objBmp.SaveToStream( CurrentCell.Stream ) ;
      finally

      end ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;

  end ;

  function TGIS_FilePixelStoreAbstract.calculateScope(
    const _level  : Integer     ;
    const _extent : TGIS_Extent
  ) : TRect ;
  var
    dval : Double      ;
    ival : Integer     ;
    berr : Boolean     ;
    ext  : TGIS_Extent ;
    r_Left   : Integer ;
    r_Right  : Integer ;
    r_Top    : Integer ;
    r_Bottom : Integer ;
    tol      : Double ;
  begin
    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent() ;
    {$ENDIF}
    ext.XMin := _extent.XMin -  Extent.XMin ;
    ext.XMax := _extent.XMax -  Extent.XMin ;
    ext.YMin :=  Extent.YMax - _extent.YMin ;
    ext.YMax :=  Extent.YMax - _extent.YMax ;
    tol   := PixelSize / CellSize ;

    // left
    dval := ( ext.XMin/ PixelSize  ) / CellSize / _level ;
    ival := RoundS( dval ) ;
    berr := Abs(dval-ival) > tol ;
    if berr then begin
      if ival > 0 then
        ival := ival -1 ;
    end ;

    r_Left := ival ;

    // right
    dval := ( ext.XMax / PixelSize  ) / CellSize / _level ;
    if directWrite then
      ival := RoundS( dval )
    else
      ival := TruncS( dval ) ;
    berr := Abs(dval-ival) > tol ;
    if berr then
      ival := ival + 1 ;

    r_Right := ival ;

    // top
    dval := ( ext.YMax / PixelSize ) / CellSize / _level ;
    if directWrite then
      ival := RoundS( dval )
    else
      ival := FloorS( dval ) ;
    berr := Abs(dval-ival) > tol ;
    if berr then begin
      if ival > 0 then
        ival := ival -1 ;
    end ;

    r_Top := ival ;

    // bottom
    dval := ( ext.YMin / PixelSize ) / CellSize / _level ;
    if directWrite then
      ival := RoundS( dval )
    else
      ival := TruncS( dval ) ;
    berr := Abs(dval-ival) > tol ;
    if berr then
      ival := ival + 1 ;

    r_Bottom := ival ;

    Result := Rect( r_Left, r_Top, r_Right, r_Bottom ) ;
  end ;

  function TGIS_FilePixelStoreAbstract.calculateInsertTile(
     const _level  : Integer ;
     const _extent : TGIS_Extent
  ) : TRect ;
  var
    dval   : Double      ;
    ext    : TGIS_Extent ;
    r_Left : Integer ;
    r_Top  : Integer ;
  begin
    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent() ;
    {$ENDIF}
    ext.XMin := _extent.XMin -  Extent.XMin ;
    ext.XMax := _extent.XMax -  Extent.XMin ;
    ext.YMin :=  Extent.YMax - _extent.YMin ;
    ext.YMax :=  Extent.YMax - _extent.YMax ;

    // left
    dval := ( ext.XMin/ PixelSize  ) / CellSize / _level ;
    r_Left := RoundS( dval ) ;

    // top
    dval := ( ext.YMax / PixelSize ) / CellSize / _level ;
    r_Top := RoundS( dval ) ;

    Result := Rect( r_Left, r_Top, r_Left + 1, r_Top + 1 ) ;
  end;

  procedure TGIS_FilePixelStoreAbstract.updateFactor ;
  begin
    if scopeFactor <= FMaxLevel then exit ;

    FMaxLevel := Max( FMaxLevel, scopeFactor ) ;
    if not directWrite then
      oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_UPDATE_MASTER ),
                              [ getSchemaPrefix, FMaxLevel, Table ]
                             )
                     ) ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.UpdateSubFormat(
    const _sformat : TGIS_LayerPixelSubFormat
  ) ;
  begin
    FSubFormat := _sformat ;
  end;

  procedure TGIS_FilePixelStoreAbstract.setScopeQuad(
    const _pos  : TPoint  ;
    const _zoom : Double
  ) ;
  begin
   scopeFactor := RoundS(1 / _zoom) ;
   scopeRect   := Rect( _pos.X * 2,
                        _pos.Y * 2,
                        _pos.X * 2 + 2,
                        _pos.Y * 2 + 2
                      ) ;

   scopePos.X := scopeRect.Left ;
   scopePos.Y := scopeRect.Top  ;
   scopePos.X := scopePos.X - 1 ; // to be "before" first element

   scopeEof := False ;
  end ;

  function TGIS_FilePixelStoreAbstract.passwordCallBack(
    const _token : String
  ) : String ;
  var
    tmp : String ;
  begin
    if not IsStringEmpty( Name ) then tmp := Name
                                 else tmp := Table ;
    Result := GisPasswordList.Get( tmp, _token ) ;
    if IsStringEmpty( Result ) then begin
      if assigned( FOnPassword ) then
        {$IFDEF OXYGENE}
          Result := FOnPassword( Self,
                                 TGIS_TemplateProducerEventArgs.Create( _token )
                               )
        {$ELSE}
          Result := FOnPassword( Self, _token )
        {$ENDIF}
      else
        Result := _token ;
      GisPasswordList.Add( tmp, _token, Result ) ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.prepareCommandList ;
  var
    i   : Integer     ;
    lst : TStringList ;
  begin
    if assigned( FOnBeforeDialectChange ) then
      {$IFDEF CLR}
        FOnBeforeDialectChange( Self, EventArgs.Create ) ;
      {$ELSE}
        FOnBeforeDialectChange( Self ) ;
      {$ENDIF}

    FSQLCommands.Clear ;
    for i:=0 to T_SQLPS.ID_END do
      FSQLCommands.Add( '' ) ;

       assert( FSQLCommands[ T_SQLPS.ID_CREATE_MASTER         ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_CREATE_MASTER         ]
                             := T_SQLPS.CREATE_MASTER         ;
       assert( FSQLCommands[ T_SQLPS.ID_CREATE_TABLE_CELL     ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_CREATE_TABLE_CELL     ]
                             := T_SQLPS.CREATE_TABLE_CELL     ;
       assert( FSQLCommands[ T_SQLPS.ID_DROP_TABLE            ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_DROP_TABLE            ]
                             := T_SQLPS.DROP_TABLE            ;
       assert( FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_PK       ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_PK       ]
                             := T_SQLPS.CREATE_INDEX_PK       ;
       assert( FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_NAME     ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_NAME     ]
                             := T_SQLPS.CREATE_INDEX_NAME     ;
       assert( FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_LEVEL    ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_LEVEL    ]
                             := T_SQLPS.CREATE_INDEX_LEVEL    ;
       assert( FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_ROW      ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_ROW      ]
                             := T_SQLPS.CREATE_INDEX_ROW      ;
       assert( FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_COL      ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_CREATE_INDEX_COL      ]
                             := T_SQLPS.CREATE_INDEX_COL      ;
       assert( FSQLCommands[ T_SQLPS.ID_INSERT_MASTER         ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_INSERT_MASTER         ]
                             := T_SQLPS.INSERT_MASTER         ;
       assert( FSQLCommands[ T_SQLPS.ID_UPDATE_MASTER         ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_UPDATE_MASTER         ]
                             := T_SQLPS.UPDATE_MASTER         ;
       assert( FSQLCommands[ T_SQLPS.ID_SELECT_MASTER         ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_SELECT_MASTER         ]
                             := T_SQLPS.SELECT_MASTER         ;
       assert( FSQLCommands[ T_SQLPS.ID_SELECT_MASTER_ALL     ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_SELECT_MASTER_ALL     ]
                             := T_SQLPS.SELECT_MASTER_ALL     ;
       assert( FSQLCommands[ T_SQLPS.ID_DELETE_TABLE          ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_DELETE_TABLE          ]
                             := T_SQLPS.DELETE_TABLE          ;
       assert( FSQLCommands[ T_SQLPS.ID_SELECT_TABLE_ALL      ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_SELECT_TABLE_ALL      ]
                             := T_SQLPS.SELECT_TABLE_ALL      ;
       assert( FSQLCommands[ T_SQLPS.ID_SELECT_TABLE_ALL_EX   ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_SELECT_TABLE_ALL_EX   ]
                             := T_SQLPS.SELECT_TABLE_ALL_EX   ;
       assert( FSQLCommands[ T_SQLPS.ID_SELECT_TABLE_WHERE    ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_SELECT_TABLE_WHERE    ]
                             := T_SQLPS.SELECT_TABLE_WHERE    ;
       assert( FSQLCommands[ T_SQLPS.ID_SELECT_TABLE_WHERE_EX ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_SELECT_TABLE_WHERE_EX ]
                             := T_SQLPS.SELECT_TABLE_WHERE_EX ;
       assert( FSQLCommands[ T_SQLPS.ID_INSERT_DBX_CELL_VALUE ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_INSERT_DBX_CELL_VALUE ]
                             := T_SQLPS.INSERT_DBX_CELL_VALUE ;
       assert( FSQLCommands[ T_SQLPS.ID_INSERT_DBX_CELL_PARAM ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_INSERT_DBX_CELL_PARAM ]
                             := T_SQLPS.INSERT_DBX_CELL_PARAM ;
       assert( FSQLCommands[ T_SQLPS.ID_INSERT_DBX            ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_INSERT_DBX            ]
                             := T_SQLPS.INSERT_DBX            ;
       assert( FSQLCommands[ T_SQLPS.ID_SELECT_DB_AND_USER    ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_SELECT_DB_AND_USER    ]
                             := T_SQLPS.SELECT_DB_AND_USER    ;
       assert( FSQLCommands[ T_SQLPS.ID_UPDATE_DBX_CELL       ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_UPDATE_DBX_CELL       ]
                             := T_SQLPS.UPDATE_DBX_CELL       ;
       assert( FSQLCommands[ T_SQLPS.ID_UPDATE_CELL           ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_UPDATE_CELL           ]
                             := T_SQLPS.UPDATE_CELL           ;
       assert( FSQLCommands[ T_SQLPS.ID_UPDATE_DBX_ORA        ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_UPDATE_DBX_ORA        ]
                             := T_SQLPS.UPDATE_DBX_ORA        ;
       assert( FSQLCommands[ T_SQLPS.ID_UPDATE_DBX            ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_UPDATE_DBX            ]
                             := T_SQLPS.UPDATE_DBX            ;
       assert( FSQLCommands[ T_SQLPS.ID_FILTER_CELL           ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_FILTER_CELL           ]
                             := T_SQLPS.FILTER_CELL           ;
       assert( FSQLCommands[ T_SQLPS.ID_MASTERLAYER           ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_MASTERLAYER           ]
                             := T_SQLPS.MASTERLAYER           ;
       assert( FSQLCommands[ T_SQLPS.ID_NAME                  ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_NAME                  ]
                             := T_SQLPS.NAME                  ;
       assert( FSQLCommands[ T_SQLPS.ID_XMIN                  ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_XMIN                  ]
                             := T_SQLPS.XMIN                  ;
       assert( FSQLCommands[ T_SQLPS.ID_XMAX                  ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_XMAX                  ]
                             := T_SQLPS.XMAX                  ;
       assert( FSQLCommands[ T_SQLPS.ID_YMIN                  ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_YMIN                  ]
                             := T_SQLPS.YMIN                  ;
       assert( FSQLCommands[ T_SQLPS.ID_YMAX                  ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_YMAX                  ]
                             := T_SQLPS.YMAX                  ;
       assert( FSQLCommands[ T_SQLPS.ID_PIXELSIZE             ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_PIXELSIZE             ]
                             := T_SQLPS.PIXELSIZE             ;
       assert( FSQLCommands[ T_SQLPS.ID_IMAGETYPE             ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_IMAGETYPE             ]
                             := T_SQLPS.IMAGETYPE             ;
       assert( FSQLCommands[ T_SQLPS.ID_COMPRESS              ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_COMPRESS              ]
                             := T_SQLPS.COMPRESS              ;
       assert( FSQLCommands[ T_SQLPS.ID_LEVEL                 ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_LEVEL                 ]
                             := T_SQLPS.LEVEL                 ;
       assert( FSQLCommands[ T_SQLPS.ID_WKT                   ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_WKT                   ]
                             := T_SQLPS.WKT                   ;
       assert( FSQLCommands[ T_SQLPS.ID_COL                   ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_COL                   ]
                             := T_SQLPS.COL                   ;
       assert( FSQLCommands[ T_SQLPS.ID_ROW                   ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_ROW                   ]
                             := T_SQLPS.ROW                   ;
       assert( FSQLCommands[ T_SQLPS.ID_CELL                  ] = '' ) ;
               FSQLCommands[ T_SQLPS.ID_CELL                  ]
                             := T_SQLPS.CELL                  ;

    lst := TStringList.Create ;
    try
      lst.AddStrings( FSQLParameters  ) ;
      lst.AddStrings( FSQLDialectList ) ;

      for i := 0 to FSQLCommands.Count - 1 do begin
        FSQLCommands[i] := TemplateProducer( FSQLCommands[i], lst, nil, False ) ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;

    if assigned( FOnAfterDialectChange ) then
      {$IFDEF CLR}
        FOnAfterDialectChange( Self, EventArgs.Create ) ;
      {$ELSE}
        FOnAfterDialectChange( Self ) ;
      {$ENDIF}
  end ;

  function TGIS_FilePixelStoreAbstract.getCmd(
    const _id : Integer
  ) : String ;
  begin
    assert( _id < FSQLCommands.Count, 'no such command' ) ;
    Result := FSQLCommands[ _id ] ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.prepareCapabilities  ;
  begin
    inherited ;

    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.RGB, False, TGIS_PixelSubFormat.JPEG,
                        TGIS_CompressionType.JPEG,
                        90
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.Bit8, True , TGIS_PixelSubFormat.JPEG,
                        TGIS_CompressionType.JPEG,
                        90
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.RGB, False, TGIS_PixelSubFormat.PNG,
                        TGIS_CompressionType.PNG,
                        0
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.ARGB, False, TGIS_PixelSubFormat.PNG,
                        TGIS_CompressionType.PNG,
                        0
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.Bit8, False, TGIS_PixelSubFormat.PNG,
                        TGIS_CompressionType.PNG,
                        0
                      )
                    ) ;
   Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                       TGIS_PixelFormat.Custom, False, TGIS_PixelSubFormat.GRID,
                       TGIS_CompressionType.ZLIB,
                       0
                     )
                   ) ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.parseConfigLayerName ;
  var
    tkn : TGIS_Tokenizer ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( FTable, ';', ' ' ) ;
      if ( ( tkn.Result.Count = 0 ) or IsStringEmpty( tkn.Result.Strings[ 0 ] ) ) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                     GIS_INI_LAYERSQL_LAYER,
                                     0
                                   ) ;
      case tkn.Result.Count of
        1 : begin
              strName    := tkn.Result[0] ;
              strSchema  := ''            ;
              strCatalog := ''            ;
            end ;
        2 : begin
              strName    := tkn.Result[0] ;
              strSchema  := tkn.Result[1] ;
              strCatalog := ''            ;
            end ;
        3 : begin
              strName    := tkn.Result[0] ;
              strSchema  := tkn.Result[1] ;
              strCatalog := tkn.Result[2] ;
            end ;
      end ;

      if IsStringEmpty( strSchema ) then
        strSchema := 'public' ;

      if IsStringEmpty( strCatalog ) or IsStringEmpty( strSchema ) then begin
        oGisDb.sqlQueryOpen( getCmd( T_SQLPS.ID_SELECT_DB_AND_USER ), 0 ) ;
        try
          if IsStringEmpty( strCatalog ) then
            strCatalog := UpperCase( VarToString( oGisDb.sqlQueryGetFieldById(0, 0) ) ) ;

          if IsStringEmpty( strSchema ) then
            strSchema := UpperCase( VarToString( oGisDb.sqlQueryGetFieldById(1, 0) ) ) ;
        finally
          oGisDb.sqlQueryClose(0) ;
        end ;
      end ;
    finally
      if not IsStringEmpty( strName ) then
        FTable := strName ;
      FreeObject( tkn ) ;
    end ;
  end ;


  {@@ Get schema text.
  }
  function TGIS_FilePixelStoreAbstract.getSchemaPrefix : String ;
  begin
    if IsStringEmpty( strSchema ) then
      Result := ''
    else
      Result := strSchema + '.' ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.macroConnect ;
  var
    i    : Integer     ;
    lst  : TStringList ;
    tmp  : String      ;
  begin
    oGisDb.InitializeProvider ;

    lst := TStringList.Create ;
    try
      ReadSQLParamsFromPath( Path, lst ) ;

      for i := 0 to lst.Count - 1 do begin
        tmp := lst.Names[ i ] ;
        if IsStringEmpty( FSQLParameters.Values[ tmp ] ) then
        FSQLParameters.Values[ tmp ] := lst.Values[ tmp ] ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;

    // resolve any ID/Password tokens
    {$IFNDEF OXYGENE}
      FSQLParameters.Text := TemplateProducer( FSQLParameters.Text, nil,
                                               passwordCallBack, False
                                             ) ;
    {$ELSE}
      FSQLParameters.Text := TemplateProducer( FSQLParameters.Text, nil,
                                               @passwordCallBack, False
                                             ) ;
    {$ENDIF}

    FSQLDialectList.Text := GetSQLDialect(
                               UpperCase(
                                 FSQLParameters.Values[
                                    GIS_INI_LAYERSQL_DIALECT
                                  ]
                               )
                             ) ;
    oGisDb.CurrentSQLDialect := UpperCase( FSQLParameters.Values
                                           [ GIS_INI_LAYERSQL_DIALECT ] ) ;

    FTable         := FSQLParameters.Values[ GIS_INI_LAYERSQL_LAYER    ] ;
    isReadOnly     := ParamBoolean(
                        FSQLParameters.Values[ GIS_INI_LAYERSQL_READONLY ],
                        False
                      ) ;

    if oGisDb.IsPostgreSql then begin
      updateDialectList( 'NLEVEL', 'nlevel' ) ;
      updateDialectList( 'NROW'  , 'nrow'   ) ;
      updateDialectList( 'NCOL'  , 'ncol'   ) ;
      updateDialectList( 'CELL'  , 'cell'   ) ;
    end ;

    if FSQLParameters.Values[GIS_INI_LAYERSQL_CONNECTOR_FIREDAC] = 'Ora' then begin
      FSQLDialectList.Values[':WKB_GEOMETRY'] := '' ;
      FSQLDialectList.Values[':GEOMETRY'] := '' ;
      FSQLDialectList.Values[':CELL'] := '' ;
    end ;

    if FSQLCommands.Count = 0 then
      prepareCommandList ;

    // read options
    oGisDb.sqlInitialize( FSQLParameters, FSQLDialectList );

    try
      if not assigned( FGIS_Layer ) then
        oGisDb.sqlConnect(  GetFileDir( Path ), FSQLParameters )
      else
        oGisDb.sqlConnect( oGisDb.sqlBaseFolder(FGIS_Layer), FSQLParameters ) ;

      if oGisDb.IsPostgreSql then
        parseConfigLayerName ;

      // connection is OK so passwords are OK as well.
      GisPasswordList.Merge( tmpPasswords );
    except
      on e : Exception do begin
        if IsStringEmpty( Path ) then
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_LAYERSQLERROR ),
                  Format( '%s; %s', [ Table, e.Message ] ),
                  0
                )
        else
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_LAYERSQLERROR ),
                  Format( '%s; %s', [ GetSafeSQLPath( Path ), e.Message ] ),
                  0
                ) ;
      end ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.macroDisconnect ;
  var
    i : Integer ;
  begin
    // guarantee connection closing
    for i := 0 to BUILTIN_CURSORS - 1 do
      oGisDb.sqlQueryClose(i) ;
    oGisDb.sqlTableClose( 0 ) ;
    oGisDb.sqlTableClose( 1 ) ;
    oGisDb.sqlDisconnect ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.macroMasterCreate ;
  begin
    if isReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try
      oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_CREATE_MASTER ), [getSchemaPrefix] ) ) ;
      oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_CREATE_INDEX_NAME ), [getSchemaPrefix] ) ) ;
    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.macroTableCreate(
    const _extent      : TGIS_Extent          ;
    const _type        : TGIS_PixelStoreType ;
    const _compression : Integer              ;
    const _pixelsize   : Double               ;
    const _srtext      : String
  ) ;
  begin
    if isReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try

      oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_CREATE_TABLE_CELL    ),
                       [ Table ]
                     )
             ) ;
      oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_CREATE_INDEX_PK ),
                       [ FTable, Table ]
                     )
             ) ;

      oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_CREATE_INDEX_LEVEL ),
                       [ FTable, Table ]
                     )
             ) ;
      oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_CREATE_INDEX_ROW ),
                       [ FTable, Table ]
                     )
             ) ;
      oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_CREATE_INDEX_COL ),
                       [ FTable, Table ]
                     )
             ) ;

      oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_INSERT_MASTER   ),
                       [ getSchemaPrefix, Table,
                         DotFloatToStr(_extent.XMin),
                         DotFloatToStr(_extent.XMax),
                         DotFloatToStr(_extent.YMin),
                         DotFloatToStr(_extent.YMax),
                         DotFloatToStr(_pixelsize  ),
                         ord( _type ),
                         _compression,
                         1,
                         _srtext
                       ]
                     )
              ) ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.macroTableDrop ;
  begin
    if isReadOnly then exit ;

    oGisDb.sqlTransactRestructStart ;
    try

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_DROP_TABLE   ), [ Table  ] ) ) ;
      except
        // can no exist
      end ;

      try
        oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_DELETE_TABLE ), [ getSchemaPrefix, Table ] ) )
      except
        // can no exist
      end ;

    finally
      oGisDb.sqlTransactRestructCommit ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.Open ;
  var
    v  : Variant ;
    it : Integer ;
  begin
    macroConnect ;

    oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPS.ID_SELECT_MASTER ),
                                [getSchemaPrefix, Table]
                                ),
                                0
                        ) ;
    if oGisDb.sqlQueryEof(0) then exit ;
    try
      {$IFDEF GIS_NORECORDS}
      FExtent := new TGIS_Extent() ;
      {$ENDIF}
      FExtent.XMin := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPS.ID_XMIN        ),0 ) ) ;
      FExtent.XMax := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPS.ID_XMAX        ),0 ) ) ;
      FExtent.YMin := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPS.ID_YMIN        ),0 ) ) ;
      FExtent.YMax := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPS.ID_YMAX        ),0 ) ) ;
      FPixelSize   := VarToDouble( oGisDb.sqlQueryGetField( getCmd( T_SQLPS.ID_PIXELSIZE   ),0 ) ) ;
      FCompressionLevel := VarToInt32(  oGisDb.sqlQueryGetField( getCmd( T_SQLPS.ID_COMPRESS    ),0 ) ) ;

      it := VarToInt32( oGisDb.sqlQueryGetField( getCmd( T_SQLPS.ID_IMAGETYPE ),0 ) )  ;
      case it of
        0 : FImageType := TGIS_PixelStoreType.BMP;
        1 : FImageType := TGIS_PixelStoreType.JPEG24;
        2 : FImageType := TGIS_PixelStoreType.JPEG8;
        3 : FImageType := TGIS_PixelStoreType.PNG24;
        4 : FImageType := TGIS_PixelStoreType.PNG8;
        5 : FImageType := TGIS_PixelStoreType.PNG32;
        6 : FImageType := TGIS_PixelStoreType.GRID;
      end ;

      FLevel := VarToInt32(  oGisDb.sqlQueryGetField( getCmd( T_SQLPS.ID_LEVEL       ),0 ) ) ;
      v      := oGisDb.sqlQueryGetField( getCmd( T_SQLPS.ID_WKT         ),0 ) ;
      if not( VarIsNull( v ) or VarIsEmpty( v ) ) then
        FWKT := VarToString( v ) ;

      if FImageType = TGIS_PixelStoreType.GRID then
        FIsGrid := True ;
    finally
      oGisDb.sqlQueryClose(0) ;
    end ;

    oGisDb.GeometryType := TGIS_SubDataType.Native ;
  end ;

  function TGIS_FilePixelStoreAbstract.CalculateLevel(
    const _zoom : Double
  ) : Integer ;
  var
    zoom   : Double ;
    factor : Integer ;
  begin
    // calculate factor (which must be 1,2,4,6,16 etc
    if _zoom > 0 then begin
      factor := Level ;
      zoom := _zoom * PixelSize ;
    end
    else begin
      // select level bigger then current - for quad generation
      factor := Level * 2 ;
      zoom := Abs( _zoom ) ;
    end ;

    while factor > 1 do begin
      if factor <= 1/zoom then break ;
      factor := factor div 2 ;
    end ;

    Result := factor ;
  end;

  procedure TGIS_FilePixelStoreAbstract.SetScope(
    const _extent : TGIS_Extent ;
    const _zoom   : Double
  ) ;
  begin
    assert( _extent.XMax >= _extent.XMin ) ;
    assert( _extent.YMax >= _extent.YMin ) ;

    scopeEof := True ;

    if GisIsEmptyExtent( _extent ) then exit ;

    scopeFactor := CalculateLevel( _zoom ) ;
    scopeRect   := calculateScope( scopeFactor, _extent ) ;

    scopePos.X := scopeRect.Left ;
    scopePos.Y := scopeRect.Top  ;
    scopePos.X := scopePos.X - 1 ; ; // to be "before" first element

    scopeEof := False ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.MoveFirst ;
  begin
    MoveNext ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.MoveNext  ;
  begin
    scopePos.X := scopePos.X + 1 ;
    if scopePos.X >= scopeRect.Right then begin
      scopePos.X := scopeRect.Left ;
      scopePos.Y := scopePos.Y + 1 ;
    end ;

    if scopePos.Y >= scopeRect.Bottom then begin
      scopeEof := True ;
    end ;

    if not Eof and not directWrite then readCell ;
  end ;

  function TGIS_FilePixelStoreAbstract.Eof : Boolean ;
  begin
    Result := scopeEof ;
  end ;

  function TGIS_FilePixelStoreAbstract.CurrentCell : TGIS_PixelStoreCell ;
  begin
    objCell.FBitmap := objBmp  ;
    objCell.FGrid   := objGrid ;
    objCell.FLevel  := scopeFactor ;

    with objCell.FExtent do begin
      XMin := ( scopePos.X + 0 ) * CellSize * PixelSize  * scopeFactor +
              Extent.XMin ;
      XMax := ( scopePos.X + 1 ) * CellSize * PixelSize  * scopeFactor +
              Extent.XMin ;
      YMin := Extent.YMax -
              ( scopePos.Y + 1 ) * CellSize * PixelSize * scopeFactor ;
      YMax := Extent.YMax -
              ( scopePos.Y + 0 ) * CellSize * PixelSize * scopeFactor ;
    end ;

    objCell.FProgress := ( 1.0 *
                           ( Abs(scopeRect.Right  - scopeRect.Left   + 1 ) *
                             Abs(scopeRect.Bottom - scopePos.Y           ) +
                             Abs(scopePos.X       - scopeRect.Left   + 1 )
                           )
                         ) /
                         ( Abs(scopeRect.Right  - scopeRect.Left   + 1 ) *
                           Abs(scopeRect.Bottom - scopeRect.Top    + 1 )
                         ) ;

    Result := objCell ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.WriteCell ;
  var
    blob : TGIS_MemoryStream ;
    cmpr : TCustomZStream ;
  begin
    if isReadOnly then exit ;

    if not directWrite then
      oGisDb.sqlQueryOpen( prepareSelectCommand( Table,
                         prepareFilterCell( scopeFactor, scopePos.X, scopePos.Y ) )
                   ,0);

    oGisDb.sqlTransactUpdateStart;
    try
      if directWrite or oGisDb.sqlQueryEof(0) then begin
        sqlTableAppend( Table ) ;
        oGisDb.sqlTableSetField( 0, getCmd( T_SQLPS.ID_LEVEL ), scopeFactor, 1 ) ;
        oGisDb.sqlTableSetField( 0, getCmd( T_SQLPS.ID_COL   ), scopePos.X,  1 ) ;
        oGisDb.sqlTableSetField( 0, getCmd( T_SQLPS.ID_ROW   ), scopePos.Y,  1 ) ;
      end
      else begin
        sqlTableOpenWrite( Table,
                     prepareFilterCell( scopeFactor, scopePos.X, scopePos.Y )
                   ) ;
        oGisDb.sqlTableSetField( 0, getCmd( T_SQLPS.ID_LEVEL ), scopeFactor, 1 ) ;
        oGisDb.sqlTableSetField( 0, getCmd( T_SQLPS.ID_COL   ), scopePos.X,  1 ) ;
        oGisDb.sqlTableSetField( 0, getCmd( T_SQLPS.ID_ROW   ), scopePos.Y,  1 ) ;
      end ;

      blob := TGIS_MemoryStream.Create ;
      try
        case ImageType of

          TGIS_PixelStoreType.BMP    :
            begin
              objBmp.SaveToStream( blob, FSubFormat.PixelFormat, TGIS_PixelSubFormat.BMP, CompressionLevel ) ;
            end ;

          TGIS_PixelStoreType.JPEG24 ,
          TGIS_PixelStoreType.JPEG8  :
            begin
              objBmp.SaveToStream( blob, FSubFormat.PixelFormat, TGIS_PixelSubFormat.JPEG, CompressionLevel ) ;
            end ;

          TGIS_PixelStoreType.PNG24  ,
          TGIS_PixelStoreType.PNG8   ,
          TGIS_PixelStoreType.PNG32  :
            begin
              objBmp.SaveToStream( blob, FSubFormat.PixelFormat, TGIS_PixelSubFormat.PNG, CompressionLevel ) ;
            end ;

          TGIS_PixelStoreType.GRID :
            begin
              cmpr := TZCompressionStream.Create( blob ) ;
              try
                objGrid.SaveToStream( cmpr ) ;
              finally
                FreeObject( cmpr ) ;
              end ;
            end ;
          else
            assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
        end ;
      finally
        T_lruList( lruList ).Update( scopeFactor, scopePos ) ;
        if blob.Size > 0 then
          sqlTableSetCell( getCmd( T_SQLPS.ID_CELL ), blob ) ;

        FreeObject( blob ) ;
      end ;

    finally
      oGisDb.sqlTablePost( 0 )       ;
      oGisDb.sqlTransactUpdateCommit ;
      if not oGisDb.InBatchMode then
        oGisDb.sqlTableClose( 0 ) ;
      if not directWrite then
        oGisDb.sqlQueryClose( 0 )      ;
    end ;

    updateFactor ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.AddCell(
    const _bmp    : TGIS_Bitmap;
    const _extent : TGIS_Extent;
    const _scale  : Double
  ) ;
  begin
    if isReadOnly then exit ;

    SetScope( _extent, _scale ) ;
    MoveFirst ;
    if not Eof then begin
      if ( CurrentCell.Bitmap.Width  <> _bmp.Width  ) or
         ( CurrentCell.Bitmap.Height <> _bmp.Height ) then
         raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_PIXELSTORE_WRONGSOURCE ), '', 0 ) ;
      CurrentCell.Bitmap.Assign( _bmp ) ;
      WriteCell ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.AddCellEx(
    const _bmp    : TGIS_Bitmap;
    const _extent : TGIS_Extent;
    const _scale  : Double
  ) ;
  var
    zoom : Double      ;
  begin
    if isReadOnly then exit ;

    assert( _extent.XMax >= _extent.XMin ) ;
    assert( _extent.YMax >= _extent.YMin ) ;

    zoom := _scale ;

    scopeEof := True ;

    if GisIsEmptyExtent( _extent ) then exit ;

    scopeFactor := Level ;

    scopeRect := calculateInsertTile( scopeFactor, _extent ) ;

    scopePos.X := scopeRect.Left ;
    scopePos.Y := scopeRect.Top  ;
    scopePos.X := scopePos.X - 1 ; ; // to be "before" first element

    scopeEof := False ;

    MoveFirst ;
    if not Eof then begin
      if ( CurrentCell.Bitmap.Width  <> _bmp.Width  ) or
         ( CurrentCell.Bitmap.Height <> _bmp.Height ) then
         raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_PIXELSTORE_WRONGSOURCE ), '', 0 ) ;
      CurrentCell.Bitmap.Assign( _bmp ) ;
      WriteCell ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.AddCellGrid(
    const _grd    : TGIS_GridArray ;
    const _extent : TGIS_Extent    ;
    const _scale  : Double
  ) ;
  begin
    if isReadOnly then exit ;

    assert( _extent.XMax >= _extent.XMin ) ;
    assert( _extent.YMax >= _extent.YMin ) ;

    scopeEof := True ;

    if GisIsEmptyExtent( _extent ) then exit ;

    scopeFactor := Level ;

    scopeRect := calculateInsertTile( scopeFactor, _extent ) ;

    scopePos.X := scopeRect.Left ;
    scopePos.Y := scopeRect.Top  ;
    scopePos.X := scopePos.X - 1 ; ; // to be "before" first element

    scopeEof := False ;

    MoveFirst ;
    if not Eof then begin
      if ( CurrentCell.Grid.Width  <> length( _grd    ) ) or
         ( CurrentCell.Grid.Height <> length( _grd[0] ) ) then
         raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_PIXELSTORE_WRONGSOURCE ), '', 0 ) ;
      CurrentCell.Grid.Assign( _grd ) ;
      WriteCell ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.Write(
     const _x       : Integer ;
     const _y       : Integer ;
     const _pixels  : TGIS_Pixels ;
     const _pformat : TGIS_PixelFormat ;
     const _width   : Integer ;
     const _height  : Integer
  ) ;
  var
    ext   : TGIS_Extent ;
    bmp   : TGIS_Bitmap ;
    plock : TGIS_Pixels ;
  begin
    if isReadOnly then exit ;

    {$IFDEF GIS_NORECORDS}
    ext := new TGIS_Extent() ;
    {$ENDIF}
    ext.XMin := Extent.XMin + FPixelSize * _x               ;
    ext.XMax := ext.XMin    + FPixelSize * CellSize         ;
    ext.YMin := Extent.YMax - FPixelSize * (_y  + CellSize) ;
    ext.YMax := ext.YMin    + FPixelSize * CellSize         ;

    bmp := TGIS_Bitmap.Create( _width, _height ) ;
    try
      bmp.LockPixels( plock, True ) ;
      GisCopyPixels( _pixels, 0, plock, 0, _width*_height ) ;
      bmp.UnlockPixels ;
      AddCell( bmp, ext, 1 ) ;
    finally
      FreeObject( bmp ) ;
    end ;
  end ;

 procedure TGIS_FilePixelStoreAbstract.Write(
  const _ext      : TGIS_Extent ;
  const _level    : Integer ;
  const _pixels   : TGIS_Pixels ;
  const _width    : Integer ;
  const _height   : Integer
 ) ;
  var
    bmp     : TGIS_Bitmap ;
    plock   : TGIS_Pixels ;
  begin
    if isReadOnly then exit ;

    bmp := TGIS_Bitmap.Create( _width, _height ) ;
    try
      bmp.LockPixels( plock, True ) ;
      {$IFDEF OXYGENE}
        GisCopyPixels( _pixels, 0, plock, 0, _width*_height ) ;
      {$ELSE}
        System.Move( _pixels[0], plock[0], _width*_height*4 ) ;
      {$ENDIF}
      bmp.UnlockPixels ;

      FLevel := TruncS( Power( 2, _level ) ) ;

      AddCellEx( bmp, _ext, 1 ) ;
    finally
      FreeObject( bmp ) ;
    end ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.WriteGrid(
    const _x   : Integer ;
    const _y   : Integer ;
    const _grd : TGIS_GridArray
  ) ;
  var
    ext : TGIS_Extent ;
  begin
    if isReadOnly then exit ;

    {$IFDEF GIS_NORECORDS}
    ext := new TGIS_Extent() ;
    {$ENDIF}
    ext.XMin := Extent.XMin + FPixelSize * _x               ;
    ext.XMax := ext.XMin    + FPixelSize * CellSize         ;
    ext.YMin := Extent.YMax - FPixelSize * (_y  + CellSize) ;
    ext.YMax := ext.YMin    + FPixelSize * CellSize         ;

    AddCellGrid( _grd, ext, 1 ) ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.WriteGrid(
    const _ext     : TGIS_Extent ;
    const _x       : Integer ;
    const _y       : Integer ;
    const _level   : Integer ;
    const _grd     : TGIS_GridArray
  ) ;
  var
    ext : TGIS_Extent ;
    vext : TGIS_Extent ;
    dx, dy : Integer ;
    xtiles, ytiles : Integer ;
  begin
    if isReadOnly then exit ;

    FLevel := TruncS( Power( 2, _level ) ) ;

    dx := TruncS((Extent.XMax-Extent.XMin)/(FPixelSize*FLevel)) ;
    dy := TruncS((Extent.YMax-Extent.YMin)/(FPixelSize*FLevel)) ;

    xtiles := dx div 512 ;
    if ( dx mod 512 ) > 0 then
      xtiles := xtiles + 1 ;

    ytiles := dy div 512 ;
    if ( dy mod 512 ) > 0 then
      ytiles := ytiles + 1 ;

    {$IFDEF GIS_NORECORDS}
    vext := new TGIS_Extent() ;
    {$ENDIF}
    vext.XMin := Extent.XMin ;
    vext.XMax := vext.XMin + FPixelSize * CellSize * xtiles *FLevel ;
    vext.YMax := Extent.YMax ;
    vext.YMin := vext.YMax - FPixelSize * CellSize * ytiles *FLevel ;

    {$IFDEF GIS_NORECORDS}
    ext := new TGIS_Extent() ;
    {$ENDIF}
    ext.XMin := vext.XMin + FPixelSize * _x * FLevel              ;
    ext.XMax := ext.XMin  + FPixelSize * CellSize *FLevel         ;
    ext.YMin := vext.YMax - FPixelSize * (_y  + CellSize)* FLevel ;
    ext.YMax := ext.YMin  + FPixelSize * CellSize * FLevel        ;

    AddCellGrid( _grd, ext, 1 ) ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.InitializeWrite ;
  begin
  { TODO : start global transaction, batch mode and direct write mode (no tile check) }
    oGisDb.sqlTransactGlobalUpdateStart ;
    oGisDb.InBatchMode := True ;
    directWrite := True ;
  end ;

  procedure TGIS_FilePixelStoreAbstract.FinalizeWrite ;
  begin
  { TODO : End global transaction, batch mode and direct write mode (no tile check) }
    oGisDb.sqlTransactGlobalUpdateCommit ;
    oGisDb.sqlTableClose( 0 )      ;
    oGisDb.InBatchMode := False ;
    directWrite := False ;
    oGisDb.sqlExec( Format( getCmd( T_SQLPS.ID_UPDATE_MASTER ), [ getSchemaPrefix, FMaxLevel, Table ] ) ) ;
  end ;

  function TGIS_FilePixelStoreAbstract.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    lname : String  ;
  begin
    Result := TGIS_LayerInfoList.Create ;

    try
      macroConnect ;
      try
        try
          oGisDb.sqlQueryOpen( Format( getCmd( T_SQLPS.ID_SELECT_MASTER_ALL ),
                                      [getSchemaPrefix]), 0
                              ) ;
          while not oGisDb.sqlQueryEof(0) do begin
            lname := VarToString( oGisDb.sqlQueryGetFieldById( 0, 0 ) ) ;
            Result.Add(
              TGIS_LayerInfo.Create( lname,
                                     TGIS_RegisteredLayerType.Pixel,
                                     TGIS_ShapeType.Unknown
                                    )
            ) ;

            oGisDb.sqlQueryMoveNext(0) ;
          end ;
        except
          // can not exist
        end ;
      finally
        oGisDb.sqlQueryClose(0) ;
        macroDisconnect ;
      end ;
    except
      // wrong connection
      on E : EGIS_Exception do
        TGIS_Logger.AsError( GetClassName(Self), E.Message ) ;
    end ;
  end ;

//==================================== END =====================================
end.
