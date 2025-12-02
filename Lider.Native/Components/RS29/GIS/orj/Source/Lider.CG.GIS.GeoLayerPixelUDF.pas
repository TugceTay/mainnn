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
  Encapsulation of a UDF Pixel layer.
}

{$IFDEF DCC}
  unit GisLayerPixelUDF ;
  {$HPPEMIT '#pragma link "GisLayerPixelUDF"'}
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

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Variants,
    System.Types,

    GisTypes,
    GisTypesUI,
    GisClasses,
    GisFunctions,
    GisLayerPixel ;
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
  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data to the SetUp event.
    /// </summary>
    TGIS_SetUpEventArgs = public class ( EventArgs )
      private
        FBitHeight   : Integer ;
        FBitWidth    : Integer ;
        FIsGridImage : Boolean ;
        FMinZ        : Single ;
        FMaxZ        : Single ;
        FNoDataValue : Single ;
        FExtent      : TGIS_Extent ;

      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        constructor Create  ;

      public

        /// <summary>
        ///   Height of a layer in pixels.
        /// </summary>
        property BitHeight   : Integer     read  FBitHeight
                                           write FBitHeight   ;

        /// <summary>
        ///   Width of a layer in pixels.
        /// </summary>
        property BitWidth    : Integer     read  FBitWidth
                                           write FBitWidth    ;

        /// <summary>
        ///   Is layer a grid type.
        /// </summary>
        property IsGridImage : Boolean     read  FIsGridImage
                                           write FIsGridImage ;

        /// <summary>
        ///   Minimum Z value (used in rendering).
        /// </summary>
        property MinZ        : Single      read  FMinZ
                                           write FMinZ        ;

        /// <summary>
        ///   Maximum Z value (used in rendering).
        /// </summary>
        property MaxZ        : Single      read  FMaxZ
                                           write FMaxZ        ;

        /// <summary>
        ///   Value specified as NoData.
        /// </summary>
        property NoDataValue : Single      read  FNoDataValue
                                           write FNoDataValue ;

        /// <summary>
        ///   Layer extent.
        /// </summary>
        property Extent      : TGIS_Extent read  FExtent
                                           write FExtent      ;
    end ;

    /// <summary>
    ///   SetUp event. Fired upon a layer initialization.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_SetUpEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure(
      _sender : TObject ;
      _e      : TGIS_SetUpEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   SetUp event. Fired upon a layer initialization.
    /// </summary>
    /// <param name="_bitHeight">
    ///   height of a layer in pixels.
    /// </param>
    /// <param name="_bitWidth">
    ///   Width of a layer in pixels.
    /// </param>
    /// <param name="_isGridImage">
    ///   is layer a grid type
    /// </param>
    /// <param name="_minZ">
    ///   minimum Z value (used in rendering)
    /// </param>
    /// <param name="_maxZ">
    ///   maximum Z value (used in rendering)
    /// </param>
    /// <param name="_noDataValue">
    ///   value specified as NoData
    /// </param>
    /// <param name="_extent">
    ///   layer extent
    /// </param>  
    TGIS_SetUpEvent = procedure(
      var _bitHeight     : Integer ;
      var _bitWidth      : Integer ;
      var _isGridImage   : Boolean ;
      var _minZ          : Single ;
      var _maxZ          : Single ;
      var _noDataValue   : Single ;
      var _extent        : TGIS_Extent
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data to the GetLine event.
    /// </summary>
    TGIS_GetLineEventArgs = public class ( EventArgs )
      private
        FBuffer : TBytes  ;
        FOffset : Integer ;
        FLineNr : Integer ;
        FStart  : Integer ;
        FBytes  : Integer ;

      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_buffer">
        ///   buffer that must be filled with pixel data (24bit RGB)
        /// </param>
        /// <param name="_offset">
        ///   buffer offset
        /// </param>  
        /// <param name="_linenr">
        ///   current line number to display
        /// </param>
        /// <param name="_start">
        ///   left margin (bytes to skip)
        /// </param>
        /// <param name="_bytes">
        ///   bytes number to read
        /// </param>
        constructor Create  ( const _buffer : TBytes  ;
                              const _offset : Integer ;
                              const _linenr : Integer ;
                              const _start  : Integer ;
                              const _bytes  : Integer
                            ) ;

      public

        /// <summary>
        ///   Buffer that must be filled with pixel data (24bit RGB).
        /// </summary>
        property Buffer : TBytes  read FBuffer ;

        /// <summary>
        ///   Buffer offset.
        /// </summary>
        property Offset : Integer read FOffset ;

        /// <summary>
        ///   Current line number to display.
        /// </summary>
        property LineNr : Integer read FLineNr ;

        /// <summary>
        ///   Left margin (bytes to skip).
        /// </summary>
        property Start  : Integer read FStart  ;

        /// <summary>
        ///   Bytes number to read.
        /// </summary>
        property Bytes  : Integer read FBytes  ;
    end ;

    /// <summary>
    ///   GetLine event. Fired upon prepared layer data reading.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_GetLineEvent = {$IFDEF OXYGENE} public {$ENDIF} function(
      _sender : TObject ;
      _e      : TGIS_GetLineEventArgs
    ) : Integer of object ;
  {$ELSE}

    /// <summary>
    ///   GetLine event. Fired upon prepared layer data reading.
    /// </summary>
    /// <param name="_buffer">
    ///   buffer that must be filled with pixel data (24bit RGB)
    /// </param>
    /// <param name="_offset">
    ///   buffer offset
    /// </param>  
    /// <param name="_linenr">
    ///   current line number to display
    /// </param>
    /// <param name="_start">
    ///   left margin (bytes to skip)
    /// </param>
    /// <param name="_bytes">
    ///   bytes number to read
    /// </param>
    TGIS_GetLineEvent = function ( 
      const _buffer : TBytes  ;
      const _offset : Integer ;
      const _linenr : Integer ;
      const _start  : Integer ;
      const _bytes  : Integer
    ) : Integer of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data to the GetNativeLine event.
    /// </summary>
    TGIS_GetNativeLineEventArgs = public class ( EventArgs )
      private
        FBuffer   : TGIS_SingleArray ;
        FLineNr   : Integer          ;
        FStartIdx : Integer          ;
        FCount    : Integer          ;

      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_buffer">
        ///   predefined buffer of singles
        /// </param>
        /// <param name="_linenr">
        ///   number of needed grid line
        /// </param>
        /// <param name="_startIdx">
        ///   left margin (pixels to skip)
        /// </param>
        /// <param name="_count">
        ///   pixel count
        /// </param>
        constructor Create  ( const _buffer   : TGIS_SingleArray ;
                              const _linenr   : Integer          ;
                              const _startIdx : Integer          ;
                              const _count    : Integer
                            ) ;

      public

        /// <summary>
        ///   Predefined buffer of singles.
        /// </summary>
        property Buffer   : TGIS_SingleArray read FBuffer ;

        /// <summary>
        ///   Number of needed grid line.
        /// </summary>
        property LineNr   : Integer          read FLineNr ;

        /// <summary>
        ///   Left margin (pixels to skip).
        /// </summary>
        property StartIdx : Integer          read FStartIdx ;

        /// <summary>
        ///   Pixel count.
        /// </summary>
        property Count    : Integer          read FCount  ;
    end ;

    /// <summary>
    ///   GetNativeLine event. Fired upon real or grid layer data reading.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_GetNativeLineEvent  = public function(
                                 const _sender : TObject ;
                                 const _e      : TGIS_GetNativeLineEventArgs
                               ) : Integer of object ;
  {$ELSE}

    /// <summary>
    ///   GetNativeLine event. Fired upon real or grid layer data reading.
    /// </summary>
    /// <param name="_buffer">
    ///   predefined buffer of singles
    /// </param>
    /// <param name="_linenr">
    ///   number of needed grid line
    /// </param>
    /// <param name="_startIdx">
    ///   left margin (pixels to skip)
    /// </param>
    /// <param name="_count">
    ///   pixel count
    /// </param>
    TGIS_GetNativeLineEvent  = function( const _buffer   : TGIS_SingleArray ;
                                         const _linenr   : Integer          ;
                                         const _startIdx : Integer          ;
                                         const _count    : Integer
                                       ) : Integer of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data to the GetNativeValue event.
    /// </summary>
    TGIS_GetNativeValueEventArgs = public class ( EventArgs )
      private
        FPoint : TPoint  ;
        FArray : TGIS_DoubleArray ;

      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_point">
        ///   requested point
        /// </param>
        /// <param name="_array">
        ///   array of float values for _pt point
        /// </param>
        constructor Create  ( const _point : TPoint  ;
                              const _array : TGIS_DoubleArray
                            ) ;

      public

        /// <summary>
        ///   Requested point.
        /// </summary>
        property Point : TPoint           read FPoint ;

        /// <summary>
        ///   Array of float values for Point.
        /// </summary>
        property &Array : TGIS_DoubleArray read FArray ;
    end ;

    /// <summary>
    ///   GetNativeValue event. Fired upon reading original grid value of point.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_GetNativeValueEvent = public function(
      _sender : TObject ;
      _e      : TGIS_GetNativeValueEventArgs
    ) : Boolean of object ;
  {$ELSE}

    /// <summary>
    ///   GetNativeValue event. Fired upon reading original grid value of point.
    /// </summary>
    /// <param name="_pt">
    ///   requested point
    /// </param>
    /// <param name="_ar">
    ///   array of float values for _pt point
    /// </param>
    TGIS_GetNativeValueEvent = function( 
      const _pt     : TPoint  ;
      const _ar     : TGIS_DoubleArray
    ) : Boolean of object ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of pixel UDF layer.
  /// </summary>
  /// <remarks>
  ///   Based on this layer user can read different formats and using events
  ///   can fill the layer with proper parameters and values.
  /// </remarks>
  TGIS_LayerPixelUDF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private
      FOnSetUpEvent           : TGIS_SetUpEvent ;
      FOnGetLineEvent         : TGIS_GetLineEvent ;
      FOnGetNativeLineEvent   : TGIS_GetNativeLineEvent ;
      FOnGetNativeValueEvent  : TGIS_GetNativeValueEvent ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <inheritdoc/>
      procedure setUp    ; override;
      
      /// <inheritdoc/>
      function  getLine      ( const _buffer   : TBytes  ;
                               const _offset   : Integer ;
                               const _linenr   : Integer ;
                               const _start    : Integer ;
                               const _bytes    : Integer
                              ) : Integer; override;

      /// <inheritdoc/>
      function  getNativeValue ( const _pt       : TPoint  ;
                                 const _ar       : TGIS_DoubleArray
                               ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine  ( const _buffer   : TGIS_SingleArray ;
                                 const _linenr   : Integer          ;
                                 const _startIdx : Integer          ;
                                 const _count    : Integer
                               ) : Integer ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;

    public // various public routines

      /// <inheritdoc/>
      constructor Create  ; override;

    published // events

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   SetUp event. Fired upon a layer initialization.
        /// </summary>
        event SetUpEvent             : TGIS_SetUpEvent
                                                delegate FOnSetUpEvent ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   SetUp event. Fired upon a layer initialization.
        /// </summary>
        property SetUpEvent          : TGIS_SetUpEvent
                                                read  FOnSetUpEvent
                                                write FOnSetUpEvent ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   GetLine event. Fired upon prepared layer data reading.
        /// </summary>
        event GetLineEvent           : TGIS_GetLineEvent
                                                delegate FOnGetLineEvent ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   GetLine event. Fired upon prepared layer data reading.
        /// </summary>
        property GetLineEvent        : TGIS_GetLineEvent
                                                read  FOnGetLineEvent
                                                write FOnGetLineEvent ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   GetNativeLine event. Fired upon real or grid layer data reading.
        /// </summary>
        event GetNativeLineEvent     : TGIS_GetNativeLineEvent
                                                delegate FOnGetNativeLineEvent ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   GetNativeLine event. Fired upon real or grid layer data reading.
        /// </summary>
        property GetNativeLineEvent  : TGIS_GetNativeLineEvent
                                                read  FOnGetNativeLineEvent
                                                write FOnGetNativeLineEvent ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   GetNativeValue event. OnGetNativeValue event. Fired upon reading
        ///   original grid value of point.
        /// </summary>
        event GetNativeValueEvent : TGIS_GetNativeValueEvent
                                                delegate FOnGetNativeValueEvent ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   GetNativeValue event. OnGetNativeValue event. Fired upon reading
        ///   original grid value of point.
        /// </summary>
        property GetNativeValueEvent : TGIS_GetNativeValueEvent
                                                read  FOnGetNativeValueEvent
                                                write FOnGetNativeValueEvent ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Math,
    
    GisResource ;
{$ENDIF}

//==============================================================================
// TGIS_SetUpEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_SetUpEventArgs.Create ;
    begin
      inherited ;
      FBitHeight   := 0 ;
      FBitWidth    := 0 ;
      FIsGridImage := false ;
      FMinZ        := 0 ;
      FMaxZ        := 0 ;
      FNoDataValue := 0 ;
      FExtent      := GisNoWorld ;
    end ;
  {$ENDIF}

//==============================================================================
// TGIS_GetLineEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_GetLineEventArgs.Create(
      const _buffer : TBytes  ;
      const _offset : Integer ;
      const _linenr : Integer ;
      const _start  : Integer ;
      const _bytes  : Integer
    ) ;
    begin
      inherited Create ;
      FBuffer := _buffer ;
      FOffset := _offset ;
      FLineNr := _linenr ;
      FStart  := _start  ;
      FBytes  := _bytes  ;
    end ;
  {$ENDIF}

//==============================================================================
// TGIS_GetNativeLineEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_GetNativeLineEventArgs.Create(
      const _buffer   : TGIS_SingleArray ;
      const _linenr   : Integer          ;
      const _startIdx : Integer          ;
      const _count    : Integer
    ) ;
    begin
      inherited Create ;
      FBuffer   := _buffer ;
      FLineNr   := _linenr ;
      FStartIdx := _startIdx ;
      FCount    := _count  ;
    end ;
  {$ENDIF}

//==============================================================================
// TGIS_GetNativeValueEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_GetNativeValueEventArgs.Create(
      const _point : TPoint ;
      const _array : TGIS_DoubleArray
    ) ;
    begin
      inherited Create ;
      FPoint := _point ;
      FArray := _array ;
    end ;
  {$ENDIF}

//==============================================================================
// TGIS_LayerPixelUDF
//==============================================================================

  constructor TGIS_LayerPixelUDF.Create ;
  begin
    inherited ;

    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;

    useAltitudeZones := False ;
  end ;

  procedure TGIS_LayerPixelUDF.doDestroy ;
  begin

    inherited;
  end;

  procedure TGIS_LayerPixelUDF.setUp ;
  var
    ext : TGIS_Extent ;
    {$IFDEF OXYGENE}
       e : TGIS_SetUpEventArgs ;
    {$ELSE}
      minz, maxz : Single ;
    {$ENDIF}
  begin
    try
      if assigned( FOnSetUpEvent ) then
        {$IFDEF OXYGENE}
          begin
            e := TGIS_SetUpEventArgs.Create ;
            try
              FOnSetUpEvent( Self, e ) ;
              FBitHeight := e.BitHeight ;
              FBitWidth := e.BitWidth ;
              FIsGridImage := e.IsGridImage ;
              if FMinZ = FMaxZ  then begin
                FMinZ := e.MinZ ;
                FMaxZ := e.MaxZ ;
              end ;
              Params.Pixel.GridNoValue := e.NoDataValue ;
              FNoDataValue := Params.Pixel.GridNoValue ;
              ext := e.Extent ;
            finally
              FreeObject( e ) ;
            end ;
          end
        {$ELSE}
          begin
            FOnSetUpEvent( FBitHeight,
                           FBitWidth,
                           FIsGridImage,
                           minz,
                           maxz,
                           FNoDataValue,
                           ext
                          ) ;
            if FMinZ = FMaxZ  then begin
              FMinZ := minz ;
              FMaxZ := maxz ;
            end ;
          end
        {$ENDIF}
      else begin
        FBitHeight    := 100 ;
        FBitWidth     := 100 ;
        FIsGridImage  := False ;
        ext           := GisExtent( 0, 0, 100, 100 ) ;
      end;

      realBitCount  := 24 ;
      realLineWidth := ( FBitWidth*realBitCount +7 ) div 8 ;

      intLineWidth := realLineWidth ;
      Params.Pixel.GridNoValue := FNoDataValue ;


      redTransp[0]   := BASE_TRANSPARENT_FLAG ;
      greenTransp[0] := BASE_TRANSPARENT_FLAG ;
      blueTransp[0]  := BASE_TRANSPARENT_FLAG ;

      Extent := ext ;

      inherited ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;

  end;

  function TGIS_LayerPixelUDF.getLine( const _buffer : TBytes  ;
                                       const _offset : Integer ;
                                       const _linenr : Integer ;
                                       const _start  : Integer ;
                                       const _bytes  : Integer
                                      ) : Integer;

 begin
   try
    if assigned( FOnGetLineEvent ) then
      {$IFDEF OXYGENE}
        Result := FOnGetLineEvent( Self,
                                   TGIS_GetLineEventArgs.Create(
                                     _buffer, _offset, _linenr, _start, _bytes
                                   )
                                 )
      {$ELSE}
        Result := FOnGetLineEvent( _buffer, _offset, _linenr, _start, _bytes )
      {$ENDIF}
    else
      Result := 0 ;
   except
     raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
   end ;
  end;

  function TGIS_LayerPixelUDF.getNativeValue( const _pt : TPoint  ;
                                              const _ar : TGIS_DoubleArray
                                              ) : Boolean ;
  begin
    try
      Result := True ;

      if assigned( FOnGetNativeValueEvent ) then
        {$IFDEF OXYGENE}
          Result := FOnGetNativeValueEvent( Self,
                                            TGIS_GetNativeValueEventArgs.Create(
                                              _pt, _ar
                                            )
                                          ) ;
        {$ELSE}
          Result := FOnGetNativeValueEvent( _pt, _ar ) ;
        {$ENDIF}
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerPixelUDF.getNativeLine( const _buffer   : TGIS_SingleArray ;
                                             const _linenr   : Integer          ;
                                             const _startIdx : Integer          ;
                                             const _count    : Integer
                                            ) : Integer ;
  begin
    try
      Result := 0 ;

      if (_linenr < 0) or (_linenr > FBitHeight) then exit ;

      if assigned( FOnGetNativeLineEvent ) then
        {$IFDEF OXYGENE}
          Result := FOnGetNativeLineEvent( Self,
                                           TGIS_GetNativeLineEventArgs.Create(
                                             _buffer, _linenr, _startIdx, _count
                                           )
                                         ) ;
        {$ELSE}
          Result := FOnGetNativeLineEvent( _buffer, _linenr, _startIdx, _count ) ;
        {$ENDIF}
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

{==================================== END =====================================}
end.

