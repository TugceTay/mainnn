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
  Template printing class.
}

{$IFDEF DCC}
  unit GisTemplatePrint ;
  {$HPPEMIT '#pragma link "GisTemplatePrint"'}
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
    System.IO,
    System.Collections,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    GisInterfaces,
    GisRtl,
    GisTypesUI,
    GisTypes,
    GisSymbol,
    GisRendererAbstract;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}

type
  {$REGION 'TGIS_TemplatePrint'}
  /// <summary>
  ///   Contains graphic data used when printing templates.
  /// </summary>
  TGIS_TemplateGraphic = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_ObjectDisposable )

    private
      FBitmap : TGIS_Bitmap ;
      FPath   : String ;
      FStream : {$IFDEF CLR} Stream {$ELSE} TStream {$ENDIF} ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy ; override ;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;

    public

      /// <summary>
      ///   Loads a graphic from given bitmap.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap to load
      /// </param>
      /// <remarks>
      ///   Creates an internal copy that is freed in the destructor or on overwrite.
      /// </remarks>
      procedure LoadFromBitmap ( const _bitmap   : TGIS_Bitmap
                               ) ;

      /// <summary>
      ///   Loads a graphic from file.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      procedure LoadFromFile   ( const _path     : String
                               ) ;

      /// <summary>
      ///   Loads a graphic from platform stream.
      /// </summary>
      /// <param name="_stream">
      ///   platform specific stream (like TStream for VCL)
      /// </param>
      /// <remarks>
      ///   Creates an internal copy that is freed in the destructor or on overwrite.
      /// </remarks>
      procedure LoadFromStream ( {$IFDEF CLR}
                                   const _stream : Stream
                                 {$ELSE}
                                   const _stream : TStream
                                 {$ENDIF}
                               ) ;

      /// <summary>
      ///   Draws the graphic using given renderer.
      /// </summary>
      /// <param name="_renderer">
      ///   drawing renderer
      /// </param>
      /// <param name="_rect">
      ///   rectangle for the graphic
      /// </param>
      /// <returns>
      ///   if true, the graphic has been drawn
      ///   if false, for some reason the graphic has not been drawn
      /// </returns>
      function Draw            ( const _renderer : TGIS_RendererAbstract ;
                                 const _rect     : TRect
                               ) : Boolean ; virtual ;

  end ;

  /// <summary>
  ///   Encapsulation of template based printing according to *.tpl or *.ttktemplate file.
  /// </summary>
  TGIS_TemplatePrint = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_ObjectDisposable )

    private

      /// <summary>
      ///   Path to the template file.
      /// </summary>
      FTemplatePath  : String ;

      /// <summary>
      ///   StreamPath with the template file.
      /// </summary>
      FTemplateStream : TStream ;

      /// <summary>
      ///   Text with the template file content. Text lines must be CRLF
      ///   separated.
      /// </summary>
      FTemplateText : String ;

      /// <summary>
      ///   The TGIS_Viewer objects attached to the template.
      /// </summary>
      FGIS_Viewers : array of IGIS_Viewer ;

      /// <summary>
      ///   Extents attached to the template.
      /// </summary>
      FGIS_ViewerExtents : array of TGIS_Extent ;

      /// <summary>
      ///   Scales attached to the template.
      /// </summary>
      FGIS_ViewerScales : array of Double ;

      /// <summary>
      ///   Background colors attached to the template.
      /// </summary>
      FGIS_UseViewerColor : array of Boolean ;

      /// <summary>
      ///   Customized viewer names which are used by the template designer.
      ///   If the names are empty or undefined,
      ///   the designer takes names from FGIS_Viewers.
      /// </summary>
      FGIS_ViewerNames : array of String ;

      /// <summary>
      ///   The TGIS_ControlLegend objects attached to the template.
      /// </summary>
      FGIS_Legends : array of IGIS_PrintableControl ;

      /// <summary>
      ///   The TGIS_ControlScale objects attached to the template.
      /// </summary>
      FGIS_Scales : array of IGIS_PrintableControl  ;

      /// <summary>
      ///   The TGIS_ControlNorthArrow objects attached to the template.
      /// </summary>
      FGIS_NorthArrows : array of IGIS_PrintableControl ;

      /// <summary>
      ///   Texts attached to the template.
      /// </summary>
      FTexts : array of String ;

      /// <summary>
      ///   Graphics attached to the template.
      /// </summary>
      FGraphics : array of TGIS_TemplateGraphic ;

      /// <summary>
      ///   If True, graphic objects will be freed in the destructor and when overwritten
      /// </summary>
      FOwnsGraphics : Boolean ;

    private
      procedure clear_arrays          ;

    protected
      procedure fset_TemplatePath     ( const _value : String
                                      ) ;
      procedure fset_TemplateStream   ( const _value : TStream
                                      ) ;
      procedure fset_TemplateText     ( const _value : String
                                      ) ;
      function  fget_GIS_Viewer       ( const _idx   : Integer
                                      ) : IGIS_Viewer ;
      procedure fset_GIS_Viewer       ( const _idx   : Integer ;
                                        const _value : IGIS_Viewer
                                      ) ;
      function  fget_GIS_ViewerExtent ( const _idx   : Integer
                                      ) : TGIS_Extent ;
      procedure fset_GIS_ViewerExtent ( const _idx   : Integer ;
                                        const _value : TGIS_Extent
                                      ) ;
      function  fget_GIS_ViewerScale  ( const _idx   : Integer
                                      ) : Double ;
      procedure fset_GIS_ViewerScale  ( const _idx   : Integer ;
                                        const _value : Double
                                      ) ;
      function  fget_GIS_UseViewerColor
                                      ( const _idx   : Integer
                                      ) : Boolean ;
      procedure fset_GIS_UseViewerColor
                                      ( const _idx   : Integer ;
                                        const _value : Boolean
                                      ) ;
      function  fget_GIS_ViewerName   ( const _idx   : Integer
                                      ) : String ;
      procedure fset_GIS_ViewerName   ( const _idx   : Integer ;
                                        const _value : String
                                      ) ;
      function  fget_GIS_Legend       ( const _idx   : Integer
                                      ) : IGIS_PrintableControl ;
      procedure fset_GIS_Legend       ( const _idx   : Integer ;
                                        const _value : IGIS_PrintableControl
                                      ) ;
      function  fget_GIS_Scale        ( const _idx   : Integer
                                      ) : IGIS_PrintableControl ;
      procedure fset_GIS_Scale        ( const _idx   : Integer ;
                                        const _value : IGIS_PrintableControl
                                      ) ;
      function  fget_GIS_NorthArrow   ( const _idx   : Integer
                                      ) : IGIS_PrintableControl ;
      procedure fset_GIS_NorthArrow   ( const _idx   : Integer ;
                                        const _value : IGIS_PrintableControl
                                      ) ;
      function  fget_Text             ( const _idx   : Integer
                                      ) : String ;
      procedure fset_Text             ( const _idx   : Integer ;
                                        const _value : String
                                      ) ;
      function  fget_Graphic          ( const _idx   : Integer
                                      ) : TGIS_TemplateGraphic ;
      procedure fset_Graphic          ( const _idx   : Integer ;
                                        const _value : TGIS_TemplateGraphic
                                      ) ;
      function fget_GIS_ViewerCount         : Integer ;
      function fget_GIS_ViewerExtentCount   : Integer ;
      function fget_GIS_ViewerScaleCount    : Integer ;
      function fget_GIS_UseViewerColorCount : Integer ;
      function fget_GIS_ViewerNameCount     : Integer ;
      function fget_GIS_LegendCount         : Integer ;
      function fget_GIS_ScaleCount          : Integer ;
      function fget_GIS_NorthArrowCount     : Integer ;
      function fget_TextCount               : Integer ;
      function fget_GraphicCount            : Integer ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy ; override ;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <remarks>
      ///   Equivalent to Create( True ) .
      /// </remarks>
      constructor Create              ; overload ;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_ownsGraphics">
      ///   If True, graphic objects will be freed in the destructor and when overwritten
      /// </param>
      constructor Create              ( const _ownsGraphics : Boolean
                                      ) ; overload ;

      /// <summary>
      ///   Clear all object and texts assigned to template.
      /// </summary>
      procedure Reset                 ;

      /// <summary>
      ///   Create a new object and copy values to it .
      /// </summary>
      /// <returns>
      ///   new created object
      /// </returns>
      function  MakeCopy              : TGIS_TemplatePrint ;

      /// <summary>
      ///   Save values from a copy created by MakeCopy .
      /// </summary>
      /// <param name="_copy">
      ///   object to free
      /// </param>
      procedure SaveCopy              ( const _copy : TGIS_TemplatePrint
                                      ) ;

      /// <summary>
      ///   Resolve text macros used in printed texts.
      /// </summary>
      /// <param name="_name">
      ///   macro name
      /// </param>
      /// <returns>
      ///   resolved text
      /// </returns>
      /// <remarks>
      ///   There are predefined macros:
      ///     now      - current time
      ///     scaleN   - ScaleAsText value from GIS_Viewer[N]
      ///     projectN - ProjectName value from GIS_Viewer[N}
      /// </remarks>
      function  GetField    ( const _name : String ) : Variant ;
    public
      /// <summary>
      ///   Path to the template file.
      /// </summary>
      property TemplatePath     : String       read  FTemplatePath
                                               write fset_TemplatePath ;

      /// <summary>
      ///   Stream with the template file.
      /// </summary>
      property TemplateStream   : TStream      read  FTemplateStream
                                               write fset_TemplateStream ;

      /// <summary>
      ///   Text with the template file content. Text lines must be CRLF
      ///   separated.
      /// </summary>
      property TemplateText     : String       read  FTemplateText
                                               write fset_TemplateText ;
      /// <summary>
      ///   The TGIS_Viewer object attached to the template.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Corresponds to MAP[n] template element. Index starts from 1.
      ///   By default object provided in a Create constructor will be attached
      ///   as index 1.
      /// </remarks>
      property GIS_Viewer[ const _idx : Integer ] : IGIS_Viewer
                                               read  fget_GIS_Viewer
                                               write fset_GIS_Viewer ;

      /// <summary>
      ///   Map Extent to be used for printing.
      ///   After printing it contains the real used extent.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Index starts from 1. GIS_ViewerExtent[n] will correspond to
      ///   GIS_Viewer[n] and GIS_ViewerScale[n]. By default VisibleExtent of the
      ///   object provided in a Create constructor will be attached as index 1.
      ///   See also GIS_ViewerScale property.
      /// </remarks>
      property GIS_ViewerExtent[ const _idx : Integer ] : TGIS_Extent
                                               read  fget_GIS_ViewerExtent
                                               write fset_GIS_ViewerExtent ;

      /// <summary>
      ///   Map Scale to be used for printing.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Index starts from 1. GIS_ViewerScale[n] will correspond to
      ///   GIS_Viewer[n] and GIS_ViewerExtent[n]. If greater then zero, then
      ///   GIS_Viewer[n] object  will be printed using provided scale (centered at
      ///   the center of  GIS_ViewerExtent). If equal 0, then GIS_Viewer[n] will be
      ///   printed to encompass GIS_ViewerExtent.
      ///   By default 0 will be attached as index 1.
      /// </remarks>
      property GIS_ViewerScale[ const _idx : Integer ] : Double
                                               read  fget_GIS_ViewerScale
                                               write fset_GIS_ViewerScale ;

      /// <summary>
      ///   If Yes then viewer Color will be used as a background color,
      ///   otherwise there will be no background.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Index starts from 1. GIS_UseViewerColor[n] will correspond to
      ///   GIS_Viewer[n].
      ///   By default 0 will be attached as index 1.
      ///   Used only with ttktemplate files.
      /// </remarks>
      property GIS_UseViewerColor[ const _idx : Integer ] : Boolean
                                               read  fget_GIS_UseViewerColor
                                               write fset_GIS_UseViewerColor ;

      /// <summary>
      ///   Customized viewer names which are used by the template designer.
      ///   If the names are empty or undefined,
      ///   the designer takes names from GIS_Viewer objects.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Index starts from 1. GIS_ViewerName[n] will correspond to
      ///   GIS_Viewer[n].
      ///   By default 0 will be attached as index 1.
      ///   Used only with ttktemplate files.
      /// </remarks>
      property GIS_ViewerName[ const _idx : Integer ] : String
                                               read  fget_GIS_ViewerName
                                               write fset_GIS_ViewerName ;

      /// <summary>
      ///   The TGIS_ControlLegend object attached to the template.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Corresponds to LEGEND[n] template element.
      /// </remarks>
      property GIS_Legend[ const _idx : Integer ] : IGIS_PrintableControl
                                               read  fget_GIS_Legend
                                               write fset_GIS_Legend ;

      /// <summary>
      ///   The TGIS_ControlScale object attached to the template.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Corresponds to SCALE[n] template element.
      /// </remarks>
      property GIS_Scale[ const _idx : Integer ] : IGIS_PrintableControl
                                               read  fget_GIS_Scale
                                               write fset_GIS_Scale ;

      /// <summary>
      ///   The TGIS_ControlNorthArrow object attached to the template.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Corresponds to NORTHARROW[n] template element.
      /// </remarks>
      property GIS_NorthArrow[ const _idx : Integer ] : IGIS_PrintableControl
                                               read  fget_GIS_NorthArrow
                                               write fset_GIS_NorthArrow ;

      /// <summary>
      ///   The text attached to the template.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Corresponds to TEXT[n] template element. All texts can be
      ///   accessed at one using Texts property.
      /// </remarks>
      property Text[ const _idx : Integer ] : String
                                               read  fget_Text
                                               write fset_Text ;

      /// <summary>
      ///   The graphic attached to the template.
      /// </summary>
      /// <param name="_idx">
      ///   index on the list
      /// </param>
      /// <remarks>
      ///   Corresponds to GRAPHIC[n] template element.
      /// </remarks>
      /// <remarks>
      ///   Unlike others objects, by default they will be destroyed
      ///   along with the TemplatePrint object.
      /// </remarks>
      property Graphic[ const _idx : Integer ] : TGIS_TemplateGraphic
                                               read  fget_Graphic
                                               write fset_Graphic ;

      /// <summary>
      ///   Count of GIS_Viewer elements (counting from 1).
      /// </summary>
      property GIS_ViewerCount : Integer       read  fget_GIS_ViewerCount ;

      /// <summary>
      ///   Count of GIS_ViewerExtent elements (counting from 1).
      /// </summary>
      property GIS_ViewerExtentCount : Integer read  fget_GIS_ViewerExtentCount ;

      /// <summary>
      ///   Count of GIS_ViewerScale elements (counting from 1).
      /// </summary>
      property GIS_ViewerScaleCount : Integer  read  fget_GIS_ViewerScaleCount ;

      /// <summary>
      ///   Count of GIS_UseViewerColor elements (counting from 1).
      /// </summary>
      property GIS_UseViewerColorCount : Integer read  fget_GIS_UseViewerColorCount ;

      /// <summary>
      ///   Count of GIS_ViewerName elements (counting from 1).
      /// </summary>
      property GIS_ViewerNameCount : Integer read  fget_GIS_ViewerNameCount ;

      /// <summary>
      ///   Count of GIS_Legend elements (counting from 1).
      /// </summary>
      property GIS_LegendCount : Integer       read fget_GIS_LegendCount ;

      /// <summary>
      ///   Count of GIS_Scale elements (counting from 1).
      /// </summary>
      property GIS_ScaleCount : Integer        read fget_GIS_ScaleCount ;

      /// <summary>
      ///   Count of GIS_NorthArrow elements.
      /// </summary>
      property GIS_NorthArrowCount : Integer   read fget_GIS_NorthArrowCount ;

      /// <summary>
      ///   Count of Text elements.
      /// </summary>
      property TextCount : Integer             read fget_TextCount ;

      /// <summary>
      ///   Count of Graphic elements.
      /// </summary>
      property GraphicCount : Integer          read fget_GraphicCount ;
  end ;
  {$ENDREGION}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
  System.SysUtils,
  System.Math,

  GisClasses,
  GisInternals,
  GisResource,
  GisFunctions ;
{$ENDIF}

{$REGION 'TGIS_TemplatePrint'}
//=============================================================================
// TGIS_TemplateGraphic
//=============================================================================

  constructor TGIS_TemplateGraphic.Create ;
  begin
    inherited ;
    FBitmap := nil ;
    FPath   := '' ;
    FStream := nil ;
  end ;

  procedure TGIS_TemplateGraphic.doDestroy ;
  begin
    FreeObject( FBitmap ) ;
    FreeObject( FStream ) ;
    inherited ;
  end ;

  procedure TGIS_TemplateGraphic.LoadFromBitmap(
    const _bitmap : TGIS_Bitmap
  ) ;
  begin
    FreeObject( FBitmap ) ;
    FreeObject( FStream ) ;

    FBitmap := TGIS_Bitmap.Create ;
    FBitmap.Assign( _bitmap ) ;

    FPath := '' ;
    FStream := nil ;
  end ;

  procedure TGIS_TemplateGraphic.LoadFromFile(
    const _path : String
  ) ;
  begin
    FreeObject( FBitmap ) ;
    FreeObject( FStream ) ;

    FPath := _path ;

    FBitmap := nil ;
    FStream := nil ;
  end ;

  procedure TGIS_TemplateGraphic.LoadFromStream(
    {$IFDEF CLR}
      const _stream : Stream
    {$ELSE}
      const _stream : TStream
    {$ENDIF}
  ) ;
  begin
    FreeObject( FBitmap ) ;
    FreeObject( FStream ) ;

    FStream := TMemoryStream.Create ;
    try
      FStream.CopyFrom( _stream, _stream.Size ) ;
      FStream.Position := 0 ;
    except
      FStream := nil ;
    end;

    FPath   := '' ;
    FBitmap := nil ;
  end ;

  function TGIS_TemplateGraphic.Draw(
    const _renderer : TGIS_RendererAbstract ;
    const _rect     : TRect
  ) : Boolean ;
  var
    w, h : Integer ;
    f    : Double ;
    sym  : TGIS_SymbolAbstract ;
    path : String ;
  begin
    Result := False ;

    if ( _rect.Width = 0 ) or ( _rect.Height = 0 ) then exit ;
    w := _rect.Width ;
    h := _rect.Height ;

    if assigned( FBitmap ) then begin

      if not FBitmap.IsEmpty and
         ( FBitmap.Width > 0 ) and ( FBitmap.Height > 0 ) then begin
        f := Min( w/FBitmap.Width, h/FBitmap.Height ) ;
        _renderer.RenderBitmap( nil, FBitmap,
                                Rect( RoundS( ( w - f * FBitmap.Width ) / 2 ),
                                      RoundS( ( h - f * FBitmap.Height ) / 2 ),
                                      RoundS( ( w + f * FBitmap.Width ) / 2 ),
                                      RoundS( ( h + f * FBitmap.Height ) / 2 ) ),
                                True ) ;
        Result := True ;
      end ;
      exit ;

    end ;

    sym := nil ;
    if not IsStringEmpty( FPath ) then begin
      try
        path := FPath ;
        sym := SymbolList.Prepare( path ) ;
      except
      end ;
    end
    else if assigned( FStream ) then begin
      try
        FStream.Position := 0 ;
        path := '##templategraphicstream##' ;
        sym := SymbolList.Prepare( path, FStream ) ;
      except
      end ;
    end ;

    if assigned( sym ) then begin

      try
        if ( sym.Width > 0 ) and ( sym.Height > 0 ) then begin
          f := Min( w/sym.Width, h/sym.Height ) ;
          sym.Prepare( _renderer.Viewer,
                       -Max( RoundS(sym.Width*f), RoundS(sym.Height*f) ), // size in pixels
                       TGIS_Color.Black, // .Color,
                       TGIS_Color.Black, // .OutlineColor,
                       0,                // .SymbolRotate,
                       0,
                       TGIS_LabelPosition.MiddleCenter,
                       True,
                       _renderer
                     ) ;
          sym.Draw( w div 2, h div 2 ) ;
          sym.Unprepare ;
          Result := True ;
        end ;
      finally
        SymbolList.InternalDelete( path ) ;
      end ;

    end ;

  end ;

//=============================================================================
// TGIS_TemplatePrint
//=============================================================================

  procedure TGIS_TemplatePrint.fset_TemplatePath(
    const _value : String
  ) ;
  var
    lst : TStringList ;
  begin
    if not IsStringEmpty( _value ) then begin
      lst := TStringList.Create ;
      try
        if GetFileExt(_value) = GIS_TTKTEMPLATE_EXT then
          lst.LoadFromFile( _value, TEncoding.UTF8 )
        else
          lst.LoadFromFile( _value ) ;
        TemplateText := lst.Text ;
      finally
        FreeObject( lst ) ;
      end ;
    end else
      TemplateText := '' ;
    FTemplatePath := _value ;
  end ;

  procedure TGIS_TemplatePrint.fset_TemplateStream(
    const _value : TStream
  ) ;
  var
    lst : TStringList ;
  begin
    lst := TStringList.Create ;
    try
      lst.LoadFromStream( _value, TEncoding.UTF8 ) ;
      TemplateText := lst.Text  ;
    finally
      FreeObject( lst ) ;
    end ;
    FTemplateStream := _value ;
  end ;

  procedure TGIS_TemplatePrint.fset_TemplateText(
    const _value : String
  ) ;
  begin
    FTemplatePath := '' ;
    FTemplateStream := nil ;
    FTemplateText := _value ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_Viewer(
    const _idx : Integer
  ) : IGIS_Viewer ;
  begin
    Result := nil ;
    if ( _idx >= 1 ) and ( _idx < length( FGIS_Viewers ) ) then
      Result := FGIS_Viewers[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_GIS_Viewer(
    const _idx   : Integer ;
    const _value : IGIS_Viewer
  ) ;
  begin
    if _idx >= length( FGIS_Viewers ) then
      SetLength( FGIS_Viewers, _idx + 1 ) ;
    FGIS_Viewers[ _idx ] := _value ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_ViewerExtent(
    const _idx : Integer
  ) : TGIS_Extent ;
  begin
    Result := GisWholeWorld ;
    if ( _idx >= 1 ) and ( _idx < length( FGIS_ViewerExtents ) ) then
      Result := FGIS_ViewerExtents[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_GIS_ViewerExtent(
    const _idx   : Integer ;
    const _value : TGIS_Extent
  ) ;
  begin
    if _idx >= length( FGIS_ViewerExtents ) then
      SetLength( FGIS_ViewerExtents, _idx + 1 ) ;
    FGIS_ViewerExtents[ _idx ] := _TGIS_Extent( _value ) ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_ViewerScale(
    const _idx : Integer
  ) : Double ;
  begin
    Result := 0 ;
    if ( _idx >= 1 ) and ( _idx < length( FGIS_ViewerScales ) ) then
      Result := FGIS_ViewerScales[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_GIS_ViewerScale(
    const _idx   : Integer ;
    const _value : Double
  ) ;
  begin
    if _idx >= length( FGIS_ViewerScales ) then
      SetLength( FGIS_ViewerScales, _idx + 1 ) ;
    FGIS_ViewerScales[ _idx ] := _value ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_UseViewerColor(
    const _idx : Integer
  ) : Boolean ;
  begin
    Result := True ;
    if ( _idx >= 1 ) and ( _idx < length( FGIS_UseViewerColor ) ) then
      Result := FGIS_UseViewerColor[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_GIS_UseViewerColor(
    const _idx   : Integer ;
    const _value : Boolean
  ) ;
  var
    l : Integer ;
    i : Integer ;
  begin
    l := length( FGIS_UseViewerColor ) ;
    if _idx >= length( FGIS_UseViewerColor ) then
      SetLength( FGIS_UseViewerColor, _idx + 1 ) ;
    for i := l to _idx - 1 do
      FGIS_UseViewerColor[ i ] := True ;
    FGIS_UseViewerColor[ _idx ] := _value ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_ViewerName(
    const _idx : Integer
  ) : String ;
  begin
    Result := '' ;
    if ( _idx >= 1 ) and ( _idx < length( FGIS_ViewerNames ) ) then
      Result := FGIS_ViewerNames[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_GIS_ViewerName(
    const _idx   : Integer ;
    const _value : String
  ) ;
  begin
    if _idx >= length( FGIS_ViewerNames ) then
      SetLength( FGIS_ViewerNames, _idx + 1 ) ;
    FGIS_ViewerNames[ _idx ] := _value ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_Legend(
    const _idx : Integer
  ) : IGIS_PrintableControl ;
  begin
    Result := nil ;
    if ( _idx >= 1 ) and ( _idx < length( FGIS_Legends ) ) then
      Result := FGIS_Legends[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_GIS_Legend(
    const _idx   : Integer ;
    const _value : IGIS_PrintableControl
  ) ;
  begin
    if _idx >= length( FGIS_Legends ) then
      SetLength( FGIS_Legends, _idx + 1 ) ;
    FGIS_Legends[ _idx ] := _value ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_Scale(
    const _idx : Integer
  ) : IGIS_PrintableControl ;
  begin
    Result := nil ;
    if ( _idx >= 1 ) and ( _idx < length( FGIS_Scales ) ) then
      Result := FGIS_Scales[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_GIS_Scale(
    const _idx   : Integer ;
    const _value : IGIS_PrintableControl
  ) ;
  begin
    if _idx >= length( FGIS_Scales ) then
      SetLength( FGIS_Scales, _idx + 1 ) ;
    FGIS_Scales[ _idx ] := _value ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_NorthArrow(
    const _idx : Integer
  ) : IGIS_PrintableControl ;
  begin
    Result := nil ;
    if ( _idx >= 1 ) and ( _idx < length( FGIS_NorthArrows ) ) then
      Result := FGIS_NorthArrows[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_GIS_NorthArrow(
    const _idx   : Integer ;
    const _value : IGIS_PrintableControl
  ) ;
  begin
    if _idx >= length( FGIS_NorthArrows ) then
      SetLength( FGIS_NorthArrows, _idx + 1 ) ;
    FGIS_NorthArrows[ _idx ] := _value ;
  end ;

  function TGIS_TemplatePrint.fget_Text(
    const _idx : Integer
  ) : String ;
  begin
    Result := '' ;
    if ( _idx >= 1 ) and ( _idx < length( FTexts ) ) then
      Result := FTexts[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_Text(
    const _idx   : Integer ;
    const _value : String
  ) ;
  begin
    if _idx >= length( FTexts ) then
      SetLength( FTexts, _idx + 1 ) ;
    FTexts[ _idx ] := _value ;
  end ;

  function TGIS_TemplatePrint.fget_Graphic(
    const _idx : Integer
  ) : TGIS_TemplateGraphic ;
  begin
    Result := nil ;
    if ( _idx >= 1 ) and ( _idx < length( FGraphics ) ) then
      Result := FGraphics[ _idx ] ;
  end ;

  procedure TGIS_TemplatePrint.fset_Graphic(
    const _idx   : Integer ;
    const _value : TGIS_TemplateGraphic
  ) ;
  begin
    if FOwnsGraphics and
       ( _idx > 0 ) and ( _idx <= length( FGraphics ) ) then begin
      if assigned( FGraphics[ _idx ] ) then
        FreeObject( FGraphics[ _idx ] ) ;
    end ;
    if _idx >= length( FGraphics ) then
      SetLength( FGraphics, _idx + 1 ) ;
    FGraphics[ _idx ] := _value ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_ViewerCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FGIS_Viewers ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_ViewerExtentCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FGIS_ViewerExtents ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_ViewerScaleCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FGIS_ViewerScales ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_UseViewerColorCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FGIS_UseViewerColor ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_ViewerNameCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FGIS_ViewerNames ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_LegendCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FGIS_Legends ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_ScaleCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FGIS_Scales ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  function TGIS_TemplatePrint.fget_GIS_NorthArrowCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FGIS_NorthArrows ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  function TGIS_TemplatePrint.fget_TextCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FTexts ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  function TGIS_TemplatePrint.fget_GraphicCount
    : Integer ;
  var
    len : Integer ;
  begin
    len := length( FGraphics ) ;
    if len > 0 then
      Result := len - 1
    else
      Result := 0 ;
  end ;

  procedure TGIS_TemplatePrint.clear_arrays ;
  begin
    SetLength( FGIS_Viewers        , 0 ) ;
    SetLength( FGIS_ViewerExtents  , 0 ) ;
    SetLength( FGIS_ViewerScales   , 0 ) ;
    SetLength( FGIS_UseViewerColor , 0 ) ;
    SetLength( FGIS_ViewerNames    , 0 ) ;
    SetLength( FGIS_Legends        , 0 ) ;
    SetLength( FGIS_Scales         , 0 ) ;
    SetLength( FGIS_NorthArrows    , 0 ) ;
    SetLength( FTexts              , 0 ) ;
    SetLength( FGraphics           , 0 ) ;
  end ;

  constructor TGIS_TemplatePrint.Create ;
  begin
    Create( True ) ;
  end ;

  constructor TGIS_TemplatePrint.Create(
    const _ownsGraphics : Boolean
  ) ;
  begin
    inherited Create ;
    clear_arrays ;
    FOwnsGraphics := _ownsGraphics ;
  end ;

  procedure TGIS_TemplatePrint.doDestroy ;
  begin
    Reset ;
    inherited ;
  end ;

  procedure TGIS_TemplatePrint.Reset ;
  var
    i : Integer ;
  begin
    if FOwnsGraphics then begin
      for i := 1 to GraphicCount do
        Graphic[i] := nil ;
    end ;
    clear_arrays ;
  end ;

  function TGIS_TemplatePrint.MakeCopy
    : TGIS_TemplatePrint ;
  var
    i : Integer ;
  begin
    Result := TGIS_TemplatePrint.Create( False ) ;
    if not IsStringEmpty( TemplatePath ) then
      Result.TemplatePath := TemplatePath
    else if assigned( TemplateStream ) then
      Result.TemplateStream := TemplateStream
    else
      Result.TemplateText := TemplateText ;
    for i := 1 to length( FGIS_Viewers ) - 1 do
      Result.GIS_Viewer[i] := FGIS_Viewers[i] ;
    for i := 1 to length( FGIS_ViewerExtents ) - 1 do
      Result.GIS_ViewerExtent[i] := FGIS_ViewerExtents[i] ;
    for i := 1 to length( FGIS_ViewerScales ) - 1 do
      Result.GIS_ViewerScale[i] := FGIS_ViewerScales[i] ;
    for i := 1 to length( FGIS_UseViewerColor ) - 1 do
      Result.GIS_UseViewerColor[i] := FGIS_UseViewerColor[i] ;
    for i := 1 to length( FGIS_ViewerNames ) - 1 do
      Result.GIS_ViewerName[i] := FGIS_ViewerNames[i] ;
    for i := 1 to length( FGIS_Legends ) - 1 do begin
      if assigned( FGIS_Legends[i] ) then
        Result.GIS_Legend[i] := FGIS_Legends[i].CreateCopy
      else
        Result.GIS_Legend[i] := nil ;
    end ;
    for i := 1 to length( FGIS_Scales ) - 1 do begin
      if assigned( FGIS_Scales[i] ) then
        Result.GIS_Scale[i] := FGIS_Scales[i].CreateCopy
      else
        Result.GIS_Scale[i] := nil ;
    end ;
    for i := 1 to length( FGIS_NorthArrows ) - 1 do begin
      if assigned( FGIS_NorthArrows[i] ) then
        Result.GIS_NorthArrow[i] := FGIS_NorthArrows[i].CreateCopy
      else
        Result.GIS_NorthArrow[i] := nil ;
    end ;
    for i := 1 to length( FTexts ) - 1 do
      Result.Text[i] := FTexts[i] ;
    for i := 1 to length( FGraphics ) - 1 do begin
      if assigned( FGraphics[i] ) then
        Result.Graphic[i] := FGraphics[i]
      else
        Result.Graphic[i] := nil ;
    end ;
  end ;

  procedure TGIS_TemplatePrint.SaveCopy(
    const _copy : TGIS_TemplatePrint
  ) ;
  var
    i :Integer ;
  begin
    for i := 1 to GIS_LegendCount do
      if assigned( GIS_Legend[i] ) then
        GIS_Legend[i].FreeCopy( _copy.GIS_Legend[i] ) ;
    for i := 1 to GIS_ScaleCount do
      if assigned( GIS_Scale[i] ) then
        GIS_Scale[i].FreeCopy( _copy.GIS_Scale[i] ) ;
    for i := 1 to GIS_NorthArrowCount do
      if assigned( GIS_NorthArrow[i] ) then
        GIS_NorthArrow[i].FreeCopy( _copy.GIS_NorthArrow[i] ) ;
  end ;

  function TGIS_TemplatePrint.GetField(
    const _name : String
  ) : Variant ;
  var
    name : String ;
    idx  : Integer ;

    function is_macro( _macro : String ; var _idx : Integer ) : Boolean ;
    begin
      if Pos( _macro, name ) = 1 then Result := True
                                 else Result := False ;
      if Result then begin
        try
          _idx := StrToInt( Copy( name,
                                  length(_macro)+1,
                                  length(name) - length(_macro)
                                 )
                          ) ;
        except
          _idx := 0 ;
        end;
      end ;
    end ;
  begin
    name := LowerCase( _name ) ;
    if name = 'now' then Result := Now
    else
    if is_macro( 'scale', idx ) then begin
      if assigned( GIS_Viewer[idx] ) then
        Result := GIS_Viewer[idx].ScaleAsText
      else
        Result := '' ;
    end else
    if is_macro( 'project', idx ) then begin
      if assigned( GIS_Viewer[idx] ) then
        Result := GIS_Viewer[idx].ProjectName
      else
        Result := '' ;
    end else
      Result := '' ;
  end ;
{$ENDREGION}

{==================================== END =====================================}


end.

