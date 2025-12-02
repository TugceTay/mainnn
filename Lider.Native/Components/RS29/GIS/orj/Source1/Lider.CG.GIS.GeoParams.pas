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
  Parameters for shapes, zoom-sets etc.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoParams ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoParams"'}
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

{$IFNDEF OXYGENE}
  {$RANGECHECKS OFF}
{$ENDIF}

interface

{$IFDEF CLR}
  uses
    System.Drawing,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Types,
    System.Classes,
    System.Generics.Collections,
    {$IFDEF RTL_XE2}
      System.UITypes,
    {$ENDIF}

    Lider.CG.GIS.GeoLogger,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoSymbol,
    Lider.CG.GIS.GeoConfig,
    Lider.CG.GIS.GeoSqlQuery ;
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
  TGIS_ParamsList          = {$IFDEF OXYGENE} public {$ENDIF} class ;
  TGIS_ParamsVector        = {$IFDEF OXYGENE} public {$ENDIF} class ;
  TGIS_ParamsSectionVector = {$IFDEF OXYGENE} public {$ENDIF} class ;
  TGIS_ParamsField         = {$IFDEF OXYGENE} public {$ENDIF} class ;

  /// <summary>
  ///   User object class encapsulation.
  /// </summary>
  TGIS_ParamsUserObject = {$IFDEF OXYGENE} public abstract {$ENDIF} class( TGIS_Object )
    public
      /// <summary>
      ///   Create a copy of object.
      /// </summary>
      /// <returns>
      ///   Constructed copy of the object.
      /// </returns>
      function CreateCopy : TGIS_ParamsUserObject ; virtual ; abstract ;
  end ;

  /// <summary>
  ///   Common parameters ancestor.
  /// </summary>
  TGIS_ParamsAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                        class ( TPersistent
                                {$IFDEF CLR}
                                  , IDisposable
                                {$ENDIF} )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FSerial     : Integer ;
      FSerialSeed : Integer ;
      {$IFDEF DCC} [unsafe] {$ENDIF}
      FParamsList : TGIS_ParamsList ;
      oConfig     : TGIS_Config ;
      FUserObject : TGIS_ParamsUserObject ;
    protected
      function  fget_UserObject      : TGIS_ParamsUserObject ;
      procedure fset_UserObject      ( const _value : TGIS_ParamsUserObject
                                     ) ;
    protected
      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy          ; virtual;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create           ; virtual;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   Destroy an instance.
        /// </summary>
        procedure   Dispose        ; virtual;
      {$ELSE}
        /// <summary>
        ///   Destroy an instance.
        /// </summary>
        destructor  Destroy        ; override;
      {$ENDIF}

      /// <summary>
      ///   Make a copy of the object.
      /// </summary>
      /// <returns>
      ///   Constructed copy of the object.
      /// </returns>
      function    CreateCopy       : TGIS_ParamsAbstract ;

      /// <summary>
      ///   Touch the object by incrementing Serial number.
      /// </summary>
      procedure   Touch            ; virtual;

      /// <summary>
      ///   Reset serial numer conter.
      /// </summary>
      /// <remarks>
      ///   Use only if original value of change tracker must be restored.
      /// </remarks>
      procedure   ResetSerial      ;


      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      procedure   Assign           ( _source    : TPersistent
                                   ) ; override;

      /// <summary>
      ///   Load parameters from the configuration file.
      /// </summary>
      /// <param name="_cfg">
      ///   source configuration file
      /// </param>
      procedure   LoadFromConfig   ( const _cfg : TGIS_ConfigAbstract
                                   ) ; virtual; abstract;

      /// <summary>
      ///   Save parameters into the configuration file.
      /// </summary>
      /// <param name="_cfg">
      ///   destination configuration file
      /// </param>
      procedure   SaveToConfig    ( const _cfg : TGIS_ConfigAbstract
                                  ) ; virtual; abstract;

    public

       /// <summary>
       ///   Serial number updated after any property change. Used to identify
       ///   if object was changed. Used by legend control for smart updates.
       /// </summary>
       property Serial             : Integer
                                     read  FSerial
                                     write FSerial      ;
      /// <summary>
      ///   UserObject can be used to associate with Params a user-defined
      ///   object. Such object will be destroyed automatically
      ///   upon layer destroy. Attaching a new object to this property
      ///   will destroy existing one.
      /// </summary>
      {#ownership:set:release}
      property UserObject : TGIS_ParamsUserObject read  fget_UserObject
                                                  write fset_UserObject ;
  end ;
  TGIS_ParamsAbstractClass = class of TGIS_ParamsAbstract ;

  /// <summary>
  ///   Common parameters ancestor for Layer.Params sub objects.
  /// </summary>
  TGIS_ParamsSpecific = {$IFDEF OXYGENE} public abstract {$ENDIF}
                        class ( TGIS_ParamsAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      {$IFDEF DCC} [unsafe] {$ENDIF}
      FParent      : TPersistent ;
      {$IFDEF DCC} [unsafe] {$ENDIF}
      FParentProxy : TPersistent ;

    private

      function    fget_Shape       : TObject ;

    public

      /// <inheritdoc/>
      procedure   Touch            ; override;

      /// <summary>
      ///   Load parameters from a list of strings.
      /// </summary>
      /// <param name="_str">
      ///   source list of strings
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Provided _str should have a form of Parameter=Value pairs.
      ///    </note>
      /// </remarks>
      procedure LoadFromStrings    ( const _str : {$IFDEF OXYGENE}
                                                    TGIS_Strings
                                                  {$ELSE}
                                                    TStrings
                                                  {$ENDIF}
                                   ) ; virtual;

      /// <summary>
      ///   Save parameters into a list of strings.
      /// </summary>
      /// <param name="_str">
      ///   destination list of strings
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Provided _str will have a form of Parameter=Value pairs.
      ///    </note>
      /// </remarks>
      procedure SaveToStrings      ( const _str : {$IFDEF OXYGENE}
                                                    TGIS_Strings
                                                  {$ELSE}
                                                    TStrings
                                                  {$ENDIF}
                                   ) ; virtual;

      /// <summary>
      ///   Get root element from params list.
      /// </summary>
      /// <returns>
      ///   Root element.
      /// </returns>
      function    GetRoot          : TGIS_ParamsAbstract ;

    public
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Owner of the parameters. Section to which the parameter object
      ///   belongs.
      /// </summary>
      {$IFDEF DCC} [unsafe] {$ENDIF}
      property Parent              : TPersistent
                                     read FParent
                                     write FParent ;

      /// <summary>
      ///   Shape associated with parameter (if any)
      ///   belongs.
      /// </summary>
      {$IFDEF DCC} [unsafe] {$ENDIF}
      property Shape               : TObject
                                     read fget_Shape ;

  end ;

  /// <summary>
  ///   Common parameters ancestor for vector layer parameters.
  /// </summary>
  TGIS_ParamsVector = {$IFDEF OXYGENE} public {$ENDIF}
                      class ( TGIS_ParamsSpecific )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Prepare params for vector layers.
      /// </summary>
      function    prepareParams       : TGIS_ParamsVector ; virtual;

      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure   assignInternal      ( _source : TPersistent ) ; virtual;

      /// <summary>
      ///   Prepare params for vector layers. Allocate if necessary. Assign
      ///   from layer if Shape level not exist.
      /// </summary>
      function    prepareParamsVector : TGIS_ParamsSectionVector ;
    public

      /// <inheritdoc/>
      procedure LoadFromConfig ( const _cfg : TGIS_ConfigAbstract
                               ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig   ( const _cfg : TGIS_ConfigAbstract
                               ) ; override;
  end ;

  /// <summary>
  ///   Parameters that are common for rendering.
  /// </summary>
  TGIS_ParamsRender = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_ParamsVector )

    private
      FExpressionObj          : TGIS_SqlQuery ;
      FChart                  : String  ;
      FChartObj               : TList<TGIS_SqlQuery>   ;
      FMinVal                 : Double  ;
      FMaxVal                 : Double  ;
      FZones                  : Integer ;
      FMinValEx               : Double  ;
      FMaxValEx               : Double  ;
      FZonesEx                : Integer ;
      FSizeDefault            : Integer ;
      FSizeDefaultAsText      : String ;
      FColorDefault           : TGIS_Color ;
      FStartSize              : Integer ;
      FStartSizeAsText        : String ;
      FEndSize                : Integer ;
      FEndSizeAsText          : String ;
      FStartColor             : TGIS_Color ;
      FEndColor               : TGIS_Color ;
      FStartSizeEx            : Integer ;
      FStartSizeExAsText      : String ;
      FEndSizeEx              : Integer ;
      FEndSizeExAsText        : String ;
      FStartColorEx           : TGIS_Color ;
      FEndColorEx             : TGIS_Color ;
      FRound                  : Integer ;
      FInterpolationBase      : Double ;
      FColorInterpolationMode : TGIS_ColorInterpolationMode ;
      FColorRamp              : TGIS_ColorMapArray ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      function  fget_Expression         : String ;
      procedure fset_Expression         ( const _value : String             );
      procedure fset_Chart              ( const _value : String             );
      procedure fset_MinVal             ( const _value : Double             );
      procedure fset_MaxVal             ( const _value : Double             );
      procedure fset_Zones              ( const _value : Integer            );
      procedure fset_MinValEx           ( const _value : Double             );
      procedure fset_MaxValEx           ( const _value : Double             );
      procedure fset_ZonesEx            ( const _value : Integer            );
      procedure fset_SizeDefault        ( const _value : Integer            );
      procedure fset_ColorDefault       ( const _value : TGIS_Color         );
      procedure fset_StartSize          ( const _value : Integer            );
      procedure fset_EndSize            ( const _value : Integer            );
      procedure fset_StartColor         ( const _value : TGIS_Color         );
      procedure fset_EndColor           ( const _value : TGIS_Color         );
      procedure fset_StartSizeEx        ( const _value : Integer            );
      procedure fset_EndSizeEx          ( const _value : Integer            );
      procedure fset_StartColorEx       ( const _value : TGIS_Color         );
      procedure fset_EndColorEx         ( const _value : TGIS_Color         );
      procedure fset_Round              ( const _value : Integer            );
      procedure fset_InterpolationBase  ( const _value : Double             );
      procedure fset_ColorInterpolationMode
                                        ( const _value : TGIS_ColorInterpolationMode );
      procedure fset_ColorRamp          ( const _value : TGIS_ColorMapArray );
      function  fget_SizeDefaultAsText  : String ;
      procedure fset_SizeDefaultAsText  ( const _value : String             );
      function  fget_ColorDefaultAsText : String ;
      procedure fset_ColorDefaultAsText ( const _value : String             );
      function  fget_StartSizeAsText    : String ;
      procedure fset_StartSizeAsText    ( const _value : String             );
      function  fget_EndSizeAsText      : String ;
      procedure fset_EndSizeAsText      ( const _value : String             );
      function  fget_StartColorAsText   : String ;
      procedure fset_StartColorAsText   ( const _value : String             );
      function  fget_EndColorAsText     : String ;
      procedure fset_EndColorAsText     ( const _value : String             );
      function  fget_StartSizeExAsText  : String ;
      procedure fset_StartSizeExAsText  ( const _value : String             );
      function  fget_EndSizeExAsText    : String ;
      procedure fset_EndSizeExAsText    ( const _value : String             );
      function  fget_StartColorExAsText : String ;
      procedure fset_StartColorExAsText ( const _value : String             );
      function  fget_EndColorExAsText   : String ;
      procedure fset_EndColorExAsText   ( const _value : String             );

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Prepare parameters object. If scope is Shape (FShape is assigned)
      ///   then a copy of object will be created on the shape.
      /// </summary>
      function  prepareParams     : TGIS_ParamsVector ; override;

      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure assignInternal    ( _source : TPersistent   ) ; override;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy         ; override;

    public

      /// <inheritdoc/>
      constructor Create          ; override;

      /// <inheritdoc/>
      procedure Assign            ( _source   : TPersistent
                                  ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig    ( const _cfg : TGIS_ConfigAbstract
                                  ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig      ( const _cfg : TGIS_ConfigAbstract
                                  ) ; override;

    public
      /// <summary>
      ///   A color ramp for coloring the shape by the renderer.
      ///   If assigned, StartColor and EndColor are ignored.
      /// </summary>
      property ColorRamp          : TGIS_ColorMapArray
                                    read  FColorRamp
                                    write fset_ColorRamp ;

      /// <summary>
      ///   Render Expression. Result must be numeric value.
      /// </summary>
      property Expression         : String
                                    read  fget_Expression
                                    write fset_Expression ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Renderer Expression Prepared. Prepared SQL query.
      /// </summary>
      property ExpressionObj      : TGIS_SqlQuery
                                    read FExpressionObj ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Renderer Chart Prepared. Prepared SQL query.
      /// </summary>
      property ChartObj           : TList<TGIS_SqlQuery>
                                    read FChartObj ;

      /// <summary>
      ///   Renderer Chart. Must be in format 'minimum:maximum:val1:val2...'.
      ///   Result must be numeric value.
      /// </summary>
      property Chart              : String
                                    read  FChart
                                    write fset_Chart;

      /// <summary>
      ///   Minimum expected value of RenderExpression.
      /// </summary>
      property MinVal             : Double
                                    read  FMinVal
                                    write fset_MinVal ;

      /// <summary>
      ///   Maximum expected value of RenderExpression.
      /// </summary>
      property MaxVal             : Double
                                    read  FMaxVal
                                    write fset_MaxVal ;

      /// <summary>
      ///   Number of zones to be used while computing actual color or size.
      /// </summary>
      property Zones              : Integer
                                    read  FZones
                                    write fset_Zones ;

      /// <summary>
      ///   Minimum expected value of RenderExpression. An extended band.
      /// </summary>
      property MinValEx           : Double
                                    read  FMinValEx
                                    write fset_MinValEx ;

      /// <summary>
      ///   Maximum expected value of RenderExpression. An extended band.
      /// </summary>
      property MaxValEx           : Double
                                    read  FMaxValEx
                                    write fset_MaxValEx ;

      /// <summary>
      ///   Number of zones to be used while computing actual color or size.
      ///   An extended band.
      /// </summary>
      property ZonesEx            : Integer
                                    read  FZonesEx
                                    write fset_ZonesEx ;

      /// <summary>
      ///   Default size (in twips) for shapes which can't be sized by renderer.
      /// </summary>
      property SizeDefault        : Integer
                                    read  FSizeDefault
                                    write fset_SizeDefault ;

      /// <summary>
      ///   SymbolDefault. Grouping as a text property all properties that
      ///   affect SizeDefault presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SizeDefaultAsText  : String
                                    read  fget_SizeDefaultAsText
                                    write fset_SizeDefaultAsText ;

      /// <summary>
      ///   Default color for shapes which can't be colorized by renderer.
      /// </summary>
      property ColorDefault       : TGIS_Color
                                    read  FColorDefault
                                    write fset_ColorDefault ;

      /// <summary>
      ///   Color. Grouping as a text property all properties that
      ///   affect Color presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property ColorDefaultAsText : String
                                    read  fget_ColorDefaultAsText
                                    write fset_ColorDefaultAsText ;

      /// <summary>
      ///   Starting size (in twips) for sizing by renderer.
      /// </summary>
      property StartSize          : Integer
                                    read  FStartSize
                                    write fset_StartSize ;

      /// <summary>
      ///   StartSize. Grouping as a text property all properties that
      ///   affect StartSize presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property StartSizeAsText    : String
                                    read  fget_StartSizeAsText
                                    write fset_StartSizeAsText ;

      /// <summary>
      ///   Ending size (in twips) for sizing by renderer.
      /// </summary>
      property EndSize            : Integer
                                    read  FEndSize
                                    write fset_EndSize ;

      /// <summary>
      ///   EndSize. Grouping as a text property all properties that
      ///   affect EndSize presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property EndSizeAsText      : String
                                    read  fget_EndSizeAsText
                                    write fset_EndSizeAsText ;

      /// <summary>
      ///   A starting color for coloring the shape by the renderer.
      /// </summary>
      property StartColor         : TGIS_Color
                                    read  FStartColor
                                    write fset_StartColor ;

      /// <summary>
      ///   A starting color for coloring the shape by the renderer.
      ///   Grouping as a text property all properties that affect StartColor presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property StartColorAsText   : String
                                    read  fget_StartColorAsText
                                    write fset_StartColorAsText ;

      /// <summary>
      ///   An ending color for coloring the shape by the renderer.
      /// </summary>
      property EndColor           : TGIS_Color
                                    read  FEndColor
                                    write fset_EndColor ;

      /// <summary>
      ///   An ending color for coloring the shape by the renderer.
      ///   Grouping as a text property all properties that affect StartColor presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property EndColorAsText     : String
                                    read  fget_EndColorAsText
                                    write fset_EndColorAsText ;

      /// <summary>
      ///   Starting size (in twips) for sizing by renderer. Extended band.
      /// </summary>
      property StartSizeEx        : Integer
                                    read  FStartSizeEx
                                    write fset_StartSizeEx ;

      /// <summary>
      ///   StartSizeEx. Grouping as a text property all properties that
      ///   affect StartSizeEx presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property StartSizeExAsText  : String
                                    read  fget_StartSizeExAsText
                                    write fset_StartSizeExAsText ;

      /// <summary>
      ///   Ending size (in twips) for sizing by renderer. Extended band.
      /// </summary>
      property EndSizeEx          : Integer
                                    read  FEndSizeEx
                                    write fset_EndSizeEx ;

      /// <summary>
      ///   EndSizeEx. Grouping as a text property all properties that
      ///   affect EndSizeEx presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property EndSizeExAsText    : String
                                    read  fget_EndSizeExAsText
                                    write fset_EndSizeExAsText ;

      /// <summary>
      ///   A starting color for coloring the shape by the renderer.
      ///   An extended band.
      /// </summary>
      property StartColorEx       : TGIS_Color
                                    read  FStartColorEx
                                    write fset_StartColorEx ;

      /// <summary>
      ///   A starting color for coloring the shape by the renderer.
      ///   An extended band.
      ///   Grouping as a text property all properties that affect StartColor presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property StartColorExAsText : String
                                    read  fget_StartColorExAsText
                                    write fset_StartColorExAsText ;

      /// <summary>
      ///   An ending color for coloring the shape by the renderer.
      ///   An extended band.
      /// </summary>
      property EndColorEx         : TGIS_Color
                                    read  FEndColorEx
                                    write fset_EndColorEx ;

      /// <summary>
      ///   An ending color for coloring the shape by the renderer.
      ///   An extended band.
      ///   Grouping as a text property all properties that affect StartColor presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property EndColorExAsText   : String
                                    read  fget_EndColorExAsText
                                    write fset_EndColorExAsText ;
      /// <summary>
      ///   Number of digits to round a RenderExpression result.
      /// </summary>
      property Round              : Integer
                                    read  FRound
                                    write fset_Round ;


      /// <summary>
      ///   Controls the way at which the interpolated values change.
      ///   Possible values are real numbers greater than zero.
      /// </summary>
      /// <remarks>
      ///   Base parameter of 1 produces a linear interpolation. This is the default.
      ///   To produce exponential interpolation use values greater than 1.
      ///   Starting from 1, the higher the base is, the higher the rate of change.
      ///   To produce logarithmic interpolation use values lower than 1.
      ///   Starting from 1, the lower the base is, the lower rate of change.
      /// </remarks>
      property InterpolationBase  : Double
                                    read FInterpolationBase
                                    write fset_InterpolationBase ;

      /// <summary>
      ///   A color space for a interpolation beetwen StartColor and EndColor
      /// </summary>
      /// <remarks>
      ///   A color interpolation can be done in a variety of ways, e.g. RGB, HSL, etc.
      /// </remarks>
      property ColorInterpolationMode : TGIS_ColorInterpolationMode
                                        read FColorInterpolationMode
                                        write fset_ColorInterpolationMode;

  end ;

  /// <summary>
  ///   Parameters that are common for pixel layers.
  /// </summary>
  TGIS_ParamsPixel = {$IFDEF OXYGENE} public {$ENDIF}
                     class ( TGIS_ParamsSpecific )
    private
      FShowLegend       : Boolean     ;
      FLegendImage      : TGIS_Bitmap ;
      FPage             : Integer     ;
      FRedBand          : Integer     ;
      FGreenBand        : Integer     ;
      FBlueBand         : Integer     ;
      FAlphaBand        : Integer     ;
      FGridBand         : Integer     ;
      FGridNoValue      : Double      ;
      FGridShadow       : Boolean     ;
      FGridShadowAngle  : Double      ;
      FGridSmoothColors : Boolean     ;
      FGreen            : Integer     ;
      FBlue             : Integer     ;
      FRed              : Integer     ;
      FBrightness       : Integer     ;
      FContrast         : Integer     ;
      FInversion        : Boolean     ;
      FGrayScale        : Boolean     ;
      FHistogram        : Boolean     ;
      FContrastEnhanced : Boolean     ;
      FHistogramPath    : String      ;
      FMinZ             : Single      ;
      FMaxZ             : Single      ;
      FMinThresholdZ    : Single      ;
      FMaxThresholdZ    : Single      ;
      FAntialias        : Boolean     ;
      FTransparentZones : TGIS_StringList ;
      FGrayMapZones     : TGIS_StringList ;
      FRedMapZones      : TGIS_StringList ;
      FGreenMapZones    : TGIS_StringList ;
      FBlueMapZones     : TGIS_StringList ;
      FFullRGBMapZones  : TGIS_StringList ;
      FAltitudeMapZones : TGIS_StringList ;
      FColorRamp        : TGIS_ColorMapArray ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      procedure fset_ShowLegend           ( const _value : Boolean  ) ;
      procedure fset_Page                 ( const _value  : Integer ) ;
      procedure fset_RedBand              ( const _value  : Integer ) ;
      procedure fset_GreenBand            ( const _value  : Integer ) ;
      procedure fset_BlueBand             ( const _value  : Integer ) ;
      procedure fset_AlphaBand            ( const _value  : Integer ) ;
      procedure fset_GridBand             ( const _value  : Integer ) ;
      procedure fset_GridNoValue          ( const _value  : Double  ) ;
      procedure fset_GridShadow           ( const _value  : Boolean ) ;
      procedure fset_GridShadowAngle      ( const _value  : Double  ) ;
      procedure fset_GridSmoothColors     ( const _value  : Boolean ) ;
      procedure fset_Red                  ( const _value  : Integer ) ;
      procedure fset_Green                ( const _value  : Integer ) ;
      procedure fset_Blue                 ( const _value  : Integer ) ;
      procedure fset_Brightness           ( const _value  : Integer ) ;
      procedure fset_Contrast             ( const _value  : Integer ) ;
      procedure fset_ContrastEnhanced     ( const _value  : Boolean ) ;
      procedure fset_Inversion            ( const _value  : Boolean ) ;
      procedure fset_GrayScale            ( const _value  : Boolean ) ;
      procedure fset_Histogram            ( const _value  : Boolean ) ;
      procedure fset_HistogramPath        ( const _value  : String  ) ;
      procedure fset_MinThresholdHeight   ( const _value  : Single  ) ;
      procedure fset_MaxThresholdHeight   ( const _value  : Single  ) ;
      procedure fset_Antialias            ( const _value  : Boolean ) ;
      procedure fset_LegendImage          ( const _value  : TGIS_Bitmap ) ;
      procedure fset_ColorRamp            ( const _value : TGIS_ColorMapArray ) ;

      /// <summary>
      ///   Property TransparentZones change notification.
      /// </summary>
      procedure doTransparentZonesChange  (       _sender : TObject ) ;

      /// <summary>
      ///   Property GrayMapZones change notification.
      /// </summary>
      procedure doGrayMapZonesChange      (       _sender : TObject ) ;

      /// <summary>
      ///   Property RedMapZones change notification.
      /// </summary>
      procedure doRedMapZonesChange       (       _sender : TObject ) ;

      /// <summary>
      ///   Property GreenMapZones change notification.
      /// </summary>
      procedure doGreeenMapZonesChange    (       _sender : TObject ) ;

      /// <summary>
      ///   Property BlueMapZones change notification.
      /// </summary>
      procedure doBlueMapZonesChange      (       _sender : TObject ) ;

      /// <summary>
      ///   Property FullRGBMapZones change notification.
      /// </summary>
      procedure doFullRGBMapZonesChange   (       _sender : TObject ) ;

      /// <summary>
      ///   Property AltitudMapZones change notification.
      /// </summary>
      procedure doAltitudMapZonesChange   (       _sender : TObject ) ;

      /// <summary>
      ///   Assign bitmap from _values object.
      /// </summary>
      /// <param name="_value">
      ///   object from which the bitmap will be assigned
      /// </param>
      procedure assignBitmap         ( const _value : TGIS_Bitmap ) ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy       ; override;

    public

      /// <inheritdoc/>
      constructor Create        ; override;

      /// <inheritdoc/>
      procedure Assign          ( _source       : TPersistent
                                ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig  ( const _cfg    : TGIS_ConfigAbstract
                                ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig    ( const _cfg    : TGIS_ConfigAbstract
                                ) ; override;

    public

      /// <summary>
      ///   Show feature in legend component?
      /// </summary>
      property ShowLegend       : Boolean
                                  read  FShowLegend
                                  write fset_ShowLegend   ;

      /// <summary>
      ///   Image in legend component.
      ///   First left-top pixel will be used as transparent color.
      /// </summary>
      property LegendImage      : TGIS_Bitmap
                                  read  FLegendImage
                                  write fset_LegendImage ;

      /// <summary>
      ///   Page number. Several file formats supports pages. For example
      ///   TIFF supports pages. Each page can represent different content or
      ///   different resolutions of the same content. Pages are numbered
      ///   starting from 1. By providing 0 default page will be used (or
      ///   multiresolution content will be used if exists).
      /// </summary>
      property Page             : Integer
                                  read  FPage
                                  write fset_Page ;

      /// <summary>
      ///   Red band number. Allows selection which band will be used for red
      ///   channel. Bands are numbered starting from 1. By providing 0
      ///   default channel will be used. By providing -1 band will be turned
      ///   off. For grayscale RedBand=GreenBand=BlueBand. If GridBand is
      ///   used then RedBand, GreenBand and BlueBand are unused.
      /// </summary>
      property RedBand          : Integer
                                  read  FRedBand
                                  write fset_RedBand ;

      /// <summary>
      ///   Green band number. Allows selection which band will be used for
      ///   green channel. Bands are numbered starting from 1. By providing 0
      ///   default band will be used. By providing -1 band will be turned
      ///   off. For grayscale RedBand=GreenBand=BlueBand. If GridBand is
      ///   used then RedBand, GreenBand and BlueBand are unused.
      /// </summary>
      property GreenBand        : Integer
                                  read  FGreenBand
                                  write fset_GreenBand ;

      /// <summary>
      ///   Blue band number. Allows selection which band will be used for
      ///   blue channel. Bands are numbered starting from 1. By providing 0
      ///   default band will be used. By providing -1 band will be turned
      ///   off. For grayscale RedBand=GreenBand=BlueBand. If GridBand is
      ///   used then RedBand, GreenBand and BlueBand are unused.
      /// </summary>
      property BlueBand         : Integer
                                  read  FBlueBand
                                  write fset_BlueBand ;

      /// <summary>
      ///   Alpha band number. Allows selection which band will be used for
      ///   alpha channel. Bands are numbered starting from 1. By providing 0
      ///   default band will be used. By providing -1 band will be turned
      ///   off. For grayscale RedBand=GreenBand=BlueBand. If GridBand is
      ///   used then RedBand, GreenBand and BlueBand are unused.
      /// </summary>
      property AlphaBand        : Integer
                                  read  FAlphaBand
                                  write fset_AlphaBand ;

      /// <summary>
      ///   Grid band number. Allows selection which band will be used for
      ///   grids. Bands are numbered starting from 1. By providing 0 default
      ///   band will be used. By providing -1 band will be turned off. If
      ///   GridBand is used then RedBand, GreenBand and BlueBand are unused.
      /// </summary>
      property GridBand         : Integer
                                  read  FGridBand
                                  write fset_GridBand ;

      /// <summary>
      ///   Grid "no-value". Allows to specify the value which will be used
      ///   to discover no-assigned-data areas on a grid. By providing
      ///   GIS_GRID_NOVALUE (32768) default (embedded into the file) value be
      ///   used.
      /// </summary>
      property GridNoValue      : Double
                                  read  FGridNoValue
                                  write fset_GridNoValue ;

      /// <summary>
      ///   Grid shadow. If true then pseudo-shadow will be generated.
      /// </summary>
      property GridShadow       : Boolean
                                  read  FGridShadow
                                  write fset_GridShadow ;

      /// <summary>
      ///   Shadow angle used for grid shadowing.
      /// </summary>
      property GridShadowAngle  : Double
                                  read  FGridShadowAngle
                                  write fset_GridShadowAngle ;

      /// <summary>
      ///   Grid smooth colors. If true, each grid cell color will be
      ///   interpolated from a color ramp zones defined in AltitudeMapZones.
      ///   The result will be continuous colors between zones.
      ///   Default value is false and discrete colors between zones.
      /// </summary>
      property GridSmoothColors : Boolean
                                  read  FGridSmoothColors
                                  write fset_GridSmoothColors ;

      /// <summary>
      ///   Red brightness +/- 100%.
      /// </summary>
      property Red              : Integer
                                  read  FRed
                                  write fset_Red ;

      /// <summary>
      ///   Green brightness +/- 100%.
      /// </summary>
      property Green            : Integer
                                  read  FGreen
                                  write fset_Green ;

      /// <summary>
      ///   Blue brightness +/- 100%.
      /// </summary>
      property Blue             : Integer
                                  read  FBlue
                                  write fset_Blue ;

      /// <summary>
      ///   Brightness +/- 100%.
      /// </summary>
      property Brightness       : Integer
                                  read  FBrightness
                                  write fset_Brightness ;

      /// <summary>
      ///   Contrast +/- 100%.
      /// </summary>
      property Contrast         : Integer
                                  read  FContrast
                                  write fset_Contrast ;

      /// <summary>
      ///   If True, then image must be inverted; default is False.
      /// </summary>
      property Inversion        : Boolean
                                  read  FInversion
                                  write fset_Inversion ;

      /// <summary>
      ///   If True, then image has automatically enhanced contrast.
      /// </summary>
      property ContrastEnhanced : Boolean
                                  read  FContrastEnhanced
                                  write fset_ContrastEnhanced ;

      /// <summary>
      ///   Minimum threshold of the grid elevation value to display.
      /// </summary>
      property MinHeightThreshold : Single read  FMinThresholdZ
                                           write fset_MinThresholdHeight ;

      /// <summary>
      ///   Maximum threshold of the grid elevation value to display.
      /// </summary>
      property MaxHeightThreshold : Single read  FMaxThresholdZ
                                           write fset_MaxThresholdHeight ;

      /// <summary>
      ///   Antialias scaling.
      /// </summary>
      property Antialias        : Boolean
                                  read  FAntialias
                                  write fset_Antialias ;
      /// <summary>
      ///   If True, then image must be gray scaled; default is False.
      /// </summary>
      property GrayScale        : Boolean
                                  read  FGrayScale
                                  write fset_GrayScale ;

      /// <summary>
      ///   If True, then image must equalized by histogram; default is False.
      /// </summary>
      property Histogram        : Boolean
                                  read  FHistogram
                                  write fset_Histogram ;

      /// <summary>
      ///   Name of read or saved histogram.
      /// </summary>
      property HistogramPath    : String
                                  read  FHistogramPath
                                  write fset_HistogramPath ;

      /// <summary>
      ///   List of transparent zones. Each entry consist of: "start_color,
      ///   end_color". E.g, "$000000, $5F5f5f".
      /// </summary>
      /// <remarks>
      ///   Colors can be expressed as:
      ///   <list type="bullet">
      ///     <item>
      ///       hexadecimal RGB color ($FF0000 for red)
      ///     </item>
      ///     <item>
      ///       integer RGB color (16711680 for red )
      ///     </item>
      ///     <item>
      ///       blue:green:red (0::0:255 for red)
      ///     </item>
      ///     <item>
      ///       color names:
      ///       <list type="bullet">
      ///         <item>
      ///           AQUA
      ///         </item>
      ///         <item>
      ///           BLACK
      ///         </item>
      ///         <item>
      ///           BLUE
      ///         </item>
      ///         <item>
      ///           FUCHSIA
      ///         </item>
      ///         <item>
      ///           GRAY
      ///         </item>
      ///         <item>
      ///           GREEN
      ///         </item>
      ///         <item>
      ///           LIME
      ///         </item>
      ///         <item>
      ///           MAROON
      ///         </item>
      ///         <item>
      ///           NAVY
      ///         </item>
      ///         <item>
      ///           OLIVE
      ///         </item>
      ///         <item>
      ///           PURPLE
      ///         </item>
      ///         <item>
      ///           RED
      ///         </item>
      ///         <item>
      ///           SILVER
      ///         </item>
      ///         <item>
      ///           TEAL
      ///         </item>
      ///         <item>
      ///           WHITE
      ///         </item>
      ///         <item>
      ///           YELLOW
      ///         </item>
      ///       </list>
      ///     </item>
      ///   </list>
      ///   If last parameter of the Zone is BGR (like $000000, $5F5f5f, BGR)
      ///   then hexadecimal and integer values will be interpreted in BGR
      ///   order ($0000FF for red) .
      /// </remarks>
      property TransparentZones : TGIS_StringList
                                  read FTransparentZones ;

      /// <summary>
      ///   List of zones for mapping in gray. Each entry consists of:
      ///   "start_value,end_value,start_color,end_color".
      ///   e.g., "0,255,$000000,$0000FF" will make grayscale blue.
      /// </summary>
      property GrayMapZones     : TGIS_StringList
                                  read FGrayMapZones ;

      /// <summary>
      ///   List of zones for mapping the red channel. Each entry consist of:
      ///   "start_value,end_value,start_value_mapped,end_value_mapped".
      ///   e.g., "0,255,64,127" will shrink the values.
      /// </summary>
      property RedMapZones      : TGIS_StringList
                                  read FRedMapZones ;

      /// <summary>
      ///   List of zones for mapping the green channel. Each entry consist
      ///   of: "start_value,end_value,start_value_mapped,end_value_mapped".
      ///   e.g., "0,255,64,127" will shrink the values.
      /// </summary>
      property GreenMapZones    : TGIS_StringList
                                  read FGreenMapZones ;

      /// <summary>
      ///   List of zones for mapping the blue channel. Each entry consist
      ///   of: "start_value,end_value,start_value_mapped,end_value_mapped".
      ///   e.g., "0,255,64,127" will shrink the values.
      /// </summary>
      property BlueMapZones     : TGIS_StringList
                                  read FBlueMapZones ;

      /// <summary>
      ///   List of zones for mapping the full color (RGB). Each entry consist
      ///   of: "start_color,end_color, mapped_color". e.g.,
      ///   "$EFEFEF,$FFFFFF,$00FF00" will change white and White almost to
      ///   green.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Colors can be expressed as
      ///   </para>
      ///   <list type="bullet">
      ///     <item>
      ///       hexadecimal RGB color ($0000FF for red)
      ///     </item>
      ///     <item>
      ///       integer RGB color (255 for red )
      ///     </item>
      ///     <item>
      ///       red:green:blue (255:0:0 for red)
      ///     </item>
      ///   </list>
      ///   <para>
      ///     If last parameter of the Zone is BGR (like $000000, $5F5f5f,
      ///     BGR) then hexadecimal and integer values will be interpreted in
      ///     BGR order ($0000FF for red).
      ///   </para>
      /// </remarks>
      property FullRGBMapZones  : TGIS_StringList
                                  read FFullRGBMapZones ;

      /// <summary>
      ///   List of zones for mapping the altitude. Each entry consist of:
      ///   "start_altitude,end_altitude, color". e.g., "100.45,800.12,$FF0000"
      ///   will map altitudes 100.45..800.12 to red.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Colors can be expressed as
      ///   </para>
      ///   <list type="bullet">
      ///     <item>
      ///       hexadecimal RGB color ($0000FF for red)
      ///     </item>
      ///     <item>
      ///       integer RGB color (255 for red )
      ///     </item>
      ///     <item>
      ///       red:green:blue (255:0:0 for red)
      ///     </item>
      ///     <item>
      ///       color names:
      ///       <list type="bullet">
      ///         <item>
      ///           AQUA
      ///         </item>
      ///         <item>
      ///           BLACK
      ///         </item>
      ///         <item>
      ///           BLUE
      ///         </item>
      ///         <item>
      ///           FUCHSIA
      ///         </item>
      ///         <item>
      ///           GRAY
      ///         </item>
      ///         <item>
      ///           GREEN
      ///         </item>
      ///         <item>
      ///           LIME
      ///         </item>
      ///         <item>
      ///           MAROON
      ///         </item>
      ///         <item>
      ///           NAVY
      ///         </item>
      ///         <item>
      ///           OLIVE
      ///         </item>
      ///         <item>
      ///           PURPLE
      ///         </item>
      ///         <item>
      ///           RED
      ///         </item>
      ///         <item>
      ///           SILVER
      ///         </item>
      ///         <item>
      ///           TEAL
      ///         </item>
      ///         <item>
      ///           WHITE
      ///         </item>
      ///         <item>
      ///           YELLOW
      ///         </item>
      ///       </list>
      ///     </item>
      ///   </list>
      ///   <para>
      ///     If last parameter of the Zone is BGR (like $000000, $5F5f5f,
      ///     BGR) then hexadecimal and integer values will be interpreted in
      ///     BGR order ($0000FF for red).
      ///   </para>
      /// </remarks>
      property AltitudeMapZones : TGIS_StringList
                                  read FAltitudeMapZones ;

      /// <summary>
      ///   A color ramp for coloring grid values.
      /// </summary>
      /// <remarks>
      ///   If assigned, AltitudeMapZones is ignored.
      /// </remarks>
      property ColorRamp        : TGIS_ColorMapArray
                                  read  FColorRamp
                                  write fset_ColorRamp ;

  end ;

  /// <summary>
  ///   Parameters for reading properties from fields.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    For internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  TGIS_ParamsField = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FFieldName  : String ;
      FFactor     : Double ;
      FScalable   : Boolean ;
      {$IFDEF DCC} [unsafe] {$ENDIF}
      FParams     : TGIS_ParamsAbstract ;
      FSubType    : String ;


      procedure fset_FieldName( const _value : String  ) ;
      procedure fset_Factor   ( const _value : Double  ) ;
      procedure fset_Scalable ( const _value : Boolean ) ;

    protected

      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy     ; override;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Assign value.
      /// </summary>
      /// <param name="_parent">
      ///   owner of destination parameter; used upon creation of
      ///   a new parameter
      /// </param>
      /// <param name="_dst">
      ///   destination field; will be allocated if required
      /// </param>
      /// <param name="_src">
      ///   source field
      /// </param>
      class procedure assignEx( const _parent : TGIS_ParamsAbstract ;
                                var   _dst    : TGIS_ParamsField    ;
                                const _src    : TGIS_ParamsField
                              ) ;

      /// <summary>
      ///   Read parameters from ini.
      /// </summary>
      /// <param name="_parent">
      ///   owner of destination parameter; used upon creation of a
      ///   new parameter
      /// </param>
      /// <param name="_cfg">
      ///   config file
      /// </param>
      /// <param name="_dst">
      ///   destination field; will be allocated if required
      /// </param>
      /// <param name="_key">
      ///   string key in the config file
      /// </param>
      /// <param name="_def">
      ///   default field definition; used if config value does not exist
      /// </param>
      /// <param name="_astext">
      ///   'astext' field definition; to be emptied if value does exit
      /// </param>
      class procedure readFromConfig(
                                const _parent   : TGIS_ParamsAbstract ;
                                var   _dst      : TGIS_ParamsField    ;
                                const _cfg      : TGIS_ConfigAbstract ;
                                const _key      : String              ;
                                const _def      : TGIS_ParamsField    ;
                                var   _astext   : String
                              ) ;

      /// <summary>
      ///   Write parameters to ini.
      /// </summary>
      /// <param name="_cfg">
      ///   config file
      /// </param>
      /// <param name="_key">
      ///   string key in the config file
      /// </param>
      /// <param name="_value">
      ///   field definition to be written
      /// </param>
      /// <remarks>
      ///   Default value not used because default value for field is
      ///   always empty.
      /// </remarks>
      class procedure writeToConfig (
                                const _cfg    : TGIS_ConfigAbstract ;
                                const _key    : String              ;
                                const _value  : TGIS_ParamsField
                              ) ;

    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_params">
      ///   base parameters
      /// </param>
      constructor  Create     ( const _params : TGIS_ParamsAbstract ) ;

    public

      /// <summary>
      ///   Assign value.
      /// </summary>
      /// <param name="_src">
      ///   source field
      /// </param>
      procedure Assign        ( const _src    : TGIS_ParamsField
                              ) ;

      /// <summary>
      ///   Is field object different from another.
      /// </summary>
      /// <param name="_src">
      ///   field to compare
      /// </param>
      /// <returns>
      ///   False if objects are same.
      /// </returns>
      function  IsDifferent   ( const _src : TGIS_ParamsField
                              ) : Boolean ;

    public

      /// <summary>
      ///   Field name with values.
      /// </summary>
      property Field        : String
                              read  FFieldName
                              write fset_FieldName ;

      /// <summary>
      ///   Factor to multiply field value.
      /// </summary>
      property Factor        : Double
                               read  FFactor
                               write fset_Factor ;

      /// <summary>
      ///   Is value scalable.
      /// </summary>
      property Scalable      : Boolean
                               read  FScalable
                               write fset_Scalable ;
  end ;

  /// <summary>
  ///   Parameters that are common for features like Lines, Areas, Markers,
  ///   Labels.
  /// </summary>
  TGIS_ParamsFeature = {$IFDEF OXYGENE} public {$ENDIF}
                       class ( TGIS_ParamsVector )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FShowLegend           : Boolean     ;
      FSmartSize            : Integer     ;
      FSmartSizeAsText      : String      ;
      FSmartSizeInternal    : TGIS_ParamsField ;
      FColor                : TGIS_Color  ;
      FColorInternal        : TGIS_ParamsField ;
      FBitmap               : TGIS_Bitmap ;
      FPattern              : TGIS_BrushStyle  ;
      FOutlineStyle         : TGIS_PenStyle    ;
      FOutlineStyleAsText   : String      ;
      FOutlineStyleInternal : TGIS_ParamsField ;
      FOutlineWidth         : Integer     ;
      FOutlineWidthAsText   : String      ;
      FOutlineWidthInternal : TGIS_ParamsField ;
      FOutlineColor         : TGIS_Color  ;
      FOutlineBackcolor     : TGIS_Color  ;
      FOutlineColorInternal : TGIS_ParamsField ;
      FOutlineBitmap        : TGIS_Bitmap ;
      FOutlinePattern       : TGIS_BrushStyle  ;
      FOffsetX              : Integer     ;
      FOffsetXAsText        : String      ;
      FOffsetXInternal      : TGIS_ParamsField ;
      FOffsetY              : Integer     ;
      FOffsetYAsText        : String      ;
      FOffsetYInternal      : TGIS_ParamsField ;
      FOffsetPosition       : TGIS_OffsetPosition ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      procedure fset_ShowLegend          ( const _value : Boolean             );
      procedure fset_SmartSize           ( const _value : Integer             );
      function  fget_Color               : TGIS_Color ;
      procedure fset_Color               ( const _value : TGIS_Color          );
      function  fget_OutlineColor        : TGIS_Color ;
      procedure fset_OutlineColor        ( const _value : TGIS_Color          );
      procedure fset_OutlineBackcolor    ( const _value : TGIS_Color          );
      function  fget_Bitmap              : TGIS_Bitmap ;
      procedure fset_Bitmap              ( const _value : TGIS_Bitmap         );
      procedure fset_Pattern             ( const _value : TGIS_BrushStyle     );
      function  fget_OutlineWidth        : Integer ;
      function  fget_OutlineStyle        : TGIS_PenStyle ;
      procedure fset_OutlineStyle        ( const _value : TGIS_PenStyle       );
      procedure fset_OutlineWidth        ( const _value : Integer             );
      function  fget_OutlineBitmap       : TGIS_Bitmap ;
      procedure fset_OutlineBitmap       ( const _value : TGIS_Bitmap         );
      procedure fset_OutlinePattern      ( const _value : TGIS_BrushStyle     );

      function  fget_ColorAsText         : String ;
      procedure fset_ColorAsText         ( const _value : String              );
      function  fget_PatternAsText       : String ;
                                         virtual;
      procedure fset_PatternAsText       ( const _value : String              );
                                         virtual;
      function  fget_OutlineStyleAsText  : String ;
                                         virtual;
      procedure fset_OutlineStyleAsText  ( const _value : String              );
                                         virtual;
      function  fget_OutlinePatternAsText: String ;
                                         virtual;
      procedure fset_OutlinePatternAsText( const _value : String              );
                                         virtual;
      function  fget_OutlineColorAsText  : String ;
      procedure fset_OutlineColorAsText  ( const _value : String              );
      function  fget_OutlineWidthAsText  : String ;
      procedure fset_OutlineWidthAsText  ( const _value : String              );
      function  fget_SmartSizeAsText     : String ;
      procedure fset_SmartSizeAsText     ( const _value : String              );
      function  fget_OffsetX             : Integer ;
      procedure fset_OffsetX             ( const _value : Integer             );
      function  fget_OffsetXAsText       : String ;
      procedure fset_OffsetXAsText       ( const _value : String              );
      function  fget_OffsetY             : Integer ;
      procedure fset_OffsetY             ( const _value : Integer             );
      function  fget_OffsetYAsText       : String ;
      procedure fset_OffsetYAsText       ( const _value : String              );
      function  fget_OffsetPosition      : TGIS_OffsetPosition ;
      procedure fset_OffsetPosition      ( const _value : TGIS_OffsetPosition );
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure assignInternal       (      _source : TPersistent ) ; override;

      /// <summary>
      ///   Assign bitmap from _values object.
      /// </summary>
      /// <param name="_value">
      ///   object from which the bitmap will be assigned
      /// </param>
      procedure assignBitmap         ( const _value : TGIS_Bitmap ) ;

      /// <summary>
      ///   Assign outline bitmap from _values object.
      /// </summary>
      /// <param name="_value">
      ///   object from which the outline bitmap will be assigned
      /// </param>
      procedure assignOutlineBitmap  ( const _value : TGIS_Bitmap ) ;

    protected
      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy            ; override;

    public

      /// <inheritdoc/>
      constructor Create             ; override;

      /// <inheritdoc/>
      procedure Assign               (      _source : TPersistent ) ; override;

    public
      /// <summary>
      ///   Helper for safe scaling size feature even if feature is
      ///   auto-scaled.
      /// </summary>
      /// <param name="_size">
      ///   value to be scaled
      /// </param>
      /// <param name="_factor">
      ///   scaled factor
      /// </param>
      /// <returns>
      ///   Scaled value. Information about auto-scale will persist as well as
      ///   rendering markers.
      /// </returns>
      class function SizeHelper      ( const   _size   : Integer  ;
                                       const   _factor : Double
                                     ) : Integer ;
    public

      /// <summary>
      ///   Show feature in legend component?
      /// </summary>
      property ShowLegend            : Boolean
                                       read  FShowLegend
                                       write fset_ShowLegend   ;

      /// <summary>
      ///   Default minimum bounding box size which is to be visible (&gt;0
      ///   in twips, &lt;0 in pixels). If a shape in current zoom is
      ///   smaller, then the shape will not be visible. Used only if
      ///   SmartSizeField is not set. The value is read from main section
      ///   only.
      /// </summary>
      property SmartSize              : Integer
                                        read  FSmartSize
                                        write fset_SmartSize ;

      /// <summary>
      ///   Smart Size. Grouping as a text property all properties that
      ///   affect Smart Size presentation like SmartSize or SmartSizeField and
      ///   represent sizes in a human readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SmartSizeAsText : String
                                read  fget_SmartSizeAsText
                                write fset_SmartSizeAsText ;
      /// <summary>
      ///   Interior color.
      /// </summary>
      property Color                 : TGIS_Color
                                       read  fget_Color
                                       write fset_Color ;


      /// <summary>
      ///   Color. Grouping as a text property all properties that
      ///   affect Color presentation like Color or ColorEx.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property ColorAsText           : String
                                       read  fget_ColorAsText
                                       write fset_ColorAsText ;

      /// <summary>
      ///   Interior bitmap. Assigning a new value will destroy the existing
      ///   one and call Assign for a new value on a newly created object.
      /// </summary>
      {#ownership:set:release}
      property Bitmap                : TGIS_Bitmap
                                       read  fget_Bitmap
                                       write fset_Bitmap ;

      /// <summary>
      ///   Interior pattern.
      /// </summary>
      property Pattern               : TGIS_BrushStyle
                                       read  FPattern
                                       write fset_Pattern ;

      /// <summary>
      ///   Pattern. Grouping as a text property all properties that
      ///   affect Outline presentation like Symbol, Bitmap or Style.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property PatternAsText         : String
                                       read  fget_PatternAsText
                                       write fset_PatternAsText ;

      /// <summary>
      ///   Outline style.
      /// </summary>
      property OutlineStyle          : TGIS_PenStyle
                                       read  fget_OutlineStyle
                                       write fset_OutlineStyle ;

      /// <summary>
      ///   OutlineStyle. Grouping as a text property all properties that
      ///   affect OutlineStyle presentation like OutlineSymbol or
      ///   OutlineStyle.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property OutlineStyleAsText    : String
                                       read  fget_OutlineStyleAsText
                                       write fset_OutlineStyleAsText ;

      /// <summary>
      ///   Outline width.
      /// </summary>
      property OutlineWidth          : Integer
                                       read  fget_OutlineWidth
                                       write fset_OutlineWidth ;

      /// <summary>
      ///   OutlineWidth. Grouping as a text property all properties that
      ///   affect Width presentation like OutlineWidth or OutlineWidthEx
      ///   and represent sizes in a human readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property OutlineWidthAsText    : String
                                       read  fget_OutlineWidthAsText
                                       write fset_OutlineWidthAsText ;

      /// <summary>
      ///   Outline color.
      /// </summary>
      property OutlineColor          : TGIS_Color
                                       read  fget_OutlineColor
                                       write fset_OutlineColor ;

      /// <summary>
      ///   OutlineColor. Grouping as a text property all properties that
      ///   affect Color presentation like OutlineColor or OutlineColorEx.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property OutlineColorAsText    : String
                                       read fget_OutlineColorAsText
                                       write fset_OutlineColorAsText ;

      /// <summary>
      ///   Outline backcolor; used for dash styles.
      /// </summary>
      property OutlineBackcolor      : TGIS_Color
                                       read  FOutlineBackcolor
                                       write fset_OutlineBackcolor ;

      /// <summary>
      ///   Outline bitmap. Assigning a new value will cause destroying
      ///   existing one and calling Assign for a new value on a newly
      ///   created object.
      /// </summary>
//      ddd
      property OutlineBitmap         : TGIS_Bitmap
                                       read  fget_OutlineBitmap
                                       write fset_OutlineBitmap  ;

      /// <summary>
      ///   Outline pattern.
      /// </summary>
      property OutlinePattern        : TGIS_BrushStyle
                                       read  FOutlinePattern
                                       write fset_OutlinePattern ;

      /// <summary>
      ///   Outline pattern. Grouping as a text property all properties that
      ///   affect Pattern presentation like OutlineSymbol, OutlineBitmap or
      ///   OutlineStyle.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property OutlinePatternAsText  : String
                                       read  fget_OutlinePatternAsText
                                       write fset_OutlinePatternAsText ;

      /// <summary>
      ///   Offset for drawing a shape.
      /// </summary>
      property OffsetX          : Integer
                                       read  fget_OffsetX
                                       write fset_OffsetX ;

      /// <summary>
      ///   Offset for drawing a shape. Grouping as a text property all properties that
      ///   affect Offset and represent sizes in a human readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property OffsetXAsText    : String
                                       read  fget_OffsetXAsText
                                       write fset_OffsetXAsText ;

      /// <summary>
      ///   Offset for drawing a shape.
      /// </summary>
      property OffsetY          : Integer
                                       read  fget_OffsetY
                                       write fset_OffsetY ;

      /// <summary>
      ///   Offset for drawing a shape. Grouping as a text property all properties that
      ///   affect Offset and represent sizes in a human readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property OffsetYAsText    : String
                                       read  fget_OffsetYAsText
                                       write fset_OffsetYAsText ;

      /// <summary>
      ///   Offset position to assign positive or negative sign for OffsetX and OffsetY.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property OffsetPosition   : TGIS_OffsetPosition
                                       read  fget_OffsetPosition
                                       write fset_OffsetPosition ;

  end ;

  /// <summary>
  ///   Parameters that are common for Lines.
  /// </summary>
  TGIS_ParamsLine = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_ParamsFeature )

    private
      FStyle                : TGIS_PenStyle       ;
      FStyleAsText          : String              ;
      FStyleInternal        : TGIS_ParamsField    ;
      FSymbol               : TGIS_SymbolAbstract ;
      FWidth                : Integer             ;
      FWidthAsText          : String              ;
      FWidthInternal        : TGIS_ParamsField    ;
      FSymbolGap            : Integer             ;
      FSymbolGapAsText      : String              ;
      FSymbolRotate         : Double              ;
      FSymbolRotateAsText   : String              ;
      FSymbolRotateInternal : TGIS_ParamsField    ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      procedure fset_Style               ( const _value : TGIS_PenStyle       );
      function  fget_Style               : TGIS_PenStyle ;
      procedure fset_Symbol              ( const _value : TGIS_SymbolAbstract );
      function  fget_Width               : Integer ;
      procedure fset_Width               ( const _value : Integer             );
      procedure fset_SymbolGap           ( const _value : Integer             );
      function  fget_SymbolRotate        : Double ;
      procedure fset_SymbolRotate        ( const _value : Double              );

      function  fget_StyleAsText         : String ;
      procedure fset_StyleAsText         ( const _value : String              );
      function  fget_WidthAsText         : String ;
      procedure fset_WidthAsText         ( const _value : String              );
      function  fget_SymbolGapAsText     : String ;
      procedure fset_SymbolGapAsText     ( const _value : String              );
      function  fget_SymbolRotateAsText  : String ;
      procedure fset_SymbolRotateAsText  ( const _value : String              );

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Prepare parameters object. If scope is Shape (FShape is assigned)
      ///   then a copy of object will be created on the shape.
      /// </summary>
      function  prepareParams   : TGIS_ParamsVector ; override;

      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure assignInternal  ( _source       : TPersistent   ) ; override;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy       ; override;

    public

      /// <summary>
      ///   Create an instance with default values. Default: 1 twip width,
      ///   black, no outline.
      /// </summary>
      constructor  Create       ; override;

      /// <inheritdoc/>
      procedure Assign          ( _source        : TPersistent
                                ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig  ( const _cfg     : TGIS_ConfigAbstract
                                ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig    ( const _cfg     : TGIS_ConfigAbstract
                                ) ; override;

    public

      /// <summary>
      ///   Interior style.
      /// </summary>
      property Style            : TGIS_PenStyle
                                  read  fget_Style
                                  write fset_Style ;

      /// <summary>
      ///   OutlineStyle. Grouping as a text property all properties that
      ///   affect OutlineStyle presentation like OutlineSymbol or
      ///   OutlineStyle.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property StyleAsText      : String
                                  read  fget_StyleAsText
                                  write fset_StyleAsText ;


      /// <summary>
      ///   Symbol used for drawing a line.
      /// </summary>
      {#ownership:set:release}
      property Symbol           : TGIS_SymbolAbstract
                                  read  FSymbol
                                  write fset_Symbol ;

      /// <summary>
      ///   Symbol gap (spacing) used for drawing a line.
      /// </summary>
      property SymbolGap        : Integer
                                  read  FSymbolGap
                                  write fset_SymbolGap ;

      /// <summary>
      ///   SymbolGap. Grouping as a text property all properties that
      ///   affect SymbolGap presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SymbolGapAsText  : String
                                  read  fget_SymbolGapAsText
                                  write fset_SymbolGapAsText ;

      /// <summary>
      ///   Symbol rotation, in radians, used for drawing a line.
      /// </summary>
      property SymbolRotate     : Double
                                  read  fget_SymbolRotate
                                  write fset_SymbolRotate ;

      /// <summary>
      ///   SymbolRotate. Grouping as a text property all properties that
      ///   affect SymbolRotate presentation like SymbolRotate or SymbolRotateEx
      ///   and represent rotation in a human readable form like '45deg',
      ///   '0.5rad'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SymbolRotateAsText
                                : String
                                  read  fget_SymbolRotateAsText
                                  write fset_SymbolRotateAsText ;

      /// <summary>
      ///   Interior width.
      /// </summary>
      property Width            : Integer
                                  read  fget_Width
                                  write fset_Width ;

      /// <summary>
      ///   Width. Grouping as a text property all properties that affect
      ///   Width presentation like Width or  and represent sizes in a
      ///   human readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property WidthAsText      : String
                                  read  fget_WidthAsText
                                  write fset_WidthAsText ;
  end ;

  /// <summary>
  ///   Parameters that are common for Areas.
  /// </summary>
  TGIS_ParamsArea = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_ParamsFeature )

    private
      FSymbol                    : TGIS_SymbolAbstract ;
      FSymbolSize                : Integer             ;
      FSymbolSizeAsText          : String             ;
      FSymbolRotate              : Double              ;
      FSymbolRotateAsText        : String             ;
      FSymbolRotateInternal      : TGIS_ParamsField    ;
      FSymbolGap                 : Integer             ;
      FSymbolGapAsText           : String             ;
      FOutlineSymbol             : TGIS_SymbolAbstract ;
      FOutlineSymbolGap          : Integer             ;
      FOutlineSymbolGapAsText    : String             ;
      FOutlineSymbolRotate       : Double              ;
      FOutlineSymbolRotateAsText : String             ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      procedure fset_Symbol              ( const _value : TGIS_SymbolAbstract );
      procedure fset_SymbolSize          ( const _value : Integer             );
      function  fget_SymbolRotate        : Double ;
      procedure fset_SymbolRotate        ( const _value : Double              );
      procedure fset_SymbolGap           ( const _value : Integer             );
      procedure fset_OutlineSymbol       ( const _value : TGIS_SymbolAbstract );
      procedure fset_OutlineSymbolGap    ( const _value : Integer             );
      procedure fset_OutlineSymbolRotate ( const _value : Double              );

      function  fget_PatternAsText       : String ;
                                         override;
      procedure fset_PatternAsText       ( const _value : String              );
                                         override;
      function  fget_OutlineStyleAsText  : String ;
                                         override;
      procedure fset_OutlineStyleAsText  ( const _value : String              );
                                         override;
      function  fget_OutlinePatternAsText: String ;
                                         override;
      procedure fset_OutlinePatternAsText( const _value : String              );
                                         override;
      function  fget_SymbolSizeAsText    : String ;
      procedure fset_SymbolSizeAsText    ( const _value : String              );
      function  fget_SymbolGapAsText     : String ;
      procedure fset_SymbolGapAsText     ( const _value : String              );
      function  fget_SymbolRotateAsText  : String ;
      procedure fset_SymbolRotateAsText  ( const _value : String              );
      function  fget_OutlineSymbolGapAsText
                                         : String ;
      procedure fset_OutlineSymbolGapAsText(
                                           const _value : String              );
      function  fget_OutlineSymbolRotateAsText
                                         : String ;
      procedure fset_OutlineSymbolRotateAsText(
                                           const _value : String              );

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Prepare parameters object. If scope is Shape (FShape is assigned)
      ///   then a copy of object will be created on the shape.
      /// </summary>
      function  prepareParams           : TGIS_ParamsVector ; override;

      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure assignInternal          ( _source : TPersistent ) ; override;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy     ; override;

    public

      /// <summary>
      ///   Create an instance with default values. Default: gray fill, outline
      ///   black, 1 twips width.
      /// </summary>
      constructor Create      ; override;

      /// <inheritdoc/>
      procedure Assign        ( _source    : TPersistent
                              ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig( const _cfg : TGIS_ConfigAbstract
                              ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig  ( const _cfg : TGIS_ConfigAbstract
                              ) ; override;

    public
      /// <summary>
      ///   Symbol used for drawing an interior.
      /// </summary>
      {#ownership:set:release}
      property Symbol                    : TGIS_SymbolAbstract
                                           read FSymbol
                                           write fset_Symbol ;

      /// <summary>
      ///   Symbol size used for drawing an interior.
      /// </summary>
      property SymbolSize                : Integer
                                           read FSymbolSize
                                           write fset_SymbolSize ;

      /// <summary>
      ///   SymbolSize. Grouping as a text property all properties that
      ///   affect SymbolSize presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SymbolSizeAsText          : String
                                           read  fget_SymbolSizeAsText
                                           write fset_SymbolSizeAsText ;

      /// <summary>
      ///   Symbol gap (spacing) used for drawing an interior.
      /// </summary>
      property SymbolGap                 : Integer
                                           read FSymbolGap
                                           write fset_SymbolGap ;

      /// <summary>
      ///   SymbolGap. Grouping as a text property all properties that
      ///   affect SymbolGap presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SymbolGapAsText           : String
                                           read  fget_SymbolGapAsText
                                           write fset_SymbolGapAsText ;

      /// <summary>
      ///   Symbol rotation, in radians, used for drawing an interior.
      /// </summary>
      property SymbolRotate              : Double
                                           read  fget_SymbolRotate
                                           write fset_SymbolRotate ;

      /// <summary>
      ///   SymbolRotate. Grouping as a text property all properties that
      ///   affect SymbolRotate presentation like SymbolRotate or SymbolRotateEx
      ///   and represent rotation in a human readable form like '45deg',
      ///   '0.5rad'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SymbolRotateAsText        : String
                                           read  fget_SymbolRotateAsText
                                           write fset_SymbolRotateAsText ;

      /// <summary>
      ///   Symbol used for drawing an outline.
      /// </summary>
      property OutlineSymbol             : TGIS_SymbolAbstract
                                           read  FOutlineSymbol
                                           write fset_OutlineSymbol ;

      /// <summary>
      ///   Symbol gap (spacing) used for drawing an outline.
      /// </summary>
      property OutlineSymbolGap          : Integer
                                           read  FOutlineSymbolGap
                                           write fset_OutlineSymbolGap ;

      /// <summary>
      ///   OutlineSymbolGap. Grouping as a text property all properties that
      ///   affect OutlineSymbolGap presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property OutlineSymbolGapAsText    : String
                                           read  fget_OutlineSymbolGapAsText
                                           write fset_OutlineSymbolGapAsText ;

      /// <summary>
      ///   Symbol rotation, in radians, used for drawing an outline.
      /// </summary>
      property OutlineSymbolRotate       : Double
                                           read  FOutlineSymbolRotate
                                           write fset_OutlineSymbolRotate ;


      /// <summary>
      ///   OutlineSymbolRotate. Grouping as a text property all properties that
      ///   affect SymbolSize presentation like OutlineSymbolRotate
      ///   or OutlineSymbolRotateEx and represent rotation in a human readable
      ///   form like '45deg', '0.5rad'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property OutlineSymbolRotateAsText : String
                                           read fget_OutlineSymbolRotateAsText
                                           write fset_OutlineSymbolRotateAsText ;

  end ;

  /// <summary>
  ///   Parameters that are common for Markers.
  /// </summary>
  TGIS_ParamsMarker = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_ParamsFeature )

    private
      FStyle                : TGIS_MarkerStyle    ;
      FSymbol               : TGIS_SymbolAbstract ;
      FSymbolRotate         : Double              ;
      FSymbolRotateAsText   : String              ;
      FSymbolRotateInternal : TGIS_ParamsField    ;
      FSize                 : Integer             ;
      FSizeAsText           : String              ;
      FSizeInternal         : TGIS_ParamsField    ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      procedure fset_Symbol              ( const _value : TGIS_SymbolAbstract );
      procedure fset_Style               ( const _value : TGIS_MarkerStyle    );
      function  fget_SymbolRotate        : Double ;
      procedure fset_SymbolRotate        ( const _value : Double              );
      function  fget_SymbolRotateIndirect: Boolean ;
      function  fget_Size                : Integer ;
      procedure fset_Size                ( const _value : Integer             );

      function  fget_StyleAsText         : String ;
      procedure fset_StyleAsText         ( const _value : String              );
      function  fget_SizeAsText          : String ;
      procedure fset_SizeAsText          ( const _value : String              );
      function  fget_SymbolRotateAsText  : String ;
      procedure fset_SymbolRotateAsText  ( const _value : String              );

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Prepare parameters object. If scope is Shape (FShape is assigned)
      ///   then a copy of object will be created on the shape.
      /// </summary>
      function  prepareParams     : TGIS_ParamsVector ; override;

      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure assignInternal    ( _source : TPersistent ) ; override;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy     ; override;

    public

      /// <summary>
      ///   Create an instance with default values. Default: red gmsBox, size
      ///   150 twips, no outline.
      /// </summary>
      constructor Create      ; override;

      /// <inheritdoc/>
      procedure Assign        ( _source    : TPersistent
                              ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig( const _cfg : TGIS_ConfigAbstract
                              ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig  ( const _cfg : TGIS_ConfigAbstract
                              ) ; override;

    public

      /// <summary>
      ///   Marker style.
      /// </summary>
      property Style          : TGIS_MarkerStyle
                                read  FStyle
                                write fset_Style ;

      /// <summary>
      ///   Style. Grouping as a text property all properties that
      ///   affect Style presentation like Symbol or Style.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property StyleAsText    : String
                                read  fget_StyleAsText
                                write fset_StyleAsText ;

      /// <summary>
      ///   Symbol used for drawing a marker.
      /// </summary>
      property Symbol         : TGIS_SymbolAbstract
                                read  FSymbol
                                write fset_Symbol ;

      /// <summary>
      ///   Symbol rotation in radians used for drawing a marker.
      /// </summary>
      property SymbolRotate   : Double
                                read  fget_SymbolRotate
                                write fset_SymbolRotate ;

      /// <summary>
      ///   True if marker is rotated by attribute field.
      /// </summary>
      /// <remarks>
      ///   Required by TGS_LayerVector.SymbolingMode.
      /// </remarks>
      property SymbolRotateIndirect
                               : Boolean
                                 read  fget_SymbolRotateIndirect ;

      /// <summary>
      ///   SymbolRotate. Grouping as a text property all properties that
      ///   affect SymbolSize presentation like SymbolRotate or SymbolRotateEx
      ///   and represent rotation in a human readable form like '45deg',
      ///   '0.5rad'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SymbolRotateAsText
                              : String
                                read  fget_SymbolRotateAsText
                                write fset_SymbolRotateAsText ;

      /// <summary>
      ///   Marker size.
      /// </summary>
      property Size           : Integer
                                read  fget_Size
                                write fset_Size ;

      /// <summary>
      ///   Size. Grouping as a text property all properties that
      ///   affect Size presentation like Size or SizeEx and represent sizes
      ///   in a human readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SizeAsText     : String
                                read  fget_SizeAsText
                                write fset_SizeAsText ;

  end ;


  /// <summary>
  ///   Helper object for deprecated TGIS_ParamsLabel.Font property.
  /// </summary>
  /// <remarks>
  ///   Setting value of this object set TGIS_ParamsLabelFontName, FontSize and
  ///   other properties respectively.
  /// </remarks>
  TGIS_ParamsLabelFont = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      {$IFDEF DCC} [unsafe] {$ENDIF}
      FParent : TObject ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      function  fget_name  : String ;
      procedure fset_name  ( const _value  : String         ) ;
      function  fget_size  : Integer ;
      procedure fset_size  ( const _value  : Integer         ) ;
      function  fget_style : TGIS_FontStyles ;
      procedure fset_style ( const _value  : TGIS_FontStyles ) ;
      function  fget_color : TGIS_Color ;
      procedure fset_color ( const _value  : TGIS_Color      ) ;

    public
      /// <summary>
      ///   Convert points to font size.
      /// </summary>
      /// <param name="_value">
      ///   value in points
      /// </param>
      /// <returns>
      ///   Font size in internal units.
      /// </returns>
      class function PtToFontSize( const _value : Integer ) : Integer ;

      /// <summary>
      ///   Convert font size in internal units to points.
      /// </summary>
      /// <param name="_value">
      ///   font size
      /// </param>
      /// <returns>
      ///   Font size expressed in points.
      /// </returns>
      class function FontSizeToPt( const _value : Integer ) : Integer ;
    public

      /// <summary>
      ///   Assign value form font object
      /// </summary>
      /// <param name="_source">
      ///   source font object
      /// </param>
      procedure Assign     ( const _source :  TGIS_Font      ) ;

    public

      /// <summary>
      ///   Set/Get TGIS_ParamsLabel.FontName.
      /// </summary>
      property Name   : String          read fget_name  write fset_name  ;

      /// <summary>
      ///   Set/Get TGIS_ParamsLabel.FontSize with converting from point (as
      ///   expected by TGIS_Font.Size) to twips (as expected by
      ///   TGIS_ParamsLabel.FontSize).
      /// </summary>
      property Size   : Integer         read fget_size  write fset_size  ;

      /// <summary>
      ///   Set/Get TGIS_ParamsLabel.FontStyle
      /// </summary>
      property Style  : TGIS_FontStyles read fget_style write fset_style ;

      /// <summary>
      ///   Set/Get TGIS_ParamsLabel.FontColor.
      /// </summary>
      property Color  : TGIS_Color      read fget_color write fset_color ;
  end ;

  /// <summary>
  ///   Parameters that are common for Labels.
  /// </summary>
  TGIS_ParamsLabel = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_ParamsFeature )

    private
      FVisible           : Boolean ;
      FAllocator         : Boolean ;
      FDuplicates        : Boolean ;
      FField             : String ;
      FValue             : String ;
      FAlignment         : TGIS_LabelAlignment ;
      FPosition          : TGIS_LabelPositions ;
      FPositionInternal  : TGIS_ParamsField ;
      FPositionAsText    : String ;
      FFont              : TGIS_ParamsLabelFont ;
      FFontName          : String ;
      FFontStyle         : TGIS_FontStyles ;
      FFontSize          : Integer ;
      FFontSizeInternal  : TGIS_ParamsField ;
      FFontSizeAsText    : String ;
      FFontColor         : TGIS_Color ;
      FFontColorInternal : TGIS_ParamsField ;

      FHeight            : Integer ;
      FHeightAsText      : String ;
      FWidth             : Integer ;
      FWidthAsText       : String ;
      FRotate            : Double  ;
      FRotateAsText      : String ;
      FRotateInternal    : TGIS_ParamsField  ;

      FShield            : TGIS_SymbolAbstract ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      procedure fset_Visible        ( const _value : Boolean              );
      procedure fset_Allocator      ( const _value : Boolean              );
      procedure fset_Duplicates     ( const _value : Boolean              );
      procedure fset_Field          ( const _value : String               );
      procedure fset_Value          ( const _value : String               );
      procedure fset_Alignment      ( const _value : TGIS_LabelAlignment  );
      function  fget_Position       : TGIS_LabelPositions                  ;
      procedure fset_Position       ( const _value : TGIS_LabelPositions  );
      function  fget_Font           : TGIS_ParamsLabelFont                 ;
      procedure fset_Height         ( const _value : Integer              );
      procedure fset_Width          ( const _value : Integer              );
      function  fget_Rotate         : Double                               ;
      procedure fset_Rotate         ( const _value : Double               );

      function  fget_FontName       : String                              ;
      procedure fset_FontName       ( const _value : String               );
      function  fget_FontSize       : Integer                              ;
      procedure fset_FontSize       ( const _value : Integer              );
      function  fget_FontStyle      : TGIS_FontStyles                      ;
      procedure fset_FontStyle      ( const _value : TGIS_FontStyles      );
      function  fget_FontColor      : TGIS_Color                          ;
      procedure fset_FontColor      ( const _value : TGIS_Color           );

      function  fget_PositionAsText : String ;
      procedure fset_PositionAsText ( const _value : String               );
      function  fget_FontColorAsText: String ;
      procedure fset_FontColorAsText( const _value : String               );
      function  fget_FontSizeAsText : String ;
      procedure fset_FontSizeAsText ( const _value : String               );
      function  fget_HeightAsText   : String ;
      procedure fset_HeightAsText   ( const _value : String               );
      function  fget_WidthAsText    : String ;
      procedure fset_WidthAsText    ( const _value : String               );
      function  fget_RotateAsText   : String ;
      procedure fset_RotateAsText   ( const _value : String               );
      function  fget_RotateIndirect : Boolean ;
      procedure fset_Shield         ( const _value : TGIS_SymbolAbstract  );
      function  fget_ShieldAsText   : String ;
      procedure fset_ShieldAsText   ( const _value : String               );

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Prepare parameters object. If scope is Shape (FShape is assigned)
      ///   then a copy of object will be created on the shape.
      /// </summary>
      function  prepareParams     : TGIS_ParamsVector ; override;

      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure assignInternal    ( _source    : TPersistent ) ; override;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy     ; override;

    public
      /// <summary>
      ///   Create an instance with default values. Default: yellow background,
      ///   outline of 1 twip, black, Arial 8pt, single line, located
      ///   glpUpRight, maximum size of 100x800 twips.
      /// </summary>
      constructor Create      ; override;

      /// <inheritdoc/>
      procedure Assign        ( _source    : TPersistent
                              ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig( const _cfg : TGIS_ConfigAbstract
                              ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig  ( const _cfg : TGIS_ConfigAbstract
                              ) ; override;

    public

      /// <summary>
      ///   If False, then label will not be visible.
      /// </summary>
      property Visible        : Boolean
                                read  FVisible
                                write fset_Visible ;

      /// <summary>
      ///   Use allocator to avoid overlapping.
      /// </summary>
      property Allocator      : Boolean
                                read  FAllocator
                                write fset_Allocator ;

      /// <summary>
      ///   If False then duplicated label will not be visible.
      /// </summary>
      property Duplicates     : Boolean
                                read  FDuplicates
                                write fset_Duplicates ;

      /// <summary>
      ///   Database field from which a label can be retrieved.
      /// </summary>
      property Field          : String
                                read  FField
                                write fset_Field ;

      /// <summary>
      ///   Label format string in a form '{FIELD_NAME} html text'.
      /// </summary>
      property Value          : String
                                read  FValue
                                write fset_Value ;

      /// <summary>
      ///   Alignment of text within a label.
      /// </summary>
      property Alignment      : TGIS_LabelAlignment
                                read  FAlignment
                                write fset_Alignment ;

      /// <summary>
      ///   Position of Label.
      /// </summary>
      property Position       : TGIS_LabelPositions
                                read  fget_Position
                                write fset_Position ;

      /// <summary>
      ///   Position. Grouping as a text property all properties that
      ///   affect Position presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property PositionAsText : String
                                read  fget_PositionAsText
                                write fset_PositionAsText ;

      /// <summary>
      ///   Font for label. Assigning a new value will destroy existing one and
      ///   call Assign for a new value on a newly created object.
      /// </summary>
      /// <remarks>
      ///   <note type="important">
      ///     Property is deprecated use FontName, FontSize, FontColor and
      ///     FontStyle instead.
      ///   </note>
      /// </remarks>
      property Font           : TGIS_ParamsLabelFont
                                read  fget_Font ;

      /// <summary>
      ///   Font name for label.
      /// </summary>
      property FontName       : String
                                read  fget_FontName
                                write fset_FontName ;

      /// <summary>
      ///   Font size for label.
      /// </summary>
      property FontSize       : Integer
                                read  fget_FontSize
                                write fset_FontSize ;

      /// <summary>
      ///   FontSize. Grouping as a text property all properties that
      ///   affect Font.Size presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property FontSizeAsText   : String
                                read  fget_FontSizeAsText
                                write fset_FontSizeAsText ;

      /// <summary>
      ///   Font style for label.
      /// </summary>
      property FontStyle      : TGIS_FontStyles
                                read  fget_FontStyle
                                write fset_FontStyle ;

      /// <summary>
      ///   Font color for label.
      /// </summary>
      property FontColor      : TGIS_Color
                                read  fget_FontColor
                                write fset_FontColor ;

      /// <summary>
      ///   Font.Color. Grouping as a text property all properties that
      ///   affect Font.Color presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property FontColorAsText : String
                                 read  fget_FontColorAsText
                                 write fset_FontColorAsText ;

      /// <summary>
      ///   Maximum height of label.
      /// </summary>
      property Height         : Integer
                                read  FHeight
                                write fset_Height ;

      /// <summary>
      ///   Height. Grouping as a text property all properties that
      ///   affect Height presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property HeightAsText   : String
                                read  fget_HeightAsText
                                write fset_HeightAsText ;

      /// <summary>
      ///   Maximum width of label.
      /// </summary>
      property Width          : Integer
                                read  FWidth
                                write fset_Width ;

      /// <summary>
      ///   Width. Grouping as a text property all properties that
      ///   affect Width presentation and represent sizes in a human
      ///   readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property WidthAsText    : String
                                read  fget_WidthAsText
                                write fset_WidthAsText ;

      /// <summary>
      ///   Rotation angle in radians.
      /// </summary>
      property Rotate         : Double
                                read  fget_Rotate
                                write fset_Rotate ;

      /// <summary>
      ///   Rotate. Grouping as a text property all properties that
      ///   affect Rotate presentation like Rotate or RotateEx
      ///   and represent rotation in a human readable form like '45deg',
      ///   '0.5rad'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property RotateAsText   : String
                                read  fget_RotateAsText
                                write fset_RotateAsText ;

      /// <summary>
      ///   True if label is rotated by attribute field.
      /// </summary>
      /// <remarks>
      ///   Required by TGS_LayerVector.LabelingMode.
      /// </remarks>
      property RotateIndirect : Boolean
                                read  fget_RotateIndirect ;

      /// <summary>
      ///   Label shield.
      /// </summary>
      {#ownership:set:release}
      property Shield         : TGIS_SymbolAbstract
                                read  FShield
                                write fset_Shield ;

      /// <summary>
      ///   Label shield. Grouping as a text property all properties that
      ///   affect Label.Shield presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property ShieldAsText   : String
                                read  fget_ShieldAsText
                                write fset_ShieldAsText ;
  end ;

  /// <summary>
  ///   Parameters that are common for Charts.
  /// </summary>
  TGIS_ParamsChart = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_ParamsFeature )

    private
      FStyle          : TGIS_ChartStyle  ;
      FSize           : Integer          ;
      FSizeAsText     : String           ;
      FLegend         : String           ;

    public

        /// <summary>
        ///   Internal values storage.
        /// </summary>
        /// <remarks>
        ///   <note type="note">
        ///    Only for internal use of TatukGIS.
        ///    </note>
        /// </remarks>
        ValuesInternal  : TGIS_DoubleArray  ;

        /// <summary>
        ///   Internal color storage.
        /// </summary>
        /// <remarks>
        ///   <note type="note">
        ///    Only for internal use of TatukGIS.
        ///    </note>
        /// </remarks>
        ColorsInternal  : TGIS_ColorArray ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      procedure fset_Style      ( const _value : TGIS_ChartStyle  ) ;
      procedure fset_Size       ( const _value : Integer          ) ;
      function  fget_Values     : String                            ;
      procedure fset_Values     ( const _value : String           ) ;
      function  fget_Colors     : String                            ;
      procedure fset_Colors     ( const _value : String           ) ;
      procedure fset_Legend     ( const _value : String           ) ;
      function  fget_SizeAsText : String ;
      procedure fset_SizeAsText ( const _value : String           ) ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Prepare parameters object. If scope is Shape (FShape is assigned)
      ///   then a copy of object will be created on the shape.
      /// </summary>
      function  prepareParams   : TGIS_ParamsVector ; override;

      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure assignInternal  ( _source       : TPersistent   ) ; override;

    public

      /// <summary>
      ///   Create an instance with default values. Default: size 0, type PIE.
      /// </summary>
      constructor Create  ; override;

      /// <inheritdoc/>
      procedure Assign          ( _source    : TPersistent
                                ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig  ( const _cfg : TGIS_ConfigAbstract
                                ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig    ( const _cfg : TGIS_ConfigAbstract
                                ) ; override;

    public

      /// <summary>
      ///   Style of the chart.
      /// </summary>
      property Style            : TGIS_ChartStyle
                                  read  FStyle
                                  write fset_Style ;

      /// <summary>
      ///   Size of chart.
      /// </summary>
      property Size             : Integer
                                  read  FSize
                                  write fset_Size ;

      /// <summary>
      ///   Size. Grouping as a text property all properties that
      ///   affect Size presentation like Size or SizeEx and represent sizes
      ///   in a human readable form like '10pt', '3in'.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property SizeAsText     : String
                                read  fget_SizeAsText
                                write fset_SizeAsText ;

      /// <summary>
      ///   Value ( 'minimum:maximum:val1:val2:val3...' ).
      /// </summary>
      property Values           : String
                                  read  fget_Values
                                  write fset_Values ;

      /// <summary>
      ///   Color ( 'col1:col2:col3:col4:col5...' ).
      /// </summary>
      property Colors           : String
                                  read  fget_Colors
                                  write fset_Colors ;

      /// <summary>
      ///   Legend text. Must be in format 'minimum:maximum:val1:val2...'.
      /// </summary>
      property Legend           : String
                                  read  FLegend
                                  write fset_Legend ;
  end ;

  /// <summary>
  ///   Grouped parameters.
  /// </summary>
  TGIS_ParamsSection = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_ParamsAbstract )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      {$IFDEF DCC} [unsafe] {$ENDIF}
      FShape          : TObject ;
      FStyle          : String  ;
      FVisible        : Boolean ;
      FMinLevel       : Double  ;
      FMaxLevel       : Double  ;
      FMinScale       : Double  ;
      FMaxScale       : Double  ;
      FMinZoom        : Double  ;
      FMaxZoom        : Double  ;
      FLegend         : String  ;
      FNormalizedZ    : TGIS_3DNormalizationType ;
      FScaleZ         : Double  ;
      FFalseZ         : Double  ;
      FFalseZInternal : TGIS_ParamsField ;
      FFalseZAsText   : String  ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      procedure fset_Style       ( const _value : String   ) ;
      procedure fset_Visible     ( const _value : Boolean  ) ;
      procedure fset_MinLevel    ( const _value : Double   ) ;
      procedure fset_MaxLevel    ( const _value : Double   ) ;
      procedure fset_MinScale    ( const _value : Double   ) ;
      procedure fset_MaxScale    ( const _value : Double   ) ;
      procedure fset_MinZoom     ( const _value : Double   ) ;
      procedure fset_MaxZoom     ( const _value : Double   ) ;
      procedure fset_Legend      ( const _value : String   ) ;
      procedure fset_NormalizedZ ( const _value : TGIS_3DNormalizationType  ) ; virtual;
      procedure fset_ScaleZ      ( const _value : Double   ) ; virtual;
      function  fget_FalseZ      : Double ;
      procedure fset_FalseZ      ( const _value : Double   ) ; virtual;
      function  fget_FalseZAsText: String ;
      procedure fset_FalseZAsText( const _value : String   ) ; virtual;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy      ; override;
    public

      /// <inheritdoc/>
      constructor Create       ; override;

      /// <inheritdoc/>
      procedure Assign         ( _source    : TPersistent
                               ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig ( const _cfg : TGIS_ConfigAbstract
                               ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig   ( const _cfg : TGIS_ConfigAbstract
                               ) ; override;

    public

      /// <summary>
      ///   Only for internal use of TatukGIS.
      /// </summary>
      InternalIndex : Integer ;

      /// <summary>
      ///   assigned shape if parameter is on a shape level; nil if parameter
      ///   is on a layer level.
      /// </summary>
      {$IFDEF DCC} [unsafe] {$ENDIF}
      property Shape           : TObject
                                 read  FShape
                                 write FShape ;

      /// <summary>
      ///   Style name.
      /// </summary>
      property Style           : String
                                 read  FStyle
                                 write fset_Style ;

      /// <summary>
      ///   Checks whether a layer section is visible. Combined with other
      ///   properties like MinLevel / MaxLevel or MinScale / MaxScale
      ///   can define a rule that determinates the layer visibility.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Unused on a shape level.
      ///    </note>
      /// </remarks>
      property Visible         : Boolean
                                 read  FVisible
                                 write fset_Visible ;

      /// <summary>
      ///   Starting visibility level. If not in range -128..128 then used
      ///   instead of MinScale or MinZoom.
      /// </summary>
      property MinLevel         : Double
                                  read  FMinLevel
                                  write fset_MinLevel ;

      /// <summary>
      ///   Ending visibility level. If not in range -128..128 then used
      ///   instead of MaxScale or MaxZoom.
      /// </summary>
      property MaxLevel         : Double
                                  read  FMaxLevel
                                  write fset_MaxLevel ;

      /// <summary>
      ///   Starting visibility zoom. If &lt;&gt; 0 then used instead of
      ///   MinLevel or MinZoom.
      /// </summary>
      property MinScale         : Double
                                  read  FMinScale
                                  write fset_MinScale ;

      /// <summary>
      ///   Ending visibility zoom. If &lt;&gt; 0 then used instead of
      ///   MinLevel or MaxZoom.
      /// </summary>
      property MaxScale         : Double
                                  read  FMaxScale
                                  write fset_MaxScale ;

      /// <summary>
      ///   Starting visibility zoom; if positive, then value refers to
      ///   pixels; If negative then value refers to twips (1/1440 of inch).
      ///   Zoom must be in MinZoom..MaxZoom to make the layer visible.
      ///   See also MaxLevel or MaxScale.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Unused on a shape level.
      ///    </note>
      /// </remarks>
      property MinZoom          : Double
                                  read  FMinZoom
                                  write fset_MinZoom ;

      /// <summary>
      ///   Ending visibility zoom; if positive, then value refers to pixels;
      ///   If negative then value refers to twips (1/1440 of inch).
      ///   Zoom must be in MinZoom..MaxZoom to make the layer visible.
      ///   See also MaxLevel or MaxScale.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Unused on a shape level.
      ///    </note>
      /// </remarks>
      property MaxZoom          : Double
                                  read  FMaxZoom
                                  write fset_MaxZoom ;

      /// <summary>
      ///   <para>
      ///     Text for caption in legend component.
      ///   </para>
      ///   <para>
      ///
      ///   </para>
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Unused on a shape level.
      ///    </note>
      /// </remarks>
      property Legend           : String
                                  read  FLegend
                                  write fset_Legend ;

      /// <summary>
      ///   If not Off then Z coordinated scaling should be normalized according
      ///   to viewer size.
      ///   Useful for data which Z coordinate does not represent heights.
      ///   Default is Off.
      /// </summary>
      property NormalizedZ      : TGIS_3DNormalizationType
                                  read  FNormalizedZ
                                  write fset_NormalizedZ ;

      /// <summary>
      ///   Z coordinate scaling (for 3D mode); Default is 1.
      /// </summary>
      property ScaleZ           : Double
                                  read  FScaleZ
                                  write fset_ScaleZ ;

      /// <summary>
      ///   Z coordinate offset (for 3D mode); Default is 0.
      /// </summary>
      property FalseZ           : Double
                                  read  fget_FalseZ
                                  write fset_FalseZ ;

      /// <summary>
      ///   FalseZ. Grouping as a text property all properties that
      ///   affect FalseZ presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property FalseZAsText     : String
                                  read  fget_FalseZAsText
                                  write fset_FalseZAsText ;

  end ;

  /// <summary>
  ///   Section parameters for vectors.
  /// </summary>
  TGIS_ParamsSectionVector = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_ParamsSection
                                  )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FQueryObj       : TGIS_SqlQuery       ;
      FRender         : TGIS_ParamsRender   ;
      FLine           : TGIS_ParamsLine     ;
      FArea           : TGIS_ParamsArea     ;
      FMarker         : TGIS_ParamsMarker   ;
      FLabels         : TGIS_ParamsLabel    ;
      FChart          : TGIS_ParamsChart    ;
      FGroundDefault  : Boolean             ;
      FGround         : TGIS_3DGroundType   ;
      FBasementDefault: Boolean             ;
      FBasement       : TGIS_3DBasementType ;
      FNormalizedM    : TGIS_3DNormalizationType ;
      FScaleM         : Double              ;
      FFalseM         : Double              ;
      FFalseMInternal : TGIS_ParamsField    ;
      FFalseMAsText   : String              ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      function  fget_Query       : String                  ;
      procedure fset_Query       ( const _value : String ) ;
      function  fget_Render      : TGIS_ParamsRender       ;
      function  fget_Line        : TGIS_ParamsLine         ;
      function  fget_Area        : TGIS_ParamsArea         ;
      function  fget_Marker      : TGIS_ParamsMarker       ;
      function  fget_Labels      : TGIS_ParamsLabel        ;
      function  fget_Chart       : TGIS_ParamsChart        ;
      procedure fset_Ground      ( const _value : TGIS_3DGroundType   ) ;
      procedure fset_Basement    ( const _value : TGIS_3DBasementType ) ;
      procedure fset_NormalizedZ ( const _value : TGIS_3DNormalizationType ) ; override;
      procedure fset_NormalizedM ( const _value : TGIS_3DNormalizationType ) ;
      procedure fset_ScaleM      ( const _value : Double              ) ;
      procedure fset_ScaleZ      ( const _value : Double              ) ; override;
      procedure fset_FalseZ      ( const _value : Double              ) ; override;
      function  fget_FalseM      : Double                               ;
      procedure fset_FalseM      ( const _value : Double              ) ;
      procedure fset_FalseZAsText( const _value : String              ) ; override;
      function  fget_FalseMAsText: String                               ;
      procedure fset_FalseMAsText( const _value : String              ) ;

    protected
      procedure fset_GroundIfNotDefault(
                                   const _value : TGIS_3DGroundType
                                 ) ;
      procedure fset_BasementIfNotDefault(
                                   const _value : TGIS_3DBasementType
                                 ) ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Assign values from _source object.
      /// </summary>
      /// <param name="_source">
      ///   object from which the value will be assigned
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use of TatukGIS.
      ///    </note>
      /// </remarks>
      procedure assignInternal ( _source : TPersistent ) ;

      /// <summary>
      ///   Prepare params for vector layers.
      /// </summary>
      function  prepareParams : TGIS_ParamsSectionVector ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy      ; override;

    public

      /// <summary>
      ///   Create an instance with default values.
      /// </summary>
      constructor Create       ; override;

      /// <inheritdoc/>
      procedure Assign         ( _source   : TPersistent
                               ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig ( const _cfg : TGIS_ConfigAbstract
                               ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig   ( const _cfg : TGIS_ConfigAbstract
                               ) ; override;

    public

      /// <summary>
      ///   Expression which describes when the section is active. Expression
      ///   must return a boolean value.
      /// </summary>
      property Query           : String
                                 read  fget_Query
                                 write fset_Query ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Precompiled Query.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Only for internal use.
      ///    </note>
      /// </remarks>
      property QueryObj        : TGIS_SqlQuery
                                 read  FQueryObj ;

      /// <summary>
      ///   Render parameters object.
      /// </summary>
      property Render          : TGIS_ParamsRender
                                 read  fget_Render ;

      /// <summary>
      ///   Line parameters object.
      /// </summary>
      property Line            : TGIS_ParamsLine
                                 read  fget_Line ;

      /// <summary>
      ///   Area parameters object.
      /// </summary>
      property Area            : TGIS_ParamsArea
                                 read  fget_Area ;

      /// <summary>
      ///   Marker parameters object.
      /// </summary>
      property Marker          : TGIS_ParamsMarker
                                 read  fget_Marker ;

      /// <summary>
      ///   Labels parameters object.
      /// </summary>
      property Labels          : TGIS_ParamsLabel
                                 read  fget_Labels ;

      /// <summary>
      ///   Chart parameters object.
      /// </summary>
      property Chart           : TGIS_ParamsChart
                                 read  fget_Chart  ;

      /// <summary>
      ///   If no Off then M coordinated scaling should be normalized according
      ///   to viewer size.
      ///   Useful for data which M coordinate does not represent heights.
      ///   Default is False.
      /// </summary>
      property NormalizedM     : TGIS_3DNormalizationType
                                 read  FNormalizedM
                                 write fset_NormalizedM ;

      /// <summary>
      ///   M coordinate scaling (for 3D mode); Default is 1.
      /// </summary>
      property ScaleM          : Double
                                 read  FScaleM
                                 write fset_ScaleM ;

      /// <summary>
      ///   M coordinate offset (for 3D mode); Default is 0.
      /// </summary>
      property FalseM          : Double
                                 read  fget_FalseM
                                 write fset_FalseM ;

      /// <summary>
      ///   FalseM. Grouping as a text property all properties that
      ///   affect FalseM presentation.
      ///   <para>
      ///     Uses AsText parameter syntax.
      ///   </para>
      /// </summary>
      property FalseMAsText     : String
                                  read  fget_FalseMAsText
                                  write fset_FalseMAsText ;

      /// <summary>
      ///   Ground relation mode (for 3D mode).
      /// </summary>
      property Ground          : TGIS_3DGroundType
                                 read  FGround
                                 write fset_Ground ;

      /// <summary>
      ///   Basement enforcement (for 3D mode).
      /// </summary>
      property Basement        : TGIS_3DBasementType
                                 read  FBasement
                                 write fset_Basement ;
  end ;

  /// <summary>
  ///   Section parameters for pixels.
  /// </summary>
  TGIS_ParamsSectionPixel = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_ParamsSection )
    private
      FPixel : TGIS_ParamsPixel ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      function fget_Pixel   : TGIS_ParamsPixel ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy     ; override;

    public

      /// <summary>
      ///   Create an instance with default values.
      /// </summary>
      constructor Create      ; override;

      /// <inheritdoc/>
      procedure Assign        ( _source    : TPersistent
                              ) ; override;

      /// <inheritdoc/>
      procedure LoadFromConfig( const _cfg : TGIS_ConfigAbstract
                              ) ; override;

      /// <inheritdoc/>
      procedure SaveToConfig  ( const _cfg : TGIS_ConfigAbstract
                              ) ; override;

    public

        /// <summary>
        ///   Pixel parameters set which belongs to the section.
        /// </summary>
        property Pixel        : TGIS_ParamsPixel
                                read  fget_Pixel ;
  end ;

  /// <summary>
  ///   List of parameters (sections).
  /// </summary>
  TGIS_ParamsList = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )

    private
      FSelected : Integer ;
      {$IFDEF DCC} [unsafe] {$ENDIF}
      FLayer : TObject ;
      {$IFDEF DCC} [unsafe] {$ENDIF}
      FSelectedObj : TGIS_ParamsSection ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      function  fget_Count    : Integer ;
      procedure fset_Selected ( const _idx : Integer ) ;
      function  fget_Item     ( const _idx : Integer ) : TGIS_ParamsSection ;
      procedure fset_Item     ( const _idx : Integer;
                                const _value : TGIS_ParamsSection
                              ) ;
      function  fget_Serial   : Integer ;

    private
      listObj : TObjectList<TGIS_ParamsAbstract> ;

    protected

      /// <summary>
      ///   Destroy an object.
      /// </summary>
      procedure doDestroy        ; override;

    public
        /// <summary>
        ///   Create an object.
        /// </summary>
        constructor Create       ;

      /// <summary>
      ///   Assign another TGIS_ParamsList to the current object.
      /// </summary>
      /// <param name="_params">
      ///   list to be assigned from
      /// </param>
      procedure   Assign         ( const _params : TGIS_ParamsList
                                 ) ;

      /// <summary>
      ///   Prepare list by assigning a main section.
      /// </summary>
      /// <param name="_params">
      ///   main section to be assigned
      /// </param>
      procedure   SetUp          ( const _params : TGIS_ParamsSection
                                 ) ;

      /// <summary>
      ///   Clear the list. Delete all items except the first one (main section).
      /// </summary>
      procedure   Clear          ;

      /// <summary>
      ///   Clear the list and set default parameters.
      /// </summary>
      procedure   ClearAndSetDefaults ;

      /// <summary>
      ///   Add a new empty section to the tail.
      /// </summary>
      procedure   Add            ;

      /// <summary>
      ///   Delete selected section.
      /// </summary>
      procedure   Delete         ;

      /// <summary>
      ///   Swaps the position of two items on the list.
      /// </summary>
      /// <param name="_index1">
      ///   first item
      /// </param>
      /// <param name="_index2">
      ///   second item
      /// </param>
      procedure   Exchange       ( const _index1 : Integer ;
                                   const _index2 : Integer
                                 ) ;
      /// <summary>
      ///   Load parameters from a list of strings.
      /// </summary>
      /// <param name="_str">
      ///   source list of strings
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Provided _str should have a form of Parameter=Value pairs.
      ///    </note>
      /// </remarks>
      procedure LoadFromStrings    ( const _str : {$IFDEF OXYGENE}
                                                    TGIS_Strings
                                                  {$ELSE}
                                                    TStrings
                                                  {$ENDIF}
                                   ) ; virtual;

      /// <summary>
      ///   Save parameters into a list of strings.
      /// </summary>
      /// <param name="_str">
      ///   destination list of strings
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Provided _str will have a form of Parameter=Value pairs.
      ///    </note>
      /// </remarks>
      procedure SaveToStrings      ( const _str : {$IFDEF OXYGENE}
                                                    TGIS_Strings
                                                  {$ELSE}
                                                    TStrings
                                                  {$ENDIF}
                                   ) ; virtual;


      /// <summary>
      ///   Load parameters from the configuration file.
      /// </summary>
      /// <param name="_cfg">
      ///   source configuration file
      /// </param>
      procedure LoadFromConfig  ( const _cfg       : TGIS_ConfigAbstract
                                ) ; overload;

      /// <summary>
      ///   Load parameters from the configuration file.
      /// </summary>
      /// <param name="_cfg">
      ///   source configuration file
      /// </param>
      /// <param name="_subLayer">
      ///   True if load sublayers
      /// </param>
      procedure LoadFromConfig  ( const _cfg       : TGIS_ConfigAbstract ;
                                  const _subLayer  : Boolean
                                ) ; overload;

      /// <summary>
      ///   Load parameters from the configuration file.
      /// </summary>
      /// <param name="_cfg">
      ///   source configuration file
      /// </param>
      /// <param name="_subLayer">
      ///   True if load sublayers
      /// </param>
      /// <param name="_groups">
      ///   True if load groups
      /// </param>
      procedure LoadFromConfig   ( const _cfg       : TGIS_ConfigAbstract ;
                                   const _subLayer  : Boolean  ;
                                   const _groups    : Boolean
                                 ) ; overload;

      /// <summary>
      ///   Save parameters into the configuration file.
      /// </summary>
      /// <param name="_cfg">
      ///   destination configuration file
      /// </param>
      procedure SaveToConfig     ( const _cfg       : TGIS_ConfigAbstract
                                 ) ; overload;

      /// <summary>
      ///   Save parameters into the configuration file.
      /// </summary>
      /// <param name="_cfg">
      ///   destination configuration file
      /// </param>
      /// <param name="_subLayer">
      ///   True if save sublayers
      /// </param>
      procedure SaveToConfig     ( const _cfg       : TGIS_ConfigAbstract ;
                                   const _subLayer  : Boolean
                                 ) ; overload;

      /// <summary>
      ///   Save parameters into the configuration file.
      /// </summary>
      /// <param name="_cfg">
      ///   destination configuration file
      /// </param>
      /// <param name="_subLayer">
      ///   True if save sublayers
      /// </param>
      /// <param name="_groups">
      ///   True if save groups
      /// </param>
      procedure   SaveToConfig   ( const _cfg       : TGIS_ConfigAbstract ;
                                   const _subLayer  : Boolean  ;
                                   const _groups    : Boolean
                                 ) ; overload;

      /// <summary>
      ///   Load parameters from the configuration file.
      /// </summary>
      /// <param name="_path">
      ///   source configuration file
      /// </param>
      procedure   LoadFromFile   ( const _path : String  ) ;

      /// <summary>
      ///   Save parameters into the configuration file.
      /// </summary>
      /// <param name="_path">
      ///   destination configuration file
      /// </param>
      procedure   SaveToFile     ( const _path : String  ) ;

      /// <summary>
      ///   Reset serial numer conter.
      /// </summary>
      /// <remarks>
      ///   Use only if original value of change tracker must be restored.
      /// </remarks>
      procedure   ResetSerial    ;

    public

      /// <summary>
      ///   Count of sections on the list.
      /// </summary>
      property Count             : Integer
                                   read  fget_Count ;

      /// <summary>
      ///   Index of selected (active) item.
      /// </summary>
      property Selected          : Integer
                                   read  FSelected
                                   write fset_Selected ;

      /// <summary>
      ///   Select (active) object.
      /// </summary>
      {$IFDEF DCC} [unsafe] {$ENDIF}
      property SelectedObj     : TGIS_ParamsSection
                                 read  FSelectedObj
                                 write FSelectedObj ;

      /// <summary>
      ///   Direct access to the item by using array like operations.
      /// </summary>
      /// <param name="_idx">
      ///   index of item
      /// </param>
      {#ownership:set:release}
      property Items[ const _idx : Integer ]
                                 : TGIS_ParamsSection
                                   read  fget_Item
                                   write fset_Item ;
                                   default ;

       /// <summary>
       ///   Serial number updated after any property change. Used to
       ///   identify if object was changed. Used by legend control for smart
       ///   updates.
       /// </summary>
       property Serial           : Integer
                                   read  fget_Serial ;

       /// <summary>
       ///   Layer associated with this list.
       /// </summary>
       {$IFDEF DCC} [unsafe] {$ENDIF}
       property Layer            : TObject
                                   read  FLayer
                                   write FLayer ;
  end ;

{$REGION 'Public functions for parameter conversion'}
   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate; can be: SINGLE, LEFTJUSTIFY, CENTER, RIGHTJUSTIFY,
   ///   FOLLOW
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   function ParamAlignment         ( const _value   : String ;
                                     const _default : TGIS_LabelAlignment
                                   ) : TGIS_LabelAlignment ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate;
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   function ConstructParamAlignment( const _value   : TGIS_LabelAlignment
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate; bitmap will be loaded into memory; please remember
   ///   to free it on your own;
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   function ParamBitmap            ( const _value   : String ;
                                     const _default : TGIS_Bitmap
                                   ) : TGIS_Bitmap ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   function ParamBoolean           ( const _value   : String;
                                     const _default : Boolean
                                   ) : Boolean ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   function ConstructParamBoolean  ( const _value   : Boolean
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Multi user value can be constructed form:
   ///   <list type="bullet">
   ///     <item>
   ///       SingleUser
   ///     </item>
   ///     <item>
   ///       MultiUser
   ///     </item>
   ///     <item>
   ///       Default
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamMultiUser         ( const _value   : String;
                                     const _default : TGIS_MultiUser
                                   ) : TGIS_MultiUser ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Multi user value can be constructed form:
   ///   <list type="bullet">
   ///     <item>
   ///       SingleUser
   ///     </item>
   ///     <item>
   ///       MultiUser
   ///     </item>
   ///     <item>
   ///       Default
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamMultiUser( const _value   : TGIS_MultiUser
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Color value to translate can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       hexadecimal BGR color ($0000FF for red)
   ///     </item>
   ///     <item>
   ///       integer BGR color (255 for red )
   ///     </item>
   ///     <item>
   ///       red:green:blue:alpha (255:0:0:255 for red)
   ///     </item>
   ///     <item>
   ///       color names:
   ///       <list type="bullet">
   ///         <item>
   ///           AQUA
   ///         </item>
   ///         <item>
   ///           BLACK
   ///         </item>
   ///         <item>
   ///           BLUE
   ///         </item>
   ///         <item>
   ///           FUCHSIA
   ///         </item>
   ///         <item>
   ///           GRAY
   ///         </item>
   ///         <item>
   ///           GREEN
   ///         </item>
   ///         <item>
   ///           LIME
   ///         </item>
   ///         <item>
   ///           MAROON
   ///         </item>
   ///         <item>
   ///           NAVY
   ///         </item>
   ///         <item>
   ///           OLIVE
   ///         </item>
   ///         <item>
   ///           PURPLE
   ///         </item>
   ///         <item>
   ///           RED
   ///         </item>
   ///         <item>
   ///           SILVER
   ///         </item>
   ///         <item>
   ///           TEAL
   ///         </item>
   ///         <item>
   ///           WHITE
   ///         </item>
   ///         <item>
   ///           YELLOW
   ///         </item>
   ///       </list>
   ///     </item>
   ///     <item>
   ///       if value RENDER, then predefined value for renderer will be
   ///       assigned.
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamColor             ( const _value   : String ;
                                     const _default : TGIS_Color
                                   ) : TGIS_Color ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Color value to translate can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       hexadecimal BGR color ($0000FF for red)
   ///     </item>
   ///     <item>
   ///       integer BGR color (255 for red )
   ///     </item>
   ///     <item>
   ///       red:green:blue:alpha (255:0:0:255 for red)
   ///     </item>
   ///     <item>
   ///       color names:
   ///       <list type="bullet">
   ///         <item>
   ///           AQUA
   ///         </item>
   ///         <item>
   ///           BLACK
   ///         </item>
   ///         <item>
   ///           BLUE
   ///         </item>
   ///         <item>
   ///           FUCHSIA
   ///         </item>
   ///         <item>
   ///           GRAY
   ///         </item>
   ///         <item>
   ///           GREEN
   ///         </item>
   ///         <item>
   ///           LIME
   ///         </item>
   ///         <item>
   ///           MAROON
   ///         </item>
   ///         <item>
   ///           NAVY
   ///         </item>
   ///         <item>
   ///           OLIVE
   ///         </item>
   ///         <item>
   ///           PURPLE
   ///         </item>
   ///         <item>
   ///           RED
   ///         </item>
   ///         <item>
   ///           SILVER
   ///         </item>
   ///         <item>
   ///           TEAL
   ///         </item>
   ///         <item>
   ///           WHITE
   ///         </item>
   ///         <item>
   ///           YELLOW
   ///         </item>
   ///       </list>
   ///     </item>
   ///     <item>
   ///       if value RENDER, then predefined value for renderer will be
   ///       assigned.
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamColor    ( const _value   : TGIS_Color
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   <para>
   ///     Pie value can be expressed as:
   ///   </para>
   ///   <list type="bullet">
   ///     <item>
   ///       PIE
   ///     </item>
   ///     <item>
   ///       BAR
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamChart             ( const _value   : String;
                                     const _default : TGIS_ChartStyle
                                   ) : TGIS_ChartStyle ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   <para>
   ///     Pie value can be expressed as:
   ///   </para>
   ///   <list type="bullet">
   ///     <item>
   ///       PIE
   ///     </item>
   ///     <item>
   ///       BAR
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamChart    ( const _value   : TGIS_ChartStyle
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Dormant value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       Off
   ///     </item>
   ///     <item>
   ///       Standard
   ///     </item>
   ///     <item>
   ///       Aggressive
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamDormant           ( const _value   : String;
                                     const _default : TGIS_LayerDormantMode
                                   ) : TGIS_LayerDormantMode ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Dormant value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       Off
   ///     </item>
   ///     <item>
   ///       Standard
   ///     </item>
   ///     <item>
   ///       Aggressive
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamDormant  ( const _value   : TGIS_LayerDormantMode
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate;
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Font style value can be expressed as: val1:val2:...:valN (BOLD:ITALIC)
   ///   from set:
   ///   <list type="bullet">
   ///     <item>
   ///       NORMAL
   ///     </item>
   ///     <item>
   ///       BOLD
   ///     </item>
   ///     <item>
   ///       ITALIC
   ///     </item>
   ///     <item>
   ///       UNDERLINE
   ///     </item>
   ///     <item>
   ///       STRIKEOUT
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamFontStyle         ( const _value   : String ;
                                     const _default : TGIS_FontStyles
                                   ) : TGIS_FontStyles ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Font style value can be expressed as: val1:val2:...:valN (BOLD:ITALIC)
   ///   from set:
   ///   <list type="bullet">
   ///     <item>
   ///       NORMAL
   ///     </item>
   ///     <item>
   ///       BOLD
   ///     </item>
   ///     <item>
   ///       ITALIC
   ///     </item>
   ///     <item>
   ///       UNDERLINE
   ///     </item>
   ///     <item>
   ///       STRIKEOUT
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamFontStyle( const _value   : TGIS_FontStyles
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate;
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   North arrow style value can be expressed as a value from set:
   ///   <list type="bullet">
   ///     <item>
   ///       ARROW1
   ///     </item>
   ///     <item>
   ///       ARROW2
   ///     </item>
   ///     <item>
   ///       NEEDLE1
   ///     </item>
   ///     <item>
   ///       NEEDLE2
   ///     </item>
   ///     <item>
   ///       NEEDLE3
   ///     </item>
   ///     <item>
   ///       ROSE1
   ///     </item>
   ///     <item>
   ///       ROSE2
   ///     </item>
   ///     <item>
   ///       ROSE3
   ///     </item>
   ///     <item>
   ///       DISK1
   ///     </item>
   ///     <item>
   ///       DISK2
   ///     </item>
   ///     <item>
   ///       DISK3
   ///     </item>
   ///     <item>
   ///       TRIANGLE1
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamNorthArrowStyle   ( const _value   : String ;
                                     const _default : TGIS_ControlNorthArrowStyle
                                   ) : TGIS_ControlNorthArrowStyle ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   North arrow style value can be expressed as a value from set:
   ///   <list type="bullet">
   ///     <item>
   ///       ARROW1
   ///     </item>
   ///     <item>
   ///       ARROW2
   ///     </item>
   ///     <item>
   ///       NEEDLE1
   ///     </item>
   ///     <item>
   ///       NEEDLE2
   ///     </item>
   ///     <item>
   ///       NEEDLE3
   ///     </item>
   ///     <item>
   ///       ROSE1
   ///     </item>
   ///     <item>
   ///       ROSE2
   ///     </item>
   ///     <item>
   ///       ROSE3
   ///     </item>
   ///     <item>
   ///       DISK1
   ///     </item>
   ///     <item>
   ///       DISK2
   ///     </item>
   ///     <item>
   ///       DISK3
   ///     </item>
   ///     <item>
   ///       TRIANGLE1
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamNorthArrowStyle(
                                     const _value   : TGIS_ControlNorthArrowStyle
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate;
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Legend icon style value can be expressed as a value from set:
   ///   <list type="bullet">
   ///     <item>
   ///       DEFAULT
   ///     </item>
   ///     <item>
   ///       RECTANGULAR
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamLegendIconStyle   ( const _value   : String ;
                                     const _default : TGIS_LegendIconStyle
                                   ) : TGIS_LegendIconStyle ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Legend icon style value can be expressed as a value from set:
   ///   <list type="bullet">
   ///     <item>
   ///       DEFAULT
   ///     </item>
   ///     <item>
   ///       RECTANGULAR
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamLegendIconStyle(
                                     const _value   : TGIS_LegendIconStyle
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   If value is RENDER then predefined value for renderer will be
   ///   assigned.
   /// </remarks>
   function ParamInteger           ( const _value   : String;
                                     const _default : Integer
                                   ) : Integer ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   function ConstructParamInteger  ( const _value   : Integer
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Marker value can be constructed from:
   ///   <list type="bullet">
   ///     <item>
   ///       BOX
   ///     </item>
   ///     <item>
   ///       CIRCLE
   ///     </item>
   ///     <item>
   ///       CROSS
   ///     </item>
   ///     <item>
   ///       DIAGCROSS
   ///     </item>
   ///     <item>
   ///       TRIANGLEUP
   ///     </item>
   ///     <item>
   ///       TRIANGLEDOWN
   ///     </item>
   ///     <item>
   ///       TRIANGLELEFT
   ///     </item>
   ///     <item>
   ///       TRIANGLERIGHT
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamMarker            ( const _value   : String;
                                     const _default : TGIS_MarkerStyle
                                   ) : TGIS_MarkerStyle ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate;
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Marker value can be constructed from:
   ///   <list type="bullet">
   ///     <item>
   ///       BOX
   ///     </item>
   ///     <item>
   ///       CIRCLE
   ///     </item>
   ///     <item>
   ///       CROSS
   ///     </item>
   ///     <item>
   ///       DIAGCROSS
   ///     </item>
   ///     <item>
   ///       TRIANGLEUP
   ///     </item>
   ///     <item>
   ///       TRIANGLEDOWN
   ///     </item>
   ///     <item>
   ///       TRIANGLELEFT
   ///     </item>
   ///     <item>
   ///       TRIANGLERIGHT
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamMarker   ( const _value   : TGIS_MarkerStyle
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Pattern value can be constructed form:
   ///   <list type="bullet">
   ///     <item>
   ///       SOLID
   ///     </item>
   ///     <item>
   ///       BDIAGONAL
   ///     </item>
   ///     <item>
   ///       FDIAGONAL
   ///     </item>
   ///     <item>
   ///       CROSS
   ///     </item>
   ///     <item>
   ///       DIAGCROSS
   ///     </item>
   ///     <item>
   ///       HORIZONTAL
   ///     </item>
   ///     <item>
   ///       VERTICAL
   ///     </item>
   ///     <item>
   ///       TRANSPARENT
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamPattern           ( const _value   : String;
                                     const _default : TGIS_BrushStyle
                                   ) : TGIS_BrushStyle ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Pattern value can be constructed form:
   ///   <list type="bullet">
   ///     <item>
   ///       SOLID
   ///     </item>
   ///     <item>
   ///       BDIAGONAL
   ///     </item>
   ///     <item>
   ///       FDIAGONAL
   ///     </item>
   ///     <item>
   ///       CROSS
   ///     </item>
   ///     <item>
   ///       DIAGCROSS
   ///     </item>
   ///     <item>
   ///       HORIZONTAL
   ///     </item>
   ///     <item>
   ///       VERTICAL
   ///     </item>
   ///     <item>
   ///       TRANSPARENT
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamPattern  ( const _value   : TGIS_BrushStyle
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Pen value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       SOLID
   ///     </item>
   ///     <item>
   ///       DASH
   ///     </item>
   ///     <item>
   ///       DOT
   ///     </item>
   ///     <item>
   ///       DASHDOT
   ///     </item>
   ///     <item>
   ///       DASHDOTDOT
   ///     </item>
   ///     <item>
   ///       CLEAR
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamSymbol            ( const _value   : String;
                                     const _default : TGIS_SymbolAbstract
                                   ) : TGIS_SymbolAbstract ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Pen value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       SOLID
   ///     </item>
   ///     <item>
   ///       DASH
   ///     </item>
   ///     <item>
   ///       DOT
   ///     </item>
   ///     <item>
   ///       DASHDOT
   ///     </item>
   ///     <item>
   ///       DASHDOTDOT
   ///     </item>
   ///     <item>
   ///       CLEAR
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamPen               ( const _value   : String;
                                     const _default : TGIS_PenStyle
                                   ) : TGIS_PenStyle ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   function ConstructParamPen      ( const _value   : TGIS_PenStyle
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   function ParamFloat             ( const _value   : String;
                                     const _default : Double
                                   ) : Double ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   function ConstructParamFloat    ( const _value   : Double
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate; empty string or string 'nil' will be replaced with
   ///  _default value
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   String value string is expected to be in C/C# format:
   ///   <list type="bullet">
   ///     <item>
   ///       '\\' will be converted to '\' and '\n' to CRLF (new line -
   ///       codes 13+10)
   ///     </item>
   ///     <item>
   ///       if value starts with '@' sign then special '\' treatment will
   ///       be suspended for the rest of the string
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamString            ( const _value   : String;
                                     const _default : String
                                   ) : String ; overload ;
   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate; string 'nil' will be replaced with
   ///  _default value
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <param name="_emptyAsDefault">
   ///   if true then empty string will be replaced with _default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   String value string is expected to be in C/C# format:
   ///   <list type="bullet">
   ///     <item>
   ///       '\\' will be converted to '\' and '\n' to CRLF (new line -
   ///       codes 13+10)
   ///     </item>
   ///     <item>
   ///       if value starts with '@' sign then special '\' treatment will
   ///       be suspended for the rest of the string
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamString            ( const _value   : String;
                                     const _default : String;
                                     const _emptyAsDefault
                                                    : Boolean
                                   ) : String ; overload ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   String value string is expected to be in C/C# format:
   ///   <list type="bullet">
   ///     <item>
   ///       '\\' will be converted to '\' and '\n' to CRLF (new line -
   ///       codes 13+10)
   ///     </item>
   ///     <item>
   ///       if value starts with '@' sign then special '\' treatment will
   ///       be suspended for the rest of the string
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamString   ( const _value   : String
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Position value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       UPLEFT
   ///     </item>
   ///     <item>
   ///       UPCENTER
   ///     </item>
   ///     <item>
   ///       UPRIGHT
   ///     </item>
   ///     <item>
   ///       MIDDLELEFT
   ///     </item>
   ///     <item>
   ///       MIDDLECENTER
   ///     </item>
   ///     <item>
   ///       MIDDLERIGHT
   ///     </item>
   ///     <item>
   ///       DOWNLEFT
   ///     </item>
   ///     <item>
   ///       DOWNCENTER
   ///     </item>
   ///     <item>
   ///       DOWNRIGHT
   ///     </item>
   ///     <item>
   ///       FLOW,
   ///     </item>
   ///     <item>
   ///       ANY
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamPosition          ( const _value   : String;
                                     const _default : TGIS_LabelPositions
                                   ) : TGIS_LabelPositions ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Position value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       UPLEFT
   ///     </item>
   ///     <item>
   ///       UPCENTER
   ///     </item>
   ///     <item>
   ///       UPRIGHT
   ///     </item>
   ///     <item>
   ///       MIDDLELEFT
   ///     </item>
   ///     <item>
   ///       MIDDLECENTER
   ///     </item>
   ///     <item>
   ///       MIDDLERIGHT
   ///     </item>
   ///     <item>
   ///       DOWNLEFT
   ///     </item>
   ///     <item>
   ///       DOWNCENTER
   ///     </item>
   ///     <item>
   ///       DOWNRIGHT
   ///     </item>
   ///     <item>
   ///       FLOW,
   ///     </item>
   ///     <item>
   ///       ANY
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamPosition ( const _value   : TGIS_LabelPositions
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Ground value can e expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       AboveZero
   ///     </item>
   ///     <item>
   ///       AboveDem
   ///     </item>
   ///     <item>
   ///       OnDem
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamGround            ( const _value   : String;
                                     const _default : TGIS_3DGroundType
                                   ) : TGIS_3DGroundType ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   function ConstructParamGround   ( const _value   : TGIS_3DGroundType
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Normalized value can e expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       Off
   ///     </item>
   ///     <item>
   ///       Max
   ///     </item>
   ///     <item>
   ///       Range
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamNormalized        (  const _value   : String;
                                      const _default : TGIS_3DNormalizationType
                                   ) : TGIS_3DNormalizationType ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   function ConstructParamNormalized(  const _value : TGIS_3DNormalizationType
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Basement value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       Off
   ///     </item>
   ///     <item>
   ///       Lowest
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamBasement          ( const _value   : String;
                                     const _default : TGIS_3DBasementType
                                   ) : TGIS_3DBasementType ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Basement value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       Off
   ///     </item>
   ///     <item>
   ///       Lowest
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamBasement ( const _value   : TGIS_3DBasementType
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate; can be\: * Off * Lowest
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   function Param3DLayerType       ( const _value   : String;
                                     const _default : TGIS_3DLayerType
                                   ) : TGIS_3DLayerType ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   function ConstructParam3DLayerType(
                                     const _value   : TGIS_3DLayerType
                                   ) : String ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   function ConstructParamInterpretation(
                                      const _value   : TGIS_LayerPixelInterpretation
                                    ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate; can be\: * Off * Lowest
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   function ParamInterpretation   ( const _value   : String;
                                    const _default : TGIS_LayerPixelInterpretation
                                  ) : TGIS_LayerPixelInterpretation ;


   /// <summary>
   ///   Split a literal value like 'FIELD:name:11 px' into parts.
   /// </summary>
   /// <param name="_text">
   ///   text to be split
   /// </param>
   /// <param name="_type">
   ///   type of text like "FIELD, SIZE etc.
   /// </param>
   /// <param name="_value1">
   ///   value part of text
   /// </param>
   /// <param name="_value2">
   ///   value part of text
   /// </param>
   /// <param name="_value3">
   ///   value part of text
   /// </param>
   /// <param name="_value4">
   ///   value part of text
   /// </param>
   /// <param name="_value5">
   ///   value part of text
   /// </param>
   /// <remarks>
   ///   Helper function for legend forms
   /// </remarks>
   procedure SplitParamAsText      ( const _text   : String ;
                                     var   _type   : String ;
                                     var   _value1 : String ;
                                     var   _value2 : String ;
                                     var   _value3 : String ;
                                     var   _value4 : String ;
                                     var   _value5 : String
                                   ) ; overload;

   /// <summary>
   ///   Split a literal value like 'FIELD:name:11 px' into parts.
   /// </summary>
   /// <param name="_text">
   ///   text to be split
   /// </param>
   /// <param name="_type">
   ///   type of text like "FIELD, SIZE etc.
   /// </param>
   /// <param name="_value1">
   ///   value part of text
   /// </param>
   /// <param name="_value2">
   ///   value part of text
   /// </param>
   /// <remarks>
   ///   Helper function for legend forms
   /// </remarks>
   procedure SplitParamAsText      ( const _text   : String ;
                                     var   _type   : String ;
                                     var   _value1 : String ;
                                     var   _value2 : String
                                   ) ; overload;

   /// <summary>
   ///   Construct AsText representation of property.
   /// </summary>
   /// <param name="_type">
   ///   type of text like "FIELD, SIZE etc.
   /// </param>
   /// <param name="_value1">
   ///   value part of text
   /// </param>
   /// <param name="_value2">
   ///   value part of text
   /// </param>
   /// <param name="_value3">
   ///   value part of text
   /// </param>
   /// <param name="_value4">
   ///   value part of text
   /// </param>
   /// <param name="_value5">
   ///   value part of text
   /// </param>
   /// <returns>
   ///   Constructed string in a form '_type:_value1:value2'.
   /// </returns>
   /// <remarks>
   ///   Helper function for legend forms
   /// </remarks>
   function  ConstructParamAsText  ( const _type   : String ;
                                     const _value1 : String ;
                                     const _value2 : String ;
                                     const _value3 : String ;
                                     const _value4 : String ;
                                     const _value5 : String
                                   ) : String ; overload;

   /// <summary>
   ///   Construct AsText representation of property.
   /// </summary>
   /// <param name="_type">
   ///   type of text like "FIELD, SIZE etc.
   /// </param>
   /// <param name="_value1">
   ///   value part of text
   /// </param>
   /// <param name="_value2">
   ///   value part of text
   /// </param>
   /// <returns>
   ///   Constructed string in a form '_type:_value1:value2'.
   /// </returns>
   /// <remarks>
   ///   Helper function for legend forms
   /// </remarks>
   function  ConstructParamAsText  ( const _type   : String ;
                                     const _value1 : String ;
                                     const _value2 : String
                                   ) : String ; overload;

   /// <summary>
   ///   Split a literal value like 'FIELD:name:11 px' into parts.
   /// </summary>
   /// <param name="_text">
   ///   text to be split
   /// </param>
   /// <param name="_type">
   ///   type of value like 'SIZE", 'FIELD'
   /// </param>
   /// <param name="_field">
   ///   field name or empty string if text is like 'SIZE:11 px'
   /// </param>
   /// <param name="_value">
   ///   value part of text
   /// </param>
   /// <param name="_unit">
   ///   unit part of text
   /// </param>
   /// <remarks>
   ///   Helper function for legend forms
   /// </remarks>
   procedure SplitNumberAsText     ( const _text   : String ;
                                     var   _type   : String ;
                                     var   _field  : String ;
                                     var   _value  : String ;
                                     var   _unit   : String
                                   ) ;

   /// <summary>
   ///   Construct AsText representation of property.
   /// </summary>
   /// <param name="_field">
   ///   field name; if empty then property is type of _size
   /// </param>
   /// <param name="_value">
   ///   value part of text
   /// </param>
   /// <param name="_unit">
   ///   units; can be empty if value in unnamed
   /// </param>
   /// <returns>
   ///   Constructed string in a form 'FIELD:name:11 px'.
   /// </returns>
   /// <remarks>
   ///   Helper function for legend forms
   /// </remarks>
   function  ConstructNumberAsText ( const _field  : String ;
                                     const _value  : String ;
                                     const _unit   : String
                                   ) : String ;

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <param name="_default">
   ///   default value
   /// </param>
   /// <returns>
   ///   Constructed parameter from string representation.
   /// </returns>
   /// <remarks>
   ///   Direction value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       UPLEFT
   ///     </item>
   ///     <item>
   ///       UPRIGHT
   ///     </item>
   ///     <item>
   ///       DOWNLEFT
   ///     </item>
   ///     <item>
   ///       DOWNRIGHT
   ///     </item>
   ///   </list>
   /// </remarks>
   function ParamOffsetPosition    ( const _value   : String;
                                     const _default : TGIS_OffsetPosition
                                   ) : TGIS_OffsetPosition ;

   /// <summary>
   ///   Convert an internal representation of a value into the literal
   ///   representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate
   /// </param>
   /// <returns>
   ///   Constructed parameter string representation.
   /// </returns>
   /// <remarks>
   ///   Direction value can be expressed as:
   ///   <list type="bullet">
   ///     <item>
   ///       UPLEFT
   ///     </item>
   ///     <item>
   ///       UPRIGHT
   ///     </item>
   ///     <item>
   ///       DOWNLEFT
   ///     </item>
   ///     <item>
   ///       DOWNRIGHT
   ///     </item>
   ///   </list>
   /// </remarks>
   function ConstructParamOffsetPosition( const _value   : TGIS_OffsetPosition
                                        ) : String ;


{$ENDREGION}
{$REGION 'Const'}
const
  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_CUSTOM   = 'CUSTOM' ;

  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_RENDERER = 'RENDERER' ;

  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_FIELD    = 'FIELD' ;

  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_STOCK    = 'STOCK' ;

  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_TEXTURE  = 'TEXTURE' ;

  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_SYMBOL   = 'SYMBOL' ;

  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_CODE     = 'CODE' ;

  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_ARGB     = 'ARGB' ;

  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_SIZE     = 'SIZE' ;

  {#gendoc:hide}
  GIS_PARAMTXT_TYPE_ANGLE    = 'ANGLE' ;

  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_HAIR   = 'HAIR' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_PX     = 'px' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_DIP    = 'dip' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_TWIPS  = 'twips' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_PT     = 'pt' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_MM     = 'mm' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_CM     = 'cm' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_M      = 'm' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_KM     = 'km' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_IN     = 'in' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_FT     = 'ft' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_YD     = 'yd' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_MI     = 'mi' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_NM     = 'nm' ;
  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_MU     = 'mu' ;

  {#gendoc:hide}
  GIS_PARAMTXT_ROTATION_DEG = 'deg' ;
  {#gendoc:hide}
  GIS_PARAMTXT_ROTATION_RAD = 'rad' ;

  {#gendoc:hide}
  GIS_PARAMTXT_SIZE_CNT     = 13 ;

  {#gendoc:hide}
  GIS_PARAMTXT_MEASURE_CNT  = 10 ;

  {#gendoc:hide}
  GIS_PARAMTXT_ANGLE_CNT    = 2 ;

{$ENDREGION}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.Variants,
    {$IFDEF LEVEL_XE3_RTL}
      System.UITypes,
    {$ENDIF}

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoResource;
{$ENDIF}

{$REGION 'set_bitmap_property utilities'}
type
  T_set_bitmap_property = procedure( const _value : TGIS_Bitmap ) of object;

// set bitmap property with a proper object release
procedure set_bitmap_property(
  const _cfg     : TGIS_ConfigAbstract ;
  const _setter  : T_set_bitmap_property ;
  const _name    : String      ;
  const _default : TGIS_Bitmap
) ;
var
  bmp : TGIS_Bitmap ;
begin
  bmp := TGIS_Config( _cfg ).ReadBitmap( _name, nil ) ;

  if assigned( bmp ) then begin
    _setter( bmp ) ;
    FreeObject( bmp ) ;
  end
  else begin
    _setter( _default ) ;
  end;
end;
{$ENDREGION 'setBitmapProperty utilities'}

{$REGION 'config_format utilities'}
function config_format(
  const _config  : TGIS_Config;
  const _version : Integer
) : Boolean ;
begin
  Result := TGIS_Config( _config ).Version = _version ;
end ;

function config_old_format(
  const _config : TGIS_ConfigAbstract
) : Boolean ;
begin
  Result := ( TGIS_Config( _config ).Version div 1000 ) <= GIS_CONFIG_VER_DK10_MAJOR ;
end ;

function config_new_format( const _config : TGIS_ConfigAbstract ) : Boolean ;
begin
  Result := ( TGIS_Config( _config ).Version div 1000 ) = GIS_CONFIG_VER_DK11_MAJOR ;
end ;

function config_any_format( const _config : TGIS_ConfigAbstract ) : Boolean ;
begin
  Result := True ;
end ;
{$ENDREGION 'config_format utilities'}

{$REGION 'ParameterAsText utilities'}
const
  DIP_TO_TWIPS = 15;
  PT_TO_TWIPS  = 20;
  IN_TO_TWIPS  = 72    * PT_TO_TWIPS ;
  FT_TO_TWIPS  = 12    * IN_TO_TWIPS ;
  YD_TO_TWIPS  = 3     * FT_TO_TWIPS ;
  MI_TO_TWIPS  = 1760  * YD_TO_TWIPS ;
  MM_TO_TWIPS  = 56.692913386;
  CM_TO_TWIPS  = 10    * MM_TO_TWIPS ;
  M_TO_TWIPS   = 100   * CM_TO_TWIPS ;
  KM_TO_TWIPS  = 1000  * M_TO_TWIPS  ;
  NM_TO_TWIPS  = 1.852 * KM_TO_TWIPS ;

  IN_TO_MM     = 25.4  ;
  FT_TO_MM     = 12    * IN_TO_MM ;
  YD_TO_MM     = 3     * FT_TO_MM ;
  MI_TO_MM     = 1760  * YD_TO_MM ;
  MM_TO_MM     = 1;
  CM_TO_MM     = 10    * MM_TO_MM ;
  M_TO_MM      = 100   * CM_TO_MM ;
  KM_TO_MM     = 1000  * M_TO_MM  ;
  NM_TO_MM     = 1.852 * KM_TO_MM ;

  IN_TO_M      = 25.4 / 1000 ;
  FT_TO_M      = 12    * IN_TO_M ;
  YD_TO_M      = 3     * FT_TO_M ;
  MI_TO_M      = 1760  * YD_TO_M ;
  MM_TO_M      = 1.0 / 1000 ;
  CM_TO_M      = 10    * MM_TO_M ;
  M_TO_M       = 100   * CM_TO_M ;
  KM_TO_M      = 1000  * M_TO_M  ;
  NM_TO_M      = 1.852 * KM_TO_M ;

function make_size_DK10( const _val : Integer ) : Integer ;
begin
  if Abs( _val ) > GIS_AUTOSIZE_SIZE_MU then
    Result :=  RoundS( ( Abs(_val ) mod GIS_AUTOSIZE_SIZE_MU )
                       * 10 * MM_TO_TWIPS + GIS_AUTOSIZE_SIZE
                     )
  else
    Result := _val ;
end;

procedure split_value(
  const _text  : String ;
  var   _type  : String ;
  var   _value : String
) ; overload ;
var
  k : Integer ;
begin
  _type  := '' ;
  _value := '' ;

  k := Pos( ':', _text ) ;
  if k < StringFirst then begin
    _type := _text ;
    exit ;
  end ;

  _type  := UpperCase( Copy( _text, StringFirst, k-StringFirst  ) ) ;
  _value := Copy( _text, k+1, 4096  ) ;
end ;

procedure split_value(
  const _text  : String ;
  var   _type  : String ;
  var   _value : String ;
  var   _subtype : String
) ; overload ;
var
  tkn : TGIS_Tokenizer;
begin
  _type  := '' ;
  _value := '' ;
  _subtype := '' ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _text, [':'] ) ;

    if tkn.Result.Count > 0 then
      _type := tkn.Result[0] ;
    if tkn.Result.Count > 1 then
      _value := tkn.Result[1] ;
    if tkn.Result.Count > 2 then
      _subtype := tkn.Result[2] ;
  finally
    FreeObject( tkn ) ;
  end ;
end ;

procedure parse_byfield(
    var _field  : String ;
  const _value  : String
) ; overload;
var
  tkn : TGIS_Tokenizer ;
begin
  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.ExecuteEx( _value, ':' ) ;
    if tkn.Result.Count > 0  then
      _field := tkn.Result[0] ;
  finally
    FreeObject( tkn ) ;
  end ;
end ;

function pen_to_text(
  const _symbol  : TGIS_SymbolAbstract ;
  const _value   : String ;
  const _style   : TGIS_PenStyle ;
  const _field   : TGIS_ParamsField
) : String ;
begin
  if not IsStringEmpty( _value ) then begin
    Result := _value ;
  end
  else
  if assigned( _symbol ) then begin
    if ( _symbol is TGIS_SymbolLineEx ) and
       ( _symbol.Name[StringFirst] = '&' ) then
      Result := GIS_PARAMTXT_TYPE_CODE + ':' + _symbol.Name
    else
      Result := GIS_PARAMTXT_TYPE_SYMBOL + ':' + _symbol.Name ;
  end
  else
  if assigned( _field ) and ( not IsStringEmpty( _field.Field ) ) then
    Result := GIS_PARAMTXT_TYPE_FIELD + ':'  + _field.Field
  else
    Result := GIS_PARAMTXT_TYPE_STOCK + ':' + ConstructParamPen( _style )
end ;

procedure pen_from_text(
  const _cfg     : TGIS_Config         ;
  const _parent  : TGIS_ParamsAbstract ;
  const _value   : String              ;
  var   _symbol  : TGIS_SymbolAbstract ;
  var   _style   : TGIS_PenStyle       ;
  var   _field   : TGIS_ParamsField    ;
  const _bsymbol : Boolean
) ;
var
  stype : String ;
  sval  : String ;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if _value = GIS_PARAM_NIL then
    exit ;

  split_value( _value, stype, sval ) ;

  if _bsymbol and _stype( GIS_PARAMTXT_TYPE_SYMBOL ) then begin
    if assigned( _cfg ) then
     _symbol := ParamSymbol( _cfg.AbsolutePath( sval ), nil )
    else
     _symbol := ParamSymbol( sval, nil ) ;
    FreeObject( _field ) ;
  end
  else if _bsymbol and _stype( GIS_PARAMTXT_TYPE_CODE ) then begin
    if sval[StringFirst] = '&' then
      _symbol := ParamSymbol( sval, nil )
    else
      _symbol := ParamSymbol( '&' + sval, nil ) ;
    FreeObject( _field ) ;
  end
  else if _stype( GIS_PARAMTXT_TYPE_STOCK ) then begin
     _symbol  := nil ;
    _style  := ParamPen( sval,  TGIS_PenStyle.Solid ) ;
    FreeObject( _field ) ;
  end
  else if _stype( GIS_PARAMTXT_TYPE_FIELD ) then begin
    if not assigned( _field ) then
      _field := TGIS_ParamsField.Create( _parent ) ;
    _field.Field := sval ;
  end
  else
    Abort ;
end ;

function marker_to_text(
  const _symbol  : TGIS_SymbolAbstract ;
  const _style   : TGIS_MarkerStyle
) : String ;
begin
  if assigned( _symbol ) then
    Result := GIS_PARAMTXT_TYPE_SYMBOL + ':' + _symbol.Name
  else
    Result := GIS_PARAMTXT_TYPE_STOCK + ':' + ConstructParamMarker( _style )
end ;

procedure marker_from_text(
  const _cfg     : TGIS_Config         ;
  const _value   : String              ;
  var   _symbol  : TGIS_SymbolAbstract ;
  var   _style   : TGIS_MarkerStyle    ;
  const _bsymbol : Boolean
) ;
var
  stype : String ;
  sval  : String ;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if _value = GIS_PARAM_NIL then
    exit ;

  split_value( _value, stype, sval ) ;

  if _bsymbol and _stype( GIS_PARAMTXT_TYPE_SYMBOL ) then begin
    if assigned( _cfg ) then
     _symbol := ParamSymbol( _cfg.AbsolutePath( sval ), nil )
    else
     _symbol := ParamSymbol( sval, nil ) ;
  end
  else
  if _stype( GIS_PARAMTXT_TYPE_STOCK ) then begin
     _symbol  := nil ;
    _style  := ParamMarker( sval,  TGIS_MarkerStyle.Cross ) ;
  end
  else
    Abort ;
end ;


function pattern_to_text(
  const _symbol  : TGIS_SymbolAbstract ;
  const _bitmap  : TGIS_Bitmap         ;
  const _pattern : TGIS_BrushStyle
) : String ;
begin
  if assigned( _symbol ) then
    Result := GIS_PARAMTXT_TYPE_SYMBOL + ':' + _symbol.Name
  else if not TGIS_Bitmap.IsNilOrEmpty( _bitmap ) then
    Result := GIS_PARAMTXT_TYPE_TEXTURE + ':' +  _bitmap.Path
  else
    Result := GIS_PARAMTXT_TYPE_STOCK + ':' + ConstructParamPattern( _pattern )
end ;

procedure pattern_from_text(
  const _cfg     : TGIS_Config         ;
  const _value   : String              ;
  var   _symbol  : TGIS_SymbolAbstract ;
  var   _bitmap  : TGIS_Bitmap         ;
  var   _pattern : TGIS_BrushStyle     ;
  const _bsymbol : Boolean
) ;
var
  stype : String ;
  sval  : String ;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if _value = GIS_PARAM_NIL then
    exit ;

  split_value( _value, stype, sval ) ;

  if _bsymbol and _stype( GIS_PARAMTXT_TYPE_SYMBOL ) then begin
    if assigned( _cfg ) then
     _symbol := ParamSymbol( _cfg.AbsolutePath( sval ), nil )
    else
     _symbol := ParamSymbol( sval, nil ) ;
    _bitmap := nil ;
  end
  else if _stype( GIS_PARAMTXT_TYPE_TEXTURE ) then begin
    _symbol  := nil ;
    if assigned( _cfg ) then
      _bitmap := ParamBitmap( _cfg.AbsolutePath( sval ), nil )
    else
      _bitmap := ParamBitmap( sval, nil )
  end
  else if _stype( GIS_PARAMTXT_TYPE_STOCK ) then begin
    _symbol  := nil ;
    _bitmap  := nil ;
    _pattern := ParamPattern( sval,  TGIS_BrushStyle.Solid ) ;
  end
  else
    Abort ;
end ;

function symbol_to_text(
  const _symbol  : TGIS_SymbolAbstract
) : String ;
begin
  if assigned( _symbol ) then
    Result := GIS_PARAMTXT_TYPE_SYMBOL + ':' + _symbol.Name
  else
    Result := GIS_PARAM_NIL ;
end ;

procedure symbol_from_text(
  const _cfg     : TGIS_Config         ;
  const _value   : String              ;
  var   _symbol  : TGIS_SymbolAbstract
) ;
var
  stype : String ;
  sval  : String ;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if _value = GIS_PARAM_NIL then
    exit ;

  split_value( _value, stype, sval ) ;

  if _stype( GIS_PARAMTXT_TYPE_SYMBOL ) then begin
    if assigned( _cfg ) then
     _symbol := ParamSymbol( _cfg.AbsolutePath( sval ), nil )
    else
     _symbol := ParamSymbol( sval, nil ) ;
  end
  else
    Abort ;
end ;

function color_to_text(
  const _color   : TGIS_Color ;
  const _field   : TGIS_ParamsField
) : String ;
begin
  if assigned( _field ) and ( not IsStringEmpty( _field.Field ) ) then begin
    if _field.FSubType = '' then
      Result := GIS_PARAMTXT_TYPE_FIELD + ':' + _field.Field
    else
      Result := GIS_PARAMTXT_TYPE_FIELD + ':' + _field.Field + ':' + _field.FSubType ;
  end
  else if _color.ARGB = TGIS_Color.RenderColor.ARGB then
    Result := GIS_PARAMTXT_TYPE_RENDERER
  else
    Result := GIS_PARAMTXT_TYPE_ARGB + ':' + IntToHex( _color.ARGB, 8 ) ;
end ;

procedure color_from_text(
  const _parent : TGIS_ParamsAbstract ;
  const _value : String               ;
  var   _color : TGIS_Color           ;
  var   _field : TGIS_ParamsField
) ; overload;
var
  stype : String ;
  sval  : String ;
  ssubtype : String ;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if _value = GIS_PARAM_NIL then
    exit ;

  split_value( _value, stype, sval, ssubtype ) ;

  if _stype( GIS_PARAMTXT_TYPE_ARGB ) then begin
    if length( sval ) > 6 then
      _color := TGIS_Color.FromARGB( StrToInt( '$' + sval ) )
    else
      _color := TGIS_Color.FromRGB( StrToInt( '$' + sval ) ) ;
    FreeObject( _field ) ;
  end
  else if _stype( GIS_PARAMTXT_TYPE_RENDERER ) then begin
    _color  := TGIS_Color.RenderColor ;
    FreeObject( _field ) ;
  end
  else if _stype( GIS_PARAMTXT_TYPE_FIELD ) then begin
    if not assigned( _field ) then
      _field := TGIS_ParamsField.Create( _parent ) ;
    _field.Field    := sval ;
    _field.FSubType := ssubtype ;
  end
  else
    Abort ;
end ;

procedure color_from_text(
  const _value : String     ;
  var   _color : TGIS_Color
) ; overload;
var
  stype : String ;
  sval  : String ;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if _value = GIS_PARAM_NIL then
    exit ;

  split_value( _value, stype, sval ) ;

  if _stype( GIS_PARAMTXT_TYPE_ARGB ) then begin
    if length( sval ) > 6 then
      _color := TGIS_Color.FromARGB( StrToInt( '$' + sval ) )
    else
      _color := TGIS_Color.FromRGB( StrToInt( '$' + sval ) ) ;
  end
  else if _stype( GIS_PARAMTXT_TYPE_RENDERER ) then begin
    _color  := TGIS_Color.RenderColor ;
  end
  else
    Abort ;
end ;

function position_to_text(
  const _position  : TGIS_LabelPositions ;
  const _field     : TGIS_ParamsField
) : String ;
begin
  if assigned( _field ) and ( not IsStringEmpty( _field.Field ) ) then
    Result := GIS_PARAMTXT_TYPE_FIELD + ':'  + _field.Field
  else
    Result := GIS_PARAMTXT_TYPE_STOCK + ':' + ConstructParamPosition( _position ) ;
end ;

procedure position_from_text(
  const _parent   : TGIS_ParamsAbstract ;
  const _value    : String              ;
  var   _position : TGIS_LabelPositions ;
  var   _field    : TGIS_ParamsField
) ;
var
  stype : String ;
  sval  : String ;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if _value = GIS_PARAM_NIL then
    exit ;

  split_value( _value, stype, sval ) ;

  if _stype( GIS_PARAMTXT_TYPE_FIELD ) then begin
    if not assigned( _field ) then
      _field := TGIS_ParamsField.Create( _parent ) ;
    _field.Field := sval ;
  end
  else if _stype( GIS_PARAMTXT_TYPE_STOCK ) then begin
    _position := ParamPosition( sval, _position ) ;
    FreeObject( _field ) ;
  end
  else
    Abort ;
end ;

procedure split_size_val(
  const _text  : String ;
  var   _value : String ;
  var   _unit  : String
) ;
var
  i     : Integer ;
  state : Integer ;
  c     : Char    ;
  bldvalue  : TStringBuilder;
  bldunit   : TStringBuilder;
begin
  bldvalue := TStringBuilder.Create ;
  bldunit  := TStringBuilder.Create ;
  try
    state := 0 ;
    i := StringFirst ;
    while i < StringFirst + length( _text ) do begin
      c := _text[ i ] ;

      case state of
        0 :  begin
               case c of
                 ' '  :
                   continue   ;
                 else
                   begin
                     state := 1 ;
                     continue ;
                   end;
               end;
             end;
        1 :  begin
               case c of
                 '0'..'9',
                 '+', '-' :
                   bldvalue.Append( c ) ;
                 '.',
                 ',' :
                   begin
                     bldvalue.Append( ',' ) ;
                     state := 2 ;
                   end;
                 ' ' :
                   begin
                     state := 3 ;
                     continue ;
                   end;
                 else
                   begin
                     state := 4 ;
                     continue ;
                   end;
               end;
             end ;
        2 :  begin
               case c of
                 '0'..'9' :
                   bldvalue.Append( c ) ;
                 ' ' :
                   begin
                     state := 3 ;
                     continue ;
                   end;
                 else begin
                   state := 4 ;
                   continue ;
                 end;
               end;
             end ;
        3 :  begin
               case c of
                 ' '  :
                   begin
                   // just ignore
                   end
                 else
                   begin
                     state := 4 ;
                     continue ;
                   end;
               end;
             end;
        4 :  begin
               case c of
                 ' ' :
                   break ;
                 else
                   bldunit.Append( c ) ;
               end;
             end;
        else
          assert( false );
      end;

      inc( i ) ;
    end;

    _value := bldvalue.ToString ;
    _unit  := bldunit.ToString ;
  finally
    FreeObject( bldvalue );
    FreeObject( bldunit  );
  end;
end ;


// Convert AsText property value (as WidthAsText) to a proper field
//  (as WidthEx) and integer property value (as Width)
function size_to_text(
  const _value  : String  ;
  const _size   : Integer ;
  const _field  : TGIS_ParamsField
) : String ;

  function fld_to_text : String ;
  var
    stmp : String ;
    sunt : String ;
  begin
    stmp := DotFloatToStr( Abs( _field.Factor ) ) ;

    if _field.Scalable then begin
      if      _field.Factor = IN_TO_M then sunt := GIS_PARAMTXT_SIZE_IN
      else if _field.Factor = FT_TO_M then sunt := GIS_PARAMTXT_SIZE_FT
      else if _field.Factor = YD_TO_M then sunt := GIS_PARAMTXT_SIZE_YD
      else if _field.Factor = MI_TO_M then sunt := GIS_PARAMTXT_SIZE_MI
      else if _field.Factor = MM_TO_M then sunt := GIS_PARAMTXT_SIZE_MM
      else if _field.Factor = CM_TO_M then sunt := GIS_PARAMTXT_SIZE_CM
      else if _field.Factor = M_TO_M  then sunt := GIS_PARAMTXT_SIZE_M
      else if _field.Factor = KM_TO_M then sunt := GIS_PARAMTXT_SIZE_KM
      else if _field.Factor = NM_TO_M then sunt := GIS_PARAMTXT_SIZE_NM
      else                                 sunt := GIS_PARAMTXT_SIZE_M ;
    end
    else begin
      if _field.Factor < 1 then
        sunt := GIS_PARAMTXT_SIZE_PX
      else
        sunt := GIS_PARAMTXT_SIZE_TWIPS ;
    end;

    Result := stmp + ' ' + sunt ;
  end;

  function autosize_to_text : String ;
  var
    itmp : Int64 ;
  begin
    if _size < 0 then begin
      itmp := ( Abs( _size ) mod GIS_AUTOSIZE_SIZE ) ;
      itmp := itmp * 10 ;
    end
    else
      itmp := RoundS( ( _size  mod GIS_AUTOSIZE_SIZE  ) / MM_TO_TWIPS ) * 10 ;


    if itmp mod RoundS( MI_TO_MM * 10 ) = 0 then
      Result := IntToStr(
                  RoundS( itmp / NM_TO_MM / 10 )
                ) +  ' ' + GIS_PARAMTXT_SIZE_NM
    else
    if itmp mod RoundS( MI_TO_MM * 10 ) = 0 then
      Result := IntToStr(
                  RoundS( itmp / MI_TO_MM / 10 )
                ) +  ' ' + GIS_PARAMTXT_SIZE_MI
    else
    if itmp mod RoundS( FT_TO_MM * 10 ) = 0 then
      Result := IntToStr(
                  RoundS( itmp / FT_TO_MM / 10  )
                ) +  ' ' + GIS_PARAMTXT_SIZE_FT
    else
    if itmp mod RoundS( IN_TO_MM * 10 ) = 0 then
      Result := IntToStr(
                  RoundS( itmp / IN_TO_MM / 10  )
                ) +  ' ' + GIS_PARAMTXT_SIZE_IN
    else
    if itmp mod RoundS( KM_TO_MM * 10  ) = 0 then
      Result := IntToStr(
                  RoundS( itmp / KM_TO_MM / 10 )
                ) +  ' ' + GIS_PARAMTXT_SIZE_KM
    else
    if itmp mod RoundS( M_TO_MM * 10 ) = 0 then
      Result := IntToStr(
                  RoundS( itmp / M_TO_MM / 10 )
                ) +  ' ' + GIS_PARAMTXT_SIZE_M
    else
    if itmp mod RoundS( CM_TO_MM * 10 ) = 0 then
      Result := IntToStr(
                  RoundS( itmp  / CM_TO_MM / 10  )
                ) +  ' ' + GIS_PARAMTXT_SIZE_CM
    else
      Result := IntToStr(
                  RoundS( itmp / 10  )
                ) +  ' ' + GIS_PARAMTXT_SIZE_MM
  end;

  function val_to_text : String ;
  begin
    if _size < 0 then begin
      if _size < -GIS_AUTOSIZE_SIZE_MU then
        Result := DotFloatToStr(
                    ( Abs( _size ) mod GIS_AUTOSIZE_SIZE_MU ) / 100
                  )  + ' ' + GIS_PARAMTXT_SIZE_MU
      else
      if _size < -GIS_AUTOSIZE_SIZE then begin
        Result := autosize_to_text ;
      end
      else
        Result := IntToStr(
                    Abs( _size )
                  ) + ' ' + GIS_PARAMTXT_SIZE_PX
    end
    else begin
      if _size > GIS_AUTOSIZE_SIZE then begin
        Result := autosize_to_text ;
      end
      else begin
        if _size mod PT_TO_TWIPS = 0 then begin
          if ( _size / PT_TO_TWIPS ) < 100  then
            Result := IntToStr(
                        RoundS( _size / PT_TO_TWIPS )
                      ) + ' ' + GIS_PARAMTXT_SIZE_PT
          else
            Result := IntToStr(
                        _size
                    ) + ' ' + GIS_PARAMTXT_SIZE_TWIPS
        end
        else
          Result := IntToStr(
                      _size
                    ) + ' ' + GIS_PARAMTXT_SIZE_TWIPS
      end;
    end;
  end ;
begin
  if not IsStringEmpty( _value ) then begin
    Result := _value ;
  end
  else
  if assigned( _field ) and ( not IsStringEmpty( _field.Field ) ) then begin
    Result := GIS_PARAMTXT_TYPE_FIELD + ':'  + fld_to_text ;
  end
  else
  if _size = GIS_RENDER_SIZE then begin
    Result := GIS_PARAMTXT_TYPE_RENDERER ;
  end
  else begin
    Result := GIS_PARAMTXT_TYPE_SIZE + ':'  + val_to_text ;
  end;
end;

// Convert size (as Width) and field (as WidthEx) to a proper string
// represtention for AsText property value (as WidthAsText)
procedure size_from_text(
  const _parent : TGIS_ParamsAbstract ;
  const _value  : String              ;
  var   _size   : Integer             ;
  var   _field  : TGIS_ParamsField
) ; overload;
var
  tkn : TGIS_Tokenizer ;
  stype : String  ;
  sval1 : String  ;
  sval2 : String  ;

  dval  : Double ;
  bscalable : Boolean ;

  function text_to_val(
    const _val : String
  ) : Integer ;
  var
    s1   : String  ;
    s2   : String  ;

    function _s2( const _text : String ) : Boolean ;
    begin
      Result := CompareText( s2, _text ) = 0 ;
    end;
  begin
    split_size_val( _val, s1, s2 );

    Result := 0 ;
    dval   := 1.0 ;

    if      _s2( GIS_PARAMTXT_SIZE_HAIR  ) then begin
      dval   :=   1.0 ;
    end
    else if _s2( GIS_PARAMTXT_SIZE_PX    ) then begin
      dval   := - Abs( DotStrToFloat( s1 ) )
    end
    else if _s2( GIS_PARAMTXT_SIZE_DIP   ) then begin
      dval   :=   Abs( DotStrToFloat( s1 ) ) * DIP_TO_TWIPS
    end
    else if _s2( GIS_PARAMTXT_SIZE_TWIPS ) then begin
      dval   :=   Abs( DotStrToFloat( s1 ) )
    end
    else if _s2( GIS_PARAMTXT_SIZE_PT    ) then begin
      dval   :=   Abs( DotStrToFloat( s1 ) ) * PT_TO_TWIPS
    end
    else if _s2( GIS_PARAMTXT_SIZE_IN    ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE ;
      dval   := - DotStrToFloat( s1 ) * IN_TO_MM
    end
    else if _s2( GIS_PARAMTXT_SIZE_FT    ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE ;
      dval   := - DotStrToFloat( s1 ) * FT_TO_MM
    end
    else if _s2( GIS_PARAMTXT_SIZE_YD    ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE ;
      dval   := - DotStrToFloat( s1 ) * YD_TO_MM
    end
    else if _s2( GIS_PARAMTXT_SIZE_MI    ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE ;
      dval   := - DotStrToFloat( s1 ) * MI_TO_MM
    end
    else if _s2( GIS_PARAMTXT_SIZE_MM    ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE ;
      dval   := - DotStrToFloat( s1 ) * MM_TO_MM * MM_TO_TWIPS
    end
    else if _s2( GIS_PARAMTXT_SIZE_CM    ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE ;
      dval   := - DotStrToFloat( s1 ) * CM_TO_MM
    end
    else if _s2( GIS_PARAMTXT_SIZE_M     ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE ;
      dval   := - DotStrToFloat( s1 ) * M_TO_MM
    end
    else if _s2( GIS_PARAMTXT_SIZE_KM    ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE ;
      dval   := - DotStrToFloat( s1 ) * KM_TO_MM
    end
    else if _s2( GIS_PARAMTXT_SIZE_NM    ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE ;
      dval   := - DotStrToFloat( s1 ) * NM_TO_MM
    end
    else if _s2( GIS_PARAMTXT_SIZE_MU    ) then begin
      if DotStrToFloat( s1 ) > 0 then
        Result := - GIS_AUTOSIZE_SIZE_MU ;
      dval   := - DotStrToFloat( s1 ) * 100 ; // 2 decimal points
    end
    else
      dval   := -1.0 ;

    if dval >= 0.0 then
      dval := Min( dval, GIS_AUTOSIZE_SIZE - 1 )
    else
      dval := - Min( Abs( dval ), GIS_AUTOSIZE_SIZE - 1 );

    bscalable := ( Abs( Result ) = GIS_AUTOSIZE_SIZE    ) or
                 ( Abs( Result ) = GIS_AUTOSIZE_SIZE_MU ) ;

    Result := Result + RoundS( dval ) ;
  end ;

  procedure text_to_fld ;
  begin
    if IsStringEmpty( sval1 ) or IsStringEmpty( sval2 ) then
      exit ;

    text_to_val( sval2 ) ;

    if not assigned( _field )  then
      _field :=  TGIS_ParamsField.Create( _parent ) ;

    _field.Field    := sval1 ;
    _field.Scalable := bscalable ;
    _field.Factor   := dval  ;
  end;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then
    exit ;
  
  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _value, [':'] );

    if tkn.Result.Count > 0 then
      stype := tkn.Result[0]
    else
      stype := '' ;

    if tkn.Result.Count > 1 then
      sval1 := tkn.Result[1]
    else
      sval1 := '' ;

    if tkn.Result.Count > 2then
      sval2 := tkn.Result[2]
    else
      sval2 := '' ;
  finally
    FreeObject( tkn ) ;
  end ;

  if _stype( GIS_PARAMTXT_TYPE_SIZE     ) then begin
    _size := text_to_val( sval1 ) ;
    FreeObject( _field ) ;
  end
  else
  if _stype( GIS_PARAMTXT_TYPE_RENDERER ) then begin
    _size := GIS_RENDER_SIZE ;
    FreeObject( _field ) ;
  end
  else
  if _stype( GIS_PARAMTXT_TYPE_FIELD    ) then begin
    text_to_fld ;
  end
  else
    Abort ;
end ;

procedure size_from_text(
  const _parent : TGIS_ParamsAbstract ;
  const _value  : String              ;
  var   _size   : Integer
) ; overload;
var
  fld : TGIS_ParamsField ;
begin
  fld := nil  ;
  try
    size_from_text( _parent, _value, _size, fld )
  finally
    FreeObject( fld ) ;
  end;
end;


// Convert AsText metric property value (as FalseMAsText) to a proper field
// (as FalseMEx) and integer property value (as FalseM)
function measure_to_text(
  const _value  : String  ;
  const _size   : Double  ;
  const _field  : TGIS_ParamsField
) : String ;

  function fld_to_text : String ;
  var
    stmp : String ;
  begin
    stmp := DotFloatToStr( _field.Factor ) ;

    Result :=  stmp + GIS_PARAMTXT_SIZE_M ;
  end;

  function val_to_text : String ;
  var
    stmp : String ;
  begin
    stmp := DotFloatToStr( Abs( _size ) ) ;

    Result :=  stmp + GIS_PARAMTXT_SIZE_M ;
  end ;
begin
  if not IsStringEmpty( _value ) then begin
    Result := _value ;
  end
  else
  if assigned( _field ) and ( not IsStringEmpty( _field.Field ) ) then begin
    Result := GIS_PARAMTXT_TYPE_FIELD + ':'  + _field.Field + ':' + fld_to_text ;
  end
  else begin
    Result := GIS_PARAMTXT_TYPE_SIZE + ':'  + val_to_text ;
  end;
end;


// Convert metric size (as FlaseM) and field (as FalseMEx) to a proper string
// represtention for AsText property value (as FalseMAsText)
procedure measure_from_text(
  const _parent : TGIS_ParamsAbstract ;
  const _value  : String              ;
  var   _size   : Double              ;
  var   _field  : TGIS_ParamsField
) ; overload;
var
  tkn   : TGIS_Tokenizer ;
  stype : String  ;
  sval1 : String  ;
  sval2 : String  ;

  dval  : Double  ;

  function text_to_val(
    const _val : String
  ) : Double ;
  var
    s1   : String  ;
    s2   : String  ;

    function _s2( const _text : String ) : Boolean ;
    begin
      Result := CompareText( s2, _text ) = 0 ;
    end;
  begin
    split_size_val( _val, s1, s2 );

    Result := 0 ;
    dval   := 1 ;
    if      _s2( GIS_PARAMTXT_SIZE_IN    ) then begin
      dval   := DotStrToFloat( s1 ) * IN_TO_M
    end
    else if _s2( GIS_PARAMTXT_SIZE_FT    ) then begin
      dval   := DotStrToFloat( s1 ) * FT_TO_M
    end
    else if _s2( GIS_PARAMTXT_SIZE_YD    ) then begin
      dval   := DotStrToFloat( s1 ) * YD_TO_M
    end
    else if _s2( GIS_PARAMTXT_SIZE_MI    ) then begin
      dval   := DotStrToFloat( s1 ) * MI_TO_M
    end
    else if _s2( GIS_PARAMTXT_SIZE_MM    ) then begin
      dval   := DotStrToFloat( s1 ) * MM_TO_M
    end
    else if _s2( GIS_PARAMTXT_SIZE_CM    ) then begin
      dval   := DotStrToFloat( s1 ) * CM_TO_M
    end
    else if _s2( GIS_PARAMTXT_SIZE_M     ) then begin
      dval   := DotStrToFloat( s1 ) * M_TO_M
    end
    else if _s2( GIS_PARAMTXT_SIZE_KM    ) then begin
      dval   := DotStrToFloat( s1 ) * KM_TO_M
    end
    else if _s2( GIS_PARAMTXT_SIZE_NM    ) then begin
      dval   := DotStrToFloat( s1 ) * NM_TO_M
    end
    else
      Abort ;

    Result := dval ;
  end ;

  procedure text_to_fld ;
  begin
    if IsStringEmpty( sval1 ) or IsStringEmpty( sval2 ) then
      exit ;

    text_to_val( sval2 ) ;

    if not assigned( _field ) then
      _field := TGIS_ParamsField.Create( _parent ) ;

    _field.Field    := sval1 ;
    _field.Scalable := False ;
    _field.Factor   := dval  ;
  end;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then
    exit ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _value, [':'] );

    if tkn.Result.Count > 0 then
      stype := tkn.Result[0]
    else
      stype := '' ;

    if tkn.Result.Count > 1 then
      sval1 := tkn.Result[1]
    else
      sval1 := '' ;

    if tkn.Result.Count > 2 then
      sval2 := tkn.Result[2]
    else
      sval2 := '' ;
  finally
    FreeObject( tkn ) ;
  end ;

  if _stype( GIS_PARAMTXT_TYPE_SIZE ) then begin
    _size := text_to_val( sval1 ) ;
    FreeObject( _field ) ;
  end
  else
  if _stype( GIS_PARAMTXT_TYPE_FIELD ) then begin
    text_to_fld ;
  end
  else
    Abort ;
end ;

function angle_to_text(
  const _value       : Double ;
  const _angleastext : String ;
  const _field       : TGIS_ParamsField
) : String ;
  function val_to_text : String ;
  begin
    Result := DotFloatToStr( RoundS( RadToDeg( _value ) ) ) + ' ' +
    GIS_PARAMTXT_ROTATION_DEG ;
  end ;
begin
  if not IsStringEmpty( _angleastext ) then begin
    Result := _angleastext ;
  end
  else
  if assigned( _field ) and ( not IsStringEmpty( _field.Field ) ) then begin
    Result := GIS_PARAMTXT_TYPE_FIELD + ':' +
              _field.Field + ':' +
              DotFloatToStr( _field.Factor )+ ' ' +
              GIS_PARAMTXT_ROTATION_RAD
  end
  else
    Result := GIS_PARAMTXT_TYPE_ANGLE + ':'  + val_to_text ;
end ;

procedure angle_from_text(
  const _parent : TGIS_ParamsAbstract ;
  const _value  : String              ;
  var   _angle  : Double              ;
  var   _field  : TGIS_ParamsField
) ; overload;
var
  tkn : TGIS_Tokenizer ;
  stype : String  ;
  sval1 : String  ;
  sval2 : String  ;

  function text_to_val(
    const _val : String
  ) : Double ;
  var
    k : Integer ;
  begin
    k := Pos( UpperCase( GIS_PARAMTXT_ROTATION_DEG ), UpperCase( _val ) ) ;
    if k > StringFirst then begin
      Result :=
        DegToRad(
          DotStrToFloat( Trim( Copy( _val, StringFirst, k - StringFirst ) ) )
        ) ;
      exit ;
    end ;

    k := Pos( UpperCase( GIS_PARAMTXT_ROTATION_RAD ), UpperCase( _val ) ) ;
    if k > StringFirst then begin
      Result :=
        DotStrToFloat( Trim( Copy( _val, StringFirst, k - StringFirst ) ) ) ;
      exit ;
    end ;

    Result := 0.0 ;
  end ;

  procedure text_to_fld ;
  begin
    if IsStringEmpty( sval1 ) or IsStringEmpty( sval2 ) then
      exit ;

    text_to_val( sval2 ) ;

    if not assigned( _field ) then
      _field := TGIS_ParamsField.Create( _parent ) ;

    _field.Field    := sval1 ;
    _field.Scalable := false ;
    _field.Factor   := text_to_val( sval2 ) ;
  end;

  function _stype( const _text : String ) : Boolean ;
  begin
    Result := CompareText( stype, _text ) = 0 ;
  end;
begin
  if _value = GIS_PARAM_NIL then
    exit ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _value, [':'] );

    if tkn.Result.Count > 0 then
      stype := tkn.Result[0]
    else
      stype := '' ;

    if tkn.Result.Count > 1 then
      sval1 := tkn.Result[1]
    else
      sval1 := '' ;

    if tkn.Result.Count > 2then
      sval2 := tkn.Result[2]
    else
      sval2 := '' ;
  finally
    FreeObject( tkn ) ;
  end ;

  if _stype( GIS_PARAMTXT_TYPE_ANGLE ) then begin
    try
      _angle := text_to_val( sval1 ) ;
      FreeObject( _field ) ;
    except
      // do nothing
    end ;
  end
  else
  if _stype( GIS_PARAMTXT_TYPE_FIELD ) then begin
    text_to_fld ;
  end
  else
    Abort ;
end ;

procedure angle_from_text(
  const _parent : TGIS_ParamsAbstract ;
  const _value  : String              ;
  var   _angle  : Double
) ; overload;
var
  fld : TGIS_ParamsField ;
begin
  fld := nil  ;
  try
    angle_from_text( _parent, _value, _angle, fld ) ;
  finally
    FreeObject( fld ) ;
  end;
end;

{$ENDREGION}
{$REGION 'compute parameter utilities'}

// Compute field value from internal (extended) value specification
function  compute_field(
  const _shape         : TObject ;
  const _valueInternal : TGIS_ParamsField ;
  var   _fieldvalue    : Variant
) : Boolean ;// {$IFDEF DCC} inline ; {$ENDIF}
begin
  Result := False ;

  if assigned( _valueInternal )
     and
     ( not IsStringEmpty( _valueInternal.Field ) )
     and
     ( _shape <> nil )
  then begin
    try
      _fieldvalue := TGIS_Shape( _shape ).GetFieldEx( _valueInternal.Field ) ;
      if not ( VarIsEmpty( _fieldvalue ) or VarIsNull( _fieldvalue ) ) then
        Result := True ;
    except
    end ;
  end;
end;

// Compute size (like Width) for shape from property value and
// internal (extended) value specification
function  compute_size(
  const _shape         : TObject ;
  const _value         : Integer ;
  const _valueInternal : TGIS_ParamsField
) : Integer ;
var
  v   : Variant ;
  val : Double ;
begin
  Result := _value ;

  if compute_field( _shape, _valueInternal, v ) then begin
    try
      val := DotStrToFloat( VarToString( v ) ) * _valueInternal.Factor ;
      if _valueInternal.Scalable then
        val := val - GIS_AUTOSIZE_SIZE ;
      Result := RoundS( val ) ;
    except
      Result := 0 ;
    end ;
  end;
end ;

// Compute size (like FalseM) for shape from property value and
// internal (extended) value specification
function compute_measure(
  const _shape         : TObject ;
  const _value         : Double ;
  const _valueInternal : TGIS_ParamsField
) : Double ;
var
  v   : Variant ;
begin
  Result := _value ;

  if compute_field( _shape, _valueInternal, v ) then begin
    try
      Result := DotStrToFloat( VarToString( v ) ) * _valueInternal.Factor ;
    except
      Result := 0 ;
    end ;
  end ;
end ;

// Compute rotation for shape from property value and
// internal (extended) value specification
function compute_rotate(
  const _shape         : TObject ;
  const _value         : Double ;
  const _valueInternal : TGIS_ParamsField
) : Double ;
var
  v : Variant ;
begin
  Result := _value ;

  if compute_field( _shape, _valueInternal, v ) then begin
    try
      Result := DotStrToFloat( VarToString( v ) ) * _valueInternal.Factor ;
    except
      Result := 0 ;
    end ;
  end ;
end ;

// Compute color for shape from property value and
// internal (extended) value specification
function compute_color(
  const _shape         : TObject ;
  const _value         : TGIS_Color ;
  const _valueInternal : TGIS_ParamsField
) : TGIS_Color ;
var
  v : Variant ;
  c : Cardinal ;
begin
  Result := _value ;

  if compute_field( _shape, _valueInternal, v ) then begin
    try
      if _valueInternal.FSubType = 'CSS' then
        Result := TGIS_Color.FromString( VarToString( v ) )
      else begin
        c := Cardinal( StrToInt64( VarToString( v ) ) ) ;
        if _valueInternal.FSubType = 'ABGR' then
          Result := TGIS_Color.FromABGR( c )
        else
        if _valueInternal.FSubType = 'ARGB' then
          Result := TGIS_Color.FromARGB( c )
        else
        if _valueInternal.FSubType = 'BGR' then
          Result := TGIS_Color.FromBGR( c )
        else
        if _valueInternal.FSubType = 'RGB' then
          Result := TGIS_Color.FromRGB( c )
        else
          Result := TGIS_Color.FromBGR( c ) ;
      end ;
    except
    end ;
  end ;
end ;

// Compute label position for shape from property value and
// internal (extended) value specification
function compute_position(
  const _shape         : TObject ;
  const _value         : TGIS_LabelPositions ;
  const _valueInternal : TGIS_ParamsField
) : TGIS_LabelPositions ;
var
  v : Variant ;
begin
  Result := _value ;

  if compute_field( _shape, _valueInternal, v ) then begin
    try
      Result := ParamPosition(
                  String(v),
                 [TGIS_LabelPosition.UpRight]
                ) ;
    except
      Result := [TGIS_LabelPosition.UpRight] ;
    end ;
  end ;
end;

// Compute line style for shape from property value and
// internal (extended) value specification
function compute_style(
  const _shape         : TObject ;
  const _value         : TGIS_PenStyle ;
  const _valueInternal : TGIS_ParamsField
) : TGIS_PenStyle ;
var
  v : Variant ;
begin
  Result := _value ;

  if compute_field( _shape, _valueInternal, v ) then begin
    try
      Result := ParamPen(
                  String(v),
                 TGIS_PenStyle.Solid
                ) ;
    except
      Result := TGIS_PenStyle.Solid ;
    end ;
  end ;
end;


{$ENDREGION 'computeParameter utilities'}

var
  iLastSerial : Integer = 1 ;

  function SerialSeed : Integer ;
  begin
    iLastSerial := iLastSerial + 67 ; // prime seed
    if iLastSerial > (GIS_MAX_INTEGER - 1000) then
      iLastSerial := 1 ;
    Result := iLastSerial ;
  end ;

{$REGION 'TGIS_ParamsAbstract'}

constructor TGIS_ParamsAbstract.Create ;
begin
  inherited ;

  FSerialSeed := SerialSeed ;
  FSerial := FSerialSeed ;
  FUserObject := nil ;
end ;

{$IFDEF OXYGENE}
  procedure TGIS_ParamsAbstract.Dispose ;
  begin
    doDestroy ;
  end ;
{$ELSE}
  destructor TGIS_ParamsAbstract.Destroy ;
  begin
    doDestroy ;
    inherited ;
  end ;
{$ENDIF}

procedure TGIS_ParamsAbstract.doDestroy ;
begin
  // do nothing
  FreeObject( FUserObject ) ;
end ;

function TGIS_ParamsAbstract.fget_UserObject
  : TGIS_ParamsUserObject ;
begin
  Result := FUserObject ;
end ;

procedure TGIS_ParamsAbstract.fset_UserObject(
  const _value : TGIS_ParamsUserObject
) ;
begin
  FreeObject( FUserObject ) ;
  FUserObject := _value ;
end ;

function TGIS_ParamsAbstract.CreateCopy : TGIS_ParamsAbstract ;
begin
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      Result := Self.Class.getConstructor.newInstance() as TGIS_ParamsAbstract ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := TGIS_ParamsAbstract( Activator.CreateInstance( Self.GetType() ) ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      Result := TGIS_ParamsAbstract( Self.GetType().Instantiate ) ;
    {$ENDIF}
  {$ELSE}
    Result := TGIS_ParamsAbstractClass( Self.ClassType ).Create ;
  {$ENDIF}
  Result.FSerial := self.Serial ;
  if assigned( self.UserObject ) then
    Result.FUserObject := self.UserObject.CreateCopy ;

  Result.Assign( Self ) ;
end ;

procedure TGIS_ParamsAbstract.Touch ;
begin
  inc( FSerial ) ;
  if FSerial > (GIS_MAX_INTEGER - 1000) then
    FSerial := 1 ;
end ;

procedure TGIS_ParamsAbstract.ResetSerial ;
begin
  FSerial := FSerialSeed ;
end;

procedure TGIS_ParamsAbstract.Assign(
  _source: TPersistent
) ;
begin
  Touch ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsSpecific'}

function TGIS_ParamsSpecific.fget_Shape : TObject ;
begin
  if assigned( FParent ) and assigned( TGIS_ParamsSection(FParent).FShape ) then
    Result := TGIS_ParamsSection(FParent).FShape
  else
    Result := nil ;
end ;

function TGIS_ParamsSpecific.GetRoot
  : TGIS_ParamsAbstract ;
begin
  if assigned( FParent )
     and
     assigned( TGIS_ParamsSection( FParent ).FParamsList )
     and
     ( TGIS_ParamsSection( FParent ).FParamsList.Count > 0 )
  then
    Result := TGIS_ParamsSection( FParent ).FParamsList.Items[0]
  else if assigned( FParamsList ) and ( FParamsList.Count > 0 ) then
    Result := FParamsList.Items[0]
  else begin
    Result := TGIS_ParamsSection( FParent ) ;
  end ;
end ;

procedure TGIS_ParamsSpecific.Touch ;
begin
  inherited Touch ;
  if assigned( Parent ) and ( Parent is TGIS_ParamsAbstract ) then
    TGIS_ParamsAbstract( Parent ).Touch ;
end ;

procedure TGIS_ParamsSpecific.LoadFromStrings(
  const _str : {$IFDEF OXYGENE}
                 TGIS_Strings
               {$ELSE}
                 TStrings
               {$ENDIF}
) ;
var
  cfg : TGIS_Config ;
begin
  cfg := TGIS_ConfigFactory.CreateConfig( nil, '' ) ;
  try
    _str.Insert( 0, Format( '[%s]', [GIS_INI_LAYER_HEADER] ) ) ;
    cfg.SetStrings( _str ) ;
    _str.Delete( 0 ) ;
    cfg.SetLayer( nil ) ;
    LoadFromConfig( cfg ) ;
  finally
    FreeObject( cfg ) ;
  end ;
end ;

procedure TGIS_ParamsSpecific.SaveToStrings(
  const _str : {$IFDEF OXYGENE}
                 TGIS_Strings
               {$ELSE}
                 TStrings
               {$ENDIF}
) ;
var
  cfg : TGIS_Config ;
begin
  cfg := TGIS_ConfigFactory.CreateConfig( nil, '' ) ;
  try
    SaveToConfig( cfg ) ;
    cfg.GetStrings( _str ) ;
    if _str.Count > 0 then
      _str.Delete( 0 ) ;
  finally
    FreeObject( cfg ) ;
  end ;
end ;

//==============================================================================
// TGIS_ParamsVector
//==============================================================================

procedure TGIS_ParamsVector.assignInternal(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsVector.Create
  else
    src := _source ;
  try
    inherited Assign( src ) ;
  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

function TGIS_ParamsVector.prepareParamsVector
  : TGIS_ParamsSectionVector ;
var
  tmp_parent : TPersistent ;
begin
  if assigned( FParentProxy ) then
    tmp_parent := FParentProxy
  else
    tmp_parent := FParent ;

  if assigned( tmp_parent ) and
     not assigned ( TGIS_Shape( TGIS_ParamsSectionVector( tmp_parent ).FShape ) )
  then
     Result := nil
  else if assigned( tmp_parent ) and
     TGIS_Shape( TGIS_ParamsSectionVector( tmp_parent ).FShape ).LocalParams
  then
    Result := TGIS_Shape( TGIS_ParamsSectionVector( tmp_parent ).FShape ).Params
  else begin
    Result := TGIS_ParamsSectionVector.Create ;
    Result.FShape := TGIS_Shape( TGIS_ParamsSectionVector( tmp_parent ).FShape ) ;
    TGIS_Shape( TGIS_ParamsSectionVector( tmp_parent ).FShape
              ).SetParamsInternal( Result ) ;
  end ;
end ;

procedure TGIS_ParamsVector.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  // just for safe inheritance
end ;

procedure TGIS_ParamsVector.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  // just for safe inheritance
end ;

function TGIS_ParamsVector.prepareParams
  : TGIS_ParamsVector ;
begin
  // just for safe inheritance
  Result := TGIS_ParamsVector.Create ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsRender'}

constructor TGIS_ParamsRender.Create ;
begin
  inherited ;

  FMinVal                 := 0 ; // band of the renderer is disabled by default
  FMaxVal                 := 0 ;
  FZones                  := 0 ;

  FMinValEx               := 0 ; // band of the renderer is disabled by default
  FMaxValEx               := 0 ;
  FZonesEx                := 0 ;

  FSizeDefault            := GIS_RENDER_SIZE  ;
  FColorDefault           := TGIS_Color.RenderColor ;

  FStartSize              := 1 ;
  FEndSize                := 480 ;
  FSizeDefault            := 120 ;
  FStartColor             := TGIS_Color.White ;
  FEndColor               := TGIS_Color.Green ;
  FColorDefault           := TGIS_Color.Navy  ;
  FStartSizeEx            := 1 ;
  FEndSizeEx              := 480 ;
  FStartColorEx           := TGIS_Color.Red   ;
  FEndColorEx             := TGIS_Color.White ;

  FRound                  := 0 ;
  FInterpolationBase      := 1.0 ;
  FColorInterpolationMode := TGIS_ColorInterpolationMode.RGB ;
  FColorRamp              := nil ;
end ;

procedure TGIS_ParamsRender.doDestroy ;
var
  i    : Integer  ;
  obj  : TGIS_SqlQuery ;
begin
  {$IFNDEF NEXTGEN}
  if assigned( FChartObj ) then begin
    for i:= 0 to FChartObj.Count -1 do begin
      obj := TGIS_SqlQuery( FChartObj[ i ] ) ;
      FreeObject( obj ) ;
    end ;
  end ;
  {$ENDIF}
  FreeObject( FChartObj      ) ;

  FreeObject( FExpressionObj ) ;

  inherited ;
end ;

function TGIS_ParamsRender.fget_Expression
  : String ;
begin
  if assigned( FExpressionObj ) then Result := FExpressionObj.Query
                                else Result := '' ;
end ;

procedure TGIS_ParamsRender.fset_Expression(
  const _value : String
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if assigned( FExpressionObj ) then begin
      if _value = FExpressionObj.Query then exit ;
    end
    else begin
      if IsStringEmpty( _value ) then exit ;
    end ;
    FreeObject( FExpressionObj ) ;
    if not IsStringEmpty( _value ) then begin
      FExpressionObj := TGIS_SqlQuery.Create ;
      FExpressionObj.Prepare( _value ) ;
    end ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_Chart(
  const _value : String
) ;
var
  prm  : TGIS_ParamsRender ;
  i    : Integer  ;
  itm : String   ;
  c    : Char     ;
  any  : Boolean  ;
  btmp : Boolean  ;
  obj  : TGIS_SqlQuery ;

  function parse( _prm : TGIS_ParamsRender ) : Boolean ;
  begin
    obj := nil ;
    try
      if IsStringEmpty( itm ) then begin
        Result := False ;
        if _prm.FChartObj.Count < 2 then
          itm := '0'
        else
          exit ;
      end
      else
        Result := True ;

      obj := TGIS_SqlQuery.Create ;
      try
        obj.Prepare( itm ) ;
      except
        obj.Prepare( '0' ) ;
      end ;
    finally
      _prm.FChartObj.Add( obj ) ;
      itm := '' ;
    end ;
  end ;

begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FChart = _value then exit ;

    if assigned( FChartObj ) then begin
      {$IFNDEF NEXTGEN}
      for i:= 0 to FChartObj.Count -1 do begin
        obj := TGIS_SqlQuery( FChartObj[ i ] ) ;
        FreeObject( obj ) ;
      end ;
      {$ENDIF}
      FreeObject ( FChartObj ) ;
    end ;

    any := False ;

    if not IsStringEmpty( _value ) then begin
      FChartObj := TList<TGIS_SqlQuery>.Create ;

      itm := '' ;
      for i := StringFirst to StringLast( _value ) do begin
        c := _value[i] ;
        if c = ':' then begin
          btmp := parse( prm ) ;
          any  := any or btmp ;
          continue ;
        end
        else
         itm := itm + c ;
      end ;
      if length( itm ) > 0 then begin
        btmp := parse( prm ) ;
        any  := any or btmp ;
      end ;
    end ;

    if any then
      FChart := _value
    else
      FChart := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_MinVal(
  const _value : Double
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FMinVal = _value then exit ;
    FMinVal := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_MaxVal(
  const _value : Double
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FMaxVal = _value then exit ;
    FMaxVal := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_Zones(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FZones = _value then exit ;
    FZones := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_MinValEx(
  const _value : Double
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FMinValEx = _value then exit ;
    FMinValEx := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_MaxValEx(
  const _value : Double
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FMaxValEx = _value then exit ;
    FMaxValEx := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_ZonesEx(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FZonesEx = _value then exit ;
    FZonesEx := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_SizeDefault(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FSizeDefault = _value then exit ;
    FSizeDefault := _value ;
    FSizeDefaultAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_ColorDefault(
  const _value : TGIS_Color
) ;
var
  prm : TGIS_ParamsRender ;
  cl  : TGIS_Color ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    cl := _value ;
    if FColorDefault.ARGB = cl.ARGB then exit ;
    FColorDefault := cl ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_StartSize(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FStartSize = _value then exit ;
    FStartSize := _value ;
    FStartSizeAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_EndSize(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FEndSize = _value then exit ;
    FEndSize := _value ;
    FEndSizeAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_StartColor(
  const _value : TGIS_Color
) ;
var
  prm : TGIS_ParamsRender ;
  cl  : TGIS_Color ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    cl := _value ;
    if FStartColor.ARGB = cl.ARGB then exit ;
    FStartColor := cl ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_EndColor(
  const _value : TGIS_Color
) ;
var
  prm : TGIS_ParamsRender ;
  cl  : TGIS_Color ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    cl := _value ;
    if FEndColor.ARGB = cl.ARGB then exit ;
    FEndColor := cl ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_StartSizeEx(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FStartSizeEx = _value then exit ;
    FStartSizeEx := _value ;
    FStartSizeExAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_EndSizeEx(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FEndSizeEx = _value then exit ;
    FEndSizeEx := _value ;
    FEndSizeExAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_StartColorEx(
  const _value : TGIS_Color
) ;
var
  prm : TGIS_ParamsRender ;
  cl  : TGIS_Color ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    cl := _value ;
    if FStartColorEx.ARGB = cl.ARGB then exit ;
    FStartColorEx := cl ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_EndColorEx(
  const _value : TGIS_Color
) ;
var
  prm : TGIS_ParamsRender ;
  cl  : TGIS_Color ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    cl := _value ;
    if FEndColorEx.ARGB = cl.ARGB then exit ;
    FEndColorEx := cl ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_Round(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FRound = _value then exit ;
    FRound := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_InterpolationBase(
  const _value : Double
) ;
var
  prm : TGIS_ParamsRender ;
  value : Double ;
begin
  value := _value ;
  if _value <= 0 then
    value := 1 ;

  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FInterpolationBase = value then exit ;
    FInterpolationBase := value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_ColorInterpolationMode(
  const _value : TGIS_ColorInterpolationMode
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    if FColorInterpolationMode = _value then exit ;
    FColorInterpolationMode := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsRender.fset_ColorRamp(
  const _value : TGIS_ColorMapArray
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  with prm do begin
    FColorRamp := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsRender.fget_SizeDefaultAsText
  : String ;
begin
  Result := size_to_text( FSizeDefaultAsText, SizeDefault, nil ) ;
end;

procedure TGIS_ParamsRender.fset_SizeDefaultAsText(
  const _value : String
);
var
  prm       : TGIS_ParamsRender ;
  itmp_size : Integer ;
begin
  itmp_size := SizeDefault ;

  try
    size_from_text( self, _value, itmp_size );

    SizeDefault := itmp_size ;

    prm := TGIS_ParamsRender( prepareParams ) ;
    with prm do begin
      if FSizeDefaultAsText <> _value then begin
        FSizeDefaultAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_RENDER_SIZE_DEF,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsRender.fget_ColorDefaultAsText
  : String ;
begin
  Result := color_to_text( ColorDefault, nil ) ;
end;

procedure TGIS_ParamsRender.fset_ColorDefaultAsText(
  const _value : String
);
var
  tmp_color : TGIS_Color ;
begin
  tmp_color := ColorDefault ;

  try
    color_from_text( _value, tmp_color );

    ColorDefault := tmp_color ;
  except
    TGIS_Logger.AsWarning(
      'Bad ColorDefault', '"' + _value + '"');
  end;
end;

function TGIS_ParamsRender.fget_StartSizeAsText
  : String ;
begin
  Result := size_to_text( FStartSizeAsText, StartSize, nil ) ;
end;

procedure TGIS_ParamsRender.fset_StartSizeAsText(
  const _value : String
);
var
  prm       : TGIS_ParamsRender ;
  itmp_size : Integer ;
begin
  itmp_size := StartSize ;

  try
    size_from_text( self, _value, itmp_size );

    StartSize := itmp_size ;

    prm := TGIS_ParamsRender( prepareParams ) ;
    with prm do begin
      if FStartSizeAsText <> _value then begin
        FStartSizeAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_RENDER_STARTSIZE,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsRender.fget_EndSizeAsText
  : String ;
begin
  Result := size_to_text( FEndSizeAsText, EndSize, nil ) ;
end;

procedure TGIS_ParamsRender.fset_EndSizeAsText(
  const _value : String
);
var
  prm       : TGIS_ParamsRender ;
  itmp_size : Integer ;
begin
  itmp_size := EndSize ;

  try
    size_from_text( self, _value, itmp_size );

    EndSize := itmp_size ;

    prm := TGIS_ParamsRender( prepareParams ) ;
    with prm do begin
      if FEndSizeAsText <> _value then begin
        FEndSizeAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_RENDER_ENDSIZE,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsRender.fget_StartColorAsText
  : String ;
begin
  Result := color_to_text( StartColor, nil ) ;
end;

procedure TGIS_ParamsRender.fset_StartColorAsText(
  const _value : String
);
var
  tmp_color : TGIS_Color ;
begin
  tmp_color := StartColor ;

  try
    color_from_text( _value, tmp_color );

    StartColor := tmp_color ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_RENDER_STARTCOLOR,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsRender.fget_EndColorAsText
  : String ;
begin
  Result := color_to_text( EndColor, nil ) ;
end;

procedure TGIS_ParamsRender.fset_EndColorAsText(
  const _value : String
);
var
  tmp_color : TGIS_Color ;
begin
  tmp_color := EndColor ;

  try
    color_from_text( _value, tmp_color );

    EndColor := tmp_color ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_RENDER_ENDCOLOR,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsRender.fget_StartSizeExAsText
  : String ;
begin
  Result := size_to_text( FStartSizeExAsText, StartSizeEx, nil ) ;
end;

procedure TGIS_ParamsRender.fset_StartSizeExAsText(
  const _value : String
);
var
  prm       : TGIS_ParamsRender ;
  itmp_size : Integer ;
begin
  itmp_size := StartSizeEx ;

  try
    size_from_text( self, _value, itmp_size );

    prm := TGIS_ParamsRender( prepareParams ) ;
    with prm do begin
      if FStartSizeExAsText <> _value then begin
        FStartSizeEx := itmp_size ;
        FStartSizeExAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_RENDER_STARTSIZE_EX,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsRender.fget_EndSizeExAsText
  : String ;
begin
  Result := size_to_text( FEndSizeExAsText, EndSizeEx, nil ) ;
end;

procedure TGIS_ParamsRender.fset_EndSizeExAsText(
  const _value : String
);
var
  prm       : TGIS_ParamsRender ;
  itmp_size : Integer ;
begin
  itmp_size := EndSizeEx ;

  try
    size_from_text( self, _value, itmp_size );

    EndSizeEx := itmp_size ;

    prm := TGIS_ParamsRender( prepareParams ) ;
    with prm do begin
      if FEndSizeExAsText <> _value then begin
        FEndSizeExAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_RENDER_ENDSIZE_EX,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsRender.fget_StartColorExAsText
  : String ;
begin
  Result := color_to_text( StartColorEx, nil ) ;
end;

procedure TGIS_ParamsRender.fset_StartColorExAsText(
  const _value : String
);
var
  tmp_color : TGIS_Color ;
begin
  tmp_color := StartColorEx ;

  try
    color_from_text( _value, tmp_color );

    StartColorEx := tmp_color ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_RENDER_STARTCOLOR_EX,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsRender.fget_EndColorExAsText
  : String ;
begin
  Result := color_to_text( EndColorEx, nil ) ;
end;

procedure TGIS_ParamsRender.fset_EndColorExAsText(
  const _value : String
);
var
  tmp_color : TGIS_Color ;
begin
  tmp_color := EndColorEx ;

  try
    color_from_text( _value, tmp_color );

    EndColorEx := tmp_color ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_RENDER_ENDCOLOR_EX,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsRender.prepareParams
  : TGIS_ParamsVector ;
var
  {$IFDEF DCC} [unsafe] {$ENDIF}
  params : TGIS_ParamsSectionVector ;
begin
  if assigned( FParentProxy ) and
     assigned( TGIS_ParamsSectionVector(FParentProxy).FShape ) then
  begin
    params := prepareParamsVector ;
    if not assigned( params.FRender ) then begin
      params.FRender := TGIS_ParamsRender( CreateCopy ) ;
      params.FRender.Parent := params ;
    end ;
    Result := params.FRender ;
  end
  else
    Result := self ;
end ;

procedure TGIS_ParamsRender.assignInternal(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsRender.Create
  else
    src := _source ;
  try
    if src is TGIS_ParamsRender then begin
      Expression              := TGIS_ParamsRender(src).Expression ;
      Chart                   := TGIS_ParamsRender(src).Chart ;
      FMinVal                 := TGIS_ParamsRender(src).MinVal ;
      FMaxVal                 := TGIS_ParamsRender(src).MaxVal ;
      FZones                  := TGIS_ParamsRender(src).Zones ;
      FMinValEx               := TGIS_ParamsRender(src).MinValEx ;
      FMaxValEx               := TGIS_ParamsRender(src).MaxValEx ;
      FZonesEx                := TGIS_ParamsRender(src).ZonesEx ;
      FSizeDefault            := TGIS_ParamsRender(src).SizeDefault ;
      FSizeDefaultAsText      := TGIS_ParamsRender(src).SizeDefaultAsText ;
      FColorDefault           := TGIS_ParamsRender(src).ColorDefault ;
      FStartSize              := TGIS_ParamsRender(src).StartSize ;
      FStartSizeAsText        := TGIS_ParamsRender(src).StartSizeAsText ;
      FEndSize                := TGIS_ParamsRender(src).EndSize ;
      FEndSizeAsText          := TGIS_ParamsRender(src).EndSizeAsText ;
      FStartColor             := TGIS_ParamsRender(src).StartColor ;
      FEndColor               := TGIS_ParamsRender(src).EndColor ;
      FStartSizeEx            := TGIS_ParamsRender(src).StartSizeEx ;
      FStartSizeExAsText      := TGIS_ParamsRender(src).StartSizeExAsText ;
      FEndSizeEx              := TGIS_ParamsRender(src).EndSizeEx ;
      FEndSizeExAsText        := TGIS_ParamsRender(src).EndSizeExAsText ;
      FStartColorEx           := TGIS_ParamsRender(src).StartColorEx ;
      FEndColorEx             := TGIS_ParamsRender(src).EndColorEx ;
      FRound                  := TGIS_ParamsRender(src).Round ;
      FInterpolationBase      := TGIS_ParamsRender(src).InterpolationBase ;
      FColorInterpolationMode := TGIS_ParamsRender(src).ColorInterpolationMode ;
      FColorRamp              := TGIS_ParamsRender(src).ColorRamp ;
    end ;
    inherited assignInternal(src) ;
  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsRender.Assign(
  _source: TPersistent
) ;
var
  prm : TGIS_ParamsRender ;
begin
  prm := TGIS_ParamsRender( prepareParams ) ;
  prm.assignInternal(_source) ;
end ;

procedure TGIS_ParamsRender.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  with TGIS_Config( _cfg ) do begin
    oConfig := TGIS_Config( _cfg ) ;

    if config_old_format( _cfg ) then begin
      Expression             := ReadString    ( GIS_INI_RENDER_EXPRESSION,
                                                Expression
                                              ) ;
      Chart                  := ReadString    ( GIS_INI_RENDER_CHART,
                                                Chart
                                              ) ;
      MinVal                 := ReadFloat     ( GIS_INI_RENDER_MINVAL,
                                                MinVal
                                              ) ;
      MaxVal                 := ReadFloat     ( GIS_INI_RENDER_MAXVAL,
                                                MaxVal
                                              ) ;
      Zones                  := ReadInteger   ( GIS_INI_RENDER_ZONES,
                                                Zones
                                              ) ;
      MinValEx               := ReadFloat     ( GIS_INI_RENDER_MINVAL_EX,
                                                MinValEx
                                              ) ;
      MaxValEx               := ReadFloat     ( GIS_INI_RENDER_MAXVAL_EX,
                                                MaxValEx
                                              ) ;
      ZonesEx                := ReadInteger   ( GIS_INI_RENDER_ZONES_EX,
                                                ZonesEx
                                              ) ;
      SizeDefault            := ReadInteger   ( GIS_INI_RENDER_SIZE_DEF,
                                                SizeDefault
                                              ) ;
      ColorDefault           := ReadColor     ( GIS_INI_RENDER_COLOR_DEF,
                                                ColorDefault
                                              ) ;
      StartSize              := ReadInteger   ( GIS_INI_RENDER_STARTSIZE,
                                                StartSize
                                              ) ;
      EndSize                := ReadInteger   ( GIS_INI_RENDER_ENDSIZE,
                                                EndSize
                                              ) ;
      StartColor             := ReadColor     ( GIS_INI_RENDER_STARTCOLOR,
                                                StartColor
                                              ) ;
      EndColor               := ReadColor     ( GIS_INI_RENDER_ENDCOLOR,
                                                EndColor
                                              ) ;
      StartSizeEx            := ReadInteger   ( GIS_INI_RENDER_STARTSIZE_EX,
                                                StartSizeEx
                                              ) ;
      EndSizeEx              := ReadInteger   ( GIS_INI_RENDER_ENDSIZE_EX,
                                                EndSizeEx
                                              ) ;
      StartColorEx           := ReadColor     ( GIS_INI_RENDER_STARTCOLOR_EX,
                                                StartColorEx
                                              ) ;
      EndColorEx             := ReadColor     ( GIS_INI_RENDER_ENDCOLOR_EX,
                                                EndColorEx
                                              ) ;
      Round                  := ReadInteger   ( GIS_INI_RENDER_ROUND,
                                                Round
                                              ) ;
    end ;

    if config_new_format( _cfg ) then begin
      Expression             := ReadString    ( GIS_INI_RENDER_EXPRESSION,
                                                Expression
                                              ) ;
      Chart                  := ReadString    ( GIS_INI_RENDER_CHART,
                                                Chart
                                              ) ;
      MinVal                 := ReadFloat     ( GIS_INI_RENDER_MINVAL,
                                                MinVal
                                              ) ;
      MaxVal                 := ReadFloat     ( GIS_INI_RENDER_MAXVAL,
                                                MaxVal
                                              ) ;
      Zones                  := ReadInteger   ( GIS_INI_RENDER_ZONES,
                                                Zones
                                              ) ;
      MinValEx               := ReadFloat     ( GIS_INI_RENDER_MINVAL_EX,
                                                MinValEx
                                              ) ;
      MaxValEx               := ReadFloat     ( GIS_INI_RENDER_MAXVAL_EX,
                                                MaxValEx
                                              ) ;
      ZonesEx                := ReadInteger   ( GIS_INI_RENDER_ZONES_EX,
                                                ZonesEx
                                              ) ;
      SizeDefaultAsText      := ReadString    ( GIS_INI_RENDER_SIZE_DEF,
                                                SizeDefaultAsText
                                              ) ;
      ColorDefaultAsText     := ReadString    ( GIS_INI_RENDER_COLOR_DEF,
                                                ColorDefaultAsText
                                              ) ;
      StartSizeAsText        := ReadString    ( GIS_INI_RENDER_STARTSIZE,
                                                StartSizeAsText
                                              ) ;
      EndSizeAsText          := ReadString    ( GIS_INI_RENDER_ENDSIZE,
                                                EndSizeAsText
                                              ) ;
      StartColorAsText       := ReadString    ( GIS_INI_RENDER_STARTCOLOR,
                                                StartColorAsText
                                              ) ;
      EndColorAsText         := ReadString    ( GIS_INI_RENDER_ENDCOLOR,
                                                EndColorAsText
                                              ) ;
      StartSizeExAsText      := ReadString    ( GIS_INI_RENDER_STARTSIZE_EX,
                                                StartSizeExAsText
                                              ) ;
      EndSizeExAsText        := ReadString    ( GIS_INI_RENDER_ENDSIZE_EX,
                                                EndSizeExAsText
                                              ) ;
      StartColorExAsText     := ReadString    ( GIS_INI_RENDER_STARTCOLOR_EX,
                                                StartColorExAsText
                                              ) ;
      EndColorExAsText       := ReadString    ( GIS_INI_RENDER_ENDCOLOR_EX,
                                                EndColorExAsText
                                              ) ;
      Round                  := ReadInteger   ( GIS_INI_RENDER_ROUND,
                                                Round
                                              ) ;
      InterpolationBase      := ReadFloat     ( GIS_XML_RENDER_INTERPOLATION_BASE,
                                                InterpolationBase
                                              ) ;
      ColorInterpolationMode := ReadColorInterpolationMode
                                              ( GIS_XML_RENDER_COLOR_INTERPOLATION_MODE,
                                                ColorInterpolationMode
                                              ) ;
      ColorRamp              := ReadColorRamp ( GIS_XML_RENDER_COLORRAMP,
                                                ColorRamp
                                              ) ;
    end ;

    oConfig := nil ;
  end ;
end ;

procedure TGIS_ParamsRender.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
const
  RANDOM_VAL = 624799894.583435833 ;
var
  def : TGIS_ParamsRender;
begin

  def := TGIS_ParamsRender.Create ;
  try
    with TGIS_Config( _cfg ) do begin
      if config_old_format( _cfg ) then begin
        WriteString                          ( GIS_INI_RENDER_EXPRESSION,
                                               Expression,
                                               def.Expression
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_CHART,
                                               Chart,
                                               def.Chart
                                             ) ;
        WriteFloat                           ( GIS_INI_RENDER_MINVAL,
                                               MinVal,
                                               RANDOM_VAL
                                             ) ;
        WriteFloat                           ( GIS_INI_RENDER_MAXVAL,
                                               MaxVal,
                                               RANDOM_VAL
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_ZONES,
                                               Zones,
                                               def.Zones
                                             ) ;
        WriteFloat                           ( GIS_INI_RENDER_MINVAL_EX,
                                               MinValEx,
                                               RANDOM_VAL
                                             ) ;
        WriteFloat                           ( GIS_INI_RENDER_MAXVAL_EX,
                                               MaxValEx,
                                               RANDOM_VAL
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_ZONES_EX,
                                               make_size_DK10( ZonesEx ),
                                               def.ZonesEx
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_SIZE_DEF,
                                               make_size_DK10( SizeDefault ),
                                               def.SizeDefault
                                             ) ;
        WriteColor                           ( GIS_INI_RENDER_COLOR_DEF,
                                               ColorDefault,
                                               def.ColorDefault
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_STARTSIZE,
                                               make_size_DK10( StartSize ),
                                               def.StartSize
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_ENDSIZE,
                                               make_size_DK10( EndSize ),
                                               def.EndSize
                                             ) ;
        WriteColor                           ( GIS_INI_RENDER_STARTCOLOR,
                                               StartColor,
                                               def.StartColor
                                             ) ;
        WriteColor                           ( GIS_INI_RENDER_ENDCOLOR,
                                               EndColor,
                                               def.EndColor
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_STARTSIZE_EX,
                                               make_size_DK10( StartSizeEx ),
                                               def.StartSizeEx
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_ENDSIZE_EX,
                                               make_size_DK10( EndSizeEx ),
                                               def.EndSizeEx
                                             ) ;
        WriteColor                           ( GIS_INI_RENDER_STARTCOLOR_EX,
                                               StartColorEx,
                                               def.StartColorEx
                                             ) ;
        WriteColor                           ( GIS_INI_RENDER_ENDCOLOR_EX,
                                               EndColorEx,
                                               def.EndColorEx
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_ROUND,
                                               Round,
                                               def.Round
                                             ) ;
      end;

      if config_new_format( _cfg ) then begin
        WriteString                          ( GIS_INI_RENDER_EXPRESSION,
                                               Expression,
                                               def.Expression
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_CHART,
                                               Chart,
                                               def.Chart
                                             ) ;
        WriteFloat                           ( GIS_INI_RENDER_MINVAL,
                                               MinVal,
                                               RANDOM_VAL
                                             ) ;
        WriteFloat                           ( GIS_INI_RENDER_MAXVAL,
                                               MaxVal,
                                               RANDOM_VAL
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_ZONES,
                                               Zones,
                                               def.Zones
                                             ) ;
        WriteFloat                           ( GIS_INI_RENDER_MINVAL_EX,
                                               MinValEx,
                                               RANDOM_VAL
                                             ) ;
        WriteFloat                           ( GIS_INI_RENDER_MAXVAL_EX,
                                               MaxValEx,
                                               RANDOM_VAL
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_ZONES_EX,
                                               ZonesEx,
                                               def.ZonesEx
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_SIZE_DEF,
                                               SizeDefaultAsText,
                                               def.SizeDefaultAsText
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_COLOR_DEF,
                                               ColorDefaultAsText,
                                               def.ColorDefaultAsText
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_STARTSIZE,
                                               StartSizeAsText,
                                               def.StartSizeAsText
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_ENDSIZE,
                                               EndSizeAsText,
                                               def.EndSizeAsText
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_STARTCOLOR,
                                               StartColorAsText,
                                               def.StartColorAsText
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_ENDCOLOR,
                                               EndColorAsText,
                                               def.EndColorAsText
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_STARTSIZE_EX,
                                               StartSizeExAsText,
                                               def.StartSizeExAsText
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_ENDSIZE_EX,
                                               EndSizeExAsText,
                                               def.EndSizeExAsText
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_STARTCOLOR_EX,
                                               StartColorExAsText,
                                               def.StartColorExAsText
                                             ) ;
        WriteString                          ( GIS_INI_RENDER_ENDCOLOR_EX,
                                               EndColorExAsText,
                                               def.EndColorExAsText
                                             ) ;
        WriteInteger                         ( GIS_INI_RENDER_ROUND,
                                               Round,
                                               def.Round
                                             ) ;
        WriteFloat                           ( GIS_XML_RENDER_INTERPOLATION_BASE,
                                               InterpolationBase,
                                               def.InterpolationBase
                                             ) ;
        WriteColorInterpolationMode          ( GIS_XML_RENDER_COLOR_INTERPOLATION_MODE,
                                               ColorInterpolationMode,
                                               def.ColorInterpolationMode
                                             ) ;
        WriteColorRamp                       ( GIS_XML_RENDER_COLORRAMP,
                                               ColorRamp,
                                               def.ColorRamp
                                             ) ;
      end ;
    end ;
  finally
    FreeObject( def ) ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsPixel' }

constructor TGIS_ParamsPixel.Create ;
begin
  inherited ;

  FInversion        := False ;
  FGrayScale        := False ;
  FHistogram        := False ;
  FHistogramPath    := ''    ;
  FPage             := 1 ;

  FRedBand          := 0 ;
  FGreenBand        := 0 ;
  FBlueBand         := 0 ;
  FAlphaBand        := 0 ;
  FGridBand         := 0 ;
  FGridNoValue      := GIS_GRID_NOVALUE ;
  FGridShadow       := True  ;
  FGridSmoothColors := False ;
  FGridShadowAngle  := 90 ;

  FRed              := 0 ;
  FGreen            := 0 ;
  FBlue             := 0 ;
  FBrightness       := 0 ;
  FContrast         := 0 ;
  FContrastEnhanced := False ;
  FMinZ             :=  GIS_MAX_SINGLE ; // negated value by design to allow
  FMaxZ             := -GIS_MAX_SINGLE ; // recognition when value are set
  FMinThresholdZ    := -GIS_MAX_SINGLE ;
  FMaxThresholdZ    :=  GIS_MAX_SINGLE ;
  FLegendImage      := nil ;
  FAntialias        := True ;

  FTransparentZones := TGIS_StringList.Create ;
  FGrayMapZones     := TGIS_StringList.Create ;
  FRedMapZones      := TGIS_StringList.Create ;
  FGreenMapZones    := TGIS_StringList.Create ;
  FBlueMapZones     := TGIS_StringList.Create ;
  FFullRGBMapZones  := TGIS_StringList.Create ;
  FAltitudeMapZones := TGIS_StringList.Create ;

  FColorRamp        := nil ;

  {$IFDEF OXYGENE}
    FTransparentZones.OnChange := @doTransparentZonesChange ;
    FGrayMapZones.OnChange     := @doGrayMapZonesChange     ;
    FRedMapZones.OnChange      := @doRedMapZonesChange      ;
    FGreenMapZones.OnChange    := @doGreeenMapZonesChange   ;
    FBlueMapZones.OnChange     := @doBlueMapZonesChange     ;
    FFullRGBMapZones.OnChange  := @doFullRGBMapZonesChange  ;
    FAltitudeMapZones.OnChange := @doAltitudMapZonesChange  ;
  {$ELSE}
    FTransparentZones.OnChange :=  doTransparentZonesChange ;
    FGrayMapZones.OnChange     :=  doGrayMapZonesChange     ;
    FRedMapZones.OnChange      :=  doRedMapZonesChange      ;
    FGreenMapZones.OnChange    :=  doGreeenMapZonesChange   ;
    FBlueMapZones.OnChange     :=  doBlueMapZonesChange     ;
    FFullRGBMapZones.OnChange  :=  doFullRGBMapZonesChange  ;
    FAltitudeMapZones.OnChange :=  doAltitudMapZonesChange  ;
  {$ENDIF}
end ;

procedure TGIS_ParamsPixel.doDestroy ;
begin

  FreeObject( FFullRGBMapZones  ) ;
  FreeObject( FBlueMapZones     ) ;
  FreeObject( FGreenMapZones    ) ;
  FreeObject( FRedMapZones      ) ;
  FreeObject( FGrayMapZones     ) ;
  FreeObject( FTransparentZones ) ;
  FreeObject( FAltitudeMapZones ) ;
  FreeObject( FLegendImage      ) ;

  inherited ;
end ;

procedure TGIS_ParamsPixel.fset_ShowLegend(
  const _value : Boolean
) ;
begin
  if FShowLegend = _value then exit ;

  FShowLegend := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_Page(
  const _value : Integer
) ;
begin
  if FPage = _value then exit ;

  if _value < 0  then FPage := 0
  else                FPage := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_LegendImage(
  const _value : TGIS_Bitmap
) ;
begin
  if FLegendImage = _value then exit ;

  FreeObject( FLegendImage ) ;
  FLegendImage := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
end ;


procedure TGIS_ParamsPixel.fset_RedBand(
  const _value : Integer
) ;
begin
  if FRedBand = _value then exit ;

  if _value < 0  then FRedBand := -1
  else                FRedBand := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_GreenBand(
  const _value : Integer
) ;
begin
  if FGreenBand = _value then exit ;

  if _value < 0  then FGreenBand := -1
  else                FGreenBand := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_BlueBand(
  const _value : Integer
) ;
begin
  if FBlueBand = _value then exit ;

  if _value < 0  then FBlueBand := -1
  else                FBlueBand:= _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_AlphaBand(
  const _value : Integer
) ;
begin
  if FAlphaBand = _value then exit ;

  if _value < 0  then FAlphaBand := -1
  else                FAlphaBand:= _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_GridBand(
  const _value : Integer
) ;
begin
  if FGridBand = _value then exit ;

  if _value < 0  then FGridBand := -1
  else                FGridBand:= _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_GridNoValue(
  const _value : Double
) ;
begin
  if FGridNoValue = _value then exit ;

  FGridNoValue := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_GridShadow(
  const _value : Boolean
) ;
begin
  if FGridShadow = _value then exit ;

  FGridShadow := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_GridShadowAngle(
  const _value : Double
) ;
begin
  if FGridShadowAngle = _value then exit ;

  FGridShadowAngle := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_GridSmoothColors(
  const _value : Boolean
) ;
begin
  if FGridSmoothColors = _value then exit ;

  FGridSmoothColors := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_Red(
  const _value : Integer
) ;
begin
  if FRed = _value then exit ;

  if      _value > 100  then FRed :=  100
  else if _value < -100 then FRed := -100
  else                      FRed := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_Green(
  const _value : Integer
) ;
begin
  if FGreen = _value then exit ;

  if      _value >  100 then FGreen :=  100
  else if _value < -100 then FGreen := -100
  else                       FGreen := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_Blue(
  const _value : Integer
) ;
begin
  if FBlue = _value then exit ;

  if      _value >  100 then FBlue :=  100
  else if _value < -100 then FBlue := -100
  else                       FBlue := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_Brightness(
  const _value : Integer
) ;
begin
  if FBrightness = _value then exit ;

  if      _value >  100 then FBrightness :=  100
  else if _value < -100 then FBrightness := -100
  else                       FBrightness := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_Contrast(
  const _value : Integer
) ;
begin
  if FContrast = _value then exit ;

  if      _value > 100  then FContrast :=  100
  else if _value < -100 then FContrast := -100
  else                       FContrast := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_ContrastEnhanced(
  const _value : Boolean
) ;
begin
  if FContrastEnhanced = _value then exit ;

  FContrastEnhanced := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_MinThresholdHeight(
  const _value : Single
) ;
begin
  if FMinThresholdZ = _value then exit ;

  FMinThresholdZ := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_MaxThresholdHeight(
  const _value : Single
) ;
begin
  if FMaxThresholdZ = _value then exit ;

  FMaxThresholdZ := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_Antialias(
  const _value : Boolean
) ;
begin
  if FAntialias = _value then exit ;

  FAntialias := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_Inversion(
  const _value : Boolean
) ;
begin
  if FInversion = _value then exit ;

  FInversion := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_GrayScale(
  const _value : Boolean
) ;
begin
  if FGrayScale = _value then exit ;

  FGrayScale := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_Histogram(
  const _value : Boolean
) ;
begin
  if FHistogram = _value then exit ;

  FHistogram := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_HistogramPath(
  const _value : String
) ;
begin
  if FHistogramPath = _value then exit ;

  FHistogramPath := _value ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.fset_ColorRamp(
  const _value : TGIS_ColorMapArray
) ;
begin
  FColorRamp := _value ;

  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
 end ;

procedure TGIS_ParamsPixel.doTransparentZonesChange(
  _sender : TObject
) ;
begin
  Touch ;
end ;

procedure TGIS_ParamsPixel.doGrayMapZonesChange(
  _sender : TObject
) ;
begin
  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.doRedMapZonesChange(
  _sender : TObject
) ;
begin
  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.doGreeenMapZonesChange(
  _sender : TObject
) ;
begin
  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.doBlueMapZonesChange(
  _sender : TObject
) ;
begin
  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.doFullRGBMapZonesChange(
  _sender : TObject
) ;
begin
  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.doAltitudMapZonesChange(
  _sender : TObject
) ;
begin
  if assigned( FParent ) then
    TGIS_ParamsSection( FParent ).Touch ;
  Touch ;
end ;

procedure TGIS_ParamsPixel.assignBitmap(
  const _value : TGIS_Bitmap
) ;
begin
  if _value <> FLegendImage then begin
    FreeObject( FLegendImage ) ;
    if not assigned( _value ) then
      exit ;
    FLegendImage := TGIS_Bitmap.Create ;
    if assigned( _value ) then
      FLegendImage.Assign( _value ) ;
  end ;
end ;

procedure TGIS_ParamsPixel.Assign(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsPixel.Create
  else
    src := _source ;
  try

    if src is TGIS_ParamsPixel then begin
      FPage             := TGIS_ParamsPixel(src).Page               ;
      FShowLegend       := TGIS_ParamsPixel(src).ShowLegend         ;
      FRedBand          := TGIS_ParamsPixel(src).RedBand            ;
      FGreenBand        := TGIS_ParamsPixel(src).GreenBand          ;
      FBlueBand         := TGIS_ParamsPixel(src).BlueBand           ;
      FAlphaBand        := TGIS_ParamsPixel(src).AlphaBand          ;
      FGridBand         := TGIS_ParamsPixel(src).GridBand           ;
      FGridNoValue      := TGIS_ParamsPixel(src).GridNoValue        ;
      FGridShadow       := TGIS_ParamsPixel(src).GridShadow         ;
      FGridShadowAngle  := TGIS_ParamsPixel(src).GridShadowAngle    ;
      FGridSmoothColors := TGIS_ParamsPixel(src).GridSmoothColors   ;
      FRed              := TGIS_ParamsPixel(src).Red                ;
      FGreen            := TGIS_ParamsPixel(src).Green              ;
      FBlue             := TGIS_ParamsPixel(src).Blue               ;
      FBrightness       := TGIS_ParamsPixel(src).Brightness         ;
      FContrast         := TGIS_ParamsPixel(src).Contrast           ;
      FContrastEnhanced := TGIS_ParamsPixel(src).ContrastEnhanced   ;
      FMinThresholdZ    := TGIS_ParamsPixel(src).MinHeightThreshold ;
      FMaxThresholdZ    := TGIS_ParamsPixel(src).MaxHeightThreshold ;
      FAntialias        := TGIS_ParamsPixel(src).Antialias          ;
      FInversion        := TGIS_ParamsPixel(src).Inversion          ;
      FGrayScale        := TGIS_ParamsPixel(src).GrayScale          ;
      FHistogram        := TGIS_ParamsPixel(src).Histogram          ;
      FHistogramPath    := TGIS_ParamsPixel(src).HistogramPath      ;
      FColorRamp        := TGIS_ParamsPixel(src).ColorRamp          ;

      assignBitmap            ( TGIS_ParamsPixel(src).LegendImage      ) ;
      FTransparentZones.Assign( TGIS_ParamsPixel(src).TransparentZones ) ;
      FGrayMapZones.Assign    ( TGIS_ParamsPixel(src).GrayMapZones     ) ;
      FRedMapZones.Assign     ( TGIS_ParamsPixel(src).RedMapZones      ) ;
      FGreenMapZones.Assign   ( TGIS_ParamsPixel(src).GreenMapZones    ) ;
      FBlueMapZones.Assign    ( TGIS_ParamsPixel(src).BlueMapZones     ) ;
      FFullRGBMapZones.Assign ( TGIS_ParamsPixel(src).FullRGBMapZones  ) ;
      FAltitudeMapZones.Assign( TGIS_ParamsPixel(src).AltitudeMapZones ) ;
    end ;
    inherited Assign(src) ;

  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsPixel.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  with TGIS_Config( _cfg ) do begin
    oConfig := TGIS_Config( _cfg ) ;

    if Version > 0 then begin
      Page                   := ReadInteger  ( GIS_INI_PIXEL_PAGE,
                                               Page
                                             ) ;
      ShowLegend             := ReadBoolean  ( GIS_INI_PIXEL_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
      RedBand                := ReadInteger  ( GIS_INI_PIXEL_REDBAND,
                                               RedBand
                                             ) ;
      GreenBand              := ReadInteger  ( GIS_INI_PIXEL_GREENBAND,
                                               GreenBand
                                             ) ;
      BlueBand               := ReadInteger  ( GIS_INI_PIXEL_BLUEBAND,
                                               BlueBand
                                             ) ;
      AlphaBand              := ReadInteger  ( GIS_INI_PIXEL_ALPHABAND,
                                               AlphaBand
                                             ) ;
      GridBand               := ReadInteger  ( GIS_INI_PIXEL_GRIDBAND,
                                               GridBand
                                             ) ;
      GridNoValue            := ReadFloat    ( GIS_INI_PIXEL_GRIDNOVALUE,
                                               GridNoValue
                                             ) ;
      GridShadow             := ReadBoolean  ( GIS_INI_PIXEL_GRIDSHADOW,
                                               GridShadow
                                             ) ;
      GridShadowAngle        := ReadFloat    ( GIS_INI_PIXEL_GRIDSHADOWANGLE,
                                               GridShadowAngle
                                             ) ;
      GridSmoothColors       := ReadBoolean  ( GIS_INI_PIXEL_GRIDSMOOTHCOLORS,
                                               GridSmoothColors
                                             ) ;
      Red                    := ReadInteger  ( GIS_INI_PIXEL_RED,
                                               Red
                                             ) ;
      Green                  := ReadInteger  ( GIS_INI_PIXEL_GREEN,
                                               Green
                                             ) ;
      Blue                   := ReadInteger  ( GIS_INI_PIXEL_BLUE,
                                               Blue
                                             ) ;
      Brightness             := ReadInteger  ( GIS_INI_PIXEL_BRIGHTNESS,
                                               Brightness
                                             ) ;
      Contrast               := ReadInteger  ( GIS_INI_PIXEL_CONTRAST,
                                               Contrast
                                             ) ;
      ContrastEnhanced       := ReadBoolean  ( GIS_INI_PIXEL_CONTRASTENHANCED,
                                               ContrastEnhanced
                                             ) ;
      MinHeightThreshold     := ReadFloat    ( GIS_INI_PIXEL_MINHEIGHTTHRESHOLD,
                                               MinHeightThreshold
                                             ) ;
      MaxHeightThreshold     := ReadFloat    ( GIS_INI_PIXEL_MAXHEIGHTTHRESHOLD,
                                               MaxHeightThreshold
                                             ) ;
      Antialias              := ReadBoolean  ( GIS_INI_PIXEL_ANTIALIAS,
                                               Antialias
                                             ) ;
      Inversion              := ReadBoolean  ( GIS_INI_PIXEL_INVERSION,
                                               Inversion
                                             ) ;
      GrayScale              := ReadBoolean  ( GIS_INI_PIXEL_GRAYSCALE,
                                               GrayScale
                                             ) ;
      Histogram              := ReadBoolean  ( GIS_INI_PIXEL_HISTOGRAM,
                                               Histogram
                                             ) ;
      HistogramPath          := ReadString   ( GIS_INI_PIXEL_HISTOGRAMPATH,
                                               HistogramPath
                                             ) ;
      LegendImage            := ReadBitmap   ( GIS_INI_PIXEL_LEGENDIMAGE,
                                               LegendImage
                                             ) ;
      ColorRamp              := ReadColorRamp( GIS_XML_PIXEL_COLORRAMP,
                                               ColorRamp
                                             ) ;
      ReadZone                               ( GIS_INI_PIXEL_TRANSPARENTZONE,
                                               TransparentZones
                                             ) ;
      ReadZone                               ( GIS_INI_PIXEL_GRAYMAPZONE,
                                               GrayMapZones
                                             ) ;
      ReadZone                               ( GIS_INI_PIXEL_REDMAPZONE,
                                               RedMapZones
                                             ) ;
      ReadZone                               ( GIS_INI_PIXEL_GREENMAPZONE,
                                               GreenMapZones
                                             ) ;
      ReadZone                               ( GIS_INI_PIXEL_BLUEMAPZONE,
                                               BlueMapZones
                                             ) ;
      ReadZone                               ( GIS_INI_PIXEL_FULLRGBMAPZONE,
                                               FullRGBMapZones
                                             ) ;
      ReadZone                               ( GIS_INI_PIXEL_ALTITUDEMAPZONE,
                                               AltitudeMapZones
                                             ) ;
    end ;

    oConfig := nil ;
  end ;
end ;

procedure TGIS_ParamsPixel.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  def : TGIS_ParamsPixel ;
begin
  def := TGIS_ParamsPixel.Create ;
  try
    with TGIS_Config( _cfg ) do begin

      if Version > 0 then begin
        WriteBoolean                         ( GIS_INI_PIXEL_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_PAGE,
                                               Page,
                                               def.Page
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_REDBAND,
                                               RedBand,
                                               def.RedBand
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_GREENBAND,
                                               GreenBand,
                                               def.GreenBand
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_BLUEBAND,
                                               BlueBand,
                                               def.BlueBand
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_ALPHABAND,
                                               AlphaBand,
                                               def.AlphaBand
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_GRIDBAND,
                                               GridBand,
                                               def.GridBand
                                             ) ;
        WriteFloat                           ( GIS_INI_PIXEL_GRIDNOVALUE,
                                               GridNoValue,
                                               def.GridNoValue
                                             ) ;
        WriteBoolean                         ( GIS_INI_PIXEL_GRIDSHADOW,
                                               GridShadow,
                                               def.GridShadow
                                             ) ;
        WriteFloat                           ( GIS_INI_PIXEL_GRIDSHADOWANGLE,
                                               GridShadowAngle,
                                               def.GridShadowAngle
                                             ) ;
        WriteBoolean                         ( GIS_INI_PIXEL_GRIDSMOOTHCOLORS,
                                               GridSmoothColors,
                                               def.GridSmoothColors
                                             ) ;

        WriteInteger                         ( GIS_INI_PIXEL_RED,
                                               Red,
                                               def.Red
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_GREEN,
                                               Green,
                                               def.Green
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_BLUE,
                                               Blue,
                                               def.Blue
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_BRIGHTNESS,
                                               Brightness,
                                               def.Brightness
                                             ) ;
        WriteInteger                         ( GIS_INI_PIXEL_CONTRAST,
                                               Contrast,
                                               def.Contrast
                                             ) ;
        WriteBoolean                         ( GIS_INI_PIXEL_CONTRASTENHANCED,
                                               ContrastEnhanced,
                                               def.ContrastEnhanced
                                             ) ;
        WriteFloat                           ( GIS_INI_PIXEL_MINHEIGHTTHRESHOLD,
                                               MinHeightThreshold,
                                               def.MinHeightThreshold
                                             ) ;
        WriteFloat                           ( GIS_INI_PIXEL_MAXHEIGHTTHRESHOLD,
                                               MaxHeightThreshold,
                                               def.MaxHeightThreshold
                                             ) ;
        WriteBoolean                         ( GIS_INI_PIXEL_ANTIALIAS,
                                               Antialias,
                                               def.Antialias
                                             ) ;
        WriteBoolean                         ( GIS_INI_PIXEL_INVERSION,
                                               Inversion,
                                               def.Inversion
                                             ) ;
        WriteBoolean                         ( GIS_INI_PIXEL_GRAYSCALE,
                                               GrayScale,
                                               def.GrayScale
                                             ) ;
        WriteBoolean                         ( GIS_INI_PIXEL_HISTOGRAM,
                                               Histogram,
                                               def.Histogram
                                             ) ;
        WriteString                          ( GIS_INI_PIXEL_HISTOGRAMPATH,
                                               HistogramPath,
                                               def.HistogramPath
                                             ) ;
        WriteColorRamp                       ( GIS_XML_PIXEL_COLORRAMP,
                                               ColorRamp,
                                               def.ColorRamp
                                             ) ;
        WriteZone                            ( GIS_INI_PIXEL_TRANSPARENTZONE,
                                               TransparentZones,
                                               def.TransparentZones
                                             ) ;
        WriteZone                            ( GIS_INI_PIXEL_GRAYMAPZONE,
                                               GrayMapZones,
                                               def.GrayMapZones
                                             ) ;
        WriteZone                            ( GIS_INI_PIXEL_REDMAPZONE,
                                               RedMapZones,
                                               def.RedMapZones
                                             ) ;
        WriteZone                            ( GIS_INI_PIXEL_GREENMAPZONE,
                                               GreenMapZones,
                                               def.GreenMapZones
                                             ) ;
        WriteZone                            ( GIS_INI_PIXEL_BLUEMAPZONE,
                                               BlueMapZones,
                                               def.BlueMapZones
                                             ) ;
        WriteZone                            ( GIS_INI_PIXEL_FULLRGBMAPZONE,
                                               FullRGBMapZones,
                                               def.FullRGBMapZones
                                             ) ;
        WriteZone                            ( GIS_INI_PIXEL_ALTITUDEMAPZONE,
                                               AltitudeMapZones,
                                               def.AltitudeMapZones
                                             ) ;
        WriteBitmap                          ( GIS_INI_PIXEL_LEGENDIMAGE,
                                               LegendImage,
                                               def.LegendImage
                                             ) ;
        WriteBitmap                          ( GIS_INI_PIXEL_LEGENDIMAGE,
                                               LegendImage,
                                               def.LegendImage
                                             ) ;
      end ;
    end ;
  finally
    FreeObject( def ) ;
  end ;
end;

{$ENDREGION}
{$REGION 'TGIS_ParamsFeature'}

constructor TGIS_ParamsFeature.Create ;
begin
  inherited ;

  FSmartSizeAsText    := '' ;
end ;

procedure TGIS_ParamsFeature.doDestroy ;
begin
  FreeObject( FBitmap        ) ;
  FreeObject( FOutlineBitmap ) ;

  FreeObject( FSmartSizeInternal    ) ;
  FreeObject( FColorInternal        ) ;
  FreeObject( FOutlineStyleInternal ) ;
  FreeObject( FOutlineColorInternal ) ;
  FreeObject( FOutlineWidthInternal ) ;
  FreeObject( FOffsetXInternal ) ;
  FreeObject( FOffsetYInternal ) ;

  inherited ;
end ;

procedure TGIS_ParamsFeature.fset_ShowLegend(
  const _value : Boolean
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FShowLegend = _value then exit ;
    FShowLegend := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsFeature.fset_SmartSize(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FSmartSize = _value then exit ;
    FreeObject( FSmartSizeInternal ) ;
    FSmartSize := _value ;
    FSmartSizeAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_Color : TGIS_Color ;
begin
  Result := compute_color( Shape, FColor, FColorInternal ) ;
end ;

procedure TGIS_ParamsFeature.fset_Color(
  const _value : TGIS_Color
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FColor.ARGB = _value.ARGB then exit ;
    FreeObject( FColorInternal ) ;
    FColor := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_OutlineColor : TGIS_Color ;
begin
  Result := compute_color( Shape, FOutlineColor, FOutlineColorInternal ) ;
end ;

procedure TGIS_ParamsFeature.fset_OutlineColor(
  const _value : TGIS_Color
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FOutlineColor.ARGB = _value.ARGB then exit ;
    FreeObject( FOutlineColorInternal ) ;
    FOutlineColor := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsFeature.fset_OutlineBackcolor(
  const _value : TGIS_Color
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FOutlineBackcolor.ARGB = _value.ARGB then exit ;
    FOutlineBackcolor := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_Bitmap
  : TGIS_Bitmap ;
begin
  Result := FBitmap;
end ;

procedure TGIS_ParamsFeature.fset_Bitmap(
  const _value : TGIS_Bitmap
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  if _value <> prm.FBitmap then begin
    FreeObject( prm.FBitmap ) ;
    if not assigned( _value ) then
      exit ;
    prm.FBitmap := TGIS_Bitmap.Create ;
    if assigned( _value ) then
      prm.FBitmap.Assign( _value ) ;
    prm.Touch ;
  end ;
end ;

procedure TGIS_ParamsFeature.fset_Pattern(
  const _value : TGIS_BrushStyle
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FPattern = _value then exit ;
    FPattern := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_OutlineStyle : TGIS_PenStyle ;
begin
  Result := compute_style( Shape, FOutlineStyle, FOutlineStyleInternal ) ;
end ;

procedure TGIS_ParamsFeature.fset_OutlineStyle(
  const _value : TGIS_PenStyle
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FOutlineStyle = _value then exit ;
    FreeObject( FOutlineStyleInternal ) ;
    FOutlineStyle := _value ;
    FOutlineStyleAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_OutlineWidth : Integer ;
begin
  Result := compute_size( Shape, FOutlineWidth, FOutlineWidthInternal ) ;
end ;

procedure TGIS_ParamsFeature.fset_OutlineWidth(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FOutlineWidth = _value then exit ;
    FreeObject( FOutlineWidthInternal ) ;
    FOutlineWidth := _value ;
    FOutlineWidthAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_OffsetX : Integer ;
begin
  Result := compute_size( Shape, FOffsetX, FOffsetXInternal ) ;
end ;

procedure TGIS_ParamsFeature.fset_OffsetX(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FOffsetX = _value then exit ;
    FreeObject( FOffsetXInternal ) ;
    FOffsetX := _value ;
    FOffsetXAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_OffsetY : Integer ;
begin
  Result := compute_size( Shape, FOffsetY, FOffsetYInternal ) ;
end ;

procedure TGIS_ParamsFeature.fset_OffsetY(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FOffsetY = _value then exit ;
    FreeObject( FOffsetYInternal ) ;
    FOffsetY := _value ;
    FOffsetYAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_OffsetPosition : TGIS_OffsetPosition ;
begin
  Result := FOffsetPosition ;
end ;

procedure TGIS_ParamsFeature.fset_OffsetPosition(
  const _value : TGIS_OffsetPosition
);
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FOffsetPosition = _value then exit ;
    FOffsetPosition := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_OutlineBitmap
  : TGIS_Bitmap ;
begin
  Result := FOutlineBitmap;
end ;

procedure TGIS_ParamsFeature.fset_OutlineBitmap(
  const _value : TGIS_Bitmap
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;

  if _value <> prm.FOutlineBitmap then begin
    FreeObject( prm.FOutlineBitmap ) ;
    if not assigned( _value ) then
      exit ;
    prm.FOutlineBitmap := TGIS_Bitmap.Create ;
    if assigned( _value ) then
      prm.FOutlineBitmap.Assign( _value ) ;
    prm.Touch ;
  end ;
end ;

function TGIS_ParamsFeature.fget_PatternAsText
  : String ;
begin
  Result := pattern_to_text( nil, FBitmap, Pattern ) ;
end ;

procedure TGIS_ParamsFeature.fset_OutlinePattern(
  const _value : TGIS_BrushStyle
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  with prm do begin
    if FOutlinePattern = _value then exit ;
    FOutlinePattern := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsFeature.fset_PatternAsText(
  const _value : String
) ;
var
  tmp_symbol  : TGIS_SymbolAbstract ;
  tmp_bitmap  : TGIS_Bitmap         ;
  tmp_pattern : TGIS_BrushStyle     ;
begin
  tmp_symbol  := nil     ;
  tmp_bitmap  := Bitmap  ;
  tmp_pattern := Pattern ;

  try
    pattern_from_text( oConfig,
                       _value, tmp_symbol, tmp_bitmap, tmp_pattern, False
                     ) ;

    Bitmap  := tmp_bitmap  ;
    if assigned( tmp_bitmap ) and ( tmp_bitmap <> Bitmap ) then
      FreeObject( tmp_bitmap ) ;
    Pattern := tmp_pattern ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_STEM_PATTERN,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsFeature.fget_ColorAsText
  : String ;
begin
  Result := color_to_text( FColor, FColorInternal ) ;
end ;

procedure TGIS_ParamsFeature.fset_ColorAsText(
  const _value : String
) ;
var
  tmp_color : TGIS_Color ;
begin
  tmp_color := Color ;

  try
    color_from_text( self, _value, tmp_color, FColorInternal );
    if not assigned( FColorInternal ) then
      Color := tmp_color ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_STEM_COLOR,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsFeature.fget_OutlineStyleAsText
  : String ;
begin
  Result := pen_to_text( nil, FOutlineStyleAsText,
                         OutlineStyle, FOutlineStyleInternal
                       ) ;
end ;

procedure TGIS_ParamsFeature.fset_OutlineStyleAsText(
  const _value : String
) ;
var
  prm         : TGIS_ParamsFeature ;
  tmp_symbol  : TGIS_SymbolAbstract ;
  tmp_style   : TGIS_PenStyle       ;
begin
  tmp_symbol  := nil            ;
  tmp_style   := OutlineStyle   ;
  try
    pen_from_text( oConfig,
                   self, _value, tmp_symbol, tmp_style,
                   FOutlineStyleInternal, False
                 ) ;

    if not assigned( FOutlineStyleInternal ) then
      OutlineStyle   := tmp_style   ;

    prm := TGIS_ParamsFeature( prepareParams ) ;
    with prm do begin
      if FOutlineStyleAsText <> _value then begin
        FOutlineStyleAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_STEM_OUTLINESTYLE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsFeature.fget_OutlinePatternAsText
  : String ;
begin
  Result := pattern_to_text( nil, FOutlineBitmap, OutlinePattern ) ;
end ;

procedure TGIS_ParamsFeature.fset_OutlinePatternAsText(
  const _value : String
) ;
var
  tmp_symbol  : TGIS_SymbolAbstract ;
  tmp_bitmap  : TGIS_Bitmap         ;
  tmp_pattern : TGIS_BrushStyle     ;
begin
  tmp_symbol  := nil            ;
  tmp_bitmap  := OutlineBitmap  ;
  tmp_pattern := OutlinePattern ;

  try
    pattern_from_text( oConfig,
                       _value, tmp_symbol, tmp_bitmap, tmp_pattern, False
                     ) ;

    OutlineBitmap  := tmp_bitmap  ;
    if assigned( tmp_bitmap ) and ( tmp_bitmap <> Bitmap ) then
      FreeObject( tmp_bitmap ) ;
    OutlinePattern := tmp_pattern ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_STEM_OUTLINEPATTERN,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsFeature.fget_OutlineColorAsText
  : String ;
begin
  Result := color_to_text( OutlineColor, FOutlineColorInternal ) ;
end ;

procedure TGIS_ParamsFeature.fset_OutlineColorAsText(
  const _value : String
) ;
var
  tmp_color : TGIS_Color ;
begin
  tmp_color := OutlineColor ;

  try
    color_from_text( self, _value, tmp_color, FOutlineColorInternal );

    if not assigned( FOutlineColorInternal ) then
      OutlineColor := tmp_color ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_STEM_OUTLINECOLOR,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsFeature.fget_OutlineWidthAsText
  : String ;
begin
  Result := size_to_text( FOutlineWidthAsText,
                          OutlineWidth,
                          FOutlineWidthInternal
                        ) ;
end ;

procedure TGIS_ParamsFeature.fset_OutlineWidthAsText(
  const _value : String
) ;
var
  prm       : TGIS_ParamsFeature ;
  itmp_size : Integer ;
begin
  itmp_size := OutlineWidth ;

  try
    size_from_text( self, _value, itmp_size, FOutlineWidthInternal );

    if not assigned( FOutlineWidthInternal ) then
      OutlineWidth := itmp_size ;

    prm := TGIS_ParamsFeature( prepareParams ) ;
    with prm do begin
      if FOutlineWidthAsText <> _value then begin
        FOutlineWidthAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_STEM_OUTLINEWIDTH,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsFeature.fget_SmartSizeAsText
  : String ;
begin
  Result := size_to_text( FSmartSizeAsText, SmartSize, FSmartSizeInternal ) ;
end ;

procedure TGIS_ParamsFeature.fset_SmartSizeAsText(
  const _value : String
);
var
  prm       : TGIS_ParamsFeature ;
  itmp_size : Integer ;
begin
  itmp_size := SmartSize ;

  try
    size_from_text( self, _value, itmp_size, FSmartSizeInternal );

    if not assigned( FSmartSizeInternal ) then
      SmartSize := itmp_size ;

    prm := TGIS_ParamsFeature( prepareParams ) ;
    with prm do begin
      if FSmartSizeAsText <> _value then begin
        FSmartSizeAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_STEM_SMARTSIZE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsFeature.fget_OffsetXAsText
  : String ;
begin
  Result := size_to_text( FOffsetXAsText,
                          OffsetX,
                          FOffsetXInternal
                        ) ;
end ;

procedure TGIS_ParamsFeature.fset_OffsetXAsText(
  const _value : String
) ;
var
  prm       : TGIS_ParamsFeature ;
  itmp_size : Integer ;
begin
  itmp_size := OffsetX ;

  try
    size_from_text( self, _value, itmp_size, FOffsetXInternal );

    if not assigned( FOffsetXInternal ) then
      OffsetX := itmp_size ;

    prm := TGIS_ParamsFeature( prepareParams ) ;
    with prm do begin
      if FOffsetXAsText <> _value then begin
        FOffsetXAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + 'OffsetX',
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsFeature.fget_OffsetYAsText
  : String ;
begin
  Result := size_to_text( FOffsetYAsText,
                          OffsetY,
                          FOffsetYInternal
                        ) ;
end ;

procedure TGIS_ParamsFeature.fset_OffsetYAsText(
  const _value : String
) ;
var
  prm       : TGIS_ParamsFeature ;
  itmp_size : Integer ;
begin
  itmp_size := OffsetY ;

  try
    size_from_text( self, _value, itmp_size, FOffsetYInternal );

    if not assigned( FOffsetYInternal ) then
      OffsetY := itmp_size ;

    prm := TGIS_ParamsFeature( prepareParams ) ;
    with prm do begin
      if FOffsetYAsText <> _value then begin
        FOffsetYAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + 'OffsetY',
      '"' + _value + '"'
    );
  end;
end ;

procedure TGIS_ParamsFeature.assignInternal(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsFeature.Create
  else
    src := _source ;
  try
    if src is TGIS_ParamsAbstract then begin
      FShowLegend          := TGIS_ParamsFeature(src).ShowLegend          ;
      FSmartSize           := TGIS_ParamsFeature(src).SmartSize           ;
      FSmartSizeAsText     := TGIS_ParamsFeature(src).SmartSizeAsText     ;

      TGIS_ParamsField.assignEx(
                              self,
                              FSmartSizeInternal,
                              TGIS_ParamsFeature(src).FSmartSizeInternal
                            ) ;
      FColor               := TGIS_ParamsFeature(src).Color               ;
      TGIS_ParamsField.assignEx(
                               self,
                               FColorInternal,
                               TGIS_ParamsFeature(src).FColorInternal
                             ) ;
      assignBitmap(           TGIS_ParamsFeature(src).Bitmap )            ;
      FPattern             := TGIS_ParamsFeature(src).Pattern             ;
      FOutlineStyle        := TGIS_ParamsFeature(src).OutlineStyle        ;
      FOutlineStyleAsText  := TGIS_ParamsFeature(src).OutlineStyleAsText  ;
      TGIS_ParamsField.assignEx(
                               self,
                               FOutlineStyleInternal,
                               TGIS_ParamsFeature(src).FOutlineStyleInternal
                             );
      FOutlineWidth        := TGIS_ParamsFeature(src).OutlineWidth        ;
      FOutlineWidthAsText  := TGIS_ParamsFeature(src).OutlineWidthAsText  ;
      TGIS_ParamsField.assignEx(
                               self,
                               FOutlineWidthInternal,
                               TGIS_ParamsFeature(src).FOutlineWidthInternal
                              ) ;
      FOutlineColor        := TGIS_ParamsFeature(src).OutlineColor        ;
      TGIS_ParamsField.assignEx(
                               self,
                               FOutlineColorInternal,
                               TGIS_ParamsFeature(src).FOutlineColorInternal
                              ) ;
      FOutlineBackcolor    := TGIS_ParamsFeature(src).OutlineBackcolor    ;
      assignOutlineBitmap(    TGIS_ParamsFeature(src).OutlineBitmap )     ;
      FOutlinePattern      := TGIS_ParamsFeature(src).OutlinePattern      ;
      FOffsetX             := TGIS_ParamsFeature(src).OffsetX        ;
      FOffsetXAsText       := TGIS_ParamsFeature(src).OffsetXAsText  ;
      TGIS_ParamsField.assignEx(
                               self,
                               FOffsetXInternal,
                               TGIS_ParamsFeature(src).FOffsetXInternal
                              ) ;
      FOffsetY             := TGIS_ParamsFeature(src).OffsetY        ;
      FOffsetYAsText       := TGIS_ParamsFeature(src).OffsetYAsText  ;
      TGIS_ParamsField.assignEx(
                               self,
                               FOffsetYInternal,
                               TGIS_ParamsFeature(src).FOffsetYInternal
                              ) ;
      FOffsetPosition      := TGIS_ParamsFeature(src).OffsetPosition ;
    end ;
    inherited assignInternal(src) ;

  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsFeature.assignBitmap(
  const _value : TGIS_Bitmap
) ;
begin
  if _value <> FBitmap then begin
    FreeObject( FBitmap ) ;
    if not assigned( _value ) then
      exit ;
    FBitmap := TGIS_Bitmap.Create ;
    if assigned( _value ) then
      FBitmap.Assign( _value ) ;
  end ;
end ;

procedure TGIS_ParamsFeature.assignOutlineBitmap(
  const _value : TGIS_Bitmap
) ;
begin
  if _value <> FOutlineBitmap then begin
    FreeObject( FOutlineBitmap ) ;
    if not assigned( _value ) then
      exit ;
    FOutlineBitmap := TGIS_Bitmap.Create ;
    if assigned( _value ) then
      FOutlineBitmap.Assign( _value ) ;
  end ;
end ;

procedure TGIS_ParamsFeature.Assign(
  _source: TPersistent
) ;
var
  prm : TGIS_ParamsFeature ;
begin
  prm := TGIS_ParamsFeature( prepareParams ) ;
  prm.assignInternal(_source) ;
end ;

class function TGIS_ParamsFeature.SizeHelper(
  const   _size   : Integer  ;
  const   _factor : Double
) : Integer ;
begin
  if _size = GIS_RENDER_SIZE then
    Result := GIS_RENDER_SIZE
  else
  if Abs( _size ) > GIS_AUTOSIZE_SIZE then begin
    Result:= RoundS( Abs( _size ) mod GIS_AUTOSIZE_SIZE * _factor ) ;
    if _size < 0 then
      Result := -Result ;
  end
  else
    Result := RoundS( _size * _factor ) ;
end;

{$ENDREGION}
{$REGION 'TGIS_ParamsLine'}

constructor TGIS_ParamsLine.Create ;
begin
  inherited ;

  FSmartSize            := -1 ;

  FStyle                := TGIS_PenStyle.Solid ;
  FStyleAsText          := '' ;
  FColor                := TGIS_Color.Black ;
  FWidth                := 1 ;
  FWidthAsText          := '' ;
  FPattern              := TGIS_BrushStyle.Solid ;
  FSymbol               := nil ;
  FSymbolGap            := 0 ;
  FSymbolRotate         := 0 ;
  FSymbolRotateAsText   := '' ;
  FOutlineStyle         := TGIS_PenStyle.Solid ;
  FOutlineStyleAsText   := '' ;
  FOutlineWidth         := 0 ;
  FOutlineWidthAsText   := '' ;
  FOutlineColor         := TGIS_Color.Gray ;
  FOutlineBackcolor     := TGIS_Color.Crazy ;
  FOutlinePattern       := TGIS_BrushStyle.Solid ;
  FOffsetX              := 0 ;
  FOffsetXAsText        := '' ;
  FOffsetY              := 0 ;
  FOffsetYAsText        := '' ;
  FOffsetPosition       := TGIS_OffsetPosition.DownRight ;
end ;

procedure TGIS_ParamsLine.doDestroy ;
begin
  FreeObject( FStyleInternal ) ;
  FreeObject( FColorInternal ) ;
  FreeObject( FWidthInternal ) ;
  FreeObject( FSymbolRotateInternal ) ;
  FreeObject( FOutlineStyleInternal ) ;
  FreeObject( FOutlineColorInternal ) ;

  inherited ;
end ;

function TGIS_ParamsLine.fget_Style : TGIS_PenStyle ;
begin
  Result := compute_style( Shape, FStyle, FStyleInternal ) ;
end ;

procedure TGIS_ParamsLine.fset_Style(
  const _value : TGIS_PenStyle
) ;
var
  prm : TGIS_ParamsLine ;
begin
  prm := TGIS_ParamsLine( prepareParams ) ;
  with prm do begin
    if FStyle = _value then exit ;
    FreeObject( FStyleInternal ) ;
    FStyle := _value ;
    FStyleAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsLine.fset_Symbol(
  const _value : TGIS_SymbolAbstract
) ;
var
  prm : TGIS_ParamsLine ;
begin
  prm := TGIS_ParamsLine( prepareParams ) ;
  with prm do begin
    if FSymbol = _value then exit ;
    FSymbol := _value ;
    FStyleAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLine.fget_Width : Integer ;
begin
  Result := compute_size( Shape, FWidth, FWidthInternal ) ;
end ;

procedure TGIS_ParamsLine.fset_Width(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsLine ;
begin
  prm := TGIS_ParamsLine( prepareParams ) ;
  with prm do begin
    if FWidth = _value then exit ;
    FreeObject( FWidthInternal );
    FWidth := _value ;
    FWidthAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsLine.fset_SymbolGap(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsLine ;
begin
  prm := TGIS_ParamsLine( prepareParams ) ;
  with prm do begin
    if FSymbolGap = _value then exit ;
    FSymbolGap := _value ;
    FSymbolGapAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLine.fget_SymbolRotate : Double ;
begin
  Result := compute_rotate( Shape, FSymbolRotate, FSymbolRotateInternal ) ;
end ;

procedure TGIS_ParamsLine.fset_SymbolRotate(
  const _value : Double
) ;
var
  prm : TGIS_ParamsLine ;
begin
  prm := TGIS_ParamsLine( prepareParams ) ;
  with prm do begin
    if FSymbolRotate = _value then exit ;
    FreeObject( FSymbolRotateInternal ) ;
    FSymbolRotate := _value ;
    FSymbolRotateAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLine.fget_StyleAsText
  : String ;
begin
  Result := pen_to_text( Symbol, FStyleAsText, Style, FStyleInternal ) ;
end ;

procedure TGIS_ParamsLine.fset_StyleAsText(
  const _value : String
) ;
var
  prm         : TGIS_ParamsLine     ;
  tmp_symbol  : TGIS_SymbolAbstract ;
  tmp_style   : TGIS_PenStyle       ;
begin
  tmp_symbol  := Symbol ;
  tmp_style   := Style  ;

  try
    pen_from_text( oConfig,
                   self, _value, tmp_symbol, tmp_style, FStyleInternal, True
                 ) ;

    Symbol  := tmp_symbol ;
    if not assigned( FStyleInternal ) then
      Style   := tmp_style  ;

    prm := TGIS_ParamsLine( prepareParams ) ;
    with prm do begin
      if FStyleAsText <> _value then begin
        FStyleAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LINE_SMARTSIZE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsLine.fget_WidthAsText
 : String ;
begin
  Result := size_to_text( FWidthAsText, Width, FWidthInternal ) ;
end ;

procedure TGIS_ParamsLine.fset_WidthAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsLine ;
  itmp_size  : Integer ;
begin
  itmp_size := Width ;

  try
    size_from_text( self, _value, itmp_size, FWidthInternal );

    if not assigned( FWidthInternal ) then
      Width := itmp_size ;

    prm := TGIS_ParamsLine( prepareParams ) ;
    with prm do begin
      if FWidthAsText <> _value then begin
        FWidthAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LINE_WIDTH,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsLine.fget_SymbolGapAsText
  : String ;
begin
  Result := size_to_text( FSymbolGapAsText, SymbolGap, nil ) ;
end ;

procedure TGIS_ParamsLine.fset_SymbolGapAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsLine ;
  itmp_size  : Integer ;
begin
  itmp_size := SymbolGap ;

  try
    size_from_text( self, _value, itmp_size );

    SymbolGap := itmp_size ;

    prm := TGIS_ParamsLine( prepareParams ) ;
    with prm do begin
      if FSymbolGapAsText <> _value then begin
        FSymbolGapAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LINE_SYMBOLGAP,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsLine.fget_SymbolRotateAsText
  : String ;
begin
  Result := angle_to_text( SymbolRotate,
                           FSymbolRotateAsText,
                           FSymbolRotateInternal
                         ) ;
end ;

procedure TGIS_ParamsLine.fset_SymbolRotateAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsLine ;
  itmp_angle : Double ;
begin
  itmp_angle := SymbolRotate ;

  try
    angle_from_text( self, _value, itmp_angle, FSymbolRotateInternal );

    if not assigned( FSymbolRotateInternal ) then
      SymbolRotate := itmp_angle ;

    prm := TGIS_ParamsLine( prepareParams ) ;
    with prm do begin
      if FSymbolRotateAsText <> _value then begin
        FSymbolRotateAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LINE_SYMBOLROTATE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsLine.prepareParams
  : TGIS_ParamsVector ;
var
  params : TGIS_ParamsSectionVector ;
begin
  if assigned( FParentProxy ) and
     assigned( TGIS_ParamsSectionVector(FParentProxy).FShape ) then
  begin
    params := prepareParamsVector ;
    if not assigned( params.FLine ) then begin
      params.FLine := TGIS_ParamsLine( CreateCopy ) ;
      params.FLine.Parent := params ;
    end ;
    Result := params.FLine ;
  end
  else
    Result := self ;
end ;

procedure TGIS_ParamsLine.assignInternal(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsLine.Create
  else
    src := _source ;
  try

    if src is TGIS_ParamsLine then begin
      FStyle               := TGIS_ParamsLine(src).Style              ;
      FStyleAsText         := TGIS_ParamsLine(src).StyleAsText        ;

      TGIS_ParamsField.assignEx(
                               self,
                               FStyleInternal,
                               TGIS_ParamsLine(src).FStyleInternal
                             ) ;
      FWidth               := TGIS_ParamsLine(src).Width              ;
      FWidthAsText         := TGIS_ParamsLine(src).WidthAsText        ;
      TGIS_ParamsField.assignEx(
                               self,
                               FWidthInternal,
                               TGIS_ParamsLine(src).FWidthInternal
                             ) ;
      FSymbol              := TGIS_ParamsLine(src).Symbol             ;
      FSymbolGap           := TGIS_ParamsLine(src).SymbolGap          ;
      FSymbolGapAsText     := TGIS_ParamsLine(src).SymbolGapAsText    ;
      FSymbolRotate        := TGIS_ParamsLine(src).SymbolRotate       ;
      FSymbolRotateAsText  := TGIS_ParamsLine(src).SymbolRotateAsText ;
      TGIS_ParamsField.assignEx(
                               self,
                               FSymbolRotateInternal,
                               TGIS_ParamsLine(src).FSymbolRotateInternal
                             )   ;
    end ;
    inherited assignInternal(src) ;

  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsLine.Assign(
  _source: TPersistent
) ;
var
  prm : TGIS_ParamsVector ;
begin
  prm := prepareParams ;
  prm.assignInternal( _source ) ;
end ;

procedure TGIS_ParamsLine.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  stmp  : String ;
  snull : String ;
begin
  with TGIS_Config( _cfg ) do begin
    oConfig := TGIS_Config( _cfg ) ;

    if config_old_format( _cfg ) then begin
      Style                  := ReadPen      ( GIS_INI_LINE_STYLE,
                                               Style
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FStyleInternal,
                                               _cfg,
                                               GIS_INI_LINE_STYLEEX,
                                               FStyleInternal,
                                               FStyleAsText
                                             ) ;
      Width                  := ReadInteger  ( GIS_INI_LINE_WIDTH,
                                               Width
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FWidthInternal,
                                               _cfg,
                                               GIS_INI_LINE_WIDTHEX,
                                               FWidthInternal,
                                               FWidthAsText
                                             ) ;
      Color                  := ReadColor    ( GIS_INI_LINE_COLOR,
                                               Color
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FColorInternal,
                                               _cfg,
                                               GIS_INI_LINE_COLOREX,
                                               FColorInternal,
                                               snull
                                             ) ;
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_Bitmap,
                                               GIS_INI_LINE_BITMAP,
                                               Bitmap
                                             ) ;
      Pattern                := ReadPattern  ( GIS_INI_LINE_PATTERN,
                                               Pattern
                                             ) ;
      Symbol                 := ReadSymbol   ( GIS_INI_LINE_SYMBOL,
                                               Symbol
                                             ) ;
      SymbolGap              := ReadInteger  ( GIS_INI_LINE_SYMBOLGAP,
                                               SymbolGap
                                             ) ;
      SymbolRotate           := ReadFloat    ( GIS_INI_LINE_SYMBOLROTATE,
                                               SymbolRotate
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FSymbolRotateInternal,
                                               _cfg,
                                               GIS_INI_LINE_SYMBOLROTATEEX,
                                               FSymbolRotateInternal,
                                               FSymbolRotateAsText
                                             ) ;
      OutlineStyle           := ReadPen      ( GIS_INI_LINE_OUTLINESTYLE,
                                               OutlineStyle
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineStyleInternal,
                                               _cfg,
                                               GIS_INI_LINE_OUTLINESTYLEEX,
                                               FOutlineStyleInternal,
                                               FOutlineStyleAsText
                                             ) ;
      OutlineWidth           := ReadInteger  ( GIS_INI_LINE_OUTLINEWIDTH,
                                               OutlineWidth
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineWidthInternal,
                                               _cfg,
                                               GIS_INI_LINE_OUTLINEWIDTHEX,
                                               FOutlineWidthInternal,
                                               FOutlineWidthAsText
                                             ) ;
      OutlineColor           := ReadColor    ( GIS_INI_LINE_OUTLINECOLOR,
                                               OutlineColor
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineColorInternal,
                                               _cfg,
                                               GIS_INI_LINE_OUTLINECOLOREX,
                                               FOutlineColorInternal,
                                               snull
                                             ) ;
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_OutlineBitmap,
                                               GIS_INI_LINE_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      OutlineBitmap          := ReadBitmap   ( GIS_INI_LINE_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      OutlinePattern         := ReadPattern  ( GIS_INI_LINE_OUTLINEPATTERN,
                                               OutlinePattern
                                             ) ;
      SmartSize              := ReadInteger  ( GIS_INI_LINE_SMARTSIZE,
                                               SmartSize
                                             ) ;
      if assigned( FSmartSizeInternal ) then
        stmp                 := ReadString   ( GIS_INI_LINE_SMARTFIELD,
                                               FSmartSizeInternal.FFieldName
                                             )
      else
        stmp                 := ReadString   ( GIS_INI_LINE_SMARTFIELD,
                                               ''
                                             ) ;

      FreeObject( FSmartSizeInternal ) ;
      if not IsStringEmpty( stmp ) then begin
        FSmartSizeInternal := TGIS_ParamsField.Create( self ) ;
        FSmartSizeInternal.Field    := stmp  ;
        FSmartSizeInternal.Scalable := false ;
        FSmartSizeInternal.Factor   := 1     ;
      end;

      ShowLegend             := ReadBoolean  ( GIS_INI_LINE_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
      OffsetX                := ReadInteger  ( GIS_INI_LINE_OFFSETX,
                                               OffsetX
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOffsetXInternal,
                                               _cfg,
                                               GIS_INI_LINE_OFFSETXEX,
                                               FOffsetXInternal,
                                               FOffsetXAsText
                                             ) ;
      OffsetY                := ReadInteger  ( GIS_INI_LINE_OFFSETY,
                                               OffsetY
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOffsetYInternal,
                                               _cfg,
                                               GIS_INI_LINE_OFFSETYEX,
                                               FOffsetYInternal,
                                               FOffsetYAsText
                                             ) ;
      OffsetPosition := ReadOffsetPosition  ( GIS_INI_LINE_OFFSETPOSITION,
                                              OffsetPosition
                                            ) ;
    end;

    if config_new_format( _cfg ) then begin
      StyleAsText            := ReadString   ( GIS_INI_LINE_STYLE,
                                               StyleAsText
                                             ) ;
      WidthAsText            := ReadString   ( GIS_INI_LINE_WIDTH,
                                               WidthAsText
                                             ) ;
      ColorAsText            := ReadString   ( GIS_INI_LINE_COLOR,
                                               ColorAsText
                                             ) ;
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_Bitmap,
                                               GIS_INI_LINE_BITMAP,
                                               Bitmap
                                             ) ;
      PatternAsText          := ReadString   ( GIS_INI_LINE_PATTERN,
                                               PatternAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      Symbol                 := ReadSymbol   ( GIS_INI_LINE_SYMBOL,
                                               Symbol
                                             ) ;
      SymbolGapAsText        := ReadString   ( GIS_INI_LINE_SYMBOLGAP,
                                               SymbolGapAsText
                                             ) ;
      SymbolRotateAsText     := ReadString   ( GIS_INI_LINE_SYMBOLROTATE,
                                               SymbolRotateAsText
                                             ) ;

      OutlineStyleAsText     := ReadString   ( GIS_INI_LINE_OUTLINESTYLE,
                                               OutlineStyleAsText
                                             ) ;
      OutlineWidthAsText     := ReadString   ( GIS_INI_LINE_OUTLINEWIDTH,
                                               OutlineWidthAsText
                                             ) ;
      OutlineColorAsText     := ReadString   ( GIS_INI_LINE_OUTLINECOLOR,
                                               OutlineColorAsText
                                             ) ;
      OffsetXAsText          := ReadString   ( GIS_INI_LINE_OFFSETX,
                                               OffsetXAsText
                                             ) ;
      OffsetYAsText          := ReadString   ( GIS_INI_LINE_OFFSETY,
                                               OffsetYAsText
                                             ) ;
      OffsetPosition         := ReadOffsetPosition( GIS_INI_LINE_OFFSETPOSITION,
                                               OffsetPosition
                                             ) ;

      { TODO -cObsolate : not used after 11.56 }
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_OutlineBitmap,
                                               GIS_INI_LINE_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      OutlineBitmap          := ReadBitmap   ( GIS_INI_LINE_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      OutlinePatternAsText   := ReadString   ( GIS_INI_LINE_OUTLINEPATTERN,
                                               OutlinePatternAsText
                                             ) ;
      SmartSizeAsText        := ReadString   ( GIS_INI_LINE_SMARTSIZE,
                                               SmartSizeAsText
                                             ) ;
      ShowLegend             := ReadBoolean  ( GIS_INI_LINE_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
    end ;

    oConfig := nil ;
  end ;
end ;

procedure TGIS_ParamsLine.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  def : TGIS_ParamsLine ;
begin
  if TGIS_ParamsSectionVector(GetRoot).Line = self then
    def := TGIS_ParamsLine.Create
  else
    def := TGIS_ParamsSectionVector(GetRoot).Line ;
  try
    with TGIS_Config( _cfg ) do begin

      if config_old_format( _cfg ) then begin
        WritePen                             ( GIS_INI_LINE_STYLE,
                                               Style,
                                               def.Style
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LINE_STYLEEX,
                                               FStyleInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_LINE_WIDTH,
                                               make_size_DK10( Width ),
                                               def.Width
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LINE_WIDTHEX,
                                               FWidthInternal
                                             ) ;
        WriteColor                           ( GIS_INI_LINE_COLOR,
                                               Color,
                                               def.Color
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LINE_COLOREX,
                                               FColorInternal
                                             ) ;
        WriteBitmap                          ( GIS_INI_LINE_BITMAP,
                                               Bitmap,
                                               def.Bitmap
                                             ) ;
        WritePattern                         ( GIS_INI_LINE_PATTERN,
                                               Pattern,
                                               def.Pattern
                                             ) ;
        WriteSymbol                          ( GIS_INI_LINE_SYMBOL,
                                               Symbol,
                                               def.Symbol
                                             ) ;
        WriteInteger                         ( GIS_INI_LINE_SYMBOLGAP,
                                               make_size_DK10( SymbolGap ),
                                               def.SymbolGap
                                             ) ;
        WriteFloat                           ( GIS_INI_LINE_SYMBOLROTATE,
                                               SymbolRotate,
                                               def.SymbolRotate
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LINE_SYMBOLROTATEEX,
                                               FSymbolRotateInternal
                                             ) ;
        WritePen                             ( GIS_INI_LINE_OUTLINESTYLE,
                                               OutlineStyle,
                                               def.OutlineStyle
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LINE_OUTLINESTYLEEX,
                                               FOutlineStyleInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_LINE_OUTLINEWIDTH,
                                               make_size_DK10( OutlineWidth ),
                                               def.OutlineWidth
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LINE_OUTLINEWIDTHEX,
                                               FOutlineWidthInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_LINE_OFFSETX,
                                               make_size_DK10( OffsetX ),
                                               def.OffsetX
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LINE_OFFSETXEX,
                                               FOffsetXInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_LINE_OFFSETY,
                                               make_size_DK10( OffsetY ),
                                               def.OffsetY
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LINE_OFFSETYEX,
                                               FOffsetYInternal
                                             ) ;
        WriteOffsetPosition                  ( GIS_INI_LINE_OFFSETPOSITION,
                                               OffsetPosition,
                                               def.OffsetPosition
                                             ) ;
        WriteColor                           ( GIS_INI_LINE_OUTLINECOLOR,
                                               OutlineColor,
                                               def.OutlineColor
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LINE_OUTLINECOLOREX,
                                               FOutlineColorInternal
                                             ) ;
        WriteBitmap                          ( GIS_INI_LINE_OUTLINEBITMAP,
                                               OutlineBitmap,
                                               def.OutlineBitmap
                                             ) ;
        WritePattern                         ( GIS_INI_LINE_OUTLINEPATTERN,
                                               OutlinePattern,
                                               def.OutlinePattern
                                             ) ;
        WriteInteger                         ( GIS_INI_LINE_SMARTSIZE,
                                               make_size_DK10( SmartSize ),
                                               def.SmartSize
                                             ) ;
        if assigned( FSmartSizeInternal          ) and
           ( FSmartSizeInternal.Scalable = False ) and
           ( FSmartSizeInternal.Factor   = 1     )
        then
          WriteString                        ( GIS_INI_LINE_SMARTFIELD,
                                               FSmartSizeInternal.Field,
                                               ''
                                             ) ;

        WriteBoolean                         ( GIS_INI_LINE_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;

     if config_new_format( _cfg ) then begin
        WriteStyle                           ( GIS_INI_LINE_STYLE,
                                               StyleAsText,
                                               def.StyleAsText
                                             ) ;
        WriteString                          ( GIS_INI_LINE_WIDTH,
                                               WidthAsText,
                                               def.WidthAsText
                                             ) ;
        WriteString                          ( GIS_INI_LINE_COLOR,
                                               ColorAsText,
                                               def.ColorAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_LINE_PATTERN,
                                               PatternAsText,
                                               def.PatternAsText
                                             ) ;
        WriteString                          ( GIS_INI_LINE_SYMBOLGAP,
                                               SymbolGapAsText,
                                               def.SymbolGapAsText
                                             ) ;
        WriteString                          ( GIS_INI_LINE_SYMBOLROTATE,
                                               SymbolRotateAsText,
                                               def.SymbolRotateAsText
                                             ) ;
        WriteString                          ( GIS_INI_LINE_OUTLINESTYLE,
                                               OutlineStyleAsText,
                                               def.OutlineStyleAsText
                                             ) ;
        WriteString                          ( GIS_INI_LINE_OUTLINEWIDTH,
                                               OutlineWidthAsText,
                                               def.OutlineWidthAsText
                                             ) ;
        WriteString                          ( GIS_INI_LINE_OFFSETX,
                                               OffsetXAsText,
                                               def.OffsetXAsText
                                             ) ;
        WriteString                          ( GIS_INI_LINE_OFFSETY,
                                               OffsetYAsText,
                                               def.OffsetYAsText
                                             ) ;
        WriteOffsetPosition                  ( GIS_INI_LINE_OFFSETPOSITION,
                                               OffsetPosition,
                                               def.OffsetPosition
                                             ) ;
        WriteString                          ( GIS_INI_LINE_OUTLINECOLOR,
                                               OutlineColorAsText,
                                               def.OutlineColorAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_LINE_OUTLINEPATTERN,
                                               OutlinePatternAsText,
                                               def.OutlinePatternAsText
                                             ) ;
        WriteString                          ( GIS_INI_LINE_SMARTSIZE,
                                               SmartSizeAsText,
                                               def.SmartSizeAsText
                                             ) ;
        WriteBoolean                         ( GIS_INI_LINE_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;

    end ;
  finally
    if TGIS_ParamsSectionVector(GetRoot).Line = Self then
      FreeObject( def ) ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsArea'}

constructor TGIS_ParamsArea.Create ;
begin
  inherited ;

  FSmartSize            := -1 ;

  FColor                := TGIS_Color.Silver ;
  FPattern              := TGIS_BrushStyle.Solid ;
  FSymbol               := nil ;
  FSymbolSize           := 120 ;
  FSymbolGap            := 120 ;
  FSymbolRotate         := 0 ;
  FSymbolRotateAsText   := '' ;
  FOutlineStyle         := TGIS_PenStyle.Solid ;
  FOutlineStyleAsText   := '' ;
  FOutlineWidth         := 1 ;
  FOutlineWidthAsText   := '' ;
  FOutlineColor         := TGIS_Color.Gray ;
  FOutlineBackcolor     := TGIS_Color.Crazy ;
  FOutlinePattern       := TGIS_BrushStyle.Solid ;
  FOutlineSymbol        := nil ;
  FOutlineSymbolGap     := 0 ;
  FOutlineSymbolRotate  := 0 ;
  FOffsetX              := 0 ;
  FOffsetXAsText        := '' ;
  FOffsetY              := 0 ;
  FOffsetYAsText        := '' ;
  FOffsetPosition       := TGIS_OffsetPosition.DownRight ;
end ;

procedure TGIS_ParamsArea.doDestroy ;
begin
  FreeObject( FSymbolRotateInternal  ) ;
  inherited;
end ;

procedure TGIS_ParamsArea.fset_Symbol(
  const _value : TGIS_SymbolAbstract
) ;
var
  prm : TGIS_ParamsArea ;
begin
  prm := TGIS_ParamsArea( prepareParams ) ;
  with prm do begin
    if FSymbol = _value then exit ;
    FSymbol := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsArea.fset_SymbolSize(
  const _value : Integer
) ;
var
  params : TGIS_ParamsArea ;
begin
  params := TGIS_ParamsArea( prepareParams ) ;
  with params do begin
    if FSymbolSize = _value then exit ;
    FSymbolSize := _value ;
    FSymbolSizeAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsArea.fset_SymbolRotate(
  const _value : Double
) ;
var
  prm : TGIS_ParamsArea ;
begin
  prm := TGIS_ParamsArea( prepareParams ) ;
  with prm do begin
    if FSymbolRotate = _value then exit ;
    FreeObject( FSymbolRotateInternal ) ;
    FSymbolRotate := _value ;
    FSymbolRotateAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsArea.fget_SymbolRotate
  : Double ;
begin
  Result := compute_rotate( Shape, FSymbolRotate, FSymbolRotateInternal ) ;
end ;

procedure TGIS_ParamsArea.fset_SymbolGap(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsArea ;
begin
  prm := TGIS_ParamsArea( prepareParams ) ;
  with prm do begin
    if FSymbolGap = _value then exit ;
    FSymbolGap := _value ;
    FSymbolGapAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsArea.fset_OutlineSymbol(
  const _value : TGIS_SymbolAbstract
) ;
var
  prm : TGIS_ParamsArea ;
begin
  prm := TGIS_ParamsArea( prepareParams ) ;
  with prm do begin
    if FOutlineSymbol = _value then exit ;
    FOutlineSymbol := _value ;
    FOutlineStyleAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsArea.fset_OutlineSymbolGap(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsArea ;
begin
  prm := TGIS_ParamsArea( prepareParams ) ;
  with prm do begin
    if FOutlineSymbolGap = _value then exit ;
    FOutlineSymbolGap := _value ;
    FOutlineSymbolGapAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsArea.fset_OutlineSymbolRotate(
  const _value : Double
) ;
var
  prm : TGIS_ParamsArea ;
begin
  prm := TGIS_ParamsArea( prepareParams ) ;
  with prm do begin
    if FOutlineSymbolRotate = _value then exit ;
    FOutlineSymbolRotate := _value ;
    FOutlineSymbolRotateAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsArea.fget_PatternAsText
  : String ;
begin
  Result := pattern_to_text( Symbol, FBitmap, Pattern ) ;
end ;

procedure TGIS_ParamsArea.fset_PatternAsText(
  const _value : String
) ;
var
  tmp_symbol  : TGIS_SymbolAbstract ;
  tmp_bitmap  : TGIS_Bitmap         ;
  tmp_pattern : TGIS_BrushStyle     ;
begin
  tmp_symbol  := Symbol  ;
  tmp_bitmap  := Bitmap  ;
  tmp_pattern := Pattern ;

  try
    pattern_from_text( oConfig, _value, tmp_symbol, tmp_bitmap, tmp_pattern, True ) ;

    Symbol  := tmp_symbol  ;
    Bitmap  := tmp_bitmap  ;
    if assigned( tmp_bitmap ) and ( tmp_bitmap <> Bitmap ) then
      FreeObject( tmp_bitmap ) ;
    Pattern := tmp_pattern ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_AREA_PATTERN,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsArea.fget_SymbolSizeAsText
  : String ;
begin
  Result := size_to_text( FSymbolSizeAsText, SymbolSize, nil ) ;
end ;

procedure TGIS_ParamsArea.fset_SymbolSizeAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsArea ;
  itmp_size  : Integer ;
begin
  itmp_size := SymbolSize ;

  try
    size_from_text( self, _value, itmp_size );

    SymbolSize := itmp_size ;

    prm := TGIS_ParamsArea( prepareParams ) ;
    with prm do begin
      if FSymbolSizeAsText <> _value then begin
        FSymbolSizeAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_AREA_SYMBOLSIZE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsArea.fget_SymbolGapAsText
  : String ;
begin
  Result := size_to_text( FSymbolGapAsText, SymbolGap, nil ) ;
end ;

procedure TGIS_ParamsArea.fset_SymbolGapAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsArea ;
  itmp_size  : Integer ;
begin
  itmp_size := SymbolGap ;

  try
    size_from_text( self, _value, itmp_size );

    SymbolGap := itmp_size ;

    prm := TGIS_ParamsArea( prepareParams ) ;
    with prm do begin
      if FSymbolGapAsText <> _value then begin
        FSymbolGapAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_AREA_SYMBOLGAP,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsArea.fget_SymbolRotateAsText
  : String ;
begin
  Result := angle_to_text( SymbolRotate,
                           FSymbolRotateAsText,
                           FSymbolRotateInternal
                         ) ;
end ;

procedure TGIS_ParamsArea.fset_SymbolRotateAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsArea ;
  itmp_angle : Double ;
begin
  itmp_angle := SymbolRotate ;

  try
    angle_from_text( self, _value, itmp_angle, FSymbolRotateInternal );

    if not assigned( FSymbolRotateInternal ) then
      SymbolRotate := itmp_angle ;

    prm := TGIS_ParamsArea( prepareParams ) ;
    with prm do begin
      if FSymbolRotateAsText <> _value then begin
        FSymbolRotateAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_AREA_SYMBOLROTATE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsArea.fget_OutlineStyleAsText
  : String ;
begin
  Result := pen_to_text( OutlineSymbol, FOutlineStyleAsText,
                         OutlineStyle, FOutlineStyleInternal
                       ) ;
end ;

procedure TGIS_ParamsArea.fset_OutlineStyleAsText(
  const _value : String
) ;
var
  prm         : TGIS_ParamsArea     ;
  tmp_symbol  : TGIS_SymbolAbstract ;
  tmp_style   : TGIS_PenStyle       ;
begin
  tmp_symbol  := OutlineSymbol  ;
  tmp_style   := OutlineStyle   ;

  try
    pen_from_text( oConfig,
                   self, _value, tmp_symbol, tmp_style,
                   FOutlineStyleInternal,  True
                 ) ;

    OutlineSymbol  := tmp_symbol  ;
    if not assigned( FOutlineStyleInternal ) then
      OutlineStyle   := tmp_style   ;

    prm := TGIS_ParamsArea( prepareParams ) ;
    with prm do begin
      if FOutlineStyleAsText <> _value then begin
        FOutlineStyleAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_AREA_OUTLINESTYLE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsArea.fget_OutlinePatternAsText
  : String ;
begin
  Result := pattern_to_text( nil, FOutlineBitmap, OutlinePattern ) ;
end ;

procedure TGIS_ParamsArea.fset_OutlinePatternAsText(
  const _value : String
) ;
var
  tmp_symbol  : TGIS_SymbolAbstract ;
  tmp_bitmap  : TGIS_Bitmap         ;
  tmp_pattern : TGIS_BrushStyle     ;
begin
  tmp_symbol  := nil  ;
  tmp_bitmap  := OutlineBitmap  ;
  tmp_pattern := OutlinePattern ;

  try
    pattern_from_text( oConfig,
                       _value, tmp_symbol, tmp_bitmap, tmp_pattern, False
                     ) ;

    OutlineBitmap  := tmp_bitmap  ;
    if assigned( tmp_bitmap ) and ( tmp_bitmap <> Bitmap ) then
      FreeObject( tmp_bitmap ) ;
    OutlinePattern := tmp_pattern ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_AREA_OUTLINEPATTERN,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsArea.fget_OutlineSymbolGapAsText
  : String ;
begin
  Result := size_to_text( FOutlineSymbolGapAsText, OutlineSymbolGap, nil ) ;
end ;

procedure TGIS_ParamsArea.fset_OutlineSymbolGapAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsArea ;
  itmp_size  : Integer ;
begin
  itmp_size := OutlineSymbolGap ;

  try
    size_from_text( self, _value, itmp_size );

    OutlineSymbolGap := itmp_size ;

    prm := TGIS_ParamsArea( prepareParams ) ;
    with prm do begin
      if FOutlineSymbolGapAsText <> _value then begin
        FOutlineSymbolGapAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_AREA_OUTLINESYMBOLGAP,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsArea.fget_OutlineSymbolRotateAsText
  : String ;
begin
  Result := angle_to_text( OutlineSymbolRotate, FOutlineSymbolRotateAsText, nil ) ;
end ;

procedure TGIS_ParamsArea.fset_OutlineSymbolRotateAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsArea ;
  itmp_angle : Double ;
begin
  itmp_angle := OutlineSymbolRotate ;

  try
    angle_from_text( self, _value, itmp_angle );

    OutlineSymbolRotate := itmp_angle ;

    prm := TGIS_ParamsArea( prepareParams ) ;
    with prm do begin
      if FOutlineSymbolRotateAsText <> _value then begin
        FOutlineSymbolRotateAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_AREA_OUTLINESYMBOLROTATE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsArea.prepareParams
  : TGIS_ParamsVector ;
var
  params : TGIS_ParamsSectionVector ;
begin
  if assigned( FParentProxy ) and
     assigned( TGIS_ParamsSectionVector(FParentProxy).FShape ) then
  begin
   params := prepareParamsVector ;
   if not assigned( params.FArea ) then begin
     params.FArea := TGIS_ParamsArea( CreateCopy ) ;
     params.FArea.Parent := params ;
   end ;
   Result := params.FArea ;
 end
 else
   Result := self ;
end ;

procedure TGIS_ParamsArea.assignInternal(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsArea.Create
  else
    src := _source ;
  try

    if src is TGIS_ParamsArea then begin
      FSymbol                    := TGIS_ParamsArea(src).Symbol                ;
      FSymbolSize                := TGIS_ParamsArea(src).SymbolSize            ;
      FSymbolSizeAsText          := TGIS_ParamsArea(src).SymbolSizeAsText      ;
      FSymbolGap                 := TGIS_ParamsArea(src).SymbolGap             ;
      FSymbolGapAsText           := TGIS_ParamsArea(src).SymbolGapAsText       ;
      FSymbolRotate              := TGIS_ParamsArea(src).SymbolRotate          ;
      FSymbolRotateAsText        := TGIS_ParamsArea(src).SymbolRotateAsText    ;
      TGIS_ParamsField.assignEx   ( self,
                                    FSymbolRotateInternal,
                                    TGIS_ParamsArea(src).FSymbolRotateInternal
                                  ) ;
      FOutlineSymbol             := TGIS_ParamsArea(src).OutlineSymbol         ;
      FOutlineSymbolGap          := TGIS_ParamsArea(src).OutlineSymbolGap      ;
      FOutlineSymbolGapAsText    := TGIS_ParamsArea(src).OutlineSymbolGapAsText;
      FOutlineSymbolRotate       := TGIS_ParamsArea(src).OutlineSymbolRotate   ;
      FOutlineSymbolRotateAsText := TGIS_ParamsArea(src).OutlineSymbolRotateAsText ;
    end ;
    inherited assignInternal(src) ;

  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsArea.Assign(
  _source: TPersistent
) ;
var
  prm : TGIS_ParamsArea ;
begin
  prm := TGIS_ParamsArea( prepareParams ) ;
  prm.assignInternal(_source) ;
end ;

procedure TGIS_ParamsArea.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  stmp  : String ;
  snull : String ;
begin
  with TGIS_Config( _cfg ) do begin
    oConfig := TGIS_Config( _cfg ) ;

    if config_old_format( _cfg ) then begin
      Color                  := ReadColor    ( GIS_INI_AREA_COLOR,
                                               Color
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FColorInternal,
                                               _cfg,
                                               GIS_INI_AREA_COLOREX,
                                               FColorInternal,
                                               snull
                                             ) ;
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_Bitmap,
                                               GIS_INI_AREA_BITMAP,
                                               Bitmap
                                             ) ;
      Pattern                := ReadPattern  ( GIS_INI_AREA_PATTERN,
                                               Pattern
                                             ) ;
      Symbol                 := ReadSymbol   ( GIS_INI_AREA_SYMBOL,
                                               Symbol
                                             ) ;
      SymbolSize             := ReadInteger  ( GIS_INI_AREA_SYMBOLSIZE,
                                               SymbolSize
                                             ) ;
      SymbolGap              := ReadInteger  ( GIS_INI_AREA_SYMBOLGAP,
                                               SymbolGap
                                             ) ;
      SymbolRotate           := ReadFloat    ( GIS_INI_AREA_SYMBOLROTATE,
                                               SymbolRotate
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FSymbolRotateInternal,
                                               _cfg,
                                               GIS_INI_AREA_SYMBOLROTATEEX,
                                               FSymbolRotateInternal,
                                               FSymbolRotateAsText
                                             ) ;
      OutlineStyle           := ReadPen      ( GIS_INI_AREA_OUTLINESTYLE,
                                               OutlineStyle
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineStyleInternal,
                                               _cfg,
                                               GIS_INI_AREA_OUTLINESTYLEEX,
                                               FOutlineStyleInternal,
                                               snull
                                             ) ;
      OutlineWidth           := ReadInteger  ( GIS_INI_AREA_OUTLINEWIDTH,
                                               OutlineWidth
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineWidthInternal,
                                               _cfg,
                                               GIS_INI_AREA_OUTLINEWIDTHEX,
                                               FOutlineWidthInternal,
                                               FOutlineWidthAsText
                                             ) ;
      OffsetX                := ReadInteger  ( GIS_INI_AREA_OFFSETX,
                                               OffsetX
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOffsetXInternal,
                                               _cfg,
                                               GIS_INI_AREA_OFFSETXEX,
                                               FOffsetXInternal,
                                               FOffsetXAsText
                                             ) ;
      OffsetY                := ReadInteger  ( GIS_INI_AREA_OFFSETY,
                                               OffsetY
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOffsetYInternal,
                                               _cfg,
                                               GIS_INI_AREA_OFFSETYEX,
                                               FOffsetYInternal,
                                               FOffsetYAsText
                                             ) ;
      OffsetPosition         := ReadOffsetPosition( GIS_INI_AREA_OFFSETPOSITION,
                                               OffsetPosition
                                             ) ;
      OutlineColor           := ReadColor    ( GIS_INI_AREA_OUTLINECOLOR,
                                               OutlineColor
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineColorInternal,
                                               _cfg,
                                               GIS_INI_AREA_OUTLINECOLOREX,
                                               FOutlineColorInternal,
                                               snull
                                             ) ;
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_OutlineBitmap,
                                               GIS_INI_AREA_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      OutlinePattern         := ReadPattern  ( GIS_INI_AREA_OUTLINEPATTERN,
                                               OutlinePattern
                                             ) ;
      OutlineSymbol          := ReadSymbol   ( GIS_INI_AREA_OUTLINESYMBOL,
                                               OutlineSymbol
                                             ) ;
      OutlineSymbolGap       := ReadInteger  ( GIS_INI_AREA_OUTLINESYMBOLGAP,
                                               OutlineSymbolGap ) ;
      OutlineSymbolRotate    := ReadFloat    ( GIS_INI_AREA_OUTLINESYMBOLROTATE,
                                               OutlineSymbolRotate
                                             ) ;
      SmartSize              := ReadInteger  ( GIS_INI_AREA_SMARTSIZE,
                                               SmartSize
                                             ) ;

      if assigned( FSmartSizeInternal ) then
        stmp                 := ReadString   ( GIS_INI_AREA_SMARTFIELD,
                                               FSmartSizeInternal.FFieldName
                                             )
      else
        stmp                 := ReadString   ( GIS_INI_AREA_SMARTFIELD,
                                               ''
                                             ) ;

      FreeObject( FSmartSizeInternal ) ;
      if not IsStringEmpty( stmp ) then begin
        FSmartSizeInternal := TGIS_ParamsField.Create( self ) ;
        FSmartSizeInternal.Field    := stmp  ;
        FSmartSizeInternal.Scalable := false ;
        FSmartSizeInternal.Factor   := 1     ;
      end;

      ShowLegend             := ReadBoolean  ( GIS_INI_AREA_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
    end;

    if config_new_format( _cfg ) then begin
      ColorAsText            := ReadString   ( GIS_INI_AREA_COLOR,
                                               ColorAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_Bitmap,
                                               GIS_INI_AREA_BITMAP,
                                               Bitmap
                                             ) ;
      PatternAsText          := ReadString   ( GIS_INI_AREA_PATTERN,
                                               PatternAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      Symbol                 := ReadSymbol   ( GIS_INI_AREA_SYMBOL,
                                               Symbol
                                             ) ;
      SymbolSizeAsText       := ReadString   ( GIS_INI_AREA_SYMBOLSIZE,
                                               SymbolSizeAsText
                                             ) ;
      SymbolGapAsText        := ReadString   ( GIS_INI_AREA_SYMBOLGAP,
                                               SymbolGapAsText
                                             ) ;
      SymbolRotateAsText     := ReadString   ( GIS_INI_AREA_SYMBOLROTATE,
                                               SymbolRotateAsText
                                             ) ;
      OutlineStyleAsText     := ReadString   ( GIS_INI_AREA_OUTLINESTYLE,
                                               OutlineStyleAsText
                                             ) ;
      OutlineWidthAsText     := ReadString   ( GIS_INI_AREA_OUTLINEWIDTH,
                                               OutlineWidthAsText
                                             ) ;
      OffsetXAsText          := ReadString   ( GIS_INI_AREA_OFFSETX,
                                               OffsetXAsText
                                             ) ;
      OffsetYAsText          := ReadString   ( GIS_INI_AREA_OFFSETY,
                                               OffsetYAsText
                                             ) ;
      OffsetPosition         := ReadOffsetPosition( GIS_INI_AREA_OFFSETPOSITION,
                                               OffsetPosition
                                             ) ;
      OutlineColorAsText     := ReadString   ( GIS_INI_AREA_OUTLINECOLOR,
                                               OutlineColorAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_OutlineBitmap,
                                               GIS_INI_AREA_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      OutlinePatternAsText   := ReadString   ( GIS_INI_AREA_OUTLINEPATTERN,
                                               OutlinePatternAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      OutlineSymbol          := ReadSymbol   ( GIS_INI_AREA_OUTLINESYMBOL,
                                               OutlineSymbol
                                             ) ;
      OutlineSymbolGapAsText := ReadString   ( GIS_INI_AREA_OUTLINESYMBOLGAP,
                                               OutlineSymbolGapAsText
                                             ) ;
      OutlineSymbolRotateAsText
                             := ReadString   ( GIS_INI_AREA_OUTLINESYMBOLROTATE,
                                               OutlineSymbolRotateAsText
                                             ) ;
      SmartSizeAsText        := ReadString   ( GIS_INI_AREA_SMARTSIZE,
                                               SmartSizeAsText
                                             ) ;
      ShowLegend             := ReadBoolean  ( GIS_INI_AREA_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
    end ;

    oConfig := nil ;
  end ;
end ;

procedure TGIS_ParamsArea.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  def : TGIS_ParamsArea ;
begin
  if TGIS_ParamsSectionVector(GetRoot).Area = self then
    def := TGIS_ParamsArea.Create
  else
    def := TGIS_ParamsSectionVector(GetRoot).Area ;
  try
    with TGIS_Config( _cfg ) do begin

      if config_old_format( _cfg ) then begin
        WriteColor                           ( GIS_INI_AREA_COLOR,
                                               Color,
                                               def.Color
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_AREA_COLOREX,
                                               FColorInternal
                                             ) ;
        WriteBitmap                          ( GIS_INI_AREA_BITMAP,
                                               Bitmap,
                                               def.Bitmap
                                             ) ;
        WritePattern                         ( GIS_INI_AREA_PATTERN,
                                               Pattern,
                                               def.Pattern
                                             ) ;
        WriteSymbol                          ( GIS_INI_AREA_SYMBOL,
                                               Symbol,
                                               def.Symbol
                                             ) ;
        WriteInteger                         ( GIS_INI_AREA_SYMBOLSIZE,
                                               make_size_DK10( SymbolSize ),
                                               def.SymbolSize
                                             ) ;
        WriteInteger                         ( GIS_INI_AREA_SYMBOLGAP,
                                               make_size_DK10( SymbolGap ),
                                               def.SymbolGap
                                             ) ;
        WriteFloat                           ( GIS_INI_AREA_SYMBOLROTATE,
                                               SymbolRotate,
                                               def.SymbolRotate
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_AREA_SYMBOLROTATEEX,
                                               FSymbolRotateInternal
                                             ) ;
        WritePen                             ( GIS_INI_AREA_OUTLINESTYLE,
                                               OutlineStyle,
                                               def.OutlineStyle
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_AREA_OUTLINESTYLEEX,
                                               FOutlineStyleInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_AREA_OUTLINEWIDTH,
                                               make_size_DK10( OutlineWidth ),
                                               def.OutlineWidth
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_AREA_OUTLINEWIDTHEX,
                                               FOutlineWidthInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_AREA_OFFSETX,
                                               make_size_DK10( OffsetX ),
                                               def.OffsetX
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_AREA_OFFSETXEX,
                                               FOffsetXInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_AREA_OFFSETY,
                                               make_size_DK10( OffsetY ),
                                               def.OffsetY
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_AREA_OFFSETYEX,
                                               FOffsetYInternal
                                             ) ;
        WriteOffsetPosition                  ( GIS_INI_AREA_OFFSETPOSITION,
                                               OffsetPosition,
                                               def.OffsetPosition
                                             ) ;
        WriteColor                           ( GIS_INI_AREA_OUTLINECOLOR,
                                               OutlineColor,
                                               def.OutlineColor
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_AREA_OUTLINECOLOREX,
                                               FOutlineColorInternal
                                             ) ;
        WriteBitmap                          ( GIS_INI_AREA_OUTLINEBITMAP,
                                               OutlineBitmap,
                                               def.OutlineBitmap
                                             ) ;
        WritePattern                         ( GIS_INI_AREA_OUTLINEPATTERN,
                                               OutlinePattern,
                                               def.OutlinePattern
                                             ) ;
        WriteSymbol                          ( GIS_INI_AREA_OUTLINESYMBOL,
                                               OutlineSymbol,
                                               def.OutlineSymbol
                                             ) ;
        WriteInteger                         ( GIS_INI_AREA_OUTLINESYMBOLGAP,
                                               make_size_DK10( OutlineSymbolGap ),
                                               def.OutlineSymbolGap
                                             ) ;
        WriteFloat                           ( GIS_INI_AREA_OUTLINESYMBOLROTATE,
                                               OutlineSymbolRotate,
                                               def.OutlineSymbolRotate
                                             ) ;
        WriteInteger                         ( GIS_INI_AREA_SMARTSIZE,
                                               make_size_DK10( SmartSize ),
                                               def.SmartSize
                                             ) ;
        if assigned( FSmartSizeInternal          ) and
           ( FSmartSizeInternal.Scalable = False ) and
           ( FSmartSizeInternal.Factor   = 1     )
        then
          WriteString                        ( GIS_INI_AREA_SMARTFIELD,
                                               FSmartSizeInternal.Field,
                                               ''
                                             ) ;
        WriteBoolean                         ( GIS_INI_AREA_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;

      if config_new_format( _cfg ) then begin
        WriteString                          ( GIS_INI_AREA_COLOR,
                                               ColorAsText,
                                               def.ColorAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_AREA_PATTERN,
                                               PatternAsText,
                                               def.PatternAsText
                                             ) ;
        WriteString                          ( GIS_INI_AREA_SYMBOLSIZE,
                                               SymbolSizeAsText,
                                               def.SymbolSizeAsText
                                             ) ;
        WriteString                          ( GIS_INI_AREA_SYMBOLGAP,
                                               SymbolGapAsText,
                                               def.SymbolGapAsText
                                             ) ;
        WriteString                          ( GIS_INI_AREA_SYMBOLROTATE,
                                               SymbolRotateAsText,
                                               def.SymbolRotateAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_AREA_OUTLINESTYLE,
                                               OutlineStyleAsText,
                                               def.OutlineStyleAsText
                                             ) ;
        WriteString                          ( GIS_INI_AREA_OUTLINEWIDTH,
                                               OutlineWidthAsText,
                                               def.OutlineWidthAsText
                                             ) ;
        WriteString                          ( GIS_INI_AREA_OFFSETX,
                                               OffsetXAsText,
                                               def.OffsetXAsText
                                             ) ;
        WriteString                          ( GIS_INI_AREA_OFFSETY,
                                               OffsetYAsText,
                                               def.OffsetYAsText
                                             ) ;
        WriteOffsetPosition                  ( GIS_INI_AREA_OFFSETPOSITION,
                                               OffsetPosition,
                                               def.OffsetPosition
                                             ) ;
        WriteString                          ( GIS_INI_AREA_OUTLINECOLOR,
                                               OutlineColorAsText,
                                               def.OutlineColorAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_AREA_OUTLINEPATTERN,
                                               OutlinePatternAsText,
                                               def.OutlinePatternAsText
                                             ) ;
        WriteString                          ( GIS_INI_AREA_OUTLINESYMBOLGAP,
                                               OutlineSymbolGapAsText,
                                               def.OutlineSymbolGapAsText
                                             ) ;
        WriteString                          ( GIS_INI_AREA_OUTLINESYMBOLROTATE,
                                               OutlineSymbolRotateAsText,
                                               def.OutlineSymbolRotateAsText
                                             ) ;
        WriteString                          ( GIS_INI_AREA_SMARTSIZE,
                                               SmartSizeAsText,
                                               def.SmartSizeAsText
                                             ) ;
        WriteBoolean                         ( GIS_INI_AREA_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;

    end;
  finally
    if TGIS_ParamsSectionVector(GetRoot).Area = self then
      FreeObject( def ) ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsMarker'}

constructor TGIS_ParamsMarker.Create ;
begin
  inherited ;

  FSmartSize            := -1 ;

  FStyle                := TGIS_MarkerStyle.Box ;
  FSize                 := 100 ;
  FSizeAsText           := '' ;
  FColor                := TGIS_Color.Red ;
  FPattern              := TGIS_BrushStyle.Solid ;
  FSymbol               := nil ;
  FSymbolRotate         := 0 ;
  FSymbolRotateAsText   := '' ;
  FOutlineStyle         := TGIS_PenStyle.Solid ;
  FOutlineStyleAsText   := '' ;
  FOutlineWidth         := 0 ;
  FOutlineWidthAsText   := '' ;
  FOutlineColor         := TGIS_Color.Gray ;
  FOutlineBackcolor     := TGIS_Color.Crazy ;
  FOutlinePattern       := TGIS_BrushStyle.Solid ;
  FOffsetX              := 0 ;
  FOffsetXAsText        := '' ;
end ;

procedure TGIS_ParamsMarker.doDestroy ;
begin
  FreeObject( FSizeInternal ) ;
  FreeObject( FSymbolRotateInternal ) ;
  inherited ;
end ;

procedure TGIS_ParamsMarker.fset_Style(
  const _value : TGIS_MarkerStyle
) ;
var
  prm : TGIS_ParamsMarker ;
begin
  prm := TGIS_ParamsMarker( prepareParams ) ;
  with prm do begin
    if FStyle = _value then exit ;
    FStyle := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsMarker.fset_Symbol(
  const _value : TGIS_SymbolAbstract
) ;
var
  prm : TGIS_ParamsMarker ;
begin
  prm := TGIS_ParamsMarker( prepareParams ) ;
  with prm do begin
    if FSymbol = _value then exit ;
    FSymbol := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsMarker.fget_SymbolRotate
  : Double ;
begin
  Result := compute_rotate( Shape, FSymbolRotate, FSymbolRotateInternal ) ;
end ;

function TGIS_ParamsMarker.fget_SymbolRotateIndirect
  : Boolean ;
begin
  Result := assigned( FSymbolRotateInternal ) and
            ( not IsStringEmpty( FSymbolRotateInternal.Field ) ) ;
end;

procedure TGIS_ParamsMarker.fset_SymbolRotate(
  const _value : Double
) ;
var
  prm : TGIS_ParamsMarker ;
begin
  prm := TGIS_ParamsMarker( prepareParams ) ;
  with prm do begin
    if FSymbolRotate = _value then exit ;
    FreeObject( FSymbolRotateInternal ) ;
    FSymbolRotate := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsMarker.fget_Size : Integer ;
begin
  Result := compute_size( Shape, FSize, FSizeInternal ) ;
end ;

procedure TGIS_ParamsMarker.fset_Size(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsMarker ;
begin
  prm := TGIS_ParamsMarker( prepareParams ) ;
  with prm do begin
    if FSize = _value then exit ;
    FreeObject( FSizeInternal ) ;
    FSize := _value ;
    FSizeAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsMarker.fget_StyleAsText
  : String ;
begin
  Result := marker_to_text( Symbol, Style ) ;
end ;

procedure TGIS_ParamsMarker.fset_StyleAsText(
  const _value : String
);
var
  tmp_symbol  : TGIS_SymbolAbstract ;
  tmp_style   : TGIS_MarkerStyle       ;
begin
  tmp_symbol  := Symbol  ;
  tmp_style   := Style   ;

  try
    marker_from_text( oConfig, _value, tmp_symbol, tmp_style, True ) ;

    Style   := tmp_style   ;
    Symbol  := tmp_symbol ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_MARKER_STYLE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsMarker.fget_SizeAsText
  : String ;
begin
  Result := size_to_text( FSizeAsText, Size, FSizeInternal ) ;
end ;

procedure TGIS_ParamsMarker.fset_SizeAsText(
  const _value : String
);
var
  prm        : TGIS_ParamsMarker ;
  itmp_size  : Integer ;
begin
  itmp_size := Size ;

  try
    size_from_text( self, _value, itmp_size, FSizeInternal );

    if not assigned( FSizeInternal ) then
      Size := itmp_size ;
    FSizeAsText := _value ;

    prm := TGIS_ParamsMarker( prepareParams ) ;
    with prm do begin
      if FSizeAsText <> _value then begin
        FSizeAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_MARKER_SIZE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsMarker.fget_SymbolRotateAsText
  : String ;
begin
  Result := angle_to_text( SymbolRotate,
                           FSymbolRotateAsText,
                           FSymbolRotateInternal
                         ) ;
end ;

procedure TGIS_ParamsMarker.fset_SymbolRotateAsText(
  const _value : String
);
var
  prm        : TGIS_ParamsMarker ;
  itmp_angle : Double ;
begin
  itmp_angle := SymbolRotate ;

  try
    angle_from_text( self, _value, itmp_angle, FSymbolRotateInternal );

    if not assigned( FSymbolRotateInternal ) then
      SymbolRotate := itmp_angle ;

    prm := TGIS_ParamsMarker( prepareParams ) ;
    with prm do begin
      if FSymbolRotateAsText <> _value then begin
        FSymbolRotateAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_MARKER_SYMBOLROTATE,
      '"' + _value + '"'
    );
  end;
end ;


function TGIS_ParamsMarker.prepareParams
  : TGIS_ParamsVector ;
var
  params : TGIS_ParamsSectionVector ;
begin
  if assigned( FParentProxy ) and
     assigned( TGIS_ParamsSectionVector(FParentProxy).FShape ) then
  begin
    params := prepareParamsVector ;
    if not assigned( params.FMarker ) then begin
      params.FMarker := TGIS_ParamsMarker( CreateCopy ) ;
      params.FMarker.Parent := params ;
    end ;
    Result := params.FMarker ;
  end
  else
    Result := self ;
end ;

procedure TGIS_ParamsMarker.assignInternal(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsMarker.Create
  else
    src := _source ;
  try
    if src is TGIS_ParamsMarker then begin
      FStyle                     := TGIS_ParamsMarker(src).Style          ;
      FSize                      := TGIS_ParamsMarker(src).Size           ;
      FSizeAsText                := TGIS_ParamsMarker(src).SizeAsText     ;
      TGIS_ParamsField.assignEx   ( self,
                                    FSizeInternal,
                                    TGIS_ParamsMarker(src).FSizeInternal
                                  ) ;
      FSymbol                    := TGIS_ParamsMarker(src).Symbol         ;
      FSymbolRotate              := TGIS_ParamsMarker(src).SymbolRotate   ;
      FSymbolRotateAsText        := TGIS_ParamsMarker(src).SymbolRotateAsText ;
      TGIS_ParamsField.assignEx   ( self,
                                    FSymbolRotateInternal,
                                    TGIS_ParamsMarker(src).FSymbolRotateInternal
                                  ) ;
    end ;
    inherited assignInternal(src) ;
  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsMarker.Assign(
  _source: TPersistent
) ;
var
  prm : TGIS_ParamsMarker ;
begin
  prm := TGIS_ParamsMarker( prepareParams ) ;
  prm.assignInternal(_source) ;
end ;

procedure TGIS_ParamsMarker.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  stmp  : String ;
  snull : String ;
begin
  with TGIS_Config( _cfg ) do begin
    oConfig := TGIS_Config( _cfg ) ;

    if config_old_format( _cfg ) then begin
      Style                  := ReadMarker   ( GIS_INI_MARKER_STYLE,
                                               Style
                                             ) ;
      Size                   := ReadInteger  ( GIS_INI_MARKER_SIZE,
                                               Size
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FSizeInternal,
                                               _cfg,
                                               GIS_INI_MARKER_SIZEEX,
                                               FSizeInternal,
                                               FSizeAsText
                                             ) ;
      Color                  := ReadColor    ( GIS_INI_MARKER_COLOR,
                                              Color
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FColorInternal,
                                               _cfg,
                                               GIS_INI_MARKER_COLOREX,
                                               FColorInternal,
                                               snull
                                             ) ;
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_Bitmap,
                                               GIS_INI_MARKER_BITMAP,
                                               Bitmap
                                             ) ;
      Pattern               := ReadPattern   ( GIS_INI_MARKER_PATTERN,
                                               Pattern
                                             ) ;
      Symbol                := ReadSymbol    ( GIS_INI_MARKER_SYMBOL,
                                               Symbol
                                             ) ;
      SymbolRotate          := ReadFloat     ( GIS_INI_MARKER_SYMBOLROTATE,
                                               SymbolRotate
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FSymbolRotateInternal,
                                               _cfg,
                                               GIS_INI_MARKER_SYMBOLROTATEEX,
                                               FSymbolRotateInternal,
                                               FSymbolRotateAsText
                                             ) ;
      OutlineStyle           := ReadPen      ( GIS_INI_MARKER_OUTLINESTYLE,
                                               OutlineStyle
                                             ) ;
      OutlineWidth           := ReadInteger  ( GIS_INI_MARKER_OUTLINEWIDTH,
                                               OutlineWidth
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineWidthInternal,
                                               _cfg,
                                               GIS_INI_MARKER_OUTLINEWIDTHEX,
                                               FOutlineWidthInternal,
                                               FOutlineWidthAsText
                                             ) ;
      OffsetX                := ReadInteger  ( GIS_INI_MARKER_OFFSETX,
                                               OffsetX
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOffsetXInternal,
                                               _cfg,
                                               GIS_INI_MARKER_OFFSETXEX,
                                               FOffsetXInternal,
                                               FOffsetXAsText
                                             ) ;
      OffsetY                := ReadInteger  ( GIS_INI_MARKER_OFFSETY,
                                               OffsetY
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOffsetYInternal,
                                               _cfg,
                                               GIS_INI_MARKER_OFFSETYEX,
                                               FOffsetYInternal,
                                               FOffsetYAsText
                                             ) ;
      OffsetPosition         := ReadOffsetPosition( GIS_INI_MARKER_OFFSETPOSITION,
                                               OffsetPosition
                                             ) ;
      OutlineColor           := ReadColor    ( GIS_INI_MARKER_OUTLINECOLOR,
                                              OutlineColor
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineColorInternal,
                                               _cfg,
                                               GIS_INI_MARKER_OUTLINECOLOREX,
                                               FOutlineColorInternal,
                                               snull
                                             ) ;
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_OutlineBitmap,
                                               GIS_INI_MARKER_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      OutlinePattern        := ReadPattern   ( GIS_INI_MARKER_OUTLINEPATTERN,
                                               OutlinePattern
                                             ) ;
      SmartSize             := ReadInteger   ( GIS_INI_MARKER_SMARTSIZE,
                                               SmartSize
                                             ) ;
      if assigned( FSmartSizeInternal ) then
        stmp                 := ReadString   ( GIS_INI_MARKER_SMARTFIELD,
                                               FSmartSizeInternal.FFieldName
                                             )
      else
        stmp                 := ReadString   ( GIS_INI_MARKER_SMARTFIELD,
                                               ''
                                             ) ;

      FreeObject( FSmartSizeInternal ) ;
      if not IsStringEmpty( stmp ) then begin
        FSmartSizeInternal := TGIS_ParamsField.Create( self ) ;
        FSmartSizeInternal.Field    := stmp  ;
        FSmartSizeInternal.Scalable := false ;
        FSmartSizeInternal.Factor   := 1     ;
      end;

      ShowLegend            := ReadBoolean   ( GIS_INI_MARKER_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
    end ;

    if config_new_format( _cfg ) then begin
      StyleAsText            := ReadString   ( GIS_INI_MARKER_STYLE,
                                               StyleAsText
                                             ) ;
      SizeAsText             := ReadString   ( GIS_INI_MARKER_SIZE,
                                               SizeAsText
                                             ) ;
      ColorAsText            := ReadString   ( GIS_INI_MARKER_COLOR,
                                               ColorAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_Bitmap,
                                               GIS_INI_MARKER_BITMAP,
                                               Bitmap
                                             ) ;
      PatternAsText          := ReadString   ( GIS_INI_MARKER_PATTERN,
                                               PatternAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      Symbol                 := ReadSymbol   ( GIS_INI_MARKER_SYMBOL,
                                               Symbol
                                              ) ;
      SymbolRotateAsText     := ReadString   ( GIS_INI_MARKER_SYMBOLROTATE,
                                               SymbolRotateAsText
                                             ) ;
      OutlineStyleAsText     := ReadString   ( GIS_INI_MARKER_OUTLINESTYLE,
                                               OutlineStyleAsText
                                             ) ;
      OutlineWidthAsText     := ReadString   ( GIS_INI_MARKER_OUTLINEWIDTH,
                                               OutlineWidthAsText
                                             ) ;
      OffsetXAsText          := ReadString   ( GIS_INI_MARKER_OFFSETX,
                                               OffsetXAsText
                                             ) ;
      OffsetYAsText          := ReadString   ( GIS_INI_MARKER_OFFSETY,
                                               OffsetYAsText
                                             ) ;
      OffsetPosition         := ReadOffsetPosition( GIS_INI_MARKER_OFFSETPOSITION,
                                               OffsetPosition
                                             ) ;
      OutlineColorAsText     := ReadString   ( GIS_INI_MARKER_OUTLINECOLOR,
                                               OutlineColorAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_OutlineBitmap,
                                               GIS_INI_MARKER_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      OutlinePatternAsText  := ReadString    ( GIS_INI_MARKER_OUTLINEPATTERN,
                                               OutlinePatternAsText
                                             ) ;
      SmartSizeAsText       := ReadString    ( GIS_INI_MARKER_SMARTSIZE,
                                               SmartSizeAsText
                                             ) ;
      ShowLegend            := ReadBoolean   ( GIS_INI_MARKER_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
    end ;

    oConfig := nil ;
  end ;

end ;

procedure TGIS_ParamsMarker.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  def : TGIS_ParamsMarker ;
begin
  if TGIS_ParamsSectionVector(GetRoot).Marker = self then
    def := TGIS_ParamsMarker.Create
  else
    def := TGIS_ParamsSectionVector(GetRoot).Marker ;
  try
    with TGIS_Config( _cfg ) do begin

      if config_old_format( _cfg ) then begin
        WriteMarker                          ( GIS_INI_MARKER_STYLE,
                                               Style,
                                               def.Style
                                             ) ;
        WriteInteger                         ( GIS_INI_MARKER_SIZE,
                                               make_size_DK10( Size ),
                                               def.Size
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_MARKER_SIZEEX,
                                               FSizeInternal
                                             ) ;
        WriteColor                           ( GIS_INI_MARKER_COLOR,
                                               Color,
                                               def.Color
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_MARKER_COLOREX,
                                               FColorInternal
                                             ) ;
        WriteBitmap                          ( GIS_INI_MARKER_BITMAP,
                                               Bitmap,
                                               def.Bitmap
                                             ) ;
        WritePattern                         ( GIS_INI_MARKER_PATTERN,
                                               Pattern,
                                               def.Pattern
                                             ) ;
        WriteSymbol                          ( GIS_INI_MARKER_SYMBOL,
                                               Symbol,
                                               def.Symbol
                                             ) ;
        WriteFloat                           ( GIS_INI_MARKER_SYMBOLROTATE,
                                               SymbolRotate,
                                               def.SymbolRotate
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_MARKER_SYMBOLROTATEEX,
                                               FSymbolRotateInternal
                                             ) ;
        WritePen                             ( GIS_INI_MARKER_OUTLINESTYLE,
                                               OutlineStyle,
                                               def.OutlineStyle
                                             ) ;
        WriteInteger                         ( GIS_INI_MARKER_OUTLINEWIDTH,
                                               make_size_DK10( OutlineWidth ),
                                               def.OutlineWidth
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_MARKER_OUTLINEWIDTHEX,
                                               FOutlineWidthInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_MARKER_OFFSETX,
                                               make_size_DK10( OffsetX ),
                                               def.OffsetX
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_MARKER_OFFSETXEX,
                                               FOffsetXInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_MARKER_OFFSETY,
                                               make_size_DK10( OffsetY ),
                                               def.OffsetY
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_MARKER_OFFSETYEX,
                                               FOffsetYInternal
                                             ) ;
        WriteOffsetPosition                  ( GIS_INI_MARKER_OFFSETPOSITION,
                                               OffsetPosition,
                                               def.OffsetPosition
                                             ) ;
        WriteColor                           ( GIS_INI_MARKER_OUTLINECOLOR,
                                               OutlineColor,
                                               def.OutlineColor
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_MARKER_OUTLINECOLOREX,
                                               FOutlineColorInternal
                                             ) ;
        WriteBitmap                          ( GIS_INI_MARKER_OUTLINEBITMAP,
                                               OutlineBitmap,
                                               def.OutlineBitmap
                                             ) ;
        WritePattern                         ( GIS_INI_MARKER_OUTLINEPATTERN,
                                               OutlinePattern,
                                               def.OutlinePattern
                                             ) ;
        WriteInteger                         ( GIS_INI_MARKER_SMARTSIZE,
                                               make_size_DK10( SmartSize ),
                                               def.SmartSize
                                             ) ;
        if assigned( FSmartSizeInternal          ) and
           ( FSmartSizeInternal.Scalable = False ) and
           ( FSmartSizeInternal.Factor   = 1     )
        then
          WriteString                        ( GIS_INI_MARKER_SMARTFIELD,
                                               FSmartSizeInternal.Field,
                                               ''
                                             ) ;
        WriteBoolean                         ( GIS_INI_MARKER_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;

      if config_new_format( _cfg ) then begin
        WriteStyle                           ( GIS_INI_MARKER_STYLE,
                                               StyleAsText,
                                               def.StyleAsText
                                             ) ;
        WriteString                          ( GIS_INI_MARKER_SIZE,
                                               SizeAsText,
                                               def.SizeAsText
                                             ) ;
        WriteString                          ( GIS_INI_MARKER_COLOR,
                                               ColorAsText,
                                               def.ColorAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_MARKER_PATTERN,
                                               PatternAsText,
                                               def.PatternAsText
                                             ) ;
        WriteString                          ( GIS_INI_MARKER_SYMBOLROTATE,
                                               SymbolRotateAsText,
                                               def.SymbolRotateAsText
                                             ) ;
        WriteString                          ( GIS_INI_MARKER_OUTLINESTYLE,
                                               OutlineStyleAsText,
                                               def.OutlineStyleAsText
                                             ) ;
        WriteString                          ( GIS_INI_MARKER_OUTLINEWIDTH,
                                               OutlineWidthAsText,
                                               def.OutlineWidthAsText
                                             ) ;
        WriteString                          ( GIS_INI_MARKER_OFFSETX,
                                               OffsetXAsText,
                                               def.OffsetXAsText
                                             ) ;
        WriteString                          ( GIS_INI_MARKER_OFFSETY,
                                               OffsetYAsText,
                                               def.OffsetYAsText
                                             ) ;
        WriteOffsetPosition                  ( GIS_INI_MARKER_OFFSETPOSITION,
                                               OffsetPosition,
                                               def.OffsetPosition
                                             ) ;
        WriteString                          ( GIS_INI_MARKER_OUTLINECOLOR,
                                               OutlineColorAsText,
                                               def.OutlineColorAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_MARKER_OUTLINEPATTERN,
                                               OutlinePatternAsText,
                                               def.OutlinePatternAsText
                                             ) ;
        WriteString                          ( GIS_INI_MARKER_SMARTSIZE,
                                               SmartSizeAsText,
                                               def.SmartSizeAsText
                                             ) ;
        WriteBoolean                         ( GIS_INI_MARKER_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;

    end ;
  finally
    if TGIS_ParamsSectionVector(GetRoot).Marker = self then
      FreeObject( def ) ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsLabelFont'}

class function TGIS_ParamsLabelFont.PtToFontSize(
  const _value : Integer
) : Integer ;
begin
  if _value = GIS_RENDER_SIZE then
    Result := GIS_RENDER_SIZE
  else
    Result := Abs( _value * 20) ;
end;

class function TGIS_ParamsLabelFont.FontSizeToPt(
  const _value : Integer
) : Integer ;
var
  tmp : Integer ;
begin
  tmp := 0 ;
  if _value = GIS_RENDER_SIZE then
    Result := GIS_RENDER_SIZE
  else if _value < 0 then begin
    if Abs( _value ) > GIS_AUTOSIZE_SIZE then
      tmp := 10 * 1440 div 96
    else
      tmp := _value ;
    tmp := RoundS( tmp * 96 / 1440 * 20  ) ;
  end
  else begin
    if Abs( _value) > GIS_AUTOSIZE_SIZE then
      tmp := 10 * 20
    else
      tmp := _value ;
  end;

  Result := Min( tmp div 20, 250 ) ;
end ;

function TGIS_ParamsLabelFont.fget_name
  : String ;
begin
  Result := TGIS_ParamsLabel( FParent ).FontName ;
end;

procedure TGIS_ParamsLabelFont.fset_name(
  const _value : String
) ;
begin
  TGIS_ParamsLabel( FParent ).FontName := _value ;
end;

function TGIS_ParamsLabelFont.fget_size : Integer ;
begin
  Result := FontSizeToPt( TGIS_ParamsLabel( FParent ).FontSize ) ;
end;

procedure TGIS_ParamsLabelFont.fset_size(
  const _value : Integer
) ;
begin
  TGIS_ParamsLabel( FParent ).FontSize := PtToFontSize( _value ) ;
end ;

function TGIS_ParamsLabelFont.fget_style
  : TGIS_FontStyles ;
begin
  Result := TGIS_ParamsLabel( FParent ).FontStyle ;
end;

procedure TGIS_ParamsLabelFont.fset_style(
  const _value : TGIS_FontStyles
) ;
begin
  TGIS_ParamsLabel( FParent ).FontStyle := _value ;
end;

procedure TGIS_ParamsLabelFont.Assign(const _source: TGIS_Font);
begin
  TGIS_ParamsLabel( FParent ).FontName  := _source.Name ;
  TGIS_ParamsLabel( FParent ).FontSize  := _source.Size * 20 ;
  TGIS_ParamsLabel( FParent ).FontStyle := _source.Style ;
  TGIS_ParamsLabel( FParent ).FontColor := _source.Color ;
end;

function TGIS_ParamsLabelFont.fget_color
  : TGIS_Color ;
begin
   Result := TGIS_ParamsLabel( FParent ).FontColor ;
end;

procedure TGIS_ParamsLabelFont.fset_color(
  const _value : TGIS_Color
) ;
begin
  TGIS_ParamsLabel( FParent ).FontColor := _value ;
end;
{$ENDREGION}
{$REGION 'TGIS_ParamsLabel'}

constructor TGIS_ParamsLabel.Create ;
begin
  inherited ;

  FSmartSize            := -1 ;

  FVisible              := True ;
  FAllocator            := True ;
  FDuplicates           := True ;
  FColor                := TGIS_Color.Yellow ;
  FPattern              := TGIS_BrushStyle.Clear ;
  FOutlineStyle         := TGIS_PenStyle.Clear ;
  FOutlineStyleAsText   := '' ;
  FOutlineWidth         := 1 ;
  FOutlineWidthAsText   := '' ;
  FOutlineColor         := TGIS_Color.Black ;
  FOutlineBackcolor     := TGIS_Color.Crazy ;
  FOutlinePattern       := TGIS_BrushStyle.Solid ;
  FOffsetX              := 0 ;
  FOffsetXAsText        := '' ;
  FOffsetY              := 0 ;
  FOffsetYAsText        := '' ;
  FOffsetPosition       := TGIS_OffsetPosition.DownRight ;
  FFont                 := nil ;
  FontName              := 'Arial' ;
  FontStyle             := GisGetEmptyFontStyle ;
  FontColor             := TGIS_Color.Black ;
  FontSize              := 8 * 20 ;
  FFontSizeAsText       := '' ;
  FHeight               := 2500 ;
  FWidth                := 2500 ;
  FRotate               := 0 ;
  FRotateAsText         := '' ;
  FPosition             := GisGetLabelPosition(TGIS_LabelPosition.UpRight) ;
  FAlignment            := TGIS_LabelAlignment.LeftJustify ;
end ;

procedure TGIS_ParamsLabel.doDestroy ;
begin
  FreeObject( FFont ) ;
  FreeObject( FFontColorInternal ) ;
  FreeObject( FFontSizeInternal  ) ;
  FreeObject( FPositionInternal  ) ;
  FreeObject( FRotateInternal    ) ;
  inherited ;
end ;

procedure TGIS_ParamsLabel.fset_Visible(
  const _value : Boolean
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FVisible = _value then exit ;
    FVisible := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsLabel.fset_Allocator(
  const _value : Boolean
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FAllocator = _value then exit ;
    FAllocator := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsLabel.fset_Duplicates(
  const _value : Boolean
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FDuplicates = _value then exit ;
    FDuplicates := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsLabel.fset_Field(
  const _value : String
) ;
var
  prm  : TGIS_ParamsLabel ;
  tmp  : String           ;
  otmp : String           ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    otmp := FField ;
    tmp := GisDeNormalizedSQLName( _value ) ;
    if FField = tmp then exit ;

    FField := tmp ;

    if ( not IsStringEmpty( FField ) ) then
      FValue := '{' + FField + '}'
    else
      FValue := '' ;

    Touch ;
  end ;
end ;

procedure TGIS_ParamsLabel.fset_Value(
  const _value : String
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FValue = _value then exit ;
    FValue := _value ;
    FField := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsLabel.fset_Alignment(
  const _value : TGIS_LabelAlignment
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FAlignment = _value then exit ;
    FAlignment := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLabel.fget_Position
  : TGIS_LabelPositions ;
begin
  Result := compute_position( Shape, FPosition, FPositionInternal ) ;
end;

procedure TGIS_ParamsLabel.fset_Position(
  const _value : TGIS_LabelPositions
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FPosition = _value then exit ;
    FPosition := _value ;
    FPositionAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLabel.fget_Font : TGIS_ParamsLabelFont ;
begin
  if not assigned( FFont ) then begin
    FFont := TGIS_ParamsLabelFont.Create ;
    FFont.FParent := self ;
  end ;
  Result := FFont ;
end ;

function TGIS_ParamsLabel.fget_Rotate : Double ;
begin
  Result := compute_rotate( Shape, FRotate, FRotateInternal ) ;
end ;

procedure TGIS_ParamsLabel.fset_Height(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FHeight = _value then exit ;
    FHeight := _value ;
    FHeightAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsLabel.fset_Width(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FWidth = _value then exit ;
    FWidth := _value ;
    FWidthAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsLabel.fset_Rotate(
  const _value : Double
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FRotate = _value then exit ;
    FreeObject( FRotateInternal ) ;
    FRotate := _value ;
    FRotateAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLabel.fget_FontName
  : String ;
begin
  Result := FFontName ;
end;

procedure TGIS_ParamsLabel.fset_FontName(
  const _value : String
);
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FFontName = _value then exit ;
    FFontName := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLabel.fget_FontSize
  : Integer;
begin
  Result := compute_size( Shape, FFontSize, FFontSizeInternal ) ;
end;

procedure TGIS_ParamsLabel.fset_FontSize(
  const _value : Integer
);
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FFontSize = _value then exit ;
    FFontSize := _value ;
    FFontSizeAsText := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLabel.fget_FontStyle
  : TGIS_FontStyles  ;
begin
  Result := FFontStyle ;
end;

procedure TGIS_ParamsLabel.fset_FontStyle(
  const _value : TGIS_FontStyles
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FFontStyle = _value then exit ;
    FFontStyle := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLabel.fget_FontColor
  : TGIS_Color ;
begin
  Result := compute_color( Shape, FFontColor, FFontColorInternal ) ;
end;

procedure TGIS_ParamsLabel.fset_FontColor(
  const _value : TGIS_Color
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FFontColor = _value then exit ;
    FreeObject( FFontColorInternal ) ;
    FFontColor := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLabel.fget_HeightAsText
  : String ;
begin
  Result := size_to_text( FHeightAsText, Height, nil ) ;
end;

procedure TGIS_ParamsLabel.fset_HeightAsText(
  const _value : String
);
var
  prm        : TGIS_ParamsLabel ;
  itmp_size  : Integer ;
begin
  itmp_size := Height ;

  try
    size_from_text( self, _value, itmp_size );

    Height := itmp_size ;

    prm := TGIS_ParamsLabel( prepareParams ) ;
    with prm do begin
      if FHeightAsText <> _value then begin
        FHeightAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LABEL_HEIGHT,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsLabel.fget_PositionAsText
  : String ;
begin
  Result := position_to_text( Position, FPositionInternal ) ;
end ;

procedure TGIS_ParamsLabel.fset_PositionAsText(
  const _value : String
) ;
var
  prm     : TGIS_ParamsLabel    ;
  tmp_pos : TGIS_LabelPositions ;
begin
  tmp_pos := Position ;

  try
    position_from_text( self, _value, tmp_pos, FPositionInternal );
    Position := tmp_pos ;

    prm := TGIS_ParamsLabel( prepareParams ) ;
    with prm do begin
      if FPositionAsText <> _value then begin
        FPositionAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LABEL_POSITION,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsLabel.fget_FontColorAsText
  : String ;
begin
  Result := color_to_text( FontColor, FFontColorInternal ) ;
end ;

procedure TGIS_ParamsLabel.fset_FontColorAsText(
  const _value : String
) ;
var
  tmp_color : TGIS_Color ;
begin
  tmp_color := FontColor ;

  try
    color_from_text( self, _value, tmp_color, FFontColorInternal );

    if not assigned( FFontColorInternal ) then
      FontColor := tmp_color ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LABEL_FONTCOLOR,
      '"' + _value + '"'
    );
  end;
end ;
function TGIS_ParamsLabel.fget_FontSizeAsText
  : String ;
begin
  Result := size_to_text( FFontSizeAsText, FontSize, FFontSizeInternal ) ;
end;

procedure TGIS_ParamsLabel.fset_FontSizeAsText(
  const _value : String
);
var
  prm        : TGIS_ParamsLabel ;
  itmp_size  : Integer ;
begin
  itmp_size := FontSize ;

  try
    size_from_text( self, _value, itmp_size, FFontSizeInternal );

    FontSize := itmp_size ;

    prm := TGIS_ParamsLabel( prepareParams ) ;
    with prm do begin
      if FFontSizeAsText <> _value then begin
        FFontSizeAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LABEL_FONTSIZE,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsLabel.fget_WidthAsText
  : String ;
begin
  Result := size_to_text( FWidthAsText, Width, nil ) ;
end;

procedure TGIS_ParamsLabel.fset_WidthAsText(
  const _value : String
);
var
  prm        : TGIS_ParamsLabel ;
  itmp_size  : Integer ;
begin
  itmp_size := Width ;

  try
    size_from_text( self, _value, itmp_size );

    Width := itmp_size ;

    prm := TGIS_ParamsLabel( prepareParams ) ;
    with prm do begin
      if FWidthAsText <> _value then begin
        FWidthAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LABEL_WIDTH,
      '"' + _value + '"'
    );
  end;
end;

function TGIS_ParamsLabel.fget_RotateAsText
  : String ;
begin
  Result := angle_to_text( Rotate, FRotateAsText, FRotateInternal ) ;
end;

function TGIS_ParamsLabel.fget_RotateIndirect
  : Boolean ;
begin
  Result := assigned( FRotateInternal ) and
            ( not IsStringEmpty( FRotateInternal.Field ) ) ;
end;

procedure TGIS_ParamsLabel.fset_Shield(
  const _value : TGIS_SymbolAbstract
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  with prm do begin
    if FShield = _value then exit ;
    FShield := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsLabel.fget_ShieldAsText
  : String ;
begin
  Result := symbol_to_text( FShield ) ;
end ;

procedure TGIS_ParamsLabel.fset_ShieldAsText(
  const _value : String
);
var
  tmp_symbol  : TGIS_SymbolAbstract ;
  tmp_bitmap  : TGIS_Bitmap         ;
  tmp_pattern : TGIS_BrushStyle     ;
begin
  tmp_symbol  := nil     ;
  tmp_bitmap  := Bitmap  ;
  tmp_pattern := Pattern ;

  try
    symbol_from_text( oConfig,
                       _value, tmp_symbol
                     ) ;

    Shield  := tmp_symbol  ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_STEM_SHIELD,
      '"' + _value + '"'
    );
  end;
end ;

procedure TGIS_ParamsLabel.fset_RotateAsText(
  const _value : String
);
var
  prm        : TGIS_ParamsLabel ;
  itmp_angle : Double ;
begin
  itmp_angle := Rotate ;

  try
    angle_from_text( self, _value, itmp_angle, FRotateInternal );

    if not assigned( FRotateInternal ) then
      Rotate := itmp_angle ;

    prm := TGIS_ParamsLabel( prepareParams ) ;
    with prm do begin
      if FRotateAsText <> _value then begin
        FRotateAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_LABEL_ROTATE,
      '"' + _value + '"'
    );
  end;
end;


function TGIS_ParamsLabel.prepareParams
  : TGIS_ParamsVector ;
var
  params : TGIS_ParamsSectionVector ;
begin
  if assigned( FParentProxy ) and
     assigned( TGIS_ParamsSectionVector(FParentProxy).FShape ) then
  begin
    params := prepareParamsVector ;
    if not assigned( params.FLabels ) then begin
      params.FLabels := TGIS_ParamsLabel( CreateCopy ) ;
      params.FLabels.Parent := params ;
    end ;
    Result := params.FLabels ;
  end
  else
    Result := self ;
end ;

procedure TGIS_ParamsLabel.assignInternal(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsLabel.Create
  else
    src := _source ;
  try

    if src is TGIS_ParamsLabel then begin
      FVisible                 := TGIS_ParamsLabel(src).Visible             ;
      FAllocator               := TGIS_ParamsLabel(src).Allocator           ;
      FDuplicates              := TGIS_ParamsLabel(src).Duplicates          ;
      FField                   := TGIS_ParamsLabel(src).Field               ;
      FValue                   := TGIS_ParamsLabel(src).Value               ;
      FAlignment               := TGIS_ParamsLabel(src).Alignment           ;
      FPosition                := TGIS_ParamsLabel(src).Position            ;
      TGIS_ParamsField.assignEx ( self,
                                  FPositionInternal,
                                  TGIS_ParamsLabel(src).FPositionInternal
                                ) ;
      FPositionAsText          := TGIS_ParamsLabel(src).PositionAsText      ;
      FRotate                  := TGIS_ParamsLabel(src).Rotate              ;
      FRotateAsText            := TGIS_ParamsLabel(src).RotateAsText        ;
      TGIS_ParamsField.assignEx ( self,
                                  FRotateInternal,
                                  TGIS_ParamsLabel(src).FRotateInternal
                                ) ;
      FontName                 := TGIS_ParamsLabel(src).FontName            ;
      FontSize                 := TGIS_ParamsLabel(src).FontSize            ;
      FFontSizeAsText          := TGIS_ParamsLabel(src).FontSizeAsText      ;
      TGIS_ParamsField.assignEx ( self,
                                  FFontSizeInternal,
                                  TGIS_ParamsLabel(src).FFontSizeInternal
                                ) ;
      FontStyle                := TGIS_ParamsLabel(src).FontStyle           ;
      FontColor                := TGIS_ParamsLabel(src).FontColor           ;
      TGIS_ParamsField.assignEx ( self,
                                  FFontColorInternal,
                                  TGIS_ParamsLabel(src).FFontColorInternal
                                ) ;
      FHeight                  := TGIS_ParamsLabel(src).Height              ;
      FHeightAsText            := TGIS_ParamsLabel(src).HeightAsText        ;
      FWidth                   := TGIS_ParamsLabel(src).Width               ;
      FWidthAsText             := TGIS_ParamsLabel(src).WidthAsText         ;

      FShield                  := TGIS_ParamsLabel(src).Shield              ;
    end ;
    inherited assignInternal(src) ;

  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsLabel.Assign(
  _source: TPersistent
) ;
var
  prm : TGIS_ParamsLabel ;
begin
  prm := TGIS_ParamsLabel( prepareParams ) ;
  prm.assignInternal(_source) ;
end ;

procedure TGIS_ParamsLabel.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  stmp  : String ;
  snull : String ;
begin
  with TGIS_Config( _cfg ) do begin
    oConfig := TGIS_Config( _cfg ) ;

    if config_old_format( _cfg ) then begin
      Visible                := ReadBoolean  ( GIS_INI_LABEL_VISIBLE,
                                               Visible
                                             ) ;
      Allocator              := ReadBoolean  ( GIS_INI_LABEL_ALLOCATOR,
                                               Allocator
                                             ) ;
      Duplicates             := ReadBoolean  ( GIS_INI_LABEL_DUPLICATES,
                                               Duplicates
                                             ) ;
      Field                  := ReadString   ( GIS_INI_LABEL_FIELD,
                                               Field
                                             ) ;
      Value                  := ReadString   ( GIS_INI_LABEL_VALUE,
                                               Value
                                             ) ;
      FontName               := ReadString   ( GIS_INI_LABEL_FONTNAME,
                                               FontName
                                             ) ;

      if ReadInteger( GIS_INI_LABEL_FONTSIZEPT, 0 ) <> 0 then
        // read "old"" fspecification only if something is inside
        FontSize              := TGIS_ParamsLabelFont.PtToFontSize(
                                               ReadInteger(
                                                 GIS_INI_LABEL_FONTSIZEPT,
                                                 TGIS_ParamsLabelFont.FontSizeToPt( FontSize )
                                               )
                                             ) ;

      FontSize               := ReadInteger  ( GIS_INI_LABEL_FONTSIZE,
                                               FontSize
                                             ) ;

      TGIS_ParamsField.readFromConfig        ( self,
                                               FFontSizeInternal,
                                               _cfg,
                                               GIS_INI_LABEL_FONTSIZEEX,
                                               FFontSizeInternal,
                                               FFontSizeAsText
                                             ) ;
      if ( FontSize <> GIS_RENDER_SIZE )
         and
         ( Abs( FontSize ) < GIS_AUTOSIZE_SIZE )
         and
         ( FontSize mod 20 = 0  ) then
      begin
        // interpret size in pt
        if ( assigned( FFontSizeInternal ) ) and
           not IsStringEmpty( FFontSizeInternal.Field )
        then begin
          if FFontSizeInternal.Factor < 0 then
            FontSizeAsText   := ConstructNumberAsText(
                                               FFontSizeInternal.Field,
                                               DotFloatToStr(-FFontSizeInternal.Factor),
                                               GIS_PARAMTXT_SIZE_PX
                                             )
          else
            FontSizeAsText   := ConstructNumberAsText(
                                               FFontSizeInternal.Field,
                                               DotFloatToStr(FFontSizeInternal.Factor),
                                               GIS_PARAMTXT_SIZE_TWIPS
                                             ) ;
        end
        else
          FontSizeAsText     := ConstructNumberAsText(
                                               '',
                                               IntToStr(FontSize div 20),
                                               GIS_PARAMTXT_SIZE_PT
                                             ) ;
      end ;

      FontStyle              := ReadFontStyle( GIS_INI_LABEL_FONTSTYLE,
                                               FontStyle
                                             ) ;
      FontColor              := ReadColor    ( GIS_INI_LABEL_FONTCOLOR,
                                               FontColor
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FFontColorInternal,
                                               _cfg,
                                               GIS_INI_LABEL_FONTCOLOREX,
                                               FFontColorInternal,
                                               snull
                                             ) ;
      Width                  := ReadInteger  ( GIS_INI_LABEL_WIDTH,
                                               Width
                                             ) ;
      Height                 := ReadInteger  ( GIS_INI_LABEL_HEIGHT,
                                              Height
                                             ) ;
      Position               := ReadPosition ( GIS_INI_LABEL_POSITION,
                                              Position
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FPositionInternal,
                                               _cfg,
                                               GIS_INI_LABEL_POSITIONEX,
                                               FPositionInternal,
                                               FPositionAsText
                                             ) ;
      Alignment              := ReadAlignment( GIS_INI_LABEL_ALIGNMENT,
                                               Alignment
                                             ) ;
      Rotate                 := ReadFloat    ( GIS_INI_LABEL_ROTATE,
                                               Rotate
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FRotateInternal,
                                               _cfg,
                                               GIS_INI_LABEL_ROTATEEX,
                                               FRotateInternal,
                                               FRotateAsText
                                             ) ;
      Color                  := ReadColor    ( GIS_INI_LABEL_COLOR,
                                               Color
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FColorInternal,
                                               _cfg,
                                               GIS_INI_LABEL_COLOREX,
                                               FColorInternal,
                                               snull
                                             ) ;
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_Bitmap,
                                               GIS_INI_LABEL_BITMAP,
                                               Bitmap
                                             ) ;
      Pattern                := ReadPattern  ( GIS_INI_LABEL_PATTERN,
                                               Pattern
                                             ) ;
      OutlineStyle           := ReadPen      ( GIS_INI_LABEL_OUTLINESTYLE,
                                               OutlineStyle
                                             ) ;
      OutlineWidth           := ReadInteger  ( GIS_INI_LABEL_OUTLINEWIDTH,
                                               OutlineWidth
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineWidthInternal,
                                               _cfg,
                                               GIS_INI_LABEL_OUTLINEWIDTHEX,
                                               FOutlineWidthInternal,
                                               FOutlineWidthAsText
                                             ) ;
      OffsetX                := ReadInteger  ( GIS_INI_LABEL_OFFSETX,
                                               OffsetX
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOffsetXInternal,
                                               _cfg,
                                               GIS_INI_LABEL_OFFSETXEX,
                                               FOffsetXInternal,
                                               FOffsetXAsText
                                             ) ;
      OffsetY                := ReadInteger  ( GIS_INI_LABEL_OFFSETY,
                                               OffsetY
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOffsetYInternal,
                                               _cfg,
                                               GIS_INI_LABEL_OFFSETYEX,
                                               FOffsetYInternal,
                                               FOffsetYAsText
                                             ) ;
      OffsetPosition         := ReadOffsetPosition( GIS_INI_LABEL_OFFSETPOSITION,
                                               OffsetPosition
                                             ) ;
      OutlineColor           := ReadColor    ( GIS_INI_LABEL_OUTLINECOLOR,
                                               OutlineColor
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FOutlineColorInternal,
                                               _cfg,
                                               GIS_INI_LABEL_OUTLINECOLOREX,
                                               FOutlineColorInternal,
                                               snull
                                             ) ;
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_OutlineBitmap,
                                               GIS_INI_LABEL_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      OutlinePattern         := ReadPattern  ( GIS_INI_LABEL_OUTLINEPATTERN,
                                               OutlinePattern
                                             ) ;
      SmartSize              := ReadInteger  ( GIS_INI_LABEL_SMARTSIZE,
                                               SmartSize
                                             ) ;

      if assigned( FSmartSizeInternal ) then
        stmp                 := ReadString   ( GIS_INI_LABEL_SMARTFIELD,
                                               FSmartSizeInternal.FFieldName
                                             )
      else
        stmp                 := ReadString   ( GIS_INI_LABEL_SMARTFIELD,
                                               ''
                                             ) ;

      FreeObject( FSmartSizeInternal ) ;
      if not IsStringEmpty( stmp ) then begin
        FSmartSizeInternal := TGIS_ParamsField.Create( self ) ;
        FSmartSizeInternal.Field    := stmp  ;
        FSmartSizeInternal.Scalable := false ;
        FSmartSizeInternal.Factor   := 1     ;
      end;

      ShowLegend             := ReadBoolean  ( GIS_INI_LABEL_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
    end ;

    if config_new_format( _cfg ) then begin
      Visible                := ReadBoolean  ( GIS_INI_LABEL_VISIBLE,
                                               Visible
                                             ) ;
      Allocator              := ReadBoolean  ( GIS_INI_LABEL_ALLOCATOR,
                                               Allocator
                                             ) ;
      Duplicates             := ReadBoolean  ( GIS_INI_LABEL_DUPLICATES,
                                               Duplicates
                                             ) ;
      Value                  := ReadString   ( GIS_INI_LABEL_VALUE,
                                               Value
                                             ) ;
      FontName               := ReadString   ( GIS_INI_LABEL_FONTNAME,
                                               FontName
                                             ) ;
      FontSizeAsText         := ReadString   ( GIS_INI_LABEL_FONTSIZE,
                                               FontSizeAsText
                                             ) ;
      FontStyle              := ReadFontStyle( GIS_INI_LABEL_FONTSTYLE,
                                               FontStyle
                                             ) ;
      FontColorAsText        := ReadString   ( GIS_INI_LABEL_FONTCOLOR,
                                               FontColorAsText
                                             ) ;
      WidthAsText            := ReadString   ( GIS_INI_LABEL_WIDTH,
                                               WidthAsText
                                             ) ;
      HeightAsText           := ReadString   ( GIS_INI_LABEL_HEIGHT,
                                              HeightAsText
                                             ) ;
      PositionAsText         := ReadString   ( GIS_INI_LABEL_POSITION,
                                               PositionAsText
                                             ) ;
      Alignment              := ReadAlignment( GIS_INI_LABEL_ALIGNMENT,
                                               Alignment
                                             ) ;
      RotateAsText           := ReadString   ( GIS_INI_LABEL_ROTATE,
                                               RotateAsText
                                             ) ;
      ColorAsText            := ReadString   ( GIS_INI_LABEL_COLOR,
                                               ColorAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_Bitmap,
                                               GIS_INI_LABEL_BITMAP,
                                               Bitmap
                                             ) ;
      PatternAsText          := ReadString   ( GIS_INI_LABEL_PATTERN,
                                               PatternAsText
                                             ) ;

      ShieldAsText           := ReadString   ( GIS_INI_LABEL_SHIELD,
                                               ShieldAsText
                                             ) ;
      OutlineStyleAsText     := ReadString   ( GIS_INI_LABEL_OUTLINESTYLE,
                                               OutlineStyleAsText
                                             ) ;
      OutlineWidthAsText     := ReadString   ( GIS_INI_LABEL_OUTLINEWIDTH,
                                               OutlineWidthAsText
                                             ) ;
      OffsetXAsText          := ReadString   ( GIS_INI_LABEL_OFFSETX,
                                               OffsetXAsText
                                             ) ;
      OffsetYAsText          := ReadString   ( GIS_INI_LABEL_OFFSETY,
                                               OffsetYAsText
                                             ) ;
      OffsetPosition         := ReadOffsetPosition( GIS_INI_LABEL_OFFSETPOSITION,
                                               OffsetPosition
                                             ) ;
      OutlineColorAsText     := ReadString   ( GIS_INI_LABEL_OUTLINECOLOR,
                                               OutlineColorAsText
                                             ) ;
      { TODO -cObsolate : not used after 11.56 }
      set_bitmap_property                    ( _cfg,
                                               {$IFNDEF DCC}@{$ENDIF}fset_OutlineBitmap,
                                               GIS_INI_LABEL_OUTLINEBITMAP,
                                               OutlineBitmap
                                             ) ;
      OutlinePatternAsText   := ReadString   ( GIS_INI_LABEL_OUTLINEPATTERN,
                                               OutlinePatternAsText
                                             ) ;
      SmartSizeAsText        := ReadString   ( GIS_INI_LABEL_SMARTSIZE,
                                               SmartSizeAsText
                                             ) ;
      ShowLegend             := ReadBoolean  ( GIS_INI_LABEL_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
    end ;

    oConfig := nil ;
  end ;
end ;

procedure TGIS_ParamsLabel.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  def : TGIS_ParamsLabel ;
begin
  if TGIS_ParamsSectionVector(GetRoot).Labels = self then
    def := TGIS_ParamsLabel.Create
  else
    def := TGIS_ParamsSectionVector(GetRoot).Labels ;
  try
    with TGIS_Config( _cfg ) do begin

      if config_old_format( _cfg ) then begin
        WriteBoolean                         ( GIS_INI_LABEL_VISIBLE,
                                               Visible,
                                               def.Visible
                                             ) ;
        WriteBoolean                         ( GIS_INI_LABEL_ALLOCATOR,
                                               Allocator,
                                               def.Allocator
                                             ) ;
        WriteBoolean                         ( GIS_INI_LABEL_DUPLICATES,
                                               Duplicates,
                                               def.Duplicates
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_FIELD,
                                               Field,
                                               def.Field
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_VALUE,
                                               Value,
                                               def.Value
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_FONTNAME,
                                               FontName,
                                               def.FontName
                                             ) ;
        WriteInteger                         ( GIS_INI_LABEL_FONTSIZEPT,
                                               TGIS_ParamsLabelFont.FontSizeToPt(FontSize),
                                               def.FontSize
                                             ) ;
        WriteInteger                         ( GIS_INI_LABEL_FONTSIZE,
                                               FontSize,
                                               def.FontSize
                                             ) ;
        WriteFontStyle                       ( GIS_INI_LABEL_FONTSTYLE,
                                               FontStyle,
                                               def.FontStyle
                                             ) ;
        WriteColor                           ( GIS_INI_LABEL_FONTCOLOR,
                                               FontColor,
                                               def.FontColor
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LABEL_FONTCOLOREX,
                                               FFontColorInternal
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LABEL_FONTSIZEEX,
                                               FFontSizeInternal
                                             ) ;

        WriteInteger                         ( GIS_INI_LABEL_WIDTH,
                                               make_size_DK10( Width ),
                                               def.Width
                                             ) ;
        WriteInteger                         ( GIS_INI_LABEL_HEIGHT,
                                               make_size_DK10( Height ),
                                               def.Height
                                             ) ;
        WritePosition                        ( GIS_INI_LABEL_POSITION,
                                               Position,
                                               def.Position
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LABEL_POSITIONEX,
                                               FPositionInternal
                                             ) ;
        WriteAlignment                       ( GIS_INI_LABEL_ALIGNMENT,
                                               Alignment,
                                               def.Alignment
                                             ) ;
        WriteFloat                           ( GIS_INI_LABEL_ROTATE,
                                               Rotate,
                                               def.Rotate
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LABEL_ROTATEEX,
                                               FRotateInternal
                                             ) ;
        WriteColor                           ( GIS_INI_LABEL_COLOR,
                                               Color,
                                               def.Color
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LABEL_COLOREX,
                                               FColorInternal
                                             ) ;
        WriteBitmap                          ( GIS_INI_LABEL_BITMAP,
                                               Bitmap,
                                               def.Bitmap
                                             ) ;
        WritePattern                         ( GIS_INI_LABEL_PATTERN,
                                               Pattern,
                                               def.Pattern
                                             ) ;
        WritePen                             ( GIS_INI_LABEL_OUTLINESTYLE,
                                               OutlineStyle,
                                               def.OutlineStyle
                                             ) ;
        WriteInteger                         ( GIS_INI_LABEL_OUTLINEWIDTH,
                                               make_size_DK10( OutlineWidth ),
                                               def.OutlineWidth
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LABEL_OUTLINEWIDTHEX,
                                               FOutlineWidthInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_LABEL_OFFSETX,
                                               make_size_DK10( OffsetX ),
                                               def.OffsetX
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LABEL_OFFSETXEX,
                                               FOffsetXInternal
                                             ) ;
        WriteInteger                         ( GIS_INI_LABEL_OFFSETY,
                                               make_size_DK10( OffsetY ),
                                               def.OffsetY
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LABEL_OFFSETYEX,
                                               FOffsetYInternal
                                             ) ;
        WriteOffsetPosition                  ( GIS_INI_LABEL_OFFSETPOSITION,
                                               OffsetPosition,
                                               def.OffsetPosition
                                             ) ;
        WriteColor                           ( GIS_INI_LABEL_OUTLINECOLOR,
                                               OutlineColor,
                                               def.OutlineColor
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_LABEL_OUTLINECOLOREX,
                                               FOutlineColorInternal
                                             ) ;
        WriteBitmap                          ( GIS_INI_LABEL_OUTLINEBITMAP,
                                               OutlineBitmap,
                                               def.OutlineBitmap
                                             ) ;
        WritePattern                         ( GIS_INI_LABEL_OUTLINEPATTERN,
                                               OutlinePattern,
                                               def.OutlinePattern
                                             ) ;
        WriteInteger                         ( GIS_INI_LABEL_SMARTSIZE,
                                               make_size_DK10( SmartSize ),
                                               def.SmartSize
                                             ) ;
        if assigned( FSmartSizeInternal          ) and
           ( FSmartSizeInternal.Scalable = False ) and
           ( FSmartSizeInternal.Factor   = 1     )
        then
          WriteString                        ( GIS_INI_LABEL_SMARTFIELD,
                                               FSmartSizeInternal.Field,
                                               ''
                                             ) ;
        WriteBoolean                         ( GIS_INI_LABEL_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;

      if config_new_format( _cfg ) then begin
        WriteBoolean                         ( GIS_INI_LABEL_VISIBLE,
                                               Visible,
                                               def.Visible
                                             ) ;
        WriteBoolean                         ( GIS_INI_LABEL_ALLOCATOR,
                                               Allocator,
                                               def.Allocator
                                             ) ;
        WriteBoolean                         ( GIS_INI_LABEL_DUPLICATES,
                                               Duplicates,
                                               def.Duplicates
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_VALUE,
                                               Value,
                                               def.Value
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_FONTNAME,
                                               FontName,
                                               def.FontName
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_FONTSIZE,
                                               FontSizeAsText,
                                               def.FontSizeAsText
                                             ) ;
        WriteFontStyle                       ( GIS_INI_LABEL_FONTSTYLE,
                                               FontStyle,
                                               def.FontStyle
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_FONTCOLOR,
                                               FontColorAsText,
                                               def.FontColorAsText
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_WIDTH,
                                               WidthAsText,
                                               def.WidthAsText
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_HEIGHT,
                                               HeightAsText,
                                               def.HeightAsText
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_POSITION,
                                               PositionAsText,
                                               def.PositionAsText
                                             ) ;
        WriteAlignment                       ( GIS_INI_LABEL_ALIGNMENT,
                                               Alignment,
                                               def.Alignment
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_ROTATE,
                                               RotateAsText,
                                               def.RotateAsText
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_COLOR,
                                               ColorAsText,
                                               def.ColorAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_LABEL_PATTERN,
                                               PatternAsText,
                                               def.PatternAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_LABEL_SHIELD,
                                               ShieldAsText,
                                               def.ShieldAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_LABEL_OUTLINESTYLE,
                                               OutlineStyleAsText,
                                               def.OutlineStyleAsText
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_OUTLINEWIDTH,
                                               OutlineWidthAsText,
                                               def.OutlineWidthAsText
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_OFFSETX,
                                               OffsetXAsText,
                                               def.OffsetXAsText
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_OFFSETY,
                                               OffsetYAsText,
                                               def.OffsetYAsText
                                             ) ;
        WriteOffsetPosition                  ( GIS_INI_LABEL_OFFSETPOSITION,
                                               OffsetPosition,
                                               def.OffsetPosition
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_OUTLINECOLOR,
                                               OutlineColorAsText,
                                               def.OutlineColorAsText
                                             ) ;
        WriteStyle                           ( GIS_INI_LABEL_OUTLINEPATTERN,
                                               OutlinePatternAsText,
                                               def.OutlinePatternAsText
                                             ) ;
        WriteString                          ( GIS_INI_LABEL_SMARTSIZE,
                                               SmartSizeAsText,
                                               def.SmartSizeAsText
                                             ) ;
        WriteBoolean                         ( GIS_INI_LABEL_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;


    end ;
  finally
    if TGIS_ParamsSectionVector(GetRoot).Labels = self then
      FreeObject( def ) ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsChart' }

constructor TGIS_ParamsChart.Create ;
begin
  inherited ;

  FStyle                := TGIS_ChartStyle.Pie ;
  FSize                 := 250 ;
  FLegend               := '' ;
  FOutlineStyle         := TGIS_PenStyle.Clear ;
  FOutlineWidthAsText   := '' ;
  FOutlineStyleAsText   := '' ;
  FOffsetX              := 0 ;
  FOffsetXAsText        := '' ;
  FOffsetY              := 0 ;
  FOffsetYAsText        := '' ;
  FOffsetPosition       := TGIS_OffsetPosition.DownRight ;
  SetLength( ColorsInternal, 9 ) ;

  ColorsInternal[ 0 ] := TGIS_Color.Red     ;
  ColorsInternal[ 1 ] := TGIS_Color.Lime    ;
  ColorsInternal[ 2 ] := TGIS_Color.Blue    ;
  ColorsInternal[ 3 ] := TGIS_Color.Fuchsia ;
  ColorsInternal[ 4 ] := TGIS_Color.Aqua    ;
  ColorsInternal[ 5 ] := TGIS_Color.Green   ;
  ColorsInternal[ 6 ] := TGIS_Color.White   ;
  ColorsInternal[ 7 ] := TGIS_Color.Black   ;
  ColorsInternal[ 8 ] := TGIS_Color.None    ;
end ;

procedure TGIS_ParamsChart.fset_Style(
  const _value : TGIS_ChartStyle
) ;
var
  prm : TGIS_ParamsChart ;
begin
  prm := TGIS_ParamsChart( prepareParams ) ;
  with prm do begin
    if FStyle = _value then exit ;
    FStyle := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsChart.fset_Size(
  const _value : Integer
) ;
var
  prm : TGIS_ParamsChart ;
begin
  prm := TGIS_ParamsChart( prepareParams ) ;
  with prm do begin
    if FSize = _value then exit ;
    FSize := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsChart.fget_Values
  : String ;
var
  i : Integer ;
begin
  Result := '' ;
  for i:=0 to high( ValuesInternal ) do begin
    if i > 0 then
      Result := Result + ':' ;
    Result := Result + DotFloatToStr( ValuesInternal[i] ) ;
  end ;
end ;

procedure TGIS_ParamsChart.fset_Values(
  const _value : String
) ;
var
  i    : Integer  ;
  ipos : Integer  ;
  item : String   ;
  c    : Char     ;
  any  : Boolean  ;
  btmp : Boolean  ;
  prm : TGIS_ParamsChart ;

  function parse : Boolean ;
  begin
    Result := False ;

    SetLength( prm.ValuesInternal, ipos + 1 ) ;
    if IsStringEmpty( item ) then
      prm.ValuesInternal[ipos] := GIS_MAX_DOUBLE
    else
      try
        prm.ValuesInternal[ipos] := DotStrToFloat( item ) ;
        if ipos >= 2 then
          Result := True ;
      except
        prm.ValuesInternal[ipos] := 0 ;
      end ;

    item := '' ;
    inc( ipos ) ;
  end ;

begin
  prm := TGIS_ParamsChart( prepareParams ) ;
  with prm do begin
    any := False ;

    if Values = _value then exit ;
    ipos := 0  ;
    item := '' ;
    for i := StringFirst to StringLast( _value ) do begin
      c := _value[i] ;
      if c = ':' then begin
        btmp := parse ;
        any  := any or btmp ;
        continue ;
      end
      else
       item := item + c ;
    end ;
    if length( item ) > 0 then begin
      btmp := parse ;
      any  := any or btmp ;
    end ;

    if not any then
      SetLength( prm.ValuesInternal, 0 ) ;
    Touch ;
  end ;
end ;

function TGIS_ParamsChart.fget_Colors
  : String ;
var
  i : Integer ;
begin
  Result := '' ;
  for i:=0 to high( ColorsInternal ) do begin
    if i > 0 then
      Result := Result + ',' ;
    Result := Result + ConstructParamColor(ColorsInternal[i]) ;
  end ;
end ;


procedure TGIS_ParamsChart.fset_Colors(
  const _value : String
) ;
var
  i    : Integer  ;
  ipos : Integer  ;
  item : String   ;
  c    : Char     ;
  any  : Boolean  ;
  btmp : Boolean  ;
  prm : TGIS_ParamsChart ;

  function parse : Boolean ;
  begin
    Result := False ;
    if IsStringEmpty( item ) then
      prm.ColorsInternal[ipos] := TGIS_Color.Black
    else
      try
        prm.ColorsInternal[ipos] :=  ParamColor( item, TGIS_Color.Black ) ; //hexStrToColor( item ) ;
        if ipos >= 2 then
          Result := True ;
      except
        prm.ColorsInternal[ipos] := TGIS_Color.Black ;
      end ;

    item := '' ;
    inc( ipos ) ;
  end ;

begin
  prm := TGIS_ParamsChart( prepareParams ) ;
  with prm do begin
    any := False ;

    if Values = _value then exit ;
    ipos := 0  ;
    item := '' ;
    for i := StringFirst to StringLast( _value ) do begin
      c := _value[i] ;
      if c = ',' then begin
        btmp := parse ;
        any  := any or btmp ;
        continue ;
      end
      else
       item := item + c ;
    end ;
    if length( item ) > 0 then begin
      btmp := parse ;
      any  := any or btmp ;
    end ;

//    if not any then
//      SetLength( prm.ColorsInternal, 0 ) ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsChart.fset_Legend(
  const _value : String
) ;
var
  i    : Integer  ;
  ipos : Integer  ;
  item : String   ;
  c    : Char     ;
  any  : Boolean  ;
  btmp : Boolean  ;

  function parse( _prm : TGIS_ParamsChart ) : Boolean ;
  begin
    with _prm do begin
      Result := False ;

      if not IsStringEmpty( item ) then begin
        FLegend := FLegend + item ;
        if ipos >= 2 then
          Result := True ;
      end ;
      item := '' ;
      inc( ipos ) ;
    end ;
  end ;

var
  prm : TGIS_ParamsChart ;
begin
  prm := TGIS_ParamsChart( prepareParams ) ;
  with prm do begin
    any := False ;

    if Values = _value then exit ;

    FLegend := '' ;
    ipos    := 0  ;
    item    := '' ;
    for i := StringFirst to StringLast( _value ) do begin
      c := _value[i] ;
      if c = ':' then begin
        btmp := parse( prm ) ;
        FLegend := FLegend + ':' ;
        any  := any or btmp ;
        continue ;
      end
      else
       item := item + c ;
    end ;
    if length( item ) > 0 then begin
      btmp := parse( prm ) ;
      any  := any or btmp ;
    end ;

    if not any then
      FLegend := '' ;
    Touch ;
  end ;
end ;

function TGIS_ParamsChart.fget_SizeAsText
  : String ;
begin
  Result := size_to_text( FSizeAsText, Size, nil ) ;
end ;

procedure TGIS_ParamsChart.fset_SizeAsText(
  const _value : String
);
var
  prm        : TGIS_ParamsChart ;
  itmp_size  : Integer ;
begin
  itmp_size := Size ;

  try
    size_from_text( self, _value, itmp_size );

    Size := itmp_size ;

    prm := TGIS_ParamsChart( prepareParams ) ;
    with prm do begin
      if FSizeAsText <> _value then begin
        FSizeAsText := _value ;
        Touch ;
      end;
    end ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_CHART_SIZE,
      '"' + _value + '"'
    );
  end;
end ;

function TGIS_ParamsChart.prepareParams
  : TGIS_ParamsVector ;
var
  params : TGIS_ParamsSectionVector ;
begin
  if assigned( FParentProxy ) and
     assigned( TGIS_ParamsSectionVector(FParentProxy).FShape ) then
  begin
    params := prepareParamsVector ;
    if not assigned( params.FChart ) then begin
      params.FChart := TGIS_ParamsChart( CreateCopy ) ;
      params.FChart.Parent := params ;
    end ;
    Result := params.FChart ;
  end
  else
    Result := self ;
end ;

procedure TGIS_ParamsChart.assignInternal(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsChart.Create
  else
    src := _source ;
  try

    if src is TGIS_ParamsChart then begin
      FStyle         := TGIS_ParamsChart(src).Style           ;
      FSize          := TGIS_ParamsChart(src).Size            ;
      FSizeAsText    := TGIS_ParamsChart(src).SizeAsText      ;
      ValuesInternal := TGIS_ParamsChart(src).ValuesInternal  ;
      ColorsInternal := TGIS_ParamsChart(src).ColorsInternal  ;
      FLegend        := TGIS_ParamsChart(src).Legend          ;
    end ;
    inherited assignInternal(src) ;

  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsChart.Assign(
  _source: TPersistent
) ;
var
  prm : TGIS_ParamsChart ;
begin
  prm := TGIS_ParamsChart( prepareParams ) ;
  prm.assignInternal(_source) ;
end ;

procedure TGIS_ParamsChart.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  with TGIS_Config( _cfg ) do begin
    oConfig := TGIS_Config( _cfg ) ;

    if config_old_format( _cfg ) then begin
      Style                  := ReadChart    ( GIS_INI_CHART_STYLE,
                                               Style
                                             ) ;
      Size                   := ReadInteger  ( GIS_INI_CHART_SIZE,
                                               Size
                                             ) ;
      Values                 := ReadString   ( GIS_INI_CHART_VALUES,
                                               Values
                                             ) ;
      Colors                 := ReadString   ( GIS_INI_CHART_COLORS,
                                               Colors
                                             ) ;
      Legend                 := ReadString   ( GIS_INI_CHART_LEGEND,
                                               Legend
                                             ) ;
      ShowLegend             := ReadBoolean  ( GIS_INI_CHART_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
    end ;

    if config_new_format( _cfg ) then begin
      Style                  := ReadChart    ( GIS_INI_CHART_STYLE,
                                               Style
                                             ) ;
      SizeAsText             := ReadString   ( GIS_INI_CHART_SIZE,
                                               SizeAsText
                                             ) ;
      Values                 := ReadString   ( GIS_INI_CHART_VALUES,
                                               Values
                                             ) ;
      Colors                 := ReadString   ( GIS_INI_CHART_COLORS,
                                               Colors
                                             ) ;
      Legend                 := ReadString   ( GIS_INI_CHART_LEGEND,
                                               Legend
                                             ) ;
      ShowLegend             := ReadBoolean  ( GIS_INI_CHART_SHOWLEGEND,
                                               ShowLegend
                                             ) ;
    end ;

    oConfig := nil ;
  end ;
end ;

procedure TGIS_ParamsChart.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  def : TGIS_ParamsChart;
begin
  if TGIS_ParamsSectionVector(GetRoot).Chart = self then
    def := TGIS_ParamsChart.Create
  else
    def := TGIS_ParamsSectionVector(GetRoot).Chart ;
  try
    with TGIS_Config( _cfg ) do begin

      if config_old_format( _cfg ) then begin
        WriteChart                           ( GIS_INI_CHART_STYLE,
                                               Style,
                                               def.Style
                                             ) ;
        WriteInteger                         ( GIS_INI_CHART_SIZE,
                                               make_size_DK10( Size ),
                                               def.Size
                                             ) ;
        WriteString                          ( GIS_INI_CHART_VALUES,
                                               Values,
                                               def.Values
                                             ) ;
        WriteString                          ( GIS_INI_CHART_COLORS,
                                               Colors,
                                               def.Colors
                                             ) ;
        WriteString                          ( GIS_INI_CHART_LEGEND,
                                               Legend,
                                               def.Legend
                                             ) ;
        WriteBoolean                         ( GIS_INI_CHART_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;

      if config_new_format( _cfg ) then begin
        WriteChart                           ( GIS_INI_CHART_STYLE,
                                               Style,
                                               def.Style
                                             ) ;
        WriteString                          ( GIS_INI_CHART_SIZE,
                                               SizeAsText,
                                               def.SizeAsText
                                             ) ;
        WriteString                          ( GIS_INI_CHART_VALUES,
                                               Values,
                                               def.Values
                                             ) ;
        WriteString                          ( GIS_INI_CHART_COLORS,
                                               Colors,
                                               def.Colors
                                             ) ;
        WriteString                          ( GIS_INI_CHART_LEGEND,
                                               Legend,
                                               def.Legend
                                             ) ;
        WriteBoolean                         ( GIS_INI_CHART_SHOWLEGEND,
                                               ShowLegend,
                                               def.ShowLegend
                                             ) ;
      end;

    end ;
  finally
    if TGIS_ParamsSectionVector(GetRoot).Chart = self then
      FreeObject( def ) ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsSection'}

constructor TGIS_ParamsSection.Create ;
begin
  inherited ;
  FShape          := nil             ;
  FVisible        := True            ;
  FMinLevel       := -GIS_MAX_DOUBLE ;              ;
  FMaxLevel       := GIS_MAX_DOUBLE  ;
  FMinScale       := 0               ;
  FMaxScale       := GIS_MAX_DOUBLE  ;
  FMinZoom        := 0               ;
  FMaxZoom        := GIS_MAX_DOUBLE  ;
  FLegend         := ''              ;
  FScaleZ         := 1 ;
  FNormalizedZ    := TGIS_3DNormalizationType.Off ;
  FFalseZ         := 0 ;
  FFalseZAsText   := '' ;
end ;

procedure TGIS_ParamsSection.doDestroy ;
begin
  FreeObject( FFalseZInternal ) ;

  inherited ;
end ;

procedure TGIS_ParamsSection.fset_Style(
  const _value : String
) ;
begin
  if FStyle = _value then exit ;
  FStyle := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_Visible(
  const _value : Boolean
) ;
begin
  if FVisible = _value then exit ;
  FVisible := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_MinLevel(
  const _value : Double
) ;
begin
  if GisIsSameValue( FMinLevel, _value ) then exit ;
  FMinLevel := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_MaxLevel(
  const _value : Double
) ;
begin
  if GisIsSameValue( FMaxLevel, _value ) then exit ;
  FMaxLevel := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_MinScale(
  const _value : Double
) ;
begin
  FMinLevel := -GIS_MAX_DOUBLE ;
  FMaxLevel := GIS_MAX_DOUBLE ;

  if GisIsSameValue( FMinScale, _value ) then exit ;
  FMinScale := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_MaxScale(
  const _value : Double
) ;
begin
  FMinLevel := -GIS_MAX_DOUBLE ;
  FMaxLevel := GIS_MAX_DOUBLE ;

  if GisIsSameValue( FMaxScale, _value ) then exit ;
  FMaxScale := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_MinZoom(
  const _value : Double
) ;
begin
  FMinLevel := -GIS_MAX_DOUBLE ;
  FMaxLevel := GIS_MAX_DOUBLE ;

  if GisIsSameValue( FMinZoom, _value ) then exit ;
  FMinZoom := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_MaxZoom(
  const _value : Double
) ;
begin
  FMinLevel := -GIS_MAX_DOUBLE ;
  FMaxLevel := GIS_MAX_DOUBLE ;

  if GisIsSameValue( FMaxZoom, _value ) then exit ;
  FMaxZoom := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_Legend(
  const _value : String
) ;
begin
  if FLegend = _value then exit ;
  FLegend := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_NormalizedZ(
  const _value : TGIS_3DNormalizationType
) ;
begin
  if _value = FNormalizedZ then exit ;
  FNormalizedZ := _value ;
  Touch ;
end ;

procedure TGIS_ParamsSection.fset_ScaleZ(
  const _value : Double
) ;
begin
  if _value = FScaleZ then exit ;
  FScaleZ := _value ;
  Touch ;
end ;

function TGIS_ParamsSection.fget_FalseZ
  : Double ;
begin
  Result := compute_measure( Shape, FFalseZ, FFalseZInternal ) ;
end ;

procedure TGIS_ParamsSection.fset_FalseZ(
  const _value : Double
) ;
begin
  if _value = FFalseZ then exit ;
  FFalseZ := _value ;
  FFalseZAsText := '' ;
  Touch ;
end ;

function TGIS_ParamsSection.fget_FalseZAsText
  : String ;
begin
  Result := measure_to_text( FFalseZAsText, FalseZ, FFalseZInternal ) ;
end;

procedure TGIS_ParamsSection.fset_FalseZAsText(
  const _value : String
) ;
var
  dtmp_size  : Double ;
begin
  dtmp_size := FalseZ ;

  try
    measure_from_text( self, _value, dtmp_size, FFalseZInternal );

    FalseZ := dtmp_size ;
    FFalseZAsText := _value ;
  except
    TGIS_Logger.AsWarning(
      'Bad ' + GIS_INI_FALSEZ,
      '"' + _value + '"'
    );
  end;
end;

procedure TGIS_ParamsSection.Assign(
  _source: TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsSection.Create
  else
    src := _source ;
  try

  if src is TGIS_ParamsSection then begin
    FStyle                := TGIS_ParamsSection(src).Style        ;
    FVisible              := TGIS_ParamsSection(src).Visible      ;
    FMinLevel             := TGIS_ParamsSection(src).MinLevel     ;
    FMaxLevel             := TGIS_ParamsSection(src).MaxLevel     ;
    FMinScale             := TGIS_ParamsSection(src).MinScale     ;
    FMaxScale             := TGIS_ParamsSection(src).MaxScale     ;
    FMinZoom              := TGIS_ParamsSection(src).MinZoom      ;
    FMaxZoom              := TGIS_ParamsSection(src).MaxZoom      ;
    FLegend               := TGIS_ParamsSection(src).Legend       ;
    FNormalizedZ          := TGIS_ParamsSection(src).NormalizedZ  ;
    FScaleZ               := TGIS_ParamsSection(src).ScaleZ       ;
    FFalseZ               := TGIS_ParamsSection(src).FalseZ       ;
    FFalseZAsText         := TGIS_ParamsSection(src).FalseZAsText ;
    TGIS_ParamsField.assignEx(
                             self,
                             FFalseZInternal,
                             TGIS_ParamsSection(src).FFalseZInternal
                           ) ;
  end ;
  inherited Assign( src ) ;

  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsSection.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  tmp : Double ;
begin
  with TGIS_Config( _cfg ) do begin
    oConfig := TGIS_Config( _cfg ) ;

    if config_any_format( _cfg ) then begin
      Style                  := ReadString   ( GIS_INI_STYLE,
                                               Style
                                             ) ;
      Visible                := ReadBoolean  ( GIS_INI_VISIBLE,
                                               Visible
                                             ) ;
      MinLevel               := ReadFloat    ( GIS_INI_MINLEVEL,
                                               MinLevel
                                             ) ;
      MaxLevel               := ReadFloat    ( GIS_INI_MAXLEVEL,
                                               MaxLevel
                                             ) ;
      MinZoom                := ReadFloat    ( GIS_INI_MINZOOM,
                                               MinZoom
                                             ) ;
      MaxZoom                := ReadFloat    ( GIS_INI_MAXZOOM,
                                               MaxZoom
                                             ) ;
      MinScale               := ReadFloat    ( GIS_INI_MINSCALE,
                                               MinScale
                                             ) ;
      MaxScale               := ReadFloat    ( GIS_INI_MAXSCALE,
                                               MaxScale
                                             ) ;
      Legend                 := ReadString   ( GIS_INI_LEGEND,
                                               Legend
                                             ) ;
    end;

    if config_old_format( _cfg ) then begin
      NormalizedZ            := ReadNormalized
                                             ( GIS_INI_NORMALIZEDZ,
                                               NormalizedZ
                                             ) ;
      ScaleZ                 := ReadFloat    ( GIS_INI_SCALEZ,
                                               ScaleZ
                                             ) ;
      FalseZ                 := ReadFloat    ( GIS_INI_FALSEZ,
                                               FalseZ
                                             ) ;
      TGIS_ParamsField.readFromConfig        ( self,
                                               FFalseZInternal,
                                               _cfg,
                                               GIS_INI_FALSEZEX,
                                               FFalseZInternal,
                                               FFalseZAsText
                                             ) ;
    end;

    if config_new_format( _cfg ) then begin
      NormalizedZ            := ReadNormalized
                                             ( GIS_INI_NORMALIZEDZ,
                                               NormalizedZ
                                             ) ;
      ScaleZ                 := ReadFloat    ( GIS_INI_SCALEZ,
                                               ScaleZ
                                             ) ;
      FalseZAsText           := ReadString   ( GIS_INI_FALSEZ,
                                               FalseZAsText
                                             ) ;
    end ;

    oConfig := nil ;
  end ;

  if MaxScale > 1e30 then MaxScale := GIS_MAX_DOUBLE ;

  // fix for badly assigned MinZoom/MaxZoom
  if Abs(MinZoom) > Abs(MaxZoom) then begin
    tmp := MinZoom ;
    MinZoom := MaxZoom ;
    MaxZoom := tmp ;
  end ;
  if Abs(MinScale) > Abs(MaxScale) then begin
    tmp := MinScale ;
    MinScale := MaxScale ;
    MaxScale := tmp ;
  end ;
  if MinLevel > MaxLevel then begin
    tmp := MinLevel ;
    MinLevel := MaxLevel ;
    MaxLevel := tmp ;
  end ;
end ;

procedure TGIS_ParamsSection.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  def             : TGIS_ParamsSection ;
  blevel          : Boolean ;
  bscale          : Boolean ;
  bzoom           : Boolean ;
begin
  blevel := ( MinLevel > -GIS_HALF_MAX_DOUBLE ) or ( MaxLevel < GIS_HALF_MAX_DOUBLE ) ;
  bscale := ( MinScale <> 0 ) or ( MaxScale < GIS_HALF_MAX_DOUBLE ) ;
  bzoom  := ( MinZoom  <> 0 ) or ( MaxZoom  < GIS_HALF_MAX_DOUBLE ) ;

  if ( not assigned( FParamsList ) ) or
     ( TGIS_ParamsSection(FParamsList[0]) = self )
  then
    def := TGIS_ParamsSection.Create
  else
    def := TGIS_ParamsSection(FParamsList[0]) ;
  try
    with TGIS_Config( _cfg ) do begin

      if config_any_format( _cfg ) then begin
        WriteString                          ( GIS_INI_STYLE,
                                               Style,
                                               def.Style
                                             ) ;
        // force writing of Active property
        if Visible then
          WriteBoolean                       ( GIS_INI_VISIBLE,
                                               Visible,
                                               False
                                             )
        else
          WriteBoolean                       ( GIS_INI_VISIBLE,
                                               Visible,
                                               True
                                             ) ;

        // delete default value
        WriteFloat                           ( GIS_INI_MINLEVEL,
                                               -GIS_MAX_DOUBLE,
                                               -GIS_MAX_DOUBLE
                                             ) ;
        WriteFloat                           ( GIS_INI_MAXLEVEL,
                                               GIS_MAX_DOUBLE,
                                               GIS_MAX_DOUBLE
                                             ) ;
        WriteFloat                           ( GIS_INI_MINSCALE,
                                               0,
                                               0
                                             ) ;
        WriteFloat                           ( GIS_INI_MAXSCALE,
                                               GIS_MAX_DOUBLE,
                                               GIS_MAX_DOUBLE
                                             ) ;
        WriteFloat                           ( GIS_INI_MINZOOM,
                                               0,
                                               0
                                             ) ;
        WriteFloat                           ( GIS_INI_MAXZOOM,
                                               GIS_MAX_DOUBLE,
                                               GIS_MAX_DOUBLE
                                             ) ;

        if blevel then begin
          WriteFloat                         ( GIS_INI_MINLEVEL,
                                               MinLevel,
                                               -128
                                             ) ;
          WriteFloat                         ( GIS_INI_MAXLEVEL,
                                               MaxLevel,
                                               128
                                             ) ;
        end
        else if bscale then begin
          WriteFloat                         ( GIS_INI_MINSCALE,
                                               MinScale,
                                               -1
                                             ) ;
          WriteFloat                         ( GIS_INI_MAXSCALE,
                                               MaxScale,
                                               -1
                                             ) ;
        end
        else if bzoom then begin
          WriteFloat                         ( GIS_INI_MINLEVEL,
                                               -GIS_MAX_DOUBLE,
                                               -1
                                             ) ;
          WriteFloat                         ( GIS_INI_MAXLEVEL,
                                               GIS_MAX_DOUBLE,
                                               -1
                                             ) ;
          WriteFloat                         ( GIS_INI_MINSCALE,
                                               0,
                                               -1
                                             ) ;
          WriteFloat                         ( GIS_INI_MAXSCALE,
                                               GIS_MAX_DOUBLE,
                                               -1
                                             ) ;
          WriteFloat                         ( GIS_INI_MINZOOM,
                                               MinZoom,
                                               -1
                                             ) ;
          WriteFloat                         ( GIS_INI_MAXZOOM,
                                               MaxZoom,
                                               -1
                                             ) ;
        end ;
      end ;

      if config_old_format( _cfg ) then begin
        WriteString                          ( GIS_INI_LEGEND,
                                               Legend,
                                               def.Legend
                                             ) ;
        WriteNormalized                      ( GIS_INI_NORMALIZEDZ,
                                               NormalizedZ,
                                               def.NormalizedZ
                                             ) ;
        WriteFloat                           ( GIS_INI_SCALEZ,
                                               ScaleZ,
                                               def.ScaleZ
                                             ) ;
        WriteFloat                           ( GIS_INI_FALSEZ,
                                               FalseZ,
                                               def.FalseZ
                                             ) ;
        TGIS_ParamsField.writeToConfig       ( _cfg,
                                               GIS_INI_FALSEZEX,
                                               FFalseZInternal
                                             ) ;
      end ;

      if config_new_format( _cfg ) then begin
        WriteString                          ( GIS_INI_LEGEND,
                                               Legend,
                                               def.Legend
                                             ) ;
        WriteNormalized                      ( GIS_INI_NORMALIZEDZ,
                                               NormalizedZ,
                                               def.NormalizedZ
                                             ) ;
        WriteFloat                           ( GIS_INI_SCALEZ,
                                               ScaleZ,
                                               def.ScaleZ
                                             ) ;
        WriteString                          ( GIS_INI_FALSEZ,
                                               FalseZAsText,
                                               def.FalseZAsText
                                             ) ;
      end ;

    end;
  finally
    if ( not assigned( FParamsList ) ) or
       ( TGIS_ParamsSection(FParamsList[0]) = self )
    then
      FreeObject( def ) ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsSectionVector'}

constructor TGIS_ParamsSectionVector.Create ;
begin
  inherited ;

  FQueryObj        := nil ;
  FRender          := nil ;
  FLine            := nil ;
  FArea            := nil ;
  FMarker          := nil ;
  FLabels          := nil ;
  FChart           := nil ;

  FNormalizedM     := TGIS_3DNormalizationType.Off ;
  FScaleM          := 1 ;
  FFalseM          := 0 ;
  FFalseMAsText    := '' ;
  FGroundDefault   := True ;
  FGround          := TGIS_3DGroundType.OnDem   ;
  FBasementDefault := True ;
  FBasement        := TGIS_3DBasementType.Off ;
end ;

procedure  TGIS_ParamsSectionVector.doDestroy ;
begin
  FreeObject( FQueryObj ) ;
  FreeObject( FRender   ) ;
  FreeObject( FLine     ) ;
  FreeObject( FArea     ) ;
  FreeObject( FMarker   ) ;
  FreeObject( FLabels   ) ;
  FreeObject( FChart    ) ;
  FreeObject( FFalseMInternal ) ;

  inherited ;
end ;

function TGIS_ParamsSectionVector.fget_Query
  : String ;
begin
  if assigned( FQueryObj ) then Result := FQueryObj.Query
                           else Result := '' ;
end ;

procedure TGIS_ParamsSectionVector.fset_Query(
  const _value : String
) ;
begin
  if assigned( FQueryObj ) then
    if FQueryObj.Query = _value then exit ;
  FreeObject( FQueryObj ) ;
  if not IsStringEmpty( _value ) then begin
    FQueryObj := TGIS_SqlQuery.Create ;
    FQueryObj.Prepare( _value ) ;
    Touch ;
  end ;
end ;

function TGIS_ParamsSectionVector.fget_Render
  : TGIS_ParamsRender ;
begin
  if assigned( FRender ) then
    Result := FRender
  else begin
    if assigned( Shape ) and assigned( TGIS_Shape( Shape ).Layer ) then
      Result := TGIS_Shape( Shape ).Layer.Params.Render
    else
      Result := nil ;

    if not assigned( Result ) then begin
      FRender := TGIS_ParamsRender.Create ;
      FRender.Parent := Self ;
      Result := FRender ;
    end;
  end ;
  Result.FParentProxy := self ;
end ;

function TGIS_ParamsSectionVector.fget_Line
  : TGIS_ParamsLine ;
begin
  if assigned( FLine ) then
    Result := FLine
  else begin
    if assigned( Shape ) and assigned( TGIS_Shape( Shape ).Layer ) then
      Result := TGIS_Shape( Shape ).Layer.Params.Line
    else
      Result := nil ;

    if not assigned( Result ) then begin
      FLine := TGIS_ParamsLine.Create ;
      FLine.Parent := Self ;
      Result := FLine ;
    end;
  end ;
  Result.FParentProxy := self ;
end ;

function TGIS_ParamsSectionVector.fget_Area
  : TGIS_ParamsArea ;
begin
  if assigned( FArea ) then
    Result := FArea
  else begin
    if assigned( Shape ) and assigned( TGIS_Shape( Shape ).Layer ) then
      Result := TGIS_Shape( Shape ).Layer.Params.Area
    else
      Result := nil ;

    if not assigned( Result ) then begin
      FArea := TGIS_ParamsArea.Create ;
      FArea.Parent := Self ;
      Result := FArea ;
    end;
  end ;
  Result.FParentProxy := self ;
end ;

function TGIS_ParamsSectionVector.fget_Marker
  : TGIS_ParamsMarker ;
begin
  if assigned( FMarker ) then
    Result := FMarker
  else begin
    if assigned( Shape ) and assigned( TGIS_Shape( Shape ).Layer ) then
      Result := TGIS_Shape( Shape ).Layer.Params.Marker
    else
      Result := nil ;

    if not assigned( Result ) then begin
      FMarker := TGIS_ParamsMarker.Create ;
      FMarker.Parent := Self ;
      Result := FMarker ;
    end;
  end ;
  Result.FParentProxy := self ;
end ;

function TGIS_ParamsSectionVector.fget_Labels
  : TGIS_ParamsLabel ;
begin
  if assigned( FLabels ) then
    Result := FLabels
  else begin
    if assigned( Shape ) and assigned( TGIS_Shape( Shape ).Layer ) then
      Result := TGIS_Shape( Shape ).Layer.Params.Labels
    else
      Result := nil ;

    if not assigned( Result ) then begin
      FLabels := TGIS_ParamsLabel.Create ;
      FLabels.Parent := Self ;
      Result := FLabels ;
    end;
  end ;
  Result.FParentProxy := self ;
end ;

function TGIS_ParamsSectionVector.fget_Chart
  : TGIS_ParamsChart ;
begin
  if assigned( FChart ) then
    Result := FChart
  else begin
    if assigned( Shape ) and assigned( TGIS_Shape( Shape ).Layer ) then
      Result := TGIS_Shape( Shape ).Layer.Params.Chart
    else
      Result := nil ;

    if not assigned( Result ) then begin
      FChart := TGIS_ParamsChart.Create ;
      FChart.Parent := Self ;
      Result := FChart ;
    end;
  end ;
  Result.FParentProxy := self ;
end ;

procedure TGIS_ParamsSectionVector.fset_NormalizedZ(
  const _value : TGIS_3DNormalizationType
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if _value = FNormalizedZ then exit ;
    FNormalizedZ := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsSectionVector.fset_ScaleZ(
  const _value : Double
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if _value = FScaleZ then exit ;
    FScaleZ := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsSectionVector.fset_FalseZ(
  const _value : Double
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if _value = FFalseZ then exit ;
    FFalseZ := _value ;
    FFalseZAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsSectionVector.fset_NormalizedM(
  const _value : TGIS_3DNormalizationType
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if _value = FNormalizedM then exit ;
    FNormalizedM := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsSectionVector.fset_ScaleM(
  const _value : Double
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if _value = FScaleM then exit ;
    FScaleM := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsSectionVector.fset_FalseM(
  const _value : Double
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if _value = FFalseM then exit ;
    FFalseM := _value ;
    FFalseMAsText := '' ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsSectionVector.fset_Ground(
  const _value : TGIS_3DGroundType
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if _value = FGround then exit ;
    FGround := _value ;
    Touch ;
  end ;
end ;

procedure TGIS_ParamsSectionVector.fset_Basement(
  const _value : TGIS_3DBasementType
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if _value = FBasement then exit ;
    FBasement := _value ;
    Touch ;
  end ;
end ;

function TGIS_ParamsSectionVector.fget_FalseM
  : Double ;
begin
  Result := compute_measure( Shape, FFalseM, FFalseMInternal ) ;
end ;

procedure TGIS_ParamsSectionVector.fset_FalseZAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsSectionVector ;
  dtmp_size  : Double ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    dtmp_size := FalseZ ;

    try
      measure_from_text( self, _value, dtmp_size, FFalseZInternal );

      FalseZ := dtmp_size ;
      FFalseZAsText := _value ;
    except
      TGIS_Logger.AsWarning(
        'Bad ' + GIS_INI_FALSEZ,
        '"' + _value + '"'
      );
    end;
  end;
end;

function TGIS_ParamsSectionVector.fget_FalseMAsText
  : String ;
begin
  Result := measure_to_text( FFalseMAsText, FalseM, FFalseMInternal ) ;
end;

procedure TGIS_ParamsSectionVector.fset_FalseMAsText(
  const _value : String
) ;
var
  prm        : TGIS_ParamsSectionVector ;
  dtmp_size  : Double ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    dtmp_size := FalseM ;

    try
      measure_from_text( self, _value, dtmp_size, FFalseMInternal );

      FalseM := dtmp_size ;
      FFalseMAsText := _value ;
    except
      TGIS_Logger.AsWarning(
        'Bad ' + GIS_INI_FALSEM,
        '"' + _value + '"'
      );
    end;
  end;
end;

procedure TGIS_ParamsSectionVector.fset_GroundIfNotDefault(
  const _value : TGIS_3DGroundType
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if FGroundDefault then begin
      Ground := _value ;
      FGroundDefault := False ;
    end;
  end ;
end ;

procedure TGIS_ParamsSectionVector.fset_BasementIfNotDefault(
  const _value : TGIS_3DBasementType
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := TGIS_ParamsSectionVector( prepareParams ) ;
  with prm do begin
    if FBasementDefault then begin
      Basement := _value ;
      FBasementDefault := False ;
    end;
  end ;
end ;

function TGIS_ParamsSectionVector.prepareParams
  : TGIS_ParamsSectionVector ;
begin
  if not assigned ( FShape )
  then
     Result := self
  else if TGIS_Shape( FShape ).LocalParams
  then
    Result := Self
  else begin
    Result := TGIS_ParamsSectionVector.Create ;
    Result.assignInternal( Self ) ;
    Result.FShape := FShape  ;
    TGIS_Shape( FShape
              ).SetParamsInternal( Result ) ;
  end ;
end ;

procedure TGIS_ParamsSectionVector.assignInternal(
  _source : TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsSectionVector.Create
  else
    src := _source ;
  try
    if src is TGIS_ParamsSectionVector then begin
      if not assigned( FShape ) then begin
        Query  := TGIS_ParamsSectionVector( src ).Query  ;
        Render.assignInternal( TGIS_ParamsSectionVector( src ).Render ) ;
      end ;
      Line.assignInternal( TGIS_ParamsSectionVector( src ).Line   ) ;
      Area.assignInternal( TGIS_ParamsSectionVector( src ).Area   ) ;
      Marker.assignInternal( TGIS_ParamsSectionVector( src ).Marker ) ;
      Labels.assignInternal( TGIS_ParamsSectionVector( src ).Labels ) ;
      Chart.assignInternal ( TGIS_ParamsSectionVector( src ).Chart  ) ;

      FGround               := TGIS_ParamsSectionVector( src ).Ground        ;
      FBasement             := TGIS_ParamsSectionVector( src ).Basement      ;
      FNormalizedM          := TGIS_ParamsSectionVector( src ).NormalizedM   ;
      FScaleM               := TGIS_ParamsSectionVector( src ).ScaleM        ;
      FFalseM               := TGIS_ParamsSectionVector( src ).FalseM        ;
      FFalseMAsText         := TGIS_ParamsSectionVector( src ).FalseMAsText  ;
      TGIS_ParamsField.assignEx(
                               self,
                               FFalseMInternal,
                               TGIS_ParamsSectionVector( src ).FFalseMInternal
                             ) ;
    end ;
    inherited Assign( src ) ;
  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsSectionVector.Assign(
  _source : TPersistent
) ;
var
  prm : TGIS_ParamsSectionVector ;
begin
  prm := Line.prepareParamsVector ;
  if assigned( prm ) then
    prm.assignInternal( _source )
  else
    assignInternal( _source ) ;
end ;

procedure TGIS_ParamsSectionVector.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  if not TGIS_Config( _cfg ).IsShapeStyle then begin
    inherited ;

    with TGIS_Config( _cfg ) do begin
      oConfig := TGIS_Config( _cfg ) ;

      if config_any_format( _cfg ) then begin
        Query                  := ReadString   ( GIS_INI_QUERY,
                                                 Query
                                               ) ;
      end;

      if config_old_format( _cfg ) then begin
        FGroundDefault         := ReadString   ( GIS_INI_GROUND,
                                                 ''
                                               ) = '' ;
        Ground                 := ReadGround   ( GIS_INI_GROUND,
                                                 Ground
                                               ) ;

        FBasementDefault       := ReadString   ( GIS_INI_GROUND,
                                                 ''
                                               ) = '' ;
        Basement               := ReadBasement ( GIS_INI_BASEMENT,
                                                 Basement
                                               ) ;
        NormalizedM            := ReadNormalized
                                               ( GIS_INI_NORMALIZEDM,
                                                 NormalizedM
                                               ) ;
        ScaleM                 := ReadFloat    ( GIS_INI_SCALEM,
                                                 ScaleM
                                               ) ;
        FalseM                 := ReadFloat    ( GIS_INI_FALSEM,
                                                 FalseM
                                               ) ;
        TGIS_ParamsField.readFromConfig        ( self,
                                                 FFalseMInternal,
                                                 _cfg,
                                                 GIS_INI_FALSEMEX,
                                                 FFalseMInternal,
                                                 FFalseMAsText
                                               );
      end;

      if config_new_format( _cfg ) then begin
        FGroundDefault         := ReadString   ( GIS_INI_GROUND,
                                                 ''
                                               ) = '' ;
        Ground                 := ReadGround   ( GIS_INI_GROUND,
                                                 Ground
                                               ) ;
        FBasementDefault       := ReadString   ( GIS_INI_GROUND,
                                                 ''
                                               ) = '' ;
        Basement               := ReadBasement ( GIS_INI_BASEMENT,
                                                 Basement
                                               ) ;
        NormalizedM            := ReadNormalized
                                               ( GIS_INI_NORMALIZEDM,
                                                 NormalizedM
                                               ) ;
        ScaleM                 := ReadFloat    ( GIS_INI_SCALEM,
                                                 ScaleM
                                               ) ;
        FalseMAsText           := ReadString   ( GIS_INI_FALSEM,
                                                 FalseMAsText
                                               ) ;
      end ;

      oConfig := nil ;
    end ;

    Render.LoadFromConfig( _cfg ) ;
  end ;

  Line.LoadFromConfig  ( _cfg ) ;
  Area.LoadFromConfig  ( _cfg ) ;
  Marker.LoadFromConfig( _cfg ) ;
  Labels.LoadFromConfig( _cfg ) ;
  Chart.LoadFromConfig ( _cfg ) ;
end ;

procedure TGIS_ParamsSectionVector.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
var
  def : TGIS_ParamsSectionVector;
begin
  def := TGIS_ParamsSectionVector.Create ;
  try
    if not TGIS_Config( _cfg ).IsShapeStyle then begin

      inherited ;

      with TGIS_Config( _cfg ) do begin

        if config_any_format( _cfg ) then begin
          WriteString                          ( GIS_INI_QUERY,
                                                 Query,
                                                 ''
                                               ) ;
        end;

        if config_old_format( _cfg ) then begin
          if Ground = TGIS_3DGroundType.OnDem then
            WriteGround                        ( GIS_INI_GROUND,
                                                 Ground,
                                                 TGIS_3DGroundType.AboveZero
                                               )
          else
            WriteGround                        ( GIS_INI_GROUND,
                                                 Ground,
                                                 def.Ground
                                               ) ;
          WriteBasement                        ( GIS_INI_BASEMENT,
                                                 Basement,
                                                 def.Basement
                                               ) ;
          WriteNormalized                      ( GIS_INI_NORMALIZEDM,
                                                 NormalizedM,
                                                 def.NormalizedM
                                                ) ;
          WriteFloat                           ( GIS_INI_SCALEM,
                                                 ScaleM,
                                                 def.ScaleM
                                                ) ;
          WriteFloat                           ( GIS_INI_FALSEM,
                                                 FalseM,
                                                 def.FalseM
                                               ) ;
          TGIS_ParamsField.writeToConfig       ( _cfg,
                                                 GIS_INI_FALSEMEX,
                                                 FFalseMInternal
                                               ) ;
        end ;

        if config_new_format( _cfg ) then begin
          if Ground = TGIS_3DGroundType.OnDem then
            WriteGround                        ( GIS_INI_GROUND,
                                                 Ground,
                                                 TGIS_3DGroundType.AboveZero
                                               )
          else
            WriteGround                        ( GIS_INI_GROUND,
                                                 Ground,
                                                 def.Ground
                                               ) ;
          WriteBasement                        ( GIS_INI_BASEMENT,
                                                 Basement,
                                                 def.Basement
                                               ) ;
          WriteNormalized                      ( GIS_INI_NORMALIZEDM,
                                                 NormalizedM,
                                                 def.NormalizedM
                                                ) ;
          WriteFloat                           ( GIS_INI_SCALEM,
                                                 ScaleM,
                                                 def.ScaleM
                                                ) ;
          WriteString                          ( GIS_INI_FALSEM,
                                                 FalseMAsText,
                                                 def.FalseMAsText
                                               ) ;
        end ;
      end;

      Render.SaveToConfig( _cfg ) ;
    end ;

    Line.SaveToConfig  ( _cfg ) ;
    Area.SaveToConfig  ( _cfg ) ;
    Marker.SaveToConfig( _cfg ) ;
    Labels.SaveToConfig( _cfg ) ;
    Chart.SaveToConfig ( _cfg ) ;

  finally
    FreeObject( def )
  end ;

end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsSectionPixel'}

constructor TGIS_ParamsSectionPixel.Create ;
begin
  inherited ;

  FPixel := nil ;
end ;

procedure TGIS_ParamsSectionPixel.doDestroy ;
begin
  FreeObject( FPixel ) ;
  inherited ;
end ;

function TGIS_ParamsSectionPixel.fget_Pixel
  : TGIS_ParamsPixel ;
begin
  if not assigned( FPixel ) then begin
    FPixel := TGIS_ParamsPixel.Create ;
    FPixel.Parent := Self ;
  end ;
  Result := FPixel ;
end ;

procedure TGIS_ParamsSectionPixel.Assign(
  _source : TPersistent
) ;
var
  src : TPersistent ;
begin
  if not assigned( _source ) then
    src := TGIS_ParamsSectionPixel.Create
  else
    src := _source ;
  try

    if src is TGIS_ParamsSectionPixel then begin
      Pixel.Assign( TGIS_ParamsSectionPixel( src ).Pixel ) ;
    end ;
    inherited Assign( src ) ;

  finally
    if src <> _source then
      FreeObject( src )
  end ;
end ;

procedure TGIS_ParamsSectionPixel.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  inherited ;

  Pixel.LoadFromConfig( _cfg ) ;
end ;

procedure TGIS_ParamsSectionPixel.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  inherited ;

  Pixel.SaveToConfig( _cfg ) ;
end ;

{$ENDREGION}
{$REGION 'TGIS_ParamsList'}

constructor TGIS_ParamsList.Create ;
begin
  inherited Create ;
  FSelected := 0 ;

  listObj := TObjectList<TGIS_ParamsAbstract>.Create( False ) ;
end ;

procedure TGIS_ParamsList.doDestroy ;
var
  i : Integer ;
  obj : TGIS_ParamsSection ;
begin
  for i:= listObj.Count - 1 downto 0 do begin
    obj := TGIS_ParamsSection( listObj[ i ] ) ;
    FreeObject( obj ) ;
  end ;

  FreeObject( listObj ) ;

  inherited ;
end ;

function TGIS_ParamsList.fget_Count
  : Integer ;
begin
  Result := listObj.Count ;
end ;

procedure TGIS_ParamsList.fset_Selected(
  const _idx : Integer
) ;
begin
  assert( listObj.Count > 0 ) ;

  if (_idx < 0 ) or (_idx >= listObj.Count ) then exit ;

  FSelected := _idx ;

  FSelectedObj := TGIS_ParamsSection( listObj[ _idx ] ) ;
end ;

function TGIS_ParamsList.fget_Item(
  const _idx : Integer
) : TGIS_ParamsSection ;
begin
  assert( listObj.Count > 0 ) ;

  Result := TGIS_ParamsSection( listObj[ _idx ] ) ;
  Result.FShape := nil ;
end ;

procedure TGIS_ParamsList.fset_Item(
  const _idx   : Integer            ;
  const _value : TGIS_ParamsSection
) ;
begin
  assert( listObj.Count > 0 ) ;

  TGIS_ParamsAbstract( listObj[ _idx ] ).Assign( _value ) ;
end ;

function TGIS_ParamsList.fget_Serial
  : Integer ;
var
  n : Int64 ;
  i : Integer ;
begin
  n := 0 ;
  for i:=0 to listObj.Count - 1 do
    n := n + TGIS_ParamsAbstract( listObj[ i ] ).Serial ;

  Result := n mod high(Integer);
end ;

procedure TGIS_ParamsList.Assign(
  const _params : TGIS_ParamsList
) ;
var
  i   : Integer ;
  obj : TGIS_ParamsAbstract ;
begin
  {$IFNDEF NEXTGEN}
    for i:= listObj.Count - 1 downto 0 do begin
      FreeObjectNotNil( TGIS_ParamsSection( listObj[ i ] ) ) ;
    end ;
  {$ENDIF}
  listObj.Clear ;

  for i:= 0 to _params.Count - 1 do begin
    obj := _params.Items[ i ].CreateCopy ;
    obj.FParamsList := self ;
    listObj.Add( TGIS_ParamsSection( obj ) ) ;
  end ;
  Selected := 0 ;
end ;

procedure TGIS_ParamsList.SetUp(
  const _params : TGIS_ParamsSection
) ;
begin
  assert( listObj.Count <= 0 ) ;

  listObj.Add( _params ) ;
  _params.FParamsList := self ;

  Selected := 0  ;
end ;

procedure TGIS_ParamsList.Clear ;
var
  i   : Integer ;
begin
  // clear all except first
  for i:= listObj.Count - 1 downto 1 do begin
    {$IFNDEF NEXTGEN}
      FreeObjectNotNil( TGIS_ParamsSection( listObj[ i ] ) ) ;
    {$ENDIF}
    listObj.Delete( i ) ;
  end ;
  Selected := 0 ;
end ;

procedure TGIS_ParamsList.ClearAndSetDefaults ;
var
  pv  : TGIS_ParamsSectionVector ;
  pp  : TGIS_ParamsSectionPixel  ;
begin
  Clear ;

  if Items[0] is TGIS_ParamsSectionVector then begin
    pv := TGIS_ParamsSectionVector.Create ;
    TGIS_ParamsSectionVector(Items[0]).Assign( pv ) ;
    FreeObject( pv ) ;
  end
  else if Items[0] is TGIS_ParamsSectionPixel then begin
    pp := TGIS_ParamsSectionPixel.Create ;
    pp.Pixel.GridNoValue := TGIS_ParamsSectionPixel(Items[0]).Pixel.GridNoValue ;
    TGIS_ParamsSectionPixel(Items[0]).Assign( pp ) ;
    FreeObject( pp ) ;
  end ;
end ;

procedure TGIS_ParamsList.Add ;
var
  obj : TGIS_ParamsAbstract ;
begin
  assert( listObj.Count > 0 ) ;

  obj := TGIS_ParamsSection( listObj[ 0 ] ).CreateCopy ;
  obj.FParamsList := self ;

  obj.FSerial := SerialSeed ;

  listObj.Add( obj ) ;

  Selected := listObj.Count - 1 ;
end ;

procedure TGIS_ParamsList.Delete ;
begin
  assert( listObj.Count > 0 ) ;

  if FSelected <= 0 then exit ;

  FreeObjectNotNil( TGIS_ParamsSection( listObj[ FSelected ] ) ) ;
  listObj.Delete( FSelected ) ;

  Selected := 0 ;
end ;

procedure TGIS_ParamsList.Exchange(
  const _index1 : Integer ;
  const _index2 : Integer
) ;
begin
  listObj.Exchange( _index1, _index2 ) ;
end ;

procedure TGIS_ParamsList.LoadFromStrings(
  const _str : {$IFDEF OXYGENE}
                 TGIS_Strings
               {$ELSE}
                 TStrings
               {$ENDIF}
) ;
var
  cfg  : TGIS_Config ;
  i    : Integer  ;
  fnd  : Boolean  ;
  head : String   ;
begin
  cfg := TGIS_ConfigFactory.CreateConfig( nil, '' ) ;
  try
    head := Format( '[%s]', [GIS_INI_LAYER_HEADER] ) ;
    fnd := False ;
    for i:=0 to _str.Count - 1 do begin
      if CompareText( Trim(_str[i]), head ) = 0 then begin
        fnd := True ;
        break ;
      end ;
    end ;

    if not fnd  then
      _str.Insert( 0, head ) ;

    cfg.SetStrings( _str ) ;
    _str.Delete( 0 ) ;
    cfg.SetLayer( nil ) ;
    LoadFromConfig( cfg ) ;
  finally
    FreeObject( cfg ) ;
  end ;
end ;

procedure TGIS_ParamsList.SaveToStrings(
  const _str : {$IFDEF OXYGENE}
                 TGIS_Strings
               {$ELSE}
                 TStrings
               {$ENDIF}
) ;
var
  cfg : TGIS_Config ;
begin
  cfg := TGIS_ConfigFactory.CreateConfig( nil, '' ) ;
  try
    SaveToConfig( cfg ) ;
    cfg.GetStrings( _str ) ;
  finally
    FreeObject( cfg ) ;
  end ;
end ;

procedure TGIS_ParamsList.LoadFromConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  LoadFromConfig( _cfg, False, False ) ;
end ;

procedure TGIS_ParamsList.LoadFromConfig(
  const _cfg      : TGIS_ConfigAbstract ;
  const _subLayer : Boolean
) ;
begin
  LoadFromConfig( _cfg, _subLayer, False ) ;
end ;

procedure TGIS_ParamsList.LoadFromConfig(
  const _cfg      : TGIS_ConfigAbstract ;
  const _subLayer : Boolean  ;
  const _groups   : Boolean
) ;
var
  i  : Integer ;
begin
  if _subLayer then begin
    TGIS_Config( _cfg ).SetSubSection( 0, False ) ;
    Items[ 0 ].LoadFromConfig( _cfg ) ;
    i := 1 ;
    while TGIS_Config( _cfg ).SetSubSection( i, False ) do begin
      if i = 1 then Clear ;
      Add ;
      Items[ Selected ].LoadFromConfig( _cfg ) ;
      inc( i )
    end ;
  end
  else if _groups then begin
    TGIS_Config( _cfg ).SetGroupSection( 0 ) ;
    Items[ 0 ].LoadFromConfig( _cfg ) ;
    i := 1 ;
    while TGIS_Config( _cfg ).SetGroupSection( i ) do begin
      if i = 1 then Clear ;
      Add ;
      Items[ Selected ].LoadFromConfig( _cfg ) ;
      inc( i )
    end ;
  end
  else begin
    TGIS_Config( _cfg ).SetSection( 0, False ) ;
    Items[ 0 ].LoadFromConfig( _cfg ) ;
    i := 1 ;
    while TGIS_Config( _cfg ).SetSection( i, False ) do begin
      if i = 1 then Clear ;
      Add ;
      Items[ Selected ].LoadFromConfig( _cfg ) ;
      inc( i )
    end ;
  end
end ;

procedure TGIS_ParamsList.SaveToConfig(
  const _cfg : TGIS_ConfigAbstract
) ;
begin
  SaveToConfig( _cfg, False, False ) ;
end ;

procedure TGIS_ParamsList.SaveToConfig(
  const _cfg      : TGIS_ConfigAbstract ;
  const _subLayer : Boolean
) ;
begin
  SaveToConfig( _cfg, _subLayer, False ) ;
end ;

procedure TGIS_ParamsList.SaveToConfig(
  const _cfg      : TGIS_ConfigAbstract ;
  const _subLayer : Boolean  ;
  const _groups   : Boolean
) ;
var
  i  : Integer ;
begin
  if _subLayer then
    TGIS_Config( _cfg ).ClearSubSections
  else if _groups then
    TGIS_Config( _cfg ).ClearGroups
  else
    TGIS_Config( _cfg ).ClearSections ;

  if _subLayer then begin
    for i := 0 to Count - 1 do begin
      TGIS_Config( _cfg ).SetSubSection( i, True ) ;
      Items[ i ].SaveToConfig( _cfg ) ;
    end ;
  end
  else if _groups then begin
    for i := 0 to Count - 1 do begin
      TGIS_Config( _cfg ).SetGroupSection( i ) ;
      Items[ i ].SaveToConfig( _cfg ) ;
    end ;
  end
  else begin
    for i := 0 to Count - 1 do begin
      TGIS_Config( _cfg ).SetSection( i, True ) ;
      Items[ i ].SaveToConfig( _cfg ) ;
    end ;
  end ;
end ;

procedure TGIS_ParamsList.LoadFromFile(
  const _path : String
) ;
var
  cfg : TGIS_Config ;
begin
   cfg := TGIS_ConfigFactory.CreateConfig( nil, _path ) ;
   try
     LoadFromConfig( cfg ) ;
   finally
     FreeObject( cfg ) ;
   end ;
end ;

procedure TGIS_ParamsList.SaveToFile(
  const _path : String
) ;
var
  cfg : TGIS_Config ;
begin
  cfg := TGIS_ConfigFactory.CreateConfig( nil, _path ) ;
 try
   cfg.WriteFull := False ;
   SaveToConfig( cfg ) ;
   cfg.Save ;
 finally
   FreeObject( cfg ) ;
 end ;
end ;

procedure TGIS_ParamsList.ResetSerial ;
var
  i : Integer ;
begin
  for i:=0 to listObj.Count - 1 do
    TGIS_ParamsAbstract( listObj[ i ] ).ResetSerial ;
end;

{$ENDREGION}
{$REGION 'TGIS_ParamsField'}

constructor TGIS_ParamsField.Create( const _params : TGIS_ParamsAbstract ) ;
begin
  inherited Create ;

  FParams    := _params ;
  FFieldName := '' ;
  FFactor    := 1 ;
  FScalable  := False ;
  FSubType   := '' ;
end ;

procedure TGIS_ParamsField.doDestroy ;
begin
  inherited ;
end ;

class procedure TGIS_ParamsField.assignEx(
  const _parent : TGIS_ParamsAbstract ;
  var   _dst    : TGIS_ParamsField    ;
  const _src    : TGIS_ParamsField
) ;
begin
  if not assigned( _src ) then begin
    FreeObject( _dst ) ;
    exit ;
  end;

  if not assigned( _dst ) then
    _dst := TGIS_ParamsField.Create( _parent ) ;

  _dst.Assign( _src ) ;
end ;

class procedure TGIS_ParamsField.readFromConfig(
  const _parent   : TGIS_ParamsAbstract ;
  var   _dst      : TGIS_ParamsField    ;
  const _cfg      : TGIS_ConfigAbstract ;
  const _key      : String              ;
  const _def      : TGIS_ParamsField    ;
  var   _astext   : String
) ;
var
  def_fieldname : String  ;
  def_factor    : Double  ;
  def_scalable  : Boolean ;
  def_subtype   : String ;

  dst_fieldname : String  ;
  dst_factor    : Double  ;
  dst_scalable  : Boolean ;
  dst_subtype   : String ;
begin
  if assigned( _def ) then begin
    def_fieldname := _def.FFieldName ;
    def_factor    := _def.Factor     ;
    def_scalable  := _def.Scalable   ;
    def_subtype   := _def.FSubType   ;
  end
  else begin
    def_fieldname := ''    ;
    def_factor    := 1     ;
    def_scalable  := False ;
    def_subtype   := ''    ;
  end;

  with TGIS_Config( _cfg ) do begin
    dst_fieldname := ReadString ( _key + GIS_INI_PARAMSFIELD_FIELD,
                                  def_fieldname
                                ) ;
    dst_factor    := ReadFloat  ( _key + GIS_INI_PARAMSFIELD_FACTOR,
                                  def_factor
                                ) ;
    dst_scalable  := ReadBoolean( _key + GIS_INI_PARAMSFIELD_SCALABLE,
                                  def_scalable
                                ) ;
    dst_subtype   := ReadString( _key + GIS_INI_PARAMSFIELD_SUBTYPE,
                                  def_subtype
                                ) ;
  end;

  FreeObject( _dst ) ;

  if not IsStringEmpty( dst_fieldname ) then begin
    _dst := TGIS_ParamsField.Create( _parent ) ;

    _dst.Field    := dst_fieldname ;
    _dst.Factor   := dst_factor    ;
    _dst.Scalable := dst_scalable  ;
    _dst.FSubType := dst_subtype   ;
    _astext       := ''            ;
  end ;
end ;

class procedure TGIS_ParamsField.writeToConfig(
  const _cfg   : TGIS_ConfigAbstract ;
  const _key   : String              ;
  const _value : TGIS_ParamsField
) ;
begin
  with TGIS_Config( _cfg ) do begin
    if assigned( _value ) then begin
      WriteString ( _key + GIS_INI_PARAMSFIELD_FIELD,
                    _value.FFieldName,
                    ''
                  ) ;
      WriteFloat  ( _key + GIS_INI_PARAMSFIELD_FACTOR,
                    _value.FFactor,
                    1
                  ) ;
      WriteBoolean( _key + GIS_INI_PARAMSFIELD_SCALABLE,
                    _value.FScalable,
                    False
                  ) ;
      WriteString ( _key + GIS_INI_PARAMSFIELD_SUBTYPE,
                    _value.FSubType,
                    ''
                  ) ;
    end
    else begin
      WriteString ( _key + GIS_INI_PARAMSFIELD_FIELD,
                    '',
                    ''
                  ) ;
      WriteFloat  ( _key + GIS_INI_PARAMSFIELD_FACTOR,
                    0,
                    0
                  ) ;
      WriteBoolean( _key + GIS_INI_PARAMSFIELD_SCALABLE,
                    False,
                    False
                  ) ;
      WriteString ( _key + GIS_INI_PARAMSFIELD_SUBTYPE,
                    '',
                    ''
                  ) ;
    end;
  end;
end ;

function TGIS_ParamsField.IsDifferent(
  const _src : TGIS_ParamsField
) : Boolean;
begin
  Result := not assigned( _src ) or
            ( FFieldName <> _src.FFieldName ) or
            ( FFactor    <> _src.FFactor    ) or
            ( FScalable  <> _src.FScalable  ) or
            ( FSubType   <> _src.FSubType   );
end ;

procedure TGIS_ParamsField.fset_Factor(
  const _value : Double
) ;
begin
  if FFactor = _value then exit ;
  FFactor := _value ;
  if assigned( FParams ) then
    FParams.Touch ;
end ;

procedure TGIS_ParamsField.fset_FieldName(
  const _value : String
) ;
begin
  if FFieldName = _value then exit ;
  FFieldName := GisDeNormalizedSQLName(_value) ;
  if assigned( FParams ) then
    FParams.Touch ;
end ;

procedure TGIS_ParamsField.fset_Scalable(
  const _value : Boolean
) ;
begin
  if FScalable = _value then exit ;
  FScalable := _value ;
  if assigned( FParams ) then
    FParams.Touch ;
end ;

procedure TGIS_ParamsField.Assign(
  const _src : TGIS_ParamsField
) ;
begin
  if assigned( _src ) then begin
    FFieldName := GisDeNormalizedSQLName(_src.FFieldName) ;
    FFactor    := _src.FFactor ;
    FScalable  := _src.FScalable ;
    FSubType   := _src.FSubType ;
  end
  else begin
    FFieldName := '' ;
    FFactor    := 1 ;
    FScalable  := False ;
    FSubType   := '' ;
  end ;
end ;

{$ENDREGION}
{$REGION 'Public functions for parameter conversion'}

function paramValueIsText(
  const _value : String ;
  const _txt   : String
) : Boolean ;
begin
  Result := CompareText( _value, _txt ) = 0 ;
end ;

function ParamAlignment (
  const _value   : String  ;
  const _default : TGIS_LabelAlignment
) : TGIS_LabelAlignment ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_ALIGNMENT_SINGLE ) then
    Result := TGIS_LabelAlignment.Single
  else if paramValueIsText( _value, GIS_INI_PARAM_ALIGNMENT_LEFTJUSTIFY ) then
    Result := TGIS_LabelAlignment.LeftJustify
  else if paramValueIsText( _value, GIS_INI_PARAM_ALIGNMENT_CENTER ) then
    Result := TGIS_LabelAlignment.Center
  else if paramValueIsText( _value, GIS_INI_PARAM_ALIGNMENT_RIGHTJUSTIFY ) then
    Result := TGIS_LabelAlignment.RightJustify
  else if paramValueIsText( _value, GIS_INI_PARAM_ALIGNMENT_FOLLOW ) then
    Result := TGIS_LabelAlignment.Follow
  else
    Result := _default ;

end ;

function ConstructParamAlignment(
  const _value  : TGIS_LabelAlignment
) : String ;
begin
  case _value of
    TGIS_LabelAlignment.Single       :
       Result := GIS_INI_PARAM_ALIGNMENT_SINGLE       ;
    TGIS_LabelAlignment.LeftJustify  :
       Result := GIS_INI_PARAM_ALIGNMENT_LEFTJUSTIFY  ;
    TGIS_LabelAlignment.Center       :
       Result := GIS_INI_PARAM_ALIGNMENT_CENTER       ;
    TGIS_LabelAlignment.RightJustify :
       Result := GIS_INI_PARAM_ALIGNMENT_RIGHTJUSTIFY ;
    TGIS_LabelAlignment.Follow       :
       Result := GIS_INI_PARAM_ALIGNMENT_FOLLOW       ;
    else begin
      assert( False, 'Unsupported Case' ) ;
    end ;
  end ;
end ;

function ParamBitmap(
  const _value   : String ;
  const _default : TGIS_Bitmap
) : TGIS_Bitmap ;
begin
  if _value = GIS_PARAM_NIL then begin
     Result := _default ;
  end
  else if IsStringEmpty( _value ) then begin
    Result := nil
  end
  else begin
    Result := TGIS_Bitmap.Create ;
    try
      Result.LoadFromFile( _value ) ;
    except
      FreeObject( Result ) ;
      Result := _default ;
    end ;
  end ;
end ;

function ParamBoolean(
  const _value   : String ;
  const _default : Boolean
) : Boolean ;
begin
  Result := StrToBoolean( _value, _default ) ;
end ;

function ConstructParamBoolean(
  const _value : Boolean
) : String ;
begin
  if _value then Result := GIS_INI_PARAM_BOOLEAN_YES
            else Result := GIS_INI_PARAM_BOOLEAN_NO ;
end ;

function ParamMultiUser(
  const _value   : String ;
  const _default : TGIS_MultiUser
) : TGIS_MultiUser ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_MULTIUSER_MODE_SINGLE ) then
    Result := TGIS_MultiUser.SingleUser
  else if paramValueIsText( _value, GIS_INI_MULTIUSER_MODE_MULTI ) then
    Result := TGIS_MultiUser.MultiUser
  else
    Result := TGIS_MultiUser.Default ;
end ;

function ConstructParamMultiUser(
  const _value : TGIS_MultiUser
) : String ;
begin
  case _value of
    TGIS_MultiUser.SingleUser : Result := GIS_INI_MULTIUSER_MODE_SINGLE  ;
    TGIS_MultiUser.MultiUser  : Result := GIS_INI_MULTIUSER_MODE_MULTI   ;
    TGIS_MultiUser.Default    : Result := GIS_INI_MULTIUSER_MODE_DEFAULT ;
  else                          Result := GIS_INI_MULTIUSER_MODE_DEFAULT ;
  end ;
end ;

function ParamColor(
  const _value   : String ;
  const _default : TGIS_Color
) : TGIS_Color ;
var
  red   : Byte   ;
  green : Byte   ;
  blue  : Byte   ;
  alpha : Byte   ;
  res   : TGIS_Color ;

  function parseRGB(
    const _val   : String ;
    const _def : TGIS_Color
  ) : TGIS_Color ;
  var
    c   : Cardinal ;
    tkn : TGIS_Tokenizer ;
  begin
    if Pos( String( ':' ), _val ) < StringFirst then begin
      try
        c := StrToInt( Trim( _val ) ) ;
        Result := TGIS_Color.FromABGR( c ) ;
        if ( Result.ARGB and $FF000000 ) = 0 then begin
          // potentential RGB

          if ( Result.ARGB = 0 ) then begin
            // potential black $000000 or 0
            if Pos( String( '$' ), _val ) = StringFirst then
              if length( _val ) > 7 then
                exit
            else
            if length( _val ) < 7 then
              exit ;
          end;
          Result.ARGB := Result.ARGB or $FF000000
        end;
      except
        Result := _def ;
      end ;
      exit ;
    end ;

    red   := 0 ;
    green := 0 ;
    blue  := 0 ;
    alpha := 255 ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( _val, [':'] ) ;
      try
        if tkn.Result.Count > 0 then
           red := StrToInt( tkn.Result[0] ) ;
        if tkn.Result.Count > 1 then
           green := StrToInt( tkn.Result[1] ) ;
        if tkn.Result.Count > 2 then
           blue := StrToInt( tkn.Result[2] ) ;
        if tkn.Result.Count > 3 then
           alpha := StrToInt( tkn.Result[3] ) ;
        Result := TGIS_Color.FromARGB( alpha, red, green, blue )
      except
        Result := _def ;
      end;
    finally
      FreeObject( tkn ) ;
    end;
  end ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_COLOR_AQUA ) then
    res := TGIS_Color.Aqua
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_GRAY ) then
    res := TGIS_Color.Gray
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_NAVY ) then
    res := TGIS_Color.Navy
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_SILVER ) then
    res := TGIS_Color.Silver
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_BLACK ) then
    res := TGIS_Color.Black
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_GREEN ) then
    res := TGIS_Color.Green
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_OLIVE ) then
    res := TGIS_Color.Olive
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_TEAL ) then
    res := TGIS_Color.Teal
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_BLUE ) then
    res := TGIS_Color.Blue
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_LIME ) then
    res := TGIS_Color.Lime
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_PURPLE ) then
    res := TGIS_Color.Purple
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_WHITE ) then
    res := TGIS_Color.White
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_FUCHSIA ) then
    res := TGIS_Color.Fuchsia
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_MAROON ) then
    res := TGIS_Color.Maroon
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_RED) then
    res := TGIS_Color.Red
  else if paramValueIsText( _value, GIS_INI_PARAM_COLOR_YELLOW )
    then res := TGIS_Color.Yellow
  else if paramValueIsText( _value, GIS_INI_PARAM_RENDER ) then
    res := TGIS_Color.RenderColor
  else
    res := parseRGB( _value, _default ) ;

  Result := res ;
end ;

function ConstructParamColor(
  const _value : TGIS_Color
) : String ;
begin
  if      _value.ARGB = TGIS_Color.Aqua.ARGB         then
          Result := GIS_INI_PARAM_COLOR_AQUA
  else if _value.ARGB = TGIS_Color.Gray.ARGB         then
          Result := GIS_INI_PARAM_COLOR_GRAY
  else if _value.ARGB = TGIS_Color.Navy.ARGB         then
          Result := GIS_INI_PARAM_COLOR_NAVY
  else if _value.ARGB = TGIS_Color.Silver.ARGB       then
          Result := GIS_INI_PARAM_COLOR_SILVER
  else if _value.ARGB = TGIS_Color.Black.ARGB        then
          Result := GIS_INI_PARAM_COLOR_BLACK
  else if _value.ARGB = TGIS_Color.Green.ARGB        then
          Result := GIS_INI_PARAM_COLOR_GREEN
  else if _value.ARGB = TGIS_Color.Olive.ARGB        then
          Result := GIS_INI_PARAM_COLOR_OLIVE
  else if _value.ARGB = TGIS_Color.Teal.ARGB         then
          Result := GIS_INI_PARAM_COLOR_TEAL
  else if _value.ARGB = TGIS_Color.Blue.ARGB         then
          Result := GIS_INI_PARAM_COLOR_BLUE
  else if _value.ARGB = TGIS_Color.Lime.ARGB         then
          Result := GIS_INI_PARAM_COLOR_LIME
  else if _value.ARGB = TGIS_Color.Purple.ARGB       then
          Result := GIS_INI_PARAM_COLOR_PURPLE
  else if _value.ARGB = TGIS_Color.White.ARGB        then
          Result := GIS_INI_PARAM_COLOR_WHITE
  else if _value.ARGB = TGIS_Color.Fuchsia.ARGB      then
          Result := GIS_INI_PARAM_COLOR_FUCHSIA
  else if _value.ARGB = TGIS_Color.Maroon.ARGB       then
          Result := GIS_INI_PARAM_COLOR_MAROON
  else if _value.ARGB = TGIS_Color.Red.ARGB          then
          Result := GIS_INI_PARAM_COLOR_RED
  else if _value.ARGB = TGIS_Color.Yellow.ARGB       then
          Result := GIS_INI_PARAM_COLOR_YELLOW
  else if _value.ARGB = TGIS_Color.RenderColor.ARGB  then
          Result := GIS_INI_PARAM_RENDER
  else begin
    {$IFDEF JAVA}
      Result :=  java.lang.String.format("%d:%d:%d:%d",
                   _value.R,_value.G,_value.B,_value.A
                 );
    {$ELSE}
      Result := Format( '%d:%d:%d:%d',
                  [ _value.R,_value.G,_value.B,_value.A ]
                ) ;
    {$ENDIF}
  end;
end ;

function ParamChart(
  const _value   : String ;
  const _default : TGIS_ChartStyle
) : TGIS_ChartStyle ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_CHART_PIE ) then
    Result := TGIS_ChartStyle.Pie
  else if paramValueIsText( _value, GIS_INI_PARAM_CHART_BAR ) then
    Result := TGIS_ChartStyle.Bar
  else
    Result := _default ;
end ;

function ConstructParamChart(
  const _value : TGIS_ChartStyle
) : String ;
begin
  case _value of
    TGIS_ChartStyle.Pie : Result := GIS_INI_PARAM_CHART_PIE  ;
    TGIS_ChartStyle.Bar : Result := GIS_INI_PARAM_CHART_BAR  ;
    else begin
      assert( False, 'Unsupported Case' ) ;
    end ;
  end ;
end ;

function ParamDormant(
  const _value   : String ;
  const _default : TGIS_LayerDormantMode
) : TGIS_LayerDormantMode ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_DORMANT_OFF ) then
    Result := TGIS_LayerDormantMode.Off
  else if paramValueIsText( _value, GIS_INI_PARAM_DORMANT_STANDARD ) then
    Result := TGIS_LayerDormantMode.Standard
  else if paramValueIsText( _value, GIS_INI_PARAM_DORMANT_AGRESSIVE ) then
    Result := TGIS_LayerDormantMode.Agressive
  else
    Result := _default ;
end ;

function ConstructParamDormant(
  const _value : TGIS_LayerDormantMode
) : String ;
begin
  case _value of
    TGIS_LayerDormantMode.Off        : Result := GIS_INI_PARAM_DORMANT_OFF       ;
    TGIS_LayerDormantMode.Standard   : Result := GIS_INI_PARAM_DORMANT_STANDARD  ;
    TGIS_LayerDormantMode.Agressive  : Result := GIS_INI_PARAM_DORMANT_AGRESSIVE ;
    else begin
      assert( False, 'Unsupported Case' ) ;
    end ;
  end ;
end ;

function ParamFontStyle (
  const _value   : String ;
  const _default : TGIS_FontStyles
) : TGIS_FontStyles ;
var
  i     : Integer ;
  res   : TGIS_FontStyles ;
  tkn   : TGIS_Tokenizer ;
  value : String ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  res := GisGetEmptyFontStyle ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _value, [':'] ) ;
    for i := 0 to tkn.Result.Count -1  do begin
      value := tkn.Result[i] ;
      if paramValueIsText( value, GIS_INI_PARAM_FONTSTYLE_NORMAL ) then begin
        res := GisGetEmptyFontStyle ;
        break ;  //?
      end
      else if paramValueIsText( value,GIS_INI_PARAM_FONTSTYLE_BOLD ) then
        res := GisAddFontStyle( res, TGIS_FontStyle.Bold )
      else if paramValueIsText( value,GIS_INI_PARAM_FONTSTYLE_ITALIC ) then
        res := GisAddFontStyle( res, TGIS_FontStyle.Italic )
      else if paramValueIsText( value,GIS_INI_PARAM_FONTSTYLE_UNDERLINE ) then
        res := GisAddFontStyle( res, TGIS_FontStyle.Underline )
      else if paramValueIsText( value,GIS_INI_PARAM_FONTSTYLE_STRIKEOUT ) then
        res := GisAddFontStyle( res, TGIS_FontStyle.StrikeOut )
      else
        res := _default ;
    end ;
  finally
    FreeObject( tkn )
  end ;

  Result := res ;
end ;

function ConstructParamFontStyle(
  const _value : TGIS_FontStyles
) : String ;
var
  res : String ;

  procedure add_result( const _str : String ) ;
  begin
    if not IsStringEmpty( res ) then res := res + ':' ;
    res := res + _str ;
  end ;
begin
  if _value = GisGetEmptyFontStyle then begin
    Result := GIS_INI_PARAM_FONTSTYLE_NORMAL ;
    exit ;
  end ;

  res := '' ;

  if GisTestFontStyle( TGIS_FontStyle.Bold, _value ) then
    add_result( GIS_INI_PARAM_FONTSTYLE_BOLD      ) ;
  if GisTestFontStyle( TGIS_FontStyle.Italic, _value ) then
    add_result( GIS_INI_PARAM_FONTSTYLE_ITALIC    ) ;
  if GisTestFontStyle( TGIS_FontStyle.Underline, _value ) then
    add_result( GIS_INI_PARAM_FONTSTYLE_UNDERLINE ) ;
  if GisTestFontStyle( TGIS_FontStyle.StrikeOut, _value ) then
    add_result( GIS_INI_PARAM_FONTSTYLE_STRIKEOUT ) ;
  Result := res ;
end ;

function ParamNorthArrowStyle(
  const _value   : String ;
  const _default : TGIS_ControlNorthArrowStyle
) : TGIS_ControlNorthArrowStyle ;
var
  value : String ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  //? move constants to resources
  value := UpperCase( _value ) ;
  if value = 'ARROW2' then
    Result := TGIS_ControlNorthArrowStyle.Arrow2
  else
  if value = 'NEEDLE1' then
    Result := TGIS_ControlNorthArrowStyle.Needle1
  else
  if value = 'NEEDLE2' then
    Result := TGIS_ControlNorthArrowStyle.Needle2
  else
  if value = 'NEEDLE3' then
    Result := TGIS_ControlNorthArrowStyle.Needle3
  else
  if value = 'ROSE1' then
    Result := TGIS_ControlNorthArrowStyle.Rose1
  else
  if value = 'ROSE2' then
    Result := TGIS_ControlNorthArrowStyle.Rose2
  else
  if value = 'ROSE3' then
    Result := TGIS_ControlNorthArrowStyle.Rose3
  else
  if value = 'DISK1' then
    Result := TGIS_ControlNorthArrowStyle.Disk1
  else
  if value = 'DISK2' then
    Result := TGIS_ControlNorthArrowStyle.Disk2
  else
  if value = 'DISK3' then
    Result := TGIS_ControlNorthArrowStyle.Disk3
  else
  if value = 'TRIANGLE1' then
    Result := TGIS_ControlNorthArrowStyle.Triangle1
  else
    Result := TGIS_ControlNorthArrowStyle.Arrow1 ;
end ;

function ConstructParamNorthArrowStyle(
  const _value : TGIS_ControlNorthArrowStyle
) : String ;
begin
  case _value of
    TGIS_ControlNorthArrowStyle.Arrow2    : Result := 'Arrow2'    ;
    TGIS_ControlNorthArrowStyle.Needle1   : Result := 'Needle1'   ;
    TGIS_ControlNorthArrowStyle.Needle2   : Result := 'Needle2'   ;
    TGIS_ControlNorthArrowStyle.Needle3   : Result := 'Needle3'   ;
    TGIS_ControlNorthArrowStyle.Rose1     : Result := 'Rose1'     ;
    TGIS_ControlNorthArrowStyle.Rose2     : Result := 'Rose2'     ;
    TGIS_ControlNorthArrowStyle.Rose3     : Result := 'Rose3'     ;
    TGIS_ControlNorthArrowStyle.Disk1     : Result := 'Disk1'     ;
    TGIS_ControlNorthArrowStyle.Disk2     : Result := 'Disk2'     ;
    TGIS_ControlNorthArrowStyle.Disk3     : Result := 'Disk3'     ;
    TGIS_ControlNorthArrowStyle.Triangle1 : Result := 'Triangle1' ;
  else
                                            Result := 'Arrow1' ;
  end ;
end ;

function ParamLegendIconStyle(
  const _value   : String ;
  const _default : TGIS_LegendIconStyle
) : TGIS_LegendIconStyle ;
var
  value : String ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  //? move constants to resources
  value := UpperCase( _value ) ;
  if value = 'DEFAULT' then
    Result := TGIS_LegendIconStyle.Default
  else
  if value = 'RECTANGULAR' then
    Result := TGIS_LegendIconStyle.Rectangular ;
end ;

function ConstructParamLegendIconStyle(
  const _value : TGIS_LegendIconStyle
) : String ;
begin
  case _value of
    TGIS_LegendIconStyle.Default     : result := 'Default'     ;
    TGIS_LegendIconStyle.Rectangular : result := 'Rectangular' ;
  else
                                       result := 'Default'     ;
  end ;
end ;

function ParamInteger(
  const _value   : String ;
  const _default : Integer
) : Integer ;
begin
  if _value = GIS_PARAM_NIL then begin
    Result := _default ;
    exit ;
  end ;
  if IsStringEmpty( _value ) then begin
    Result := 0 ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_RENDER ) then
    Result := GIS_RENDER_SIZE
  else if not TryStrToInt( _value, Result ) then
    Result := _default ;
end;

function ConstructParamInteger(
  const _value : Integer
) : String ;
begin
  Result := IntToStr( _value ) ;
end ;

function ParamMarker(
  const _value   : String ;
  const _default : TGIS_MarkerStyle
) : TGIS_MarkerStyle ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_MARKER_BOX ) then
    Result := TGIS_MarkerStyle.Box
  else if paramValueIsText( _value, GIS_INI_PARAM_MARKER_CIRCLE ) then
    Result := TGIS_MarkerStyle.Circle
  else if paramValueIsText( _value, GIS_INI_PARAM_MARKER_CROSS ) then
    Result := TGIS_MarkerStyle.Cross
  else if paramValueIsText( _value, GIS_INI_PARAM_MARKER_DIAGCROSS ) then
    Result := TGIS_MarkerStyle.DiagCross
  else if paramValueIsText( _value, GIS_INI_PARAM_MARKER_TRIANGLEUP ) then
    Result := TGIS_MarkerStyle.TriangleUp
  else if paramValueIsText( _value, GIS_INI_PARAM_MARKER_TRIANGLEDOWN ) then
    Result := TGIS_MarkerStyle.TriangleDown
  else if paramValueIsText( _value, GIS_INI_PARAM_MARKER_TRIANGLELEFT ) then
    Result := TGIS_MarkerStyle.TriangleLeft
  else if paramValueIsText( _value, GIS_INI_PARAM_MARKER_TRIANGLERIGHT ) then
    Result := TGIS_MarkerStyle.TriangleRight
  else
    Result := _default ;
end ;

function ConstructParamMarker(
  const _value : TGIS_MarkerStyle
) : String ;
begin
  case _value of
    TGIS_MarkerStyle.Box           :
       Result := GIS_INI_PARAM_MARKER_BOX           ;
    TGIS_MarkerStyle.Circle        :
       Result := GIS_INI_PARAM_MARKER_CIRCLE        ;
    TGIS_MarkerStyle.Cross         :
       Result := GIS_INI_PARAM_MARKER_CROSS         ;
    TGIS_MarkerStyle.DiagCross     :
       Result := GIS_INI_PARAM_MARKER_DIAGCROSS     ;
    TGIS_MarkerStyle.TriangleUp    :
       Result := GIS_INI_PARAM_MARKER_TRIANGLEUP    ;
    TGIS_MarkerStyle.TriangleDown  :
       Result := GIS_INI_PARAM_MARKER_TRIANGLEDOWN  ;
    TGIS_MarkerStyle.TriangleLeft  :
       Result := GIS_INI_PARAM_MARKER_TRIANGLELEFT  ;
    TGIS_MarkerStyle.TriangleRight :
       Result := GIS_INI_PARAM_MARKER_TRIANGLERIGHT ;
    else begin
      assert( False, 'Unsupported Case' ) ;
    end ;
  end ;
end ;

function ParamPattern(
  const _value   : String ;
  const _default : TGIS_BrushStyle
) : TGIS_BrushStyle ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_PATTERN_SOLID ) then
    Result := TGIS_BrushStyle.Solid
  else if paramValueIsText( _value, GIS_INI_PARAM_PATTERN_BDIAGONAL ) then
    Result := TGIS_BrushStyle.BDiagonal
  else if paramValueIsText( _value, GIS_INI_PARAM_PATTERN_FDIAGONAL ) then
    Result := TGIS_BrushStyle.FDiagonal
  else if paramValueIsText( _value, GIS_INI_PARAM_PATTERN_CROSS ) then
    Result := TGIS_BrushStyle.Cross
  else if paramValueIsText( _value, GIS_INI_PARAM_PATTERN_DIAGCROSS ) then
    Result := TGIS_BrushStyle.DiagCross
  else if paramValueIsText( _value, GIS_INI_PARAM_PATTERN_HORIZONTAL ) then
    Result := TGIS_BrushStyle.Horizontal
  else if paramValueIsText( _value, GIS_INI_PARAM_PATTERN_VERTICAL ) then
    Result := TGIS_BrushStyle.Vertical
  else if paramValueIsText( _value, GIS_INI_PARAM_PATTERN_TRANSPARENT ) then
    Result := TGIS_BrushStyle.Clear
  else
    Result := _default ;
end ;

function ConstructParamPattern(
  const _value : TGIS_BrushStyle
) : String  ;
begin
  case _value of
    TGIS_BrushStyle.Solid      :
      Result := GIS_INI_PARAM_PATTERN_SOLID       ;
    TGIS_BrushStyle.BDiagonal  :
      Result := GIS_INI_PARAM_PATTERN_BDIAGONAL   ;
    TGIS_BrushStyle.FDiagonal  :
      Result := GIS_INI_PARAM_PATTERN_FDIAGONAL   ;
    TGIS_BrushStyle.Cross      :
      Result := GIS_INI_PARAM_PATTERN_CROSS       ;
    TGIS_BrushStyle.DiagCross  :
      Result := GIS_INI_PARAM_PATTERN_DIAGCROSS   ;
    TGIS_BrushStyle.Horizontal :
      Result := GIS_INI_PARAM_PATTERN_HORIZONTAL  ;
    TGIS_BrushStyle.Vertical   :
      Result := GIS_INI_PARAM_PATTERN_VERTICAL    ;
    TGIS_BrushStyle.Clear      :
      Result := GIS_INI_PARAM_PATTERN_TRANSPARENT ;
    else begin
      assert( False, 'Unsupported Case' ) ;
    end ;
  end ;
end ;

function ParamSymbol(
  const _value   : String              ;
  const _default : TGIS_SymbolAbstract
) : TGIS_SymbolAbstract ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  Result := SymbolList.Prepare( _value ) ;
end ;

function ParamPen(
  const _value   : String ;
  const _default : TGIS_PenStyle
) : TGIS_PenStyle ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_PEN_SOLID ) then
    Result := TGIS_PenStyle.Solid
  else if paramValueIsText( _value, GIS_INI_PARAM_PEN_DASH ) then
    Result := TGIS_PenStyle.Dash
  else if paramValueIsText( _value, GIS_INI_PARAM_PEN_DOT ) then
    Result := TGIS_PenStyle.Dot
  else if paramValueIsText( _value, GIS_INI_PARAM_PEN_DASHDOT ) then
    Result := TGIS_PenStyle.DashDot
  else if paramValueIsText( _value, GIS_INI_PARAM_PEN_DASHDOTDOT ) then
    Result := TGIS_PenStyle.DashDotDot
  else if paramValueIsText( _value, GIS_INI_PARAM_PEN_CLEAR ) then
    Result := TGIS_PenStyle.Clear
  else
    Result := _default ;
end ;

function ConstructParamPen(
  const _value : TGIS_PenStyle
) : String ;
begin
  case _value of
    TGIS_PenStyle.Solid      :
      Result := GIS_INI_PARAM_PEN_SOLID      ;
    TGIS_PenStyle.Dash       :
      Result := GIS_INI_PARAM_PEN_DASH       ;
    TGIS_PenStyle.Dot        :
      Result := GIS_INI_PARAM_PEN_DOT        ;
    TGIS_PenStyle.DashDot    :
      Result := GIS_INI_PARAM_PEN_DASHDOT    ;
    TGIS_PenStyle.DashDotDot :
      Result := GIS_INI_PARAM_PEN_DASHDOTDOT ;
    TGIS_PenStyle.Clear      :
      Result := GIS_INI_PARAM_PEN_CLEAR      ;
    else begin
      assert( False, 'Unsupported Case' ) ;
    end ;
  end ;
end ;

function ParamFloat(
  const _value   : String ;
  const _default : Double
) : Double ;
begin
  if _value = GIS_PARAM_NIL then begin
    Result := _default
  end
  else if IsStringEmpty( _value ) then begin
    Result := 0.0
  end
  else begin
    try
      Result := DotStrToFloat( _value ) ;
    except
      Result := _default ;
    end ;
  end ;
end ;

function ConstructParamFloat(
  const _value : Double
) : String ;
begin
  Result := DotFloatToStr( _value ) ;
end ;

function ParamString(
  const _value   : String ;
  const _default : String
) : String ;
begin
  Result := ParamString( _value, _default, True ) ;
end;

function ParamString(
  const _value          : String ;
  const _default        : String ;
  const _emptyAsDefault : Boolean
) : String ;
var
  i      : Integer ;
  ilen   : Integer ;
  state  : Integer ;
  c      : Char    ;
  ares   : array of Char ;
  ires   : Integer ;
  touch  : Boolean ;
begin
  if _value = GIS_PARAM_NIL then begin
    Result := _default ;
    exit ;
  end ;

  if IsStringEmpty( _value ) then begin
    if _emptyAsDefault then
      Result := _default
    else
      Result := ''
  end
  else begin
    ilen := StringLast( _value ) ;
    SetLength( ares, ilen + 1 ) ;

    ires := 0 ;
    state := 0 ;
    touch := False ;
    for i:= StringFirst to ilen do begin
      c := _value[ i ] ;
      case state of
        0 : if      ( i = StringFirst ) and ( c = '@' )
                                then begin
                                  state := 2 ;
                                end
            else if c = '\'     then begin
                                  state := 1 ;
                                end
            else                begin
                                  ares[ ires ] := c   ;
                                  inc( ires ) ;
                                end ;
        1 : if      c = 'n'     then begin
                                  ares[ ires ] := #13 ;
                                  inc( ires ) ;
                                  ares[ ires ] := #10 ;
                                  inc( ires ) ;
                                  state := 0 ;
                                  touch := True ;
                                end
            else if c = '\'     then begin
                                  ares[ ires ] := '\' ;
                                  inc( ires ) ;
                                  state := 0 ;
                                  touch := True ;
                                end
            else                begin
                                  ares[ ires ] := '\' ;
                                  inc( ires ) ;
                                  ares[ ires ] := c   ;
                                  inc( ires ) ;
                                  state := 0 ;
                                end ;
        2 :                     begin
                                  ares[ ires ] := c   ;
                                  inc( ires ) ;
                                  touch := True ;
                                end ;
        else                  begin
                                assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                              end ;
      end ;
    end ;
    if state = 1 then begin
      ares[ ires ] := '\' ;
      inc( ires ) ;
    end ;

    if touch then begin
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          Result := new String( ares, 0, ires ) ;
        {$ENDIF}
        {$IFDEF CLR}
          SetLength( ares, ires ) ;
          Result := new String(  ares ) ;
        {$ENDIF}
        {$IFDEF ISLAND}
          SetLength( ares, ires ) ;
          Result := String.FromCharArray( ares ) ;
        {$ENDIF}
      {$ELSE}
        Result := Copy( PChar(@(ares[0])), 0, ires ) ;
      {$ENDIF}
    end
    else
      Result := _value ;

  end ;

end ;

function ConstructParamString(
  const _value : String
) : String ;
var
  i     : Integer ;
  ilen  : Integer ;
  c     : Char    ;
  ares  : array of Char ;
  ires  : Integer ;
  bcr   : Boolean ;
begin
  ilen := StringLast( _value ) ;
  SetLength( ares, 2 * ( ilen + 1 - StringFirst ) ) ;

  ires := 0 ;

  for i := StringFirst to ilen do begin
    c := _value[ i ] ;
    if      c = #13 then begin
                      ares[ ires ] := '\'  ;
                      inc( ires ) ;
                      ares[ ires ] := 'n'  ;
                      inc( ires ) ;
                      bcr := true ;
                    end
    else if c = #10 then begin
                      if not bcr then begin
                        ares[ ires ] := '\'  ;
                        inc( ires ) ;
                        ares[ ires ] := 'n'  ;
                        inc( ires ) ;
                        bcr := false ;
                      end;
                    end
    else if c = '\' then begin
                      ares[ ires ] := c   ;
                      inc( ires ) ;
                      ares[ ires ] := '\'  ;
                      inc( ires ) ;
                      bcr := false ;
                    end
    else            begin
                      ares[ ires ] := c  ;
                      inc( ires ) ;
                      bcr := false ;
                    end ;
  end ;

  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      Result := new String( ares, 0, ires ) ;
    {$ENDIF}
    {$IFDEF CLR}
      SetLength( ares, ires ) ;
      Result := new String( ares ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      SetLength( ares, ires ) ;
      Result := String.FromCharArray( ares ) ;
    {$ENDIF}
  {$ELSE}
    Result := Copy( PChar(@(ares[0])), 0, ires ) ;
  {$ENDIF}
end ;

function ParamPosition (
  const _value   : String ;
  const _default : TGIS_LabelPositions
) : TGIS_LabelPositions ;
var
  i     : Integer             ;
  res   : TGIS_LabelPositions ;
  tkn   : TGIS_Tokenizer ;
  value : String ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  res := GisGetEmptyLabelPosition ;
  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _value, [':'] ) ;
    for i := 0 to tkn.Result.Count - 1 do begin
      value := tkn.Result[i] ;

      if paramValueIsText( value, GIS_INI_PARAM_POSITION_UPLEFT ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.UpLeft )
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_UPCENTER ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.UpCenter )
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_UPRIGHT ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.UpRight )
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_MIDDLELEFT ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.MiddleLeft   )
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_MIDDLECENTER ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.MiddleCenter )
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_MIDDLERIGHT ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.MiddleRight  )
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_DOWNLEFT ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.DownLeft     )
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_DOWNCENTER ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.DownCenter   )
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_DOWNRIGHT ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.DownRight    )
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_ANY ) then begin
        res := GisAddLabelPosition( res, TGIS_LabelPosition.UpCenter     ) ;
        res := GisAddLabelPosition( res, TGIS_LabelPosition.UpRight      ) ;
        res := GisAddLabelPosition( res, TGIS_LabelPosition.MiddleLeft   ) ;
        res := GisAddLabelPosition( res, TGIS_LabelPosition.MiddleCenter ) ;
        res := GisAddLabelPosition( res, TGIS_LabelPosition.MiddleRight  ) ;
        res := GisAddLabelPosition( res, TGIS_LabelPosition.DownLeft     ) ;
        res := GisAddLabelPosition( res, TGIS_LabelPosition.DownCenter   ) ;
        res := GisAddLabelPosition( res, TGIS_LabelPosition.DownRight    ) ;
      end
      else if paramValueIsText( value, GIS_INI_PARAM_POSITION_FLOW ) then
        res := GisAddLabelPosition( res, TGIS_LabelPosition.Flow ) ;
    end ;
  finally
    FreeObject( tkn ) ;
  end ;

  Result := res ;
end ;

function ConstructParamPosition(
  const _value : TGIS_LabelPositions
) : String ;
var
  res : String ;

  procedure add_result( const _str : String ) ;
  begin
    if not IsStringEmpty( res ) then res := res + ':' ;
    res := res + _str ;
  end ;
begin
  res := '' ;

  if GisTestLabelPosition( TGIS_LabelPosition.UpLeft,       _value ) then
     add_result( GIS_INI_PARAM_POSITION_UPLEFT       ) ;
  if GisTestLabelPosition( TGIS_LabelPosition.UpCenter,     _value ) then
     add_result( GIS_INI_PARAM_POSITION_UPCENTER     ) ;
  if GisTestLabelPosition( TGIS_LabelPosition.UpRight,      _value ) then
     add_result( GIS_INI_PARAM_POSITION_UPRIGHT      ) ;
  if GisTestLabelPosition( TGIS_LabelPosition.MiddleLeft,   _value ) then
     add_result( GIS_INI_PARAM_POSITION_MIDDLELEFT   ) ;
  if GisTestLabelPosition( TGIS_LabelPosition.MiddleCenter, _value ) then
     add_result( GIS_INI_PARAM_POSITION_MIDDLECENTER ) ;
  if GisTestLabelPosition( TGIS_LabelPosition.MiddleRight,  _value ) then
     add_result( GIS_INI_PARAM_POSITION_MIDDLERIGHT  ) ;
  if GisTestLabelPosition( TGIS_LabelPosition.DownLeft,     _value ) then
     add_result( GIS_INI_PARAM_POSITION_DOWNLEFT     ) ;
  if GisTestLabelPosition( TGIS_LabelPosition.DownCenter,   _value ) then
     add_result( GIS_INI_PARAM_POSITION_DOWNCENTER   ) ;
  if GisTestLabelPosition( TGIS_LabelPosition.DownRight,    _value ) then
     add_result( GIS_INI_PARAM_POSITION_DOWNRIGHT    ) ;
  if GisTestLabelPosition( TGIS_LabelPosition.Flow,         _value ) then
     add_result( GIS_INI_PARAM_POSITION_FLOW         ) ;

  Result := res ;
end ;

function ParamGround(
  const _value   : String;
  const _default : TGIS_3DGroundType
) : TGIS_3DGroundType ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_GROUND_ONDEM ) then
    Result := TGIS_3DGroundType.OnDem
  else if paramValueIsText( _value, GIS_INI_PARAM_GROUND_ABOVEDEM ) then
    Result := TGIS_3DGroundType.AboveDem
  else if paramValueIsText( _value, GIS_INI_PARAM_GROUND_ABOVEZERO ) then
    Result := TGIS_3DGroundType.AboveZero
  else
    Result := _default ;
end ;

function ConstructParamGround(
  const _value : TGIS_3DGroundType
) : String ;
begin
  if      _value = TGIS_3DGroundType.OnDem     then
    Result := GIS_INI_PARAM_GROUND_ONDEM
  else if _value = TGIS_3DGroundType.AboveDem  then
    Result := GIS_INI_PARAM_GROUND_ABOVEDEM
  else
    Result := GIS_INI_PARAM_GROUND_ABOVEZERO ;
end ;

function ParamNormalized(
  const _value   : String;
  const _default : TGIS_3DNormalizationType
) : TGIS_3DNormalizationType ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  // read all boolean values
  if paramValueIsText( _value, GIS_INI_PARAM_BOOLEAN_YES ) then
    Result := TGIS_3DNormalizationType.Max
  else if paramValueIsText( _value, GIS_INI_PARAM_BOOLEAN_NO ) then
    Result := TGIS_3DNormalizationType.Off
  else if paramValueIsText( _value, GIS_INI_PARAM_BOOLEAN_TRUE ) then
    Result := TGIS_3DNormalizationType.Max
  else if paramValueIsText( _value, GIS_INI_PARAM_BOOLEAN_FALSE ) then
    Result := TGIS_3DNormalizationType.Off
  else if paramValueIsText( _value, GIS_INI_PARAM_BOOLEAN_1 ) then
    Result := TGIS_3DNormalizationType.Max
  else if paramValueIsText( _value, GIS_INI_PARAM_BOOLEAN_0 ) then
    Result := TGIS_3DNormalizationType.Off
  else if paramValueIsText( _value, GIS_INI_PARAM_NORMALIZED_OFF ) then
    Result := TGIS_3DNormalizationType.Off
  else if paramValueIsText( _value, GIS_INI_PARAM_NORMALIZED_MAX ) then
    Result := TGIS_3DNormalizationType.Max
  else if paramValueIsText( _value, GIS_INI_PARAM_NORMALIZED_RANGE ) then
    Result := TGIS_3DNormalizationType.Range
  else
    Result := _default ;
end ;

function ConstructParamNormalized(
  const _value : TGIS_3DNormalizationType
) : String ;
begin
  if      _value = TGIS_3DNormalizationType.Off     then
    Result := GIS_INI_PARAM_NORMALIZED_OFF
  else if _value = TGIS_3DNormalizationType.Max  then
    Result := GIS_INI_PARAM_NORMALIZED_MAX
  else
    Result := GIS_INI_PARAM_NORMALIZED_RANGE ;
end ;

function ParamBasement(
  const _value   : String              ;
  const _default : TGIS_3DBasementType
) : TGIS_3DBasementType ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_BASEMENT_OFF    ) then
    Result := TGIS_3DBasementType.Off
  else if paramValueIsText( _value, GIS_INI_PARAM_BASEMENT_LOWEST ) then
    Result := TGIS_3DBasementType.Lowest
  else
    Result := _default ;
end ;

function ConstructParamBasement(
  const _value   : TGIS_3DBasementType
) : String ;
begin
  if      _value = TGIS_3DBasementType.Lowest  then
    Result := GIS_INI_PARAM_BASEMENT_LOWEST
  else
    Result := GIS_INI_PARAM_BASEMENT_OFF      ;
end ;

function Param3DLayerType(
  const _value   : String;
  const _default : TGIS_3DLayerType
) : TGIS_3DLayerType ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_3DLAYER_DEM ) then
    Result := TGIS_3DLayerType.Dem
  else if paramValueIsText( _value, GIS_INI_PARAM_3DLAYER_SHAPES ) then
    Result := TGIS_3DLayerType.Shapes
  else
    Result := TGIS_3DLayerType.Off
end ;

function ConstructParam3DLayerType(
  const _value   : TGIS_3DLayerType
) : String ;
begin
  case _value of
    TGIS_3DLayerType.Dem    :
      Result := GIS_INI_PARAM_3DLAYER_DEM     ;
    TGIS_3DLayerType.Shapes :
      Result := GIS_INI_PARAM_3DLAYER_SHAPES  ;
    else
      Result := GIS_INI_PARAM_3DLAYER_OFF     ;
  end ;
end ;

function ParamInterpretation(
  const _value   : String;
  const _default : TGIS_LayerPixelInterpretation
) : TGIS_LayerPixelInterpretation ;
begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_INTERPRETATION_GRID ) then
    Result := TGIS_LayerPixelInterpretation.Grid
  else if paramValueIsText( _value, GIS_INI_INTERPRETATION_PIXEL ) then
    Result := TGIS_LayerPixelInterpretation.Pixel
  else
    Result := TGIS_LayerPixelInterpretation.Default
end ;

function ConstructParamInterpretation(
  const _value   : TGIS_LayerPixelInterpretation
) : String ;
begin
  case _value of
    TGIS_LayerPixelInterpretation.Grid    :
      Result := GIS_INI_INTERPRETATION_GRID     ;
    TGIS_LayerPixelInterpretation.Pixel :
      Result := GIS_INI_INTERPRETATION_PIXEL  ;
    else
      Result := GIS_INI_INTERPRETATION_DEFAULT     ;
  end ;
end ;

procedure SplitParamAsText(
  const _text   : String ;
  var   _type   : String ;
  var   _value1 : String ;
  var   _value2 : String ;
  var   _value3 : String ;
  var   _value4 : String ;
  var   _value5 : String
) ;
var
  tkn : TGIS_Tokenizer ;
begin
  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _text, [':'] );

    if tkn.Result.Count > 0 then
      _type := UpperCase( tkn.Result[0] )
    else
      _type := '' ;

    if tkn.Result.Count > 1 then
      _value1 := tkn.Result[1]
    else
      _value1 := '' ;

    if tkn.Result.Count > 2 then
      _value2 := tkn.Result[2]
    else
      _value2 := '' ;

    if tkn.Result.Count > 3 then
      _value3 := tkn.Result[3]
    else
      _value3 := '' ;

    if tkn.Result.Count > 4 then
      _value4 := tkn.Result[4]
    else
      _value4 := '' ;

    if tkn.Result.Count > 5 then
      _value5 := tkn.Result[5]
    else
      _value5 := '' ;

  finally
    FreeObject( tkn ) ;
  end ;
end;

procedure SplitParamAsText(
  const _text   : String ;
  var   _type   : String ;
  var   _value1 : String ;
  var   _value2 : String
) ;
var
  tkn : TGIS_Tokenizer ;
begin
  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _text, [':'] );

    if tkn.Result.Count > 0 then
      _type := UpperCase( tkn.Result[0] )
    else
      _type := '' ;

    if tkn.Result.Count > 1 then
      _value1 := tkn.Result[1]
    else
      _value1 := '' ;

    if tkn.Result.Count > 2 then
      _value2 := tkn.Result[2]
    else
      _value2 := '' ;
  finally
    FreeObject( tkn ) ;
  end ;
end;


function ConstructParamAsText(
  const _type   : String ;
  const _value1 : String ;
  const _value2 : String ;
  const _value3 : String ;
  const _value4 : String ;
  const _value5 : String
) : String ;
begin
  Result := _type ;

  if not IsStringEmpty( _value1 ) then
    Result := Result + ':' + _value1 ;
  if not IsStringEmpty( _value2 ) then
    Result := Result + ':' + _value2 ;
  if not IsStringEmpty( _value3 ) then
    Result := Result + ':' + _value3 ;
  if not IsStringEmpty( _value4 ) then
    Result := Result + ':' + _value4 ;
  if not IsStringEmpty( _value5 ) then
    Result := Result + ':' + _value5 ;
end;

function ConstructParamAsText(
  const _type   : String ;
  const _value1 : String ;
  const _value2 : String
) : String ;
begin
  Result := _type ;

  if not IsStringEmpty( _value1 ) then
    Result := Result + ':' + _value1 ;
  if not IsStringEmpty( _value2 ) then
    Result := Result + ':' + _value2 ;
end;

procedure SplitNumberAsText(
  const _text   : String ;
  var   _type   : String ;
  var   _field  : String ;
  var   _value  : String ;
  var   _unit   : String
) ;
var
  tkn : TGIS_Tokenizer ;
  sval1 : String  ;
  sval2 : String  ;
begin
  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _text, [':'] );

    if tkn.Result.Count > 0 then
      _type := UpperCase( tkn.Result[0] )
    else
      _type := '' ;

    if tkn.Result.Count > 1 then
      sval1 := tkn.Result[1]
    else
      sval1 := '' ;

    if tkn.Result.Count > 2then
      sval2 := tkn.Result[2]
    else
      sval2 := '' ;
  finally
    FreeObject( tkn ) ;
  end ;

  if _type = GIS_PARAMTXT_TYPE_FIELD then begin
    _field := sval1 ;

    split_size_val( sval2, _value, _unit ) ;
  end
  else
  if ( _type = GIS_PARAMTXT_TYPE_SIZE  ) or
     ( _type = GIS_PARAMTXT_TYPE_ANGLE ) then begin
    _field := '' ;
    split_size_val( sval1, _value, _unit ) ;
  end
  else
  if _type = GIS_PARAMTXT_TYPE_RENDERER then begin
  end
  else
  begin
    assert( false, GIS_RS_ERR_BAD_CALL );
    exit ;
  end;
end;

function ConstructNumberAsText(
  const _field  : String ;
  const _value  : String ;
  const _unit   : String
) : String ;
begin
  if IsStringEmpty( _value ) then begin
    assert( false, GIS_RS_ERR_BAD_CALL );
    exit ;
  end;

  Result := '' ;
  if not IsStringEmpty( _field ) then begin
    Result := Result + GIS_PARAMTXT_TYPE_FIELD + ':' + Trim( _field ) ;
  end
  else begin
    Result := Result + GIS_PARAMTXT_TYPE_SIZE ;
  end;

  if not IsStringEmpty( _value ) then begin
    Result := Result + ':' + Trim( _value ) ;
    if not IsStringEmpty( _unit ) then begin
      Result := Result + ' ' + Trim( _unit ) ;
    end;
  end;
end;

function ParamOffsetPosition(
  const _value   : String ;
  const _default : TGIS_OffsetPosition
) : TGIS_OffsetPosition ;

begin
  if IsStringEmpty( _value ) or ( _value = GIS_PARAM_NIL ) then begin
    Result := _default ;
    exit ;
  end ;

  if paramValueIsText( _value, GIS_INI_PARAM_POSITION_UPLEFT ) then
    Result := TGIS_OffsetPosition.UpLeft
  else if paramValueIsText( _value, GIS_INI_PARAM_POSITION_UPRIGHT ) then
    Result := TGIS_OffsetPosition.UpRight
  else if paramValueIsText( _value, GIS_INI_PARAM_POSITION_DOWNLEFT ) then
    Result := TGIS_OffsetPosition.DownLeft
  else if paramValueIsText( _value, GIS_INI_PARAM_POSITION_DOWNRIGHT ) then
    Result := TGIS_OffsetPosition.DownRight
  else
    Result := TGIS_OffsetPosition.DownRight ;
end ;

function ConstructParamOffsetPosition(
  const _value : TGIS_OffsetPosition
) : String ;
begin
  case _value of
    TGIS_OffsetPosition.UpLeft     : Result := GIS_INI_PARAM_POSITION_UPLEFT ;
    TGIS_OffsetPosition.UpRight    : Result := GIS_INI_PARAM_POSITION_UPRIGHT ;
    TGIS_OffsetPosition.DownLeft   : Result := GIS_INI_PARAM_POSITION_DOWNLEFT ;
    TGIS_OffsetPosition.DownRight  : Result := GIS_INI_PARAM_POSITION_DOWNRIGHT ;
  end ;
end ;

{$ENDREGION}

//==================================== END =====================================
end.
