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
  Encapsulation of AutoCAD DWG file reader.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileDWG ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileDWG"'}
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

{$IFDEF DCC}
uses
  System.Classes,
  Variants,
  Generics.Collections,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

type
  /// <summary>
  ///   Flags describing reference of style property value.
  /// </summary>
  {#gendoc:hide}
  T_FileDwg_Flag = {$IFDEF OXYGENE} assembly {$ENDIF}(
    /// <summary>
    ///   Current value is used
    /// </summary>
    Default = 0,
    /// <summary>
    ///   Block value is used
    /// </summary>
    ByBlock = 1,
    /// <summary>
    ///   Layer value is used
    /// </summary>
    ByLayer = 2,
    /// <summary>
    ///   Reference value is used
    /// </summary>
    ByReference = 3
  ) ;

  /// <summary>
  ///   Types of dwg entities.
  /// </summary>
  {#gendoc:hide}
  T_FileDwg_EntityType = {$IFDEF OXYGENE} assembly {$ENDIF}(
    /// <summary>
    ///   Arc entity
    /// </summary>
    Arc,
    /// <summary>
    ///   Attrib entity
    /// </summary>
    Attrib,
    /// <summary>
    ///   AttDef entity
    /// </summary>
    AttDef,
    /// <summary>
    ///   Block entity
    /// </summary>
    Block,
    /// <summary>
    ///   Circle entity
    /// </summary>
    Circle,
    /// <summary>
    ///   Dimension linear entity
    /// </summary>
    DimensionLinear,
    /// <summary>
    ///   Dimension aligned entity
    /// </summary>
    DimensionAligned,
    /// <summary>
    ///   Dimension ordinate entity
    /// </summary>
    DimensionOrdinate,
    /// <summary>
    ///   Dimension diametric entity
    /// </summary>
    DimensionDiametric,
    /// <summary>
    ///   Ellipse entity
    /// </summary>
    Ellipse,
    /// <summary>
    ///   Face3D entity
    /// </summary>
    Face3D,
    /// <summary>
    ///   Hatch entity
    /// </summary>
    Hatch,
    /// <summary>
    ///   Insert entity
    /// </summary>
    Insert,
    /// <summary>
    ///   Line entity
    /// </summary>
    Line,
    /// <summary>
    ///   LWPolyline entity
    /// </summary>
    LWPolyline,
    /// <summary>
    ///   Leader entity
    /// </summary>
    Leader,
    /// <summary>
    ///   MText entity
    /// </summary>
    MText,
    /// <summary>
    ///   Point entity
    /// </summary>
    Point,
    /// <summary>
    ///   Polyline entity
    /// </summary>
    Polyline,
    /// <summary>
    ///   Ray entity
    /// </summary>
    Ray,
    /// <summary>
    ///   Solid entity
    /// </summary>
    Solid,
    /// <summary>
    ///   Spline entity
    /// </summary>
    Spline,
    /// <summary>
    ///   Text entity
    /// </summary>
    Text,
    /// <summary>
    ///   Trace entity
    /// </summary>
    Trace,
    /// <summary>
    ///   XLine entity
    /// </summary>
    XLine
  ) ;

  /// <summary>
  ///   Entity style color definition.
  /// </summary>
  {#gendoc:hide}
  T_FileDwg_Color = {$IFDEF OXYGENE} assembly {$ENDIF} record
    public
      /// <summary>
      ///   Color definition.
      /// </summary>
      Color : TGIS_Color ;
      /// <summary>
      ///   Reference flag.
      /// </summary>
      CType : T_FileDwg_Flag ;
      /// <summary>
      ///   Reference flag.
      /// </summary>
      Reference : Cardinal ;
    public
      /// <summary>
      ///   Create a color object.
      /// </summary>
      /// <param name="_ctype">
      ///   reference flag
      /// </param>
      constructor Create( const _ctype : T_FileDwg_Flag
                         ) ; overload;
      /// <summary>
      ///   Create a color object.
      /// </summary>
      /// <param name="_ctype">
      ///   reference flag
      /// </param>
      /// <param name="_handle">
      ///   color reference
      /// </param>
      constructor Create( const _ctype  : T_FileDwg_Flag ;
                          const _handle : Cardinal
                         ) ; overload;

      /// <summary>
      ///   Create a color object.
      /// </summary>
      /// <param name="_color">
      ///   color value
      /// </param>
      constructor Create( const _color : TGIS_Color
                         ) ; overload;
      /// <summary>
      ///   Create a color object.
      /// </summary>
      /// <param name="_r">
      ///   Red value of color
      /// </param>
      /// <param name="_g">
      ///   Green value of color
      /// </param>
      /// <param name="_b">
      ///   Blue value of color
      /// </param>
      constructor Create( const _r     : Byte ;
                          const _g     : Byte ;
                          const _b     : Byte
                         ) ; overload;
  end ;

  /// <summary>
  ///   Entity style pen definition.
  /// </summary>
  {#gendoc:hide}
  T_FileDwg_Pen = {$IFDEF OXYGENE} assembly {$ENDIF} record
    public
      /// <summary>
      ///   Pen definition.
      /// </summary>
      Pen   : TGIS_PenStyle ;
      /// <summary>
      ///   Reference flag.
      /// </summary>
      PType : T_FileDwg_Flag ;
    public
      /// <summary>
      ///   Create a pen object.
      /// </summary>
      /// <param name="_ctype">
      ///   reference flag
      /// </param>
      constructor Create( const _ctype : T_FileDwg_Flag
                         ) ; overload;
      /// <summary>
      ///   Create a pen object.
      /// </summary>
      /// <param name="_pen">
      ///   pen value
      /// </param>
      constructor Create( const _pen : TGIS_PenStyle
                         ) ; overload;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Entity style width definition.
  /// </summary>
  T_FileDwg_Width = {$IFDEF OXYGENE} assembly {$ENDIF} record
    public
      /// <summary>
      ///   Width definition.
      /// </summary>
      Width : Double ;
      /// <summary>
      ///   Reference flag.
      /// </summary>
      WType : T_FileDwg_Flag ;
    public
      /// <summary>
      ///   Create a width object.
      /// </summary>
      /// <param name="_ctype">
      ///   reference flag
      /// </param>
      constructor Create( const _ctype : T_FileDwg_Flag
                         ) ; overload;
      /// <summary>
      ///   Create a width object.
      /// </summary>
      /// <param name="_width">
      ///   width value
      /// </param>
      constructor Create( const _width : Double
                         ) ; overload;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Entity style definition.
  /// </summary>
  T_FileDwg_Style = {$IFDEF OXYGENE} assembly {$ENDIF} record
    public
      /// <summary>
      ///   Color definition.
      /// </summary>
      Color : T_FileDwg_Color ;
      /// <summary>
      ///   Width definition
      /// </summary>
      Width : T_FileDwg_Width ;
      /// <summary>
      ///   Pen definition
      /// </summary>
      Pen   : T_FileDwg_Pen ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Entity drawing style definition.
  /// </summary>
  T_FileDwg_DrawStyle  = {$IFDEF OXYGENE} assembly {$ENDIF} record
    public
      /// <summary>
      ///   Width definition
      /// </summary>
      Width : Integer ;
      /// <summary>
      ///   Pen definition
      /// </summary>
      Pen   : TGIS_PenStyle ;
      /// <summary>
      ///   Color definition.
      /// </summary>
      Color : TGIS_Color ;
      /// <summary>
      ///   Reference flag.
      /// </summary>
      SType : T_FileDwg_Flag ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Layer definition.
  /// </summary>
  T_FileDwg_Layer = {$IFDEF OXYGENE} assembly {$ENDIF} class
    public
      /// <summary>
      ///   Layer name
      /// </summary>
      Name        : String ;
      /// <summary>
      ///   Style definition
      /// </summary>
      Style       : T_FileDwg_Style ;
      /// <summary>
      ///   Visibility flag
      /// </summary>
      Visible     : Boolean ;
      /// <summary>
      ///   Handle to native layer
      /// </summary>
      NativeLayer : TObject ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Entity definition.
  /// </summary>
  T_FileDwg_Entity = {$IFDEF OXYGENE} assembly {$ENDIF} class
    public
      /// <summary>
      ///   Entity Handle
      /// </summary>
      Handle : UInt64 ;
      /// <summary>
      ///   Entity type
      /// </summary>
      eType  : T_FileDwg_EntityType ;
      /// <summary>
      ///   Style definition
      /// </summary>
      Style  : T_FileDwg_Style ;
      /// <summary>
      ///   Handle to layer
      /// </summary>
      Layer  : T_FileDwg_Layer ;
      /// <summary>
      ///   Handle to parent entity
      /// </summary>
      Parent : T_FileDwg_Entity ;
      /// <summary>
      ///   EED
      /// </summary>
      ExtData : TStringList ;
    public
      /// <summary>
      ///   Create an entity object.
      /// </summary>
      /// <param name="_handle">
      ///   handle value
      /// </param>
      /// <param name="_eType">
      ///   entity type
      /// </param>
      constructor Create( const _handle : UInt64 ;
                          const _eType  : T_FileDwg_EntityType
                         ) ; virtual;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroy an entity instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}
      /// <summary>
      ///   Get drawing style of entity
      /// </summary>
      /// <param name="_model">
      ///   model handle
      /// </param>
      /// <returns>
      ///   style.
      /// </returns>
      function GetDrawStyle( const _model : TObject ) : T_FileDwg_DrawStyle ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Block definition.
  /// </summary>
  T_FileDwg_Block = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Block name
      /// </summary>
      Name      : String ;
      /// <summary>
      ///   Base point of block
      /// </summary>
      BasePoint : TGIS_Point3D ;
      /// <summary>
      ///   List of entities inside a block
      /// </summary>
      Entities  : TList<T_FileDwg_Entity> ;
    public
      /// <inheritdoc/>
      constructor Create( const _handle : UInt64 ;
                          const _eType  : T_FileDwg_EntityType
                         ) ; override;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroy a block instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}
      /// <summary>
      ///   Add entity to block
      /// </summary>
      /// <param name="_e">
      ///   entity handle
      /// </param>
      procedure AddEntity( const _e : T_FileDwg_Entity ) ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Model definition.
  /// </summary>
  T_FileDwg_Model = {$IFDEF OXYGENE} assembly {$ENDIF} class
    {$IFNDEF OXYGENE} protected {$ELSE} assembly {$ENDIF}
      /// <summary>
      ///   List of blocks
      /// </summary>
      Blocks      : TDictionary<UInt64, T_FileDwg_Block> ;
      /// <summary>
      ///   Model handle value
      /// </summary>
      Handle      : UInt64 ;
      /// <summary>
      ///   List of reference colors
      /// </summary>
      Colors      : TDictionary<Cardinal, T_FileDwg_Color> ;
      /// <summary>
      ///   List of palette colors
      /// </summary>
      Palette      : TList<TGIS_Color> ;
    public
      /// <summary>
      ///   Model extent value
      /// </summary>
      Extent      : TGIS_Extent3D ;
      /// <summary>
      ///   First entity handle
      /// </summary>
      FirstEntity : UInt64 ;
      /// <summary>
      ///   Last entity handle
      /// </summary>
      LastEntity  : UInt64 ;
      /// <summary>
      ///   List of layers
      /// </summary>
      Layers      : TDictionary<String, T_FileDwg_Layer> ;
    public
      /// <summary>
      ///   Create a model object.
      /// </summary>
      constructor Create ;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroys an instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}

      /// <summary>
      ///   Add a block
      /// </summary>
      /// <param name="_blk">
      ///   block handle
      /// </param>
      /// <returns>
      ///   True if block was added
      /// </returns>
      function  AddBlock     ( const _blk    : T_FileDwg_Block  ) : Boolean ;

      /// <summary>
      ///   Add a layer
      /// </summary>
      /// <param name="_la">
      ///   layer handle
      /// </param>
      procedure AddLayer     ( const _la     : T_FileDwg_Layer  ) ;

      /// <summary>
      ///   Add a reference color
      /// </summary>
      /// <param name="_handle">
      ///   color handle
      /// </param>
      /// <param name="_color">
      ///   color data
      /// </param>
      procedure AddColor     ( const _handle : Cardinal ;
                               const _color  : T_FileDwg_Color
                              ) ;

      /// <summary>
      ///   Add a palette color
      /// </summary>
      /// <param name="_color">
      ///   color data
      /// </param>
      procedure AddPaletteColor  ( const _color  : TGIS_Color
                                  ) ;

      /// <summary>
      ///   Get a palette color
      /// </summary>
      /// <param name="_idx">
      ///   color index
      /// </param>
      /// <returns>
      ///   color
      /// </returns>
      function  GetPaletteColor    ( const _idx   : Integer
                                   ) : TGIS_Color ;

      /// <summary>
      ///   Find a color by handle
      /// </summary>
      /// <param name="_handle">
      ///   color handle
      /// </param>
      /// <param name="_color">
      ///   color value
      /// </param>
      /// <returns>
      ///   True if found
      /// </returns>
      function  FindColor    ( const _handle   : Cardinal;
                                 var _color    : T_FileDwg_Color
                              ) : Boolean ;

      /// <summary>
      ///   Find a layer by name
      /// </summary>
      /// <param name="_name">
      ///   layer name
      /// </param>
      /// <returns>
      ///   Layer handle if found
      /// </returns>
      function  FindLayer    ( const _name   : String         ) : T_FileDwg_Layer ;

      /// <summary>
      ///   Set a model block
      /// </summary>
      /// <param name="_blk">
      ///   block handle
      /// </param>
      procedure SetModelBlock( const _blk    : T_FileDwg_Block  ) ;

      /// <summary>
      ///   Find a block by handle
      /// </summary>
      /// <param name="_handle">
      ///   block handle
      /// </param>
      /// <returns>
      ///   block handle if found
      /// </returns>
      function  GetBlock     ( const _handle : UInt64       ) : T_FileDwg_Block ;

      /// <summary>
      ///   Get a model block
      /// </summary>
      /// <returns>
      ///   Block handle
      /// </returns>
      function  GetModelBlock : T_FileDwg_Block ;

  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Point entity definition.
  /// </summary>
  T_FileDwg_Point = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Point coordinates
      /// </summary>
      Ptg       : TGIS_Point3D ;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion : TGIS_Point3D ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Line entity definition.
  /// </summary>
  T_FileDwg_Line = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Start point coordinates
      /// </summary>
      Ptg1      : TGIS_Point3D ;
      /// <summary>
      ///   End point coordinates
      /// </summary>
      Ptg2      : TGIS_Point3D ;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion : TGIS_Point3D ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Line entity definition.
  /// </summary>
  T_FileDwg_Dimension = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Start point coordinates of first extended line
      /// </summary>
      Ptg1      : TGIS_Point3D ;
      /// <summary>
      ///   Start point coordinates of second extended line
      /// </summary>
      Ptg2      : TGIS_Point3D ;
      /// <summary>
      ///   Point coordinates of dimension line location
      /// </summary>
      Ptg3      : TGIS_Point3D ;
      /// <summary>
      ///   Midpoint coordinates of the dimension text
      /// </summary>
      PtgText   : TGIS_Point3D ;
      /// <summary>
      ///   Dimension text
      /// </summary>
      Text   : String ;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion : TGIS_Point3D ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Circle entity definition.
  /// </summary>
  T_FileDwg_Circle = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Center point coordinates
      /// </summary>
      Center    : TGIS_Point3D ;
      /// <summary>
      ///   Radius value
      /// </summary>
      Radius    : Double ;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion : TGIS_Point3D ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Ellipse entity definition.
  /// </summary>
  T_FileDwg_Ellipse = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Center point coordinates
      /// </summary>
      Center      : TGIS_Point3D ;
      /// <summary>
      ///   Axis length
      /// </summary>
      MajorAxis   : TGIS_Point3D ;
      /// <summary>
      ///   Axis ration
      /// </summary>
      AxisRatio   : Double ;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion   : TGIS_Point3D ;
      /// <summary>
      ///   Start angle value
      /// </summary>
      InitAngle   : Double ;
      /// <summary>
      ///   Stop angle value
      /// </summary>
      EndAngle    : Double ;
      /// <summary>
      ///   Is CCW direction
      /// </summary>
      IsCCW       : Boolean ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Arc entity definition.
  /// </summary>
  T_FileDwg_Arc = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Center point coordinates
      /// </summary>
      Center      : TGIS_Point3D ;
      /// <summary>
      ///   Radius value
      /// </summary>
      Radius      : Double ;
      /// <summary>
      ///   Start angle value
      /// </summary>
      StartAngle  : Double ;
      /// <summary>
      ///   Stop angle value
      /// </summary>
      EndAngle    : Double ;
      /// <summary>
      ///   Is CCW direction
      /// </summary>
      IsCCW       : Boolean ;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion   : TGIS_Point3D ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   LWPolyline entity definition.
  /// </summary>
  T_FileDwg_LWPolyline = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Line width
      /// </summary>
      Width     : Double;
      /// <summary>
      ///   Line thickness
      /// </summary>
      Thickness : Double;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion : TGIS_Point3D ;
      /// <summary>
      ///   Line flags
      /// </summary>
      Flag      : Integer ;
      /// <summary>
      ///   Bulges count
      /// </summary>
      NumBulges : Integer ;
      /// <summary>
      ///   Vertexes count
      /// </summary>
      NumVertex : Integer ;
      /// <summary>
      ///   List of Vertexes
      /// </summary>
      Vertices  : TList<TGIS_Point3D>;
    public
      /// <inheritdoc/>
      constructor Create( const _handle : UInt64 ;
                          const _eType  : T_FileDwg_EntityType
                         ) ; override;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroys an instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}
      /// <summary>
      ///   Add vertex to line
      /// </summary>
      /// <param name="_v">
      ///   vertex
      /// </param>
      procedure AddVertex( const _v : TGIS_Point3D ) ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Leader entity definition.
  /// </summary>
  T_FileDwg_Leader = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion : TGIS_Point3D ;
      /// <summary>
      ///   Vertexes count
      /// </summary>
      NumVertex : Integer ;
      /// <summary>
      ///   List of Vertexes
      /// </summary>
      Vertices  : TList<TGIS_Point3D>;
    public
      /// <inheritdoc/>
      constructor Create( const _handle : UInt64 ;
                          const _eType  : T_FileDwg_EntityType
                         ) ; override;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroys an instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}
      /// <summary>
      ///   Add vertex to line
      /// </summary>
      /// <param name="_v">
      ///   vertex
      /// </param>
      procedure AddVertex( const _v : TGIS_Point3D ) ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Text VAlign types.
  /// </summary>
  TGIS_TextVAlign = {$IFDEF OXYGENE} assembly {$ENDIF}(
    /// <summary>
    ///   align type
    /// </summary>
    VBaseLine = 0,
    /// <summary>
    ///   align type
    /// </summary>
    VBottom,
    /// <summary>
    ///   align type
    /// </summary>
    VMiddle,
    /// <summary>
    ///   align type
    /// </summary>
    VTop
  ) ;

  {#gendoc:hide}
  /// <summary>
  ///   Text HAlign types.
  /// </summary>
  TGIS_TextHAlign = {$IFDEF OXYGENE} assembly {$ENDIF}(
    /// <summary>
    ///   align type
    /// </summary>
    HLeft = 0,
    /// <summary>
    ///   align type
    /// </summary>
    HCenter,
    /// <summary>
    ///   align type
    /// </summary>
    HRight,
    /// <summary>
    ///   align type
    /// </summary>
    HAligned,      // if VAlign=0
    /// <summary>
    ///   align type
    /// </summary>
    HMiddle,       // if VAlign=0
    /// <summary>
    ///   align type
    /// </summary>
    HFit           // if VAlign=0
  );

  {#gendoc:hide}
  /// <summary>
  ///   Text entity definition.
  /// </summary>
  T_FileDwg_Text = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Insert point
      /// </summary>
      InsertionPoint  : TGIS_Point3D ;
      /// <summary>
      ///   Align point
      /// </summary>
      AlignmentPoint  : TGIS_Point3D ;
      /// <summary>
      ///   Text string
      /// </summary>
      Text            : String;
      /// <summary>
      ///   Text rotation
      /// </summary>
      Angle           : Double;
      /// <summary>
      ///   Text height
      /// </summary>
      Height          : Double;
      /// <summary>
      ///   Text width scale
      /// </summary>
      WidthScale      : Double;
      /// <summary>
      ///   HAlign type
      /// </summary>
      AlignH          : TGIS_TextHAlign;
      /// <summary>
      ///   VAlign type
      /// </summary>
      AlignV          : TGIS_TextVAlign;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion       : TGIS_Point3D ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Solid entity definition.
  /// </summary>
  T_FileDwg_Solid = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   First point
      /// </summary>
      Ptg1      : TGIS_Point3D ;
      /// <summary>
      ///   Second point
      /// </summary>
      Ptg2      : TGIS_Point3D ;
      /// <summary>
      ///   Third point
      /// </summary>
      Ptg3      : TGIS_Point3D ;
      /// <summary>
      ///   Fourth point
      /// </summary>
      Ptg4      : TGIS_Point3D ;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion : TGIS_Point3D ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   3DFace entity definition.
  /// </summary>
  T_FileDwg_3DFace = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Solid )
    public
      /// <summary>
      ///   Flag of invisibility
      /// </summary>
      InvisibleFlag : Integer ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Spline entity definition.
  /// </summary>
  T_FileDwg_Spline = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion         : TGIS_Point3D ;
      /// <summary>
      ///   Spline degree
      /// </summary>
      Degree            : Integer ;
      /// <summary>
      ///   Flag of invisibility
      /// </summary>
      Flag              : Integer ;
      /// <summary>
      ///   Number of control points
      /// </summary>
      NumControlPoints  : Integer ;
      /// <summary>
      ///   Number of knots
      /// </summary>
      NumKnots          : Integer ;
      /// <summary>
      ///   Number of fit points
      /// </summary>
      NumFitPoints      : Integer ;
      /// <summary>
      ///   List of control points
      /// </summary>
      ControlPoints     : TList<TGIS_Point3D>;
      /// <summary>
      ///   List of fit points
      /// </summary>
      FitPoints         : TList<TGIS_Point3D>;
      /// <summary>
      ///   List of knots
      /// </summary>
      Knots             : TList<Double>;
      /// <summary>
      ///   List of weights
      /// </summary>
      Weights           : TList<Double>;
    public
      /// <inheritdoc/>
      constructor Create( const _handle : UInt64 ;
                          const _eType  : T_FileDwg_EntityType
                         ) ; override;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroys an instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}
      /// <summary>
      ///   Add control point
      /// </summary>
      /// <param name="_p">
      ///   point
      /// </param>
      procedure AddControlPoint( const _p : TGIS_Point3D ) ;
      /// <summary>
      ///   Add fit point
      /// </summary>
      /// <param name="_p">
      ///   point
      /// </param>
      procedure AddFitPoint    ( const _p : TGIS_Point3D ) ;
      /// <summary>
      ///   Add knot
      /// </summary>
      /// <param name="_k">
      ///   knot value
      /// </param>
      procedure AddKnot        ( const _k : Double       ) ;
      /// <summary>
      ///   Add weight
      /// </summary>
      /// <param name="_w">
      ///   weight value
      /// </param>
      procedure AddWeight      ( const _w : Double       ) ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Types of Polylines.
  /// </summary>
  T_FileDwg_PolylineType = {$IFDEF OXYGENE} assembly {$ENDIF} (
    /// <summary>
    ///   polyline 2D
    /// </summary>
    P2D,
    /// <summary>
    ///   polyline 3D
    /// </summary>
    P3D,
    /// <summary>
    ///   polyline pface
    /// </summary>
    PFace,
    /// <summary>
    ///   polyline mesh
    /// </summary>
    PMesh
  ) ;

  {#gendoc:hide}
  /// <summary>
  ///   Types of Polyline vertexes.
  /// </summary>
  T_FileDwg_VertexType = {$IFDEF OXYGENE} assembly {$ENDIF} (
    /// <summary>
    ///   vertex 2D
    /// </summary>
    V2D,
    /// <summary>
    ///   vertex 3D
    /// </summary>
    V3D,
    /// <summary>
    ///   vertex pface
    /// </summary>
    VPFace,
    /// <summary>
    ///   vertex pface face
    /// </summary>
    VPFaceFace,
    /// <summary>
    ///   vertex mesh
    /// </summary>
    VMesh
  ) ;

  {#gendoc:hide}
  /// <summary>
  ///   Face entity definition.
  /// </summary>
  T_FileDwg_Face = {$IFDEF OXYGENE} assembly {$ENDIF} record
    public
      /// <summary>
      ///   Face vertex indexes
      /// </summary>
      VIndex : array[0..3] of SmallInt ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Polyline Vertex entity definition.
  /// </summary>
  T_FileDwg_Vertex = {$IFDEF OXYGENE} assembly {$ENDIF} record
    public
      /// <summary>
      ///   vertex id
      /// </summary>
      Id            : Integer ;
      /// <summary>
      ///   vertex coordinates
      /// </summary>
      Ptg           : TGIS_Point3D ;
      /// <summary>
      ///   vertex flags
      /// </summary>
      Flag          : Integer ;
      /// <summary>
      ///   vertex bulge
      /// </summary>
      Bulge         : Double;
      /// <summary>
      ///   vertex type
      /// </summary>
      VType         : T_FileDwg_VertexType ;
    public
      /// <summary>
      ///   Create a vertex
      /// </summary>
      /// <param name="_vt">
      ///   vertex type
      /// </param>
      constructor Create( const _vt : T_FileDwg_VertexType ) ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Polyline entity definition.
  /// </summary>
  T_FileDwg_Polyline = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion     : TGIS_Point3D ;
      /// <summary>
      ///   polyline flags
      /// </summary>
      Flag          : Integer ;
      /// <summary>
      ///   polyline curve type
      /// </summary>
      CurveType     : Integer ;
      /// <summary>
      ///   M vertex count
      /// </summary>
      MVertexCount  : Integer;
      /// <summary>
      ///   N vertex count
      /// </summary>
      NVertexCount  : Integer;
      /// <summary>
      ///   M density
      /// </summary>
      MDensity      : Integer;
      /// <summary>
      ///   N density
      /// </summary>
      NDensity      : Integer;
      /// <summary>
      ///   polyline type
      /// </summary>
      PType         : T_FileDwg_PolylineType ;
      /// <summary>
      ///   number of vertexes
      /// </summary>
      NumVertex     : Integer ;
      /// <summary>
      ///   number of faces
      /// </summary>
      NumFaces      : Integer ;
      /// <summary>
      ///   list of vertexes
      /// </summary>
      Vertices      : TList<T_FileDwg_Vertex>;
      /// <summary>
      ///   list of faces
      /// </summary>
      Faces         : TList<T_FileDwg_Face>;
    public
      /// <inheritdoc/>
      constructor Create( const _handle : UInt64 ;
                          const _eType  : T_FileDwg_EntityType
                         ) ; override;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroys an instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}
      /// <summary>
      ///   Add vertex to line
      /// </summary>
      /// <param name="_v">
      ///   vertex
      /// </param>
      procedure AddVertex( const _v : T_FileDwg_Vertex ) ;
      /// <summary>
      ///   Add face to line
      /// </summary>
      /// <param name="_v">
      ///   vertex
      /// </param>
      procedure AddFace  ( const _v : T_FileDwg_Face  ) ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Hatch Loop entity definition.
  /// </summary>
  T_FileDwg_HatchLoop = {$IFDEF OXYGENE} assembly {$ENDIF} class
    public
      /// <summary>
      ///   loop type
      /// </summary>
      HLType    : Integer ;
      /// <summary>
      ///   number of edges
      /// </summary>
      NumEdges  : Integer ;
      /// <summary>
      ///   list of entities
      /// </summary>
      Entities  : TList<T_FileDwg_Entity> ;
    public
      /// <summary>
      ///   Create a hatch loop.
      /// </summary>
      /// <param name="_lType">
      ///   loop type
      /// </param>
      constructor Create( const _lType  : Integer
                         ) ;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroys an instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}
      /// <summary>
      ///   Add loop entity
      /// </summary>
      /// <param name="_e">
      ///   entity handle
      /// </param>
      procedure AddEntity( const _e : T_FileDwg_Entity  ) ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Hatch entity definition.
  /// </summary>
  T_FileDwg_Hatch = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion : TGIS_Point3D ;
      /// <summary>
      ///   Is hatch solid
      /// </summary>
      Solid     : Integer ;
      /// <summary>
      ///   number of loops
      /// </summary>
      NumLoops  : Integer ;
      /// <summary>
      ///   hatch rotation
      /// </summary>
      Angle     : Double ;
      /// <summary>
      ///   hatch scale
      /// </summary>
      Scale     : Double ;
      /// <summary>
      ///   list of loops
      /// </summary>
      Loops     : TList<T_FileDwg_HatchLoop>;
    public
      /// <inheritdoc/>
      constructor Create( const _handle : UInt64 ;
                          const _eType  : T_FileDwg_EntityType
                         ) ; override;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroys an instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}
      /// <summary>
      ///   Add loop to hatch.
      /// </summary>
      /// <param name="_e">
      ///   hatch loop
      /// </param>
      procedure AddLoop( const _e : T_FileDwg_HatchLoop  ) ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Insert entity definition.
  /// </summary>
  T_FileDwg_Insert = {$IFDEF OXYGENE} assembly {$ENDIF} class( T_FileDwg_Entity )
    public
      /// <summary>
      ///   insert point of block
      /// </summary>
      InsertionPoint  : TGIS_Point3D ;
      /// <summary>
      ///   insert scale
      /// </summary>
      Scale           : TGIS_Point3D ;
      /// <summary>
      ///   rotation of block
      /// </summary>
      Rotation        : Double ;
      /// <summary>
      ///   reference to block
      /// </summary>
      BlockHandle     : UInt64 ;
      /// <summary>
      ///   block base point
      /// </summary>
      BlockBasePoint  : TGIS_Point3D ;
      /// <summary>
      ///   Extrusion of axis
      /// </summary>
      Extrusion       : TGIS_Point3D ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Encapsulation of class reading AutoCAD DWG format file.
  /// </summary>
  TGIS_FileDWG = {$IFDEF OXYGENE} assembly {$ENDIF} class
    private
      FileName : String ;
      FModel   : T_FileDwg_Model ;
    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;
      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroys an instance.
        /// </summary>
        destructor Destroy; override;
      {$ENDIF}
      /// <summary>
      ///   Open a file.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      /// <returns>
      ///   true if file was opened
      /// </returns>
      function Open( const _path  : String
                    ) : Boolean ;
      /// <summary>
      ///   Get a model.
      /// </summary>
      /// <returns>
      ///   model handle
      /// </returns>
      function GetModel : T_FileDwg_Model ;
  end ;

//##############################################################################
implementation

{
  This unit was inspired by Open Design Specification for DWG files v5.3
  and libDXFrw - Library to read/write DXF files
}

{$IFDEF DCC}
uses
  {$IFDEF GIS_DEBUG}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils,
  System.Math,
  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoStreams;
{$ENDIF}

const
  // DWG Object types.
  DWG_OBJ_ID_UNUSED                   = 0 ;
  DWG_OBJ_ID_TEXT                     = 1 ;
  DWG_OBJ_ID_ATTRIB                   = 2 ;
  DWG_OBJ_ID_ATTDEF                   = 3 ;
  DWG_OBJ_ID_BLOCK                    = 4 ;
  DWG_OBJ_ID_ENDBLK                   = 5 ;
  DWG_OBJ_ID_SEQEND                   = 6 ;
  DWG_OBJ_ID_INSERT                   = 7 ;
  DWG_OBJ_ID_MINSERT                  = 8 ;
  DWG_OBJ_ID_VERTEX_2D                = 10 ;
  DWG_OBJ_ID_VERTEX_3D                = 11 ;
  DWG_OBJ_ID_VERTEX_MESH              = 12 ;
  DWG_OBJ_ID_VERTEX_PFACE             = 13 ;
  DWG_OBJ_ID_VERTEX_PFACE_FACE        = 14 ;
  DWG_OBJ_ID_POLYLINE_2D              = 15 ;
  DWG_OBJ_ID_POLYLINE_3D              = 16 ;
  DWG_OBJ_ID_ARC                      = 17 ;
  DWG_OBJ_ID_CIRCLE                   = 18 ;
  DWG_OBJ_ID_LINE                     = 19 ;
  DWG_OBJ_ID_DIMENSION_ORDINATE       = 20 ;
  DWG_OBJ_ID_DIMENSION_LINEAR         = 21 ;
  DWG_OBJ_ID_DIMENSION_ALIGNED        = 22 ;
  DWG_OBJ_ID_DIMENSION_ANG3PT         = 23 ;
  DWG_OBJ_ID_DIMENSION_ANG2LN         = 24 ;
  DWG_OBJ_ID_DIMENSION_RADIUS         = 25 ;
  DWG_OBJ_ID_DIMENSION_DIAMETER       = 26 ;
  DWG_OBJ_ID_POINT                    = 27 ;
  DWG_OBJ_ID_FACE3D                   = 28 ;
  DWG_OBJ_ID_POLYLINE_PFACE           = 29 ;
  DWG_OBJ_ID_POLYLINE_MESH            = 30 ;
  DWG_OBJ_ID_SOLID                    = 31 ;
  DWG_OBJ_ID_TRACE                    = 32 ;
  DWG_OBJ_ID_SHAPE                    = 33 ;
  DWG_OBJ_ID_VIEWPORT_ENTITY          = 34 ;
  DWG_OBJ_ID_ELLIPSE                  = 35 ;
  DWG_OBJ_ID_SPLINE                   = 36 ;
  DWG_OBJ_ID_REGION                   = 37 ;
  DWG_OBJ_ID_SOLID3D                  = 38 ;
  DWG_OBJ_ID_BODY                     = 39 ;
  DWG_OBJ_ID_RAY                      = 40 ;
  DWG_OBJ_ID_XLINE                    = 41 ;
  DWG_OBJ_ID_DICTIONARY               = 42 ;
  DWG_OBJ_ID_MTEXT                    = 44 ;
  DWG_OBJ_ID_LEADER                   = 45 ;
  DWG_OBJ_ID_TOLERANCE                = 46 ;
  DWG_OBJ_ID_MLINE                    = 47 ;
  DWG_OBJ_ID_BLOCK_CONTROL            = 48 ;
  DWG_OBJ_ID_BLOCK_HEADER             = 49 ;
  DWG_OBJ_ID_LAYER_CONTROL            = 50 ;
  DWG_OBJ_ID_LAYER                    = 51 ;
  DWG_OBJ_ID_SHAPEFILE_CONTROL        = 52 ;
  DWG_OBJ_ID_SHAPEFILE                = 53 ;
  DWG_OBJ_ID_LINETYPE_CONTROL         = 56 ;
  DWG_OBJ_ID_LTYPE                    = 57 ;
  DWG_OBJ_ID_VIEW_CONTROL             = 60 ;
  DWG_OBJ_ID_VIEW                     = 61 ;
  DWG_OBJ_ID_UCS_CONTROL              = 62 ;
  DWG_OBJ_ID_UCS                      = 63 ;
  DWG_OBJ_ID_VPORT_CONTROL            = 64 ;
  DWG_OBJ_ID_VPORT                    = 65 ;
  DWG_OBJ_ID_APPID_CONTROL            = 66 ;
  DWG_OBJ_ID_APPID                    = 67 ;
  DWG_OBJ_ID_DIMSTYLE_CONTROL         = 68 ;
  DWG_OBJ_ID_DIMSTYLE                 = 69 ;
  DWG_OBJ_ID_VIEWPORT_ENTITY_CONTROL  = 70 ;
  DWG_OBJ_ID_VIEWPORT_ENTITY_HEADER   = 71 ;
  DWG_OBJ_ID_GROUP                    = 72 ;
  DWG_OBJ_ID_MLINESTYLE               = 73 ;
  DWG_OBJ_ID_OLE2FRAME                = 74 ;
  DWG_OBJ_ID_LONG_TRANSACTION         = 76 ;
  DWG_OBJ_ID_LWPOLYLINE               = 77 ;
  DWG_OBJ_ID_HATCH                    = 78 ;
  DWG_OBJ_ID_XRECORD                  = 79 ;
  DWG_OBJ_ID_PLACEHOLDER              = 80 ;
  DWG_OBJ_ID_VBA_PROJECT              = 81 ;
  DWG_OBJ_ID_LAYOUT                   = 82 ;
  DWG_OBJ_ID_LWPOLYLINE7              = 83 ;
  DWG_OBJ_ID_CLASS                    = 500 ;
  DWG_OBJ_ID_DBCOLOR                  = 498 ;

  DWG_COLORS : array [0..255] of array[0..2] of Integer =
  (
   (255,255,255),
   (255,  0,  0), (255,255,  0), (  0,255,  0), (  0,255,255), (  0,  0,255),
   (255,  0,255), (255,255,255), (128,128,128), (192,192,192), (255,  0,  0), // 10
   (255,127,127), (204,  0,  0), (204,102,102), (153,  0,  0), (153, 76, 76), // 15
   (127,  0,  0), (127, 63, 63), ( 76,  0,  0), ( 76, 38, 38), (255, 63,  0), // 20
   (255,159,127), (204, 51,  0), (204,127,102), (153, 38,  0), (153, 95, 76), // 25
   (127, 31,  0), (127, 79, 63), ( 76, 19,  0), ( 76, 47, 38), (255,127,  0), // 30
   (255,191,127), (204,102,  0), (204,153,102), (153, 76,  0), (153,114, 76), // 35
   (127, 63,  0), (127, 95, 63), ( 76, 38,  0), ( 76, 57, 38), (255,191,  0), // 40
   (255,223,127), (204,153,  0), (204,178,102), (153,114,  0), (153,133, 76), // 45
   (127, 95,  0), (127,111, 63), ( 76, 57,  0), ( 76, 66, 38), (255,255,  0), // 50
   (255,255,127), (204,204,  0), (204,204,102), (153,153,  0), (153,153, 76), // 55
   (127,127,  0), (127,127, 63), ( 76, 76,  0), ( 76, 76, 38), (191,255,  0), // 60
   (223,255,127), (153,204,  0), (178,204,102), (114,153,  0), (133,153, 76), // 65
   ( 95,127,  0), (111,127, 63), ( 57, 76,  0), ( 66, 76, 38), (127,255,  0), // 70
   (191,255,127), (102,204,  0), (153,204,102), ( 76,153,  0), (114,153, 76), // 75
   ( 63,127,  0), ( 95,127, 63), ( 38, 76,  0), ( 57, 76, 38), ( 63,255,  0), // 80
   (159,255,127), ( 51,204,  0), (127,204,102), ( 38,153,  0), ( 95,153, 76), // 85
   ( 31,127,  0), ( 79,127, 63), ( 19, 76,  0), ( 47, 76, 38), (  0,255,  0), // 90
   (127,255,127), (  0,204,  0), (102,204,102), (  0,153,  0), ( 76,153, 76), // 95
   (  0,127,  0), ( 63,127, 63), (  0, 76,  0), ( 38, 76, 38), (  0,255, 63), // 100
   (127,255,129), (  0,204, 51), (102,204,127), (  0,153, 38), ( 76,153, 95), // 105
   (  0,127, 31), ( 63,127, 79), (  0, 76, 19), ( 38, 76, 47), (  0,255,127), // 110
   (127,255,191), (  0,204,102), (102,204,153), (  0,153, 76), ( 76,153,114), // 115
   (  0,127, 63), ( 63,127, 95), (  0, 76, 38), ( 38, 76, 57), (  0,255,191), // 120
   (127,255,223), (  0,204,153), (102,204,178), (  0,153,114), ( 76,153,133), // 125
   (  0,127, 95), ( 63,127,111), (  0, 76, 57), ( 38, 76, 66), (  0,255,255), // 130
   (127,255,255), (  0,204,204), (102,204,204), (  0,153,153), ( 76,153,153), // 135
   (  0,127,127), ( 63,127,127), (  0, 76, 76), ( 38, 76, 76), (  0,191,255), // 140
   (127,223,255), (  0,153,204), (102,178,204), (  0,114,153), ( 76,133,153), // 145
   (  0, 95,127), ( 63,111,127), (  0, 57, 76), ( 38, 66, 76), (  0,127,255), // 150
   (127,191,255), (  0,102,204), (102,153,204), (  0, 76,153), ( 76,114,153), // 155
   (  0, 63,127), ( 63, 95,127), (  0, 38, 76), ( 38, 57, 76), (  0, 63,255), // 160
   (127,159,255), (  0, 51,204), (102,127,204), (  0, 38,153), ( 76, 95,153), // 165
   (  0, 31,127), ( 63, 79,127), (  0, 19, 76), ( 38, 47, 76), (  0,  0,255), // 170
   (127,127,255), (  0,  0,204), (102,102,204), (  0,  0,153), ( 76, 76,153), // 175
   (  0,  0,127), ( 63, 63,127), (  0,  0, 76), ( 38, 38, 76), ( 63,  0,255), // 180
   (159,127,255), ( 51,  0,204), (127,102,204), ( 38,  0,153), ( 95, 76,153), // 185
   ( 31,  0,127), ( 79, 63,127), ( 19,  0, 76), ( 47, 38, 76), (127,  0,255), // 190
   (191,127,255), (102,  0,204), (153,102,204), ( 76,  0,153), (114, 76,153), // 195
   ( 63,  0,127), ( 95, 63,127), ( 38,  0, 76), ( 57, 38, 76), (191,  0,255), // 200
   (223,127,255), (153,  0,204), (178,102,204), (114,  0,153), (133, 76,153), // 205
   ( 95,  0,127), (111, 63,127), ( 57,  0, 76), ( 66, 38, 76), (255,  0,255), // 210
   (255,127,255), (204,  0,204), (204,102,204), (153,  0,153), (153, 76,153), // 215
   (127,  0,127), (127, 63,127), ( 76,  0, 76), ( 76, 38, 76), (255,  0,191), // 220
   (255,127,223), (204,  0,153), (204,102,178), (153,  0,114), (153, 76,133), // 225
   (127,  0, 95), (127, 63,111), ( 76,  0, 57), ( 76, 38, 66), (255,  0,127), // 230
   (255,127,191), (204,  0,102), (204,102,153), (153,  0, 76), (153, 76,114), // 235
   (127,  0, 63), (127, 63, 95), ( 76,  0, 38), ( 76, 38, 57), (255,  0, 63), // 240
   (255,127,159), (204,  0, 51), (204,102,127), (153,  0, 38), (153, 76, 95), // 245
   (127,  0, 31), (127, 63, 79), ( 76,  0, 19), ( 76, 38, 47), ( 51, 51, 51), // 250
   ( 91, 91, 91), (132,132,132), (173,173,173), (214,214,214), (255,255,255)  // 255
  ) ;

  // Convert object type to String.

  function ObjectTypeToStr( const _type : Cardinal ) : String ; {$IFDEF DCC} inline ; {$ENDIF}
  begin
    {$IFDEF GIS_DEBUG}
    case _type of
      DWG_OBJ_ID_UNUSED                   : Result := 'UNUSED' ;
      DWG_OBJ_ID_TEXT                     : Result := 'TEXT' ;
      DWG_OBJ_ID_ATTRIB                   : Result := 'ATTRIB' ;
      DWG_OBJ_ID_ATTDEF                   : Result := 'ATTDEF' ;
      DWG_OBJ_ID_BLOCK                    : Result := 'BLOCK' ;
      DWG_OBJ_ID_ENDBLK                   : Result := 'ENDBLK' ;
      DWG_OBJ_ID_SEQEND                   : Result := 'SEQEND' ;
      DWG_OBJ_ID_INSERT                   : Result := 'INSERT' ;
      DWG_OBJ_ID_MINSERT                  : Result := 'MINSERT' ;
      DWG_OBJ_ID_VERTEX_2D                : Result := 'VERTEX_2D' ;
      DWG_OBJ_ID_VERTEX_3D                : Result := 'VERTEX_3D' ;
      DWG_OBJ_ID_VERTEX_MESH              : Result := 'VERTEX_MESH' ;
      DWG_OBJ_ID_VERTEX_PFACE             : Result := 'VERTEX_PFACE' ;
      DWG_OBJ_ID_VERTEX_PFACE_FACE        : Result := 'VERTEX_PFACE_FACE' ;
      DWG_OBJ_ID_POLYLINE_2D              : Result := 'POLYLINE_2D' ;
      DWG_OBJ_ID_POLYLINE_3D              : Result := 'POLYLINE_3D' ;
      DWG_OBJ_ID_ARC                      : Result := 'ARC' ;
      DWG_OBJ_ID_CIRCLE                   : Result := 'CIRCLE' ;
      DWG_OBJ_ID_LINE                     : Result := 'LINE' ;
      DWG_OBJ_ID_DIMENSION_ORDINATE       : Result := 'DIMENSION_ORDINATE' ;
      DWG_OBJ_ID_DIMENSION_LINEAR         : Result := 'DIMENSION_LINEAR' ;
      DWG_OBJ_ID_DIMENSION_ALIGNED        : Result := 'DIMENSION_ALIGNED' ;
      DWG_OBJ_ID_DIMENSION_ANG3PT         : Result := 'DIMENSION_ANG3PT' ;
      DWG_OBJ_ID_DIMENSION_ANG2LN         : Result := 'DIMENSION_ANG2LN' ;
      DWG_OBJ_ID_DIMENSION_RADIUS         : Result := 'DIMENSION_RADIUS' ;
      DWG_OBJ_ID_DIMENSION_DIAMETER       : Result := 'DIMENSION_DIAMETER' ;
      DWG_OBJ_ID_POINT                    : Result := 'POINT' ;
      DWG_OBJ_ID_FACE3D                   : Result := '3DFACE' ;
      DWG_OBJ_ID_POLYLINE_PFACE           : Result := 'POLYLINE_PFACE' ;
      DWG_OBJ_ID_POLYLINE_MESH            : Result := 'POLYLINE_MESH' ;
      DWG_OBJ_ID_SOLID                    : Result := 'SOLID' ;
      DWG_OBJ_ID_TRACE                    : Result := 'TRACE' ;
      DWG_OBJ_ID_SHAPE                    : Result := 'SHAPE' ;
      DWG_OBJ_ID_VIEWPORT_ENTITY          : Result := 'VIEWPORT_ENTITY' ;
      DWG_OBJ_ID_ELLIPSE                  : Result := 'ELLIPSE' ;
      DWG_OBJ_ID_SPLINE                   : Result := 'SPLINE' ;
      DWG_OBJ_ID_REGION                   : Result := 'REGION' ;
      DWG_OBJ_ID_SOLID3D                  : Result := '3DSOLID' ;
      DWG_OBJ_ID_BODY                     : Result := 'BODY' ;
      DWG_OBJ_ID_RAY                      : Result := 'RAY' ;
      DWG_OBJ_ID_XLINE                    : Result := 'XLINE' ;
      DWG_OBJ_ID_DICTIONARY               : Result := 'DICTIONARY' ;
      DWG_OBJ_ID_MTEXT                    : Result := 'MTEXT' ;
      DWG_OBJ_ID_LEADER                   : Result := 'LEADER' ;
      DWG_OBJ_ID_TOLERANCE                : Result := 'TOLERANCE' ;
      DWG_OBJ_ID_MLINE                    : Result := 'MLINE' ;
      DWG_OBJ_ID_BLOCK_CONTROL            : Result := 'BLOCK_CONTROL' ;
      DWG_OBJ_ID_BLOCK_HEADER             : Result := 'BLOCK_HEADER' ;
      DWG_OBJ_ID_LAYER_CONTROL            : Result := 'LAYER_CONTROL' ;
      DWG_OBJ_ID_LAYER                    : Result := 'LAYER' ;
      DWG_OBJ_ID_SHAPEFILE_CONTROL        : Result := 'SHAPEFILE_CONTROL' ;
      DWG_OBJ_ID_SHAPEFILE                : Result := 'SHAPEFILE' ;
      DWG_OBJ_ID_LINETYPE_CONTROL         : Result := 'LINETYPE_CONTROL' ;
      DWG_OBJ_ID_LTYPE                    : Result := 'LTYPE' ;
      DWG_OBJ_ID_VIEW_CONTROL             : Result := 'VIEW_CONTROL' ;
      DWG_OBJ_ID_VIEW                     : Result := 'VIEW' ;
      DWG_OBJ_ID_UCS_CONTROL              : Result := 'UCS_CONTROL' ;
      DWG_OBJ_ID_UCS                      : Result := 'UCS' ;
      DWG_OBJ_ID_VPORT_CONTROL            : Result := 'VPORT_CONTROL' ;
      DWG_OBJ_ID_VPORT                    : Result := 'VPORT' ;
      DWG_OBJ_ID_APPID_CONTROL            : Result := 'APPID_CONTROL' ;
      DWG_OBJ_ID_APPID                    : Result := 'APPID' ;
      DWG_OBJ_ID_DIMSTYLE_CONTROL         : Result := 'DIMSTYLE_CONTROL' ;
      DWG_OBJ_ID_DIMSTYLE                 : Result := 'DIMSTYLE' ;
      DWG_OBJ_ID_VIEWPORT_ENTITY_CONTROL  : Result := 'VIEWPORT_ENTITY_CONTROL' ;
      DWG_OBJ_ID_VIEWPORT_ENTITY_HEADER   : Result := 'VIEWPORT_ENTITY_HEADER' ;
      DWG_OBJ_ID_GROUP                    : Result := 'GROUP' ;
      DWG_OBJ_ID_MLINESTYLE               : Result := 'MLINESTYLE' ;
      DWG_OBJ_ID_LWPOLYLINE               : Result := 'LWPOLYLINE' ;
      DWG_OBJ_ID_HATCH                    : Result := 'HATCH' ;
      DWG_OBJ_ID_XRECORD                  : Result := 'XRECORD' ;
      DWG_OBJ_ID_PLACEHOLDER              : Result := 'PLACEHOLDER' ;
      DWG_OBJ_ID_VBA_PROJECT              : Result := 'VBA_PROJECT' ;
      DWG_OBJ_ID_LAYOUT                   : Result := 'LAYOUT' ;
      DWG_OBJ_ID_LWPOLYLINE7              : Result := 'LWPOLYLINE7' ;
      DWG_OBJ_ID_CLASS                    : Result := 'CLASS' ;
      DWG_OBJ_ID_DBCOLOR                  : Result := 'DBCOLOR'
    else                                    Result := '???'
    end ;
    {$ENDIF}
  end ;

type

  DWG_Version = (
    UNKNOWNV,    // UNKNOWN VERSION.
    AC1006,      // R10.
    AC1009,      // R11 & R12.
    AC1012,      // R13.
    AC1014,      // R14.
    AC1015,      // ACAD 2000.
    AC1018,      // ACAD 2004.
    AC1021,      // ACAD 2007.
    AC1024,      // ACAD 2010.
    AC1027,      // ACAD 2013.
    AC1032       // ACAD 2018.
  ) ;

  dint8     = ShortInt; // 8 bit signed
  dint16    = SmallInt; // 16 bit signed
  dint32    = Integer;  // 32 bit signed
  dint64    = Int64;    // 64 bit signed
  duint8    = Byte;     // 8 bit unsigned
  duint16   = Word;     // 16 bit unsigned
  duint32   = Cardinal; // 32 bit unsigned
  duint64   = UInt64;   // 64 bit unsigned
  dfloat32  = Single;   // 32 bit floating point
  ddouble64 = Double;   // 64 bit floating point
  ddouble80 = Extended; // 80 bit floating point

  DwgCoord = record
    public
      x : Double ;
      y : Double ;
      z : Double ;
    public
      constructor Create( const _ix : Double ;
                          const _iy : Double ;
                          const _iz : Double
                         ) ;
      function ToArray : TArray<Double>;
      function ToPoint : TGIS_Point3D ;
  end ;

  DwgVertex2D = class
    public
      constructor Create; overload;
      constructor Create(
        const _sx : Double;
        const _sy : Double;
        const _b  : Double
      ); overload;
    public
      x        : Double;
      y        : Double;
      stawidth : Double;
      endwidth : Double;
      bulge    : Double;
  end ;

  DwgHandle = record
    code : duint8;
    size : duint8;
    ref  : duint64;
  end ;

  DwgBasicStream = {$IFDEF OXYGENE} abstract {$ENDIF} class
    public
      function read(    var _s : TBytes;
                      const _n : duint64
                    ) : Boolean; virtual; abstract;
      function getSize : duint64; virtual; abstract;
      function getPos : duint64; virtual; abstract;
      function setPos( const _p : duint64
                      ) : Boolean; virtual; abstract;
      function good : Boolean; virtual; abstract;
      function clone : DwgBasicStream; virtual; abstract;
    public
      property Pos  : duint64 read getPos ;
      property Size : duint64 read getSize ;
  end ;

  DwgByteStream = class( DwgBasicStream )
    private
      stream : TBytes;
      sz     : duint64;
      pos    : duint64;
      isOk   : Boolean;
    public
      constructor Create( const _buf  : TBytes;
                          const _size : Integer
                         ) ;
      function read(    var _s : TBytes;
                      const _n : duint64
                     ) : Boolean; override;
      function getSize : duint64; override;
      function getPos : duint64; override;
      function setPos( const _p : duint64 ) : Boolean; override;
      function good : Boolean; override;
      function clone : DwgBasicStream; override;
  end ;

  DwgFileStream = class( DwgBasicStream )
    private
      stream : TStream;
      sz     : duint64;
    public
      constructor Create( const _stream : TStream ) ;
      function read(
        var _s   : TBytes;
        const _n : duint64 ) : Boolean; override;
      function getSize : duint64; override;
      function getPos : duint64; override;
      function setPos( const _p : duint64 ) : Boolean; override;
      function good : Boolean; override;
      function clone : DwgBasicStream; override;
  end ;

  DwgColorType = (
    ByBlock = 0,
    ByLayer,
    ByRGB,
    ByIndex,
    ByReference
  ) ;

  DwgColor = record
    public
      CType : DwgColorType ;
      Value : duint32 ;
    public
      constructor Create( const _ctype : DwgColorType ;
                          const _value : duint32
                         ) ;
      function ToArray  : TArray<duint32>;
      function ToString : String ; {$IFDEF OXYGENE}override;{$ENDIF}
  end ;

  DwgBuffer = class
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      filestr  : DwgBasicStream;
      maxSize  : duint64;
      currByte : TBytes;
      bitPos   : duint8;
      decoder  : TEncoding;
    private
      function get8bitStr : String;
      function get16bitStr( _textSize : duint16;
                            _nullTerm : Boolean = true
                           ) : String;
    public
      constructor Create; overload;
      constructor Create( const _stream : TStream ) ; overload;
      constructor Create( const _buf : TBytes;
                          const _size : Integer;
                          const _decoder : TEncoding
                         ) ; overload;
      constructor Create( const _org : DwgBuffer ) ; overload;

      {$IFNDEF OXYGENE}
      destructor Destroy; override;
      {$ENDIF}

      function clone : DwgBuffer;
      function size : duint64;
      function setPosition( const _pos : duint64 ) : Boolean;
      function getPosition : duint64;
      procedure resetPosition;
      procedure setBitPos( const _pos : duint8 ) ;
      function getBitPos : duint8;
      function moveBitPos( const _size : dint32 ) : Boolean;
      function getBit : duint8; // B7
      function getBoolBit : Boolean; // B as Boolean
      function get2Bits : duint8; // BB
      function get3Bits : duint8; // 3B
      function getBitShort : duint16; // BS
      function getSBitShort : dint16; // BS
      function getBitLong : dint32; // BL
      function getBitLongLong : duint64; // BLL (R24)
      function getBitDouble : Double;
      function get3BitDouble : DwgCoord; // 3BD
      function getRawChar8 : duint8; // RC
      function getRawShort16 : duint16; // RS
      function getRawDouble : Double; // RD
      function getRawLong32 : duint32; // RL
      function getRL32 : duint32;
      function getRL64 : duint64;
      function getRawLong64 : duint64; // RLL
      function get2RawDouble : DwgCoord; // 2RD
      function getUModularChar : duint64; // UMC
      function getModularChar : dint32; // MC
      function getModularShort : dint32; // MS
      function getHandle : DwgHandle; // H
      function getOffsetHandle( const _href : duint64 ) : DwgHandle;
      function getVariableText( const _v : DWG_Version; const _nullTerm : Boolean = true ) : String;
      function getCP8Text : String;
      function getUCSText( const _nullTerm : Boolean = true ) : String;
      function getUCSStr( const _ts : duint16 ) : String;
      function getUCSEed( const _ts : duint16 ) : String;
      function getObjType( const _v : DWG_Version ) : duint16; // OT
      function getExtrusion( const _b_R2000_style : Boolean ) : DwgCoord; // BE
      function getDefaultDouble( const _d : Double ) : Double; // DD
      function getThickness( const _b_R2000_style : Boolean ) : Double; // BT
      function getCmColor( const _v : DWG_Version ) : DwgColor; // CMC
      function getEnColor( const _v : DWG_Version ) : DwgColor; // ENC
      function getBERawShort16 : duint16;
      function isGood : Boolean;
      function getBytes( var _buf : TBytes; const _size : Integer ) : Boolean;
      function numRemainingBytes : Integer;
    public
      property Stream  : DwgBasicStream read filestr ;
  end ;

  DWG_Section = (
    UNKNOWNS,       // UNKNOWN section.
    FILEHEADER,     // File Header
    HEADER,         // AcDb:Header
    CLASSES,        // AcDb:Classes
    SUMARYINFO,     // AcDb:SummaryInfo
    PREVIEW,        // AcDb:Preview
    VBAPROY,        // AcDb:VBAProject
    APPINFO,        // AcDb:AppInfo
    FILEDEP,        // AcDb:FileDepList
    REVHISTORY,     // AcDb:RevHistory
    SECURITY,       // AcDb:Security
    OBJECTS,        // AcDb:AcDbObjects
    OBJFREESPACE,   // AcDb:ObjFreeSpace
    TEMPLATE,       // AcDb:Template
    HANDLES,        // AcDb:Handles
    PROTOTYPE,      // AcDb:AcDsPrototype_1b
    AUXHEADER,      // AcDb:AuxHeader
    SIGNATURE,      // AcDb:Signature
    APPINFOHISTORY, // AcDb:AppInfoHistory
    EXTEDATA,       // Extended Entity Data
    PROXYGRAPHICS   // PROXY ENTITY GRAPHICS
  ) ;

  DWG_TTYPE = (
    UNKNOWNT,
    LTYPE,
    LAYER,
    STYLE,
    DIMSTYLE,
    VPORT,
    BLOCK_RECORD,
    APPID,
    IMAGEDEF,
    LAYOUT
  ) ;

  DwgPageInfo = class
    public
      Id          : dint64;
      address     : duint64;
      size        : duint64;
      dataSize    : duint64;
      startOffset : duint64;
      cSize       : duint64;
      uSize       : duint64;
    public
      constructor Create( const _i  : dint64;
                          const _ad : duint64;
                          const _sz : duint32
                         ) ;
  end ;

  DwgSectionInfo = class
    public
      Id        : dint32;
      name      : String;
      compresed : duint32;
      encrypted : duint32;
      size      : duint64;
      maxSize   : duint64;
      address   : duint64;
      pageCount : duint64;
      pages     : TDictionary<duint32, TObject>;
    public
      constructor Create;
      {$IFNDEF OXYGENE}
      destructor Destroy ; override;
      {$ENDIF}
  end ;

  DwgClass = class
    public
      recName       : String;
      className     : String;
      appName       : String;
      proxyFlag     : Integer;
      instanceCount : Integer;
      wasaProxyFlag : Integer;
      entityFlag    : Integer;
      classNum      : duint16;
      DwgType       : Integer;
    public
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _strBuf  : DwgBuffer
      ) : Boolean;
  end ;

  DwgObjHandle = record
    public
      typ    : duint32;
      handle : duint64;
      loc    : duint32;
    public
      constructor Create(
        const _t : duint32;
        const _h : duint64;
        const _l : duint32
      ) ; overload;
  end ;

  DwgHeader = class
    public
      vars : TDictionary<String, Variant>;
    private
      comments : String;
      name     : String;
      curr     : Variant;
      version  : Integer;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      linetypeCtrl      : duint64 ;
      layerCtrl         : duint64 ;
      styleCtrl         : duint64 ;
      dimstyleCtrl      : duint64 ;
      appidCtrl         : duint64 ;
      blockCtrl         : duint64 ;
      viewCtrl          : duint64 ;
      ucsCtrl           : duint64 ;
      vportCtrl         : duint64 ;
      vpEntHeaderCtrl   : duint64 ;
      modelSpaceBlkRec  : duint64 ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _hBbuf   : DwgBuffer;
        const _mv      : dint16 = 0
      ) : Boolean;
    public
      constructor Create;
      {$IFNDEF OXYGENE}
      destructor Destroy ; override;
      {$ENDIF}
  end ;

  DwgTableEntry = class
    public
      tableType    : DWG_TTYPE;
      handle       : duint64;
      parentHandle : duint64;
      name         : String;
      flag         : dint32;
    protected
      oType       : dint16;
      xDictFlag   : duint8;
      numReactors : dint32;
      objSize     : duint32;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; virtual;
      function parseDwgEx(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _strBuf  : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean;
      procedure reset; virtual;
    public
      constructor Create;
  end ;

  DwgObjControl = class( DwgTableEntry )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      procedure reset; override;

    public
      handlesList : TList<duint64>;
    public
      constructor Create;
      {$IFNDEF OXYGENE}
      destructor Destroy ; override;
      {$ENDIF}
  end ;

  DwgLType = class( DwgTableEntry )
    private
      pathIdx : dint32;
    public
      desc   : String;
      size   : dint32;
      length : Double;
      path   : TList<Double>;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      procedure reset; override;
      procedure update;
    public
      constructor Create;
      {$IFNDEF OXYGENE}
      destructor Destroy ; override;
      {$ENDIF}
  end ;

  DwgLayer = class( DwgTableEntry )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      procedure reset; override;
    public
      lineType        : String;
      color           : DwgColor;
      plotF           : Boolean;
      lWeight         : dint32;
      handlePlotS     : String;
      handleMaterialS : String;
      lTypeH          : DwgHandle;
    public
      constructor Create;
  end ;

  DwgTextstyle = class( DwgTableEntry )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      procedure reset; override;
    public
      height     : Double;
      width      : Double;
      oblique    : Double;
      genFlag    : dint32;
      lastHeight : Double;
      font       : String;
      bigFont    : String;
      fontFamily : dint32;
    public
      constructor Create;
  end ;

  DwgDimstyle = class( DwgTableEntry )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      procedure reset; override;
    public
      constructor Create;
  end ;

  DwgVport = class( DwgTableEntry )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      procedure reset; override;
    public
      lowerLeft    : DwgCoord;
      UpperRight   : DwgCoord;
      center       : DwgCoord;
      snapBase     : DwgCoord;
      snapSpacing  : DwgCoord;
      gridSpacing  : DwgCoord;
      viewDir      : DwgCoord;
      viewTarget   : DwgCoord;
      height       : Double;
      ratio        : Double;
      lensHeight   : Double;
      frontClip    : Double;
      backClip     : Double;
      snapAngle    : Double;
      twistAngle   : Double;
      viewMode     : dint32;
      circleZoom   : dint32;
      fastZoom     : dint32;
      ucsIcon      : dint32;
      snap         : dint32;
      grid         : dint32;
      snapStyle    : dint32;
      snapIsopair  : dint32;
      gridBehavior : dint32;
    public
      constructor Create;
  end ;

  DwgBlockRecord = class( DwgTableEntry )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      procedure reset; override;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      blockEnt : duint64;
      endBlock : duint64;
      firstEH  : duint64;
      lastEH   : duint64;
      entMap   : TList<duint64>;
    public
      insUnits : dint32;
      basePoint : DwgCoord;
    public
      constructor Create;
      {$IFNDEF OXYGENE}
      destructor Destroy ; override;
      {$ENDIF}
  end ;

  DwgDbColor = class( DwgTableEntry )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      color : DwgColor ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
  end ;

  DwgLayout = class( DwgTableEntry )
   {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     page_setup_name             : String ;
     printer_or_config           : String ;
     plot_layout_flags           : duint32 ;
     left_margin                 : Double ;
     bottom_margin               : Double ;
     right_margin                : Double ;
     top_margin                  : Double ;
     paper_width                 : Double ;
     paper_height                : Double ;
     paper_size                  : String ;
     plot_origin                 : DwgCoord ;
     paper_units                 : duint32 ;
     plot_rotation               : duint32 ;
     plot_type                   : duint32 ;
     window_min                  : DwgCoord ;
     window_max                  : DwgCoord ;
     plot_view_name              : String ;
     real_world_units            : Double ;
     drawing_units               : Double ;
     current_style_sheet         : String ;
     scale_type                  : duint32 ;
     scale_factor                : Double ;
     paper_image_origin          : DwgCoord ;
     shade_plot_mode             : duint32 ;
     shade_plot_res_level        : duint32 ;
     shade_plot_custom_dpi       : duint32 ;
     layout_name                 : String ;
     tab_order                   : duint32 ;
     flags                       : duint32 ;
     ucs_origin                  : DwgCoord ;
     minimum_limits              : DwgCoord ;
     maximum_limits              : DwgCoord ;
     ins_point                   : DwgCoord ;
     ucs_x_axis                  : DwgCoord ;
     ucs_y_axis                  : DwgCoord ;
     elevation                   : Double ;
     orthoview_type              : duint32 ;
     extent_min                  : DwgCoord ;
     extent_max                  : DwgCoord ;
     viewport_count              : duint32 ;
     plot_view_handle            : DwgHandle ;
     visual_style_handle         : DwgHandle ;
     assoc_paperspace_block_hdl  : DwgHandle ;
     last_active_viewport_handle : DwgHandle ;
     base_ucs_handle             : DwgHandle ;
     named_ucs_handle            : DwgHandle ;
     viewport_handles            : TList<DwgHandle> ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      procedure reset; override;
    public
      constructor Create;
      {$IFNDEF OXYGENE}
      destructor Destroy ; override;
      {$ENDIF}
  end ;

  DwgAppId = class( DwgTableEntry )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      procedure reset; override;
  end ;

  DwgSpace = (
    ModelSpace = 0,
    PaperSpace = 1
  ) ;

  DWG_ETYPE = (
    E3DFACE,
    ARC,
    ATTDEF,
    ATTRIB,
    BLOCK,
    CIRCLE,
    DIMENSION, DIMALIGNED, DIMLINEAR, DIMRADIAL, DIMDIAMETRIC,
    DIMANGULAR, DIMANGULAR3P, DIMORDINATE,
    ELLIPSE,
    HATCH,
    // HELIX,
    IMAGE,
    INSERT,
    LEADER,
    // LIGHT,
    LINE,
    LWPOLYLINE,
    // MESH,
    // MLINE,
    // MLEADERSTYLE,
    // MLEADER,
    MTEXT,
    // OLEFRAME,
    // OLE2FRAME,
    POINT,
    POLYLINE,
    RAY,
    // REGION,
    // SHAPE,
    SOLID,
    SPLINE,
    // SUN,
    // SURFACE,
    // TABLE,
    TEXT,
    // TOLERANCE,
    TRACE,
    UNDERLAY,
    VERTEX,
    VIEWPORT,
    // WIPEOUT,
    XLINE,
    UNKNOWN
  ) ;

  DwgEntity = class
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; virtual;
      function parseDwgEx(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _strBuf  : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean;
      function parseDwgEntHandle(
        const _version : DWG_Version;
        const _buf     : DwgBuffer
      ) : Boolean;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      haveNextLinks : duint8;
      plotFlags     : duint8;
      ltFlags       : duint8;
      materialFlag  : duint8;
      shadowFlag    : duint8;
      lTypeH        : DwgHandle;
      layerH        : DwgHandle;
      nextEntLink   : duint64;
      prevEntLink   : duint64;
      ownerHandle   : Boolean;

      xDictFlag     : duint8;
      numReactors   : dint32;
      objSize       : duint32;
      oType         : dint16;
    private
      extAxisX      : DwgCoord;
      extAxisY      : DwgCoord;
      curr          : Variant;
    public
      eType         : DWG_ETYPE;
      handle        : duint64;
      parentHandle  : duint64;
      space         : DwgSpace;
      layer         : String;
      lineType      : String;
      material      : duint32;
      color         : DwgColor ;
      lWeight       : dint32;
      ltypeScale    : Double;
      visible       : Boolean;
      numProxyGraph : dint32;
      proxyGraphics : String;
      colorName     : String;
      transparency  : dint32;
      plotStyle     : dint32;
      shadow        : dint32;
      extData       : TStringList;
    public
      constructor Create;
      {$IFDEF DCC}
      destructor Destroy; override;
      {$ENDIF}
  end ;

  DwgPoint = class( DwgEntity )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      basePoint : DwgCoord;
      thickness : Double;
      extPoint  : DwgCoord;
      constructor Create;
  end ;

  DwgLine = class( DwgPoint )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      secPoint : DwgCoord;
      constructor Create;
  end ;

  DwgRay = class( DwgLine )
   {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     function parseDwg(
       const _version : DWG_Version;
       const _buf     : DwgBuffer;
       const _bs      : duint32 = 0
     ) : Boolean; override;
   public
     constructor Create;
  end ;

  DwgXline = class( DwgRay )
   public
     constructor Create;
  end ;

  DwgCircle = class( DwgPoint )
   {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     function parseDwg(
       const _version : DWG_Version;
       const _buf     : DwgBuffer;
       const _bs      : duint32 = 0
     ) : Boolean; override;
   public
     radious : Double;
     constructor Create;
  end ;

  DwgArc = class( DwgCircle )
   {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     function parseDwg(
       const _version : DWG_Version;
       const _buf     : DwgBuffer;
       const _bs      : duint32 = 0
     ) : Boolean; override;
   public
     staangle : Double;
     endangle : Double;
     isccw    : dint32;
     constructor Create;
  end ;

  DwgEllipse = class( DwgLine )
   {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     function parseDwg(
       const _version : DWG_Version;
       const _buf     : DwgBuffer;
       const _bs      : duint32 = 0
     ) : Boolean; override;
   public
     staparam : Double;
     endparam : Double;
     ratio    : Double ;
     isccw    : dint32;
     constructor Create;
  end ;

  DwgTrace = class( DwgLine )
   {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     function parseDwg(
       const _version : DWG_Version;
       const _buf     : DwgBuffer;
       const _bs      : duint32 = 0
     ) : Boolean; override;
   public
     thirdPoint : DwgCoord ;
     fourPoint  : DwgCoord ;
     constructor Create;
  end ;

  DwgSolid = class( DwgTrace )
   {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     function parseDwg(
       const _version : DWG_Version;
       const _buf     : DwgBuffer;
       const _bs      : duint32 = 0
     ) : Boolean; override;
   public
     constructor Create;
  end ;

  {$IFDEF OXYGENE}
    InvisibleEdgeFlags nested in Dwg3Dface = (
      NoEdge     = $00,
      FirstEdge  = $01,
      SecodEdge  = $02,
      ThirdEdge  = $04,
      FourthEdge = $08,
      AllEdges   = $0F
    );
  {$ENDIF}
  Dwg3Dface = class( DwgTrace )
    {$IFDEF DCC}
     type
       InvisibleEdgeFlags = ( NoEdge     = $00,
                              FirstEdge  = $01,
                              SecodEdge  = $02,
                              ThirdEdge  = $04,
                              FourthEdge = $08,
                              AllEdges   = $0F
                            );
    {$ENDIF}
   {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     function parseDwg(
       const _version : DWG_Version;
       const _buf     : DwgBuffer;
       const _bs      : duint32 = 0
     ) : Boolean; override;
   public
     invisibleflag : dint32 ;
     constructor Create;
  end ;

  DwgInsert = class( DwgPoint )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     function parseDwg(
       const _version : DWG_Version;
       const _buf     : DwgBuffer;
       const _bs      : duint32 = 0
     ) : Boolean; override;
    public
      name      : String;
      xscale    : Double;
      yscale    : Double;
      zscale    : Double;
      angle     : Double;
      colcount  : dint32;
      rowcount  : dint32;
      colspace  : Double;
      rowspace  : Double;
      blockRecH : DwgHandle;
      seqendH   : DwgHandle;
    constructor Create;
  end ;

  DwgLWPolyline = class( DwgEntity )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      vertexnum : dint32;
      bulgesnum : duint32 ;
      flag      : dint32;
      width     : Double;
      elevation : Double;
      thickness : Double;
      extPoint  : DwgCoord;
      vertex    : DwgVertex2D;
      vertlist  : TList<DwgVertex2D>;
    public
      constructor Create;
      {$IFDEF DCC}
      destructor Destroy; override;
      {$ENDIF}
  end ;

  DwgLeader = class( DwgEntity )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      style           : String ;
      arrow           : dint32 ;
      leadertype      : dint32 ;
      flag            : dint32 ;
      hookline        : dint32 ;
      hookflag        : dint32 ;
      textheight      : Double ;
      textwidth       : Double ;
      vertnum         : dint32 ;
      coloruse        : dint32 ;
      annotHandle     : duint32 ;
      extrusionPoint  : DwgCoord ;
      horizdir        : DwgCoord ;
      offsetblock     : DwgCoord ;
      offsettext      : DwgCoord ;
      dimStyleH       : DwgHandle;
      annotH          : DwgHandle ;
      vertexlist      : TList<DwgCoord>;
    public
      constructor Create;
      {$IFDEF DCC}
      destructor Destroy; override;
      {$ENDIF}
  end ;

  DwgText = class( DwgLine )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      height     : Double;
      text       : String;
      angle      : Double;
      widthscale : Double;
      oblique    : Double;
      style      : String;
      textgen    : dint32;
      alignH     : TGIS_TextHAlign;
      alignV     : TGIS_TextVAlign;
      styleH     : DwgHandle;
      constructor Create;
  end ;

  DwgAttrib = class( DwgText )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      tag         : String;
      fieldLength : duint16;
      fflags      : duint8;
    public
      constructor Create;
  end ;

  DwgAttDef = class( DwgText )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      tag         : String;
      fieldLength : duint16;
      fflags      : duint8;
      prompt      : String ;
      defValue    : String ;
    public
      constructor Create;
  end ;

  {$IFDEF OXYGENE}
    Attach nested in DwgMText = (
      TopLeft = 1,
      TopCenter,
      TopRight,
      MiddleLeft,
      MiddleCenter,
      MiddleRight,
      BottomLeft,
      BottomCenter,
      BottomRight
    );
  {$ENDIF}

  DwgMText = class( DwgText )
    {$IFDEF DCC}
     type
       Attach = ( TopLeft = 1,
                  TopCenter,
                  TopRight,
                  MiddleLeft,
                  MiddleCenter,
                  MiddleRight,
                  BottomLeft,
                  BottomCenter,
                  BottomRight
                 );
    {$ENDIF}
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
     function parseDwg(
       const _version : DWG_Version;
       const _buf     : DwgBuffer;
       const _bs      : duint32 = 0
     ) : Boolean; override;
    public
      constructor Create;
  end ;

  DwgVertex = class( DwgPoint )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
      function parseDwgEx(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0;
        const _el      : Double = 0
      ) : Boolean;
    public
      stawidth : Double;
      endwidth : Double;
      bulge    : Double;

      flag       : dint32;
      tgdir      : Double;
      vindex1    : dint32;
      vindex2    : dint32;
      vindex3    : dint32;
      vindex4    : dint32;
      identifier : dint32;
    public
      constructor Create; overload;
      constructor Create(
        const _sx : Double;
        const _sy : Double;
        const _sz : Double;
        const _b  : Double
      ); overload;
  end ;

  DwgPolyline = class( DwgPoint )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      flag          : dint32;
      defstawidth   : Double;
      defendwidth   : Double;
      vertexcount   : dint32;
      facecount     : dint32;
      smoothM       : dint32;
      smoothN       : dint32;
      curvetype     : dint32;
      mvertexcount  : dint32;
      nvertexcount  : dint32;
      mdensity      : dint32;
      ndensity      : dint32;
      vertlist      : TList<DwgVertex>;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      hadlesList : TList<duint64>;
      firstEH    : duint64;
      lastEH     : duint64;
      seqendH    : DwgHandle;
    public
      Constructor Create;
      {$IFDEF DCC}
      destructor Destroy; override;
      {$ENDIF}
  end ;

  DwgSpline = class( DwgEntity )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      normalVec : DwgCoord;
      tgStart   : DwgCoord;
      tgEnd     : DwgCoord;
      flags     : dint32;
      degree    : dint32;
      nknots    : dint32;
      ncontrol  : dint32;
      nfit      : dint32;
      tolknot   : Double;
      tolcontrol: Double;
      tolfit    : Double;

      knotslist   : TList<Double>;
      weightslist : TList<Double>;
      controllist : TList<DwgCoord>;
      fitlist     : TList<DwgCoord>;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      controlpoint : DwgCoord ;
      fitpoint     : DwgCoord ;

    public
      Constructor Create;
      {$IFDEF DCC}
      destructor Destroy; override;
      {$ENDIF}
  end ;

  DwgHatchLoop = class
    public
      typ     : dint32 ;
      numedges: dint32 ;
      objlist : TList<DwgEntity> ;
    public
      Constructor Create( const _t : dint32 );
      procedure update ;
      {$IFDEF DCC}
      destructor Destroy; override;
      {$ENDIF}
  end ;

  DwgHatch = class( DwgPoint )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      name        : String ;
      solid       : dint32 ;
      associative : dint32 ;
      hstyle      : dint32 ;
      hpattern    : dint32 ;
      doubleflag  : dint32 ;
      loopsnum    : dint32 ;
      angle       : Double ;
      scale       : Double ;
      deflines    : dint32 ;
      looplist    : TList<DwgHatchLoop> ;
    private
      ispol : Boolean ;
    public
      Constructor Create;
      {$IFDEF DCC}
      destructor Destroy; override;
      {$ENDIF}
      procedure appendLoop( const _v : DwgHatchLoop ) ;
  end ;

  DwgBlock = class( DwgPoint )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwg(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _bs      : duint32 = 0
      ) : Boolean; override;
    public
      name  : String;
      flag  : dint32;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      isEnd : Boolean;
    public
      Constructor Create;
  end ;

  DwgDimensionCommon = class( DwgEntity )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function parseDwgCommon(
        const _version : DWG_Version;
        const _buf     : DwgBuffer;
        const _sbuf    : DwgBuffer
      ) : Boolean; virtual;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      name        : String ;
      defPoint    : DwgCoord ;
      textPoint   : DwgCoord ;
      text        : String ;
      style       : String ;
      align       : dint32 ;
      linesty     : dint32 ;
      linefactor  : Double ;
      rot         : Double ;
      extPoint    : DwgCoord ;
      hdir        : Double ;
      clonePoint  : DwgCoord ;
      def1        : DwgCoord ;
      def2        : DwgCoord ;
      angle       : Double ;
      oblique     : Double ;
      circlePoint : DwgCoord ;
      arcPoint    : DwgCoord ;
      length      : Double ;
      dimStyleH   : DwgHandle ;
      blockH      : DwgHandle ;
    public
      dtype       : dint32 ;
    public
      Constructor Create;
  end ;

  DwgDimensionAligned = class( DwgDimensionCommon )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
       function parseDwg(
         const _version : DWG_Version;
         const _buf     : DwgBuffer;
         const _bs      : duint32 = 0
       ) : Boolean; override;
    public
      Constructor Create;
  end ;

  DwgDimensionLinear = class( DwgDimensionAligned )
    public
      Constructor Create;
  end ;

  DwgDimensionOrdinate = class( DwgDimensionCommon )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
       function parseDwg(
         const _version : DWG_Version;
         const _buf     : DwgBuffer;
         const _bs      : duint32 = 0
       ) : Boolean; override;
    public
      Constructor Create;
  end ;

  DwgDimensionDiametric = class( DwgDimensionCommon )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
       function parseDwg(
         const _version : DWG_Version;
         const _buf     : DwgBuffer;
         const _bs      : duint32 = 0
       ) : Boolean; override;
    public
      Constructor Create;
  end ;

  DwgReaderCallBack = class
    private
      rdr       : TObject ;
      inBlock   : Boolean ;
      model     : T_FileDwg_Model ;
      currBlock : T_FileDwg_Block ;
    private
      procedure checkBlock ;
      procedure setEntityStyle( const _entity : T_FileDwg_Entity ;
                                const _attrib : DwgEntity
                               ) ;
      function  numberToColor ( const _color : DwgColor
                               ) : T_FileDwg_Color ;
      function  nameToLineType( const _name : String
                               ) : T_FileDwg_Pen ;
      function  numberToWidth ( const _lw : Integer
                               ) : T_FileDwg_Width ;
    public
      constructor Create      ( const _rdr   : TObject ;
                                const _model : T_FileDwg_Model
                               ) ;
      procedure addHeader     ( const _data : DwgHeader );
      procedure addLType      ( const _data : DwgLType );
      procedure addLayer      ( const _data : DwgLayer );
      procedure addDimStyle   ( const _data : DwgDimstyle );
      procedure addVport      ( const _data : DwgVport );
      procedure addTextStyle  ( const _data : DwgTextstyle );
      procedure addAppId      ( const _data : DwgAppId );
      procedure addBlock      ( const _data : DwgBlock );
      procedure endBlock      ;
      procedure addLine       ( const _data : DwgLine   ) ;
      procedure addPoint      ( const _data : DwgPoint  ) ;
      procedure addRay        ( const _data : DwgRay );
      procedure addXline      ( const _data : DwgXline );
      procedure addArc        ( const _data : DwgArc );
      procedure addCircle     ( const _data : DwgCircle );
      procedure addEllipse    ( const _data : DwgEllipse );
      procedure addLWPolyline ( const _data : DwgLWPolyline );
      procedure addPolyline   ( const _data : DwgPolyline );
      procedure addSpline     ( const _data : DwgSpline  );
      procedure addInsert     ( const _data : DwgInsert  );
      procedure addTrace      ( const _data : DwgTrace   );
      procedure add3dFace     ( const _data : Dwg3Dface  );
      procedure addSolid      ( const _data : DwgSolid   );
      procedure addMText      ( const _data : DwgMText   );
      procedure addText       ( const _data : DwgText    );
      procedure addHatch      ( const _data : DwgHatch   );
      procedure addAttrib     ( const _data : DwgAttrib  );
      procedure addAttDef     ( const _data : DwgAttDef  );
      procedure addLayout     ( const _data : DwgLayout  );
      procedure addLeader     ( const _data : DwgLeader  );
      procedure addColor      ( const _data : DwgDbColor );
      procedure addDimAligned ( const _data : DwgDimensionAligned ) ;
      procedure addDimLinear  ( const _data : DwgDimensionLinear ) ;
      procedure addDimOrdinate( const _data : DwgDimensionOrdinate ) ;
      procedure addDimDiameter( const _data : DwgDimensionDiametric ) ;
  end ;

  DwgReader = class
    {$IFDEF OXYGENE}unit {$ELSE} protected {$ENDIF}
      nextEntLink        : duint64;
      prevEntLink        : duint64;
      fileBuf            : DwgBuffer;
      version            : DWG_Version;
      decoder            : TEncoding;
      ownDecoder         : Boolean ;
      previewImagePos    : duint32;
      maintenanceVersion : dint16;
      iRdrClbk           : DwgReaderCallBack ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function readMetaData : Boolean; virtual;
      function readPreview : Boolean; virtual;
      function readFileHeader : Boolean; virtual;
      function readDwgHeader( const _hdr : DwgHeader ) : Boolean; virtual;
      function readDwgClasses : Boolean; virtual;
      function readDwgHandles : Boolean; virtual;
      function readDwgTables( const _hdr : DwgHeader ) : Boolean; virtual;
      function readDwgBlocks : Boolean; virtual;
      function readDwgEntities : Boolean; virtual;
      function readDwgObjects : Boolean; virtual;

      function readDwgEntity(
        const dbuf : DwgBuffer;
        var objH : DwgObjHandle
      ) : Boolean; virtual;
      function readDwgObject(
        const dbuf : DwgBuffer;
        const objH : DwgObjHandle
      ) : Boolean; virtual;
   {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function readDwgHeaderEx(
        const _hdr  : DwgHeader;
        const _buf  : DwgBuffer;
        const _hBuf : DwgBuffer
      ) : Boolean;
      function readDwgHandlesEx(
        const _dbuf   : DwgBuffer;
        const _offset : duint32;
        const _size   : duint32
      ) : Boolean;
      function readDwgTablesEx(
        const _hdr  : DwgHeader;
        const _dbuf : DwgBuffer
      ) : Boolean;
      function readDwgBlocksEx(
        const _dbuf : DwgBuffer
      ) : Boolean;
      function readDwgEntitiesEx(
        const _dbuf : DwgBuffer
      ) : Boolean;
      function readDwgObjectsEx(
        const _dbuf : DwgBuffer
      ) : Boolean;
      function checkSentinel(
        const _buf   : DwgBuffer;
        const _enum  : DWG_Section;
        const _start : Boolean
      ) : Boolean;
      procedure parseAttribs(
        const _e : DwgEntity
      ) ;
      function readPlineVertex(
        const _pline : DwgPolyline;
        const _dbuf  : DwgBuffer
      ) : Boolean;
      function findTable(
        const _table  : DWG_TTYPE ;
        const _handle : duint64
      ) : DwgTableEntry ;
     function findTableName(
        const _table  : DWG_TTYPE ;
        const _handle : duint64
      ) : String ;
      function prepareModel(
        const _hdr    : DwgHeader ;
        const _model  : T_FileDwg_Model
      ) : Boolean ;
      function setupDecoder(
        const _codepage : Integer
      ) : Boolean ;
    public
      mapSections         : TDictionary<DWG_Section, DwgSectionInfo>;
      mapClasses          : TDictionary<duint32, TObject>;
      mapHandles          : TDictionary<duint64, DwgObjHandle>;
      mapUnusedHandles    : TDictionary<duint64, DwgObjHandle>;
      mapRemainingHandles : TDictionary<duint64, DwgObjHandle>;
      mapLType            : TDictionary<duint64, DwgTableEntry>;
      mapLayers           : TDictionary<duint64, DwgTableEntry>;
      mapStyles           : TDictionary<duint64, DwgTableEntry>;
      mapDimStyles        : TDictionary<duint64, DwgTableEntry>;
      mapVPorts           : TDictionary<duint64, DwgTableEntry>;
      mapBlockRecords     : TDictionary<duint64, DwgTableEntry>;
      mapAppIds           : TDictionary<duint64, DwgTableEntry>;
    public
      constructor Create(
        const _stream  : TStream;
        const _version : DWG_Version
      ) ;
      {$IFDEF DCC}
      destructor Destroy; override;
      {$ENDIF}
  end ;

  DwgReader15 = class( DwgReader )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function readMetaData : Boolean; override;
      function readFileHeader : Boolean; override;
      function readDwgHeader( const _hdr : DwgHeader ) : Boolean; override;
      function readDwgClasses : Boolean; override;
      function readDwgHandles : Boolean; override;
      function readDwgTables( const _hdr : DwgHeader ) : Boolean; override;
      function readDwgBlocks : Boolean; override;
      function readDwgEntities : Boolean; override;
      function readDwgObjects : Boolean; override;
  end ;

  DwgReader18 = class( DwgReader )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      objData       : TBytes;
      uncompSize    : duint64;
      securityFlags : duint32;
    private
      function parseSysPage(
        const decompSec  : TBytes;
        const decompSize : duint32
      ) : Boolean;
      function parseDataPage( const _si : DwgSectionInfo ) : Boolean;
      function checksum(
        const seed : duint32;
        const data : TBytes;
        const sz   : duint32
      ) : duint32;
      procedure genMagicNumber;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function readMetaData : Boolean; override;
      function readFileHeader : Boolean; override;
      function readDwgHeader( const _hdr : DwgHeader ) : Boolean; override;
      function readDwgClasses : Boolean; override;
      function readDwgHandles : Boolean; override;
      function readDwgTables( const _hdr : DwgHeader ) : Boolean; override;
      function readDwgBlocks : Boolean; override;
      function readDwgEntities : Boolean; override;
      function readDwgObjects : Boolean; override;
  end ;

  DwgReader21 = class( DwgReader18 )
    private
      objData  : TBytes;
      dataSize : duint64;
    private
      function parseSysPage(
        const _sizeCompressed   : duint64;
        const _sizeUncompressed : duint64;
        const _correctionFactor : duint64;
        const _offset           : duint64;
        const _decompData       : TBytes
      ) : Boolean;
      function parseDataPage(
        const _si    : DwgSectionInfo;
        const _dData : TBytes
      ) : Boolean;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function readMetaData : Boolean; override;
      function readFileHeader : Boolean; override;
      function readDwgHeader( const _hdr : DwgHeader ) : Boolean; override;
      function readDwgClasses : Boolean; override;
      function readDwgHandles : Boolean; override;
      function readDwgTables( const _hdr : DwgHeader ) : Boolean; override;
      function readDwgBlocks : Boolean; override;
      function readDwgEntities : Boolean; override;
      function readDwgObjects : Boolean; override;
  end ;

  DwgReader24 = class( DwgReader18 )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function readFileHeader : Boolean; override;
      function readDwgHeader( const _hdr : DwgHeader ) : Boolean; override;
      function readDwgClasses : Boolean; override;
      function readDwgBlocks : Boolean; override;
      function readDwgEntities : Boolean; override;
      function readDwgObjects : Boolean; override;
  end ;

  DwgReader27 = class( DwgReader18 )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function readFileHeader : Boolean; override;
      function readDwgHeader( const _hdr : DwgHeader ) : Boolean; override;
      function readDwgClasses : Boolean; override;
      function readDwgBlocks : Boolean; override;
      function readDwgEntities : Boolean; override;
      function readDwgObjects : Boolean; override;
  end ;

  DwgReader32 = class( DwgReader18 )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      function readMetaData : Boolean; override;
  end ;

  DwgRSCodec = class
    public
      class procedure decode239I(
        const _in  : TBytes;
        const _out : TBytes;
        const _blk : duint32
      ) ;
      class procedure decode251I(
        const _in  : TBytes;
        const _out : TBytes;
        const _blk : duint32
      ) ;
  end ;

  DwgCompressor = class
    private
      class function litLength21(
        const cbuf : TBytes;
        const oc   : duint8;
        var si     : duint32
      ) : duint32;
      class procedure copyCompBytes21(
        const cbuf : TBytes;
        const dbuf : TBytes;
        const l    : duint32;
        const si   : duint32;
        const di   : duint32
      ) ;
      class procedure readInstructions21(
        const cbuf : TBytes;
        var si     : duint32;
        var oc     : duint8;
        var so     : duint32;
        var l      : duint32
      ) ;
    public
      class procedure decompress18(
        const _aSrc     : TBytes ;
        const _aDest    : TBytes ;
        const _aSrcSize : duint32 ;
        const _aDstSize : duint32
      ) ;
      class procedure decrypt18Hdr(
        const _buf    : TBytes;
        const _size   : duint32;
        const _offset : duint32
      ) ;
      class procedure decompress21(
        const _cbuf  : TBytes;
        const _dbuf  : TBytes;
        const _csize : duint32;
        const _dsize : duint32
      ) ;
  end ;

  RScodec = class
    private
      mm       : Integer;
      tt       : Integer;
      nn       : Integer;
      kk       : Integer;
      gg       : TArray<Integer>;
      isOk     : Boolean;
      index_of : TArray<Integer>;
      alpha_to : TArray<Integer>;
    private
      procedure RSgenerate_gf( const _pp : duint32 ) ;
      procedure RSgen_poly;
      function calcDecode(
        const _data : TBytes;
        const _recd : TArray<Integer>;
        const _elp  : array of TArray<Integer>;
        const _d    : TArray<Integer>;
        const _l    : TArray<Integer>;
        const _u_lu : TArray<Integer>;
        const _s    : TArray<Integer>;
        const _root : TArray<Integer>;
        const _loc  : TArray<Integer>;
        const _z    : TArray<Integer>;
        const _err  : TArray<Integer>;
        const _reg  : TArray<Integer>;
        const _bb   : dint32
      ) : dint32;
    public
      constructor Create(
        const _pp : duint32;
        const _mm : dint32;
        const _tt : dint32
      ) ;
      function decode( const _data : TBytes ) : dint32;
  end ;

const
  DwgmagicNum18 : Array [ 0 .. 107 ] of dint32 = ( $29, $23, $BE, $84, $E1,
    $6C, $D6, $AE, $52, $90, $49, $F1, $F1, $BB, $E9, $EB, $B3, $A6, $DB, $3C,
    $87, $0C, $3E, $99, $24, $5E, $0D, $1C, $06, $B7, $47, $DE, $B3, $12, $4D,
    $C8, $43, $BB, $8B, $A6, $1F, $03, $5A, $7D, $09, $38, $25, $1F, $5D, $D4,
    $CB, $FC, $96, $F5, $45, $3B, $13, $0D, $89, $0A, $1C, $DB, $AE, $32, $20,
    $9A, $50, $EE, $40, $78, $36, $FD, $12, $49, $32, $F6, $9E, $7D, $49, $DC,
    $AD, $4F, $14, $F2, $44, $40, $66, $D0, $6B, $C4, $30, $B7, $32, $3B, $A1,
    $22, $F6, $22, $91, $9D, $E1, $8B, $1F, $DA, $B0, $CA, $99, $02 ) ;

  DwgmagicNumEnd18 : Array [ 0 .. 19 ] of dint32 = ( $F8, $46, $6A, $04, $96,
    $73, $0E, $D9, $16, $2F, $67, $68, $D4, $F7, $4A, $4A, $D0, $57, $68, $76 ) ;

// ----------------------------------------------------------------------------
//  Various functions
// ----------------------------------------------------------------------------

  function getEnum( const _nameSec : String ) : DWG_Section;
  begin
    if ( _nameSec = 'AcDb:Header' ) then
      Result := DWG_Section.HEADER
    else if ( _nameSec = 'AcDb:Classes' ) then
      Result := DWG_Section.CLASSES
    else if ( _nameSec = 'AcDb:SummaryInfo' ) then
      Result := DWG_Section.SUMARYINFO
    else if ( _nameSec = 'AcDb:Preview' ) then
      Result := DWG_Section.PREVIEW
    else if ( _nameSec = 'AcDb:VBAProject' ) then
      Result := DWG_Section.VBAPROY
    else if ( _nameSec = 'AcDb:AppInfo' ) then
      Result := DWG_Section.APPINFO
    else if ( _nameSec = 'AcDb:FileDepList' ) then
      Result := DWG_Section.FILEDEP
    else if ( _nameSec = 'AcDb:RevHistory' ) then
      Result := DWG_Section.REVHISTORY
    else if ( _nameSec = 'AcDb:Security' ) then
      Result := DWG_Section.SECURITY
    else if ( _nameSec = 'AcDb:AcDbObjects' ) then
      Result := DWG_Section.OBJECTS
    else if ( _nameSec = 'AcDb:ObjFreeSpace' ) then
      Result := DWG_Section.OBJFREESPACE
    else if ( _nameSec = 'AcDb:Template' ) then
      Result := DWG_Section.TEMPLATE
    else if ( _nameSec = 'AcDb:Handles' ) then
      Result := DWG_Section.HANDLES
    else if ( _nameSec = 'AcDb:AcDsPrototype_1b' ) then
      Result := DWG_Section.PROTOTYPE
    else if ( _nameSec = 'AcDb:AuxHeader' ) then
      Result := DWG_Section.AUXHEADER
    else if ( _nameSec = 'AcDb:Signature' ) then
      Result := DWG_Section.SIGNATURE
    else if ( _nameSec = 'AcDb:AppInfoHistory' ) then
      Result := DWG_Section.APPINFOHISTORY
    else
      Result := DWG_Section.UNKNOWNS;
  end ;

  function convertAnsiString(
    const _bytes : TBytes
  ) : String;
  var
    i     : Integer;
    count : Integer;
  begin
    count := 0;
    for i := 0 to high( _bytes ) do begin
      if _bytes[i] = 0 then
        break;
      inc( count ) ;
    end ;

    if count > 0 then
      Result := TEncoding.ASCII.GetString( {$IFDEF JAVA}array of Byte{$ENDIF}(_bytes), 0, count )
    else
      Result := '';
  end ;

  procedure dwg_log( const _text : String ) ; {$IFDEF DCC} inline ; {$ENDIF}
  begin
    {$IFDEF GIS_DEBUG}
    OutputDebugString( PWideChar( StringReplaceAll( _text, '\n', #13#10 ) ) ) ;
    {$ENDIF}
  end ;

  function dwgVersionHigher(
    const _v1 : DWG_Version ;
    const _v2 : DWG_Version
  ) :Boolean ; {$IFDEF DCC} inline; {$ENDIF}
  begin
    Result := _v1 > _v2 ;
  end ;

  function dwgVersionLower(
    const _v1 : DWG_Version ;
    const _v2 : DWG_Version
  ) :Boolean ; {$IFDEF DCC} inline; {$ENDIF}
  begin
    Result := _v1 < _v2 ;
  end ;

  function dwgVersionEqual(
    const _v1 : DWG_Version ;
    const _v2 : DWG_Version
  ) :Boolean ; {$IFDEF DCC} inline; {$ENDIF}
  begin
    Result := _v1 = _v2 ;
  end ;

// ----------------------------------------------------------------------------
// DwgRSCodec
// ----------------------------------------------------------------------------

  class procedure DwgRSCodec.decode239I(
    const _in  : TBytes ;
    const _out : TBytes ;
    const _blk : duint32
  ) ;
  var
    k       : duint32;
    data    : TBytes;
    rsc     : RScodec;
    i, j, r : Integer;
  begin
    SetLength( data, 255 ) ;
    rsc := RScodec.Create( $96, 8, 8 ) ; // (255, 239)
    try
      for i := 0 to _blk - 1 do begin
        k := i;
        for j := 0 to 255 - 1 do begin
          data[j] := _in[ k ];
          k := k + _blk;
        end ;
        r := rsc.decode( data ) ;
        k := i * 239;
        for j := 0 to 239 - 1 do begin
          _out[ k ] := data[j];
          inc( k ) ;
        end ;
      end ;
    finally
      FreeObject( rsc ) ;
    end ;
  end ;

  class procedure DwgRSCodec.decode251I(
    const _in  : TBytes;
    const _out : TBytes;
    const _blk : duint32
  ) ;
  var
    k       : duint32;
    data    : TBytes;
    rsc     : RScodec;
    i, j, r : Integer;
  begin
    SetLength( data, 255 ) ;
    rsc := RScodec.Create( $B8, 8, 2 ) ; // (255, 251)
    try
      for i := 0 to _blk - 1 do begin
        k := i;
        for j := 0 to 255 - 1 do begin
          data[j] := _in[ k ];
          k := k + _blk;
        end ;
        r := rsc.decode( data ) ;
        k := i * 251;
        for j := 0 to 251 - 1 do begin
          _out[ k ] := data[j];
          inc( k ) ;
        end
      end ;
    finally
      FreeObject( rsc ) ;
    end ;
  end ;

// ----------------------------------------------------------------------------
// DwgCompressor
// ----------------------------------------------------------------------------

  class procedure DwgCompressor.copyCompBytes21(
    const cbuf : TBytes;
    const dbuf : TBytes;
    const l    : duint32;
    const si   : duint32;
    const di   : duint32
  ) ;
  var
    length, dix, six : duint32;
    i                : duint32;
  begin
    length := l;
    dix := di;
    six := si;

    while ( length > 31 ) do begin

      for i := six + 24 to six + 32 - 1 do begin
        dbuf[dix] := cbuf[i];
        inc( dix ) ;
      end ;
      for i := six + 16 to six + 24 - 1 do begin
        dbuf[dix] := cbuf[i];
        inc( dix ) ;
      end ;
      for i := six + 8 to six + 16 - 1 do begin
        dbuf[dix] := cbuf[i];
        inc( dix ) ;
      end ;
      for i := six to six + 8 - 1 do begin
        dbuf[dix] := cbuf[i];
        inc( dix ) ;
      end ;
      six := six + 32;
      length := length - 32;
    end ;

    case ( length ) of
      0 : begin
          end ;
      1 : begin
            dbuf[dix] := cbuf[six];
          end ;
      2 : begin
            dbuf[dix] := cbuf[ six + 1 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[six];
          end ;
      3 : begin
            dbuf[dix] := cbuf[ six + 2 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 1 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[six];
          end ;
      4 : begin
            for i := 0 to 4 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      5 : begin
            dbuf[dix] := cbuf[ six + 4 ];
            inc( dix ) ;
            for i := 0 to 4 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      6 : begin
            dbuf[dix] := cbuf[ six + 5 ];
            inc( dix ) ;
            for i := 1 to 5 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[six];
          end ;
      7 : begin
            dbuf[dix] := cbuf[ six + 6 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 5 ];
            inc( dix ) ;
            for i := 1 to 5 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[six];
          end ;
      8 : begin
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      9 : begin
            dbuf[dix] := cbuf[ six + 8 ];
            inc( dix ) ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      10 : begin
            dbuf[dix] := cbuf[ six + 9 ];
            inc( dix ) ;
            for i := 1 to 9 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[six];
          end ;
      11 : begin
            dbuf[dix] := cbuf[ six + 10 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 9 ];
            inc( dix ) ;
            for i := 1 to 9 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[six];
          end ;
      12 : begin
            for i := 8 to 12 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      13 : begin
            dbuf[dix] := cbuf[ six + 12 ];
            inc( dix ) ;
            for i := 8 to 12 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      14 : begin
            dbuf[dix] := cbuf[ six + 13 ];
            inc( dix ) ;
            for i := 9 to 13 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 1 to 9 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[six];
          end ;
      15 : begin
            dbuf[dix] := cbuf[ six + 14 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 13 ];
            inc( dix ) ;
            for i := 9 to 13 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 1 to 9 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[six];
          end ;
      16 : begin
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      17 : begin
            for i := 9 to 17 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[ six + 8 ];
            inc( dix ) ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      18 : begin
            dbuf[dix] := cbuf[ six + 17 ];
            inc( dix ) ;
            for i := 9 to 17 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 1 to 9 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[six];
          end ;
      19 : begin
            dbuf[dix] := cbuf[ six + 18 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 17 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 16 ];
            inc( dix ) ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      20 : begin
            for i := 16 to 20 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      21 : begin
            dbuf[dix] := cbuf[ six + 20 ];
            inc( dix ) ;
            for i := 16 to 20 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
          end ;
      22 : begin
            dbuf[dix] := cbuf[ six + 21 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 20 ];
            inc( dix ) ;
            for i := 16 to 20 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      23 : begin
            dbuf[dix] := cbuf[ six + 22 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 21 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 20 ];
            inc( dix ) ;
            for i := 16 to 20 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
          end ;
      24 : begin
            for i := 16 to 24 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      25 : begin
            for i := 17 to 25 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[ six + 16 ];
            inc( dix ) ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      26 : begin
            dbuf[dix] := cbuf[ six + 25 ];
            inc( dix ) ;
            for i := 17 to 25 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[ six + 16 ];
            inc( dix ) ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      27 : begin
            dbuf[dix] := cbuf[ six + 26 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 25 ];
            inc( dix ) ;
            for i := 17 to 25 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[ six + 16 ];
            inc( dix ) ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      28 : begin
            for i := 24 to 28 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 16 to 24 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      29 : begin
            dbuf[dix] := cbuf[ six + 28 ];
            inc( dix ) ;
            for i := 24 to 28 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 16 to 24 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      30 : begin
            dbuf[dix] := cbuf[ six + 29 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[ six + 28 ];
            inc( dix ) ;
            for i := 24 to 28 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 16 to 24 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 8 to 16 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 0 to 8 - 1 do begin
              dbuf[dix] := cbuf[six];
              inc( dix ) ;
              inc( six ) ;
            end ;
          end ;
      31 : begin
            dbuf[dix] := cbuf[ six + 30 ];
            inc( dix ) ;
            for i := 26 to 30 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 18 to 26 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 10 to 18 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            for i := 2 to 10 - 1 do begin
              dbuf[dix] := cbuf[six+i];
              inc( dix ) ;
            end ;
            dbuf[dix] := cbuf[ six + 1 ];
            inc( dix ) ;
            dbuf[dix] := cbuf[six];
          end ;
      else begin

      end ;
    end ;
  end ;

  class procedure DwgCompressor.decompress18(
    const _aSrc     : TBytes ;
    const _aDest    : TBytes ;
    const _aSrcSize : duint32 ;
    const _aDstSize : duint32
  ) ;
  var
    iDstCurr : duint32 ;
    iSrcCurr : duint32 ;
    iSrcEnd  : duint32 ;
    vOffset,
    vLength,
    vOpCode1,
    vOpCode2,
    vVal      : Integer ;
    vDstEnd   : duint32 ;

    function SAR(_value : Integer ; const _count : Byte) : Integer ; {$IFDEF DCC} inline ; {$ENDIF}
    begin
      Result := _value shr _count ;
    end ;

    procedure validate ;
    begin
      vVal := _aSrc[iSrcCurr] ;
      inc(iSrcCurr) ;
      if vVal <> 0 then
        Abort ;
    end ;

    procedure copy_compressed(_length, _offset : Integer) ;
    var
      vDstPrev : Cardinal ;
    begin
      vDstPrev := iDstCurr ;
      dec(vDstPrev, _offset) ;
      if _length < _offset then begin
        GisCopyMemory( _aDest, vDstPrev, _aDest, iDstCurr, _length ) ;
        inc(iDstCurr, _length) ;
      end
      else begin
        while True do begin
          dec(_length) ;
          if _length < 0 then exit ;
          _aDest[iDstCurr] := _aDest[vDstPrev] ;
          inc(iDstCurr) ;
          inc(vDstPrev) ;
        end ;
      end ;
    end ;

    procedure get_longLength(var _length : Integer) ;
    var
      vChar : Byte ;
    begin
      vChar := _aSrc[iSrcCurr] ;
      inc(iSrcCurr) ;
      while vChar = 0 do begin
        inc(_length, $FF) ;
        vChar := _aSrc[iSrcCurr] ;
        inc(iSrcCurr) ;
      end ;
      inc(_length, vChar) ;
    end ;

    procedure get_length(var _length : Integer ; _opCode, _aMask : Integer) ;
    begin
      _length := _opCode and _aMask ;
      if _length = 0 then begin
        _length := _aMask ;
        get_longLength(_length) ;
      end ;
      inc(_length, 2) ;
    end ;

    procedure get_literal(_opCode : Integer) ;
    var
      vLitLength : Integer ;
      vByte      : Byte ;
    begin
      vLitLength := _opCode and 3 ;
      if vLitLength = 0 then begin
        if _aSrc[iSrcCurr] and $0F0 = 0 then begin
          vByte := _aSrc[iSrcCurr] ;
          inc(iSrcCurr) ;
          get_length( vLitLength, vByte, $F ) ;
          inc(vLitLength) ;
        end ;
      end ;
      if vLitLength = 0 then exit ;
      GisCopyMemory( _aSrc, iSrcCurr, _aDest, iDstCurr, vLitLength ) ;
      inc(iSrcCurr, vLitLength) ;
      inc(iDstCurr, vLitLength) ;
    end ;

    procedure get_offset(var _aOpCode1, _aOpCode2, _offset : Integer) ;
    begin
      _aOpCode1 := _aSrc[iSrcCurr] ;
      inc(iSrcCurr) ;
      _aOpCode2 := _aSrc[iSrcCurr] ;
      inc(iSrcCurr) ;
      _offset := SAR(_aOpCode1, 2) or _offset ;
      _offset := (_aOpCode2 shl 6) or _offset ;
    end ;

  begin
    if (_aSrc = nil) or (_aDest = nil) or (_aSrcSize = 0) or (_aDstSize = 0) then exit ;

    iDstCurr := 0 ;
    vDstEnd  := 0 ;
    inc(vDstEnd, _aDstSize) ;
    iSrcCurr := 0 ;
    iSrcEnd  := 0 ;
    inc(iSrcEnd, _aSrcSize) ;
    vOpCode1 := 0 ;
    vOpCode2 := 0 ;

    while iSrcCurr < iSrcEnd do begin
      vOffset := 0 ;
      vLength := 0 ;
      get_literal(vOpCode1) ;
      vOpCode1 := _aSrc[iSrcCurr] ;
      inc(iSrcCurr) ;

      while True do begin
        if vOpCode1 <> $11 then break ;
        validate ;
        validate ;
        exit ;
      end ;

      if iDstCurr > vDstEnd then
        exit ;

      if (vOpCode1 < $10) or (vOpCode1 >= $40) then begin
        vOpCode2 := _aSrc[iSrcCurr] ;
        inc(iSrcCurr) ;
        vOffset := (SAR(vOpCode1, 2) and 3) or (vOpCode2 shl 2) ;
        vLength := SAR(vOpCode1, 4) ;
        dec(vLength) ;
        inc(vOffset) ;
      end
      else begin
        if vOpCode1 < $20 then begin
          get_length( vLength, vOpCode1, 7 ) ;
          vOffset := (vOpCode1 and 8) shl $B ;
          get_offset( vOpCode1, vOpCode2, vOffset ) ;
          inc(vOffset, $4000) ;
        end
        else begin
          get_length( vLength, vOpCode1, $1F ) ;
          get_offset( vOpCode1, vOpCode2, vOffset ) ;
          inc(vOffset) ;
        end ;
      end ;
      copy_compressed( vLength, vOffset ) ;
    end ;
  end ;

  class procedure DwgCompressor.decompress21(
    const _cbuf  : TBytes;
    const _dbuf  : TBytes;
    const _csize : duint32;
    const _dsize : duint32
  ) ;
  var
    srcIndex     : duint32;
    dstIndex     : duint32;
    length       : duint32;
    sourceOffset : duint32;
    opCode       : duint8;
    i            : duint32;
  begin
    srcIndex := 0;
    dstIndex := 0;
    length := 0;

    opCode := _cbuf[srcIndex];
    inc( srcIndex ) ;
    if ( ( opCode shr 4 ) = 2 ) then begin
      srcIndex := srcIndex + 2;
      length := _cbuf[srcIndex] and $07;
      inc( srcIndex ) ;
    end ;

    while ( ( srcIndex < _csize ) and ( dstIndex < _dsize + 1 ) ) do begin
      if ( length = 0 ) then
        length := litLength21( _cbuf, opCode, srcIndex ) ;
      copyCompBytes21( _cbuf, _dbuf, length, srcIndex, dstIndex ) ;
      srcIndex := srcIndex + length;
      dstIndex := dstIndex + length;
      if ( dstIndex >= _dsize ) then
        break;

      length := 0;
      opCode := _cbuf[srcIndex];
      inc( srcIndex ) ;
      readInstructions21( _cbuf, srcIndex, opCode, sourceOffset, length ) ;
      while ( true ) do begin
        if ( sourceOffset > dstIndex ) then begin
          sourceOffset := dstIndex;
        end ;

        if ( length > _dsize - dstIndex ) then begin
          length := _dsize - dstIndex;
          srcIndex := _csize; // force exit
        end ;
        sourceOffset := dstIndex - sourceOffset;
        for i := 0 to length - 1 do begin
          _dbuf[ dstIndex ] := _dbuf[ sourceOffset + i ];
          inc( dstIndex ) ;
        end ;

        length := opCode and 7;
        if ( ( length <> 0 ) or ( srcIndex >= _csize ) ) then
          break;
        opCode := _cbuf[srcIndex];
        inc( srcIndex ) ;
        if ( ( opCode shr 4 ) = 0 ) then
          break;

        if ( ( opCode shr 4 ) = 15 ) then
          opCode := opCode and 15;

        readInstructions21( _cbuf, srcIndex, opCode, sourceOffset, length ) ;
      end ;
    end ;
  end ;

  class procedure DwgCompressor.decrypt18Hdr(
    const _buf    : TBytes;
    const _size   : duint32;
    const _offset : duint32
  ) ;
  var
    max     : duint8;
    secMask : duint32;
    j       : Integer;
    Hdr     : duint32;
    pHdr    : duint32;
    {$IFDEF OXYGENE}
    buf     : array of Byte ;
    {$ENDIF}
  begin
    max := _size div 4;
    secMask := $4164536B xor _offset;
    pHdr := 0;
    {$IFDEF OXYGENE}
      buf := {$IFDEF JAVA}array of Byte{$ENDIF}(_buf) ;
    {$ENDIF}
    for j := 0 to max - 1 do begin
      {$IFDEF OXYGENE}
        Hdr := BitConverter.ToUInt32( buf, pHdr ) ;
      {$ELSE}
        Move( _buf[ pHdr ], Hdr, 4 ) ;
      {$ENDIF}
        Hdr := Hdr xor secMask;
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          System.Buffer.BlockCopy( BitConverter.GetBytes( Hdr ), 0, _buf, pHdr, 4 ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          System.arraycopy( BitConverter.GetBytes( Hdr ), 0, _buf, pHdr, 4 ) ;
        {$ENDIF}
      {$ELSE}
        Move( Hdr, _buf[ pHdr ], 4 ) ;
      {$ENDIF}
      inc( pHdr, 4 ) ;
    end ;
  end ;

  class function DwgCompressor.litLength21(
    const cbuf : TBytes;
    const oc   : duint8;
      var si   : duint32
  ) : duint32;
  var
    srcIndex : duint32;
    length   : duint32;
    n        : duint32;
  begin
    srcIndex := si;
    length := oc + 8;
    if ( length = $17 ) then begin
      n := cbuf[srcIndex];
      inc( srcIndex ) ;
      length := length + n;
      if ( n = $FF ) then begin
        repeat
          n := cbuf[srcIndex];
          inc( srcIndex ) ;
          n := n or duint32( cbuf[srcIndex] shl 8 ) ;
          inc( srcIndex ) ;
          length := length + n;
        until ( n <> $FFFF ) ;
      end
    end ;

    si := srcIndex;
    Result := length;
  end ;

  class procedure DwgCompressor.readInstructions21(
    const cbuf : TBytes;
    var si     : duint32;
    var oc     : duint8;
    var so, l  : duint32
  ) ;
  var
    length       : duint32;
    srcIndex     : duint32;
    sourceOffset : duint32;
    opCode       : duint8;
  begin
    srcIndex := si;
    opCode := oc;
    case ( ( opCode shr 4 ) ) of
      0 : begin
            length := ( opCode and $F ) + $13;
            sourceOffset := cbuf[srcIndex];
            inc( srcIndex ) ;
            opCode := cbuf[srcIndex];
            inc( srcIndex ) ;
            length := ( ( opCode shr 3 ) and $10 ) + length;
            sourceOffset := ( ( opCode and $78 ) shl 5 ) + 1 + sourceOffset;
          end ;
      1 : begin
            length := ( opCode and $F ) + 3;
            sourceOffset := cbuf[srcIndex];
            inc( srcIndex ) ;
            opCode := cbuf[srcIndex];
            inc( srcIndex ) ;
            sourceOffset := ( ( opCode and $F8 ) shl 5 ) + 1 + sourceOffset;
          end ;
      2 : begin
            sourceOffset := cbuf[srcIndex];
            inc( srcIndex ) ;
            sourceOffset := ( ( cbuf[srcIndex] shl 8 ) and $FF00 ) or
                            sourceOffset;
            inc( srcIndex ) ;
            length := opCode and 7;
            if ( ( opCode and 8 ) = 0 ) then begin
              opCode := cbuf[srcIndex];
              inc( srcIndex ) ;
              length := ( opCode and $F8 ) + length;
            end
            else begin
              inc( sourceOffset ) ;
              length := ( cbuf[srcIndex] shl 3 ) + length;
              inc( srcIndex ) ;
              opCode := cbuf[srcIndex];
              inc( srcIndex ) ;
              length := ( ( ( opCode and $F8 ) shl 8 ) + length ) + $100;
            end
          end
      else begin
          length := opCode shr 4;
          sourceOffset := opCode and 15;
          opCode := cbuf[srcIndex];
          inc( srcIndex ) ;
          sourceOffset := ( ( ( opCode and $F8 ) shl 1 ) + sourceOffset ) + 1;
      end
    end ;

    oc := opCode;
    si := srcIndex;
    so := sourceOffset;
    l := length;
  end ;

// ----------------------------------------------------------------------------
// RScodec
// ----------------------------------------------------------------------------

 // Reed Solomon code inspired by encoder/decoder written by Simon Rockliff

  constructor RScodec.Create(
    const _pp      : duint32;
    const _mm, _tt : dint32
  ) ;
  begin
    inherited Create;

    mm := _mm;
    tt := _tt;
    nn := ( 1 shl mm ) - 1; // mm==8 nn=255
    kk := nn - ( tt * 2 ) ;
    isOk := true;

    SetLength( alpha_to, nn + 1 ) ;
    SetLength( index_of, nn + 1 ) ;
    SetLength( gg, nn - kk + 1 ) ;

    RSgenerate_gf( _pp ) ;
    RSgen_poly;
  end ;

  function RScodec.decode(
    const _data : TBytes
  ) : dint32 ;
  var
    bb, i                  : Integer;
    recd, d, l, u_lu, s,
    root, loc, z, err, reg : TArray<Integer>;
    elp                    : array of TArray<Integer>;
  begin
    if not isOk then begin
      Result := - 1;
      exit;
    end ;
    bb := nn - kk;

    SetLength( recd, nn ) ;
    SetLength( elp, bb + 2 ) ;
    for i := 0 to bb + 2 - 1 do
      SetLength( elp[i], bb ) ;

    SetLength( d, bb + 2 ) ;
    SetLength( l, bb + 2 ) ;
    SetLength( u_lu, bb + 2 ) ;
    SetLength( s, bb + 1 ) ;
    SetLength( root, tt ) ;
    SetLength( loc, tt ) ;
    SetLength( z, tt + 1 ) ;
    SetLength( err, nn ) ;
    SetLength( reg, tt + 1 ) ;

    Result := calcDecode( _data, recd, elp, d, l, u_lu, s, root, loc, z,
                          err, reg, bb ) ;
  end ;

  procedure RScodec.RSgenerate_gf(
    const _pp : duint32
  );
  var
    i, mask, pb : Integer;
  begin
    mask := 1;
    alpha_to[ mm ] := 0;
    for i := 0 to mm - 1 do begin
      alpha_to[i] := mask;
      index_of[ alpha_to[i] ] := i;
      pb := ( _pp shr ( mm - 1 - i ) ) and 1;
      if ( pb <> 0 ) then
        alpha_to[ mm ] := alpha_to[ mm ] xor mask;
      mask := mask shl 1;
    end ;
    index_of[ alpha_to[ mm ] ] := mm;
    mask := mask shr 1;
    for i := mm + 1 to nn - 1 do begin
      if ( alpha_to[ i - 1 ] >= mask ) then
        alpha_to[i] := alpha_to[ mm ]
          xor ( ( alpha_to[ i - 1 ] xor mask ) shl 1 )
      else
        alpha_to[i] := alpha_to[ i - 1 ] shl 1;
      index_of[ alpha_to[i] ] := i;
    end ;
    index_of[0] := ( - 1 ) ;
  end ;

  procedure RScodec.RSgen_poly;
  var
    i, j, tmp, bb : Integer;
  begin
    bb := nn - kk; // nn-kk length of parity data

    gg[0] := 2;
    gg[1] := 1;
    for i := 2 to bb do begin
      gg[i] := 1;
      for j := i - 1 downto 1 do begin
        if ( gg[j] <> 0 ) then begin
          if ( gg[j] < 0 ) then begin
            isOk := false;
            exit;
          end ;
          tmp := ( index_of[ gg[j] ] + i ) mod nn;
          if ( tmp < 0 ) then begin
            isOk := false;
            exit;
          end ;
          gg[j] := gg[j-1] xor alpha_to[tmp];
        end
        else
          gg[j] := gg[j-1];
      end ;

      gg[0] := alpha_to[ ( index_of[ gg[0] ] + i ) mod nn ];
    end ;

    for i := 0 to bb do
      gg[i] := index_of[ gg[i] ];

  end ;

  function RScodec.calcDecode(
    const _data : TBytes;
    const _recd : TArray<Integer>;
    const _elp  : array of TArray<Integer>;
    const _d    : TArray<Integer>;
    const _l    : TArray<Integer>;
    const _u_lu : TArray<Integer>;
    const _s    : TArray<Integer>;
    const _root : TArray<Integer>;
    const _loc  : TArray<Integer>;
    const _z    : TArray<Integer>;
    const _err  : TArray<Integer>;
    const _reg  : TArray<Integer>;
    const _bb   : dint32
  ) : dint32;
  var
    count, syn_error, i, j, u, q : Integer;
  begin
    if not isOk then begin
      Result := - 1;
      exit;
    end ;
    syn_error := 0;

    j := _bb;
    for i := 0 to kk - 1 do begin
      _recd[j] := index_of[ _data[j] ];
      inc( j ) ;
    end ;
    j := 0;
    for i := kk to nn - 1 do begin
      _recd[j] := index_of[ _data[j] ];
      inc( j ) ;
    end ;

    for i := 1 to _bb do begin
      _s[i] := 0;
      for j := 0 to nn - 1 do begin
        if ( _recd[j] <> - 1 ) then
          _s[i] := _s[i] xor alpha_to[ ( _recd[j] + i * j ) mod nn ];
      end ;

      if ( _s[i] <> 0 ) then
        syn_error := 1;
      _s[i] := index_of[ _s[i] ];
    end ;

    if syn_error = 0 then begin
      Result := 0;
      exit;
    end ;

    _d[0] := 0;
    _d[1] := _s[1];
    _elp[0][0] := 0;
    _elp[1][0] := 1;
    for i := 1 to _bb - 1 do begin
      _elp[0][i] := - 1;
      _elp[1][i] := 0;
    end ;
    _l[0] := 0;
    _l[1] := 0;
    _u_lu[0] := - 1;
    _u_lu[1] := 0;
    u := 0;

    repeat
      inc( u ) ;
      if ( _d[u] = - 1 ) then begin
        _l[u+1] := _l[u];
        for i := 0 to _l[u] do begin
          _elp[u+1][i] := _elp[u][i];
          _elp[u][i] := index_of[ _elp[u][i] ];
        end
      end
      else begin
        q := u - 1;
        while ( ( _d[q] = - 1 ) and ( q > 0 ) ) do
          dec( q ) ;

        if ( q > 0 ) then begin
          j := q;
          repeat
            dec( j ) ;
            if ( ( _d[j] <> - 1 ) and ( _u_lu[q] < _u_lu[j] ) ) then
              q := j;
          until not ( j > 0 ) ;
        end ;

        if ( _l[u] > _l[q] + u - q ) then
          _l[u+1] := _l[u]
        else
          _l[u+1] := _l[q] + u - q;

        for i := 0 to _bb - 1 do
          _elp[u+1][i] := 0;
        for i := 0 to _l[q] do begin
          if ( _elp[q][i] <> - 1 ) then
            _elp[u+1][ i + u - q ] :=
              alpha_to[ ( _d[u] + nn - _d[q] + _elp[q][i] ) mod nn ];
        end ;
        for i := 0 to _l[u] do begin
          _elp[u+1][i] := _elp[u+1][i] xor _elp[u][i];
          _elp[u][i] := index_of[ _elp[u][i] ];
        end ;
      end ;
      _u_lu[u+1] := u - _l[u+1];

      if ( u < _bb ) then begin
        if ( _s[u+1] <> - 1 ) then
          _d[u+1] := alpha_to[ _s[u+1] ]
        else
          _d[u+1] := 0;

        for i := 1 to _l[u+1] do begin
          if ( ( _s[ u + 1 - i ] <> - 1 ) and ( _elp[u+1][i] <> 0 ) ) then
            _d[u+1] := _d[u+1] xor alpha_to
              [ ( _s[ u + 1 - i ] + index_of[ _elp[u+1][i] ] ) mod nn ];
        end ;
        _d[u+1] := index_of[ _d[u+1] ];
      end ;
    until not ( ( u < _bb ) and ( _l[u+1] <= tt ) ) ;

    inc( u ) ;
    if ( _l[u] > tt ) then begin
      Result := - 1;
      exit;
    end ;

    for i := 0 to _l[u] do
      _elp[u][i] := index_of[ _elp[u][i] ];

    for i := 1 to _l[u] do
      _reg[i] := _elp[u][i];

    count := 0;
    for i := 1 to nn do begin
      q := 1;
      for j := 1 to _l[u] do begin
        if ( _reg[j] <> - 1 ) then begin
          _reg[j] := ( _reg[j] + j ) mod nn;
          q := q xor alpha_to[ _reg[j] ];
        end ;
      end ;
      if q = 0 then begin
        _root[ count ] := i;
        _loc[ count ] := nn - i;
        inc( count ) ;
      end ;
    end ;

    if ( count <> _l[u] ) then begin
      Result := - 1;
      exit;
    end ;

    for i := 1 to _l[u] do begin
      if ( ( _s[i] <> - 1 ) and ( _elp[u][i] <> - 1 ) ) then
        _z[i] := alpha_to[ _s[i] ] xor alpha_to[ _elp[u][i] ]
      else if ( ( _s[i] <> - 1 ) and ( _elp[u][i] = - 1 ) ) then
        _z[i] := alpha_to[ _s[i] ]
      else if ( ( _s[i] = - 1 ) and ( _elp[u][i] <> - 1 ) ) then
        _z[i] := alpha_to[ _elp[u][i] ]
      else
        _z[i] := 0;

      for j := 1 to i - 1 do begin
        if ( ( _s[j] <> - 1 ) and ( _elp[u][i-j] <> - 1 ) ) then
          _z[i] := _z[i] xor alpha_to
            [ ( _elp[u][i-j] + _s[j] ) mod nn ];

      end ;
      _z[i] := index_of[ _z[i] ];
    end ;

    for i := 0 to nn - 1 do
      _err[i] := 0;
    for i := 0 to _l[u] - 1 do begin
      _err[ _loc[i] ] := 1;
      for j := 1 to _l[u] do begin
        if ( _z[j] <> - 1 ) then
          _err[ _loc[i] ] := _err[ _loc[i] ] xor alpha_to
            [ ( _z[j] + j * _root[i] ) mod nn ];
      end ;
      if ( _err[ _loc[i] ] <> 0 ) then begin
        _err[ _loc[i] ] := index_of[ _err[ _loc[i] ] ];
        q := 0;
        for j := 0 to _l[u] - 1 do begin
          if ( j <> i ) then
            q := q + index_of
              [ 1 xor alpha_to[ ( _loc[j] + _root[i] ) mod nn ] ];
        end ;
        q := q mod nn;
        _err[ _loc[i] ] := alpha_to[ ( _err[ _loc[i] ] - q + nn ) mod nn ];
        _data[ _loc[i] ] := _data[ _loc[i] ] xor _err[ _loc[i] ];
      end ;
    end ;
    Result := count;
  end ;

// ----------------------------------------------------------------------------
// DwgColor
// ----------------------------------------------------------------------------

  constructor DwgColor.Create(
    const _ctype : DwgColorType ;
    const _value : duint32
  ) ;
  begin
    CType := _ctype ;
    Value := _value ;
  end ;

  function DwgColor.ToArray : TArray<duint32>;
  begin
    SetLength( Result, 2 ) ;
    Result[0] := ord(CType) ;
    Result[1] := Value ;
  end ;

  function DwgColor.ToString : String ;
  begin
    case CType of
      DwgColorType.ByBlock     : Result := 'ByBlock ';
      DwgColorType.ByLayer     : Result := 'ByLayer ';
      DwgColorType.ByRGB       : Result := 'ByRGB ';
      DwgColorType.ByIndex     : Result := 'ByIndex ';
      DwgColorType.ByReference : Result := 'ByReference ';
    end ;
    Result := Result + IntToStr( Value ) ;
  end ;

// ----------------------------------------------------------------------------
// DwgBuffer
// ----------------------------------------------------------------------------

  constructor DwgBuffer.Create;
  begin
    inherited Create;

    filestr := nil ;
    maxSize := 0;
    bitPos  := 0;
    SetLength( currByte, 1 ) ;
  end ;

  constructor DwgBuffer.Create(
    const _buf     : TBytes;
    const _size    : Integer;
    const _decoder : TEncoding
  ) ;
  begin
    inherited Create;

    filestr := DwgByteStream.Create( _buf, _size ) ;
    maxSize := size;
    bitPos  := 0;
    SetLength( currByte, 1 ) ;
    decoder := _decoder ;
  end ;

  constructor DwgBuffer.Create(
    const _stream : TStream
  ) ;
  begin
    inherited Create;

    filestr := DwgFileStream.Create( _stream ) ;
    maxSize := filestr.Size;
    bitPos  := 0;
    SetLength( currByte, 1 ) ;
  end ;

  constructor DwgBuffer.Create(
    const _org : DwgBuffer
  ) ;
  begin
    inherited Create;

    filestr := _org.filestr.clone;
    maxSize := filestr.Size;
    SetLength( currByte, 1 ) ;
    currByte[0] := _org.currByte[0];
    bitPos := _org.bitPos;
  end ;

  function DwgBuffer.clone : DwgBuffer;
  begin
    Result := DwgBuffer.Create;
    Result.filestr := filestr.clone;
    Result.decoder := decoder;
    Result.maxSize := filestr.Size;
    SetLength( Result.currByte, 1 ) ;
    Result.currByte[0] := currByte[0];
    Result.bitPos := bitPos;
  end ;

  {$IFNDEF OXYGENE}
    destructor DwgBuffer.Destroy ;
    begin
      FreeObject( filestr ) ;
      inherited ;
    end ;
  {$ENDIF}

  function DwgBuffer.size : duint64;
  begin
    Result := filestr.Size;
  end ;

  procedure DwgBuffer.resetPosition;
  begin
    setPosition( 0 ) ;
    setBitPos( 0 ) ;
  end ;

  function DwgBuffer.getBitPos : duint8;
  begin
    Result := bitPos;
  end ;

  function DwgBuffer.isGood : Boolean;
  begin
    Result := filestr.good;
  end ;

  function DwgBuffer.numRemainingBytes : Integer;
  begin
    Result := Integer( maxSize - filestr.getPos ) ;
  end ;

  function DwgBuffer.getPosition : duint64;
  begin
    if ( bitPos <> 0 ) then
      Result := filestr.getPos - 1
    else
      Result := filestr.getPos;
  end ;

  function DwgBuffer.setPosition(
    const _pos : duint64
  ) : Boolean;
  begin
    bitPos := 0;
    Result := filestr.setPos( _pos ) ;
  end ;

  procedure DwgBuffer.setBitPos(
    const _pos : duint8
  ) ;
  var
    buffer : TBytes;
  begin
    if ( _pos > 7 ) then
      exit;
    if ( _pos <> 0 ) and ( bitPos = 0 ) then begin
      SetLength( buffer, 1 ) ;
      filestr.read( buffer, 1 ) ;
      currByte[0] := buffer[0];
    end ;
    if ( _pos = 0 ) and ( bitPos <> 0 ) then begin // reset current byte
      filestr.setPos( filestr.getPos - 1 ) ;
    end ;
    bitPos := _pos;
  end ;

  function DwgBuffer.moveBitPos(
    const _size : dint32
  ) : Boolean;
  var
    b   : dint32;
    p   : dint32;
    pos : dint64;
  begin
    if ( size = 0 ) then begin
      Result := true;
      exit;
    end ;
    b := _size + bitPos;
    if b < 0 then
      p := ( b shr 3 ) or ( ( 0 - ( ( b shr 31 ) and 1 ) ) shl ( 32 - 3 ) )
    else
      p := b shr 3;

    pos := Int64( getPosition ) + p;
    filestr.setPos( pos ) ;
    bitPos := b and 7;

    if ( bitPos <> 0 ) then begin
      filestr.read( currByte, 1 ) ;
    end ;
    Result := filestr.good;
  end ;

  function DwgBuffer.getBit : duint8;
  var
    ret : duint8;
  begin
    if ( bitPos = 0 ) then begin
      filestr.read( currByte, 1 ) ;
    end ;

    ret := ( currByte[0] shr ( 7 - bitPos ) and 1 ) ;
    bitPos := bitPos + 1;
    if ( bitPos = 8 ) then
      bitPos := 0;

    Result := ret;
  end ;

  function DwgBuffer.getBoolBit : Boolean;
  begin
    Result := ( getBit <> 0 ) ;
  end ;

  function DwgBuffer.get2Bits : duint8;
  var
    ret : duint8;
  begin
    if ( bitPos = 0 ) then begin
      filestr.read( currByte, 1 ) ;
    end ;

    bitPos := bitPos + 2;
    if ( bitPos < 9 ) then
      ret := currByte[0] shr ( 8 - bitPos )
    else begin // read one bit per byte
      ret := duint8( currByte[0] shl 1 ) ;
      filestr.read( currByte, 1 ) ;
      bitPos := 1;
      ret := ret or currByte[0] shr 7;
    end ;
    if ( bitPos = 8 ) then
      bitPos := 0;
    ret := ret and 3;
    Result := ret;
  end ;

  function DwgBuffer.get3Bits : duint8;
  var
    ret : duint8;
  begin
    if ( bitPos = 0 ) then begin
      filestr.read( currByte, 1 ) ;
    end ;

    bitPos := bitPos + 3;
    if ( bitPos < 9 ) then
      ret := currByte[0] shr ( 8 - bitPos )
    else begin // read one bit per byte
      ret := currByte[0] shl 1;
      filestr.read( currByte, 1 ) ;
      bitPos := 1;
      ret := ret or currByte[0] shr 7;
    end ;
    if ( bitPos = 8 ) then
      bitPos := 0;
    ret := ret and 7;
    Result := ret;
  end ;

  function DwgBuffer.getBitShort : duint16;
  var
    b : duint8;
  begin
    b := get2Bits;
    if ( b = 0 ) then
      Result := getRawShort16
    else if ( b = 1 ) then
      Result := $FF and getRawChar8
    else if ( b = 2 ) then
      Result := 0
    else
      Result := 256;
  end ;

  function DwgBuffer.getSBitShort : dint16;
  var
    b : duint8;
  begin
    b := get2Bits;
    if ( b = 0 ) then
      Result := dint16( getRawShort16 )
    else if ( b = 1 ) then
      Result := dint16( getRawChar8 )
    else if ( b = 2 ) then
      Result := 0
    else
      Result := 256;
  end ;

  function DwgBuffer.getBitLong : dint32;
  var
    b : dint8;
  begin
    b := get2Bits;
    if ( b = 0 ) then
      Result := dint32( getRawLong32 )
    else if ( b = 1 ) then
      Result := getRawChar8
    else
      Result := 0;
  end ;

  function DwgBuffer.getBitLongLong : duint64;
  var
    b   : dint8;
    ret : duint64;
    i   : duint8;
  begin
    b := get3Bits;
    ret := 0;
    if b > 0 then
      for i := 0 to b - 1 do begin
        ret := ret shl 8;
        ret := ret or getRawChar8;
      end ;
    Result := ret;
  end ;

  function DwgBuffer.getBitDouble : Double;
  var
    b      : dint8;
    buffer : TBytes ;
    i      : Integer;
    ret    : Double;
  begin
    b := get2Bits;
    if ( b = 1 ) then
      Result := 1.0
    else if ( b = 0 ) then begin
      SetLength( buffer, 8 ) ;
      if ( bitPos <> 0 ) then begin
        for i := 0 to 8 - 1 do
          buffer[i] := getRawChar8;
      end
      else begin
        filestr.read( buffer, 8 ) ;
      end ;
      {$IFDEF OXYGENE}
        ret := BitConverter.ToDouble( {$IFDEF JAVA}array of Byte{$ENDIF}(buffer), 0 ) ;
      {$ELSE}
        Move( buffer[0], ret, 8 ) ;
      {$ENDIF}
      Result := ret;
      exit;
    end
    else
      Result := 0.0;
  end ;

  function DwgBuffer.get3BitDouble : DwgCoord;
  var
    crd : DwgCoord ;
  begin
    {$IFDEF GIS_NORECORDS}
    crd := new DwgCoord(0, 0, 0) ;
    {$ENDIF}
    crd.x := getBitDouble;
    crd.y := getBitDouble;
    crd.z := getBitDouble;
    Result := crd;
  end ;

  function DwgBuffer.getRawChar8 : duint8;
  var
    buffer : TBytes;
    ret    : duint8;
  begin
    SetLength( buffer, 1 ) ;
    filestr.read( buffer, 1 ) ;
    if ( bitPos = 0 ) then begin
      Result := $FF and buffer[0];
      exit;
    end
    else begin
      {$IFDEF JAVA}
        ret := $FF and (Integer( Integer($FF and currByte[0]) shl bitPos ) ) ;
        currByte[0] := buffer[0];
        ret := $FF and (ret or ( currByte[0] shr ( 8 - bitPos ) )) ;
      {$ELSE}
        ret := duint8( currByte[0] shl bitPos ) ;
        currByte[0] := buffer[0];
        ret := ret or ( currByte[0] shr ( 8 - bitPos ) ) ;
      {$ENDIF}
    end ;
    Result := ret;
  end ;

  function DwgBuffer.getRawShort16 : duint16;
  var
    ret    : {$IFDEF JAVA} Integer {$ELSE} duint16 {$ENDIF} ;
    buffer : TBytes;
  begin
    SetLength( buffer, 2 ) ;

    filestr.read( buffer, 2 ) ;
    if ( bitPos = 0 ) then begin
      ret := ( buffer[ 1 ] shl 8 ) or ( buffer[0] and $00FF ) ;
    end
    else begin
      ret := ( buffer[0] shl 8 ) or ( buffer[ 1 ] and $00FF ) ;
      ret := ret shr ( 8 - bitPos ) ;
      ret := duint16( ret or ( currByte[0] shl ( 8 + bitPos ) ) ) ;
      currByte[0] := buffer[ 1 ];
      ret := duint16( ( ret shl 8 ) or ( ret shr 8 ) ) ;
    end ;
    Result := ret;
  end ;

  function DwgBuffer.getRawDouble : Double;
  var
    buffer  : TBytes ;
    i       : Integer;
    nOffset : Double;
  begin
    SetLength( buffer, 8 ) ;
    if ( bitPos = 0 ) then
      filestr.read( buffer, 8 )
    else begin
      for i := 0 to 8 - 1 do
        buffer[i] := getRawChar8;
    end ;
    {$IFDEF OXYGENE}
      nOffset := BitConverter.ToDouble( {$IFDEF JAVA}array of Byte{$ENDIF}(buffer), 0 ) ;
    {$ELSE}
      Move( buffer[0], nOffset, 8 ) ;
    {$ENDIF}
    if IsNan(nOffset) or (Abs(nOffset) > 1E+101) then
      nOffset := 0 ;
    Result := nOffset;
  end ;

  function DwgBuffer.get2RawDouble : DwgCoord;
  var
    crd : DwgCoord;
  begin
    {$IFDEF GIS_NORECORDS}
    crd := new DwgCoord(0, 0, 0) ;
    {$ENDIF}
    crd.x := getRawDouble;
    crd.y := getRawDouble;
    Result := crd;
  end ;

  function DwgBuffer.getRawLong32 : duint32;
  var
    tmp1 : duint16;
    tmp2 : duint16;
    ret  : duint32;
  begin
    tmp1 := getRawShort16;
    tmp2 := getRawShort16;
    ret := ( tmp2 shl 16 ) or ( tmp1 and $0000FFFF ) ;

    Result := ret;
  end ;

  function DwgBuffer.getRawLong64 : duint64;
  var
    tmp1 : duint32;
    tmp2 : duint64;
    ret  : duint64;
  begin
    tmp1 := getRawLong32;
    tmp2 := getRawLong32;
    ret := duint64(( tmp2 shl 32 ) or ( tmp1 and $00000000FFFFFFFF )) ;

    Result := ret;
  end ;

  function DwgBuffer.getRL32 : duint32;
  var
    buffer  : TBytes ;
    i       : Integer;
    nOffset : duint32;
  begin
    SetLength( buffer, 4 ) ;
    if ( bitPos = 0 ) then
      filestr.read( buffer, 4 )
    else begin
      for i := 0 to 4 - 1 do
        buffer[i] := getRawChar8;
    end ;
    {$IFDEF OXYGENE}
      nOffset := BitConverter.ToUInt32( {$IFDEF JAVA}array of Byte{$ENDIF}(buffer), 0 ) ;
    {$ELSE}
      Move( buffer[0], nOffset, 4 ) ;
    {$ENDIF}
    Result := nOffset;
  end;

  function DwgBuffer.getRL64 : duint64;
  var
    buffer  : TBytes ;
    i       : Integer;
    nOffset : duint64;
  begin
    SetLength( buffer, 8 ) ;
    if ( bitPos = 0 ) then
      filestr.read( buffer, 8 )
    else begin
      for i := 0 to 8 - 1 do
        buffer[i] := getRawChar8;
    end ;
    {$IFDEF OXYGENE}
      nOffset := BitConverter.ToUInt64( {$IFDEF JAVA}array of Byte{$ENDIF}(buffer), 0 ) ;
    {$ELSE}
      Move( buffer[0], nOffset, 8 ) ;
    {$ENDIF}
    Result := nOffset;
  end;


  function DwgBuffer.getUModularChar : duint64;
  var
    vNext : duint64 ;
    vShift: duint8 ;
  begin
    try
      Result := 0 ;
      vShift := 0 ;
      repeat
        vNext := getRawChar8 ;
        if vShift <= 63 then
          Result := Result or (vNext and $7F) shl vShift;
        inc(vShift, 7);
      until vNext and $80 = 0;
    except
      Result := 0;
    end;
  end ;

  function DwgBuffer.getModularChar : dint32;
  var
    vNext : Integer;
    vShift: duint8;
  begin
    Result := 0;
    vShift := 0;
    vNext  := 0;
    try
      while True do
      begin
        vNext := getRawChar8 ;
        if vNext and $80 = 0 then break;
        Result := Result or (vNext and $7F) shl vShift ;
        inc(vShift, 7) ;
      end ;
      Result := Result or (vNext and $3F) shl vShift ;
      if vNext and $40 <> 0 then
        Result := -Result ;
    except
      Result := 0 ;
    end ;
  end ;

  function DwgBuffer.getModularShort : dint32;
  var
    buffer : TArray<dint16>;
    i      : Integer;
    cnt    : Integer;
    b      : duint16;
    offset : Integer;
  begin
    Result := 0;
    SetLength( buffer, 2 ) ;
    for i := 0 to 2 - 1 do begin
      b := getRawShort16 ;
      buffer[i] := ( b and $7FFF ) ;
      cnt := i ;
      if ( ( b and $8000 ) = 0 ) then
        break;
    end ;

    offset := 0;
    for i := 0 to cnt do begin
      Result := Result + ( buffer[i] shl offset ) ;
      offset := offset + 15;
    end ;
  end ;

  function DwgBuffer.getHandle : DwgHandle;
  var
    hl   : DwgHandle;
    data : duint8;
    i    : Integer;
  begin
    data := getRawChar8;
    hl.code := ( data shr 4 ) and $0F;
    hl.size := data and $0F;
    hl.ref := 0;
    for i := 0 to hl.size - 1 do begin
      {$IFDEF JAVA}
        hl.ref := Cardinal(( Integer( hl.ref) shl 8 ) or Integer($FF and getRawChar8));
      {$ELSE}
        hl.ref := ( hl.ref shl 8 ) or getRawChar8;
      {$ENDIF}
    end ;
    Result := hl;
  end ;

  function DwgBuffer.getOffsetHandle(
    const _href : duint64
  ) : DwgHandle;
  var
    hl : DwgHandle;
  begin
    hl := getHandle;

    if ( hl.code > 5 ) then begin
      if ( hl.code = $0C ) then
        hl.ref := _href - hl.ref
      else if ( hl.code = $0A ) then
        hl.ref := _href + hl.ref
      else if ( hl.code = $08 ) then
        hl.ref := _href - 1
      else if ( hl.code = $06 ) then
        hl.ref := _href + 1;
      hl.code := 7;
    end ;
    Result := hl;
  end ;

  function DwgBuffer.get8bitStr : String;
  var
    textSize  : duint16;
    tmpBuffer : TBytes;
    good      : Boolean;
    i         : Integer ;
    count     : Integer ;
  begin
    textSize := getBitShort;
    if ( textSize = 0 ) then begin
      Result := '';
      exit;
    end ;
    SetLength( tmpBuffer, textSize ) ;
    good := getBytes( tmpBuffer, textSize ) ;
    if not good then begin
      Result := '';
      exit;
    end ;

    count := 0 ;
    for i := 0 to textSize - 1 do begin
      if tmpBuffer[i] = 0 then
        break ;
      inc( count ) ;
    end ;
    if assigned( decoder ) then
      Result := decoder.GetString( {$IFDEF JAVA}array of Byte{$ENDIF}(tmpBuffer), 0, count )
    else
      Result := TEncoding.ASCII.GetString( {$IFDEF JAVA}array of Byte{$ENDIF}(tmpBuffer), 0, count ) ;
  end ;

  function DwgBuffer.get16bitStr(
    _textSize : duint16;
    _nullTerm : Boolean = true
  ) : String;
  var
    ts        : Integer ;
    tmpBuffer : TBytes;
    good      : Boolean;
    txt_size  : Integer ;
  begin
    if ( _textSize = 0 ) then begin
      Result := '';
      exit;
    end ;
    txt_size := _textSize * 2 ;
    ts := txt_size;
    if ( _nullTerm ) then
      ts := ts + 2;
    SetLength( tmpBuffer, txt_size + 2 ) ;
    good := getBytes( tmpBuffer, ts ) ;
    if not good then begin
      Result := '';
      exit;
    end ;
    if not ( _nullTerm ) then begin
      if ( tmpBuffer[ txt_size - 2 ] = 0 ) and ( tmpBuffer[ txt_size - 1 ] = 0 )
      then
        txt_size := txt_size - 2;
    end ;

    {$IFDEF JAVA}
      Result := TEncoding.UTF16LE.GetString( array of Byte(tmpBuffer), 0, txt_size ) ;
    {$ELSE}
      Result := TEncoding.Unicode.GetString( tmpBuffer, 0, txt_size ) ;
    {$ENDIF}
  end ;

  function DwgBuffer.getCP8Text : String;
  begin
    Result := get8bitStr;
  end ;

  function DwgBuffer.getUCSStr( const _ts : duint16 ) : String;
  begin
    if ( _ts < 4 ) then begin
      Result := '';
      exit;
    end ;
    Result := get16bitStr( _ts div 2, false ) ;
  end ;

  function DwgBuffer.getUCSEed( const _ts : duint16 ) : String ;
  begin
    Result := get16bitStr( _ts, false ) ;
  end ;

  function DwgBuffer.getUCSText( const _nullTerm : Boolean ) : String;
  var
    ts : dint32;
  begin
    ts := $ffff and getBitShort;
    if ( ts = 0 ) then begin
      Result := '';
      exit;
    end ;

    Result := get16bitStr( ts, _nullTerm ) ;
  end ;

  function DwgBuffer.getVariableText(
    const _v        : DWG_Version;
    const _nullTerm : Boolean = true
  ) : String;
  begin
    if dwgVersionHigher( _v, DWG_Version.AC1018 ) then
      Result := getUCSText( _nullTerm )
    else
      Result := getCP8Text;
  end ;

  function DwgBuffer.getObjType(
    const _v : DWG_Version
  ) : duint16;
  var
    b : duint8;
  begin
    if dwgVersionHigher( _v, DWG_Version.AC1021 ) then begin
      b := get2Bits;
      if ( b = 0 ) then
        Result := getRawChar8
      else if ( b = 1 ) then begin
        Result := ( getRawChar8 + $01F0 ) ;
      end
      else
        Result := getRawShort16;
    end
    else
      Result := getBitShort;
  end ;

  function DwgBuffer.getExtrusion(
    const _b_R2000_style : Boolean
  ) : DwgCoord;
  var
    ext : DwgCoord;
  begin
    {$IFDEF GIS_NORECORDS}
      ext := DwgCoord.Create( 0, 0, 0 ) ;
    {$ENDIF}
    ext.x := 0.0;
    ext.y := 0.0;
    ext.z := 1.0;
    if ( _b_R2000_style ) then
      if ( getBit = 1 ) then begin
        Result := ext;
        exit;
      end ;

    ext.x := getBitDouble;
    ext.y := getBitDouble;
    ext.z := getBitDouble;
    Result := ext;
  end ;

  function DwgBuffer.getDefaultDouble(
    const _d : Double
  ) : Double;
  var
    b      : dint8;
    buffer : TBytes;
    tmp    : array of duint8;
    i      : Integer;
    ret    : Double;
  begin
    b := get2Bits;
    if ( b = 0 ) then
      Result := _d
    else if ( b = 1 ) then begin
      SetLength( buffer, 4 ) ;

      if ( bitPos <> 0 ) then begin
        for i := 0 to 4 - 1 do
          buffer[i] := getRawChar8;
      end
      else begin
        filestr.read( buffer, 4 ) ;
      end ;
      SetLength( tmp, 8 ) ;
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          System.Buffer.BlockCopy( BitConverter.GetBytes(_d), 0, tmp, 0, 8 ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          System.arraycopy( BitConverter.GetBytes(_d), 0, tmp, 0, 8 ) ;
        {$ENDIF}
      {$ELSE}
        Move( _d, tmp[0], 8 ) ;
      {$ENDIF}
      for i := 0 to 4 - 1 do
        tmp[i] := buffer[i];
      {$IFDEF OXYGENE}
        ret := BitConverter.ToDouble( tmp, 0 ) ;
      {$ELSE}
        Move( tmp[0], ret, 8 ) ;
      {$ENDIF}
      Result := ret;
    end
    else if ( b = 2 ) then begin
      SetLength( buffer, 6 ) ;
      if ( bitPos <> 0 ) then begin
        for i := 0 to 6 - 1 do
          buffer[i] := getRawChar8;
      end
      else begin
        filestr.read( buffer, 6 ) ;
      end ;
      SetLength( tmp, 8 ) ;
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          System.Buffer.BlockCopy( BitConverter.GetBytes(_d), 0, tmp, 0, 8 ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          System.arraycopy( BitConverter.GetBytes(_d), 0, tmp, 0, 8 ) ;
        {$ENDIF}
      {$ELSE}
        Move( _d, tmp[0], 8 ) ;
      {$ENDIF}
      for i := 2 to 6 - 1 do
        tmp[ i - 2 ] := buffer[i];
      tmp[ 4 ] := buffer[0];
      tmp[ 5 ] := buffer[ 1 ];
      {$IFDEF OXYGENE}
        ret := BitConverter.ToDouble( tmp, 0 ) ;
      {$ELSE}
        Move( tmp[0], ret, 8 ) ;
      {$ENDIF}
      Result := ret;
    end
    else
      Result := getRawDouble;
  end ;

  function DwgBuffer.getThickness(
    const _b_R2000_style : Boolean
  ) : Double;
  begin
    if ( _b_R2000_style ) then
      if ( getBit = 1 ) then begin
        Result := 0.0;
        exit;
      end ;

    Result := getBitDouble;
  end ;

  function DwgBuffer.getCmColor(
    const _v : DWG_Version
  ) : DwgColor;
  var
    idx       : duint16;
    rgb       : duint32;
    cb        : duint8;
    typ       : duint8;
    colorName : String;
    bookName  : String;
    val       : dint16;
  begin
    if dwgVersionLower( _v, DWG_Version.AC1018 ) then begin
      val := getSBitShort ;
      case val of
        256 : Result := DwgColor.Create( DwgColorType.ByLayer, 256 ) ;
        0   : Result := DwgColor.Create( DwgColorType.ByBlock, 0 )
      else    Result := DwgColor.Create( DwgColorType.ByIndex, val );
      end ;
      exit;
    end ;

    idx := getBitShort;
    rgb := duint32( getBitLong ) ;
    cb  := getRawChar8;
    typ := rgb shr 24;

    if ( cb and 1 ) <> 0 then begin
      colorName := getVariableText( _v, false ) ;
    end ;
    if ( cb and 2 ) <> 0 then begin
      bookName := getVariableText( _v, false ) ;
    end ;

    case ( typ and $FF ) of
      $C0 : Result := DwgColor.Create( DwgColorType.ByLayer, 256 ); // ByLayer
      $C1 : Result := DwgColor.Create( DwgColorType.ByBlock, 0 ) ; // ByBlock
      $C2 : Result := DwgColor.Create( DwgColorType.ByRGB, rgb ); // RGB
      $C3 : Result := DwgColor.Create( DwgColorType.ByIndex, rgb and $FF ); // ACIS
      else  Result := DwgColor.Create( DwgColorType.ByLayer, 256 ); // default
    end ;
  end ;

  function DwgBuffer.getEnColor(
    const _v : DWG_Version
  ) : DwgColor;
  var
    rgb   : duint32;
    cb    : duint32;
    idx   : duint16;
    val   : dint16;
    flag  : duint16;
  begin
    if dwgVersionLower( _v, DWG_Version.AC1018 ) then begin
      val := getSBitShort ;
      case val of
        256 : Result := DwgColor.Create( DwgColorType.ByLayer, 256 ) ;
        0   : Result := DwgColor.Create( DwgColorType.ByBlock, 0 )
      else    Result := DwgColor.Create( DwgColorType.ByIndex, val );
      end ;
      exit;
    end ;
    idx := getBitShort;

    flag := idx shr 8;
    idx  := idx and $1FF;
    rgb  := 0 ;
    cb   := 255 ;

    // has transparency
    if ( ( flag and $20 ) <> 0 ) then begin
      cb := duint32(getBitLong);
    end ;

    // has AcDbColor reference
    if ( ( flag and $40 ) = $40 ) then
      Result := DwgColor.Create( DwgColorType.ByReference, 0 )
    else begin
    // complex color (rgb).
    if ( ( flag and $80 ) = $80 ) then begin
        rgb := duint32(getBitLong);
        Result := DwgColor.Create( DwgColorType.ByRGB, rgb );
      end
      else begin
      case idx of
        256 : Result := DwgColor.Create( DwgColorType.ByLayer, 256 ) ;
        0   : Result := DwgColor.Create( DwgColorType.ByBlock, 0 )
      else    Result := DwgColor.Create( DwgColorType.ByIndex, idx );
      end ;
    end ;
    end
  end ;

  function DwgBuffer.getBERawShort16 : duint16;
  var
    buffer : TBytes;
  begin
    SetLength( buffer, 2 ) ;
    buffer[0] := getRawChar8;
    buffer[ 1 ] := getRawChar8;
    Result := ( buffer[0] shl 8 ) or ( buffer[ 1 ] and $FF ) ;
  end ;

  function DwgBuffer.getBytes(
    var _buf    : TBytes;
    const _size : Integer
  ) : Boolean;
  var
    tmp : duint8;
    i   : Integer;
  begin
    filestr.read( _buf, _size ) ;
    if not filestr.good then begin
      Result := false;
      exit;
    end ;

    if ( bitPos <> 0 ) then begin
      for i := 0 to _size - 1 do begin
        tmp := _buf[i];
        _buf[i] := duint8( ( currByte[0] shl bitPos ) or
          ( tmp shr ( 8 - bitPos ) ) ) ;
        currByte[0] := tmp;
      end ;
    end ;
    Result := true;
  end ;

// ----------------------------------------------------------------------------
// DwgByteStream
// ----------------------------------------------------------------------------

  constructor DwgByteStream.Create(
    const _buf  : TBytes;
    const _size : Integer
  ) ;
  begin
    inherited Create;
    stream := _buf;
    sz     := _size;
    pos    := 0;
    isOk   := true;
  end ;

  function DwgByteStream.clone : DwgBasicStream;
  begin
    Result := DwgByteStream.Create( stream, sz ) ;
  end ;

  function DwgByteStream.getPos : duint64;
  begin
    Result := pos;
  end ;

  function DwgByteStream.good : Boolean;
  begin
    Result := isOk;
  end ;

  function DwgByteStream.read(
    var _s   : TBytes;
    const _n : duint64
  ) : Boolean;
  var
    i : Integer;
  begin
    if ( _n > ( sz - pos ) ) then begin
      isOk := false;
      Result := false;
      exit;
    end ;

    for i := 0 to _n - 1 do begin
      _s[i] := stream[ pos ];
      inc( pos ) ;
    end ;

    Result := true;
  end ;

  function DwgByteStream.setPos(
    const _p : duint64
  ) : Boolean;
  begin
    if ( _p > Size ) then begin
      isOk := false;
      Result := false;
      exit;
    end ;

    pos := _p;
    Result := true;
  end ;

  function DwgByteStream.getSize : duint64;
  begin
    Result := sz;
  end ;

// ----------------------------------------------------------------------------
// DwgFileStream
// ----------------------------------------------------------------------------

  constructor DwgFileStream.Create(
    const _stream : TStream
  ) ;
  begin
    inherited Create;

    stream := _stream;
    sz := stream.Size;
    stream.Position := 0;
  end ;

  function DwgFileStream.clone : DwgBasicStream;
  begin
    Result := DwgFileStream.Create( stream ) ;
  end ;

  function DwgFileStream.getPos : duint64;
  begin
    Result := stream.Position;
  end ;

  function DwgFileStream.good : Boolean;
  begin
    Result := true;
  end ;

  function DwgFileStream.read(
    var _s   : TBytes;
    const _n : duint64
  ) : Boolean;
  begin
    {$IFDEF JAVA}
      Result := stream.read( array of Byte(_s), _n ) = _n;
    {$ELSE}
      Result := stream.Read( _s, _n ) = _n;
    {$ENDIF}
  end ;

  function DwgFileStream.setPos(
    const _p : duint64
  ) : Boolean;
  begin
    if ( _p >= sz ) then begin
      Result := false;
      exit;
    end ;

    stream.Position := _p;
    Result := true;
  end ;

  function DwgFileStream.getSize : duint64;
  begin
    Result := sz;
  end ;

// ----------------------------------------------------------------------------
// DwgPageInfo
// ----------------------------------------------------------------------------

  constructor DwgPageInfo.Create(
    const _i      : dint64;
    const _ad     : duint64;
    const _sz     : duint32
  ) ;
  begin
    inherited Create;

    Id      := _i;
    address := _ad;
    size    := _sz;
  end ;

// ----------------------------------------------------------------------------
// DwgSectionInfo
// ----------------------------------------------------------------------------

  constructor DwgSectionInfo.Create;
  begin
    inherited;
    compresed := 1; // 1=no, 2=yes
    encrypted := 0;
    pageCount := 0;
    Id := - 1;

    pages := TDictionary<duint32, TObject>.Create;
  end ;

  {$IFNDEF OXYGENE}
    destructor DwgSectionInfo.Destroy ;
    var
      it : TPair<duint32, TObject> ;
    begin
      for it in pages do
        FreeObjectNotNil( it.Value ) ;

      FreeObject( pages ) ;
      inherited ;
    end ;
  {$ENDIF}

// ----------------------------------------------------------------------------
// DwgObjHandle
// ----------------------------------------------------------------------------

  constructor DwgObjHandle.Create(
    const _t : duint32;
    const _h : duint64;
    const _l : duint32
  ) ;
  begin
    typ    := _t;
    handle := _h;
    loc    := _l;
  end ;

// ----------------------------------------------------------------------------
// DwgCoord
// ----------------------------------------------------------------------------

  constructor DwgCoord.Create(
    const _ix, _iy, _iz: Double
  );
  begin
    x := _ix ;
    y := _iy ;
    z := _iz ;
  end ;

  function DwgCoord.ToArray : TArray<Double>;
  begin
    SetLength( Result, 3 ) ;
    Result[0] := x ;
    Result[1] := y ;
    Result[2] := z ;
  end ;

  function DwgCoord.ToPoint : TGIS_Point3D ;
  begin
    Result := GisPoint3D( x, y, z ) ;
  end ;

// ----------------------------------------------------------------------------
// DwgReader
// ----------------------------------------------------------------------------

  constructor DwgReader.Create(
    const _stream  : TStream;
    const _version : DWG_Version
  ) ;
  begin
    inherited Create;

    fileBuf := DwgBuffer.Create( _stream ) ;
    version := _version;
    {$IFDEF JAVA}
      decoder := TEncoding.ASCII;
    {$ELSE}
      decoder := TEncoding.ASCII;
    {$ENDIF}
    ownDecoder  := False ;
    nextEntLink := 0;
    prevEntLink := 0;
    maintenanceVersion := 0;

    mapSections         := TDictionary<DWG_Section, DwgSectionInfo>.Create;
    mapClasses          := TDictionary<duint32, TObject>.Create;
    mapHandles          := TDictionary<duint64, DwgObjHandle>.Create;
    mapUnusedHandles    := TDictionary<duint64, DwgObjHandle>.Create;
    mapRemainingHandles := TDictionary<duint64, DwgObjHandle>.Create;
    mapLType            := TDictionary<duint64, DwgTableEntry>.Create;
    mapLayers           := TDictionary<duint64, DwgTableEntry>.Create;
    mapStyles           := TDictionary<duint64, DwgTableEntry>.Create;
    mapDimStyles        := TDictionary<duint64, DwgTableEntry>.Create;
    mapVPorts           := TDictionary<duint64, DwgTableEntry>.Create;
    mapBlockRecords     := TDictionary<duint64, DwgTableEntry>.Create;
    mapAppIds           := TDictionary<duint64, DwgTableEntry>.Create;
  end ;

  {$IFDEF DCC}
  destructor DwgReader.Destroy;
  var
    its   : TPair<DWG_Section, DwgSectionInfo> ;
    itc   : TPair<duint32, TObject> ;
    ito   : TPair<duint32, DwgObjHandle>;
    ituo  : TPair<duint32, DwgObjHandle>;
    itr   : TPair<duint32, DwgObjHandle>;
    itlt  : TPair<duint64, DwgTableEntry>;
    itla  : TPair<duint64, DwgTableEntry>;
    itst  : TPair<duint64, DwgTableEntry>;
    itds  : TPair<duint64, DwgTableEntry>;
    itv   : TPair<duint64, DwgTableEntry>;
    itbr  : TPair<duint64, DwgTableEntry>;
    ita   : TPair<duint64, DwgTableEntry>;
  begin
    FreeObject( fileBuf ) ;
    if ownDecoder then
      FreeObject( decoder ) ;

    for its in mapSections do
      FreeObjectNotNil( its.Value ) ;
    for itc in mapClasses do
      FreeObjectNotNil( itc.Value ) ;

    for itlt in mapLType do
      FreeObjectNotNil( itlt.Value ) ;
    for itla in mapLayers do
      FreeObjectNotNil( itla.Value ) ;
    for itst in mapStyles do
      FreeObjectNotNil( itst.Value ) ;
    for itds in mapDimStyles do
      FreeObjectNotNil( itds.Value ) ;
    for itv in mapVPorts do
      FreeObjectNotNil( itv.Value ) ;
    for itbr in mapBlockRecords do
      FreeObjectNotNil( itbr.Value ) ;
    for ita in mapAppIds do
      FreeObjectNotNil( ita.Value ) ;

    FreeObject( mapSections         ) ;
    FreeObject( mapClasses          ) ;
    FreeObject( mapHandles          ) ;
    FreeObject( mapUnusedHandles    ) ;
    FreeObject( mapRemainingHandles ) ;
    FreeObject( mapLType            ) ;
    FreeObject( mapLayers           ) ;
    FreeObject( mapStyles           ) ;
    FreeObject( mapDimStyles        ) ;
    FreeObject( mapVPorts           ) ;
    FreeObject( mapBlockRecords     ) ;
    FreeObject( mapAppIds           ) ;
    FreeObject( iRdrClbk            ) ;

    inherited;
  end ;
  {$ENDIF}


  function DwgReader.findTable(
    const _table  : DWG_TTYPE ;
    const _handle : duint64
  ) : DwgTableEntry ;
  var
    ts : DwgTableEntry ;
    ds : DwgTableEntry ;
    br : DwgTableEntry ;
    ll : DwgTableEntry ;
    lt : DwgTableEntry ;
    vp : DwgTableEntry ;
    ap : DwgTableEntry ;
  begin
    Result := nil ;
    case _table of
      DWG_TTYPE.STYLE:
        if mapStyles.TryGetValue( _handle, ts ) then
          Result := ts ;
      DWG_TTYPE.DIMSTYLE:
        if mapDimStyles.TryGetValue( _handle, ds ) then
          Result := ds ;
      DWG_TTYPE.BLOCK_RECORD:
        if mapBlockRecords.TryGetValue( _handle, br ) then
          Result := br ;
      DWG_TTYPE.LAYER:
        if mapLayers.TryGetValue( _handle, ll ) then
          Result := ll ;
      DWG_TTYPE.LTYPE:
        if mapLType.TryGetValue( _handle, lt ) then
          Result := lt ;
      DWG_TTYPE.VPORT:
        if mapVPorts.TryGetValue( _handle, vp ) then
          Result := vp ;
      DWG_TTYPE.APPID:
        if mapAppIds.TryGetValue( _handle, ap ) then
          Result := ap ;
    end ;
  end ;

  function DwgReader.findTableName(
    const _table  : DWG_TTYPE ;
    const _handle : duint64
  ) : String ;
  var
    tab : DwgTableEntry;
  begin
    tab := findTable( _table, _handle ) ;
    if assigned( tab ) then
      Result := tab.name
    else
      Result := '' ;
  end ;

  procedure DwgReader.parseAttribs(
    const _e : DwgEntity
  ) ;
  var
    ltref : duint64;
    lyref : duint64;
    lt_it : DwgTableEntry;
    ly_it : DwgTableEntry;
  begin
    if not assigned( _e ) then
      exit;

    ltref := _e.lTypeH.ref;
    lyref := _e.layerH.ref;

    if mapLType.TryGetValue( ltref, lt_it ) then
      _e.lineType := lt_it.name;

    if mapLayers.TryGetValue( lyref, ly_it ) then
      _e.layer := ly_it.name;
  end ;

  function DwgReader.readDwgBlocks : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.readDwgBlocksEx(
    const _dbuf : DwgBuffer
  ) : Boolean;
  var
    bs          : duint32;
    {$IFDEF DCC}
    it          : TPair< duint64, DwgTableEntry >;
    hit         : duint64;
    {$ENDIF}
    bkr         : DwgBlockRecord;
    oc          : {$IFDEF JAVA} nullable {$ENDIF} DwgObjHandle;
    {$IFDEF JAVA}
    oc2         : DwgObjHandle := new DwgObjHandle(0,0,0);
    {$ENDIF}
    size        : dint32;
    ret, ret2   : Boolean;
    tmpByteStr  : TBytes;
    buff, buff1 : DwgBuffer;
    bk          : DwgBlock;
    nextH       : duint64;
    _end        : DwgBlock;
  begin
    bs := 0;

    bk := DwgBlock.Create;
    try
      for it in mapBlockRecords do begin
        bkr := it.Value as DwgBlockRecord;

        if not mapHandles.TryGetValue( bkr.blockEnt, oc ) then begin
          dwg_log( '\nWARNING: block entity not found\n' ) ;
          ret := false;
          continue;
        end ;
        mapHandles.Remove( bkr.blockEnt ) ;
        if ( not ( _dbuf.setPosition( oc.loc ) ) ) then begin
          dwg_log( 'WARNING: bad location reading blocks\n' ) ;
          ret := false;
          continue;
        end ;
        size := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;

        SetLength( tmpByteStr, size ) ;
        _dbuf.getBytes( tmpByteStr, size ) ;

        buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
        try
          ret := bk.parseDwg( version, buff, bs ) ;
        finally
          FreeObject( buff ) ;
        end ;
        parseAttribs( bk ) ;
        bk.basePoint := bkr.basePoint;
        bk.flag := bkr.flag;
        iRdrClbk.addBlock(bk);
        bkr.name := bk.name;

        if ( bk.parentHandle = 0 ) then begin
          bk.parentHandle := bkr.handle;
        end ;
       // else begin
          if dwgVersionLower( version, DWG_Version.AC1018 ) then begin
            nextH := bkr.firstEH;
            while ( nextH <> 0 ) do begin
              if not mapHandles.TryGetValue( nextH, oc ) then begin
                dwg_log( '\nWARNING: Entity of block not found\n' ) ;
                ret := false;
                nextEntLink := nextH + 1 ;
              end
              else begin
                mapHandles.Remove( nextH ) ;
                {$IFDEF JAVA}
                  oc2 := oc ;
                  ret2 := readDwgEntity( _dbuf, oc2 ) ;
                {$ELSE}
                  ret2 := readDwgEntity( _dbuf, oc ) ;
                {$ENDIF}
                ret := ret and ret2;
              end ;
              if ( nextH = bkr.lastEH ) then
                nextH := 0
              else
                nextH := nextEntLink;
            end ;
          end
          else begin
            for hit in bkr.entMap do begin
              nextH := hit;
              if not mapHandles.TryGetValue( nextH, oc ) then begin
                dwg_log( '\nWARNING: Entity of block not found\n' ) ;
                ret := false;
                continue;
              end
              else begin
                mapHandles.Remove( nextH ) ;
                {$IFDEF JAVA}
                  oc2 := oc;
                  ret2 := readDwgEntity( _dbuf, oc2 ) ;
                {$ELSE}
                  ret2 := readDwgEntity( _dbuf, oc ) ;
                {$ENDIF}
                ret := ret and ret2;
              end ;
            end ;
          end ;
       // end ;

        if not mapHandles.TryGetValue( bkr.endBlock, oc ) then begin
          dwg_log( '\nWARNING: End block entity not found\n' ) ;
          ret := false;
          continue;
        end ;
        mapHandles.Remove( bkr.endBlock ) ;
        _dbuf.setPosition( oc.loc ) ;
        size := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, size ) ;
        _dbuf.getBytes( tmpByteStr, size ) ;
        _end := DwgBlock.Create;
        try
          _end.isEnd := true;
          buff1 := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
          try
            ret2 := _end.parseDwg( version, buff1, bs ) ;
            ret := ret and ret2;
          finally
            FreeObject( buff1 ) ;
          end ;
          if ( bk.parentHandle = 0 ) then
            bk.parentHandle := bkr.handle;
          parseAttribs( _end ) ;
        finally
          FreeObject( _end ) ;
        end ;
        iRdrClbk.endBlock ;
      end ;
    finally
      FreeObject( bk ) ;
    end ;

    Result := true;
  end ;

  function DwgReader.readDwgClasses : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.readDwgEntities : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.readDwgEntitiesEx(
    const _dbuf : DwgBuffer
  ) : Boolean;
  var
    {$IFDEF DCC}
    it  : TPair<duint64, DwgObjHandle>;
    k   : duint64;
    {$ENDIF}
    lst : TList<duint64>;
    res : Boolean;
    oh  : DwgObjHandle ;
  begin
    res := false;

    lst := TList<duint64>.Create;
    try
      for it in mapHandles do begin
        oh := it.Value ;
        try
          res := readDwgEntity( _dbuf, oh ) ;
        except
          // ignore invalid ones
        end;
        lst.Add( it.Key ) ;
      end ;
      for k in lst do
        mapHandles.Remove( k ) ;
    finally
      FreeObject( lst );
    end ;
    Result := res;
  end ;

  function DwgReader.readDwgEntity(
    const dbuf : DwgBuffer;
      var objH : DwgObjHandle
  ) : Boolean;
  var
    bs         : duint32;
    ret        : Boolean;
    buff       : DwgBuffer;
    size       : dint32;
    tmpByteStr : TBytes;
    oType      : duint16;
    cl         : TObject;
    e          : DwgEntity;
    o          : DwgTableEntry;

      procedure entry_parse( const _e : DwgEntity ) ;
      begin
        ret := _e.parseDwg( version, buff, bs ) ;
        parseAttribs( _e ) ;
        nextEntLink := _e.nextEntLink;
        prevEntLink := _e.prevEntLink;
      end ;

  begin
    bs := 0;
    e := nil ;
    o := nil ;

    nextEntLink := 0;
    prevEntLink := 0;
    dbuf.setPosition( objH.loc ) ;

    if not dbuf.isGood then begin
      dwg_log( ' WARNING: readDwgEntity, bad location\n' ) ;
      Result := false;
      exit;
    end ;
    size := dbuf.getModularShort;
    if dwgVersionHigher( version, DWG_Version.AC1021 ) then begin
      bs := dbuf.getUModularChar;
    end ;
    if size = 0 then begin
      dwg_log( ' WARNING: readDwgEntity, bad size\n' ) ;
      Result := false;
      exit;
    end ;

    SetLength( tmpByteStr, size ) ;
    dbuf.getBytes( tmpByteStr, size ) ;

    if not dbuf.isGood then begin
      dwg_log( ' WARNING: readDwgEntity, bad size\n' ) ;
      Result := false;
      exit;
    end ;

    buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
    try
      oType := buff.getObjType( version ) ;
      buff.resetPosition;

      cl := nil ;
      if ( oType > 499 ) then begin
        if not mapClasses.TryGetValue( oType, cl ) then begin
          dwg_log( ' WARNING: Class not found\n' ) ;
          Result := false;
          exit;
        end
        else begin
          if ( DwgClass(cl).DwgType <> 0 ) then
            oType := DwgClass(cl).DwgType;
        end
      end ;

      ret := False ;
      objH.typ := oType;
      case ( oType ) of
        DWG_OBJ_ID_LINE :
          begin
            e := DwgLine.Create;
            try
              entry_parse( e ) ;
              iRdrClbk.addLine(DwgLine(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_POINT :
          begin
            e := DwgPoint.Create;
            try
              entry_parse( e ) ;
              iRdrClbk.addPoint(DwgPoint(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_ARC :
          begin
            e := DwgArc.Create ;
            try
              entry_parse(e) ;
              iRdrClbk.addArc(DwgArc(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_CIRCLE :
          begin
            e := DwgCircle.Create;
            try
              entry_parse(e) ;
              iRdrClbk.addCircle(DwgCircle(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_ELLIPSE :
          begin
            e := DwgEllipse.Create ;
            try
              entry_parse(e) ;
              iRdrClbk.addEllipse(DwgEllipse(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_INSERT,
        DWG_OBJ_ID_MINSERT :
          begin
            e := DwgInsert.Create ;
            try
              entry_parse(e) ;
              DwgInsert(e).name := findTableName( DWG_TTYPE.BLOCK_RECORD,
                                                  DwgInsert(e).blockRecH.ref
                                                 ) ;
              iRdrClbk.addInsert(DwgInsert(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_LWPOLYLINE :
          begin
            e := DwgLWPolyline.Create;
            try
              entry_parse(e) ;
              iRdrClbk.addLWPolyline(DwgLWPolyline(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_TEXT :
          begin
            e := DwgText.Create ;
            try
              entry_parse(e) ;
              DwgText(e).style := findTableName(DWG_TTYPE.STYLE, DwgText(e).styleH.ref) ;
              iRdrClbk.addText(DwgText(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_ATTRIB :
          begin
            e := DwgAttrib.Create ;
            try
              entry_parse(e) ;
              DwgAttrib(e).style := findTableName(DWG_TTYPE.STYLE, DwgAttrib(e).styleH.ref) ;
              iRdrClbk.addAttrib(DwgAttrib(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_ATTDEF :
          begin
            e := DwgAttDef.Create ;
            try
             entry_parse(e) ;
             DwgAttDef(e).style := findTableName(DWG_TTYPE.STYLE, DwgAttDef(e).styleH.ref) ;
             iRdrClbk.addAttDef(DwgAttDef(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_MTEXT :
          begin
            e := DwgMText.Create ;
            try
              entry_parse(e) ;
              DwgMText(e).style := findTableName(DWG_TTYPE.STYLE, DwgMText(e).styleH.ref) ;
              iRdrClbk.addMText(DwgMText(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_FACE3D :
          begin
            e := Dwg3Dface.Create ;
            try
              entry_parse(e) ;
              iRdrClbk.add3dFace(Dwg3Dface(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_DIMENSION_ORDINATE :
          begin
            e := DwgDimensionOrdinate.Create ;
            try
              entry_parse(e) ;
              DwgDimensionOrdinate(e).style := findTableName(DWG_TTYPE.DIMSTYLE,
                                                DwgDimensionOrdinate(e).dimStyleH.ref
                                              ) ;
              iRdrClbk.addDimOrdinate(DwgDimensionOrdinate(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_DIMENSION_LINEAR :
          begin
            e := DwgDimensionLinear.Create ;
            try
              entry_parse(e) ;
              DwgDimensionLinear(e).style := findTableName(DWG_TTYPE.DIMSTYLE,
                                                DwgDimensionLinear(e).dimStyleH.ref
                                              ) ;
              iRdrClbk.addDimLinear(DwgDimensionLinear(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_DIMENSION_ALIGNED :
          begin
            e := DwgDimensionAligned.Create ;
            try
              entry_parse(e) ;
              DwgDimensionAligned(e).style := findTableName(DWG_TTYPE.DIMSTYLE,
                                                DwgDimensionAligned(e).dimStyleH.ref
                                              ) ;
              iRdrClbk.addDimAligned(DwgDimensionAligned(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        // 23: {
        // DwgDimAngular3p e;
        // entry_parse(e)
        // e.style = findTableName(DRW::dimstyle, e.dimStyleH.ref) ;
        // intfa.addDimAngular3P(&e) ;
        // break; }
        // 24: {
        // DwgDimAngular e;
        // entry_parse(e)
        // e.style = findTableName(DRW::dimstyle, e.dimStyleH.ref) ;
        // intfa.addDimAngular(&e) ;
        // break; }
        // 25: {
        // DwgDimRadial e;
        // entry_parse(e)
        // e.style = findTableName(DRW::dimstyle, e.dimStyleH.ref) ;
        // intfa.addDimRadial(&e) ;
        // break; }
        DWG_OBJ_ID_DIMENSION_DIAMETER :
          begin
            e := DwgDimensionDiametric.Create ;
            try
              entry_parse(e) ;
              DwgDimensionDiametric(e).style := findTableName(DWG_TTYPE.DIMSTYLE,
                                                  DwgDimensionDiametric(e).dimStyleH.ref
                                                ) ;
              iRdrClbk.addDimDiameter(DwgDimensionDiametric(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_LEADER :
          begin
            e := DwgLeader.Create ;
            try
              entry_parse(e) ;
              DwgLeader(e).style := findTableName( DWG_TTYPE.DIMSTYLE,
                                                   DwgLeader(e).dimStyleH.ref) ;
              iRdrClbk.addLeader(DwgLeader(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_SOLID :
          begin
            e := DwgSolid.Create ;
            try
              entry_parse(e) ;
              iRdrClbk.addSolid(DwgSolid(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_HATCH :
          begin
            e := DwgHatch.Create ;
            try
              entry_parse(e) ;
              iRdrClbk.addHatch(DwgHatch(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_TRACE :
          begin
            e := DwgTrace.Create ;
            try
              entry_parse(e) ;
              iRdrClbk.addTrace(DwgTrace(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        // 34: {
        // DwgViewport e;
        // entry_parse(e)
        // intfa.addViewport(e) ;
        // break; }
        DWG_OBJ_ID_SPLINE :
          begin
            e := DwgSpline.Create ;
            try
              entry_parse(e) ;
              iRdrClbk.addSpline(DwgSpline(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_RAY :
          begin
            e := DwgRay.Create ;
            try
              entry_parse(e) ;
              iRdrClbk.addRay(DwgRay(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_POLYLINE_2D,
        DWG_OBJ_ID_POLYLINE_3D,
        DWG_OBJ_ID_POLYLINE_PFACE,
        DWG_OBJ_ID_POLYLINE_MESH :
          begin
            e := DwgPolyline.Create ;
            try
              entry_parse(e) ;
              readPlineVertex(DwgPolyline(e), dbuf) ;
              iRdrClbk.addPolyline(DwgPolyline(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_XLINE :
          begin
            e := DwgXline.Create ;
            try
              entry_parse(e) ;
              iRdrClbk.addXline(DwgXline(e)) ;
            finally
              FreeObject( e ) ;
            end ;
          end ;
        DWG_OBJ_ID_MLINE :
          begin
            //TODO
          end ;
        // 101: {
        // DwgImage e;
        // entry_parse(e)
        // intfa.addImage(&e) ;
        // break; }
        //
        DWG_OBJ_ID_LAYOUT :  begin
          o := DwgLayout.Create ;
          try
            ret := o.parseDwg( version, buff, bs ) ;
            iRdrClbk.addLayout(DwgLayout(o)) ;
          finally
            FreeObject( o ) ;
          end ;
        end ;
        DWG_OBJ_ID_DBCOLOR :  begin
          o := DwgDbColor.Create ;
          try
            ret := o.parseDwg( version, buff, bs ) ;
            iRdrClbk.addColor( DwgDbColor(o)) ;
          finally
            FreeObject( o ) ;
          end ;
        end ;
        DWG_OBJ_ID_VERTEX_2D : begin  // read by polyline
          ret := True ;
        end ;
        DWG_OBJ_ID_VERTEX_3D : begin  // read by polyline
          ret := True ;
        end ;
        DWG_OBJ_ID_VERTEX_MESH : begin // read by polyline
          ret := True ;
        end ;
        DWG_OBJ_ID_VERTEX_PFACE : begin  // read by polyline
          ret := True ;
        end ;
        DWG_OBJ_ID_VERTEX_PFACE_FACE : begin // read by polyline
          ret := True ;
        end ;
        DWG_OBJ_ID_XRECORD : begin  // skip
          ret := True ;
        end ;
        DWG_OBJ_ID_DICTIONARY : begin // skip
          ret := True ;
        end ;
        DWG_OBJ_ID_MLINESTYLE :  begin  // skip
          ret := True ;
        end ;
        DWG_OBJ_ID_PLACEHOLDER :  begin // skip
          ret := True ;
        end ;
        DWG_OBJ_ID_SEQEND :  begin  // skip
          ret := True ;
        end ;
        else  // not supported or to add to remaining map
          mapUnusedHandles.Add( objH.handle, objH ) ;
      end ;
    finally
      FreeObject( buff ) ;
    end ;

    if not ret then begin
      dwg_log( '\nWARNING: Entity type ' ) ;
      if ( oType > 499 ) then
        dwg_log( ': ' +DwgClass(cl).className )
      else
        dwg_log( ': ' + ObjectTypeToStr( oType ) ) ;
      dwg_log( ' is not supported' ) ;
    end ;
    Result := ret;

  end ;

  function DwgReader.readDwgHandles : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.checkSentinel(
    const _buf   : DwgBuffer;
    const _enum  : DWG_Section;
    const _start : Boolean
  ) : Boolean;
  var
    i : Integer;
  begin
    for i := 0 to 16 - 1 do begin
      _buf.getRawChar8 ;
    end ;
    Result := true;
  end ;

  function DwgReader.readDwgHandlesEx(
    const _dbuf          : DwgBuffer;
    const _offset, _size : duint32
  ) : Boolean;
  var
    maxPos     : duint32;
    startPos   : duint32;
    tmpByteStr : TBytes;
    size       : duint16;
    buff       : DwgBuffer;
    lastHandle : duint64;
    newHandle  : duint64;
    lastLoc    : dint64;
    newLoc     : dint64;
    crcRead    : duint16;
  begin
    if not _dbuf.setPosition( _offset ) then begin
      Result := false;
      exit;
    end ;

    maxPos   := _offset + _size;
    startPos := _offset;

    while ( maxPos > _dbuf.getPosition ) do begin
      size := _dbuf.getBERawShort16;
      _dbuf.setPosition( startPos ) ;
      SetLength( tmpByteStr, size ) ;
      _dbuf.getBytes( tmpByteStr, size ) ;

      buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
      try
        if ( size <> 2 )  then begin
          buff.setPosition( 2 ) ;
          lastHandle := 0;
          lastLoc := 0;
          while ( buff.getPosition < size ) do begin
            newHandle  := buff.getUModularChar ;
            try
              lastHandle := lastHandle + newHandle ;
            except

            end;
            newLoc     := buff.getModularChar ;
            lastLoc    := lastLoc + newLoc ;
            if lastLoc >= 0 then
              mapHandles.AddOrSetValue( lastHandle, DwgObjHandle.Create( 0, lastHandle, lastLoc ) )
            else
              lastLoc := 0 ;
          end ;
        end ;
      finally
        FreeObject( buff ) ;
      end ;
      crcRead := _dbuf.getBERawShort16;
      startPos := _dbuf.getPosition;
    end ;

    Result := _dbuf.isGood;
  end ;

  function DwgReader.readDwgHeader(
    const _hdr : DwgHeader
  ) : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.readDwgHeaderEx(
    const _hdr        : DwgHeader;
    const _buf, _hBuf : DwgBuffer
  ) : Boolean;
  begin
    Result := _hdr.parseDwg( version, _buf, _hBuf, maintenanceVersion ) ;
  end ;

  function DwgReader.readDwgObject(
    const dbuf : DwgBuffer;
    const objH : DwgObjHandle
  ) : Boolean;
  var
    bs         : duint32 ;
    size       : dint32 ;
    tmpByteStr : TBytes ;
    buff       : DwgBuffer ;
    oType      : dint16 ;
  begin
    bs := 0;

    dbuf.setPosition(objH.loc);
    if (not dbuf.isGood) then begin
      dwg_log('\nWARNING: readDwgObject, bad location\n');
      Result := false;
      exit ;
    end ;
    size := dbuf.getModularShort;
    if dwgVersionHigher(version, DWG_Version.AC1021) then
      bs := dbuf.getUModularChar;

    SetLength( tmpByteStr, size ) ;
    dbuf.getBytes(tmpByteStr, size);
    if (not dbuf.isGood) then begin
      dwg_log('\nWARNING: readDwgObject, bad size\n');
      Result := false;
      exit ;
    end ;
    buff := DwgBuffer.Create(tmpByteStr, size, decoder);
    try
      oType := objH.typ;
      case oType of
        102: begin
              //DwgImageDef e;
              //ret = e.parseDwg(version, &buff, bs);
              //intfa.linkImage(&e);
              //break;
             end
      else begin
            mapRemainingHandles.Add(objH.handle, objH) ;
           end ;
      end ;
    finally
      FreeObject( buff ) ;
    end ;
    Result := True ;
  end ;

  function DwgReader.readDwgObjects : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.readDwgObjectsEx(
    const _dbuf : DwgBuffer
  ) : Boolean;
  var
    {$IFDEF DCC}
    it  : TPair<duint64, DwgObjHandle>;
    k   : duint64;
    {$ENDIF}
    lst : TList<duint64>;
    res : Boolean;
  begin
    res := false;

    lst := TList<duint64>.Create;
    try
      for it in mapUnusedHandles do begin
        res := readDwgObject( _dbuf, it.Value ) ;
        lst.Add( it.Key ) ;
      end ;
      for k in lst do
        mapUnusedHandles.Remove( k ) ;
    finally
      FreeObject( lst );
    end ;
    Result := res;
  end ;

  function DwgReader.readDwgTables(
    const _hdr : DwgHeader
  ) : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.readDwgTablesEx(
    const _hdr  : DwgHeader;
    const _dbuf : DwgBuffer
  ) : Boolean;
  var
    ret                : Boolean;
    ret2               : Boolean;
    oc                 : {$IFDEF JAVA} nullable {$ENDIF} DwgObjHandle;
    oType              : dint16;
    bs                 : duint32;
    tmpByteStr         : TBytes;
    ltControl          : DwgObjControl;
    layControl         : DwgObjControl;
    cSize              : dint32;
    cbuff, lbuff, buff : DwgBuffer;
    lt                 : DwgTableEntry;
    lsize, size        : dint32;
    la, ly             : DwgLayer;
    styControl         : DwgObjControl;
    {$IFDEF DCC}
    it                 : duint64;
    mit                : TPair< duint64, DwgTableEntry >;
    it_lt              : TPair< duint64, DwgTableEntry >;
    it_la              : TPair< duint64, DwgTableEntry >;
    it_sty             : TPair< duint64, DwgTableEntry >;
    it_dsty            : TPair< duint64, DwgTableEntry >;
    it_vp              : TPair< duint64, DwgTableEntry >;
    it_ai              : TPair< duint64, DwgTableEntry >;
    {$ENDIF}
    ref                : duint64;
    sty                : DwgTextstyle;
    dimstyControl      : DwgObjControl;
    dsty               : DwgDimstyle;
    vportControl       : DwgObjControl;
    vp                 : DwgVport;
    blockControl       : DwgObjControl;
    br                 : DwgBlockRecord;
    appIdControl       : DwgObjControl;
    ai                 : DwgAppId;
    viewControl        : DwgObjControl;
    ucsControl         : DwgObjControl;
    vpEntHeader        : DwgObjControl;
  begin
    ret := true;
    ret2 := true;
    Result := true;
    bs := 0;
    // parse linetypes, start with linetype control
    if not mapHandles.TryGetValue( _hdr.linetypeCtrl, oc ) then begin
      dwg_log( '\nWARNING: LineType control not found\n' ) ;
      ret := false;
    end
    else begin
      ltControl := DwgObjControl.Create;
      try
        mapHandles.Remove( _hdr.linetypeCtrl ) ;
        _dbuf.setPosition( oc.loc ) ;
        cSize := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, cSize ) ;
        _dbuf.getBytes( tmpByteStr, cSize ) ;
        cbuff := DwgBuffer.Create( tmpByteStr, cSize, decoder ) ;
        try
          oType := cbuff.getObjType( version ) ;
          if ( oType <> $38 ) then begin
            ret := false;
          end
          else begin
            cbuff.resetPosition;
            ret2 := ltControl.parseDwg( version, cbuff, bs ) ;
            if ( ret ) then
              ret := ret2;
          end ;
        finally
          FreeObject( cbuff ) ;
        end ;

        for it in ltControl.handlesList do begin
          if not mapHandles.TryGetValue( it, oc ) then begin
            dwg_log( '\nWARNING: LineType not found\n' ) ;
            ret := false;
          end
          else begin
            mapHandles.Remove( it ) ;

            lt := DwgLType.Create;
            _dbuf.setPosition( oc.loc ) ;
            lsize := _dbuf.getModularShort;
            if dwgVersionHigher( version, DWG_Version.AC1021 ) then
              bs := _dbuf.getUModularChar
            else
              bs := 0;
            SetLength( tmpByteStr, lsize ) ;
            _dbuf.getBytes( tmpByteStr, lsize ) ;
            lbuff := DwgBuffer.Create( tmpByteStr, lsize, decoder ) ;
            try
              ret2 := lt.parseDwg( version, lbuff, bs ) ;
            finally
              FreeObject( lbuff ) ;
            end ;
            mapLType.Add( lt.handle, lt ) ;
            if ( ret ) then
              ret := ret2;
          end
        end
      finally
        FreeObject( ltControl ) ;
      end ;
    end ;

    // parse layers, start with layer control
    if not mapHandles.TryGetValue( _hdr.layerCtrl, oc ) then begin
      dwg_log( '\nWARNING: Layer control not found\n' ) ;
      ret := false;
    end
    else begin
      layControl := DwgObjControl.Create;
      try
        mapHandles.Remove( _hdr.layerCtrl ) ;
        _dbuf.setPosition( oc.loc ) ;
        size := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, size ) ;
        _dbuf.getBytes( tmpByteStr, size ) ;
        buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
        try
          // verify if object are correct
          oType := buff.getObjType( version ) ;
          if ( oType <> $32 ) then begin
            ret := false;
          end
          else begin // reset position
            buff.resetPosition;
            ret2 := layControl.parseDwg( version, buff, bs ) ;
            if ( ret ) then
              ret := ret2;
          end ;
        finally
          FreeObject( buff ) ;
        end ;

        for it in layControl.handlesList do begin
          if not mapHandles.TryGetValue( it, oc ) then begin
            dwg_log( '\nWARNING: Layer not found\n' ) ;
            ret := false;
          end
          else begin
            mapHandles.Remove( it ) ;
            la := DwgLayer.Create;
            _dbuf.setPosition( oc.loc ) ;
            size := _dbuf.getModularShort;
            if dwgVersionHigher( version, DWG_Version.AC1021 ) then
              bs := _dbuf.getUModularChar
            else
              bs := 0;
            SetLength( tmpByteStr, size ) ;
            _dbuf.getBytes( tmpByteStr, size ) ;
            buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
            try
              ret2 := la.parseDwg( version, buff, bs ) ;
              mapLayers.Add( la.handle, la ) ;
              if ( ret ) then
                ret := ret2;
            finally
              FreeObject( buff ) ;
            end ;
          end
        end
      finally
        FreeObject( layControl ) ;
      end ;
    end ;

    // set linetype in layer
    for mit in mapLayers do begin
      ly := mit.Value as DwgLayer;
      ref := ly.lTypeH.ref;
      if mapLType.TryGetValue( ref, lt ) then
        ly.lineType := lt.name;
    end ;

    // parse text styles, start with style control
    if not mapHandles.TryGetValue( _hdr.styleCtrl, oc ) then begin
      dwg_log( '\nWARNING: Style control not found\n' ) ;
      ret := false;
    end
    else begin
      styControl := DwgObjControl.Create;
      try
        mapHandles.Remove( _hdr.styleCtrl ) ;
        _dbuf.setPosition( oc.loc ) ;
        size := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, size ) ;
        _dbuf.getBytes( tmpByteStr, size ) ;
        buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
        try
          oType := buff.getObjType( version ) ;
          if ( oType <> $34 ) then begin
            ret := false;
          end
          else begin
            buff.resetPosition;
            ret2 := styControl.parseDwg( version, buff, bs ) ;
            if ( ret ) then
              ret := ret2;
          end ;
        finally
          FreeObject( buff ) ;
        end ;

        for it in styControl.handlesList do begin
          if not mapHandles.TryGetValue( it, oc ) then begin
            dwg_log( '\nWARNING: Style not found\n' ) ;
            ret := false;
          end
          else begin
            mapHandles.Remove( it ) ;
            sty := DwgTextstyle.Create;
            _dbuf.setPosition( oc.loc ) ;
            size := _dbuf.getModularShort;
            if dwgVersionHigher( version, DWG_Version.AC1021 ) then
              bs := _dbuf.getUModularChar
            else
              bs := 0;
            SetLength( tmpByteStr, size ) ;
            _dbuf.getBytes( tmpByteStr, size ) ;
            buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
            try
              ret2 := sty.parseDwg( version, buff, bs ) ;
              mapStyles.Add( sty.handle, sty ) ;
              if ( ret ) then
                ret := ret2;
            finally
              FreeObject( buff ) ;
            end ;
          end
        end
      finally
        FreeObject( styControl ) ;
      end ;
    end ;

    // parse dim styles, start with dimstyle control
    if not mapHandles.TryGetValue( _hdr.dimstyleCtrl, oc ) then begin
      dwg_log( '\nWARNING: Dimension Style control not found\n' ) ;
      ret := false;
    end
    else begin
      mapHandles.Remove( _hdr.dimstyleCtrl ) ;
      dimstyControl := DwgObjControl.Create;
      try
        _dbuf.setPosition( oc.loc ) ;
        size := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, size ) ;
        _dbuf.getBytes( tmpByteStr, size ) ;
        buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
        try
          // verify if object are correct
          oType := buff.getObjType( version ) ;
          if ( oType <> $44 ) then begin
            ret := false;
          end
          else begin // reset position
            buff.resetPosition;
            ret2 := dimstyControl.parseDwg( version, buff, bs ) ;
            if ( ret ) then
              ret := ret2;
          end ;
        finally
          FreeObject( buff ) ;
        end ;

        for it in dimstyControl.handlesList do begin
          if not mapHandles.TryGetValue( it, oc ) then begin
            dwg_log( '\nWARNING: Dimension Style not found\n' ) ;
            ret := false;
          end
          else begin
            mapHandles.Remove( it ) ;
            dsty := DwgDimstyle.Create;
            _dbuf.setPosition( oc.loc ) ;
            size := _dbuf.getModularShort;
            if dwgVersionHigher( version, DWG_Version.AC1021 ) then
              bs := _dbuf.getUModularChar
            else
              bs := 0;
            SetLength( tmpByteStr, size ) ;
            _dbuf.getBytes( tmpByteStr, size ) ;
            buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
            try
              ret2 := dsty.parseDwg( version, buff, bs ) ;
              mapDimStyles.Add( dsty.handle, dsty ) ;
              if ( ret ) then
                ret := ret2;
            finally
              FreeObject( buff ) ;
            end ;
          end
        end
      finally
        FreeObject( dimstyControl ) ;
      end ;
    end ;

    // parse vports, start with vports control
    if not mapHandles.TryGetValue( _hdr.vportCtrl, oc ) then begin
      dwg_log( '\nWARNING: VPorts control not found\n' ) ;
      ret := false;
    end
    else begin
      mapHandles.Remove( _hdr.vportCtrl ) ;
      vportControl := DwgObjControl.Create;
      try
        _dbuf.setPosition( oc.loc ) ;
        size := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, size ) ;
        _dbuf.getBytes( tmpByteStr, size ) ;
        buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
        try
          oType := buff.getObjType( version ) ;
          if ( oType <> $40 ) then begin
            ret := false;
          end
          else begin
            buff.resetPosition;
            ret2 := vportControl.parseDwg( version, buff, bs ) ;
            if ( ret ) then
              ret := ret2;
          end ;
        finally
          FreeObject( buff ) ;
        end ;

        for it in vportControl.handlesList do begin
          if not mapHandles.TryGetValue( it, oc ) then begin
            dwg_log( '\nWARNING: vport not found\n' ) ;
            ret := false;
          end
          else begin
            mapHandles.Remove( it ) ;
            vp := DwgVport.Create;
            _dbuf.setPosition( oc.loc ) ;
            size := _dbuf.getModularShort;
            if dwgVersionHigher( version, DWG_Version.AC1021 ) then
              bs := _dbuf.getUModularChar
            else
              bs := 0;
            SetLength( tmpByteStr, size ) ;
            _dbuf.getBytes( tmpByteStr, size ) ;
            buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
            try
              ret2 := vp.parseDwg( version, buff, bs ) ;
              mapVPorts.Add( vp.handle, vp ) ;
              if ( ret ) then
                ret := ret2;
            finally
              FreeObject( buff ) ;
            end ;
          end
        end
      finally
        FreeObject( vportControl ) ;
      end ;
    end ;

    // parse Block_records , start with Block_record control
    if not mapHandles.TryGetValue( _hdr.blockCtrl, oc ) then begin
      dwg_log( '\nWARNING: Block_record control not found\n' ) ;
      ret := false;
    end
    else begin
      mapHandles.Remove( _hdr.blockCtrl ) ;
      blockControl := DwgObjControl.Create;
      try
        _dbuf.setPosition( oc.loc ) ;
        cSize := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, cSize ) ;
        _dbuf.getBytes( tmpByteStr, cSize ) ;
        buff := DwgBuffer.Create( tmpByteStr, cSize, decoder ) ;
        try
          oType := buff.getObjType( version ) ;
          if ( oType <> $30 ) then begin
            ret := false;
          end
          else begin
            buff.resetPosition;
            ret2 := blockControl.parseDwg( version, buff, bs ) ;
            if ( ret ) then
              ret := ret2;
          end ;
        finally
          FreeObject( buff ) ;
        end ;

        for it in blockControl.handlesList do begin
          if not mapHandles.TryGetValue( it, oc ) then begin
            dwg_log( '\nWARNING: block record not found\n' ) ;
            ret := false;
          end
          else begin
            mapHandles.Remove( it ) ;
            br := DwgBlockRecord.Create;
            _dbuf.setPosition( oc.loc ) ;
            size := _dbuf.getModularShort;
            if dwgVersionHigher( version, DWG_Version.AC1021 ) then
              bs := _dbuf.getUModularChar
            else
              bs := 0;
            SetLength( tmpByteStr, size ) ;
            _dbuf.getBytes( tmpByteStr, size ) ;
            buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
            try
              ret2 := br.parseDwg( version, buff, bs ) ;
            finally
              FreeObject( buff ) ;
            end ;
            mapBlockRecords.Add( br.handle, br ) ;
            if ( ret ) then
              ret := ret2;
          end
        end
      finally
        FreeObject( blockControl ) ;
      end ;
    end ;

    if not mapHandles.TryGetValue( _hdr.appidCtrl, oc ) then begin
      dwg_log( '\nWARNING: AppId control not found\n' ) ;
      ret := false;
    end
    else begin
      mapHandles.Remove( _hdr.appidCtrl ) ;
      appIdControl := DwgObjControl.Create;
      try
        _dbuf.setPosition( oc.loc ) ;
        size := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, size ) ;
        _dbuf.getBytes( tmpByteStr, size ) ;
        buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
        try
          oType := buff.getObjType( version ) ;
          if ( oType <> $42 ) then begin
            ret := false;
          end
          else begin
            buff.resetPosition;
            ret2 := appIdControl.parseDwg( version, buff, bs ) ;
            if ( ret ) then
              ret := ret2;
          end ;
        finally
          FreeObject( buff ) ;
        end ;

        for it in appIdControl.handlesList do begin
          if not mapHandles.TryGetValue( it, oc ) then begin
            dwg_log( '\nWARNING: AppId not found\n' ) ;
            ret := false;
          end
          else begin
            mapHandles.Remove( it ) ;
            ai := DwgAppId.Create;
            _dbuf.setPosition( oc.loc ) ;
            size := _dbuf.getModularShort;
            if dwgVersionHigher( version, DWG_Version.AC1021 ) then
              bs := _dbuf.getUModularChar
            else
              bs := 0;
            if size > 0 then begin
              SetLength( tmpByteStr, size ) ;
              _dbuf.getBytes( tmpByteStr, size ) ;
              buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
              try
                ret2 := ai.parseDwg( version, buff, bs ) ;
                mapAppIds.Add( ai.handle, ai ) ;
                if ( ret ) then
                  ret := ret2;
              finally
                FreeObject( buff ) ;
              end ;
            end;
          end
        end
      finally
        FreeObject( appIdControl ) ;
      end ;
    end ;

    if not mapHandles.TryGetValue( _hdr.viewCtrl, oc ) then begin
      dwg_log( '\nWARNING: View control not found\n' ) ;
      ret := false;
    end
    else begin
      mapHandles.Remove( _hdr.viewCtrl ) ;
      viewControl := DwgObjControl.Create;
      try
        _dbuf.setPosition( oc.loc ) ;
        size := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, size ) ;
        _dbuf.getBytes( tmpByteStr, size ) ;
        buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
        try
          // verify if object are correct
          oType := buff.getObjType( version ) ;
          if ( oType <> $3C ) then begin
            ret := false;
          end
          else begin // reset position
            buff.resetPosition;
            ret2 := viewControl.parseDwg( version, buff, bs ) ;
            if ( ret ) then
              ret := ret2;
          end
        finally
          FreeObject( buff ) ;
        end ;
      finally
        FreeObject( viewControl ) ;
      end ;
    end ;

    if not mapHandles.TryGetValue( _hdr.ucsCtrl, oc ) then begin
      dwg_log( '\nWARNING: Ucs control not found\n' ) ;
      ret := false;
    end
    else begin
      mapHandles.Remove( _hdr.ucsCtrl ) ;
      ucsControl := DwgObjControl.Create;
      try
        _dbuf.setPosition( oc.loc ) ;
        size := _dbuf.getModularShort;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then
          bs := _dbuf.getUModularChar
        else
          bs := 0;
        SetLength( tmpByteStr, size ) ;
        _dbuf.getBytes( tmpByteStr, size ) ;
        buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
        try
          oType := buff.getObjType( version ) ;
          if ( oType <> $3E ) then begin
            ret := false;
          end
          else begin // reset position
            buff.resetPosition;
            ret2 := ucsControl.parseDwg( version, buff, bs ) ;
            if ( ret ) then
              ret := ret2;
          end
        finally
          FreeObject( buff ) ;
        end ;
      finally
        FreeObject( ucsControl ) ;
      end ;
    end ;

    if dwgVersionLower( version, DWG_Version.AC1018 ) then begin
      if not mapHandles.TryGetValue( _hdr.vpEntHeaderCtrl, oc ) then begin
        dwg_log( '\nWARNING: vpEntHeader control not found\n' ) ;
        ret := false;
      end
      else begin
        mapHandles.Remove( _hdr.vpEntHeaderCtrl ) ;
        vpEntHeader := DwgObjControl.Create;
        try
          _dbuf.setPosition( oc.loc ) ;
          size := _dbuf.getModularShort;
          if dwgVersionHigher( version, DWG_Version.AC1021 ) then
            _dbuf.getUModularChar;
          SetLength( tmpByteStr, size ) ;
          _dbuf.getBytes( tmpByteStr, size ) ;
          buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
          try
            oType := buff.getObjType( version ) ;
            if ( oType <> $46 ) then begin
              ret := false;
            end
            else begin
              buff.resetPosition;
              ret2 := vpEntHeader.parseDwg( version, buff, bs ) ;
              if ( ret ) then
                ret := ret2;
            end
          finally
            FreeObject( buff ) ;
          end ;
        finally
          FreeObject( vpEntHeader ) ;
        end ;
      end
    end ;

    iRdrClbk.addHeader(_hdr);

    for it_lt in mapLType do
      iRdrClbk.addLType( DwgLType(it_lt.Value) ) ;

    for it_la in mapLayers do
      iRdrClbk.addLayer( DwgLayer(it_la.Value) ) ;

    for it_sty in mapStyles do
      iRdrClbk.addTextStyle( DwgTextstyle(it_sty.Value) ) ;

    for it_dsty in mapDimStyles do
      iRdrClbk.addDimStyle( DwgDimstyle(it_dsty.Value) ) ;

    for it_vp in mapVPorts do
      iRdrClbk.addVport( DwgVport(it_vp.Value) ) ;

    for it_ai in mapAppIds do
      iRdrClbk.addAppId( DwgAppId(it_ai.Value) ) ;

    Result := true;
  end ;

  function DwgReader.readFileHeader : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.readMetaData : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.readPlineVertex(
    const _pline  : DwgPolyline;
    const _dbuf   : DwgBuffer
  ) : Boolean ;
  var
    ret        : Boolean;
    oc         : {$IFDEF JAVA} nullable {$ENDIF} DwgObjHandle;
    bs         : duint32;
    nextH      : duint64;
    {$IFDEF DCC}
    mit        : duint64;
    {$ENDIF}
    vt         : DwgVertex;
    size       : dint32;
    tmpByteStr : TBytes;
    buff       : DwgBuffer;
    oType      : dint16;
    oldnextEntLink        : duint64;
    oldprevEntLink        : duint64;
  begin
    bs := 0;
    oldnextEntLink := nextEntLink ;
    oldprevEntLink := prevEntLink ;
    ret := false;
    if dwgVersionLower( version, DWG_Version.AC1018 ) then begin
      nextH := _pline.firstEH;
      while ( nextH <> 0 ) do begin
        if not mapHandles.TryGetValue( nextH, oc ) then begin
          nextH := _pline.lastEH;
          dwg_log( '\nWARNING: pline vertex not found\n' );
          ret := false;
          continue;
        end
        else begin
          vt := DwgVertex.Create;
          _dbuf.setPosition( oc.loc );

          size := _dbuf.getModularShort;
          if dwgVersionHigher( version, DWG_Version.AC1021 ) then begin
            bs := _dbuf.getUModularChar;
          end ;
          SetLength( tmpByteStr, size );
          _dbuf.getBytes( tmpByteStr, size );
          buff := DwgBuffer.Create( tmpByteStr, size, decoder );
          try
            oType := buff.getObjType( version );
            buff.resetPosition;
            ret := vt.parseDwgEx( version, buff, bs, _pline.basePoint.z );
            _pline.vertlist.Add( vt );
          finally
            FreeObject( buff ) ;
          end ;
          nextEntLink := vt.nextEntLink;
          prevEntLink := vt.prevEntLink;
        end ;
        if ( nextH = _pline.lastEH ) then
          nextH := 0
        else
          nextH := nextEntLink;
      end ;
    end
    else begin
      for mit in _pline.hadlesList do begin
        nextH := mit;
        if not mapHandles.TryGetValue( nextH, oc ) then begin
          dwg_log( '\nWARNING: Entity of block not found\n' );
          ret := false;
          continue;
        end
        else begin
          vt := DwgVertex.Create;
          _dbuf.setPosition( oc.loc );

          size := _dbuf.getModularShort;
          if dwgVersionHigher( version, DWG_Version.AC1021 ) then begin
            bs := _dbuf.getUModularChar;
          end ;
          SetLength( tmpByteStr, size );
          _dbuf.getBytes( tmpByteStr, size );
          buff := DwgBuffer.Create( tmpByteStr, size, decoder );
          try
            oType := buff.getObjType( version );
            buff.resetPosition;
            ret := vt.parseDwgEx( version, buff, bs, _pline.basePoint.z );
            _pline.vertlist.Add( vt );
          finally
            FreeObject( buff ) ;
          end ;
          nextEntLink := vt.nextEntLink;
          prevEntLink := vt.prevEntLink;
        end ;
      end ;
    end ;

    nextEntLink := oldnextEntLink ;
    prevEntLink := oldprevEntLink ;
    Result := ret;
  end ;

  function DwgReader.readPreview : Boolean;
  begin
    Result := false;
  end ;

  function DwgReader.setupDecoder(
    const _codepage: Integer
  ) : Boolean;

    procedure setEncoding( const _cp : Integer ) ;
    begin
      {$IFDEF JAVA}
        var scp := TCodePageConverter.Convert(_cp) ;
        if not IsStringEmpty(scp) then
          decoder := TEncoding.GetEncoding( scp )
        else
          decoder := TEncoding.ASCII ;
      {$ELSE}
        decoder := TEncoding.GetEncoding( _cp ) ;
      {$ENDIF}
      ownDecoder := True ;
    end ;

  begin
    Result := True ;
    if      _codepage = 28 then
      setEncoding( 1250 )
    else if _codepage = 29 then
      setEncoding( 28591 )
    else if _codepage = 30 then
      setEncoding( 28591 )
    else if _codepage = 31 then
      setEncoding( 936 )
    else if _codepage = 32 then
      setEncoding( 1253 )
    else if _codepage = 33 then
      setEncoding( 1254 )
    else if _codepage = 34 then
      setEncoding( 1255 )
    else if _codepage = 35 then
      setEncoding( 1256 )
    else if _codepage = 36 then
      setEncoding( 1257 )
    else if _codepage = 34 then
      setEncoding( 1255 )
    else if _codepage = 37 then
      setEncoding( 874 )
    else if _codepage = 38 then
      setEncoding( 932 )
    else if _codepage = 39 then
      setEncoding( 936 )
    else if _codepage = 40 then
      setEncoding( 949 )
    else if _codepage = 41 then
      setEncoding( 950 )
    else if _codepage = 42 then
      setEncoding( 1361 )
    else if _codepage = 43 then
      setEncoding( 65001 )
    else if _codepage = 44 then
      setEncoding( 1258 )
    else if _codepage = 1 then
      setEncoding( 20127 )
    else if _codepage = 2 then
      setEncoding( 28591 )
    else if _codepage = 3 then
      setEncoding( 28592 )
    else if _codepage = 5 then
      setEncoding( 28594 )
    else if _codepage = 6 then
      setEncoding( 28595 )
    else if _codepage = 7 then
      setEncoding( 28596 )
    else if _codepage = 8 then
      setEncoding( 28597 )
    else if _codepage = 9 then
      setEncoding( 28598 )
    else if _codepage = 10 then
      setEncoding( 28599 )
    else if _codepage = 22 then
      setEncoding( 932 )
    else if _codepage = 24 then
      setEncoding( 950 )
    else
      Result := False ;
  end;

  function DwgReader.prepareModel(
    const _hdr    : DwgHeader ;
    const _model  : T_FileDwg_Model
  ) : Boolean ;
  var
    bhdr      : DwgTableEntry ;
    block_hdr : DwgBlockRecord ;
    adbl_min  : TArray<Double> ;
    adbl_max  : TArray<Double> ;
    i         : Integer ;
  begin
    dwg_log('\nBuilding model...\n') ;

    if mapBlockRecords.TryGetValue( _hdr.modelSpaceBlkRec, bhdr ) then begin
      block_hdr := bhdr as DwgBlockRecord ;
      adbl_min := TArray<Double>(_hdr.vars['EXTMIN']) ;
      adbl_max := TArray<Double>(_hdr.vars['EXTMAX']) ;
      dwg_log( 'Extent: ' ) ;
      dwg_log( 'xmin: '  ) ; dwg_log( DotFloatToStr(adbl_min[0]) ) ;
      dwg_log( ' ymin: ' ) ; dwg_log( DotFloatToStr(adbl_min[1]) ) ;
      dwg_log( ' zmin: ' ) ; dwg_log( DotFloatToStr(adbl_min[2]) ) ;
      dwg_log( ' xmax: ' ) ; dwg_log( DotFloatToStr(adbl_max[0]) ) ;
      dwg_log( ' ymax: ' ) ; dwg_log( DotFloatToStr(adbl_max[1]) ) ;
      dwg_log( ' zmax: ' ) ; dwg_log( DotFloatToStr(adbl_max[2]) ) ;
      dwg_log( '\n' ) ;
      dwg_log( '\n' ) ;

      _model.Extent := GisExtent3D( adbl_min[0], adbl_min[1], adbl_min[2],
                                    adbl_max[0], adbl_max[1], adbl_max[2]
                                   ) ;

      dwg_log( 'Entities\n' ) ;
      if dwgVersionLower( version, DWG_Version.AC1018 ) then begin
        dwg_log( ' First entity: ' ) ;
        dwg_log( LowerCase('0x'+IntToHex(block_hdr.firstEH, 2)) )  ;
        dwg_log( '\n' ) ;
        dwg_log( ' Last entity: ' ) ;
        dwg_log( LowerCase('0x'+IntToHex(block_hdr.lastEH, 2)) )  ;
        dwg_log( '\n' ) ;
        _model.FirstEntity := block_hdr.firstEH ;
        _model.LastEntity  := block_hdr.lastEH ;
      end
      else begin
        if block_hdr.entMap.Count > 0 then begin
          dwg_log( ' First entity: ' ) ;
          dwg_log( LowerCase('0x'+IntToHex(block_hdr.entMap[0], 2)) )  ;
          dwg_log( '\n' ) ;
          dwg_log( ' Last entity: ' ) ;
          dwg_log( LowerCase('0x'+IntToHex(block_hdr.entMap[block_hdr.entMap.Count-1], 2)) )  ;
          dwg_log( '\n' ) ;
          _model.FirstEntity := block_hdr.entMap[0] ;
          _model.LastEntity  := block_hdr.entMap[block_hdr.entMap.Count-1] ;
        end ;
      end ;
    end ;

    for i := low( DWG_COLORS) to high( DWG_COLORS ) do
       _model.AddPaletteColor( TGIS_Color.FromRGB( DWG_COLORS[i][0],
                                                   DWG_COLORS[i][1],
                                                   DWG_COLORS[i][2]
                                                  )
                             );

    Result := True ;
  end ;

// ----------------------------------------------------------------------------
// DwgReader15
// ----------------------------------------------------------------------------

  function DwgReader15.readDwgBlocks : Boolean;
  begin
    Result := inherited readDwgBlocksEx( fileBuf ) ;
  end ;

  function DwgReader15.readDwgClasses : Boolean;
  var
    si         : DwgSectionInfo;
    size       : duint32;
    tmpByteStr : TBytes;
    buff       : DwgBuffer;
    cl         : DwgClass;
  begin
    si := mapSections[ DWG_Section.CLASSES ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;
    if not fileBuf.setPosition( si.address ) then begin
      Result := false;
      exit;
    end ;

    checkSentinel( fileBuf, DWG_Section.CLASSES, true ) ;

    size := fileBuf.getRawLong32 ;
    if ( size <> ( si.size - 38 ) ) then begin
      dwg_log( '\nWARNING readDwgClasses size is invalid\n' ) ;
    end ;
    SetLength( tmpByteStr, size ) ;

    fileBuf.getBytes( tmpByteStr, size ) ;
    buff := DwgBuffer.Create( tmpByteStr, size, decoder ) ;
    try
      dec( size ) ;
      while ( size > buff.getPosition ) do begin
        cl := DwgClass.Create;
        cl.parseDwg( version, buff, buff ) ;
        mapClasses.Add( cl.classNum, cl ) ;
      end ;
      fileBuf.getRawShort16 ;
      checkSentinel( fileBuf, DWG_Section.CLASSES, false ) ;
      Result := buff.isGood ;
    finally
      FreeObject( buff ) ;
    end ;
  end ;

  function DwgReader15.readDwgEntities : Boolean;
  begin
    Result := inherited readDwgEntitiesEx( fileBuf ) ;
  end ;

  function DwgReader15.readDwgHandles : Boolean;
  var
    si : DwgSectionInfo;
  begin
    si := mapSections[ DWG_Section.HANDLES ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;

    Result := inherited readDwgHandlesEx( fileBuf, si.address, si.size ) ;
  end ;

  function DwgReader15.readDwgHeader(
    const _hdr : DwgHeader
  ) : Boolean;
  var
    si         : DwgSectionInfo;
    tmpByteStr : TBytes;
    buff       : DwgBuffer;
  begin
    si := mapSections[ DWG_Section.HEADER ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;
    if not fileBuf.setPosition( si.address ) then begin
      Result := false;
      exit;
    end ;
    SetLength( tmpByteStr, si.size ) ;
    fileBuf.getBytes( tmpByteStr, si.size ) ;
    buff := DwgBuffer.Create( tmpByteStr, si.size, decoder ) ;
    try
      checkSentinel( buff, DWG_Section.HEADER, true ) ;
      Result := inherited readDwgHeaderEx( _hdr, buff, buff ) ;
    finally
      FreeObject( buff ) ;
    end ;
  end ;

  function DwgReader15.readDwgObjects : Boolean;
  begin
    Result := inherited readDwgObjectsEx( fileBuf ) ;
  end ;

  function DwgReader15.readDwgTables(
    const _hdr : DwgHeader
  ) : Boolean;
  begin
    Result := inherited readDwgTablesEx( _hdr, fileBuf ) ;
  end ;

  function DwgReader15.readFileHeader : Boolean;
  var
    count   : duint32;
    i       : Integer;
    rec     : duint8;
    address : duint32;
    size    : duint32;
    si      : DwgSectionInfo;
    ckcrc   : duint32;
  begin
    if not fileBuf.setPosition( 21 ) then begin
      Result := false;
      exit;
    end ;
    count := fileBuf.getRawLong32 ;

    for i := 0 to count - 1 do begin
      rec := fileBuf.getRawChar8 ;
      address := fileBuf.getRawLong32 ;
      size := fileBuf.getRawLong32 ;

      si := DwgSectionInfo.Create;
      si.Id := rec;
      si.size := size;
      si.address := address;
      if ( rec = 0 ) then begin
        mapSections.Add( DWG_Section.HEADER, si ) ;
      end
      else if ( rec = 1 ) then begin
        mapSections.Add( DWG_Section.CLASSES, si ) ;
      end
      else if ( rec = 2 ) then begin
        mapSections.Add( DWG_Section.HANDLES, si ) ;
      end
      else if ( rec = 3 ) then begin
        mapSections.Add( DWG_Section.UNKNOWNS, si ) ;
      end
      else if ( rec = 4 ) then begin
        mapSections.Add( DWG_Section.TEMPLATE, si ) ;
      end
      else if ( rec = 5 ) then begin
        mapSections.Add( DWG_Section.AUXHEADER, si ) ;
      end
    end ;
    if not fileBuf.isGood then begin
      Result := false;
      exit;
    end ;
    ckcrc := 0;
    case ( count ) of
      3 : ckcrc := ckcrc xor $A598;
      4 : ckcrc := ckcrc xor $8101;
      5 : ckcrc := ckcrc xor $3CC4;
      6 : ckcrc := ckcrc xor $8461;
    end ;
    fileBuf.getRawShort16 ;
    checkSentinel( fileBuf, DWG_Section.FILEHEADER, false ) ;
    Result := true;
  end ;

  function DwgReader15.readMetaData : Boolean;
  var
    meas : duint16;
    cp   : duint16;
  begin
    if not fileBuf.setPosition( 13 ) then begin
      Result := false;
      exit;
    end ;
    previewImagePos := fileBuf.getRawLong32 ;
    meas := fileBuf.getRawShort16 ;
    cp := fileBuf.getRawShort16 ;
    setupDecoder( cp ) ;
    Result := true;
  end ;

// ----------------------------------------------------------------------------
// DwgReader18
// ----------------------------------------------------------------------------

  function DwgReader18.checksum(
    const seed : duint32;
    const data : TBytes;
    const sz   : duint32
  ) : duint32;
  var
    size, sum1, sum2 : duint32;
    chunkSize        : duint32;
    i, pos           : Integer;
  begin
    size := sz;
    sum1 := seed and $FFFF;
    sum2 := seed shr $10;
    pos := 0;

    while ( size <> 0 ) do begin
      if $15B0 < size then
        chunkSize := $15B0
      else
        chunkSize := size;
      size := size - chunkSize;
      for i := 0 to chunkSize - 1 do begin
        sum1 := sum1 + data[ pos ];
        inc( pos ) ;
        sum2 := sum2 + sum1;
      end ;
      sum1 := sum1 mod $FFF1;
      sum2 := sum2 mod $FFF1;
    end ;
    Result := ( sum2 shl $10 ) or ( sum1 and $FFFF ) ;
  end ;

  procedure DwgReader18.genMagicNumber;
  var
    size, rSeed,
    p           : Integer;
    tmpMagicStr : TBytes;
  begin
    size := $114;
    SetLength( tmpMagicStr, size ) ;
    p := 0;
    rSeed := 1;
    while ( size > 0 ) do begin
      rSeed := rSeed * $343FD;
      rSeed := rSeed + $269EC3;
      tmpMagicStr[ p ] := duint8( rSeed shr $10 ) ;
      dec( size ) ;
      inc( p ) ;
    end ;
  end ;

  function DwgReader18.parseDataPage(
    const _si : DwgSectionInfo
  ) : Boolean;
  var
    {$IFDEF DCC}
    it        : TPair<duint32, TObject>;
    {$ENDIF}
    pi        : DwgPageInfo;
    hdrData,
    cData,
    oData     : TBytes;
    i         : Integer;
    bufHdr    : DwgBuffer;
    calcsD,
    calcsH    : duint32;
  begin
    SetLength( objData, _si.pageCount * _si.maxSize ) ;

    for it in _si.pages do begin
      pi := it.Value as DwgPageInfo;
      if not fileBuf.setPosition( pi.address ) then begin
        Result := false;
        exit;
      end ;

      SetLength( hdrData, 32 ) ;
      fileBuf.getBytes( hdrData, 32 ) ;
      DwgCompressor.decrypt18Hdr( hdrData, 32, pi.address ) ;

      bufHdr := DwgBuffer.Create( hdrData, 32, decoder ) ;
      try
        bufHdr.getRawLong32 ;
        bufHdr.getRawLong32 ;
        pi.cSize := bufHdr.getRawLong32;
        pi.uSize := bufHdr.getRawLong32;
        bufHdr.getRawLong32 ;
        bufHdr.getRawLong32 ;
        bufHdr.getRawLong32 ;
        bufHdr.getRawLong32 ;
      finally
        FreeObject( bufHdr ) ;
      end ;

      SetLength( cData, pi.cSize ) ;
      if not fileBuf.setPosition( pi.address + 32 ) then begin
        Result := false;
        exit;
      end ;
      fileBuf.getBytes( cData, pi.cSize ) ;

      calcsD := checksum( 0, cData, pi.cSize ) ;
      for i := 24 to 28 - 1 do
        hdrData[i] := 0;
      calcsH := checksum( calcsD, hdrData, 32 ) ;

      pi.uSize := _si.maxSize;
      SetLength( oData, pi.uSize ) ;

      DwgCompressor.decompress18( cData, oData, pi.cSize, pi.uSize ) ;
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          System.Buffer.BlockCopy( oData, 0, objData, pi.startOffset, pi.uSize ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          System.arraycopy( oData, 0, objData, pi.startOffset, pi.uSize ) ;
        {$ENDIF}
      {$ELSE}
        Move( oData[0], objData[ pi.startOffset ], pi.uSize ) ;
      {$ENDIF}
    end ;
    Result := true;
  end ;

  function DwgReader18.parseSysPage(
    const decompSec  : TBytes;
    const decompSize : duint32
  ) : Boolean;
  var
    compSize            : duint32;
    hdrData, tmpCompSec : TBytes;
    i                   : Integer;
    calcsH, calcsD      : duint32;
  begin
    compSize := fileBuf.getRawLong32;
    fileBuf.getRawLong32 ;
    fileBuf.getRawLong32 ;

    SetLength( hdrData, 20 ) ;
    fileBuf.moveBitPos( - 160 ) ;
    fileBuf.getBytes( hdrData, 20 ) ;
    for i := 16 to 20 - 1 do
      hdrData[i] := 0;
    calcsH := checksum( 0, hdrData, 20 ) ;

    SetLength( tmpCompSec, compSize ) ;
    fileBuf.getBytes( tmpCompSec, compSize ) ;
    calcsD := checksum( calcsH, tmpCompSec, compSize ) ;

    DwgCompressor.decompress18( tmpCompSec, decompSec, compSize, decompSize ) ;
    Result := true;
  end ;

  function DwgReader18.readDwgBlocks : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
    try
      Result := inherited readDwgBlocksEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader18.readDwgClasses : Boolean;
  var
    si              : DwgSectionInfo;
    dataBuf         : DwgBuffer;
    size, bitSize   : duint32;
    hSize           : duint32;
    maxClassNum     : duint32;
    strBuf, strBuff : DwgBuffer;
    strStartPos     : duint32;
    strDataSize     : duint32;
    hiSize          : duint32;
    endDataPos      : duint32;
    cl              : DwgClass;
    i               : Integer;
  begin
    si := mapSections[ DWG_Section.CLASSES ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;
    Result := parseDataPage( si ) ;
    uncompSize := si.size;
    if ( Result ) then begin

      dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
      try
        checkSentinel( dataBuf, DWG_Section.CLASSES, true ) ;

        size := dataBuf.getRawLong32;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) and ( maintenanceVersion > 3 ) then
        begin
          hSize := dataBuf.getRawLong32;
        end ;
        bitSize := 0;
        if dwgVersionHigher( version, DWG_Version.AC1021 ) then begin
          bitSize := dataBuf.getRawLong32;
        end ;
        maxClassNum := dataBuf.getBitShort;
        dataBuf.getRawChar8 ;
        dataBuf.getRawChar8 ;
        dataBuf.getBit ;

        strBuf := dataBuf;
        strBuff := DwgBuffer.Create( objData, uncompSize, decoder ) ;
        try
          if dwgVersionHigher( version, DWG_Version.AC1021 ) then begin
            strBuf := strBuff;
            strStartPos := bitSize + 191;
            strBuff.setPosition( strStartPos shr 3 ) ;
            strBuff.setBitPos( strStartPos and 7 ) ;
            strBuff.getBit ;
            strStartPos := strStartPos - 16;
            strBuff.setPosition( strStartPos shr 3 ) ;
            strBuff.setBitPos( strStartPos and 7 ) ;
            strDataSize := strBuff.getRawShort16;
            if ( strDataSize and $8000 ) <> 0 then begin
              strStartPos := strStartPos - 16;
              strDataSize := strDataSize and $7FFF;
              strBuff.setPosition( strStartPos shr 3 ) ;
              strBuff.setBitPos( strStartPos and 7 ) ;
              hiSize := strBuff.getRawShort16;
              strDataSize := strDataSize or ( hiSize shl 15 ) ;
            end ;
            strStartPos := strStartPos - strDataSize;
            strBuff.setPosition( strStartPos shr 3 ) ;
            strBuff.setBitPos( strStartPos and 7 ) ;
          end ;

          endDataPos := maxClassNum - 499;
          for i := 0 to endDataPos - 1 do begin
            cl := DwgClass.Create;
            cl.parseDwg( version, dataBuf, strBuf ) ;
            mapClasses.Add( cl.classNum, cl ) ;
          end ;

          strBuf.setPosition( strBuf.getPosition + 1 ) ;
          strBuf.getRawShort16 ;
          if dwgVersionHigher( version, DWG_Version.AC1018 ) then begin
            strBuf.getRawShort16 ;
          end ;
          checkSentinel( strBuf, DWG_Section.CLASSES, false ) ;

          Result := strBuf.isGood;
        finally
          FreeObject( strBuff ) ;
        end ;
      finally
        FreeObject( dataBuf ) ;
      end ;
    end ;

    objData := nil;
  end ;

  function DwgReader18.readDwgEntities : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
    try
      Result := inherited readDwgEntitiesEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader18.readDwgHandles : Boolean;
  var
    si      : DwgSectionInfo;
    dataBuf : DwgBuffer;
  begin
    si := mapSections[ DWG_Section.HANDLES ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;
    Result := parseDataPage( si ) ;

    uncompSize := si.size;
    if Result then begin
      dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
      try
        Result := inherited readDwgHandlesEx( dataBuf, 0, si.size ) ;
      finally
        FreeObject( dataBuf ) ;
      end ;
    end ;

    objData := nil;
  end ;

  function DwgReader18.readDwgHeader(
    const _hdr : DwgHeader
  ) : Boolean;
  var
    si        : DwgSectionInfo;
    dataBuf,
    handleBuf : DwgBuffer;
  begin
    si := mapSections[ DWG_Section.HEADER ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;
    Result := parseDataPage( si ) ;

    uncompSize := si.size;
    if Result then begin
      dataBuf := DwgBuffer.Create( objData, si.size, decoder ) ;
      try
        checkSentinel( dataBuf, DWG_Section.HEADER, true ) ;
        if ( version = DWG_Version.AC1018 ) then
          Result := inherited readDwgHeaderEx( _hdr, dataBuf, dataBuf )
        else begin
          handleBuf := DwgBuffer.Create( objData, si.size, decoder ) ;
          try
            Result := inherited readDwgHeaderEx( _hdr, dataBuf, handleBuf ) ;
          finally
            FreeObject( handleBuf ) ;
          end ;
        end ;
      finally
        FreeObject( dataBuf ) ;
      end ;
    end ;
    objData := nil;
  end ;

  function DwgReader18.readDwgObjects : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
    try
      Result := inherited readDwgObjectsEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader18.readDwgTables(
    const _hdr : DwgHeader
  ) : Boolean;
  var
    si      : DwgSectionInfo;
    dataBuf : DwgBuffer;
  begin
    si := mapSections[ DWG_Section.OBJECTS ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;
    Result := parseDataPage( si ) ;

    uncompSize := si.size;
    if Result then begin
      dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
      try
        Result := inherited readDwgTablesEx( _hdr, dataBuf ) ;
      finally
        FreeObject( dataBuf ) ;
      end ;
    end ;
  end ;

  function DwgReader18.readFileHeader : Boolean;
  var
    byteStr, tmpDecompSec : TBytes;
    size, i, j            : Integer;
    ch                    : duint8;
    buff, buff2, buff3    : DwgBuffer;
    name                  : String;
    secPageMapId          : dint32;
    secPageMapAddr, ii    : duint64;
    secMapId              : duint32;
    pageType              : duint32;
    decompSize            : duint32;
    address               : duint32;
    sectionPageMapTmp     : TDictionary<dint64, DwgPageInfo>;
    Id                    : Int32;
    size2                 : duint32;
    sectionMap            : DwgPageInfo;
    numDescriptions       : duint32;
    secInfo               : DwgSectionInfo;
    nameCStr              : TBytes;
    pn                    : duint64;
    pi                    : DwgPageInfo;
    {$IFDEF DCC}
    tpi                   : TPair<dint64, DwgPageInfo>;
    {$ENDIF}
  begin
    Result := false;
    if not fileBuf.setPosition( $80 ) then begin
      Result := false;
      exit;
    end ;

    SetLength( byteStr, $6C ) ;
    size := $6C;
    j := 0;
    for i := 0 to $6C - 1 do begin
      ch := fileBuf.getRawChar8;
      byteStr[i] := DwgmagicNum18[i] xor ch;
    end ;

    buff := DwgBuffer.Create( byteStr, $6C, decoder ) ;
    try
      name := convertAnsiString( byteStr ) ;

      buff.setPosition( 12 ) ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong64 ;
      buff.getRawLong64 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      secPageMapId := buff.getRawLong32;
      secPageMapAddr := buff.getRawLong64 + $100;
      secMapId := buff.getRawLong32;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
      buff.getRawLong32 ;
    finally
      FreeObject( buff ) ;
    end ;

    for i := $68 to $6C - 1 do
      byteStr[i] := 0;

    j := 0;
    for i := 0 to $14 - 1 do begin
      fileBuf.getRawChar8 ;
    end ;

    if not fileBuf.setPosition( secPageMapAddr ) then begin
      Result := false;
      exit;
    end ;
    pageType := fileBuf.getRawLong32;
    decompSize := fileBuf.getRawLong32;
    if ( pageType <> $41630E3B ) then begin
      dwg_log( '\nWARNING: bad page type, was expected $41630e3b instead\n' ) ;
      Result := false;
      exit;
    end ;
    SetLength( tmpDecompSec, decompSize ) ;
    parseSysPage( tmpDecompSec, decompSize ) ;

    sectionPageMapTmp := TDictionary<dint64, DwgPageInfo>.Create;
    try
      buff2 := DwgBuffer.Create( tmpDecompSec, decompSize, decoder ) ;
      try
        address := $100;
        ii := 0;
        while ii < decompSize do begin
          Id := dint32(buff2.getRawLong32);
          size2 := buff2.getRawLong32;
          ii := ii + 8;
          if ( Id < 0 ) then begin
            buff2.getRawLong32 ;
            buff2.getRawLong32 ;
            buff2.getRawLong32 ;
            buff2.getRawLong32 ;
            ii := ii + 16;
          end
          else
            sectionPageMapTmp.Add( Id, DwgPageInfo.Create( Id, address, size2 ) ) ;
          address := address + size2;
        end ;
      finally
        FreeObject( buff2 ) ;
      end ;

      sectionMap := sectionPageMapTmp[ dint64(secMapId) ];
      if not fileBuf.setPosition( sectionMap.address ) then begin
        Result := false;
        exit;
      end ;
      pageType := fileBuf.getRawLong32;
      decompSize := fileBuf.getRawLong32;
      if ( pageType <> $4163003B ) then begin
        dwg_log( '\nWARNING: bad page type, was expected $4163003b instead\n' ) ;
        Result := false;
        exit;
      end ;
      SetLength( tmpDecompSec, decompSize ) ;
      parseSysPage( tmpDecompSec, decompSize ) ;

      buff3 := DwgBuffer.Create( tmpDecompSec, decompSize, decoder ) ;
      try
        numDescriptions := buff3.getRawLong32;
        buff3.getRawLong32 ;
        buff3.getRawLong32 ;
        buff3.getRawLong32 ;
        buff3.getRawLong32 ;

        for i := 0 to numDescriptions - 1 do begin
          secInfo := DwgSectionInfo.Create;
          secInfo.size := buff3.getRawLong64;
          secInfo.pageCount := buff3.getRawLong32;
          secInfo.maxSize := buff3.getRawLong32;
          buff3.getRawLong32 ;
          secInfo.compresed := buff3.getRawLong32;
          secInfo.Id := buff3.getRawLong32;
          secInfo.encrypted := buff3.getRawLong32;

          SetLength( nameCStr, 64 ) ;
          buff3.getBytes( nameCStr, 64 ) ;
          secInfo.name := convertAnsiString( nameCStr ) ;
          if secInfo.pageCount > 0 then
            for j := 0 to secInfo.pageCount - 1 do begin
              pn := buff3.getRawLong32;
              if not sectionPageMapTmp.TryGetValue( pn, pi ) then begin
                buff3.getRawLong32;
                buff3.getRawLong64;
                sectionPageMapTmp.Remove( pn ) ;
                continue ;
              end ;
              pi.dataSize := buff3.getRawLong32;
              pi.startOffset := buff3.getRawLong64;
              secInfo.pages.Add( pn, pi ) ;
              sectionPageMapTmp.Remove( pn ) ;
            end ;

          if length( secInfo.name ) > 0 then begin
            mapSections.AddOrSetValue( getEnum( secInfo.name ), secInfo ) ;
          end
          else begin
            sectionPageMapTmp.Remove( secInfo.Id ) ;
            FreeObject( secInfo ) ;
          end ;
        end ;
      finally
        FreeObject( buff3 ) ;
      end ;
    finally
      for tpi in  sectionPageMapTmp do
        FreeObjectNotNil( tpi.Value ) ;

      FreeObject( sectionPageMapTmp ) ;
    end ;

    if not fileBuf.isGood then begin
      Result := false;
      exit;
    end ;
    Result := true;
  end ;

  function DwgReader18.readMetaData : Boolean;
  var
    cp          : duint16;
    uk          : duint32;
    sumInfoAddr : duint32;
    vbaAdd      : duint32;
  begin
    if not fileBuf.setPosition( 11 ) then begin
      Result := false;
      exit;
    end ;
    maintenanceVersion := fileBuf.getRawChar8 ;
    fileBuf.getRawChar8 ;
    previewImagePos := fileBuf.getRawLong32;
    fileBuf.getRawChar8 ;
    fileBuf.getRawChar8 ;
    cp := fileBuf.getRawShort16;
    setupDecoder( cp ) ;
    fileBuf.getRawChar8 ;
    fileBuf.getRawChar8 ;
    fileBuf.getRawChar8 ;
    securityFlags := fileBuf.getRawLong32;

    uk := fileBuf.getRawLong32;
    sumInfoAddr := fileBuf.getRawLong32;
    vbaAdd := fileBuf.getRawLong32;
    fileBuf.getRawLong32 ;
    Result := true;
  end ;

// ----------------------------------------------------------------------------
// DwgReader21
// ----------------------------------------------------------------------------

  function DwgReader21.parseDataPage(
    const _si    : DwgSectionInfo;
    const _dData : TBytes
  ) : Boolean;
  var
    {$IFDEF DCC}
    it         : TPair<duint32, TObject>;
    {$ENDIF}
    pi         : DwgPageInfo;
    tmpPageRaw : TBytes;
    tmpPageRS  : TBytes;
    chunks     : duint32;
    pageData   : TBytes;
  begin
    for it in _si.pages do begin
      pi := it.Value as DwgPageInfo ;
      if not fileBuf.setPosition( pi.address ) then begin
        Result := false;
        exit;
      end ;

      SetLength( tmpPageRaw, pi.size ) ;
      fileBuf.getBytes( tmpPageRaw, pi.size ) ;

      SetLength( tmpPageRS, pi.size ) ;

      chunks := pi.size div 255;
      DwgRSCodec.decode251I( tmpPageRaw, tmpPageRS, chunks ) ;

      SetLength( pageData, pi.uSize ) ;
      if pi.cSize < pi.uSize  then
        DwgCompressor.decompress21( tmpPageRS, pageData, pi.cSize, pi.uSize )
      else
        pageData := tmpPageRS ;

      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          System.Buffer.BlockCopy( pageData, 0, _dData, pi.startOffset, pi.uSize ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          System.arraycopy( pageData, 0, _dData, pi.startOffset, pi.uSize )  ;
        {$ENDIF}
      {$ELSE}
        Move( pageData[0], _dData[ pi.startOffset ], pi.uSize ) ;
      {$ENDIF}
    end ;
    Result := true;
  end ;

  function DwgReader21.parseSysPage(
    const _sizeCompressed   : duint64;
    const _sizeUncompressed : duint64;
    const _correctionFactor : duint64;
    const _offset           : duint64;
    const _decompData       : TBytes
  ) : Boolean;
  var
    alsize     : duint64;
    chunks     : duint32;
    fpsize     : duint64;
    tmpDataRaw : TBytes;
    tmpDataRS  : TBytes;
  begin
    alsize := ( _sizeCompressed + 7 ) and ( - 8 ) ;
    chunks := ( ( ( alsize * _correctionFactor ) + 238 ) div 239 ) ;
    fpsize := chunks * 255;

    if not fileBuf.setPosition( _offset ) then begin
      Result := false;
      exit;
    end ;

    SetLength( tmpDataRaw, fpsize ) ;
    fileBuf.getBytes( tmpDataRaw, fpsize ) ;

    SetLength( tmpDataRS, fpsize ) ;
    DwgRSCodec.decode239I( tmpDataRaw, tmpDataRS, fpsize div 255 ) ;

    DwgCompressor.decompress21( tmpDataRS, _decompData, _sizeCompressed,
                                _sizeUncompressed
                               ) ;
    Result := true;
  end ;

  function DwgReader21.readDwgBlocks : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, dataSize, decoder ) ;
    try
      Result := inherited readDwgBlocksEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader21.readDwgClasses : Boolean;
  var
    si             : DwgSectionInfo;
    tmpClassesData : TBytes;
    buff           : DwgBuffer;
    size           : duint32;
    bitSize        : duint32;
    maxClassNum    : duint32;
    strBuff        : DwgBuffer;
    strStartPos    : duint32;
    strDataSize    : duint32;
    hiSize         : duint32;
    endDataPos     : duint32;
    i              : Integer;
    cl             : DwgClass;
  begin
    si := mapSections[ DWG_Section.CLASSES ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;

    SetLength( tmpClassesData, si.size ) ;
    Result := parseDataPage( si, tmpClassesData ) ;
    if not Result then
      exit;

    buff := DwgBuffer.Create( tmpClassesData, si.size, decoder ) ;
    try
      checkSentinel( buff, DWG_Section.CLASSES, true ) ;

      size := buff.getRawLong32;
      bitSize := buff.getRawLong32;
      maxClassNum := buff.getBitShort;
      buff.getRawChar8 ;
      buff.getRawChar8 ;
      buff.getBit ;

      strBuff := DwgBuffer.Create( tmpClassesData, si.size, decoder ) ;
      try
        strStartPos := bitSize + 159;

        strBuff.setPosition( strStartPos shr 3 ) ;
        strBuff.setBitPos( strStartPos and 7 ) ;
        strBuff.getBit ;
        strStartPos := strStartPos - 16;
        strBuff.setPosition( strStartPos shr 3 ) ;
        strBuff.setBitPos( strStartPos and 7 ) ;
        strDataSize := strBuff.getRawShort16;
        if ( strDataSize and $8000 ) <> 0 then begin
          strStartPos := strStartPos - 16;
          strDataSize := strDataSize and $7FFF;
          strBuff.setPosition( strStartPos shr 3 ) ;
          strBuff.setBitPos( strStartPos and 7 ) ;
          hiSize := strBuff.getRawShort16;
          strDataSize := strDataSize or ( hiSize shl 15 ) ;
        end ;
        strStartPos := strStartPos - strDataSize;
        strBuff.setPosition( strStartPos shr 3 ) ;
        strBuff.setBitPos( strStartPos and 7 ) ;

        endDataPos := maxClassNum - 499;
        for i := 0 to endDataPos - 1 do begin
          cl := DwgClass.Create;
          cl.parseDwg( version, buff, strBuff ) ;
          mapClasses.Add( cl.classNum, cl ) ;
        end ;
      finally
        FreeObject( strBuff ) ;
      end ;

      buff.setPosition( size + 20 ) ;
      buff.getRawShort16 ;
      checkSentinel( buff, DWG_Section.CLASSES, true ) ;
      Result := buff.isGood;
    finally
      FreeObject( buff ) ;
    end ;
  end ;

  function DwgReader21.readDwgEntities : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, dataSize, decoder ) ;
    try
      Result := inherited readDwgEntitiesEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader21.readDwgHandles : Boolean;
  var
    si             : DwgSectionInfo;
    tmpHandlesData : TBytes;
    dataBuf        : DwgBuffer;
  begin
    si := mapSections[ DWG_Section.HANDLES ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;

    SetLength( tmpHandlesData, si.size ) ;
    Result := parseDataPage( si, tmpHandlesData ) ;
    if not Result then
      exit;

    dataBuf := DwgBuffer.Create( tmpHandlesData, si.size, decoder ) ;
    try
      Result := inherited readDwgHandlesEx( dataBuf, 0, si.size ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader21.readDwgHeader( const _hdr : DwgHeader ) : Boolean;
  var
    si                 : DwgSectionInfo;
    tmpHeaderData      : TBytes;
    dataBuf, handleBuf : DwgBuffer;
  begin
    si := mapSections[ DWG_Section.HEADER ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;
    SetLength( tmpHeaderData, si.size ) ;

    Result := parseDataPage( si, tmpHeaderData ) ;
    if not Result then
      exit;

    dataBuf := DwgBuffer.Create( tmpHeaderData, si.size, decoder ) ;
    try
      handleBuf := DwgBuffer.Create( tmpHeaderData, si.size, decoder ) ;
      try
        checkSentinel( dataBuf, DWG_Section.HEADER, true ) ;
        Result := inherited readDwgHeaderEx( _hdr, dataBuf, handleBuf ) ;
      finally
        FreeObject( handleBuf ) ;
      end ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader21.readDwgObjects : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, dataSize, decoder ) ;
    try
      Result := inherited readDwgObjectsEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader21.readDwgTables( const _hdr : DwgHeader ) : Boolean;
  var
    si      : DwgSectionInfo;
    dataBuf : DwgBuffer;
  begin
    si := mapSections[ DWG_Section.OBJECTS ];
    if ( si.Id < 0 ) then begin
      Result := false;
      exit;
    end ;

    dataSize := si.size;
    SetLength( objData, dataSize ) ;
    Result := parseDataPage( si, objData ) ;
    if not Result then
      exit;

    dataBuf := DwgBuffer.Create( objData, dataSize, decoder ) ;
    try
      Result := inherited readDwgTablesEx( _hdr, dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader21.readFileHeader : Boolean;
  var
    fileHdrRaw                  : TBytes;
    fileHdrdRS                  : TBytes;
    fileHdrBuf                  : DwgBuffer;
    fileHdrDataBuf              : DwgBuffer;
    fileHdrCompLength           : dint32;
    fileHdrCompLength2          : dint32;
    fileHdrDataLength           : Integer;
    fileHdrData                 : TBytes;
    compByteStr                 : TBytes;
    PagesMapCorrectionFactor    : duint64;
    PagesMapOffset              : duint64;
    PagesMapSizeCompressed      : duint64;
    PagesMapSizeUncompressed    : duint64;
    PagesMaxId                  : duint64;
    SectionsMapSizeCompressed   : duint64;
    SectionsMapId               : duint64;
    SectionsMapSizeUncompressed : duint64;
    SectionsMapCorrectionFactor : duint64;
    PagesMapData                : TBytes;
    address                     : duint64;
    i                           : duint64;
    ii                          : Integer;
    PagesMapBuf                 : DwgBuffer;
    sectionPageMapTmp           : TDictionary< duint32, DwgPageInfo >;
    size                        : duint64;
    Id                          : dint64;
    ind                         : duint64;
    SectionsMapData             : TBytes;
    sectionMap                  : DwgPageInfo;
    SectionsMapBuf              : DwgBuffer;
    nextId                      : duint8;
    secInfo                     : DwgSectionInfo;
    SectionNameLength           : duint64;
    po                          : duint64;
    ds                          : duint32;
    pn                          : duint32;
    pi                          : DwgPageInfo;
    {$IFDEF DCC}
    tpi                         : TPair< duint32, DwgPageInfo > ;
    {$ENDIF}
  begin
    if not fileBuf.setPosition( $80 ) then begin
      Result := false;
      exit;
    end ;
    SetLength( fileHdrRaw, $2FD ) ;
    fileBuf.getBytes( fileHdrRaw, $2FD ) ;
    SetLength( fileHdrdRS, $2CD ) ;
    DwgRSCodec.decode239I( fileHdrRaw, fileHdrdRS, 3 ) ;

    fileHdrBuf := DwgBuffer.Create( fileHdrdRS, $2CD, decoder ) ;
    try
      fileHdrBuf.getRawLong64 ;
      fileHdrBuf.getRawLong64 ;
      fileHdrBuf.getRawLong64 ;
      fileHdrCompLength := fileHdrBuf.getRawLong32;
      fileHdrCompLength2 := fileHdrBuf.getRawLong32;

      fileHdrDataLength := $110;

      if ( fileHdrCompLength < 0 ) then begin
        fileHdrDataLength := fileHdrCompLength * - 1;
        SetLength( fileHdrData, fileHdrDataLength ) ;
        fileHdrBuf.getBytes( fileHdrData, fileHdrDataLength ) ;
      end
      else begin
        SetLength( compByteStr, fileHdrCompLength ) ;
        fileHdrBuf.getBytes( compByteStr, fileHdrCompLength ) ;
        SetLength( fileHdrData, fileHdrDataLength ) ;
        DwgCompressor.decompress21( compByteStr, fileHdrData, fileHdrCompLength,
                                    fileHdrDataLength ) ;
      end ;
    finally
      FreeObject( fileHdrBuf ) ;
    end ;

    fileHdrDataBuf := DwgBuffer.Create( fileHdrData, fileHdrDataLength,
                                        decoder ) ;
    try
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      PagesMapCorrectionFactor := fileHdrDataBuf.getRawLong64;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      PagesMapOffset := fileHdrDataBuf.getRawLong64;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      PagesMapSizeCompressed := fileHdrDataBuf.getRawLong64;
      PagesMapSizeUncompressed := fileHdrDataBuf.getRawLong64;
      fileHdrDataBuf.getRawLong64 ;
      PagesMaxId := fileHdrDataBuf.getRawLong64;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      SectionsMapSizeCompressed := fileHdrDataBuf.getRawLong64;
      fileHdrDataBuf.getRawLong64 ;
      SectionsMapId := fileHdrDataBuf.getRawLong64;
      SectionsMapSizeUncompressed := fileHdrDataBuf.getRawLong64;
      fileHdrDataBuf.getRawLong64 ;
      SectionsMapCorrectionFactor := fileHdrDataBuf.getRawLong64;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
      fileHdrDataBuf.getRawLong64 ;
    finally
      FreeObject( fileHdrDataBuf ) ;
    end ;
    SetLength( PagesMapData, PagesMapSizeUncompressed ) ;

    Result := parseSysPage( PagesMapSizeCompressed, PagesMapSizeUncompressed,
      PagesMapCorrectionFactor, $480 + PagesMapOffset, PagesMapData ) ;
    if not Result then
      exit;

    address := $480;
    i := 0;

    sectionPageMapTmp := TDictionary<duint32, DwgPageInfo>.Create;
    try
      PagesMapBuf := DwgBuffer.Create( PagesMapData, PagesMapSizeUncompressed,
                                       decoder ) ;
      try
        while ( PagesMapSizeUncompressed > i ) do begin
          size := PagesMapBuf.getRawLong64;
          Id := dint64(PagesMapBuf.getRawLong64);
          if Id > 0 then
            ind := Id
          else
            ind := - Id;
          i := i + 16;
          sectionPageMapTmp.Add( ind, DwgPageInfo.Create( ind, address, size ) ) ;
          address := address + size;
        end ;
      finally
        FreeObject( PagesMapBuf ) ;
      end ;

      SetLength( SectionsMapData, SectionsMapSizeUncompressed ) ;
      sectionMap := sectionPageMapTmp[ duint32(SectionsMapId) ];
      Result := parseSysPage( SectionsMapSizeCompressed,
                              SectionsMapSizeUncompressed,
                              SectionsMapCorrectionFactor,
                              sectionMap.address, SectionsMapData
                             ) ;
      if not Result then
        exit;

      SectionsMapBuf := DwgBuffer.Create( SectionsMapData, SectionsMapSizeUncompressed,
                                          decoder ) ;
      try
        nextId := 1;
        while ( SectionsMapBuf.getPosition < SectionsMapBuf.size ) do begin
          secInfo := DwgSectionInfo.Create;
          secInfo.size := SectionsMapBuf.getRawLong64;
          secInfo.maxSize := SectionsMapBuf.getRawLong64;
          secInfo.encrypted := SectionsMapBuf.getRawLong64;
          SectionsMapBuf.getRawLong64 ;
          SectionNameLength := SectionsMapBuf.getRawLong64;
          SectionsMapBuf.getRawLong64 ;
          secInfo.compresed := SectionsMapBuf.getRawLong64;
          secInfo.pageCount := SectionsMapBuf.getRawLong64;
          secInfo.name := SectionsMapBuf.getUCSStr( SectionNameLength ) ;

          if secInfo.pageCount > 0 then
            for ii := 0 to secInfo.pageCount - 1 do begin
              po := SectionsMapBuf.getRawLong64;
              ds := SectionsMapBuf.getRawLong64;
              pn := SectionsMapBuf.getRawLong64;
              pi := sectionPageMapTmp[ pn ];
              pi.dataSize := ds;
              pi.startOffset := po;
              pi.uSize := SectionsMapBuf.getRawLong64;
              pi.cSize := SectionsMapBuf.getRawLong64;
              secInfo.pages.Add( pn, pi ) ;
              sectionPageMapTmp.Remove( pn ) ;
              SectionsMapBuf.getRawLong64 ;
              SectionsMapBuf.getRawLong64 ;
            end ;

          if length( secInfo.name ) > 0 then begin
            secInfo.Id := nextId;
            nextId := nextId + 1;
            mapSections.Add( getEnum( secInfo.name ), secInfo ) ;
          end
          else begin
            FreeObject( secInfo ) ;
          end ;
        end ;
      finally
        FreeObject( SectionsMapBuf ) ;
      end ;
    finally
      for tpi in sectionPageMapTmp do
        FreeObjectNotNil( tpi.Value ) ;

      FreeObject( sectionPageMapTmp ) ;
    end ;

    if not fileBuf.isGood then
      Result := false
    else
      Result := true;
  end ;

  function DwgReader21.readMetaData : Boolean;
  var
    cp      : duint16;
    secType : duint32;
  begin
    if not fileBuf.setPosition( 11 ) then begin
      Result := false;
      exit;
    end ;
    maintenanceVersion := fileBuf.getRawChar8;
    fileBuf.getRawChar8 ;
    previewImagePos := fileBuf.getRawLong32;
    fileBuf.getRawChar8 ;
    fileBuf.getRawChar8 ;
    cp := fileBuf.getRawShort16;
    if not setupDecoder( cp ) then begin
      {$IFDEF JAVA}
        decoder := TEncoding.UTF16LE;
      {$ELSE}
        decoder := TEncoding.Unicode;
      {$ENDIF}
    end ;
    fileBuf.getRawShort16 ;
    fileBuf.getRawChar8 ;
    secType := fileBuf.getRawLong32;
    fileBuf.getRawLong32 ;
    fileBuf.getRawLong32 ;
    fileBuf.getRawLong32 ;
    fileBuf.getRawLong32 ;
    fileBuf.getRawLong32 ;
    fileBuf.getRawLong32 ;
    Result := true;
  end ;

// ----------------------------------------------------------------------------
// DwgReader24
// ----------------------------------------------------------------------------

  function DwgReader24.readDwgBlocks : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
    try
      Result := inherited readDwgBlocksEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader24.readDwgClasses : Boolean;
  begin
    Result := inherited readDwgClasses;
  end ;

  function DwgReader24.readDwgEntities : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
    try
      Result := inherited readDwgEntitiesEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader24.readDwgHeader( const _hdr : DwgHeader ) : Boolean;
  begin
    Result := inherited readDwgHeader( _hdr ) ;
  end ;

  function DwgReader24.readDwgObjects : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
    try
      Result := inherited readDwgObjectsEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader24.readFileHeader : Boolean;
  begin
    Result := inherited readFileHeader;
  end ;

// ----------------------------------------------------------------------------
// DwgReader27
// ----------------------------------------------------------------------------

  function DwgReader27.readDwgBlocks : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
    try
      Result := inherited readDwgBlocksEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader27.readDwgClasses : Boolean;
  begin
    Result := inherited readDwgClasses;
  end ;

  function DwgReader27.readDwgEntities : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
    try
      Result := inherited readDwgEntitiesEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader27.readDwgHeader( const _hdr : DwgHeader ) : Boolean;
  begin
    Result := inherited readDwgHeader( _hdr ) ;
  end ;

  function DwgReader27.readDwgObjects : Boolean;
  var
    dataBuf : DwgBuffer;
  begin
    dataBuf := DwgBuffer.Create( objData, uncompSize, decoder ) ;
    try
      Result := inherited readDwgObjectsEx( dataBuf ) ;
    finally
      FreeObject( dataBuf ) ;
    end ;
  end ;

  function DwgReader27.readFileHeader : Boolean;
  begin
    Result := inherited readFileHeader;
  end ;

  function DwgReader32.readMetaData : Boolean;
  var
    cp          : duint16;
    uk          : duint32;
    sumInfoAddr : duint32;
    vbaAdd      : duint32;
  begin
    if not fileBuf.setPosition( 11 ) then begin
      Result := false;
      exit;
    end ;
    // here is 0 but we force to read 2010+ & MV> 3, higth 32b:
    maintenanceVersion := fileBuf.getRawChar8 + 4;
    fileBuf.getRawChar8 ;
    previewImagePos := fileBuf.getRawLong32;
    fileBuf.getRawChar8 ;
    fileBuf.getRawChar8 ;
    cp := fileBuf.getRawShort16;
    setupDecoder( cp ) ;
    fileBuf.getRawChar8 ;
    fileBuf.getRawChar8 ;
    fileBuf.getRawChar8 ;
    securityFlags := fileBuf.getRawLong32;

    uk := fileBuf.getRawLong32;
    sumInfoAddr := fileBuf.getRawLong32;
    vbaAdd := fileBuf.getRawLong32;
    fileBuf.getRawLong32 ;
    Result := true;
  end ;

// ----------------------------------------------------------------------------
// DwgHeader
// ----------------------------------------------------------------------------

  constructor DwgHeader.Create;
  begin
    inherited;

    vars := TDictionary<String, Variant>.Create;
  end ;

  {$IFNDEF OXYGENE}
  destructor DwgHeader.Destroy ;
  begin
    FreeObject( vars ) ;

    inherited ;
  end ;
  {$ENDIF}

  function DwgHeader.parseDwg(
    const _version     : DWG_Version;
    const _buf, _hBbuf : DwgBuffer;
    const _mv          : dint16
  ) : Boolean;
  var
    size, bitSize, endBitPos, hSize : duint32;
    requiredVersions                : duint64;
    hcv                             : DwgHandle;
    msec, day                       : ddouble64;
    handseed, clayer, textstyle, celtype, dimstyle, cmlstyle, pucsname,
    pucsorthoref, pucsbase, ucsorthoref, ucsbase, cmaterial, ucsname,
    dimtxsty, dimldrblk, dimblk, dimltype, dimblk1, dimblk2, dimltex1,
    dimltex2, control   : DwgHandle;
    cepsntype           : duint16;
    strStartPos         : duint32;
    strDataSize         : duint32;
    hiSize              : duint32;
    sz                  : duint64;
    i                   : Integer;
    {$IFDEF DCC}
    it                  : TPair< String, Variant >;
    {$ENDIF}
    {$IFDEF GIS_DEBUG}
      vt : Variant ;
      color : DwgColor ;
    {$ENDIF}
  begin
    Result := true;
    size := _buf.getRawLong32;
    bitSize := 0;
    endBitPos := 160;
    if dwgVersionHigher( _version, DWG_Version.AC1021 ) and ( _mv > 3 ) then begin
      hSize := _buf.getRawLong32;
      endBitPos := endBitPos + 32;
    end ;

    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      bitSize := _buf.getRawLong32;
      endBitPos := endBitPos + bitSize;
      _hBbuf.setPosition( endBitPos shr 3 ) ;
      _hBbuf.setBitPos( endBitPos and 7 ) ;
    end ;

    if dwgVersionHigher( _version, DWG_Version.AC1024 ) then begin
      requiredVersions := _buf.getBitLongLong;
    end ;
    _buf.getBitDouble ;
    _buf.getBitDouble ;
    _buf.getBitDouble ;
    _buf.getBitDouble ;
    if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
      _buf.getCP8Text ;
      _buf.getCP8Text ;
      _buf.getCP8Text ;
      _buf.getCP8Text ;
    end ;
    _buf.getBitLong ;
    _buf.getBitLong ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      _buf.getBitShort ;
    end ;
    if dwgVersionLower( _version, DWG_Version.AC1018 ) then begin
      hcv := _hBbuf.getHandle;
    end ;
    vars.Add( 'DIMASO', _buf.getBit ) ;
    vars.Add( 'DIMSHO', _buf.getBit ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'DIMSAV', _buf.getBit ) ;
    end ;
    vars.Add( 'PLINEGEN', _buf.getBit ) ;
    vars.Add( 'ORTHOMODE', _buf.getBit ) ;
    vars.Add( 'REGENMODE', _buf.getBit ) ;
    vars.Add( 'FILLMODE', _buf.getBit ) ;
    vars.Add( 'QTEXTMODE', _buf.getBit ) ;
    vars.Add( 'PSLTSCALE', _buf.getBit ) ;
    vars.Add( 'LIMCHECK', _buf.getBit ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'BLIPMODE', _buf.getBit ) ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      _buf.getBit ;
    end ;
    vars.Add( 'USRTIMER', _buf.getBit ) ;
    vars.Add( 'SKPOLY', _buf.getBit ) ;
    vars.Add( 'ANGDIR', _buf.getBit ) ;
    vars.Add( 'SPLFRAME', _buf.getBit ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'ATTREQ', _buf.getBit ) ;
      vars.Add( 'ATTDIA', _buf.getBit ) ;
    end ;
    vars.Add( 'MIRRTEXT', _buf.getBit ) ;
    vars.Add( 'WORLDVIEW', _buf.getBit ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'WIREFRAME', _buf.getBit ) ;
    end ;
    vars.Add( 'TILEMODE', _buf.getBit ) ;
    vars.Add( 'PLIMCHECK', _buf.getBit ) ;
    vars.Add( 'VISRETAIN', _buf.getBit ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'DELOBJ', _buf.getBit ) ;
    end ;
    vars.Add( 'DISPSILH', _buf.getBit ) ;
    vars.Add( 'PELLIPSE', _buf.getBit ) ;
    vars.Add( 'PROXIGRAPHICS', _buf.getBitShort ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'DRAGMODE', _buf.getBitShort ) ;
    end ;
    vars.Add( 'TREEDEPTH', _buf.getBitShort ) ;
    vars.Add( 'LUNITS', _buf.getBitShort ) ;
    vars.Add( 'LUPREC', _buf.getBitShort ) ;
    vars.Add( 'AUNITS', _buf.getBitShort ) ;
    vars.Add( 'AUPREC', _buf.getBitShort ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'OSMODE', _buf.getBitShort ) ;
    end ;
    vars.Add( 'ATTMODE', _buf.getBitShort ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'COORDS', _buf.getBitShort ) ;
    end ;
    vars.Add( 'PDMODE', _buf.getBitShort ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'PICKSTYLE', _buf.getBitShort ) ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      _buf.getBitLong ;
      _buf.getBitLong ;
      _buf.getBitLong ;
    end ;
    vars.Add( 'USERI1', _buf.getBitShort ) ;
    vars.Add( 'USERI2', _buf.getBitShort ) ;
    vars.Add( 'USERI3', _buf.getBitShort ) ;
    vars.Add( 'USERI4', _buf.getBitShort ) ;
    vars.Add( 'USERI5', _buf.getBitShort ) ;
    vars.Add( 'SPLINESEGS', _buf.getBitShort ) ;
    vars.Add( 'SURFU', _buf.getBitShort ) ;
    vars.Add( 'SURFV', _buf.getBitShort ) ;
    vars.Add( 'SURFTYPE', _buf.getBitShort ) ;
    vars.Add( 'SURFTAB1', _buf.getBitShort ) ;
    vars.Add( 'SURFTAB2', _buf.getBitShort ) ;
    vars.Add( 'SPLINETYPE', _buf.getBitShort ) ;
    vars.Add( 'SHADEDGE', _buf.getBitShort ) ;
    vars.Add( 'SHADEDIF', _buf.getBitShort ) ;
    vars.Add( 'UNITMODE', _buf.getBitShort ) ;
    vars.Add( 'MAXACTVP', _buf.getBitShort ) ;
    vars.Add( 'ISOLINES', _buf.getBitShort ) ;
    vars.Add( 'CMLJUST', _buf.getBitShort ) ;
    vars.Add( 'TEXTQLTY', _buf.getBitShort ) ;
    vars.Add( 'LTSCALE', _buf.getBitDouble ) ;
    vars.Add( 'TEXTSIZE', _buf.getBitDouble ) ;
    vars.Add( 'TRACEWID', _buf.getBitDouble ) ;
    vars.Add( 'SKETCHINC', _buf.getBitDouble ) ;
    vars.Add( 'FILLETRAD', _buf.getBitDouble ) ;
    vars.Add( 'THICKNESS', _buf.getBitDouble ) ;
    vars.Add( 'ANGBASE', _buf.getBitDouble ) ;
    vars.Add( 'PDSIZE', _buf.getBitDouble ) ;
    vars.Add( 'PLINEWID', _buf.getBitDouble ) ;
    vars.Add( 'USERR1', _buf.getBitDouble ) ;
    vars.Add( 'USERR2', _buf.getBitDouble ) ;
    vars.Add( 'USERR3', _buf.getBitDouble ) ;
    vars.Add( 'USERR4', _buf.getBitDouble ) ;
    vars.Add( 'USERR5', _buf.getBitDouble ) ;
    vars.Add( 'CHAMFERA', _buf.getBitDouble ) ;
    vars.Add( 'CHAMFERB', _buf.getBitDouble ) ;
    vars.Add( 'CHAMFERC', _buf.getBitDouble ) ;
    vars.Add( 'CHAMFERD', _buf.getBitDouble ) ;
    vars.Add( 'FACETRES', _buf.getBitDouble ) ;
    vars.Add( 'CMLSCALE', _buf.getBitDouble ) ;
    vars.Add( 'CELTSCALE', _buf.getBitDouble ) ;
    if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
      vars.Add( 'MENU', _buf.getCP8Text ) ;
    end ;
    day := _buf.getBitLong;
    msec := _buf.getBitLong;
    while ( msec > 0 ) do
      msec := msec / 10;
    vars.Add( 'TDCREATE', day + msec ) ;
    day := _buf.getBitLong;
    msec := _buf.getBitLong;
    while ( msec > 0 ) do
      msec := msec / 10;
    vars.Add( 'TDUPDATE', day + msec ) ;
    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      _buf.getBitLong ;
      _buf.getBitLong ;
      _buf.getBitLong ;
    end ;
    day := _buf.getBitLong;
    msec := _buf.getBitLong;
    while ( msec > 0 ) do
      msec := msec / 10;
    vars.Add( 'TDINDWG', day + msec ) ;
    day := _buf.getBitLong;
    msec := _buf.getBitLong;
    while ( msec > 0 ) do
      msec := msec / 10;
    vars.Add( 'TDUSRTIMER', day + msec ) ;
    vars.Add( 'CECOLOR', _buf.getCmColor( _version ).ToArray ) ;

    handseed := _buf.getHandle;
    clayer := _hBbuf.getHandle;
    textstyle := _hBbuf.getHandle;
    celtype := _hBbuf.getHandle;
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      cmaterial := _hBbuf.getHandle;
    end ;
    dimstyle := _hBbuf.getHandle;
    cmlstyle := _hBbuf.getHandle;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      vars.Add( 'PSVPSCALE', _buf.getBitDouble ) ;
    end ;
    vars.Add( 'PINSBASE', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'PEXTMIN', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'PEXTMAX', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'PLIMMIN', _buf.get2RawDouble.ToArray ) ;
    vars.Add( 'PLIMMAX', _buf.get2RawDouble.ToArray ) ;
    vars.Add( 'PELEVATION', _buf.getBitDouble ) ;
    vars.Add( 'PUCSORG', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'PUCSXDIR', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'PUCSYDIR', _buf.get3BitDouble.ToArray ) ;
    pucsname := _hBbuf.getHandle;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      pucsorthoref := _hBbuf.getHandle;
      vars.Add( 'PUCSORTHOVIEW', _buf.getBitShort ) ;
      pucsbase := _hBbuf.getHandle;
      vars.Add( 'PUCSORGTOP', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'PUCSORGBOTTOM', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'PUCSORGLEFT', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'PUCSORGRIGHT', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'PUCSORGFRONT', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'PUCSORGBACK', _buf.get3BitDouble.ToArray ) ;
    end ;
    vars.Add( 'INSBASE', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'EXTMIN', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'EXTMAX', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'LIMMIN', _buf.get2RawDouble.ToArray ) ;
    vars.Add( 'LIMMAX', _buf.get2RawDouble.ToArray ) ;
    vars.Add( 'ELEVATION', _buf.getBitDouble ) ;
    vars.Add( 'UCSORG', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'UCSXDIR', _buf.get3BitDouble.ToArray ) ;
    vars.Add( 'UCSYDIR', _buf.get3BitDouble.ToArray ) ;
    ucsname := _hBbuf.getHandle;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      ucsorthoref := _hBbuf.getHandle;
      vars.Add( 'UCSORTHOVIEW', _buf.getBitShort ) ;
      ucsbase := _hBbuf.getHandle;
      vars.Add( 'UCSORGTOP', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'UCSORGBOTTOM', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'UCSORGLEFT', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'UCSORGRIGHT', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'UCSORGFRONT', _buf.get3BitDouble.ToArray ) ;
      vars.Add( 'UCSORGBACK', _buf.get3BitDouble.ToArray ) ;
      if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
        vars.Add( 'DIMPOST', _buf.getCP8Text ) ;
        vars.Add( 'DIMAPOST', _buf.getCP8Text ) ;
      end ;
    end ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'DIMTOL', _buf.getBit ) ;
      vars.Add( 'DIMLIM', _buf.getBit ) ;
      vars.Add( 'DIMTIH', _buf.getBit ) ;
      vars.Add( 'DIMTOH', _buf.getBit ) ;
      vars.Add( 'DIMSE1', _buf.getBit ) ;
      vars.Add( 'DIMSE2', _buf.getBit ) ;
      vars.Add( 'DIMALT', _buf.getBit ) ;
      vars.Add( 'DIMTOFL', _buf.getBit ) ;
      vars.Add( 'DIMSAH', _buf.getBit ) ;
      vars.Add( 'DIMTIX', _buf.getBit ) ;
      vars.Add( 'DIMSOXD', _buf.getBit ) ;
      vars.Add( 'DIMALTD', _buf.getRawChar8 ) ;
      vars.Add( 'DIMZIN', _buf.getRawChar8 ) ;
      vars.Add( 'DIMSD1', _buf.getBit ) ;
      vars.Add( 'DIMSD2', _buf.getBit ) ;
      vars.Add( 'DIMTOLJ', _buf.getRawChar8 ) ;
      vars.Add( 'DIMJUST', _buf.getRawChar8 ) ;
      vars.Add( 'DIMFIT', _buf.getRawChar8 ) ;
      vars.Add( 'DIMUPT', _buf.getBit ) ;
      vars.Add( 'DIMTZIN', _buf.getRawChar8 ) ;
      vars.Add( 'DIMALTZ', _buf.getRawChar8 ) ;
      vars.Add( 'DIMALTTZ', _buf.getRawChar8 ) ;
      vars.Add( 'DIMTAD', _buf.getRawChar8 ) ;
      vars.Add( 'DIMUNIT', _buf.getBitShort ) ;
      vars.Add( 'DIMAUNIT', _buf.getBitShort ) ;
      vars.Add( 'DIMDEC', _buf.getBitShort ) ;
      vars.Add( 'DIMTDEC', _buf.getBitShort ) ;
      vars.Add( 'DIMALTU', _buf.getBitShort ) ;
      vars.Add( 'DIMALTTD', _buf.getBitShort ) ;
      dimtxsty := _hBbuf.getHandle;
    end ;
    vars.Add( 'DIMSCALE', _buf.getBitDouble ) ;
    vars.Add( 'DIMASZ', _buf.getBitDouble ) ;
    vars.Add( 'DIMEXO', _buf.getBitDouble ) ;
    vars.Add( 'DIMDLI', _buf.getBitDouble ) ;
    vars.Add( 'DIMEXE', _buf.getBitDouble ) ;
    vars.Add( 'DIMRND', _buf.getBitDouble ) ;
    vars.Add( 'DIMDLE', _buf.getBitDouble ) ;
    vars.Add( 'DIMTP', _buf.getBitDouble ) ;
    vars.Add( 'DIMTM', _buf.getBitDouble ) ;
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      vars.Add( 'DIMFXL', _buf.getBitDouble ) ;
      vars.Add( 'DIMJOGANG', _buf.getBitDouble ) ;
      vars.Add( 'DIMTFILL', _buf.getBitShort ) ;
      vars.Add( 'DIMTFILLCLR', _buf.getCmColor( _version ).ToArray ) ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      vars.Add( 'DIMTOL', _buf.getBit ) ;
      vars.Add( 'DIMLIM', _buf.getBit ) ;
      vars.Add( 'DIMTIH', _buf.getBit ) ;
      vars.Add( 'DIMTOH', _buf.getBit ) ;
      vars.Add( 'DIMSE1', _buf.getBit ) ;
      vars.Add( 'DIMSE2', _buf.getBit ) ;
      vars.Add( 'DIMTAD', _buf.getBitShort ) ;
      vars.Add( 'DIMZIN', _buf.getBitShort ) ;
      vars.Add( 'DIMAZIN', _buf.getBitShort ) ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      vars.Add( 'DIMARCSYM', _buf.getBitShort ) ;
    end ;
    vars.Add( 'DIMTXT', _buf.getBitDouble ) ;
    vars.Add( 'DIMCEN', _buf.getBitDouble ) ;
    vars.Add( 'DIMTSZ', _buf.getBitDouble ) ;
    vars.Add( 'DIMALTF', _buf.getBitDouble ) ;
    vars.Add( 'DIMLFAC', _buf.getBitDouble ) ;
    vars.Add( 'DIMTVP', _buf.getBitDouble ) ;
    vars.Add( 'DIMTFAC', _buf.getBitDouble ) ;
    vars.Add( 'DIMGAP', _buf.getBitDouble ) ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'DIMPOST', _buf.getCP8Text ) ;
      vars.Add( 'DIMAPOST', _buf.getCP8Text ) ;
      vars.Add( 'dimblk', _buf.getCP8Text ) ;
      vars.Add( 'dimblk1', _buf.getCP8Text ) ;
      vars.Add( 'dimblk2', _buf.getCP8Text ) ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      vars.Add( 'DIMALTRND', _buf.getBitDouble ) ;
      vars.Add( 'DIMALT', _buf.getBit ) ;
      vars.Add( 'DIMALTD', _buf.getBitShort ) ;
      vars.Add( 'DIMTOFL', _buf.getBit ) ;
      vars.Add( 'DIMSAH', _buf.getBit ) ;
      vars.Add( 'DIMTIX', _buf.getBit ) ;
      vars.Add( 'DIMSOXD', _buf.getBit ) ;
    end ;
    vars.Add( 'DIMCLRD', _buf.getCmColor( _version ).ToArray ) ;
    vars.Add( 'DIMCLRE', _buf.getCmColor( _version ).ToArray ) ;
    vars.Add( 'DIMCLRT', _buf.getCmColor( _version ).ToArray ) ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      vars.Add( 'DIAMDEC', _buf.getBitShort ) ;
      vars.Add( 'DIMDEC', _buf.getBitShort ) ;
      vars.Add( 'DIMTDEC', _buf.getBitShort ) ;
      vars.Add( 'DIMALTU', _buf.getBitShort ) ;
      vars.Add( 'DIMALTTD', _buf.getBitShort ) ;
      vars.Add( 'DIMAUNIT', _buf.getBitShort ) ;
      vars.Add( 'DIMFAC', _buf.getBitShort ) ;

      vars.Add( 'DIMLUNIT', _buf.getBitShort ) ;
      vars.Add( 'DIMDSEP', _buf.getBitShort ) ;
      vars.Add( 'DIMTMOVE', _buf.getBitShort ) ;
      vars.Add( 'DIMJUST', _buf.getBitShort ) ;
      vars.Add( 'DIMSD1', _buf.getBit ) ;
      vars.Add( 'DIMSD2', _buf.getBit ) ;
      vars.Add( 'DIMTOLJ', _buf.getBitShort ) ;
      vars.Add( 'DIMTZIN', _buf.getBitShort ) ;
      vars.Add( 'DIMALTZ', _buf.getBitShort ) ;
      vars.Add( 'DIMALTTZ', _buf.getBitShort ) ;
      vars.Add( 'DIMUPT', _buf.getBit ) ;
      vars.Add( 'DIMATFIT', _buf.getBitShort ) ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      vars.Add( 'DIMFXLON', _buf.getBit ) ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
      vars.Add( 'DIMTXTDIRECTION', _buf.getBit ) ;
      vars.Add( 'DIMALTMZF', _buf.getBitDouble ) ;
      vars.Add( 'DIMMZF', _buf.getBitDouble ) ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      dimtxsty := _hBbuf.getHandle;
      dimldrblk := _hBbuf.getHandle;
      dimblk := _hBbuf.getHandle;
      dimblk1 := _hBbuf.getHandle;
      dimblk2 := _hBbuf.getHandle;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      dimltype := _hBbuf.getHandle;
      dimltex1 := _hBbuf.getHandle;
      dimltex2 := _hBbuf.getHandle;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      vars.Add( 'DIMLWD', _buf.getBitShort ) ;
      vars.Add( 'DIMLWE', _buf.getBitShort ) ;
    end ;
    control := _hBbuf.getHandle;
    blockCtrl := control.ref;
    control := _hBbuf.getHandle;
    layerCtrl := control.ref;
    control := _hBbuf.getHandle;
    styleCtrl := control.ref;
    control := _hBbuf.getHandle;
    linetypeCtrl := control.ref;
    control := _hBbuf.getHandle;
    viewCtrl := control.ref;
    control := _hBbuf.getHandle;
    ucsCtrl := control.ref;
    control := _hBbuf.getHandle;
    vportCtrl := control.ref;
    control := _hBbuf.getHandle;
    appidCtrl := control.ref;
    control := _hBbuf.getHandle;
    dimstyleCtrl := control.ref;
    if dwgVersionLower( _version, DWG_Version.AC1018 ) then begin
      control := _hBbuf.getHandle;
      vpEntHeaderCtrl := control.ref;
    end ;
    control := _hBbuf.getHandle;
    control := _hBbuf.getHandle;
    control := _hBbuf.getHandle;

    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      vars.Add( 'TSTACKALIGN', _buf.getBitShort ) ;
      vars.Add( 'TSTACKSIZE', _buf.getBitShort ) ;
      if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
        vars.Add( 'HYPERLINKBASE', _buf.getCP8Text ) ;
        vars.Add( 'STYLESHEET', _buf.getCP8Text ) ;
      end ;
      control := _hBbuf.getHandle;
      control := _hBbuf.getHandle;
      control := _hBbuf.getHandle;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      control := _hBbuf.getHandle;
      control := _hBbuf.getHandle;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      control := _hBbuf.getHandle;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1024 ) then begin
      control := _hBbuf.getHandle;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      _buf.getBitLong ;
      vars.Add( 'INSUNITS', _buf.getBitShort ) ;
      cepsntype := _buf.getBitShort;
      vars.Add( 'CEPSNTYPE', cepsntype ) ;
      if ( cepsntype = 3 ) then begin
        control := _hBbuf.getHandle;
      end ;
      if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
        vars.Add( 'FINGERPRINTGUID', _buf.getCP8Text ) ;
        vars.Add( 'VERSIONGUID', _buf.getCP8Text ) ;
      end ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      vars.Add( 'SORTENTS', _buf.getRawChar8 ) ;
      vars.Add( 'INDEXCTL', _buf.getRawChar8 ) ;
      vars.Add( 'HIDETEXT', _buf.getRawChar8 ) ;
      vars.Add( 'XCLIPFRAME', _buf.getRawChar8 ) ;
      vars.Add( 'DIMASSOC', _buf.getRawChar8 ) ;
      vars.Add( 'HALOGAP', _buf.getRawChar8 ) ;
      vars.Add( 'OBSCUREDCOLOR', _buf.getBitShort ) ;
      vars.Add( 'INTERSECTIONCOLOR', _buf.getBitShort ) ;
      vars.Add( 'OBSCUREDLTYPE', _buf.getRawChar8 ) ;
      vars.Add( 'INTERSECTIONDISPLAY', _buf.getRawChar8 ) ;
      if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
        vars.Add( 'PROJECTNAME', _buf.getCP8Text ) ;
      end ;
    end ;
    control := _hBbuf.getHandle;
    control := _hBbuf.getHandle;
    modelSpaceBlkRec := control.ref ;
    control := _hBbuf.getHandle;
    control := _hBbuf.getHandle;
    control := _hBbuf.getHandle;
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      vars.Add( 'CAMERADISPLAY', _buf.getBit ) ;
      _buf.getBitLong ;
      _buf.getBitLong ;
      _buf.getBitDouble ;
      vars.Add( 'STEPSPERSEC', _buf.getBitDouble ) ;
      vars.Add( 'STEPSIZE', _buf.getBitDouble ) ;
      vars.Add( '3DDWFPREC', _buf.getBitDouble ) ;
      vars.Add( 'LENSLENGTH', _buf.getBitDouble ) ;
      vars.Add( 'CAMERAHEIGHT', _buf.getBitDouble ) ;
      vars.Add( 'SOLIDHIST', _buf.getRawChar8 ) ;
      vars.Add( 'SHOWHIST', _buf.getRawChar8 ) ;
      vars.Add( 'PSOLWIDTH', _buf.getBitDouble ) ;
      vars.Add( 'PSOLHEIGHT', _buf.getBitDouble ) ;
      vars.Add( 'LOFTANG1', _buf.getBitDouble ) ;
      vars.Add( 'LOFTANG2', _buf.getBitDouble ) ;
      vars.Add( 'LOFTMAG1', _buf.getBitDouble ) ;
      vars.Add( 'LOFTMAG2', _buf.getBitDouble ) ;
      vars.Add( 'LOFTPARAM', _buf.getBitShort ) ;
      vars.Add( 'LOFTNORMALS', _buf.getRawChar8 ) ;
      vars.Add( 'LATITUDE', _buf.getBitDouble ) ;
      vars.Add( 'LONGITUDE', _buf.getBitDouble ) ;
      vars.Add( 'NORTHDIRECTION', _buf.getBitDouble ) ;
      vars.Add( 'TIMEZONE', _buf.getBitLong ) ;
      vars.Add( 'LIGHTGLYPHDISPLAY', _buf.getRawChar8 ) ;
      vars.Add( 'TILEMODELIGHTSYNCH', _buf.getRawChar8 ) ;
      vars.Add( 'DWFFRAME', _buf.getRawChar8 ) ;
      vars.Add( 'DGNFRAME', _buf.getRawChar8 ) ;
      _buf.getBit ;
      vars.Add( 'INTERFERECOLOR', _buf.getCmColor( _version ).ToArray ) ;
      control := _hBbuf.getHandle;
      control := _hBbuf.getHandle;
      control := _hBbuf.getHandle;
      vars.Add( 'CSHADOW', _buf.getRawChar8 ) ;
      _buf.getBitDouble ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1012 ) then begin
      _buf.getBitShort ;
      _buf.getBitShort ;
      _buf.getBitShort ;
      _buf.getBitShort ;
    end ;

    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      strStartPos := endBitPos - 1;
      _buf.setPosition( strStartPos shr 3 ) ;
      _buf.setBitPos( strStartPos and 7 ) ;
      if ( _buf.getBit = 1 ) then begin
        strStartPos := strStartPos - 16;
        _buf.setPosition( strStartPos shr 3 ) ;
        _buf.setBitPos( strStartPos and 7 ) ;
        strDataSize := _buf.getRawShort16;
        if ( strDataSize and $8000 ) <> 0 then begin
          strStartPos := strStartPos - 16;
          strDataSize := strDataSize and $7FFF;
          _buf.setPosition( strStartPos shr 3 ) ;
          _buf.setBitPos( strStartPos and 7 ) ;
          hiSize := _buf.getRawShort16;
          strDataSize := strDataSize or ( hiSize shl 15 ) ;
        end ;
        strStartPos := strStartPos - strDataSize;
        _buf.setPosition( strStartPos shr 3 ) ;
        _buf.setBitPos( strStartPos and 7 ) ;
      end ;
      _buf.getUCSText( false ) ;
      _buf.getUCSText( false ) ;
      _buf.getUCSText( false ) ;
      _buf.getUCSText( false ) ;
      vars.Add( 'MENU', _buf.getUCSText( false ) ) ;
      vars.Add( 'DIMPOST', _buf.getUCSText( false ) ) ;
      vars.Add( 'DIMAPOST', _buf.getUCSText( false ) ) ;
      if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
        vars.Add( 'DIMALTMZS', _buf.getUCSText( false ) ) ;
        vars.Add( 'DIMMZS', _buf.getUCSText( false ) ) ;
      end ;
      vars.Add( 'HYPERLINKBASE', _buf.getUCSText( false ) ) ;
      vars.Add( 'STYLESHEET', _buf.getUCSText( false ) ) ;
      vars.Add( 'FINGERPRINTGUID', _buf.getUCSText( false ) ) ;
      vars.Add( 'VERSIONGUID', _buf.getUCSText( false ) ) ;
      vars.Add( 'PROJECTNAME', _buf.getUCSText( false ) ) ;
    end ;

    {$IFDEF GIS_DEBUG}
    for it in vars do begin
      dwg_log( '\n' ) ; dwg_log( it.Key ) ; dwg_log( ': ' ) ;
      vt := VarType( it.Value ) ;
      case VarType( it.Value ) of
        $2005 : begin
            dwg_log( 'x= ' ) ;
            dwg_log( Double( {$IFDEF OXYGENE}array of Double{$ENDIF}(it.Value)[0]).ToString ) ;
            dwg_log( ', y= ' ) ;
            dwg_log( Double( {$IFDEF OXYGENE}array of Double{$ENDIF}(it.Value)[1]).ToString ) ;
            dwg_log( ', z= ' ) ;
            dwg_log( Double( {$IFDEF OXYGENE}array of Double{$ENDIF}(it.Value)[2]).ToString ) ;
          end ;
        8195 : begin
            color := DwgColor.Create(
                      DwgColorType( {$IFDEF OXYGENE}array of Integer{$ENDIF}(it.Value)[0]),
                      Cardinal( {$IFDEF OXYGENE}array of Integer{$ENDIF}(it.Value)[1])
                   );
            dwg_log( 'Color ' ) ; dwg_log( color.toString ) ;
          end
        else
          dwg_log( VarToString( it.Value ) ) ;
      end ;
    end ;
    {$ENDIF}

    _buf.setPosition( size + 16 + 4 ) ;

    if dwgVersionHigher( _version, DWG_Version.AC1021 ) and ( _mv > 3 ) then begin
      _buf.getRawLong32;
    end ;
    _buf.getRawShort16 ;
    for i := 0 to 16 - 1 do begin
      _buf.getRawChar8 ;
    end ;

    if dwgVersionLower( _version, DWG_Version.AC1018 ) then begin
      sz := _buf.size - 16;
      _buf.setPosition( sz ) ;
      for i := 0 to 16 - 1 do begin
        _buf.getRawChar8 ;
      end ;
    end
    else if ( _version = DWG_Version.AC1018 ) then begin
      _buf.moveBitPos( - 128 ) ;
      for i := 0 to 16 - 1 do begin
        _buf.getRawChar8 ;
      end ;
    end
    else if ( _version = DWG_Version.AC1021 ) then begin
      sz := _buf.size - 16;
      _buf.setPosition( sz ) ;
      for i := 0 to 16 - 1 do begin
        _buf.getRawChar8 ;
      end ;
    end
    else if ( _version = DWG_Version.AC1024 ) then begin
      _buf.moveBitPos( - 128 ) ;
      for i := 0 to 16 - 1 do begin
        _buf.getRawChar8 ;
      end ;
    end
    else if ( _version = DWG_Version.AC1027 ) then begin
      _buf.moveBitPos( - 128 ) ;
      for i := 0 to 16 - 1 do begin
        _buf.getRawChar8 ;
      end ;
    end ;
  end ;

// ----------------------------------------------------------------------------
// DwgClass
// ----------------------------------------------------------------------------

  function DwgClass.parseDwg(
    const _version      : DWG_Version;
    const _buf, _strBuf : DwgBuffer
  ) : Boolean;
  var
    DwgVersion : duint32;
  begin
    classNum  := _buf.getBitShort;
    proxyFlag := _buf.getBitShort;
    appName   := _strBuf.getVariableText( _version, false ) ;
    className := _strBuf.getVariableText( _version, false ) ;
    recName   := _strBuf.getVariableText( _version, false ) ;

    wasaProxyFlag := _buf.getBit;
    entityFlag := _buf.getBitShort;
    if entityFlag = $1F2 then
      entityFlag := 1
    else
      entityFlag := 0;

    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      instanceCount := _buf.getBitLong;
      DwgVersion := _buf.getBitLong;
      _buf.getBitLong ;
      _buf.getBitLong ;
      _buf.getBitLong ;
    end ;

    if (recName = 'LWPOLYLINE') or (recName = 'LWPLINE') then
      DwgType := DWG_OBJ_ID_LWPOLYLINE
    else if (recName = 'HATCH') then
      DwgType := DWG_OBJ_ID_HATCH
    else if (recName = 'GROUP') then
      DwgType := DWG_OBJ_ID_GROUP
    else if (recName = 'LAYOUT') then
      DwgType := DWG_OBJ_ID_LAYOUT
    else if (recName = 'IMAGE') then
      DwgType := 101
    else if (recName = 'IMAGEDEF') then
      DwgType := 102
    else if (recName = 'DBCOLOR') then
      DwgType := DWG_OBJ_ID_DBCOLOR
    else
      DwgType := 0 ;

    Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgTableEntry
// ----------------------------------------------------------------------------

  constructor DwgTableEntry.Create;
  begin
    inherited;

    tableType     := DWG_TTYPE.UNKNOWNT;
    flag          := 0;
    numReactors   := 0;
    xDictFlag     := 0;
    parentHandle  := 0;
  end ;

  function DwgTableEntry.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  begin
    Result := false;
  end ;

  function DwgTableEntry.parseDwgEx(
    const _version      : DWG_Version;
    const _buf, _strBuf : DwgBuffer;
    const _bs           : duint32
  ) : Boolean;
  var
    ms            : duint32;
    strDataSize   : dint16;
    hiSize        : duint16;
    ho            : DwgHandle;
    ah            : DwgHandle;
    extDataSize   : duint16;
    tmpExtData    : TBytes;
    tmpExtDataBuf : DwgBuffer;
    pos, bpos     : dint32;
    dxfCode       : duint8;
    strLength     : duint8;
    cp            : duint16;
    i             : Integer;
    dxfChar       : duint8;
    bd            : duint8;
  begin
    objSize := 0;
    oType := _buf.getObjType( _version ) ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) and
       dwgVersionLower( _version, DWG_Version.AC1024 ) then begin
      objSize := _buf.getRawLong32;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
      ms := _buf.size;
      objSize := ms * 8 - _bs;
    end ;
    if ( _strBuf <> nil ) and dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      _strBuf.moveBitPos( objSize - 1 ) ;
      if ( _strBuf.getBit = 1 ) then begin
        _strBuf.moveBitPos( - 17 ) ;
        strDataSize := _strBuf.getRawShort16;
        if ( ( strDataSize and $8000 ) = $8000 ) then begin
          _strBuf.moveBitPos( - 33 ) ;
          hiSize := _strBuf.getRawShort16;
          strDataSize := ( ( strDataSize and $7FFF ) or ( hiSize shl 15 ) ) ;
        end ;
        _strBuf.moveBitPos( - strDataSize - 16 ) ;
      end ;
    end ;

    ho := _buf.getHandle;
    handle := ho.ref;
    extDataSize := _buf.getBitShort;
    while ( extDataSize > 0 ) and ( _buf.isGood ) do begin
      ah := _buf.getHandle;
      SetLength( tmpExtData, extDataSize ) ;
      _buf.getBytes( tmpExtData, extDataSize ) ;

      tmpExtDataBuf := DwgBuffer.Create( tmpExtData, extDataSize, _buf.decoder ) ;
      try
        pos := tmpExtDataBuf.getPosition;
        bpos := tmpExtDataBuf.getBitPos;
        dxfCode := tmpExtDataBuf.getRawChar8;
        case ( dxfCode ) of
          0 : begin
                strLength := tmpExtDataBuf.getRawChar8;
                cp := tmpExtDataBuf.getBERawShort16;
                for i := 0 to strLength do begin
                  dxfChar := tmpExtDataBuf.getRawChar8;
                end
              end
        end ;
        extDataSize := _buf.getBitShort;
      finally
        FreeObject( tmpExtDataBuf ) ;
      end ;
    end ;

    if dwgVersionLower( _version, DWG_Version.AC1015 ) then
      objSize := _buf.getRawLong32;

    numReactors := _buf.getBitLong;
    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      xDictFlag := _buf.getBit;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1024 ) then begin
      bd := _buf.getBit;
    end ;
    Result := _buf.isGood;
  end ;

  procedure DwgTableEntry.reset;
  begin
    flag := 0;
  end ;

// ----------------------------------------------------------------------------
// DwgObjControl
// ----------------------------------------------------------------------------

  constructor DwgObjControl.Create;
  begin
    inherited;

    handlesList := TList<duint64>.Create;
    reset;
  end ;

 {$IFNDEF OXYGENE}
  destructor DwgObjControl.Destroy ;
  begin
    FreeObject( handlesList ) ;
    inherited ;
  end ;
 {$ENDIF}

  function DwgObjControl.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    unkData    : dint32;
    ret        : Boolean;
    numEntries : dint32;
    stringBit  : dint32;
    objectH    : DwgHandle;
    XDicObjH   : DwgHandle;
    i          : Integer;
  begin
    unkData := 0;
    ret := inherited parseDwgEx( _version, _buf, nil, _bs ) ;

    if not ret then begin
      Result := ret;
      exit;
    end ;

    numEntries := _buf.getBitLong;

    if ( oType = 68 ) and dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      unkData := _buf.getRawChar8;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      stringBit := _buf.getBit;
    end ;

    objectH := _buf.getHandle;

    if ( xDictFlag <> 1 ) then begin
      XDicObjH := _buf.getHandle;
    end ;

    if ( ( oType = 48 ) or ( oType = 56 ) ) then
      numEntries := numEntries + 2 ;

    for i := 0 to numEntries - 1 do begin
      objectH := _buf.getOffsetHandle( handle ) ;
      if ( objectH.ref <> 0 ) then
        handlesList.Add( objectH.ref ) ;
    end ;

    for i := 0 to unkData - 1 do begin
      objectH := _buf.getOffsetHandle( handle ) ;
    end ;
    Result := _buf.isGood;
  end ;

  procedure DwgObjControl.reset;
  begin

  end ;

// ----------------------------------------------------------------------------
// DwgLType
// ----------------------------------------------------------------------------

  constructor DwgLType.Create;
  begin
    inherited;

    path := TList<Double>.Create;
    reset;
  end ;

  {$IFNDEF OXYGENE}
  destructor DwgLType.Destroy ;
  begin
    FreeObject( path ) ;
    inherited ;
  end ;
  {$ENDIF}

  function DwgLType.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret                        : Boolean;
    sBuff, sBuf                : DwgBuffer;
    xrefindex                  : dint16;
    xdep                       : duint8;
    align                      : duint8;
    haveStrArea                : Boolean;
    i                          : Integer;
    bs2                        : dint32;
    strarea                    : TBytes;
    ltControlH                 : DwgHandle;
    reactorsH                  : DwgHandle;
    XDicObjH, XRefH, shpHandle : DwgHandle;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then
        sBuf := sBuff;

      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;

      if not ret then begin
        Result := ret;
        exit;
      end ;
      name := sBuf.getVariableText( _version, false ) ;
      flag := _buf.getBit shl 6;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      end
      else begin
        xrefindex := _buf.getBitShort;
      end ;
      xdep := _buf.getBit;
      flag := flag or xdep shl 4;
      desc := sBuf.getVariableText( _version, false ) ;
      length := _buf.getBitDouble;
      align := _buf.getRawChar8;
      size := _buf.getRawChar8;
      haveStrArea := false;
      for i := 0 to size - 1 do begin
        path.Add( _buf.getBitDouble ) ;
        _buf.getBitShort;
        _buf.getRawDouble;
        _buf.getRawDouble;
        _buf.getBitDouble;
        _buf.getBitDouble;
        bs2 := _buf.getBitShort;
        if ( ( bs2 and 2 ) <> 0 ) then
          haveStrArea := true;
      end ;
      if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
        SetLength( strarea, 256 ) ;
        _buf.getBytes( strarea, 256 ) ;
      end
      else begin
        if ( haveStrArea ) then begin
          SetLength( strarea, 512 ) ;
          _buf.getBytes( strarea, 512 ) ;
        end
      end ;

      if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
      end ;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        _buf.setPosition( objSize shr 3 ) ;
        _buf.setBitPos( objSize and 7 ) ;
      end ;

      if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
      end ;

      ltControlH := _buf.getHandle;
      parentHandle := ltControlH.ref;
      for i := 0 to numReactors - 1 do begin
        reactorsH := _buf.getHandle;
      end ;
      if ( xDictFlag <> 1 ) then begin
        XDicObjH := _buf.getHandle;
      end ;
      if ( size > 0 ) then begin
        XRefH := _buf.getHandle;
        shpHandle := _buf.getHandle;
      end ;
      shpHandle := _buf.getHandle;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

  procedure DwgLType.reset;
  begin
    tableType := DWG_TTYPE.LTYPE;
    desc      := '';
    size      := 0;
    length    := 0.0;
    pathIdx   := 0;
    inherited reset;
  end ;

  procedure DwgLType.update;
  var
    d : Double ;
    i : Integer ;
  begin
    d := 0 ;
    size := path.Count ;
    for i := 0 to size-1 do
      d := d + Abs( path[i] ) ;

    length := d;
  end ;

// ----------------------------------------------------------------------------
// DwgLayer
// ----------------------------------------------------------------------------

  constructor DwgLayer.Create ;
  begin
    inherited;
    reset;
  end ;

  function DwgLayer.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret                         : Boolean;
    sBuff, sBuf                 : DwgBuffer;
    f                           : dint16;
    layerControlH, XDicObjH,
    XRefH, plotStyH, materialH : DwgHandle;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        sBuf := sBuff;
      end ;
      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;

      if not ret then begin
        Result := ret;
        exit;
      end ;
      name := sBuf.getVariableText( _version, false ) ;

      flag := flag or _buf.getBit shl 6;
      if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
        _buf.getBitShort ;
      end ;
      flag := flag or _buf.getBit shl 4;
      if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
        flag := flag or _buf.getBit;
        _buf.getBit;
        flag := flag or _buf.getBit shl 1;
        flag := flag or _buf.getBit shl 3;
      end ;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        f := _buf.getSBitShort;
        flag := flag or f and $0001;
        flag := flag or ( f shr 1 ) and $0002;
        flag := flag or ( f shr 1 ) and $0004;
        plotF := ( ( f shr 4 ) and $0001 ) > 0;
        lWeight := ( f and $03E0 ) shr 5;
      end ;
      color := _buf.getCmColor( _version ) ;

      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        _buf.setPosition( objSize shr 3 ) ;
        _buf.setBitPos( objSize and 7 ) ;
      end ;
      layerControlH := _buf.getHandle;
      parentHandle := layerControlH.ref;

      if ( xDictFlag <> 1 ) then begin
        XDicObjH := _buf.getHandle;
      end ;
      XRefH := _buf.getHandle;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        plotStyH := _buf.getHandle;
        handlePlotS := IntToHex( plotStyH.ref,4 ) ;
      end ;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        materialH := _buf.getHandle;
        handleMaterialS := IntToHex( materialH.ref,4 ) ;
      end ;
      lTypeH := _buf.getHandle;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

  procedure DwgLayer.reset;
  begin
    tableType := DWG_TTYPE.LAYER;
    lineType  := 'CONTINUOUS';
    color     := DwgColor.Create( DwgColorType.ByLayer, 256 ); // default BYLAYER (256)
    plotF     := true; // default TRUE
    lWeight   := 31; // default BYDEFAULT
    inherited;
  end ;

// ----------------------------------------------------------------------------
// DwgTextstyle
// ----------------------------------------------------------------------------

  constructor DwgTextstyle.Create;
  begin
    inherited;

    reset;
  end ;

  function DwgTextstyle.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret                          : Boolean;
    sBuff, sBuf                  : DwgBuffer;
    shpControlH, XDicObjH, XRefH : DwgHandle;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        sBuf := sBuff;
      end ;
      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;

      if not ret then begin
        Result := ret;
        exit;
      end ;
      name := sBuf.getVariableText( _version, false ) ;
      flag := flag or _buf.getBit shl 6;
      _buf.getBitShort;
      flag := flag or _buf.getBit shl 4;
      flag := flag or _buf.getBit shl 2;
      flag := flag or _buf.getBit;
      height := _buf.getBitDouble;
      width := _buf.getBitDouble;
      oblique := _buf.getBitDouble;
      genFlag := _buf.getRawChar8;
      lastHeight := _buf.getBitDouble;
      font := sBuf.getVariableText( _version, false ) ;
      bigFont := sBuf.getVariableText( _version, false ) ;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        _buf.setPosition( objSize shr 3 ) ;
        _buf.setBitPos( objSize and 7 ) ;
      end ;
      shpControlH := _buf.getHandle;
      parentHandle := shpControlH.ref;
      if ( xDictFlag <> 1 ) then begin
        XDicObjH := _buf.getHandle;
      end ;
      XRefH := _buf.getHandle;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

  procedure DwgTextstyle.reset;
  begin
    tableType   := DWG_TTYPE.STYLE;
    height      := 0.0;
    oblique     := 0.0;
    width       := 1.0;
    lastHeight  := 1.0;
    font        := 'txt';
    genFlag     := 0; // 2= X mirror, 4= Y mirror
    fontFamily  := 0;
    inherited;
  end ;

// ----------------------------------------------------------------------------
// DwgDimstyle
// ----------------------------------------------------------------------------

  constructor DwgDimstyle.Create;
  begin
    inherited;

    reset;
  end ;

  function DwgDimstyle.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret         : Boolean;
    sBuff, sBuf : DwgBuffer;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        sBuf := sBuff;
      end ;
      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;

      if not ret then begin
        Result := ret;
        exit;
      end ;
      name := sBuf.getVariableText( _version, false ) ;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

  procedure DwgDimstyle.reset;
  begin
    tableType := DWG_TTYPE.DIMSTYLE;

    inherited;
  end ;

// ----------------------------------------------------------------------------
// DwgVport
// ----------------------------------------------------------------------------

  constructor DwgVport.Create;
  begin
    inherited;
    {$IFDEF GIS_NORECORDS}
      lowerLeft    := DwgCoord.Create( 0, 0, 0 ) ;
      UpperRight   := DwgCoord.Create( 0, 0, 0 ) ;
      center       := DwgCoord.Create( 0, 0, 0 ) ;
      snapBase     := DwgCoord.Create( 0, 0, 0 ) ;
      snapSpacing  := DwgCoord.Create( 0, 0, 0 ) ;
      gridSpacing  := DwgCoord.Create( 0, 0, 0 ) ;
      viewDir      := DwgCoord.Create( 0, 0, 0 ) ;
      viewTarget   := DwgCoord.Create( 0, 0, 0 ) ;
    {$ENDIF}
    reset;
  end ;

  function DwgVport.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret         : Boolean;
    sBuff, sBuf : DwgBuffer;
    vpControlH, XDicObjH, XRefH, bkgrdH, visualStH, sunH, namedUCSH,
      baseUCSH : DwgHandle;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        sBuf := sBuff;
      end ;
      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;

      if not ret then begin
        Result := ret;
        exit;
      end ;
      name := sBuf.getVariableText( _version, false ) ;

      flag := flag or _buf.getBit shl 6;
      if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
        _buf.getBitShort;
      end ;
      flag := flag or _buf.getBit shl 4;

      height := _buf.getBitDouble;
      ratio := _buf.getBitDouble;
      center := _buf.get2RawDouble;
      viewTarget.x := _buf.getBitDouble;
      viewTarget.y := _buf.getBitDouble;
      viewTarget.z := _buf.getBitDouble;
      viewDir.x := _buf.getBitDouble;
      viewDir.y := _buf.getBitDouble;
      viewDir.z := _buf.getBitDouble;
      twistAngle := _buf.getBitDouble;
      lensHeight := _buf.getBitDouble;
      frontClip := _buf.getBitDouble;
      backClip := _buf.getBitDouble;
      viewMode := _buf.getBit;
      viewMode := viewMode or _buf.getBit shl 1;
      viewMode := viewMode or _buf.getBit shl 2;
      viewMode := viewMode or _buf.getBit shl 4;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        _buf.getRawChar8 ;
        if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
          _buf.getBit ;
          _buf.getRawChar8 ;
          _buf.getBitDouble ;
          _buf.getBitDouble ;
          _buf.getCmColor( _version ) ;
        end ;
      end ;
      lowerLeft := _buf.get2RawDouble;
      UpperRight := _buf.get2RawDouble;
      viewMode := viewMode or _buf.getBit shl 3;
      circleZoom := _buf.getBitShort;
      fastZoom := _buf.getBit;
      ucsIcon := _buf.getBit;
      ucsIcon := ucsIcon or _buf.getBit shl 1;
      grid := _buf.getBit;
      gridSpacing := _buf.get2RawDouble;
      snap := _buf.getBit;
      snapStyle := _buf.getBit;
      snapIsopair := _buf.getBitShort;
      snapAngle := _buf.getBitDouble;
      snapBase := _buf.get2RawDouble;
      snapSpacing := _buf.get2RawDouble;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        _buf.getBit ;
        _buf.getBit ;
        _buf.getBitDouble; _buf.getBitDouble; _buf.getBitDouble ;
        _buf.getBitDouble; _buf.getBitDouble; _buf.getBitDouble ;
        _buf.getBitDouble; _buf.getBitDouble; _buf.getBitDouble ;
        _buf.getBitDouble ;
        _buf.getBitShort ;
        if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
          gridBehavior := _buf.getBitShort;
          _buf.getBitShort ;
        end ;
      end ;

      // common handles
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        _buf.setPosition( objSize shr 3 ) ;
        _buf.setBitPos( objSize and 7 ) ;
      end ;
      vpControlH := _buf.getHandle;
      parentHandle := vpControlH.ref;
      if ( xDictFlag <> 1 ) then begin
        XDicObjH := _buf.getHandle;
      end ;
      XRefH := _buf.getHandle;

      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
          bkgrdH := _buf.getHandle;
          visualStH := _buf.getHandle;
          sunH := _buf.getHandle;
        end ;
        namedUCSH := _buf.getHandle;
        baseUCSH := _buf.getHandle;
      end ;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

  procedure DwgVport.reset;
  begin
    tableType     := DWG_TTYPE.VPORT;
    UpperRight.x  := 1.0;
    UpperRight.y  := 1.0;
    snapSpacing.x := 10.0;
    snapSpacing.y := 10.0;
    gridSpacing   := snapSpacing;
    center.x      := 0.651828;
    center.y      := -0.16;
    viewDir.z     := 1;
    height        := 5.13732;
    ratio         := 2.4426877;
    lensHeight    := 50;
    frontClip     := 0.0;
    backClip      := 0.0;
    snapAngle     := 0.0;
    twistAngle    := 0.0;
    viewMode      := 0;
    snap          := 0;
    grid          := 0;
    snapStyle     := 0;
    snapIsopair   := 0;
    fastZoom      := 1;
    circleZoom    := 100;
    ucsIcon       := 3;
    gridBehavior  := 7;
    inherited;
  end ;

// ----------------------------------------------------------------------------
// DwgBlockRecord
// ----------------------------------------------------------------------------

  constructor DwgBlockRecord.Create;
  begin
    inherited;

    {$IFDEF GIS_NORECORDS}
      basePoint := DwgCoord.Create( 0, 0, 0 ) ;
    {$ENDIF}
    reset;
    entMap := TList<duint64>.Create;
  end ;

  {$IFNDEF OXYGENE}
  destructor DwgBlockRecord.Destroy ;
  begin
    FreeObject( entMap ) ;
    inherited ;
  end ;
  {$ENDIF}

  function DwgBlockRecord.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret          : Boolean;
    sBuff, sBuf  : DwgBuffer;
    insertCount  : duint32;
    objectCount  : duint32;
    xrefindex    : dint16;
    blockIsXref  : duint8;
    xrefOverlaid : duint8;
    path, bkdesc : String;
    i, j         : Integer;
    ii           : duint32;
    prevData     : duint32;
    canExplode   : duint8;
    bkScaling    : duint8;
    blockControlH, reactorH,
    XDicObjH, NullH,
    blockH, entityH,
    firstH, lastH,
    endBlockH, insertsH,
    layoutH       : DwgHandle;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        sBuf := sBuff;
      end ;
      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;

      if not ret then begin
        Result := ret;
        exit;
      end ;
      name := sBuf.getVariableText( _version, false ) ;

      insertCount := 0;
      objectCount := 0;
      flag := flag or _buf.getBit shl 6;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      end
      else begin
        xrefindex := _buf.getBitShort;
      end ;
      flag := flag or _buf.getBit shl 4;
      flag := flag or _buf.getBit;
      flag := flag or _buf.getBit shl 1;
      blockIsXref := _buf.getBit;
      xrefOverlaid := _buf.getBit;
      flag := flag or blockIsXref shl 2;
      flag := flag or xrefOverlaid shl 3;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        flag := flag or _buf.getBit shl 5;
      end ;
      if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
        objectCount := _buf.getBitLong;
        {$IFNDEF JAVA OR ISLAND}
        entMap.Capacity := objectCount;
        {$ENDIF}
      end ;
      basePoint.x := _buf.getBitDouble;
      basePoint.y := _buf.getBitDouble;
      basePoint.z := _buf.getBitDouble;
      path := sBuf.getVariableText( _version, false ) ;

      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        insertCount := 0;
        ii := _buf.getRawChar8;
        while ii <> 0 do begin
          insertCount := insertCount + ii;
          ii := _buf.getRawChar8;
        end ;
        bkdesc := sBuf.getVariableText( _version, false ) ;

        prevData := duint32( _buf.getBitLong ) ;
        if prevData > 0 then
          for j := 0 to prevData - 1 do
            _buf.getRawChar8;
      end ;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        insUnits := _buf.getBitShort;
        canExplode := _buf.getBit;
        bkScaling := _buf.getRawChar8;
      end ;

      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        _buf.setPosition( objSize shr 3 ) ;
        _buf.setBitPos( objSize and 7 ) ;
      end ;

      blockControlH := _buf.getHandle;
      parentHandle := blockControlH.ref;

      for i := 0 to numReactors - 1 do begin
        reactorH := _buf.getHandle;
      end ;
      if ( xDictFlag <> 1 ) then begin
        XDicObjH := _buf.getHandle;
      end ;
      if not dwgVersionEqual( _version, DWG_Version.AC1021 ) then begin

      end ;
      NullH := _buf.getHandle;
      blockH := _buf.getOffsetHandle( handle ) ;
      blockEnt := blockH.ref;

      if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
        if objectCount > 0 then begin
          for i := 0 to objectCount - 1 do begin
            entityH := _buf.getHandle;
            entMap.Add( entityH.ref ) ;
          end ;
        end
      end
      else begin
        if ( blockIsXref = 0 ) and ( xrefOverlaid = 0 ) then begin
          firstH := _buf.getHandle;
          firstEH := firstH.ref;
          lastH := _buf.getHandle;
          lastEH := lastH.ref;
        end
      end ;
      endBlockH := _buf.getOffsetHandle( handle ) ;
      endBlock := endBlockH.ref;

      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        if insertCount > 0 then begin
          for i := 0 to insertCount - 1 do begin
            insertsH := _buf.getHandle;
          end ;
        end ;
        layoutH := _buf.getHandle;
      end ;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

  procedure DwgBlockRecord.reset;
  begin
    tableType := DWG_TTYPE.BLOCK_RECORD;
    flag      := 0;
    firstEH   := 0;
    lastEH    := 0;

    inherited;
  end ;

  function DwgDbColor.parseDwg(
    const _version  : DWG_Version;
    const _buf      : DwgBuffer;
    const _bs       : duint32
  ) : Boolean ;
  var
   ret : Boolean;
   i : Integer ;
   subEntityHandle, reactorH, XDicObjH : DwgHandle ;
  begin
    ret := inherited parseDwgEx( _version, _buf, nil, _bs ) ;

    if not ret then begin
      Result := ret;
      exit;
    end ;

    color := _buf.getCmColor( _version ) ;

    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      _buf.setPosition( objSize shr 3 ) ;
      _buf.setBitPos( objSize and 7 ) ;
    end ;

    subEntityHandle := _buf.getOffsetHandle( handle ) ;
    parentHandle := subEntityHandle.ref;

    for i := 0 to numReactors - 1 do begin
      reactorH := _buf.getHandle;
    end ;

    if ( xDictFlag <> 1 ) then begin
      XDicObjH := _buf.getHandle;
    end ;

    Result := _buf.isGood;
  end ;


// ----------------------------------------------------------------------------
// DwgLayout
// ----------------------------------------------------------------------------

   constructor DwgLayout.Create;
   begin
     inherited;
     reset;
     viewport_handles := TList<DwgHandle>.Create ;
     {$IFDEF GIS_NORECORDS}
     plot_origin         := new DwgCoord(0, 0, 0) ;
     window_min          := new DwgCoord(0, 0, 0) ;
     window_max          := new DwgCoord(0, 0, 0) ;
     paper_image_origin  := new DwgCoord(0, 0, 0) ;
     ucs_origin          := new DwgCoord(0, 0, 0) ;
     minimum_limits      := new DwgCoord(0, 0, 0) ;
     maximum_limits      := new DwgCoord(0, 0, 0) ;
     ins_point           := new DwgCoord(0, 0, 0) ;
     ucs_x_axis          := new DwgCoord(0, 0, 0) ;
     ucs_y_axis          := new DwgCoord(0, 0, 0) ;
     extent_min          := new DwgCoord(0, 0, 0) ;
     extent_max          := new DwgCoord(0, 0, 0) ;
     {$ENDIF}

   end ;

   {$IFNDEF OXYGENE}
   destructor DwgLayout.Destroy;
   begin
     FreeObject( viewport_handles ) ;

     inherited;
   end ;
  {$ENDIF}

   function DwgLayout.parseDwg(
     const _version  : DWG_Version;
     const _buf      : DwgBuffer;
     const _bs       : duint32
   ) : Boolean ;
   var
     ret             : Boolean;
     sBuff, sBuf     : DwgBuffer;
     reactorsH   : DwgHandle;
     XDicObjH : DwgHandle;
     i : Integer ;
   begin
     sBuff := DwgBuffer.Create( _buf ) ;
     try
       sBuf := _buf;
       if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
         sBuf := sBuff;
       end ;
       ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;

       if not ret then begin
         Result := ret;
         exit;
       end ;

       page_setup_name   := sBuf.getVariableText( _version, false ) ;
       printer_or_config := sBuf.getVariableText( _version, false ) ;
       plot_layout_flags := _buf.getSBitShort ;
       left_margin := _buf.getBitDouble ;
       bottom_margin := _buf.getBitDouble ;
       right_margin := _buf.getBitDouble ;
       top_margin := _buf.getBitDouble ;
       paper_width := _buf.getBitDouble ;
       paper_height := _buf.getBitDouble ;
       paper_size := sBuf.getVariableText( _version, false ) ;
       plot_origin.x := _buf.getBitDouble ;
       plot_origin.y := _buf.getBitDouble ;
       paper_units := _buf.getSBitShort ;
       plot_rotation := _buf.getSBitShort ;
       plot_type := _buf.getSBitShort ;
       window_min.x := _buf.getBitDouble ;
       window_min.y := _buf.getBitDouble ;
       window_max.x := _buf.getBitDouble ;
       window_max.y := _buf.getBitDouble ;

       if dwgVersionLower( _version, DWG_Version.AC1018) then
         plot_view_name := sBuf.getVariableText( _version, false ) ;

       real_world_units := _buf.getBitDouble ;
       drawing_units := _buf.getBitDouble ;
       current_style_sheet := sBuf.getVariableText( _version, false ) ;
       scale_type := _buf.getSBitShort ;
       scale_factor := _buf.getBitDouble ;
       paper_image_origin.x := _buf.getBitDouble  ;
       paper_image_origin.y := _buf.getBitDouble  ;

       if dwgVersionHigher(_version, DWG_Version.AC1015) then begin
         shade_plot_mode := _buf.getSBitShort ;
         shade_plot_res_level := _buf.getSBitShort ;
         shade_plot_custom_dpi := _buf.getSBitShort ;
       end ;

       layout_name := sBuf.getVariableText( _version, false ) ;
       tab_order := _buf.getSBitShort ;
       flags := _buf.getSBitShort ;
       ucs_origin.x := _buf.getBitDouble ;
       ucs_origin.y := _buf.getBitDouble ;
       ucs_origin.z := _buf.getBitDouble ;
       minimum_limits.x := _buf.getRawDouble ;
       minimum_limits.y := _buf.getRawDouble ;
       maximum_limits.x := _buf.getRawDouble ;
       maximum_limits.y := _buf.getRawDouble ;
       ins_point.x := _buf.getBitDouble ;
       ins_point.y := _buf.getBitDouble ;
       ins_point.z := _buf.getBitDouble ;
       ucs_x_axis.x := _buf.getBitDouble ;
       ucs_x_axis.y := _buf.getBitDouble ;
       ucs_x_axis.z := _buf.getBitDouble ;
       ucs_y_axis.x := _buf.getBitDouble ;
       ucs_y_axis.y := _buf.getBitDouble ;
       ucs_y_axis.z := _buf.getBitDouble ;
       elevation := _buf.getBitDouble ;
       orthoview_type := _buf.getSBitShort ;
       extent_min.x := _buf.getBitDouble ;
       extent_min.y := _buf.getBitDouble ;
       extent_min.z := _buf.getBitDouble ;
       extent_max.x := _buf.getBitDouble ;
       extent_max.y := _buf.getBitDouble ;
       extent_max.z := _buf.getBitDouble ;

       if dwgVersionHigher(_version, DWG_Version.AC1015) then
         viewport_count := _buf.getBitLong ;

       if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
         _buf.setPosition( objSize shr 3 ) ;
         _buf.setBitPos( objSize and 7 ) ;
       end ;

       parentHandle := _buf.getHandle.ref;

       for i := 0 to numReactors - 1 do begin
         reactorsH := _buf.getHandle;
       end ;

       if ( xDictFlag <> 1 ) then begin
         XDicObjH := _buf.getHandle;
       end ;

       if dwgVersionHigher(_version, DWG_Version.AC1015) then
         plot_view_handle := _buf.getHandle;

       if dwgVersionHigher(_version, DWG_Version.AC1018) then
         visual_style_handle := _buf.getHandle;

       assoc_paperspace_block_hdl := _buf.getHandle;
       last_active_viewport_handle := _buf.getHandle;
       base_ucs_handle := _buf.getHandle;
       named_ucs_handle := _buf.getHandle;

       if dwgVersionHigher(_version, DWG_Version.AC1015) then begin
         if viewport_count > 0 then
           for i := 0 to viewport_count - 1 do
             viewport_handles.Add( _buf.getHandle );
       end ;

       Result := _buf.isGood;
     finally
       FreeObject( sBuff ) ;
     end ;
   end ;

   procedure DwgLayout.reset;
   begin
     tableType := DWG_TTYPE.LAYOUT;

     inherited;
   end ;

// ----------------------------------------------------------------------------
// DwgAppId
// ----------------------------------------------------------------------------

  function DwgAppId.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret             : Boolean;
    sBuff, sBuf     : DwgBuffer;
    UNKNOWN         : duint8;
    appIdControlH   : DwgHandle;
    XRefH, XDicObjH : DwgHandle;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        sBuf := sBuff;
      end ;
      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;

      if not ret then begin
        Result := ret;
        exit;
      end ;
      name := sBuf.getVariableText( _version, false ) ;
      flag := flag or _buf.getBit shl 6;
      _buf.getBitShort;
      flag := flag or _buf.getBit shl 4;

      UNKNOWN := _buf.getRawChar8;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        _buf.setPosition( objSize shr 3 ) ;
        _buf.setBitPos( objSize and 7 ) ;
      end ;
      appIdControlH := _buf.getHandle;
      parentHandle := appIdControlH.ref;
      if ( xDictFlag <> 1 ) then begin
        XDicObjH := _buf.getHandle;
      end ;
      XRefH := _buf.getHandle;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

  procedure DwgAppId.reset;
  begin
    inherited;

  end ;

// ----------------------------------------------------------------------------
// DwgBlock
// ----------------------------------------------------------------------------

  constructor DwgBlock.Create;
  begin
    inherited;

    eType := DWG_ETYPE.BLOCK;
    layer := '0';
    flag  := 0;
    name  := '*0';
    isEnd := false;
  end ;

  function DwgBlock.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret         : Boolean;
    sBuff, sBuf : DwgBuffer;
    unk         : duint8;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        sBuf := sBuff;
      end ;
      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;
      if not ret then begin
        Result := ret;
        exit;
      end ;
      if not isEnd then begin
        name := sBuf.getVariableText( _version, false ) ;
      end ;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        unk := _buf.getBit;
      end ;

      ret := inherited parseDwgEntHandle( _version, _buf ) ;
      if not ret then
        Result := ret
      else
        Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

 // ----------------------------------------------------------------------------
 // DwgDimensionCommon
 // ----------------------------------------------------------------------------

  constructor DwgDimensionCommon.Create;
  begin
    inherited;

    eType      := DWG_ETYPE.DIMENSION;
    dtype      := 0;
    linesty    := 1;
    linefactor := 1.0 ;
    angle      := 0 ;
    oblique    := 0 ;
    rot        := 0.0;
    align      := 5;
    rot        := 0;
    style      := 'STANDARD';
    {$IFDEF GIS_NORECORDS}
      defPoint   := DwgCoord.Create( 0, 0, 0 ) ;
      clonePoint := DwgCoord.Create( 0, 0, 0 ) ;
      textPoint  := DwgCoord.Create( 0, 0, 0 ) ;
      extPoint   := DwgCoord.Create( 0, 0, 0 ) ;
    {$ENDIF}
    extPoint.z   := 1.0;
    defPoint.z   := 0;
    extPoint.x   := 0;
    extPoint.y   := 0;
    textPoint.z  := 0;
    clonePoint.x := 0;
    clonePoint.y := 0;
    clonePoint.z := 0;
  end ;

  function DwgDimensionCommon.parseDwgCommon(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _sbuf    : DwgBuffer
  ) : Boolean;
  var
    dimVersion : duint8 ;
    inspoint   : DwgCoord ;
    actMeas    : Double ;
    insRot_code54 : Double ;
    unk, flip1, flip2 : duint8 ;
  begin
    if dwgVersionHigher( _version, DWG_Version.AC1021) then begin
      dimVersion := _buf.getRawChar8;
    end ;
    extPoint := _buf.getExtrusion( dwgVersionHigher( _version, DWG_Version.AC1014) );
    if dwgVersionHigher( _version, DWG_Version.AC1014) then begin
      _buf.getBit ;
      _buf.getBit ;
      _buf.getBit ;
      _buf.getBit ;
      _buf.getBit ;
    end ;
    textPoint.x := _buf.getRawDouble;
    textPoint.y := _buf.getRawDouble;
    textPoint.z := _buf.getBitDouble;
    dtype := _buf.getRawChar8;
    if (dtype and 1) <> 0 then
      dtype := dtype and $7F
    else
      dtype := dtype or $80;
    if (dtype and 2) <> 0 then
      dtype := dtype or $20
    else
      dtype := dtype and $DF;

    dtype := dtype and $F8;
    text := _sbuf.getVariableText(_version, false);
    rot := _buf.getBitDouble;
    hdir := _buf.getBitDouble;
    inspoint := _buf.get3BitDouble;
    insRot_code54 := _buf.getBitDouble;
    if dwgVersionHigher( _version, DWG_Version.AC1014) then begin
      align := _buf.getBitShort;
      linesty := _buf.getBitShort;
      linefactor := _buf.getBitDouble;
      actMeas := _buf.getBitDouble;
      if dwgVersionHigher( _version, DWG_Version.AC1018) then begin
        unk := _buf.getBit;
        flip1 := _buf.getBit;
        flip2 := _buf.getBit;
      end ;
    end ;
    clonePoint.x := _buf.getRawDouble;
    clonePoint.y := _buf.getRawDouble;

    Result := _buf.isGood;
  end ;


  constructor DwgDimensionAligned.Create;
  begin
    inherited;

    eType := DWG_ETYPE.DIMALIGNED;
  end ;

  function DwgDimensionAligned.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret         : Boolean;
    sBuff, sBuf : DwgBuffer;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
       sBuf := _buf;
       if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
         sBuf := sBuff;
       end ;
       ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;
       if not ret then begin
         Result := ret;
         exit;
       end ;
       ret := inherited parseDwgCommon( _version, _buf, sBuf ) ;
       if not ret then begin
         Result := ret;
         exit;
       end ;

       def1 := _buf.get3BitDouble();
       def2 := _buf.get3BitDouble();
       defPoint := _buf.get3BitDouble();
       oblique := _buf.getBitDouble();
       if (oType = $15) then
         angle := _buf.getBitDouble()
       else
         dtype := dtype or 1;

       ret := inherited parseDwgEntHandle( _version, _buf ) ;

       dimStyleH := _buf.getHandle();
       blockH := _buf.getHandle();

       if not ret then
         Result := ret
       else
         Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

  constructor DwgDimensionLinear.Create;
  begin
    inherited;

    eType := DWG_ETYPE.DIMLINEAR;
  end ;

  constructor DwgDimensionOrdinate.Create;
  begin
    inherited;

    eType := DWG_ETYPE.DIMORDINATE;
  end ;

  function DwgDimensionOrdinate.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret         : Boolean;
    sBuff, sBuf : DwgBuffer;
    type2       : duint8 ;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        sBuf := sBuff;
      end ;
      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;
      if not ret then begin
        Result := ret;
        exit;
      end ;
      ret := inherited parseDwgCommon( _version, _buf, sBuf ) ;
      if not ret then begin
        Result := ret;
        exit;
      end ;

      defPoint := _buf.get3BitDouble();
      def1 := _buf.get3BitDouble();
      def2 := _buf.get3BitDouble();

      type2 := _buf.getRawChar8 ;
      if (type2 and 1) <> 0 then
        dtype :=  dtype or $80
      else
        dtype :=  dtype and $BF;

      dtype := dtype or 6 ;

      ret := inherited parseDwgEntHandle( _version, _buf ) ;

      dimStyleH := _buf.getHandle();
      blockH := _buf.getHandle();

      if not ret then
        Result := ret
      else
        Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

  constructor DwgDimensionDiametric.Create;
  begin
    inherited;

    eType := DWG_ETYPE.DIMDIAMETRIC;
  end ;

  function DwgDimensionDiametric.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ret         : Boolean;
    sBuff, sBuf : DwgBuffer;
  begin
    sBuff := DwgBuffer.Create( _buf ) ;
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        sBuf := sBuff;
      end ;
      ret := inherited parseDwgEx( _version, _buf, sBuf, _bs ) ;
      if not ret then begin
        Result := ret;
        exit;
      end ;
      ret := inherited parseDwgCommon( _version, _buf, sBuf ) ;
      if not ret then begin
        Result := ret;
        exit;
      end ;

      defPoint := _buf.get3BitDouble();
      def1 := _buf.get3BitDouble();
      {$IFDEF JAVA}
        def2 := new DwgCoord(0, 0, 0);
      {$ENDIF}
      length := _buf.getBitDouble ;
      dtype := dtype or 3 ;

      ret := inherited parseDwgEntHandle( _version, _buf ) ;

      dimStyleH := _buf.getHandle();
      blockH := _buf.getHandle();

      if not ret then
        Result := ret
      else
        Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

// ----------------------------------------------------------------------------
// DwgEntity
// ----------------------------------------------------------------------------

  constructor DwgEntity.Create;
  begin
    inherited;

    eType         := DWG_ETYPE.UNKNOWN;
    handle        := 0;
    parentHandle  := 0;
    space         := DwgSpace.ModelSpace;
    layer         := '0';
    lineType      := 'BYLAYER';
    material      := 0;
    color         := DwgColor.Create( DwgColorType.ByLayer, 256 );
    lWeight       := 0;
    ltypeScale    := 1.0;
    visible       := true;
    numProxyGraph := 0;
    proxyGraphics := '' ;
    transparency  := 0;
    plotStyle     := 0;
    shadow        := 0;
    nextEntLink   := 0;
    prevEntLink   := 0;
    ownerHandle   := false;
    xDictFlag     := 0;
    numReactors   := 0;
  end ;

  {$IFDEF DCC}
  destructor DwgEntity.Destroy;
  begin
    FreeObject( extData ) ;

    inherited ;
  end ;
  {$ENDIF}

  function DwgEntity.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  begin
    Result := false;
  end ;

  function DwgEntity.parseDwgEntHandle(
    const _version : DWG_Version;
    const _buf     : DwgBuffer
  ) : Boolean;
  var
    i                     : Integer;
    ownerH, reactorsH,
    XDicObjH, nextLinkH,
    materialH, shadowH,
    plotStyleH, colorRefH : DwgHandle;
  begin
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      _buf.setPosition( objSize shr 3 ) ;
      _buf.setBitPos( objSize and 7 ) ;
    end ;

    if ( ownerHandle ) then begin
      ownerH := _buf.getOffsetHandle( handle ) ;
      parentHandle := ownerH.ref;
    end ;

    for i := 0 to numReactors - 1 do begin
      reactorsH := _buf.getHandle;
    end ;
    if ( xDictFlag <> 1 ) then begin
      XDicObjH := _buf.getHandle;
    end ;

    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      layerH := _buf.getOffsetHandle( handle ) ;
      if ( ltFlags = 3 ) then begin
        lTypeH := _buf.getOffsetHandle( handle ) ;
      end ;
    end ;
    if dwgVersionLower( _version, DWG_Version.AC1018 ) then begin
      if ( haveNextLinks = 0 ) then begin
        nextLinkH := _buf.getOffsetHandle( handle ) ;
        prevEntLink := nextLinkH.ref;
        nextLinkH := _buf.getOffsetHandle( handle ) ;
        nextEntLink := nextLinkH.ref;
      end
      else begin
        nextEntLink := handle + 1;
        prevEntLink := handle - 1;
      end ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      if color.CType = DwgColorType.ByReference then begin
        colorRefH   := _buf.getOffsetHandle( handle ) ;
        color.Value := colorRefH.ref ;
      end ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      layerH := _buf.getOffsetHandle( handle ) ;
      if ( ltFlags = 3 ) then begin
        lTypeH := _buf.getOffsetHandle( handle ) ;
      end ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        if ( materialFlag = 3 ) then begin
          materialH := _buf.getOffsetHandle( handle ) ;
        end ;
        if ( shadowFlag = 3 ) then begin
          shadowH := _buf.getOffsetHandle( handle ) ;
        end ;
      end ;
      if ( plotFlags = 3 ) then begin
        plotStyleH := _buf.getOffsetHandle( handle ) ;
      end ;
    end ;
    Result := _buf.isGood;
  end ;

  function DwgEntity.parseDwgEx(
    const _version      : DWG_Version;
    const _buf, _strBuf : DwgBuffer;
    const _bs           : duint32
  ) : Boolean;
  var
    ms                             : duint32;
    strDataSize                    : dint32;
    hiSize                         : duint16;
    ho, ah                         : DwgHandle;
    extDataSize                    : dint16;
    tmpExtData, tmpGraphData       : TBytes;
    tmpExtDataBuf, tmpGraphDataBuf : DwgBuffer;
    dxfCode, dxfChar               : duint8;
    strLength                      : duint8;
    cp                             : duint16;
    i                              : Integer;
    graphFlag                      : duint8;
    graphDataSize                  : duint32;
    entmode                        : duint8;
    plotStyleName                  : String;
    visualFlags                    : duint8;
    unk                            : duint8;
    invisibleFlag                  : duint16;
    eed                            : TBytes ;
    eedtxt                         : String ;
  begin
    objSize := 0;
    oType := _buf.getObjType( _version ) ;

    if dwgVersionHigher( _version, DWG_Version.AC1014 ) and
       dwgVersionLower( _version, DWG_Version.AC1024 ) then begin
      objSize := _buf.getRawLong32;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
      ms := _buf.size;
      objSize := ms * 8 - _bs;
    end ;

    if ( _strBuf <> nil ) and dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      _strBuf.moveBitPos( objSize - 1 ) ;
      if ( _strBuf.getBit = 1 ) then begin
        _strBuf.moveBitPos( - 17 ) ;
        strDataSize := _strBuf.getRawShort16;
        if ( ( strDataSize and $8000 ) = $8000 ) then begin
          _strBuf.moveBitPos( - 33 ) ;
          hiSize := _strBuf.getRawShort16;
          strDataSize := dint32( ( strDataSize and $7FFF ) or ( hiSize shl 15 ) ) ;
        end ;
        _strBuf.moveBitPos( - strDataSize - 16 ) ;
      end ;
    end ;

    ho := _buf.getHandle;
    handle := ho.ref;
    extDataSize := _buf.getBitShort;
    while ( extDataSize > 0 ) and ( _buf.isGood ) do begin // EED
      ah := _buf.getHandle;
      SetLength( tmpExtData, extDataSize ) ;
      _buf.getBytes( tmpExtData, extDataSize ) ;

      tmpExtDataBuf := DwgBuffer.Create( tmpExtData, extDataSize, _buf.decoder ) ;
      try
        while ( tmpExtDataBuf.getPosition < tmpExtDataBuf.size ) do begin
          dxfCode := tmpExtDataBuf.getRawChar8;
          case ( dxfCode ) of
            0 : begin
                  if dwgVersionLower( _version, DWG_Version.AC1021 ) then begin
                    strLength := tmpExtDataBuf.getRawChar8;
                    cp := tmpExtDataBuf.getBERawShort16;
                    SetLength( eed, strLength ) ;
                    for i := 0 to strLength-1 do
                      eed[i] := tmpExtDataBuf.getRawChar8;
                    eedtxt := tmpExtDataBuf.decoder.GetString( eed ) ;
                  end
                  else begin
                    cp := tmpExtDataBuf.getRawShort16;
                    eedtxt := tmpExtDataBuf.getUCSEed( cp ) ;
                  end;
                  if not assigned( extData ) then
                    extData := TStringList.Create ;
                  if not IsStringEmpty( eedtxt ) then
                    extData.Add( eedtxt ) ;
                end ;
            2 : begin
                  dxfChar := tmpExtDataBuf.getRawChar8;
                end;
            3,
            5 : begin
                  tmpExtDataBuf.getRawLong32 ;
                end;
            4 : begin
                  strLength := tmpExtDataBuf.getRawChar8;
                  if strLength > 0 then begin
                    SetLength( eed, strLength ) ;
                    for i := 0 to strLength-1 do
                      eed[i] := tmpExtDataBuf.getRawChar8 ;
                  end ;
                end;
            10,
            11,
            12,
            13 : begin
                   tmpExtDataBuf.getRawDouble ;
                   tmpExtDataBuf.getRawDouble ;
                   tmpExtDataBuf.getRawDouble ;
                 end;
            40,
            41,
            42 : begin
                   tmpExtDataBuf.getRawDouble ;
                 end;
            70 : begin
                   tmpExtDataBuf.getBERawShort16;
                 end;
            71 : begin
                   tmpExtDataBuf.getRawLong32;
                 end;
          end ;
        end ;
      finally
        FreeObject( tmpExtDataBuf ) ;
      end ;
      extDataSize := _buf.getBitShort;
    end ;

    graphFlag := _buf.getBit;
    if ( graphFlag <> 0 ) then begin
      graphDataSize := _buf.getRawLong32;

      SetLength( tmpGraphData, graphDataSize ) ;
      _buf.getBytes( tmpGraphData, graphDataSize ) ;
      tmpGraphDataBuf := DwgBuffer.Create( tmpGraphData, graphDataSize,
                                          _buf.decoder ) ;
    end ;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      objSize := _buf.getRawLong32;
    end ;

    entmode := _buf.get2Bits;
    if ( entmode = 0 ) then
      ownerHandle := true
    else if ( entmode = 2 ) then
      entmode := 0;
    space := DwgSpace( entmode ) ;
    numReactors := _buf.getBitShort;

    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      if ( _buf.getBit <> 0 ) then begin
        lineType := 'BYLAYER';
        ltFlags := 0;
      end
      else begin
        lineType := '';
        ltFlags := 3;
      end ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      xDictFlag := _buf.getBit;
    end ;

    if dwgVersionHigher( _version, DWG_Version.AC1024 ) or
       dwgVersionLower( _version, DWG_Version.AC1018 ) then
    begin
      haveNextLinks := _buf.getBit;
    end
    else begin
      haveNextLinks := 1;
    end ;

    // ENC color
    color := _buf.getEnColor( _version ) ;
    ltypeScale := _buf.getBitDouble;

    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      for i := 0 to 2 - 1 do begin
        plotFlags := _buf.get2Bits;
        if ( plotFlags = 1 ) then
          plotStyleName := 'byblock'
        else if ( plotFlags = 2 ) then
          plotStyleName := 'continuous'
        else if ( plotFlags = 0 ) then
          plotStyleName := 'bylayer'
        else
          plotStyleName := '';
        if ( i = 0 ) then begin
          ltFlags := plotFlags;
          lineType := plotStyleName;
        end
      end ;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
      materialFlag := _buf.get2Bits;
      shadowFlag := _buf.getRawChar8;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
      visualFlags := _buf.get2Bits;
      unk := _buf.getBit;
    end ;
    invisibleFlag := _buf.getBitShort;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      lWeight := _buf.getRawChar8;
    end ;
    Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgPoint
// ----------------------------------------------------------------------------

  constructor DwgPoint.Create;
  begin
    inherited;

    eType       := DWG_ETYPE.POINT;
    {$IFDEF GIS_NORECORDS}
      basePoint := DwgCoord.Create( 0, 0, 0 ) ;
      extPoint  := DwgCoord.Create( 0, 0, 0 ) ;
    {$ENDIF}
    basePoint.z := 0;
    extPoint.x  := 0;
    extPoint.y  := 0;
    extPoint.z  := 1;
    thickness   := 0;
  end ;

  function DwgPoint.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    x_axis : Double;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs ) ;
    if not Result then
      exit;

    basePoint.x := _buf.getBitDouble;
    basePoint.y := _buf.getBitDouble;
    basePoint.z := _buf.getBitDouble;
    thickness := _buf.getThickness(dwgVersionHigher( _version, DWG_Version.AC1014 )) ;
    extPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 )) ;
    x_axis := _buf.getBitDouble;

    Result := inherited parseDwgEntHandle( _version, _buf ) ;
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgLine
// ----------------------------------------------------------------------------

  constructor DwgLine.Create;
  begin
    inherited;

    eType := DWG_ETYPE.LINE;
    {$IFDEF GIS_NORECORDS}
      secPoint := DwgCoord.Create( 0, 0, 0 ) ;
    {$ENDIF}
    secPoint.z := 0;
  end ;

  function DwgLine.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    zIsZero : duint8;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs ) ;
    if not Result then
      exit;

    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      basePoint.x := _buf.getBitDouble;
      basePoint.y := _buf.getBitDouble;
      basePoint.z := _buf.getBitDouble;
      secPoint.x := _buf.getBitDouble;
      secPoint.y := _buf.getBitDouble;
      secPoint.z := _buf.getBitDouble;
    end ;
    if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
      zIsZero := _buf.getBit;
      basePoint.x := _buf.getRawDouble;
      secPoint.x := _buf.getDefaultDouble( basePoint.x ) ;
      basePoint.y := _buf.getRawDouble;
      secPoint.y := _buf.getDefaultDouble( basePoint.y ) ;
      if zIsZero = 0 then begin
        basePoint.z := _buf.getRawDouble;
        secPoint.z := _buf.getDefaultDouble( basePoint.z ) ;
      end
    end ;
    thickness := _buf.getThickness(dwgVersionHigher( _version, DWG_Version.AC1014 )) ;
    extPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 )) ;

    Result := inherited parseDwgEntHandle( _version, _buf ) ;
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgCircle
// ----------------------------------------------------------------------------

  constructor DwgCircle.Create;
  begin
    inherited;

    eType := DWG_ETYPE.CIRCLE;
  end ;

  function DwgCircle.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    basePoint.x := _buf.getBitDouble;
    basePoint.y := _buf.getBitDouble;
    basePoint.z := _buf.getBitDouble;
    radious := _buf.getBitDouble;
    thickness := _buf.getThickness(dwgVersionHigher( _version, DWG_Version.AC1014 ));
    extPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 ));

    Result := inherited parseDwgEntHandle( _version, _buf );
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgArc
// ----------------------------------------------------------------------------

  constructor DwgArc.Create;
  begin
    inherited;

    eType := DWG_ETYPE.ARC;
    isccw := 1;
  end ;

  function DwgArc.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    basePoint.x := _buf.getBitDouble;
    basePoint.y := _buf.getBitDouble;
    basePoint.z := _buf.getBitDouble;
    radious := _buf.getBitDouble;
    thickness := _buf.getThickness(dwgVersionHigher( _version, DWG_Version.AC1014 ));
    extPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 ));
    staangle := _buf.getBitDouble;
    endangle := _buf.getBitDouble;

    Result := inherited parseDwgEntHandle( _version, _buf );
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgEllipse
// ----------------------------------------------------------------------------

  constructor DwgEllipse.Create;
  begin
    eType := DWG_ETYPE.ELLIPSE;
    isccw := 1;
  end ;

  function DwgEllipse.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    basePoint := _buf.get3BitDouble;
    secPoint := _buf.get3BitDouble;
    extPoint := _buf.get3BitDouble;
    ratio := _buf.getBitDouble;
    staparam := _buf.getBitDouble;
    endparam := _buf.getBitDouble;

    Result := inherited parseDwgEntHandle( _version, _buf );
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgInsert
// ----------------------------------------------------------------------------

  constructor DwgInsert.Create;
  begin
    inherited;

    eType    := DWG_ETYPE.INSERT;
    xscale   := 1;
    yscale   := 1;
    zscale   := 1;
    angle    := 0;
    colcount := 1;
    rowcount := 1;
    colspace := 0;
    rowspace := 0;
  end ;

  function DwgInsert.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    dataFlags : duint8;
    hasAttrib : duint8;
    objCount  : dint32;
    i         : Integer;
    attH      : DwgHandle;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    objCount := 0;

    basePoint.x := _buf.getBitDouble;
    basePoint.y := _buf.getBitDouble;
    basePoint.z := _buf.getBitDouble;
    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      xscale := _buf.getBitDouble;
      yscale := _buf.getBitDouble;
      zscale := _buf.getBitDouble;
    end
    else begin
      dataFlags := _buf.get2Bits;
      if ( dataFlags = 3 ) then begin
        // none default value 1,1,1
      end
      else if ( dataFlags = 1 ) then begin
        yscale := _buf.getDefaultDouble( xscale );
        zscale := _buf.getDefaultDouble( xscale );
      end
      else if ( dataFlags = 2 ) then begin
        xscale := _buf.getRawDouble;
        yscale := xscale;
        zscale := xscale;
      end
      else begin
        xscale := _buf.getRawDouble;
        yscale := _buf.getDefaultDouble( xscale );
        zscale := _buf.getDefaultDouble( xscale );
      end
    end ;
    angle := _buf.getBitDouble;
    extPoint := _buf.getExtrusion( false );
    hasAttrib := _buf.getBit;

    if ( hasAttrib = 1 ) and dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      objCount := _buf.getBitLong;
    end ;
    if ( oType = 8 ) then begin
      colcount := _buf.getBitShort;
      rowcount := _buf.getBitShort;
      colspace := _buf.getBitDouble;
      rowspace := _buf.getBitDouble;
    end ;

    Result := inherited parseDwgEntHandle( _version, _buf );

    blockRecH := _buf.getHandle;

    if ( hasAttrib = 1 ) then begin
      if dwgVersionLower( _version, DWG_Version.AC1018 ) then begin
        attH := _buf.getHandle;
        attH := _buf.getHandle;
      end
      else begin
        if objCount > 0 then
          for i := 0 to objCount - 1 do begin
            attH := _buf.getHandle;
          end ;
      end ;
      seqendH := _buf.getHandle;
    end ;

    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgVertex2D
// ----------------------------------------------------------------------------

  constructor DwgVertex2D.Create;
  begin
    inherited;
    x        := 0;
    y        := 0;
    stawidth := 0;
    endwidth := 0;
    bulge    := 0;
  end ;

  constructor DwgVertex2D.Create(
    const _sx, _sy, _b : Double
  );
  begin
    inherited Create;
    x        := _sx;
    y        := _sy;
    stawidth := 0;
    endwidth := 0;
    bulge    := 0;
  end ;

// ----------------------------------------------------------------------------
// DwgLWPolyline
// ----------------------------------------------------------------------------

  constructor DwgLWPolyline.Create;
  begin
    inherited;

    eType     := DWG_ETYPE.LWPOLYLINE;
    elevation := 0.0;
    thickness := 0.0;
    width     := 0.0;
    flag      := 0;
    {$IFDEF GIS_NORECORDS}
      extPoint := DwgCoord.Create( 0, 0, 0 ) ;
    {$ENDIF}
    extPoint.x := 0.0;
    extPoint.y := 0.0;
    extPoint.z := 1;
    vertlist   := TList<DwgVertex2D>.Create ;
  end ;

  {$IFDEF DCC}
  destructor DwgLWPolyline.Destroy;
  var
    v : DwgVertex2D ;
  begin
    for v in vertlist do
      FreeObjectNotNil( v ) ;

    FreeObject( vertlist ) ;
    inherited ;
  end ;
  {$ENDIF}

  function DwgLWPolyline.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    widthsnum    : duint32;
    vertexIdCount,
    vertexId      : dint32;
    i             : Integer;
    {$IFDEF DCC}
    pvit          : DwgVertex2D;
    {$ENDIF}
    pv            : DwgVertex2D;
    bulge, staW,
    endW          : Double;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    flag := _buf.getBitShort;
    if ( flag and 4 ) <> 0 then
      width := _buf.getBitDouble;
    if ( flag and 8 ) <> 0 then
      elevation := _buf.getBitDouble;
    if ( flag and 2 ) <> 0 then
      thickness := _buf.getBitDouble;
    if ( flag and 1 ) <> 0 then
      extPoint := _buf.getExtrusion( false );

    vertexnum := _buf.getBitLong;
    {$IFNDEF JAVA OR ISLAND}
    vertlist.Capacity := vertexnum;
    {$ENDIF}
    bulgesnum := 0;
    if ( flag and 16 ) <> 0 then
      bulgesnum := _buf.getBitLong;
    vertexIdCount := 0;
    if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
      if ( flag and 1024 ) <> 0 then
        vertexIdCount := _buf.getBitLong;
    end ;

    widthsnum := 0;
    if ( flag and 32 ) <> 0 then
      widthsnum := _buf.getBitLong;
    if ( flag and 512 ) <> 0 then
      flag := flag or 1
    else
      flag := flag or 0;
    flag := flag and 129;

    if ( vertexnum > 0 ) then begin
      vertex := DwgVertex2D.Create;
      vertex.x := _buf.getRawDouble;
      vertex.y := _buf.getRawDouble;
      vertlist.Add( vertex );
      pv := vertex;
      for i := 1 to vertexnum - 1 do begin
        vertex := DwgVertex2D.Create;
        if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
          vertex.x := _buf.getRawDouble;
          vertex.y := _buf.getRawDouble;
        end
        else begin
          vertex.x := _buf.getDefaultDouble( pv.x );
          vertex.y := _buf.getDefaultDouble( pv.y );
        end ;
        pv := vertex;
        vertlist.Add( vertex );
      end ;
      // add bulges
      if bulgesnum > 0 then
        for i := 0 to bulgesnum - 1 do begin
          bulge := _buf.getBitDouble;
          if ( vertlist.Count > i ) then
            vertlist[i].bulge := bulge;
        end ;
      // add vertexId
      if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
        for i := 0 to vertexIdCount - 1 do begin
          vertexId := _buf.getBitLong;
        end
      end ;
      // add widths
      if widthsnum > 0 then
        for i := 0 to widthsnum - 1 do begin
          staW := _buf.getBitDouble;
          endW := _buf.getBitDouble;
          if ( vertlist.Count < i ) then begin
            vertlist[i].stawidth := staW;
            vertlist[i].endwidth := endW;
          end
        end ;
    end ;

    Result := inherited parseDwgEntHandle( _version, _buf );
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

 // ----------------------------------------------------------------------------
 // DwgLeader
 // ----------------------------------------------------------------------------

   constructor DwgLeader.Create;
   begin
     inherited;

     eType      := DWG_ETYPE.LEADER;
     flag       := 3;
     hookflag   := 0;
     vertnum    := 0;
     leadertype := 0;
     arrow      := 1;
     {$IFDEF GIS_NORECORDS}
       extrusionPoint := DwgCoord.Create( 0, 0, 0 ) ;
     {$ENDIF}
     extrusionPoint.x := 0.0;
     extrusionPoint.y := 0.0;
     extrusionPoint.z := 1;
     vertexlist       := TList<DwgCoord>.Create ;
   end ;

   {$IFDEF DCC}
   destructor DwgLeader.Destroy;
   begin
     FreeObject( vertexlist ) ;
     inherited ;
   end ;
   {$ENDIF}

   function DwgLeader.parseDwg(
     const _version : DWG_Version;
     const _buf     : DwgBuffer;
     const _bs      : duint32
   ) : Boolean;
  var
    sBuff, sBuf : DwgBuffer;
    i           : Integer ;
    vertex,
    endptproj,
    unk         : DwgCoord ;
  begin
    sBuff := DwgBuffer.Create( _buf );
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then
        sBuf := sBuff;

      Result := inherited parseDwgEx( _version, _buf, sBuf, _bs );

      if not Result then
        exit;

      _buf.getBit;
      _buf.getBitShort;
      _buf.getBitShort;
      vertnum := _buf.getBitLong;

      // add vertexs
      for i := 0 to vertnum-1 do begin
        vertex := _buf.get3BitDouble ;
        vertexlist.Add( vertex );
      end ;

      endptproj := _buf.get3BitDouble ;
      extrusionPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 ));

      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        _buf.getBit;
        _buf.getBit;
        _buf.getBit;
        _buf.getBit;
        _buf.getBit;
      end ;

      horizdir := _buf.get3BitDouble;
      offsetblock := _buf.get3BitDouble;

      if dwgVersionHigher( _version, DWG_Version.AC1012) then begin
        unk := _buf.get3BitDouble;
      end ;
      if dwgVersionLower( _version, DWG_Version.AC1015) then begin
        _buf.getBitDouble;
      end ;
      if dwgVersionLower( _version, DWG_Version.AC1024) then begin
        textheight := _buf.getBitDouble;
        textwidth := _buf.getBitDouble;
      end ;
      hookline := _buf.getBit;
      arrow := _buf.getBit;

      if dwgVersionLower( _version, DWG_Version.AC1015) then begin
        _buf.getBitShort;
        _buf.getBitDouble;
        _buf.getBit;
        _buf.getBit;
        _buf.getBitShort;
        _buf.getBitShort;
        _buf.getBit;
        _buf.getBit;
      end
      else begin
        _buf.getBitShort;
        _buf.getBit;
        _buf.getBit;
      end ;
      Result := inherited parseDwgEntHandle( _version, _buf );

      annotH := _buf.getHandle;
      annotHandle := annotH.ref;
      dimStyleH := _buf.getHandle;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

// ----------------------------------------------------------------------------
// DwgText
// ----------------------------------------------------------------------------

  constructor DwgText.Create;
  begin
    inherited;
    eType       := DWG_ETYPE.TEXT;
    angle       := 0;
    widthscale  := 1;
    oblique     := 0;
    style       := 'STANDARD';
    textgen     := 0;
    alignH      := TGIS_TextHAlign.HLeft;
    alignV      := TGIS_TextVAlign.VBaseLine;
  end ;

  function DwgText.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    sBuff, sBuf : DwgBuffer;
    data_flags  : duint8;
  begin
    sBuff := DwgBuffer.Create( _buf );
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then
        sBuf := sBuff;

      Result := inherited parseDwgEx( _version, _buf, sBuf, _bs );

      if not Result then
        exit;

      data_flags := $00;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        data_flags := _buf.getRawChar8;
        if ( ( data_flags and $01 ) = 0 ) then begin
          basePoint.z := _buf.getRawDouble;
        end ;
      end
      else begin
        basePoint.z := _buf.getBitDouble;
      end ;
      basePoint.x := _buf.getRawDouble;
      basePoint.y := _buf.getRawDouble;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        if ( ( data_flags and $02 ) = 0 ) then begin
          secPoint.x := _buf.getDefaultDouble( basePoint.x );
          secPoint.y := _buf.getDefaultDouble( basePoint.y );
        end
        else begin
          secPoint := basePoint;
        end ;
      end
      else begin
        secPoint.x := _buf.getRawDouble;
        secPoint.y := _buf.getRawDouble;
      end ;
      secPoint.z := basePoint.z;
      extPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 ));
      thickness := _buf.getThickness(dwgVersionHigher( _version, DWG_Version.AC1014 ));

      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        if ( ( data_flags and $04 ) = 0 ) then begin
          oblique := _buf.getRawDouble;
        end ;
        if ( ( data_flags and $08 ) = 0 ) then begin
          angle := _buf.getRawDouble;
        end ;
        height := _buf.getRawDouble;
        if ( ( data_flags and $10 ) = 0 ) then begin
          widthscale := _buf.getRawDouble;
        end ;
      end
      else begin
        oblique := _buf.getBitDouble;
        angle := _buf.getBitDouble;
        height := _buf.getBitDouble;
        widthscale := _buf.getBitDouble;
      end ;
      text := sBuf.getVariableText( _version, false );

      if ( ( data_flags and $20 ) = 0 ) then begin
        textgen := _buf.getBitShort;
      end ;
      if ( ( data_flags and $40 ) = 0 ) then begin
        alignH := TGIS_TextHAlign( _buf.getBitShort );
      end ;
      if ( ( data_flags and $80 ) = 0 ) then begin
        alignV := TGIS_TextVAlign( _buf.getBitShort );
      end ;

      Result := inherited parseDwgEntHandle( _version, _buf );
      if not Result then
        exit;

      styleH := _buf.getHandle;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

// ----------------------------------------------------------------------------
// DwgAttrib
// ----------------------------------------------------------------------------

  constructor DwgAttrib.Create ;
  begin
    inherited ;

    eType := DWG_ETYPE.ATTRIB;
  end ;

  function DwgAttrib.parseDwg(
    const _version  : DWG_Version;
    const _buf      : DwgBuffer;
    const _bs       : duint32
  ) : Boolean ;
  var
    sBuff, sBuf : DwgBuffer;
    data_flags, lock_flag  : duint8;
    ver : duint32 ;
  begin
    sBuff := DwgBuffer.Create( _buf );
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then
        sBuf := sBuff;

      Result := inherited parseDwgEx( _version, _buf, sBuf, _bs );

      if not Result then
        exit;

      data_flags := $00;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        data_flags := _buf.getRawChar8;
        if ( ( data_flags and $01 ) = 0 ) then begin
          basePoint.z := _buf.getRawDouble;
        end ;
      end
      else begin
        basePoint.z := _buf.getBitDouble;
      end ;
      basePoint.x := _buf.getRawDouble;
      basePoint.y := _buf.getRawDouble;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        if ( ( data_flags and $02 ) = 0 ) then begin
          secPoint.x := _buf.getDefaultDouble( basePoint.x );
          secPoint.y := _buf.getDefaultDouble( basePoint.y );
        end
        else begin
          secPoint := basePoint;
        end ;
      end
      else begin
        secPoint.x := _buf.getRawDouble;
        secPoint.y := _buf.getRawDouble;
      end ;
      secPoint.z := basePoint.z;
      extPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 ));
      thickness := _buf.getThickness(dwgVersionHigher( _version, DWG_Version.AC1014 ));

      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        if ( ( data_flags and $04 ) = 0 ) then begin
          oblique := _buf.getRawDouble;
        end ;
        if ( ( data_flags and $08 ) = 0 ) then begin
          angle := _buf.getRawDouble;
        end ;
        height := _buf.getRawDouble;
        if ( ( data_flags and $10 ) = 0 ) then begin
          widthscale := _buf.getRawDouble;
        end ;
      end
      else begin
        oblique := _buf.getBitDouble;
        angle := _buf.getBitDouble;
        height := _buf.getBitDouble;
        widthscale := _buf.getBitDouble;
      end ;
      text := sBuf.getVariableText( _version, false );

      if ( ( data_flags and $20 ) = 0 ) then begin
        textgen := _buf.getBitShort;
      end ;
      if ( ( data_flags and $40 ) = 0 ) then begin
        alignH := TGIS_TextHAlign( _buf.getBitShort );
      end ;
      if ( ( data_flags and $80 ) = 0 ) then begin
        alignV := TGIS_TextVAlign( _buf.getBitShort );
      end ;

      if dwgVersionHigher(_version, DWG_Version.AC1021) then
        ver := _buf.getRawChar8 ;

      tag := sBuf.getVariableText( _version, false );
      fieldLength := _buf.getBitShort ;
      fflags := _buf.getRawChar8 ;

      if dwgVersionHigher(_version, DWG_Version.AC1018) then
        lock_flag := _buf.getBit ;

      Result := inherited parseDwgEntHandle( _version, _buf );
      if not Result then
        exit;

      styleH := _buf.getHandle;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

// ----------------------------------------------------------------------------
// DwgAttDef
// ----------------------------------------------------------------------------

  constructor DwgAttDef.Create ;
  begin
    inherited ;

    eType := DWG_ETYPE.ATTDEF;
  end ;

  function DwgAttDef.parseDwg(
    const _version  : DWG_Version ;
    const _buf      : DwgBuffer ;
    const _bs       : duint32
  ) : Boolean ;
  var
    sBuff, sBuf : DwgBuffer;
    data_flags,
    lock_flag   : duint8;
    ver         : duint32 ;
  begin
    sBuff := DwgBuffer.Create( _buf );
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then
        sBuf := sBuff;

      Result := inherited parseDwgEx( _version, _buf, sBuf, _bs );

      if not Result then
        exit;

      data_flags := $00;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        data_flags := _buf.getRawChar8;
        if ( ( data_flags and $01 ) = 0 ) then begin
          basePoint.z := _buf.getRawDouble;
        end ;
      end
      else begin
        basePoint.z := _buf.getBitDouble;
      end ;
      basePoint.x := _buf.getRawDouble;
      basePoint.y := _buf.getRawDouble;
      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        if ( ( data_flags and $02 ) = 0 ) then begin
          secPoint.x := _buf.getDefaultDouble( basePoint.x );
          secPoint.y := _buf.getDefaultDouble( basePoint.y );
        end
        else begin
          secPoint := basePoint;
        end ;
      end
      else begin
        secPoint.x := _buf.getRawDouble;
        secPoint.y := _buf.getRawDouble;
      end ;
      secPoint.z := basePoint.z;
      extPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 ));
      thickness := _buf.getThickness(dwgVersionHigher( _version, DWG_Version.AC1014 ));

      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        if ( ( data_flags and $04 ) = 0 ) then begin
          oblique := _buf.getRawDouble;
        end ;
        if ( ( data_flags and $08 ) = 0 ) then begin
          angle := _buf.getRawDouble;
        end ;
        height := _buf.getRawDouble;
        if ( ( data_flags and $10 ) = 0 ) then begin
          widthscale := _buf.getRawDouble;
        end ;
      end
      else begin
        oblique := _buf.getBitDouble;
        angle := _buf.getBitDouble;
        height := _buf.getBitDouble;
        widthscale := _buf.getBitDouble;
      end ;
      text := sBuf.getVariableText( _version, false );
      defValue := text ;

      if ( ( data_flags and $20 ) = 0 ) then begin
        textgen := _buf.getBitShort;
      end ;
      if ( ( data_flags and $40 ) = 0 ) then begin
        alignH := TGIS_TextHAlign( _buf.getBitShort );
      end ;
      if ( ( data_flags and $80 ) = 0 ) then begin
        alignV := TGIS_TextVAlign( _buf.getBitShort );
      end ;

      tag := sBuf.getVariableText( _version, false );
      fieldLength := _buf.getBitShort ;
      fflags := _buf.getRawChar8 ;

      if dwgVersionHigher(_version, DWG_Version.AC1018) then
        lock_flag := _buf.getBit ;

      if dwgVersionHigher(_version, DWG_Version.AC1021) then
        ver := _buf.getRawChar8 ;

      prompt := sBuf.getVariableText( _version, false );

      Result := inherited parseDwgEntHandle( _version, _buf );
      if not Result then
        exit;

      styleH := _buf.getHandle;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

// ----------------------------------------------------------------------------
// DwgVertex
// ----------------------------------------------------------------------------

  constructor DwgVertex.Create;
  begin
    inherited;
    eType      := DWG_ETYPE.VERTEX;
    stawidth   := 0;
    endwidth   := 0;
    bulge      := 0;
    vindex1    := 0;
    vindex2    := 0;
    vindex3    := 0;
    vindex4    := 0;
    flag       := 0;
    identifier := 0;
  end ;

  constructor DwgVertex.Create(
    const _sx, _sy, _sz, _b : Double
  );
  begin
    inherited Create;

    eType       := DWG_ETYPE.VERTEX;
    stawidth    := 0;
    endwidth    := 0;
    bulge       := 0;
    vindex1     := 0;
    vindex2     := 0;
    vindex3     := 0;
    vindex4     := 0;
    flag        := 0;
    identifier  := 0;
    basePoint.x := _sx;
    basePoint.y := _sy;
    basePoint.z := _sz;
    bulge       := _b;
  end ;

  function DwgVertex.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  begin
    Result := true;
  end ;

  function DwgVertex.parseDwgEx(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32;
    const _el      : Double
  ) : Boolean;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    if ( oType = $0A ) then begin
      flag := _buf.getRawChar8;
      basePoint := _buf.get3BitDouble;
      basePoint.z := _el;
      stawidth := _buf.getBitDouble;
      if ( stawidth < 0 ) then begin
        endwidth := Abs( stawidth );
        stawidth := Abs( stawidth )
      end
      else
        endwidth := _buf.getBitDouble;
      bulge := _buf.getBitDouble;
      if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
        _buf.getBitLong ;
      end ;
      tgdir := _buf.getBitDouble;
    end
    else if ( oType = $0B ) or ( oType = $0C ) or ( oType = $0D ) then begin
      flag := _buf.getRawChar8;
      basePoint := _buf.get3BitDouble;
    end
    else if ( oType = $0E ) then begin
      // PFACE FACE
      vindex1 := _buf.getBitShort;
      vindex2 := _buf.getBitShort;
      vindex3 := _buf.getBitShort;
      vindex4 := _buf.getBitShort;
    end ;

    Result := inherited parseDwgEntHandle( _version, _buf );
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgPolyline
// ----------------------------------------------------------------------------

  constructor DwgPolyline.Create;
  begin
    inherited;
    eType       := DWG_ETYPE.POLYLINE;
    defstawidth := 0.0;
    defendwidth := 0.0;
    basePoint.x := 0.0;
    basePoint.y := 0.0;
    flag        := 0;
    vertexcount := 0;
    facecount   := 0;
    smoothM     := 0;
    smoothN     := 0;
    curvetype   := 0;
    hadlesList  := TList<duint64>.Create;
    vertlist    := TList<DwgVertex>.Create;
  end ;

  {$IFDEF DCC}
  destructor DwgPolyline.Destroy;
  var
    v : DwgVertex ;
  begin
    for v in vertlist do
      FreeObjectNotNil( v ) ;

    FreeObject( hadlesList ) ;
    FreeObject( vertlist ) ;
    inherited ;
  end ;
  {$ENDIF}

  function DwgPolyline.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    ooCount : dint32;
    i       : Integer;
    tmpFlag : duint8;
    objectH : DwgHandle;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    ooCount := 0;
    if ( oType = $0F ) then begin
      // pline 2D
      flag := _buf.getBitShort;
      curvetype := _buf.getBitShort;
      defstawidth := _buf.getBitDouble;
      defendwidth := _buf.getBitDouble;
      thickness := _buf.getThickness(dwgVersionHigher( _version, DWG_Version.AC1014 ));
      basePoint := DwgCoord.Create( 0, 0, _buf.getBitDouble );
      extPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 ));
    end
    else if ( oType = $10 ) then begin
      // pline 3D
      tmpFlag := _buf.getRawChar8;
      if ( tmpFlag and 1 ) <> 0 then
        curvetype := 5
      else if ( tmpFlag and 2 ) <> 0 then
        curvetype := 6;
      if ( tmpFlag and 3 ) <> 0 then begin
        curvetype := 8;
        flag := flag or 4;
      end ;
      tmpFlag := _buf.getRawChar8;
      if ( tmpFlag and 1 ) <> 0 then
        flag := flag or 1;
      flag := flag or 8;
    end
    else if ( oType = $1D ) then begin
      // PFACE
      flag := 64;
      vertexcount := _buf.getBitShort;
      facecount := _buf.getBitShort;
    end
    else if ( oType = 30 ) then begin
      // MESH
      flag := _buf.getBitShort;
      curvetype := _buf.getBitShort;
      mvertexcount := _buf.getBitShort;
      nvertexcount := _buf.getBitShort;
      mdensity := _buf.getBitShort;
      ndensity := _buf.getBitShort;
    end ;

    if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
      ooCount := _buf.getBitLong;
    end ;

    Result := inherited parseDwgEntHandle( _version, _buf );
    if not Result then
      exit;

    if dwgVersionLower( _version, DWG_Version.AC1018 ) then begin
      objectH := _buf.getOffsetHandle( handle );
      firstEH := objectH.ref;
      objectH := _buf.getOffsetHandle( handle );
      lastEH := objectH.ref;
    end
    else begin
      if ooCount > 0 then
        for i := 0 to ooCount - 1 do begin
          objectH := _buf.getOffsetHandle( handle );
          hadlesList.Add( objectH.ref );
        end ;
    end ;
    seqendH := _buf.getOffsetHandle( handle );

    Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgMText
// ----------------------------------------------------------------------------

  constructor DwgMText.Create;
  begin
    inherited Create;
    eType     := DWG_ETYPE.MTEXT;
    textgen   := 1;
  end ;

  function DwgMText.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    draw_dir  : dint16 ;
    ext_ht    : Double ;
    ext_wid   : Double ;
    sBuff,
    sBuf      : DwgBuffer;
    bk_flags  : dint32 ;
  begin
    sBuff := DwgBuffer.Create( _buf );
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then
        sBuf := sBuff;

      Result := inherited parseDwgEx( _version, _buf, sBuf, _bs );
      if not Result then
        exit;

      basePoint := _buf.get3BitDouble;
      extPoint := _buf.get3BitDouble;
      secPoint := _buf.get3BitDouble;
      if ( secPoint.x <> 0 ) then begin
        if Abs( secPoint.x ) < 1.0e-6 then begin
          if ( secPoint.y > 0.0 ) then
            angle := Pi/2.0
           else
            angle := Pi/2.0*3.0
        end
        else
          angle := ArcTan( secPoint.y/secPoint.x ) ;
      end ;
      widthscale := _buf.getBitDouble;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then begin
        _buf.getBitDouble;
      end ;
      height := _buf.getBitDouble;
      textgen := _buf.getBitShort;
      draw_dir := _buf.getBitShort;
      ext_ht := _buf.getBitDouble;
      ext_wid := _buf.getBitDouble;
      text := sBuf.getVariableText( _version, false );

      if dwgVersionHigher( _version, DWG_Version.AC1014 ) then begin
        _buf.getBitShort;
        _buf.getBitDouble;
        _buf.getBit;
      end ;
      if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
        bk_flags := _buf.getBitLong;
        if ( bk_flags = 1 ) then begin
          _buf.getBitLong;
          _buf.getCmColor( _version );
          _buf.getBitLong;
        end ;
      end ;
      Result := inherited parseDwgEntHandle( _version, _buf );
      if not Result then
        exit;

      styleH := _buf.getHandle;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

// ----------------------------------------------------------------------------
// DwgRay
// ----------------------------------------------------------------------------

  constructor DwgRay.Create;
  begin
    inherited ;
    eType := DWG_ETYPE.RAY ;
  end ;

  function DwgRay.parseDwg(
    const _version  : DWG_Version;
    const _buf      : DwgBuffer;
    const _bs       : duint32
  ) : Boolean ;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    basePoint.x := _buf.getBitDouble;
    basePoint.y := _buf.getBitDouble;
    basePoint.z := _buf.getBitDouble;
    secPoint.x := _buf.getBitDouble;
    secPoint.y := _buf.getBitDouble;
    secPoint.z := _buf.getBitDouble;

    Result := inherited parseDwgEntHandle(_version, _buf);
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgXline
// ----------------------------------------------------------------------------

  constructor DwgXline.Create;
  begin
    inherited ;
    eType := DWG_ETYPE.XLINE ;
  end ;

// ----------------------------------------------------------------------------
// DwgTrace
// ----------------------------------------------------------------------------

  constructor DwgTrace.Create;
  begin
    inherited ;

    eType        := DWG_ETYPE.TRACE ;
    {$IFDEF GIS_NORECORDS}
      thirdPoint := DwgCoord.Create( 0, 0, 0 ) ;
      fourPoint  := DwgCoord.Create( 0, 0, 0 ) ;
    {$ENDIF}
    thirdPoint.z := 0;
    fourPoint.z  := 0;
  end ;

  function DwgTrace.parseDwg(
    const _version  : DWG_Version;
    const _buf      : DwgBuffer;
    const _bs       : duint32
  ) : Boolean;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    thickness   := _buf.getThickness(dwgVersionHigher( _version, DWG_Version.AC1014 ));
    basePoint.z := _buf.getBitDouble;
    basePoint.x := _buf.getRawDouble;
    basePoint.y := _buf.getRawDouble;
    secPoint.x  := _buf.getRawDouble;
    secPoint.y  := _buf.getRawDouble;
    secPoint.z  := basePoint.z;
    thirdPoint.x := _buf.getRawDouble;
    thirdPoint.y := _buf.getRawDouble;
    thirdPoint.z := basePoint.z;
    fourPoint.x := _buf.getRawDouble;
    fourPoint.y := _buf.getRawDouble;
    fourPoint.z := basePoint.z;
    extPoint := _buf.getExtrusion(dwgVersionHigher( _version, DWG_Version.AC1014 ));

    Result := inherited parseDwgEntHandle(_version, _buf);
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgSolid
// ----------------------------------------------------------------------------

  constructor DwgSolid.Create;
  begin
    inherited ;

    eType := DWG_ETYPE.SOLID ;
  end ;

  function DwgSolid.parseDwg(
    const _version  : DWG_Version;
    const _buf      : DwgBuffer;
    const _bs       : duint32
  ): Boolean;
  begin
    Result := inherited parseDwg( _version, _buf, _bs ) ;
  end ;

// ----------------------------------------------------------------------------
// Dwg3Dface
// ----------------------------------------------------------------------------

  constructor Dwg3Dface.Create;
  begin
    inherited ;

    eType := DWG_ETYPE.E3DFACE ;
    invisibleflag := 0;
  end ;

  function Dwg3Dface.parseDwg(
    const _version  : DWG_Version;
    const _buf      : DwgBuffer;
    const _bs       : duint32
  ): Boolean;
  var
    has_no_flag,
    z_is_zero   : duint8 ;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    if dwgVersionLower( _version, DWG_Version.AC1015 ) then begin
      basePoint.x := _buf.getBitDouble;
      basePoint.y := _buf.getBitDouble;
      basePoint.z := _buf.getBitDouble;
      secPoint.x := _buf.getBitDouble;
      secPoint.y := _buf.getBitDouble;
      secPoint.z := _buf.getBitDouble;
      thirdPoint.x := _buf.getBitDouble;
      thirdPoint.y := _buf.getBitDouble;
      thirdPoint.z := _buf.getBitDouble;
      fourPoint.x := _buf.getBitDouble;
      fourPoint.y := _buf.getBitDouble;
      fourPoint.z := _buf.getBitDouble;
      invisibleflag := _buf.getBitShort;
    end
    else begin
      has_no_flag := _buf.getBit;
      z_is_zero := _buf.getBit;
      basePoint.x := _buf.getRawDouble;
      basePoint.y := _buf.getRawDouble;
      if z_is_zero = 1 then
        basePoint.z := 0.0
      else
        basePoint.z := _buf.getRawDouble;
      secPoint.x := _buf.getDefaultDouble(basePoint.x);
      secPoint.y := _buf.getDefaultDouble(basePoint.y);
      secPoint.z := _buf.getDefaultDouble(basePoint.z);
      thirdPoint.x := _buf.getDefaultDouble(secPoint.x);
      thirdPoint.y := _buf.getDefaultDouble(secPoint.y);
      thirdPoint.z := _buf.getDefaultDouble(secPoint.z);
      fourPoint.x := _buf.getDefaultDouble(thirdPoint.x);
      fourPoint.y := _buf.getDefaultDouble(thirdPoint.y);
      fourPoint.z := _buf.getDefaultDouble(thirdPoint.z);
      if has_no_flag = 1 then
        invisibleflag := 0
      else
        invisibleflag := _buf.getBitShort;
    end ;

    Result := inherited parseDwgEntHandle(_version, _buf);
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgSpline
// ----------------------------------------------------------------------------

  constructor DwgSpline.Create;
  begin
    inherited ;

    eType       := DWG_ETYPE.SPLINE ;
    flags       := 0;
    nknots      := 0;
    ncontrol    := 0;
    nfit        := 0;
    tolknot     := 0.0000001;
    tolcontrol  := 0.0000001;
    tolfit      := 0.0000001;

    knotslist   := TList<Double>.Create;
    controllist := TList<DwgCoord>.Create;
    fitlist     := TList<DwgCoord>.Create;
    weightslist := TList<Double>.Create;

  end ;

  {$IFDEF DCC}
  destructor DwgSpline.Destroy;
  begin
    FreeObject( knotslist   ) ;
    FreeObject( controllist ) ;
    FreeObject( fitlist     ) ;
    FreeObject( weightslist ) ;

    inherited ;
  end ;
  {$ENDIF}

  function DwgSpline.parseDwg(
    const _version : DWG_Version;
    const _buf     : DwgBuffer;
    const _bs      : duint32
  ) : Boolean;
  var
    weight    : duint8;
    scenario,
    splFlag1,
    knotParam : dint32;
    i         : Integer;
    bclosed   : Boolean ;
  begin
    Result := inherited parseDwgEx( _version, _buf, nil, _bs );
    if not Result then
      exit;

    weight := 0;
    bclosed := False ;
    scenario := _buf.getBitLong;
    if dwgVersionHigher( _version, DWG_Version.AC1024 ) then begin
      splFlag1 := _buf.getBitLong;
      if ( splFlag1 and 1 ) <> 0 then
        scenario := 2;
      if ( splFlag1 and 4 ) = 4 then
        bclosed := True ;
      knotParam := _buf.getBitLong;
      if knotParam = 15 then
        scenario := 1 ;
    end ;

    degree := _buf.getBitLong;
    if ( scenario = 2 ) then begin
      flags := 8 ;
      if bclosed then
        flags := flags or 1 ;
      tolfit := _buf.getBitDouble;
      tgStart := _buf.get3BitDouble;
      tgEnd := _buf.get3BitDouble;
      nfit := _buf.getBitLong;
    end
    else if ( scenario = 1 ) then begin
      flags := 8;
      flags := flags or _buf.getBit shl 2;
      flags := flags or _buf.getBit;
      flags := flags or _buf.getBit shl 1;
      tolknot := _buf.getBitDouble;
      tolcontrol := _buf.getBitDouble;
      nknots := _buf.getBitLong;
      ncontrol := _buf.getBitLong;
      weight := _buf.getBit;
    end
    else begin
      Result := false;
      exit;
    end ;

    {$IFNDEF JAVA OR ISLAND}
    knotslist.Capacity := nknots;
    {$ENDIF}
    for i := 0 to nknots - 1 do begin
      knotslist.Add( _buf.getBitDouble );
    end ;

    {$IFNDEF JAVA OR ISLAND}
    controllist.Capacity := ncontrol;
    {$ENDIF}
    for i := 0 to ncontrol - 1 do begin
      controllist.Add( _buf.get3BitDouble );
      if ( weight <> 0 ) then begin
        weightslist.Add( _buf.getBitDouble ) ;
      end ;
    end ;
    {$IFNDEF JAVA OR ISLAND}
    fitlist.Capacity := nfit;
    {$ENDIF}
    for i := 0 to nfit - 1 do
      fitlist.Add( _buf.get3BitDouble );

    Result := inherited parseDwgEntHandle( _version, _buf );
    if not Result then
      exit
    else
      Result := _buf.isGood;
  end ;

// ----------------------------------------------------------------------------
// DwgHatchLoop
// ----------------------------------------------------------------------------

  constructor DwgHatchLoop.Create(
    const _t : dint32
  ) ;
  begin
    inherited Create;

    typ      := _t;
    numedges := 0;
    objlist  := TList<DwgEntity>.Create ;
  end ;

  {$IFDEF DCC}
  destructor DwgHatchLoop.Destroy;
  var
    e : DwgEntity ;
  begin
    for e in objlist do
      FreeObjectNotNil( e ) ;
    FreeObject( objlist ) ;
    inherited ;
  end ;
  {$ENDIF}

  procedure DwgHatchLoop.update;
  begin
    numedges := objlist.Count ;
  end ;

// ----------------------------------------------------------------------------
// DwgHatch
// ----------------------------------------------------------------------------

  constructor DwgHatch.Create;
  begin
    inherited ;

    eType       := DWG_ETYPE.HATCH;
    angle       := 0.0;
    scale       := 0.0;
    basePoint.x := 0.0;
    basePoint.y := 0.0;
    basePoint.z := 0.0;
    loopsnum    := 0;
    hstyle      := 0;
    associative := 0;
    solid       := 1;
    hpattern    := 1;
    deflines    := 0;
    doubleflag  := 0;
    looplist    := TList<DwgHatchLoop>.Create ;
  end ;

  {$IFDEF DCC}
  destructor DwgHatch.Destroy;
  var
    lp : DwgHatchLoop ;
  begin
    for lp in looplist do
      FreeObjectNotNil( lp ) ;
    FreeObject( looplist ) ;
    inherited ;
  end ;
  {$ENDIF}

  procedure DwgHatch.appendLoop(
    const _v: DwgHatchLoop
  ) ;
  begin
    looplist.Add(_v) ;
  end ;

  function DwgHatch.parseDwg(
    const _version  : DWG_Version;
    const _buf      : DwgBuffer;
    const _bs       : duint32
  ) : Boolean;
  var
    sBuff, sBuf    : DwgBuffer;
    totalBoundItems: duint32;
    havePixelSize  : Boolean;
    i, j, k        : Integer;
    isGradient     : dint32;
    res            : dint32;
    gradAngle      : Double;
    gradShift      : Double;
    singleCol      : dint32;
    gradTint       : Double;
    numCol         : dint32;
    unkDouble      : Double;
    unkShort       : duint16;
    rgbCol         : dint32;
    ignCol         : duint8;
    gradName       : String;
    cName          : String;
    hloop          : DwgHatchLoop;
    line           : DwgLine;
    arc            : DwgArc;
    ellipse        : DwgEllipse;
    spline         : DwgSpline;
    pline          : DwgLWPolyline;
    pt             : DwgPoint;
    plvert         : DwgVertex2D;
    numPathSeg     : dint32 ;
    typePath       : duint8 ;
    isRational     : duint8 ;
    asBulge        : duint8 ;
    crd            : DwgCoord ;
    numVert        : dint32 ;
    v              : DwgVertex2D ;
    ptL, offL      : DwgCoord ;
    seedPt         : DwgCoord ;
    angleL         : Double ;
    numDashL       : duint16 ;
    lenghtL        : Double ;
    pixsize        : ddouble64 ;
    numSeedPoints  : dint32 ;
    biH            : DwgHandle ;
  begin
    {$IFDEF GIS_NORECORDS}
      ptL    := new DwgCoord(0, 0, 0) ;
      offL   := new DwgCoord(0, 0, 0) ;
      seedPt := new DwgCoord(0, 0, 0) ;
    {$ENDIF}

    sBuff := DwgBuffer.Create( _buf );
    try
      sBuf := _buf;
      if dwgVersionHigher( _version, DWG_Version.AC1018 ) then
        sBuf := sBuff;

      Result := inherited parseDwgEx( _version, _buf, sBuf, _bs );

      if not Result then
        exit;

      totalBoundItems := 0;
      havePixelSize  := false;
      hloop          := nil ;
      line           := nil ;
      arc            := nil ;
      ellipse        := nil ;
      spline         := nil ;
      pline          := nil ;
      pt             := nil ;
      plvert         := nil ;

      if dwgVersionHigher( _version, DWG_Version.AC1015 ) then begin
        isGradient := _buf.getBitLong;
        res        := _buf.getBitLong;
        gradAngle  := _buf.getBitDouble;
        gradShift  := _buf.getBitDouble;
        singleCol  := _buf.getBitLong;
        gradTint   := _buf.getBitDouble;
        numCol     := _buf.getBitLong;

        for i := 0 to numCol - 1 do begin
          unkDouble := _buf.getBitDouble;
          unkShort  := _buf.getBitShort;
          rgbCol    := _buf.getBitLong;
          ignCol    := _buf.getRawChar8;
          if (ignCol and 1) = 1 then begin
            cName := sBuf.getVariableText( _version, false ); // color name
          end ;
          if (ignCol and 2) = 2 then begin
            cName := sBuf.getVariableText( _version, false ); // book name
          end ;
        end ;
        gradName := sBuf.getVariableText( _version, false );
      end ;

      basePoint.z := _buf.getBitDouble;
      extPoint    := _buf.get3BitDouble;
      name        := sBuf.getVariableText( _version, false );
      solid       := _buf.getBit;
      associative := _buf.getBit;
      loopsnum    := _buf.getBitLong;

      // read loops
      for i := 0 to loopsnum - 1 do begin
        hloop := DwgHatchLoop.Create( _buf.getBitLong );
        havePixelSize := havePixelSize or ( ( hloop.typ and 4 ) <> 0 );
        if ( ( hloop.typ and 2 ) = 0 ) then begin // Not polyline
          numPathSeg := _buf.getBitLong;
          for j := 0 to numPathSeg - 1 do begin
            typePath := _buf.getRawChar8;
            if ( typePath = 1 ) then begin // line
              line := DwgLine.Create;
              hloop.objlist.Add( line );
              line.basePoint := _buf.get2RawDouble;
              line.secPoint := _buf.get2RawDouble;
            end
            else if ( typePath = 2 ) then begin // circle arc
              arc := DwgArc.Create;
              hloop.objlist.Add( arc );
              arc.basePoint := _buf.get2RawDouble;
              arc.radious := _buf.getBitDouble;
              arc.staangle := _buf.getBitDouble;
              arc.endangle := _buf.getBitDouble;
              arc.isccw := _buf.getBit;
            end
            else if ( typePath = 3 ) then begin // ellipse arc
              ellipse := DwgEllipse.Create;
              hloop.objlist.Add( ellipse );
              ellipse.basePoint := _buf.get2RawDouble;
              ellipse.secPoint := _buf.get2RawDouble;
              ellipse.ratio := _buf.getBitDouble;
              ellipse.staparam := _buf.getBitDouble;
              ellipse.endparam := _buf.getBitDouble;
              ellipse.isccw := _buf.getBit;
            end
            else if ( typePath = 4 ) then begin // spline
              spline := DwgSpline.Create;
              hloop.objlist.Add( spline );
              spline.degree := _buf.getBitLong;
              isRational := _buf.getBit;
              spline.flags := spline.flags or ( isRational shl 2 );
              // rational
              spline.flags := spline.flags or ( _buf.getBit shl 1 );
              // periodic
              spline.nknots := _buf.getBitLong;
              {$IFNDEF JAVA OR ISLAND}
              spline.knotslist.Capacity := spline.nknots;
              {$ENDIF}
              spline.ncontrol := _buf.getBitLong;
              {$IFNDEF JAVA OR ISLAND}
              spline.controllist.Capacity := spline.ncontrol;
              {$ENDIF}
              for k := 0 to spline.nknots - 1 do begin
                spline.knotslist.Add( _buf.getBitDouble );
              end ;
              for k := 0 to spline.ncontrol - 1 do begin
                crd := _buf.get2RawDouble;
                crd.z := 0 ;
                if ( isRational <> 0 ) then
                  spline.weightslist.Add( _buf.getBitDouble ) ;
                spline.controllist.Add( crd );
              end ;
              if dwgVersionHigher( _version, DWG_Version.AC1021 ) then begin
                spline.nfit := _buf.getBitLong;
                if spline.nfit > 0 then begin
                  {$IFNDEF JAVA OR ISLAND}
                  spline.fitlist.Capacity := spline.nfit;
                  {$ENDIF}
                  for k := 0 to spline.nfit - 1 do begin
                    crd := _buf.get2RawDouble;
                    crd.z := 0 ;
                    spline.fitlist.Add( crd );
                  end ;
                  spline.tgStart := _buf.get2RawDouble;
                  spline.tgEnd := _buf.get2RawDouble;
                end ;
              end ;
            end ;
          end ;
        end
        else begin // end not pline, start polyline
          pline := DwgLWPolyline.Create;
          asBulge := _buf.getBit;
          pline.bulgesnum := asBulge ;
          pline.flag := _buf.getBit;
          numVert := _buf.getBitLong;
          pline.vertexnum := numVert ;
          for j := 0 to numVert - 1 do begin
            v := DwgVertex2D.Create;
            v.x := _buf.getRawDouble;
            v.y := _buf.getRawDouble;
            if ( asBulge <> 0 ) then
              v.bulge := _buf.getBitDouble;
            pline.vertlist.Add( v );
          end ;
          hloop.objlist.Add( pline );
        end ; // end polyline
        hloop.update;
        looplist.Add( hloop );
        totalBoundItems := totalBoundItems + _buf.getBitLong;
      end ; // end read loops

      hstyle := _buf.getBitShort;
      hpattern := _buf.getBitShort;
      if ( solid = 0 ) then begin
        angle := _buf.getBitDouble;
        scale := _buf.getBitDouble;
        doubleflag := _buf.getBit;
        deflines := _buf.getBitShort;
        for i := 0 to deflines - 1 do begin
          angleL := _buf.getBitDouble;
          ptL.x := _buf.getBitDouble;
          ptL.y := _buf.getBitDouble;
          offL.x := _buf.getBitDouble;
          offL.y := _buf.getBitDouble;
          numDashL := _buf.getBitShort;
          for j := 0 to numDashL - 1 do begin
            lenghtL := _buf.getBitDouble;
          end ;
        end ;
      end ;

      if ( havePixelSize ) then begin
        pixsize := _buf.getBitDouble;
      end ;
      numSeedPoints := _buf.getBitLong;

      for i := 0 to numSeedPoints - 1 do begin
        seedPt.x := _buf.getRawDouble;
        seedPt.y := _buf.getRawDouble;
      end ;

      Result := inherited parseDwgEntHandle( _version, _buf );
      if not Result then
        exit
      else
        Result := _buf.isGood;

      if totalBoundItems > 0 then
        for i := 0 to totalBoundItems - 1 do begin
          biH := _buf.getHandle;
        end ;

      Result := _buf.isGood;
    finally
      FreeObject( sBuff ) ;
    end ;
  end ;

// ----------------------------------------------------------------------------
// DWG_ReaderCallBack
// ----------------------------------------------------------------------------

  constructor DwgReaderCallBack.Create(
    const _rdr   : TObject ;
    const _model : T_FileDwg_Model
  ) ;
  begin
    inherited Create ;

    rdr       := _rdr ;
    model     := _model ;
    inBlock   := False ;
    currBlock := nil ;
  end ;

  procedure DwgReaderCallBack.checkBlock ;
  begin
    if inBlock then
      dwg_log( '  ' ) ;
  end ;

  function DwgReaderCallBack.numberToColor(
    const _color : DwgColor
  ) : T_FileDwg_Color ;
  begin
    if ( _color.CType = DwgColorType.ByBlock ) then
      Result := T_FileDwg_Color.Create( T_FileDwg_Flag.ByBlock )
    else if ( _color.CType = DwgColorType.ByLayer ) then
      Result := T_FileDwg_Color.Create( T_FileDwg_Flag.ByLayer )
    else if ( _color.CType = DwgColorType.ByRGB ) then
      Result := T_FileDwg_Color.Create( TGIS_Color.FromRGB( _color.Value ) )
    else if ( _color.CType = DwgColorType.ByIndex ) then begin
      if (_color.Value >=0) and (_color.Value <=255) then
      Result := T_FileDwg_Color.Create( DWG_COLORS[_color.Value][0],
                                      DWG_COLORS[_color.Value][1],
                                      DWG_COLORS[_color.Value][2]
                                     )
      else
        Result := T_FileDwg_Color.Create( TGIS_Color.White )
    end
    else if ( _color.CType = DwgColorType.ByReference ) then
      Result := T_FileDwg_Color.Create( T_FileDwg_Flag.ByReference, _color.Value )
    else
      Result := T_FileDwg_Color.Create( T_FileDwg_Flag.ByLayer )
  end ;

  function DwgReaderCallBack.nameToLineType(
    const _name : String
  ) : T_FileDwg_Pen ;
  var
    lname : String ;
  begin
    lname := UpperCase( _name ) ;

    if IsStringEmpty( lname ) or (lname = 'BYLAYER') then
      Result := T_FileDwg_Pen.Create( T_FileDwg_Flag.ByLayer )
    else if (lname = 'BYBLOCK') then
      Result := T_FileDwg_Pen.Create( T_FileDwg_Flag.ByBlock )
    else if (lname = 'CONTINUOUS') or (lname = 'ACAD_ISO01W100') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Solid )
    else if (lname = 'ACAD_ISO07W100') or (lname = 'DOT') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dot )
    else if (lname = 'DOTTINY') or (lname = 'ISO DOT') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dot )
    else if (lname = 'DOT2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dot )
    else if (lname = 'DOTX2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dot )
    else if (lname = 'ACAD_ISO02W100') or (lname = 'ACAD_ISO03W100') or
            (lname = 'DASHED') or (lname = 'HIDDEN') or (lname = 'ISO DASH') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dash )
    else if (lname = 'DASHEDTINY') or (lname = 'HIDDEN2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dash )
    else if (lname = 'DASHED2') or (lname = 'HIDDEN2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dash )
    else if (lname = 'DASHEDX2') or (lname = 'HIDDENX2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dash )
    else if (lname = 'ACAD_ISO10W100') or (lname = 'DASHDOT') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDot )
    else if (lname = 'DASHDOTTINY') or (lname = 'ISO DASH DOT') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDot )
    else if (lname = 'DASHDOT2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDot )
    else if (lname = 'ACAD_ISO04W100') or (lname = 'DASHDOTX2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDot )
    else if (lname = 'ACAD_ISO12W100') or (lname = 'DIVIDE') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDotDot )
    else if (lname = 'DIVIDETINY') or (lname = 'ISO DASH Double-DOT') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDotDot )
    else if (lname = 'DIVIDE2') or (lname = 'ISO DASH TRIPLE-DOT') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDotDot )
    else if (lname = 'ACAD_ISO05W100') or (lname = 'DIVIDEX2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDotDot )
    else if (lname = 'CENTER') or (lname = 'ISO DASH LONG GAPS') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dash )
    else if (lname = 'CENTERTINY') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dash )
    else if (lname = 'CENTER2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dash )
    else if (lname = 'CENTERX2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Dash )
    else if (lname = 'BORDER') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDot )
    else if (lname = 'BORDERTINY') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDot )
    else if (lname = 'BORDER2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDot )
    else if (lname = 'BORDERX2') then
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.DashDot )
    else
      Result := T_FileDwg_Pen.Create( TGIS_PenStyle.Solid ) ;
  end ;

  function DwgReaderCallBack.numberToWidth(
    const _lw : Integer
  ) : T_FileDwg_Width ;
  begin
    case _lw of
      00 : Result := T_FileDwg_Width.Create( 0   ) ;     //  1.  (0.00mm)
      01 : Result := T_FileDwg_Width.Create( 5   ) ;     //  2.  (0.05mm)
      02 : Result := T_FileDwg_Width.Create( 9   ) ;     //  3.  (0.09mm)
      03 : Result := T_FileDwg_Width.Create( 13  ) ;     //  4.  (0.13mm)
      04 : Result := T_FileDwg_Width.Create( 15  ) ;     //  5.  (0.15mm)
      05 : Result := T_FileDwg_Width.Create( 18  ) ;     //  6.  (0.18mm)
      06 : Result := T_FileDwg_Width.Create( 20  ) ;     //  7.  (0.20mm)
      07 : Result := T_FileDwg_Width.Create( 25  ) ;     //  8.  (0.25mm)
      08 : Result := T_FileDwg_Width.Create( 30  ) ;     //  9.  (0.30mm)
      09 : Result := T_FileDwg_Width.Create( 35  ) ;     //  10. (0.35mm)
      10 : Result := T_FileDwg_Width.Create( 40  ) ;     //  11. (0.40mm)
      11 : Result := T_FileDwg_Width.Create( 50  ) ;     //  12. (0.50mm)
      12 : Result := T_FileDwg_Width.Create( 53  ) ;     //  13. (0.53mm)
      13 : Result := T_FileDwg_Width.Create( 60  ) ;     //  14. (0.60mm)
      14 : Result := T_FileDwg_Width.Create( 70  ) ;     //  15. (0.70mm)
      15 : Result := T_FileDwg_Width.Create( 80  ) ;     //  16. (0.80mm)
      16 : Result := T_FileDwg_Width.Create( 90  ) ;     //  17. (0.90mm)
      17 : Result := T_FileDwg_Width.Create( 100 ) ;     //  18. (1.00mm)
      18 : Result := T_FileDwg_Width.Create( 106 ) ;     //  19. (1.06mm)
      19 : Result := T_FileDwg_Width.Create( 120 ) ;     //  20. (1.20mm)
      20 : Result := T_FileDwg_Width.Create( 140 ) ;     //  21. (1.40mm)
      21 : Result := T_FileDwg_Width.Create( 158 ) ;     //  22. (1.58mm)
      22 : Result := T_FileDwg_Width.Create( 200 ) ;     //  23. (2.00mm)
      23 : Result := T_FileDwg_Width.Create( 211 ) ;     //  24. (2.11mm)
      29 : Result := T_FileDwg_Width.Create( T_FileDwg_Flag.ByLayer ) ;
      30 : Result := T_FileDwg_Width.Create( T_FileDwg_Flag.ByBlock ) ;
      31 : Result := T_FileDwg_Width.Create( T_FileDwg_Flag.Default )
    else   Result := T_FileDwg_Width.Create( _lw )
    end ;
  end ;

  procedure DwgReaderCallBack.setEntityStyle(
    const _entity : T_FileDwg_Entity ;
    const _attrib : DwgEntity
  ) ;
  begin
    _entity.Style.Color := T_FileDwg_Color.Create( TGIS_Color.Black ) ;
    _entity.Style.Pen   := T_FileDwg_Pen.Create( TGIS_PenStyle.Solid ) ;
    _entity.Style.Width := T_FileDwg_Width.Create( 1 ) ;
    _entity.Layer       := model.FindLayer( _attrib.layer ) ;
    _entity.Style.Color := numberToColor( _attrib.color ) ;
    _entity.Style.Pen   := nameToLineType( _attrib.lineType ) ;
    _entity.Style.Width := numberToWidth ( _attrib.lWeight  ) ;

    if assigned( _attrib.extData ) then begin
      _entity.ExtData := TStringList.Create ;
      _entity.ExtData.AddStrings( _attrib.extData ) ;
    end ;
  end ;

  procedure DwgReaderCallBack.add3dFace(
    const _data : Dwg3Dface
  );
  var
    face : T_FileDwg_3DFace ;
  begin
    checkBlock ;
    dwg_log( '3DFACE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    face := T_FileDwg_3DFace.Create( _data.handle, T_FileDwg_EntityType.Face3D ) ;
    face.Ptg1      := _data.basePoint.ToPoint ;
    face.Ptg2      := _data.secPoint.ToPoint ;
    face.Ptg3      := _data.thirdPoint.ToPoint ;
    face.Ptg4      := _data.fourPoint.ToPoint ;
    face.Extrusion := _data.extPoint.ToPoint ;
    face.InvisibleFlag := _data.invisibleflag ;

    setEntityStyle( face, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( face ) ;
  end ;

  procedure DwgReaderCallBack.addAppId(
    const _data : DwgAppId
  );
  begin
    dwg_log( 'APPID  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');
  end ;

  procedure DwgReaderCallBack.addArc(
    const _data : DwgArc
  );
  var
    arc : T_FileDwg_Arc ;
  begin
    checkBlock ;
    dwg_log( 'ARC  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    arc := T_FileDwg_Arc.Create( _data.handle, T_FileDwg_EntityType.Arc ) ;
    arc.Center      := _data.basePoint.ToPoint ;
    arc.Radius      := _data.radious ;
    arc.StartAngle  := _data.staangle ;
    arc.EndAngle    := _data.endangle ;
    arc.Extrusion   := _data.extPoint.ToPoint ;

    setEntityStyle( arc, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( arc ) ;
  end ;

  procedure DwgReaderCallBack.addAttDef(
    const _data : DwgAttDef
  ) ;
  var
    att : T_FileDwg_Text ;
  begin
    checkBlock ;
    dwg_log( 'ATTDEF  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    att := T_FileDwg_Text.Create( _data.handle, T_FileDwg_EntityType.AttDef ) ;
    att.InsertionPoint  := _data.basePoint.ToPoint ;
    att.AlignmentPoint  := _data.secPoint.ToPoint ;

    if (_data.alignV <> TGIS_TextVAlign.VBaseLine) or
       (_data.alignH <> TGIS_TextHAlign.HLeft    ) or
       (_data.alignH = TGIS_TextHAlign.HMiddle   ) then begin
       if (_data.alignH <> TGIS_TextHAlign.HAligned) and
          (_data.alignH <> TGIS_TextHAlign.HFit) then begin
           att.AlignmentPoint := _data.basePoint.ToPoint ;
           att.InsertionPoint := _data.secPoint.ToPoint ;
       end ;
    end ;
    att.Text            := _data.tag ;
    att.Angle           := _data.angle ;
    att.Height          := _data.height ;
    att.WidthScale      := _data.widthscale ;
    att.AlignH          := _data.alignH ;
    att.AlignV          := _data.alignV ;
    att.Extrusion       := _data.extPoint.ToPoint ;

    setEntityStyle( att, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( att ) ;
  end ;

  procedure DwgReaderCallBack.addAttrib(
    const _data : DwgAttrib
  ) ;
  var
    att : T_FileDwg_Text ;
  begin
    checkBlock ;
    dwg_log( 'ATTRIB  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    att := T_FileDwg_Text.Create( _data.handle, T_FileDwg_EntityType.Attrib ) ;
    att.InsertionPoint  := _data.basePoint.ToPoint ;
    att.AlignmentPoint  := _data.secPoint.ToPoint ;

    if (_data.alignV <> TGIS_TextVAlign.VBaseLine) or
       (_data.alignH <> TGIS_TextHAlign.HLeft    ) or
       (_data.alignH = TGIS_TextHAlign.HMiddle   ) then begin
       if (_data.alignH <> TGIS_TextHAlign.HAligned) and
          (_data.alignH <> TGIS_TextHAlign.HFit) then begin
           att.AlignmentPoint := _data.basePoint.ToPoint ;
           att.InsertionPoint := _data.secPoint.ToPoint ;
       end ;
    end ;
    att.Text            := _data.text ;
    att.Angle           := _data.angle ;
    att.Height          := _data.height ;
    att.WidthScale      := _data.widthscale ;
    att.AlignH          := _data.alignH ;
    att.AlignV          := _data.alignV ;
    att.Extrusion       := _data.extPoint.ToPoint ;

    setEntityStyle( att, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( att ) ;

  end ;

  procedure DwgReaderCallBack.addBlock(
    const _data : DwgBlock
  );
  var
    blk   : T_FileDwg_Block ;
    bname : String ;
  begin
    inBlock := True ;
    dwg_log( 'BLOCK "' ) ; dwg_log( _data.name ) ;
    dwg_log( '"  ' ) ;dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    bname := UpperCase( _data.name ) ;
    if (bname <> '*MODEL_SPACE') and (bname <> '*PAPER_SPACE') then begin
      blk := T_FileDwg_Block.Create( _data.parentHandle, T_FileDwg_EntityType.Block )  ;
      blk.Name      := _data.name ;
      blk.BasePoint := _data.basePoint.ToPoint ;
      setEntityStyle( blk, _data ) ;
      if model.AddBlock( blk ) then
        currBlock := blk
      else
        FreeObject( blk ) ;
    end
    else if (bname = '*MODEL_SPACE') then begin
      blk := T_FileDwg_Block.Create( _data.handle, T_FileDwg_EntityType.Block )  ;
      blk.Name      := _data.name ;
      blk.BasePoint := _data.basePoint.ToPoint ;
      setEntityStyle( blk, _data ) ;
      model.SetModelBlock( blk ) ;
      currBlock := blk ;
    end
    else if (bname = '*PAPER_SPACE') then begin
      blk := T_FileDwg_Block.Create( _data.handle, T_FileDwg_EntityType.Block )  ;
      blk.Name      := _data.name ;
      blk.BasePoint := _data.basePoint.ToPoint ;
      setEntityStyle( blk, _data ) ;
      model.AddBlock( blk ) ;
      currBlock := blk ;
    end ;
  end ;

  procedure DwgReaderCallBack.addCircle(
    const _data : DwgCircle
  );
  var
    circle : T_FileDwg_Circle ;
  begin
    checkBlock ;
    dwg_log( 'CIRCLE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    circle := T_FileDwg_Circle.Create( _data.handle, T_FileDwg_EntityType.Circle ) ;
    circle.Center    := _data.basePoint.ToPoint ;
    circle.Radius    := _data.radious ;
    circle.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( circle, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( circle ) ;

  end ;

  procedure DwgReaderCallBack.addDimStyle(
    const _data : DwgDimstyle
  );
  begin
    dwg_log( 'DIMSTYLE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');
  end ;

  procedure DwgReaderCallBack.addEllipse(
    const _data : DwgEllipse
  );
  var
    elipse : T_FileDwg_Ellipse ;
  begin
    checkBlock ;
    dwg_log( 'ELLIPSE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    elipse := T_FileDwg_Ellipse.Create( _data.handle, T_FileDwg_EntityType.Ellipse ) ;
    elipse.Center    := _data.basePoint.ToPoint ;
    elipse.MajorAxis := _data.secPoint.ToPoint ;
    elipse.Extrusion := _data.extPoint.ToPoint ;
    elipse.AxisRatio := _data.ratio ;
    elipse.InitAngle := _data.staparam ;
    elipse.EndAngle  := _data.endparam ;

    setEntityStyle( elipse, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( elipse ) ;

  end ;

  procedure DwgReaderCallBack.addHatch(
    const _data : DwgHatch
  );
  var
    hatch : T_FileDwg_Hatch ;
    {$IFDEF DCC}
      hl  : DwgHatchLoop ;
      e   : DwgEntity ;
      hle : T_FileDwg_Entity ;
    {$ENDIF}
    lp : T_FileDwg_HatchLoop ;
    lpContainer : T_FileDwg_Block ;
    oldBlock : T_FileDwg_Block ;
  begin
    checkBlock ;
    dwg_log( 'HATCH  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    hatch := T_FileDwg_Hatch.Create( _data.handle, T_FileDwg_EntityType.Hatch ) ;
    hatch.Extrusion := _data.extPoint.ToPoint ;
    hatch.Solid := _data.solid ;
    hatch.Angle := _data.angle ;
    hatch.Scale := _data.scale ;
    hatch.NumLoops := _data.loopsnum ;

    lpContainer := T_FileDwg_Block.Create( 0, T_FileDwg_EntityType.Block ) ;
    oldBlock := currBlock ;
    try
      currBlock := lpContainer ;
      for hl in _data.looplist do begin
        lp := T_FileDwg_HatchLoop.Create( hl.typ ) ;
        lp.NumEdges := hl.numedges ;

        for e in hl.objlist do begin
          case e.eType of
            DWG_ETYPE.LWPOLYLINE : addLWPolyline( DwgLWPolyline(e) ) ;
            DWG_ETYPE.POLYLINE   : addPolyline( DwgPolyline(e) ) ;
            DWG_ETYPE.LINE       : addLine    ( DwgLine(e)     ) ;
            DWG_ETYPE.ELLIPSE    : addEllipse ( DwgEllipse(e)  ) ;
            DWG_ETYPE.SPLINE     : addSpline  ( DwgSpline(e)   ) ;
          end ;
        end ;

        for hle in currBlock.Entities do begin
          hle.Parent := hatch ;
          hle.Layer  := nil ;
          lp.AddEntity( hle ) ;
        end ;
        currBlock.Entities.Clear ;
        hatch.AddLoop( lp ) ;
      end ;
    finally
      currBlock := oldBlock ;
      FreeObject( lpContainer ) ;
    end ;

    setEntityStyle( hatch, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( hatch ) ;
  end ;

  procedure DwgReaderCallBack.addHeader(
    const _data : DwgHeader
  );
  begin
    dwg_log( 'HEADER' ) ; dwg_log('\n');
  end ;

  procedure DwgReaderCallBack.addInsert(
    const _data : DwgInsert
  ) ;
  var
    ins : T_FileDwg_Insert ;
    tbl : DwgBlockRecord ;
  begin
    checkBlock ;
    dwg_log( 'INSERT  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log( ' BLOCK "' );
    dwg_log( DwgReader(rdr).findTableName( DWG_TTYPE.BLOCK_RECORD, _data.blockRecH.ref ) );
    dwg_log('"\n');

    ins := T_FileDwg_Insert.Create( _data.handle, T_FileDwg_EntityType.Insert ) ;
    ins.InsertionPoint := _data.basePoint.ToPoint ;
    ins.Scale          := GisPoint3D( _data.xscale, _data.yscale, _data.zscale ) ;
    ins.Rotation       := _data.angle ;
    ins.Extrusion      := _data.extPoint.ToPoint ;

    tbl := DwgReader(rdr).findTable( DWG_TTYPE.BLOCK_RECORD,
                                     _data.blockRecH.ref
                                    ) as DwgBlockRecord;
    if ( tbl <> nil ) then begin
      ins.BlockHandle  := tbl.handle ;
      ins.BlockBasePoint := tbl.basePoint.ToPoint ;
    end
    else
      tbl := nil ;

    setEntityStyle( ins, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( ins ) ;
  end ;

  procedure DwgReaderCallBack.addLayer(
    const _data : DwgLayer
  );
  var
    la : T_FileDwg_Layer ;
  begin
    dwg_log( 'LAYER "' ) ; dwg_log( _data.name ) ;
    dwg_log('"  ');dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    la := T_FileDwg_Layer.Create ;
    la.Name        := _data.name ;
    if IsStringEmpty(la.Name) then
      la.Name := _data.handle.ToString ;
    la.Style.Color := numberToColor( _data.color ) ;
    la.Style.Pen   := nameToLineType( _data.lineType ) ;
    la.Style.Width := numberToWidth ( _data.lWeight  ) ;
    la.Visible     := (_data.flag and $01) = 0 ;
    la.NativeLayer := nil ;

    model.AddLayer( la ) ;
  end ;

  procedure DwgReaderCallBack.addLayout( const _data: DwgLayout);
  begin
    dwg_log( 'LAYOUT "' ) ; dwg_log( _data.name ) ;
    dwg_log( '"  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');
  end ;

  procedure DwgReaderCallBack.addLine(
    const _data : DwgLine
  ) ;
  var
    line : T_FileDwg_Line ;
  begin
    checkBlock ;
    dwg_log( 'LINE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    line := T_FileDwg_Line.Create( _data.handle, T_FileDwg_EntityType.Line ) ;
    line.Ptg1      := _data.basePoint.ToPoint ;
    line.Ptg2      := _data.secPoint.ToPoint ;
    line.Extrusion := GisPoint3D( 0, 0, 1 ) ;

    setEntityStyle( line, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( line ) ;
  end ;

  procedure DwgReaderCallBack.addLType(
    const _data : DwgLType
  );
  begin
    dwg_log( 'LTYPE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');
  end ;

  procedure DwgReaderCallBack.addLWPolyline(
    const _data : DwgLWPolyline
  );
  var
    lw : T_FileDwg_LWPolyline ;
    {$IFDEF DCC}
    v : DwgVertex2D ;
    {$ENDIF}
  begin
    checkBlock ;
    dwg_log( 'LWPOLYLINE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    lw := T_FileDwg_LWPolyline.Create( _data.handle, T_FileDwg_EntityType.LWPolyline ) ;
    lw.Width     := _data.width ;
    lw.Thickness := _data.thickness ;
    lw.Extrusion := _data.extPoint.ToPoint ;
    lw.NumBulges := _data.bulgesnum ;
    lw.NumVertex := _data.vertexnum ;
    lw.Flag      := _data.flag ;

    for v in _data.vertlist do
      lw.AddVertex( GisPoint3D( v.x, v.y, _data.elevation, v.bulge ) );

    if _data.width > 0 then
      _data.lWeight := RoundS(_data.width) ;

    setEntityStyle( lw, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( lw ) ;

  end ;

  procedure DwgReaderCallBack.addLeader(
    const _data : DwgLeader
  );
  var
    ld : T_FileDwg_Leader ;
    {$IFDEF DCC}
    v : DwgCoord ;
    {$ENDIF}
  begin
    checkBlock ;
    dwg_log( 'LEADER  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    ld := T_FileDwg_Leader.Create( _data.handle, T_FileDwg_EntityType.Leader ) ;
    ld.Extrusion := _data.extrusionPoint.ToPoint ;
    ld.NumVertex := _data.vertnum ;

    for v in _data.vertexlist do
      ld.AddVertex( GisPoint3D( v.x, v.y, 0, 0 ) );

    setEntityStyle( ld, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( ld ) ;
  end ;

  procedure DwgReaderCallBack.addColor(
    const _data : DwgDbColor
  );
  var
    color : T_FileDwg_Color ;
  begin
    checkBlock ;
    dwg_log( 'DBCOLOR  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    color := numberToColor( _data.color ) ;

    model.AddColor( _data.handle, color ) ;
  end ;

  procedure DwgReaderCallBack.addMText(
    const _data : DwgMText
  );
  var
    txt : T_FileDwg_Text ;
  begin
    checkBlock ;
    dwg_log( 'MTEXT  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    txt := T_FileDwg_Text.Create( _data.handle, T_FileDwg_EntityType.MText ) ;
    txt.InsertionPoint  := _data.basePoint.ToPoint ;
    txt.AlignmentPoint  := _data.secPoint.ToPoint ;

    txt.Text            := _data.text ;
    txt.Angle           := _data.angle ;
    txt.Height          := _data.height ;
    txt.WidthScale      := _data.widthscale ;
    txt.AlignH          := _data.alignH ;
    txt.AlignV          := _data.alignV ;
    txt.Extrusion       := _data.extPoint.ToPoint ;

    setEntityStyle( txt, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( txt ) ;

  end ;

  procedure DwgReaderCallBack.addPoint(
    const _data : DwgPoint
  ) ;
  var
    pt : T_FileDwg_Point ;
  begin
    checkBlock ;
    dwg_log( 'POINT  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    pt := T_FileDwg_Point.Create( _data.handle, T_FileDwg_EntityType.Point ) ;
    pt.Ptg       := _data.basePoint.ToPoint ;
    pt.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( pt, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( pt ) ;
  end ;

  procedure DwgReaderCallBack.addPolyline(
    const _data : DwgPolyline
  );
  var
    pl : T_FileDwg_Polyline ;
    {$IFDEF DCC}
     v : DwgVertex ;
    {$ENDIF}
    vt : T_FileDwg_Vertex ;
    vf : T_FileDwg_Face ;
    pvt: T_FileDwg_VertexType ;

      function getVType( const _vt : dint16 ) : T_FileDwg_VertexType ;
      begin
        case _vt of
          DWG_OBJ_ID_VERTEX_2D         : Result := T_FileDwg_VertexType.V2D ;
          DWG_OBJ_ID_VERTEX_3D         : Result := T_FileDwg_VertexType.V3D ;
          DWG_OBJ_ID_VERTEX_MESH       : Result := T_FileDwg_VertexType.VMesh ;
          DWG_OBJ_ID_VERTEX_PFACE      : Result := T_FileDwg_VertexType.VPFace ;
          DWG_OBJ_ID_VERTEX_PFACE_FACE : Result := T_FileDwg_VertexType.VPFaceFace
        else                             Result := T_FileDwg_VertexType.V2D
        end ;
      end ;
  begin
    checkBlock ;
    dwg_log( 'POLYLINE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    pl := T_FileDwg_Polyline.Create( _data.handle, T_FileDwg_EntityType.Polyline ) ;
    pl.Extrusion    := _data.extPoint.ToPoint ;
    if _data.vertexcount <> 0 then
      pl.NumVertex    := _data.vertexcount
    else
      pl.NumVertex    := _data.vertlist.Count ;
    pl.NumFaces     := _data.facecount ;
    pl.MVertexCount := _data.mvertexcount ;
    pl.NVertexCount := _data.nvertexcount ;
    pl.MDensity     := _data.mdensity ;
    pl.NDensity     := _data.ndensity ;
    pl.Flag         := _data.flag ;
    pl.CurveType    := _data.curvetype ;

    if _data.oType = DWG_OBJ_ID_POLYLINE_2D then begin
      pl.PType := T_FileDwg_PolylineType.P2D ;
      for v in _data.vertlist do begin
        vt := T_FileDwg_Vertex.Create( getVType(v.oType) );
        vt.Id    := v.identifier ;
        vt.Ptg   := v.basePoint.ToPoint ;
        vt.Bulge := v.bulge ;
        vt.Flag  := v.flag ;
        pl.AddVertex( vt );
      end ;
    end
    else if _data.oType = DWG_OBJ_ID_POLYLINE_3D then begin
      pl.PType := T_FileDwg_PolylineType.P3D ;
      for v in _data.vertlist do begin
        vt := T_FileDwg_Vertex.Create( getVType(v.oType) );
        vt.Id    := v.identifier ;
        vt.Ptg   := v.basePoint.ToPoint ;
        vt.Flag  := v.flag ;
        vt.Bulge := v.bulge ;
        pl.AddVertex( vt );
      end ;
    end
    else if _data.oType = DWG_OBJ_ID_POLYLINE_PFACE then begin
      pl.PType := T_FileDwg_PolylineType.PFace ;
      for v in _data.vertlist do begin
        pvt := getVType(v.oType) ;
        if pvt = T_FileDwg_VertexType.VPFace then begin
          vt := T_FileDwg_Vertex.Create( pvt );
          vt.Id    := v.identifier ;
          vt.Ptg   := v.basePoint.ToPoint ;
          vt.Flag  := v.flag ;
          vt.Bulge := v.bulge ;
          pl.AddVertex( vt );
        end
        else if pvt = T_FileDwg_VertexType.VPFaceFace then begin
          {$IFDEF OXYGENE}
          vf.VIndex := new SmallInt[4] ;
          {$ENDIF}
          vf.VIndex[0] := SmallInt(v.vindex1) ;
          vf.VIndex[1] := SmallInt(v.vindex2) ;
          vf.VIndex[2] := SmallInt(v.vindex3) ;
          vf.VIndex[3] := SmallInt(v.vindex4) ;
          pl.AddFace( vf );
        end
      end ;
    end
    else if _data.oType = DWG_OBJ_ID_POLYLINE_MESH then begin
      pl.PType := T_FileDwg_PolylineType.PMesh ;
      for v in _data.vertlist do begin
        vt := T_FileDwg_Vertex.Create( getVType(v.oType) );
        vt.Id    := v.identifier ;
        vt.Ptg   := v.basePoint.ToPoint ;
        vt.Flag  := v.flag ;
        vt.Bulge := v.bulge ;
        pl.AddVertex( vt );
      end ;
    end ;

    setEntityStyle( pl, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( pl ) ;

  end ;

  procedure DwgReaderCallBack.addRay(
    const _data : DwgRay
  );
  var
    ray : T_FileDwg_Line ;
  begin
    checkBlock ;
    dwg_log( 'RAY  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    ray := T_FileDwg_Line.Create( _data.handle, T_FileDwg_EntityType.Ray ) ;
    ray.Ptg1      := _data.basePoint.ToPoint ;
    ray.Ptg2      := _data.secPoint.ToPoint ;
    ray.Ptg2.X    := ray.Ptg2.X + ray.Ptg1.X ;
    ray.Ptg2.Y    := ray.Ptg2.Y + ray.Ptg1.Y ;
    ray.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( ray, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( ray ) ;
  end ;

  procedure DwgReaderCallBack.addSolid(
    const _data : DwgSolid
  );
  var
    solid : T_FileDwg_Solid ;
  begin
    checkBlock ;
    dwg_log( 'SOLID  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    solid := T_FileDwg_Solid.Create( _data.handle, T_FileDwg_EntityType.Solid ) ;
    solid.Ptg1      := _data.basePoint.ToPoint ;
    solid.Ptg2      := _data.secPoint.ToPoint ;
    solid.Ptg3      := _data.thirdPoint.ToPoint ;
    solid.Ptg4      := _data.fourPoint.ToPoint ;
    solid.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( solid, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( solid ) ;

  end ;

  procedure DwgReaderCallBack.addSpline(
    const _data : DwgSpline
  );
  var
    spline : T_FileDwg_Spline ;
    {$IFDEF DCC}
      p : DwgCoord ;
      k : Double ;
      w : Double ;
    {$ENDIF}
  begin
    checkBlock ;
    dwg_log( 'SPLINE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    spline := T_FileDwg_Spline.Create( _data.handle, T_FileDwg_EntityType.Spline ) ;
    spline.Extrusion := GisPoint3D( 0, 0, 1);
    spline.Degree := _data.degree ;
    spline.NumControlPoints := _data.ncontrol ;
    spline.NumKnots := _data.nknots ;
    spline.NumFitPoints := _data.nfit ;
    spline.Flag := _data.flags ;

    for p in _data.controllist do
      spline.AddControlPoint( GisPoint3D( p.x, p.y, p.z ) );

    for p in _data.fitlist do
      spline.AddFitPoint( GisPoint3D( p.x, p.y, p.z ) );

    for k in _data.knotslist do
      spline.AddKnot( k );

    for w in _data.weightslist do
      spline.AddWeight( w );

    setEntityStyle( spline, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( spline ) ;
  end ;

  procedure DwgReaderCallBack.addText(
    const _data : DwgText
  );
  var
    txt : T_FileDwg_Text ;
  begin
    checkBlock ;
    dwg_log( 'TEXT  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    txt := T_FileDwg_Text.Create( _data.handle, T_FileDwg_EntityType.Text ) ;
    txt.InsertionPoint  := _data.basePoint.ToPoint ;
    txt.AlignmentPoint  := _data.secPoint.ToPoint ;

    if (_data.alignV <> TGIS_TextVAlign.VBaseLine) or
       (_data.alignH <> TGIS_TextHAlign.HLeft    ) or
       (_data.alignH = TGIS_TextHAlign.HMiddle   ) then begin
       if (_data.alignH <> TGIS_TextHAlign.HAligned) and
          (_data.alignH <> TGIS_TextHAlign.HFit) then begin
           txt.AlignmentPoint := _data.basePoint.ToPoint ;
           txt.InsertionPoint := _data.secPoint.ToPoint ;
       end ;
    end ;
    txt.Text            := _data.text ;
    txt.Angle           := _data.angle ;
    txt.Height          := _data.height ;
    txt.WidthScale      := _data.widthscale ;
    txt.AlignH          := _data.alignH ;
    txt.AlignV          := _data.alignV ;
    txt.Extrusion       := _data.extPoint.ToPoint ;

    setEntityStyle( txt, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( txt ) ;

  end ;

  procedure DwgReaderCallBack.addTextStyle(
    const _data : DwgTextstyle
  );
  begin
    dwg_log( 'TEXT STYLE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');
  end ;

  procedure DwgReaderCallBack.addTrace(
    const _data : DwgTrace
  );
  var
    trace : T_FileDwg_Solid ;
  begin
    checkBlock ;
    dwg_log( 'TRACE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    trace := T_FileDwg_Solid.Create( _data.handle, T_FileDwg_EntityType.Trace ) ;
    trace.Ptg1      := _data.basePoint.ToPoint ;
    trace.Ptg2      := _data.secPoint.ToPoint ;
    trace.Ptg3      := _data.thirdPoint.ToPoint ;
    trace.Ptg4      := _data.fourPoint.ToPoint ;
    trace.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( trace, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( trace ) ;
  end ;

  procedure DwgReaderCallBack.addVport(
    const _data : DwgVport
  );
  begin
    dwg_log( 'VPORT  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');
  end ;

  procedure DwgReaderCallBack.addXline(
    const _data : DwgXline
  );
  var
    line : T_FileDwg_Line ;
  begin
    checkBlock ;
    dwg_log( 'XLINE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    line := T_FileDwg_Line.Create( _data.handle, T_FileDwg_EntityType.XLine ) ;
    line.Ptg1      := _data.basePoint.ToPoint ;
    line.Ptg2      := _data.secPoint.ToPoint ;
    line.Ptg2.X    := line.Ptg2.X + line.Ptg1.X ;
    line.Ptg2.Y    := line.Ptg2.Y + line.Ptg1.Y ;
    line.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( line, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( line ) ;

  end ;

  procedure DwgReaderCallBack.addDimAligned(
    const _data : DwgDimensionAligned
  ) ;
  var
    dimAligned : T_FileDwg_Dimension ;
  begin
    checkBlock ;
    dwg_log( 'DIMENSION ALIGNED  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    dimAligned := T_FileDwg_Dimension.Create( _data.handle, T_FileDwg_EntityType.DimensionAligned ) ;
    dimAligned.Ptg1      := _data.def1.ToPoint ;
    dimAligned.Ptg2      := _data.def2.ToPoint ;
    dimAligned.Ptg3      := _data.defPoint.ToPoint ;
    dimAligned.PtgText   := _data.textPoint.ToPoint ;
    dimAligned.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( dimAligned, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( dimAligned ) ;
  end ;

  procedure DwgReaderCallBack.addDimLinear(
    const _data : DwgDimensionLinear
  ) ;
  var
    dimLinear : T_FileDwg_Dimension ;
  begin
    checkBlock ;
    dwg_log( 'DIMENSION LINEAR  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    dimLinear := T_FileDwg_Dimension.Create( _data.handle, T_FileDwg_EntityType.DimensionLinear ) ;
    dimLinear.Ptg1      := _data.def1.ToPoint ;
    dimLinear.Ptg2      := _data.def2.ToPoint ;
    dimLinear.Ptg3      := _data.defPoint.ToPoint ;
    dimLinear.PtgText   := _data.textPoint.ToPoint ;
    dimLinear.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( dimLinear, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( dimLinear ) ;
  end ;

  procedure DwgReaderCallBack.addDimOrdinate(
    const _data : DwgDimensionOrdinate
  ) ;
  var
    dimOrdinate : T_FileDwg_Dimension ;
  begin
    checkBlock ;
    dwg_log( 'DIMENSION ORDINATE  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    dimOrdinate := T_FileDwg_Dimension.Create( _data.handle, T_FileDwg_EntityType.DimensionOrdinate ) ;
    dimOrdinate.Ptg1      := _data.def1.ToPoint ;
    dimOrdinate.Ptg2      := _data.def2.ToPoint ;
    dimOrdinate.Ptg3      := _data.defPoint.ToPoint ;
    dimOrdinate.PtgText   := _data.textPoint.ToPoint ;
    dimOrdinate.Text      := _data.text ;
    dimOrdinate.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( dimOrdinate, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( dimOrdinate ) ;
  end ;

  procedure DwgReaderCallBack.addDimDiameter(
    const _data : DwgDimensionDiametric
  ) ;
  var
    dimDiametric : T_FileDwg_Dimension ;
  begin
    checkBlock ;
    dwg_log( 'DIMENSION DIAMETRIC  ' ) ; dwg_log( LowerCase('0x'+IntToHex(_data.handle, 2)) )  ;
    dwg_log('\n');

    dimDiametric := T_FileDwg_Dimension.Create( _data.handle, T_FileDwg_EntityType.DimensionDiametric ) ;
    dimDiametric.Ptg1      := _data.def1.ToPoint ;
    dimDiametric.Ptg2      := _data.defPoint.ToPoint ;
    dimDiametric.Ptg3      := _data.def2.ToPoint ;
    dimDiametric.PtgText   := _data.textPoint.ToPoint ;
    dimDiametric.Text      := _data.text ;
    dimDiametric.Extrusion := _data.extPoint.ToPoint ;

    setEntityStyle( dimDiametric, _data ) ;
    assert( currBlock <> nil ) ;
    currBlock.AddEntity( dimDiametric ) ;
  end ;

  procedure DwgReaderCallBack.endBlock;
  begin
    inBlock := False ;
    dwg_log( 'ENDBLOCK' ) ; dwg_log('\n');
    currBlock := model.GetModelBlock ;
  end ;

// ----------------------------------------------------------------------------
// T_FileDwg_Entity
// ----------------------------------------------------------------------------

  constructor T_FileDwg_Entity.Create(
    const _handle : UInt64 ;
    const _eType  : T_FileDwg_EntityType
  ) ;
  begin
    inherited Create ;

    Handle  := _handle ;
    eType   := _eType ;
    Layer   := nil ;
    Parent  := nil ;
    ExtData := nil ;
  end ;

  {$IFNDEF OXYGENE}
    destructor T_FileDwg_Entity.Destroy ;
    begin
      if assigned( ExtData ) then
        FreeObject( ExtData ) ;

      inherited ;
    end ;
  {$ENDIF}


  function T_FileDwg_Entity.GetDrawStyle(
    const _model : TObject
  ) : T_FileDwg_DrawStyle ;
  var
    la : T_FileDwg_Layer ;
    pb : T_FileDwg_Entity ;
    p  : T_FileDwg_Style ;
    c  : T_FileDwg_Color ;
  begin
    if assigned( Layer ) then
      la := Layer
    else if assigned( Parent ) then
      la := Parent.Layer
    else
      la := nil ;

    p := Style ;
    if assigned( Parent ) then begin
      pb := Parent ;

      while (p.Color.CType = T_FileDwg_Flag.ByBlock) do begin
        if assigned(pb) then begin
          p.Color := Parent.Style.Color ;
          pb := pb.Parent ;
        end
        else
          break;
      end ;

      pb := Parent ;
      while (p.Width.WType = T_FileDwg_Flag.ByBlock) do begin
        if assigned(pb) then begin
          p.Width := Parent.Style.Width ;
          pb := pb.Parent ;
        end
        else
          break;
      end ;

      pb := Parent ;
      while (p.Pen.PType = T_FileDwg_Flag.ByBlock) do begin
        if assigned(pb) then begin
          p.Pen := Parent.Style.Pen ;
          pb := pb.Parent ;
        end
        else
          break;
      end ;
    end ;

    if assigned(la) then begin
      if (p.Color.CType = T_FileDwg_Flag.ByLayer) then
        p.Color := la.Style.Color ;

      if (p.Width.WType = T_FileDwg_Flag.ByLayer) then
        p.Width := la.Style.Width ;

      if (p.Pen.PType = T_FileDwg_Flag.ByLayer) then
        p.Pen := la.Style.Pen ;
    end ;

    if p.Width.Width > 0 then
      Result.Width := TruncS(p.Width.Width)
    else
      Result.Width := 1 ;

    Result.Pen   := p.Pen.Pen ;
    if p.Color.CType = T_FileDwg_Flag.ByReference then begin
      if T_FileDwg_Model(_model).FindColor( p.Color.Reference, c ) then
        Result.Color := c.Color
    end
    else
      Result.Color := p.Color.Color ;
  end ;

// ----------------------------------------------------------------------------
// T_FileDwg_Block
// ----------------------------------------------------------------------------

  constructor T_FileDwg_Block.Create(
    const _handle : UInt64 ;
    const _eType  : T_FileDwg_EntityType
  ) ;
  begin
    inherited Create( _handle, _eType );

    Entities := TList<T_FileDwg_Entity>.Create ;
  end ;


  {$IFNDEF OXYGENE}
  destructor T_FileDwg_Block.Destroy ;
  var
    e : T_FileDwg_Entity ;
  begin
    for e in Entities do
      FreeObjectNotNil( e ) ;

    FreeObject( Entities ) ;

    inherited ;
  end ;
  {$ENDIF}

  procedure T_FileDwg_Block.AddEntity(
    const _e : T_FileDwg_Entity
  ) ;
  begin
    _e.Parent := Self ;
    Entities.Add( _e ) ;
  end ;


// ----------------------------------------------------------------------------
// T_FileDwg_LWPolyline
// ----------------------------------------------------------------------------

  constructor T_FileDwg_LWPolyline.Create(
    const _handle : UInt64 ;
    const _eType  : T_FileDwg_EntityType
  ) ;
  begin
    inherited ;
    Vertices := TList<TGIS_Point3D>.Create ;
  end ;

  {$IFNDEF OXYGENE}
  destructor T_FileDwg_LWPolyline.Destroy;
  begin
    FreeObject( vertices ) ;

    inherited ;
  end ;
 {$ENDIF}

  procedure T_FileDwg_LWPolyline.AddVertex(
    const _v : TGIS_Point3D
  ) ;
  begin
    Vertices.Add( _v ) ;
  end ;


 // ----------------------------------------------------------------------------
 // T_FileDwg_Leader
 // ----------------------------------------------------------------------------

   constructor T_FileDwg_Leader.Create(
     const _handle : UInt64 ;
     const _eType  : T_FileDwg_EntityType
   ) ;
   begin
     inherited ;
     Vertices := TList<TGIS_Point3D>.Create ;
   end ;

   {$IFNDEF OXYGENE}
   destructor T_FileDwg_Leader.Destroy;
   begin
     FreeObject( vertices ) ;

     inherited ;
   end ;
  {$ENDIF}

   procedure T_FileDwg_Leader.AddVertex(
     const _v : TGIS_Point3D
   ) ;
   begin
     Vertices.Add( _v ) ;
   end ;

// ----------------------------------------------------------------------------
// T_FileDwg_Spline
// ----------------------------------------------------------------------------

  constructor T_FileDwg_Spline.Create(
    const _handle : UInt64 ;
    const _eType  : T_FileDwg_EntityType
  ) ;
  begin
    inherited ;

    Degree            := 0 ;
    NumControlPoints  := 0 ;
    NumKnots          := 0 ;
    NumFitPoints      := 0 ;
    ControlPoints     := TList<TGIS_Point3D>.Create;
    FitPoints         := TList<TGIS_Point3D>.Create;
    Knots             := TList<Double>.Create;
    Weights           := TList<Double>.Create;
  end ;

  {$IFNDEF OXYGENE}
  destructor T_FileDwg_Spline.Destroy;
  begin
    FreeObject( ControlPoints ) ;
    FreeObject( FitPoints     ) ;
    FreeObject( Knots         ) ;
    FreeObject( Weights       ) ;

    inherited ;
  end ;
 {$ENDIF}

  procedure T_FileDwg_Spline.AddControlPoint(
    const _p : TGIS_Point3D
  ) ;
  begin
    ControlPoints.Add( _p ) ;
  end ;

  procedure T_FileDwg_Spline.AddFitPoint(
    const _p : TGIS_Point3D
  ) ;
  begin
    FitPoints.Add( _p ) ;
  end ;

  procedure T_FileDwg_Spline.AddKnot(
    const _k : Double
  ) ;
  begin
    Knots.Add( _k ) ;
  end ;

  procedure T_FileDwg_Spline.AddWeight(
    const _w : Double
  ) ;
  begin
    Weights.Add( _w ) ;
  end ;

  constructor T_FileDwg_Vertex.Create(
    const _vt : T_FileDwg_VertexType
  ) ;
  begin
    Id      := 0 ;
    Ptg     := GisPoint3D(0, 0, 0) ;
    Flag    := 0 ;
    Bulge   := 0 ;
    VType   := _vt ;
  end ;

 // ----------------------------------------------------------------------------
 // T_FileDwg_Polyline
 // ----------------------------------------------------------------------------

  constructor T_FileDwg_Polyline.Create(
    const _handle : UInt64 ;
    const _eType  : T_FileDwg_EntityType
  ) ;
  begin
    inherited ;
    Vertices := TList<T_FileDwg_Vertex>.Create ;
    Faces    := TList<T_FileDwg_Face>.Create ;
  end ;

   {$IFNDEF OXYGENE}
   destructor T_FileDwg_Polyline.Destroy;
   begin
     FreeObject( vertices ) ;
     FreeObject( faces    ) ;

     inherited ;
   end ;
  {$ENDIF}

  procedure T_FileDwg_Polyline.AddVertex(
    const _v : T_FileDwg_Vertex
  ) ;
  begin
    Vertices.Add( _v ) ;
  end ;

  procedure T_FileDwg_Polyline.AddFace(
    const _v : T_FileDwg_Face
  ) ;
  begin
    Faces.Add( _v ) ;
  end ;

 // ----------------------------------------------------------------------------
 // T_FileDwg_HatchLoop
 // ----------------------------------------------------------------------------

  constructor T_FileDwg_HatchLoop.Create(
    const _lType  : Integer
  ) ;
  begin
    inherited Create;
    HLType := _lType ;
    Entities := TList<T_FileDwg_Entity>.Create ;
  end ;

   {$IFNDEF OXYGENE}
   destructor T_FileDwg_HatchLoop.Destroy;
   var
     e : T_FileDwg_Entity ;
   begin
     for e in Entities do
       FreeObjectNotNil( e ) ;

     FreeObject( Entities ) ;

     inherited ;
   end ;
  {$ENDIF}

  procedure T_FileDwg_HatchLoop.AddEntity(
    const _e : T_FileDwg_Entity
  ) ;
  begin
    Entities.Add( _e ) ;
  end ;

 // ----------------------------------------------------------------------------
 // T_FileDwg_Hatch
 // ----------------------------------------------------------------------------

  constructor T_FileDwg_Hatch.Create(
    const _handle : UInt64 ;
    const _eType  : T_FileDwg_EntityType
  ) ;
  begin
    inherited ;
    Loops := TList<T_FileDwg_HatchLoop>.Create ;
  end ;

   {$IFNDEF OXYGENE}
   destructor T_FileDwg_Hatch.Destroy;
   var
     hl : T_FileDwg_HatchLoop ;
   begin
     for hl in Loops do
       FreeObjectNotNil( hl ) ;

     FreeObject( Loops ) ;

     inherited ;
   end ;
  {$ENDIF}

  procedure T_FileDwg_Hatch.AddLoop(
    const _e : T_FileDwg_HatchLoop
  ) ;
  begin
    Loops.Add( _e ) ;
  end ;

// ----------------------------------------------------------------------------
// T_FileDwg_Model
// ----------------------------------------------------------------------------

  constructor T_FileDwg_Model.Create ;
  begin
    inherited ;

    Blocks  := TDictionary<UInt64, T_FileDwg_Block>.Create ;
    Layers  := TDictionary<String, T_FileDwg_Layer>.Create;
    Colors  := TDictionary<Cardinal, T_FileDwg_Color>.Create ;
    Palette := TList<TGIS_Color>.Create ;

    Extent := GisNoWorld3D ;
    Handle := 0 ;
  end ;

  {$IFNDEF OXYGENE}
    destructor T_FileDwg_Model.Destroy ;
    var
      b : TPair<UInt64, T_FileDwg_Block> ;
      l : TPair<String, T_FileDwg_Layer> ;
    begin
      for b in Blocks do
        FreeObjectNotNil( b .Value) ;
      for l in Layers do
        FreeObjectNotNil( l.Value ) ;

      FreeObject( Blocks   ) ;
      FreeObject( Layers   ) ;
      FreeObject( Colors   ) ;
      FreeObject( Palette  ) ;

      inherited ;
    end ;
  {$ENDIF}

  function T_FileDwg_Model.AddBlock (
    const _blk : T_FileDwg_Block
  ) : Boolean ;
  begin
    Result := True ;
    if Blocks.ContainsKey( _blk.Handle ) then
      Result := False
    else
      Blocks.Add( _blk.Handle, _blk ) ;
  end ;

  procedure T_FileDwg_Model.AddLayer(
    const _la   : T_FileDwg_Layer
  ) ;
  begin
    Layers.Add( _la.Name, _la ) ;
  end ;

  procedure T_FileDwg_Model.AddColor(
    const _handle : Cardinal ;
    const _color  : T_FileDwg_Color
  ) ;
  begin
    Colors.Add( _handle, _color ) ;
  end ;

  procedure T_FileDwg_Model.AddPaletteColor(
    const _color  : TGIS_Color
  ) ;
  begin
    Palette.Add( _color ) ;
  end ;

  function T_FileDwg_Model.GetPaletteColor(
    const _idx   : Integer
  ) : TGIS_Color ;
  begin
    if (_idx >= 0) and (_idx < Palette.Count) then
      Result := Palette[_idx]
    else
      Result := TGIS_Color.White ;
  end ;

  function T_FileDwg_Model.FindColor(
    const _handle   : Cardinal;
      var _color    : T_FileDwg_Color
  ) : Boolean ;
  var
    c : {$IFDEF JAVA} nullable {$ENDIF}T_FileDwg_Color ;
  begin
    Result := Colors.TryGetValue( _handle, c ) ;
    if Result then
      _color := c ;
  end ;

  function T_FileDwg_Model.FindLayer(
    const _name : String
  ) : T_FileDwg_Layer ;
  var
    la : T_FileDwg_Layer ;
  begin
    if Layers.TryGetValue( _name, la ) then
      Result := la
    else
      Result := nil ;
  end ;

  procedure T_FileDwg_Model.SetModelBlock(
    const _blk : T_FileDwg_Block
  ) ;
  begin
    if not Blocks.ContainsKey( _blk.Handle ) then
      Blocks.Add( _blk.Handle, _blk ) ;
    Handle := _blk.Handle ;
  end ;

  function T_FileDwg_Model.GetModelBlock : T_FileDwg_Block ;
  var
    blk : T_FileDwg_Block ;
  begin
    if Blocks.TryGetValue( Handle, blk ) then
      Result := blk
    else
      Result := nil ;
  end ;

  function T_FileDwg_Model.GetBlock(
    const _handle : UInt64
  ) : T_FileDwg_Block ;
  var
    blk : T_FileDwg_Block ;
  begin
    if Blocks.TryGetValue( _handle, blk ) then
      Result := blk
    else
      Result := nil ;
  end ;


// ----------------------------------------------------------------------------
// T_FileDwg_Color
// ----------------------------------------------------------------------------

  constructor T_FileDwg_Color.Create(
    const _ctype : T_FileDwg_Flag
  ) ;
  begin
    CType := _ctype ;
    Color := TGIS_Color.Black ;
    Reference := 0 ;
  end ;

  constructor T_FileDwg_Color.Create(
    const _color : TGIS_Color
  ) ;
  begin
    Color := _color ;
    CType := T_FileDwg_Flag.Default ;
    Reference := 0 ;
  end ;

  constructor T_FileDwg_Color.Create(
    const _r     : Byte ;
    const _g     : Byte ;
    const _b     : Byte
   ) ;
  begin
    Color := TGIS_Color.FromRGB( _r, _g, _b ) ;
    CType := T_FileDwg_Flag.Default ;
    Reference := 0 ;
  end ;

  constructor T_FileDwg_Color.Create(
    const _ctype  : T_FileDwg_Flag ;
    const _handle : Cardinal
  ) ;
  begin
    CType := _ctype ;
    Color := TGIS_Color.Black ;
    Reference := _handle ;
  end ;

// ----------------------------------------------------------------------------
// T_FileDwg_Pen
// ----------------------------------------------------------------------------

  constructor T_FileDwg_Pen.Create(
    const _ctype : T_FileDwg_Flag
  ) ;
  begin
    Pen   := TGIS_PenStyle.Solid ;
    PType := _ctype ;
  end ;

  constructor T_FileDwg_Pen.Create(
    const _pen : TGIS_PenStyle
  ) ;
  begin
    Pen   := _pen ;
    PType := T_FileDwg_Flag.Default ;
  end ;

// ----------------------------------------------------------------------------
// T_FileDwg_Width
// ----------------------------------------------------------------------------

  constructor T_FileDwg_Width.Create(
    const _ctype : T_FileDwg_Flag
  ) ;
  begin
    WType := _ctype ;
    Width := 1 ;
  end ;

  constructor T_FileDwg_Width.Create(
    const _width : Double
  ) ;
  begin
    Width := _width ;
    WType := T_FileDwg_Flag.Default ;
  end ;

// ----------------------------------------------------------------------------
// T_FileDwg_Reader
// ----------------------------------------------------------------------------

  constructor TGIS_FileDWG.Create;
  begin
    inherited;

    FileName := '';
    FModel := T_FileDwg_Model.Create ;
  end ;

  {$IFNDEF OXYGENE}
  destructor TGIS_FileDWG.Destroy;
  begin
    FreeObject( FModel ) ;

    inherited;
  end ;
  {$ENDIF}

  function TGIS_FileDWG.Open(
    const _path  : String
  ) : Boolean;
  var
    filestrm : TGIS_BufferedFileStream;
    ver      : String;
    buf      : TBytes;
    hdr      : DwgHeader;
    ret      : Boolean;
    version  : DWG_Version;
    reader   : DwgReader;
  begin
    Result := false;
    FileName := _path;

    filestrm := TGIS_BufferedFileStream.Create( FileName, TGIS_StreamMode.Read ) ;
    try
      SetLength( buf, 6 ) ;
      {$IFDEF OXYGENE}
        filestrm.Read( buf, 0, 6 ) ;
        ver := TEncoding.ASCII.GetString( buf, 0, 6 ) ;
      {$ELSE}
        filestrm.read( buf[0], 6 ) ;
        ver := TEncoding.ASCII.GetString( buf ) ;
      {$ENDIF}

      reader := nil ;
      if ver = 'AC1006' then begin
        version := DWG_Version.AC1006;
      end
      else if ver = 'AC1009' then begin
        version := DWG_Version.AC1009;
      end
      else if ver = 'AC1012' then begin
        version := DWG_Version.AC1012;
        reader := DwgReader15.Create( filestrm, version ) ;
      end
      else if ver = 'AC1014' then begin
        version := DWG_Version.AC1014;
        reader := DwgReader15.Create( filestrm, version ) ;
      end
      else if ver = 'AC1015' then begin
        version := DWG_Version.AC1015;
        reader := DwgReader15.Create( filestrm, version ) ;
      end
      else if ver = 'AC1018' then begin
        version := DWG_Version.AC1018;
        reader := DwgReader18.Create( filestrm, version ) ;
      end
      else if ver = 'AC1021' then begin
        version := DWG_Version.AC1021;
        reader := DwgReader21.Create( filestrm, version ) ;
      end
      else if ver = 'AC1024' then begin
        version := DWG_Version.AC1024;
        reader := DwgReader24.Create( filestrm, version ) ;
      end
      else if ver = 'AC1027' then begin
        version := DWG_Version.AC1027;
        reader := DwgReader27.Create( filestrm, version ) ;
      end
      else if ver = 'AC1032' then begin
        version := DWG_Version.AC1032;
        reader := DwgReader32.Create( filestrm, version ) ;
      end
      else
        version := DWG_Version.UNKNOWNV;

      if assigned( reader ) then begin
        reader.iRdrClbk := DwgReaderCallBack.Create( reader, FModel ) ;

        Result := reader.readMetaData ;
        if Result then begin
          Result := reader.readFileHeader ;
          if Result then begin
            hdr := DwgHeader.Create ;
            try
              ret := reader.readDwgHeader( hdr ) ;
              ret := reader.readDwgClasses ;
              ret := reader.readDwgHandles ;
              ret := reader.readDwgTables( hdr ) ;
              ret := reader.readDwgBlocks ;
              ret := reader.readDwgEntities ;
              ret := reader.readDwgObjects ;
              ret := reader.prepareModel( hdr, FModel ) ;
            finally
              FreeObject( hdr ) ;
            end ;
            Result := ret ;
          end ;
        end ;
        if assigned( reader ) then
          FreeObject( reader ) ;
      end ;
    finally
      FreeObject( filestrm ) ;
    end ;
  end ;

  function TGIS_FileDWG.GetModel : T_FileDwg_Model ;
  begin
    Result := FModel ;
  end ;

//==================================== END =====================================
end.
