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
  Sub-layer encapsulation.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerSublayer ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerSublayer"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Generics.Collections,
    System.Generics.Defaults,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoRTree,
    Lider.CG.GIS.GeoSqlQuery,
    Lider.CG.GIS.GeoTransform;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   <para>
  ///     Sublayer class represents a 'virtual' views of the normal layer.
  ///     Typical sub-layers are named layers within TGIS_LayerWMS server
  ///     connection.
  ///   </para>
  ///   <para>
  ///     Sublayer offers only minimal set of native properties( Like Name,
  ///     Caption) while other calls will be forwarded to the parent layer.
  ///   </para>
  /// </summary>
  TGIS_LayerSublayer = {$IFDEF OXYGENE} public {$ENDIF}
                       class ( TGIS_Layer )
    protected // property access routines

      function  fget_Path            : String ; override;
      procedure fset_Path            ( const _value   : String
                                     ) ; override;
      function  fget_PathWithDriver  : String ; override;
      function  fget_Stream          : TStream ; override;
      procedure fset_Stream          ( const _value   : TStream
                                     ) ; override;
      function  fget_Caption         : String ; override;
      procedure fset_Caption         ( const _value   : String
                                     ) ; override;
      function  fget_Tag             : Integer ; override;
      procedure fset_Tag             ( const _value   : Integer
                                     ) ; override;
      {$IFDEF MANAGED}
        function  fget_TagPointer    : Object ; override;
        procedure fset_TagPointer    ( const _value   : Object
                                     ) ; override;
      {$ELSE}
        function  fget_TagPointer    : Pointer ; override;
        procedure fset_TagPointer    ( const _value   : Pointer
                                     ) ; override;
      {$ENDIF}
      function  fget_TagInternal     : Integer ; override;
      procedure fset_TagInternal     ( const _value   : Integer
                                     ) ; override;
      function  fget_UserObject      : TObject ; override;
      procedure fset_UserObject      ( const _value : TObject
                                     ) ; override;
      function  fget_Name            : String ; override;
      procedure fset_Name            ( const _value   : String
                                     ) ; override;
      function  fget_Basemap         : Boolean ; override;
      procedure fset_Basemap         ( const _value : Boolean
                                     ) ; override;
      function  fget_FileInfo        : String ; override;
      function  fget_FileCopyrights  : String ; override;
      function  fget_Comments        : String ; override;
      procedure fset_Comments        ( const _value   : String
                                     ) ; override;
      function  fget_IsLocked        : Boolean ; override;
      function  fget_IsOpened        : Boolean ; override;
      function  fget_DirectMode      : Boolean ; override;
      {$IFDEF CLR}
        function  fget_Age           : DateTime ; override;
      {$ELSE}
        function  fget_Age           : TDateTime ; override;
      {$ENDIF}
      function  fget_SubType         : TGIS_LayerSubTypeSet ; override;
      function  fget_Viewer          : TGIS_ViewerRef ; override;
      procedure fset_Viewer          ( const _value   : TGIS_ViewerRef
                                     ) ; override;
      function  fget_ConfigFile      : TGIS_ConfigAbstract ; override;
      function  fget_Extent          : TGIS_Extent ; override;
      procedure fset_Extent          ( const _value   : TGIS_Extent
                                     ) ; override;
      function  fget_Extent3D        : TGIS_Extent3D ; override;
      procedure fset_Extent3D        ( const _value : TGIS_Extent3D
                                     ) ; override;
      function  fget_ProjectedExtent : TGIS_Extent ; override;

      procedure fset_ProjectedExtent ( const _value   : TGIS_Extent
                                     ) ; override;

      function  fget_Active          : Boolean ; override;
      procedure fset_Active          ( const _value   : Boolean
                                     ) ; override;
      function  fget_HideFromLegend  : Boolean ; override;
      procedure fset_HideFromLegend  ( const _value   : Boolean
                                     ) ; override;
      function  fget_Collapsed       : Boolean ; override;
      procedure fset_Collapsed       ( const _value   : Boolean
                                     ) ; override;
      function  fget_ZOrder          : Integer ; override;
      procedure fset_ZOrder          ( const _value   : Integer
                                     ) ; override;
      function  fget_ZOrderEx        : Integer ; override;
      procedure fset_ZOrderEx        ( const _value   : Integer
                                     ) ; override;
      function  fget_UseConfig       : Boolean ; override;
      procedure fset_UseConfig       ( const _value   : Boolean
                                     ) ; override;
      function  fget_ConfigName      : String ; override;
      procedure fset_ConfigName      ( const _value   : String
                                     ) ; override;
      function  fget_CachedPaint     : Boolean ; override;
      procedure fset_CachedPaint     ( const _value   : Boolean
                                     ) ; override;
      function  fget_Transparency    : Integer ; override;
      procedure fset_Transparency    ( const _value   : Integer
                                     ) ; override;
      function  fget_Addition        : Integer ; override;
      procedure fset_Addition        ( const _value   : Integer
                                     ) ; override;
      function  fget_DormantMode     : TGIS_LayerDormantMode ; override;
      procedure fset_DormantMode     ( const _value   : TGIS_LayerDormantMode
                                     ) ; override;
      function  fget_CodePage        : Integer ; override;
      procedure fset_CodePage        ( const _value   : Integer
                                     ) ; override;
      function  fget_SubLayers       : TGIS_LayerAbstractList ; override;
      procedure fset_SubLayers       ( const _value   : TGIS_LayerAbstractList
                                     ) ; override;
      function  fget_ParentLayer     : TGIS_Layer ; override;
      procedure fset_ParentLayer     ( const _value   : TGIS_Layer
                                     ) ; override;
      function  fget_CS              : TGIS_CSCoordinateSystem ; override;
      procedure fset_CS              ( const _value   : TGIS_CSCoordinateSystem
                                     ) ; override;
      function  fget_MultiUserMode   : TGIS_MultiUser ; override;
      procedure fset_MultiUserMode   ( const _value   : TGIS_MultiUser
                                     ) ; override;
      function  fget_IsReadOnly      : Boolean ; override;
      function  fget_Params          : TGIS_ParamsSection ; override;
      procedure fset_Params          ( const _value   : TGIS_ParamsSection
                                     ) ; override;
      function  fget_ParamsList      : TGIS_ParamsList ; override;
      function  fget_Transform       : TGIS_Transform ; override;
      procedure fset_Transform       ( const _value : TGIS_Transform
                                     ) ; override;
      function  fget_Renderer        : TObject ; override;
      procedure fset_Renderer        ( const _value : TObject
                                     ) ; override;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      procedure setUp          ; override;

      /// <inheritdoc/>
      procedure setUp2         ; override;

      /// <inheritdoc/>
      procedure setUp3         ; override;

      /// <inheritdoc/>
      procedure applyConfigProjection
                                  ( const _cfg : TGIS_ConfigAbstract
                                  ) ; override;

      /// <inheritdoc/>
      procedure applyConfigOptions( const _cfg : TGIS_ConfigAbstract
                                  ) ; override;

      /// <inheritdoc/>
      procedure storeConfigOptions( const _cfg : TGIS_ConfigAbstract
                                  ) ; override;
    public
      /// <inheritdoc/>
      procedure Prepare            ; override;

      /// <inheritdoc/>
      procedure Open               ; override;

      /// <inheritdoc/>
      procedure ReOpen             ; override;


      /// <inheritdoc/>
      procedure Move               ( const _step    : Integer
                                   ) ; override;


      /// <inheritdoc/>
      procedure MoveEx             ( const _step    : Integer
                                   ) ; override;

      /// <inheritdoc/>
      procedure ReadConfig         ; override;

      /// <inheritdoc/>
      function  ReadConfigParam    ( const _name    : String
                                   ) : String ; override;

      /// <inheritdoc/>
      procedure RereadConfig       ; override;

      /// <inheritdoc/>
      procedure WriteConfig        ; override;

      /// <inheritdoc/>
      function  DrawEx             ( const _extent : TGIS_Extent
                                   ) : Boolean ; override;

      /// <inheritdoc/>
      procedure DrawFlash          ; override;

      /// <inheritdoc/>
      procedure SaveData           ; override;

      /// <inheritdoc/>
      procedure SaveAll            ; override;

      /// <inheritdoc/>
      function  MustSave           : Boolean ; override;

      /// <inheritdoc/>
      procedure RecalcExtent       ; override;

      /// <inheritdoc/>
      procedure RecalcProjectedExtent
                                   ; override;

      /// <inheritdoc/>
      procedure Lock               ; override;

      /// <inheritdoc/>
      procedure Unlock             ; override;

      /// <inheritdoc/>
      procedure Project_Ref        ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point
                                   ) ; override;

      /// <inheritdoc/>
      procedure Unproject_Ref      ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point
                                   ) ; override;

      /// <inheritdoc/>
      procedure Project3D_Ref      ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                   ) ; override;

      /// <inheritdoc/>
      procedure Unproject3D_Ref    ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                   ) ; override;

      /// <inheritdoc/>
      function  Project            ( const _ptg     : TGIS_Point
                                   ) : TGIS_Point ; override;

      /// <inheritdoc/>
      function  Project3D          ( const _ptg     : TGIS_Point3D
                                   ) : TGIS_Point3D ; override;

      /// <inheritdoc/>
      function  Unproject          ( const _ptg     : TGIS_Point
                                   ) : TGIS_Point ; override;

      /// <inheritdoc/>
      function  Unproject3D        ( const _ptg     : TGIS_Point3D
                                   ) : TGIS_Point3D ; override;

      /// <inheritdoc/>
      procedure Alive              ; override;

      /// <inheritdoc/>
      function  DormantGain        : Integer ; override;
      /// <inheritdoc/>
      procedure Dormant            ; override;

      /// <inheritdoc/>
      function  PreRecognize       ( const _path     : String ;
                                       var _new_path : String
                                   ) : Boolean ; override;

      /// <inheritdoc/>
      procedure SetCSByWKT         ( const _wkt     : String
                                   ) ; override;

      /// <inheritdoc/>
      procedure SetCSByEPSG        ( const _epsg    : Integer
                                   ) ; override;

      /// <inheritdoc/>
      procedure SetCSByWKTFile     ( const _path    : String
                                   ) ; override;
    end ;

    /// <summary>
    ///   Sublayer vector class represents a 'virtual' views of the normal
    ///   layer. Sublayer offers only minimal set of native properties( Like
    ///   Name, Caption) while other calls will be forwarded to the parent layer.
    /// </summary>
    TGIS_LayerSublayerVector = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_LayerVector )
      private
        FUseParentParams : Boolean ;
      {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // property access routines

        function  fget_Path            : String ; override;
        procedure fset_Path            ( const _value   : String
                                       ) ; override;
        function  fget_CS              : TGIS_CSCoordinateSystem ; override;
        procedure fset_CS              ( const _value   : TGIS_CSCoordinateSystem
                                       ) ; override;
        function  fget_PathRTree       : String ; override;
        function  fget_UseRTree        : Boolean ; override;
        function  fget_RTree           : TGIS_RTree ; override;
        function  fget_Items           : TGIS_ShapeList ; override;
        function  fget_Fields          : TGIS_FieldInfoList ; override;
        procedure fset_Active          ( const _value : Boolean
                                       ) ; override;
        function  fget_Stream          : TStream ; override;
        procedure fset_Stream          ( const _value   : TStream
                                       ) ; override;
        function  fget_Caption         : String ; override;
        procedure fset_Caption         ( const _value   : String
                                       ) ; override;
        function  fget_Tag             : Integer ; override;
        procedure fset_Tag             ( const _value   : Integer
                                       ) ; override;
        {$IFDEF MANAGED}
          function  fget_TagPointer    : Object ; override;
          procedure fset_TagPointer    ( const _value   : Object
                                       ) ; override;
        {$ELSE}
          function  fget_TagPointer    : Pointer ; override;
          procedure fset_TagPointer    ( const _value   : Pointer
                                       ) ; override;
        {$ENDIF}
        function  fget_TagInternal     : Integer ; override;
        procedure fset_TagInternal     ( const _value   : Integer
                                       ) ; override;
        function  fget_Name            : String ; override;
        procedure fset_Name            ( const _value   : String
                                       ) ; override;
        function  fget_Basemap         : Boolean ; override;
        procedure fset_Basemap         ( const _value : Boolean
                                       ) ; override;
        function  fget_FileInfo        : String ; override;
        function  fget_Comments        : String ; override;
        procedure fset_Comments        ( const _value   : String
                                       ) ; override;
        function  fget_IsLocked        : Boolean ; override;
        function  fget_IsOpened        : Boolean ; override;
        function  fget_DirectMode      : Boolean ; override;
        {$IFDEF CLR}
          function  fget_Age           : DateTime ; override;
        {$ELSE}
          function  fget_Age           : TDateTime ; override;
        {$ENDIF}
        function  fget_SubType         : TGIS_LayerSubTypeSet ; override;
        function  fget_Viewer          : TGIS_ViewerRef ; override;
        procedure fset_Viewer          ( const _value   : TGIS_ViewerRef
                                       ) ; override;
        function  fget_ConfigFile      : TGIS_ConfigAbstract ; override;
        function  fget_Extent          : TGIS_Extent ; override;
        procedure fset_Extent          ( const _value   : TGIS_Extent
                                       ) ; override;
        function  fget_ProjectedExtent : TGIS_Extent ; override;
        procedure fset_ProjectedExtent ( const _value   : TGIS_Extent
                                       ) ; override;
        function  fget_Active          : Boolean ; override;
        function  fget_HideFromLegend  : Boolean ; override;
        procedure fset_HideFromLegend  ( const _value   : Boolean
                                       ) ; override;
        function  fget_Collapsed       : Boolean ; override;
        procedure fset_Collapsed       ( const _value   : Boolean
                                       ) ; override;
        function  fget_ZOrder          : Integer ; override;
        procedure fset_ZOrder          ( const _value   : Integer
                                       ) ; override;
        function  fget_ZOrderEx        : Integer ; override;
        procedure fset_ZOrderEx        ( const _value   : Integer
                                       ) ; override;
        function  fget_UseConfig       : Boolean ; override;
        procedure fset_UseConfig       ( const _value   : Boolean
                                       ) ; override;
        function  fget_ConfigName      : String ; override;
        procedure fset_ConfigName      ( const _value   : String
                                       ) ; override;
        function  fget_CachedPaint     : Boolean ; override;
        procedure fset_CachedPaint     ( const _value   : Boolean
                                       ) ; override;
        function  fget_Transparency    : Integer ; override;
        procedure fset_Transparency    ( const _value   : Integer
                                       ) ; override;
        function  fget_Addition        : Integer ; override;
        procedure fset_Addition        ( const _value   : Integer
                                       ) ; override;
        function  fget_DormantMode     : TGIS_LayerDormantMode ; override;
        procedure fset_DormantMode     ( const _value   : TGIS_LayerDormantMode
                                       ) ; override;
        function  fget_CodePage        : Integer ; override;
        procedure fset_CodePage        ( const _value   : Integer
                                       ) ; override;
        function  fget_SubLayers       : TGIS_LayerAbstractList ; override;
        procedure fset_SubLayers       ( const _value   : TGIS_LayerAbstractList
                                       ) ; override;
        function  fget_ParentLayer     : TGIS_Layer ; override;
        procedure fset_ParentLayer     ( const _value   : TGIS_Layer
                                       ) ; override;
        function  fget_MultiUserMode   : TGIS_MultiUser ; override;
        procedure fset_MultiUserMode   ( const _value   : TGIS_MultiUser
                                       ) ; override;
        function  fget_IsReadOnly      : Boolean ; override;
        function  fget_Params          : TGIS_ParamsSection ; override;
        procedure fset_Params          ( const _value   : TGIS_ParamsSection
                                       ) ; override;
        function  fget_ParamsList      : TGIS_ParamsList ; override;
        function  fget_Transform       : TGIS_Transform ; override;
        procedure fset_Transform       ( const _value : TGIS_Transform
                                       ) ; override;
        function  fget_SelectedList    : TGIS_SelectedShapeList ; override;
      public
        /// <inheritdoc/>
         constructor Create           ; override;

        /// <inheritdoc/>
        function  FindFirst           ( const _extent      : TGIS_Extent ;
                                        const _query       : String      ;
                                        const _shape       : TGIS_Shape  ;
                                        const _de9im       : String      ;
                                        const _skipDeleted : Boolean
                                       ) : TGIS_Shape ; override ;

        /// <inheritdoc/>
        procedure Deselect           ( const _uid         : TGIS_Uid
                                     ) ; override;

        /// <inheritdoc/>
        procedure DeselectAll        ; override;

        /// <inheritdoc/>
        function  FindNext            : TGIS_Shape ; override ;

        /// <inheritdoc/>
        function  FindField           ( const _name    : String
                                      ) : Integer ; override;

        /// <inheritdoc/>
        function  FieldInfo           ( const _id      : Integer
                                      ) : TGIS_FieldInfo ; override;

        /// <inheritdoc/>
        procedure Open                ; override;

        /// <inheritdoc/>
        procedure Move                ( const _step    : Integer
                                      ) ; override;

        /// <inheritdoc/>
        procedure MoveEx              ( const _step    : Integer
                                      ) ; override;

        /// <inheritdoc/>
        procedure ReadConfig          ; override;

        /// <inheritdoc/>
        procedure RereadConfig        ; override;

        /// <inheritdoc/>
        procedure WriteConfig         ; override;

        {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
        /// <inheritdoc/>
        procedure BindSqlQueryInternal( const _sqlquery : TGIS_SqlQuery
                                      ) ; override ;
        /// <inheritdoc/>
        function  DrawEx              ( const _extent  : TGIS_Extent
                                      ) : Boolean ; override;

        /// <inheritdoc/>
        procedure FlashShape          ( const _uid     : TGIS_Uid ;
                                        const _times   : Integer ;
                                        const _delay   : Integer
                                      ) ; override;

        /// <inheritdoc/>
        procedure SaveData            ; override;

        /// <inheritdoc/>
        procedure SaveAll             ; override;

        /// <inheritdoc/>
        function  MustSave            : Boolean ; override;

        /// <inheritdoc/>
        procedure RecalcExtent        ; override;

        /// <inheritdoc/>
        procedure RecalcProjectedExtent
                                      ; override;

        /// <inheritdoc/>
        procedure Lock                ; override;

        /// <inheritdoc/>
        procedure Unlock              ; override;

        /// <inheritdoc/>
        procedure Project_Ref         ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point
                                      ) ; override;

        /// <inheritdoc/>
        procedure Unproject_Ref       ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point
                                      ) ; override;

        /// <inheritdoc/>
        procedure Project3D_Ref       ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                      ) ; override;

        /// <inheritdoc/>
        procedure Unproject3D_Ref     ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                      ) ; override;

        /// <inheritdoc/>
        function  Project             ( const _ptg     : TGIS_Point
                                      ) : TGIS_Point ; override;

        /// <inheritdoc/>
        function  Project3D           ( const _ptg     : TGIS_Point3D
                                      ) : TGIS_Point3D ; override;


        /// <inheritdoc/>
        function  Unproject           ( const _ptg     : TGIS_Point
                                      ) : TGIS_Point ; override;


        /// <inheritdoc/>
        function  Unproject3D         ( const _ptg     : TGIS_Point3D
                                      ) : TGIS_Point3D ; override;

        /// <inheritdoc/>
        procedure Alive               ; override;

        /// <inheritdoc/>
        procedure Dormant             ; override;


        /// <inheritdoc/>
        function  PreRecognize        ( const _path     : String ;
                                        var   _new_path : String
                                      ) : Boolean ; override;

        /// <inheritdoc/>
        procedure SetCSByWKT          ( const _wkt      : String
                                      ) ; override;

        /// <inheritdoc/>
        procedure SetCSByEPSG         ( const _epsg     : Integer
                                      ) ; override;

        /// <inheritdoc/>
        procedure SetCSByWKTFile      ( const _path     : String
                                      ) ; override;
      public
        /// <summary>
        ///   if true, Params from parent layer will be used.
        /// </summary>
        property UseParentParams : Boolean  read  FUseParentParams
                                            write FUseParentParams ;
    end ;

    /// <summary>
    ///   Sublayer pixel class represents a 'virtual' views of the normal
    ///   layer. Sublayer offers only minimal set of native properties( Like
    ///   Name, Caption) while other calls will be forwarded to the parent layer.
    /// </summary>
    TGIS_LayerSublayerPixel = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_LayerPixel )
      private
        FUseParentParams : Boolean ;
      {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // property access routines

        function  fget_Path            : String ; override;
        procedure fset_Path            ( const _value   : String
                                       ) ; override;
        function  fget_CS              : TGIS_CSCoordinateSystem ; override;
        procedure fset_CS              ( const _value   : TGIS_CSCoordinateSystem
                                       ) ; override;
        procedure fset_Active          ( const _value : Boolean
                                       ) ; override;
        function  fget_Stream          : TStream ; override;
        procedure fset_Stream          ( const _value   : TStream
                                       ) ; override;
        function  fget_Caption         : String ; override;
        procedure fset_Caption         ( const _value   : String
                                       ) ; override;
        function  fget_Tag             : Integer ; override;
        procedure fset_Tag             ( const _value   : Integer
                                       ) ; override;
        {$IFDEF MANAGED}
          function  fget_TagPointer    : Object ; override;
          procedure fset_TagPointer    ( const _value   : Object
                                       ) ; override;
        {$ELSE}
          function  fget_TagPointer    : Pointer ; override;
          procedure fset_TagPointer    ( const _value   : Pointer
                                       ) ; override;
        {$ENDIF}
        function  fget_TagInternal     : Integer ; override;
        procedure fset_TagInternal     ( const _value   : Integer
                                       ) ; override;
        function  fget_Name            : String ; override;
        procedure fset_Name            ( const _value   : String
                                       ) ; override;
        function  fget_Basemap         : Boolean ; override;
        procedure fset_Basemap         ( const _value : Boolean
                                       ) ; override;
        function  fget_FileInfo        : String ; override;
        function  fget_Comments        : String ; override;
        procedure fset_Comments        ( const _value   : String
                                       ) ; override;
        function  fget_IsLocked        : Boolean ; override;
        function  fget_IsOpened        : Boolean ; override;
        function  fget_DirectMode      : Boolean ; override;
        {$IFDEF CLR}
          function  fget_Age           : DateTime ; override;
        {$ELSE}
          function  fget_Age           : TDateTime ; override;
        {$ENDIF}
        function  fget_SubType         : TGIS_LayerSubTypeSet ; override;
        function  fget_Viewer          : TGIS_ViewerRef ; override;
        procedure fset_Viewer          ( const _value   : TGIS_ViewerRef
                                       ) ; override;
        function  fget_ConfigFile      : TGIS_ConfigAbstract ; override;
        function  fget_Extent          : TGIS_Extent ; override;
        procedure fset_Extent          ( const _value   : TGIS_Extent
                                       ) ; override;
        function  fget_ProjectedExtent : TGIS_Extent ; override;
        procedure fset_ProjectedExtent ( const _value   : TGIS_Extent
                                       ) ; override;
        function  fget_Active          : Boolean ; override;
        function  fget_HideFromLegend  : Boolean ; override;
        procedure fset_HideFromLegend  ( const _value   : Boolean
                                       ) ; override;
        function  fget_Collapsed       : Boolean ; override;
        procedure fset_Collapsed       ( const _value   : Boolean
                                       ) ; override;
        function  fget_ZOrder          : Integer ; override;
        procedure fset_ZOrder          ( const _value   : Integer
                                       ) ; override;
        function  fget_ZOrderEx        : Integer ; override;
        procedure fset_ZOrderEx        ( const _value   : Integer
                                       ) ; override;
        function  fget_UseConfig       : Boolean ; override;
        procedure fset_UseConfig       ( const _value   : Boolean
                                       ) ; override;
        function  fget_ConfigName      : String ; override;
        procedure fset_ConfigName      ( const _value   : String
                                       ) ; override;
        function  fget_CachedPaint     : Boolean ; override;
        procedure fset_CachedPaint     ( const _value   : Boolean
                                       ) ; override;
        function  fget_Transparency    : Integer ; override;
        procedure fset_Transparency    ( const _value   : Integer
                                       ) ; override;
        function  fget_Addition        : Integer ; override;
        procedure fset_Addition        ( const _value   : Integer
                                       ) ; override;
        function  fget_DormantMode     : TGIS_LayerDormantMode ; override;
        procedure fset_DormantMode     ( const _value   : TGIS_LayerDormantMode
                                       ) ; override;
        function  fget_CodePage        : Integer ; override;
        procedure fset_CodePage        ( const _value   : Integer
                                       ) ; override;
        function  fget_SubLayers       : TGIS_LayerAbstractList ; override;
        procedure fset_SubLayers       ( const _value   : TGIS_LayerAbstractList
                                       ) ; override;
        function  fget_ParentLayer     : TGIS_Layer ; override;
        procedure fset_ParentLayer     ( const _value   : TGIS_Layer
                                       ) ; override;
        function  fget_MultiUserMode   : TGIS_MultiUser ; override;
        procedure fset_MultiUserMode   ( const _value   : TGIS_MultiUser
                                       ) ; override;
        function  fget_IsReadOnly      : Boolean ; override;
        function  fget_Params          : TGIS_ParamsSection ; override;
        procedure fset_Params          ( const _value   : TGIS_ParamsSection
                                       ) ; override;
        function  fget_ParamsList      : TGIS_ParamsList ; override;
        function  fget_Transform       : TGIS_Transform ; override;
        procedure fset_Transform       ( const _value : TGIS_Transform
                                       ) ; override;
      public
        /// <inheritdoc/>
         constructor Create           ; override;

        /// <inheritdoc/>
        procedure Open               ; override;

        /// <inheritdoc/>
        procedure Move               ( const _step    : Integer
                                     ) ; override;

        /// <inheritdoc/>
        procedure MoveEx             ( const _step    : Integer
                                     ) ; override;

        /// <inheritdoc/>
        procedure ReadConfig         ; override;

        /// <inheritdoc/>
        procedure RereadConfig       ; override;

        /// <inheritdoc/>
        procedure WriteConfig        ; override;

        /// <inheritdoc/>
        function  DrawEx             ( const _extent  : TGIS_Extent
                                     ) : Boolean ; override;

        /// <inheritdoc/>
        procedure SaveData           ; override;

        /// <inheritdoc/>
        procedure SaveAll            ; override;

        /// <inheritdoc/>
        function  MustSave           : Boolean ; override;

        /// <inheritdoc/>
        procedure RecalcExtent       ; override;

        /// <inheritdoc/>
        procedure RecalcProjectedExtent
                                     ; override;

        /// <inheritdoc/>
        procedure Lock               ; override;

        /// <inheritdoc/>
        procedure Unlock             ; override;

        /// <inheritdoc/>
        procedure Project_Ref        ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point
                                     ) ; override;

        /// <inheritdoc/>
        procedure Unproject_Ref      ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point
                                     ) ; override;

        /// <inheritdoc/>
        procedure Project3D_Ref      ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                     ) ; override;

        /// <inheritdoc/>
        procedure Unproject3D_Ref    ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                     ) ; override;

        /// <inheritdoc/>
        function  Project            ( const _ptg     : TGIS_Point
                                     ) : TGIS_Point ; override;

        /// <inheritdoc/>
        function  Project3D          ( const _ptg     : TGIS_Point3D
                                     ) : TGIS_Point3D ; override;


        /// <inheritdoc/>
        function  Unproject          ( const _ptg     : TGIS_Point
                                     ) : TGIS_Point ; override;


        /// <inheritdoc/>
        function  Unproject3D        ( const _ptg     : TGIS_Point3D
                                     ) : TGIS_Point3D ; override;

        /// <inheritdoc/>
        procedure Alive              ; override;

        /// <inheritdoc/>
        procedure Dormant            ; override;

        /// <inheritdoc/>
        function  GetBitmap          (  const _extent   : TGIS_Extent ;
                                        const _bitmap   : TGIS_Pixels ;
                                        const _width    : Integer ;
                                        const _height    : Integer
                                     ) : Boolean; override;

        /// <inheritdoc/>
        function  PreRecognize       ( const _path    : String ;
                                        var _new_path : String
                                     ) : Boolean ; override;

        /// <inheritdoc/>
        procedure SetCSByWKT         ( const _wkt     : String
                                     ) ; override;

        /// <inheritdoc/>
        procedure SetCSByEPSG        ( const _epsg    : Integer
                                     ) ; override;

        /// <inheritdoc/>
        procedure SetCSByWKTFile     ( const _path    : String
                                     ) ; override;
      public
        /// <summary>
        ///   if true, Params from parent layer will be used.
        /// </summary>
        property UseParentParams : Boolean  read  FUseParentParams
                                            write FUseParentParams ;
    end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoFunctions ;
{$ENDIF}

//==============================================================================
// Property Access
//==============================================================================

  function TGIS_LayerSublayer.fget_Path
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Path
    else
      Result := inherited fget_Path ;
  end ;

  procedure TGIS_LayerSublayer.fset_Path(
    const _value : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Path := _value
    else
      inherited fset_Path( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_PathWithDriver
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.PathWithDriver
    else
      Result := inherited fget_PathWithDriver ;
  end ;

  function TGIS_LayerSublayer.fget_Stream
    : TStream ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Stream
    else
      Result := inherited fget_Stream ;
  end ;

  procedure TGIS_LayerSublayer.fset_Stream(
    const _value : TStream
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Stream := _value
    else
      inherited fset_Stream( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Caption
    : String ;
  begin
    // use local property only
    Result := inherited fget_Caption ;
  end ;

  procedure TGIS_LayerSublayer.fset_Caption(
    const _value : String
  ) ;
  begin
    // use local property only
    inherited fset_Caption( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Tag
    : Integer ;
  begin
    // use local property only
    Result := inherited fget_Tag ;
  end ;

  procedure TGIS_LayerSublayer.fset_Tag(
    const _value : Integer
  ) ;
  begin
    // use local property only
    inherited fset_Tag( _value ) ;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_LayerSublayer.fget_TagPointer
      : Object ;
    begin
      // use local property only
      Result := inherited fget_TagPointer ;
    end ;

    procedure TGIS_LayerSublayer.fset_TagPointer(
      const _value : Object
    ) ;
    begin
      // use local property only
      inherited fset_TagPointer( _value ) ;
    end ;
  {$ELSE}

    function TGIS_LayerSublayer.fget_TagPointer
      : Pointer ;
    begin
      // use local property only
      Result := inherited fget_TagPointer ;
    end ;

    procedure TGIS_LayerSublayer.fset_TagPointer(
      const _value : Pointer
    ) ;
    begin
      // use local property only
      inherited fset_TagPointer( _value ) ;
    end ;
  {$ENDIF}

  function TGIS_LayerSublayer.fget_TagInternal
    : Integer ;
  begin
    // use local property only
    Result := inherited fget_TagInternal ;
  end ;

  procedure TGIS_LayerSublayer.fset_TagInternal(
    const _value : Integer
  ) ;
  begin
    // use local property only
    inherited fset_TagInternal( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_UserObject
    : TObject ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.UserObject
    else
      Result := inherited fget_UserObject ;
  end ;

  procedure TGIS_LayerSublayer.fset_UserObject(
    const _value : TObject
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.UserObject := _value
    else
      inherited fset_UserObject( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Name
    : String ;
  begin
    // use local property only
    Result := inherited fget_Name ;
  end ;

  procedure TGIS_LayerSublayer.fset_Name(
    const _value : String
  ) ;
  begin
    // use local property only
    inherited fset_Name( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Basemap
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Basemap
    else
      Result := inherited fget_Basemap ;
  end ;

  procedure TGIS_LayerSublayer.fset_Basemap(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Basemap := _value
    else
      inherited fset_Basemap( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_FileInfo
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.FileInfo
    else
      Result := inherited fget_FileInfo ;
  end ;

  function TGIS_LayerSublayer.fget_FileCopyrights
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.FileCopyrights
    else
      Result := inherited fget_FileCopyrights ;
  end ;

  function TGIS_LayerSublayer.fget_Comments
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Comments
    else
      Result := inherited fget_Comments ;
  end ;

  procedure TGIS_LayerSublayer.fset_Comments(
    const _value : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Comments := _value
    else
      inherited fset_Comments( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_IsLocked
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.IsLocked
    else
      Result := inherited fget_IsLocked ;
  end ;

  function TGIS_LayerSublayer.fget_IsOpened
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.IsOpened
    else
      Result := inherited fget_IsOpened ;
  end ;

  function TGIS_LayerSublayer.fget_DirectMode
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.DirectMode
    else
      Result := inherited fget_DirectMode ;
  end ;

  {$IFDEF CLR}
    function TGIS_LayerSublayer.fget_Age
      : DateTime ;
    begin
      if assigned( ParentLayer ) then
        Result := ParentLayer.Age
      else
        Result := inherited fget_Age ;
    end ;
  {$ELSE}

    function TGIS_LayerSublayer.fget_Age
      : TDateTime ;
    begin
      if assigned( ParentLayer ) then
        Result := ParentLayer.Age
      else
        Result := inherited fget_Age ;
    end ;
  {$ENDIF}

  function TGIS_LayerSublayer.fget_SubType
    : TGIS_LayerSubTypeSet ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.SubType
    else
      Result := inherited fget_SubType ;
  end ;

  function TGIS_LayerSublayer.fget_Viewer
    : TGIS_ViewerRef ;
  begin
    // use local property only
    Result := inherited fget_Viewer ;
  end ;

  procedure TGIS_LayerSublayer.fset_Viewer(
    const _value : TGIS_ViewerRef
  ) ;
  begin
    // use local and parent property
    if assigned( ParentLayer ) then
      ParentLayer.Viewer := _value ;

    inherited fset_Viewer( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_ConfigFile
    : TGIS_ConfigAbstract ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ConfigFile
    else
      Result := inherited fget_ConfigFile ;
  end ;

  function TGIS_LayerSublayer.fget_Extent
    : TGIS_Extent ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Extent
    else
      Result := inherited fget_Extent ;
  end ;

  procedure TGIS_LayerSublayer.fset_Extent(
    const _value : TGIS_Extent
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Extent := _value
    else
      inherited fset_Extent( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Extent3D
    : TGIS_Extent3D ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Extent3D
    else
      Result := inherited fget_Extent3D ;
  end ;

  procedure TGIS_LayerSublayer.fset_Extent3D(
    const _value : TGIS_Extent3D
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Extent3D := _value
    else
      inherited fset_Extent3D( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_ProjectedExtent
    : TGIS_Extent ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ProjectedExtent
    else
      Result := inherited fget_ProjectedExtent ;
  end ;

  procedure TGIS_LayerSublayer.fset_ProjectedExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ProjectedExtent := _value
    else
      inherited fset_ProjectedExtent( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Active
    : Boolean ;
  begin
    // use local property only
    Result := inherited fget_Active ;
  end ;

  procedure TGIS_LayerSublayer.fset_Active(
    const _value : Boolean
  ) ;
  begin
    // use local property only
    inherited fset_Active( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_HideFromLegend
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.HideFromLegend
    else
      Result := inherited fget_HideFromLegend ;
  end ;

  procedure TGIS_LayerSublayer.fset_HideFromLegend(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.HideFromLegend := _value
    else
      inherited fset_HideFromLegend( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Collapsed
    : Boolean ;
  begin
    // use local property only
    Result := inherited fget_Collapsed ;
  end ;

  procedure TGIS_LayerSublayer.fset_Collapsed(
    const _value : Boolean
  ) ;
  begin
    // use local property only
    inherited fset_Collapsed( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_ZOrder
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ZOrder
    else
      Result := inherited fget_ZOrder ;
  end ;

  procedure TGIS_LayerSublayer.fset_ZOrder(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ZOrder := _value
    else
      inherited fset_ZOrder( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_ZOrderEx
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ZOrderEx
    else
      Result := inherited fget_ZOrderEx ;
  end ;

  procedure TGIS_LayerSublayer.fset_ZOrderEx(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ZOrderEx := _value
    else
      inherited fset_ZOrderEx( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_UseConfig
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.UseConfig
    else
      Result := inherited fget_UseConfig ;
  end ;

  procedure TGIS_LayerSublayer.fset_UseConfig(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.UseConfig := _value
    else
      inherited fset_UseConfig( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_ConfigName
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Path
    else
      Result := inherited fget_Path ;
  end ;

  procedure TGIS_LayerSublayer.fset_ConfigName(
    const _value : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ConfigName := _value
    else
      inherited fset_ConfigName( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_CachedPaint
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.CachedPaint
    else
      Result := inherited fget_CachedPaint ;
  end ;

  procedure TGIS_LayerSublayer.fset_CachedPaint(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.CachedPaint := _value
    else
      inherited fset_CachedPaint( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Transparency
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Transparency
    else
      Result := inherited fget_Transparency ;
  end ;

  procedure TGIS_LayerSublayer.fset_Transparency(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Transparency := _value
    else
      inherited fset_Transparency( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Addition
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Addition
    else
      Result := inherited fget_Addition ;
  end ;

  procedure TGIS_LayerSublayer.fset_Addition(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Addition := _value
    else
      inherited fset_Addition( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_DormantMode
    : TGIS_LayerDormantMode ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.DormantMode
    else
      Result := inherited fget_DormantMode ;
  end ;

  procedure TGIS_LayerSublayer.fset_DormantMode(
    const _value : TGIS_LayerDormantMode
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.DormantMode := _value
    else
      inherited fset_DormantMode( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_CodePage
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.CodePage
    else
      Result := inherited fget_CodePage ;
  end ;

  procedure TGIS_LayerSublayer.fset_CodePage(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.CodePage := _value
    else
      inherited fset_CodePage( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_SubLayers
    : TGIS_LayerAbstractList ;
  begin
    // use local property only
    Result := inherited fget_SubLayers ;
  end ;

  procedure TGIS_LayerSublayer.fset_SubLayers(
    const _value : TGIS_LayerAbstractList
  ) ;
  begin
    // use local property only
    inherited fset_SubLayers( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_ParentLayer
    : TGIS_Layer ;
  begin
    // use local property only
    Result := inherited fget_ParentLayer ;
  end ;

  procedure TGIS_LayerSublayer.fset_ParentLayer(
    const _value : TGIS_Layer
  ) ;
  begin
    // use local property only
    inherited fset_ParentLayer( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_CS
    : TGIS_CSCoordinateSystem ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.CS
    else
      Result := inherited fget_CS ;
  end ;

  procedure TGIS_LayerSublayer.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.CS:= _value
    else
      inherited fset_CS( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_MultiUserMode
    : TGIS_MultiUser ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.MultiUserMode
    else
      Result := inherited fget_MultiUserMode ;
  end ;

  procedure TGIS_LayerSublayer.fset_MultiUserMode(
    const _value : TGIS_MultiUser
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.MultiUserMode := _value
    else
      inherited fset_MultiUserMode( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_IsReadOnly
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.IsReadOnly
    else
      Result := inherited fget_IsReadOnly ;
  end ;

  function TGIS_LayerSublayer.fget_Params
    : TGIS_ParamsSection ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Params
    else
      Result := inherited fget_Params ;
  end ;

  procedure TGIS_LayerSublayer.fset_Params(
    const _value : TGIS_ParamsSection
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Params := _value
    else
      inherited fset_Params( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_ParamsList
    : TGIS_ParamsList ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ParamsList
    else
      Result := inherited fget_ParamsList ;
  end ;

  function TGIS_LayerSublayer.fget_Transform
    : TGIS_Transform ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Transform
    else
      Result := inherited fget_Transform ;
  end ;

  procedure TGIS_LayerSublayer.fset_Transform(
    const _value : TGIS_Transform
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Transform := _value
    else
      inherited fset_Transform( _value ) ;
  end ;

  function TGIS_LayerSublayer.fget_Renderer
    : TObject ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Renderer
    else
      Result := inherited fget_Renderer ;
  end ;

  procedure TGIS_LayerSublayer.fset_Renderer(
    const _value : TObject
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Renderer := _value
    else
      inherited fset_Renderer( _value ) ;
  end ;

  procedure TGIS_LayerSublayer.setUp ;
  begin
    inherited setUp ;
  end ;

  procedure TGIS_LayerSublayer.setUp2 ;
  begin
    inherited setUp2 ;
  end ;

  procedure TGIS_LayerSublayer.setUp3 ;
  begin
    inherited setUp3 ;
  end ;

  procedure TGIS_LayerSublayer.applyConfigProjection(
    const _cfg : TGIS_ConfigAbstract
  ) ;
  begin
    inherited applyConfigProjection( _cfg ) ;
  end ;

  procedure TGIS_LayerSublayer.applyConfigOptions(
    const _cfg : TGIS_ConfigAbstract
  ) ;
  begin
    inherited applyConfigOptions( _cfg ) ;
  end ;

  procedure TGIS_LayerSublayer.storeConfigOptions(
    const _cfg : TGIS_ConfigAbstract
  ) ;
  begin
    inherited storeConfigOptions( _cfg ) ;
  end ;

  procedure TGIS_LayerSublayer.Prepare ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Prepare
    else
      inherited Prepare ;
  end ;

  procedure TGIS_LayerSublayer.ReOpen ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ReOpen
    else
      inherited ReOpen ;
  end ;

  function TGIS_LayerSublayer.ReadConfigParam(
    const _name : String
  ) : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ReadConfigParam( _name )
    else
      inherited ReadConfigParam( _name ) ;
  end ;

  procedure TGIS_LayerSublayer.DrawFlash ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.DrawFlash
    else
      inherited DrawFlash ;
  end ;

//==============================================================================
// Public Methods
//==============================================================================

  procedure TGIS_LayerSublayer.Open ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Open ;
  end ;

  procedure TGIS_LayerSublayer.Move(
    const _step : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Move( _step ) ;
  end ;

  procedure TGIS_LayerSublayer.MoveEx(
    const _step : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.MoveEx( _step ) ;
  end ;

  procedure TGIS_LayerSublayer.ReadConfig ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ReadConfig ;
  end ;

  procedure TGIS_LayerSublayer.RereadConfig ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.RereadConfig ;
  end ;

  procedure TGIS_LayerSublayer.WriteConfig ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.WriteConfig ;
  end ;

  function TGIS_LayerSublayer.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.DrawEx( _extent )
    else
      Result := False ;
  end ;

  procedure TGIS_LayerSublayer.SaveData ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SaveData ;
  end ;

  procedure TGIS_LayerSublayer.SaveAll ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SaveAll ;
  end ;

  function TGIS_LayerSublayer.MustSave : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.MustSave
    else
      Result := False ;
  end ;

  procedure TGIS_LayerSublayer.RecalcExtent ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.RecalcExtent ;
  end ;

  procedure TGIS_LayerSublayer.RecalcProjectedExtent ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.RecalcProjectedExtent ;
  end ;

  procedure TGIS_LayerSublayer.Lock ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Lock ;
  end ;

  procedure TGIS_LayerSublayer.Unlock ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Unlock ;
  end ;

  procedure TGIS_LayerSublayer.Project_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Project_Ref( _ptg ) ;
  end ;

  procedure TGIS_LayerSublayer.Unproject_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Unproject_Ref( _ptg ) ;
  end ;

  procedure TGIS_LayerSublayer.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Project3D_Ref( _ptg ) ;
  end ;

  procedure TGIS_LayerSublayer.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Unproject3D_Ref( _ptg ) ;
  end ;

  function TGIS_LayerSublayer.Project(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Project( _ptg )
    else
      Result := GisPoint( 0, 0 ) ;
  end ;

  function TGIS_LayerSublayer.Project3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Project3D( _ptg )
    else
      Result := GisPoint3D( 0, 0, 0, 0 ) ;
  end ;

  function TGIS_LayerSublayer.Unproject(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Unproject( _ptg )
    else
      Result := GisPoint( 0, 0 ) ;
  end ;

  function TGIS_LayerSublayer.Unproject3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Unproject3D( _ptg )
    else
      Result := GisPoint3D( 0, 0, 0, 0 ) ;
  end ;

  procedure TGIS_LayerSublayer.Alive ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Alive ;
  end ;

  function TGIS_LayerSublayer.DormantGain
    : Integer ;
  var
    i   : Integer ;
  begin
    Result := 0 ;

    for i := 0 to SubLayers.Count - 1 do
      Result := Result + TGIS_Layer( SubLayers[i] ).DormantGain ;
  end ;

  procedure TGIS_LayerSublayer.Dormant ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Dormant ;
  end ;

  function TGIS_LayerSublayer.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.PreRecognize( _path, _new_path )
    else
      Result := False ;
  end ;

  procedure TGIS_LayerSublayer.SetCSByWKT(
    const _wkt : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SetCSByWKT( _wkt ) ;
  end ;

  procedure TGIS_LayerSublayer.SetCSByEPSG(
    const _epsg : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SetCSByEPSG( _epsg ) ;
  end ;

  procedure TGIS_LayerSublayer.SetCSByWKTFile(
    const _path : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SetCSByWKTFile( _path ) ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Active(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      inherited fset_Active( _value )
    else
      inherited fset_Active( _value ) ;
  end;

  function TGIS_LayerSublayerVector.fget_PathRTree : String;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      Result := TGIS_LayerVector( ParentLayer ).PathRTree
    else
      Result := inherited fget_PathRTree ;
  end;

  function TGIS_LayerSublayerVector.fget_RTree : TGIS_RTree ;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      Result := TGIS_LayerVector( ParentLayer ).RTreeFile
    else
      Result := inherited fget_RTree ;
  end;

  function TGIS_LayerSublayerVector.fget_UseRTree: Boolean;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      Result := TGIS_LayerVector( ParentLayer ).UseRTree
    else
      Result := inherited fget_UseRTree ;
  end;

  function TGIS_LayerSublayerVector.fget_Items : TGIS_ShapeList ;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      Result := TGIS_LayerVector( ParentLayer ).Items
    else
      Result := inherited fget_Items ;
  end;

  function TGIS_LayerSublayerVector.fget_Fields : TGIS_FieldInfoList ;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      Result := TGIS_LayerVector( ParentLayer ).Fields
    else
      Result := inherited fget_Fields ;
  end;

  function TGIS_LayerSublayerVector.fget_Path
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Path
    else
      Result := inherited fget_Path ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Path(
    const _value : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Path := _value
    else
      inherited fset_Path( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_Stream
    : TStream ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Stream
    else
      Result := inherited fget_Stream ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Stream(
    const _value : TStream
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Stream := _value
    else
      inherited fset_Stream( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_Caption
    : String ;
  begin
    // use local property only
    Result := inherited fget_Caption ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Caption(
    const _value : String
  ) ;
  begin
    // use local property only
    inherited fset_Caption( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_Tag
    : Integer ;
  begin
    // use local property only
    Result := inherited fget_Tag ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Tag(
    const _value : Integer
  ) ;
  begin
    // use local property only
    inherited fset_Tag( _value ) ;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_LayerSublayerVector.fget_TagPointer
      : Object ;
    begin
      // use local property only
      Result := inherited fget_TagPointer ;
    end ;

    procedure TGIS_LayerSublayerVector.fset_TagPointer(
      const _value : Object
    ) ;
    begin
      // use local property only
      inherited fset_TagPointer( _value ) ;
    end ;
  {$ELSE}

    function TGIS_LayerSublayerVector.fget_TagPointer
      : Pointer ;
    begin
      // use local property only
      Result := inherited fget_TagPointer ;
    end ;

    procedure TGIS_LayerSublayerVector.fset_TagPointer(
      const _value : Pointer
    ) ;
    begin
      // use local property only
      inherited fset_TagPointer( _value ) ;
    end ;
  {$ENDIF}

  function TGIS_LayerSublayerVector.fget_TagInternal
    : Integer ;
  begin
    // use local property only
    Result := inherited fget_TagInternal ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_TagInternal(
    const _value : Integer
  ) ;
  begin
    // use local property only
    inherited fset_TagInternal( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_Name
    : String ;
  begin
    // use local property only
    Result := inherited fget_Name ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Name(
    const _value : String
  ) ;
  begin
    // use local property only
    inherited fset_Name( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_Basemap
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Basemap
    else
      Result := inherited fget_Basemap ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Basemap(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Basemap := _value
    else
      inherited fset_Basemap( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_FileInfo
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.FileInfo
    else
      Result := inherited fget_FileInfo ;
  end ;

  function TGIS_LayerSublayerVector.fget_Comments
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Comments
    else
      Result := inherited fget_Comments ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Comments(
    const _value : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Comments := _value
    else
      inherited fset_Comments( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_IsLocked
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.IsLocked
    else
      Result := inherited fget_IsLocked ;
  end ;

  function TGIS_LayerSublayerVector.fget_IsOpened
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.IsOpened
    else
      Result := inherited fget_IsOpened ;
  end ;

  function TGIS_LayerSublayerVector.fget_DirectMode
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.DirectMode
    else
      Result := inherited fget_DirectMode ;
  end ;

  {$IFDEF CLR}
    function TGIS_LayerSublayerVector.fget_Age
      : DateTime ;
    begin
      if assigned( ParentLayer ) then
        Result := ParentLayer.Age
      else
        Result := inherited fget_Age ;
    end ;
  {$ELSE}

    function TGIS_LayerSublayerVector.fget_Age
      : TDateTime ;
    begin
      if assigned( ParentLayer ) then
        Result := ParentLayer.Age
      else
        Result := inherited fget_Age ;
    end ;
  {$ENDIF}

  function TGIS_LayerSublayerVector.fget_SubType
    : TGIS_LayerSubTypeSet ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.SubType
    else
      Result := inherited fget_SubType ;
  end ;

  function TGIS_LayerSublayerVector.fget_Viewer
    : TGIS_ViewerRef ;
  begin
    // use local property only
    Result := inherited fget_Viewer ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Viewer(
    const _value : TGIS_ViewerRef
  ) ;
  begin
    // use local and parent property
    if assigned( ParentLayer ) then
      ParentLayer.Viewer := _value ;

    inherited fset_Viewer( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_ConfigFile
    : TGIS_ConfigAbstract ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ConfigFile
    else
      Result := inherited fget_ConfigFile ;
  end ;

  function TGIS_LayerSublayerVector.fget_Extent
    : TGIS_Extent ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Extent
    else
      Result := inherited fget_Extent ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Extent(
    const _value : TGIS_Extent
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Extent := GisMaxExtent( ParentLayer.Extent, _value )
    else
      inherited fset_Extent( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_ProjectedExtent
    : TGIS_Extent ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ProjectedExtent
    else
      Result := inherited fget_ProjectedExtent ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_ProjectedExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ProjectedExtent := GisMaxExtent( ParentLayer.ProjectedExtent, _value )
    else
      inherited fset_ProjectedExtent( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_Active
    : Boolean ;
  begin
    // use local property only
    Result := inherited fget_Active ;
  end ;

  function TGIS_LayerSublayerVector.fget_HideFromLegend
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.HideFromLegend
    else
      Result := inherited fget_HideFromLegend ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_HideFromLegend(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.HideFromLegend := _value
    else
      inherited fset_HideFromLegend( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_Collapsed
    : Boolean ;
  begin
    // use local property only
    Result := inherited fget_Collapsed ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Collapsed(
    const _value : Boolean
  ) ;
  begin
    // use local property only
    inherited fset_Collapsed( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_ZOrder
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ZOrder
    else
      Result := inherited fget_ZOrder ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_ZOrder(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ZOrder := _value
    else
      inherited fset_ZOrder( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_ZOrderEx
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ZOrderEx
    else
      Result := inherited fget_ZOrderEx ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_ZOrderEx(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ZOrderEx := _value
    else
      inherited fset_ZOrderEx( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_UseConfig
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.UseConfig
    else
      Result := inherited fget_UseConfig ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_UseConfig(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.UseConfig := _value
    else
      inherited fset_UseConfig( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_ConfigName
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Path
    else
      Result := inherited fget_Path ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_ConfigName(
    const _value : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ConfigName := _value
    else
      inherited fset_ConfigName( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_CachedPaint
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.CachedPaint
    else
      Result := inherited fget_CachedPaint ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_CachedPaint(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.CachedPaint := _value
    else
      inherited fset_CachedPaint( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_Transparency
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Transparency
    else
      Result := inherited fget_Transparency ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Transparency(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Transparency := _value
    else
      inherited fset_Transparency( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_Addition
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Addition
    else
      Result := inherited fget_Addition ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Addition(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Addition := _value
    else
      inherited fset_Addition( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_DormantMode
    : TGIS_LayerDormantMode ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.DormantMode
    else
      Result := inherited fget_DormantMode ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_DormantMode(
    const _value : TGIS_LayerDormantMode
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.DormantMode := _value
    else
      inherited fset_DormantMode( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_CodePage
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.CodePage
    else
      Result := inherited fget_CodePage ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_CodePage(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.CodePage := _value
    else
      inherited fset_CodePage( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_SubLayers
    : TGIS_LayerAbstractList ;
  begin
    // use local property only
    Result := inherited fget_SubLayers ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_SubLayers(
    const _value : TGIS_LayerAbstractList
  ) ;
  begin
    // use local property only
    inherited fset_SubLayers( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_ParentLayer
    : TGIS_Layer ;
  begin
    // use local property only
    Result := inherited fget_ParentLayer ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_ParentLayer(
    const _value : TGIS_Layer
  ) ;
  begin
    // use local property only
    inherited fset_ParentLayer( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_CS
    : TGIS_CSCoordinateSystem ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.CS
    else
      Result := inherited fget_CS ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.CS:= _value
    else
      inherited fset_CS( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_MultiUserMode
    : TGIS_MultiUser ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.MultiUserMode
    else
      Result := inherited fget_MultiUserMode ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_MultiUserMode(
    const _value : TGIS_MultiUser
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.MultiUserMode := _value
    else
      inherited fset_MultiUserMode( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_IsReadOnly
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.IsReadOnly
    else
      Result := inherited fget_IsReadOnly ;
  end ;

  function TGIS_LayerSublayerVector.fget_Params
    : TGIS_ParamsSection ;
  begin
    if assigned( ParentLayer ) and FUseParentParams then
      Result := ParentLayer.Params
    else
      Result := inherited fget_Params ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Params(
    const _value : TGIS_ParamsSection
  ) ;
  begin
    if assigned( ParentLayer ) and FUseParentParams then
      ParentLayer.Params := _value
    else
      inherited fset_Params( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_ParamsList
    : TGIS_ParamsList ;
  begin
    if assigned( ParentLayer ) and FUseParentParams then
      Result := ParentLayer.ParamsList
    else
      Result := inherited fget_ParamsList ;
  end ;

  function TGIS_LayerSublayerVector.fget_Transform
    : TGIS_Transform ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Transform
    else
      Result := inherited fget_Transform ;
  end ;

  procedure TGIS_LayerSublayerVector.fset_Transform(
    const _value : TGIS_Transform
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Transform := _value
    else
      inherited fset_Transform( _value ) ;
  end ;

  function TGIS_LayerSublayerVector.fget_SelectedList
    : TGIS_SelectedShapeList ;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      Result := TGIS_LayerVector( ParentLayer ).SelectedList
    else
      Result := inherited fget_SelectedList ;
  end ;

  function  TGIS_LayerSublayerVector.FindFirst(
    const _extent      : TGIS_Extent ;
    const _query       : String      ;
    const _shape       : TGIS_Shape  ;
    const _de9im       : String      ;
    const _skipDeleted : Boolean
  ) : TGIS_Shape ;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      Result := TGIS_LayerVector( ParentLayer ).FindFirst( _extent, _query, _shape, _de9im, _skipDeleted)
    else
      Result := inherited FindFirst( _extent, _query, _shape, _de9im, _skipDeleted);

    if assigned( Result ) and ( Result.Layer <> self ) then begin
      Result := FindNext ; // return only current sublayer
    end ;
  end ;

  function  TGIS_LayerSublayerVector.FindNext
    : TGIS_Shape ;
  begin
    while True do begin
      if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
        Result := TGIS_LayerVector( ParentLayer ).FindNext
      else
        Result := inherited FindNext ;

      if not assigned( Result ) or ( Result.Layer = self ) then begin
        // return only current sublayer
        break ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerSublayerVector.Deselect(
    const _uid : TGIS_Uid
  ) ;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      TGIS_LayerVector( ParentLayer ).Deselect(_uid)
    else
      inherited Deselect(_uid) ;
  end ;

  procedure TGIS_LayerSublayerVector.DeselectAll ;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      TGIS_LayerVector( ParentLayer ).DeselectAll
    else
      inherited DeselectAll ;
  end ;

  function TGIS_LayerSublayerVector.FieldInfo(const _id: Integer)
    : TGIS_FieldInfo;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      Result := TGIS_LayerVector( ParentLayer ).FieldInfo(_id)
    else
      Result := inherited FieldInfo(_id) ;
  end ;

  function TGIS_LayerSublayerVector.FindField(const _name: String): Integer;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      Result := TGIS_LayerVector( ParentLayer ).FindField(_name)
    else
      Result := inherited FindField(_name) ;
  end;

  procedure TGIS_LayerSublayerVector.Open ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Open ;
  end ;

  procedure TGIS_LayerSublayerVector.Move(
    const _step : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Move( _step ) ;
  end ;

  procedure TGIS_LayerSublayerVector.MoveEx(
    const _step : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.MoveEx( _step ) ;
  end ;

  procedure TGIS_LayerSublayerVector.ReadConfig ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ReadConfig ;
  end ;

  procedure TGIS_LayerSublayerVector.RereadConfig ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.RereadConfig ;
  end ;

  procedure TGIS_LayerSublayerVector.WriteConfig ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.WriteConfig ;
  end ;

  procedure TGIS_LayerSublayerVector.BindSqlQueryInternal(
    const _sqlquery    : TGIS_SqlQuery
  ) ;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      TGIS_LayerVector( ParentLayer ).BindSqlQueryInternal( _sqlquery ) ;
  end;

  function TGIS_LayerSublayerVector.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.DrawEx( _extent )
    else
      Result := False ;
  end ;

  procedure TGIS_LayerSublayerVector.FlashShape(
    const _uid     : TGIS_Uid ;
    const _times   : Integer ;
    const _delay   : Integer
  ) ;
  begin
    if assigned( ParentLayer ) and ( ParentLayer is TGIS_LayerVector ) then
      TGIS_LayerVector( ParentLayer ).FlashShape( _uid, _times, _delay ) ;
  end;

  procedure TGIS_LayerSublayerVector.SaveData ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SaveData ;
  end ;

  procedure TGIS_LayerSublayerVector.SaveAll ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SaveAll ;
  end ;

  function TGIS_LayerSublayerVector.MustSave : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.MustSave
    else
      Result := False ;
  end ;

  procedure TGIS_LayerSublayerVector.RecalcExtent ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.RecalcExtent ;
  end ;

  procedure TGIS_LayerSublayerVector.RecalcProjectedExtent ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.RecalcProjectedExtent ;
  end ;

  procedure TGIS_LayerSublayerVector.Lock ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Lock ;
  end ;

  procedure TGIS_LayerSublayerVector.Unlock ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Unlock ;
  end ;

  procedure TGIS_LayerSublayerVector.Project_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Project_Ref( _ptg ) ;
  end ;

  procedure TGIS_LayerSublayerVector.Unproject_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Unproject_Ref( _ptg ) ;
  end ;

  procedure TGIS_LayerSublayerVector.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Project3D_Ref( _ptg ) ;
  end ;

  procedure TGIS_LayerSublayerVector.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Unproject3D_Ref( _ptg ) ;
  end ;

  function TGIS_LayerSublayerVector.Project(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Project( _ptg )
    else
      Result := GisPoint( 0, 0 ) ;
  end ;

  function TGIS_LayerSublayerVector.Project3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Project3D( _ptg )
    else
      Result := GisPoint3D( 0, 0, 0, 0 ) ;
  end ;

  function TGIS_LayerSublayerVector.Unproject(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Unproject( _ptg )
    else
      Result := GisPoint( 0, 0 ) ;
  end ;

  function TGIS_LayerSublayerVector.Unproject3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Unproject3D( _ptg )
    else
      Result := GisPoint3D( 0, 0, 0, 0 ) ;
  end ;

  procedure TGIS_LayerSublayerVector.Alive ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Alive ;
  end ;

  procedure TGIS_LayerSublayerVector.Dormant ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Dormant ;
  end ;

  function TGIS_LayerSublayerVector.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.PreRecognize( _path, _new_path )
    else
      Result := False ;
  end ;

  procedure TGIS_LayerSublayerVector.SetCSByWKT(
    const _wkt : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SetCSByWKT( _wkt ) ;
  end ;

  procedure TGIS_LayerSublayerVector.SetCSByEPSG(
    const _epsg : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SetCSByEPSG( _epsg ) ;
  end ;

  procedure TGIS_LayerSublayerVector.SetCSByWKTFile(
    const _path : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SetCSByWKTFile( _path ) ;
  end ;

  constructor TGIS_LayerSublayerVector.Create ;
  begin
    inherited ;

    FUseParentParams := True ;
  end;

  procedure TGIS_LayerSublayerPixel.fset_Active(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      inherited fset_Active( _value )
    else
      inherited fset_Active( _value ) ;
  end;

  function TGIS_LayerSublayerPixel.fget_Path
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Path
    else
      Result := inherited fget_Path ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Path(
    const _value : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Path := _value
    else
      inherited fset_Path( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Stream
    : TStream ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Stream
    else
      Result := inherited fget_Stream ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Stream(
    const _value : TStream
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Stream := _value
    else
      inherited fset_Stream( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Caption
    : String ;
  begin
    // use local property only
    Result := inherited fget_Caption ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Caption(
    const _value : String
  ) ;
  begin
    // use local property only
    inherited fset_Caption( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Tag
    : Integer ;
  begin
    // use local property only
    Result := inherited fget_Tag ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Tag(
    const _value : Integer
  ) ;
  begin
    // use local property only
    inherited fset_Tag( _value ) ;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_LayerSublayerPixel.fget_TagPointer
      : Object ;
    begin
      // use local property only
      Result := inherited fget_TagPointer ;
    end ;

    procedure TGIS_LayerSublayerPixel.fset_TagPointer(
      const _value : Object
    ) ;
    begin
      // use local property only
      inherited fset_TagPointer( _value ) ;
    end ;
  {$ELSE}

    function TGIS_LayerSublayerPixel.fget_TagPointer
      : Pointer ;
    begin
      // use local property only
      Result := inherited fget_TagPointer ;
    end ;

    procedure TGIS_LayerSublayerPixel.fset_TagPointer(
      const _value : Pointer
    ) ;
    begin
      // use local property only
      inherited fset_TagPointer( _value ) ;
    end ;
  {$ENDIF}

  function TGIS_LayerSublayerPixel.fget_TagInternal
    : Integer ;
  begin
    // use local property only
    Result := inherited fget_TagInternal ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_TagInternal(
    const _value : Integer
  ) ;
  begin
    // use local property only
    inherited fset_TagInternal( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Name
    : String ;
  begin
    // use local property only
    Result := inherited fget_Name ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Name(
    const _value : String
  ) ;
  begin
    // use local property only
    inherited fset_Name( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Basemap
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Basemap
    else
      Result := inherited fget_Basemap ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Basemap(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Basemap := _value
    else
      inherited fset_Basemap( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_FileInfo
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.FileInfo
    else
      Result := inherited fget_FileInfo ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Comments
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Comments
    else
      Result := inherited fget_Comments ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Comments(
    const _value : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Comments := _value
    else
      inherited fset_Comments( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_IsLocked
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.IsLocked
    else
      Result := inherited fget_IsLocked ;
  end ;

  function TGIS_LayerSublayerPixel.fget_IsOpened
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.IsOpened
    else
      Result := inherited fget_IsOpened ;
  end ;

  function TGIS_LayerSublayerPixel.fget_DirectMode
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.DirectMode
    else
      Result := inherited fget_DirectMode ;
  end ;

  {$IFDEF CLR}
    function TGIS_LayerSublayerPixel.fget_Age
      : DateTime ;
    begin
      if assigned( ParentLayer ) then
        Result := ParentLayer.Age
      else
        Result := inherited fget_Age ;
    end ;
  {$ELSE}

    function TGIS_LayerSublayerPixel.fget_Age
      : TDateTime ;
    begin
      if assigned( ParentLayer ) then
        Result := ParentLayer.Age
      else
        Result := inherited fget_Age ;
    end ;
  {$ENDIF}

  function TGIS_LayerSublayerPixel.fget_SubType
    : TGIS_LayerSubTypeSet ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.SubType
    else
      Result := inherited fget_SubType ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Viewer
    : TGIS_ViewerRef ;
  begin
    // use local property only
    Result := inherited fget_Viewer ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Viewer(
    const _value : TGIS_ViewerRef
  ) ;
  begin
    // use local and parent property
    if assigned( ParentLayer ) then
      ParentLayer.Viewer := _value ;

    inherited fset_Viewer( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_ConfigFile
    : TGIS_ConfigAbstract ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ConfigFile
    else
      Result := inherited fget_ConfigFile ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Extent
    : TGIS_Extent ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Extent
    else
      Result := inherited fget_Extent ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Extent(
    const _value : TGIS_Extent
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Extent := GisMaxExtent( ParentLayer.Extent, _value )
    else
      inherited fset_Extent( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_ProjectedExtent
    : TGIS_Extent ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ProjectedExtent
    else
      Result := inherited fget_ProjectedExtent ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_ProjectedExtent(
    const _value : TGIS_Extent
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ProjectedExtent := GisMaxExtent( ParentLayer.ProjectedExtent, _value )
    else
      inherited fset_ProjectedExtent( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Active
    : Boolean ;
  begin
    // use local property only
    Result := inherited fget_Active ;
  end ;

  function TGIS_LayerSublayerPixel.fget_HideFromLegend
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.HideFromLegend
    else
      Result := inherited fget_HideFromLegend ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_HideFromLegend(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.HideFromLegend := _value
    else
      inherited fset_HideFromLegend( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Collapsed
    : Boolean ;
  begin
    // use local property only
    Result := inherited fget_Collapsed ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Collapsed(
    const _value : Boolean
  ) ;
  begin
    // use local property only
    inherited fset_Collapsed( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_ZOrder
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ZOrder
    else
      Result := inherited fget_ZOrder ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_ZOrder(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ZOrder := _value
    else
      inherited fset_ZOrder( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_ZOrderEx
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.ZOrderEx
    else
      Result := inherited fget_ZOrderEx ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_ZOrderEx(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ZOrderEx := _value
    else
      inherited fset_ZOrderEx( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_UseConfig
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.UseConfig
    else
      Result := inherited fget_UseConfig ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_UseConfig(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.UseConfig := _value
    else
      inherited fset_UseConfig( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_ConfigName
    : String ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Path
    else
      Result := inherited fget_Path ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_ConfigName(
    const _value : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ConfigName := _value
    else
      inherited fset_ConfigName( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_CachedPaint
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.CachedPaint
    else
      Result := inherited fget_CachedPaint ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_CachedPaint(
    const _value : Boolean
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.CachedPaint := _value
    else
      inherited fset_CachedPaint( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Transparency
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Transparency
    else
      Result := inherited fget_Transparency ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Transparency(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Transparency := _value
    else
      inherited fset_Transparency( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Addition
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Addition
    else
      Result := inherited fget_Addition ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Addition(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Addition := _value
    else
      inherited fset_Addition( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_DormantMode
    : TGIS_LayerDormantMode ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.DormantMode
    else
      Result := inherited fget_DormantMode ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_DormantMode(
    const _value : TGIS_LayerDormantMode
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.DormantMode := _value
    else
      inherited fset_DormantMode( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_CodePage
    : Integer ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.CodePage
    else
      Result := inherited fget_CodePage ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_CodePage(
    const _value : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.CodePage := _value
    else
      inherited fset_CodePage( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_SubLayers
    : TGIS_LayerAbstractList ;
  begin
    // use local property only
    Result := inherited fget_SubLayers ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_SubLayers(
    const _value : TGIS_LayerAbstractList
  ) ;
  begin
    // use local property only
    inherited fset_SubLayers( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_ParentLayer
    : TGIS_Layer ;
  begin
    // use local property only
    Result := inherited fget_ParentLayer ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_ParentLayer(
    const _value : TGIS_Layer
  ) ;
  begin
    // use local property only
    inherited fset_ParentLayer( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_CS
    : TGIS_CSCoordinateSystem ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.CS
    else
      Result := inherited fget_CS ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.CS:= _value
    else
      inherited fset_CS( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_MultiUserMode
    : TGIS_MultiUser ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.MultiUserMode
    else
      Result := inherited fget_MultiUserMode ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_MultiUserMode(
    const _value : TGIS_MultiUser
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.MultiUserMode := _value
    else
      inherited fset_MultiUserMode( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_IsReadOnly
    : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.IsReadOnly
    else
      Result := inherited fget_IsReadOnly ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Params
    : TGIS_ParamsSection ;
  begin
    if assigned( ParentLayer ) and FUseParentParams then
      Result := ParentLayer.Params
    else
      Result := inherited fget_Params ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Params(
    const _value : TGIS_ParamsSection
  ) ;
  begin
    if assigned( ParentLayer ) and FUseParentParams then
      ParentLayer.Params := _value
    else
      inherited fset_Params( _value ) ;
  end ;

  function TGIS_LayerSublayerPixel.fget_ParamsList
    : TGIS_ParamsList ;
  begin
    if assigned( ParentLayer ) and FUseParentParams then
      Result := ParentLayer.ParamsList
    else
      Result := inherited fget_ParamsList ;
  end ;

  function TGIS_LayerSublayerPixel.fget_Transform
    : TGIS_Transform ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Transform
    else
      Result := inherited fget_Transform ;
  end ;

  procedure TGIS_LayerSublayerPixel.fset_Transform(
    const _value : TGIS_Transform
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Transform := _value
    else
      inherited fset_Transform( _value ) ;
  end ;

  procedure TGIS_LayerSublayerPixel.Open ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Open ;
  end ;

  procedure TGIS_LayerSublayerPixel.Move(
    const _step : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Move( _step ) ;
  end ;

  procedure TGIS_LayerSublayerPixel.MoveEx(
    const _step : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.MoveEx( _step ) ;
  end ;

  procedure TGIS_LayerSublayerPixel.ReadConfig ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.ReadConfig ;
  end ;

  procedure TGIS_LayerSublayerPixel.RereadConfig ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.RereadConfig ;
  end ;

  procedure TGIS_LayerSublayerPixel.WriteConfig ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.WriteConfig ;
  end ;

  function TGIS_LayerSublayerPixel.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.DrawEx( _extent )
    else
      Result := False ;
  end ;

  procedure TGIS_LayerSublayerPixel.SaveData ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SaveData ;
  end ;

  procedure TGIS_LayerSublayerPixel.SaveAll ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SaveAll ;
  end ;

  function TGIS_LayerSublayerPixel.MustSave : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.MustSave
    else
      Result := False ;
  end ;

  procedure TGIS_LayerSublayerPixel.RecalcExtent ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.RecalcExtent ;
  end ;

  procedure TGIS_LayerSublayerPixel.RecalcProjectedExtent ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.RecalcProjectedExtent ;
  end ;

  procedure TGIS_LayerSublayerPixel.Lock ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Lock ;
  end ;

  procedure TGIS_LayerSublayerPixel.Unlock ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Unlock ;
  end ;

  procedure TGIS_LayerSublayerPixel.Project_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Project_Ref( _ptg ) ;
  end ;

  procedure TGIS_LayerSublayerPixel.Unproject_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Unproject_Ref( _ptg ) ;
  end ;

  procedure TGIS_LayerSublayerPixel.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Project3D_Ref( _ptg ) ;
  end ;

  procedure TGIS_LayerSublayerPixel.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Unproject3D_Ref( _ptg ) ;
  end ;

  function TGIS_LayerSublayerPixel.Project(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Project( _ptg )
    else
      Result := GisPoint( 0, 0 ) ;
  end ;

  function TGIS_LayerSublayerPixel.Project3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Project3D( _ptg )
    else
      Result := GisPoint3D( 0, 0, 0, 0 ) ;
  end ;

  function TGIS_LayerSublayerPixel.Unproject(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Unproject( _ptg )
    else
      Result := GisPoint( 0, 0 ) ;
  end ;

  function TGIS_LayerSublayerPixel.Unproject3D(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.Unproject3D( _ptg )
    else
      Result := GisPoint3D( 0, 0, 0, 0 ) ;
  end ;

  procedure TGIS_LayerSublayerPixel.Alive ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Alive ;
  end ;

  procedure TGIS_LayerSublayerPixel.Dormant ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.Dormant ;
  end ;

  function TGIS_LayerSublayerPixel.GetBitmap(
    const _extent   : TGIS_Extent ;
    const _bitmap   : TGIS_Pixels ;
    const _width    : Integer ;
    const _height    : Integer
  ) : Boolean;
  begin
    if assigned( ParentLayer ) then
      Result := TGIS_LayerPixel(ParentLayer).GetBitmap( _extent, _bitmap, _width, _height )
    else
      Result := False ;
  end;

  function TGIS_LayerSublayerPixel.PreRecognize(
    const _path     : String ;
    var   _new_path : String
  ) : Boolean ;
  begin
    if assigned( ParentLayer ) then
      Result := ParentLayer.PreRecognize( _path, _new_path )
    else
      Result := False ;
  end ;

  procedure TGIS_LayerSublayerPixel.SetCSByWKT(
    const _wkt : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SetCSByWKT( _wkt ) ;
  end ;

  procedure TGIS_LayerSublayerPixel.SetCSByEPSG(
    const _epsg : Integer
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SetCSByEPSG( _epsg ) ;
  end ;

  procedure TGIS_LayerSublayerPixel.SetCSByWKTFile(
    const _path : String
  ) ;
  begin
    if assigned( ParentLayer ) then
      ParentLayer.SetCSByWKTFile( _path ) ;
  end ;

  constructor TGIS_LayerSublayerPixel.Create ;
  begin
    inherited ;

    FUseParentParams := True ;
  end;

{==================================== END =====================================}
end.

