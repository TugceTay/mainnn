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
  Encapsulation of a TAB file access.

  This unit was partially based on MITAB:

  - Copyright (c) 1998-2000, Daniel Morissette
  - Copyright (c) 1998-2000, Frank Warmerdam
  - Copyright (c) 1999,2000, Stephane Villeneuve

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  Sublicensing of this unit is a subject of TatukGIS Developer
  Kernel License
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerTAB ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerTAB"'}
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
    System.Classes,
    System.SysUtils,
    System.Variants,
    Lider.CG.GIS.GeoTypes,
    {$IFNDEF GIS_NOADO}
      Lider.CG.GIS.GeoDbAdo,
    {$ENDIF}
    Lider.CG.GIS.GeoFileDBF,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoFileTAB,
    Lider.CG.GIS.GeoSymbol ;
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

  {#gendoc:hide}
  // Initialization section handler
  GisLayerTAB = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {$IFDEF OXYGENE}
    T_cursorTab nested in TGIS_LayerTAB = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public
      /// <summary>
      ///   Is cursor in use.
      /// </summary>
      curInUse : Boolean ;

      /// <summary>
      ///   Current shape. Layer access is based on record-by-record access.
      /// </summary>
      currShape : TGIS_Shape ;

      /// <summary>
      ///   Pre-allocated shape. Recreating the shape on each MoveNext call is
      //    much faster than full Create constructor
      /// </summary>
      currPoint : TGIS_ShapePoint ;

      /// <summary>
      ///   Pre-allocated shape. Recreating it on each MoveNext call is much
      ///   faster than full Create constructor
      /// </summary>
      currArc : TGIS_ShapeArc ;

      /// <summary>
      ///   Pre-allocated shape. Recreating it on each MoveNext call is much
      ///   faster than full Create constructor
      /// </summary>
      currPolygon : TGIS_ShapePolygon ;

      /// <summary>
      ///   Pr-eallocated shape. Recreating it on each MoveNext call is much
      ///   faster than full Create constructor
      /// </summary>
      currMultipoint : TGIS_ShapeMultiPoint  ;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   Pre-allocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currRecord  : TGIS_RecordTAB_MAP ;

      {$ELSE}
        /// <summary>
        ///   Pre-allocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor
        /// </summary>
        currRecord  : PGIS_RecordTAB_MAP ;

      {$ENDIF}
      /// <summary>
      ///   Last UID in a shape file. If not set then -1.
      /// </summary>
      lastTraversedUid   : TGIS_Uid ;

      /// <summary>
      ///   Is first record?
      /// </summary>
      isFirst         : Boolean ;

      {$IFNDEF GIS_NOADO}
        /// <summary>
        ///   Data file encapsulation.
        /// </summary>
        dataDB : TGIS_DbAdo ;
      {$ENDIF}
    end ;

    T_arVeritces nested in TGIS_LayerTAB = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public
        Num    : Integer ;
        Offset : Integer ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Layer that can read TAB file.
  /// </summary>
  TGIS_LayerTAB = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private

        /// <summary>
        ///   Shape file encapsulation.
        /// </summary>
        shapeFile : TGIS_FileTAB_MAP ;

        /// <summary>
        ///   Index file encapsulation.
        /// </summary>
        indexFile : TGIS_FileTAB_ID ;

        /// <summary>
        ///   Data file encapsulation.
        /// </summary>
        dataFile  : TGIS_FileDBF ;

        /// <summary>
        ///   Last UID in a shape file. If not set then -1.
        /// </summary>
        lastUid   : TGIS_Uid ;
        sqlParams : TStrings ;
    private
      {$IFDEF OXYGENE}
        cursorTab : array of T_cursorTab ;
      {$ELSE}
        cursorTab : array of record
          /// <summary>
          ///    Is cursor in use.
          /// </summary>
          curInUse : Boolean ;

          /// <summary>
          ///    Current shape. Layer access is based on record-by-record access.
          /// </summary>
          currShape : TGIS_Shape ;

          /// <summary>
          ///   Pre-allocated shape. Recreating the shape on each MoveNext call is
          ///   much faster than full Create constructor
          /// </summary>
          currPoint : TGIS_ShapePoint ;

          /// <summary>
          ///   Pre-allocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor
          /// </summary>
          currArc : TGIS_ShapeArc ;

          /// <summary>
          ///   Pre-allocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor
          /// </summary>
          currPolygon : TGIS_ShapePolygon ;

          /// <summary>
          ///   Pre-allocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor
          /// </summary>
          currMultipoint : TGIS_ShapeMultiPoint  ;

          /// <summary>
          ///   Pre-allocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor
          /// </summary>
          currRecord  : PGIS_RecordTAB_MAP ;

          /// <summary>
          ///   Last UID in a shape file. If not set then -1.
          /// </summary>
          lastTraversedUid   : TGIS_Uid ;

          /// <summary>
          ///   Is first record?
          /// </summary>
          isFirst         : Boolean ;

          {$IFNDEF GIS_NOADO}
          /// <summary>
          ///   Data file encapsulation.
          /// </summary>
          dataDB    : TGIS_DbAdo ;
          {$ENDIF}
        end ;
      {$ENDIF}
      {$IFDEF OXYGENE}

        /// <summary>
        ///   Array of verices offsets.
        /// </summary>
        arVeritces : array of T_arVeritces ;
      {$ELSE}
          /// <summary>
          ///   Array of verices offsets.
          /// </summary>
          arVeritces : array of record
            Num    : Integer ;
            Offset : Integer ;
          end ;
      {$ENDIF}

        tableName : String ;

        /// <summary>
        ///   Last used symbology.
        /// </summary>
        lastFontIndex : Integer ;
        lastSymbolNo  : Integer ;
        lastSymbol    : TGIS_SymbolAbstract ;
        isLabelSet    : Boolean ;
    private // property access functions

      function fget_PathTAB   : String ; virtual;
      function fget_PathMAP   : String ; virtual;
      function fget_PathDAT   : String ; virtual;
      function fget_PathDBF   : String ; virtual;
      function fget_PathID    : String ; virtual;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // property access functions

      function fget_PathRTree : String ; override;

    private // private methods

      /// <summary>
      ///   Read a shape from the shape file and recreate it.
      /// </summary>
      procedure readShape             ( const _cursor      : Integer
                                      ) ;

      /// <summary>
      ///   Set shape colors.
      /// </summary>
      /// <param name="_shapeType">
      ///   shape type
      /// </param>
      /// <param name="_brushIndex">
      ///   brush index
      /// </param>
      /// <param name="_penIndex">
      ///   pen index
      /// </param>
      /// <param name="_symbolIndex">
      ///   symbol index
      /// </param>
      procedure setShapeParams        ( const _shapeType   : TGIS_ShapeType    ;
                                        const _brushIndex  : Integer           ;
                                        const _penIndex    : Integer           ;
                                        const _symbolIndex : Integer           ;
                                        const _fontIndex   : Integer           ;
                                        const _useCustom   : Boolean           ;
                                        const _customSymbol: TGIS_CustomSymbolDef ;
                                        const _cursor      : Integer
                                      );
      {$IFNDEF GIS_NOADO}
        /// <summary>
        ///   Read field structure form database.
        /// </summary>
        procedure ReadDBFieldsStructure
                                      ( const _cursor      : Integer
                                      ) ;
      {$ENDIF}

      /// <summary>
      ///   Add new field.
      /// </summary>
      /// <param name="_name">
      ///   name of field to be add
      /// </param>
      /// <param name="_type">
      ///   type of field
      /// </param>
      /// <param name="_width">
      ///   width of field
      /// </param>
      /// <param name="_decimal">
      ///   decimal places
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDEXIST
      /// </exception>
      /// <remarks>
      ///   <note type="note">
      ///    This method is used only for internal use on layer reading.
      ///    </note>
      /// </remarks>
      procedure macroAddField         ( const _uidname     : String            ;
                                        const _name        : String            ;
                                        const _type        : TGIS_FieldType    ;
                                        const _width       : Integer           ;
                                        const _decimal     : Integer
                                      ) ; overload;

      /// <summary>
      ///   Add new field.
      /// </summary>
      /// <param name="_name">
      ///   name of field to be add
      /// </param>
      /// <param name="_type">
      ///   type of field
      /// </param>
      /// <param name="_width">
      ///   width of field
      /// </param>
      /// <param name="_decimal">
      ///   decimal places
      /// </param>
      /// <param name="_saved">
      ///   True, if field exist in an original file
      /// </param>
      /// <param name="_binary">
      ///   binary width (used only for TAB/DAT files)
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDEXIST
      /// </exception>
      /// <remarks>
      ///   <note type="note">
      ///    This method is used only for internal use on layer reading.
      ///    </note>
      /// </remarks>
      procedure macroAddField         ( const _uidname     : String            ;
                                        const _name        : String            ;
                                        const _type        : TGIS_FieldType    ;
                                        const _width       : Integer           ;
                                        const _decimal     : Integer           ;
                                        const _saved       : Boolean           ;
                                        const _binary      : Integer
                                      ) ; overload;
      {$IFNDEF GIS_NOADO}
        /// <summary>
        ///   Macro for fetching proper record form the database.
        /// </summary>
        /// <param name="_uid">
        ///   uid of the shape for which corresponding record should be fetched
        /// </param>
        function  macroFetchRecord    ( const _uid         : TGIS_Uid           ;
                                        const _cursor      : Integer
                                      ) : Boolean ;
      {$ENDIF}

       /// <summary>
       ///   Parse projection info.
       /// </summary>
       procedure doCoordSys ;
      {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         function  getFieldInternal   ( const _uid         : TGIS_Uid             ;
                                        const _name        : String            ;
                                        const _cursor      : Integer
                                      ) : Variant ; override;

         /// <inheritdoc/>
         function  getBindedFieldInternal(
                                        const _shape       : TObject           ;
                                        const _field       : Integer           ;
                                        const _cursor      : Integer
                                      ) : Variant ; override;

         /// <inheritdoc/>
         procedure setUp              ; override;
      // cursor access function(s)

         /// <inheritdoc/>
         function  cursorOpen         :  Integer ; override;

         /// <inheritdoc/>
         procedure cursorClose        ( const _cursor      : Integer
                                      ) ; override;

         /// <inheritdoc/>
         procedure cursorFirst        ( const _cursor      : Integer           ;
                                        const _viewerCS    : Boolean           ;
                                        const _extent      : TGIS_Extent       ;
                                        const _query       : String            ;
                                        const _shape       : TGIS_Shape        ;
                                        const _de9im       : String            ;
                                        const _skipDeleted : Boolean
                                      ) ; override;

         /// <inheritdoc/>
         procedure cursorNext         ( const _cursor      : Integer
                                      ) ; override;

         /// <inheritdoc/>
         function  cursorEof          ( const _cursor      : Integer
                                      ) : Boolean ; override;

         /// <inheritdoc/>
         function  cursorShape        ( const _cursor      : Integer
                                      ) : TGIS_Shape ; override;
    protected
      // destructor

         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create  ; override;
      // shape access function(s)

         /// <inheritdoc/>
         function  GetShape           ( const _uid         : TGIS_Uid           ;
                                        const _cursor      : Integer
                                      ) : TGIS_Shape ; override;

         /// <inheritdoc/>
         function  GetLastUid         : TGIS_Uid ; override;

         /// <inheritdoc/>
         function  GetNewUid          : TGIS_Uid ; override;

         /// <inheritdoc/>
         procedure SaveData           ; override;

         /// <inheritdoc/>
         function  PreRecognize       ( const _path        : String            ;
                                        var   _new_path    : String
                                      ) : Boolean ; override;

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
         procedure Build              ( const _path        : String            ;
                                        const _extent      : TGIS_Extent       ;
                                        const _type        : TGIS_ShapeType    ;
                                        const _dim         : TGIS_DimensionType
                                      ) ; override;

         /// <summary>
         ///   Close internal files.
         /// </summary>
         procedure CloseFiles         ;
    public

        /// <summary>
        ///   Path to a TAB file.
        /// </summary>
        property PathTAB : String read fget_PathTAB ;
        /// <summary>
        ///   Path to a DAT file.
        /// </summary>
        property PathDAT : String read fget_PathDAT ;
        /// <summary>
        ///   Path to a DBF file.
        /// </summary>
        property PathDBF : String read fget_PathDBF ;
        /// <summary>
        ///   Path to a MAP file.
        /// </summary>
        property PathMAP : String read fget_PathMAP ;
        /// <summary>
        ///   Path to a ID file.
        /// </summary>
        property PathID  : String read fget_PathID  ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoCsMapinfo,
    Lider.CG.GIS.GeoCsSystems ;
{$ENDIF}

const
  // features
     GIS_TAB_NONE                   = $00 ;
     GIS_TAB_SYMBOL_C               = $01 ;
     GIS_TAB_SYMBOL                 = $02 ;
     GIS_TAB_LINE_C                 = $04 ;
     GIS_TAB_LINE                   = $05 ;
     GIS_TAB_PLINE_C                = $07 ;
     GIS_TAB_PLINE                  = $08 ;
     GIS_TAB_ARC_C                  = $0a ;
     GIS_TAB_ARC                    = $0b ;
     GIS_TAB_REGION_C               = $0d ;
     GIS_TAB_REGION                 = $0e ;
     GIS_TAB_TEXT_C                 = $10 ;
     GIS_TAB_TEXT                   = $11 ;
     GIS_TAB_RECT_C                 = $13 ;
     GIS_TAB_RECT                   = $14 ;
     GIS_TAB_ROUNDRECT_C            = $16 ;
     GIS_TAB_ROUNDRECT              = $17 ;
     GIS_TAB_ELLIPSE_C              = $19 ;
     GIS_TAB_ELLIPSE                = $1a ;
     GIS_TAB_MULTIPLINE_C           = $25 ;
     GIS_TAB_MULTIPLINE             = $26 ;
     GIS_TAB_FONTSYMBOL_C           = $28 ;
     GIS_TAB_FONTSYMBOL             = $29 ;
     GIS_TAB_CUSTOMSYMBOL_C         = $2b ;
     GIS_TAB_CUSTOMSYMBOL           = $2c ;
     GIS_TAB_V450_REGION_C          = $2e ;
     GIS_TAB_V450_REGION            = $2f ;
     GIS_TAB_V450_MULTIPLINE_C      = $31 ;
     GIS_TAB_V450_MULTIPLINE        = $32 ;
     GIS_TAB_MULTIPOINT_C           = $34 ;
     GIS_TAB_MULTIPOINT             = $35 ;
     GIS_TAB_COLLECTION_C           = $37 ;
     GIS_TAB_COLLECTION             = $38 ;
     GIS_TAB_V800_REGION_C          = $3d ;
     GIS_TAB_V800_REGION            = $3e ;
     GIS_TAB_V800_MULTIPLINE_C      = $40 ;
     GIS_TAB_V800_MULTIPLINE        = $41 ;
     GIS_TAB_V800_MULTIPOINT_C      = $43 ;
     GIS_TAB_V800_MULTIPOINT        = $44 ;
     GIS_TAB_V800_COLLECTION_C      = $46 ;
     GIS_TAB_V800_COLLECTION        = $47 ;

  // Built-in fields
     GIS_TAB_FLD_LABEL          = 'TAB_LABEL'  ;
     GIS_TAB_FLD_LINE           = 'TAB_LINE'   ;
     GIS_TAB_FLD_SYMBOL         = 'TAB_SYMBOL' ;

     GIS_TAB_FLD_DATABASE_ID    = 'MAPINFO_ID' ;

     TAB_METADATA_OPEN_SEAMLESS = 'TGIS_LayerTAB.OpenSeamless' ;

//=============================================================================
// TGIS_LayerTAB
//=============================================================================

  constructor TGIS_LayerTAB.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable
                            ] ;
    shapeFile      := nil ;
    dataFile       := nil ;
    sqlParams := TStringList.Create ;
  end ;

    procedure TGIS_LayerTAB.doDestroy ;
    begin
      FreeObject( shapeFile ) ;
      FreeObject( indexFile ) ;
      FreeObject( dataFile  ) ;
      FreeObject( sqlParams ) ;

      inherited ;
    end ;

  function TGIS_LayerTAB.fget_PathTAB : String ;
  begin
    Result := Path;
  end ;

  function TGIS_LayerTAB.fget_PathDAT : String ;
  begin
    Result := GetPathNoExt(Path) + '.dat'
  end ;

  function TGIS_LayerTAB.fget_PathDBF : String ;
  begin
    Result := GetPathNoExt(Path) + '.dbf'
  end ;

  function TGIS_LayerTAB.fget_PathMAP : String ;
  begin
    Result := GetPathNoExt(Path) + '.map'
  end ;

  function TGIS_LayerTAB.fget_PathID : String ;
  begin
    Result := GetPathNoExt(Path) + '.id'
  end ;

  function TGIS_LayerTAB.fget_PathRTree : String ;
  begin
    Result := ExpandForcedRTreePath(
                GetPathNoExt( PathTAB ) + GIS_RTREE_EXT
              ) ;
  end ;

  procedure TGIS_LayerTAB.setShapeParams(
    const _shapeType   : TGIS_ShapeType    ;
    const _brushIndex  : Integer           ;
    const _penIndex    : Integer           ;
    const _symbolIndex : Integer           ;
    const _fontIndex   : Integer           ;
    const _useCustom   : Boolean           ;
    const _customSymbol: TGIS_CustomSymbolDef ;
    const _cursor      : Integer
   );

    function _getWidth( const _def : TGIS_TABPenDef ) : Integer ;
    begin
      if _def.nPointWidth > 0 then
         Result := _def.nPointWidth
      else
         Result := - Integer( _def.nPixelWidth ) ;
    end ;

    function _getSize( const _psize : Integer ) : Integer ;
    begin
      if _psize > 0 then
         Result := TruncS( Max( 25, _psize * 1440 / 72 ) )
      else
         Result := - Integer( _psize ) ;
    end ;

    function _getStyle( const _def : TGIS_TABPenDef ) : TGIS_PenStyle ;
    begin
      case _def.nLinePattern of
        1                   : Result := TGIS_PenStyle.Clear ;
        2                   : Result := TGIS_PenStyle.Solid ;
        3,4,10              : Result := TGIS_PenStyle.Dot ;
        5,6,7,8,9,11,12,13  : Result := TGIS_PenStyle.Dash ;
        14,15,16,17,23,25   : Result := TGIS_PenStyle.DashDot ;
        18,19,20,21,22,24   : Result := TGIS_PenStyle.DashDotDot
      else                    Result := TGIS_PenStyle.Solid
      end ;
    end;

  begin
    case _shapeType of
      TGIS_ShapeType.Polygon :
        begin
          if ( not IgnoreShapeParams ) and
             ( _brushIndex > 0 )       and
             ( length( shapeFile.TabBrushDef ) > 0 )
          then begin
            with cursorTab[ _cursor ].currShape.Params.Area do begin
              Color := shapeFile.TabBrushDef[ _brushIndex-1 ].rgbFGColor ;
              case shapeFile.TabBrushDef[ _brushIndex-1 ].nFillPattern of
                1                   : Pattern := TGIS_BrushStyle.Clear       ;
                2                   : Pattern := TGIS_BrushStyle.Solid       ;
                3,16,19,20,22,23    : Pattern := TGIS_BrushStyle.Horizontal  ;
                4,21,24,25,26,27,28 : Pattern := TGIS_BrushStyle.Vertical    ;
                5,29,30,31          : Pattern := TGIS_BrushStyle.BDiagonal   ;
                6,32,33,34,35       : Pattern := TGIS_BrushStyle.FDiagonal   ;
                7,36,37,38,39,40    : Pattern := TGIS_BrushStyle.Cross       ;
                8,41,42,43,44       : Pattern := TGIS_BrushStyle.DiagCross   ;
              end ;
              if _penIndex > 0 then begin
                OutlineColor := shapeFile.TabPenDef[ _penIndex-1 ].rgbColor ;
                OutlineWidth := _getWidth( shapeFile.TabPenDef[ _penIndex-1 ] ) ;
                OutlineStyle := _getStyle( shapeFile.TabPenDef[ _penIndex-1 ] ) ;
              end
              else begin
                OutlineColor := TGIS_Color.Black;
                OutlineWidth := 1 ;
              end ;
              if (cursorTab[ _cursor ].currShape.Params.Area.Pattern = TGIS_BrushStyle.Clear) and
                 (cursorTab[ _cursor ].currShape.Params.Area.OutlineStyle = TGIS_PenStyle.Clear ) then
                OutlineStyle := TGIS_PenStyle.Solid
            end ;
          end
          else if ( not IgnoreShapeParams ) then begin
            with cursorTab[ _cursor ].currShape.Params.Area do begin
              Pattern := TGIS_BrushStyle.Clear  ;
              if ( _penIndex > 0 ) and ( length( shapeFile.TabPenDef ) > 0 )
              then begin
                OutlineColor := shapeFile.TabPenDef[ _penIndex-1 ].rgbColor ;
                OutlineWidth := _getWidth( shapeFile.TabPenDef[ _penIndex-1 ] ) ;
              end
              else begin
                OutlineColor := TGIS_Color.Black ;
                OutlineWidth := 1 ;
              end ;
            end ;
          end ;
        end ;
      TGIS_ShapeType.Arc :
        begin
          if ( not IgnoreShapeParams ) and
             (_penIndex > 0)           and
             ( length( shapeFile.TabPenDef ) > 0 )
          then begin
            if _penIndex < length( shapeFile.TabPenDef ) then
              with cursorTab[ _cursor ].currShape.Params.Line do begin
                Color :=  shapeFile.TabPenDef[ _penIndex-1 ].rgbColor ;
                Width := _getWidth( shapeFile.TabPenDef[ _penIndex-1 ] ) ;

                if shapeFile.TabPenDef[ _penIndex-1 ].nLinePattern = 193 then
                begin
                  Width        := 30 ;
                  OutlineWidth := 1  ;
                end
                else
                  Style := _getStyle( shapeFile.TabPenDef[ _penIndex-1 ] ) ;

                if ( cursorTab[ _cursor ].currShape.Params.Line.Style = TGIS_PenStyle.Clear ) then
                  cursorTab[ _cursor ].currShape.Params.Line.Style := TGIS_PenStyle.Solid ;

                cursorTab[ _cursor ].currShape.SetField(
                  GIS_TAB_FLD_LINE,
                  shapeFile.TabPenDef[ _penIndex-1 ].nLinePattern
                );
              end ;
          end ;
        end ;
      TGIS_ShapeType.Point,
      TGIS_ShapeType.MultiPoint :
        begin
          if ( not IgnoreShapeParams ) then begin
            with cursorTab[ _cursor ].currShape.Params.Marker do begin
              if (_penIndex > 0 ) and ( length( shapeFile.TabPenDef ) > 0 ) then
                Color := shapeFile.TabPenDef[ _penIndex-1 ].rgbColor
              else
                if ( _symbolIndex >=1) and
                   ( _symbolIndex<=length( shapeFile.TabSymbolDef ) ) and
                   not _useCustom then begin

                  Color := shapeFile.TabSymbolDef[ _symbolIndex-1 ].rgbColor ;
                  cursorTab[ _cursor ].currShape.SetField(
                    GIS_TAB_FLD_SYMBOL,
                    shapeFile.TabSymbolDef[ _symbolIndex-1 ].nSymbolNo
                  );
                  Size := _getSize( shapeFile.TabSymbolDef[ _symbolIndex-1 ].nPointSize ) ;
                  if ( lastFontIndex <> _fontIndex ) and
                     ( lastSymbolNo <> _customSymbol.symbolNo ) then begin
                    Symbol := SymbolList.Prepare(
                                Format( '%s:%d',
                                        ['MapInfo Symbols',
                                         Integer(shapeFile.TabSymbolDef[ _symbolIndex-1 ].nSymbolNo+1)]
                                       )
                               ) ;
                    lastFontIndex := _fontIndex ;
                    lastSymbolNo  := _customSymbol.symbolNo ;
                    lastSymbol    := Symbol ;
                  end
                  else
                    Symbol := lastSymbol ;
              end
              else if _useCustom then begin
                  Color        := _customSymbol.rgbColor ;
                  OutlineColor := _customSymbol.rgbColor2 ;
                  cursorTab[ _cursor ].currShape.SetField(
                    GIS_TAB_FLD_SYMBOL,
                    _customSymbol.symbolNo
                  );
                  Size := _getSize( _customSymbol.pointSize ) ;
                  SymbolRotate := -DegToRad( _customSymbol.rotation/10.0 ) ;
                  if ( _fontIndex > 0 )  and ( _fontIndex<=length( shapeFile.TabFontDef ) ) then begin
                    if ( lastFontIndex <> _fontIndex ) and
                       ( lastSymbolNo <> _customSymbol.symbolNo ) then begin

                      Symbol := SymbolList.Prepare(
                                  Format( '%s:%d',
                                          [shapeFile.TabFontDef[_fontIndex-1].szFontName,
                                           Integer(_customSymbol.symbolNo)]
                                         )
                                 ) ;
                      lastFontIndex := _fontIndex ;
                      lastSymbolNo  := _customSymbol.symbolNo ;
                      lastSymbol    := Symbol ;
                    end
                    else
                      Symbol := lastSymbol ;
                  end ;
              end
              else
                Size := 25;
            end ;
          end ;
        end ;
      else
        begin
          assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
        end ;
    end ;

  end ;

  {$IFNDEF GIS_NOADO}
    procedure TGIS_LayerTAB.ReadDBFieldsStructure( const _cursor : Integer );
    begin
      cursorTab[ _cursor ].dataDB.sqlQueryOpen(
        Format( 'SELECT * FROM %s WHERE %s=0',
                [ tableName, GIS_TAB_FLD_DATABASE_ID ] ),
        _cursor
      ) ;
      {$IFNDEF OXYGENE}
        cursorTab[ _cursor ].dataDB.sqlQueryStructure(
          tableName,GIS_TAB_FLD_DATABASE_ID,  macroAddField
        ) ;
      {$ELSE}
        cursorTab[ _cursor ].dataDB.sqlQueryStructure(
          tableName,GIS_TAB_FLD_DATABASE_ID, @macroAddField
        ) ;
      {$ENDIF}
      cursorTab[ _cursor ].dataDB.sqlQueryClose( _cursor ) ;
    end ;
  {$ENDIF}

  procedure TGIS_LayerTAB.macroAddField( const _uidname  : String          ;
                                         const _name     : String          ;
                                         const _type     : TGIS_FieldType  ;
                                         const _width    : Integer         ;
                                         const _decimal  : Integer
                                       ) ;
  begin
    macroAddField( _uidname, _name, _type, _width, _decimal, True, 0 ) ;
  end ;

  procedure TGIS_LayerTAB.macroAddField( const _uidname  : String          ;
                                         const _name     : String          ;
                                         const _type     : TGIS_FieldType  ;
                                         const _width    : Integer         ;
                                         const _decimal  : Integer         ;
                                         const _saved    : Boolean         ;
                                         const _binary   : Integer
                                       ) ;
  begin
    AddFieldInternal( _name, _type, _width, _decimal, _saved, _binary );

    if CompareText( _name, _uidname ) = 0 then begin
      if Fields.Count > 0 then begin
        with FieldInfo( Fields.Count -1 ) do begin
          FileFormat  := True ;
          {$IFDEF OXYGENE}
            &ReadOnly := True ;
          {$ELSE}
            ReadOnly  := True ;
          {$ENDIF}
          Hidden      := True ;
        end ;
      end ;
    end ;
  end ;

  {$IFNDEF GIS_NOADO}
    function TGIS_LayerTAB.macroFetchRecord( const _uid     : TGIS_Uid ;
                                             const _cursor  : Integer
                                           ) : Boolean ;
    var
      fetch : Boolean ;  // is fetch necessary?
    begin
      fetch := True ;

      if not cursorTab[ _cursor ].dataDB.sqlQueryEof(_cursor) then
        if VarToInt32( cursorTab[ _cursor ].dataDB.sqlQueryGetField(
                         GIS_TAB_FLD_DATABASE_ID, _cursor
                       )
           ) = _uid
        then
          fetch := False ;

      if fetch then begin
        cursorTab[ _cursor ].dataDB.sqlQueryClose(_cursor) ;
        cursorTab[ _cursor ].dataDB.sqlQueryOpen(
          Format( 'SELECT * FROM %s WHERE %s=%d',
                  [ tableName, GIS_TAB_FLD_DATABASE_ID, _uid ]
                ),
          _cursor
        ) ;
      end ;

      Result := fetch ;
    end ;
  {$ENDIF}

  procedure TGIS_LayerTAB.readShape( const _cursor : Integer ) ;
  var
    i,j,k        : Integer     ;
    compr        : Boolean     ;
    num_points   : Cardinal    ;
    num_sections : Cardinal    ;
    num_vertices : Cardinal    ;
    coord_ptr    : Cardinal    ;
    coord_size   : Cardinal    ;
    ptr_tmp      : Cardinal    ;
    size         : Cardinal    ;
    num_tpoints  : Cardinal    ;
    stmp         : String      ;
    penIndex     : Byte        ;
    brushIndex   : Byte        ;
    symbolIndex  : Byte        ;
    fontIndex    : Byte        ;
    r, g, b      : Byte        ;
    align        : Word        ;
    style        : Word        ;
    theight      : Cardinal    ;
    rot          : Double      ;
    fcolor       : TGIS_Color  ;
    bcolor       : TGIS_Color  ;
    scolor       : TGIS_Color  ;
    startAngle   : Integer     ;
    endAngle     : Integer     ;
    dStartAngle  : Double      ;
    dEndAngle    : Double      ;
    ext          : TGIS_Extent ;
    ax, ay       : Double      ;
    sw, an       : Double      ;
    sa, ca       : Double      ;
    ctr          : TGIS_Point  ;
    customSymbol : TGIS_CustomSymbolDef {$IFDEF GIS_NORECORDS} := new TGIS_CustomSymbolDef {$ENDIF} ;
    useCustom    : Boolean ;
    rec_type     : Integer ;
    rec_number   : Integer ;
    all_points   : array of TGIS_Point ;

      function getPosition( const _idx : Integer ) : TGIS_LabelPosition ;
      begin
        if ( _idx and $200 ) <> 0 then
          Result := TGIS_LabelPosition.MiddleCenter
        else if ( _idx and $400 ) <> 0 then
          Result := TGIS_LabelPosition.MiddleRight
        else
          Result := TGIS_LabelPosition.MiddleLeft ;
      end ;

      function getFontStyle( const _val : Integer ) : TGIS_FontStyles ;
      begin
        Result := GisGetEmptyFontStyle ;
        if ( _val and 1 ) <> 0 then
          Result := GisAddFontStyle( Result, TGIS_FontStyle.Bold ) ;
        if ( _val and 2 ) <> 0 then
          Result := GisAddFontStyle( Result, TGIS_FontStyle.Italic ) ;
        if ( _val and 4 ) <> 0 then
          Result := GisAddFontStyle( Result, TGIS_FontStyle.Underline ) ;
        if ( _val and 8 ) <> 0 then
          Result := GisAddFontStyle( Result, TGIS_FontStyle.StrikeOut ) ;
      end ;

      function lblHasOutline( const _val : Integer ) : Boolean ;
      begin
        Result := (_val and $0010) <> 0 ;
      end;
      function lblHasShadow( const _val : Integer ) : Boolean ;
      begin
        Result := (_val and $0020) <> 0 ;
      end;
      function lblHasBox( const _val : Integer ) : Boolean ;
      begin
        Result := (_val and $0100) <> 0 ;
      end;
      function lblHasHalo( const _val : Integer ) : Boolean ;
      begin
        Result := (_val and $0200) <> 0 ;
      end;

  begin
    penIndex    := 0;
    brushIndex  := 0;
    symbolIndex := 0;
    fontIndex   := 0;
    useCustom   := False ;
    cursorTab[ _cursor ].currRecord := shapeFile.ReadRecord( _cursor ) ;

    {$IFDEF OXYGENE}
      rec_type   := cursorTab[ _cursor ].currRecord.RecordType ;
      rec_number := cursorTab[ _cursor ].currRecord.RecordNumber ;
    {$ELSE}
      rec_type   := cursorTab[ _cursor ].currRecord^.RecordType ;
      rec_number := cursorTab[ _cursor ].currRecord^.RecordNumber ;
    {$ENDIF}

    case rec_type of
      GIS_TAB_SYMBOL,
      GIS_TAB_SYMBOL_C :
          begin
            compr := rec_type = GIS_TAB_SYMBOL_C ;

            if compr then shapeFile.ReadCenterFromBlock( _cursor ) ;

            cursorTab[ _cursor ].currPoint.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currPoint.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currPoint.AddPart ;
              cursorTab[ _cursor ].currPoint.AddPoint(
                shapeFile.ReadCoordinate( compr, _cursor )
              ) ;

            symbolIndex := shapeFile.ReadByte;

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currPoint ;
          end ;
      GIS_TAB_MULTIPOINT_C,
      GIS_TAB_MULTIPOINT,
      GIS_TAB_V800_MULTIPOINT,
      GIS_TAB_V800_MULTIPOINT_C :
          begin
            compr := rec_type = GIS_TAB_MULTIPOINT_C ;

            coord_ptr     := shapeFile.ReadLongword ;
            num_points    := shapeFile.ReadLongword ;

            shapeFile.ReadLongint;
            shapeFile.ReadLongint;
            shapeFile.ReadLongint;
            shapeFile.ReadByte;
            shapeFile.ReadByte;
            shapeFile.ReadByte;

            if ( rec_type = GIS_TAB_V800_MULTIPOINT   ) or
               ( rec_type = GIS_TAB_V800_MULTIPOINT_C ) then
            begin
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadByte ;
            end ;

            symbolIndex := shapeFile.ReadByte;
            penIndex    := 0;
            brushIndex  := 0;

            shapeFile.ReadByte;

            if compr then shapeFile.ReadCenterFromBlock( _cursor ) ;

            shapeFile.Position := coord_ptr ;

            cursorTab[ _cursor ].currMultipoint.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currMultipoint.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currMultipoint.AddPart ;
              for i:=1 to num_points do
                cursorTab[ _cursor ].currMultipoint.AddPoint(
                  shapeFile.ReadCoordinate( compr, _cursor  )
                ) ;

            cursorTab[ _cursor ].currShape
              := cursorTab[ _cursor ].currMultipoint ;
          end ;
      GIS_TAB_LINE_C,
      GIS_TAB_LINE :
          begin
            compr := rec_type = GIS_TAB_LINE_C ;

            if compr then shapeFile.ReadCenterFromBlock( _cursor ) ;

            cursorTab[ _cursor ].currArc.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currArc.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currArc.AddPart ;
              cursorTab[ _cursor ].currArc.AddPoint(
                shapeFile.ReadCoordinate( compr, _cursor )
              ) ;
              cursorTab[ _cursor ].currArc.AddPoint(
                shapeFile.ReadCoordinate( compr, _cursor )
              ) ;

            penIndex := shapeFile.ReadByte;

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currArc ;
          end ;
      GIS_TAB_PLINE_C,
      GIS_TAB_PLINE :
          begin
            compr := rec_type = GIS_TAB_PLINE_C ;

            coord_ptr     := shapeFile.ReadLongword ;
            coord_size    := shapeFile.ReadLongword ;

            if (coord_size and $80000000) <> 0  then
              coord_size := coord_size and $7FFFFFFF;

            if compr then begin // skip data
              shapeFile.ReadWord ;
              shapeFile.ReadWord ;
            end ;

            shapeFile.ReadCenter( _cursor ) ;
            shapeFile.ReadExtent( compr, _cursor ) ;

            penIndex    := shapeFile.ReadByte;
            brushIndex  := 0;

            if compr then num_points := coord_size div 4
                     else num_points := coord_size div 8 ;

            shapeFile.Position := coord_ptr ;

            cursorTab[ _cursor ].currArc.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currArc.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currArc.AddPart ;
              for i:=1 to num_points do
                cursorTab[ _cursor ].currArc.AddPoint(
                  shapeFile.ReadCoordinate( compr, _cursor  )
                ) ;

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currArc ;
          end ;
      GIS_TAB_ARC_C,
      GIS_TAB_ARC :
          begin
            compr := rec_type = GIS_TAB_ARC_C ;

            startAngle := shapeFile.ReadWord ;
            endAngle   := shapeFile.ReadWord ;

            shapeFile.ReadCenterFromBlock( _cursor );
            ext := shapeFile.ReadExtent( compr, _cursor ) ;

            penIndex   := shapeFile.ReadByte ;

            cursorTab[ _cursor ].currArc.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            if ( shapeFile.CoordOriginQuadrant = 1 ) or
               ( shapeFile.CoordOriginQuadrant = 3 ) or
               ( shapeFile.CoordOriginQuadrant = 0 ) then begin

                dStartAngle := startAngle / 10.0 ;
                dEndAngle   := endAngle   / 10.0 ;
            end
            else begin
                dStartAngle := endAngle   / 10.0 ;
                dEndAngle   := startAngle / 10.0 ;
            end ;

            if ( shapeFile.CoordOriginQuadrant = 2 ) or
               ( shapeFile.CoordOriginQuadrant = 3 ) or
               ( shapeFile.CoordOriginQuadrant = 0 ) then begin

                if dStartAngle <= 180.0 then
                  dStartAngle := 180.0 - dStartAngle
                else
                  dStartAngle := 540.0 - dStartAngle ;

                if dEndAngle <= 180.0 then
                  dEndAngle   := 180.0 - dEndAngle
                else
                  dEndAngle   := 540.0 - dEndAngle ;
            end ;

            if ( shapeFile.CoordOriginQuadrant = 3 ) or
               ( shapeFile.CoordOriginQuadrant = 4 ) or
               ( shapeFile.CoordOriginQuadrant = 0 ) then begin

                dStartAngle := 360.0 - dStartAngle ;
                dEndAngle   := 360.0 - dEndAngle ;
            end ;

            cursorTab[ _cursor ].currArc.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currArc.AddPart;

              dStartAngle := DegToRad( dStartAngle ) ;
              dEndAngle   := DegToRad( dEndAngle ) ;

              // Adjust angles to go counterclockwise
              if (dEndAngle < dStartAngle) then
                dEndAngle := dEndAngle + 2.0*Pi;

              sw  := ( dEndAngle - dStartAngle) / 90 ;
              ctr := GisPoint( ( ext.XMax + ext.XMin ) / 2,
                               ( ext.YMax + ext.YMin ) / 2 ) ;
              ax  := Abs( ext.XMax - ext.XMin ) / 2 ;
              ay  := Abs( ext.YMax - ext.YMin ) / 2 ;

              for i := 0 to 89 do begin
                an := dStartAngle + i*sw ;
                SinCos( an, sa, ca ) ;
                cursorTab[ _cursor ].currArc.AddPoint(
                  GisPoint( ctr.X + ax*ca,
                            ctr.Y + ay*sa
                          )
                ) ;
              end ;

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currArc ;
          end ;
      GIS_TAB_REGION_C,
      GIS_TAB_REGION,
      GIS_TAB_V450_REGION_C,
      GIS_TAB_V450_REGION,
      GIS_TAB_V800_REGION_C,
      GIS_TAB_V800_REGION :
          begin
            compr := ( rec_type = GIS_TAB_REGION_C      ) or
                     ( rec_type = GIS_TAB_V450_REGION_C ) or
                     ( rec_type = GIS_TAB_V800_REGION_C ) ;

            coord_ptr := shapeFile.ReadLongword ;
            shapeFile.ReadLongword ;

            if ( rec_type = GIS_TAB_V800_REGION_C  ) or
               ( rec_type = GIS_TAB_V800_REGION    ) then
            begin
              num_sections := shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadByte ;
            end
            else
              num_sections := shapeFile.ReadWord ;

            if compr then begin // skip data
              shapeFile.ReadWord ;
              shapeFile.ReadWord ;
            end ;

            shapeFile.ReadCenter( _cursor ) ;
            shapeFile.ReadExtent( compr, _cursor ) ;

            penIndex := shapeFile.ReadByte ;
            brushIndex := shapeFile.ReadByte ;

            cursorTab[ _cursor ].currPolygon.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currPolygon.Lock( TGIS_Lock.Internal ) ;
              shapeFile.Position := coord_ptr ;

              SetLength( arVeritces, num_sections ) ;

              num_tpoints := 0 ;
              for i := 1 to num_sections do begin
                if ( rec_type = GIS_TAB_V450_REGION   ) or
                   ( rec_type = GIS_TAB_V450_REGION_C ) or
                   ( rec_type = GIS_TAB_V800_REGION   ) or
                   ( rec_type = GIS_TAB_V800_REGION_C ) then
                begin
                  size := 28 ;
                  num_vertices := shapeFile.ReadLongword
                end
                else begin
                  size := 24 ;
                  num_vertices := shapeFile.ReadWord ;
                end ;

                if ( rec_type = GIS_TAB_V800_REGION_C  ) or
                   ( rec_type = GIS_TAB_V800_REGION    ) then
                  shapeFile.ReadLongword
                else
                  shapeFile.ReadWord ; // numHoles

                shapeFile.ReadExtent( compr, _cursor ) ;
                ptr_tmp  := ( shapeFile.ReadLongword - num_sections * size
                            ) div 8 ;

                {$IFDEF GIS_NORECORDS}
                  arVeritces[i-1] := new T_arVeritces ;
                {$ENDIF}
                arVeritces[i-1].Num  := num_vertices ;
                num_tpoints := num_tpoints + num_vertices ;
                if compr then arVeritces[i-1].Offset := ptr_tmp * 4
                         else arVeritces[i-1].Offset := ptr_tmp * 8 ;
              end ;

              SetLength( all_points, num_tpoints ) ;
              coord_ptr := shapeFile.Position ;
              k := 0 ;
              for i := 1 to num_sections do begin
                for j := 1 to arVeritces[ i-1 ].Num do begin
                  all_points[k] := shapeFile.ReadCoordinate( compr, _cursor ) ;
                  inc( k ) ;
                end ;
              end ;

              k := 0 ;
              for i := 1 to num_sections do begin
                if compr then k := arVeritces[ i-1 ].Offset div 4
                else          k := arVeritces[ i-1 ].Offset div 8 ;

                cursorTab[ _cursor ].currPolygon.AddPart ;
                for j := 0 to arVeritces[i-1].Num-1 do
                  cursorTab[ _cursor ].currPolygon.AddPoint( all_points[k+j] ) ;
              end ;

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currPolygon ;
          end ;
      GIS_TAB_TEXT_C,
      GIS_TAB_TEXT :
          begin
            cursorTab[ _cursor ].currShape := nil ;
            compr := rec_type = GIS_TAB_TEXT_C ;

            if compr then shapeFile.ReadCenterFromBlock( _cursor ) ;

            coord_ptr := shapeFile.ReadLongword ;
            size   := shapeFile.ReadWord ;

            align := shapeFile.ReadWord ;                // alignment
            rot   := shapeFile.ReadWord / 10.0 ;         // rotation
            style := shapeFile.ReadWord ;                // style

            b := shapeFile.ReadByte;
            g := shapeFile.ReadByte;
            r := shapeFile.ReadByte;
            fcolor :=  TGIS_Color.FromBGR( r, g, b ) ;

            b := shapeFile.ReadByte;
            g := shapeFile.ReadByte;
            r := shapeFile.ReadByte;
            bcolor :=  TGIS_Color.FromBGR( r, g, b ) ;

            scolor := TGIS_Color.Gray;

            cursorTab[ _cursor ].currPoint.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currPoint.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currPoint.AddPart ;
              cursorTab[ _cursor ].currPoint.AddPoint(
                shapeFile.ReadCoordinate( compr, _cursor )
              ) ;

            if not IgnoreShapeParams then begin
              cursorTab[ _cursor ].currPoint.Params.Labels.FontColor := fcolor;

              if lblHasShadow( style ) then
                cursorTab[ _cursor ].currPoint.Params.Labels.Color := scolor
              else if lblHasHalo( style ) then
                cursorTab[ _cursor ].currPoint.Params.Labels.Color := bcolor
              else
                cursorTab[ _cursor ].currPoint.Params.Labels.Color := fcolor ;
              if lblHasBox( style ) then begin
                cursorTab[ _cursor ].currPoint.Params.Labels.Pattern := TGIS_BrushStyle.Solid ;
                cursorTab[ _cursor ].currPoint.Params.Labels.Color   := bcolor ;
              end ;

              cursorTab[ _cursor ].currPoint.Params.Labels.Rotate    := -rot*(Pi/180);
              cursorTab[ _cursor ].currPoint.Params.Labels.Allocator := false ;
              cursorTab[ _cursor ].currPoint.Params.Labels.Alignment
                := TGIS_LabelAlignment.LeftJustify ;
              cursorTab[ _cursor ].currPoint.Params.Labels.Position
                := GisGetLabelPosition( getPosition( align ) );
              cursorTab[ _cursor ].currPoint.Params.Labels.FontStyle
                := getFontStyle( style ) ;

              if compr then theight := shapeFile.ReadWord      // text height
                       else theight := shapeFile.ReadLongword ;
              if theight = 0 then
                theight := 10;
              cursorTab[ _cursor ].currPoint.Params.Labels.FontSizeAsText
                := 'SIZE:' + DotFloatToStr( theight/10 * 0.69 ) + 'm' ;
              cursorTab[ _cursor ].currPoint.Params.Labels.Width := -99999;
            end ;

            shapeFile.ReadByte ;                  // font name
            shapeFile.ReadExtent( compr, _cursor ) ;
            penIndex := shapeFile.ReadByte ;     // pen

            shapeFile.Position := coord_ptr ;

            shapeFile.ReadString( stmp, size ) ;

            if not isLabelSet then begin

               if FindField( GIS_TAB_FLD_LABEL )  < 0 then
                 AddFieldInternal( GIS_TAB_FLD_LABEL , TGIS_FieldType.String, 5, 0 ) ;
               Params.Labels.Field := GIS_TAB_FLD_LABEL;

              isLabelSet := True ;
            end ;

            cursorTab[ _cursor ].currPoint.SetField( GIS_TAB_FLD_LABEL, stmp );

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currPoint ;

          end ;
      GIS_TAB_RECT_C,
      GIS_TAB_RECT :
          begin
            compr := rec_type = GIS_TAB_RECT_C ;

            if compr then shapeFile.ReadCenterFromBlock( _cursor ) ;
            ext := shapeFile.ReadExtent( compr, _cursor ) ;

            penIndex := shapeFile.ReadByte ;
            brushIndex := shapeFile.ReadByte ;

            cursorTab[ _cursor ].currPolygon.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currPolygon.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currPolygon.AddPart;
              cursorTab[ _cursor ].currPolygon.AddPoint(
                                                 GisPoint( ext.XMin, ext.YMin )
                                               );
              cursorTab[ _cursor ].currPolygon.AddPoint(
                                                 GisPoint( ext.XMax, ext.YMin )
                                               );
              cursorTab[ _cursor ].currPolygon.AddPoint(
                                                 GisPoint( ext.XMax, ext.YMax )
                                               );
              cursorTab[ _cursor ].currPolygon.AddPoint(
                                                 GisPoint( ext.XMin, ext.YMax )
                                               );

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currPolygon ;
          end ;
      GIS_TAB_ROUNDRECT_C,
      GIS_TAB_ROUNDRECT :
          begin
            compr := rec_type = GIS_TAB_RECT_C ;

            if compr then shapeFile.ReadCenterFromBlock( _cursor ) ;
            // corners radius
            if compr then begin
              shapeFile.ReadWord;
              shapeFile.ReadWord;
            end
            else begin
              shapeFile.ReadLongint;
              shapeFile.ReadLongint;
            end ;

            ext := shapeFile.ReadExtent( compr, _cursor ) ;

            penIndex := shapeFile.ReadByte ;
            brushIndex := shapeFile.ReadByte ;

            cursorTab[ _cursor ].currPolygon.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currPolygon.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currPolygon.AddPart;
              cursorTab[ _cursor ].currPolygon.AddPoint(
                                                 GisPoint( ext.XMin, ext.YMin )
                                               );
              cursorTab[ _cursor ].currPolygon.AddPoint(
                                                 GisPoint( ext.XMax, ext.YMin )
                                               );
              cursorTab[ _cursor ].currPolygon.AddPoint(
                                                 GisPoint( ext.XMax, ext.YMax )
                                               );
              cursorTab[ _cursor ].currPolygon.AddPoint(
                                                 GisPoint( ext.XMin, ext.YMax )
                                               );

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currPolygon ;
          end ;
      GIS_TAB_ELLIPSE_C,
      GIS_TAB_ELLIPSE :
          begin
            compr := rec_type = GIS_TAB_ELLIPSE_C ;

            shapeFile.ReadCenterFromBlock( _cursor );
            ext := shapeFile.ReadExtent( compr, _cursor ) ;

            penIndex   := shapeFile.ReadByte ;
            brushIndex := shapeFile.ReadByte ;

            cursorTab[ _cursor ].currArc.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currArc.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currArc.AddPart;
              cursorTab[ _cursor ].currArc.StrokeArc(
                GisPoint3D( ( ext.XMax + ext.XMin ) / 2,
                            ( ext.YMax + ext.YMin ) / 2, 0
                          ),
                Abs( ext.XMax - ext.XMin ) / 2,
                Abs( ext.YMax - ext.YMin ) / 2,
                0,
                2*Pi,
                0,
                90
              );
            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currArc ;
          end ;
      GIS_TAB_MULTIPLINE_C,
      GIS_TAB_MULTIPLINE,
      GIS_TAB_V450_MULTIPLINE_C,
      GIS_TAB_V450_MULTIPLINE,
      GIS_TAB_V800_MULTIPLINE_C,
      GIS_TAB_V800_MULTIPLINE :
          begin
            compr := ( rec_type = GIS_TAB_MULTIPLINE_C      ) or
                     ( rec_type = GIS_TAB_V450_MULTIPLINE_C ) or
                     ( rec_type = GIS_TAB_V800_MULTIPLINE_C ) ;

            coord_ptr := shapeFile.ReadLongword ;
            shapeFile.ReadLongword ;

            if ( rec_type = GIS_TAB_V800_MULTIPLINE   ) or
               ( rec_type = GIS_TAB_V800_MULTIPLINE_C ) then
            begin
              num_sections := shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadLongword ;
              shapeFile.ReadByte ;
            end
            else
              num_sections := shapeFile.ReadWord ;

            if compr then begin // skip data
              shapeFile.ReadWord ;
              shapeFile.ReadWord ;
            end ;

            shapeFile.ReadCenter( _cursor ) ;
            shapeFile.ReadExtent( compr, _cursor ) ;

            penIndex   := shapeFile.ReadByte ;
            brushIndex := shapeFile.ReadByte ;

            cursorTab[ _cursor ].currArc.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currArc.Lock( TGIS_Lock.Internal ) ;
              shapeFile.Position := coord_ptr ;

              SetLength( arVeritces, num_sections ) ;

              for i := 1 to num_sections do begin
                if ( rec_type = GIS_TAB_V450_MULTIPLINE   ) or
                   ( rec_type = GIS_TAB_V450_MULTIPLINE_C ) or
                   ( rec_type = GIS_TAB_V800_MULTIPLINE   ) or
                   ( rec_type = GIS_TAB_V800_MULTIPLINE_C ) then
                begin
                  size := 28 ;
                  num_vertices := shapeFile.ReadLongword
                end
                else begin
                  size := 24 ;
                  num_vertices := shapeFile.ReadWord ;
                end ;

                shapeFile.ReadWord ;
                shapeFile.ReadExtent( compr, _cursor ) ;
                ptr_tmp  := ( shapeFile.ReadLongword - num_sections * size
                            ) div 8 ;

                {$IFDEF GIS_NORECORDS}
                  arVeritces[i-1] := new T_arVeritces ;
                {$ENDIF}
                arVeritces[i-1].Num  := num_vertices ;
                if compr then arVeritces[i-1].Offset := ptr_tmp * 4
                         else arVeritces[i-1].Offset := ptr_tmp * 8 ;
              end ;

              coord_ptr := shapeFile.Position ;
              for i := 1 to num_sections do begin
                shapeFile.Position := coord_ptr ;
                shapeFile.Move( arVeritces[ i-1 ].Offset ) ;
                 cursorTab[ _cursor ].currArc.AddPart ;
                 for j := 1 to arVeritces[ i-1 ].Num do
                   cursorTab[ _cursor ].currArc.AddPoint(
                     shapeFile.ReadCoordinate( compr, _cursor )
                   ) ;
              end ;

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currArc ;
          end ;
      GIS_TAB_FONTSYMBOL_C,
      GIS_TAB_FONTSYMBOL :
          begin
            compr := rec_type = GIS_TAB_FONTSYMBOL_C ;

            if compr then shapeFile.ReadCenterFromBlock( _cursor ) ;

            useCustom := True ;
            symbolIndex := shapeFile.ReadByte ; // symbol
            customSymbol.symbolNo  := symbolIndex ;
            customSymbol.pointSize := shapeFile.ReadByte ; // point size

            shapeFile.ReadWord ; // font style

            b := shapeFile.ReadByte;
            g := shapeFile.ReadByte;
            r := shapeFile.ReadByte;
            fcolor :=  TGIS_Color.FromBGR( r, g, b ) ;

            customSymbol.rgbColor := fcolor ;

            b := shapeFile.ReadByte;
            g := shapeFile.ReadByte;
            r := shapeFile.ReadByte;
            bcolor :=  TGIS_Color.FromBGR( r, g, b ) ;

            customSymbol.rgbColor2 := bcolor ;

            customSymbol.rotation  := shapeFile.ReadWord ; // angle

            cursorTab[ _cursor ].currPoint.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currPoint.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currPoint.AddPart ;
              cursorTab[ _cursor ].currPoint.AddPoint(
                shapeFile.ReadCoordinate( compr, _cursor )
              ) ;

            fontIndex := shapeFile.ReadByte ; // font

            if not IgnoreShapeParams then begin
              cursorTab[ _cursor ].currPoint.Params.Marker.Color
                := fcolor ;
              cursorTab[ _cursor ].currPoint.Params.Marker.OutlineColor
                := bcolor ;
            end ;
            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currPoint ;
          end ;
      GIS_TAB_CUSTOMSYMBOL_C,
      GIS_TAB_CUSTOMSYMBOL :
          begin
            compr := rec_type = GIS_TAB_CUSTOMSYMBOL_C ;

            if compr then shapeFile.ReadCenterFromBlock( _cursor ) ;

            shapeFile.ReadByte ;  // unknown
            shapeFile.ReadByte ;  // color attribute

            cursorTab[ _cursor ].currPoint.Recreate(
              nil,
              nil,
              False,
              rec_number,
              self
            ) ;

            cursorTab[ _cursor ].currPoint.Lock( TGIS_Lock.Internal ) ;
              cursorTab[ _cursor ].currPoint.AddPart ;
              cursorTab[ _cursor ].currPoint.AddPoint(
                 shapeFile.ReadCoordinate( compr, _cursor )
              ) ;

            symbolIndex := shapeFile.ReadByte;

            cursorTab[ _cursor ].currShape := cursorTab[ _cursor ].currPoint ;
          end ;
      else
          begin
            cursorTab[ _cursor ].currShape := nil ;
          end ;
    end ;

    if assigned( cursorTab[ _cursor ].currShape ) then begin
      setShapeParams( cursorTab[ _cursor ].currShape.ShapeType,
                      brushIndex, penIndex,
                      symbolIndex,
                      fontIndex,
                      useCustom,
                      customSymbol,
                      _cursor
                    );
      cursorTab[ _cursor ].currShape.Unlock ;
      cursorTab[ _cursor ].currShape := getEdited(
                                          cursorTab[ _cursor ].currShape
                                        ) ;
    end ;
  end ;

  function TGIS_LayerTAB.getFieldInternal( const _uid      : TGIS_Uid;
                                           const _name     : String ;
                                           const _cursor   : Integer
                                         ) : Variant ;
  begin
    lockThread ;
    try
      if assigned( dataFile ) then
        Result := dataFile.GetField( _uid, _name, -1 )
      {$IFNDEF GIS_NOADO}
        else if assigned( cursorTab[ _cursor ].dataDB ) then begin
          if (_uid < 0) then begin
            Result := Unassigned ;
            exit ;
          end ;

          macroFetchRecord( _uid, _cursor ) ;

          if not cursorTab[ _cursor ].dataDB.sqlQueryEof(_cursor) then begin
            try
              if ( CompareText( GIS_TAB_FLD_LABEL , _name ) <> 0 ) and
                 ( CompareText( GIS_TAB_FLD_LINE  , _name ) <> 0 ) and
                 ( CompareText( GIS_TAB_FLD_SYMBOL, _name ) <> 0 ) then
                Result := cursorTab[ _cursor ].dataDB.sqlQueryGetField( _name, _cursor )
              else
                Result := Unassigned ;
            except
              Result := Unassigned ;
            end ;
          end
          else
            Result := Unassigned ;
        end
      {$ENDIF}
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerTAB.getBindedFieldInternal( const _shape  : TObject ;
                                                 const _field  : Integer ;
                                                 const _cursor : Integer
                                               ) : Variant ;
  var
    shp : TGIS_Shape ;
  begin
    lockThread ;
    try
      shp := TGIS_Shape( _shape ) ;
      if assigned( shp ) then begin
        if assigned( dataFile ) then
          Result := dataFile.GetField( shp.Uid, '', _field )
        {$IFNDEF GIS_NOADO}
          else if assigned( cursorTab[ _cursor ].dataDB ) then begin
            macroFetchRecord( shp.Uid, _cursor ) ;

            if not cursorTab[ _cursor ].dataDB.sqlQueryEof(_cursor) then begin
              try
                Result := cursorTab[ _cursor ].dataDB.sqlQueryGetFieldById( _field, _cursor ) ;
              except
                Result := Unassigned ;
              end ;
            end
            else
              Result := Unassigned ;
          end
        {$ENDIF}
        else
          Result := Unassigned ;
      end
      else
        Result := Unassigned ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerTAB.doCoordSys ;
  var
    dparams   : TGIS_DoubleArray ;
    dparamsEx : TGIS_DoubleArray ;
  begin
    SetLength( dparams, 6 ) ;
    dparams[ 0 ] := shapeFile.ProjParams[ 0 ] ;
    dparams[ 1 ] := shapeFile.ProjParams[ 1 ] ;
    dparams[ 2 ] := shapeFile.ProjParams[ 2 ] ;
    dparams[ 3 ] := shapeFile.ProjParams[ 3 ] ;
    dparams[ 4 ] := shapeFile.ProjParams[ 4 ] ;
    dparams[ 5 ] := shapeFile.ProjParams[ 5 ] ;

    SetLength( dparamsEx, 8 ) ;
    dparamsEx[ 0 ] := shapeFile.DatumShiftX ;
    dparamsEx[ 1 ] := shapeFile.DatumShiftY ;
    dparamsEx[ 2 ] := shapeFile.DatumShiftZ ;
    dparamsEx[ 3 ] := shapeFile.DatumParams[0] ;
    dparamsEx[ 4 ] := shapeFile.DatumParams[1] ;
    dparamsEx[ 5 ] := shapeFile.DatumParams[2] ;
    dparamsEx[ 6 ] := shapeFile.DatumParams[3] ;

    try
      CS := TGIS_CSFactoryMapInfo.BuildCs( shapeFile.ProjId,
                               shapeFile.DatumId,
                               shapeFile.EllipsoidId,
                               IntToStr( shapeFile.UnitsId ),
                               dparams,
                               dparamsEx
                              ) ;
    except
      CS := CSUnknownCoordinateSystem ;
      raise ;
    end ;
  end ;

  procedure TGIS_LayerTAB.setUp ;
  var
    tab_file  : TGIS_FileTAB ;
    i         : Integer ;
    dtype     : Integer ;

    // remove _DUMMY field if exists
    procedure remove_dummy_field ;
    var
      id   : Integer ;
      fld : TGIS_FieldInfo ;
    begin
      id := FindField( GIS_FIELD_DUMMY ) ;
      if id >= 0 then begin
        fld := FieldInfo( id ) ;
        fld.Deleted := True ;
      end
    end ;

    procedure merge_tab_dbf_structure ;
    var
      lv      : TGIS_LayerVector ;
      j       : Integer ;
      fld     : TGIS_FieldInfo ;
      fld_tab : TGIS_FieldInfo ;
    begin
      lv := TGIS_LayerVector.Create ;
      try
        tab_file.ReadFieldsStructure( lv ) ;
        for j := 0 to Self.Fields.Count - 1 do begin
          fld := FieldInfo( j ) ;
          fld_tab := lv.FieldInfo( j ) ;
          if assigned( fld_tab ) then
            Self.RenameField( fld.NewName, fld_tab.Name, fld.Width, fld.Decimal ) ;
        end ;
        Self.ClearModified ;
      finally
        FreeObject( lv ) ;
      end ;
    end ;

  begin
    inherited ;

    lastFontIndex := -1 ;
    lastSymbolNo  := -1 ;
    lastSymbol    := nil ;
    isLabelSet    := False ;

    // Read field from TAB structure
    tab_file := TGIS_FileTAB.Create( PathTAB, False ) ;
    try
      dtype := tab_file.Prerecognize ;
      case dtype of
        1:   dataFile := TGIS_FileDBF.Create(
                           PathDAT, Self, TGIS_StreamMode.Read, nil, nil
                         ) ;
        2:   dataFile := TGIS_FileDBF.Create(
                           PathDBF, Self, TGIS_StreamMode.Read, nil, nil
                         ) ;
        {$IFNDEF GIS_NOADO}
          4: cursorTab[ 0 ].dataDB := TGIS_DbAdo.Create ;
        {$ENDIF}
        5:   dataFile := TGIS_FileDBF.Create(
                           PathDAT, Self, TGIS_StreamMode.Read, nil, nil
                         ) ;
        else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ),
                                          PathTAB, 0
                                        ) ;
      end ;

      CodePage   := tab_file.CodePage ;
      {$IFNDEF GIS_NOADO}
        if assigned( cursorTab[ 0 ].dataDB ) then begin
          sqlParams.Add( 'DIALECT=MSJET' ) ;
          sqlParams.Add( 'ADO=Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' +
                         GetFilePath( Path ) + tab_file.FilePath
                       ) ;
          cursorTab[ 0 ].dataDB.sqlConnect( Path, sqlParams );
        end ;
      {$ENDIF}

      tableName := tab_file.TableName ;

      if assigned( dataFile ) then begin
       if ( dtype = 1 ) then
          tab_file.ReadFieldsStructure( self )
       else if ( dtype = 2 ) then
         merge_tab_dbf_structure ;
      end
      {$IFNDEF GIS_NOADO}
        else
         ReadDBFieldsStructure(0)
      {$ENDIF} ;
    finally
      FreeObject( tab_file ) ;
    end ;

    // open shape file
    lastUid   := -1 ;
    shapeFile := TGIS_FileTAB_MAP.Create( PathMAP, False ) ;

    for i := 0 to length( cursorTab ) - 1 do begin
      if cursorTab[i].curInUse then begin
        shapeFile.CursorOpen( i ) ;
      end ;
    end ;
    shapeFile.ReadHeader ;
    shapeFile.CodePage := CodePage ;
    Extent := shapeFile.Extent ;

    // open index file
    indexFile := TGIS_FileTAB_ID.Create( PathID, False ) ;
    for i := 0 to length( cursorTab ) - 1 do begin
      if cursorTab[i].curInUse then begin
        indexFile.CursorOpen( i ) ;
      end ;
    end ;
    indexFile.ReadHeader ;

    // add field for text labels
    try
      if FindField( GIS_TAB_FLD_LINE )   < 0 then
        AddFieldInternal( GIS_TAB_FLD_LINE  , TGIS_FieldType.Number, 5, 0 ) ;
      if FindField( GIS_TAB_FLD_SYMBOL ) < 0 then
        AddFieldInternal( GIS_TAB_FLD_SYMBOL, TGIS_FieldType.Number, 5, 0 ) ;
    except
    end ;

    remove_dummy_field ;

    doCoordSys ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'MapInfo Native Format (TAB/MAP)' ;
  end ;

  function  TGIS_LayerTAB.cursorOpen   :  Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length(cursorTab)  then
        SetLength( cursorTab, Result + 1 ) ;
      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorTab[Result] ) then
          cursorTab[Result] := new T_cursorTab ;
      {$ENDIF}
      cursorTab[Result].curInUse := True ;

      cursorTab[Result].currPoint      := TGIS_ShapePoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorTab[Result].currMultipoint := TGIS_ShapeMultiPoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorTab[Result].currArc        := TGIS_ShapeArc.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorTab[Result].currPolygon    := TGIS_ShapePolygon.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      {$IFNDEF GIS_NOADO}
        // if first cursor has database, we know it exists
        if ( Result > 0 ) and assigned( cursorTab[0].dataDB ) then begin
          cursorTab[Result].dataDB := TGIS_DbAdo.Create ;
          try
            cursorTab[Result].dataDB.sqlConnect( Path, sqlParams );
          except
            // parameters can be wrong
          end ;
        end ;
      {$ENDIF}

      if assigned( shapeFile ) then
        shapeFile.CursorOpen( Result );
      if assigned( indexFile ) then
        indexFile.CursorOpen( Result );
      {$IFNDEF GIS_NOADO}
        if assigned( cursorTab[Result].dataDB ) then
          cursorTab[Result].dataDB.cursorOpen( Result );
      {$ENDIF}
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerTAB.cursorClose  ( const _cursor      : Integer
                                       ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorTab[_cursor].curInUse := False ;
      FreeObject( cursorTab[_cursor].currPoint ) ;
      FreeObject( cursorTab[_cursor].currMultipoint ) ;
      FreeObject( cursorTab[_cursor].currArc ) ;
      FreeObject( cursorTab[_cursor].currPolygon ) ;

      {$IFNDEF GIS_NOADO}
        if assigned( cursorTab[_cursor].dataDB ) then
          FreeObject( cursorTab[_cursor].dataDB ) ;
      {$ENDIF}

      // truncate cursorState at the tail;
      for i := length( cursorTab ) - 1 downto 0 do begin
        if not cursorTab[i].curInUse then begin
          SetLength( cursorTab, i ) ;
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

  procedure TGIS_LayerTAB.cursorFirst( const _cursor      : Integer          ;
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
      cursorTab[_cursor].currShape := nil ;

      if GisIsNoWorld( _extent ) then
        exit ;

      inherited cursorFirstInternal(
                  _cursor, _viewerCS,
                  _extent, _query, _shape, _de9im, _skipDeleted
                ) ;

      cursorTab[_cursor].lastTraversedUid := 1 ;
      cursorTab[_cursor].isFirst          := True ;

      if assigned( shapeFile ) then begin
        shapeFile.FlushBuffer ;
        shapeFile.ReadHeader ;
      end ;
      if assigned( indexFile ) then
        indexFile.FlushBuffer ;
      if assigned( dataFile ) then
        dataFile.FlushBuffer ;
      {$IFNDEF GIS_NOADO}
        if assigned( cursorTab[_cursor].dataDB ) then
          cursorTab[_cursor].dataDB.sqlQueryOpen(
            Format( 'SELECT * FROM %s', [ tableName ] ), _cursor
          ) ;
      {$ENDIF}

      cursorNext( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerTAB.cursorNext( const _cursor : Integer ) ;
  var
    {$IFDEF OXYGENE}
      shx : TGIS_RecordTAB_ID ;
    {$ELSE}
      shx : PGIS_RecordTAB_ID ;
    {$ENDIF}
    ihglass : Integer ;
  begin
    lockThread ;
    try
      if not assigned( indexFile ) or not assigned( shapeFile ) then exit ;

    // read each shape
        ihglass := 0 ;
        while True do begin
         inc( ihglass ) ;
         if ihglass mod GIS_PROGRESS_TRESHOLD = 0 then begin
           if HourglassShake then begin
             cursorTab[ _cursor ].currShape := nil ;
             break ;
           end ;
         end ;

         shx := indexFile.ReadRecord( cursorTab[ _cursor ].lastTraversedUid,
                                      _cursor
                                    ) ;
         {$IFDEF OXYGENE}
           if shx.Offset > 0 then begin
         {$ELSE}
           if shx^.Offset > 0 then begin
         {$ENDIF}
           {$IFDEF OXYGENE}
             shapeFile.Position := shx.Offset ;
           {$ELSE}
             shapeFile.Position := shx^.Offset ;
           {$ENDIF}
           {$IFNDEF GIS_NOADO}
             if assigned( cursorTab[_cursor].dataDB ) and
                ( not cursorTab[_cursor].dataDB.sqlQueryEof(_cursor) ) and
                not cursorTab[ _cursor ].isFirst then
               cursorTab[_cursor].dataDB.sqlQueryMoveNext(_cursor) ;
           {$ENDIF}

           cursorTab[ _cursor ].isFirst := False ;
           readShape( _cursor ) ;
         end
         else
           cursorTab[ _cursor ].currShape := nil ;

         if cursorTab[ _cursor ].currShape = nil then begin
           if cursorTab[ _cursor ].lastTraversedUid < indexFile.RecordsCount
           then begin
             inc( cursorTab[ _cursor ].lastTraversedUid ) ;
             continue ;
           end ;

           cursorTab[ _cursor ].currShape :=
              inherited GetShape( cursorTab[ _cursor ].lastTraversedUid,
                                  _cursor
                                ) ;
           if cursorTab[ _cursor ].currShape = nil then exit ;
         end ;

         inc( cursorTab[ _cursor ].lastTraversedUid ) ;

         if cursorState[_cursor].curSkipDeleted and
            cursorTab[_cursor].currShape.IsDeleted then
         begin
           continue ;
         end ;

         if not isInScope( cursorTab[_cursor].currShape, _cursor ) then
           continue
         else
           exit ;

       end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerTAB.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := cursorTab[_cursor].currShape = nil ;
  end ;

  function TGIS_LayerTAB.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if assigned( cursorTab[_cursor].currShape ) then
       Result := cursorTab[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;
  end ;

  function TGIS_LayerTAB.GetShape(
    const _uid    : TGIS_Uid ;
    const _cursor : Integer
  ) : TGIS_Shape ;
  var
    {$IFDEF OXYGENE}
      shx : TGIS_RecordTAB_ID ;
    {$ELSE}
      shx : PGIS_RecordTAB_ID ;
    {$ENDIF}
  begin
    lockThread ;
    try
      Result := nil ;
      if _uid <= 0 then exit ;

      // if it is in edited list
         Result := inherited GetShape( _uid, _cursor ) ;
         if Result <> nil then exit ;

      // is it a current shape
         if assigned( cursorTab[_cursor].currShape )    and
            ( _uid = cursorTab[_cursor].currShape.Uid ) then
         begin
           Result := cursorTab[_cursor].currShape ;
           exit ;
         end ;

      // if index file
         shx := indexFile.ReadRecord( _uid, _cursor ) ;
         {$IFDEF OXYGENE}
           if shx.Offset > 0 then begin
         {$ELSE}
           if shx^.Offset > 0 then begin
         {$ENDIF}
           {$IFDEF OXYGENE}
             shapeFile.Position := shx.Offset ;
           {$ELSE}
             shapeFile.Position := shx^.Offset ;
           {$ENDIF}
           readShape( _cursor ) ;
           Result := cursorTab[_cursor].currShape ;
         end
         else
           Result := nil ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerTAB.GetLastUid : TGIS_Uid ;
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

  function TGIS_LayerTAB.GetNewUid : TGIS_Uid ;
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

  function TGIS_LayerTAB.PreRecognize( const _path     : String ;
                                         var _new_path : String
                                     ) : Boolean ;
  var
    tab     : TGIS_FileTAB ;
    bsless  : Boolean ;
  begin
    Result := inherited PreRecognize( _path, _new_path );

    tab := TGIS_FileTAB.Create( _path, False ) ;
    try
      case tab.Prerecognize of
        0 : if UpperCase( GetFileExt( _path ) ) = '.TAB' then
              Result := True
            else
              Result := False ;
        1 : Result := True  ;
        2 : Result := True  ;
        3 : begin
              Result    := False ;
              _new_path := GetPathAbsolute( GetFilePath( _path ),
                                            tab.FilePath
                                          ) ;
            end ;
        5 : begin
              bsless := GisMetadataAsBoolean( TAB_METADATA_OPEN_SEAMLESS, False ) ;
              if bsless then begin
                Result := False  ;
                _new_path := _path ;
              end
              else
                Result := True  ;
            end ;
      end ;
    finally
      FreeObject( tab ) ;
    end ;
  end ;

  procedure TGIS_LayerTAB.CloseFiles;
  begin
    FreeObject( shapeFile      ) ;
    FreeObject( indexFile      ) ;
    FreeObject( dataFile       ) ;
  end ;

  procedure TGIS_LayerTAB.Build( const _path   : String ;
                                 const _extent : TGIS_Extent;
                                 const _type   : TGIS_ShapeType ;
                                 const _dim    : TGIS_DimensionType
                                ) ;
  var
    tab : TGIS_FileTAB ;
  begin
    inherited ;

    FreeObject( shapeFile ) ;
    FreeObject( indexFile ) ;
    FreeObject( dataFile  ) ;

    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    tab := TGIS_FileTAB.Create( _path, True ) ;
    try
      // DBF file must have at least one field
      if Fields.Count = 0 then
        AddField( '_DUMMY', TGIS_FieldType.String, 1, 0 ) ;

      tab.Write( self, self, _extent, _type, '', nil, '', False );
    finally
      FreeObject( tab ) ;
    end ;
  end ;

  procedure TGIS_LayerTAB.ImportLayerEx(
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent ;
    const _type      : TGIS_ShapeType ;
    const _scope     : String ;
    const _shape     : TGIS_Shape ;
    const _de9im     : String ;
    const _truncated : Boolean
  ) ;
  var
    tab       : TGIS_FileTAB ;
    same_name : Boolean ;
    isrtree   : Boolean ;
    c         : Integer ;
  begin
    if not assigned( _layer ) then exit ;

    if not ( CheckFileWriteAccessEx( PathTAB   , True , True, True ) and
             CheckFileWriteAccessEx( PathDAT   , True , True, True ) and
             CheckFileWriteAccessEx( PathID    , True , True, True ) and
             CheckFileWriteAccessEx( PathRTree , False, True, True )
           )
    then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    same_name := CompareText( GetPathAbsolute( '', Path        ),
                              GetPathAbsolute( '', _layer.Path )
                            ) = 0  ;
    isrtree := _layer.UseRTree ;

    tab := TGIS_FileTAB.Create( Path, True ) ;
    try
      tab.Write( self, _layer, _extent, _type, _scope, _shape, _de9im, _truncated );
    finally
      FreeObject( tab ) ;

      if same_name then begin
        FreeObject( FRTreeFile ) ;
        UseRTree := False ;

        Items.Clear ;
        Fields.Clear ;

        setUp ;
        setUp2 ;
        UseRTree := isrtree ;

        if assigned( Viewer ) then
          Viewer.Ref.InvalidateExtent(Extent) ;

      end ;

      for c := 0 to BUILTIN_CURSORS - 1 do
        cursorTab[c].currShape := nil ;

      FIsModified := False ;

      if not IsOpened then
        Open ;
    end ;
  end ;

 procedure TGIS_LayerTAB.SaveData  ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;


//==============================================================================
// Lider.CG.GIS.GeoLayerTAB
//==============================================================================

  class procedure GisLayerTAB.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TAB', 'MapInfo Native Format',
                   TGIS_LayerTAB, '.tab',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                    True
                 ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    GisLayerTAB.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

