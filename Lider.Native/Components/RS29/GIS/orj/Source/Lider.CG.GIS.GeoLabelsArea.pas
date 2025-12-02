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
  Label Area Reservation.

  Avoid label overlapping.

  Each label is represented as a rectangle.

  Before drawing a label program, check if there is enough space for the label.
  After drawing a label, a rectangle occupied by this label will be added to
  the list of occupied space.
}

{$IFDEF DCC}
  unit GisLabelsArea ;
  {$HPPEMIT '#pragma link "GisLabelsArea"'}
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
    TatukGIS.RTL,
    System.Runtime.InteropServices ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Generics.Collections,
    System.Math,

    GisRtl,
    GisInterfaces,
    GisTypes ;
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

  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
  {$ENDIF}

  /// <summary>
  ///   Like TRect, but based on doubles instead of integers.
  /// </summary>
  TGIS_ExtentUid = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFDEF OXYGENE} public {$ENDIF}

      /// <summary>
      ///   left border
      /// </summary>
      XMin  : Double ;

      /// <summary>
      ///   bottom border
      /// </summary>
      YMin  : Double ;

      /// <summary>
      ///   right border
      /// </summary>
      XMax  : Double ;

      /// <summary>
      ///   top border
      /// </summary>
      YMax  : Double ;
      /// <summary>
      ///   layer index
      /// </summary>
      Layer : Integer ;
      /// <summary>
      ///   unique identifier
      /// </summary>
      Uid   : TGIS_Uid ;
  end ;

  /// <summary>
  ///   Label area allocator.
  /// </summary>
  TGIS_LabelsArea = {$IFDEF OXYGENE} public {$ENDIF}
                    class( TGIS_LabelsAreaAbstract )
    private

      {$IFDEF MANAGED}
        viewerObj : TObject ;
      {$ELSE}
        viewerObj : Pointer ;
      {$ENDIF}

        /// <summary>
        ///   List of duplicates.
        /// </summary>
        lstDuplicates : TGIS_StringList ;

        /// <summary>
        ///   All allocated labels.
        /// </summary>
        lstItems : {$IFNDEF MANAGED}
                     TObjectList< TGIS_ExtentUid > ;
                   {$ELSE}
                     TList< TGIS_ExtentUid > ;
                   {$ENDIF}
    protected

      /// <summary>
      ///   Destroy label renderer.
      /// </summary>
      procedure doDestroy ; override;
    public
      // constructors

       /// <summary>
       ///   Construct label renderer.
       /// </summary>
       /// <param name="_viewer">
       ///   address of viewer object
       /// </param>
       constructor Create( const _viewer : {$IFDEF MANAGED}
                                             TObject
                                           {$ELSE}
                                             Pointer
                                           {$ENDIF}
                         ) ;
       /// <inheritdoc/>
       procedure  Reset          ; override;

       /// <inheritdoc/>
       function   IsDuplicated   ( const _label     : String
                                 ) : Boolean ;
                                 override;
       /// <inheritdoc/>
       procedure  AddDuplicated  ( const _label     : String
                                 ) ;
                                 override;
       /// <inheritdoc/>
       function   Allocate       ( const _rect      : TGIS_Extent ;
                                   const _layer     : Integer ;
                                   const _uid       : TGIS_Uid ;
                                   const _tolerance : Double
                                 ) : Boolean ;
                                 override;
       /// <inheritdoc/>
       function   FindShape      ( const _ptg       : TGIS_Point ;
                                   const _layer     : Integer ;
                                     var _uid       : TGIS_Uid
                                 ) : Boolean ;
                                 override;
       /// <inheritdoc/>
       function   AllocateEx     ( const _ptgA      : TGIS_Point ;
                                   const _ptgB      : TGIS_Point ;
                                   const _ptgC      : TGIS_Point ;
                                   const _ptgD      : TGIS_Point ;
                                   const _layer     : Integer ;
                                   const _uid       : TGIS_Uid ;
                                   const _tolerance : Double
                                 ) : Boolean ;
                                 override;
       /// <inheritdoc/>
       function   IsAny          ( const _layer     : Integer ;
                                   const _uid       : TGIS_Uid
                                 ) : Boolean ;
                                 override;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Types,
    GisFunctions,
    GisViewer ;
{$ENDIF}

//==============================================================================
// TGIS_LabelsArea
//==============================================================================

  constructor TGIS_LabelsArea.Create(
    const _viewer : {$IFDEF MANAGED}
                      TObject
                    {$ELSE}
                      Pointer
                    {$ENDIF}
  ) ;
  begin
    {$IFDEF CLR}
      inherited Create ;
    {$ENDIF}
    viewerObj := _viewer ;

    lstDuplicates := TGIS_StringList.Create ;
    lstDuplicates.Sorted := True ;
    lstDuplicates.Duplicates := TDuplicates.dupIgnore ;
  end ;

  procedure TGIS_LabelsArea.doDestroy ;
  begin
    FreeObject( lstDuplicates ) ;

    if assigned( lstItems ) then begin
      FreeObject( lstItems ) ; // free list of rectangles
    end ;
    inherited ;
  end ;

  function TGIS_LabelsArea.IsDuplicated(
    const _label  : String
  ) : Boolean ;
  var
    i : Integer ;
  begin
    Result := lstDuplicates.Find( _label, i )
  end ;

  procedure TGIS_LabelsArea.AddDuplicated(
    const _label : String
  ) ;
  var
    i : Integer ;
  begin
    if not lstDuplicates.Find( _label, i ) then
      lstDuplicates.Add( _label ) ;
  end ;

  function TGIS_LabelsArea.Allocate(
    const _rect      : TGIS_Extent ;
    const _layer     : Integer ;
    const _uid       : TGIS_Uid ;
    const _tolerance : Double
  ) : Boolean ;
  var
    rect  : TGIS_ExtentUid ;
    i     : Integer     ;
    pixel : Double      ;
  begin
    Result := False ;

    if assigned( viewerObj ) then begin
      with TGIS_Viewer( viewerObj ) do begin
        if not GisIsNoWorld( TemporaryVisibleExtent ) then begin
          if not GisIsContainExtent( _rect, TemporaryVisibleExtent ) then
            exit ;
        end
        else begin
          if not GisIsContainExtent( _rect, VisibleExtent ) then
            exit ;
        end ;
      end ;
    end;

    if lstItems = nil then
      lstItems :=  {$IFDEF MANAGED}
                     TList< TGIS_ExtentUid >.Create() ;
                   {$ELSE}
                     TObjectList< TGIS_ExtentUid >.Create( True ) ;
                   {$ENDIF}

    pixel := 0 ;

    Result := True ;

    if lstItems.Count > 0 then begin
      for i := 0 to lstItems.Count -1 do begin
        rect := lstItems[i] ;

        if ( Abs( _rect.XMin - rect.XMin ) < _tolerance ) and
           ( Abs( _rect.XMax - rect.XMax ) < _tolerance ) and
           ( Abs( _rect.YMin - rect.YMin ) < _tolerance ) and
           ( Abs( _rect.YMax - rect.YMax ) < _tolerance ) and
           ( _layer = rect.Layer ) and
           ( _uid = rect.Uid     )
        then begin
           Result := True ;
           exit ;
        end ;

        if  ( ( _rect.XMin - pixel ) <= rect.XMax ) and
            ( ( _rect.XMax + pixel ) >= rect.XMin ) and
            ( ( _rect.YMin - pixel ) <= rect.YMax ) and
            ( ( _rect.YMax + pixel ) >= rect.YMin )
        then begin
          Result := False ;
          break ;
        end;
      end;
    end ;

    if Result = False then
      exit ;

    rect := TGIS_ExtentUid.Create ;

    rect.XMin := _rect.XMin ;
    rect.YMin := _rect.YMin ;
    rect.XMax := _rect.XMax ;
    rect.YMax := _rect.YMax ;
    rect.Uid  := _uid ;
    rect.Layer:= _layer ;

    lstItems.Add(rect);
  end ;

  function TGIS_LabelsArea.FindShape(
    const _ptg    : TGIS_Point ;
    const _layer  : Integer ;
      var _uid    : TGIS_Uid
  ) : Boolean ;
  var
    rect  : TGIS_ExtentUid ;
    i     : Integer     ;
  begin
    Result := False ;
    _uid := -1 ;

    if lstItems = nil then exit ;

    if lstItems.Count > 0 then
      for i := 0 to lstItems.Count -1 do begin
        rect := lstItems[i] ;
        if ( _ptg.X < rect.XMin ) or
           ( _ptg.X > rect.XMax ) or
           ( _ptg.Y < rect.YMin ) or
           ( _ptg.Y > rect.YMax )
        then
          continue ;

        if rect.Layer <> _layer then Continue ;
         _uid   := rect.Uid ;

        Result := True ;
        break ;
    end ;
  end ;

  function TGIS_LabelsArea.AllocateEx(
    const _ptgA      : TGIS_Point ;
    const _ptgB      : TGIS_Point ;
    const _ptgC      : TGIS_Point ;
    const _ptgD      : TGIS_Point ;
    const _layer     : Integer    ;
    const _uid       : TGIS_Uid   ;
    const _tolerance : Double
  ) : Boolean ;
  var
    ext : TGIS_Extent ;
  begin
    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent ;
    {$ENDIF}
    ext.XMin := Min( Min(_ptgA.X,_ptgB.X ), Min(_ptgC.X,_ptgD.X ) ) ;
    ext.XMax := Max( Max(_ptgA.X,_ptgB.X ), Max(_ptgC.X,_ptgD.X ) ) ;
    ext.YMin := Min( Min(_ptgA.Y,_ptgB.Y ), Min(_ptgC.Y,_ptgD.Y ) ) ;
    ext.YMax := Max( Max(_ptgA.Y,_ptgB.Y ), Max(_ptgC.Y,_ptgD.Y ) ) ;

    Result := Allocate( ext, _layer, _uid, _tolerance ) ;
  end ;

  function TGIS_LabelsArea.IsAny(
    const _layer     : Integer ;
    const _uid       : TGIS_Uid
  ) : Boolean ;
  var
    rect  : TGIS_ExtentUid ;
    i     : Integer     ;
  begin
    Result := False ;

    if lstItems = nil then
      lstItems :=  {$IFDEF MANAGED}
                     TList< TGIS_ExtentUid >.Create() ;
                   {$ELSE}
                     TObjectList< TGIS_ExtentUid >.Create( True ) ;
                   {$ENDIF}

    if lstItems.Count > 0 then begin
      for i := 0 to lstItems.Count -1 do begin
        rect := lstItems[i] ;

        if ( _layer = rect.Layer ) and
           ( _uid = rect.Uid     )
        then begin
           Result := True ;
           exit ;
        end ;
      end;
    end ;
  end ;

  procedure TGIS_LabelsArea.Reset ;
  begin
    lstDuplicates.Clear ;
    if assigned( lstItems ) then begin
      lstItems.Clear ;
    end ;
  end ;

//==================================== END =====================================
end.

