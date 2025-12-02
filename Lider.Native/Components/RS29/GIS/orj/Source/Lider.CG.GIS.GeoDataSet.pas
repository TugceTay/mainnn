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
  Encapsulation of a dataset.
}

unit GisDataSet ;
{$HPPEMIT '#pragma link "GisDataSet"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  Data.DB,

  GisRtl,
  GisTypes,
  GisFunctions,
  GisInterfaces,
  GisLayerVector,
  GisFieldRules,
  GisResource ;

type
  //TRecordBuffer = PChar ;

  /// <summary>
  ///   Dataset providing data from the TGIS_LayerVector.
  /// </summary>
  /// <remarks>
  ///   Call Open() to open dataset base on selected parameters.
  /// </remarks>
  [ComponentPlatformsAttribute( pidAllPlatforms )]
  TGIS_DataSet = class (TDataSet, IGIS_Subscribe)
    private // properties internal values
      FOnBusy             : TGIS_BusyEvent     ;
      FShowVirtualFields  : Boolean            ;
      FVirtualFields      : TGIS_VirtualFields ;
    private // record data and status
      lstRecords    : TGIS_ObjectList   ;
      currRecord    : Integer           ;
      currShape     : TGIS_Shape        ;
      oldLayer      : TGIS_LayerVector  ;
      oldShape      : TGIS_Shape        ;
      {$IFDEF GIS_PDK}
       oInternalLayer: TGIS_LayerVector ;
      {$ENDIF}
    private // scope
      dsLayer       : TGIS_LayerVector  ;
      dsExtent      : TGIS_Extent       ;
      dsQuery       : String            ;
      dsShape       : TGIS_Shape        ;
      dsDe9im       : String            ;
      dsSkipDeleted : Boolean           ;
      dsMaxRecords  : Integer           ;
      uponDestroy   : Boolean           ;
      uponClose     : Boolean           ;
      isBusy        : Boolean           ;
      currCursor    : Integer           ;
      bReadOnly     : Boolean           ;
      [weak]
      oViewer       : IGIS_Viewer       ;
    private // various private routine

      /// <summary>
      ///   Prepare OnBusy handling.
      /// </summary>
      procedure doBusyPrepare           ;

      /// <summary>
      ///   Release OnBusy handling.
      /// </summary>
      procedure doBusyRelease           ;

      /// <summary>
      ///   Report OnBusy state.
      /// </summary>
      /// <param name="_pos">
      ///   current progress
      /// </param>
      /// <param name="_end">
      ///   maximal progress value
      /// </param>
      function  doBusyShake             (  const _pos : Integer ;
                                           const _end : Integer
                                        ) : Boolean ;

      /// <summary>
      ///   Return Uid of the current record.
      /// </summary>
      function  getUid                  : TGIS_Uid ;
    private // property access routines

      function  fget_ActiveShape        : TGIS_Shape ; virtual ;

    private // private methods

      function  getFieldDataInternal (      _field        : TField        ;
                                            _buffer       : Pointer
                                     ) : Boolean ;
      procedure setFieldDataInternal (     _field         : TField        ;
                                           _buffer        : Pointer
                                     ) ;

      procedure setBookmarkDataInternal(
                                           _buffer        : {$IFDEF NEXTGEN}
                                                              TRecBuf     ;
                                                            {$ELSE}
                                                              TRecordBuffer ;
                                                            {$ENDIF}
                                           _data          : Pointer
                                     ) ;
      procedure getBookmarkDataInternal(
                                           _buffer        : {$IFDEF NEXTGEN}
                                                              TRecBuf     ;
                                                            {$ELSE}
                                                              TRecordBuffer ;
                                                            {$ENDIF}
                                           _data          : Pointer
                                     ) ;

    protected // dataset virtual methods

      {$IFDEF NEXTGEN}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <returns>
        ///   See TDataSet.
        /// </returns>
        function AllocRecBuf          : TRecBuf; override;
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <returns>
        ///   See TDataSet.
        /// </returns>
        function AllocRecordBuffer    : TRecordBuffer; override;
      {$ENDIF}

      {$IFDEF NEXTGEN}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        procedure FreeRecBuf          ( var    _buffer      : TRecBuf
                                      ); override;
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        procedure FreeRecordBuffer    ( var   _buffer       : TRecordBuffer
                                      ) ; override;
      {$ENDIF}

      {$IFDEF LEVEL_XE3_VCL}
        {$IFDEF NEXTGEN}
          /// <summary>
          ///   See TDataSet.
          /// </summary>
          /// <param name="_buffer">
          ///   see TDataSet
          /// </param>
          /// <param name="_data">
          ///   see TDataSet
          /// </param>
          procedure GetBookmarkData   (       _buffer       : TRecBuf       ;
                                              _data         : TBookmark
                                      ) ; override;
        {$ELSE}
          /// <summary>
          ///   See TDataSet.
          /// </summary>
          /// <param name="_buffer">
          ///   see TDataSet
          /// </param>
          /// <param name="_data">
          ///   see TDataSet
          /// </param>
          procedure GetBookmarkData   (       _buffer       : TRecordBuffer ;
                                              _data         : TBookmark
                                      ) ; override;
        {$ENDIF}
      {$ENDIF}

      {$IFDEF NEXTGEN}
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <param name="_data">
        ///   see TDataSet
        /// </param>
        procedure GetBookmarkData     (       _buffer       : TRecordBuffer ;
                                              _data         : Pointer
                                      ) ; override;
      {$ENDIF}

      {$IFDEF NEXTGEN}
        /// <summary>
        ///   See TDataSet.
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <param name="_get_mode">
        ///   see TDataSet
        /// </param>
        /// <param name="_do_check">
        ///   see TDataSet
        /// </param>
        /// <returns>
        ///   See TDataSet.
        /// </returns>
        function  GetRecord           (       _buffer       : TRecBuf       ;
                                              _get_mode     : TGetMode      ;
                                              _do_check     : Boolean
                                      ) : TGetResult ; override;
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <param name="_get_mode">
        ///   see TDataSet
        /// </param>
        /// <param name="_do_check">
        ///   see TDataSet
        /// </param>
        /// <returns>
        ///   See TDataSet.
        /// </returns>
        function  GetRecord           (       _buffer       : TRecordBuffer ;
                                              _get_mode     : TGetMode      ;
                                              _do_check     : Boolean
                                      ) : TGetResult ; override;
      {$ENDIF}

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  GetRecordSize         : Word ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  GetRecordCount        : Integer ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  GetRecNo              : Integer ; override;

    private
      procedure doDestroy             ;

    protected

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure InternalOpen          ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure InternalClose         ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure InternalEdit          ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure InternalPost          ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure InternalCancel        ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure InternalFirst         ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure InternalLast          ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure DoBeforeInsert        ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure DoBeforeDelete        ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure DoBeforeRefresh       ; override;

      {$IFDEF LEVEL_XE3_VCL}
        {$IFDEF NEXTGEN}
          /// <summary>
          ///   See TDataSet.
          /// </summary>
          /// <param name="_buffer">
          ///   see TDataSet
          /// </param>
          /// <param name="_append">
          ///   see TDataSet
          /// </param>
          procedure InternalAddRecord (
                                              _buffer       : TRecBuf       ;
                                              _append       : Boolean
                                      ) ; override;
        {$ELSE}
          /// <summary>
          ///   See TDataSet.
          /// </summary>
          /// <param name="_buffer">
          ///   see TDataSet
          /// </param>
          /// <param name="_append">
          ///   see TDataSet
          /// </param>
          procedure InternalAddRecord (
                                              _buffer       : TRecordBuffer ;
                                              _append       : Boolean
                                      ) ; override;
        {$ENDIF}
      {$ENDIF}

      {$IFDEF NEXTGEN}
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <param name="_append">
        ///   see TDataSet
        /// </param>
        procedure InternalAddRecord   (
                                              _buffer       : Pointer       ;
                                              _append       : Boolean
                                      ) ; override;
      {$ENDIF}
      {$IFDEF LEVEL_XE3_VCL}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_bookmark">
        ///   see TDataSet
        /// </param>
        procedure InternalGotoBookmark(
                                              _bookmark     : TBookmark
                                      ) ; override;
      {$ENDIF}

      {$IFDEF NEXTGEN}
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_bookmark">
        ///   see TDataSet
        /// </param>
        procedure InternalGotoBookmark(
                                              _bookmark     : Pointer
                                      ) ; override;
      {$ENDIF}

      {$IFDEF NEXTGEN}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        procedure InternalSetToRecord (       _buffer       : TRecBuf
                                      ) ; override;
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        procedure InternalSetToRecord (       _buffer       : TRecordBuffer
                                      ) ; override;
      {$ENDIF}

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure InternalInitFieldDefs ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      procedure InternalHandleException
                                      ; override;

      {$IFDEF NEXTGEN}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <returns>
        ///   See TDataSet.
        /// </returns>
        function  GetBookmarkFlag     (       _buffer       : TRecBuf
                                      ) : TBookmarkFlag ; override;
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <returns>
        ///   See TDataSet.
        /// </returns>
        function  GetBookmarkFlag     (       _buffer       : TRecordBuffer
                                      ) : TBookmarkFlag ; override;
      {$ENDIF}

      {$IFDEF LEVEL_XE3_RTL}
        {$IFDEF NEXTGEN}
          /// <summary>
          ///   See TDataSet.
          /// </summary>
          /// <param name="_buffer">
          ///   see TDataSet
          /// </param>
          /// <param name="_data">
          ///   see TDataSet
          /// </param>
          procedure SetBookmarkData   (       _buffer       : TRecBuf       ;
                                              _data         : TBookmark
                                      ) ; override  ;
        {$ELSE}
          /// <summary>
          ///   See TDataSet.
          /// </summary>
          /// <param name="_buffer">
          ///   see TDataSet
          /// </param>
          /// <param name="_data">
          ///   see TDataSet
          /// </param>
          procedure SetBookmarkData   (       _buffer       : TRecordBuffer ;
                                              _data         : TBookmark
                                      ) ; override  ;
        {$ENDIF}
      {$ENDIF}

      {$IFDEF NEXTGEN}
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <param name="_data">
        ///   see TDataSet
        /// </param>
        procedure SetBookmarkData     (       _buffer       : TRecordBuffer ;
                                              _data         : Pointer
                                      ) ; override  ;
      {$ENDIF}

      {$IFDEF NEXTGEN}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <param name="_value">
        ///   see TDataSet
        /// </param>
        procedure SetBookmarkFlag     (       _buffer       : TRecBuf       ;
                                              _value        : TBookmarkFlag
                                      ) ; override;
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <param name="_value">
        ///   see TDataSet
        /// </param>
        procedure SetBookmarkFlag     (       _buffer       : TRecordBuffer ;
                                              _value        : TBookmarkFlag
                                      ) ; override;
      {$ENDIF}

      {$IFDEF LEVEL_XE3_VCL}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_fields">
        ///   see TDataSet
        /// </param>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        procedure SetFieldData        (       _field        : TField        ;
                                              _buffer       : TValueBuffer
                                      ) ; override;
      {$ENDIF}

      {$IFDEF NEXTGEN}
      {$ELSE}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_field">
        ///   see TDataSet
        /// </param>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        procedure SetFieldData        (       _field        : TField        ;
                                              _buffer       : Pointer
                                      ) ; override;
      {$ENDIF}

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <param name="_value">
      ///   see TDataSet
      /// </param>
      procedure SetFilterText         ( const _value        : String
                                      ) ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <param name="_value">
      ///   see TDataSet
      /// </param>
      procedure SetFiltered           (       _value        : Boolean
                                      ) ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <param name="_value">
      ///   see TDataSet
      /// </param>
      procedure SetRecNo              (       _value        : Integer
                                      ) ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  GetCanModify          : Boolean ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <param name="_info_query">
      ///   see TDataSet
      /// </param>
      procedure OpenCursor            (       _info_query   : Boolean = False
                                      ) ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  IsCursorOpen          : Boolean ; override;

    protected // dataset introduced virtual methods

      /// <summary>
      ///   Perform internal part of Locate &amp; Lookup methods. See
      ///   TDataSet.Locate for more details.
      /// </summary>
      /// <param name="_keyFields">
      ///   list of fields separated by ';'
      /// </param>
      /// <param name="_keyValues">
      ///   list of values corresponding to _keyFields separated by ';' (see
      ///   TDataSet.Locate)
      /// </param>
      /// <param name="_options">
      ///   locate options
      /// </param>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  LocateRecord          ( const _keyFields    : String         ;
                                        const _keyValues    : Variant        ;
                                        const _options      : TLocateOptions
                                      ) : TGIS_Shape ; virtual;

    public
      {$IFDEF LEVEL_XE3_VCL}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_field">
        ///   see TDataSet
        /// </param>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <returns>
        ///   See TDataSet.
        /// </returns>
        function  GetFieldData        (       _field        : TField        ;
                                              {$IFDEF LEVEL_XE4_VCL}
                                                var _buffer : TValueBuffer
                                              {$ELSE}
                                                _buffer     : TValueBuffer
                                              {$ENDIF}
                                      ) : Boolean ; override;
      {$ENDIF}

      {$IFNDEF NEXTGEN}
        /// <summary>
        ///   See TDataSet.
        /// </summary>
        /// <param name="_field">
        ///   see TDataSet
        /// </param>
        /// <param name="_buffer">
        ///   see TDataSet
        /// </param>
        /// <returns>
        ///   See TDataSet.
        /// </returns>
        function  GetFieldData        (       _field        : TField        ;
                                              _buffer       : Pointer
                                      ) : Boolean ; override;
      {$ENDIF}

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <param name="_bookmark">
      ///   see TDataSet
      /// </param>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  BookmarkValid         (       _bookmark     : TBookmark
                                      ) : Boolean ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <param name="_bookmark1">
      ///   see TDataSet
      /// </param>
      /// <param name="_bookmark2">
      ///   see TDataSet
      /// </param>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  CompareBookmarks      (       _bookmark1    : TBookmark     ;
                                              _bookmark2    : TBookmark
                                      ) : Integer ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <param name="_keyFields">
      ///   see TDataSet
      /// </param>
      /// <param name="_keyValues">
      ///   see TDataSet
      /// </param>
      /// <param name="_options">
      ///   see TDataSet
      /// </param>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  Locate                ( const _keyFields    : String        ;
                                        const _keyValues    : Variant       ;
                                              _options      : TLocateOptions
                                      ) : Boolean ; override;

      /// <summary>
      ///   See TDataSet.
      /// </summary>
      /// <param name="_keyFields">
      ///   see TDataSet
      /// </param>
      /// <param name="_keyValues">
      ///   see TDataSet
      /// </param>
      /// <param name="_resultFields">
      ///   see TDataSet
      /// </param>
      /// <returns>
      ///   See TDataSet.
      /// </returns>
      function  Lookup                ( const _keyFields    : String        ;
                                        const _keyValues    : Variant       ;
                                        const _resultFields : String
                                      ) : Variant ; override;

    public

      /// <summary>
      ///   Create dataset.
      /// </summary>
      /// <param name="_owner">
      ///   component owner
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create              (       _owner        : TComponent
                                      ) ; override;

      /// <summary>
      ///   Destroy dataset.
      /// </summary>
      destructor  Destroy               ; override;

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;
    public

      /// <summary>
      ///   Open the database which meets extent and query. Will use RTree if
      ///   available.
      /// </summary>
      /// <param name="_layer">
      ///   source layer for the dataset
      /// </param>
      /// <param name="_extent">
      ///   extent of items to be found; expected _extent units are in a
      ///   Layer coordinate space
      /// </param>
      /// <param name="_query">
      ///   query which must be matched by item; closely mimics SQL WHERE
      ///   clause; for examples you can use 'AGE &gt;= 18'; empty (default)
      ///   means that no items will match.
      /// </param>
      /// <param name="_shape">
      ///   if not nil, then only shapes matched dm9 matrix with _shape will
      ///   be found
      /// </param>
      /// <param name="_de9im">
      ///   DE-9IM matrix of comparison
      /// </param>
      /// <param name="_skipDeleted">
      ///   set a skip deleted treatment; by default deleted items will be
      ///   ignored
      /// </param>
      /// <param name="_maxRecords">
      ///   maximum number of records to be returned; default is -1 which
      ///   means no limit; 0 means that only structure  will be filled
      /// </param>
      /// <remarks>
      ///   Use this method to fill the dataset with all shapes in the layer
      ///   matching given criteria. Using _query param we can narrow the
      ///   result of the shape searching down to the minimum. This should
      ///   speed up our process of locating a desirable shape.
      /// </remarks>
      procedure Open                  ( const _layer        : TGIS_LayerVector ;
                                        const _extent       : TGIS_Extent      ;
                                        const _query        : String           ;
                                        const _shape        : TGIS_Shape = nil ;
                                        const _de9im        : String = ''      ;
                                        const _skipDeleted  : Boolean = True   ;
                                        const _maxRecords   : Integer = -1
                                      ) ; overload; virtual;

      /// <summary>
      ///   Open the database which meets extent and query. Will use RTree if
      ///   available.
      /// </summary>
      /// <param name="_layer">
      ///   source layer for the dataset
      /// </param>
      /// <param name="_extent">
      ///   extent of items to be found; expected _extent units are in a
      ///   Layer coordinate space
      /// </param>
      procedure Open                  ( const _layer        : TGIS_LayerVector ;
                                        const _extent       : TGIS_Extent
                                      ) ; overload; virtual;

      {$IFDEF GIS_PDK}
        {#gendoc:hide}
        procedure OpenInternalLayer   ( const _layer        : TGIS_LayerVector
                                      ) ;
      {$ENDIF}


      /// <summary>
      ///   Go to record and make it active based on shape UID.
      /// </summary>
      /// <param name="_uid">
      ///   uid of shape to be found
      /// </param>
      /// <returns>
      ///   True if record found.
      /// </returns>
      function  GotoShape             ( const _uid          : TGIS_Uid
                                      ) : Boolean ; virtual;
    public
      /// <summary>
      ///   Current shape of the dataset (shape based on ActiveRecord) or
      ///   nil.
      /// </summary>
      property ActiveShape  : TGIS_Shape read fget_ActiveShape ;

      /// <summary>
      ///   Show or hide internal layer fields.
      /// </summary>
      /// <remarks>
      ///   <note type="important">
      ///     This method is deprecated. Use ShowVirtualFields.
      ///    </note>
      /// </remarks>
      property ShowInternalFields : Boolean read  FShowVirtualFields
                                            write FShowVirtualFields ;

      /// <summary>
      ///   Show or hide virtual layer fields.
      /// </summary>
      property ShowVirtualFields : Boolean  read  FShowVirtualFields
                                            write FShowVirtualFields ;

      /// <summary>
      ///   The set of virtual fields that are displayed when
      ///   ShowVirtualFields property is active.
      /// </summary>
      property VirtualFields : TGIS_VirtualFields read  FVirtualFields
                                                  write FVirtualFields ;

      /// <summary>
      ///   If true, displayed fields will be read only.
      /// </summary>
      property &ReadOnly : Boolean read  bReadOnly
                                   write bReadOnly ;

      /// <summary>
      ///   Current layer of the dataset.
      /// </summary>
      property Layer : TGIS_LayerVector read dsLayer ;

    published

      /// <event/>
      /// <summary>
      ///   OnBusy event. Will be fired regularly during long-drawn
      ///   operations. If end value will be zero, the meaning is:
      ///   long-drawn with unknown end time. Close long-drawn operation by
      ///   calling with parameters (-1,-1).
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    If not assigned then TGIS_Viewer.BusyEvent will be used.
      ///    </note>
      /// </remarks>
      property OnBusy : TGIS_BusyEvent read FOnBusy write FOnBusy ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property Active ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property BeforeOpen ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property AfterOpen ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property BeforeClose ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property AfterClose ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property BeforeEdit ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property AfterEdit ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property BeforePost ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property AfterPost ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property BeforeCancel ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property AfterCancel ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property BeforeScroll ;

      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property AfterScroll ;

      /// <event/>
      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property OnCalcFields ;

      /// <event/>
      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property OnDeleteError ;

      /// <event/>
      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property OnEditError ;

      /// <event/>
      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property OnFilterRecord ;

      /// <event/>
      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property OnNewRecord ;

      /// <event/>
      /// <summary>
      ///   See: TDataSet
      /// </summary>
      property OnPostError ;
  end ;

//##############################################################################
implementation

{$R GisDataSet_16x16.RES}

uses
  {$IFNDEF GIS_NOADO_JOIN}
    {$IFNDEF ADOINTUNIT}
      GisAdoInt,
    {$ENDIF}
  {$ENDIF}
  GisInternals,
  GisClasses,
  Math ;

type

  TRecInfo = record
    RecordID     : Integer       ;
    Bookmark     : Pointer       ;
    BookmarkFlag : TBookmarkFlag ;
  end ;
  PRecInfo = ^TRecInfo;

  T_dsRecord = class
    public
      Uid : TGIS_Uid;
      constructor Create( const _uid : TGIS_Uid) ;
  end ;

//==============================================================================
// TGIS_DataSet
//==============================================================================

  constructor TGIS_DataSet.Create( _owner: TComponent ) ;
  begin
    inherited;
    FShowVirtualFields := True ;
    FVirtualFields := [ TGIS_VirtualField.GisUid,
                        TGIS_VirtualField.GisSelected,
                        TGIS_VirtualField.GisHidden,
                        TGIS_VirtualField.GisArea,
                        TGIS_VirtualField.GisLength ];
    lstRecords := TGIS_ObjectList.Create( True ) ;
    uponDestroy := False ;
    uponClose := False ;
    bReadOnly := False ;
    oViewer := nil ;
    {$IFDEF GIS_PDK}
      oInternalLayer := nil ;
    {$ENDIF}
  end ;

  destructor TGIS_DataSet.Destroy;
  begin
    if not uponDestroy then
      doDestroy ;
    inherited;
  end ;

  procedure TGIS_DataSet.doDestroy;
  begin
    uponDestroy := True ;
    uponClose   := True ;

    if assigned( oViewer ) then
      oViewer.UnSubscribe( Self ) ;

    Close ;
    FreeObject( lstRecords ) ;
  end ;

  procedure TGIS_DataSet.doBusyPrepare ;
  var
    isabort : Boolean ;
  begin
    isBusy  := True  ;

    isabort := False ;

    if      Assigned( FOnBusy ) then begin
              // isabort will be ignored
              OnBusy( self, 0, 100, isabort )
            end
    else if Assigned( dsLayer ) then begin
              dsLayer.RaiseBusyPrepare( self, _rsrc( GIS_RS_BUSY_DATA_LOAD ) ) ;
            end ;
  end ;

  procedure TGIS_DataSet.doBusyRelease ;
  var
    isabort : Boolean ;
  begin
    isBusy := False ;

    isabort := False ;

    if      Assigned( FOnBusy ) then begin
              // isabort will be ignored
              OnBusy( self, -1, -1, isabort )
            end
    else if Assigned( dsLayer ) then begin
              dsLayer.RaiseBusyRelease( self ) ;
            end ;
  end ;


  function TGIS_DataSet.doBusyShake(
    const _pos : Integer ;
    const _end : Integer
  ) : Boolean ;
  var
    isabort : Boolean ;
  begin
    Result := False ;

    if not isBusy then exit ;

    isabort := False ;

    if      Assigned( FOnBusy ) then begin
              OnBusy( self, _pos, _end, isabort )
            end
    else if Assigned( dsLayer ) then begin
              if Assigned( dsLayer.Viewer ) then
                if dsLayer.Viewer.Ref.IsBusy then
                  isabort := dsLayer.RaiseBusyShake( self, _pos, _end )
            end ;

    Result := isabort ;
  end;

  function  TGIS_DataSet.getUid : TGIS_Uid ;
  var
    rec : T_dsRecord ;
    r   : TRecInfo   ;
  begin
    Result := 0 ;

    if not Assigned( dsLayer ) then exit ;

    if State = dsFilter then
      r := PRecInfo(TempBuffer)^
    else
      r := PRecInfo(ActiveBuffer)^ ;

    if r.BookmarkFlag = bfEOF then exit ;
    rec := lstRecords[r.RecordID] as T_dsRecord ;

    Result:= rec.Uid ;
  end ;


  function  TGIS_DataSet.fget_ActiveShape : TGIS_Shape ;
  var
    uid : TGIS_Uid ;
  begin
    Result := nil ;

    if not Assigned( dsLayer ) then exit ;

    uid := getUid ;

    if ( not Assigned( currShape ) ) or ( currShape.Uid <> uid ) then
      // take it only if was changed
      currShape := dsLayer.GetShape( uid ) ;

    Result := currShape ;
  end ;

  {$IFDEF NEXTGEN}
    function TGIS_DataSet.AllocRecBuf
      : TRecBuf ;
    begin
      Result := TRecBuf( GetMemory( SizeOf( TRecInfo ) ) ) ;
    end ;
  {$ELSE}
    function TGIS_DataSet.AllocRecordBuffer
      : TRecordBuffer ;
    begin
      Result := GetMemory( SizeOf( TRecInfo ) ) ;
    end ;
  {$ENDIF}

  {$IFDEF NEXTGEN}
    procedure TGIS_DataSet.FreeRecBuf(
      var _buffer : TRecBuf
    ) ;
    begin
      FreeMemory( Pointer(_buffer) ) ;
    end ;
  {$ELSE}
    procedure TGIS_DataSet.FreeRecordBuffer(
      var _buffer : TRecordBuffer
    ) ;
    begin
      FreeMemory( _buffer ) ;
    end ;
  {$ENDIF}

  {$IFDEF NEXTGEN}
    function TGIS_DataSet.GetBookmarkFlag(
      _buffer : TRecBuf
    ) : TBookmarkFlag ;
    var
      r : TRecInfo ;
    begin
      r := PRecInfo( _buffer )^ ;
      Result := r.BookmarkFlag;
    end ;
  {$ELSE}
    function TGIS_DataSet.GetBookmarkFlag(
      _buffer : TRecordBuffer
    ) : TBookmarkFlag ;
    var
      r : TRecInfo ;
    begin
      r := PRecInfo( _buffer )^ ;
      Result := r.BookmarkFlag;
    end ;
  {$ENDIF}

  {$IFDEF NEXTGEN}
    function TGIS_DataSet.GetRecord(
      _buffer   : TRecBuf   ;
      _get_mode : TGetMode  ;
      _do_check : Boolean
    ) : TGetResult ;
  {$ELSE}
    function TGIS_DataSet.GetRecord(
      _buffer   : TRecordBuffer ;
      _get_mode : TGetMode      ;
      _do_check : Boolean
    ) : TGetResult ;
  {$ENDIF}
  var
    r       : TRecInfo ;
    baccept : Boolean ;
    oldstate: TDataSetState;
  begin
    baccept := True ;

    repeat
      if not IsCursorOpen then begin
        Result := grBOF ;
        exit ;
      end ;

      Result := grOK; // default

      case _get_mode of
        gmNext: // move on
          begin
            if currRecord mod 500 = 0 then
              if doBusyShake( currRecord, lstRecords.Count - 1 ) then
                Result := grEof ;
            if Result = grOK then begin
              if currRecord < lstRecords.Count - 1 then
                Inc( currRecord )
              else
                Result := grEOF ; // end of file
            end ;
          end ;
        gmPrior: // move back
          if currRecord > 0 then
            Dec( currRecord )
          else
            Result := grBOF ; // begin of file
        gmCurrent: // check if empty
          if currRecord >= lstRecords.Count then
            Result := grEOF ;
      end ;

      if Result = grOK then begin // read the data

        if assigned( OnFilterRecord ) then begin
          oldstate := SetTempState( dsFilter ) ;
          try
            r := PRecInfo(TempBuffer)^ ;
            with r do begin
              RecordID     := currRecord ;
              BookmarkFlag := bfCurrent ;
              Bookmark     := Pointer( RecordID ) ;
            end ;
            PRecInfo(TempBuffer)^ := r ;

            OnFilterRecord( Self, baccept ) ;
          finally
            RestoreState( oldstate ) ;
          end;
        end ;

        if baccept then begin
          r := PRecInfo(_buffer)^ ;
          with r do begin
            RecordID     := currRecord ;
            BookmarkFlag := bfCurrent ;
            Bookmark     := Pointer( RecordID ) ;
          end ;
          PRecInfo(_buffer)^ := r ;
        end ;

      end;

    until baccept or (Result in [grEOF, grBOF]) ;

  end ;

  function TGIS_DataSet.GetRecordSize: Word;
  begin
    if IsCursorOpen then
      Result := 4 // actual data without house-keeping
    else
      Result := inherited GetRecordSize ;
  end ;

  function TGIS_DataSet.GetRecordCount: Integer;
  begin
    if IsCursorOpen then
      Result := lstRecords.Count
    else
      Result := inherited GetRecordCount ;
  end ;

  function TGIS_DataSet.GetRecNo: Integer;
  begin
    CheckActive;
    UpdateCursorPos ;

    if IsCursorOpen then
      Result := currRecord + 1
    else
      Result := inherited GetRecNo ;
  end ;

  procedure TGIS_DataSet.DoBeforeInsert;
  begin
    // Inserting records not implemented
    Abort ;
  end ;

  procedure TGIS_DataSet.DoBeforeDelete;
  begin
    // Deleting records not implemented
    Abort ;
  end ;

  procedure TGIS_DataSet.DoBeforeRefresh ;
  begin
    currShape := nil ;
    inherited ;
  end ;

  {$IFDEF LEVEL_XE3_VCL}
    {$IFDEF NEXTGEN}
      procedure TGIS_DataSet.InternalAddRecord(
        _buffer : TRecBuf ;
        _append : Boolean
      ) ;
      begin
        // Adding records not implemented
      end ;
    {$ELSE}
      procedure TGIS_DataSet.InternalAddRecord(
        _buffer : TRecordBuffer ;
        _append : Boolean
      ) ;
      begin
        // Adding records not implemented
      end ;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF NEXTGEN}
  {$ELSE}
    procedure TGIS_DataSet.InternalAddRecord(
      _buffer : Pointer  ;
      _append : Boolean
    ) ;
    begin
      // Adding records not implemented
    end ;
  {$ENDIF}

  procedure TGIS_DataSet.InternalOpen;
  var
    lst_uid   : TGIS_Uid   ;
    shp       : TGIS_Shape ;
    rec       : T_dsRecord ;
    tmp_query : String     ;
    fld       : TField     ;
    p         : Integer    ;
    i         : Integer    ;
    itr       : TGIS_LayerVectorEnumerator ;
  begin
    lst_uid := 0 ;
    uponClose := False ;

    doBusyPrepare ;
    try
      // initialize field definitions and create fields
      InternalInitFieldDefs;
      if DefaultFields then
        CreateFields;
      BindFields( True ) ;

      // fill data set
      lstRecords.Clear;

      if not Assigned( dsLayer ) then exit ;
      if dsMaxRecords = 0 then exit ;
      if GisIsNoWorld( dsExtent )then exit ;

      try
        if not IsStringEmpty( dsQuery ) then begin
          if Filtered then
            tmp_query := Format( '(%s) and (%s)', [dsQuery, Filter] )
          else
            tmp_query := dsQuery ;
        end
        else begin
          if Filtered then
            tmp_query := Filter
          else
            tmp_query := '' ;
        end ;

        itr := TGIS_LayerVectorEnumerator.Create(
                 dsLayer,
                 dsExtent,
                 tmp_query,
                 dsShape,
                 dsDe9im,
                 dsSkipDeleted
              ) ;
        try
          currCursor := itr.Cursor ;
          while itr.MoveNext and
                ( ( dsMaxRecords < 0 ) or
                  ( lstRecords.Count < dsMaxRecords )
                )
          do begin
            shp := itr.GetCurrent ;
            rec := T_dsRecord.Create( shp.Uid ) ;

            lstRecords.Add( rec ) ;

            if rec.Uid mod GIS_PROGRESS_TRESHOLD = 0 then begin
              if lst_uid = 0 then begin
                // do not report full progress
                if doBusyShake( 50, 100 ) then
                  break
              end
              else begin
                // report full progress
                if doBusyShake( rec.Uid, lst_uid ) then
                  break ;
              end ;
            end ;
          end ;
        finally
          FreeObject( itr ) ;
        end;
      finally
        // obsolete
      end ;

      // initialize
      currRecord     := -1 ;
      BookmarkSize := Sizeof( Integer ) ;

      // prepare shape for cancel operations
      oldLayer := TGIS_LayerVector.Create ;
      oldLayer.Open ;
      oldLayer.ImportStructure( dsLayer );
      oldShape := oldLayer.CreateShape( TGIS_ShapeType.Point ) ;

    finally
      doBusyRelease ;
    end ;
  end ;

  procedure TGIS_DataSet.InternalClose;
  begin
    // disconnect and destroy field objects
    BindFields( False ) ;
    if DefaultFields then
      DestroyFields ;
    // closed
    dsLayer    := nil ;
    currShape  := nil ;
    currRecord := -1  ;
    lstRecords.Clear ;

    FreeObject( oldLayer ) ;
    {$IFDEF GIS_PDK}
      FreeObject( oInternalLayer ) ;
    {$ENDIF}
  end ;

  procedure TGIS_DataSet.InternalEdit;
  var
    shp : TGIS_Shape ;
  begin
    if uponDestroy then exit ;
    if uponClose then exit ;

    shp := dsLayer.GetShape( getUid ) ;
    if not assigned( shp ) then exit ;

    currShape := shp.MakeEditable ;
    oldShape.CopyFields( currShape );
    oldShape.IsSelected := currShape.IsSelected ;
  end ;

  procedure TGIS_DataSet.InternalPost;
  begin
    // do nothing
  end ;

  procedure TGIS_DataSet.InternalCancel;
  var
    shp : TGIS_Shape ;
  begin
    if uponDestroy then exit ;
    if uponClose then exit ;

    shp := dsLayer.GetShape( getUid ) ;
    if not assigned( shp ) then exit ;

    currShape := shp.MakeEditable ;
    currShape.CopyFields( oldShape ) ;
    currShape.IsSelected := oldShape.IsSelected ;
  end ;

  procedure TGIS_DataSet.InternalFirst;
  begin
    currRecord := -1;
  end ;

  procedure TGIS_DataSet.InternalLast;
  begin
    currRecord := lstRecords.Count ;
  end ;

  {$IFDEF LEVEL_XE3_VCL}
    procedure TGIS_DataSet.InternalGotoBookmark(
      _bookmark : TBookmark
    ) ;
    begin
      currRecord := PInteger( @_bookmark[0] )^ ;
    end;
  {$ENDIF}

  {$IFNDEF NEXTGEN}
    procedure TGIS_DataSet.InternalGotoBookmark(
      _bookmark : Pointer
    ) ;
    begin
      currRecord := PInteger(_bookmark)^ ;
    end ;
  {$ENDIF}

  {$IFDEF NEXTGEN}
    procedure TGIS_DataSet.InternalSetToRecord(
      _buffer : TRecBuf
    ) ;
    var
      r : TRecInfo ;
    begin
      r := PRecInfo( _buffer )^ ;

      currRecord := r.RecordID ;
    end ;
  {$ELSE}
    procedure TGIS_DataSet.InternalSetToRecord(
      _buffer : TRecordBuffer
    ) ;
    var
      r : TRecInfo ;
    begin
      r := PRecInfo( _buffer )^ ;

      currRecord := r.RecordID ;
    end ;
  {$ENDIF}

  procedure TGIS_DataSet.InternalInitFieldDefs;
  var
    wasjoin : Boolean        ;
    i       : Integer        ;
    fld     : TGIS_FieldInfo ;
    fld_def : TFieldDef      ;
    flt     : TFieldType     ;
    siz     : Integer        ;
    rule    : TGIS_FieldRule ;
    vFields : TGIS_VirtualFields ;

    function is_ro_predefined( const _name : String ) : Boolean ;
    begin
      if _name = GIS_FIELD_SELECTED then
        Result := False
      else
      if _name = GIS_FIELD_HIDDEN then
        Result := False
      else
        Result := Pos( _name, GIS_FIELDS_PREDEFINED ) >= StringFirst ;
    end ;

  begin
    FieldDefs.Clear ;

    if not Assigned( dsLayer ) then exit ;

    vFields := VirtualFields ;

    if ShowVirtualFields then begin
      if TGIS_VirtualField.GisUid         in vFields then FieldDefs.Add( GIS_FIELD_UID,          Data.DB.ftLargeint, 0, True ) ;
      if TGIS_VirtualField.GisSelected    in vFields then FieldDefs.Add( GIS_FIELD_SELECTED,     Data.DB.ftBoolean,  0, True ) ;
      if TGIS_VirtualField.GisHidden      in vFields then FieldDefs.Add( GIS_FIELD_HIDDEN,       Data.DB.ftBoolean,  0, True ) ;
      if TGIS_VirtualField.GisArea        in vFields then FieldDefs.Add( GIS_FIELD_AREA,         Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisLength      in vFields then FieldDefs.Add( GIS_FIELD_LENGTH,       Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisCoordZ      in vFields then FieldDefs.Add( GIS_FIELD_COORD_Z,      Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisCoordM      in vFields then FieldDefs.Add( GIS_FIELD_COORD_M,      Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisNow         in vFields then FieldDefs.Add( GIS_FIELD_NOW,          Data.DB.ftDateTime, 0, True ) ;
      if TGIS_VirtualField.GisMinX        in vFields then FieldDefs.Add( GIS_FIELD_MIN_X,        Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisMinY        in vFields then FieldDefs.Add( GIS_FIELD_MIN_Y,        Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisMinZ        in vFields then FieldDefs.Add( GIS_FIELD_MIN_Z,        Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisMinM        in vFields then FieldDefs.Add( GIS_FIELD_MIN_M,        Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisMaxX        in vFields then FieldDefs.Add( GIS_FIELD_MAX_X,        Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisMaxY        in vFields then FieldDefs.Add( GIS_FIELD_MAX_Y,        Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisMaxZ        in vFields then FieldDefs.Add( GIS_FIELD_MAX_Z,        Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisMaxM        in vFields then FieldDefs.Add( GIS_FIELD_MAX_M,        Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisCenterX     in vFields then FieldDefs.Add( GIS_FIELD_CENTER_X,     Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisCenterY     in vFields then FieldDefs.Add( GIS_FIELD_CENTER_Y,     Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisCenterZ     in vFields then FieldDefs.Add( GIS_FIELD_CENTER_Z,     Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisCenterM     in vFields then FieldDefs.Add( GIS_FIELD_CENTER_M,     Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GixCentroidX   in vFields then FieldDefs.Add( GIS_FIELD_CENTROID_X,   Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GixCentroidY   in vFields then FieldDefs.Add( GIS_FIELD_CENTROID_Y,   Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisNumPoints   in vFields then FieldDefs.Add( GIS_FIELD_NUM_POINTS,   Data.DB.ftLargeint, 0, True ) ;
      if TGIS_VirtualField.GisNumParts    in vFields then FieldDefs.Add( GIS_FIELD_NUM_PARTS,    Data.DB.ftLargeint, 0, True ) ;
      if TGIS_VirtualField.GisShapeType   in vFields then FieldDefs.Add( GIS_FIELD_SHAPE_TYPE,   Data.DB.ftString,   0, True ) ;
      if TGIS_VirtualField.GisViewerScale in vFields then FieldDefs.Add( GIS_FIELD_VIEWER_SCALE, Data.DB.ftFloat,    0, True ) ;
      if TGIS_VirtualField.GisViewerLevel in vFields then FieldDefs.Add( GIS_FIELD_VIEWER_LEVEL, Data.DB.ftFloat,    0, True ) ;
    end ;
    for i:=0 to FieldDefs.Count - 1 do begin
      fld_def := FieldDefs[i] ;
      if is_ro_predefined( fld_def.Name ) then
        fld_def.Attributes := fld_def.Attributes + [faReadOnly]
    end;

    for i:=0 to dsLayer.Fields.Count - 1 do begin
      fld := dsLayer.FieldInfo( i ) ;
      if not (TGIS_FieldFlags.Visible in fld.Flags) then continue ;

      rule := TGIS_FieldRule(fld.Rules) ;
      if assigned( rule ) and (rule.ValueAliases.Aliases.Count > 0 ) then
        FieldDefs.Add( fld.NewName, Data.DB.ftWideString, fld.NewWidth, True )
      else begin
        case fld.FieldType of
          TGIS_FieldType.String :
            begin
              FieldDefs.Add( fld.NewName, Data.DB.ftWideString, fld.NewWidth, True ) ;
            end ;
          TGIS_FieldType.Number :
            begin
              if fld.Decimal = 0 then
                FieldDefs.Add( fld.NewName, Data.DB.ftLargeint, 0, True )
              else
                FieldDefs.Add( fld.NewName, Data.DB.ftFloat, 0, True )
            end ;
          TGIS_FieldType.Float :
            FieldDefs.Add( fld.NewName, Data.DB.ftFloat, 0, True ) ;
          TGIS_FieldType.Boolean :
            FieldDefs.Add( fld.NewName, Data.DB.ftBoolean , 0, True ) ;
          TGIS_FieldType.Date :
            FieldDefs.Add( fld.NewName, Data.DB.ftDateTime, 0, True ) ;
          else
            begin
              Assert( False,
                      Format( '%s %s %d', [ _rsrc( GIS_RS_ERR_UNTESTED ),
                                            'InternalInitFieldDefs',
                                            IntToStr( Ord( fld.FieldType ) )
                                          ]
                            )
                    ) ;
            end ;
        end ;
      end ;

      fld_def := FieldDefList.Find( fld.NewName ) ;

      if Assigned( fld_def ) then begin
        if fld.ReadOnly then
          fld_def.Attributes := fld_def.Attributes + [faReadOnly ];
        if fld.Hidden then
          fld_def.Attributes := fld_def.Attributes + [faHiddenCol ];
      end;
    end ;

    wasjoin := False ;
    {$IFNDEF GIS_NODB}
      if ( not wasjoin ) and Assigned( dsLayer.JoinDB ) then begin
        wasjoin := True ;
        for i:=0 to dsLayer.JoinDB.FieldCount-1 do begin
          JoinFieldInfo( dsLayer.JoinDB.Fields[i], flt, siz ) ;

          if flt <> ftUnknown then
            FieldDefs.Add(
              ToJoinFieldName( dsLayer.JoinDB.Fields[i].DisplayName ),
              flt, siz, True
            ) ;
        end ;
      end ;
    {$ENDIF}

    {$IFNDEF GIS_NOADO_JOIN}
      if ( not wasjoin ) and Assigned( dsLayer.JoinADO ) then begin
        for i:=0 to dsLayer.JoinADO.Fields.Count-1 do begin
          JoinFieldInfo( dsLayer.JoinADO.Fields.Item[i], flt, siz ) ;
          {$IFNDEF GIS_NODB}
          if flt <> ftUnknown then
            FieldDefs.Add(
              ToJoinFieldName( dsLayer.JoinADO.Fields.Item[i].Name ),
              flt, siz, True
            ) ;
          {$ENDIF}
        end ;
      end ;
    {$ENDIF}
  end ;

  procedure TGIS_DataSet.InternalHandleException;
  begin
   if Assigned(System.Classes.ApplicationHandleException) then
     ApplicationHandleException(Self);
  end ;

  {$IFDEF LEVEL_XE3_VCL}
    {$IFDEF NEXTGEN}
      procedure TGIS_DataSet.GetBookmarkData(
        _buffer  : TRecBuf ;
        _data    : TBookmark
      ) ;
      begin
        getBookmarkDataInternal( _buffer, @_data[0] );
      end;
    {$ELSE}
      procedure TGIS_DataSet.GetBookmarkData(
        _buffer  : TRecordBuffer ;
        _data    : TBookmark
      ) ;
      begin
        getBookmarkDataInternal( _buffer, @_data[0] );
      end;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF NEXTGEN}
  {$ELSE}
    procedure TGIS_DataSet.GetBookmarkData(
      _buffer  : TRecordBuffer ;
      _data    : Pointer
    ) ;
    begin
      getBookmarkDataInternal( _buffer, _data ) ;
    end ;
  {$ENDIF}

  procedure TGIS_DataSet.getBookmarkDataInternal(
    _buffer  : {$IFDEF NEXTGEN}
                 TRecBuf     ;
               {$ELSE}
                 TRecordBuffer ;
               {$ENDIF}
    _data    : Pointer
  ) ;
  var
    r : TRecInfo ;
  begin
    r := PRecInfo(_buffer)^ ;
    PPointer( _data )^ := r.Bookmark ;
  end ;

  {$IFDEF LEVEL_XE3_VCL}
    {$IFDEF NEXTGEN}
      procedure TGIS_DataSet.SetBookmarkData(
        _buffer : TRecBuf ;
        _data   : TBookmark
      ) ;
    {$ELSE}
      procedure TGIS_DataSet.SetBookmarkData(
        _buffer : TRecordBuffer ;
        _data   : TBookmark
      ) ;
    {$ENDIF}
    begin
      setBookmarkDataInternal( _buffer, @_data[0] );
    end;
  {$ENDIF}

  {$IFNDEF NEXTGEN}
    procedure TGIS_DataSet.SetBookmarkData(
      _buffer : TRecordBuffer ;
      _data   : Pointer
    ) ;
    begin
      setBookmarkDataInternal( _buffer, _data );
    end;
  {$ENDIF}

  procedure TGIS_DataSet.setBookmarkDataInternal(
    _buffer : {$IFDEF NEXTGEN}
                TRecBuf       ;
              {$ELSE}
                TRecordBuffer ;
              {$ENDIF}
    _data   : Pointer
  ) ;
  var
    r : TRecInfo ;
  begin
    r := PRecInfo(_buffer)^ ;

    r.Bookmark := _data ;

    PRecInfo(_buffer)^ := r  ;
  end ;

  {$IFDEF NEXTGEN}
    procedure TGIS_DataSet.SetBookmarkFlag(
      _buffer : TRecBuf ;
      _value  : TBookmarkFlag
    ) ;
    var
      r : TRecInfo ;
    begin
      r := PRecInfo(_buffer)^ ;

      r.BookmarkFlag := _value;

      PRecInfo(_buffer)^ := r ;
    end ;
  {$ELSE}
    procedure TGIS_DataSet.SetBookmarkFlag(
      _buffer : TRecordBuffer ;
      _value  : TBookmarkFlag
    ) ;
    var
      r : TRecInfo ;
    begin
      r := PRecInfo(_buffer)^ ;

      r.BookmarkFlag := _value;

      PRecInfo(_buffer)^ := r ;
    end ;
  {$ENDIF}

  procedure TGIS_DataSet.SetFilterText(
    const _value : String
  ) ;
  var
    ls : TGIS_Layervector ;
  begin
    if _value = Filter then exit ;

    inherited SetFilterText( _value ) ;

    if Filtered then begin
      ls := dsLayer ;
      Close ;
      dsLayer := ls ;

      inherited Open ;
    end ;
  end ;

  procedure TGIS_DataSet.SetFiltered(
    _value : Boolean
  ) ;
  var
    ls : TGIS_Layervector ;
  begin
    if _value = Filtered then exit ;

    inherited SetFiltered( _value ) ;

    ls := dsLayer ;
    Close ;
    dsLayer := ls ;

    inherited Open ;
  end ;

  procedure TGIS_DataSet.SetRecNo(
    _value: Integer
  ) ;
  begin
    if lstRecords.Count <= 0 then
      exit ;

    if  ( _value <= 0 ) then
      _value := 1 ;

    if _value > lstRecords.Count then
      _value := lstRecords.Count ;

    currRecord := _value - 1;

    Resync([]);
  end ;

  function TGIS_DataSet.GetCanModify: Boolean;
  begin
    Result := True; // read/write
  end ;

  procedure TGIS_DataSet.OpenCursor(
    _info_query : Boolean = False
  ) ;
  begin
    inherited ;
    if not Assigned( dsLayer ) then CloseCursor ;
  end ;

  function TGIS_DataSet.IsCursorOpen : Boolean ;
  begin
    Result := Assigned( dsLayer ) ;
  end ;

  function TGIS_DataSet.LocateRecord(
    const _keyFields   : String         ;
    const _keyValues   : Variant        ;
    const _options     : TLocateOptions
  ) : TGIS_Shape ;

    // check value identity
    function is_same_value(
      _v1            : Variant ;
      _v2            : Variant ;
      _isstring      : Boolean ;
      _isinsensitive : Boolean ;
      _ispartial     : Boolean
    ) : Boolean ;
    var
      v : Variant ;
    begin
      if not _isstring then
        Result := VarCompareValue( _v1, _v2 ) = vrEqual
      else begin
        if _ispartial     then v := Copy( _v1, StringFirst, Length( _v2 ) )
                          else v := _v1 ;
        if _isinsensitive then Result := LowerCase( v ) = LowerCase( _v2 )
                          else Result := v = _v2;
      end ;
    end ;

    // verify if value matches
    function check_values(
      _shp           : TGIS_Shape ;
      _fields        : TStrings   ;
      _values        : Variant    ;
      _isinsensitive : Boolean    ;
      _ispartial     : Boolean
    ) : Boolean ;
    var
      i      : Integer ;
      fld    : Variant ;
      is_str : Boolean ;
    begin
      Result := False ;
      if not Assigned( _shp ) then
        exit ;

      Result := True;
      for i := 0 to _fields.Count -1 do begin
        fld := _shp.GetField( _fields[i] ) ;

        case VarType( fld ) of
          {$IFDEF DCC}
            varUString : is_str := True  ;
          {$ENDIF}
          varString    : is_str := True  ;
          varOleStr    : is_str := True  ;
          varStrArg    : is_str := True  ;
          else           is_str := False ;
        end ;

        if not is_same_value( fld,
                              _values[i],
                              is_str,
                              _isinsensitive,
                              _ispartial
                            )
        then begin
          Result := False ;
          break ;
        end ;
      end ;
    end ;

  var
    lst_fields     : TStringList ;
    is_partial     : Boolean     ;
    is_insensitive : Boolean     ;
    values         : Variant     ;
    uid_idx        : Integer     ;
    uid_val        : TGIS_Uid    ;
    rec            : T_dsRecord  ;
    shp            : TGIS_Shape  ;
    i : Integer ;
  begin
    Result := nil ;
    CheckBrowseMode  ;
    CursorPosChanged ;

    lst_fields := TStringList.Create ;
    lst_fields.CaseSensitive := False ; // to find GIS_UID case insensitive

    try
      lst_fields.CommaText := StringReplace( _keyFields, ';', ',',
                                             [ rfReplaceAll ]
                                           ) ;

      is_partial     := loPartialKey      in _options ;
      is_insensitive := loCaseInsensitive in _options ;

      if VarIsArray( _keyValues ) then values := _keyValues
                                  else values := VarArrayOf( [_keyValues] ) ;

      // if GIS_UID was referred use it as the most important case
      uid_idx := lst_fields.IndexOf( GIS_FIELD_UID ) ;

      if uid_idx >= 0 then begin
        uid_val := values[ uid_idx ] ;
        shp := dsLayer.GetShape( uid_val ) ;
        if check_values( shp, lst_fields, values, is_insensitive, is_partial )
        then
          Result := shp ;
      end
      else begin
        // must loop shapes
          for i:=0 to lstRecords.Count -1 do begin
            rec := lstRecords[i] as T_dsRecord ;
            shp := dsLayer.GetShape( rec.Uid ) ;
            if check_values( shp, lst_fields, values, is_insensitive, is_partial )
            then begin
              Result := shp ;
              break;
            end ;
          end ;
      end ;
    finally
      lst_fields.Free ;
    end ;
  end ;

  {$IFDEF LEVEL_XE3_VCL}
    procedure TGIS_DataSet.SetFieldData(
      _field  : TField       ;
      _buffer : TValueBuffer
    ) ;
    begin
      if assigned( _buffer ) then
        setFieldDataInternal( _field, @_buffer[0] )
      else
        setFieldDataInternal( _field, nil ) ;
    end;
  {$ENDIF}

  {$IFNDEF NEXTGEN}
    procedure TGIS_DataSet.SetFieldData(
      _field  : TField       ;
      _buffer : Pointer
    ) ;
    begin
      setFieldDataInternal( _field,_buffer ) ;
    end;
  {$ENDIF}

  procedure TGIS_DataSet.setFieldDataInternal(
    _field  : TField       ;
    _buffer : Pointer
  ) ;
  var
    vstring     : AnsiString   ;
    vwidestring : String       ;
    vword       : Word         ;
    vsmallint   : Smallint     ;
    vinteger    : Integer      ;
    vint64      : Int64        ;
    vdouble     : Double       ;
    vbool       : WordBool     ;
    vdate       : TDateTimeRec ;
    shp         : TGIS_Shape   ;
  begin
    if lstRecords.Count < 1 then exit ;
    if not Assigned( dsLayer ) then exit ;

    shp := dsLayer.GetShape( getUid ) ;
    if not assigned( shp ) then exit ;

    currShape := shp.MakeEditable ;
    try

      if Assigned( _buffer ) then begin
        case _field.DataType of
          Data.DB.ftString:
            begin
              {$IFDEF NEXTGEN}
                { TODO -cEvaluate : What do do on NEXTGEN? }
                Assert( False, 'Untested case' ) ;
              {$ELSE}
                SetLength( vstring, StrLen( PAnsiChar(_buffer) ) ) ;
                Move( _buffer^, PByte(vstring)^,
                      StrLen( PAnsiChar(_buffer) )
                    ) ;
                currShape.SetField( _field.FullName, vstring ) ;
              {$ENDIF}
            end ;
          Data.DB.ftWideString:
            begin
              {$IFDEF NEXTGEN}
                SetLength( vwidestring, Length( PChar( _buffer ) ) ) ;
                Move( _buffer^, PByte(vwidestring)^,
                      Length( PChar( _buffer ) ) * 2
                    ) ;
              {$ELSE}
                SetLength( vwidestring, StrLen( PWideChar(_buffer) ) ) ;
                Move( _buffer^, PByte(vwidestring)^,
                      StrLen( PChar(_buffer) ) * 2
                    ) ;
              {$ENDIF}
              currShape.SetField( _field.FullName, vwidestring ) ;
            end ;
          Data.DB.ftWord:
            begin
              Move( _buffer^, vword, SizeOf( vword ) ) ;
              currShape.SetField( _field.FullName, vword ) ;
            end ;
          Data.DB.ftSmallint:
            begin
              Move( _buffer^, vsmallint, SizeOf( vsmallint ) ) ;
              currShape.SetField( _field.FullName, vsmallint ) ;
            end ;
          Data.DB.ftInteger:
            begin
              Move( _buffer^, vinteger, SizeOf( vinteger ) ) ;
              currShape.SetField( _field.FullName, vinteger ) ;
            end ;
          Data.DB.ftLargeint :
            begin
              Move( _buffer^, vint64, SizeOf( vint64 ) ) ;
              currShape.SetField( _field.FullName, vint64 ) ;
            end ;
          Data.DB.ftFloat :
            begin
              Move( _buffer^, vdouble, SizeOf( vdouble ) ) ;
              currShape.SetField( _field.FullName, vdouble ) ;
            end ;
          Data.DB.ftBoolean:
            begin
              Move( _buffer^, vbool, SizeOf( vbool ) ) ;
              currShape.SetField( _field.FullName, vbool ) ;
            end ;
          Data.DB.ftDateTime:
            begin
              Move( _buffer^, vdate, SizeOf( vdate ) ) ;
              currShape.SetField( _field.FullName,
                                  TimeStampToDateTime(
                                    MSecsToTimeStamp (
                                      vdate.DateTime
                                    )
                                  )
                                ) ;
            end ;
        else
          begin
            Assert( False,
                    Format( '%s %s %d', [ GIS_RS_ERR_UNTESTED,
                                          'SetFieldData',
                                          IntToStr( Ord( _field.DataType ) )
                                        ]
                          )
                  ) ;
          end ;
        end ;
      end
      else begin
        currShape.SetField( _field.FullName, NullVar )
      end ;

      if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        DataEvent(deFieldChange, NativeInt(_field));
    except
    end ;
  end ;

  {$IFDEF LEVEL_XE3_VCL}
    function TGIS_DataSet.GetFieldData(
      _field        : TField       ;
      {$IFDEF LEVEL_XE4_VCL}
        var _buffer : TValueBuffer
      {$ELSE}
        _buffer     : TValueBuffer
      {$ENDIF}
    ) : Boolean ;

    begin
      if _buffer = nil then
        Result := getFieldDataInternal( _field, nil )
      else
        Result := getFieldDataInternal( _field, @_buffer[0] ) ;
    end;
  {$ENDIF}

  {$IFNDEF NEXTGEN}
    function TGIS_DataSet.GetFieldData(
      _field  : TField       ;
      _buffer : Pointer
    ) : Boolean ;
    begin
      Result := getFieldDataInternal( _field, _buffer ) ;
    end;
  {$ENDIF}

  function TGIS_DataSet.getFieldDataInternal(
    _field  : TField       ;
    _buffer : Pointer
  ) : Boolean ;
  var
    v           : Variant      ;
    vstring     : AnsiString   ;
    vwidestring : String       ;
    vsmallint   : Smallint     ;
    vinteger    : Integer      ;
    vint64      : Int64        ;
    vdouble     : Double       ;
    vbool       : WordBool     ;
    vdate       : TDateTimeRec ;
    uid         : TGIS_Uid     ;
  begin
    if not Assigned( dsLayer ) or ( lstRecords.Count < 1 ) then begin
      Result := False ;
      exit ;
    end ;

    if not Assigned( dsLayer ) or ( lstRecords.Count < 1 ) then begin
      Result := False ;
      exit ;
    end ;

    if not Assigned( _buffer ) then begin
      Result := True;
      exit ;
    end ;

    if uponClose then begin
      Result := False;
      exit ;
    end ;

    uid := getUid ;

    if ( not Assigned( currShape ) ) or ( currShape.Uid <> uid ) then
      // take it only if was changed
      currShape := dsLayer.GetShape( uid ) ;

    v := Unassigned ;
    if Assigned( currShape ) then
      v := currShape.GetFieldEx( _field.FullName, True ) ;

    if not VarIsNull( v ) then begin
      case _field.DataType of
        Data.DB.ftString:
          begin
            {$IFDEF NEXTGEN}
              { TODO -cEvaluate : What do do on NEXTGEN? }
              Assert( False, 'Untested case' ) ;
            {$ELSE}
              vstring := ConvertVar2StrCP(
                           v, dsLayer.CodePage, dsLayer.CodePage
                         )  + #0 ;
            {$ENDIF}
            Move( PByte(vstring)^, _buffer^, Length( vstring ) ) ;
          end ;
        Data.DB.ftWideString:
          begin
            {$IFDEF NEXTGEN}
              vwidestring := v + #0 ;
            {$ELSE}
              vwidestring := ConvertVar2WStrCP( v, dsLayer.CodePage ) + #0 ;
            {$ENDIF}
            Move( PByte(vwidestring)^, _buffer^, Length( vwidestring ) * 2 ) ;
          end ;
        Data.DB.ftSmallint:
          begin
            vsmallint := v ;
            Move( vsmallint, _buffer^, SizeOf( vsmallint ) ) ;
          end ;
        Data.DB.ftInteger:
          begin
            vinteger := v ;
            Move( vinteger, _buffer^, SizeOf( vinteger ) ) ;
          end ;
        Data.DB.ftLargeint:
          begin
            vint64 := v ;
            Move( vint64, _buffer^, SizeOf( vint64 ) ) ;
          end ;
        Data.DB.ftFloat:
          begin
            vdouble := v ;
            Move( vdouble, _buffer^, SizeOf( vdouble ) ) ;
          end ;
        Data.DB.ftBoolean:
          begin
            vbool := v ;
            Move( vbool, _buffer^, SizeOf( vbool ) ) ;
          end ;
        Data.DB.ftDateTime:
          begin
            vdate.DateTime := TimeStampToMSecs(
                                DateTimeToTimeStamp(
                                  v
                                )
                              ) ;
            Move( vdate, _buffer^, SizeOf( vdate ) ) ;
          end ;
        Data.DB.ftVariant:
          begin
            Move( v, _buffer^, SizeOf( v ) ) ;
          end ;
        else
          begin
            Assert( False,
                    Format( '%s %s %d', [ GIS_RS_ERR_UNTESTED,
                                          'GetFieldData',
                                          Ord( _field.DataType )
                                        ]
                          )
                  ) ;
          end ;
      end ;
      Result := True ;
    end
    else
      Result := False ;

  end ;

  function TGIS_DataSet.BookmarkValid(
    _bookmark: TBookmark
  ) : Boolean ;
  var
    irec : Integer ;
  begin
    irec := PInteger(_bookmark)^ ;

    Result := ( irec >= 0 ) and ( irec < lstRecords.Count ) ;
  end;

  function TGIS_DataSet.CompareBookmarks(
    _bookmark1 : TBookmark ;
    _bookmark2 : TBookmark
  ) : Integer ;
  var
    irec1 : Integer ;
    irec2 : Integer ;
  begin
    irec1 := PInteger(_bookmark1)^ ;
    irec2 := PInteger(_bookmark2)^ ;
    Result := CompareValue( irec1, irec2 ) ;
  end;

  function TGIS_DataSet.Locate(
    const _keyFields : String        ;
    const _keyValues : Variant       ;
          _options   : TLocateOptions
  ) : Boolean ;
  var
    shp : TGIS_Shape ;
  begin
    Result := False ;
    shp := LocateRecord( _keyFields, _keyValues, _options ) ;
    if Assigned( shp ) then begin
      GotoShape( shp.Uid ) ;
      Result := True ;
    end ;
  end ;

  function TGIS_DataSet.Lookup(
    const _keyFields    : String  ;
    const _keyValues    : Variant ;
    const _resultFields : String
  ) : Variant ;
  var
    i   : Integer ;
    shp : TGIS_Shape ;
    lst_fields : TStringList ;
  begin
    Result := False ;

    shp := LocateRecord( _keyFields, _keyValues, [] ) ;

    if Assigned( shp ) then begin
      lst_fields := TStringList.Create ;
      try
        lst_fields.CommaText := StringReplace( _resultFields, ';', ',',
                                               [ rfReplaceAll ]
                                             ) ;
        if      lst_fields.Count > 1 then
                begin
                  Result := VarArrayCreate( [0,lst_fields.Count-1], varVariant ) ;
                  for i:=0 to lst_fields.Count-1 do begin
                    Result[i] := shp.GetField( lst_fields[i] ) ;
                  end ;
                end
        else if lst_fields.Count = 1 then begin
                  Result := shp.GetField( lst_fields[0] ) ;
                end
        else    begin
                  Result := False ;
                end ;
      finally
        lst_fields.Free ;
      end ;
    end ;
  end ;

  procedure TGIS_DataSet.Open(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent      ;
    const _query       : String           ;
    const _shape       : TGIS_Shape = nil ;
    const _de9im       : String = ''      ;
    const _skipDeleted : Boolean = True   ;
    const _maxRecords  : Integer = -1
  ) ;
  begin
    Close ;

    dsLayer       := _layer        ;
    dsExtent      := _extent       ;
    dsQuery       := _query        ;
    dsShape       := _shape        ;
    dsDe9im       := _de9im        ;
    dsSkipDeleted := _skipDeleted  ;
    dsMaxRecords  := _maxRecords   ;

    if not Assigned( dsLayer ) then exit ;

    if not dsLayer.IsOpened then
      dsLayer.Open;

    if assigned( dsLayer.Viewer ) then
      oViewer := dsLayer.Viewer.Ref ;

    if assigned( oViewer ) then
      oViewer.Subscribe( Self ) ;

    inherited Open ;
  end ;

  procedure TGIS_DataSet.Open(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent
  ) ;
  begin
    Open( _layer, _extent, '' ) ;
  end;

{$IFDEF GIS_PDK}
  procedure TGIS_DataSet.OpenInternalLayer(
    const _layer       : TGIS_LayerVector
  ) ;
  begin
    Open( _layer, GisWholeWorld ) ;
    oInternalLayer := _layer ;
  end;
{$ENDIF}

  function TGIS_DataSet.GotoShape(
    const _uid : TGIS_Uid
  ) : Boolean ;
  var
    i   : Integer    ;
    rec : T_dsRecord ;
  begin
    Result := False ;

    for i:=0 to lstRecords.Count - 1 do begin
      rec := lstRecords[i] as T_dsRecord ;
      if rec.Uid = _uid then begin
        DoBeforeScroll;
        currRecord := i ;
        Result := True ;
        Resync([rmExact, rmCenter]);
        DoAfterScroll;
        break ;
      end ;
    end ;
  end ;

  procedure TGIS_DataSet.SubscribedEvent(
    _sender  : TObject ;
    _event   : Integer ;
    _context : TObject
  ) ;
  begin
    case _event of
      GIS_SUBSCRIBED_DESTROY :
        begin
          uponClose := True ;
          oViewer := nil ;
        end ;
      GIS_SUBSCRIBED_PROJECT_CLOSE :
        begin
          uponClose := True ;
          Close ;
        end ;
    end ;
  end ;

//==============================================================================
// T_dsRecord
//==============================================================================

  constructor T_dsRecord.Create(const _uid : TGIS_Uid) ;
  begin
    Uid := _uid ;
  end ;

{==================================== END =====================================}
end.


