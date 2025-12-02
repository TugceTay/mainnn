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
  PVL Grid control
}
{$IFDEF DCC}
  unit PVL.GisGrid;
  {$HPPEMIT '#pragma link "PVL.GisGrid"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

interface

uses
  {$IFDEF CLR}
    System.Text.RegularExpressions,
    TatukGIS.NDK,
    TatukGIS.NDK.WinForms,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
  {$M+}
    System.Classes,
    System.SysUtils,
    System.Types,
    System.Math,
    System.RegularExpressions,
    GisRtl,
    GisTypes,
    GisDataSet,
    PVL.GisPvl;
  {$ENDIF}

  {$IFDEF JAVA}
    java.util,
    tatukgis.jdk.*,
    java.awt.*,
    javax.swing.*,
    java.beans.*,
    tatukgis.rtl ;
  {$ENDIF}


type
  /// <summary>
  ///   Style of rows/colums header.
  /// </summary>
  TGIS_PVlGridHeader = (
    /// <summary>
    ///  No header.
    /// </summary>
    None,

    /// <summary>
    ///  Header is automatic: for rows row number; for columns columns name from
    ///  data set or specified by Ca[ption property.
    /// </summary>
    Auto,

    /// <summary>
    ///  Header represnets forst row/column.
    /// </summary>
    First
  ) ;

  /// <summary>
  ///   Cell alignement.
  /// </summary>
  TGIS_PvlGridCellAlignment = (
    /// <summary>
    ///  Left justified.
    /// </summary>
    Left,

    /// <summary>
    ///  Centered..
    /// </summary>
    Center,

    /// <summary>
    ///  Right justified.
    /// </summary>
    Right
  ) ;

  TGIS_PvlGrid = class;


  /// <summary>
  ///   Grid's column representation.
  /// </summary>
  TGIS_PVlGridColumn = class
    public
      /// <summary>
      ///   Create a column and assign it to the grid.
      /// </summary>
      /// <param name="_parent">
      ///   parent grid
      /// </param>
      constructor Create         ( const _parent      : TGIS_PvlGrid
                                 ) ;
    private
      oParent       : TGIS_PvlGrid ;
      sCaption      : String ;
      oAlign        : TGIS_PvlGridCellAlignment ;
      bAllowNulls   : Boolean ;
      oFieldType    : TGIS_FieldType ;
      iFieldWidth   : Integer ;
      iFieldDecimal : Integer ;
      bReadOnly     : Boolean ;
      bVisible      : Boolean ;
      iWidth        : Integer ;
      bFitWidth     : Boolean ;
      sFormat       : String ;
      sRegex        : String ;
    protected
      function  fget_Visible      : Boolean ;
      procedure fset_Visible      ( const _value      : Boolean
                                  );
      function  fget_ReadOnly     : Boolean ;
      procedure fset_ReadOnly     ( const _value      : Boolean
                                  );
      function  fget_Caption      : String ;
      procedure fset_Caption      ( const _value      : String
                                  );
      function  fget_Align        : TGIS_PvlGridCellAlignment ;
      procedure fset_Align        ( const _value      : TGIS_PvlGridCellAlignment
                                  );
      function  fget_FieldType    : TGIS_FieldType  ;
      procedure fset_FieldType    ( const _value      : TGIS_FieldType
                                  );
      function  fget_FieldWidth   : Integer  ;
      procedure fset_FieldWidth   ( const _value      : Integer
                                  );
      function  fget_FieldDecimal : Integer  ;
      procedure fset_FieldDecimal ( const _value      : Integer
                                  );
      function  fget_Width        : Integer ;
      procedure fset_Width        ( const _value      : Integer
                                  );
      function  fget_FitWidth     : Boolean ;
      procedure fset_FitWidth     ( const _value      : Boolean
                                  );
      function  fget_Format       : String ;
      procedure fset_Format       ( const _value      : String
                                  );
      function  fget_Regex        : String ;
      procedure fset_Regex        ( const _value      : String
                                  );

    private
      procedure defaultFormat     ;

    public
      /// <summary>
      ///   Validaty if text matches column format.
      /// </summary>
      /// <param name="_text">
      ///   text to be validated
      /// </param>
      /// <returns>
      ///   True, if provided text matches column formats
      /// </returns>
      /// <remarks>
      ///   Use Regex defintion wich is set by default accoring to FieldType
      ///   property.
      /// </remarks>
      function  Validate          ( const _text       : String
                                  ) : Boolean ;

    public
      /// <summary>
      ///   Is column visible?
      /// </summary>
      property Visible
        : Boolean
          read  fget_Visible
          write fset_Visible ;

      /// <summary>
      ///   Is column read-only?
      /// </summary>
      property ReadOnly
        : Boolean
          read  fget_ReadOnly
          write fset_ReadOnly ;

      /// <summary>
      ///   Column caption.
      /// </summary>
      property Caption
        : String
          read  fget_Caption
          write fset_Caption ;

      /// <summary>
      ///   Column alignment.
      /// </summary>
      property Align
        : TGIS_PvlGridCellAlignment
          read  fget_Align
          write fset_Align ;

      /// <summary>
      ///   Are nulls/empty values allowed?
      /// </summary>
      property AllowNulls
        : Boolean
          read  bAllowNulls
          write bAllowNulls ;

      /// <summary>
      ///   Column  field type?
      /// </summary>
      property FieldType
        : TGIS_FieldType
          read  fget_FieldType
          write fset_FieldType ;

      /// <summary>
      ///   Column  field width. Important for number and string fields.
      /// </summary>
      property FieldWidth
        : Integer
          read  fget_FieldWidth
          write fset_FieldWidth ;

      /// <summary>
      ///   Column  field decimal points. Important for number and floatd fields.
      /// </summary>
      property FieldDecimal
        : Integer
          read  fget_FieldDecimal
          write fset_FieldDecimal;

      /// <summary>
      ///   Column  width expressed in device independent units.
      /// </summary>
      property Width
        : Integer
          read  fget_Width
          write fset_Width ;

      /// <summary>
      ///   If colunt shpould be expanded to perfeclty fill grid wdith. Onnly
      ///   frst such columnn will be expanded accrodingly.
      /// </summary>
      property FitWidth
        : Boolean
          read  fget_FitWidth
          write fset_FitWidth ;

      /// <summary>
      ///   Column format specifier. Same syntax as TGIS_StringFormat.
      /// </summary>
      property Format
        : String
          read  fget_Format
          write fset_Format ;

      /// <summary>
      ///   Regex expression to be used to validate conent upon editing.
      /// </summary>
      property Regex
        : String
          read  fget_Regex
          write fset_Regex ;
  end;

  /// <summary>
  ///   Grid's row representation.
  /// </summary>
  TGIS_PVlGridRow = class( TGIS_BaseObjectDisposable )
    public
      /// <summary>
      ///   Create a row and assign it to the grid.
      /// </summary>
      /// <param name="_parent">
      ///   parent grid
      /// </param>
      constructor Create          ( const _parent     : TGIS_PvlGrid
                                  ) ;
    protected
      procedure doDestroy; override;

    private
      oParent    : TGIS_PvlGrid ;
      arElements : array of Variant;
      rowId      : Int64;
    private
      function  fget_Element      ( const _index      : Integer
                                  ) : Variant;
      procedure fset_Element      ( const _index      : Integer;
                                    const _value      : Variant
                                  );
      procedure setSize           ( const _cols       : Integer
                                  ) ;
    public
      /// <summary>
      ///    Access comulnt by number. Columns ar numbered from 1.
      /// </summary>
      /// <param name="_index">
      ///   column number; column=0 is a row header column
      /// </param>
      property Column[ const _index : Integer ]
        : Variant
          read fget_Element
          write fset_Element ;
    end;

  /// <summary>
  ///   PVL Grid interface.
  /// </summary>
  IGIS_PvlGrid = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{5F1D60E0-2FCA-426C-9505-B3553B296ADC}']
    {$ENDIF}

    function  fget_ClientWidth  : Integer;
    function  fget_ClientHeight : Integer;

    /// <summary>
    ///   Measure text size.
    /// </summary>
    /// <param name="_txt">
    ///   text to be meausred
    /// </param>
    /// <returns>
    ///   Measued text width/height in devic3e independent units.
    /// </returns>
    function  MeasureText       ( const _txt        : String
                                ) : TPoint ;

    /// <summary>
    ///   Draw a cell.
    /// </summary>
    /// <param name="_col">
    ///   cell column
    /// </param>
    /// <param name="_row">
    ///   cell row
    /// </param>
    /// <param name="_txt">
    ///   text to be drawn
    /// </param>
    /// <param name="_canvas">
    ///   canvas object to be drawn on
    /// </param>
    procedure DrawCell          ( const _col        : Integer;
                                  const _row        : Integer;
                                  const _txt        : String;
                                  const _canvas     : TObject
                                  ) ;

    /// <summary>
    ///   Draw whole content.
    /// </summary>
    /// <param name="_canvas">
    ///   canvas object to be drawn on
    /// </param>
    procedure DrawAll           ( const _canvas     : TObject
                                ) ;

    /// <summary>
    ///   Invalidate control - mark for redraw.
    /// </summary>
    procedure InvalidateControl ;

    /// <summary>
    ///   Realign control - recalvulate srollbars etc.
    /// </summary>
    procedure RealignControl    ;

    /// <summary>
    ///   Realign control - recalvulate srollbars etc.
    /// </summary>
    /// <param name="_txt">
    ///   content to be edited
    /// </param>
    /// <param name="_char">
    ///   key char taht started editing or #0;
    ///   if editing is tarted by pressing a char key it will
    ///   put editor into proper state
    /// </param>
    procedure BeginEdit         ( const _txt        : String ;
                                  const _char       : Char
                                ) ;

    /// <summary>
    ///   Finalize cell editing.
    /// </summary>
    /// <param name="_commit">
    ///   if true, then edited text will be applied;
    ///   otherwise editing is canceled
    /// </param>
    procedure EndEdit           ( const _commit     : Boolean
                                ) ;


    function  fget_DataSet      : TObject ;
    procedure fset_DataSet      ( const _value      : TObject
                                );

    /// <summary>
    ///   Moves to the first record in the associated dataset if one is present.
    /// </summary>
    procedure First             ;

    /// <summary>
    ///   Moves to another record relative
    ///   to the active record in the associated dataset if one is present.
    /// </summary>
    /// <param name="_dist">
    ///   A positive value for Distance indicates forward progress through the dataset, 
    ///   while a negative value indicates backward progress
    /// </param>
    procedure MoveBy            ( const _dist       : Integer
                                );
  end;

  /// <summary>
  ///   PVL grid component.
  /// </summary>
  TGIS_PvlGrid = class( TGIS_PvlControl, IGIS_PvlGrid )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected

      /// <inheritdoc/>
      procedure doDestroy         ; override;

      /// <inheritdoc/>
      procedure initControl       ; override;

      private
        oPlatformCasted : IGIS_PvlGrid ;
      private
        function fget_PlatformControl
                                  : IGIS_PvlGrid ;
                                  reintroduce;
      private
        property PlatformControl
          : IGIS_PvlGrid
           read  fget_PlatformControl; {$IFDEF OXYGENE}override;{$ENDIF}

    private
      bReadOnly            : Boolean ;
      iLock                : Integer ;
      arRows               : array of TGIS_PVlGridRow;
      arColumns            : array of TGIS_PVlGridColumn;

      fcolsHeader          : TGIS_PVlGridHeader;
      frowsHeader          : TGIS_PVlGridHeader;

      fdefaultColumnsWidth : Integer;
      frowsHeight          : Integer;
      rActiveCell          : TPoint;
      bActiveEditor        : Boolean ;

      visibleCells         : TRect;
      visibleCellsOrigin   : TPoint;

      iPrevCol             : Integer ;

      rScrollPos           : TPoint ;
      rScrollMax           : TPoint ;

      iLastXOffset         : Integer ;

      evnBeginEditEvent    : TNotifyEvent;
      evnEndEditEvent      : TNotifyEvent;
      evnSelectEvent       : TNotifyEvent;

      bRowSelect           : Boolean;

    public
      /// <summary>
      ///   Clear whole grid. Disconnect dataset if attached.
      /// </summary>
      procedure Clear             ;

      /// <summary>
      ///   Set size of the grid. Setting DataSet will verride this.
      /// </summary>
      /// <param name="_cols">
      ///   number of columns
      /// </param>
      /// <param name="_rows">
      ///   number of rows
      /// </param>
      /// <remarks>
      ///    Same can be achived by AddColumn/AddRow.
      /// </remarks>
      procedure SetSize           ( const _cols       : Integer;
                                    const _rows       : Integer
                                  );

      /// <summary>
      ///   Add a new row.
      /// </summary>
      /// <returns>
      ///   Newly created row.
      /// </returns>
      function  AddRow            : TGIS_PVlGridRow ;

      /// <summary>
      ///   Add a new Column.
      /// </summary>
      /// <returns>
      ///   Newly created column.
      /// </returns>
      function  AddColumn         : TGIS_PVlGridColumn ;


      /// <summary>
      ///   Delete a row.
      /// </summary>
      /// <param name="_row">
      ///   number of the roww to be deleted
      /// </param>
      procedure DeleteRow         ( const _row        : Integer
                                  );


      /// <summary>
      ///   Find cell (col/row).
      /// </summary>
      /// <param name="_x">
      ///   x postion related to left-up corner expressed in screen coordinates
      /// </param>
      /// <param name="_y">
      ///   y postion related to left-up corner expressed in screen coordinates
      /// </param>
      /// <returns>
      ///   Cell corridinate in a meaning cof col/row.
      /// </returns>
      function  CellByLocation    ( const _x          : Integer;
                                    const _y          : Integer
                                  ) : TPoint ;

      /// <summary>
      ///   Find screen rectangle occupied by a cell.
      /// </summary>
      /// <param name="_col">
      ///   column corrdinate of the cell
      /// </param>
      /// <param name="_row">
      ///   row corrdinate of the cell
      /// </param>
      /// <returns>
      ///   Cell rectangle related to left-up corner expressed in screen units.
      /// </returns>
      function  CellRect          ( const _col        : Integer;
                                    const _row        : Integer
                                  ) : TRect ;

      /// <summary>
      ///   Find screen rectangle occupied by a cell editor for active cell.
      /// </summary>
      /// <returns>
      ///   Cell rectangle related to left-up corner expressed in screen units..
      /// </returns>
      function  CellEditRect      : TRect ;

      /// <summary>
      ///   Scroll view to absolute position.
      ///   Cooridinates are expressed in width/height of entire grid in
      ///   screen coordinates.
      /// </summary>
      /// <param name="_x">
      ///    horizontal scroll
      /// </param>
      /// <param name="_y">
      ///    vertical scroll
      /// </param>
      procedure ScrollTo          ( const _x          : Integer;
                                    const _y          : Integer
                                  ) ;

      /// <summary>
      ///   Scroll view by delta.
      ///   Cooridinates are expressed in width/height of entire grid in in
      ///   screen coordinates.
      /// </summary>
      /// <param name="_x">
      ///    horizontal scroll
      /// </param>
      /// <param name="_y">
      ///    vertical scroll
      /// </param>
      procedure ScrollBy          ( const _x          : Integer;
                                    const _y          : Integer
                                  ) ;

      /// <summary>
      ///   Ensure that active cell is visible in the view (screen).
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      function  ScrollToActiveCell: Boolean ;

      /// <summary>
      ///   Scroll right by one column.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollRight       : Boolean ;

      /// <summary>
      ///   Scroll left by one column.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollLeft        : Boolean ;

      /// <summary>
      ///   Scroll to the first column.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollHome        : Boolean ;

      /// <summary>
      ///   Scroll to the last column.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollEnd         : Boolean ;

      /// <summary>
      ///   Scroll to next line.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollDown        : Boolean ;

      /// <summary>
      ///   Scroll to previous line.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollUp          : Boolean ;

      /// <summary>
      ///   Scroll one page down.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollPageDown    : Boolean ;

      /// <summary>
      ///   Scroll one page up.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollPageUp      : Boolean ;

      /// <summary>
      ///   Scroll to the first row.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollTop         : Boolean ;

      /// <summary>
      ///   Scroll to the last row.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollBottom      : Boolean ;

      /// <summary>
      ///   Scroll to the next column; if column is last one,
      ///   then scroll to the first column of the next row.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollNext        : Boolean ;

      /// <summary>
      ///   Scroll to the previous colun; if column is the first one,
      ///   then scroll scroll to the last column of the previous row.
      /// </summary>
      /// <returns>
      ///   True if the view was scrolled.
      /// </returns>
      /// <remarks>
      ///   Scrolling also move active cell adequately.
      /// </remarks>
      function  ScrollPrevious    : Boolean ;

    protected // interface API
      /// <inheritdoc/>
      function  fget_ClientWidth  : Integer;
      /// <inheritdoc/>
      function  fget_ClientHeight : Integer;
      /// <inheritdoc/>
      function  fget_DataSet      : TObject ;
      /// <inheritdoc/>
      procedure fset_DataSet      ( const _value      : TObject
                                  );

    public // interface API
      /// <inheritdoc/>
      procedure SetFocus          ; override;

      /// <inheritdoc/>
      function  MeasureText       ( const _txt        : String
                                  ) : TPoint ;

      /// <inheritdoc/>
      procedure DrawCell          ( const _col        : Integer;
                                    const _row        : Integer;
                                    const _txt        : String;
                                    const _canvas     : TObject
                                  ) ;

      /// <inheritdoc/>
      procedure DrawAll           ( const _canvas     : TObject
                                  ) ;

      /// <inheritdoc/>
      procedure InvalidateControl ;

      /// <inheritdoc/>
      procedure RealignControl    ;

      /// <inheritdoc/>
      procedure BeginEdit         ( const _txt        : String ;
                                    const _char       : Char
                                  ) ;
      /// <inheritdoc/>
      procedure EndEdit           ( const _commit     : Boolean
                                  ) ;
      /// <inheritdoc/>
      procedure First             ;
      /// <inheritdoc/>
      procedure MoveBy            ( const _dist       : Integer
                                  );
    private
      function  fget_ScrollRect   : TRect;
      function  fget_ColumnsCount : Integer;
      function  fget_RowsCount    : Integer;
      function  fget_Column       ( const _index      : Integer
                                  ) : TGIS_PVlGridColumn;
      function  fget_Row          ( const _index      : Integer
                                  ) : TGIS_PVlGridRow;
      function  fget_Cell         ( const _col        : Integer;
                                    const _row        : Integer
                                  ) : Variant;
      procedure fset_Cell         ( const _col        : Integer;
                                    const _row        : Integer;
                                    const _value      : Variant
                                  );
      procedure fset_ReadOnly     ( const _value      : Boolean
                                  );
      procedure fset_ColumnsHeader( const _value      : TGIS_PVlGridHeader
                                  );
      procedure fset_RowsHeader   ( const _value      : TGIS_PVlGridHeader
                                  );
      procedure fset_ActiveCell   ( const _value      : TPoint
                                  );
      function  leftMargin        : Integer ;
      function  topMargin         : Integer ;
      function  startColumn       : Integer ;
      function  startRow          : Integer ;
      function  formattedCell     ( const _col        : Integer;
                                    const _val        : Variant
                                  ) : String ;
    public
      /// <summary>
      ///   Resize grid.
      /// </summary>
      procedure AutoSize          ;

      /// <summary>
      ///   Begin update with large data.
      ///   Locks visual update of the grid.
      /// </summary>
      procedure BeginUpdate       ;

      /// <summary>
      ///   End update with large data.
      ///   Unlocks visual update of the grid.
      /// </summary>
      procedure EndUpdate         ;

    public
      /// <summary>
      ///   Is grid read-only?
      /// </summary>
      property ReadOnly
        : Boolean
          read  bReadOnly
          write fset_ReadOnly      ;

      /// <summary>
      ///   Defines columns header mode.
      /// </summary>
      property ColumnsHeader
        : TGIS_PVlGridHeader
          read  fcolsHeader
          write fset_ColumnsHeader ;

      /// <summary>
      ///   Defines rows header mode.
      /// </summary>
      property RowsHeader
        : TGIS_PVlGridHeader
          read  frowsHeader
          write fset_RowsHeader ;

      /// <summary>
      ///   If true, then slection will selct whole row.
      /// </summary>
      property RowSelect
        : Boolean
          read  bRowSelect
          write bRowSelect ;

      /// <summary>
      ///   Number of columns.
      /// </summary>
      property ColumnsCount
        : Integer
          read  fget_ColumnsCount ;

      /// <summary>
      ///   Default column width.
      /// </summary>
      property DefaultColumnsWidth
        : Integer
          read  fdefaultColumnsWidth
          write fdefaultColumnsWidth;

      /// <summary>
      ///   Rows  height expressed in device independent units.
      /// </summary>
      property RowsHeight
        : Integer
          read  frowsHeight
          write frowsHeight;

      /// <summary>
      ///   Number of rows.
      /// </summary>
      property RowsCount
        : Integer
          read  fget_RowsCount ;

      /// <summary>
      ///   Access column object.
      /// </summary>
      /// <param name="_index">
      ///   column number; column=0 is a row header column
      /// </param>
      property Column[ const _index : Integer ]
        : TGIS_PVlGridColumn
          read  fget_Column ;

      /// <summary>
      ///   Access row object.
      /// </summary>
      /// <param name="_index">
      ///   rown number; counted from 1
      /// </param>
      property Row[ const _index : Integer ]
        : TGIS_PVlGridRow
          read  fget_Row ;

      /// <summary>
      ///   Access cell.
      /// </summary>
      /// <param name="_col">
      ///   column number ; counted from 1
      /// </param>
      /// <param name="_row">
      ///   row number ; counted from 1
      /// </param>
      property Cell[ const _col : Integer; const _row : Integer ]
        : Variant
          read  fget_Cell
          write fset_Cell ;

      /// <summary>
      ///   Connect to dataset.
      /// </summary>
      /// <remarks>
      ///   Upon assigning exiting conent will be cleared and
      ///   control will be in "data grid"mode. By providing null
      ///   control will be switched back to a "string grid" mode.
      /// </remarks>
      property DataSet
        : TObject
         read  fget_DataSet
         write fset_DataSet ;

      /// <summary>
      ///   Active cell.
      /// </summary>
      /// <returns>
      ///   Active cell  column/row. If cell was not selected then result is (-1,-1).
      /// </returns>
      property ActiveCell
        : TPoint
          read  rActiveCell
          write fset_ActiveCell ;

      /// <summary>
      ///   True if editor is active
      /// </summary>
      property ActiveEditor
        : Boolean
          read  bActiveEditor;

      /// <summary>
      ///   Position of scrollbars.
      /// </summary>
      property ScrollPos
        : TPoint
          read  rScrollPos ;

      /// <summary>
      ///   Maximum scrolling distance. Calulated by countingg visible
      ///   columns/row width and columns/rows header.
      /// </summary>
      property ScrollMax
        : TPoint
          read  rScrollMax ;

      /// <summary>
      ///   Area of control that can be scrolled. Calulated by removing
      ///   area occupied by columns/rows header.
      /// </summary>
      property ScrollRect
        : TRect
          read fget_ScrollRect ;

    published
      /// <summary>
      ///   Event to be fired upon begin of cell editing.
      /// </summary>
      /// <event/>
      property BeginEditEvent
        : TNotifyEvent
          read  evnBeginEditEvent
          write evnBeginEditEvent;

      /// <summary>
      ///   Event to be fired upon end of cell editing.
      /// </summary>
      /// <event/>
      property EndEditEvent
        : TNotifyEvent
          read  evnEndEditEvent
         write evnEndEditEvent;

      /// <summary>
      ///   Event to be fired upon end cell celection.
      /// </summary>
      /// <event/>
      property SelectEvent
        : TNotifyEvent
          read  evnSelectEvent
          write evnSelectEvent;
    end;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Variants,
    // ensure that proper implementation files are referenced in Delphi
    GisStringFormat
    {$IFDEF USE_FMX}
      ,FMX.GisPvlGrid
    {$ENDIF}
    {$IFDEF USE_VCL}
      ,VCL.GisPvlGrid
    {$ENDIF}
      ;
{$ENDIF}

function spreasheetColumn(
  const _idx : Integer
) : String ;
var
  q     : Integer ;
  q_tmp : Integer ;
  r     : Integer ;
begin
  Result := '';

  q := _idx ;
  repeat
    q := q-1;
    q_tmp := q div 26 ;
    r := q - q_tmp * 26 ;

    Result := Result + Char( r + ord('A') ) ;

    q := q_tmp ;
  until q = 0 ;
end;


{$REGION 'TGIS_PVlGridColum'}

constructor TGIS_PVlGridColumn.Create(
  const _parent: TGIS_PvlGrid
);
begin
 oParent := _parent;
  bVisible := True ;
  bReadOnly := False ;
  bFitWidth := False;
  bAllowNulls := True ;
  oFieldType := TGIS_FieldType.String ;
  iFieldWidth := 0 ;
  iFieldDecimal := 0 ;
  oAlign := TGIS_PvlGridCellAlignment.Center ;
  sFormat  := '' ;
  sRegex  := '' ;
  iWidth := oParent.DefaultColumnsWidth ;
end;

function TGIS_PVlGridColumn.fget_Visible
  : Boolean ;
begin
  Result := bVisible ;
end;

procedure TGIS_PVlGridColumn.fset_Visible(
  const _value : Boolean
);
begin
  bVisible := _value ;
  oParent.RealignControl ;
end;

function TGIS_PVlGridColumn.fget_ReadOnly
  : Boolean ;
begin
  Result := bReadOnly ;
end;

procedure TGIS_PVlGridColumn.fset_ReadOnly(
  const _value : Boolean
);
begin
  bReadOnly := _value ;
  oParent.RealignControl ;
end;

function TGIS_PVlGridColumn.fget_Caption
  : String ;
begin
  Result := sCaption ;
end;

procedure TGIS_PVlGridColumn.fset_Caption(
  const _value : String
);
begin
  sCaption := _value ;
  oParent.RealignControl ;
end;

function TGIS_PVlGridColumn.fget_Align
  : TGIS_PvlGridCellAlignment ;
begin
  Result := oAlign ;
 end;

procedure TGIS_PVlGridColumn.fset_Align(
  const _value : TGIS_PvlGridCellAlignment
) ;
begin
  oAlign := _value ;
end ;

function TGIS_PVlGridColumn.fget_FieldType
  : TGIS_FieldType ;
begin
  Result := oFieldType ;
end;

procedure TGIS_PVlGridColumn.fset_FieldType(
  const _value : TGIS_FieldType
) ;
begin
  oFieldType := _value ;
  case oFieldType of
    TGIS_FieldType.String :
      begin
        sFormat := '$' ;
        sRegex := '' ;
        FieldWidth := 0 ;
        FieldDecimal := 0 ;
        end;
    TGIS_FieldType.Number:
      begin
        sFormat :='n';
        sRegex := '^[-+]?[0-9]*\.?[0-9]*$';
        FieldWidth := 10 ;
        FieldDecimal := 0 ;
      end;
    TGIS_FieldType.Float :
      begin
        sFormat :='g';
        sRegex := '^[-+]?[0-9]*\.?[0-9]*$';
        FieldWidth := 0 ;
        FieldDecimal := 2 ;
      end;
    TGIS_FieldType.Boolean :
      begin
        sFormat := '' ;
        sRegex := ''; //?
        end;
    TGIS_FieldType.Date :
      begin
        sFormat := 'yyyy/MM/dd' ;
        sRegex := ''; //?
      end;
  end;
end ;

function TGIS_PVlGridColumn.fget_FieldWidth
  : Integer ;
begin
  Result := iFieldWidth ;
end;

procedure TGIS_PVlGridColumn.fset_FieldWidth(
  const _value : Integer
) ;
begin
  iFieldWidth := Max( 0, _value ) ;
  defaultFormat ;
end ;

function TGIS_PVlGridColumn.fget_FieldDecimal
  : Integer ;
begin
  Result := iFieldDecimal ;
end;

procedure TGIS_PVlGridColumn.fset_FieldDecimal(
  const _value : Integer
) ;
begin
  iFieldDecimal := _value ;

  defaultFormat ;
end ;

function TGIS_PVlGridColumn.fget_Width
  : Integer ;
begin
  Result := iWidth ;
end;

procedure TGIS_PVlGridColumn.fset_Width(
  const _value : Integer
);
begin
  iWidth := _value ;
  oParent.RealignControl ;
end;

function TGIS_PVlGridColumn.fget_FitWidth
  : Boolean ;
begin
  Result := bFitWidth ;
end;

procedure TGIS_PVlGridColumn.fset_FitWidth(
  const _value : Boolean
);
begin
  bFitWidth := _value ;
  oParent.RealignControl ;
end;

function TGIS_PVlGridColumn.fget_Format
  : String ;
begin
  Result := sFormat ;
end;

procedure TGIS_PVlGridColumn.fset_Format(
  const _value : String
);
begin
  sFormat := _value ;
  oParent.RealignControl ;
end;

function TGIS_PVlGridColumn.fget_Regex
  : String ;
begin
  Result := sRegex ;
end;

procedure TGIS_PVlGridColumn.fset_Regex(
  const _value : String
);
begin
  sRegex := _value ;
end;

procedure TGIS_PVlGridColumn.defaultFormat ;
var
  i : Integer ;
begin
  case oFieldType of
    TGIS_FieldType.String :
      begin
        iFieldDecimal := 0;
        sFormat := '' ;
        if iFieldWidth > 0 then
          sRegex := String.Format( '^.{1,%d}$', [iFieldWidth] )
        else
          sRegex := '' ;
      end;
    TGIS_FieldType.Number :
      begin
        if iFieldDecimal <= 0 then begin
          sFormat :='0';
          sRegex := '^[-+]?[0-9]*$'
        end
        else begin
          sFormat :='0.';
          for i := 1 to iFieldDecimal do
            sFormat := sFormat + '0';
          sRegex := '^[-+]?[0-9]*\.?[0-9]*$';
        end;
      end;
  TGIS_FieldType.Float :
    begin
     sFormat :='g';
     sRegex := '^[-+]?[0-9]*\.?[0-9]*$';
    end;
  TGIS_FieldType.Boolean :
    begin
      sFormat := '' ;
      sRegex := ''; //?
    end;
  TGIS_FieldType.Date :
    begin
      sFormat := 'yyyy/MM/dd' ;
      sRegex := ''; //?
    end;
  end;
end;

function TGIS_PVlGridColumn.Validate(
  const _text : String
) : Boolean ;
var
 {$IFDEF OXYGENE}
   rx : Regex ;
 {$ELSE}
  rx : TRegEx ;
 {$ENDIF}
begin
  Result := True ;
  if IsStringEmpty( sRegex ) then
    exit ;

  if IsStringEmpty( _text ) and AllowNulls then
    exit ;

  try
    {$IFDEF OXYGENE}
      rx := Regex.Create ( sRegex ) ;
    {$ELSE}
      rx := TRegEx.Create( sRegex ) ;
    {$ENDIF}

    Result := rx.IsMatch( _text );
  except
    Result := False ;
  end;
end;

{$ENDREGION 'TGIS_PVlGridColum'}

{$REGION 'TGIS_PVlGridRow'}

constructor TGIS_PVlGridRow.Create(
  const _parent: TGIS_PvlGrid
);
begin
  oParent := _parent;
  SetLength( arElements, oParent.ColumnsCount );
end;

procedure TGIS_PVlGridRow.doDestroy;
begin
  inherited;
end;

function TGIS_PVlGridRow.fget_Element(
  const _index : Integer
) : Variant;
begin
  Result := arElements[ _index-1 ] ;
end;

procedure TGIS_PVlGridRow.fset_Element(
  const _index : Integer ;
  const _value : Variant
);
begin
  arElements[ _index-1 ] := _value ;
end;

procedure TGIS_PVlGridRow.setSize(
  const _cols : Integer
);
begin
  SetLength( arElements, _cols ) ;
end;

{$ENDREGION 'TGIS_PVlGridRow'}

{$REGION 'TGIS_PvlGrid'}

procedure TGIS_PvlGrid.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.Platform.CreateObject(self, 'Grid') ;

  bReadOnly := False ;
  iLock     := 0 ;

  visibleCells := Rect( 0, 0, -1, -1 );

  {$IFDEF GIS_NORECORDS}
    rActiveCell := new TPoint( 0, 0 ) ; //?
    rScrollPos := new TPoint( 0, 0 ) ; //?
    visibleCellsOrigin := new TPoint( 0, 0 ) ; //?
    rScrollMax := new TPoint( 0, 0 ) ; //?
  {$ENDIF}

  frowsHeight := 20 ;//?
  fdefaultColumnsWidth := 50 ;//?

  SetLength( arColumns, 1 );
  arColumns[0] := TGIS_PVlGridColumn.Create( self ) ;
end;

procedure TGIS_PvlGrid.doDestroy ;
var
  x : TGIS_PVlGridColumn ;
  y : TGIS_PVlGridRow ;
  i   : Integer ;
begin
  inherited ;

  for i := 0 to length( arColumns ) - 1 do begin
    x := arColumns[i] ;
    FreeObject( x ) ;
  end;

  for i := 0 to length( arRows ) - 1 do begin
    y := arRows[i] ;
    FreeObject( y ) ;
  end;

end;

function TGIS_PvlGrid.fget_PlatformControl
  : IGIS_PvlGrid;
begin
  if not assigned( oPlatformCasted ) then
    oPlatformCasted := oPlatform as IGIS_PvlGrid ;

  Result := oPlatformCasted;
end;

procedure TGIS_PvlGrid.initControl;
begin
  inherited;
end;

function TGIS_PvlGrid.fget_ScrollRect
  : TRect ;
begin
  Result := Rect( leftMargin, topMargin, fget_ClientWidth - leftMargin , fget_ClientHeight - topMargin );
end;

function TGIS_PvlGrid.fget_ColumnsCount
  : Integer;
begin
  Result := length( arColumns ) - 1;
end;

function TGIS_PvlGrid.fget_RowsCount
  : Integer;
begin
  Result := length( arRows ) ;
  if Result > 0 then
    if not assigned( arRows[0] ) then
      Result := 0 ;
end;

function TGIS_PvlGrid.fget_Column(
  const _index : Integer
) : TGIS_PVlGridColumn;
begin
  Result := arColumns[ _index ] ;
end;

function TGIS_PvlGrid.fget_Row(
  const _index : Integer
) : TGIS_PVlGridRow;
begin
  Result := arRows[ _index-1 ] ;
end;

function TGIS_PvlGrid.fget_Cell(
  const _col : Integer;
  const _row : Integer
  ) : Variant;
begin
  Result := Row[_row].Column[_col];
end;

procedure TGIS_PvlGrid.fset_Cell(
  const _col   : Integer;
  const _row   : Integer;
  const _value : Variant
  ) ;
begin
  Row[_row].Column[_col] := _value ;
end;

function TGIS_PvlGrid.fget_DataSet
  : TObject ;
begin
  Result := PlatformControl.fget_DataSet ;
end;

procedure TGIS_PvlGrid.fset_DataSet(
  const _value  : TObject
) ;
begin
  PlatformControl.fset_DataSet( _value )
end;

procedure TGIS_PvlGrid.fset_ReadOnly(
  const _value : Boolean
);
begin
  if bReadOnly = _value then
    exit ;

  bReadOnly := _value ;
end;

procedure TGIS_PvlGrid.fset_ColumnsHeader(
  const _value : TGIS_PVlGridHeader
);
begin
  fcolsHeader := _value ;
  //> Invalidate
end;

procedure TGIS_PvlGrid.fset_RowsHeader(
  const _value : TGIS_PVlGridHeader
);
begin
  frowsHeader := _value ;
  // Invalidate
end;

procedure TGIS_PvlGrid.fset_ActiveCell(
  const _value : TPoint
);
begin
  if _value = ActiveCell then
    exit ;

  rActiveCell := _value ;

  if rActiveCell.X < startColumn then
    rActiveCell.X := startColumn
  else
  if rActiveCell.X > ColumnsCount then
    rActiveCell.X := ColumnsCount
  else
  if rActiveCell.Y < startRow then
    rActiveCell.Y := startRow
  else
  if rActiveCell.Y > RowsCount then
    rActiveCell.Y := RowsCount;


  if assigned( evnSelectEvent ) and ( iLock=0 ) then
    evnSelectEvent( self );

  if assigned( DataSet ) then begin
    First;
    MoveBy(rActiveCell.Y-1) ;
  end ;
end;

function TGIS_PvlGrid.leftMargin
  : Integer ;
begin
  case RowsHeader of
    TGIS_PVlGridHeader.None :
      begin
        Result := 0;
      end;
    TGIS_PVlGridHeader.Auto :
      begin
        Result := Column[0].Width;
      end;
    TGIS_PVlGridHeader.First :
      begin
        if ColumnsCount > 0 then
          Result := Column[1].Width
        else
          Result := Column[0].Width;
      end;
  end;
end;

function TGIS_PvlGrid.topMargin
  : Integer ;
begin
  case ColumnsHeader of
    TGIS_PVlGridHeader.None :
      begin
        Result := 0;
      end;
    TGIS_PVlGridHeader.Auto :
      begin
        Result := frowsHeight;
      end;
    TGIS_PVlGridHeader.First :
      begin
        Result := frowsHeight;
      end;
  end;
end;


function TGIS_PvlGrid.startColumn
  : Integer ;
begin
  case RowsHeader of
    TGIS_PVlGridHeader.None :
      begin
        Result := 1 ;
      end;
    TGIS_PVlGridHeader.Auto :
      begin
        Result := 1 ;
      end;
    TGIS_PVlGridHeader.First :
      begin
        Result := 2 ;
        end;
  end;
end ;

function TGIS_PvlGrid.startRow
  : Integer ;
begin
  case ColumnsHeader of
    TGIS_PVlGridHeader.None :
      begin
        Result := 1 ;
      end;
    TGIS_PVlGridHeader.Auto :
      begin
        Result := 1 ;
      end;
    TGIS_PVlGridHeader.First :
      begin
        Result := 2 ;
      end;
    end;
 end ;

function TGIS_PvlGrid.formattedCell(
  const _col  : Integer;
  const _val  : Variant
  ) : String ;
begin
  Result := '' ;
  if VarIsNull( _val ) then
    exit ;

  if ( Column[ _col ].Format <> '' ) then begin
    try
      Result := TGIS_StringFormat.Format(
        Column[ _col ].Format,
        _val
      ) ;
    except
      Result := 'ERR' ;
    end;
  end
  else
    {$IFDEF OXYGENE}
      if assigned( _val ) then
        Result :=  _val.ToString 
      else
        Result := '';
    {$ELSE}
      Result := String(_val) ;
    {$ENDIF}
end;

procedure TGIS_PvlGrid.AutoSize ;
var
  icol   : Integer;
  irow   : Integer;
  iwidth : Integer;
begin
  if ColumnsCount < 1 then
    exit;

  for icol := 1 to ColumnsCount do begin
    iwidth := MeasureText( Column[ icol ].Caption ).X ;
    for irow := visibleCells.Top to visibleCells.Bottom do begin
      iwidth := Max( iwidth, MeasureText( formattedCell( icol,  Row[irow].Column[ icol ] ) ).X );
    end;
    Column[ icol ].Width := Min( 150, iwidth ) ;
  end;
  Column[ 0 ].Width := Column[ 1 ].Width ;

  ScrollBy(0,0);
end ;

procedure TGIS_PvlGrid.BeginUpdate ;
begin
  inc(iLock) ;
end;

procedure TGIS_PvlGrid.EndUpdate ;
begin
  dec(iLock) ;
  if iLock < 0 then
    iLock := 0 ;

  if iLock = 0 then begin
    ScrollBy(0,0);
  end;

end;

procedure TGIS_PvlGrid.Clear ;
begin
  DataSet := nil;

  SetSize( 0, 0 );
end;

procedure TGIS_PvlGrid.SetSize(
  const _cols : Integer;
  const _rows : Integer
  );
var
  i : Integer ;
  old_cols : Integer ;
  old_rows : Integer ;
begin
  BeginUpdate ;
  try
    old_cols := ColumnsCount ;

    if old_cols <> _cols then begin
      // downsize columns
      for i := Max( 1, _cols ) to old_cols do
        FreeObject( arColumns[ i ] ) ;

      SetLength( arColumns, _cols+1 );

      for i := old_cols+1 to Max( 1, _cols ) do begin
        arColumns[i] := TGIS_PVlGridColumn.Create( self ) ;
        arColumns[i].sCaption := spreasheetColumn( i ) ; //?
      end;
    end;

    old_rows := length( arRows ) ;
    if _rows <> old_rows then begin
      // downsize rows
      for i := _rows to old_rows -1 do
        FreeObject( arRows[ i ] ) ;

      SetLength( arRows, _rows );

      for i := old_rows to _rows-1 do begin
        arRows[i] := TGIS_PVlGridRow.Create( self ) ;
      end;
    end;

    if ( ActiveCell.X > ColumnsCount )
       or
       ( ActiveCell.X > RowsCount )
    then
      rActiveCell := Point( -1, -1 ) ;

    ScrollBottom;

    RealignControl;
  finally
    EndUpdate ;
  end;
end;

function TGIS_PvlGrid.AddRow
  : TGIS_PVlGridRow ;
begin
  SetLength( arRows, length( arRows ) + 1 ) ;

  arRows[ high( arRows ) ] := TGIS_PVlGridRow.Create( self ) ;
  Result := arRows[ high( arRows ) ] ;

  if iLock > 0 then
    exit ;

  ScrollBy(0,0);

  RealignControl;
end;

function TGIS_PvlGrid.AddColumn
  : TGIS_PVlGridColumn ;
begin
  SetLength( arColumns, length( arColumns ) + 1 ) ;

  arColumns[ high( arColumns ) ] := TGIS_PVlGridColumn.Create( self ) ;
  arColumns[ high( arColumns ) ].sCaption := spreasheetColumn( high( arColumns ) ) ; //?
  Result := arColumns[ high( arColumns ) ] ;

  if iLock > 0 then
    exit ;

  ScrollBy(0,0);

  RealignControl;
end;

procedure TGIS_PvlGrid.DeleteRow(
const _row : Integer
  );
var
  i : Integer ;
 begin
  if ( _row < 1 ) or ( _row > RowsCount ) then
    exit ;

  FreeObject( arRows[ _row-1 ] );

  if _row < RowsCount then begin
  for i := _row-1 to RowsCount-2 do begin
    arRows[ i ] := arRows[i+1] ;
      end;
    end;

  if ActiveCell.Y = _row then
  ActiveCell := Point(-1,-1);

  SetLength( arRows, length( arRows ) - 1 ) ;

  ScrollToActiveCell ;
end;

function TGIS_PvlGrid.CellByLocation(
  const _x : Integer;
  const _y : Integer
  ) : TPoint ;
var
  icol : Integer ;
  irow : Integer ;
  r    : TRect   ;
begin
  Result := Point( -1, -1 );

  // upper left corener
  if ( ColumnsHeader <> TGIS_PVlGridHeader.None )
     or
     ( RowsHeader <> TGIS_PVlGridHeader.None )
  then begin
    r := CellRect( 0, 0 ) ;

    if ( _x > r.Left   ) and
       ( _x < r.Right  ) and
       ( _y > r.Top    ) and
       ( _y < r.Bottom )
    then begin
      exit ;
    end;
  end;

  // columns header
  if ColumnsHeader <> TGIS_PVlGridHeader.None then begin
    for icol := visibleCells.Left to visibleCells.Right do begin
      r := CellRect( icol, 0 ) ;

      if ( _x > r.Left   ) and
         ( _x < r.Right  ) and
         ( _y > r.Top    ) and
         ( _y < r.Bottom )
      then begin
        //? fortra
        Result := Point( icol, 0 ) ;
        exit ;
      end;
    end;
  end;

    // rows header
  if RowsHeader <> TGIS_PVlGridHeader.None then begin
    for irow := visibleCells.Top to visibleCells.Bottom do begin
      r := CellRect( 0, irow ) ;

      if ( _x > r.Left   ) and
         ( _x < r.Right  ) and
         ( _y > r.Top    ) and
         ( _y < r.Bottom )
      then begin
        Result := Point( 0, irow ) ;
        exit ;
      end;
    end;
  end;

  // cells
  for icol := visibleCells.Left to visibleCells.Right do begin
    for irow := visibleCells.Top to visibleCells.Bottom do begin
       r := CellRect( icol, irow ) ;

       if ( _x > r.Left   ) and
          ( _x < r.Right  ) and
          ( _y > r.Top    ) and
          ( _y < r.Bottom )
       then begin
         Result := Point( icol, irow ) ;
        exit ;
       end;
     end;
   end;
end;

function TGIS_PvlGrid.CellRect(
  const _col : Integer;
  const _row : Integer
  ) : TRect ;
var
  icol : Integer;
  irow : Integer;
  w    : Integer;
  xoff : Integer;
  yoff : Integer;
begin
  if ( _col > 0 ) and
     ( _row > 0 ) and
     (
     ( _col < visibleCells.Left   ) or
       ( _col > visibleCells.Right  ) or
       ( _row < visibleCells.Top    ) or
       ( _row > visibleCells.Bottom )
     )
  then begin
    // bruce force

    xoff := - rScrollPos.X + leftMargin ;
    yoff := - rScrollPos.Y + topMargin ;

    for icol := startColumn to _col-1 do begin
      if not Column[icol].Visible then
        continue;
      xoff := xoff + Column[icol].Width;
    end;

    for irow := startRow to _row-1 do begin
      yoff := yoff + frowsHeight;
    end;

    Result := Rect( xoff, yoff, xoff + Column[_col].Width, yoff + frowsHeight );

    exit ;
  end;

  if _col = 0 then begin
    iLastXOffset := 0 ;
    iPrevCol := -1;
    end
  else
  if _col = iPrevCol then begin
    // do nothing
//    iLastXOffset := 0;
  end
  else
  if ( iPrevCol > 0 ) and ( _col = iPrevCol + 1 ) then begin
    // easy - next col
    iLastXOffset := iLastXOffset + Column[iPrevCol].Width;
    iPrevCol := iPrevCol + 1;
  end
  else begin
   iLastXOffset := visibleCellsOrigin.X;


   // offset must be recalculated
   iLastXOffset := visibleCellsOrigin.X;
   for icol := visibleCells.Left to _col-1 do
     iLastXOffset := iLastXOffset + Column[icol].Width;
   iPrevCol := _col;
  end;

  if ( _col = 0 ) and ( RowsHeader = TGIS_PVlGridHeader.First ) and ( ColumnsCount > 0 ) then
    w := Column[0].Width
  else
    w := Column[_col].Width;

  if _row = 0 then
    yoff := 0
  else
    yoff := visibleCellsOrigin.Y + (_row - visibleCells.Top ) * frowsHeight ;

  Result := Rect( RoundS(  iLastXOffset * Context.PPIFix ),
                  RoundS(  yoff         * Context.PPIFix ),
                  RoundS( ( iLastXOffset + w   ) * Context.PPIFix ),
                  RoundS( ( yoff + frowsHeight ) * Context.PPIFix )
                ) ;
end;

function TGIS_PvlGrid.CellEditRect
  : TRect ;
var
  r : TRect ;
begin
  Result := Rect( 0, 0, 0, 0 );

  // update edit pos
  if (ActiveCell.X < 1) or (ActiveCell.Y < 1) then
    exit ;

  r := CellRect( ActiveCell.X, ActiveCell.Y );

  if r.Top < topMargin then
    exit ;

  if r.Left < leftMargin then
    exit;

  if r.Right > fget_ClientWidth * Context.PPIFix then
    r := Rect( r.Left, r.Top, RoundS(fget_ClientWidth * Context.PPIFix), r.Bottom );
  if r.Bottom > fget_ClientHeight * Context.PPIFix then
    r := Rect( 0, 0, 0, 0 );

  Result := r;
end;


procedure TGIS_PvlGrid.ScrollTo(
  const _x : Integer;
  const _y : Integer
) ;
var
  i : Integer ;
  x : Integer ;
  y : Integer ;
  xoff : Integer ;
  yoff : Integer ;

 begin
  if _y < 0 then
    y := 0
  else
  if ( ScrollMax.Y > 0 ) and ( _y > ( ScrollMax.Y - ScrollRect.Height ) ) then
    y := Max( 0, ScrollMax.Y - ScrollRect.Height )
  else
    y := _y ;

  if _x < 0 then
    x := 0
  else
  if ( ScrollMax.X > 0 ) and ( _x > ( ScrollMax.X - ScrollRect.Width ) ) then
    x := Max( 0, ScrollMax.X - ScrollRect.Width )
  else
    x := _x ;

  iPrevCol := -1;
  rScrollPos := Point( x, y ) ;

  xoff := - x + leftMargin ;
  yoff := - y + topMargin ;

  visibleCells := Rect( -1, visibleCells.Top, -2, visibleCells.Bottom );
  for i := startColumn to ColumnsCount do begin
    if not Column[i].Visible then
      continue;

    xoff := xoff + Column[i].Width;
    if ( xoff > 0 ) and ( visibleCells.Left < 0 ) then begin
      visibleCells := Rect( i, visibleCells.Top, visibleCells.Right, visibleCells.Bottom );
      visibleCellsOrigin.X := xoff - Column[i].Width ;
    end;
    visibleCells := Rect( visibleCells.Left, visibleCells.Top, i, visibleCells.Bottom );

    if ( xoff > Width ) then
      break;
  end;

  visibleCells := Rect( visibleCells.Left, -1, visibleCells.Right, -2 );
  for i := startRow to RowsCount do begin
    yoff := yoff + RoundS( frowsHeight  );
    if ( yoff >= 0 ) and ( visibleCells.Top < 0 ) then begin
      visibleCells := Rect( visibleCells.Left, i, visibleCells.Right, visibleCells.Bottom );
      visibleCellsOrigin.Y := yoff - RoundS( frowsHeight ) ;
    end;
    visibleCells := Rect( visibleCells.Left, visibleCells.Top, visibleCells.Right, i );

    if ( yoff > Height ) then
      break;
  end;

  RealignControl ;
  InvalidateControl;
end;

procedure TGIS_PvlGrid.ScrollBy(
  const _x : Integer;
  const _y : Integer
  ) ;
begin
  ScrollTo( rScrollPos.X + _x, rScrollPos.Y + _y ) ;
end;

function TGIS_PvlGrid.ScrollToActiveCell
  : Boolean ;
var
  r : TRect ;
  bscroll : Boolean ;
begin
  if ( ActiveCell.X < 1 )            or
     ( ActiveCell.X > ColumnsCount ) or
     ( ActiveCell.Y < 1 )            or
     ( ActiveCell.Y > RowsCount    )
  then begin
    Result := False ;
    ScrollBy(0,0);
    exit ;
  end;

  bscroll := False ;

  r := CellRect( ActiveCell.X, ActiveCell.Y );
  if r.Right > fget_ClientWidth then begin
    ScrollBy( ( r.Right - fget_ClientWidth  ), 0 );
    bscroll := True ;
  end;

  r := CellRect( ActiveCell.X, ActiveCell.Y );
  if r.Left < leftMargin then begin
    ScrollBy( r.Left - leftMargin, 0 );
    bscroll := True ;
  end;

  r := CellRect( ActiveCell.X, ActiveCell.Y );
  if r.Bottom > fget_ClientHeight then begin
    ScrollBy( 0, ( r.Bottom - fget_ClientHeight ) ) ;
    bscroll := True ;
  end;

  r := CellRect( ActiveCell.X, ActiveCell.Y );
  if r.Top < topMargin then begin
    ScrollBy( 0, r.Top - topMargin );
    bscroll := True ;
  end;

  if not bscroll then
    ScrollBy( 0, 0 );

  InvalidateControl ;
  Result := True ;
end;

function TGIS_PvlGrid.ScrollRight
  : Boolean ;
var
  oldval : TPoint ;
 begin
  Result := False ;
  oldval := ActiveCell;

  while ActiveCell.X < ColumnsCount do begin
    ActiveCell := Point( ActiveCell.X + 1, ActiveCell.Y ) ;
    if Column[ ActiveCell.X ].Visible then begin
      if RowSelect then
        if CellRect( ActiveCell.X, ActiveCell.Y ).Right < Width then
          continue ;
      Result := True ;
      break ;
    end;
  end;

  if not Result then begin
    ActiveCell := oldval;
    exit ;
  end;

  ScrollToActiveCell;
end;

function TGIS_PvlGrid.ScrollLeft
: Boolean ;
  var
oldval : TPoint ;
  begin
Result := False ;
  oldval := ActiveCell;
  while ActiveCell.X > startColumn do begin
  ActiveCell := Point( ActiveCell.X - 1, ActiveCell.Y ) ;
    if Column[ ActiveCell.X ].Visible then begin
    if RowSelect then
      if CellRect( ActiveCell.X, ActiveCell.Y ).Left > leftMargin then
        continue ;
          Result := True ;
      break ;
      end;
    end;
  if not Result then begin
  ActiveCell := oldval;
    exit ;
    end;

  ScrollToActiveCell;
  end;

function TGIS_PvlGrid.ScrollHome
  : Boolean ;
begin
  if ActiveCell.X = 1 then begin
    Result := False ;
    exit;
  end;

  ActiveCell := Point( 1, ActiveCell.Y ) ;

  Result := ScrollToActiveCell;
end;

function TGIS_PvlGrid.ScrollEnd
  : Boolean ;
begin
  if ActiveCell.X = ColumnsCount then begin
    Result := False ;
    exit;
  end;

  ActiveCell := Point( ColumnsCount, ActiveCell.Y ) ;

  Result := ScrollToActiveCell;
end;

function TGIS_PvlGrid.ScrollDown
  : Boolean ;
var
  oldval : TPoint ;
begin
  Result := False ;
  oldval := ActiveCell;

  while ActiveCell.Y < RowsCount do begin
    ActiveCell := Point( ActiveCell.X, ActiveCell.Y+1 ) ;
    Result := True ;
    break ;
  end;

  if not Result then begin
    ActiveCell := oldval;
    exit ;
  end;

  ScrollToActiveCell;
end;

function TGIS_PvlGrid.ScrollUp
  : Boolean;
var
  oldval : TPoint ;
begin
  Result := False ;
  oldval := ActiveCell;

  while ActiveCell.Y > 1 do begin
    ActiveCell := Point( ActiveCell.X, ActiveCell.Y-1 ) ;
    Result := True ;
    break ;
  end;

  if not Result then begin
    ActiveCell := oldval;
    exit ;
  end;

  ScrollToActiveCell;
end;

function TGIS_PvlGrid.ScrollPageDown
  : Boolean ;
var
  irow : Integer ;
begin
  irow := ActiveCell.Y + ( fget_ClientHeight - topMargin ) div frowsHeight ;

  if irow > RowsCount then
    irow := RowsCount ;

  ActiveCell := Point( ActiveCell.X, irow ) ;
  Result := ScrollToActiveCell;
end;

function TGIS_PvlGrid.ScrollPageUp
  : Boolean ;
var
  irow : Integer ;
begin
  irow := ActiveCell.Y - ( fget_ClientHeight - topMargin ) div frowsHeight ;

  if irow < 1 then
    irow := 1 ;

  ActiveCell := Point( ActiveCell.X, irow ) ;
  Result := ScrollToActiveCell;
end;

function TGIS_PvlGrid.ScrollTop
  : Boolean ;
begin
  if ActiveCell.Y = 1 then begin
    Result := False ;
    exit;
  end;

  ActiveCell := Point( ActiveCell.X, 1 ) ;
  Result := ScrollToActiveCell;
end;

function TGIS_PvlGrid.ScrollBottom
  : Boolean ;
begin
  if ActiveCell.Y = RowsCount then begin
    Result := False ;
    exit;
  end;

  RealignControl;
  ActiveCell := Point( ActiveCell.X, RowsCount ) ;
  Result := ScrollToActiveCell;
end;

function TGIS_PvlGrid.ScrollNext
  : Boolean ;
begin
  Result := ScrollRight;
  if Result then begin
    Result := ScrollDown;
    if Result then
      Result := ScrollHome;
  end;
end;

function TGIS_PvlGrid.ScrollPrevious
  : Boolean ;
begin
  Result := ScrollLeft;
  if not Result then begin
    Result := ScrollUp;
    if Result then
      Result := ScrollEnd ;
  end;
end;

function TGIS_PvlGrid.fget_ClientWidth
  : Integer;
begin
  Result := PlatformControl.fget_ClientWidth;
end;

function TGIS_PvlGrid.fget_ClientHeight
  : Integer;
begin
  Result := PlatformControl.fget_ClientHeight;
end;

procedure TGIS_PvlGrid.SetFocus ;
begin
  PlatformControl.SetFocus ;
end;

function TGIS_PvlGrid.MeasureText(
  const _txt    : String
) : TPoint ;
begin
  Result := PlatformControl.MeasureText( _txt );
end;

procedure TGIS_PvlGrid.DrawCell(
  const _col    : Integer ;
  const _row    : Integer ;
  const _txt    : String;
  const _canvas : TObject
  ) ;
begin
  PlatformControl.DrawCell( _col, _row, _txt, _canvas ) ;
end;

procedure TGIS_PvlGrid.DrawAll(
  const _canvas : TObject
) ;
var
  icol : Integer ;
  irow : Integer ;
  val : Variant  ;
begin
  PlatformControl.DrawAll( _canvas );

  if iLock > 0  then
    exit ;

  if ColumnsCount < 1 then
    exit;
  if ( RowsCount < 1 ) and not ( ColumnsHeader = TGIS_PVlGridHeader.Auto ) then
    exit;


  for icol := visibleCells.Left to visibleCells.Right do begin
    for irow := visibleCells.Top to visibleCells.Bottom do begin
      if ( icol = ActiveCell.X ) and ( irow = ActiveCell.Y ) then
        continue;
      DrawCell( icol, irow,
                formattedCell( icol, Row[irow].Column[icol] ),
                _canvas
              ) ;
    end;
  end;

  if ( ActiveCell.X > 0 ) and ( ActiveCell.Y > 0 ) then begin
    DrawCell( ActiveCell.X, ActiveCell.Y,
              formattedCell( ActiveCell.X, Row[ActiveCell.Y].Column[ActiveCell.X]),
              _canvas
            ) ;
  end;

  if ColumnsHeader <> TGIS_PVlGridHeader.None then begin
    if ColumnsHeader = TGIS_pvlGridHeader.Auto then
    begin
      for icol := 1 to ColumnsCount do begin
        val := Column[icol].Caption ;
        {$IFDEF OXYGENE}
          DrawCell( icol, 0, val.ToString(), _canvas ) ;
        {$ELSE}
          DrawCell( icol, 0, val, _canvas ) ;
        {$ENDIF}
      end;
    end else begin
      for icol := visibleCells.Left to visibleCells.Right do begin
        // header line
        if ColumnsHeader = TGIS_PVlGridHeader.First then
          val := Row[1].Column[icol]
        else
          val := '?';

        {$IFDEF OXYGENE}
          DrawCell( icol, 0, val.ToString(), _canvas ) ;
        {$ELSE}
          DrawCell( icol, 0, val, _canvas ) ;
        {$ENDIF}
      end;
    end;
  end;

  if RowsHeader <> TGIS_PVlGridHeader.None then begin
    for irow := visibleCells.Top to visibleCells.Bottom do begin
      // header line
      if RowsHeader = TGIS_PVlGridHeader.Auto then
        val := irow
      else
      if RowsHeader = TGIS_PVlGridHeader.First then
        val := Row[irow].Column[1]
      else
        val := '?';

      {$IFDEF JAVA}
        if val = nil then
          val := '' ;
      {$ENDIF}
      DrawCell( 0, irow, formattedCell( 0, val ), _canvas ) ;
    end;
  end;

  if ( ColumnsHeader <> TGIS_PVlGridHeader.None ) and ( RowsHeader <> TGIS_PVlGridHeader.None ) then
    DrawCell( 0, 0,  '-', _canvas );
end;

procedure TGIS_PvlGrid.InvalidateControl;
begin
  PlatformControl.InvalidateControl ;
end;

procedure TGIS_PvlGrid.RealignControl;
var
  i: Integer;
  wtmp: Integer;
  htmp: Integer;
  bfirstfit: Boolean;
begin
  // recalc sizes
  bfirstfit := True;
  wtmp := -ScrollRect.Left;
  for i := startColumn to ColumnsCount do begin
    if Column[i].Visible then begin
      if Column[i].FitWidth and bfirstfit then begin
        Column[i].iWidth := ScrollRect.Width - wtmp;
        bfirstfit := False;
      end;
      wtmp := wtmp + Column[i].Width;
    end;
  end;

  htmp := RowsCount * RowsHeight - ScrollRect.Top;

  rScrollMax := Point(wtmp, htmp);

  PlatformControl.RealignControl;
end;


procedure TGIS_PvlGrid.BeginEdit(
  const _txt  : String;
  const _char : Char
  ) ;
var
  val : Variant ;
begin
  if ReadOnly then
    exit;

  if RowSelect then
    exit;

  if Column[ActiveCell.X].ReadOnly then
    exit;

  if assigned( evnBeginEditEvent ) then
    evnBeginEditEvent( Self ) ;

  val := Row[ActiveCell.Y].Column[ActiveCell.X] ;

  if _char <> #0 then begin
    try
      case arColumns[ ActiveCell.X ].FieldType of
        TGIS_FieldType.String:
          val := _char ;
        TGIS_FieldType.Number:
          val := VarToInt64( _char );
        TGIS_FieldType.Float:
          val := VarToDouble( _char );
        TGIS_FieldType.Boolean:
          val := VarToBoolean( _char );
        TGIS_FieldType.Date:
          val := VarToDateTime( _char );
      end;
    except
      exit ;
    end;
  end;

  PlatformControl.BeginEdit( formattedCell( ActiveCell.X, val ), #0 ) ;
end;

procedure TGIS_PvlGrid.EndEdit(
  const _commit : Boolean
) ;
begin
  PlatformControl.EndEdit( _commit ) ;

  if assigned( evnEndEditEvent ) then
    evnEndEditEvent( Self ) ;
end;

procedure TGIS_PvlGrid.First ;
begin
  PlatformControl.First ;
end;

procedure TGIS_PvlGrid.MoveBy(
  const _dist: Integer
) ;
begin
  PlatformControl.MoveBy( _dist ) ;
end;

{$ENDREGION 'TGIS_PvlGrid'}

//==================================== END =====================================
end.

