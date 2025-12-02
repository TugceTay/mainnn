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
  VCL Control Helper.
}

unit VCL.GisControlHelper ;
{$HPPEMIT '#pragma link "VCL.GisControlHelper"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Classes,
  System.Types,
  VCL.Controls;

  /// <summary>
  ///   Align control vertically to a reference control.
  /// </summary>
  /// <param name="_control">
  ///   control to align
  /// </param>
  /// <param name="_referenceControl">
  ///   reference control
  /// </param>
  procedure AlignVertically( _control          : TControl ;
                             _referenceControl : TControl
                           ) ;

  /// <summary>
  ///   Calculate right bottom corner of the control.
  /// </summary>
  /// <param name="_control">
  ///   control to calculate
  /// </param>
  /// <returns>
  ///   Right bottom corner.
  /// </returns>
  function RightBottomCorner( _control : TControl ) : TPoint ;

  /// <summary>
  ///   Calculate control's position according to current BiDiMode.
  /// </summary>
  /// <param name="_bdmode">
  ///   bidirectional mode
  /// </param>
  /// <param name="_previousControl">
  ///   if not nil, position is calculated relative to it
  /// </param>
  /// <param name="_control">
  ///   control to place
  /// </param>
  /// <param name="_offset">
  ///   offset relative to border or previous control
  /// </param>
  /// <param name="_width">
  ///   control width; if -1 then calculated 'as long as possible'
  /// </param>
  procedure PlaceControl( _bdmode          : TBiDiMode ;
                          _previousControl : TControl  ;
                          _control         : TControl  ;
                          _offset          : Integer   ;
                          _width           : Integer
                        ) ;

//##############################################################################
implementation

  procedure AlignVertically(
    _control          : TControl ;
    _referenceControl : TControl
  ) ;
  begin
    _control.Top := _referenceControl.Top -
                    ( _control.Height - _referenceControl.Height  ) div 2 ;
  end ;

  function RightBottomCorner(
    _control : TControl
  ) : TPoint ;
  begin
    Result.X := _control.Left + _control.Width  ;
    Result.Y := _control.Top  + _control.Height ;
  end ;

  procedure PlaceControl(
    _bdmode          : TBiDiMode ;
    _previousControl : TControl  ;
    _control         : TControl  ;
    _offset          : Integer   ;
    _width           : Integer
  ) ;
  var
    x : Integer ;
    offset : Integer ;
    right_to_left : Boolean ;
  begin
    if _bdmode = bdRightToLeft then begin
      if _offset >= 0 then
        right_to_left := True
      else
        right_to_left := False ;
    end else begin
      if _offset >= 0 then
        right_to_left := False
      else
        right_to_left := True ;
    end;
    offset := Abs(_offset) ;

    if Assigned( _previousControl ) then begin
      if right_to_left then
        x := _previousControl.Left
      else
        x := _previousControl.Left + _previousControl.Width ;
    end
    else begin
      if right_to_left then
        x := _control.Parent.ClientWidth
      else
        x := 0 ;
    end ;

    if _width = -1 then
      _control.Width := _control.Parent.ClientWidth - 2 * offset
    else
      _control.Width := _width ;

    if right_to_left then
      _control.Left := x - offset - _control.Width
    else
      _control.Left := x + offset
  end ;

{==================================== END =====================================}
end.

