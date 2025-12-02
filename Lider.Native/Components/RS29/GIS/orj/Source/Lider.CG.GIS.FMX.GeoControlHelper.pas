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

unit FMX.GisControlHelper ;
{$HPPEMIT '#pragma link "FMX.GisControlHelper"'}

{$INCLUDE GisInclude.inc}
interface

uses
  System.Classes,
  System.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Types,
  FMX.Edit;

type
  /// <summary>
  ///   Basic class helper for FMX control.
  /// </summary>
  TControlHelper = class helper for TControl

    /// <summary>
    ///   Calculate control size, even when control has been scaled.
    /// </summary>
    /// <returns>
    ///   calculated size
    /// </returns>
    public function DesignSize : TSizeF ;

    /// <summary>
    ///   Calculate right bottom corner of the control.
    /// </summary>
    /// <returns>
    ///   right bottom corner
    /// </returns>
    public function RightBottomCorner : TPointF ;

    /// <summary>
    ///   Align control vertically to a reference control.
    /// </summary>
    /// <param name="_control">
    ///   reference control
    /// </param>
    public procedure AlignVertically( _control : TControl ) ;
  end;

  /// <summary>
  ///   Class helper for FMX TEdit control.
  /// </summary>
  TEditHelper = class helper for TEdit

    /// <summary>
    ///   Modify edit box on mobile to make them bit longer.
    /// </summary>
    public procedure FixSize ;

    procedure Hook ;

    procedure OnChangeTrackingHandler(_sender: TObject);

    procedure OnChangeHandler(_sender: TObject);
  end;

  /// <summary>
  ///   Class helper for FMX TLabel control.
  /// </summary>
  TLabelHelper = class helper for TLabel

    /// <summary>
    ///   Modify labels on mobile to make them bit smaller and not wrapable.
    /// </summary>
    public procedure FixSize ;
  end;

  /// <summary>
  ///   Class helper for FMX TRadioButton control.
  /// </summary>
  TRadioButtonHelper = class helper for TRadioButton

    /// <summary>
    ///   Modify labels on mobile to make them bit smaller and not wrapable.
    /// </summary>
    public procedure FixSize ;
  end;

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
  procedure PlaceControl        ( _bdmode          : TBiDiMode ;
                                  _previousControl : TControl  ;
                                  _control         : TControl  ;
                                  _offset          : Single    ;
                                  _width           : Single
                                ) ;

//##############################################################################
implementation

uses
  FMX.GisModalForm ;

  function TControlHelper.DesignSize : TSizeF ;
  begin
    Result.cx :=  Size.Width  * Scale.X ;
    Result.cy :=  Size.Height * Scale.X ;
  end ;

  function TControlHelper.RightBottomCorner
    : TPointF ;
  begin
    Result.X := Position.X + DesignSize.Width  ;
    Result.Y := Position.Y + DesignSize.Height ;
  end ;

  procedure TControlHelper.AlignVertically(
    _control : TControl
  ) ;
  begin
    Position.Y := _control.Position.Y -
                  ( DesignSize.Height - _control.DesignSize.Height  ) / 2 ;
  end ;

  procedure TEditHelper.FixSize;
  begin
    if Tag = 1 then exit ;
    Tag := 1 ;

    {$IFDEF IOS}
      Size.Width := Size.Width * 1.2 ;
    {$ENDIF}
    {$IFDEF ANDROID}
      Size.Width := Size.Width * 1.2 ;
    {$ENDIF}
  end;

  procedure TEditHelper.Hook;
  begin
    {$IFDEF ANDROID}
      Assert( not Assigned( OnChangeTracking ) ) ;

      TagObject := TObject( @OnChange ) ;
      TagString := Text ;

      OnChange := OnChangeHandler ;
      OnChangeTracking := OnChangeTrackingHandler ;
    {$ENDIF}
  end;

  procedure TEditHelper.OnChangeHandler(
    _sender: TObject
  ) ;
  var
    proc : procedure( _sender : TObject ) ;
  begin
    TagString := Text ;

    if Assigned( TagObject ) then begin
      @proc := Pointer(TagObject) ;
      proc( _sender )
    end;
  end;

  procedure TEditHelper.OnChangeTrackingHandler(
    _sender: TObject
  );
  var
    i         : Integer ;
    key_val   : Word ;
    key_char  : Char ;
    shift     : TShiftState ;
    old_caret : Integer ;
  begin
    old_caret := CaretPosition ;
    key_char  := Char(0) ;

    for i:=1 to Length( Text ) do begin
      if i > Length(TagString) then begin
        key_char := Text[i] ;
      end
      else begin
        if Text[i] <> TagString[i] then begin
          key_char := Text[i] ;
        end;
      end ;

      if Ord( key_char ) <> 0  then begin
        key_val := Ord( key_char ) ;
        shift := [] ;

        if Assigned( OnKeyDown ) then begin
          OnKeyDown( _sender, key_val, key_char, shift );
          if key_char = #0 then begin
            Text := TagString ;
            CaretPosition := old_caret - 1;
            exit ;
          end;
        end;
        break ;
      end;
    end;

    TagString := Text ;
  end;

  procedure TLabelHelper.FixSize;
  begin
    if Tag = 1 then exit ;
    Tag := 1 ;

    {$IFDEF IOS}
      TextSettings.WordWrap := False ;
      Size.Width := Size.Width * 1.2 ;
      Scale.X := 0.8 ;
      Scale.Y := 0.8 ;
    {$ENDIF}
    {$IFDEF ANDROID}
      TextSettings.WordWrap := False ;
      TextSettings.Trimming := TTextTrimming.None ;
      Size.Width := Size.Width * 1.2 ;
      Scale.X := 0.8 ;
      Scale.Y := 0.8 ;
    {$ENDIF}
  end;

  procedure TRadioButtonHelper.FixSize;
  begin
    if Tag = 1 then exit ;
    Tag := 1 ;

    {$IFDEF IOS}
      Size.Width := Size.Width * 1.5 ;
    {$ENDIF}
    {$IFDEF ANDROID}
      Size.Width := Size.Width * 1.5 ;
    {$ENDIF}
  end;

  procedure PlaceControl(
    _bdmode          : TBiDiMode ;
    _previousControl : TControl  ;
    _control         : TControl  ;
    _offset          : Single   ;
    _width           : Single
  ) ;
  var
    x : Single ;
    offset : Single ;
    right_to_left : Boolean ;
    parent_width  : Single ;
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

    if _control.Parent is TGIS_ModalForm then
      parent_width := TGIS_ModalForm(_control.Parent).ClientWidth
    else
      parent_width := TControl(_control.Parent).Width ;

    if Assigned( _previousControl ) then begin
      if right_to_left then
        x := _previousControl.Position.X
      else
        x := _previousControl.Position.X + _previousControl.Width ;
    end
    else begin
      if right_to_left then
        x := parent_width
      else
        x := 0 ;
    end ;

    if _width = -1 then
      _control.Width := parent_width - 2 * offset
    else
      _control.Width := _width ;

    if right_to_left then
      _control.Position.X := x - offset - _control.Width
    else
      _control.Position.X := x + offset ;

    _control.RecalcSize ;
  end ;

{==================================== END =====================================}
end.
