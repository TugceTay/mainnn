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
  Fourier transform.
}

{$IFDEF DCC}
  unit GisFourierTransform ;
  {$HPPEMIT '#pragma link "GisFourierTransform"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,

    GisBaseObject,
    GisClasses,
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
  /// <summary>
  ///   Encapsulation of one- and two-dimensional Discrete and Fast Fourier
  ///   Transform (DFT and FFT) for a real-valued signal. FFT uses the
  ///   Cooley-Tukey radix-2 algorithm.
  /// </summary>
  TGIS_FourierTransform = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_Object )
    private
      FRe              : TGIS_SingleArray ;
      FIm              : TGIS_SingleArray ;
      FRe2D            : TGIS_GridArray ;
      FIm2D            : TGIS_GridArray ;
      FShifted         : Boolean ;

    private
      oSignal          : TGIS_SingleArray ;
      oSignal2D        : TGIS_GridArray ;
      oRe1D            : TGIS_GridArray ;
      oIm1D            : TGIS_GridArray ;
      oSinCos          : TGIS_GridArray ;
      iWidth           : Integer ;
      iHeight          : Integer ;
      iSize            : Integer ;
      iLevel           : Integer ;
      iState           : Integer ;
      b2D              : Boolean ;
      bColumns         : Boolean ;
      bInverse         : Boolean ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      busyEventManager : TGIS_BusyEventManager ;

    private
      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

      function  fget_Modulus    ( const _i : Integer
                                ) : Single ;
      function  fget_Argument   ( const _i : Integer
                                ) : Single ;
      function  fget_Modulus2D  ( const _y : Integer ;
                                  const _x : Integer
                                ) : Single ;
      function  fget_Argument2D ( const _y : Integer ;
                                  const _x : Integer
                                ) : Single ;

    private
      procedure prepSinCos       ;
      procedure getSinCos        ( const _lvl  : Integer ;
                                   const _idx  : Integer ;
                                     var _sin  : Single ;
                                     var _cos  : Single
                                 ) ;
      function  calcModulus      ( const _re   : Single ;
                                   const _im   : Single
                                 ) : Single ;
      function  calcArgument     ( const _re   : Single ;
                                   const _im   : Single
                                 ) : Single ;
      function  compFast         ( const _lvl  : Integer ;
                                   const _idx  : Integer ;
                                   const _row  : Integer
                                 ) : TObject ;
      function  copyWindow       ( const _signal : TGIS_SingleArray ;
                                   const _start  : Integer ;
                                   const _end    : Integer
                                 ) : TGIS_SingleArray ;
      function  copyWindow2D     ( const _signal : TGIS_GridArray ;
                                   const _bounds : TRect
                                 ) : TGIS_GridArray ;
      procedure shiftFFT         ;
      procedure shiftFFT2D       ;
      function  invertDiscrete   : TGIS_SingleArray ;
      function  invertFast       : TGIS_SingleArray ;
      function  invertDiscrete2D : TGIS_GridArray ;
      function  invertFast2D     : TGIS_GridArray ;

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    public
      /// <summary>
      ///   Computes the one-dimensonal Fourier transform of the provided
      ///   real-valued signal using the discrete algorithm (DFT).
      /// </summary>
      /// <param name="_signal">
      ///   signal as an array of single-precision values
      /// </param>
      procedure RunDiscrete      ( const _signal : TGIS_SingleArray
                                 ) ; overload ;
      /// <summary>
      ///   Computes the one-dimensonal Fourier transform of the provided
      ///   real-valued signal using the discrete algorithm (DFT).
      /// </summary>
      /// <param name="_signal">
      ///   signal as an array of single-precision values
      /// </param>
      /// <param name="_start">
      ///   start index of the signal window
      /// </param>
      /// <param name="_end">
      ///   end index of the signal window
      /// </param>
      procedure RunDiscrete      ( const _signal : TGIS_SingleArray ;
                                   const _start  : Integer ;
                                   const _end    : Integer
                                 ) ; overload ;
      /// <summary>
      ///   Computes the one-dimensonal Fourier transform of the provided
      ///   real-valued signal using the fast algorithm (FFT).
      /// </summary>
      /// <param name="_signal">
      ///   signal as an array of single-precision values
      /// </param>
      procedure RunFast          ( const _signal : TGIS_SingleArray
                                 ) ; overload ;
      /// <summary>
      ///   Computes the one-dimensonal Fourier transform of the provided
      ///   real-valued signal using the fast algorithm (FFT).
      /// </summary>
      /// <param name="_signal">
      ///   signal as an array of single-precision values
      /// </param>
      /// <param name="_start">
      ///   start index of the signal window
      /// </param>
      /// <param name="_end">
      ///   end index of the signal window
      /// </param>
      procedure RunFast          ( const _signal : TGIS_SingleArray ;
                                   const _start  : Integer ;
                                   const _end    : Integer
                                 ) ; overload ;
      /// <summary>
      ///   Inverts the one-dimensonal Fourier transform.
      /// </summary>
      /// <returns>
      ///   reconstructed signal as an array of single-precision values
      /// </returns>
      function  Invert           : TGIS_SingleArray ;
      /// <summary>
      ///   Computes the two-dimensonal Fourier transform of the provided
      ///   real-valued signal using the discrete algorithm (DFT).
      /// </summary>
      /// <param name="_signal">
      ///   signal as a two-dimensional array of single-precision values
      /// </param>
      procedure RunDiscrete2D    ( const _signal : TGIS_GridArray
                                 ) ; overload ;
      /// <summary>
      ///   Computes the two-dimensonal Fourier transform of the provided
      ///   real-valued signal using the discrete algorithm (DFT).
      /// </summary>
      /// <param name="_signal">
      ///   signal as a two-dimensional array of single-precision values
      /// </param>
      /// <param name="_bounds">
      ///   index bounds of the signal window
      /// </param>
      procedure RunDiscrete2D    ( const _signal : TGIS_GridArray ;
                                   const _bounds : TRect
                                 ) ; overload ;
      /// <summary>
      ///   Computes the two-dimensonal Fourier transform of the provided
      ///   real-valued signal using the fast algorithm (FFT).
      /// </summary>
      /// <param name="_signal">
      ///   signal as a two-dimensional array of single-precision values
      /// </param>
      procedure RunFast2D        ( const _signal : TGIS_GridArray
                                 ) ; overload ;
      /// <summary>
      ///   Computes the two-dimensonal Fourier transform of the provided
      ///   real-valued signal using the fast algorithm (FFT).
      /// </summary>
      /// <param name="_signal">
      ///   signal as a two-dimensional array of single-precision values
      /// </param>
      /// <param name="_bounds">
      ///   index bounds of the signal window
      /// </param>
      procedure RunFast2D        ( const _signal : TGIS_GridArray ;
                                   const _bounds : TRect
                                 ) ; overload ;
      /// <summary>
      ///   Inverts the two-dimensonal Fourier transform.
      /// </summary>
      /// <returns>
      ///   reconstructed signal as a two-dimensional array of single-precision
      ///   values
      /// </returns>
      function  Invert2D         : TGIS_GridArray ;
    public
      /// <summary>
      ///   Real part of the one-dimensional Fourier transform.
      /// </summary>
      property Real        : TGIS_SingleArray
                             read  FRe ;
      /// <summary>
      ///   Imaginary part of the one-dimensional Fourier transform.
      /// </summary>
      property Imaginary   : TGIS_SingleArray
                             read  FIm ;
      /// <summary>
      ///   Modulus of the one-dimensional Fourier transform.
      /// </summary>
      /// <param name="_i">
      ///   index of the transform value
      /// </param>
      property Modulus     [const _i : Integer]
                           : Single
                             read  fget_Modulus ;
      /// <summary>
      ///   Argument of the one-dimensional Fourier transform.
      /// </summary>
      /// <param name="_i">
      ///   index of the transform value
      /// </param>
      property Argument    [const _i : Integer]
                           : Single
                             read  fget_Argument ;
      /// <summary>
      ///   Real part of the two-dimensional Fourier transform.
      /// </summary>
      property Real2D      : TGIS_GridArray
                             read  FRe2D ;
      /// <summary>
      ///   Imaginary part of the two-dimensional Fourier transform.
      /// </summary>
      property Imaginary2D : TGIS_GridArray
                             read  FIm2D ;
      /// <summary>
      ///   Modulus of the two-dimensional Fourier transform.
      /// </summary>
      /// <param name="_y">
      ///   row index of the transform value
      /// </param>
      /// <param name="_x">
      ///   column index of the transform value
      /// </param>
      property Modulus2D   [const _y : Integer ; const _x : Integer]
                           : Single
                             read  fget_Modulus2D ;
      /// <summary>
      ///   Argument of the two-dimensional Fourier transform.
      /// </summary>
      /// <param name="_y">
      ///   row index of the transform value
      /// </param>
      /// <param name="_x">
      ///   column index of the transform value
      /// </param>
      property Argument2D  [const _y : Integer ; const _x : Integer]
                           : Single
                             read  fget_Argument2D ;
      /// <summary>
      ///   If True then the transform is shifted by half the size so that the
      ///   low frequencies are in the middle instead of the corners; does not
      ///   have any effect after the transform has been computed; default is
      ///   True.
      /// </summary>
      property Shifted     : Boolean
                             read  FShifted
                             write FShifted ;
    published
      /// <summary>
      ///   Event fired upon progress of the generation process; is fired only
      ///   for two-dimensional transforms.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent    : TGIS_BusyEvent
                             add fadd_BusyEvent
                             remove fremove_BusyEvent ;
      {$ELSE}
        /// <event/>
        property BusyEvent : TGIS_BusyEvent
                             read  fget_BusyEvent
                             write fset_BusyEvent ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    GisResource,
    GisRtl ;
{$ENDIF}


type

  T_FFTlevel = class
    public
      constructor Create ( const _size : Integer
                         ) ;
    public
      Re : TGIS_SingleArray ;
      Im : TGIS_SingleArray ;
  end ;


//==============================================================================
// T_FFTlevel
//==============================================================================

  constructor T_FFTlevel.Create(
    const _size : Integer
  ) ;
  begin
    inherited Create ;

    SetLength( Re, _size ) ;
    SetLength( Im, _size ) ;
  end ;


//==============================================================================
// TGIS_FourierTransform
//==============================================================================

  constructor TGIS_FourierTransform.Create ;
  begin
    inherited ;

    iState := 0 ;
    FShifted := True ;
    busyEventManager := TGIS_BusyEventManager.Create( Self ) ;
  end ;

  procedure TGIS_FourierTransform.doDestroy ;
  begin
    FreeObject( busyEventManager ) ;

    inherited ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_FourierTransform.fadd_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent += _value ;
    end ;

    procedure TGIS_FourierTransform.fremove_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent -= _value ;
    end ;
  {$ELSE}
    procedure TGIS_FourierTransform.fset_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent := _value ;
    end ;

    function TGIS_FourierTransform.fget_BusyEvent : TGIS_BusyEvent ;
    begin
      Result := busyEventManager.BusyEvent ;
    end ;
  {$ENDIF}

  function TGIS_FourierTransform.fget_Modulus(
    const _i : Integer
  ) : Single ;
  begin
    Result := calcModulus( FRe[_i], FIm[_i] ) ;
  end ;


  function TGIS_FourierTransform.fget_Argument(
    const _i : Integer
  ) : Single ;
  begin
    Result := calcArgument( FRe[_i], FIm[_i] ) ;
  end ;


  function TGIS_FourierTransform.fget_Modulus2D(
    const _y   : Integer ;
    const _x   : Integer
  ) : Single ;
  begin
    Result := calcModulus( FRe2D[_y,_x], FIm2D[_y,_x] ) ;
  end ;


  function TGIS_FourierTransform.fget_Argument2D(
    const _y   : Integer ;
    const _x   : Integer
  ) : Single ;
  begin
    Result := calcArgument( FRe2D[_y,_x], FIm2D[_y,_x] ) ;
  end ;


  procedure TGIS_FourierTransform.prepSinCos ;
  var
    vps  : Single ;
    siz  : Integer ;
    vsin : Double ;
    vcos : Double ;
    i    : Integer ;
    k    : Integer ;
  begin
    if iLevel = 0 then
      exit ;

    SetLength( oSinCos, iLevel ) ;

    for i := 0 to iLevel  - 1 do begin
      siz := 1 shl ( iLevel - i ) ;
      vps := Pi/siz ;
      SetLength( oSinCos[i], 2*siz ) ;
      for k := 0 to siz - 1 do begin
        SinCos( k*vps, vsin, vcos ) ;
        oSinCos[i,2*k  ] := vsin ;
        oSinCos[i,2*k+1] := vcos ;
      end ;
    end ;
  end ;


  procedure TGIS_FourierTransform.getSinCos(
    const _lvl  : Integer ;
    const _idx  : Integer ;
      var _sin  : Single ;
      var _cos  : Single
  ) ;
  begin
    _sin := oSinCos[_lvl,2*_idx  ] ;
    _cos := oSinCos[_lvl,2*_idx+1] ;
  end ;


  function TGIS_FourierTransform.calcModulus(
    const _re : Single ;
    const _im : Single
  ) : Single ;
  begin
    Result := Sqrt( _re*_re + _im*_im ) ;
  end ;


  function TGIS_FourierTransform.calcArgument(
    const _re : Single ;
    const _im : Single
  ) : Single ;
  var
    res : Single ;
  begin
    if ( _re <> 0.0 ) and ( _im <> 0.0 ) then
      res := ArcTan2( _im, _re )
    else
      res := NaN ;

    Result := res ;
  end ;


  function TGIS_FourierTransform.compFast(
    const _lvl  : Integer ;
    const _idx  : Integer ;
    const _row  : Integer
  ) : TObject ;
  var
    res  : T_FFTlevel ;
    ft0  : T_FFTlevel ;
    ft1  : T_FFTlevel ;
    step : Integer ;
    siz  : Integer ;
    siz2 : Integer ;
    vsin : Single ;
    vcos : Single ;
    re0  : Single ;
    re1  : Single ;
    im0  : Single ;
    im1  : Single ;
    i    : Integer ;

    function re_val( const _i : Integer ) : Single ;
    begin
      if b2D then begin
        if not bInverse then begin
          if not bColumns then begin
            if ( _row < iHeight ) and ( _i < iWidth ) then
              Result := oSignal2D[_row,_i]
            else
              Result := 0.0 ;
          end
          else
            Result := oRe1D[_i,_row]
        end
        else begin
          if not bColumns then
            Result := FIm2D[_row,_i]
          else
            Result := oIm1D[_i,_row]
        end ;
      end
      else begin
        if not bInverse then begin
          if _i < iWidth then
            Result := oSignal[_i]
          else
            Result := 0.0 ;
        end
        else
          Result := FIm[_i] ;
      end ;
    end ;

    function im_val( const _i : Integer ) : Single ;
    begin
      if b2D then begin
        if not bInverse then begin
          if not bColumns then
            Result := 0.0
          else
            Result := oIm1D[_i,_row]
        end
        else begin
          if not bColumns then
            Result := FRe2D[_row,_i]
          else
            Result := oRe1D[_i,_row]
        end ;
      end
      else begin
        if not bInverse then
          Result := 0.0
        else
          Result := FRe[_i] ;
      end ;
    end ;

  begin
    step := 1 shl _lvl ;
    if _lvl < iLevel then begin
      ft0 := T_FFTlevel( compFast( _lvl + 1, _idx, _row ) ) ;
      ft1 := T_FFTlevel( compFast( _lvl + 1, _idx + step, _row ) ) ;
      siz := iSize div step ;
      siz2 := siz div 2 ;
      res := T_FFTlevel.Create( siz ) ;
      for i := 0 to siz2 - 1 do begin
        getSinCos( _lvl, i, vsin, vcos ) ;
        res.Re[i] := ft0.Re[i] + vcos*ft1.Re[i] + vsin*ft1.Im[i] ;
        res.Im[i] := ft0.Im[i] - vsin*ft1.Re[i] + vcos*ft1.Im[i] ;
        res.Re[siz2+i] := ft0.Re[i] - vcos*ft1.Re[i] - vsin*ft1.Im[i] ;
        res.Im[siz2+i] := ft0.Im[i] + vsin*ft1.Re[i] - vcos*ft1.Im[i] ;
      end ;
      FreeObject( ft0 ) ;
      FreeObject( ft1 ) ;
    end
    else begin
      re0 := re_val( _idx ) ;
      im0 := im_val( _idx ) ;
      re1 := re_val( _idx + step ) ;
      im1 := im_val( _idx + step ) ;

      res := T_FFTlevel.Create( 2 ) ;
      res.Re[0] := re0 + re1 ;
      res.Im[0] := im0 + im1 ;
      res.Re[1] := re0 - re1 ;
      res.Im[1] := im0 - im1 ;
    end ;

    Result := res ;
  end ;


  function TGIS_FourierTransform.copyWindow(
    const _signal : TGIS_SingleArray ;
    const _start  : Integer ;
    const _end    : Integer
  ) : TGIS_SingleArray ;
  var
    res : TGIS_SingleArray ;
    len : Integer ;
  begin
    len := _end - _start + 1 ;
    SetLength( res, len ) ;
    res := TGIS_SingleArray( Copy( _signal, _start, len ) ) ;

    Result := res ;
  end ;


  function TGIS_FourierTransform.copyWindow2D(
    const _signal : TGIS_GridArray ;
    const _bounds : TRect
  ) : TGIS_GridArray ;
  var
    res  : TGIS_GridArray ;
    wdth : Integer ;
    hght : Integer ;
    i    : Integer ;
  begin
    hght := _bounds.Bottom - _bounds.Top + 1 ;
    wdth := _bounds.Right - _bounds.Left + 1 ;
    SetLength( res, hght, wdth ) ;
    for i := 0 to hght - 1 do
      res[i] := TGIS_SingleArray(
        Copy( _signal[_bounds.Top+i], _bounds.Left, wdth )
      ) ;

    Result := res ;
  end ;


  procedure TGIS_FourierTransform.shiftFFT ;
  var
    siz2 : Integer ;
    tmp  : Single ;
    i    : Integer ;
  begin
    siz2 := iSize div 2 ;
    if FShifted then begin
      for i := 0 to siz2 - 1 do begin
        tmp := FRe[i] ;
        FRe[i] := FRe[i+siz2] ;
        FRe[i+siz2] := tmp ;

        tmp := FIm[i] ;
        FIm[i] := FIm[i+siz2] ;
        FIm[i+siz2] := tmp ;
      end ;
    end ;
  end ;


  procedure TGIS_FourierTransform.shiftFFT2D ;
  var
    siz2 : Integer ;
    tmp  : Single ;
    i    : Integer ;
    k    : Integer ;
  begin
    siz2 := iSize div 2 ;
    if FShifted then begin
      for i := 0 to iSize - 1 do begin
        if i < siz2 then begin
          for k := 0 to siz2 - 1 do begin
            tmp := FRe2D[i,k] ;
            FRe2D[i,k] := FRe2D[i+siz2,k+siz2] ;
            FRe2D[i+siz2,k+siz2] := tmp ;

            tmp := FIm2D[i,k] ;
            FIm2D[i,k] := FIm2D[i+siz2,k+siz2] ;
            FIm2D[i+siz2,k+siz2] := tmp ;
          end ;
        end
        else begin
          for k := 0 to siz2 - 1 do begin
            tmp := FRe2D[i,k] ;
            FRe2D[i,k] := FRe2D[i-siz2,k+siz2] ;
            FRe2D[i-siz2,k+siz2] := tmp ;

            tmp := FIm2D[i,k] ;
            FIm2D[i,k] := FIm2D[i-siz2,k+siz2] ;
            FIm2D[i-siz2,k+siz2] := tmp ;
          end ;
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_FourierTransform.RunDiscrete(
    const _signal : TGIS_SingleArray
  ) ;
  var
    pi2n : Single ;
    vsin : Double ;
    vcos : Double ;
    siz2 : Single ;
    i    : Integer ;
    k    : Integer ;
  begin
    iWidth := length( _signal ) ;
    if iWidth < 2 then
      exit ;

    oSignal := _signal ;

    iSize := iWidth ;

    if FShifted then
      siz2 := iSize/2.0
    else
      siz2 := 0.0 ;

    SetLength( FRe, iSize ) ;
    SetLength( FIm, iSize ) ;

    pi2n := 2.0*Pi/iSize ;
    for i := 0 to iSize - 1 do begin
      FRe[i] := 0.0 ;
      FIm[i] := 0.0 ;
      for k := 0 to iSize - 1 do begin
        SinCos( pi2n*i*( k + siz2 ), vsin, vcos ) ;
        FRe[i] := FRe[i] + _signal[k]*vcos ;
        FIm[i] := FIm[i] - _signal[k]*vsin ;
      end ;
    end ;

    iState := 10 ;
  end ;


  procedure TGIS_FourierTransform.RunDiscrete(
    const _signal : TGIS_SingleArray ;
    const _start  : Integer ;
    const _end    : Integer
  ) ;
  var
    sig : TGIS_SingleArray ;
  begin
    sig := copyWindow( _signal, _start, _end ) ;
    RunDiscrete( sig ) ;
  end ;


  function TGIS_FourierTransform.invertDiscrete
    : TGIS_SingleArray ;
  var
    res  : TGIS_SingleArray ;
    pi2n : Single ;
    vsin : Double ;
    vcos : Double ;
    siz2 : Single ;
    i    : Integer ;
    k    : Integer ;
  begin
    if FShifted then
      siz2 := iSize/2.0
    else
      siz2 := 0.0 ;

    SetLength( res, iSize ) ;

    pi2n := 2.0*Pi/iSize ;
    for i := 0 to iSize - 1 do begin
      res[i] := 0.0 ;
      for k := 0 to iSize - 1 do begin
        SinCos( pi2n*i*( k + siz2 ), vsin, vcos ) ;
        res[i] := res[i] + FRe[k]*vcos - FIm[k]*vsin ;
      end ;
      res[i] := res[i]/iSize ;
    end ;

    Result := res ;
  end ;


  procedure TGIS_FourierTransform.RunFast(
    const _signal : TGIS_SingleArray
  ) ;
  var
    part : T_FFTlevel ;
    aux  : Single ;
  begin
    iWidth := length( _signal ) ;
    if iWidth < 2 then
      exit ;

    oSignal := _signal ;

    aux := Log2( 1.0*iWidth ) ;
    if aux <> 1.0*FloorS( aux ) then begin
      aux := FloorS( aux ) + 1.0 ;
      aux := Power( 2.0, aux ) ;
      iSize := RoundS( aux ) ;
    end
    else
      iSize := iWidth ;

    iLevel := RoundS( Log2( 1.0*iSize ) - 1 ) ;

    prepSinCos ;

    SetLength( FRe, iSize ) ;
    SetLength( FIm, iSize ) ;

    b2D := False ;
    bInverse := False ;

    part := T_FFTlevel( compFast( 0, 0, 0 ) ) ;
    FRe := TGIS_SingleArray( Copy( part.Re, 0, iSize ) ) ;
    FIm := TGIS_SingleArray( Copy( part.Im, 0, iSize ) ) ;
    FreeObject( part ) ;

    shiftFFT ;

    iState := 11 ;
  end ;


  procedure TGIS_FourierTransform.RunFast(
    const _signal : TGIS_SingleArray ;
    const _start  : Integer ;
    const _end    : Integer
  ) ;
  var
    sig : TGIS_SingleArray ;
  begin
    sig := copyWindow( _signal, _start, _end ) ;
    RunFast( sig ) ;
  end ;


  function TGIS_FourierTransform.invertFast
    : TGIS_SingleArray ;
  var
    res  : TGIS_SingleArray ;
    part : T_FFTlevel ;
    i    : Integer ;
  begin
    shiftFFT ;

    b2D := False ;
    bInverse := True ;
    part := T_FFTlevel( compFast( 0, 0, 0 ) ) ;
    SetLength( res, iWidth ) ;
    for i := 0 to iWidth - 1 do
      res[i] := part.Im[i]/iSize ;
    FreeObject( part ) ;

    shiftFFT ;

    Result := res ;
  end ;


  function TGIS_FourierTransform.Invert
    : TGIS_SingleArray ;
  var
    res : TGIS_SingleArray ;
  begin
    if iState = 10 then
      res := invertDiscrete
    else
    if iState = 11 then
      res := invertFast
    else
      SetLength( res, 0 ) ;

    Result := res ;
  end ;


  procedure TGIS_FourierTransform.RunDiscrete2D(
    const _signal : TGIS_GridArray
  ) ;
  var
    pi2n : Single ;
    vsin : Double ;
    vcos : Double ;
    siz2 : Single ;
    xi   : Integer ;
    yi   : Integer ;
    xk   : Integer ;
    yk   : Integer ;
    abrt : Boolean ;

  begin
    iHeight := length( _signal ) ;
    if iHeight < 2 then
      exit ;

    iWidth := length( _signal[0] ) ;
    if iWidth < 2 then
      exit ;

    oSignal2D := _signal ;

    iSize := Max( iHeight, iWidth ) ;
    if FShifted then
      siz2 := iSize/2.0
    else
      siz2 := 0.0 ;

    SetLength( FRe2D, iSize, iSize ) ;
    SetLength( FIm2D, iSize, iSize ) ;

    busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_FOURIER_DFT ), iSize*iSize ) ;
    try
      pi2n := 2.0*Pi/iSize ;
      for yi := 0 to iSize - 1 do begin
        for xi := 0 to iSize - 1 do begin

          abrt := busyEventManager.PushEvent ;
          if abrt then
            exit ;

          FRe2D[yi,xi] := 0.0 ;
          FIm2D[yi,xi] := 0.0 ;
          for yk := 0 to iHeight - 1 do begin
            for xk := 0 to iWidth - 1 do begin
              SinCos( pi2n*( xk*( xi + siz2 ) + yk*( yi + siz2 ) ),
                      vsin, vcos ) ;
              FRe2D[yi,xi] := FRe2D[yi,xi] + _signal[yk,xk]*vcos ;
              FIm2D[yi,xi] := FIm2D[yi,xi] - _signal[yk,xk]*vsin ;
            end ;
          end ;
        end ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end ;

    iState := 20 ;
  end ;


  procedure TGIS_FourierTransform.RunDiscrete2D(
    const _signal : TGIS_GridArray ;
    const _bounds : TRect
  ) ;
  var
    sig : TGIS_GridArray ;
  begin
    sig := copyWindow2D( _signal, _bounds ) ;
    RunDiscrete2D( sig ) ;
  end ;


  function TGIS_FourierTransform.invertDiscrete2D
    : TGIS_GridArray ;
  var
    res  : TGIS_GridArray ;
    pi2n : Single ;
    vsin : Double ;
    vcos : Double ;
    siz2 : Single ;
    xi   : Integer ;
    yi   : Integer ;
    xk   : Integer ;
    yk   : Integer ;
    abrt : Boolean ;

  begin
    if FShifted then
      siz2 := iSize/2.0
    else
      siz2 := 0.0 ;

    SetLength( res, iHeight, iWidth ) ;

    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_FOURIER_IDFT ), iHeight*iWidth
    ) ;
    try
      pi2n := 2.0*Pi/iSize ;
      for yi := 0 to iHeight - 1 do begin
        for xi := 0 to iWidth - 1 do begin

          abrt := busyEventManager.PushEvent ;
          if abrt then
            exit ;

          res[yi,xi] := 0.0 ;
          for yk := 0 to iSize - 1 do begin
            for xk := 0 to iSize - 1 do begin
              SinCos( pi2n*( xi*( xk + siz2 ) + yi*( yk + siz2 ) ),
                      vsin, vcos ) ;
              res[yi,xi] := res[yi,xi] +
                            FRe2D[yk,xk]*vcos - FIm2D[yk,xk]*vsin ;
            end ;
          end ;
          res[yi,xi] := res[yi,xi]/( iSize*iSize ) ;
        end ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end ;

    Result := res ;
  end ;


  procedure TGIS_FourierTransform.RunFast2D(
    const _signal : TGIS_GridArray
  ) ;
  var
    part : T_FFTlevel ;
    waux : Single ;
    haux : Single ;
    i    : Integer ;
    k    : Integer ;
    abrt : Boolean ;

  begin
    iHeight := length( _signal ) ;
    if iHeight < 2 then
      exit ;

    iWidth := length( _signal[0] ) ;
    if iWidth < 2 then
      exit ;

    oSignal2D := _signal ;

    haux := Log2( iHeight ) ;
    if haux <> 1.0*FloorS( haux ) then begin
      haux := FloorS( haux ) + 1.0 ;
      haux := Power( 2.0, haux ) ;
      i := RoundS( haux ) ;
    end
    else
      i := iHeight ;

    waux := Log2( iWidth ) ;
    if waux <> 1.0*FloorS( waux ) then begin
      waux := FloorS( waux ) + 1.0 ;
      waux := Power( 2.0, waux ) ;
      k := RoundS( waux ) ;
    end
    else
      k := iWidth ;

    iSize := Max( i, k ) ;
    iLevel := RoundS( Log2( 1.0*iSize ) - 1 ) ;

    prepSinCos ;

    SetLength( oRe1D, iSize, iSize ) ;
    SetLength( oIm1D, iSize, iSize ) ;

    SetLength( FRe2D, iSize, iSize ) ;
    SetLength( FIm2D, iSize, iSize ) ;

    busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_FOURIER_FFT ), 2*iSize ) ;
    try
      b2D := True ;
      bInverse := False ;

      bColumns := False ;
      for i := 0 to iSize - 1 do begin
        part := T_FFTlevel( compFast( 0, 0, i ) ) ;
        try
          oRe1D[i] := TGIS_SingleArray( Copy( part.Re, 0, iSize ) ) ;
          oIm1D[i] := TGIS_SingleArray( Copy( part.Im, 0, iSize ) ) ;
        finally
          FreeObject( part ) ;
        end ;

        abrt := busyEventManager.PushEvent ;
        if abrt then
          exit ;
      end ;

      bColumns := True ;
      for i := 0 to iSize - 1 do begin
        part := T_FFTlevel( compFast( 0, 0, i ) ) ;
        try
          for k := 0 to iSize - 1 do begin
            FRe2D[k,i] := part.Re[k] ;
            FIm2D[k,i] := part.Im[k] ;
          end ;
        finally
          FreeObject( part ) ;
        end ;

        abrt := busyEventManager.PushEvent ;
        if abrt then
          exit ;
      end ;

      shiftFFT2D ;
    finally
      busyEventManager.EndEvent ;
    end;

    iState := 21 ;
  end ;


  procedure TGIS_FourierTransform.RunFast2D(
    const _signal : TGIS_GridArray ;
    const _bounds : TRect
  ) ;
  var
    sig : TGIS_GridArray ;
  begin
    sig := copyWindow2D( _signal, _bounds ) ;
    RunFast2D( sig ) ;
  end ;


  function TGIS_FourierTransform.invertFast2D
    : TGIS_GridArray ;
  var
    res  : TGIS_GridArray ;
    part : T_FFTlevel ;
    i    : Integer ;
    k    : Integer ;
    abrt : Boolean ;

  begin
    SetLength( oRe1D, iSize, iSize ) ;
    SetLength( oIm1D, iSize, iSize ) ;

    busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_FOURIER_IFFT ), 2*iSize ) ;
    try
      shiftFFT2D ;

      b2D := True ;
      bInverse := True ;

      bColumns := False ;
      for i := 0 to iSize - 1 do begin
        part := T_FFTlevel( compFast( 0, 0, i ) ) ;
        try
          oRe1D[i] := TGIS_SingleArray( Copy( part.Im, 0, iSize ) ) ;
          oIm1D[i] := TGIS_SingleArray( Copy( part.Re, 0, iSize ) ) ;
        finally
          FreeObject( part ) ;
        end ;

        abrt := busyEventManager.PushEvent ;
        if abrt then
          exit ;
      end ;

      bColumns := True ;
      SetLength( res, iHeight, iWidth ) ;
      for i := 0 to iWidth - 1 do begin
        part := T_FFTlevel( compFast( 0, 0, i ) ) ;
        try
          for k := 0 to iHeight - 1 do
            res[k,i] := part.Im[k]/( iSize*iSize ) ;
        finally
          FreeObject( part ) ;
        end ;

        abrt := busyEventManager.PushEvent ;
        if abrt then
          exit ;
      end ;

      shiftFFT2D ;
    finally
      busyEventManager.EndEvent ;
    end ;

    Result := res ;
  end ;


  function TGIS_FourierTransform.Invert2D
    : TGIS_GridArray ;
  var
    res : TGIS_GridArray ;
  begin
    if iState = 20 then
      res := invertDiscrete2D
    else
    if iState = 21 then
      res := invertFast2D
    else
      SetLength( res, 0, 0 ) ;

    Result := res ;
  end ;


//==================================== END =====================================
end.
