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
  Encapsulation of the TGIS_Renderer3DDirectXAbstract class.
}

{$IFDEF DCC}
  unit GisRenderer3DDirectXAbstract ;
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL.SharpDX ;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisRenderer3DAbstract ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.util,
    tatukgis.rtl ;
{$ENDIF}

const
  {$IFDEF CLR}
    /// <summary>
    ///   DirectX9 compatibility definition.
    /// </summary>
    D3DUSAGE_NONE = D3DUSAGE.None ;
  {$ELSE}
    /// <summary>
    ///   DirectX9 compatibility definition.
    /// </summary>
    D3DUSAGE_NONE = 0 ;
  {$ENDIF}

const
  {$IFDEF CLR}
    /// <summary>
    ///   DirectX9 compatibility definition.
    /// </summary>
    D3DLOCK_NONE = D3DLOCK.None ;
  {$ELSE}
    /// <summary>
    ///   DirectX9 compatibility definition.
    /// </summary>
    D3DLOCK_NONE = 0 ;
  {$ENDIF}

type
  /// <summary>
  ///   DirectX9 Abstract class.
  /// </summary>
  TGIS_Renderer3DDirectXAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF} class ( TGIS_Renderer3DAbstract )
    protected
      procedure doDestroy              ; override;

      /// <inheritdoc/>
      function  reverseValue           ( const _value : Single
                                       ) : Single ; override;

    public //  constructors
      /// <inheritdoc/>
      constructor Create               ; override;

  end ;


//##############################################################################
implementation


  constructor TGIS_Renderer3DDirectXAbstract.Create ;
  begin
    inherited ;

  end ;

  procedure TGIS_Renderer3DDirectXAbstract.doDestroy ;
  begin

    inherited ;
  end ;

  function  TGIS_Renderer3DDirectXAbstract.reverseValue(
    const _value : Single
  ) : Single ;
  begin
    Result := _value ;
  end ;

//==================================== END =====================================
end.

