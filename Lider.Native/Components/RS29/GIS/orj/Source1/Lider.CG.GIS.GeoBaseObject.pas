unit Lider.CG.GIS.GeoBaseObject;
{
Compiled by Delphi XE7 (Win32)
Decompiled by DCU32INT Version: 1.20
Source files:
  PROTECT_TIME.inc (24.4.2015 14:41:46),
  PROTECT_SEED.INC (24.4.2015 14:41:46), Lider.CG.GIS.GeoInclude.inc (7.4.2015 11:54:06),
  Lider.CG.GIS.GeoBaseObject.pas (26.5.2014 07:45:12)}

interface

uses

  System.SysUtils {
    T:TCardinalHelper, T:TInt64Helper, T:TBooleanHelper, T:TIntegerHelper,
    T:TNativeUIntHelper, T:TStringHelper, T:TByteHelper,
    A:Exception.Create, A:Now, A:Format, A:UpperCase, A:ExtractFileName},
  System.Types,
  System.Classes {
    T:TComponent, A:TComponent.CanObserve, A:TComponent.ObserverAdded,
    A:TComponent.GetObservers, A:TComponent.UpdateRegistry,
    A:TComponent.ValidateRename, A:TComponent.WriteState,
    A:TComponent.QueryInterface, A:TComponent.SafeCallException,
    A:TComponent.UpdateAction, A:TPersistent.Assign, T:EComponentError,
    A:.EComponentError, T::45, A:csDesigning};

type
  TGIS_Object = class(TObject)
  protected
    procedure doDestroy; virtual{@0};
  public
    destructor Destroy; override{; virtual @-4};
  end;

  TGIS_ObjectDisposable = class(TGIS_Object) ;

  TGIS_BaseObject = class(TObject)
  private
    FSreial : int64;
  protected
    procedure doDestroy; virtual{@0};

  public
    constructor create;
    destructor Destroy; override{; virtual @-4};
    function VerifyObject: Boolean;
    property Serial:Int64 read FSreial;

  end;

  TGIS_BaseObjectDisposable = class(TGIS_BaseObject) ;

  TGIS_BaseControl = class(TObject)
  private

  public
    constructor Create; virtual;
    function VerifyObject: Boolean;

  end;

//procedure Finalization;

//procedure Lider.CG.GIS.GeoBaseObject;

implementation


destructor TGIS_Object.Destroy;
begin
  doDestroy;
  inherited;
end;

procedure TGIS_Object.doDestroy;
begin
end;

constructor TGIS_BaseObject.create;
begin
  inherited;
end;

destructor TGIS_BaseObject.Destroy;
begin
  doDestroy;
  inherited;
end;

procedure TGIS_BaseObject.doDestroy;
begin
end;



function TGIS_BaseObject.VerifyObject: Boolean;
begin
   result := true;
end;



constructor TGIS_BaseControl.Create;
begin
  inherited;
end;

function TGIS_BaseControl.VerifyObject: Boolean;
begin
  result := true;
end;

end.




