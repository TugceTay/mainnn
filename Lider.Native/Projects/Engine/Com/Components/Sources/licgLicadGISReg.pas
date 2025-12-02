unit licgLicadGISReg;

{$I cxVer.inc}
{$I Lider.CG.Com.Component.inc}
{$R licgLicadGIS.dcr}

interface

uses
  Classes, Controls, DesignEditors, DesignIntf;

procedure Register;

implementation

uses
  Lider.CG.Com.Base,
  Lider.CG.Com.Ruler,
  Lider.CG.Com.DB,
  Lider.CG.Com.GIS,
  Lider.CG.Com.ScaleBar;


procedure Register;
begin
  RegisterComponents('Licad GIS', [//TlicgThematicBuilder,
                                   //TlicgLegend,
                                   TlicgHorizontalRuler,
                                   TlicgVerticalRuler,
                                   TlicgScaleBar,
                                   TlicgTable
                                   ]);
end;

initialization

finalization

end.

