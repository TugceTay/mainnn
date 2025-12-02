//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to add multilanguage support.
}

program Languages;

uses
  Vcl.Forms,
  Vcl.Dialogs,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  ShowMessage( 'Language support for desired language must'#13+
               'be installed in your system'
             ) ;  

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

