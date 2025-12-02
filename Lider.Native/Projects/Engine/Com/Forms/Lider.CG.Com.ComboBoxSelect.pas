unit Lider.CG.Com.ComboBoxSelect;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  dxSkinsCore,
  dxSkinsDefaultPainters,
  cxTextEdit,
  cxMaskEdit,
  cxDropDownEdit,
  cxGroupBox,
  cxPropertiesStore,
  Menus,
  StdCtrls,
  cxButtons,
  cxClasses;

type
  TfmSelectCBox = class(TForm)
    cxGroupBox1: TcxGroupBox;
    cxComboBox1: TcxComboBox;
    cxButton2: TcxButton;
    cxButton1: TcxButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SelectComboBox(Cap: string; const SL: TStrings; var Itemintex: integer): boolean;

implementation

{$R *.dfm}

function SelectComboBox(Cap: string; const SL: TStrings; var Itemintex: integer): boolean;
begin
  Itemintex := -1;
  with TfmSelectCBox.Create(nil) do
  begin
    try
      Caption := Cap;
      cxComboBox1.Properties.Items.AddStrings(SL);
      cxComboBox1.ItemIndex := 0;
      Result := ShowModal = mrOK;
      if Result then
        Itemintex := cxComboBox1.itemindex;
    finally
      Free;
    end;
  end;
end;

end.


