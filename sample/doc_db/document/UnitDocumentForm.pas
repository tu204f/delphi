unit UnitDocumentForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TDocumentForm = class(TForm)
    Memo: TMemo;
    ButtonExecute: TButton;
    ButtonLoad: TButton;
    ButtonSave: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DocumentForm: TDocumentForm;

implementation

{$R *.dfm}

end.
