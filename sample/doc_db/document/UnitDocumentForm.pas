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
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FileName: String;
  end;

var
  DocumentForm: TDocumentForm;

implementation

{$R *.dfm}

uses
  UnitDocumentFileNameForm;

procedure TDocumentForm.ButtonLoadClick(Sender: TObject);
begin
  //
end;

procedure TDocumentForm.ButtonSaveClick(Sender: TObject);
begin
  if FileName.IsEmpty then
  begin
    FileName := GetFileNameScript;
    Memo.Lines.SaveToFile(FileName);
  end;
end;

procedure TDocumentForm.ButtonExecuteClick(Sender: TObject);
begin
  //
end;


end.
