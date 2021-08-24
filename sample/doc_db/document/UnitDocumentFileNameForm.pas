unit UnitDocumentFileNameForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TDocumentFileNameForm = class(TForm)
    ScriptEdit: TEdit;
    ButtonOk: TButton;
    procedure ButtonOkClick(Sender: TObject);
  private
  public
    FileName: String;
  end;

function GetFileNameScript: String;

implementation

{$R *.dfm}

function GetFileNameScript: String;
var
  xDocumentFileNameForm: TDocumentFileNameForm;
begin
  Result := '';
  xDocumentFileNameForm := TDocumentFileNameForm.Create(nil);
  try
    if xDocumentFileNameForm.ShowModal = mrOk then
      Result := xDocumentFileNameForm.FileName;
  finally
    FreeAndNil(xDocumentFileNameForm)
  end;
end;


procedure TDocumentFileNameForm.ButtonOkClick(Sender: TObject);
begin
  Self.FileName := ScriptEdit.Text;
  if not FileName.IsEmpty then
  begin
    FileName := ExtractFilePath(ParamStr(0)) + 'scripts' + PathDelim + Self.FileName;
    Self.ModalResult := mrOk;
  end;
end;

end.
