program document;

uses
  Vcl.Forms,
  UnitDocumentForm in 'UnitDocumentForm.pas' {DocumentForm},
  UnitScriptForm in 'UnitScriptForm.pas' {ScriptsForm},
  UnitDocumentFileNameForm in 'UnitDocumentFileNameForm.pas' {DocumentFileNameForm},
  Lb.ApplicationVersion in '..\..\..\library\Lb.ApplicationVersion.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDocumentForm, DocumentForm);
  Application.Run;
end.
