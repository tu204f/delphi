program document;

uses
  Vcl.Forms,
  UnitDocumentForm in 'UnitDocumentForm.pas' {DocumentForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDocumentForm, DocumentForm);
  Application.Run;
end.
