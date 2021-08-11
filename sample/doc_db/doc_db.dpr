program doc_db;



{$R 'db.res' '..\..\library\doc_db\res\db.rc'}

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.DataModuleDB in '..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Logger in '..\..\library\Lb.Logger.pas',
  Lb.SysUtils.ISO860 in '..\..\library\Lb.SysUtils.ISO860.pas',
  Lb.Resource.Script in '..\..\library\Lb.Resource.Script.pas',
  UnitValueForm in 'UnitValueForm.pas' {ValueForm},
  UnitDocumentForm in 'UnitDocumentForm.pas' {DocumentForm},
  Lb.Doc.DB.Params in '..\..\library\doc_db\Lb.Doc.DB.Params.pas',
  Lb.Doc.DB in '..\..\library\doc_db\Lb.Doc.DB.pas',
  Lb.Doc.DB.Query in '..\..\library\doc_db\Lb.Doc.DB.Query.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  TLogger.ClearLog;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
