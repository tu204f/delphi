program ini_files;

{$R 'db.res' '..\..\..\library\doc_db\res\db.rc'}

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Setting.Values in '..\..\..\library\setting\Lb.Setting.Values.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Doc.DB.Params in '..\..\..\library\doc_db\Lb.Doc.DB.Params.pas',
  Lb.Doc.DB in '..\..\..\library\doc_db\Lb.Doc.DB.pas',
  Lb.Doc.DB.Query in '..\..\..\library\doc_db\Lb.Doc.DB.Query.pas',
  Lb.Doc.DB.SysUtils in '..\..\..\library\doc_db\Lb.Doc.DB.SysUtils.pas',
  Lb.Resource.Script in '..\..\..\library\Lb.Resource.Script.pas',
  Lb.SysUtils.ISO860 in '..\..\..\library\Lb.SysUtils.ISO860.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Setting.Values.SQLiteDB in '..\..\..\library\setting\Lb.Setting.Values.SQLiteDB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
