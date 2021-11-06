program export;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Search.Files in '..\library\Lb.Search.Files.pas',
  Lb.Export in '..\library\Lb.Export.pas',
  Lb.StringsFile in '..\library\Lb.StringsFile.pas',
  Lb.SysUtils in '..\library\Lb.SysUtils.pas',
  SQLite3 in '..\library\sqlite\SQLite3.pas',
  SQLiteWrap in '..\library\sqlite\SQLiteWrap.pas',
  Lb.Logger in '..\library\Lb.Logger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
