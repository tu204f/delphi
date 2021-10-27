program sort_file;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.SysUtils in '..\library\Lb.SysUtils.pas',
  Lb.Message in '..\library\message\Lb.Message.pas',
  Lb.StringsFile in '..\library\Lb.StringsFile.pas',
  Lb.Sort in '..\library\Lb.Sort.pas',
  SQLite3 in '..\library\sqlite\SQLite3.pas',
  SQLiteWrap in '..\library\sqlite\SQLiteWrap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
