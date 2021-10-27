program gen_file;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Message in '..\library\message\Lb.Message.pas',
  Lb.GenFile in '..\library\Lb.GenFile.pas',
  Lb.SysUtils in '..\library\Lb.SysUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
