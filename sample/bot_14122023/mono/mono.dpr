program mono;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.ReadPrice in '..\lb\Lb.ReadPrice.pas',
  Lb.SysUtils in '..\lb\Lb.SysUtils.pas',
  Lb.Bot in '..\lb\Lb.Bot.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
