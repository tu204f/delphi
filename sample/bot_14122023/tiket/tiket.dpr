program tiket;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bot in '..\lb\Lb.Bot.pas',
  Lb.ReadPrice in '..\lb\Lb.ReadPrice.pas',
  Lb.SysUtils in '..\lb\Lb.SysUtils.pas',
  Lb.Bot.Tiket in '..\lb\Lb.Bot.Tiket.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
