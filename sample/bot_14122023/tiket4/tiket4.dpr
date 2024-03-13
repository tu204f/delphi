program tiket4;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.ReadPrice in '..\lb\Lb.ReadPrice.pas',
  Lb.SysUtils in '..\lb\Lb.SysUtils.pas',
  Lb.Bot.Tiket in '..\lb\Lb.Bot.Tiket.pas',
  UnitLogForm in '..\lb\lb\UnitLogForm.pas' {LogForm},
  Lb.Position.Trade in '..\lb\Lb.Position.Trade.pas',
  Lb.Mode in '..\lb\Lb.Mode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.Run;
end.
