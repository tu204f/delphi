program bot_tik_v1;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  UnitValuesFrame in 'lib\UnitValuesFrame.pas' {ValuesFrame: TFrame},
  UnitValueFrame in 'lib\UnitValueFrame.pas' {ValueFrame: TFrame},
  Lb.CandelBot in 'lib\Lb.CandelBot.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
