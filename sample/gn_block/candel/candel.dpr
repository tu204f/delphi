program candel;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {Form4},
  Lb.ReadPrice in '..\lib\Lb.ReadPrice.pas',
  Lb.SysUtils in '..\lib\Lb.SysUtils.pas',
  Lb.WorkTraderThread in '..\lib\Lb.WorkTraderThread.pas',
  Lb.Population in '..\lib\Lb.Population.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
