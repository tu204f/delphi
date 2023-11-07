program nr_3;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Block in 'lb\Lb.Block.pas',
  UnitChartCandelsFrame in 'lb\UnitChartCandelsFrame.pas' {ChartCandelsFrame: TFrame},
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.NeuronNet in '..\lib\Lb.NeuronNet.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
