program nr_5;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Block in 'lb\Lb.Block.pas',
  UnitChartCandelsFrame in 'lb\UnitChartCandelsFrame.pas' {ChartCandelsFrame: TFrame},
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.NeuronNet in '..\lib\Lb.NeuronNet.pas',
  Lb.ActivationFunction in '..\lib\Lb.ActivationFunction.pas',
  Lb.Trader in 'lb\Lb.Trader.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Sort in 'lb\Lb.Sort.pas',
  Lb.Generation in 'lb\Lb.Generation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
